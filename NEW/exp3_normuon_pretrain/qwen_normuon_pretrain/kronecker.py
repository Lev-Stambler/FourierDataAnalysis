from __future__ import annotations

import math
import os
import threading
from math import isqrt, prod

import torch
from torch import nn
from torch.utils.checkpoint import checkpoint

_CUTENSOR_BINARY_PLANS = {}
_CUTENSOR_THREAD_STATE = threading.local()


def _einsum_output_shape(
    expression: str,
    left: torch.Tensor,
    right: torch.Tensor,
) -> tuple[int, ...]:
    inputs, output = expression.replace(" ", "").split("->")
    left_modes, right_modes = inputs.split(",")
    dimensions: dict[str, int] = {}
    for modes, tensor in ((left_modes, left), (right_modes, right)):
        if len(modes) != tensor.ndim:
            raise ValueError(
                f"{expression} expects {len(modes)} dimensions, "
                f"got {tensor.ndim}"
            )
        for mode, size in zip(modes, tensor.shape, strict=True):
            previous = dimensions.setdefault(mode, int(size))
            if previous != int(size):
                raise ValueError(
                    f"inconsistent size for mode {mode}: "
                    f"{previous} vs {size}"
                )
    return tuple(dimensions[mode] for mode in output)


def _cutensor_binary_execute(
    expression: str,
    left: torch.Tensor,
    right: torch.Tensor,
) -> torch.Tensor:
    import nvmath
    from cuda.core import Device

    device_index = left.device.index or 0
    if getattr(_CUTENSOR_THREAD_STATE, "device_index", None) != device_index:
        _CUTENSOR_THREAD_STATE.context = Device(device_index).set_current()
        _CUTENSOR_THREAD_STATE.device_index = device_index
    output = torch.empty(
        _einsum_output_shape(expression, left, right),
        device=left.device,
        dtype=left.dtype,
    )
    key = (
        expression,
        tuple(left.shape),
        tuple(left.stride()),
        tuple(right.shape),
        tuple(right.stride()),
        tuple(output.shape),
        tuple(output.stride()),
        left.dtype,
        left.device,
    )
    contraction = _CUTENSOR_BINARY_PLANS.get(key)
    if contraction is None:
        contraction = nvmath.tensor.BinaryContraction(
            expression,
            left,
            right,
            out=output,
        )
        contraction.plan()
        _CUTENSOR_BINARY_PLANS[key] = contraction
    else:
        contraction.reset_operands_unchecked(
            a=left,
            b=right,
            out=output,
        )
    result = contraction.execute()
    contraction.release_operands()
    return result


_CUTENSOR_BACKWARD_EXPRESSIONS = {
    "bijk,rai->brajk": (
        "brajk,rai->bijk",
        "brajk,bijk->rai",
    ),
    "brajk,rcj->brack": (
        "brack,rcj->brajk",
        "brack,brajk->rcj",
    ),
    "brack,rdk->bacd": (
        "bacd,rdk->brack",
        "bacd,brack->rdk",
    ),
    "bijk,rcj->brick": (
        "brick,rcj->bijk",
        "brick,bijk->rcj",
    ),
    "brick,rdk->bricd": (
        "bricd,rdk->brick",
        "bricd,brick->rdk",
    ),
    "bricd,rai->bacd": (
        "bacd,rai->bricd",
        "bacd,bricd->rai",
    ),
}


class _CuTensorBinary(torch.autograd.Function):
    @staticmethod
    def forward(
        ctx,
        left: torch.Tensor,
        right: torch.Tensor,
        expression: str,
    ) -> torch.Tensor:
        if expression not in _CUTENSOR_BACKWARD_EXPRESSIONS:
            raise ValueError(
                f"unsupported cuTENSOR contraction: {expression}"
            )
        ctx.expression = expression
        ctx.save_for_backward(left, right)
        return _cutensor_binary_execute(expression, left, right)

    @staticmethod
    def backward(ctx, grad_output: torch.Tensor):
        left, right = ctx.saved_tensors
        left_expression, right_expression = (
            _CUTENSOR_BACKWARD_EXPRESSIONS[ctx.expression]
        )
        grad_output = grad_output.contiguous()
        grad_left = (
            _cutensor_binary_execute(
                left_expression,
                grad_output,
                right,
            )
            if ctx.needs_input_grad[0]
            else None
        )
        grad_right = (
            _cutensor_binary_execute(
                right_expression,
                grad_output,
                left,
            )
            if ctx.needs_input_grad[1]
            else None
        )
        return grad_left, grad_right, None


def context_channel_modes(
    context_length: int,
    embedding_width: int,
    logical_width: int,
) -> tuple[int, int, int]:
    if min(context_length, embedding_width, logical_width) <= 0:
        raise ValueError("Kronecker dimensions must be positive")
    if logical_width % context_length:
        raise ValueError("logical width must divide by context length")
    per_position = logical_width // context_length
    channel = isqrt(per_position)
    if channel * channel != per_position:
        raise ValueError("per-position width must be a perfect square")
    if logical_width not in (
        context_length * embedding_width,
        4 * context_length * embedding_width,
    ):
        raise ValueError(
            "width must be the full width or expansion-four width"
        )
    return context_length, channel, channel


class KroneckerLinear(nn.Module):
    """A rank-summed order-three Kronecker map."""

    def __init__(
        self,
        in_features: int,
        out_features: int,
        *,
        input_modes: tuple[int, int, int],
        output_modes: tuple[int, int, int],
        rank: int,
        rank_chunk: int,
        bias: bool = True,
    ) -> None:
        super().__init__()
        if prod(input_modes) != in_features:
            raise ValueError("input modes do not multiply to input width")
        if prod(output_modes) != out_features:
            raise ValueError("output modes do not multiply to output width")
        if min(in_features, out_features, rank, rank_chunk) <= 0:
            raise ValueError("Kronecker dimensions must be positive")
        self.in_features = int(in_features)
        self.out_features = int(out_features)
        self.input_modes = tuple(int(value) for value in input_modes)
        self.output_modes = tuple(int(value) for value in output_modes)
        self.rank = int(rank)
        runtime_chunk = os.environ.get("QWEN_NORMUON_RANK_CHUNK")
        self.rank_chunk = int(runtime_chunk or rank_chunk)
        if self.rank_chunk <= 0:
            raise ValueError("rank chunk must be positive")

        factors = []
        gains = []
        target_rms = []
        for input_mode, output_mode in zip(
            self.input_modes,
            self.output_modes,
            strict=True,
        ):
            factor = nn.Parameter(torch.empty(
                self.rank,
                output_mode,
                input_mode,
            ))
            factors.append(factor)
            gains.append(nn.Parameter(torch.ones(self.rank)))
            target_rms.append(
                input_mode ** -0.5
                * min(1.0, math.sqrt(output_mode / input_mode))
            )
        self.factors = nn.ParameterList(factors)
        self.gains = nn.ParameterList(gains)
        self.target_rms_values = tuple(
            float(value) for value in target_rms
        )
        self.register_buffer(
            "target_rms",
            torch.tensor(self.target_rms_values, dtype=torch.float32),
            persistent=False,
        )
        self.mixing = nn.Parameter(torch.empty(self.rank))
        self.bias = (
            nn.Parameter(torch.empty(self.out_features)) if bias else None
        )
        self.reset_parameters()

    def reset_parameters(self) -> None:
        for index, factor in enumerate(self.factors):
            nn.init.normal_(
                factor,
                std=self.target_rms_values[index],
            )
        for gain in self.gains:
            nn.init.ones_(gain)
        nn.init.constant_(self.mixing, self.rank ** -0.5)
        if self.bias is not None:
            nn.init.zeros_(self.bias)

    def normalized_factor(self, index: int) -> torch.Tensor:
        factor = self.factors[index]
        rms = torch.sqrt(
            factor.float().square().mean(dim=(-2, -1)) + 1e-8
        )
        scale = self.gains[index].float() / torch.clamp(
            rms / self.target_rms[index],
            min=1.0,
        )
        return factor * scale[:, None, None].to(dtype=factor.dtype)

    @staticmethod
    def _chunk_torch(
        value: torch.Tensor,
        factor0: torch.Tensor,
        factor1: torch.Tensor,
        factor2: torch.Tensor,
        mixing: torch.Tensor,
    ) -> torch.Tensor:
        if factor1.shape[-2] < factor1.shape[-1]:
            value = torch.einsum("bijk,rcj->brick", value, factor1)
            value = torch.einsum("brick,rdk->bricd", value, factor2)
            weighted_factor0 = factor0 * mixing[:, None, None]
            value = torch.einsum(
                "bricd,rai->bacd",
                value,
                weighted_factor0,
            )
            return value.flatten(start_dim=1)
        value = torch.einsum("bijk,rai->brajk", value, factor0)
        value = torch.einsum("brajk,rcj->brack", value, factor1)
        weighted_factor2 = factor2 * mixing[:, None, None]
        value = torch.einsum(
            "brack,rdk->bacd",
            value,
            weighted_factor2,
        )
        return value.flatten(start_dim=1)

    @staticmethod
    def _chunk_cutensor(
        value: torch.Tensor,
        factor0: torch.Tensor,
        factor1: torch.Tensor,
        factor2: torch.Tensor,
        mixing: torch.Tensor,
    ) -> torch.Tensor:
        dtype = value.dtype
        factor0 = factor0.to(dtype=dtype)
        factor1 = factor1.to(dtype=dtype)
        factor2 = factor2.to(dtype=dtype)
        mixing = mixing.to(dtype=dtype)
        if factor1.shape[-2] < factor1.shape[-1]:
            value = _CuTensorBinary.apply(
                value,
                factor1,
                "bijk,rcj->brick",
            )
            value = _CuTensorBinary.apply(
                value,
                factor2,
                "brick,rdk->bricd",
            )
            weighted_factor0 = factor0 * mixing[:, None, None]
            value = _CuTensorBinary.apply(
                value,
                weighted_factor0,
                "bricd,rai->bacd",
            )
            return value.flatten(start_dim=1)
        value = _CuTensorBinary.apply(
            value,
            factor0,
            "bijk,rai->brajk",
        )
        value = _CuTensorBinary.apply(
            value,
            factor1,
            "brajk,rcj->brack",
        )
        weighted_factor2 = factor2 * mixing[:, None, None]
        value = _CuTensorBinary.apply(
            value,
            weighted_factor2,
            "brack,rdk->bacd",
        )
        return value.flatten(start_dim=1)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        if value.shape[-1] != self.in_features:
            raise ValueError(
                f"expected final width {self.in_features}, "
                f"got {value.shape[-1]}"
            )
        batch_shape = value.shape[:-1]
        reshaped = value.reshape(-1, *self.input_modes)
        factors = [
            self.normalized_factor(index) for index in range(3)
        ]
        backend = os.environ.get(
            "QWEN_NORMUON_KRONECKER_BACKEND",
            "torch",
        )
        chunk_function = {
            "torch": self._chunk_torch,
            "cutensor": self._chunk_cutensor,
        }.get(backend)
        if chunk_function is None:
            raise ValueError(f"unsupported backend {backend}")
        output = None
        for start in range(0, self.rank, self.rank_chunk):
            stop = min(start + self.rank_chunk, self.rank)
            arguments = (
                reshaped,
                factors[0][start:stop],
                factors[1][start:stop],
                factors[2][start:stop],
                self.mixing[start:stop],
            )
            if self.training and torch.is_grad_enabled():
                chunk = checkpoint(
                    chunk_function,
                    *arguments,
                    use_reentrant=backend == "cutensor",
                )
            else:
                chunk = chunk_function(*arguments)
            output = chunk if output is None else output + chunk
        result = output.reshape(*batch_shape, self.out_features)
        if self.bias is not None:
            result = result + self.bias
        return result
