from __future__ import annotations

import math
import os
import threading
from math import isqrt, prod

import torch
from torch import nn
import torch.nn.functional as F
from torch.utils.checkpoint import checkpoint


_CUTENSOR_BINARY_PLANS = {}
_CUTENSOR_THREAD_STATE = threading.local()


def _einsum_output_shape(
    expression: str,
    left: torch.Tensor,
    right: torch.Tensor,
) -> tuple[int, ...]:
    """Infer the output shape for a two-operand, explicit-output einsum."""
    inputs, output = expression.replace(" ", "").split("->")
    left_modes, right_modes = inputs.split(",")
    dimensions: dict[str, int] = {}
    for modes, tensor in ((left_modes, left), (right_modes, right)):
        if len(modes) != tensor.ndim:
            raise ValueError(
                f"{expression} expects {len(modes)} dimensions, got "
                f"{tensor.ndim}"
            )
        for mode, size in zip(modes, tensor.shape, strict=True):
            previous = dimensions.setdefault(mode, int(size))
            if previous != int(size):
                raise ValueError(
                    f"inconsistent size for mode {mode}: {previous} vs {size}"
                )
    return tuple(dimensions[mode] for mode in output)


def _cutensor_binary_execute(
    expression: str,
    left: torch.Tensor,
    right: torch.Tensor,
) -> torch.Tensor:
    """Execute a planned cuTENSOR contraction into a fresh Torch tensor.

    The cache key includes every property on which a cuTENSOR plan depends.
    Only pointers change between executions, so the unchecked reset is safe.
    """
    import nvmath
    from cuda.core import Device

    # Reentrant activation checkpointing recomputes inside an autograd-engine
    # worker thread. cuda.core tracks current contexts per thread, separately
    # from torch.cuda, so initialize the CUDA primary context once per thread.
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
    # A stateful object otherwise retains its most recent operands. With many
    # distinct forward/backward shapes this pins gigabytes of dead activation
    # tensors in the plan cache. Releasing operands preserves the plan and its
    # workspace; the next call supplies compatible pointers via reset.
    contraction.release_operands()
    # With ``out`` supplied this is normally the same tensor. Returning the
    # library result also handles any future wrapper change without a copy.
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
    "brack,rdk->bracd": (
        "bracd,rdk->brack",
        "bracd,brack->rdk",
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
    """Autograd bridge around reusable nvmath/cuTENSOR plans."""

    @staticmethod
    def forward(
        ctx,
        left: torch.Tensor,
        right: torch.Tensor,
        expression: str,
    ) -> torch.Tensor:
        if expression not in _CUTENSOR_BACKWARD_EXPRESSIONS:
            raise ValueError(f"unsupported cuTENSOR contraction: {expression}")
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
            if ctx.needs_input_grad[0] else None
        )
        grad_right = (
            _cutensor_binary_execute(
                right_expression,
                grad_output,
                left,
            )
            if ctx.needs_input_grad[1] else None
        )
        return grad_left, grad_right, None


def context_channel_modes(
    context_length: int,
    embedding_width: int,
    logical_width: int,
    *,
    order: int = 3,
) -> tuple[int, ...]:
    """Factor a full-width state with an explicit context axis.

    Order three uses ``context × channel × channel``. Order four uses the
    pinned, padding-free Qwen layout ``context × 8 × 8 × 16`` at full width
    and ``context × 16 × 16 × 16`` at expansion-four width.
    """
    if min(context_length, embedding_width, logical_width) <= 0:
        raise ValueError("Kronecker dimensions must be positive")
    if logical_width % context_length:
        raise ValueError("logical width must be divisible by context length")
    if order == 3:
        per_position = logical_width // context_length
        channel = isqrt(per_position)
        if channel * channel != per_position:
            raise ValueError(
                "context_channel_v1 requires a square per-position width"
            )
        if logical_width not in (
            context_length * embedding_width,
            4 * context_length * embedding_width,
        ):
            raise ValueError(
                "Kronecker width must be the full width or expansion-four width"
            )
        return context_length, channel, channel
    if order == 4:
        if embedding_width != 1024:
            raise ValueError(
                "order-four context-channel layout requires embedding width 1024"
            )
        if logical_width not in (
            context_length * embedding_width,
            4 * context_length * embedding_width,
        ):
            raise ValueError(
                "Kronecker width must be the full width or expansion-four width"
            )
        if logical_width == context_length * embedding_width:
            return context_length, 8, 8, 16
        return context_length, 16, 16, 16
    raise ValueError("Kronecker context-channel layout supports order three or four")


class KroneckerLinear(nn.Module):
    """An order-c Kronecker product applied without materializing a matrix."""

    def __init__(
        self,
        in_features: int,
        out_features: int,
        *,
        input_modes: tuple[int, ...],
        output_modes: tuple[int, ...],
        bias: bool = True,
        weight_norm: bool = True,
        rank: int = 1,
        rank_chunk: int = 8,
    ) -> None:
        super().__init__()
        if min(in_features, out_features) <= 0:
            raise ValueError("Kronecker dimensions must be positive")
        if len(input_modes) != len(output_modes) or len(input_modes) < 2:
            raise ValueError(
                "Kronecker input/output modes must have equal order >= 2"
            )
        if min(*input_modes, *output_modes) <= 0:
            raise ValueError("Kronecker modes must be positive")
        if prod(input_modes) != in_features:
            raise ValueError("input modes do not multiply to in_features")
        if prod(output_modes) != out_features:
            raise ValueError("output modes do not multiply to out_features")
        if not weight_norm:
            raise ValueError(
                "Kronecker study requires factor weight normalization"
            )
        if rank <= 0 or rank_chunk <= 0:
            raise ValueError("Kronecker rank and rank chunk must be positive")
        self.in_features = int(in_features)
        self.out_features = int(out_features)
        self.input_modes = tuple(int(value) for value in input_modes)
        self.output_modes = tuple(int(value) for value in output_modes)
        self.order = len(self.input_modes)
        self.weight_norm = bool(weight_norm)
        self.rank = int(rank)
        runtime_rank_chunk = os.environ.get("QWEN_KRONECKER_RANK_CHUNK")
        self.rank_chunk = int(runtime_rank_chunk or rank_chunk)
        if self.rank_chunk <= 0:
            raise ValueError("runtime Kronecker rank chunk must be positive")

        factors = []
        gains = []
        target_rms = []
        for input_mode, output_mode in zip(
            self.input_modes, self.output_modes, strict=True
        ):
            shape = (
                (output_mode, input_mode)
                if self.rank == 1
                else (self.rank, output_mode, input_mode)
            )
            factor = nn.Parameter(torch.empty(shape))
            factor.lr_multiplier = (
                self.in_features / input_mode / self.order
            )
            factor.optimizer_role = "kronecker_factor"
            factors.append(factor)

            gain = nn.Parameter(
                torch.ones(()) if self.rank == 1 else torch.ones(self.rank)
            )
            gain.lr_multiplier = 1.0
            gain.optimizer_role = "kronecker_gain"
            gains.append(gain)
            target_rms.append(
                input_mode ** -0.5
                * min(1.0, math.sqrt(output_mode / input_mode))
            )

        self.factors = nn.ParameterList(factors)
        self.gains = nn.ParameterList(gains)
        self.target_rms_values = tuple(float(value) for value in target_rms)
        self.register_buffer(
            "target_rms",
            torch.tensor(self.target_rms_values, dtype=torch.float32),
            persistent=False,
        )
        self.mixing = (
            nn.Parameter(torch.empty(self.rank))
            if self.rank > 1
            else None
        )
        if self.mixing is not None:
            self.mixing.lr_multiplier = 1.0
            self.mixing.optimizer_role = "kronecker_mixing"
        self.bias = (
            nn.Parameter(torch.empty(self.out_features)) if bias else None
        )
        if self.bias is not None:
            self.bias.lr_multiplier = 1.0
            self.bias.optimizer_role = "bias"
        self.reset_parameters()

    def reset_parameters(self) -> None:
        for index, factor in enumerate(self.factors):
            nn.init.normal_(
                factor,
                std=self.target_rms_values[index],
            )
        for gain in self.gains:
            nn.init.ones_(gain)
        if self.mixing is not None:
            nn.init.constant_(self.mixing, self.rank ** -0.5)
        if self.bias is not None:
            nn.init.zeros_(self.bias)

    def normalized_factor(self, index: int) -> torch.Tensor:
        factor = self.factors[index]
        reduction = None if self.rank == 1 else (-2, -1)
        rms = torch.sqrt(
            factor.float().square().mean(dim=reduction) + 1e-8
        )
        scale = self.gains[index].float() / torch.clamp(
            rms / self.target_rms[index],
            min=1.0,
        )
        if self.rank > 1:
            scale = scale[:, None, None]
        return factor * scale.to(dtype=factor.dtype)

    @staticmethod
    def _rank_chunk_forward_unfused(
        value: torch.Tensor,
        factor0: torch.Tensor,
        factor1: torch.Tensor,
        factor2: torch.Tensor,
        mixing: torch.Tensor,
    ) -> torch.Tensor:
        value = torch.einsum("bijk,rai->brajk", value, factor0)
        value = torch.einsum("brajk,rcj->brack", value, factor1)
        value = torch.einsum("brack,rdk->bracd", value, factor2)
        value = value.flatten(start_dim=2)
        return torch.einsum("bro,r->bo", value, mixing)

    @staticmethod
    def _rank_chunk_forward(
        value: torch.Tensor,
        factor0: torch.Tensor,
        factor1: torch.Tensor,
        factor2: torch.Tensor,
        mixing: torch.Tensor,
    ) -> torch.Tensor:
        """Contract rank away before materializing the final output modes."""
        if factor1.shape[-2] < factor1.shape[-1]:
            # Down projection: reduce the two 64-wide channel modes before
            # carrying context through the rank axis. This makes the largest
            # rank-indexed intermediate four times smaller.
            value = torch.einsum("bijk,rcj->brick", value, factor1)
            value = torch.einsum("brick,rdk->bricd", value, factor2)
            weighted_factor0 = factor0 * mixing[:, None, None]
            value = torch.einsum(
                "bricd,rai->bacd", value, weighted_factor0
            )
            return value.flatten(start_dim=1)
        value = torch.einsum("bijk,rai->brajk", value, factor0)
        value = torch.einsum("brajk,rcj->brack", value, factor1)
        weighted_factor2 = factor2 * mixing[:, None, None]
        value = torch.einsum("brack,rdk->bacd", value, weighted_factor2)
        return value.flatten(start_dim=1)

    @staticmethod
    def _rank_chunk_forward_cutensor(
        value: torch.Tensor,
        factor0: torch.Tensor,
        factor1: torch.Tensor,
        factor2: torch.Tensor,
        mixing: torch.Tensor,
    ) -> torch.Tensor:
        """Apply the three factors with reusable arbitrary-stride plans."""
        dtype = value.dtype
        factor0 = factor0.to(dtype=dtype)
        factor1 = factor1.to(dtype=dtype)
        factor2 = factor2.to(dtype=dtype)
        mixing = mixing.to(dtype=dtype)
        if factor1.shape[-2] < factor1.shape[-1]:
            value = _CuTensorBinary.apply(
                value, factor1, "bijk,rcj->brick"
            )
            value = _CuTensorBinary.apply(
                value, factor2, "brick,rdk->bricd"
            )
            weighted_factor0 = factor0 * mixing[:, None, None]
            value = _CuTensorBinary.apply(
                value, weighted_factor0, "bricd,rai->bacd"
            )
            return value.flatten(start_dim=1)
        value = _CuTensorBinary.apply(
            value, factor0, "bijk,rai->brajk"
        )
        value = _CuTensorBinary.apply(
            value, factor1, "brajk,rcj->brack"
        )
        weighted_factor2 = factor2 * mixing[:, None, None]
        value = _CuTensorBinary.apply(
            value, weighted_factor2, "brack,rdk->bacd"
        )
        return value.flatten(start_dim=1)

    @staticmethod
    def _rank_chunk_forward_general(
        value: torch.Tensor,
        *factors_and_mixing: torch.Tensor,
    ) -> torch.Tensor:
        """Apply an arbitrary-order Kronecker sum one rank chunk at a time.

        The rank axis is introduced by the first factor and retained through
        the remaining mode contractions. It is reduced by the learned mixing
        coefficients before returning, so neither an individual dense matrix
        nor the full rank-summed matrix is materialized.
        """
        factors = factors_and_mixing[:-1]
        mixing = factors_and_mixing[-1]
        if len(factors) < 2:
            raise ValueError("general Kronecker contraction needs at least two factors")

        # Contract low expansion-ratio modes first. Keep the largest ratio for
        # the final contraction, where its factor is multiplied by the rank
        # mixture and the rank axis is eliminated directly. For the pinned
        # order-four FFN this reduces the peak rank-indexed activation from
        # rank*65,536 to rank*32,768 elements without changing the operator.
        axes = sorted(
            range(len(factors)),
            key=lambda axis: (
                factors[axis].shape[-2] / factors[axis].shape[-1],
                axis,
            ),
        )
        final_axis = axes.pop()
        for contraction_index, axis in enumerate(axes):
            factor = factors[axis]
            if contraction_index == 0:
                # [batch, i0, i1, ...] -> [batch, rank, ..., o_axis, ...]
                value = torch.movedim(value, 1 + axis, -1)
                value = torch.einsum("b...i,roi->br...o", value, factor)
                value = torch.movedim(value, -1, 2 + axis)
                continue

            # Every original mode keeps its logical position: contraction
            # replaces i_axis with o_axis while the rank dimension sits at 1.
            tensor_axis = 2 + axis
            value = torch.movedim(value, tensor_axis, -1)
            value = torch.einsum("br...i,roi->br...o", value, factor)
            value = torch.movedim(value, -1, tensor_axis)

        final_factor = factors[final_axis] * mixing[:, None, None]
        value = torch.movedim(value, 2 + final_axis, -1)
        value = torch.einsum("br...i,roi->b...o", value, final_factor)
        value = torch.movedim(value, -1, 1 + final_axis)
        return value.flatten(start_dim=1)

    def _rank_sum_forward_general(self, x: torch.Tensor) -> torch.Tensor:
        """Torch fallback for rank sums whose tensor order is not three."""
        backend = os.environ.get("QWEN_KRONECKER_BACKEND", "torch")
        if backend not in ("torch", "torch-unfused", "cutensor"):
            raise ValueError(f"unsupported Kronecker backend: {backend}")

        batch_shape = x.shape[:-1]
        value = x.reshape(-1, *self.input_modes)
        factors = [
            self.normalized_factor(index) for index in range(self.order)
        ]
        output = None
        for start in range(0, self.rank, self.rank_chunk):
            stop = min(start + self.rank_chunk, self.rank)
            arguments = (
                value,
                *(factor[start:stop] for factor in factors),
                self.mixing[start:stop],
            )
            if self.training and torch.is_grad_enabled():
                chunk = checkpoint(
                    self._rank_chunk_forward_general,
                    *arguments,
                    use_reentrant=False,
                )
            else:
                chunk = self._rank_chunk_forward_general(*arguments)
            output = chunk if output is None else output + chunk
        return output.reshape(*batch_shape, self.out_features)

    def _rank_sum_forward(self, x: torch.Tensor) -> torch.Tensor:
        if self.order != 3:
            return self._rank_sum_forward_general(x)

        batch_shape = x.shape[:-1]
        value = x.reshape(-1, *self.input_modes)
        factors = [
            self.normalized_factor(index) for index in range(self.order)
        ]
        output = None
        backend = os.environ.get("QWEN_KRONECKER_BACKEND", "torch")
        chunk_forward = {
            "torch": self._rank_chunk_forward,
            "torch-unfused": self._rank_chunk_forward_unfused,
            "cutensor": self._rank_chunk_forward_cutensor,
        }.get(backend)
        if chunk_forward is None:
            raise ValueError(f"unsupported Kronecker backend: {backend}")
        for start in range(0, self.rank, self.rank_chunk):
            stop = min(start + self.rank_chunk, self.rank)
            arguments = (
                value,
                factors[0][start:stop],
                factors[1][start:stop],
                factors[2][start:stop],
                self.mixing[start:stop],
            )
            if self.training and torch.is_grad_enabled():
                chunk = checkpoint(
                    chunk_forward,
                    *arguments,
                    # The external contraction backends retain per-operation
                    # state. Reentrant checkpointing keeps only one rank
                    # chunk's intermediate tensors live during backward.
                    use_reentrant=backend != "torch",
                )
            else:
                chunk = chunk_forward(*arguments)
            output = chunk if output is None else output + chunk
        return output.reshape(*batch_shape, self.out_features)

    def _rank_one_forward(self, x: torch.Tensor) -> torch.Tensor:
        batch_shape = x.shape[:-1]
        batch_dimensions = len(batch_shape)
        value = x.reshape(*batch_shape, *self.input_modes)
        for axis in range(self.order):
            tensor_axis = batch_dimensions + axis
            value = torch.movedim(value, tensor_axis, -1)
            value = F.linear(value, self.normalized_factor(axis))
            value = torch.movedim(value, -1, tensor_axis)
        return value.reshape(*batch_shape, self.out_features)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        if x.shape[-1] != self.in_features:
            raise ValueError(
                f"expected final width {self.in_features}, got {x.shape[-1]}"
            )
        output = (
            self._rank_one_forward(x)
            if self.rank == 1
            else self._rank_sum_forward(x)
        )
        if self.bias is not None:
            output = output + self.bias
        return output

    @property
    def saving(self) -> float:
        dense = self.in_features * self.out_features
        structured = sum(factor.numel() for factor in self.factors)
        return structured / dense

    def extra_repr(self) -> str:
        return (
            f"in_features={self.in_features}, out_features={self.out_features}, "
            f"input_modes={self.input_modes}, output_modes={self.output_modes}, "
            f"rank={self.rank}, rank_chunk={self.rank_chunk}, "
            f"bias={self.bias is not None}"
        )
