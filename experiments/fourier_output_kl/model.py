"""A centered binary energy model made from exact Walsh characters.

For an input bit vector ``x`` the model learns a probability gap ``g(x)`` and
returns the two output-conditioned scores ``[-g(x)/2, +g(x)/2]``.  This is the
efficient form of appending one output-selector bit to every Walsh character:
output-independent characters would cancel exactly under the final softmax.

The forward pass is exact TopK parity.  Mask parameters are persistent
log-potentials and receive the product-vertex straight-through gradient, which
does not suffer the exponential attenuation of an expected-parity relaxation.
"""

from __future__ import annotations

import math
import itertools
from collections.abc import Mapping
from typing import Any

import numpy as np
import torch
from torch import nn
from torch.utils.checkpoint import checkpoint


def _splitmix64(value: np.ndarray) -> np.ndarray:
    value = value.astype(np.uint64, copy=False)
    value = value + np.uint64(0x9E3779B97F4A7C15)
    value = (value ^ (value >> np.uint64(30))) * np.uint64(0xBF58476D1CE4E5B9)
    value = (value ^ (value >> np.uint64(27))) * np.uint64(0x94D049BB133111EB)
    return value ^ (value >> np.uint64(31))


def deterministic_unique_supports(
    n_bits: int,
    terms: int,
    *,
    max_degree: int = 8,
    seed: int = 0,
    support_layout: str = "uniform",
    token_bits: int = 32,
    target_tokens: int = 16,
) -> tuple[np.ndarray, np.ndarray]:
    """Create a diverse, prefix-stable support bank with no exact duplicates."""
    if not 1 <= max_degree < n_bits or terms <= 0:
        raise ValueError("invalid support-bank shape")
    if support_layout not in {"uniform", "semantic_structured_v2"}:
        raise ValueError(f"unknown support layout {support_layout!r}")
    if (support_layout == "semantic_structured_v2"
            and (token_bits <= max_degree or n_bits % token_bits
                 or not 0 < target_tokens * token_bits < n_bits)):
        raise ValueError("target-structured layout is incompatible with the bits")
    rows = np.full((terms, max_degree), n_bits, dtype=np.uint32)
    degrees = np.ones(terms, dtype=np.uint8)
    singleton_count = min(n_bits, terms)
    rows[:singleton_count, 0] = np.arange(singleton_count, dtype=np.uint32)
    if singleton_count == terms:
        return rows, degrees

    # Bias the fresh bank toward low-degree structure while retaining a tail of
    # contextual degree-5--8 characters.  The integer cycle makes prefixes
    # stable as M changes.
    degree_weight = {2: 30, 3: 25, 4: 20, 5: 10, 6: 7, 7: 5, 8: 3}
    degree_cycle = np.asarray([
        degree for degree, weight in degree_weight.items()
        if degree <= max_degree for _ in range(weight)
    ], dtype=np.uint8)
    if not len(degree_cycle):
        raise ValueError("more than n_bits degree-one terms cannot be unique")
    remaining = terms - singleton_count
    if terms > sum(math.comb(n_bits, degree)
                   for degree in range(1, max_degree + 1)):
        raise ValueError("requested more unique supports than the degree bank holds")
    proposed_degree = np.resize(degree_cycle, remaining)
    degree_count = {degree: 0 for degree in range(2, max_degree + 1)}
    for offset, raw_degree in enumerate(proposed_degree):
        degree = int(raw_degree)
        if degree_count[degree] >= math.comb(n_bits, degree):
            available = [candidate for candidate in range(2, max_degree + 1)
                         if degree_count[candidate] < math.comb(n_bits, candidate)]
            if not available:
                raise ValueError("non-singleton degree bank is exhausted")
            degree = min(available, key=lambda candidate: (
                abs(candidate - degree), candidate
            ))
        degree_count[degree] += 1
        degrees[singleton_count + offset] = degree
    if support_layout == "uniform":
        row_ids = np.arange(singleton_count, terms, dtype=np.uint64)
        base_seed = np.uint64((seed * 0xD1B54A32D192ED03) & ((1 << 64) - 1))
        chosen = np.empty((remaining, max_degree), dtype=np.uint32)
        state_base = row_ids ^ base_seed
        for column in range(max_degree):
            salt = np.uint64(
                ((column + 1) * 0x9E3779B97F4A7C15) & ((1 << 64) - 1)
            )
            state = _splitmix64(state_base + salt)
            candidate = (state % np.uint64(n_bits)).astype(np.uint32)
            if column:
                collision = (candidate[:, None] == chosen[:, :column]).any(axis=1)
                attempt = 0
                while collision.any():
                    attempt += 1
                    state[collision] = _splitmix64(
                        state[collision] + np.uint64(attempt)
                    )
                    candidate[collision] = (
                        state[collision] % np.uint64(n_bits)
                    ).astype(np.uint32)
                    collision = (
                        candidate[:, None] == chosen[:, :column]
                    ).any(axis=1)
            chosen[:, column] = candidate
        active = np.arange(max_degree)[None, :] < degrees[singleton_count:, None]
        rows[singleton_count:] = np.where(active, chosen, n_bits)
        rows[singleton_count:].sort(axis=1)
    else:
        # The fixed-fields layout stores the target surface form, lowercase,
        # prefix and suffix in the first 16 tokens.  Uniform subsets of all
        # 4096 bits almost never form a useful within-token Walsh character.
        # Stratification retains global exploration while giving categorical
        # target structure enough coverage to be learnable from the outset.
        rng = np.random.default_rng(seed ^ 0x51A7C7)
        token_count = n_bits // token_bits
        target_stop = target_tokens * token_bits

        # Exact 20/10/20/40/10 per-degree cycle: target-context, target-local,
        # target-cross, target-context, other-local, target-context,
        # target-cross, global, target-local, target-context.  Keeping a
        # separate cursor for each degree prevents layout/degree confounding.
        stratum_cycle = (
            "target_context", "target_local", "target_cross",
            "target_context", "other_local", "target_context",
            "target_cross", "global", "target_local", "target_context",
        )
        stratum_cursor = {degree: 0 for degree in range(2, max_degree + 1)}
        target_degree_two = np.asarray([
            (token * token_bits + left, token * token_bits + right)
            for token in range(target_tokens)
            for left, right in itertools.combinations(range(token_bits), 2)
        ], dtype=np.uint32)
        target_degree_two = target_degree_two[
            np.random.default_rng(seed ^ 0xD2).permutation(len(target_degree_two))
        ]
        target_degree_two_cursor = 0
        context_tokens = np.arange(target_tokens, token_count, dtype=np.int64)
        # Neighbor fields 16:56, sentence position/shape 56:64 and prompt
        # suffix 64:128 receive 60/10/30 percent of context selections.
        context_weight = np.where(
            context_tokens < min(56, token_count),
            0.60 / max(1, min(56, token_count) - target_tokens),
            np.where(
                context_tokens < min(64, token_count),
                0.10 / max(1, min(64, token_count) - min(56, token_count)),
                0.30 / max(1, token_count - min(64, token_count)),
            ),
        )
        context_weight /= context_weight.sum()

        def token_pool(token: int) -> np.ndarray:
            return np.arange(
                token * token_bits, (token + 1) * token_bits,
                dtype=np.uint32,
            )

        def candidate_support(stratum: str, degree: int) -> np.ndarray:
            if stratum == "target_local":
                token = int(rng.integers(target_tokens))
                return np.sort(rng.choice(
                    token_pool(token), degree, replace=False
                ))
            if stratum == "other_local":
                token = int(rng.integers(target_tokens, token_count))
                return np.sort(rng.choice(
                    token_pool(token), degree, replace=False
                ))
            if stratum == "target_cross":
                while True:
                    support = np.sort(rng.choice(
                        target_stop, degree, replace=False
                    )).astype(np.uint32)
                    if len(np.unique(support // token_bits)) >= 2:
                        return support
            if stratum == "target_context":
                target_degree = (degree + 1) // 2
                context_degree = degree - target_degree
                target_token = int(rng.integers(target_tokens))
                target_part = rng.choice(
                    token_pool(target_token), target_degree, replace=False
                )
                selected_context_tokens = rng.choice(
                    context_tokens, context_degree, replace=False,
                    p=context_weight,
                )
                context_part = (
                    selected_context_tokens * token_bits
                    + rng.integers(token_bits, size=context_degree)
                ).astype(np.uint32)
                return np.sort(np.concatenate((target_part, context_part)))
            return np.sort(rng.choice(n_bits, degree, replace=False))

        used = {tuple(map(int, rows[row, :int(degrees[row])]))
                for row in range(singleton_count)}
        for row in range(singleton_count, terms):
            degree = int(degrees[row])
            cursor = stratum_cursor[degree]
            stratum_cursor[degree] += 1
            stratum = stratum_cycle[cursor % len(stratum_cycle)]
            if stratum == "target_local" and degree == 2:
                while target_degree_two_cursor < len(target_degree_two):
                    replacement = target_degree_two[target_degree_two_cursor]
                    target_degree_two_cursor += 1
                    key = tuple(map(int, replacement))
                    if key not in used:
                        break
                else:
                    # The exact family is finite.  At M>131k keep uniqueness
                    # and target involvement by spilling into cross-token
                    # target-only degree-two supports.
                    while True:
                        replacement = candidate_support("target_cross", degree)
                        key = tuple(map(int, replacement))
                        if key not in used:
                            break
            else:
                while True:
                    replacement = candidate_support(stratum, degree)
                    key = tuple(map(int, replacement))
                    if key not in used:
                        break
            if key in used:
                raise RuntimeError("structured support selection failed")
            used.add(key)
            rows[row, :degree] = replacement

    # Degree-two collisions are possible at million-character scale.  Repair
    # only colliding rows; the common path stays vectorized and deterministic.
    used: set[tuple[int, ...]] = set()
    rng = np.random.default_rng(seed ^ 0xA5A5A5A5)
    for row in range(terms):
        degree = int(degrees[row])
        key = tuple(map(int, rows[row, :degree]))
        if key not in used:
            used.add(key)
            continue
        while True:
            replacement = np.sort(
                rng.choice(n_bits, size=degree, replace=False)
            ).astype(np.uint32)
            key = tuple(map(int, replacement))
            if key not in used:
                used.add(key)
                rows[row].fill(n_bits)
                rows[row, :degree] = replacement
                break
    if len(used) != terms:
        raise RuntimeError("unique support initialization failed")
    return rows, degrees


def hard_topk_mask(theta: torch.Tensor, degree: torch.Tensor,
                   max_degree: int) -> torch.Tensor:
    # Rows have heterogeneous fixed degree.  The leading ``degree`` entries
    # must therefore be the actual largest scores, not an arbitrary ordering
    # of the top ``max_degree`` set.
    indices = theta.topk(max_degree, dim=1, sorted=True).indices
    active = (
        torch.arange(max_degree, device=theta.device)[None, :] < degree[:, None]
    )
    return torch.zeros_like(theta, dtype=torch.bool).scatter(1, indices, active)


class _ExactWalshProductVertexSTE(torch.autograd.Function):
    """Exact parity forward with the multilinear vertex derivative backward."""

    @staticmethod
    def forward(ctx, bits: torch.Tensor, theta: torch.Tensor,
                degree: torch.Tensor, max_degree: int) -> torch.Tensor:
        mask = hard_topk_mask(theta, degree, max_degree)
        parity = torch.remainder(bits @ mask.to(bits.dtype).t(), 2.0)
        sign = 1.0 - 2.0 * parity
        ctx.save_for_backward(bits, mask, sign)
        return sign

    @staticmethod
    def backward(ctx, output_gradient: torch.Tensor):
        bits, mask, sign = ctx.saved_tensors
        # At a Boolean mask vertex, for x in {0,1}:
        # d chi / d m_d = -2 x_d chi (1 - 2 m_d).
        weighted = output_gradient.float() * sign.float()
        gradient = -2.0 * (weighted.t() @ bits.float())
        gradient.mul_(1.0 - 2.0 * mask.float())
        # TopK is invariant to a rowwise score offset.  Remove this null-space
        # component so AdamW capacity is spent only on support-changing updates.
        gradient.sub_(gradient.mean(dim=1, keepdim=True))
        return None, gradient, None, None


def exact_walsh_ste(bits: torch.Tensor, theta: torch.Tensor,
                    degree: torch.Tensor, max_degree: int) -> torch.Tensor:
    return _ExactWalshProductVertexSTE.apply(bits, theta, degree, max_degree)


def _gap_chunk(bits: torch.Tensor, theta: torch.Tensor, degree: torch.Tensor,
               coefficient: torch.Tensor, max_degree: int,
               output_scale: float) -> torch.Tensor:
    character = exact_walsh_ste(bits, theta, degree, max_degree)
    return output_scale * (character.float() @ coefficient.float())


def _exact_gap_chunk(bits: torch.Tensor, theta: torch.Tensor,
                     degree: torch.Tensor, coefficient: torch.Tensor,
                     max_degree: int, output_scale: float) -> torch.Tensor:
    mask = hard_topk_mask(theta, degree, max_degree).to(bits.dtype)
    parity = torch.remainder(bits @ mask.t(), 2.0)
    return output_scale * ((1.0 - 2.0 * parity).float() @ coefficient.float())


class OutputSelectorWalshStudent(nn.Module):
    """One exact Fourier bank representing the class-1 versus class-0 gap."""

    def __init__(self, n_bits: int, terms: int, *, seed: int = 0,
                 max_degree: int = 8, char_chunk: int = 1024,
                 support_layout: str = "uniform",
                 token_bits: int = 32, target_tokens: int = 16,
                 initial_probability: float = 0.5,
                 initial_score_gap: float = 0.02,
                 coefficient_std: float = 0.02,
                 checkpoint_chunks: bool = True,
                 compile_chunks: bool = False):
        super().__init__()
        if char_chunk <= 0 or initial_score_gap <= 0 or coefficient_std < 0:
            raise ValueError("invalid model initialization")
        rows, degrees = deterministic_unique_supports(
            n_bits, terms, max_degree=max_degree, seed=seed,
            support_layout=support_layout, token_bits=token_bits,
            target_tokens=target_tokens,
        )
        theta = torch.zeros((terms, n_bits), dtype=torch.float32)
        for lo in range(0, terms, char_chunk):
            hi = min(lo + char_chunk, terms)
            block = torch.from_numpy(rows[lo:hi].astype(np.int64))
            active = block < n_bits
            row = torch.arange(hi - lo)[:, None].expand_as(block)[active]
            theta[lo:hi][row, block[active]] = initial_score_gap
        generator = torch.Generator().manual_seed(seed + 17)
        coefficient = torch.randn(terms, generator=generator) * coefficient_std
        self.theta = nn.Parameter(theta)
        self.coefficient = nn.Parameter(coefficient.float())
        probability = float(np.clip(initial_probability, 1e-5, 1.0 - 1e-5))
        self.bias = nn.Parameter(torch.tensor(math.log(
            probability / (1.0 - probability)
        )))
        self.register_buffer("degree", torch.from_numpy(degrees.astype(np.int64)))
        self.n_bits = int(n_bits)
        self.terms = int(terms)
        self.max_degree = int(max_degree)
        self.char_chunk = int(char_chunk)
        self.support_layout = support_layout
        self.token_bits = int(token_bits)
        self.target_tokens = int(target_tokens)
        self.output_scale = terms ** -0.5
        self.checkpoint_chunks = bool(checkpoint_chunks)
        chunk_function = _gap_chunk
        self.compile_error: str | None = None
        if compile_chunks and torch.cuda.is_available():
            try:
                chunk_function = torch.compile(
                    chunk_function, mode="max-autotune-no-cudagraphs",
                    fullgraph=False, dynamic=False,
                )
            except Exception as error:
                self.compile_error = repr(error)
        self._chunk_function = chunk_function

    def gap(self, bits: torch.Tensor) -> torch.Tensor:
        if bits.ndim != 2 or bits.shape[1] != self.n_bits:
            raise ValueError(
                f"expected bits [batch,{self.n_bits}], got {tuple(bits.shape)}"
            )
        value = self.bias.expand(len(bits)).float()
        bits = bits.float()
        for lo in range(0, self.terms, self.char_chunk):
            hi = min(lo + self.char_chunk, self.terms)
            arguments = (
                bits, self.theta[lo:hi], self.degree[lo:hi],
                self.coefficient[lo:hi], self.max_degree, self.output_scale,
            )
            if self.training:
                contribution = (
                    checkpoint(
                        self._chunk_function, *arguments, use_reentrant=False
                    ) if self.checkpoint_chunks else
                    self._chunk_function(*arguments)
                )
            else:
                contribution = _exact_gap_chunk(*arguments)
            value = value + contribution
        return value

    def class_scores(self, bits: torch.Tensor) -> torch.Tensor:
        gap = self.gap(bits)
        return torch.stack((-0.5 * gap, 0.5 * gap), dim=-1)

    def score(self, bits: torch.Tensor,
              output_bit: int | torch.Tensor) -> torch.Tensor:
        scores = self.class_scores(bits)
        if isinstance(output_bit, int):
            if output_bit not in (0, 1):
                raise ValueError("output bit must be zero or one")
            return scores[:, output_bit]
        selector = torch.as_tensor(output_bit, device=scores.device).long()
        if selector.shape != (len(bits),) or bool(((selector < 0) | (selector > 1)).any()):
            raise ValueError("output selector must be a length-batch binary vector")
        return scores.gather(1, selector[:, None]).squeeze(1)

    def forward(self, bits: torch.Tensor) -> torch.Tensor:
        return self.class_scores(bits)

    def use_eager_chunks(self) -> None:
        """Fall back cleanly if first-call compilation fails at runtime."""
        self._chunk_function = _gap_chunk

    @torch.no_grad()
    def hard_index_rows(self) -> torch.Tensor:
        output = []
        for lo in range(0, self.terms, self.char_chunk):
            hi = min(lo + self.char_chunk, self.terms)
            ranked = self.theta[lo:hi].topk(
                self.max_degree, dim=1, sorted=True
            ).indices
            active = (
                torch.arange(self.max_degree, device=self.theta.device)[None, :]
                < self.degree[lo:hi, None]
            )
            output.append(
                ranked.masked_fill(~active, self.n_bits).sort(dim=1).values.cpu()
            )
        return torch.cat(output)

    @torch.no_grad()
    def sparse_state(self) -> dict[str, Any]:
        rows = self.hard_index_rows().numpy().astype(np.uint32)
        valid = rows < self.n_bits
        degrees = valid.sum(axis=1, dtype=np.int32)
        return {
            "schema": "centered-binary-output-selector-v1",
            "n_bits": self.n_bits,
            "degrees": degrees.astype(np.uint8),
            "indices": rows[valid].astype(np.uint16),
            "coefficient": (
                self.coefficient.detach().float().cpu().numpy()
                * self.output_scale
            ).astype(np.float32),
            "bias": float(self.bias.detach()),
        }


def teacher_distribution(probability: torch.Tensor) -> torch.Tensor:
    value = probability.float().clamp(0.0, 1.0)
    return torch.stack((1.0 - value, value), dim=-1)


def teacher_student_kl(scores: torch.Tensor,
                       probability: torch.Tensor) -> torch.Tensor:
    if scores.ndim != 2 or scores.shape[-1] != 2:
        raise ValueError("scores must have shape [batch,2]")
    target = teacher_distribution(probability)
    return torch.nn.functional.kl_div(
        torch.log_softmax(scores.float(), dim=-1), target,
        reduction="batchmean",
    )


def distribution_metrics(scores: np.ndarray | torch.Tensor,
                         teacher_probability_value: np.ndarray | torch.Tensor,
                         ) -> dict[str, float]:
    value = torch.as_tensor(scores, dtype=torch.float64)
    if value.ndim != 2 or value.shape[1] != 2:
        raise ValueError("scores must be [examples,2]")
    target_probability = torch.as_tensor(
        teacher_probability_value, dtype=torch.float64
    ).reshape(-1).clamp(1e-8, 1.0 - 1e-8)
    if len(value) != len(target_probability):
        raise ValueError("scores and teacher targets disagree")
    target = torch.stack((1.0 - target_probability, target_probability), -1)
    log_probability = torch.log_softmax(value, dim=-1)
    probability = log_probability.exp()[:, 1]
    gap = value[:, 1] - value[:, 0]
    residual = probability - target_probability
    absolute = residual.abs()
    entropy = -(target * target.log()).sum(-1).mean()
    cross_entropy = -(target * log_probability).sum(-1).mean()
    teacher_hard = target_probability >= 0.5
    student_hard = probability >= 0.5
    positive = teacher_hard.sum().clamp_min(1)
    negative = (~teacher_hard).sum().clamp_min(1)
    teacher_variance = (
        target_probability - target_probability.mean()
    ).square().sum()
    joint = torch.stack((
        ((~teacher_hard) & (~student_hard)).sum(),
        ((~teacher_hard) & student_hard).sum(),
        (teacher_hard & (~student_hard)).sum(),
        (teacher_hard & student_hard).sum(),
    )).reshape(2, 2).double()
    joint /= joint.sum().clamp_min(1.0)
    independent = joint.sum(1, keepdim=True) * joint.sum(0, keepdim=True)
    active = joint > 0
    hard_mi = float((joint[active] * torch.log2(
        joint[active] / independent[active].clamp_min(1e-12)
    )).sum())
    teacher_rate = float(teacher_hard.double().mean())
    teacher_hard_entropy = (
        -teacher_rate * math.log2(teacher_rate)
        - (1.0 - teacher_rate) * math.log2(1.0 - teacher_rate)
        if 0.0 < teacher_rate < 1.0 else 0.0
    )

    def cosine(left: torch.Tensor, right: torch.Tensor) -> float:
        return float(torch.dot(left, right) / (
            left.norm() * right.norm()
        ).clamp_min(1e-12))

    quantile = torch.quantile(
        absolute, torch.tensor([0.5, 0.9, 0.95, 0.99], dtype=torch.float64)
    )
    result = {
        "kl": float(cross_entropy - entropy),
        "cross_entropy": float(cross_entropy),
        "teacher_entropy": float(entropy),
        "mae": float(absolute.mean()),
        "rmse": float(residual.square().mean().sqrt()),
        "absolute_error_max": float(absolute.max()),
        "absolute_error_variance": float(absolute.var(unbiased=False)),
        "absolute_error_p50": float(quantile[0]),
        "absolute_error_p90": float(quantile[1]),
        "absolute_error_p95": float(quantile[2]),
        "absolute_error_p99": float(quantile[3]),
        "residual_mean": float(residual.mean()),
        "residual_variance": float(residual.var(unbiased=False)),
        "r_squared": 1.0 - float(
            residual.square().sum() / teacher_variance.clamp_min(1e-12)
        ),
        "probability_cosine": cosine(target_probability, probability),
        "centered_probability_cosine": cosine(
            target_probability - target_probability.mean(),
            probability - probability.mean(),
        ),
        "centered_logit_cosine": cosine(
            torch.logit(target_probability) - torch.logit(target_probability).mean(),
            gap - gap.mean(),
        ),
        "agreement": float((teacher_hard == student_hard).double().mean()),
        "balanced_agreement": 0.5 * float(
            (teacher_hard & student_hard).sum() / positive
            + ((~teacher_hard) & (~student_hard)).sum() / negative
        ),
        "teacher_positive_recall": float(
            (teacher_hard & student_hard).sum() / positive
        ),
        "teacher_negative_recall": float(
            ((~teacher_hard) & (~student_hard)).sum() / negative
        ),
        "teacher_positive_rate": teacher_rate,
        "student_positive_rate": float(student_hard.double().mean()),
        "hard_mutual_information_bits": hard_mi,
        "hard_mutual_information_fraction": (
            hard_mi / teacher_hard_entropy if teacher_hard_entropy else 0.0
        ),
        "gap_mean": float(gap.mean()),
        "gap_std": float(gap.std(unbiased=False)),
        "gap_abs_max": float(gap.abs().max()),
    }
    confidence = torch.maximum(target_probability, 1.0 - target_probability)
    for name, lower, upper in (
        ("50_60", 0.50, 0.60), ("60_75", 0.60, 0.75),
        ("75_90", 0.75, 0.90), ("90_100", 0.90, 1.01),
    ):
        selected = (confidence >= lower) & (confidence < upper)
        result[f"mae_confidence_{name}"] = (
            float(absolute[selected].mean()) if selected.any() else 0.0
        )
    return result


def functional_diversity_loss(
    model: OutputSelectorWalshStudent,
    bits: torch.Tensor,
    *,
    character_sample: int = 128,
    generator: torch.Generator | None = None,
    report: bool = True,
    duplicate_threshold: float = 0.95,
) -> tuple[torch.Tensor, dict[str, float]]:
    """Penalize only near-duplicate empirical characters.

    Useful features of the same target word are legitimately correlated on the
    language distribution.  Driving the entire Gram matrix toward identity
    destroys that signal.  A high-correlation hinge implements the intended
    uniqueness constraint without treating ordinary correlation as a defect.
    """
    count = min(model.terms, character_sample)
    if count < 2:
        return model.theta.sum() * 0.0, {"sampled_characters": float(count)}
    rows = torch.randint(
        model.terms, (count + 32,), device=model.theta.device,
        generator=generator,
    ).unique()[:count]
    if len(rows) < 2:
        return model.theta.sum() * 0.0, {"sampled_characters": float(len(rows))}
    count = len(rows)
    character = exact_walsh_ste(
        bits.float(), model.theta[rows], model.degree[rows], model.max_degree
    ).float()
    centered = character - character.mean(dim=0, keepdim=True)
    # Adding epsilon inside sqrt is essential: sqrt(0).clamp_min(eps) can
    # still backpropagate a 0*inf NaN for characters constant on this batch.
    normalized = centered / (
        centered.square().mean(dim=0, keepdim=True) + 1e-4
    ).sqrt()
    gram = normalized.t() @ normalized / len(bits)
    off_diagonal = gram - torch.diag_embed(torch.diagonal(gram))
    excess = torch.relu(off_diagonal.abs() - duplicate_threshold)
    loss = excess.square().sum() / (count * (count - 1))
    if not report:
        return loss, {}
    absolute = off_diagonal.detach().abs()
    pair = absolute[torch.triu(
        torch.ones_like(absolute, dtype=torch.bool), diagonal=1
    )]
    return loss, {
        "sampled_characters": float(count),
        "mean_abs_correlation": float(pair.mean()),
        "p95_abs_correlation": float(torch.quantile(pair, 0.95)),
        "max_abs_correlation": float(pair.max()),
        "near_duplicate_fraction": float(
            (pair > duplicate_threshold).float().mean()
        ),
    }


@torch.no_grad()
def repair_duplicate_supports(
    model: OutputSelectorWalshStudent,
    optimizer: torch.optim.Optimizer,
    *,
    seed: int,
) -> dict[str, int | float]:
    """Merge exact duplicate characters and reseed freed rows at zero weight."""
    rows = model.hard_index_rows().numpy().astype(np.uint32)
    _, inverse, counts = np.unique(
        rows, axis=0, return_inverse=True, return_counts=True
    )
    groups = np.flatnonzero(counts > 1)
    duplicate_count = int((counts[groups] - 1).sum())
    if not duplicate_count:
        return {"duplicates_before": 0, "duplicates_repaired": 0,
                "unique_after": model.terms}
    coefficient = model.coefficient.detach().cpu().numpy()
    used = {tuple(map(int, row)) for row in rows}
    rng = np.random.default_rng(seed)
    keepers: list[int] = []
    freed: list[int] = []
    replacements: list[np.ndarray] = []
    for group in groups:
        members = np.flatnonzero(inverse == group)
        keeper = int(members[np.argmax(np.abs(coefficient[members]))])
        model.coefficient[keeper] = float(coefficient[members].sum())
        keepers.append(keeper)
        for raw_duplicate in members:
            duplicate = int(raw_duplicate)
            if duplicate == keeper:
                continue
            degree = int(model.degree[duplicate])
            model.coefficient[duplicate] = 0.0
            while True:
                support = np.sort(
                    rng.choice(model.n_bits, size=degree, replace=False)
                ).astype(np.int64)
                padded = np.full(model.max_degree, model.n_bits, dtype=np.int64)
                padded[:degree] = support
                key = tuple(map(int, padded))
                if key not in used:
                    used.add(key)
                    break
            freed.append(duplicate)
            replacements.append(support)
    freed_tensor = torch.as_tensor(freed, device=model.theta.device)
    model.theta[freed_tensor] = 0.0
    for row, support in zip(freed, replacements, strict=True):
        model.theta[row, torch.as_tensor(
            support, dtype=torch.long, device=model.theta.device
        )] = 0.02
    affected = torch.as_tensor(keepers + freed, device=model.theta.device)
    for parameter in (model.theta, model.coefficient):
        for state in optimizer.state.get(parameter, {}).values():
            if torch.is_tensor(state) and state.ndim and state.shape == parameter.shape:
                state[affected] = 0
    unique_after = len(np.unique(model.hard_index_rows().numpy(), axis=0))
    if unique_after != model.terms:
        raise RuntimeError("duplicate repair failed")
    return {
        "duplicates_before": duplicate_count,
        "duplicates_repaired": len(freed),
        "unique_after": unique_after,
    }


@torch.no_grad()
def sampled_signature_audit(
    model: OutputSelectorWalshStudent,
    bits: torch.Tensor,
    *,
    character_sample: int = 4096,
    seed: int = 0,
) -> dict[str, float]:
    """Measure equal/negated empirical features on a fixed sampled bank."""
    count = min(model.terms, character_sample)
    bits = bits.float()
    generator = torch.Generator(device=model.theta.device).manual_seed(seed)
    rows = torch.randperm(
        model.terms, device=model.theta.device, generator=generator
    )[:count]
    signatures: set[bytes] = set()
    duplicates = 0
    constants = 0
    for lo in range(0, count, 256):
        selected = rows[lo:lo + 256]
        mask = hard_topk_mask(
            model.theta[selected], model.degree[selected], model.max_degree
        ).to(bits.dtype)
        parity = torch.remainder(bits @ mask.t(), 2).byte().cpu().numpy().T
        for value in parity:
            ones = int(value.sum())
            if ones in (0, len(value)):
                constants += 1
                continue
            packed = np.packbits(value, bitorder="little").tobytes()
            inverse = np.packbits(1 - value, bitorder="little").tobytes()
            key = packed if packed <= inverse else inverse
            duplicates += int(key in signatures)
            signatures.add(key)
    return {
        "sampled_characters": float(count),
        "functional_unique": float(len(signatures)),
        "functional_duplicates": float(duplicates),
        "functional_constants": float(constants),
        "functional_unique_fraction": len(signatures) / max(1, count),
    }


@torch.no_grad()
def support_locality_audit(
    model: OutputSelectorWalshStudent,
    *,
    token_bits: int = 32,
    target_tokens: int = 16,
) -> dict[str, float]:
    """Report whether hard characters cover the field structure they encode."""
    rows = model.hard_index_rows().numpy()
    degree = model.degree.detach().cpu().numpy()
    active = np.arange(model.max_degree)[None, :] < degree[:, None]
    target_stop = token_bits * target_tokens
    in_target = (rows < target_stop) & active
    in_context = (rows >= target_stop) & active
    token = rows // token_bits
    first_token = token[:, :1]
    same_token = np.all((token == first_token) | ~active, axis=1)
    all_target = np.all(in_target | ~active, axis=1)
    crosses = in_target.any(axis=1) & in_context.any(axis=1)
    any_target = in_target.any(axis=1)
    nonsingleton = degree > 1

    def fraction(mask: np.ndarray, base: np.ndarray | None = None) -> float:
        selected = np.ones(len(mask), dtype=bool) if base is None else base
        return float(mask[selected].mean()) if selected.any() else 0.0

    coefficient = (
        model.coefficient.detach().float().cpu().numpy() * model.output_scale
    )
    energy = np.square(coefficient)
    energy_total = max(float(energy.sum()), 1e-20)
    return {
        "nonsingleton_same_token_fraction": fraction(same_token, nonsingleton),
        "nonsingleton_all_target_fraction": fraction(all_target, nonsingleton),
        "nonsingleton_target_context_fraction": fraction(crosses, nonsingleton),
        "nonsingleton_any_target_fraction": fraction(any_target, nonsingleton),
        "same_token_coefficient_energy_fraction": float(
            energy[same_token].sum() / energy_total
        ),
        "all_target_coefficient_energy_fraction": float(
            energy[all_target].sum() / energy_total
        ),
        "target_context_coefficient_energy_fraction": float(
            energy[crosses].sum() / energy_total
        ),
    }


@torch.no_grad()
def support_retention_audit(
    model: OutputSelectorWalshStudent,
    reference_rows: np.ndarray | torch.Tensor,
) -> dict[str, float]:
    """Measure how much of the initialization survives trainable TopK search."""
    current = model.hard_index_rows().numpy()
    reference = np.asarray(reference_rows)
    if reference.shape != current.shape:
        raise ValueError("reference support bank has the wrong shape")
    exact = np.all(current == reference, axis=1)
    jaccard = np.empty(model.terms, dtype=np.float64)
    degree = model.degree.detach().cpu().numpy()
    for value in np.unique(degree):
        selected = degree == value
        overlap = (
            current[selected, :value, None]
            == reference[selected, None, :value]
        ).any(axis=2).sum(axis=1)
        jaccard[selected] = overlap / (2 * value - overlap)
    return {
        "exact_row_fraction": float(exact.mean()),
        "mean_jaccard": float(jaccard.mean()),
        "p50_jaccard": float(np.quantile(jaccard, 0.5)),
        "p10_jaccard": float(np.quantile(jaccard, 0.1)),
    }


def pack_fixed_width(values: np.ndarray, bit_width: int) -> np.ndarray:
    values = np.asarray(values)
    if values.ndim != 1 or not 1 <= bit_width <= 32:
        raise ValueError("invalid fixed-width values")
    unsigned = values.astype(np.uint64, copy=False)
    if np.any(values < 0) or np.any(unsigned >= (1 << bit_width)):
        raise ValueError("integer does not fit bit width")
    positions = np.arange(bit_width, dtype=np.uint64)
    bits = ((unsigned[:, None] >> positions) & 1).astype(np.uint8)
    return np.packbits(bits.reshape(-1), bitorder="little")


def unpack_fixed_width(packed: np.ndarray, count: int,
                       bit_width: int) -> np.ndarray:
    raw = np.asarray(packed, dtype=np.uint8).reshape(-1)
    if len(raw) != (count * bit_width + 7) // 8:
        raise ValueError("packed byte count mismatch")
    bits = np.unpackbits(
        raw, count=count * bit_width, bitorder="little"
    ).reshape(count, bit_width)
    weights = np.left_shift(
        np.uint64(1), np.arange(bit_width, dtype=np.uint64)
    )
    return (bits.astype(np.uint64) @ weights).astype(np.uint32)


def encode_compact_student(state: Mapping[str, Any],
                           block_size: int = 256) -> dict[str, Any]:
    degrees = np.asarray(state["degrees"], dtype=np.uint8)
    indices = np.asarray(state["indices"], dtype=np.uint32)
    coefficient = np.asarray(state["coefficient"], dtype=np.float32)
    n_bits = int(state["n_bits"])
    if len(degrees) != len(coefficient) or int(degrees.sum()) != len(indices):
        raise ValueError("sparse character arrays disagree")
    index_bits = max(1, (n_bits - 1).bit_length())
    blocks = math.ceil(len(coefficient) / block_size)
    scale = np.ones(blocks, dtype=np.float32)
    quantized = np.empty(len(coefficient), dtype=np.float16)
    for block in range(blocks):
        lo, hi = block * block_size, min((block + 1) * block_size, len(coefficient))
        maximum = float(np.abs(coefficient[lo:hi]).max(initial=0.0))
        scale[block] = maximum if maximum > 1e-12 else 1.0
        quantized[lo:hi] = (coefficient[lo:hi] / scale[block]).astype(np.float16)
    return {
        "schema": "centered-binary-output-selector-v1",
        "n_bits": n_bits,
        "degrees": degrees,
        "index_bits": index_bits,
        "index_count": len(indices),
        "packed_indices": pack_fixed_width(indices, index_bits),
        "coefficient_fp16": quantized,
        "coefficient_scale": scale,
        "coefficient_block_size": block_size,
        "bias": float(state["bias"]),
    }


def decode_compact_student(state: Mapping[str, Any]) -> dict[str, Any]:
    if state.get("schema") != "centered-binary-output-selector-v1":
        raise ValueError("unsupported output-selector artifact")
    degrees = np.asarray(state["degrees"], dtype=np.uint8)
    block_size = int(state["coefficient_block_size"])
    quantized = np.asarray(state["coefficient_fp16"], dtype=np.float16)
    scale = np.asarray(state["coefficient_scale"], dtype=np.float32)
    return {
        "schema": state["schema"],
        "n_bits": int(state["n_bits"]),
        "degrees": degrees,
        "indices": unpack_fixed_width(
            state["packed_indices"], int(state["index_count"]),
            int(state["index_bits"]),
        ),
        "coefficient": quantized.astype(np.float32) * np.repeat(
            scale, block_size
        )[:len(quantized)],
        "bias": float(state["bias"]),
    }


@torch.no_grad()
def load_compact_student(model: OutputSelectorWalshStudent,
                         state: Mapping[str, Any]) -> None:
    """Load a quantized hard-support artifact into a live GPU model."""
    decoded = decode_compact_student(state) if "packed_indices" in state else state
    degree = np.asarray(decoded["degrees"], dtype=np.int64)
    if (int(decoded["n_bits"]) != model.n_bits or len(degree) != model.terms
            or np.any(degree != model.degree.detach().cpu().numpy())):
        raise ValueError("artifact shape or degree bank does not match model")
    indices = np.asarray(decoded["indices"], dtype=np.int64)
    offsets = np.concatenate(([0], np.cumsum(degree)))
    model.theta.zero_()
    for lo in range(0, model.terms, model.char_chunk):
        hi = min(lo + model.char_chunk, model.terms)
        block_degree = degree[lo:hi]
        width = int(block_degree.max(initial=0))
        padded = np.full((hi - lo, width), model.n_bits, dtype=np.int64)
        for row, global_row in enumerate(range(lo, hi)):
            padded[row, :block_degree[row]] = indices[
                offsets[global_row]:offsets[global_row + 1]
            ]
        tensor = torch.from_numpy(padded).to(model.theta.device)
        active = tensor < model.n_bits
        local_row = torch.arange(
            hi - lo, device=model.theta.device
        )[:, None].expand_as(tensor)[active]
        model.theta[lo:hi][local_row, tensor[active]] = 0.02
    coefficient = torch.as_tensor(
        decoded["coefficient"], device=model.coefficient.device,
        dtype=model.coefficient.dtype,
    ) / model.output_scale
    model.coefficient.copy_(coefficient)
    model.bias.fill_(float(decoded["bias"]))


def sparse_gap(bits: np.ndarray, state: Mapping[str, Any],
               chunk: int = 4096) -> np.ndarray:
    if "packed_indices" in state:
        state = decode_compact_student(state)
    x = np.asarray(bits, dtype=np.uint8)
    degree = np.asarray(state["degrees"], dtype=np.int64)
    indices = np.asarray(state["indices"], dtype=np.int64)
    coefficient = np.asarray(state["coefficient"], dtype=np.float32)
    offsets = np.concatenate(([0], np.cumsum(degree)))
    width = int(degree.max(initial=0))
    padded = np.zeros((len(degree), width), dtype=np.int64)
    active = np.arange(width)[None, :] < degree[:, None]
    rows = np.repeat(np.arange(len(degree)), degree)
    columns = np.arange(len(indices)) - np.repeat(offsets[:-1], degree)
    padded[rows, columns] = indices
    output = np.full(len(x), float(state["bias"]), dtype=np.float32)
    for lo in range(0, len(degree), chunk):
        hi = min(lo + chunk, len(degree))
        parity = np.bitwise_xor.reduce(
            x[:, padded[lo:hi]], axis=2,
            where=active[None, lo:hi], initial=0,
        )
        output += (1.0 - 2.0 * parity.astype(np.float32)) @ coefficient[lo:hi]
    return output


def sparse_scores(bits: np.ndarray, state: Mapping[str, Any],
                  chunk: int = 4096) -> np.ndarray:
    gap = sparse_gap(bits, state, chunk)
    return np.stack((-0.5 * gap, 0.5 * gap), axis=-1)
