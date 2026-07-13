"""Part 2: affine sparse-Fourier controls for tokenizer-native Dataset GL.

The exact identities in this module are valid over a prime field.  The toy
q-SFT control deliberately uses chosen point evaluations.  It is kept separate
from the natural-sample/prefix-CSAMP implementation in :mod:`qwen_argl`.
"""

from __future__ import annotations

import hashlib
import itertools
import json
import math
from pathlib import Path

import numpy as np


PROTOCOL_NAME = "part2-large-q-affine-sft-v1"


def protocol_path() -> Path:
    return Path(__file__).resolve().parents[1] / "part2_protocol.json"


def load_protocol(path: str | Path | None = None) -> tuple[dict, str]:
    path = Path(path) if path is not None else protocol_path()
    raw = path.read_bytes()
    value = json.loads(raw)
    if value.get("protocol") != PROTOCOL_NAME:
        raise ValueError(f"unexpected Part 2 protocol: {value.get('protocol')!r}")
    return value, hashlib.sha256(raw).hexdigest()


def is_prime(q: int) -> bool:
    if q < 2:
        return False
    if q % 2 == 0:
        return q == 2
    limit = math.isqrt(q)
    return all(q % d for d in range(3, limit + 1, 2))


def require_prime(q: int) -> None:
    if not is_prime(int(q)):
        raise ValueError("Part 2 affine linear algebra requires a prime q")


def modular_rank(matrix, q: int) -> int:
    """Gaussian-elimination rank over the prime field Z_q."""
    require_prime(q)
    a = np.asarray(matrix, dtype=np.int64).copy() % q
    if a.ndim != 2:
        raise ValueError("matrix must be two dimensional")
    rank = 0
    for col in range(a.shape[1]):
        pivots = np.flatnonzero(a[rank:, col])
        if not len(pivots):
            continue
        pivot = rank + int(pivots[0])
        a[[rank, pivot]] = a[[pivot, rank]]
        a[rank] = a[rank] * pow(int(a[rank, col]), q - 2, q) % q
        for row in range(a.shape[0]):
            if row != rank and a[row, col]:
                a[row] = (a[row] - a[row, col] * a[rank]) % q
        rank += 1
        if rank == a.shape[0]:
            break
    return rank


def random_full_column_rank(rng: np.random.Generator, n: int, b: int, q: int) -> np.ndarray:
    require_prime(q)
    if not 0 < b <= n:
        raise ValueError("expected 0 < b <= n")
    while True:
        matrix = rng.integers(0, q, size=(n, b), dtype=np.int64)
        if modular_rank(matrix, q) == b:
            return matrix


def group_grid(q: int, n: int) -> np.ndarray:
    if q <= 0 or n <= 0:
        raise ValueError("q and n must be positive")
    return np.asarray(list(itertools.product(range(q), repeat=n)), dtype=np.int64)


def ravel_group(points, q: int) -> np.ndarray:
    points = np.asarray(points, dtype=np.int64) % q
    if points.ndim != 2:
        raise ValueError("points must have shape (rows, dimensions)")
    return np.ravel_multi_index(points.T, (q,) * points.shape[1])


def dataset_coefficients(distribution, values, q: int, n: int) -> np.ndarray:
    """All a_D(k)=sum_x D(x)F(x)conj(chi_k(x)) via an exact q-ary DFT."""
    distribution = np.asarray(distribution, dtype=np.float64)
    values = np.asarray(values, dtype=np.complex128)
    size = q ** n
    if distribution.shape != (size,) or values.ndim != 2 or values.shape[0] != size:
        raise ValueError("distribution/values do not match q^n")
    if np.any(distribution < 0) or not np.isclose(distribution.sum(), 1.0):
        raise ValueError("distribution must be a probability vector")
    m = values.shape[1]
    weighted = (distribution[:, None] * values).reshape((q,) * n + (m,))
    return np.fft.fftn(weighted, axes=tuple(range(n))).reshape(size, m)


def affine_bins_from_time(h, M, d, q: int) -> np.ndarray:
    """U_{M,d}(j) from chosen evaluations of h on the complete affine grid."""
    require_prime(q)
    h = np.asarray(h, dtype=np.complex128)
    M = np.asarray(M, dtype=np.int64) % q
    d = np.asarray(d, dtype=np.int64) % q
    n, b = M.shape
    if modular_rank(M, q) != b or d.shape != (n,):
        raise ValueError("M must have full column rank and d must match it")
    if h.ndim != 2 or h.shape[0] != q ** n:
        raise ValueError("h must have q^n rows")
    ell = group_grid(q, b)
    points = (ell @ M.T + d) % q
    samples = h[ravel_group(points, q)]
    return (np.fft.fftn(samples.reshape((q,) * b + (h.shape[1],)),
                        axes=tuple(range(b))) / (q ** b)).reshape(q ** b, h.shape[1])


def affine_bins_from_coefficients(coefficients, M, d, q: int) -> np.ndarray:
    """Right side of the affine alias identity, grouped by M^T k."""
    coefficients = np.asarray(coefficients, dtype=np.complex128)
    M = np.asarray(M, dtype=np.int64) % q
    d = np.asarray(d, dtype=np.int64) % q
    n, b = M.shape
    frequencies = group_grid(q, n)
    if coefficients.shape[0] != len(frequencies):
        raise ValueError("coefficient table does not match q^n")
    bins = (frequencies @ M) % q
    phases = np.exp(2j * np.pi * ((frequencies @ d) % q) / q)
    out = np.zeros((q ** b, coefficients.shape[1]), dtype=np.complex128)
    np.add.at(out, ravel_group(bins, q), coefficients * phases[:, None])
    return out


def affine_energy_buckets(coefficients, M, q: int) -> np.ndarray:
    coefficients = np.asarray(coefficients, dtype=np.complex128)
    M = np.asarray(M, dtype=np.int64) % q
    n, b = M.shape
    frequencies = group_grid(q, n)
    if coefficients.shape[0] != len(frequencies):
        raise ValueError("coefficient table does not match q^n")
    out = np.zeros(q ** b, dtype=np.float64)
    np.add.at(out, ravel_group((frequencies @ M) % q, q),
              np.sum(np.abs(coefficients) ** 2, axis=1))
    return out


def affine_energy_from_pairs(distribution, values, M, q: int) -> np.ndarray:
    """Pair side of Proposition part2-affine-pair, evaluated without N^2 storage."""
    require_prime(q)
    distribution = np.asarray(distribution, dtype=np.float64)
    values = np.asarray(values, dtype=np.complex128)
    M = np.asarray(M, dtype=np.int64) % q
    n, b = M.shape
    points = group_grid(q, n)
    ell = group_grid(q, b)
    if distribution.shape != (len(points),) or values.shape[0] != len(points):
        raise ValueError("distribution/values do not match M")
    pair_by_difference = np.empty(len(ell), dtype=np.complex128)
    for row, t in enumerate(ell):
        shifted = (points + M @ t) % q
        shifted_index = ravel_group(shifted, q)
        inner = np.sum(values * np.conjugate(values[shifted_index]), axis=1)
        pair_by_difference[row] = np.sum(
            distribution * distribution[shifted_index] * inner
        )
    # q^(n-b) times the positive, unnormalized transform over t.
    transformed = (q ** n) * np.fft.ifftn(
        pair_by_difference.reshape((q,) * b)
    ).reshape(-1)
    return transformed


def _sparse_time_values(points, support, coefficients, q: int) -> np.ndarray:
    points = np.asarray(points, dtype=np.int64)
    support = np.asarray(support, dtype=np.int64)
    dot = (points @ support.T) % q
    phase = np.exp(2j * np.pi * dot / q)
    return phase @ np.asarray(coefficients, dtype=np.complex128)


def _decode_singleton(observation, M, bin_vector, q: int, tolerance: float):
    """Noiseless q-SFT singleton detector for offsets (0,e_1,...,e_n)."""
    observation = np.asarray(observation, dtype=np.complex128)
    if np.max(np.abs(observation)) <= tolerance:
        return None
    base = observation[0]
    if abs(base) <= tolerance:
        return False
    ratios = observation[1:] / base
    angles = np.mod(np.angle(ratios), 2 * np.pi)
    k = np.rint(angles * q / (2 * np.pi)).astype(np.int64) % q
    predicted = base * np.exp(2j * np.pi * k / q)
    if (np.max(np.abs(predicted - observation[1:])) > tolerance
            or np.any((M.T @ k) % q != bin_vector % q)):
        return False
    return k, base


def noiseless_qsft_control(q: int, n: int, b: int, sparsity: int, seed: int,
                           groups: int = 4, tolerance: float = 2e-8) -> dict:
    """Small chosen-point implementation of q-SFT affine bins and peeling.

    This is deliberately an oracle control.  It evaluates every point on each
    designed affine grid and must never be used as a prefix-CSAMP result.
    """
    require_prime(q)
    if sparsity >= q ** n:
        raise ValueError("sparsity must be smaller than the group")
    rng = np.random.default_rng(seed)
    support_index = rng.choice(q ** n, size=sparsity, replace=False)
    all_frequencies = group_grid(q, n)
    support = all_frequencies[support_index]
    coefficient_phases = rng.integers(0, q, size=sparsity)
    values = np.exp(2j * np.pi * coefficient_phases / q)
    matrices = [random_full_column_rank(rng, n, b, q) for _ in range(groups)]
    offsets = np.vstack([np.zeros((1, n), dtype=np.int64), np.eye(n, dtype=np.int64)])
    ell = group_grid(q, b)
    bin_observations = np.empty((groups, n + 1, q ** b), dtype=np.complex128)
    query_count = 0
    for c, matrix in enumerate(matrices):
        for p, offset in enumerate(offsets):
            points = (ell @ matrix.T + offset) % q
            samples = _sparse_time_values(points, support, values, q)
            bin_observations[c, p] = (
                np.fft.fftn(samples.reshape((q,) * b)) / (q ** b)
            ).reshape(-1)
            query_count += len(points)

    residual = bin_observations.copy()
    queue: list[tuple[int, int, np.ndarray, complex]] = []
    for c, matrix in enumerate(matrices):
        for flat, bin_vector in enumerate(group_grid(q, b)):
            decoded = _decode_singleton(residual[c, :, flat], matrix, bin_vector, q, tolerance)
            if decoded not in (None, False):
                queue.append((c, flat, decoded[0], decoded[1]))

    recovered: dict[tuple[int, ...], complex] = {}
    while queue:
        _, _, k, coefficient = queue.pop()
        key = tuple(int(x) for x in k)
        if key in recovered:
            continue
        recovered[key] = complex(coefficient)
        phase = np.r_[1.0 + 0.0j, np.exp(2j * np.pi * k / q)]
        for c, matrix in enumerate(matrices):
            bin_vector = (matrix.T @ k) % q
            flat = int(ravel_group(bin_vector[None], q)[0])
            residual[c, :, flat] -= coefficient * phase
            decoded = _decode_singleton(
                residual[c, :, flat], matrix, bin_vector, q, tolerance
            )
            if decoded not in (None, False):
                queue.append((c, flat, decoded[0], decoded[1]))

    truth = {tuple(int(x) for x in k): complex(v) for k, v in zip(support, values)}
    common = truth.keys() & recovered.keys()
    max_value_error = max((abs(truth[k] - recovered[k]) for k in common), default=math.inf)
    return {
        "q": q,
        "n": n,
        "b": b,
        "sparsity": sparsity,
        "groups": groups,
        "offsets_per_group": n + 1,
        "chosen_point_queries": query_count,
        "recovered": len(common),
        "false_positives": len(recovered.keys() - truth.keys()),
        "missed": len(truth.keys() - recovered.keys()),
        "max_value_error": float(max_value_error),
        "max_degree": int(np.max(np.count_nonzero(support, axis=1))),
        "all_recovered": recovered.keys() == truth.keys() and max_value_error < 1e-6,
        "oracle": "arbitrary chosen point evaluations on complete affine grids",
        "theorem_status": "oracle_control",
    }


def run_toy_audit(protocol_file: str | Path | None = None) -> dict:
    protocol, protocol_hash = load_protocol(protocol_file)
    cfg = protocol["toy"]
    q, n, b = int(cfg["q"]), int(cfg["n"]), int(cfg["b"])
    rng = np.random.default_rng(int(cfg["seed"]))
    points = group_grid(q, n)
    raw_distribution = rng.gamma(shape=0.7, scale=1.0, size=len(points))
    distribution = raw_distribution / raw_distribution.sum()
    values = rng.gamma(shape=0.8, scale=1.0,
                       size=(len(points), int(cfg["output_dimension"])))
    values /= values.sum(axis=1, keepdims=True)
    coefficients = dataset_coefficients(distribution, values, q, n)
    h = (q ** n) * distribution[:, None] * values
    matrix = random_full_column_rank(rng, n, b, q)
    offset = rng.integers(0, q, size=n, dtype=np.int64)
    time_bins = affine_bins_from_time(h, matrix, offset, q)
    coefficient_bins = affine_bins_from_coefficients(coefficients, matrix, offset, q)
    energy_coefficients = affine_energy_buckets(coefficients, matrix, q)
    energy_pairs = affine_energy_from_pairs(distribution, values, matrix, q)

    check_indices = rng.choice(len(points), size=64, replace=False)
    direct = np.empty((len(check_indices), values.shape[1]), dtype=np.complex128)
    for row, index in enumerate(check_indices):
        phase = np.exp(-2j * np.pi * ((points @ points[index]) % q) / q)
        direct[row] = np.sum(distribution[:, None] * values * phase[:, None], axis=0)
    density_error = np.max(np.abs(direct - coefficients[check_indices]))
    alias_error = np.max(np.abs(time_bins - coefficient_bins))
    energy_error = np.max(np.abs(energy_coefficients - energy_pairs))
    qsft_trials = [
        noiseless_qsft_control(
            q=q, n=n, b=b, sparsity=int(cfg["sparsity"]),
            seed=int(cfg["seed"]) + 1 + trial,
            groups=int(cfg["groups"]),
        )
        for trial in range(int(cfg["trials"]))
    ]
    qsft = {
        "algorithm": cfg["algorithm"],
        "trials": len(qsft_trials),
        "successful_trials": sum(row["all_recovered"] for row in qsft_trials),
        "all_trials_recovered": all(row["all_recovered"] for row in qsft_trials),
        "max_value_error": max(row["max_value_error"] for row in qsft_trials),
        "max_false_positives": max(row["false_positives"] for row in qsft_trials),
        "max_missed": max(row["missed"] for row in qsft_trials),
        "chosen_point_queries_per_trial": qsft_trials[0]["chosen_point_queries"],
        "oracle": qsft_trials[0]["oracle"],
        "theorem_status": "oracle_control",
        "trial_reports": qsft_trials,
    }
    return {
        "protocol": protocol["protocol"],
        "protocol_sha256": protocol_hash,
        "date": protocol["date_iso"],
        "q": q,
        "n": n,
        "b": b,
        "output_dimension": int(cfg["output_dimension"]),
        "density_transform_max_error": float(density_error),
        "affine_alias_max_error": float(alias_error),
        "affine_energy_max_error": float(energy_error),
        "q_sft": qsft,
        "theorem_status": "oracle_control",
        "strict_dataset_gl": False,
    }


def affine_line_membership(differences, directions, q: int) -> tuple[np.ndarray, np.ndarray]:
    """Return whether each difference lies in span(direction), plus its scalar."""
    require_prime(q)
    differences = np.asarray(differences, dtype=np.int64) % q
    directions = np.asarray(directions, dtype=np.int64) % q
    if differences.shape != directions.shape or differences.ndim != 2:
        raise ValueError("differences and directions must have matching matrix shapes")
    member = np.zeros(len(differences), dtype=bool)
    scalars = np.zeros(len(differences), dtype=np.int64)
    for row, (difference, direction) in enumerate(zip(differences, directions)):
        nonzero = np.flatnonzero(direction)
        if not len(nonzero):
            raise ValueError("line directions must be nonzero")
        pivot = int(nonzero[0])
        scalar = int(difference[pivot]) * pow(int(direction[pivot]), q - 2, q) % q
        scalars[row] = scalar
        member[row] = np.array_equal(difference, scalar * direction % q)
    return member, scalars


def normalized_importance_ess(log_weights, axis: int = -1) -> np.ndarray:
    log_weights = np.asarray(log_weights, dtype=np.float64)
    maximum = np.max(log_weights, axis=axis, keepdims=True)
    weights = np.exp(log_weights - maximum)
    ess = np.sum(weights, axis=axis) ** 2 / np.sum(weights ** 2, axis=axis)
    return ess / log_weights.shape[axis]


_COMPILED_AFFINE_POINTS = None


def torch_affine_points(offsets, directions, scalars, q: int):
    """Compiled native-Z_q construction, with no bit representation."""
    import torch

    global _COMPILED_AFFINE_POINTS

    def kernel(d, m, ell, modulus: int):
        return torch.remainder(
            d[:, None, :] + ell[:, :, None] * m[:, None, :], modulus
        )

    if _COMPILED_AFFINE_POINTS is None:
        _COMPILED_AFFINE_POINTS = torch.compile(kernel, fullgraph=True, dynamic=False)
    return _COMPILED_AFFINE_POINTS(offsets, directions, scalars, int(q))


def sample_rollout_with_log_prob(model, prefixes, new_tokens: int, q: int, generator):
    """Natural autoregressive generation plus the exact sampled-sequence log mass."""
    import torch

    if prefixes.ndim != 2 or new_tokens <= 0:
        raise ValueError("prefixes must be a matrix and new_tokens must be positive")
    from .qwen_argl import tokenizer_softmax

    log_mass = torch.zeros(len(prefixes), dtype=torch.float64, device=prefixes.device)
    made = []
    with torch.inference_mode():
        out = model(input_ids=prefixes, use_cache=True, return_dict=True, logits_to_keep=1)
        past = out.past_key_values
        logits = out.logits[:, -1, :q]
        for _ in range(new_tokens):
            probabilities = tokenizer_softmax(logits, q)
            token = torch.multinomial(probabilities, 1, generator=generator)
            log_mass += torch.log(
                probabilities.gather(1, token).squeeze(1).clamp_min(1e-45)
            ).double()
            made.append(token)
            out = model(input_ids=token, past_key_values=past, use_cache=True,
                        return_dict=True, logits_to_keep=1)
            past = out.past_key_values
            logits = out.logits[:, -1, :q]
    return torch.cat(made, dim=1), log_mass


def teacher_forced_log_mass(model, prefixes, sequences, q: int):
    """Compute log D_z(x) for arbitrary chosen x without sampling x."""
    import torch

    if prefixes.ndim != 2 or sequences.ndim != 2 or len(prefixes) != len(sequences):
        raise ValueError("prefixes and sequences must be aligned matrices")
    from .qwen_argl import tokenizer_softmax

    log_mass = torch.zeros(len(prefixes), dtype=torch.float64, device=prefixes.device)
    with torch.inference_mode():
        out = model(input_ids=prefixes, use_cache=True, return_dict=True, logits_to_keep=1)
        past = out.past_key_values
        logits = out.logits[:, -1, :q]
        for column in range(sequences.shape[1]):
            token = sequences[:, column:column + 1]
            probabilities = tokenizer_softmax(logits, q)
            log_mass += torch.log(
                probabilities.gather(1, token).squeeze(1).clamp_min(1e-45)
            ).double()
            out = model(input_ids=token, past_key_values=past, use_cache=True,
                        return_dict=True, logits_to_keep=1)
            past = out.past_key_values
            logits = out.logits[:, -1, :q]
    return log_mass


def x_only_label_summary(model, sequences, q: int, batch: int = 16) -> dict:
    """Read the token-129 distribution; no token 129 is sampled."""
    import torch
    from .qwen_argl import x_only_teacher_logits

    winners, top_probabilities, margins = [], [], []
    for lo in range(0, len(sequences), batch):
        logits = x_only_teacher_logits(model, sequences[lo:lo + batch], q).float()
        probabilities = torch.softmax(logits, dim=-1)
        top = torch.topk(probabilities, 2, dim=1)
        winners.append(top.indices[:, 0].cpu())
        top_probabilities.append(top.values[:, 0].cpu())
        margins.append((top.values[:, 0] - top.values[:, 1]).cpu())
    winner = torch.cat(winners).numpy()
    top_probability = torch.cat(top_probabilities).numpy()
    margin = torch.cat(margins).numpy()
    return {
        "winner": winner,
        "unique_winners": int(len(np.unique(winner))),
        "top_probability_mean": float(top_probability.mean()),
        "top_probability_quantiles": np.quantile(top_probability, [0, 0.5, 0.9, 1]).tolist(),
        "top_two_margin_mean": float(margin.mean()),
    }


def run_qwen_affine_audit(model, prefixes, q: int,
                          protocol_file: str | Path | None = None,
                          generation_batch: int = 8,
                          query_batch: int = 16) -> dict:
    """Natural-generator audit plus a separately labeled chosen-affine control."""
    import time
    import torch

    protocol, protocol_hash = load_protocol(protocol_file)
    cfg = protocol["qwen_audit"]
    n = int(protocol["n"])
    count = int(cfg["contexts"])
    affine_count = int(cfg["affine_contexts"])
    points_per_context = int(cfg["points_per_context"])
    if q != int(protocol["q"]) or n != 128:
        raise ValueError("teacher alphabet/context length does not match Part 2 protocol")
    prefixes = np.asarray(prefixes[:count], dtype=np.int64)
    if prefixes.shape != (count, 128) or not 0 < affine_count <= count:
        raise ValueError("Part 2 requires the registered number of 128-token prefixes")

    device = next(model.parameters()).device
    generator = torch.Generator(device=device).manual_seed(int(cfg["seed"]))
    natural = torch.empty((count, 2, n), dtype=torch.long, device=device)
    natural_log_mass = torch.empty((count, 2), dtype=torch.float64, device=device)
    generated_started = time.time()
    for lo in range(0, count, generation_batch):
        z = torch.as_tensor(prefixes[lo:lo + generation_batch], device=device)
        paired_z = z.repeat_interleave(2, dim=0)
        x, log_mass = sample_rollout_with_log_prob(model, paired_z, n, q, generator)
        rows = len(z)
        natural[lo:lo + rows] = x.reshape(rows, 2, n)
        natural_log_mass[lo:lo + rows] = log_mass.reshape(rows, 2)
    generation_seconds = time.time() - generated_started

    # Prefix-compatible control: both full strings share 127 generated tokens
    # and independently sample only their final token from a forked state.
    prefix_control = torch.empty((affine_count, 2, n), dtype=torch.long, device=device)
    for lo in range(0, affine_count, generation_batch):
        z = torch.as_tensor(prefixes[lo:lo + generation_batch], device=device)
        left, _ = sample_rollout_with_log_prob(model, z, n - 1, q, generator)
        forked = torch.cat([z, left], dim=1).repeat_interleave(2, dim=0)
        last, _ = sample_rollout_with_log_prob(model, forked, 1, q, generator)
        rows = len(z)
        prefix_control[lo:lo + rows] = torch.cat([
            left[:, None].expand(-1, 2, -1), last.reshape(rows, 2, 1)
        ], dim=2)

    np_rng = np.random.default_rng(int(cfg["seed"]) + 1)
    directions = np_rng.integers(0, q, size=(affine_count, n), dtype=np.int64)
    zero = ~np.any(directions, axis=1)
    directions[zero, 0] = 1
    difference = (natural[:affine_count, 1] - natural[:affine_count, 0]).cpu().numpy() % q
    dense_member, _ = affine_line_membership(difference, directions, q)
    exact_collision = np.all(difference == 0, axis=1)

    scalars = np_rng.integers(0, q, size=(affine_count, points_per_context), dtype=np.int64)
    split = affine_count // 2
    scalars[:split, 0] = 0
    anchored_offsets = natural[:split, 0].detach().clone()
    random_offsets = torch.as_tensor(
        np_rng.integers(0, q, size=(affine_count - split, n), dtype=np.int64),
        device=device,
    )
    offsets = torch.cat([anchored_offsets, random_offsets], dim=0)
    direction_tensor = torch.as_tensor(directions, device=device)
    scalar_tensor = torch.as_tensor(scalars, device=device)
    affine_points = torch_affine_points(offsets, direction_tensor, scalar_tensor, q)
    family = np.asarray(["anchored"] * split + ["random"] * (affine_count - split))

    flat_points = affine_points.reshape(-1, n)
    flat_prefixes = torch.as_tensor(
        np.repeat(prefixes[:affine_count], points_per_context, axis=0), device=device
    )
    chosen_log_mass = torch.empty(len(flat_points), dtype=torch.float64, device=device)
    query_started = time.time()
    for lo in range(0, len(flat_points), query_batch):
        chosen_log_mass[lo:lo + query_batch] = teacher_forced_log_mass(
            model, flat_prefixes[lo:lo + query_batch], flat_points[lo:lo + query_batch], q
        )
    label_summary = x_only_label_summary(model, flat_points, q, batch=query_batch)
    query_seconds = time.time() - query_started
    line_log_mass = chosen_log_mass.reshape(affine_count, points_per_context).cpu().numpy()
    ess = normalized_importance_ess(line_log_mass, axis=1)

    natural_labels = x_only_label_summary(
        model, natural.reshape(-1, n), q, batch=query_batch
    )
    natural_winner = natural_labels.pop("winner").reshape(count, 2)
    affine_winner = label_summary.pop("winner")
    points_per_second = len(flat_points) / query_seconds
    scenario = protocol["complexity_scenario"]
    per_group = (n + int(bool(scenario["fixed_zero_delay"]))) * q
    projected = per_group * int(scenario["groups_for_cost_projection"])

    natural_cpu = natural.detach().cpu().numpy().astype(np.int32)
    affine_cpu = flat_points.detach().cpu().numpy().astype(np.int32)
    return {
        "protocol": protocol["protocol"],
        "protocol_sha256": protocol_hash,
        "date": protocol["date_iso"],
        "q": q,
        "n": n,
        "theorem_status": protocol["affine"]["expected_theorem_status"],
        "strict_dataset_gl": False,
        "token_129_sampled_for_label": False,
        "natural_generator": {
            "contexts": count,
            "continuations_per_context": 2,
            "temperature": protocol["sampling"]["temperature"],
            "top_k": protocol["sampling"]["top_k"],
            "top_p": protocol["sampling"]["top_p"],
            "generation_seconds": generation_seconds,
            "log_mass_quantiles": np.quantile(
                natural_log_mass.cpu().numpy(), [0, 0.5, 0.9, 1]
            ).tolist(),
            "exact_sequence_collisions": int(exact_collision.sum()),
            "paired_argmax_agreement": float((natural_winner[:, 0] == natural_winner[:, 1]).mean()),
            "token_sha256": hashlib.sha256(natural_cpu.tobytes()).hexdigest(),
            "label_summary": natural_labels,
        },
        "prefix_compatible_control": {
            "contexts": affine_count,
            "shared_generated_prefix_tokens": n - 1,
            "pairs_in_final_coordinate_subspace": int(np.all(
                (prefix_control[:, 1] - prefix_control[:, 0]).cpu().numpy()[:, :-1] == 0,
                axis=1,
            ).sum()),
        },
        "dense_affine_pair_event": {
            "dimension": 1,
            "constraints": n - 1,
            "memberships": int(dense_member.sum()),
            "pairs": affine_count,
            "uniform_nonzero_difference_probability_exact": f"({q}-1)/({q}^{n}-1)",
            "uniform_nonzero_difference_log10_probability": (
                math.log10(q - 1) - math.log10(q) * n
            ),
        },
        "chosen_affine_point_control": {
            "contexts": affine_count,
            "points_per_context": points_per_context,
            "families": {
                name: {
                    "lines": int(np.sum(family == name)),
                    "log_mass_quantiles": np.quantile(
                        line_log_mass[family == name], [0, 0.5, 0.9, 1]
                    ).tolist(),
                    "normalized_ess_quantiles": np.quantile(
                        ess[family == name], [0, 0.5, 0.9, 1]
                    ).tolist(),
                }
                for name in ("anchored", "random")
            },
            "query_seconds_including_x_only_labels": query_seconds,
            "measured_points_per_second": points_per_second,
            "point_sha256": hashlib.sha256(affine_cpu.tobytes()).hexdigest(),
            "argmax_sha256": hashlib.sha256(affine_winner.astype(np.int32).tobytes()).hexdigest(),
            "label_summary": label_summary,
        },
        "q_sft_cost_projection": {
            "sparsity": int(scenario["sparsity"]),
            "subsampling_dimension": int(scenario["subsampling_dimension"]),
            "chosen_values_per_group_including_zero_delay": per_group,
            "registered_groups": int(scenario["groups_for_cost_projection"]),
            "projected_chosen_values": projected,
            "projected_gpu_seconds_at_measured_throughput": projected / points_per_second,
            "warning": "throughput projection only; no full affine grid was launched",
        },
        "frequencies": [],
    }
