"""Data-only image spectra in cosine and Gaussian-characteristic bases.

The DCT-II profile measures spatial-frequency energy of the images themselves.
The Gaussian profile rank-Gaussianizes patch coordinates and expands their
dependence relative to product Gaussian measure.  In that measure the normalized
Hermite polynomials are the characteristic orthonormal basis; the off-diagonal
degree-two coefficients are ordinary Gaussianized correlations.

Neither object is silently equated with the categorical next-token spectrum.
They are modality-specific descriptors consumed by the same held-out prediction
protocol.
"""

from __future__ import annotations

import math

import numpy as np
from scipy.fft import dctn
from scipy.special import ndtri
from scipy.stats import rankdata


def _weighted_quantile(values: np.ndarray, weights: np.ndarray, q: float) -> float | None:
    total = float(weights.sum())
    if total <= 0.0:
        return None
    order = np.argsort(values, kind="stable")
    cumulative = np.cumsum(weights[order])
    index = int(np.searchsorted(cumulative, q * total, side="left"))
    return float(values[order[min(index, len(order) - 1)]])


def dct_image_profile(images: np.ndarray) -> dict:
    """Parseval-normalized 2D DCT-II energy profile for grayscale images."""
    values = np.asarray(images, dtype=np.float64)
    if values.ndim != 3 or min(values.shape) < 2:
        raise ValueError("images must have shape (N,H,W)")
    if not np.all(np.isfinite(values)):
        raise ValueError("images contain non-finite values")
    coefficients = dctn(values, axes=(-2, -1), norm="ortho", type=2)
    energy = np.mean(coefficients * coefficients, axis=0)
    total = float(energy.sum())
    h, w = energy.shape
    u = np.arange(h, dtype=float) / max(1, h - 1)
    v = np.arange(w, dtype=float) / max(1, w - 1)
    radius = np.sqrt(u[:, None] ** 2 + v[None, :] ** 2) / math.sqrt(2.0)
    flat_energy = energy.reshape(-1)
    flat_radius = radius.reshape(-1)
    probabilities = flat_energy / total if total > 0.0 else np.zeros_like(flat_energy)
    entropy = float(
        -sum(probability * math.log2(probability) for probability in probabilities if probability > 0)
    )
    dc = float(energy[0, 0])
    non_dc = max(0.0, total - dc)
    non_dc_energy = flat_energy.copy()
    non_dc_energy[0] = 0.0
    non_dc_probabilities = (
        non_dc_energy / non_dc if non_dc > 0.0 else np.zeros_like(non_dc_energy)
    )
    non_dc_entropy = float(
        -sum(
            probability * math.log2(probability)
            for probability in non_dc_probabilities
            if probability > 0
        )
    )
    return {
        "basis": "orthonormal two-dimensional DCT-II",
        "shape": [h, w],
        "total_square_energy": total,
        "parseval_pixel_square_energy": float(np.mean(np.sum(values * values, axis=(1, 2)))),
        "dc_energy": dc,
        "non_dc_energy": non_dc,
        "dc_concentration": dc / total if total > 0.0 else None,
        "low_frequency_concentration_r_leq_quarter": float(
            flat_energy[flat_radius <= 0.25].sum() / total
        )
        if total > 0.0
        else None,
        "high_frequency_tail_r_gt_half": float(
            flat_energy[flat_radius > 0.5].sum() / total
        )
        if total > 0.0
        else None,
        "frequency_centroid": float(np.dot(probabilities, flat_radius))
        if total > 0.0
        else None,
        "non_dc_frequency_centroid": float(
            np.dot(non_dc_probabilities, flat_radius)
        )
        if non_dc > 0.0
        else None,
        "frequency_radius_50": _weighted_quantile(flat_radius, flat_energy, 0.5),
        "frequency_radius_90": _weighted_quantile(flat_radius, flat_energy, 0.9),
        "non_dc_frequency_radius_50": _weighted_quantile(
            flat_radius, non_dc_energy, 0.5
        ),
        "non_dc_frequency_radius_90": _weighted_quantile(
            flat_radius, non_dc_energy, 0.9
        ),
        "spectral_entropy_bits": entropy,
        "non_dc_spectral_entropy_bits": non_dc_entropy,
    }


def _patches(images: np.ndarray, patch: int) -> np.ndarray:
    n, h, w = images.shape
    if h % patch or w % patch:
        raise ValueError("patch size must divide both image dimensions")
    gh, gw = h // patch, w // patch
    return (
        images.reshape(n, gh, patch, gw, patch)
        .transpose(0, 1, 3, 2, 4)
        .reshape(n * gh * gw, patch * patch)
    )


def gaussian_hermite_patch_profile(
    images: np.ndarray,
    *,
    patch: int = 8,
    max_patches: int = 100_000,
    seed: int = 0,
) -> dict:
    """Resolved degree-two Hermite dependence and its spatial locality."""
    values = np.asarray(images, dtype=np.float64)
    if values.ndim != 3:
        raise ValueError("images must have shape (N,H,W)")
    patches = _patches(values, patch)
    if len(patches) > max_patches:
        rng = np.random.default_rng(seed)
        selected = np.sort(rng.choice(len(patches), size=max_patches, replace=False))
        patches = patches[selected]
    if len(patches) < 100:
        raise ValueError("too few patches for a Gaussian-Hermite profile")

    gaussian = np.empty_like(patches, dtype=np.float64)
    denominator = len(patches) + 1.0
    for column in range(patches.shape[1]):
        ranks = rankdata(patches[:, column], method="average")
        probabilities = np.clip((ranks - 0.5) / denominator, 1e-6, 1.0 - 1e-6)
        gaussian[:, column] = ndtri(probabilities)
    gaussian -= gaussian.mean(axis=0, keepdims=True)
    scales = gaussian.std(axis=0, ddof=0)
    gaussian /= np.where(scales > 1e-12, scales, 1.0)
    correlation = gaussian.T @ gaussian / len(gaussian)

    left, right = np.triu_indices(patch * patch, k=1)
    coefficients = correlation[left, right]
    energy = coefficients * coefficients
    left_xy = np.column_stack((left // patch, left % patch))
    right_xy = np.column_stack((right // patch, right % patch))
    distances = np.linalg.norm(left_xy - right_xy, axis=1)
    total = float(energy.sum())
    probabilities = energy / total if total > 0.0 else np.zeros_like(energy)
    entropy = float(
        -sum(probability * math.log2(probability) for probability in probabilities if probability > 0)
    )
    eigenvalues = np.maximum(np.linalg.eigvalsh(correlation), 0.0)
    trace = float(eigenvalues.sum())
    effective_rank = (
        trace * trace / float(np.dot(eigenvalues, eigenvalues))
        if np.dot(eigenvalues, eigenvalues) > 0.0
        else None
    )
    return {
        "basis": "rank-Gaussianized product Hermite; resolved degree-two h1_i*h1_j terms",
        "patch": patch,
        "n_patches": len(patches),
        "degree2_correlation_energy": total,
        "mean_squared_degree2_coefficient": float(np.mean(energy)),
        "energy_weighted_distance": float(np.dot(probabilities, distances))
        if total > 0.0
        else None,
        "energy_weighted_log_distance": float(
            np.dot(probabilities, np.log2(1.0 + distances))
        )
        if total > 0.0
        else None,
        "locality_radius_50": _weighted_quantile(distances, energy, 0.5),
        "locality_radius_90": _weighted_quantile(distances, energy, 0.9),
        "spectral_entropy_bits": entropy,
        "correlation_effective_rank": effective_rank,
        "scope": "degree-two patch-coordinate lower bound; not the complete image density spectrum",
    }


def image_spectral_profile(
    images: np.ndarray,
    *,
    patch: int = 8,
    max_images: int = 10_000,
    max_patches: int = 100_000,
    seed: int = 0,
) -> dict:
    values = np.asarray(images)
    if len(values) > max_images:
        rng = np.random.default_rng(seed)
        selected = np.sort(rng.choice(len(values), size=max_images, replace=False))
        values = values[selected]
    return {
        "n_images": len(values),
        "dct": dct_image_profile(values),
        "gaussian_hermite": gaussian_hermite_patch_profile(
            values, patch=patch, max_patches=max_patches, seed=seed
        ),
        "modalities_are_separate": True,
    }
