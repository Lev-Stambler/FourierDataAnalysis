"""Degree-profile tools: planted, enumerated, and (later) measured profiles.

PLAN §4: the enumerated profile is the brute-force ground truth — the vector
Fourier transform of the next-token function restricted to its support subspace.
"""

from .enumerate import enumerated_profile
from .filtration import (
    averaged_filtration_uniform_n,
    exact_filtration_uniform,
    levels_from_averaged_filtration,
    order_mixing_matrix,
)
from .inverse_likelihood import (
    InverseLikelihoodTerm,
    canonical_terms,
    fit_exact_inverse_likelihood,
    inverse_likelihood_column,
)
from .planted import planted_profile
from .stability import (
    exact_stability_mask_refill,
    exact_stability_uniform_from_W,
    sampled_stability_mask_refill,
    sampled_stability_uniform,
    tail_bounds,
)
from .text_anova import (
    conditional_fourier_spectrum,
    inverse_likelihood_pair_profile,
    text_inverse_likelihood_profile,
    variance_concentration,
)

__all__ = [
    "InverseLikelihoodTerm",
    "averaged_filtration_uniform_n",
    "canonical_terms",
    "conditional_fourier_spectrum",
    "enumerated_profile",
    "exact_filtration_uniform",
    "exact_stability_mask_refill",
    "exact_stability_uniform_from_W",
    "fit_exact_inverse_likelihood",
    "inverse_likelihood_column",
    "inverse_likelihood_pair_profile",
    "levels_from_averaged_filtration",
    "order_mixing_matrix",
    "planted_profile",
    "sampled_stability_mask_refill",
    "sampled_stability_uniform",
    "tail_bounds",
    "text_inverse_likelihood_profile",
    "variance_concentration",
]
