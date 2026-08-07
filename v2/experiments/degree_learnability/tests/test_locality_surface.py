from __future__ import annotations

import numpy as np

from dlx.data.planted_spectrum import planted_modular_blocks
from dlx.profiles.locality_surface import resolved_surface_from_pairs
from dlx.profiles.text_anova import inverse_likelihood_pair_profile


def _pair(lags: tuple[int, int], weights: tuple[float, float, float]) -> dict:
    total = sum(weights)
    nonconstant = sum(weights[1:])
    return {
        "lags": list(lags),
        "conditional_fourier_spectrum": {
            "level_weights": list(weights),
            "level_cardinalities": [1, 6, 9],
            "total_square_energy": total,
            "nonconstant_energy": nonconstant,
            "mean_nonconstant_spectral_degree": (
                (weights[1] + 2 * weights[2]) / nonconstant
            ),
            "cumulative_concentration": [
                weights[0] / total,
                sum(weights[:2]) / total,
                1.0,
            ],
        },
    }


def test_resolved_surface_is_nested_and_locality_weighted() -> None:
    result = resolved_surface_from_pairs(
        [
            _pair((1, 2), (0.2, 0.1, 0.0)),
            _pair((1, 4), (0.2, 0.1, 0.2)),
            _pair((2, 8), (0.2, 0.1, 0.3)),
        ],
        radii=(1, 2, 4, 8),
    )
    energies = [row["energy_degree_leq_2"] for row in result["surface"]]
    assert energies == sorted(energies)
    assert result["features"]["locality_radius_50"] == 4
    assert result["features"]["locality_radius_90"] == 8
    assert result["features"]["energy_weighted_log_radius"] is not None


def test_planted_modular_rules_recover_degree() -> None:
    common = {
        "n_blocks": 20_000,
        "ctx_len": 8,
        "q": 4,
        "signal_probability": 0.9,
        "seed": 7,
    }
    tokens1, positions1, folds1 = planted_modular_blocks(
        support_lags=(4,), **common
    )
    degree1 = inverse_likelihood_pair_profile(
        tokens1,
        q=4,
        lag_a=1,
        lag_b=4,
        positions=positions1,
        fold_ids=folds1,
        smoothing=2.0,
    )
    tokens2, positions2, folds2 = planted_modular_blocks(
        support_lags=(1, 4), **common
    )
    degree2 = inverse_likelihood_pair_profile(
        tokens2,
        q=4,
        lag_a=1,
        lag_b=4,
        positions=positions2,
        fold_ids=folds2,
        smoothing=2.0,
    )
    assert degree1["variance_concentration"]["concentration_leq_1"] > 0.95
    assert degree2["variance_concentration"]["concentration_leq_1"] < 0.05
    assert abs(
        degree1["conditional_fourier_spectrum"]["nonconstant_energy"]
        - degree2["conditional_fourier_spectrum"]["nonconstant_energy"]
    ) < 0.03


def test_planted_blocks_validate_arguments() -> None:
    with np.testing.assert_raises(ValueError):
        planted_modular_blocks(
            n_blocks=100,
            ctx_len=4,
            q=4,
            support_lags=(5,),
            signal_probability=0.5,
            seed=0,
        )
