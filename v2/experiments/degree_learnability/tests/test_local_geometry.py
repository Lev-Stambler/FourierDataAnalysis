import math

import numpy as np
import pytest

from dlx.analysis.local_geometry import (
    local_ball_cardinalities,
    mixed_level_cardinalities,
    spectral_search_complexity,
)
from scripts.v19_analyze import stride_interleave
from scripts.v20_analyze import exact_blocked_permutation
from scripts.v21_analyze import exact_blocked_spearman


def test_local_ball_cardinalities_match_pair_basis() -> None:
    assert local_ball_cardinalities(256, radius=2, max_degree=2) == [
        1,
        510,
        65025,
    ]


def test_mixed_level_cardinalities_count_full_tensor_basis() -> None:
    cardinalities = mixed_level_cardinalities([3, 4, 2])
    assert cardinalities == [1, 6, 11, 6]
    assert sum(cardinalities) == 3 * 4 * 2


def test_search_complexity_distinguishes_degree_and_radius() -> None:
    copy = spectral_search_complexity(
        [0.0, 1.0, 0.0], local_ball_cardinalities(256, 16, 2)
    )
    local_pair = spectral_search_complexity(
        [0.0, 0.0, 1.0], local_ball_cardinalities(256, 2, 2)
    )
    assert copy == pytest.approx(math.log2(16 * 255))
    assert local_pair == pytest.approx(math.log2(255**2))
    assert copy < local_pair


def test_search_complexity_rejects_zero_signal() -> None:
    with pytest.raises(ValueError, match="positive nonconstant"):
        spectral_search_complexity([1.0, 0.0], [1, 7])


def test_stride_interleave_preserves_values_and_moves_adjacency() -> None:
    original = np.arange(24, dtype=np.int64)
    transformed = stride_interleave(original, stride=3)
    assert np.array_equal(np.sort(transformed), original)
    assert np.array_equal(transformed[3:], transformed[:-3] + 1)


def test_exact_blocked_permutation_uses_full_within_dataset_null() -> None:
    rows_by_dataset = {
        f"d{dataset}": [
            {
                "geometric_moment_total": float(stride),
                "final_ce_fraction": float(stride),
            }
            for stride in range(4)
        ]
        for dataset in range(4)
    }
    result = exact_blocked_permutation(rows_by_dataset)
    assert result["observed_rho"] == pytest.approx(1.0)
    assert result["total_permutations"] == 24**4
    assert result["exact_two_sided_p"] < 0.05


def test_exact_eight_stride_null_convolves_all_dataset_permutations() -> None:
    rows_by_dataset = {
        f"d{dataset}": [
            {
                "geometric_moment_total": float(stride),
                "final_ce_fraction": float(stride),
            }
            for stride in range(8)
        ]
        for dataset in range(6)
    }
    result = exact_blocked_spearman(rows_by_dataset)
    assert result["observed_rho"] == pytest.approx(1.0)
    assert int(result["total_permutations"]) == math.factorial(8) ** 6
    assert result["exact_two_sided_p"] < 0.01
