import numpy as np
import pytest

from dlx.analysis.floor_independent import curve_metrics, normalized_learning_time


def test_floor_independent_curve_metrics():
    m = curve_metrics([0, 10, 100], [4.0, 3.0, 2.0])
    assert m["initial_ce_bits"] == 4.0
    assert m["learning_amount_bits"] == 2.0
    assert m["fractional_learning"] == 0.5
    assert m["half_best_learning_at"] == 10
    assert 0.5 < m["normalized_curve_area"] < 1.0


def test_floor_independent_metrics_need_no_floor_and_allow_flat_curve():
    m = curve_metrics([0, 1, 2], [3.0, 3.0, 3.0])
    assert m["learning_amount_bits"] == 0.0
    assert m["half_best_learning_at"] is None
    assert np.isclose(m["normalized_curve_area"], 1.0)


def test_normalized_learning_time_and_identification_gate():
    assert normalized_learning_time(0.7, 0.4) == pytest.approx(0.5)
    with pytest.raises(ValueError, match="unidentifiable"):
        normalized_learning_time(1.0, 0.99)
