"""Small inline version of the M2 scaling check (full run: scripts/m2_scaling.py)."""

import numpy as np

from scripts.m2_scaling import run


def test_scaling_slope_small():
    result = run(n=5, q=4, ds=(1, 2), eps=0.1, seeds=2,
                 c_grid=(0.5, 1.0, 2.0, 4.0, 8.0, 16.0))
    # theorem predicts m* ~ N_d: slope ~ 1 (wide tolerance at this tiny size)
    assert 0.5 <= result["slope"] <= 1.5, result
    for r in result["records"]:
        assert r["m_star"] >= r["N_d"] * 0.4  # cannot beat ~N_d by much
