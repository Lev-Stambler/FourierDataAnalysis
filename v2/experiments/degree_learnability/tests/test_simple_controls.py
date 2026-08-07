from __future__ import annotations

import numpy as np

from dlx.profiles.simple_controls import simple_text_controls


def test_controls_distinguish_periodic_from_iid() -> None:
    periodic = np.tile(np.arange(4, dtype=np.uint8), 10_000)
    iid = np.random.default_rng(0).integers(0, 4, size=40_000, dtype=np.uint8)
    structured = simple_text_controls(periodic, q=4)
    random = simple_text_controls(iid, q=4)
    assert structured["lag1_mutual_information_bits"] > 1.9
    assert structured["heldout_bigram_ce_bits"] < 0.01
    assert structured["zlib_bits_per_byte"] < random["zlib_bits_per_byte"]


def test_controls_reject_short_stream() -> None:
    with np.testing.assert_raises(ValueError):
        simple_text_controls(np.arange(10), q=16)
