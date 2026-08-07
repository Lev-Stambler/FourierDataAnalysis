from __future__ import annotations

import numpy as np

from dlx.profiles.image_spectrum import (
    dct_image_profile,
    gaussian_hermite_patch_profile,
)


def test_dct_parseval_and_frequency_ordering() -> None:
    constant = np.ones((32, 8, 8), dtype=np.float64)
    checker = np.indices((8, 8)).sum(axis=0) % 2
    checker = np.repeat(checker[None, :, :], 32, axis=0).astype(float)
    low = dct_image_profile(constant)
    high = dct_image_profile(checker)
    assert np.isclose(low["total_square_energy"], low["parseval_pixel_square_energy"])
    assert low["dc_concentration"] > 0.999999
    assert high["frequency_centroid"] > low["frequency_centroid"]
    assert high["non_dc_frequency_centroid"] is not None
    assert high["high_frequency_tail_r_gt_half"] > 0.2


def test_gaussian_hermite_detects_correlated_local_pixels() -> None:
    rng = np.random.default_rng(4)
    independent = rng.normal(size=(500, 8, 8))
    shared = rng.normal(size=(500, 1, 1))
    correlated = shared + 0.2 * rng.normal(size=(500, 8, 8))
    null = gaussian_hermite_patch_profile(independent, patch=8)
    signal = gaussian_hermite_patch_profile(correlated, patch=8)
    assert signal["degree2_correlation_energy"] > 20 * null["degree2_correlation_energy"]
    assert 1.0 <= signal["locality_radius_50"] <= np.sqrt(98)
