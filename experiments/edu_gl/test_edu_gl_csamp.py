"""Planted-recovery tests for the CSAMP tree (CPU, synthetic fiber forks)."""

import numpy as np

import edu_gl as E
import edu_gl_csamp as C


def _make_levels(depth, B, m_fibers, g, planted_bits, coef, seed=0, noise=0.02):
    """Synthetic fork tables: at level j the newest j+1 blocks are resampled
    iid per row; older blocks are FIXED per fiber.  f = coef * parity(planted
    bits) + noise, |f| <= 1."""
    rng = np.random.default_rng(seed)
    total = depth * B
    fiber_cond = rng.integers(0, 2, (m_fibers, total), dtype=np.uint8)
    levels = []
    for j in range(depth):
        w = (j + 1) * B
        rows = m_fibers * g
        bits = np.repeat(fiber_cond, g, axis=0)
        bits[:, :w] = rng.integers(0, 2, (rows, w), dtype=np.uint8)
        f = coef * (1.0 - 2.0 * np.bitwise_xor.reduce(bits[:, planted_bits], axis=1))
        f = (f + noise * rng.standard_normal(rows)).astype(np.float32)
        levels.append((bits[:, :w], f, np.repeat(np.arange(m_fibers), g)))
    return levels


def test_pair_psi_scalar_planted():
    B, depth = 4, 3
    levels = _make_levels(depth, B, m_fibers=400, g=8, planted_bits=[1, 5],
                          coef=0.7, seed=1)
    bits, f, gid = levels[1]                        # both planted bits split
    masks = np.zeros((3, 2 * B), np.uint8)
    masks[0, [1, 5]] = 1                            # the planted char
    masks[1, [2, 6]] = 1                            # a null char
    masks[2, 1] = 1                                 # deg-1 sub-parity: dead once
    psi = C.pair_psi_scalar(bits, masks, f, gid)    # bit 5 is resampled
    assert abs(psi[0] - 0.49) < 0.05, psi
    assert abs(psi[1]) < 0.02
    assert abs(psi[2]) < 0.02


def test_tree_recovers_planted_char():
    B, depth = 4, 3
    levels = _make_levels(depth, B, m_fibers=400, g=8, planted_bits=[1, 5],
                          coef=0.7, seed=2)
    tree = C.gl_tree_scalar(levels, B, tau=0.3, max_width=64)
    assert len(tree["masks"]) >= 1
    expect = np.zeros(depth * B, np.uint8)
    expect[[1, 5]] = 1
    hit = np.flatnonzero((tree["masks"] == expect).all(1))
    assert len(hit) == 1, tree["masks"]
    assert abs(tree["psi"][hit[0]] - 0.49) < 0.05
    # the deg-1 sub-parity {1} is ALSO kept with its level-0 score (bit 5 was
    # conditioning there) -- the kept dict is a per-level snapshot, exactly as
    # in canonical pure_gl_tree; deflation later zeroes the stale char


def test_mask_parity_matches_xor():
    rng = np.random.default_rng(3)
    bits = rng.integers(0, 2, (500, 12), dtype=np.uint8)
    idx = np.array([[0, 5, -1, -1], [2, 7, 11, -1]], np.int16)
    masks = np.zeros((2, 12), np.uint8)
    masks[0, [0, 5]] = 1
    masks[1, [2, 7, 11]] = 1
    np.testing.assert_allclose(E.mask_parity_features(bits, masks),
                               E.xor_parity_features(bits, idx), atol=1e-6)


def test_deflate_with_masks():
    rng = np.random.default_rng(4)
    bits = rng.integers(0, 2, (20_000, 10), dtype=np.uint8)
    masks = np.zeros((2, 10), np.uint8)
    masks[0, [0, 3, 7]] = 1                          # degree-3 character
    masks[1, [1, 4]] = 1
    g = (0.5 * E.mask_parity_features(bits, masks[:1])[:, 0]
         - 0.3 * E.mask_parity_features(bits, masks[1:])[:, 0])
    Cc, g_out = E.sequential_deflate(bits, g, None, block=2, masks=masks)
    assert abs(Cc[0] - 0.5) < 1e-3
    assert abs(Cc[1] + 0.3) < 1e-3
    assert float((np.asarray(g_out) ** 2).mean()) < 1e-4
