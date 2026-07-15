"""Planted-recovery tests for the edu-GL math core (CPU, no Modal/HF calls)."""

import numpy as np
import pytest

import edu_gl


def _pair_parity(bits, a, b):
    return (1.0 - 2.0 * (bits[:, a] ^ bits[:, b])).astype(np.float32)


def _deg1(bits, a):
    return (1.0 - 2.0 * bits[:, a]).astype(np.float32)


def test_codes_unique_and_capacity_matched():
    rng = np.random.default_rng(0)
    E = rng.standard_normal((200, 16)).astype(np.float32)
    E[7] = E[3]                                     # byte-identical rows -> tie-break
    lsh = edu_gl.build_lsh_codes(E, B=12)
    assert len(np.unique(lsh, axis=0)) == 200
    assert lsh.shape[1] >= 12
    ctrl = edu_gl.control_codes(200, lsh.shape[1])
    assert ctrl.shape == lsh.shape
    tokid = edu_gl.token_id_codes(200)
    assert tokid.shape == (200, 8)
    assert len(np.unique(tokid, axis=0)) == 200
    wins = rng.integers(0, 200, (50, 4))
    bits = edu_gl.window_bits(wins, lsh)
    assert bits.shape == (50, 4 * lsh.shape[1])
    np.testing.assert_array_equal(bits[:, :lsh.shape[1]], lsh[wins[:, 0]])


def test_deg1_exact_recovers_planted_linear():
    rng = np.random.default_rng(1)
    D, n = 20_000, 32
    bits = rng.integers(0, 2, (D, n), dtype=np.uint8)
    W_true = rng.standard_normal(n).astype(np.float32) * 0.1
    b_true = 0.3
    X = 1.0 - 2.0 * bits.astype(np.float32)
    y = X @ W_true + b_true
    W, b = edu_gl.fit_deg1_exact(bits, y)
    np.testing.assert_allclose(W, W_true, atol=1e-4)
    assert abs(b - b_true) < 1e-4


def test_deg2_psi_ranks_planted_pairs_top():
    rng = np.random.default_rng(2)
    D, n = 30_000, 24
    bits = rng.integers(0, 2, (D, n), dtype=np.uint8)
    planted = [(0, 5, 0.5), (2, 11, -0.4), (7, 19, 0.35), (13, 21, -0.3), (4, 22, 0.3)]
    g = 0.05 * rng.standard_normal(D).astype(np.float32)
    for a, b, c in planted:
        g += c * _pair_parity(bits, a, b)
    psi2, mass = edu_gl.deg2_psi_scalar(bits, g)
    floor = mass / D
    iu = np.triu_indices(n, k=1)
    top5 = np.argsort(-psi2[iu])[:5]
    found = {(int(iu[0][t]), int(iu[1][t])) for t in top5}
    assert found == {(a, b) for a, b, _ in planted}
    assert all(psi2[a, b] > 2.0 * floor for a, b, _ in planted)


def test_sequential_deflate_duplicates_and_constants():
    rng = np.random.default_rng(3)
    D = 20_000
    bits = rng.integers(0, 2, (D, 6), dtype=np.uint8)
    bits[:, 4] = bits[:, 0]                          # pair (4,5) == pair (0,1) on-data
    bits[:, 5] = bits[:, 1]
    g0 = 0.8 * _pair_parity(bits, 0, 1)
    idx = np.array([[0, 1, -1, -1], [4, 5, -1, -1], [2, 2, -1, -1]], np.int16)
    # sequential: later duplicate sees ~0 residual; (2,2) = constant char -> ~0
    C, g = edu_gl.sequential_deflate(bits, g0.copy(), idx, block=1)
    assert abs(C[0] - 0.8) < 1e-4
    assert abs(C[1]) < 1e-4
    assert abs(C[2]) < 1e-4
    assert float((np.asarray(g) ** 2).mean()) < 1e-7
    # block-OMP: the duplicate cluster's coefficient SPLITS and sums correctly
    C2, g2 = edu_gl.sequential_deflate(bits, g0.copy(), idx, block=3)
    assert abs(C2[0] + C2[1] - 0.8) < 1e-3
    assert abs(C2[2]) < 1e-5
    assert float((np.asarray(g2) ** 2).mean()) < 1e-6
    # residual never grows, step by step
    g_run = 0.8 * _pair_parity(bits, 0, 1) + 0.1 * _deg1(bits, 2)
    prev = float((g_run ** 2).mean())
    for row in idx:
        _, g_run = edu_gl.sequential_deflate(bits, g_run, row[None], block=1)
        cur = float((np.asarray(g_run) ** 2).mean())
        assert cur <= prev + 1e-9
        prev = cur


def test_end_to_end_synthetic_exact_recovery():
    q, w = 64, 6
    codes = edu_gl.token_id_codes(q)                 # 6 bits, bijective
    B = codes.shape[1]

    def f(bits):
        out = (0.15 * _deg1(bits, 3) - 0.2 * _deg1(bits, 10)
               + 0.25 * _pair_parity(bits, 5, 9)
               - 0.2 * _pair_parity(bits, 14, 26)
               + 0.15 * _pair_parity(bits, 20, 33))
        return out

    def draw(D, seed):
        wins = np.random.default_rng(seed).integers(0, q, (D, w))
        bits = edu_gl.window_bits(wins, codes)
        return bits, 2.5 + 2.5 * f(bits)             # raw 0-5 scores

    bits_tr, y_tr = draw(40_000, 10)
    bits_va, y_va = draw(5_000, 11)
    bits_te, y_te = draw(5_000, 12)
    assert bits_tr.shape[1] == w * B
    summary, model = edu_gl.fit_core(
        bits_tr, y_tr, {"val": (bits_va, y_va), "test": (bits_te, y_te)},
        max_pairs=200, ks=(10,), device="cpu")
    assert summary["deg1"]["test"]["r2"] > 0.0
    last = summary["ladder"][-1]
    assert last["test"]["r2"] > 0.99
    assert last["test"]["mse"] < 0.01
    assert model["C"].shape[0] == summary["K"]


def _triple_parity(bits, a, b, c):
    return (1.0 - 2.0 * (bits[:, a] ^ bits[:, b] ^ bits[:, c])).astype(np.float32)


def test_deg3_anchored_recovers_planted_triple():
    rng = np.random.default_rng(5)
    D, n = 40_000, 20
    bits = rng.integers(0, 2, (D, n), dtype=np.uint8)
    g = (0.4 * _triple_parity(bits, 2, 7, 13)
         + 0.02 * rng.standard_normal(D).astype(np.float32))
    pair_idx = np.array([[2, 7, -1, -1], [4, 9, -1, -1]], np.int16)  # anchor 1 hits
    M3 = edu_gl.deg3_anchored_psi(bits, g, pair_idx, anchor_chunk=1)
    assert abs(M3[13, 0] - 0.4) < 0.02                # coefficient at (2,7)+13
    floor = float((g ** 2).mean()) / D
    idx3, psi3 = edu_gl.select_triples(M3, pair_idx, floor, max_triples=10)
    assert tuple(idx3[0, :3]) == (2, 7, 13)
    assert psi3[0] > 100 * floor
    # deflating the found triple removes the planted signal
    C3, g_out = edu_gl.sequential_deflate(bits, g, idx3[:1], block=1)
    assert abs(C3[0] - 0.4) < 0.02
    assert float((np.asarray(g_out) ** 2).mean()) < 0.01


def test_end_to_end_with_deg3():
    q, w = 64, 6
    codes = edu_gl.token_id_codes(q)

    def f(bits):
        return (0.2 * _pair_parity(bits, 5, 9)
                + 0.3 * _triple_parity(bits, 5, 9, 20)
                - 0.15 * _triple_parity(bits, 2, 14, 33))

    def draw(D, seed):
        wins = np.random.default_rng(seed).integers(0, q, (D, w))
        bits = edu_gl.window_bits(wins, codes)
        return bits, 2.5 + 2.5 * f(bits)

    bits_tr, y_tr = draw(50_000, 20)
    bits_te, y_te = draw(5_000, 21)
    summary, _ = edu_gl.fit_core(
        bits_tr, y_tr, {"test": (bits_te, y_te)}, max_pairs=50, ks=(10,),
        deg3_anchors=50, max_triples=50, ks3=(10,), device="cpu")
    # triple (2,14,33) has NO planted pair inside it -- only (5,9,20) is
    # anchored-reachable, so R2 lands near the reachable ceiling, not 0.99
    assert summary["ladder3"][-1]["test"]["r2"] > summary["ladder"][-1]["test"]["r2"]
    r2 = summary["ladder3"][-1]["test"]["r2"]
    assert r2 > 0.5, r2


def test_score_metrics_sane():
    y = np.array([0.5, 2.0, 3.5, 4.5, 1.0])
    perfect = edu_gl.score_metrics(edu_gl.normalize_scores(y), y)
    assert perfect["r2"] == pytest.approx(1.0)
    assert perfect["mse"] == pytest.approx(0.0)
    assert perfect["spearman"] == pytest.approx(1.0)
    assert perfect["f1_ge3"] == pytest.approx(1.0)
    const = edu_gl.score_metrics(np.zeros(5), y)
    assert const["r2"] <= 0.0 + 1e-6
