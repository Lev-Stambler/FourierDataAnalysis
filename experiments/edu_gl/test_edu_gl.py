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


def test_adamw_core_recovers_planted():
    rng = np.random.default_rng(6)
    D, n = 30_000, 24
    bits = rng.integers(0, 2, (D, n), dtype=np.uint8)
    idx = np.array([[2, 9, -1, -1], [5, 17, -1, -1]], np.int16)
    y = (2.5 + 2.5 * (0.2 * _deg1(bits, 3) + 0.3 * _pair_parity(bits, 2, 9)
                      - 0.25 * _pair_parity(bits, 5, 17)))
    bte = rng.integers(0, 2, (5_000, n), dtype=np.uint8)
    yte = (2.5 + 2.5 * (0.2 * _deg1(bte, 3) + 0.3 * _pair_parity(bte, 2, 9)
                        - 0.25 * _pair_parity(bte, 5, 17)))
    summary, model = edu_gl.adamw_core(
        bits, y, {"val": (bte, yte), "test": (bte, yte)}, idx,
        W1_init=np.zeros(n, np.float32), C_init=np.zeros(2, np.float32),
        b_init=0.0, lr=3e-3, wd=0.0, steps=4000, batch=4096,
        eval_every=500, device="cpu")
    assert summary["test"]["r2"] > 0.95, summary["test"]
    assert abs(model["C"][0] - 0.3) < 0.05
    assert abs(model["C"][1] + 0.25) < 0.05


def test_ste_chars_forward_is_exact_parity():
    import torch
    rng = np.random.default_rng(7)
    bits = rng.integers(0, 2, (256, 12), dtype=np.uint8)
    theta = np.full((2, 12), -4.0, np.float32)
    theta[0, [1, 7]] = 4.0
    theta[1, [0, 4, 9]] = 4.0
    th = torch.tensor(theta, requires_grad=True)
    out = edu_gl.logspace_ste_chars(torch.tensor(bits, dtype=torch.float32), th)
    expect = np.stack([_pair_parity(bits, 1, 7),
                       (1.0 - 2.0 * (bits[:, 0] ^ bits[:, 4] ^ bits[:, 9]))], 1)
    np.testing.assert_allclose(out.detach().numpy(), expect, atol=1e-6)
    out.sum().backward()                             # grads finite and nonzero
    g = th.grad.numpy()
    assert np.isfinite(g).all() and np.abs(g).sum() > 0


def test_ste_grad_alive_at_realistic_width():
    """The surrogate magnitude is exp(sum over SET BITS of log|1-2m|): at
    n=4416 (the real width) a -2 off-init gives exp(-600)=0 and theta gets NO
    data gradient (bug: three dead STE rounds).  At +-8 it must be alive."""
    import torch
    rng = np.random.default_rng(9)
    n = 4416
    bits = torch.tensor(rng.integers(0, 2, (64, n)).astype(np.float32))
    for off, want_alive in ((-8.0, True), (-2.0, False)):
        th = np.full((4, n), off, np.float32)
        for k in range(4):
            th[k, rng.choice(n, 2, replace=False)] = -off
        theta = torch.tensor(th, requires_grad=True)
        out = edu_gl.logspace_ste_chars(bits, theta)
        (out.sum()).backward()
        alive = float(theta.grad.abs().sum()) > 1e-8
        assert alive == want_alive, (off, float(theta.grad.abs().sum()))


def test_ste_core_learns_planted_pair():
    rng = np.random.default_rng(8)
    D, n = 20_000, 16
    bits = rng.integers(0, 2, (D, n), dtype=np.uint8)
    y = 2.5 + 2.5 * 0.6 * _pair_parity(bits, 3, 9)
    bte = rng.integers(0, 2, (4_000, n), dtype=np.uint8)
    yte = 2.5 + 2.5 * 0.6 * _pair_parity(bte, 3, 9)
    summary, model = edu_gl.ste_core(
        bits, y, {"val": (bte, yte), "test": (bte, yte)}, K=64,
        warm_idx=None, lr_theta=3e-2, lr_c=3e-3, wd=0.0, steps=2500,
        batch=4096, eval_every=500, device="cpu", seed=0)
    assert summary["test"]["r2"] > 0.5, summary["test"]
    expect = np.zeros(n, np.uint8); expect[[3, 9]] = 1
    assert (model["masks"] == expect).all(1).any()


def test_token_table_recovers_planted_values():
    rng = np.random.default_rng(11)
    q, w, D = 64, 6, 30_000
    v_true = (0.1 * rng.standard_normal(q)).astype(np.float32)
    tok = rng.integers(0, q, (D, w))
    y = v_true[tok].sum(1) + 0.3
    v, b = edu_gl.fit_token_table(tok, y, q, device="cpu", wd=1e-3)
    tok_te = rng.integers(0, q, (5_000, w))
    pred = edu_gl.token_table_apply(tok_te, v, b, device="cpu").cpu().numpy()
    y_te = v_true[tok_te].sum(1) + 0.3
    ss = 1.0 - np.mean((pred - y_te) ** 2) / np.var(y_te)
    assert ss > 0.999, ss
    sweep = edu_gl.fit_token_table(tok, y, q, device="cpu", wd=(1e-3, 1e3))
    assert len(sweep) == 2 and sweep[0][0] == 1e-3


def test_ngram_cg_recovers_planted_bigram_values():
    rng = np.random.default_rng(12)
    q, w, D = 64, 8, 40_000
    tok = rng.integers(0, q, (D, w))
    ids, n_feat = edu_gl._ngram_ids(tok, q, orders=(1, 2), n_hash=4096)
    assert ids.shape == (D, w + w - 1)
    v_uni = (0.05 * rng.standard_normal(q)).astype(np.float32)
    y = v_uni[tok].sum(1)
    big = (tok[:, :-1] == 3) & (tok[:, 1:] == 7)     # bigram "3 7" worth +0.5
    y = y + 0.5 * big.sum(1)
    v = edu_gl.fit_ngram_cg(ids, y - y.mean(), n_feat, wd=1e-2, iters=150,
                            device="cpu")
    tok_te = rng.integers(0, q, (5_000, w))
    ids_te, _ = edu_gl._ngram_ids(tok_te, q, orders=(1, 2), n_hash=4096)
    y_te = v_uni[tok_te].sum(1) \
        + 0.5 * ((tok_te[:, :-1] == 3) & (tok_te[:, 1:] == 7)).sum(1)
    pred = edu_gl.ngram_apply(ids_te, v, device="cpu").cpu().numpy() + y.mean()
    r2 = 1.0 - np.mean((pred - y_te) ** 2) / np.var(y_te)
    assert r2 > 0.95, r2


def test_score_metrics_sane():
    y = np.array([0.5, 2.0, 3.5, 4.5, 1.0])
    perfect = edu_gl.score_metrics(edu_gl.normalize_scores(y), y)
    assert perfect["r2"] == pytest.approx(1.0)
    assert perfect["mse"] == pytest.approx(0.0)
    assert perfect["spearman"] == pytest.approx(1.0)
    assert perfect["f1_ge3"] == pytest.approx(1.0)
    const = edu_gl.score_metrics(np.zeros(5), y)
    assert const["r2"] <= 0.0 + 1e-6


def test_slot_forward_exact_and_grads_alive():
    import torch
    rng = np.random.default_rng(13)
    B, w, r, S = 32, 64, 8, 6
    feat = torch.tensor(rng.standard_normal((B, w, r)).astype(np.float32))
    th = np.full((S, w), -8.0, np.float32)
    supports = []
    for s in range(S):
        sup = rng.choice(w, s % 4 + 1, replace=False)
        th[s, sup] = 8.0
        supports.append(sup)
    theta = torch.tensor(th, requires_grad=True)
    Z = torch.tensor(rng.standard_normal((S, r)).astype(np.float32),
                     requires_grad=True)
    out = edu_gl.slot_forward(feat, theta, Z)
    u = torch.tanh(torch.einsum("bwr,sr->bws", feat, Z)
                   / feat.shape[-1] ** 0.5)
    for s in range(S):                               # exact hard product
        expect = torch.ones(B)
        for p in supports[s]:
            expect = expect * u[:, p, s]
        np.testing.assert_allclose(out[:, s].detach().numpy(),
                                   expect.detach().numpy(), atol=1e-5)
    out.sum().backward()
    assert torch.isfinite(theta.grad).all() and theta.grad.abs().sum() > 0
    assert torch.isfinite(Z.grad).all() and Z.grad.abs().sum() > 0


def test_slots_core_learns_planted_product():
    rng = np.random.default_rng(14)
    q, w, D = 64, 8, 20_000
    E = rng.standard_normal((q, 16)).astype(np.float32)
    cls = np.where(np.arange(q) < 32, 1.0, -1.0).astype(np.float32)
    E[:, 0] = cls * 2.0                              # class linearly readable
    tok = rng.integers(0, q, (D, w))
    v = cls[tok]
    y = 2.5 + 2.5 * 0.6 * (v[:, 2] * v[:, 5] * v[:, 7])
    tok_te = rng.integers(0, q, (4_000, w))
    v_te = cls[tok_te]
    y_te = 2.5 + 2.5 * 0.6 * (v_te[:, 2] * v_te[:, 5] * v_te[:, 7])
    summary, model = edu_gl.slots_core(
        tok, y, {"val": (tok_te, y_te), "test": (tok_te, y_te)}, E,
        S=192, r=16, lr_theta=5e-2, lr_z=5e-3, lam_div=1e-3, steps=2500,
        batch=1024, eval_every=250, patience=8, warmup=100, warm_frac=0.0,
        slot_chunk=192, val_fast=4000, device="cpu", seed=0)
    assert summary["test"]["r2"] > 0.4, summary["test"]
