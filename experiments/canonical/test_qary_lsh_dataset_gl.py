"""Tests for the canonical q-ary Dataset GL experiment.

The search is anchored against BRUTE-FORCE enumeration of the paper's bucket
weight W(S | level kk) = (2^(n-kk)/m^2) * sum_z ( sum_{x in group z} chi_S(x) A[x,t] )^2
summed over classes t -- not against any other implementation.
"""

import numpy as np

from qary_lsh_dataset_gl import (
    _group_ids,
    _iter_suffix_gids,
    build_lsh_codes,
    context_bits,
    control_codes,
    dataset_gl_tau,
    draw_fibers,
    fit_softmax_slots,
    parity_features,
    soft_collapse,
    token_id_codes,
)


def _all_masks(n):
    return ((np.arange(2 ** n)[:, None] >> np.arange(n)) & 1).astype(np.uint8)


def _chi(bits, masks):
    return 1.0 - 2.0 * ((bits.astype(np.int64) @ masks.T.astype(np.int64)) % 2)


def _brute(bits, A, tau, norm_m):
    """Leaf mask set and per-level bucket counts by direct enumeration."""
    n = bits.shape[1]
    thresh = tau * tau / 4.0
    leaf_masks, widths = [], []
    for kk in range(1, n + 1):
        pre = _all_masks(kk)
        # pad prefix masks to n bits (high bits zero)
        masks = np.zeros((len(pre), n), dtype=np.uint8)
        masks[:, :kk] = pre
        chi = _chi(bits[:, :kk], pre)                              # (m, 2^kk)
        # group rows by the un-split suffix bits kk..n-1
        _, gid = np.unique(bits[:, kk:], axis=0, return_inverse=True)
        ng = gid.max() + 1
        W = np.zeros(len(pre))
        for t in range(A.shape[1]):
            G = np.zeros((ng, len(pre)))
            np.add.at(G, gid, chi * A[:, t][:, None])
            W += (G * G).sum(0)
        W *= (2.0 ** (n - kk)) / (norm_m ** 2)
        widths.append(int((W >= thresh).sum()))
        if kk == n:
            leaf_masks = [tuple(mk) for mk, keep in zip(masks, W >= thresh) if keep]
    return set(leaf_masks), widths


def test_search_matches_bruteforce_everywhere():
    rng = np.random.default_rng(0)
    m_rows, n = 200, 10
    bits = rng.integers(0, 2, (m_rows, n)).astype(np.uint8)
    A = rng.normal(size=(m_rows, 3)).astype(np.float32)
    # tau strictly between two leaf order statistics: a real margin, so the
    # fp32 bucket accumulation cannot flip any threshold comparison
    leaf_norms = np.sort(np.sqrt(((_chi(bits, _all_masks(n)).T @ A / m_rows) ** 2).sum(1)))
    tau = float(leaf_norms[-8] + leaf_norms[-9])                   # = 2 * midpoint
    want_masks, want_widths = _brute(bits, A, tau, m_rows)
    got = dataset_gl_tau(bits, A, tau, norm_m=m_rows, device="cpu")
    assert got["status"] == "ok"
    assert got["widths"] == want_widths
    assert {tuple(mk) for mk in got["masks"]} == {mk for mk in want_masks if any(mk)}


def test_planted_character_with_collapse_and_norm_m():
    rng = np.random.default_rng(1)
    m_rows, n = 512, 8
    beta = np.zeros(n, np.uint8); beta[[1, 4]] = 1
    gamma = np.zeros(n, np.uint8); gamma[6] = 1
    bits = rng.integers(0, 2, (m_rows, n)).astype(np.uint8)
    bits[m_rows // 2:] = bits[: m_rows // 2]                       # force duplicates
    f = np.stack([0.9 * _chi(bits, beta[None])[:, 0],
                  0.3 * _chi(bits, gamma[None])[:, 0]], axis=1)
    rows, inv = np.unique(bits, axis=0, return_inverse=True)
    A = np.zeros((len(rows), 2), dtype=np.float32)
    np.add.at(A, inv, f.astype(np.float32))                        # per-distinct weighted sums
    got = dataset_gl_tau(rows, A, tau=1.0, norm_m=m_rows, device="cpu")
    assert {tuple(mk) for mk in got["masks"]} == {tuple(beta)}
    empty = dataset_gl_tau(rows, A, tau=2.5, norm_m=m_rows, device="cpu")
    assert len(empty["masks"]) == 0


def test_ordered_frontier_saturates_and_completes():
    rng = np.random.default_rng(6)
    bits = rng.integers(0, 2, (300, 12)).astype(np.uint8)
    A = rng.normal(size=(300, 2)).astype(np.float32)
    got = dataset_gl_tau(bits, A, tau=1e-6, norm_m=300, max_width=4, device="cpu")
    assert got["status"] == "ok"
    assert len(got["saturated_levels"]) >= 1
    assert max(got["widths"]) <= 4


def test_frontier_dedupes_on_data_equivalent_masks():
    # Bit 3 is constant on the data, so any mask and mask|{3} are the SAME
    # function on the dataset; without dedup they flood a small frontier and
    # crowd out genuinely distinct characters.
    rng = np.random.default_rng(8)
    m_rows = 400
    bits = rng.integers(0, 2, (m_rows, 8)).astype(np.uint8)
    bits[:, 3] = 0
    beta = np.zeros(8, np.uint8); beta[1] = 1
    gamma = np.zeros(8, np.uint8); gamma[6] = 1
    A = (0.9 * _chi(bits, beta[None])[:, 0]
         + 0.6 * _chi(bits, gamma[None])[:, 0]).astype(np.float32)[:, None]
    got = dataset_gl_tau(bits, A, tau=1.0, norm_m=m_rows, max_width=3, device="cpu")
    masks = {tuple(mk) for mk in got["masks"]}
    assert tuple(beta) in masks and tuple(gamma) in masks


def test_n_search_truncation_with_exact_leaf_filter():
    rng = np.random.default_rng(7)
    bits = rng.integers(0, 2, (400, 10)).astype(np.uint8)
    inside = np.zeros(10, np.uint8); inside[2] = 1
    outside = np.zeros(10, np.uint8); outside[8] = 1
    A = (0.8 * _chi(bits, inside[None])[:, 0]
         + 0.8 * _chi(bits, outside[None])[:, 0]).astype(np.float32)[:, None]
    got = dataset_gl_tau(bits, A, tau=0.8, norm_m=400, n_search=5, device="cpu")
    masks = {tuple(mk) for mk in got["masks"]}
    assert tuple(inside) in masks
    assert tuple(outside) not in masks                             # beyond the searched span
    assert len(got["widths"]) == 5


def test_code_tables_unique():
    rng = np.random.default_rng(2)
    e = rng.standard_normal((96, 8))
    e[10] = e[3]                                                   # inseparable duplicates
    codes, report = build_lsh_codes(e, B0=8, cap=64, seed=0)
    assert len(np.unique(codes, axis=0)) == len(e)
    assert report["B_proj"] < 64                                   # dups must not force the cap
    ctrl = control_codes(96, codes.shape[1], seed=1)
    assert len(np.unique(ctrl, axis=0)) == 96 and ctrl.shape == codes.shape
    ids = token_id_codes(1000)
    assert ids.shape == (1000, 10) and len(np.unique(ids, axis=0)) == 1000


def test_soft_collapse_sums_targets_over_duplicates():
    windows = np.array([[1, 2], [3, 4], [1, 2]])
    P = np.array([[0.25, 0.75], [0.5, 0.5], [0.75, 0.25]], dtype=np.float32)
    rows, A, counts, inv = soft_collapse(windows, P)
    i = int(np.flatnonzero((rows == [1, 2]).all(1))[0])
    assert counts[i] == 2 and np.allclose(A[i], [1.0, 1.0])
    assert len(rows) == 2 and inv.shape == (3,)


def test_fiber_drawing_dedupes_fixed_string():
    ctx, w, k = 32, 6, 3
    spans = np.arange(20 * ctx).reshape(20, ctx) % 97
    spans[5] = spans[3]                                            # duplicate fixed string
    pre = draw_fibers(spans, w=w, k=k, M=10, seed=0)
    assert pre.shape == (10, ctx - (w - k))
    s = pre[:, -k:]
    assert len(np.unique(s, axis=0)) == len(s)


def test_context_bits_reverses_blocks():
    codes = ((np.arange(16)[:, None] >> np.arange(4)) & 1).astype(np.uint8)
    contexts = np.array([[3, 5, 9]])                               # text order, 9 = last token
    bits = context_bits(contexts, codes)
    want = np.concatenate([codes[9], codes[5], codes[3]])          # nearest-prediction first
    assert np.array_equal(bits[0], want)


def test_incremental_suffix_gids_match_direct():
    rng = np.random.default_rng(4)
    bits = rng.integers(0, 2, (60, 23)).astype(np.uint8)
    for k, (gid, ng) in enumerate(_iter_suffix_gids(bits, block=7)):
        want, want_ng = _group_ids(bits[:, k + 1:])
        assert ng == want_ng
        # same partition up to relabeling
        assert len(np.unique(np.stack([gid, want], 1), axis=0)) == want_ng


def test_full_domain_recovers_prefix_token_character():
    # f depends on a bit of the EARLIEST context token: in reversed block order
    # that is the deepest tree region, so this proves prefix bits are searched.
    rng = np.random.default_rng(5)
    q_tok, ctx = 8, 4
    codes = ((np.arange(q_tok)[:, None] >> np.arange(3)) & 1).astype(np.uint8)
    contexts = rng.integers(0, q_tok, (600, ctx))
    bits = context_bits(contexts, codes)
    n = bits.shape[1]                                              # 12
    target_bit = n - 2                                             # a bit of token 0
    A = (1.0 - 2.0 * bits[:, target_bit]).astype(np.float32)[:, None]
    got = dataset_gl_tau(bits, A, tau=1.0, norm_m=len(bits), device="cpu")
    want = np.zeros(n, np.uint8); want[target_bit] = 1
    assert {tuple(mk) for mk in got["masks"]} == {tuple(want)}


def test_fit_recovers_planted_soft_teacher():
    rng = np.random.default_rng(3)
    m_rows, n, V = 2048, 6, 4
    bits = rng.integers(0, 2, (m_rows, n)).astype(np.uint8)
    masks = np.zeros((2, n), np.uint8); masks[0, 1] = 1; masks[1, [2, 4]] = 1
    logits = parity_features(bits, masks) @ rng.normal(size=(2, V)) * 2.0
    P = np.exp(logits); P /= P.sum(1, keepdims=True)
    counts = np.ones(m_rows)
    half = m_rows // 2
    fit = fit_softmax_slots(parity_features(bits[:half], masks), P[:half], counts[:half],
                            parity_features(bits[half:], masks), P[half:], counts[half:],
                            steps=800, lr=0.1, device="cpu")
    assert fit["val_kl"] < 0.05
