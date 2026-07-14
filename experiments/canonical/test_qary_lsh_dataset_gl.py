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
    dataset_gl_csamp,
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


# ---------------------------------------------- the paired-CSAMP estimator
# The paper's pair shares the real context Z AND the un-split continuation
# L_k (ar_categorical_gl.typ, lem:qary-kv-estimator): on a flat table a valid
# pair is two rows in the same fiber that AGREE on every un-split coordinate,
# and the diagonal (x = x') is excluded.  These tests anchor the tree against
# direct enumeration of that estimator -- not against any other code path.


def _pair_cells(bits, fib, split_k):
    key = np.column_stack([fib[:, None], bits[:, split_k:]])
    _, cell = np.unique(key, axis=0, return_inverse=True)
    return cell


def _brute_pair_psi(bits, F, fib, masks, split_k):
    """psi = (sum_cells |sum chi F|^2 - sum ||F||^2) / n_pairs, or None if the
    level has no matched pairs."""
    cell = _pair_cells(bits, fib, split_k)
    cc = np.bincount(cell).astype(np.float64)
    npairs = float((cc * (cc - 1)).sum())
    if npairs <= 0:
        return None, 0.0
    chi = _chi(bits, masks)
    Q = np.zeros(len(masks))
    for t in range(F.shape[1]):
        G = np.zeros((int(cell.max()) + 1, len(masks)))
        np.add.at(G, cell, chi * F[:, t][:, None])
        Q += (G * G).sum(0)
    diag = float((F.astype(np.float64) ** 2).sum())
    return (Q - diag) / npairs, npairs


def _brute_csamp(bits, F, fib, tau):
    n = bits.shape[1]
    thresh = tau * tau / 4.0
    live = np.zeros((1, n), np.uint8)
    widths, profile = [], []
    for k in range(n):
        add = live.copy(); add[:, k] = 1
        cand = np.concatenate([live, add])
        psi, npairs = _brute_pair_psi(bits, F, fib, cand, k + 1)
        profile.append(npairs)
        live = cand if psi is None else cand[psi >= thresh]
        widths.append(len(live))
        if len(live) == 0:
            break
    return {tuple(mk) for mk in live if any(mk)}, widths, profile


def _fiber_table(rng, n_fibers, r, n, flip=0.15):
    """Per-fill rows with heavy suffix collisions: each fiber has a base
    pattern and every fill flips bits independently with prob ``flip``."""
    base = rng.integers(0, 2, (n_fibers, n)).astype(np.uint8)
    fib = np.repeat(np.arange(n_fibers), r)
    bits = base[fib] ^ (rng.random((n_fibers * r, n)) < flip).astype(np.uint8)
    return bits, fib


def test_csamp_matches_bruteforce_everywhere():
    rng = np.random.default_rng(11)
    bits, fib = _fiber_table(rng, n_fibers=24, r=8, n=8)
    F = rng.normal(size=(len(bits), 2)).astype(np.float32)
    # tau strictly inside the widest gap of the attainable psi order
    # statistics, so no comparison is a knife edge in fp32
    vals = []
    for kk in range(1, 9):
        pre = _all_masks(kk)
        masks = np.zeros((len(pre), 8), np.uint8); masks[:, :kk] = pre
        psi, _ = _brute_pair_psi(bits, F, fib, masks, kk)
        vals.append(psi)
    vals = np.unique(np.concatenate(vals))
    top = vals[vals > 0][-30:]
    i = int(np.argmax(np.diff(top)))
    tau = float(2.0 * np.sqrt(0.5 * (top[i] + top[i + 1])))
    want_masks, want_widths, want_profile = _brute_csamp(bits, F, fib, tau)
    got = dataset_gl_csamp(bits, F, fib, tau, max_width=10_000, device="cpu")
    assert got["status"] == "ok"
    assert got["widths"] == want_widths
    assert got["pair_profile"] == want_profile
    assert {tuple(mk) for mk in got["masks"]} == want_masks


def test_csamp_finds_within_fiber_character_at_every_level():
    # f = chi_{bit 4} with ALL bits i.i.d. uniform within fiber: pairing on the
    # fiber alone gives psi(prefix) ~ 0 at every level before bit 4 (the
    # cancellation-prone pooled quantity), while matched un-split values keep
    # psi(path) = 1 exactly -- the tree must walk straight to {4}.
    rng = np.random.default_rng(12)
    n = 6
    bits = rng.integers(0, 2, (60 * 16, n)).astype(np.uint8)
    fib = np.repeat(np.arange(60), 16)
    F = (1.0 - 2.0 * bits[:, 4]).astype(np.float32)[:, None]
    got = dataset_gl_csamp(bits, F, fib, tau=1.0, max_width=10_000, device="cpu")
    want = np.zeros(n, np.uint8); want[4] = 1
    assert got["status"] == "ok"
    assert {tuple(mk) for mk in got["masks"]} == {tuple(want)}
    assert got["no_evidence_levels"] == []
    assert got["psi_top"] and abs(got["psi_top"][0] - 1.0) < 0.05


def test_csamp_zero_pair_levels_carry_the_frontier():
    # Distinct un-split values everywhere at level 0 -> no matched pairs -> no
    # verdict: both children must be carried and the level recorded.
    n = 5
    pat = ((np.arange(12)[:, None] >> np.arange(4)) & 1).astype(np.uint8)
    bits = np.zeros((12, n), np.uint8)
    bits[:, 1:] = pat                                              # unique suffixes
    bits[:, 0] = np.arange(12) % 2
    fib = np.repeat(np.arange(6), 2)
    F = np.ones((12, 1), np.float32)
    got = dataset_gl_csamp(bits, F, fib, tau=0.5, max_width=10_000, device="cpu")
    assert got["pair_profile"][0] == 0
    assert 0 in got["no_evidence_levels"]
    assert got["widths"][0] == 2


def test_csamp_singleton_fibers_report_no_pairs():
    rng = np.random.default_rng(13)
    bits = rng.integers(0, 2, (16, 4)).astype(np.uint8)
    F = rng.normal(size=(16, 1)).astype(np.float32)
    got = dataset_gl_csamp(bits, F, np.arange(16), tau=0.1, device="cpu")
    assert got["status"] == "no-pairs" and len(got["masks"]) == 0


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
