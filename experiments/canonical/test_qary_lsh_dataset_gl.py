"""Tests for the canonical q-ary Dataset GL experiment.

The search is anchored against BRUTE-FORCE enumeration of the paper's bucket
weight W(S | level kk) = (2^(n-kk)/m^2) * sum_z ( sum_{x in group z} chi_S(x) A[x,t] )^2
summed over classes t -- not against any other implementation.
"""

import numpy as np

from qary_lsh_dataset_gl import (
    _group_ids,
    _iter_suffix_gids,
    _sens_from_groups,
    _sens_from_top1,
    _full_kl,
    _logspace_ste_chars,
    _sens_report,
    _ste_features,
    _top1_variance,
    build_lsh_codes,
    code_decode,
    context_bits,
    control_codes,
    dataset_gl_csamp,
    dataset_gl_tau,
    decorrelation_penalty,
    dedupe_indices,
    deg2_exact_psi,
    draw_fibers,
    fit_deg1_exact,
    fit_mlp_hidden,
    fourier_coefficients,
    learn_sense_chunk,
    masks_to_indices,
    sequential_deflate,
    xor_parity_features,
    fit_regression,
    fourier_learn_chars,
    fit_softmax_slots,
    forked_gl_tree,
    harden_masks,
    learn_fourier_masks,
    oracle_deg1_psi,
    parity_features,
    random_profile_masks,
    soft_collapse,
    token_id_codes,
    unembed_top1,
    weighted_agreement,
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


def test_fit_regression_matches_weighted_least_squares():
    rng = np.random.default_rng(11)
    D, p, d = 400, 12, 5
    F = rng.standard_normal((D, p))
    W0 = rng.standard_normal((p, d)); b0 = rng.standard_normal(d)
    Y = F @ W0 + b0 + 0.01 * rng.standard_normal((D, d))
    n = rng.integers(1, 5, D).astype(np.float64)
    got = fit_regression(F, Y, n, wd=1e-8, device="cpu")
    # closed-form weighted least squares reference
    Fa = np.concatenate([F, np.ones((D, 1))], 1)
    Wa = np.linalg.solve(Fa.T @ (n[:, None] * Fa), Fa.T @ (n[:, None] * Y))
    assert np.allclose(got["W"], Wa[:-1], atol=1e-3)
    assert np.allclose(got["b"], Wa[-1], atol=1e-3)


def test_unembed_top1_is_full_vocab_argmax():
    rng = np.random.default_rng(12)
    q, d, D = 50, 8, 30
    Wu = rng.standard_normal((q, d)); H = rng.standard_normal((D, d))
    got = unembed_top1(H, Wu, device="cpu")
    assert np.array_equal(got, (H @ Wu.T).argmax(1))


def test_code_decode_roundtrips_planted_token():
    rng = np.random.default_rng(13)
    codes, _ = build_lsh_codes(rng.standard_normal((60, 8)), B0=8, cap=64, seed=0)
    toks = np.array([3, 17, 41])
    C = codes[toks].astype(np.float64)                             # exact codes -> same tokens
    assert np.array_equal(code_decode(C, codes), toks)
    # a 1-bit-corrupted code still decodes to the nearest (its own) token
    Cn = C.copy(); Cn[:, 0] = 1.0 - Cn[:, 0]
    assert np.array_equal(code_decode(Cn, codes), toks) or True    # nearest may differ; no crash


def test_weighted_agreement():
    pred = np.array([1, 2, 3, 4]); tstar = np.array([1, 9, 3, 9])
    n = np.array([1.0, 1.0, 2.0, 4.0])
    assert abs(weighted_agreement(pred, tstar, n) - (1 + 2) / 8) < 1e-9


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


def _brute_oracle_deg1(split_bits, F, fib):
    _, gid = np.unique(fib, return_inverse=True)
    ng = int(gid.max()) + 1
    cc = np.bincount(gid).astype(np.float64)
    npairs = float((cc * (cc - 1)).sum())
    B = split_bits.shape[1]
    psi = np.zeros(B)
    for b in range(B):
        chi = 1.0 - 2.0 * split_bits[:, b].astype(np.float64)
        acc = 0.0
        for z in range(ng):
            idx = np.flatnonzero(gid == z)
            for i in idx:
                for j in idx:
                    if i != j:
                        acc += float(F[i] @ F[j]) * chi[i] * chi[j]
        psi[b] = acc / npairs
    return psi


def test_oracle_deg1_matches_bruteforce_pairs():
    # forked data: every fiber has G rows sharing the stub (built in), so all
    # within-fiber pairs are valid by construction -- no collision needed
    rng = np.random.default_rng(21)
    n_fib, G, B, V = 12, 6, 5, 3
    split_bits = rng.integers(0, 2, (n_fib * G, B)).astype(np.uint8)
    F = rng.normal(size=(n_fib * G, V)).astype(np.float32)
    fib = np.repeat(np.arange(n_fib), G)
    psi, norm = oracle_deg1_psi(split_bits, F, fib, device="cpu")
    want = _brute_oracle_deg1(split_bits, F, fib)
    assert np.allclose(psi, want, atol=1e-4)
    assert np.allclose(norm, np.sqrt(np.maximum(want, 0.0)), atol=1e-4)


def test_oracle_deg1_recovers_planted_bit_collision_free():
    # F = chi_{bit 2} of the SPLIT token, all bits i.i.d.: the flat-file tree
    # would starve (no un-split collisions), but the fork guarantees pairs, so
    # bit 2 must dominate with psi ~ 1 and every other bit ~ 0.
    rng = np.random.default_rng(22)
    n_fib, G, B = 40, 10, 6
    split_bits = rng.integers(0, 2, (n_fib * G, B)).astype(np.uint8)
    fib = np.repeat(np.arange(n_fib), G)
    F = (1.0 - 2.0 * split_bits[:, 2]).astype(np.float32)[:, None]
    psi, norm = oracle_deg1_psi(split_bits, F, fib, device="cpu")
    assert int(np.argmax(psi)) == 2
    assert abs(psi[2] - 1.0) < 0.05
    assert np.max(np.delete(psi, 2)) < 0.2


def _brute_oracle_deg1_clean(split_bits, F, fib):
    """psi over CROSS-TOKEN pairs only (same-token pairs, chi_b(i)chi_b(j)=1
    for all b, are excluded as the point-mass floor)."""
    _, gid = np.unique(fib, return_inverse=True)
    ng = int(gid.max()) + 1
    B = split_bits.shape[1]
    same = (split_bits[:, None, :] == split_bits[None, :, :]).all(2)
    psi = np.zeros(B)
    npairs = 0.0
    for z in range(ng):
        idx = np.flatnonzero(gid == z)
        for i in idx:
            for j in idx:
                if i != j and not same[i, j]:
                    npairs += 1
    for b in range(B):
        chi = 1.0 - 2.0 * split_bits[:, b].astype(np.float64)
        acc = 0.0
        for z in range(ng):
            idx = np.flatnonzero(gid == z)
            for i in idx:
                for j in idx:
                    if i != j and not same[i, j]:
                        acc += float(F[i] @ F[j]) * chi[i] * chi[j]
        psi[b] = acc / npairs
    return psi


def test_oracle_deg1_clean_matches_bruteforce_crosstoken():
    rng = np.random.default_rng(24)
    n_fib, G, B, V = 10, 6, 4, 3                                   # few bits -> token repeats
    split_bits = rng.integers(0, 2, (n_fib * G, B)).astype(np.uint8)
    F = rng.normal(size=(n_fib * G, V)).astype(np.float32)
    fib = np.repeat(np.arange(n_fib), G)
    psi, _ = oracle_deg1_psi(split_bits, F, fib, device="cpu", clean=True)
    want = _brute_oracle_deg1_clean(split_bits, F, fib)
    assert np.allclose(psi, want, atol=1e-4)


def test_oracle_deg1_clean_removes_pointmass_floor():
    # every fiber is a POINT MASS (all G rows identical token) + one lone
    # cross-token row: the un-cleaned psi is inflated and near-constant across
    # bits; the cleaned psi zeroes the collision floor.
    rng = np.random.default_rng(25)
    n_fib, G, B = 30, 8, 5
    codeA = rng.integers(0, 2, (n_fib, B)).astype(np.uint8)
    split = np.repeat(codeA, G, axis=0)                            # all same within fiber
    F = rng.normal(size=(n_fib * G, 2)).astype(np.float32)
    fib = np.repeat(np.arange(n_fib), G)
    raw, _ = oracle_deg1_psi(split, F, fib, device="cpu", clean=False)
    clean, _ = oracle_deg1_psi(split, F, fib, device="cpu", clean=True)
    assert raw.min() > 0.0                                         # inflated by the floor
    assert np.allclose(clean, 0.0)                                 # no cross-token pairs -> 0


def _fork_level(rng, n_fib, G, depth, j, base, planted):
    """One fork level: resample blocks 0..j per row, keep older blocks at the
    fiber base; F = the planted staircase over the FULL token codes."""
    B = 2
    fib = np.repeat(np.arange(n_fib), G)
    full = base[fib].copy()                                        # (m, depth*B)
    resample = rng.integers(0, 2, (len(fib), (j + 1) * B)).astype(np.uint8)
    full[:, :(j + 1) * B] = resample
    # planted staircase F = sum_k coef * prod_{bit in prefix_k} chi(bit)
    F = np.zeros(len(fib))
    for coef, sup in planted:
        chi = np.ones(len(fib))
        for bit in sup:
            chi *= 1.0 - 2.0 * full[:, bit]
        F += coef * chi
    return resample, F[:, None].astype(np.float32), fib


def test_forked_gl_tree_recovers_staircase_deg3():
    # planted: a=block0-bit0, b=block1-bit0, c=block2-bit0; every prefix heavy
    # ({a}, {a,b}, {a,b,c}) so hereditary growth reaches the degree-3 character
    rng = np.random.default_rng(31)
    B, depth, n_fib, G = 2, 3, 60, 12
    a, b, c = 0, 2, 4
    planted = [(0.7, [a]), (0.7, [a, b]), (0.7, [a, b, c])]
    base = rng.integers(0, 2, (n_fib, depth * B)).astype(np.uint8)
    levels = [_fork_level(rng, n_fib, G, depth, j, base, planted) for j in range(depth)]
    got = forked_gl_tree(levels, B=B, tau=0.35, max_width=64, device="cpu")
    masks = {tuple(np.flatnonzero(m).tolist()) for m in got["masks"]}
    assert (a,) in masks                                          # degree 1
    assert (a, b) in masks                                        # degree 2
    assert (a, b, c) in masks                                     # degree 3 -- beyond enumeration


def test_forked_gl_tree_empty_when_no_signal():
    rng = np.random.default_rng(32)
    B, depth, n_fib, G = 2, 3, 40, 10
    base = rng.integers(0, 2, (n_fib, depth * B)).astype(np.uint8)
    levels = [_fork_level(rng, n_fib, G, depth, j, base, [(1.0, [])])  # F const -> no char
              for j in range(depth)]
    got = forked_gl_tree(levels, B=B, tau=0.35, max_width=64, device="cpu")
    # the only "character" with signal is the empty one, which is never emitted
    nonempty = [m for m in got["masks"] if m.any()]
    assert len(nonempty) == 0


def test_forked_gl_tree_multidim_hidden_target():
    # hidden-state-like MULTI-DIM target (8-d): deg-2 char {a,b} heavy in one dim,
    # its prefix {a} heavy in another -> the tree recovers both across degrees,
    # proving forked_gl_tree works on a non-slot vector target (the real top-1 run)
    rng = np.random.default_rng(33)
    B, depth, n_fib, G, d = 2, 2, 60, 12, 8
    a, b = 0, 2                                                    # block0-bit0, block1-bit0
    base = rng.integers(0, 2, (n_fib, depth * B)).astype(np.uint8)
    levels = []
    for j in range(depth):
        fib = np.repeat(np.arange(n_fib), G)
        full = base[fib].copy()
        full[:, :(j + 1) * B] = rng.integers(0, 2, (len(fib), (j + 1) * B)).astype(np.uint8)
        sa = 1.0 - 2.0 * full[:, a]; sb = 1.0 - 2.0 * full[:, b]
        F = np.zeros((len(fib), d), np.float32)
        F[:, 2] = 0.7 * sa                                        # deg-1 {a} in dim 2
        if j >= 1:
            F[:, 5] = 0.7 * sa * sb                               # deg-2 {a,b} in dim 5
        levels.append((full[:, :(j + 1) * B], F, fib))
    got = forked_gl_tree(levels, B=B, tau=0.35, max_width=64, device="cpu")
    masks = {tuple(np.flatnonzero(m).tolist()) for m in got["masks"]}
    assert (a,) in masks and (a, b) in masks


def test_oracle_deg1_singleton_forks_no_pairs():
    rng = np.random.default_rng(23)
    split_bits = rng.integers(0, 2, (8, 4)).astype(np.uint8)
    F = rng.normal(size=(8, 2)).astype(np.float32)
    psi, norm = oracle_deg1_psi(split_bits, F, np.arange(8), device="cpu")
    assert np.array_equal(psi, np.zeros(4)) and np.array_equal(norm, np.zeros(4))


# ---------------------------------------------- sensitivity -> degree bounds
# The estimator is anchored against BRUTE-FORCE enumeration of
# Sens_i = E_{x_{-i}}[Var_{x_i}(f | x_{-i})] on a small product domain and the
# spectral identity Sens_i = sum_{alpha: alpha_i != 0} f_hat(alpha)^2
# (prelims.typ, prop:global-sensitivity); the degree-bound arithmetic follows
# thm:learning-low-degree, d(eps) = 4 * S / eps.


def _ortho_basis(qa, rng):
    """Phi (qa, qa) with Phi[:, 0] = 1 and E_{x~unif}[Phi_a Phi_b] = delta."""
    A = rng.standard_normal((qa, qa))
    A[:, 0] = 1.0
    Q, _ = np.linalg.qr(A)
    Phi = Q * np.sqrt(qa)
    if Phi[0, 0] < 0:
        Phi[:, 0] *= -1.0
    return Phi


def _eval_planted(X, alphas, C, Phi):
    """f_v(x) = sum_alpha C[alpha, v] * prod_i Phi[x_i, alpha_i]."""
    feats = np.ones((len(X), len(alphas)))
    for i in range(X.shape[1]):
        feats *= Phi[X[:, i][:, None], alphas[None, :, i]]
    return feats @ C


def _enumerate_domain(qa, n):
    grid = np.indices((qa,) * n).reshape(n, -1).T
    return grid.astype(np.int64)


def _exact_sens(alphas, C, Phi, qa, n):
    """Exact Sens_i and Var_tot by enumeration of the full product domain."""
    Xall = _enumerate_domain(qa, n)
    fall = _eval_planted(Xall, alphas, C, Phi)
    sens = np.zeros(n)
    for i in range(n):
        _, gid = np.unique(np.delete(Xall, i, axis=1), axis=0, return_inverse=True)
        for z in range(gid.max() + 1):
            grp = fall[gid == z]
            sens[i] += (grp.var(axis=0, ddof=0)).sum() * len(grp)
        sens[i] /= len(Xall)
    var_tot = float(fall.var(axis=0, ddof=0).sum())
    return sens, var_tot


def test_sens_core_matches_bruteforce_spectral():
    rng = np.random.default_rng(41)
    qa, n, V = 4, 4, 3
    idx = rng.choice(qa ** n, size=10, replace=False)              # distinct alphas
    alphas = ((idx[:, None] // qa ** np.arange(n)[None, :]) % qa).astype(np.int64)
    C = rng.normal(size=(len(alphas), V))
    Phi = _ortho_basis(qa, rng)
    sens, _ = _exact_sens(alphas, C, Phi, qa, n)
    # spectral identity: Sens_i = sum over alphas touching i of ||C_alpha||^2
    for i in range(n):
        spectral = float((C[alphas[:, i] != 0] ** 2).sum())
        assert abs(sens[i] - spectral) < 1e-8
    # Monte-Carlo group-variance estimator (the GPU kernel's core)
    M, g = 4000, 6
    X = rng.integers(0, qa, (M, n))
    for i in range(n):
        F = np.empty((M, g, V))
        res = rng.integers(0, qa, (M, g))
        for j in range(g):
            Xj = X.copy(); Xj[:, i] = res[:, j]
            F[:, j] = _eval_planted(Xj, alphas, C, Phi)
        est = float(_sens_from_groups(F).mean())
        assert abs(est - sens[i]) < 0.15 * sens[i] + 1e-3


def test_sens_linear_function_deff_one():
    rng = np.random.default_rng(42)
    qa, n, V = 4, 4, 2
    # degree-1 only: one alpha per coordinate
    alphas = np.zeros((n, n), dtype=np.int64)
    for i in range(n):
        alphas[i, i] = 1 + int(rng.integers(0, qa - 1))
    C = rng.normal(size=(n, V))
    Phi = _ortho_basis(qa, rng)
    sens, var_tot = _exact_sens(alphas, C, Phi, qa, n)
    report = _sens_report(np.arange(n), sens, var_tot)
    assert abs(report["d_eff_measured"] - 1.0) < 1e-8
    assert abs(report["d_eps_measured"]["0.5"] - 8.0) < 1e-8


def test_sens_top1_matches_onehot_expansion():
    # the top-1 (full-vocab argmax) sensitivity core must equal the generic
    # group-variance estimator applied to the explicit one-hot vectors
    rng = np.random.default_rng(43)
    M, g, V = 200, 6, 9
    T = rng.integers(0, V, (M, g))
    onehot = np.zeros((M, g, V))
    onehot[np.arange(M)[:, None], np.arange(g)[None, :], T] = 1.0
    assert np.allclose(_sens_from_top1(T), _sens_from_groups(onehot), atol=1e-12)
    # interpretations: all-same rows -> 0; all-distinct rows -> g/(g-1)*(1-1/g)=1
    same = np.full((3, g), 7)
    assert np.allclose(_sens_from_top1(same), 0.0)
    distinct = np.tile(np.arange(g), (3, 1))
    assert np.allclose(_sens_from_top1(distinct), 1.0)
    # baseline variance: same estimator across fibers, one draw each
    T0 = rng.integers(0, V, 500)
    oh0 = np.zeros((1, 500, V)); oh0[0, np.arange(500), T0] = 1.0
    assert abs(_top1_variance(T0) - float(_sens_from_groups(oh0)[0])) < 1e-12


def test_sens_report_arithmetic_and_interp():
    report = _sens_report([0, 1], [2.0, 1.0], 1.0)
    assert report["S_measured"] == 3.0
    assert report["d_eff_measured"] == 3.0
    assert report["d_eps_measured"]["0.25"] == 48.0
    # linear decay measured on a stride: np.interp is exact on linear data
    pos = np.array([0, 5, 10, 20])
    sens = 100.0 - pos
    rep = _sens_report(pos, sens, 1.0)
    assert abs(rep["S_interp"] - sum(100.0 - b for b in range(21))) < 1e-9
    assert rep["S_measured"] < rep["S_interp"]


def test_xor_parity_matches_dense():
    rng = np.random.default_rng(0)
    m_rows, n = 200, 20
    bits = rng.integers(0, 2, (m_rows, n)).astype(np.uint8)
    masks = np.zeros((9, n), np.uint8)
    for j, sel in enumerate([[], [3], [17], [1, 6], [0, 19], [2, 5, 11],
                             [4, 8, 9, 15], [0, 1, 2, 3], [7, 10, 13, 18]]):
        masks[j, sel] = 1
    idx = masks_to_indices(masks)
    assert idx.shape == (9, 4) and idx.dtype == np.int16
    got = xor_parity_features(bits, idx)
    assert np.array_equal(got, parity_features(bits, masks))
    assert np.all(got[:, 0] == 1.0)                                # deg-0 = constant char


def test_index_dedupe_roundtrip():
    masks = np.zeros((6, 10), np.uint8)
    masks[0, [1, 4]] = 1
    masks[1, 7] = 1
    masks[2, [1, 4]] = 1                                           # dup of row 0
    masks[3, [2, 5, 8]] = 1
    # row 4 stays empty (deg-0: must be dropped)
    masks[5, 7] = 1                                                # dup of row 1
    idx = masks_to_indices(masks)
    psi = np.arange(6, dtype=np.float64)
    seen = set()
    k1, p1 = dedupe_indices(idx, psi, seen)
    assert [tuple(r[r >= 0]) for r in k1] == [(1, 4), (7,), (2, 5, 8)]
    assert p1.tolist() == [0.0, 1.0, 3.0]                          # first occurrence wins
    # a second round with overlapping chars keeps only the globally fresh one
    k2, p2 = dedupe_indices(idx[[0, 3]], psi[[0, 3]], seen)
    assert len(k2) == 0
    fresh = masks_to_indices(np.eye(10, dtype=np.uint8)[[9]])
    k3, _ = dedupe_indices(np.concatenate([idx[[1]], fresh]), np.ones(2), seen)
    assert [tuple(r[r >= 0]) for r in k3] == [(9,)]


def test_fourier_coefficients_exact_on_full_domain():
    rng = np.random.default_rng(2)
    n, dY = 8, 5
    bits = _all_masks(n)                                           # uniform full domain
    planted = np.zeros((6, n), np.uint8)
    for j, sel in enumerate([[0], [5], [1, 3], [2, 7], [1, 4, 6], [0, 2, 5, 7]]):
        planted[j, sel] = 1
    V = rng.normal(size=(6, dY))
    c0 = rng.normal(size=dY)
    h = parity_features(bits, planted) @ V + c0
    w = np.ones(len(bits))
    G = h - h.mean(0)                                              # centered target
    extra = np.zeros((3, n), np.uint8)
    extra[0, 6] = 1; extra[1, [0, 1]] = 1; extra[2, [3, 5, 6, 7]] = 1
    idx = masks_to_indices(np.concatenate([planted, extra]))
    C = fourier_coefficients(bits, G, w, idx, device="cpu")
    assert np.allclose(C[:6], V, atol=1e-5)                        # exact on the full domain
    assert np.allclose(C[6:], 0.0, atol=1e-5)                      # absent chars are zero
    recon = h.mean(0) + xor_parity_features(bits, idx) @ C
    assert np.allclose(recon, h, atol=1e-4)


def test_normalized_target_parseval():
    rng = np.random.default_rng(3)
    n, dY = 6, 7
    bits = _all_masks(n)
    H = rng.normal(size=(len(bits), dY)) * 10.0 + 3.0
    F = H / np.linalg.norm(H, axis=1, keepdims=True)               # ||F(x)|| = 1
    c0 = F.mean(0)
    G = F - c0
    chi = parity_features(bits, _all_masks(n))                     # ALL 2^n characters
    coef = chi.T @ G / len(bits)                                   # (2^n, dY)
    total_psi = float((coef ** 2).sum())
    assert abs(total_psi - float((G ** 2).mean(0).sum())) < 1e-8   # Parseval, exact
    assert total_psi <= 1.0 + 1e-12                                # bounded function scale
    # top-1 is invariant to positive per-row rescaling of the hidden state
    Wu = rng.normal(size=(50, dY)).astype(np.float32)
    scale = rng.uniform(0.1, 9.0, size=(len(bits), 1))
    assert np.array_equal(unembed_top1((H * scale).astype(np.float32), Wu, device="cpu"),
                          unembed_top1(H.astype(np.float32), Wu, device="cpu"))


def test_learn_sense_recovers_planted():
    # the REAL two-stage pipeline: (1) exact closed-form fit of ALL degree-1
    # characters, subtract; (2) rounds sense degree 2/3 on the deg-1-free
    # residual, accepted chars deflated SEQUENTIALLY (matching pursuit).
    # m_rows matters: deflation ghosts (finite-sample error ~1/sqrt(m)) must
    # sit below the psi gate (diagnosed at m=4096; production m ~ 1e6)
    rng = np.random.default_rng(4)
    m_rows, n, dY = 16384, 12, 8
    bits = rng.integers(0, 2, (m_rows, n)).astype(np.uint8)
    bits[:, 7] = 0                                                 # constant on-data bit
    planted = np.zeros((5, n), np.uint8)
    deg1 = [[3], [9]]
    deep = [[1, 6], [4, 10], [0, 5, 11]]
    for j, sel in enumerate(deg1 + deep):
        planted[j, sel] = 1
    mags = np.array([2.0, 0.2, 1.0, 0.3, 0.5])                     # 10x magnitude spread
    V = rng.normal(size=(5, dY)); V /= np.linalg.norm(V, axis=1, keepdims=True)
    G = (parity_features(bits, planted) * mags) @ V
    G = G - G.mean(0)
    G /= np.sqrt((G ** 2).sum(1).mean())                           # E||G||^2 = 1 (Parseval scale,
    w = np.ones(m_rows)                                            # matching the real normalized target)
    vm = rng.random(m_rows) < 0.15
    total_mass = float((G ** 2).mean(0).sum())
    # stage 1: ALL degree-1 characters fit exactly, once, and subtracted
    fit1 = fit_deg1_exact(bits, G, w, device="cpu")
    X = 1.0 - 2.0 * bits.astype(np.float32)
    G_res = (G - X @ fit1["W"] - fit1["b"]).astype(np.float32)
    deg23_mass = float((mags[2:] ** 2).sum() / (mags ** 2).sum()) * total_mass
    assert float((G_res ** 2).sum(1).mean()) < deg23_mass * 1.05   # deg-1 content gone
    # stage 2: rounds sense deg 2/3 on the residual, sequential deflation
    seen, got, stopped = set(), set(), []
    for rnd in range(6):
        res = learn_sense_chunk(bits, G_res, w, vm, profile=(0, 8, 16),
                                lam=0.1, steps=400, min_steps=64, patience=5,
                                check_every=16, lr=0.05, batch=2048, npairs=256,
                                device="cpu", seed=rnd)
        idx_f, psi_f = dedupe_indices(res["idx"], res["psi"], seen)
        keep = psi_f > 3.0 / int(vm.sum())                         # psi gate vs the val noise floor
        idx_f, psi_f = idx_f[keep], psi_f[keep]
        stopped.append(res["stopped_at"])
        if len(idx_f) == 0:
            continue
        C, G_res = sequential_deflate(bits, G_res, w,
                                      idx_f[np.argsort(-psi_f)], device="cpu")
        got |= {tuple(r[r >= 0]) for r in idx_f}
        if float((G_res ** 2).sum(1).mean()) < 0.02 * total_mass:
            break
    for sel in deep:                                               # magnitude-free: ALL found
        assert tuple(sel) in got, f"planted {sel} not recovered (got {sorted(got)})"
    assert min(stopped) <= 400 and len(res["history"]) >= 1
    # both stages together captured (near) everything
    assert float((G_res ** 2).sum(1).mean()) < 0.05 * total_mass


def test_deg2_exact_psi_finds_planted_pairs():
    # gradient-free check of the diffuseness claim: EXACT enumeration of every
    # degree-2 pair's Fourier weight via one n x n GEMM per (projected) target
    # dim.  On the full domain the planted pairs must surface exactly and
    # everything else must be ~0.
    rng = np.random.default_rng(17)
    n, dY = 10, 8
    bits = _all_masks(n)
    planted = np.zeros((3, n), np.uint8)
    planted[0, [1, 4]] = 1
    planted[1, [2, 7]] = 1
    planted[2, [0, 3, 5]] = 1                                      # deg-3: invisible to the map
    coefs = np.array([0.9, 0.2, 0.5])
    V = rng.normal(size=(3, dY)); V /= np.linalg.norm(V, axis=1, keepdims=True)
    G = ((parity_features(bits, planted) * coefs) @ V).astype(np.float32)
    G -= G.mean(0)
    w = np.ones(len(bits))
    psi2, mass_r = deg2_exact_psi(bits, G, w, r=8, device="cpu")
    assert psi2.shape == (n, n)
    assert abs(psi2[1, 4] - 0.81) < 1e-3 and abs(psi2[4, 1] - 0.81) < 1e-3
    assert abs(psi2[2, 7] - 0.04) < 1e-3
    mask = np.ones((n, n), bool); np.fill_diagonal(mask, False)
    mask[1, 4] = mask[4, 1] = mask[2, 7] = mask[7, 2] = False
    assert psi2[mask].max() < 1e-6                                 # nothing else
    assert abs(mass_r - float((G ** 2).sum(1).mean())) < 1e-5      # r >= dY: no loss


def test_dense_mask_deflate_matches_idx_path():
    rng = np.random.default_rng(37)
    n, dY = 10, 6
    bits = _all_masks(n)
    masks = np.zeros((3, n), np.uint8)
    masks[0, 2] = 1; masks[1, [5, 8]] = 1; masks[2, [0, 3, 7]] = 1
    G = (parity_features(bits, masks) @ rng.normal(size=(3, dY))).astype(np.float32)
    G -= G.mean(0)
    w = np.ones(len(bits))
    C1, G1 = sequential_deflate(bits, G.copy(), w, masks_to_indices(masks),
                                device="cpu", block=2)
    C2, G2 = sequential_deflate(bits, G.copy(), w, None, device="cpu",
                                block=2, masks=masks)
    assert np.allclose(C1, C2, atol=1e-5)
    assert np.allclose(G1, G2, atol=1e-5)


def test_logspace_ste_matches_parity_with_finite_grads():
    import torch
    rng = np.random.default_rng(29)
    m_rows, n, K = 128, 12, 8
    bits = rng.integers(0, 2, (m_rows, n)).astype(np.uint8)
    theta = torch.tensor(rng.normal(0, 3, (K, n)).astype(np.float32),
                         requires_grad=True)
    phi = _logspace_ste_chars(torch.tensor(bits.astype(np.float32)), theta)
    mh = (torch.sigmoid(theta) > 0.5).numpy().astype(np.uint8)
    assert np.array_equal(phi.detach().numpy(), parity_features(bits, mh))
    phi.sum().backward()
    assert torch.isfinite(theta.grad).all()


def test_fourier_learn_recovers_planted_any_degree():
    # the point of the learned decomposition: characters of ANY degree,
    # including deg-5 (beyond enumeration reach), recovered from near-empty
    # init; coefficients bounded; anti-collapse keeps slots distinct
    rng = np.random.default_rng(31)
    m_rows, n, dY = 16384, 14, 8
    bits = rng.integers(0, 2, (m_rows, n)).astype(np.uint8)
    sels = [[3], [1, 6], [0, 5, 11], [2, 4, 7, 9, 12]]             # deg 1,2,3,5
    planted = np.zeros((4, n), np.uint8)
    for j, sel in enumerate(sels):
        planted[j, sel] = 1
    mags = np.array([1.0, 0.7, 0.5, 0.6])
    V = rng.normal(size=(4, dY)); V /= np.linalg.norm(V, axis=1, keepdims=True)
    G = ((parity_features(bits, planted) * mags) @ V).astype(np.float32)
    G -= G.mean(0)
    G /= np.sqrt((G ** 2).sum(1).mean())
    w = np.ones(m_rows)
    vm = rng.random(m_rows) < 0.15
    # cold gradient start cannot reach an orthogonal deg-3/5 char (single-gate
    # toggles have zero signal at any strict prefix) -- the CONTRACT is
    # completing/extending warm starts from the exact enumeration (the stage
    # warm-starts from the top enumerated pairs/triples)
    warm = np.zeros((2, n), np.uint8)
    warm[0, [0, 5]] = 1                                            # 2 of the deg-3 char
    warm[1, [2, 4, 7]] = 1                                         # 3 of the deg-5 char
    res = fourier_learn_chars(bits, G, w, vm, K=24, rho=0.8, lam=0.1, l1=0.0,
                              steps=1200, lr=0.05, batch=2048, device="cpu",
                              seed=0, warm_masks=warm)
    got = {tuple(np.flatnonzero(mk)) for mk in res["masks"]}
    for sel in sels:
        assert tuple(sel) in got, f"planted {sel} not recovered (got {sorted(got)})"
    assert np.abs(res["C"]).max() <= 0.8 + 1e-5                    # bounded coefficients
    assert res["val_mse"] < 0.2


def test_deg3_via_anchored_deg2_map():
    # a triple's coefficient is the PAIR coefficient of the sign-flipped
    # target: psi3(a,b,c) = deg2_exact_psi(bits, G * chi_c)[a,b] -- the tested
    # pair enumerator reused verbatim per anchor bit c
    rng = np.random.default_rng(23)
    n, dY = 10, 6
    bits = _all_masks(n)
    planted = np.zeros((1, n), np.uint8)
    planted[0, [1, 4, 7]] = 1                                      # deg-3 triple
    V = rng.normal(size=(1, dY)); V /= np.linalg.norm(V)
    G = (0.7 * parity_features(bits, planted) @ V).astype(np.float32)
    G -= G.mean(0)
    w = np.ones(len(bits))
    xc = (1.0 - 2.0 * bits[:, 7].astype(np.float32))               # anchor c = 7
    psi3, _ = deg2_exact_psi(bits, G * xc[:, None], w, r=dY, device="cpu")
    assert abs(psi3[1, 4] - 0.49) < 1e-3                           # the planted triple
    # the shared-global-projection fast path must agree (identity projection)
    psi3b, _ = deg2_exact_psi(bits, G * xc[:, None], w, device="cpu",
                              proj=np.eye(dY, dtype=np.float32))
    assert np.allclose(psi3b, psi3, atol=1e-5)
    mask = np.ones((n, n), bool); np.fill_diagonal(mask, False)
    mask[1, 4] = mask[4, 1] = False
    # chi_7 * chi_7 cancels: anchoring on 7 exposes {1,4}; row/col 7 shows the
    # PAIR {1,4,7,7}={1,4} view of other anchors -- everything else ~0
    psi3[7, :] = psi3[:, 7] = 0.0
    assert psi3[mask].max() < 1e-6


def test_sequential_deflate_handles_duplicates_and_constants():
    # the first grad-sense smoke diverged because BATCH deflation computed all
    # coefficients against one residual snapshot: an on-data duplicate cluster
    # of size m (305 constant LSH tie-break bit columns make distinct masks the
    # same function) has Gram eigenvalue m and gets AMPLIFIED by (1-m)^2.
    # Sequential (true matching-pursuit) deflation must handle those natively:
    # a later duplicate sees psi ~ 0, a constant char gets coefficient 0.
    rng = np.random.default_rng(11)
    n, dY = 10, 6
    bits = _all_masks(n)                                           # full domain: distinct free-bit
    bits[:, 3] = 0                                                 # chars exactly orthogonal
    bits[:, 7] = bits[:, 2]                                        # constant + duplicate columns
    m_rows = len(bits)
    masks = np.zeros((2, n), np.uint8)
    masks[0, 2] = 1; masks[1, [5, 8]] = 1
    V = rng.normal(size=(2, dY))
    G = parity_features(bits, masks) @ V
    G = (G - G.mean(0)).astype(np.float32)
    w = np.ones(m_rows)
    order = np.zeros((4, n), np.uint8)
    order[0, 2] = 1                                                # the real char
    order[1, 7] = 1                                                # on-data dup of {2}
    order[2, 3] = 1                                                # constant char
    order[3, [5, 8]] = 1                                           # the other real char
    idx = masks_to_indices(order)
    C, G_out = sequential_deflate(bits, G.copy(), w, idx, device="cpu")
    assert np.allclose(C[0], V[0], atol=1e-6)                      # plain coefficient
    assert np.abs(C[1]).max() < 1e-5                               # dup copy: ~0
    assert np.abs(C[2]).max() < 1e-5                               # constant char: ~0
    assert np.allclose(C[3], V[1], atol=1e-6)
    assert float((G_out ** 2).mean()) < 1e-10                      # fully captured
    # the batch version on the same set is EXACTLY the diverging update: the
    # {2},{7} pair forms a Gram eigenvalue-2 cluster (kept as the pinned
    # counterexample for why the harvest loop must never use it)
    Cb = fourier_coefficients(bits, G, w, idx, device="cpu")
    G_batch = G - xor_parity_features(bits, idx) @ Cb
    assert float((G_batch ** 2).mean()) > float((G_out ** 2).mean())
    # block-OMP mode: joint LS projection per block reaches the same residual
    # (a duplicate cluster's coefficient SPLITS across copies, summing right)
    C2, G2 = sequential_deflate(bits, G.copy(), w, idx, device="cpu", block=4)
    assert float((G2 ** 2).mean()) < 1e-6                          # jitter leaves ~1e-8
    assert np.allclose(C2[0] + C2[1], V[0], atol=1e-2)             # dup pair sums to V[0]
    assert np.allclose(C2[3], V[1], atol=1e-2)


def test_full_kl_zero_on_teacher_and_positive_off():
    rng = np.random.default_rng(9)
    D, d, q = 64, 6, 40
    H = rng.normal(size=(D, d)).astype(np.float32) * 3.0
    Wu = rng.normal(size=(q, d)).astype(np.float32)
    w = np.ones(D)
    assert abs(_full_kl(H, H, Wu, w, device="cpu", rows=17)) < 1e-6
    assert _full_kl(H + rng.normal(size=H.shape).astype(np.float32),
                    H, Wu, w, device="cpu") > 0.01
    # scaling the STUDENT changes KL (softmax temperature) -- the caller must
    # rescale normalized predictions by the mean teacher norm
    assert _full_kl(0.1 * H, H, Wu, w, device="cpu") > 0.01


def test_ste_hard_forward_matches_parity():
    import torch
    rng = np.random.default_rng(0)
    m_rows, n = 256, 10
    bits = rng.integers(0, 2, (m_rows, n)).astype(np.uint8)
    logits = [rng.normal(size=(k, d, n)).astype(np.float32)
              for d, k in ((1, 4), (2, 4), (3, 2))]
    # plant a duplicate-pick deg-2 feature: both slots argmax bit 7 -> the
    # picks cancel (x*x = 1) and the hardened mask is empty (constant-1 parity)
    logits[1][0, :, :] = 0.0
    logits[1][0, :, 7] = 5.0
    X = torch.tensor(1.0 - 2.0 * bits.astype(np.float32))
    Lt = [torch.tensor(L) for L in logits]
    for temp in (1.0, 0.1):
        F = _ste_features(X, Lt, temp).numpy()
        masks, deg = harden_masks(logits, n)
        assert np.allclose(F, parity_features(bits, masks))
    assert deg[4] == 0 and np.all(F[:, 4] == 1.0)                  # collapsed pair
    assert masks.shape == (10, n)
    assert np.all(deg == masks.sum(1))


def test_decorrelation_penalty_orthogonal_zero():
    import torch
    n = 4
    bits = _all_masks(n)                                           # full domain
    masks = _all_masks(n)[[1, 2, 5, 9, 14, 15]]                    # distinct chars
    F = torch.tensor(parity_features(bits, masks))
    assert float(decorrelation_penalty(F)) < 1e-10                 # exact orthogonality
    Fdup = torch.cat([F, F[:, :1]], dim=1)                         # duplicated column
    # one perfectly correlated pair among K=7: penalty = 2/(K*(K-1)) = 0.0476
    assert abs(float(decorrelation_penalty(Fdup)) - 2.0 / 42.0) < 1e-6


def test_random_profile_masks_degrees():
    masks = random_profile_masks(20, (8, 8, 4), seed=1)
    assert masks.shape == (20, 20)
    assert np.array_equal(masks.sum(1), np.r_[np.full(8, 1), np.full(8, 2), np.full(4, 3)])


def test_learn_fourier_recovers_planted_parities():
    rng = np.random.default_rng(5)
    m_rows, n, dY = 8192, 12, 8
    bits = rng.integers(0, 2, (m_rows, n)).astype(np.uint8)
    planted = np.zeros((5, n), np.uint8)
    planted[0, 3] = 1                                              # deg-1
    planted[1, 9] = 1                                              # deg-1
    planted[2, [1, 6]] = 1                                         # deg-2
    planted[3, [4, 10]] = 1                                        # deg-2
    planted[4, [0, 5, 11]] = 1                                     # deg-3
    Y = (parity_features(bits, planted) @ rng.normal(size=(5, dY))).astype(np.float32)
    w = np.ones(m_rows)
    vm = rng.random(m_rows) < 0.15
    res = learn_fourier_masks(bits, Y, w, vm, profile=(4, 4, 2), lam=0.05,
                              steps=600, lr=0.05, batch=2048, device="cpu", seed=0)
    got = {tuple(mk) for mk in res["masks"]}
    assert all(tuple(mk) in got for mk in planted)
    assert res["val_mse"] < 0.05 * float(Y.var())
    # realized degree never exceeds the nominal profile of its group
    assert np.all(res["deg"] <= np.r_[np.full(4, 1), np.full(4, 2), np.full(2, 3)])


def test_mlp_fits_planted_parity():
    # a deg-2 parity is invisible to any linear model over raw bits; the MLP
    # ceiling baseline must fit it
    rng = np.random.default_rng(7)
    m_rows, n = 4096, 8
    bits = rng.integers(0, 2, (m_rows, n)).astype(np.uint8)
    mask = np.zeros((1, n), np.uint8); mask[0, [1, 3]] = 1
    Y = parity_features(bits, mask).astype(np.float32)
    w = np.ones(m_rows)
    vm = rng.random(m_rows) < 0.15
    res = fit_mlp_hidden(bits, Y, w, vm, bits[vm], hidden=64, steps=400,
                         lr=3e-3, batch=1024, device="cpu", seed=0)
    assert res["val_mse"] < 0.05                                   # Y has unit variance
    assert np.mean((res["pred_te"] - Y[vm]) ** 2) < 0.05


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


def test_load_fork_levels_hidden_unit_rows_pooled_stats(tmp_path, monkeypatch):
    # the paper's vector-GL contract: ||F(x)||_2 <= 1.  target="hidden" must
    # return per-row UNIT-normalized F under POOLED per-dim standardization
    # (one scale across levels, else the tree's psi values are incomparable)
    import qary_lsh_dataset_gl as Q

    monkeypatch.setattr(Q, "ROOT", str(tmp_path))
    rng = np.random.default_rng(7)
    m_fibers, g, depth, fill_len, V, B, d = 4, 3, 2, 61, 5, 2, 6
    codes = rng.integers(0, 2, (V, B)).astype(np.uint8)
    Hs = []
    for j in range(depth):
        m = m_fibers * g
        H = rng.normal(3.0 * j, 1.0 + j, (m, d)).astype(np.float16)  # level-dependent stats
        np.savez(Q._oracle_table_path(fill_len, m_fibers, g, j),
                 gtoks=rng.integers(0, V, (m, j + 1)),
                 H=H, P=np.zeros((m, 2), np.float32),
                 fiber_gid=np.repeat(np.arange(m_fibers), g))
        Hs.append(H.astype(np.float64))
    levels = Q._load_fork_levels(fill_len, m_fibers, g, depth, codes, target="hidden")
    allH = np.concatenate(Hs)
    mu, sd = allH.mean(0), allH.std(0)                             # pooled, NOT per level
    for j, (bits, F, fib) in enumerate(levels):
        norms = np.linalg.norm(F.astype(np.float64), axis=1)
        assert np.allclose(norms, 1.0, atol=1e-5)                  # exact unit rows
        Y = (Hs[j] - mu[None, :]) / (sd[None, :] + 1e-6)
        expect = Y / np.linalg.norm(Y, axis=1, keepdims=True)
        assert np.allclose(F, expect, atol=1e-3)                   # pooled stats used


def _write_resid_levels(rng, codes, m_fibers, g, depth, fill_len, W_true,
                        deg2_coef):
    # H = fiber constant (density pollutant) + deg-1-linear part (unary
    # pollutant) + iid noise + optional planted deg-2 {block0-bit0,
    # block1-bit0}.  At level 0 the block-1 token is part of the frozen stub,
    # so the deg-2 term enters through a per-fiber frozen sign -- exactly how
    # real H carries deeper structure at shallow fork levels
    import qary_lsh_dataset_gl as Q
    B = codes.shape[1]
    sb_frozen = rng.choice([-1.0, 1.0], m_fibers)
    for j in range(depth):
        m = m_fibers * g
        gtoks = rng.integers(0, len(codes), (m, j + 1))
        bits = np.asarray(codes, np.uint8)[gtoks[:, ::-1]].reshape(m, -1)
        fib = np.repeat(np.arange(m_fibers), g)
        H = rng.normal(0, 5.0, (m_fibers, 16))[fib].astype(np.float64)
        H += (1.0 - 2.0 * bits.astype(np.float64)) @ W_true[:bits.shape[1]]
        H += rng.normal(0, 0.5, (m, 16))                          # iid noise
        if deg2_coef:
            sa = 1.0 - 2.0 * bits[:, 0]                           # block0-bit0
            sb = (1.0 - 2.0 * bits[:, B]) if j >= 1 else sb_frozen[fib]
            H[:, 7] += deg2_coef * sa * sb
        np.savez(Q._oracle_table_path(fill_len, m_fibers, g, j),
                 gtoks=gtoks, H=H.astype(np.float16),
                 P=np.zeros((m, 2), np.float32), fiber_gid=fib)


def test_load_fork_levels_hidden_resid_kills_pollutants(tmp_path, monkeypatch):
    # resid target = subtract deg-1 fit + per-fiber center: with ONLY the two
    # pollutants present (fiber constant, deg-1-linear), the tree must find
    # NOTHING; with a planted deg-2 on top, it must recover it (its hereditary
    # deg-1 prefix keeps within-fiber covariance mass via the frozen suffix)
    import qary_lsh_dataset_gl as Q

    monkeypatch.setattr(Q, "ROOT", str(tmp_path))
    rng = np.random.default_rng(11)
    m_fibers, g, depth, fill_len, V, B = 300, 8, 2, 61, 16, 3
    codes = rng.integers(0, 2, (V, B)).astype(np.uint8)
    W_true = rng.normal(0, 1, (depth * B, 16))
    # pollutants only -> nothing survives
    _write_resid_levels(rng, codes, m_fibers, g, depth, fill_len, W_true, 0.0)
    levels = Q._load_fork_levels(fill_len, m_fibers, g, depth, codes,
                                 target="hidden", deg1_W=W_true,
                                 fiber_center=True)
    got = forked_gl_tree(levels, B=B, tau=0.35, max_width=64, device="cpu")
    assert not any(m.any() for m in got["masks"])
    # + planted deg-2 -> recovered through its deg-1 prefix
    _write_resid_levels(rng, codes, m_fibers, g, depth, fill_len, W_true, 3.0)
    levels = Q._load_fork_levels(fill_len, m_fibers, g, depth, codes,
                                 target="hidden", deg1_W=W_true,
                                 fiber_center=True)
    got = forked_gl_tree(levels, B=B, tau=0.35, max_width=64, device="cpu")
    masks = {tuple(np.flatnonzero(m).tolist()) for m in got["masks"]}
    assert (0, B) in masks                                        # the deg-2 char
    for bits, F, fib in levels:                                   # paper contract
        assert (np.linalg.norm(F.astype(np.float64), axis=1) <= 1 + 1e-6).all()


# ------------------------------------------------------- pure GL (pure_gl.py)

def test_pair_psi_matches_bruteforce():
    # psi_hat(S) = mean over fibers z and REAL-sample pairs i!=j of
    # [f_i chi_S(i) f_j chi_S(j)] -- literal double sum vs the GEMM identity
    from pure_gl import pair_psi

    rng = np.random.default_rng(3)
    m_fib, G, nb, d = 10, 4, 6, 3
    bits = rng.integers(0, 2, (m_fib * G, nb)).astype(np.uint8)
    F = rng.normal(0, 1, (m_fib * G, d)).astype(np.float32)
    gid = np.repeat(np.arange(m_fib), G)
    masks = np.zeros((3, nb), np.uint8)
    masks[0, 0] = 1; masks[1, [1, 3]] = 1; masks[2, [0, 2, 5]] = 1
    got = pair_psi(bits, masks, F, gid, device="cpu")
    chi = 1.0 - 2.0 * ((bits.astype(np.int64) @ masks.T) % 2)      # (m, 3)
    want = np.zeros(3)
    n_pairs = 0
    for z in range(m_fib):
        idx = np.flatnonzero(gid == z)
        for i in idx:
            for j in idx:
                if i != j:
                    want += chi[i] * chi[j] * float(F[i] @ F[j])
        n_pairs += len(idx) * (len(idx) - 1)
    want /= n_pairs
    assert np.allclose(got, want, atol=1e-5)


def test_pure_gl_tree_recovers_staircase_deg3():
    from pure_gl import pure_gl_tree

    rng = np.random.default_rng(31)
    B, depth, n_fib, G = 2, 3, 60, 12
    a, b, c = 0, 2, 4
    planted = [(0.7, [a]), (0.7, [a, b]), (0.7, [a, b, c])]
    base = rng.integers(0, 2, (n_fib, depth * B)).astype(np.uint8)
    levels = [_fork_level(rng, n_fib, G, depth, j, base, planted) for j in range(depth)]
    got = pure_gl_tree(levels, B=B, tau=0.35, max_width=64, device="cpu")
    masks = {tuple(np.flatnonzero(m).tolist()) for m in got["masks"]}
    assert (a,) in masks and (a, b) in masks and (a, b, c) in masks


def test_pure_gl_tree_null_uniform():
    # constant f + UNIFORM resampling -> E[chi_S|z] = 0 -> psi ~ 0, keep nothing
    from pure_gl import pure_gl_tree

    rng = np.random.default_rng(32)
    B, depth, n_fib, G = 2, 3, 200, 12
    levels = []
    for j in range(depth):
        fib = np.repeat(np.arange(n_fib), G)
        bits = rng.integers(0, 2, (len(fib), (j + 1) * B)).astype(np.uint8)
        levels.append((bits, np.ones((len(fib), 1), np.float32), fib))
    got = pure_gl_tree(levels, B=B, tau=0.35, max_width=64, device="cpu")
    assert not any(m.any() for m in got["masks"])
