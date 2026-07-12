"""TDD tests for on-policy diffusion-LM compression via dataset-CSAMP (distill_data / distill_lm).

All CPU, no real LLM: the model is stubbed (infill contract) or planted (sparse-Fourier E2E).
"""
import numpy as np
import pytest

from fda_exp.householder import hadamard_basis
from fda_exp.qary_gl import _char_columns, _encode_qary


def test_fixed_string_lands_in_high_bits():
    """Fiber invariant: with contexts in text order [g..., s...], the search's suffix group-by
    (idx >> fill_bits) must recover exactly the fixed string s."""
    V, w, k = 8, 4, 2
    rng = np.random.default_rng(0)
    g = rng.integers(1, V, (100, w - k)).astype(np.int64)
    s = rng.integers(1, V, (100, k)).astype(np.int64)
    C = np.hstack([g, s])
    bits = int(V).bit_length() - 1
    assert np.array_equal(_encode_qary(C, V) >> ((w - k) * bits), _encode_qary(s, V))


def test_soft_collapse_dedups_with_multiplicity():
    from fda_exp.distill_data import soft_collapse
    C = np.array([[1, 2], [1, 2], [3, 4], [1, 2]], dtype=np.int64)
    Cd, n_ctx, inv, m = soft_collapse(C)
    assert m == 4 and len(Cd) == 2 and sorted(n_ctx.tolist()) == [1, 3]
    assert np.array_equal(Cd[inv], C)


def test_fiber_disjoint_split():
    from fda_exp.distill_data import fiber_disjoint_split
    fiber = np.array([0, 0, 1, 2, 2, 3, 3, 3])
    tr, va = fiber_disjoint_split(fiber, val_frac=0.5, seed=0)
    assert np.array_equal(tr | va, np.ones(len(fiber), bool)) and not (tr & va).any()
    for f in np.unique(fiber):                                    # no fiber straddles the split
        rows = fiber == f
        assert tr[rows].all() or va[rows].all()
    assert va.any() and tr.any()


def test_project_to_slots_rows_sum_to_one():
    from fda_exp.distill_data import project_to_slots
    rng = np.random.default_rng(0)
    full = rng.random((5, 100))
    full /= full.sum(1, keepdims=True)
    slot_ids = np.array([-1, 7, 3, 11], dtype=np.int64)           # V=4, slot 0 = OTHER
    P = project_to_slots(full, slot_ids)
    assert P.shape == (5, 4) and np.allclose(P.sum(1), 1.0)
    assert np.allclose(P[:, 1], full[:, 7])
    assert np.allclose(P[:, 0], 1.0 - full[:, [7, 3, 11]].sum(1))


class _StubLM:
    """Random-logit stand-in for Dream: same call contract, full 50-token vocab."""
    def __init__(self, vocab=50):
        self.vocab = vocab

    def __call__(self, input_ids=None, **kw):
        import torch
        B, L = input_ids.shape
        g = torch.Generator().manual_seed(int(input_ids.sum().item()) % (2 ** 31))
        return type("Out", (), {"logits": torch.randn(B, L, self.vocab, generator=g)})()


def test_diffusion_infill_stays_in_alphabet():
    """Alphabet contract: filled positions come only from allowed_ids, fixed string untouched,
    no mask token survives; R fill-ins per fiber with matching fiber ids."""
    from fda_exp.distill_data import diffusion_infill
    V, w, k, M, R, mask_id, pre_len = 8, 4, 2, 6, 3, 49, 5
    rng = np.random.default_rng(0)
    allowed = rng.choice(40, size=V - 1, replace=False).astype(np.int64) + 1   # 7 allowed real ids
    S_tok = allowed[rng.integers(0, V - 1, (M, k))]
    PRE = rng.integers(1, 40, (M, pre_len)).astype(np.int64)                   # real-prefix stand-in
    C_tok, fiber = diffusion_infill(_StubLM(), PRE, S_tok, w, allowed, mask_id, R=R,
                                    temperature=1.0, batch=5, seed=0, device="cpu")
    assert C_tok.shape == (M * R, w) and fiber.shape == (M * R,)
    assert np.isin(C_tok[:, :w - k], allowed).all()               # fills in-alphabet
    assert np.array_equal(C_tok[:, w - k:], S_tok[fiber])         # fixed strings untouched
    assert (C_tok != mask_id).all()


def test_draw_fibers_contract():
    """Fibers anchor at real story positions: prefix is the ctx_len-w tokens before the window,
    NEXT token in-alphabet (window may contain out-of-alphabet ids), fixed strings distinct."""
    from fda_exp.distill_data import draw_fibers
    w, k, ctx, M = 4, 2, 12, 20
    rng = np.random.default_rng(0)
    allowed = np.arange(1, 9, dtype=np.int64)                     # ids 1..8 in-alphabet
    streams = [rng.integers(1, 12, size=rng.integers(40, 80)).astype(np.int64) for _ in range(30)]
    PRE, S, WIN, y = draw_fibers(streams, w, k, ctx, M, allowed, seed=0)
    assert PRE.shape == (M, ctx - w) and S.shape == (M, k) and WIN.shape == (M, w)
    assert np.array_equal(WIN[:, w - k:], S)                      # s = last k window tokens
    assert np.isin(y, allowed).all()                              # next token always in-alphabet
    assert len({tuple(r) for r in S}) == M                        # distinct fixed strings


def test_fit_additive_recovers_planted_additive():
    """Long-window path: a planted additive (deg<=1) model over w=10 positions is recovered with
    KL ~ 0 by fit_additive — no int64 packing anywhere."""
    from fda_exp.distill_lm import fit_additive, kl_model_student
    V, w, D = 8, 10, 3000
    rng = np.random.default_rng(2)
    E = rng.normal(0, 0.7, (w, V, V)); E[:, :, 0] = -3.0
    C = rng.integers(0, V, (D, w)).astype(np.int64)               # slot 0 allowed in contexts

    def planted(Cx):
        lg = sum(E[p][Cx[:, p]] for p in range(w))
        P = np.exp(lg - lg.max(1, keepdims=True))
        return P / P.sum(1, keepdims=True)
    n = np.ones(D, dtype=np.int64)
    tr = np.arange(D) < int(0.85 * D)
    model = fit_additive(C[tr], n[tr], planted(C[tr]).astype(np.float32),
                         C[~tr], n[~tr], planted(C[~tr]).astype(np.float32),
                         V, w, device="cpu", steps=2500)
    assert kl_model_student(model, C[~tr], planted(C[~tr])) < 0.05


def test_fit_staged_recovers_planted_additive():
    """Staged backoff fit reaches the same planted additive model, one position at a time."""
    from fda_exp.distill_lm import fit_staged, kl_model_student
    V, w, D = 8, 6, 3000
    rng = np.random.default_rng(3)
    E = rng.normal(0, 0.7, (w, V, V)); E[:, :, 0] = -3.0
    C = rng.integers(0, V, (D, w)).astype(np.int64)

    def planted(Cx):
        lg = sum(E[p][Cx[:, p]] for p in range(w))
        P = np.exp(lg - lg.max(1, keepdims=True))
        return P / P.sum(1, keepdims=True)
    n = np.ones(D, dtype=np.int64)
    tr = np.arange(D) < int(0.85 * D)
    model = fit_staged(C[tr], n[tr], planted(C[tr]).astype(np.float32),
                       C[~tr], n[~tr], planted(C[~tr]).astype(np.float32),
                       V, w, device="cpu", steps=1500)
    assert kl_model_student(model, C[~tr], planted(C[~tr])) < 0.1


def test_fit_hybrid_captures_suffix_interaction():
    """Planted = additive + a strong deg-2 character on the last two positions: the hybrid's
    suffix-window search stage must capture what the additive span cannot."""
    from fda_exp.distill_lm import fit_hybrid, fit_staged, kl_model_student
    V, w, D = 8, 6, 5000
    rng = np.random.default_rng(4)
    E = rng.normal(0, 0.4, (w, V, V))
    C = rng.integers(0, V, (D, w)).astype(np.int64)
    H = hadamard_basis(V)
    theta = rng.normal(0, 1.5, V)

    def planted(Cx):
        lg = sum(E[p][Cx[:, p]] for p in range(w))
        lg = lg + np.outer(H[3, Cx[:, -1]] * H[5, Cx[:, -2]], theta)   # deg-2 on last two positions
        P = np.exp(lg - lg.max(1, keepdims=True))
        return P / P.sum(1, keepdims=True)
    n = np.ones(D, dtype=np.int64)
    tr = np.arange(D) < int(0.85 * D)
    args = (C[tr], n[tr], planted(C[tr]).astype(np.float32),
            C[~tr], n[~tr], planted(C[~tr]).astype(np.float32), V, w)
    base = fit_staged(*args, device="cpu", steps=1200)
    hyb = fit_hybrid(*args, device="cpu", sub_w=4, top_k=100, max_width=1500, steps=1500)
    kl_base = kl_model_student(base, C[~tr], planted(C[~tr]))
    kl_hyb = kl_model_student(hyb, C[~tr], planted(C[~tr]))
    assert kl_hyb < kl_base - 0.1, (kl_base, kl_hyb)              # interaction captured
    assert kl_hyb < 0.15, kl_hyb


class _PositionLM:
    """Raw output at position i puts all mass on token id i+1 (the AR-init convention Dream uses:
    output i predicts position i+1).  Pins _pred_logits' slice math."""
    def __call__(self, input_ids=None, **kw):
        import torch
        B, L = input_ids.shape
        logits = torch.zeros(B, L, 60)
        for i in range(L):
            logits[:, i, min(i + 1, 59)] = 10.0
        return type("Out", (), {"logits": logits})()


def test_pred_logits_shift_convention():
    """_pred_logits(model, ids, lo, hi) must return predictions FOR positions lo..hi-1, i.e. raw
    outputs lo-1..hi-2.  With _PositionLM, the argmax at returned index j must equal lo+j."""
    import torch

    from fda_exp.distill_data import _pred_logits
    ids = torch.zeros((2, 9), dtype=torch.int64)
    lg = _pred_logits(_PositionLM(), ids, 3, 7)                   # predictions for positions 3..6
    assert lg.shape[1] == 4
    assert lg.argmax(-1).tolist() == [[3, 4, 5, 6]] * 2
    last = _pred_logits(_PositionLM(), ids, 8, 9)                 # final-position (appended mask) path
    assert last.argmax(-1).tolist() == [[8], [8]]


def _planted_P(C, V, w, codes, theta, const):
    """P(x) = softmax_t(const_t + sum_i theta[t,i] * chi_code_i(x)) — exactly representable
    by the student head over the planted characters."""
    chi = _char_columns(C, codes, V, hadamard_basis(V), w)        # (m, K)
    logits = const[None, :] + chi @ theta.T                       # (m, V)
    P = np.exp(logits - logits.max(1, keepdims=True))
    return P / P.sum(1, keepdims=True)


def test_planted_model_recovered_end_to_end():
    """Headline E2E: generate fibered contexts exactly like the real pipeline, label with a
    planted sparse low-degree model, and require the search to recover every planted character
    and the student to match the model (KL ~ 0) on fiber-disjoint validation contexts."""
    from fda_exp.distill_data import fiber_disjoint_split, soft_collapse
    from fda_exp.distill_lm import fit_distill_lm, kl_model_student
    V, w, k, M, R = 8, 4, 2, 400, 12
    codes = np.array([5, 3 * V ** 2, 5 + 3 * V ** 2], dtype=np.int64)   # token-degree 1,1,2
    rng = np.random.default_rng(1)
    theta = rng.normal(0, 1.2, (V, len(codes)))
    theta[0] = 0.0
    const = np.zeros(V)
    const[0] = -8.0                                               # OTHER slot ~ empty

    S = rng.integers(1, V, (M, k)).astype(np.int64)               # fixed strings
    g = rng.integers(1, V, (M * R, w - k)).astype(np.int64)       # uniform "fill-in" stub
    C = np.hstack([g, np.repeat(S, R, axis=0)])
    fiber = np.repeat(np.arange(M), R)

    tr, va = fiber_disjoint_split(fiber, val_frac=0.2, seed=0)
    Cd_tr, n_tr, _, _ = soft_collapse(C[tr])
    Cd_va, n_va, _, _ = soft_collapse(C[va])
    P_tr = _planted_P(Cd_tr, V, w, codes, theta, const)
    P_va = _planted_P(Cd_va, V, w, codes, theta, const)

    model = fit_distill_lm(Cd_tr, n_tr, P_tr, Cd_va, n_va, P_va, V, w,
                           max_width=1500, top_classes=V, top_k=50, device="cpu")
    got = set(int(c) for c in model["codes"])
    assert all(int(c) in got for c in codes), (sorted(got)[:10], codes)
    assert kl_model_student(model, Cd_tr, P_tr) < 0.05
    assert kl_model_student(model, Cd_va, P_va) < 0.15
