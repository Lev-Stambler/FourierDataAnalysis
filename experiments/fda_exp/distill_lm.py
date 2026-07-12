"""Fit + evaluate the CSAMP-recovered student that compresses a diffusion LM (see distill_data).

Mirrors `bpe_lm.fit_bpe_lm` with SOFT labels: the search/coeff/fit stack is already generic in the
per-class weights, so with soft counts  soft = n_ctx[:,None] * P  (P = the model's next-token
distribution per distinct context) `multiclass_search` (A = n*(2P-1)), `coeffs_all` and
`_softmax_fit` (weighted CE = KL distillation) are reused VERBATIM.  The returned model dict has the
same shape as `fit_bpe_lm`'s, so `bpe_lm._proba` / `ce_perplexity` apply unchanged.
"""

from __future__ import annotations

import numpy as np

from .bpe_lm import _char_np, _proba, _softmax_fit, ce_perplexity
from .fast_gl import coeffs_all, multiclass_search
from .gl_torch import get_device
from .qary_gl import _encode_qary, degree_of_codes


def deg1_codes(V, w):
    """The complete degree-1 character basis: every code with one nonzero base-V digit (511 per
    position).  Spans all per-position bucket structure (n-gram-style signal), which the top-W-mass
    search can under-allocate."""
    a = np.arange(1, V, dtype=np.int64)
    return (a[None, :] * (V ** np.arange(w, dtype=np.int64))[:, None]).ravel()


def fit_distill_lm(Cd_tr, n_tr, P_tr, Cd_val, n_val, P_val, V, w, max_width=6000, device=None,
                   top_classes=64, top_k=2000, l2s=None, steps=2500, add_deg1=True):
    """CSAMP multiclass top-K search on soft labels + KL-distilled softmax head (L2 tuned on the
    fiber-disjoint val).  add_deg1 unions the full degree-1 basis into the searched characters.
    Returns a model dict compatible with `bpe_lm._proba`."""
    device = device or get_device()
    soft = n_tr[:, None] * P_tr.astype(np.float64)
    soft_v = n_val[:, None] * P_val.astype(np.float64)
    m = int(n_tr.sum())
    idx = _encode_qary(Cd_tr, V)
    n_bits = w * (int(V).bit_length() - 1)
    topT = np.argsort(-soft.sum(0))[:top_classes]                 # heaviest slots by soft mass
    A = (2.0 * soft[:, topT] - n_tr[:, None]).astype(np.float32)  # = n*(2P-1) on the searched slots
    codes, widths = multiclass_search(idx, A, n_bits, max_width=max_width, device=device)
    if len(codes) > top_k:                                        # keep the top_k heaviest characters
        coeffs, _ = coeffs_all(soft, n_tr, Cd_tr, codes, V, w, m)
        codes = codes[np.argsort(-(coeffs ** 2).sum(0))[:top_k]]
    if add_deg1:
        codes = np.unique(np.concatenate([codes, deg1_codes(V, w)]))
    degs = degree_of_codes(codes, V, w) if len(codes) else np.zeros(0, int)

    def feat(Cx, sel):
        return np.hstack([np.ones((len(Cx), 1)), _char_np(Cx, codes[sel], V, w)]).astype(np.float32)
    l2s = tuple(l2s) if l2s is not None else (3e-4, 1e-3, 3e-3, 1e-2, 3e-2)
    d2 = degs <= 2
    W2, l2_2 = _softmax_fit(feat(Cd_tr, d2), soft, feat(Cd_val, d2), soft_v, device,
                            steps=steps, l2s=l2s, l2_intercept=False)
    all_ = np.ones(len(codes), bool)
    W3, l2_3 = _softmax_fit(feat(Cd_tr, all_), soft, feat(Cd_val, all_), soft_v, device,
                            steps=steps, l2s=l2s, l2_intercept=False)
    base = (soft.sum(0) + 1.0) / (m + V)                          # soft unigram
    dh = np.bincount(degs, minlength=w + 1).tolist() if len(codes) else [0] * (w + 1)
    print(f"[distill] CSAMP soft multiclass on {len(Cd_tr)} distinct ctx -> {len(codes)} chars "
          f"(degree {dh}); L2 (fiber-disjoint val) deg<=2={l2_2:.0e} all={l2_3:.0e}", flush=True)
    return dict(V=V, w=w, n_bits=n_bits, device=device, codes=codes, degs=degs,
                W2=W2, W3=W3, base=base, merges=None, dh=dh)


def kl_model_student(model, C, P_ref, deg3=True):
    """Mean KL(model || student) in nats over contexts C (slot ids)."""
    Q = np.clip(_proba(model, C, deg3), 1e-12, 1.0)
    P = np.clip(np.asarray(P_ref, dtype=np.float64), 1e-12, 1.0)
    return float((P * (np.log(P) - np.log(Q))).sum(1).mean())


def eval_distill(model, Ceval, yeval, P_eval, teacher_params=7.6e9):
    """Student vs the compressed model on REAL long-context windows: KL, top-1 agreement, CE/ppl vs
    the actual next tokens, the model's own CE (floor), unigram CE (ceiling), compression ratio."""
    Q = _proba(model, Ceval, deg3=True)
    P = np.asarray(P_eval, dtype=np.float64)
    y = np.asarray(yeval, dtype=np.int64)
    ce_model = float(-np.log(np.clip(P[np.arange(len(y)), y], 1e-12, 1)).mean())
    base = np.clip(model["base"], 1e-12, 1)
    n_params = model["W3"].size + model["base"].size
    real = P.argmax(1) != 0                                       # rows where the model's top pick is
    return dict(                                                  # a real token, not the OTHER lump
        kl=kl_model_student(model, Ceval, P_eval, deg3=True),
        kl_deg2=kl_model_student(model, Ceval, P_eval, deg3=False),
        top1_agree=float((Q.argmax(1) == P.argmax(1)).mean()),
        top1_agree_real=float((Q.argmax(1) == P.argmax(1))[real].mean()) if real.any() else float("nan"),
        frac_real_top1=float(real.mean()),
        student=ce_perplexity(model, Ceval, y, deg3=True),
        model_ce_nats=ce_model, model_ppl=float(np.exp(ce_model)),
        unigram_ce_nats=float(-np.log(base[y]).mean()),
        other_mass=float(P[:, 0].mean()),
        student_params=int(n_params), compression=float(teacher_params / n_params),
    )
