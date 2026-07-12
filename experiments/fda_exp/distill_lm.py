"""Fit + evaluate the CSAMP-recovered student that compresses a diffusion LM (see distill_data).

Mirrors `bpe_lm.fit_bpe_lm` with SOFT labels: the search/coeff/fit stack is already generic in the
per-class weights, so with soft counts  soft = n_ctx[:,None] * P  (P = the model's next-token
distribution per distinct context) `multiclass_search` (A = n*(2P-1)), `coeffs_all` and
`_softmax_fit` (weighted CE = KL distillation) are reused VERBATIM.  The returned model dict has the
same shape as `fit_bpe_lm`'s, so `bpe_lm._proba` / `ce_perplexity` apply unchanged.
"""

from __future__ import annotations

import numpy as np

from .bpe_lm import _char_np, _proba, _softmax_fit
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
                   top_classes=64, top_k=2000, steps=3000, add_deg1=True):
    """CSAMP multiclass top-K search on soft labels + KL-distilled softmax head (bias starts at the
    soft unigram, deg<=3 warm-started from deg<=2, early-stopped on the fiber-disjoint val -- no L2,
    mirroring fit_bpe_lm).  add_deg1 unions the full degree-1 basis into the searched characters.
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
    order = np.argsort(degs > 2, kind="stable")                   # deg<=2 characters first, deg>=3 after
    codes, degs = codes[order], degs[order]
    n2 = int((degs <= 2).sum())
    base = (soft.sum(0) + 1.0) / (m + V)                          # soft unigram

    def feat(Cx, ncodes):
        return np.hstack([np.ones((len(Cx), 1)), _char_np(Cx, codes[:ncodes], V, w)]).astype(np.float32)
    W2 = _softmax_fit(feat(Cd_tr, n2), soft, feat(Cd_val, n2), soft_v, base, device, steps=steps)
    if n2 < len(codes):                                           # warm-start: deg<=3 can't lose to deg<=2
        Winit = np.zeros((1 + len(codes), int(V)), np.float32)
        Winit[:W2.shape[0]] = W2
        W3 = _softmax_fit(feat(Cd_tr, len(codes)), soft, feat(Cd_val, len(codes)), soft_v, base,
                          device, W_init=Winit, steps=steps)
    else:
        W3 = W2
    dh = np.bincount(degs, minlength=w + 1).tolist() if len(codes) else [0] * (w + 1)
    print(f"[distill] CSAMP soft multiclass on {len(Cd_tr)} distinct ctx -> {len(codes)} chars "
          f"(degree {dh}); deg<=2 fit + all warm-started (no L2)", flush=True)
    return dict(V=V, w=w, n_bits=n_bits, device=device, codes=codes, degs=degs,
                W2=W2, W3=W3, base=base, merges=None, dh=dh)


def fit_additive(Cd_tr, n_tr, P_tr, Cd_val, n_val, P_val, V, w, device=None, steps=4000, lr=0.05,
                 patience=10, check=25, warmup=400):
    """Degree<=1 dataset-GL student for LONG windows (w*log2V > 62, where int64 code packing and the
    tree search do not apply): logits(x) = b + sum_p E[p, x_p].  Same span as the complete degree-1
    Walsh basis, fit in one-hot coordinates (which transfer better -- see the bucket-mean reference);
    soft-KL objective, bias starts at the log soft-unigram (floor), early stop on the fiber-disjoint
    val.  Returns a model dict with kind='additive' for `proba_any`."""
    import torch
    device = device or get_device()
    soft = torch.tensor(n_tr[:, None] * P_tr, dtype=torch.float32, device=device)
    soft_v = torch.tensor(n_val[:, None] * P_val, dtype=torch.float32, device=device)
    Ct = torch.tensor(np.asarray(Cd_tr, np.int64), device=device)
    Cv = torch.tensor(np.asarray(Cd_val, np.int64), device=device)
    N, Nv = float(soft.sum()), float(max(soft_v.sum().item(), 1.0))
    m = float(n_tr.sum())
    base = ((n_tr[:, None] * P_tr).sum(0) + 1.0) / (m + V)
    E = torch.zeros(w, V, V, device=device, requires_grad=True)
    b = torch.tensor(np.log(base), dtype=torch.float32, device=device, requires_grad=True)

    def logits(C):
        out = b.expand(len(C), V).clone()
        for p in range(C.shape[1]):
            out = out + E[p][C[:, p]]
        return out

    with torch.no_grad():
        best = -(soft_v * torch.log_softmax(logits(Cv), 1)).sum().item() / Nv     # unigram floor
    bE, bb, bad = E.detach().cpu().numpy().copy(), b.detach().cpu().numpy().copy(), 0
    opt = torch.optim.Adam([E, b], lr=lr)
    for step in range(steps):
        opt.zero_grad()
        loss = -(soft * torch.log_softmax(logits(Ct), 1)).sum() / N               # NO L2
        loss.backward(); opt.step()
        if step % check == 0:
            with torch.no_grad():
                vce = -(soft_v * torch.log_softmax(logits(Cv), 1)).sum().item() / Nv
            if vce < best - 1e-4:
                best, bE, bb, bad = vce, E.detach().cpu().numpy().copy(), b.detach().cpu().numpy().copy(), 0
            else:
                bad += 1
                if step >= warmup and bad > patience:
                    break
    print(f"[distill] additive deg<=1 fit on {len(Cd_tr)} distinct ctx (w={w}, V={V}); "
          f"val soft-CE {best:.3f}", flush=True)
    return dict(kind="additive", V=V, w=w, E=bE, b=bb, base=base)


def proba_any(model, C, deg3=True):
    """(n, V) student distribution for either model kind (CSAMP-spectrum head or additive)."""
    if model.get("kind") == "additive":
        C = np.atleast_2d(np.asarray(C, np.int64))
        logits = model["b"][None, :] + sum(model["E"][p][C[:, p]] for p in range(C.shape[1]))
        Q = np.exp(logits - logits.max(1, keepdims=True))
        return Q / Q.sum(1, keepdims=True)
    return _proba(model, C, deg3)


def kl_model_student(model, C, P_ref, deg3=True):
    """Mean KL(model || student) in nats over contexts C (slot ids)."""
    Q = np.clip(proba_any(model, C, deg3), 1e-12, 1.0)
    P = np.clip(np.asarray(P_ref, dtype=np.float64), 1e-12, 1.0)
    return float((P * (np.log(P) - np.log(Q))).sum(1).mean())


def eval_distill(model, Ceval, yeval, P_eval, teacher_params=7.6e9):
    """Student vs the compressed model on REAL long-context windows: KL, top-1 agreement, CE/ppl vs
    the actual next tokens, the model's own CE (floor), unigram CE (ceiling), compression ratio."""
    Q = proba_any(model, Ceval, deg3=True)
    P = np.asarray(P_eval, dtype=np.float64)
    y = np.asarray(yeval, dtype=np.int64)
    ce_model = float(-np.log(np.clip(P[np.arange(len(y)), y], 1e-12, 1)).mean())
    base = np.clip(model["base"], 1e-12, 1)
    n_params = model["E"].size + model["b"].size if model.get("kind") == "additive" else \
        model["W3"].size + model["base"].size
    qt = np.clip(Q[np.arange(len(y)), y], 1e-12, 1.0)
    ce_student = float(-np.log(qt).mean())
    real = P.argmax(1) != 0                                       # rows where the model's top pick is
    return dict(                                                  # a real token, not the OTHER lump
        kl=kl_model_student(model, Ceval, P_eval, deg3=True),
        kl_deg2=kl_model_student(model, Ceval, P_eval, deg3=False),
        top1_agree=float((Q.argmax(1) == P.argmax(1)).mean()),
        top1_agree_real=float((Q.argmax(1) == P.argmax(1))[real].mean()) if real.any() else float("nan"),
        frac_real_top1=float(real.mean()),
        student=dict(ce_nats=ce_student, ppl=float(np.exp(ce_student)),
                     top1=float((Q.argmax(1) == y).mean())),
        model_ce_nats=ce_model, model_ppl=float(np.exp(ce_model)),
        unigram_ce_nats=float(-np.log(base[y]).mean()),
        other_mass=float(P[:, 0].mean()),
        student_params=int(n_params), compression=float(teacher_params / n_params),
    )
