"""GL-as-a-language-model: an ACTUAL next-token predictor from the in-distribution CSAMP search,
GPU-accelerated via a ONE-SHOT multiclass search (`fast_gl`).

The model IS the CSAMP-recovered spectrum -- no separately-fit head.  ONE top-K multiclass Walsh CSAMP
search (`fast_gl.multiclass_search`, float32, MPS/CUDA) recovers the characters predictive of the whole
next-token distribution; their exact dataset coefficients `f̂_t(code) = E_D[f_t*chi]` (one GEMM,
`fast_gl.coeffs_all`) reconstruct `g_t(x) = f̂_t(∅) + Σ f̂_t(code)*chi(x) ≈ 2*P(y=t|x) - 1`, so
`P(t|x) ≈ (g_t+1)/2`, normalized over `t`.  We report held-out **cross-entropy / perplexity** on a
STORY-DISJOINT split against a unigram floor and a full-context n-gram reference, and expose
`predict_next(text)` / `generate(text)` for a text demo.

    from fda_exp.bpe_lm import run_bpe_lm_demo
    run_bpe_lm_demo(vocab_size=512, window=3)      # runs on the GPU (MPS locally / CUDA on Modal)
"""

from __future__ import annotations

import numpy as np
import torch

from . import bpe
from .fast_gl import char_matrix, coeffs_all, multiclass_search
from .gl_torch import get_device
from .hf_data import collapse_contexts, load_texts
from .qary_gl import _encode_qary, degree_of_codes


def _bpe_split(window, vocab_size, n_stories, max_pairs, seed, split, shards, bpe_train_stories,
               val_frac=0.1, test_frac=0.15):
    """3-way STORY-DISJOINT (context, next-token) split over a real byte-level BPE: each story goes
    wholly to train / val / test, then windows within.  The val split is story-disjoint from train so
    regularization tuned on it reflects the (also story-disjoint) test.  Returns
    (Ctr, ytr, Cval, yval, Cte, yte, V, w, merges)."""
    from numpy.lib.stride_tricks import sliding_window_view
    k = int(vocab_size).bit_length() - 1
    if vocab_size != (1 << k):
        raise ValueError(f"vocab_size must be a power of 2, got {vocab_size}")
    if window * k > 62:
        raise ValueError(f"window*log2(vocab_size)={window * k} exceeds int64 packing limit 62")
    merges = bpe.train_or_load(vocab_size, bpe_train_stories)
    texts = load_texts(n_stories, split=split, shards=shards)
    rng = np.random.default_rng(seed)
    buckets = ([], [], [])                                          # 0=train, 1=val, 2=test  (C, y) each
    trbuf = [[], []]
    tot = 0
    for ids in bpe.encode_iter(texts, merges):
        if len(ids) <= window:
            continue
        wins = sliding_window_view(np.asarray(ids, np.int64), window + 1)
        r = rng.random()
        which = 2 if r < test_frac else (1 if r < test_frac + val_frac else 0)
        if which == 0:
            trbuf[0].append(wins[:, :window]); trbuf[1].append(wins[:, window]); tot += len(wins)
        else:
            buckets[which].append((wins[:, :window], wins[:, window]))
        if tot >= max_pairs:
            break
    Ctr = np.concatenate(trbuf[0])[:max_pairs].copy(); ytr = np.concatenate(trbuf[1])[:max_pairs].copy()
    Cval = np.concatenate([c for c, _ in buckets[1]]); yval = np.concatenate([y for _, y in buckets[1]])
    Cte = np.concatenate([c for c, _ in buckets[2]]); yte = np.concatenate([y for _, y in buckets[2]])
    print(f"[bpe_lm] V={vocab_size} w={window}: story-disjoint train={len(Ctr)} val={len(Cval)} "
          f"test={len(Cte)} (merges={len(merges)})", flush=True)
    return Ctr, ytr, Cval, yval, Cte, yte, vocab_size, window, merges


def _char_np(Cd, codes, V, w):
    """(D, K) numpy Walsh characters via `_char_columns` (Householder=Walsh at power-of-2 V)."""
    from .householder import hadamard_basis
    from .qary_gl import _char_columns
    if len(codes) == 0:
        return np.zeros((len(Cd), 0))
    return _char_columns(Cd, codes, V, hadamard_basis(V), w)


def _softmax_fit(X, counts, Xval, counts_val, device, steps=2500, lr=0.05,
                 l2s=(3e-4, 1e-3, 3e-3, 1e-2, 3e-2), patience=6):
    """Softmax regression W (K,V) minimizing weighted cross-entropy on the train contexts, with the L2
    strength + stopping point selected on the STORY-DISJOINT val (the fix for cross-story overfitting:
    a random train-context split can't see it).  Returns (W (K,V), chosen_l2)."""
    Xt = torch.tensor(X, dtype=torch.float32, device=device)
    Ct = torch.tensor(counts, dtype=torch.float32, device=device)
    Xv = torch.tensor(Xval, dtype=torch.float32, device=device)
    Cv = torch.tensor(counts_val, dtype=torch.float32, device=device)
    N, Nv = float(Ct.sum()), float(max(Cv.sum().item(), 1.0))
    best = (float("inf"), np.zeros((X.shape[1], counts.shape[1])), l2s[0])
    for l2 in l2s:
        W = torch.zeros(X.shape[1], counts.shape[1], device=device, requires_grad=True)
        opt = torch.optim.Adam([W], lr=lr)
        bl, bW, bad = float("inf"), None, 0
        for step in range(steps):
            opt.zero_grad()
            loss = -(Ct * torch.log_softmax(Xt @ W, dim=1)).sum() / N + l2 * (W * W).sum()
            loss.backward(); opt.step()
            if step % 15 == 0:
                with torch.no_grad():
                    vce = -(Cv * torch.log_softmax(Xv @ W, dim=1)).sum().item() / Nv
                if vce < bl - 1e-4:
                    bl, bW, bad = vce, W.detach().clone(), 0
                else:
                    bad += 1
                    if bad > patience:
                        break
        if bl < best[0]:
            best = (bl, bW.cpu().numpy(), l2)
    return best[1], best[2]


def fit_bpe_lm(Ctr, ytr, Cval, yval, V, w, merges=None, max_width=6000, device=None, top_classes=48,
               top_k=2000, seed=0):
    """The CSAMP multiclass top-K search (`fast_gl.multiclass_search`) selects the shared heavy
    characters; a GPU softmax regression fits their per-token weights, with L2 tuned on the
    STORY-DISJOINT val (Cval,yval).  Returns a model dict for `_proba` etc."""
    device = device or get_device()
    Cd, counts, n_ctx, m = collapse_contexts(Ctr, ytr, V)
    Cdv, counts_v, _, _ = collapse_contexts(Cval, yval, V)         # story-disjoint val contexts
    idx = _encode_qary(Cd, V)
    n_bits = w * (int(V).bit_length() - 1)
    topT = np.argsort(-counts.sum(0))[:top_classes]                # search over the most frequent tokens
    A = (2.0 * counts[:, topT] - n_ctx[:, None]).astype(np.float32)   # (D, T)
    codes, widths = multiclass_search(idx, A, n_bits, max_width=max_width, device=device)
    if len(codes) > top_k:                                         # keep the top_k heaviest characters
        coeffs, _ = coeffs_all(counts, n_ctx, Cd, codes, V, w, m)  # (V, K)
        codes = codes[np.argsort(-(coeffs ** 2).sum(0))[:top_k]]
    degs = degree_of_codes(codes, V, w) if len(codes) else np.zeros(0, int)

    def feat(Cx, sel):
        return np.hstack([np.ones((len(Cx), 1)), _char_np(Cx, codes[sel], V, w)]).astype(np.float32)
    d2 = degs <= 2
    W2, l2_2 = _softmax_fit(feat(Cd, d2), counts, feat(Cdv, d2), counts_v, device)
    all_ = np.ones(len(codes), bool)
    W3, l2_3 = _softmax_fit(feat(Cd, all_), counts, feat(Cdv, all_), counts_v, device)
    base = np.bincount(ytr, minlength=V).astype(np.float64); base = (base + 1) / (base.sum() + V)
    dh = np.bincount(degs, minlength=w + 1).tolist() if len(codes) else [0] * (w + 1)
    print(f"[bpe_lm] CSAMP multiclass on {len(Cd)} distinct ctx -> {len(codes)} chars (degree {dh}); "
          f"softmax L2 (val-tuned) deg<=2={l2_2:.0e} deg<=3={l2_3:.0e}", flush=True)
    return dict(V=V, w=w, n_bits=n_bits, device=device, codes=codes, degs=degs,
                W2=W2, W3=W3, base=base, merges=merges, dh=dh)


def _proba(model, C, deg3=True, mix=None):
    """Full (n, V) next-token distribution = softmax of the fitted logits over the recovered characters,
    interpolated with the unigram (BACKOFF: `mix`) so novel story-disjoint contexts can't blow up.
    deg3=False uses only the degree<=2 characters."""
    V, w = model["V"], model["w"]
    codes, degs = model["codes"], model["degs"]
    sel = np.ones(len(codes), bool) if deg3 else (degs <= 2)
    W = model["W3"] if deg3 else model["W2"]
    C = np.atleast_2d(np.asarray(C, dtype=np.int64))
    X = np.hstack([np.ones((len(C), 1)), _char_np(C, codes[sel], V, w)]).astype(np.float32)
    logits = X @ W                                                 # (n, V)
    P = np.exp(logits - logits.max(1, keepdims=True))
    return P / P.sum(1, keepdims=True)


def ce_perplexity(model, C, y, deg3=True):
    """Held-out cross-entropy (nats/bits), perplexity, and top-1 for the CSAMP-reconstruction LM."""
    P = _proba(model, C, deg3)
    pt = np.clip(P[np.arange(len(y)), y], 1e-12, 1.0)
    ce = float(-np.log(pt).mean())
    return dict(ce_nats=ce, ce_bits=ce / np.log(2), ppl=float(np.exp(ce)), top1=float((P.argmax(1) == y).mean()))


def unigram_perplexity(model, y):
    pt = np.clip(model["base"][y], 1e-12, 1.0)
    ce = float(-np.log(pt).mean())
    return dict(ce_nats=ce, ppl=float(np.exp(ce)))


def ngram_perplexity(Ctr, ytr, Cte, yte, V, alpha=1.0):
    """Full-context n-gram reference (the 'degree-infinity' memorization model): empirical
    `P(next|context)` from train counts, add-alpha backed off to the unigram.  Vectorized (searchsorted)."""
    Cd, counts, n_ctx, m = collapse_contexts(Ctr, ytr, V)
    base = np.bincount(ytr, minlength=V).astype(np.float64); base = (base + 1) / (base.sum() + V)
    kd = _encode_qary(Cd, V); order = np.argsort(kd)
    kd_s, counts_s, n_s = kd[order], counts[order], n_ctx[order]
    kte = _encode_qary(np.asarray(Cte), V)
    pos = np.clip(np.searchsorted(kd_s, kte), 0, len(kd_s) - 1)
    hit = kd_s[pos] == kte
    p = base[yte].astype(np.float64)
    gi = pos[hit]
    p[hit] = (counts_s[gi, yte[hit]] + alpha * base[yte[hit]]) / (n_s[gi] + alpha)
    ce = float(-np.log(np.clip(p, 1e-12, 1.0)).mean())
    return dict(ce_nats=ce, ppl=float(np.exp(ce)))


def predict_next(model, text, topk=10):
    """Text -> top-k predicted next BPE tokens with probabilities (the in-distribution CSAMP LM)."""
    ids = bpe.encode(text, model["merges"]); w = model["w"]
    ctx = (list(ids[-w:]) if len(ids) >= w else [0] * (w - len(ids)) + list(ids))
    P = _proba(model, np.array(ctx, dtype=np.int64)[None, :], deg3=True)[0]
    return [(bpe.decode([int(t)], model["merges"]), float(P[t])) for t in np.argsort(-P)[:topk]]


def generate(model, prompt, n_tokens=20, temperature=0.0, seed=0):
    """Greedy (temperature=0) or sampled continuation from a prompt (demo generation)."""
    rng = np.random.default_rng(seed)
    ids = list(bpe.encode(prompt, model["merges"])); w = model["w"]; out = []
    for _ in range(n_tokens):
        ctx = (ids[-w:] if len(ids) >= w else [0] * (w - len(ids)) + ids)
        P = _proba(model, np.array(ctx, dtype=np.int64)[None, :], deg3=True)[0]
        if temperature <= 0:
            nxt = int(P.argmax())
        else:
            logp = np.log(np.clip(P, 1e-12, 1.0)) / temperature
            p = np.exp(logp - logp.max()); p /= p.sum(); nxt = int(rng.choice(len(p), p=p))
        ids.append(nxt); out.append(nxt)
    return prompt + " |>> " + bpe.decode(out, model["merges"])


def run_bpe_lm_demo(vocab_size=512, window=3, n_stories=60000, max_pairs=1_500_000, split="valid",
                    shards=None, max_width=6000, device=None, seed=0,
                    prompts=("once upon a time", "the little girl", "she was very", "they lived in a")):
    """Fit the GPU CSAMP LM, report held-out CE/perplexity vs baselines, demo text->next-token."""
    import time
    t0 = time.time()
    Ctr, ytr, Cval, yval, Cte, yte, V, w, merges = _bpe_split(window, vocab_size, n_stories, max_pairs,
                                                              seed, split, shards, 20000)
    model = fit_bpe_lm(Ctr, ytr, Cval, yval, V, w, merges=merges, max_width=max_width, device=device, seed=seed)
    print(f"[bpe_lm] fit in {time.time() - t0:.1f}s on device={model['device']}", flush=True)
    uni, ng = unigram_perplexity(model, yte), ngram_perplexity(Ctr, ytr, Cte, yte, V)
    m2, m3 = ce_perplexity(model, Cte, yte, deg3=False), ce_perplexity(model, Cte, yte, deg3=True)
    print(f"\n===== HELD-OUT next-token metrics (story-disjoint, V={V}, w={w}, test={len(yte)}) =====", flush=True)
    print(f"  unigram (base rate)             CE={uni['ce_nats']:.3f} nats  PPL={uni['ppl']:.1f}", flush=True)
    print(f"  CSAMP LM  degree<=2             CE={m2['ce_nats']:.3f} nats  PPL={m2['ppl']:.1f}  top1={m2['top1']:.3f}", flush=True)
    print(f"  CSAMP LM  degree<=3 (all)       CE={m3['ce_nats']:.3f} nats  PPL={m3['ppl']:.1f}  top1={m3['top1']:.3f}", flush=True)
    print(f"  full n-gram lookup (deg-inf)    CE={ng['ce_nats']:.3f} nats  PPL={ng['ppl']:.1f}   <- memorization ref", flush=True)
    print(f"  >>> degree-3 delta: CE {m2['ce_nats'] - m3['ce_nats']:+.4f} nats, "
          f"PPL {m2['ppl'] - m3['ppl']:+.2f}, top1 {m3['top1'] - m2['top1']:+.4f}", flush=True)
    print("\n===== DEMO: text -> next-token distribution (degree<=3 CSAMP LM) =====", flush=True)
    for p in prompts:
        pretty = "  ".join(f"{tok!r}:{pr:.2f}" for tok, pr in predict_next(model, p, topk=6))
        print(f"  {p!r:28s} -> {pretty}", flush=True)
    print("\n===== greedy generations =====", flush=True)
    for p in prompts[:2]:
        print(f"  {generate(model, p, n_tokens=16)!r}", flush=True)
    return dict(model=model, uni=uni, ngram=ng, deg2=m2, deg3=m3)
