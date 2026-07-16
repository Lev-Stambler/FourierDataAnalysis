"""CSAMP dataset-GL for the edu classifier (v2) -- the tree, over an LM's law.

v1 (edu_gl.py) enumerates deg-1/2/3 exactly on plain FineWeb windows; its
global noise floor hides higher-degree structure (deg-3 at 1M rows: +0.002).
Dataset-GL attacks that with CONDITIONING: pairs of independent model
continuations (x1, x2) of a shared prefix z estimate the prefix-conditional
coefficient energy psi(S) = E_z[e_z(S)^2], which concentrates mass that the
global coefficient spreads thin.

Sampler: Qwen3.5-0.8B generates W_Q-token windows after a real FineWeb prefix
(pure sampling: temperature 1, no top-k/p).  f = fineweb-edu-classifier score
of the DECODED window text, normalized to [-1, 1] (||f|| <= 1, uncentered --
the pure-GL contract, density term included).  Domain = the W_Q Qwen tokens,
encoded with sign-LSH codes built from Qwen's own input embeddings.

Recipes cribbed from experiments/canonical/pure_gl.py (pair_psi L20-44,
pure_gl_tree L47-92, gl_recon_eval L324-388) -- scalar f, self-contained.

Stages:  gen  (fiber prefixes + flat fills + per-level forks, all labeled)
         tree (hereditary GL on the pair score + calculated refit ladder)
"""

import json
import os

import numpy as np

import edu_gl as E

QWEN_ID = "Qwen/Qwen3.5-0.8B-Base"
FIBER_CORPUS = ("HuggingFaceFW/fineweb", "CC-MAIN-2023-50")   # disjoint dump: no
PRE_Q = 64            # real prefix tokens (Qwen tokenizer)   # v1 doc overlap
W_Q = 48              # generated window tokens = the function domain
DEPTH = 6             # tree depth in token blocks, newest-first
ROOT = f"{E.ROOT}/csamp"

image = (E.image.pip_install("accelerate>=1.2")
         .env({"PYTORCH_CUDA_ALLOC_CONF": "expandable_segments:True"})
         .add_local_python_source("edu_gl"))


def _fork_path(m, g, j):
    return f"{ROOT}/forks_m{m}_g{g}_L{j}.npz"


def _flat_path(m, r, tag):
    return f"{ROOT}/flat_{tag}_m{m}_r{r}.npz"


# ------------------------------------------------------------------- pair score

def pair_psi_scalar(bits, masks, f, gid, device=None, char_chunk=4096):
    """psi_hat(S) = mean over fibers z and continuation pairs i != j in z of
    [f_i chi_S(i) * f_j chi_S(j)] -- computed as sum_z (sum_i chi f)^2 minus
    the i = j diagonal, over n_pairs = sum_z c_z(c_z - 1).
    crib: canonical pure_gl.pair_psi L20-44, scalar f."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    bits_t = bits if torch.is_tensor(bits) else \
        torch.tensor(np.asarray(bits, np.uint8), device=device)
    dev = bits_t.device
    f_t = torch.as_tensor(np.asarray(f, np.float32), device=dev)
    _, gi = np.unique(np.asarray(gid), return_inverse=True)
    gid_t = torch.as_tensor(gi.astype(np.int64), device=dev)
    ng = int(gi.max()) + 1
    counts = np.bincount(gi)
    n_pairs = float((counts * (counts - 1)).sum())
    assert n_pairs > 0, "no fiber has two continuations -- nothing to pair"
    # NO centering: the PAPER'S estimator, density term and all.  (Per-fiber
    # centering was tried 2026-07-15 -- it changes the estimand and breaks
    # the Parseval subtree identity.)  psi is a SIEVE, not a ranking: keep
    # everything above the tau gate and let calculated deflation on a LARGE
    # refit table decide -- never rank by raw psi (recorded pure-GL lesson).
    diag = float((f_t.double() ** 2).sum())
    masks = np.asarray(masks, np.uint8)
    out = []
    for lo in range(0, len(masks), char_chunk):
        chi = E.mask_parity_features(bits_t, masks[lo:lo + char_chunk])
        chi *= f_t[:, None]
        S = torch.zeros((ng, chi.shape[1]), device=dev)
        S.index_add_(0, gid_t, chi)
        out.append(((S.double() ** 2).sum(0).cpu().numpy() - diag) / n_pairs)
    return np.concatenate(out) if out else np.zeros(0)


def gl_tree_scalar(levels, B, tau, max_width=512, device=None, progress=None):
    """Hereditary Dataset GL on the pure pair score.  levels[j] = (bits
    (m, (j+1)*B) newest-first, f (m,) with |f| <= 1, fiber gid (m,)).
    Extend live prefixes by single bits of token block j, keep children with
    psi_hat >= tau^2/4, carry the empty char + the top max_width forward.
    crib: canonical pure_gl.pure_gl_tree L47-92."""
    depth = len(levels)
    thresh = tau * tau / 4.0
    total = depth * B
    live = np.zeros((1, total), np.uint8)
    kept = {}
    per_level_kept = []
    for j, (bits, f, gid) in enumerate(levels):
        w = (j + 1) * B
        block_bits = np.arange(j * B, w)
        rows = []
        for parent in live:
            free = block_bits[parent[block_bits] == 0]
            if len(free):
                rep = np.repeat(parent[None, :], len(free), axis=0)
                rep[np.arange(len(free)), free] = 1
                rows.append(rep)
        if not rows:
            per_level_kept.append(0)
            continue
        children = np.unique(np.concatenate(rows), axis=0)
        psi = pair_psi_scalar(bits, children[:, :w], f, gid, device=device)
        if len(psi):
            q = np.percentile(psi, [50, 90, 99, 100])
            print(f"[tree] level {j}: {len(psi)} children, psi p50/p90/p99/max "
                  f"{q[0]:.2e}/{q[1]:.2e}/{q[2]:.2e}/{q[3]:.2e} "
                  f"(gate {thresh:.2e})", flush=True)
        heavy = np.flatnonzero(psi >= thresh)
        for i in heavy:
            key = children[i].tobytes()              # packed keys: tuple keys
            if psi[i] > kept.get(key, -1e30):        # are ~3.3KB each and OOM
                kept[key] = float(psi[i])            # the host at ~7M entries
        per_level_kept.append(int(len(heavy)))
        top = sorted(kept.items(), key=lambda kv: -kv[1])[:max_width]
        live = np.concatenate(
            [np.zeros((1, total), np.uint8),
             np.stack([np.frombuffer(k, np.uint8) for k, _ in top])]) \
            if top else np.zeros((1, total), np.uint8)
        if progress is not None:
            progress(j, depth, kept)
    if not kept:
        return dict(masks=np.zeros((0, total), np.uint8), psi=np.zeros(0),
                    per_level_kept=per_level_kept)
    order = sorted(kept.items(), key=lambda kv: -kv[1])
    return dict(masks=np.stack([np.frombuffer(k, np.uint8) for k, _ in order]),
                psi=np.array([v for _, v in order]),
                per_level_kept=per_level_kept)


# ------------------------------------------------------------------ generation

def _stream_prefixes(qtok, n, skip=0):
    """First PRE_Q Qwen tokens of qualifying docs from the fiber corpus."""
    from datasets import load_dataset
    tok_hf = (os.environ.get("HF_TOKEN") or os.environ.get("HF_HUB_TOKEN")
              or os.environ.get("HUGGING_FACE_HUB_TOKEN"))
    ds = load_dataset(FIBER_CORPUS[0], name=FIBER_CORPUS[1], split="train",
                      streaming=True, token=tok_hf)
    pre, seen = [], 0
    for row in ds:
        ids = qtok(row.get("text") or "", add_special_tokens=False)["input_ids"]
        if len(ids) >= PRE_Q:
            seen += 1
            if seen > skip:
                pre.append(np.asarray(ids[:PRE_Q], np.int32))
        if len(pre) >= n:
            return np.stack(pre)
    raise RuntimeError(f"stream ended at {len(pre)}/{n} prefixes")


def _gen_tails(model, prompts, k, nret, pad_id, temp=1.0, batch=64):
    """(P*nret, k) sampled continuations; PURE sampling (no top-k/p) so forks
    and flat fills share one law.  generate() groups returns per input row."""
    import torch
    outs = []
    for lo in range(0, len(prompts), batch):
        ids = torch.tensor(prompts[lo:lo + batch].astype(np.int64), device="cuda")
        with torch.inference_mode():
            out = model.generate(ids, do_sample=True, temperature=temp,
                                 top_k=0, top_p=1.0, max_new_tokens=k,
                                 num_return_sequences=nret,
                                 pad_token_id=pad_id)
        outs.append(out[:, ids.shape[1]:].cpu().numpy().astype(np.int32))
        del out, ids
    torch.cuda.empty_cache()
    return np.concatenate(outs)


def _score_texts(clf, ctok, texts, batch=256):
    import torch
    ys = []
    for lo in range(0, len(texts), batch):
        enc = ctok(texts[lo:lo + batch], truncation=True, max_length=512,
                   padding=True, return_tensors="pt").to("cuda")
        with torch.inference_mode():
            ys.append(clf(**enc).logits.squeeze(-1).float().cpu().numpy())
    return np.concatenate(ys).astype(np.float32)


@E.app.function(image=image, gpu="A10G", volumes={"/cache": E.vol},
                timeout=43200, memory=32768,
                secrets=E.WANDB_SECRET + E.HF_SECRET)
def gen_csamp(m_fibers: int = 1000, g: int = 12, r: int = 8,
              m_test: int = 300, depth: int = DEPTH, batch: int = 16,
              temp: float = 1.0):
    """Fiber prefixes + flat fills (train/test refit tables) + per-level forks,
    every window labeled by the classifier; also saves Qwen LSH/ctrl codes."""
    import torch
    from transformers import (AutoModelForImageTextToText,
                              AutoModelForSequenceClassification, AutoTokenizer)
    E.vol.reload()
    os.makedirs(ROOT, exist_ok=True)
    done = ([_flat_path(m_fibers, r, "tr"), _flat_path(m_test, r, "te")]
            + [_fork_path(m_fibers, g, j) for j in range(depth)])
    if all(os.path.exists(p) for p in done) and os.path.exists(f"{ROOT}/qcodes.npz"):
        print("gen: all tables exist", flush=True)
        return done
    qtok = AutoTokenizer.from_pretrained(QWEN_ID)
    model = AutoModelForImageTextToText.from_pretrained(
        QWEN_ID, dtype=torch.bfloat16, device_map="cuda",
        low_cpu_mem_usage=True).eval()
    ctok = AutoTokenizer.from_pretrained(E.MODEL_ID)
    clf = AutoModelForSequenceClassification.from_pretrained(
        E.MODEL_ID, dtype=torch.bfloat16).cuda().eval()
    if not os.path.exists(f"{ROOT}/qcodes.npz"):
        q = len(qtok)
        Eq = model.get_input_embeddings().weight.detach().float().cpu().numpy()[:q]
        lsh = E.build_lsh_codes(Eq, B=E.B_LSH)
        np.savez_compressed(f"{ROOT}/qcodes.npz", lsh=lsh,
                            ctrl=E.control_codes(q, lsh.shape[1]))
        E.vol.commit()
        print(f"[gen] qwen codes saved (q={q}, B_total={lsh.shape[1]})", flush=True)
    pad_id = qtok.eos_token_id or qtok.pad_token_id
    PRE = _stream_prefixes(qtok, m_fibers + m_test)
    PRE_tr, PRE_te = PRE[:m_fibers], PRE[m_fibers:]

    def label_wins(wins):
        return _score_texts(clf, ctok, qtok.batch_decode(
            wins.astype(np.int64), skip_special_tokens=True))

    def flat(pre, m, tag):
        p = _flat_path(m, r, tag)
        if not os.path.exists(p):
            wins = _gen_tails(model, pre, W_Q, r, pad_id, temp, batch)
            y = label_wins(wins)
            np.savez(p + ".tmp.npz", wins=wins, y=y,
                     gid=np.repeat(np.arange(m), r))
            os.replace(p + ".tmp.npz", p)
            E.vol.commit()
            print(f"[gen] flat {tag}: {len(wins)} windows, score mean "
                  f"{np.clip(y, 0, 5).mean():.3f}", flush=True)
        return np.load(p)

    ftr = flat(PRE_tr, m_fibers, "tr")
    flat(PRE_te, m_test, "te")
    spine = ftr["wins"].reshape(m_fibers, r, W_Q)[:, 0]
    for j in range(depth):
        p = _fork_path(m_fibers, g, j)
        if os.path.exists(p):
            continue
        k = j + 1
        prompts = np.concatenate([PRE_tr, spine[:, :W_Q - k]], axis=1)
        tails = _gen_tails(model, prompts, k, g, pad_id, temp, batch)
        head = np.repeat(spine[:, :W_Q - k], g, axis=0)
        gtoks = np.concatenate([head, tails], axis=1)
        y = label_wins(gtoks)
        np.savez(p + ".tmp.npz", gtoks=gtoks, y=y,
                 gid=np.repeat(np.arange(m_fibers), g))
        os.replace(p + ".tmp.npz", p)
        E.vol.commit()
        print(f"[gen] level {j}: {len(gtoks)} forks (resample last {k}), "
              f"score mean {np.clip(y, 0, 5).mean():.3f}", flush=True)
    return done


@E.app.function(image=image, gpu="A10G", volumes={"/cache": E.vol},
                timeout=43200, memory=32768, secrets=E.HF_SECRET)
def gen_flat(n_pre: int = 25000, r: int = 8, tag: str = "tr2",
             skip: int = 1500, batch: int = 32):
    """LARGE independent model-law refit table: fresh prefixes (skip past the
    fiber draws), r fills each -- deflating ALL sieved tree characters needs
    rows >> #chars; the 8k fiber-fill table was the real v2 bottleneck."""
    import torch
    from transformers import (AutoModelForImageTextToText,
                              AutoModelForSequenceClassification, AutoTokenizer)
    E.vol.reload()
    p = _flat_path(n_pre, r, tag)
    if os.path.exists(p):
        print(f"exists: {p}", flush=True)
        return p
    qtok = AutoTokenizer.from_pretrained(QWEN_ID)
    model = AutoModelForImageTextToText.from_pretrained(
        QWEN_ID, dtype=torch.bfloat16, device_map="cuda",
        low_cpu_mem_usage=True).eval()
    ctok = AutoTokenizer.from_pretrained(E.MODEL_ID)
    clf = AutoModelForSequenceClassification.from_pretrained(
        E.MODEL_ID, dtype=torch.bfloat16).cuda().eval()
    pad_id = qtok.eos_token_id or qtok.pad_token_id
    pre = _stream_prefixes(qtok, n_pre, skip=skip)
    sdir = f"{ROOT}/flat_shards_{tag}_m{n_pre}_r{r}"
    os.makedirs(sdir, exist_ok=True)
    chunk = 2000
    import time
    t0 = time.time()
    for lo in range(0, n_pre, chunk):
        sp = f"{sdir}/s{lo}.npz"
        if os.path.exists(sp):
            continue
        wins = _gen_tails(model, pre[lo:lo + chunk], W_Q, r, pad_id, 1.0, batch)
        y = _score_texts(clf, ctok, qtok.batch_decode(
            wins.astype(np.int64), skip_special_tokens=True))
        np.savez(sp + ".tmp.npz", wins=wins, y=y,
                 gid=np.repeat(np.arange(lo, lo + len(wins) // r), r))
        os.replace(sp + ".tmp.npz", sp)
        E.vol.commit()
        print(f"[genflat:{tag}] {min(lo + chunk, n_pre)}/{n_pre} prefixes "
              f"({time.time() - t0:.0f}s)", flush=True)
    parts = [np.load(f"{sdir}/s{lo}.npz") for lo in range(0, n_pre, chunk)]
    np.savez(p + ".tmp.npz",
             wins=np.concatenate([q["wins"] for q in parts]),
             y=np.concatenate([q["y"] for q in parts]),
             gid=np.concatenate([q["gid"] for q in parts]))
    os.replace(p + ".tmp.npz", p)
    E.vol.commit()
    print(f"saved {p}", flush=True)
    return p


# ------------------------------------------------------------------- tree + fit

@E.app.function(image=image, gpu="A10G", volumes={"/cache": E.vol},
                timeout=43200, memory=32768, secrets=E.WANDB_SECRET)
def gl_tree(encoding: str = "lsh", m_fibers: int = 1000, g: int = 12,
            r: int = 8, m_test: int = 300, depth: int = DEPTH,
            tau: float = 0.05, max_width: int = 512,
            ks: str = "1000,4000,16000,50000,100000",
            tr_spec: str = "", te_spec: str = ""):
    """Tree on the fork tables, then the calculated refit ladder: exact deg-1
    base on the flat train table, sequential deflation of the tree characters
    (psi order), score_metrics on the flat model-law test table.
    crib: canonical pure_gl.gl_recon_eval L324-388."""
    import torch
    E.vol.reload()
    codes = np.load(f"{ROOT}/qcodes.npz")[encoding]
    B = codes.shape[1]

    def load_flat(spec, dm, dr, dtag):                # "m:r:tag" override
        if spec:
            m_, r_, tag = spec.split(":")
            return np.load(_flat_path(int(m_), int(r_), tag))
        return np.load(_flat_path(dm, dr, dtag))

    dtr = load_flat(tr_spec, m_fibers, r, "tr")
    c0 = float(E.normalize_scores(dtr["y"]).mean())
    levels = []
    for j in range(depth):
        d = np.load(_fork_path(m_fibers, g, j))
        bits = codes[d["gtoks"][:, ::-1].astype(np.int64)] \
            .reshape(len(d["gtoks"]), -1)[:, :(j + 1) * B]
        # f normalized to [-1,1] ONLY (||f|| <= 1) -- no centering; the tree
        # is the paper's sieve and deflation downstream decides usefulness
        levels.append((bits, E.normalize_scores(d["y"]), d["gid"]))
    run = E._wandb_run(f"csamp-tree-{encoding}-d{depth}",
                       {"encoding": encoding, "m_fibers": m_fibers, "g": g,
                        "depth": depth, "tau": tau, "max_width": max_width})
    tree = gl_tree_scalar(
        levels, B, tau, max_width=max_width,
        progress=lambda j, D, kept: print(
            f"[tree:{encoding}] level {j + 1}/{D} kept {len(kept)}", flush=True))
    masks, psi = tree["masks"], tree["psi"]
    deg = (np.array([int(m.reshape(depth, B).any(1).sum()) for m in masks])
           if len(masks) else np.zeros(0, int))
    print(f"[tree:{encoding}] {len(masks)} chars, per-level kept "
          f"{tree['per_level_kept']}, deg hist "
          f"{np.bincount(deg, minlength=depth + 1).tolist()}", flush=True)
    np.savez_compressed(f"{ROOT}/tree_{encoding}_d{depth}.npz",
                        masks=masks, psi=psi, deg=deg)
    E.vol.commit()
    dte = load_flat(te_spec, m_test, r, "te")
    dev = "cuda" if torch.cuda.is_available() else "cpu"
    nb = depth * B                                   # refit on the tree's

    def tree_bits(wins):                             # domain only: 8k rows
        return codes[wins[:, ::-1].astype(np.int64)] \
            .reshape(len(wins), -1)[:, :nb]          # >> nb, well-posed deg-1

    # refit on the flat fills ONLY: fork windows share their spine within a
    # fiber, so adding them collapses the train law onto ~m near-duplicate
    # contexts and the deg-1 base overfits (tried 2026-07-15: 0.02 -> -0.03)
    bt = torch.tensor(tree_bits(dtr["wins"]), device=dev)
    bte = torch.tensor(tree_bits(dte["wins"]), device=dev)
    g_t = torch.tensor(E.normalize_scores(dtr["y"]) - c0, device=dev)
    W1, b1 = E.fit_deg1_exact(bt, g_t, device=dev)
    W1_t = torch.tensor(W1, device=dev)
    g_t -= (1.0 - 2.0 * bt.float()) @ W1_t + b1
    base_te = (1.0 - 2.0 * bte.float()) @ W1_t + (b1 + c0)
    summary = {"encoding": encoding, "n_chars": int(len(masks)),
               "per_level_kept": tree["per_level_kept"],
               "deg_hist": np.bincount(deg, minlength=depth + 1).tolist(),
               "deg1": {"test": E.score_metrics(base_te.cpu().numpy(), dte["y"])},
               "ladder": []}
    print(f"[tree:{encoding}] deg-1 base test R2 "
          f"{summary['deg1']['test']['r2']:.4f}", flush=True)
    kmax = min(len(masks), len(bt) // 4)             # deflate ALL sieved chars
    if kmax:                                         # that the rows support
        masks_k = masks[:kmax]
        C, _ = E.sequential_deflate(bt, g_t, None, device=dev, block=512,
                                    masks=masks_k)
        add_te = torch.zeros(len(bte), device=dev)
        lo = 0
        for k in sorted({int(x) for x in ks.split(",") if int(x) < kmax}
                        | {kmax}):
            for blo in range(lo, k, 4096):
                hi = min(blo + 4096, k)
                add_te += E.mask_parity_features(bte, masks_k[blo:hi]) \
                    @ torch.tensor(C[blo:hi], device=dev)
            lo = k
            entry = {"k": int(k),
                     "test": E.score_metrics((base_te + add_te).cpu().numpy(),
                                             dte["y"])}
            summary["ladder"].append(entry)
            print(f"[tree:{encoding}] === deg-1 + {k} chars: test R2 "
                  f"{entry['test']['r2']:.4f} sp {entry['test']['spearman']:.3f}",
                  flush=True)
            if run is not None:
                run.log({"chars": k, "test_r2": entry["test"]["r2"],
                         "test_spearman": entry["test"]["spearman"]})
    with open(f"{ROOT}/summary_tree_{encoding}_d{depth}.json", "w") as fh:
        json.dump(summary, fh, indent=1)
    E.vol.commit()
    if run is not None:
        run.summary.update({"n_chars": int(len(masks)),
                            "deg1_test_r2": summary["deg1"]["test"]["r2"]})
        run.finish()
    print(json.dumps(summary), flush=True)
    return summary


PLANTED = [  # (coefficient, bit indices in the newest-first depth*B domain)
    (0.30, (3,)),                    # deg-1, newest token block
    (0.25, (70, 141)),               # deg-2 across blocks 1,2
    (-0.20, (10, 75, 200)),          # deg-3 across blocks 0,1,2
    (0.20, (5, 140, 210, 350)),      # deg-4 across blocks 0,2,3,5
    (-0.15, (300, 401)),             # deg-2, older blocks 4,5
]


def _planted_f(wins, codes, depth, rng):
    """Known sparse spectrum over the newest depth token blocks of REAL model
    windows: y_raw = 2.5 + 2.5*(sum_c c*chi_S(bits)) + small noise, |f|<=1."""
    B = codes.shape[1]
    bits = codes[np.asarray(wins)[:, ::-1].astype(np.int64)] \
        .reshape(len(wins), -1)[:, :depth * B]
    f = np.zeros(len(wins), np.float32)
    for coef, sel in PLANTED:
        f += coef * (1.0 - 2.0 * np.bitwise_xor.reduce(bits[:, list(sel)], axis=1))
    f += 0.03 * rng.standard_normal(len(wins)).astype(np.float32)
    return 2.5 + 2.5 * f


@E.app.function(image=image, gpu="A100-40GB", volumes={"/cache": E.vol},
                timeout=21600, memory=49152, secrets=E.WANDB_SECRET)
def planted_cert(m_fibers: int = 1000, g: int = 12, r: int = 8,
                 depth: int = DEPTH, tau: float = 0.1, max_width: int = 512,
                 tr_spec: str = "25000:8:tr2", te_spec: str = "5000:2:te2"):
    """The PROVABLY-WORKS certificate: relabel the cached fork/flat tables
    (real Qwen windows, real fiber structure, real density) with a KNOWN
    sparse spectrum, run the identical tree + deflation, and check the
    theorem's promise end-to-end: every planted char recovered with
    psi ~ coef^2, refit test R2 ~ 1.  Zero generation cost."""
    import torch
    E.vol.reload()
    codes = np.load(f"{ROOT}/qcodes.npz")["lsh"]
    B = codes.shape[1]
    rng = np.random.default_rng(42)
    levels = []
    for j in range(depth):
        d = np.load(_fork_path(m_fibers, g, j))
        bits = codes[d["gtoks"][:, ::-1].astype(np.int64)] \
            .reshape(len(d["gtoks"]), -1)[:, :(j + 1) * B]
        y = _planted_f(d["gtoks"], codes, depth, rng)
        levels.append((bits, E.normalize_scores(y), d["gid"]))
    tree = gl_tree_scalar(levels, B, tau, max_width=max_width,
                          progress=lambda j, D, kept: print(
                              f"[cert] level {j + 1}/{D} kept {len(kept)}",
                              flush=True))
    masks, psi = tree["masks"], tree["psi"]
    found = {}
    for coef, sel in PLANTED:
        expect = np.zeros(depth * B, np.uint8)
        expect[list(sel)] = 1
        hit = np.flatnonzero((masks == expect).all(1)) if len(masks) else []
        found[str(sel)] = {"coef2": coef ** 2,
                           "psi": float(psi[hit[0]]) if len(hit) else None,
                           "rank": int(hit[0]) if len(hit) else None}
    n_extra = int(len(masks) - sum(1 for v in found.values()
                                   if v["psi"] is not None))
    print(f"[cert] recovered {sum(v['psi'] is not None for v in found.values())}"
          f"/{len(PLANTED)} planted chars; {n_extra} extra kept "
          f"(gate {tau * tau / 4:.2e})", flush=True)
    for k, v in found.items():
        print(f"[cert]   {k}: coef^2 {v['coef2']:.4f} psi "
              f"{v['psi'] if v['psi'] is None else round(v['psi'], 4)} "
              f"rank {v['rank']}", flush=True)
    # refit on relabeled tr2, eval on relabeled te2 -- calculated only
    m_, r_, tag = tr_spec.split(":")
    dtr = np.load(_flat_path(int(m_), int(r_), tag))
    m_, r_, tag = te_spec.split(":")
    dte = np.load(_flat_path(int(m_), int(r_), tag))
    y_tr = _planted_f(dtr["wins"], codes, depth, rng)
    y_te = _planted_f(dte["wins"], codes, depth, rng)
    dev = "cuda" if torch.cuda.is_available() else "cpu"

    def tb(wins):
        return codes[wins[:, ::-1].astype(np.int64)] \
            .reshape(len(wins), -1)[:, :depth * B]

    bt = torch.tensor(tb(dtr["wins"]), device=dev)
    bte = torch.tensor(tb(dte["wins"]), device=dev)
    ytrn = E.normalize_scores(y_tr)
    c0 = float(ytrn.mean())
    g_t = torch.tensor(ytrn - c0, device=dev)
    summary = {"tau": tau, "n_masks": int(len(masks)), "found": found}
    kmax = min(len(masks), len(bt) // 4)             # rows must support the
    if kmax:                                         # deflated char count
        masks_k = masks[:kmax]
        C, _ = E.sequential_deflate(bt, g_t, None, device=dev, block=512,
                                    masks=masks_k)
        C_t = torch.tensor(C, device=dev)
        pred = torch.full((len(bte),), c0, device=dev)
        for lo in range(0, kmax, 4096):              # chunked: 1.1M masks x
            pred += E.mask_parity_features(bte, masks_k[lo:lo + 4096])                 @ C_t[lo:lo + 4096]                  # 10k rows OOM'd unchunked
        summary["kmax"] = int(kmax)
        summary["test"] = E.score_metrics(pred.cpu().numpy(), y_te)
        print(f"[cert] refit test R2 {summary['test']['r2']:.4f} "
              f"(noise ceiling ~{1 - 0.03**2 / np.var(E.normalize_scores(y_te)):.4f})",
              flush=True)
    with open(f"{ROOT}/summary_planted_cert.json", "w") as fh:
        json.dump(summary, fh, indent=1)
    E.vol.commit()
    print(json.dumps(summary), flush=True)
    return summary


@E.app.local_entrypoint()
def csamp_main(stage: str = "tree", encoding: str = "lsh",
               m_fibers: int = 1000, g: int = 12, r: int = 8,
               m_test: int = 300, depth: int = DEPTH, tau: float = 0.05,
               max_width: int = 512, batch: int = 16,
               n_pre: int = 25000, tag: str = "tr2", skip: int = 1500,
               tr_spec: str = "", te_spec: str = "",
               ks: str = "1000,4000,16000,50000,100000"):
    if stage == "gen":
        print(gen_csamp.remote(m_fibers, g, r, m_test, depth, batch))
    elif stage == "genflat":
        print(gen_flat.remote(n_pre, r, tag, skip, batch))
    elif stage == "tree":
        encs = ("lsh", "ctrl") if encoding == "all" else (encoding,)
        for enc in encs:
            gl_tree.remote(enc, m_fibers, g, r, m_test, depth, tau, max_width,
                           ks, tr_spec, te_spec)
    elif stage == "planted":
        print(planted_cert.remote(m_fibers, g, r, depth, tau, max_width))
    else:
        raise SystemExit(f"unknown stage {stage}")
