"""Modal A10G: compress Dream-7B (masked-diffusion LM) via on-policy dataset-CSAMP distillation.

Two stages so LLM cost is paid once per dataset:
  gen_dataset : Dream builds the slot alphabet, draws real-story fibers (>=128-token context),
                infills the free window positions on-policy, labels distinct contexts with its own
                next-token distribution -> npz on the fda-cache volume.
  fit_search  : loads the npz, runs the (verbatim) GPU multiclass CSAMP search + KL-distilled head,
                prints the eval table vs the model on real held-out long-context windows.

    cd experiments
    uv run modal run modal_distill.py --regen        # stage 1 + stage 2
    uv run modal run modal_distill.py                # stage 2 only (iterate on search/fit knobs)
"""

import modal

app = modal.App("fda-distill")
vol = modal.Volume.from_name("fda-cache", create_if_missing=True)

image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install("numpy", "scikit-learn", "pyarrow", "torch==2.5.1", "transformers==4.46.2",
                 "accelerate")                                    # Dream's tested pins
    .env({"HF_HOME": "/cache/hf", "FDA_DATA_DIR": "/cache/fda_data"})
    .add_local_python_source("fda_exp")
)

CTX_LEN = 128                                                     # model context (user: >= 64 or 128)


def _npz_name(V, w, k, M, R, seed):
    return f"distill_V{V}_w{w}_k{k}_ctx{CTX_LEN}_M{M}_R{R}_s{seed}.npz"


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=10800)
def gen_dataset(V=512, w=6, k=3, M=4000, R=8, seed=0, n_stories=8000, n_eval=3000):
    import time

    import numpy as np
    import torch

    from fda_exp.distill_data import (build_top_vocab, diffusion_infill, draw_fibers,
                                      fiber_disjoint_split, label_model, load_dream,
                                      make_tok2slot, soft_collapse)
    from fda_exp.hf_data import load_texts

    t0 = time.time()
    model, tok, mask_id = load_dream(device="cuda")
    print(f"[gen] Dream loaded ({time.time() - t0:.0f}s), mask_id={mask_id}", flush=True)

    # Dream-native sanity check: the labeling path must produce story-like continuations.
    probe = np.asarray(tok("Once upon a time, there was a little", add_special_tokens=False)["input_ids"])
    out = model(input_ids=torch.as_tensor(probe[None], device="cuda"))
    lg = (out.logits if hasattr(out, "logits") else out[0])[0, -1].float()          # predicts next pos
    top = torch.topk(torch.softmax(lg, -1), 5)
    print("[gen] probe 'Once upon a time, there was a little' ->",
          [(tok.decode([int(i)]), round(float(p), 3)) for i, p in zip(top.indices, top.values)],
          flush=True)

    texts = load_texts(n_stories, split="valid")
    cut = int(0.85 * len(texts))
    slot_ids, streams = build_top_vocab(model, tok, texts[:cut], V=V, mask_id=mask_id,
                                        ctx_len=CTX_LEN, device="cuda")
    tok2slot = make_tok2slot(slot_ids, model.config.vocab_size)
    allowed = slot_ids[1:]
    print(f"[gen] slots built ({time.time() - t0:.0f}s); e.g. "
          f"{[tok.decode([i]) for i in allowed[:8]]}", flush=True)

    PRE, S, WIN, _ = draw_fibers(streams, w, k, CTX_LEN, M, allowed, seed=seed)
    t1 = time.time()
    C_tok, fiber = diffusion_infill(model, PRE, S, w, allowed, mask_id, R=R, batch=64,
                                    seed=seed, device="cuda")
    print(f"[gen] infilled {len(C_tok)} contexts in {time.time() - t1:.0f}s "
          f"({len(C_tok) / (time.time() - t1):.0f}/s)", flush=True)

    tr, va = fiber_disjoint_split(fiber, val_frac=0.15, seed=seed)
    parts = {}
    for name, mask in (("tr", tr), ("va", va)):
        Cd, n_ctx, inv, m = soft_collapse(tok2slot[C_tok[mask]])
        fib_d = np.zeros(len(Cd), dtype=np.int64)
        fib_d[inv] = fiber[mask]                                  # each distinct ctx -> its fiber
        P = label_model(model, PRE[fib_d], slot_ids[Cd], mask_id, slot_ids, batch=64, device="cuda")
        parts[name] = (Cd, n_ctx, P)
        print(f"[gen] {name}: {m} ctx -> {len(Cd)} distinct, labeled "
              f"({time.time() - t0:.0f}s), OTHER={P[:, 0].mean():.3f}", flush=True)

    # eval fibers from held-out stories: REAL windows + real next tokens, labeled with real prefixes
    ev_streams = [np.asarray(tok(t, add_special_tokens=False)["input_ids"], np.int64)
                  for t in texts[cut:]]
    PRE_e, _, WIN_e, y_e = draw_fibers(ev_streams, w, k, CTX_LEN, n_eval, allowed, seed=seed)
    P_e = label_model(model, PRE_e, WIN_e, mask_id, slot_ids, batch=64, device="cuda")
    print(f"[gen] eval: {n_eval} real windows labeled ({time.time() - t0:.0f}s)", flush=True)

    name = _npz_name(V, w, k, M, R, seed)
    np.savez_compressed(
        f"/cache/{name}", V=V, w=w, k=k, ctx_len=CTX_LEN, M=M, R=R, seed=seed, slot_ids=slot_ids,
        Cd_tr=parts["tr"][0], n_tr=parts["tr"][1], P_tr=parts["tr"][2],
        Cd_va=parts["va"][0], n_va=parts["va"][1], P_va=parts["va"][2],
        Ceval=tok2slot[WIN_e], yeval=tok2slot[y_e], P_eval=P_e)
    vol.commit()
    print(f"[gen] saved /cache/{name} ({time.time() - t0:.0f}s total)", flush=True)
    return name


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=1800)
def fit_search(name, max_width=6000, top_classes=64, top_k=2000, additive=False, staged=False):
    import numpy as np

    from fda_exp.distill_lm import (eval_distill, fit_additive, fit_distill_lm, fit_staged,
                                    kl_model_student)

    vol.reload()                                                  # warm container: pick up fresh npz
    z = np.load(f"/cache/{name}")
    V, w = int(z["V"]), int(z["w"])
    if staged:                                                    # backoff: one position at a time
        model = fit_staged(z["Cd_tr"], z["n_tr"], z["P_tr"], z["Cd_va"], z["n_va"], z["P_va"],
                           V, w, device="cuda")
    elif additive or w * (int(V).bit_length() - 1) > 62:          # one-hot deg<=1 student (also the
        model = fit_additive(z["Cd_tr"], z["n_tr"], z["P_tr"], z["Cd_va"], z["n_va"], z["P_va"],
                             V, w, device="cuda")                 # only option past int64 packing)
    else:
        model = fit_distill_lm(z["Cd_tr"], z["n_tr"], z["P_tr"], z["Cd_va"], z["n_va"], z["P_va"],
                               V, w, max_width=max_width, top_classes=top_classes, top_k=top_k,
                               device="cuda")
    res = eval_distill(model, z["Ceval"], z["yeval"], z["P_eval"])
    for split, dg in (("tr", True), ("va", True), ("va_deg2", False)):
        key = split.split("_")[0]
        res[f"kl_{split}_gen"] = kl_model_student(model, z[f"Cd_{key}"], z[f"P_{key}"], deg3=dg)

    def klrow(P, Q):                                              # constant-predictor calibration
        P = np.clip(np.asarray(P, np.float64), 1e-12, 1)
        return (P * (np.log(P) - np.log(np.clip(Q, 1e-12, 1)))).sum(1)
    n_tr, P_tr = z["n_tr"].astype(np.float64), z["P_tr"].astype(np.float64)
    Pbar = (n_tr[:, None] * P_tr).sum(0) / n_tr.sum()
    for split, P in (("tr", z["P_tr"]), ("va", z["P_va"]), ("ev", z["P_eval"])):
        res[f"kl_unigram_{split}"] = float(klrow(P, Pbar[None]).mean())
    for j in (1, 2, 3):                                           # bucket-mean (suffix j-gram) refs:
        key = lambda C: (C[:, w - j:].astype(np.int64) * (V ** np.arange(j))).sum(1)
        uk, inv = np.unique(key(z["Cd_tr"]), return_inverse=True)
        num = np.zeros((len(uk), V)); den = np.zeros(len(uk))
        np.add.at(num, inv, n_tr[:, None] * P_tr)
        np.add.at(den, inv, n_tr)
        mean = (num + 1e-3 * Pbar[None] * den.mean()) / (den + 1e-3 * den.mean())[:, None]
        lut = {int(u): mean[i] for i, u in enumerate(uk)}
        Qe = np.stack([lut.get(int(x), Pbar) for x in key(z["Ceval"])])
        res[f"kl_bucket{j}_ev"] = float(klrow(z["P_eval"], Qe).mean())
    print("\n######## DISTILL EVAL (real held-out long-context windows) ########", flush=True)
    for kk, vv in res.items():
        print(f"  {kk:>16}: {vv}", flush=True)
    return {k: (v if not isinstance(v, dict) else dict(v)) for k, v in res.items()}


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=3600)
def prep_fibers(V=512, w=6, k=3, M=12000, seed=0, n_stories=8000, n_eval=3000, win_f=False):
    """STAGE 0 (one GPU): slot vocab, M fiber anchors, and labeled REAL eval windows -> prep npz.
    win_f=True targets the WINDOW-RESTRICTED function: every label (train shards + eval) uses one
    CONSTANT 122-token story prefix, so f is fully determined by the window (no hidden-prefix info,
    no 1.62-nat floor) while Dream still sees ctx_len tokens."""
    import os

    import numpy as np

    from fda_exp.distill_data import build_top_vocab, draw_fibers, label_model, load_dream, make_tok2slot
    from fda_exp.hf_data import load_texts

    name = f"prep_V{V}_w{w}_k{k}_ctx{CTX_LEN}_M{M}_s{seed}{'_winf' if win_f else ''}.npz"
    vol.reload()
    if os.path.exists(f"/cache/{name}"):
        print(f"[prep] cached: {name}", flush=True)
        return name
    model, tok, mask_id = load_dream(device="cuda")
    texts = load_texts(n_stories, split="valid")
    cut = int(0.85 * len(texts))
    slot_ids, streams = build_top_vocab(model, tok, texts[:cut], V=V, mask_id=mask_id,
                                        ctx_len=CTX_LEN, device="cuda")
    PRE, S, _, _ = draw_fibers(streams, w, k, CTX_LEN, M, slot_ids[1:], seed=seed)
    const_pre = np.concatenate([s for s in streams if len(s) >= CTX_LEN - w])[:CTX_LEN - w]
    if win_f:
        PRE = np.tile(const_pre, (len(PRE), 1))                   # labels see only the window vary
    ev_streams = [np.asarray(tok(t, add_special_tokens=False)["input_ids"], np.int64)
                  for t in texts[cut:]]
    PRE_e, _, WIN_e, y_e = draw_fibers(ev_streams, w, k, CTX_LEN, n_eval, slot_ids[1:], seed=seed)
    if win_f:
        PRE_e = np.tile(const_pre, (len(PRE_e), 1))
    P_e = label_model(model, PRE_e, WIN_e, mask_id, slot_ids, batch=64, device="cuda")
    t2s = make_tok2slot(slot_ids, model.config.vocab_size)
    np.savez_compressed(f"/cache/{name}", V=V, w=w, k=k, mask_id=mask_id, slot_ids=slot_ids,
                        PRE=PRE, S=S, Ceval=t2s[WIN_e], yeval=t2s[y_e], P_eval=P_e,
                        WIN_tok=WIN_e)                            # raw ids (slots are lossy at slot 0)
    vol.commit()
    print(f"[prep] saved /cache/{name}: {M} fibers, {n_eval} eval windows (win_f={win_f})", flush=True)
    return name


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=10800, retries=2)
def gen_shard(prep_name, lo, hi, R=8, steps=None, temperature=1.0, seed=0):
    """One shard: infill + collapse + label fibers [lo, hi).  Per-shard collapse is globally correct
    because fixed strings are unique per fiber (duplicate contexts cannot span shards).  DURABLE:
    saves its own npz to the volume and is skipped when it already exists, so one crashed shard never
    loses its siblings' work.  Returns the shard npz name."""
    import numpy as np

    from fda_exp.distill_data import diffusion_infill, label_model, load_dream, make_tok2slot, soft_collapse

    out = f"shard_{prep_name.replace('.npz', '')}_{lo}_{hi}_R{R}_st{steps or 0}_t{temperature}.npz"
    vol.reload()
    import os
    if os.path.exists(f"/cache/{out}"):
        print(f"[shard {lo}:{hi}] cached: {out}", flush=True)
        return out
    z = np.load(f"/cache/{prep_name}")
    model, tok, mask_id = load_dream(device="cuda")
    slot_ids, w = z["slot_ids"], int(z["w"])
    PRE, S = z["PRE"][lo:hi], z["S"][lo:hi]
    batch = 32 if w > 16 else 64                                  # long windows: more cublas headroom
    C_tok, fib = diffusion_infill(model, PRE, S, w, slot_ids[1:], int(z["mask_id"]), R=R,
                                  steps=steps, temperature=temperature, batch=batch,
                                  seed=seed + lo, device="cuda")
    t2s = make_tok2slot(slot_ids, model.config.vocab_size)
    Cd, n_ctx, inv, m = soft_collapse(t2s[C_tok])
    fib_d = np.zeros(len(Cd), dtype=np.int64)
    fib_d[inv] = fib
    Rd = np.zeros_like(Cd)                                        # RAW ids per distinct ctx: slot 0
    Rd[inv] = C_tok                                               # is lossy (-1 sentinel), never feed
    P = label_model(model, PRE[fib_d], Rd, int(z["mask_id"]), slot_ids,   # slot_ids[Cd] to the model
                    batch=batch, device="cuda")
    np.savez_compressed(f"/cache/{out}", Cd=Cd, n_ctx=n_ctx.astype(np.int64), P=P, fib=fib_d + lo)
    vol.commit()
    print(f"[shard {lo}:{hi}] {m} ctx -> {len(Cd)} distinct, labeled -> {out}", flush=True)
    return out


@app.function(image=image, volumes={"/cache": vol}, timeout=14400)
def gen_parallel(V=512, w=6, k=3, M=12000, R=8, seed=0, shards=8, steps=0, temperature=1.0,
                 win_f=False):
    """Sharded stage 1: prep once, fan infill+labeling out over `shards` A10G containers, merge,
    fiber-disjoint split, save the same npz schema fit_search consumes."""
    import numpy as np

    prep_name = prep_fibers.remote(V=V, w=w, k=k, M=M, seed=seed, win_f=win_f)
    bounds = np.linspace(0, M, shards + 1, dtype=int)
    args = [(prep_name, int(a), int(b), R, (steps or None), temperature, seed)
            for a, b in zip(bounds[:-1], bounds[1:]) if b > a]
    parts = list(gen_shard.starmap(args))                         # durable shard npz names
    vol.reload()
    zs = []
    for p in parts:                                               # EAGER read + close: npz handles
        with np.load(f"/cache/{p}") as q:                         # left open block vol.commit()
            zs.append({kk: q[kk] for kk in ("Cd", "n_ctx", "P", "fib")})
    Cd = np.concatenate([q["Cd"] for q in zs])
    n_ctx = np.concatenate([q["n_ctx"] for q in zs])
    P = np.concatenate([q["P"] for q in zs])
    fib = np.concatenate([q["fib"] for q in zs])
    rng = np.random.default_rng(seed)
    va_fibers = rng.random(M) < 0.15                              # fiber-disjoint split
    va = va_fibers[fib]
    with np.load(f"/cache/{prep_name}") as zp:                    # eager read + close (see above)
        prep = {kk: zp[kk] for kk in ("slot_ids", "Ceval", "yeval", "P_eval")}
    tag = ("" if not steps and temperature == 1.0 else f"_st{steps}_t{temperature}") + \
        ("_winf" if win_f else "")
    name = _npz_name(V, w, k, M, R, seed).replace(".npz", tag + ".npz")
    np.savez_compressed(f"/cache/{name}", V=V, w=w, k=k, ctx_len=CTX_LEN, M=M, R=R, seed=seed,
                        slot_ids=prep["slot_ids"],
                        Cd_tr=Cd[~va], n_tr=n_ctx[~va], P_tr=P[~va],
                        Cd_va=Cd[va], n_va=n_ctx[va], P_va=P[va],
                        Ceval=prep["Ceval"], yeval=prep["yeval"], P_eval=prep["P_eval"])
    vol.commit()
    print(f"[gen-parallel] saved /cache/{name}: {len(Cd)} distinct ctx "
          f"(tr={int((~va).sum())} va={int(va.sum())})", flush=True)
    return name


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=1800)
def measure_floor(name):
    """The info-theoretic floor for ANY window-only student vs full-context labels:
    mean KL( Dream(next | real 122-tok prefix + window)  ||  Dream(next | window alone) )
    on the eval windows.  If this >> target KL, the target needs the window-restricted f."""
    import numpy as np
    import torch

    from fda_exp.distill_data import label_model, load_dream

    vol.reload()
    z = np.load(f"/cache/{name}")
    model, tok, mask_id = load_dream(device="cuda")
    slot_ids = z["slot_ids"]
    if "WIN_tok" in z.files:
        WIN_tok = z["WIN_tok"]                                    # raw ids (windows may contain OOV)
    else:
        assert (z["Ceval"] > 0).all(), "legacy npz lacks WIN_tok and has OOV slots"
        WIN_tok = slot_ids[z["Ceval"]]                            # legacy all-in-alphabet w=6 files
    bos = np.full((len(WIN_tok), 1), tok.convert_tokens_to_ids("<|beginoftext|>"), dtype=np.int64)
    P_win = label_model(model, bos, WIN_tok, mask_id, slot_ids, batch=64, device="cuda")
    P_full = np.clip(z["P_eval"].astype(np.float64), 1e-12, 1)
    kl = float((P_full * (np.log(P_full) - np.log(np.clip(P_win.astype(np.float64), 1e-12, 1))))
               .sum(1).mean())
    agree = float((P_full.argmax(1) == P_win.argmax(1)).mean())
    print(f"[floor] over {len(WIN_tok)} eval windows (w={int(z['w'])}): "
          f"KL(full-ctx || window-only) = {kl:.3f} nats, top-1 agreement = {agree:.3f}", flush=True)
    return dict(kl=kl, top1_agree=agree)


@app.local_entrypoint()
def main(regen: bool = False, v: int = 512, w: int = 6, k: int = 3, m: int = 4000, r: int = 8,
         seed: int = 0, max_width: int = 6000, top_k: int = 2000, shards: int = 0,
         gen_steps: int = 0, temperature: float = 1.0, win_f: bool = False,
         additive: bool = False, staged: bool = False):
    name = _npz_name(v, w, k, m, r, seed)
    if gen_steps or temperature != 1.0:
        name = name.replace(".npz", f"_st{gen_steps}_t{temperature}.npz")
    if win_f:
        name = name.replace(".npz", "_winf.npz")
    if regen and shards > 0:                                      # parallel sharded generation
        name = gen_parallel.remote(V=v, w=w, k=k, M=m, R=r, seed=seed, shards=shards,
                                   steps=gen_steps, temperature=temperature, win_f=win_f)
    elif regen:
        name = gen_dataset.remote(V=v, w=w, k=k, M=m, R=r, seed=seed)
    print(fit_search.remote(name, max_width=max_width, top_k=top_k, additive=additive,
                            staged=staged))
