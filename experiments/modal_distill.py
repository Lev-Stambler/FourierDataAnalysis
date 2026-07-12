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
def fit_search(name, max_width=6000, top_classes=64, top_k=2000):
    import numpy as np

    from fda_exp.distill_lm import eval_distill, fit_distill_lm, kl_model_student

    vol.reload()                                                  # warm container: pick up fresh npz
    z = np.load(f"/cache/{name}")
    V, w = int(z["V"]), int(z["w"])
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


@app.local_entrypoint()
def main(regen: bool = False, v: int = 512, w: int = 6, k: int = 3, m: int = 4000, r: int = 8,
         seed: int = 0, max_width: int = 6000, top_k: int = 2000):
    name = _npz_name(v, w, k, m, r, seed)
    if regen:
        name = gen_dataset.remote(V=v, w=w, k=k, M=m, R=r, seed=seed)
    print(fit_search.remote(name, max_width=max_width, top_k=top_k))
