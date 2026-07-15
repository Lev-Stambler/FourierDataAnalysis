"""PURE Dataset GL -- the canonical, minimal implementation.

The score is measured, not constructed: with z a dataset prefix and x1, x2 two
REAL independent model continuations of z,

    psi_hat(S) = E_z E_{x1,x2}[ f(x1,z) chi_S(x1) * f(x2,z) chi_S(x2) ]

estimated verbatim from the cached forks (1500 dataset fibers x g independent
continuations per level; the i != j branch pairs ARE the (x1,x2) samples).
Density term and all -- the coefficient under the model law given dataset z.
No subcell subtraction, no centering, no deg-1 residual, no standardization:
f = H / ||H||_row (the paper's ||f|| <= 1 contract), the hereditary tree, the
tau^2/4 gate, a supervised refit ladder.  That's everything.
"""
import numpy as np

import qary_lsh_dataset_gl as Q


def pair_psi(bits, masks, F, gid, device=None, char_chunk=4096):
    """psi_hat(S) = mean over fibers z and REAL-sample pairs i != j in z of
    [f_i chi_S(i) * f_j chi_S(j)].  Computed as sum_z ||sum_i chi F||^2 minus
    the i = j terms, over n_pairs = sum_z c_z(c_z - 1) -- a computational
    identity for the pair double-sum (a branch paired with itself is E[f^2],
    never an (x1,x2) sample)."""
    import torch
    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    F_t = torch.as_tensor(np.asarray(F, np.float32), device=device)
    _, gi = np.unique(np.asarray(gid), return_inverse=True)
    gid_t = torch.as_tensor(gi.astype(np.int64), device=device)
    ng = int(gi.max()) + 1
    counts = np.bincount(gi)
    n_pairs = float((counts * (counts - 1)).sum())
    assert n_pairs > 0, "no fiber has two continuations -- nothing to pair"
    diag = float((F_t.double() ** 2).sum())
    masks = np.asarray(masks, np.uint8)
    out = []
    for lo in range(0, len(masks), char_chunk):
        chi = torch.as_tensor(
            Q.parity_features(bits, masks[lo:lo + char_chunk]).T.copy(),
            device=device)
        Qs = Q._bucket_Q(chi, F_t, gid_t, ng, mem_budget=2.0e8).cpu().numpy()
        out.append((Qs - diag) / n_pairs)
    return np.concatenate(out) if out else np.zeros(0)


def pure_gl_tree(levels, B, tau, max_width=512, device=None, progress=None):
    """Hereditary Dataset GL on the pure pair score.  levels[j] = (bits
    (m, (j+1)*B) newest-first, F (m, d) with ||f|| <= 1, fiber gid (m,)).
    Extend live prefixes by single bits of token block j, keep children with
    psi_hat >= tau^2/4, carry the empty char + the top max_width forward.
    Returns dict(masks psi-sorted, psi, per_level_kept)."""
    depth = len(levels)
    thresh = tau * tau / 4.0
    total = depth * B
    live = np.zeros((1, total), np.uint8)
    kept = {}
    per_level_kept = []
    for j, (bits, F, gid) in enumerate(levels):
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
        psi = pair_psi(bits, children[:, :w], F, gid, device=device)
        heavy = np.flatnonzero(psi >= thresh)
        for i in heavy:
            key = tuple(int(x) for x in children[i])
            if psi[i] > kept.get(key, -1e30):
                kept[key] = float(psi[i])
        per_level_kept.append(int(len(heavy)))
        top = sorted(kept.items(), key=lambda kv: -kv[1])[:max_width]
        live = np.concatenate([np.zeros((1, total), np.uint8),
                               np.array([k for k, _ in top], np.uint8)]) \
            if top else np.zeros((1, total), np.uint8)
        if progress is not None:
            progress(j, depth, kept)
    if not kept:
        return dict(masks=np.zeros((0, total), np.uint8), psi=np.zeros(0),
                    per_level_kept=per_level_kept)
    order = sorted(kept.items(), key=lambda kv: -kv[1])
    return dict(masks=np.array([k for k, _ in order], np.uint8),
                psi=np.array([v for _, v in order]),
                per_level_kept=per_level_kept)


@Q.app.function(image=Q.image.add_local_python_source("qary_lsh_dataset_gl"),
                gpu="A100-40GB", volumes={"/cache": Q.vol}, timeout=43200,
                memory=32768, secrets=Q.WANDB_SECRET)
def pure_gl_top1(m_fibers: int = 1500, g: int = 16, depth: int = 6,
                 fill_len: int = 61, tau: float = 0.1, max_width: int = 512,
                 flat_m: int = 16000, flat_r: int = 8):
    """Pure Dataset GL end to end: pure_gl_tree per encoding on the cached
    H-forks, then a supervised refit ladder (top-k by psi -> ridge -> unembed
    -> full-vocab TEST top-1)."""
    import json
    import os
    import torch
    Q.vol.reload()
    for j in range(depth):
        p = Q._oracle_table_path(fill_len, m_fibers, g, j)
        assert os.path.exists(p) and "H" in np.load(p).files, f"missing H fork {p}"
    te_tbl = Q.make_data.local(3000, flat_r, 0, fill_len, "edu", 0, "edu_test")
    tr_tbl = Q.make_data.local(flat_m, flat_r, 0, fill_len, "edu", 9000, "edu_tr")
    te_lbl = Q.relabel_hidden.local(3000, fill_len, flat_r, "edu_test")
    tr_lbl = Q.relabel_hidden.local(flat_m, fill_len, flat_r, "edu_tr")
    Wu = np.load(f"{Q.ROOT}/lm_head.npz")["Wu"].astype(np.float32)
    ztab = dict(np.load(f"{Q.ROOT}/codes.npz"))

    def collapse(tbl, lbl):
        d = np.load(tbl); L = np.load(lbl)
        ctx = np.concatenate([d["PRE"][d["fiber_id"]], d["G"]], axis=1)
        rows, inv, cnt = np.unique(ctx, axis=0, return_inverse=True,
                                   return_counts=True)
        Hs = np.zeros((len(rows), L["H"].shape[1]), np.float64)
        np.add.at(Hs, inv, L["H"].astype(np.float64))
        tg = np.empty(len(rows), np.int64); tg[inv] = L["tstar"]
        return rows, Hs / cnt[:, None], cnt.astype(np.float64), tg

    c_tr, mH_tr, n_tr, t_tr = collapse(tr_tbl, tr_lbl)
    c_te, mH_te, n_te, t_te = collapse(te_tbl, te_lbl)
    vm = np.random.default_rng(0).random(len(n_tr)) < 0.15
    sane = Q.weighted_agreement(Q.unembed_top1(mH_te.astype(np.float32), Wu),
                                t_te, n_te)
    summary = {"depth": depth, "tau": tau, "encodings": {}}
    for name in ("lsh", "ctrl"):
        codes = ztab[name]; B = codes.shape[1]; nb = depth * B
        levels = []
        for j in range(depth):
            d = np.load(Q._oracle_table_path(fill_len, m_fibers, g, j))
            bits = np.asarray(codes, np.uint8)[d["gtoks"][:, ::-1]] \
                .reshape(len(d["gtoks"]), -1)
            Y = d["H"].astype(np.float64)
            F = (Y / (np.linalg.norm(Y, axis=1, keepdims=True) + 1e-12)
                 ).astype(np.float32)                              # ||f|| <= 1
            levels.append((bits, F, d["fiber_gid"]))
        tree = pure_gl_tree(levels, B, tau, max_width=max_width,
                            progress=lambda j, D, kept: print(
                                f"[puregl:{name}] level {j+1}/{D} kept {len(kept)}",
                                flush=True))
        masks, psi = tree["masks"], tree["psi"]
        deg = np.array([int(m.reshape(depth, B).any(1).sum()) for m in masks]) \
            if len(masks) else np.zeros(0, int)
        print(f"[puregl:{name}] {len(masks)} chars, deg hist "
              f"{np.bincount(deg, minlength=depth+1).tolist()}", flush=True)
        np.savez_compressed(
            f"{Q.ROOT}/pure_gl_masks_{name}_f{fill_len}_d{depth}.npz",
            masks=masks, psi=psi, deg=deg)
        Q.vol.commit()
        bt = Q.context_bits(c_tr, codes)[:, :nb]
        bte = Q.context_bits(c_te, codes)[:, :nb]
        run = Q._wandb_run(f"pure-gl-{name}-d{depth}",
                           {"encoding": name, "depth": depth, "tau": tau,
                            "n_chars": int(len(masks)), "sanity": sane})
        psitot = float(psi.sum()) if len(psi) else 1.0
        ladder = []
        max_chars = min(5000, len(masks))
        ks = list(range(1000, max_chars + 1, 1000))
        if not ks or ks[-1] < max_chars:
            ks.append(max_chars)                                  # final partial rung
        for k in ks:
            Ftr = Q.parity_features(bt, masks[:k])
            Fte = Q.parity_features(bte, masks[:k])
            best = None
            for wd in (10.0, 100.0, 1000.0):
                fit = Q.fit_regression(Ftr[~vm], mH_tr[~vm], n_tr[~vm], wd=wd)
                acc = Q.weighted_agreement(
                    Q.unembed_top1(Ftr[vm] @ fit["W"] + fit["b"], Wu),
                    t_tr[vm], n_tr[vm])
                if best is None or acc > best[0]:
                    best = (acc, wd)
            fit = Q.fit_regression(Ftr, mH_tr, n_tr, wd=best[1])
            Hp = Fte @ fit["W"] + fit["b"]
            rec = {"n_chars": int(k),
                   "TEST_top1": Q.weighted_agreement(Q.unembed_top1(Hp, Wu),
                                                     t_te, n_te),
                   "psi_frac": float(psi[:k].sum() / psitot), "wd": best[1]}
            ladder.append(rec)
            print(f"[puregl:{name}] top-{k} top1 {rec['TEST_top1']:.4f} "
                  f"psi_frac {rec['psi_frac']:.3f} (sanity {sane:.3f})",
                  flush=True)
            if run is not None:
                run.log(rec)
            del Ftr, Fte
            torch.cuda.empty_cache()
        if run is not None:
            run.finish()
        summary["encodings"][name] = {"n_chars": int(len(masks)),
                                      "sanity": sane, "ladder": ladder}
    Q._write_json(f"{Q.ROOT}/summary_pure_gl_d{depth}.json", summary)
    print(json.dumps(summary), flush=True)
    return summary


@Q.app.local_entrypoint()
def pure_main(m_fibers: int = 1500, g: int = 16, depth: int = 6,
              fill_len: int = 61, tau: float = 0.1, max_width: int = 512):
    print(pure_gl_top1.remote(m_fibers, g, depth, fill_len, tau, max_width))


@Q.app.function(image=Q.image.add_local_python_source("qary_lsh_dataset_gl"),
                gpu="A100-40GB", volumes={"/cache": Q.vol}, timeout=21600,
                memory=32768, secrets=Q.WANDB_SECRET)
def kl_refit(prefix: str = "pure_gl_masks", encodings: str = "lsh,ctrl",
             ks: str = "1000,5000", depth: int = 6, fill_len: int = 61,
             flat_m: int = 16000, flat_r: int = 8, steps: int = 400,
             lr: float = 0.05, batch: int = 1024):
    """The PAPER'S prescribed refit (ar_categorical_gl.typ, density-spectrum &
    supervised-refitting): recovered characters are FEATURES; their vector
    weights are refit under the Fourier student's SUPERVISED KL objective on
    full-vocab probability vectors -- teacher p = softmax(H_true @ Wu^T),
    student q = softmax((F @ W + b) @ Wu^T), Wu frozen.  Reports TEST
    KL(teacher||student) and top-1, overall AND on the high-teacher-margin
    half (the paper: top-1 needs a teacher-margin condition).  Also runs the
    all-deg-1 control (798 chars) for calibration.  No tree rerun: consumes
    the persisted masks npz."""
    import json
    import torch
    Q.vol.reload()
    dev = "cuda"
    te_tbl = Q.make_data.local(3000, flat_r, 0, fill_len, "edu", 0, "edu_test")
    tr_tbl = Q.make_data.local(flat_m, flat_r, 0, fill_len, "edu", 9000, "edu_tr")
    te_lbl = Q.relabel_hidden.local(3000, fill_len, flat_r, "edu_test")
    tr_lbl = Q.relabel_hidden.local(flat_m, fill_len, flat_r, "edu_tr")
    Wu = torch.tensor(np.load(f"{Q.ROOT}/lm_head.npz")["Wu"].astype(np.float32),
                      device=dev)
    ztab = dict(np.load(f"{Q.ROOT}/codes.npz"))

    def collapse(tbl, lbl):
        d = np.load(tbl); L = np.load(lbl)
        ctx = np.concatenate([d["PRE"][d["fiber_id"]], d["G"]], axis=1)
        rows, inv, cnt = np.unique(ctx, axis=0, return_inverse=True,
                                   return_counts=True)
        Hs = np.zeros((len(rows), L["H"].shape[1]), np.float64)
        np.add.at(Hs, inv, L["H"].astype(np.float64))
        tg = np.empty(len(rows), np.int64); tg[inv] = L["tstar"]
        return rows, (Hs / cnt[:, None]).astype(np.float32), \
            cnt.astype(np.float64), tg

    c_tr, mH_tr, n_tr, t_tr = collapse(tr_tbl, tr_lbl)
    c_te, mH_te, n_te, t_te = collapse(te_tbl, te_lbl)

    def teacher_margin(mH):                                       # top1 - top2 logit gap
        out = np.empty(len(mH), np.float32)
        for lo in range(0, len(mH), 2048):
            lg = torch.tensor(mH[lo:lo + 2048], device=dev) @ Wu.t()
            v, _ = torch.topk(lg, 2, dim=1)
            out[lo:lo + 2048] = (v[:, 0] - v[:, 1]).cpu().numpy()
        return out
    marg_te = teacher_margin(mH_te)
    hi = marg_te >= np.median(marg_te)

    def fit_eval(Ftr, Fte, tag):
        K = Ftr.shape[1]
        F_t = torch.tensor(Ftr, device=dev)
        Fe_t = torch.tensor(Fte, device=dev)
        Ht = torch.tensor(mH_tr, device=dev)
        w_t = torch.tensor(n_tr / n_tr.sum(), device=dev, dtype=torch.float32)
        W = torch.zeros((K, Wu.shape[1]), device=dev, requires_grad=True)
        b = torch.tensor(mH_tr.mean(0), device=dev).requires_grad_(True)
        opt = torch.optim.Adam([W, b], lr=lr)
        sched = torch.optim.lr_scheduler.CosineAnnealingLR(opt, T_max=steps)
        gen = torch.Generator(device=dev).manual_seed(0)
        for s in range(steps):
            idx = torch.multinomial(w_t, batch, replacement=True, generator=gen)
            with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
                p = torch.softmax(Ht[idx] @ Wu.t(), dim=1)
                ql = torch.log_softmax((F_t[idx] @ W + b) @ Wu.t(), dim=1)
                loss = -(p * ql).sum(1).mean()                    # CE == KL + const
            opt.zero_grad(); loss.backward(); opt.step(); sched.step()
            if s % 100 == 0:
                print(f"[klrefit:{tag}] step {s} loss {float(loss):.4f}",
                      flush=True)
        with torch.no_grad():
            Hp = (Fe_t @ W + b).cpu().numpy()
        kl = Q._full_kl(Hp, mH_te, Wu.cpu().numpy(), n_te)
        pred = Q.unembed_top1(Hp, Wu.cpu().numpy())
        rec = {"tag": tag, "K": int(K), "TEST_KL": kl,
               "TEST_top1": Q.weighted_agreement(pred, t_te, n_te),
               "TEST_top1_himargin": Q.weighted_agreement(
                   pred[hi], t_te[hi], n_te[hi])}
        print(f"[klrefit:{tag}] KL {rec['TEST_KL']:.4f} "
              f"top1 {rec['TEST_top1']:.4f} "
              f"top1@himargin {rec['TEST_top1_himargin']:.4f}", flush=True)
        return rec

    results = []
    for name in encodings.split(","):
        codes = ztab[name]; B = codes.shape[1]; nb = depth * B
        bt = Q.context_bits(c_tr, codes)[:, :nb]
        bte = Q.context_bits(c_te, codes)[:, :nb]
        d1 = np.eye(nb, dtype=np.uint8)                           # deg-1 control
        results.append(fit_eval(Q.parity_features(bt, d1),
                                Q.parity_features(bte, d1), f"{name}-deg1"))
        md = np.load(f"{Q.ROOT}/{prefix}_{name}_d{depth}.npz")["masks"]
        for k in [int(x) for x in ks.split(",")]:
            k = min(k, len(md))
            results.append(fit_eval(Q.parity_features(bt, md[:k]),
                                    Q.parity_features(bte, md[:k]),
                                    f"{name}-top{k}"))
    Q._write_json(f"{Q.ROOT}/summary_kl_refit_{prefix}.json",
                  {"results": results})
    print(json.dumps(results), flush=True)
    return results


@Q.app.local_entrypoint()
def kl_refit_main(prefix: str = "pure_gl_masks", encodings: str = "lsh,ctrl",
                  ks: str = "1000,5000"):
    print(kl_refit.remote(prefix=prefix, encodings=encodings, ks=ks))


@Q.app.function(image=Q.image.add_local_python_source("qary_lsh_dataset_gl"),
                gpu="A10G", volumes={"/cache": Q.vol}, timeout=21600,
                memory=32768, secrets=Q.WANDB_SECRET)
def gl_recon_eval(prefix: str = "pure_gl_masks", encodings: str = "lsh,ctrl",
                  ks: str = "0,1000,5000", depth: int = 6, fill_len: int = 61,
                  flat_m: int = 16000, flat_r: int = 8):
    """CLOSED-FORM Dataset-GL evaluation -- coefficients are CALCULATED, never
    optimized (no Adam, no ridge).  Base = exact weighted deg-1 LS
    (fit_deg1_exact); the tree's characters then get their plain weighted
    dataset Fourier coefficients by sequential matching-pursuit deflation
    against the deg-1 TRAIN residual (sequential_deflate, psi order);
    reconstruct_ladder snapshots TEST full-vocab top-1 and KL at each k
    (k=0 rung = the deg-1 base alone)."""
    import json
    import os
    Q.vol.reload()
    te_tbl = Q.make_data.local(3000, flat_r, 0, fill_len, "edu", 0, "edu_test")
    tr_tbl = Q.make_data.local(flat_m, flat_r, 0, fill_len, "edu", 9000, "edu_tr")
    te_lbl = Q.relabel_hidden.local(3000, fill_len, flat_r, "edu_test")
    tr_lbl = Q.relabel_hidden.local(flat_m, fill_len, flat_r, "edu_tr")
    Wu = np.load(f"{Q.ROOT}/lm_head.npz")["Wu"].astype(np.float32)
    ztab = dict(np.load(f"{Q.ROOT}/codes.npz"))

    def collapse(tbl, lbl):
        d = np.load(tbl); L = np.load(lbl)
        ctx = np.concatenate([d["PRE"][d["fiber_id"]], d["G"]], axis=1)
        rows, inv, cnt = np.unique(ctx, axis=0, return_inverse=True,
                                   return_counts=True)
        Hs = np.zeros((len(rows), L["H"].shape[1]), np.float64)
        np.add.at(Hs, inv, L["H"].astype(np.float64))
        tg = np.empty(len(rows), np.int64); tg[inv] = L["tstar"]
        return rows, (Hs / cnt[:, None]).astype(np.float32), \
            cnt.astype(np.float64), tg

    c_tr, mH_tr, n_tr, t_tr = collapse(tr_tbl, tr_lbl)
    c_te, mH_te, n_te, t_te = collapse(te_tbl, te_lbl)
    sane = Q.weighted_agreement(Q.unembed_top1(mH_te, Wu), t_te, n_te)
    klist = sorted({int(x) for x in ks.split(",")})
    out = {}
    for name in encodings.split(","):
        codes = ztab[name]; B = codes.shape[1]; nb = depth * B
        bt = Q.context_bits(c_tr, codes)[:, :nb]
        bte = Q.context_bits(c_te, codes)[:, :nb]
        f1 = Q.fit_deg1_exact(bt, mH_tr, n_tr)                    # closed form
        X_tr = 1.0 - 2.0 * bt.astype(np.float32)
        X_te = 1.0 - 2.0 * bte.astype(np.float32)
        base_tr = X_tr @ f1["W"] + f1["b"]
        base_te = X_te @ f1["W"] + f1["b"]
        G_res = mH_tr - base_tr                                   # deg-1-free residual
        c0 = (G_res * (n_tr / n_tr.sum())[:, None]).sum(0)        # deg-0 of residual
        p = f"{Q.ROOT}/{prefix}_{name}_f{fill_len}_d{depth}.npz"
        if not os.path.exists(p):
            p = f"{Q.ROOT}/{prefix}_{name}_d{depth}.npz"          # legacy f61 name
        md = np.load(p)["masks"][:max(klist)]
        idx = Q.masks_to_indices(md, width=depth)
        C, _ = Q.sequential_deflate(bt, G_res, n_tr, idx)         # calculated coeffs
        ladder = Q.reconstruct_ladder(bte, idx, C, c0,
                                      np.arange(len(idx)), klist, Wu, t_te,
                                      n_te, kl_ref=(mH_te, 1.0), base=base_te)
        for rec in ladder:
            print(f"[glrecon:{name}] k={rec['k']} top1 {rec['top1']:.4f} "
                  f"KL {rec.get('kl', float('nan')):.4f} (sanity {sane:.3f})",
                  flush=True)
        out[name] = ladder
    Q._write_json(f"{Q.ROOT}/summary_gl_recon_{prefix}_f{fill_len}.json",
                  {"sanity": sane, "ladders": out})
    print(json.dumps(out), flush=True)
    return out


@Q.app.local_entrypoint()
def gl_recon_main(prefix: str = "pure_gl_masks", encodings: str = "lsh,ctrl",
                  ks: str = "0,1000,5000", fill_len: int = 61):
    print(gl_recon_eval.remote(prefix=prefix, encodings=encodings, ks=ks,
                               fill_len=fill_len))
