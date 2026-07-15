"""Diagnose the gl-tree-top1 flatline: (A) is the fork psi-estimator output on
the hidden target just noise floor (real vs shuffled-pairing null, vs the
tau^2/4 keep threshold)?  (B) does a deg-1-only fit on the flat table still
carry real top-1 signal (fit-path sanity, should reproduce the earlier
LSH-beats-ctrl result)?"""
import numpy as np

import qary_lsh_dataset_gl as Q


@Q.app.function(image=Q.image.add_local_python_source("qary_lsh_dataset_gl"),
                gpu="A10G", volumes={"/cache": Q.vol}, timeout=3600, memory=32768)
def diag(fill_len: int = 61, m_fibers: int = 1500, g: int = 16,
         flat_m: int = 16000, flat_r: int = 8, tau: float = 0.1,
         do_fit: bool = True):
    import torch
    Q.vol.reload()
    out = {}

    # ---- A) psi noise-floor test on the p0 fork (deg-1, block 0) ----------
    codes = dict(np.load(f"{Q.ROOT}/codes.npz"))["lsh"]
    B = codes.shape[1]
    d = np.load(Q._oracle_table_path(fill_len, m_fibers, g, 0))
    gt = d["gtoks"]                                   # (m, 1) newest token
    bits = np.asarray(codes, np.uint8)[gt[:, ::-1]].reshape(len(gt), -1)
    Y = d["H"].astype(np.float64)
    F = ((Y - Y.mean(0)[None, :]) / (Y.std(0)[None, :] + 1e-6)).astype(np.float32)
    gid = d["fiber_gid"].astype(np.int64)

    def psi_all_bits(F_rows, bits_rows, gid_rows, masks=None):
        dev = "cuda" if torch.cuda.is_available() else "cpu"
        F_t = torch.tensor(F_rows, device=dev)
        _, g2 = np.unique(gid_rows, return_inverse=True)
        g2 = g2.astype(np.int64)
        ng = int(g2.max()) + 1
        gid_t = torch.tensor(g2, dtype=torch.long, device=dev)
        counts = np.bincount(g2, minlength=ng).astype(np.float64)
        packed = np.packbits(bits_rows, axis=1)
        view = np.ascontiguousarray(packed).view(
            [("", packed.dtype)] * packed.shape[1]).ravel()
        _, cod = np.unique(view, return_inverse=True)
        _, sub = np.unique(g2 * (int(cod.max()) + 1) + cod, return_inverse=True)
        sub = sub.astype(np.int64)
        nsub = int(sub.max()) + 1
        sub_t = torch.tensor(sub, dtype=torch.long, device=dev)
        subc = np.bincount(sub, minlength=nsub).astype(np.float64)
        block = float(Q._bucket_Q(torch.ones((1, len(F_rows)), device=dev), F_t,
                                  sub_t, nsub, mem_budget=2.0e8).cpu().numpy()[0])
        n_pairs = float((counts * (counts - 1)).sum() - (subc * (subc - 1)).sum())
        if masks is None:
            masks = np.eye(bits_rows.shape[1], dtype=np.uint8)
        psis = []
        for lo in range(0, len(masks), 4096):                      # char-batched
            chi = torch.tensor(
                Q.parity_features(bits_rows, masks[lo:lo + 4096]).T.copy(),
                device=dev)
            psis.append(Q._level_psi(chi, F_t, gid_t, ng, block, n_pairs, dev))
        return np.concatenate(psis)

    psi_real = psi_all_bits(F, bits, gid)
    rng = np.random.default_rng(0)
    psi_null = psi_all_bits(F[rng.permutation(len(F))], bits, gid)
    # the paper's "constant unit-vector query": F == a fixed unit vector for
    # every row -> psi is the PURE ROLLOUT-DENSITY spectrum E_z E[chi_S|z]^2
    # (no functional content).  If it matches psi_real, the spectrum is
    # density-dominated and no tau prunes without subtracting it.
    psi_dens = psi_all_bits(np.ones((len(F), 1), np.float32), bits, gid)
    thresh = tau * tau / 4.0
    out["psi"] = {
        "thresh": thresh,
        "real_q": np.percentile(psi_real, [5, 25, 50, 75, 95]).tolist(),
        "null_q": np.percentile(psi_null, [5, 25, 50, 75, 95]).tolist(),
        "density_q": np.percentile(psi_dens, [5, 25, 50, 75, 95]).tolist(),
        "real_frac_above_thresh": float((psi_real >= thresh).mean()),
        "null_frac_above_thresh": float((psi_null >= thresh).mean()),
        "density_frac_above_thresh": float((psi_dens >= thresh).mean()),
        "real_max": float(psi_real.max()), "null_max": float(psi_null.max()),
        "density_max": float(psi_dens.max()),
        "corr_real_density": float(np.corrcoef(psi_real, psi_dens)[0, 1]),
    }
    print("[diag] psi:", out["psi"], flush=True)

    # ---- A2) DEG-2 selection: raw vs density vs per-fiber-CENTERED psi -----
    # the run's frontier/top-K choose among deg-2+ chars; measure whether that
    # choice is density-driven there, and how it differs from the functional
    # (covariance) ranking that per-fiber centering isolates
    d1 = np.load(Q._oracle_table_path(fill_len, m_fibers, g, 1))   # p1 fork
    gt1 = d1["gtoks"]
    bits1 = np.asarray(codes, np.uint8)[gt1[:, ::-1]].reshape(len(gt1), -1)
    gid1 = d1["fiber_gid"].astype(np.int64)
    Y1 = d1["H"].astype(np.float64)
    Y1 = (Y1 - Y1.mean(0)[None, :]) / (Y1.std(0)[None, :] + 1e-6)
    F1 = (Y1 / (np.linalg.norm(Y1, axis=1, keepdims=True) + 1e-12)).astype(np.float32)
    fm = np.zeros((gid1.max() + 1, Y1.shape[1]))                   # per-fiber mean
    np.add.at(fm, gid1, Y1)
    fm /= np.bincount(gid1)[:, None]
    Yc = Y1 - fm[gid1]
    Fc = (Yc / (np.linalg.norm(Yc, axis=1, keepdims=True) + 1e-12)).astype(np.float32)
    ii, jj = np.meshgrid(np.arange(B), np.arange(B), indexing="ij")
    masks2 = np.zeros((B * B, 2 * B), np.uint8)                    # all deg-2 pairs
    masks2[np.arange(B * B), ii.ravel()] = 1                       # bit in block 0
    masks2[np.arange(B * B), B + jj.ravel()] = 1                   # bit in block 1
    p_raw = psi_all_bits(F1, bits1, gid1, masks2)
    p_den = psi_all_bits(np.ones((len(F1), 1), np.float32), bits1, gid1, masks2)
    p_cen = psi_all_bits(Fc, bits1, gid1, masks2)
    p_cen_null = psi_all_bits(Fc[np.random.default_rng(1).permutation(len(Fc))],
                              bits1, gid1, masks2)

    def top_overlap(a, b, k=512):
        return float(len(set(np.argsort(-a)[:k]) & set(np.argsort(-b)[:k])) / k)

    out["deg2"] = {
        "n_chars": int(len(masks2)),
        "corr_raw_density": float(np.corrcoef(p_raw, p_den)[0, 1]),
        "corr_raw_centered": float(np.corrcoef(p_raw, p_cen)[0, 1]),
        "top512_overlap_raw_vs_density": top_overlap(p_raw, p_den),
        "top512_overlap_raw_vs_centered": top_overlap(p_raw, p_cen),
        "raw_q": np.percentile(p_raw, [5, 50, 95]).tolist(),
        "density_q": np.percentile(p_den, [5, 50, 95]).tolist(),
        "centered_q": np.percentile(p_cen, [5, 50, 95]).tolist(),
        "centered_null_q": np.percentile(p_cen_null, [5, 50, 95]).tolist(),
        "centered_frac_above_thresh": float((p_cen >= thresh).mean()),
        "centered_null_max": float(p_cen_null.max()),
    }
    print("[diag] deg2:", out["deg2"], flush=True)

    # ---- B) deg-1-only fit on the flat table (all 6*B single-bit chars) ---
    if not do_fit:
        return out
    Wu = np.load(f"{Q.ROOT}/lm_head.npz")["Wu"].astype(np.float32)
    nb = 6 * B

    def collapse(tag, m):
        dd = np.load(Q._table_path(fill_len, m, flat_r, tag))
        L = np.load(f"{Q.ROOT}/labels_hidden_f{fill_len}_M{m}_R{flat_r}_{tag}.npz")
        ctx = np.concatenate([dd["PRE"][dd["fiber_id"]], dd["G"]], axis=1)
        rows, inv, cnt = np.unique(ctx, axis=0, return_inverse=True,
                                   return_counts=True)
        Hs = np.zeros((len(rows), L["H"].shape[1]), np.float64)
        np.add.at(Hs, inv, L["H"].astype(np.float64))
        tg = np.empty(len(rows), np.int64)
        tg[inv] = L["tstar"]
        return rows, Hs / cnt[:, None], cnt.astype(np.float64), tg

    c_tr, mH_tr, n_tr, t_tr = collapse("edu_tr", flat_m)
    c_te, mH_te, n_te, t_te = collapse("edu_test", 3000)
    bt = Q.context_bits(c_tr, codes)[:, :nb]
    bte = Q.context_bits(c_te, codes)[:, :nb]
    masks1 = np.eye(nb, dtype=np.uint8)                       # every deg-1 char
    Ftr = Q.parity_features(bt, masks1)
    Fte = Q.parity_features(bte, masks1)
    vm = np.random.default_rng(0).random(len(n_tr)) < 0.15
    best = None
    for wd in (10.0, 100.0, 1000.0):
        fit = Q.fit_regression(Ftr[~vm], mH_tr[~vm], n_tr[~vm], wd=wd)
        acc = Q.weighted_agreement(
            Q.unembed_top1(Ftr[vm] @ fit["W"] + fit["b"], Wu), t_tr[vm], n_tr[vm])
        if best is None or acc > best[0]:
            best = (acc, wd)
    fit = Q.fit_regression(Ftr, mH_tr, n_tr, wd=best[1])
    Htr_hat = Ftr @ fit["W"] + fit["b"]
    Hte_hat = Fte @ fit["W"] + fit["b"]
    out["deg1_fit"] = {
        "wd": best[1], "val_top1": best[0],
        "TRAIN_top1": Q.weighted_agreement(Q.unembed_top1(Htr_hat, Wu), t_tr, n_tr),
        "TEST_top1": Q.weighted_agreement(Q.unembed_top1(Hte_hat, Wu), t_te, n_te),
        "sanity": Q.weighted_agreement(Q.unembed_top1(mH_te.astype(np.float32), Wu),
                                       t_te, n_te),
        "base_rate": float(max(np.bincount(t_te,
                                           weights=n_te)) / n_te.sum()),
    }
    print("[diag] deg1_fit:", out["deg1_fit"], flush=True)
    return out


@Q.app.local_entrypoint()
def diag_main(do_fit: bool = True):
    import json
    print(json.dumps(diag.remote(do_fit=do_fit), indent=2))
