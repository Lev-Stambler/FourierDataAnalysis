"""GPU-batched Goldreich-Levin over a dataset (torch backend).

Same tree search as ``context_gl.context_gl_search`` (SAMP + CSAMP), but the two
expensive expectations are batched on the GPU:

- ``chi_S(x) = (-1)^{popcount(S & idx(x))}`` for ALL live buckets at a level at
  once, via an int64 AND + a bit-folding parity (works on MPS/CUDA);
- the sampled ``Psi(S|J) = E_z E_{x,x'~D_z}[ f(x)chi_S(x) f(x')chi_S(x') ]`` as a
  single ``(L, n_exp)`` elementwise product + mean.

Encodes contexts as an int64 ``idx`` (so ``n <= 62``); CSAMP draws a partner
sharing ``idx >> level`` (the suffix), the n-gram/subcube-conditioning primitive.
A ``max_width`` guard makes the *blindness* regime (search width explodes) return
cleanly instead of hanging.
"""

from __future__ import annotations

import numpy as np
import torch


def get_device(prefer="mps"):
    if prefer == "mps" and torch.backends.mps.is_available():
        return "mps"
    if torch.cuda.is_available():
        return "cuda"
    return "cpu"


def _parity64(v: torch.Tensor) -> torch.Tensor:
    """popcount(v) mod 2 for an int64 tensor, via bit folding."""
    for s in (32, 16, 8, 4, 2, 1):
        v = v ^ (v >> s)
    return v & 1


def sample_partners(gid: np.ndarray, rr: np.ndarray, rng) -> np.ndarray:
    """CSAMP: for each sampled row rr[e], return a random row sharing gid (same
    context suffix).  gid = idx >> level."""
    order = np.argsort(gid, kind="stable")
    gid_sorted = gid[order]
    uniq, start, counts = np.unique(gid_sorted, return_index=True, return_counts=True)
    grp = np.searchsorted(uniq, gid[rr])
    off = (rng.random(len(rr)) * counts[grp]).astype(np.int64)
    return order[start[grp] + off]


def _psi_batch(masks_dev, idx_rr, idx_rp, f_rr, f_rp, chunk=1024):
    """Psi estimate for every bucket in masks_dev (L,), batched over the shared
    (rr, rp) sample pairs."""
    L = masks_dev.shape[0]
    out = torch.empty(L, device=masks_dev.device, dtype=torch.float32)
    for i in range(0, L, chunk):
        M = masks_dev[i:i + chunk, None]                          # (c,1) int64
        chi_rr = 1.0 - 2.0 * _parity64(M & idx_rr[None, :]).float()   # (c,E)
        chi_rp = 1.0 - 2.0 * _parity64(M & idx_rp[None, :]).float()
        a_rr = f_rr[None, :] * chi_rr
        a_rp = f_rp[None, :] * chi_rp
        out[i:i + chunk] = (a_rr * a_rp).mean(dim=1)
    return out


def gl_search_torch(idx_np, f_np, n, tau, n_exp=20000, device=None,
                    mode="csamp", seed=0, max_width=200_000):
    """Run GL; return the recovered heavy masks and the per-level search widths.

    mode='csamp' = real oracle; mode='samp' = samples-only (partner drawn
    uniformly, ignoring context) -> demonstrates blindness.
    """
    device = device or get_device()
    m = len(idx_np)
    thresh = tau * tau / 4.0
    rng = np.random.default_rng(seed)
    idx_np = idx_np.astype(np.int64)
    f_np = f_np.astype(np.float32)

    live = np.array([0], dtype=np.int64)   # bucket S = empty
    widths, experiments = [], 0
    for k in range(n):
        kk = k + 1
        children = np.unique(np.concatenate([live, live | (np.int64(1) << k)]))
        rr = rng.integers(0, m, size=n_exp)
        if mode == "csamp":
            rp = sample_partners(idx_np >> kk, rr, rng)
        else:                                # samples-only: no context matching
            rp = rng.integers(0, m, size=n_exp)
        experiments += n_exp

        M = torch.tensor(children, dtype=torch.int64, device=device)
        idx_rr = torch.tensor(idx_np[rr], dtype=torch.int64, device=device)
        idx_rp = torch.tensor(idx_np[rp], dtype=torch.int64, device=device)
        f_rr = torch.tensor(f_np[rr], dtype=torch.float32, device=device)
        f_rp = torch.tensor(f_np[rp], dtype=torch.float32, device=device)
        psi = _psi_batch(M, idx_rr, idx_rp, f_rr, f_rp).cpu().numpy()

        live = children[psi >= thresh]
        widths.append(int(len(live)))
        if len(live) > max_width:            # blindness / blow-up guard
            return dict(status="blowup", level=kk, width=int(len(live)),
                        widths=widths, experiments=experiments, L=None)
    return dict(status="ok", L=sorted(int(s) for s in live),
                widths=widths, experiments=experiments)
