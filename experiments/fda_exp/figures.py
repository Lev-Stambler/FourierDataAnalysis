"""The two headline figures.

- ``fig_context_profile``: R_k (and exact N_k where available) vs level k, for
  several datasets/orders on one axis --- random blows up, subcube is
  output-sized, structured/real sits low.  *The* figure for E2.
- ``fig_degree``: on-dataset vs global degree profiles (fraction of spectral
  mass per degree) for one (D, f).  The figure for E5.
"""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np


def fig_context_profile(profiles: dict, path: str, title: str = "Context profile"):
    """``profiles``: mapping label -> per-level dict from ``context_profile``."""
    fig, (axR, axN) = plt.subplots(1, 2, figsize=(11, 4.2))
    for label, levels in profiles.items():
        ks = sorted(levels)
        Rk = [levels[k]["R_k"] for k in ks]
        axR.semilogy(ks, np.maximum(Rk, 1e-12), marker="o", ms=3, label=label)
        Nk = [levels[k]["N_k"] for k in ks]
        if all(v is not None for v in Nk):
            axN.semilogy(ks, np.maximum(Nk, 0.5), marker="s", ms=3, label=label)
    n = max(max(levels) for levels in profiles.values())
    axR.semilogy(range(n + 1), [2.0 ** k for k in range(n + 1)],
                 "k--", lw=1, alpha=0.5, label=r"$2^k$ (blind)")
    axN.semilogy(range(n + 1), [2.0 ** k for k in range(n + 1)],
                 "k--", lw=1, alpha=0.5, label=r"$2^k$ (blind)")
    axR.set_xlabel("level $k$"); axR.set_ylabel(r"level mass $R_k$"); axR.legend(fontsize=8)
    axN.set_xlabel("level $k$"); axN.set_ylabel(r"search width $N_k$"); axN.legend(fontsize=8)
    fig.suptitle(title)
    fig.tight_layout()
    fig.savefig(path, dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_degree(report: dict, path: str, title: str = "On-dataset vs global degree"):
    on = np.asarray(report["on_dataset_fraction"]) if report.get("on_dataset_fraction") else None
    gl = np.asarray(report["global_fraction"]) if report.get("global_fraction") else None
    fig, ax = plt.subplots(figsize=(6.5, 4.2))
    if on is not None:
        ks = np.arange(len(on))
        ax.bar(ks - 0.2, on, width=0.4,
               label=f"on-dataset (mean deg {report.get('on_dataset_mean_degree'):.2f})")
    if gl is not None:
        ks = np.arange(len(gl))
        ax.bar(ks + 0.2, gl, width=0.4,
               label=f"global (mean deg {report.get('global_mean_degree'):.2f})")
    ax.set_xlabel("degree $k$"); ax.set_ylabel("fraction of spectral mass")
    ax.legend(fontsize=9); ax.set_title(title)
    fig.tight_layout()
    fig.savefig(path, dpi=150, bbox_inches="tight")
    plt.close(fig)
