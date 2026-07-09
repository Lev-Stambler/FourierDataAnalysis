"""Hydra entrypoint: run E2 (context profile) + E5 (degree) and emit figures + JSON.

    uv run python -m fda_exp.cli
    uv run python -m fda_exp.cli tau=0.2 datasets='[{name:random,kind:uniform_random,kwargs:{n:12,m:800,label:"parity:0,1"}}]'
"""

from __future__ import annotations

import json
import os

import hydra
import numpy as np
from omegaconf import DictConfig, OmegaConf

from . import datasets
from .context_profile import apply_order, context_profile, make_order, profile_summary
from .degree import degree_report
from . import figures


def _build(spec: DictConfig):
    kwargs = OmegaConf.to_container(spec.kwargs, resolve=True) if "kwargs" in spec else {}
    D, fD, f_full = datasets.build(spec.kind, **kwargs)
    return D, np.asarray(fD, dtype=np.float64), f_full


def run(cfg: DictConfig) -> dict:
    os.makedirs(cfg.outdir, exist_ok=True)
    results = {"config": OmegaConf.to_container(cfg, resolve=True), "E2": {}, "E5": {}}

    # ---- E2: context profile across datasets (and orders) ------------------
    profiles_for_fig = {}
    for spec in cfg.datasets:
        D, fD, _ = _build(spec)
        for order_kind in cfg.orders:
            order = make_order(D, order_kind, seed=cfg.seed)
            Dp = apply_order(D, order)
            levels = context_profile(Dp, fD, tau=float(cfg.tau), exact_cap=int(cfg.exact_cap))
            key = f"{spec.name}/{order_kind}"
            results["E2"][key] = {
                "m": int(D.shape[0]), "n": int(D.shape[1]),
                "summary": profile_summary(levels),
                "levels": {int(k): v for k, v in levels.items()},
            }
            # one order per dataset on the figure to keep it legible
            if order_kind == cfg.orders[0]:
                profiles_for_fig[spec.name] = levels
            s = results["E2"][key]["summary"]
            oh = s["search_overhead"]
            print(f"[E2/GL] {key:22s} m={D.shape[0]:5d} "
                  f"output={s['output']:6d}  N_max={s['N_max']:6d}  "
                  f"search_overhead={oh:.1f}x" if oh else f"[E2/GL] {key}")

    figures.fig_context_profile(
        profiles_for_fig, os.path.join(cfg.outdir, "fig_e2_context_profile.png"),
        title=f"Context profile (tau={cfg.tau})",
    )

    # ---- E5: on-dataset vs global degree -----------------------------------
    D, fD, f_full = _build(cfg.degree_dataset)
    n = int(D.shape[1])
    rep = degree_report(D, fD, f_full, n)
    results["E5"] = {"dataset": cfg.degree_dataset.name, "m": int(D.shape[0]), "n": n,
                     "report": rep}
    # NOTE: E5 (degree) tests the *separate* low-degree LEARNING theorem
    # (thm:learning-low-degree). It is irrelevant to the GL headline, which
    # recovers heavy coefficients at ANY degree. Reported for completeness only.
    print(f"[E5/learning] {cfg.degree_dataset.name}: on-dataset mean degree="
          f"{rep.get('on_dataset_mean_degree'):.2f}, global={rep.get('global_mean_degree'):.2f} "
          f"(NB: degree is irrelevant to GL)")
    figures.fig_degree(rep, os.path.join(cfg.outdir, "fig_e5_degree.png"),
                       title=f"On-dataset vs global degree ({cfg.degree_dataset.name})")

    with open(os.path.join(cfg.outdir, "results.json"), "w") as fh:
        json.dump(results, fh, indent=2)
    print(f"[done] wrote {cfg.outdir}/results.json and two figures")
    return results


@hydra.main(version_base=None, config_path="../conf", config_name="config")
def main(cfg: DictConfig) -> None:
    run(cfg)


if __name__ == "__main__":
    main()
