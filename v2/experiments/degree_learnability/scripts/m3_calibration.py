"""M3 S2-tier calibration: measured (B, Stab_rho) vs planted profile at protocol size.

Instance: F2(q=32, L=64, lags=(1,16), eta=0.1) — protocol-size domain, s=2.
Target f: the vector one-hot-ish next-token distribution (q-dim) on the uniform
context cube.  Uniform-resample noise (characters are eigenfunctions).

Preregistered pass threshold: max relative error over the rho-grid <= 5%
(at m = 2^16 pairs, MC SE ~ 0.4%, so 5% is generous), plus tail-bound
containment of the planted tail at every (rho, d).

Writes runs/local/m3/calibration.json with a PASS/FAIL verdict.
"""

from __future__ import annotations

import argparse
import json
import time
from pathlib import Path

import numpy as np

from dlx.families import F2SubsetSum
from dlx.profiles import (
    exact_stability_uniform_from_W,
    planted_profile,
    sampled_stability_uniform,
    tail_bounds,
)

THRESHOLD_REL_ERR = 0.05  # preregistered (PLAN M3 gate)
DELTA = 1e-6              # Hoeffding confidence for measured-input containment


def hoeffding_margin(m: int, n_checks: int, delta: float = DELTA) -> float:
    """Uniform margin over n_checks bounded [0,1] pair-mean estimates."""
    return float(np.sqrt(np.log(2.0 * n_checks / delta) / (2.0 * m)))


def run(q: int = 32, L: int = 64, lags=(1, 16), eta: float = 0.1, m: int = 1 << 16,
        rhos=(0.25, 0.5, 0.75, 0.9), seed: int = 3) -> dict:
    fam = F2SubsetSum(q=q, L=L, lags=lags, eta=eta)
    W = planted_profile(fam)
    B_true = float(W.sum())
    rng = np.random.default_rng(seed)

    def f_batch(xs):
        return fam.next_token_dist_batch(xs)

    # budget measured independently
    probe = rng.integers(0, q, size=(m, L))
    B_meas = float(np.mean(np.sum(np.abs(f_batch(probe)) ** 2, axis=-1)))

    records = []
    bounds_ok = True
    margin = hoeffding_margin(m, len(rhos) * 4)
    for rho in rhos:
        est = sampled_stability_uniform(f_batch, q, L, rho, m=m, rng=rng)
        stab_true = exact_stability_uniform_from_W(W, rho)
        rel_err = abs(est.real - stab_true) / max(abs(stab_true), 1e-12)
        # tail-bound containment of the PLANTED tail
        for d in (0, 1, 2, 3):
            truth = float(W[d + 1 :].sum())
            # (a) algebraic soundness with exact inputs: must hold tightly
            lo_x, up_x = tail_bounds(B_true, stab_true, rho, d)
            exact_ok = lo_x - 1e-9 <= truth <= up_x + 1e-9
            # (b) measured inputs: containment up to the registered margin
            lo, up = tail_bounds(B_meas, est.real, rho, d)
            meas_ok = lo - margin - 1e-9 <= truth <= up + margin + 1e-9
            bounds_ok &= exact_ok and meas_ok
            records.append({"rho": rho, "d": d, "stab_est": est.real,
                            "stab_true": stab_true, "rel_err": rel_err,
                            "tail_lo": lo, "tail_up": up, "tail_true": truth,
                            "bounds_contain": meas_ok,
                            "exact_input_contain": exact_ok})
    max_rel = max(r["rel_err"] for r in records if r["d"] == 0)
    verdict = "PASS" if (max_rel <= THRESHOLD_REL_ERR and bounds_ok) else "FAIL"
    return {"family": fam.name, "family_version": fam.version, "q": q, "L": L,
            "lags": list(lags), "eta": eta, "m": m, "B_true": B_true,
            "B_meas": B_meas, "B_rel_err": abs(B_meas - B_true) / B_true,
            "threshold_rel_err": THRESHOLD_REL_ERR, "max_rel_err": max_rel,
            "hoeffding_margin": margin, "delta": DELTA,
            "bounds_all_contain": bounds_ok, "verdict": verdict,
            "records": records}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--outdir", type=Path, default=Path("runs/local/m3"))
    ap.add_argument("--m", type=int, default=1 << 16)
    args = ap.parse_args()

    t0 = time.time()
    result = run(m=args.m)
    result["wall_seconds"] = time.time() - t0

    args.outdir.mkdir(parents=True, exist_ok=True)
    out = args.outdir / "calibration.json"
    out.write_text(json.dumps(result, indent=2))
    print(f"verdict={result['verdict']}  max_rel_err={result['max_rel_err']:.4f} "
          f"(threshold {THRESHOLD_REL_ERR})  B_rel_err={result['B_rel_err']:.4f}  "
          f"bounds_contain={result['bounds_all_contain']}")
    print(f"wrote {out} in {result['wall_seconds']:.1f}s")


if __name__ == "__main__":
    main()
