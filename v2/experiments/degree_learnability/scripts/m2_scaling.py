"""M2 theorem-scaling curves: L0 sample complexity vs N_d.

Target: random degree-<=d polynomial over Z_q^n (dense, unit energy). Measure
m*(eps) = smallest sample count with exact-cube error <= eps, for several d,
and fit log m* vs log N_d. The theorem predicts slope ~ 1 (m* ~ N_d / eps).

Runs at S1/S2 tier sizes; writes JSON artifacts under runs/.
"""

from __future__ import annotations

import argparse
import json
import time
from pathlib import Path

import numpy as np

from dlx.learners import SpectralLearner, low_degree_indices


def random_target(q: int, n: int, d: int, rng: np.random.Generator):
    """Unit-E2-energy random polynomial of degree <= d (complex coefficients)."""
    idx = low_degree_indices(q, n, d)
    c = rng.standard_normal(idx.shape[0]) + 1j * rng.standard_normal(idx.shape[0])
    c /= np.sqrt((np.abs(c) ** 2).sum())  # Parseval: E|f|^2 = sum |c|^2 = 1
    return idx, c


def target_values(idx: np.ndarray, c: np.ndarray, xs: np.ndarray, q: int) -> np.ndarray:
    phase = (idx @ xs.T) % q
    return (np.exp(2j * np.pi / q) ** phase).T @ c


def run(n: int = 6, q: int = 4, ds=(1, 2, 3), eps: float = 0.1, seeds: int = 5,
        c_grid=(0.5, 1.0, 2.0, 3.0, 4.0, 6.0, 8.0, 12.0, 16.0, 24.0, 32.0)) -> dict:
    cube = np.array(np.unravel_index(np.arange(q**n), (q,) * n)).T
    records = []
    for d in ds:
        N_d = int(low_degree_indices(q, n, d).shape[0])
        m_stars = []
        for seed in range(seeds):
            rng = np.random.default_rng(1000 + 100 * d + seed)
            idx, c = random_target(q, n, d, rng)
            f_cube = target_values(idx, c, cube, q)
            m_star = None
            for cmult in c_grid:
                m = max(int(round(N_d * cmult)), N_d)
                xs = cube[rng.integers(0, q**n, size=m)]
                ys = target_values(idx, c, xs, q)
                lr = SpectralLearner(q=q, n=n, d=d).fit(xs, ys, dataset_size=q**n)
                err = float(np.mean(np.abs(f_cube - lr.predict(cube)) ** 2))
                if err <= eps:
                    m_star = m
                    break
            m_stars.append(m_star if m_star is not None else int(N_d * c_grid[-1]) * 2)
        records.append({"d": d, "N_d": N_d, "m_star_per_seed": m_stars,
                        "m_star": float(np.median(m_stars))})
    Nd = np.array([r["N_d"] for r in records], dtype=float)
    ms = np.array([r["m_star"] for r in records], dtype=float)
    slope = float(np.polyfit(np.log(Nd), np.log(ms), 1)[0])
    return {"n": n, "q": q, "eps": eps, "records": records, "slope": slope}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=6)
    ap.add_argument("--q", type=int, default=4)
    ap.add_argument("--outdir", type=Path, default=Path("runs/local/m2"))
    args = ap.parse_args()

    t0 = time.time()
    result = run(n=args.n, q=args.q)
    result["wall_seconds"] = time.time() - t0

    args.outdir.mkdir(parents=True, exist_ok=True)
    out = args.outdir / "scaling.json"
    out.write_text(json.dumps(result, indent=2))
    print(f"slope(log m* vs log N_d) = {result['slope']:.3f}  (theorem: ~1)")
    for r in result["records"]:
        print(f"  d={r['d']}  N_d={r['N_d']:6d}  m*={r['m_star']:.0f}")
    print(f"wrote {out} in {result['wall_seconds']:.1f}s")


if __name__ == "__main__":
    main()
