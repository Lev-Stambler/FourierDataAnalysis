"""G1 smoke: F1-k1 easy cell at protocol size (q=32, L=64), 1e5 tokens.

Gate (PLAN M4): val CE reaches Bayes floor + theta within budget; manifest written.
Runs on CPU locally (no GPU on this host — documented deviation); the Modal GPU
path lands with the M6 grid runner.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from dlx.families import F1Markov
from dlx.learners.transformer import TransformerConfig
from dlx.training.run import THETA_DEFAULT, train_run


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--budget", type=int, default=100_000)
    ap.add_argument("--device", default="cpu")
    ap.add_argument("--outdir", type=Path, default=Path("runs/local/g1_smoke"))
    args = ap.parse_args()

    fam = F1Markov(q=32, L=64, k=1, eta=0.1)
    cfg = TransformerConfig(vocab=32, ctx_len=64)  # preregistered defaults
    out = args.outdir / f"F1_k1_q32_L64_s0"
    m = train_run(fam, cfg, budget_tokens=args.budget, seed=0, out_dir=out,
                  cell_id="G1-smoke/F1-k1", protocol_hash="pre-freeze:M4-smoke",
                  device=args.device, n_checkpoints=20)
    gate = m["T_star"] is not None
    print(json.dumps({
        "gate": "PASS" if gate else "FAIL",
        "bayes_floor": round(m["bayes_floor_bits"], 4),
        "T_star": m["T_star"],
        "final_gap_bits": round(m["final_gap_bits"], 4),
        "val_curve_last5": [round(v, 4) for v in m["val_ce_bits"][-5:]],
        "wallclock_seconds": round(m["wallclock_seconds"], 1),
        "run_dir": str(out),
    }, indent=2))


if __name__ == "__main__":
    main()
