"""M5 pilot: select the single fixed L1 Transformer config (PLAN §5).

Preregistered rule (verbatim from PLAN §5): run a pilot on the mid-difficulty
family only; select the SMALLEST config whose final val CE is within 10% of the
LARGEST pilot config's final val CE on that family.

Mid family: F1Markov(q=32, L=64, k=3, eta=0.1).

Pre-freeze exploration (2026-08-04, logged in README deviation log): spread-lag
mod-sum families (F2) sit at a chance-level plateau for this harness within all
probed budgets (flat at 1M tokens even for d256/6L), while contiguous Markov-k
families learn with difficulty increasing in k, and pure lagged copy at any span
stays easy. The pilot therefore uses the learnable mid of the gradient, F1-k3;
F2 spread-lag cells remain in the grid as censored hard controls (T* = None
recorded). This keeps the pilot rule meaningful: the largest config must reach a
definite final loss within the pilot budget.

Pilot ladder (fixed here, pre-freeze): small/mid/large. One seed (pilot only;
grid seeds are separate). Budget 2M tokens each.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from dlx.families import F1Markov
from dlx.learners.transformer import TransformerConfig
from dlx.training.run import train_run

PILOT_LADDER = {
    "small": dict(d_model=64, n_layers=2, n_heads=4),
    "mid": dict(d_model=128, n_layers=4, n_heads=4),
    "large": dict(d_model=256, n_layers=6, n_heads=8),
}
ORDER = ["small", "mid", "large"]
PILOT_BUDGET = 2_000_000
WITHIN = 0.10


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--outdir", type=Path, default=Path("runs/local/m5_pilot"))
    ap.add_argument("--device", default="cpu")
    args = ap.parse_args()

    fam = F1Markov(q=32, L=64, k=3, eta=0.1)
    results = {}
    for name in ORDER:
        cfg = TransformerConfig(vocab=32, ctx_len=64, **PILOT_LADDER[name])
        m = train_run(fam, cfg, budget_tokens=PILOT_BUDGET, seed=0,
                      out_dir=args.outdir / name, cell_id=f"M5-pilot/{name}",
                      protocol_hash="pre-freeze:M5-pilot", device=args.device,
                      n_checkpoints=10)
        results[name] = {"final_val_ce": m["val_ce_bits"][-1],
                         "final_gap_bits": m["final_gap_bits"],
                         "n_params": m["n_params"], "config": PILOT_LADDER[name]}
        print(f"{name}: final_val_ce={results[name]['final_val_ce']:.4f} "
              f"params={results[name]['n_params']}")

    largest_final = results[ORDER[-1]]["final_val_ce"]
    threshold = largest_final * (1.0 + WITHIN)
    selected = next(n for n in ORDER if results[n]["final_val_ce"] <= threshold)

    out = {
        "rule": "smallest config with final val CE within 10% of the largest pilot config's final val CE",
        "mid_family": {"name": fam.name, "version": fam.version,
                       "params": fam.params()},
        "pilot_budget_tokens": PILOT_BUDGET,
        "within_fraction": WITHIN,
        "largest_config_final_val_ce": largest_final,
        "threshold": threshold,
        "results": results,
        "selected": selected,
        "selected_config": PILOT_LADDER[selected],
    }
    args.outdir.mkdir(parents=True, exist_ok=True)
    (args.outdir / "pilot.json").write_text(json.dumps(out, indent=2))
    print(f"\nselected: {selected}  (threshold {threshold:.4f})")
    print(f"wrote {args.outdir / 'pilot.json'}")


if __name__ == "__main__":
    main()
