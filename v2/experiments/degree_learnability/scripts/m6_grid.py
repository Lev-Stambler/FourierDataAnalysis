"""M6 synthetic grid runner (PLAN §8 milestone M6).

Default: Modal fan-out (gpu or cpu per PLAN tiers). --local: parallel local CPU
execution (fallback path, PLAN goal constraints), e.g. for debugging or when Modal
is unavailable; artifacts then land under runs/local/m6/.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path


def _one_local(cell_dict):
    import torch
    torch.set_num_threads(2)
    sys.path.insert(0, str(Path(__file__).parent.parent))
    from dlx.grid import CellSpec, load_protocol, make_family
    from dlx.learners.transformer import TransformerConfig
    from dlx.training.run import train_run
    proto = load_protocol(Path(os.environ["DLX_PROTOCOL"]))
    outdir = Path(os.environ["DLX_OUTDIR"])
    spec = CellSpec.from_dict(cell_dict)
    fam = make_family(spec.family, spec.family_params)
    cfg = TransformerConfig(**proto["learner_config"])
    out = outdir / spec.cell_id.replace("/", "__")
    m = train_run(fam, cfg, budget_tokens=spec.budget_tokens, seed=spec.seed,
                  out_dir=out, cell_id=spec.cell_id,
                  protocol_hash=proto["protocol_hash"], device="cpu",
                  n_checkpoints=20, tokens_per_step=proto["tokens_per_step"])
    return {"cell_id": spec.cell_id, "T_star": m["T_star"],
            "final_gap_bits": m["final_gap_bits"]}


def run_local(cells, outdir: Path, workers: int, protocol: dict):
    os.makedirs(outdir, exist_ok=True)
    os.environ["DLX_PROTOCOL"] = str(Path(__file__).parent.parent / "configs" / "protocol_v1.json")
    os.environ["DLX_OUTDIR"] = str(outdir)
    from multiprocessing import Pool
    with Pool(workers) as pool:
        results = pool.map(_one_local, [c if isinstance(c, dict) else c.to_dict() for c in cells])
    return results


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--local", action="store_true", help="run on local CPU instead of Modal")
    ap.add_argument("--device", default="cpu", choices=["cpu", "gpu"],
                    help="Modal cell type (PLAN G2 default gpu; d64 cells are CPU-fine)")
    ap.add_argument("--limit", type=int, default=0)
    ap.add_argument("--workers", type=int, default=max(2, (os.cpu_count() or 4) - 2))
    args = ap.parse_args()

    sys.path.insert(0, str(Path(__file__).parent.parent))
    from dlx.grid import load_protocol, synthetic_grid
    proto = load_protocol()
    cells = synthetic_grid(proto)
    if args.limit:
        cells = cells[: args.limit]

    if args.local:
        print(f"LOCAL mode: {len(cells)} cells, {args.workers} workers")
        results = run_local(cells, Path("runs/local/m6"), args.workers, proto)
        out = Path("runs/local/m6/grid_results.json")
        out.write_text(json.dumps(results, indent=2))
        print(f"wrote {out}; {sum(1 for r in results if r['final_gap_bits'] is not None)}/{len(results)} done")
    else:
        cmd = ["modal", "run", "dlx/modal_app.py::run_grid",
               "--selector", "m6-synthetic", "--device", args.device]
        if args.limit:
            cmd += ["--limit", str(args.limit)]
        print("MODAL mode:", " ".join(cmd))
        subprocess.run(cmd, check=True)


if __name__ == "__main__":
    main()
