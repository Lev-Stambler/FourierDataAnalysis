"""Modal app for dlx tier execution (PLAN §13.2).

One app: dlx-degree-learnability; one Volume: dlx-runs (mounted at /runs).
Cells are JSON-serializable CellSpec dicts; results land on the volume with full
manifests (PLAN §12.6). Fail-fast: every cell re-validates its family version
against the frozen protocol before training (dlx.grid.synthetic_grid asserts).
"""

from __future__ import annotations

from pathlib import Path

import modal

ROOT = Path(__file__).parent  # dlx package dir
PKG_ROOT = ROOT.parent          # degree_learnability/ (has configs/, tests/, scripts/)

app = modal.App("dlx-degree-learnability")
vol = modal.Volume.from_name("dlx-runs", create_if_missing=True)

_IGNORE = ("__pycache__", ".venv", ".pytest_cache", ".ruff_cache", "runs/", "nohup.out")


def _ignore(path) -> bool:
    """True = exclude from the image (modal ignore semantics)."""
    p = str(path)
    return any(seg in p for seg in _IGNORE)


image_gpu = (modal.Image.debian_slim(python_version="3.12")
             .pip_install("numpy>=1.26", "torch>=2.3", "pytest>=8")
             .add_local_dir(PKG_ROOT, "/root/pkg", ignore=_ignore))
image_cpu = (modal.Image.debian_slim(python_version="3.12")
             .pip_install("numpy>=1.26", "pytest>=8")
             .pip_install("torch", extra_index_url="https://download.pytorch.org/whl/cpu")
             .add_local_dir(PKG_ROOT, "/root/pkg", ignore=_ignore))


def _run_cell(cell: dict, device: str) -> dict:
    import sys
    sys.path.insert(0, "/root/pkg")
    from dlx.grid import CellSpec, load_protocol, make_family
    from dlx.learners.transformer import TransformerConfig
    from dlx.training.run import train_run

    spec = CellSpec.from_dict(cell)
    proto = load_protocol()
    fam = make_family(spec.family, spec.family_params)
    assert fam.version == proto["family_versions"].get(
        spec.cell_id.split("/")[1], fam.version), "family version drift vs protocol"
    cfg = TransformerConfig(**proto["learner_config"])
    out = Path("/runs") / spec.cell_id.replace("/", "__")
    metrics = train_run(fam, cfg, budget_tokens=spec.budget_tokens, seed=spec.seed,
                        out_dir=out, cell_id=spec.cell_id,
                        protocol_hash=proto["protocol_hash"], device=device,
                        n_checkpoints=20, tokens_per_step=proto["tokens_per_step"])
    vol.commit()
    return {"cell_id": spec.cell_id, "T_star": metrics["T_star"],
            "final_gap_bits": metrics["final_gap_bits"],
            "bayes_floor_bits": metrics["bayes_floor_bits"],
            "wallclock_seconds": metrics["wallclock_seconds"]}


@app.function(image=image_gpu, gpu="A10G", volumes={"/runs": vol},
              timeout=3 * 3600, retries=1)
def gpu_cell(cell: dict) -> dict:
    return _run_cell(cell, "cuda")


@app.function(image=image_cpu, volumes={"/runs": vol},
              timeout=3 * 3600, retries=1, cpu=4.0)
def cpu_cell(cell: dict) -> dict:
    return _run_cell(cell, "cpu")


@app.function(image=image_cpu, volumes={"/runs": vol}, timeout=1800)
def tier_smoke(tier: str) -> dict:
    """Fail-fast: import checks + tiny suite before any fan-out spends money."""
    import subprocess
    import sys
    sys.path.insert(0, "/root/pkg")
    r = subprocess.run([sys.executable, "-m", "pytest", "-q", "tests/test_domains.py",
                        "tests/test_profiles.py", "-x"], cwd="/root/pkg",
                       capture_output=True, text=True)
    ok = r.returncode == 0
    return {"tier": tier, "smoke_tests_ok": ok,
            "tail": (r.stdout + r.stderr)[-800:]}


@app.local_entrypoint()
def run_grid(selector: str, device: str = "gpu", limit: int = 0, only: str = ""):
    """Fan out one milestone's cells. selector: 'm6-synthetic' (more later)."""
    import sys
    sys.path.insert(0, str(PKG_ROOT))
    from dlx.grid import load_protocol, synthetic_grid

    proto = load_protocol()
    if selector == "m6-synthetic":
        cells = [c.to_dict() for c in synthetic_grid(proto)]
    else:
        raise ValueError(f"unknown selector {selector}")
    if only:
        cells = [c for c in cells if only in c["cell_id"]]
    if limit:
        cells = cells[:limit]

    smoke = tier_smoke.remote(selector)
    if not smoke["smoke_tests_ok"]:
        print("SMOKE FAILED — aborting before fan-out:\n", smoke["tail"])
        return
    print(f"smoke ok; fanning out {len(cells)} cells on device={device}")
    fn = gpu_cell if device == "gpu" else cpu_cell
    results = list(fn.map(cells))
    n_done = sum(1 for r in results if r.get("final_gap_bits") is not None)
    print(f"completed {n_done}/{len(results)} cells")
    for r in results:
        print(f"  {r['cell_id']}: T*={r['T_star']} gap={r['final_gap_bits']:.4f} "
              f"({r['wallclock_seconds']:.0f}s)")
