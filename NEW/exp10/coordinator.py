"""Equal-budget multi-fidelity coordinator and audit for Experiment 10."""

from __future__ import annotations

import argparse
import json
import math
import os
import statistics
import subprocess
from pathlib import Path

import numpy as np

from architecture_verdict import ARCHITECTURES, PRESETS, SCHEMA, SCALES, assert_matched_presets


LR_GRIDS = {
    # TinyStories broad-search optima were bracketed at 0.1024 and 0.0064.
    "kronecker": (0.0064, 0.0128, 0.0256, 0.0512, 0.1024, 0.2048),
    "transformer": (0.0008, 0.0016, 0.0032, 0.0064, 0.0128, 0.0256),
}
PHASE_FRACTIONS = {"screen": 0.10, "promotion": 0.30, "final": 1.00}
PROMOTIONS = 2
FINAL_SEEDS = (0, 1, 2)


def slug(lr: float) -> str:
    return f"{lr:.8g}".replace(".", "p")


def run_directory(
    root: Path,
    dataset: str,
    scale: str,
    architecture: str,
    phase: str,
    lr: float,
    seed: int,
) -> Path:
    return root / dataset / scale / architecture / phase / f"lr-{slug(lr)}" / f"seed-{seed}"


def corpus_windows(data_root: Path) -> int:
    manifest = json.loads((data_root / "manifest.json").read_text())
    return int(manifest["files"]["train.npy"]["windows"])


def phase_steps(data_root: Path, local_batch: int, phase: str, world: int = 8) -> int:
    full = corpus_windows(data_root) // (local_batch * world)
    return max(1, math.floor(full * PHASE_FRACTIONS[phase]))


def command(
    args: argparse.Namespace,
    dataset: str,
    scale: str,
    architecture: str,
    phase: str,
    lr: float,
    seed: int,
    mode: str = "train",
) -> list[str]:
    output = run_directory(
        Path(args.output_root), dataset, scale, architecture, phase, lr, seed
    )
    steps = phase_steps(Path(args.data_root) / dataset, args.local_batch, phase)
    return [
        "uv", "run", "--no-sync", "torchrun", "--standalone", "--nproc-per-node=8",
        "exp10/architecture_verdict.py",
        f"--mode={mode}",
        f"--architecture={architecture}",
        f"--scale={scale}",
        f"--seed={seed}",
        f"--lr={lr}",
        f"--phase={phase}",
        f"--steps={steps}",
        f"--local-batch={args.local_batch}",
        f"--eval-batch={args.eval_batch}",
        "--no-compile",
        f"--data-root={Path(args.data_root) / dataset}",
        f"--output-dir={output}",
        f"--run-name=exp10-{dataset}-{scale}-{architecture}-{phase}-lr{slug(lr)}-seed{seed}",
    ]


def launch(value: list[str], *, allow_failure: bool = False) -> bool:
    mode = next(item.split("=", 1)[1] for item in value if item.startswith("--mode="))
    output = Path(
        next(item.split("=", 1)[1] for item in value if item.startswith("--output-dir="))
    )
    result_path = output / "result.json"
    if result_path.is_file():
        result = json.loads(result_path.read_text())
        complete = (
            mode == "train"
            or mode == "evaluate" and "test" in result
            or mode == "benchmark" and "benchmark" in result
        )
        if complete:
            print(f"SKIP_COMPLETE mode={mode} output={output}", flush=True)
            return True
    completed = subprocess.run(
        value,
        check=False,
        env={**os.environ, "PYTHONUNBUFFERED": "1"},
    )
    if completed.returncode == 0:
        return True
    if not allow_failure:
        raise subprocess.CalledProcessError(completed.returncode, value)
    output.mkdir(parents=True, exist_ok=True)
    (output / "failure.json").write_text(
        json.dumps({"returncode": completed.returncode, "command": value}, indent=2)
    )
    print(f"SCREEN_FAILED returncode={completed.returncode} output={output}", flush=True)
    return False


def load_result(path: Path) -> dict:
    value = json.loads((path / "result.json").read_text())
    if value.get("schema") != SCHEMA or not value.get("wandb_url"):
        raise RuntimeError(f"invalid result at {path}")
    return value


def selected_lrs(
    args: argparse.Namespace,
    dataset: str,
    scale: str,
    architecture: str,
    phase: str,
    candidates: tuple[float, ...] | list[float],
    count: int,
) -> list[float]:
    rows = []
    for lr in candidates:
        path = run_directory(
            Path(args.output_root), dataset, scale, architecture, phase, lr, 0
        )
        result = load_result(path)
        rows.append((float(result["validation"]["nll"]), lr))
    return [lr for _, lr in sorted(rows)[:count]]


def campaign_pairs(args: argparse.Namespace) -> list[tuple[str, str]]:
    if args.stage == "tinystories":
        return [("tinystories", "small")]
    if args.stage == "wikitext":
        return [("wikitext", scale) for scale in SCALES]
    return [("tinystories", "small"), *(("wikitext", scale) for scale in SCALES)]


def plan(args: argparse.Namespace) -> dict:
    pairs = []
    for dataset, scale in campaign_pairs(args):
        pairs.append(
            {
                "dataset": dataset,
                "scale": scale,
                "screen_steps": phase_steps(Path(args.data_root) / dataset, args.local_batch, "screen"),
                "promotion_steps": phase_steps(Path(args.data_root) / dataset, args.local_batch, "promotion"),
                "final_steps": phase_steps(Path(args.data_root) / dataset, args.local_batch, "final"),
                "lr_grids": LR_GRIDS,
                "architectures": ARCHITECTURES,
                "final_seeds": FINAL_SEEDS,
            }
        )
    return {
        "schema": "exp10-campaign-plan-v1",
        "local_batch_per_gpu": args.local_batch,
        "world_size": 8,
        "global_token_batch": args.local_batch * 8 * 256,
        "pairs": pairs,
    }


def run_campaign(args: argparse.Namespace) -> None:
    if not os.environ.get("WANDB_API_KEY"):
        raise RuntimeError("WANDB_API_KEY must be present before campaign launch")
    assert_matched_presets()
    root = Path(args.output_root)
    root.mkdir(parents=True, exist_ok=True)
    (root / "campaign-plan.json").write_text(json.dumps(plan(args), indent=2))
    for dataset, scale in campaign_pairs(args):
        winners = {}
        for architecture in ARCHITECTURES:
            lr_grid = LR_GRIDS[architecture]
            for lr in lr_grid:
                launch(
                    command(args, dataset, scale, architecture, "screen", lr, 0),
                    allow_failure=True,
                )
            screened = tuple(
                lr
                for lr in lr_grid
                if (
                    run_directory(
                        Path(args.output_root),
                        dataset,
                        scale,
                        architecture,
                        "screen",
                        lr,
                        0,
                    )
                    / "result.json"
                ).is_file()
            )
            if len(screened) < PROMOTIONS:
                raise RuntimeError(f"fewer than two stable LR screens for {architecture}")
            promoted = selected_lrs(
                args, dataset, scale, architecture, "screen", screened, PROMOTIONS
            )
            for lr in promoted:
                launch(command(args, dataset, scale, architecture, "promotion", lr, 0))
            winners[architecture] = selected_lrs(
                args, dataset, scale, architecture, "promotion", promoted, 1
            )[0]
        for architecture in ARCHITECTURES:
            for seed in FINAL_SEEDS:
                launch(
                    command(
                        args,
                        dataset,
                        scale,
                        architecture,
                        "final",
                        winners[architecture],
                        seed,
                    )
                )
        # Test stays locked until every final recipe and seed exists.
        for architecture in ARCHITECTURES:
            for seed in FINAL_SEEDS:
                evaluation = command(
                    args,
                    dataset,
                    scale,
                    architecture,
                    "final",
                    winners[architecture],
                    seed,
                    mode="evaluate",
                )
                launch(evaluation)
                benchmark = command(
                    args,
                    dataset,
                    scale,
                    architecture,
                    "final",
                    winners[architecture],
                    seed,
                    mode="benchmark",
                )
                launch(benchmark)
        audited = audit(args)
        if dataset == "tinystories" and audited["tinystories_gate"] == "fail":
            raise RuntimeError(
                "TinyStories gate failed: Kronecker is worse in NLL and throughput"
            )


def mean(values: list[float]) -> float:
    return statistics.fmean(values)


def bootstrap_ci(values: list[float], seed: int = 10, draws: int = 20_000) -> list[float]:
    generator = np.random.default_rng(seed)
    array = np.asarray(values, dtype=np.float64)
    samples = generator.choice(array, size=(draws, len(array)), replace=True).mean(1)
    return [float(value) for value in np.quantile(samples, (0.025, 0.975))]


def audit(args: argparse.Namespace) -> dict:
    failures = []
    comparisons = {}
    root = Path(args.output_root)
    for dataset, scale in campaign_pairs(args):
        key = f"{dataset}-{scale}"
        architecture_rows = {}
        for architecture in ARCHITECTURES:
            expected_screens = [
                run_directory(
                    root, dataset, scale, architecture, "screen", lr, 0
                )
                for lr in LR_GRIDS[architecture]
            ]
            screens = [path / "result.json" for path in expected_screens if (path / "result.json").is_file()]
            screen_failures = [
                path / "failure.json"
                for path in expected_screens
                if (path / "failure.json").is_file()
            ]
            promotions = list((root / dataset / scale / architecture / "promotion").glob("**/result.json"))
            if len(screens) + len(screen_failures) != len(LR_GRIDS[architecture]):
                failures.append(
                    f"{key}/{architecture}: incomplete "
                    f"{len(LR_GRIDS[architecture])}-LR screen"
                )
            if len(promotions) != PROMOTIONS:
                failures.append(f"{key}/{architecture}: expected two promotions")
            promotion_values = [json.loads(path.read_text()) for path in promotions]
            if not promotion_values:
                continue
            winner = min(promotion_values, key=lambda value: value["validation"]["nll"])["lr"]
            finals = []
            for seed in FINAL_SEEDS:
                path = run_directory(root, dataset, scale, architecture, "final", winner, seed)
                if not (path / "result.json").is_file():
                    failures.append(f"{key}/{architecture}/seed{seed}: missing final")
                    continue
                value = load_result(path)
                if "test" not in value:
                    failures.append(f"{key}/{architecture}/seed{seed}: test still missing")
                if "benchmark" not in value:
                    failures.append(f"{key}/{architecture}/seed{seed}: benchmark missing")
                finals.append(value)
            if len(finals) == len(FINAL_SEEDS) and all(
                "test" in value and "benchmark" in value for value in finals
            ):
                architecture_rows[architecture] = {
                    "selected_lr": winner,
                    "parameters": finals[0]["inventory"]["total"],
                    "validation_nll": [value["validation"]["nll"] for value in finals],
                    "test_nll": [value.get("test", {}).get("nll") for value in finals],
                    "tokens_per_second": [
                        value["performance"].get(
                            "sustained_tokens_per_second",
                            value["performance"]["tokens_per_second"],
                        )
                        for value in finals
                    ],
                    "peak_allocated_gib": [value["performance"]["peak_allocated_gib"] for value in finals],
                    "decode_tokens_per_second": [
                        value.get("benchmark", {}).get("decode_tokens_per_second_batch1")
                        for value in finals
                    ],
                }
        if all(name in architecture_rows for name in ARCHITECTURES):
            kronecker = architecture_rows["kronecker"]
            transformer = architecture_rows["transformer"]
            deltas = [
                transformer["test_nll"][index] - kronecker["test_nll"][index]
                for index in range(3)
            ]
            comparisons[key] = {
                "architectures": architecture_rows,
                "paired_test_nll_transformer_minus_kronecker": deltas,
                "mean_test_nll_delta": mean(deltas),
                "paired_test_nll_delta_95pct_bootstrap_ci": bootstrap_ci(deltas),
                "kronecker_training_throughput_ratio": mean(kronecker["tokens_per_second"]) / mean(transformer["tokens_per_second"]),
                "kronecker_decode_throughput_ratio": mean(kronecker["decode_tokens_per_second"]) / mean(transformer["decode_tokens_per_second"]),
            }
    verdict = "incomplete"
    tiny = comparisons.get("tinystories-small")
    tinystories_gate = "pending"
    if tiny:
        tinystories_gate = (
            "fail"
            if tiny["mean_test_nll_delta"] < 0
            and tiny["kronecker_training_throughput_ratio"] <= 1.0
            else "pass"
        )
    wiki_small = comparisons.get("wikitext-small")
    wiki_large = comparisons.get("wikitext-large")
    if wiki_small and wiki_large:
        quality = wiki_large["mean_test_nll_delta"]
        train_ratio = wiki_large["kronecker_training_throughput_ratio"]
        decode_ratio = wiki_large["kronecker_decode_throughput_ratio"]
        memory_ratio = mean(
            wiki_large["architectures"]["kronecker"]["peak_allocated_gib"]
        ) / mean(wiki_large["architectures"]["transformer"]["peak_allocated_gib"])
        efficiency_win = train_ratio >= 1.2 or memory_ratio <= 1 / 1.2
        if decode_ratio >= 0.5 and (quality >= 0.03 or (quality >= -0.01 and efficiency_win)):
            verdict = "pursue_general_causal_lm"
        elif quality >= 0.03 or (quality >= -0.01 and efficiency_win):
            verdict = "pursue_fixed_window_mixer_only"
        elif (
            quality < 0
            and wiki_small["mean_test_nll_delta"] < 0
            and train_ratio <= 1.0
        ):
            verdict = "stop_pursuing"
        else:
            verdict = "ambiguous_requires_web_confirmation"
    result = {
        "schema": "exp10-campaign-audit-v1",
        "status": "failed" if failures else "complete",
        "parameter_presets": {f"{a}-{s}": PRESETS[(a, s)].__dict__ for a in ARCHITECTURES for s in SCALES},
        "comparisons": comparisons,
        "tinystories_gate": tinystories_gate,
        "verdict": verdict,
        "failures": failures,
    }
    root.mkdir(parents=True, exist_ok=True)
    (root / "comparison.json").write_text(json.dumps(result, indent=2, sort_keys=True))
    print(json.dumps(result, indent=2))
    if failures and args.mode == "audit":
        raise RuntimeError("Exp10 audit failed")
    return result


def parser() -> argparse.ArgumentParser:
    value = argparse.ArgumentParser()
    value.add_argument("--mode", choices=("plan", "run", "audit"), default="plan")
    value.add_argument("--data-root", default="/cache/exp10/data")
    value.add_argument("--output-root", default="/cache/exp10/runs")
    value.add_argument("--local-batch", type=int, default=1)
    value.add_argument("--eval-batch", type=int, default=512)
    value.add_argument(
        "--stage", choices=("tinystories", "wikitext", "all"), default="all"
    )
    return value


def main() -> None:
    args = parser().parse_args()
    if args.mode == "plan":
        print(json.dumps(plan(args), indent=2))
    elif args.mode == "run":
        run_campaign(args)
    else:
        audit(args)


if __name__ == "__main__":
    main()
