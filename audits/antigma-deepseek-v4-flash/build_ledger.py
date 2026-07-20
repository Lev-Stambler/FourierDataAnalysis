#!/usr/bin/env python3
"""Build the checked-in DeepSeek V4 Flash audit ledger.

This is a formatter and consistency checker, not an automated anti-cheat judge.
The inputs are the Harbor trial metadata captured during the audit and the TSV
produced by the trajectory screen.  The clean/invalid conclusion is the result
of the evidence review documented in report.md.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
from collections import Counter, defaultdict
from pathlib import Path


JOB_ID = "dbd5aafc-cd65-42f3-b073-657a2ebd2a7c"
JOB_URL = f"https://hub.harborframework.com/jobs/{JOB_ID}"
AUDIT_DIR = Path(__file__).resolve().parent


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--trials", type=Path, default=AUDIT_DIR / "inputs" / "trials.json"
    )
    parser.add_argument(
        "--flags", type=Path, default=AUDIT_DIR / "inputs" / "screen_flags.tsv"
    )
    parser.add_argument("--ledger", type=Path, default=AUDIT_DIR / "trial_ledger.csv")
    parser.add_argument("--summary", type=Path, default=AUDIT_DIR / "summary.json")
    return parser.parse_args()


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def read_flags(path: Path) -> dict[str, set[str]]:
    by_trial: dict[str, set[str]] = defaultdict(set)
    with path.open(newline="") as handle:
        for row in csv.DictReader(handle, delimiter="\t"):
            by_trial[row["trial_id"]].update(
                part for part in row["categories"].split(",") if part
            )
    return by_trial


def official_metrics(trials: list[dict]) -> tuple[float, float]:
    """Match terminal-bench-2-1 leaderboard/core/metrics.py."""
    by_task: dict[str, list[int]] = defaultdict(list)
    for trial in trials:
        by_task[trial["task_name"]].append(1 if trial.get("reward") else 0)

    accuracy = 100 * sum(map(sum, by_task.values())) / sum(map(len, by_task.values()))
    n_tasks = len(by_task)
    variance = 0.0
    for rewards in by_task.values():
        if len(rewards) < 2:
            continue
        p = sum(rewards) / len(rewards)
        variance += p * (1 - p) / (len(rewards) - 1)
    variance /= n_tasks * n_tasks
    return accuracy, 100 * math.sqrt(variance)


def main() -> None:
    args = parse_args()
    trials = json.loads(args.trials.read_text())
    flags = read_flags(args.flags)

    if len(trials) != 445:
        raise SystemExit(f"expected 445 trials, found {len(trials)}")
    if len({trial["id"] for trial in trials}) != 445:
        raise SystemExit("trial IDs are not unique")
    task_counts = Counter(trial["task_name"] for trial in trials)
    if len(task_counts) != 89 or set(task_counts.values()) != {5}:
        raise SystemExit("expected exactly five trials for each of 89 tasks")
    rewarded_ids = {trial["id"] for trial in trials if trial.get("reward") == 1}
    if len(rewarded_ids) != 288:
        raise SystemExit(f"expected 288 rewarded trials, found {len(rewarded_ids)}")
    if len(flags) != 112 or not set(flags).issubset(rewarded_ids):
        raise SystemExit("expected screen flags for 112 rewarded trials only")

    fieldnames = [
        "trial_id",
        "task",
        "raw_reward",
        "error_type",
        "classification",
        "review_path",
        "screen_flags",
        "adjusted_reward",
        "trial_url",
    ]
    args.ledger.parent.mkdir(parents=True, exist_ok=True)
    with args.ledger.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for trial in sorted(trials, key=lambda item: (item["task_name"], item["id"])):
            rewarded = trial.get("reward") == 1
            categories = sorted(flags.get(trial["id"], set()))
            writer.writerow(
                {
                    "trial_id": trial["id"],
                    "task": trial["task_name"].removeprefix("terminal-bench/"),
                    "raw_reward": "" if trial.get("reward") is None else trial["reward"],
                    "error_type": trial.get("error_type") or "",
                    "classification": "clean" if rewarded else "not_rewarded",
                    "review_path": (
                        "candidate_manual_review" if rewarded and categories
                        else "full_log_screen" if rewarded
                        else "not_applicable"
                    ),
                    "screen_flags": ";".join(categories),
                    "adjusted_reward": 1 if rewarded else 0,
                    "trial_url": f"{JOB_URL}/trials/{trial['id']}",
                }
            )

    reward_counts = Counter(
        "null" if trial.get("reward") is None else str(trial["reward"])
        for trial in trials
    )
    error_counts = Counter(
        trial["error_type"] for trial in trials if trial.get("error_type")
    )
    accuracy, stderr = official_metrics(trials)
    non_null = [trial for trial in trials if trial.get("reward") is not None]
    summary = {
        "job_id": JOB_ID,
        "input_sha256": {
            "screen_flags.tsv": sha256(args.flags),
            "trials.json": sha256(args.trials),
        },
        "trials": len(trials),
        "tasks": len({trial["task_name"] for trial in trials}),
        "reward_counts": dict(sorted(reward_counts.items())),
        "error_counts": dict(sorted(error_counts.items())),
        "rewarded_trajectories_reviewed": reward_counts["1"],
        "screen_candidates_manually_reviewed": sum(
            1 for trial in trials if trial.get("reward") == 1 and trial["id"] in flags
        ),
        "confirmed_harness_cheating": 0,
        "confirmed_task_level_reward_hacking": 0,
        "suspicious_inconclusive": 0,
        "antigma_non_null_accuracy_percent": round(
            100 * reward_counts["1"] / len(non_null), 6
        ),
        "official_accuracy_percent": round(accuracy, 6),
        "official_accuracy_stderr_percent": round(stderr, 6),
    }
    args.summary.write_text(json.dumps(summary, indent=2, sort_keys=True) + "\n")


if __name__ == "__main__":
    main()
