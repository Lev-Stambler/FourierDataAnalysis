from __future__ import annotations

from .config import ArchitectureConfig, TrialConfig, monarch_screen

LR_GRID = (3e-4, 1e-3, 3e-3)


def screen_trials() -> list[TrialConfig]:
    return [
        TrialConfig(config, lr=1e-3, seed=0, steps=1_000, stage="screen")
        for config in monarch_screen()
    ]


def select_topology(results: list[dict]) -> ArchitectureConfig:
    candidates = []
    for result in results:
        config = ArchitectureConfig(**result["architecture"])
        candidates.append(
            (
                float(result["validation"]["kl"]),
                int(result["trainable_parameters"]),
                config.depth,
                config.expansion,
                config.form,
                config,
            )
        )
    if not candidates:
        raise RuntimeError("screen produced no Monarch configurations")
    return min(candidates, key=lambda item: item[:-1])[-1]


def tuning_trials(selected: ArchitectureConfig) -> list[TrialConfig]:
    selected.validate()
    return [
        TrialConfig(selected, lr=lr, seed=0, steps=1_000, stage="tune")
        for lr in LR_GRID
        if lr != 1e-3
    ]


def select_lr(config: ArchitectureConfig, results: list[dict]) -> float:
    matching = []
    for result in results:
        candidate = ArchitectureConfig(**result["architecture"])
        if candidate == config:
            matching.append(
                (float(result["validation"]["kl"]), float(result["lr"]))
            )
    if not matching:
        raise RuntimeError(f"no learning-rate results for {config.label}")
    return min(matching)[1]


def final_trials(
    screen_results: list[dict],
    tune_results: list[dict],
) -> list[TrialConfig]:
    selected = select_topology(screen_results)
    lr = select_lr(selected, screen_results + tune_results)
    return [
        TrialConfig(
            selected,
            lr=lr,
            seed=seed,
            steps=4_000,
            stage="final",
            audit_every=250,
        )
        for seed in (0, 1, 2)
    ]
