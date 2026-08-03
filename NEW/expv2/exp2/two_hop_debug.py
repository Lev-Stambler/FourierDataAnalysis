"""Cloud-only component and curriculum ladder for two-hop recall."""

from __future__ import annotations

import gc
import os
import time
from pathlib import Path
from typing import Any, Literal

import torch

from expv2.exp1.model import build_model, model_inventory
from expv2.exp1.utils import atomic_json, finite_tree, seed_everything

from .budget import CalibrationBudget
from .campaign import Clock
from .config import ID_ACCURACY_THRESHOLD
from .preflight import paid_preflight
from .synthetic import SyntheticBatch, make_batch, masked_loss_and_accuracy
from .training import CalibrationRecipe, _optimizer


DebugKind = Literal["first-edge", "second-edge", "marked-two-hop", "two-hop"]
EDGE_ONE_MARKER = 1
EDGE_TWO_MARKER = 2
QUERY_MARKER = 3
DEBUG_LR = 3e-4
WANDB_PROJECT = "expv2-2-two-hop-debug"


def debug_batch(
    kind: DebugKind,
    batch_size: int,
    *,
    seed: int,
    split: str = "sanity",
    device: torch.device | str = "cpu",
) -> SyntheticBatch:
    base = make_batch(
        "two-hop-recall", batch_size, seed=seed, split=split, device=device
    )
    inputs = base.inputs.clone()
    targets = base.targets.clone()
    rows = torch.arange(batch_size, device=inputs.device)[:, None]
    first_slots = base.support_positions[:, 0]
    second_slots = base.support_positions[:, 1]
    first_keys = inputs[rows, first_slots]
    middles = inputs[rows, first_slots + 1]
    second_keys = inputs[rows, second_slots]
    finals = inputs[rows, second_slots + 1]
    query_key = inputs[rows, base.query_positions]
    first_matches = query_key[:, :, None] == first_keys[:, None, :]
    selected_middle = (first_matches.long() * middles[:, None, :]).sum(-1)
    second_matches = selected_middle[:, :, None] == second_keys[:, None, :]
    selected_final = (second_matches.long() * finals[:, None, :]).sum(-1)

    candidates: torch.Tensor
    if kind == "first-edge":
        targets[rows, base.query_positions] = selected_middle
        candidates = middles
    elif kind == "second-edge":
        inputs[rows, base.query_positions] = selected_middle
        targets[rows, base.query_positions] = selected_final
        candidates = finals
    elif kind == "marked-two-hop":
        inputs[rows, first_slots - 1] = EDGE_ONE_MARKER
        inputs[rows, second_slots - 1] = EDGE_TWO_MARKER
        inputs[rows, base.query_positions - 1] = QUERY_MARKER
        candidates = finals
    elif kind == "two-hop":
        candidates = finals
    else:
        raise ValueError(f"unknown two-hop diagnostic kind: {kind}")
    result = SyntheticBatch(
        inputs=inputs,
        targets=targets,
        mask=base.mask,
        query_positions=base.query_positions,
        support_positions=base.support_positions,
        candidate_values=candidates,
        cardinality=base.cardinality,
    )
    result.validate()
    return result


def mixed_debug_batch(
    kinds: tuple[DebugKind, ...],
    batch_size: int,
    *,
    seed: int,
    split: str = "sanity",
    device: torch.device | str = "cpu",
) -> SyntheticBatch:
    """Pack multiple supervised objectives into one physical GPU batch."""

    if not kinds or batch_size < len(kinds):
        raise ValueError("mixed batch requires at least one context per kind")
    base = batch_size // len(kinds)
    counts = [base] * len(kinds)
    counts[-1] += batch_size - sum(counts)
    parts = [
        debug_batch(kind, count, seed=seed, split=split, device=device)
        for kind, count in zip(kinds, counts, strict=True)
    ]
    if any(part.candidate_values is None for part in parts):
        raise RuntimeError("all mixed tasks require explicit candidate values")
    result = SyntheticBatch(
        inputs=torch.cat([part.inputs for part in parts]),
        targets=torch.cat([part.targets for part in parts]),
        mask=torch.cat([part.mask for part in parts]),
        query_positions=torch.cat([part.query_positions for part in parts]),
        support_positions=torch.cat([part.support_positions for part in parts]),
        candidate_values=torch.cat(
            [part.candidate_values for part in parts if part.candidate_values is not None]
        ),
        cardinality=(
            parts[0].cardinality
            if all(part.cardinality == parts[0].cardinality for part in parts)
            else None
        ),
    )
    result.validate()
    return result


def local_debug_audit(output: str | Path | None = None) -> dict[str, Any]:
    failures: list[str] = []
    rows: dict[str, Any] = {}
    for index, kind in enumerate(
        ("first-edge", "second-edge", "marked-two-hop", "two-hop")
    ):
        left = debug_batch(kind, 4_096, seed=800 + index)
        right = debug_batch(kind, 4_096, seed=800 + index)
        query_rows = torch.arange(left.inputs.shape[0])[:, None]
        deterministic = all(
            torch.equal(getattr(left, field), getattr(right, field))
            for field in ("inputs", "targets", "mask")
        )
        candidates = left.candidate_values
        assert candidates is not None
        target = left.targets[query_rows, left.query_positions]
        oracle_membership = bool((target[:, :, None] == candidates[:, None, :]).any(-1).all())
        if not deterministic:
            failures.append(f"{kind} is nondeterministic")
        if not oracle_membership:
            failures.append(f"{kind} target is not a recorded candidate")
        rows[kind] = {
            "deterministic": deterministic,
            "oracle_candidate_membership": oracle_membership,
            "targets_per_context": int(left.mask.sum(1).unique().item()),
            "analytical_visible_candidate_shortcut": 0.5,
        }
    marked = debug_batch("marked-two-hop", 512, seed=99)
    observed = set(marked.inputs.unique().tolist())
    marker_coverage = {EDGE_ONE_MARKER, EDGE_TWO_MARKER, QUERY_MARKER} <= observed
    if not marker_coverage:
        failures.append("marked task lacks all three role markers")
    result = {
        "schema": "expv2-2-two-hop-debug-audit-v1",
        "status": "pass" if not failures else "fail",
        "rows": rows,
        "markers": {
            "edge_one": EDGE_ONE_MARKER,
            "edge_two": EDGE_TWO_MARKER,
            "query": QUERY_MARKER,
            "all_observed": marker_coverage,
        },
        "failures": failures,
    }
    if output is not None:
        atomic_json(output, result)
    return result


@torch.inference_mode()
def _evaluate(
    model: torch.nn.Module,
    kind: DebugKind,
    *,
    device: torch.device,
    split: str = "sanity",
    examples: int = 4_096,
    batch_contexts: int = 512,
) -> dict[str, Any]:
    model.eval()
    total_loss = 0.0
    correct = 0.0
    total = 0
    shortcut_correct = 0.0
    for start in range(0, examples, batch_contexts):
        current = min(batch_contexts, examples - start)
        batch = debug_batch(
            kind,
            current,
            seed=91_000 + start,
            split=split,
            device=device,
        )
        loss, accuracy = masked_loss_and_accuracy(
            model(batch.inputs), batch.targets, batch.mask
        )
        count = int(batch.mask.sum())
        total_loss += float(loss) * count
        correct += float(accuracy) * count
        total += count
        shortcut_correct += count / float(batch.cardinality or 2)
    return {
        "kind": kind,
        "loss": total_loss / total,
        "accuracy": correct / total,
        "targets": total,
        "visible_candidate_shortcut": shortcut_correct / total,
        "split": split,
    }


def _train_phase(
    model: torch.nn.Module,
    optimizer: torch.optim.Optimizer,
    *,
    kind: DebugKind,
    label: str,
    budget: CalibrationBudget,
    device: torch.device,
    run: Any,
) -> dict[str, Any]:
    model.train()
    finite = True
    torch.cuda.reset_peak_memory_stats(device)
    torch.cuda.synchronize(device)
    started = time.monotonic()
    for update in range(budget.actual_updates):
        batch = debug_batch(
            kind, budget.batch_contexts, seed=700_000 + update, device=device
        )
        optimizer.zero_grad(set_to_none=True)
        loss, accuracy = masked_loss_and_accuracy(
            model(batch.inputs), batch.targets, batch.mask
        )
        loss.backward()
        gradient_norm = torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        optimizer.step()
        finite = finite and bool(torch.isfinite(loss)) and bool(
            torch.isfinite(gradient_norm)
        )
        run.log(
            {
                f"debug/{label}/loss": float(loss),
                f"debug/{label}/accuracy": float(accuracy),
                f"debug/{label}/gradient_norm": float(gradient_norm),
                f"debug/{label}/optimizer_update": update + 1,
                f"debug/{label}/tokens_seen": (
                    (update + 1) * budget.batch_contexts * 128
                ),
                "global_context_batch": budget.batch_contexts,
                "global_token_batch": budget.batch_contexts * 128,
            }
        )
    torch.cuda.synchronize(device)
    elapsed = time.monotonic() - started
    evaluation = _evaluate(model, kind, device=device)
    result = {
        "status": "complete" if finite and finite_tree(optimizer.state) else "failed",
        "kind": kind,
        "label": label,
        "budget": budget.as_dict(),
        "elapsed_seconds": elapsed,
        "tokens_per_second": budget.training_tokens / elapsed,
        "finite_training_state": finite and finite_tree(optimizer.state),
        "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
        "evaluation": evaluation,
    }
    run.log(
        {
            f"evaluation/{label}/accuracy": evaluation["accuracy"],
            f"evaluation/{label}/loss": evaluation["loss"],
        }
    )
    return result


def _train_mixed_phase(
    model: torch.nn.Module,
    optimizer: torch.optim.Optimizer,
    *,
    kinds: tuple[DebugKind, ...],
    label: str,
    budget: CalibrationBudget,
    device: torch.device,
    run: Any,
    train_split: str = "sanity",
    probe_split: str = "sanity",
) -> dict[str, Any]:
    model.train()
    finite = True
    torch.cuda.reset_peak_memory_stats(device)
    torch.cuda.synchronize(device)
    started = time.monotonic()
    for update in range(budget.actual_updates):
        batch = mixed_debug_batch(
            kinds,
            budget.batch_contexts,
            seed=1_700_000 + update,
            split=train_split,
            device=device,
        )
        optimizer.zero_grad(set_to_none=True)
        loss, accuracy = masked_loss_and_accuracy(
            model(batch.inputs), batch.targets, batch.mask
        )
        loss.backward()
        gradient_norm = torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        optimizer.step()
        finite = finite and bool(torch.isfinite(loss)) and bool(
            torch.isfinite(gradient_norm)
        )
        run.log(
            {
                f"joint/{label}/loss": float(loss),
                f"joint/{label}/aggregate_accuracy": float(accuracy),
                f"joint/{label}/gradient_norm": float(gradient_norm),
                f"joint/{label}/optimizer_update": update + 1,
                f"joint/{label}/tokens_seen": (
                    (update + 1) * budget.batch_contexts * 128
                ),
                "global_context_batch": budget.batch_contexts,
                "global_token_batch": budget.batch_contexts * 128,
            }
        )
    torch.cuda.synchronize(device)
    elapsed = time.monotonic() - started
    probes = {
        kind: _evaluate(model, kind, device=device, split=probe_split)
        for kind in ("first-edge", "second-edge", "two-hop")
    }
    for kind, probe in probes.items():
        run.log(
            {
                f"joint-evaluation/{label}/{kind}/accuracy": probe["accuracy"],
                f"joint-evaluation/{label}/{kind}/loss": probe["loss"],
            }
        )
    return {
        "status": "complete" if finite and finite_tree(optimizer.state) else "failed",
        "label": label,
        "training_kinds": kinds,
        "budget": budget.as_dict(),
        "elapsed_seconds": elapsed,
        "tokens_per_second": budget.training_tokens / elapsed,
        "finite_training_state": finite and finite_tree(optimizer.state),
        "peak_allocated_gib": torch.cuda.max_memory_allocated(device) / 2**30,
        "probes": probes,
    }


def _fresh_cell(
    kind: DebugKind,
    *,
    label: str,
    batch_contexts: int,
    output: Path,
    run: Any,
    clock: Clock,
    measured_tokens_per_second: float,
) -> dict[str, Any]:
    budget = CalibrationBudget(batch_contexts=batch_contexts)
    clock.require(
        budget.training_tokens / measured_tokens_per_second * 1.35 + 10, label
    )
    seed_everything(0)
    device = torch.device("cuda")
    model = build_model("transformer", vocab_size=128).to(
        device=device, dtype=torch.bfloat16
    )
    optimizer, routing = _optimizer(model, CalibrationRecipe("adamw", DEBUG_LR))
    result = _train_phase(
        model,
        optimizer,
        kind=kind,
        label=label,
        budget=budget,
        device=device,
        run=run,
    )
    result.update({"optimizer_routing": routing, "inventory": model_inventory(model)})
    atomic_json(output, result)
    del model, optimizer
    gc.collect()
    torch.cuda.empty_cache()
    return result


def _curriculum(
    *,
    batch_contexts: int,
    output: Path,
    run: Any,
    clock: Clock,
    measured_tokens_per_second: float,
) -> dict[str, Any]:
    seed_everything(0)
    device = torch.device("cuda")
    model = build_model("transformer", vocab_size=128).to(
        device=device, dtype=torch.bfloat16
    )
    optimizer, routing = _optimizer(model, CalibrationRecipe("adamw", DEBUG_LR))
    budget = CalibrationBudget(batch_contexts=batch_contexts)
    phases: list[dict[str, Any]] = []
    for kind in ("first-edge", "second-edge", "two-hop"):
        label = f"curriculum-{kind}"
        clock.require(
            budget.training_tokens / measured_tokens_per_second * 1.35 + 10,
            label,
        )
        row = _train_phase(
            model,
            optimizer,
            kind=kind,  # type: ignore[arg-type]
            label=label,
            budget=budget,
            device=device,
            run=run,
        )
        row["unmarked_two_hop_probe"] = _evaluate(model, "two-hop", device=device)
        phases.append(row)
        atomic_json(
            output,
            {
                "schema": "expv2-2-two-hop-curriculum-v1",
                "status": "running",
                "optimizer_routing": routing,
                "inventory": model_inventory(model),
                "phases": phases,
            },
        )
    result = {
        "schema": "expv2-2-two-hop-curriculum-v1",
        "status": "complete",
        "optimizer_routing": routing,
        "inventory": model_inventory(model),
        "phases": phases,
        "final_unmarked_two_hop": phases[-1]["unmarked_two_hop_probe"],
    }
    atomic_json(output, result)
    del model, optimizer
    gc.collect()
    torch.cuda.empty_cache()
    return result


def run_two_hop_debug(
    *,
    output_root: str | Path,
    result_path: str | Path,
    preflight_path: str | Path | None = None,
) -> dict[str, Any]:
    output = Path(output_root)
    output.mkdir(parents=True, exist_ok=True)
    key = os.environ.get("WANDB_API_KEY")
    if not key:
        raise RuntimeError("WANDB_API_KEY is required before paid debugging")
    import wandb

    wandb.login(key=key, relogin=True, verify=True)
    run = wandb.init(
        project=WANDB_PROJECT,
        name="expv2-2-transformer-two-hop-ladder",
        job_type="positive-control-debug",
        dir=str(output),
        config={
            "optimizer": "adamw",
            "lr": DEBUG_LR,
            "updates_per_phase": 1_000,
            "cloud_only_training": True,
        },
    )
    wandb_url = str(run.url or run.get_url() or "")
    if not wandb_url.startswith("http"):
        run.finish(exit_code=1)
        raise RuntimeError("W&B did not provide a direct run URL")
    atomic_json(
        output / "wandb-launch.json",
        {
            "schema": "expv2-2-wandb-launch-v1",
            "status": "complete",
            "wandb_url": wandb_url,
        },
    )
    clock = Clock()
    try:
        preflight = paid_preflight(
            preflight_path or output / "paid-preflight.json", wandb_url=wandb_url
        )
        if preflight["status"] != "pass":
            raise RuntimeError("paid preflight failed: " + "; ".join(preflight["failures"]))
        batch_contexts = int(preflight["selected"]["batch_contexts"])
        throughput = float(preflight["selected"]["tokens_per_second"])
        run.config.update(
            {
                "global_context_batch": batch_contexts,
                "global_token_batch": batch_contexts * 128,
                "gradient_accumulation_steps": 1,
                "measured_tokens_per_second": throughput,
            },
            allow_val_change=True,
        )
        components = {
            kind: _fresh_cell(
                kind,  # type: ignore[arg-type]
                label=f"fresh-{kind}",
                batch_contexts=batch_contexts,
                output=output / f"fresh-{kind}.json",
                run=run,
                clock=clock,
                measured_tokens_per_second=throughput,
            )
            for kind in ("first-edge", "second-edge")
        }
        primitives_pass = all(
            row["evaluation"]["accuracy"] >= ID_ACCURACY_THRESHOLD
            for row in components.values()
        )
        marked: dict[str, Any] | None = None
        curriculum: dict[str, Any] | None = None
        if primitives_pass:
            marked = _fresh_cell(
                "marked-two-hop",
                label="fresh-marked-two-hop",
                batch_contexts=batch_contexts,
                output=output / "fresh-marked-two-hop.json",
                run=run,
                clock=clock,
                measured_tokens_per_second=throughput,
            )
            curriculum = _curriculum(
                batch_contexts=batch_contexts,
                output=output / "curriculum.json",
                run=run,
                clock=clock,
                measured_tokens_per_second=throughput,
            )
        curriculum_accuracy = (
            curriculum["final_unmarked_two_hop"]["accuracy"] if curriculum else 0.0
        )
        marked_accuracy = marked["evaluation"]["accuracy"] if marked else 0.0
        verdict = (
            "primitive_failed"
            if not primitives_pass
            else "curriculum_unlocks_composition"
            if curriculum_accuracy >= ID_ACCURACY_THRESHOLD
            else "markers_unlock_composition"
            if marked_accuracy >= ID_ACCURACY_THRESHOLD
            else "transformer_composition_failed"
        )
        result = {
            "schema": "expv2-2-two-hop-debug-result-v1",
            "status": "complete",
            "wandb_url": wandb_url,
            "verdict": verdict,
            "preflight": preflight,
            "components": components,
            "marked_two_hop": marked,
            "curriculum": curriculum,
            "elapsed_seconds": clock.elapsed,
        }
        atomic_json(result_path, result)
        run.summary["verdict"] = verdict
        run.summary["primitives_pass"] = primitives_pass
        run.summary["marked_two_hop_accuracy"] = marked_accuracy
        run.summary["curriculum_two_hop_accuracy"] = curriculum_accuracy
        return result
    except Exception as error:
        atomic_json(
            result_path,
            {
                "schema": "expv2-2-two-hop-debug-result-v1",
                "status": "failed",
                "wandb_url": wandb_url,
                "verdict": "infrastructure_or_budget_failure",
                "reason": repr(error),
                "elapsed_seconds": clock.elapsed,
            },
        )
        raise
    finally:
        run.finish()


def run_joint_two_hop(
    *,
    output_root: str | Path,
    result_path: str | Path,
    preflight_path: str | Path | None = None,
) -> dict[str, Any]:
    """Train both edges concurrently, add composition, then fine-tune it."""

    output = Path(output_root)
    output.mkdir(parents=True, exist_ok=True)
    key = os.environ.get("WANDB_API_KEY")
    if not key:
        raise RuntimeError("WANDB_API_KEY is required before paid debugging")
    import wandb

    wandb.login(key=key, relogin=True, verify=True)
    run = wandb.init(
        project=WANDB_PROJECT,
        name="expv2-2-transformer-joint-auxiliary-curriculum",
        job_type="positive-control-debug",
        dir=str(output),
        config={
            "optimizer": "adamw",
            "lr": DEBUG_LR,
            "updates_per_phase": 1_000,
            "cloud_only_training": True,
            "phase_order": [
                "concurrent-edges",
                "concurrent-edges-and-two-hop",
                "two-hop-finetune",
            ],
        },
    )
    wandb_url = str(run.url or run.get_url() or "")
    if not wandb_url.startswith("http"):
        run.finish(exit_code=1)
        raise RuntimeError("W&B did not provide a direct run URL")
    atomic_json(
        output / "wandb-launch.json",
        {
            "schema": "expv2-2-wandb-launch-v1",
            "status": "complete",
            "wandb_url": wandb_url,
        },
    )
    clock = Clock()
    try:
        preflight = paid_preflight(
            preflight_path or output / "paid-preflight.json", wandb_url=wandb_url
        )
        if preflight["status"] != "pass":
            raise RuntimeError("paid preflight failed: " + "; ".join(preflight["failures"]))
        batch_contexts = int(preflight["selected"]["batch_contexts"])
        throughput = float(preflight["selected"]["tokens_per_second"])
        run.config.update(
            {
                "global_context_batch": batch_contexts,
                "global_token_batch": batch_contexts * 128,
                "gradient_accumulation_steps": 1,
                "measured_tokens_per_second": throughput,
            },
            allow_val_change=True,
        )
        seed_everything(0)
        device = torch.device("cuda")
        model = build_model("transformer", vocab_size=128).to(
            device=device, dtype=torch.bfloat16
        )
        optimizer, routing = _optimizer(model, CalibrationRecipe("adamw", DEBUG_LR))
        budget = CalibrationBudget(batch_contexts=batch_contexts)
        phases: list[dict[str, Any]] = []
        schedule: tuple[tuple[str, tuple[DebugKind, ...]], ...] = (
            ("concurrent-edges", ("first-edge", "second-edge")),
            (
                "concurrent-edges-and-two-hop",
                ("first-edge", "second-edge", "two-hop"),
            ),
            ("two-hop-finetune", ("two-hop",)),
        )
        for label, kinds in schedule:
            clock.require(
                budget.training_tokens / throughput * 1.35 + 10,
                label,
            )
            row = _train_mixed_phase(
                model,
                optimizer,
                kinds=kinds,
                label=label,
                budget=budget,
                device=device,
                run=run,
            )
            phases.append(row)
            atomic_json(
                output / "joint-curriculum.json",
                {
                    "schema": "expv2-2-joint-two-hop-curriculum-v1",
                    "status": "running",
                    "optimizer_routing": routing,
                    "inventory": model_inventory(model),
                    "phases": phases,
                },
            )
        final_accuracy = phases[-1]["probes"]["two-hop"]["accuracy"]
        verdict = (
            "joint_auxiliary_unlocks_composition"
            if final_accuracy >= ID_ACCURACY_THRESHOLD
            else "joint_auxiliary_failed_composition"
        )
        result = {
            "schema": "expv2-2-joint-two-hop-result-v1",
            "status": "complete",
            "wandb_url": wandb_url,
            "verdict": verdict,
            "preflight": preflight,
            "optimizer_routing": routing,
            "inventory": model_inventory(model),
            "phases": phases,
            "elapsed_seconds": clock.elapsed,
        }
        atomic_json(output / "joint-curriculum.json", result)
        atomic_json(result_path, result)
        run.summary["verdict"] = verdict
        run.summary["final_two_hop_accuracy"] = final_accuracy
        del model, optimizer
        gc.collect()
        torch.cuda.empty_cache()
        return result
    except Exception as error:
        atomic_json(
            result_path,
            {
                "schema": "expv2-2-joint-two-hop-result-v1",
                "status": "failed",
                "wandb_url": wandb_url,
                "verdict": "infrastructure_or_budget_failure",
                "reason": repr(error),
                "elapsed_seconds": clock.elapsed,
            },
        )
        raise
    finally:
        run.finish()
