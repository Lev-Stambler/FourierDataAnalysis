"""Resumable Modal runner for Fourier-character CE spectrum matching."""

from __future__ import annotations

import gzip
import hashlib
import json
import sys
import time
from pathlib import Path

import modal

LOCAL_ROOT = Path(__file__).parent.parent
IS_REMOTE = str(Path(__file__).resolve()).startswith("/root/")
ROOT = Path("/root/pkg") if IS_REMOTE else LOCAL_ROOT
sys.path.insert(0, str(ROOT))

from dlx.analysis.character_response import enumerate_supports
from dlx.protocol.frozen import (
    file_sha256,
    load_frozen_protocol,
    verify_hash_lock,
    write_hash_once,
    write_json_once,
)

OUT = ROOT / "runs/local/v30_architecture_spectrum"
PROTOCOL = load_frozen_protocol(ROOT / "configs/protocol_v3.1.json")
MANIFEST = (
    {"corpora": []}
    if IS_REMOTE
    else json.loads((OUT / "data_manifest.json").read_text())
)
CORPORA = MANIFEST["corpora"]


def _ignore(path: Path) -> bool:
    return any(
        part in str(path)
        for part in (
            "__pycache__",
            ".venv",
            ".pytest_cache",
            ".ruff_cache",
            "runs/",
            "dlx/data_cache/",
        )
    )


app = modal.App("dlx-v31-fourier-ce-spectrum")
image = (
    modal.Image.debian_slim(python_version="3.12")
    .pip_install("numpy>=1.26", "torch>=2.3")
    .add_local_dir(ROOT, "/root/pkg", ignore=_ignore)
)
if not IS_REMOTE:
    for row in CORPORA:
        image = image.add_local_file(ROOT / row["path"], f"/data/{row['dataset']}.npy")


def _seed(*parts: object) -> int:
    from dlx.seeding import seed_from

    return int(seed_from(*parts) % (2**32))


def _split_seed(row: dict) -> int:
    return _seed(
        PROTOCOL["protocol_hash"],
        row["byte_stream_sha256"],
        PROTOCOL["sampling"]["split_seed"],
    )


def _architecture(identifier: str) -> dict:
    return next(row for row in PROTOCOL["architectures"] if row["id"] == identifier)


def _model_config(
    architecture: dict, *, vocab: int, ctx_len: int, attention_window: int
):
    from dlx.learners.transformer import TransformerConfig

    training = PROTOCOL["natural_training"]
    return TransformerConfig(
        vocab=vocab,
        ctx_len=ctx_len,
        d_model=training["d_model"],
        n_layers=training["n_layers"],
        n_heads=training["n_heads"],
        mlp_mult=training["mlp_mult"],
        dropout=training["dropout"],
        tie_weights=training["tie_weights"],
        lr=training["lr"],
        weight_decay=training["weight_decay"],
        grad_clip=training["grad_clip"],
        position_encoding=architecture["position_encoding"],
        rope_base=float(architecture.get("rope_base", 10_000.0)),
        attention_window=attention_window,
    )


@app.function(
    image=image,
    gpu="H100",
    cpu=8.0,
    memory=16384,
    timeout=1800,
    max_containers=1,
    block_network=True,
)
def character_train_cell(payload: dict) -> dict:
    import math

    import numpy as np
    import torch

    from dlx.analysis.floor_independent import curve_metrics
    from dlx.learners.transformer import CausalTransformer

    architecture = payload["architecture"]
    support = tuple(int(value) for value in payload["support"])
    seed = int(payload["seed"])
    protocol = payload["protocol"]
    spec = protocol["fourier_character_training"]
    torch.manual_seed(seed)
    np.random.seed(seed)
    model = CausalTransformer(
        _model_config(
            architecture,
            vocab=2,
            ctx_len=spec["context"],
            attention_window=spec["context"],
        )
    ).cuda()
    train_rng = np.random.default_rng(
        _seed(
            spec["data_seed_namespace"],
            architecture["id"],
            support,
            seed,
            "char_train",
        )
    )
    val_rng = np.random.default_rng(
        _seed(spec["data_seed_namespace"], support, seed, "char_validation")
    )

    def sample(rng: np.random.Generator, count: int) -> tuple[np.ndarray, np.ndarray]:
        x = rng.integers(0, 2, size=(count, spec["context"]), dtype=np.int64)
        y = np.zeros(count, dtype=np.int64)
        for lag in support:
            y ^= x[:, -lag]
        return x, y

    val_x, val_y = sample(val_rng, spec["validation_examples"])

    @torch.no_grad()
    def validate() -> float:
        model.eval()
        total = 0.0
        for start in range(0, len(val_x), 256):
            x = torch.from_numpy(val_x[start : start + 256]).cuda()
            y = torch.from_numpy(val_y[start : start + 256]).cuda()
            logits = model(x)[:, -1, :]
            total += float(torch.nn.functional.cross_entropy(logits, y).item()) * len(x)
        model.train()
        return total / len(val_x) / math.log(2.0)

    steps = spec["examples"] // spec["batch_examples"]
    every = max(1, steps // spec["checkpoints"])
    optimizer = torch.optim.AdamW(
        model.parameters(),
        lr=model.cfg.lr,
        weight_decay=model.cfg.weight_decay,
    )
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=steps, eta_min=model.cfg.lr * 0.1
    )
    example_grid = [0]
    curve = [validate()]
    started = time.monotonic()
    for step in range(1, steps + 1):
        x_np, y_np = sample(train_rng, spec["batch_examples"])
        x = torch.from_numpy(x_np).cuda()
        y = torch.from_numpy(y_np).cuda()
        logits = model(x)[:, -1, :]
        loss = torch.nn.functional.cross_entropy(logits, y)
        optimizer.zero_grad(set_to_none=True)
        loss.backward()
        torch.nn.utils.clip_grad_norm_(model.parameters(), model.cfg.grad_clip)
        optimizer.step()
        scheduler.step()
        if step % every == 0 or step == steps:
            example_grid.append(step * spec["batch_examples"])
            curve.append(validate())
    summary = curve_metrics(example_grid, curve, curve[0])
    support_id = "-".join(str(value) for value in support)
    return {
        "cell_id": f"V31F/{architecture['id']}/A{support_id}/s{seed}",
        "protocol_hash": protocol["protocol_hash"],
        "data_seed_namespace": spec["data_seed_namespace"],
        "architecture": architecture["id"],
        "support": list(support),
        "degree": len(support),
        "radius": max(support),
        "seed": seed,
        "config_hash": model.cfg.config_hash,
        "example_grid": example_grid,
        "val_ce_bits": curve,
        "character_hardness": summary["normalized_curve_area"],
        "floor_independent": summary,
        "remote": {
            "gpu": torch.cuda.get_device_name(0),
            "wallclock_seconds": time.monotonic() - started,
        },
    }


@app.function(
    image=image,
    cpu=4.0,
    memory=16384,
    timeout=7200,
    max_containers=8,
    block_network=True,
)
def profile_cell(payload: dict) -> dict:
    import numpy as np

    from dlx.data.random_windows import RandomWindowCorpus
    from dlx.profiles.sampled_degree import (
        marginal_locality_features,
        marginal_support_energy,
        sampled_nested_degree_profile,
    )
    from dlx.profiles.simple_controls import blockwise_simple_text_controls

    protocol = payload["protocol"]
    row = payload["corpus"]
    spec = protocol["profile"]
    values = np.load(f"/data/{row['dataset']}.npy", mmap_mode="r")
    corpus = RandomWindowCorpus(
        values,
        ctx_len=protocol["natural_training"]["sequence_len"],
        block_size=protocol["sampling"]["block_size"],
        split_seed=_seed(
            protocol["protocol_hash"],
            row["byte_stream_sha256"],
            protocol["sampling"]["split_seed"],
        ),
        split_fractions=tuple(protocol["sampling"]["split_fractions"]),
    )
    rng = np.random.default_rng(
        _seed(protocol["protocol_hash"], row["byte_stream_sha256"], "profile_positions")
    )
    starts = corpus.sample_starts("profile", spec["positions"], rng, replace=False)
    targets = starts + max(spec["lags"])
    contexts = np.column_stack([values[targets - lag] for lag in spec["lags"]])
    started = time.monotonic()
    profile = sampled_nested_degree_profile(
        contexts,
        values[targets],
        max_degree=spec["max_degree"],
        n_chains=spec["chains"],
        seed=spec["seed"],
        delta=spec["delta"],
        q=spec["q"],
        coordinate_radii=tuple(spec["lags"]),
        include_chains=True,
    )
    marginal = marginal_locality_features(
        profile["chains"], tuple(spec["lags"]), feature_degree=spec["max_degree"]
    )
    support_energy = marginal_support_energy(
        profile["chains"], tuple(spec["lags"]), feature_degree=spec["max_degree"]
    )
    controls = blockwise_simple_text_controls(
        values,
        corpus.block_ranges["profile"],
        q=spec["q"],
        seed=_seed(protocol["protocol_hash"], row["dataset"], "controls"),
    )
    return {
        "dataset": row["dataset"],
        "panel": row["panel"],
        "stratum": row["stratum"],
        "protocol_hash": protocol["protocol_hash"],
        "data_sha256": row["byte_stream_sha256"],
        "split": corpus.summary(),
        "sampled_position_sha256": hashlib.sha256(starts.tobytes()).hexdigest(),
        "features": {**profile["sampled_features"], **marginal, **controls},
        "support_energy": support_energy,
        "remote_wallclock_seconds": time.monotonic() - started,
        **profile,
    }


@app.function(
    image=image,
    gpu="H100",
    cpu=8.0,
    memory=16384,
    timeout=1800,
    max_containers=1,
    block_network=True,
)
def natural_train_cell(payload: dict) -> dict:
    import numpy as np
    import torch

    from dlx.analysis.floor_independent import curve_metrics
    from dlx.data.random_windows import RandomWindowCorpus
    from dlx.learners.transformer import CausalTransformer, cross_entropy_bits

    protocol = payload["protocol"]
    row = payload["corpus"]
    architecture = payload["architecture"]
    seed = int(payload["seed"])
    spec = protocol["natural_training"]
    values = np.load(f"/data/{row['dataset']}.npy", mmap_mode="r")
    corpus = RandomWindowCorpus(
        values,
        ctx_len=spec["sequence_len"],
        block_size=protocol["sampling"]["block_size"],
        split_seed=_seed(
            protocol["protocol_hash"],
            row["byte_stream_sha256"],
            protocol["sampling"]["split_seed"],
        ),
        split_fractions=tuple(protocol["sampling"]["split_fractions"]),
    )
    torch.manual_seed(seed)
    np.random.seed(seed)
    model = CausalTransformer(
        _model_config(
            architecture,
            vocab=spec["vocab"],
            ctx_len=spec["sequence_len"],
            attention_window=spec["attention_window"],
        )
    ).cuda()
    train_rng = np.random.default_rng(
        _seed(protocol["protocol_hash"], row["dataset"], seed, "train_chunks")
    )
    val_rng = np.random.default_rng(
        _seed(protocol["protocol_hash"], row["dataset"], "fixed_validation")
    )
    val_starts = corpus.sample_starts(
        "validation", spec["validation_chunks"], val_rng, replace=False
    )
    score_start, score_stop = spec["scored_positions"]

    @torch.no_grad()
    def validate() -> float:
        model.eval()
        total = 0.0
        for start in range(0, len(val_starts), 32):
            x_np, y_np = corpus.batch(val_starts[start : start + 32])
            x = torch.from_numpy(x_np).cuda()
            y = torch.from_numpy(y_np).cuda()
            logits = model(x)[:, score_start:score_stop, :]
            total += cross_entropy_bits(logits, y[:, score_start:score_stop]) * len(x)
        model.train()
        return total / len(val_starts)

    scored_per_chunk = score_stop - score_start
    chunks_per_step = spec["target_tokens_per_step"] // scored_per_chunk
    steps = spec["budget_target_tokens"] // spec["target_tokens_per_step"]
    every = max(1, steps // spec["checkpoints"])
    optimizer = torch.optim.AdamW(
        model.parameters(), lr=model.cfg.lr, weight_decay=model.cfg.weight_decay
    )
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=steps, eta_min=model.cfg.lr * 0.1
    )
    token_grid = [0]
    curve = [validate()]
    started = time.monotonic()
    for step in range(1, steps + 1):
        starts = corpus.sample_starts("train", chunks_per_step, train_rng, replace=True)
        x_np, y_np = corpus.batch(starts)
        x = torch.from_numpy(x_np).cuda()
        y = torch.from_numpy(y_np).cuda()
        logits = model(x)[:, score_start:score_stop, :]
        loss = torch.nn.functional.cross_entropy(
            logits.reshape(-1, spec["vocab"]), y[:, score_start:score_stop].reshape(-1)
        )
        optimizer.zero_grad(set_to_none=True)
        loss.backward()
        torch.nn.utils.clip_grad_norm_(model.parameters(), model.cfg.grad_clip)
        optimizer.step()
        scheduler.step()
        if step % every == 0 or step == steps:
            token_grid.append(step * spec["target_tokens_per_step"])
            curve.append(validate())
    summary = curve_metrics(token_grid, curve, curve[0])
    default_prefix = "P" if row["panel"] == "pilot" else "C"
    return {
        "cell_id": payload.get(
            "cell_id",
            f"V31{default_prefix}/{row['dataset']}/{architecture['id']}/s{seed}",
        ),
        "dataset": row["dataset"],
        "panel": row["panel"],
        "stratum": row["stratum"],
        "architecture": architecture["id"],
        "seed": seed,
        "protocol_hash": protocol["protocol_hash"],
        "analysis_protocol_hash": payload.get("analysis_protocol_hash"),
        "data_manifest_hash": payload["data_manifest_hash"],
        "profile_manifest_hash": payload["profile_manifest_hash"],
        "fourier_ce_kernel_hash": payload["fourier_ce_kernel_hash"],
        "prediction_lock_hash": payload.get("prediction_lock_hash"),
        "data_sha256": row["byte_stream_sha256"],
        "config_hash": model.cfg.config_hash,
        "validation_starts_sha256": hashlib.sha256(val_starts.tobytes()).hexdigest(),
        "token_grid": token_grid,
        "val_ce_bits": curve,
        "floor_independent": summary,
        "remote": {
            "gpu": torch.cuda.get_device_name(0),
            "wallclock_seconds": time.monotonic() - started,
        },
    }


def _save_cells(path: Path, rows: list[dict], *, key: str = "cell_id") -> None:
    existing = json.loads(path.read_text()) if path.exists() else []
    known = {row[key] for row in existing}
    existing.extend(row for row in rows if row[key] not in known)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(sorted(existing, key=lambda row: row[key]), indent=2) + "\n"
    )


def _run_character_training(
    limit: int,
    *,
    selected_supports: tuple[tuple[int, ...], ...] | None = None,
) -> None:
    spec = PROTOCOL["fourier_character_training"]
    all_supports = enumerate_supports(spec["lags"], max_degree=spec["max_degree"])
    supports = selected_supports or all_supports
    result_path = OUT / "fourier_character_results.json"
    existing = json.loads(result_path.read_text()) if result_path.exists() else []
    completed = {row["cell_id"] for row in existing}
    cells = [
        (architecture, support, seed)
        for architecture in PROTOCOL["architectures"]
        for support in supports
        for seed in spec["seeds"]
    ]
    if limit:
        cells = cells[:limit]
    for architecture, support, seed in cells:
        support_id = "-".join(str(value) for value in support)
        cell_id = f"V31F/{architecture['id']}/A{support_id}/s{seed}"
        if cell_id in completed:
            continue
        result = character_train_cell.remote(
            {
                "protocol": PROTOCOL,
                "architecture": architecture,
                "support": support,
                "seed": seed,
            }
        )
        _save_cells(result_path, [result])
        print(cell_id, result["character_hardness"], flush=True)
    if limit or selected_supports is not None:
        return
    cells = json.loads(result_path.read_text())
    from dlx.analysis.character_response import empirical_character_ce_kernel

    kernel = {
        "protocol_hash": PROTOCOL["protocol_hash"],
        "definition": "median held-out normalized CE curve area for learning each exact Fourier character",
        "results_sha256": file_sha256(result_path),
        "architecture_hardness": empirical_character_ce_kernel(
            cells,
            architectures=[row["id"] for row in PROTOCOL["architectures"]],
            supports=supports,
            seeds=spec["seeds"],
        ),
    }
    digest = write_json_once(OUT / "fourier_ce_kernel.json", kernel)
    write_hash_once(OUT / "fourier_ce_kernel.sha256", digest)
    print(f"Fourier-character CE kernel: {digest}")


def _run_degree_three_sentinels(limit: int) -> None:
    config_path = ROOT / "configs/degree3_sentinels_v3.1.json"
    lock_path = ROOT / "configs/degree3_sentinels_v3.1.sha256"
    verify_hash_lock(config_path, lock_path)
    config = json.loads(config_path.read_text())
    supports = tuple(
        tuple(int(value) for value in support)
        for group in (
            "high_energy_unmeasured_supports",
            "geometric_and_boundary_stress_supports",
        )
        for support in config["selection"][group]
    )
    if len(supports) != len(set(supports)) or any(
        len(support) != 3 for support in supports
    ):
        raise ValueError("degree-three sentinel supports must be unique triples")

    result_path = OUT / "fourier_character_results.json"
    existing = json.loads(result_path.read_text())
    sentinel_set = set(supports)
    pre_sentinel = [
        row for row in existing if tuple(int(value) for value in row["support"])
        not in sentinel_set
    ]
    pre_sentinel_payload = (
        json.dumps(
            sorted(pre_sentinel, key=lambda row: row["cell_id"]), indent=2
        )
        + "\n"
    ).encode()
    pre_sentinel_hash = hashlib.sha256(pre_sentinel_payload).hexdigest()
    if pre_sentinel_hash != config["character_results_sha256_before_sentinels"]:
        raise ValueError("pre-sentinel Fourier-character result lock mismatch")
    _run_character_training(limit, selected_supports=supports)


def _run_degree_two_completion(limit: int) -> None:
    spec = PROTOCOL["fourier_character_training"]
    supports = tuple(
        support
        for support in enumerate_supports(
            spec["lags"], max_degree=spec["max_degree"]
        )
        if len(support) == 2
    )
    if len(supports) != 21:
        raise ValueError("expected all 21 degree-two Fourier supports")
    _run_character_training(limit, selected_supports=supports)


def _run_profiles() -> None:
    data_hash = verify_hash_lock(
        OUT / "data_manifest.json", OUT / "data_manifest.sha256"
    )
    profile_dir = OUT / "profiles"
    profile_dir.mkdir(parents=True, exist_ok=True)
    missing = [
        row
        for row in CORPORA
        if not (profile_dir / f"{row['dataset']}.json").exists()
        or not (OUT / "audit_chains" / f"{row['dataset']}.json.gz").exists()
    ]
    for result in profile_cell.map(
        [{"protocol": PROTOCOL, "corpus": row} for row in missing]
    ):
        chains = result.pop("chains")
        path = profile_dir / f"{result['dataset']}.json"
        if path.exists():
            prior = json.loads(path.read_text())
            comparable_prior = {
                key: value
                for key, value in prior.items()
                if key != "remote_wallclock_seconds"
            }
            comparable_result = {
                key: value
                for key, value in result.items()
                if key != "remote_wallclock_seconds"
            }
            if comparable_prior != comparable_result:
                raise ValueError(
                    f"non-deterministic profile rerun for {result['dataset']}"
                )
        else:
            write_json_once(path, result)
        audit = {
            "dataset": result["dataset"],
            "summary_sha256": file_sha256(path),
            "chains": chains,
        }
        audit_path = OUT / "audit_chains" / f"{result['dataset']}.json.gz"
        audit_path.parent.mkdir(parents=True, exist_ok=True)
        audit_path.write_bytes(
            gzip.compress(json.dumps(audit, separators=(",", ":")).encode(), mtime=0)
        )
        print(f"profiled {result['dataset']}", flush=True)
    records = []
    for row in CORPORA:
        path = profile_dir / f"{row['dataset']}.json"
        audit_path = OUT / "audit_chains" / f"{row['dataset']}.json.gz"
        if not path.exists() or not audit_path.exists():
            raise ValueError(f"missing profile artifact for {row['dataset']}")
        records.append(
            {
                "dataset": row["dataset"],
                "profile_sha256": file_sha256(path),
                "audit_sha256": file_sha256(audit_path),
            }
        )
    manifest = {
        "protocol_hash": PROTOCOL["protocol_hash"],
        "data_manifest_hash": data_hash,
        "profiles": sorted(records, key=lambda row: row["dataset"]),
    }
    digest = write_json_once(OUT / "profile_manifest.json", manifest)
    write_hash_once(OUT / "profile_manifest.sha256", digest)
    print(f"profile manifest: {digest}")


def _run_natural(stage: str, limit: int) -> None:
    data_hash = verify_hash_lock(
        OUT / "data_manifest.json", OUT / "data_manifest.sha256"
    )
    profile_hash = verify_hash_lock(
        OUT / "profile_manifest.json", OUT / "profile_manifest.sha256"
    )
    fourier_ce_kernel_hash = verify_hash_lock(
        OUT / "fourier_ce_kernel.json", OUT / "fourier_ce_kernel.sha256"
    )
    prediction_hash = None
    analysis_protocol_hash = None
    if stage == "confirmation":
        prediction_hash = verify_hash_lock(
            OUT / "predictions.json", OUT / "predictions.sha256"
        )
    elif stage == "expansion":
        prediction_hash = verify_hash_lock(
            OUT / "expansion_predictions.json",
            OUT / "expansion_predictions.sha256",
        )
        expansion_protocol = load_frozen_protocol(ROOT / "configs/protocol_v3.2.json")
        analysis_protocol_hash = expansion_protocol["protocol_hash"]
    result_path = OUT / f"{stage}_results.json"
    existing = json.loads(result_path.read_text()) if result_path.exists() else []
    completed = {row["cell_id"] for row in existing}
    cells = [
        (row, architecture, seed)
        for row in CORPORA
        if row["panel"] == ("confirmation" if stage == "expansion" else stage)
        for architecture in PROTOCOL["architectures"]
        for seed in PROTOCOL["natural_training"]["seeds"]
    ]
    if limit:
        cells = cells[:limit]
    for row, architecture, seed in cells:
        if stage == "pilot":
            cell_id = f"V31P/{row['dataset']}/{architecture['id']}/s{seed}"
        elif stage == "confirmation":
            cell_id = f"V31C/{row['dataset']}/{architecture['id']}/s{seed}"
        else:
            cell_id = f"V32E/{row['dataset']}/{architecture['id']}/s{seed}"
        if cell_id in completed:
            continue
        result = natural_train_cell.remote(
            {
                "protocol": PROTOCOL,
                "corpus": row,
                "architecture": architecture,
                "seed": seed,
                "data_manifest_hash": data_hash,
                "profile_manifest_hash": profile_hash,
                "fourier_ce_kernel_hash": fourier_ce_kernel_hash,
                "prediction_lock_hash": prediction_hash,
                "analysis_protocol_hash": analysis_protocol_hash,
                "cell_id": cell_id,
            }
        )
        _save_cells(result_path, [result])
        print(cell_id, result["floor_independent"]["normalized_curve_area"], flush=True)


@app.local_entrypoint()
def main(stage: str = "character", limit: int = 0) -> None:
    if stage == "character":
        _run_character_training(limit)
    elif stage == "sentinel":
        _run_degree_three_sentinels(limit)
    elif stage == "degree2":
        _run_degree_two_completion(limit)
    elif stage == "profile":
        _run_profiles()
    elif stage in {"pilot", "confirmation", "expansion"}:
        _run_natural(stage, limit)
    else:
        raise ValueError(
            "stage must be character, sentinel, degree2, profile, pilot, or "
            "confirmation, or expansion"
        )
