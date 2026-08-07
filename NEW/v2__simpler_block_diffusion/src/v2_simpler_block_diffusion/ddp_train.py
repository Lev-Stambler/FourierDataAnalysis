from __future__ import annotations

import argparse
import gc
import json
import os
import time
from contextlib import nullcontext
from dataclasses import asdict
from pathlib import Path

import torch
import torch.distributed as dist
from torch.nn.parallel import DistributedDataParallel
from safetensors.torch import load_file

from .config import SimplerBlockDiffusionConfig
from .data import DataSource, load_and_validate_manifest, manifest_sha256
from .dagger import rollout_corruption
from .distributed import DistributedContext, global_mean, global_sum, initialize_distributed
from .loss import DistillationLoss, ExactFullKLLoss, GroupedTeacherTargets, HardCrossEntropyLoss
from .masking import CorruptionBatch, corrupt_blocks, subsample_corruption_targets
from .model import SimplerBlockDiffusionForMaskedLM
from .optimizer import MuonWithAuxAdamW, build_factorized_muon, set_lr_scale
from .shards import load_resolved_shards
from .streaming_data import collate_examples
from .teacher import OnlineDreamTeacher, TeacherSpec
from .telemetry import GpuTelemetry
from .token_cache import TokenCacheMixture
from .training import OptimizerConfig, TokenCounters, build_adamw, token_lr_scale


def _wandb_start(context: DistributedContext, args: argparse.Namespace):
    if not context.primary:
        return None, ""
    key = os.environ.get("WANDB_API_KEY", "")
    if not key:
        raise RuntimeError("WANDB_API_KEY is required before a paid launch")
    import wandb

    wandb.login(key=key, relogin=True, verify=True)
    run = wandb.init(
        project=args.wandb_project,
        name=args.run_name,
        job_type=args.mode,
        config=vars(args),
    )
    url = str(run.url or "")
    if not url.startswith("https://wandb.ai/"):
        raise RuntimeError(f"W&B did not return a direct run URL: {url!r}")
    return run, url


def _broadcast_string(value: str, context: DistributedContext) -> str:
    values = [value]
    dist.broadcast_object_list(values, src=0)
    return str(values[0])


def _download_assets(context: DistributedContext, spec: TeacherSpec) -> None:
    if context.primary:
        from huggingface_hub import snapshot_download

        snapshot_download(spec.model_id, revision=spec.revision)
    dist.barrier()


def _finite_tensors(tensors: list[torch.Tensor]) -> bool:
    predicates_by_device: dict[torch.device, list[torch.Tensor]] = {}
    for tensor in tensors:
        predicates_by_device.setdefault(tensor.device, []).append(torch.isfinite(tensor).all())
    return all(bool(torch.stack(predicates).all()) for predicates in predicates_by_device.values())


def _finite_model(model: torch.nn.Module) -> bool:
    return _finite_tensors(list(model.parameters()))


def _finite_optimizer(optimizer) -> bool:
    tensors = [
        value
        for state in optimizer.state.values()
        for value in state.values()
        if torch.is_tensor(value)
    ]
    return _finite_tensors(tensors)


def _write_json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")


def _parse_checkpoint_targets(value: str) -> list[int]:
    if not value.strip():
        return []
    targets = sorted({int(item) for item in value.split(",")})
    if any(item <= 0 for item in targets):
        raise ValueError("checkpoint targets must be positive")
    return targets


def _evaluate_fixed(
    student: SimplerBlockDiffusionForMaskedLM,
    loss_fn: DistillationLoss,
    corruption: CorruptionBatch,
    targets: GroupedTeacherTargets,
    context: DistributedContext,
    baseline: dict[str, float] | None,
) -> dict[str, float]:
    student.eval()
    with torch.no_grad():
        output = student(
            corruption.noisy_ids,
            corruption.clean_ids,
            corruption.block_noise,
        )
        hidden = output.noisy_hidden.reshape(-1, student.config.hidden_size).index_select(
            0, corruption.selected_indices.long()
        )
        losses = loss_fn(
            hidden,
            student.embed_tokens.weight,
            targets,
            corruption.hard_labels,
            corruption.selected_block_ids,
        )
    student.train()
    result = {
        "eval/grouped_kl": global_mean(losses.grouped_kl, context),
        "eval/hard_nll": global_mean(losses.hard_nll, context),
        "eval/top1_agreement": global_mean(losses.top1_agreement, context),
    }
    local_positions = int(corruption.hard_labels.numel())
    global_positions = global_sum(local_positions, context)
    teacher_hard_matches = int(
        (targets.top_ids[:, 0] == corruption.hard_labels).sum().item()
    )
    result["eval/teacher_top1_hard_label_agreement"] = (
        global_sum(teacher_hard_matches, context) / global_positions
    )
    result["eval/teacher_mean_top1_probability"] = (
        global_sum(float(targets.top_probs[:, 0].sum().item()), context) / global_positions
    )
    result["eval/teacher_mean_top16_mass"] = (
        global_sum(float(targets.top_probs.sum().item()), context) / global_positions
    )
    full_blocks = corruption.block_noise.reshape(-1).eq(1)
    selected_full = full_blocks.index_select(0, corruption.selected_block_ids.long())
    full_positions = int(selected_full.sum().item())
    if full_positions == 0:
        raise RuntimeError("fixed evaluation produced no fully masked positions")
    full_targets = GroupedTeacherTargets(
        targets.top_ids[selected_full],
        targets.top_log_probs[selected_full],
        targets.tail_log_prob[selected_full],
    )
    full_losses = loss_fn(
        hidden[selected_full],
        student.embed_tokens.weight,
        full_targets,
        corruption.hard_labels[selected_full],
        corruption.selected_block_ids[selected_full],
    )
    full_groups = int(torch.unique(corruption.selected_block_ids[selected_full]).numel())
    global_full_positions = global_sum(full_positions, context)
    global_full_groups = global_sum(full_groups, context)
    result.update(
        {
            "eval/fully_masked_positions": int(global_full_positions),
            "eval/fully_masked_blocks": int(global_full_groups),
            "eval/fully_masked_top1_agreement": global_sum(
                float(full_losses.top1_agreement.item()) * full_positions, context
            )
            / global_full_positions,
            "eval/fully_masked_grouped_kl": global_sum(
                float(full_losses.grouped_kl.item()) * full_groups, context
            )
            / global_full_groups,
            "eval/fully_masked_hard_nll": global_sum(
                float(full_losses.hard_nll.item()) * full_groups, context
            )
            / global_full_groups,
            "eval/fully_masked_teacher_mean_top1_probability": global_sum(
                float(full_targets.top_probs[:, 0].sum().item()), context
            )
            / global_full_positions,
            "eval/fully_masked_teacher_mean_top16_mass": global_sum(
                float(full_targets.top_probs.sum().item()), context
            )
            / global_full_positions,
        }
    )
    if baseline is not None:
        start = baseline["eval/grouped_kl"]
        result["eval/grouped_kl_relative_improvement"] = (start - result["eval/grouped_kl"]) / max(start, 1e-12)
    return result


def _weighted_rank_mean(value: torch.Tensor, local_weight: int, context: DistributedContext) -> float:
    total_weight = global_sum(local_weight, context)
    if total_weight == 0:
        raise RuntimeError("distributed metric has zero weight")
    return global_sum(float(value.item()) * local_weight, context) / total_weight


def _evaluate_full_kl(
    student: SimplerBlockDiffusionForMaskedLM,
    loss_fn: ExactFullKLLoss,
    corruption: CorruptionBatch,
    teacher_hidden: torch.Tensor,
    teacher_weight: torch.Tensor,
    context: DistributedContext,
    baseline: dict[str, float] | None,
) -> dict[str, float]:
    student.eval()
    with torch.no_grad():
        output = student(
            corruption.noisy_ids, corruption.clean_ids, corruption.block_noise
        )
        hidden = output.noisy_hidden.reshape(-1, student.config.hidden_size).index_select(
            0, corruption.selected_indices.long()
        )
        losses = loss_fn(
            hidden,
            student.embed_tokens.weight,
            teacher_hidden,
            teacher_weight,
            corruption.hard_labels,
            corruption.selected_block_ids,
        )
    student.train()
    groups = int(torch.unique(corruption.selected_block_ids).numel())
    positions = corruption.target_tokens
    result = {
        "eval/full_kl": _weighted_rank_mean(losses.full_kl, groups, context),
        "eval/student_hard_nll": _weighted_rank_mean(losses.student_hard_nll, groups, context),
        "eval/teacher_hard_nll": _weighted_rank_mean(losses.teacher_hard_nll, groups, context),
        "eval/top1_agreement": _weighted_rank_mean(losses.top1_agreement, positions, context),
    }
    full_blocks = corruption.block_noise.reshape(-1).eq(1)
    selected_full = full_blocks.index_select(0, corruption.selected_block_ids.long())
    full_positions = int(selected_full.sum().item())
    if full_positions == 0:
        raise RuntimeError("fixed evaluation produced no fully masked positions")
    full_losses = loss_fn(
        hidden[selected_full],
        student.embed_tokens.weight,
        teacher_hidden[selected_full],
        teacher_weight,
        corruption.hard_labels[selected_full],
        corruption.selected_block_ids[selected_full],
    )
    full_groups = int(torch.unique(corruption.selected_block_ids[selected_full]).numel())
    result.update(
        {
            "eval/fully_masked_full_kl": _weighted_rank_mean(full_losses.full_kl, full_groups, context),
            "eval/fully_masked_student_hard_nll": _weighted_rank_mean(
                full_losses.student_hard_nll, full_groups, context
            ),
            "eval/fully_masked_top1_agreement": _weighted_rank_mean(
                full_losses.top1_agreement, full_positions, context
            ),
            "eval/fully_masked_positions": global_sum(full_positions, context),
            "eval/fully_masked_blocks": global_sum(full_groups, context),
        }
    )
    if baseline is not None:
        start = baseline["eval/full_kl"]
        result["eval/full_kl_relative_improvement"] = (
            start - result["eval/full_kl"]
        ) / max(start, 1e-12)
    return result


def _save_checkpoint(
    destination: Path,
    *,
    student: SimplerBlockDiffusionForMaskedLM,
    optimizer: torch.optim.Optimizer,
    context: DistributedContext,
    counters: TokenCounters,
    data_stage: int,
    mixture: TokenCacheMixture | None,
    generator: torch.Generator,
    metadata: dict,
    eval_baseline: dict[str, float] | None,
    milestone_target_tokens: int | None,
) -> None:
    if context.primary:
        destination.mkdir(parents=True, exist_ok=True)
    dist.barrier()
    rank_state = {
        "schema": "v2-sbd-rank-state-v1",
        "rank": context.rank,
        "world_size": context.world_size,
        "counters": asdict(counters),
        "data_stage": data_stage,
        "mixture_state": None if mixture is None else mixture.state_dict(),
        "corruption_generator_state": generator.get_state(),
        "torch_rng_state": torch.get_rng_state(),
        "cuda_rng_state": torch.cuda.get_rng_state(context.device),
        "eval_baseline": eval_baseline,
        "milestone_target_tokens": milestone_target_tokens,
    }
    torch.save(rank_state, destination / f"rank-{context.rank:02d}-state.pt")
    dist.barrier()
    if context.primary:
        checkpoint_metadata = {
            **metadata,
            "token_counters": asdict(counters),
            "data_stage": data_stage,
            "milestone_target_tokens": milestone_target_tokens,
            "eval_baseline": eval_baseline,
        }
        student.save_pretrained(destination, checkpoint_metadata)
        torch.save(optimizer.state_dict(), destination / "optimizer.pt")
    dist.barrier()


def _load_rank_state(
    resume: Path,
    *,
    student: SimplerBlockDiffusionForMaskedLM,
    optimizer: torch.optim.Optimizer,
    context: DistributedContext,
    architecture_id: str,
    data_manifest_sha: str,
) -> dict:
    metadata = json.loads((resume / "metadata.json").read_text())
    if metadata.get("architecture_id") != architecture_id:
        raise ValueError("resume checkpoint architecture does not match")
    if metadata.get("data_manifest_sha256") != data_manifest_sha:
        raise ValueError("resume checkpoint data manifest does not match")
    if metadata.get("world_size") != context.world_size:
        raise ValueError("resume checkpoint world size does not match")
    student.load_state_dict(load_file(resume / "model.safetensors", device="cpu"))
    optimizer.load_state_dict(
        torch.load(resume / "optimizer.pt", map_location=context.device, weights_only=True)
    )
    state = torch.load(
        resume / f"rank-{context.rank:02d}-state.pt",
        map_location="cpu",
        weights_only=True,
    )
    if state.get("schema") != "v2-sbd-rank-state-v1" or state.get("rank") != context.rank:
        raise ValueError("invalid per-rank resume state")
    return state


def run(args: argparse.Namespace) -> None:
    if not args.allow_paid:
        raise RuntimeError("refusing paid execution without --allow-paid")
    if args.objective == "exact-full-kl" and args.teacher_mode != "online":
        raise ValueError("exact full KL requires the online teacher")
    schedule_horizon = (
        args.lr_schedule_target_tokens
        if args.lr_schedule_target_tokens is not None
        else args.target_tokens
    )
    if args.lr_schedule_origin_target_tokens < 0:
        raise ValueError("LR schedule origin must be non-negative")
    if schedule_horizon <= 0:
        raise ValueError("LR schedule horizon must be positive")
    if not 0 <= args.warmup_target_tokens <= schedule_horizon:
        raise ValueError("warmup target tokens must be in [0, schedule horizon]")
    context = initialize_distributed()
    run_handle, run_url = _wandb_start(context, args)
    run_url = _broadcast_string(run_url, context)
    output = Path(args.output)
    if context.primary:
        output.mkdir(parents=True, exist_ok=True)
        _write_json(
            output / "launch.json",
            {
                "schema": "v2-sbd-launch-v1",
                "wandb_url": run_url,
                "world_size": context.world_size,
                "microbatch_per_gpu": args.microbatch,
                "gradient_accumulation": args.gradient_accumulation,
                "global_contexts_per_step": args.microbatch
                * args.gradient_accumulation
                * context.world_size,
                "teacher_mode": args.teacher_mode,
                "arguments": vars(args),
            },
        )
    manifest = load_and_validate_manifest(args.manifest)
    sources = [DataSource(**source) for source in manifest["sources"]]
    resolved_shards = None
    if not args.synthetic_data:
        if not args.resolved_shards:
            raise RuntimeError("real-data runs require --resolved-shards")
        resolved_shards = load_resolved_shards(args.resolved_shards, args.manifest)
        missing = {source.source_id for source in sources} - set(resolved_shards)
        if missing:
            raise RuntimeError(f"resolved-shard manifest lacks sources: {sorted(missing)}")
    spec = TeacherSpec(position_chunk_size=args.position_chunk, vocab_chunk_size=args.vocab_chunk)
    _download_assets(context, spec)

    # DDP broadcasts rank-zero parameters, but an explicit seed is required so
    # separate LR/compile runs start from the identical student checkpoint.
    torch.manual_seed(args.seed)
    torch.cuda.manual_seed_all(args.seed)

    from transformers import AutoTokenizer

    tokenizer = AutoTokenizer.from_pretrained(
        spec.model_id, revision=spec.revision, trust_remote_code=True
    )
    if (
        tokenizer.vocab_size != 151_643
        or len(tokenizer) != 151_670
        or tokenizer.mask_token_id != 151_669
    ):
        raise RuntimeError(
            "teacher tokenizer changed: "
            f"base={tokenizer.vocab_size}, full={len(tokenizer)}, mask={tokenizer.mask_token_id}"
        )
    student = SimplerBlockDiffusionForMaskedLM(SimplerBlockDiffusionConfig()).to(
        device=context.device, dtype=torch.bfloat16
    )
    if student.num_parameters() != 97_832_320:
        raise RuntimeError(f"student parameter count changed: {student.num_parameters()}")
    oracle_projection = None
    oracle_metadata = None
    if args.oracle:
        oracle = torch.load(args.oracle, map_location="cpu", weights_only=True)
        if oracle.get("schema") != "v2-sbd-head-oracle-v1":
            raise ValueError("invalid head-oracle artifact")
        embedding = oracle["student_embedding"]
        projection = oracle["teacher_projection"]
        if embedding.shape != student.embed_tokens.weight.shape:
            raise ValueError("oracle embedding does not match the selected student")
        if projection.shape[1] != student.config.hidden_size:
            raise ValueError("oracle projection does not match the selected student width")
        student.embed_tokens.weight.data.copy_(embedding.to(context.device, torch.bfloat16))
        oracle_projection = projection.to(context.device, torch.bfloat16)
        oracle_metadata = {
            key: value
            for key, value in oracle.items()
            if key not in {"student_embedding", "teacher_projection"}
        }
    optimizer_config = OptimizerConfig(
        learning_rate=args.learning_rate,
        total_target_tokens=(
            args.lr_schedule_target_tokens
            if args.lr_schedule_target_tokens is not None
            else args.target_tokens
        ),
        warmup_target_tokens=min(
            args.warmup_target_tokens,
            args.lr_schedule_target_tokens
            if args.lr_schedule_target_tokens is not None
            else args.target_tokens,
        ),
        schedule_origin_target_tokens=args.lr_schedule_origin_target_tokens,
    )
    optimizer_inventory = None
    if args.optimizer == "factorized-muon":
        optimizer, optimizer_inventory = build_factorized_muon(
            student,
            muon_lr=args.muon_learning_rate,
            auxiliary_lr=args.learning_rate,
            auxiliary_weight_decay=optimizer_config.weight_decay,
        )
    else:
        optimizer = build_adamw(student, optimizer_config)
    resume_state = None
    if args.resume:
        resume_state = _load_rank_state(
            Path(args.resume),
            student=student,
            optimizer=optimizer,
            context=context,
            architecture_id=student.config.architecture_id,
            data_manifest_sha=manifest_sha256(args.manifest),
        )
    compile_artifact_path = None
    if args.compile_artifact:
        artifact_base = Path(args.compile_artifact)
        compile_artifact_path = artifact_base.with_name(
            f"{artifact_base.stem}-rank{context.local_rank:02d}{artifact_base.suffix}"
        )
    compile_artifact_loaded = False
    distributed_student: torch.nn.Module = student
    if args.compile:
        # DDPOptimizer repartitions an already compiled module at first use,
        # producing a different cache key from the ahead-of-DDP warm graph.
        # Keep synchronization in ordinary DDP and execute the one explicitly
        # compiled fixed-shape student graph unchanged.
        torch._dynamo.config.optimize_ddp = False
        if compile_artifact_path is None:
            raise RuntimeError("--compile requires --compile-artifact")
        if compile_artifact_path.is_file():
            cache_info = torch.compiler.load_cache_artifacts(compile_artifact_path.read_bytes())
            if cache_info is None:
                raise RuntimeError(f"failed to load compile artifact {compile_artifact_path}")
            compile_artifact_loaded = True
            distributed_student = torch.compile(student, dynamic=False)
        else:
            # Inductor cache guards include the local CUDA device index, so
            # each DDP rank owns one persistent artifact. Build all missing
            # device-specific graphs in parallel with bounded compiler pools.
            distributed_student = torch.compile(student, dynamic=False)
            dummy_noisy_ids = torch.zeros(
                (args.microbatch, student.config.max_position_embeddings),
                device=context.device,
                dtype=torch.long,
            )
            dummy_clean_ids = torch.zeros_like(dummy_noisy_ids)
            dummy_noise = torch.zeros(
                (
                    args.microbatch,
                    student.config.max_position_embeddings // student.config.block_size,
                ),
                device=context.device,
                dtype=torch.float32,
            )
            warm_output = distributed_student(
                dummy_noisy_ids, dummy_clean_ids, dummy_noise
            )
            warm_output.noisy_hidden.float().mean().backward()
            student.zero_grad(set_to_none=True)
            torch.cuda.synchronize(context.device)
            saved = torch.compiler.save_cache_artifacts()
            if saved is None:
                raise RuntimeError("torch.compile produced no serializable cache artifacts")
            compile_artifact_path.parent.mkdir(parents=True, exist_ok=True)
            temporary_artifact = compile_artifact_path.with_suffix(
                compile_artifact_path.suffix + ".part"
            )
            temporary_artifact.write_bytes(saved[0])
            temporary_artifact.replace(compile_artifact_path)
        dist.barrier()
    ddp = DistributedDataParallel(
        distributed_student,
        device_ids=[context.local_rank],
        output_device=context.local_rank,
        broadcast_buffers=False,
        gradient_as_bucket_view=True,
        static_graph=True,
    )
    forward_model = ddp
    train_distillation_loss = DistillationLoss(
        kd_weight=args.kd_weight,
        hard_weight=args.hard_weight,
        vocab_chunk_size=args.vocab_chunk,
        excluded_token_id=student.config.mask_token_id,
    )
    eval_loss_fn = DistillationLoss(
        vocab_chunk_size=args.vocab_chunk,
        excluded_token_id=student.config.mask_token_id,
    )
    train_hard_loss = HardCrossEntropyLoss(
        vocab_chunk_size=args.vocab_chunk,
        excluded_token_id=student.config.mask_token_id,
    )
    full_kl_loss = ExactFullKLLoss(
        position_chunk_size=args.position_chunk,
        vocab_chunk_size=args.vocab_chunk,
        excluded_token_id=student.config.mask_token_id,
    )
    counters = (
        TokenCounters()
        if resume_state is None
        else TokenCounters(**resume_state["counters"])
    )
    initial_lr_scale = token_lr_scale(counters.target_tokens, optimizer_config)
    if isinstance(optimizer, MuonWithAuxAdamW):
        set_lr_scale(optimizer, initial_lr_scale)
    else:
        for group in optimizer.param_groups:
            group["lr"] = optimizer_config.learning_rate * initial_lr_scale
    data_stage = (
        1 if counters.target_tokens >= args.stage1_start_target_tokens else 0
    ) if resume_state is None else int(resume_state["data_stage"])
    mixture = None
    if not args.synthetic_data:
        if not args.token_cache:
            raise RuntimeError("real-data paid runs require a prebuilt --token-cache")
        selected_cache = args.token_cache
        if data_stage == 1:
            if not args.stage1_token_cache:
                raise RuntimeError("resumed stage 1 requires --stage1-token-cache")
            selected_cache = args.stage1_token_cache
        mixture = TokenCacheMixture(
            selected_cache,
            rank=context.rank,
            data_manifest_path=args.manifest,
            stage=data_stage,
        )
        if resume_state is not None:
            mixture.load_state_dict(resume_state["mixture_state"])
    generator = torch.Generator(device=context.device).manual_seed(args.seed + context.rank)
    if resume_state is not None:
        generator.set_state(resume_state["corruption_generator_state"])
        torch.set_rng_state(resume_state["torch_rng_state"])
        torch.cuda.set_rng_state(resume_state["cuda_rng_state"], device=context.device)

    teacher = None
    if args.teacher_mode == "online" or args.eval_token_cache:
        teacher = OnlineDreamTeacher.from_pretrained(device=context.device, spec=spec)
        if int(teacher.model.config.vocab_size) != 151_936:
            raise RuntimeError(
                f"teacher model vocabulary changed: {teacher.model.config.vocab_size}"
            )

    eval_corruption = None
    eval_targets = None
    eval_teacher_hidden = None
    eval_baseline = None if resume_state is None else resume_state.get("eval_baseline")
    latest_fixed_kl = float("inf")
    if args.eval_token_cache:
        eval_mixture = TokenCacheMixture(
            args.eval_token_cache,
            rank=context.rank,
            data_manifest_path=args.manifest,
            stage=0,
        )
        eval_examples = [next(eval_mixture) for _ in range(args.eval_microbatch)]
        eval_clean, eval_eligible = collate_examples(eval_examples, context.device)
        eval_generator = torch.Generator(device=context.device).manual_seed(
            args.eval_seed + context.rank
        )
        eval_corruption = corrupt_blocks(
            eval_clean,
            mask_token_id=student.config.mask_token_id,
            block_size=student.config.block_size,
            eligible_mask=eval_eligible,
            generator=eval_generator,
        )
        with torch.no_grad():
            assert teacher is not None
            if args.objective == "exact-full-kl":
                eval_teacher_hidden = teacher.hidden_for_loss(eval_corruption)
            else:
                eval_targets = teacher.targets(
                    eval_corruption, excluded_token_id=student.config.mask_token_id
                )
        if args.objective == "exact-full-kl":
            assert eval_teacher_hidden is not None and teacher is not None
            initial_eval = _evaluate_full_kl(
                student,
                full_kl_loss,
                eval_corruption,
                eval_teacher_hidden,
                teacher.output_weight,
                context,
                eval_baseline,
            )
        else:
            assert eval_targets is not None
            initial_eval = _evaluate_fixed(
                student,
                eval_loss_fn,
                eval_corruption,
                eval_targets,
                context,
                eval_baseline,
            )
        if eval_baseline is None:
            eval_baseline = dict(initial_eval)
            initial_eval[
                "eval/full_kl_relative_improvement"
                if args.objective == "exact-full-kl"
                else "eval/grouped_kl_relative_improvement"
            ] = 0.0
        latest_fixed_kl = float(
            initial_eval.get("eval/full_kl", initial_eval.get("eval/grouped_kl", float("inf")))
        )
        if context.primary:
            assert run_handle is not None
            run_handle.log(initial_eval, step=counters.target_tokens)
            print(json.dumps({**initial_eval, "eval/target_tokens": counters.target_tokens}, sort_keys=True), flush=True)
    elif not args.synthetic_data and args.mode == "train":
        raise RuntimeError("real-data training requires --eval-token-cache")
    if args.teacher_mode == "eval-only":
        del teacher
        teacher = None
        gc.collect()
        torch.cuda.empty_cache()
        dist.barrier()

    # Exclude teacher loading, fixed-eval construction, and one-time compiler
    # warmup from steady training memory/throughput evidence.
    torch.cuda.reset_peak_memory_stats(context.device)
    started = time.perf_counter()
    last_time = started
    last_targets = counters.target_tokens
    peak_allocated = 0
    peak_reserved = 0
    last_metrics: dict[str, float | int] = {}
    last_eval_tokens = counters.target_tokens if eval_corruption is not None else -1
    next_eval_tokens = (
        ((counters.target_tokens // args.eval_every_target_tokens) + 1)
        * args.eval_every_target_tokens
        if eval_corruption is not None and args.eval_every_target_tokens > 0
        else None
    )
    checkpoint_targets = _parse_checkpoint_targets(args.checkpoint_targets)
    pending_checkpoint_targets = [
        target for target in checkpoint_targets if target > counters.target_tokens
    ]
    global_targets = 0
    checkpoint_metadata_base = {
        "schema": "v2-sbd-checkpoint-v2",
        "architecture_id": student.config.architecture_id,
        "teacher_model_id": spec.model_id,
        "teacher_revision": spec.revision,
        "data_manifest_sha256": manifest_sha256(args.manifest),
        "wandb_url": run_url,
        "world_size": context.world_size,
        "global_contexts_per_step": args.microbatch
        * args.gradient_accumulation
        * context.world_size,
        "synthetic_data": args.synthetic_data,
        "compile": args.compile,
        "arguments": vars(args),
        "optimizer_inventory": optimizer_inventory,
        "head_oracle": oracle_metadata,
    }
    telemetry = GpuTelemetry(context.device)
    telemetry.start()
    preflight_start_step = counters.optimizer_steps

    while counters.target_tokens < args.target_tokens:
        run_step = counters.optimizer_steps - preflight_start_step + 1
        terminal_preflight_step = (
            args.mode == "preflight" and run_step >= args.preflight_steps
        )
        should_log = run_step == 1 or run_step % args.log_every == 0 or terminal_preflight_step
        should_audit = should_log
        optimizer.zero_grad(set_to_none=True)
        step_target_tokens = 0
        metric_kl = 0.0
        metric_nll = 0.0
        metric_agreement = 0.0
        on_policy_microbatches = 0
        teacher_milliseconds = 0.0
        student_milliseconds = 0.0
        for accumulation_index in range(args.gradient_accumulation):
            if args.synthetic_data:
                clean_ids = torch.randint(
                    0,
                    tokenizer.vocab_size,
                    (args.microbatch, student.config.max_position_embeddings),
                    device=context.device,
                    generator=generator,
                )
                eligible = torch.ones_like(clean_ids, dtype=torch.bool)
            else:
                assert mixture is not None
                examples = [next(mixture) for _ in range(args.microbatch)]
                clean_ids, eligible = collate_examples(examples, context.device)
            dagger_probability = (
                0.8 if latest_fixed_kl <= 0.5 else 0.5 if latest_fixed_kl <= 1.0 else 0.0
            )
            use_on_policy = args.dagger and bool(
                torch.rand((), device=context.device, generator=generator) < dagger_probability
            )
            if use_on_policy:
                corruption = rollout_corruption(
                    student,
                    clean_ids,
                    eligible_mask=eligible,
                    generator=generator,
                    projection_group_chunk=args.dagger_projection_group_chunk,
                )
                on_policy_microbatches += 1
            else:
                corruption = corrupt_blocks(
                    clean_ids,
                    mask_token_id=student.config.mask_token_id,
                    block_size=student.config.block_size,
                    eligible_mask=eligible,
                    generator=generator,
                )
            if args.objective == "exact-full-kl":
                corruption = subsample_corruption_targets(
                    corruption,
                    max_targets_per_context=args.max_targets_per_context,
                    generator=generator,
                )
            teacher_start = torch.cuda.Event(enable_timing=True)
            teacher_end = torch.cuda.Event(enable_timing=True)
            student_end = torch.cuda.Event(enable_timing=True)
            teacher_start.record()
            targets = None
            teacher_hidden = None
            if args.teacher_mode == "online":
                assert teacher is not None
                if args.objective == "exact-full-kl":
                    teacher_hidden = teacher.hidden_for_loss(corruption)
                else:
                    targets = teacher.targets(
                        corruption, excluded_token_id=student.config.mask_token_id
                    )
            teacher_end.record()
            sync = accumulation_index + 1 == args.gradient_accumulation
            sync_context = nullcontext() if sync else ddp.no_sync()
            with sync_context:
                output_state = forward_model(
                    corruption.noisy_ids,
                    corruption.clean_ids,
                    corruption.block_noise,
                )
                # Keep the stochastic-length gather outside torch.compile. The
                # dense backbone has fixed [batch, 2048] shapes and therefore
                # needs exactly one compiled graph instead of one graph per
                # random target-token count.
                selected_hidden = output_state.noisy_hidden.reshape(
                    -1, student.config.hidden_size
                ).index_select(0, corruption.selected_indices.long())
                if args.teacher_mode == "online":
                    if args.objective == "exact-full-kl":
                        assert teacher_hidden is not None and teacher is not None
                        losses = full_kl_loss(
                            selected_hidden,
                            (
                                student.embed_tokens.weight.detach()
                                if oracle_projection is not None
                                and counters.target_tokens < args.oracle_freeze_target_tokens
                                else student.embed_tokens.weight
                            ),
                            teacher_hidden,
                            teacher.output_weight,
                            corruption.hard_labels,
                            corruption.selected_block_ids,
                        )
                        if (
                            oracle_projection is not None
                            and counters.target_tokens < args.oracle_freeze_target_tokens
                        ):
                            projected_teacher = teacher_hidden.to(torch.bfloat16) @ oracle_projection
                            hidden_cosine = 1 - torch.nn.functional.cosine_similarity(
                                selected_hidden.float(), projected_teacher.float(), dim=-1
                            ).mean()
                            losses.loss = losses.loss + args.hidden_aux_weight * hidden_cosine
                    else:
                        assert targets is not None
                        losses = train_distillation_loss(
                            selected_hidden,
                            student.embed_tokens.weight,
                            targets,
                            corruption.hard_labels,
                            corruption.selected_block_ids,
                        )
                else:
                    losses = train_hard_loss(
                        selected_hidden,
                        student.embed_tokens.weight,
                        corruption.hard_labels,
                        corruption.selected_block_ids,
                    )
                (losses.loss / args.gradient_accumulation).backward()
            student_end.record()
            if should_log:
                student_end.synchronize()
                teacher_milliseconds += teacher_start.elapsed_time(teacher_end)
                student_milliseconds += teacher_end.elapsed_time(student_end)
            step_target_tokens += corruption.target_tokens
            if args.teacher_mode == "online" and should_log:
                measured_kl = (
                    losses.full_kl if args.objective == "exact-full-kl" else losses.grouped_kl
                )
                local_groups = int(torch.unique(corruption.selected_block_ids).numel())
                metric_kl += _weighted_rank_mean(measured_kl, local_groups, context)
            if should_log:
                measured_nll = (
                    losses.student_hard_nll
                    if args.objective == "exact-full-kl"
                    else losses.hard_nll
                )
                metric_nll += global_mean(measured_nll, context)
            agreement = (
                losses.top1_agreement
                if args.teacher_mode == "online"
                else losses.hard_top1_accuracy
            )
            if should_log:
                metric_agreement += global_mean(agreement, context)

        if (
            oracle_projection is not None
            and counters.target_tokens < args.oracle_freeze_target_tokens
        ):
            student.embed_tokens.weight.grad = None
        gradient_norm = torch.nn.utils.clip_grad_norm_(student.parameters(), optimizer_config.gradient_clip)
        if should_audit:
            local_finite = float(bool(torch.isfinite(gradient_norm)) and _finite_model(student))
            if global_sum(local_finite, context) != context.world_size:
                raise FloatingPointError("non-finite model or gradient on at least one rank")
        optimizer.step()
        if should_audit:
            local_finite = float(_finite_model(student) and _finite_optimizer(optimizer))
            if global_sum(local_finite, context) != context.world_size:
                raise FloatingPointError("non-finite student or optimizer state")
        global_targets = int(global_sum(step_target_tokens, context))
        global_clean = args.microbatch * args.gradient_accumulation * context.world_size * student.config.max_position_embeddings
        counters.target_tokens += global_targets
        counters.clean_tokens += global_clean
        counters.model_tokens += 2 * global_clean
        counters.optimizer_steps += 1
        if (
            not args.synthetic_data
            and data_stage == 0
            and counters.target_tokens >= args.stage1_start_target_tokens
        ):
            data_stage = 1
            if not args.stage1_token_cache:
                raise RuntimeError("stage 1 requires --stage1-token-cache")
            mixture = TokenCacheMixture(
                args.stage1_token_cache,
                rank=context.rank,
                data_manifest_path=args.manifest,
                stage=1,
            )
        scale = token_lr_scale(counters.target_tokens, optimizer_config)
        if isinstance(optimizer, MuonWithAuxAdamW):
            set_lr_scale(optimizer, scale)
        else:
            for group in optimizer.param_groups:
                group["lr"] = optimizer_config.learning_rate * scale
        peak_allocated = max(peak_allocated, torch.cuda.max_memory_allocated(context.device))
        peak_reserved = max(peak_reserved, torch.cuda.max_memory_reserved(context.device))

        if should_log:
            dist.barrier()
            now = time.perf_counter()
            throughput = (counters.target_tokens - last_targets) / (now - last_time)
            last_targets = counters.target_tokens
            last_time = now
            local_util_mean, local_util_median, utilization_samples = telemetry.drain()
            util_mean_tensor = torch.tensor(local_util_mean, device=context.device)
            util_median_tensor = torch.tensor(local_util_median, device=context.device)
            dist.all_reduce(util_mean_tensor, op=dist.ReduceOp.SUM)
            dist.all_reduce(util_median_tensor, op=dist.ReduceOp.MIN)
            measured_ms = teacher_milliseconds + student_milliseconds
            metrics = {
                "train/target_tokens": counters.target_tokens,
                "train/clean_tokens": counters.clean_tokens,
                "train/model_tokens": counters.model_tokens,
                "train/optimizer_steps": counters.optimizer_steps,
                "train/global_target_tokens_per_step": global_targets,
                "train/global_contexts_per_step": args.microbatch
                * args.gradient_accumulation
                * context.world_size,
                "train/target_tokens_per_second": throughput,
                "train/hard_nll": metric_nll / args.gradient_accumulation,
                "train/learning_rate": optimizer.param_groups[0]["lr"],
                "train/data_stage": data_stage,
                "train/on_policy_fraction": on_policy_microbatches
                / args.gradient_accumulation,
                "train/dagger_mixture_probability": dagger_probability,
                "system/peak_allocated_gib": peak_allocated / 2**30,
                "system/peak_reserved_gib": peak_reserved / 2**30,
                "system/gpu_utilization_mean": float(util_mean_tensor.item() / context.world_size),
                "system/gpu_utilization_min_rank_median": float(util_median_tensor.item()),
                "system/utilization_samples_per_rank": utilization_samples,
                "system/teacher_time_fraction": teacher_milliseconds / max(measured_ms, 1e-9),
                "system/teacher_milliseconds": teacher_milliseconds,
                "system/student_milliseconds": student_milliseconds,
                "system/compile_artifact_loaded": int(compile_artifact_loaded),
            }
            if args.teacher_mode == "online":
                metrics[
                    "train/full_kl" if args.objective == "exact-full-kl" else "train/grouped_kl"
                ] = metric_kl / args.gradient_accumulation
                metrics["train/top1_agreement"] = metric_agreement / args.gradient_accumulation
            else:
                metrics["train/hard_top1_accuracy"] = (
                    metric_agreement / args.gradient_accumulation
                )
            last_metrics = metrics
            if context.primary:
                assert run_handle is not None
                run_handle.log(metrics, step=counters.target_tokens)
                print(json.dumps(metrics, sort_keys=True), flush=True)

        if next_eval_tokens is not None and counters.target_tokens >= next_eval_tokens:
            assert eval_corruption is not None
            if args.objective == "exact-full-kl":
                assert eval_teacher_hidden is not None and teacher is not None
                eval_metrics = _evaluate_full_kl(
                    student,
                    full_kl_loss,
                    eval_corruption,
                    eval_teacher_hidden,
                    teacher.output_weight,
                    context,
                    eval_baseline,
                )
            else:
                assert eval_targets is not None
                eval_metrics = _evaluate_fixed(
                    student,
                    eval_loss_fn,
                    eval_corruption,
                    eval_targets,
                    context,
                    eval_baseline,
                )
            last_eval_tokens = counters.target_tokens
            latest_fixed_kl = float(
                eval_metrics.get(
                    "eval/full_kl", eval_metrics.get("eval/grouped_kl", latest_fixed_kl)
                )
            )
            if context.primary:
                assert run_handle is not None
                run_handle.log(eval_metrics, step=counters.target_tokens)
                print(json.dumps({**eval_metrics, "eval/target_tokens": counters.target_tokens}, sort_keys=True), flush=True)
            while next_eval_tokens <= counters.target_tokens:
                next_eval_tokens += args.eval_every_target_tokens

        crossed_targets = []
        while pending_checkpoint_targets and pending_checkpoint_targets[0] <= counters.target_tokens:
            crossed_targets.append(pending_checkpoint_targets.pop(0))
        if crossed_targets:
            milestone = crossed_targets[-1]
            _save_checkpoint(
                output / f"checkpoint-{milestone:012d}",
                student=student,
                optimizer=optimizer,
                context=context,
                counters=counters,
                data_stage=data_stage,
                mixture=mixture,
                generator=generator,
                metadata={
                    **checkpoint_metadata_base,
                    "last_global_target_tokens_per_step": global_targets,
                },
                eval_baseline=eval_baseline,
                milestone_target_tokens=milestone,
            )

        if terminal_preflight_step:
            break

    dist.barrier()
    telemetry.stop()
    terminal_finite = float(_finite_model(student) and _finite_optimizer(optimizer))
    if global_sum(terminal_finite, context) != context.world_size:
        raise FloatingPointError("non-finite terminal student or optimizer state")
    if eval_corruption is not None and last_eval_tokens != counters.target_tokens:
        if args.objective == "exact-full-kl":
            assert eval_teacher_hidden is not None and teacher is not None
            eval_metrics = _evaluate_full_kl(
                student,
                full_kl_loss,
                eval_corruption,
                eval_teacher_hidden,
                teacher.output_weight,
                context,
                eval_baseline,
            )
        else:
            assert eval_targets is not None
            eval_metrics = _evaluate_fixed(
                student,
                eval_loss_fn,
                eval_corruption,
                eval_targets,
                context,
                eval_baseline,
            )
        last_eval_tokens = counters.target_tokens
        if context.primary:
            assert run_handle is not None
            run_handle.log(eval_metrics, step=counters.target_tokens)
            print(json.dumps({**eval_metrics, "eval/target_tokens": counters.target_tokens}, sort_keys=True), flush=True)
            last_metrics.update(eval_metrics)
    metadata = {
        **checkpoint_metadata_base,
        "last_global_target_tokens_per_step": global_targets,
    }
    _save_checkpoint(
        output / "checkpoint",
        student=student,
        optimizer=optimizer,
        context=context,
        counters=counters,
        data_stage=data_stage,
        mixture=mixture,
        generator=generator,
        metadata=metadata,
        eval_baseline=eval_baseline,
        milestone_target_tokens=None,
    )
    if context.primary:
        metadata = {**metadata, "token_counters": asdict(counters), "data_stage": data_stage}
        result = {
            **metadata,
            "status": "preflight_complete" if args.mode == "preflight" else "complete",
            "elapsed_seconds": time.perf_counter() - started,
            "peak_allocated_gib": peak_allocated / 2**30,
            "peak_reserved_gib": peak_reserved / 2**30,
            "last_metrics": last_metrics,
        }
        _write_json(output / "result.json", result)
        _write_json(output / "audit.json", result)
        assert run_handle is not None
        run_handle.summary.update(result)
        run_handle.finish()
    dist.barrier()
    dist.destroy_process_group()


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description="Eight-H100 online DreamReasoner distillation")
    result.add_argument("--allow-paid", action="store_true")
    result.add_argument("--mode", choices=("preflight", "train"), default="preflight")
    result.add_argument("--manifest", required=True)
    result.add_argument("--resolved-shards")
    result.add_argument("--token-cache")
    result.add_argument("--stage1-token-cache")
    result.add_argument("--eval-token-cache")
    result.add_argument("--output", required=True)
    result.add_argument("--run-name", required=True)
    result.add_argument("--wandb-project", default="v2-simpler-block-diffusion")
    result.add_argument("--microbatch", type=int, default=12)
    result.add_argument("--gradient-accumulation", type=int, default=1)
    result.add_argument("--target-tokens", type=int, default=100_000_000)
    result.add_argument("--warmup-target-tokens", type=int, default=100_000_000)
    result.add_argument(
        "--lr-schedule-origin-target-tokens",
        type=int,
        default=0,
        help="absolute counter at which this LR schedule starts",
    )
    result.add_argument(
        "--lr-schedule-target-tokens",
        type=int,
        help="schedule horizon measured from the schedule origin; defaults to --target-tokens",
    )
    result.add_argument("--learning-rate", type=float, default=3e-4)
    result.add_argument("--muon-learning-rate", type=float, default=0.02)
    result.add_argument(
        "--optimizer", choices=("factorized-muon", "adamw"), default="factorized-muon"
    )
    result.add_argument(
        "--objective", choices=("exact-full-kl", "grouped-kl"), default="exact-full-kl"
    )
    result.add_argument("--max-targets-per-context", type=int, default=256)
    result.add_argument("--oracle")
    result.add_argument("--oracle-freeze-target-tokens", type=int, default=10_000_000)
    result.add_argument("--hidden-aux-weight", type=float, default=0.1)
    result.add_argument("--dagger", action=argparse.BooleanOptionalAction, default=True)
    result.add_argument("--dagger-projection-group-chunk", type=int, default=32)
    result.add_argument("--kd-weight", type=float, default=0.8)
    result.add_argument("--hard-weight", type=float, default=0.2)
    result.add_argument("--teacher-mode", choices=("online", "eval-only"), default="online")
    result.add_argument("--position-chunk", type=int, default=512)
    result.add_argument("--vocab-chunk", type=int, default=8192)
    result.add_argument("--preflight-steps", type=int, default=10)
    result.add_argument("--log-every", type=int, default=1)
    result.add_argument("--seed", type=int, default=194)
    result.add_argument("--eval-seed", type=int, default=19_941)
    result.add_argument("--eval-microbatch", type=int, default=16)
    result.add_argument("--eval-every-target-tokens", type=int, default=5_000_000)
    result.add_argument("--stage1-start-target-tokens", type=int, default=8_000_000_000)
    result.add_argument(
        "--checkpoint-targets",
        default="10000000,50000000,100000000,250000000,500000000,1000000000,2000000000,4000000000,6000000000,8000000000,10000000000",
    )
    result.add_argument("--resume")
    result.add_argument("--compile", action="store_true")
    result.add_argument("--compile-artifact")
    result.add_argument(
        "--synthetic-data",
        action="store_true",
        help="isolate model/teacher throughput; never valid as corpus evidence",
    )
    return result


def main() -> None:
    run(parser().parse_args())


if __name__ == "__main__":
    main()
