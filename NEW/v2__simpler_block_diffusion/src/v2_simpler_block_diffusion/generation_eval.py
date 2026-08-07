from __future__ import annotations

import argparse
import json
import os
import time
from pathlib import Path

import torch
import torch.distributed as dist

from .distributed import DistributedContext, initialize_distributed
from .generation import generate_blocks_batch
from .model import SimplerBlockDiffusionForMaskedLM
from .teacher import TeacherSpec
from .telemetry import GpuTelemetry
from .token_cache import TokenCacheMixture


def repeated_ngram_fraction(token_ids: list[int], n: int = 4) -> float:
    if len(token_ids) < n:
        return 0.0
    ngrams = [tuple(token_ids[index : index + n]) for index in range(len(token_ids) - n + 1)]
    return 1.0 - len(set(ngrams)) / len(ngrams)


def longest_identical_run(token_ids: list[int]) -> int:
    longest = 0
    current = 0
    prior: int | None = None
    for token in token_ids:
        current = current + 1 if token == prior else 1
        longest = max(longest, current)
        prior = token
    return longest


def generation_health(
    token_ids: list[int],
    decoded: str,
    *,
    mask_token_id: int,
) -> dict[str, int | float | bool]:
    unique_ratio = len(set(token_ids)) / max(len(token_ids), 1)
    repeated_4gram = repeated_ngram_fraction(token_ids)
    identical_run = longest_identical_run(token_ids)
    decoded_characters = len(decoded.strip())
    non_degenerate = (
        len(token_ids) >= 8
        and mask_token_id not in token_ids
        and decoded_characters >= 8
        and unique_ratio >= 0.10
        and identical_run <= 8
        and repeated_4gram <= 0.50
    )
    return {
        "generated_tokens": len(token_ids),
        "decoded_characters": decoded_characters,
        "unique_token_ratio": unique_ratio,
        "longest_identical_run": identical_run,
        "repeated_4gram_fraction": repeated_4gram,
        "contains_mask": mask_token_id in token_ids,
        "non_degenerate": non_degenerate,
    }


def _start_wandb(context: DistributedContext, args: argparse.Namespace):
    if not context.primary:
        return None, ""
    key = os.environ.get("WANDB_API_KEY", "")
    if not key:
        raise RuntimeError("WANDB_API_KEY is required before accelerator evaluation")
    import wandb

    wandb.login(key=key, relogin=True, verify=True)
    run = wandb.init(
        project=args.wandb_project,
        name=args.run_name,
        job_type="generation-eval",
        config=vars(args),
    )
    url = str(run.url or "")
    if not url.startswith("https://wandb.ai/"):
        raise RuntimeError(f"W&B did not return a direct run URL: {url!r}")
    return run, url


def _broadcast_string(value: str) -> str:
    values = [value]
    dist.broadcast_object_list(values, src=0)
    return str(values[0])


def run(args: argparse.Namespace) -> None:
    if not args.allow_paid:
        raise RuntimeError("refusing accelerator evaluation without --allow-paid")
    context = initialize_distributed()
    run_handle, wandb_url = _start_wandb(context, args)
    wandb_url = _broadcast_string(wandb_url)

    checkpoint = Path(args.checkpoint)
    metadata = json.loads((checkpoint / "metadata.json").read_text())
    model = SimplerBlockDiffusionForMaskedLM.from_pretrained(checkpoint)
    model = model.to(device=context.device, dtype=torch.bfloat16).eval()
    if model.num_parameters() != model.config.expected_parameter_count:
        raise RuntimeError("checkpoint parameter count does not match frozen architecture")

    from transformers import AutoTokenizer

    spec = TeacherSpec()
    tokenizer = AutoTokenizer.from_pretrained(
        spec.model_id,
        revision=spec.revision,
        trust_remote_code=True,
    )
    cache = TokenCacheMixture(
        args.eval_token_cache,
        rank=context.rank,
        data_manifest_path=args.manifest,
        stage=0,
    )
    torch.manual_seed(args.seed + context.rank)
    prompts = []
    for local_index in range(args.prompts_per_rank):
        example = next(cache)
        prompts.append(example.input_ids[: args.prompt_tokens])
    prompt_batch = torch.stack(prompts).to(context.device)
    torch.cuda.reset_peak_memory_stats(context.device)
    telemetry = GpuTelemetry(context.device)
    dist.barrier()
    torch.cuda.synchronize(context.device)
    telemetry.start()
    generation_start = time.perf_counter()
    outputs = generate_blocks_batch(
        model,
        prompt_batch,
        max_new_tokens=args.max_new_tokens,
        eos_token_id=tokenizer.eos_token_id,
        temperature=args.temperature,
        top_p=args.top_p,
    )
    torch.cuda.synchronize(context.device)
    generation_seconds = time.perf_counter() - generation_start
    telemetry.stop()
    local_util_mean, local_util_median, utilization_samples = telemetry.drain()
    elapsed_tensor = torch.tensor(generation_seconds, device=context.device)
    util_mean_tensor = torch.tensor(local_util_mean, device=context.device)
    util_median_tensor = torch.tensor(local_util_median, device=context.device)
    sample_count_tensor = torch.tensor(utilization_samples, device=context.device, dtype=torch.long)
    peak_allocated_tensor = torch.tensor(
        torch.cuda.max_memory_allocated(context.device), device=context.device, dtype=torch.long
    )
    dist.all_reduce(elapsed_tensor, op=dist.ReduceOp.MAX)
    dist.all_reduce(util_mean_tensor, op=dist.ReduceOp.SUM)
    dist.all_reduce(util_median_tensor, op=dist.ReduceOp.MIN)
    dist.all_reduce(sample_count_tensor, op=dist.ReduceOp.MIN)
    dist.all_reduce(peak_allocated_tensor, op=dist.ReduceOp.MAX)
    local_samples: list[dict] = []
    for local_index, (prompt, output) in enumerate(zip(prompt_batch, outputs)):
        generated = output[prompt.numel() :].tolist()
        generated_text = tokenizer.decode(generated, skip_special_tokens=True)
        sample = {
            "sample_id": context.rank * args.prompts_per_rank + local_index,
            "rank": context.rank,
            "prompt_token_ids": prompt.tolist(),
            "generated_token_ids": generated,
            "prompt_text": tokenizer.decode(prompt.tolist(), skip_special_tokens=True),
            "generated_text": generated_text,
            **generation_health(
                generated,
                generated_text,
                mask_token_id=model.config.mask_token_id,
            ),
        }
        local_samples.append(sample)
        if context.primary:
            print(json.dumps({
                "generation/sample_id": sample["sample_id"],
                "generation/non_degenerate": sample["non_degenerate"],
                "generation/generated_tokens": sample["generated_tokens"],
            }), flush=True)

    gathered: list[list[dict] | None] | None = (
        [None for _ in range(context.world_size)] if context.primary else None
    )
    dist.gather_object(local_samples, gathered, dst=0)
    if context.primary:
        assert gathered is not None and run_handle is not None
        samples = sorted(
            [sample for rank_samples in gathered if rank_samples for sample in rank_samples],
            key=lambda sample: sample["sample_id"],
        )
        count = len(samples)
        pass_count = sum(bool(sample["non_degenerate"]) for sample in samples)
        generated_token_count = sum(sample["generated_tokens"] for sample in samples)
        protocol_matches_gate = args.temperature == 0.0 and args.top_p == 1.0
        metrics = {
            "generation/prompts": count,
            "generation/non_degenerate": pass_count,
            "generation/non_degenerate_rate": pass_count / max(count, 1),
            "generation/mean_generated_tokens": sum(sample["generated_tokens"] for sample in samples)
            / max(count, 1),
            "generation/mean_unique_token_ratio": sum(
                sample["unique_token_ratio"] for sample in samples
            )
            / max(count, 1),
            "generation/mean_repeated_4gram_fraction": sum(
                sample["repeated_4gram_fraction"] for sample in samples
            )
            / max(count, 1),
            "generation/protocol_matches_gate": protocol_matches_gate,
            "generation/gate_pass": (
                protocol_matches_gate and pass_count / max(count, 1) >= 0.95
            ),
            "system/generation_seconds": float(elapsed_tensor.item()),
            "system/generated_tokens_per_second": generated_token_count
            / max(float(elapsed_tensor.item()), 1e-9),
            "system/gpu_utilization_mean": float(util_mean_tensor.item() / context.world_size),
            "system/gpu_utilization_min_rank_median": float(util_median_tensor.item()),
            "system/utilization_samples_per_rank": int(sample_count_tensor.item()),
            "system/peak_allocated_gib": float(peak_allocated_tensor.item() / 2**30),
        }
        result = {
            "schema": "v2-sbd-generation-eval-v1",
            "architecture_id": model.config.architecture_id,
            "checkpoint": str(checkpoint),
            "training_wandb_url": metadata.get("wandb_url"),
            "wandb_url": wandb_url,
            "criteria": {
                "temperature": args.temperature,
                "top_p": args.top_p,
                "minimum_generated_tokens": 8,
                "minimum_decoded_characters": 8,
                "minimum_unique_token_ratio": 0.10,
                "maximum_identical_token_run": 8,
                "maximum_repeated_4gram_fraction": 0.50,
                "mask_token_forbidden": True,
                "gate_rate": 0.95,
            },
            "metrics": metrics,
            "samples": samples,
        }
        destination = Path(args.output)
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")
        run_handle.log(metrics)
        run_handle.summary.update(
            {
                **metrics,
                "architecture_id": model.config.architecture_id,
                "checkpoint": str(checkpoint),
                "training_wandb_url": metadata.get("wandb_url"),
                "generation_result": str(destination),
            }
        )
        run_handle.finish()
        print(json.dumps(metrics, sort_keys=True), flush=True)
        print(json.dumps({"wandb_url": wandb_url, "output": str(destination)}), flush=True)
    dist.barrier()
    dist.destroy_process_group()


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description="Eight-H100 fixed generation evaluation")
    result.add_argument("--allow-paid", action="store_true")
    result.add_argument("--checkpoint", required=True)
    result.add_argument("--manifest", required=True)
    result.add_argument("--eval-token-cache", required=True)
    result.add_argument("--output", required=True)
    result.add_argument("--run-name", required=True)
    result.add_argument("--wandb-project", default="v2-simpler-block-diffusion")
    result.add_argument("--prompts-per-rank", type=int, default=16)
    result.add_argument("--prompt-tokens", type=int, default=64)
    result.add_argument("--max-new-tokens", type=int, default=64)
    result.add_argument("--temperature", type=float, default=0.0)
    result.add_argument("--top-p", type=float, default=1.0)
    result.add_argument("--seed", type=int, default=202_608_06)
    return result


def main() -> None:
    run(parser().parse_args())


if __name__ == "__main__":
    main()
