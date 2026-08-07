from __future__ import annotations

import argparse
import json
import os
from pathlib import Path

import torch
import torch.distributed as dist

from .data import load_and_validate_manifest
from .distributed import initialize_distributed
from .loss import ExactFullKLLoss
from .masking import corrupt_blocks
from .streaming_data import collate_examples
from .teacher import OnlineDreamTeacher, TeacherSpec
from .token_cache import TokenCacheMixture


def factorize_tied_head(
    weight: torch.Tensor, *, width: int, oversample: int, iterations: int
) -> tuple[torch.Tensor, torch.Tensor]:
    """Randomized SVD factors W ~= student_embedding @ teacher_projection.T."""
    if not 0 < width <= min(weight.shape):
        raise ValueError("oracle width is outside the matrix rank")
    q = min(width + oversample, min(weight.shape))
    left, singular, right = torch.svd_lowrank(
        weight.float(), q=q, niter=iterations
    )
    root = singular[:width].sqrt()
    return left[:, :width] * root, right[:, :width] * root


def run(args: argparse.Namespace) -> None:
    if not args.allow_paid:
        raise RuntimeError("refusing paid oracle execution without --allow-paid")
    context = initialize_distributed()
    widths = [int(value) for value in args.widths.split(",")]
    if context.world_size != len(widths):
        raise ValueError("provide exactly one oracle width per GPU rank")
    if context.primary:
        key = os.environ.get("WANDB_API_KEY", "")
        if not key:
            raise RuntimeError("WANDB_API_KEY is required before a paid launch")
        import wandb

        wandb.login(key=key, relogin=True, verify=True)
        run_handle = wandb.init(
            project=args.wandb_project,
            name=args.run_name,
            job_type="head-oracle",
            config=vars(args),
        )
        run_url = str(run_handle.url or "")
        if not run_url.startswith("https://wandb.ai/"):
            raise RuntimeError("W&B did not return a direct run URL")
    else:
        run_handle, run_url = None, ""
    urls = [run_url]
    dist.broadcast_object_list(urls, src=0)
    run_url = urls[0]

    load_and_validate_manifest(args.manifest)
    spec = TeacherSpec(
        position_chunk_size=args.position_chunk,
        vocab_chunk_size=args.vocab_chunk,
    )
    teacher = OnlineDreamTeacher.from_pretrained(device=context.device, spec=spec)
    width = widths[context.rank]
    torch.manual_seed(args.seed + context.rank)
    embedding, projection = factorize_tied_head(
        teacher.output_weight,
        width=width,
        oversample=args.oversample,
        iterations=args.iterations,
    )

    mixture = TokenCacheMixture(
        args.eval_token_cache,
        rank=context.rank,
        data_manifest_path=args.manifest,
        stage=0,
    )
    examples = [next(mixture) for _ in range(args.eval_microbatch)]
    clean, eligible = collate_examples(examples, context.device)
    generator = torch.Generator(device=context.device).manual_seed(
        args.eval_seed + context.rank
    )
    corruption = corrupt_blocks(
        clean,
        mask_token_id=151_669,
        block_size=32,
        eligible_mask=eligible,
        generator=generator,
    )
    teacher_hidden = teacher.hidden_for_loss(corruption)
    projected_hidden = teacher_hidden.float() @ projection
    loss_fn = ExactFullKLLoss(
        position_chunk_size=args.position_chunk,
        vocab_chunk_size=args.vocab_chunk,
        excluded_token_id=151_669,
    )
    output = loss_fn(
        projected_hidden.to(torch.bfloat16),
        embedding.to(torch.bfloat16),
        teacher_hidden,
        teacher.output_weight,
        corruption.hard_labels,
        corruption.selected_block_ids,
    )
    full = corruption.block_noise.reshape(-1).eq(1).index_select(
        0, corruption.selected_block_ids
    )
    full_output = loss_fn(
        projected_hidden[full].to(torch.bfloat16),
        embedding.to(torch.bfloat16),
        teacher_hidden[full],
        teacher.output_weight,
        corruption.hard_labels[full],
        corruption.selected_block_ids[full],
    )
    metrics = {
        "rank": context.rank,
        "width": width,
        "seed": args.seed + context.rank,
        "full_kl": float(output.full_kl.item()),
        "fully_masked_full_kl": float(full_output.full_kl.item()),
        "targets": corruption.target_tokens,
        "fully_masked_targets": int(full.sum().item()),
    }
    destination = Path(args.output)
    if context.primary:
        destination.mkdir(parents=True, exist_ok=True)
    dist.barrier()
    torch.save(
        {
            "schema": "v2-sbd-head-oracle-v1",
            "teacher_model_id": spec.model_id,
            "teacher_revision": spec.revision,
            "width": width,
            "seed": args.seed + context.rank,
            "metrics": metrics,
            "wandb_url": run_url,
            "student_embedding": embedding.cpu().to(torch.bfloat16),
            "teacher_projection": projection.cpu().to(torch.bfloat16),
        },
        destination / f"oracle-width{width}-rank{context.rank}.pt",
    )
    gathered: list[dict | None] = [None] * context.world_size
    dist.all_gather_object(gathered, metrics)
    if context.primary:
        result = {"wandb_url": run_url, "candidates": gathered}
        (destination / "result.json").write_text(
            json.dumps(result, indent=2, sort_keys=True) + "\n"
        )
        assert run_handle is not None
        for item in gathered:
            assert item is not None
            run_handle.log({f"oracle/{item['width']}/rank{item['rank']}/full_kl": item["full_kl"]})
        run_handle.summary.update(result)
        run_handle.finish()
    dist.barrier()
    dist.destroy_process_group()


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description="Eight-GPU Dream head-rank oracle")
    result.add_argument("--allow-paid", action="store_true")
    result.add_argument("--manifest", required=True)
    result.add_argument("--eval-token-cache", required=True)
    result.add_argument("--output", required=True)
    result.add_argument("--run-name", required=True)
    result.add_argument("--wandb-project", default="v2-simpler-block-diffusion")
    result.add_argument("--widths", default="256,256,288,288,320,320,384,384")
    result.add_argument("--oversample", type=int, default=32)
    result.add_argument("--iterations", type=int, default=3)
    result.add_argument("--eval-microbatch", type=int, default=2)
    result.add_argument("--position-chunk", type=int, default=2048)
    result.add_argument("--vocab-chunk", type=int, default=8192)
    result.add_argument("--seed", type=int, default=8_061)
    result.add_argument("--eval-seed", type=int, default=29_941)
    return result


if __name__ == "__main__":
    run(parser().parse_args())
