from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import torch


APPROVED_LICENSES = {
    "apache-2.0",
    "cc-by-4.0",
    "mit",
    "odc-by-1.0",
    "permissive-spdx-with-attribution",
}


@dataclass(frozen=True)
class DataSource:
    source_id: str
    dataset: str
    subset: str | None
    revision: str
    license: str
    stage0_weight: float
    stage1_weight: float
    allowed: bool
    kind: str = "document"
    text_field: str = "text"
    allowed_row_sources: tuple[str, ...] = ()


def load_and_validate_manifest(path: str | Path, *, require_locked: bool = True) -> dict[str, Any]:
    manifest = json.loads(Path(path).read_text())
    if manifest.get("schema") != "v2-sbd-data-manifest-v1":
        raise ValueError("unsupported data manifest schema")
    if require_locked and manifest.get("state") != "locked":
        raise ValueError("data manifest is not revision-locked")
    sources = manifest.get("sources", [])
    if not sources:
        raise ValueError("data manifest contains no sources")
    for raw in sources:
        source = DataSource(**raw)
        if not source.allowed:
            raise ValueError(f"disallowed source remains in active manifest: {source.source_id}")
        if source.license.lower() not in APPROVED_LICENSES:
            raise ValueError(f"unapproved license for {source.source_id}: {source.license}")
        if not source.revision or source.revision == "TO_BE_PINNED":
            raise ValueError(f"source revision is not pinned: {source.source_id}")
    for field in ("stage0_weight", "stage1_weight"):
        total = sum(float(source[field]) for source in sources)
        if abs(total - 1.0) > 1e-8:
            raise ValueError(f"{field} weights sum to {total}, not 1")
    return manifest


def manifest_sha256(path: str | Path) -> str:
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def pack_document(
    token_ids: list[int] | torch.Tensor,
    *,
    context_length: int,
    pad_token_id: int,
) -> tuple[torch.Tensor, torch.Tensor]:
    ids = torch.as_tensor(token_ids, dtype=torch.long)[:context_length]
    valid = torch.ones(ids.numel(), dtype=torch.bool)
    if ids.numel() < context_length:
        pad = context_length - ids.numel()
        ids = torch.cat((ids, torch.full((pad,), pad_token_id, dtype=torch.long)))
        valid = torch.cat((valid, torch.zeros(pad, dtype=torch.bool)))
    return ids, valid


def assistant_eligibility(
    length: int,
    assistant_start: int,
    assistant_stop: int,
    *,
    device: torch.device | None = None,
) -> torch.Tensor:
    if not 0 <= assistant_start < assistant_stop <= length:
        raise ValueError("invalid assistant target span")
    eligible = torch.zeros(length, dtype=torch.bool, device=device)
    eligible[assistant_start:assistant_stop] = True
    return eligible
