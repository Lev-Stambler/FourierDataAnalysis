"""Shared protocol and execution helpers for the corrected v1.4 text run."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import torch

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs" / "local" / "v14_text_anova"


def load_v14() -> dict:
    path = ROOT / "configs" / "protocol_v1.4.json"
    protocol = json.loads(path.read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol_v1.4 hash mismatch: {recorded} != {expected}")
    return protocol


def set_bounded_threads(protocol: dict) -> None:
    torch.set_num_threads(int(protocol["execution"]["max_torch_threads"]))
    try:
        torch.set_num_interop_threads(int(protocol["execution"]["max_interop_threads"]))
    except RuntimeError:
        pass

