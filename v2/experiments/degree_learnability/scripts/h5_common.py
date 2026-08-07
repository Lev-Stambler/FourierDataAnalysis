"""Shared helpers for the bounded local revised-H5 campaign."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import numpy as np
import torch

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs" / "local" / "h5_matched"


def load_v13() -> dict:
    path = ROOT / "configs" / "protocol_v1.3.json"
    proto = json.loads(path.read_text())
    got = proto["protocol_hash"]
    unhashed = dict(proto)
    unhashed.pop("protocol_hash")
    expect = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if got != expect:
        raise ValueError(f"protocol_v1.3 hash mismatch: {got} != {expect}")
    return proto


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with Path(path).open("rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def sha256_tokens(tokens: np.ndarray) -> str:
    return hashlib.sha256(np.ascontiguousarray(tokens.astype(np.uint8)).tobytes()).hexdigest()


def set_bounded_threads(proto: dict) -> None:
    n = int(proto["matched_h5"]["execution"]["max_torch_threads"])
    torch.set_num_threads(n)
    try:
        torch.set_num_interop_threads(1)
    except RuntimeError:
        pass


def reconstruct_initial_ce(tokens: np.ndarray, q: int, name: str, floor: float,
                           seed: int, proto: dict, data_version: str) -> float:
    """Recreate the exact seeded t=0 student and evaluate the held-out tail."""
    from dlx.data.corpus_family import CorpusFamily
    from dlx.learners.transformer import CausalTransformer, TransformerConfig
    from dlx.training.run import _seed_everything, _validate

    cfg0 = proto["learner_config"]
    cfg = TransformerConfig(vocab=q, ctx_len=cfg0["ctx_len"],
                            d_model=cfg0["d_model"], n_layers=cfg0["n_layers"],
                            n_heads=cfg0["n_heads"])
    _seed_everything(seed)
    model = CausalTransformer(cfg).to("cpu")
    fam = CorpusFamily(tokens, q=q, L=cfg.ctx_len, name=name, floor_bits=floor,
                       cyclic=True, shuffle_seed=seed, data_version=data_version)
    # CorpusFamily's val view is deterministic and ignores this RNG, but pass an
    # explicit one to retain the standard validation interface.
    return _validate(model, fam, np.random.default_rng(seed), "cpu", cfg.ctx_len)
