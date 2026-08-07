"""Deterministic seed tree (PLAN §12.5).

Every RNG is derived from explicit string parts; identical parts give identical
streams on any platform.
"""

from __future__ import annotations

import hashlib

import numpy as np


def seed_from(*parts: object) -> int:
    """Derive a 63-bit nonnegative seed from arbitrary string-able parts."""
    h = hashlib.sha256()
    for p in parts:
        h.update(str(p).encode("utf-8"))
        h.update(b"\x1f")  # unit separator: disambiguates part boundaries
    return int.from_bytes(h.digest()[:8], "big") % (2**63)


def rng_for(protocol_hash: str, family_version: str, cell_id: str, split: str) -> np.random.Generator:
    """The canonical RNG for one (protocol, family, cell, split) combination."""
    return np.random.default_rng(seed_from(protocol_hash, family_version, cell_id, split))
