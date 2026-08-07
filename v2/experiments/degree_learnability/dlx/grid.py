"""Grid cell generation from the frozen protocol (M6).

Cells: one run per (family, seed) at the max budget; the protocol's budget grid
is then read off the checkpointed val curve by analysis/difficulty.py (documented
efficiency interpretation: PLAN §6 budget grid = checkpoint extraction from one
max-budget run, cosine schedule over the max budget — no redundant runs).
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path

from .families import (F1Markov, F2SubsetSum, F3RandomPoly, F4MixedProfile,
                       F5IID, F5MaxSum)

PROTOCOL_DIR = Path(__file__).parent.parent / "configs"


def protocol_path() -> Path:
    """Latest frozen protocol (amendments supersede; documented in each amendment)."""
    for name in ("protocol_v1.3.json", "protocol_v1.2.json", "protocol_v1.1.json",
                 "protocol_v1.json"):
        p = PROTOCOL_DIR / name
        if p.exists():
            return p
    raise FileNotFoundError("no frozen protocol in configs/")


def load_protocol(path: Path | None = None) -> dict:
    return json.loads(Path(path if path is not None else protocol_path()).read_text())


@dataclass(frozen=True)
class CellSpec:
    cell_id: str
    family: str
    family_params: dict
    seed: int
    budget_tokens: int

    def to_dict(self) -> dict:
        return {"cell_id": self.cell_id, "family": self.family,
                "family_params": self.family_params, "seed": self.seed,
                "budget_tokens": self.budget_tokens}

    @staticmethod
    def from_dict(d: dict) -> "CellSpec":
        return CellSpec(**d)


def make_family(spec_family: str, params: dict):
    if spec_family == "F1_markov":
        return F1Markov(**params)
    if spec_family == "F2_subset_sum":
        params = dict(params)
        params["lags"] = tuple(params["lags"])
        return F2SubsetSum(**params)
    if spec_family == "F3_random_poly":
        return F3RandomPoly(**params)
    if spec_family == "F4_mixed_profile":
        return F4MixedProfile(**params)
    if spec_family == "F5_iid":
        return F5IID(**params)
    if spec_family == "F5_max_sum":
        return F5MaxSum(**params)
    raise ValueError(f"unknown family {spec_family}")


def _cells_from_ids(protocol: dict) -> list[tuple[str, str, dict]]:
    """(tag, family_name, params) for every frozen synthetic cell."""
    q, L, eta = protocol["domain"]["q"], protocol["domain"]["L"], protocol["domain"]["eta"]
    out: list[tuple[str, str, dict]] = []
    for k in (1, 2, 3, 4, 6, 8):
        out.append((f"F1_k{k}", "F1_markov", {"q": q, "L": L, "k": k, "eta": eta}))
    for lag in (4, 16):
        out.append((f"copy_lag{lag}", "F2_subset_sum",
                    {"q": q, "L": L, "lags": [lag], "eta": eta}))
    for lags in ((1, 4), (1, 16), (1, 16, 32)):
        tag = "F2_s" + str(len(lags)) + "_" + "-".join(map(str, lags))
        out.append((tag, "F2_subset_sum", {"q": q, "L": L, "lags": list(lags), "eta": eta}))
    for d in (2, 4):
        out.append((f"F3_d{d}_M6", "F3_random_poly",
                    {"q": q, "L": L, "d": d, "M": 6, "amp": 1.0, "beta": 32.0,
                     "eta": eta, "draw_seed": 100 + d}))
    for r in (0.3, 0.7):
        out.append((f"F4_r{r}_K6_M2", "F4_mixed_profile",
                    {"q": q, "L": L, "K": 6, "M": 2, "r": r, "beta": 32.0,
                     "eta": eta, "draw_seed": 200 + int(r * 10)}))
    out.append(("F5_iid", "F5_iid", {"q": q, "L": L}))
    out.append(("F5_max_sum", "F5_max_sum", {"q": q, "L": L, "eta": eta}))
    return out


def synthetic_grid(protocol: dict) -> list[CellSpec]:
    """All M6 cells; family versions must match the frozen protocol."""
    max_budget = max(protocol["budget_grid_tokens"])
    seeds = protocol["seeds"]
    pinned = protocol["family_versions"]
    cells = []
    for tag, fam_name, params in _cells_from_ids(protocol):
        fam = make_family(fam_name, params)
        expect = pinned.get(tag)
        assert expect == fam.version, \
            f"family version mismatch for {tag}: {fam.version} != frozen {expect}"
        for seed in seeds:
            cells.append(CellSpec(cell_id=f"M6/{tag}/s{seed}", family=fam_name,
                                  family_params=params, seed=seed,
                                  budget_tokens=max_budget))
    return cells
