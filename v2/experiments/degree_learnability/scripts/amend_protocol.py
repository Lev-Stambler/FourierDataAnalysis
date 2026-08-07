"""Protocol amendment v1.1 (M6, post-grid diagnosis; documented deviation).

Changes from v1 (all applied uniformly, before M10 analysis, recorded here):
1. F3/F4 rebuilt in exponential form with stronger signal (amp=1.0, beta=32.0).
   Reason: at v1's additive amplitude (0.9/q) the Bayes floors were 4.97-4.98 bits,
   within theta of the uniform CE, making T* degenerate (trivially reached at the
   first checkpoint) — verified in the M6 difficulty table.
2. Secondary difficulty scalars added (computable from the checkpointed curves of
   existing runs): final_gap_bits, norm_remaining, T_half. Primary registered
   metric T*(theta=0.05) is retained verbatim.
Everything else from v1 is unchanged.
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import sys
sys.path.insert(0, str(Path(__file__).parent.parent))
from dlx.grid import _cells_from_ids  # noqa: E402
from dlx.grid import make_family  # noqa: E402


def main():
    cfg_dir = Path(__file__).parent.parent / "configs"
    v1 = json.loads((cfg_dir / "protocol_v1.json").read_text())

    v11 = dict(v1)
    v11["protocol_id"] = "dlx-v1.1"
    v11["supersedes"] = v1["protocol_hash"]
    v11["amendment_date"] = "2026-08-04"
    v11["amendment_note"] = (
        "F3/F4 rebuilt in exponential form (amp=1.0, beta=32.0) after M6 diagnosis "
        "showed v1 amplitudes left Bayes floors within theta of uniform CE "
        "(T* degenerate). Secondary difficulty scalars added: final_gap_bits, "
        "norm_remaining, T_half — uniform across all cells, extractable from saved "
        "curves. Primary metric T*(theta=0.05) retained verbatim. Cells affected: "
        "the 12 F3/F4 grid cells retrained under this protocol; all other cells "
        "inherit v1 results unchanged.")

    # updated family versions for the rebuilt families (others inherited from v1)
    versions = dict(v1["family_versions"])
    proto_like = {"domain": v1["domain"]}
    for tag, fam_name, params in _cells_from_ids(proto_like):
        if fam_name in ("F3_random_poly", "F4_mixed_profile"):
            versions[tag] = make_family(fam_name, params).version
    v11["family_versions"] = versions

    v11["metrics"] = dict(v1["metrics"])
    v11["metrics"]["secondary_scalars"] = {
        "final_gap_bits": "val CE - bayes floor at the max budget",
        "norm_remaining": ("(final CE - floor) / (init CE - floor); fraction of the "
                           "reducible gap remaining; init CE = first checkpoint "
                           "(~uniform predictor, common reference by construction)"),
        "T_half": "first checkpoint where CE <= floor + (init CE - floor)/2",
        "status": ("added by amendment v1.1; applied uniformly to all cells; primary "
                   "registered metric T*(theta=0.05) unchanged"),
    }

    canonical = json.dumps(v11, sort_keys=True)
    v11["protocol_hash"] = hashlib.sha256(canonical.encode()).hexdigest()

    out = cfg_dir / "protocol_v1.1.json"
    out.write_text(json.dumps(v11, indent=2, sort_keys=True))
    print(f"protocol_hash(v1.1) = {v11['protocol_hash']}")
    print("updated families:", [t for t in versions if t.startswith(("F3", "F4"))])
    print(f"wrote {out}")


if __name__ == "__main__":
    main()
