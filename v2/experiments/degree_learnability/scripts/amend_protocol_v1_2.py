"""Protocol amendment v1.2 (M7/M8 compute tractability; documented deviation).

Changes from v1.1 (all applied uniformly, recorded before the M7/M8 student runs):
1. Student training budget for the R1 language and R2 image ladders reduced from
   the v1 train_token_cap of 20,000,000 to STUDENT_BUDGET = 5,000,000
   tokens/codes per cell. Reason: 39 cells x 20M tokens is infeasible within the
   CPU budget cap (< $5) and wall-clock limits; 5M tokens is sufficient to expose
   the difficulty gradient (the difficulty signal is read from the learning curve /
   normalized_remaining, and T* saturates for natural corpora regardless).
2. M7/M8 student cells execute on LOCAL CPU (not Modal) to stay within the CPU
   budget; manifest discipline unchanged. Recorded per the goal's local-fallback
   allowance.
3. connect-4 resolves to OpenML did 1591 (exact-name, first-registered), which is a
   126-feature / many-class variant, not the 3-class UCI board dataset. Recorded as
   a resolution outcome, retained as a large control.
4. R2 image rung conversion to 32x32 grayscale so one fixed VQ architecture applies
   uniformly across the ladder (color dropped; documented).
Everything else from v1.1 is unchanged.
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

STUDENT_BUDGET = 5_000_000


def main():
    cfg_dir = Path(__file__).parent.parent / "configs"
    v11 = json.loads((cfg_dir / "protocol_v1.1.json").read_text())

    v12 = dict(v11)
    v12["protocol_id"] = "dlx-v1.2"
    v12["supersedes"] = v11["protocol_hash"]
    v12["amendment_date"] = "2026-08-04"
    v12["student_budget_tokens"] = STUDENT_BUDGET
    v12["student_execution"] = "local-cpu (budget + tractability; manifest discipline unchanged)"
    v12["amendment_note"] = (
        "Student training budget for R1/R2 ladders reduced to 5M tokens/codes per "
        "cell (from 20M) for CPU-budget tractability; difficulty read from learning "
        "curves / normalized_remaining. M7/M8 students run on local CPU. connect-4 "
        "resolved to did 1591 variant. R2 images converted to 32x32 grayscale for a "
        "uniform fixed VQ architecture. See scripts/amend_protocol_v1_2.py docstring.")

    canonical = json.dumps(v12, sort_keys=True)
    v12["protocol_hash"] = hashlib.sha256(canonical.encode()).hexdigest()

    out = cfg_dir / "protocol_v1.2.json"
    out.write_text(json.dumps(v12, indent=2, sort_keys=True))
    print(f"protocol_hash(v1.2) = {v12['protocol_hash']}")
    print(f"wrote {out}")


if __name__ == "__main__":
    main()
