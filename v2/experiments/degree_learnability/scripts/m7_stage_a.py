"""M7 stage A: build + cache all R1 ladder token streams, compute measured
(suffix-filtration) degree profiles, and write rung metadata. No training here.
"""

from __future__ import annotations

import json
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

from dlx.data.corpora import (CACHE, rung2_synthetic_f2, rung3_tinystories,
                              rung4_enwik8, rung5_wikitext2, rung6_codeparrot,
                              rung7_arithmetic)
from dlx.profiles.corpus_profile import suffix_filtration_profile

N_TOKENS = 20_000_000
OUT = Path("runs/local/m7")


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    report = {}

    t0 = time.time()
    print("[rung 3] TinyStories (BPE 4096)...", flush=True)
    tok3, meta3 = rung3_tinystories(N_TOKENS)
    report[3] = meta3.to_dict()
    print(f"  {meta3.n_tokens} tokens in {time.time()-t0:.0f}s", flush=True)

    from dlx.data.corpora import rung1_iid_random
    t0 = time.time()
    print("[rung 1] iid random (vocab-matched to rung 3)...", flush=True)
    tok1, meta1 = rung1_iid_random(N_TOKENS, meta3.vocab_size)
    report[1] = meta1.to_dict()
    print(f"  {meta1.n_tokens} tokens in {time.time()-t0:.0f}s", flush=True)

    t0 = time.time()
    print("[rung 2] synthetic F2 corpus...", flush=True)
    tok2, meta2 = rung2_synthetic_f2(N_TOKENS)
    report[2] = meta2.to_dict()
    print(f"  {meta2.n_tokens} tokens in {time.time()-t0:.0f}s", flush=True)

    t0 = time.time()
    print("[rung 4] enwik8 (bytes)...", flush=True)
    tok4, meta4 = rung4_enwik8(N_TOKENS)
    report[4] = meta4.to_dict()
    print(f"  {meta4.n_tokens} tokens in {time.time()-t0:.0f}s", flush=True)

    t0 = time.time()
    print("[rung 5] wikitext-2-raw (BPE 10k)...", flush=True)
    tok5, meta5 = rung5_wikitext2(min(N_TOKENS, 2_000_000))
    report[5] = meta5.to_dict()
    print(f"  {meta5.n_tokens} tokens in {time.time()-t0:.0f}s", flush=True)

    t0 = time.time()
    print("[rung 6] codeparrot python (BPE 10k)...", flush=True)
    tok6, meta6 = rung6_codeparrot(N_TOKENS)
    report[6] = meta6.to_dict()
    print(f"  {meta6.n_tokens} tokens in {time.time()-t0:.0f}s", flush=True)

    t0 = time.time()
    print("[rung 7] arithmetic corpus...", flush=True)
    tok7, meta7 = rung7_arithmetic(N_TOKENS)
    report[7] = meta7.to_dict()
    print(f"  {meta7.n_tokens} tokens in {time.time()-t0:.0f}s", flush=True)

    # measured degree profiles (oracle-free suffix filtration)
    tokens_by_rung = {1: tok1, 2: tok2, 3: tok3, 4: tok4, 5: tok5, 6: tok6, 7: tok7}
    q_by_rung = {r: report[r]["vocab_size"] for r in report}
    profiles = {}
    for r, tok in tokens_by_rung.items():
        q = q_by_rung[r]
        k_max = 3 if q**4 <= (1 << 26) else 2  # keep group keys tractable
        prof = suffix_filtration_profile(tok, q=q, L=64, k_max=k_max, stride=1)
        profiles[r] = prof
        print(f"[profile rung {r}] R={[round(v,4) for v in prof['R']]} "
              f"coverage={round(prof['coverage'][-1],3)}")

    out = OUT / "stage_a.json"
    out.write_text(json.dumps({"rungs": report, "profiles": profiles}, indent=2))
    print(f"wrote {out}")


if __name__ == "__main__":
    main()
