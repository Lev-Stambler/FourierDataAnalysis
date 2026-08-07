"""M7 stage C: train the fixed student on every R1 rung x seed (local CPU,
protocol v1.2 student budget), writing full manifests + metrics. Also persists the
measured suffix-filtration profile next to each cell for M10 analysis.
"""

from __future__ import annotations

import json
import sys
import time
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))

from dlx.data.corpora import CACHE
from dlx.data.corpus_family import CorpusFamily
from dlx.grid import load_protocol
from dlx.learners.transformer import TransformerConfig
from dlx.training.run import train_run

OUT = Path("runs/local/m7")
SEEDS = (0, 1, 2)

RUNKG_STREAMS = {
    1: ("r1_iid_q4096_n20000000_s1001", 4096, "iid_random_tokens"),
    2: ("r2_f2s2_n20000000_s1002", 32, "synthetic_F2_s2_corpus"),
    3: ("r3_tinystories_bpe4096_n20000000", 4096, "roneneldan_TinyStories"),
    4: ("r4_enwik8_n20000000", 256, "enwik8"),
    5: ("r5_wikitext2_bpe10000_n2000000", 10000, "Salesforce_wikitext2"),
    6: ("r6_codeparrot_py_bpe10000_n20000000", 10000, "codeparrot_python"),
    7: ("r7_arith_n20000000_s1007", 16, "arithmetic_corpus"),
}


def main():
    only = sys.argv[1] if len(sys.argv) > 1 else None
    proto = load_protocol()
    budget = proto.get("student_budget_tokens", 5_000_000)
    floors = json.loads((OUT / "stage_b_floors.json").read_text())
    # rung 1 is iid uniform over the rung-3 vocab: exact floor log2(4096)=12, not
    # Qwen's text entropy (random token ids decode to meaningless bytes, inflating it)
    floors["1"]["floor_bits_per_student_token"] = 12.0
    floors["1"]["method"] = "iid uniform over rung-3 vocab (exact generating-law floor)"
    stage_a = json.loads((OUT / "stage_a.json").read_text())
    OUT.mkdir(parents=True, exist_ok=True)

    base_cfg = proto["learner_config"]
    results = []
    for rung, (key, q, name) in RUNKG_STREAMS.items():
        if only and str(rung) != only:
            continue
        floor = floors[str(rung)]["floor_bits_per_student_token"]
        tokens = np.load(CACHE / f"{key}.npy")
        for seed in SEEDS:
            cell_id = f"M7/rung{rung}/{name}/s{seed}"
            out_dir = OUT / cell_id.replace("/", "__")
            if (out_dir / "manifest.json").exists():
                print(f"skip (exists): {cell_id}", flush=True)
                continue
            fam = CorpusFamily(tokens, q=q, L=64, name=name, floor_bits=float(floor),
                               cyclic=True, shuffle_seed=seed)
            cfg = TransformerConfig(vocab=q, ctx_len=base_cfg["ctx_len"],
                                    d_model=base_cfg["d_model"],
                                    n_layers=base_cfg["n_layers"],
                                    n_heads=base_cfg["n_heads"])
            t0 = time.time()
            try:
                m = train_run(fam, cfg, budget_tokens=budget, seed=seed, out_dir=out_dir,
                              cell_id=cell_id, protocol_hash=proto["protocol_hash"],
                              device="cpu", n_checkpoints=20,
                              tokens_per_step=proto.get("tokens_per_step", 1024))
            except RuntimeError as e:
                print(f"  {cell_id}: EXHAUSTED ({e})", flush=True)
                results.append({"cell_id": cell_id, "error": str(e)[:80]})
                continue
            # persist measured profile + floor method alongside the run
            (out_dir / "rung_meta.json").write_text(json.dumps({
                "rung": rung, "name": name, "q": q,
                "floor_method": floors[str(rung)].get("method", floors[str(rung)].get("teacher_model", "")),
                "measured_profile": stage_a["profiles"].get(str(rung)),
                "rung_meta": stage_a["rungs"].get(str(rung)),
            }, indent=2))
            print(f"  {cell_id}: gap={m['final_gap_bits']:.4f} T*={m['T_star']} "
                  f"({time.time()-t0:.0f}s)", flush=True)
            results.append({"cell_id": cell_id, "T_star": m["T_star"],
                            "final_gap_bits": m["final_gap_bits"],
                            "norm_remaining": (m["val_ce_bits"][-1] - floor) /
                            max(m["val_ce_bits"][0] - floor, 1e-9)})
    (OUT / "stage_c_results.json").write_text(json.dumps(results, indent=2))
    print("wrote", OUT / "stage_c_results.json")


if __name__ == "__main__":
    main()
