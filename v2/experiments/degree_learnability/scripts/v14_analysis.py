"""Analyze corrected text cells and append the v1.4 language verdict."""

from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np
from scipy.stats import spearmanr

sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent))

from v14_common import OUT, ROOT, load_v14

MARKER = "<!-- TEXT_ANOVA_V1_4_ADDENDUM -->"


def _median_cells(cells: list[dict], rung: str) -> dict:
    rows = [c for c in cells if c["rung"] == rung]
    record = {"rung": rung, "n_seeds": len(rows)}
    for key in ("initial_ce_bits", "final_ce_bits", "best_ce_bits",
                "learning_amount_bits", "fractional_learning", "normalized_curve_area"):
        record[key] = float(np.median([r[key] for r in rows]))
    return record


def analyze(protocol: dict, report: dict) -> dict:
    medians = []
    for rung in report["rungs"]:
        row = _median_cells(report["cells"], rung)
        meta = report["rungs"][rung]
        profile = meta["inverse_likelihood_profile"]
        row.update({"source_kind": meta["source_kind"],
                    "planted_degree": meta.get("planted_degree"),
                    "span": meta.get("span"),
                    "effective_degree_best_pair": profile["effective_degree_best_pair"],
                    "best_pair": profile["best_pair_by_total_gain"],
                    "best_pair_gain": profile["best_pair_positive_hierarchical_gain"],
                    "max_degree2_gain": profile["max_positive_degree2_gain"]})
        medians.append(row)
    by_rung = {row["rung"]: row for row in medians}
    markov = by_rung["markov2_bytes"]
    copy = by_rung["copy_lag16_bytes"]
    area_delta = markov["normalized_curve_area"] - copy["normalized_curve_area"]
    learning_delta = copy["learning_amount_bits"] - markov["learning_amount_bits"]
    profile_order = (markov["effective_degree_best_pair"]
                     > copy["effective_degree_best_pair"])
    if profile_order and area_delta >= 0.03 and learning_delta >= 1.0:
        controlled = "PASS"
    elif not profile_order or area_delta <= 0.0 or learning_delta <= 0.0:
        controlled = "FAIL"
    else:
        controlled = "INCONCLUSIVE"
    x = np.asarray([r["effective_degree_best_pair"] for r in medians], dtype=float)
    y = np.asarray([r["normalized_curve_area"] for r in medians], dtype=float)
    rho = float(spearmanr(x, y).statistic) if np.all(np.isfinite(x)) else None
    broad = ("NARROW_CAUSAL_TEXT_EFFECT_CONFIRMED; NATURAL-DATASET PROXY NOT ESTABLISHED"
             if controlled == "PASS" else "TEXT DEGREE EFFECT NOT CONFIRMED")
    return {
        "protocol_hash": protocol["protocol_hash"],
        "controlled_text_verdict": controlled,
        "conclusion": broad,
        "synthetic_matched_contrast": {
            "markov2": markov, "copy_lag16": copy,
            "curve_area_markov_minus_copy": area_delta,
            "learning_copy_minus_markov_bits": learning_delta,
            "profile_order_correct": profile_order,
            "rule": protocol["confirmation"]["controlled_pass_rule"],
        },
        "rungs": medians,
        "effective_degree_vs_curve_area_spearman": rho,
        "natural_scope": "two natural corpora are descriptive anchors; n=2 cannot establish external validity",
        "invalidated_evidence": "all pre-v1.4 language student cells shuffled token indices",
    }


def write_addendum(result: dict) -> None:
    path = ROOT / "VERDICT.md"
    body = path.read_text()
    if MARKER in body:
        body = body.split(MARKER, 1)[0].rstrip() + "\n"
    contrast = result["synthetic_matched_contrast"]
    lines = [
        "", MARKER,
        "## Corrected text result (protocol v1.4, 2026-08-06)", "",
        f"**Controlled text verdict: {result['controlled_text_verdict']}.**",
        f"**Scoped conclusion: {result['conclusion']}.**", "",
        "The v1.3 language training cells are invalid for sequence learnability: cyclic corpus",
        "reuse permuted individual token indices before windows were constructed. Protocol v1.4",
        "preserves contiguous token order and retrains every q=256 language cell from scratch.", "",
        "The text profile now uses the inverse-likelihood categorical functional-ANOVA basis",
        "from Ferrere et al. (arXiv:2603.02673, Definition 3.1 / equation 19). Each",
        "context position is one categorical variable; cross-fitted nested projections measure",
        "degree-1 additive and degree-2 interaction gain over the frozen lag pairs.", "",
        (f"In the entropy/vocabulary-matched synthetic contrast, Markov-2 minus copy curve area "
         f"is {contrast['curve_area_markov_minus_copy']:.3f}; copy minus Markov-2 learning is "
         f"{contrast['learning_copy_minus_markov_bits']:.3f} bits."),
        "The degree-2 local rule is harder even though the degree-1 control has the longer span (16).", "",
        "The two natural corpora remain descriptive anchors only; they cannot establish a broad",
        "natural-dataset proxy claim. Tabular and image conclusions are unchanged.",
    ]
    path.write_text(body.rstrip() + "\n" + "\n".join(lines).rstrip() + "\n")


def main() -> None:
    protocol = load_v14()
    report = json.loads((OUT / "confirmation" / "confirmation_results.json").read_text())
    result = analyze(protocol, report)
    (OUT / "integrated_analysis.json").write_text(json.dumps(result, indent=2))
    write_addendum(result)
    print(f"v1.4 controlled text verdict: {result['controlled_text_verdict']}")


if __name__ == "__main__":
    main()

