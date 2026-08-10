from __future__ import annotations

import json
from pathlib import Path

from dlx.protocol.confirmation import confirmation_cells
from dlx.protocol.frozen import file_sha256, load_frozen_protocol
from scripts.v27_analyze import _blocked_rank_test, _stratified_rmse_bootstrap

ROOT = Path(__file__).parent.parent


def test_v27_protocol_and_source_panel_are_frozen() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v2.7.json")
    sources = protocol["corpora"]["sources"]
    assert len(sources) == 24
    assert len({row["id"] for row in sources}) == 24
    counts = {}
    for source in sources:
        counts[source["stratum"]] = counts.get(source["stratum"], 0) + 1
    assert len(counts) == 6
    assert set(counts.values()) == {4}
    assert (
        file_sha256(ROOT / protocol["development"]["manifest"])
        == protocol["development"]["manifest_sha256"]
    )
    assert (
        file_sha256(ROOT / protocol["corpora"]["source_registry"])
        == protocol["corpora"]["source_registry_sha256"]
    )
    assert (
        file_sha256(ROOT / protocol["corpora"]["source_feasibility"])
        == protocol["corpora"]["source_feasibility_sha256"]
    )


def test_v27_grid_and_prediction_direction() -> None:
    protocol = load_frozen_protocol(ROOT / "configs/protocol_v2.7.json")
    cells = confirmation_cells(protocol)
    assert len(cells) == 48
    assert {(row["dataset"], row["seed"]) for row in cells} == {
        (source["id"], seed)
        for source in protocol["corpora"]["sources"]
        for seed in (0, 1)
    }
    predictions = json.loads(
        (ROOT / "runs/local/v27_marginal_locality/predictions.json").read_text()
    )
    assert predictions["positive_development_coefficient"] > 0.0
    assert len(predictions["predictions"]) == 24


def test_v27_stratified_decision_statistics_detect_ordered_signal() -> None:
    rows = []
    for stratum in ("a", "b", "c", "d", "e", "f"):
        for index in range(4):
            target = 0.25 + 0.1 * index
            rows.append(
                {
                    "stratum": stratum,
                    "normalized_learning_time": target,
                    "predictions": {
                        "intercept_only": 0.4,
                        "marginal_locality": target,
                    },
                }
            )
    bootstrap = _stratified_rmse_bootstrap(rows, samples=1_000, seed=2701)
    rank = _blocked_rank_test(rows, permutations=1_000, seed=2702)
    assert bootstrap["stratified_bootstrap_95_interval"][0] > 0.0
    assert rank["mean_rho"] == 1.0
    assert rank["one_sided_permutation_p"] < 0.05
