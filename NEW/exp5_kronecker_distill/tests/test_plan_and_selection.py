import copy

import pytest
from qwen_kron_distill.config import (
    EXTENSION_2X_EXAMPLES,
    EXTENSION_4X_EXAMPLES,
    FINAL_EXAMPLES,
    MID_EXAMPLES,
    PLAN_SCHEMA,
    RESULT_SCHEMA,
    SCREEN_EXAMPLES,
    Architecture,
    Cell,
    depth_cells,
    push_dense_vocabulary_cells,
    push_wide_cells,
    rank_cells,
    study_plan,
    wsd_multiplier,
)
from qwen_kron_distill.study import (
    continuation_cell,
    continuation_improved,
    select_depths,
    select_overall,
    select_ranks,
    should_extend,
)


def result(cell: Cell, kl: float, parameters: int = 1000) -> dict:
    return {
        "schema": RESULT_SCHEMA,
        "status": "complete",
        "label": cell.label,
        "cell": cell.to_dict(),
        "examples_seen": cell.target_examples,
        "validation": {"kl": kl},
        "inventory": {"trainable_parameters": parameters},
    }


def test_plan_commits_exact_grid_optimizer_and_runtime():
    plan = study_plan()

    assert plan["schema"] == PLAN_SCHEMA
    assert len(depth_cells()) == 12
    assert {cell.architecture.depth for cell in depth_cells()} == {1, 2, 4, 8, 16, 32}
    assert {cell.architecture.factor_order for cell in depth_cells()} == {2, 3}
    assert plan["student"]["embedding_width"] == 64
    assert plan["data"]["context_length"] == 16
    assert plan["optimizer"]["factor"] == "normuon"
    assert plan["optimizer"]["factor_lr_grid"] == [3e-3, 1e-2, 3e-2]
    assert plan["runtime"]["world_size"] == 8
    assert plan["runtime"]["global_batch"] == 65_536
    assert plan["runtime"]["global_token_batch"] == 1_048_576
    assert plan["teacher"]["live"] is True


def test_wsd_is_driven_by_examples_and_reaches_zero():
    assert wsd_multiplier(1) == pytest.approx(1 / 262_144)
    assert wsd_multiplier(262_144) == 1.0
    assert wsd_multiplier(262_145) == 1.0
    assert wsd_multiplier(FINAL_EXAMPLES) == 0.0


def test_wide_screen_crosses_width_and_learning_rate():
    cells = push_wide_cells()

    assert len(cells) == 5
    assert {cell.architecture.embedding_width for cell in cells} == {
        128,
        256,
        384,
    }
    assert {cell.factor_lr for cell in cells} == {3e-2, 1e-1}
    assert all(cell.stage == "width" for cell in cells)


def test_dense_vocabulary_screen_isolates_head_width_at_fixed_lr():
    cells = push_dense_vocabulary_cells()

    assert len(cells) == 3
    assert {
        cell.architecture.vocabulary_width for cell in cells
    } == {64, 128, 256}
    assert {
        cell.architecture.embedding_width for cell in cells
    } == {64}
    assert {cell.factor_lr for cell in cells} == {0.2}
    assert {cell.auxiliary_lr for cell in cells} == {0.2}
    assert all(cell.stage == "width" for cell in cells)


def test_staged_selection_and_tie_breaks():
    depth_results = []
    for cell in depth_cells():
        distance = abs(cell.architecture.depth - 4)
        depth_results.append(result(cell, 0.2 + distance / 100))
    winners = select_depths(depth_results)
    assert {
        order: value["cell"]["architecture"]["depth"]
        for order, value in winners.items()
    } == {2: 4, 3: 4}

    rank_results = []
    for cell in rank_cells({2: 4, 3: 4}):
        # Tie rank 2 with rank 1; the lower rank must win.
        kl = 0.2 if cell.architecture.rank == 2 else 0.3
        rank_results.append(result(cell, kl))
    rank_winners = select_ranks(winners, rank_results)
    assert {
        order: value["cell"]["architecture"]["rank"]
        for order, value in rank_winners.items()
    } == {2: 1, 3: 1}


def test_continuation_gate_and_overall_selection():
    sources = []
    mids = []
    for order, kl, parameters in ((2, 0.20, 2000), (3, 0.19, 3000)):
        source_cell = Cell(
            stage="rank",
            architecture=Architecture(
                factor_order=order,
                depth=4,
                rank=2,
            ),
            target_examples=SCREEN_EXAMPLES,
        )
        source = result(source_cell, kl + 0.002, parameters)
        mid_cell = continuation_cell(
            source,
            stage="mid",
            target_examples=MID_EXAMPLES,
        )
        sources.append(source)
        mids.append(result(mid_cell, kl, parameters))

    assert all(
        continuation_improved(source, mid)
        for source, mid in zip(sources, mids, strict=True)
    )
    assert select_overall(mids)["cell"]["architecture"]["factor_order"] == 3

    insufficient = copy.deepcopy(mids[0])
    insufficient["validation"]["kl"] = sources[0]["validation"]["kl"] - 0.0009
    assert not continuation_improved(sources[0], insufficient)


def test_extension_cells_and_plateau_gate():
    mid_cell = Cell(
        stage="mid",
        architecture=Architecture(
            factor_order=2,
            depth=32,
            rank=8,
            vocabulary_width=64,
        ),
        target_examples=MID_EXAMPLES,
        factor_lr=0.2,
        auxiliary_lr=0.2,
    )
    mid = result(mid_cell, 2.3, 17_070_016)
    final_cell = continuation_cell(
        mid,
        stage="final",
        target_examples=FINAL_EXAMPLES,
    )
    final = result(final_cell, 2.1, 17_070_016)
    assert should_extend(mid, final)

    extension_cell = continuation_cell(
        final,
        stage="extend2x",
        target_examples=EXTENSION_2X_EXAMPLES,
    )
    extension = result(extension_cell, 2.05, 17_070_016)
    assert not should_extend(final, extension)

    target = result(extension_cell, 0.99, 17_070_016)
    assert not should_extend(final, target)

    max_cell = continuation_cell(
        extension,
        stage="extend4x",
        target_examples=EXTENSION_4X_EXAMPLES,
    )
    assert max_cell.target_examples == EXTENSION_4X_EXAMPLES


def test_rank_grid_requires_selected_depth_for_each_order():
    with pytest.raises(ValueError, match="cover"):
        rank_cells({2: 4})
