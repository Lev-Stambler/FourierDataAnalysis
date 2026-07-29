import itertools

import numpy as np
from qwen_lsh_monarch.config import monarch_screen
from qwen_lsh_monarch.data import (
    deterministic_span,
    deterministic_spans,
    document_digest,
    split_for_digest,
)
from qwen_lsh_monarch.study import (
    final_trials,
    screen_trials,
    select_topology,
    tuning_trials,
)


def result(trial, kl, parameters=100):
    return {
        "architecture": trial.architecture.to_dict(),
        "validation": {"kl": kl},
        "trainable_parameters": parameters,
        "lr": trial.lr,
    }


def test_grid_is_16_screen_2_tune_and_3_seeded_finals():
    configs = monarch_screen()
    assert len(configs) == 16
    assert {config.form for config in configs} == {
        "sequential", "residual_one", "residual_ffn"
    }
    assert {config.depth for config in configs} == {1, 2, 4, 8}
    assert {
        config.expansion for config in configs
        if config.form == "residual_ffn"
    } == {1, 4}

    screens = [
        result(trial, 1.0 if index == 5 else 10.0)
        for index, trial in enumerate(screen_trials())
    ]
    selected = select_topology(screens)
    tunes = tuning_trials(selected)
    assert len(tunes) == 2
    assert {trial.lr for trial in tunes} == {3e-4, 3e-3}
    tune_results = [
        result(trial, 0.5 if trial.lr == 3e-4 else 2.0)
        for trial in tunes
    ]
    finals = final_trials(screens, tune_results)
    assert len(finals) == 3
    assert {trial.seed for trial in finals} == {0, 1, 2}
    assert {trial.lr for trial in finals} == {3e-4}
    assert {trial.steps for trial in finals} == {4_000}
    assert {trial.audit_every for trial in finals} == {250}


def test_selection_tie_breaks_by_parameter_count():
    trials = screen_trials()[:2]
    rows = [result(trials[0], 1.0, 20), result(trials[1], 1.0, 10)]
    assert select_topology(rows) == trials[1].architecture


def test_data_windows_remain_deterministic_and_nonoverlapping():
    digest = document_digest("stable LSH experiment document")
    assert split_for_digest(digest) == split_for_digest(digest)
    ids = np.arange(17 * 40 + 3)
    assert len(deterministic_span(ids, digest)) == 17
    first = deterministic_spans(ids, digest)
    second = deterministic_spans(ids, digest)
    assert len(first) == 32
    for left, right in zip(first, second, strict=True):
        np.testing.assert_array_equal(left, right)
    starts = [int(window[0]) for window in first]
    assert all(b - a == 17 for a, b in itertools.pairwise(starts))
