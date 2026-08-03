from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest
import torch
import torch.nn.functional as F

from expv2.exp1.campaign import (
    COARSE_LM_TOKENS,
    COARSE_SYNTHETIC_TOKENS,
    FINAL_LM_TOKENS,
    ROBUST_LM_TOKENS,
    ROBUST_SYNTHETIC_TOKENS,
    _document_bootstrap,
)
from expv2.exp1.config import (
    CONTEXT_LENGTH,
    KRONECKER_SHAPES,
    TINY_STORIES_ID,
    TINY_STORIES_REVISION,
)
from expv2.exp1.data import (
    CORPUS_SCHEMA,
    _keep_validation_document,
    load_document_ids,
    load_windows,
    validate_manifest,
)
from expv2.exp1.model import (
    DenseCausalResidual,
    KroneckerResidual,
    build_model,
    inventory,
)
from expv2.exp1.optimizer import BatchedMuon, build_muon
from expv2.exp1.synthetic import TASKS, make_batch
from expv2.exp1.training import TrainingRecipe, schedule_multiplier
from expv2.exp1.utils import sha256


def test_locked_parameter_match() -> None:
    values = inventory()
    assert values["maximum_total_mismatch_fraction"] < 0.01
    assert values["maximum_body_mismatch_fraction"] < 0.01
    models = values["models"]
    assert {models[name]["body_parameters"] for name in KRONECKER_SHAPES} == {
        1_626_240
    }
    assert models["dense"]["total_parameters"] == 2_146_048
    assert models["transformer"]["total_parameters"] == 2_154_496


def test_kronecker_factored_materialized_forward_backward() -> None:
    torch.manual_seed(2)
    value = torch.randn(2, 7, 6, dtype=torch.float64, requires_grad=True)
    block = KroneckerResidual(6, 3, 7, 5).double()
    factored = block.branch(value)
    reference = block.materialized_branch(value)
    assert torch.allclose(factored, reference, atol=1e-10, rtol=1e-10)
    left = torch.autograd.grad(factored.square().sum(), value, retain_graph=True)[0]
    right = torch.autograd.grad(reference.square().sum(), value)[0]
    assert torch.allclose(left, right, atol=1e-10, rtol=1e-10)
    assert torch.count_nonzero(torch.triu(block.token.matrix(), diagonal=1)) == 0


def test_dense_chunked_reference_forward_backward() -> None:
    torch.manual_seed(4)
    value = torch.randn(2, 6, 4, dtype=torch.float64, requires_grad=True)
    block = DenseCausalResidual(6, 4, pair_chunk=5).double()
    chunked = block.branch(value)
    reference = block.materialized_branch(value)
    assert torch.allclose(chunked, reference, atol=1e-10, rtol=1e-10)
    left = torch.autograd.grad(chunked.square().sum(), value, retain_graph=True)[0]
    right = torch.autograd.grad(reference.square().sum(), value)[0]
    assert torch.allclose(left, right, atol=1e-10, rtol=1e-10)


@pytest.mark.parametrize("variant", [*KRONECKER_SHAPES, "dense", "transformer"])
def test_models_are_causal_and_pure_muon(variant: str) -> None:
    torch.manual_seed(8)
    model = build_model(variant, vocab_size=32, context_length=8)
    tokens = torch.randint(0, 32, (1, 8))
    changed = tokens.clone()
    changed[:, 4:] = torch.randint(0, 32, (1, 4))
    with torch.no_grad():
        left = model.hidden(tokens)[:, :4]
        right = model.hidden(changed)[:, :4]
    assert torch.allclose(left, right, atol=1e-6, rtol=1e-6)
    assert all(parameter.ndim >= 2 for parameter in model.parameters())
    optimizer, routing = build_muon(model, lr=0.01)
    assert routing["parameters"] == sum(p.numel() for p in model.parameters())
    assert isinstance(optimizer, BatchedMuon)


def test_batched_muon_matches_torch_original_adjustment() -> None:
    torch.manual_seed(12)
    batched = torch.nn.Parameter(torch.randn(2, 8, 8))
    references = [
        torch.nn.Parameter(matrix.detach().clone()) for matrix in batched.detach()
    ]
    ours = BatchedMuon([batched], lr=0.02, weight_decay=0.0)
    standard = torch.optim.Muon(
        references,
        lr=0.02,
        weight_decay=0.0,
        adjust_lr_fn="original",
    )
    for _ in range(2):
        gradient = torch.randn_like(batched)
        batched.grad = gradient.clone()
        for parameter, item in zip(references, gradient, strict=True):
            parameter.grad = item.clone()
        ours.step()
        standard.step()
    assert torch.allclose(batched, torch.stack(references), atol=2e-3, rtol=2e-3)


@pytest.mark.parametrize("task", TASKS)
@pytest.mark.parametrize("split", ["train", "id", "ood"])
def test_synthetic_tasks_are_deterministic_and_causal(task: str, split: str) -> None:
    first = make_batch(task, 3, seed=91, split=split)
    second = make_batch(task, 3, seed=91, split=split)
    assert torch.equal(first.inputs, second.inputs)
    assert torch.equal(first.targets, second.targets)
    assert torch.equal(first.mask, second.mask)
    assert first.inputs.shape == (3, CONTEXT_LENGTH)
    assert first.mask.any()
    assert int(first.inputs.max()) < 128
    # Every answer is produced at or after all defining key/value material.
    if task == "associative-recall":
        assert int(first.mask.nonzero()[:, 1].min()) >= 64
    if task == "two-hop-recall":
        assert int(first.mask.nonzero()[:, 1].min()) >= 80


def test_schedule_is_token_based() -> None:
    constant = TrainingRecipe(0.01)
    cosine = TrainingRecipe(0.01, schedule="warmup-cosine")
    assert schedule_multiplier(constant, 1, 1_000_000) == 1.0
    assert schedule_multiplier(cosine, 128, 1_000_000) < 1.0
    assert schedule_multiplier(cosine, 50_000, 1_000_000) == pytest.approx(1.0)
    assert schedule_multiplier(cosine, 1_000_000, 1_000_000) == pytest.approx(0.1)


def test_all_campaign_budgets_are_whole_contexts() -> None:
    budgets = (
        COARSE_SYNTHETIC_TOKENS,
        ROBUST_SYNTHETIC_TOKENS,
        COARSE_LM_TOKENS,
        ROBUST_LM_TOKENS,
        FINAL_LM_TOKENS,
    )
    assert all(value > 0 and value % CONTEXT_LENGTH == 0 for value in budgets)


def _fake_corpus(root: Path) -> None:
    root.mkdir()
    tokenizer = root / "tokenizer.json"
    tokenizer.write_text("fake-tokenizer")
    files = {}
    for split, ids in {
        "train": np.array([1, 1], dtype=np.uint64),
        "validation": np.array([2, 2], dtype=np.uint64),
        "test": np.array([3, 3], dtype=np.uint64),
    }.items():
        windows = np.zeros((2, CONTEXT_LENGTH + 1), dtype=np.uint16)
        window_path = root / f"{split}.npy"
        document_path = root / f"{split}-documents.npy"
        np.save(window_path, windows)
        np.save(document_path, ids)
        files[split] = {
            "path": window_path.name,
            "sha256": sha256(window_path),
            "windows": 2,
            "prediction_tokens": 2 * CONTEXT_LENGTH,
            "documents": 1,
            "document_set_sha256": str(ids[0]),
            "source_split": "train" if split == "train" else "validation",
            "selection": "all",
            "document_ids_path": document_path.name,
            "document_ids_sha256": sha256(document_path),
        }
    manifest = {
        "schema": CORPUS_SCHEMA,
        "dataset_id": TINY_STORIES_ID,
        "dataset_revision": TINY_STORIES_REVISION,
        "context_length": CONTEXT_LENGTH,
        "vocab_size": 4096,
        "window_policy": "test",
        "validation_test_policy": "test",
        "tokenizer_sha256": sha256(tokenizer),
        "files": files,
    }
    (root / "manifest.json").write_text(json.dumps(manifest))


def test_corpus_validation_and_test_seal(tmp_path: Path) -> None:
    root = tmp_path / "data"
    _fake_corpus(root)
    validate_manifest(root)
    assert load_windows(root, "validation").shape == (2, CONTEXT_LENGTH + 1)
    with pytest.raises(PermissionError):
        load_windows(root, "test")
    with pytest.raises(PermissionError):
        load_document_ids(root, "test")
    assert load_windows(root, "test", allow_test=True).shape[0] == 2
    assert load_document_ids(root, "test", allow_test=True).tolist() == [3, 3]


def test_validation_test_hash_parity_is_disjoint() -> None:
    texts = [f"story-{index}" for index in range(100)]
    validation = {text for text in texts if _keep_validation_document(text, "validation")}
    test = {text for text in texts if _keep_validation_document(text, "test")}
    assert validation.isdisjoint(test)
    assert validation | test == set(texts)


def test_document_bootstrap_detects_consistent_win() -> None:
    documents = np.array([1, 1, 2, 2, 3, 3], dtype=np.uint64)
    candidate = [np.array([1.0, 1.1, 1.0, 1.1, 1.0, 1.1]) for _ in range(3)]
    control = [np.array([1.2, 1.3, 1.2, 1.3, 1.2, 1.3]) for _ in range(3)]
    result = _document_bootstrap(candidate, control, documents, replicates=500)
    assert result["upper_95"] < 0
    assert result["documents"] == 3
