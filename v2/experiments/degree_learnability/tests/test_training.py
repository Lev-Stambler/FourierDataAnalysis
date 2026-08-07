"""M4 tests: metrics, CPU smoke train, bit-reproducibility, manifest discipline."""

import numpy as np
import pytest

torch = pytest.importorskip("torch")

from dlx.data.corpus_family import CorpusFamily
from dlx.families import F1Markov
from dlx.learners.transformer import (
    CausalTransformer,
    TransformerConfig,
    cross_entropy_bits,
)
from dlx.protocol import REQUIRED_FIELDS, load_manifest
from dlx.training.metrics import final_gap, grokking_jump, tokens_to_threshold
from dlx.training.run import train_run


def test_cyclic_corpus_preserves_adjacency_and_varies_start():
    tokens = np.arange(40, dtype=np.int64)
    fam = CorpusFamily(tokens, q=40, L=4, name="ordered", floor_bits=0.0,
                       val_fraction=0.2, cyclic=True, shuffle_seed=7)
    draw = fam.sample(70, np.random.default_rng(0))

    train_hi = int(len(tokens) * 0.8)
    transitions = set(zip(draw[:-1].tolist(), draw[1:].tolist()))
    allowed = {(i, i + 1) for i in range(train_hi - 1)} | {(train_hi - 1, 0)}
    assert transitions <= allowed
    assert not np.array_equal(draw[:train_hi], np.arange(train_hi))


def test_metrics_primitives():
    grid = [100, 200, 300, 400]
    ce = [2.0, 1.2, 0.55, 0.51]
    assert tokens_to_threshold(grid, ce, floor=0.5, theta=0.05) == 300
    assert tokens_to_threshold(grid, [2.0, 2.0, 2.0, 2.0], 0.5, 0.05) is None
    assert abs(final_gap(ce, 0.5) - 0.01) < 1e-12
    assert grokking_jump(grid, [3.0, 3.0, 0.4, 0.3], min_drop_bits=0.5, window=1) == 300


def _tiny(cell, seed=7):
    fam = F1Markov(q=8, L=16, k=1, eta=0.1)
    cfg = TransformerConfig(vocab=8, ctx_len=16, d_model=32, n_layers=2, n_heads=2,
                            lr=2e-3, weight_decay=0.05)
    return train_run(fam, cfg, budget_tokens=100_000, seed=seed, out_dir=cell,
                     cell_id="smoke", protocol_hash="test-proto", device="cpu",
                     n_checkpoints=5, tokens_per_step=2048)


def test_cpu_smoke_and_manifest(tmp_path):
    m = _tiny(tmp_path / "run")
    assert (tmp_path / "run" / "metrics.json").exists()
    man = load_manifest(tmp_path / "run")
    for f in REQUIRED_FIELDS:
        assert f in man and man[f] not in (None, "")
    # copy rule (k=1) with generous lr should approach the floor
    assert m["final_gap_bits"] < 0.35, m["val_ce_bits"]


def test_cpu_bit_reproducible(tmp_path):
    m1 = _tiny(tmp_path / "a", seed=11)
    m2 = _tiny(tmp_path / "b", seed=11)
    assert m1["val_ce_bits"] == m2["val_ce_bits"]
    assert m1["train_ce_bits"] == m2["train_ce_bits"]
    m3 = _tiny(tmp_path / "c", seed=12)
    assert m3["val_ce_bits"] != m1["val_ce_bits"]


def test_model_shapes_and_ce():
    cfg = TransformerConfig(vocab=8, ctx_len=16, d_model=32, n_layers=2, n_heads=2)
    model = CausalTransformer(cfg)
    xb = torch.randint(0, 8, (4, 16))
    logits = model(xb)
    assert logits.shape == (4, 16, 8)
    ce = cross_entropy_bits(logits, torch.randint(0, 8, (4, 16)))
    assert 2.5 < ce < 3.6  # near log2(8)=3 at init


@pytest.mark.parametrize(
    "position_encoding", ["learned_absolute", "sinusoidal", "alibi"]
)
def test_positional_configurations_are_causal_and_finite(position_encoding):
    cfg = TransformerConfig(
        vocab=8,
        ctx_len=16,
        d_model=32,
        n_layers=2,
        n_heads=4,
        position_encoding=position_encoding,
    )
    model = CausalTransformer(cfg).eval()
    left = torch.randint(0, 8, (2, 16))
    right = left.clone()
    right[:, 9:] = torch.randint(0, 8, (2, 7))
    with torch.no_grad():
        logits_left = model(left)
        logits_right = model(right)
    assert torch.isfinite(logits_left).all()
    assert logits_left.shape == (2, 16, 8)
    assert torch.equal(logits_left[:, :9], logits_right[:, :9])


def test_default_position_mode_preserves_historical_config_hash():
    cfg = TransformerConfig(
        vocab=256, ctx_len=64, d_model=64, n_layers=2, n_heads=4
    )
    assert cfg.config_hash == "6d7e38e4d696eb6e"
    assert "position_encoding" not in cfg.to_json()
    sinusoidal = TransformerConfig(
        vocab=256,
        ctx_len=64,
        d_model=64,
        n_layers=2,
        n_heads=4,
        position_encoding="sinusoidal",
    )
    assert "sinusoidal" in sinusoidal.to_json()
    assert sinusoidal.config_hash != cfg.config_hash
