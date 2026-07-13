"""Audited tests for Walsh-Hadamard Dataset GL over LSH token codes."""

import json

import numpy as np

from fda_exp.argl import classify_scores, phase_from_difference
from fda_exp.householder import hadamard_basis
from fda_exp.lsh import bit_expand, build_control_codes, build_lsh_codes
from fda_exp.whlsh_argl import (
    bit_gate_level,
    load_bit_frequency_file,
    score_bit_children,
)


def _bits(x, width):
    x = np.asarray(x, dtype=np.int64)
    return ((x[..., None] >> np.arange(width)) & 1).astype(np.uint8)


def test_lsh_codes_unique_with_duplicate_rows():
    rng = np.random.default_rng(0)
    e = rng.standard_normal((64, 8))
    e[10] = e[3]                      # exact duplicate pair
    e[11] = e[3]                      # ...grown to an exact triple
    e[20] = e[21]                     # second exact group
    codes, report = build_lsh_codes(e, B0=4, cap=64, seed=0)
    assert codes.dtype == np.uint8 and set(np.unique(codes)) <= {0, 1}
    assert len(np.unique(codes, axis=0)) == len(e)
    # 4 projection bits cannot separate 64 rows, so B must have doubled -- but
    # the inseparable exact-duplicate rows must NOT force B to the cap.
    assert 4 < report["B_proj"] < 64
    assert report["collisions_vs_B"][str(report["B_proj"])] == 0
    # Identical rows are only separable by tie-break bits.
    assert report["tiebreak_bits"] >= 2  # a group of 3 needs two bits
    assert sorted(report["duplicate_groups"]) == [2, 3]
    codes2, _ = build_lsh_codes(e, B0=4, cap=64, seed=0)
    assert np.array_equal(codes, codes2)
    # Projection bits of duplicates agree; only appended bits differ.
    b = report["B_proj"]
    assert np.array_equal(codes[3, :b], codes[10, :b])
    assert not np.array_equal(codes[3, b:], codes[10, b:])


def test_pca_codes_leading_bit_is_dominant_axis():
    from fda_exp.lsh import build_pca_codes
    rng = np.random.default_rng(9)
    e = rng.standard_normal((128, 8)) * 0.1
    e[:, 3] += np.where(np.arange(128) < 64, 5.0, -5.0)  # dominant axis
    e[10] = e[3]  # exact duplicate still handled
    codes, report = build_pca_codes(e, B=8)
    assert len(np.unique(codes, axis=0)) == len(e)
    lead = codes[:, 0].astype(int)
    split = np.where(np.arange(128) < 64, 1, 0)
    assert max((lead == split).mean(), (lead == 1 - split).mean()) > 0.95


def test_control_codes_unique_and_deterministic():
    codes = build_control_codes(500, 64, seed=1)
    assert codes.shape == (500, 64) and codes.dtype == np.uint8
    assert len(np.unique(codes, axis=0)) == 500
    assert np.array_equal(codes, build_control_codes(500, 64, seed=1))


def test_bit_expand_indexes_code_table():
    codes = _bits(np.arange(16), 4)
    tokens = np.array([[3, 5], [15, 0]])
    got = bit_expand(tokens, codes)
    assert got.shape == (2, 2, 4)
    assert np.array_equal(got[0, 1], _bits(5, 4))
    assert np.array_equal(got[1, 0], np.ones(4, np.uint8))


def test_q2_phase_is_xor_parity():
    rng = np.random.default_rng(2)
    y = rng.integers(0, 2, (32, 12))
    yp = rng.integers(0, 2, (32, 12))
    a = rng.integers(0, 2, 12)
    got = phase_from_difference(a, y, yp, 2)
    want = 1.0 - 2.0 * ((a & (y ^ yp)).sum(1) % 2)
    assert np.max(np.abs(got - want)) < 1e-12
    # Cross-check against the explicit Walsh-Hadamard matrix on 4-bit codes.
    h = hadamard_basis(16)
    for alpha in range(16):
        for x in range(16):
            phase = phase_from_difference(_bits(alpha, 4), np.zeros(4, np.int64),
                                          _bits(x, 4).astype(np.int64), 2)
            assert h[alpha, x] == phase.real


def test_lsh_locality():
    rng = np.random.default_rng(3)
    centers = rng.standard_normal((32, 16))
    near = centers + 0.05 * rng.standard_normal(centers.shape)
    codes, report = build_lsh_codes(np.vstack([centers, near]), B0=64, cap=64, seed=0)
    b = report["B_proj"]
    near_agree = (codes[:32, :b] == codes[32:, :b]).mean()
    far_agree = (codes[:32, :b] == codes[np.roll(np.arange(32), 1) + 32, :b]).mean()
    assert near_agree > far_agree + 0.2


def test_score_bit_children_matches_definition():
    rng = np.random.default_rng(4)
    d0 = rng.integers(0, 2, (100, 6)).astype(np.uint8)
    w = rng.normal(size=100)
    got = score_bit_children(d0, w)
    want = np.array([np.mean(w * (1.0 - 2.0 * d0[:, j])) for j in range(6)])
    assert np.allclose(got, want)
    batched = score_bit_children(d0, np.stack([w, 2 * w]))
    assert np.allclose(batched, np.stack([want, 2 * want]))


def test_bit_gate_recovers_planted_characters_with_growth():
    # f(l) = 0.6*chi_{bit1} + 0.8*chi_{bit1,bit3}: the 2-bit character is only
    # reachable through the bit-1 stepping stone, exercising greedy growth.
    rng = np.random.default_rng(5)
    m, width = 4096, 4
    l = rng.integers(0, 16, m)
    lp = rng.integers(0, 16, m)

    def f(t):
        b = _bits(t, width).astype(np.float64)
        return 0.6 * (1 - 2 * b[:, 1]) + 0.8 * (1 - 2 * ((b[:, 1] + b[:, 3]) % 2))

    w = f(l) * f(lp)
    d0 = _bits(l, width) ^ _bits(lp, width)
    out = bit_gate_level(d0, np.zeros((m, 0), np.uint8), w,
                         np.zeros((1, 0), np.uint8), beam=8, growth=2)
    masks = {tuple(mask): score for score, mask in out["candidates"]}
    two_bit = (0, 1, 0, 1)
    one_bit = (0, 1, 0, 0)
    assert abs(masks[two_bit] - 0.64) < 0.08
    assert abs(masks[one_bit] - 0.36) < 0.08
    assert tuple(out["candidates"][0][1]) == two_bit
    decision = classify_scores([masks[two_bit], masks[one_bit]], 0.10, m)
    assert len(decision.heavy) == 2


def test_bit_gate_excludes_near_constant_bits():
    # A bit that never flips between paired samples has a parity constant on
    # the data: its score equals the zero-extension score for ANY weights, so
    # it must not enter the candidate pool (this is the tie-break-bit trap).
    rng = np.random.default_rng(7)
    m, width = 1024, 4
    l = rng.integers(0, 8, m)   # values 0..7: bit 3 is constant zero
    lp = rng.integers(0, 8, m)

    def f(t):
        b = _bits(t, width).astype(np.float64)
        return 1 - 2 * b[:, 1]

    w = f(l) * f(lp)
    d0 = _bits(l, width) ^ _bits(lp, width)
    out = bit_gate_level(d0, np.zeros((m, 0), np.uint8), w,
                         np.zeros((1, 0), np.uint8), beam=8, growth=2)
    masks = [tuple(mask) for _, mask in out["candidates"]]
    assert all(mask[3] == 0 for mask in masks)
    assert masks[0] == (0, 1, 0, 0)


def test_bit_gate_level_two_uses_parent_phase():
    # f(p, l) = chi_{bit2}(p) * chi_{bit1}(l); resample both tokens.  Under the
    # correct parent mask on l the product telescopes to exactly +1 per pair.
    rng = np.random.default_rng(6)
    m, width = 512, 4
    p, pp = rng.integers(0, 16, (2, m))
    l, lp = rng.integers(0, 16, (2, m))
    bp, bpp, bl, blp = (_bits(t, width).astype(np.int64) for t in (p, pp, l, lp))
    w = ((1 - 2 * bp[:, 2]) * (1 - 2 * bpp[:, 2]) *
         (1 - 2 * bl[:, 1]) * (1 - 2 * blp[:, 1])).astype(np.float64)
    parent = np.zeros((1, width), np.uint8); parent[0, 1] = 1
    out = bit_gate_level(bp ^ bpp, bl ^ blp, w, parent, beam=4, growth=1)
    score, mask = out["candidates"][0]
    want = np.zeros(2 * width, np.uint8); want[2] = 1; want[width + 1] = 1
    assert np.array_equal(mask, want)
    assert abs(score - 1.0) < 1e-12
    wrong = np.zeros((1, width), np.uint8); wrong[0, 0] = 1
    bad = bit_gate_level(bp ^ bpp, bl ^ blp, w, wrong, beam=4, growth=1)
    assert bad["candidates"][0][0] < 0.2


def test_bit_frequency_loader_dedups_and_drops_zero(tmp_path):
    total = 128 * 4  # ROLLOUT_LEN * B
    payload = {"B": 4, "frequencies_bits": [[], [510, 3], [3, 510], [511]]}
    path = tmp_path / "spectral_bits.json"
    path.write_text(json.dumps(payload))
    got = load_bit_frequency_file(path)
    assert got.shape == (2, total) and got.dtype == np.uint8
    assert got[:, 3].sum() == 1 and got[:, 510].sum() == 1 and got[:, 511].sum() == 1


def test_bit_linear_fits_planted_linear_teacher(tmp_path):
    # A teacher whose logits ARE linear in the code bits must be recoverable
    # by the pure parity model, driving val KL far below the unigram floor.
    import torch
    from fda_exp.whlsh_argl import fit_bits_linear
    rng = np.random.default_rng(8)
    q, width, n_ctx, ctx_len = 16, 4, 512, 8
    codes = _bits(np.arange(q), width)
    w_true = rng.normal(size=(2 * width, q)) * 2.0
    def make(n, seed):
        r = np.random.default_rng(seed)
        ctx = r.integers(0, q, (n, ctx_len))
        phi = 1.0 - 2.0 * codes[ctx[:, -2:]].reshape(n, -1)
        return {"contexts": torch.as_tensor(ctx, dtype=torch.int32),
                "teacher_logits": torch.as_tensor(phi @ w_true, dtype=torch.bfloat16),
                "q": q}
    paths = {}
    for split, seed in (("train", 0), ("val", 1), ("test", 2)):
        paths[split] = tmp_path / f"{split}.pt"
        torch.save(make(n_ctx, seed), paths[split])
    factor = np.eye(q, dtype=np.float32)  # rank q: exact head
    result = fit_bits_linear(paths["train"], paths["val"], paths["test"], codes,
                             tmp_path / "lin.pt", window=2, epochs=200, lr=0.05,
                             device="cpu", initial_vocab=factor)
    assert result["test"]["kl_mean"] < 0.2
    assert result["test"]["top1"] > 0.8


def test_bit_student_shapes_and_cos_only():
    import torch
    from fda_exp.qwen_argl import _bit_features, build_student
    codes = _bits(np.arange(16), 4)
    masks = np.zeros((3, 128 * 4), dtype=np.uint8)
    masks[0, -1] = 1
    masks[1, -4:] = [1, 0, 1, 0]
    masks[2, -9] = 1
    model = build_student(16, masks, layers=1, d_model=24, rank=8, heads=4, ff=48,
                          codes=codes)
    assert model.fourier.in_features == 3
    tokens = torch.randint(0, 16, (2, 256))
    logits = model(tokens)
    assert logits.shape == (2, 16)
    phi = _bit_features(tokens[:, -128:], torch.as_tensor(masks),
                        torch.as_tensor(codes))
    bits = codes[tokens[:, -128:].numpy()].reshape(2, -1)
    want = 1.0 - 2.0 * ((bits @ masks.T) % 2)
    assert np.allclose(phi.numpy(), want)
