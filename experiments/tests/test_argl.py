"""Audited tests for tokenizer-native random-context vector Dataset GL."""

import itertools

import numpy as np

from fda_exp.argl import (
    child_scores_from_histogram,
    direct_child_scores,
    phase_from_difference,
    simplex_kernel,
    tokenizer_alphabet,
    weighted_histogram,
)


def _chars(q, n):
    grid = np.array(list(itertools.product(range(q), repeat=n)), dtype=np.int64)
    return grid, np.exp(2j * np.pi * (grid @ grid.T) / q)


def _problem(q=3, n=3, m=4, seed=0):
    rng = np.random.default_rng(seed)
    x = np.array(list(itertools.product(range(q), repeat=n)), dtype=np.int64)
    dz = rng.random((2, len(x))); dz /= dz.sum(1, keepdims=True)
    f = rng.random((2, len(x), m)); f /= f.sum(2, keepdims=True)
    mu = np.array([0.4, 0.6])
    return x, dz, f, mu


def _coefficient_energy(x, dz, f, mu, alpha, q):
    phase = np.exp(-2j * np.pi * (x @ alpha) / q)
    hz = (dz[:, :, None] * f * phase[None, :, None]).sum(1)
    return float((mu * (np.abs(hz) ** 2).sum(1)).sum())


def _bucket(x, dz, f, mu, a, k, q):
    n = x.shape[1]; r = n - k
    total = 0.0
    for z in range(len(mu)):
        prefixes = np.unique(x[:, :r], axis=0) if r else np.zeros((1, 0), dtype=np.int64)
        for pre in prefixes:
            mask = (x[:, :r] == pre).all(1) if r else np.ones(len(x), bool)
            mass = dz[z, mask].sum()
            if mass == 0:
                continue
            phase = np.exp(-2j * np.pi * (x[mask, r:] @ a) / q) if k else np.ones(mask.sum())
            v = (dz[z, mask, None] * f[z, mask] * phase[:, None]).sum(0) / mass
            total += mu[z] * mass * float((np.abs(v) ** 2).sum())
    return total


def test_random_context_vector_bucket_identities():
    q, n = 3, 3
    x, dz, f, mu = _problem(q=q, n=n)
    for k in range(n + 1):
        for a in itertools.product(range(q), repeat=k):
            a = np.asarray(a, dtype=np.int64)
            parent = _bucket(x, dz, f, mu, a, k, q)
            # Completeness for every full descendant.
            for b in itertools.product(range(q), repeat=n-k):
                energy = _coefficient_energy(x, dz, f, mu, np.r_[b, a], q)
                assert energy <= parent + 1e-12
            if k < n:
                for c in range(q):
                    child = _bucket(x, dz, f, mu, np.r_[c, a], k + 1, q)
                    assert child <= parent + 1e-12
            else:
                assert abs(parent - _coefficient_energy(x, dz, f, mu, a, q)) < 1e-12


def test_vector_level_mass():
    q, n = 3, 3
    x, dz, f, mu = _problem(q=q, n=n, seed=2)
    for k in range(n + 1):
        lhs = sum(_bucket(x, dz, f, mu, np.asarray(a), k, q)
                  for a in itertools.product(range(q), repeat=k))
        r = n - k; rhs = 0.0
        for z in range(len(mu)):
            prefixes = np.unique(x[:, :r], axis=0) if r else np.zeros((1, 0), dtype=np.int64)
            for pre in prefixes:
                mask = (x[:, :r] == pre).all(1) if r else np.ones(len(x), bool)
                mass = dz[z, mask].sum()
                if mass:
                    p = dz[z, mask] / mass
                    rhs += mu[z] * mass * q**k * np.sum(p**2 * np.sum(f[z, mask]**2, axis=1))
        assert abs(lhs - rhs) < 1e-11


def test_rms_context_energy_does_not_cancel():
    # F(z,x)=(-1)^(z+x), q=2.  Conditional frequency one has signs +/-1.
    hz = np.array([1.0, -1.0])
    assert np.mean(np.abs(hz) ** 2) == 1.0
    assert abs(np.mean(hz)) ** 2 == 0.0


def test_positive_exponent_ifft_and_histogram_weighting():
    q = 11
    d = np.array([1, 1, 2, 7, 7, 7])
    w = np.array([1+2j, 2-1j, -0.5j, 3, -2j, 0.2+0.7j])
    h = weighted_histogram(d, w, q, symmetrize=False)
    got = child_scores_from_histogram(h)
    want = direct_child_scores(d, w, q)
    assert np.max(np.abs(got - want)) < 1e-12


def test_hermitian_histogram_is_real_after_ifft():
    q = 13
    rng = np.random.default_rng(3)
    d = rng.integers(0, q, 100)
    w = rng.normal(size=100) + 1j * rng.normal(size=100)
    got = child_scores_from_histogram(weighted_histogram(d, w, q, symmetrize=True))
    assert np.max(np.abs(got.imag)) < 1e-12
    assert np.max(np.abs(got.real - direct_child_scores(d, w, q).real)) < 1e-12


def test_phase_uses_native_categorical_coordinates():
    q = 17
    alpha = np.array([1, 7, 16])
    y = np.array([[2, 4, 8], [1, 1, 1]])
    yp = np.array([[3, 9, 0], [1, 2, 3]])
    got = phase_from_difference(alpha, y, yp, q)
    want = np.exp(2j * np.pi * (((yp-y) * alpha).sum(1) % q) / q)
    assert np.allclose(got, want)


def test_simplex_kernel_is_permutation_invariant():
    q = 7
    x = np.array([[0, 1, 2], [4, 1, 6]])
    xp = np.array([[0, 3, 2], [5, 1, 6]])
    support = [0, 2]
    base = simplex_kernel(x, xp, support, q)
    perm = np.array([4, 6, 1, 0, 5, 2, 3])
    assert np.allclose(base, simplex_kernel(perm[x], perm[xp], support, q))


class _Tokenizer:
    def __init__(self, ids):
        self.ids = ids
    def __len__(self):
        return len(self.ids)
    def get_vocab(self):
        return {str(i): v for i, v in enumerate(self.ids)}


def test_tokenizer_alphabet_excludes_padded_rows():
    assert tokenizer_alphabet(_Tokenizer([0, 1, 2, 3]), raw_logit_width=8) == 4
    try:
        tokenizer_alphabet(_Tokenizer([0, 1, 3]), raw_logit_width=8)
    except ValueError:
        pass
    else:
        raise AssertionError("non-contiguous tokenizer must be rejected")


def test_small_student_is_full_q_and_under_cap():
    import torch
    from fda_exp.qwen_argl import build_student
    freq = np.zeros((2, 128), dtype=np.int64); freq[0, -1] = 1; freq[1, -2:] = [2, 3]
    model = build_student(17, freq, layers=1, d_model=24, rank=8, heads=4, ff=48)
    logits = model(torch.randint(0, 17, (2, 256)))
    assert logits.shape == (2, 17)


def test_frequency_loader_drops_constant_and_conjugate_duplicates(tmp_path):
    import json
    from fda_exp.qwen_argl import load_frequency_file
    rows = np.zeros((4, 128), dtype=int)
    rows[1, -1] = 2
    rows[2, -1] = 15  # -2 mod 17
    rows[3, -2:] = [3, 4]
    p = tmp_path / "spectral.json"
    p.write_text(json.dumps({"q": 17, "frequencies": rows.tolist()}))
    got = load_frequency_file(p)
    assert got.shape == (2, 128)


def test_static_compile_batch_padding_only_duplicates_inputs():
    import torch
    from fda_exp.qwen_argl import _pad_context_batch

    x = torch.arange(12).reshape(3, 4)
    got = _pad_context_batch(x, 5)
    assert got.shape == (5, 4)
    assert torch.equal(got[:3], x)
    assert torch.equal(got[3:], x[:1].expand(2, -1))


def test_simplex_features_reproduce_exact_one_position_kernel():
    import torch
    from fda_exp.qwen_argl import _simplex_features

    q = 7
    landmarks = torch.tensor([[0, a] for a in range(q)])
    tokens = torch.tensor([[2], [5]])
    phi = _simplex_features(tokens, landmarks, q)
    got = phi @ phi.T
    want = torch.tensor([[1.0, -1.0 / (q - 1)], [-1.0 / (q - 1), 1.0]])
    assert torch.allclose(got, want, atol=1e-6)


def test_fourier_simplex_and_no_feature_controls_are_parameter_matched():
    from fda_exp.qwen_argl import build_student

    freq = np.zeros((2, 128), dtype=np.int64); freq[:, -1] = [1, 2]
    marks = np.asarray([[127, 0], [127, 1], [127, 2], [127, 3]])
    kw = dict(q=17, layers=1, d_model=24, rank=8, heads=4, ff=48)
    fourier = build_student(frequencies=freq, **kw)
    simplex = build_student(frequencies=np.zeros((0, 128), int), feature_kind="simplex",
                            landmarks=marks, **kw)
    baseline = build_student(frequencies=np.zeros((0, 128), int), feature_kind="none",
                             matched_feature_width=4, **kw)
    counts = [sum(p.numel() for p in m.parameters()) for m in (fourier, simplex, baseline)]
    assert counts[0] == counts[1] == counts[2]


def test_label_shard_integrity_check_rejects_truncation(tmp_path):
    import torch
    from fda_exp.qwen_argl import labeled_split_is_valid

    path = tmp_path / "labels.pt"
    torch.save({"q": 7, "contexts": torch.zeros((3, 256), dtype=torch.int32),
                "teacher_logits": torch.zeros((3, 7), dtype=torch.bfloat16)}, path)
    assert labeled_split_is_valid(path, 3, 7)
    path.write_bytes(path.read_bytes()[:32])
    assert not labeled_split_is_valid(path, 3, 7)
