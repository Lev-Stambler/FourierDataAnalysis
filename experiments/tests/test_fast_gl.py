"""Tests for the GPU one-shot multiclass top-K Walsh CSAMP search."""
import numpy as np
import pytest

from fda_exp.fast_gl import char_matrix, coeffs_all, multiclass_search
from fda_exp.gl_torch import get_device
from fda_exp.hf_data import collapse_contexts
from fda_exp.householder import hadamard_basis
from fda_exp.qary_gl import _char_columns, _encode_qary, degree_of_codes


def _planted(V=8, w=3, m=20000, seed=0):
    """Next-token determined by a planted degree-2 Walsh character of the context."""
    rng = np.random.default_rng(seed)
    C = rng.integers(0, V, (m, w)).astype(np.int64)
    H = hadamard_basis(V)
    chi = H[5, C[:, 0]] * H[3, C[:, 2]]                                # degree-2 char on positions {0,2}
    y = np.where(chi > 0, 1, 2).astype(np.int64)                      # token 1 or 2 by the character
    code = 5 + 3 * V ** 2                                              # base-V LSD code of the planted char
    return C, y, V, w, H, code


def test_multiclass_recovers_planted_and_topk_bounds_width():
    C, y, V, w, H, code = _planted()
    Cd, counts, n_ctx, m = collapse_contexts(C, y, V)
    A = (2.0 * counts - n_ctx[:, None]).astype(np.float32)
    idx = _encode_qary(Cd, V)
    codes, widths = multiclass_search(idx, A, w * (V.bit_length() - 1), max_width=1500, device="cpu")
    assert code in set(int(c) for c in codes), (code, sorted(int(c) for c in codes)[:10])
    assert all(wd <= 1500 for wd in widths), widths                   # top-K never blows up
    assert degree_of_codes(np.array([code]), V, w)[0] == 2


def test_char_matrix_matches_char_columns():
    C, y, V, w, H, code = _planted(m=2000)
    Cd, counts, n_ctx, m = collapse_contexts(C, y, V)
    idx = _encode_qary(Cd, V)
    codes = np.array([code, 5, 3 * V ** 2, 0], dtype=np.int64)
    ref = _char_columns(Cd, codes, V, H, w)                           # (D, K) numpy Householder=Walsh
    got = char_matrix(idx, codes, w * (V.bit_length() - 1), device="cpu").cpu().numpy()
    assert np.allclose(got, ref, atol=1e-5), np.abs(got - ref).max()


def test_coeffs_all_matches_per_token_reference():
    C, y, V, w, H, code = _planted(m=8000)
    Cd, counts, n_ctx, m = collapse_contexts(C, y, V)
    codes = np.array([code, 5, 3 * V ** 2], dtype=np.int64)
    coeffs, const = coeffs_all(counts, n_ctx, Cd, codes, V, w, m)     # (V,K), (V,)
    for t in (1, 2):                                                   # the two active tokens
        fsum = 2.0 * counts[:, t] - n_ctx
        ref = (_char_columns(Cd, codes, V, H, w).T @ fsum) / m
        assert np.allclose(coeffs[t], ref, atol=1e-8), (t, coeffs[t], ref)
    # the planted char is the heaviest for tokens 1/2 -> |coeff| ~ 1
    assert abs(coeffs[1, 0]) > 0.5 and abs(coeffs[2, 0]) > 0.5


def test_runs_on_default_device():
    """Smoke: the search runs on the auto GPU device (MPS locally / CUDA on Modal), float32 only."""
    C, y, V, w, H, code = _planted(m=6000)
    Cd, counts, n_ctx, m = collapse_contexts(C, y, V)
    A = (2.0 * counts - n_ctx[:, None]).astype(np.float32)
    idx = _encode_qary(Cd, V)
    dev = get_device()
    try:
        codes, _ = multiclass_search(idx, A, w * (V.bit_length() - 1), max_width=1500, device=dev)
    except (RuntimeError, TypeError) as e:
        pytest.skip(f"device {dev} unsupported op: {e}")
    assert code in set(int(c) for c in codes), dev
