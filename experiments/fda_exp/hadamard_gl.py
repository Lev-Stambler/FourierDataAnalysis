"""Categorical (Householder) Goldreich-Levin on a POWER-OF-2 token alphabet, realized as binary
Walsh GL over the packed token bits.

For V = 2^k the Walsh-Hadamard basis (`householder.hadamard_basis`) is a tensor product over each
token's k bits, so a V-ary character chi_alpha factors into a single binary Walsh parity over the
w*k bits.  Packing token position p's k bits into global bits k*p..k*p+k-1 -- exactly what
`_encode_qary(C, V)` already does (v_p * V^p = v_p << k*p) -- turns the branch-V, depth-w categorical
CSAMP tree into the branch-2, depth-(w*k) binary tree of `qary_gl_search` at V=2.  This is
~V/(2 log2 V) faster (28x at V=512, 51x at V=1024), unit-modulus (NO high-degree magnitude
inflation, unlike `householder_basis` at V>=8), and completeness-correct for free: V=2 characters are
+/-1, so the exact-W Parseval group-by inside `qary_gl_search` already holds.  CSAMP conditions on the
un-split bit suffix; at a token-block boundary that is exactly the token suffix (n-gram conditioning).

The packed integer IS the mixed-radix base-V LSD code, so a recovered binary mask read base-V gives
the per-token contrasts a_p = (mask >> k*p) & (V-1) directly.  Every V-ary utility
(`qary_coeffs_at`, `_char_columns`, `qary_recon`, `degree_of_codes`) then consumes the result
unchanged with `Psi = hadamard_basis(V)`; token-degree = #nonzero base-V digits.
"""

from __future__ import annotations

import numpy as np

from .householder import householder_basis
from .qary_gl import _digits, _encode_qary, degree_of_codes, qary_gl_search


def _k(V: int) -> int:
    k = int(V).bit_length() - 1
    if V != (1 << k):
        raise ValueError(f"hadamard_gl requires V a power of 2, got V={V}")
    return k


def pack_tokens(C: np.ndarray, V: int) -> np.ndarray:
    """(m,w) token-ids 0..V-1 -> (m,) int64 with token p's k bits at global bits k*p..k*p+k-1.
    Identical to `_encode_qary(C, V)`: `sum_p C[:,p] * V^p` places v_p's k bits in block p."""
    return _encode_qary(C, V)


def token_blocks(codes: np.ndarray, V: int, w: int) -> np.ndarray:
    """(K,) codes/masks -> (K,w) per-token contrasts a_p = (code >> k*p) & (V-1) = base-V digit p."""
    return _digits(codes, V, w)


def token_degree(codes: np.ndarray, V: int, w: int) -> np.ndarray:
    """Token-level degree = number of token positions with a nonzero contrast (== base-V degree)."""
    return degree_of_codes(codes, V, w)


def hadamard_gl_search(C, f, w, V, tau, n_exp=90000, device=None, mode="csamp", seed=0,
                       max_width=5000, norm_m=None):
    """Categorical Householder CSAMP over V=2^k tokens via binary Walsh GL on the packed token bits.

    C: (m,w) token-ids 0..V-1; f: (m,) target.  Runs `qary_gl_search` at V=2 over the w*k packed bits
    (branch-2, depth w*k, exact-W group-by).  Returns dict(status, codes, contrasts (K,w), degrees
    (K,), widths, experiments); `codes` are base-V LSD codes (== the recovered binary masks), directly
    consumable by `qary_coeffs_at` / `_char_columns` / `qary_recon` with `hadamard_basis(V)`.
    On blowup/failure returns the raw `qary_gl_search` dict (status != "ok").

    norm_m: pass the ORIGINAL row count when `C`/`f` are frequency-collapsed distinct contexts + their
    per-context weighted sums (see `hf_data.collapse_contexts`) -- exact, but O(#distinct) not O(m)."""
    k = _k(V)
    n = w * k
    if n > 62:
        raise ValueError(f"w*log2(V) = {n} exceeds the int64 packing limit (62); reduce window or vocab")
    idx = pack_tokens(C, V)                                       # bit-packed context == base-V code
    r = qary_gl_search(idx, np.asarray(f, dtype=np.float64), n, 2, householder_basis(2), tau,
                       n_exp=n_exp, device=device, mode=mode, seed=seed, max_width=max_width, norm_m=norm_m)
    if r["status"] != "ok":
        return r
    codes = np.array([c for c in (r["L"] or []) if c != 0], dtype=np.int64)   # binary masks = base-V codes
    return dict(status="ok", codes=codes, contrasts=token_blocks(codes, V, w),
                degrees=token_degree(codes, V, w), widths=r["widths"],
                experiments=r.get("experiments"))
