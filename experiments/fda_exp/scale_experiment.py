"""Scaling: how does dataset-GL behave on REAL sequences as n grows?

For each context length w (n = 4w one-hot), on real human-promoter DNA:
- run the GPU GL (CSAMP, the real oracle) on a real next-nucleotide indicator,
  recording search width, runtime, recovered-set size (or where it blows up);
- run samples-only (SAMP) to show blindness;
- (largest n) a random-sequence control, where contexts don't repeat.

No planting: the target is the real next nucleotide. The honest question is
whether real data keeps the search width bounded (context repetition) as n grows,
or whether it hits the blindness blow-up.

    uv run python -m fda_exp.scale_experiment
"""

from __future__ import annotations

import time

import numpy as np

from .dna_data import BLOCK, _ID, load_sequences
from .gl_torch import gl_search_torch, get_device
from .predict import majority_by_context
from .spectrum import coeffs_at, points_to_index


def _encode_window(ids, encoding):
    blocks = []
    for t in ids:
        if encoding == "onehot":                 # 4 redundant bits, one +1 (aliasing-prone)
            b = -np.ones(BLOCK, dtype=np.int64)
            b[t] = 1
        else:                                     # "twobit": 2 bijective bits, no redundancy
            b = np.array([1 - 2 * (t & 1), 1 - 2 * ((t >> 1) & 1)], dtype=np.int64)
        blocks.append(b)
    return np.concatenate(blocks)


def build(window, seqs, max_pairs, encoding="onehot"):
    rows, nxt = [], []
    for s in seqs:
        ids = [_ID.get(c, -1) for c in s]
        for i in range(len(ids) - window):
            ctx = ids[i:i + window]
            tgt = ids[i + window]
            if tgt < 0 or min(ctx) < 0:
                continue
            rows.append(_encode_window(ctx, encoding))
            nxt.append(tgt)
        if len(rows) >= max_pairs:
            break
    X = np.array(rows[:max_pairs], dtype=np.int64)
    y = np.array(nxt[:max_pairs], dtype=np.int64)
    return X, y


def _random_seqs(n_seqs, length, seed=0):
    rng = np.random.default_rng(seed)
    letters = np.array(list("ACGT"))
    return ["".join(letters[rng.integers(0, 4, size=length)]) for _ in range(n_seqs)]


def sweep_on_seqs(seqs, name, windows=(6, 8, 10, 12, 14), max_pairs=250_000,
                  tau=0.3, n_exp=15000, target_nt=0, max_width=80_000, device=None,
                  encoding="onehot"):
    """Width-vs-context-length sweep on a given list of DNA sequences (any source).
    Returns rows; prints a table. The honest test: does the CSAMP search width
    stay bounded as n grows (contexts repeat) or blow up (blindness/density)?"""
    dev = device or get_device()
    print(f"\n===== {name}  ({encoding}, device={dev}, tau={tau}, n_exp={n_exp}) =====")
    print(f"{'n':>4} {'w':>3} {'m_ctx':>8} {'rep':>5} | {'CSAMP width':>11} {'recov':>9} {'sec':>6} | {'SAMP w':>7} blind?")
    rows = []
    for w in windows:
        X, y = build(w, seqs, max_pairs, encoding=encoding)
        if len(X) == 0:
            continue
        D, maj = majority_by_context(X, y)
        n = D.shape[1]
        if n > 62:
            print(f"  (skip w={w}: n={n} > 62, needs multi-word idx)")
            continue
        idx = points_to_index(D)
        f = (2 * (maj == target_nt) - 1).astype(np.float64)
        rep = len(D) / len(X)
        t = time.time()
        rc = gl_search_torch(idx, f, n, tau, n_exp=n_exp, device=dev, mode="csamp", seed=1, max_width=max_width)
        sec = time.time() - t
        rs = gl_search_torch(idx, f, n, tau, n_exp=n_exp, device=dev, mode="samp", seed=1, max_width=max_width)
        cw, sw = max(rc["widths"]), max(rs["widths"])
        recov = str(len(rc["L"])) if rc["status"] == "ok" else f"blow@{rc['level']}"
        blind = "yes" if sw < max(cw, 1) / 5 else "no"
        print(f"{n:>4} {w:>3} {len(D):>8} {rep:>5.2f} | {cw:>11} {recov:>9} {sec:>6.1f} | {sw:>7} {blind}")
        rows.append(dict(name=name, n=n, w=w, m=int(len(D)), rep=float(rep),
                         csamp_width=int(cw), recov=recov, samp_width=int(sw),
                         status=rc["status"], sec=round(sec, 1)))
    return rows


def run(windows=(6, 8, 10, 12), n_seqs=12000, max_pairs=250_000,
        tau=0.3, n_exp=15000, target_nt=0, max_width=80_000):
    dev = get_device()
    seqs = load_sequences(n_seqs)
    print(f"device={dev}  tau={tau}  n_exp={n_exp}  (target: next nt == {'ACGT'[target_nt]})\n")
    print(f"{'n':>4} {'w':>3} {'m_ctx':>7} {'rep':>5} | "
          f"{'CSAMP width':>11} {'recov':>8} {'sound':>6} {'sec':>6} | {'SAMP width':>10} {'blind?':>6}")
    print("-" * 82)

    rows_for_plot = []
    for w in windows:
        X, y = build(w, seqs, max_pairs)
        D, maj = majority_by_context(X, y)
        n = D.shape[1]
        idx = points_to_index(D)
        f = (2 * (maj == target_nt) - 1).astype(np.float64)
        rep = len(D) / len(X)                                  # distinct-context fraction (low = repetitive)

        t = time.time()
        rc = gl_search_torch(idx, f, n, tau, n_exp=n_exp, device=dev, mode="csamp", seed=1, max_width=max_width)
        sec = time.time() - t
        rs = gl_search_torch(idx, f, n, tau, n_exp=n_exp, device=dev, mode="samp", seed=1, max_width=max_width)

        cwidth = max(rc["widths"])
        swidth = max(rs["widths"])
        if rc["status"] == "ok":
            recov = str(len(rc["L"]))
            # soundness: are recovered coeffs actually heavy?  spot-check up to 200
            L = rc["L"]
            samp = list(np.random.default_rng(0).choice(len(L), size=min(200, len(L)), replace=False)) if L else []
            vals = coeffs_at(D, f, [L[i] for i in samp]) if samp else np.array([1.0])
            sound = f"{np.mean(np.abs(vals) >= tau / 2):.2f}"
        else:
            recov = f"blow@{rc['level']}"
            sound = "-"
        blind = "yes" if swidth < max(cwidth, 1) / 5 else "no"
        print(f"{n:>4} {w:>3} {len(D):>7} {rep:>5.2f} | {cwidth:>11} {recov:>8} {sound:>6} {sec:>6.1f} | {swidth:>10} {blind:>6}")
        rows_for_plot.append((n, cwidth, sec, rc["status"]))

    # random-sequence control at the largest window
    w = windows[-1]
    Xr, yr = build(w, _random_seqs(n_seqs, length=251), max_pairs)
    Dr, mr = majority_by_context(Xr, yr)
    nr = Dr.shape[1]
    fr = (2 * (mr == target_nt) - 1).astype(np.float64)
    rr = gl_search_torch(points_to_index(Dr), fr, nr, tau, n_exp=n_exp, device=dev, mode="csamp", seed=1, max_width=max_width)
    cw = max(rr["widths"])
    print("-" * 82)
    print(f"{nr:>4} {w:>3} {len(Dr):>7} {len(Dr)/len(Xr):>5.2f} | "
          f"{cw:>11} {('blow@'+str(rr['level'])) if rr['status']!='ok' else len(rr['L']):>8}  "
          f"(RANDOM-DNA control: contexts do not repeat)")
    return rows_for_plot


if __name__ == "__main__":
    run()
