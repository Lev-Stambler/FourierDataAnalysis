"""Real protein fitness landscapes (deep mutational scanning) as categorical
product spaces, for the sample-efficiency test of sparse-Fourier / dataset-GL.

GB1 (Olson/Wu 2014): the four sites V39,D40,G41,V54 of protein G domain B1,
all 20^4 variants assayed for IgG-Fc binding enrichment.  An exact V=20, w=4
product space with documented order-1/2/3 epistasis -- the canonical sparse
higher-order fitness landscape.  Fitness = `I20fit` (enrichment-based), WT=VDGV.

    uv run python -m fda_exp.fitness_data
"""

from __future__ import annotations

import os
import urllib.request

import numpy as np

AA20 = "ACDEFGHIKLMNPQRSTVWY"
_AID = {c: i for i, c in enumerate(AA20)}
# cache dir is env-overridable so Modal can point it at a writable Volume (/cache)
_DATA = os.environ.get("FDA_DATA_DIR") or os.path.join(os.path.dirname(__file__), "..", "data")
_GB1_URL = "https://raw.githubusercontent.com/wchnicholas/ProteinGFourMutants/master/result/Mutfit"
_GB1_CACHE = os.path.join(_DATA, "gb1_fitness.parquet")


def load_gb1(cache=_GB1_CACHE, url=_GB1_URL, log=True):
    """Return (variants: list[str of len4], fitness: np.ndarray).  Downloads once."""
    cache = os.path.abspath(cache)
    if os.path.exists(cache):
        import pyarrow.parquet as pq
        t = pq.read_table(cache)
        return t.column("variant").to_pylist(), t.column("fitness").to_numpy()
    os.makedirs(os.path.dirname(cache), exist_ok=True)
    print(f"downloading GB1 landscape -> {cache} ...")
    req = urllib.request.Request(url, headers={"User-Agent": "fda-exp/0.1"})
    body = urllib.request.urlopen(req, timeout=90).read().decode("utf-8", "replace")
    variants, fit = [], []
    for line in body.splitlines()[1:]:                     # skip header
        f = line.split("\t")
        mut, val = f[0], f[10]                              # I20fit is the canonical fitness column
        if len(mut) != 4 or any(c not in _AID for c in mut) or val == "NA":
            continue
        variants.append(mut)
        fit.append(float(val))
    y = np.asarray(fit, dtype=np.float64)
    if log:                                                # enrichment -> log fitness (more Gaussian)
        y = np.log(np.clip(y, 1e-4, None))
    import pyarrow as pa
    import pyarrow.parquet as pq
    pq.write_table(pa.table({"variant": variants, "fitness": y}), cache)
    print(f"  cached {len(variants)} variants of 20^4=160000 ({len(variants)/160000:.1%} complete)")
    return variants, y


def gb1_windows(log=True):
    """(m,4) amino-acid id windows + fitness for GB1.  V=20, w=4."""
    variants, y = load_gb1(log=log)
    C = np.array([[_AID[c] for c in v] for v in variants], dtype=np.int64)
    return C, y, 20, 4


# --- Poelwijk 2019: mTagBFP2<->mKate2, all 2^13 binary genotypes, brightness in two
#     channels.  Documented to have MANY high-order epistatic terms yet be SPARSE in
#     the Walsh basis -- the regime where sparse-Fourier should beat additive/black-box.
_POELWIJK_URL = ("https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-019-12130-8"
                 "/MediaObjects/41467_2019_12130_MOESM7_ESM.xlsx")
_POELWIJK_CACHE = os.path.join(_DATA, "poelwijk_gfp.parquet")


def load_poelwijk(target="red", cache=_POELWIJK_CACHE, url=_POELWIJK_URL):
    """Return (genotypes: list[str of 13 '0'/'1'], y: brightness for `target`).
    target in {'red','blue','combined'}.  Downloads/parses the xlsx once."""
    cache = os.path.abspath(cache)
    cols = {"red": "b_red", "blue": "b_blue", "combined": "b_comb"}
    if not os.path.exists(cache):
        os.makedirs(os.path.dirname(cache), exist_ok=True)
        print(f"downloading Poelwijk GFP landscape -> {cache} ...")
        import io

        import openpyxl
        data = urllib.request.urlopen(urllib.request.Request(url, headers={"User-Agent": "fda-exp/0.1"}),
                                      timeout=120).read()
        ws = openpyxl.load_workbook(io.BytesIO(data), read_only=True)["genodata"]
        geno, b_red, b_blue, b_comb = [], [], [], []
        for i, row in enumerate(ws.iter_rows(values_only=True)):
            if i < 2:                                      # two header rows
                continue
            g = str(row[0]).strip().strip("'")             # "'0000...'" -> "0000..."
            if len(g) != 13 or any(c not in "01" for c in g):
                continue
            geno.append(g)
            b_red.append(float(row[6])); b_blue.append(float(row[7])); b_comb.append(float(row[9]))
        import pyarrow as pa
        import pyarrow.parquet as pq
        pq.write_table(pa.table({"geno": geno, "b_red": b_red, "b_blue": b_blue, "b_comb": b_comb}), cache)
        print(f"  cached {len(geno)} genotypes")
    import pyarrow.parquet as pq
    t = pq.read_table(cache)
    return t.column("geno").to_pylist(), t.column(cols[target]).to_numpy()


def poelwijk_windows(target="red"):
    """(m,13) binary genotypes + brightness.  V=2, w=13 (full 2^13 measured)."""
    geno, y = load_poelwijk(target)
    C = np.array([[int(b) for b in g] for g in geno], dtype=np.int64)
    return C, y, 2, 13


if __name__ == "__main__":
    C, y, V, w = gb1_windows()
    print(f"GB1: {C.shape} windows, V={V}, w={w}")
    print(f"fitness: mean {y.mean():.3f} std {y.std():.3f} min {y.min():.3f} max {y.max():.3f}")
    wt = np.array([[_AID[c] for c in "VDGV"]])
    print(f"WT VDGV present: {(C == wt).all(1).any()}")
