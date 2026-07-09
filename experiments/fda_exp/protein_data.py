"""Real protein sequences (UniProt human reviewed) + the N-glycosylation sequon.

The sequon N-X-[S/T] (X != P) is the *real* enzyme-recognition motif for N-linked
glycosylation -- a genuine degree-3 conjunction over three adjacent residues.  We
compute it as a local window label on REAL protein sequences (real amino-acid
distribution and local correlations); the label itself is not random/planted, it
is the actual biological motif.  This is the V=20 decisive test: Householder is
the only feasible basis (one-hot-binary would be 2^(20*w)), and a degree-3 AND
carries a degree-3 Fourier coefficient that logistic-<=2 structurally cannot fit.

    uv run python -c "from fda_exp.protein_data import load_uniprot; load_uniprot()"
"""

from __future__ import annotations

import os
import re
import urllib.request

import numpy as np

AA = "ACDEFGHIKLMNPQRSTVWY"                       # 20 standard amino acids -> ids 0..19
_AID = {c: i for i, c in enumerate(AA)}
N, P, S, T = _AID["N"], _AID["P"], _AID["S"], _AID["T"]
_CACHE = os.path.join(os.path.dirname(__file__), "..", "data", "uniprot_human_reviewed.parquet")
# paginated search endpoint (the `stream` endpoint is flaky and can return a body of
# "ERROR ENCOUNTERED WHEN STREAMING DATA"); 500/page + cursor is reliable.
_BASE = ("https://rest.uniprot.org/uniprotkb/search?"
         "query=reviewed:true+AND+organism_id:9606&format=tsv&fields=sequence&size=500")


def load_uniprot(cache=_CACHE, n_seqs=15000, base=_BASE):
    """Return a list of amino-acid sequences (cached parquet; paginates on first call)."""
    cache = os.path.abspath(cache)
    if os.path.exists(cache):
        import pyarrow.parquet as pq
        return pq.read_table(cache, columns=["seq"]).column("seq").to_pylist()
    os.makedirs(os.path.dirname(cache), exist_ok=True)
    print(f"downloading UniProt human reviewed (paginated) -> {cache} ...")
    seqs, url, pages = [], base, 0
    while url and len(seqs) < n_seqs:
        req = urllib.request.Request(url, headers={"User-Agent": "fda-exp/0.1"})
        with urllib.request.urlopen(req, timeout=60) as r:
            body = r.read().decode("utf-8", "replace")
            link = r.headers.get("Link", "")
        for ln in body.splitlines():
            ln = ln.strip()
            if ln and ln != "Sequence":                        # drop the per-page header
                seqs.append(ln.upper())
        pages += 1
        m = re.search(r'<([^>]+)>;\s*rel="next"', link)
        url = m.group(1) if m else None
        if pages % 5 == 0:
            print(f"  page {pages}: {len(seqs)} sequences")
    seqs = seqs[:n_seqs]
    import pyarrow as pa
    import pyarrow.parquet as pq
    pq.write_table(pa.table({"seq": seqs}), cache)
    print(f"  cached {len(seqs)} sequences over {pages} pages")
    return seqs


def build_windows(w=5, n_seqs=8000, max_windows=250_000, seed=0):
    """(m,w) amino-acid id windows over real sequences + the center-anchored sequon label.

    Sequon occupies positions [c, c+1, c+2] with c=(w-3)//2 (centered); the other
    positions are real-sequence context distractors.  label +1 iff
    aa[c]==N and aa[c+1]!=P and aa[c+2] in {S,T}.
    """
    seqs = load_uniprot()
    idx = np.random.default_rng(seed).permutation(len(seqs))[:n_seqs]
    c = (w - 3) // 2
    ctx, lab = [], []
    for si in idx:
        ids = [_AID.get(ch, -1) for ch in seqs[si]]
        for i in range(len(ids) - w + 1):
            win = ids[i:i + w]
            if min(win) < 0:                     # skip non-standard residues (X/U/B/Z/*)
                continue
            sequon = (win[c] == N) and (win[c + 1] != P) and (win[c + 2] in (S, T))
            ctx.append(win)
            lab.append(1 if sequon else -1)
        if len(ctx) >= max_windows:
            break
    C = np.array(ctx[:max_windows], dtype=np.int64)
    y = np.array(lab[:max_windows], dtype=np.int64)
    return C, y, c


if __name__ == "__main__":
    seqs = load_uniprot()
    print(f"{len(seqs)} sequences; example len {len(seqs[0])}: {seqs[0][:40]}...")
    C, y, c = build_windows()
    print(f"windows {C.shape}, sequon center col={c}, positive rate {(y == 1).mean():.4f}")
