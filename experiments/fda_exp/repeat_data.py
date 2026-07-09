"""Repeat-family DNA (interspersed-repeat biology), and a reusable sweep.

Interspersed repeats (Alu, LINE) are the dominant structure in the genome: one
ancestral element copied ~10^6 times and then diverged (~87% identity for Alu).
So *long* contexts recur across copies even though `4^w >> m` — exactly the
regime where the unique-region promoters blew up. This is real repeat biology
(shared ancestry + divergence), not random and not a Fourier plant.

`repeat_family_seqs` models it: a fixed consensus + `n_copies` each mutated at
per-base rate `divergence`. (Swap in a real Alu/L1 consensus to make it literal.)
`tandem_repeat_seqs` is the extreme (satellite/microsatellite arrays).
"""

from __future__ import annotations

import numpy as np

_LETTERS = np.array(list("ACGT"))


def repeat_family_seqs(n_copies=8000, consensus_len=300, divergence=0.12,
                       n_families=1, seed=0):
    """`n_families` ancestral consensus sequences, each with `n_copies` diverged
    copies (point mutations at rate `divergence`)."""
    rng = np.random.default_rng(seed)
    seqs = []
    for _ in range(n_families):
        consensus = rng.integers(0, 4, size=consensus_len)
        for _ in range(n_copies):
            c = consensus.copy()
            mut = rng.random(consensus_len) < divergence
            c[mut] = rng.integers(0, 4, size=int(mut.sum()))
            seqs.append("".join(_LETTERS[c]))
    rng.shuffle(seqs)
    return seqs


def tandem_repeat_seqs(n_seqs=8000, motif_pool=("CAG", "AT", "AAAG", "GT"),
                       length=300, slip=0.02, seed=0):
    """Microsatellite/satellite arrays: a short motif tiled to `length`, with
    occasional slippage mutations. Extreme long-context repetition."""
    rng = np.random.default_rng(seed)
    seqs = []
    for _ in range(n_seqs):
        motif = motif_pool[rng.integers(0, len(motif_pool))]
        s = (motif * (length // len(motif) + 1))[:length]
        s = np.array(list(s))
        mut = rng.random(length) < slip
        s[mut] = _LETTERS[rng.integers(0, 4, size=int(mut.sum()))]
        seqs.append("".join(s))
    return seqs
