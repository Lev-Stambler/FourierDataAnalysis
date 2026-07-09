"""Experiments for 'Fourier Analysis on a Dataset'.

Two headline measurements, matching the paper:

- E2 (context profile): does the search width ``N`` / level mass ``R_k`` stay small
  for structured/real data and blow up for generic (random) data?  This decides
  whether ``thm:context-gl`` (Goldreich--Levin from context conditioning) is
  non-vacuous in practice.
- E5 (degree): is a trained model *low-degree over the data* even when it is not
  low-degree globally?  This is the empirical form of the conclusion's open
  question.

All quantities are computed *exactly* (fast Walsh--Hadamard transform, full
subset enumeration) on a small-``n`` track (``n <= ~20``) so there is genuine
ground truth.  The identity tests in ``tests/`` encode the paper's theorems.
"""

__all__ = ["spectrum", "datasets", "context_profile", "degree", "figures"]
