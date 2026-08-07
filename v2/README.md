# v2 — theory-focused draft

Clean rewrite of the paper, focused on the theory. Created 2026-08-03.

## Scope

Included (theory):

- `sections/dataset_fourier.typ` — Fourier analysis on datasets: lift, mass identity,
  normalized Parseval, dataset sensitivity, low-degree learning.
- `sections/generative_oracle.typ` + `sections/ar_categorical_gl.typ` — categorical
  Dataset GL for autoregressive rollout laws: conditional-bucket identities, weighted
  level mass, paired-suffix estimator, all-children DFT, fixed-prefix and random-context
  vector theorems.
- `sections/oracle_separation.typ` — affine alias/pair identities and the affine-oracle
  barrier separating prefix-conditional sampling from the chosen-point oracles of
  AGS / q-SFT / GFast (theory core of the old "Part 2"; preregistration and protocol
  JSON dropped).
- `sections/correlation_memory.typ` — representation/degree-growth theory for
  position-shared Fourier correlation memories.
- `sections/related.typ`, `sections/discussion.typ`, `sections/conclusion.typ`,
  `sections/appendix.typ`.

Deliberately excluded:

- The Qwen3.5 compression experiment preregistration (`sections/experiments.typ` in the
  repo root) and all result includes — the empirical program proved inconclusive. The
  discussion section states what future experiments must establish, without making any
  empirical claim.
- Everything under `NEW/`, `experiments/`, and `experiments--bin-lsh/` is untouched and
  not referenced by this draft.

The original `sections/` files are also untouched; this folder is a copy-and-edit, so
nothing is lost.

## Build

```bash
typst compile v2/main.typ v2/main.pdf
```

## Known TODOs

- Intro is a first draft; tighten contribution phrasing.
- `discussion.typ` mentions "early architecture experiments" only in passing — decide
  whether to cite them or keep them anonymous.
