# Qwen compression experiments

This directory is an ordered workspace of self-contained experiments:

1. `exp1_fullwidth_distill` — full-width dense, Monarch, BTT, and
   Kronecker distillation, plus the historical next-token baselines.
2. `exp2_lsh_monarch_distill` — 18-bit tied-LSH Monarch distillation.
3. `exp3_normuon_pretrain` — next-token Kronecker pretraining with NorMuon.
4. `exp4_adamw_control` — winner-matched AdamW control for experiment 3.
5. `exp5_kronecker_distill` — tensor-native, small-width Qwen distillation
   with depth-first Kronecker order/rank ablations.
6. `expv6` — long-run and parameter-matched Transformer controls.
7. `exp7` — dense tied-vocabulary continuation.
8. `exp8` — scale-safe canonical Kronecker factors.
9. `exp9` — standard Muon continuation and projection diagnostics.
10. `exp10` — completed causal 256-token TinyStories/WikiText architecture
    verdict. The standard Transformer won and is the active reference; the
    Kronecker path is retained only to reproduce the rejected comparison.
11. `exp11_kronecker_debug` — debug-first reopening after identifying Exp10's
    causal-row normalization defect. It implements the canonical order-three
    mixer, mechanistic gates, independently tuned WikiText controls, and a hard
    one-GPU-hour research controller; no scale run is pre-authorized.
12. `exp12_deep_kronecker` — completed paid 32-layer shared-basis,
    content-routed Kronecker pilot. It beat the independently tuned matched
    Transformer by `1.558` mean validation NLL across three seeds with fewer
    body parameters, but missed the locked throughput promotion threshold
    (`0.3952x` versus `0.4x`), so the 40M-token extension and test evaluation
    were not run.
13. `exp13_wikitext_confirmation` — adversarial eight-H100 confirmation of the
    Exp12 signal against four independently AdamW/Muon-tuned, total-parameter-
    matched standard Transformer aspect ratios, with disjoint sealed
    confirmation/final WikiText holdouts and token- plus compute-matched gates.

Python import package names and remote artifact roots from experiments 1–4
are unchanged. The shared Northflank lifecycle and launch aliases live in
`northflank/`.

Run the complete local suite with:

```bash
uv sync --all-packages
uv run pytest -q
```
