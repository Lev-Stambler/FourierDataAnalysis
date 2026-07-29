# Qwen compression experiments

This directory is an ordered workspace of self-contained experiments:

1. `exp1_fullwidth_distill` — full-width dense, Monarch, BTT, and
   Kronecker distillation, plus the historical next-token baselines.
2. `exp2_lsh_monarch_distill` — 18-bit tied-LSH Monarch distillation.
3. `exp3_normuon_pretrain` — next-token Kronecker pretraining with NorMuon.
4. `exp4_adamw_control` — winner-matched AdamW control for experiment 3.
5. `exp5_kronecker_distill` — tensor-native, small-width Qwen distillation
   with depth-first Kronecker order/rank ablations.

Python import package names and remote artifact roots from experiments 1–4
are unchanged. The shared Northflank lifecycle and launch aliases live in
`northflank/`.

Run the complete local suite with:

```bash
uv sync --all-packages
uv run pytest -q
```
