# Output-conditioned Fourier KL

This experiment is deliberately isolated from the pursuit and rescue stacks.
It trains one bank of exact Walsh characters from scratch.  Every character
implicitly contains a binary output-selector bit, so the model produces the
centered scores `[-gap(x)/2, +gap(x)/2]`, applies `log_softmax`, and minimizes
teacher-to-student KL against `[1-p_teacher, p_teacher]`.

For two classes this KL equals soft BCE minus teacher entropy.  The useful
changes are therefore the fresh persistent-log-potential support search, exact
product-vertex STE, continuous mask learning, functional diversity diagnostics,
and larger/longer H100 recipe—not a claim that rewriting BCE changes the math.

All tests and training run remotely:

```bash
cd experiments
uv run modal run fourier_output_kl/modal_train.py --stage tests
uv run modal run fourier_output_kl/modal_train.py --stage smoke
uv run modal run fourier_output_kl/modal_train.py --stage sweep
uv run modal run fourier_output_kl/modal_train.py --stage mobility_sweep \
  --x 131072 --steps 500 --coefficient-lr 0.01
uv run modal run fourier_output_kl/modal_train.py --stage train \
  --x 262144 --steps 800 --mask-lr 0.01 --coefficient-lr 0.01 \
  --support-layout semantic_structured_v2 --initial-score-gap 2.0
uv run modal run fourier_output_kl/modal_train.py --stage train \
  --x 262144 --steps 1000 --mask-lr 0.001 --coefficient-lr 0.003 \
  --support-layout semantic_structured_v2 --initial-score-gap 2.0 \
  --parent-run-id 3lnxu8vc --run-label output-kl-x262k-refine-1k
```

The semantic support bank reflects the actual fixed-field encoding.  Within
every degree it assigns 20% of characters to one target token, 10% to one
non-target token, 20% across target-derived tokens, 40% across target and
context, and 10% to unrestricted exploration.  The masks remain trainable for
the entire run.  A larger initial TopK log-score margin and lower mask LR slow
support churn without freezing it; retention/Jaccard and locality audits make
that behavior visible in W&B.  Exact index uniqueness is checked globally.

The diversity auxiliary is a near-duplicate hinge, not a generic
decorrelation loss.  Ordinary correlations between useful target features are
allowed; only empirical parity correlations above 0.95 are penalized.  The
runner also uses the cached one-million-example web teacher artifact plus 10x
sampling weight on all 90k in-domain EWT examples, fused AdamW, separate
clipped mask/coefficient groups, warmup-stable-decay, compiled chunks, compact
12-bit support serialization, block-scaled fp16 coefficients, W&B logging,
exact support repair, sampled empirical signature audits, and a hard 16 MB
exported-artifact gate (at least 100x versus the 1.6 GB teacher).

The selected 131,072-character mobility run was
[`voj9vjqj`](https://wandb.ai/umd-leans-well/fda-fourier-noun/runs/voj9vjqj):
test KL 0.01453, agreement 90.14%, probability R2 0.775, and 761.5x
compression.  Scaling revealed synchronized mask churn, so banks above 131k
now finish mask WSD halfway through training while coefficient WSD retains the
full horizon.  Masks remain live at a 5% LR floor.  The resulting 262,144-term
run [`3lnxu8vc`](https://wandb.ai/umd-leans-well/fda-fourier-noun/runs/3lnxu8vc)
achieved test KL 0.01351, agreement 90.47%, probability R2 0.7905, and 497.5x
compression; its best validation step was 700/800.

`--parent-run-id` starts a new W&B run from the parent's selected compact
artifact.  It restores the exact hard supports and quantized coefficients,
reopens every selected-vs-unselected mask boundary with the requested score
gap, resets AdamW, and runs a fresh WSD horizon.  This is intentionally distinct
from `--resume-run-id`, which continues the same run from its latest dense
checkpoint and step counter.
