# Eighteen-bit LSH Monarch distillation

This experiment replaces Qwen's 1,024-dimensional tied token embeddings with
a frozen, tied 18-bit signed LSH codebook. It is isolated from the neighboring
full-width experiment but reuses its byte-identical context-16 FineWeb data.

```text
token ids [B, 16]
  -> frozen signed LSH lookup [B, 16, 18]
  -> flatten [B, 288]
  -> append one global zero [B, 289]
  -> Monarch stack (17 banks of 17x17 blocks)
  -> read final-token positions 270:288 [B, 18]
  -> frozen tied LSH unembedding [B, tokenizer_vocab]
```

The valid tokenizer has fewer than `2^18` rows, but ordinary 18-hyperplane LSH
is not injective because of hash collisions. Codebook preparation therefore
keeps one representative in each raw bucket and moves the other rows to the
nearest unused 18-bit codes. Hamming distance is the primary criterion;
projection margins and stable numeric ordering resolve ties. The artifact
records the original collision counts and the complete repair-distance
histogram. Input and output use the repaired bits as `-1/+1`, and the table is
never trained.

## Study

The screen contains only rank-one Monarch models:

- Sequential, one-map residual, unexpanded two-map residual, and expansion-4
  two-map residual forms.
- Depths 1, 2, 4, and 8 for every form, for 16 screen trials total.
- A 1,000-step screen at learning rate `1e-3`.
- Validation-only selection followed by two 1,000-step refinements at `3e-4`
  and `3e-3`.
- Three 4,000-step final runs at seeds 0, 1, and 2.

Training otherwise retains the base experiment's full-tokenizer,
temperature-1 `KL(P_teacher || P_student)`, effective batch 1,024, AdamW
settings, document-disjoint validation/test protocol, and resumable final
checkpoints.

## Local tests

From this directory:

```bash
uv run --with pytest pytest -q
```

The tests use synthetic embeddings and do not download Qwen.

## Modal stages

```bash
uv run modal run modal_app.py --stage tests
uv run modal run modal_app.py --stage prepare
uv run modal run modal_app.py --stage smoke
uv run modal run modal_app.py --stage screen
uv run modal run --detach modal_app.py::app.study_remote
uv run modal run modal_app.py --stage audit
```

`prepare` reuses `/cache/qwen_fullwidth_distill/context16-data-v2` when its
pinned manifest matches and creates
`/cache/qwen_lsh18_monarch/codebook/lsh18.pt`. Study artifacts live under
`/cache/qwen_lsh18_monarch/runs`, and metrics use the
`qwen-lsh18-monarch-distill` W&B project.

Codebook, result, progress, and final checkpoint artifacts are gated by both
the exact Qwen embedding hash and the repaired codebook hash. The final audit
checks the 16 + 2 + 3 trial grid, validation-only choices, test isolation,
checkpoint parameter counts, artifact hashes, and matching finished W&B runs.
