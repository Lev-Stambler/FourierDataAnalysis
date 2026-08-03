# Experiment 12: deep content-routed Kronecker mixer

Experiment 12 treats the mixing thesis as a falsifiable architecture claim:
Kronecker structure should buy more useful compositional depth and broader use
of the hidden representation than a parameter-matched Transformer. It does not
assume that merely repeating the Experiment 11 block will work.

No paid Exp12 result exists yet. The architecture, correctness gates,
successive-halving tuner, held-out promotion rule, and resumable GPU runner are
implemented and locally verified.

## Why Experiment 11 underused its parameters

The Experiment 11 rank-8 body has `2,125,888` parameters. Its eight independent
packed causal matrices consume `2,105,344`, or `99.03%`, leaving little learned
capacity for channel mixing. It has eight residual/nonlinear updates and no
FFN. That is a poor test of a thesis about repeatedly mixing and reusing rich
representations.

## Current architecture

The new candidate is `deep-kron-r8`:

- context `256`, vocabulary `16,384`, tied input/output vocabulary matrix;
- hidden width `128`, tensorized as `8 × 16`;
- `32` blocks and `64` nonlinear residual updates;
- Kronecker rank `8`;
- four row-energy-balanced learned causal basis banks, reused cyclically;
- layer-specific `8 × 8` and `16 × 16` channel factors;
- a zero-initialized content router producing a rank gate for every token;
- a different bijective channel layout in every layer, so successive
  factorizations do not preserve one fixed grouping of the representation;
- a layer-specific SwiGLU channel update of width `256` after every mixer;
- learned mixer and FFN residual gains initialized to `1/sqrt(2*depth)`.

For block `l`, normalized hidden state `Z` is rearranged by layout `P_l`, and
the mixer is

```text
M_l(Z)[s] = P_l^-1 sum_r g_l(Z[s])_r a_l,r
            (P_l Z ×token A_bank(l),r ×mode1 B_l,r ×mode2 C_l,r)[s]
H <- RMS(H + alpha_l M_l(SiLU(RMS(H))))
H <- RMS(H + beta_l SwiGLU_l(RMS(H)))
```

`A` is strictly causal and normalized independently in every output row. The
router depends only on the current token state, so it adds content adaptation
without violating causality.

## Matched control

The control is a 32-layer, width-128, four-head causal RoPE Transformer with a
width-192 SwiGLU and the same tied vocabulary, normalization, and learned deep
residual scaling.

| Model | Depth | Nonlinear residual updates | Body parameters | Total parameters |
|---|---:|---:|---:|---:|
| deep Kronecker rank 8 | 32 | 64 | 4,313,408 | 6,410,560 |
| Transformer | 32 | 64 | 4,456,512 | 6,553,664 |

The Kronecker body is `96.789%` of the Transformer body. Its four shared causal
banks are `24.405%` of its body, versus static position tables consuming
`99.034%` of the Experiment 11 body.

## Local gates

The checked implementation currently passes:

| Check | Result |
|---|---:|
| Factored vs materialized forward max error | `4.44e-16` |
| Factored vs materialized backward max error | `1.42e-14` |
| Future entries in learned causal bases | `0` |
| Causal-row energy max error | `2.38e-7` |
| Kronecker/Transformer prefix error | `0 / 0` |
| Distinct layer channel layouts | `32` |
| Neutral content-router error from one | `0` |
| All parameter gradients finite and active | yes |
| Tiny memorization NLL, initial -> final | `3.26765 -> 0.03033` |

## Compute-smart WikiText campaign

The runner tunes AdamW and canonical Muon independently for both architectures.
Selection uses validation only; test data is not loaded unless the 10M-token
promotion gate passes.

1. Coarse screen: four LRs per optimizer family, seed 0, 1M tokens, with up to
   two automatic 3x boundary extensions.
2. Robust screen: the top two LRs from each family, constant versus token-based
   warmup/cosine, seeds 0 and 1, 3M tokens. Muon also tests half/double auxiliary
   AdamW LR.
3. Selection screen: the best two recipes overall per architecture, three
   seeds, 10M tokens.
4. Promotion requires mean paired Kronecker-minus-Transformer validation NLL
   `<= 0.05`, every seed `<= 0.15`, body ratio `<= 1.0`, and measured throughput
   ratio `>= 0.4`.
5. Only after promotion are both selected recipes extended to 40M tokens and
   evaluated on test. A frontier win requires the paired-bootstrap 95% upper
   bound on test-NLL difference to be `<= 0.02`.

The maximum pre-promotion budget is `232M` prediction tokens. The additional
three-seed final costs `180M` tokens only after promotion, for a maximum of
`412M`. Cells checkpoint by architecture, optimizer recipe, seed, and token
horizon, so successive stages resume instead of restarting.

The paid preflight authenticates W&B, requires a direct run URL, benchmarks the
actual AdamW and Muon paths separately, searches down from ambitious physical
batches, records utilization/power/VRAM/throughput/global token batch, tests
compilation, uses no gradient accumulation, and refuses the campaign if
measured throughput cannot fit the locked wall budget.

## Commands

```bash
uv run --no-sync python -m exp12_deep_kronecker inventory
uv run --no-sync python -m exp12_deep_kronecker local-correctness
uv run --no-sync pytest -q exp12_deep_kronecker/test_exp12.py
```

After locking the controller manifest and provisioning the exact paid target:

```bash
uv run --no-sync python -m research_control plan \
  exp12_deep_kronecker/experiment.json
uv run --no-sync python -m research_control run \
  exp12_deep_kronecker/experiment.json --stage local-correctness
uv run --no-sync python -m research_control run \
  exp12_deep_kronecker/experiment.json --stage wikitext-campaign
```
