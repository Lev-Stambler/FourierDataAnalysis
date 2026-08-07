# Exp20: cache the causal outer block and make the mixer measurable

Exp20 is a cloud-only implementation and throughput exploration derived from
the corrected Exp19 clean-token model. It separates three hypotheses that must
not be conflated:

1. the current dense lower-triangular outer factor admits an exact blockwise
   inference cache;
2. the same dense model may execute faster when its inner Kronecker products
   are expressed as rank-batched GEMMs instead of multi-input `einsum`;
3. a separately named order-three semiseparable outer can replace sixteen
   cached group tensors with three recurrent state tensors.

The objective remains the 16-token block-shifted objective. Exp20 does not add
the within-block causal decoder and makes no ordinary autoregressive claim.

## Exact cache

For each layer, the existing mixer is split into

$$
z_g = (W_{1,r}\otimes W_{2,r})\,\phi(x_g)\,
      (C_{1,r}\otimes C_{2,r})^T,
\qquad
y_s = \sum_{g\le s} A_{r,s,g}z_g.
$$

The dense cache stores each completed $z_g$ in the permuted pre-outer basis.
When group $s$ arrives, only $z_s$ is computed and the new row $A_{s,:s+1}$ is
applied. Lower-triangular causality guarantees that no completed group needs to
be updated. The cache API is inference-only and rejects active autograd.

## Compact recurrent outer

The separately labeled semiseparable candidate has three states per mixer
rank:

$$
h_{s,m}=a_{s,m}h_{s-1,m}+b_{s,m}z_s,
\qquad
y_s=\frac{\sum_m c_{s,m}h_{s,m}}{\|A_{s,:}\|_2}.
$$

The row norm is computed from the explicitly tracked causal row coefficients.
The original $3\times3$ Gram quadratic form was algebraically exact but failed
the first H100 BF16 preflight: cancellation made a nominally non-negative
quadratic form produce a non-finite gradient. The explicit-row norm is
non-negative by construction and is accumulated in FP32. The full materialized
matrix, full recurrent scan, and streaming step share the same parameters and
are numerically audited against one another. This changes the architecture and
is never reported as an implementation-only speedup.

## Registered tracks

| Track | Mixer rank | Outer | Inner backend | Hidden channel |
|---|---:|---|---|---:|
| `dense-r8-reference` | 8 | dense history | original factorized einsum | 250 |
| `dense-r8-packed` | 8 | dense history | packed GEMM | 250 |
| `dense-r4-packed` | 4 | dense history | packed GEMM | 255 |
| `dense-r2-packed` | 2 | dense history | packed GEMM | 258 |
| `dense-r1-packed` | 1 | dense history | packed GEMM | 259 |
| `semiseparable3-r8-packed` | 8 | recurrent order 3 | packed GEMM | 250 |
| `transformer-d32-w128` | — | block-causal attention | fused SDPA/GEMM | 97/98 |

All models remain within `0.1%` of 5,400,896 parameters. The group-density
branch remains rank one; the sweep changes the structured mixer rank only.

## Paid-run gates

- Authenticate W&B and obtain a direct run URL before training.
- Require exactly eight H100-80GB GPUs and fill every GPU in every paid wave.
- Pass full forward/backward, checkpoint, compiled BF16, block-cache, and
  semiseparable materialization parity.
- Search downward from 4,096 contexts, require at least 100,000 global tokens
  per optimizer step, no gradient accumulation, and at least 85% median GPU
  utilization for training benchmarks.
- Record operator traces, throughput, step time, allocated/reserved VRAM,
  utilization, power, cache bytes, and per-prefix inference latency.
- Require packed rank eight to reach at least `2x` the contemporaneous dynamic
  reference before spending on learning screens.
- If it misses, stop with verdict `requires-fused-triton` rather than hiding an
  implementation failure in an architecture comparison.
- Independently tune AdamW and Muon for dense rank eight and recurrent rank
  eight, then confirm on two examples and three fresh seeds.
- Promote recurrent order three only if it is no more than 25% worse in
  optimizer steps and faster in wall-clock time.

The campaign stops before corpus training regardless of outcome. Direct run
links and measured results belong in [`RUN_RESULTS.md`](RUN_RESULTS.md).
