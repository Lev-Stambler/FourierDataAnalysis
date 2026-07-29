# Multi-rank Kronecker ablation

This report compares sums of order-three Kronecker products at fixed full
activation width 16,384 and approximately 84M trainable student parameters.
All cells copy and freeze Qwen3.5-0.8B's tied embedding/unembedding matrix,
use context length 16, and optimize full-vocabulary forward KL.

## Parameter-matched architectures

| Physical depth | Kronecker rank per map | Trainable parameters |
|---:|---:|---:|
| 16 | 593 | 84,232,320 |
| 32 | 291 | 84,271,872 |
| 64 | 140 | 84,350,976 |
| 128 | 64 | 83,951,616 |

Here, rank is the number of independently learned order-three Kronecker terms
summed in each structured linear map:

`W = Σ_r m_r (A_r ⊗ B_r ⊗ C_r)`.

It is not the ordinary matrix rank of the materialized 16,384×16,384 map.

## Step-64 audit

Each cell below has seen 32,768 examples. Validation KL is measured on the
same deterministic 2,048-example audit split.

| Depth / rank | Base LR | Validation KL | Change from step 32 | Examples/s | Peak GiB | Activation growth |
|---|---:|---:|---:|---:|---:|---:|
| 32 / 291 | 3e-6 | **3.578852** | -0.317714 | 13.47 | 5.91 | 1.871× |
| 64 / 140 | 3e-6 | 3.612513 | -0.299430 | 13.80 | 7.15 | 1.900× |
| 128 / 64 | 3e-6 | 3.616505 | -0.310588 | 14.64 | 9.62 | 1.898× |
| 32 / 291 | 1e-6 | 3.808844 | -0.250129 | 13.37 | 5.91 | 1.655× |
| 64 / 140 | 1e-6 | 3.817258 | -0.245443 | 14.26 | 7.15 | 1.664× |
| 128 / 64 | 1e-6 | 3.838296 | -0.242350 | 14.81 | 9.62 | 1.649× |

At 3e-6 the entire depth/rank frontier spans only 0.037653 KL. The shallower,
higher-rank allocation is currently best and uses 39% less peak allocated
memory than depth 128, while depth 128 processes about 9% more examples per
second. Since parameter count is the primary constraint, depth 32/rank 291 is
the provisional winner.

All learned mixing distributions retain essentially their complete nominal
effective rank at step 64. The result therefore does not come from most
Kronecker terms being pruned or ignored.

## Parameter-matched Monarch control

The Monarch control has 84,279,296 trainable parameters and uses the same
batch 512, seed, data order, and audit split.

| Architecture | LR | Examples seen | Validation KL | Examples/s | Peak GiB | Activation growth |
|---|---:|---:|---:|---:|---:|---:|
| Monarch d4×4 | 1e-3 | 16,384 | **3.515551** | 426.20 | 3.69 | 3.789× |
| Monarch d4×4 | 5e-4 | 16,384 | **3.737357** | 457.25 | 3.69 | 3.234× |
| Kron d32/r291 | 3e-6 | 16,384 | 3.896567 | 13.44 | 5.91 | 1.717× |
| Monarch d4×4 | 1e-3 | 32,768 | **3.131375** | 428.50 | 3.78 | 4.958× |
| Monarch d4×4 | 5e-4 | 32,768 | **3.346186** | 457.33 | 3.69 | 4.230× |
| Kron d32/r291 | 3e-6 | 32,768 | 3.578852 | 13.47 | 5.91 | 1.871× |
| Monarch d4×4 | 1e-3 | 49,152 | **2.920932** | 432.81 | 3.78 | 5.233× |
| Monarch d4×4 | 5e-4 | 49,152 | **3.086656** | 457.10 | 3.69 | 5.172× |
| Kron d32/r291 | 3e-6 | 49,152 | 3.358458 | 13.73 | 5.91 | 2.043× |
| Monarch d4×4 | 1e-3 | 65,536 | **2.803485**¹ | 431.07 | 3.78 | 5.685× |
| Monarch d4×4 | 5e-4 | 65,536 | **2.956698**¹ | 452.73 | 3.69 | 5.630× |

At the first matched audit, Monarch leads by 0.159210 KL and processes about
34 times as many examples per second. Kronecker is substantially more stable
in activation scale. At the second audit, the Monarch lead grows to 0.232666
KL, and at the third it grows again to 0.271802 KL. Kronecker has not closed
the initial quality gap.

¹ Full 8,192-example validation. The corresponding 2,048-example audit was
2.956914 KL at 5e-4 and 2.808414 KL at 1e-3.

Doubling the Monarch learning rate improves final KL by another 0.153213
without destabilizing the run. Together with the 0.347949 improvement from
1e-6 to 3e-6 for d32/r291, this confirms that the short-budget comparisons
were strongly learning-rate limited.

### Monarch learning-rate boundary

The open Monarch LR edge was tested at the same 65,536-example budget.

| LR | Step-32 KL | Step-64 KL | Step-96 KL | Step-128 audit KL | Outcome |
|---:|---:|---:|---:|---:|---|
| 1e-3 | **3.515551** | **3.131375** | **2.920932** | **2.808414** | stable |
| 2e-3 | 3.822815 | 3.324867 | 3.052052 | 2.892458 | stable, worse |
| 3e-3 | — | — | — | — | stopped at step 32; 43.98× activation growth |

Thus 1e-3 is a real local optimum among the tested rates. The 2e-3 run never
catches it, and 3e-3 crosses the predeclared activation-divergence guard.

### Long Monarch control

The winning d4×4, 1e-3 control was run from scratch to 262,144 examples. Its
65,536-example audit reproduces the short run to within 0.00013 KL.

| Examples | Audit validation KL |
|---:|---:|
| 65,536 | 2.808280 |
| 131,072 | 2.559650 |
| 196,608 | 2.385310 |
| 262,144 | 2.290462 |
| 524,288 | 2.052646 |
| 786,432 | 1.911070 |
| 1,048,576 | 1.820780 |
| 1,310,720 | 1.738350 |
| 1,572,864 | 1.693950 |
| 1,835,008 | 1.654560 |
| 2,097,152 | 1.632230 |
| 2,359,296 | 1.523550 (LR decayed to 5e-4) |
| 2,359,296 | **1.504800** (LR decayed to 3e-4) |
| 2,621,440 | 1.480440 (LR decayed to 5e-4) |
| 2,621,440 | **1.463280** (LR decayed to 3e-4) |
| 2,883,584 | 1.456750 (LR decayed to 5e-4) |
| 2,883,584 | **1.439140** (LR decayed to 3e-4) |
| 3,145,728 | 1.443110 (LR decayed to 5e-4) |
| 3,145,728 | **1.420630** (LR decayed to 3e-4) |
| 3,407,872 | 1.401560 (held at 3e-4) |
| 3,407,872 | **1.387260** (LR decayed to 1.5e-4) |
| 3,670,016 | **1.374330** (LR 1.5e-4) |
| 3,932,160 | **1.361530** (LR 1.5e-4) |
| 4,194,304 | **1.338844** (full validation, LR 1.5e-4) |
| 4,456,448 | **1.342220** (audit, batch 512, LR 1.5e-4) |
| 4,456,448 | 1.370890 (audit, batch 128, LR 1.5e-4) |
| 4,456,448 | 1.344570 (audit, batch 128, LR 7.5e-5) |
| 4,456,448 | **1.337610** (audit, batch 128, LR 3.75e-5) |
| 4,456,448 | 1.337730 (audit, batch 64, LR 1.875e-5) |
| 4,456,448 | 1.339160 (audit, batch 32, LR 9.375e-6) |
| 4,718,592 | 1.338050 (audit, batch 512, LR 1.5e-4) |
| 4,718,592 | **1.332330** (audit, batch 128, LR 3.75e-5) |
| 4,718,592 | 1.333280 (audit, batch 64, LR 1.875e-5) |
| 4,718,592 | 1.334590 (audit, batch 32, LR 9.375e-6) |
| 4,980,736 | 1.335120 (audit, batch 512, LR 1.5e-4) |
| 4,980,736 | **1.328340** (audit, batch 128, LR 3.75e-5) |
| 4,980,736 | 1.328830 (audit, batch 64, LR 1.875e-5) |
| 4,980,736 | 1.331040 (audit, batch 32, LR 9.375e-6) |
| 5,242,880 | 1.330578 (audit, batch 512, LR 1.5e-4) |
| 5,242,880 | 1.311571 (full validation, batch 512, LR 1.5e-4) |
| 5,242,880 | 1.325610 (audit, batch 128, LR 3.75e-5) |
| 5,242,880 | **1.308061** (full validation, batch 128, LR 3.75e-5) |
| 5,242,880 | 1.326810 (audit, batch 64, LR 1.875e-5) |
| 5,242,880 | 1.309773 (full validation, batch 64, LR 1.875e-5) |
| 5,242,880 | 1.328880 (audit, batch 32, LR 9.375e-6) |
| 5,242,880 | 1.311883 (full validation, batch 32, LR 9.375e-6) |

The final full 8,192-example validation KL is **2.269406** and test KL is
2.281056. The curve remains monotonic, but this budget is still 1.269406 KL
above the eventual ≤1 target.

The exact continuation completes 2,097,152 examples at full-validation KL
**1.607329** and test KL 1.618024. AdamW resumed exactly from step 512, and
the final checkpoint retains the optimizer state. The target gap is still
0.607329 KL, so reaching ≤1 will likely require an architecture/capacity or
optimization improvement in addition to more data.

The first exact-AdamW decay audit is decisive: changing only the optimizer
group LR from 1e-3 at 2,097,152 examples reaches audit KL 1.523550 at 5e-4
and **1.504800** at 3e-4 after 2,359,296 examples. The lower LR leads by
0.018750 KL at matched model, optimizer moments, data, and example count.
The 5e-4 branch completes 3,145,728 examples at full-validation KL
**1.428054** and test KL 1.429777, reducing the target gap to 0.428054.
The 3e-4 winner completes at full-validation KL **1.403815** and test KL
1.406260. It wins by 0.024239 KL and reduces the target gap to 0.403815.
At the next matched audit, halving again to 1.5e-4 leads held 3e-4 by
0.014300 KL. The dominated hold branch was stopped. The 1.5e-4 branch
completes 4,194,304 examples at full-validation KL **1.338844** and test KL
1.339444. Its exact AdamW checkpoint is the source for a matched batch-512
versus batch-128 transition through 5,242,880 examples. At 4,456,448
examples, keeping batch 512 at 1.5e-4 reaches 1.342220. Changing only to
batch 128 at the same LR is substantially worse at 1.370890, but scaling the
LR down repairs the transition: 7.5e-5 reaches 1.344570 and 3.75e-5 reaches
**1.337610**. The latter narrowly beats the unchanged batch-512 path at that
boundary and extends its lead at 4,718,592 examples, reaching **1.332330**
versus 1.338050. Batch 512 subsequently reaches 1.335120 at 4,980,736
examples, while batch 128 reaches **1.328340**. Batch 512 completes at
full-validation KL **1.311571** and test KL
1.311737 at 5,242,880. Batch 128 completes at full-validation KL
**1.308061** and test KL 1.308596, a small but real 0.003509 improvement.
Batch 64 completes at full-validation KL **1.309773** and test KL 1.310304;
batch 32 reaches **1.311883** and test KL 1.312517. Thus batch 128 at
3.75e-5 is the mature optimum among the tested settings, leading batch 64
by 0.001712, batch 512 by 0.003509, and batch 32 by 0.003821.

### Parameter-matched tied depth

Looping the same four Monarch layers changes effective depth without changing
the 84,279,296 trainable parameters. Residual updates are scaled by the
inverse repetition count.

| Physical depth | Repetitions | Effective depth | Step-128 audit KL |
|---:|---:|---:|---:|
| 4 | 1 | 4 | **2.808414** |
| 4 | 2 | 8 | 2.953150 |
| 4 | 4 | 16 | 3.055880 |

The repeated models are worse at every audit and never catch the untied d4
baseline. At this budget, tied compute depth is not a substitute for distinct
parameters.

### Parameter-matched physical Monarch depth

Doubling `monarch_blocks` from 128 to 256 halves each layer's structured
factor count. Thus d8/blocks256 and d4/blocks128 both contain exactly
83,886,080 factor parameters while preserving the full 16,384-wide state.
Norms and biases make the complete trainable totals 84,672,512 and
84,279,296, respectively, a 0.47% difference.

| Examples | Depth | Blocks | LR | Validation KL |
|---:|---:|---:|---:|---:|
| 16,384 | 4 | 128 | 1e-3 | **2.997120** |
| 16,384 | 8 | 256 | 5e-4 | 3.021040 |
| 16,384 | 8 | 256 | 1e-3 | **2.995160** |
| 32,768 | 4 | 128 | 1e-3 | **2.762568** |
| 32,768 | 8 | 256 | 5e-4 | 2.821410 |
| 32,768 | 8 | 256 | 1e-3 | 2.797740 |
| 49,152 | 4 | 128 | 1e-3 | **2.654722** |
| 49,152 | 8 | 256 | 5e-4 | 2.705880 |
| 49,152 | 8 | 256 | 1e-3 | 2.673910 |
| 65,536 | 4 | 128 | 1e-3 | **2.540366** |
| 65,536 | 8 | 256 | 5e-4 | 2.606115 |
| 65,536 | 8 | 256 | 1e-3 | 2.565387 |

At 5e-4, the deeper, weaker-per-layer model is worse at every checkpoint and
finishes 0.065749 KL behind despite nearly identical trainable capacity. The
1e-3 control briefly ties at 16k, but finishes at 2.565387—still 0.025021
worse than d4. Activation-RMS growth remains finite at 4.327x. Trading
per-layer factors for physical depth therefore does not improve this Monarch
frontier.

### Monarch 2x-capacity allocation

The 2x-capacity screen matches the structured factor count exactly at
167,772,160, allocating it either to eight rank-one layers or four rank-two
layers. Including norms and biases gives 168,558,592 and 168,165,376
trainable parameters, respectively.

| Examples | Physical depth | Monarch rank | Validation KL |
|---:|---:|---:|---:|
| 16,384 | 8 | 1 | **3.688600** |
| 16,384 | 4 | 2 | **3.548180** |
| 16,384 | 8 | 1 (1/√depth residual) | 3.804510 |
| 32,768 | 8 | 1 | **3.270160** |
| 32,768 | 4 | 2 | **3.158290** |
| 32,768 | 8 | 1 (1/√depth residual) | 3.350490 |
| 49,152 | 8 | 1 | **3.029440** |
| 49,152 | 4 | 2 | **2.952820** |
| 49,152 | 8 | 1 (1/√depth residual) | 3.114340 |
| 65,536 | 8 | 1 | **2.887444** (full validation) |
| 65,536 | 4 | 2 | **2.820105** (full validation) |
| 131,072 | 4 | 2 | **2.558330** |
| 196,608 | 4 | 2 | **2.384040** |
| 262,144 | 4 | 2 | **2.269073** (full validation) |

The unscaled depth-8 model finishes 0.083959 KL worse than the 84M
depth-4/rank-1 baseline despite twice the parameters. Its peak activation-RMS
growth is 4.068, so a depth-aware residual scale is the necessary follow-up
before concluding that physical depth itself is ineffective.
Parallel rank is consistently better than unscaled physical depth and closes
the full-validation gap to 0.067339 KL, but it still trails the 84M
depth-4/rank-1 baseline by 0.016620 despite doubling the structured factors.
By 131,072 examples it reaches 2.558330, essentially tying the 84M
rank-one curve's 2.559650 at the same audit. The extra capacity learns more
slowly initially rather than remaining uniformly worse. The tie persists at
196,608 examples: 2.384040 for rank two versus 2.385310 for rank one.
At 262,144 examples the full-validation values are 2.269073 for rank two
and 2.269406 for rank one—a negligible 0.000333 gain for twice the factors.
At this budget, extra Monarch rank is not parameter-efficient.
The inverse-sqrt-depth residual control is worse at every completed audit and
hits the relative activation-growth guard at the final step. Its activations
remain finite, but this scaling recipe does not rescue depth.

### Monarch batch boundary

The original Monarch controls used effective batch 512. A fixed-example
batch-256 screen tests whether the family is update-limited too:

| Examples | Rank | Batch | LR | Validation KL |
|---:|---:|---:|---:|---:|
| 16,384 | 1 | 512 | 1e-3 | 3.515551 |
| 16,384 | 1 | 256 | 5e-4 | 3.388980 |
| 16,384 | 1 | 256 | 1e-3 | **3.205410** |
| 16,384 | 2 | 256 | 1e-3 | 3.229580 |
| 16,384 | 1 | 128 | 5e-4 | 3.126780 |
| 16,384 | 1 | 128 | 1e-3 | **3.049690** |
| 16,384 | 1 | 64 | 5e-4 | 3.066670 |
| 16,384 | 1 | 64 | 1e-3 | **3.024200** |
| 16,384 | 1 | 32 | 1e-3 | **2.997120** |
| 16,384 | 2 | 32 | 1e-3 | **2.988060** |
| 16,384 | 1 | 16 | 5e-4 | 2.969228 |
| 16,384 | 1 | 16 | 1e-3 | **2.946028** |
| 32,768 | 1 | 512 | 1e-3 | 3.131375 |
| 32,768 | 1 | 256 | 5e-4 | 3.056640 |
| 32,768 | 1 | 256 | 1e-3 | **2.917630** |
| 32,768 | 2 | 256 | 1e-3 | 2.936370 |
| 32,768 | 1 | 128 | 5e-4 | 2.899990 |
| 32,768 | 1 | 128 | 1e-3 | **2.834320** |
| 32,768 | 1 | 64 | 5e-4 | 2.825190 |
| 32,768 | 1 | 64 | 1e-3 | **2.802730** |
| 32,768 | 1 | 32 | 1e-3 | **2.762568** |
| 32,768 | 2 | 32 | 1e-3 | 2.771520 |
| 32,768 | 1 | 16 | 5e-4 | 2.773857 |
| 32,768 | 1 | 16 | 1e-3 | **2.749823** |
| 49,152 | 1 | 256 | 5e-4 | 2.871830 |
| 49,152 | 1 | 256 | 1e-3 | 2.784180 |
| 49,152 | 2 | 256 | 1e-3 | **2.773930** |
| 49,152 | 1 | 128 | 1e-3 | **2.697800** |
| 49,152 | 1 | 64 | 5e-4 | 2.701370 |
| 49,152 | 1 | 64 | 1e-3 | **2.655090** |
| 49,152 | 1 | 32 | 1e-3 | **2.654722** |
| 49,152 | 2 | 32 | 1e-3 | 2.666590 |
| 49,152 | 1 | 16 | 5e-4 | **2.635463** |
| 49,152 | 1 | 16 | 1e-3 | 2.639067 |
| 65,536 | 1 | 256 | 5e-4 | 2.759356 |
| 65,536 | 1 | 256 | 1e-3 | 2.677943 |
| 65,536 | 2 | 256 | 1e-3 | **2.673725** |
| 65,536 | 1 | 128 | 5e-4 | 2.653475 |
| 65,536 | 1 | 128 | 1e-3 | **2.599580** |
| 65,536 | 1 | 64 | 5e-4 | 2.613945 |
| 65,536 | 1 | 64 | 1e-3 | **2.565898** |
| 65,536 | 1 | 32 | 1e-3 | **2.540366** |
| 65,536 | 1 | 16 | 5e-4 | 2.553490 |
| 65,536 | 1 | 16 | 1e-3 | 2.575657 |
| 131,072 | 1 | 32 | 1e-3 | 2.356890 |
| 131,072 | 1 | 32 | 5e-4 | **2.293350** |
| 196,608 | 1 | 32 | 5e-4 | **2.211980** |
| 262,144 | 1 | 32 | 5e-4 | **2.150277** (full validation) |
| 393,216 | 1 | 32 | 2.5e-4 | **2.009850** |
| 393,216 | 1 | 32 | 5e-4 | 2.055740 |
| 524,288 | 1 | 32 | 2.5e-4 | **1.948950** |
| 655,360 | 1 | 32 | 2.5e-4 | **1.909610** |
| 786,432 | 1 | 32 | 2.5e-4 | **1.865430** |
| 917,504 | 1 | 32 | 2.5e-4 | **1.847270** |
| 1,048,576 | 1 | 32 | 2.5e-4 | **1.800870** (full validation) |
| 1,310,720 | 1 | 32 | 2.5e-4 | 1.758100 |
| 1,310,720 | 1 | 32 | 1.25e-4 | **1.722880** |
| 1,572,864 | 1 | 32 | 1.25e-4 | **1.695670** |
| 1,835,008 | 1 | 32 | 1.25e-4 | **1.674720** |
| 2,097,152 | 1 | 32 | 1.25e-4 | **1.660060** (audit) |
| 2,097,152 | 1 | 32 | 1.25e-4 | **1.642074** (full validation) |
| 2,359,296 | 1 | 32 | 1.25e-4 | 1.636960 |
| 2,359,296 | 1 | 32 | 6.25e-5 | **1.617080** |
| 2,621,440 | 1 | 32 | 1.25e-4 | 1.618410 |
| 2,621,440 | 1 | 32 | 6.25e-5 | **1.597340** |
| 2,883,584 | 1 | 32 | 1.25e-4 | 1.595730 |
| 2,883,584 | 1 | 32 | 6.25e-5 | **1.579100** |
| 3,145,728 | 1 | 32 | 1.25e-4 | 1.569241 (full validation) |
| 3,145,728 | 1 | 32 | 6.25e-5 | **1.550759** (full validation) |

Batch 256 at the unchanged 1e-3 LR improves the 32k audit by 0.213745 KL.
Halving LR gives up most of the gain, and doubling rank is already worse
than rank one at the first audit. Monarch, like Kronecker, was materially
update-limited in the original batch-512 comparison.
At the optimized batch-32 setting, rank two does reverse its early ordering:
it reaches 2.988060 versus rank one's 2.997120 at 16,384 examples. The
0.009060 gain costs 168,165,376 versus 84,279,296 trainable parameters,
however, so it is not parameter-efficient at this boundary. The advantage
then reverses: rank two trails by 0.008952 at 32,768 and by 0.011868 at
49,152 examples.
Batch 128 extends the full-validation gain to **2.599580** (test 2.596606),
0.203905 below the batch-512 control. Its 5e-4 counterpart finishes at
2.653475, confirming that 1e-3 remains the better startup LR.
Batch 64 improves again to **2.565898** (test 2.565716), while its 5e-4
counterpart reaches only 2.613945. The original batch-32 cell stopped at step
1,536 because its activation RMS rose 11.849x from an unusually small
step-one baseline; the absolute RMS was still only 1.388 and all losses and
gradients were finite. With the relative guard relaxed to 50x, it completes
at **2.540366** (test 2.540415), proving that the stop was diagnostic rather
than numerical.
Batch 16 is past the useful update-count frontier: 5e-4 reaches 2.553490 and
1e-3 reaches 2.575657, both worse than batch 32. The exact batch-32/1e-3
checkpoint is therefore the 262,144-example continuation source; hold-1e-3
and decay-to-5e-4 branches test its next LR boundary. At the first exact
continuation audit, decay-to-5e-4 wins decisively: **2.293350** versus
2.356890 at 131,072 examples. Both step-4,096 checkpoints were committed;
the joint app was then stopped and only the 5e-4 winner resumed, preserving
weights, AdamW moments, and the exact data cursor. It completes at
full-validation KL **2.150277** and test KL 2.169580. This beats the original
batch-512 rank-one result by 0.119129 and the doubled-rank result by 0.118796
at the same 262,144-example budget.

From that exact checkpoint, holding 5e-4 reaches audit KL 2.055740 at
393,216 examples, while decaying to 2.5e-4 reaches **2.009850**. The latter
continues monotonically to full-validation KL **1.800870** and test KL
1.813263 at 1,048,576 examples. Its terminal 2,048-example audit is 1.817330,
only 0.00345 below the old batch-512 curve's 1.820780 at the same example
count: the small-batch schedule learns much faster early, but most of that
advantage washes out by one million examples. Exact hold-2.5e-4 and
decay-to-1.25e-4 continuations test the next LR boundary. At 1,310,720
examples, the decay wins decisively at **1.722880** versus 1.758100. The
dominated hold branch was stopped and the 1.25e-4 branch is continuing
exactly through 2,097,152 examples. It reaches 1.695670 at 1,572,864,
essentially tied with the old batch-512 curve's 1.693950 at the same budget.
By 1,835,008 it reaches 1.674720, now 0.020160 behind the old curve's
1.654560: the optimized startup advantage has fully washed out. Its terminal
2,097,152-example audit is 1.660060, 0.027830 behind the old curve's
1.632230 audit. Full validation is **1.642074** and test KL is 1.654267,
0.034745 worse than the old batch-512 checkpoint's 1.607329.
The exact 2M→3M comparison is also complete. Decaying to 6.25e-5 wins every
audit and finishes at full-validation KL **1.550759** and test KL 1.561960,
0.018482 ahead of holding 1.25e-4 at 1.569241. Nevertheless, the optimized
small-batch path is now 0.146944 KL worse than the mature batch-512 schedule's
1.403815 at the same 3,145,728-example boundary: its early advantage has
decisively washed out.

### Kronecker optimization boundary

At the first matched 16,384-example audit:

| Examples | Effective batch | LR | Optimizer steps | Validation KL |
|---:|---:|---:|---:|---:|
| 16,384 | 512 | 3e-6 | 32 | 3.896567 |
| 16,384 | 512 | 6e-6 | 32 | 3.880600 |
| 16,384 | 256 | 3e-6 | 64 | **3.588640** |
| 16,384 | 256 | 6e-6 | 64 | 3.538120 |
| 16,384 | 128 | 3e-6 | 128 | 3.334980 |
| 16,384 | 64 | 3e-6 | 256 | **3.188510** |
| 16,384 | 64 | 6e-6 | 256 | **3.120170** |
| 16,384 | 32 | 3e-6 | 512 | **3.110960** |
| 16,384 | 32 | 6e-6 | 512 | **3.050580** |
| 16,384 | 16 | 3e-6 | 1,024 | **3.033080** |
| 16,384 | 16 | 6e-6 | 1,024 | **2.994060** |
| 16,384 | 8 | 3e-6 | 2,048 | **3.012640** |
| 16,384 | 8 | 6e-6 | 2,048 | **3.002260** |
| 32,768 | 512 | 3e-6 | 64 | 3.578852 |
| 32,768 | 512 | 6e-6 | 64 | 3.523520 |
| 32,768 | 256 | 3e-6 | 128 | **3.240050** |
| 32,768 | 256 | 6e-6 | 128 | 3.097550 |
| 32,768 | 128 | 3e-6 | 256 | 2.990340 |
| 32,768 | 64 | 3e-6 | 512 | **2.908900** |
| 32,768 | 64 | 6e-6 | 512 | **2.855520** |
| 32,768 | 32 | 3e-6 | 1,024 | **2.851520** |
| 32,768 | 32 | 6e-6 | 1,024 | **2.808420** |
| 32,768 | 16 | 3e-6 | 2,048 | **2.824760** |
| 32,768 | 16 | 6e-6 | 2,048 | **2.820710** |
| 49,152 | 512 | 3e-6 | 96 | 3.358458 |
| 49,152 | 512 | 6e-6 | 96 | 3.220250 |
| 49,152 | 256 | 3e-6 | 192 | **3.006740** |
| 49,152 | 64 | 6e-6 | 768 | **2.678920** |
| 49,152 | 32 | 3e-6 | 1,536 | **2.714233** |
| 49,152 | 32 | 6e-6 | 1,536 | **2.674580** |
| 49,152 | 16 | 3e-6 | 3,072 | **2.664220** |
| 49,152 | 16 | 6e-6 | 3,072 | **2.656940** |
| 65,536 | 512 | 3e-6 | 128 | 3.170772 |
| 65,536 | 512 | 6e-6 | 128 | 3.032330 |
| 65,536 | 256 | 3e-6 | 256 | **2.882244** |
| 65,536 | 256 | 6e-6 | 256 | 2.773251 |
| 65,536 | 128 | 3e-6 | 512 | 2.719047 |
| 65,536 | 64 | 3e-6 | 1,024 | **2.636279** |
| 65,536 | 32 | 3e-6 | 2,048 | **2.603122** |
| 65,536 | 32 | 6e-6 | 2,048 | **2.573685** |
| 65,536 | 16 | 3e-6 | 4,096 | **2.617731** |

Rows before 65,536 use the deterministic 2,048-example audit set; all
65,536-example rows use the complete 8,192-example validation set. At the
first audit,
doubling LR at batch 512 helps only 0.015967 KL, whereas halving the batch and
therefore doubling the number of AdamW updates helps 0.307927 KL. By the final
evaluation, both levers matter: 6e-6 improves b512 by 0.138442, and b256/3e-6
then improves another 0.150086. Kronecker is still primarily update-limited,
but is also LR-limited over this range. The extended curve continues improving
through batch 16. At the first audit, b16 gains 0.077880 over b32—essentially
the same gain as b64→b32—so sample-efficiency has not saturated yet. Both
b32 and b16 at only 16,384 examples beat the original b512 result after
65,536.
Batch 8 at 3e-6 improves on b16/3e-6 by only 0.020440, but it is 0.018580
worse than b16/6e-6 and takes twice as many optimizer steps. This makes LR
the more useful edge to complete before extending to still smaller batches.
At 6e-6, b8 improves by only 0.010380 over b8/3e-6 and remains 0.008200
worse than b16/6e-6, reinforcing the b16–b32 practical boundary.
At 32,768 examples, b64 reaches 2.908900 and beats the matched Monarch
1e-3 control's 3.131375 by 0.222475 KL; b32 extends the margin to 0.279855.
At 6e-6, b32 reaches 2.808420 and narrowly beats b16/6e-6 by 0.012290 at
the same example count, while requiring half as many optimizer steps.
The optimized Kronecker model is now better in fixed-example quality, though
it remains roughly 32× slower. On the complete validation split at 65,536
examples, b64 reaches 2.636279 and beats the matched Monarch d4×4 result by
0.167206 KL. Batch 32 improves the full-set result again to 2.603122, a
0.200363-KL advantage over Monarch.
Batch 16/3e-6 finishes at 2.617731 on the complete split, 0.014609 worse
than b32/3e-6 despite its stronger small audit. Thus b32 is also the robust
full-validation winner, not only the throughput choice.
At 6e-6, b32 improves again to full-validation KL **2.573685** and test KL
2.568880. It is the checkpointed Kronecker continuation source.

The exact b32 continuation keeps 6e-6 and tests a 1.2e-5 increase:

| Examples | LR | Validation KL |
|---:|---:|---:|
| 81,920 | 6e-6 | **2.502410** |
| 81,920 | 1.2e-5 | 2.611470 |
| 98,304 | 6e-6 | **2.470050** |
| 114,688 | 6e-6 | **2.429690** |
| 131,072 | 6e-6 | **2.358621** |
| 147,456 | 6e-6 | **2.337380** |
| 163,840 | 6e-6 | **2.304070** |
| 180,224 | 6e-6 | **2.269990** |
| 196,608 | 6e-6 | **2.262470** |
| 212,992 | 6e-6 | **2.270820** |
| 229,376 | 6e-6 | **2.232290** |
| 245,760 | 6e-6 | **2.205400** |
| 262,144 | 6e-6 | **2.200465** (full validation) |

Doubling LR damages the trained model immediately, whereas holding 6e-6
continues to improve apart from one small audit regression at 212,992
examples. It completes at full-validation KL **2.200465** and test KL
2.209868. At the matched 262,144-example boundary this is 0.050188 worse
than optimized Monarch's 2.150277. The early Kronecker quality advantage has
therefore disappeared, while its roughly 14 examples/s remains about 30x
below the optimized Monarch path.

The parameter-matched depth frontier was also rerun at the optimized batch-32,
6e-6 setting:

| Examples | Depth / rank | Trainable params | Validation KL |
|---:|---:|---:|---:|
| 16,384 | 4 / 2407 | 84,272,352 | 3.032570 |
| 16,384 | 8 / 1198 | 84,282,240 | **3.022270** |
| 16,384 | 16 / 593 | 84,232,320 | **3.027500** |
| 16,384 | 32 / 291 | 84,271,872 | 3.050580 |
| 16,384 | 64 / 140 | 84,350,976 | 3.048220 |
| 16,384 | 128 / 64 | 83,951,616 | 3.052790 |
| 32,768 | 4 / 2407 | 84,272,352 | **2.771010** |
| 32,768 | 8 / 1198 | 84,282,240 | 2.771340 |
| 32,768 | 16 / 593 | 84,232,320 | **2.788820** |
| 32,768 | 32 / 291 | 84,271,872 | 2.808420 |
| 32,768 | 64 / 140 | 84,350,976 | 2.819180 |
| 32,768 | 128 / 64 | 83,951,616 | 2.827340 |
| 49,152 | 4 / 2407 | 84,272,352 | **2.618190** |
| 49,152 | 8 / 1198 | 84,282,240 | 2.638290 |
| 49,152 | 16 / 593 | 84,232,320 | 2.658500 |
| 49,152 | 32 / 291 | 84,271,872 | **2.674580** |
| 49,152 | 64 / 140 | 84,350,976 | 2.696990 |
| 49,152 | 128 / 64 | 83,951,616 | 2.723110 |
| 65,536 | 4 / 2407 | 84,272,352 | **2.489824** (full validation) |
| 65,536 | 8 / 1198 | 84,282,240 | 2.514363 (full validation) |
| 65,536 | 16 / 593 | 84,232,320 | 2.538978 (full validation) |
| 65,536 | 32 / 291 | 84,271,872 | **2.573685** (full validation) |
| 65,536 | 64 / 140 | 84,350,976 | 2.607379 (full validation) |
| 65,536 | 128 / 64 | 83,951,616 | 2.638527 (full validation) |

The original d32/d64/d128 cells are tied within 0.00457 KL at the first
audit. By 32k, d32 leads d64 by 0.01076 and d128 by 0.01892; at 49k, that
lead grows to 0.02241 and 0.04853 KL. Moving parameters from Kronecker rank
into physical depth is becoming more harmful, not less, with training.
The shallower d16/r593 extension reverses the direction and reaches
**3.027500** at its first audit, beating d32 by 0.023080 at essentially the
same parameter count. It retains a 0.019600 lead at 32,768 examples,
reaching 2.788820 versus 2.808420. At the first boundary, d8/r1198 improves
again to 3.022270, 0.005230 better than d16 and 0.028310 better than d32.
d4/r2407 turns back up to 3.032570, 0.010300 worse than d8. The first
boundary therefore has an interior optimum at d8 rather than favoring the
shallowest possible stack. That early ordering does not persist: d4 is
already 0.020100 ahead of d8 at 49,152 examples and finishes at
full-validation KL **2.489824**, leading d8 by 0.024539, d16 by 0.049154,
d32 by 0.083861, d64 by 0.117555, and d128 by 0.148703. The final
fixed-parameter depth frontier is monotonic in favor of fewer physical
layers and more Kronecker mixture rank.

### Rank-chunk throughput

The d32/r291 implementation checkpoints each rank chunk. A matched H100
benchmark at effective batch 32 measured complete
teacher + student + backward + AdamW steps:

| Rank chunk | Examples/s | Peak GiB |
|---:|---:|---:|
| 16 | 9.474 | 4.28 |
| 32 | 12.760 | 4.49 |
| 48 | 12.526 | 4.69 |
| 64 | **12.772** | 4.89 |
| 128 | 11.211 | 5.71 |
| 291 | 10.138 | 7.77 |

Chunks 32–64 are effectively tied over the short five-step measurement.
Chunk 32 remains the production choice because it has the lowest memory in
that throughput plateau and already sustains about 14 examples/s over long
runs. Both smaller and larger chunks are slower, so chunking alone does not
explain or solve the large Monarch wall-clock advantage.

A full-stack `torch.compile` benchmark was also stopped after more than
14 minutes without completing its first warmup step and after repeated worker
heartbeat failures. Compilation is therefore not operationally viable for
this contraction graph in its current form.

## Step-96 audit

Each cell has now seen 49,152 examples.

| Depth / rank | Base LR | Validation KL | Change from step 64 | Examples/s | Activation growth |
|---|---:|---:|---:|---:|---:|
| 32 / 291 | 3e-6 | **3.358458** | -0.220395 | 13.73 | 2.043× |
| 128 / 64 | 3e-6 | 3.363334 | -0.253171 | 15.19 | 2.073× |
| 64 / 140 | 3e-6 | 3.372124 | -0.240389 | 14.44 | 2.075× |
| 32 / 291 | 1e-6 | 3.667238 | -0.141607 | 13.29 | 1.759× |
| 128 / 64 | 1e-6 | 3.695559 | -0.142738 | 15.33 | 1.749× |

The step-96 3e-6 frontier spans only 0.013666 KL. Depth 128 has almost
completely caught depth 32, but has not surpassed it. This is strong evidence
that, at fixed parameters and this data budget, the rank/depth allocation is
nearly flat. The practical Pareto choices are d32/r291 when quality and memory
matter most, or d128/r64 when throughput matters most.

## Final result at 65,536 examples

The final metric is KL on the full deterministic 8,192-example validation
set. All six runs completed normally.

| Depth / rank | Base LR | Full validation KL | Examples/s | Peak GiB | Effective rank min |
|---|---:|---:|---:|---:|---:|
| 32 / 291 | 3e-6 | **3.170772** | 13.49 | 5.91 | 290.9996 |
| 64 / 140 | 3e-6 | 3.177412 | 14.08 | 7.15 | 139.9999 |
| 128 / 64 | 3e-6 | 3.187571 | 14.65 | 9.62 | 64.0000 |
| 32 / 291 | 1e-6 | **3.518721** | 13.24 | 5.91 | 290.9998 |
| 64 / 140 | 1e-6 | 3.529058 | 13.85 | 7.15 | 140.0000 |
| 128 / 64 | 1e-6 | 3.552398 | 14.74 | 9.62 | 64.0000 |

d32/r291 is the final winner at both learning rates. At 3e-6, however, it
leads d64/r140 by only 0.006640 KL and d128/r64 by 0.016799 KL. In contrast,
changing d32/r291 from 1e-6 to 3e-6 improves KL by 0.347949. Optimization
choice matters far more than rank/depth allocation over the range tested.

The learned term-mixing distributions remain essentially full effective rank,
so the high-rank models did use all of their Kronecker terms. This supports
d32/r291 for the next optimization sweep: it has the best KL and lowest
memory, although its advantage over deeper allocations is small.

At the original batch-512 setting, the matched Monarch d4×4 control at 5e-4
reaches 2.956698 KL, 0.214074 lower than Kronecker at the same example budget,
and is about 33.6× faster. The small-batch sweep proves that this initial
Kronecker comparison was under-optimized: b64 already beats the better
Monarch 1e-3 control at 32,768 examples, and its full-validation advantage at
65,536 examples grows to 0.200363 KL at b32. Monarch's large wall-clock advantage is
unchanged.

## Runs

- [d32/r291, 3e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/l2vto5yt)
- [d64/r140, 3e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/o4981g3q)
- [d128/r64, 3e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/gldvgeb3)
- [d32/r291, 1e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/te4kt94k)
- [d64/r140, 1e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/vrn26mnh)
- [d128/r64, 1e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/6okn4vgd)
- [Monarch d4×4, 5e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/08q52u3b)
- [Monarch d4×4, 1e-3](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/ve4g1xku)
- [Kron d32/r291, 6e-6 boundary](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/3u3nlrdv)
- [Kron d32/r291, batch-256 boundary](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/r92htb99)
- [Kron d32/r291, long continuation](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/v2c0chqc)
- [Monarch d4×4, long 1e-3](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/qefm2tl0)
- [Monarch d4×4, 2e-3 boundary](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/rb8sm84d)
- [Monarch d4×4, 3e-3 boundary](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/0db2xl4f)
- [Monarch d4×4 repeat-2](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/ojlefkfd)
- [Monarch d4×4 repeat-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/ag1ryg8z)
- [Monarch d4×4, 2M continuation](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/vpusazm6)
- [Monarch d4×4, 2M→3M decay to 5e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/zrnfnp09)
- [Monarch d4×4, 2M→3M decay to 3e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/6sd49d8d)
- [Monarch d4×4, 3M→4M hold 3e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/dxu4m3fh)
- [Monarch d4×4, 3M→4M decay to 1.5e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/rqnmg55s)
- [Monarch d4×4, exact step-7168 restart](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/hytk5z7d)
- [Monarch depth-8/rank-1 capacity](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/6htx2uwe)
- [Monarch depth-4/rank-2 capacity](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/zn9a2wwk)
- [Monarch scaled depth-8 control](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/ni9wb4n1)
- [Monarch rank-2 checkpoint reproduction](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/0dnrdg96)
- [Monarch rank-2 262k continuation](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/w4675wv0)
- [Monarch rank-1 b256/1e-3](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/k7n0nc22)
- [Monarch rank-1 b256/5e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/ckb29np0)
- [Monarch rank-2 b256/1e-3](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/i34rywij)
- [Monarch rank-2 b32/1e-3](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/7hy1xoya)
- [Monarch rank-1 b128/1e-3](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/ietf4s7w)
- [Monarch rank-1 b128/5e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/z1645v0w)
- [Monarch rank-1 b64/1e-3](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/ehcel4vv)
- [Monarch rank-1 b64/5e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/22iopqfo)
- [Monarch rank-1 b32/1e-3 original guard stop](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/zwgxkw5s)
- [Monarch rank-1 b32/1e-3 guarded retry](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/w51hs63z)
- [Monarch rank-1 b16/1e-3](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/lewcopt6)
- [Monarch rank-1 b16/5e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/ffpy6xfa)
- [Monarch b32 continuation holding 1e-3](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/qzmbb9ge)
- [Monarch b32 continuation decaying to 5e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/c6zkiua2)
- [Monarch b32 5e-4 exact step-4096 restart](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/7owmixm3)
- [Mature Monarch batch-512 transition](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/pspo93gn)
- [Mature Monarch batch-128 transition](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/2sqnycv1)
- [Mature Monarch batch-512 exact step-9216 restart](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/jchgoyfa)
- [Mature Monarch batch-128 at 7.5e-5](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/nrlr3lmi)
- [Mature Monarch batch-128 at 3.75e-5](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/58odpe9h)
- [Mature Monarch batch-64 at 1.875e-5](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/xh9u1iid)
- [Mature Monarch batch-32 at 9.375e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/gvmwnmov)
- [Mature Monarch batch-64 exact step-73728 restart](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/0p87lzbx)
- [Mature Monarch batch-32 exact step-147456 restart](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/g4zn92xd)
- [Parameter-matched Monarch d8/b256-blocks, LR 5e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/vtomp65f)
- [Parameter-matched Monarch d8/b256-blocks, LR 1e-3](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/yvbjfwvz)
- [Optimized Monarch b32, 262k→1M decay to 2.5e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/ctd3iq8f)
- [Optimized Monarch b32, 262k→1M hold 5e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/kvsh7w58)
- [Optimized Monarch b32, 1M→2M hold 2.5e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/36tw2a3k)
- [Optimized Monarch b32, 1M→2M decay to 1.25e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/1itplqql)
- [Optimized Monarch b32, 2M→3M hold 1.25e-4](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/l2jj0vda)
- [Optimized Monarch b32, 2M→3M decay to 6.25e-5](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/korgstf6)
- [Optimized Monarch b32, 2M→3M hold restart](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/18z0f7uk)
- [Optimized Monarch b32, 2M→3M decay restart](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/033j1hds)
- [Kron d32/r291, b256/6e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/hm7rbwtm)
- [Kron d32/r291, b128/3e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/ca1b5qd4)
- [Kron d32/r291, b64/3e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/dvzd5ntq)
- [Kron d32/r291, b32/3e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/i2dwqamu)
- [Kron d32/r291, b16/3e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/chw0xdpq)
- [Kron d32/r291, b16/6e-6 checkpointable](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/81mfkeel)
- [Kron d32/r291, b8/3e-6 checkpointable](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/lxzsepg2)
- [Kron d32/r291, b32/6e-6 checkpointable](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/efyh2nnk)
- [Kron d32/r291, b64/6e-6 checkpointable](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/jzcnc12a)
- [Kron d32/r291, b8/6e-6 checkpointable](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/2zii98yy)
- [Kron d32/r291, b32 continuation at 6e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/nx2hm0vm)
- [Kron d32/r291, b32 continuation at 1.2e-5](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/bt1mtre3)
- [Kron d32/r291, exact step-4096 continuation restart](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/8ip98p6a)
- [Kron d32/r291, per-audit-checkpoint restart](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/el1gsvkq)
- [Kron d4/r2407, optimized b32/6e-6 depth control](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/tijihbwe)
- [Kron d8/r1198, optimized b32/6e-6 depth control](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/2qe5d578)
- [Kron d16/r593, optimized b32/6e-6 depth control](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/lsbat1sc)
- [Kron d4/r2407, per-audit-checkpoint restart](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/dy2kbqz9)
- [Kron d8/r1198, per-audit-checkpoint restart](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/fwu7dp7p)
- [Kron d16/r593, per-audit-checkpoint restart](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/zdwev5ei)
- [Kron d64/r140, optimized b32/6e-6 depth control](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/ionkrd2d)
- [Kron d128/r64, optimized b32/6e-6 depth control](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/z5jb4ups)
- [Kron d4/r2407, 65k→131k hold 6e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/61k9lmup)
- [Kron d4/r2407, 65k→131k decay to 3e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/b6fwy5ek)
- [Kron d8/r1198, 65k→131k hold 6e-6](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/6nyj5kan)
- [Mature Monarch b128 hold 3.75e-5](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/jd4gwb46)
- [Mature Monarch b128 decay to 1.875e-5](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/0ygzwjdy)
- [Optimized Monarch b32 3M→4M hold 6.25e-5](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/x6wkbfnf)
- [Optimized Monarch b32 3M→4M decay to 3.125e-5](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/1d3ea2lu)
- [Monarch rank-2/b32 relaxed-guard retry](https://wandb.ai/umd-leans-well/qwen-fullwidth-monarch-distill/runs/mhotaabn)

## Follow-ups

1. The original six-cell rank/depth study and the full batch/LR boundary are
   complete. Kronecker d32/r291 at batch 32 and 6e-6 is the continuation
   source.
2. The optimized d32/r291 Kronecker continuation reaches full-validation KL
   2.200465 at 262,144 examples, 0.050188 worse than matched-budget Monarch.
   Its doubled-LR branch is dominated. The complete depth frontier initially
   favors d8/r1198 at 16,384 examples but reverses with training: d4/r2407
   wins at 65,536 examples with full-validation KL 2.489824.
3. The Monarch startup batch frontier is complete. Batch 32/1e-3 wins at
   65,536 examples; batch 16 is past the useful frontier.
4. The optimized batch-32 Monarch reaches full-validation KL 2.150277 at
   262,144 examples. Decaying to 2.5e-4 wins its long-run LR branch and
   reaches full-validation KL 1.800870 at 1,048,576 examples. Exact
   decay-to-1.25e-4 then wins by 0.035220 at 1,310,720; the dominated hold
   branch is stopped. It finishes 2,097,152 examples at full-validation KL
   1.642074, 0.034745 worse than the old batch-512 curve. Decaying to
   6.25e-5 wins the exact 2M→3M branch at 1.550759, but now trails the mature
   schedule by 0.146944 at matched examples.
5. Doubling Monarch rank and trading structured factors for physical depth
   are both parameter-inefficient at the measured budgets. The fair rank-2
   batch-32 cell trails rank one at 32k and 49k; a 50× diagnostic-guard retry
   is configured for its final boundary.
6. The mature batch-512 model reaches full-validation KL 1.338844 at
   4,194,304 examples and full-validation KL 1.311571 at 5,242,880. Batch 128 at the
   unchanged LR is worse; at the first transition audit 3.75e-5 is best at
   1.337610, then extends its lead to 1.332330 versus 1.338050 at 4,718,592.
   At 4,980,736 it leads 1.328340 versus 1.335120.
   It finishes at full-validation KL 1.308061, beating batch 512 by 0.003509,
   batch 64 by 0.001712, and batch 32 by 0.003821. Batch 128/3.75e-5 is the
   mature winner among all four tested batches.
7. Modal hit the workspace spend limit at 06:29 UTC on 2026-07-25 and
   terminated the original eight H100 containers. After the budget was
   raised, the audited restart slate launched eight replacement H100s at
   01:10 UTC on 2026-07-26. All eight replacements completed normally:
   mature b64/b32, d32 through 262,144 examples, both 2M→3M LR branches, and
   the complete d4/d8/d16 shallow frontier. That restart allocation was empty
   before the next slate launched.
8. The next eight-way winner slate launched at 12:21 UTC on 2026-07-26 in
   Modal app `ap-hH3T9KjX8X7JHysSr3jJa4`. It uses all eight H100 slots:
   d4 hold/decay and d8 hold through 131,072 examples; mature b128
   hold/decay through 6,291,456; optimized b32 hold/decay through 4,194,304;
   and a fresh rank-2/b32 Monarch retry under the corrected 50× diagnostic
   guard.
