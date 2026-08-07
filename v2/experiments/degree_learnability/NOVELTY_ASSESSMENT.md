# Research novelty assessment

Date: 2026-08-07

## Verdict

The broad idea is **not unique**. Fourier/degree bias, sparse-variable
learnability, context-length effects, and preference for nearby context all have
substantial prior art. The categorical inverse-likelihood basis is directly
borrowed from Ferrere et al., not introduced here.

This is also not “the first model-independent theory of dataset hardness.”
Data-derived, model-agnostic complexity measures predate this work, and v2.3-v2.4
directly show that the same spectral dependency has different difficulty under
different positional encodings. A descriptor can be independent of a trained
model without its relationship to learning difficulty being independent of the
learner.

The exact combination may still support a narrow empirical contribution:

> Estimate a data-conditioned categorical ANOVA spectrum of a next-token
> conditional on real byte streams, then use a byte-multiset-preserving
> intervention to move low-degree dependence outward and test whether support
> locality predicts fixed-budget Transformer learning beyond degree.

A targeted search did not locate that exact experimental construction. That is
not proof of novelty; a paper would still need a systematic related-work search.

## Directly overlapping prior art

- [Rahaman et al. (ICML 2019)](https://proceedings.mlr.press/v97/rahaman19a.html)
  established frequency-dependent neural-network learning speed.
- [Gorji et al. (UAI 2023)](https://proceedings.mlr.press/v216/gorji23a.html)
  explicitly measured low-degree Walsh-Hadamard bias for networks on discrete
  inputs.
- [Hahn and Rofin (ACL 2024)](https://aclanthology.org/2024.acl-long.800/)
  connected Transformer learnability to low sensitivity and low degree through
  loss-landscape geometry.
- [Bhattamishra et al. (2022)](https://arxiv.org/abs/2211.12316) studied
  Transformer simplicity bias and sparse Boolean functions empirically.
- [Edelman et al. (ICML 2022)](https://proceedings.mlr.press/v162/edelman22a.html)
  analyzed sparse relevant variables in self-attention and obtained logarithmic
  context-length sample complexity in their setting.
- [Lavie et al. (ICML 2024)](https://arxiv.org/abs/2402.05173) used symmetric-group
  sectors to derive Transformer learnability and context-length scaling in an
  infinite-width setting.
- [Khandelwal et al. (ACL 2018)](https://aclanthology.org/P18-1027/) showed that
  language models treat nearby context more precisely than distant context.
- [Ferrere et al. (2026)](https://arxiv.org/abs/2603.02673) supplied the
  inverse-likelihood categorical functional-ANOVA construction used here.
- [Lorena et al. (2019)](https://arxiv.org/abs/1808.03591) surveyed a large
  pre-existing literature on complexity measures extracted directly from
  datasets to predict classification difficulty.
- [Shamir (JMLR 2018)](https://jmlr.org/papers/v19/17-537.html) studied
  distribution-specific neural-network hardness with Fourier-based lower-bound
  techniques and showed why simple assumptions on either the distribution or
  target alone do not provide a general learnability guarantee.

These papers prevent defensible claims that this project discovered spectral
bias, low-degree bias, sparse-variable bias, or context locality in general.

## What is plausibly new here

1. The use of nested cross-fitted Brier projections to estimate degree-level
   square mass of a categorical next-byte conditional under dependent context.
2. The stride-interleaving intervention, which preserves the exact byte multiset
   while moving a strong pair dependency from $(1,2)$ to $(s,2s)$.
3. Prospective, blocked evidence on prose, source code, formal mathematics, and
   technical text that locality rank tracks finite-budget held-out CE while
   measured degree remains nearly fixed.
4. The negative model-selection result: a simple log-radius baseline predicts
   held-out corpus deltas better than the proposed Parseval-weighted
   $G_{\mathrm{total}}$. Reporting this weakens the grand theory but strengthens
   the credibility of the empirical result.
5. A resolved dyadic lower-bound surface that aggregates all 21 profiled lag
   pairs without double-counting overlapping projections, followed by a frozen
   12-corpus prediction test against entropy, bigram, mutual-information, and
   compression controls.
6. The negative confirmatory finding that those Fourier summaries predict unseen
   natural-text difficulty but do not outperform the ordinary controls, alongside
   an exact equal-energy factorial that separates degree from architecture-specific
   locality.

The clean paper claim is therefore about a controlled empirical methodology and
result, not a new Fourier transform or theorem.

## Model-independent claim boundary

The spectrum and radius are **model-agnostic descriptors**: they are computed
from the data distribution without training the student model. The supported
statement is nevertheless model-specific:

\[
\text{descriptor ordering}\quad\Longrightarrow\quad
\text{finite-budget difficulty for the tested Transformers}.
\]

A model-independent theory would need to specify a nontrivial learner class and
prove a lower or upper bound that holds across that class. Without restricting
the learner, the same dependency can be built into one architecture and absent
from another, making an absolute learner-free hardness ordering untenable.

## Why it is not conclusive yet

- The v2.4 spectrum covers all pairs on a seven-lag dyadic grid but is still only
  a degree-2 lower bound, not the full context. Unmeasured higher-order and
  off-grid dependencies can change under interleaving.
- Radius, stride, lane periodicity, and the reorganization of all sequential
  structure move together. The intervention is byte-marginal matched, not a
  surgical change to one Fourier coefficient.
- $G_{\mathrm{total}}$, its entropy upper bound, and radius are nearly
  collinear here. Rank correlation cannot distinguish them, and quantitative
  validation favored radius.
- Final CE at one budget measures finite-budget optimization/generalization, not
  sample complexity. Learning curves help but do not replace budget-scaling
  experiments.
- Results cover two Transformer sizes and three positional encodings. The
  sinusoidal failure and exact-factorial interactions actively reject a universal
  architecture-independent ordering; RNN, convolutional, and state-space controls
  remain missing.
- The corpus is the blocking unit. Many GPU seeds reduce measurement noise but
  do not create more independent domains.
- On 12 new natural corpora, the Fourier features did not improve locked ridge
  prediction over simple controls. Any paper must lead with that negative result,
  not only the earlier within-corpus rank correlations.
- The six-dataset image result is exploratory and confounded by the shared VQ
  tokenizer; nominal p-values are not corrected for the inspected descriptors.

## Publication-level next tests

1. Extend the exact factorial with learnable degree-2 targets, same-degree and
   same-radius support-shape contrasts, and token budgets large enough to separate
   failure to learn from delayed learning.
2. Replace the pairwise lower envelope with scalable projections onto joint
   multi-position subspaces, including degrees above two and off-grid lags.
3. Fit token-budget scaling curves and compare tokens-to-threshold, not only one
   endpoint.
4. Cross the data interventions with Transformer, RNN, local convolution, and
   state-space learners.
5. Expand the locked prediction panel and determine whether Fourier features add
   value beyond the already-strong bigram/entropy/compression controls.
6. Derive a model-specific bound linking support geometry to an attention-kernel
   eigenvalue, gradient signal, or sample requirement.

Current readiness: a credible empirical workshop paper if framed around the
controlled methodology, architecture interaction, and honest negative prediction
result; not a standalone model-independent theory of dataset hardness.
