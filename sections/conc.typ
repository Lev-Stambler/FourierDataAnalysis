#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

= Conclusion and Future Directions

An autoregressive model gives Dataset GL exactly the access it needs.
Real corpus contexts define the outer distribution; the model sampling probabilities define a non-uniform
conditional continuation law, and its tokenizer-level
probabilities label those continuations, and a cached realized prefix can be forked to draw independent
conditional suffixes.
Processing coordinates newest-first aligns this oracle with native categorical Goldreich--Levin over
$ZZ_q^n$.

The resulting guarantee has no low-degree assumption: it recovers every heavy coefficient through token
degree $n=128$, with complexity governed by the live spectral width.
One categorical inverse DFT per live parent estimates all $q$ children from the same suffix pairs, so large $q$
increases computation but not conditional-sample count by a multiplicative $q$.

The recovered quantities are prefix-conditional RMS character correlations of a distribution-weighted
target.  They are not non-uniform Fourier reconstruction weights, and rollout density alone may create
high-support moments.  Constant-output and continuation-residual controls, supervised refitting, and
held-out document evaluation therefore separate the theorem from the empirical compression claim.

The cyclic token-id geometry remains a modeling choice, not a linguistic fact.
For that reason the empirical study must include the centered one-hot simplex control and fixed random
vocabulary permutations.
Stable support-level and predictive results across those controls are the evidence that a recovered
interaction belongs to the model and rollout distribution rather than to token numbering.

Immediate next steps are to measure the live-width profile at lens $128$, validate the all-child estimator
against exhaustive small-$q$ models, and compare $ZZ_q$ recovery with simplex refits on fresh rollouts.

#h1([Acknowledgments])

*AI usage:* the author would like to acknowledge the use of language models (Gemini and Claude) in drafting
and revising this document; all theorem statements remain the author's responsibility and the categorical
identities are accompanied by executable verification tests.
