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
high-support moments.  The constant-output density diagnostic, continuation residual, supervised Fourier
refitting, and held-out document evaluation therefore separate the theorem from the empirical compression
claim.  The theorem and student use only tokenizer-native $ZZ_q$ characters, represented for the real-valued
student by their cosine and sine coordinates.

Most importantly, the predeclared $90%$ target was not met: the primary Fourier student reached $18.22%$
top-token agreement at a $27.10 times$ full-checkpoint parameter ratio.  The experiment demonstrates that
Dataset GL's heavy-correlation guarantee alone is far from a high-agreement compression theorem.

Immediate next steps are to extend the measured three-depth profile toward all $128$ generated coordinates,
confirm every retained Fourier leaf on independent conditional samples, and test variance-sensitive
simultaneous bounds before spending samples on a frontier that Hoeffding leaves unresolved.

#h1([Acknowledgments])

*AI usage:* the author would like to acknowledge the use of language models (Gemini and Claude) in drafting
and revising this document; all theorem statements remain the author's responsibility and the categorical
identities are accompanied by executable verification tests.
