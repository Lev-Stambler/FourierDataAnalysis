#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

= Conclusion and Future Directions

An autoregressive model gives Dataset GL exactly the access it needs.
Real corpus contexts define the outer distribution; the model sampling probabilities define a non-uniform
conditional continuation law, and a cached realized prefix can be forked to draw independent conditional
suffixes.  In the corrected experiment the real context $Z$ is used only to sample $X|Z$.  A separate
teacher forward then receives exactly the generated $128$-token $X$ and defines the single vector function
$f(X)=P_theta(dot|X)$; it does not receive $Z$.
Processing coordinates newest-first aligns this oracle with native categorical Goldreich--Levin over
$ZZ_q^n$.

The resulting guarantee has no low-degree assumption: it recovers every heavy coefficient through token
degree $n=128$, with complexity governed by the live spectral width.
One categorical inverse DFT per live parent estimates all $q$ children from the same suffix pairs, so large $q$
increases computation but not conditional-sample count by a multiplicative $q$.

The recovered quantities are corpus-seed-conditional RMS character correlations of the
distribution-weighted function $f(X)$.  They are not non-uniform Fourier reconstruction weights, and rollout
density alone may create high-support moments.  A constant-output density diagnostic, supervised Fourier
refitting against $f(X)$, and held-out document evaluation therefore separate the theorem from the empirical
compression claim.  The teacher function and pure Fourier student are both defined on $X$ alone.  The input
features are only tokenizer-native $ZZ_q$ characters, represented by their cosine and sine coordinates, and
the student fits their vector output weights.

Results obtained from a prefix-inclusive label $P_theta(dot|Z,X)$ do not evaluate this corrected function
and cannot be carried over.  The $X$-only spectral search, Fourier fit, and held-out agreement evaluation must
therefore be rerun before making a compression or top-token-agreement claim.

Immediate next steps are to extend the measured three-depth profile toward all $128$ generated coordinates,
confirm every retained Fourier leaf on independent conditional samples, and test variance-sensitive
simultaneous bounds before spending samples on a frontier that Hoeffding leaves unresolved.

#h1([Acknowledgments])

*AI usage:* the author would like to acknowledge the use of language models (Gemini and Claude) in drafting
and revising this document; all theorem statements remain the author's responsibility and the categorical
identities are accompanied by executable verification tests.
