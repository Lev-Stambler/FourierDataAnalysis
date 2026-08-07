#import "../../levs-commands/main_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

= Conclusion and Open Problems

Autoregressive models give Dataset Goldreich--Levin exactly the access it needs: real corpus contexts
define the outer distribution, the sampling kernel defines a non-uniform conditional continuation law, and
a cached realized prefix can be forked to draw independent conditional suffixes.  Processing coordinates
newest-first aligns this oracle with categorical Goldreich--Levin over $ZZ_q^n$, and one shared length-$q$
inverse Fourier transform per live parent estimates all categorical children at once.  The resulting
guarantee has no low-degree assumption and its complexity is governed by the live spectral width.

The same analysis is honest about limits.  The generative oracle is strictly weaker than the chosen-point
oracles of finite-group sparse Fourier transforms: the affine alias and pair identities pin down exactly
what density-and-label point queries buy, and the affine-oracle barrier shows why prefix-conditional
sampling cannot substitute for them.  And recovered characters are correlations, not reconstruction
weights; the density-aliasing pathology is exact, not asymptotic.

*Open problems.*
+ *Live-width profiles.*  Measure or bound the live spectral width $N$ of concrete teacher--rollout pairs.
  The theorem is instance-optimal in $N$ up to logarithms; whether real language-model spectra have small
  $N$ at useful thresholds is open.
+ *New oracles.*  An affine-difference *conditional pair* oracle, with event-probability bounds under
  non-uniform laws, would support vector energy-hashing algorithms beyond the reverse-time tree.  The
  identities of @sec:oracle-separation are the starting point; the sampling access is the missing piece.
+ *Vector singleton peeling.*  Published $q$-SFT singleton tests are scalar.  A vector peeling theorem
  compatible with paired Hermitian estimators would remove the $q$-fold output-run overhead.
+ *Coefficient-function learning.*  For genuinely context-dependent targets $F(z,x)$, the theorem
  identifies heavy character *correlations* but does not learn the coefficient functions
  $z mapsto hat(F)_z(alpha)$.  A $Z$-conditioned student is an additional learning problem.
+ *Shared-memory versus list hypotheses.*  Whether a teacher's measured degree profile is better explained
  by a small number of shared correlation heads than by a growing list of position-specific characters is
  now a well-posed comparison (@sec:fourier-correlation-memory).
+ *Margin conditions.*  A teacher-margin condition under which small KL implies top-token agreement would
  close the last gap between probabilistic distillation and hard-label recovery.

#h1([Acknowledgments])

*AI usage:* the author would like to acknowledge the use of language models (Gemini and Claude) in drafting
and revising this document; all theorem statements remain the author's responsibility and the categorical
identities are accompanied by executable verification tests.
