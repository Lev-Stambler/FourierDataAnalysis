#import "@preview/arkheion:0.1.0": arkheion, arkheion-appendices
#show: arkheion-appendices
#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#set math.equation(numbering: none)

#show: thmrules
#show: eqrules

= Verification Contract

The categorical implementation is checked first on enumerable autoregressive models.
The required tests cover: $ZZ_q$ orthogonality for composite $q$; exact agreement between KV continuation
and the enumerated conditional law; the weighted level-mass identity; unbiasedness of the paired-suffix
estimator; equality of the all-child DFT with $q$ separately computed child buckets; recovery of a planted
degree-$n$ character; and invariance of the centered one-hot simplex kernel under vocabulary permutations.
The existing identity script #raw("verification/verify_identities.py") remains a regression suite for the
general dataset Fourier identities in the preliminaries; the autoregressive categorical tests live with the
new experiment code.

= Notes

#CLAUDE[
  *Frame-theory reading (removed from the main text; kept here to revisit).*
  In frame-theory language: the mass identity (@lem:mass) says the restricted characters ${phi_alpha|_calD}$ form a _tight frame_ for $L^2 (calD)$ with frame bound $C_calD = |calT|^n \/ |calD|$; dividing each frame vector by $sqrt(C_calD)$ — i.e., passing to the normalized characters of @defn:normalized-coeffs — yields a _Parseval frame_, for which every identity holds over the dataset with no constants at all.
  The normalized coefficients are the frame (analysis) coefficients, and the scaled reconstruction in @thm:learning-low-degree is the synthesis operator truncated to low degree.
  Aliasing is the frame's overcompleteness made visible: $|calT|^n$ frame vectors in a $|calD|$-dimensional space.
  Standard reference: Christensen, _An Introduction to Frames and Riesz Bases_.
]
