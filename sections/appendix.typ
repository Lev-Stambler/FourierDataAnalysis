#import "@preview/arkheion:0.1.0": arkheion, arkheion-appendices
#show: arkheion-appendices
#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#set math.equation(numbering: none)

#show: thmrules
#show: eqrules

= Numerical Verification

Every identity, counterexample, and theorem constant in this note was verified numerically on random and structured instances, exact to $10^(-9)$: the mass and closeness identities, the normalized Parseval identities, the pair-form and blindness lemmas, the convolution identity and its heavy-bias tail bound, all five properties of the conditional bucket weight $Psi$ together with the unbiasedness of its estimator, the subcube and half-cube aliasing examples, the dataset sensitivity bounds, the corrected learning bound, the $eps_calD$-closeness reconstruction bound of the context-conditioning theorem (@thm:context-gl), and the candidate-generation constants of both Goldreich-Levin theorems.
The self-contained script (Python, standard library only) lives at #raw("verification/verify_identities.py") in the repository; run it with #raw("python3 verification/verify_identities.py").

#TODO[Deferred proofs and the general finite-abelian-group statements, as they accumulate.]

= Notes

#CLAUDE[
  *Frame-theory reading (removed from the main text; kept here to revisit).*
  In frame-theory language: the mass identity (@lem:mass) says the restricted characters ${phi_alpha|_calD}$ form a _tight frame_ for $L^2 (calD)$ with frame bound $C_calD = |calT|^n \/ |calD|$; dividing each frame vector by $sqrt(C_calD)$ — i.e., passing to the normalized characters of @defn:normalized-coeffs — yields a _Parseval frame_, for which every identity holds over the dataset with no constants at all.
  The normalized coefficients are the frame (analysis) coefficients, and the scaled reconstruction in @thm:learning-low-degree is the synthesis operator truncated to low degree.
  Aliasing is the frame's overcompleteness made visible: $|calT|^n$ frame vectors in a $|calD|$-dimensional space.
  Standard reference: Christensen, _An Introduction to Frames and Riesz Bases_.
]
