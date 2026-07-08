#import "@preview/arkheion:0.1.0": arkheion, arkheion-appendices
#show: arkheion-appendices
#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#set math.equation(numbering: none)

#show: thmrules
#show: eqrules

= Numerical Verification

Every identity, counterexample, and theorem constant stated in this paper has been verified numerically on random and structured instances, exact to $10^(-9)$: the mass and closeness identities, the Parseval-frame identities, the pair-form and blindness lemmas, the convolution identity and its heavy-bias tail bound, all five properties of the conditional bucket weight $Psi$ together with the unbiasedness of its estimator, the subcube and half-cube aliasing examples, the dataset sensitivity bounds, the corrected learning bound, and the candidate-generation constants of both Goldreich-Levin theorems.
The self-contained script (Python, standard library only) lives at #raw("verification/verify_identities.py") in the repository; run it with #raw("python3 verification/verify_identities.py").

#TODO[Deferred proofs and the general finite-abelian-group statements, as they accumulate.]
