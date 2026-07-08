#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

= Conclusion and Future Directions

Taking expectations over a dataset while keeping the characters of the ambient product space yields a usable, sample-estimable Fourier calculus — provided one respects the density constant $C_cal(D)$ and the aliasing it encodes.
The dictionary (lift, inversion, mass identity, closeness) is elementary; the payoff is that classical spectral algorithms can be transplanted honestly: low-degree learning survives with a scaled reconstruction, and Goldreich-Levin survives exactly when the dataset's bias spectrum is dominated by few heavy terms — with a matching impossibility (blindness) for sample-only access.

Beyond the open problems listed at the end of the Goldreich-Levin section, some directions we find promising:

#TODO[
  - _Degree monotonicity._ Is there a monotone relationship between the degree (spectral concentration) of $f$ over the dataset and over the full space?
    A clean statement here would let one read off, from dataset quantities alone, whether a given model class can learn $f$ — connect to distribution-agnostic learning theory.
  - _The dataset as its own function._ What happens when $f = cal(D)$ (the indicator itself)?
    The bias spectrum is then the whole story; relate its decay to standard notions of dataset complexity.
  - _PAC learning and entropy._ Relate normalized weight profiles $overline(W)^k_cal(D)$ to PAC-learnability on-distribution, and to entropy-type quantities (an analogue of Hartley entropy for datasets).
]

#h1([Acknowledgments])

*AI usage:* the author would like to acknowledge the use of language models (Gemini and Claude) in drafting and revising this document; all stated identities and theorem constants were additionally verified numerically on random and structured instances.
