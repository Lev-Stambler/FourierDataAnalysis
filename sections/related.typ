#import "../levs-commands/main_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

= Related Work <sec:related>

*Heavy Fourier coefficients.*
Goldreich--Levin recovers every heavy Boolean character from queries @goldreich1989hardcore, and
Akavia, Goldwasser, and Safra extend significant-coefficient recovery to arbitrary finite abelian groups
@akavia2003proving.
Modern $q$-SFT searches sparse, potentially high-degree spectra directly on $ZZ_q^n$ rather than first
encoding the alphabet as Boolean variables @erginbas2023efficiently.
These algorithms use the uniform group law and chosen function evaluations.
Our measure is instead the non-uniform autoregressive rollout law, and our oracle returns samples and
prefix-conditional samples from that law.

*Conditional access.*
Conditional sampling is known to be strictly stronger than ordinary samples in distribution testing and
learning @canonne2015testing @chakraborty2013power @chen2021junta.
In our application it is not an abstract data-service assumption: after a random real corpus context and a
realized generated left prefix, the same
autoregressive model that defines the distribution generates an exact conditional suffix.
KV caching changes only the cost of this operation, not its law.

*Categorical bases.*
The cyclic characters of $ZZ_q$ are orthonormal, multiplicative, and unit modulus, which is what makes the
Dataset-GL completeness proof and the simultaneous all-child DFT work.
We use the tokenizer's native, contiguous ids directly as elements of $ZZ_q$: there is no binary expansion
and no alternate categorical representation in the theorem or Fourier student.

*Fourier auditing of models.*
The Active Fourier Auditor estimates distributional properties of a queried model @ajarra2024active, and
recent explanation methods recover sparse spectra of trained predictors @gorji2025amortized @kang2025spex.
Our corpus context $Z$ seeds an in-distribution continuation $X|Z$, but the analyzed function is the model's
complete next-token vector from a separate forward on $X$ alone, $f(X)=P_theta(dot|X)$.  This keeps the
sampling oracle autoregressive while making the Fourier teacher and student functions of the same
tokenizer-native input $X$.
