#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

= Dataset Goldreich--Levin with a Generative Oracle

We study the setting needed for model compression.
Fix a real prefix $pi$ and an autoregressive model $f_theta$.
The model plays two roles:

+ its sampling probabilities generate continuations $X$;
+ its raw next-token probabilities label those same continuations.

This defines the dataset distribution rather than merely approximating one.
If the sampling kernel is $Q_theta$, then
$
calD_(pi,theta)^n(x)
= product_(j=1)^n Q_theta(x_j | pi,x_(<j)).
$
Every generated continuation is therefore in-distribution for $calD_(pi,theta)^n$.
We do not need a flat corpus to contain two strings with the same prefix, and we do not retrieve or search
for such strings.
Given a realized prefix $z$, we compute its KV cache once and generate as many independent suffixes as the
conditional estimator needs.
Autoregressive factorization makes these exact draws from
$calD_(pi,theta)^n | X_(1:|z|)=z$.

The Fourier domain is native categorical $ZZ_q^n$.
One token position is one variable, irrespective of $q$; no token is compressed into Boolean coordinates.
The search is allowed to recover coefficients of every token-degree through $n=128$.

The rest of this section defines the categorical basis, proves that KV continuation implements the required
conditional oracle, extends the bucket identities to the non-uniform rollout law, and proves the resulting
high-degree Dataset-GL theorem.

#include "ar_categorical_gl.typ"

== Interpretation

The theorem's resource is not accidental prefix repetition in stored text.
It is the ability of the same generative model that defines $calD_(pi,theta)^n$ to resample a suffix after
any realized left prefix.
The generated dataset may be highly non-product and may occupy a tiny portion of $ZZ_q^n$; both are covered
by the weighted bucket identity.

The remaining instance-dependent quantity is the live spectral width $N$.
Generation solves conditional access, not spectral density: if exponentially many buckets are genuinely
heavy, every correct all-heavy-coefficient algorithm must pay for them.
The theorem therefore reports its complexity in $N$ and makes no low-degree assumption.

For the experiment, $F(x)=P_theta(dot|pi,x)$ is a probability vector and
@cor:ar-qary-vector performs one shared search over the whole next-token distribution.
Generation may use a restricted or temperature-scaled $Q_theta$ while the label retains the raw projected
probabilities.
This separation changes the rollout measure but never evaluates the label off that measure.
