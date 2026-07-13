#import "../levs-commands/main_commands.typ": *
#import "../custom_commands.typ": *
#show: thmrules
#show: eqrules
#set math.equation(numbering: none)

= Dataset Goldreich--Levin with a Generative Oracle

We study the setting needed for model compression.
Draw a real context $Z$ from a corpus distribution and use an autoregressive model $f_theta$ to generate
a continuation $X$.
The model plays two roles:

+ its sampling probabilities generate continuations $X$;
+ a fresh context-lens-$128$ forward on $X$ alone labels those continuations by
  $f(X)=P_theta(dot|X)$.

This defines the dataset distribution rather than merely approximating one.
Conditioned on $Z=z$, if the sampling kernel is $Q_theta$, then
$
calD_z^n(x)
= product_(j=1)^n Q_theta(x_j | z,x_(<j)).
$
Every generated continuation is therefore in-distribution for $calD_z^n$.
We do not need a flat corpus to contain two strings with the same prefix, and we do not retrieve or search
for such strings.
Given a realized real-and-generated left context, we compute its KV cache once and generate independent suffixes as the
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

For the experiment, $F(z,x)=f(x)=P_theta(dot|x)$ is a probability vector and
@thm:random-context-vector-gl performs one shared search over the whole next-token distribution.
Generation may use a restricted or temperature-scaled $Q_theta$ while the label retains the raw projected
probabilities.
The real context $z$ changes the rollout measure only; it is absent from both the teacher label forward and
the pure Fourier student's input.
