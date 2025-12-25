#import "../main_commands.typ": *

#show: thmrules
#show: eqrules
#let GC = $sans("GC")$

#let Sim-GC() = [
  #definition[Garbled Circuit, @yaoHowGenerateExchange1986 @bellareFoundationsGarbledCircuits2012][
    Let ${calC_n}_n$ be a family of circuits where each circuit in $calC_n$ has $n$ bit inputs.
    A garbling scheme, $GC$, for circuit family ${calC_n}_n$ consists of polynomial-time algorithms $(Garble, Eval)$:
    - $Garble(1^lambda, C in calC_n)$: The algorithm takes a security parameter $lambda$ and a circuit $C$ as input and outputs a garbled circuit $G$ together with $2n$ wire keys ${w_(i, b)}_(i in [n], b in {0, 1})$
    - $Eval(G, {w_i}_(i in [n]))$: The evaluation algorithm takes as input a garbled circuit $G$ and $n$ wire keys ${w_i}_(i in [n])$ and outputs $y in {0, 1}^m$.

  ]

  #definition[Garbling Correctness][
    A garbling scheme $GC$ for circuit family ${calC_n}_n$ is said to be correct if for all $lambda$, $n$, $x in {0, 1}^n$ and $C in calC_n$, $ Eval(G, {w_(i, x_i)}(i in [n])) = C(x), $ where $ (G, {w_(i,b)}_(i in [n], b in {0,1})) <- Garble(1^lambda, C). $
  ]

  // Security definition
  //#definition[Security][
  //  Informally, a garbling scheme is said to be secure if for every circuit $C$ and input $x$, the garbled circuit $G$ together with input wires ${w_(i,x_i)}_(i <= n)$ corresponding to some input $x$ reveals only the output of the circuit $C(x)$, and nothing else about the circuit $C$ or input $x$.
  //]
  //
  // Definition 3.6
  #definition[Selective Security][
    A garbling scheme $GC = (Garble, Eval)$ for a class of circuits $calC = {calC_n}_n$ is said to be a selectively secure garbling scheme if there exists a polynomial-time simulator $"Sim"$ such that for all $lambda$, $n$, $C in calC_n$ and $x in {0,1}^n$, the following holds:

    $
      { "Sim"(1^lambda, 1^n, 1^(|C|), C(x)) }
      compInd & { (G, {w_(i,x_i)}_(i in [n])) \
              & : (G, {w_(i,b)}_(i in [n], b in {0,1})) <- Garble(1^lambda, C) }
    $
  ]<def:GCSelectSec>
]
