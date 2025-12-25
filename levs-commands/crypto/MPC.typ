#import "../format.typ": *
#import "../math.typ": *
#import "./UC.typ": *

#let accepted = $mono("accepted")$
#let witness = $bold("witness")$
#let verification = $bold("verification")$
#let accept = $mono("accept")$
#let register = $bold("register")$
#let reject = $mono("reject")$
#let send = $bold("send")$
#let compute = $bold("compute")$
#let MPoC = $sans("MPoC")$
#let idx = $mono("idx")$
#let partySet = $calP$

#let Sim-MPoC(citation: 0) = [
  #figure(
    kind: "ideal",
    supplement: [Ideal Functionality],
    fbox[
      #align(center)[*Functionality* $calF_MPoC$]
      #align(left)[
        Computing party $C$, adversary $calS$, and function $f : {0, 1}^(a times k) -> {0, 1}^c$.
        Keep indexed list of registered parties $partySet = emptyset$, parties which sent input, $calS = emptyset$, and input $X in {0, 1}^(a times k)$ initialized to $0^(a times k)$.

        *Register*: Upon receiving $(register, id)$ from a party, add $id$ to $partySet$ if not already present and $|partySet| < k$.

        *Send*: $(send, id, x)$ for $x in {0, 1}^a$ from party $id$: if $id in partySet$ and $|partySet| = k$, set $X[idx] = x_id$ where $idx$ is the index of party $id$ in $partySet$ and add $id$ to $calS$.

        *Compute*: $(compute)$ from party $C$: if $|partySet| = |calS| = k$, evaluate $y = f(X)$ and delete all entries of $X$ and $calS$
      ]
    ],
    caption: [
      Multi-party outsourced computation ideal functionality $calF_MPoC$ for program $f : {0, 1}^a times {0, 1}^b -> {0, 1}^c$ // #cite(citation)
    ],
  )<Sim-Ideal-MPoC>
]


