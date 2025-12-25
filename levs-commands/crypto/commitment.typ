#import "../format.typ": *
#import "../math.typ": *
#import "./UC.typ": *

#let Comm = $sans("Comm")$
#let commit = $bold("commit")$
#let open = $bold("open")$
#let receipt = $bold("receipt")$
#let cid = $bold("cid")$

#let UC-Comm(citation) = [
  #figure(
    kind: "ideal",
    supplement: [Ideal Functionality],
    fbox[
      // https://dl.acm.org/doi/pdf/10.1145/2220357.2220358?casa_token=XYrP0sGDY-sAAAAA:bagPvzoelujdyMPr4vOtS4Q_fTvB5Pc0D_lfa_g2PsQu3dhY6wYUrbmYiq1cQX_-mzsbSK9Fpw Page 21 ideal func
      #set text(top-edge: 0.8em, bottom-edge: -0.1em)
      #align(center)[*Functionality* $calF_Comm$]
      #align(left)[
        Running parties $P_1, dots, P_n$ and adversary $calS$ for message space $calM$:

        *Commit Phase*: Upon receiving $(commit, sid, cid, P_i, P_j, m in calM)$ from committer $P_i$, record $(cid, P_i, P_j, m)$ and send $(receipt, sid, cid, P_i, P_j)$ to $P_j$ and $calS$.
        Ignore any subsequent $(commit, sid, cid, P_i, P_j, m')$ from $P_i$ for any $m' in calM$.

        *Opening Phase*: Upon receiving $(open, sid, cid, P_i, P_j)$ from committer $P_i$ proceed as follows: if $(cid, P_i, P_j, m)$ is recorded, send $(open, sid, cid, P_i, P_j, m)$ to $P_j$ and $calS$.
        Otherwise, do nothing.
      ]
    ],
    caption: [
      Commitment ideal functionality $calF_Comm$ #cite(citation) // TODO: would be nice to have a better citation system rather than just passing it in
    ],
  )<UC-Comm>
]

