#import "../format.typ": *
#import "../math.typ": *
#import "./UC.typ": *

#let Rel = $calR$
#let NIZKArg = $sans("NIZK-Arg")$
#let ZK = $sans("ZK")$
#let ZKPoK = $sans("ZKPoK")$
#let NIZK = $sans("NIZK")$
#let accept = $mono("accept")$
#let reject = $mono("reject")$
#let prove = $bold("prove")$
#let verify = $bold("verify")$
#let verification = $bold("verification")$
#let witness = $bold("witness")$
#let proofTxt = $bold("proof")$

#let UC-NIZK-Arg(citation) = [
  #figure(
    kind: "ideal",
    supplement: [Ideal Functionality],
    fbox[
      // https://dl.acm.org/doi/pdf/10.1145/2220357.2220358?casa_token=XYrP0sGDY-sAAAAA:bagPvzoelujdyMPr4vOtS4Q_fTvB5Pc0D_lfa_g2PsQu3dhY6wYUrbmYiq1cQX_-mzsbSK9Fpw Page 21 ideal func
      #set text(top-edge: 0.8em, bottom-edge: -0.1em)
      #align(center)[*Functionality* $calF_NIZKArg$]
      #align(left)[
        Parameterized with relation $Rel$ and running parties $P_1, dots, P_n$ and adversary $calS$:

        *Proving Phase*: Upon receiving $(prove, sid, x, w)$ from $P$ ignore if $(x, w) in.not Rel$.
        Send $(prove, x)$ to $calS$ and wait for answer $(proofTxt, pi)$.
        Upon receiving the answer store $(x, pi)$ and send $(proofTxt, sid, pi)$ to $P$.

        *Verifying Phase*: Upon receiving $(verify, sid, x, pi)$ from party $V$ check whether $(x, pi)$ is stored.
        If not, send $(verify, x)$ to $calS$ and wait for answer $(witness, w)$.
        Upon receiving the answer, check whether $(x, w) in Rel$ and if yes, store $(x, pi)$.
        If $(x, pi)$ has been stored  return $(verification, sid, accept)$ to $V$ else return $(verification, sid, reject)$.
      ]
    ],
    caption: [
      NIZK argument ideal functionality $calF_NIZKArg$ #cite(citation) // TODO: would be nice to have a better citation system rather than just passing it in
    ],
  )<UC-Ideal-NIZK-Arg>]

