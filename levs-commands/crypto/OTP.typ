#import "../format.typ": *
#import "../math.typ": *
#import "./UC.typ": *

#let OTP = $sans("OTP")$
#let OTM = $sans("OTM")$
#let OTPComp = $sans("OTP")^*$
#let VOTP = $"Ver-"sans("OTP")$
#let otp = $sans("otp")$
#let votp = $sans("v-otp")$
#let crs = $mono("crs")$
#let create = $bold("create")$
#let execute = $bold("execute")$
#let verify = $bold("verify")$
#let proofTxt = $bold("proof")$
#let prove = $bold("prove")$
#let Rel = $calR$
#let accepted = $mono("accepted")$
#let witness = $bold("witness")$
#let verification = $bold("verification")$
#let accept = $mono("accept")$
#let reject = $mono("reject")$


#let UC-OTP(citation) = [
  #figure(
    kind: "ideal",
    supplement: [Ideal Functionality],
    fbox[
      #align(center)[*Functionality* $calF_OTP$]
      #align(left)[
        Running parties $P_1, dots, P_n$ and adversary $calS$:

        *Create*: Upon receiving $(create, sid, s_otp)$ from the sender, send $create$ to the receiver and store $s_otp$

        *Execute*: Upon receiving $(execute, sid, x in {0, 1}^b)$ from the receiver, if $s_otp$ is stored, compute $y = f(s_otp, x)$ and delete $s_otp$.
      ]
    ],
    caption: [
      OTP functionality $calF_OTP$ for poly-time $f : {0, 1}^a times {0, 1}^b -> {0, 1}^c$ #cite(citation) // TODO: would be nice to have a better citation system rather than just passing it in
    ],
  )<UC-Ideal-OTP>
]

#let UC-OTPComp() = [
  #figure(
    kind: "ideal",
    supplement: [Ideal Functionality],
    fbox[
      #align(center)[*Functionality* $calF_OTPComp$]
      #align(left)[
        Parameterized by non-interactive ideal functionalities $calF_1, dots, calF_m$ and running parties $P_1, dots, P_n$ and adversary $calS$:

        *Create*: Upon receiving $(create, sid, s_otp)$ from the sender, send $create$ to the receiver and store $s_otp$

        *Execute*: Upon receiving $(execute, sid, x in {0, 1}^b)$ from the receiver, if $s_otp$ is stored, compute $y = f^(calF_1, dots, calF_m)(s_otp, x)$ and delete $s_otp$.
      ]
    ],
    caption: [
      OTP functionality $calF_OTPComp$ for program $f^(calF_1, dots, calF_m) : {0, 1}^a times {0, 1}^b -> {0, 1}^c$ and non-interactive ideal functionalities $calF_1, dots, calF_m$
    ],
  )<UC-Ideal-OTPComp>
]



#let VerOTP = $"Ver-"sans("OTP")$
#let UC-VerOTP(is-UC: true, citation: 0) = [
  #let sidMod = if is-UC { $sid,$ } else {}
  #figure(
    kind: "ideal",
    supplement: [Ideal Functionality],
    fbox[
      // https://dl.acm.org/doi/pdf/10.1145/2220357.2220358?casa_token=XYrP0sGDY-sAAAAA:bagPvzoelujdyMPr4vOtS4Q_fTvB5Pc0D_lfa_g2PsQu3dhY6wYUrbmYiq1cQX_-mzsbSK9Fpw Page 21 ideal func
      #align(center)[*Functionality* $calF_VerOTP$]
      #align(left)[
        Parameterized with relation $Rel$ Running parties $P_1, dots, P_n$ and adversary $calS$.
        For security purposes, we also generate a re-usable common reference string $crs$ and a trapdoor $tau$ such that $crs$ is a commitment to a random value and $tau$ is the opening of the commitment.
        We write $z$ for the public input to the relation and $s_otp$ for the secret input to the relation, which is also the secret input to the program $f$.

        *Create*: Upon receiving $(create, sidMod s_otp, z, crs)$ from the sender $P$:
        - Ignore if $(z, s_otp) in.not Rel$
        - Send $(prove, z)$ to $calS$ and wait for answer $(proofTxt, s^*, z^*)$
        - If $s^* != bot$, set $s_otp = s^*$. Set $z = z^*$  // allows simulator to change public input if needed
        - If $(z, s_otp) in.not Rel$ ignore
        - Otherwise, send $(create, votp)$ to the sender, store $(z, s_otp)$

        *Verifying Phase*: Upon receiving $(verify, votp, sidMod$z$, crs)$ from party $V$:
        - check whether $(z, s_otp)$ is stored
        //If not, send $(verify, z)$ to $calS$ and wait for answer $(witness, w)$. Upon receiving the answer, check whether $(z, w) in Rel$ and if yes, store $(z, pi)$. TODO: we do not use this as we have an assumed "injectivity" with the secret
        - If $(z, s_otp)$ has been stored return $(verification, otp, sidMod accept)$ to $V$ and store $accepted$
        - Otherwise return $(verification, sidMod reject)$.

        *Execute*: Upon receiving $(execute, otp, sidMod x in {0, 1}^b)$ from the receiver, if $s_otp$ is stored and $accepted$ is stored, compute $y = f(s_otp, x)$ and delete $s_otp$ and $accepted$.
      ]
    ],
    caption: [
      Verifiable OTP ideal functionality $calF_VerOTP$ for program $f : {0, 1}^a times {0, 1}^b -> {0, 1}^c$ #if citation != 0 { [#cite(citation)] } // TODO: would be nice to have a better citation system rather than just passing it in
    ],
  )
  #label(if is-UC { "UC-Ideal-VerOTP" } else { "Sim-Ideal-VerOTP" })
]


