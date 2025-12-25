#import "@preview/ctheorems:1.1.3": *
#import emoji: checkmark

///////////////// Theorem Styles //////////////////
#show: thmrules
#let thmrulesCustom = thmrules.with(qed-symbol: [#checkmark.box])

#set page(width: 16cm, height: auto, margin: 1.5cm)
#set heading(numbering: "1.1.")

#let theorem = thmbox("theorem", "Theorem", fill: rgb("#eeeeee"), base_level: 1)
#let assumption = thmbox(
  "assumption",
  "Assumption",
  fill: rgb("#fffeee"),
  base_level: 1,
)
#let corollary = thmplain(
  "corollary",
  "Corollary",
  base_level: 1,
  base: "theorem",
  titlefmt: strong,
)

// Due to a bug in the ctheorems package, if a section is lacking an inital theorem statement and we want to use a lemma, we need to define a new theorem style as follows
#let lemmathm = thmplain(
  "theorem",
  "Lemma",
  base_level: 1,
  titlefmt: strong,
)

#let proposition = thmplain(
  "proposition",
  "Proposition",
  base_level: 1,
  //base: "theorem",
  titlefmt: strong,
)


#let lemma = thmplain(
  "lemma",
  "Lemma",
  base_level: 1,
  //base: "theorem",
  titlefmt: strong,
)

#let definition = thmplain(
  "definition",
  "Definition",
  base_level: 1,
  titlefmt: strong,
)
#let principle = thmplain(
  "principle",
  "Principle",
  base_level: 1,
  titlefmt: strong,
)
#let axiom = thmplain("axiom", "Axiom", base_level: 1, titlefmt: strong)
#let observation = thmbox(
  "observation",
  "Observation",
  inset: 1.5em,
  fill: rgb("eeeeee"),
  base_level: 1,
)

#let example = thmplain("example", "Example").with(numbering: none)
#let proof = thmproof("proof", "Proof")


#let eqNoNumb = f => math.equation(f, numbering: none, block: true, supplement: none)
#let eq = f => math.equation(f, numbering: "(1)", block: true, supplement: none)
#let eqrules(doc) = {
  show ref: it => {
    if it.element != none and it.element.func() == math.equation {
      let num = it.element.numbering
      let number = counter(math.equation).at(it.element.location()).first()
      if number != none {
        [eq. (#number)]
      } else {
        it
      }
    } else {
      it
    }
  }
  doc
}


///////////////// End Theorem Styles //////////////////

///////////////// Caligraphic Letters //////////////////
#let calA = $cal(A)$
#let calB = $cal(B)$
#let calC = $cal(C)$
#let calD = $cal(D)$
#let calE = $cal(E)$
#let calF = $cal(F)$
#let calG = $cal(G)$
#let calH = $cal(H)$
#let calI = $cal(I)$
#let calJ = $cal(J)$
#let calK = $cal(K)$
#let calL = $cal(L)$
#let calM = $cal(M)$
#let calN = $cal(N)$
#let calO = $cal(O)$
#let calP = $cal(P)$
#let calQ = $cal(Q)$
#let calR = $cal(R)$
#let calS = $cal(S)$
#let calT = $cal(T)$
#let calU = $cal(U)$
#let calV = $cal(V)$
#let calW = $cal(W)$
#let calX = $cal(X)$
#let calY = $cal(Y)$
#let calZ = $cal(Z)$

#let sfA = $sans(A)$
#let sfB = $sans(B)$
#let sfCount = $sans("Count")$
#let sfC = $sans(C)$
#let sfD = $sans(D)$
#let sfE = $sans(E)$
#let sfF = $sans(F)$
#let sfG = $sans(G)$
#let sfH = $sans(H)$
#let sfI = $sans(I)$
#let sfJ = $sans(J)$
#let sfK = $sans(K)$
#let sfL = $sans(L)$


///////////////// Symbols from Latex That I like //////////////////
#let circ = $" " circle.small" "$
#let otimes = $times.circle$
#let bigotimes = $times.circle.big$
#let oplus = $plus.circle$
#let bigoplus = $plus.circle.big$
#let subseteq = $subset.eq$

///////////////// Math Symbols //////////////////
#let eps = $epsilon.alt$
#let prod = $product$

#let ProbDist(x) = $bb(P)_#x$
#let Var = $"Var"$
#let ind = $"ind"$

////////////////// Math Functions //////////////////
#let Enc = $bold("Enc")$
#let Dec = $"Dec"$

/////////////// Probability ////////////////
#let Expec = $limits(bb(E))$

//////////// Algorithms ////////////
#let Comment(x) = text(rgb("#333333"))[\/\/ #x]
