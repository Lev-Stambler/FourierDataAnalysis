#import "@preview/arkheion:0.1.0": arkheion, arkheion-appendices
#import "levs-commands/main_commands.typ": *
#import "custom_commands.typ": *
#import "@preview/cheq:0.2.2": checklist
#show: checklist

#show: thmrules
#show: eqrules
#set math.equation(numbering: none)


#show: arkheion.with(
  title: "Characterizing Datasets: Hardness, Learning, and More",
  authors: (
    (
      name: "Lev Stambler",
      email: "lev@tearlabs.ai",
      affiliation: [Tear Labs Corp. \ University of Maryland, College Park],
    ),
  ),
  abstract: [
    #align(left)[
      We develop elementary Fourier analysis _over a finite dataset_ $cal(D) subset cal(T)^n$: coefficients are taken against the empirical measure on $cal(D)$, while the characters stay those of the ambient product space, so every quantity is estimable from samples.
      Parseval fails over a dataset, off by exactly the density factor $C_cal(D) = |cal(T)|^n \/ |cal(D)|$; the replacements are a mass identity, a closeness identity, and a low-degree learning theorem whose normalization constants cancel through a scaled reconstruction.
      We then adapt the Goldreich-Levin algorithm to find every character heavily correlated with a function _over the dataset_.
      Sample access alone is blind: over a collision-free dataset, every bucket weight in the classical tree search is identical.
      For autoregressive text, a reverse-time $ZZ_q^n$ search turns prefix-conditional sampling into exact KV-cached continuation, yielding a native categorical, arbitrary-degree Dataset-GL theorem for the non-uniform fixed-prefix rollout law without encoding tokens into bits.
      Query access to a model, together with the dataset's heavy bias spectrum, suffices — via a convolution identity writing the on-dataset spectrum as the global spectrum convolved with the dataset's own spectrum.
      Corollaries include a Kushilevitz-Mansour guarantee for decision trees over datasets.
    ]
  ],
  //keywords: ("First keyword", "Second keyword", "etc."),
  date: "July 8, 2026",
)

// #include "sections/intro.typ"
#include "sections/prelims.typ"
#include "sections/gl.typ"
#include "sections/related.typ"
#include "sections/conc.typ"

#bibliography(("levs-bibs/crypto.bib", "levs-bibs/ai-complexity.bib",), style: "association-for-computing-machinery")

#include "sections/appendix.typ"
