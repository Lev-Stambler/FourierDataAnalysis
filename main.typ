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
      email: "levstamb@umd.edu",
      affiliation: [University of Maryland, College Park, \ NeverLocal Ltd.],
      orcid: "0000-0002-0952-8528",
    ),
  ),
  abstract: [
    #align(left)[
      We develop elementary Fourier analysis _over a finite dataset_ $cal(D) subset cal(T)^n$: coefficients are taken with respect to the empirical measure on $cal(D)$ while the characters remain those of the ambient product space, so that every quantity is estimable from samples.
      Parseval fails over a dataset — by exactly the density factor $C_cal(D) = |cal(T)|^n \/ |cal(D)|$ — and we give the correct replacements: a mass identity, a closeness identity, and a low-degree learning theorem in which the normalization constants cancel through a scaled reconstruction.
      As the highlight, we adapt the Goldreich-Levin algorithm to find all characters heavily correlated with a function _over the dataset_.
      Sample access alone is provably blind: over a collision-free dataset, every bucket weight in the classical tree search is identical.
      Query access to a model together with the dataset's heavy bias spectrum suffices, via a convolution identity expressing the on-dataset spectrum as the global spectrum convolved with the spectrum of the dataset itself.
      Corollaries include a Kushilevitz-Mansour-style guarantee for decision trees over datasets.
    ]
  ],
  //keywords: ("First keyword", "Second keyword", "etc."),
  date: "July 8, 2026",
)

// #include "sections/intro.typ"
#include "sections/prelims.typ"
#include "sections/gl.typ"
#include "sections/conc.typ"

#bibliography(("levs-bibs/crypto.bib", "levs-bibs/ai-complexity.bib",), style: "association-for-computing-machinery")

#include "sections/appendix.typ"
