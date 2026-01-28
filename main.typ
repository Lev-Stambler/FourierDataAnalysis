#import "@preview/arkheion:0.1.0": arkheion, arkheion-appendices
#import "levs-commands/main_commands.typ": *
#import "custom_commands.typ": *
#import "@preview/cheq:0.2.2": checklist
#show: checklist

#show: thmrules
#show: eqrules
#set math.equation(numbering: none)


#show: arkheion.with(
  title: "My Title",
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
      #lorem(30)
    ]
  ],
  //keywords: ("First keyword", "Second keyword", "etc."),
  date: "September 10, 2025",
)

// #include "sections/intro.typ"
#include "sections/idea.typ"
#include "sections/ai.typ"
#include "sections/conc.typ"

#bibliography(("levs-bibs/crypto.bib", "levs-bibs/ai-complexity.bib",), style: "association-for-computing-machinery")

#include "sections/appendix.typ"
