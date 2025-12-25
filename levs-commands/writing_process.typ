#import "./constants.typ" as Constants

#let h1 = x => heading(x, numbering: none, level: 1)
#let h2 = x => heading(x, numbering: none, level: 2)
#let h3 = x => heading(x, numbering: none, level: 3)

////////// Writing Process Commands //////////
#let TODO = x => text(red)[
  TODO: #x
]

// Change to your name!
#let lev = x => text(green)[
  LS: #x
]

#let LLMTODO = x => text(green)[
  LLM TODO: #x
]

#let LLMExplains = (x, name: "An LLM", title: "") => box(
  fill: rgb("#f0f0f0"),
  inset: 1em,
  stroke: 1pt + rgb("#000000"),
  radius: Constants.borderRadius,
  width: 100%,
)[
  *#name explains #title*:	#x
]

