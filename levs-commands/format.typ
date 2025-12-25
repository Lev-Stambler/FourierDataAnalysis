#let fbox = (x, width: 100%) => box(
  stroke: (paint: black, thickness: 1pt),
  inset: (x: 16pt, y: 12pt),
  radius: 2pt,
  width: width,
)[
  #x
]

#let appendix(body) = {
  set heading(numbering: "A.1", supplement: [Appendix])
  counter(heading).update(0)
  body
}
