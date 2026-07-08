// Add custom commands to the command palette

// Claude-injected correction notes: clearly-attributed blocks explaining a
// problem found in the earlier draft and the fix. Old (incorrect) material is
// kept adjacent inside `#if false [ ... ]` for comparison.
#let CLAUDE = x => block(
  fill: rgb("#eef4fb"),
  stroke: (left: 2pt + rgb("#2b6cb0")),
  inset: 8pt,
  radius: 2pt,
  [#text(rgb("#2b6cb0"), weight: "bold")[CLAUDE:] #x],
)
