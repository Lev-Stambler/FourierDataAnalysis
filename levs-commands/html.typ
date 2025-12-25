#import "./math.typ": *

#let in-no-rules = state("in-no-rules", false)

//#let is-html = state("is-html", true)
#let ishtml = false //true
#let is-html = state("is-html", ishtml)
#let htmlswitch(optHTML, optRegular) = if ishtml { optHTML } else { optRegular }

#if ishtml {
  let TODO = x => html.elem("span", attrs: (style: "color: red;"))[
    TODO: #x
  ]
}


#let gridH(columns: 1fr, ..content) = if ishtml {
  for it in content.pos() {
    //#html.elem("div", attrs: (class: "grid-item"))[ #it ]
    it
  }
} else {
  grid(content, columns: columns)
}


#let gridH(columns: 1fr, ..content) = if ishtml {
  html.elem("div", attrs: (
    class: "grid-container",
    style: "display: grid; grid-template-columns: "
      + repr(columns).replace(",", " ").replace("(", "").replace(")", "")
      + "; gap: 1em;",
  ))[
    #for it in content.pos() {
      html.elem("div", attrs: (class: "grid-item"))[ #it ]
      //it
    }
  ]
} else {
  grid(..content, columns: columns)
}

#let iframeExp = src => if ishtml [
  #html.elem("div", attrs: (
    class: "maximizable-iframe-container",
    style: "margin: 1em;",
  ))[

    //#html.elem("button", attrs: (
    //  class: "iframe-back-button",
    //  type: "button",
    //  value: "Back",
    //))[
    //  Back ↩
    //]

    #html.elem("iframe", attrs: (src: src))[
    ]

    #html.elem("button", attrs: (class: "iframe-toggle-button"))[
      ↗ Maximize
    ]
  ]
] else [
  NOT HTML DOCUMENT!
]



#let defineThmLikeH(name, display-name) = {
  let c = counter(name)

  return (title: none, body) => {
    let env_type = display-name
    c.step()


    // Use a context block to provide different output for HTML and other
    // formats (like PDF).
    context {
      if is-html.get() {
        let full-title = [
          *#env_type #c.display()*
          #if title != none {
            [ (#title) ]
          }
        ]
        // For HTML output, we construct the content by concatenating raw HTML
        // snippets with the body content. The `html` function is part of
        // Typst's native HTML support. The `body` content will be
        // automatically converted to HTML by the compiler.
        html.elem("div", attrs: (
          class: name,
          style: "display: block; padding: 1em;",
        ))[
          #html.elem("span", attrs: (class: "theorem-title"))[
            #full-title
            //#if title != none {
            //  html.elem("span", attrs: (class: "theorem-title-text"))[(#title)]
            //}
          ]:
          #html.elem("span", attrs: (class: "theorem-body"))[
            #body
          ]
        ]
      } else {
        if name == "axiom" {
          axiom[#title][#body]
        } else if name == "lemma" {
          if title == none {
            lemma[#body]
          } else {
            lemma[#title][#body]
          }
        } else if name == "definition" {
          definition[#title][#body]
        } else if name == "theorem" {
          theorem[#title][#body]
        } else if name == "principle" {
          principle[#title][#body]
        } else if name == "axiom" {
          axiom[#title][#body]
        } else if name == "lemma" {
          lemma[#title][#body]
        }
      }
    }
  }
}
}

// TODO: allow for some style!
#let thmH = defineThmLikeH("theorem", "Theorem")
#let axiomH = defineThmLikeH("axiom", "Axiom")
#let principleH = defineThmLikeH("principle", "Principle")
#let definitionH = defineThmLikeH("definition", "Definition")
#let lemmaH = defineThmLikeH("lemma", "Lemma")
#let theoremH = defineThmLikeH("theorem", "Theorem")

#let proofH(x) = if ishtml {
  html.elem("div", attrs: (
    class: "proof",
    style: "display: block; padding: 1em",
  ))[
    _Proof_: #x
  ]
} else {
  proof(x)
}


#let htmlrules(doc) = {
  let incanvas = state("in-cetz-canvas", false)
  let inpad = state("in-pad", false)

  show block: it => context {
    if in-no-rules.get() { it } else {
      html.elem("div", attrs: (style: "display: block;"))[
        #it
      ]
    }
  }

  show grid: it => context {
    if in-no-rules.get() { it } else {
      html.frame(it)
    }
  }


  show math.equation.where(block: false): it => context {
    if incanvas.get() or inpad.get() or in-no-rules.get() { it } else {
      box(html.frame(it))
    }
  }
  show math.equation.where(block: true): it => context {
    if in-no-rules.get() { it } else [
      #html.elem(
        "div",
        attrs: (
          style: "padding: 0.5em; display: grid; justify-content: center; zoom: 1.3;",
        ),
      )[
        #math.equation(it.body, block: false)
      ]
    ]
  }

  /////////////// Footnote style as tooltip ////////////
  show footnote: it => context {
    if in-no-rules.get() { it } else {
      html.elem("span", attrs: (class: "tooltip-container"))[
        #it
        #html.elem("span", attrs: (class: "tooltip-text"))[
          #it.body
        ]
      ]
    }
  }

  show footnote.entry: it => context {
    if in-no-rules.get() { it } else {
      html.elem("span", attrs: (class: "footnote", title: it.body))[
        #it
      ]
    }
  }

  doc
}

#let frameOriginal = (it, div-wrap-style: "") => context {
  if is-html.get() {
    in-no-rules.update(true)
    if div-wrap-style != "" {
      html.elem("div", attrs: (style: div-wrap-style))[
        #html.frame(it)
      ]
    } else {
      html.frame(it)
    }
    in-no-rules.update(false)
  } else {
    it
  }
}

