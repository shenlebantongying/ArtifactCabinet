#set text(lang: "fr")
#set page(height: auto)
#set smartquote(enabled: false)

#let t(t) = {
  text([#t], blue)
}

// examples
#let e(eg, ..trans) = {
  text([#eg], blue)
  linebreak()
  trans.pos().join("n")
}

// terms
#let t(term, ..trans) = {
  text(weight: "bold")[#term ]
  [: #trans.pos().join("|")]
}

= Books

Développement d'applications avec OCaml

+ #link("https://caml.inria.fr/pub/docs/oreilly-book/html/index.html")
+ #link("https://www-apr.lip6.fr/~chaillou/Public/DA-OCAML/")

Apprendre à programmer avec OCaml

+ #link("https://usr.lmf.cnrs.fr/programmer-avec-ocaml/")
+ #link("https://usr.lmf.cnrs.fr/lpo/")

= Terms & Phrases

== 1
#t[Un langage de programmation]
#t[Programmation fonctionnelle]
#t[logiciel][software]
#t[téléchargement][download]
#t[calcul][computation]
#t[déclaration de valeur][variable declaration]
#t[caractères et chaînes][characters and strings]
#t[structure de contrôle conditionnelle][structure of conditional control]

= OCaml
