#set text(
  lang: "fr",
  hyphenate: false
)

#set page(margin: 2em)
#set smartquote(enabled: false)
#show link: set text(fill: rgb("#003DA5"))
#show emph: it => {
  text(it.body,fill: rgb("#4C7739"))
}

#let Tr(c) = {
  linebreak()
  text(c,fill:gray)
}

#let __ = line(length: 5%, stroke: 0.5pt + gray)
#let est = sym.arrow.r
#let o(tt) = {
  block(tt,
    width:100%,
    stroke: (
      paint: gray,
      thickness: 0.5pt,
      dash: "dashed"),
    radius: 0.3em,
    outset:1em,
    spacing:2.5em,
    breakable:false)
}
#let Conj(t) = link("https://leconjugueur.lefigaro.fr/php5/index.php?verbe="+t.text)[#t]
#let DictCollins(t) = text(link("https://www.collinsdictionary.com/dictionary/french-english/"+t.text)[#t])
#let DictLeRobert(t) = text(link("https://dictionnaire.lerobert.com/definition/"+t.text)[#t])
#o[
  https://vitrinelinguistique.oqlf.gouv.qc.ca/22926/le-vocabulaire/nuances-semantiques/difference-entre-information-et-renseignement

  Vous permettez que je vous demande un _renseignement_?
  #Tr[Do you permit me to ask for some information?]
]

#o[
  Conditionnel Présent

  Si nous avions des pommes, je _les_ mangerais.
  #Tr[If we had some apples, I would eat them.]

  avions #est avoir, imparfait

  mangerais #est  manger, conditionnel

  #__
  Est-ce que tu _serais_ intéressé?
  #Tr[Would you be interested?]

  ser-ais #est être, conditionnel

  #__
  Si j'_étais_ toi, je _ferais_ me devoirs.

  imparfait, être  + conditional, faire #est fer + ais/ais/ait/ions/iez/aient

  #__
  Si elle pouvait, elle irait avec lui.
  #Tr[If she could, she would go with him.]

  conditional, aller #est ir #est irait

  #__
  Je sais qui saurait quoi faire!
  #Tr[I know who would know what to do!]

  savoir #est Saur #est saurait

  #__
  Si tu regardais, tu verrais.
  #Tr[If you looked, you would see.]

  voir #est verr #est verrais
]

https://dictionnaire.lerobert.com/guide/qu-est-ce-que-le-cod

- complément d'objet direct (COD)
- complément d’objet indirect (COI)
- complément d’objet second (COS)

#o[
  https://www.alloprof.qc.ca/fr/eleves/bv/francais/la-fonction-complement-direct-du-verbe-cd-f1253
  À l'automne, les randonneurs observent _les jolies couleurs de la nature_.
  #Tr[In autumn, the hikers observes the pretty colours of the nature.]

  #__
  Mon frère et moi aimons _qu'il y ait_ de nombreuses feuilles mortes sur le sol.
  #Tr[My father and I love to have dead leaves on the ground.]

  ?? qu'il ait #est #Conj[avoir], subjonctif

  y #est adverbial pronoun.
]

#o[
  - Tu vas à la bibliothèque aujourd'hui?
  - Oui, j'_y_ vais.

  The _y_ refers to the library previously mentioned.
]

What is ...
- Qu'est-ce que c'est qu ...
- C'est quoi ...

- Je veux, I want..., More blunt, le présent tense
- Je voudrais, I would like to have..., More polite, le conditionnel
- J'aimerais..., Even more polite

#__
Une fillette _belle_ comme une fleur

The _belle_ is usually placed before a noun.

#__
Paul est _étudiant_.

The occupation doesn't not have an article.

#o[
  https://vitrinelinguistique.oqlf.gouv.qc.ca/index.php?id=24654

  Réguliers
  - -er #est Le premier groupe
  - -ir #est Le deuxième groupe

  Irréguliers #est Le troisième groupe
]

#o[
  ... avoir besoin de + infinitive form word ...

  need to do something

  - J'ai besoin d'un verre.#Tr[I need a drink.]
  - J'ai besoin d'aller à la poste.#Tr[I need to go to the post ]
  - J'ai besoin de manger.
]

#__
Comment est-ce que l'on y va?
#Tr[How do we get there]

_l'on_ #est _on_ but fancy

#__
- D'accord #est Oki Doki
- On y va! #est Let's go!
- Je vous suis! #est I will follow you.


#o[
  https://www.uottawa.ca/notre-universite/administration-services/services-linguistiques/guide-de-redaction


  Les recommandations suivantes s'adressent aux personnes qui #Conj[doivent] rédiger, réviser ou traduire des textes à l' Université d'Ottawa.
  #Tr[The following recommandations is for personals who have to write, revise or translate texts for the University of Ottawa.]

  - #DictLeRobert[suivant]
  - Qui suit, qui vient immédiatement après. #Tr[The thing that follows, the thing that comes immediately after.]
  - venir, to come
]
