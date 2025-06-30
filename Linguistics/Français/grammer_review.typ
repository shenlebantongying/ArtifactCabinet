#set document(title: "Français")
#set text(lang: "fr")
#set page(height: auto)
#set smartquote(enabled: false)

#set heading(numbering: (..numbers) => { h(-4em) + numbers.pos().map(_ => h(1em)).join() })

#let _is = sym.arrow.r
#let _eg(eg, trans) = {
  text([#eg], blue)
  linebreak()
  [#trans]
}

#let _term(term, trans) = {
  text(weight: "bold")[#term ]
  [: #trans]
}

= ... avoir besoin de + infinitive form word ...

need to do something

- #_eg[J'ai besoin d'un verre.][I need a drink.]
- #_eg[J'ai besoin d'aller à la poste.][I need to go to the post office.]
- #_eg[J'ai besoin de manger.][]

= Passé composé

== General

... + avoir in present tense + past participle of the main verb + ...

- #_eg[J'ai fait mes valises hier soir.][I packed my bags last night.]

The endings of past participle should match with the person or thing completing the action.

- #_eg[*Elle* est *allée* à la plage.][She went to a beach.]
- #_eg[*Elles* sont *allées* à la plage.][They went to a beach.]
- #_eg[J'ai grandi pas mal dans le milieu international][I grown up a lot in an international environment.]

#_term[pas mal][lot less (a lot)]

== M.VANDERTRAMPS

Some verbs combine with *être* instead of *avoir*.

== Negative

Subject + n' + avoir + pas + past participle of the main verb + ...

- #_eg[Vous n'avez visité aucun musée?][Didn't you not visited any museum?]

== Question

Avoir + a hyphen + subject + past participle ...

- #_eg[As-tu mangé avec ton frère pendant le week-end?][Did you eat with your brother during the weekend?]

== Reflexive pronoun

Subject + reflexive pronoun + *être* in present tense + past participle of the main verb + ...

- #_eg[Je me suis douché.][I has a shower.]

Subject + n'/ne + être in present tense + pas + past participle of the main verb + ...

- #_eg[Nous ne sommes pas allés travailler ce matin.][We didn't go to work this morning.]

If the reflexive pronoun is the *direct object* then the past participle agrees gender and number with it. If reflexive pronoun is the *indirect object*, then there is no agreement.

- #_eg[Elle s'est lavée.][She washed herself. Note that the `se` refers herself, aka the direct object of the action.]
- #_eg[Elle s'est lavé les cheveux][She washed her hair. Note that `se` refers the les cheveux.]


#link("https://www.ou.edu/class/FRINFO/gram/2/3/1.html")
