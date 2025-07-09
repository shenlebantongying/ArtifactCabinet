#set document(title: "Français")
#set text(lang: "fr")
#set smartquote(enabled: false)

#set heading(numbering: (..numbers) => { h(-4em) + numbers.pos().map(_ => h(1em)).join() })

#let is_ = sym.arrow.r
#let eg_(eg, ..trans) = {
  text([#eg], blue)
  trans.pos().map(w => [\ #is_ #w ]).join()
  linebreak()
}


#let term_(term, ..trans) = {
  text(weight: "bold")[#term ]
  trans.pos().map(w => [: #w ]).join()
  linebreak()
}

#let hr_ = line(length: 50%, stroke: 0.5pt + gray)

= ... avoir besoin de + infinitive form word ...

need to do something

- #eg_[J'ai besoin d'un verre.][I need a drink.]
- #eg_[J'ai besoin d'aller à la poste.][I need to go to the post office.]
- #eg_[J'ai besoin de manger.]

= Passé composé

== General

... + avoir in present tense + past participle of the main verb + ...

- #eg_[J'ai fait mes valises hier soir.][I packed my bags last night.]

The endings of past participle should match with the person or thing completing the action.

- #eg_[*Elle* est *allée* à la plage.][She went to a beach.]
- #eg_[*Elles* sont *allées* à la plage.][They went to a beach.]
- #eg_[J'ai grandi pas mal dans le milieu international][I grown up a lot in an international environment.]

#term_[pas mal][lot less (a lot)]

== M.VANDERTRAMPS

Some verbs combine with *être* instead of *avoir*.

== Negative

Subject + n' + avoir + pas + past participle of the main verb + ...

- #eg_[Vous n'avez visité aucun musée?][Didn't you not visited any museum?]

== Question

Avoir + a hyphen + subject + past participle ...

- #eg_[As-tu mangé avec ton frère pendant le week-end?][Did you eat with your brother during the weekend?]
- As-tu #is_ swap subject & verb for questioning.

== Reflexive pronoun

Subject + reflexive pronoun + *être* in present tense + past participle of the main verb + ...

- #eg_[Je me suis douché.][I has a shower.]

Subject + n'/ne + être in present tense + pas + past participle of the main verb + ...

- #eg_[Nous ne sommes pas allés travailler ce matin.][We didn't go to work this morning.]

If the reflexive pronoun is the *direct object* then the past participle agrees gender and number with it. If reflexive pronoun is the *indirect object*, then there is no agreement.

- #eg_[Elle s'est lavée.][She washed herself. Note that the `se` refers herself, aka the direct object of the action.]
- #eg_[Elle s'est lavé les cheveux][She washed her hair. Note that `se` refers the les cheveux.]


#link("https://www.ou.edu/class/FRINFO/gram/2/3/1.html")

= Random Reviews

== July 1

#eg_[Quelle heure est-il?][What time is it?]

#eg_[Je vais prendre un café / une bière.][I will go take a cup of coffee / a beer.]

#eg_[jus d'orange][orange juice]

#eg_[Je travaille avec un kinésiologue.]

#eg_[Une fille parle sa saison préférée.][A girl talks abot her favorite season.]

#eg_[Il est ici/là-bas.]
- ici #is_ here
- là-bas #is_ over there

#eg_[Il faut qu'on parle][We need to talk. #link("https://fr.wikipedia.org/wiki/Il_faut_qu%27on_parle")]

#eg_[Elle et son amie veulent parler avec le garçon.][She and her friend want to talk with the boy.]

#eg_[Vous voulez quoi?] est une _autre façon_ de dire #eg_[Qu'est-ce que vous voulez?]

#hr_

- à #is_ before city names (à Montréal)
- en #is_ feminine countries (en Russie)
- au #is_ masculine countries (au Canada)
- aux #is_ plural countries (aux États-Unis)

#hr_

Comparatif de supériorité/d'infériorité/d'égalité

plus/moins/aussi + adj. + que

#hr_

Regular conjugaisons du participe passé

- -er #is_ -é
- -ir #is_ -i
- -re #is_ -u

#hr_

== July 8

#eg_[Va dans le salon, j'ai des choses à finir dans la cuisine.][
  Go to the living rootm, I have things to finish in the kitchen.
]

#eg_[Je m'excuse.][Sorry!]

#term_[écrivain écrivaine][writer]
#term_[danseur danseuse][dancer]

#term_[réalisateur réalisatrice][director]

#eg_[Malika fait des films. Ell est _réalisatrice_.]

#term_[pouvoir][to be able to]
#eg_[Il ne _peut_ pas parler][He cannot speak.]

#term_[qqch. #is_ quelque chose][something]
#eg_[Mets _quelque chose_ de chaud.][Wear something warm.]

#eg_[Tu as passé un bon week-end?][
  How was your weekend?]

#eg_[On va se voir au même café _le mois prochain_.][
  We're going to meet at the same coffee shop next month.
]
#term_[prochain][next]
