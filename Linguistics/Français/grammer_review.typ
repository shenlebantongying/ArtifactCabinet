#set document(title: "Français")
#set text(
  font:"New Computer Modern",
  lang: "fr")
#set smartquote(enabled: false)

#set heading(numbering: (..numbers) => { h(-4em) + numbers.pos().map(_ => h(1em)).join() })

#show link: set text(fill: luma(30%))
#show link: underline

#let is_ = sym.arrow.r

/* Sentence */
#let S(eg, ..trans) = {
  text([#eg],purple)
  trans.pos().map(w => [\ #is_ #text([#w],olive)]).join()
  linebreak()
}

/* Terms */
#let T(term, ..trans) = {
  text(weight: "bold")[#term ]
  trans.pos().map(w => [: #w ]).join()
  linebreak()
}

#let Dict(word) = {
  link("https://dictionnaire.lerobert.com/definition/"+word.text)[#word]
}

#let Conj(word) = {
  link("https://leconjugueur.lefigaro.fr/conjugaison/verbe/"+word.text)[#word]
}

#let hr_ = line(length: 50%, stroke: 0.5pt + gray)

#align(center, text(17pt)[
  *Langue Française*
])

= ... avoir besoin de + infinitive form word ...

need to do something

- #S[J'ai besoin d'un verre.][I need a drink.]
- #S[J'ai besoin d'aller à la poste.][I need to go to the post office.]
- #S[J'ai besoin de manger.]

= Passé composé

== General

... + avoir in present tense + past participle of the main verb + ...

- #S[J'ai fait mes valises hier soir.][I packed my bags last night.]

The endings of past participle should match with the person or thing completing the action.

- #S[*Elle* est *allée* à la plage.][She went to a beach.]
- #S[*Elles* sont *allées* à la plage.][They went to a beach.]
- #S[J'ai grandi pas mal dans le milieu international][I grown up a lot in an international environment.]

#T[pas mal][lot less (a lot)]

== M.VANDERTRAMPS

Some verbs combine with *être* instead of *avoir*.

== Negative

Subject + n' + avoir + pas + past participle of the main verb + ...

- #S[Vous n'avez visité aucun musée?][Didn't you not visited any museum?]

== Question

Avoir + a hyphen + subject + past participle ...

- #S[As-tu mangé avec ton frère pendant le week-end?][Did you eat with your brother during the weekend?]
- As-tu #is_ swap subject & verb for questioning.

== Reflexive pronoun

Subject + reflexive pronoun + *être* in present tense + past participle of the main verb + ...

- #S[Je me suis douché.][I has a shower.]

Subject + n'/ne + être in present tense + pas + past participle of the main verb + ...

- #S[Nous ne sommes pas allés travailler ce matin.][We didn't go to work this morning.]

If the reflexive pronoun is the *direct object* then the past participle agrees gender and number with it. If reflexive pronoun is the *indirect object*, then there is no agreement.

- #S[Elle s'est lavée.][She washed herself. Note that the `se` refers herself, aka the direct object of the action.]
- #S[Elle s'est lavé les cheveux][She washed her hair. Note that `se` refers the les cheveux.]


#link("https://www.ou.edu/class/FRINFO/gram/2/3/1.html")

== Adverbe

#link("https://dictionnaire.lerobert.com/guide/formation-de-l-adverbe")

Adjectif masc #is_ Adjectif fem #is_ +ment #is_ Adverbe

- général, générale, généralement
- parfait, parfaite, #Dict[parfaitement]

Or just add +ment if masc already ends -e.

- difficile, difficilement 

#S[Vous faites _rarement_ de l'exercice!][You rarely exercise.]

= Les modes

- Indicatif
- Subjonctif
- Conditionnel
- Impératif

== Le Conditionnel Présent

Verb form:

le futur simple + imparfait endings #is_ le conditionnel

parler_ai + ais #is_ parlerais

== Combination of imparfait et conditionnel

If something, then something.

#Dict[Si] imparfait + conditionnel

#S[Si tu veux, tu peux répondre aux questions.][
  If I want, you can answer the questions.
]

#S[Si j'#Conj[avais] un chien, je serais le plus heureux au monde!][
  If I had a dog, I would be the happiest person on earth!
]

#S[J'irais en Espagne si j'avais plus d'argent.][
  I would go to Spain if I had more money.
][Note the #Conj[aller] vs #Conj[avoir].]

= The 21 Verbe Forms

TODO: #link("https://vitrinelinguistique.oqlf.gouv.qc.ca/24658/la-grammaire/le-verbe/temps-grammaticaux/temps-simples-et-temps-composes")

== L'indicatif

=== Le Présent

#S[*Je danse* avec mes amis.][
  *I dance / I am dancing* with my friends.
]

Note: French does not have have _present continuous verb form_.

=== Le Passé Composé

=== Le Passé Simple

Similar to _Passé Composé_ and rarely used, except professional writing & fairy tales.


=== L'imparfait

Similar to _Passé Composé_.

#S[Il *#Conj[faisait]* beau, mais *j'étais* triste.][
  It was nice outside, but I am sad.]

=== Le Futur Simple

#S[Nous *#Conj[finirons]* bientôt.][
  We will finish it soon.
]

*Le futur proche*: talking about future by conjugate *aller*.

#S[Demain, je #Conj[vais] #Conj[visiter] un musée sur les planètes.][
  Tomorrow, I will visit a museum about planets.
]

=== Le Plus-Que-Parfait

"More than perfect" #is_ talking about an event that happened before another event in the past.

#S[J'#Conj[avais] beaucoup #Conj[pratiqué] avant d'étudier en France.][
  I *had practised* a lot before studying in France.
]

=== Passé Antérieur
=== Le Futur Antérieur

== Subjonctif

=== Subjonctif Présent

#S[Il est dommage que les parents de Tex #Conj[soient] morts.][
  It is sad that Tex's parents are dead.    
]

=== Subjonctif Passé
=== Subjonctif Imparfait
=== Subjonctif Plus-Que-Parfait

== Conditionnel

=== Conditionnel Présent

#S[Je te #Conj[donnerais] de l'argent][
  I *would give* you some money.
]

=== Conditionnel Passé
=== Conditionnel Passé 2 (Formal)

== L'impératif
=== L'impératif Présent
=== L'impératif Passé

== L'infinitif
=== L'infinitif Présent
=== L'infinitif Passé

== Le Participe
=== Le Participe Présent
=== Le Participe Passé

#pagebreak()
= Random Reviews

== July 1

#S[Quelle heure est-il?][What time is it?]

#S[Je vais prendre un café / une bière.][I will go take a cup of coffee / a beer.]

#S[jus d'orange][orange juice]

#S[Je travaille avec un kinésiologue.]

#S[Une fille parle sa saison préférée.][A girl talks abot her favorite season.]

#S[Il est ici/là-bas.]
- ici #is_ here
- là-bas #is_ over there

#S[Il faut qu'on parle][We need to talk. #link("https://fr.wikipedia.org/wiki/Il_faut_qu%27on_parle")]

#S[Elle et son amie veulent parler avec le garçon.][She and her friend want to talk with the boy.]

#S[Vous voulez quoi?] est une _autre façon_ de dire #S[Qu'est-ce que vous voulez?]

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

#S[Va dans le salon, j'ai des choses à finir dans la cuisine.][
  Go to the living rootm, I have things to finish in the kitchen.
]

#S[Je m'excuse.][Sorry!]

#T[écrivain écrivaine][writer]
#T[danseur danseuse][dancer]

#T[réalisateur réalisatrice][director]

#S[Malika fait des films. Ell est _réalisatrice_.]

#T[pouvoir][to be able to]
#S[Il ne _peut_ pas parler][He cannot speak.]

#T[qqch. #is_ quelque chose][something]
#S[Mets _quelque chose_ de chaud.][Wear something warm.]

#S[Tu as passé un bon week-end?][
  How was your weekend?]

#S[On va se voir au même café _le mois prochain_.][
  We're going to meet at the same coffee shop next month.
]
#T[prochain][next]

== July 21

#T[convenir][to admit; to agree]

#S[Je conviens que c'est un problème.][
  I agree that it is an problem.
]

#S[Nous sommes convenus d'un planning][
  Have we agreed a schedule?
]

#T[antérieur, ultérieur][
  earlier, later
]

#S[Je #Conj[tiens] à *avoir* de bonnes relations avec toi.][
  I want to *have* a good relationship with you.
]
#S[Je #Dict[rêve] d'avoir une belle vie][
  I dream about having a good life.
]

#S[Vincent *a* accepté *de* participer *à* l'événement.][
  Vincent *has* accepted to attend the event.
][
  Note that *a* is #Conj[avoir] and *de* and *à* are prepositions.
]

=== Je veux
#link("https://www.youtube.com/watch?v=0TFNGRYMz1U")

#S[Je veux de l'amour, de la joie, de la bonne humeur][
  I want love, joy and good spirit.
]

#S[Bienvenue dans ma réalité.][
  Welcome to my world.
]

#S[Allons ensemble, découvrir ma liberté.][
  Let's go together, to discover my liberty.
]
