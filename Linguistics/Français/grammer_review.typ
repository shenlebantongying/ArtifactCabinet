#set document(title: "Français")
#set text(
  font:("Comic Neue","New Computer Modern"),
  lang: "fr")
#set smartquote(enabled: false)

#set heading(numbering: (..numbers) => { h(-4em) + numbers.pos().map(_ => h(1em)).join() })

#show link: i => {
  underline(
    text(i,
      weight: "bold",
      fill: luma(30%)))
}

#show emph: it => {
  underline(it.body)
}

#let est = sym.arrow.r

/* Translations */
#let Tr(t) = {[\ #est #text(t,olive)]}

/* Sentence -> Verbatim + Translation */
#let S(eg, ..trans) = {
  text([#eg],purple)
  trans.pos().map(w => Tr(w)).join()
  linebreak()
}

/* Terms */
#let T(term, ..trans) = {
  text(weight: "bold",fill: rgb(8,25,45))[#term ]
  trans.pos().map(w => [: #w ]).join()
  linebreak()
}

/* Inline Annotation, NOTE: there is no way to align box to text' top bounding edge as of 2025 */
#let Anno(t1,t2) = {
  box(
    par([
      #text(t2,gray,0.75em)
      #linebreak()
      #t1],
      leading:0.3em))
}

#let Dict(word) = {link("https://dictionnaire.lerobert.com/definition/"+word.text)[#word]}
#let DictWiki(word) = {link("https://en.wiktionary.org/wiki/"+word.text)[#word]}
#let DictCollins(w) = {link("https://www.collinsdictionary.com/dictionary/french-english/"+w.text)[#w]}
#let Conj(word) = {link("https://leconjugueur.lefigaro.fr/conjugaison/verbe/"+word.text)[#word]}

#let WordTable2(..tt) = {
  table(
    columns:2,
    align: (right, left),
    stroke: (top:gray, bottom: gray, left:none, right:none),
    ..tt.pos().chunks(2,exact:true).map(pair=>{
      let(word,comment)=pair
      (Dict(word),comment)
    }).flatten())
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
- As-tu #est swap subject & verb for questioning.

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

Adjectif masc #est Adjectif fem #est +ment #est Adverbe

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

le futur simple + imparfait endings #est le conditionnel

parler_ai + ais #est parlerais

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

"More than perfect" #est talking about an event that happened before another event in the past.

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

= La Musique

== Je veux

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

== Les Yeux Noirs
- #link("https://www.youtube.com/watch?v=gfiqW1WaGbw")
- #link("https://lyricstranslate.com/en/les-yeux-noirs-blackdark-eyes.html")

= La Vidéos

== Sciences de la Terre et de l'environnement

#link("https://www.youtube.com/watch?v=GLu5jvIHlE8")

Notre planète a débuté son historie sous la forme d'une boule de feu.
#Tr[Our plant starts its history from a big fire ball.]

Les géosciences étudient la Terre et son environnement
dans une optique interdisciplinaire qui intègre les connaissances
et les méthodes des sciences biologiques, chimiques et physiques.

Le géoscientifiques étudient des problèmes à des échelles temporelles et spatiales très vastes.
#Tr[The geo-scientists study problems in a vaste temporal and spatial scale.]

Si vous êtes curieux, créatif, avez le sens l'observation, aimez les activités de plein air
et #Dict[souhaitez] comprendre _notre monde naturel_.

Les études en géosciences _pourraient être un choix de carrière idéal_.
#Tr[Those geo-science studies could be an ideal carrier choice.]

= Le Cinéma

== Amélie, 2001

=== Chapter 1

Dès lors, son père *la croit* victime d'une anomalie cardiaque.
#Tr[Thenceforth, her father believed that she was a heart disease's victim. ]

Amélie n'a de refuge *que* dans le monde qu'elle invente.
#Tr[Amélie don't have a shelter except her imaginary world.]

TODO: que?

=== Chapter 2

Les jours, les mois, puis les années passent.

Mais ça, pour le moment, elle n'en sait rien.
#Tr[But, for this moment, she knows nothing.]

Je travaille jamais le dimanche.
#Tr[I never work in Sundays.]

Quand on #Conj[était] jeune avec ta mère, on #Conj[aurait] bien voyagé.
#Tr[When you where young and your mom is alive, we would have a good travel.]

J'aime bien me retourner dans le noir et contempler le visage des autres spectateurs.
#Tr[I love watching in dark and thinking the faces of other watchers.]

Briser la croûte des crèmes brûlées avec la pointe de la petite cuillère.
#Tr[Break the crust of burnt cream with the tip of a little spoon.]

Le temps n'a rien changé.
#Tr[Time does not bring change.]

C'est alors que survient l'événement qui va #Dict[bouleverser] la vie d'Amélie.
#Tr[It is the time when the event, that changes the life of Amélie, occurs.]

Où qu'il soit, elle va #Dict[retrouver]
_la piste du propriétaire_ de _la boîte aux souvenirs_,
et lui restituer son trésor.
#Tr[No matter where he is, she is going to find the trail of the memory box's owner,
and give his treasure back.]

- qu'il #est que + il
- Où que #est wherever

Je voulais _vous demander_... les gens qui habitaient chez moi dans les années cinquante?
#Tr[I want to ask you who lives in my home in the fifties.]

C'est tout ce que j'ai à dire.
#Tr[That's is all what would say.]
#Tr[What more can I say?]

=== Chapter 3

Ça vous ennuie si je pars un peu plus #Dict[tôt], cet après midi?
#Tr[Do you mind if I leave earlier, this afternoon?]

Depuis cinq ans que j'habite ici,
c'est la première fois que je vous #Dict[croise].
#Tr[In the five years when I live here,
This is the first time that I see you.]

J'aime beaucoup ce tableau.

Le plus dur, ce sont les regards.
#Tr[The looks are the hardest part.]

Elle est #Dict[peut-être] #Dict[seulement] différente des _autres_.
#Tr[She is maybe only/just different from others.]

Ça doit être mon ange gardien. C'est pas possible, autrement.
#Tr[It must be my guardian angle. Other than that, it is no possible]

Vous #Dict[croyez] pas?
#Tr[You don't believe that?]

Amélie #Conj[a] soudain le sentiment étrange *d'être* en harmonie totale avec elle même.
#Tr[Amélie suddenly has a strange feeling *of being* in total harmony with herself.]

=== Chapter 4

Gina est assez grande pour *se défendre* toute seule.
#Tr[Gina is strong enough to defend herself all alone.]

Ouvrez les yeux, elle est là espérer une miette d'intérêt de votre part, et vous n'*en* avez que pour Gina.
#Tr[Open your eyes, she is desperate for a tiny share of you interest, and you don't have that for Gina.]

Note the `en + verb`.

=== Chapter 5

Une femme sans amour, c'est comme une fleur sans soleil.
#Tr[A women without love is like flower without sun.]

Je crois qu'*il y a* _quelque_ #Dict[chose] qui change.
#Tr[I believe that there are things that changes.]

Il y a _quelque_ chose qui _ne va pas_?
#Tr[Is there something that's not going?]
#Tr[Are things going well?]

=== Chapter 6
``
Vous me désirez?
#Tr[What do you want from me?]

*J'y* retourne.
#Tr[I go back there.]

Note the `y` in `J'y` #est `Je y`  refers to the obvious place based on context.

Voilà, tout ce que vous m'avez demandé.
#Tr[Here, all this that you have asked.]

=== Chapter 7

Elle *espère* une récompense en échange de l'album.
#Tr[She desperately want compensation in exchange for the album.]

=== Chapter 8

=== Chapter 9

A ce moment précis de l'historie, Amélie est la seule à connaître la chef de l'énigme de _l'inconnu des photomatons_.
#Tr[At this precise, Amélie is the only one knows the enigma/secret of the unknown (man) in the photo album.]

=== Chapter 10

C'est épatant! Tu #Conj[pouvait] pas fair mieux.
#Tr[You are amazing. You couldn't do it better.]

C'est vous?

_L'inconnu des photomatons_ n'était pas _un revenant_, ni un obsédé par la peur de vieillir, c'était juste le réparateur.
#Tr[The unknown (man) in the photo album is not a ghost, not a maniac, and not a man afraid of aging, he is just a repairer.]

C'est vous qui *m'avez* laissé ça dans ma poche?
#Tr[Is is you who put this in my pocket?]
TODO: m'avez?

=== Chapter 11

Qu'est-ce qui se passe ici?
#Tr[What's is happening here?]

Vous croyez aux miracles?
#Tr[Do you believe miracles?]

=== Chapter 12

Si vous laissez passer cette chance, alors avec le temps, c'est votre cœur qui va devenir aussi sec et cassant.
#Tr[If you let this chance goes by, with the time passing, it is you heart that will become dry and brittle.]

= Random Reviews

== July 1

#S[Quelle heure est-il?][What time is it?]

#S[Je vais prendre un café / une bière.][I will go take a cup of coffee / a beer.]

#S[jus d'orange][orange juice]

#S[Je travaille avec un kinésiologue.]

#S[Une fille parle sa saison préférée.][A girl talks abot her favorite season.]

#S[Il est ici/là-bas.]
- ici #est here
- là-bas #est over there

#S[Il faut qu'on parle][We need to talk. #link("https://fr.wikipedia.org/wiki/Il_faut_qu%27on_parle")]

#S[Elle et son amie veulent parler avec le garçon.][She and her friend want to talk with the boy.]

#S[Vous voulez quoi?] est une _autre façon_ de dire #S[Qu'est-ce que vous voulez?]

#hr_

- à #est before city names (à Montréal)
- en #est feminine countries (en Russie)
- au #est masculine countries (au Canada)
- aux #est plural countries (aux États-Unis)

#hr_

Comparatif de supériorité/d'infériorité/d'égalité

plus/moins/aussi + adj. + que

#hr_

Regular conjugaisons du participe passé

- -er #est -é
- -ir #est -i
- -re #est -u

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

#T[qqch. #est quelque chose][something]
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

== July 28

- #Dict[bientôt]
- #S[Dans #Dict[peu] de temps, dans un proche futur.][
  In short time, in near future]
- #S[Au revoir et à bientôt!][
  Good bye and see you soon!]

#T[journée soirée nuit][morning, evening, night]

#[prénom, nom de famille]

*Interrogative adjectives and pronouns*

- quel/quels
- quelle/quelles

#S[Quelle est la couleur que tu préfères?][
  Which is your favourite colour?
]

- qui #est who/whom
- de qui/à qui #est whose

qui as subject:

#S[Qui est Américain dans ce groupe?][
  Who is American in this group?
]

qui as direct object:

#S[Qui #Conj[as]-tu #Conj[vu] ce matin?][
  Who did you see this morning?
][
  as-tu #est tu avoir in reverse.
]

quit with à indicating possession:

#S[À qui est cet ordinateur?][
  Whose computer is this?
]

#hr_

dont #est pronom relatif

#S[#Dict[Voici] ce #Dict[dont] je veux te parler][
  This is what I what i want to talk about.
]

#hr_

#S[Ça *#Conj[vent] #Conj[dire]*...][
  It means... or It want to say...
]

#S[Que *veut dire*...?][
  What does ... mean?
]

#S[Après son explication, j'ai compris ce qu'elle voulait dire.][
  After her

#S[Ça s'écrit avec deux Y][It's spelled with two letter Y]
explanation, I understood what she meant.
]

#S[Comment on dit «friend» en français?][
  How do you say "friend" in french?
]

#hr_

- #T[comment]
- #S[De quelle manière; par quel moyen.][
  In what way; by what means.
]
- #S[Comment allez-vous?][How you doing?]

#hr_

== July 31

- #S[Vous #Conj[pouvez] m'#Conj[aider]?][
  Can you help me?
]
- #S[Je peux vous aider?][
  Can I help you?
]

#S[Vous faites quoi dans la vie?][
  What do you do for a living?
]

#hr_


- #S[C'est toi qui...][
  It was you who... (Somewhat fixed expression.)]
- #S[C'est toi qui le dis!][That's what you say!]

#S[Vous travaillez où?][
  Where do you work?
]

#hr_

#link("https://www.alloprof.qc.ca/fr/eleves/bv/francais/qu-elle-et-qu-elles-quel-quels-quelle-et-quelles-f1328")

- quel/quels #est masc.
- quels/quelles #est fem.


#hr_

*Verb + preposition(à/de/par/pour) + infinitive*

- #Dict[parvenir]
- #S[Elle est finalement *parvenue* à ouvrir la porte.][
  She finally managed to open the door.
]
- #S[On est *parvenu* à régler l'affaire.][
  We managed to setting/concluding the business.
]

== Aug 3

#S[Vous #Conj[avez] quel âge?][
  How old are you?
]
#S[Quel âge a votre fils?][
  How old is your son?
]
#S[Est-ce que ce sont vos frères et sœurs?][
  Are these your siblings?
]
#S[Je fais du vélo][
  I go biking
]
#S[On fait *de* la zumba dans une école.][
  We do zumba in a school.
]
#S[Je fais du vélo.][
  I go biking.
]

Partitive articles

TODO: word combinations?s

- du, de l' #est masc.
- de la, de l' #est fem.
- des #est plural.

== Aug 9

#S[Ils n'étudient pas la fin de semaine (FDS).][
  They don't study in weekends.
]

#S[Vous étudiez quels jours de la semaine?][
  You study in what days of the week?
]

#T[Le matin, L'après-midi, Le soir, La nuit][Morning, Afternoon, Evening, Night]

- #S[*À* quelle heure vous mangez?][
  When do you eat?
]
- #S[Je déjeune *à* huit heures *du* matin.][
  I eat breakfast at 8:00 in morning.
]

- #S[Où habites-tu?][Where do you live?]
- #S[Nous habitons au centre-ville de Tornoto][
  We live in the central city of Tornoto.
]

au, à, en, aux #link("https://apprendre.tv5monde.com/en/aides/grammar-prepositions-au-en-aux-name-city-or-country")

- #S[Mon condo *n'a* pas deux _salles de bain_.][
  My condo don't have two _bathrooms_.
][
  Note that the *n'a* is ne + #Conj[a].
]
- #S[Est-ce que votre condo *a* un balcon?]

- #T[il y a][there is/are]
- #S[Dans la cuisine, *il y a* une table et quatre chaises.]
- #S[Est-ce *qu'il* y a une télé dans ta chambre?]

Mon,Ma,Mes #link("https://www.bbc.co.uk/bitesize/guides/z8tfxfr/revision/1")

- #S[Bravo, tu as atteint la fin du niveau!]
- #S[Tu #Conj[peux] maintenant accéder au prochain niveau de ton #Dict[parcours].]
- #T[niveau][level]

== Aug 10

- #S[Bonne fin de semaine!][Have a good weekend!]
- #S[Bonne fin de journée et à demain!][Have a good day and see ya tomorrow!]
- #S[À bientôt!][See ya soon!]
- #S[Je suis contente de te voir!][Good to see you!]
- #S[Je suis très *contente de vous rencontrer*.][
  I am very glad to meet you.]
- #S[Je suis contente *de* vous *revoir*!][
  I am happy to see you again!]

#S[Je ne #Dict[connais] pas du tout cette région.][
  I don't know about this region at all.]

#S[la première dois][the first time]

#S[Vous venez d'*où?*][Where are you from?]
#S[On #Conj[vient] du Québec.][We're from Quebec.]

#S[Que faites-vous dans la vie?][What do u do for living?]

== Aug 11

#S[Notre père *n'a* pas de cheveux.][My father don't have hair.]
#S[Ils ont les yeux bleus.][He has blue eyes.]
#S[Je porte une chemise blanche.][I wear a white shirt.]
#S[Je porte des lunettes.][I wear eye glasses.]

#S[Je porte une robe *blanche* et des souliers *noirs*][
  Note that adjectives agrees with the nouns.
]

#DictWiki[déjà]

#WordTable2(
  [malade],[sick],
  [inquiète],[worried],
  [nerveux],[nervous],
  [en colère],[angry],
  [fâchée],[angry],
  [faim],[hungry],
  [soif],[thirsty],
  [chaud],[hot],
  [froid],[cold],
  [peur],[scared,fear],
)

== Aug 13

#S[Êtes-vous correct?][Are you OK?]
#S[Est-ce que vous #Conj[voyez] des amis la fin de semaine?]
#S[Qu'est-ce qui te fait tripper dans la vie #DictCollins[à part] les fraises?][What makes you happy beside those berries.]
#S[*Amies-tu aller* au restaurant la fin de semaine?][Would you like to go to restaurant in the weekend?]
#S[Aimes-tu aller *au* gym ou *aux* arcades?]
#S[Tu #Conj[vas] à la librairie avec Hassan.]
#S[Je vais *au* supermarché.][*au* #est à + le for masc.]
#S[Elle est restée *à la* maison.][She stayed at home.]
#S[Est-ce que vous _allez au_ cinéma avec votre fils?]

#hr_

à vs de #link("https://dictionnaire.lerobert.com/guide/a-ou-de")

#S[Je vais *à* la plage et j'écoute *de* la musique. ]

#S[Joues-tu d'un instrument?][Do you play an instrument?]
#S[Je joue *du* piano et *de la* guitare.]

#S[Je #Dict[préfère] lire.]
#S[Quel est ton sport préféré?]
#S[Je *préfère* lire. Mon livre *préféré* est...]
#S[Préfères-tu la ville ou la campagne?]
Note that `préféré` is adjectif and `préférer` is verbe with conjugaison `je préfère`.

- à + le #est au
- de + le #est du

== Aug 14

#link("https://en.wiktionary.org/wiki/Appendix:French_verbs")

#S[Je *m'habille* et je *me prépare*.][I get dressed and I get ready.]

#S[Comment vas-tu au travaille?][How do you get to work?]
#S[Je prends le transport en commun.]
#S[Je me promène à vélo et je marche à côté de la rivière.][
  I ride a bike and walk in the river sides.]
#S[Tu #Dict[vas] #Dict[au] travail en métro ou à vélo?][
  Do you get to work by metro or by bike?]

== Aug 15

#WordTable2(
  [des tâches ménagères], [household chores],
  [le lavage],[laundry],
  [range la maison],[clean the house],
  [couchez], [go to bed]
)

Que fais-tu le soir?
Qu'est-ce que vous faites après le travail?
Est-ce qu'il fait la vaisselle? #Tr[Will you do the dishes?]
Mon conjoint prépare le *souper* et on *soupe* ensemble.

#Conj[vivre] \
Vous vivez seul?
#Tr[Do you live alone?]

#Dict[sortir] \
Tu *sors* avec quelqu'un?
#Tr[Are you dating any one?]
Je sors avec...
