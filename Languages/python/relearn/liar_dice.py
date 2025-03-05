lol = [[2, 2, 4, 5, 6], [3, 4, 4, 5, 6], [6, 4, 5, 1, 4]]


def maximum_liar_dice(players):
    count_occr = (
        lambda lol, n: 0 if lol == [] else lol[0].count(n) + count_occr(lol[1:], n)
    )
    return sorted(
        list(
            map(lambda n: [count_occr(lol, n) + count_occr(lol, 1), n], [2, 3, 4, 5, 6])
        ),
        reverse=True,
    )[0]


# advantage of racket
