import os
import random
from itertools import groupby

import matplotlib.pyplot as plt

random.seed(os.urandom(128))

ball_move_Left = -1
ball_move_Right = 1


def oneBall(stages: int) -> int:
    return sum(
        [random.choice([ball_move_Left, ball_move_Right]) for _ in range(stages)]
    )


def galton(stages: int, n_balls: int):
    all_results = sorted([oneBall(stages) for _ in range(n_balls)])
    position = []
    counter = []
    for k, g in groupby(all_results):
        position.append(k)
        counter.append(len(list(g)))

    fig, ax = plt.subplots()
    ax.bar(position, counter)

    ax.set_xlim([-stages, stages])
    ax.set_xlabel("Position")
    ax.set_ylabel("Times")

    fig.show()


galton(100, 5000)
