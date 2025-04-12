#!/usr/bin/env python3

# https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle

import random


def shuffle(l):
    new_l = l.copy()
    for j in range(len(new_l) - 1, 0, -1):
        r = random.randrange(j + 1)
        new_l[j], new_l[r] = new_l[r], new_l[j]
    return new_l


if __name__ == "__main__":
    l = [1, 2, 3, 4, 5]
    stat = dict()

    for i in range(1000000):
        tl = shuffle(l)
        k = "".join(map(str, tl))
        if k not in stat.keys():
            stat[k] = 1
        else:
            stat[k] = stat[k] + 1
    for i, k in enumerate(sorted(stat.keys())):
        print(i, stat[k])

    # print(statistics.mean(stat.values()))
    # print(statistics.variance(stat.values()))

"""
./Fisher–Yates-Shuffle.py > a.dat 

gnu plot:

set yrange [0:9000]
plot 'a.dat' with linespoints linestyle 1
"""
