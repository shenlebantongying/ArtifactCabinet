#!/usr/bin/env python3
import time


class Timer:
    """
    A hack to measure function runtime with some hacking
    """

    def __enter__(self):
        self.start = time.perf_counter()  # this function used to be time.clock()
        return self

    def __exit__(self, *args):
        self.end = time.perf_counter()
        self.interval = self.end - self.start


def quartic_power():
    res = 0
    for i in range(0, 1000):
        for j in range(0, 100):
            for k in range(0, 100):
                res += i * j * k


with Timer() as t:
    quartic_power()

print(t.interval)
