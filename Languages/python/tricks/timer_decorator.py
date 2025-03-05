import time


def timer(func):
    def wrapper():
        start = time.perf_counter()
        func()
        end = time.perf_counter()
        print(end - start)

    return wrapper()


# > when this wrapper is applied,
# the function will be executed.
# because the timer wrapper invoked it
@timer
def quartic_power():
    res = 0
    for i in range(0, 100):
        for j in range(0, 100):
            for k in range(0, 100):
                res += i * j * k
