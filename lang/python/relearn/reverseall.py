L = [12, 45]


def rev_number(n: int):
    temp_L = list(str(n))
    temp_L.reverse()
    return int("".join(temp_L))


def reverse_all(l):
    l[:] = map(rev_number, l)


print(reverse_all(L))
print(L)
