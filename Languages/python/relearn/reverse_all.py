def reverse_all(L):
    L[:] = [int("".join(list(reversed(list(str((li))))))) for li in L]


print(reverse_all([45, 789, 8513600]))
