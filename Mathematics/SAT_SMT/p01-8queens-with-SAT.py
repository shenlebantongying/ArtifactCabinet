#
# Replication of the 8 queen example from "Effective problem solving using SAT solvers"
# Using Z3
#

# %%

from z3 import *
import matplotlib.pyplot as plt

# %%

N = 8
n_seq = range(0, N)

Q = [[Bool(f"Q{i}{j}") for j in n_seq] for i in n_seq]

# %%

"""
Obtain diagonals:
tl->br: c =  r+o
tr->bl: c = -r+o

       r
  ┌──────────►
  │
 c│
  │
  ▼
"""


def queen_diagonal_1(r0, c0, mat):
    """
    top-left to bottom-right
    """
    o = c0 - r0
    for r in n_seq:
        c = r + o
        if 0 <= c < N and 0 <= r < N and (r != r0 and c != c0):
            yield mat[r][c]


def queen_diagonal_2(r0, c0, mat):
    """
    top-right to bottom-left
    """
    o = c0 + r0
    for r in n_seq:
        c = -r + o
        if 0 <= c < N and 0 <= r < N and (r != r0 and c != c0):
            yield mat[r][c]


def queen_vertical(r0, c0, mat):
    for r in n_seq:
        if r != r0:
            yield mat[r][c0]


def queen_horizontal(r0, c0, mat):
    for c in n_seq:
        if c != c0:
            yield mat[r0][c]


# %%

t = Solver()
for row in n_seq:
    t.add(Or(*Q[row]))
for column in n_seq:
    cur_column = [Q[i][column] for i in n_seq]
    t.add(Or(*cur_column))


# TODO: there are array type in Z3?

for row in n_seq:
    for column in n_seq:
        t.add(Implies(Q[row][column], Not(Or(*queen_diagonal_1(row, column, Q)))))
        t.add(Implies(Q[row][column], Not(Or(*queen_diagonal_2(row, column, Q)))))
        t.add(Implies(Q[row][column], Not(Or(*queen_horizontal(row, column, Q)))))
        t.add(Implies(Q[row][column], Not(Or(*queen_vertical(row, column, Q)))))

t.check()
m = t.model()

# %%


def find_in_model(z3model, target):
    for i in z3model:
        if i.name() == target.__str__():
            return z3model[target]
    return None


def pp(b):
    return 1 if b else 0


print([[pp(find_in_model(m, Q[r][c])) for c in n_seq] for r in n_seq])

# %%

fig, ax = plt.subplots()
ax.pcolormesh(
    [[pp(find_in_model(m, Q[r][c])) + 0.5 for c in n_seq] for r in n_seq],
    cmap="Greys",
    edgecolors="k",
)
ax.set_aspect("equal")
fig.savefig("solution.pdf")
