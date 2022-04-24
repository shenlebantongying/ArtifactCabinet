#!/usr/bin/env python3

from z3 import *

# Solve a equation system

x,y,z = Reals('x y z')
s = Solver()

s.add(3*x + 2*y-z == 1)
s.add(2*x - 2*y + 4*x == -2)
s.add(-x + 0.5*y -z == 0)
print(s.check())
print(s.model())
