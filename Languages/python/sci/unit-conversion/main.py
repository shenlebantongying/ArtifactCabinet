import pint
import operator

unit = pint.UnitRegistry()
unit.load_definitions("./definition.txt")

Q_ = unit.Quantity

a = unit.Quantity(10, "hour")
print(a.to("min"))

b = unit.Quantity(1, "cubic_m")
print(b.to("quadic_m"))

print(b.to("quadic_m").magnitude)

# compose types

# !!!!!!! pitfall : must use () to group a quantity

print(Q_("3 m / (10 s)"))

# list

alist = [1, 2, 3]

print(
    list(
        map(operator.mul, [Q_(x, "m") for x in alist], [Q_(x + 1, "m") for x in alist])
    )
)

print(
    [sum(x) for x in zip([Q_(x, "m") for x in alist], [Q_(x + 1, "m") for x in alist])]
)
