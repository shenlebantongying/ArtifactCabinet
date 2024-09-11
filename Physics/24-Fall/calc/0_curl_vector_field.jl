# Plotting vector field z = -y vx + x vy
# where vx and vy are unit vectors.

using CairoMakie
f = Figure(size=(1000, 1000))
Axis(f[1, 1])

xs = LinRange(-100, 100, 9)
ys = LinRange(-100, 100, 9)

xvs = [ -y for x in xs, y in ys ]
yvs = [ x for x in xs, y in ys ]

arrows!(xs, ys, xvs, yvs, arrowsize = 10, lengthscale = 0.3)

save("0curl.pdf",f)

