Simplified Barycentric (a,b,c) -> Cartesian (x,y) coordinate transformation.

Because `a = 1 - ( b + c )`, only 2 vectors have to be involved.

For converting coordinates, simply solve `(x,y) = A * (a,b,c)` where `A` are matrix of `(b vec, c vec)` --> `[0,1; sin(deg2rad(60)) 0]`.

`./p.jl && latexmk ./plot_t/a.tex && open ./a.pdf`

TODO: review linearAlgebra.
