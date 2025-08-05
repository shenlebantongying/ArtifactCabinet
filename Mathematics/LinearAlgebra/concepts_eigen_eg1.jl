#=

Goal:
Find eigenvalues and eigenvectors
λ and v for a 2×2 square matrix.
Replicate https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors#Two-dimensional_matrix_example

Steps:
+ solve det(A-λI) == 0
+ solve Av = λv -> (A-λI)×v == 0

NOTE:
The Eigenvectors cannot be trivially obtained by v = inv(A)*[0,0]
because there is no unique solution.

Julia will throw `LinearAlgebra.SingularException`

Eigenvectors belongs to be a space.
For example, if the row reduced form is [[4 -1], [0 0]],
then any vectors that are constrained by y=4x is an Eigenvector.

TODO: a better way to compute _reduced echelon form_? It does not exists in std?
TODO: Why LU doesn't work?

=#

import LinearAlgebra
import AbstractAlgebra


function compute(A)

    ma = A[1, 1]
    mb = A[1, 2]
    mc = A[2, 1]
    md = A[2, 2]

    a = 1
    b = -(ma + md)
    c = ma * md - mb * mc

    r(lambda) = begin
        @show s = A - lambda * LinearAlgebra.I(2)
        @show AbstractAlgebra.rref(AbstractAlgebra.matrix(AbstractAlgebra.QQ, 2, 2, s))
    end

    println("Solution1:")
    @show lambda1 = (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
    r(lambda1)

    println("Solution 2:")
    @show lambda2 = (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)
    r(lambda2)

    println()

end

compute([[-6 3]; [4 5]])

# wikipedia's example
# Eigenvectors is y = x and y = -x
compute([[2 1]; [1 2]])

