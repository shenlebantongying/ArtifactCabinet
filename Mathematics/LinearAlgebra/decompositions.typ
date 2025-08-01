#let Code(path) = {
  block(
    raw(read(path)),
    width:100%,
    stroke:black,
    inset:5pt
  )
}

#set math.mat(delim: "[",align:right)

#align(center,text(17pt)[
  *Factorization & Decomposition of Matrices*
])


= A=LU factorization

== A example of manual factorization

Goal: decompose $A=mat(1,2,3;2,6,7;2,2,4)$ to $L U$.

Step 1, find the lower triangular matrix $E$ such that produces an upper triangular matrix ($E A=U$).

$
  E A = E_1 E_2 E_3 A =
  mat(1,0,0; 0,1,0; 0,1,1)
  mat(1,0,0; 0,1,0; -2,0,1)
  mat(1,0,0; -2,1,0; 0,0,1)
  mat(1,2,3;2,6,7;2,2,4)
  =mat(1,2,3;0,2,1;0,0,-1) = U
$

$
  E=E_1 E_2 E_3 =  mat(1,0,0;-2,1,0;-4,1,1)
$

Note that the diagonal of $E$ is identify matrix, and $E_1,E_2,E_3$ are processes of _Gaussian Elimination_.

Step 2, find the lower triangular matrix $L$ such that $A=L U$

$
  A= E^(-1)U = mat(1,0,0;2,1,0;2,-1,1) mat(1,2,3;0,2,1;0,0,-1) = L U
$

#Code("decompose_01.jl")
