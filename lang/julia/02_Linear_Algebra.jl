using LinearAlgebra

# [Matrics]
#******************************************************

A = [1 2 3; 4 5 6]
# 2×3 Array{Int64,2}:
#  1  2  3
#  4  5  6

# function based construction
f(x,y) = x - y
B = [f(x,y) for x = 1:5, y=1:5]
# 5×5 Array{Int64,2}:
#  0  -1  -2  -3  -4
#  1   0  -1  -2  -3
#  2   1   0  -1  -2
#  3   2   1   0  -1
#  4   3   2   1   0

# Reshaping

C=reshape(collect(1:15),(3,5))
# 3×5 Array{Int64,2}:
#  1  4  7  10  13
#  2  5  8  11  14
#  3  6  9  12  15


transpose(A)

# A^2 -> invlid
A.^2 # square each elemets

# A [n-th rows, n-th cols]
A[:,1]
A[1,:]

1:10
