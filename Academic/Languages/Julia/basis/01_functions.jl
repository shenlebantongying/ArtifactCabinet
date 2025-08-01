# [Functions as first-class citizens]
# In Julia, a function is an object that maps a tuple of argument values to a return value
function f(x,y)
    x+y
end
f(1,2)

g(a,b) = a*b
g(2,3)

# Julia can use Unicode!
∑(a,b,c) = a+b+c
∑(1,2,3)

# varargs (variadic) functions
third(a,b...)=(a,b)
third(1) # -> (1, ())
third(1,2) # -> (1, (2,))

function generic_sum(args...)
    sum=0
    for i in args
        sum+=i
    end
    return sum
end

generic_sum(1,2,3,4,5,6)
# -> 21

# Anonymous functions
# WOW, this is easy!
map(x->x^2,[1.1, 2.2, 3.3])

# -> 3-element Array{Float64,1}
#1.2100000000000002
#4.840000000000001
#10.889999999999999

# [Meta Programmming]
myprog = "1+1"
expr1  = Meta.parse(myprog)
typeof(expr1)

expr1.head #-> call
expr1.args #-> :+ 1 1

# thus expr1 is equal to
expr2 = Expr(:call, :+, 1 , 1)
# which is exactly a (+ 1 1) in lisp!
