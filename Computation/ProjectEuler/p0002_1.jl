mutable struct Fib
    a::Int64
    b::Int64
end

f = Fib(0, 1)
s::Int64 = 0
while f.b < 4 * 10^6
    new = f.a + f.b
    f.a = f.b
    f.b = new
    if iseven(new)
        global s += new
    end
end

println(s)
