using Base.Threads

import Base.Threads.@spawn

function fib(n::Int)
    if n < 2
        return n
    end
    println(Threads.threadid())
    t = @spawn fib(n - 2)
    return fib(n - 1) + fetch(t)
end

fib(10)