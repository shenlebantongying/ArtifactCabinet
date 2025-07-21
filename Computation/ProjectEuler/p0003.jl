#=
Note: Naive solution will take unrealistic long time.
TODO: https://en.wikipedia.org/wiki/Integer_factorization
TODO: https://projecteuler.net/overview=0003
=#

function smallest_prime_factor(n::Int64)
    # TODO: optimize opportunity here, the upper bound can be smaller than n
    for i in 2:n
        if rem(n, i) == 0
            return i
        end
    end
end

function largest_prime_factor(n::Int64)
    while true
        s = smallest_prime_factor(n)
        if s < n
            n = div(n, s)
        else
            return n
        end
    end
end

println(largest_prime_factor(13195))
println(largest_prime_factor(600851475143))