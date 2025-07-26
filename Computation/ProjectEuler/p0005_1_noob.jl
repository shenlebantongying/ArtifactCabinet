function is_evenly_divisible(n::Int)
    yep = true
    for i = 20:-1:2
        if rem(n, i) != 0
            yep = false
            break
        end
    end
    return yep
end

for v = 1:typemax(Int64)
    if is_evenly_divisible(v)
        println(v)
        break
    end
end
