# TODO: does julia auto vectorize this?

ret = 0
for i in 1:(1000-1)
    if rem(i, 3) == 0 || rem(i, 5) == 0
        global ret += i
    end
end
println(ret)
