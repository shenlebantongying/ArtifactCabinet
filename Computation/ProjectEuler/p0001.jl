# noob
begin
    local ret = 0
    for i = 1:(1000-1)
        if rem(i, 3) == 0 || rem(i, 5) == 0
            ret += i
        end
    end
    println(ret)
end

#reduce
begin
    reduce(
        (acc, i) -> (rem(i, 3) == 0 || rem(i, 5) == 0) ? acc + i : acc,
        1:(1000-1),
        init=0,
    )
end
