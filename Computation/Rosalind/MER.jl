# TODO: generic multi sequence merger?

begin
    data = readlines("./data/rosalind_mer.txt")
    global a = map(x -> parse(Int, x), split(data[2]))
    global b = map(x -> parse(Int, x), split(data[4]))
end

begin
    passEndQ(array, i) = i > length(array)
    local ia = 1
    local ib = 1

    ret::Vector{Int64} = []

    while !passEndQ(a, ia) || !passEndQ(b, ib)
        if passEndQ(a, ia)
            append!(ret, b[ib:length(b)])
            break
        end

        if passEndQ(b, ib)
            append!(ret, a[ia:length(a)])
            break
        end

        if a[ia] < b[ib]
            push!(ret, a[ia])
            ia += 1
        else
            push!(ret, b[ib])
            ib += 1
        end
    end

    for i in ret
        print(i)
        print(" ")
    end
    println()
    ret
end
