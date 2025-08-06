begin
    f = read("./data/rosalind_ini6.txt", String)
end
begin
    local d = Dict{String, Int64}()

    for s in split(f)
        if getkey(d, s, nothing) !== nothing
            d[s] = d[s] + 1

        else
            d[s] = 1
        end
    end

    for (k, v) in d
        println("$(k) $(v)")
    end
end
