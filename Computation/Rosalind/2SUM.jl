import DelimitedFiles

begin
    d = DelimitedFiles.readdlm("./data/rosalind_2sum.txt", Int64, skipstart = 1)
end

begin
    function find_pos(l)
        out = nothing
        for (i, v) in enumerate(l)
            p = findnext(isequal(-v), l, i + 1) # note the i+1 is important when v==0
            if (i == p)
                break
            end
            if (p !== nothing)
                out = "$(i) $(p)"
                break
            end
        end

        if (out !== nothing)
            println(out)
        else
            println("-1")
        end
    end
    foreach(find_pos, eachrow(d))
end
