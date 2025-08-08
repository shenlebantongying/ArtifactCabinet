#=
the one described in EPFL's lecture

julia --threads 8 ./3SUM.jl
=#

import DelimitedFiles

d = DelimitedFiles.readdlm("./data/rosalind_3sum.txt", skipstart = 1, Int32)'

function sum3(v)
    sp = sortperm(v)
    a = v[sp]
    for k in eachindex(a)
        i = 1
        j = length(a)
        while i != j
            if a[i] + a[j] < -a[k]
                i += 1
            elseif a[i] + a[j] > -a[k]
                j -= 1
            elseif a[i] + a[j] == -a[k] && i != k
                return sort([sp[i], sp[j], sp[k]])
            end
        end
    end
    return nothing
end

results_lock = ReentrantLock();
results = Vector{String}(undef, size(d)[2]);

Threads.@threads for c in 1:size(d)[2]
    t = sum3(d[:, c])
    lock(results_lock) do
        if isnothing(t)
            results[c] = "-1"
        else
            results[c] = join(string.(t), " ")
        end
    end
end

for s in results
    println(s)
end
