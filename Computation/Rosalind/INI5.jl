begin
    f = readlines("./data/rosalind_ini5.txt")
end

for i in getindex(f, collect(2:2:length(f)))
    println(i)
end
