import DelimitedFiles
begin
    local v = DelimitedFiles.readdlm("./data/rosalind_ddeg.txt", ' ', Int, '\n')
    n = v[1, 1]
    m = v[1, 2]
    edges = v[2:end, :]
end

begin
    local neighbors = [Int64[] for i in 1:n]
    for e in eachrow(edges)
        push!(neighbors[e[1]], e[2])
        push!(neighbors[e[2]], e[1])
    end
    local degrees = length.(neighbors)
    local ddeg = (x -> sum(getindex(degrees, x))).(neighbors)
    print(join(string.(ddeg), ' '))
end
