import DelimitedFiles

begin
    local d = DelimitedFiles.readdlm("./data/rosalind_dij.txt")
    n = d[1, 1]
    edges::Matrix{Int64} = d[2:end, :]
end
begin
    infinity = typemax(Int32)
    function dij()
        dist::Vector{Int64} = fill(infinity, n)
        prev::Vector{Int64} = fill(0, n)

        dist[1] = 0

        # TODO: replace this with proper priority queue
        priority = fill(0, n)
        priority[1] = -1

        while any(priority .<= 0)
            u = argmin(priority)
            priority[u] = 1
            for (u_e, v, d) in eachrow(edges)
                if u_e == u
                    if dist[v] > dist[u] + d
                        dist[v] = dist[u] + d
                        prev[v] = u
                        priority[v] -= 1
                    end
                end
            end
        end
        return dist
    end

    ret = dij()
    ret[ret .== infinity] .= -1
    println(join(string.(ret), " "))
end
