import DelimitedFiles

begin
    data = DelimitedFiles.readdlm("./data/rosalind_maj.txt", Int64, skipstart = 1)
end

function get_maj(v)
    stats = zeros(1, maximum(v))
    (i -> stats[i] += 1).(v)

    ret_max = argmax(stats)[2]
    stats[ret_max] / sum(stats)
    return if (stats[ret_max] / sum(stats) > 0.5)
        ret_max
    else
        -1
    end
end

get_maj.(eachrow(data))'
