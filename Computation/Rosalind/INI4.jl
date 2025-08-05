import DelimitedFiles
begin
    v = DelimitedFiles.readdlm("./data/rosalind_ini4.txt", ' ', Int, '\n')
    reduce((acc, x) -> if (isodd(x))
            acc + x
        else
            acc
        end, v[1]:v[2]; init = 0)
end
