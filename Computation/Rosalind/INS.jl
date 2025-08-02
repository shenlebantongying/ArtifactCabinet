begin
    data = readlines("./data/rosalind_ins.txt")
    global seq = map(x -> parse(Int, x), split(data[2]))
end

begin
    local acc = 0
    function swap!(array, a, b)
        array[a], array[b] = array[b], array[a]
        acc += 1
    end

    for i = 2:length(seq)
        k = i
        while k > 1 && seq[k] < seq[k-1]
            swap!(seq, k, k - 1)
            k = k - 1
        end

    end
    println(seq)
    println(acc)
end
