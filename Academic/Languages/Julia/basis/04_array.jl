# generate some arrays

# the noob way
begin
    acc = 0.0
    b = zeros(3, 6)
    for i in 1:size(b)[1]
        for j in 1:size(b)[2]
            setindex!(b, acc, i, j)
            acc += 1
        end
    end
    b
end

# still noob but assign a row at a time
begin
    b = zeros(3, 6)
    init = Array(1:6)
    for i in 1:size(b)[1]
        b[i, :] = init
        init .+= size(b)[2]
    end
    b
end

# The good way
begin
    b = 1.0:(3.0*6)
    reshape(b, 3, 6)
end