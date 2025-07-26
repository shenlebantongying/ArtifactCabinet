#=
Trick: using a 2*2 matrix to generate fib sequence
=#
begin
    local init = [0 1; 2 3]
    local stepper = [0 1; 1 1]
    local acc = sum(init)

    cur(arr::Array{Int,(2, 2)}) = arr[2, 2]

    while cur(init) < 4 * 10^6
        init = init * stepper
        if iseven(cur(init))
            acc += init[2, 2]
        end
    end
    acc
end
