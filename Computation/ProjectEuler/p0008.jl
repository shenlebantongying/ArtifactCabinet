s = replace(read("p0008_input.txt", String), "\n" => "")

begin
    sol = 0
    #TODO: window function?
    for i = 1:(length(s)-12)
        numbers = map(c -> parse(Int64, c), collect(s[i:(i+12)]))
        sol = max(sol, reduce(*, numbers))
    end
    sol
end
