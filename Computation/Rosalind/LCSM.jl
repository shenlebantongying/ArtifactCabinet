
begin
    local seqs = split(
        replace(read("./data/rosalind_lcsm.txt", String), "\n" => ""),
        r">Rosalind_\d+",
        keepempty = false
    )
    main_str = seqs[1]
    other_str = seqs[2:length(seqs)]
end

begin
    local long = ""
    for len in length(main_str):-1:1
        # TODO: there is no sliding window function in std as of 1.11
        for ia in 1:(length(main_str) - len + 1)
            seg = main_str[ia:(ia + len - 1)]
            if all(s -> !isnothing(findfirst(seg, s)), other_str)
                long = seg
                @goto ok
            end
        end
    end
    @label ok
    @show long
end
