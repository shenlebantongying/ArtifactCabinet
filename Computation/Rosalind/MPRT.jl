begin
    ids = readlines("./data/rosalind_mprt.txt")
end

begin
    function api1(id::String)
        cmd = `curl -s https://rest.uniprot.org/uniprotkb/$(id).fasta`
        ret = readlines(cmd)
        str = join(ret[2:end])
        return str
    end

    function api2(id::String)
        cmd = `curl -s https://rest.uniprot.org/unisave/$(id)"?"format=fasta`
        ret = readlines(cmd)
        str = join(ret[2:(findnext(s -> startswith(s, '>'), ret, 2) - 1)])
        return str
    end

    function uniport_get(id::String)
        return api1(id)
        #return api2(id)
    end

    function get_primary_id(s)
        pos_underscore = findfirst('_', s)
        if !isnothing(pos_underscore)
            return s[1:(pos_underscore - 1)]
        else
            return s
        end
    end
    for i in ids
        offsets = (i -> i.offset).(eachmatch(
            r"(?=(N[^P][ST][^P]))", uniport_get(get_primary_id(i))))'
        if !isempty(offsets)
            println(i)
            println(join((o -> string(o)).(offsets), " "))
        end
    end
end
