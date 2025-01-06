# similar to Go

function l_list(ch::Channel, n::Integer)
    for i in 1:n
        put!(ch,i)
    end
end

for i in Channel(ch -> l_list(ch,10))
    println(i)
end