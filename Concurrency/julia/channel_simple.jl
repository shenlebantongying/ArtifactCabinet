chnl = Channel() do ch
    foreach(i -> put!(ch,i),1:4)
end;

for i in chnl
    @show i
end;