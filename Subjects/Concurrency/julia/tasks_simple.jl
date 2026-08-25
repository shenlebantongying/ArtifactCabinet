l() = begin
    t = sum(i for i in 1:1000)
    println(t)
end
tas = Task(l)
schedule(tas)
yield()

println()
println(istaskstarted(tas))
