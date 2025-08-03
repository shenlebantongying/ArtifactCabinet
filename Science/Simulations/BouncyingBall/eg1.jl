import Plots
import LinearAlgebra

begin
    local pos = [0 0]
    local v = [100 100]
    local G = [0 -1]
    R(v) = -0.01 * v

    local trace_x::Vector{Float64} = []
    local trace_y::Vector{Float64} = []

    time_delta = 0.5 # computation density

    while LinearAlgebra.norm(v) > 0.1
        println(pos)
        acceleration = G + R(v)
        v += time_delta * acceleration
        pos += time_delta * v
        if pos[2] <= 0 # hitting ground
            v = [1 -1] .* v
            pos = [1 -1] .* pos
        end
        push!(trace_x, pos[1])
        push!(trace_y, pos[2])
    end

    fig = Plots.plot(trace_x, trace_y)
    Plots.savefig(fig, "./eg1.pdf")
end
