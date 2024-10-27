#!/usr/bin/env julia

f = open("./plot_t/a.tex","w+")

headerString = raw"\documentclass{standalone}
\usepackage{tikz}
\begin{document}
\begin{tikzpicture}
"

endString = raw"\end{tikzpicture}
\end{document}
"

aStr = raw"
\draw (-1.5,0) -- (1.5,0);
\draw (0,-1.5) -- (0,1.5);
"

A = [5 10;
10*sin(deg2rad(60)) 0]

struct ABC
    b::Float64
    c::Float64

    function ABC(a,b,c)
        @assert a+b+c == 1.0
        return new(b,c)
    end
end

function vec(v::ABC)
    return [v.b;v.c]
end

struct XY
    x::Float64
    y::Float64
end

function c_abc_xy(v::ABC)::XY
    n = A * vec(v)
    return XY(n[1],n[2])
end

function drawLine(p1::XY,p2::XY)
    return "\\draw ($(p1.x), $(p1.y)) -- ($(p2.x),$(p2.y));\n"
end

function drawLine(p1::ABC,p2::ABC)
    p1_xy = c_abc_xy(p1)
    p2_xy = c_abc_xy(p2)
    return "\\draw ($(p1_xy.x), $(p1_xy.y)) -- ($(p2_xy.x),$(p2_xy.y));\n"
end


write(f,headerString)

write(f, drawLine(ABC(1,0,0),ABC(0,1,0)))
write(f, drawLine(ABC(0,1,0),ABC(0,0,1)))
write(f, drawLine(ABC(0,0,1),ABC(1,0,0)))

write(f, drawLine(ABC(1,0,0),ABC(0.1,0.3,0.6)))
write(f, drawLine(ABC(0.9,0.1,0),ABC(0,0.1,0.9)))


write(f,endString)