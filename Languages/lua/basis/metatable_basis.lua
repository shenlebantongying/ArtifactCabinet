Pos = {}
Pos.mt = {}

function Pos.new (x,y)
    local pos = {x=x,y=y}
    setmetatable(pos, Pos.mt)
    return pos
end

function Pos.mt.__add (p1,p2)
    return Pos.new(p1.x+p2.x,p1.y+p2.y)
end

A = Pos.new(1,2);
B = Pos.new(3,4);
C = A+B;

print(getmetatable(A),getmetatable(B),getmetatable(C))
print((A+B).x,(A+B).y);