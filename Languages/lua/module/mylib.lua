local MyLib = {}

function MyLib.new (x,y)
    return {x=x,y=y}
end

function MyLib.add (u,v)
    return MyLib.new(u.x+v.x, u.y+v.y)
end

return MyLib