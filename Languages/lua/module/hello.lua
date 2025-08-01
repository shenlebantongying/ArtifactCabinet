local m = require "mylib"

local res = m.add(m.new(1,2),m.new(3,4))

print("->", res.x, res.y)
