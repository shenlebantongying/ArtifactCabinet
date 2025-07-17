local fun = {}

function fun.plus(a,b)
	tex.print("$",a,"+",b,"=>",a+b,"$")
end

function fun.LaTeX()
	tex.print("Printed from Lua -> \\LaTeX!")
end

return fun

