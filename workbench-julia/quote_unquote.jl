a=1
ex = :($a+b)
# => :(1 + b)
b=2
eval(ex)