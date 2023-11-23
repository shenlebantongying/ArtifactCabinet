using SymbolicUtils

function add1(x)
    x + 1
end

@syms a b c
rule1 = @rule cos(~x) => 2 * sin(~x)
rule1_multi = @rule ~x + ~ ~ xx => ~~xx

# note that ~x matches a+b
rule1(cos(a + b)) # -> 2sin(a+b)

# "eval"

substitute(rule1(cos(a + b)), Dict(a => 1, b => 2))

@show rule1_multi(a + b + c)[2]

## Chaining rules

function chain2(a::String)
    println("This is -> ", a)
end

@syms w chain0(w::String) chain1(w::String)
rule2 = @rule chain0(~x) => chain1(~x)
rule3 = @rule chain1(~x) => chain2(~x)

rule3(rule2(chain0("jerry")))

## Constrains

