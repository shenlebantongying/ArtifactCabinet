macro repeat(n::Integer, expr::Expr)
    local var = gensym()
    quote
        let $var = $n
            while $var > 0
                $expr
                $var -= 1
            end
        end
    end
end

@repeat(10, println("hi!"))

println("--> Expanded form")
@macroexpand @repeat(10, println("hi!"))
