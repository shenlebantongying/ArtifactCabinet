begin
    local sol = 0
    local sol_t1 = 0
    local sol_t2 = 0

    for t1 = 100:999, t2 = 100:999
        m_product = t1 * t2
        d = digits(m_product)

        yep = true
        for i = 1:floor(Int, length(d)/2)
            if d[i] != d[length(d)+1-i]
                yep = false
            end
        end
        if (yep && m_product > sol)
            sol = m_product
            st1 = t1
            st2 = t2
        end

    end
    println("$(sol), $(sol_t1) - $(sol_t2)")
end
