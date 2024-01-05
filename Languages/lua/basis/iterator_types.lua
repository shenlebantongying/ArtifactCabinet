-- via closure
function fromto(a,b)
    return function()
        if a > b then
            return nil
        else
            -- We want to print a,
            -- but before the next iteration, we want value of a increment
            a = a + 1
            return a - 1
        end
    end
end

-- via states
function fromto2(a,b)
    return function (state)
        if state[1] > state[2] then
            return nil
        else
            state[1] = state[1] + 1
            return state[1] - 1
        end
    end, {a,b}
end




for i in fromto2(4,8) do print(i) end