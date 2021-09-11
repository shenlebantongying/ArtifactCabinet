main = interact $ show.solve.map read.words

solve [n, k] 
    | nT >= k = n-k -- K -> final answer accumulator
    | otherwise = solve [ n `div` 10, k-nT-1] 
                        --            ^ minus all 1s directly
                        -- TODO: this is a duplicated div
    where  nT = n `mod` 10  -- judge if the number can be divided by zero