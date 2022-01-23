double = (2*)

{- |
>>> double 12
24
-}

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- >>> split [1,2,3,4]
-- [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

--  date: Thu Jan  6 02:25:02 EST 2022

-- >>> map (\x -> x + 1) [1,3,4]
-- [2,4,5]


mylen :: [a] -> Int
mylen = foldr (\ x -> (+) 1) 0

effiReverse lst =
    let rev ( [], accum )  = accum
        rev ( y:ys, accum) = rev (ys, y:accum)
    in rev (lst, [])

-- >>> effiReverse [4,3,2,1]
-- [1,2,3,4]


