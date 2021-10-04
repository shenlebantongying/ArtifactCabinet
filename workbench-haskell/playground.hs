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
