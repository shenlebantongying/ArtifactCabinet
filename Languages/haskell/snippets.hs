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

-- Pythagorean Triples
-- >>> [(a,b,c) | c <- [1..20], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
-- [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20)]

-- >>> [(a,b) | a <- [1..3], b <- [1..a]]
-- [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3)]

max' :: Ord a => [a] -> a
max' [] = error "empty"
max' [x] = x
max' (x:xs) 
    | x > maxTail = x 
    | otherwise = maxTail
    where maxTail = max' xs -- recursion happens here
