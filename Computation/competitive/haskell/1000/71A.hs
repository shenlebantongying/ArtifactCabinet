f x | length x<11=x
    | otherwise = [head x] ++ show(length x -2) ++ [last x]

main = interact $ unlines.map f . tail . lines

{-
. is function chain calling (or composition)

(f . g) x = f (g x)
(f . g . h) x = f(g(h x))
-}
