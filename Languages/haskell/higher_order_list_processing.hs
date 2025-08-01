{-
use ghci -> shift+enter to send codes to repl

command block
:{
    code here
:}

-}

print $ foldl (-) 6 [3,2,1]
-- (((6 - 3) -2) -1) = 0
print $ foldr (-) 6 [3,2,1]
-- (3 -(2 -(1 - 6))) = -4

:{
add1 :: [Int] -> [Int]
add1 lst =
  if null lst
  then []
  else (head lst + 1) : (add1 (tail lst));
--      ^ ((head lst) + 1)
:}
add1 [1,2,3]

:{
recMap :: (a -> b) -> [a] -> [b]
recMap f lst =
  if null lst
  then []
  else (f (head lst)) : (recMap f (tail lst))
:}
recMap (\x -> x+2) [1,2,3]

:{
matchMap :: (a->b)->[a]->[b]
matchMap f [] = []
matchMap f (hd:tl) = (f hd):(matchMap f tl)
:}
matchMap (\x->x*2) [-1,2,3,4]

-- built-in comprehension

[ x*x | x <- [2,3,4,5]]

-- filtering
:{
matchFilter p [] = []
matchFilter p (hd:tl) | p hd = hd:(matchFilter p tl)
                      | otherwise = matchFilter p tl
:}

matchFilter(\x -> (x `rem` 2 == 0)) [1,2,3,4]
--                ^ raw even

{-
Reducing is a special case of folding in which no initial accumulation element is supplied
and the folding operation is an associative, commutative binary operator over a set.
-}

:{
slbreduce op [] = error "no elements"
slbreduce op [x] = x
slbreduce op (x:tl) = op x (slbreduce op tl);
:}
slbreduce (+) [1,2,3]

-- https://matt.might.net/articles/higher-order-list-operations/
