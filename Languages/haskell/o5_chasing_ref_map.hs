import qualified Data.Map.Strict as Map


{- Say we want a function that uses a key to look up a value, then treat that
value as another key to look up a third key, which we look up and return, e.g., -}
aMap :: Map.Map String String
aMap = Map.fromList [("1","2"),("2","3"),("3","4")]

-- >>> Map.lookup "1" aMap
-- Just "2"

{- Simple version -}

lookup3 :: Ord k => k -> Map.Map k k -> Maybe k
lookup3 k1 m = (helper . helper . helper) (Just k1)
    where helper Nothing = Nothing
          helper (Just k) = Map.lookup k m
-- TODO -> how to implement this recrusively?

-- >>> lookup3 "1" aMap
-- Just "4"

-- >>> (succ . succ . succ) 1
-- 4
