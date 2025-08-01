class MyClass a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    (/=) x y = (x Main.== y)

data MyData = Zero | Succ MyData deriving (Show)

instance MyClass MyData where
    (==) Zero Zero = True
    (==) (Succ x) (Succ y) = (x Main.== y)
    (==) _ _ = False

-- >>> Succ (Succ Zero) Main.== Succ (Succ Zero)
-- True
