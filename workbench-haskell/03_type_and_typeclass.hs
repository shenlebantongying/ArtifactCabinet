-- algebraic data types (ADT)

data BaseShape = Circle Float
            | Trapezoid Float Float Float

surface :: BaseShape -> Float
surface (Circle r) = pi * r ^2
surface (Trapezoid a b h) = (a + b) * (/) h 2
--                                    ^ float div
-- >>> surface $ Trapezoid 1.0 2.0 3.0
-- 4.5

-- Named ADT

data Point = Point {
    x :: Float,
    y :: Float
} deriving (Show)

data Circ = Circ {
    center::Point,
    r:: Float
} deriving (Show)

data Sqr = Sqr {
    pa::Point,
    pb::Point
} deriving (Show)

-- >>> Sqr { pa = Point {x=1.0,y=2.0}, pb = Point {x=1.0,y=2.0} }
-- Sqr {pa = Point {x = 1.0, y = 2.0}, pb = Point {x = 1.0, y = 2.0}}

-- TODO: compositing records?
-- data Sap =  Circ | Sqr


-- Type synonyms (TYPE)

type PhoneBook = [(String, String)]
inPhoneBook :: String -> String -> PhoneBook -> Bool 
inPhoneBook name pnum pbook = (name, pnum) `elem` pbook
-- >>> :t PhoneBook
-- Data constructor not in scope: PhoneBook

iBook :: [(String, String)]
iBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]  

-- >>> inPhoneBook "wendy" "939-8282" iBook
-- True

