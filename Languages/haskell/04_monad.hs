
-- from kfisher's slides
echoDup :: IO ()
echoDup = getChar >>= (\c -> putChar c >>= (\() -> putChar c))

-- use of >>
echoDup' = getChar >>= (\c -> putChar c >> putChar c)

-- get two char by using return

obtainTwoChars = getChar >>= (\c1 -> getChar >>= (\c2 -> putChar '\n' >> return (c1, c2)))

-- "do" Notation

obTwoChars = do {
    c1 <- getChar;
    c2 <- getChar;

    return (c1,c2)
}


-- For loop

mfor :: [a] -> (a -> IO b) -> IO ()
mfor [] fa = return ()
mfor (x:xs) fa = fa x >> mfor xs fa

-- mfor [1..10] (\x -> putStr (show x))
