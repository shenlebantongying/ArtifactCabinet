import Data.Char

area :: Num a => a -> a
area r = r^2

fib :: (Ord a, Num a, Num p) => a -> p
fib x
    | x < 2 = 1
    | otherwise =  fib(x - 1)+fib(x-2)


main :: IO ()
main = do {
    let x = 2 in
    print 123;
    putStrLn "Hello, World!";
    print (fib 6);
    let word = "hello" in
    putStrLn [Data.Char.toUpper c | c <- word];
    print (area 2)
}
