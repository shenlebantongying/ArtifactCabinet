import System.IO
import Control.Monad


main = do
    -- use file handle
    let list = []
    handle <- openFile "test.csv" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        list = f singlewords
    print list
    hClose handle
    -- get whole file
    s <- readFile "test.csv"
    -- print whole
    print s
    -- map each lines
    let lines_of_s = lines s in
        mapM print lines_of_s

f :: [String] -> [Int]
f = map read
