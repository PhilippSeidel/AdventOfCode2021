module Day01.CountIncreases where
import System.IO

main = do
        handle <- openFile "input" ReadMode
        content <- hGetContents handle
        print $ countIncreases 0 $ map (read::String->Int) $ lines content
        hClose handle

countIncreases :: Int -> [Int] -> Int 
countIncreases acc [] = acc
countIncreases acc [x] = acc
countIncreases acc (x:rest@(y:xs))
                | x < y = countIncreases (acc + 1) rest
                | otherwise = countIncreases acc rest