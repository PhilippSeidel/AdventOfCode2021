module Day01.CountIncreases where
import System.IO

main = do
        handle <- openFile "input" ReadMode
        content <- hGetContents handle
        putStr $ show $ countIncreases 0 $ map (read::String->Int) $ lines content
        hClose handle

countIncreases :: Int -> [Int] -> Int 
countIncreases acc (x:xs)
                | x < head xs = countIncreases (acc + 1) xs
                | otherwise = countIncreases acc xs
countIncreases acc _ = acc