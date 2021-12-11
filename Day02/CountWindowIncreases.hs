module Day02.CountWindowIncreases where
import System.IO


main = do
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        print $ countWindowIncreases 0 (maxBound :: Int) $ map (read::String->Int) $ lines contents
        hClose handle

countWindowIncreases :: Int -> Int -> [Int] -> Int 
countWindowIncreases acc last [] = acc
countWindowIncreases acc last [x] = acc
countWindowIncreases acc last (x:rest@(y:z:xs))
                            | sum [x,y,z] > last = countWindowIncreases (acc + 1) (sum [x,y,z]) rest
                            | otherwise = countWindowIncreases acc (sum [x,y,z]) rest
countWindowIncreases acc last (x:y:xs) = acc
