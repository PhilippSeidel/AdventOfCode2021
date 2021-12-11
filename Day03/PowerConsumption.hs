module Day03.PowerConsumption where
import System.IO
import Data.Char (digitToInt)
import Text.Html (input)

main = do
        handle <- openFile "input" ReadMode
        content <- hGetContents handle
        print $ powerConsumption $ parseContent $ lines content
        hClose handle

parseContent :: [String] -> [[Int]]
parseContent = map (map ((\x -> if x == 0 then -1 else x) . digitToInt))

powerConsumption :: [[Int]] -> Int
powerConsumption input = gamma input * epsilon input

gamma :: [[Int]] -> Int
gamma input = binaryToInt (mostComman (commonalityDistribution input))
            where mostComman = map (\x -> if x < 0 then 0 else 1)

epsilon :: [[Int]] -> Int
epsilon input = binaryToInt (leastComman (commonalityDistribution input))
            where leastComman = map (\x -> if x < 0 then 1 else 0)

commonalityDistribution :: [[Int]] -> [Int]
commonalityDistribution = foldr (zipWith (+)) [0..]

binaryToInt :: [Int] -> Int
binaryToInt [] = 0
binaryToInt binary@(x:xs) = x * 2 ^ (length binary - 1) + binaryToInt xs