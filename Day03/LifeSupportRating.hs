module Day03.LifeSupportRating where
import System.IO
import Data.Char (digitToInt)

main = do
        handle <- openFile "input" ReadMode
        content <- hGetContents handle
        print (lifeSupportRating (commonalityDistribution $ parseForCommonalityCheck $ lines content) (parseToIntLists $ lines content))
        hClose handle


parseForCommonalityCheck :: [String] -> [[Int]]
parseForCommonalityCheck = map (map ((\x -> if x == 0 then -1 else x) . digitToInt))

parseToIntLists :: [String] -> [[Int]]
parseToIntLists = map (map digitToInt)

lifeSupportRating :: [Int] -> [[Int]] -> Int
lifeSupportRating commonality input = oxygenRating commonality input * co2Rating commonality input

oxygenRating :: [Int] -> [[Int]] -> Int
oxygenRating = rating (map (\x -> if x < 0 then 0 else 1))

co2Rating :: [Int] -> [[Int]] -> Int
co2Rating = rating (map (\x -> if x < 0 then 1 else 0))

rating :: ([Int] -> [Int]) -> [Int] -> ([[Int]] -> Int)
rating f commonality = binaryToInt . getClosestMatch (f commonality) 0 

getClosestMatch :: [Int] -> Int -> [[Int]] -> [Int]
getClosestMatch _ _ [] = error "no input provided"
getClosestMatch _ _ [x] = x
getClosestMatch target pos input = getClosestMatch target (pos + 1) (filter (\x -> target!!pos == x!!pos) input)


commonalityDistribution :: [[Int]] -> [Int]
commonalityDistribution = foldr (zipWith (+)) [0..]

binaryToInt :: [Int] -> Int
binaryToInt = sum . zipWith (*) powersOfTwo . reverse
                where powersOfTwo = map (2^) [0..]