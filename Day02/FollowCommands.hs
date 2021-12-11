module Day02.FollowCommands where
import System.IO



main = do
        handle <- openFile "input" ReadMode
        content <- hGetContents handle
        print $ followCommands 0 0 $ concatMap words $ lines content
        hClose handle

followCommands :: Int -> Int -> [String] -> Int
followCommands depth horizontal [] = depth * horizontal
followCommands depth horizontal (command:parameter:rest)
            | command == "forward" = followCommands depth (horizontal + parseToInt parameter) rest
            | command == "up" = followCommands (depth - parseToInt parameter) horizontal rest
            | command == "down" = followCommands (depth + parseToInt parameter) horizontal rest
            | otherwise = error "unknown command"
followCommands _ _ [x] = error "unequal number of commands and parameters"

parseToInt = read::String->Int