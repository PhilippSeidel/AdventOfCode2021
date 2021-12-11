module Day02.FollowCommands2 where
import System.IO


main = do
        handle <- openFile "input" ReadMode
        content <- hGetContents handle
        print $ followCommands 0 0 0 $ concatMap words $ lines content
        hClose handle

followCommands :: Int -> Int -> Int -> [String] -> Int
followCommands aim depth horizontal [] = depth * horizontal
followCommands aim depth horizontal (command:parameter:rest)
            | command == "forward" = followCommands aim (depth + aim * parseToInt parameter) (horizontal + parseToInt parameter) rest
            | command == "up" = followCommands (aim - parseToInt parameter) depth horizontal rest
            | command == "down" = followCommands (aim + parseToInt parameter) depth horizontal rest
            | otherwise = error "unknown command"
followCommands _ _ _ [x] = error "unequal number of commands and parameters"

parseToInt = read::String->Int