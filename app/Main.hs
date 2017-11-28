module Main (main) where

import Paths_aoc2017 (getDataFileName)

run :: Int -> [String -> String] -> IO ()
run i funcs = do
    putStrLn $ "Day " ++ show i
    contents <- getDataFileName ("day" ++ show i ++ ".txt") >>= readFile
    mapM_ (putStrLn . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = return ()
