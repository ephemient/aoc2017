# [Advent of Code 2017](http://adventofcode.com/2017)
### my answers
~~~ {.haskell}
module Main (main) where
~~~

## [Day 1: Inverse Captcha](/src/Day1.hs)
~~~ {.haskell}
import Day1 (day1a, day1b)
~~~
<!--
~~~ {.haskell}
import Paths_aoc2017 (getDataFileName)

run :: Int -> [String -> String] -> IO ()
run i funcs = do
    putStrLn $ "Day " ++ show i
    contents <- getDataFileName ("day" ++ show i ++ ".txt") >>= readFile
    mapM_ (putStrLn . ($ contents)) funcs
    putStrLn ""
~~~
-->

---

~~~ {.haskell}
main :: IO ()
main = do
    run 1 [show . day1a, show . day1b]
~~~
