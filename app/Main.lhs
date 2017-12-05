# [Advent of Code 2017](http://adventofcode.com/2017)
### my answers
~~~ {.haskell}
module Main (main) where
~~~

## [Day 1: Inverse Captcha](/src/Day1.hs)
~~~ {.haskell}
import Day1 (day1a, day1b)
~~~
## [Day 2: Corruption Checksum](/src/Day2.hs)
~~~ {.haskell}
import Day2 (day2a, day2b)
~~~
## [Day 3: Spiral Memory](/src/Day3.hs)
~~~ {.haskell}
import Day3 (day3a, day3b)
~~~
## [Day 4: High-Entropy Passphrases](/src/Day4.hs)
~~~ {.haskell}
import Day4 (day4a, day4b)
~~~
## [Day 5: A Maze of Twisty Trampolines, All Alike](/src/Day5.hs)
~~~ {.haskell}
import Day5 (day5a, day5b)
~~~

---

~~~ {.haskell}
import Paths_aoc2017 (getDataFileName)

getDayInput :: Int -> IO String
getDayInput i = do
    putStrLn $ "Day " ++ show i
    getDataFileName ("day" ++ show i ++ ".txt") >>= readFile

readDayInput :: (Read a) => Int -> IO a
readDayInput = fmap read . getDayInput

run :: (b -> IO ()) -> [a -> b] -> a -> IO ()
run showIO funcs contents = do
    mapM_ (showIO . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
    getDayInput 1 >>= run print [day1a, day1b]
    getDayInput 2 >>= run print [day2a, day2b]
    readDayInput 3 >>= run print [day3a, day3b]
    getDayInput 4 >>= run print [day4a, day4b]
    getDayInput 5 >>= run print [day5a, day5b]
~~~
