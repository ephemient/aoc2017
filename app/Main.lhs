# [Advent of Code 2017](http://adventofcode.com/2017)
### my answers in [Haskell](https://www.haskell.org/)

This project builds with [The Haskell Tool Stack](https://haskellstack.org/).

Setup:

```sh
curl -sSL https://get.haskellstack.org/ | sh -s -
stack setup
```

> If you encounter
> [linker errors](https://github.com/commercialhaskell/stack/issues/2712) with
> the commands below, try `stack setup --ghc-build=nopie` or adding
>
> ```yaml
> ghc-build: nopie
> ```
>
> to `~/.stack/config.yaml`.

Run the [HSpec](https://hspec.github.io/) test suite:

```sh
stack test
```

Run [criterion](http://www.serpentine.com/criterion/) benchmarks:

```sh
stack bench
```

Print solutions for the inputs provided in local data files:

```sh
stack build
stack exec aoc2017-exe
```

Generate [Haddock](https://www.haskell.org/haddock/) API documentation
(rendered at [ephemient.github.io/aoc2017](https://ephemient.github.io/aoc2017)):

```sh
stack haddock
```

---

```haskell
module Main (main) where
```

## [Day 1: Inverse Captcha](/src/Day1.hs)
```haskell
import Day1 (day1a, day1b)
```
## [Day 2: Corruption Checksum](/src/Day2.hs)
```haskell
import Day2 (day2a, day2b)
```
## [Day 3: Spiral Memory](/src/Day3.hs)
```haskell
import Day3 (day3a, day3b)
```
## [Day 4: High-Entropy Passphrases](/src/Day4.hs)
```haskell
import Day4 (day4a, day4b)
```
## [Day 5: A Maze of Twisty Trampolines, All Alike](/src/Day5.hs)
```haskell
import Day5 (day5a, day5b)
```
## [Day 6: Memory Reallocation](/src/Day6.hs)
```haskell
import Day6 (day6a, day6b)
```
## [Day 7: Recursive Circus](/src/Day7.hs)
```haskell
import Day7 (day7a, day7b)
```
## [Day 8: I Heard You Like Registers](/src/Day8.hs)
```haskell
import Day8 (day8a, day8b)
```
## [Day 9: Stream Processing](/src/Day9.hs)
```haskell
import Day9 (day9a, day9b)
```

---

```haskell
import Paths_aoc2017 (getDataFileName)

getDayInput :: Int -> IO String
getDayInput i = do
    putStrLn $ "Day " ++ show i
    getDataFileName ("day" ++ show i ++ ".txt") >>= readFile

readDayInput :: (Read a) => Int -> IO a
readDayInput = fmap read . getDayInput

maybeBottom :: (a -> String) -> Maybe a -> String
maybeBottom = maybe "(⊥)"

showError :: (Show a) => (b -> String) -> Either a b -> String
showError = either (\err -> "(" ++ show err ++ ")")

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
    getDayInput 6 >>= run (putStrLn . maybeBottom show) [day6a, day6b]
    getDayInput 7 >>= run (putStrLn . maybeBottom id) [day7a, fmap show . day7b]
    getDayInput 8 >>= run print [day8a, day8b]
    getDayInput 9 >>= run print [day9a, day9b]
```
