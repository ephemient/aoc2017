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
stack test aoc2017:test:aoc2017-test
```

Run [criterion](http://www.serpentine.com/criterion/) benchmarks:

```sh
stack bench aoc2017:bench:aoc2017-bench
```

Print solutions for the inputs provided in local data files:

```sh
stack build aoc2017:exe:aoc2017-exe
stack exec aoc2017-exe
```

Animate the Day 11 path (rendered at
[Gyfcat](https://gfycat.com/UnnaturalSourFiddlercrab)):

```sh
stack build aoc2017:exe:aoc2017-day11
stack exec aoc2017-day11
ffmpeg -vf lavfi -i nullsrc=s=$(identify day11-0000.png | cut -d' ' -f3):d=30 -framerate 300 -i 'day11-%04d.png' -vf '[0:v][1:v]overlay[video]' -map '[video]' -r 60 -c:v libx264 -pix_fmt yuv420p -profile:v baseline -level 3.0 -movflags +faststart -y day11.mp4
```

Animate the Day 14 grid (rendered at
[Gyfcat](https://gfycat.com/AdventurousUnconsciousKarakul)):

```sh
stack build aoc2017:exe:aoc2017-day14
stack exec aoc2017-day14
ffmpeg -framerate 60 -i 'day11-%04d.png' -c:v libx264 -pix_fmt yuv420p -profile:v baseline -level 3.0 -movflags +faststart -y day11.mp4
```

Generate [Haddock](https://www.haskell.org/haddock/) API documentation
(rendered at [ephemient.github.io/aoc2017](https://ephemient.github.io/aoc2017)):

```sh
stack haddock aoc2017:lib
```

---

<!--
```haskell
{-# LANGUAGE NondecreasingIndentation #-}
module Main (main) where
```
-->

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
## [Day 10: Knot Hash](/src/Day10.hs)
```haskell
import Day10 (day10a, day10b)
```
## [Day 11: Hex Ed](/src/Day11.hs)
```haskell
import Day11 (day11a, day11b)
```
## [Day 12: Digital Plumber](/src/Day12.hs)
```haskell
import Day12 (day12a, day12b)
```
## [Day 13: Packet Scanners](/src/Day13.hs)
```haskell
import Day13 (day13a, day13b)
```
## [Day 14: Disk Defragmentation](/src/Day14.hs)
```haskell
import Day14 (day14a, day14b)
```
## [Day 15: Dueling Generators](/src/Day15.hs)
```haskell
import Day15 (day15a, day15b)
```

---

```haskell
import Control.Monad (when)
import Data.Maybe (mapMaybe)
import Paths_aoc2017 (getDataFileName)
import System.Environment (getArgs)
import Text.Read (readMaybe)

getDayInput :: Int -> IO String
getDayInput i = getDataFileName ("day" ++ show i ++ ".txt") >>= readFile

readDayInput :: (Read a) => Int -> IO a
readDayInput = fmap read . getDayInput

maybeBottom :: (a -> String) -> Maybe a -> String
maybeBottom = maybe "(⊥)"

showError :: (Show a) => (b -> String) -> Either a b -> String
showError = either (\err -> "(" ++ show err ++ ")")

run :: Int -> (Int -> IO a) -> (b -> IO ()) -> [a -> b] -> IO ()
run day readIO showIO funcs = do
    days <- mapMaybe readMaybe <$> getArgs
    when (null days || day `elem` days) $ do
    putStrLn $ "Day " ++ show day
    contents <- readIO day
    mapM_ (showIO . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
    run 1 getDayInput print [day1a, day1b]
    run 2 getDayInput print [day2a, day2b]
    run 3 readDayInput print [day3a, day3b]
    run 4 getDayInput print [day4a, day4b]
    run 5 getDayInput print [day5a, day5b]
    run 6 getDayInput (putStrLn . maybeBottom show) [day6a, day6b]
    run 7 getDayInput (putStrLn . maybeBottom id) [day7a, fmap show . day7b]
    run 8 getDayInput print [day8a, day8b]
    run 9 getDayInput print [day9a, day9b]
    run 10 getDayInput putStrLn [show . day10a 256, day10b]
    run 11 getDayInput print [day11a, day11b]
    run 12 getDayInput print [day12a, day12b]
    run 13 getDayInput print [day13a, day13b]
    run 14 getDayInput print [day14a, day14b]
    run 15 getDayInput print [day15a, day15b]
```
