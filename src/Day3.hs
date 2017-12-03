{-|
Module:         Day3
Description:    <http://adventofcode.com/2017/day/3 Day 3: Spiral Memory>
-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day3 (day3a, day3b) where

import Data.List (mapAccumL, scanl')
import qualified Data.Map.Strict as Map (insert, lookup, singleton)
import Data.Maybe (mapMaybe)

day3a :: String -> Int
day3a (read -> n) = a + abs ((n - b) `mod` (2 * a) - a) where
    (a, b) = last $ takeWhile ((< n) . snd) $ zip [1..] $ scanl' (+) 1 [8, 16..]

day3b :: String -> Int
day3b (read -> n) = head $ filter (> n) values where
    (_, values) = mapAccumL next (Map.singleton (0, 0) 1) positions
    next m pos = (Map.insert pos n m, n) where
        n = sum . mapMaybe ((`Map.lookup` m) . add pos) $ take 8 positions
    _:positions = scanl add (0, 0) $ do
        n <- [0, 2..]
        replicate (n - 1) (0, -1) ++
            replicate n (-1, 0) ++
            replicate n (0, 1) ++
            replicate (n + 1) (1, 0)
    add (a, b) (c, d) = (a + c, b + d)
