{-|
Module:         Day15
Description:    <http://adventofcode.com/2017/day/15 Day 15: Dueling Generators>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day15 (day15a, day15b) where

import Data.Word (Word32)
import Data.List (stripPrefix)
import LCGMatches (countMatches, countMatchesMod)

-- | Returns the initial values for generators A and B.
parse :: String -> (Word32, Word32)
parse input = (a, b) where
    [line1, line2] = lines input
    Just a = read <$> stripPrefix "Generator A starts with " line1
    Just b = read <$> stripPrefix "Generator B starts with " line2

day15a :: String -> Int
day15a = countMatches 40000000 . parse

day15b :: String -> Int
day15b = countMatchesMod 5000000 . parse
