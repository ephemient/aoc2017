{-|
Module:         Day5
Description:    <http://adventofcode.com/2017/day/5 Day 5: A Maze of Twisty Trampolines, All Alike>
-}
{-# LANGUAGE BangPatterns, PatternGuards, TupleSections #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day5 (day5a, day5b) where

import Control.Monad (join)
import Data.Bool (bool)

-- | At each step, the current item is used as a relative index shift.
steps :: (Int -> Int) -- ^ After each step, mutate the previous item
      -> [Int]        -- ^ Start at index 0
      -> Int          -- ^ Number of steps until the index is out of bounds
steps f = steps' 0 . ([],) where
    steps' !k (before, a:as)
      | a >= 0, (bs, after') <- splitAt a after
      = steps' (k + 1) (reverse bs ++ before, after')
      | a < 0, (as, a':before') <- splitAt (-a - 1) before
      = steps' (k + 1) (before', a' : reverse as ++ after)
      where after = f a : as
    steps' k _ = k

day5a :: String -> Int
day5a = steps succ . map read . lines

day5b :: String -> Int
day5b = steps (join $ bool succ pred . (>= 3)) . map read . lines
