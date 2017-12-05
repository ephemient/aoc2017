{-|
Module:         Day5
Description:    <http://adventofcode.com/2017/day/5 Day 5: A Maze of Twisty Trampolines, All Alike>
-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day5 (day5a, day5b) where

import Control.Monad (join)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Bool (bool)
import Data.Ix (inRange)

-- | At each step, the current item is used as a relative index shift.
steps :: (Int -> Int) -- ^ After each step, mutate the previous item
      -> [Int]        -- ^ Start at index 0
      -> Int          -- ^ Number of steps until the index is out of bounds
steps f jumps = runST $ do
    let bounds = (0, length jumps - 1)
    mem <- newListArray bounds jumps :: ST s (STUArray s Int Int)
    let step ip k | inRange bounds ip = do
            jump <- readArray mem ip
            writeArray mem ip $ f jump
            step (ip + jump) $! k + 1
        step _ k = return k
    step 0 0

day5a :: String -> Int
day5a = steps succ . map read . lines

day5b :: String -> Int
day5b = steps (join $ bool succ pred . (>= 3)) . map read . lines
