{-|
Module:         Day17
Description:    <http://adventofcode.com/2017/day/17 Day 17: Spinlock>
-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day17 (day17a, day17b) where

import Data.Bool (bool)
import Data.List (foldl')
import qualified Data.Sequence as Seq (index, insertAt, length, singleton)

day17a :: Int -> Int
day17a step = Seq.index s $ (i + 1) `mod` Seq.length s where
    (i, s) = foldl' insert (0, Seq.singleton 0) [1..2017]
    insert (pos, s) value = (pos', Seq.insertAt pos' value s) where
        pos' = (pos + step) `mod` Seq.length s + 1

day17b :: Int -> Int
day17b step = insert 0 1 0 where
    insert pos len !next
      | len' > 50000000 = next
      | otherwise = insert pos' len' . bool next (len + q) $ pos' == 1 where
        q = (len - pos) `div` (step + 1)
        pos' = (pos + (q + 1) * (step + 1) - 1) `mod` (len + q) + 1
        len' = len + q + 1
