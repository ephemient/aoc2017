{-|
Module:         Day6
Description:    <http://adventofcode.com/2017/day/6 Day 6: Memory Reallocation>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day6 (day6a, day6b) where

import Control.Arrow (second)
import Control.Monad (ap)
import Data.Array.IArray (IArray, array, assocs, bounds, listArray)
import Data.Array.Unboxed (UArray)
import Data.Ix (Ix, index, rangeSize)
import Data.List (find, genericLength, genericSplitAt, inits, maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (Down(Down), comparing)
import Data.Tuple (swap)

-- | Reads the words of a string to an array.
parse :: (IArray a e, Ix i, Num i, Read e) => String -> a i e
parse s = let l = map read $ words s in listArray (0, genericLength l - 1) l

-- | Returns the index and value of first maximum in an array.
top :: (IArray a e, Ix i, Ord e) => a i e -> (i, e)
top = maximumBy (comparing $ second Down . swap) . assocs

-- | Removes the first maximum and redistributes its value throughout the array.
redistribute :: (IArray a i, Integral i, Ix i) => a i i -> a i i
redistribute arr =
    let b = bounds arr
        (i, v) = top arr
        (q, r) = v `quotRem` fromIntegral (rangeSize b)
        (before, _:after) = splitAt (index b i) $ assocs arr
        (up, down) = genericSplitAt r $ after ++ before ++ [(i, 0)]
    in array b $ map (second (q + 1 +)) up ++ map (second (q +)) down

-- | The longest initial sublist with no duplicates.
takeUnique :: (Eq a) => [a] -> [a]
takeUnique = map fst . takeWhile (not . uncurry elem) . ap zip inits

-- | The first sublist /[a, b, ..]/ which is immediately followed by /a/.
findCycle :: (Eq a) => [a] -> Maybe [a]
findCycle = find (not . null) . ap (zipWith $ dropWhile . (/=)) inits

day6a :: String -> Int
day6a = length . takeUnique . iterate redistribute . parse @UArray @Int

day6b :: String -> Int
day6b = length . fromJust . findCycle . iterate redistribute . parse @UArray @Int
