{-|
Module:         Day6
Description:    <http://adventofcode.com/2017/day/6 Day 6: Memory Reallocation>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day6 (day6a, day6b) where

import Control.Arrow (second)
import Control.Monad (msum)
import Data.Array.IArray (IArray, array, assocs, bounds, listArray)
import Data.Array.Unboxed (UArray)
import Data.Ix (Ix, index, rangeSize)
import Data.List (genericLength, genericSplitAt, maximumBy)
import qualified Data.Map.Strict as Map (empty, insert, lookup)
import Data.Maybe (catMaybes, listToMaybe)
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

-- | The indices of the first two duplicated elements in a list.
indexDup :: (Ord a) => [a] -> Maybe (Int, Int)
indexDup l = msum . zipWith (fmap . (,)) [0..] . zipWith Map.lookup l .
             scanl (flip ($)) Map.empty $ zipWith Map.insert l [0..]

day6a :: String -> Maybe Int
day6a = fmap fst . indexDup . iterate redistribute . parse @UArray @Int

day6b :: String -> Maybe Int
day6b = fmap (uncurry (-)) . indexDup . iterate redistribute . parse @UArray @Int
