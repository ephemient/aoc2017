{-|
Module:         Day12
Description:    <http://adventofcode.com/2017/day/12 Day 12: Digital Plumber>
-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day12 (day12a, day12b) where

import Data.Char (isDigit)
import Data.List (unfoldr)
import qualified Data.Map.Lazy as Map ((!?), fromList, keysSet)
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set ((\\), empty, insert, member, minView, size)
import Data.Set (Set)

-- | Returns a mapping containing each input line "0 -> 1, 2..".
parse :: String -> Map Int [Int]
parse = Map.fromList . map parseLine . lines where
    parseLine (words -> x : "<->" : xs) = (read x, read . filter isDigit <$> xs)

-- | Returns the set of all nodes reachable from a seed node.
connected :: (Ord a) => Map a [a] -> a -> Set a
connected neighbors = grow Set.empty . (:[]) where
    grow s [] = s
    grow s (x:xs) = grow (Set.insert x s) $ queue ++ xs where
        queue = if Set.member x s then [] else fromMaybe [] $ neighbors Map.!? x

day12a :: String -> Int
day12a = Set.size . flip connected 0 . parse

day12b :: String -> Int
day12b input = length . unfoldr (fmap dropConnected . Set.minView) $
               Map.keysSet neighbors where
    neighbors = parse input
    dropConnected (x, xs) = ((), xs Set.\\ connected neighbors x)
