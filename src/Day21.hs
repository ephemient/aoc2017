{-|
Module:         Day21
Description:    <http://adventofcode.com/2017/day/21 Day 21: Fractal Art>
-}
{-# LANGUAGE TupleSections, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day21 (day21, day21a, day21b) where

import Data.List (foldl', transpose)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map.Lazy as Map ((!), assocs, empty, fromList, fromListWith, insert, map, member, unionsWith)
import Data.Map.Lazy (Map)

-- | 8 affine transformations.
transforms :: [[[a]] -> [[a]]]
transforms = [id, f, g, f . g, t, t . f, t . g, t . f . g] where
    f = reverse
    g = map reverse
    t = transpose

-- | Parse a list of enhancements.
parse :: String -> Map [[Bool]] [[Bool]]
parse = Map.fromList . concatMap parseLine . lines where
    parseLine s = let [a, "=>", b] = words s in
      [ (transform a', b')
      | let a' = map (== '#') <$> splitOn "/" a
      , let b' = map (== '#') <$> splitOn "/" b
      , transform <- transforms
      ]

-- | The glider.
start :: [[Bool]]
start = map (== '#') <$> splitOn "/" ".#./..#/###"

-- | Expand sub-squares according to the rules map.
step :: (Ord a) => Map [[a]] [[a]] -> [[a]] -> [[a]]
step rules grid = assemble $ map (rules Map.!) exploded where
    (d, n):_ = [(d, n) | d <- [2..], (n, 0) <- [length grid `divMod` d]]
    exploded = chunksOf d grid >>= transpose . map (chunksOf d)
    assemble parts = chunksOf n parts >>= map concat . transpose

-- | Returns the same valeu for all transformations of the same block.
canonicalize :: (Ord a) => [[a]] -> [[a]]
canonicalize grid = minimum [t grid | t <- transforms]

-- | Returns the number of occurrences of each type of 3x3 block, starting from
-- a 3x3 block, after iterating the rules @3*n@ times.
--step3 :: (Ord a) => Map [[a]] [[a]] -> [[a]] -> Int -> [([[a]], Int)]
step3 rules (canonicalize -> grid) n = iterate expand3 [(grid, 1)] !! n where
    rules3 = step' Map.empty grid
    step' expansions grid
      | Map.member grid expansions = expansions
      | otherwise = foldl' step' (Map.insert grid counts expansions) parts
      where
        parts = chunksOf 3 (iterate (step rules) grid !! 3) >>=
                map canonicalize . transpose . map (chunksOf 3)
        counts = Map.fromListWith (+) $ (, 1) <$> parts
    expand3 counts = Map.assocs $ Map.unionsWith (+)
        [Map.map (* count) $ rules3 Map.! grid | (grid, count) <- counts]

-- | @day21 n input@ returns the number of bits set in the @n@-th 'step' of
-- transforming the 'start' glider using 'parse' rules from @input@.
day21 :: Int -> String -> Int
day21 n (parse -> rules) = sum
  [ count * length (iterate (step rules) grid !! r >>= filter id)
  | (grid, count) <- step3 rules start q
  ] where (q, r) = n `quotRem` 3

day21a :: String -> Int
day21a = day21 5

day21b :: String -> Int
day21b = day21 18
