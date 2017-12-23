{-|
Module:         Day21
Description:    <http://adventofcode.com/2017/day/21 Day 21: Fractal Art>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day21 (day21, day21a, day21b) where

import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map.Lazy as Map ((!), fromList)
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

-- | @day21 n input@ returns the number of bits set in the @n@-th 'step' of
-- transforming the 'start' glider using 'parse' rules from @input@.
day21 :: Int -> String -> Int
day21 n input =
    length . filter id . concat $ iterate (step $ parse input) start !! n

day21a :: String -> Int
day21a = day21 5

day21b :: String -> Int
day21b = day21 18
