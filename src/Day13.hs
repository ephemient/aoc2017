{-|
Module:         Day13
Description:    <http://adventofcode.com/2017/day/13 Day 13: Packet Scanners>
-}
{-# LANGUAGE TransformListComp #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day13 (day13a, day13b) where

import Data.List ((\\))
import GHC.Exts (groupWith, the)

-- | Maps each @x: y@ line in the input to a @(x, y)@ tuple.
parse :: String -> [(Int, Int)]
parse = map parseLine . lines where
    parseLine input = head
        [(d, n) | (d, ':':' ':rest) <- reads input, (n, "") <- reads rest]

-- | @combine (rs1, q1) (rs2, q2)@ returns an @(rs3, lcm q1 q2)@ such that
--
-- prop> and [r3 `mod` q1 `elem` rs1 && r3 `mod` q2 `elem` rs2 | r3 <- rs3]
combine :: (Integral a) => ([a], a) -> ([a], a) -> ([a], a)
combine (rs1, q1) (rs2, q2) = (rs3, q3) where
    q3 = lcm q1 q2
    rs3 = common (broaden rs1 q1) (broaden rs2 q2)
    broaden rs q = [0, q .. q3 - 1] >>= flip map rs . (+)
    common xs@(x:xs') ys@(y:ys') = case compare x y of
        EQ -> x : common xs' ys'
        LT -> common xs' ys
        GT -> common xs ys'
    common _ _ = []

day13a :: String -> Int
day13a input = sum [d * n | (d, n) <- parse input, d `mod` (2 * n - 2) == 0]

day13b :: String -> Int
day13b input = head . fst $ foldl combine ([0], 1)
  [ ([0 .. q - 1] \\ [q - 1 - (t - 1) `mod` q | t <- d], q)
  | (d, n) <- parse input
  , then group by n using groupWith
  , let q = 2 * the n - 2
  ]
