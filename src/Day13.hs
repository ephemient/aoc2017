{-|
Module:         Day13
Description:    <http://adventofcode.com/2017/day/13 Day 13: Packet Scanners>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day13 (day13a, day13b) where

-- | Maps each @x: y@ line in the input to a @(x, y)@ tuple.
parse :: String -> [(Int, Int)]
parse = map parseLine . lines where
    parseLine input = head
        [(d, n) | (d, ':':' ':rest) <- reads input, (n, "") <- reads rest]

day13a :: String -> Int
day13a input = sum [d * n | (d, n) <- parse input, d `mod` (2 * n - 2) == 0]

day13b :: String -> Int
day13b input = head [i | i <- [0..], not $ caught i] where
    caught i = or [(d + i) `mod` (2 * n - 2) == 0 | (d, n) <- parse input]
