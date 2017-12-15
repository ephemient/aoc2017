{-|
Module:         Day15
Description:    <http://adventofcode.com/2017/day/15 Day 15: Dueling Generators>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day15 (day15a, day15b) where

import Data.Bits ((.&.))
import Data.List (stripPrefix)

-- | Returns the initial values for generators A and B.
parse :: String -> (Int, Int)
parse input = (read a, read b) where
    [line1, line2] = lines input
    Just a = stripPrefix "Generator A starts with " line1
    Just b = stripPrefix "Generator B starts with " line2

day15a :: String -> Int
day15a input = length . filter id . take 40000000 $ zipWith (==) a b where
    (a0, b0) = parse input
    a = map (.&. 0xffff) $ iterate (\x -> x * 16807 `mod` 2147483647) a0
    b = map (.&. 0xffff) $ iterate (\x -> x * 48271 `mod` 2147483647) b0

day15b :: String -> Int
day15b input = length . filter id . take 5000000 $ zipWith (==) a b where
    (a0, b0) = parse input
    a = map (.&. 0xffff) . filter ((== 0) . (`mod` 4)) $
        iterate (\x -> x * 16807 `mod` 2147483647) a0
    b = map (.&. 0xffff) . filter ((== 0) . (`mod` 8)) $
        iterate (\x -> x * 48271 `mod` 2147483647) b0
