{-|
Module:         Day15
Description:    <http://adventofcode.com/2017/day/15 Day 15: Dueling Generators>
-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day15 (day15a, day15b) where

import Control.Arrow ((***))
import Data.Bits ((.&.))
import Data.Function (on)
import Data.List (stripPrefix)
import Data.Word (Word64)

-- | Returns the initial values for generators A and B.
parse :: String -> (Word64, Word64)
parse input = (read a, read b) where
    [line1, line2] = lines input
    Just a = stripPrefix "Generator A starts with " line1
    Just b = stripPrefix "Generator B starts with " line2

-- | One step of generator A.
genA :: Word64 -> Word64
genA x = x * 16807 `mod` 2147483647

-- | One step of generator B.
genB :: Word64 -> Word64
genB x = x * 48271 `mod` 2147483647

-- | Step generator A until a multiple of 4.
genA' :: Word64 -> Word64
genA' x = let y = genA x in if y .&. 3 == 0 then y else genA' y

-- | Step generator A until a multiple of 8.
genB' :: Word64 -> Word64
genB' x = let y = genB x in if y .&. 7 == 0 then y else genB' y

-- | One step of both generators A and B.
gen :: (Word64, Word64) -> (Word64, Word64)
gen = genA *** genB

-- | Step both generators A and B until a multiple of 4 and 8 respectively.
gen' :: (Word64, Word64) -> (Word64, Word64)
gen' = genA' *** genB'

-- | prop> count p f x n == length (filter p . take n . tail $ iterate f x)
count :: (a -> Bool) -> (a -> a) -> a -> Int -> Int
count p f = count' 0 where
    count' !k x 0 = k
    count' !k x n = count' (if p y then k + 1 else k) y (n - 1) where y = f x

day15a :: String -> Int
day15a input =
    count (uncurry ((==) `on` (.&. 0xffff))) gen (parse input) 40000000

day15b :: String -> Int
day15b input =
    count (uncurry ((==) `on` (.&. 0xffff))) gen' (parse input) 5000000
