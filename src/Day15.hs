{-|
Module:         Day15
Description:    <http://adventofcode.com/2017/day/15 Day 15: Dueling Generators>
-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day15 (day15a, day15b) where

import Data.List (stripPrefix)
import GHC.Prim (Word#, and#, eqWord#, remWord#, timesWord#)
import GHC.Types (Int(I#), Word(W#))

-- | Returns the initial values for generators A and B.
parse :: String -> (# Word#, Word# #)
parse input = (# a, b #) where
    [line1, line2] = lines input
    Just (W# a) = read <$> stripPrefix "Generator A starts with " line1
    Just (W# b) = read <$> stripPrefix "Generator B starts with " line2

-- | One step of generator A.
genA :: Word# -> Word#
genA x = remWord# (timesWord# x 16807##) 2147483647##

-- | One step of generator B.
genB :: Word# -> Word#
genB x = remWord# (timesWord# x 48271##) 2147483647##

-- | One step of both generators A and B.
gen :: (# Word#, Word# #) -> (# Word#, Word# #)
gen (# a, b #) = (# genA a, genB b #)

-- | Step both generators A and B until a multiple of 4 and 8 respectively.
gen' :: (# Word#, Word# #) -> (# Word#, Word# #)
gen' (# a, b #) = (# genA' a, genB' b #) where
    genA' x = if W# (and# y 3##) == 0 then y else genA' y where y = genA x
    genB' x = if W# (and# y 7##) == 0 then y else genB' y where y = genB x

-- | Tests whether the lower 16 bits of each pair item are equal.
eq16 :: (# Word#, Word# #) -> Bool
eq16 (# a, b #) = I# (eqWord# (and# a 65535##) (and# b 65535##)) /= 0

-- | @count n 0 x@ counts how many match 'eq' in
-- @take n [gen x, gen (gen x), ..]@
count :: Int -> Int -> (# Word#, Word# #) -> Int
count 0 k _ = k
count n !k x = count (n - 1) (if eq16 y then k + 1 else k) y where y = gen x

-- | @count' n 0 x@ counts how many match 'eq' in
-- @take n [gen x, gen (gen x), ..]@
count' :: Int -> Int -> (# Word#, Word# #) -> Int
count' 0 k _ = k
count' n !k x = count' (n - 1) (if eq16 y then k + 1 else k) y where y = gen' x

day15a :: String -> Int
day15a input = count 40000000 0 (parse input)

day15b :: String -> Int
day15b input = count' 5000000 0 (parse input)
