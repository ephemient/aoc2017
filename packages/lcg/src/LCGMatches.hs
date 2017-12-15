{-|
Module:         LCGMatches
Description:    Count matches between 'minstdRand0' and 'minstdRand'
-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module LCGMatches (countMatches, countMatchesMod) where

import GHC.Prim (Word#, and#, eqWord#, remWord#, timesWord#)
import GHC.Types (Int(I#), Word(W#))
import GHC.Word (Word32(W32#))
import LinearCongruentialGenerator (next32#)

genA, genB :: Word# -> Word#
genA = next32# (# 16807##, 2147483647## #)
genB = next32# (# 48271##, 2147483647## #)

gen, gen' :: (# Word#, Word# #) -> (# Word#, Word# #)
gen (# a, b #) = (# genA a, genB b #)
gen' (# a, b #) = (# genA' a, genB' b #) where
    genA' x = if W# (and# y 3##) == 0 then y else genA' y where y = genA x
    genB' x = if W# (and# y 7##) == 0 then y else genB' y where y = genB x

eq16 :: (# Word#, Word# #) -> Bool
eq16 (# a, b #) = I# (eqWord# (and# a 65535##) (and# b 65535##)) /= 0

count, count' :: Int -> Int -> (# Word#, Word# #) -> Int
count 0 k _ = k
count n !k x = count (n - 1) (if eq16 y then k + 1 else k) y where y = gen x
count' 0 k _ = k
count' n !k x = count' (n - 1) (if eq16 y then k + 1 else k) y where y = gen' x

countMatches, countMatchesMod :: Int -> (Word32, Word32) -> Int
countMatches n (W32# a, W32# b) = count n 0 (# a, b #)
countMatchesMod n (W32# a, W32# b) = count' n 0 (# a, b #)
