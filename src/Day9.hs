{-|
Module:         Day9
Description:    <http://adventofcode.com/2017/day/9 Day 9: Stream Processing>
-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day9 (day9a, day9b) where

import Data.List (mapAccumL)

day9a :: String -> Int
day9a = sum . snd . mapAccumL accum (1, Nothing) where
    accum (k, Nothing) '{' = ((k + 1, Nothing), k)
    accum (k, Nothing) '}' = ((k - 1, Nothing), 0)
    accum (k, Nothing) '<' = ((k, Just False), 0)
    accum (k, Just False) '>' = ((k, Nothing), 0)
    accum (k, Just False) '!' = ((k, Just True), 0)
    accum (k, Just True) _ = ((k, Just False), 0)
    accum acc _ = (acc, 0)

day9b :: String -> Int
day9b = sum . snd . mapAccumL accum Nothing where
    accum Nothing '<' = (Just True, 0)
    accum Nothing _ = (Nothing, 0)
    accum (Just True) '>' = (Nothing, 0)
    accum (Just True) '!' = (Just False, 0)
    accum (Just True) _ = (Just True, 1)
    accum (Just False) _ = (Just True, 0)
