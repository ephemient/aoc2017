{-|
Module:         Day8
Description:    <http://adventofcode.com/2017/day/8 Day 8: I Heard You Like Registers>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day8 (day8a, day8b) where

import Data.List (foldl', scanl')
import qualified Data.Map.Strict as Map (elems, empty, insert, lookup)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)

-- | Performs one operation on a register state, if the condition applies.
exec :: (Num a, Ord a, Read a) => Map String a -> String -> Map String a
exec regs line
  | q cond (fromMaybe 0 $ Map.lookup when regs) (read cmp)
  = Map.insert reg (p op (fromMaybe 0 $ Map.lookup reg regs) (read val)) regs
  | otherwise = regs where
    [reg, op, val, "if", when, cond, cmp] = words line
    p "inc" = (+); p "dec" = (-); q "<" = (<); q "<=" = (<=)
    q "==" = (==); q ">=" = (>=); q ">" = (>); q "!=" = (/=)

day8a :: String -> Int
day8a = maximum . Map.elems . foldl' exec Map.empty . lines

day8b :: String -> Int
day8b = maximum . concatMap Map.elems . scanl' exec Map.empty . lines
