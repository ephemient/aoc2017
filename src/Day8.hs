{-|
Module:         Day8
Description:    <http://adventofcode.com/2017/day/8 Day 8: I Heard You Like Registers>
-}
{-# LANGUAGE PatternGuards, RecordWildCards #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day8 (day8a, day8b) where

import Data.List (foldl', scanl')
import qualified Data.Map.Strict as Map (elems, fromList, insert, lookup)
import Data.Map.Strict (Map)

-- | A conditional register instruction.
data Instruction reg val = Instruction
  { target :: reg        -- ^ The register to be manipulated.
  , op :: val -> val     -- ^ The operation to apply to /target/.
  , cond :: val -> Bool  -- ^ The condition on this instruction.
  , when :: reg          -- ^ The register for /cond/ evaluation.
  }

-- | Parses a string into a sequence of instructions.
parse :: String -> [Instruction String Int]
parse = map (line . words) . lines where
    line [target, op, val, "if", when, cond, cmp] = Instruction
        {op = p op `flip` read val, cond = q cond `flip` read cmp, ..}
    p "inc" = (+); p "dec" = (-); q "<" = (<); q "<=" = (<=)
    q "==" = (==); q ">=" = (>=); q ">" = (>); q "!=" = (/=)

-- | Returns an initial state with all known registered set to a constant value.
initialize :: (Ord reg) => val -> [Instruction reg val] -> Map reg val
initialize zero instructions = Map.fromList
    [(reg, zero) | reg <- map target instructions ++ map when instructions]

-- | Performs one operation on a register state, if the condition applies.
exec :: (Ord reg, Num val) => Map reg val -> Instruction reg val -> Map reg val
exec regs Instruction {..}
  | Just True <- cond <$> Map.lookup when regs
  , Just val <- Map.lookup target regs
  = Map.insert target (op val) regs
  | otherwise = regs

day8a :: String -> Int
day8a = maximum . Map.elems . (foldl' exec =<< initialize 0) . parse

day8b :: String -> Int
day8b = maximum . concatMap Map.elems . (scanl' exec =<< initialize 0) . parse
