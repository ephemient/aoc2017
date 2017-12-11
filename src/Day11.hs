{-|
Module:         Day11
Description:    <http://adventofcode.com/2017/day/11 Day 11: Hex Ed>
-}
{-# LANGUAGE BangPatterns, NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day11 (day11a, day11b) where

import Data.Char (toUpper)
import Data.List (foldl', scanl')

-- | A single step on a hexagonal grid.
data Step = N | NE | SE | S | SW | NW deriving Read

-- | A position on a hexagonal grid.
data Pos = Pos
  { x :: !Int   -- ^ The horizontal offset from the origin.
  , y :: !Int   -- ^ The vertical offset from the origin.
  }             -- ^ Invariant: @x@ and @y@ must both be even or both be odd.

-- | Parse comma-separated steps.
parse :: String -> [Step]
parse s = read $ '[' : map toUpper s ++ "]"

-- | Move from a position by one step.
step :: Pos -> Step -> Pos
step p@Pos {y} N = p {y = y + 2}
step Pos {..} NE = Pos (x + 1) (y + 1)
step Pos {..} SE = Pos (x + 1) (y - 1)
step p@Pos {y} S = p {y = y - 2}
step Pos {..} SW = Pos (x - 1) (y - 1)
step Pos {..} NW = Pos (x - 1) (y + 1)

-- | Returns the minimum number of steps required to reach the origin.
walk :: Pos -> Int
walk = walk' 0 where
    walk' !k (Pos 0 0) = k
    walk' !k (Pos x 0) = k + 2 * abs x
    walk' !k (Pos 0 y) = k + abs y `div` 2
    walk' !k Pos {..} = walk' (k + d) $ Pos x' y' where
        d = min (abs x) (abs y + 1)
        x' = if x > 0 then x - d else x + d
        y' = if y > 0 then y - d else y + d

day11a :: String -> Int
day11a = walk . foldl' step (Pos 0 0) . parse

day11b :: String -> Int
day11b = maximum . map walk . scanl' step (Pos 0 0) . parse
