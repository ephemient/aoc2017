{-|
Module:         Day11
Description:    <http://adventofcode.com/2017/day/11 Day 11: Hex Ed>
-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day11 (day11a, day11b, Chart(..), chart) where

import Data.Char (toUpper)
import Data.List (foldl', scanl')

-- | A single step on a hexagonal grid.
data Step = N | NE | SE | S | SW | NW deriving Read

-- | A position on a hexagonal grid, in trapezoidal coordinates.
data Pos = Pos
  { x :: !Int   -- ^ The distance along the NE/SW axis.
  , y :: !Int   -- ^ The distance along the NW/SE axis.
  }             -- ^ The distance along the N/S axis is given by @x + y@.

-- | A representation of all hexagons touched along a path.
data Chart = Chart
  { bounds :: ((Int, Int), (Int, Int)) -- ^ Cartesian bounding box.
  , maxDistance :: Int                 -- ^ The maximum distance from origin.
  , path :: [((Int, Int), Int)]        -- ^ Hexagons and distances from origin.
  }

-- | Parse comma-separated steps.
parse :: String -> [Step]
parse s = read $ '[' : map toUpper s ++ "]"

-- | Move from a position by one step.
step :: Pos -> Step -> Pos
step Pos {..} N  = Pos (x + 1) (y + 1)
step Pos {..} NE = Pos (x + 1)  y
step Pos {..} SE = Pos  x      (y - 1)
step Pos {..} S  = Pos (x - 1) (y - 1)
step Pos {..} SW = Pos (x - 1)  y
step Pos {..} NW = Pos  x      (y + 1)

-- | Returns the minimum number of steps required to reach the origin.
walk :: Pos -> Int
walk Pos {..} = (abs x + abs y + abs (x - y)) `div` 2

day11a :: String -> Int
day11a = walk . foldl' step (Pos 0 0) . parse

day11b :: String -> Int
day11b = maximum . map walk . scanl' step (Pos 0 0) . parse

{-
           ( 2, 1)       ( 3, 0)
    ( 1, 1)       ( 2, 0)       ( 3,-1)
           ( 1, 0)       ( 2,-1)
    ( 0, 0)       ( 1,-1)       ( 2,-2)
           ( 0,-1)       ( 1,-2)         Trapezoidal
-------------------------------------------
           ( 1, 3)       ( 3, 3)          Cartesian
    ( 0, 2)       ( 2, 2)       ( 4, 2)
           ( 1, 1)       ( 3, 1)
    ( 0, 0)       ( 2, 0)       ( 4, 0)
           ( 1,-1)       ( 3, -1)
-}

chart :: String -> Chart
chart input = Chart {..} where
    positions = scanl' step (Pos 0 0) $ parse input
    points = [(x - y, x + y) | Pos {..} <- positions]
    distances = map walk positions
    bounds =
        ((minimum $ map fst points, minimum $ map snd points),
         (maximum $ map fst points, maximum $ map snd points))
    maxDistance = maximum distances
    path = zip points distances
