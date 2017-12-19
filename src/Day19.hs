{-|
Module:         Day19
Description:    <http://adventofcode.com/2017/day/19 Day 19: A Series of Tubes>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day19 (day19a, day19b) where

import Data.Array.IArray (IArray, (!), array, bounds)
import Data.Array.Unboxed (UArray)
import Data.Char (isAlpha, isSpace)
import Data.Ix (Ix, inRange)
import Data.List ((\\), elemIndex)

-- | Represents a cardinal direction.
data Direction = U | D | L | R deriving (Bounded, Enum, Eq)

-- | Returns the 2D representation of a string by lines and the index of the
-- only @|@ pipe character on the first line.
parse :: (IArray a Char) => String -> (a (Int, Int) Char, (Int, Int))
parse input = (maze, (x0 + 1, 1)) where
    Just x0 = elemIndex '|' row0
    rows@(row0:_) = lines input
    maze = array ((1, 1), (maximum $ length <$> rows, length rows))
        [((x, y), c) | (y, row) <- zip [1..] rows, (x, c) <- zip [1..] row]

-- | Rotates a cardinal direction by 180 degrees.
rot180 :: Direction -> Direction
rot180 U = D
rot180 D = U
rot180 L = R
rot180 R = L

-- | Moves a point one step in a cardinal direction.
move :: (Num a) => (a, a) -> Direction -> (a, a)
move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)
move (x, y) L = (x - 1, y)
move (x, y) R = (x + 1, y)

-- | Returns the cardinal directions around a point which contain non-space.
joints :: (IArray a Char, Ix i, Num i) => a (i, i) Char -> (i, i) -> [Direction]
joints maze p = filter f [minBound .. maxBound] where
    f d = (&&) <$> inRange (bounds maze) <*> not . isSpace . (maze !) $ move p d

-- | Walks a maze from a starting point in a direction until it runs of
-- non-space characters to follow. Returns a list of all characters on the walk.
walk :: (IArray a Char, Ix i, Num i) =>
    a (i, i) Char -> (i, i) -> Direction -> String
walk maze p d
  | not (inRange (bounds maze) p) || isSpace c = []
  | otherwise = c : (walk maze =<< move p) (if c == '+' then d' else d)
  where
    c = maze ! p
    [d'] = joints maze p \\ [rot180 d]

day19a :: String -> String
day19a input = filter isAlpha $ walk maze p0 D where
    (maze, p0) = parse @UArray input

day19b :: String -> Int
day19b input = length $ walk maze p0 D where (maze, p0) = parse @UArray input
