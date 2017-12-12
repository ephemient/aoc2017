{-|
Module:         Day12
Description:    <http://adventofcode.com/2017/day/12 Day 12: Digital Plumber>
-}
{-# LANGUAGE TupleSections, TypeApplications, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day12 (day12a, day12b) where

import Control.Arrow (second)
import Data.Char (isDigit)
import Data.Graph.Inductive (Gr, Graph, bfs, mkUGraph, noComponents)

-- | Creates an unlabeled graph with edges from each "0 <-> 1, 2.." line.
parse :: (Graph gr) => String -> gr () ()
parse = uncurry mkUGraph . second concat . unzip . map parseLine . lines where
    parseLine (words -> (read -> x) : "<->" : xs) =
        (x, (x,) . read . filter isDigit <$> xs)

day12a :: String -> Int
day12a = length . bfs 0 . parse @Gr

day12b :: String -> Int
day12b = noComponents . parse @Gr
