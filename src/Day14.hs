{-|
Module:         Day14
Description:    <http://adventofcode.com/2017/day/14 Day 14: Disk Defragmentation>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day14 (bits, day14a, day14b, groupedBits) where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Bits (FiniteBits, finiteBitSize, popCount, testBit)
import Data.Graph (components, graphFromEdges)
import Data.List (zip3, zip4)
import Data.Tree (flatten)
import Data.Word (Word8)
import Day10 (hashString)

-- | Given an input string, returns 'hashString' of the string with a dash and
-- each integer in @[0..127]@.
grid :: String -> [[Word8]]
grid input = parMap rpar hashString $ ((input ++) . ('-':) . show) <$> [0..127]

-- | Returns a list of bits from most-significant to least-significant.
bits :: (FiniteBits b) => b -> [Bool]
bits b = map (testBit b) $ reverse [0 .. finiteBitSize b - 1]

-- | Returns a list of the coordinates of connected groups of bits.
groupedBits :: (FiniteBits b) => [[b]] -> [[(Int, Int)]]
groupedBits input = map vertex . flatten <$> components gr where
    (gr, f, _) = graphFromEdges
      [ ((), (x, y), [(x - 1, y) | left] ++ [(x, y - 1) | up])
      | let bitgrid = concatMap bits <$> input
      , (y, line, prev) <- zip3 [0..] bitgrid $ repeat False : bitgrid
      , (x, True, up, left) <- zip4 [0..] line prev $ False : line
      ]
    vertex v = let (_, xy, _) = f v in xy

day14a :: String -> Int
day14a = sum . map popCount . concat . grid

day14b :: String -> Int
day14b = length . groupedBits . grid
