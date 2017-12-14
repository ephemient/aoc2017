{-|
Module:         Day14
Description:    <http://adventofcode.com/2017/day/14 Day 14: Disk Defragmentation>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day14 (day14a, day14b) where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Bits (FiniteBits, finiteBitSize, popCount, testBit)
import Data.Graph (components, graphFromEdges)
import Data.List (zip3, zip4)
import Data.Word (Word8)
import Day10 (hashString)

-- | Given an input string, returns 'hashString' of the string with a dash and
-- each integer in @[0..127]@.
grid :: String -> [[Word8]]
grid input = parMap rpar hashString $ ((input ++) . ('-':) . show) <$> [0..127]

-- | Returns a list of bits from most-significant to least-significant.
bits :: (FiniteBits b) => b -> [Bool]
bits b = map (testBit b) $ reverse [0 .. finiteBitSize b - 1]

day14a :: String -> Int
day14a = sum . map popCount . concat . grid

day14b :: String -> Int
day14b input = length $ components g where
    (g, _, _) = graphFromEdges
      [ ((), (x, y), [(x - 1, y) | left] ++ [(x, y - 1) | up])
      | let bitgrid = map (concatMap bits) $ grid input
      , (y, line, prev) <- zip3 [0..] bitgrid $ repeat False : bitgrid
      , (x, True, up, left) <- zip4 [0..] line prev $ False : line
      ]
