{-|
Module:         Day22
Description:    <http://adventofcode.com/2017/day/22 Day 22: Sporifica Virus>
-}
{-# LANGUAGE BangPatterns, TypeApplications #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day22 (day22a, day22b) where

import Control.Monad.ST (runST)
import Data.Bool (bool)
import Data.List (genericLength)
import Data.Maybe (fromMaybe)
import Data.Word (Word64, Word8)
import GrowArray (GrowArray, newGrowArray, readGrowArray, writeGrowArray)
import Morton (Z(..), decX, decY, incX, incY, toZ)

-- | A cardinal direction.
data O = U | R | D | L deriving (Bounded, Enum, Eq)

-- | Node state.
data S = C | W | I | F deriving (Bounded, Enum, Eq)

-- | 'succ' with wraparound.
next :: (Bounded a, Enum a, Eq a) => a -> a
next a = if a == maxBound then minBound else succ a

-- | 'pred' with wraparound.
prev :: (Bounded a, Enum a, Eq a) => a -> a
prev a = if a == minBound then maxBound else pred a

-- | Moves a Cartesian coordinate by one step in a direction.
move :: O -> Z -> Z
move U = decY
move R = incX
move D = incY
move L = decX

-- | Parses a string as a 2-D grid centered around @(0, 0)@, returning positions
-- with a @'#'@ character.
parse :: String -> [Z]
parse s =
  [ toZ x y
  | let h = genericLength $ lines s
  , (y, line) <- zip [- h `div` 2 ..] $ lines s
  , let w = genericLength line
  , (x, '#') <- zip [- w `div` 2 ..] line
  ]

-- | Returns all viral activity from an initial state.
day22 :: (Enum a, Eq a) =>
    a -> (a -> O -> O) -> (a -> a) -> a -> Int -> [Z] -> Int
day22 infected turn mut def n input = runST $ do
    grid <- newGrowArray @Word64 @Word8 (0, 0) . fromIntegral $ fromEnum def
    let infected' = fromIntegral $ fromEnum infected
    sequence_ [writeGrowArray grid z infected' | Z z <- input]
    let loop 0 k _ _ = pure k
        loop n !k dir pos = do
            a <- toEnum . fromIntegral <$> readGrowArray grid (getZ pos)
            let a' = mut a
                dir' = turn a dir
                pos' = move dir' pos
            writeGrowArray grid (getZ pos) . fromIntegral $ fromEnum a'
            loop (n - 1) (if a' == infected then k + 1 else k) dir' pos'
    loop n 0 U $ toZ 0 0

day22a :: String -> Int
day22a = day22 True (bool prev next) next False 10000 . parse

day22b :: String -> Int
day22b = day22 I turn next C 10000000 . parse where
    turn C = prev
    turn W = id
    turn I = next
    turn F = next . next
