{-|
Module:         Day22
Description:    <http://adventofcode.com/2017/day/22 Day 22: Sporifica Virus>
-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day22 (day22a, day22b) where

import Control.Monad.ST (runST)
import Data.Bool (bool)
import qualified Data.HashTable.ST.Basic as HashTable (insert, lookup, new)
import Data.Maybe (fromMaybe)

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
move :: (Num a, Num b) => (a, b) -> O -> (a, b)
move (x, y) U = (x, y - 1)
move (x, y) R = (x + 1, y)
move (x, y) D = (x, y + 1)
move (x, y) L = (x - 1, y)

-- | Parses a string as a 2-D grid centered around @(0, 0)@, returning positions
-- with a @'#'@ character.
parse :: String -> [(Int, Int)]
parse s =
  [ (x, y)
  | let h = length $ lines s
  , (y, line) <- zip [- h `div` 2 ..] $ lines s
  , let w = length line
  , (x, '#') <- zip [- w `div` 2 ..] line
  ]

-- | Returns all viral activity from an initial state.
day22 :: (Eq a) =>
    a -> (a -> O -> O) -> (a -> a) -> a -> Int -> [(Int, Int)] -> Int
day22 infected turn mut def n input = runST $ do
    grid <- HashTable.new
    mapM_ (HashTable.insert grid `flip` infected) input
    let loop 0 k _ _ = pure k
        loop n !k dir pos = do
            a <- fromMaybe def <$> HashTable.lookup grid pos
            let a' = mut a
                dir' = turn a dir
                pos' = move pos dir'
            HashTable.insert grid pos a'
            loop (n - 1) (if a' == infected then k + 1 else k) dir' pos'
    loop n 0 U (0, 0)

day22a :: String -> Int
day22a = day22 True (bool prev next) next False 10000 . parse

day22b :: String -> Int
day22b = day22 I turn next C 10000000 . parse where
    turn C = prev
    turn W = id
    turn I = next
    turn F = next . next
