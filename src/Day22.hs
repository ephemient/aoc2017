{-|
Module:         Day22
Description:    <http://adventofcode.com/2017/day/22 Day 22: Sporifica Virus>
-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day22 (day22a, day22b) where

import Data.Bool (bool)
import Data.List (genericLength, unfoldr)
import qualified Data.IntMap.Lazy as IntMap (delete, findWithDefault, fromList, insert)
import Data.IntMap.Lazy (IntMap)
import Data.Maybe (fromMaybe)
import Morton (Z(..), decX, decY, incX, incY, toZ)

-- | A cardinal direction.
data O = U | R | D | L deriving (Bounded, Enum, Eq)

-- | Node state.
data S = C | W | I | F deriving (Bounded, Enum, Eq)

-- | Viral state
data V k a b = V {nodes :: IntMap a, dir :: b, pos :: k}

-- | @Just . succ@ if in bounds, @Nothing@ if not.
maybeNext :: (Bounded a, Enum a, Eq a) => a -> Maybe a
maybeNext a = if a == maxBound then Nothing else Just $ succ a

-- | 'succ' with wraparound.
next :: (Bounded a, Enum a, Eq a) => a -> a
next = fromMaybe minBound . maybeNext

-- | 'pred' with wraparound.
prev :: (Bounded a, Enum a, Eq a) => a -> a
prev a = if a == minBound then maxBound else pred a

-- | Moves a Cartesian coordinate by one step in a direction.
move :: O -> Z -> Z
move U = decY
move R = incX
move D = incY
move L = decX

-- | Parses a string as a 2-D grid centered around @(0, 0)@, mapping positions
-- with a @'#'@ character to a given value.
parse :: a -> String -> IntMap a
parse a s = IntMap.fromList
  [ (fromIntegral . getZ $ toZ x y, a)
  | let h = genericLength $ lines s
  , (y, line) <- zip [- h `div` 2 ..] $ lines s
  , let w = genericLength line
  , (x, '#') <- zip [- w `div` 2 ..] line
  ]

-- | At a viral state, return the current activity and the next viral state.
step :: (Ord k) => (a -> b -> b) -> (b -> k -> k) -> (a -> Maybe a) -> a ->
    (k -> Int) -> V k a b -> (Maybe a, V k a b)
step turn move mut def toKey V {..} = (a', V nodes' dir' pos') where
    key = toKey pos
    a = IntMap.findWithDefault def key nodes
    a' = mut a
    nodes' = maybe IntMap.delete (flip IntMap.insert) a' key nodes
    dir' = turn a dir
    pos' = move dir' pos

-- | Returns all viral activity from an initial state.
day22 :: a -> (a -> O -> O) -> (a -> Maybe a) -> a -> String -> [Maybe a]
day22 a turn mut def input =
    unfoldr (Just . step turn move mut def (fromIntegral . getZ)) .
    V (parse a input) U $ toZ 0 0

day22a :: String -> Int
day22a = length . filter (== Just True) . take 10000 .
         day22 True (bool prev next) maybeNext False

day22b :: String -> Int
day22b = length . filter (== Just I) . take 10000000 .
         day22 I turn maybeNext C where
    turn C = prev
    turn W = id
    turn I = next
    turn F = next . next
