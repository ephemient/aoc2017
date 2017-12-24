{-|
Module:         Day24
Description:    <http://adventofcode.com/2017/day/24 Day 24: Electromagnetic Moat>
-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day24 (day24a, day24b) where

import Control.Arrow ((***))
import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.Split (splitOn)
import qualified Data.Map.Lazy as Map (assocs, delete, empty, findWithDefault, fromListWith, null, singleton, union, update)
import Data.Map.Lazy (Map)
import Data.Semigroup (Max(..), Semigroup, sconcat)

-- | Bidrectionally associates @a => n => b@ and @b => n => a@, where @n@ is an
-- unique identifier for each @a/b@ line.
parse :: (Ord a, Read a) => String -> Map a (Map Int a)
parse = Map.fromListWith Map.union . concatMap parseLine . zip [0..] . lines
  where
    parseLine (n, splitOn "/" -> [read -> a, read -> b]) =
        [(a, Map.singleton n b), (b, Map.singleton n a)]

-- | Deletes a key from a 'Map', returning 'Nothing' if it becomes empty.
deleteOrNull :: (Ord k) => k -> Map k v -> Maybe (Map k v)
deleteOrNull k m = bool Just (const Nothing) =<< Map.null $ Map.delete k m

-- | Explores all bridges.
buildBridge :: (Num a, Ord a, Ord b, Semigroup s) =>
    (a -> a -> s -> s) -> s -> a -> Map a (Map b a) -> s
buildBridge f k start parts = sconcat $ k :|
  [ buildBridge f (f start next k) next .
        Map.update (deleteOrNull n) start .
        Map.update (deleteOrNull n) next $ parts
  | (n, next) <- Map.assocs $ Map.findWithDefault Map.empty start parts
  ]

day24a :: String -> Int
day24a = getMax . buildBridge toMax (Max 0) 0 . parse where
    toMax start next = fmap (start + next +)

day24b :: String -> Int
day24b = snd . getMax . buildBridge toMax (Max (0 :: Int, 0)) 0 . parse where
    toMax start next = fmap $ succ *** (start + next +)
