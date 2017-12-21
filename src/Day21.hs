{-|
Module:         Day21
Description:    <http://adventofcode.com/2017/day/21 Day 21: Fractal Art>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, PatternGuards, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day21 (day21, day21a, day21b) where

import Control.Arrow ((***), first, second)
import Control.Monad.State.Strict (MonadState, evalState, get, modify)
import Data.Array.IArray (IArray, array, assocs, bounds, elems, ixmap)
import Data.Array.Unboxed (UArray)
import Data.Ix (Ix, rangeSize)
import Data.List (foldl', mapAccumL)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map.Strict as Map (fromList, insert, lookup)
import Data.Map.Strict (Map)
import Data.Tuple (swap)

-- | Converts to a 2-D array with lower bound @(0,0)@.
array2D :: (IArray a e) => [[e]] -> a (Int, Int) e
array2D grid = array ((0, 0), (w - 1, h - 1)) assocs where
    (w, h) = (foldl' max 0 $ length <$> grid, length grid)
    assocs = [((x, y), e) | (y, row) <- zip [0..] grid, (x, e) <- zip [0..] row]

-- | 8 affine transformations.
transforms :: (IArray a e, Ix i, Num i) => [a (i, i) e -> a (i, i) e]
transforms = [id, f, g, f . g, t, t . f, t . g, t . f . g] where
    f a = let b@((_, l), (_, h)) = bounds a in ixmap b (second (l + h -)) a
    g a = let b@((l, _), (h, _)) = bounds a in ixmap b (first (l + h -)) a
    t a = let (l, h) = bounds a in ixmap (swap l, swap h) swap a

-- | Parse a list of enhancements.
parse :: (IArray a Bool, Ord (a (Int, Int) Bool)) =>
    String -> Map (a (Int, Int) Bool) (a (Int, Int) Bool)
parse = Map.fromList . concatMap parseLine . lines where
    parseLine s = let [a, "=>", b] = words s in
      [ (transform a', b')
      | let a' = array2D $ map (== '#') <$> splitOn "/" a
      , let b' = array2D $ map (== '#') <$> splitOn "/" b
      , transform <- transforms
      ]

-- | The glider.
start :: UArray (Int, Int) Bool
start = array2D $ map (== '#') <$>
  [ ".#."
  , "..#"
  , "###"
  ]

-- | Expand sub-squares according to the rules map.
step :: (IArray a e, Ord (a (Int, Int) e),
        MonadState (Map (a (Int, Int) e) (a (Int, Int) e)) m) =>
    a (Int, Int) e -> m (a (Int, Int) e)
step grid = Map.lookup grid <$> get >>= \case
    Just art -> pure art
    _ | ((qx, 0), (qy, 0)) <- (w `divMod` 3, h `divMod` 3) -> divide 3 qx qy
      | ((qx, 0), (qy, 0)) <- (w `divMod` 2, h `divMod` 2) -> divide 2 qx qy
  where
    ((lx, ly), (hx, hy)) = bounds grid
    (w, h) = (rangeSize (lx, hx), rangeSize (ly, hy))
    divide n qx qy = assemble n =<< sequence
      [ step $ ixmap ((0, 0), (qx - 1, qy - 1)) ((x - lx +) *** (y - ly +)) grid
      | y <- [0, qy .. h - 1]
      , x <- [0, qx .. w - 1]
      ]
    assemble n parts = result <$ modify insertAll where
        (h', unzip -> (foldl' max 0 -> w', concat -> grids)) =
            mapAccumL assembleRow 0 $ chunksOf n parts
        result = array ((0, 0), (w' - 1, h' - 1)) $ grids >>= assocs
        insertAll = foldr ((.) . (`Map.insert` result) . ($ grid)) id transforms
    assembleRow top row = (top + h', mapAccumL (shiftCell top) 0 row) where
        h' = foldl' max 0 $ rangeSize . (snd *** snd) . bounds <$> row
    shiftCell top left cell =
        (left + w',
         ixmap ((left, top), (left + w' - 1, top + h' - 1)) shift cell) where
        ((lx', ly'), (hx', hy')) = bounds cell
        (w', h') = (rangeSize (lx', hx'), rangeSize (ly', hy'))
        shift = subtract (left - lx') *** subtract (top - ly')

-- | @day21 n input@ returns the number of bits set in the @n@-th 'step' of
-- transforming the 'start' glider using 'parse' rules from @input@.
day21 :: Int -> String -> Int
day21 n input = evalState (loop n start) (parse input) where
    loop 0 art = pure . length . filter id . elems $ art
    loop n art = step art >>= loop (n - 1)

day21a :: String -> Int
day21a = day21 5

day21b :: String -> Int
day21b = day21 18
