{-|
Module:         Day21
Description:    <http://adventofcode.com/2017/day/21 Day 21: Fractal Art>
-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day21 (day21, day21a, day21b) where

import Control.Arrow ((***), first, second)
import Data.Array.IArray (IArray, array, assocs, bounds, elems, ixmap)
import Data.Array.Unboxed (UArray)
import Data.Ix (Ix, rangeSize)
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map (fromList, lookup)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Tuple (swap)

-- | Converts to a 2-D array with lower bound @(0,0)@.
array2D :: (IArray a e) => [[e]] -> a (Int, Int) e
array2D grid = array ((0, 0), (w - 1, h - 1)) assocs where
    (w, h) = (foldl' max 0 $ length <$> grid, length grid)
    assocs = [((x, y), e) | (y, row) <- zip [0..] grid, (x, e) <- zip [0..] row]

-- | 8 affine transformations.
rotations :: (IArray a e, Ix i, Num i) => [a (i, i) e -> a (i, i) e]
rotations = [id, f, g, f . g, t, t . f, t . g, t . f . g] where
    f a = let b@((_, l), (_, h)) = bounds a in ixmap b (second (l + h -)) a
    g a = let b@((l, _), (h, _)) = bounds a in ixmap b (first (l + h -)) a
    t a = let (l, h) = bounds a in ixmap (swap l, swap h) swap a

-- | Parse a list of enhancements.
parse :: (IArray a Bool, Ord (a (Int, Int) Bool)) =>
    String -> Map (a (Int, Int) Bool) (a (Int, Int) Bool)
parse = Map.fromList . concatMap parseLine . lines where
    parseLine s = let [a, "=>", b] = words s in
      [ (rot a', b')
      | let a' = array2D $ map (== '#') <$> splitOn "/" a
      , let b' = array2D $ map (== '#') <$> splitOn "/" b
      , rot <- rotations
      ]

-- | The glider.
start :: UArray (Int, Int) Bool
start = array2D $ map (== '#') <$>
  [ ".#."
  , "..#"
  , "###"
  ]

-- | Expand sub-squares according to the rules map.
step :: (IArray a e, Ord (a (Int, Int) e), Show (a (Int, Int) e)) =>
    Map (a (Int, Int) e) (a (Int, Int) e) -> a (Int, Int) e -> a (Int, Int) e
step rules grid = head $ do
    let ((lx, ly), (hx, hy)) = bounds grid
        (w, h) = (rangeSize (lx, hx), rangeSize (ly, hy))
    (dx, dy, mx, my) <- [(2, 2, 3, 3), (3, 3, 4, 4)]
    (qx, 0) <- [w `divMod` dx]
    (qy, 0) <- [h `divMod` dy]
    let bounds' = ((0, 0), (qx * mx - 1, qy * my - 1))
    maybeToList $ array bounds' . concatMap assocs <$> sequence
      [ Map.lookup part rules >>= shiftBounds
      | i <- [0 .. qx - 1]
      , j <- [0 .. qy - 1]
      , let (x, y) = (lx + i * dx, ly + j * dy)
            (x', y') = (i * mx, j * my)
            part = ixmap ((0, 0), (dx - 1, dy - 1)) ((x +) *** (y +)) grid
            shiftBounds arr
              | rangeSize (lx', hx') == mx
              , rangeSize (lx', hx') == mx
              = Just $ ixmap ((x', y'), (x' + mx - 1, y' + my - 1))
                    (subtract (x' - lx') *** subtract (y' - ly')) arr
              | otherwise = Nothing
              where ((lx', ly'), (hx', hy')) = bounds arr
      ]

-- | @day21 n input@ returns the number of bits set in the @n@-th 'step' of
-- transforming the 'start' glider using 'parse' rules.
day21 :: Int -> String -> Int
day21 n input =
    length . filter id . elems $ iterate (step $ parse input) start !! n

day21a :: String -> Int
day21a = day21 5

day21b :: String -> Int
day21b = day21 18
