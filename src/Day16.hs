{-|
Module:         Day16
Description:    <http://adventofcode.com/2017/day/16 Day 16: Permutation Promenade>
-}
{-# LANGUAGE FlexibleContexts, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day16 (day16a, day16b) where

import Control.Arrow (second)
import Data.Array.IArray (IArray, Ix, (!), amap, bounds, elems, ixmap, listArray)
import Data.Array.Unboxed (UArray)
import Data.Bool (bool)
import Data.Char (chr, isSpace, ord)
import Data.Ix (range)
import Data.List (foldl', unfoldr)

-- | A dance move.
data Move i e = Spin i | Exchange i i | Partner e e

-- | Split words by whitespace or comma.
words' :: String -> [String]
words' = unfoldr $
    bool Nothing . Just . second (drop 1) . break delim <*> not . null where
    delim c = c == ',' || isSpace c

-- | Parse a list of dance moves.
parse :: String -> [Move Int Char]
parse = map parseMove . words' where
    parseMove ('s':d) = Spin $ read d
    parseMove ('x':(break (== '/') -> (x, _:y))) = Exchange (read x) (read y)
    parseMove ['p',a,'/',b] = Partner a b

-- | Compose two permutations.
(-*-) :: (IArray a e, IArray a' i, Ix i) => a i e -> a' i i -> a i e
a -*- b = ixmap (bounds a) (b !) a

-- | Exponentiate a permutation.
(-^-) :: (IArray a i, Ix i) => a i i -> Int -> a i i
a -^- 0 = listArray <*> range $ bounds a
a -^- n
  | (h, 0) <- n `divMod` 2 = (a -*- a) -^- h
  | (h, 1) <- n `divMod` 2 = (a -*- a) -^- h -*- a

-- | Like 'id', but swapping the two given values.
exchange :: (Eq a) => a -> a -> a -> a
exchange x y z
  | x == z = y
  | y == z = x
  | otherwise = z

-- | Given a list of dance moves, map out the resulting partner changes.
permuteNames :: (IArray a Char) => Int -> [Move i Char] -> a Char Char
permuteNames size moves = foldl' permuteName
    (listArray ('a', chr $ ord 'a' + size - 1) ['a'..])
    [(x, y) | Partner x y <- moves] where
    permuteName arr (x, y) = amap (exchange x y) arr

-- | Given a list of dance moves, map out the resulting spins and exchanges.
permuteIndices :: (IArray a Int) => Int -> [Move Int e] -> a Int Int
permuteIndices size = foldl' permuteIndex (listArray (0, size - 1) [0..]) where
    permuteIndex arr (Spin d) =
        ixmap (bounds arr) ((`mod` size) . subtract d) arr
    permuteIndex arr (Exchange x y) = ixmap (bounds arr) (exchange x y) arr
    permuteIndex arr _ = arr

day16a :: Int -> String -> String
day16a = flip day16b 1

day16b :: Int -> Int -> String -> String
day16b size n input =
    let dance = parse input
        pn = permuteNames size dance -^- n :: UArray Char Char
        pi = permuteIndices size dance -^- n :: UArray Int Int
    in map (elems pn !!) (elems pi)
