{-|
Module:         Day10
Description:    <http://adventofcode.com/2017/day/10 Day 10: Knot Hash>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day10 (day10a, day10b) where

import Data.Bits (xor)
import Data.Bool (bool)
import Data.Char (ord)
import Data.List (foldl', replicate, unfoldr)
import Text.Printf (printf)

-- | @roll len list (count, skip)@ reverses the first @count@ elements of
-- @list@ then rolls both those reversed elements and an additional @skip@
-- elements from the start of the list to the end of the list.
--
-- @len@ must be equal to @length list@.
roll :: Int -> [a] -> (Int, Int) -> [a]
roll len list (count, skip) = end ++ start where
    (knot, keep) = splitAt count list
    (start, end) = splitAt skip $ keep ++ reverse knot

-- | @rolls len counts@ iterates @roll len list (count, skip)@, starting with
-- @list = [0..len-1]@, for each @count@ in @counts@ and @skip = 0, 1, ..@.
-- Finally, it unrotates by the total @count + skip@ sum.
rolls :: Int -> [Int] -> [Int]
rolls len counts = end ++ start where
    list = [0 .. len - 1]
    rotated = foldl (roll len) list . zip counts $ cycle list
    rotation = sum (zipWith (+) [0..] counts) `mod` len
    (start, end) = splitAt (len - rotation) rotated

day10a :: Int -> String -> Int
day10a len input = let x:y:_ = rolls len . read $ '[' : input ++ "]" in x * y

day10b :: String -> String
day10b input = map (foldl' xor 0) dense >>= printf "%02x" where
    sparse = rolls 256 . concat . replicate 64 $
        map ord (concat $ lines input) ++ [17, 31, 73, 47, 23]
    dense = unfoldr (bool (Just . splitAt 16) (const Nothing) =<< null) sparse
