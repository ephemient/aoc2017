{-|
Module:         Day10
Description:    <http://adventofcode.com/2017/day/10 Day 10: Knot Hash>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day10 (day10a, day10b) where

import Data.Array.IArray (IArray, (!), bounds, elems, ixmap, listArray)
import Data.Array.Unboxed (UArray)
import Data.Bits (xor)
import Data.Bool (bool)
import Data.Char (ord)
import Data.Ix (Ix, inRange, index, rangeSize)
import Data.List (foldl', replicate, scanl', unfoldr)
import Data.Word (Word8)
import Text.Printf (printf)

-- | Reverse elements of an array in a range of indices. The range may wrap.
reverseRange :: (IArray a e, Ix i, Num i) => a i e -> (i, i) -> a i e
reverseRange arr r@(start, end) = ixmap b reverseIx arr where
    b@(low, high) = bounds arr
    ix = index b
    reverseIx i
      | start <= end, inRange r i = start + end - i
      | start > end, inRange (start, high) i || inRange (low, end) i
      = low + fromIntegral ((ix start + ix end - ix i) `mod` rangeSize b)
      | otherwise = i

-- | Given array bounds and a list of lengths, returns a list of ranges in the
-- array, with each one starting at an increasing distance from the end of the
-- previous, wrapping around the ends of the array.
knotRanges :: (Ix i, Num i) => (i, i) -> [Int] -> [(i, i)]
knotRanges b@(low, high) counts =
  [ (low + fromIntegral start, low + fromIntegral (addMod start $ len - 1))
  | (len, start) <- zip counts $ scanl' addMod 0 $ zipWith (+) [0..] counts
  , len > 0
  ] where addMod x y = (x + y) `mod` rangeSize b

-- | Sequentially reverses all ranges in an array from a list of lengths.
hash :: (IArray a e, Ix i, Num i) => a i e -> [Int] -> a i e
hash arr counts = foldl' reverseRange arr $ knotRanges (bounds arr) counts

day10a :: Int -> String -> Int
day10a len input = hashed ! 0 * hashed ! 1 where
    arr = listArray (0, len - 1) [0..] :: UArray Int Int
    hashed = hash arr . read $ '[' : input ++ "]"

day10b :: String -> String
day10b input = dense >>= printf "%02x" . foldl' xor 0 where
    arr = listArray (0, 255) [0..] :: UArray Int Word8
    sparse = elems . hash arr . concat . replicate 64 $
        (lines input >>= map ord) ++ [17, 31, 73, 47, 23]
    dense = unfoldr (bool Nothing . Just . splitAt 16 <*> not . null) sparse
