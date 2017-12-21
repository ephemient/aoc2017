{-|
Module:         Day10
Description:    <http://adventofcode.com/2017/day/10 Day 10: Knot Hash>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day10 (day10a, day10b, deriveKey, hashString, knotRanges, reverseRange, xorEach) where

import Data.Array.IArray (IArray, (!), bounds, elems, ixmap, listArray)
import Data.Array.Unboxed (UArray)
import Data.Bits (Bits, xor)
import Data.Bool (bool)
import Data.Char (ord)
import Data.Ix (Ix, inRange, index, rangeSize)
import Data.List (foldl', foldl1', replicate, scanl')
import Data.List.Split (chunksOf)
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
knotRanges :: (Ix i, Num i) => (i, i) -> [Int] -> [Maybe (i, i)]
knotRanges b@(low, high) counts =
  [ if len <= 0 then Nothing else Just
    (low + fromIntegral start, low + fromIntegral (addMod start $ len - 1))
  | (len, start) <- zip counts $ scanl' addMod 0 $ zipWith (+) [0..] counts
  ] where addMod x y = (x + y) `mod` rangeSize b

-- | Sequentially reverses all ranges in an array from a list of lengths.
hash :: (IArray a e, Ix i, Num i) => a i e -> [Int] -> a i e
hash arr counts =
    foldl' (maybe <*> reverseRange) arr $ knotRanges (bounds arr) counts

-- | Adds some magic numbers to the codepoints to a string, repeated 64 times.
deriveKey :: String -> [Int]
deriveKey = concat . replicate 64 . (++ [17, 31, 73, 47, 23]) . map ord

-- | Reduce consecutive groups of a fixed length by @xor@.
xorEach :: (IArray a e, Ix i, Bits e) => Int -> a i e -> [e]
xorEach n = fmap (foldl1' xor) . chunksOf n . elems

-- | Deriving a key from a string by using its codepoints plus some magic
-- numbers, 'hash' @[0..255]@ 64 times, then 'xor' together each group of 16.
hashString :: String -> [Word8]
hashString = xorEach 16 . hash arr . deriveKey where
    arr = listArray (0, 255) [0..] :: UArray Int Word8

day10a :: Int -> String -> Int
day10a len input = hashed ! 0 * hashed ! 1 where
    arr = listArray (0, len - 1) [0..] :: UArray Int Int
    hashed = hash arr . read $ '[' : input ++ "]"

day10b :: String -> String
day10b = concatMap (printf "%02x") . hashString
