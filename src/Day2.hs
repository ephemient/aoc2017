{-|
Module:         Day2
Description:    <http://adventofcode.com/2017/day/2 Day 2: Corruption Checksum>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day2 (day2a, day2b) where

import Control.Monad (liftM2)

-- | The 'readSpreadsheet' function splits lines to rows and spaces to columns.
readSpreadsheet :: String -> [[Int]]
readSpreadsheet = map (map read . words) . lines

day2a :: String -> Int
day2a = sum . map (liftM2 (-) maximum minimum) . readSpreadsheet

day2b :: String -> Int
day2b = sum . map checksum . readSpreadsheet where
    checksum l = head [q | (q, 0) <- liftM2 quotRem l l, q /= 1]
