{-|
Module:         Day4
Description:    <http://adventofcode.com/2017/day/4 Day 4: High-Entropy Passphrases>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day4 (day4a, day4b) where

import Data.List (sort, tails, uncons)

-- | The 'allUnique' function returns whether the list has no duplicates.
allUnique :: (Eq a) => [a] -> Bool
allUnique = not . any (maybe False (uncurry elem) . uncons) . tails

day4a :: String -> Int
day4a = length . filter allUnique . map words . lines

day4b :: String -> Int
day4b = length . filter (allUnique . map sort) . map words . lines
