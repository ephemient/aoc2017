{-|
Module:         Day1
Description:    <http://adventofcode.com/2017/day/1 Day 1: Inverse Captcha>
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day1 (day1a, day1b) where

import Control.Monad (ap, liftM2)
import Data.Char (digitToInt, isDigit)

-- |The 'digits' function returns a list of digits contained in a string.
digits :: String -> [Int]
digits = map digitToInt . filter isDigit

day1a :: String -> Int
day1a = sum . map fst . filter (uncurry (==)) . ap zip (tail . cycle) . digits

day1b :: String -> Int
day1b = sum . map fst . filter (uncurry (==)) .
        ap zip (liftM2 drop half cycle) . digits
  where half = (`div` 2) . length
