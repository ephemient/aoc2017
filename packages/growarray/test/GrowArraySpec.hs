module GrowArraySpec (spec) where

import Control.Monad.ST (ST)
import Data.Ix (range)
import GrowArray (GrowArray, newGrowArray, readGrowArray, writeGrowArray)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck ((===), property)
import Test.QuickCheck.Monadic (monadicST, pre, run)

spec :: Spec
spec = do
    describe "read" $ do
        it "in bounds" $ property $ \lo def xs -> monadicST $ do
            pre $ not (null xs)
            let hi = lo + length xs - 1
            arr <- run $ fillArray lo def xs
            xs' <- run $ readArray arr $ range (lo, hi)
            pure $ xs === xs'
        it "out of bounds" $ property $ \lo def xs n -> monadicST $ do
            pre $ not (null xs)
            let p i = (if i < 0 then lo else lo + length xs) + i
            arr <- run $ fillArray lo def xs
            x <- run $ readGrowArray arr $ p n
            pure $ def === x
    describe "write" $
        it "out of bounds" $ property $ \lo def xs n m x -> monadicST $ do
            pre $ not (null xs) && n /= m
            let hi = lo + length xs - 1
                p i = (if i < 0 then lo else hi + 1) + i
            arr <- run $ fillArray lo def xs
            run $ writeGrowArray arr (p n) x
            xs' <- run $ readArray arr $ range (lo, hi) ++ map p [n, m]
            pure $ xs ++ [x, def] === xs'
  where
    fillArray :: Int -> Char -> String -> ST s (GrowArray s Int Char)
    fillArray lo def xs = do
        let hi = lo + length xs - 1
        arr <- newGrowArray (lo, hi) def
        sequence_ [writeGrowArray arr i x | (i, x) <- zip [lo..] xs]
        pure arr
    readArray :: GrowArray s Int Char -> [Int] -> ST s String
    readArray arr is = sequence [readGrowArray arr i | i <- is]
