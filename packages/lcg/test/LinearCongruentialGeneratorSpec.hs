module LinearCongruentialGeneratorSpec (spec) where

import Data.Word (Word64)
import LinearCongruentialGenerator (lcg32, next32)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Small(..), (===), (==>), property)

spec :: Spec
spec = do
    describe "next32" $
        it "is a * x % m" $ property $ \a m x -> m /= 0 ==>
            let (a', m', x') = (fromIntegral a, fromIntegral m, fromIntegral x)
            in fromIntegral (next32 (a, m) x) === (a' * x' `mod` m' :: Word64)
    describe "lcg32" $
        it "iterates" $ property $ \a m x (Small n) -> m /= 0 && n > 1 ==>
            let (a', m', x') = (fromIntegral a, fromIntegral m, fromIntegral x)
                _:lcg32' = iterate ((`mod` m') . (a' *)) x' :: [Word64]
            in map fromIntegral (lcg32 (a, m) x) !! n === (lcg32' !! n)
