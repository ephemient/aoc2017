module Day14Spec (spec) where

import Day14 (day14a, day14b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day14a "" `shouldBe` 7964
    describe "part 2" $
        it "examples" $
            day14b "" `shouldBe` 1275
