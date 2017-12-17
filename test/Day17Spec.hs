module Day17Spec (spec) where

import Day17 (day17a, day17b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day17a 3 `shouldBe` 638
    describe "part 2" $
        it "examples" $
            day17b 3 `shouldBe` 1222153
