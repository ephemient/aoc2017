module Day17Spec (spec) where

import Day17 (day17a, day17b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "part 1" $
        it "examples" $
            day17a 3 `shouldBe` 638
