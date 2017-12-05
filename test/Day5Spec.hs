module Day5Spec (spec) where

import Day5 (day5a, day5b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day5a (unlines $ map show [0, 3, 0, 1, -3]) `shouldBe` 5
    describe "part 2" $
        it "examples" $
            day5b (unlines $ map show [0, 3, 0, 1, -3]) `shouldBe` 10
