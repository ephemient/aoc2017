module Day15Spec (spec) where

import Day15 (day15a, day15b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = "Generator A starts with 65\nGenerator B starts with 8921\n"

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day15a example `shouldBe` 588
    describe "part 2" $
        it "examples" $
            day15b example `shouldBe` 309
