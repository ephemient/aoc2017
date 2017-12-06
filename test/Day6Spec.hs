module Day6Spec (spec) where

import Day6 (day6a, day6b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day6a "0 2 7 0" `shouldBe` 5
    describe "part 2" $
        it "examples" $
            day6b "0 2 7 0" `shouldBe` 4
