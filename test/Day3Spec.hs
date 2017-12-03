module Day3Spec (spec) where

import Day3 (day3a, day3b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day3a "12" `shouldBe` 3
            day3a "23" `shouldBe` 2
            day3a "1024" `shouldBe` 31
    describe "part 2" $
        it "examples" $ do
            day3b "351" `shouldBe` 362
            day3b "362" `shouldBe` 747
            day3b "747" `shouldBe` 806
