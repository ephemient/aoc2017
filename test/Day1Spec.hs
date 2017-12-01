module Day1Spec (spec) where

import Day1 (day1a, day1b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day1a "1122" `shouldBe` 3
            day1a "1111" `shouldBe` 4
            day1a "1234" `shouldBe` 0
            day1a "91212129" `shouldBe` 9
    describe "part 2" $
        it "examples" $ do
            day1b "1212" `shouldBe` 6
            day1b "1221" `shouldBe` 0
            day1b "123425" `shouldBe` 4
            day1b "123123" `shouldBe` 12
            day1b "12131415" `shouldBe` 4
