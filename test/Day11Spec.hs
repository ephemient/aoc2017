module Day11Spec (spec) where

import Day11 (day11a, day11b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day11a "ne,ne,ne" `shouldBe` 3
            day11a "ne,ne,sw,sw" `shouldBe` 0
            day11a "ne,ne,s,s" `shouldBe` 2
            day11a "se,sw,se,sw,sw" `shouldBe` 3
    describe "part 2" $
        it "examples" $
            day11b "ne,ne,sw,sw" `shouldBe` 2
