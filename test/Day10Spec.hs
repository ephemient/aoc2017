module Day10Spec (spec) where

import Day10 (day10a, day10b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day10a 5 "3,4,1,5" `shouldBe` 12
    describe "part 2" $
        it "examples" $ do
            day10b "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
            day10b "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
            day10b "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
            day10b "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"
