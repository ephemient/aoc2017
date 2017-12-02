module Day2Spec (spec) where

import Day2 (day2a, day2b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day2a (unlines
              [ "5 1 9 5"
              , "7 5 3"
              , "2 4 6 8"
              ]) `shouldBe` 18
    describe "part 2" $
        it "examples" $
            day2b (unlines
              [ "5 9 2 8"
              , "9 4 7 3"
              , "3 8 6 5"
              ]) `shouldBe` 9
