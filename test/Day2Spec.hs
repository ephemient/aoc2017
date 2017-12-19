module Day2Spec (spec) where

import Day2 (day2a, day2b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
  [ "5 1 9 5"
  , "7 5 3"
  , "2 4 6 8"
  ]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day2a example `shouldBe` 18
    describe "part 2" $
        it "examples" $
            day2b example `shouldBe` 9
