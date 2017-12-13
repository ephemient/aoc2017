module Day13Spec (spec) where

import Day13 (day13a, day13b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
  [ "0: 3"
  , "1: 2"
  , "4: 4"
  , "6: 4"
  ]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day13a example `shouldBe` 24
    describe "part 2" $
        it "examples" $
            day13b example `shouldBe` 10
