module Day8Spec (spec) where

import Day8 (day8a, day8b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
  [ "b inc 5 if a > 1"
  , "a inc 1 if b < 5"
  , "c dec -10 if a >= 1"
  , "c inc -20 if c == 10"
  ]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day8a example `shouldBe` 1
    describe "part 2" $
        it "examples" $
            day8b example `shouldBe` 10
