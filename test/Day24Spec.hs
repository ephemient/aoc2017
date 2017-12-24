module Day24Spec (spec) where

import Day24 (day24a, day24b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
  [ "0/2"
  , "2/2"
  , "2/3"
  , "3/4"
  , "3/5"
  , "0/1"
  , "10/1"
  , "9/10"
  ]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day24a example `shouldBe` 31
    describe "part 2" $
        it "examples" $
            day24b example `shouldBe` 19
