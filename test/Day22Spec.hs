module Day22Spec (spec) where

import Day22 (day22a, day22b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
  [ "..#"
  , "#.."
  , "..."
  ]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day22a example `shouldBe` 5587
    describe "part 2" $
        it "examples" $
            day22b example `shouldBe` 2511944
