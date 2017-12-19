module Day19Spec (spec) where

import Day19 (day19a, day19b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
  [ "     |          "
  , "     |  +--+    "
  , "     A  |  C    "
  , " F---|----E|--+ "
  , "     |  |  |  D "
  , "     +B-+  +--+ "
  ]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day19a example `shouldBe` "ABCDEF"
    describe "part 2" $
        it "examples" $
            day19b example `shouldBe` 38
