module Day21Spec (spec) where

import Day21 (day21)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = it "examples" $
    day21 2 (unlines
      [ "../.# => ##./#../..."
      , ".#./..#/### => #..#/..../..../#..#"
      ]) `shouldBe` 12
