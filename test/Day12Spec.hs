module Day12Spec (spec) where

import Day12 (day12a, day12b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
  [ "0 <-> 2"
  , "1 <-> 1"
  , "2 <-> 0, 3, 4"
  , "3 <-> 2, 4"
  , "4 <-> 2, 3, 6"
  , "5 <-> 6"
  , "6 <-> 4, 5"
  ]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day12a example `shouldBe` 6
    describe "part 2" $
        it "examples" $
            day12b example `shouldBe` 2
