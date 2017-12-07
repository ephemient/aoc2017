module Day7Spec (spec) where

import Day7 (day7a, day7b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
  [ "pbga (66)"
  , "xhth (57)"
  , "ebii (61)"
  , "havc (66)"
  , "ktlj (57)"
  , "fwft (72) -> ktlj, cntj, xhth"
  , "qoyq (66)"
  , "padx (45) -> pbga, havc, qoyq"
  , "tknk (41) -> ugml, padx, fwft"
  , "jptl (61)"
  , "ugml (68) -> gyxo, ebii, jptl"
  , "gyxo (61)"
  , "cntj (57)"
  ]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day7a example `shouldBe` Just "tknk"
    describe "part 2" $
        it "examples" $
            day7b example `shouldBe` Just 60
