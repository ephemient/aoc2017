module Day18Spec (spec) where

import Day18 (day18a, day18b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
  [ "set a 1"
  , "add a 2"
  , "mul a a"
  , "mod a 5"
  , "snd a"
  , "set a 0"
  , "rcv a"
  , "jgz a -1"
  , "set a 1"
  , "jgz a -2"
  ]

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day18a example `shouldBe` 4
    describe "part 2" $
        it "examples" $
            day18b example `shouldBe` 1
