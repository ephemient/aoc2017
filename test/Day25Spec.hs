module Day25Spec (spec) where

import Day25 (day25)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = unlines
  [ "Begin in state A."
  , "Perform a diagnostic checksum after 6 steps."
  , ""
  , "In state A:"
  , "  If the current value is 0:"
  , "    - Write the value 1."
  , "    - Move one slot to the right."
  , "    - Continue with state B."
  , "  If the current value is 1:"
  , "    - Write the value 0."
  , "    - Move one slot to the left."
  , "    - Continue with state B."
  , ""
  , "In state B:"
  , "  If the current value is 0:"
  , "    - Write the value 1."
  , "    - Move one slot to the left."
  , "    - Continue with state A."
  , "  If the current value is 1:"
  , "    - Write the value 1."
  , "    - Move one slot to the right."
  , "    - Continue with state A."
  ]

spec :: Spec
spec = describe "part 1" $ it "examples" $ day25 example `shouldBe` 3
