module Day4Spec (spec) where

import Day4 (day4a, day4b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day4a (unlines
              [ "aa bb cc dd ee"
              , "aa bb cc dd aa"
              , "aa bb cc dd aaa"
              ]) `shouldBe` 2
    describe "part 2" $
        it "examples" $
            day4b (unlines
              [ "abcde fghij"
              , "abcde xyz ecdab"
              , "a ab abc abd abf abj"
              , "iiii oiii ooii oooi oooo"
              , "oiii ioii iioi iiio"
              ]) `shouldBe` 3
