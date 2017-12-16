module Day16Spec (spec) where

import Day16 (day16a, day16b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: String
example = "s1,x3/4,pe/b\n"

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day16a 5 example `shouldBe` "baedc"
    describe "part 2" $
        it "examples" $
            day16b 5 2 example `shouldBe` "ceadb"
