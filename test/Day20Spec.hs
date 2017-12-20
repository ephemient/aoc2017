module Day20Spec (spec) where

import Day20 (day20a, day20b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day20a (unlines
              [ "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"
              , "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"
              ]) `shouldBe` 0
    describe "part 2" $
        it "examples" $
            day20b (unlines
              [ "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>"
              , "p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>"
              , "p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>"
              , "p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>"
              ]) `shouldBe` [3]
