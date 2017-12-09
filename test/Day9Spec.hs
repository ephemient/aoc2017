module Day9Spec (spec) where

import Day9 (day9a, day9b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day9a "{}" `shouldBe` Right 1
            day9a "{{{}}}" `shouldBe` Right 6
            day9a "{{},{}}" `shouldBe` Right 5
            day9a "{{{},{},{{}}}}" `shouldBe` Right 16
            day9a "{<a>,<a>,<a>,<a>}" `shouldBe` Right 1
            day9a "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` Right 9
            day9a "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` Right 9
            day9a "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` Right 3
    describe "part 2" $
        it "examples" $ do
            day9b "<>" `shouldBe` Right 0
            day9b "<random characters>" `shouldBe` Right 17
            day9b "<<<<>" `shouldBe` Right 3
            day9b "<{!>}>" `shouldBe` Right 2
            day9b "<!!>" `shouldBe` Right 0
            day9b "<!!!>>" `shouldBe` Right 0
            day9b "<{o\"i!a,<{i<a>" `shouldBe` Right 10
