module Day9Spec (spec) where

import Day9 (day9a, day9b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day9a "{}" `shouldBe` 1
            day9a "{{{}}}" `shouldBe` 6
            day9a "{{},{}}" `shouldBe` 5
            day9a "{{{},{},{{}}}}" `shouldBe` 16
            day9a "{<a>,<a>,<a>,<a>}" `shouldBe` 1
            day9a "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 9
            day9a "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` 9
            day9a "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` 3
    describe "part 2" $
        it "examples" $ do
            day9b "<>" `shouldBe` 0
            day9b "<random characters>" `shouldBe` 17
            day9b "<<<<>" `shouldBe` 3
            day9b "<{!>}>" `shouldBe` 2
            day9b "<!!>" `shouldBe` 0
            day9b "<!!!>>" `shouldBe` 0
            day9b "<{o\"i!a,<{i<a>" `shouldBe` 10
