module TuringMachine.X86_64Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldReturn)
import TuringMachine.X86_64 (foldTape, withTape)

spec :: Spec
spec =
    describe "tape" $
        it "fold" $
            withTape (0 :: Int) (foldTape (\k a -> return $! k + a) 0)
                `shouldReturn` 0
