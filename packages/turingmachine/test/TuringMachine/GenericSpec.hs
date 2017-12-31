module TuringMachine.GenericSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldReturn)
import TuringMachine.Generic (foldTape, withTape)

spec :: Spec
spec =
    describe "tape" $
        it "fold" $
            withTape (0 :: Int) (foldTape (\k a -> return $! k + a) 0)
                `shouldReturn` 0
