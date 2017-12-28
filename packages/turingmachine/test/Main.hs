module Main (main, spec) where

import qualified TuringMachine.InternalSpec as InternalSpec
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "TuringMachine.Internal" InternalSpec.spec
