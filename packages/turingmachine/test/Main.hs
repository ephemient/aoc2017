{-# LANGUAGE CPP #-}
#include "ghcconfig.h"
module Main (main, spec) where

import qualified TuringMachine.GenericSpec as TMPureSpec (spec)
import qualified TuringMachine.InternalSpec as InternalSpec (spec)
#if linux_TARGET_OS && x86_64_TARGET_ARCH
import qualified TuringMachine.X86_64Spec as TMAsmSpec (spec)
#endif
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "TuringMachine.Internal" InternalSpec.spec
    describe "TuringMachine.Generic" TMPureSpec.spec
#if linux_TARGET_OS && x86_64_TARGET_ARCH
    describe "TuringMachine.X86_64" TMAsmSpec.spec
#endif
