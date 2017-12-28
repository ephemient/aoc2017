{-# LANGUAGE RecordWildCards, TypeApplications #-}
module TuringMachine.InternalSpec (spec) where

import Control.Exception (AssertionFailed(..), evaluate)
import Data.Array.IArray (Array, assocs, listArray)
import Data.Array.Unboxed (UArray)
import Data.Bits (bit)
import Data.Word (Word8, Word16)
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldThrow)
import TuringMachine.Base (Instruction(..), Move(..), Table)
import TuringMachine.Internal (FlatTable(..), flatTable)

spec :: Spec
spec = do
    let table = listArray @Array ((0, 0 :: Int), (4, 25)) $
            zipWith3 InstructionContinue ([0..4] >>= replicate 26)
            (cycle [0..25]) (cycle [MoveLeft, MoveRight])
    describe "flatTable" $ do
        it "asserts on too many states x symbols" $  do
            let ~FlatTable {..} = flatTable @UArray @Word8 table
            evaluate flatTableTable `shouldThrow` \(AssertionFailed _) -> True
        it "returns transitions" $ do
            let ~FlatTable {..} = flatTable @UArray @Word16 table
            assocs flatTableTable `shouldContain` [(0, bit 15), (1, 1)]
            flatTableStateShift `shouldBe` 5
            flatTableSymbolMask `shouldBe` 31
