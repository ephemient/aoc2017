{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_ghc -fno-ignore-asserts #-}
module TuringMachine.Internal (FlatTable(..), flatTable) where

import Control.Exception (assert)
import Data.Array.IArray (IArray, accumArray, assocs, bounds, elems)
import Data.Bits (FiniteBits, (.&.), (.|.), clearBit, complement, countLeadingZeros, finiteBitSize, setBit, shiftL, zeroBits)
import Data.Ix (Ix, inRange, index, rangeSize)
import Data.List (elemIndex, nub)
import Data.Maybe (mapMaybe)
import TuringMachine.Base (Instruction(..), Move(..), Table)
import Debug.Trace

data FlatTable a e = FlatTable
  { flatTableTable :: a e e
  , flatTableStateShift :: Int
  , flatTableSymbolMask :: e
  }
deriving instance (Eq (a e e), Eq e) => Eq (FlatTable a e)
deriving instance (Read (a e e), Read e) => Read (FlatTable a e)
deriving instance (Show (a e e), Show e) => Show (FlatTable a e)

flatTable :: (IArray a e, FiniteBits e, Ix e, Num e,
        IArray arr (Instruction state symbol), Ix state, FiniteBits symbol,
        Integral symbol, Ix symbol) =>
    Table arr state symbol -> FlatTable a e
flatTable table =
    assert (stateBits + flatTableStateShift < finiteBitSize flatHalt)
    FlatTable {..} where
    ((minState, minSymbol), (maxState, maxSymbol)) = bounds table
    stateBits = finiteBitSize (0 :: Int) -
        countLeadingZeros (max 1 (rangeSize (minState, maxState)) - 1)
    flatTableStateShift = finiteBitSize minSymbol -
        min (countLeadingZeros minSymbol) (countLeadingZeros maxSymbol)
    flatTableSymbolMask = complement $ complement 0 `shiftL` flatTableStateShift
    flatHalt = complement 0
    flatTableTable = accumArray (flip const) flatHalt
        (0, fromIntegral . subtract 1 $
            rangeSize (minState, maxState) `shiftL` flatTableStateShift)
      [ (key, value)
      | ((state, rawSymbol), InstructionContinue {..}) <- assocs table
      , let stateIndex = index (minState, maxState) state
            stateIndex' = index (minState, maxState) instructionState
            symbol = fromIntegral rawSymbol .&. flatTableSymbolMask
            symbol' = fromIntegral instructionSymbol .&. flatTableSymbolMask
      , let key = fromIntegral
                (stateIndex `shiftL` flatTableStateShift) .|. symbol
            moveBit = case instructionMove of
                MoveLeft -> flip setBit $ finiteBitSize flatHalt - 1
                MoveRight -> flip clearBit $ finiteBitSize flatHalt - 1
            value = moveBit $ fromIntegral
                (stateIndex' `shiftL` flatTableStateShift) .|. symbol'
      ]
