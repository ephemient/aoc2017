{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, NondecreasingIndentation, RecordWildCards #-}
module TuringMachine.Generic (Instruction(..), Move(..), Table, Tape, TuringMachine, createTape, createTuringMachine, destroyTape, destroyTuringMachine, foldTape, moveLeftTape, moveRightTape, readTape, runTuringMachine, stepTuringMachine, withTape, withTuringMachine, writeTape) where

import Control.Exception (bracket)
import Control.Monad.ST (RealWorld, stToIO)
import Data.Array.IArray (IArray, (!), bounds)
import Data.Array.Unboxed (UArray)
import Data.Bits (FiniteBits, (.&.), (.|.), clearBit, complement, finiteBitSize, shiftL, shiftR, testBit, zeroBits)
import Data.Bool (bool)
import Data.IORef.Unboxed (IORefU, modifyIORefU, newIORefU, readIORefU)
import Data.Ix (Ix, index, range)
import Data.List (elemIndex)
import Data.Vector.Unboxed.Base (Unbox)
import GrowArray (GrowArray, foldGrowArrayIO, newGrowArray, readGrowArray, writeGrowArray)
import qualified TuringMachine.Base as Base (Tape, TuringMachine)
import TuringMachine.Base (Instruction(..), Move(..), Table, createTape, createTuringMachine, destroyTape, destroyTuringMachine, moveLeftTape, moveRightTape, readTape, runTuringMachine, stepTuringMachine, writeTape)
import TuringMachine.Internal (FlatTable(..), flatTable)

data Tape a = Tape
  { tapeData :: GrowArray RealWorld Int a
  , tapePos :: IORefU Int
  }

data TuringMachine state symbol = TuringMachine
  { turingMachineTable :: UArray Int Int
  , turingMachineStateRange :: (state, state)
  , turingMachineStateShift :: Int
  , turingMachineSymbolMask :: Int
  }

instance (FiniteBits a, Unbox a) => Base.Tape IO a (Tape a) where
    createTape zero = Tape <$> stToIO (newGrowArray (0, 0) zero) <*> newIORefU 0
    destroyTape _ = return ()
    {-# INLINE readTape #-}
    readTape Tape {..} = readIORefU tapePos >>= stToIO . readGrowArray tapeData
    {-# INLINE writeTape #-}
    writeTape Tape {..} symbol =
        readIORefU tapePos >>= stToIO . (writeGrowArray tapeData `flip` symbol)
    {-# INLINE moveLeftTape #-}
    moveLeftTape Tape {tapePos} = modifyIORefU tapePos pred
    {-# INLINE moveRightTape #-}
    moveRightTape Tape {tapePos} = modifyIORefU tapePos succ

instance (Ix state, FiniteBits symbol, Integral symbol, Unbox symbol) =>
    Base.TuringMachine IO state symbol (Tape symbol)
    (TuringMachine state symbol) where
    createTuringMachine table = return TuringMachine
      { turingMachineTable = flatTableTable
      , turingMachineStateRange = (minState, maxState)
      , turingMachineStateShift = flatTableStateShift
      , turingMachineSymbolMask = flatTableSymbolMask
      } where
        ((minState, _), (maxState, _)) = bounds table
        FlatTable {..} = flatTable table
    destroyTuringMachine _ = return ()
    stepTuringMachine tm@TuringMachine {..} tape =
        fmap (fmap $ (range turingMachineStateRange !!) .
                (`shiftR` turingMachineStateShift)) .
        stepTuringMachine' tm tape . (`shiftL` turingMachineStateShift) .
        index turingMachineStateRange
    runTuringMachine tm@TuringMachine {..} tape = runTuringMachine' . Just .
        (`shiftL` turingMachineStateShift) . index turingMachineStateRange where
        runTuringMachine' Nothing steps = return $ Left steps
        runTuringMachine' (Just state) 0 = return . Right $
            range turingMachineStateRange !!
            (state `shiftR` turingMachineStateShift)
        runTuringMachine' (Just state) steps =
            stepTuringMachine' tm tape state >>=
            flip runTuringMachine' (steps - 1)

{-# INLINE foldTape #-}
foldTape :: (FiniteBits a, Unbox a) => (k -> a -> IO k) -> k -> Tape a -> IO k
foldTape f z Tape {tapeData} = foldGrowArrayIO f z tapeData

withTape :: (FiniteBits a, Unbox a) => a -> (Tape a -> IO b) -> IO b
withTape zero = bracket (createTape zero) destroyTape

withTuringMachine :: (IArray arr (Instruction state symbol), Ix state,
        FiniteBits symbol, Integral symbol, Ix symbol, Unbox symbol) =>
    Table arr state symbol -> (TuringMachine state symbol -> IO a) -> IO a
withTuringMachine table =
    bracket (createTuringMachine table) destroyTuringMachine

{-# INLINE stepTuringMachine' #-}
stepTuringMachine' :: (FiniteBits symbol, Integral symbol, Unbox symbol) =>
    TuringMachine state symbol -> Tape symbol -> Int -> IO (Maybe Int)
stepTuringMachine' TuringMachine {..} tape state = do
    symbol <- readTape tape
    let idx = state .|.  fromIntegral symbol .&. turingMachineSymbolMask
        idx' = turingMachineTable ! idx
    if idx' == complement 0 then return Nothing else Just <$> do
    let moveBit = finiteBitSize idx' - 1
        move = bool moveRightTape moveLeftTape $ testBit idx' moveBit
        state' = clearBit idx' moveBit .&. complement turingMachineSymbolMask
        symbol' = fromIntegral $ idx' .&. turingMachineSymbolMask
    writeTape tape symbol'
    move tape
    return state'
