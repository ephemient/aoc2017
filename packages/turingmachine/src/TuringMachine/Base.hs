{-# LANGUAGE FlexibleContexts, FunctionalDependencies #-}
module TuringMachine.Base (Instruction(..), Move(..), Table, Tape(..), TuringMachine(..)) where

import Data.Array.IArray (IArray)
import Data.Ix (Ix)

data Move = MoveLeft | MoveRight
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

data Instruction state symbol
  = InstructionContinue
      { instructionState :: state
      , instructionSymbol :: symbol
      , instructionMove :: Move
      }
  | InstructionHalt
  deriving (Eq, Read, Show)

type Table a state symbol = a (state, symbol) (Instruction state symbol)

class (Monad m) => Tape m symbol tape | tape -> m, tape -> symbol where
    createTape :: symbol -> m tape
    destroyTape :: tape -> m ()
    readTape :: tape -> m symbol
    writeTape :: tape -> symbol -> m ()
    moveLeftTape :: tape -> m ()
    moveRightTape :: tape -> m ()

class (Tape m symbol tape) =>
    TuringMachine m state symbol tape tm | tm -> state, tm -> tape where
    createTuringMachine ::
        (IArray a (Instruction state symbol), Ix state, Ix symbol) =>
        Table a state symbol -> m tm
    destroyTuringMachine :: tm -> m ()
    stepTuringMachine :: tm -> tape -> state -> m (Maybe state)
    runTuringMachine :: tm -> tape -> state -> Int -> m (Either Int state)
