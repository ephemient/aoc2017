{-|
Module:         Day18
Description:    <http://adventofcode.com/2017/day/18 Day 18: Duet>
-}
{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day18 (day18a, day18b) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (BlockedIndefinitelyOnMVar(..), bracket, handle)
import Control.Monad (when)
import Control.Monad.State (evalState, get, put)
import Control.Monad.Writer (execWriterT, tell)
import Data.Array (Array, (!), bounds, listArray)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Ix (Ix, inRange)
import qualified Data.Map.Lazy as Map (empty, insert, lookup, singleton)
import Data.Map.Lazy (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (First(..))
import Text.Read (lex)

-- | A single instruction.
--
-- 'Either' represents either a register reference or an immediate value.
data Ins reg val
  = Rcv reg
  | Snd (Either reg val)
  | Set reg (Either reg val)
  | Add reg (Either reg val)
  | Mul reg (Either reg val)
  | Mod reg (Either reg val)
  | Jgz (Either reg val) (Either reg val)

-- | A specification for how to run a machine.
data MachineSpec m pc reg val = MachineSpec
  { program :: Array pc (Ins reg val)  -- ^ The list of instructions.
  , send :: val -> m ()                -- ^ Handler for 'Snd'.
  , recv :: m val                      -- ^ Handler for 'Rcv'.
  }

-- | The current state of a machine.
data MachineState pc reg val
    -- | The machine is running.
  = MachineState
      { pc :: pc             -- ^ The program counter.
      , regs :: Map reg val  -- ^ The registers.
      }
    -- | The machine is stopped.
  | MachineTerminated

-- | Parse an assembly listing to instructions.
parse :: String -> [Ins String Int]
parse = head . mapM parseIns . lines where
    parseIns line = lex line >>= \case
        ("rcv", s) -> [Rcv reg | (reg, "") <- lex s]
        ("snd", s) -> [Snd val | (val, "") <- lex' s]
        ("set", s) -> [Set reg val | (reg, r) <- lex s, (val, "") <- lex' r]
        ("add", s) -> [Add reg val | (reg, r) <- lex s, (val, "") <- lex' r]
        ("mul", s) -> [Mul reg val | (reg, r) <- lex s, (val, "") <- lex' r]
        ("mod", s) -> [Mod reg val | (reg, r) <- lex s, (val, "") <- lex' r]
        ("jgz", s) ->
            [Jgz cond dest | (cond, r) <- lex' s, (dest, "") <- lex' r]
    lex' s = [(Right val, r) | (val, r) <- reads s] ++
             [(Left reg, r) | (reg, r) <- lex s]

-- | Evaluate a single instruction.
step :: (Monad m, Ix pc, Num pc, Ord reg, Integral val, Ord val) =>
    MachineSpec m pc reg val -> MachineState pc reg val ->
    m (MachineState pc reg val)
step MachineSpec {..} s@MachineState {..} = case program ! pc of
    Rcv reg -> do
        val <- recv
        check s {pc = pc + 1, regs = Map.insert reg val regs}
    Snd (load -> val) -> send val >> check s {pc = pc + 1}
    Set reg val -> mut (flip const) reg val
    Add reg val -> mut (+) reg val
    Mul reg val -> mut (*) reg val
    Mod reg val -> mut mod reg val
    Jgz (load -> cond) (load -> dest) ->
        check s {pc = pc + (if cond > 0 then fromIntegral dest else 1)}
  where
    load = either (fromMaybe 0 . flip Map.lookup regs) id
    mut op reg@(load . Left -> src) (load -> val) =
        check s {pc = pc + 1, regs = Map.insert reg (op src val) regs}
    check MachineState {pc} | not (inRange (bounds program) pc) =
        pure MachineTerminated
    check state = pure state

-- | Run a machine until its state reaches 'MachineTerminated'.
loop :: (Monad m, Ix pc, Num pc, Ord reg, Integral val, Ord val) =>
    MachineSpec m pc reg val -> MachineState pc reg val -> m ()
loop spec = loop' where
    loop' MachineTerminated = pure ()
    loop' state = step spec state >>= loop'

day18a :: String -> Int
day18a input = fromJust . getFirst . flip evalState 0 . execWriterT $
    loop spec MachineState {pc = 0, regs = Map.empty} where
    program = parse input
    spec = MachineSpec
      { program = listArray (0, length program - 1) program
      , send = put
      , recv = do
            val <- get
            when (val /= 0) . tell . First $ Just val
            pure val
      }

day18b :: String -> IO Int
day18b input = do
    counter <- newIORef 0
    chan0 <- newChan
    chan1 <- newChan
    let program = parse input
        spec0 = MachineSpec
          { program = listArray (0, length program - 1) program
          , send = writeChan chan1
          , recv = readChan chan0
          }
        spec1 = spec0
          { send = (modifyIORef' counter (+ 1) >>) . writeChan chan0
          , recv = readChan chan1
          }
    handle (\BlockedIndefinitelyOnMVar -> return ()) . bracket
        (forkIO $ loop spec0 MachineState {pc = 0, regs = Map.singleton "p" 0})
        killThread . const $
        loop spec1 MachineState {pc = 0, regs = Map.singleton "p" 1}
    readIORef counter
