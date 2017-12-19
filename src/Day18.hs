{-|
Module:         Day18
Description:    <http://adventofcode.com/2017/day/18 Day 18: Duet>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, NamedFieldPuns, RecordWildCards, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day18 (day18a, day18b) where

import Control.Arrow (first)
import Control.Monad (liftM2)
import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.State (evalState, get, modify, put)
import Control.Monad.Writer (execWriterT, tell)
import Data.Array (Array, (!), bounds, listArray)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Ix (Ix, inRange)
import qualified Data.Map.Lazy as Map (empty, insert, lookup, singleton)
import Data.Map.Lazy (Map)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
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
  , recv :: val -> m val               -- ^ Handler for 'Rcv'.
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
parse :: (Read a) => String -> [Ins String a]
parse = head . mapM parseIns . lines where
    parseIns line = lex line >>= \case
        ("rcv", s) -> [Rcv reg | (reg, "") <- lex s]
        ("snd", s) -> [Snd val | (val, "") <- lex' s]
        ("set", s) -> [Set reg val | (reg, r) <- lex s, (val, "") <- lex' r]
        ("add", s) -> [Add reg val | (reg, r) <- lex s, (val, "") <- lex' r]
        ("mul", s) -> [Mul reg val | (reg, r) <- lex s, (val, "") <- lex' r]
        ("mod", s) -> [Mod reg val | (reg, r) <- lex s, (val, "") <- lex' r]
        ("jgz", s) -> [Jgz cnd jmp | (cnd, r) <- lex' s, (jmp, "") <- lex' r]
    lex' s = (first Right <$> reads s) ++ (first Left <$> lex s)

-- | Evaluate a single instruction.
step :: (Monad m, Ix pc, Num pc, Ord reg, Integral val, Ord val) =>
    MachineSpec m pc reg val -> MachineState pc reg val ->
    m (MachineState pc reg val)
step MachineSpec {..} s@MachineState {..} = case program ! pc of
    Rcv reg -> do
        val <- recv $ loadReg reg
        check s {pc = pc + 1, regs = Map.insert reg val regs}
    Snd (load -> val) -> send val >> check s {pc = pc + 1}
    Set reg val -> mut (flip const) reg val
    Add reg val -> mut (+) reg val
    Mul reg val -> mut (*) reg val
    Mod reg val -> mut mod reg val
    Jgz (load -> cnd) (load -> jmp) ->
        check s {pc = pc + (if cnd > 0 then fromIntegral jmp else 1)}
  where
    loadReg = fromMaybe 0 . flip Map.lookup regs
    load = either loadReg id
    mut op reg@(loadReg -> src) (load -> val) =
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

day18a :: String -> Int64
day18a input = fromJust . getFirst . flip evalState 0 . execWriterT $
    loop spec MachineState {pc = 0, regs = Map.empty} where
    program = parse input
    spec = MachineSpec
      { program = listArray (0, length program - 1) program
      , send = put
      , recv = \case
            0 -> pure 0
            _ -> get >>= liftM2 (<$) id (tell . First . Just)
      }

day18b :: String -> Int
day18b input =
    let program = parse input :: [Ins String Int64]
        recvJust (n, Just val : rest) = val <$ put (n, rest)
        recvJust (n, Nothing:rest) | n > 0 = recvJust (n - 1, rest)
        recvJust _ = throwError ()
        spec = MachineSpec
          { program = listArray (0, length program - 1) program
          , send = \val -> modify (first succ) >> tell [Just val]
          , recv = const $ tell [Nothing] >> get >>= recvJust
          }
        state p = MachineState {pc = 0, regs = Map.singleton "p" p}
        m0 = flip evalState (0, m1) . execWriterT . runExceptT .
             flip catchError (const $ pure ()) . loop spec $ state 0
        m1 = flip evalState (0, m0) . execWriterT . runExceptT .
             flip catchError (const $ pure ()) . loop spec $ state 1
    in length $ catMaybes m1
