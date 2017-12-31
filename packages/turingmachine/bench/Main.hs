{-# LANGUAGE CPP, TypeApplications #-}
#include "ghcconfig.h"
module Main (main) where

import Control.Monad ((>=>))
import Criterion.Main (bench, defaultMain, envWithCleanup, nfIO)
import Data.Array.IArray (Array)
import Data.IORef (newIORef, readIORef)
import TuringMachine.Base (Table, createTuringMachine, destroyTuringMachine)
import qualified TuringMachine.Generic as Generic (TuringMachine, foldTape, withTape)
#if linux_TARGET_OS && x86_64_TARGET_ARCH
import qualified TuringMachine.X86_64 as X86_64 (TuringMachine, foldTape, withTape)
#endif

main :: IO ()
main = defaultMain
  [ envWithCleanup
        (createTuringMachine table >>=
            (newIORef @(Generic.TuringMachine Int Int) $!))
        (readIORef >=> destroyTuringMachine) $ \env ->
        bench "TuringMachine.Generic" $ nfIO $ readIORef env >>= \tm ->
        Generic.withTape (0 :: Int) $ \tape ->
        Generic.foldTape (\k a -> return $! k + a) 0 tape
#if linux_TARGET_OS && x86_64_TARGET_ARCH
  , envWithCleanup
        (createTuringMachine table >>=
            (newIORef @(X86_64.TuringMachine Int Int) $!))
        (readIORef >=> destroyTuringMachine) $ \env ->
        bench "TuringMachine.X86_64" $ nfIO $ readIORef env >>= \tm ->
        X86_64.withTape (0 :: Int) $ \tape ->
        X86_64.foldTape (\k a -> return $! k + a) 0 tape
#endif
  ] where
    table :: Table Array Int Int
    table = undefined
