{-|
Module:         Day23
Description:    <http://adventofcode.com/2017/day/23 Day 23: Coprocessor Conflagration>
-}
{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards, TypeApplications, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day23 (day23a, day23b) where

import Data.List (findIndices)
import Data.Array.IArray (IArray, (!), (//), listArray)
import Data.Array.Unboxed (UArray)
import Data.Ix (Ix)
import Math.NumberTheory.Primes (isPrime)
import Text.Read (readMaybe)

-- | A single instruction.
--
-- 'Either' represents either a register reference or an immediate value.
data Ins reg imm
  = Set {reg :: reg, val :: Either reg imm}
  | Sub {reg :: reg, val :: Either reg imm}
  | Mul {reg :: reg, val :: Either reg imm}
  | Jnz {cnd :: Either reg imm, jmp :: Either reg imm}

-- | The current state of a machine.
data State a reg imm = State {pc :: Int, regs :: a reg imm}

-- | Parses an immediate value or a single character register name.
parseVal :: (Read imm) => String -> Either Char imm
parseVal (readMaybe -> Just imm) = Right imm
parseVal [reg] = Left reg

-- | Parses an assembly listing to instructions.
parse :: (Read imm) => String -> [Ins Char imm]
parse = map (parseIns . words) . lines where
    parseIns ["set", [reg], parseVal -> val] = Set {..}
    parseIns ["sub", [reg], parseVal -> val] = Sub {..}
    parseIns ["mul", [reg], parseVal -> val] = Mul {..}
    parseIns ["jnz", parseVal -> cnd, parseVal -> jmp] = Jnz {..}

-- | Evaluates a single instruction.
step :: (IArray a imm, Ix reg, Integral imm) =>
    [Ins reg imm] -> State a reg imm -> Maybe (State a reg imm)
step ins State {pc} | pc < 0 || null (drop pc ins) = Nothing
step ins state@State {..} = Just $ case ins !! pc of
    Set {..} -> State (pc + 1) (regs // [(reg, load val)])
    Sub {..} -> State (pc + 1) (regs // [(reg, regs ! reg - load val)])
    Mul {..} -> State (pc + 1) (regs // [(reg, regs ! reg * load val)])
    Jnz {..}
      | load cnd == 0 -> state {pc = pc + 1}
      | otherwise -> state {pc = pc + fromIntegral (load jmp)}
  where
    load = either (regs !) id

-- | Evaluates the @f = 0 if !isPrime(b)@ sequence, or a single instruction.
stepOptimized :: (IArray a imm, Integral imm) =>
    [Ins Char imm] -> State a Char imm -> Maybe (State a Char imm)
stepOptimized ins State {pc} | pc < 0 = Nothing
stepOptimized ins state@State {..} = case drop pc ins of
    ( Set 'd' (Right 2) :
      Set 'e' (Right 2) :
      Set 'g' (Left 'd') :
      Mul 'g' (Left 'e') :
      Sub 'g' (Left 'b') :
      Jnz (Left 'g') (Right 2) :
      Set 'f' (Right 0) :
      Sub 'e' (Right (-1)) :
      Set 'g' (Left 'e') :
      Sub 'g' (Left 'b') :
      Jnz (Left 'g') (Right (-8)) :
      Sub 'd' (Right (-1)) :
      Set 'g' (Left 'd') :
      Sub 'g' (Left 'b') :
      Jnz (Left 'g') (Right (-13)) :
      _) -> Just . State (pc + 15) $ regs //
            ( [ ('d', regs ! 'b'), ('e', regs ! 'b'), ('g', 0) ] ++
              [('f', 0) | not . isPrime . fromIntegral $ regs ! 'b'] )
    _ -> step ins state

-- | Iterates a function until 'Nothing'.
iterateMaybe :: (a -> Maybe a) -> Maybe a -> [a]
iterateMaybe f = maybe [] $ (:) <*> iterateMaybe f . f

day23a :: String -> Int
day23a input = countMuls $ iterateMaybe (step ins) $ Just state0 where
    ins = parse input
    muls = findIndices (\case Mul {} -> True; _ -> False) ins
    countMuls = length . filter (`elem` muls) . map pc
    state0 = State {pc = 0, regs = listArray @UArray @Int ('a', 'h') $ repeat 0}

day23b :: String -> Int
day23b input =
    regs (last . iterateMaybe (stepOptimized ins) $ Just state0) ! 'h' where
    ins = parse input
    state0 =
        State {pc = 0, regs = listArray @UArray @Int ('a', 'h') $ 1 : repeat 0}
