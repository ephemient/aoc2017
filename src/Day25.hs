{-|
Module:         Day25
Description:    <http://adventofcode.com/2017/day/25 Day 25: The Halting Problem>
-}
{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards, TypeApplications #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day25 (day25) where

import Control.Monad.ST (ST, runST)
import Data.Array.IArray (Array, array)
import Data.Bits (FiniteBits)
import Data.Char (isSpace)
import Data.Ix (Ix)
import Data.Word (Word8)
import GrowArray (GrowArray, foldGrowArray, newGrowArray, readGrowArray, writeGrowArray)
import Text.ParserCombinators.ReadP (ReadP, (<++), between, char, many1, readP_to_S, readS_to_P, sepBy, string)
import TuringMachine (Instruction(..), Move(..), Table, foldTape, runTuringMachine, withTape, withTuringMachine)

-- | The state of the the Turing machine.
data State = A | B | C | D | E | F deriving (Eq, Ix, Ord, Read)

-- | The setup for a run of a Turing machine.
data Program state value = Program
  { start :: state  -- ^ Initial state of the Turing machine.
  , steps :: Int    -- ^ Number of cycles to run the Turing machine.
  , table :: Table Array state value  -- ^ State transitions.
  }

-- | Parses a description to a 'Program'.
parseProgram :: (Ix state, Read state, Ix value, Num value, Read value) =>
    ReadP (Program state value)
parseProgram = do
    let readp = readS_to_P reads
    start <- between (string "Begin in state ") (string ".\n") readp
    steps <- between (string "Perform a diagnostic checksum after ")
                     (string " steps.\n\n") readp
    transitions <- fmap concat . flip sepBy (many1 $ char '\n') $ do
        state <- between (string "In state ") (string ":\n") readp
        flip sepBy (char '\n') $ do
            value <- between (string "  If the current value is ")
                             (string ":\n") readp
            instructionSymbol <- between (string "    - Write the value ")
                                         (string ".\n") readp
            instructionMove <- between (string "    - Move one slot to the ")
                                       (string ".\n") $
                                       (MoveLeft <$ string "left") <++
                                       (MoveRight <$ string "right")
            instructionState <- between (string "    - Continue with state ")
                                        (string ".") readp
            pure ((state, value), InstructionContinue {..})
    let indices = fst <$> transitions
        minIndex = (minimum $ fst <$> indices, minimum $ snd <$> indices)
        maxIndex = (maximum $ fst <$> indices, maximum $ snd <$> indices)
    pure Program {table = array (minIndex, maxIndex) transitions, ..}

day25 :: String -> IO Int
day25 input = do
    let (Program {..}, _):_ = filter (all isSpace . snd) $
                              readP_to_S (parseProgram @State @Word8) input
    withTuringMachine table $ \tm -> withTape 0 $ \tape -> do
        Right _ <- runTuringMachine tm tape start steps
        foldTape (\a b -> return $ a + fromIntegral b) 0 tape
