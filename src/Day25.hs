{-|
Module:         Day25
Description:    <http://adventofcode.com/2017/day/25 Day 25: The Halting Problem>
-}
{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards, TypeApplications #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day25 (day25) where

import Control.Monad.ST (ST, runST)
import Data.Bits (FiniteBits)
import Data.Char (isSpace)
import Data.Ix (Ix)
import Data.List (uncons)
import qualified Data.Map.Lazy as Map ((!), fromList)
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe)
import Data.Primitive (Prim)
import qualified Data.Vector.Unboxed as V (Unbox)
import Data.Word (Word8)
import GrowArray (GrowArray, foldGrowArray, newGrowArray, readGrowArray, writeGrowArray)
import Text.ParserCombinators.ReadP (ReadP, (<++), between, char, many1, readP_to_S, readS_to_P, sepBy, string)

-- | A movement of the cursor of the Turing machine on its tape.
data Move = MoveLeft | MoveRight

-- | The state of the the Turing machine.
data State = A | B | C | D | E | F deriving (Eq, Ord, Read)

-- | An operation to be performed by the Turing machine.
data Op state value = Op {write :: value, move :: Move, next :: state}

-- | The setup for a run of a Turing machine.
data Program state value = Program
  { start :: state  -- ^ Initial state of the Turing machine.
  , steps :: Int    -- ^ Number of cycles to run the Turing machine.
    -- | State transitions.
  , transitions :: Map (state, value) (Op state value)
  }

-- | Parses a description to a 'Program'.
parseProgram :: (Ord state, Read state, Ord value, Read value) =>
    ReadP (Program state value)
parseProgram = do
    let readp = readS_to_P reads
    start <- between (string "Begin in state ") (string ".\n") readp
    steps <- between (string "Perform a diagnostic checksum after ")
                     (string " steps.\n\n") readp
    transitions <- fmap (Map.fromList . concat) .
                   flip sepBy (many1 $ char '\n') $ do
        state <- between (string "In state ") (string ":\n") readp
        flip sepBy (char '\n') $ do
            value <- between (string "  If the current value is ")
                             (string ":\n") readp
            write <- between (string "    - Write the value ")
                             (string ".\n") readp
            move <- between (string "    - Move one slot to the ")
                            (string ".\n") $
                            (MoveLeft <$ string "left") <++
                            (MoveRight <$ string "right")
            next <- between (string "    - Continue with state ")
                            (string ".") readp
            pure ((state, value), Op {..})
    pure Program {..}

-- | Performs a single state transition.
step :: (Ord state, Num value, Ord value, V.Unbox value, FiniteBits pos, Num pos, Ix pos, Prim pos) =>
    Map (state, value) (Op state value) -> GrowArray s pos value ->
    (pos, state) -> ST s (pos, state)
step transitions tape (pos, state) = do
    val <- readGrowArray tape pos
    let Op {..} = transitions Map.! (state, val)
        pos' = case move of MoveLeft -> pos - 1; MoveRight -> pos + 1
    (pos', next) <$ writeGrowArray tape pos write

day25 :: String -> Int
day25 input = runST $ newGrowArray @Int (0, 0) 0 >>= loop steps (0, start) where
    (Program {..}, _):_ = filter (all isSpace . snd) $
                          readP_to_S (parseProgram @State @Word8) input
    loop 0 _ tape = foldGrowArray (\a e -> pure $! a + fromIntegral e) 0 tape
    loop n tm tape = step transitions tape tm >>= \tm -> loop (n - 1) tm tape
