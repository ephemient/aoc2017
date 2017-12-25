{-|
Module:         Day25
Description:    <http://adventofcode.com/2017/day/25 Day 25: The Halting Problem>
-}
{-# LANGUAGE NamedFieldPuns, NoMonomorphismRestriction, RecordWildCards, TypeApplications, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day25 (day25) where

import Data.Char (isSpace)
import Data.List (uncons)
import qualified Data.Map.Lazy as Map ((!), fromList)
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe)
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

-- | A cursor view of a Turing machine's tape.
data Tape a = Tape {before :: [a], cur :: a, after :: [a]}

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

-- | Writes a value then moves the cursor.
writeMoveTape :: (Num a) => a -> Move -> Tape a -> Tape a
writeMoveTape new MoveLeft
    Tape {before = (fromMaybe (0, []) . uncons -> (x, xs)), after = ys} =
    Tape {before = xs, cur = x, after = new:ys}
writeMoveTape new MoveRight
    Tape {before = xs, after = (fromMaybe (0, []) . uncons -> (y, ys))} =
    Tape {before = new:xs, cur = y, after = ys}

-- | Performs a single state transition.
step :: (Ord state, Num value, Ord value) =>
    Map (state, value) (Op state value) ->
    (state, Tape value) -> (state, Tape value)
step transitions (state, tape) = (next, writeMoveTape write move tape) where
    Op {..} = transitions Map.! (state, cur tape)

day25 :: String -> Int
day25 input = sum before + cur + sum after where
    (Program {..}, _):_ =
        filter (all isSpace . snd) $ readP_to_S (parseProgram @State @Int) input
    (_, Tape {..}) = iterate (step transitions) (start, Tape [] 0 []) !! steps
