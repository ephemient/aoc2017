{-|
Module:         Day9
Description:    <http://adventofcode.com/2017/day/9 Day 9: Stream Processing>
-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day9 (day9a, day9b) where

import Control.Monad (void)
import Data.Maybe (catMaybes)
import Text.Parsec (ParseError, ParsecT, (<|>), many, parse, skipMany, try)
import Text.Parsec.Char (anyChar, char, noneOf)

-- | Parse a section of angle-bracket bang-ignored garbage.
parseGarbage :: (Monad m) => ParsecT String u m String
parseGarbage = char '<' >> do
    garbage <- many $ Just <$> noneOf "!>" <|> Nothing <$ (char '!' >> anyChar)
    catMaybes garbage <$ char '>'

-- | @parseGroup depth@ parses a group at @depth@ with nested groups and
-- garbage, returning the total score: the sum of depths of all groups.
parseGroup :: (Monad m) => Int -> ParsecT String u m Int
parseGroup depth = do
    let skipGarbage = skipMany $ void (char ',') <|> void parseGarbage
    skipGarbage >> char '{'
    score <- sum <$> many (try $ parseGroup $! depth + 1)
    skipGarbage >> depth + score <$ char '}'

day9a :: String -> Either ParseError Int
day9a = parse (parseGroup 1) ""

day9b :: String -> Either ParseError Int
day9b = fmap sum . parse (many $ (length <$> parseGarbage) <|> 0 <$ anyChar) ""
