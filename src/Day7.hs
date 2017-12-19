{-|
Module:         Day7
Description:    <http://adventofcode.com/2017/day/7 Day 7: Recursive Circus>
-}
{-# LANGUAGE FlexibleContexts, PatternGuards #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day7 (day7a, day7b) where

import Control.Arrow ((***))
import Control.Monad ((>=>), guard, join, zipWithM_)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Char (isAlphaNum)
import Data.Functor (($>))
import Data.List (find, group, maximumBy)
import qualified Data.Map.Strict as Map (Map, fromList, lookup)
import Data.Maybe (listToMaybe)
import Data.Monoid (First(..))
import Data.Ord (comparing)
import qualified Data.Set as Set (difference, fromList, lookupMin, unions)
import Text.ParserCombinators.ReadP (char, eof, many1, option, readP_to_S, readS_to_P, satisfy, sepBy1, skipSpaces, string)

-- | Parses a string as a table of node name to node weight and children.
parseTree :: String -> Maybe [(String, (Int, [String]))]
parseTree = mapM (fmap fst . find (null . snd) . readsLine) . lines where
    readsLine = readP_to_S $ do
        name <- many1 $ satisfy isAlphaNum
        skipSpaces; char '('
        weight <- readS_to_P reads
        char ')'; skipSpaces
        children <- option [] $ do
            string "->"; skipSpaces
            sepBy1 (many1 $ satisfy isAlphaNum) (char ',' >> skipSpaces)
        eof $> (name, (weight, children))

-- | Finds a node with no parents.
findRoot :: (Ord a) => [(a, (b, [a]))] -> Maybe a
findRoot = Set.lookupMin . uncurry Set.difference .
           (Set.fromList *** Set.unions . map (Set.fromList . snd)) . unzip

-- | Returns the most common element.
mode :: (Eq a) => [a] -> Maybe a
mode [] = Nothing
mode x = listToMaybe . maximumBy (comparing length) $ group x

-- | Returns the total weight of the tree rooted at the given node.
--
-- As a side effect, also 'tell' the corrected weight for nodes whose tree
-- weight does not equal the majority of their siblings'.
weighTree :: (Monad m, MonadWriter (First b) m, Ord a, Eq b, Num b) =>
    Map.Map a (b, [a]) -> a -> m b
weighTree tree root = do
    let Just (weight, children) = Map.lookup root tree
    childWeights <- mapM (weighTree tree) children
    let Just targetWeight = mode childWeights
        check child actualWeight
          | actualWeight == targetWeight = pure ()
          | Just (childWeight, _) <- Map.lookup child tree
          = tell . First . Just $ childWeight + targetWeight - actualWeight
    zipWithM_ check children childWeights
    pure $ weight + sum childWeights

day7a :: String -> Maybe String
day7a = parseTree >=> findRoot

day7b :: String -> Maybe Int
day7b input = do
    tree <- parseTree input
    root <- findRoot tree
    join $ getFirst <$> execWriterT (weighTree (Map.fromList tree) root)
