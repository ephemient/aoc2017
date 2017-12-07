{-|
Module:         Day7
Description:    <http://adventofcode.com/2017/day/7 Day 7: Recursive Circus>
-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day7 (day7a, day7b) where

import Control.Arrow (second)
import Control.Monad ((>=>), forM_, join)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.Functor (($>))
import Data.List ((\\), find,groupBy, maximumBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
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
findRoot :: (Eq a) => [(a, (b, [a]))] -> Maybe a
findRoot = listToMaybe . uncurry (\\) . second (concatMap snd) . unzip

-- | Returns the most common value and entries whose values do not match.
findUnbalanced :: (Eq b) => [(a, b)] -> Maybe (b, [(a, b)])
findUnbalanced weights
  | groups@(_:_:_) <- groupBy ((==) `on` snd) weights
  , (_, goal):_ <- maximumBy (comparing length) groups
  = Just (goal, filter ((/= goal) . snd) weights)
  | otherwise = Nothing

-- | Returns the total weight of the tree rooted at the given node.
--
-- As a side effect, also 'tell' the corrected weight for nodes whose tree
-- weight does not equal their siblings'.
weighTree :: (MonadFail m, MonadWriter [(a, b)] m, Eq a, Eq b, Num b) =>
    [(a, (b, [a]))] -> a -> m (a, b)
weighTree tree root = do
    let Just (weight, children) = lookup root tree
    childWeights <- mapM (weighTree tree) children
    let unbalanced = findUnbalanced childWeights
    forM_ unbalanced $ \(targetWeight, badChildren) ->
        forM_ badChildren $ \(child, actualWeight) -> do
            let Just (childWeight, _) = lookup child tree
            tell [(child, childWeight + targetWeight - actualWeight)]
    pure (root, weight + sum (map snd childWeights))

day7a :: String -> Maybe String
day7a = parseTree >=> findRoot

day7b :: String -> Maybe Int
day7b input = do
    tree <- parseTree input
    root <- findRoot tree
    join $ listToMaybe . map snd <$> execWriterT (weighTree tree root)
