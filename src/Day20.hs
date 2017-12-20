{-|
Module:         Day20
Description:    <http://adventofcode.com/2017/day/20 Day 20: Particle Stream>
-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day20 (day20a, day20b) where

import Control.Arrow (second)
import Data.Function (on)
import Data.List (find, groupBy, minimumBy, sortOn, tails)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Ord (comparing)

-- | A 3D vector.
data Vec3 a = Vec3 {x :: !a, y :: !a, z :: !a} deriving (Eq, Ord)

-- | A point with position, velocity, and acceleration in 3D space.
data Point a = Point {pos :: !(Vec3 a), vel :: !(Vec3 a), acc :: !(Vec3 a)}

-- | Reads points.
parse :: (Read a) => String -> [Point a]
parse = map parseLine . lines where
    parseLine line = Point {..} where
        [parseVec3 -> pos, parseVec3 -> vel, parseVec3 -> acc] = words line
    parseVec3 s = Vec3 {..} where
        t = takeWhile (/= '>') . tail $ dropWhile (/= '<') s
        (x, y, z) = read $ '(' : t ++ ")"

-- | Pointwise addition.
(*+*) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
Vec3 x y z *+* Vec3 u v w = Vec3 (x + u) (y + v) (z + w)
infixl 3 *+*

-- | Pointwise subtraction.
(*-*) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
Vec3 x y z *-* Vec3 u v w = Vec3 (x - u) (y - v) (z - w)
infixl 3 *-*

-- | Performs one timestep of velocity and position updates.
step :: (Num a) => Point a -> Point a
step p@Point {..} = p {pos = pos *+* v', vel = v'} where v' = vel *+* acc

-- | Returns the Manhattan distance of a vector to the origin.
manhattan :: (Num a) => Vec3 a -> a
manhattan Vec3 {..} = abs x + abs y + abs z

-- | Returns true if repeated 'step' will not change the sign of any component
-- of velocity and position.
signumsMatch :: (Eq a, Num a) => Point a -> Bool
signumsMatch Point {..} =
    (x acc == 0 || signum (x acc) == signum (x vel)) &&
    (x acc == 0 && x vel == 0 || signum (x vel) == signum (x pos)) &&
    (y acc == 0 || signum (y acc) == signum (y vel)) &&
    (y acc == 0 && y vel == 0 || signum (y vel) == signum (y pos)) &&
    (z acc == 0 || signum (z acc) == signum (z vel)) &&
    (z acc == 0 && z vel == 0 || signum (z vel) == signum (z pos))

-- | Filters out all elements which are duplicated under transformation.
collide :: (Ord a) => (b -> a) -> [b] -> [b]
collide f points =
    filter ((== Just 1) . (`Map.lookup` counts) . f) points where
    counts = Map.fromListWith (+) [(f p, 1) | p <- points]

day20a :: String -> Int
day20a input = fst $ minimumBy (comparing $ manhattan . pos . snd) points'''
  where
    points = zip [0..] $ parse input
    minAcc = minimum $ manhattan . acc . snd <$> points
    points' = filter ((== minAcc) . manhattan . acc . snd) points
    points'':_ = dropWhile (any $ not . signumsMatch . snd) $
                 iterate (map $ second step) points'
    minVel = minimum $ manhattan . vel . snd <$> points''
    points''' = filter ((== minVel) . manhattan . vel . snd) points''

day20b :: String -> [Int]
day20b =
    map fst . fromJust . find (done . map snd) .
    iterate (collide (pos . snd) . map (second step)) .
    sortOn (manhattan . acc . snd) . zip [0..] . parse where
    done points = all signumsMatch points && and
      [ dAcc <== dVel && dVel <== dPos
      | let sgnVec3 Vec3 {..} = (signum x, signum y, signum z)
            Vec3 x y z <== Vec3 u v w =
                abs x <= abs u && abs y <= abs v && abs z <= abs w
            infix 4 <==
      , octant <- groupBy ((==) `on` sgnVec3 . pos) points
      , p:ps <- tails octant
      , q <- ps
      , let dPos = pos p *-* pos q
            dVel = vel p *-* vel q
            dAcc = acc p *-* acc q
      ]
