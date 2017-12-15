module Main (main) where

import Criterion.Main (bench, defaultMain, nf)
import LinearCongruentialGenerator (minstdRand, minstdRand0)

main :: IO ()
main = defaultMain
  [ bench "minstdRand0" $ nf (sum . take 40000000 . minstdRand0) 1
  , bench "minstdRand" $ nf (sum . take 40000000 . minstdRand) 1
  ]
