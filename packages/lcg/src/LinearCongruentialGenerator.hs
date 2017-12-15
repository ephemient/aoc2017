{-|
Module:         LinearCongruentialGenerator
Description:    Linear congruential generators
-}
{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
#include "MachDeps.h"
module LinearCongruentialGenerator
  ( lcg32
  , minstdRand
  , minstdRand0
  , next32
  , next32#
  ) where

import GHC.Prim (Word#, not#, quotRemWord2#, remWord#, timesWord#, timesWord2#)
import GHC.Types (Int(I#))
import GHC.Word (Word32(W32#))

#if WORD_SIZE_IN_BITS < 32
#  error "The native word size must be at least 32 bits."
#endif

{-# INLINE next32# #-}
next32# :: (# Word#, Word# #) -> Word# -> Word#
next32# (# a, m #) x
#if WORD_SIZE_IN_BITS < 64
  | W32# (or# (and# a (not# 0x7FFFFFFF##)) (and# m (not# 0x7FFFFFFF##))) /= 0 ||
    I# (mulIntMayOflo# (word2Int# a) (word2Int# m)) /= 0
  = let (# y1, y0 #) = timesWord2# a x
        (# _, r #) = quotRemWord2# y1 y0 m
     in r
  | otherwise
#endif
  = remWord# (timesWord# a x) m

-- | prop> next32 (a, m) x == a * x `mod` x
{-# INLINE next32 #-}
next32 :: (Word32, Word32) -> Word32 -> Word32
next32 (W32# a, W32# m) (W32# x) = W32# (next32# (# a,  m #) x)

-- | prop> (x : lcg (a, m) x) == iterate (next32 (a, m)) x
{-# INLINE lcg32 #-}
lcg32 :: (Word32, Word32) -> Word32 -> [Word32]
lcg32 (W32# a, W32# m) (W32# i) = loop i where
    loop x = W32# y : loop y where y = next32# (# a, m #) x

minstdRand0, minstdRand :: Word32 -> [Word32]
minstdRand0 = lcg32 (16807, 2147483647)
minstdRand  = lcg32 (48271, 2147483647)
