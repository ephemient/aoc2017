{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, NondecreasingIndentation, RecordWildCards #-}
module GrowArray (GrowArray, foldGrowArray, newGrowArray, readGrowArray, writeGrowArray) where

import Control.Monad.ST (ST)
import Data.Bits (FiniteBits, countLeadingZeros, finiteBitSize, shiftL)
import Data.Ix (Ix, inRange, index, rangeSize)
import Data.Primitive (Prim)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.STRef.Unboxed (STRefU, newSTRefU, readSTRefU, writeSTRefU)
import Data.Vector.Unboxed.Mutable as V (STVector, Unbox, length, replicate, unsafeCopy, unsafeRead, unsafeSlice, unsafeWrite)

-- | A growable array with unboxed indices and elements.
data GrowArray s i e = GrowArray
  { growArrayDef :: e
  , growArrayMin :: STRefU s i
  , growArrayMax :: STRefU s i
  , growArrayVec :: STRef s (V.STVector s e)
  }

-- | Creates a new growable array, pre-allocating the given indices with the
-- given default value.
newGrowArray :: (FiniteBits i, Ix i, Num i, Prim i, V.Unbox e) =>
    (i, i) -> e -> ST s (GrowArray s i e)
newGrowArray (min, max) def | min <= max =
    GrowArray def <$> newSTRefU min <*> newSTRefU (min + size - 1) <*>
    (newSTRef =<< V.replicate size def) where
    size = 1 `shiftL` (finiteBitSize max - countLeadingZeros (max - min))

getBounds :: (Prim i) => GrowArray s i e -> ST s (i, i)
getBounds GrowArray {..} =
    (,) <$> readSTRefU growArrayMin <*> readSTRefU growArrayMax

-- | Reads an element at an index. Returns the default value if unallocated.
{-# INLINE readGrowArray #-}
readGrowArray :: (Ix i, Prim i, V.Unbox e) => GrowArray s i e -> i -> ST s e
readGrowArray arr@GrowArray {..} i = do
    bounds <- getBounds arr
    if inRange bounds i
    then flip V.unsafeRead (index bounds i) =<< readSTRef growArrayVec
    else pure growArrayDef

-- | Writes an element at an index, growing the underlying storage as needed.
{-# INLINE writeGrowArray #-}
writeGrowArray :: (FiniteBits i, Ix i, Num i, Prim i, V.Unbox e) =>
    GrowArray s i e -> i -> e -> ST s ()
writeGrowArray arr@GrowArray {..} i e = do
    bounds@ ~(l, h) <- getBounds arr
    if inRange bounds i
    then readSTRef growArrayVec >>= \v -> V.unsafeWrite v (index bounds i) e
    else do
    let bits = finiteBitSize i - countLeadingZeros (max h i - min l i)
        oldSize = rangeSize bounds
        newSize = 1 `shiftL` bits
    old <- readSTRef growArrayVec
    new <- V.replicate newSize growArrayDef
    let min = if i < l then h - newSize + 1 else l
        max = if i < l then h else l + newSize - 1
        start = if i < l then newSize - oldSize else 0
        dest = V.unsafeSlice start oldSize new
    V.unsafeCopy dest old
    V.unsafeWrite new (index (min, max) i) e
    writeSTRefU growArrayMin min
    writeSTRefU growArrayMax max
    writeSTRef growArrayVec new

-- | folds a function over all elements, potentially including default values at
-- indices that were not explicitly allocated or written.
{-# INLINE foldGrowArray #-}
foldGrowArray :: (Prim i, V.Unbox e) =>
    (a -> e -> ST s a) -> a -> GrowArray s i e -> ST s a
foldGrowArray f z arr@GrowArray{..} = do
    vec <- readSTRef growArrayVec
    let go i a = if i >= V.length vec then pure a else
                 V.unsafeRead vec i >>= f a >>= go (i + 1)
    go 0 z
