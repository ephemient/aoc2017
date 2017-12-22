module Morton (Z(..), decX, decY, getX, getY, incX, incY, toZ) where

import Data.Bits ((.&.), (.|.), complement, finiteBitSize, shiftL, xor)
import Data.Int (Int32)
import Data.Ix (Ix)
import Data.List (foldl', transpose)
import Data.Word (Word64)

-- | 2-D Z-order.
newtype Z = Z {getZ :: Word64} deriving (Bounded, Eq, Ix, Ord)

instance Show Z where
    showsPrec n z = showsPrec n (getX z, getY z)

instance Read Z where
    readsPrec n s = [(toZ x y, r) | ((x, y), r) <- readsPrec n s]

getX :: Z -> Int32
getX (Z z) = get 0 z

getY :: Z -> Int32
getY (Z z) = get 1 z

incX :: Z -> Z
incX (Z z)
  | z .&. 1 == 0 = Z $ incX' z
  | z .&. 0x5555555555555555 == 1 = Z $ z `xor` 1
  | otherwise = Z $ decX' z

incY :: Z -> Z
incY (Z z)
  | z .&. 2 == 0 = Z $ incY' z
  | z .&. 0xAAAAAAAAAAAAAAAA == 2 = Z $ z `xor` 2
  | otherwise = Z $ decY' z

decX :: Z -> Z
decX (Z z)
  | z .&. 1 /= 0 = Z $ incX' z
  | z .&. 0x5555555555555555 == 0 = Z $ z `xor` 1
  | otherwise = Z $ decX' z

decY :: Z -> Z
decY (Z z)
  | z .&. 2 /= 0 = Z $ incY' z
  | z .&. 0xAAAAAAAAAAAAAAAA == 0 = Z $ z `xor` 2
  | otherwise = Z $ decY' z

toZ :: Int32 -> Int32 -> Z
toZ x y = Z $ foldl' (.|.) 0
    [1 `shiftL` i | (i, True) <- zip [0..] $ bits x +/+ bits y] where
    bits n = if n < 0 then True : bits' (complement n) else False : bits' n
    bits' n = [n .&. 1 `shiftL` i /= 0 | i <- [0 .. finiteBitSize n - 2]]
    x +/+ y = concat $ transpose [x, y]
    infix 5 +/+

get :: Int -> Word64 -> Int32
get i z = if s then complement n else n where
    n = foldl' (.|.) 0 [1 `shiftL` j | (j, True) <- zip [0..] bits]
    s:bits = [z .&. 1 `shiftL` j /= 0 | j <- [i, i + 2 .. finiteBitSize z - 1]]

incX' :: Word64 -> Word64
incX' z = x .&. 0x5555555555555555 .|. y where
    x = (z .|. 0xAAAAAAAAAAAAAAAA) + 4
    y = z .&. 0xAAAAAAAAAAAAAAAA

incY' :: Word64 -> Word64
incY' z = x .|. y .&. 0xAAAAAAAAAAAAAAAA where
    x = z .&. 0x5555555555555555
    y = (z .|. 0x5555555555555555) + 8

decX' :: Word64 -> Word64
decX' z = x .&. 0x5555555555555555 .|. y where
    x = (z .&. 0x5555555555555555) - 4
    y = z .&. 0xAAAAAAAAAAAAAAAA

decY' :: Word64 -> Word64
decY' z = x .|. y .&. 0xAAAAAAAAAAAAAAAA where
    x = z .&. 0x5555555555555555
    y = (z .&. 0xAAAAAAAAAAAAAAAA) - 8
