module MortonSpec (spec) where

import Morton (decX, decY, getX, getY, incX, incY, toZ)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck ((===), (==>), property)

spec :: Spec
spec =
    describe "Z" $ do
        it "getX, getY" $ property $ \x y ->
            let z = toZ x y in (x, y) === (getX z, getY z)
        it "incX" $ property $ \x y -> x /= maxBound ==>
            toZ (x + 1) y === incX (toZ x y)
        it "incY" $ property $ \x y -> y /= maxBound ==>
            toZ x (y + 1) === incY (toZ x y)
        it "decX" $ property $ \x y -> x /= minBound ==>
            toZ (x - 1) y === decX (toZ x y)
        it "decY" $ property $ \x y -> y /= minBound ==>
            toZ x (y - 1) === decY (toZ x y)
