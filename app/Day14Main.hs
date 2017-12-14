{-# LANGUAGE OverloadedStrings #-}
module Day14Main (main) where

import Codec.Picture (writePng)
import Codec.Picture.Types (Image, Pixel, Pixel8, PixelRGB8(..), createMutableImage, freezeImage, writePixel)
import Control.Monad (zipWithM_)
import Control.Monad.ST (runST)
import Data.Array.IArray (listArray)
import Data.Array.Unboxed (UArray)
import qualified Data.HashMap.Lazy as M ((!))
import Data.List (scanl', sortOn, transpose)
import Data.Maybe (isJust)
import Data.Scientific (toRealFloat)
import Data.Tuple (swap)
import qualified Data.Vector as V ((!), length)
import Data.Word (Word8)
import Data.Yaml (Value(..), decodeFileEither, prettyPrintParseException)
import Day10 (deriveKey, knotRanges, reverseRange, xorEach)
import Day14 (groupedBits)
import Paths_aoc2017 (getDataFileName)
import Text.Printf (printf)

scale :: Int
scale = 4

drawImage :: (Pixel px) =>
    px -> (Int -> Int -> px) -> [[(Int, Int)]] -> Image px
drawImage bg color groups = runST $ do
    image <- createMutableImage (128 * scale) (128 * scale) bg
    sequence_ $ do
        (d, group) <- zip [0 :: Int ..] $ sortOn (minimum . map swap) groups
        (x, y) <- group
        writePixel image <$> [x * scale .. (x + 1) * scale - 1] <*>
            [y * scale .. (y + 1) * scale - 1] <*> [color d $ length groups - 1]
    freezeImage image

main :: IO ()
main = do
    let getViridis (Object colormaps) = colormaps M.! "viridis"
    Array colors <- getDataFileName "colormaps.yaml" >>=
        fmap (either (error . prettyPrintParseException) getViridis) .
        decodeFileEither
    input <- getDataFileName "day14.txt" >>= readFile
    let white = PixelRGB8 255 255 255
        color d n = PixelRGB8 (part 0) (part 1) (part 2) where
            idx = max 0 . min (V.length colors - 1) . floor $
                fromIntegral d / fromIntegral n * fromIntegral (V.length colors)
            Array rgb = colors V.! idx
            part c = floor $ toRealFloat n * fromIntegral (maxBound :: Pixel8)
                where Number n = rgb V.! c
        keys = [deriveKey $ input ++ "-" ++ show i | i <- [0..127]]
        ranges = takeWhile (any isJust) $ transpose
            [knotRanges (0, 255) key ++ repeat Nothing | key <- keys]
        empty = replicate 128 $ listArray (0, 255) [0..] :: [UArray Int Word8]
        states = scanl' (zipWith $ maybe <*> reverseRange) empty ranges
    zipWithM_ writePng (printf "day14-%04d.png" <$> [0 :: Int ..]) $
        drawImage white color . groupedBits . map (xorEach 16) <$> states
