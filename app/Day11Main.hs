{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Day11Main (main) where

import Codec.Picture (writePng)
import Codec.Picture.Types (MutableImage, Pixel, Pixel8, PixelRGB8(..), createMutableImage, freezeImage, writePixel)
import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.HashMap.Lazy as M ((!))
import Data.Scientific (toRealFloat)
import qualified Data.Vector as V ((!), length)
import Data.Yaml (Value(..), decodeFileEither, prettyPrintParseException)
import Day11 (Chart(..), chart)
import Paths_aoc2017 (getDataFileName)
import Text.Printf (printf)

hexagonSpacing, hexagonRadius :: Double
hexagonSpacing = 1
hexagonRadius = 8

background :: (Pixel px, PrimMonad m) =>
    ((Int, Int), (Int, Int)) -> px -> m (MutableImage (PrimState m) px)
background ((minX, minY), (maxX, maxY)) = createMutableImage width height where
    xSpan = fromIntegral $ maxX - minX
    ySpan = fromIntegral $ maxY - minY
    width = ceiling $ xSpan * sqrt 3 * hexagonSpacing + 2 * hexagonRadius
    height = ceiling $ ySpan * hexagonSpacing + 2 * hexagonRadius

drawHexagon :: (Pixel px, PrimMonad m) => MutableImage (PrimState m) px ->
    ((Int, Int), (Int, Int)) -> (Int, Int) -> px -> m ()
drawHexagon image ((minX, minY), (maxX, maxY)) (x, y) color =
    let dx = fromIntegral $ x - minX
        dy = fromIntegral $ maxY - y
        cx = dx * sqrt 3 * hexagonSpacing + hexagonRadius
        cy = dy * hexagonSpacing + hexagonRadius
     in sequence_
          [ writePixel image i j color
          | i <- [ceiling (cx - hexagonRadius) .. floor (cx + hexagonRadius)]
          , let h = sqrt $ hexagonRadius ^ 2 - (cx - fromIntegral i) ^ 2
          , j <- [ceiling (cy - h) .. floor (cy + h)]
          ]

main :: IO ()
main = do
    let getViridis (Object colormaps) = colormaps M.! "viridis"
    Array colors <- getDataFileName "colormaps.yaml" >>=
        fmap (either (error . prettyPrintParseException) getViridis) .
        decodeFileEither
    Chart {..} <- getDataFileName "day11.txt" >>= fmap chart . readFile
    image <- background bounds $ PixelRGB8 maxBound maxBound maxBound
    let colorIx d = fromIntegral d / fromIntegral maxDistance *
                    fromIntegral (V.length colors)
        color d = PixelRGB8 (part 0) (part 1) (part 2) where
            idx = max 0 . min (V.length colors - 1) . floor $ colorIx d
            Array rgb = colors V.! idx
            part c = floor $ toRealFloat n * fromIntegral (maxBound :: Pixel8)
                where Number n = rgb V.! c
        writeImage i = freezeImage image >>=
            writePng (printf "day11-%04d.png" (i :: Int))
    forM_ (zip [0..] path) $ \(idx, (pos, d)) ->
        drawHexagon image bounds pos (color d) >> writeImage idx
