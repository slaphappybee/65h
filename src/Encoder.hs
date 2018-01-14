module Encoder (
    fromRightImageRGB8,
    writeCHR,
    encodeAll,
    palettes
) where

import Codec.Picture
import Data.Either
import Data.List
import Data.Maybe
import Data.Bits
import Text.Printf
import System.Environment (getArgs)
import Data.ByteString (pack, writeFile)

type Palette4 b = [b]

fromRightImageRGB8 :: Either String DynamicImage -> Image PixelRGB8
fromRightImageRGB8 (Right (ImageRGB8 image)) = image

pixelAtT :: Pixel b => Image b -> (Int, Int) -> b
pixelAtT img (x, y) = pixelAt img x y

blockCoordinates :: (Int, Int) -> Int -> [(Int, Int)]
blockCoordinates (x, y) size = (\a -> \b -> (b,a)) <$> [y..(y+size-1)] <*> [x..(x+size-1)]

blockPixels :: Pixel b => Int -> (Int, Int) -> Image b -> [b]
blockPixels size (bx, by) img = (pixelAtT img) <$> blockCoordinates (bx*size, by*size) size

blockIndexes :: Pixel b => Image b -> [(Int, Int)]
blockIndexes img = (\a -> \b -> (b,a)) <$> [0..((imageWidth img) `quot` 8)-1] <*> [0..((imageHeight img) `quot` 8)-1]

palettes :: Pixel b => Image b -> [Palette4 b]
palettes img = (palettePixels img) <$> [(0, 0), (4, 0), (8, 0), (12, 0), (0, 1), (4, 1), (8, 1), (12, 1)]
    where palettePixels img (x, y) = (pixelAtT img) <$> [(x, y), (x+1, y), (x+2, y), (x+3, y)]

indexify :: Pixel b => Palette4 b -> [b] -> [Int]
indexify pal pix = fromJust . flip findIndex pal . (==) <$> pix

groupN :: Int -> [b] -> [[b]]
groupN _ [] = []
groupN size l = (take size l):(groupN size (drop size l))

fromBin :: [Int] -> Int
fromBin l = sum $ zipWith pwr (reverse l) [0..]
    where pwr b p = b * (2 ^p)

encodeBlock :: Pixel b => Palette4 b -> [b] -> [Int]
encodeBlock pal blk = fromBin <$> groupN 8 (lowBits ++ highBits)
    where   bits = indexify pal blk
            lowBits = (.&. 1) <$> bits
            highBits = (flip shift (-1)) <$> bits

encodeAll :: Pixel b => Palette4 b -> Image b -> [[Int]]
encodeAll pal img = (\c -> encodeBlock pal (blockPixels 8 c img)) <$> (blockIndexes img)

formatBlock :: [Int] -> String
formatBlock l = "    +raw8 " ++ (intercalate " " $ map (printf "#$%02x") l)

writeCHR :: String -> [[Int]] -> IO ()
writeCHR filename bytes = Data.ByteString.writeFile filename $ pack $ fromIntegral <$> concat bytes
