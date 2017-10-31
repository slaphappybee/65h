module INESFormat (
    INESHeader (INESHeader), inesSignature, inesPRGRomSizeKb, inesCHRRomSizeKb, inesPRGRamSizeKb, inesFlags,
    readHeader,
    mkHeader,
    generateHeader
    ) where

import Data.Char (chr, ord)
import Data.Word

data INESHeader = INESHeader {
    inesSignature :: String,
    inesPRGRomSizeKb :: Int,
    inesCHRRomSizeKb :: Int,
    inesPRGRamSizeKb :: Int,
    inesFlags :: (Int, Int, Int, Int)
} deriving Show

mkHeader :: Int -> Int -> Int -> INESHeader
mkHeader po co pa = INESHeader "NES\x1A" po co pa (0, 0, 0, 0)

readHeader :: [Word8] -> INESHeader
readHeader buffer = let (i:n:e:s:po:co:f6:f7:pa:f8:f9:_) = fmap fromIntegral buffer
    in INESHeader (fmap chr [i, n, e, s]) (po * 16) (co * 8) (pa * 8) (f6, f7, f8, f9)

generateHeader :: INESHeader -> [Word8]
generateHeader header = let
    po = (inesPRGRomSizeKb header) `quot` 16
    co = (inesCHRRomSizeKb header) `quot` 8
    pa = (inesPRGRamSizeKb header) `quot` 8
    (f6, f7, f8, f9) = inesFlags header
    [i, n, e, s] = fmap ord $ inesSignature header
    in fmap fromIntegral [i,n,e,s,po,co,f6,f7,pa,f8,f9,0,0,0,0,0]
