import Data.Tuple
import Data.Word
import Data.List
import Data.ByteString (pack, writeFile)
import System.Environment (getArgs)

import Assembler
import Parser
import Spec
import INESFormat

toi :: Integral a => a -> Int
toi = fromIntegral

writeINESFile :: String -> [(Word16, [Word8])] -> IO ()
writeINESFile filename prg = let
    headerRaw = generateHeader $ mkHeader 16 8 0
    prgRaw = buildPRG prg
    chrRaw = replicate (8 * 1024) 0
    bs = pack (headerRaw ++ prgRaw ++ chrRaw)
    in Data.ByteString.writeFile filename bs

buildImage :: Int -> Word16 -> [(Word16, [Word8])] -> [Word8]
buildImage size _    []     = replicate size 0
buildImage size off blocks = let
    nextBlock = head blocks
    blockOff = fst nextBlock
    nextOff = blockOff + fromIntegral (length $ snd nextBlock)
    padding = replicate (fromIntegral (blockOff - off)) 0
    in padding ++ (snd nextBlock) ++ (buildImage (size - fromIntegral (nextOff - off)) nextOff (tail blocks))

buildPRG :: [(Word16, [Word8])] -> [Word8]
buildPRG blocks = let
    compareFirsts = \a -> \b -> compare (fst a) (fst b)
    sortedBlocks = sortBy compareFirsts blocks in
    buildImage (16 * 1024) 0xe000 blocks

assembleFile :: String -> IO ()
assembleFile filename = do
    parsedFile <- parseFile filename
    case parsedFile of
        Left pa -> putStrLn "Parse error"
        Right blocks -> writeINESFile (filename ++ ".nes") (assemble blocks)

main :: IO ()
main = do
    args <- getArgs
    assembleFile $ head args
