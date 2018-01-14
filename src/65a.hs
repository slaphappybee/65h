import Data.Tuple
import Data.Word
import Data.List
import Data.ByteString (ByteString, pack, writeFile, readFile, append)
import System.Environment (getArgs)
import System.FilePath

import Assembler
import Parser
import Spec

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
    buildImage (16 * 1024) 0xc000 blocks

assembleFile :: String -> String -> IO ()
assembleFile output filename = do
    parsedFile <- parseFile filename
    case parsedFile of
        Left _ -> putStrLn "Parse error"
        Right blocks -> Data.ByteString.writeFile output (pack $ buildPRG $ assemble blocks)

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    let outputFile = addExtension inputFile ".prg"

    if (length args) < 1 then
        putStrLn "Usage: 65a <file>"
    else
        assembleFile outputFile inputFile
