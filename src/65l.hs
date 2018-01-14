import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.FilePath
import INESFormat

rawSizeKb :: Int -> BS.ByteString -> Int
rawSizeKb inc bs = inc * (BS.length bs + (1024 * inc) - 1) `quot` (1024 * inc)

writeINESFile :: String -> BS.ByteString -> BS.ByteString -> IO ()
writeINESFile filename prg chr = let
    header = BS.pack $ generateHeader $ mkHeader (rawSizeKb 16 prg) (rawSizeKb 8 chr) 0
    in BS.writeFile filename $ BS.concat [header, prg, chr]

main :: IO ()
main = do
    args <- getArgs
    let prgFile = head args
    let chrFile = head $ tail args
    let outputFile = addExtension prgFile ".nes"

    prg <- BS.readFile prgFile
    case (length args) of
        1 -> writeINESFile outputFile prg BS.empty
        2 -> do
            chr <- BS.readFile chrFile
            writeINESFile outputFile prg chr
        _ -> putStrLn "Usage: 65l <prg-file> [<chr-file>]"
