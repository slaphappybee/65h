import Codec.Picture
import System.Environment (getArgs)
import System.FilePath
import Encoder

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    let paletteFile = replaceFileName inputFile "palette.bmp"
    let outputFile = addExtension inputFile ".chr"

    if (length args) < 1 then
        putStrLn "Usage: 65e <file>"
    else do
        img <- fromRightImageRGB8 <$> readImage inputFile
        pal <- fromRightImageRGB8 <$> readImage paletteFile
        writeCHR outputFile $ encodeAll (head $ palettes pal) img
