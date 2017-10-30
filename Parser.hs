import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Char
import Data.Word
import Control.Monad
import Control.Applicative ((<$>))

data FileContents = FileContents [TopLevelDeclaration]
    deriving Show

data TopLevelDeclaration = SubDeclaration SubBlock
    deriving Show

data SubBlock = SubBlock String Word16 [Statement]
    deriving Show

data Statement =
    InstructionStatement Instruction |
    MacroStatement String [MacroParameter]
    deriving Show

data MacroParameter =
    ParameterInstruction Instruction |
    ParameterValue Value
    deriving Show

data Value = 
    ValueLiteral (Either Word8 Word16) |
    ValueIdentifier String |
    ValueModifier Char Value
    deriving Show

data Instruction = Instruction String
    deriving Show

manyN :: Int -> GenParser Char st a -> GenParser Char st [a]
manyN n p = (++) <$> (count n p) <*> (many p)

spacesP :: GenParser Char st String
spacesP = many (char ' ')

fromRight :: Either a b -> b
fromRight (Right b) = b

parseHex :: String -> Int
parseHex = parseHexR . reverse
    where
        parseHexR cs = case cs of
            [] -> 0
            s  -> (digitToInt $ head s) + 16 * (parseHexR $ tail s)

hexValue :: String -> Either Word8 Word16
hexValue s = let intValue = parseHex s in 
    if intValue > 0xff 
        then Right (fromIntegral intValue)
        else Left (fromIntegral intValue)

macroNameP :: GenParser Char st String
macroNameP = (:) <$> letter <*> (many (alphaNum <|> (char '_')))

identifierP :: GenParser Char st String
identifierP = (:) <$> letter <*> (manyN 3 (alphaNum <|> (char '_')))

instructionP :: GenParser Char st Instruction
instructionP = Instruction <$> (count 3 letter)

statementP :: GenParser Char st Statement
statementP =
    MacroStatement <$> ((char '+') *> macroNameP <* spacesP) <*> (many macroParameterP) <|>
    InstructionStatement <$> instructionP

literalValueP :: GenParser Char st (Either Word8 Word16)
literalValueP = hexValue <$> ((char '$') *> (many1 hexDigit))

valueP :: GenParser Char st Value
valueP =
    ValueModifier <$> (char '#') <*> valueP <|>
    ValueLiteral <$> literalValueP <|>
    ValueIdentifier <$> (try $ identifierP)

macroParameterP :: GenParser Char st MacroParameter
macroParameterP = 
    ParameterValue <$> valueP <|> 
    ParameterInstruction <$> instructionP

blankLineP :: GenParser Char st String
blankLineP = spacesP <* newline

subBlockP :: GenParser Char st SubBlock
subBlockP = SubBlock
    <$> ((string "sub ") *> spacesP *> identifierP <* spacesP)
    <*> ((char '=') *> spacesP *> (fromRight <$> literalValueP) <* spacesP <* newline)
    <*> ((many $ (try ((count 4 space) *> statementP <* newline) <* (many $ try blankLineP))))

fileP :: GenParser Char st FileContents
fileP = FileContents <$> many (SubDeclaration <$> subBlockP)

parseFile :: String -> IO (Either ParseError FileContents)
parseFile = parseFromFile fileP
