module Parser (
    SubBlock (SubBlock),
    Statement (InstructionStatement, MacroStatement),
    MacroParameter (ParameterInstruction, ParameterValue),
    Value (ValueLiteral, ValueIdentifier, ValueModifier),
    Instruction (Instruction),
    parseFile) where

import Data.Maybe
import Data.Char
import Data.Word
import Control.Monad
import Control.Applicative ((<$>))
import Text.Parsec
import Text.Parsec.Indent

data TopLevelElement =
    ElementSubBlock SubBlock |
    ElementVariable VariableDeclaration
    deriving Show

data SubBlock = SubBlock String Word16 [Statement]
    deriving Show

data Statement =
    InstructionStatement Instruction |
    MacroStatement String [MacroParameter]
    deriving Show

data VariableAccess = Extern deriving Show
data VariableType = W8 deriving Show
data VariableDeclaration = VariableDeclaration VariableAccess VariableType String Word16 deriving Show

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

type Parser a = IndentParser String () a

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p f = (runIndentParser p () f) <$> (readFile f)

manyN :: Int -> Parser a -> Parser [a]
manyN n p = (++) <$> (count n p) <*> (many p)

spacesP :: Parser String
spacesP = many (char ' ')

blankLineP :: Parser String
blankLineP = spacesP <* newline

nextLineP :: Parser ()
nextLineP = newline *> (many $ try blankLineP) >> (pure ())

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

macroNameP :: Parser String
macroNameP = (:) <$> letter <*> (many (alphaNum <|> (char '_')))

identifierP :: Parser String
identifierP = (:) <$> letter <*> (manyN 3 (alphaNum <|> (char '_')))

instructionP :: Parser Instruction
instructionP = Instruction <$> (count 3 letter)

statementP :: Parser Statement
statementP =
    MacroStatement <$> ((char '+') *> macroNameP <* spacesP) <*> (many macroParameterP) <|>
    InstructionStatement <$> instructionP

literalValueP :: Parser (Either Word8 Word16)
literalValueP = hexValue <$> ((char '$') *> (many1 hexDigit))

valueP :: Parser Value
valueP =
    ValueModifier <$> (char '#') <*> valueP <|>
    ValueLiteral <$> literalValueP <|>
    ValueIdentifier <$> (try $ identifierP)

macroParameterP :: Parser MacroParameter
macroParameterP = 
    ParameterValue <$> valueP <|> 
    ParameterInstruction <$> instructionP

subBlockP :: Parser SubBlock
subBlockP = SubBlock
    <$> ((string "sub ") *> spacesP *> identifierP <* spacesP)
    <*> ((char '=') *> spacesP *> (fromRight <$> literalValueP) <* spacesP <* nextLineP)
    <*> (many $ (try (count 4 space) *> statementP <* nextLineP))

variableAccessP :: Parser VariableAccess
variableAccessP = (string "extern") >> (return Extern)
variableTypeP :: Parser VariableType
variableTypeP = (string "w8") >> (return W8)

variableDeclarationP :: Parser VariableDeclaration
variableDeclarationP = VariableDeclaration <$> (variableAccessP <* spacesP) <*> (variableTypeP <* spacesP) <*> (identifierP <* spacesP) <*> (char '=' *> spacesP *> (fromRight <$> literalValueP) <* nextLineP)

fileP :: Parser [TopLevelElement]
fileP = (many (ElementSubBlock <$> subBlockP <|> ElementVariable <$> variableDeclarationP)) <* eof

parseFile :: String -> IO (Either ParseError [TopLevelElement])
parseFile = parseFromFile fileP
