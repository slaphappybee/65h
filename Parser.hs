module Parser (
    SubBlock (SubBlock),
    VariableDeclaration (VariableDeclaration),
    VariableType (W8, W16),
    VariableAccess (Extern, Const),
    AddressingParameter (AddressingAbsolute, AddressingRelative),
    TopLevelElement (ElementVariable, ElementSubBlock), partitionTopLevel,
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
    MacroStatement String [MacroParameter] [Statement]
    deriving Show

data VariableAccess =
    Extern |
    Const
    deriving Show

data VariableType =
    W8 |
    W16
    deriving Show

data VariableDeclaration = VariableDeclaration VariableAccess VariableType String Word16 deriving Show

data MacroParameter =
    ParameterInstruction Instruction |
    ParameterValue AddressingParameter
    deriving Show

data AddressingParameter =
    AddressingAbsolute Value |
    AddressingRelative Value Char
    deriving Show

data Value = 
    ValueLiteral (Either Word8 Word16) |
    ValueIdentifier String |
    ValueModifier Char Value
    deriving Show

data Instruction = Instruction String (Maybe AddressingParameter)
    deriving Show

type Parser a = IndentParser String () a

partitionTopLevel :: [TopLevelElement] -> ([SubBlock], [VariableDeclaration])
partitionTopLevel = foldr dispatch ([], [])
    where 
        dispatch (ElementSubBlock b) (bs, vs) = (b:bs, vs)
        dispatch (ElementVariable v) (bs, vs) = (bs, v:vs)

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
instructionP = Instruction <$> (count 3 letter) <* spacesP <*> (optionMaybe addressingParameterP) <* spacesP

macroBlockP :: Parser Statement
macroBlockP = withBlock (\(name, params) -> \block -> MacroStatement name params block)
    ((,) <$> ((char '+') *> macroNameP <* spacesP) <*> (many (macroParameterP <* spacesP)) <* spaces)
    (statementP <* spaces)

statementP :: Parser Statement
statementP = 
    macroBlockP <|>
    InstructionStatement <$> instructionP

literalValueP :: Parser (Either Word8 Word16)
literalValueP = hexValue <$> ((char '$') *> (many1 hexDigit))

addressingParameterP :: Parser AddressingParameter
addressingParameterP = boxParam <$> valueP <*> optionMaybe (char ',' *> (oneOf "xyXY"))
    where boxParam value Nothing = AddressingAbsolute value
          boxParam value (Just r) = AddressingRelative value $ toUpper r

valueP :: Parser Value
valueP =
    ValueModifier <$> (char '#') <*> valueP <|>
    ValueLiteral <$> literalValueP <|>
    ValueIdentifier <$> (try $ identifierP)

macroParameterP :: Parser MacroParameter
macroParameterP = 
    ParameterValue <$> addressingParameterP <|> 
    ParameterInstruction <$> instructionP

subHeaderP = (string "sub ") *> spacesP *> identifierP <* spacesP
subHeaderAddressP = (char '=') *> spacesP *> (fromRight <$> literalValueP) <* spacesP

subBlockP :: Parser SubBlock
subBlockP = withBlock (\(name, addr) -> \block -> SubBlock name addr block)
    ((,) <$> subHeaderP <*> subHeaderAddressP <* spaces)
    (statementP <* spaces)

variableAccessP :: Parser VariableAccess
variableAccessP = ((string "extern") >> (return Extern)) <|> ((string "const") >> (return Const))

variableTypeP :: Parser VariableType
variableTypeP = ((try $ string "w8") >> (return W8)) <|> ((string "w16") >> (return W16))

variableDeclarationP :: Parser VariableDeclaration
variableDeclarationP = VariableDeclaration <$> (variableAccessP <* spacesP) <*> (variableTypeP <* spacesP) <*> (identifierP <* spacesP) <*> (char '=' *> spacesP *> (fromRight <$> literalValueP) <* nextLineP)

fileP :: Parser [TopLevelElement]
fileP = (many (ElementSubBlock <$> subBlockP <|> ElementVariable <$> variableDeclarationP)) <* eof

parseFile :: String -> IO (Either ParseError [TopLevelElement])
parseFile = parseFromFile fileP
