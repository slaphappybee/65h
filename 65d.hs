import Data.Word
import Data.List (head, find, intercalate)
import Data.Maybe
import Data.ByteString (readFile, unpack)
import Control.Monad
import Text.Printf
import System.Environment (getArgs)
import Spec
import INESFormat

type Operation = String

data AddressingMode = Immediate Word8 | Implied | Accumulator 
    | ZeroPage Word8 | ZeroPageX Word8 | ZeroPageY Word8
    | Absolute (Word8, Word8) | AbsoluteX (Word8, Word8) | AbsoluteY (Word8, Word8)
    | Indirect (Word8, Word8) | IndirectX Word8 | IndirectY Word8
    deriving Show

data Instruction = Instruction {
    operation :: Operation,
    addressing :: AddressingMode
} deriving Show

data DisassemblyToken = DisassemblyToken {
    tokenInstruction :: Maybe Instruction,
    tokenRaw :: [Word8],
    tokenOffset :: Int
} deriving Show

getDConst :: Show d => d -> String
getDConst = head . words . show

opcodeFor :: Instruction -> Maybe Word8
opcodeFor (Instruction op addr) = encode $ Opcode op (getDConst addr)

getAddressingSize :: String -> Int
getAddressingSize "Accumulator" = 0
getAddressingSize "Implied"     = 0
getAddressingSize "Immediate"   = 1
getAddressingSize "ZeroPage"    = 1
getAddressingSize "ZeroPageX"   = 1
getAddressingSize "ZeroPageY"   = 1
getAddressingSize "Absolute"    = 2
getAddressingSize "AbsoluteX"   = 2
getAddressingSize "AbsoluteY"   = 2
getAddressingSize "Indirect"    = 2
getAddressingSize "IndirectX"   = 1
getAddressingSize "IndirectY"   = 1

buildAddressingMode :: String -> [Word8] -> Maybe AddressingMode
buildAddressingMode "Accumulator" []     = Just Accumulator
buildAddressingMode "Implied"     []     = Just Implied
buildAddressingMode "Immediate"   [o]    = Just (Immediate o)
buildAddressingMode "ZeroPage"    [o]    = Just (ZeroPage  o)
buildAddressingMode "ZeroPageX"   [o]    = Just (ZeroPageX o)
buildAddressingMode "ZeroPageY"   [o]    = Just (ZeroPageY o)
buildAddressingMode "Absolute"    [b, a] = Just (Absolute  (a, b))
buildAddressingMode "AbsoluteX"   [b, a] = Just (AbsoluteX (a, b))
buildAddressingMode "AbsoluteY"   [b, a] = Just (AbsoluteY (a, b))
buildAddressingMode "Indirect"    [b, a] = Just (Indirect  (a, b))
buildAddressingMode "IndirectX"   [o]    = Just (IndirectX o)
buildAddressingMode "IndirectY"   [o]    = Just (IndirectY o)
buildAddressingMode _             _      = Nothing

param :: AddressingMode -> [Word8]
param (Immediate i) = [i]
param (ZeroPage o) = [o]
param (ZeroPageX o) = [o]
param (ZeroPageY o) = [o]
param (Absolute (a, b)) = [a, b]
param (AbsoluteX (a, b)) = [a, b]
param (AbsoluteY (a, b)) = [a, b]
param (Indirect (a, b)) = [a, b]
param (IndirectX o) = [o]
param (IndirectY o) = [o]
param _ = []

toMachine :: Instruction -> [Word8]
toMachine inst = (fromMaybe 0 $ opcodeFor inst) : (param $ addressing inst)

readInstruction :: [Word8] -> Int -> (DisassemblyToken, [Word8])
readInstruction buffer off = 
    let mOpcode = decode $ head buffer
        tBuffer = tail buffer
        paramSize = maybe 0 (\opcode -> getAddressingSize $ opcodeMode opcode) mOpcode
        getInstruction = \opcode -> Instruction (opcodeName opcode) <$> buildAddressingMode (opcodeMode opcode) (take paramSize tBuffer)
    in (DisassemblyToken (getInstruction =<< mOpcode) (take (paramSize + 1) buffer) off, (drop paramSize tBuffer))

readNInstructions :: Int -> Int -> [Word8] -> ([DisassemblyToken], [Word8])
readNInstructions 0 _   buffer = ([], buffer)
readNInstructions n off buffer = 
    let (t, tbuf) = readInstruction buffer off
        (l, ttbuf) = readNInstructions (n - 1) (off + (length $ tokenRaw t)) tbuf
    in (t : l, ttbuf)

readAllInstructions :: Int -> [Word8] -> [DisassemblyToken]
readAllInstructions _  []     = []
readAllInstructions off buffer =
    let (tok, tBuffer) = readInstruction buffer off
        toks = readAllInstructions (off + (length $ tokenRaw tok)) tBuffer
    in (tok : toks)

formatChunk :: Int -> [Word8] -> String
formatChunk cols bytes = printf "%-*s" cols $ intercalate " " (map (printf "%02x") bytes)

formatToken :: DisassemblyToken -> String
formatToken t = (printf "%04x  " $ tokenOffset t) ++ (formatChunk 10 $ tokenRaw t) ++ (formatAsm $ tokenInstruction t) ++ (formatHint t)

formatHint :: DisassemblyToken -> String
formatHint (DisassemblyToken Nothing _ _) = ""
formatHint t = let target = (tokenOffset t) + (fromIntegral $ head $ param $ addressing $ fromJust $ tokenInstruction t) + 2 - 0x100 in
    if elem (operation $ fromJust $ tokenInstruction t) ["BPL", "BMI", "BVC", "BVS", "BCC", "BCS", "BNE", "BEQ"] 
    then printf " (%04x)" target
    else ""

formatAsm :: Maybe Instruction -> String
formatAsm Nothing = ""
formatAsm (Just (Instruction op adm)) = op ++ " " ++ (formatAsmADM adm)

formatAsmADM :: AddressingMode -> String
formatAsmADM (Immediate i) = printf "#$%02x" i
formatAsmADM (ZeroPage o) = printf "$%02x" o
formatAsmADM (ZeroPageX o) = printf "$%02x, X" o
formatAsmADM (Absolute (a, b)) = printf "$%02x%02x" a b
formatAsmADM (AbsoluteX (a, b)) = printf "$%02x%02x, X" a b
formatAsmADM (AbsoluteY (a, b)) = printf "$%02x%02x, Y" a b
formatAsmADM (IndirectX o) = printf "($%02x, X)" o
formatAsmADM (IndirectY o) = printf "($%02x), Y" o
formatAsmADM _ = ""

disassembleBuffer :: [Word8] -> [String]
disassembleBuffer buf = let is = readAllInstructions 0xe000 buf in
    map formatToken is

disassembleFile :: String -> IO [String]
disassembleFile filename = do
    fcBS <- Data.ByteString.readFile filename
    let buffer = Data.ByteString.unpack fcBS
    let header = readHeader buffer
    let prgRomSize = 1024 * inesPRGRomSizeKb header
    return $ (formatHeader header) ++ (disassembleBuffer $ ((drop 16) . (take prgRomSize)) buffer)

formatHeader :: INESHeader -> [String]
formatHeader (INESHeader sig prgRomSize chrRomSize prgRamSize _) =
    if sig /= "NES\x1A"
        then ["Invalid Header"]
        else [printf "INES v1 PRG ROM = %u Kb, CHR ROM = %u Kb, PRG RAM = %u Kb" prgRomSize chrRomSize prgRamSize]

main :: IO ()
main = do
    args <- getArgs
    unlines <$> disassembleFile (head args) >>= putStr
