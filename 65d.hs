import Data.Word
import Data.List (head, find, intercalate)
import Data.Maybe
import Data.ByteString (readFile, unpack)
import Control.Monad
import Text.Printf
import System.Environment (getArgs)

type Operation = String

data AddressingMode = Immediate Word8 | Implied | Accumulator 
    | ZeroPage Word8 | ZeroPageX Word8 
    | Absolute (Word8, Word8) | AbsoluteX (Word8, Word8) | AbsoluteY (Word8, Word8) 
    | IndirectX Word8 | IndirectY Word8 
    deriving Show

data Instruction = Instruction {
    operation :: Operation,
    addressing :: AddressingMode
} deriving Show

data OperationSpec = OperationSpec {
    operationName :: Operation,
    modeName :: String,
    opcode :: Word8    
} deriving Show

data DisassemblyToken = DisassemblyToken {
    tokenInstruction :: Instruction,
    tokenRaw :: [Word8],
    tokenOffset :: Int
} deriving Show

spec = [
    OperationSpec "ADC" "Immediate"   0x69, -- 011 010 01
    OperationSpec "ADC" "ZeroPage"    0x65, -- 011 001 01
    OperationSpec "ADC" "ZeroPageX"   0x75, -- 011 101 01
    OperationSpec "ADC" "Absolute"    0x6D, -- 011 011 01
    OperationSpec "ADC" "AbsoluteX"   0x7D, -- 011 111 01
    OperationSpec "ADC" "AbsoluteY"   0x79, -- 011 110 01
    OperationSpec "ADC" "IndirectX"   0x61, -- 011 000 01
    OperationSpec "ADC" "IndirectY"   0x71, -- 011 100 01

    OperationSpec "AND" "Immediate"   0x29, -- 001 010 01
    OperationSpec "AND" "ZeroPage"    0x25, -- 001 001 01
    OperationSpec "AND" "ZeroPageX"   0x35, -- 001 101 01
    OperationSpec "AND" "Absolute"    0x2D, -- 001 011 01
    OperationSpec "AND" "AbsoluteX"   0x3D, -- 001 111 01
    OperationSpec "AND" "AbsoluteY"   0x39, -- 001 110 01
    OperationSpec "AND" "IndirectX"   0x21, -- 001 000 01
    OperationSpec "AND" "IndirectY"   0x31, -- 001 100 01

    OperationSpec "ASL" "Accumulator" 0x0A, -- 000 010 10
    OperationSpec "ASL" "ZeroPage"    0x06, -- 000 001 10
    OperationSpec "ASL" "ZeroPageX"   0x16, -- 000 101 10
    OperationSpec "ASL" "Absolute"    0x0E, -- 000 011 10
    OperationSpec "ASL" "AbsoluteX"   0x1E, -- 000 111 10

    OperationSpec "BIT" "ZeroPage"    0x24, -- 001 001 00
    OperationSpec "BIT" "Absolute"    0x2C, -- 001 011 00

    OperationSpec "BPL" "Immediate"   0x10, -- 000 100 00
    OperationSpec "BMI" "Immediate"   0x30, -- 001 100 00
    OperationSpec "BVC" "Immediate"   0x50, -- 010 100 00
    OperationSpec "BVS" "Immediate"   0x70, -- 011 100 00
    OperationSpec "BCC" "Immediate"   0x90, -- 100 100 00
    OperationSpec "BCS" "Immediate"   0xB0, -- 101 100 00
    OperationSpec "BNE" "Immediate"   0xD0, -- 110 100 00
    OperationSpec "BEQ" "Immediate"   0xF0, -- 111 100 00

    OperationSpec "BRK" "Implied"     0x00, -- 0000 0000

    OperationSpec "CMP" "Immediate"   0xC9, -- 110 010 01
    OperationSpec "CMP" "ZeroPage"    0xC5, -- 110 001 01
    OperationSpec "CMP" "ZeroPageX"   0xD5, -- 110 101 01
    OperationSpec "CMP" "Absolute"    0xCD, -- 110 011 01
    OperationSpec "CMP" "AbsoluteX"   0xDD, -- 110 111 01
    OperationSpec "CMP" "AbsoluteY"   0xD9, -- 110 110 01
    OperationSpec "CMP" "IndirectX"   0xC1, -- 110 000 01
    OperationSpec "CMP" "IndirectY"   0xD1, -- 110 100 01

    OperationSpec "CPX" "Immediate"   0xE0, -- 111 000 00
    OperationSpec "CPX" "ZeroPage"    0xE4, -- 111 001 00
    OperationSpec "CPX" "Absolute"    0xEC, -- 111 011 00

    OperationSpec "CPY" "Immediate"   0xC0, -- 110 000 00
    OperationSpec "CPY" "ZeroPage"    0xC4, -- 110 001 00
    OperationSpec "CPY" "Absolute"    0xCC, -- 110 011 00

    OperationSpec "DEC" "ZeroPage"    0xC6, -- 110 001 10
    OperationSpec "DEC" "ZeroPageX"   0xD6, -- 110 101 10
    OperationSpec "DEC" "Absolute"    0xCE, -- 110 011 10
    OperationSpec "DEC" "AbsoluteX"   0xDE, -- 110 111 10

    OperationSpec "EOR" "Immediate"   0x49, -- 010 010 01
    OperationSpec "EOR" "ZeroPage"    0x45, -- 010 001 01
    OperationSpec "EOR" "ZeroPageX"   0x55, -- 010 101 01
    OperationSpec "EOR" "Absolute"    0x4D, -- 010 011 01
    OperationSpec "EOR" "AbsoluteX"   0x5D, -- 010 111 01
    OperationSpec "EOR" "AbsoluteY"   0x59, -- 010 110 01
    OperationSpec "EOR" "IndirectX"   0x41, -- 010 000 01
    OperationSpec "EOR" "IndirectY"   0x51, -- 010 100 01

    OperationSpec "CLC" "Implied"     0x18, -- 000 110 00
    OperationSpec "SEC" "Implied"     0x38, -- 001 110 00
    OperationSpec "CLI" "Implied"     0x58, -- 010 110 00
    OperationSpec "SEI" "Implied"     0x78, -- 011 110 00
    OperationSpec "CLV" "Implied"     0xB8, -- 101 110 00
    OperationSpec "CLD" "Implied"     0xD8, -- 110 110 00
    OperationSpec "SED" "Implied"     0xF8, -- 111 110 00

    OperationSpec "INC" "ZeroPage"    0xE6,
    OperationSpec "INC" "ZeroPageX"   0xF6,
    OperationSpec "INC" "Absolute"    0xEE,
    OperationSpec "INC" "AbsoluteX"   0xFE,

    OperationSpec "JMP" "Absolute"    0x4C,
    OperationSpec "JMP" "Indirect"    0x6C,

    OperationSpec "JSR" "Absolute"    0x20,

    OperationSpec "LDA" "Immediate"   0xA9,
    OperationSpec "LDA" "ZeroPage"    0xA5,
    OperationSpec "LDA" "ZeroPageX"   0xB5,
    OperationSpec "LDA" "Absolute"    0xAD,
    OperationSpec "LDA" "AbsoluteX"   0xBD,
    OperationSpec "LDA" "AbsoluteY"   0xB9,
    OperationSpec "LDA" "IndirectX"   0xA1,
    OperationSpec "LDA" "IndirectY"   0xB1,

    OperationSpec "LDX" "Immediate"   0xA2,
    OperationSpec "LDX" "ZeroPage"    0xA6,
    OperationSpec "LDX" "ZeroPageY"   0xB6,
    OperationSpec "LDX" "Absolute"    0xAE,
    OperationSpec "LDX" "AbsoluteY"   0xBE,

    OperationSpec "LDY" "Immediate"   0xA0,
    OperationSpec "LDY" "ZeroPage"    0xA4,
    OperationSpec "LDY" "ZeroPageX"   0xB4,
    OperationSpec "LDY" "Absolute"    0xAC,
    OperationSpec "LDY" "AbsoluteX"   0xBC,

    OperationSpec "LSR" "Accumulator" 0x4A,
    OperationSpec "LSR" "ZeroPage"    0x46,
    OperationSpec "LSR" "ZeroPageX"   0x56,
    OperationSpec "LSR" "Absolute"    0x4E,
    OperationSpec "LSR" "AbsoluteX"   0x5E,

    OperationSpec "NOP" "Implied"     0xEA,

    OperationSpec "ORA" "Immediate"   0x09,
    OperationSpec "ORA" "ZeroPage"    0x05,
    OperationSpec "ORA" "ZeroPageX"   0x15,
    OperationSpec "ORA" "Absolute"    0x0D,
    OperationSpec "ORA" "AbsoluteX"   0x1D,
    OperationSpec "ORA" "AbsoluteY"   0x19,
    OperationSpec "ORA" "IndirectX"   0x01,
    OperationSpec "ORA" "IndirectY"   0x11,

    OperationSpec "TAX" "Implied"     0xAA, -- 1010 10 10
    OperationSpec "TXA" "Implied"     0x8A, -- 1000 10 10
    OperationSpec "TAY" "Implied"     0xA8, -- 1010 10 00
    OperationSpec "TYA" "Implied"     0x98, -- 1001 10 00
    OperationSpec "TSX" "Implied"     0xBA, -- 1011 10 10
    OperationSpec "TXS" "Implied"     0x9A, -- 1001 10 10
    
    OperationSpec "DEX" "Implied"     0xCA, -- 110 010 10
    OperationSpec "INX" "Implied"     0xE8, -- 111 010 00
    OperationSpec "DEY" "Implied"     0x88, -- 100 010 00
    OperationSpec "INY" "Implied"     0xC8, -- 110 010 00
    OperationSpec "PHA" "Implied"     0x48, -- 010 010 00
    OperationSpec "PLA" "Implied"     0x68, -- 011 010 00
    OperationSpec "PHP" "Implied"     0x08, -- 000 010 00
    OperationSpec "PLP" "Implied"     0x28, -- 001 010 00

    OperationSpec "RTI" "Implied"     0x40, -- 010 000 00
    OperationSpec "RTS" "Implied"     0x60, -- 011 000 00

    OperationSpec "ROL" "Accumulator" 0x2A,
    OperationSpec "ROL" "ZeroPage"    0x26,
    OperationSpec "ROL" "ZeroPageX"   0x36,
    OperationSpec "ROL" "Absolute"    0x2E,
    OperationSpec "ROL" "AbsoluteX"   0x3E,

    OperationSpec "ROR" "Accumulator" 0x6A,
    OperationSpec "ROR" "ZeroPage"    0x66,
    OperationSpec "ROR" "ZeroPageX"   0x76,
    OperationSpec "ROR" "Absolute"    0x6E,
    OperationSpec "ROR" "AbsoluteX"   0x7E,

    OperationSpec "SBC" "Immediate"   0xE9,
    OperationSpec "SBC" "ZeroPage"    0xE5,
    OperationSpec "SBC" "ZeroPageX"   0xF5,
    OperationSpec "SBC" "Absolute"    0xED,
    OperationSpec "SBC" "AbsoluteX"   0xFD,
    OperationSpec "SBC" "AbsoluteY"   0xF9,
    OperationSpec "SBC" "IndirectX"   0xE1,
    OperationSpec "SBC" "IndirectY"   0xF1,

    OperationSpec "STA" "ZeroPage"    0x85,
    OperationSpec "STA" "ZeroPageX"   0x95,
    OperationSpec "STA" "Absolute"    0x8D,
    OperationSpec "STA" "AbsoluteX"   0x9D,
    OperationSpec "STA" "AbsoluteY"   0x99,
    OperationSpec "STA" "IndirectX"   0x81,
    OperationSpec "STA" "IndirectY"   0x91,

    OperationSpec "STX" "ZeroPage"    0x86,
    OperationSpec "STX" "ZeroPageY"   0x96,
    OperationSpec "STX" "Absolute"    0x8E,

    OperationSpec "STY" "ZeroPage"    0x84,
    OperationSpec "STY" "ZeroPageX"   0x94,
    OperationSpec "STY" "Absolute"    0x8C
    ]

getDConst :: Show d => d -> String
getDConst = head . words . show

matchSpec :: (Operation, String) -> OperationSpec -> Bool
matchSpec (op, addr) (OperationSpec sop saddr _) = (op == sop && addr == saddr)

opcodeFor :: Instruction -> Maybe Word8
opcodeFor (Instruction op addr) = liftM opcode (find (matchSpec (op, getDConst addr)) spec)

specFor :: Word8 -> OperationSpec
specFor i = fromJust $ find (\s -> opcode s == i) spec

getAddressingSize :: String -> Int
getAddressingSize "Accumulator" = 0
getAddressingSize "Implied"     = 0
getAddressingSize "Immediate"   = 1
getAddressingSize "ZeroPage"    = 1
getAddressingSize "ZeroPageX"   = 1
getAddressingSize "Absolute"    = 2
getAddressingSize "AbsoluteX"   = 2
getAddressingSize "AbsoluteY"   = 2
getAddressingSize "IndirectX"   = 1
getAddressingSize "IndirectY"   = 1

buildAddressingMode :: String -> [Word8] -> AddressingMode
buildAddressingMode "Accumulator" []     = Accumulator
buildAddressingMode "Implied"     []     = Implied
buildAddressingMode "Immediate"   [o]    = Immediate o
buildAddressingMode "ZeroPage"    [o]    = ZeroPage o
buildAddressingMode "ZeroPageX"   [o]    = ZeroPageX o
buildAddressingMode "Absolute"    [b, a] = Absolute (a, b)
buildAddressingMode "AbsoluteX"   [b, a] = AbsoluteX (a, b)
buildAddressingMode "AbsoluteY"   [b, a] = AbsoluteY (a, b)
buildAddressingMode "IndirectX"   [o]    = IndirectX o
buildAddressingMode "IndirectY"   [o]    = IndirectY o

param :: AddressingMode -> [Word8]
param (Immediate i) = [i]
param (ZeroPage o) = [o]
param (ZeroPageX o) = [o]
param (Absolute (a, b)) = [a, b]
param (AbsoluteX (a, b)) = [a, b]
param (AbsoluteY (a, b)) = [a, b]
param (IndirectX o) = [o]
param (IndirectY o) = [o]
param _ = []

toMachine :: Instruction -> [Word8]
toMachine inst = (fromMaybe 0 $ opcodeFor inst) : (param $ addressing inst)

readInstruction :: [Word8] -> Int -> (DisassemblyToken, [Word8])
readInstruction buffer off = 
    let opspec = specFor $ head buffer
        psize = getAddressingSize $ modeName opspec
        tbuffer = tail buffer
        instruction = Instruction (operationName opspec) (buildAddressingMode (modeName opspec) (take psize tbuffer))
    in (DisassemblyToken instruction (take (psize + 1) buffer) off, (drop psize tbuffer))

readNInstructions :: Int -> Int -> [Word8] -> ([DisassemblyToken], [Word8])
readNInstructions 0 _   buffer = ([], buffer)
readNInstructions n off buffer = 
    let (t, tbuf) = readInstruction buffer off
        (l, ttbuf) = readNInstructions (n - 1) (off + (length $ tokenRaw t)) tbuf
    in (t : l, ttbuf)

formatChunk :: Int -> [Word8] -> String
formatChunk cols bytes = printf "%-*s" cols $ intercalate " " (map (printf "%02x") bytes)

formatToken :: DisassemblyToken -> String
formatToken t = (printf "%04x  " $ tokenOffset t) ++ (formatChunk 10 $ tokenRaw t) ++ (formatAsm $ tokenInstruction t) ++ (formatHint t)

formatHint :: DisassemblyToken -> String
formatHint t = let target = (tokenOffset t) + (fromIntegral $ head $ param $ addressing $ tokenInstruction t) + 2 - 0x100 in
    if elem (operation $ tokenInstruction t) ["BPL", "BMI", "BVC", "BVS", "BCC", "BCS", "BNE", "BEQ"] 
    then printf " (%04x)" target
    else ""

formatAsm :: Instruction -> String
formatAsm (Instruction op adm) = op ++ " " ++ (formatAsmADM adm)

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
disassembleBuffer buf = let (is, _) = readNInstructions 128 0xe000 buf in
    map formatToken is

disassembleFile :: String -> IO [String]
disassembleFile filename = do
    fcBS <- Data.ByteString.readFile filename
    return $ disassembleBuffer (drop 16 $ Data.ByteString.unpack fcBS)

main :: IO ()
main = do
    args <- getArgs
    unlines <$> disassembleFile (head args) >>= putStr
