import Data.Word
import Data.List (head, find)
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

spec = [
    OperationSpec "ADC" "Immediate"   0x69,
    OperationSpec "ADC" "ZeroPage"    0x65,
    OperationSpec "ADC" "ZeroPageX"   0x75,
    OperationSpec "ADC" "Absolute"    0x6D,
    OperationSpec "ADC" "AbsoluteX"   0x7D,
    OperationSpec "ADC" "AbsoluteY"   0x79,
    OperationSpec "ADC" "IndirectX"   0x61,
    OperationSpec "ADC" "IndirectY"   0x71,

    OperationSpec "AND" "Immediate"   0x29,
    OperationSpec "AND" "ZeroPage"    0x25,
    OperationSpec "AND" "ZeroPageX"   0x35,
    OperationSpec "AND" "Absolute"    0x2D,
    OperationSpec "AND" "AbsoluteX"   0x3D,
    OperationSpec "AND" "AbsoluteY"   0x39,
    OperationSpec "AND" "IndirectX"   0x21,
    OperationSpec "AND" "IndirectY"   0x31,

    OperationSpec "ASL" "Accumulator" 0x0A,
    OperationSpec "ASL" "ZeroPage"    0x06,
    OperationSpec "ASL" "ZeroPageX"   0x16,
    OperationSpec "ASL" "Absolute"    0x0E,
    OperationSpec "ASL" "AbsoluteX"   0x1E,

    OperationSpec "BIT" "ZeroPage"    0x24,
    OperationSpec "BIT" "Absolute"    0x2C,

    OperationSpec "BPL" "Immediate"   0x10,
    OperationSpec "BMI" "Immediate"   0x30,
    OperationSpec "BVC" "Immediate"   0x50,
    OperationSpec "BVS" "Immediate"   0x70,
    OperationSpec "BCC" "Immediate"   0x90,
    OperationSpec "BCS" "Immediate"   0xB0,
    OperationSpec "BNE" "Immediate"   0xD0,
    OperationSpec "BEQ" "Immediate"   0xF0,

    OperationSpec "BRK" "Implied"     0x00,

    OperationSpec "CMP" "Immediate"   0xC9,
    OperationSpec "CMP" "ZeroPage"    0xC5,
    OperationSpec "CMP" "ZeroPageX"   0xD5,
    OperationSpec "CMP" "Absolute"    0xCD,
    OperationSpec "CMP" "AbsoluteX"   0xDD,
    OperationSpec "CMP" "AbsoluteY"   0xD9,
    OperationSpec "CMP" "IndirectX"   0xC1,
    OperationSpec "CMP" "IndirectY"   0xD1,

    OperationSpec "CPX" "Immediate"   0xE0,
    OperationSpec "CPX" "ZeroPage"    0xE4,
    OperationSpec "CPX" "Absolute"    0xEC,

    OperationSpec "CPY" "Immediate"   0xC0,
    OperationSpec "CPY" "ZeroPage"    0xC4,
    OperationSpec "CPY" "Absolute"    0xCC,

    OperationSpec "DEC" "ZeroPage"    0xC6,
    OperationSpec "DEC" "ZeroPageX"   0xD6,
    OperationSpec "DEC" "Absolute"    0xCE,
    OperationSpec "DEC" "AbsoluteX"   0xDE,

    OperationSpec "EOR" "Immediate"   0x49,
    OperationSpec "EOR" "ZeroPage"    0x45,
    OperationSpec "EOR" "ZeroPageX"   0x55,
    OperationSpec "EOR" "Absolute"    0x4D,
    OperationSpec "EOR" "AbsoluteX"   0x5D,
    OperationSpec "EOR" "AbsoluteY"   0x59,
    OperationSpec "EOR" "IndirectX"   0x41,
    OperationSpec "EOR" "IndirectY"   0x51,

    OperationSpec "CLC" "Implied"     0x18,
    OperationSpec "SEC" "Implied"     0x38,
    OperationSpec "CLI" "Implied"     0x58,
    OperationSpec "SEI" "Implied"     0x78,
    OperationSpec "CLV" "Implied"     0xB8,
    OperationSpec "CLD" "Implied"     0xD8,
    OperationSpec "SED" "Implied"     0xF8,

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

    OperationSpec "TAX" "Implied"     0xAA,
    OperationSpec "TXA" "Implied"     0x8A,
    OperationSpec "DEX" "Implied"     0xCA,
    OperationSpec "INX" "Implied"     0xE8,
    OperationSpec "TAY" "Implied"     0xA8,
    OperationSpec "TYA" "Implied"     0x98,
    OperationSpec "DEY" "Implied"     0x88,
    OperationSpec "INY" "Implied"     0xC8,

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

    OperationSpec "RTI" "Implied"     0x40,
    OperationSpec "RTS" "Implied"     0x60,

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

    OperationSpec "TXS" "Implied"     0x9A,
    OperationSpec "TSX" "Implied"     0xBA,
    OperationSpec "PHA" "Implied"     0x48,
    OperationSpec "PLA" "Implied"     0x68,
    OperationSpec "PHP" "Implied"     0x08,
    OperationSpec "PLP" "Implied"     0x28,

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

readInstruction :: [Word8] -> (Instruction, [Word8])
readInstruction buffer = 
    let opspec = specFor $ head buffer
        psize = getAddressingSize $ modeName opspec
        tbuffer = tail buffer
        instruction = Instruction (operationName opspec) (buildAddressingMode (modeName opspec) (take psize tbuffer))
    in (instruction, drop psize tbuffer)

readNInstructions :: Int -> [Word8] -> ([Instruction], [Word8])
readNInstructions 0 buffer = ([], buffer)
readNInstructions n buffer = 
    let (i, tbuf) = readInstruction buffer
        (l, ttbuf) = readNInstructions (n - 1) tbuf
    in (i : l, ttbuf)

toAsm :: Instruction -> String
toAsm (Instruction op adm) = op ++ " " ++ (toAsmADM adm)

toAsmADM :: AddressingMode -> String
toAsmADM (Immediate i) = printf "#$%02x" i
toAsmADM (ZeroPage o) = printf "$%02x" o
toAsmADM (ZeroPageX o) = printf "$%02x, X" o
toAsmADM (Absolute (a, b)) = printf "$%02x%02x" a b
toAsmADM (AbsoluteX (a, b)) = printf "$%02x%02x, X" a b
toAsmADM (AbsoluteY (a, b)) = printf "$%02x%02x, Y" a b
toAsmADM (IndirectX o) = printf "($%02x, X)" o
toAsmADM (IndirectY o) = printf "($%02x), Y" o
toAsmADM _ = ""

disassembleBuffer :: [Word8] -> [String]
disassembleBuffer buf = let (is, _) = readNInstructions 128 buf in
    map toAsm is

main :: IO ()
main = do
    args <- getArgs
    fcBS <- Data.ByteString.readFile $ head args
    putStr $ unlines $ disassembleBuffer (drop 16 $ Data.ByteString.unpack fcBS)
