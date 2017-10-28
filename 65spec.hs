module Spec (Opcode (Opcode), opcodeName, opcodeMode, decode, encode) where

import Data.List (find)
import Data.Word

data Opcode = Opcode {
    opcodeName :: String,
    opcodeMode :: String	
}

decode :: Word8 -> Maybe Opcode
decode b = do 
    (OperationSpec name mode _) <- find (\s -> opcode s == b) spec
    return $ Opcode name mode

encode :: Opcode -> Maybe Word8
encode (Opcode name mode) = fmap opcode (find pred spec)
    where pred = \(OperationSpec sname smode _) -> (name == sname && mode == smode)

data OperationSpec = OperationSpec {
    specName :: String,
    specMode :: String,
    opcode :: Word8    
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

    OperationSpec "BRK" "Implied"     0x00, -- 000 000 00

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

    OperationSpec "INC" "ZeroPage"    0xE6, -- 111 001 10
    OperationSpec "INC" "ZeroPageX"   0xF6, -- 111 101 10
    OperationSpec "INC" "Absolute"    0xEE, -- 111 011 10
    OperationSpec "INC" "AbsoluteX"   0xFE, -- 111 111 10

    OperationSpec "JMP" "Absolute"    0x4C, -- 010 011 00
    OperationSpec "JMP" "Indirect"    0x6C, -- 011 011 00

    OperationSpec "JSR" "Absolute"    0x20, -- 001 000 00

    OperationSpec "LDA" "Immediate"   0xA9, -- 101 010 01
    OperationSpec "LDA" "ZeroPage"    0xA5, -- 101 001 01
    OperationSpec "LDA" "ZeroPageX"   0xB5, -- 101 101 01
    OperationSpec "LDA" "Absolute"    0xAD, -- 101 011 01
    OperationSpec "LDA" "AbsoluteX"   0xBD, -- 101 111 01
    OperationSpec "LDA" "AbsoluteY"   0xB9, -- 101 110 01
    OperationSpec "LDA" "IndirectX"   0xA1, -- 101 000 01
    OperationSpec "LDA" "IndirectY"   0xB1, -- 101 100 01

    OperationSpec "LDX" "Immediate"   0xA2, -- 101 000 10
    OperationSpec "LDX" "ZeroPage"    0xA6, -- 101 001 10
    OperationSpec "LDX" "ZeroPageY"   0xB6, -- 101 101 10
    OperationSpec "LDX" "Absolute"    0xAE, -- 101 011 10
    OperationSpec "LDX" "AbsoluteY"   0xBE, -- 101 111 10

    OperationSpec "LDY" "Immediate"   0xA0, -- 101 000 00
    OperationSpec "LDY" "ZeroPage"    0xA4, -- 101 001 00
    OperationSpec "LDY" "ZeroPageX"   0xB4, -- 101 101 00
    OperationSpec "LDY" "Absolute"    0xAC, -- 101 011 00
    OperationSpec "LDY" "AbsoluteX"   0xBC, -- 101 111 00

    OperationSpec "LSR" "Accumulator" 0x4A, -- 010 010 10
    OperationSpec "LSR" "ZeroPage"    0x46, -- 010 001 10
    OperationSpec "LSR" "ZeroPageX"   0x56, -- 010 101 10
    OperationSpec "LSR" "Absolute"    0x4E, -- 010 011 10
    OperationSpec "LSR" "AbsoluteX"   0x5E, -- 010 111 10

    OperationSpec "NOP" "Implied"     0xEA, -- 111 010 10

    OperationSpec "ORA" "Immediate"   0x09, -- 000 010 01
    OperationSpec "ORA" "ZeroPage"    0x05, -- 000 001 01
    OperationSpec "ORA" "ZeroPageX"   0x15, -- 000 101 01
    OperationSpec "ORA" "Absolute"    0x0D, -- 000 011 01
    OperationSpec "ORA" "AbsoluteX"   0x1D, -- 000 111 01
    OperationSpec "ORA" "AbsoluteY"   0x19, -- 000 110 01
    OperationSpec "ORA" "IndirectX"   0x01, -- 000 000 01
    OperationSpec "ORA" "IndirectY"   0x11, -- 000 100 01

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

    OperationSpec "ROL" "Accumulator" 0x2A, -- 001 010 10
    OperationSpec "ROL" "ZeroPage"    0x26, -- 001 001 10
    OperationSpec "ROL" "ZeroPageX"   0x36, -- 001 101 10
    OperationSpec "ROL" "Absolute"    0x2E, -- 001 011 10
    OperationSpec "ROL" "AbsoluteX"   0x3E, -- 001 111 10

    OperationSpec "ROR" "Accumulator" 0x6A, -- 011 010 10
    OperationSpec "ROR" "ZeroPage"    0x66, -- 011 001 10
    OperationSpec "ROR" "ZeroPageX"   0x76, -- 011 101 10
    OperationSpec "ROR" "Absolute"    0x6E, -- 011 011 10
    OperationSpec "ROR" "AbsoluteX"   0x7E, -- 011 111 10

    OperationSpec "SBC" "Immediate"   0xE9, -- 111 010 01
    OperationSpec "SBC" "ZeroPage"    0xE5, -- 111 001 01
    OperationSpec "SBC" "ZeroPageX"   0xF5, -- 111 101 01
    OperationSpec "SBC" "Absolute"    0xED, -- 111 011 01
    OperationSpec "SBC" "AbsoluteX"   0xFD, -- 111 111 01
    OperationSpec "SBC" "AbsoluteY"   0xF9, -- 111 110 01
    OperationSpec "SBC" "IndirectX"   0xE1, -- 111 000 01
    OperationSpec "SBC" "IndirectY"   0xF1, -- 111 100 01

    OperationSpec "STA" "ZeroPage"    0x85, -- 100 001 01
    OperationSpec "STA" "ZeroPageX"   0x95, -- 100 101 01
    OperationSpec "STA" "Absolute"    0x8D, -- 100 011 01
    OperationSpec "STA" "AbsoluteX"   0x9D, -- 100 111 01
    OperationSpec "STA" "AbsoluteY"   0x99, -- 100 110 01
    OperationSpec "STA" "IndirectX"   0x81, -- 100 000 01
    OperationSpec "STA" "IndirectY"   0x91, -- 100 100 01

    OperationSpec "STX" "ZeroPage"    0x86, -- 100 001 10
    OperationSpec "STX" "ZeroPageY"   0x96, -- 100 101 10
    OperationSpec "STX" "Absolute"    0x8E, -- 100 011 10

    OperationSpec "STY" "ZeroPage"    0x84, -- 100 001 00
    OperationSpec "STY" "ZeroPageX"   0x94, -- 100 101 00
    OperationSpec "STY" "Absolute"    0x8C  -- 100 011 00
   ]
