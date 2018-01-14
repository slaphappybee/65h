module Assembler (
    assemble
    ) where

import Parser
import Spec

import Data.Word
import Data.Maybe (fromJust, maybeToList)
import Data.List (find)
import Data.Char (ord)

data TokenBlock identity = TokenBlock {
    blockIdentity :: identity,
    blockData :: [ObjectToken]
} deriving Show

data ObjectToken =
    Byte Word8 |
    Reference ReferenceExpression
    deriving Show

data ReferenceExpression =
    LowByte ReferenceExpression |
    HighByte ReferenceExpression |
    RelativeLocation Int |
    ExternalReference String
    deriving Show

fromRight (Right x) = x

getLowByte :: Word16 -> Word8
getLowByte w = fromIntegral (w `mod` 256)

getHighByte :: Word16 -> Word8
getHighByte w = fromIntegral (w `quot` 256)

findT1 :: Eq a => [(a, b)] -> a -> b
findT1 l v = case fromJust $ find (\(x, y) -> x == v) l of (_, r) -> r

splitReference :: ReferenceExpression -> [ObjectToken]
splitReference (RelativeLocation i) = [Reference $ LowByte (RelativeLocation i), Reference $ HighByte (RelativeLocation i)]
splitReference (ExternalReference n) = [Reference $ LowByte (ExternalReference n), Reference $ HighByte (ExternalReference n)]

encodeOpcode :: String -> String -> ObjectToken
encodeOpcode s a = Byte $ fromJust $ encode (Opcode s a)

fromImmediate :: Value -> ObjectToken
fromImmediate (ValueLiteral (Left b))                  = Byte b
fromImmediate (ValueModifier '<' (ValueIdentifier id)) = Reference $ LowByte  $ ExternalReference id
fromImmediate (ValueModifier '>' (ValueIdentifier id)) = Reference $ HighByte $ ExternalReference id

assembleInstruction :: String -> Maybe AddressingParameter -> [ObjectToken]
assembleInstruction s Nothing = [encodeOpcode s "Implied"]
assembleInstruction s (Just (AddressingAbsolute (ValueIdentifier identifier)))                  = encodeOpcode s "Absolute"  : (splitReference $ ExternalReference identifier)
assembleInstruction s (Just (AddressingRelative (ValueIdentifier identifier) 'X'))              = encodeOpcode s "AbsoluteX" : (splitReference $ ExternalReference identifier)
assembleInstruction s (Just (AddressingAbsolute (ValueModifier '#' imm)))                       = encodeOpcode s "Immediate" : [fromImmediate imm]

encodeForBranch :: Instruction -> Int -> [ObjectToken]
encodeForBranch (Instruction "jmp" Nothing) l = (Byte $ fromJust $ encode $ Opcode "JMP" "Absolute") : (splitReference $ RelativeLocation (0-l-1))
encodeForBranch (Instruction bi    Nothing) l = (Byte $ fromJust $ encode $ Opcode bi   "Immediate") : [Byte $ fromIntegral (0-l-2)]

pToSS :: MacroParameter -> [Statement]
pToSS (ParameterInstruction i) = [InstructionStatement i]

buildForLoop :: [Statement] -> MacroParameter -> [Statement] -> [Statement] -> [ObjectToken]
buildForLoop s (ParameterInstruction b) i ss = initStatement ++ assembledBlock ++ (encodeForBranch b (length assembledBlock))
    where
        initStatement = concat $ map assembleStatement s
        assembledBlock = concat $ map assembleStatement (ss ++ i)

macroFor :: [MacroParameter] -> [Statement] -> [ObjectToken]
macroFor [b]     ss = buildForLoop []        b []        ss
macroFor [s,b,i] ss = buildForLoop (pToSS s) b (pToSS i) ss

macroRaw8 :: [MacroParameter] -> [ObjectToken]
macroRaw8 vals = concat (fmap unboxImmediate8 vals)
    where unboxImmediate8 (ParameterValue (AddressingAbsolute (ValueModifier '#' (ValueLiteral (Left b))))) = [Byte b]
          unboxImmediate8 (ParameterString s) = fmap (Byte . fromIntegral . ord) s

macroRaw16 :: [MacroParameter] -> [ObjectToken]
macroRaw16 [ParameterValue (AddressingAbsolute (ValueModifier '#' (ValueIdentifier identifier)))] = (splitReference $ ExternalReference identifier)

macroTUA :: [MacroParameter] -> [ObjectToken]
macroTUA [(ParameterValue dst),(ParameterValue src)] =
    (assembleInstruction "LDA" (Just src)) ++
    (assembleInstruction "STA" (Just dst))

macroTUXIO :: [MacroParameter] -> [ObjectToken]
macroTUXIO [(ParameterValue dst),(ParameterValue src)] =
    (assembleInstruction "LDA" (Just (sliceImm '>' src))) ++
    (assembleInstruction "STA" (Just dst)) ++
    (assembleInstruction "LDA" (Just (sliceImm '<' src))) ++
    (assembleInstruction "STA" (Just dst))
    where sliceImm c (AddressingAbsolute (ValueModifier '#' imm)) = AddressingAbsolute (ValueModifier '#' (ValueModifier c imm))

assembleStatement :: Statement -> [ObjectToken]
assembleStatement (InstructionStatement (Instruction s p)) = assembleInstruction s p
assembleStatement (MacroStatement "for" p b) = macroFor p b
assembleStatement (MacroStatement "raw8" p []) = macroRaw8 p
assembleStatement (MacroStatement "raw16" p []) = macroRaw16 p
assembleStatement (MacroStatement "tux_io" p []) = macroTUXIO p
assembleStatement (MacroStatement "tua" p []) = macroTUA p

assembleBlock :: SubBlock -> TokenBlock (String, Word16)
assembleBlock (SubBlock n a ss) = TokenBlock (n, a) (concat $ fmap assembleStatement ss)

resolveRelativeLocation :: Int -> ObjectToken -> ObjectToken
resolveRelativeLocation off (Reference (LowByte (RelativeLocation delta))) = Byte $ getLowByte $ fromIntegral $ (off + delta)
resolveRelativeLocation off (Reference (HighByte (RelativeLocation delta))) = Byte $ getHighByte $ fromIntegral $ (off + delta)
resolveRelativeLocation _ b = b

resolveRelativeLocations :: TokenBlock (String, Word16) -> TokenBlock (String, Word16)
resolveRelativeLocations (TokenBlock (name, off) toks) = TokenBlock (name, off) (zipWith resolveRelativeLocation [(fromIntegral off)..] toks)

resolveBlockRefs :: [(String, Word16)] -> TokenBlock (String, Word16) -> TokenBlock Word16
resolveBlockRefs symbols (TokenBlock (name, addr) toks) = TokenBlock addr (fmap (resolveTokenRefs symbols) toks)

resolveTokenRefs :: [(String, Word16)] -> ObjectToken -> ObjectToken
resolveTokenRefs symbols (Reference expr) = Byte $ resolveExprRefs symbols expr
resolveTokenRefs _ b = b

resolveExprRefs :: [(String, Word16)] -> ReferenceExpression -> Word8
resolveExprRefs symbols (LowByte (ExternalReference name)) = getLowByte $ findT1 symbols name
resolveExprRefs symbols (HighByte (ExternalReference name)) = getHighByte $ findT1 symbols name

resolveExpressions :: [(String, Word16)] -> [TokenBlock (String, Word16)] -> [TokenBlock Word16]
resolveExpressions symbols blocks = let allSymbols = symbols ++ (fmap blockIdentity blocks) in
    fmap (resolveBlockRefs allSymbols) blocks

resolveAll :: [(String, Word16)] -> [TokenBlock (String, Word16)] -> [TokenBlock Word16]
resolveAll symbols = (resolveExpressions symbols) . (fmap resolveRelativeLocations)

processVar :: VariableDeclaration -> (String, Word16)
processVar (VariableDeclaration Extern W8 name addr) = (name, addr)
processVar (VariableDeclaration Const W16 name addr) = (name, addr)

unbox :: TokenBlock Word16 -> (Word16, [Word8])
unbox (TokenBlock id bs) = (id, unboxB <$> bs)
    where unboxB t = case t of Byte b -> b

assemble :: [TopLevelElement] -> [(Word16, [Word8])]
assemble els = unbox <$> (resolveAll symbols $ assembleBlock <$> blocks)
    where 
        (blocks, vars) = partitionTopLevel els
        symbols = map processVar vars
