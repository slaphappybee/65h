module Assembler (
    ObjectToken (Byte),
    TokenBlock (TokenBlock),
    assembleBlock,
    resolveAll
    ) where

import Parser
import Spec

import Data.Word
import Data.Maybe (fromJust)
import Data.List (find)

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

assembleStatement :: Statement -> [ObjectToken]
assembleStatement (InstructionStatement (Instruction s)) = [Byte $ fromJust $ encode (Opcode s "Implied")]
assembleStatement (MacroStatement "for" [ParameterInstruction (Instruction "jmp")]) = (Byte $ fromJust $ encode $ Opcode "JMP" "Absolute") : (splitReference $ RelativeLocation (0-1))
assembleStatement (MacroStatement "raw16" [ParameterValue (ValueModifier '#' (ValueIdentifier identifier))]) = (splitReference $ ExternalReference identifier)

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

resolveExpressions :: [TokenBlock (String, Word16)] -> [TokenBlock Word16]
resolveExpressions blocks = let symbols = fmap blockIdentity blocks in
    fmap (resolveBlockRefs symbols) blocks

resolveAll :: [TokenBlock (String, Word16)] -> [TokenBlock Word16]
resolveAll = resolveExpressions . (fmap resolveRelativeLocations)
