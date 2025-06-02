{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Bits
import Data.Binary.Put (runPut, putWord32be)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Char (digitToInt, isSpace)
import Data.Functor (($>))
import Data.List (find, groupBy, intercalate, isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (replace)
import Data.Word
import Numeric (showHex)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Parsec (Parsec, ParsecT, Stream, char, choice, count, eof, digit, hexDigit, many, many1, parse, spaces, string, try, (<|>))
import Text.Printf (printf)

-- TYPES

newtype Sreg = Sreg Word8 -- ^Scalar register

newtype Vreg = Vreg Word8 -- ^Vector register

newtype Imm8 = Imm8 Word8 -- ^8-bit value

newtype Imm16 = Imm16 Word16 -- ^16-bit value

newtype Mem = Mem (Sreg, Imm8) -- ^memory address (ie. [S1 + imm8])

data Lane = Lane0 | Lane1 | Lane2 | Lane3 | Lang4 | Lane5 | Lane6 | Lane7 deriving Enum

data MOV
  = SCALARMOV_RR Sreg Sreg -- ^mov s1, s2
  | SCALARMOV_RM Sreg Mem -- ^mov s1, [s2 + imm8]
  | SCALARMOV_MR Mem Sreg -- ^mov [s1 + imm8], s2
  | SCALARMOV_RI Sreg Imm16 -- ^mov s1, imm16
  | VECTORMOV_RR Vreg Vreg -- ^mov v1, v2
  | VECTORMOV_RM Vreg Mem -- ^mov v1, [s2 + imm8]
  | VECTORMOV_MR Mem Vreg -- ^mov [s1 + imm8], v2
  | BROADCAST Vreg Sreg Imm8 -- ^mov v1, s2, imm8 OR mov v1, s2

data ALU
  = SADD Sreg Sreg Sreg -- ^add s1, s2, s3
  | SMUL Sreg Sreg Sreg -- ^mul s1, s2, s3
  | SNEG Sreg Sreg -- ^neg s1, s2
  | SDIV Sreg Sreg Sreg -- ^div s1, s2, s3
  | SAND Sreg Sreg Sreg -- ^and s1, s2, s3
  | SOR Sreg Sreg Sreg -- ^or s1, s2, s3
  | SXOR Sreg Sreg Sreg -- ^xor s1, s2, s3
  | SNOT Sreg Sreg -- ^not s1, s2
  | VADD Vreg Vreg Vreg -- ^add v1, v2, v3
  | VMUL Vreg Vreg Vreg -- ^mul v1, v2, v3
  | VNEG Vreg Vreg -- ^neg v1, v2
  | VDIV Vreg Vreg Vreg -- ^div v1, v2, v3
  | VAND Vreg Vreg Vreg -- ^and v1, v2, v3
  | VOR Vreg Vreg Vreg -- ^or v1, v2, v3
  | VXOR Vreg Vreg Vreg -- ^xor v1, v2, v3
  | VNOT Vreg Vreg -- ^not v1, v2
  | VEQ Vreg Vreg Vreg -- ^veq v1, v2, v3
  | VGT Vreg Vreg Vreg -- ^vgt v1, v2, v3

data PERM
  = SCATTER [Lane] -- ^scatter l1, l2, l3, l4, l5, l6, l7, l8
  | GATHER [Lane] -- ^gather l1, l2, l3, l4, l5, l6, l7, l8
  | LOAD Vreg -- ^mov vperm, v1
  | STORE Vreg -- ^mov v1, vperm

data JUMP
  = J Imm16 -- ^jump
  | JEQ Imm16 -- ^jump if equal
  | JNE Imm16 -- ^jump if not equal
  | JGE Imm16 -- ^jump if greater or equal
  | JLE Imm16 -- ^jump if less or equal
  | JGT Imm16 -- ^jump if greater
  | JLT Imm16 -- ^jump if less
  | JR Sreg -- ^jump to scalar register

data CMP
  = CMP_RR Sreg Sreg -- ^cmp s1, s2
  | CMP_RI Sreg Imm16 -- ^cmp s1, imm16
  | CMP_IR Imm16 Sreg -- ^cmp imm16, s1
  | SET Imm8 -- ^set imm8 (sets flags)

data INOUT
  = INL Sreg -- ^inl s1; read byte from serial into lower half of s1
  | INH Sreg -- ^inh s1; read byte from serial into higher half of s1
  | OUTL Sreg -- ^outl s1; write lower half of s1 to serial
  | OUTH Sreg -- ^outh s1; write higher half of s1 to serial

data Instruction
  = MOV MOV
  | ALU ALU
  | PERM PERM
  | JUMP JUMP
  | CMP CMP
  | INOUT INOUT
  | HALT

newtype Directive
  = ORG Imm16 -- ^org directive, sets current offset

data Line = Instruction Instruction | Directive Directive

#ifdef ANSICOLOR
instance Show Sreg where show (Sreg s) = "\x1b[32ms" ++ show s ++ "\x1b[0m"
instance Show Vreg where show (Vreg v) = "\x1b[34mv" ++ show v ++ "\x1b[0m"
instance Show Imm8 where show (Imm8 i) = "\x1b[35m0x" ++ showHex i "" ++ "\x1b[0m"
instance Show Imm16 where show (Imm16 i) = "\x1b[35m0x" ++ showHex i "" ++ "\x1b[0m"
instance Show Lane where show l = "\x1b[36m" ++ (show . fromEnum) l ++ "\x1b[0m"
#else
instance Show Sreg where show (Sreg s) = "s" ++ show s
instance Show Vreg where show (Vreg v) = "v" ++ show v
instance Show Imm8 where show (Imm8 i) = "0x" ++ showHex i ""
instance Show Imm16 where show (Imm16 i) = "0x" ++ showHex i ""
instance Show Lane where show = show . fromEnum
#endif

instance Show Mem where show (Mem (s, Imm8 i)) = "[" ++ show s ++ " + " ++ show i ++ "]"

instance Show MOV where
#ifdef ANSICOLOR
  show m = "\x1b[33mmov\x1b[0m " ++ case m of
#else
  show m = "mov " ++ case m of
#endif
    SCALARMOV_RR s1 s2 -> show s1 ++ ", " ++ show s2
    SCALARMOV_RM s1 m -> show s1 ++ ", " ++ show m
    SCALARMOV_MR m s2 -> show m ++ ", " ++ show s2
    SCALARMOV_RI s1 i -> show s1 ++ ", " ++ show i
    VECTORMOV_RR v1 v2 -> show v1 ++ ", " ++ show v2
    VECTORMOV_RM v1 m -> show v1 ++ ", " ++ show m
    VECTORMOV_MR m v2 -> show m ++ ", " ++ show v2
    BROADCAST v1 s2 i -> show v1 ++ ", " ++ show s2 ++ ", " ++ show i

instance Show ALU where
#ifdef ANSICOLOR
  show a = "\x1b[33m" ++ case a of
#else
  show a = case a of
#endif
    SADD s1 s2 s3 -> "add " ++ show s1 ++ ", " ++ show s2 ++ ", " ++ show s3
    SMUL s1 s2 s3 -> "mul " ++ show s1 ++ ", " ++ show s2 ++ ", " ++ show s3
    SNEG s1 s2 -> "neg " ++ show s1 ++ ", " ++ show s2
    SDIV s1 s2 s3 -> "div " ++ show s1 ++ ", " ++ show s2 ++ ", " ++ show s3
    SAND s1 s2 s3 -> "and " ++ show s1 ++ ", " ++ show s2 ++ ", " ++ show s3
    SOR s1 s2 s3 -> "or " ++ show s1 ++ ", " ++ show s2 ++ ", " ++ show s3
    SXOR s1 s2 s3 -> "xor " ++ show s1 ++ ", " ++ show s2 ++ ", " ++ show s3
    SNOT s1 s2 -> "not " ++ show s1 ++ ", " ++ show s2
    VADD v1 v2 v3 -> "add " ++ show v1 ++ ", " ++ show v2 ++ ", " ++ show v3
    VMUL v1 v2 v3 -> "mul " ++ show v1 ++ ", " ++ show v2 ++ ", " ++ show v3
    VNEG v1 v2 -> "neg " ++ show v1 ++ ", " ++ show v2
    VDIV v1 v2 v3 -> "div " ++ show v1 ++ ", " ++ show v2 ++ ", " ++ show v3
    VAND v1 v2 v3 -> "and " ++ show v1 ++ ", " ++ show v2 ++ ", " ++ show v3
    VOR v1 v2 v3 -> "or " ++ show v1 ++ ", " ++ show v2 ++ ", " ++ show v3
    VXOR v1 v2 v3 -> "xor " ++ show v1 ++ ", " ++ show v2 ++ ", " ++ show v3
    VNOT v1 v2 -> "not " ++ show v1 ++ ", " ++ show v2
    VEQ v1 v2 v3 -> "veq " ++ show v1 ++ ", " ++ show v2 ++ ", " ++ show v3
    VGT v1 v2 v3 -> "vgt " ++ show v1 ++ ", " ++ show v2 ++ ", " ++ show v3

instance Show PERM where
#ifdef ANSICOLOR
  show p = "\x1b[33m" ++ case p of
#else
  show p = case p of
#endif
    SCATTER ls -> "scatter " ++ intercalate ", " (map show ls)
    GATHER ls -> "gather " ++ intercalate ", " (map show ls)
#ifdef ANSICOLOR
    LOAD v -> "mov \x1b[34mvperm\x1b[0m, " ++ show v
    STORE v -> "mov " ++ show v ++ ", \x1b[34mvperm\x1b[0m"
#else
    LOAD v -> "mov vperm, " ++ show v
    STORE v -> "mov " ++ show v ++ ", vperm"
#endif

instance Show JUMP where
#ifdef ANSICOLOR
  show j = "\x1b[33m" ++ case j of
#else
  show j = case j of
#endif
    J i -> "j " ++ show i
    JEQ i -> "jeq " ++ show i
    JNE i -> "jne " ++ show i
    JGE i -> "jge " ++ show i
    JLE i -> "jle " ++ show i
    JGT i -> "jgt " ++ show i
    JLT i -> "jlt " ++ show i
    JR s -> "jr " ++ show s

instance Show CMP where
#ifdef ANSICOLOR
  show t = "\x1b[33m" ++ case t of
#else
  show t = case t of
#endif
    CMP_RR s1 s2 -> "cmp " ++ show s1 ++ ", " ++ show s2
    CMP_RI s1 i -> "cmp " ++ show s1 ++ ", " ++ show i
    CMP_IR i s1 -> "cmp " ++ show i ++ ", " ++ show s1
    SET i -> "set " ++ show i

instance Show INOUT where
#ifdef ANSICOLOR
  show i = "\x1b[33m" ++ case i of
#else
  show i = case i of
#endif
    INL s -> "inl " ++ show s
    INH s -> "inh " ++ show s
    OUTL s -> "outl " ++ show s
    OUTH s -> "outh " ++ show s

instance Show Instruction where
  show (MOV m) = show m
  show (ALU a) = show a
  show (PERM p) = show p
  show (JUMP j) = show j
  show (CMP t) = show t
  show (INOUT i) = show i
  show HALT =
#ifdef ANSICOLOR
    "\x1b[33mhalt\x1b[0m"
#else
    "halt"
#endif

-- PARSING

countSepBy :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
countSepBy n p sep = do
  x <- p
  xs <- count (n - 1) (sep *> p)
  return (x:xs)

symbol :: Char -> Parsec String () Char
symbol c = spaces *> char c <* spaces

sreg :: Parsec String () Sreg
sreg = char 's' >> (Sreg . read <$> many1 digit)

vreg :: Parsec String () Vreg
vreg = char 'v' >> (Vreg . read <$> many1 digit)

imm8 :: Parsec String () Imm8
imm8 = Imm8 . read <$> (try (string "0x" <> many1 hexDigit) <|> many digit)

imm16 :: Parsec String () Imm16
imm16 = Imm16 . read <$> (try (string "0x" <> many1 hexDigit) <|> many digit)

mem :: Parsec String () Mem
mem = Mem <$> (symbol '[' *> liftA2 (,) sreg (symbol '+' *> imm8) <* symbol ']')

lane :: Parsec String () Lane
lane = toEnum . digitToInt <$> digit

mov :: Parsec String () MOV
mov =
  string "mov"
    *> spaces
    *> ( try (SCALARMOV_RR <$> sreg <* symbol ',' <*> sreg)
           <|> try (SCALARMOV_RM <$> sreg <* symbol ',' <*> mem)
           <|> try (SCALARMOV_MR <$> mem <* symbol ',' <*> sreg)
           <|> try (SCALARMOV_RI <$> sreg <* symbol ',' <*> imm16)
           <|> try (VECTORMOV_RR <$> vreg <* symbol ',' <*> vreg)
           <|> try (VECTORMOV_RM <$> vreg <* symbol ',' <*> mem)
           <|> try (VECTORMOV_MR <$> mem <* symbol ',' <*> vreg)
           <|> try (BROADCAST <$> vreg <* symbol ',' <*> sreg <* symbol ',' <*> imm8)
           <|> BROADCAST <$> vreg <* symbol ',' <*> sreg <*> (pure . Imm8) 0xFF
       )

alu :: Parsec String () ALU
alu = choice (map try (init ops) ++ [last ops])
  where
    ops =
      [ string "add" *> spaces *> (try (instr3 sreg SADD) <|> instr3 vreg VADD),
        string "mul" *> spaces *> (try (instr3 sreg SMUL) <|> instr3 vreg VMUL),
        string "neg" *> spaces *> (try (instr2 sreg SNEG) <|> instr2 vreg VNEG),
        string "div" *> spaces *> (try (instr3 sreg SDIV) <|> instr3 vreg VDIV),
        string "and" *> spaces *> (try (instr3 sreg SAND) <|> instr3 vreg VAND),
        string "or"  *> spaces *> (try (instr3 sreg SOR)  <|> instr3 vreg VOR),
        string "xor" *> spaces *> (try (instr3 sreg SXOR) <|> instr3 vreg VXOR),
        string "not" *> spaces *> (try (instr2 sreg SNOT) <|> instr2 vreg VNOT),
        string "veq" *> spaces *> instr3 vreg VEQ,
        string "vgt" *> spaces *> instr3 vreg VGT
      ]
    instr2 p c = c <$> p <* symbol ',' <*> p
    instr3 p c = c <$> p <* symbol ',' <*> p <* symbol ',' <*> p

perm :: Parsec String () PERM
perm = choice (map try (init opts) ++ [last opts])
  where
    opts =
      [ string "scatter" *> spaces *> (SCATTER <$> countSepBy 8 lane (symbol ',')),
        string "gather" *> spaces *> (GATHER <$> countSepBy 8 lane (symbol ',')),
        string "mov" *> spaces *> (try (LOAD <$> (string "vperm" *> symbol ',' *> vreg)) <|> (STORE <$> (vreg <* symbol ',' <* string "vperm")))
      ]

jump :: Parsec String () JUMP
jump = choice (map try (init opts) ++ [last opts])
  where
    opts =
      [ string "jeq" *> spaces *> (JEQ <$> imm16),
        string "jne" *> spaces *> (JNE <$> imm16),
        string "jge" *> spaces *> (JGE <$> imm16),
        string "jle" *> spaces *> (JLE <$> imm16),
        string "jgt" *> spaces *> (JGT <$> imm16),
        string "jlt" *> spaces *> (JLT <$> imm16),
        string "jr" *> spaces *> (JR <$> sreg),
        string "j" *> spaces *> (J <$> imm16)
      ]

cmp :: Parsec String () CMP
cmp = cmp' <|> (string "set" *> spaces *> (SET <$> imm8))
  where
    cmp' = string "cmp"
      *> spaces
      *> ( try (CMP_RR <$> sreg <* symbol ',' <*> sreg)
             <|> try (CMP_RI <$> sreg <* symbol ',' <*> imm16)
             <|> try (CMP_IR <$> imm16 <* symbol ',' <*> sreg)
         )

inout :: Parsec String () INOUT
inout = choice (map try (init opts) ++ [last opts])
  where
    opts =
      [ string "inl" *> spaces *> (INL <$> sreg),
        string "inh" *> spaces *> (INH <$> sreg),
        string "outl" *> spaces *> (OUTL <$> sreg),
        string "outh" *> spaces *> (OUTH <$> sreg)
      ]

instr :: Parsec String () Instruction
instr = choice (map try opts)
  where
    opts = [MOV <$> mov, ALU <$> alu, PERM <$> perm, JUMP <$> jump, CMP <$> cmp, INOUT <$> inout, string "halt" $> HALT]

dir :: Parsec String () Directive
dir = string ".org" *> spaces *> (ORG <$> imm16)

-- ENCODING

construct :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
construct a b c d = fromIntegral a .<<. 24 .|. fromIntegral b .<<. 16 .|. fromIntegral c .<<. 8 .|. fromIntegral d

packLanes :: [Lane] -> Word32
packLanes = foldl (\acc l -> acc .<<. 3 .|. (fromIntegral . fromEnum) l) 0

encode :: Instruction -> Word32
encode (MOV (SCALARMOV_RR (Sreg s1) (Sreg s2))) = construct 0b00000_000 s1 s2 0
encode (MOV (SCALARMOV_RM (Sreg s1) (Mem (Sreg s2, Imm8 i)))) = construct 0b00000_001 s1 s2 i
encode (MOV (SCALARMOV_MR (Mem (Sreg s1, Imm8 i)) (Sreg s2))) = construct 0b00000_010 s1 i s2
encode (MOV (SCALARMOV_RI (Sreg s1) (Imm16 i))) = construct 0b00000_011 s1 (fromIntegral (i .>>. 8)) (fromIntegral i)
encode (MOV (VECTORMOV_RR (Vreg v1) (Vreg v2))) = construct 0b00000_100 v1 v2 0
encode (MOV (VECTORMOV_RM (Vreg v1) (Mem (Sreg s2, Imm8 i)))) = construct 0b00000_101 v1 s2 i
encode (MOV (VECTORMOV_MR (Mem (Sreg s1, Imm8 i)) (Vreg v2))) = construct 0b00000_110 s1 i v2
encode (MOV (BROADCAST (Vreg v1) (Sreg s2) (Imm8 i))) = construct 0b00000_111 v1 s2 i
encode (ALU (SADD (Sreg s1) (Sreg s2) (Sreg s3))) = construct 0b00001_000 s1 s2 s3
encode (ALU (SMUL (Sreg s1) (Sreg s2) (Sreg s3))) = construct 0b00001_001 s1 s2 s3
encode (ALU (SNEG (Sreg s1) (Sreg s2))) = construct 0b00001_010 s1 s2 0
encode (ALU (SDIV (Sreg s1) (Sreg s2) (Sreg s3))) = construct 0b00001_011 s1 s2 s3
encode (ALU (SAND (Sreg s1) (Sreg s2) (Sreg s3))) = construct 0b00001_100 s1 s2 s3
encode (ALU (SOR (Sreg s1) (Sreg s2) (Sreg s3))) = construct 0b00001_101 s1 s2 s3
encode (ALU (SXOR (Sreg s1) (Sreg s2) (Sreg s3))) = construct 0b00001_110 s1 s2 s3
encode (ALU (SNOT (Sreg s1) (Sreg s2))) = construct 0b00001_111 s1 s2 0
encode (ALU (VADD (Vreg v1) (Vreg v2) (Vreg v3))) = construct 0b00010_000 v1 v2 v3
encode (ALU (VMUL (Vreg v1) (Vreg v2) (Vreg v3))) = construct 0b00010_001 v1 v2 v3
encode (ALU (VNEG (Vreg v1) (Vreg v2))) = construct 0b00010_010 v1 v2 0
encode (ALU (VDIV (Vreg v1) (Vreg v2) (Vreg v3))) = construct 0b00010_011 v1 v2 v3
encode (ALU (VAND (Vreg v1) (Vreg v2) (Vreg v3))) = construct 0b00010_100 v1 v2 v3
encode (ALU (VOR (Vreg v1) (Vreg v2) (Vreg v3))) = construct 0b00010_101 v1 v2 v3
encode (ALU (VXOR (Vreg v1) (Vreg v2) (Vreg v3))) = construct 0b00010_110 v1 v2 v3
encode (ALU (VNOT (Vreg v1) (Vreg v2))) = construct 0b00010_111 v1 v2 0
encode (ALU (VEQ (Vreg v1) (Vreg v2) (Vreg v3))) = construct 0b00011_000 v1 v2 v3
encode (ALU (VGT (Vreg v1) (Vreg v2) (Vreg v3))) = construct 0b00011_001 v1 v2 v3
encode (PERM (SCATTER ls)) = 0b00011_010 .<<. 24 .|. packLanes ls
encode (PERM (GATHER ls)) = 0b00011_011 .<<. 24 .|. packLanes ls
encode (PERM (LOAD (Vreg v))) = construct 0b00011_100 v 0 0
encode (PERM (STORE (Vreg v))) = construct 0b00011_101 v 0 0
encode (JUMP (J (Imm16 i))) = construct 0b00100_000 (fromIntegral (i .>>. 8)) (fromIntegral i) 0
encode (JUMP (JEQ (Imm16 i))) = construct 0b00100_001 (fromIntegral (i .>>. 8)) (fromIntegral i) 0
encode (JUMP (JNE (Imm16 i))) = construct 0b00100_010 (fromIntegral (i .>>. 8)) (fromIntegral i) 0
encode (JUMP (JGE (Imm16 i))) = construct 0b00100_011 (fromIntegral (i .>>. 8)) (fromIntegral i) 0
encode (JUMP (JLE (Imm16 i))) = construct 0b00100_100 (fromIntegral (i .>>. 8)) (fromIntegral i) 0
encode (JUMP (JGT (Imm16 i))) = construct 0b00100_101 (fromIntegral (i .>>. 8)) (fromIntegral i) 0
encode (JUMP (JLT (Imm16 i))) = construct 0b00100_110 (fromIntegral (i .>>. 8)) (fromIntegral i) 0
encode (JUMP (JR (Sreg s))) = construct 0b00100_111 s 0 0
encode (CMP (CMP_RR (Sreg s1) (Sreg s2))) = construct 0b00101_000 s1 s2 0
encode (CMP (CMP_RI (Sreg s1) (Imm16 i))) = construct 0b00101_001 s1 (fromIntegral (i .>>. 8)) (fromIntegral i)
encode (CMP (CMP_IR (Imm16 i) (Sreg s1))) = construct 0b00101_010 (fromIntegral (i .>>. 8)) (fromIntegral i) s1
encode (CMP (SET (Imm8 i))) = construct 0b00101_011 i 0 0
encode (INOUT (INL (Sreg s))) = construct 0b00110_000 s 0 0
encode (INOUT (INH (Sreg s))) = construct 0b00110_001 s 0 0
encode (INOUT (OUTL (Sreg s))) = construct 0b00110_010 s 0 0
encode (INOUT (OUTH (Sreg s))) = construct 0b00110_011 s 0 0
encode HALT = construct 0b00111_000 0 0 0

write :: Handle -> [Line] -> IO ()
write h [] = return ()
write h (Instruction i:t) = BL.hPut h (runPut $ putWord32be (encode i)) >> write h t
write h (Directive (ORG (Imm16 i)):t) = hSeek h AbsoluteSeek (fromIntegral i) >> write h t

-- DECODING

unpackLanes :: Word32 -> [Lane]
unpackLanes = map (toEnum . fromIntegral . (.&. 0b111)) . reverse . take 8 . iterate (.>>. 3)

decode :: Word8 -> Word8 -> Word8 -> Word8 -> Instruction
decode a b c d = case a of
  0b00000_000 -> MOV (SCALARMOV_RR (Sreg b) (Sreg c))
  0b00000_001 -> MOV (SCALARMOV_RM (Sreg b) (Mem (Sreg c, Imm8 d)))
  0b00000_010 -> MOV (SCALARMOV_MR (Mem (Sreg b, Imm8 c)) (Sreg d))
  0b00000_011 -> MOV (SCALARMOV_RI (Sreg b) (Imm16 (pack2 c d)))
  0b00000_100 -> MOV (VECTORMOV_RR (Vreg b) (Vreg c))
  0b00000_101 -> MOV (VECTORMOV_RM (Vreg b) (Mem (Sreg c, Imm8 d)))
  0b00000_110 -> MOV (VECTORMOV_MR (Mem (Sreg b, Imm8 c)) (Vreg d))
  0b00000_111 -> MOV (BROADCAST (Vreg b) (Sreg c) (Imm8 d))
  0b00001_000 -> ALU (SADD (Sreg b) (Sreg c) (Sreg d))
  0b00001_001 -> ALU (SMUL (Sreg b) (Sreg c) (Sreg d))
  0b00001_010 -> ALU (SNEG (Sreg b) (Sreg c))
  0b00001_011 -> ALU (SDIV (Sreg b) (Sreg c) (Sreg d))
  0b00001_100 -> ALU (SAND (Sreg b) (Sreg c) (Sreg d))
  0b00001_101 -> ALU (SOR (Sreg b) (Sreg c) (Sreg d))
  0b00001_110 -> ALU (SXOR (Sreg b) (Sreg c) (Sreg d))
  0b00001_111 -> ALU (SNOT (Sreg b) (Sreg c))
  0b00010_000 -> ALU (VADD (Vreg b) (Vreg c) (Vreg d))
  0b00010_001 -> ALU (VMUL (Vreg b) (Vreg c) (Vreg d))
  0b00010_010 -> ALU (VNEG (Vreg b) (Vreg c))
  0b00010_011 -> ALU (VDIV (Vreg b) (Vreg c) (Vreg d))
  0b00010_100 -> ALU (VAND (Vreg b) (Vreg c) (Vreg d))
  0b00010_101 -> ALU (VOR (Vreg b) (Vreg c) (Vreg d))
  0b00010_110 -> ALU (VXOR (Vreg b) (Vreg c) (Vreg d))
  0b00010_111 -> ALU (VNOT (Vreg b) (Vreg c))
  0b00011_000 -> ALU (VEQ (Vreg b) (Vreg c) (Vreg d))
  0b00011_001 -> ALU (VGT (Vreg b) (Vreg c) (Vreg d))
  0b00011_010 -> PERM (SCATTER (unpackLanes (pack3 b c d)))
  0b00011_011 -> PERM (GATHER (unpackLanes (pack3 b c d)))
  0b00011_100 -> PERM (LOAD (Vreg b))
  0b00011_101 -> PERM (STORE (Vreg b))
  0b00100_000 -> JUMP (J (Imm16 (pack2 b c)))
  0b00100_001 -> JUMP (JEQ (Imm16 (pack2 b c)))
  0b00100_010 -> JUMP (JNE (Imm16 (pack2 b c)))
  0b00100_011 -> JUMP (JGE (Imm16 (pack2 b c)))
  0b00100_100 -> JUMP (JLE (Imm16 (pack2 b c)))
  0b00100_101 -> JUMP (JGT (Imm16 (pack2 b c)))
  0b00100_110 -> JUMP (JLT (Imm16 (pack2 b c)))
  0b00100_111 -> JUMP (JR (Sreg b))
  0b00101_000 -> CMP (CMP_RR (Sreg b) (Sreg c))
  0b00101_001 -> CMP (CMP_RI (Sreg b) (Imm16 (pack2 c d)))
  0b00101_010 -> CMP (CMP_IR (Imm16 (pack2 b c)) (Sreg d))
  0b00101_011 -> CMP (SET (Imm8 b))
  0b00110_000 -> INOUT (INL (Sreg b))
  0b00110_001 -> INOUT (INH (Sreg b))
  0b00110_010 -> INOUT (OUTL (Sreg b))
  0b00110_011 -> INOUT (OUTH (Sreg b))
  0b00111_000 -> HALT
  _ -> error "Invalid instruction"
  where
    pack2 x y = fromIntegral x .<<. 8 .|. fromIntegral y
    pack3 x y z = fromIntegral x .<<. 16 .|. fromIntegral y .<<. 8 .|. fromIntegral z

-- PREPROCESSING

takeWhileIncluding :: (a -> Bool) -> [a] -> [a]
takeWhileIncluding _ [] = []
takeWhileIncluding p (x:xs) =
  x : if p x then takeWhileIncluding p xs else []

getLabels :: [String] -> (Map String Int, [String])
getLabels l = get (foldl go (Map.empty, [], 0) l)
  where
    go (m, acc, count) l
      | null s = (m, "":acc, count)
      | ':' `elem` s = (Map.insert name (count * 4) m, "":acc, count)
      | otherwise = (m, s':acc, count + 1)
        where
          s = takeWhile (/= ';') $ dropWhile isSpace l
          s' = takeWhileIncluding (/= ';') $ dropWhile isSpace l
          name = takeWhile (/= ':') s
    get (a,b,c) = (a, reverse b)

resolveLabel :: Map String Int -> String -> String
resolveLabel m line = replaceAll (Map.map show m) line
  where replaceAll m line = unwords $ map (\tok -> Map.findWithDefault tok tok m) (words line)

-- MAIN

usage :: IO ()
usage = putStrLn "usage: simd as <input file> <output file>\n       simd objdump <input file>" >> exitFailure

main :: IO ()
main =
  getArgs >>= \case
    ["as", i, o] ->
      readFile i >>= \s ->
        let (labels, ls) = getLabels (lines s)
            s' = unlines (map (resolveLabel labels) ls)
        in case parse (many1 (spaces *> ((Instruction <$> instr <* symbol ';') <|> (Directive <$> dir)) <* spaces) <* eof) i s' of
          Left err -> print err >> exitFailure
          Right is -> withBinaryFile o WriteMode $ \h -> write h is
    ["objdump", i] ->
      BS.readFile i >>=
      mapM_ printRun . groupBy (\(_,d1) (_,d2) -> d1 == d2) . zip [0 :: Integer ..] . chunk4 . BS.unpack
      where
        chunk4 :: [Word8] -> [(Word8, Word8, Word8, Word8)]
        chunk4 [] = []
        chunk4 (a : b : c : d : t) = (a, b, c, d) : chunk4 t
        chunk4 _ = error "Invalid input file"
        printRun :: [(Integer, (Word8, Word8, Word8, Word8))] -> IO ()
        printRun ((i, (a, b, c, d)):t) = do
          printf "%8x:\t%08b %02x %02x %02x\t%s\n" (i * 4) a b c d (show (decode a b c d))
          unless (null t) $
            printf "\t\t...\n%8x:\t%08b %02x %02x %02x\t%s\n" (ti * 4) ta tb tc td (show (decode ta tb tc td))
          where
            (ti, (ta, tb, tc, td)) = last t
    _ -> usage
