module As where
import Data.Word

newtype Sreg  = Word8    -- Scalar register
newtype Vreg  = Word8    -- Vector register
newtype Mem   = (Sreg, Imm8) -- Memory address (ie. [S1 + imm8])
newtype Imm8  = Word8    -- 8-bit value
newtype Imm16 = Word16  -- 16-bit value

data MOV =
    SCALARMOV_RR Sreg Sreg      -- mov s1, s2
  | SCALARMOV_RM Sreg Mem       -- mov s1, [s2 + imm8]
  | SCALARMOV_MR Mem Sreg       -- mov [s1 + imm8], s2
  | SCALARMOV_RI Sreg Imm16     -- mov s1, imm8
  | VECTORPERM   Vreg Vreg Imm8 -- mov v1, v2, perm
  | VECTORMOV_RM Vreg Mem       -- mov v1, [s2 + imm8]
  | VECTORMOV_MR Mem Vreg       -- mov [s1 + imm8], v2
  | BROADCAST    Vreg Vreg Imm8 -- mov v1, v2, imm8

data ALU =
    SADD Sreg Sreg Sreg          -- add s1, s2, s3
  | SMUL Sreg Sreg Sreg          -- mul s1, s2, s3
  | SNEG Sreg Sreg               -- neg s1, s2
  | SDIV Sreg Sreg Sreg          -- div s1, s2, s3
  | SAND Sreg Sreg Sreg          -- and s1, s2, s3
  | SOR  Sreg Sreg Sreg          -- or s1, s2, s3
  | SXOR Sreg Sreg Sreg          -- xor s1, s2, s3
  | SNOT Sreg Sreg               -- not s1, s2
  | VADD Vreg Vreg Vreg          -- add v1, v2, v3
  | VMUL Vreg Vreg Vreg          -- mul v1, v2, v3
  | VNEG Vreg Vreg               -- neg v1, v2
  | VDIV Vreg Vreg Vreg          -- div v1, v2, v3
  | VAND Vreg Vreg Vreg          -- and v1, v2, v3
  | VOR  Vreg Vreg Vreg          -- or v1, v2, v3
  | VXOR Vreg Vreg Vreg          -- xor v1, v2, v3
  | VNOT Vreg Vreg               -- not v1, v2 

data Instruction = 
    MOV MOV
  | ALU ALU

parse :: String -> Instruction
parse prog =
  case words prog of
    ["mov", a, b] -> 

