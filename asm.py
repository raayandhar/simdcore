from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from typing import Optional
from abc import ABC, abstractmethod
import struct

"""
class Op(Enum):
  MOV =  0b00000;  BRA = 0b00001
  # Scalar ops (s16)
  # Scalar ALU (0b01...)
  SADD = 0b01000; SMUL = 0b01001; SNEG = 0b01010; SDIV = 0b01011
  SAND = 0b01100; SOR  = 0b01101; SXOR = 0b01110; SNOT = 0b01111

  # Vector ops
  SPCL = 0b00011
  
  VADD = 0b11000; VMUL = 0b11001; VFMA = 0b11010; VNEG = 0b11011

"""

class Argument(ABC):
  @staticmethod
  def decode(s: str) -> Argument:
    if s.startswith('s'): return ScalarRegister.decode(s)
    if s.startswith('v'): return VectorRegister.decode(s)
    if s.startswith('['): return Dereference.decode(s)
    if s.isdigit(): return Constant(int(s))
    raise ValueError(f"Invalid argument: {s}")

class Register(ABC):
  pass

@dataclass
class ScalarRegister(Register):
  num: int
  def __repr__(self): return f"s{self.num}"
  
  @staticmethod
  def decode(s: str) -> ScalarRegister:
    assert s.startswith('s'), f"Invalid scalar register: {s}"
    return ScalarRegister(int(s[1:]))

SZERO = ScalarRegister(0)

@dataclass
class VectorRegister(Register):
  num: int

  @staticmethod
  def decode(s: str) -> ScalarRegister:
    assert s.startswith('v'), f"Invalid vector register: {s}"
    return VectorRegister(int(s[1:]))

@dataclass
class Dereference:
  register: ScalarRegister
  offset: Constant
  def __repr__(self): return f"[{self.register} + {self.offset}]"
  @staticmethod
  def decode(s) -> Dereference:
    assert s.startswith('[') and s.endswith(']'), f"Invalid dereference: {s}"
    if "+" in (s := s[1:-1]): return Dereference(ScalarRegister.decode(s.split('+')[0]), int(s.split('+')[1]))
    elif "-" in s: return Dereference(ScalarRegister.decode(s.split('-')[0]), -int(s.split('-')[1]))
    elif s.startswith('s'): return Dereference(ScalarRegister.decode(s), Constant(0))
    return Dereference(SZERO, Constant.decode(s))

@dataclass
class Constant:
  val:int
  def __repr__(self): return f"{self.val}"
  @staticmethod
  def decode(s: str) -> Constant:
    if s.startswith('0x'): return Constant(int(s, 16))
    if s.startswith('0b'): return Constant(int(s, 2))
    return Constant(int(s))

class Instruction(ABC):
  @staticmethod
  def decode(inp:str) -> Instruction:
    match inp.split(" ", 1):
      case ["mov", tail]: return Mov.decode(tail)
      case ["sadd", tail]: return ScalarAdd.decode(tail)

class Mov(Instruction):
  @staticmethod
  def decode(tail: str) -> Mov:
    args = tail.split(", ")
    assert len(args) == 2, "Invalid MOV instruction"
    if args[0].startswith('s') or args[1].startswith('s'): return ScalarMov.decode(*args)
    if args[0].startswith('v') or args[1].startswith('v'): return VectorMov.decode(*args)
    raise ValueError("Invalid MOV instruction")

WIDE = 0b100
IMM  = 0b010 # is source an immediate?
LOAD = 0b001 # move into a register or out of register 

@dataclass
class ScalarMov(Instruction):
  """
  Possible instructions:
    - mov s1, s2        0b000 0x01 0x02 DC
    - mov s1, [s2 + 4]  0b001 0x01 0x02 0x04
    - mov [s1 + 4], s2  0b010 0x01 0x02 0x04
    - mov s1, 4         0b011 0x01 0x0004
  """
  dst: ScalarRegister | Dereference
  src: ScalarRegister | Dereference | Constant
  def __repr__(self): return f"mov {self.dst}, {self.src}"
  @staticmethod
  def decode(dst, src) -> ScalarMov: return ScalarMov(Argument.decode(dst), Argument.decode(src))
  def encode(self) -> bytes:
    match self.dst, self.src:
      case ScalarRegister(), ScalarRegister(): return struct.pack("BBBB", 0b00000000, self.dst.num, self.src.num, 0x00)
      case ScalarRegister(), Dereference(): return struct.pack("BBBB", 0b00000001, self.dst.num, self.src.register.num, self.src.offset.val)
      case Dereference(), ScalarRegister(): return struct.pack("BBBB", 0b00000010, self.dst.register.num, self.src.num, self.dst.offset.val)
      case ScalarRegister(), Constant(): return struct.pack("BBBB", 0b00000011, self.dst.num, self.src.val, 0x00)
      case _, _: raise ValueError(f"Invalid MOV instruction: {self.dst}, {self.src}")

@dataclass
class ScalarAdd(Instruction):
  """
  add s1, s2, s3        0b00001 0b000 0x01 0x02 0x03
  """
  dst: ScalarRegister
  src1: ScalarRegister
  src2: ScalarRegister
  def __repr__(self): return f"add {self.dst}, {self.src1}, {self.src2}"
  @staticmethod
  def decode(dst, src1, src2) -> ScalarAdd: return ScalarAdd(ScalarRegister.decode(dst), ScalarRegister.decode(src1), ScalarRegister.decode(src2))
  def encode(self) -> bytes: return struct.pack("BBBB", 0b00001000, self.dst.num, self.src1.num, self.src2.num)

@dataclass
class ScalarMultiply(Instruction):
  """
  mul s1, s2, s3        0b00001 0b001 0x01 0x02 0x03
  """
  dst: ScalarRegister
  src1: ScalarRegister
  src2: ScalarRegister
  def __repr__(self): return f"mul {self.dst}, {self.src1}, {self.src2}"
  @staticmethod
  def decode(dst, src1, src2) -> ScalarMultiply: return ScalarMultiply(ScalarRegister.decode(dst), ScalarRegister.decode(src1), ScalarRegister.decode(src2))
  def encode(self) -> bytes: return struct.pack("BBBB", 0b00001001, self.dst.num, self.src1.num, self.src2.num)

@dataclass
class ScalarNegation(Instruction):
  """
  neg s1, s2        0b00001 0b010 0x01 0x02 DC
  """
  dst: ScalarRegister
  src: ScalarRegister
  def __repr__(self): return f"neg {self.dst}, {self.src1}"
  @staticmethod
  def decode(dst, src) -> ScalarNegation: return ScalarNegation(ScalarRegister.decode(dst), ScalarRegister.decode(src))
  def encode(self) -> bytes: return struct.pack("BBBB", 0b00001010, self.dst.num, self.src.num, 0x00)

@dataclass
class ScalarDivision(Instruction):
  """
  s1 = s2 / s3
  div s1, s2, s3        0b00001 0b011 0x01 0x02 0x03
  """
  dst: ScalarRegister
  src1: ScalarRegister
  src2: ScalarRegister
  def __repr__(self): return f"div {self.dst}, {self.src1}, {self.src2}"
  def __repr__(self): return f"div {self.dst}, {self.src1}, {self.src2}"
  @staticmethod
  def decode(dst, src1, src2) -> ScalarDivision: return ScalarDivision(ScalarRegister.decode(dst), ScalarRegister.decode(src1), ScalarRegister.decode(src2)) 
  def encode(self) -> bytes: return struct.pack("BBBB", 0b00001011, self.dst.num, self.src1.num, self.src2.num)


@dataclass
class ScalarAnd(Instruction):
  """
  and s1, s2, s3        0b00001 0b100 0x01 0x02 0x03
  """
  dst: ScalarRegister
  src1: ScalarRegister
  src2: ScalarRegister
  def __repr__(self): return f"and {self.dst}, {self.src1}, {self.src2}"
  @staticmethod
  def decode(dst, src1, src2) -> ScalarAnd: return ScalarAnd(ScalarRegister.decode(dst), ScalarRegister.decode(src1), ScalarRegister.decode(src2))
  def encode(self) -> bytes: return struct.pack("BBBB", 0b00001100, self.dst.num, self.src1.num, self.src2.num)

@dataclass
class ScalarOr(Instruction):
  """
  or s1, s2, s3        0b00001 0b101 0x01 0x02 0x03
  """
  dst: ScalarRegister
  src1: ScalarRegister
  src2: ScalarRegister
  def __repr__(self): return f"or {self.dst}, {self.src1}, {self.src2}"
  @staticmethod
  def decode(dst, src1, src2) -> ScalarOr: return ScalarOr(ScalarRegister.decode(dst), ScalarRegister.decode(src1), ScalarRegister.decode(src2))
  def encode(self) -> bytes: return struct.pack("BBBB", 0b00001101, self.dst.num, self.src1.num, self.src2.num)

@dataclass
class ScalarXor(Instruction):
  """
  xor s1, s2, s3        0b00001 0b110 0x01 0x02 0x03
  """
  def __repr__(self): return f"xor {self.dst}, {self.src1}, {self.src2}"
  @staticmethod
  def decode(dst, src1, src2) -> ScalarXor: return ScalarXor(ScalarRegister.decode(dst), ScalarRegister.decode(src1), ScalarRegister.decode(src2))
  def encode(self) -> bytes: return struct.pack("BBBB", 0b00001110, self.dst.num, self.src1.num, self.src2.num)

@dataclass
class ScalarNot(Instruction):
  """
  not s1, s2        0b00001 0b111 0x01 0x02 DC
  """
  dst: ScalarRegister
  src: ScalarRegister
  def __repr__(self): return f"not {self.dst}, {self.src1}"
  @staticmethod
  def decode(dst, src) -> ScalarNot: return ScalarNot(ScalarRegister.decode(dst), ScalarRegister.decode(src))
  def encode(self) -> bytes: return struct.pack("BBBB", 0b00001111, self.dst.num, self.src.num, 0x00)

###########################
### VECTOR INSTRUCTIONS ###
###########################

@dataclass
class VectorMov(Instruction):
  """
  types of vector move:
    - mov v1, v2          0b100 0x01 0x02 0x00 -- move without permutation
    - mov v1, v2, perm    0b100 0x01 0x02 perm -- move with permutation
    - mov v1, [r2 + 4]    0b101 0x01 0x02 0x04
    - mov [r1 + 4], v2    0b110 0x01 0x02 0x04
    - mov v1, s2          0b111 0x01 0x02 0xFF -- load to every lane
    - mov v1, s2, mask    0b111 0x01 0x02 MASK -- load only masked lanes
  """
  dst: VectorRegister | Dereference
  src: VectorRegister | Dereference | ScalarRegister
  extra: Optional[Constant]
  def __repr__(self): return f"mov {self.dst}, {self.src}" if self.extra is None else f"mov {self.dst}, {self.src}, {self.extra}"
  @staticmethod
  def decode(dst, src) -> VectorMov:
    return VectorMov(Argument.decode(dst), Argument.decode(src))
  def encode(self) -> bytes:
    match self.dst, self.src, self.extra:
      case VectorRegister(), VectorRegister(), None: return struct.pack("BBBB", 0b00000100, self.dst.num, self.src.num, 0x00) # SHUFFLER (placeholder)
      case VectorRegister(), VectorRegister(), Constant(x): return struct.pack("BBBB", 0b00000100, self.dst.num, self.src.num, 0x00) # SHUFFLER (placeholder)
      case VectorRegister(), Dereference(): return struct.pack("BBBB", 0x00000101, self.dst.num, self.src.register.num, self.src.offset.val)
      case Dereference(), VectorRegister(): return struct.pack("BBBB", 0x00000110, self.dst.register.num, self.src.num, self.dst.offset.val)
      case VectorRegister(), ScalarRegister(): return struct.pack("BBBB", 0b00000111, self.dst.num, self.src.num, 0x00) # MASKLER (placeholder)

class VectorAdd(Instruction):
  """
    - vadd v3, v1, v2 0b11000 0x03 0x01 0x02
  
  """
  def __init__(self, dst, src1:VectorRegister, src2:VectorRegister):
    self.dst = dst
    self.src1 = src1
    self.src2 = src2
  def __repr__(self): return f"VADD {self.dst}, {self.src1}, {self.src2}"
  @staticmethod
  def decode(dst, src1, src2) -> VectorAdd:
    return VectorAdd()
  def encode(self) -> bytes:
    match self.dst, self.src:
      case VectorRegister(), VectorRegister():
        return struct.pack("BBBB", 0b00000100, self.dst.num, self.src.num)

@dataclass
class VectorMultiply(Instruction):
  def __init__(self, dst, src1:VectorRegister, src2:VectorRegister):
    self.dst = dst
    self.src1 = src1
    self.src2 = src2
  def __repr__(self): return f"VMUL {self.dst}, {self.src1}, {self.src2}"
  @staticmethod
  def decode(dst, src1, src2) -> VectorMultiply:
    return VectorMultiply()

@dataclass
class VectorNegation(Instruction):
  def __init__(self, dst, src1:ScalarRegister):
    self.dst = dst
    self.src1 = src1
    def __repr__(self): return f"VNEG {self.dst}, {self.src1}"
  @staticmethod
  def decode(dst, src1) -> VectorNegation:
    return VectorNegation()

@dataclass
class VectorDivision(Instruction):
  def __init__(self, dst, src1:VectorRegister, src2:VectorRegister):
    self.dst = dst
    self.src1 = src1
    self.src2 = src2
  def __repr__(self): return f"VDIV {self.dst}, {self.src1}, {self.src2}"
  @staticmethod
  def decode(dst, src1, src2) -> VectorDivision:
    return VectorDivision()

@dataclass
class VectorAnd(Instruction):
  def __init__(self, dst, src1:VectorRegister, src2:VectorRegister):
    self.dst = dst
    self.src1 = src1
    self.src2 = src2
  def __repr__(self): return f"VAND {self.dst}, {self.src1}, {self.src2}"
  @staticmethod
  def decode(dst, src1, src2) -> VectorAnd:
    return VectorAnd()

@dataclass
class VectorOr(Instruction):
  def __init__(self, dst, src1:VectorRegister, src2:VectorRegister):
    self.dst = dst
    self.src1 = src1
    self.src2 = src2
  def __repr__(self): return f"VOR {self.dst}, {self.src1}, {self.src2}"
  @staticmethod
  def decode(dst, src1, src2) -> VectorOr:
    return VectorOr()

@dataclass
class VectorXor(Instruction):
  def __init__(self, dst, src1:VectorRegister, src2:VectorRegister):
    self.dst = dst
    self.src1 = src1
    self.src2 = src2
  def __repr__(self): return f"VXOR {self.dst}, {self.src1}, {self.src2}"
  @staticmethod
  def decode(dst, src1, src2) -> VectorXor:
    return VectorXor()

@dataclass
class VectorNot(Instruction):
  def __init__(self, dst, src1:VectorRegister):
    self.dst = dst
    self.src1 = src1
  def __repr__(self): return f"VADD {self.dst}, {self.src1}"
  @staticmethod
  def decode(dst, src1, src2) -> VectorNot:
    return VectorNot()
@dataclass
class Program:
  statements: list[Statement]
  constants: list[Constant]

  @staticmethod
  def build(src:str) -> Program:
    prog = Program([], [])
    i, labels = 0, {}
    for line in src.splitlines():
      if ';' in line: line = line.split(';')[0]
      line = line.strip()
      if ':' in line:
        labels[line.split(':')[0]] = i
        line = line.split(':')[1]
      if line != '':
        args = line.split()
        if args[0] in Op.__members__:
          match op:=Op[args[0]]:
            case Op.STOR: prog.statements.append(Statement(op, (Address(args[1]), parse_arg(args[2]), None)))
            case Op.LOAD: prog.statements.append(Statement(op, (parse_arg(args[1]), Address(args[2]), None)))
            case Op.CJMP: prog.statements.append(Statement(op, (Address(args[1]), None, None)))
            case _: prog.statements.append(Statement(op, (parse_arg(args[1]), parse_arg(args[2]), parse_arg(args[3]))))
        elif args[0] in Special.__members__:
          prog.statements.append(Statement(Op.SPCL, (Special[args[0]], parse_arg(args[1]), parse_arg(args[2]))))
        else: raise ValueError(f"Unknown operation: {args[0]}")
    for s in prog.statements:
      if isinstance(s.args[0], Address) and s.args[0].val == -1: s.args[0].val = labels[s.args[0].label]
      if isinstance(s.args[1], Address) and s.args[1].val == -1: s.args[1].val = labels[s.args[1].label]
    return prog
  
def parse_arg(arg: str) -> Register|Constant: return Register(int(arg[1:])) if arg.startswith('r') else Constant(int(arg))
