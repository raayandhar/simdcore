from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from typing import Optional

# STORE: [16 bit address] register
# LOAD: register [16 bit address]
# CJMP: [16 bit address]
# SPCL: special opcode register register|constant
# otherwise: register register|constant register|constant

# NOTE: INEG HNEG, BNOT can be SPCL instead

class Op(Enum):
  STOR = 0b00000; LOAD = 0b00001; CJMP = 0b00010; SPCL = 0b00011
  IADD = 0b01000; IMUL = 0b01001; IFMA = 0b01010; INEG = 0b01011; IDIV = 0b01100
  BAND = 0b10000; BSFT = 0b10001; BXOR = 0b10010; BOR  = 0b10011; BNOT = 0b10100
  HADD = 0b11000; HMUL = 0b11001; HFMA = 0b11010; HNEG = 0b11011

@dataclass
class Register:
  val: int
  def __repr__(self): return f"r{self.val}"

@dataclass
class Constant:
  val: int
  def __repr__(self): return str(self.val)

@dataclass
class Address:
  val: int
  label: Optional[str]
  def __repr__(self): return f"[0x{self.val:X}]"
  def __init__(self, addr: str):
    if addr[0].isalpha(): self.label = addr; self.val = -1
    else:
      self.label = None
      self.val = int(addr, 16) if addr.startswith('0x') else int(addr)

class Special(Enum):
  RECIP = 0x00; LOG2 = 0x01; EXP2 = 0x02; SQRT = 0x03; ICAST = 0x04; HCAST = 0x05; CMPLE = 0x06; CMPNE = 0x07

Arg1Type = Register | Special | Address
Arg2Type = Register | Constant | Address | None
Arg3Type = Register | Constant | None

@dataclass
class Statement:
  op: Op
  args: tuple[Arg1Type, Arg2Type, Arg3Type]
  def __repr__(self): return f"{self.op.name.lower()} {self.args[0]}, {self.args[1]}, {self.args[2]}"

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
