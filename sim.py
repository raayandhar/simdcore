from asm import Op, Program, Special
import numpy as np
 
def as_np(arr: bytearray, dtype:np.dtype) -> np.ndarray: return np.frombuffer(arr, dtype=dtype)
def from_np(arr: np.ndarray) -> bytearray: return arr.tobytes()
def apply_func(func, dtype:np.dtype, *args) -> bytearray: return from_np(func(*[as_np(arg, dtype) for arg in args]))

def fma(a: np.ndarray, b: np.ndarray, c: np.ndarray) -> np.ndarray: return np.add(np.multiply(a, b), c)

class Sim:
  def __init__(self, prog: Program, constants, n_regs: int=16, n_mem: int=1024):
    self.prog = prog
    self.regs = [bytearray(16) for _ in range(n_regs)]
    self.mem = [bytearray(16) for _ in range(n_mem)]
    self.constants = constants

  def print(self):
    print("Regs:")
    for i, r in enumerate(self.regs):
      print(f"r{i:2}: {as_np(r, np.int16)} {as_np(r, np.half)}")

  def execute(self, breakpoints: set[int]=set()):
    pc = 0
    while pc < len(self.prog.statements):
      print(f"PC: {pc}")
      if pc in breakpoints: self.print()
      match (s:=self.prog.statements[pc]).op:
        case Op.STOR: self.mem[s.args[0].val] = self.regs[s.args[1].val]
        case Op.LOAD: self.regs[s.args[0].val] = self.mem[s.args[1].val] 
        case Op.CJMP: pc = s.args[0].addr
        case Op.SPCL:
          match s.args[0]:
            case Special.RECIP: self.regs[s.args[1].val] = apply_func(np.reciprocal, np.half, self.regs[s.args[1].val])
            case Special.LOG2:  self.regs[s.args[1].val] = apply_func(np.log2, np.half, self.regs[s.args[1].val])
            case Special.EXP2:  self.regs[s.args[1].val] = apply_func(np.exp2, np.half, self.regs[s.args[1].val])
            case Special.SQRT:  self.regs[s.args[1].val] = apply_func(np.sqrt, np.half, self.regs[s.args[1].val])
            case Special.ICAST: self.regs[s.args[1].val] = apply_func(np.int16, np.half, self.regs[s.args[1].val])
            case Special.HCAST: self.regs[s.args[1].val] = apply_func(np.half, np.int16, self.regs[s.args[1].val])
            case Special.CMPLE: raise NotImplementedError("CMPLE not implemented")
            case Special.CMPNE: raise NotImplementedError("CMPNE not implemented")
        case Op.IADD: self.regs[s.args[0].val] = apply_func(np.add, np.int16, self.regs[s.args[1].val], self.regs[s.args[2].val])
        case Op.IMUL: self.regs[s.args[0].val] = apply_func(np.multiply, np.int16, self.regs[s.args[1].val], self.regs[s.args[2].val])
        case Op.IFMA: self.regs[s.args[0].val] = apply_func(fma, np.int16, self.regs[s.args[1].val], self.regs[s.args[2].val], self.regs[s.args[0].val]) 
        case Op.INEG: self.regs[s.args[0].val] = apply_func(np.negative, np.int16, self.regs[s.args[1].val])
        case Op.IDIV: self.regs[s.args[0].val] = apply_func(np.divide, np.int16, self.regs[s.args[1].val], self.regs[s.args[2].val])
        case Op.BAND: self.regs[s.args[0].val] = apply_func(np.bitwise_and, np.int16, self.regs[s.args[1].val], self.regs[s.args[2].val])
        case Op.BSFT: raise NotImplementedError("BSFT not implemented")
        case Op.BXOR: self.regs[s.args[0].val] = apply_func(np.bitwise_xor, np.int16, self.regs[s.args[1].val], self.regs[s.args[2].val])
        case Op.BOR:  self.regs[s.args[0].val] = apply_func(np.bitwise_or, np.int16, self.regs[s.args[1].val], self.regs[s.args[2].val])
        case Op.BNOT: self.regs[s.args[0].val] = apply_func(np.bitwise_not, np.int16, self.regs[s.args[1].val])
        case Op.HADD: self.regs[s.args[0].val] = apply_func(np.add, np.half, self.regs[s.args[1].val], self.regs[s.args[2].val])
        case Op.HMUL: self.regs[s.args[0].val] = apply_func(np.multiply, np.half, self.regs[s.args[1].val], self.regs[s.args[2].val])
        case Op.HFMA: self.regs[s.args[0].val] = apply_func(fma, np.half, self.regs[s.args[1].val], self.regs[s.args[2].val], self.regs[s.args[0].val])
        case Op.HNEG: self.regs[s.args[0].val] = apply_func(np.negative, np.half, self.regs[s.args[1].val])
        case _: raise NotImplementedError(f"Operation {s.op.name} not implemented")
      pc += 1
