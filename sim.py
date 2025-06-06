import struct, sys, warnings, fcntl, os, time
import numpy as np
from os import getenv

warnings.filterwarnings("ignore")
fcntl.fcntl(0, fcntl.F_SETFL, fcntl.fcntl(0, fcntl.F_GETFL) | os.O_NONBLOCK)

DEBUG = int(getenv("DEBUG", "0")) != 0
TRACE_FLAGS = int(getenv("TRACE_FLAGS", "0")) != 0

def as_np(arr: bytearray) -> np.ndarray: return np.frombuffer(arr, dtype=np.int16)
def from_np(arr: np.ndarray) -> bytearray: return arr.tobytes()
def pack2(arg1: int, arg2: int) -> int: return arg1 << 8 | arg2
def apply_func(func, *args) -> bytearray: return from_np(func(*[as_np(arg) for arg in args]))

def vadd(a: np.ndarray, b: np.ndarray) -> np.ndarray: return (a + b).astype(np.int16)
def vsub(a: np.ndarray, b: np.ndarray) -> np.ndarray: return (a - b).astype(np.int16)
def vmul(a: np.ndarray, b: np.ndarray) -> np.ndarray:
  tmp = (a.astype(np.int16) * b.astype(np.int16)) >> FRAC_BITS
  return np.clip(tmp, -32768, 32767).astype(np.int16)
def vneg(a: np.ndarray) -> np.ndarray: return (-a).astype(np.int16)
def vdiv(a: np.ndarray, b: np.ndarray) -> np.ndarray:
  tmp = (a.astype(np.int16) << FRAC_BITS) // b.astype(np.int16)
  return np.clip(tmp, -32768, 32767).astype(np.int16)

SF = 0b00000001
ZF = 0b00000010

FRAC_BITS = 15


def get_flags(val: int) -> int:
  val &= 0xFFFF
  zf = ZF if val == 0 else 0
  sf = SF if val & 0x8000 else 0
  return zf | sf

if __name__ == "__main__":
  if len(sys.argv) != 2:
    print("Usage: python sim.py <input_file>")
    sys.exit(1)

  with open(sys.argv[1], 'rb') as f: data = f.read()

  pc = 0
  memory = bytearray(2 ** 16)
  memory[0:len(data)] = data
  sregs = np.zeros((256,), np.uint16)
  vregs = [bytearray(16) for _ in range(256)]
  flags = 0
  tracing = [3]
  trace_vals = {}
  prev_flags = 0

  while True:
    try:
      opcode, arg1, arg2, arg3 = struct.unpack("BBBB", data[pc:pc + 4])
      op = bin(opcode)[2:].zfill(8)
      trace_vals = {k:sregs[k] for k in tracing}
      match op[:5], op[5:]:
        case "00000", "000": sregs[arg1] = sregs[arg2]
        case "00000", "001": sregs[arg1] = memory[sregs[arg2] + arg3] << 8 | memory[sregs[arg2] + arg3 + 1]
        case "00000", "010":
          memory[sregs[arg1] + arg2] = sregs[arg3] >> 8
          memory[sregs[arg1] + arg2 + 1] = sregs[arg3] & 0xFF
        case "00000", "011": sregs[arg1] = arg2 << 8 | arg3
        case "00000", "100": vregs[arg1][:] = vregs[arg2]
        case "00000", "101":
          for i in range(16): vregs[arg1][i] = memory[sregs[arg2] + arg3 + i]
        case "00000", "110":
          for i in range(16): memory[sregs[arg1] + arg2 + i] = vregs[arg3][i]
        case "00000", "111": as_np(vregs[arg1])[[(arg3 >> i) & 1 for i in range(8)]] = np.int16(sregs[arg2])
        case "00001", "000": sregs[arg1] = sregs[arg2] + sregs[arg3]
        case "00001", "001": sregs[arg1] = sregs[arg2] * sregs[arg3]
        case "00001", "010": sregs[arg1] = -sregs[arg2]
        case "00001", "011": sregs[arg1] = sregs[arg2] // sregs[arg3] # Need to set a flag for div by zero.
        case "00001", "100": sregs[arg1] = sregs[arg2] & sregs[arg3]
        case "00001", "101": sregs[arg1] = sregs[arg2] | sregs[arg3]
        case "00001", "110": sregs[arg1] = sregs[arg2] ^ sregs[arg3]
        case "00001", "111": sregs[arg1] = ~sregs[arg2]
        case "00010", "000": vregs[arg1] = apply_func(vadd, vregs[arg2], vregs[arg3])
        case "00010", "001": vregs[arg1] = apply_func(vmul, vregs[arg2], vregs[arg3])
        case "00010", "010": vregs[arg1] = apply_func(vneg, vregs[arg2])
        case "00010", "011": vregs[arg1] = apply_func(vdiv, vregs[arg2], vregs[arg3]) # Need to set a flag for div by zero.
        case "00010", "100": vregs[arg1] = apply_func(np.bitwise_and, vregs[arg2], vregs[arg3])
        case "00010", "101": vregs[arg1] = apply_func(np.bitwise_or, vregs[arg2], vregs[arg3])
        case "00010", "110": vregs[arg1] = apply_func(np.bitwise_xor, vregs[arg2], vregs[arg3])
        case "00010", "111": vregs[arg1] = apply_func(np.bitwise_not, vregs[arg2])
        case "00011", "000": vregs[arg1] = apply_func(np.equal, vregs[arg2], vregs[arg3])
        case "00011", "001": vregs[arg1] = apply_func(np.greater, vregs[arg2], vregs[arg3])
        case "00100", "000": # J (unconditional)
          pc = pack2(arg1, arg2)
          continue
        case "00100", "001": # JE
          if flags & ZF:
            pc = pack2(arg1, arg2)
            continue
        case "00100", "010": # JNE
          if not (flags & ZF):
            pc = pack2(arg1, arg2)
            continue
        case "00100", "011": # JGE
          if (flags & SF) == 0 or (flags & ZF):
            pc = pack2(arg1, arg2)
            continue
        case "00100", "100": # JLE
          if (flags & SF) or (flags & ZF):
            pc = pack2(arg1, arg2)
            continue
        case "00100", "101": # JGT
          if (flags & SF) == 0 and not (flags & ZF):
            pc = pack2(arg1, arg2)
            continue
        case "00100", "110": # JLT
          if flags & SF:
            pc = pack2(arg1, arg2)
            continue
        case "00100", "111": # JR
          pc = sregs[arg1]
          continue
        case "00101", "000": flags = get_flags(sregs[arg1] - sregs[arg2])
        case "00101", "001": flags = get_flags(sregs[arg1] - pack2(arg2, arg3))
        case "00101", "010": flags = get_flags(pack2(arg1, arg2) - sregs[arg3])
        case "00101", "011": flags = arg1 & (SF | ZF)
        case "00110", "000":
          if (inp := sys.stdin.read(1)) != '':
            flags = 0
            sregs[arg1] = sregs[arg1] & 0xff00 | ord(inp)
          else: flags = ZF
        case "00110", "001":
          if (inp := sys.stdin.read(1)) != '':
            flags = 0
            sregs[arg1] = sregs[arg1] & 0xff | (ord(inp) << 8)
          else: flags = ZF
        case "00110", "010":
          if DEBUG: print(f"OUTPUT: 0x{sregs[arg1] & 0xFF:X}")
          else:
            sys.stdout.buffer.write(chr(sregs[arg1] & 0xFF).encode())
            sys.stdout.flush()
        case "00110", "011":
          if DEBUG: print(f"OUTPUT: 0x{sregs[arg1] >> 8:X}")
          else:
            sys.stdout.buffer.write(chr(sregs[arg1] >> 8).encode())
            sys.stdout.flush()
      pc += 4
      for k in tracing:
        if DEBUG and sregs[k] != trace_vals[k]: print(f"TRACE s{k}: 0x{trace_vals[k]:X} -> 0x{sregs[k]:X}")
      if TRACE_FLAGS:
        if flags != prev_flags: print(f"TRACE flags: {'SF ' if prev_flags & SF else ''}{'ZF' if prev_flags & ZF else ''} -> {'SF ' if flags & SF else ''}{'ZF' if flags & ZF else ''}")
        prev_flags = flags
    except struct.error as e:
      print(f"PC: {pc:x}")
      print("Sregs:")
      for i in range(32):
        print(f"s{i:2}: 0x{sregs[i]:4X}")
      from hexdump import hexdump
      hexdump(memory[0xFF00:])
      exit(0)


