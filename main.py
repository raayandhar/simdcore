from asm import Program
from sim import Sim
import argparse

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="Simulate a program.")
  parser.add_argument("file", type=str, help="The file to simulate.")
  parser.add_argument("--breakpoints", type=int, nargs="*", default=[], help="The breakpoints to set.")
  args = parser.parse_args()
  with open(args.file, "r") as f:
    src = f.read()
  prog = Program.build(src)
  s = Sim(prog)
  s.execute(set(args.breakpoints))