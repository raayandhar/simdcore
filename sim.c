#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>

#define ZF 0x01
#define SF 0x02
#define FLAG(x) (((x) == 0) ? ZF : 0) | ((x) & 0x8000 ? SF : 0)

union instruction {
  struct {
    uint8_t opcode:5;
    uint8_t aluctrl:3;
    uint8_t A;
    uint8_t B;
    uint8_t C;
  } __attribute__((packed)) arg3;
  struct {
    uint8_t opcode:5;
    uint8_t aluctrl:3;
    uint8_t A;
    uint16_t imm16;
  } __attribute__((packed)) arg2;
  struct {
    uint8_t opcode:5;
    uint8_t aluctrl:3;
    uint16_t imm16;
    uint8_t _pad;
  } __attribute__((packed)) arg1;
  uint32_t raw;
};

uint8_t flags = 0;
uint8_t memory[65536] = {0};
uint16_t sregs[256] = {0};

uint8_t in() {
  uint8_t in;
  if (read(0, &in, 1) != 1) {
    if (errno != EAGAIN || errno != EWOULDBLOCK) {
      perror("read");
      exit(1);
    }
    flags = ZF;
    return 0;
  }
  return in;
}

void out(uint8_t out) {
  if (write(1, &out, 1) != 1) {
    perror("write");
    exit(1);
  }
}

void execute() {
  uint16_t pc = 0;

  while (1) {
    union instruction inst;
    inst.raw = *(uint32_t *)(memory + pc);

    switch(inst.arg3.opcode) {
      case 0x00: // MOV
        switch (inst.arg3.aluctrl) {
          case 0: sregs[inst.arg3.A] = sregs[inst.arg3.B]; break;
          case 1: sregs[inst.arg3.A] = memory[sregs[inst.arg3.B] + inst.arg3.C]; break;
          case 2: memory[sregs[inst.arg3.A] + inst.arg3.B] = sregs[inst.arg3.C]; break;
          case 3: sregs[inst.arg2.A] = inst.arg2.imm16; break;
          case 4:
          case 5:
          case 6:
          case 7: fprintf(stderr, "vector registers unimplemented\n"); exit(1);
        } break;
      case 0x01: // SCALAR ALU
        switch (inst.arg3.aluctrl) {
          case 0: sregs[inst.arg3.A] = sregs[inst.arg3.B] + sregs[inst.arg3.C]; break;
          case 1: sregs[inst.arg3.A] = sregs[inst.arg3.B] * sregs[inst.arg3.C]; break;
          case 2: sregs[inst.arg3.A] = -sregs[inst.arg3.B]; break;
          case 3: sregs[inst.arg3.A] = sregs[inst.arg3.B] / sregs[inst.arg3.C]; break;
          case 4: sregs[inst.arg3.A] = sregs[inst.arg3.B] & sregs[inst.arg3.C]; break;
          case 5: sregs[inst.arg3.A] = sregs[inst.arg3.B] | sregs[inst.arg3.C]; break;
          case 6: sregs[inst.arg3.A] = sregs[inst.arg3.B] ^ sregs[inst.arg3.C]; break;
          case 7: sregs[inst.arg3.A] = ~sregs[inst.arg3.B]; break;
        } break;
      case 0x02: // VECTOR ALU
      case 0x03: // VECTOR COMPARE
        fprintf(stderr, "vector ALU unimplemented\n"); exit(1);
      case 0x04: // JUMP
        switch (inst.arg3.aluctrl) {
          case 0: pc = inst.arg1.imm16; continue;
          case 1: if (flags & ZF) pc = inst.arg1.imm16; continue;
          case 2: if (!(flags & ZF)) pc = inst.arg1.imm16; continue;
          case 3: if (!(flags & SF) || flags & ZF) pc = inst.arg1.imm16; continue;
          case 4: if (flags & SF || flags & ZF) pc = inst.arg1.imm16; continue;
          case 5: if (!(flags & SF) && !(flags & ZF)) pc = inst.arg1.imm16; continue;
          case 6: if (flags & SF) pc = inst.arg1.imm16; continue;
          case 7: pc = sregs[inst.arg3.A]; continue;
        } break;
      case 0x05: // TEST
        switch (inst.arg3.aluctrl) {
          case 0: flags = FLAG(sregs[inst.arg3.A] - sregs[inst.arg3.B]); break;
          case 1: flags = FLAG(sregs[inst.arg2.A] - inst.arg2.imm16); break;
          case 2: fprintf(stderr, "test_ir unimplemented\n"); exit(1);
          case 3: flags = inst.arg1.imm16 & (ZF | SF);
          default: fprintf(stderr, "unknown test instruction\n"); exit(1);
        } break;
      case 0x06: // IO
        switch (inst.arg3.aluctrl) {
          case 0: sregs[inst.arg3.A] = sregs[inst.arg3.A] & 0xFF00 | in(); break;
          case 1: sregs[inst.arg3.A] = sregs[inst.arg3.A] & 0x00FF | (in() << 8); break;
          case 2: out(sregs[inst.arg3.A] & 0xFF); break;
          case 3: out(sregs[inst.arg3.A] >> 8); break;
          default: fprintf(stderr, "unknown IO instruction\n"); exit(1);
        } break;
      case 0x07: return;
      default: fprintf(stderr, "unknown opcode 0x%02x\n", inst.arg3.opcode); exit(1);
    }

    pc += 4;
  }
}

void main(int argc, char **argv) {
  int fd;
  struct stat sb;

  if (argc < 2) {
    fprintf(stderr, "usage: %s <input_file>\n", argv[0]);
    exit(1);
  }

  if (open(argv[1], O_RDONLY) == -1) {
    perror("open");
    exit(1);
  }

  if (fstat(fd, &sb) == -1) {
    perror("fstat");
    exit(1);
  }

  if (mmap(memory, sb.st_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0) != memory) {
    perror("mmap");
    exit(1);
  }

  // add HLT (0x07) instruction at the end of the memory
  memory[sb.st_size] = 0x07; // HLT instruction

  execute();
}


