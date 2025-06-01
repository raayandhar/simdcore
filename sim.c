#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <string.h>

#define ZF 0x01
#define SF 0x02
#define FLAG(x) (((x) == 0) ? ZF : 0) | ((x) & 0x8000 ? SF : 0)

struct __attribute__((packed)) instruction {
  uint8_t aluctrl:3;
  uint8_t opcode:5;
  uint8_t A;
  uint8_t B;
  uint8_t C;
};

__attribute__((aligned(4096))) uint8_t memory[65536] = {0};
uint8_t flags = 0;
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
  fprintf(stderr, "Starting execution...\n");
  uint16_t pc = 0;

  while (1) {
    struct instruction inst = *((struct instruction *)(&memory[pc]));

    switch(inst.opcode) {
      case 0x00: // MOV
        switch (inst.aluctrl) {
          case 0: sregs[inst.A] = sregs[inst.B]; break;
          case 1: sregs[inst.A] = memory[sregs[inst.B] + inst.C]; break;
          case 2: memory[sregs[inst.A] + inst.B] = sregs[inst.C]; break;
          case 3: sregs[inst.A] = inst.B << 8 | inst.C; break;
          case 4:
          case 5:
          case 6:
          case 7: fprintf(stderr, "vector registers unimplemented\n"); exit(1);
        } break;
      case 0x01: // SCALAR ALU
        switch (inst.aluctrl) {
          case 0: sregs[inst.A] = sregs[inst.B] + sregs[inst.C]; break;
          case 1: sregs[inst.A] = sregs[inst.B] * sregs[inst.C]; break;
          case 2: sregs[inst.A] = -sregs[inst.B]; break;
          case 3: sregs[inst.A] = sregs[inst.B] / sregs[inst.C]; break;
          case 4: sregs[inst.A] = sregs[inst.B] & sregs[inst.C]; break;
          case 5: sregs[inst.A] = sregs[inst.B] | sregs[inst.C]; break;
          case 6: sregs[inst.A] = sregs[inst.B] ^ sregs[inst.C]; break;
          case 7: sregs[inst.A] = ~sregs[inst.B]; break;
        } break;
      case 0x02: // VECTOR ALU
      case 0x03: // VECTOR COMPARE
        fprintf(stderr, "vector ALU unimplemented\n"); exit(1);
      case 0x04: // JUMP
        switch (inst.aluctrl) {
          case 0: pc = inst.A << 8 | inst.B; continue;
          case 1: if (flags & ZF) pc = inst.A << 8 | inst.B; continue;
          case 2: if (!(flags & ZF)) pc = inst.A << 8 | inst.B; continue;
          case 3: if (!(flags & SF) || flags & ZF) pc = inst.A << 8 | inst.B; continue;
          case 4: if (flags & SF || flags & ZF) pc = inst.A << 8 | inst.B; continue;
          case 5: if (!(flags & SF) && !(flags & ZF)) pc = inst.A << 8 | inst.B; continue;
          case 6: if (flags & SF) pc = inst.A << 8 | inst.B; continue;
          case 7: pc = sregs[inst.A]; continue;
        } break;
      case 0x05: // TEST
        switch (inst.aluctrl) {
          case 0: flags = FLAG(sregs[inst.A] - sregs[inst.B]); break;
          case 1: flags = FLAG(sregs[inst.A] - (inst.B << 8 | inst.C)); break;
          case 2: fprintf(stderr, "test_ir unimplemented\n"); exit(1);
          case 3: flags = inst.A & (ZF | SF);
          default: fprintf(stderr, "unknown test instruction\n"); exit(1);
        } break;
      case 0x06: // IO
        switch (inst.aluctrl) {
          case 0: sregs[inst.A] = (sregs[inst.A] & 0xFF00) | in(); break;
          case 1: sregs[inst.A] = (sregs[inst.A] & 0x00FF) | (in() << 8); break;
          case 2: out(sregs[inst.A] & 0xFF); break;
          case 3: out(sregs[inst.A] >> 8); break;
          default: fprintf(stderr, "unknown IO instruction\n"); exit(1);
        } break;
      case 0x07: return;
      default: fprintf(stderr, "unknown opcode 0x%02x\n", inst.opcode); exit(1);
    }
    pc += 4;
  }
}

int main(int argc, char **argv) {
  int fd;
  struct stat sb;

  if (fcntl(0, F_SETFL, O_NONBLOCK) == -1) {
    perror("fcntl");
    exit(1);
  }

  if (argc < 2) {
    fprintf(stderr, "usage: %s <input_file>\n", argv[0]);
    exit(1);
  }

  if ((fd = open(argv[1], O_RDONLY)) == -1) {
    perror("open");
    exit(1);
  }

  if (fstat(fd, &sb) == -1) {
    perror("fstat");
    exit(1);
  }

  if (read(fd, memory, sb.st_size) != sb.st_size) {
    perror("read");
    exit(1);
  }

  close(fd);

  // add HLT (0x07) instruction at the end of the memory
  memcpy(&memory[sb.st_size], &(struct instruction){0, 0x07, 0, 0, 0}, sizeof(struct instruction));

  execute();

  fprintf(stderr, "Execution finished.\n");

  return 0;
}


