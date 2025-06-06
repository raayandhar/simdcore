#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <string.h>
#include <signal.h>

#define ZF 0x01
#define SF 0x02
#define FLAG(x) (((x) == 0) ? ZF : 0) | ((x) & 0x8000 ? SF : 0)

struct instruction {
  uint8_t aluctrl:3;
  uint8_t opcode:5;
  uint8_t A;
  uint8_t B;
  uint8_t C;
};

typedef uint16_t vreg_t[8];

__attribute__((aligned(4096))) uint8_t memory[65536] = {0};
uint8_t flags = 0;
uint16_t pc = 0;
uint16_t sregs[256] = {0};
vreg_t vregs[256] = {0};

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
  flags = 0;
  return in;
}

void out(uint8_t out) {
  if (write(1, &out, 1) != 1) {
    perror("write");
    exit(1);
  }
}

void dump() {
  fprintf(stderr, "PC: %04x\n", pc);
  for (int i = 0; i < 16; i++) fprintf(stderr, "s%d: 0x%04x\n", i, sregs[i]);
  for (int i = 0; i < 4; i++)
    fprintf(stderr, "v%d: %04x %04x %04x %04x %04x %04x %04x %04x\n",
        i, vregs[i][0], vregs[i][1], vregs[i][2], vregs[i][3], vregs[i][4], vregs[i][5], vregs[i][6], vregs[i][7]);
  for (int i = 0x1000; i < 0x1040; i += 0x10)
    fprintf(stderr, "%04x: %02x%02x %02x%02x %02x%02x %02x%02x\n",
        i, memory[i], memory[i + 1], memory[i + 2], memory[i + 3], memory[i + 4], memory[i + 5], memory[i + 6], memory[i + 7]);

  exit(0);
}

void execute() {
  fprintf(stderr, "Starting execution...\n");

  while (1) {
    struct instruction inst = *((struct instruction *)(&memory[pc]));
    pc += 4;
    switch(inst.opcode) {
      case 0x00: // MOV
        switch (inst.aluctrl) {
          case 0: sregs[inst.A] = sregs[inst.B]; break;
          case 1: sregs[inst.A] = memory[sregs[inst.B] + inst.C] << 8 | memory[sregs[inst.B] + inst.C + 1]; break;
          case 2:
            memory[sregs[inst.A] + inst.B] = sregs[inst.C] >> 8;
            memory[sregs[inst.A] + inst.B + 1] = sregs[inst.C] & 0xFF;
            break;
          case 3: sregs[inst.A] = inst.B << 8 | inst.C; break;
          case 4: memcpy(&vregs[inst.A], &vregs[inst.B], sizeof(vreg_t)); break;
          case 5:
            for (int i = 0; i < 8; i++) vregs[inst.A][i] = memory[sregs[inst.B] + inst.C + i * 2] << 8 | memory[sregs[inst.B] + inst.C + i * 2 + 1];
            break;
          case 6:
            for (int i = 0; i < 8; i++) {
              memory[sregs[inst.A] + inst.B + i * 2] = vregs[inst.C][i] >> 8;
              memory[sregs[inst.A] + inst.B + i * 2 + 1] = vregs[inst.C][i] & 0xFF;
            }
            break;
          case 7:
            for (int i = 0; i < 8; i++)
              if (inst.C & (1 << i)) vregs[inst.A][i] = sregs[inst.B];
            break;
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
        switch (inst.aluctrl) {
          case 0:
            for (int i = 0; i < 8; i++) vregs[inst.A][i] = vregs[inst.B][i] + vregs[inst.C][i];
            break;
          case 1:
            for (int i = 0; i < 8; i++) vregs[inst.A][i] = vregs[inst.B][i] * vregs[inst.C][i];
            break;
          case 2:
            for (int i = 0; i < 8; i++) vregs[inst.A][i] = -vregs[inst.B][i];
            break;
          case 3:
            for (int i = 0; i < 8; i++) vregs[inst.A][i] = vregs[inst.B][i] / vregs[inst.C][i];
            break;
          case 4:
            for (int i = 0; i < 8; i++) vregs[inst.A][i] = vregs[inst.B][i] & vregs[inst.C][i];
            break;
          case 5:
            for (int i = 0; i < 8; i++) vregs[inst.A][i] = vregs[inst.B][i] | vregs[inst.C][i];
            break;
          case 6:
            for (int i = 0; i < 8; i++) vregs[inst.A][i] = vregs[inst.B][i] ^ vregs[inst.C][i];
            break;
          case 7:
            for (int i = 0; i < 8; i++) vregs[inst.A][i] = ~vregs[inst.B][i];
            break;
        } break;
      case 0x03: // VECTOR COMPARE
        switch (inst.aluctrl) {
          case 0:
            for (int i = 0; i < 8; i++) vregs[inst.A][i] = (vregs[inst.B][i] == vregs[inst.C][i]) ? 0xFFFF : 0;
            break;
          case 1:
            for (int i = 0; i < 8; i++) vregs[inst.A][i] = (vregs[inst.B][i] > vregs[inst.C][i]) ? 0xFFFF : 0;
            break;
          default: fprintf(stderr, "unknown vector compare instruction\n"); dump();
        } break;
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
          case 2: flags = FLAG((inst.A << 8 | inst.B) - sregs[inst.C]); break;
          case 3: flags = inst.A & (ZF | SF); break;
          default: fprintf(stderr, "unknown test instruction\n"); dump();
        } break;
      case 0x06: // IO
        switch (inst.aluctrl) {
          case 0: sregs[inst.A] = (sregs[inst.A] & 0xFF00) | in(); break;
          case 1: sregs[inst.A] = (sregs[inst.A] & 0x00FF) | (in() << 8); break;
          case 2: out(sregs[inst.A] & 0xFF); break;
          case 3: out(sregs[inst.A] >> 8); break;
          default: fprintf(stderr, "unknown IO instruction\n"); dump();
        } break;
      case 0x07: return;
      default: fprintf(stderr, "unknown opcode 0x%02x\n", inst.opcode); dump();
    }
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

  signal(SIGINT, dump);

  execute();

  fprintf(stderr, "Execution finished.\n");
  dump();

  return 0;
}


