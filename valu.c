#include <stdio.h>
#include <stdint.h>
#include <immintrin.h>

typedef union {
  struct half {
		uint16_t m:10;	
    uint16_t e:5;
		uint16_t s:1;
  } bits;
  uint16_t u;
  _Float16 f;
} half;

size_t clz_16(uint16_t x) { return __builtin_clz(x) - 16; }
size_t hamming_16(uint16_t x, uint16_t y) { return __builtin_popcount(x ^ y); }

half add_half(half a, half b) {
  half ret;
	uint16_t e, a_m, b_m, ret_m;
	if (a.bits.e > b.bits.e) {
		e = a.bits.e;
		a_m = a.bits.m;
		b_m = b.bits.m >> (a.bits.e - b.bits.e);
	} else if (a.bits.e < b.bits.e) {
		e = b.bits.e;
		a_m = b.bits.m >> (b.bits.e - a.bits.e);
		b_m = b.bits.m;
	} else {
		e = a.bits.e;
		a_m = a.bits.m;
		b_m = b.bits.m;
	}
	if (a.bits.s == b.bits.s) ret_m = a_m + b_m;
	else if (a.bits.s == 0) ret_m = a_m - b_m;
	else ret_m = b_m - a_m;
	// recompute ret.m , ret.e
	if (a.bits.s == b.bits.s) {
		ret.bits.s = a.bits.s;
	} else {
		if (a.bits.s == 0) { ret.bits.s = a_m > b_m; }
		else { ret.bits.s = a_m < b_m; }
	}
	if (ret_m & 0xFC00) {
		size_t amt = 6 - clz_16(ret_m);
		ret.bits.m = ret_m >> amt;
		ret.bits.e = e + amt;
	} else {
		ret.bits.m = ret_m;
		ret.bits.e = e;
	}
	return ret;
}

// void main(void) {
// 	size_t max_hamming = 0;
// 	uint64_t avg_hamming = 0;
//   for (half a = {0}; a.u <= 0xFFFF; a.u++) {
// 		printf("a: %f\n", a.f);
//     for (half b = {0}; b.u <= 0xFFFF; b.u++) {
//       half result = add_half(a, b);
//       size_t hamming = hamming_16(a.u, b.u);
//       if (result.f != a.f + b.f) {
// 				if (hamming > max_hamming) { max_hamming = hamming; }
// 				avg_hamming += hamming;
//         //printf("Error: %f(0x%04X) + %f(0x%04X) = %f(0x%04X), expected %f(0x%04X)\n", 
//               // a.f, a.u, b.f, b.u, result.f, result.u, a.f + b.f, (half){.f = a.f + b.f}.u);
//         // exit(-1);
//       }
//     }
//   }	
//   printf("max hamming: %lu\n", max_hamming);
// 	double div = (double)(0x100000000);
//   printf("avg hamming: %f\n", (double)avg_hamming / div);
// }

void main(int argc, char **argv) {
	if (argc != 3) {
		printf("usage: %s a b\n", argv[0]);
		exit(-1);
	}
	half a = {.f = atof(argv[1])};
	half b = {.f = atof(argv[2])};
	half result = add_half(a, b);
	printf("a: %f(s:%d m:%x e:%d)\n", (double)a.f, a.bits.s, a.bits.m, a.bits.e);
	printf("b: %f(s:%d m:%x e:%d)\n", (double)b.f, b.bits.s, b.bits.m, b.bits.e);
	printf("result: %f(s:%d m:%x e:%d)\n", (double)result.f, result.bits.s, result.bits.m, result.bits.e);
}