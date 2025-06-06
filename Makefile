.PHONY: all clean examples b

EXAMPLES := $(wildcard examples/*.s)
EXAMPLE_BINS := $(EXAMPLES:.s=.bin)

DEMO_SRCS := $(wildcard demo/*.s)
DEMO_BINS := $(DEMO_SRCS:.s=.bin)

all: simd ld b sim examples demo.bin

simd: as.hs
	ghc --make as.hs -o simd -optP-DANSICOLOR

ld: ld.c
	$(CC) -o ld ld.c -Wall -Wextra

b:
	$(MAKE) -C b

sim: sim.c
	$(CC) -o sim sim.c -Wall -Wextra

examples: $(EXAMPLE_BINS)

examples/%.bin: examples/%.s simd
	./simd as $< $@

demo.bin :$(DEMO_BINS) ld
	./ld $(DEMO_BINS) > $@

demo/%.bin: demo/%.s simd
	./simd as $< $@

clean:
	rm -f simd ld as.hi as.o sim examples/*.bin
	$(MAKE) -C b clean

