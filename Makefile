.PHONY: all clean examples

EXAMPLES := $(wildcard examples/*.s)
EXAMPLE_BINS := $(EXAMPLES:.s=.bin)

all: simd sim examples

simd: as.hs
	ghc --make as.hs -o simd -optP-DANSICOLOR

sim: sim.c
	$(CC) -o sim sim.c -Wall -Wextra

examples: $(EXAMPLE_BINS)

examples/%.bin: examples/%.s simd
	./simd as $< $@

clean:
	rm -f simd as.hi as.o sim examples/*.bin

