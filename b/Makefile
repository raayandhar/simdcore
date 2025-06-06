.PHONY: clean

SRCS := $(wildcard *.c)
OBJS := $(SRCS:.c=.o) stb_c_lexer.o

b: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

stb_c_lexer.o: stb_c_lexer.h
	$(CC) $(CFLAGS) -DSTB_C_LEXER_IMPLEMENTATION -x c -c $< -o $@

clean:
	rm -f $(OBJS) b
