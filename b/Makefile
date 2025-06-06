.PHONY: clean

SRCS := $(wildcard *.c)
OBJS := $(SRCS:.c=.o) stb_c_lexer.o
DEPS := $(OBJS:.o=.d)

CFLAGS := -Wall -Wextra -Werror -std=c99 -MMD -MP -D_POSIX_C_SOURCE=200809L

b: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

-include $(DEPS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

stb_c_lexer.o: stb_c_lexer.h
	$(CC) $(CFLAGS) -DSTB_C_LEXER_IMPLEMENTATION -x c -c $< -o $@

clean:
	rm -f $(OBJS) $(DEPS) b
