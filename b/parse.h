#ifndef _PARSE_H
#define _PARSE_H

typedef struct Node {
  enum {
    LIST, FUNC, DECL,
    BLOCK, IF, WHILE, FOR,
    GOTO, RETURN,
    ID, INT, STR,
    ASSIGN, BINOP, UNOP, TERN, CALL
  } kind;
  struct Node *a,
              *b,
              *c;
  union {
    struct {
      struct Node **items;
      int count, capacity;
    } da; // LIST, BLOCK, DECL
    long op; // BINOP, UNOP
    char *s; // ID, STR, GOTO, FUNC
    long val; // INT
    enum { AUTO, EXTRN, INTERNAL } storage_class; // DECL
  };
} Node;

Node *parse(const char *input_stream, const char *input_stream_end);

#endif

