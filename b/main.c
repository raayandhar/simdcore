#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "stb_c_lexer.h"

#define error(msg) \
  do { perror(msg); exit(1); } while (0)

#define DA_INIT_CAP 16

#define da_reserve(da, cap) \
  do { \
    if ((cap) > (da)->capacity) { \
      if ((da)->capacity == 0) (da)->capacity = DA_INIT_CAP; \
      while ((cap) > (da)->capacity) (da)->capacity *= 2; \
      (da)->items = realloc((da)->items, (da)->capacity * sizeof(*(da)->items)); \
    } \
  } while (0)

#define da_append(da, item) \
  do { \
    da_reserve(da, (da)->count + 1); \
    (da)->items[(da)->count++] = (item); \
  } while (0)

#define da_last(da) (da)->items[(da)->count - 1]
#define da_foreach(Type, it, da) for (Type *it = (da)->items; it < (da)->items + (da)->count; ++it)

#define fatalf(msg, ...) \
  do { \
    stb_lex_location loc; \
    stb_c_lexer_get_location(&lex, lex.where_firstchar, &loc); \
    fprintf(stderr, "fatal error at %d:%d (in %s): " msg "\n", loc.line_number, loc.line_offset, __func__, ##__VA_ARGS__); \
    exit(1); \
  } while (0)

#define expect(clex, msg) \
  do { \
    stb_c_lexer_get_token(&lex); \
    if (lex.token == CLEX_eof) fatalf(msg " (got eof)"); \
    if (lex.token != clex) fatalf(msg " (got '%.*s')", (int)(lex.where_lastchar - lex.where_firstchar + 1), lex.where_firstchar); \
  } while (0)

#define MAX_PREC 6

int prec(long clex) {
  switch (clex) {
    case '|': return 0;
    case '&': return 1;
    case CLEX_eq:
    case CLEX_noteq: return 2;
    case '<':
    case '>':
    case CLEX_lesseq:
    case CLEX_greatereq: return 3;
    case CLEX_shl:
    case CLEX_shr: return 4;
    case '+':
    case '-': return 5;
    case '*':
    case '/':
    case '%': return 6;
    default: return -1;
  }
}

#define RESERVED "while switch goto break return extrn auto"

char *input;
char store[1024];
stb_lexer lex;

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

char *kind_to_string(int kind) {
  switch (kind) {
    case LIST: return "LIST";
    case FUNC: return "FUNC";
    case DECL: return "DECL";
    case BLOCK: return "BLOCK";
    case IF: return "IF";
    case WHILE: return "WHILE";
    case FOR: return "FOR";
    case GOTO: return "GOTO";
    case RETURN: return "RETURN";
    case ID: return "ID";
    case INT: return "INT";
    case STR: return "STR";
    case ASSIGN: return "ASSIGN";
    case BINOP: return "BINOP";
    case UNOP: return "UNOP";
    case TERN: return "TERN";
    case CALL: return "CALL";
  }
  return "UNKNOWN";
}

void print_node_helper(Node *n, const char *prefix) {
  char new_prefix[256];

  printf("%s %s", prefix, kind_to_string(n->kind));
  switch (n->kind) {
    case BINOP:
    case UNOP: printf(" (op: %ld)", n->op); break;
    case ID:
    case STR:
    case GOTO:
    case FUNC: printf(" (s: %s)", n->s); break;
    case INT: printf(" (val: %ld)", n->val); break;
    case DECL: printf(" (class: %s)", n->storage_class == AUTO ? "AUTO" : n->storage_class == EXTRN ? "EXTRN" : "INTERNAL"); break;
    default: break;
  }
  printf("\n");

  snprintf(new_prefix, sizeof(new_prefix), "%s  ", prefix);
  if (n->kind == LIST || n->kind == BLOCK || n->kind == DECL)
    da_foreach(Node *, item, &n->da) print_node_helper(*item, new_prefix);
  if (n->kind == ASSIGN || n->kind == TERN || n->kind == BINOP || n->kind == UNOP || n->kind == CALL
      || n->kind == IF || n->kind == WHILE || (n->kind == RETURN && n->a) || n->kind == FUNC)
    print_node_helper(n->a, new_prefix);
  if (n->kind == ASSIGN || n->kind == TERN || n->kind == BINOP || n->kind == CALL
      || n->kind == IF || n->kind == WHILE || n->kind == FUNC)
    print_node_helper(n->b, new_prefix);
  if (n->kind == TERN) print_node_helper(n->c, new_prefix);
}

void print_node(Node *n) { print_node_helper(n, ""); }

long peek() {
  char *saved_pt = lex.parse_point;
  long ret = (stb_c_lexer_get_token(&lex), lex.token);
  lex.parse_point = saved_pt;
  return ret;
}

static Node *program(void), *decl(void), *compound(void), *stmt(void), *expr(void);
static Node *ternary(void), *binop(int), *unop(void), *postfix(void), *primary(void);

static Node *expr(void) {
  Node *l = ternary();
  if (peek() == '=') {
    stb_c_lexer_get_token(&lex);
    Node *r = expr();
    Node *ret = malloc(sizeof(Node));
    ret->kind = ASSIGN;
    ret->a = l;
    ret->b = r;
    return ret;
  }
  return l;
}

static Node *ternary(void) {
  Node *n = binop(0);
  if (peek() == '?') {
    stb_c_lexer_get_token(&lex);
    Node *then = expr();
    expect(':', "expected ':' in ternary expression");
    Node *els = expr();
    Node *ret = malloc(sizeof(Node));
    ret->kind = TERN;
    ret->a = n;
    ret->b = then;
    ret->c = els;
    return ret;
  }
  return n;
}

static Node *binop(int precedence) {
  if (precedence > MAX_PREC) return unop();
  Node *n = binop(precedence + 1);
  if (prec(peek()) == precedence) {
    stb_c_lexer_get_token(&lex);
    long op = lex.token;
    Node *rhs = binop(precedence);
    Node *ret = malloc(sizeof(Node));
    ret->kind = BINOP;
    ret->op = op;
    ret->a = n;
    ret->b = rhs;
    return ret;
  }
  return n;
}

static Node *unop(void) {
  switch (peek()) {
    case CLEX_plusplus:
    case CLEX_minusminus:
    case '+':
    case '-':
    case '!':
    case '*':
    case '&':
      stb_c_lexer_get_token(&lex);
      Node *ret = malloc(sizeof(Node));
      ret->kind = UNOP;
      ret->op = lex.token;
      ret->a = unop();
      return ret;
    default: return postfix();
  }
}

static Node *postfix(void) {
  Node *n = primary();
  while (1) {
    if (peek() == '(') {
      stb_c_lexer_get_token(&lex);
      Node *call = malloc(sizeof(Node));
      Node *args = malloc(sizeof(Node));
      memset(&args->da, 0, sizeof(args->da));
      if (peek() != ')') {
        da_append(&args->da, expr());
        while (peek() == ',') {
          stb_c_lexer_get_token(&lex);
          da_append(&args->da, expr());
        }
      }
      expect(')', "unmatched '(' in function call");
      call->kind = CALL;
      call->a = n;
      call->b = args;
      args->kind = LIST;
    } else return n;
  }
}

static Node *primary(void) {
  Node *n;
  switch (stb_c_lexer_get_token(&lex), lex.token) {
    case CLEX_id:
      n = malloc(sizeof(Node));
      n->kind = ID;
      n->s = strdup(lex.string);
      return n;
    case CLEX_intlit:
    case CLEX_charlit:
      n = malloc(sizeof(Node));
      n->kind = INT;
      n->val = lex.int_number;
      return n;
    case CLEX_dqstring:
    case CLEX_sqstring:
      n = malloc(sizeof(Node));
      n->kind = STR;
      n->s = strdup(lex.string);
      return n;
    case '(':
      n = expr();
      expect(')', "unmatched '(' in expression");
      return n;
    default: fatalf("syntax error in primary expression (id: 0x%lx)", lex.token);
  }
}

static Node *stmt(void) {
  if (peek() == '{') return compound();
  else {
    char *saved_pt = lex.parse_point;
    Node *ret = malloc(sizeof(Node));
    if (stb_c_lexer_get_token(&lex), lex.token == CLEX_id) {
      if (strcmp(lex.string, "auto") == 0 || strcmp(lex.string, "extrn") == 0) {
        Node *id = malloc(sizeof(Node));
        ret->kind = DECL;
        ret->storage_class = strcmp(lex.string, "auto") == 0 ? AUTO : EXTRN;
        ret->a = id;
        expect(CLEX_id, "expected identifier after storage class specifier");
        id->kind = ID;
        id->s = strdup(lex.string);
        expect(';', "expected ';' after declaration");
      } else if (strcmp(lex.string, "if") == 0) {
        ret->kind = IF;
        expect('(', "expected '(' after 'if'");
        ret->a = expr();
        expect(')', "unmatched '(' in 'if' predicate");
        ret->b = stmt();
        // TODO: else
      } else if (strcmp(lex.string, "while") == 0) {
        ret->kind = WHILE;
        expect('(', "expected '(' after 'while'");
        ret->a = expr();
        expect(')', "unmatched '(' in 'while' predicate");
        ret->b = stmt();
      } else if (strcmp(lex.string, "switch") == 0) {
        fatalf("switch statement not implemented");
      } else if (strcmp(lex.string, "goto") == 0) {
        ret->kind = GOTO;
        if (peek() != CLEX_id) fatalf("expected identifier after 'goto'");
        ret->s = strdup(lex.string);
        expect(';', "expected ';' after 'goto'");
      } else if (strcmp(lex.string, "break") == 0) {
        fatalf("break statement not implemented");
      } else if (strcmp(lex.string, "return") == 0) {
        ret->kind = RETURN;
        if (peek() != ';') ret->a = expr();
        else ret->a = NULL;
        expect(';', "expected ';' after 'return'");
      } else goto expr;
    } else {
expr:
      lex.parse_point = saved_pt;
      ret = expr();
      expect(';', "expected ';' after expr");
    }
    return ret;
  }
}

static Node *compound(void) {
  Node *ret = malloc(sizeof(Node));
  ret->kind = BLOCK;
  memset(&ret->da, 0, sizeof(ret->da));
  expect('{', "expected '{'");
  while (peek() != '}') da_append(&ret->da, stmt());
  expect('}', "unmatched '{' in compound statement");
  return ret;
}

static Node *decl(void) {
  char *name;
  Node *ret = malloc(sizeof(Node));
  expect(CLEX_id, "expected identifier");
  name = strdup(lex.string);
  if (peek() == '(') {
    stb_c_lexer_get_token(&lex);
    ret->kind = FUNC;
    Node *args = malloc(sizeof(Node));
    args->kind = LIST;
    memset(&args->da, 0, sizeof(args->da));
    if (peek() != ')') {
      stb_c_lexer_get_token(&lex);
      Node *id = malloc(sizeof(Node));
      id->kind = ID;
      id->s = strdup(lex.string);
      da_append(&args->da, id);
      while(peek() == ',') {
        stb_c_lexer_get_token(&lex);
        stb_c_lexer_get_token(&lex);
        if (lex.token != CLEX_id) fatalf("expected identifier after ','");
        id = malloc(sizeof(Node));
        id->kind = ID;
        id->s = strdup(lex.string);
        da_append(&args->da, id);
      }
    }
    expect(')', "unmatched '(' in function declaration");
    ret->a = args;
    ret->b = compound();
    ret->s = name;
    return ret;
  } else {
    Node *id = malloc(sizeof(Node));
    ret->kind = DECL;
    ret->storage_class = INTERNAL;
    id->kind = ID;
    id->s = name;
    memset(&ret->da, 0, sizeof(ret->da));
    da_append(&ret->da, id);
    while (peek() == ',') {
      stb_c_lexer_get_token(&lex);
      expect(CLEX_id, "expected identifier after ','");
      id = malloc(sizeof(Node));
      id->kind = ID;
      id->s = strdup(lex.string);
      da_append(&ret->da, id);
    }
    expect(';', "expected ';' after declaration");
    return ret;
  }
}

static Node *program(void) {
  Node *ret = malloc(sizeof(Node));
  ret->kind = LIST;
  memset(&ret->da, 0, sizeof(ret->da));
  while (peek() != CLEX_eof) da_append(&ret->da, decl());
  return ret;
}

void usage(const char *name) {
  fprintf(stderr, "usage: %s <filename>\n", name);
  exit(1);
}

int main(int argc, char **argv) {
  int fd;
  struct stat sb;

  if (argc < 2) usage(argv[0]);
  if ((fd = open(argv[1], O_RDONLY)) < 0) error("open");
  if (fstat(fd, &sb) == -1) error("fstat");
  if ((input = mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0)) == MAP_FAILED) error("mmap");
  close(fd);

  stb_c_lexer_init(&lex, input, input + sb.st_size, store, sizeof(store));
  print_node(program());
}

