#include "parse.h"
#include "utils.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "stb_c_lexer.h"

static Node *decl(stb_lexer *lex), *compound(stb_lexer *lex), *stmt(stb_lexer *lex), *expr(stb_lexer *lex);
static Node *ternary(stb_lexer *lex), *binop(stb_lexer *lex, int), *unop(stb_lexer *lex), *postfix(stb_lexer *lex), *primary(stb_lexer *lex);

#define fatalf(msg, ...) \
  do { \
    stb_lex_location loc; \
    stb_c_lexer_get_location(lex, lex->where_firstchar, &loc); \
    fprintf(stderr, "fatal error at %d:%d (in %s): " msg "\n", loc.line_number, loc.line_offset, __func__, ##__VA_ARGS__); \
    exit(1); \
  } while (0)

#define expect(clex, msg) \
  do { \
    stb_c_lexer_get_token(lex); \
    if (lex->token == CLEX_eof) fatalf(msg " (got eof)"); \
    if (lex->token != clex) fatalf(msg " (got '%.*s')", (int)(lex->where_lastchar - lex->where_firstchar + 1), lex->where_firstchar); \
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

long peek(stb_lexer *lex) {
  char *saved_pt = lex->parse_point;
  long ret = (stb_c_lexer_get_token(lex), lex->token);
  lex->parse_point = saved_pt;
  return ret;
}

static Node *expr(stb_lexer *lex) {
  Node *l = ternary(lex);
  if (peek(lex) == '=') {
    stb_c_lexer_get_token(lex);
    Node *r = expr(lex);
    Node *ret = malloc(sizeof(Node));
    ret->kind = ASSIGN;
    ret->a = l;
    ret->b = r;
    return ret;
  }
  return l;
}

static Node *ternary(stb_lexer *lex) {
  Node *n = binop(lex, 0);
  if (peek(lex) == '?') {
    stb_c_lexer_get_token(lex);
    Node *then = expr(lex);
    expect(':', "expected ':' in ternary expression");
    Node *els = expr(lex);
    Node *ret = malloc(sizeof(Node));
    ret->kind = TERN;
    ret->a = n;
    ret->b = then;
    ret->c = els;
    return ret;
  }
  return n;
}

static Node *binop(stb_lexer *lex, int precedence) {
  if (precedence > MAX_PREC) return unop(lex);
  Node *n = binop(lex, precedence + 1);
  if (prec(peek(lex)) == precedence) {
    stb_c_lexer_get_token(lex);
    long op = lex->token;
    Node *rhs = binop(lex, precedence);
    Node *ret = malloc(sizeof(Node));
    ret->kind = BINOP;
    ret->op = op;
    ret->a = n;
    ret->b = rhs;
    return ret;
  }
  return n;
}

static Node *unop(stb_lexer *lex) {
  switch (peek(lex)) {
    case CLEX_plusplus:
    case CLEX_minusminus:
    case '+':
    case '-':
    case '!':
    case '*':
    case '&':
      stb_c_lexer_get_token(lex);
      Node *ret = malloc(sizeof(Node));
      ret->kind = UNOP;
      ret->op = lex->token;
      ret->a = unop(lex);
      return ret;
    default: return postfix(lex);
  }
}

static Node *postfix(stb_lexer *lex) {
  Node *n = primary(lex);
  while (1) {
    if (peek(lex) == '(') {
      stb_c_lexer_get_token(lex);
      Node *call = malloc(sizeof(Node));
      Node *args = malloc(sizeof(Node));
      memset(&args->da, 0, sizeof(args->da));
      if (peek(lex) != ')') {
        da_append(&args->da, expr(lex));
        while (peek(lex) == ',') {
          stb_c_lexer_get_token(lex);
          da_append(&args->da, expr(lex));
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

static Node *primary(stb_lexer *lex) {
  Node *n;
  switch (stb_c_lexer_get_token(lex), lex->token) {
    case CLEX_id:
      n = malloc(sizeof(Node));
      n->kind = ID;
      n->s = strdup(lex->string);
      return n;
    case CLEX_intlit:
    case CLEX_charlit:
      n = malloc(sizeof(Node));
      n->kind = INT;
      n->val = lex->int_number;
      return n;
    case CLEX_dqstring:
    case CLEX_sqstring:
      n = malloc(sizeof(Node));
      n->kind = STR;
      n->s = strdup(lex->string);
      return n;
    case '(':
      n = expr(lex);
      expect(')', "unmatched '(' in expression");
      return n;
    default: fatalf("syntax error in primary expression (id: 0x%lx)", lex->token);
  }
}

static Node *stmt(stb_lexer *lex) {
  if (peek(lex) == '{') return compound(lex);
  else {
    char *saved_pt = lex->parse_point;
    Node *ret = malloc(sizeof(Node));
    if (stb_c_lexer_get_token(lex), lex->token == CLEX_id) {
      if (strcmp(lex->string, "auto") == 0 || strcmp(lex->string, "extrn") == 0) {
        Node *id = malloc(sizeof(Node));
        ret->kind = DECL;
        ret->storage_class = strcmp(lex->string, "auto") == 0 ? AUTO : EXTRN;
        ret->a = id;
        expect(CLEX_id, "expected identifier after storage class specifier");
        id->kind = ID;
        id->s = strdup(lex->string);
        expect(';', "expected ';' after declaration");
      } else if (strcmp(lex->string, "if") == 0) {
        ret->kind = IF;
        expect('(', "expected '(' after 'if'");
        ret->a = expr(lex);
        expect(')', "unmatched '(' in 'if' predicate");
        ret->b = stmt(lex);
        // TODO: else
      } else if (strcmp(lex->string, "while") == 0) {
        ret->kind = WHILE;
        expect('(', "expected '(' after 'while'");
        ret->a = expr(lex);
        expect(')', "unmatched '(' in 'while' predicate");
        ret->b = stmt(lex);
      } else if (strcmp(lex->string, "switch") == 0) {
        fatalf("switch statement not implemented");
      } else if (strcmp(lex->string, "goto") == 0) {
        ret->kind = GOTO;
        if (peek(lex) != CLEX_id) fatalf("expected identifier after 'goto'");
        ret->s = strdup(lex->string);
        expect(';', "expected ';' after 'goto'");
      } else if (strcmp(lex->string, "break") == 0) {
        fatalf("break statement not implemented");
      } else if (strcmp(lex->string, "return") == 0) {
        ret->kind = RETURN;
        if (peek(lex) != ';') ret->a = expr(lex);
        else ret->a = NULL;
        expect(';', "expected ';' after 'return'");
      } else goto expr;
    } else {
expr:
      lex->parse_point = saved_pt;
      ret = expr(lex);
      expect(';', "expected ';' after expr");
    }
    return ret;
  }
}

static Node *compound(stb_lexer *lex) {
  Node *ret = malloc(sizeof(Node));
  ret->kind = BLOCK;
  memset(&ret->da, 0, sizeof(ret->da));
  expect('{', "expected '{'");
  while (peek(lex) != '}') da_append(&ret->da, stmt(lex));
  expect('}', "unmatched '{' in compound statement");
  return ret;
}

static Node *decl(stb_lexer *lex) {
  char *name;
  Node *ret = malloc(sizeof(Node));
  expect(CLEX_id, "expected identifier");
  name = strdup(lex->string);
  if (peek(lex) == '(') {
    stb_c_lexer_get_token(lex);
    ret->kind = FUNC;
    Node *args = malloc(sizeof(Node));
    args->kind = LIST;
    memset(&args->da, 0, sizeof(args->da));
    if (peek(lex) != ')') {
      stb_c_lexer_get_token(lex);
      Node *id = malloc(sizeof(Node));
      id->kind = ID;
      id->s = strdup(lex->string);
      da_append(&args->da, id);
      while(peek(lex) == ',') {
        stb_c_lexer_get_token(lex);
        stb_c_lexer_get_token(lex);
        if (lex->token != CLEX_id) fatalf("expected identifier after ','");
        id = malloc(sizeof(Node));
        id->kind = ID;
        id->s = strdup(lex->string);
        da_append(&args->da, id);
      }
    }
    expect(')', "unmatched '(' in function declaration");
    ret->a = args;
    ret->b = compound(lex);
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
    while (peek(lex) == ',') {
      stb_c_lexer_get_token(lex);
      expect(CLEX_id, "expected identifier after ','");
      id = malloc(sizeof(Node));
      id->kind = ID;
      id->s = strdup(lex->string);
      da_append(&ret->da, id);
    }
    expect(';', "expected ';' after declaration");
    return ret;
  }
}

Node *parse(const char *input_stream, const char *input_stream_end) {
  stb_lexer lex;
  char storage[1024];
  Node *ret = malloc(sizeof(Node));

  stb_c_lexer_init(&lex, input_stream, input_stream_end, storage, sizeof(storage));

  ret->kind = LIST;
  memset(&ret->da, 0, sizeof(ret->da));

  while (peek(&lex) != CLEX_eof) da_append(&ret->da, decl(&lex));

  return ret;
}

