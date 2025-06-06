#ifndef _UTILS_H
#define _UTILS_H

#include "parse.h"

#define DA_INIT_CAP 16

#define da_reserve(da, cap) \
  do { \
    if ((cap) > (da)->capacity) { \
      if ((da)->capacity == 0) (da)->capacity = DA_INIT_CAP; \
      while ((cap) > (da)->capacity) (da)->capacity *= 2; \
      /* NOLINTNEXTLINE(bugprone-sizeof-expression) */ \
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

static inline char *kind_to_string(int kind) {
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

#endif

