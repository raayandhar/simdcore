#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "stb_c_lexer.h"
#include "parse.h"
#include "utils.h"

#define error(msg) \
  do { perror(msg); exit(1); } while (0)

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

void usage(const char *name) {
  fprintf(stderr, "usage: %s <filename>\n", name);
  exit(1);
}

int main(int argc, char **argv) {
  int fd;
  struct stat sb;
  char *input;

  if (argc < 2) usage(argv[0]);
  if ((fd = open(argv[1], O_RDONLY)) < 0) error("open");
  if (fstat(fd, &sb) == -1) error("fstat");
  if ((input = mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0)) == MAP_FAILED) error("mmap");
  close(fd);

  print_node(parse(input, input + sb.st_size));
}

