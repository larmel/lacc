#ifndef DECLARATION_H
#define DECLARATION_H

#include <lacc/ir.h>

struct block *declaration(struct block *parent);

struct typetree *declarator(struct typetree *base, const char **symbol);

struct typetree *declaration_specifiers(int *stc);

#endif
