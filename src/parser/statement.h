#ifndef STATEMENT_H
#define STATEMENT_H

#include <lacc/ir.h>

struct block *statement(struct definition *def, struct block *parent);

struct block *block(struct definition *def, struct block *parent);

#endif
