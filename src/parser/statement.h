#ifndef STATEMENT_H
#define STATEMENT_H

#include <lacc/ir.h>

INTERNAL struct block *statement(struct definition *def, struct block *parent);

INTERNAL struct block *block(struct definition *def, struct block *parent);

#endif
