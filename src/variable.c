#include "symbol.h"

#include <string.h>

var_t
var_direct(const symbol_t *symbol)
{
    var_t var = {0};
    var.kind = DIRECT;
    var.symbol = symbol;
    var.type = symbol->type;
    return var;
}

var_t
var_offset(const symbol_t *symbol, int offset)
{
    var_t var = {0};
    var.kind = OFFSET;
    var.symbol = symbol;
    var.type = type_deref(symbol->type);
    var.offset = offset;
    return var;
}

var_t
var_string(const char *text)
{
    var_t var = {0};
    typetree_t *type, *next;

    next = type_init(INTEGER); /* char */
    next->size = 1;

    type = type_init(ARRAY);
    type->next = next;
    type->size = next->size * strlen(text);

    var.kind = IMMEDIATE;
    var.type = type;
    var.value.string = strdup(text);
    return var;
}

var_t
var_long(long value)
{
    var_t var = {0};
    typetree_t *type;

    type = type_init(INTEGER);
    type->size = 8;

    var.kind = IMMEDIATE;
    var.type = type;
    var.value.integer = value;
    return var;
}

var_t
var_void()
{
    var_t var = {0};

    var.kind = IMMEDIATE;
    var.type = type_init(NONE);
    return var;
}

/* Determine whether the variable can be assigned to.
 */
int
islvalue(var_t var)
{
    return ((var.kind == DIRECT && var.symbol->name[0] != '@') || var.kind == OFFSET);
}
