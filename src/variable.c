#include "symbol.h"

#include <string.h>

var_t
var_direct(const symbol_t *symbol)
{
    var_t var;
    var.kind = DIRECT;
    var.symbol = symbol;
    var.type = symbol->type;
    return var;
}

var_t
var_offset(const symbol_t *symbol, int offset)
{
    var_t var;
    var.kind = OFFSET;
    var.symbol = symbol;
    var.type = type_deref(symbol->type);
    var.offset = offset;
    return var;
}

var_t
var_string(const char *text)
{
    var_t var;
    typetree_t *type;

    type = type_init(ARRAY);
    type->length = strlen(text);
    type->next = type_init(CHAR_T);
    type->size = type->next->size;

    var.kind = IMMEDIATE;
    var.type = type;
    var.value.v_string = strdup(text);
    return var;
}

var_t
var_long(long value)
{
    var_t var;
    typetree_t *type;

    type = type_init(INT64_T);

    var.kind = IMMEDIATE;
    var.type = type;
    var.value.v_long = value;
    return var;
}
