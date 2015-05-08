#include "ir.h"
#include "type.h"
#include "symbol.h"

#include <assert.h>

var_t
var_direct(const symbol_t *symbol)
{
    var_t var = {0};
    var.type = symbol->type;
    if (symbol->symtype == SYM_ENUM) {
        var.kind = IMMEDIATE;
        var.value.integer = symbol->enum_value;
    } else {
        var.kind = DIRECT;
        var.symbol = symbol;
        var.lvalue = symbol->name[0] != '.';
    }
    return var;
}

var_t
var_deref(const symbol_t *symbol, int offset)
{
    var_t var = {0};
    assert(symbol->type->type == POINTER);

    var.kind = DEREF;
    var.symbol = symbol;
    var.type = type_deref(symbol->type);
    var.offset = offset;
    var.lvalue = 1;

    return var;
}

var_t
var_string(const char *label, size_t length)
{
    var_t var = {0};

    var.kind = IMMEDIATE;
    var.type = type_init_string(length);
    var.value.string = label;

    return var;
}

var_t
var_int(int value)
{
    var_t var = {0};
    typetree_t *type;

    type = type_init(INTEGER);

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
