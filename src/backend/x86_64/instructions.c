#include "instructions.h"

#include <assert.h>

#define rd(reg) register_codes[(reg).r]

/* Map register enum values to register encoding. Depends on register
 * enumeration values.
 */
static const unsigned char register_codes[] = {
    0,
    0, /* AX, bacause AX enum = 1 */
    3, /* BX */
    1, /* CX */
    2, /* DX */
    5, /* PB */
    4, /* SP */
    6, /* SI */
    7  /* DI */
};

static struct code nop(void)
{
    struct code c = {{0x90}, 1};
    return c;
}

static struct code push(enum instr_optype optype, union operand op)
{
    struct code c = nop();
    if (optype == OPT_REG)
        c.val[0] = 0x50 + rd(op.reg);
    return c;
}

static struct code leave(void)
{
    struct code c = {{0xC9}, 1};
    return c;
}

static struct code ret(void)
{
    /* Only 'Near return' is used, returning to a function with address in the
     * same segment, and not popping any bytes from stack. */
    struct code c = {{0xC3}, 1};
    return c;
}

struct code encode(struct instruction instr)
{
    switch (instr.opcode) {
    case INSTR_PUSH:
        return push(instr.optype, instr.source);
    case INSTR_LEAVE:
        return leave();
    case INSTR_RET:
        return ret();
    default:
        return nop();
    }
}
