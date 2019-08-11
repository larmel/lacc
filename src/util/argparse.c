#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "argparse.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/*
 * Check if arg matches rule, potentially containing option brackets
 * like -f[no-]PIC, and sets like -O{0|1|2|3}.
 */
static int match_rule(const char *rule, const char *arg)
{
    const char *ptr;

    assert(rule);
    assert(arg);

    while (*rule == *arg && *rule && *arg) {
        rule++;
        arg++;
    }

    switch (*rule) {
    case '\0':
        return *arg == '\0';
    case '[':
        ptr = ++rule;
        while (*ptr != ']') {
            ptr++;
        }
        return !strncmp(rule, arg, ptr - rule)
            ? match_rule(ptr + 1, arg + (ptr - rule))
            : match_rule(ptr + 1, arg);
    case '{':
        do {
            ptr = ++rule;
            while (*ptr != '}' && *ptr != '|') {
                ptr++;
            }
            if (!strncmp(rule, arg, ptr - rule)) {
                arg = arg + (ptr - rule);
                do {
                    rule = ptr;
                    ptr++;
                } while (*rule != '}');
                return match_rule(ptr, arg);
            }
            rule = ptr;
        } while (*rule == '|');
        break;
    }

    return 0;
}

/*
 * Check if argv matches given option. Argument vector is offset, and
 * not the same as input to the program. Start reading from index 0 of
 * argv.
 *
 * In case of match, invoke callback and return the number of tokens
 * consumed. Otherwise return 0.
 */
static int match_arg(struct option opt, int argc, char *argv[], int *ret)
{
    size_t rulelen, arglen;
    assert(opt.callback);

    *ret = 0;
    rulelen = strlen(opt.rule);
    arglen = strlen(argv[0]);
    switch (opt.rule[rulelen - 1]) {
    case ':':
        rulelen -= 1;
        if (!strncmp(opt.rule, argv[0], rulelen)) {
            if (arglen == rulelen) {
                if (argc < 2) {
                    fprintf(stderr, "Missing argument to %s.\n", opt.rule);
                    *ret = 1;
                    return 0;
                }
                *ret = opt.callback(argv[1]);
                return 2;
            } else {
                assert(arglen > rulelen);
                *ret = opt.callback(argv[0] + rulelen);
                return 1;
            }
        }
        break;
    case '<':
        rulelen -= 1;
    case '=':
        if (!strncmp(opt.rule, argv[0], rulelen)) {
            if (arglen == rulelen) {
                fprintf(stderr, "Missing argument to %s.\n", argv[0]);
                *ret = 1;
                return 0;
            }
            *ret = opt.callback(argv[0] + rulelen);
            return 1;
        }
        break;
    default:
        if (match_rule(opt.rule, argv[0])) {
            *ret = opt.callback(argv[0]);
            return 1;
        }
    }

    return 0;
}

/*
 * Matching works by looping through each input token, trying every
 * option in sequence.
 *
 * First token is skipped, as it is assumed to contain program name.
 */
INTERNAL int parse_args(struct option *optv, int argc, char *argv[])
{
    int i, c, ret;
    struct option *opt, *last;

    for (last = optv; last->rule; ++last)
        ;

    for (i = 1; i < argc;) {
        if (*(argv[i]) == '-') {
            for (opt = optv, c = 0; opt->rule && !c; ++opt) {
                c = match_arg(*opt, argc - i, argv + i, &ret);
                if (ret != 0)
                    return ret;
            }
            if (c) {
                i += c;
            } else {
                fprintf(stderr, "Unrecognized option %s.\n", argv[i]);
                return 1;
            }
        } else {
            if ((ret = last->callback(argv[i])) != 0)
                return ret;
            i++;
        }
    }

    return 0;
}
