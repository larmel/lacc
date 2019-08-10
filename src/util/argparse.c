#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "argparse.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static int is_flag(struct option opt)
{
    return (strlen(opt.rule) == 2) && (opt.rule[0] == '-');
}

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
 * consumed. In case of flag option, return number of characters
 * matched. Otherwise, return 0.
 */
static int match_arg(struct option opt, int argc, char *argv[], int *ret)
{
    int i;
    size_t rulelen, arglen;
    char lastchar, flag;
    assert(opt.callback);

    *ret = 0;
    rulelen = strlen(opt.rule);
    arglen = strlen(argv[0]);
    lastchar = opt.rule[rulelen - 1];
    switch (lastchar) {
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
            if (!strcmp(argv[0], "-W")) {
                argv[0] = "-Wextra";
            } else if (arglen == rulelen) {
                fprintf(stderr, "Missing argument to %s.\n", argv[0]);
                *ret = 1;
                return 0;
            }
            *ret = opt.callback(argv[0] + rulelen);
            return 1;
        }
        break;
    default:
        if (!is_flag(opt)) {
            if (match_rule(opt.rule, argv[0])) {
                *ret = opt.callback(argv[0]);
                return 1;
            }
        } else {
            flag = opt.rule[1];
            for (i = 1; i < strlen(argv[0]); ++i) {
                if (argv[0][i] == flag) {
                    *ret = opt.callback(opt.rule + 1);
                    return 1;
                }
            }
        }
    }

    return 0;
}

/*
 * Matching works by looping through each input token, trying every
 * option in sequence. All non-flag options are tried first, meaning
 * an input like -std is first checked as "-std", then flags -s, -t, -d.
 * All characters in the token must match a flag to be accepted.
 *
 * First token is skipped, as it is assumed to contain program name.
 */
INTERNAL int parse_args(struct option *optv, int argc, char *argv[])
{
    int i, c, ret;
    struct option *opt;

    for (i = 1; i < argc;) {
        if (*(argv[i]) == '-') {
            c = 0;
            for (opt = optv; opt->rule; ++opt) {
                if (!is_flag(*opt)) {
                    c = match_arg(*opt, argc - i, argv + i, &ret);
                    if (ret) return ret;
                    if (c) {
                        i += c;
                        break;
                    }
                }
            }
            if (!c) {
                for (opt = optv; opt->rule; ++opt) {
                    if (is_flag(*opt)) {
                        c += match_arg(*opt, argc - i, argv + i, &ret);
                        if (ret) return ret;
                    }
                }
                if (c == strlen(argv[i]) - 1) {
                    i += 1;
                } else {
                    fprintf(stderr, "Invalid option %s.\n", argv[i]);
                    return 1;
                }
            }
        } else {
            for (opt = optv; opt->rule; ++opt)
                ;
            if ((ret = opt->callback(argv[i])) != 0)
                return ret;
            i++;
        }
    }

    return 0;
}
