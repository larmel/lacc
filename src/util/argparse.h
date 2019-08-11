#ifndef ARGPARSE_H
#define ARGPARSE_H

/*
 * Specify command line argument pattern as a rule template with an
 * associated callback, which is invoked on match.
 *
 *  "-S"        Regular options, which must match exactly as a single
 *  "--help"    token.
 *
 *  "-I:"       Option with argument. The next token, or suffix of
 *              this token, is passed as argument to callback. Matches
 *              both -Ifoo and -I foo.
 *
 *  "-std="     Option with argument which must not be preceeded by any
 *  "-W<"       whitespace. Matches -std=c89 and -Wall, but not
 *              -std= c89 or -W all.
 *
 *  "foo.c"     Arguments without preceeding dash is matched by NULL
 *              rule.
 */
struct option {
    const char *rule;
    int (*callback)(const char *);
};

/*
 * Parse command line arguments according to option specification, and
 * return the number of tokens consumed in the process.
 *
 * Last element of optv must have NULL as rule, and provides a callback
 * for arguments that do not match anything.
 *
 * Abort if a callback produces a non-zero value, returning that value.
 * Return 0 if all arguments were processed successfully.
 */
INTERNAL int parse_args(struct option *optv, int argc, char *argv[]);

#endif
