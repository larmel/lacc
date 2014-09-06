#include "lcc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern size_t line_number;

static const char * describe(enum token_type);
static void output_tree(int indent, struct node *tree);

int main(int argc, char* argv[])
{
    char *line;
    FILE *input = stdin;
    const char *filename;
    char *directory = ".";
    struct token t;

    if (argc == 2) {
        filename = argv[1];
        FILE *f;
        char *lastsep = strrchr(filename, '/');
        if (lastsep != NULL) {
            directory = calloc(lastsep - filename + 1, sizeof(char));
            strncpy(directory, filename, lastsep - filename);
        }
        if (f = fopen(filename, "r")) {
            input = f;
        }
    }
    printf("dir: %s\n", directory);
    printf("file: %s\n\n", filename);

    char *filebuf;
    size_t filesize;
    FILE *p = open_memstream(&filebuf, &filesize);

    init_preprocessing(input, directory);

    /* preprocess to temp file buffer */
    while (getprepline(&line) != -1) {
        fputs(line, p);
        printf("%03d  %s", (int)line_number, line);
    }

    /* parse */
    node_t *tree = parse(p);

    puts("");
    output_tree(0, tree);
    puts("");

    /* tokenize */
    //while (get_token(p, &t)) {
    //  printf("token( %s, %s )\n", (char*) t.value, describe(t.type));
    //}

    fclose(p);

    return EXIT_SUCCESS;
}

static const char *
describe(enum token_type t)
{
    if (   t == AUTO || t == BREAK || t == CASE || t == CHAR 
        || t == CONST || t == CONTINUE || t == DEFAULT || t == DO
        || t == DOUBLE || t == ELSE || t == ENUM || t == EXTERN
        || t == FLOAT || t == FOR || t == GOTO || t == IF
        || t == INT || t == LONG || t == REGISTER || t == RETURN
        || t == SHORT || t == SIGNED || t == SIZEOF || t == STATIC
        || t == STRUCT || t == SWITCH || t == TYPEDEF || t == UNION
        || t == UNSIGNED || t == VOID || t == VOLATILE || t == WHILE
       )
        return "keyword";
    if (t == INTEGER)
        return "number";
    if (t == IDENTIFIER)
        return "identifier";
    if (t == STRING)
        return "string";
    return "";
}

/* Print parse tree in human readable format */
static void 
output_tree(int indent, struct node *tree)
{
    int i;
    if (tree == NULL) {
        printf("%*s(null)", indent, "");
        return;
    }
    printf("%*s(%s", indent, "", tree->text);
    if (tree->nc > 0) {
        printf("\n");
        for (i = 0; i < tree->nc; ++i) {
            output_tree(indent + 2, tree->children[i]);
            if (i < tree->nc - 1)
                printf("\n");
        }
    }
    if (tree->token.value != NULL) {
        printf(" \"%s\"", (char *)tree->token.value);
    }
    printf(")");
}
