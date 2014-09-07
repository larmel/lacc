#ifndef LCC_H
#define LCC_H

#include <stdio.h>
#include <stdlib.h>

void init_preprocessing(FILE *, const char *);
int getprepline(char **);

typedef enum token_type
{
	/* Keyword */
	AUTO,
	BREAK,
	CASE,
	CHAR,
	CONST,
	CONTINUE,
	DEFAULT,
	DO,
	DOUBLE,
	ELSE,
	ENUM,
	EXTERN,
	FLOAT,
	FOR,
	GOTO,
	IF,
	INT,
	LONG,
	REGISTER,
	RETURN,
	SHORT,
	SIGNED,
	SIZEOF,
	STATIC,
	STRUCT,
	SWITCH,
	TYPEDEF,
	UNION,
	UNSIGNED,
	VOID,
	VOLATILE,
	WHILE,

	INTEGER,
	IDENTIFIER,
	STRING,

	DOTS, /* ... */

	/* single character */
	OPEN_PAREN = '(',
	CLOSE_PAREN = ')',
	SEMICOLON = ';',
	OPEN_CURLY = '{',
	CLOSE_CURLY = '}',
	OPEN_BRACKET = '[',
	CLOSE_BRACKET = ']',
	COMMA = ',',
	DOT = '.',
	ASSIGN = '=',
	STAR = '*',
	
} token_t;

struct token
{
	enum token_type type;
	/* Depending on token type, store a reference to an integer, char, string,
	 * or other compile time constant. */
	void *value;
};

int get_token(FILE *input, struct token* t);


/* parsing */

typedef struct node {
	char *text;
	struct token token;
	struct node **children;
	size_t nc;
} node_t;

struct node * parse(FILE *);

#endif
