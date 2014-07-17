#ifndef LCC_H
#define LCC_H

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

	/* single character */
	OPEN_PAREN = '(',
	CLOSE_PAREN = ')',
	SEMICOLON = ';',
	OPEN_CURLY = '{',
	CLOSE_CURLY = '}'
	
} token_t;

struct token
{
	enum token_type type;
	/* Depending on token type, store a reference to an integer, char, string,
	 * or other compile time constant. */
	void *value;
};

int
get_token(FILE *input, struct token* t);

void
print_token(struct token* t);

#endif
