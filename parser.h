
#include <stdlib.h>

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef PARSER_H_
#define PARSER_H_


/* Constants */

#define NO_ATTR		(-1)
#define P_TRUE		1
#define P_FALSE		0

/* keyword table indices */
#define	ELSE		0
#define	FALSE		1
#define	IF			2
#define	PLATYPUS	3
#define	READ		4
#define	REPEAT		5
#define	THEN		6
#define	TRUE		7
#define	WHILE		8
#define	WRITE		9


/* Global Variables */

extern char* kw_table[];	/* keyword lookup table - from table.h */
extern int line;			/* current source line number - from scanner.c */
extern pBuffer str_LTBL;	/* string literal table  - from platy.c*/

static Token lookahead;
int synerrno = 0;


/* Function declarations */

/* Finds and returns next token in scanner buffer - from scanner.c */
extern Token malar_next_token(void); 

void parser(void);
void match(int,int);
void syn_eh(int);
void syn_printe();
void gen_incode(char*);
void program(void);

void opt_statements(void);
void statements(void);
void statements_i(void);
void statement(void);

void assignment_statement(void);
void assignment_expression(void);

void selection_statement(void);

void iteration_statement(void);
void pre_condition(void);

void input_statement(void);
void opt_variable_list(void);
void variable_list(void);
void variable_list_i(void);
void variable_identifier(void);

void output_statement(void);
void output_list(void);

void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_i(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_i(void);
void primary_arithmetic_expression(void);

void string_expression(void);
void string_expression_i(void);
void primary_string_expression(void);

void conditional_expression(void);
void logical_OR_expression(void);
void logical_OR_expression_i(void);
void logical_AND_expression(void);
void logical_AND_expression_i(void);

void relational_expression(void);
void a_relational_operation(void);
void s_relational_operation(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);

#endif