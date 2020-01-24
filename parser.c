/*	File name: parser.c
 *	Compiler: MS Visual Studio 2019
 *	Authors: 
 *	-- Brady McIntosh SN# 040706980
 *	-- Phillip Clarke SN# 040832994
 *	Date: 2019 Dec 05
 *	Professor: Sv. Ranev
 *	Purpose: Grammar implementation using tokens
 *	Function List: parser(), match(), syn_eh, syn_printe(), gen_incode(),
		program(), opt_statements(), statements(), statements_i(), statement(),
		assignment_statement(), assignment_expression(), selection_statement(),
		iteration_statement(), pre_condition(), input_statement(),
		opt_variable_list(), variable_list(), variable_list_i(),
		variable_identifier(), output_statement(), output_list(),
		arithmetic_expression(), unary_arithmetic_expression(),
		additive_arithmetic_expression(), additive_arithmetic_expression_i(),
		multiplicative_arithmetic_expression(),
		multiplicative_arithmetic_expression_i(), primary_arithmetic_expression(),
		string_expression(), string_expression_i(), primary_string_expression(),
		conditional_expression(), logical_OR_expression(),
		logical_OR_expression_i(), logical_AND_expression(),
		logical_AND_expression_i(), relational_expression(), 
		a_relational_expression(), s_relational_expression(),
		primary_a_relational_expression(), primary_s_relational_expression()

 */

#include "parser.h"


/* Function definitions */

void parser(void)
{
	lookahead = malar_next_token();
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*  Purpose: Match the code and attribute of a given token.
 *  Authors:
 *	-- Brady McIntosh
 *	-- Phillip Clarke
 *  History: 1.0 @ 02Dec2019;
 *  Called functons:
 *	-- malar_next_token()
 *	-- syn_eh()
 *  Parameters:
 *	-- int pr_token_code: code of required token
 *	-- int pr_token_attribute: attribute of required token
 *  Return value: nothing
 *  Algorithm:
 *	-- Checks if lookahead (current token) matches token required by parser
 *	-- If it's a match, increment lookahead once with malar (if != SEOF)
 *	-- If NOT a match, call error handler syn_eh()
 */
void match(int pr_token_code, int pr_token_attribute)
{

	int MATCH = P_FALSE;
	/* this function matches two tokens:
		the current token (lookahead), and
		the token required by the parser (pr_token_code) */

		/* only use attribute code if it equals one of the following:
			KW_T, LOG_OP_T, ART_OP_T, REL_OP_T	*/

	if (pr_token_code == KW_T || pr_token_code == LOG_OP_T ||
		pr_token_code == ART_OP_T || pr_token_code == REL_OP_T)
	{
		if (lookahead.code == pr_token_code &&
			lookahead.attribute.get_int == pr_token_attribute)
		{
			MATCH = P_TRUE;
		}
	}

	else {
		if (lookahead.code == pr_token_code)
		{
			MATCH = P_TRUE;
		}
	}

	if (MATCH)
	{
		if (lookahead.code == SEOF_T)
		{
			/* if match is successful and lookahead == SEOF_T,
				the function returns */
			return;
		}

		else {
			/* if match is successful and lookahead != SEOF_T
			the function advances to the next input token
			by executing the statement:
				lookahead = malar_next_token(); */

			lookahead = malar_next_token();

			if (lookahead.code == ERR_T)
			{
				/* if the new lookahead token is ERR_T, the function
					calls the error printing function syn_printe(),
					advances to the next input token with malar again,
					increments the error counter synerrno */

				syn_printe();
				lookahead = malar_next_token();
				++synerrno;
			}
		}
	}

	else {
		/* if the match is unsuccessful,
			the function calls the error handler syn_eh(pr_token_code),
			and returns	*/

		syn_eh(pr_token_code);
	}
}

/*  Purpose: Panic-mode: finds the next available token matching the kind given.
 *  Authors:
 *	-- Brady McIntosh
 *	-- Phillip Clarke
 *  History: 1.0 @ 02Dec2019;
 *  Called functons:
 *	-- malar_next_token()
 *  Parameters:
 *	-- int pr_token_code: code of required token
 *  Return value: nothing
 *  Algorithm:
 *	-- Increments syserrno (number of errors)
 *	-- Increments lookahead with malar until SEOF or required token is found
 */
void syn_eh(int sync_token_code)
{
	/* this function implements a simple panic-mode error recovery */

	/* first the function calls syn_printe() and increments the error counter,
		then implements a panic mode recovery:
		the function advances the input token (lookahead) until it finds a token
		code matching the one required by the parser (argument). */

	int MATCH = P_FALSE;

	syn_printe(sync_token_code);
	++synerrno;

	while (MATCH == P_FALSE)
	{
		/* possible while advancing to reach the end of the source file.
			to prevent overrun, before every advance, check for end-of-file.
			if the function is looking for an argument other than SEOF_T and
			reaches SEOF, call exit(synerrno). */

		if (lookahead.code == sync_token_code)
		{
			MATCH = P_TRUE;
		}

		else {
			if (lookahead.code == SEOF_T)
			{
				exit(synerrno);
			}
		}

		lookahead = malar_next_token();
	}

	/* if a matching token is found and it is SEOF_T,
		return. */
	return;
}

/*  Purpose: Error printing function
 *  Authors:
 *	-- Svillen Ranev
 */
void syn_printe(void)
{
	/* error printing function for Assignment 3 (Parser), F19 */
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		b_mark(str_LTBL, t.attribute.str_offset);
		printf("%s\n", b_location(str_LTBL));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}

/*  Purpose: Print the argument string and a newline.
 *  Authors:
 *	-- Brady McIntosh
 *	-- Phillip Clarke
 */
void gen_incode(char* str)
{
	/* Part 1: takes a string and prints it. */

	printf("%s\n", str);

	/* Part 2 (bonus): produce machine code */
}

/*             */
/* Productions */
/*             */

/* <program> ->
 *		PLATYPUS { <opt_statements> };
 *	
 *	FIRST(<program>) = {
 *		PLATYPUS
 *	}
 *	
 *	Author: Svillen Ranev
 */
void program(void)
{
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR);
	opt_statements(); match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/* <opt_statements> ->
 *		<statements> | e
 *	
 *	FIRST(<opt_statements>) = {
 *		AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e
 *	}
 *	
 *	Author: Brady McIntosh
 */
void opt_statements(void) {
		/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statements();
			break;
		}
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/* <statements> ->
 *		<statement> <statements_i>
 *	
 *	FIRST(<statements>) = {
 *		AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)
 *	}
 *	
 *	Author: Brady McIntosh
 */
void statements(void)
{
	statement(); statements_i();
}

/* <statements_i> ->
 *		<statement> <statements_i> | e
 *	
 *	FIRST(<statements_i>) = {
 *		AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e
 *	}
 *	
 *	Author: Brady McIntosh
 */
void statements_i(void)
{
	int LOOP = P_TRUE;
		/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statement(); break;
	case KW_T:
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statement();
			break;
		}
	default: LOOP = P_FALSE; break;
	}

	if (LOOP)
	{
		statements_i();
	}
}

/* <statement> ->
 *		  <assignment statement>
 *		| <selection statement>
 *		| <iteration statement>
 *		| <input statement>
 *		| <output statement>
 *	
 *	FIRST(<statement>) = {
 *		AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)
 *	}
 *	
 *	Author: Brady McIntosh
 */
void statement(void)
{	/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: assignment_statement(); break;
	case KW_T:
		switch (lookahead.attribute.get_int) {
		case IF: selection_statement(); break;
		case WHILE: iteration_statement(); break;
		case READ: input_statement(); break;
		case WRITE: output_statement(); break;
		} break;
	case RBR_T: syn_printe();
	}
}

/* <assignment statement> ->
 *		<assignment expression>;
 *
 *	FIRST(<assignment statement>)={
 *		AVID_T, SVID_T
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void assignment_statement(void)
{/*Runs assignment_expression and a match function
 to satisfy assignment statement grammar*/
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}


/* <assignment expression> ->
 *		  AVID = <arithmetic expression>
 *		| SVID = <string expression>
 *
 *	FIRST(<assignment expression>)={
 *		AVID_T, SVID_T
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void assignment_expression(void)
{	/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code) {
	case AVID_T: match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR); arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T: match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR); string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	}
}

/* <selection statement> ->
 *		IF <pre-condition> (<conditional expression>) THEN { <opt_statements> }
 *		ELSE { <opt_statements> };
 *
 *	FIRST(<selection statements>)={
 *		KW_T(IF)
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void selection_statement(void)
{/*Runs a sequence of match functions pre_condition, 
	cndtnl_expression, and opt_statements to satisfy 
	selection statement grammar*/
	match(KW_T, IF);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}

/* <iteration statement> ->
 *		WHILE <pre-condition> ( <conditional expression> )
 *		REPEAT { <statements> };
 *
 *	FIRST(<iteration statement>)={
 *		KW_T(WHILE)
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void iteration_statement(void)
{/*Runs a sequence of match functions pre_condition, 
	cndtnl_expression, and statements to satisfy 
	iteration statement grammar*/
	match(KW_T, WHILE);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}

/* <pre-condition> ->
 *		TRUE | FALSE
 *
 *	FIRST(<pre-condition>)={
 *		KW_T(TRUE), KW_T(FALSE)
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void pre_condition(void)
{	/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch(lookahead.code){
	case KW_T:
		/*Switch statement that checks the lookahead attribute value and runs 
			logic based on the value*/
		switch (lookahead.attribute.get_int) {
		case TRUE: match(KW_T, TRUE); /*matches respective case*/
			break;
		case FALSE: match(KW_T, FALSE);
			break;
		}
		break;
	}

}

/* <input statement> ->
 *		READ ( <variable list> );
 *
 *	FIRST(<input statement>)={
 *		KW_T(READ)
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void input_statement(void)
{
	/*Runs a sequence of match functions and variable_list 
		to satisfy input statement grammar*/
	match(KW_T, READ); match(LPR_T, NO_ATTR); variable_list();
	match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/* <opt_variable list> ->
 *		<variable list> | e
 *	
 *	FIRST(<opt_variable list>) = {
 *		AVID_T, SVID_T, e
 *	}
 *	
 *	Author: Brady McIntosh
 */
void opt_variable_list(void)
{	/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: variable_list(); break;
	default: gen_incode("PLATY: Output list (empty) parsed"); break;
	}
}

/* <variable list> ->
 *		<variable identifier> <variable listI>
 *	
 *	FIRST(<variable list>) = {
 *		AVID_T, SVID_T
 *	}
 *	
 *	Author: Brady McIntosh
 */
void variable_list(void)
{
	variable_identifier(); variable_list_i();
}

/* <variable list_i> ->
 *		, <variable list> | e
 *	
 *	FIRST(<variable list_i>) = {
 *		COM_T, e
 *	}
 *	
 *	Author: Brady McIntosh
 */
void variable_list_i(void)
{	/*If statement that checks if lookahead
 code is equal to the value of COM_T*/
	if (lookahead.code == COM_T)
	{
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_i();
	}
	else {
		gen_incode("PLATY: Variable list parsed");
	}
}

/* <variable identifier> ->
 *		AVID_T | SVID_T
 *	
 *	FIRST(<variable identifier>) = {
 *		AVID_T, SVID_T
 *	}
 *	
 *	Author: Brady McIntosh
 */
void variable_identifier(void)
{
	/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code) { 
	case AVID_T: match(AVID_T, NO_ATTR); break; /*If a case is satisfied
	runs the match function for the respective case*/
	case SVID_T: match(SVID_T, NO_ATTR); break;
	default: syn_printe();
	}
}

/* <output statement> ->
 *		WRITE (<output list>);
 *
 *	FIRST(<output statement>)={
 *		KW_T(WRITE)
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void output_statement(void)
{
	/*Runs a sequence of match calls and output_list to satisfy the 
		output_statement grammar*/
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/* <output list> ->
 *		<opt_variable list> | STR_T
 *	
 *	FIRST(<output list>)={
 *		AVID_T, SVID_T, STR_T, e
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void output_list(void)
{
	/*Checks if lookahead.code value matches STR_T
	and runs opt_variable_list if it doesnt*/
	if (lookahead.code == STR_T) 
	{
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
	}
	else {
		opt_variable_list();
	}
}

/* <arithmetic expression> ->
 *		  <unary arithmetic expression>  
 *		| <additive arithmetic expression>	
 *
 *	
 *	FIRST(<arithmetic expression>) = {
 *		ART_OP_T(PLUS), ART_OP_T(MINUS), AVID_T, FPL_T, INL_T, LBR_T
 *	}
 *	
 *	Author: Brady McIntosh
 */
void arithmetic_expression(void)
{	/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T: additive_arithmetic_expression(); break;
	case ART_OP_T:
		switch (lookahead.attribute.get_int) {
		case PLUS:
		case MINUS: unary_arithmetic_expression(); break;
		}
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}

/* <unary arithmetic expression> ->
 *		  - <primary arithmetic expression>
|*		| + <primary arithmetic expression>
 *
 *	
 *	FIRST(<unary arithmetic expression>) = {
 *		ART_OP_T(PLUS), ART_OP_T(MINUS)
 *	}
 *	
 *	Author: Brady McIntosh
 */
void unary_arithmetic_expression(void)
{	/*Switch statement that checks the lookahead attribute value and runs logic 
		based on the value*/
	switch (lookahead.attribute.get_int) {
	case PLUS: match(ART_OP_T, PLUS); 
		primary_arithmetic_expression(); break;
	case MINUS: match(ART_OP_T, MINUS); 
		primary_arithmetic_expression(); break;
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}

/* <additive arithmetic expression> ->
 *		<multiplicative arithmetic expression> <additive arithmetic expressionI>
 *	
 *	FIRST(<additive arithmetic expression>) = {
 *		AVID_T, FPL_T, INL_T, LPR_T
 *	}
 *	
 *	Author: Brady McIntosh
 */
void additive_arithmetic_expression(void)
{
	multiplicative_arithmetic_expression(); 
	additive_arithmetic_expression_i();
}

/* <additive arithmetic expression_i> ->
 *		  + <additive arithmetic expression>
 *		| - <additive arithmetic expression>
 *		| e
 *
 *	
 *	FIRST(<additive arithmetic expression_i>) = {
 *		ART_OP_T(PLUS), ART_OP_T(MINUS), e
 *	}
 *	
 *	Author: Brady McIntosh
 */
void additive_arithmetic_expression_i(void)
{	/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code) {
	case ART_OP_T:
			/*Switch statement that checks the lookahead attribute value and runs logic 
		based on the value*/
		switch (lookahead.attribute.get_int) {
		case PLUS: match(ART_OP_T, PLUS); additive_arithmetic_expression();
			gen_incode("PLATY: Additive arithmetic expression parsed"); break;
		case MINUS: match(ART_OP_T, MINUS); additive_arithmetic_expression();
			gen_incode("PLATY: Additive arithmetic expression parsed"); break;
		}
	}
}

/* <multiplicative arithmetic expression> ->
 *		<primary arithmetic expression> <multiplicative arithmetic expressionI>
 *	
 *	FIRST(<multiplicative arithmetic expression>) = {
 *		AVID_T, FPL_T, INL_T, LPR_T
 *	}
 *	
 *	Author: Brady McIntosh
 */
void multiplicative_arithmetic_expression(void)
{
	primary_arithmetic_expression(); 
	multiplicative_arithmetic_expression_i();
}

/* <multiplicative arithmetic expression_i> ->
 *		  * <multiplicative arithmetic expression> 
 *		| / <multiplicative arithmetic expression>
 *		| e
 *
 *	
 *	FIRST(<multiplicative arithmetic expression_i>) = {
 *		ART_OP_T(MULT), ART_OP_T(DIV), e
 *	}
 *	
 *	Author: Brady McIntosh
 */
void multiplicative_arithmetic_expression_i(void)
{
	if (lookahead.code == ART_OP_T && lookahead.attribute.get_int == MULT)
	{
		match(ART_OP_T, MULT);
		multiplicative_arithmetic_expression();
		gen_incode("PLATY: Multiplicative arithmetic expression parsed");
	}
	if (lookahead.code == ART_OP_T && lookahead.attribute.get_int == DIV)
	{
		match(ART_OP_T, DIV);
		multiplicative_arithmetic_expression();
		gen_incode("PLATY: Multiplicative arithmetic expression parsed");
	}
}

/* <primary arithmetic expression> ->
 *		( <arithmetic expression> )	
 *	
 *	FIRST(<primary arithmetic expression>) = {
 *		AVID_T, FPL_T, INL_T, LPR_T
 *	}
 *	
 *	Author: Brady McIntosh
 */
void primary_arithmetic_expression(void)
{		/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code) {
	case AVID_T:match(AVID_T, NO_ATTR); break;
	case FPL_T:match(FPL_T, NO_ATTR); break;
	case INL_T:match(INL_T, NO_ATTR); break;
	case LPR_T: match(LPR_T, NO_ATTR);
		arithmetic_expression(); match(RPR_T, NO_ATTR);
		break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/* <string expression> ->
 *		<primary string expression> <string expressionI>
 *	
 *	FIRST(<string expression>) = {
 *		SVID_T, STR_T
 *	}
 *	
 *	Author: Brady McIntosh
 */
void string_expression(void)
{
	primary_string_expression(); string_expression_i();
}

/* <string expression_i> ->
 *		<< <string expression> | e
 *	
 *	FIRST(<string expression_i>) = {
 *		SCC_OP_T, e
 *	}
 *	
 *	Author: Brady McIntosh
 */
void string_expression_i(void)
{/*If statement that checks the lookahead code
 value to see if it matches the value of SCC_OP_T*/
	if (lookahead.code == SCC_OP_T)
	{
		match(SCC_OP_T, NO_ATTR);
		string_expression();
	}
	else {
		gen_incode("PLATY: String expression parsed");
	}
}

/* <primary string expression> ->
 *		SVID | STR_T
 *	
 *	FIRST(<primary string expression>) = {
 *		SVID_T, STR_T
 *	}
 *	
 *	Author: Brady McIntosh
 */
void primary_string_expression(void)
{	/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code)
	{
	case SVID_T: match(SVID_T, NO_ATTR); 
		gen_incode("PLATY: Primary string expression parsed"); 
		break;
	case STR_T: match(STR_T, NO_ATTR); 
		gen_incode("PLATY: Primary string expression parsed"); 
		break;
	default: syn_printe();
	}
}

/* <conditional expression> ->
 *		<logical OR expression>
 *
 *	FIRST(<conditional expression>)={
 *		AVID_T, FPL_T, INL_T, SVID_T, STR_T
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void conditional_expression(void)
{
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/* <logical OR expression> ->
 *		<logical AND expression> <logical OR expressionI>
 *
 *	FIRST(<logical_OR_expression>)={
 *		AVID_T, FPL_T, INL_T, SVID_T, STR_T
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void logical_OR_expression(void)
{
	logical_AND_expression();
	logical_OR_expression_i();
}

/* <logical OR expression_i> ->
 *		.OR. <logical OR expression>
 *		| e
 *
 *	FIRST(<logical OR expression_i>)={
 *		LOG_OP_T(OR), e
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void logical_OR_expression_i(void)
{
	/*Checks if the lookahead.code and attribute value match LOG_OP_T and OR*/
	if (lookahead.code == LOG_OP_T && lookahead.attribute.get_int == OR) 
	{
		match(LOG_OP_T, OR);
		logical_OR_expression();
		gen_incode("PLATY: Logical OR expression parsed");
	}
}

/* <logical AND expression> ->
 *		<relational expression> <logical AND expressionI>	
 *
 *	FIRST(<logical AND expression>)={
 *		AVID_T, FPL_T, INL_T, SVID_T, STR_T
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void logical_AND_expression(void)
{
	relational_expression();
	logical_AND_expression_i();
}

/* <logical AND expression i> ->
 *		.AND. <logical AND expression>
 *		| e
 *
 *	FIRST(<logical_AND_expression_i>)={
 *		LOG_OP_T(AND), e
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void logical_AND_expression_i(void)
{
	/*Checks if the lookahead.code and attribute value match LOG_OP_T and AND*/
	if (lookahead.code == LOG_OP_T && lookahead.attribute.get_int == AND) 
	{
		match(LOG_OP_T, AND);
		logical_AND_expression();
		gen_incode("PLATY: Logical AND expression parsed");
	}
}

/* <relational expression> ->
 *		  <primary a_relational expression> <a_relational operation>
 *		| <primary s_relational expression> <s_relational operation>
 *
 *	FIRST(<relational expression>)={
 *		AVID_T, FPL_T, INL_T, SVID_T, STR_T
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void relational_expression(void)
{
	/*Switch statement that checks the lookahead attribute value and runs 
		logic based on the value*/
	switch (lookahead.code) {
		/*if there's a case match, prmry_a_rltnl_expression is called, 
			a_rltnl_operation is called and the switch is broken*/
	case AVID_T:
	case FPL_T:
	case INL_T: primary_a_relational_expression(); 
		a_relational_operation(); break; 
		/*if there's a case match, prmry_s_rltnl_expression is called, 
			s_rltnl_operation is called and the switch is broken*/
	case SVID_T:
	case STR_T: primary_s_relational_expression(); 
		s_relational_operation(); break; 
	default: syn_printe(); break;
	}
	gen_incode("PLATY: Relational expression parsed");
}

/* <a_relational operation> ->
 *		  == <primary a_relational expression>
 *		| <> <primary a_relational expression>
 *		| > <primary a_relational expression>
 *		| < <primary a_relational expression> 
 *
 *	FIRST(<a_relational operation>)={
 *		REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(GT), REL_OP_T(LT)
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void a_relational_operation(void)
{
	/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code)
	{
	case REL_OP_T:
		/*Switch statement that checks the lookahead attribute value and runs 
			logic based on the value*/
		switch (lookahead.attribute.get_int)
		{
			/*if there's a case match, the function match is called for the 
				respective match, prmry_a_rltnl_expression is called and the 
				switch is broken*/
		case EQ: match(REL_OP_T, EQ);
			primary_a_relational_expression(); break;
		case NE: match(REL_OP_T, NE);
			primary_a_relational_expression(); break;
		case GT: match(REL_OP_T, GT);
			primary_a_relational_expression(); break;
		case LT: match(REL_OP_T, LT);
			primary_a_relational_expression(); break;
		}
		break;
	default: syn_printe(); break;
	}
}

/* <s_relational operation> ->
 *		  == <primary s_relational expression>
 *		| <> <primary s_relational expression>
 *		| > <primary s_relational expression>
 *		| < <primary s_relational expression> 
 *
 *	FIRST(<s_relational operation>)={
 *		REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(GT), REL_OP_T(LT)
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void s_relational_operation(void)
{
	/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code)
	{
	case REL_OP_T:
		/*Switch statement that checks the lookahead attribute value and runs 
			logic based on the value*/
		switch (lookahead.attribute.get_int)
		{
			/*if there's a case match, the function match is called for the 
				respective match, prmry_s_rltnl_expression is called and the 
				switch is broken*/
		case EQ: match(REL_OP_T, EQ);
			primary_s_relational_expression();
			break;

		case NE: match(REL_OP_T, NE);
			primary_s_relational_expression();
			break;

		case GT: match(REL_OP_T, GT);
			primary_s_relational_expression();
			break;

		case LT: match(REL_OP_T, LT);
			primary_s_relational_expression();
			break;
		}
		break;
	default: syn_printe(); break;
	}
}

/* <primary a_relational expression> ->
 *		AVID | FPL_T | INL_T
 *
 *	FIRST(<primary a_relational expression>)={
 *		AVID_T, FPL_T, INL_T
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void primary_a_relational_expression(void)
{
	/*Switch statement that checks the lookahead code value and runs logic 
		based on the value*/
	switch (lookahead.code) { 
		/*if there's a case match, the function match is called and the switch 
			is broken*/
	case AVID_T:match(AVID_T, NO_ATTR); break;
	case FPL_T:match(FPL_T, NO_ATTR); break;
	case INL_T:match(INL_T, NO_ATTR); break;
	default: syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/* <primary s_relational operation> ->
 *		<primary string expression>
 *
 *	FIRST(<primary s_relational expression>)={
 *		SVID_T, STR_T
 *	} 
 *	
 *	Author: Phillip Clarke
 */
void primary_s_relational_expression(void)
{
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}