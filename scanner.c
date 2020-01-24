/*	File name: scanner.c
 *	Compiler: MS Visual Studio 2019
 *	Authors: 
 *	-- Brady McIntosh SN# 040706980
 *	-- Phillip Clarke SN# 040832994
 *	Date: 2019 Nov 12
 *	Professor: Sv. Ranev
 *	Purpose: Lexical analysis of a character buffer, returning tokens.
 *	Function List:
 *	-- scanner_init()
 *	-- malar_next_token()
 *	-- get_next_state()
 *	-- char_class()
 *	-- aa_func02()
 *	-- aa_func03()
 *	-- aa_func05()
 *	-- aa_func08()
 *	-- aa_func10()
 *	-- aa_func11()
 */

 /* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
  * to suppress the warnings about using "unsafe" functions like fopen()
  * and standard sting library functions defined in string.h.
  * The define does not have any effect in Borland compiler projects.
  */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

  /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Constants */

#define S_FALSE	0	/* false return value */
#define S_TRUE	1	/* true return value */

#define C_EOF_0		0
#define C_EOF_255	255

#define ERR_STR_OFF	3	/* cutoff size for over-length strings */

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
static int iskeyword(char* kw_lexeme); /*keywords lookup functuion */


/*Initializes scanner */
int scanner_init(pBuffer psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*  Purpose: Find and return the next token in the scanner buffer.
 *  Authors: 
 *	-- Brady McIntosh
 *	-- Phillip Clarke
 *  History: 0.1 @ 8Nov2019; 1.0 @ 10Nov2019;
 *  Called functons:
 *	-- b_clear()
 *	-- b_mark()
 *	-- b_getcoffset()
 *	-- b_getc()
 *	-- b_retract()
 *	-- b_reset()
 *	-- get_next_state()
 *	-- b_allocate()
 *	-- b_addc()
 *	-- b_compact()
 *	-- aa_table[]()
 *  Parameters:
 *	-- void
 *  Return value: Token - token containing lexical and contextual information
 *  Algorithm:
 *	-- First, find simpler tokens using dedicated conditional logic
 *	-- Second, find more complex tokens using finite state machine
 *	-- Return Token
 */
Token malar_next_token(void)
{
	/* local variables */

	Token t = { 0 };	/* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c;	/* input symbol */
	int state = 0;		/* initial state of the FSM */
	short lexstart;		/* start offset of a lexeme in the input char buffer (array) */
	short lexend;		/* end offset of a lexeme in the input char buffer (array)*/
	int i;				/* loop index */

	
	/* main loop */
	while (1) { 

		/* get next character */
		b_clear(lex_buf);

		/* mark buffer at current index, and get next char */
		b_mark(sc_buf, b_getcoffset(sc_buf));
		c = b_getc(sc_buf);
		/*                      */
		/* TOKEN-DRIVEN SCANNER */
		/*                      */

		/* Author: Phillip Clarke */
		/* Version: 1.0 */
		/* Date: 08 Nov 2019 */
		/*Block that checks for SEOF and returns the appropriate token*/
		if (c == C_EOF_0 || c == C_EOF_255)
		{

			t.code = SEOF_T;
			if (c == C_EOF_0)
			{
				t.attribute.seof = SEOF_0;
			}
			else if (c == C_EOF_255)
			{
				t.attribute.seof = SEOF_EOF;
			}
			return t;
		}
		/*Block that checks for whitespaces, verticle and horizontal tabs and bypasses them*/
		if (c == ' ' || c == '\t' || c == '\v') continue;
		/*Block that checks for newline and increments the line number then continues*/
		if (c == '\n')
		{
			line++;
			continue;
		}
		/*Block that checks for carriage return*/
		if (c == '\r')
		{
			c = b_getc(sc_buf);/*Gets a new character to check the following logic*/
			if (c == '\n')/*Block that checks if the new character is a newline and if so, increments line and continues*/
			{
				line++;
				continue;
			}
			/*else*/
			b_retract(sc_buf);/*retracts sc_buf*/
			line++; /*increments line*/
			continue;
		}
		/*Block that checks for ! and checks the next character for another ! to detect a comment and returns the appropriate token*/
		if (c == '!')
		{
			c = b_getc(sc_buf);
			char last = c;

			while (1) /*Loop that bypasses the remainder of the current line by breaking only when a newline or carriage return is found*/
			{
				if (c == C_EOF_0 || c == C_EOF_255)
				{
					b_retract(sc_buf);
					t.code = ERR_T;
					t.attribute.err_lex[0] = '!';
					t.attribute.err_lex[1] = last;
					t.attribute.err_lex[2] = '\0';
					return t;
				}

				if (c == '\n')
				{
					break;
				}
				if (c == '\r')
				{
					break;
				}
				c = b_getc(sc_buf);
			}
			if (c == '\n' || c == '\r')
			{
				b_retract(sc_buf);
			}

			if (last == '!')/*Detects if the second character is ! detecting a comment and continues*/
			{
				continue;
			}
			else /*returns an error token due to improper comment syntax*/
			{
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = last;
				t.attribute.err_lex[2] = '\0';
				return t;
			}
		}
		if (c == '.')/*Block that runs if c == '.' to detect logical operators*/
		{
			c = b_getc(sc_buf);
			if (c == 'A')
			{
				c = b_getc(sc_buf);
				if (c == 'N')
				{
					c = b_getc(sc_buf);
					if (c == 'D')
					{
						c = b_getc(sc_buf);
						if (c == '.')/*If this point is reached - the block satisfies the logical operator token and returns the appropriate token*/
						{
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;
							return t;
						}
					}
				}
			}
			if (c == 'O')
			{
				c = b_getc(sc_buf);
				if (c == 'R')
				{
					c = b_getc(sc_buf);
					if (c == '.')/*If this point is reached - the block satisfies the logical operator token and returns the appropriate token*/
					{
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
					}
				}
			}
			
			b_reset(sc_buf); /*Resets back to the mark  and returns an error due to improper logical operator syntax*/
			c = b_getc(sc_buf);

			t.code = ERR_T;
			t.attribute.err_lex[0] = c;
			t.attribute.err_lex[1] = '\0';
			return t;
		}

		if (c == '{')/*Block that checks for { returns appropriate token*/
		{
			t.code = LBR_T; /*no attribute */ return t;
		}
		if (c == '}') {/*Block that checks for } returns appropriate token*/
			t.code = RBR_T; /*no attribute */ return t;
		}
		if (c == '+') {/*Block that checks for + returns appropriate token*/
			t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t;
		}
		if (c == '-') {/*Block that checks for - returns appropriate token*/
			t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t;
		}
		if (c == '/') {/*Block that checks for / returns appropriate token*/
			t.code = ART_OP_T; t.attribute.arr_op = DIV; return t;
		}
		if (c == '*') {/*Block that checks for * returns appropriate token*/
			t.code = ART_OP_T; t.attribute.arr_op = MULT; return t;
		}
		if (c == ';') {/*Block that checks for ; returns appropriate token*/
			t.code = EOS_T; /*No attribute*/ return t;
		}
		if (c == '=')/*Block that checks for = returns appropriate token*/
		{
			c = b_getc(sc_buf);
			if (c == '=')/*Checks if next character is another = if so, it detects a relational operator and returns the appropriate token*/
			{
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			else/*retracts and assigns the assignment operator token*/
			{
				b_retract(sc_buf);
				t.code = ASS_OP_T; /*No attribute*/ return t;
			}
		}
		if (c == '(')/*Block that checks for ( returns appropriate token*/
		{
			t.code = LPR_T; /*No attribute*/ return t;
		}
		if (c == ')')/*Block that checks for ) returns appropriate token*/
		{
			t.code = RPR_T; /*No attribute*/ return t;
		}
		if (c == ',')/*Block that checks for , returns appropriate token*/
		{
			t.code = COM_T; /*No attribute*/ return t;
		}
		if (c == '>')/*Block that checks for > returns appropriate token*/
		{
			t.code = REL_OP_T; t.attribute.rel_op = GT; return t;
		}
		if (c == '<')/*Block that checks for < returns appropriate token*/
		{
			c = b_getc(sc_buf);
			if (c == '>')/*Gets the next character and checks if it's >, if it is - it's a relational operator and returns appropriate token*/
			{
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			}
			else if (c == '<')/*If the next character is <, detects concatination operator and returns appropriate token*/
			{
				t.code = SCC_OP_T;
				/*No attribute*/
				return t;
			}
			else/*If neither of the above, returns relational operator token*/
			{
				b_retract(sc_buf);
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				return t;
			}
		}

		/*                      */
		/* FINITE STATE MACHINE */
		/*                      */

		/* Author: Brady McIntosh */
		/* Version: 1.1 */
		/* Date: 10 Nov 2019 */

		/* set lexeme start & end mark */
		/* this could be 'lexstart = b_getcoffset(sc_buf)' */
		b_reset(sc_buf);
		lexstart = b_mark(sc_buf, b_getcoffset(sc_buf));

		/* build token character-by-character, as long as state is non-accepting */

		c = b_getc(sc_buf);

		while (as_table[state = get_next_state(state,c)] == NOAS)
		{
			/* read input character */
			c = b_getc(sc_buf);
		}

		/* retract buffer before accept, if accepting state is with retract */
		if (as_table[state] == ASWR)
		{
			b_retract(sc_buf);
		}

		lexend = b_getcoffset(sc_buf);

		/* allocate temporary lexeme buffer */
		lex_buf = b_allocate(lexend - lexstart, 0, 'f');
		
		/* reset buffer getc to mark */
		b_reset(sc_buf);

		/* copy lexeme from input buffer into temporary buffer */
		for (i = lexstart; i < lexend; ++i)
		{
			b_addc(lex_buf, b_getc(sc_buf));
		}

		b_compact(lex_buf, '\0');

		/* call accepting function according to state index */
		if (aa_table[state] != NULL)
		{
			/* pass internal buffer array as argument */
			/* THIS MAY BE A PROBLEM ! ! ! ! */
			t = aa_table[state](b_location(lex_buf));
		}
		else
		{
			/* do something here if array index is wrong */
			i = -1;
		}

		b_free(lex_buf);
		return t;
	}
}
			

/* do not modify this code */
int get_next_state(int state, char c)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	assert(next != IS);
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	return next;
}

/*  Purpose: Find transition table column index of char
 *  Authors:
 *	-- Phillip Clarke
 *  History: 1.0 @ 8Nov2019;
 *  Called functons:
 *	-- none
 *  Parameters:
 *	-- char c: Symbol read through finite state machine
 *  Return value: int - Index of column in transition table
 *  Algorithm:
 *	-- Determine matching pattern to char using logical representation of regular expressions
 *	-- Return column index
 */
int char_class(char c)
{
	int val;

	/* [A-Za-z] */
	if ((c >= 65 && c <= 90) || (c >= 97 && c <= 122)) {
		val = 0;
	}

	/* [0] */
	else if (c == '0') {
		val = 1;
	}

	/* [1-9] */
	else if (c >= 49 && c <= 57) {
		val = 2;
	}

	/* . */
	else if (c == '.') {
		val = 3;
	}

	/* @ */
	else if (c == '@') {
		val = 4;
	}

	/* " */
	else if (c == '"') {
		val = 5;
	}

	/* SEOF */
	else if (c == C_EOF_0 || c == C_EOF_255) {
		val = 6;
	}

	/* other */
	else {
		val = 7;
	}

	return val;
}



/* accepting functions */

/*  Purpose: Accepting function for Arithmetic Variable ID Token
 *  Authors:
 *	-- Brady McIntosh
 *  History: 1.0 @ 8Nov2019;
 *  Called functons:
 *	-- iskeyword()
 *	-- strlen()
 *  Parameters:
 *	-- char lexeme[]: String representing lexical word found by finite state machine
 *  Return value: Token - token containing lexical and contextual information
 *  Algorithm:
 *	-- Determine if lexeme is a keyword or a Variable ID
 *	-- Return completed Token
 */
Token aa_func02(char lexeme[]) 
{
	Token t;		/* Token return */
	int kw;			/* index of keyword char table */
	int avid_len;	/* length of AVID string */
	int i;			/* loop index */

	/* find index of keyword in table */
	kw = iskeyword(lexeme);

	/* if lexeme is a keyword */
	if (kw != -1)
	{
		t.code = KW_T;
		t.attribute.kwt_idx = kw;

		return t;
	}

	/* if lexeme is not a keyword */
	else
	{
		/* sets token code to AVID */
		t.code = AVID_T;

		/* caps length of AVID string below maximum VID string length */
		avid_len = strlen(lexeme) > VID_LEN ? VID_LEN : (int)strlen(lexeme);

		/* loop to add chars to AVID lexeme */
		for (i = 0; i < avid_len; ++i)
		{
			t.attribute.vid_lex[i] = lexeme[i];
		}

		/* set final available character */
		t.attribute.vid_lex[avid_len] = '\0';

		return t;
	}

}

/*  Purpose: Accepting function for String Variable ID Token
 *  Authors:
 *	-- Phillip Clarke
 *  History: 1.0 @ 05Nov2019;
 *  Called functons:
 *	-- strlen()
 *  Parameters:
 *	-- char lexeme[]: String representing lexical word found by finite state machine
 *  Return value: Token - token containing lexical and contextual information
 *  Algorithm:
 *	-- Sets appropriate token code
 *  -- checks that lexeme length doesn't exceed VID_LEN
 *  -- Loops and builds the attribute vid_lex based on the passed in lexeme
 *	-- Appends @ and \0 to the end of the vid_lex attribute
 *	-- Returns new token
 */
Token aa_func03(char lexeme[])
{
	Token t;/*creates token*/
	t.code = 3;/*sets token code to appropriate value*/
	short temp = (short)strlen(lexeme);
	if (strlen(lexeme) > VID_LEN) /*Code to be run if length of lexeme exceeds VID_LEN value*/
	{
		temp = 0;
		for (int x = 0; x < VID_LEN; x++)/*loop that builds vid_lex based on the passed in string that exceeds the size*/
		{
			t.attribute.vid_lex[x] = lexeme[x];
			temp++;
		}
	}
	else
	{
		for (int x = 0; x < strlen(lexeme); x++)/*loop that builds vid_lex based on passed in string*/
		{
			t.attribute.vid_lex[x] = lexeme[x];
		}
	}
	/*Appends @ then the null string terminator character to the end of the vid_lex string*/
	t.attribute.vid_lex[temp-1] = '@';
	t.attribute.vid_lex[temp] = '\0';


	return t;
}

/*  Purpose: Accepting function for Floating Point Literal Token
 *  Authors:
 *	-- Phillip Clarke
 *  History: 1.0 @ 05Nov2019;
 *  Called functons:
 *	-- strlen()
 *	-- atof()
 *  Parameters:
 *	-- char lexeme[]: String representing lexical word found by finite state machine
 *  Return value: Token - token containing lexical and contextual information
 *  Algorithm:
 *	-- Ensures converted lexeme is within the bounds of a 4 byte float
 *	-- Loops and populates err_lex based on lexeme - appends ... if lexeme size exceeds ERR_LEN
 *	-- Apprends \0 and returns newly created token
 */
Token aa_func08(char lexeme[])
{

	Token t;		/*Creates token*/
	t.code = 4;		/*Sets token to appropriate value*/

	short temp = (short)strlen(lexeme);	/* temporary lexeme length counter */


	/* Checks if lexeme converted to a float is out of the range of a float */
	if (atof(lexeme) > FLT_MAX || atof(lexeme) < -FLT_MAX || 
		atof(lexeme) < FLT_MIN  && atof(lexeme) > 0 ||
		atof(lexeme) > -FLT_MIN && atof(lexeme) < 0)
	{
		/* sets token identifier to error */
		t.code = 0;
		/* Code runs if the length of the string is larger than the size of err_len */
		if (strlen(lexeme) > ERR_LEN)
		{
			temp = 0;
			/* loop that populates the err_lex based on the passed in string */
			for (int x = 0; x < ERR_LEN - ERR_STR_OFF; x++)
			{
				t.attribute.err_lex[x] = lexeme[x];
				temp++;
			}
			for (temp; temp < ERR_LEN; ++temp)
			{
				t.attribute.err_lex[temp] = '.';
			}
		}
		/* Code to be run if lexeme length is within the bounds of ERR_LENs value */
		else
		{
			/* Loop that builds the error string based on the passed in string */
			for (int x = 0; x < temp; x++)
			{
				t.attribute.err_lex[x] = lexeme[x];
			}
			/*Appends the null string terminator character to the end of the error string*/
		}
		t.attribute.err_lex[temp] = '\0';
		return t; /*returns error token*/
	}
	else
	{
		t.attribute.flt_value = (float)atof(lexeme);/*assigns the tokens value to the float converstion of lexeme*/
	}

	return t;
}

/*  Purpose: Accepting function for Decimal Integer Literal Token
 *  Authors:
 *	-- Phillip Clarke
 *  History: 1.0 @ 05Nov2019;
 *  Called functons:
 *	-- strlen()
 *	-- atoi()
 *  Parameters:
 *	-- char lexeme[]: String representing lexical word found by finite state machine
 *  Return value: Token - token containing lexical and contextual information
 *  Algorithm:
 *	-- Sets appropriate token code and ensures lexeme converted to int falls within the bounds of a SHORT
 *	-- Appends ... if lexeme exceeds short and appends \0
 *	-- Returns newly created token
 */
Token aa_func05(char lexeme[])
{


	Token t; /*Creates token*/
	t.code = 5; /*Sets token to appropriate value*/
	short temp = (short)strlen(lexeme);

	/* Checks if lexeme converted to a int is out of the range of an int */
	if (atoi(lexeme) > SHRT_MAX || atoi(lexeme) < SHRT_MIN) 
	{
		t.code = 0; /*sets token identifier to error*/
		if (strlen(lexeme) > ERR_LEN) /*Code runs if the length of the string is larger than the size of err_len*/
		{
			temp = 0;
			for (int x = 0; x < ERR_LEN - ERR_STR_OFF; x++)/*loop that populates the err_lex based on the passed in string*/
			{
				t.attribute.err_lex[x] = lexeme[x];
				temp++;
			}
			for (temp; temp < ERR_LEN; ++temp)/*Loop that appends ... to the end of ERR_LEN*/
			{
				t.attribute.err_lex[temp] = '.';
			}

		}
		else /*Code to be run if lexeme length is within the bounds of ERR_LENs value*/
		{
			/* Loop that builds the error string based on the passed in string */
			for (int x = 0; x < temp; x++)
			{
				t.attribute.err_lex[x] = lexeme[x];
			}
			/*Appends the null string terminator character to the end of the error string*/
		}
		t.attribute.err_lex[temp] = '\0';
		return t; /*returns error token*/
	}
	else
	{

		t.attribute.int_value = atoi(lexeme);/*assigns the tokens value to the integer converstion of lexeme*/
	}

	return t;
}

/*  Purpose: Accepting function for String Literal Token
 *  Authors:
 *	-- Brady McIntosh
 *  History: 1.0 @ 07Nov2019; 1.1 @ 08Nov2019;
 *  Called functons:
 *	-- strlen()
 *	-- b_return()
 *	-- b_addc()
 *  Parameters:
 *	-- char lexeme[]: String representing lexical word found by finite state machine
 *  Return value: Token - token containing lexical and contextual information
 *  Algorithm:
 *	-- Copy string lexeme into string table, avoiding quote marks
 *	-- If runtime error is possible, exit loop and create runtime error token
 *	-- Returns string token, or runtime error token in case of buffer error
 */
Token aa_func10(char lexeme[]) 
{
			
	Token t;					/* token to be returned */
	int i;						/* loop index */
	int cr = S_FALSE;			/* flag indicating carriage return found at last symbol */
	int errlen;					/* temporary error length var */

	pBuffer b_return = (pBuffer)S_TRUE;	/* buffer return state */

	/* set token attribute to string literal */
	t.code = STR_T;

	/* set offset from beginning of string table */
	t.attribute.str_offset = str_LTBL->addc_offset;

	/* copy lexeme into string table */
	for (i = 0; i < strlen(lexeme); ++i)
	{
		/* ensuring quote marks are not copied in */
		if (lexeme[i] != '"') 
		{
			/* this block assumes that line counter is incremented at EVERY line terminator found */
			/* if a line terminator character is found */
			if (lexeme[i] == '\r' || lexeme[i] == '\n')
			{
				/* if carriage return is found, set flag and increment line */
				if (lexeme[i] == '\r')
				{
					cr = S_TRUE;
					++line;
				}
				/* if carriage return is not found */
				else 
				{
					/* if carriage return flag is true, reset to false */
					if (cr) 
					{
						cr = S_FALSE;
					}
					/* if flag is not true, increment line */
					else
					{
						++line;
					}
				}
			}

			/* attempts to add symbol to buffer, saves return value */
			b_return = b_addc(str_LTBL, lexeme[i]);

			/* if return value is false, break loop */
			if (!b_return)
			{
				break;
			}
		}
	}

	/* if main b_addc loop failed, or if adding null character fails, */
	/* create and return run-time error token */
	if (!b_return || !b_addc(str_LTBL, '\0'))
	{
		/* if b_addc returns NULL */

		/* sets token code to run-time error */
		t.code = RTE_T;

		/* sets error string length to 17 if lexeme length exceeds 20 */
		/* otherwise, sets it to lexeme length */
		errlen = strlen(lexeme) > ERR_LEN ? ERR_LEN - ERR_STR_OFF : (int)strlen(lexeme);

		/* loop to add chars to error lexeme */
		for (i = 0; i < errlen; ++i)
		{
			t.attribute.err_lex[i] = lexeme[i];
		}

		/* if lexeme was cut short, fill remaining spaces with dots */
		if (strlen(lexeme) > ERR_LEN)
		{
			for (i = errlen; i < ERR_LEN; ++i)
			{
				t.attribute.err_lex[i] = '.';
			}
		}

		/* set final available character */
		t.attribute.err_lex[ERR_LEN] = '\0';

		return t;
	}

	return t;
}

/*  Purpose: Accepting function for Error Token
 *  Authors:
 *	-- Brady McIntosh
 *  History: 1.0 @ 08Nov2019;
 *  Called functons:
 *	-- strlen()
 *  Parameters:
 *	-- char lexeme[]: String representing lexical word found by finite state machine
 *  Return value: Token - token containing lexical and contextual information
 *  Algorithm:
 *	-- Copy 20 characters into error token attribute
 *	-- If lexeme > 20 chars, copy 17 and append 3 dots ("...")
 *	-- Return error token
 */
Token aa_func11(char lexeme[]) 
{
	Token t;			/* token to be returned */
	int cr = S_FALSE;	/* flag indicating carriage return found at last symbol */
	int errlen;			/* temporary error length var */
	int i;				/* loop index */

	/* sets token code to error */
	t.code = ERR_T;

	/* sets error string length to 17 if lexeme length exceeds 20 */
	/* otherwise, sets it to lexeme length */
	errlen = strlen(lexeme) > ERR_LEN ? ERR_LEN - ERR_STR_OFF : (int)strlen(lexeme);

	/* loop to increment line counter at line terminals */
	for (i = 0; i < strlen(lexeme); ++i)
	{
		/* this block assumes that line counter is incremented at EVERY line terminator found */
		/* if a line terminator character is found */
		if (lexeme[i] == '\r' || lexeme[i] == '\n')
		{
			/* if carriage return is found, set flag and increment line */
			if (lexeme[i] == '\r')
			{
				cr = S_TRUE;
				++line;
			}
			/* if carriage return is not found */
			else
			{
				/* if carriage return flag is true, reset to false */
				if (cr)
				{
					cr = S_FALSE;
				}
				/* if flag is not true, increment line */
				else
				{
					++line;
				}
			}
		}
	}

	/* loop to add chars to error lexeme */
	for (i = 0; i < errlen; ++i)
	{
		t.attribute.err_lex[i] = lexeme[i];
	}

	/* if lexeme was cut short, fill remaining spaces with dots */
	if (strlen(lexeme) > ERR_LEN)
	{
		for (i = errlen; i < ERR_LEN; ++i)
		{
			t.attribute.err_lex[i] = '.';
			++errlen;
		}
	}

	/* set final available character */
	t.attribute.err_lex[errlen] = '\0';


	return t;
}

/* Author: Phillip Clarke */
/* Version: 1.0 */
/* Date: 08 Nov 2019 */


/*  Purpose: Determine if a passed lexeme is a keyword
 *  Authors:
 *	-- Phillip Clarke
 *  History: 1.0 @ 08Nov2019;
 *  Called functons:
 *	-- strcmp()
 *  Parameters:
 *	-- char* lexeme: String representing lexical word found by finite state machine
 *  Return value: int - Index of keyword table
 *  Algorithm:
 *	--
 *	-- Returns index of keyword table, or -1 if no keyword is found
 */
int iskeyword(char* kw_lexeme)
{
	int i;

	/* in loop, compares lexeme to each keyword in table */
	for (i = 0; i < KWT_SIZE; ++i)
	{
		if (!strcmp(kw_lexeme, kw_table[i]))
		{
			/* if lexeme is a match, set keyword index and break loop */
			return i;
		}
	}
	return RT_FAIL_1;
}
