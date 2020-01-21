/* Filename: scanner.c
/* PURPOSE:
 *    SCANNER.C: Functions implementing a Lexical Analyzer (Scanner)
 *    as required for CST8152, Assignment #2
 *    scanner_init() must be called before using the scanner.
 *    The file is incomplete;
 *    Provided by: Svillen Ranev
 *    Version: 1.19.2
 *    Date: 2 October 2019
 *******************************************************************
 *    REPLACE THIS HEADER WITH YOUR HEADER
 *******************************************************************
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

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

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

Token malar_next_token(void)
{
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/

	/* local variables */

	while (1) { /* endless loop broken by token returns it will generate a warning */

		/* get next character */

		c = b_getc(sc_buf);


		/* Part 1: Implementation of token driven scanner */
		/* every token is possessed by its own dedicated code */



		/* limited example to start with */

		if (c == ' ') continue;
		if (c == '{')
		{
			t.code = RBR_T; /*no attribute */ return t;
			if (c == '+') {
				t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t;
			}


			/* part 2 *** transition table implementation */


			b_free(lex_buf);
			return t;
		}//end while(1)
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
		/* return column number in transition table, for input character c */
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
			else if (c == '\0' || c == EOF) {
				val = 6;
			}

			/* other */
			else {
				val = 7;
			}

			return val;
		}



		/* accepting functions */

		/* for AVID token */

		Token aa_func02(char lexeme[]) 
		{
			Token t;
			/* check if the lexeme is a keyword */

			/* if not, set AVID token */

			return t;
		}

		/*ACCEPTING FUNCTION FOR THE string variable identifier(VID - SVID)
			REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER*/

		Token aa_func03(char lexeme[])
		{
			Token t;
			t.code = 3;
			if (strlen(lexeme) > srtlen(t.attribute.vid_lex))
			{

			}

				/*WHEN CALLED THE FUNCTION MUST
				1. SET a SVID TOKEN.
				IF THE lexeme IS LONGER than VID_LEN characters,
				ONLY FIRST VID_LEN - 1 CHARACTERS ARE STORED
				INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[],
				AND THEN THE @ CHARACTER IS APPENDED TO THE NAME.
				ADD \0 AT THE END TO MAKE A C - type STRING.*/

			return t;
		}

		/*ACCEPTING FUNCTION FOR THE floating - point literal(FPL)*/

		Token aa_func08(char lexeme[]) 
		{
			Token t;
			/*THE FUNCTION MUST CONVERT THE LEXEME TO A FLOATING POINT VALUE,
				WHICH IS THE ATTRIBUTE FOR THE TOKEN.
				THE VALUE MUST BE IN THE SAME RANGE AS the value of 4 - byte float in C.
				IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
				THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
				than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
				STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
				err_lex C - type string.
				BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
			return t;
		}

		/*ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant(DIL)*/

		Token aa_func05(char lexeme[]) 
		{
			Token t;
			/*THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING A DECIMAL CONSTANT
				TO A DECIMAL INTEGER VALUE, WHICH IS THE ATTRIBUTE FOR THE TOKEN.
				THE VALUE MUST BE IN THE SAME RANGE AS the value of 2 - byte integer in C.
				IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
				THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
				than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
				STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
				err_lex C - type string.
				BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
			return t;
		}

		/*ACCEPTING FUNCTION FOR THE string literal(SL)*/

		Token aa_func10(char lexeme[]) 
		{
			Token t;
			/*THE FUNCTION MUST STORE THE lexeme PARAMETER CONTENT INTO THE STRING LITERAL TABLE(str_LTBL)
				FIRST THE ATTRIBUTE FOR THE TOKEN MUST BE SET.
				THE ATTRIBUTE OF THE STRING TOKEN IS THE OFFSET FROM
				THE BEGINNING OF THE str_LTBL char buffer TO THE LOCATION
				WHERE THE FIRST CHAR OF THE lexeme CONTENT WILL BE ADDED TO THE BUFFER.
				USING b_addc(..)COPY THE lexeme content INTO str_LTBL.
				THE OPENING AND CLOSING " MUST BE IGNORED DURING THE COPING PROCESS.
				ADD '\0' AT THE END MAKE THE STRING C - type string
				IF THE STING lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
				SET THE STRING TOKEN CODE.*/
			return t;
		}

		/*ACCEPTING FUNCTION FOR THE ERROR TOKEN*/

		Token aa_func11(char lexeme[]) 
		{
			Token t;
			/*THE FUNCTION SETS THE ERROR TOKEN.lexeme[] CONTAINS THE ERROR
				THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme CONTENT ITSELF
				AND IT MUST BE STORED in err_lex.IF THE ERROR lexeme IS LONGER
				than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
				STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
				err_lex C - type string.
				IF THE ERROR lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
				BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
			return t;
		}

		/*HERE YOU WRITE YOUR ADDITIONAL FUNCTIONS(IF ANY).
			FOR EXAMPLE */
		int iskeyword(char* kw_lexeme) {}