/*	File name: buffer.c
 *	Compiler: MS Visual Studio 2019
 *	Author: Brady McIntosh SN# 040706980
 *	Date: 2019 Oct 02
 *	Professor: Sv. Ranev
 *	Purpose: Function definitions for the Buffer project in Assignment #1.
 *	Function List:
 *	-- b_allocate()
 *	-- b_addc()
 *	-- b_clear()
 *	-- b_free()
 *	-- b_isfull()
 *	-- b_limit()
 *	-- b_capacity()
 *	-- b_mark()
 *	-- b_mode()
 *	-- b_incfactor()
 *	-- b_load()
 *  -- b_isempty()
 *	-- b_getc()
 *	-- b_eob()
 *	-- b_print()
 *	-- b_compact()
 *	-- b_rflag()
 *	-- b_retract()
 *	-- b_reset()
 *	-- b_getcoffset()
 *	-- b_rewind()
 *	-- b_location()
 */

#include "buffer.h"

/* function definitions */

/*  Purpose: Create a new Buffer in heap memory
 *  Author: Brady McIntosh 
 *  History: 1.0 @ 19Sep28; 1.1 @ 19Oct01;
 *  Called functons:
 *	-- malloc()
 *	-- calloc()
 *	-- free()
 *  Parameters: 
 *	-- short init_capacity: Initial storage capacity of Buffer, { 0 - 65534 }
 *	-- char inc_factor: Increment factor of Buffer, { 1 - 255 }
 *	-- char o_mode: Operational mode of Buffer, { 'a', 'f', 'm' }
 *  Return value: Buffer* - Pointer to a Buffer struct in memory
 *  Algorithm:
 *	-- Test validity of parameters
 *	-- Allocate memory for Buffer and internal char buffer
 *	-- Determine operational mode based on parameters
 *	-- Set initial members, including: capacity, inc_factor, mode, flags
 *	-- Return reference to Buffer
 */
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode)
{
	short temp_capacity;			/* temporary capacity size */
	unsigned char temp_inc_factor;	/* temporary increment factor */
	char temp_mode;					/* temporary operational mode */

	Buffer* buffer;					/* Buffer pointer, to be returned */
	char* temp_cb;					/* temporary character buffer pointer */

	/* test validity of parameters */

	if (init_capacity < 0 || init_capacity >(SHRT_MAX - 1))
	{
		/* in the event init_capacity is invalid */
		return NULL;
	}

	if (o_mode != 'a' && o_mode != 'f' && o_mode != 'm')
	{
		/* in the event o_mode is an invalid character */
		return NULL;
	}

	/*                                       */
	/* attempt to allocate memory for Buffer */
	/*                                       */
	buffer = (Buffer*) calloc(1, sizeof(Buffer));
	if (buffer == NULL)
	{
		/* in the event that memory allocation fails */
		return NULL;
	}


	/*                                                 */
	/* attempt to allocate memory for character buffer */
	/*                                                 */
	
	/* setting initial capacity */
	if (init_capacity == 0)
	{
		temp_capacity = DEFAULT_INIT_CAPACITY;
	}
	else
	{
		temp_capacity = init_capacity;
	}

	/* allocating for character buffer */
	temp_cb = (char*) malloc(temp_capacity);
	if (temp_cb == NULL)
	{
		/* in the event that memory allocation fails */
		free(buffer);
		return NULL;
	}

	/* assign to Buffer */
	buffer->cb_head = temp_cb;


	/*                                           */
	/* set operational mode and increment factor */
	/*                                           */

	/* arguments indicate default increment factor */
	if (init_capacity == 0 && (o_mode == 'a' || o_mode == 'm'))
	{
		inc_factor = DEFAULT_INC_FACTOR;
		temp_inc_factor = (unsigned char)inc_factor;
	}

	/* arguments indicate fixed mode */
	if (o_mode == 'f' || ((unsigned char)inc_factor == 0 && (unsigned char)init_capacity != 0))
	{
		inc_factor = 0;
		temp_inc_factor = (unsigned char)inc_factor;
		temp_mode = MODE_F;
	}

	/* arguments indicate additive mode */
	else if (o_mode == 'a')
	{

		temp_mode = MODE_A;

		/* force inc_factor to appropriate value */
		if ((unsigned char)inc_factor >= 1 && (unsigned char)inc_factor <= UCHAR_MAX)
		{
			temp_inc_factor = (unsigned char)inc_factor;
		}
		else if (init_capacity == 0)
		{
			temp_inc_factor = DEFAULT_INC_FACTOR;
		}

		else
		{
			/* in the event that the mode is 'a' and other settings are incorrect */
			free(buffer);
			free(temp_cb);
			return NULL;
		}
	}

	/* arguments indicate multiplicative mode */
	else if (o_mode == 'm') 
	{
		temp_mode = MODE_M;

		/* force inc_factor to appropriate value */
		if ((unsigned char)inc_factor >= 1 && (unsigned char)inc_factor <= MODE_M_RESTRICTOR)
		{
			temp_inc_factor = (unsigned char)inc_factor;
		}
		else if (init_capacity == 0)
		{
			temp_inc_factor = DEFAULT_INC_FACTOR;
		}

		else
		{
			/* in the event that the mode is 'm' and other settings are incorrect */
			free(buffer);
			free(temp_cb);
			return NULL;
		}
	}
	
	/* arguments are unexpected or illegal */
	else
	{
		free(buffer);
		free(temp_cb);
		return NULL;
	}

	/* assign to buffer */
	buffer->inc_factor = temp_inc_factor;
	buffer->mode = temp_mode;


	/*                                       */
	/* set init_capacity and flags in Buffer */
	/*                                       */

	/* assign to buffer */
	buffer->capacity = temp_capacity;
	buffer->flags = DEFAULT_FLAGS;
	

	/*        */
	/* return */
	/*        */
	return buffer;
}

/*  Purpose: Add a character to a Buffer
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30; 1.1 @ 19Oct01
 *  Called functons:
 *	-- realloc()
 *  Parameters:
 *	-- pBuffer pBD: Reference to Buffer
 *	-- char symbol: The character to be added
 *  Return value: pBuffer - Pointer to a Buffer struct in memory
 *  Algorithm:
 *	-- Test validity of parameters
 *  -- If enough space, adds character to Buffer
 *  -- If not enough space, allocates additional space according to mode
 *  -- If allocation does not succeed, return NULL
 *  -- If allocation succeeds, redirect pointer if necessary, add character and set new capacity
 */
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	short new_cap;		/* the new capacity of the Buffer */
	char* new_buffer;	/* temporary location of resized character buffer */

	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return NULL;
	}

	if (pBD->cb_head == NULL)
	{
		/* in the event the character buffer is null */
		return NULL;
	}
	

	/* reset r_flag to 0 */
	pBD->flags &= RESET_R_FLAG;


	/*                                          */
	/* attempt to add a character to the Buffer */
	/*                                          */

	/* check if buffer is full */
	/* if buffer is not full... */
	/* TODO this may cause issues... */
	if (pBD->addc_offset < pBD->capacity)
	{
		//err_printf("[b_addc] ADD @%d: %c %d", pBD->addc_offset, symbol, symbol);

		/* add symbol to character buffer */
		pBD->cb_head[pBD->addc_offset] = symbol;
		++pBD->addc_offset;


		/*        */
		/* return */
		/*        */
		return pBD;
	}

	/* if buffer is full... */
	else
	{
		/* initialize new capacity for re-sizing buffer */
		new_cap = pBD->capacity;


		/*                    */
		/* resize with mode 0 */
		/*                    */
		if (pBD->mode == MODE_F)
		{
			return NULL;
		}


		/*                    */
		/* resize with mode 1 */
		/*                    */
		if (pBD->mode == MODE_A)
		{
			/* add inc_factor to capacity and check validity of new capacity */
			new_cap += (unsigned char) pBD->inc_factor;

			if (new_cap > 0)
			{
				if (new_cap > MAX_POSITIVE_CAPACITY)
				{
					new_cap = MAX_POSITIVE_CAPACITY;
				}
			}
			/* if new capacity is negative */
			else
			{
				return NULL;
			}
		}


		/*                     */
		/* resize with mode -1 */
		/*                     */
		if (pBD->mode == MODE_M)
		{
			short available;	/* available space in buffer */
			short new_inc;		/* new capacity increment */

			/* if capacity is already at maximum allowed value */
			if (pBD->capacity >= MAX_POSITIVE_CAPACITY)
			{
				return NULL;
			}

			/* set new capacity */
			available = MAX_POSITIVE_CAPACITY - pBD->capacity;
			new_inc = (available * (unsigned char)pBD->inc_factor) / (short)NEW_INC_DIVISOR;
			new_cap = pBD->capacity + new_inc;

			/* in the event incrementing based on calculation is impossible */
			if (new_cap <= pBD->capacity || new_cap > MAX_POSITIVE_CAPACITY)
			{
				new_cap = MAX_POSITIVE_CAPACITY;
			}
		}


		/*                         */
		/* expand character buffer */
		/*                         */

		/* reallocate buffer */
		new_buffer = realloc(pBD->cb_head, new_cap);
		if (new_buffer == NULL)
		{
			/* in the event that memory allocation fails */
			return NULL;
		}

		/* if memory location changed, set r_flag */
		if (new_buffer != pBD->cb_head)
		{
			pBD->cb_head = new_buffer;
			pBD->flags |= SET_R_FLAG;
		}


		/*                               */
		/* add character after expanding */
		/*                               */

		/* add symbol to new buffer */
		pBD->cb_head[pBD->addc_offset] = symbol;
		++pBD->addc_offset;

		/* set new capacity */
		pBD->capacity = new_cap;


		/*        */
		/* return */
		/*        */
		return pBD;
	}
}

/*  Purpose: Reset contents of Buffer to new & empty
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: int - Error code
 *  Algorithm:
 *	-- Test validity of parameters
 *  -- Set appropriate member variables to default beginning values
 */
int b_clear(Buffer* const pBD)
{

	/* test validity of Buffer* */
	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	/* reset appropriate member variables */
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->flags = DEFAULT_FLAGS;

	/* return */
	return SUCCESS;
}

/*  Purpose: De-allocates all Buffer memory
 *  Author: Brady McIntosh
 *  History: 1.1 @ 09Nov30;
 *  Called functons:
 *	-- free()
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: void
 *  Algorithm:
 *	-- Test Validity of parameters
 *  -- Free memory blocks
 */
void b_free(Buffer* const pBD)
{
	//err_printf("[b_free]");

	/* test validity of Buffer* */
	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		//err_printf("[b_free] Error: Invalid parameter \"pBD\": %p", (void*)pBD);
		return;
	}

	/* free memory blocks */
	free(pBD->cb_head);
	free(pBD);
}

/*  Purpose: Check if the Buffer is full
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: int - Indicator/Error code
 *  Algorithm:
 *	-- Test validity of parameters
 *  -- Return 1 if full, 0 if not full, -1 in error
 */
int b_isfull(Buffer* const pBD)
{
	/* test validity of Buffer* */
	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		// err_printf("[b_isfull] Error: Invalid parameter \"pBD\": %p", (void*)pBD);
		return RT_FAIL_1;
	}

	/* if buffer is full */
	if (pBD->addc_offset == pBD->capacity)
	{
		return B_TRUE;
	}

	/* if buffer is not full */
	else
	{
		return B_FALSE;
	}
}

/*  Purpose: Get limit of currently stored characters in buffer
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: short - Space being used by stored characters
 *  Algorithm:
 *	-- Test validity of parameters
 *  -- Return current buffer limit, or -1 in case of error
 */
short b_limit(Buffer* const pBD)
{
	/* test validity of Buffer* */
	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	/* addc_offset also represents the space used by stored characters */
	return pBD->addc_offset;
}

/*  Purpose: Get current capacity of buffer
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: short - Current buffer capacity
 *  Algorithm:
 *	-- Test validity of parameters
 *  -- Return current capacity, or -1 in case of error
 */
short b_capacity(Buffer* const pBD)
{
	/* test validity of Buffer* */
	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	return pBD->capacity;
}

/*  Purpose: Set mark offset of buffer
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  -- short mark: New mark offset setting
 *  Return value: short - Current mark offset
 *  Algorithm:
 *	-- Test validity of parameters
 *  -- Set new mark offset setting, according to arguments
 *  -- Return current mark offset setting, or -1 in case of error
 */
short b_mark(pBuffer const pBD, short mark)
{
	/* test validity of Buffer* */
	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	if (mark < 0 || mark > pBD->addc_offset)
	{
		/* in the event mark param is illegal */
		return RT_FAIL_1;
	}

	/* set mark offset */
	pBD->markc_offset = mark;

	return pBD->markc_offset;
}

/*  Purpose: Get operational mode of buffer
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: int - Mode value
 *  Algorithm:
 *	-- Test validity of parameters
 *  -- Return operational mode, or -2 in case of error
 */
int b_mode(Buffer* const pBD)
{
	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_2;
	}

	return pBD->mode;
}

/*  Purpose: Get increment factor of buffer
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: size_t - Increment factor
 *  Algorithm:
 *	-- Test validity of parameters
 *  -- Return increment factor, or 0x100 in case of error
 */
size_t b_incfactor(Buffer* const pBD)
{
	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return UNSIGN_FAIL;
	}

	/* multi-step casting to avoid overflow */
	return (size_t)(unsigned char)pBD->inc_factor;
}

/*  Purpose: Read file contents into buffer
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30; 1.1 @ 19Oct01
 *  Called functons:
 *	-- fgetc()
 *  -- ungetc()
 *  -- b_addc()
 *  Parameters:
 *  -- FILE* fi: Reference to file, to be read
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: int - Number of characters added to buffer
 *  Algorithm:
 *	-- Test validity of parameters
 *  -- Loop through file stream until EOF
 *  -- Attempt to add character to buffer
 *  -- Return number of characters added, or -1 in case of standard error, or -2 in case of load error
 */
int b_load(FILE* const fi, Buffer* const pBD)
{
	char symbol;	/* one character read from the input stream */
	int counter;	/* the number of characters added to the buffer */

	/* test validity of parameters */

	if (fi == NULL)
	{
		/* in the event the file stream is null */
		return RT_FAIL_1;
	}

	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	counter = 0;

	/* loop as long as end-of-file is not discovered */
	/* (this will only work if EOF is set BEFORE fgetc() is called) */
	/* (otherwise, a bypass statement must be used) */
	while (!feof(fi))
	{
		symbol = (char)fgetc(fi);

		/* bypass check for EOF */
		if (feof(fi))
		{
			break;
		}

		/* attempt to add symbol to buffer */
		if (b_addc(pBD, symbol) == NULL)
		{
			/* in the event b_addc fails */
			/* TODO is the following printout necessary? */
			symbol = (int)ungetc(symbol, fi);

			if (symbol == EOF)
			{
				/* in the unexpected event ungetc fails */
			}

			return LOAD_FAIL;
		}

		/* number of characters read +1 */
		++counter;
	}

	return counter;
}

/*  Purpose: Check if buffer is empty
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: int - Error code
 *  Algorithm:
 *	-- Test validity of parameters
 *  -- Return 1 if buffer is empty, 0 if not empty, -1 in case of error
 */
int b_isempty(Buffer* const pBD)
{
	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	/* valid return cases*/
	/* if buffer is empty */
	if (pBD->addc_offset == 0)
	{
		return B_TRUE;
	}

	/* if buffer is not empty but otherwise functional */
	if (pBD->addc_offset > 0 && pBD->addc_offset <= MAX_POSITIVE_CAPACITY)
	{
		return B_FALSE;
	}

	/* TODO is this necessary? */
	/* in the event the offset is invalid */
	return RT_FAIL_1;
}

/*  Purpose: Gets the next character from the buffer
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: char - Next character in buffer
 *  Algorithm:
 *	-- Test validity of parameter
 *  -- Attempt to get next character in buffer, then increment get offset
 *  -- Set end-of-buffer flag if last stored character is reached
 *  -- Return next char if it exists, 0 if at end of buffer, -2 in case of error
 */
char b_getc(Buffer* const pBD)
{
	char c_read;	/* the character read from storage */

	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_2;
	}

	/* if all characters have been read from storage */
	if (pBD->getc_offset >= pBD->addc_offset)
	{
		pBD->flags |= SET_EOB;
		return SUCCESS;
	}

	/* if not all characters have been read yet */
	pBD->flags &= RESET_EOB;

	/* read char from buffer, then increment get offset */
	c_read = pBD->cb_head[pBD->getc_offset];
	++pBD->getc_offset;

	return c_read;
}

/*  Purpose: Get end-of-buffer flag value from buffer
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30; 1.1 @ 19Oct01;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: int - Error code
 *  Algorithm:
 *	-- Test validity of parameter
 *  -- Read the EOB flag in the buffer's flags member
 *  -- Return the EOB flag value exclusively, or -1 in case of error
 */
int b_eob(Buffer* const pBD)
{
	unsigned short eob_value;	/* the value of the EOB bit in the "flags" member */

	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	/* save and return exclusive value of EOB flag */
	eob_value = pBD->flags & CHECK_EOB;
	return eob_value;
}

/*  Purpose: Print entire contents of buffer to stdout (diagnostic)
 *  Author: Brady McIntosh
 *  History: 1.1 @ 19Sep30; 1.1 @ 19Oct01;
 *  Called functons:
 *  -- b_eob()
 *  -- b_getc()
 *	-- printf()
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  -- char nl: New line determiner
 *  Return value: int - Error code
 *  Algorithm:
 *	-- Test validity of parameters
 *  -- Sequentially read and print every character in buffer
 *  -- Print newline according to parameter
 *  -- Return number of characters read, or -1 in case of error
 */
int b_print(Buffer* const pBD, char nl)
{
	char c_read;		/* the character read from storage */
	int counter = 0;	/* number of characters read */

	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	counter = 0;

	/* loop as long as there are more characters to read */
	/* (this will only work if eob is set BEFORE b_getc() is called) */
	/* (otherwise, a bypass statement must be used) */
	while (b_eob(pBD) != CHECK_EOB)
	{
		/* read character, increment counter, print character */
		c_read = b_getc(pBD);

		/* bypass check for EOB */
		if (b_eob(pBD))
		{
			break;
		}

		++counter;

		/* print read character to stdout */
		printf("%c", c_read);
	}

	/* newline at the end if nl is non-zero */
	if (nl != 0)
	{
		printf("\n");
	}

	return counter;
}

/*  Purpose: Set free space in buffer to only 1 if possible, then add single character
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30; 1.1 @ 19Oct1;
 *  Called functons:
 *	-- realloc()
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  -- char symbol: character to add to buffer
 *  Return value: Buffer* - Reference to Buffer
 *  Algorithm:
 *	-- Test parameters for validity
 *  -- Attempt to resize buffer
 *  -- Add character to buffer
 *  -- Return reference, or NULL in case of error
 */
Buffer* b_compact(Buffer* const pBD, char symbol)
{
	short new_cap;		/* the new capacity of the buffer */
	char* new_buffer;	/* temporary location of resized character buffer */

	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return NULL;
	}

	if (pBD->cb_head == NULL)
	{
		/* in the event the character buffer is null */
		return NULL;
	}

	/*                             */
	/* resize for single character */
	/*                             */
	new_cap = pBD->addc_offset + 1;
	if (new_cap > SHRT_MAX)
	{
		/* in the event the buffer capacity cannot be increased */
		return NULL;
	}

	/* reallocate buffer */
	new_buffer = realloc(pBD->cb_head, new_cap);
	if (new_buffer == NULL)
	{
		/* in the event that memory allocation fails */
		return NULL;
	}

	/* if memory location changed, set r_flag */
	if (new_buffer != pBD->cb_head)
	{
		pBD->cb_head = new_buffer;
		pBD->flags |= SET_R_FLAG;
	}


	/*                               */
	/* add character after expanding */
	/*                               */

	/* add symbol to new buffer */
	pBD->cb_head[pBD->addc_offset] = symbol;
	++pBD->addc_offset;

	/* set new capacity */
	pBD->capacity = new_cap;


	/*        */
	/* return */
	/*        */
	return pBD;
}

/*  Purpose: Get reallocation flag value from buffer
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: char - r_flag value
 *  Algorithm:
 *	-- Test parameter for validity
 *  -- Read the r_flag in the buffer's flags member
 *  -- Return the r_flag value exclusively, or -1 in case of error
 */
char b_rflag(Buffer* const pBD)
{
	unsigned short r_value = 0x0000;	/* the value of the r_flag bit in the "flags" member */

	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	/* save and return exclusive value of r_flag */
	r_value |= pBD->flags & CHECK_R_FLAG;
	return (char) r_value;
}

/*  Purpose: Decrements get-offset of buffer by 1
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: short - New get-offset value
 *  Algorithm:
 *	-- Test parameters for validity
 *  -- Decrements get-offset
 *  -- Returns new get-offset value, or -1 in case of error
 */
short b_retract(Buffer* const pBD)
{
	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	if (pBD->getc_offset < 0)
	{
		/* in the event decrement would lead to below-zero offset */
		return RT_FAIL_1;
	}

	/* decrement offset */
	--pBD->getc_offset;
	return pBD->getc_offset;
}

/*  Purpose: Set get-offset to current mark-offset
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: short - New get-offset value
 *  Algorithm:
 *	-- Test parameter for validity
 *  -- Set get-offset to mark-offset
 *  -- Return new get-offset value, or -1 in case of error
 */
short b_reset(Buffer* const pBD)
{
	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	/* set new offset */
	pBD->getc_offset = pBD->markc_offset;
	return pBD->getc_offset;
}

/*  Purpose: Get current get-offset value of buffer
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: short - Current get-offset value
 *  Algorithm:
 *	-- Test parameters for validity
 *  -- Return current get-offset, or -1 in case of error
 */
short b_getcoffset(Buffer* const pBD)
{
	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	return pBD->getc_offset;
}

/*  Purpose: Set get-offset and mark-offset of buffer back to zero
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: int - Error code
 *  Algorithm:
 *	-- Test parameter for validity
 *  -- Reset offsets to zero
 *  -- Return 0 for success, -1 in case of error
 */
int b_rewind(Buffer* const pBD)
{

	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return RT_FAIL_1;
	}

	/* set new offsets */
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;

	/* reset eob flag */
	pBD->flags &= RESET_EOB;

	return 0;
}

/*  Purpose: Get reference to character buffer at point indicated by mark-offset
 *  Author: Brady McIntosh
 *  History: 1.0 @ 19Sep30;
 *  Called functons:
 *	-- (None)
 *  Parameters:
 *	-- Buffer* pBD: Reference to Buffer
 *  Return value: char* - Reference to character buffer
 *  Algorithm:
 *	-- Test validity of parameters
 *  -- Create new character pointer, offset by mark-offset
 *  -- Return new pointer, or NULL in case of error
 */
char* b_location(Buffer* const pBD)
{
	char* c_point;	/* marked character pointer */

	if (pBD == NULL)
	{
		/* in the event the Buffer is null */
		return NULL;
	}

	if (pBD->cb_head == NULL)
	{
		/* in the event the character buffer is null */
		return NULL;
	}

	/* add offset to pointer to create offset pointer... ;) */
	c_point = pBD->cb_head + pBD->markc_offset;

	return c_point;
}