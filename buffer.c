/* 
* File Name: buffer.c
* Compiler: MS Visual Studio 2019
* Author: Phillip Clarke, 040832994
* Course: CST 8152 – Compilers, Lab Section: 011
* Assignment: 1
* Date: October 2nd, 2019
* Professor: Sv. Ranev
* Purpose: is to take command line arguments and apply logic to them based on what is taken in as well as passed in 
* values from platy_bt.c - functions in this file are used to create a buffer and store information based on a passed in file.
* Function list:
* b_allocate();
* b_addc();
* b_clear();
* b_free();
* b_isfull();
* b_limit();
* b_capacity();
* b_mark();
* b_mode();
* b_incfactor();
* b_load();
* b_isempty();
* b_getc();
* b_eob();
* b_print();
* b_compact();
* b_rflag();
* b_retract();
* b_reset();
* b_getcoffset();
* b_rewind();
* b_location();
*/
#include <stdio.h>
#include "buffer.h"

/*
* Purpose: is to dynamically allocate space for a buffer object and character array and initialize its properties based on specific conditions related to the passed in values.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions: malloc()
* Parameters: short init_capacity (starting capacity), char inc_factor (starting increment), char o_mode (mode)
* Return value: buffer pointer
* Algorithm: Allocates memory for a buffer, allocates memory for a character pointer, sets buffer values based on passed in values.
*/
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode)
{
	Buffer* buffer = calloc(1, sizeof(Buffer)); /*Allocates space on the heap for a buffer*/
	if (buffer)
	{
		if ((unsigned short)init_capacity > (SHRT_MAX - 1) || init_capacity < 0) /*Checks if the passed in init_capacity is larger than the data type*/
		{
			free(buffer); /*Frees allocated memory for buffer*/
			return NULL;
		}
		if (init_capacity == 0 && o_mode == 'a' || init_capacity == 0 && o_mode == 'm') /*Checks if the passed in mode is 'a' or 'm' and the init_capacity is 0*/
		{
			inc_factor = 15; /*Sets the inc_factor to 15*/
		}
		else if (init_capacity == 0 && o_mode == 'f') /*Checks if the passed in mode is 'f' and the init_capacity is 0*/
		{
			inc_factor = 0; /*Sets the inc_factor to 0*/
		}
		if (init_capacity == 0) /*Checks if the passed in init_capacity is 0*/
		{
			init_capacity = 200;
			buffer->cb_head = malloc(init_capacity * sizeof(char)); /*Allocates space on the heap for the character buffer based on the init_capacity*/
			if (!buffer->cb_head) /*Checks to ensure malloc() worked correctly and returned a valid pointer*/
			{
				return NULL;
			}

		}
		else if (init_capacity > 0 && init_capacity <= (SHRT_MAX - 1)) /*Checks if the passed in init_capacity value is within the bounds of its data type*/
		{
			buffer->cb_head = malloc(init_capacity); /*Allocates space on the heap for the character buffer based on the init_capacity*/
			if (!buffer->cb_head) /*Checks to ensure malloc() worked correctly and returned a valid pointer*/
			{
				return NULL;
			}
		}

		if (o_mode == 'f') /*Applies contents if passed in o_mode is 'f'*/
		{
			buffer->mode = 0; /*Sets buffer mode to 0*/
			buffer->inc_factor = 0; /*Sets buffer inc_factor to 0*/
		}
		if (inc_factor == 0 && init_capacity != 0) /*Applies contents if passed in inc_factor is 0 and init_capacity doesn't equal 0*/
		{
			buffer->mode = 0; /*Sets the buffer mode to 0*/
			buffer->inc_factor = 0; /*Sets the buffer inc_factor to 0*/
			buffer->capacity = init_capacity; /*Sets the buffer capacity to init_capacity*/
			buffer->flags = DEFAULT_FLAGS; /*Sets the buffers flags to the default*/
			return buffer; /*Returns the newly created buffer*/
		}
		if (o_mode == 'a' && (((unsigned char)inc_factor >= 0 && (unsigned char)inc_factor < 256))) /*Applies contents if mode is 'a' and unsigned inc_factor is within 0 and 255 inclusive*/
		{
			buffer->mode = 1; /*Sets the buffer mode to 1*/
			buffer->inc_factor = inc_factor; /*Sets the buffer inc_factor to the passed in inc_factor*/
		}
		if (o_mode == 'm' && ((unsigned char)inc_factor > 0 && (unsigned char)inc_factor < 101)) /*Applies contents if mode is 'm' and unsigned inc_factor is within 1 and 100 inclusive*/
		{
			buffer->mode = -1; /*Sets the buffer mode to -1*/
			buffer->inc_factor = inc_factor; /*Sets the buffer inc_factor to the passed in inc_factor*/
		}
		if (o_mode == 'm' && ((unsigned char)inc_factor > 100)) /*Applies contents if mode is 'm' and unsigned inc_factor is greater than 100*/
		{
			free(buffer); /*Frees the allocated memory created for the buffer*/
			return NULL;
		}
		buffer->capacity = init_capacity; /*Sets the buffer capacity to the init_capacity*/
		buffer->flags = DEFAULT_FLAGS; /*Sets the buffers flags to the default flags*/
		return buffer; /*Returns the buffer pointer*/
	}
	return NULL;
}
/*
* Purpose: is to add characters to the character array in the buffer and dynamically reallocate space based on the mode.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions: realloc()
* Parameters: pBuffer const pBD (buffer pointer), char symbol (single symbol to be added)
* Return value: buffer pointer
* Algorithm: Resets the r_flag using bitwise operations, based on the mode - either doesn't change, adds or 
* multiplies the capacity and reallocates space for the character pointer cb_head based on specific math
* adds the passed in symbol to the next add location
*/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	if (pBD)/*Verifies pointer passed in is not null*/
	{
		pBD->flags &= RESET_R_FLAG; /*Using bitwise operators, sets the r_flag back to 0*/
		if (pBD->addc_offset != pBD->capacity) /*If addc_offset doesn't equal the capacity then apply the contents*/
		{
			pBD->cb_head[pBD->addc_offset] = symbol; /*Sets the value at location cb_head[index value of addc_offset] to the passed in symbol*/
			pBD->addc_offset++; /*Increments addc_offset*/
			return pBD;/*Returns the buffer pointer*/
		}
		if (pBD->mode == 0) /*Applies the contents if the buffers mode is 0 (Fixed)*/
		{
			return NULL;
		}
		if (pBD->mode == 1)/*Applies the contents if the buffers mode is 1 (Additive)*/
		{
			short tempCap = pBD->capacity + ((unsigned char)pBD->inc_factor); /*Creates a short tempCap that holds the value of the new capacity*/
			if (tempCap < 0) /*Checks if tempCap has overflown the data type*/
			{
				return NULL;
			}
			if (tempCap > (SHRT_MAX - 1))/*Checks if tempCap is larger than the maximum amount allowed for a short - 1*/
			{
				tempCap = (SHRT_MAX - 1); /*Sets value to the max number that a short can handle - 1*/
			}
			char * temp = realloc(pBD->cb_head, tempCap * sizeof(char)); /*Creates a character pointer to hold the location of the reallocated cb_head based on the new sice (tempCap)*/
			if (temp) /*If the realloc was successful the contents will run*/
			{
				pBD->cb_head = temp; /*Sets the cb_head pointer within the buffer to the new pointer created with realloc*/
				pBD->flags = pBD->flags |= SET_R_FLAG; /*Sets the r_flag indicating a change of head location*/
				pBD->capacity = tempCap; /*updates the capacity to the value of tempCap*/
				pBD->cb_head[pBD->addc_offset] = symbol; /*Adds the passed in symbol to the location of addc_offset in the charater pointer*/
				pBD->addc_offset++; /*Increments addc_offset*/
				return pBD; /*Returns the updated buffer pointer pBD*/
			}
		}
			
		
		if (pBD->mode == -1)/*Applies the contents if the buffers mode is -1 (Multiplicative)*/
		{
			short space = (SHRT_MAX - 1) - pBD->capacity; /*Creates a short that holds the maximum value a signed short can hold - 1 then subtracts the current capacity*/
			if (space == 0) /*If there's no space left returns NULL*/
			{
				return NULL;
			}
			long increment = (space * (unsigned char)pBD->inc_factor) / 100; /*Creates a long that calculates the amount the new capacity should increase by*/
			if (increment < 1) /*ints round down to their nearest whole number so if the calculation is less than 1.0 then I set it to 1 so the capacity increases*/
			{
				increment = 1; /*Sets increment to 1*/
			}
			short tempCap = pBD->capacity + increment; /*Creates a short that holds the new calculated capacity*/
			char* temp; /*Creates a char pointer that will be used to test the reallocation of the current cb_head*/
			if (tempCap > (SHRT_MAX - 1) || tempCap < 0) /*Applies contents if tempcap is greater than the maximum unsigned short value or a negative number (indicating an overflow of the short)*/
			{
				temp = realloc(pBD->cb_head, (SHRT_MAX - 1) * sizeof(char));/*Reallocates space on the heap to hold the new resized character pointer of size short max*/
			}
			else
				temp = realloc(pBD->cb_head, (tempCap * sizeof(char))); /*Reallocates space on the heap to hold the new resized character pointer of size equal to tempCap*/
			if (temp) /*Upon success of the reallocation, the contents are applied*/
			{
				pBD->flags = pBD->flags |= SET_R_FLAG; /*Sets the r_flag to 1 indicating a movement of the buffer cb_head*/
				pBD->cb_head = temp; /*Sets the pBD->cb_head to the pointer newly created by the realloc*/
				pBD->capacity = tempCap; /*Sets the new capacity to the value of tempCap*/
				pBD->cb_head[pBD->addc_offset] = symbol; /*Adds the passed in symbol to the location of addc_offset in the charater pointer*/
				pBD->addc_offset++; /*Increments addc_offset by 1*/
				return pBD; /*Returns the buffer pointer pBD*/
			}
		}
	}
	return NULL;
}

/*
* Purpose: is to reset the add, read and mark offsets and the flags for the buffer to allow for entry from the beginning of the cb_head.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: int (0 on success - RT_FAIL_1 {-1} on failure)
* Algorithm: Set the buffer add, read and mark offsets to 0 and the flags to default
*/
int b_clear(Buffer* const pBD)
{
	if (pBD)/*Validates the pointer passed in is not NULL*/
	{
		pBD->addc_offset = 0; /*Sets addc_offset to 0*/
		pBD->getc_offset = 0; /*Sets getc_offset to 0*/
		pBD->flags = DEFAULT_FLAGS; /*Sets buffer flags to default*/
		pBD->markc_offset = 0; /*Sets markc_offset to 0*/
		return 0;
	}
	return RT_FAIL_1; /*Returns RT_FAIL_1 constant value*/
}
/*
* Purpose: is to free the dynamically allocated memory for the buffer.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions: free()
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: void
* Algorithm: Frees the memory allocated for cb_head inside the Buffer pBD then frees pBD itself
*/
void b_free(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		free(pBD->cb_head); /*Frees the heap memory allocated for the character pointer cb_head*/
		free(pBD); /*Frees the heap memory allocated for the buffer pBD*/
	}
}
/*
* Purpose: is to check if the character array in the Buffer pBD is full.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: int (Returns 1 if full, 0 if not full, RT_FAIL_1 if runtime error is encountered)
* Algorithm: Checks if passed in pointer is null, checks if addc_offset is greater than capacity and returns values accordingly
*/
int b_isfull(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		if (pBD->addc_offset >= pBD->capacity) /*Contents are applied if addc)offset is greater or equal to the capacity*/
		{
			return 1;
		}
			return 0;
	}
	return RT_FAIL_1; /*Returns the constant RT_FAIL_1*/
	
}
/*
* Purpose: is to calculate the size of the current buffer in bytes.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions: 
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: short (returns size of current buffer in bytes or RT_FAIL_1 upon runtime failure)
* Algorithm: Checks if the pointer is valid, 
*/
short b_limit(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		return pBD->addc_offset * sizeof(char); /*Calculates the size of the current buffer in bytes*/
	}
	return RT_FAIL_1; /*Returns the constant value of RT_FAIL_1*/
}
/*
* Purpose: is to return the current value of the buffer capacity.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: short (returns value of the buffer capacity or RT_FAIL_1 upon runtime failure)
* Algorithm: 
*/
short b_capacity(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		return pBD->capacity; /*Returns current buffer capacity value*/
	}
	return RT_FAIL_1; /*Returns constant value RT_FAIL_1*/
}
/*
* Purpose: is to set the value of markc_offset based on passed in value and return it.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: pBuffer const pBD (buffer pointer), short mark(new markc_offset)
* Return value: short (returns new updated markc_offset upon success or constant value RT_FAIL_1 upon runtime failure)
* Algorithm: 
*/
short b_mark(pBuffer const pBD, short mark)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		if (mark <= pBD->addc_offset && mark >=0) /*Ensures past in number is within the bounds of 0 and the addc_offset*/
		{
			pBD->markc_offset = mark; /*Sets the value of the buffers markc_offset to the value of the passed in short mark*/
			return pBD->markc_offset; /*Returns new updated markc_offset*/
		}
	}
	return RT_FAIL_1;/*Returns constant value RT_FAIL_1 upon runtime failure*/
	
}
/*
* Purpose: is to return the current value of the buffer mode.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: int (returns the current mode upon success or NULL upon failure)
* Algorithm:
*/
int b_mode(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		return pBD->mode;
	}
	return NULL;
	
}
/*
* Purpose: is to return the current value of the buffer inc_factor.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: size_t (unsigned int) (Returns the current value of the buffer inc_factor casted as an unsigned char upon success or returns hex value representing 256 upon runtime failure )
* Algorithm:
*/
size_t b_incfactor(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		return (unsigned char)pBD->inc_factor; /*Returns the current value of the buffer inc_factor casted as an unsigned char*/
	}
	return 0x100; /*Returns hex value representing 256*/
}
/*
* Purpose: is to pull the next character from the passed in file and apply logic based on what is taken from the file.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions: fgetc(), ungetc(), b_addc(), feof()
* Parameters: Buffer* const pBD (buffer pointer), File pointer const fi (file being read)
* Return value: int (returns size upon success, LOAD_FAIL upon b_addc failure or RT_FAIL_1 on runtime failure)
* Algorithm: Gets next char from the file using fgetc(), checks if it's the end of the file, if not, passes character to b_addc - if a non-null value is returned
* increments size and grabs the next character from the file until either the file is empty or the buffer can't add any more and returns a null
*/
int b_load(FILE* const fi, Buffer* const pBD)
{
	if (fi && pBD) /*Validates the buffer pointer and file pointer passed in are not NULL*/
	{
		char unit = ""; /*Creates a char that holds the next value taken from the file pointer fi*/
		int size = 0; /*Creates an int named size that tracks the number of characters added in the while loop*/
		unit = fgetc(fi); /*Sets unit to the character returned form fgetc()*/

		if (feof(fi)) /*Checks if the file is empty or at the end*/
		{
			ungetc(unit, fi); /*Returns the value of unit back to the file pointer*/
			return size; /*Returns the value of size*/
		}
		while (b_addc(pBD, unit) != NULL) /*A loop that runs and adds characters until the buffer can't hold any more or the file has been read in its entirety*/
		{
			if (feof(fi)) /*Checks if the file is empty or at the end*/
			{
				ungetc(unit, fi); /*Returns the value of unit back to the file pointer*/
				pBD->addc_offset--; /*Decrements the value of addc_offset*/
				return size; /*Returns the value of size*/
			}
			size++;/*Increments the value of size by 1*/
			unit = fgetc(fi); /*Gets the next character from the file pointer fi*/

		}
		ungetc(unit, fi); /*Returns the value of unit back to the file pointer*/
		return LOAD_FAIL; /*Returns the value of the constant LOAD_FAIL if b_addc returns NULL*/
	}
	return RT_FAIL_1; /*Returns the value of the constant RT_FAIL_1 if a runtime error is encountered*/
}
/*
* Purpose: is to check if the buffer is currently empty.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: int (returns 1 if the buffer is empty, 0 if not or RT_FAIL_1 if a runtime error occurs)
* Algorithm:
*/
int b_isempty(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		if (pBD->addc_offset == 0) /*Checks if the current addc_offset is equal to 0*/
		{
			return 1; /*Returns 1*/
		}
		else
		{
			return 0; 
		}
	}
	return RT_FAIL_1; /*Returns the value of the constant RT_FAIL_1*/
}
/*
* Purpose: is to return the character within the character buffer at the index equal to getc_offset and sets the EOB bit using bitwise operators if it reaches the end of the character buffer.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: char (returns the character within the character buffer at the index equal to getc_offset upon success, returns 0 if getc_offset is equal to addc_offset or returns RT_FAIL_2 upon runtime failure)
* Algorithm:
*/
char b_getc(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		if (pBD->getc_offset == pBD->addc_offset) /*Checks if the current getc_offset is equal to the current addc_offset*/
		{
			pBD->flags |= SET_EOB; /*Using bitwise operators, sets the EOB bit to 1 in the buffer flags*/
			return 0;
		}
		pBD->flags &= RESET_EOB; /*Using bitwise operators resets the EOB bit in the buffer flags*/
		char temp = pBD->cb_head[pBD->getc_offset]; /*Sets temp to the character at index getc_offset*/
		pBD->getc_offset++; /*Increments getc_offset*/
		return temp; /*Returns temp*/
	}
	return RT_FAIL_2; /*Returns RT_FAIL_2 upon runtime failure*/
}
/*
* Purpose: is to check using bitwise operators the value of the EOB bit.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: int (returns the value of the EOB bit using bitwise operators)
* Algorithm:
*/
int b_eob(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		return pBD->flags & CHECK_EOB; /*Using bitwise operators, returns the value of the EOB bit*/
	}
	return RT_FAIL_1; /*Returns the constant value RT_FAIL_1 upon runtime failure*/
}
/*
* Purpose: is to print the current character buffer
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions: b_getc(), b_eob()
* Parameters: Buffer* const pBD (buffer pointer), char nl (dictates if a new line is inserted)
* Return value: int (returns counter upon success or RT_FAIL_1 if a runtime error occurs)
* Algorithm: Sets value of temp to the returned value of b_getc, checks result of b_eob() if not == 2, prints the value of temp and increments the counter and continues the loop
* if nl != 0 inserts a new line
*/
int b_print(Buffer* const pBD, char nl)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		int counter = 0; /*Creates an int to hold the number of characters printed*/
		char temp = b_getc(pBD); /*Creates a new char and sets its value to the return value of b_getc*/
		while (temp != NULL) /*Loops until b_getc returns NULL*/
		{
			if (b_eob == 2) /*Breaks if EOB bit value is set indicating the end of the buffer*/
			{
				break;
			}
			printf("%c", temp); /*Prints the value of temp*/
			counter++; /*Increments the variable counter*/
			temp = b_getc(pBD); /*Sets temp to the return value of b_getc*/
		}
		if (nl != 0) /*Prints a new line if nl doesn't equal 0*/
		{
			printf("\n");
		}
		return counter; /*returns the number of characters printed*/
	}
	return RT_FAIL_1; /*Returns the constant value of RT_FAIL_1*/
	
}
/*
* Purpose: is to reallocate the size of the character buffer to the current size + 1 and input the next character.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions: realloc(), b_limit()
* Parameters: Buffer* const pBD (buffer pointer), char symbol (character to be added)
* Return value: Buffer* (updated Buffer pointer upon success or NULL upon failure)
* Algorithm: Validates passed in pointer isn't null, reallocates the buffer size to the return value of b_limit +1 sets appropriate values to reflect the size and character increase in the current buffer
*/
Buffer* b_compact(Buffer* const pBD, char symbol)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		char * temp;/*Creates a character pointer named temp to hold the pointer of the realloc*/
		temp = realloc(pBD->cb_head, b_limit(pBD) + (1*sizeof(char))); /*Sets the temp char pointer to the return value of realloc*/
		if (temp) /*If the realloc was successful, the contents run*/
		{
			pBD->flags = pBD->flags | CHECK_R_FLAG; /*Using bitwise operators, the r_flag is set indicating a cb_head location change*/
			pBD->cb_head = temp; /*Sets the current cb_head pointer to the temp pointer created from malloc*/
			pBD->capacity = pBD->addc_offset + 1; /*Sets the buffer capacity to the current addc_offset size + 1*/
			pBD->cb_head[pBD->addc_offset] = symbol; /*Adds the char symbol to the index value of addc_offset*/
			pBD->addc_offset++; /*Increments addc_offset*/
			pBD->flags = pBD->flags & RESET_R_FLAG; /*Resets the r_flag to 0 because a new character has been added*/
			return pBD; /*Return the updated buffer pointer pBD*/
		}
	}
	return NULL;
}
/*
* Purpose: is to return the value of the r_flag bit using bitwise operations.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: char (returns the value of the r_flag bit using bitwise operators)
* Algorithm:
*/
char b_rflag(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		return pBD->flags & SET_R_FLAG; /*Using bitwise operations, returns the value of the r_flag bit*/
	}
	return RT_FAIL_1; /*Returns the constant value RT_FAIL_1 upon runtime failure*/
}
/*
* Purpose: is to decrement the value of getc_offset.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: short (returns the new decremented value of getc_offset or RT_FAIL_1 if a runtime error occurs)
* Algorithm:
*/
short b_retract(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		pBD->getc_offset--; /*Decrements getc_offset*/
		return pBD->getc_offset; /*Returns the value of getc_offset*/
	}
	return RT_FAIL_1; /*returns value of constant RT_FAIL_1*/
}
/*
* Purpose: is to set the value of getc_offset to the current value of markc_offset.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: short (returns the value of getc_offset upon success or value of constant RT_FAIL_1 upon failure)
* Algorithm:
*/
short b_reset(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		pBD->getc_offset = pBD->markc_offset; /*Sets the value of getc_offset to the current value of markc_offset*/
		return pBD->getc_offset; /*Returns the current value of getc_offset*/
	}
	return RT_FAIL_1;/*returns the value of the constant RT_FAIL_1*/
}
/*
* Purpose: is to return the current value of getc_offset.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: short (returns the current value of getc_offset)
* Algorithm:
*/
short b_getcoffset(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		return pBD->getc_offset; /*returns the value of getc_offset*/
	}
	return RT_FAIL_1; /*Returns the constant value RT_FAIL_1 upon runtime failure*/
}
/*
* Purpose: is to reset the value of getc_offset and markc_offset back to 0.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: int (0 upon success or the constant value RT_FAIL_1 upon runtime failure)
* Algorithm:
*/
int b_rewind(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		if (pBD->getc_offset != NULL || pBD->getc_offset == 0)/*If getc_offset isn't null or if it does equal 0, sets the value of getc_offset to 0*/
		{
			pBD->getc_offset = 0;

		}
		else
		{
			return RT_FAIL_1;/*Returns the constant value RT_FAIL_1 upon failure*/
		}
		if (pBD->markc_offset != NULL || pBD->markc_offset == 0) /*If markc_offset isn't null or if it does equal 0, sets the value of getc_offset to 0*/
		{
			pBD->markc_offset = 0;
		}
		else
		{
			return RT_FAIL_1;/*Returns the constant value RT_FAIL_1 upon failure*/
		}
		return 0;
	}
	return RT_FAIL_1; /*Returns the constant value RT_FAIL_1 upon runtime failure*/
}
/*
* Purpose: is to return the character pointer for the index of markc_offset within the cb_head pointer.
* Author: Phillip Clarke
* Hystory/Versions: V1.0 - Oct 2nd, 2019
* Called functions:
* Parameters: Buffer* const pBD (buffer pointer)
* Return value: char * (character pointer location of the index of markc_offset upon success or NULL upon failure)
* Algorithm:
*/
char* b_location(Buffer* const pBD)
{
	if (pBD) /*Validates the pointer passed in is not NULL*/
	{
		return pBD->cb_head[pBD->markc_offset];
	}
	return NULL;
}

