/*<HTML><HEAD><TITLE> rlmemory.c </TITLE></HEAD><BODY><PRE>
********************************************************************************
* rlmemory.c -- RingLib memory management routines.
*
* Mark Showalter & Neil Heather, April 1998
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "ringlib.h"

/*<A NAME="XRL_Malloc"></A>
********************************************************************************
*$ Component_name:
*	XRL_Malloc (rlmemory.c)
*$ Abstract:
*	This function allocates memory.
*$ Keywords:
*	RINGLIB, MEMORY
*	C, PUBLIC, SUBROUTINE
*$ Declarations:
*	RL_VOID		*XRL_Malloc(size)
*	RL_SIZE		size;
*$ Inputs:
*	size            number of bytes of memory to be allocated.
*$ Outputs:
*	none
*$ Returns:
*	pointer to allocated memory, or NULL if unsuccessful.
*$ Detailed_description:
*	This function allocates memory.  It raises an RL_MEMORY_ERROR error
*	condition and returns NULL on allocation failure.
*$ External_references:
*	none
*$ Side_effects:
*	Memory is allocated.
*$ Examples:
*	none
*$ Error_handling:
*	Ringlib error handling is in effect.  RL_MEMORY_ERROR condition is
*	raised and a NULL is returned on memory allocation failure.
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.1: April 1998
*$ Change_history:
*	1.0: January 1997	original version
*	1.1: April 1998		updated to handle size=0 correctly.
*******************************************************************************/

RL_VOID	*XRL_Malloc(size)
RL_SIZE	size;
{
RL_VOID	*ptr;

    if (size == 0) return NULL;

    ptr = malloc(size);
    if (ptr == NULL)
	RL_RaiseError("RL_MEMORY_ERROR", "unable to allocate memory");

    return ptr;
}

/*<A NAME="XRL_Realloc"></A>
********************************************************************************
*$ Component_name:
*	XRL_Realloc (rlmemory.c)
*$ Abstract:
*	This function re-allocates memory, extending the number of bytes alloted
*	to a pointer.
*$ Keywords:
*	RINGLIB, MEMORY
*	C, PUBLIC, SUBROUTINE
*$ Declarations:
*	RL_VOID		*XRL_Realloc(pointer, size)
*	RL_VOID		*pointer;
*	RL_SIZE		size;
*$ Inputs:
*	pointer		a pointer to previously allocated memory.
*	size            new number of bytes of memory to be allocated.
*$ Outputs:
*	none
*$ Returns:
*	pointer to new allocated memory, containing the same data as the
*	previous one; NULL if unsuccessful.
*$ Detailed_description:
*	This function re-allocates memory, extending the number of bytes alloted
*	to a pointer.  It raises an RL_MEMORY_ERROR error condition and returns
*	NULL on allocation failure.  In this case the old pointer is still
*	usable.
*$ External_references:
*	none
*$ Side_effects:
*	Memory is allocated.
*$ Examples:
*	none
*$ Error_handling:
*	Ringlib error handling is in effect.  RL_MEMORY_ERROR condition is
*	raised and a NULL is returned on memory allocation failure.  In this
*	case, the old pointer is still usable.
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: January 1997
*$ Change_history:
*	none
*******************************************************************************/

RL_VOID	*XRL_Realloc(old, size)
RL_VOID	*old;
RL_SIZE	size;
{
RL_VOID	*ptr;

    ptr = realloc(old, size);
    if (ptr == NULL)
	RL_RaiseError("RL_MEMORY_ERROR", "unable to re-allocate memory");

    return ptr;
}

/*<A NAME="XRL_MustAlloc"></A>
********************************************************************************
*$ Component_name:
*	XRL_MustAlloc (rlmemory.c)
*$ Abstract:
*	This function allocates critical memory.  On failure, it aborts.
*$ Keywords:
*	RINGLIB, MEMORY
*	C, PUBLIC, SUBROUTINE
*$ Declarations:
*	RL_VOID		*XRL_MustAlloc(size)
*	RL_SIZE		size;
*$ Inputs:
*	size            number of bytes of memory to be allocated.
*$ Outputs:
*	none
*$ Returns:
*	pointer to allocated memory.
*$ Detailed_description:
*	This function allocates critical memory.  On failure, it aborts after
*	printing an error message.
*$ External_references:
*	none
*$ Side_effects:
*	Memory is allocated.
*$ Examples:
*	none
*$ Error_handling:
*	On failure, an error message is printed and the program aborts.
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: January 1997
*$ Change_history:
*	none
*******************************************************************************/

RL_VOID	*XRL_MustAlloc(size)
RL_SIZE	size;
{
RL_VOID	*ptr;

    ptr = (RL_VOID*) malloc(size);
    if (ptr == NULL) {
	RL_SetErrorType("RL_MEMORY_ERROR", RL_ABORT);
	RL_RaiseError("RL_MEMORY_ERROR", "unable to allocate critical memory");
    }

    return ptr;
}

/*<A NAME="XRL_Free"></A>
********************************************************************************
*$ Component_name:
*	XRL_Free (rlmemory.c)
*$ Abstract:
*	This function frees memory.
*$ Keywords:
*	RINGLIB, MEMORY
*	C, PUBLIC, SUBROUTINE
*$ Declarations:
*	void		XRL_Free(pointer)
*	RL_VOID		*pointer;
*$ Inputs:
*	pointer		pointer to memory previously allocated.
*$ Outputs:
*	none
*$ Returns:
*	none
*$ Detailed_description:
*	This function frees memory.  It does nothing if the pointer passed to it
*	is NULL.
*$ External_references:
*	none
*$ Side_effects:
*	Memory is freed.
*$ Examples:
*	none
*$ Error_handling:
*	none
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: January 1997
*$ Change_history:
*	none
*******************************************************************************/

void	XRL_Free(pointer)
RL_VOID	*pointer;
{
    if (pointer != NULL) free(pointer);
}

/*******************************************************************************
</PRE></BODY></HTML>*/
