/*<HTML><HEAD><TITLE> fortran.c </TITLE></HEAD><BODY><PRE>
********************************************************************************
* fortran.c -- Routines for handling C pointers inside FORTRAN programs.
*
* Programmer routines:
*	FORT_Init()		initializes C runtime libraries from FORTRAN.
*	FORT_AddPointer()	saves a C pointer and returns an index.
*	FORT_GetPointer()	returns the C pointer given the index.
*	FORT_FreePointer()	removes a C pointer from the list.
*
* Mark Showalter, PDS Rings Node, January 1997
*******************************************************************************/
#include "fortran.h"

/* Static variables holding C pointers and FORTRAN indices */

static RL_INT4	  ZFORT_IndexCount = 0;		/* number of pointers in use */
static RL_INT4	  ZFORT_ListCount  = 0;		/* size of tables */
static RL_INT4	 *ZFORT_Indices    = NULL;	/* List of available slots in
						 * pointer table */
static RL_VOID	**ZFORT_Pointers   = NULL;	/* Pointer table */

#define	ZFORT_StepSize 	100			/* number of slots to add at one
						 * time */

/* Protoypes of internal routines */

static RL_BOOL	ZFORT_ExpandTable RL_PROTO((RL_INT4 count));

#ifdef VMS
void DECC$CRTL_INIT(void);
#endif

/*<A NAME="FORT_Init"></A>
********************************************************************************
*$ Component_name:
*	FORT_Init (fortran.c)
*$ Abstract:
*	Initializes the FORTRAN interface to a library written in C.
*$ Keywords:
*	UTILITY, FORTRAN_C
*	C, INTERNAL, SUBROUTINE
*$ Declarations:
*	void	FORT_Init()
*$ Inputs:
*	none
*$ Outputs:
*	none
*$ Returns:
*	none
*$ Detailed_description:
*	This function initializes the FORTRAN interface to a library written in
*	C.  It initializes the C Runtime Library if necessary.
*$ External_references:
*	none
*$ Side_effects:
*	none
*$ Examples:
*	none
*$ Error_handling:
*	none
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node, NASA/Ames Research Center
*$ Version_and_date:
*	1.0: January 1994
*	1.1: January 1997
*$ Change_history:
*	1.1: Updated to support other RingLib components and to allocate more
*	     memory when needed.
*******************************************************************************/

void		FORT_Init()
{
static RL_BOOL	initialized = FALSE;

    /* Return immediately if initialization already occurred */
    if (initialized) return;

#ifdef VMS
    /* Initialize C runtime library if necessary */
    DECC$CRTL_INIT();
#endif

    /* Avoid further initializations */
    initialized = TRUE;

    return;
}

/*<A NAME="FORT_AddPointer"></A>
********************************************************************************
*$ Component_name:
*	FORT_AddPointer (fortran.c)
*$ Abstract:
*	Adds the given C pointer to an internal list and returns an integer
*	index.
*$ Keywords:
*	UTILITY, FORTRAN_C
*	C, INTERNAL, SUBROUTINE
*$ Declarations:
*	RL_INT4		FORT_AddPointer(pointer)
*	RL_VOID		*pointer;
*$ Inputs:
*	pointer		pointer to newly allocated memory.
*$ Outputs:
*	none
*$ Returns:
*	an integer index to pass back when the pointer is again needed, or zero
*	on failure.
*$ Detailed_description:
*	This function adds the given C pointer to an internal list and returns
*	an integer index.  It returns zero on failure.
*
*	The value returned serves as an integer "reference" for a pointer.
*	Subsequent calls to FORT_GetPointer() with this argument will return
*	the desired pointer.
*$ External_references:
*	none
*$ Side_effects:
*	An internal static array of pointers is modified.
*$ Examples:
*	none
*$ Error_handling:
*	The routine returns zero on failure.  RingLib error handling is also in
*	in effect.
*
*	RL_MEMORY_ERROR		on memory allocation failure.
*$ Limitations:
*	None.
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node, NASA/Ames Research Center
*$ Version_and_date:
*	1.0: January 1994
*	1.1: January 1997
*$ Change_history:
*	1.1: Updated to support other RingLib components and to allocate more
*	     memory when needed.
*******************************************************************************/

RL_INT4		FORT_AddPointer(pointer)
RL_VOID		*pointer;
{
RL_INT4		index;
RL_BOOL		status;

    /* Expand the table if necessary */
    if (ZFORT_IndexCount == ZFORT_ListCount) {
	if (ZFORT_ListCount == 0) FORT_Init();

	status = ZFORT_ExpandTable(ZFORT_StepSize);
	if (!status) return 0;
    }

    /* Get the next available pointer slot */
    index = ZFORT_Indices[ZFORT_IndexCount];

    /* Increment the pointer count */
    ZFORT_IndexCount++;

    /* Save the pointer value in the selected slot */
    ZFORT_Pointers[index] = pointer;

    /* Return the slot index (offset by one because zero is an error flag) */
    return (index+1);
}

/*<A NAME="FORT_GetPointer"></A>
********************************************************************************
*$ Component_name:
*	FORT_GetPointer (fortran.c)
*$ Abstract:
*	Returns the C pointer corresponding to an integer index.
*$ Keywords:
*	UTILITY, FORTRAN_C
*	C, INTERNAL, SUBROUTINE
*$ Declarations:
*	RL_VOID		*FORT_GetPointer(index)
*	RL_INT4		index;
*$ Inputs:
*	index		integer index identifying the desired pointer.
*$ Outputs:
*	none
*$ Returns:
*	the C pointer corresponding to the given index.
*$ Detailed_description:
*	This function returns the C pointer corresponding to an integer index.
*	If the index is out of range, a NULL is returned.
*$ External_references:
*	none
*$ Side_effects:
*	none
*$ Examples:
*	none
*$ Error_handling:
*	If the index is out of range, a NULL is returned and error condition
*	FORTRAN_POINTER_ERROR is raised.
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node, NASA/Ames Research Center
*$ Version_and_date:
*	1.0: January 1994
*	1.1: January 1997
*$ Change_history:
*	1.1: Updated to support other RingLib components and to allocate more
*	     memory when needed.
*******************************************************************************/

RL_VOID	*FORT_GetPointer(index)
RL_INT4	index;
{
RL_VOID	*ptr;

    if (index <= 0 || index >= ZFORT_ListCount) ptr = NULL;
    else                                        ptr = ZFORT_Pointers[index-1];

    if (ptr == NULL) {
	RL_RaiseError("FORTRAN_POINTER_ERROR", "invalid FORTRAN pointer");
    }

    return ptr;
}

/*<A NAME="FORT_FreePointer"></A>
********************************************************************************
*$ Component_name:
*	FORT_FreePointer (fortran.c)
*$ Abstract:
*	Removes a C pointer from the internal list and frees the index for
*	subsequent use.
*$ Keywords:
*	UTILITY, FORTRAN_C
*	C, INTERNAL, SUBROUTINE
*$ Declarations:
*	RL_VOID		*FORT_FreePointer(index)
*	RL_INT4		index;
*$ Inputs:
*	index		integer index identifying the desired pointer.
*$ Outputs:
*	none
*$ Returns:
*	the value of the pointer it has just removed from the list, or NULL on
*	error.
*$ Detailed_description:
*	This function removes a C pointer from the internal list and frees the
*	index for subsequent use.
*$ External_references:
*	RingLib
*$ Side_effects:
*	An internal static array of pointers is modified.
*$ Examples:
*	none
*$ Error_handling:
*	If the pointer index is out of range, RingLib error condition
*	FORTRAN_POINTER_ERROR is raised and a NULL is returned.
*$ Limitations:
*	The program will fail in unexpected ways if one frees an index not
*	previously returned by FORT_AddPointer(), or frees the same index more
*	than once.  It will also fail if one attempts to use an index after it
*	has been freed.
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node, NASA/Ames Research Center
*$ Version_and_date:
*	1.0: January 1994
*	1.1: January 1997
*$ Change_history:
*	1.1: Updated to support other RingLib components and to allocate more
*	     memory when needed.
*******************************************************************************/

RL_VOID	*FORT_FreePointer(index)
RL_INT4	index;
{
RL_VOID	*ptr;

    /* Check for valid index */
    if (index <= 0 || index >= ZFORT_ListCount) {
	RL_RaiseError("FORTRAN_POINTER_ERROR", "invalid FORTRAN pointer");
	return NULL;
    }

    /* Decrement the pointer count after testing */
    if (ZFORT_IndexCount <= 0) {
	RL_RaiseError("FORTRAN_POINTER_ERROR",
                      "FORTRAN pointer list underflow");
	return NULL;
    }

    ZFORT_IndexCount--;

    /* Save the newly-available slot index at the top of the list */
    ZFORT_Indices[ZFORT_IndexCount] = index-1;

    /* Reset the old pointer value */
    ptr = ZFORT_Pointers[index-1];
    ZFORT_Pointers[index-1] = NULL;

    /* Return the old pointer value */
    return ptr;
}

/*<A NAME="ZFORT_ExpandTable"></A>
********************************************************************************
* ZFORT_ExpandTable(count)
*
* This internal routine expands the pointer table size by a given number of
* entries.
*
* Inputs:
*	count		number of elements to add to table.
*
* Return:		TRUE on success; FALSE on failure.
*
* Errors:
*	RL_MEMORY_ERROR		on allocation failure.  In this case the pointer
*				table is unchanged.
*******************************************************************************/

static RL_BOOL	ZFORT_ExpandTable(count)
RL_INT4		count;
{
RL_VOID		**temp_pointers;
RL_INT4		*temp_indices;
RL_INT4		temp_listcount, index;

    /* Allocate memory for the tables */
    if (ZFORT_ListCount == 0) {
	temp_listcount = count;
	temp_pointers  = (RL_VOID **) XRL_Malloc(count * sizeof(RL_VOID *));
	temp_indices   = (RL_INT4 *)  XRL_Malloc(count * sizeof(RL_INT4));
    } else {
	temp_listcount = ZFORT_ListCount + count;
	temp_pointers  = (RL_VOID **) XRL_Realloc((RL_VOID *) ZFORT_Pointers,
				temp_listcount * sizeof(RL_VOID *));
	temp_indices   = (RL_INT4 *)  XRL_Realloc((RL_VOID *) ZFORT_Indices,
				temp_listcount * sizeof(RL_INT4));
    }

    if (temp_pointers == NULL || temp_indices == NULL) {
	XRL_Free((RL_VOID *) temp_pointers);
	XRL_Free((RL_VOID *) temp_indices);
	return FALSE;
    }

    /* Update tables */
    ZFORT_ListCount = temp_listcount;
    ZFORT_Pointers  = temp_pointers;
    ZFORT_Indices   = temp_indices;

    for (index = ZFORT_ListCount - count; index < ZFORT_ListCount; index++) {
	ZFORT_Indices[index] = index;
	ZFORT_Pointers[index] = NULL;
    }

    return TRUE;
}

/*******************************************************************************
</PRE></BODY></HTML>*/
