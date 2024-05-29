/*<HTML><HEAD><TITLE> rlerrors.c </TITLE></HEAD><BODY><PRE>
********************************************************************************
* rlerrors.c -- RingLib error handling routines.
*
* Mark Showalter & Neil Heather, PDS Rings Node, March 1998
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ringlib.h"
#include "fortran.h"

/********************
 * Type definitions *
 ********************/

#define RL_ERROR_ID_LEN	31	/* max length of an error id, excluding null */

typedef struct RL_ERRORNODE_STRUCT {
    struct RL_ERRORNODE_STRUCT	*next;
    RL_CHAR			id[RL_ERROR_ID_LEN+1];
    RL_INT4			type;
} RL_ERRORNODE;

/********************
 * Static variables *
 ********************/

#define		ERROR_TYPE_LIST		32
#define		ERROR_TYPE_MASK		31

static RL_ERRORNODE RL_TypeInfo[ERROR_TYPE_LIST] = {
	{NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0},
	{NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0},
	{NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0},
	{NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0},
	{NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0},
	{NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0},
	{NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0},
	{NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0}, {NULL, "", 0}};

static RL_ERRORNODE RL_LastError = {NULL, "", 0};
static RL_ERRORNODE RL_TempNode  = {NULL, "", 0};
static FILE         *errfile     = NULL;

/********************************
 * Internal function prototypes *
 ********************************/

static RL_ERRORNODE *ZRL_FindNode   RL_PROTO((RL_CHAR *error_id,
                                              RL_ERRORNODE *top));
static RL_ERRORNODE *ZRL_MakeNode   RL_PROTO((RL_CHAR *error_id,
                                              RL_ERRORNODE *top));
static RL_ERRORNODE *ZRL_UnlinkNode RL_PROTO((RL_CHAR *error_id,
                                              RL_ERRORNODE *top));
static RL_INT4       ZRL_Hash       RL_PROTO((RL_CHAR *error_id));

/*<A NAME="RL_RaiseError"></A>
********************************************************************************
* EXPORTED ROUTINES
********************************************************************************
*$ Component_name:
*	RL_RaiseError (rlerrors.c)
*$ Abstract:
*	This routine raises an error in the manner specified by the error type.
*$ Keywords:
*	RINGLIB, ERRORS
*	C, PUBLIC, SUBROUTINE
*$ Declarations:
*	void		RL_RaiseError(error_id, message)
*	RL_CHAR		*error_id;
*	RL_CHAR		*message;
*$ Inputs:
*	error_id	string identifying the error category.
*	message 	specific message relating to this particular error.
*$ Outputs:
*	none
*$ Returns:
*	none
*$ Detailed_description:
*	This routine raises an error in the manner specified by the error type.
*	The error may or may not print a message and it may or may not be
*	recorded.  The default action is for the error message to be printed on
*	the stream specified by errfile and for the program to abort.
*
*	The behavior depends on the value of the error_type parameter associated
*	with the error code.  Options are:
*		RL_IGNORE = -1: don't print; don't record in stack.
*		RL_INFORM =  1:       print; don't record in stack.
*		RL_RECORD = -2: don't print;       record in stack.
*		RL_SIGNAL =  2:       print;       record in stack.
*		RL_ABORT  =  3:       print; abort program
*	RL_ABORT is the default behavior.  These constants are defined in the
*	include file ringlib.h.
*$ External_references:
*	none
*$ Side_effects:
*	Depending on the error type, an error may be pushed onto the top of the
*	stack, a message may be printed, and the program may abort.
*$ Examples:
*	This snippet of code raises a "DIV_BY_ZERO" error before the program
*	attempts to divide by zero.
*
*	if (x == 0.) {
*	    RL_RaiseError("DIV_BY_ZERO",
*		"You're attempting to divide by x when x is zero!");
*	    y = 0.;
*	}
*	else {
*	    y = 1. / x;
*	}
*$ Error_handling:
*	No error conditions are raised.  If out of critical memory, the program
*	prints a fatal error message and aborts.
*$ Limitations:
*	Error ids are limited to 31 characters, plus final null character.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
*******************************************************************************/

void		RL_RaiseError(error_id, message)
RL_CHAR		*error_id;
RL_CHAR		*message;
{
RL_ERRORNODE	*node;
RL_INT4		type;
RL_BOOL		qprint, qrecord, qabort;
RL_CHAR		*info;

    /* Search error-type tree for matching node */
    node = ZRL_FindNode(error_id, RL_TypeInfo + ZRL_Hash(error_id));

    /* Determine error type */
    if (node == NULL) type = RL_ABORT;		/* the default */
    else              type = node->type;

    /* Decide what to do */
    switch (type) {

    case RL_IGNORE:
	qprint = FALSE; qrecord = FALSE; qabort = FALSE;
	break;

    case RL_INFORM:
	qprint = TRUE;  qrecord = FALSE; qabort = FALSE;
	break;

    case RL_RECORD:
	qprint = FALSE; qrecord = TRUE;  qabort = FALSE;
	break;

    case RL_SIGNAL:
	qprint = TRUE;  qrecord = TRUE;  qabort = FALSE;
	break;

    case RL_ABORT:
    default:
	qprint = TRUE;  qrecord = FALSE; qabort = TRUE;
	break;
    }

    /* Record error condition if necessary */
    if (qrecord) {
	ZRL_MakeNode(error_id, &RL_LastError);
    }

    /* Print message if necessary */
    if (qprint) {
	if (qabort)       info = "fatal";
	else if (qrecord) info = "error";
	else              info = "warning";

	if (errfile == NULL)
	    fprintf(stderr,  "%s (%s): %s\n", error_id, info, message);
	else
	    fprintf(errfile, "%s (%s): %s\n", error_id, info, message);
    }

    /* Abort program if necessary */
    if (qabort) {
	exit(1);
    }
}

/*<A NAME="RL_PipeErrors"></A>
********************************************************************************
*$ Component_name:
*	RL_PipeErrors (rlerrors.c)
*$ Abstract:
*	This routine changes the file into which error messages are placed.  By
*	default, error messages are displayed on the "stderr" stream.
*$ Keywords:
*	RINGLIB, ERRORS
*	C, PUBLIC, SUBROUTINE
*$ Declarations:
*	void		RL_PipeErrors(filename)
*	RL_CHAR		*filename;
*$ Inputs:
*	filename	file where error messages are to be recorded.  Use blank
*			or NULL to direct messages back to "stderr".
*$ Outputs:
*	none
*$ Returns:
*	none
*$ Detailed_description:
*	This routine changes the file into which error messages are placed.  By
*	default, error messages are displayed on the "stderr" stream.
*$ External_references:
*	none
*$ Side_effects:
*	A file is opened if necessary.
*$ Examples:
*	This sends future error messages to file "errors.lis":
*		RL_PipeErrors("errors.lis");
*	This redirects future error messages to the terminal:
*		RL_PipeErrors(NULL);
*	or
*		RL_PipeErrors("");
*$ Error_handling:
*	RingLib error handling is in effect.  Conditions raised:
*	RL_OPEN_FAILURE		if file could not be opened for writing; in
*				this case stderr is used for this and future
*				messages.
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
*******************************************************************************/

void	RL_PipeErrors(filename)
RL_CHAR	*filename;
{
    /* Close current error file if necessary */
    if (errfile != NULL) {
	fclose(errfile);
        errfile = NULL;
    }

    /* Open new error file if necessary */
    if (filename == NULL || filename[0] == '\0') {
	errfile = NULL;
    } else {
	errfile = fopen(filename, "w+t");
	if (errfile == NULL) {
	    RL_RaiseError("RL_OPEN_FAILURE", "unable to open error file");
	}
    }
}

/*<A NAME="RL_TestError"></A>
********************************************************************************
*$ Component_name:
*	RL_TestError (rlerrors.c)
*$ Abstract:
*	This routine tests for the presence of an error on the top of the error
*	stack and returns a pointer to the error_id string.  It does not modify
*	the error stack.
*$ Keywords:
*	RINGLIB, ERRORS
*	C, PUBLIC, SUBROUTINE
*$ Declarations:
*	RL_CHAR	*RL_TestError(void)
*$ Inputs:
*	none
*$ Outputs:
*	none
*$ Returns:
*	pointer (read-only) to a string containing the error id, or NULL if no
*	error condition has been raised.
*$ Detailed_description:
*	This routine tests for the presence of an error on the top of the error
*	stack and returns a pointer to the error_id string.  If it does not find
*	an error, it returns a NULL.  It does not modify the error stack.
*$ External_references:
*	none
*$ Side_effects:
*	none
*$ Examples:
*	Suppose you call a routine Math() that might raise errors "DIV_BY_ZERO"
*	or "SQRT_NEGATIVE".  You wish to print your own error message if either
*	of these errors occurred, and then to remove them from the error stack.
*
*	RL_SetErrorType("DIV_BY_ZERO", RL_RECORD);
*	RL_SetErrorType("SQRT_NEGATIVE", RL_RECORD);
*	Math();
*	if (RL_TestError() != NULL) {
*	    printf("Error %s occurred!\n", RL_ClearError());
*	}
*$ Error_handling:
*	No error conditions are raised.
*$ Limitations:
*	Error ids are limited to 31 characters, plus final null character.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
*******************************************************************************/

RL_CHAR	*RL_TestError()
{
    if (RL_LastError.next == NULL)	/* stack is empty */
	return NULL;
    else
	return (RL_LastError.next->id);
}

/*<A NAME="RL_ClearError"></A>
********************************************************************************
*$ Component_name:
*	RL_ClearError (rlerrors.c)
*$ Abstract:
*	This routine removes the most recent error from the top of the error
*	stack and returns a pointer to the error_id string.
*$ Keywords:
*	RINGLIB, ERRORS
*	C, PUBLIC, SUBROUTINE
*$ Declarations:
*	RL_CHAR	*RL_ClearError(void)
*$ Inputs:
*	none
*$ Outputs:
*	none
*$ Returns:
*	pointer (read-only) to a string containing the error id, or NULL if no
*	error condition has been raised.
*$ Detailed_description:
*	This routine removes the most recent error from the top of the error
*	stack and returns a pointer to the error_id string.  If it does not find
*	an error, it returns a NULL.
*$ External_references:
*	none
*$ Side_effects:
*	The first error is removed from the stack and freed.
*$ Examples:
*	Suppose you call a routine Math() that might raise errors "DIV_BY_ZERO"
*	or "SQRT_NEGATIVE".  You wish to print your own error message if either
*	of these errors occurred, and then to remove them from the error stack.
*
*	RL_SetErrorType("DIV_BY_ZERO", RL_RECORD);
*	RL_SetErrorType("SQRT_NEGATIVE", RL_RECORD);
*	Math();
*	if (RL_TestError() != NULL) {
*	    printf("Error %s occurred!\n", RL_ClearError());
*	}
*$ Error_handling:
*	No error conditions are raised.
*$ Limitations:
*	Error ids are limited to 31 characters, plus final null character.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
*******************************************************************************/

RL_CHAR *RL_ClearError()
{
RL_ERRORNODE *top;

    /* If stack is empty, return NULL */
    if (RL_LastError.next == NULL) return NULL;

    /* Otherwise pop stack and return error id */
    top = RL_LastError.next;				/* save top pointer */
    RL_TempNode = *top;					/* copy top node */
    RL_LastError.next = RL_LastError.next->next;	/* unlink */
    XRL_Free(top);					/* free */
    return RL_TempNode.id;				/* return id pointer */
}

/*<A NAME="RL_TestError1"></A>
********************************************************************************
*$ Component_name:
*	RL_TestError1 (rlerrors.c)
*$ Abstract:
*	This routine tests for the presence of a particular error on the error
*	stack.  If returns TRUE if it is found and FALSE otherwise.
*$ Keywords:
*	RINGLIB, ERRORS
*	C, PUBLIC, SUBROUTINE
*$ Declarations:
*	RL_BOOL		RL_TestError1(error_id)
*	RL_CHAR		*error_id;
*$ Inputs:
*	error_id	pointer to a string containing the error id.
*$ Outputs:
*	none
*$ Returns:
*	TRUE if the error was found on the stack; FALSE otherwise.
*$ Detailed_description:
*	This routine tests for the presence of a particular error on the error
*	stack.  If returns TRUE if it is found and FALSE otherwise.  It does not
*	modify the error stack.
*$ External_references:
*	none
*$ Side_effects:
*	none
*$ Examples:
*	Suppose you call a routine Math() that might raise error "DIV_BY_ZERO".
*	You wish to print your own error message if this occurs, and then leave
*	it on the error stack for another routine to check.
*
*	RL_SetErrorType("DIV_BY_ZERO", RL_RECORD);
*	Math();
*	if (RL_TestError1("DIV_BY_ZERO")) {
*	    printf("You tried to divide by zero!\n");
*	}
*$ Error_handling:
*	No error conditions are raised.
*$ Limitations:
*	Error ids are limited to 31 characters, plus final null character.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
*******************************************************************************/

RL_BOOL		RL_TestError1(error_id)
RL_CHAR		*error_id;
{
RL_ERRORNODE	*node;

    node = ZRL_FindNode(error_id, &RL_LastError);

    if (node == NULL) return FALSE;
    else              return TRUE;
}

/*<A NAME="RL_ClearError1"></A>
********************************************************************************
*$ Component_name:
*	RL_ClearError1 (rlerrors.c)
*$ Abstract:
*	This routine removes a particular error from the error stack.  It
*	returns TRUE if the error was found and FALSE otherwise.
*$ Keywords:
*	RINGLIB, ERRORS
*	C, PUBLIC, SUBROUTINE
*$ Declarations:
*	RL_BOOL		RL_ClearError1(error_id)
*	RL_CHAR		*error_id;
*$ Inputs:
*	error_id	pointer to a string containing the error id.
*$ Outputs:
*	none
*$ Returns:
*	TRUE if the error was found; FALSE otherwise.
*$ Detailed_description:
*	This routine removes a particular error from the error stack.  It
*	returns TRUE if the error was found and FALSE otherwise.
*$ External_references:
*	none
*$ Side_effects:
*	An error may be removed from the stack and freed.
*$ Examples:
*	Suppose you call a routine Math() that might raise error "DIV_BY_ZERO".
*	You wish to print your own error message if this occurs, and then
*	remove it from the error stack.
*
*	RL_SetErrorType("DIV_BY_ZERO", RL_RECORD);
*	Math();
*	if (RL_ClearError1("DIV_BY_ZERO")) {
*	    printf("You tried to divide by zero!\n");
*	}
*$ Error_handling:
*	No error conditions are raised.
*$ Limitations:
*	Error ids are limited to 31 characters, plus final null character.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
*******************************************************************************/

RL_BOOL		RL_ClearError1(error_id)
RL_CHAR		*error_id;
{
RL_ERRORNODE	*node;

    /* Find the desired node and unlink it from the stack */
    node = ZRL_UnlinkNode(error_id, &RL_LastError);

    /* If the node was not found, return FALSE */
    if (node == NULL) return FALSE;

    /* Otherwise, free it and return TRUE */
    XRL_Free(node);
    return TRUE;
}

/*<A NAME="RL_SetErrorType"></A>
********************************************************************************
*$ Component_name:
*	RL_SetErrorType (rlerrors.c)
*$ Abstract:
*	This routine sets the type for a particular error.  It also returns the
*	current value of the error type.
*$ Keywords:
*	RINGLIB, ERRORS
*	C, PUBLIC, SUBROUTINE
*$ Declarations:
*	RL_INT4		RL_SetErrorType(error_id, error_type)
*	RL_CHAR		*error_id;
*	RL_INT4		error_type;
*$ Inputs:
*	error_id	string identifying the error id.
*	error_type	value indicating how the error is to be handled.
*			Options are:
*			    RL_IGNORE = -1: don't print; don't record in stack.
*			    RL_INFORM =  1:       print; don't record in stack.
*			    RL_RECORD = -2: don't print;       record in stack.
*			    RL_SIGNAL =  2:       print;       record in stack.
*			    RL_ABORT  =  3:       print; abort program
*			These constants are defined in the include file
*			ringlib.h.
*$ Outputs:
*	none
*$ Returns:
*	the previous error_type for this error_id.
*$ Detailed_description:
*	This routine sets the type for a particular error.  It also returns the
*	current value of the error type.
*
*	The error type controls what happens when RL_RaiseError() is called.
*	Error type values are:
*		RL_IGNORE = -1: don't print; don't record in stack.
*		RL_INFORM =  1:       print; don't record in stack.
*		RL_RECORD = -2: don't print;       record in stack.
*		RL_SIGNAL =  2:       print;       record in stack.
*		RL_ABORT  =  3:       print; abort program
*	RL_ABORT is the default behavior.  These constants are defined in the
*	include file ringlib.h.
*$ External_references:
*	none
*$ Side_effects:
*	Depending on the error type, an error may be removed from the stack and
*	freed.
*$ Examples:
*	Suppose you wish to change temporarily the method a program uses to
*	handle DIV_BY_ZERO errors to RL_RECORD, and then to restore it to the
*	previous method when you're done.
*
*	RL_INT4		old_type;
*
*	old_type = RL_SetErrorType("DIV_BY_ZERO", RL_RECORD);
*	... (DIV_BY_ZERO is now set to record)
*	RL_SetErrorType("DIV_BY_ZERO", old_type);
*	... (previous type of DIV_BY_ZERO is restored)
*$ Error_handling:
*	No error conditions are raised.
*$ Limitations:
*	Error ids are limited to 31 characters, plus final null character.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
*******************************************************************************/

RL_INT4		RL_SetErrorType(error_id, error_type)
RL_CHAR		*error_id;
RL_INT4		error_type;
{
RL_ERRORNODE	*node;
RL_INT4		save_type;

    /* On ABORT, free node */
    if (error_type == RL_ABORT) {
	node = ZRL_UnlinkNode(error_id, RL_TypeInfo + ZRL_Hash(error_id));
	if (node == NULL) {
	    save_type = RL_ABORT;
	} else {
	    save_type = node->type;
	    XRL_Free((RL_VOID *) node);
	}

    /* Otherwise, modify node */
    } else {
	node = ZRL_MakeNode(error_id, RL_TypeInfo + ZRL_Hash(error_id));
	save_type = node->type;
	node->type = error_type;
    }

    return save_type;
}

/*<A NAME="RL_GetErrorType"></A>
********************************************************************************
*$ Component_name:
*	RL_GetErrorType (rlerrors.c)
*$ Abstract:
*	This routine returns the error type for a particular error id.
*$ Keywords:
*	RINGLIB, ERRORS
*	C, PUBLIC, SUBROUTINE
*$ Declarations:
*	RL_INT4		RL_GetErrorType(error_id)
*	RL_CHAR		*error_id;
*$ Inputs:
*	error_id	string identifying the error id.
*$ Outputs:
*	none
*$ Returns:
*	the error type for the given error id.
*$ Detailed_description:
*	This routine returns the error type for a particular error id.
*
*	The error type controls what happens when RL_RaiseError() is called.
*	Error type values are:
*		RL_IGNORE = -1: don't print; don't record in stack.
*		RL_INFORM =  1:       print; don't record in stack.
*		RL_RECORD = -2: don't print;       record in stack.
*		RL_SIGNAL =  2:       print;       record in stack.
*		RL_ABORT  =  3:       print; abort program
*	RL_ABORT is the default behavior.  These constants are defined in the
*	include file ringlib.h.
*$ External_references:
*	none
*$ Side_effects:
*	none
*$ Examples:
*	Suppose you call a routine Math() that might raise error "DIV_BY_ZERO".
*	Without changing the type of this error permanently, you wish to make
*	sure a message is printed.
*
*	RL_INT4		old_type;
*
*	old_type = RL_GetErrorType("DIV_BY_ZERO");
*	if (old_type < 0) RL_SetErrorType("DIV_BY_ZERO", -old_type);
*	Math();
*	RL_SetErrorType("DIV_BY_ZERO", old_type);
*$ Error_handling:
*	No error conditions are raised.
*$ Limitations:
*	Error ids are limited to 31 characters, plus final null character.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
*******************************************************************************/

RL_INT4		RL_GetErrorType(error_id)
RL_CHAR		*error_id;
{
RL_ERRORNODE	*node;

    node = ZRL_FindNode(error_id, RL_TypeInfo + ZRL_Hash(error_id));

    if (node == NULL) return RL_ABORT;
    else              return node->type;
}

/*<A NAME="ZRL_FindNode"></A>
********************************************************************************
* INTERNAL ROUTINES
********************************************************************************
* RL_ERRORNODE *ZRL_FindNode(error_id, top)
*
* This internal function finds the node with matching error_id in a linked list
* of error nodes.
*
* Input:
*	error_id	error id to find in list.
*	top		pointer to a node whose "next" field points to the first
*			entry in list.
*
* Return:		pointer to the error node, or NULL if it was not found.
*******************************************************************************/

static RL_ERRORNODE *ZRL_FindNode(error_id, top)
RL_CHAR		*error_id;
RL_ERRORNODE	*top;
{
int		index;
RL_ERRORNODE	*node;

    /* Walk down list looking for matching node */
    for(node = top->next; node != NULL; node = node->next)
	if (strcmp(error_id, node->id) == 0) return node;

    /* If not found, return NULL */
    return NULL;
}

/*<A NAME="ZRL_MakeNode"></A>
********************************************************************************
* RL_ERRORNODE *ZRL_MakeNode(error_id, top)
*
* This internal function finds the node with matching error_id in a linked list
* of error nodes.  If it does not find a matching node, it creates one and
* adds it to the list.
*
* Input:
*	error_id	error id to find in list.
*	top		pointer to a node whose "next" field points to the first
*			entry in list.
*
* Return:		pointer to the error node.
*******************************************************************************/

static RL_ERRORNODE *ZRL_MakeNode(error_id, top)
RL_CHAR		*error_id;
RL_ERRORNODE	*top;
{
RL_ERRORNODE	*node, *root;

    /* Look for node and return it if found */
    node = ZRL_FindNode(error_id, top);
    if (node != NULL) return node;

    /* Allocate a new node and put it at top of list
     * Note that there is danger of infinite recursion here; if XRL_Malloc()
     * fails but RL_MEMORY_ERROR is set to record without aborting, then it will
     * recursively call this same routine ad infinitum.  We therefore abort if
     * this allocation fails. */
    node = (RL_ERRORNODE *) XRL_MustAlloc(sizeof(RL_ERRORNODE));

    node->next = top->next;
    top->next = node;

    /* Initialize node */
    strncpy(node->id, error_id, RL_ERROR_ID_LEN);
    node->id[RL_ERROR_ID_LEN] = '\0';		/* ensure null termination */
    node->type = RL_ABORT;

    return node;
}

/*<A NAME="ZRL_UnlinkNode"></A>
********************************************************************************
* RL_ERRORNODE *ZRL_UnlinkNode(error_id, top)
*
* This internal function finds the node giving error type for the specified
* error id.  If found, the node is removed from the linked list.
*
* Input:
*	error_id	error id to find in list.
*	top		pointer to a node whose "next" field points to the first
*			entry in list.
*
* Return:		pointer to the error node, or NULL if it is not found.
*
* Side effects:		node is unlinked from the list but is not freed.
*******************************************************************************/

static RL_ERRORNODE *ZRL_UnlinkNode(error_id, top)
RL_CHAR		*error_id;
RL_ERRORNODE	*top;
{
RL_ERRORNODE	*parent, *node;

    /* Walk down list looking for matching node */
    for (parent = top; parent->next != NULL; parent = parent->next) {

	if (strcmp(error_id, parent->next->id) == 0) {

	    /* Unlink node and return it */
	    node = parent->next;
	    parent->next = node->next;
	    return node;
	}
    }

    /* Node was not found */
    return NULL;
}

/*<A NAME="ZRL_Hash"></A>
********************************************************************************
* ZRL_Hash(error_id)
*
* This internal function calculates a quick hash value between 0 and 31 based on
* the given error id string.
*
* Inputs:
*	error_id	error id.
*
* Return:		pointer to the error node, or NULL if it is not found.
*
* Side effects:		node is unlinked from the list but is not freed.
*******************************************************************************/

static RL_INT4	ZRL_Hash(error_id)
RL_CHAR		*error_id;
{
RL_INT4		hash, i;

    hash = 0;
    for (i = 0; i < 8; i++) {
	if (error_id[i] == '\0') break;
	hash = 5*hash + (RL_INT4) error_id[i];
    }

    return (hash & ERROR_TYPE_MASK);
}

/*<A NAME="FRL_RaiseError"></A>
********************************************************************************
* FORTRAN INTERFACE ROUTINES
********************************************************************************
*$ Component_name:
*	FRL_RaiseError (rlerrors.c, fringlib.for)
*$ Abstract:
*	This routine raises an error in the manner specified by the error type.
*$ Keywords:
*	RINGLIB, ERRORS
*	FORTRAN, PUBLIC, SUBROUTINE
*$ Declarations:
*	subroutine FRL_RaiseError(error_id, message)
*	character*(*)	error_id, message
*$ Inputs:
*	error_id	string identifying the error id.
*	message 	specific message relating to this particular error.
*$ Outputs:
*	none
*$ Returns:
*	none
*$ Detailed_description:
*	This routine raises an error in the manner specified by the error type.
*	The error may or may not print a message and it may or may not be
*	recorded.  The default action is for the error message to be printed on
*	the stream specified by errfile and for the program to abort.
*
*	The behavior depends on the value of the error_type parameter associated
*	with the error code.  Options are:
*		RL_IGNORE = -1: don't print; don't record in stack.
*		RL_INFORM =  1:       print; don't record in stack.
*		RL_RECORD = -2: don't print;       record in stack.
*		RL_SIGNAL =  2:       print;       record in stack.
*		RL_ABORT  =  3:       print; abort program
*	RL_ABORT is the default behavior.  These constants are defined in the
*	include file ringlib.h.
*$ External_references:
*	none
*$ Side_effects:
*	Depending on the error type, an error may be pushed onto the top of the
*	stack, a message may be printed, and the program may abort.
*$ Examples:
*	This snippet of code raises a "DIV_BY_ZERO" error before the program
*	attempts to divide by zero.
*
*	if (x .eq. 0.) then
*	    call FRL_RaiseError('DIV_BY_ZERO',
*		'You're attempting to divide by x when x is zero!')
*	    y = 0.
*	else
*	    y = 1. / x
*	end if
*$ Error_handling:
*	No error conditions are raised.  If out of critical memory, the program
*	prints a fatal error message and aborts.
*$ Limitations:
*	Error ids are limited to 31 characters; messages are limited to 255
*	characters.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
********************************************************************************
* Note: GRL_RaiseError() defined here is the intermediary routine between
* FRL_RaiseError() and RL_RaiseError(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fringlib.for for the
* rest of the code.
*
* subroutine GRL_RaiseError(error_id, message)
* byte		error_id(*), message(*)
*******************************************************************************/

void	FORTRAN_NAME(grl_raiseerror) (error_id, message)
RL_CHAR	*error_id, *message;
{
    RL_RaiseError(error_id, message);
}

/*<A NAME="FRL_PipeErrors"></A>
********************************************************************************
*$ Component_name:
*	FRL_PipeErrors (rlerrors.c, fringlib.for)
*$ Abstract:
*	This routine changes the file into which error messages are placed.  By
*	default, error messages are displayed on the terminal.
*$ Keywords:
*	RINGLIB, ERRORS
*	FORTRAN, PUBLIC, SUBROUTINE
*$ Declarations:
*	subroutine FRL_PipeErrors(filename)
*	character*(*)	filename
*$ Inputs:
*	filename	file where error messages are to be recorded.  Use a
*			blank string to direct messages back to the terminal.
*$ Outputs:
*	none
*$ Returns:
*	none
*$ Detailed_description:
*	This routine changes the file into which error messages are placed.  By
*	default, error messages are displayed on the terminal.
*$ External_references:
*	none
*$ Side_effects:
*	A file is opened if necessary.
*$ Examples:
*	This sends future error messages to file "errors.lis":
*		call FRL_PipeErrors('errors.lis')
*	This redirects future error messages to the terminal:
*		call FRL_PipeErrors(' ')
*$ Error_handling:
*	RingLib error handling is in effect.  Conditions raised:
*	RL_OPEN_FAILURE		if file could not be opened for writing; in
*				this case the terminal is used for this and
*				future messages.
*$ Limitations:
*	File names are limited to 255 characters.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
********************************************************************************
* Note: GRL_PipeErrors() defined here is the intermediary routine between
* FRL_PipeErrors() and RL_PipeErrors(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fringlib.for for the
* rest of the code.
*
* subroutine GRL_PipeErrors(filename)
* byte		filename(*)
*******************************************************************************/

void	FORTRAN_NAME(grl_pipeerrors) (filename)
RL_CHAR *filename;
{
    RL_PipeErrors(filename);
}

/*<A NAME="FRL_TestError"></A>
********************************************************************************
*$ Component_name:
*	FRL_TestError (rlerrors.c, fringlib.for)
*$ Abstract:
*	This routine tests for the presence of an error on the top of the error
*	stack and returns the error_id string.
*$ Keywords:
*	RINGLIB, ERRORS
*	FORTRAN, PUBLIC, SUBROUTINE
*$ Declarations:
*	logical*4 function FRL_TestError(error_id)
*	character*(*)	error_id
*$ Inputs:
*	none
*$ Outputs:
*	error_id	string containing the error id, or blank if no error
*			condition is found.
*$ Returns:
*	.TRUE. if an error condition was found on the stack; .FALSE. otherwise.
*$ Detailed_description:
*	This routine tests for the presence of an error on the top of the error
*	stack and returns the error_id string.  If it does not find an error, it
*	returns a blank string.  It does not modify the error stack.  It also
*	returns .TRUE. if an error was found and .FALSE. otherwise.
*$ External_references:
*	none
*$ Side_effects:
*	none
*$ Examples:
*	Suppose you call a routine Math() that might raise errors "DIV_BY_ZERO"
*	or "SQRT_NEGATIVE".  You wish to print your own error message if either
*	of these errors occurred, and then to remove them from the error stack.
*
*	character*40	id
*
*	call FRL_SetErrorType('DIV_BY_ZERO', RL_RECORD)
*	call FRL_SetErrorType('SQRT_NEGATIVE', RL_RECORD)
*	call Math()
*	if (FRL_TestError(id)) then
*	    write(*,*) 'Error ', id, ' occurred!'
*	    call FRL_ClearError(id)
*	end if
*$ Error_handling:
*	No error conditions are raised.
*$ Limitations:
*	Error ids are limited to 31 characters.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
********************************************************************************
* Note: GRL_TestError() defined here is the intermediary routine between
* FRL_TestError() and RL_TestError(), allowing for the fact that strings cannot
* be passed directly between FORTRAN and C.  See fringlib.for for the rest of
* the code.
*
* logical*4 function GRL_TestError(buffer, lbuffer)
* byte		buffer(*)
* integer*4	lbuffer
*******************************************************************************/

RL_INT4 FORTRAN_NAME(grl_testerror) (buffer, lbuffer)
RL_CHAR *buffer;
RL_INT4 *lbuffer;
{
RL_CHAR	*error_id;

    error_id = RL_TestError();

    if (error_id == NULL) {
	buffer[0] = '\0';
	return FFALSE;
    }

    strncpy(buffer, error_id, *lbuffer-1);
    buffer[*lbuffer-1] = '\0';

    return FTRUE;
}

/*<A NAME="FRL_ClearError"></A>
********************************************************************************
*$ Component_name:
*	FRL_ClearError (rlerrors.c, fringlib.for)
*$ Abstract:
*	This routine removes the most recent error from the top of the error
*	stack and returns the error_id string.
*$ Keywords:
*	RINGLIB, ERRORS
*	FORTRAN, PUBLIC, SUBROUTINE
*$ Declarations:
*	logical*4 function FRL_ClearError(error_id)
*	character*(*)	error_id
*$ Inputs:
*	none
*$ Outputs:
*	error_id	string containing the error id, or blank if no error
*			condition was found.
*$ Returns:
*	.TRUE. if an error condition was found on the stack; .FALSE. otherwise.
*$ Detailed_description:
*	This routine removes the most recent error from the top of the error
*	stack and returns the error_id string.  If it does not find an error,
*	it returns a blank string.  It also returns .TRUE. if an error was
*	found and .FALSE. otherwise.
*$ External_references:
*	none
*$ Side_effects:
*	The first error is removed from the stack and freed.
*$ Examples:
*	Suppose you call a routine Math() that might raise errors "DIV_BY_ZERO"
*	or "SQRT_NEGATIVE".  You wish to print your own error message if either
*	of these errors occurred, and then to remove them from the error stack.
*
*	character*40	id
*
*	call FRL_SetErrorType('DIV_BY_ZERO', RL_RECORD)
*	call FRL_SetErrorType('SQRT_NEGATIVE', RL_RECORD)
*	call Math()
*	if (FRL_TestError(id)) then
*	    write(*,*) 'Error ', id, ' occurred!'
*	    call FRL_ClearError(id)
*	end if
*$ Error_handling:
*	No error conditions are raised.
*$ Limitations:
*	Error ids are limited to 31 characters.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
********************************************************************************
* Note: GRL_ClearError() defined here is the intermediary routine between
* FRL_ClearError() and RL_ClearError(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fringlib.for for the
* rest of the code.
*
* logical*4 function GRL_ClearError(buffer, lbuffer)
* byte		buffer(*)
* integer*4	lbuffer
*******************************************************************************/

RL_INT4 FORTRAN_NAME(grl_clearerror) (buffer, lbuffer)
RL_CHAR *buffer;
RL_INT4 *lbuffer;
{
RL_CHAR	*error_id;

    error_id = RL_ClearError();

    if (error_id == NULL) {
	buffer[0] = '\0';
	return FFALSE;
    }

    strncpy(buffer, error_id, *lbuffer-1);
    buffer[*lbuffer-1] = '\0';

    return FTRUE;
}

/*<A NAME="FRL_TestError1"></A>
********************************************************************************
*$ Component_name:
*	FRL_TestError1 (rlerrors.c, fringlib.for)
*$ Abstract:
*	This routine tests for the presence of a particular error on the error
*	stack.  It returns .TRUE. if it is found and .FALSE. otherwise.
*$ Keywords:
*	RINGLIB, ERRORS
*	FORTRAN, PUBLIC, SUBROUTINE
*$ Declarations:
*	logical*4 function FRL_TestError1(error_id)
*	character*(*)	error_id
*$ Inputs:
*	error_id	string containing the error id to test.
*$ Outputs:
*	none
*$ Returns:
*	.TRUE. if the error was found on the stack; .FALSE. otherwise.
*$ Detailed_description:
*	This routine tests for the presence of a particular error on the error
*	stack.  It returns .TRUE. if it is found and .FALSE. otherwise.  It does
*	not modify the error stack.
*$ External_references:
*	none
*$ Side_effects:
*	none
*$ Examples:
*	Suppose you call a routine Math() that might raise error "DIV_BY_ZERO".
*	You wish to print your own error message if this occurs, and then leave
*	it on the error stack for another routine to check.
*
*	call FRL_SetErrorType('DIV_BY_ZERO', RL_RECORD)
*	call Math()
*	if (FRL_TestError1('DIV_BY_ZERO')) then
*	    write(*,*) 'You tried to divide by zero!'
*	end if
*$ Error_handling:
*	No error conditions are raised.
*$ Limitations:
*	Error ids are limited to 31 characters.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
********************************************************************************
* Note: GRL_TestError1() defined here is the intermediary routine between
* FRL_TestError1() and RL_TestError1(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fringlib.for for the
* rest of the code.
*
* logical*4 function GRL_TestError1(error_id)
* byte		error_id(*)
*******************************************************************************/

RL_INT4 FORTRAN_NAME(grl_testerror1) (error_id)
RL_CHAR *error_id;
{
    return (RL_TestError1(error_id) ? FTRUE : FFALSE);
}

/*<A NAME="FRL_ClearError1"></A>
********************************************************************************
*$ Component_name:
*	FRL_ClearError1 (rlerrors.c, fringlib.for)
*$ Abstract:
*	This routine removes a particular error from the error stack, if found.
*	It returns .TRUE. if the error was found and .FALSE. otherwise.
*$ Keywords:
*	RINGLIB, ERRORS
*	FORTRAN, PUBLIC, SUBROUTINE
*$ Declarations:
*	logical*4 function FRL_ClearError1(error_id)
*	character*(*)	error_id
*$ Inputs:
*	error_id	string containing the error id to clear.
*$ Outputs:
*	none
*$ Returns:
*	.TRUE. if the error was found; .FALSE. otherwise.
*$ Detailed_description:
*	This routine removes a particular error from the error stack, if found.
*	It returns .TRUE. if the error was found and .FALSE. otherwise.
*$ External_references:
*	none
*$ Side_effects:
*	An error may be removed from the stack and freed.
*$ Examples:
*	Suppose you call a routine Math() that might raise error "DIV_BY_ZERO".
*	You wish to print your own error message if this occurs, and then
*	remove this error from the stack.
*
*	call FRL_SetErrorType('DIV_BY_ZERO', RL_RECORD)
*	call Math()
*	if (FRL_ClearError1('DIV_BY_ZERO')) then
*	    write(*,*) 'You tried to divide by zero!'
*	end if
*$ Error_handling:
*	No error conditions are raised.
*$ Limitations:
*	Error ids are limited to 31 characters.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
********************************************************************************
* Note: GRL_ClearError1() defined here is the intermediary routine between
* FRL_ClearError1() and RL_ClearError1(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fringlib.for for the
* rest of the code.
*
* logical*4 function GRL_ClearError1(error_id)
* byte		error_id(*)
*******************************************************************************/

RL_INT4 FORTRAN_NAME(grl_clearerror1) (error_id)
RL_CHAR *error_id;
{
    return (RL_ClearError1(error_id) ? FTRUE : FFALSE);
}

/*<A NAME="FRL_SetErrorType"></A>
********************************************************************************
*$ Component_name:
*	FRL_SetErrorType (rlerrors.c, fringlib.for)
*$ Abstract:
*	This routine sets the type for a particular error.  It also returns the
*	current value of the error type.
*$ Keywords:
*	RINGLIB, ERRORS
*	FORTRAN, PUBLIC, SUBROUTINE
*$ Declarations:
*	integer*4 function FRL_SetErrorType(error_id, error_type)
*	character*(*)	error_id
*	integer*4	error_type
*$ Inputs:
*	error_id	string identifying the error id.
*	error_type	value indicating how the error is to be handled.
*$ Outputs:
*	none
*$ Returns:
*	the previous error_type for this error_id.
*$ Detailed_description:
*	This routine sets the type for a particular error.  It also returns the
*	current value of the error type.
*
*	The error type controls what happens when RL_RaiseError() is called.
*	Error type values are:
*		RL_IGNORE = -1: don't print; don't record in stack.
*		RL_INFORM =  1:       print; don't record in stack.
*		RL_RECORD = -2: don't print;       record in stack.
*		RL_SIGNAL =  2:       print;       record in stack.
*		RL_ABORT  =  3:       print; abort program
*	RL_ABORT is the default behavior.  These constants are defined in the
*	include file ringlib.h.
*$ External_references:
*	none
*$ Side_effects:
*	Depending on the error type, an error may be removed from the stack and
*	freed.
*$ Examples:
*	Suppose you wish to change temporarily the method a program uses to
*	handle DIV_BY_ZERO errors to RL_RECORD, and then to restore it to the
*	previous method when you're done.
*
*	integer*4	old_type
*
*	old_type = FRL_SetErrorType('DIV_BY_ZERO', RL_RECORD)
*	... (DIV_BY_ZERO is now set to record)
*
*	call FRL_SetErrorType('DIV_BY_ZERO', old_type)
*	... (previous type of DIV_BY_ZERO is restored)
*$ Error_handling:
*	No error conditions are raised.
*$ Limitations:
*	Error ids are limited to 31 characters.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
********************************************************************************
* Note: GRL_SetErrorType() defined here is the intermediary routine between
* FRL_SetErrorType() and RL_SetErrorType(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fringlib.for for the
* rest of the code.
*
* integer*4 function GRL_SetErrorType(error_id, error_type)
* byte		error_id(*)
* integer*4	error_type
*******************************************************************************/

RL_INT4 FORTRAN_NAME(grl_seterrortype) (error_id, error_type)
RL_CHAR *error_id;
RL_INT4 *error_type;
{
	return RL_SetErrorType(error_id, *error_type);
}

/*<A NAME="FRL_GetErrorType"></A>
********************************************************************************
*$ Component_name:
*	FRL_GetErrorType (rlerrors.c, fringlib.for)
*$ Abstract:
*	This routine returns the error type for a particular error id.
*$ Keywords:
*	RINGLIB, ERRORS
*	FORTRAN, PUBLIC, SUBROUTINE
*$ Declarations:
	integer*4 function FRL_GetErrorType(error_id)
	character*(*)	error_id
*$ Inputs:
*	error_id	string identifying the error id.
*$ Outputs:
*	none
*$ Returns:
*	the error type for the given error id.
*$ Detailed_description:
*	This routine returns the error type for a particular error id.
*
*	The error type controls what happens when RL_RaiseError() is called.
*	Error type values are:
*		RL_IGNORE = -1: don't print; don't record in stack.
*		RL_INFORM =  1:       print; don't record in stack.
*		RL_RECORD = -2: don't print;       record in stack.
*		RL_SIGNAL =  2:       print;       record in stack.
*		RL_ABORT  =  3:       print; abort program
*	RL_ABORT is the default behavior.  These constants are defined in the
*	include file ringlib.h.
*$ External_references:
*	none
*$ Side_effects:
*	none
*$ Examples:
*	Suppose you call a routine Math() that might raise error "DIV_BY_ZERO".
*	Without changing the type of this error permanently, you wish to make
*	sure a message is printed.
*
*	integer*4	old_type
*
*	old_type = FRL_GetErrorType('DIV_BY_ZERO')
*	if (old_type .lt. 0) call FRL_SetErrorType('DIV_BY_ZERO', -old_type)
*	call Math()
*	call FRL_SetErrorType('DIV_BY_ZERO', old_type)
*$ Error_handling:
*	No error conditions are raised.
*$ Limitations:
*	Error ids are limited to 31 characters.
*$ Author_and_institution:
*	Mark R. Showalter
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: March 1998
*$ Change_history:
*	none
********************************************************************************
* Note: GRL_GetErrorType() defined here is the intermediary routine between
* FRL_GetErrorType() and RL_GetErrorType(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fringlib.for for the
* rest of the code.
*
* integer*4 function GRL_GetErrorType(error_id)
* byte		error_id(*)
*******************************************************************************/

RL_INT4 FORTRAN_NAME(grl_geterrortype) (error_id)
RL_CHAR *error_id;
{
	return RL_GetErrorType(error_id);
}

/*******************************************************************************
</PRE></BODY></HTML>*/
