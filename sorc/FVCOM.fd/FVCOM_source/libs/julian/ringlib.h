/*<HTML><HEAD><TITLE> ringlib.h </TITLE></HEAD><BODY><PRE>
********************************************************************************
* ringlib.h -- Include file for standard RingLib features
*
* Mark Showalter and Neil Heather, PDS Rings Node, May 1998
*******************************************************************************/

#ifndef RINGLIB_INCLUDED
#define RINGLIB_INCLUDED

/*******************************************************************************
* Check some properties of this compiler
*******************************************************************************/

/*
 * The only really really braindead compiler out there is the default Sun
 * compiler.  It is recognized by "sun" being defined and by "__STDC__" being
 * undefined.
 */

#ifdef __STDC__
#undef sun
#endif

#ifndef sun
#define XRL_HAS_PROTOTYPES 1
#define XRL_HAS_VOIDSTAR   1
#define XRL_HAS_SIZE_T     1
#endif

/*
 * We need to know how to declare a 4-byte integer.  The int type is usually
 * four bytes but is sometimes two bytes.  The long type is usually four bytes
 * but is sometimes eight bytes.
 *
 * So far, int is four bytes on all systems where RingLib has been tested.
 * However, this may have to be updated in the future.
 */

#define XRL_INT_IS_FOURBYTES 1

/*******************************************************************************
* DATA TYPES
*
* This section may have conditional code for different compilers
*******************************************************************************/

#ifdef XRL_INT_IS_FOURBYTES
typedef int		RL_INT4;		/* 4-byte integer */
#else
typedef long		RL_INT4;		/* 4-byte integer */
#endif

typedef int		RL_BOOL;		/* boolean */
typedef float		RL_FLT4;		/* single precision */
typedef double		RL_FLT8;		/* double precision */
typedef char		RL_CHAR;		/* 1-byte character */

/* The ANSI C type "size_t" is not supported by the Sun C compiler. */
#include <stddef.h>	/* should define size_t */

#ifdef XRL_HAS_SIZE_T
typedef size_t  	RL_SIZE;		/* size type */
#else
typedef unsigned	RL_SIZE;		/* size type */
#endif

/* The RL_VOID data type is used exclusively in the context of "void *".
 * The ANSI C data type "void *" is not supported by the Sun C compiler.
 * Note that this has to be done as a macro definition, because declaring a
 * typedef to be void is not allowed (since void is not really a data type!)
 */

#ifdef XRL_HAS_VOIDSTAR
#define RL_VOID		void
#else
#define RL_VOID		char
#endif

/*******************************************************************************
* MACRO DEFINITIONS
*******************************************************************************/

/* Required constants */

#ifndef TRUE
#define TRUE  ((RL_BOOL) 1)
#define FALSE ((RL_BOOL) 0)
#endif

#ifndef NULL
#define NULL ((RL_VOID *) 0)
#endif

/* The macro RL_PROTO(), is used in function prototype declarations.
 *
 * Using this macro, a function prototype can be declared as follows:
 *	type function_name RL_PROTO((arg1, arg2, ...))
 * Note the use of double parentheses.  The RL_PROTO macro will eliminate the
 * arguments for those C compilers (such as Sun's) on which prototypes are not
 * supported.
 */

#ifdef XRL_HAS_PROTOTYPES
#define RL_PROTO(arglist)	arglist
#else
#define RL_PROTO(arglist)	()
#endif

/* Symbolic names for RingLib error types */

#define RL_IGNORE	-1	/* don't print, don't record */
#define RL_INFORM	 1	/*       print, don't record */
#define RL_RECORD	-2	/* don't print,       record */
#define RL_SIGNAL	 2	/*       print,       record */
#define RL_ABORT  	 3	/*       print,       abort  */

/* On some systems, HUGE is defined instead of HUGE_VAL inside math.h */

#ifndef HUGE_VAL
#ifdef  HUGE
#define HUGE_VAL HUGE
#endif
#endif

/*******************************************************************************
* FUNCTION PROTOTYPES
*******************************************************************************/

/*<A NAME="rlerrors"></A>
 * rl_error.c -- RingLib error handling routines */

void	 RL_RaiseError   RL_PROTO((RL_CHAR *error_id, RL_CHAR *message));
void	 RL_PipeErrors   RL_PROTO((RL_CHAR *filename));

RL_CHAR	*RL_TestError    RL_PROTO((void));
RL_CHAR *RL_ClearError   RL_PROTO((void));

RL_BOOL  RL_TestError1   RL_PROTO((RL_CHAR *error_id));
RL_BOOL  RL_ClearError1  RL_PROTO((RL_CHAR *error_id));

RL_INT4  RL_SetErrorType RL_PROTO((RL_CHAR *error_id, RL_INT4 error_type));
RL_INT4  RL_GetErrorType RL_PROTO((RL_CHAR *error_id));

/*<A NAME="rlmemory"></A>
 * rlmemory.c -- Ringlib memory management routines */

RL_VOID *XRL_Malloc      RL_PROTO((RL_SIZE size));
RL_VOID *XRL_Realloc     RL_PROTO((RL_VOID *pointer, RL_SIZE size));
RL_VOID *XRL_MustAlloc   RL_PROTO((RL_SIZE size));
void     XRL_Free        RL_PROTO((RL_VOID *pointer));

#endif

/*******************************************************************************
</PRE></BODY></HTML>*/
