/*<HTML><HEAD><TITLE> fortran.h </TITLE></HEAD><BODY><PRE>
********************************************************************************
* fortran.h -- Include file for all FORTRAN interface routines.
*
* Mark R. Showalter, PDS Rings Node, January 1997
* Updated August 2002 for Macintosh OSX compatibility
*******************************************************************************/

#ifndef FORTRAN_INCLUDED
#define FORTRAN_INCLUDED

#include "ringlib.h"

/*******************************************************************************
* The macro FORTRAN_NAME converts a C function name to the form that is required
* when it is called by a FORTRAN program.  On many UNIX systems, subprograms
* called from FORTRAN have an implied underscore character at the ends of their
* names.  This macro takes care of this operating system quirk.
*******************************************************************************/

#ifdef VMS
#define FORTRAN_NAME(name)	name

#else

#ifdef __APPLE__
#define FORTRAN_NAME(name)	name

#else

#ifdef __STDC__
#define FORTRAN_NAME(name)	name##_

#else
#define FORTRAN_NAME(name)	name/**/_

#endif
#endif
#endif

/*******************************************************************************
* Define the FORTRAN logical constants .TRUE. and .FALSE.
*******************************************************************************/

#ifdef __APPLE__

#define FTRUE  ((RL_INT4) 1)
#define FFALSE ((RL_INT4) 0)

#else

#define FTRUE  ((RL_INT4) -1)
#define FFALSE ((RL_INT4)  0)

#endif

/*******************************************************************************
* FUNCTION PROTOTYPES
*******************************************************************************/

void     FORT_Init        RL_PROTO((void));
RL_INT4  FORT_AddPointer  RL_PROTO((RL_VOID *pointer));
RL_VOID *FORT_GetPointer  RL_PROTO((RL_INT4 index));
RL_VOID *FORT_FreePointer RL_PROTO((RL_INT4 index));

#endif

/*******************************************************************************
</PRE></BODY></HTML>*/
