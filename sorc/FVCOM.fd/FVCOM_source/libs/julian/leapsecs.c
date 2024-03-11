/*<HTML><HEAD><TITLE> leapsecs.c </TITLE></HEAD><BODY><PRE>
********************************************************************************
* leapsecs.c
*
* This set of functions makes it possible to maintain a list of the number of
* leap seconds.  This is the value of TAI (Atomic time) - UTC (Universal time).
*
* RL_INT4 Jul_LeapSecs(dutc)
*		returns the number of leap seconds prior to the beginning of the
*		given year and month.
* RL_BOOL Jul_IsLeapDay(dutc)
*		returns TRUE if the given day has a leap second.
* RL_INT4 Jul_DaySecs(dutc)
*		returns the number of seconds in the given day.
* RL_BOOL Jul_InitLeaps(leapfile)
*		initializes the table of leap seconds based on the contents of
*		a specified file.
*
* Mark R. Showalter, PDS Rings Node, June 1997
* Revised 6/98 by MRS to conform to RingLib naming standards.
* Revised 12/98 by MRS to include leap second of December 31, 1998.
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "julian.h"
#include "fortran.h"

/* The table below contains the default list of leap seconds.  Users may add
 * additional leap seconds and re-compile if they wish to do so.  Simply
 * increment the value for LEAP_DEFAULT_COUNT and also add a new entry to the
 * leap_defaults[] array of the form {year, month, #secs}.  The leap second will
 * apply just before the beginning of the specified month.  Note that month
 * values are limited to 1 (January) and 7 (July).
 */

#define LEAP_DEFAULT_COUNT	23

static struct {RL_INT4 year; RL_INT4 month; RL_INT4 secs;}
	leap_defaults[LEAP_DEFAULT_COUNT] = {
		{1972, 1, 10},
		{1972, 7, 11},
		{1973, 1, 12},
		{1974, 1, 13},
		{1975, 1, 14},
		{1976, 1, 15},
		{1977, 1, 16},
		{1978, 1, 17},
		{1979, 1, 18},
		{1980, 1, 19},
		{1981, 7, 20},
		{1982, 7, 21},
		{1983, 7, 22},
		{1985, 7, 23},
		{1988, 1, 24},
		{1990, 1, 25},
		{1991, 1, 26},
		{1992, 7, 27},
		{1993, 7, 28},
		{1994, 7, 29},
		{1996, 1, 30},
		{1997, 7, 31},
		{1999, 1, 32}
	};

/* Users should not modify these definitions */

static RL_INT4	*leap_table = NULL;
static RL_INT4	leap_size, leap_ymin, leap_ymax, leap_nmin, leap_nmax;

#define LEAPINDEX(y,m)	(2*((y)-leap_ymin) + ((m)-1)/6)	

/***********************************************************
* Prototypes of internal functions
***********************************************************/

static RL_INT4 ZJul_LeapSecsYM RL_PROTO((RL_INT4 year, RL_INT4 month));

/*<A NAME="Jul_LeapSecs"></A>
********************************************************************************
* EXPORTED USER ROUTINES
********************************************************************************
*$ Component_name:
*	Jul_LeapSecs (leapsecs.c)
*$ Abstract:
*	Returns the number of leap seconds elapsed before a given day.
*$ Keywords:
*	JULIAN, TIME, UNIVERSAL_TIME, ATOMIC_TIME
*	C, PUBLIC
*$ Declarations:
*	RL_INT4		Jul_LeapSecs(dutc)
*	RL_INT4		dutc;
*$ Inputs:
*	dutc		number of days relative to January 1, 2000.
*$ Outputs:
*	none
*$ Returns:
*	number of leap seconds before beginning of given day.
*$ Side_effects:
*	The internal table of leap seconds is initialized if necessary.
*$ Detailed_description:
*	This function returns the number of leap seconds elapsed before the
*	beginning of a given day.  This is the value for TAI (atomic time) -
*	UTC (Universal time).
*$ External_references:
*	Jul_YMDofDUTC(), Jul_InitLeaps()
*$ Examples:
*	none
*$ Error_handling:
*	none
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
*******************************************************************************/

RL_INT4	Jul_LeapSecs(dutc)
RL_INT4	dutc;
{
RL_INT4	year, month, day;

	Jul_YMDofDUTC(dutc, &year, &month, &day);
	return ZJul_LeapSecsYM(year, month);
}

/*******************************************************************************
* Internal function to calculate leap seconds from year and month
*******************************************************************************/

static RL_INT4 ZJul_LeapSecsYM(year, month)
RL_INT4	year, month;
{
RL_INT4	index, nleaps;

/* Initialize table if necessary */
	if (leap_table == NULL) Jul_InitLeaps(NULL);

/* Look up leap second count in table */
	index = LEAPINDEX(year, month);

	if      (index < 0)          nleaps = leap_nmin;
	else if (index >= leap_size) nleaps = leap_nmax;
	else                         nleaps = leap_table[index];

	return nleaps;
}

/*<A NAME="Jul_IsLeapDay"></A>
********************************************************************************
*$ Component_name:
*	Jul_IsLeapDay (leapsecs.c)
*$ Abstract:
*	Determines whether a specified day contains a leap second.
*$ Keywords:
*	JULIAN, TIME, UNIVERSAL_TIME, ATOMIC_TIME
*	C, PUBLIC
*$ Declarations:
*	RL_BOOL		Jul_IsLeapDay(dutc)
*	RL_INT4		dutc;
*$ Inputs:
*	dutc		number of days relative to January 1, 2000.
*$ Outputs:
*	none
*$ Returns:
*	TRUE if the specified day contains a leap second; FALSE otherwise.
*$ Side_effects:
*	The internal table of leap seconds is initialized if necessary.
*$ Detailed_description:
*	This function determines whether a specified day contains a leap second.
*	It returns TRUE if the day has a leap second; FALSE otherwise.
*$ External_references:
*	Jul_YMDofDUTC(), Jul_InitLeaps()
*$ Examples:
*	none
*$ Error_handling:
*	none
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
*******************************************************************************/

RL_BOOL	Jul_IsLeapDay(dutc)
RL_INT4	dutc;
{
RL_INT4	year, month, day;

/* Initialize table if necessary */
	if (leap_table == NULL) Jul_InitLeaps(NULL);

/* Convert to year, month and day */
	Jul_YMDofDUTC(dutc, &year, &month, &day);

/* Make sure it's the last day of June or December */
	if      (day <= 29) {return FALSE;}
	else if (day == 30) {if (month !=  6) return FALSE;}
	else if (day == 31) {if (month != 12) return FALSE;}
	else                {return FALSE;}	/* Invalid date! */

/* Look for a change in the leap second count */
	return (ZJul_LeapSecsYM(year,month+1) != ZJul_LeapSecsYM(year,month));
}

/*<A NAME="Jul_DaySecs"></A>
********************************************************************************
*$ Component_name:
*	Jul_DaySecs (leapsecs.c)
*$ Abstract:
*	Returns the number of seconds in a given day.
*$ Keywords:
*	JULIAN, TIME, UNIVERSAL_TIME, ATOMIC_TIME
*	C, PUBLIC
*$ Declarations:
*	RL_INT4		Jul_DaySecs(dutc)
*	RL_INT4		dutc;
*$ Inputs:
*	dutc		number of days relative to January 1, 2000.
*$ Outputs:
*	none
*$ Returns:
*	number of seconds in given day, 86400 if it does not contain a leap
*	second or 86401 if it does.
*$ Side_effects:
*	The internal table of leap seconds is initialized if necessary.
*$ Detailed_description:
*	This function returns the number of seconds on a given day.  This number
*	is 86400 on most days, but 86401 if the day contains a leap second.
*$ External_references:
*	Jul_IsLeapDay(), Jul_InitLeaps()
*$ Examples:
*	none
*$ Error_handling:
*	none
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
*******************************************************************************/

RL_INT4	Jul_DaySecs(dutc)
RL_INT4	dutc;
{
	return (Jul_IsLeapDay(dutc) ? 86401:86400);
}

/*<A NAME="Jul_InitLeaps"></A>
********************************************************************************
*$ Component_name:
*	Jul_InitLeaps (leapsecs.c)
*$ Abstract:
*       Initializes the internal table of leap seconds, optionally based on the
*       leap seconds listed in a file.
*$ Keywords:
*	JULIAN, TIME, UNIVERSAL_TIME, ATOMIC_TIME
*	C, PUBLIC
*$ Declarations:
*	RL_BOOL		Jul_InitLeaps(leapfile)
*	RL_CHAR		*leapfile;
*$ Inputs:
*	leapfile	pointer to the name of a file containing a supplementary
*			list of leap seconds.  If the file name is blank or the
*			pointer is NULL, no file is read.
*$ Outputs:
*	none
*$ Returns:
*	TRUE if the initialization was successful; FALSE if an error occurred.
*$ Side_effects:
*	The internal table of leap seconds is initialized.
*$ Detailed_description:
*	This function initializes the internal table of leap seconds, optionally
*	based on the contents of a file.  If it is called, it must be called
*	before any call to Jul_LeapSecs(), Jul_IsLeap() or Jul_DaySecs().  At
*	minimum, an internal list of leap seconds (currently up to date through
*	January 1996) is always used.
*
*	The input file must contain four ASCII integers per record, separated by
*	blanks.  The order of values is year, month, day and total elapsed leap
*	seconds at the beginning of that date.  Lines of the file beginning with
*	an exclamation point ("!") are ignored.
*
*	Alternatively, users may add new leap seconds by simply augmenting the
*	list found in file "jul_leapsecs.c" and then re-compiling.
*$ External_references:
*	none
*$ Examples:
*	How to read leaps seconds from an external file...
*
*	RL_BOOL	status;
*
*	status = Jul_InitLeaps("leapseconds.dat");
*	if (!status) exit(1);
*$ Error_handling:
*	If an error occurs, the function prints an error message to "stderr" and
*	returns a value of FALSE.
*$ Limitations:
*	This routine only supports leap seconds that occur at the end of a
*	December or a June.  Leap second file records are limited to 131
*	characters.
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
*******************************************************************************/

#define	RECORD_LEN	131

RL_BOOL	Jul_InitLeaps(leapfile)
RL_CHAR	*leapfile;
{
FILE	*file;
RL_INT4	i, ymin, ymax, count, year, month, day, nsecs;
RL_CHAR	record[RECORD_LEN+1];

/* Make sure this routine was not already called */
	if (leap_table != NULL) goto EXTRA_CALL;

/* Initialize static variables */
	leap_ymin = leap_defaults[0].year;
	leap_nmin = leap_defaults[0].secs - 1;
	leap_ymax = leap_defaults[LEAP_DEFAULT_COUNT-1].year;
	leap_nmax = leap_defaults[LEAP_DEFAULT_COUNT-1].secs;
	file = NULL;

/***********************************************************
* Open and read new leap seconds file if necessary
***********************************************************/

	if (leapfile != NULL && *leapfile != '\0') {

/* Open file */
		file = fopen(leapfile, "r");
		if (file == NULL) goto OPEN_FAILURE;

/* Get year range and leap second count */
		while (NULL != fgets(record, RECORD_LEN+1, file)) {
			if (record[0] == '!') continue;

			count = sscanf(record, "%d %d %d %d",
					&year, &month, &day, &nsecs);
			if (count == 0) continue;
			if (count < 4) goto SYNTAX_ERROR;

			if (year < leap_ymin) goto UNSUPPORTED;
			if (month != 1 && month != 7) goto UNSUPPORTED;
			if (day != 1) goto UNSUPPORTED;
			if (nsecs > 255) goto UNSUPPORTED;

			leap_ymax = year;
			leap_nmax = nsecs;
		}
	}

/***********************************************************
* Allocate and initialize the leap seconds table
***********************************************************/

	leap_size = 2 * (leap_ymax - leap_ymin + 1);
	leap_table = (RL_INT4 *) malloc(leap_size * sizeof(RL_INT4));
	if (leap_table == NULL) goto MALLOC_ERROR;

	for (i = 0; i < leap_size; i++) {
		leap_table[i] = leap_nmin;
	}

/************************************************************
* Enter default leap seconds into the table
************************************************************/

	for (i = 0; i < LEAP_DEFAULT_COUNT; i++) {
		leap_table[ LEAPINDEX(leap_defaults[i].year,
		                      leap_defaults[i].month) ]
						= leap_defaults[i].secs;
	}

/***********************************************************
* Enter new leap seconds into the table, if necessary
***********************************************************/

	if (file != NULL) {
		rewind(file);
		while (NULL != fgets(record, RECORD_LEN+1, file)) {
			count = sscanf(record, "%d %d %d %d",
				&year, &month, &day, &nsecs);
			if (count == 0) continue;

			leap_table[LEAPINDEX(year,month)] = nsecs;
		}

		fclose(file);
	}

/***********************************************************
* Fill gaps in the table
***********************************************************/

	nsecs = leap_nmin;
	for (i = 0; i < leap_size; i++) {
		if (leap_table[i] == leap_nmin) {
			leap_table[i] = nsecs;
		}
		else {
			if (leap_table[i] != nsecs+1) goto STEP_ERROR;
			nsecs = leap_table[i];
		}
	}

	return TRUE;

EXTRA_CALL:
	fprintf(stderr, "Leap seconds table was already initialized\n");
	return FALSE;

OPEN_FAILURE:
	fprintf(stderr, "Open failure on file \"%s\"\n", leapfile);
	return FALSE;

UNSUPPORTED:
	fprintf(stderr, "Unsupported leap seconds date: %s\n", record);
	return FALSE;

SYNTAX_ERROR:
	fprintf(stderr, "Syntax error in leap seconds file: %s\n", record);
	return FALSE;

MALLOC_ERROR:
	fprintf(stderr, "Unable to allocate memory for leap seconds table\n");
	return FALSE;

STEP_ERROR:
	fprintf(stderr, "Illegal leap second step from %d to %d\n",
							nsecs, leap_table[i]);

	return FALSE;
}

/*<A NAME="FJul_LeapSecs"></A>
********************************************************************************
* FORTRAN INTERFACE ROUTINES
********************************************************************************
*$ Component_name:
*	FJul_LeapSecs (leapsecs.c)
*$ Abstract:
*	Returns the number of leap seconds elapsed before a given day.
*$ Keywords:
*	JULIAN, TIME, UNIVERSAL_TIME, ATOMIC_TIME
*	FORTRAN, PUBLIC
*$ Declarations:
*	integer*4 function FJul_LeapSecs(dutc)
*	integer*4	dutc
*$ Inputs:
*	dutc		number of days relative to January 1, 2000.
*$ Outputs:
*	none
*$ Returns:
*	number of leap seconds before beginning of given day.
*$ Side_effects:
*	The internal table of leap seconds is initialized if necessary.
*$ Detailed_description:
*	This function returns the number of leap seconds elapsed before the
*	beginning of a given day.  This is the value for TAI (atomic time) -
*	UTC (Universal time).
*$ External_references:
*	Jul_LeapSecs()
*$ Examples:
*	none
*$ Error_handling:
*	none
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
*******************************************************************************/

RL_INT4	FORTRAN_NAME(fjul_leapsecs) (dutc)
RL_INT4	*dutc;
{
	return Jul_LeapSecs(*dutc);
}

/*<A NAME="FJul_IsLeapDay"></A>
********************************************************************************
*$ Component_name:
*	FJul_IsLeapDay (leapsecs.c)
*$ Abstract:
*	Determines whether a specified day contains a leap second.
*$ Keywords:
*	JULIAN, TIME, UNIVERSAL_TIME, ATOMIC_TIME
*	FORTRAN, PUBLIC
*$ Declarations:
*	logical*4 function FJul_IsLeapDay(dutc)
*	integer*4	dutc
*$ Inputs:
*	dutc		number of days relative to January 1, 2000.
*$ Outputs:
*	none
*$ Returns:
*	.TRUE. if the specified day contains a leap second; .FALSE. otherwise.
*$ Side_effects:
*	The internal table of leap seconds is initialized if necessary.
*$ Detailed_description:
*	This function determines whether a specified day contains a leap second.
*	It returns .TRUE. if the day has a leap second; .FALSE. otherwise.
*$ External_references:
*	Jul_IsLeapDay()
*$ Examples:
*	none
*$ Error_handling:
*	none
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
*******************************************************************************/

RL_INT4	FORTRAN_NAME(fjul_isleapday) (dutc)
RL_INT4	*dutc;
{
	return (Jul_IsLeapDay(*dutc) ? FTRUE:FFALSE);
}

/*<A NAME="FJul_DaySecs"></A>
********************************************************************************
*$ Component_name:
*	FJul_DaySecs (leapsecs.c)
*$ Abstract:
*	Returns the number of seconds in a given day.
*$ Keywords:
*	JULIAN, TIME, UNIVERSAL_TIME, ATOMIC_TIME
*	FORTRAN, PUBLIC
*$ Declarations:
*	integer*4 function FJul_DaySecs(dutc)
*	integer*4	dutc
*$ Inputs:
*	dutc		number of days relative to January 1, 2000.
*$ Outputs:
*	none
*$ Returns:
*	number of seconds in given day, 86400 if it does not contain a leap
*	second or 86401 if it does.
*$ Side_effects:
*	The internal table of leap seconds is initialized if necessary.
*$ Detailed_description:
*	This function returns the number of seconds on a given day.  This number
*	is 86400 on most days, but 86401 if the day contains a leap second.
*$ External_references:
*	Jul_DaySecs()
*$ Examples:
*	none
*$ Error_handling:
*	none
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
*******************************************************************************/

RL_INT4	FORTRAN_NAME(fjul_daysecs) (dutc)
RL_INT4	*dutc;
{
	return Jul_DaySecs(*dutc);
}

/*<A NAME="FJul_InitLeaps"></A>
********************************************************************************
*$ Component_name:
*	FJul_InitLeaps (leapsecs.c)
*$ Abstract:
*	Initializes the internal table of leap seconds, optionally based on the
*	leap seconds listed in a file.
*$ Keywords:
*	JULIAN, TIME, LEAP_SECONDS
*	C, PUBLIC
*$ Declarations:
*	logical*4 function FJul_InitLeaps(leapfile)
*	character*(*)	leapfile
*$ Inputs:
*	leapfile	name of a file containing a supplementary list of leap
*			seconds.  If the file name is blank, no file is read.
*$ Outputs:
*	none
*$ Returns:
*	.TRUE. if the initialization was successful; .FALSE. if an error
*	occurred.
*$ Side_effects:
*	The internal table of leap seconds is initialized.
*$ Detailed_description:
*	This function initializes the internal table of leap seconds, optionally
*	based on the contents of a file.  If it is called, it must be called
*	before any call to FJul_LeapSecs(), FJul_IsLeap() or FJul_DaySecs().  At
*	minimum, an internal list of leap seconds (currently up to date through
*	January 1996) is always used.
*
*	The input file must contain four ASCII integers per record, separated by
*	blanks.  The order of values is year, month, day and total elapsed leap
*	seconds at the beginning of that date.
*
*	Alternatively, users may augment the leap seconds list by simply adding
*	them to the list found in file "jul_leapsecs.c" and then re-compiling.
*$ External_references:
*	Jul_InitLeaps(), RL_Cstring()
*$ Examples:
*	How to read leaps seconds from an external file...
*
*	logical*4	status
*
*	status = FJul_InitLeaps('leapseconds.dat')
*	if (.not. status) stop
*$ Error_handling:
*	If an error occurs, the function prints an error message and returns a
*	value of .FALSE.
*$ Limitations:
*	This routine only supports leap seconds that occur at the end of a
*	December or a June.  Leap second file records are limited to 131
*	characters.
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
********************************************************************************
* Note: GJul_InitLeaps() defined here is the intermediary routine between
* FJul_InitLeaps() and Jul_InitLeaps(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fjulian.for for the
* rest of the code.
*
* subroutine GJul_InitLeaps(leapfile)
* byte		leapfile
*******************************************************************************/

RL_INT4	FORTRAN_NAME(gjul_initleaps) (leapfile)
RL_CHAR	*leapfile;
{
	FORT_Init();

	return (Jul_InitLeaps(leapfile) ? FTRUE:FFALSE);
}

/*******************************************************************************
</PRE></BODY></HTML>*/
