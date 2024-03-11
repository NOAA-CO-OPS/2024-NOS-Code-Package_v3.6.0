/*<HTML><HEAD><TITLE> parse.c </TITLE></HEAD><BODY><PRE>
********************************************************************************
* parse.c
*
* This set of routines parses date and time strings in a variety of formats.
*
* RL_BOOL Jul_ParseDT(RL_CHAR *string, RL_INT4 *dutc, RL_FLT8 *secs)
*		converts a date/time string to seconds relative to J2000 TAI.
* RL_BOOL Jul_ParseDate(RL_CHAR *string, RL_INT4 *dutc)
*		converts a date string to day relative to J2000.
* RL_BOOL Jul_ParseTime(RL_CHAR *string, RL_BOOL isleap, RL_FLT8 *secs)
*		converts a time string to seconds into the given day.
*
* Mark Showalter, PDS Rings Node, December 1995
* Revised 11/96 by MRS with minor updates.
* Revised 6/98 by MRS to conform to RingLib naming standards.
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "julian.h"
#include "fortran.h"

/***********************************************************
* Data structures
***********************************************************/

#define MAX_TOKENS	20
#define TOKEN_LEN	79

typedef struct Token_struct {
	RL_CHAR		string[TOKEN_LEN + 1];
	RL_CHAR		delim;
	RL_BOOL		is_alpha;
	RL_INT4		number;
} TOKEN;

/***********************************************************
* Prototypes of internal functions
***********************************************************/

/* To interpret date/time formats */

static RL_BOOL  ZJul_DTTokens   RL_PROTO((TOKEN *tokens, RL_INT4 ntokens,
                                          RL_CHAR *pref,
                                          RL_INT4 *dutc, RL_FLT8 *secs));

/* To interpret date formats */

static RL_BOOL  ZJul_DateTokens RL_PROTO((TOKEN *tokens, RL_INT4 ntokens,
                                          RL_CHAR *pref, RL_INT4 *dutc));
static RL_BOOL  ZJul_TestYMD    RL_PROTO((TOKEN *ytoken, TOKEN *mtoken,
                                          TOKEN *dtoken, RL_INT4 *year,
                                          RL_INT4 *month, RL_INT4 *day));
static RL_BOOL  ZJul_TestYear   RL_PROTO((TOKEN *token, RL_INT4 *year));
static RL_BOOL  ZJul_TestMonth  RL_PROTO((TOKEN *token, RL_INT4 *month));
static RL_BOOL  ZJul_TestDay    RL_PROTO((TOKEN *token, RL_INT4 year,
                                          RL_INT4 month, RL_INT4 *day));
static RL_BOOL  ZJul_TestDOY    RL_PROTO((TOKEN *token, RL_INT4 year,
                                          RL_INT4 *doy));

/* To interpret time formats */

static RL_BOOL  ZJul_TimeTokens RL_PROTO((TOKEN *tokens, RL_INT4 ntokens,
                                          RL_BOOL isleap, RL_FLT8 *secs));
static RL_BOOL  ZJul_TestHMS    RL_PROTO((TOKEN *token, RL_INT4 minval,
                                          RL_INT4 maxval, RL_INT4 *value));

/* To interpret Julian date formats */

static RL_BOOL  ZJul_JDTokens   RL_PROTO((TOKEN *tokens, RL_INT4 ntokens,
                                          RL_FLT8 *tai));

/* To manipulate tokens */

static RL_INT4  ZJul_FillTokens RL_PROTO((RL_CHAR *string, TOKEN *tokens,
                                          RL_INT4 max_tokens));
static RL_CHAR *ZJul_NextToken  RL_PROTO((RL_CHAR *string, TOKEN *token));
static RL_BOOL  ZJul_TestDelim  RL_PROTO((RL_INT4 delim, RL_CHAR *choices));
static RL_BOOL  ZJul_FracToken  RL_PROTO((TOKEN *token, RL_FLT8 *frac));

/*<A NAME="Jul_ParseDT"></A>
********************************************************************************
* EXPORTED USER ROUTINES
********************************************************************************
*$ Component_name:
*	Jul_ParseDT
*$ Abstract:
*	Interprets a character string as a date and time.
*$ Keywords:
*	JULIAN, TIME, PARSING
*	C, PUBLIC
*$ Declarations:
*	RL_BOOL		Jul_ParseDT(string, pref, dutc, secs)
*	RL_CHAR		*string, *pref;
*	RL_INT4		*dutc;
*	RL_FLT8		*secs;
*$ Inputs:
*	*string		the character string to parse.
*	*pref		optional three-character string giving the preferred
*			order for year, month and day values (see
*			Jul_ParseDate).
*$ Outputs:
*	*dutc		inferred days relative to January 1, 2000.
*	*secs		inferred seconds into day.
*$ Returns:
*	TRUE if a valid string interpretation was found; FALSE otherwise.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function interprets a character string as a date and time.
*
*	A string beginning with "JD" is interpreted as a Julian date; a string
*	beginning with "MJD" is interpreted as a Modified Julian Date.
*
*	Otherwise, the string is first separated into a sequence of tokens
*	separated by delimiters.  A token consists of any sequence of letters
*	or digits (but not a mixture of both).  A delimiter is any punctuation
*	or a single letter separating numeric tokens.  Blanks are not
*	significant except as delimiters when nothing else is present.
*
*	The string is parsed by testing each possible division between date and
*	time tokens until it finds one that produces both a valid date
*	according to Jul_ParseDate() and a valid time according to
*	Jul_ParseTime().  It gives preference for the longest possible date
*	token sequence; it also gives preference for dates before times.
*
*	If no valid date/time interpretation is found, it returns FALSE.
*$ External_references:
*	Jul_ParseDate(), Jul_ParseTime()
*$ Examples:
*	none
*$ Error_handling:
*	It returns FALSE if no valid interpretation is found; no error message
*	is printed.
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: December 1995
*$ Change_history:
*	none
*******************************************************************************/

RL_BOOL	Jul_ParseDT(string, pref, dutc, secs)
RL_CHAR	*string, *pref;
RL_INT4	*dutc;
RL_FLT8	*secs;
{
TOKEN	tokens[MAX_TOKENS];
RL_INT4	ntokens;
RL_BOOL	status;
RL_FLT8	tai;

/* Interpret string as a sequence of tokens */
	ntokens = ZJul_FillTokens(string, tokens, MAX_TOKENS);
	if (ntokens > MAX_TOKENS) return FALSE;

/* Check JD/MJD format */
	status = ZJul_JDTokens(tokens, ntokens, &tai);
	if (status) {
		*dutc = Jul_DUTCofTAI(tai, secs);
		return TRUE;
	}

/* Check final delimiter */
	status = ZJul_TestDelim(tokens[ntokens-1].delim, " THMSZ.");
	if (!status) return FALSE;

/* Parse date/time string */
	status = ZJul_DTTokens(tokens, ntokens, pref, dutc, secs);
	if (!status) return FALSE;

	return TRUE;
}

/*<A NAME="Jul_ParseDate"></A>
********************************************************************************
*$ Component_name:
*	Jul_ParseDate
*$ Abstract:
*	Interprets a character string as a date.
*$ Keywords:
*	JULIAN, TIME, PARSING
*	C, PUBLIC
*$ Declarations:
*	RL_BOOL		Jul_ParseDate(string, pref, dutc)
*	RL_CHAR		*string, *pref;
*	RL_INT4		*dutc;
*$ Inputs:
*	*string		the character string to parse.
*	*pref		optional three-character string giving the preferred
*			order for year, month and day values (see below).
*$ Outputs:
*	*dutc		inferred days relative to January 1, 2000.
*$ Returns:
*	TRUE if a valid string interpretation was found; FALSE otherwise.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function interprets a character string as a date.
*
*	The string is first separated into a sequence of tokens separated by
*	delimiters.  A token consists of any sequence of letters or digits (but
*	not a mixture of both).  A delimiter is any punctuation or a single
*	letter separating numeric tokens.  Blanks are not signficant except as
*	delimiters when nothing else is present.
*
*	A single token is interpreted as either a four-digit year or as an
*	eight-digit merged date of the form "yyyymmdd".  A pair of tokens is
*	interpreted as a year plus day-of-year in that order.
*
*	A set of three tokens is interpreted as a month, day and year in some
*	order.  Year values must be four digits or else 0-49 (which is
*	interpreted as 2000-2049) or 50-99 (which is interpreted as 1950-1999). 
*	Months are integers 1-12 or English names January-December, abbreviated
*	to three or more letters).  Days are integers between 1 and 31 (or less,
*	depending on the month).  The sequence of tokens is interpreted in the
*	preferred order first, then as month-day-year, day-month-year, and
*	year-month-day until a valid interpretation is found.
*
*	The pref argument, if used, enables the user to specify the preferred
*	ordering for year, month, and day.  It is a three-character string
*	containing a 'Y', 'M' and 'D', where the order of the three indicates
*	the preferred ordering for the year, month and day, respectively.
*
*	Valid delimiters are "/", "-", "." or blank.  For three tokens, the
*	delimiters between the tokens must be the same except for the special
*	case "month day, year" where a comma is allowed after the second token.
*	The final delimiter must be blank.
*
*	If no valid interpretation is found, the function returns FALSE.
*$ External_references:
*	many
*$ Examples:
*	none
*$ Error_handling:
*	It returns FALSE if no valid interpretation is found; no error message
*	is printed.
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: December 1995
*$ Change_history:
*	none
*******************************************************************************/

RL_BOOL	Jul_ParseDate(string, pref, dutc)
RL_CHAR	*string, *pref;
RL_INT4	*dutc;
{
TOKEN	tokens[MAX_TOKENS];
RL_INT4	ntokens;
RL_BOOL	status;

/* Convert to tokens */
	ntokens = ZJul_FillTokens(string, tokens, MAX_TOKENS);
	if (ntokens > MAX_TOKENS) return FALSE;

/* Check final delimiter */
	status = ZJul_TestDelim(tokens[ntokens-1].delim, " ");
	if (!status) return FALSE;

/* Parse date string */
	status = ZJul_DateTokens(tokens, ntokens, pref, dutc);
	return status;
}

/*<A NAME="Jul_ParseTime"></A>
********************************************************************************
*$ Component_name:
*	Jul_ParseTime
*$ Abstract:
*	Interprets a character string as a time.
*$ Keywords:
*	JULIAN, TIME, PARSING
*	C, PUBLIC
*$ Declarations:
*	RL_BOOL		Jul_ParseTime(string, isleap, secs)
*	RL_CHAR		*string;
*	RL_BOOL		isleap;
*	RL_FLT8		*secs;
*$ Inputs:
*	string		pointer to the character string to parse.
*	isleap		TRUE if the day has a leap second; FALSE otherwise.
*$ Outputs:
*	*dutc		inferred seconds into day.
*$ Returns:
*	TRUE if a valid string interpretation was found; FALSE otherwise.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function interprets a character string as a time.
*
*	The string is first separated into a sequence of tokens separated by
*	delimiters.  A token consists of any sequence of letters or digits (but
*	not a mixture of both).  A delimiter is any punctuation or a single
*	letter separating numeric tokens.  Blanks are not signficant except as
*	delimiters when nothing else is present.
*
*	Tokens are interpreted as hour, minute, second and millisecond in that
*	order although not all a required; the last token may have a fractional
*	part.  The string may end in "AM" or "PM", or else "Z" to be compatible
*	with PDS time formats.  Valid delimiters are colons or blanks.
*
*	Individual tokens may also end in "H", "M" or "S" to indicate hours,
*	minutes or seconds respectively.  This enables the user to express a
*	time in minutes (without hours) or seconds (without hours and minutes).
*
*	If no valid interpretation is found, the function returns FALSE.
*$ External_references:
*	many
*$ Examples:
*	none
*$ Error_handling:
*	It returns FALSE if no valid interpretation is found; no error message
*	is printed.
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: December 1995
*$ Change_history:
*	none
*******************************************************************************/

RL_BOOL	Jul_ParseTime(string, isleap, secs)
RL_CHAR	*string;
RL_BOOL	isleap;
RL_FLT8	*secs;
{
TOKEN	tokens[MAX_TOKENS];
RL_INT4	ntokens;
RL_BOOL	status;

/* Convert to tokens */
	ntokens = ZJul_FillTokens(string, tokens, MAX_TOKENS);
	if (ntokens > MAX_TOKENS) return FALSE;

/* Check final delimiter */
	status = ZJul_TestDelim(tokens[ntokens-1].delim, "HMSZ ");
	if (!status) return FALSE;

/* Parse time string */
	status = ZJul_TimeTokens(tokens, ntokens, isleap, secs);
	return status;
}

/*<A NAME="ZJul_DTTokens"></A>
********************************************************************************
* INTERNAL FUNCTION TO INTERPRET DATE/TIME FORMATS
********************************************************************************
* RL_INT4 ZJul_DTTokens(TOKEN *tokens, RL_INT4 ntokens, RL_CHAR *pref,
*			RL_INT4 *dutc, RL_FLT8 *secs)
*
* This internal function interprets a sequence of tokens as a date/time
* combination.  It returns TRUE if a valid date/time combination was found;
* FALSE otherise.
*
* Input:
*	tokens[0...ntokens-1]	array of tokens.
*	*pref			preferred order for year, month and day; see
*				Jul_ParseDate().
*
* Output:
*	*dutc			day relative to J2000.
*	*secs			seconds into day.
*
* Return:			TRUE if a valid sequence of tokens was found;
*				FALSE otherwise.
*******************************************************************************/

static RL_BOOL ZJul_DTTokens(tokens, ntokens, pref, dutc, secs)
TOKEN	*tokens;
RL_INT4	ntokens, *dutc;
RL_FLT8	*secs;
RL_CHAR*	pref;
{
RL_INT4	n, dtemp;
RL_FLT8	stemp;
RL_BOOL	dstat, sstat;

/* Consider date-time orderings */
	for (n=ntokens; n>=0; n--) {
		dstat = ZJul_DateTokens(tokens+0, n, pref, &dtemp);
		if (!dstat) continue;

		sstat = ZJul_TimeTokens(tokens+n, ntokens-n,
			Jul_IsLeapDay(dtemp), &stemp);
		if (sstat) goto DONE;
	}

/* Consider time-date orderings */
	for (n=0; n<=ntokens; n++) {
		dstat = ZJul_DateTokens(tokens+n, ntokens-n, pref, &dtemp);
		if (!dstat) continue;

		sstat = ZJul_TimeTokens(tokens+0, n,
			Jul_IsLeapDay(dtemp), &stemp);
		if (sstat) goto DONE;
	}

	return FALSE;

/* Valid interpretation found */

DONE:
	*dutc = dtemp;
	*secs = stemp;
	return TRUE;
}

/*<A NAME="ZJul_DateTokens"></A>
********************************************************************************
* INTERNAL FUNCTIONS TO INTERPRET DATES
********************************************************************************
* RL_INT4 ZJul_DateTokens(TOKEN *tokens, RL_INT4 ntokens, RL_CHAR *pref,
*			RL_INT4 *dutc)
*
* This internal function interprets a sequence of tokens as a date.  It returns
* a status value and, if status >= 0, the date inferred.
*
* Input:
*	tokens[0...ntokens-1]	array of tokens.
*	*pref			optional preferred year/month/day ordering.
*
* Output:
*	*dutc			day relative to J2000.
*
* Return:			TRUE if a valid interpretation was found;
*				FALSE otherwise.
*******************************************************************************/

static RL_BOOL ZJul_DateTokens(tokens, ntokens, pref, dutc)
TOKEN	*tokens;
RL_CHAR	*pref;
RL_INT4	ntokens, *dutc;
{
RL_INT4	year, month, day, y, m, d, i;
RL_BOOL	status, MDYonly;
RL_CHAR	string[20], test;

/* Test final delimiter */
	status = ZJul_TestDelim(tokens[ntokens-1].delim, " T/-:.");
	if (!status) return FALSE;

  switch (ntokens) {

/***********************************************************
* A single token must be either a year or "YYYYMMDD" format.
***********************************************************/

  case 1:

/* Test for a year value */
	status = ZJul_TestYear(tokens+0, &year);
	if (status) {
		*dutc = Jul_DUTCofYMD(year, 1, 1);
		return TRUE;
	}

/* Test for a "YYYYMMDD" numeric value */
	if (tokens[0].is_alpha) return FALSE;
	if (strlen(tokens[0].string) != 8) return FALSE;

	string[0]  = tokens[0].string[4];
	string[1]  = tokens[0].string[5];
	string[2]  = ' ';
	string[3]  = tokens[0].string[6];
	string[4]  = tokens[0].string[7];
	string[5]  = ',';
	string[6]  = tokens[0].string[0];
	string[7]  = tokens[0].string[1];
	string[8]  = tokens[0].string[2];
	string[9]  = tokens[0].string[3];
	string[10] = tokens[0].delim;
	string[11] = '\0';

	return Jul_ParseDate(string, "MDY", dutc);

/***********************************************************
* Two tokens must be year plus day-of-year
***********************************************************/

  case 2:
	status = ZJul_TestDelim(tokens[0].delim, "/-. ");
	if (!status) return FALSE;
			
	status = ZJul_TestYear(tokens+0, &year);
	if (!status) return FALSE;

	status = ZJul_TestDOY(tokens+1, year, &day);
	if (!status) return FALSE;

	*dutc = Jul_DUTCofYMD(year, 1, day);
	return TRUE;

/***********************************************************
* Three tokens must be year, month and day in some order
***********************************************************/

  case 3:

/* Test first delimiter */
	status = ZJul_TestDelim(tokens[0].delim, " /-.");
	if (!status) return FALSE;

/* Delimiters must match except when using "month day, year" format */
	MDYonly = (tokens[0].delim != tokens[1].delim);
	if (MDYonly) {
		if (tokens[0].delim != ' ') return FALSE;
		if (tokens[1].delim != ',') return FALSE;
	}

/* Test preferred order */
	if (!MDYonly && pref != NULL && *pref != '\0') {
	    y = -1;
	    m = -1;
	    d = -1;
	    for (i=0; i<3; i++) {
		test = toupper(pref[i]);
		if (test == 'Y') y = i;
		if (test == 'M') m = i;
		if (test == 'D') d = i;
	    }

	    if (y < 0 || m < 0 || d < 0) return FALSE;

	    status = ZJul_TestYMD(tokens+y, tokens+m, tokens+d,
				&year, &month, &day);
	    if (status) {
		*dutc = Jul_DUTCofYMD(year, month, day);
		return TRUE;
	    }
	}

/* Test MDY order */
	status = ZJul_TestYMD(tokens+2, tokens+0, tokens+1,
				&year, &month, &day);
	if (status) {
		*dutc = Jul_DUTCofYMD(year, month, day);
		return TRUE;
	}

	if (MDYonly) return FALSE;

/* Test DMY order */
	status = ZJul_TestYMD(tokens+2, tokens+1, tokens+0,
				&year, &month, &day);
	if (status) {
		*dutc = Jul_DUTCofYMD(year, month, day);
		return TRUE;
	}

/* Test YMD order */
	status = ZJul_TestYMD(tokens+0, tokens+1, tokens+2,
				&year, &month, &day);
	if (status) {
		*dutc = Jul_DUTCofYMD(year, month, day);
		return TRUE;
	}

	return FALSE;

/***********************************************************
* The number of tokens must be 1-3.
***********************************************************/

  default:
	return FALSE;
  }
}

/*<A NAME="ZJul_TestYMD"></A>
********************************************************************************
* RL_BOOL ZJul_TestYMD(TOKEN *ytoken, TOKEN *mtoken, TOKEN *dtoken,
*                   RL_INT4 *year, RL_INT4 *month, RL_INT4 *day)
*
* This internal function tests a set of three tokens to determine if they
* comprise a valid year, month and day.  It returns TRUE if they do, FALSE if
* they don't.
*
* Input:
*	ytoken		pointer to the year token.
*	mtoken		pointer to the month token.
*	dtoken		pointer to the day token.
*
* Output:
*	*year		inferred year 1900-2100.
*	*month		inferred month 1-12.
*	*day		inferred day 1-31.
*
* Return:		TRUE if all three tokens are valid; FALSE otherwise.
*******************************************************************************/

static RL_BOOL ZJul_TestYMD(ytoken, mtoken, dtoken, year, month, day)
TOKEN	*ytoken, *mtoken, *dtoken;
RL_INT4	*year, *month, *day;
{
RL_BOOL	status;

	status = ZJul_TestYear(ytoken, year);
	if (!status) return FALSE;

	status = ZJul_TestMonth(mtoken, month);
	if (!status) return FALSE;

	status = ZJul_TestDay(dtoken, *year, *month, day);
	if (!status) return FALSE;

	return TRUE;
}

/*<A NAME="ZJul_TestYear"></A>
********************************************************************************
* RL_BOOL ZJul_TestYear(TOKEN *token, RL_INT4 *year)
*
* This internal function tests a token to determine if it is a valid year.  It
* returns TRUE if the token is valid and FALSE otherwise.
*
* Input:
*	token		token structure.
*
* Output:
*	*month		year value if a valid token is found.
*
* Return:		TRUE if valid; FALSE otherwise.
*******************************************************************************/

static RL_BOOL ZJul_TestYear(token, year)
TOKEN	*token;
RL_INT4	*year;
{
RL_INT4	temp;

	if (token->is_alpha) return FALSE;
	temp = token->number;

/* Years between 1000 and 9999 are accepted exactly */
	if (temp >= 1000 && temp <= 9999) {
		*year = temp;
		return TRUE;
	}

/* Years 50-99 are treated as 1950-1999 */
	if (temp >= 50 && temp <= 99) {
		*year = temp + 1900;
		return TRUE;
	}

/* Years 0-49 are treated as 2000-2049 */
	if (temp >= 0 && temp <= 49) {
		*year = temp + 2000;
		return TRUE;
	}

	return FALSE;
}

/*<A NAME="ZJul_TestMonth"></A>
********************************************************************************
* RL_INT4 ZJul_TestMonth(TOKEN *token, RL_INT4 *month)
*
* This internal function tests a token to determine if it is a valid month.  It
* returns TRUE if it is valid, FALSE otherwise.
*
* Input:
*	token		token structure.
*
* Output:
*	*month		month value 1-12 if a valid token is found.
*
* Return:		TRUE if token was valid; FALSE otherwise.
*******************************************************************************/

static RL_BOOL ZJul_TestMonth(token, month)
TOKEN	*token;
RL_INT4	*month;
{
RL_INT4	temp, len;
static RL_CHAR *(month_names[12]) =
		{"JANUARY",   "FEBRUARY", "MARCH",    "APRIL",
		 "MAY",       "JUNE",     "JULY",     "AUGUST",
		 "SEPTEMBER", "OCTOBER",  "NOVEMBER", "DECEMBER"};

/* Test character strings */
	if (token->is_alpha) {
		len = strlen(token->string);
		if (len < 3) return FALSE;

		for (temp = 0; temp < 12; temp++) {
			if (strncmp(month_names[temp], token->string, len) == 0)
				break;
		}

		if (temp >= 12) return FALSE;

		*month = temp + 1;
		return TRUE;
	}

/* Test numeric values */
	if (token->number <= 0 || token->number > 12) return FALSE;

	*month = token->number;
	return TRUE;
}

/*<A NAME="ZJul_TestDay"></A>
********************************************************************************
* RL_BOOL ZJul_TestDay(TOKEN *token, RL_INT4 year, RL_INT4 month, RL_INT4 *day)
*
* This internal function tests a token to determine if it is a valid day-of-
* month.  It returns TRUE if it is valid, FALSE otherwise.
*
* Input:
*	token		token structure.
*	year		year 1900-2100.
*	month		month value 1-12
*
* Output:
*	*doy		day-of-month value 1-31 if a valid token is found.
*
* Return:		TRUE if token was valid; FALSE otherwise.
*******************************************************************************/

static RL_BOOL ZJul_TestDay(token, year, month, day)
TOKEN	*token;
RL_INT4	year, month, *day;
{
	if (token->is_alpha) return FALSE;

	if (token->number < 1) return FALSE;

	if (token->number <= 28 || token->number <= Jul_MonthDays(year,month)) {
		*day = token->number;
		return TRUE;
	}

	return FALSE;
}

/*<A NAME="ZJul_TestDOY"></A>
********************************************************************************
* RL_BOOL ZJul_TestDOY(TOKEN *token, RL_INT4 year, RL_INT4 *doy)
*
* This internal function tests a token to determine if it is a valid day-of-
* year.  It returns TRUE if it is valid; FALSE otherwise.
*
* Input:
*	token		token structure.
*	year		year 1900-2100.
*
* Output:
*	*doy		day-of-year value 1-366 if a valid token is found.
*
* Return:		TRUE if token was valid; FALSE otherwise.
*******************************************************************************/

static RL_BOOL ZJul_TestDOY(token, year, doy)
TOKEN	*token;
RL_INT4	year, *doy;
{
	if (token->is_alpha) return FALSE;

	if (token->number < 1) return FALSE;

	if (token->number <= 365 || token->number == Jul_YearDays(year)) {
		*doy = token->number;
		return TRUE;
	}

	return FALSE;
}

/*<A NAME="ZJul_TimeTokens"></A>
********************************************************************************
* INTERNAL FUNCTIONS TO INTERPRET TIME FORMATS
********************************************************************************
* RL_BOOL ZJul_TimeTokens(TOKEN *tokens, RL_INT4 ntokens, RL_BOOL isleap,
*			RL_FLT8 *secs)
*
* This internal function interprets a sequence of tokens as a time-of-day.
*
* Input:
*	tokens[0...ntokens-1]	array of tokens to interpret.
*	isleap			TRUE if this is a leap day; FALSE otherwise.
*
* Output:
*	*secs			number of seconds into day.
*
* Return:			TRUE if a valid sequence of tokens was found;
*				FALSE otherwise.
*******************************************************************************/

static RL_BOOL ZJul_TimeTokens(tokens, ntokens, isleap, secs)
TOKEN	*tokens;
RL_INT4	ntokens;
RL_BOOL	isleap;
RL_FLT8	*secs;
{
RL_INT4	hour, minute, second, millisec, ampm, index, maxsecs;
RL_BOOL	status;
RL_FLT8	frac, scale;
RL_CHAR	last;

	hour     = 0;
	minute   = 0;
	second   = 0;
	millisec = 0;
	frac     = 0.;

/*******************************************************************************
* Handle empty token string
*******************************************************************************/

	if (ntokens == 0) {
		*secs = 0.;
		return TRUE;
	}

/*******************************************************************************
* Test whether the last token is AM or PM
*******************************************************************************/

	if      (strcmp(tokens[ntokens-1].string, "AM") == 0) ampm = 1;
	else if (strcmp(tokens[ntokens-1].string, "PM") == 0) ampm = 2;
	else                                                  ampm = 0;

	if (ampm) {
		ntokens--;
		status = ZJul_TestDelim(tokens[ntokens-1].delim, " :/-");
		if (!status) return FALSE;
	}

/*******************************************************************************
* Check for fractional part in last token
*******************************************************************************/

	if (ntokens >= 2 && tokens[ntokens-2].delim == '.') {
		status = ZJul_FracToken(tokens + ntokens-1, &frac);
		if (!status) return FALSE;
		if (tokens[ntokens-1].delim == '.') return FALSE;

		tokens[ntokens-2].delim = tokens[ntokens-1].delim;
		ntokens--;
	}

	if (tokens[ntokens-1].delim == '.') tokens[ntokens-1].delim = ' ';

/*******************************************************************************
* Test token count and final delimiter
*******************************************************************************/

	if (ntokens < 1) return FALSE;

	if (ampm) status = ZJul_TestDelim(tokens[ntokens-1].delim, "HMS -/:");
	else      status = ZJul_TestDelim(tokens[ntokens-1].delim, "HMS -/:Z");
	if (!status) return FALSE;

	last = ' ';

/*******************************************************************************
* Interpret hours
*******************************************************************************/

	index = 0;
	if (tokens[index].delim == 'M') goto MINUTES;
	if (tokens[index].delim == 'S') goto SECONDS;

	status = ZJul_TestDelim(tokens[index].delim, "H: Z");
	if (!status) return FALSE;

	if (ampm) {
		status = ZJul_TestHMS(tokens+index, 1, 13, &hour);

		if (hour >= 12) hour -= 12;
		if (ampm == 2)  hour += 12;
	}
	else {
		status = ZJul_TestHMS(tokens+index, 0, 24, &hour);
	}
	if (!status) return FALSE;

	last = 'H';
	isleap = (isleap && hour == 23);

	index++;
	if (index >= ntokens) goto DONE;

/*******************************************************************************
* Interpret minutes
*******************************************************************************/

MINUTES:
	if (tokens[index].delim == 'S') goto SECONDS;

	status = ZJul_TestDelim(tokens[index].delim, "M: Z");
	if (!status) return FALSE;

	if (last == 'H') status = ZJul_TestHMS(tokens+index, 0,   60, &minute);
	else             status = ZJul_TestHMS(tokens+index, 0, 1440, &minute);

	if (!status) return FALSE;

	last = 'M';
	isleap = (isleap && minute + 60*hour == 1439);

	index++;
	if (index >= ntokens) goto DONE;

/*******************************************************************************
* Interpret seconds
*******************************************************************************/

SECONDS:
	status = ZJul_TestDelim(tokens[index].delim, "S: Z");
	if (!status) return FALSE;

	if      (last == 'M') maxsecs =    60 + (isleap ? 1:0);
	else if (last == 'H') maxsecs =  3600 + (isleap ? 1:0);
	else                  maxsecs = 86400 + (isleap ? 1:0);

	status = ZJul_TestHMS(tokens+index, 0, maxsecs, &second);
	if (!status) return FALSE;

	last = 'S';
	index++;
	if (index >= ntokens) goto DONE;

/*******************************************************************************
* Interpret milliseconds
*******************************************************************************/

	status = ZJul_TestDelim(tokens[index].delim, " Z");
	if (!status) return FALSE;

	status = ZJul_TestHMS(tokens+index, 0, 1000, &millisec);
	if (!status) return FALSE;

	last = 'Z';
	index++;
	if (index >= ntokens) goto DONE;

/* Any additional token constitutes an error */
	return FALSE;

/*******************************************************************************
* Assemble number of seconds
*******************************************************************************/

DONE:
	*secs = millisec/1000. + (RL_FLT8) (second + 60*(minute + 60*hour));

	if (frac != 0) {
		if      (last == 'H') scale = 3600. + (isleap ? 1:0);
		else if (last == 'M') scale =   60. + (isleap ? 1:0);
		else if (last == 'S') scale =    1.;
		else                  scale =    0.001;

		*secs += frac * scale;
	}

	return TRUE;
}

/*<A NAME="ZJul_TestHMS"></A>
********************************************************************************
* RL_BOOL ZJul_TestHMS(TOKEN *token, RL_INT4 minval, RL_INT4 maxval,
*			RL_INT4 *value);
*
* This internal extracts an integer token and confirms that it is in the allowed
* range.
*
* Input:
*	token		pointer to the token.
*	minval		lower limit on value (inclusive).
*	maxval		upper limit on value (exclusive).
*
* Output:
*	*value		numeric value inferred.
*
* Return:		TRUE if it is a valid token; FALSE othewise.
*******************************************************************************/

static RL_BOOL ZJul_TestHMS(token, minval, maxval, value)
TOKEN	*token;
RL_INT4	minval, maxval, *value;
{

/* Interpret numeric value */
	if (token->is_alpha) return FALSE;

/* Test numeric value */
	if (token->number < minval || token->number >= maxval) return FALSE;

	*value = token->number;
	return TRUE;
}

/*<A NAME="ZJul_JDTokens"></A>
********************************************************************************
* INTERNAL FUNCTION TO INTEPRET JULIAN DATE FORMATS
********************************************************************************
* RL_BOOL ZJul_JDTokens(TOKEN *tokens, RL_INT4 ntokens, RL_FLT8 *tai)
*
* This internal function interprets a sequence of tokens as a Julian Date or a
* Modified Julian Date.
*
* Input:
*	tokens[0...ntokens-1]	array of tokens to interpret.
*
* Output:
*	*tai			on success, seconds from J2000 TAI.
*
* Return:			TRUE if a valid sequence of tokens was found;
*				FALSE otherwise.
*******************************************************************************/

static RL_BOOL ZJul_JDTokens(tokens, ntokens, tai)
TOKEN	*tokens;
RL_INT4	ntokens;
RL_FLT8	*tai;
{
RL_INT4	isJD, isMJD;
RL_FLT8	value, frac;
RL_BOOL	status;

/* Test for JD or MJD prefix */
	if (!tokens[0].is_alpha) return FALSE;

	isJD  = (strcmp(tokens[0].string, "JD")  == 0);
	isMJD = (strcmp(tokens[0].string, "MJD") == 0);
	if (!isJD && !isMJD) return FALSE;

	status = ZJul_TestDelim(tokens[0].delim, " -");
	if (!status) return FALSE;

/* Interpret number */
	switch (ntokens) {

	case 2:
		if (tokens[1].is_alpha) return FALSE;
		status = ZJul_TestDelim(tokens[1].delim, " .");
		if (!status) return FALSE;

		value = (RL_FLT8) tokens[1].number;
		break;

	case 3:
		if (tokens[1].is_alpha) return FALSE;
		if (tokens[1].delim != '.') return FALSE;
		if (tokens[2].delim != ' ') return FALSE;

		status = ZJul_FracToken(tokens+2, &frac);
		if (!status) return FALSE;

		value = (RL_FLT8) tokens[1].number + frac;
		break;

	default:
		return FALSE;
	}

/* Convert numeric value to TAI */
	if (isMJD) *tai = Jul_TAIofMJD(value, JUL_UTC_TYPE);
	else       *tai = Jul_TAIofJD(value, JUL_UTC_TYPE);

	return TRUE;
}

/*<A NAME="ZJul_FillTokens"></A>
********************************************************************************
* INTERNAL FUNCTIONS FOR MANIPULATING TOKENS
********************************************************************************
* RL_INT4 ZJul_FillTokens(RL_CHAR *string, TOKEN *tokens, RL_INT4 max_tokens)
*
* This internal function parses a character string into a sequence of tokens.
*
* Input:
*	string		null-terminated character string.
*	tokens[0...max_tokens-1]
*			array of destination tokens.
*
* Output:
*	tokens[0...]	updated tokens.
*
* Return:		number of tokens written.  If >max_tokens, then not
*			enough destination tokens were provided so part of the
*			string was not parsed.
*******************************************************************************/

static RL_INT4 ZJul_FillTokens(string, tokens, max_tokens)
RL_CHAR	*string;
TOKEN	*tokens;
RL_INT4	max_tokens;
{
RL_CHAR	*s;
RL_INT4	ntokens;

	s = string;
	for (ntokens=0; ntokens<MAX_TOKENS; ntokens++) {
		if (*s == '\0') break;
		s = ZJul_NextToken(s, tokens + ntokens);
	}

	return ntokens;
}

/*<A NAME="ZJul_NextToken"></A>
********************************************************************************
* RL_CHAR *ZJul_NextToken(RL_CHAR *string, TOKEN *token)
*
* This internal function extracts the first token and delimiter from the given
* input string and then returns a pointer to the string following the delimiter.
* A token consists of an uninterrupted sequence of either numeric or alphabetic
* characters (but not a mixture of each).  A single letter is treated as a
* delimiter, not a token.  Alphabetic characters are converted to upper case.
* A blank delimiter is returned at the end of the string, or wherever the
* delimiter is merely a transition from alphabetic to numeric or back.
*
* Input:
*	string		input string;
*
* Output:
*	*token		token found.
*
* Return:		pointer to the beginning of the next token in the
*			string, which may be the string's terminal '\0'.
*******************************************************************************/

static RL_CHAR *ZJul_NextToken(string, token)
RL_CHAR	*string;
TOKEN	*token;
{
RL_CHAR	*s;
RL_INT4	i;

/* Skip over leading whitespace */
	s = string;
	while (isspace(*s)) s++;

/* Copy a numeric string and (possibly alphabetic) delimiter */
	if (isdigit(*s)) {
		for (i=0; i<TOKEN_LEN; i++) {
			if (!isdigit(*s)) break;
			token->string[i] = *s;
			s++;
		}
		token->string[i] = '\0';
		token->is_alpha = FALSE;
		token->number = atol(token->string);

		while (isspace(*s)) s++;

		if (*s == '\0' || isdigit(*s) ||
		   (isalpha(*s) && isalpha(s[1]))) {
			token->delim = ' ';
		}
		else {
			token->delim = toupper(*s);
			for (s++; isspace(*s); s++) ;
		}
	}

/* Copy an alphabetic string and delimiter */
	else if (isalpha(*s) && isalpha(s[1])) {
		for (i=0; i<TOKEN_LEN; i++) {
			if (!isalpha(*s)) break;
			token->string[i] = toupper(*s);
			s++;
		}
		token->string[i] = '\0';
		token->is_alpha = TRUE;

		while (isspace(*s)) s++;

		if (*s == '\0' || isalpha(*s) || isdigit(*s)) {
			token->delim = ' ';
		}
		else {
			token->delim = *s;
			for (s++; isspace(*s); s++) ;
		}
	}		

/* Otherwise, copy an empty token with delimiter */
	else {
		token->string[0] = '\0';
		token->is_alpha = TRUE;
		if (*s == '\0') {
			token->delim = ' ';
		}
		else {
			token->delim = toupper(*s);
			for (s++; isspace(*s); s++) ;
		}
	}

	return s;
}

/*<A NAME="ZJul_TestDelim"></A>
********************************************************************************
* RL_BOOL ZJul_TestDelim(RL_INT4 delim, RL_CHAR *choices)
*
* This internal function tests a delimiter against a set of choices.  It returns
* TRUE if the delimiter is found in the set; otherwise it returns FALSE.
*
* Input:
*	delim		delimiter character to test.
*	choices		null-terminated string of delimiter choices.
*
* Return:		TRUE if the delimiter is found in the set of choices;
*			FALSE otherwise.
*******************************************************************************/

static RL_BOOL ZJul_TestDelim(delim, choices)
RL_INT4	delim;
RL_CHAR	*choices;
{
RL_CHAR	*c;

	for (c = choices; *c != '\0'; c++) {
		if (*c == delim) return TRUE;
	}

	return FALSE;
}

/*<A NAME="ZJul_FracToken"></A>
********************************************************************************
* RL_INT4 ZJul_FracToken(TOKEN tokens, RL_FLT8 *frac)
*
* This internal function interprets a token as the fractional part of a double-
* precision number.
*
* Input:
*	token		pointer to a pair of tokens.
*
* Output:
*	*frac		fractional value found on successful conversion.
*
* Return:		TRUE if a valid interpretation was found; FALSE
*			otherwise.
*******************************************************************************/

static RL_BOOL ZJul_FracToken(token, frac)
TOKEN	*token;
RL_FLT8	*frac;
{
RL_CHAR	string[TOKEN_LEN + 2];

	if (token->is_alpha) return FALSE;

	string[0] = '.';
	strcpy(string+1, token->string);

	*frac = atof(string);
	return TRUE;
}

/*<A NAME="FJul_ParseDT"></A>
********************************************************************************
* FORTRAN INTERFACE ROUTINES
********************************************************************************
*$ Component_name:
*	FJul_ParseDT
*$ Abstract:
*	Interprets a character string as a date and time.
*$ Keywords:
*	JULIAN, TIME, PARSING
*	FORTRAN, PUBLIC
*$ Declarations:
*	logical*4 function FJul_ParseDT(string, pref, dutc, secs)
*	character*(*)	string, pref
*	integer*4	dutc
*	real*8		secs
*$ Inputs:
*	string		character string to parse.
*	pref		optional three-character string giving the preferred
*			order for year, month and day values (see
*			FJul_ParseDate).
*$ Outputs:
*	dutc		inferred days relative to January 1, 2000.
*	secs		inferred seconds into day.
*$ Returns:
*	.TRUE. if a valid string interpretation was found; .FALSE. otherwise.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function interprets a character string as a date and time.
*
*	A string beginning with "JD" is interpreted as a Julian date; a string
*	beginning with "MJD" is interpreted as a Modified Julian Date.
*
*	Otherwise, the string is first separated into a sequence of tokens
*	separated by delimiters.  A token consists of any sequence of letters
*	or digits (but not a mixture of both).  A delimiter is any punctuation
*	or a single letter separating numeric tokens.  Blanks are not
*	significant except as delimiters when nothing else is present.
*
*	The string is parsed by testing each possible division between date and
*	time tokens until it finds one that produces both a valid date
*	according to FJul_ParseDate() and a valid time according to
*	FJul_ParseTime().  It gives preference for the longest possible date
*	token sequence; it also gives preference for dates before times.
*
*	If no valid date/time interpretation is found, it returns .FALSE.
*$ External_references:
*	Jul_ParseDT(), FORT_Cstring()
*$ Examples:
*	none
*$ Error_handling:
*	It returns .FALSE. if no valid interpretation is found; no error message
*	is printed.
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: December 1995
*$ Change_history:
*	none
********************************************************************************
* Note: GJul_ParseDT() defined here is the intermediary routine between
* FJul_ParseDT() and Jul_ParseDT(), allowing for the fact that strings cannot
* be passed directly between FORTRAN and C.  See fjulian.for for the rest of
* the code.
*
* subroutine GJul_ParseDT(string, pref, dutc, secs)
* byte		string, pref
* integer*4	dutc
* real*8	secs
*******************************************************************************/

RL_INT4	FORTRAN_NAME(gjul_parsedt) (string, pref, dutc, secs)
RL_CHAR	*string, *pref;
RL_INT4	*dutc;
RL_FLT8	*secs;
{
	FORT_Init();

	return (Jul_ParseDT(string,pref,dutc,secs) ? FTRUE:FFALSE);
}

/*<A NAME="FJul_ParseDate"></A>
********************************************************************************
*$ Component_name:
*	FJul_ParseDate
*$ Abstract:
*	Interprets a character string as a date.
*$ Keywords:
*	JULIAN, TIME, PARSING
*	FORTRAN, PUBLIC
*$ Declarations:
*	logical*4 function FJul_ParseDate(string, pref, dutc)
*	character*(*)	string, pref
*	integer*4	dutc
*$ Inputs:
*	string		the character string to parse.
*	pref		optional three-character string giving the preferred
*			order for year, month and day values (see below).
*$ Outputs:
*	dutc		inferred days relative to January 1, 2000.
*$ Returns:
*	.TRUE. if a valid string interpretation was found; .FALSE. otherwise.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function interprets a character string as a date.
*
*	The string is first separated into a sequence of tokens separated by
*	delimiters.  A token consists of any sequence of letters or digits (but
*	not a mixture of both).  A delimiter is any punctuation or a single
*	letter separating numeric tokens.  Blanks are not signficant except as
*	delimiters when nothing else is present.
*
*	A single token is interpreted as either a four-digit year or as an
*	eight-digit merged date of the form "yyyymmdd".  A pair of tokens is
*	interpreted as a year plus day-of-year in that order.
*
*	A set of three tokens is interpreted as a month, day and year in some
*	order.  Year values must be four digits or else 0-49 (which is
*	interpreted as 2000-2049) or 50-99 (which is interpreted as 1950-1999). 
*	Months are integers 1-12 or English names January-December, abbreviated
*	to three or more letters).  Days are integers between 1 and 31 (or less,
*	depending on the month).  The sequence of tokens is interpreted in the
*	preferred order first, then as month-day-year, day-month-year, and
*	year-month-day until a valid interpretation is found.
*
*	The pref argument, if used, enables the user to specify the preferred
*	ordering for year, month, and day.  It is a three-character string
*	containing a "Y", "M" and "D", where the order of the three indicates
*	the preferred ordering for the year, month and day, respectively.
*
*	Valid delimiters are "/", "-", "." or blank.  For three tokens, the
*	delimiters between the tokens must be the same except for the special
*	case "month day, year" where a comma is allowed after the second token.
*	The final delimiter may be a "T" to be compatible with PDS date formats.
*
*	If no valid interpretation is found, the function returns .FALSE.
*$ External_references:
*	Jul_ParseDate(), FORT_Cstring()
*$ Examples:
*	none
*$ Error_handling:
*	It returns .FALSE. if no valid interpretation is found; no error message
*	is printed.
*$ Limitations:
*	none
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: December 1995
*$ Change_history:
*	none
********************************************************************************
* Note: GJul_ParseDate() defined here is the intermediary routine between
* FJul_ParseDate() and Jul_ParseDate(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fjulian.for for the
* rest of the code.
*
* subroutine GJul_ParseDate(string, pref, dutc)
* byte		string, pref
* integer*4	dutc
*******************************************************************************/

RL_INT4	FORTRAN_NAME(gjul_parsedate) (string, pref, dutc)
RL_CHAR	*string, *pref;
RL_INT4	*dutc;
{
	FORT_Init();

	return (Jul_ParseDate(string,pref,dutc) ? FTRUE:FFALSE);
}

/*<A NAME="FJul_ParseTime"></A>
********************************************************************************
*$ Component_name:
*	FJul_ParseTime
*$ Abstract:
*	Interprets a character string as a time.
*$ Keywords:
*	JULIAN, TIME, PARSING
*	FORTRAN, PUBLIC
*$ Declarations:
*	logical*4 function FJul_ParseTime(string, isleap, secs)
*	character*(*)	string
*	logical*4	isleap
*	real*8		secs
*$ Inputs:
*	string		the character string to parse.
*	isleap		.TRUE. if the day has a leap second; .FALSE. otherwise.
*$ Outputs:
*	dutc		inferred seconds into day.
*$ Returns:
*	.TRUE. if a valid string interpretation was found; .FALSE. otherwise.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function interprets a character string as a time.
*
*	The string is first separated into a sequence of tokens separated by
*	delimiters.  A token consists of any sequence of letters or digits (but
*	not a mixture of both).  A delimiter is any punctuation or a single
*	letter separating numeric tokens.  Blanks are not signficant except as
*	delimiters when nothing else is present.
*
*	Tokens are interpreted as hour, minute, second and millisecond in that
*	order although not all a required; the last token may have a fractional
*	part.  The string may end in "AM" or "PM", or else "Z" to be compatible
*	with PDS time formats.  Valid delimiters are colons or blanks.
*
*	Individual tokens may also end in "H", "M" or "S" to indicate hours,
*	minutes or seconds respectively.  This enables the user to express a
*	time in minutes (without hours) or seconds (without hours and minutes).
*
*	If no valid interpretation is found, the function returns .FALSE.
*$ External_references:
*	Jul_ParseTime(), FORT_Cstring()
*$ Examples:
*	none
*$ Error_handling:
*	It returns .FALSE. if no valid interpretation is found; no error message
*	is printed.
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
********************************************************************************
* Note: GJul_ParseTime() defined here is the intermediary routine between
* FJul_ParseTime() and Jul_ParseTime(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fjulian.for for the
* rest of the code.
*
* subroutine GJul_ParseTime(string, isleap, secs)
* byte		string
* logical*4	isleap
* real*8	secs
*******************************************************************************/

RL_INT4	FORTRAN_NAME(gjul_parsetime) (string, isleap, secs)
RL_CHAR	*string;
RL_INT4	*isleap;
RL_FLT8	*secs;
{
	FORT_Init();

	return (Jul_ParseTime(string, (RL_BOOL) *isleap, secs) ? FTRUE:FFALSE);
}

/*******************************************************************************
</PRE></BODY></HTML>*/
