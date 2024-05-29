/*<HTML><HEAD><TITLE> format.c </TITLE></HEAD><BODY><PRE>
********************************************************************************
* format.c
*
* This set of routines reformats dates and times into a variety of formats.
*
* void    Jul_FormatDate(dutc, format, string)
*		prepares a date in general format.
* RL_INT4 Jul_FormatTime(secs, isleap, format, string)
*		prepares a time in general format.
* void    Jul_FormatPDS(dutc, secs, ndigits, useZ, string)
*		formats the date and time in PDS format.
* void    Jul_FormatSQL(dutc, secs, string)
*		formats the date and time in SQL format.
*
* Mark Showalter, PDS Rings Node, November 1995
* Revised by MRS 7/97 with minor updates.
* Revised by MRS 6/98 to conform to RingLib naming conventions.
*******************************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "julian.h"
#include "fortran.h"

/***********************************************************
* Definitions
***********************************************************/

#define		TEMPSTR_LEN	255	/* temporary string length */
#define		ESCAPE		'\\'	/* escape character */

/***********************************************************
* Static variables
***********************************************************/

static RL_CHAR *(lower_months[12]) =
			{"january",   "february", "march",    "april",
			 "may",       "june",     "july",     "august",
			 "september", "october",  "november", "december"};
static RL_CHAR *(mixed_months[12]) =
			{"January",   "February", "March",    "April",
			 "May",       "June",     "July",     "August",
			 "September", "October",  "November", "December"};
static RL_CHAR *(upper_months[12]) =
			{"JANUARY",   "FEBRUARY", "MARCH",    "APRIL",
			 "MAY",       "JUNE",     "JULY",     "AUGUST",
			 "SEPTEMBER", "OCTOBER",  "NOVEMBER", "DECEMBER"};

/***********************************************************
* Prototypes of internal functions
***********************************************************/

static RL_INT4  ZJul_RoundSecs  RL_PROTO((RL_FLT8 secs, RL_BOOL isleap,
                                          RL_INT4 ndigits, RL_INT4 *isecond,
                                          RL_INT4 *ifrac));
static RL_BOOL  ZJul_ReplaceInt RL_PROTO((RL_CHAR *string, RL_CHAR *match,
                                          RL_CHAR *format, RL_INT4 value));
static RL_BOOL  ZJul_ReplaceStr RL_PROTO((RL_CHAR *string, RL_CHAR *match,
                                          RL_CHAR *format, RL_CHAR *value));
static RL_CHAR *ZJul_FindMatch  RL_PROTO((RL_CHAR *string, RL_CHAR *match,
                                          RL_CHAR **after));
static void     ZJul_RemoveEsc  RL_PROTO((RL_CHAR *string));

/*<A NAME="Jul_FormatDate"></A>
********************************************************************************
* EXPORTED USER ROUTINES
********************************************************************************
*$ Component_name:
*	Jul_FormatDate (format.c)
*$ Abstract:
*	Formats a date according to a format string.
*$ Keywords:
*	JULIAN, TIME, FORMATTING
*	C, PUBLIC
*$ Declarations:
*	void		Jul_FormatDate(dutc, format, string)
*	RL_INT4		dutc;
*	RL_CHAR		*format, *string;
*$ Inputs:
*	dutc		day relative to January 1, 2000.
*	*format		null-terminated format string (see detailed
*			description).
*$ Outputs:
*	*string		null-terminated character string containing formatted
*			date.
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function formats a date according to a format string.  The format
*	string is copied into the output string except for the following
*	substitutions:
*		YYYY or yyyy	4-digit year number.
*		YY or y		last two digits of year number.
*		Y or y		year number.
*		MONTH		full month name (all upper-case).
*		Month		full month name (first letter upper-case).
*		month		full month name (all lower-case).
*		MON		3-letter month name (all upper-case).
*		Mon		3-letter month name (first letter upper-case).
*		mon		3-letter month name (all lower-case).
*		MM or mm	2-digit month number.
*		M or m		month number.
*		DD or dd	2-digit day number.
*		D or d		day number.
*	The above substitutions do not take place if the pattern appears as a
*	part of a longer character string.
*
*	An escape character "\" is removed from the string but prevents the
*	next character from being either modified or treated as a part of a
*	neighboring match pattern.  Use "\\" to insert the escape character into
*	the output string.
*$ External_references:
*	Jul_YMDofDUTC()
*$ Examples:
*	Jul_FormatDate(dutc, "yyyy-mm-dd\T", string)
*		produces a string in PDS date format, including the final "T".
*	Jul_FormatDate(dutc, "Month d, y", string)
*		produces a more readable date like "July 4, 1776".
*$ Error_handling:
*	none
*$ Limitations:
*	The dimensioned length of the character string must be long enough to
*	accomodate the output string plus a final null character.  This length
*	is not checked at runtime.
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
*******************************************************************************/

void	Jul_FormatDate(dutc, format, string)
RL_INT4	dutc;
RL_CHAR *format, *string;
{
RL_INT4	year, month, day;
RL_BOOL	done;

	Jul_YMDofDUTC(dutc, &year, &month, &day);
	strcpy(string, format);

/* Replace year field */
	             done = ZJul_ReplaceInt(string, "YYYY", "%04d", year);
	if (!done) { done = ZJul_ReplaceInt(string, "yyyy", "%04d", year);
	if (!done) { done = ZJul_ReplaceInt(string, "YY",   "%0d",  year%100);
	if (!done) { done = ZJul_ReplaceInt(string, "yy",   "%0d",  year%100);
	if (!done) { done = ZJul_ReplaceInt(string, "Y",    "%d",   year);
	if (!done) { done = ZJul_ReplaceInt(string, "y",    "%d",   year);
	}}}}}

/* Replace month field */
	             done = ZJul_ReplaceStr(string, "MONTH", "%s",
							upper_months[month-1]);
	if (!done) { done = ZJul_ReplaceStr(string, "Month", "%s",
							mixed_months[month-1]);
	if (!done) { done = ZJul_ReplaceStr(string, "month", "%s",
							lower_months[month-1]);
	if (!done) { done = ZJul_ReplaceStr(string, "MON",   "%3.3s",
							upper_months[month-1]);
	if (!done) { done = ZJul_ReplaceStr(string, "Mon",   "%3.3s",
							mixed_months[month-1]);
	if (!done) { done = ZJul_ReplaceStr(string, "mon",   "%3.3s",
							lower_months[month-1]);
	if (!done) { done = ZJul_ReplaceInt(string, "MM",    "%02d", month);
	if (!done) { done = ZJul_ReplaceInt(string, "mm",    "%02d", month);
	if (!done) { done = ZJul_ReplaceInt(string, "M",     "%d",   month);
	if (!done) { done = ZJul_ReplaceInt(string, "m",     "%d",   month);
	}}}}}}}}}

/* Replace day field */
	             done = ZJul_ReplaceInt(string, "DD", "%02d", day);
	if (!done) { done = ZJul_ReplaceInt(string, "dd", "%02d", day);
	if (!done) { done = ZJul_ReplaceInt(string, "D",  "%d",   day);
	if (!done) { done = ZJul_ReplaceInt(string, "d",  "%d",   day);
	}}}

/* Remove escape characters */
	ZJul_RemoveEsc(string);

}

/*<A NAME="Jul_FormatTime"></A>
********************************************************************************
*$ Component_name:
*	Jul_FormatTime (format.c)
*$ Abstract:
*	Formats a time according to a format string.
*$ Keywords:
*	JULIAN, TIME, FORMATTING
*	C, PUBLIC
*$ Declarations:
*	RL_INT4		Jul_FormatTime(secs, isleap, format, string)
*	RL_FLT8		secs;
*	RL_BOOL		isleap;
*	RL_CHAR 	*format, *string;
*$ Inputs:
*	secs		number of seconds into day.
*	isleap		TRUE if this day has a leap second; FALSE otherwise.
*	*format		null-terminated format string (see detailed
*			description).
*$ Outputs:
*	*string		null-terminated character string containing formatted
*			time.
*$ Returns:
*	1 if rounding error shifts the time to the following day; 0 otherwise.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function formats a date according to a format string.  The format
*	string is copied into the output string except for the following
*	substitutions:
*		AM or PM	upper-case AM or PM, depending on time.
*		am or pm	lower-case am or pm, depending on time.
*		HH or hh	2-digit hour.
*		H or h		hour.
*		MM or mm	2-digit minute.
*		M or m		minute.
*		SS or ss	2-digit second.
*		S or s		second.
*		ZZ... or zz...	Any sequence is replaced by an equal number of
*				fractional seconds digits.
*	The above substitutions do not take place if the pattern appears as a
*	part of a longer character string.
*
*	An escape character "\" is removed from the string but prevents the
*	next character from being either modified or treated as a part of a
*	neighboring match pattern.  Use "\\" to insert the escape character into
*	the output string.
*$ External_references:
*	Jul_HMSofSec()
*$ Examples:
*	This fragment of code formats a date and time.  Note that it uses the
*	returned date offset when formatting the time; the reason is that a time
*	can round up to midnight, which corresponds to the following day.
*
*	RL_INT4	dutc, delta;
*	RL_FLT8	secs;
*	RL_CHAR time_string[80], date_string[80];
*
*	delta = Jul_FormatTime(secs, Jul_IsLeapDay(dutc), "hh:mm:ss.zzz",
*			time_string);
*	Jul_FormatDate(dutc + delta, "yyyy-mm-dd", date_string);
*
*$ Error_handling:
*	none
*$ Limitations:
*	The dimensioned length of the character string must be long enough to
*	accomodate the output string plus a final null character.  This length
*	is not checked at runtime.
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
*******************************************************************************/

RL_INT4	Jul_FormatTime(secs, isleap, format, string)
RL_FLT8	secs;
RL_BOOL	isleap;
RL_CHAR	*format, *string;
{
RL_INT4	delta, hour, minute, isecond, ifrac, ndigits;
RL_CHAR	*s, *zptr, tempstr[80];
RL_FLT8	second;
RL_BOOL	done, ispm;

/* Initialize output string */
	strcpy(string, format);

/* Count 'z's to determine number of fractional digits */
	for (s = string; *s != '\0'; s++) {
		if (*s == 'z' || *s == 'Z') break;
	}

	zptr = s;
	ndigits = 0;
	for ( ; *s == 'z' || *s == 'Z'; s++) {
		ndigits++;
	}

/* Replace by a sequence of 'z's by a single 'z' */
	if (ndigits > 1) {
		*zptr = 'z';
		zptr++;
		for ( ; *s != '\0'; zptr++, s++) {
			*zptr = *s;
		}
		*zptr = '\0';
	}

/* Round seconds */
	delta = ZJul_RoundSecs(secs, isleap, ndigits, &isecond, &ifrac);
	Jul_HMSofSec((RL_INT4) isecond, &hour, &minute, &second);
	isecond = (RL_INT4) second;

/* Replace AM/PM field */
	ispm = (hour > 12);
	if (ispm) {
		             done = ZJul_ReplaceStr(string, "AM", "%s", "PM");
		if (!done) { done = ZJul_ReplaceStr(string, "PM", "%s", "PM");
		if (!done) { done = ZJul_ReplaceStr(string, "am", "%s", "pm");
		if (!done) { done = ZJul_ReplaceStr(string, "pm", "%s", "pm");
		}}}
	}
	else {
		             done = ZJul_ReplaceStr(string, "AM", "%s", "AM");
		if (!done) { done = ZJul_ReplaceStr(string, "PM", "%s", "AM");
		if (!done) { done = ZJul_ReplaceStr(string, "am", "%s", "am");
		if (!done) { done = ZJul_ReplaceStr(string, "pm", "%s", "am");
		}}}
	}

	if (done) {
		if (ispm) hour -= 12;
		if (hour == 0) hour = 12;
	}

/* Replace hour field */
	             done = ZJul_ReplaceInt(string, "HH", "%02d", hour);
	if (!done) { done = ZJul_ReplaceInt(string, "hh", "%02d", hour);
	if (!done) { done = ZJul_ReplaceInt(string, "H",  "%d",   hour);
	if (!done) { done = ZJul_ReplaceInt(string, "h",  "%d",   hour);
	}}}

/* Replace minute field */
	             done = ZJul_ReplaceInt(string, "MM", "%02d", minute);
	if (!done) { done = ZJul_ReplaceInt(string, "mm", "%02d", minute);
	if (!done) { done = ZJul_ReplaceInt(string, "M",  "%d",   minute);
	if (!done) { done = ZJul_ReplaceInt(string, "m",  "%d",   minute);
	}}}

/* Replace sub-second field */
	
	             done = ZJul_ReplaceInt(string, "MM", "%02d", minute);
	if (!done) { done = ZJul_ReplaceInt(string, "mm", "%02d", minute);
	if (!done) { done = ZJul_ReplaceInt(string, "M",  "%d",   minute);
	if (!done) { done = ZJul_ReplaceInt(string, "m",  "%d",   minute);
	}}}

/* Replace second field */
	             done = ZJul_ReplaceInt(string, "SS", "%02d", isecond);
	if (!done) { done = ZJul_ReplaceInt(string, "ss", "%02d", isecond);
	if (!done) { done = ZJul_ReplaceInt(string, "S",  "%d",   isecond);
	if (!done) { done = ZJul_ReplaceInt(string, "s",  "%d",   isecond);
	}}} 

/* Replace sub-second field */
	sprintf( tempstr, "%0*d", ndigits, ifrac);
	done = ZJul_ReplaceStr(string, "z", "%s", tempstr);

/* Remove escape characters */
	ZJul_RemoveEsc(string);

/* Return change in date */
	return delta;
}

/*<A NAME="Jul_FormatPDS"></A>
********************************************************************************
*$ Component_name:
*	Jul_FormatPDS (format.c)
*$ Abstract:
*	This function formats a date and time in PDS format.
*$ Keywords:
*	JULIAN, TIME, FORMATTING
*	C, PUBLIC
*$ Declarations:
*	void		Jul_FormatPDS(dutc, secs, ndigits, useZ, string)
*	RL_INT4		dutc, nfrac;
*	RL_FLT8		secs;
*	RL_BOOL		useZ;
*	RL_CHAR		*string;
*$ Inputs:
*	dutc		day relative to January 1, 2000.
*	secs		number of seconds into day.
*	ndigits		number of fractional digits to use in second field.
*	useZ		TRUE to append a "Z" character; FALSE to leave it out.
*$ Outputs:
*	*string		null-terminated character string containing formatted
*			date and time.
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function formats a date and time in PDS format.
*$ External_references:
*	Jul_YMDodDUTC(), Jul_HMSofSec()
*$ Examples:
*	none
*$ Error_handling:
*	none
*$ Limitations:
*	The dimensioned length of the character string must be at least
*	(21+ndigits) characters.  This length is not checked at runtime.  PDS
*	format has no representation for years BCE.
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.2: July 1997
*$ Change_history:
*	1.0: November 1995
*	1.2: July 1997		-- updated so that is uses the minimum possible
*				   string length when useZ is FALSE.
*******************************************************************************/

void	Jul_FormatPDS(dutc, secs, ndigits, useZ, string)
RL_INT4	dutc, ndigits;
RL_FLT8	secs;
RL_BOOL	useZ;
RL_CHAR	*string;
{
RL_FLT8	second; 
RL_INT4	year, month, day, hour, minute, isecond, ifrac;

	dutc += ZJul_RoundSecs(secs, Jul_IsLeapDay(dutc), ndigits,
			&isecond, &ifrac);

	Jul_YMDofDUTC(dutc, &year, &month, &day);
	Jul_HMSofSec((RL_FLT8) isecond, &hour, &minute, &second);
	isecond = (RL_INT4) second;

	if (ndigits <= 0) {
		sprintf(string, "%04d-%02d-%02dT%02d:%02d:%02d",
			year, month, day, hour, minute, isecond);
	}
	else {
		sprintf(string, "%04d-%02d-%02dT%02d:%02d:%02d.%0*d",
			year, month, day, hour, minute, isecond,
			ndigits, ifrac);
	}

	if (useZ) (void) strcat(string, "Z");
}

/*<A NAME="Jul_FormatSQL"></A>
********************************************************************************
*$ Component_name:
*	Jul_FormatSQL (format.c)
*$ Abstract:
*	This function formats a date and time in a format compatible with SQL.
*$ Keywords:
*	JULIAN, TIME, FORMATTING
*	C, PUBLIC
*$ Declarations:
*	void		Jul_FormatSQL(dutc, secs, string)
*	RL_INT4		dutc;
*	RL_FLT8		secs;
*	RL_CHAR		*string;
*$ Inputs:
*	dutc		day relative to January 1, 2000.
*	secs		number of seconds into day.
*$ Outputs:
*	*string		null-terminated character string containing formatted
*			date and time.
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function formats a date and time in a format compatible with SQL.
*$ External_references:
*	Jul_YMDodDUTC(), Jul_HMSofSec()
*$ Examples:
*	none
*$ Error_handling:
*	none
*$ Limitations:
*	The dimensioned length of the character string must be at least 25
*	characters.  This length is not checked at runtime.  SQL has no
*	representation for years BCE.
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
*******************************************************************************/

void	Jul_FormatSQL(dutc, secs, string)
RL_INT4	dutc;
RL_FLT8	secs;
RL_CHAR	*string;
{
RL_FLT8	second; 
RL_INT4	year, month, day, hour, minute, isecond, millisec;

	dutc += ZJul_RoundSecs(secs, Jul_IsLeapDay(dutc), 3,
			&isecond, &millisec);

	Jul_YMDofDUTC(dutc, &year, &month, &day);
	Jul_HMSofSec((RL_FLT8) isecond, &hour, &minute, &second);
	isecond = (RL_INT4) second;

	sprintf(string, "%-3.3s %d, %d %02d:%02d:%02d:%03d",
		mixed_months[month-1], day, year, hour, minute, isecond,
		millisec);
}

/*<A NAME="ZJul_RoundSecs"></A>
********************************************************************************
* INTERNAL FUNCTIONS
********************************************************************************
* RL_INT4 ZJul_RoundSecs(secs, isleap, ndigits, isecs, ifrac)
*
* This internal function determines the integer and fractional part of a number
* of seconds after rounding it to a specified number of fractional digits.
*
* Input:
*	secs		number of seconds.
*	isleap		TRUE if this day has a leap second; FALSE otherwise.
*	ndigits		number of digits after decimal point.
*
* Output:
*	*isecs		integer part of number of seconds, after rounding.
*	*ifrac		integer digits of fractional part of number, after
*			rounding.
*
* Return:		increment to apply to dutc if rounding crosses a day
*			boundary.
*******************************************************************************/

static RL_INT4 ZJul_RoundSecs(secs, isleap, ndigits, isecs, ifrac)
RL_FLT8	secs;
RL_BOOL	isleap;
RL_INT4	ndigits, *isecs, *ifrac;
{
RL_FLT8	scale;
RL_INT4	daysecs;

/* Round off seconds value */
	if (ndigits < 0) ndigits = 0;
	scale = pow(10., (RL_FLT8) ndigits);
	secs = floor(secs * scale + 0.5) / scale;

/* Save integer and fractional parts */
	*isecs = (RL_INT4) floor(secs);
	*ifrac = (RL_INT4) floor(scale * (secs - *isecs) + 0.5);

/* Check for roundoff that crosses a day boundary */
	daysecs = (isleap ? 86401:86400);
	if (*isecs >= daysecs) {
		*isecs -= daysecs;
		return 1;
	}

	return 0;
}

/*<A NAME="ZJul_ReplaceInt"></A>
********************************************************************************
* RL_BOOL ZJul_ReplaceInt(string, match, format, value)
*
* This internal function replaces a matched substring in a character string
* with an integer.
*
* Input:
*	*string		string to search.
*	*match		pattern to search string for.
*	*format		format to apply to integer (used in sprintf()).
*	value		integer value to write.
*
* Output:
*	*string		updated string.
*
* Return:		TRUE if the match string was found; FALSE otherwise.
*******************************************************************************/

static RL_BOOL ZJul_ReplaceInt(string, match, format, value)
RL_CHAR	*string, *match, *format;
RL_INT4	value;
{
RL_CHAR	*loc, *after, tempstr1[TEMPSTR_LEN+1], tempstr2[TEMPSTR_LEN+2];
RL_BOOL	change;

	loc = ZJul_FindMatch(string, match, &after);
	change = (loc != NULL);

	if (change) {
		sprintf(tempstr1, format, value);
		*loc = '\0';
		sprintf(tempstr2, "%s%s%s",
			 string, tempstr1, loc + strlen(match));
		strcpy(string, tempstr2);
	}

	return change;	
}

/*<A NAME="ZJul_ReplaceStr"></A>
********************************************************************************
* RL_BOOL ZJul_ReplaceStr(string, match, format, value)
*
* This internal function replaces a matched substring in a character string
* with another string.
*
* Input:
*	*string		string to search.
*	*match		pattern to search string for.
*	*format		format to apply to character (used in sprintf()).
*	*value		character string to write.
*
* Output:
*	*string		updated string.
*
* Return:		TRUE if the match string was found; FALSE otherwise.
*******************************************************************************/

static RL_BOOL ZJul_ReplaceStr(string, match, format, value)
RL_CHAR	*string, *match, *format, *value;
{
RL_CHAR	*loc, *after, tempstr1[TEMPSTR_LEN+1], tempstr2[TEMPSTR_LEN+2];
RL_BOOL	change;

	loc = ZJul_FindMatch(string, match, &after);
	change = (loc != NULL);

	if (change) {
		sprintf(tempstr1, format, value);
		*loc = '\0';
		sprintf(tempstr2, "%s%s%s", string, tempstr1, after);
		strcpy(string, tempstr2);
	}

	return change;	
}

/*******************************************************************************
* RL_CHAR *ZJul_FindMatch(string, match, after)
*
* This internal function finds a matching substring within a given string.  A
* match cannot be delimited by other alphabetic characters.
*
* Input:
*	*string		string to search.
*	*match		pattern to search string for.
*
* Output:
*	after		if a match is found, a pointer to the character in the
*			string following the match.
*
* Return:		a pointer to the first matching character, or NULL if no
*			match was found.
*******************************************************************************/

static RL_CHAR *ZJul_FindMatch(string, match, after)
RL_CHAR	*string, *match, **after;
{
RL_CHAR	*s;
RL_BOOL	was_alpha;
RL_INT4	i;

/* Test each possible starting point in string */
	was_alpha = FALSE;
	for (s = string; *s != '\0'; s++) {

/* If this is the escape character, skip it and also the next character */
		if (*s == ESCAPE && *(s+1) != '\0') {
			s++;
			was_alpha = FALSE;
			continue;
		}

/* If this character is not alphabetic, no match */
		if (!isalpha(*s)) {
			was_alpha = FALSE;
			continue;
		}

/* If the previous character was alphabetic, no match */
		if (was_alpha) continue;

/* Remember that this character is alphabetic */
		was_alpha = TRUE;

/* If this isn't the first character of the match string, no match */
		if (*s != *match) continue;

/* OK so far, so test remaining characters */
		for (i=1; match[i] != '\0'; i++) {
			if (s[i] != match[i]) break;
		}
		if (match[i] != '\0') continue;

/* Make sure the next character is not alphabetic */
		if (isalpha(s[i])) continue;

/* Otherwise we have a match! */
		*after = s + i;
		return s;
	}

	return NULL;
}

/*<A NAME="ZJul_RemoveEsc"></A>
********************************************************************************
* void ZJul_RemoveEsc(string)
*
* This internal function removes escape characters from a string.
*
* Input:
*	*string		string to modify.
*
* Output:
*	*string		modified string.
*******************************************************************************/

static void ZJul_RemoveEsc(string)
RL_CHAR	*string;
{
RL_CHAR	*new, *old;

	for (new=string, old=string; *old != '\0'; new++, old++) {
		if (*old == ESCAPE) {
			old++;
			if (*old == '\0') break;
		}

		*new = *old;
	}
	*new = '\0';
}

/*<A NAME="FJul_FormatDate"></A>
********************************************************************************
* FORTRAN INTERFACE FUNCTIONS
********************************************************************************
*$ Component_name:
*	FJul_FormatDate (format.c)
*$ Abstract:
*	Formats a date according to a format string.
*$ Keywords:
*	JULIAN, TIME, FORMATTING
*	FORTRAN, PUBLIC
*$ Declarations:
*	subroutine FJul_FormatDate(dutc, format, string)
*	integer*4	dutc
*	character*(*)	format, string
*$ Inputs:
*	dutc		day relative to January 1, 2000.
*	format		format string (see detailed description).
*$ Outputs:
*	string		character string containing formatted date.
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function formats a date according to a format string.  The format
*	string is copied into the output string except for the following
*	substitutions:
*		YYYY or yyyy	4-digit year number.
*		Y or y		year number.
*		MONTH		full month name (all upper-case).
*		Month		full month name (first letter upper-case).
*		month		full month name (all lower-case).
*		MON		3-letter month name (all upper-case).
*		Mon		3-letter month name (first letter upper-case).
*		mon		3-letter month name (all lower-case).
*		MM or mm	2-digit month number.
*		M or m		month number.
*		DD or dd	2-digit day number.
*		D or d		day number.
*	The above substitutions do not take place if the pattern appears as a
*	part of a longer character string.
*
*	An escape character "\" is removed from the string but prevents the
*	next character from being either modified or treated as a part of a
*	neighboring match pattern.  Use "\\" to insert the escape character into
*	the output string.
*$ External_references:
*	Jul_FormatDate(), FORT_Cstring(), FORT_Fstring()
*$ Examples:
*	call FJul_FormatDate(dutc, 'yyyy-mm-dd\T', string)
*		produces a string in PDS date format, including the final "T".
*	call FJul_FormatDate(dutc, 'Month d, y', string)
*		produces a more readable date like "July 4, 1776".
*$ Error_handling:
*	none
*$ Limitations:
*	The dimensioned length of the character string must be long enough to
*	accomodate the output string.  This length is not checked at runtime.
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
********************************************************************************
* Note: GJul_FormatDate() defined here is the intermediary routine between
* FJul_FormatDate() and Jul_FormatDate(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fjulian.for for the
* rest of the code.
*
* subroutine GJul_FormatDate(dutc, format, string)
* integer*4	dutc
* byte		format, string
*******************************************************************************/

void	FORTRAN_NAME(gjul_formatdate) (dutc, format, string)
RL_INT4	*dutc;
RL_CHAR	*format, *string;
{
	FORT_Init();

	Jul_FormatDate(*dutc, format, string);
}

/*<A NAME="FJul_FormatTime"></A>
********************************************************************************
*$ Component_name:
*	FJul_FormatTime (format.c)
*$ Abstract:
*	Formats a time according to a format string.
*$ Keywords:
*	JULIAN, TIME, FORMATTING
*	FORTRAN, PUBLIC
*$ Declarations:
*	integer*4 function FJul_FormatTime(secs, isleap, format, string)
*	real*8		secs
*	logical*4	isleap
*	character*(*)	format, string
*$ Inputs:
*	secs		number of seconds into day.
*	isleap		.TRUE. if this day has a leap second; .FALSE. otherwise.
*	format		format string (see detailed description).
*$ Outputs:
*	string		character string containing formatted time.
*$ Returns:
*	1 if rounding error shifts the time to the following day; 0 otherwise.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function formats a date according to a format string.  The format
*	string is copied into the output string except for the following
*	substitutions:
*		AM or PM	upper-case AM or PM, depending on time.
*		am or pm	lower-case am or pm, depending on time.
*		HH or hh	2-digit hour.
*		H or h		hour.
*		MM or mm	2-digit minute.
*		M or m		minute.
*		SS or ss	2-digit second.
*		S or s		second.
*		ZZ... or zz...	Any sequence is replaced by an equal number of
*				fractional seconds digits.
*	The above substitutions do not take place if the pattern appears as a
*	part of a longer character string.
*
*	An escape character "\" is removed from the string but prevents the
*	next character from being either modified or treated as a part of a
*	neighboring match pattern.  Use "\\" to insert the escape character into
*	the output string.
*$ External_references:
*	Jul_FormatTime(), FORT_Cstring(), FORT_Fstring()
*$ Examples:
*	This fragment of code formats a date and time.  Note that it uses the
*	returned date offset when formatting the time; the reason is that a time
*	can round up to midnight, which corresponds to the following day.
*
*	integer*4	dutc, delta
*	real*8		secs
*	character*80	time_string, date_string
*
*	delta = FJul_FormatTime(secs, FJul_IsLeapDay(dutc), 'hh:mm:ss.zzz',
*    &			time_string)
*	call FJul_FormatDate(dutc + delta, 'yyyy-mm-dd', date_string)
*
*$ Error_handling:
*	none
*$ Limitations:
*	The dimensioned length of the character string must be long enough to
*	accomodate the output string.  This length is not checked at runtime.
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
********************************************************************************
* Note: GJul_FormatTime() defined here is the intermediary routine between
* FJul_FormatTime() and Jul_FormatTime(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fjulian.for for the
* rest of the code.
*
* subroutine GJul_FormatTime(secs, isleap, format, string)
* real*8	secs
* logical*4	isleap
* byte		format, string
*******************************************************************************/

RL_INT4	FORTRAN_NAME(gjul_formattime) (secs, isleap, format, string)
RL_FLT8	*secs;
RL_INT4	*isleap;
RL_CHAR	*format, *string;
{
	FORT_Init();

	return Jul_FormatTime(*secs, (RL_BOOL) *isleap, format, string);
}

/*<A NAME="FJul_FormatPDS"></A>
********************************************************************************
*$ Component_name:
*	FJul_FormatPDS (format.c)
*$ Abstract:
*	This function formats a date and time in PDS format.
*$ Keywords:
*	JULIAN, TIME, FORMATTING
*	FORTRAN, PUBLIC
*$ Declarations:
*	subroutine FJul_FormatPDS(dutc, secs, ndigits, useZ, string)
*	integer*4	dutc, nfrac
*	real*8		secs
*	logical*4	useZ
*	character*(*)	string
*$ Inputs:
*	dutc		day relative to January 1, 2000.
*	secs		number of seconds into day.
*	ndigits		number of fractional digits to use in second field.
*	useZ		.TRUE. to append a "Z" character; .FALSE. to leave it
*			out.
*$ Outputs:
*	string		character string containing formatted date and time.
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function formats a date and time in PDS format.
*$ External_references:
*	Jul_FormatPDS(), FORT_Fstring()
*$ Examples:
*	none
*$ Error_handling:
*	none
*$ Limitations:
*	The dimensioned length of the character string must be at least
*	(20+ndigits) characters.  This length is not checked at runtime.  PDS
*	format has no representation for years BCE.
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
********************************************************************************
* Note: GJul_FormatPDS() defined here is the intermediary routine between
* FJul_FormatPDS() and Jul_FormatPDS(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fjulian.for for the
* rest of the code.
*
* subroutine GJul_FormatPDS(dutc, secs, ndigits, useZ, string)
* integer*4	dutc, ndigits
* real*8	secs
* logical*4	useZ
* byte		string
*******************************************************************************/

void	FORTRAN_NAME(gjul_formatpds) (dutc, secs, ndigits, useZ, string)
RL_INT4	*dutc, *ndigits, *useZ;
RL_FLT8	*secs;
RL_CHAR	*string;
{
	FORT_Init();

	Jul_FormatPDS(*dutc, *secs, *ndigits, (RL_BOOL) *useZ, string);
}

/*<A NAME="FJul_FormatSQL"></A>
********************************************************************************
*$ Component_name:
*	FJul_FormatSQL (format.c)
*$ Abstract:
*	This function formats a date and time in a format compatible with SQL.
*$ Keywords:
*	JULIAN, TIME, FORMATTING
*	FORMAT, PUBLIC
*$ Declarations:
*	subroutine FJul_FormatSQL(dutc, secs, string)
*	integer*4	dutc
*	real*8		secs
*	character*(*)	string
*$ Inputs:
*	dutc		day relative to January 1, 2000.
*	secs		number of seconds into day.
*$ Outputs:
*	string		character string containing formatted date and time.
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function formats a date and time in a format compatible with SQL.
*$ External_references:
*	Jul_FormatSQL(), FORT_Fstring()
*$ Examples:
*	none
*$ Error_handling:
*	none
*$ Limitations:
*	The dimensioned length of the character string must be at least 24
*	characters.  This length is not checked at runtime.  SQL has no
*	representation for years BCE.
*$ Author_and_institution:
*	Mark R. Showalter
*	PDS Rings Node
*	NASA/Ames Research Center
*$ Version_and_date:
*	1.0: November 1995
*$ Change_history:
*	none
********************************************************************************
* Note: GJul_FormatSQL() defined here is the intermediary routine between
* FJul_FormatSQL() and Jul_FormatSQL(), allowing for the fact that strings
* cannot be passed directly between FORTRAN and C.  See fjulian.for for the
* rest of the code.
*
* subroutine GJul_FormatSQL(dutc, secs, string)
* integer*4	dutc
* real*8	secs
* byte		string
*******************************************************************************/

void	FORTRAN_NAME(gjul_formatsql) (dutc, secs, string)
RL_INT4	*dutc;
RL_FLT8	*secs;
RL_CHAR	*string;
{
	FORT_Init();

	Jul_FormatSQL(*dutc, *secs, string);	
}

/*******************************************************************************
</PRE></BODY></HTML>*/
