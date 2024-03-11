/*<HTML><HEAD><TITLE> dates.c </TITLE></HEAD><BODY><PRE>
********************************************************************************
* dates.c
*
* This set of functions handles conversions between calendar dates.  Days are
* given as the number of elapsed days since January 1, 2000 UTC (referred to as
* J2000 below), with negative numbers referring to earlier dates.
*
* RL_INT4 Jul_DUTCofYMD(year, month, day)
*		converts from year, month, day to days since J2000.
* void    Jul_YMDofDUTC(dutc, year, month, day)
*		converts from days since J2000 to year, month and day.
* void    Jul_YDofDUTC(dutc, year, day)
*		converts from days since J2000 to year and day-of-year.
* void    Jul_Gregorian(year, month, day)
*		sets the date at which the Gregorian calendar was adopted.
* RL_BOOL Jul_IsLeapYear(year)
*		determines whether the given year is a leap year.
* RL_INT4 Jul_YearDays(year)
*		returns the number of days in the given year.
* RL_INT4 Jul_MonthDays(year, month)
*		returns the number of days in the given month.
* RL_INT4 Jul_DUTCofJDN(jdn)
*		converts from Julian Date to days since J2000.
* RL_INT4 Jul_JDNofDUTC(dutc)
*		converts from days since J2000 to Julian Date.
*
* Mark R. Showalter, PDS Rings Node, December 1995
* Revised by MRS 6/98 to conform to RingLib naming conventions.
*******************************************************************************/
#include <stdio.h>	/* just to define TRUE and FALSE */
#include "julian.h"
#include "fortran.h"

/***********************************************************
* Definitions
***********************************************************/

#define  JD_OF_J2000	2451545		    /* Julian Date of noon, J2000 */

/***********************************************************
* Static variables
***********************************************************/

static RL_INT4 gregorian_dutc  = -152384;   /* Default is October 15, 1582 */
static RL_INT4 gregorian_year  = 1582;
static RL_INT4 gregorian_month = 10;
static RL_INT4 gregorian_day   = 15;

/***********************************************************
* Prototypes of internal functions
***********************************************************/

static RL_INT4 Jul_JDNofGregYMD RL_PROTO((RL_INT4 year, RL_INT4 month,
                                          RL_INT4 day));
static void    Jul_GregYMDofJDN RL_PROTO((RL_INT4 jdn, RL_INT4 *year,
                                          RL_INT4 *month, RL_INT4 *day));
static RL_INT4 Jul_JDNofJulYMD  RL_PROTO((RL_INT4 year, RL_INT4 month,
                                          RL_INT4 day));
static void    Jul_JulYMDofJDN  RL_PROTO((RL_INT4 jdn, RL_INT4 *year,
                                          RL_INT4 *month, RL_INT4 *day));
static void    Jul_FixYM        RL_PROTO((RL_INT4 *year, RL_INT4 *month));

/*<A NAME="Jul_DUTCofYMD"></A>
********************************************************************************
* EXPORTED USER ROUTINES
********************************************************************************
*$ Component_name:
*	Jul_DUTCofYMD (dates.c)
*$ Abstract:
*	Returns the number of days relative to January 1, 2000 for a given date.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	C, PUBLIC
*$ Declarations:
*	RL_INT4		Jul_DUTCofYMD(year, month, day)
*	RL_INT4		year, month, day;
*$ Inputs:
*	year		year value.
*	month		month (typically but not necessarily 1-12).
*	day		day (typically but not necessarily 1-31).
*$ Outputs:
*	none
*$ Returns:
*	number of days elapsed since January 1, 2000.  Negative values refer to
*	earlier dates.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function returns the number of days relative to January 1, 2000
*	for a given date.
*$ External_references:
*	none
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

RL_INT4	Jul_DUTCofYMD(year, month, day)
RL_INT4	year, month, day;
{
RL_BOOL	isgreg;

/* Correct year & month ranges */
	Jul_FixYM(&year, &month);

/* Determine which calendar to use */
	if	  (year > gregorian_year)   isgreg = TRUE;
	else if   (year < gregorian_year)   isgreg = FALSE;
	else {
	  if      (month > gregorian_month) isgreg = TRUE;
	  else if (month < gregorian_month) isgreg = FALSE;
	  else                              isgreg = (day >= gregorian_day);
	}

/* Calculate and return date */
	if (isgreg) return Jul_JDNofGregYMD(year, month, day) - JD_OF_J2000;
	else        return Jul_JDNofJulYMD (year, month, day) - JD_OF_J2000;
}

/*<A NAME="Jul_YMDofDUTC"></A>
********************************************************************************
*$ Component_name:
*	Jul_YMDofDUTC (dates.c)
*$ Abstract:
*	Returns the calendar year, month and day for a day number relative to
*	January 1, 2000 for a given date.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	C, PUBLIC
*$ Declarations:
*	void		Jul_YMDofDUTC(dutc, year, month, day)
*	RL_INT4		dutc, *year, *month, *day;
*$ Inputs:
*	dutc		number of days relative to January 1, 2000.

*$ Outputs:
*	year		year value.
*	month		month (1-12).
*	day		day (1-31).
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function returns the calendar year, month and day for a day number
*	relative to January 1, 2000.
*$ External_references:
*	none
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

void	Jul_YMDofDUTC(dutc, year, month, day)
RL_INT4	dutc, *year, *month, *day;
{
	if (dutc >= gregorian_dutc)
		Jul_GregYMDofJDN(dutc + JD_OF_J2000, year, month, day);
	else
		Jul_JulYMDofJDN (dutc + JD_OF_J2000, year, month, day);
}

/*<A NAME="Jul_YDofDUTC"></A>
********************************************************************************
*$ Component_name:
*	Jul_YDofDUTC (dates.c)
*$ Abstract:
*	Returns the calendar year and day-of-year number for a day number
*	relative to January 1, 2000 for a given date.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	C, PUBLIC
*$ Declarations:
*	void		Jul_YDofDUTC(dutc, year, doy)
*	RL_INT4		dutc, *year, *doy;
*$ Inputs:
*	dutc		number of days relative to January 1, 2000.
*$ Outputs:
*	year		year value.
*	doy		day-of-year (1-366).
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function returns the calendar year and day-of-year for a day number
*	relative to January 1, 2000.
*$ External_references:
*	Jul_YMDofDUTC(), Jul_DUTCofYMD()
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

void	Jul_YDofDUTC(dutc, year, doy)
RL_INT4	dutc, *year, *doy;
{
RL_INT4	month, day;

	Jul_YMDofDUTC(dutc, year, &month, &day);
	*doy = dutc - Jul_DUTCofYMD(*year, 1, 1) + 1;
}

/*<A NAME="Jul_Gregorian"></A>
********************************************************************************
*$ Component_name:
*	Jul_Gregorian (dates.c)
*$ Abstract:
*	Sets the date at which the Gregorian calendar was adopted.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	C, PUBLIC
*$ Declarations:
*	void		Jul_Gregorian(year, month, day)
*	RL_INT4		year, month, day;
*$ Inputs:
*	year		year value.
*	month		month value (typically but not necessarily 1-12).
*	day		day value (typically but not necessarily 1-31).
*
*	Note that the year, month and day are given according to the Gregorian
*	calendar.
*$ Outputs:
*	none
*$ Returns:
*	none
*$ Side_effects:
*	Saves the Gregorian calendar start date in internal variables.  It
*	remains in effect until the next call to Jul_Gregorian().
*$ Detailed_description:
*	This function sets the date at which the Gregorian calendar was adopted.
*	The default value is October 15, 1582; however, the user may wish to
*	change this value because some countries adopted the Gregorian calendar
*	on later dates.  It affects the behavior of Jul_DUTCofYMD(),
*	Jul_YMDofDUTC(), Jul_YDofDUTC(), etc.
*$ External_references:
*	none
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

void	Jul_Gregorian(year, month, day)
RL_INT4	year, month, day;
{
RL_INT4	jd;

/* Correct year & month ranges */
	Jul_FixYM(&year, &month);

/* Calculate Julian date */
	jd = Jul_JDNofGregYMD(year, month, day);

/* Save static variables */
	gregorian_dutc = jd - JD_OF_J2000;
	Jul_GregYMDofJDN(jd, &gregorian_year, &gregorian_month, &gregorian_day);
}

/*<A NAME="Jul_IsLeapYear"></A>
********************************************************************************
*$ Component_name:
*	Jul_IsLeapYear (dates.c)
*$ Abstract:
*	Determines whether the given year is a leap year.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	C, PUBLIC
*$ Declarations:
*	RL_BOOL		Jul_IsLeapYear(year)
*	RL_INT4		year;
*$ Inputs:
*	year		year value.
*$ Outputs:
*	none
*$ Returns:
*	TRUE if it is a leap year; FALSE if it is not.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function determines whether the given year is a leap year.  It
*	returns TRUE if it is a leap year or FALSE if it is not.
*$ External_references:
*	Jul_DUTCofYMD()
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

RL_BOOL	Jul_IsLeapYear(year)
RL_INT4	year;
{
	return (Jul_DUTCofYMD(year,3,1) != Jul_DUTCofYMD(year,2,29));
}

/*<A NAME="Jul_YearDays"></A>
********************************************************************************
*$ Component_name:
*	Jul_YearDays (dates.c)
*$ Abstract:
*	Returns the number of days in a given year.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	C, PUBLIC
*$ Declarations:
*	RL_INT4		Jul_YearDays(year)
*	RL_INT4		year;
*$ Inputs:
*	year		year value.
*$ Outputs:
*	none
*$ Returns:
*	number of days in the year.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function returns the number of days in a given year.
*$ External_references:
*	Jul_DUTCofYMD()
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

RL_INT4	Jul_YearDays(year)
RL_INT4	year;
{
	return Jul_DUTCofYMD(year,12,31) - Jul_DUTCofYMD(year,1,1) + 1;
}

/*<A NAME="Jul_MonthDays"></A>
********************************************************************************
*$ Component_name:
*	Jul_MonthDays (dates.c)
*$ Abstract:
*	Returns the number of days in a given month.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	C, PUBLIC
*$ Declarations:
*	RL_INT4		Jul_YearDays(year)
*	RL_INT4		year;
*$ Inputs:
*	year		year value.
*	month		month value (1-12).
*$ Outputs:
*	none
*$ Returns:
*	number of days in the month.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function returns the number of days in a given month.
*$ External_references:
*	Jul_DUTCofYMD()
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

RL_INT4	Jul_MonthDays(year, month)
RL_INT4	year, month;
{
	return Jul_DUTCofYMD(year,month+1,1) - Jul_DUTCofYMD(year,month,1);
}

/*<A NAME="Jul_DUTCofJDN"></A>
********************************************************************************
*$ Component_name:
*	Jul_DUTCofJDN (dates.c)
*$ Abstract:
*	Converts from Julian Date to days relative to January 1, 2000.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	C, PUBLIC
*$ Declarations:
*	RL_INT4		Jul_DUTCofJDN(jd)
*	RL_INT4		jd;
*$ Inputs:
*	jd		Julian Date.
*$ Outputs:
*	none
*$ Returns:
*	day relative to January 1, 2000 on which the given Julian date begins
*	at noon.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts from Julian Date to days relative to January 1,
*	2000.  The Julian date returned begins at noon on the specified date.
*$ External_references:
*	none
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

RL_INT4	Jul_DUTCofJDN(jd)
RL_INT4	jd;
{
	return jd - JD_OF_J2000;
}

/*<A NAME="Jul_JDNofDUTC"></A>
********************************************************************************
*$ Component_name:
*	Jul_JDNofDUTC (dates.c)
*$ Abstract:
*	Converts from day relative to January 1, 2000 to Julian Date.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	C, PUBLIC
*$ Declarations:
*	RL_INT4		Jul_JDNofDUTC(dutc)
*	RL_INT4		dutc;
*$ Inputs:
*	dutc		days relative to January 1, 2000.
*$ Outputs:
*	none
*$ Returns:
*	Julian date at noon of the given day.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts from day relative to January 1, 2000 to Julian
*	Date.  The Julian Date returned is the one beginning at noon on the
*	given day.
*$ External_references:
*	none
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

RL_INT4	Jul_JDNofDUTC(dutc)
RL_INT4	dutc;
{
	return dutc + JD_OF_J2000;
}

/*<A NAME="Jul_JDNofGregYMD"></A>
********************************************************************************
* INTERNAL FUNCTIONS
********************************************************************************
* RL_INT4 Jul_JDNofGregYMD(RL_INT4 year, RL_INT4 month, RL_INT4 day)
*
* This internal function returns the Julian Date that begins on noon of a given
* date in the Gregorian calendar.  The algorithm is from p. 604 of
*	Seidelman, P. K. 1992.  Explanatory Supplement to the Astronomical
*	Almanac.  University Science Books, Mill Valley.
*
* Input:
*	year		year value.
*	month		month value (1-12).
*	dutc		day value (1-31).
*
* Return:		Julian Date value.
*******************************************************************************/

static RL_INT4 Jul_JDNofGregYMD(year, month, day)
RL_INT4	year, month, day;
{
RL_INT4	temp;

/*	temp = (month - 14)/12; */
	temp = (month <= 2 ? -1:0);

	return   (1461*(year + 4800 + temp))/4
	       + (367*(month - 2 - 12*temp))/12
	       - (3*((year + 4900 + temp)/100))/4 + day - 32075;
}

/*<A NAME="Jul_GregYMDofJDN"></A>
********************************************************************************
* void Jul_GregYMDofJDN(RL_INT4 jd, RL_INT4 *year, RL_INT4 *month, RL_INT4 *day)
*
* This internal function calculates the year, month and day in the Gregorian
* calendar which a given Julian Date begins (at noon).  The algorithm is from
* p. 604 of
*	Seidelman, P. K. 1992.  Explanatory Supplement to the Astronomical
*	Almanac.  University Science Books, Mill Valley.
*
* Input:
*	jd		Julian Date value.
*
* Output:
*	*year		year value.
*	*month		month value (1-12).
*	*day		day value (1-31).
*******************************************************************************/

static void Jul_GregYMDofJDN(jd, year, month, day)
RL_INT4	jd, *year, *month, *day;
{
RL_INT4	l, n, i, j, d, m, y;

	l = jd + 68569;
	n = (4*l) / 146097;
	l = l - (146097*n + 3)/4;
	i = (4000*(l+1))/1461001;
	l = l - (1461*i)/4 + 31;
	j = (80*l)/2447;
	d = l - (2447*j)/80;
	l = j/11;
	m = j + 2 - 12*l;
	y = 100*(n-49) + i + l;

	*year  = y;
	*month = m;
	*day   = d;
}

/*<A NAME="Jul_JDNofJulYMD"></A>
********************************************************************************
* RL_INT4 Jul_JDNofJulYMD(RL_INT4 year, RL_INT4 month, RL_INT4 day)
*
* This internal function returns the Julian Date that begins on noon of a given
* date in the Julian calendar.  The algorithm is from p. 606 of
*	Seidelman, P. K. 1992.  Explanatory Supplement to the Astronomical
*	Almanac.  University Science Books, Mill Valley.
*
* Input:
*	year		year value.
*	month		month value (1-12).
*	day		day value (1-31).
*
* Return:		Julian Date value.
*******************************************************************************/

static RL_INT4 Jul_JDNofJulYMD(year, month, day)
RL_INT4	year, month, day;
{
	return   367*year
	       - (7*(year + 5001 + (month-9)/7))/4
	       + (275*month)/9
	       + day + 1729777;
}

/*<A NAME="Jul_YMDofJDN"></A>
********************************************************************************
* void Jul_JulYMDofJDN(RL_INT4 jd, RL_INT4 *year, RL_INT4 *month, RL_INT4 *day)
*
* This internal function calculates the year, month and day in the Julian
* calendar on which a given Julian Date begins (at noon).  The algorithm is from
* p. 606 of
*	Seidelman, P. K. 1992.  Explanatory Supplement to the Astronomical
*	Almanac.  University Science Books, Mill Valley.
*
* Input:
*	jd		Julian Date value.
*
* Output:
*	*year		year value.
*	*month		month value (1-12).
*	*day		day fvalue (1-31).
*******************************************************************************/

static void Jul_JulYMDofJDN(jd, year, month, day)
RL_INT4	jd, *year, *month, *day;
{
RL_INT4	j, k, l, n, d, i, m, y;

	j = jd + 1402;
	k = (j-1)/1461;
	l = j - 1461*k;
	n = (l-1)/365 - l/1461;
	i = l - 365*n + 30;
	j = (80*i)/2447;
	d = i - (2447*j)/80;
	i = j/11;
	m = j + 2 - 12*i;
	y = 4*k + n + i - 4716;

	*year  = y;
	*month = m;
	*day   = d;
}

/*<A NAME="Jul_FixYM"></A>
********************************************************************************
* void Jul_FixYM(RL_INT4 *year, RL_INT4 *month)
*
* This internal function shifts year and month values into the proper range.
* The month value will be between 1 and 12.
*
* Input:
*	*year, *month	current year and month values.
*
* Output:
*	*year, *month	corrected year and month values.
*******************************************************************************/

static void Jul_FixYM(year, month)
RL_INT4	*year, *month;
{
RL_INT4	y, m, dyear;

	y = *year;
	m = *month;

/* Shift month into range 1-12 */
	dyear = (m-1) / 12;	
	m -= 12 * dyear;
	y += dyear;

	if (m < 1) {
		m += 12;
		y -= 1;
	}

	*year  = y;
	*month = m;
}

/*<A NAME="FJul_DUTCofYMD"></A>
********************************************************************************
* FORTRAN INTERFACE ROUTINES
********************************************************************************
*$ Component_name:
*	FJul_DUTCofYMD (dates.c)
*$ Abstract:
*	Returns the number of days relative to January 1, 2000 for a given date.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	FORTRAN, PUBLIC
*$ Declarations:
*	integer*4 function FJul_DUTCofYMD(year, month, day)
*	integer*4	year, month, day
*$ Inputs:
*	year		year value.
*	month		month (typically but not necessarily 1-12).
*	day		day (typically but not necessarily 1-31).
*$ Outputs:
*	none
*$ Returns:
*	number of days elapsed since January 1, 2000.  Negative values refer to
*	earlier dates.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function returns the number of days relative to January 1, 2000
*	for a given date.
*$ External_references:
*	Jul_DUTCofYMD(), FORT_Init()
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

RL_INT4	FORTRAN_NAME(fjul_dutcofymd) (year, month, day)
RL_INT4	*year, *month, *day;
{
	FORT_Init();

	return Jul_DUTCofYMD(*year, *month, *day);
}

/*<A NAME="FJul_YMDofDUTC"></A>
********************************************************************************
*$ Component_name:
*	FJul_YMDofDUTC (dates.c)
*$ Abstract:
*	Returns the calendar year, month and day for a day number relative to
*	January 1, 2000 for a given date.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	FORTRAN, PUBLIC
*$ Declarations:
*	subroutine FJul_YMDofDUTC(dutc, year, month, day)
*	integer*4	dutc, year, month, day
*$ Inputs:
*	dutc		number of days relative to January 1, 2000.

*$ Outputs:
*	year		year value.
*	month		month (1-12).
*	day		day (1-31).
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function returns the calendar year, month and day for a day number
*	relative to January 1, 2000.
*$ External_references:
*	Jul_YMDofDUTC(), FORT_Init()
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

void	FORTRAN_NAME(fjul_ymdofdutc) (dutc, year, month, day)
RL_INT4	*dutc, *year, *month, *day;
{
	FORT_Init();

	Jul_YMDofDUTC(*dutc, year, month, day);
}

/*<A NAME="FJul_YDofDUTC"></A>
********************************************************************************
*$ Component_name:
*	FJul_YDofDUTC (dates.c)
*$ Abstract:
*	Returns the calendar year and day-of-year number for a day number
*	relative to January 1, 2000 for a given date.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	FORTRAN, PUBLIC
*$ Declarations:
*	subroutine FJul_YDofDUTC(dutc, year, doy)
*	integer*4	dutc, year, doy
*$ Inputs:
*	dutc		number of days relative to January 1, 2000.
*$ Outputs:
*	year		year value.
*	doy		day-of-year (1-366).
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function returns the calendar year and day-of-year for a day number
*	relative to January 1, 2000.
*$ External_references:
*	Jul_YDofDUTC(), FORT_Init()
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

void	FORTRAN_NAME(fjul_ydofdutc) (dutc, year, day)
RL_INT4	*dutc, *year, *day;
{
	FORT_Init();

	Jul_YDofDUTC(*dutc, year, day);
}

/*<A NAME="FJul_Gregorian"></A>
********************************************************************************
*$ Component_name:
*	FJul_Gregorian (dates.c)
*$ Abstract:
*	Sets the date at which the Gregorian calendar was adopted.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	FORTRAN, PUBLIC
*$ Declarations:
*	subroutine FJul_Gregorian(year, month, day)
*	integer*4	year, month, day
*$ Inputs:
*	year		year value.
*	month		month value (typically but not necessarily 1-12).
*	day		day value (typically but not necessarily 1-31).
*
*	Note that the year, month and day are given according to the Gregorian
*	calendar.
*$ Outputs:
*	none
*$ Returns:
*	none
*$ Side_effects:
*	Saves the Gregorian calendar start date in internal variables.  It
*	remains in effect until the next call to Jul_Gregorian().
*$ Detailed_description:
*	This function sets the date at which the Gregorian calendar was adopted.
*	The default value is October 15, 1582; however, the user may wish to
*	change this value because some countries adopted the Gregorian calendar
*	on later dates.  It affects the behavior of FJul_DUTCofYMD(),
*	FJul_YMDofDUTC(), FJul_YDofDUTC(), etc.
*$ External_references:
*	Jul_Gregorian(), FORT_Init()
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

void	FORTRAN_NAME(fjul_gregorian) (year, month, day)
RL_INT4	*year, *month, *day;
{
	FORT_Init();

	Jul_Gregorian(*year, *month, *day);
}

/*<A NAME="FJul_IsLeapYear"></A>
********************************************************************************
*$ Component_name:
*	FJul_IsLeapYear (dates.c)
*$ Abstract:
*	Determines whether the given year is a leap year.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	FORTRAN, PUBLIC
*$ Declarations:
*	logical*4 function FJul_IsLeapYear(year)
*	integer*4	year
*$ Inputs:
*	year		year value.
*$ Outputs:
*	none
*$ Returns:
*	.TRUE. if it is a leap year; .FALSE. if it is not.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function determines whether the given year is a leap year.  It
*	returns .TRUE. if it is a leap year or .FALSE. if it is not.
*$ External_references:
*	Jul_IsLeapYear(), FORT_Init()
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

RL_BOOL	FORTRAN_NAME(fjul_isleapyear) (year)
RL_INT4	*year;
{
	FORT_Init();

	return (Jul_IsLeapYear(*year) ? FTRUE:FFALSE);
}

/*<A NAME="FJul_YearDays"></A>
********************************************************************************
*$ Component_name:
*	FJul_YearDays (dates.c)
*$ Abstract:
*	Returns the number of days in a given year.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	FORTRAN, PUBLIC
*$ Declarations:
*	integer*4 function FJul_YearDays(year)
*	integer*4	year
*$ Inputs:
*	year		year value.
*$ Outputs:
*	none
*$ Returns:
*	number of days in the year.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function returns the number of days in a given year.
*$ External_references:
*	Jul_YearDays(), FORT_Init()
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

RL_INT4	FORTRAN_NAME(fjul_yeardays) (year)
RL_INT4	*year;
{
	FORT_Init();

	return Jul_YearDays(*year);
}

/*<A NAME="FJul_MonthDays"></A>
********************************************************************************
*$ Component_name:
*	FJul_MonthDays (dates.c)
*$ Abstract:
*	Returns the number of days in a given month.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	FORTRAN, PUBLIC
*$ Declarations:
*	integer*4 function FJul_YearDays(year)
*	integer*4	year
*$ Inputs:
*	year		year value.
*	month		month value (1-12).
*$ Outputs:
*	none
*$ Returns:
*	number of days in the month.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function returns the number of days in a given month.
*$ External_references:
*	Jul_MonthDays(), FORT_Init()
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

RL_INT4	FORTRAN_NAME(fjul_monthdays) (year, month)
RL_INT4	*year, *month;
{
	FORT_Init();

	return Jul_MonthDays(*year, *month);
}

/*<A NAME="FJul_DUTCofJDN"></A>
********************************************************************************
*$ Component_name:
*	FJul_DUTCofJDN (dates.c)
*$ Abstract:
*	Converts from Julian Date to days relative to January 1, 2000.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	FORTRAN, PUBLIC
*$ Declarations:
*	integer*4 function FJul_DUTCofJDN(jd)
*	integer*4	jd
*$ Inputs:
*	jd		Julian Date.
*$ Outputs:
*	none
*$ Returns:
*	day relative to January 1, 2000 on which the given Julian date begins
*	at noon.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts from Julian Date to days relative to January 1,
*	2000.  The Julian date returned begins at noon on the specified date.
*$ External_references:
*	none
*$ Examples:
*	Jul_DUTCofJDN(), FORT_Init()
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

RL_INT4	FORTRAN_NAME(fjul_dutcofjdn) (jdn)
RL_INT4	*jdn;
{
	FORT_Init();

	return Jul_DUTCofJDN(*jdn);
}

/*<A NAME="FJul_JDNofDUTC"></A>
********************************************************************************
*$ Component_name:
*	FJul_JDNofDUTC (dates.c)
*$ Abstract:
*	Converts from day relative to January 1, 2000 to Julian Date.
*$ Keywords:
*	JULIAN, TIME, CALENDAR
*	FORTRAN, PUBLIC
*$ Declarations:
*	integer*4 function FJul_JDNofDUTC(dutc)
*	integer*4	dutc
*$ Inputs:
*	dutc		days relative to January 1, 2000.
*$ Outputs:
*	none
*$ Returns:
*	Julian date at noon of the given day.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts from day relative to January 1, 2000 to Julian
*	Date.  The Julian Date returned is the one beginning at noon on the
*	given day.
*$ External_references:
*	Jul_JDNofDUTC(), FORT_Init()
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

RL_INT4	FORTRAN_NAME(fjul_jdnofdutc) (dutc)
RL_INT4	*dutc;
{
	FORT_Init();

	return Jul_JDNofDUTC(*dutc);
}

/*******************************************************************************
</PRE></BODY></HTML>*/
