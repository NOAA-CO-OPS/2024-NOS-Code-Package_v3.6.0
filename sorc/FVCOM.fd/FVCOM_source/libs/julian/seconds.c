/*<HTML><HEAD><TITLE> seconds.c </TITLE></HEAD><BODY><PRE>
********************************************************************************
* seconds.c
*
* This family of Julian routines converts between numbers of seconds and days,
* hours, minutes and seconds, with and without possible leap seconds.
*
* RL_FLT8 Jul_SecOfDHMS(day, hour, minute, second)
*		converts from days, hours, minutes and seconds to seconds.
* void   Jul_DSofSec(secs, day, second)
*		converts from seconds to days and seconds.
* void   Jul_DHMSofSec(secs, day, hour, minute, second)
*		converts from seconds to days, hours, minutes and seconds.
* void   Jul_HMSofSec(secs, hour, minute, second)
*		converts from seconds to hour, minutes and seconds, including
*		possible leap seconds.
*
* Mark R. Showalter, PDS Rings Node, November 1995
* Revised 6/98 by MRS to conform to RingLib naming standards.
*******************************************************************************/
#include <math.h>
#include "julian.h"
#include "fortran.h"

/*<A NAME="Jul_SecofDHMS"></A>
********************************************************************************
* EXPORTED USER ROUTINES
********************************************************************************
*$ Component_name:
*	Jul_SecofDHMS (seconds.c)
*$ Abstract:
*	Converts days, hours, minutes and seconds to a number of seconds.
*$ Keywords:
*	JULIAN, TIME, SECONDS
*	C, PUBLIC
*$ Declarations:
*	RL_FLT8		Jul_SecofDHMS(day, hour, minute, second)
*	RL_INT4		day, hour, minute;
*	RL_FLT8		second;
*$ Inputs:
*	day		number of days.
*	hour		number of hours (typically 0-23).
*	minute		number of minutes (typically 0-59).
*	second		floating-point number of seconds (typically 0-60).
*$ Outputs:
*	none
*$ Returns:
*	total number of seconds.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts days, hours, minutes and seconds to a number
*	of seconds.  It assumes no leap seconds are involved.
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

RL_FLT8	Jul_SecofDHMS(day, hour, minute, second)
RL_INT4	day, hour, minute;
RL_FLT8	second;
{
	return second + 60.*(minute + 60.*(hour + 24.*day));
}

/*<A NAME="Jul_DSofSec"></A>
********************************************************************************
*$ Component_name:
*	Jul_DSofSec (seconds.c)
*$ Abstract:
*	Converts a number of seconds to days, plus seconds into day.
*$ Keywords:
*	JULIAN, TIME, SECONDS
*	C, PUBLIC
*$ Declarations:
*	void		Jul_DSofSec(secs, day, second)
*	RL_FLT8		secs, *second;
*	RL_INT4		*day;
*$ Inputs:
*	secs		number of seconds.
*$ Outputs:
*	*day		number of days.
*	*second		seconds into day (0. <= second < 86400).
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a number of seconds to days, plus seconds into
*	day. It assumes no leap seconds are involved.
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

void	Jul_DSofSec(secs, day, second)
RL_FLT8	secs, *second;
RL_INT4	*day;
{
RL_FLT8	d, s;

	d = floor(secs / 86400.);
	s = secs - 86400. * d;

	if (s < 0.) {
		d -= 1.;
		s += 86400.;
	}

	*day    = d;
	*second = s;
}

/*<A NAME="Jul_DHMSofSec"></A>
********************************************************************************
*$ Component_name:
*	Jul_DHMSofSec (seconds.c)
*$ Abstract:
*	Converts a number of seconds to days, hours, minutes and seconds.
*$ Keywords:
*	JULIAN, TIME, SECONDS
*	C, PUBLIC
*$ Declarations:
*	void		Jul_DHMSofSec(secs, day, hour, minute, second)
*	RL_FLT8		secs, *second;
*	RL_INT4		*day, *hour, *minute;
*$ Inputs:
*	secs		number of seconds.
*$ Outputs:
*	*day		number of days.
*	*hour		number of hours into day (0-23).
*	*minute		number of minutes into hour (0-59).
*	*second		number of seconds into minute (0. <= *second < 60.)
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a number of seconds to days, hours, minutes and
*	seconds.  All days are assumed to contain 86400 seconds; leap seconds
*	are not supported.
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

void	Jul_DHMSofSec(secs, day, hour, minute, second)
RL_FLT8	secs, *second;
RL_INT4	*day, *hour, *minute;
{
RL_FLT8	h, m, s;

	Jul_DSofSec(secs, day, &s);

	h = floor(s / 3600.);
	s -= 3600.*h;

	m = floor(s / 60.);
	s -= 60.*m;

	*hour   = h;
	*minute = m;
	*second = s;
}

/*<A NAME="Jul_HMSofSec"></A>
********************************************************************************
*$ Component_name:
*	Jul_HMSofSec (seconds.c)
*$ Abstract:
*	Converts a number of seconds to hours, minutes and seconds, possibly
*	including leap seconds.
*$ Keywords:
*	JULIAN, TIME, SECONDS
*	C, PUBLIC
*$ Declarations:
*	void		Jul_HMSofSec(secs, hour, minute, second)
*	RL_FLT8		secs, *second;
*	RL_INT4		*hour, *minute;
*$ Inputs:
*	secs		number of seconds.
*$ Outputs:
*	*hour		number of hours into day (0-23).
*	*minute		number of minutes into hour (0-59).
*	*second		number of seconds into minute (>= 60. for leap seconds).
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a number of seconds to hours, minutes and
*	seconds.  Seconds greater than 86400 are treated as leap seconds.
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

void	Jul_HMSofSec(secs, hour, minute, second)
RL_FLT8	secs, *second;
RL_INT4	*hour, *minute;
{
RL_INT4	h, m;

	h = floor(secs / 3600.);
	if (h <  0) h =  0;
	if (h > 23) h = 23;
	secs -= 3600.*h;

	m = floor(secs / 60.);
	if (m <  0) m =  0;
	if (m > 59) m = 59;
	secs -= 60.*m;

	*hour   = h;
	*minute = m;
	*second = secs;
}

/*<A NAME="FJul_SecofDHMS"></A>
********************************************************************************
* FORTRAN INTERFACE ROUTINES
********************************************************************************
*$ Component_name:
*	FJul_SecofDHMS (seconds.c)
*$ Abstract:
*	Converts days, hours, minutes and seconds to a number of seconds.
*$ Keywords:
*	JULIAN, TIME, SECONDS
*	FORTRAN, PUBLIC
*$ Declarations:
*	real*8 function FJul_SecofDHMS(day, hour, minute, second)
*	integer*4	day, hour, minute
*	real*8		second
*$ Inputs:
*	day		number of days.
*	hour		number of hours (typically 0-23).
*	minute		number of minutes (typically 0-59).
*	second		floating-point number of seconds (typically 0-60).
*$ Outputs:
*	none
*$ Returns:
*	total number of seconds.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts days, hours, minutes and seconds to a number
*	of seconds.  It assumes no leap seconds are involved.
*$ External_references:
*	Jul_SecofDHMS(), FORT_Init()
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

RL_FLT8	FORTRAN_NAME(fjul_secofdhms) (day, hour, minute, second)
RL_INT4	*day, *hour, *minute;
RL_FLT8	*second;
{
	FORT_Init();

	return Jul_SecofDHMS(*day, *hour, *minute, *second);
}

/*<A NAME="FJul_DSofSec"></A>
********************************************************************************
*$ Component_name:
*	FJul_DSofSec (seconds.c)
*$ Abstract:
*	Converts a number of seconds to days, plus seconds into day.
*$ Keywords:
*	JULIAN, TIME, SECONDS
*	FORTRAN, PUBLIC
*$ Declarations:
*	subroutine FJul_DSofSec(secs, day, second)
*	real*8		secs, second
*	integer*4	day
*$ Inputs:
*	secs		number of seconds.
*$ Outputs:
*	day		number of days.
*	second		seconds into day (0. <= second < 86400).
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a number of seconds to days, plus seconds into
*	day. It assumes no leap seconds are involved.
*$ External_references:
*	Jul_DSofSec(), FORT_Init()
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

void	FORTRAN_NAME(fjul_dsofsec) (secs, day, second)
RL_FLT8	*secs, *second;
RL_INT4	*day;
{
	FORT_Init();

	Jul_DSofSec(*secs, day, second);
}

/*<A NAME="FJul_DHMSofSec"></A>
********************************************************************************
*$ Component_name:
*	FJul_DHMSofSec (seconds.c)
*$ Abstract:
*	Converts a number of seconds to days, hours, minutes and seconds.
*$ Keywords:
*	JULIAN, TIME, SECONDS
*	FORTRAN, PUBLIC
*$ Declarations:
*	subroutine FJul_DHMSofSec(secs, day, hour, minute, second)
*	real*8		secs, second
*	integer*4	day, hour, minute
*$ Inputs:
*	secs		number of seconds.
*$ Outputs:
*	day		number of days.
*	hour		number of hours into day (0-23).
*	minute		number of minutes into hour (0-59).
*	second		number of seconds into minute (0. <= second < 60.)
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a number of seconds to days, hours, minutes and
*	seconds.  All days are assumed to contain 86400 seconds; leap seconds
*	are not supported.
*$ External_references:
*	Jul_DHMSofSec(), FORT_Init()
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

void	FORTRAN_NAME(fjul_dhmsofsec) (secs, day, hour, minute, second)
RL_FLT8	*secs, *second;
RL_INT4	*day, *hour, *minute;
{
	FORT_Init();

	Jul_DHMSofSec(*secs, day, hour, minute, second);
}

/*<A NAME="FJul_HMSofSec"></A>
********************************************************************************
*$ Component_name:
*	FJul_HMSofSec (seconds.c)
*$ Abstract:
*	Converts a number of seconds to hours, minutes and seconds, possibly
*	including leap seconds.
*$ Keywords:
*	JULIAN, TIME, SECONDS
*	FORTRAN, PUBLIC
*$ Declarations:
*	subroutine	FJul_HMSofSec(secs, hour, minute, second)
*	real*8		secs, second
*	integer*4	hour, minute
*$ Inputs:
*	secs		number of seconds.
*$ Outputs:
*	hour		number of hours into day (0-23).
*	minute		number of minutes into hour (0-59).
*	second		number of seconds into minute (>= 60. for leap seconds).
*$ Returns:
*	none
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a number of seconds to hours, minutes and
*	seconds.  Seconds greater than 86400 are treated as leap seconds.
*$ External_references:
*	Jul_HMSofSec(), FORT_Init()
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

void	FORTRAN_NAME(fjul_hmsofsec) (secs, hour, minute, second)
RL_FLT8	*secs, *second;
RL_INT4	*hour, *minute;
{
	FORT_Init();

	Jul_HMSofSec(*secs, hour, minute, second);
}

/*******************************************************************************
</PRE></BODY></HTML>*/
