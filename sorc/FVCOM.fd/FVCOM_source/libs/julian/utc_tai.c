/*<HTML><HEAD><TITLE> utc_tai.c </TITLE></HEAD><BODY><PRE>
********************************************************************************
* utc_tai.c
*
* This set of Julian routines converts between days UTC (Universal time) and
* seconds TAI (Atomic time).  The time J2000 refers to noon on January 1, 2000.
*
* RL_FLT8 Jul_TAIofDUTC(dutc)
*		converts from days since J2000 UTC to seconds since J2000 TAI.
* RL_INT4 Jul_DUTCofTAI(tai, secs)
*		converts from seconds since J2000 TAI to days and seconds since
*		J2000 UTC.
*
* Mark R. Showalter, PDS Rings Node, November 1995
*******************************************************************************/
#include <stdlib.h>
#include <math.h>
#include "julian.h"
#include "fortran.h"

/*<A NAME="Jul_TAIofDUTC"></A>
********************************************************************************
* EXPORTED USER ROUTINES
********************************************************************************
*$ Component_name:
*	Jul_TAIofDUTC
*$ Abstract:
*	Converts a number of days relative to J2000 UTC to a number of seconds
*	relative to J2000 TAI.
*$ Keywords:
*	JULIAN, TIME, ATOMIC_TIME, UNIVERSAL_TIME
*	C, PUBLIC
*$ Declarations:
*	RL_FLT8		Jul_TAIofDUTC(dutc)
*	RL_INT4		dutc;
*$ Inputs:
*	dutc		number of days relative to January 1, 2000.
*$ Outputs:
*	none
*$ Returns:
*	number of seconds since noon J2000 TAI at beginning of day.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a number of days relative to J2000 UTC to a
*	number of seconds relative to J2000 TAI.
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

#define Jul_TAIofDUTC_midnight(dutc)	(86400.*(dutc) + Jul_LeapSecs(dutc))

RL_FLT8	Jul_TAIofDUTC(dutc)
RL_INT4	dutc;
{
	return Jul_TAIofDUTC_midnight(dutc) - 43200.;
}

/*<A NAME="Jul_DUTCofTAI"></A>
********************************************************************************
*$ Component_name:
*	Jul_DUTCofTAI
*$ Abstract:
*	Returns the day UTC on which a given second TAI falls.
*$ Keywords:
*	JULIAN, TIME, ATOMIC_TIME, UNIVERSAL_TIME
*	C, PUBLIC
*$ Declarations:
*	RL_INT4		Jul_DUTCofTAI(tai, secs)
*	RL_FLT8		tai, *secs;
*$ Inputs:
*	tai		number of seconds relative to J2000 TAI.
*$ Outputs:
*	*secs		if secs != NULL, the number of seconds into the day
*			(0 <= *secs < 86401).
*$ Returns:
*	number of days relative to January 1, 2000 on which the second falls.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function returns the day UTC on which a given second relative to
*	J2000 TAI falls.  Optionally, it also returns the number of seconds into
*	the given day.
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

RL_INT4	Jul_DUTCofTAI(tai, secs)
RL_FLT8	tai, *secs;
{
RL_INT4	dlo, dhi;
RL_FLT8	tlo, thi;

/* Shift time from noon to midnight */
	tai += 43200.;

/* Make a guess at the day */
	dhi = tai / 86400.;
	thi = Jul_TAIofDUTC_midnight(dhi);

/* If it's too low, search upward */
	if (thi < tai)
		do {
			tlo = thi;
			dlo = dhi;

			dhi = dlo + 1;
			thi = Jul_TAIofDUTC_midnight(dhi);
		} while (thi <= tai);

/* Otherwise, search downward */
	else {
		tlo = thi;
		dlo = dhi;
		while (tlo > tai) {
			dlo--;
			tlo = Jul_TAIofDUTC_midnight(dlo);
		}
	}

/* Return the remaining seconds if necessary */
	if (secs != NULL) {
		*secs = tai - tlo;
	}

/* Return the number of days */
	return dlo;
}

/*<A NAME="FJul_TAIofDUTC"></A>
********************************************************************************
* FORTRAN INTERFACE ROUTINES
********************************************************************************
*$ Component_name:
*	FJul_TAIofDUTC
*$ Abstract:
*	Converts a number of days relative to J2000 UTC to a number of seconds
*	relative to J2000 TAI.
*$ Keywords:
*	JULIAN, TIME, ATOMIC_TIME, UNIVERSAL_TIME
*	FORTRAN, PUBLIC
*$ Declarations:
*	real*8 function FJul_TAIofDUTC(dutc)
*	integer*4	dutc
*$ Inputs:
*	dutc		number of days relative to January 1, 2000.
*$ Outputs:
*	none
*$ Returns:
*	number of seconds since noon J2000 TAI at beginning of day.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a number of days relative to J2000 UTC to a
*	number of seconds relative to J2000 TAI.
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

RL_FLT8	FORTRAN_NAME(fjul_taiofdutc) (dutc)
RL_INT4	*dutc;
{
	return Jul_TAIofDUTC(*dutc);
}

/*<A NAME="FJul_DUTCofTAI"></A>
********************************************************************************
*$ Component_name:
*	FJul_DUTCofTAI
*$ Abstract:
*	Returns the day UTC on which a given second TAI falls.
*$ Keywords:
*	JULIAN, TIME, ATOMIC_TIME, UNIVERSAL_TIME
*	FORTRAN, PUBLIC
*$ Declarations:
*	integer*4 function FJul_DUTCofTAI(tai, secs)
*	real*8		tai, secs
*$ Inputs:
*	tai		number of seconds relative to J2000 TAI.
*$ Outputs:
*	secs		the number of seconds into the day (0 <= secs < 86401).
*$ Returns:
*	number of days relative to January 1, 2000 on which the second falls.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function returns the day UTC on which a given second relative to
*	J2000 TAI falls.  It also returns the number of seconds into the given
*	day.
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

RL_INT4	FORTRAN_NAME(fjul_dutcoftai) (tai, secs)
RL_FLT8	*tai, *secs;
{
	return Jul_DUTCofTAI(*tai, secs);
}

/*******************************************************************************
</PRE></BODY></HTML>*/
