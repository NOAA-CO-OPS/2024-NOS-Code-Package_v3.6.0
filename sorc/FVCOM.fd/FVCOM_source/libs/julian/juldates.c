/*<HTML><HEAD><TITLE> juldates.c </TITLE></HEAD><BODY><PRE>
********************************************************************************
* juldates.c
*
* This set of Julian routines converts between times and Julian dates, where
* Julian dates can be in any of three possible time systems: Universal time
* (UTC), Atomic time (TAI) and Ephemeris time (ET).
*
* RL_FLT8 Jul_TAIofJD(jd, type)
* RL_FLT8 Jul_JDofTAI(tai, type)
*		converts between Julian dates and seconds since J2000 TAI.
* RL_FLT8 Jul_TAIofMJD(mjd, type)
* RL_FLT8 Jul_MJDofTAI(tai, type)
*		converts between modified Julian dates and seconds since J2000
*		TAI.
*
* Mark R. Showalter, PDS Rings Node, December 1995
* Revised by MRS 6/98 to conform to RingLib naming conventions.
*******************************************************************************/
#include <math.h>
#include "julian.h"
#include "fortran.h"

/***********************************************************
* Definitions
***********************************************************/

#define MJD_OF_J2000_NOON    51544.5	/* Modified Julian Date of J2000 noon */
#define  JD_OF_J2000_NOON  2451545.0	/* Julian Date of J2000 noon */

/***********************************************************
* Prototypes of internal functions
***********************************************************/

static RL_FLT8 ZJul_DUTCfloatofTAI RL_PROTO((RL_FLT8 tai));
static RL_FLT8 ZJul_TAIofDUTCfloat RL_PROTO((RL_FLT8 dfloat));

/*<A NAME="Jul_TAIofJD"></A>
********************************************************************************
* EXPORTED USER ROUTINES
********************************************************************************
*$ Component_name:
*	Jul_TAIofJD (juldates.c)
*$ Abstract:
*	Converts a Julian date to a number of seconds relative to J2000 TAI.
*$ Keywords:
*	JULIAN, TIME, ATOMIC_TIME, UNIVERSAL_TIME, EPHEMERIS_TIME, JULIAN_DATE
*	C, PUBLIC
*$ Declarations:
*	RL_FLT8		Jul_TAIofJD(jd, type)
*	RL_FLT8		jd;
*	RL_INT4		type;
*$ Inputs:
*	jd		Julian date, in one of three possible time systems.
*	type		type of the Julian date, one of the constants
*			JUL_UTC_TYPE (0), JUL_TAI_TYPE (1), or JUL_ET_TYPE (2).
*$ Outputs:
*	none
*$ Returns:
*	number of seconds since noon J2000 TAI.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a Julian date to a number of seconds relative to
*	J2000 TAI.  The Julian date may be given in any of three possible time
*	systems: Universal time (UTC), Atomic time (TAI), or Ephemeris time
*	(ET).
*
*	If the date type is UTC, fractional days are scaled by the number of
*	seconds between one UTC noon and the next; since some days have leap
*	seconds, this makes the spacing of UTC Julian dates slightly
*	non-uniform.
*$ External_references:
*	Jul_TAIofET()
*$ Examples:
*	none
*$ Error_handling:
*	If the type constant is not one of the allowed values, JUL_UTC_TYPE is
*	assumed.
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

RL_FLT8	Jul_TAIofJD(jd, type)
RL_FLT8	jd;
RL_INT4	type;
{
RL_FLT8	tai, et;

	switch (type) {

	case JUL_TAI_TYPE:
		tai = (jd - JD_OF_J2000_NOON) * 86400.;
		return tai;

	case JUL_ET_TYPE:
		et = (jd - JD_OF_J2000_NOON) * 86400.;
		return Jul_TAIofET(et);

	default:
		return ZJul_TAIofDUTCfloat(jd - JD_OF_J2000_NOON);
	}
}

/*<A NAME="Jul_JDofTAI"></A>
********************************************************************************
*$ Component_name:
*	Jul_JDofTAI (juldates.c)
*$ Abstract:
*	Converts a number of seconds relative to J2000 TAI to a Julian date.
*$ Keywords:
*	JULIAN, TIME, ATOMIC_TIME, UNIVERSAL_TIME, EPHEMERIS_TIME, JULIAN_DATE
*	C, PUBLIC
*$ Declarations:
*	RL_FLT8		Jul_JDofTAI(tai, type)
*	RL_FLT8		tai;
*	RL_INT4		type;
*$ Inputs:
*	tai		number of seconds since noon J2000 TAI.
*	type		type of the Julian date, one of the constants
*			JUL_UTC_TYPE (0), JUL_TAI_TYPE (1), or JUL_ET_TYPE (2).
*$ Outputs:
*	none
*$ Returns:
*	jd		Julian date, in one of three possible time systems.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a number of seconds relative to J2000 TAI to a
*	Julian date.  The Julian date is given in any of three possible time
*	systems: Universal time (UTC), Atomic time (TAI), or Ephemeris time
*	(ET).
*
*	If the date type is UTC, fractional days are scaled by the number of
*	seconds between one UTC noon and the next; since some days have leap
*	seconds, this makes the spacing of UTC Julian dates slightly
*	non-uniform.
*$ External_references:
*	none
*$ Examples:
*	none
*$ Error_handling:
*	If the type constant is not one of the allowed values, JUL_UTC_TYPE is
*	assumed.
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

RL_FLT8	Jul_JDofTAI(tai, type)
RL_FLT8	tai;
RL_INT4	type;
{
RL_FLT8	et;

	switch (type) {

	case JUL_TAI_TYPE:
		return tai/86400. + JD_OF_J2000_NOON;

	case JUL_ET_TYPE:
		et = Jul_ETofTAI(tai);
		return et/86400. + JD_OF_J2000_NOON;

	default:
		return ZJul_DUTCfloatofTAI(tai) + JD_OF_J2000_NOON;
	}
}

/*<A NAME="Jul_TAIofMJD"></A>
********************************************************************************
*$ Component_name:
*	Jul_TAIofMJD (juldates.c)
*$ Abstract:
*	Converts a Modified Julian date to a number of seconds relative to J2000
*	TAI.
*$ Keywords:
*	JULIAN, TIME, ATOMIC_TIME, UNIVERSAL_TIME, EPHEMERIS_TIME, JULIAN_DATE
*	C, PUBLIC
*$ Declarations:
*	RL_FLT8		Jul_TAIofMJD(mjd, type)
*	RL_FLT8		mjd;
*	RL_INT4		type;
*$ Inputs:
*	mjd		Modified Julian date, in one of three possible time
*			systems.
*	type		type of the Modified Julian date, one of the constants
*			JUL_UTC_TYPE (0), JUL_TAI_TYPE (1), or JUL_ET_TYPE (2).
*$ Outputs:
*	none
*$ Returns:
*	number of seconds since noon J2000 TAI.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a Modified Julian date to a number of seconds
*	relative to J2000 TAI.  The Julian date may be given in any of three
*	possible time systems: Universal time (UTC), Atomic time (TAI), or
*	Ephemeris time (ET).
*
*	If the date type is UTC, fractional days are scaled by the number of
*	seconds between one UTC noon and the next; since some days have leap
*	seconds, this makes the spacing of UTC Modified Julian dates slightly
*	non-uniform.
*$ External_references:
*	Jul_TAIofET()
*$ Examples:
*	none
*$ Error_handling:
*	If the type constant is not one of the allowed values, JUL_UTC_TYPE is
*	assumed.
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

RL_FLT8	Jul_TAIofMJD(mjd, type)
RL_FLT8	mjd;
RL_INT4	type;
{
RL_FLT8	tai, et;

	switch (type) {

	case JUL_TAI_TYPE:
		tai = (mjd - MJD_OF_J2000_NOON) * 86400.;
		return tai;

	case JUL_ET_TYPE:
		et = (mjd - MJD_OF_J2000_NOON) * 86400.;
		return Jul_TAIofET(et);

	default:
		return ZJul_TAIofDUTCfloat(mjd - MJD_OF_J2000_NOON);
	}
}

/*<A NAME="Jul_MJDofTAI"></A>
********************************************************************************
*$ Component_name:
*	Jul_MJDofTAI (juldates.c)
*$ Abstract:
*	Converts a number of seconds relative to J2000 TAI to a Modified Julian
*	date.
*$ Keywords:
*	JULIAN, TIME, ATOMIC_TIME, UNIVERSAL_TIME, EPHEMERIS_TIME, JULIAN_DATE
*	C, PUBLIC
*$ Declarations:
*	RL_FLT8		Jul_MJDofTAI(tai, type)
*	RL_FLT8		tai;
*	RL_INT4		type;
*$ Inputs:
*	tai		number of seconds since noon J2000 TAI.
*	type		type of the Modified Julian date, one of the constants
*			JUL_UTC_TYPE (0), JUL_TAI_TYPE (1), or JUL_ET_TYPE (2).
*$ Outputs:
*	none
*$ Returns:
*	jd		Modified Julian date, in one of three possible time
*			systems.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a number of seconds relative to J2000 TAI to a
*	Julian date.  The Julian date is given in any of three possible time
*	systems: Universal time (UTC), Atomic time (TAI), or Ephemeris time
*	(ET).
*
*	If the date type is UTC, fractional days are scaled by the number of
*	seconds between one UTC noon and the next; since some days have leap
*	seconds, this makes the spacing of UTC Julian dates slightly
*	non-uniform.
*$ External_references:
*	none
*$ Examples:
*	none
*$ Error_handling:
*	If the type constant is not one of the allowed values, JUL_UTC_TYPE is
*	assumed.
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

RL_FLT8	Jul_MJDofTAI(tai, type)
RL_FLT8	tai;
RL_INT4	type;
{
RL_FLT8	et;

	switch (type) {

	case JUL_TAI_TYPE:
		return tai/86400. + MJD_OF_J2000_NOON;

	case JUL_ET_TYPE:
		et = Jul_ETofTAI(tai);
		return et/86400. + MJD_OF_J2000_NOON;

	default:
		return ZJul_DUTCfloatofTAI(tai) + MJD_OF_J2000_NOON;
	}
}

/*<A NAME="ZJul_TAIofDUTCfloat"></A>
********************************************************************************
* INTERNAL FUNCTIONS
********************************************************************************
* RL_FLT8 ZJul_TAIofDUTCfloat(RL_FLT8 dfloat)
*
* This internal function calculates the number of seconds TAI from the floating-
* point number of days UTC relative to noon J2000.
*
* Input:
*	dfloat		days relative to J2000 noon UTC.
*
* Return:		seconds from J2000 noon TAI.
*******************************************************************************/

static RL_FLT8 ZJul_TAIofDUTCfloat(dfloat)
RL_FLT8	dfloat;
{
RL_FLT8	dfloor, frac, leaps;
RL_INT4	dutc_noon;

	dfloor = floor(dfloat);
	dutc_noon = (RL_INT4) dfloor;

	leaps = Jul_LeapSecs(dutc_noon);

	frac = dfloat - dfloor;
	if (frac != 0.) frac *= (86400. + Jul_LeapSecs(dutc_noon+1) - leaps);
				
	return 86400.*dutc_noon + leaps + frac;
}

/*<A NAME="ZJul_DUTCfloatofTAI"></A>
********************************************************************************
* RL_FLT8 ZJul_DUTCfloatofTAI(RL_FLT8 tai)
*
* This internal function calculates the floating-point number of days UTC
* relative to noon J2000 from the number of seconds TAI.
*
* Input:
*	tai		seconds from J2000 noon TAI.
*
* Return:		days relative to J2000 noon UTC.
*******************************************************************************/

static RL_FLT8 ZJul_DUTCfloatofTAI(tai)
RL_FLT8	tai;
{
RL_FLT8	frac, secs;
RL_INT4	dutc;

	dutc = Jul_DUTCofTAI(tai, &secs);

	secs -= 43200.;		/* seconds relative to noon */
	if (secs < 0.) {
		dutc--;
		frac = secs / Jul_DaySecs(dutc) + 1.;
	}
	else {
		frac = secs / Jul_DaySecs(dutc);
	}

	return dutc + frac;
}

/*<A NAME="FJul_TAIofJD"></A>
********************************************************************************
* FORTRAN INTERFACE ROUTINES
********************************************************************************
*$ Component_name:
*	FJul_TAIofJD (juldates.c)
*$ Abstract:
*	Converts a Julian date to a number of seconds relative to J2000 TAI.
*$ Keywords:
*	JULIAN, TIME, ATOMIC_TIME, UNIVERSAL_TIME, EPHEMERIS_TIME, JULIAN_DATE
*	FORTRAN, PUBLIC
*$ Declarations:
*	real*8 function FJul_TAIofJD(jd, type)
*	real*8		jd
*	integer*4	type;
*$ Inputs:
*	jd		Julian date, in one of three possible time systems.
*	type		type of the Julian date, one of the constants
*			FJUL_UTC_TYPE (0), FJUL_TAI_TYPE (1), or FJUL_ET_TYPE
*			(2).  These parameters are defined in include file
*			"fjulian.inc".
*$ Outputs:
*	none
*$ Returns:
*	number of seconds since noon J2000 TAI.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a Julian date to a number of seconds relative to
*	J2000 TAI.  The Julian date may be given in any of three possible time
*	systems: Universal time (UTC), Atomic time (TAI), or Ephemeris time
*	(ET).
*
*	If the date type is UTC, fractional days are scaled by the number of
*	seconds between one UTC noon and the next; since some days have leap
*	seconds, this makes the spacing of UTC Julian dates slightly
*	non-uniform.
*$ External_references:
*	Jul_TAIofJD(), FORT_Init()
*$ Examples:
*	none
*$ Error_handling:
*	If the type constant is not one of the allowed values, FJUL_UTC_TYPE is
*	assumed.
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

RL_FLT8	FORTRAN_NAME(fjul_taiofjd) (jd, type)
RL_FLT8	*jd;
RL_INT4	*type;
{
	FORT_Init();

	return Jul_TAIofJD(*jd, *type);
}

/*<A NAME="FJul_JDofTAI"></A>
********************************************************************************
*$ Component_name:
*	FJul_JDofTAI (juldates.c)
*$ Abstract:
*	Converts a number of seconds relative to J2000 TAI to a Julian date.
*$ Keywords:
*	JULIAN, TIME, ATOMIC_TIME, UNIVERSAL_TIME, EPHEMERIS_TIME, JULIAN_DATE
*	FORTRAN, PUBLIC
*$ Declarations:
*	real*8 function FJul_JDofTAI(tai, type)
*	real*8		tai
*	integer*4	type
*$ Inputs:
*	tai		number of seconds since noon J2000 TAI.
*	type		type of the Julian date, one of the constants
*			FJUL_UTC_TYPE (0), FJUL_TAI_TYPE (1), or FJUL_ET_TYPE
*			(2).  These parameters are defined in include file
*			"fjulian.inc".
*$ Outputs:
*	none
*$ Returns:
*	jd		Julian date, in one of three possible time systems.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a number of seconds relative to J2000 TAI to a
*	Julian date.  The Julian date is given in any of three possible time
*	systems: Universal time (UTC), Atomic time (TAI), or Ephemeris time
*	(ET).
*
*	If the date type is UTC, fractional days are scaled by the number of
*	seconds between one UTC noon and the next; since some days have leap
*	seconds, this makes the spacing of UTC Julian dates slightly
*	non-uniform.
*$ External_references:
*	Jul_JDofTAI(), FORT_Init()
*$ Examples:
*	none
*$ Error_handling:
*	If the type constant is not one of the allowed values, FJUL_UTC_TYPE is
*	assumed.
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

RL_FLT8	FORTRAN_NAME(fjul_jdoftai) (tai, type)
RL_FLT8	*tai;
RL_INT4	*type;
{
	FORT_Init();

	return Jul_JDofTAI(*tai, *type);
}

/*<A NAME="FJul_TAIofMJD"></A>
********************************************************************************
*$ Component_name:
*	FJul_TAIofMJD (juldates.c)
*$ Abstract:
*	Converts a Modified Julian date to a number of seconds relative to J2000
*	TAI.
*$ Keywords:
*	JULIAN, TIME, ATOMIC_TIME, UNIVERSAL_TIME, EPHEMERIS_TIME, JULIAN_DATE
*	FORTRAN, PUBLIC
*$ Declarations:
*	real*8 function FJul_TAIofMJD(mjd, type)
*	real*8		mjd
*	integer*4	type
*$ Inputs:
*	mjd		Modified Julian date, in one of three possible time
*			systems.
*	type		type of the Modified Julian date, one of the constants
*			FJUL_UTC_TYPE (0), FJUL_TAI_TYPE (1), or FJUL_ET_TYPE
*			(2).  These parameters are defined in include file
*			"fjulian.inc".
*$ Outputs:
*	none
*$ Returns:
*	number of seconds since noon J2000 TAI.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a Modified Julian date to a number of seconds
*	relative to J2000 TAI.  The Julian date may be given in any of three
*	possible time systems: Universal time (UTC), Atomic time (TAI), or
*	Ephemeris time (ET).
*
*	If the date type is UTC, fractional days are scaled by the number of
*	seconds between one UTC noon and the next; since some days have leap
*	seconds, this makes the spacing of UTC Modified Julian dates slightly
*	non-uniform.
*$ External_references:
*	Jul_TAIofMJD(), FORT_Init()
*$ Examples:
*	none
*$ Error_handling:
*	If the type constant is not one of the allowed values, JUL_UTC_TYPE is
*	assumed.
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

RL_FLT8	FORTRAN_NAME(fjul_taiofmjd) (mjd, type)
RL_FLT8	*mjd;
RL_INT4	*type;
{
	FORT_Init();

	return Jul_TAIofMJD(*mjd, *type);
}

/*<A NAME="FJul_MJDofTAI"></A>
********************************************************************************
*$ Component_name:
*	FJul_MJDofTAI (juldates.c)
*$ Abstract:
*	Converts a number of seconds relative to J2000 TAI to a Modified Julian
*	date.
*$ Keywords:
*	JULIAN, TIME, ATOMIC_TIME, UNIVERSAL_TIME, EPHEMERIS_TIME, JULIAN_DATE
*	FORTRAN, PUBLIC
*$ Declarations:
*	real*8 function FJul_MJDofTAI(tai, type)
*	real*8		tai
*	integer*4	type
*$ Inputs:
*	tai		number of seconds since noon J2000 TAI.
*	type		type of the Modified Julian date, one of the constants
*			FJUL_UTC_TYPE (0), FJUL_TAI_TYPE (1), For JUL_ET_TYPE
*			(2).  These parameters are defined in include file
*			"fjulian.inc".
*$ Outputs:
*	none
*$ Returns:
*	mjd		Modifed Julian date, in one of three possible time
*			systems.
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts a number of seconds relative to J2000 TAI to a
*	Julian date.  The Julian date is given in any of three possible time
*	systems: Universal time (UTC), Atomic time (TAI), or Ephemeris time
*	(ET).
*
*	If the date type is UTC, fractional days are scaled by the number of
*	seconds between one UTC noon and the next; since some days have leap
*	seconds, this makes the spacing of UTC Julian dates slightly
*	non-uniform.
*$ External_references:
*	Jul_MJDofTAI(), FORT_Init()
*$ Examples:
*	none
*$ Error_handling:
*	If the type constant is not one of the allowed values, JUL_UTC_TYPE is
*	assumed.
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

RL_FLT8	FORTRAN_NAME(fjul_mjdoftai) (tai, type)
RL_FLT8	*tai;
RL_INT4	*type;
{
	FORT_Init();

	return Jul_MJDofTAI(*tai, *type);
}

/*******************************************************************************
</PRE></BODY></HTML>*/
