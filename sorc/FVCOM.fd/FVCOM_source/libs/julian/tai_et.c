/*<HTML><HEAD><TITLE> tai_et.c </TITLE></HEAD><BODY><PRE>
********************************************************************************
* tai_et.c
*
* This pair of functions converts between TAI (atomic time) and ET (ephemeris
* time).
*
* RL_FLT8 Jul_TAIofET(et)
*		converts from ephemeris time to atomic time.
* RL_FLT8 Jul_ETofTAI(tai)
*		converts from atomic time to ephemeris time.
*
* Mark R. Showalter, PDS Rings Node, November 1995
*******************************************************************************/
#include <math.h>
#include "julian.h"
#include "fortran.h"

/*<A NAME="Jul_TAIofET"></A>
********************************************************************************
* EXPORTED USER ROUTINES
********************************************************************************
*$ Component_name:
*	Jul_TAIofET (tai_et.c)
*$ Abstract:
*	Converts from ET (ephemeris time) to TAI (Atomic time).
*$ Keywords:
*	JULIAN, TIME, UNIVERSAL_TIME, EPHEMERIS_TIME
*	C, PUBLIC
*$ Declarations:
*	RL_FLT8		Jul_TAIofET(et)
*	RL_FLT8		et;
*$ Inputs:
*	et		ephmeris time (seconds relative to J2000 ET).
*$ Outputs:
*	none
*$ Returns:
*	corresponding atomic time (seconds relative to J2000 TAI).
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts from ET (ephemeris time) to TAI (Atomic time).
*	All times are given as seconds relative to J2000.  The algorithm is
*	based on that used in the SPICE toolkit.
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

#define M0              6.239996
#define M1		1.99096871e-7
#define EB		1.671e-2
#define K		1.657e-3
#define DELTA_T_A	32.184

RL_FLT8	Jul_TAIofET(et)
RL_FLT8	et;
{
RL_FLT8		mean_anom, ecc_anom;

	mean_anom = M0 + M1 * et;
	ecc_anom = mean_anom + EB * sin(mean_anom);

	return et - DELTA_T_A - K * sin(ecc_anom);
}

/*<A NAME="Jul_ETofTAI"></A>
********************************************************************************
*$ Component_name:
*	Jul_ETofTAI (tai_et.c)
*$ Abstract:
*	Converts from TAI (Atomic time) to ET (ephemeris time).
*$ Keywords:
*	JULIAN, TIME, UNIVERSAL_TIME, EPHEMERIS_TIME
*	C, PUBLIC
*$ Declarations:
*	RL_FLT8		Jul_ETofTAI(tai)
*	RL_FLT8		tai;
*$ Inputs:
*	tai		atomic time (seconds relative to J2000 TAI).
*$ Outputs:
*	none
*$ Returns:
*	corresponding ephmeris time (seconds relative to J2000 ET).
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts from TAI (Atomic time) to ET (ephemeris time).
*	All times are given as seconds relative to J2000.  This is the inverse
*	of function Jul_TAIofET().
*$ External_references:
*	Jul_TAIofET()
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

RL_FLT8	Jul_ETofTAI(tai)
RL_FLT8	tai;
{
RL_FLT8	et, et_prev;

/* Take an initial guess at the ephemeris time */
	et = tai + DELTA_T_A;

/* Iterate until time converges */
	do {
		et_prev = et;
		et += tai - Jul_TAIofET(et);
	}
	while (et != et_prev);

	return et;
}

/*<A NAME="FJul_TAIofET"></A>
********************************************************************************
* FORTRAN INTERFACE ROUTINES
********************************************************************************
*$ Component_name:
*	FJul_TAIofET (tai_et.c)
*$ Abstract:
*	Converts from ET (ephemeris time) to TAI (Atomic time).
*$ Keywords:
*	JULIAN, TIME, UNIVERSAL_TIME, EPHEMERIS_TIME
*	FORTRAN, PUBLIC
*$ Declarations:
*	real*8 function FJul_TAIofET(et)
*	real*8		et
*$ Inputs:
*	et		ephmeris time (seconds relative to J2000 ET).
*$ Outputs:
*	none
*$ Returns:
*	corresponding atomic time (seconds relative to J2000 TAI).
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts from ET (ephemeris time) to TAI (Atomic time).
*	All times are given as seconds relative to J2000.  The algorithm is
*	based on that used in the SPICE toolkit.
*$ External_references:
*	Jul_TAIofET(), RL_FortInit()
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

RL_FLT8	FORTRAN_NAME(fjul_taiofet) (et)
RL_FLT8	*et;
{
	FORT_Init();

	return Jul_TAIofET(*et);
}

/*<A NAME="FJul_ETofTAI"></A>
********************************************************************************
*$ Component_name:
*	FJul_ETofTAI (tai_et.c)
*$ Abstract:
*	Converts from TAI (Atomic time) to ET (ephemeris time).
*$ Keywords:
*	JULIAN, TIME, UNIVERSAL_TIME, EPHEMERIS_TIME
*	FORTRAN, PUBLIC
*$ Declarations:
*	real*8 function FJul_ETofTAI(tai)
*	real*8		tai
*$ Inputs:
*	tai		atomic time (seconds relative to J2000 TAI).
*$ Outputs:
*	none
*$ Returns:
*	corresponding ephmeris time (seconds relative to J2000 ET).
*$ Side_effects:
*	none
*$ Detailed_description:
*	This function converts from TAI (Atomic time) to ET (ephemeris time).
*	All times are given as seconds relative to J2000.  This is the inverse
*	of function FJul_TAIofET().
*$ External_references:
*	Jul_ETofTAI(), RL_FortInit()
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

RL_FLT8	FORTRAN_NAME(fjul_etoftai) (tai)
RL_FLT8	*tai;
{
	FORT_Init();

	return Jul_ETofTAI(*tai);
}

/*******************************************************************************
</PRE></BODY></HTML>*/
