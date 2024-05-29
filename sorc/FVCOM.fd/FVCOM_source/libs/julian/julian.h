/*<HTML><HEAD><TITLE> julian.h </TITLE></HEAD><BODY><PRE>
********************************************************************************
* julian.h -- include file for Julian Toolkit
*
* Mark Showalter, PDS Rings Node, December 1995
* Updated June 1998 to conform to other RingLib naming conventions.
*******************************************************************************/
#include "ringlib.h"

/*******************************************************************************
* GLOBAL SYMBOLS
*******************************************************************************/

#ifndef TRUE
#define TRUE	((RL_BOOL) 1)
#define FALSE	((RL_BOOL) 0)
#endif

#ifndef NULL
#define NULL	((RL_VOID *) 0)
#endif

#define		JUL_UTC_TYPE	0
#define		JUL_TAI_TYPE	1
#define		JUL_ET_TYPE	2

/*******************************************************************************
* FUNCTION PROTOTYPES
********************************************************************************
* dates.c
*******************************************************************************/

RL_INT4	Jul_DUTCofYMD  RL_PROTO((RL_INT4 year, RL_INT4 month, RL_INT4 day));
void	Jul_YDofDUTC   RL_PROTO((RL_INT4 dutc, RL_INT4 *year, RL_INT4 *day));
void	Jul_YMDofDUTC  RL_PROTO((RL_INT4 dutc, RL_INT4 *year, RL_INT4 *month,
	                         RL_INT4 *day));
void	Jul_Gregorian  RL_PROTO((RL_INT4 year, RL_INT4 month, RL_INT4 day));
RL_BOOL	Jul_IsLeapYear RL_PROTO((RL_INT4 year));
RL_INT4	Jul_YearDays   RL_PROTO((RL_INT4 year));
RL_INT4	Jul_MonthDays  RL_PROTO((RL_INT4 year, RL_INT4 month));
RL_INT4	Jul_DUTCofJDN  RL_PROTO((RL_INT4 jdn));
RL_INT4	Jul_JDNofDUTC  RL_PROTO((RL_INT4 dutc));

/*******************************************************************************
* format.c
*******************************************************************************/

void	Jul_FormatDate RL_PROTO((RL_INT4 dutc, RL_CHAR *format,
	                         RL_CHAR *string));
RL_INT4	Jul_FormatTime RL_PROTO((RL_FLT8 secs, RL_BOOL isleap, RL_CHAR *format,
	                         RL_CHAR *string));
void	Jul_FormatPDS  RL_PROTO((RL_INT4 dutc, RL_FLT8 secs, RL_INT4 ndigits,
	                         RL_BOOL useZ, RL_CHAR *string));
void	Jul_FormatSQL  RL_PROTO((RL_INT4 dutc, RL_FLT8 secs, RL_CHAR *string));

/*******************************************************************************
* juldates.c
*******************************************************************************/

RL_FLT8	Jul_TAIofJD    RL_PROTO((RL_FLT8 jd,  RL_INT4 type));
RL_FLT8	Jul_JDofTAI    RL_PROTO((RL_FLT8 tai, RL_INT4 type));
RL_FLT8	Jul_TAIofMJD   RL_PROTO((RL_FLT8 mjd, RL_INT4 type));
RL_FLT8	Jul_MJDofTAI   RL_PROTO((RL_FLT8 tai, RL_INT4 type));

/*******************************************************************************
* leapsecs.c
*******************************************************************************/

RL_INT4	Jul_LeapSecs  RL_PROTO((RL_INT4 dutc));
RL_BOOL	Jul_IsLeapDay RL_PROTO((RL_INT4 dutc));
RL_INT4	Jul_DaySecs   RL_PROTO((RL_INT4 dutc));
RL_BOOL	Jul_InitLeaps RL_PROTO((RL_CHAR *leapfile));

/*******************************************************************************
* parser.c
*******************************************************************************/

RL_BOOL	Jul_ParseDT   RL_PROTO((RL_CHAR *string, RL_CHAR *pref,
	                        RL_INT4 *dutc, RL_FLT8 *secs));
RL_BOOL	Jul_ParseDate RL_PROTO((RL_CHAR *string, RL_CHAR *pref, RL_INT4 *dutc));
RL_BOOL	Jul_ParseTime RL_PROTO((RL_CHAR *string, RL_BOOL isleap,
	                        RL_FLT8 *secs));

/*******************************************************************************
* seconds.c
*******************************************************************************/

RL_FLT8	Jul_SecofDHMS RL_PROTO((RL_INT4 day, RL_INT4 hour, RL_INT4 minute,
	                        RL_FLT8 second));
void	Jul_DSofSec   RL_PROTO((RL_FLT8 secs, RL_INT4 *day, RL_FLT8 *second));
void	Jul_DHMSofSec RL_PROTO((RL_FLT8 secs, RL_INT4 *day, RL_INT4 *hour,
	                        RL_INT4 *minute, RL_FLT8 *second));
void	Jul_HMSofSec  RL_PROTO((RL_FLT8 secs, RL_INT4 *hour, RL_INT4 *minute,
	                        RL_FLT8 *second));

/*******************************************************************************
* tai_et.c
*******************************************************************************/

RL_FLT8 Jul_TAIofET RL_PROTO((RL_FLT8 et));
RL_FLT8 Jul_ETofTAI RL_PROTO((RL_FLT8 tai));

/*******************************************************************************
* utc_tai.c
*******************************************************************************/

RL_FLT8	Jul_TAIofDUTC RL_PROTO((RL_INT4 dutc));
RL_INT4	Jul_DUTCofTAI RL_PROTO((RL_FLT8 tai, RL_FLT8 *secs));

/*******************************************************************************
</PRE></BODY></HTML>*/
