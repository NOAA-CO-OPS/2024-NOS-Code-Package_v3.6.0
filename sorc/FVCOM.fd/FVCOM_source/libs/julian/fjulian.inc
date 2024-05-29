!c<HTML><HEAD><TITLE> fjulian.inc </TITLE></HEAD><BODY><PRE>
!c*******************************************************************************
!c fjulian.inc -- FORTRAN include file for Julian Toolkit
!c
!c Mark Showalter, PDS Rings Node 11/95
!c*******************************************************************************

!c Constants
	integer*4	FJUL_UTC_TYPE, FJUL_TAI_TYPE, FJUL_ET_TYPE
	parameter	(FJUL_UTC_TYPE = 0)
	parameter	(FJUL_TAI_TYPE = 1)
	parameter	(FJUL_ET_TYPE  = 2)

!c***********************************************************************
!c jul_dutc.c
!c***********************************************************************

	integer*4	FJul_DUTCofYMD, FJul_YearDays, FJul_MonthDays, FJul_DUTCofJDN, FJul_JDNofDUTC 
	logical*4	FJul_IsLeapYear

!c***********************************************************************
!c jul_format.c
!c***********************************************************************

	integer*4	FJul_FormatTime

!c***********************************************************************
!c jul_jd.c
!c***********************************************************************

	real*8		FJul_TAIofJD, FJul_JDofTAI, FJul_TAIofMJD, FJul_MJDofTAI

!c***********************************************************************
!c jul_leapsecs.c
!c***********************************************************************

	integer*4	FJul_LeapSecs, FJul_DaySecs
	logical*4	FJul_IsLeapDay, FJul_InitLeaps

!c***********************************************************************
!c jul_parse.c
!c***********************************************************************

	logical*4	FJul_ParseDT, FJul_ParseDate, FJul_ParseTime

!c***********************************************************************
!c jul_secs.c
!c***********************************************************************

	real*8		FJul_SecofDHMS

!c***********************************************************************
!c jul_tai_et.c
!c***********************************************************************

	real*8		FJul_TAIofET, FJul_ETofTAI

!c***********************************************************************
!c jul_utc_tai.c
!c***********************************************************************

	integer*4	FJul_DUTCofTAI
	real*8		FJul_TAIofDUTC

!c*******************************************************************************
!c</PRE></BODY></HTML>
