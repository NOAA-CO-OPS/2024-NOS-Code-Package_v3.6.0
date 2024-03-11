c<HTML><HEAD><TITLE> fjulian.for </TITLE></HEAD><BODY><PRE>
c*******************************************************************************
c fjulian.for -- FORTRAN wrappers for those Julian Library routines that
c                handle character strings.
c
c Mark Showalter, PDS Rings Node, December 1995
c Revised 6/98 by MRS to conform to RingLib naming conventions.
c Revised 10/04 by MRS to correct a minor bug that prevented compilation under
c	LINUX.
c*******************************************************************************

c*******************************************************************************
c FORTRAN string conversion routines for leapsecs.c
c*******************************************************************************

	logical*4 function FJul_InitLeaps(filename)
	character*(*)	filename

	integer		MAXLEN
	parameter	(MAXLEN = 255)
	integer*1	array(MAXLEN+1)
	logical*4	GJul_InitLeaps

	call FORT_Cstring(filename, array, MAXLEN+1)
	FJul_InitLeaps = GJul_InitLeaps(array)

	return
	end

c*******************************************************************************
c FORTRAN string conversion routines for format.c
c*******************************************************************************

	subroutine FJul_FormatDate(dutc, format, string)
	integer*4	dutc
	character*(*)	format, string

	integer		MAXLEN
	parameter	(MAXLEN = 255)
	integer*1	array1(MAXLEN+1), array2(MAXLEN+1)

	call FORT_Cstring(format, array1, MAXLEN+1)
	call GJul_FormatDate(dutc, array1, array2)
	call FORT_Fstring(array2, string)

	return
	end

c***************************************

	integer*4 function FJul_FormatTime(secs, isleap, format, string)
	real*8		secs
	logical*4	isleap
	character*(*)	format, string

	integer		MAXLEN
	parameter	(MAXLEN = 255)
	integer*1	array1(MAXLEN+1), array2(MAXLEN+1)
	integer*4	GJul_FormatTime

	call FORT_Cstring(format, array1, MAXLEN+1)
	FJul_FormatTime = GJul_FormatTime(secs, isleap, array1, array2)
	call FORT_Fstring(array2, string)

	return
	end

c***************************************

	subroutine FJul_FormatPDS(dutc, secs, ndigits, useZ, string)
	integer*4	dutc, ndigits
	real*8		secs
	logical*4	useZ
	character*(*)	string

	integer		MAXLEN
	parameter	(MAXLEN = 255)
	integer*1	array(MAXLEN+1)

	call GJul_FormatPDS(dutc, secs, ndigits, useZ, array)
	call FORT_Fstring(array, string)

	return
	end

c***************************************

	subroutine FJul_FormatSQL(dutc, secs, string)
	integer*4	dutc
	real*8		secs
	character*(*)	string

	integer		MAXLEN
	parameter	(MAXLEN = 255)
	integer*1	array(MAXLEN+1)

	call GJul_FormatSQL(dutc, secs, array)
	call FORT_Fstring(array, string)

	return
	end

c*******************************************************************************
c FORTRAN string conversion routines for parse.c
c*******************************************************************************

	logical*4 function FJul_ParseDT(string, pref, dutc, secs)
	character*(*)	string, pref
	integer*4	dutc
	real*8		secs

	integer		MAXLEN, PREFLEN
	parameter	(MAXLEN = 255)
	parameter	(PREFLEN = 3)
	integer*1	array1(MAXLEN+1), array2(PREFLEN+1)
	logical*4	GJul_ParseDT

	call FORT_Cstring(string, array1, MAXLEN+1)
	call FORT_Cstring(pref,   array2, PREFLEN+1)

	FJul_ParseDT = GJUL_PARSEDT(array1, array2, dutc, secs)

	return
	end

c***************************************

	logical*4 function FJul_ParseDate(string, pref, dutc)
	character*(*)	string, pref
	integer*4	dutc

	integer		MAXLEN, PREFLEN
	parameter	(MAXLEN = 255)
	parameter	(PREFLEN = 3)
	integer*1	array1(MAXLEN+1), array2(PREFLEN+1)
	logical*4	GJul_ParseDate

	call FORT_Cstring(string, array1, MAXLEN+1)
	call FORT_Cstring(pref,   array2, PREFLEN+1)
	FJul_ParseDate = GJul_ParseDate(array1, array2, dutc)

	return
	end

c***************************************

	logical*4 function FJul_ParseTime(string, isleap, secs)
	character*(*)	string
	logical*4	isleap
	real*8		secs

	integer		MAXLEN
	parameter	(MAXLEN = 255)
	integer*1	array(MAXLEN+1)
	logical*4	GJul_ParseTime

	call FORT_Cstring(string, array, MAXLEN+1)
	FJul_ParseTime = GJul_ParseTime(array, isleap, secs)

	return
	end

c*******************************************************************************
c</PRE></BODY></HTML>
