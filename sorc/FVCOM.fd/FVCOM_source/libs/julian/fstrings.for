c<HTML><HEAD><TITLE> fstrings.for </TITLE></HEAD><BODY><PRE>
c*******************************************************************************
c fstrings.for -- routines for converting between FORTRAN and C character
c                 strings.
c
c Mark Showalter, PDS Rings Node, September 2002
c*******************************************************************************

c<A NAME="FORT_CSTRING"></A>
c*******************************************************************************
c$ Component_name:
c	FORT_CSTRING (fstrings.for)
c$ Abstract:
c	Converts a FORTRAN character string to a null-terminated byte array,
c	for passage to a C function.
c$ Keywords:
c	UTILITY, FORTRAN_C
c	FORTRAN, INTERNAL, SUBROUTINE
c$ Declarations:
c	subroutine	FORT_CSTRING(string, array, nbytes)
c	character*(*)	string
c	integer*1	array(*)
c	integer*4	nbytes
c$ Inputs:
c	string		character string to convert.
c	nbytes		dimensioned length of byte array.
c$ Outputs:
c	array(1...)	string of bytes with terminal null.
c$ Returns:
c	none
c$ Detailed_description:
c	This subroutine converts a FORTRAN character string to a null-terminated
c	byte array, for passage to a C function.  Blank characters at the end of
c	the character string are not considered significant.  The string is
c	truncated if necessary to fit into the array.
c$ External_references:
c	none
c$ Examples:
c	none
c$ Error_handling:
c	none
c$ Limitations:
c	The dimensioned length of the byte array must be at least one greater
c	than the effective length of the character string.
c$ Author_and_institution:
c	Mark R. Showalter
c	PDS Rings Node, NASA/Ames Research Center
c$ Version_and_date:
c	1.0: January 1994
c	1.1: September 2002
c$ Change_history:
c	1.1: Modified for compatibility with Absoft FORTRAN for Macintosh OS X.
c*******************************************************************************

	subroutine	FORT_CSTRING(string, array, nbytes)
	character*(*)	string
	integer*1	array(*)
	integer*4	nbytes

	integer		last, i

c Search for the last character actually used.
	do 100 last = len(string), 1, -1
		if (string(last:last) .ne. ' ') goto 101
100	continue
101	continue

c Truncate string if necessary
	if (last .gt. nbytes-1) last = nbytes-1

c Copy bytes from character string
	do 200 i = 1, last
		array(i) = ichar( string(i:i) )
200	continue

c Append null terminator
	array(last+1) = 0

	return
	end

c<A NAME="FORT_FSTRING"></A>
c*******************************************************************************
c$ Component_name:
c	FORT_FSTRING (fstrings.for)
c$ Abstract:
c	Converts a null-terminated byte array returned from a C function to a
c	FORTRAN character string.
c$ Keywords:
c	UTILITY, FORTRAN_C
c	FORTRAN, INTERNAL, SUBROUTINE
c$ Declarations:
c	subroutine	FORT_FSTRING(array, string)
c	integer*1	array(*)
c	character*(*)	string
c$ Inputs:
c	array(1...)	string of bytes with terminal null.
c$ Outputs:
c	string		FORTRAN character string.
c$ Returns:
c	none
c$ Detailed_description:
c	This subroutine converts a null-terminated byte array returned from a C
c	function to a FORTRAN character string.  The string is truncated if
c	necessary.
c$ External_references:
c	none
c$ Examples:
c	none
c$ Error_handling:
c	none
c$ Limitations:
c	none
c$ Author_and_institution:
c	Mark R. Showalter
c	PDS Rings Node, NASA/Ames Research Center
c$ Version_and_date:
c	1.0: January 1994
c$ Change_history:
c	none
c*******************************************************************************

	subroutine	FORT_FSTRING(array, string)
	integer*1	array(*)
	character*(*)	string

	integer		i

c Copy bytes into string, one by one
	do 100 i = 1, len(string)
		if (array(i) .eq. 0) goto 101
		string(i:i) = char(array(i))
100	continue
	return

c Pad remainder of string with blanks
101	continue
	string(i:) = ' '

	return
	end

c*******************************************************************************
c</PRE></BODY></HTML>
