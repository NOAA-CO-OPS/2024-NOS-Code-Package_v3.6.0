c<HTML><HEAD><TITLE> fstrings.for </TITLE></HEAD><BODY><PRE>
c*******************************************************************************
c$ Component_name:
c	TCONVERT
c$ Abstract:
c	Sample program to perform a variety of time conversions.
c$ Keywords:
c	JULIAN, TIME
c	FORTRAN, SAMPLE_PROGRAM
c$ Declarations:
c	none
c$ Inputs:
c	none
c$ Outputs:
c	none
c$ Returns:
c	none
c$ Side_effects:
c	none
c$ Detailed_description:
c	This simple program illustrates the use of a variety of the Julian
c	Library routines.  The user is prompted repeatedly for a character
c	string, which the program interprets as a character string in nearly
c	arbitrary format.  The program then prints out a variety of inferred
c	values: day relative to J2000, PDS- and SQL-format strings, Atomic time
c	(TAI, seconds relative to J2000), Ephemeris time (ET, seconds relative
c	to J2000 TAI), Julian date, and Modified Julian date.
c
c	For further information on the acceptable formats of the input string,
c	see the documentation file "parsing.txt".
c$ External_references:
c	FJul_ParseDT(), FJul_FormatPDS(), FJul_FormatSQL(), FJul_TAIofDUTC(),
c	FJul_ETofTAI(), FJul_JDofTAI(), FJul_JDofTAI(), FJul_JDofTAI(),
c	FJul_MJDofTAI(), FJul_MJDofTAI(), FJul_MJDofTAI()
c$ Examples:
c	Here is a sample run of the program
c
c	YMD order preference: MDY	<--user input
c	Date/time: 7 1 76 0 1 2		<--user input
c	PDS format: "1976-07-01T00:01:02.000Z"
c	SQL format: "Jul 1, 1976 00:01:02:000"
c
c	 UTC day  =    -8584
c	 UTC secs =       62.000000
c	 TAI secs =  -741700723.000000
c	  ET secs =  -741700690.815904
c
c	 JD (UTC) =  2442960.500718
c	 JD (TAI) =  2442960.500891
c	 JD (ET)  =  2442960.501636
c	MJD (UTC) =    42960.000718
c	MJD (TAI) =    42960.000891
c	MJD (ET)  =    42960.001636
c
c	Date/time: mjd 42960.000718	<--user input
c	PDS format: "1976-07-01T00:01:02.035Z"
c	SQL format: "Jul 1, 1976 00:01:02:035"
c
c	 UTC day  =    -8584
c	 UTC secs =       62.035200
c	 TAI secs =  -741700722.964800
c	  ET secs =  -741700690.780704
c
c	 JD (UTC) =  2442960.500718
c	 JD (TAI) =  2442960.500892
c	 JD (ET)  =  2442960.501637
c	MJD (UTC) =    42960.000718
c	MJD (TAI) =    42960.000892
c	MJD (ET)  =    42960.001637
c
c	Date/time: <end-of-file>	<--user input
c$ Error_handling:
c	"String parsing failure!" is printed if the input string cannot be
c	parsed.
c$ Limitations:
c	none
c$ Author_and_institution:
c	Mark R. Showalter
c	PDS Rings Node
c	NASA/Ames Research Center
c$ Version_and_date:
c	1.3: June 1998 improved documentation.
c$ Change_history:
c	1.0: December 1995
c******************************************************************************/

	program TCONVERT

	include 'fjulian.inc'

	character*80	string, newstring, pref
	integer*4	dutc, LENGTH
	real*8		secs, tai, et
	logical*4	status

c***********************************************************************
c Read YMD preference string
c***********************************************************************

	write(*,10) 'YMD order preference: '
	read(*,11,end=999) pref
10	format(1x, a, $)
11	format(a)

c***********************************************************************
c Loop through time strings
c***********************************************************************

100	continue

c Read date/time string
	    write(*,10) 'Date/time: '
	    read(*,11,end=999) string

c Translate to a date and report error
	    status = FJul_ParseDT(string, pref, dutc, secs)
	    if (.NOT. status) then
	    	write(*,*) 'String parsing failure!'
	    	goto 100
	    end if

c Write various date/time formats
	    call FJul_FormatPDS(dutc, secs, 3, .TRUE., newstring)
	    write(*,24) 'PDS format:', newstring(1:LENGTH(newstring))

	    call FJul_FormatSQL(dutc, secs, newstring)
	    write(*,24) 'SQL format:', newstring(1:LENGTH(newstring))
	    write(*,25)

c Write UTC/Atomic/ephemeris time information
	    tai = FJul_TAIofDUTC(dutc) + secs
	    et = FJul_ETofTAI(tai)

	    write(*,21) ' UTC day  =', dutc
	    write(*,22) ' UTC secs =', secs
	    write(*,23) ' TAI secs =', tai
	    write(*,23) '  ET secs =', et
	    write(*,25)

c Write Julian Date information
	    write(*,22) ' JD (UTC) =',
     &				FJul_JDofTAI(tai, FJUL_UTC_TYPE)
	    write(*,22) ' JD (TAI) =',
     &				FJul_JDofTAI(tai, FJUL_TAI_TYPE)
	    write(*,22) ' JD (ET)  =',
     &				FJul_JDofTAI(et,  FJUL_ET_TYPE)

	    write(*,22) 'MJD (UTC) =',
     &				FJul_MJDofTAI(tai, FJUL_UTC_TYPE)
	    write(*,22) 'MJD (TAI) =',
     &				FJul_MJDofTAI(tai, FJUL_TAI_TYPE)
	    write(*,22) 'MJD (ET)  =',
     &				FJul_MJDofTAI(et,  FJUL_ET_TYPE)
	    write(*,25)

	    goto 100
999	continue

21	format(1x, a, i9)
22	format(1x, a, f16.6)
23	format(1x, a, f19.6)
24	format(1x, a, ' "', a, '"')
25	format(1x)

c***********************************************************************
c End of loop
c***********************************************************************

	stop
	end

c*******************************************************************************
c This internal function returns the active length of a character string
c*******************************************************************************

	integer function LENGTH(string)
	character*(*)	string

	integer		i

	do 100 i = len(string), 1, -1
		if (string(i:i) .ne. ' ') then
			LENGTH = i
			return
		end if
100	continue

	LENGTH = 0
	return
	end

c*******************************************************************************
c</PRE></BODY></HTML>
