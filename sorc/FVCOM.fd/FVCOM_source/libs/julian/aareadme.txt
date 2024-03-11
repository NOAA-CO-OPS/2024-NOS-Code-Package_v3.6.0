PDS_VERSION_ID                  = PDS3
RECORD_TYPE                     = STREAM

OBJECT                          = TEXT
  PUBLICATION_DATE              = 2005-02-21
  NOTE                          = "AAREADME file containing a complete
overview of the PDS Rings Node's Julian Library, version 1.3.3"
END_OBJECT                      = TEXT
END

**********************************************************************
                    PDS Rings Node Software Library
                      Part 2: THE JULIAN LIBRARY
                     Version 1.3.3, February, 2005

                           Mark R. Showalter
**********************************************************************

1. INTRODUCTION

The Julian Library comprises a set of C functions for manipulating
dates and times.  It includes routines for conversions between
Universal Time (UTC), Atomic Time (TAI), and Ephemeris Time (ET).  It
also performs conversions between calendar dates and Julian dates, and
interprets or formats dates and times in a variety of styles. Routines
are written in C and can be accessed by programs written in either C
or FORTRAN.

2. SUMMARY OF ROUTINES

Each routine in the Julian Library is documented extensively in the
source code.  The source files should be consulted for a detailed
description of how to use each routine.

For C programmers, all the Julian Library routines have names
beginning with "Jul_".  Some more basic components of the Rings Node
Library have names beginning with "RL_".  For FORTRAN programmers, the
routine names are the same except that they have an "F" prepended.

2.1 Definitions

A variety of variable names and terms are used throughout the toolkit.

DUTC    the integer number of days elapsed since January 1, 2000.
        Negative numbers refer to earlier dates.
secs    the floating-point (double precision) number of seconds since
        the beginning of a particular UTC day.  For most days, 0 <=
        secs < 86400; however, some days have leap seconds, in which
        case 0 <= secs < 86401.
year    An integer calendar year (CE).  Years are numbered
        sequentially so, since there was no year 0, 1 BCE is indicated
        by year=0, 2 BCE by year=-1, N BCE by year=-(N-1).
month   A numeric month value, 1=January, 2=February, ... 12=December.
        Within the toolkit, there is no restriction on the numeric
        range of month values; hence month=0 refers to December of the
        previous year and month=13 refers to January of the following
        year.
day     A numeric day value within a given month or year, where 1 is
        the first day.  There is no restriction on the numeric range
        of day values; hence January 32 is equivalent to February 1.
J2000   noon, January 1, 2000.  This is the epoch reference time for
        TAI and ET values, chosen for consistency with the SPICE
        Toolkit.
TAI     the number of seconds elapsed since J2000 Atomic Time (TAI).
        In the TAI time system, every day has exactly 86400 seconds;
        hence, times TAI and UTC differ by a whole number of seconds,
        where the number changes each time a leap second occurs.
ET      the number of seconds elapsed since J2000 Ephemeris Time (ET).
        This is the time frame on which planetary ephemerides are
        calculated (including the SPICE Toolkit).  It differs from TAI
        times by a constant plus small periodic terms related to
        relativistic variations in the ticks of an Earth-based clock.
JDN     a Julian day number, equal to the whole number of elapsed days
        since since January 1, 4713 BCE.
JD      a Julian date.  The integer part is the number Julian days
        elapsed since noon January 1, 4713 BCE; the fractional part
        gives the fraction of a day from one noon to the next.  Julian
        dates can be based on times UTC, TAI or ET.
MJD     a UTC Modified Julian Date, defined as JD - 2400000.5.

2.2 Calendar date conversions

The following routines are all defined in the file "dates.c".  They
are exact for all positive Julian Dates, i.e. for all dates since
January 1, 4713 BCE.

Jul_DUTCofYMD   converts year, month and day to DUTC.
Jul_YDofDUTC    converts DUTC to year and day-of-year.
Jul_YMDofDUTC   converts DUTC to year, month and day.
Jul_Gregorian   lets the user specify the date at which the Gregorian
                calendar was adopted.  This varied from country to
                country.
Jul_IsLeapYear  determines whether a given year is a leap year.
Jul_YearDays    returns the number of days in a given year.
Jul_MonthDays   returns the number of days in a given month of a given
                year.
Jul_DUTCofJDN   converts JDN to DUTC.
Jul_JDNofDUTC   converts DUTC to JDN.

2.3 Time conversion routines

The following routines are all defined in file "seconds.c".

Jul_SecofDHMS   converts days, hours, minutes and seconds to total
                seconds.
Jul_DSofSec     converts a number of seconds to days and seconds-into-
                day.
Jul_DHMSofSec   converts a number of seconds to days, hours, minutes
                and seconds (excluding possible leap seconds).
Jul_HMSofSec    converts a number of seconds to hours, minutes and
                seconds (including possible leap seconds).

2.4 UTC/TAI conversion routines

The following routines are all defined in file "utc_tai.c".

Jul_TAIofDUTC   converts DUTC to TAI.
Jul_DUTCofTAI   converts TAI to DUTC and remainder seconds.

The following routines are all defined in file "juldates.c".

Jul_TAIofJD     converts floating-point (double precision) JD to TAI.
Jul_JDofTAI     converts TAI to floating-point (double precision) JD.
Jul_TAIofMJD    converts floating-point (double precision) MJD to TAI.
Jul_MJDofTAI    converts TAI to floating-point (double precision) MJD.

These routines depend on an additional set of routines to keep track
of when leap seconds occur, found in file "leapsecs.c".

Jul_LeapSecs    returns the number of leap seconds elapsed before a
                given day.
Jul_IsLeapDay   determines whether a given day contains a leap second.
Jul_DaySecs     returns the number of seconds in a given day.
Jul_InitLeaps   reads a list of leap seconds from a file.  A sample
                file "leapsecs.lis" is provided.

2.5 TAI/ET conversion routines

The following pair of routines is found in the file "tai_et.c".
These are intended to match (almost) exactly the algorithms used by
the SPICE toolkit.

Jul_TAIofET     converts times ET to times TAI.
Jul_ETofTAI     converts times TAI to times ET.

2.6 Julian Day number conversion routines

The following routines are all found in file "juldates.c".

Jul_TAIofJD     converts from JD to TAI.
Jul_JDofTAI     converts from TAI to JD.
Jul_TAIofMJD    converts from MJD of TAI.
Jul_MJDofTAI    converts from TAI to MJD.

The Julian Dates converted by these routines can be based on the UTC,
TAI or ET time systems.

2.7 Date/time formatting and interpretation routines

Routines are available to format dates and times according to various
styles, and also to interpret character strings containing various
representations for dates and times.  The file "format.c" contains
routines for formatting dates and times:

Jul_FormatDate  formats a date in a specified style.
Jul_FormatTime  formats a time in a specified style.
Jul_FormatPDS   formats a date and time in PDS style.
Jul_FormatSQL   formats a date and time in SQL style.

The file "parse.c" contains routines for parsing dates and times.

Jul_ParseDT     interprets a character string as a date and time in
                arbitrary format.
Jul_ParseDate   interprets a character string as a date.
Jul_ParseTime   interprets a character string as a time.

The file "parsing.txt" summarizes the rules for how these strings
are interpreted; see also the source code.

2.8 FORTRAN-callable equivalent functions

The Julian Library routines listed above are written exclusively in C.
However, an equivalent FORTRAN-callable form is also provided for each
routine, generally in the same source file.  These routines have the
letter "F" prepended to their names.

2.9 Other Source Files

The following files contain additional source code in the form of
routines that are generally not to be called directly by the user.
Only one additional source file is specific to the Julian Library:

fjulian.for    FORTRAN interfaces to Julian Library routines that
               pass character strings as arguments.

In addition, these files contain generic RingLib routines.

rlmemory.c     RingLib routines to manage memory.
rlerrors.c     RingLib routines to handle errors.
ringlib.h      C header file for RingLib.

fortran.c      Ringlib routines to convert between FORTRAN integers
               and C pointers.
fstrings.for   Ringlib routines to convert string representations
               between FORTRAN and C.
fortran.h      C header file for fortran.c and fstrings.for.

3. EXAMPLE

Test program tconvert.for is a FORTRAN program provided to illustrate
the use of Julian Library routines.  It performs a variety of useful
time conversions.  You enter a UTC date and time in essentially
arbitrary format, and it prints out the time in a variety of
additional formats and time systems.  The program source should serve
as a reasonable example of how various Julian Library routines are
used.

4. USAGE

To use Julian Library routines from within a C program, one should
include the file "julian.h".  This file provides a declaration
prototype for each function.  Because some popular C compilers, such
as that provided on Suns, are not ANSI-standard, the include file is
designed to eliminate the prototypes on these systems.

Within the Julian Toolkit and the remainder of the Rings Node Software
Library, special data types are defined to "hide" some variations
among implementations of C from one platform to the next.  These
special data types are as follows:
    RL_INT4 = a four-byte signed integer.
    RL_BOOL = an integer taking on only values TRUE=1 and FALSE=0.
    RL_FLT4 = a single-precision (four-byte) floating point number.
    RL_FLT8 = a double-precision (eight-byte) floating point number.
    RL_CHAR = a one-byte character.
    RL_VOID = used in the context of "void *" as a pointer to a
              structure of arbitrary type.
These types are defined in the include file "ringlib.h", which is
included automatically by "julian.h".

For FORTRAN programmers, the include file "fjulian.inc" is provided;
it declares the type of every function within the toolkit and also
defines some useful parameters.

5. BUILDING INSTRUCTIONS

5.1 UNIX

This library has been tested on UNIX systems by DEC, Sun, Silicon
Graphics and LINUX.  To build the library and test binaries on a Sun,
type
    chmod +x makesun.com
    ./makesun.com
To build the library and binaries on a system by DEC or Silicon
Graphics, type
    chmod +x makedec.com
    ./makedec.com
To build the library and binaries on a Macintosh under OS X, type
    chmod +x makemac.com
    ./makemac.com
To build the library and binaries under LINUX using the g77 compiler,
type
    chmod +x makeg77.com
    ./makeg77.com
In each case, the script file "make*.com" will compile each file and
build an archive file called "julian.a". (Note that any pre-existent
file called "julian.a" should be removed first).  The sample program
binary is called "tconvert".

The commands "cc" and "f77" are used by these scripts (except makeg77,
where "g77" is called in place of "f77").  Be sure that your command
search path includes these before executing the script.

To use the library, include "julian.a" among the files that appear on
a cc or f77 command line.  Note that for C programs, you must also
include the "-lm" option to load in the C math libraries.

5.2 VMS

To build the library and executables on a Vax or Alpha using the VMS
operating system, type
    @MAKEVMS.COM
This will compile each file and build an object library called
JULIAN.OLB.  The sample program binary is called TCONVERT.EXE.

To use the library, include JULIAN/LIBRARY among the files in a LINK
command.  For programs written in FORTRAN, you must also include
SYS$LIBRARY:VAXCRTL/LIBRARY because the C libraries are not linked in
by default.

6. CHANGE HISTORY

Version 1.1 (11/96) contains no significant changes to the initial
source code. However, it includes the file "parsing.txt" which
describes in detail the string parsing rules used by "parse.c".
It also updates some of the comments in the source code.

Version 1.2 (7/97) incorporates the leap second of June 30, 1997 into
"leapsecs.c" and "leapsecs.lis".  It also fixes a minor bug in
Jul_FormatPDS().

Version 1.3 (6/98) contains extensive, mostly cosmetic changes, so
that naming conventions better conform to those of the rest of the
RingLib. It also incorporates HTML tags into the source code so that
code may be easily inspected online.

Version 1.3.1 (12/98) contains the new leap second of December 31,
1998.

Version 1.3.2 (2/04) includes support for Macintosh OS X. It also
provides a detached PDS label file "JULIAN.LBL" and a software catalog
file "SOFTWARE.CAT", making the library suitable for archiving on any
PDS volume.

Version 1.3.3 (2/05) includes support for LINUX and the g77 compiler.
