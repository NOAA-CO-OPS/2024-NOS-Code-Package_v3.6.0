SHELL=/bin/sh
#
#
SRCS=nos_creofs_wl_offset_correction.f

OBJS=nos_creofs_wl_offset_correction.o

# Tunable parameters
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling

EXECnos=../../exec
LIBnos=../../lib

FC =  $(COMP_F)
#FC =           ifort 
#use for extensive compilation error reporting
#LDFLAGS = -p -bnoquiet -bloadmap:mug
LDFLAGS =	
#INC = -I/nwprod/lib/incmod/g2_4
INC = -I${G2_INC4}
NETCDF_INC = -I$(NETCDF)/include
NETCDF_LIB = -L$(NETCDF)/lib -lnetcdff

## NOS COMF unitility library
NOS_UTILITY_LIB = -L$(LIBnos) -lnosutil

INCS  = $(INC) $(NETCDF_INC)

LIB  =   ${G2_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BUFR_LIB4}\
   ${JASPER_LIB} ${PNG_LIB}  ${Z_LIB4} ${BACIO_LIB4}

LIBS  = $(NETCDF_LIB) $(NOS_UTILITY_LIB) $(LIB)
CMD =            $(EXECnos)/nos_creofs_wl_offset_correction
PROFLIB =       -lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED
FFLAGS =        -O3  $(INCS)
#FFLAGS =        -O3 -qnosave -qport=mod -qintsize=4 -qrealsize=4
#FFLAGS =	 -F
#FFLAGS =	 -Wf"-ez"

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS)  -o $(@) $(OBJS) $(LIBS)
	rm -f $(OBJS)
#	cp -p  $(CMD) ../../exec/

# Make the profiled version of the command and call it a.out.prof

#$(FC) $(LDFLAGS)  -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	rm -f $(OBJS)

clobber:	clean
	rm -f $(CMD) $(CMD).prof

void:	clobber
	rm -f $(SRCS) makefile
