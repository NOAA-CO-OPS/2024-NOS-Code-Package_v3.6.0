SHELL=/bin/sh
#
#
SRCS=nos_ofs_create_forcing_obc_fvcom_gl.f  nos_ofs_obc_write_netcdf_fvcom_gl.f nos_ofs_wlobc_offset_correction.f

OBJS=nos_ofs_create_forcing_obc_fvcom_gl.o  nos_ofs_obc_write_netcdf_fvcom_gl.o nos_ofs_wlobc_offset_correction.o

## specify NetCDF directory use netcdf 4.0.1 zaj 07/24/2009
 EXECnos=../../exec
 LIBnos=../../lib
# Tunable parameters
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling

FC =           $(COMP_F) 
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
   $(NOS_UTILITY_LIB) ${JASPER_LIB} ${PNG_LIB}  ${Z_LIB4} ${BACIO_LIB4}




LIBS  = $(NETCDF_LIB) $(LIB)



CMD =            $(EXECnos)/nos_ofs_create_forcing_obc_fvcom_gl 
PROFLIB =       -lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED
FFLAGS =     -O3 -extend-source $(INCS)
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
