SHELL=/bin/sh
#
SRCS=nos_ofs_create_forcing_met_fvcom.f nos_ofs_met_write_netcdf_SELFE.f nos_ofs_met_write_netcdf_FVCOM.f w3fc07.f
OBJS=nos_ofs_create_forcing_met_fvcom.o nos_ofs_met_write_netcdf_SELFE.o nos_ofs_met_write_netcdf_FVCOM.o w3fc07.o

## specify NetCDF directory use netcdf 4.0.1 zaj 07/24/2009
EXECnos=../../exec
LIBnos=../../lib

# Tunable parameters
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling

FC = ${COMP_F} 
LDFLAGS =	
INC = -I${G2_INC4}
NETCDF_INC = -I$(NETCDF)/include
NETCDF_LIB = -L$(NETCDF)/lib -lnetcdff

## NOS COMF unitility library
NOS_UTILITY_LIB = -L$(LIBnos) -lnosutil
INCS  = $(INC) $(NETCDF_INC)

LIB  =   ${G2_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BUFR_LIB4}\
   $(NOS_UTILITY_LIB) ${JASPER_LIB} ${PNG_LIB}  ${Z_LIB} ${BACIO_LIB4}
LIBS  = $(NETCDF_LIB) $(LIB)

CMD =     $(EXECnos)/nos_ofs_create_forcing_met_fvcom 
PROFLIB = -lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED
FFLAGS = -O3  -extend-source  $(INCS) # -check all

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)
	rm -f $(OBJS)

clean:
	rm -f $(OBJS)

clobber: clean
	 rm -f $(CMD) $(CMD).prof

void:	clobber
	rm -f $(SRCS) makefile
	if [ -s $(CMD) ]
	then
	echo copy $CMD
	  cp -p  $(CMD) ../../exec/
	fi

