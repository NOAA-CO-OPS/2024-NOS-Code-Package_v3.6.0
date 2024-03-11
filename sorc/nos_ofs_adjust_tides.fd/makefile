SHELL=/bin/sh
#
SRCS=nos_ofs_adjust_tides.f
OBJS=nos_ofs_adjust_tides.o

EXECnos=../../exec
LIBnos=../../lib

# Tunable parameters
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
FC = $(COMP_F) 

#use for extensive compilation error reporting
NETCDF_INC = -I$(NETCDF)/include
NETCDF_LIB = -L$(NETCDF)/lib -lnetcdff

## NOS COMF unitility library
NOS_UTILITY_LIB = -L$(LIBnos) -lnosutil
INCS = $(NETCDF_INC)
LIBS = $(NETCDF_LIB) $(NOS_UTILITY_LIB)

CMD =     $(EXECnos)/nos_ofs_adjust_tides 

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED
FFLAGS =  $(INCS) # -check all

all:	$(CMD)
$(CMD):	$(OBJS)
	$(FC) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)
	rm -f $(OBJS)
clean:
	rm -f $(OBJS)
clobber: clean
	rm -f $(CMD) $(CMD).prof
void:	clobber
	rm -f $(SRCS) makefile

