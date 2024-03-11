#
# Configuration file for fvcom2ncpar Code
#
            SHELL = /bin/sh

###
# NAME AND COMPILE OPTIONS HERE
###


          FC       = $(COMP_F)
          CC       = $(COMP_ICC)
          CPP      = $(COMP_CPP)
          CXX      = $(COMP_CC)

#		FC=ifort
		AR = ar rc
		RANLIB=ranlib

		# TRY gcc first... 
#		CC=icc
		#CC=icc		


            EXEC     = tconvert
            LIB      = libjulian
            JOPT     = -O3

#--------------------------------------------------------------------------
#  Preprocessing and Compilation Directives
#-------------------------------------------------------------------------

.SUFFIXES: .o .for .c 

#   -m64 -fPIC

.c.o:
	$(CC)  -c $(JOPT)  $*.c

.for.o:
	$(FC) -c $(JOPT) $*.for

#--------------------------------------------------------------------------
#  Filter Source Code.
#--------------------------------------------------------------------------


C_SRC = dates.c	format.c juldates.c leapsecs.c \
	parse.c seconds.c tai_et.c utc_tai.c \
	fortran.c rlerrors.c rlmemory.c

F_SRC = fjulian.for fstrings.for

EX_SRC = tconvert.for

OBJS = $(C_SRC:.c=.o) $(F_SRC:.for=.o)

#--------------------------------------------------------------------------
#  Linking Directives               
#--------------------------------------------------------------------------

$(LIB): 	$(OBJS)
		$(AR) $@.a $(OBJS)
		$(RANLIB) $@.a


$(EXEC):	
		make libjulian
		$(FC) $(JOPT) -o $(EXEC) $(EX_SRC) $(LIB).a

#--------------------------------------------------------------------------
#  Target to create dependecies.
#--------------------------------------------------------------------------

#depend:
#		mv makedepends makedepends.orig
#		makedepf90  $(SRCS) > makedepends


#--------------------------------------------------------------------------
#  Tar Up Code                           
#--------------------------------------------------------------------------

#tarfile:
#		tar cvf obc.tar *.F makefile 

#--------------------------------------------------------------------------
#  Cleaning targets.
#--------------------------------------------------------------------------

clean:
		/bin/rm -f *.o *.a tconvert

clobber:	
		make clean


#--------------------------------------------------------------------------
#  Common rules for all Makefiles - do not edit.
#--------------------------------------------------------------------------

emptyrule::

#--------------------------------------------------------------------------
#  Empty rules for directories that do not have SUBDIRS - do not edit.
#--------------------------------------------------------------------------

FILES = fjulian.inc libjulian.a tconvert

install::
	make tconvert
	cp fjulian.inc $(INSTALLDIR)/include
	cp libjulian.a $(INSTALLDIR)/lib
	cp tconvert $(INSTALLDIR)/bin


install.man::
	@echo "install.man in $(CURRENT_DIR) done"

Makefiles::

includes::
#include ../../make.inc
#include ./makedepends
# DO NOT DELETE
