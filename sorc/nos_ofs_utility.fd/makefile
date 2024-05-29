INCLUDES = -I.
AR = ar rv
RANLIB = ranlib
# add path to c compiler 
CC = $(COMP_CC) 
FC = $(COMP_F)
# add c compiler optimization flags
COPTIONS = -D_UNDERSCORE
OPTFLAGS = -O2
CFLAGS = $(COPTIONS) $(OPTFLAGS) $(INCLUDES)
LD = $(CC) -L. 
LIB = libnosutil.a
OBJS =  stack.o stackpair.o volume.o interp_nneighbor.o interp_remesh.o interp_regrid.o utility.o nos_ofs_tideprediction.o equarg.o sal78.o


.c.o:
	$(CC) $(CFLAGS) -c stack.c stackpair.c volume.c
.F.o:

	$FC -c -o interp_nneighbor.o interp_nneighbor.f
	$FC -c -o interp_remesh.o interp_remesh.f
	$FC -c -o interp_regrid.o interp_regrid.f
	$FC -c -o utility.o utility.f
	$FC -c -o nos_ofs_tideprediction.o nos_ofs_tideprediction.f
	$FC -c -o equarg.o equarg.f
	$FC -c -o sal78.o sal78.f
      
$(LIB): $(OBJS)
	$(AR) $@ $(OBJS)
	$(RANLIB) $@
	mv $(LIB) ../../lib
	rm -f $(OBJS)

clean:
	rm -f *.o

realclean:
	rm -f *.o ; rm -f *.a
