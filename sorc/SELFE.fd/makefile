################################################################################
# Parallel SELFE Makefile
#
# User makes environment settings for particular OS / PLATFORM / COMPILER / MPI
# below. These settings are based on the following options.
#
# Compiler name:
#   FCS: Serial compiler (for utilities)
#   FCP: Parallel compiler
#   FLD: Linker (in general same as parallel compiler)
#
# Compilation flags
#   FCSFLAGS: Flags for serial compilation
#   FCPFLAGS: Flags for parallel compilation (including all pre-processing flags)
#   FLDFLAGS: Flags for linker (e.g., -O2)
#
# Preprocessor flags:
#   DEBUG: Enable debugging code
#   ORDERED_SUM: Enable globally ordered sums & dot-products for bit reproducibility
#     of state quantities independent of number of processors (note: this can
#     significantly degrade performance);
#   INCLUDE_TIMING: Enable wallclock timing of code (note: this can have slight
#     effect on performance);
#   MPI_VERSION = 1 or 2: Version of MPI (try 2 first, if compile fails due to mpi
#     related errors then switch to version 1;
#
# Libraries (needed for parallel code)
#   MTSLIBS: Flags for linking ParMeTiS/MeTiS libaries
#   ALTLIBS: Flags for linking alternate solver libraries (LAPACK or ITPACK,
#            these are just for testing)
################################################################################


################################################################################
# Comment out the following line if you don't want the build dependent
# upon the Makefile, otherwise use the name of the Makefile
################################################################################
#MAKEFILE = Makefile

#ENV         = LONI
#ENV         = VIMS
#ENV         = XEON
#ENV         = CANOPUS
#ENV         = SIRIUS
ENV         = NCEP

################################################################################
# Executable
################################################################################
EXEC   = selfe_creofs

################################################################################
# Define pre-processing flags.
# Comment out unwanted flags.
################################################################################
# -DSELFE always on
#epm  PPFLAGS = -DSELFE 
#epm next line added
#epmeddy        PPFLAGS :=  -WF,-DSELFE,-DINCLUDE_TIMING,-DMPIVERSION=2
        PPFLAGS :=  -DSELFE -DINCLUDE_TIMING -DMPIVERSION=2

# Wave models need a coupler
# SWAN (not active)
#  USE_SWAN = yes
#  PPFLAGS := $(PPFLAGS) -DUSE_SWAN
  #MPI version
#  SWAN_DIR = /disk/ambcs03/0/yinglong/SWAN/swan4072_MPI/

# Precip/evaporation model
#  PPFLAGS := $(PPFLAGS) -DPREC_EVAP 

# MM5 in heat exchange model
#  PPFLAGS := $(PPFLAGS) -DMM5

# GOTM turbulence closure model
#  USE_GOTM = yes
#  PPFLAGS := $(PPFLAGS) -DUSE_GOTM 

# Comment out if timer report is not needed.
#  PPFLAGS := $(PPFLAGS) -DINCLUDE_TIMING

# Debug mode (more time consuming)
#  PPFLAGS := $(PPFLAGS) -DDEBUG 

# Wind wave model WWM
#  USE_WWM = yes
#  PPFLAGS := $(PPFLAGS) -DUSE_WWM

# Harmonic analysis tool
#  USE_HA = yes
#  PPFLAGS := $(PPFLAGS) -DUSE_HA

# Select only _one_ model from below

# Ecological model - NAPZD Spitz (internal use only)
#  USE_NAPZD = yes
#  PPFLAGS := $(PPFLAGS) -DUSE_NAPZD

# Or:
# Ecological model
#  USE_ECO = yes
#  PPFLAGS := $(PPFLAGS) -DUSE_ECO

# Or:
# Sediment model 
#  USE_SED = yes
#  PPFLAGS := $(PPFLAGS) -DUSE_SED

  ##Bedload 
#  PPFLAGS := $(PPFLAGS) -DBEDLOAD

  ##Bedload - MPM model
#  PPFLAGS := $(PPFLAGS) -DBEDLOAD_MPM

  ##Suspended load
#  PPFLAGS := $(PPFLAGS) -DSUSPLOAD

  ## Morphology
#  PPFLAGS := $(PPFLAGS) -DSED_MORPH

  ## Choose one drag formulation from the following 3 choices
#  PPFLAGS := $(PPFLAGS) -DUV_LOGDRAG
  #PPFLAGS := $(PPFLAGS) -DUV_QDRAG
  #PPFLAGS := $(PPFLAGS) -DUV_LDRAG

# Or:
# Oil spill model (not active)
#  USE_OIL = yes
#  PPFLAGS := $(PPFLAGS) -DUSE_OIL

# Add -DNO_TR_15581 like below for allocatable array problem in sflux_subs.F90
# PPFLAGS := $(PPFLAGS) -DNO_TR_15581

# Obsolete flags: use USE_WRAP flag to avoid problems in ParMetis lib (calling C from FORTRAN)
# PPFLAGS := $(PPFLAGS) -DUSE_WRAP 

################################################################################
# Environment for NCEP HPC
################################################################################
ifeq ($(ENV),NCEP)
  FCS = $(COMP_F)
  FCP = $(COMP_F_MPI90) 
  FLD = $(FCP)
  #####Flags for debug version
  #PPFLAGS = -DDEBUG -DORDERED_SUM -DINCLUDE_TIMING -DMPIVERSION=2
  #FCSFLAGS = -g
  #FCPFLAGS = $(PPFLAGS) -g
  #FLDFLAGS = -g
  #####Flags for optimized version
#epm  FCSFLAGS = -O2 -Bstatic -assume byterecl #for serial code
#epm  FCPFLAGS = $(PPFLAGS) -O2 -Bstatic -assume byterecl #MPI code
  FCSFLAGS =  #for serial code
  FCPFLAGS = $(PPFLAGS) -O3  #MPI code
  FLDFLAGS =  -O3 #for final linking of object files
  #####Libraries
  MTSLIBS =    ###-L./ParMetis-3.1-64bit -lparmetis -lmetis
#  MTSLIBS := $(MTSLIBS) -L/usrx/local/NetCDF/4.2/serial/lib -lnetcdff
#AJ NETCDF4.0 library on CCS/NCEP
#  CDFLIBS = -L/usrx/local/NetCDF/4.2/serial/lib -lnetcdff
#  CDFMOD = -I/usrx/local/NetCDF/4.2/serial/include # modules for netcdf
NETCDF_LDFLAGS = -L${NETCDF}/lib -lnetcdff
MTSLIBS := $(MTSLIBS) $(NETCDF_LDFLAGS)
#  CDFLIBS = -L${NETCDF_LIB} -lnetcdff
#  CDFMOD = -I${NETCDF_INC}
  CDFLIBS = -L${NETCDF}/lib -lnetcdff
  CDFMOD = -I${NETCDF}/include

endif


################################################################################
# Environment for LONI / 64 bit /  Intel Compiler / MPICH2 (LONI)
################################################################################
ifeq ($(ENV),LONI)
  FCP = mpif90 #MPI compiler
  FLD = $(FCP)
# MPI vserion (1 or 2)
  PPFLAGS := $(PPFLAGS) -DMPIVERSION=2
  FCPFLAGS = $(PPFLAGS) -O2 -Bstatic -assume byterecl  #MPI code
  FLDFLAGS = -O2   #for final linking of object files
  #####Libraries
  MTSLIBS = -L/home/hvwang/ParMetis-3.1-64bit -lparmetis -lmetis
  CDFLIBS = -L/usr/local/packages/netcdf/4.0/intel-11.1/lib -lnetcdf
  CDFMOD = -I/usr/local/packages/netcdf/4.0/intel-11.1/include # modules for netcdf
  ifdef USE_GOTM
    GTMMOD =  -I/home/users/yinglong/GOTM/gotm-3.2.5/32bit/gotm-3.2.5/modules/IFORT/ #modules
    GTMLIBS = -L/home/users/yinglong/GOTM/gotm-3.2.5/32bit/gotm-3.2.5/lib/IFORT/ -lturbulence_prod  -lutil_prod
  else
    GTMMOD =
    GTMLIBS =
  endif
endif

################################################################################
# Environment for TYPHOON cluster of VIMS-College of William and Mary
################################################################################
ifeq ($(ENV),VIMS)
  FCP = mpif90 #MPI compiler
  FLD = $(FCP)
# MPI vserion (1 or 2)
  PPFLAGS := $(PPFLAGS) -DMPIVERSION=2
  FCPFLAGS = $(PPFLAGS) -O2 -C -Bstatic -assume byterecl  #MPI code
  FLDFLAGS = -O2 -C #for final linking of object files
  #####Libraries
  MTSLIBS = -L/home/hvwang/ParMetis-3.1-64bit -lparmetis -lmetis
  CDFLIBS = -L/usr/local/packages/netcdf/4.0/intel-11.1/lib -lnetcdf
  CDFMOD = -I/usr/local/packages/netcdf/4.0/intel-11.1/include # modules for netcdf
  ifdef USE_GOTM
    GTMMOD =
    GTMLIBS =
  else
    GTMMOD =
    GTMLIBS =
  endif
endif

################################################################################
# Environment for Linux / 32 bit /  Intel Compiler / MPICH
################################################################################
ifeq ($(ENV),XEON)
  FCP = mpif90 #MPI compiler
  FLD = $(FCP)
# MPI vserion (1 or 2) 
  PPFLAGS := $(PPFLAGS) -DMPIVERSION=1 
  FCPFLAGS = $(PPFLAGS) -O2 -Bstatic -assume byterecl #MPI code
  FLDFLAGS = -O2 #for final linking of object files
  #####Libraries
  MTSLIBS = -L/home/users/yinglong/SELFE/ParMetis-3.1-32bit -lparmetis -lmetis
  CDFLIBS = -L/usr/local/netcdf/lib -lnetcdf 
  CDFMOD = -I/usr/local/netcdf/include # modules for netcdf
  ifdef USE_GOTM
    GTMMOD =  -I/home/users/yinglong/GOTM/gotm-3.2.5/32bit/gotm-3.2.5/modules/IFORT/ #modules
    GTMLIBS = -L/home/users/yinglong/GOTM/gotm-3.2.5/32bit/gotm-3.2.5/lib/IFORT/ -lturbulence_prod  -lutil_prod 
  else
    GTMMOD =
    GTMLIBS =
  endif
endif

################################################################################
# Environment for Linux / 64 bit /  Intel Compiler / MPICH (canopus)
################################################################################
ifeq ($(ENV),CANOPUS)
  #FCP = /home/yinglong/mpich2/bin/mpif90
  FCP = mpif90
  FLD = $(FCP)
# MPI vserion (1 or 2) 
  PPFLAGS := $(PPFLAGS) -DMPIVERSION=2 
  FCPFLAGS = $(PPFLAGS) -O2 -CB -Bstatic -assume byterecl #MPI code
  FLDFLAGS = -O2 #for final linking of object files
  #####Libraries
  MTSLIBS = -L/home/users/yinglong/SELFE/ParMetis-3.1-64bit/Intel -lparmetis -lmetis
  CDFLIBS = -L/usr/local/lib -lnetcdf 
  CDFMOD = -I/usr/local/include # modules for netcdf
  ifdef USE_GOTM
    GTMMOD =  -I/home/users/yinglong/GOTM/gotm-3.2.5/Intel64/modules/IFORT/ #modules
    GTMLIBS = -L/home/users/yinglong/GOTM/gotm-3.2.5/Intel64/lib/IFORT/ -lturbulence_prod  -lutil_prod 
  else
    GTMMOD =
    GTMLIBS =
  endif
endif

################################################################################
# Environment for SIRIUS cluster
################################################################################
ifeq ($(ENV),SIRIUS)
#  USE_WRAP = yes
  FCP = mpif90
  FLD = $(FCP)
# MPI vserion (1 or 2) 
  PPFLAGS := $(PPFLAGS) -DMPIVERSION=2 #-DUSE_WRAP
  #-CB is much slower to compile
  #FCPFLAGS = $(PPFLAGS) -O2 -CB -Bstatic -assume byterecl #MPI code; check bound
  FCPFLAGS = $(PPFLAGS) -O2 -Bstatic -assume byterecl #MPI code
  FLDFLAGS = -O2 #for final linking of object files
  #####Libraries
#  MTSLIBS = -L/share/apps/ParMetis/ -lparmetis -lmetis
#  MTSMOD = -I/share/apps/ParMetis/ #wrapc.c uses parmetis.h
  MTSLIBS = -L/home/users/yinglong/SELFE/ParMetis-3.1-Sep2010/ -lparmetis -lmetis
  CDFLIBS = -L/share/apps/netcdf/lib/ -lnetcdf -L/opt/intel/fce/10.1.015/lib/ -lirc
  CDFMOD = -I/share/apps/netcdf/include/ # modules for netcdf
  ifdef USE_GOTM
    GTMMOD =  -I/home/users/yinglong/GOTM/gotm-3.2.5/Intel64/modules/IFORT/ #modules
    GTMLIBS = -L/home/users/yinglong/GOTM/gotm-3.2.5/Intel64/lib/IFORT/ -lturbulence_prod  -lutil_prod
  else
    GTMMOD =
    GTMLIBS =
  endif
endif

################################################################################
# Object files; modules compile first; modules that depend on other modules compile last.
################################################################################
#Search path
VPATH = Hydro/ EcoSim/ Sediment/ WWM_Rolland/WWMII/ \
        /disk/ambcs03/0/yinglong/SELFE/from_Durski_Sept2009/V3.0c/ 

OBJS = elfe_glbl.o elfe_msgp.o 

ifdef USE_HA
  OBJS := $(OBJS) harm.o
endif

ifdef USE_ECO
  OBJS := $(OBJS) bio_param.o eclight.o biology.o bio_init.o ecosim.o read_ecoin.o specir.o
endif

ifdef USE_NAPZD
  OBJS := $(OBJS) biology_napzd.o napzd_spitz.o read_napzd_input.o
endif

ifdef USE_SED
  OBJS := $(OBJS) sed_param.o sed_mod.o ocean_mod.o init_sed.o read_sed_input.o sed_init.o \
          set_vbc.o stress.o sediment_v5.o
endif

ifdef USE_WWM
   OBJS := $(OBJS) datapl.o lineqs.o wwmpks.o mjdv2.o intple.o indiss.o \
       mod_trianglebc.o SdsSin.o femspe.o gridcf.o initio.o input.o bdcons.o \
       dsintegrate.o nums1d.o numtheta.o numsigma.o \
       fluctinit.o fluctdrv.o fluctser.o fluctlim.o diffrac.o \
       sourc0.o sourc1.o waminput.o sourc2.o sourc3.o spectr.o \
       output.o compute.o wwm_radiation.o wwm3r1.o \
       matvec.o itaux.o iters.o ilut.o blasaux.o \
       blasaux.o
endif

OBJS := $(OBJS) grid_subs.o elfe_main.o bktrk_subs.o solver_subs.o \
        io_subs.o misc_subs.o transport.o sflux_9c.o lap.o 

################################################################################
# Library list
################################################################################
LIBS = $(MTSLIBS) $(CDFLIBS) $(GTMLIBS)

################################################################################
# Build commands for SELFE
################################################################################
#ifdef USE_SWAN
#  FLDFLAGS := $(FLDFLAGS) -ipo -xW -mp1 -W0 -assume byterecl -traceback
#endif

$(EXEC): $(OBJS) $(MAKEFILE)
	$(FLD) $(FLDFLAGS) $(OBJS) $(LIBS) -o $(EXEC)
	rm -f *.o *.mod 

%.o: %.F90 $(MAKEFILE)
	$(FCP) -c $(FCPFLAGS) $(GTMMOD) $(CDFMOD) $<

################################################################################
# Build commands for utilities
################################################################################


################################################################################
# Clean up or Clobber
################################################################################
# Clean up objects and modules
clean:
	rm -f *.F90 *.c *.o *.mod $(EXEC) 
