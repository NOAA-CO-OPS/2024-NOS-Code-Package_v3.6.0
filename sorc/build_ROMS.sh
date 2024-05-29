#!/bin/sh
#HOMEnos=/lfs/h1/nos/nosofs/noscrub/$LOGNAME/packages/nosofs.v3.5.0
#HOMEnos=/lfs/h1/nos/nosofs/noscrub/$LOGNAME/code_delivered2NCO/nosofs.v3.5.0_4NCO
cd ..
HOMEnos=`pwd`

BUILD_VERSION_FILE=$HOMEnos/versions/build.ver
if [ -f $BUILD_VERSION_FILE ]; then
 . $BUILD_VERSION_FILE
else
   echo " Build Version File $BUILD_VERSION_FILE does not exist **"
   exit
fi

export HOMEnos=${HOMEnos:-${PACKAGEROOT:?}/nosofs.${nosofs_ver:?}}

export COMP_F=ftn
export COMP_F_MPI90=ftn
export COMP_F_MPI=ftn
export COMP_ICC=cc
export COMP_CC=cc
export COMP_CPP=cpp
export COMP_MPCC=cc

#module purge
#printenv SHELL
#module purge
#module load envvar/$envvars_ver
## Loading Intel Compiler Suite
#module load PrgEnv-intel/${PrgEnv_intel_ver}
#module load craype/${craype_ver}
#module load intel/${intel_ver}
#module load cray-mpich/${cray_mpich_ver}
#module load cray-pals/${cray_pals_ver}
##Set other library variables
##module load netcdf/${netcdf_ver}
##module load hdf5/${hdf5_ver}
#module load bacio/${bacio_ver}
#module load w3nco/${w3nco_ver}
#module load w3emc/${w3emc_ver}
#module load g2/${g2_ver}
#module load zlib/${zlib_ver}
#module load libpng/${libpng_ver}
#module load bufr/${bufr_ver}
#module load jasper/${jasper_ver}
##
##Set other library variables
#module load netcdf/${netcdf_ver}
#module load hdf5/${hdf5_ver}
#module load subversion/${subversion_ver}


##module purge
##printenv SHELL
##module use $HOMEnos/modulefiles
##module load nosofs
##module list 2>&1

export SORCnos=$HOMEnos/sorc
export EXECnos=$HOMEnos/exec
export LIBnos=$HOMEnos/lib

#if [ ! -s $EXECnos ]
#then
#  mkdir -p $EXECnos
#fi
#export LIBnos=$HOMEnos/lib

#if [ ! -s $LIBnos ]
#then
#  mkdir -p $LIBnos
#fi

#cd $SORCnos/nos_ofs_utility.fd
#rm -f *.o *.a
#gmake -f makefile

#if [ -s $SORCnos/nos_ofs_utility.fd/libnosutil.a ]
#then
#  chmod 755 $SORCnos/nos_ofs_utility.fd/libnosutil.a
#  mv $SORCnos/nos_ofs_utility.fd/libnosutil.a ${LIBnos}
#fi
#gmake clean

#cd $SORCnos/nos_ofs_reformat_ROMS_CTL.fd
#rm -f *.o *.a
#gmake -f makefile


#  Compile ocean model of ROMS-based OFS 
cd $SORCnos/ROMS.fd
./COMPILE_ROMS.sh

##  Compile ocean model of FVCOM-based OFS
#cd $SORCnos/FVCOM.fd
#./COMPILE_FVCOM.sh

