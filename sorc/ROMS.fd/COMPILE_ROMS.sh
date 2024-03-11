#!/bin/sh
#export HOMEnos=/lfs/h1/nos/nosofs/noscrub/$LOGNAME/packages/nosofs.v3.7.2
#cd ../..
#HOMEnos=`pwd`
#export HOMEnos=${HOMEnos:-${PACKAGEROOT:?}/nosofs.${nosofs_ver:?}}
#export HOMEnos=/lfs/h1/nos/nosofs/noscrub/aijun.zhang/tmp/roms
BUILD_VERSION_FILE=$HOMEnos/versions/build.ver
if [ -f $BUILD_VERSION_FILE ]; then
 . $BUILD_VERSION_FILE
else
   echo " Build Version File $BUILD_VERSION_FILE does not exist **"
   exit
fi


export COMP_F=ftn
export COMP_F_MPI90=ftn
export COMP_F_MPI=ftn
export COMP_ICC=cc
export COMP_CC=cc
export COMP_CPP=cpp
export COMP_MPCC=cc

module purge
printenv SHELL
module purge
module load envvar/$envvars_ver
# Loading Intel Compiler Suite
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load craype/${craype_ver}
module load intel/${intel_ver}
module load cray-mpich/${cray_mpich_ver}
module load cray-pals/${cray_pals_ver}
#Set other library variables
#module load netcdf/${netcdf_ver}
#module load hdf5/${hdf5_ver}
module load bacio/${bacio_ver}
module load w3nco/${w3nco_ver}
module load w3emc/${w3emc_ver}
module load g2/${g2_ver}
module load zlib/${zlib_ver}
module load libpng/${libpng_ver}
module load bufr/${bufr_ver}
module load jasper/${jasper_ver}
#
#Set other library variables
module load netcdf/${netcdf_ver}
module load hdf5/${hdf5_ver}
module load subversion/${subversion_ver}

export SORCnos=$HOMEnos/sorc
export EXECnos=$HOMEnos/exec
export LIBnos=$HOMEnos/lib

#cd $SORCnos/ROMS.fd
#gmake clean
#./build_tbofs.sh

#exit

#  Compile ocean model of ROMS for CBOFS
cd $SORCnos/ROMS.fd
gmake clean
./build_cbofs.sh
if [ -s  cbofs_roms_mpi ]; then
  mv cbofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for DBOFS is not created'
fi

#  Compile ocean model of ROMS for CBOFS
cd $SORCnos/ROMS.fd
gmake clean
./build_dbofs.sh
if [ -s  dbofs_roms_mpi ]; then
  mv dbofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for DBOFS is not created'
fi

#  Compile ocean model of ROMS for TBOFS
cd $SORCnos/ROMS.fd
gmake clean
./build_tbofs.sh
if [ -s  tbofs_roms_mpi ]; then
  mv tbofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for TBOFS is not created'
fi

#  Compile ocean model of ROMS for GoMOFS
cd $SORCnos/ROMS.fd
gmake clean
./build_gomofs.sh
if [ -s  gomofs_roms_mpi ]; then
  mv gomofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for GOMOFS is not created'
fi


#  Compile ocean model of ROMS for CIOFS
cd $SORCnos/ROMS.fd
gmake clean
./build_ciofs.sh
if [ -s  ciofs_roms_mpi ]; then
  mv ciofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for CIOFS is not created'
fi

gmake clean

#  Compile ocean model of ROMS for WCOFS (which includes 3 models)
cd $SORCnos/ROMS.fd
gmake clean
./build_wcofs.sh
if [ -s  wcofs_roms_mpi ]; then
  mv wcofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for WCOFS is not created'
fi

gmake clean

# Compile WCOFS_FREE
cd $SORCnos/ROMS.fd
gmake clean
./build_wcofs_free.sh
if [ -s  wcofs_free_roms_mpi ]; then
  mv wcofs_free_roms_mpi $EXECnos/.
else
  echo 'roms executable for WCOFS_FREE is not created'
fi

# Compile WCOFS_DA
#cd $SORCnos/ROMS.fd/Lib/ARPACK
#gmake clean
#gmake  lib
#gmake  plib
#gmake clean
cd $SORCnos/ROMS.fd
gmake clean
./build_wcofs_da.sh
if [ -s  wcofs_da_roms_mpi ]; then
  mv wcofs_da_roms_mpi $EXECnos/.
else
  echo 'roms executable for WCOFS_DA is not created'
fi

gmake clean


