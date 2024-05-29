#!/bin/sh

#HOMEnos=/lfs/h1/nos/nosofs/noscrub/$LOGNAME/packages/nosofs.v3.6.0
cd ../..
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
module load metis/${metis_ver}
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
module load petsc/${petsc_ver}

export SORCnos=$HOMEnos/sorc
export EXECnos=$HOMEnos/exec
export LIBnos=$HOMEnos/lib

if [ ! -s $EXECnos ]
then
  mkdir -p $EXECnos
fi
export LIBnos=$HOMEnos/lib

if [ ! -s $LIBnos ]
then
  mkdir -p $LIBnos
fi
set -x

cd  $SORCnos/FVCOM.fd/FVCOM_source/libs/julian
gmake clean
gmake -f makefile
if [ -s libjulian.a ]; then
  cp -p libjulian.a $LIBnos
else
  echo "WARNING: libjulian.a was not created"
fi
rm -f *.o
cd  $SORCnos/FVCOM.fd/FVCOM_source/libs/proj.4-master
gmake clean
./configure CC=cc FC=ftn CFLAGS='-DIFORT -g -w -O2' --prefix=$SORCnos/FVCOM.fd/FVCOM_source/libs/proj.4-master
gmake
gmake install
if [ -s ./lib64/libproj.a ]; then
  cp -p ./lib64/libproj.a $LIBnos
else
 echo "WARNING: ./lib64/libproj.a was not created"
fi

cd $SORCnos/FVCOM.fd/FVCOM_source/libs/proj4-fortran-master
gmake clean
./configure  CC=cc FC=ftn CFLAGS='-DIFORT -g -w -O2' proj4=$SORCnos/FVCOM.fd/FVCOM_source/libs/proj.4-master --prefix=$SORCnos/FVCOM.fd/FVCOM_source/libs/proj4-fortran-master
gmake
gmake install
if [ -s ./lib64/libfproj4.a ]; then
  cp -p ./lib64/libfproj4.a $LIBnos
else
  echo "WARNING: ./lib/libfproj4.a was not created"
fi


#export PETSC_DIR=/lfs/h1/nos/nosofs/noscrub/aijun.zhang/packages/noslibs.fd/petsc-3.20.0/3.20.0


cd $SORCnos/FVCOM.fd/FVCOM_source

gmake clean
gmake -f makefile_NGOFS2
if [ -s  fvcom_ngofs2 ]; then
  mv fvcom_ngofs2 $EXECnos/.
else
  echo 'fvcom executable is not created'
fi

gmake clean
gmake -f makefile_SFBOFS
if [ -s  fvcom_sfbofs ]; then
  mv fvcom_sfbofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi

gmake clean
gmake -f makefile_LEOFS
if [ -s  fvcom_leofs ]; then
  mv fvcom_leofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi

gmake clean
gmake -f makefile_LMHOFS
if [ -s  fvcom_lmhofs ]; then
  mv fvcom_lmhofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi

gmake clean
gmake -f makefile_LOOFS
if [ -s  fvcom_loofs ]; then
  mv fvcom_loofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi

gmake clean
gmake -f makefile_LSOFS
if [ -s  fvcom_lsofs ]; then
  mv fvcom_lsofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi

cd $SORCnos/FVCOM.fd/FVCOM_source
gmake clean
gmake -f makefile_SSCOFS
if [ -s  fvcom_sscofs ]; then
  mv fvcom_sscofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi

exit

