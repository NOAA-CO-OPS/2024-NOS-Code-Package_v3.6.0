#!/bin/sh
set -xa
########################################
## MANU_JNOS_OFS_AWS.sh  for development work only
#########################################
#
# This script is used to manually transfer OFS files from WCOSS onto CO-OPS AWS cloud bucket of s3://co-ops.nceptransfer
# # ./Manu_JNOS_OFS_AWS envir OFS YYYYMMDD CYC Platform
# # ./Manu_JNOS_OFS_AWS prod cbofs 20210715 06 ptmp
# #  envir - prod or dev. prod means transfer operational files from NCO run; "dev" means transfering files from development run
# #  OFS - OFS name, e.g. cbofs, dbofs, creofs, etc.
# #  YYYYMMDD - date to be transferred
# #  CYC - cycle of that day to be transferred
# #  Platform - ptmp for envir=dev; it is a dummy augment if envir=prod;
#
export PACKAGEROOT=/lfs/h1/nos/nosofs/noscrub/$LOGNAME/packages
. $PACKAGEROOT/nosofs.v3.7.2/versions/run.ver
export DATAROOT=/lfs/h1/nos/ptmp/$LOGNAME/work/${nosofs_ver}/${OFS}

module purge
module load envvar/$envvars_ver

# Loading Intel Compiler Suite
 module load PrgEnv-intel/${PrgEnv_intel_ver}
 module load craype/${craype_ver}
 module load intel/${intel_ver}
 module load cray-pals/${cray_pals_ver}
#Set other library variables
#module load libjpeg/${libjpeg_ver}
#module load netcdf/${netcdf_ver}
#module load hdf5/${hdf5_ver}
#module load subversion/${subversion_ver}
#module load python/${python_ver}
module load prod_envir/${prod_envir_ver}
module load prod_util/${prod_util_ver}
module load cfp/${cfp_ver}
module load nco/${nco_ver}
module load awscli/1.16.308
#
envir=$1
OFS=$2
PDY=$3
cyc=$4
platform=$5
job="manu_aws"

export DATAROOT=/lfs/h1/nos/${platform}/$LOGNAME/work/${nosofs_ver}/${OFS}

devname=$(cat /lfs/h1/ops/prod/config/prodmachinefile|grep backup|cut -d : -f2)
prodname=$(cat /lfs/h1/ops/prod/config/prodmachinefile|grep primary|cut -d : -f2)
echo $prodname $devname
devname=`echo $devname | cut -c 1-1`
prodname=`echo $prodname | cut -c 1-1`

########################################
# NOS_OFS_AWS  for development work only 
########################################
export HOMEnos=${HOMEnos:-${PACKAGEROOT}/nosofs.${nosofs_ver:?}}
######################################################
# The following two variable could be defined in the
# loadleveler submission script (the sms script), if
# not they will take the default values which is set
# for the NCO running enviroment
#######################################################
export RUN_ENVIR=${RUN_ENVIR:-nco}

###################################
# Specify NET and RUN Name and model
####################################
export OFS=${OFS}
export NET=${NET:-nosofs}
export RUN=${RUN:-$OFS}
export PREFIXNOS=${PREFIXNOS:-$OFS}
export platform=${platform:-H1}
###############################################################
# This block can be modified for different Production test
# environment. This is used for operational testings
###############################################################
export COMROOT=${COMROOT:-/lfs/h1/ops/$envir/com}
export DCOMROOT=${DCOMROOT:-/lfs/h1/ops/$envir/dcom}

export PS4='$SECONDS + '
date

####################################
# obtain unique process id (pid) and make temp directory
####################################
export pid=$$
export DATAROOT=${DATAROOT:-/lfs/h1/ops/prod/tmp}
#export DATA=${DATA:-${DATAROOT:?}/${jobid}}
export DATA=${DATA:-${DATAROOT:?}/nos_${OFS}_aws_${cyc}_$envir}
#rm -fr ${DATAROOT:?}/${OFS}_aws_${cyc}_${envir}*
echo "Check for existing working directory for nos_${OFS}_*_${cyc}_$envir"
ls -ltrd ${DATAROOT:?}/${OFS}_aws_${cyc}_${envir}* 2> /dev/null
isWKDirsExist=$?
if [ $isWKDirsExist -eq 0 ]
then
  echo "WARNING! Some of working directories for nos_${OFS} in ${cyc} $envir exist, move out before starting the prep job run!"
  renameTag=`date +%Y%m%d%H%M`
  for dir in `ls -d ${DATAROOT:?}/${OFS}_aws_${cyc}_${envir}*`
  do
#    mv $dir ${dir}_${renameTag}
#    echo "old working dir: $dir exists, will be removed"
    rm -fr $dir
  done
fi

#if [ $envir = prod ]; then
#  rm -rf ${DATAROOT}/*  
#fi
if [ ! -d $DATA ]
then
  mkdir -p $DATA
  cd $DATA
else
  cd $DATA
  rm -fr $DATA/*
fi
export cycle=t${cyc}z

############################################
#   Determine Job Output Name on System
############################################
export outid="LL$job"
export jobid="${outid}.o${pid}"
export pgmout="OUTPUT.${pid}"

####################################
# Specify Execution Areas
####################################
export EXECnos=${EXECnos:-${HOMEnos}/exec}
export PARMnos=${PARMnos:-${HOMEnos}/parm}
export USHnos=${USHnos:-${HOMEnos}/ush}
export SCRIPTSnos=${SCRIPTSnos:-${HOMEnos}/scripts}
export FIXnos=${FIXnos:-${HOMEnos}/fix/shared}
export FIXofs=${FIXofs:-${HOMEnos}/fix/${OFS}}

###########################################
# Run setpdy and initialize PDY variables
###########################################
#sh setpdy.sh
#. ./PDY
#export PDY=20210824
##############################################
# Define COM directories
##############################################
if [ $envir == "dev" ]; then
    export COMROOT=/lfs/h1/nos/ptmp/$LOGNAME/com	
    export COMIN=${COMIN:-${COMROOT}/${NET}/${nosofs_ver%.*}/${RUN}.${PDY}}
    export COMOUTroot=${COMOUTroot:-${COMROOT}/${NET}/${nosofs_ver%.*}}
    export COMOUT=${COMOUT:-${COMROOT}/${NET}/${nosofs_ver%.*}/${RUN}.${PDY}}
elif [ $envir == "prod" ]; then
    export COMROOT=/lfs/h1/ops/$envir/com
    export COMIN=${COMIN:-$(compath.py  ${NET}/${nosofs_ver})/${RUN}.${PDY}}
    export COMOUTroot=${COMOUTroot:-$(compath.py -o ${NET}/${nosofs_ver})}
    export COMOUT=${COMOUT:-$(compath.py -o ${NET}/${nosofs_ver})/${RUN}.${PDY}}
elif [ $envir == "para" ]; then
    export COMROOT=/lfs/h1/ops/$envir/com
    export COMIN=${COMIN:-$(compath.py  ${NET}/${nosofs_ver})/${RUN}.${PDY}}
    export COMOUTroot=${COMOUTroot:-$(compath.py -o ${NET}/${nosofs_ver})}
    export COMOUT=${COMOUT:-$(compath.py -o ${NET}/${nosofs_ver})/${RUN}.${PDY}}
#  export COMROOT=/lfs/h1/ops/prod/com
#  export COMIN=${COMIN:-$(compath.py  ${NET}/${nosofs_ver})/${RUN}.${PDY}}
#  export COMOUTroot=${COMOUTroot:-$(compath.py -o ${NET}/v3.4)}
#  export COMOUT=${COMOUT:-$(compath.py -o ${NET}/v3.4)/${RUN}.${PDY}}
fi
					    #  
export COMOUTcorms=${COMOUTcorms:-${COMOUTroot}/${RUN}.${PDY}}      # output directory
#mkdir -m 775 -p $COMOUT $COMOUTcorms

####  Log File To Sys Report  
##################################################################
export nosjlogfile=${PREFIXNOS}.${cycle}.${PDY}.jlogfile.log 
####  Log File To COMOUTcorms
export cormslogfile=${PREFIXNOS}.${cycle}.${PDY}.corms.log

if [ -s ${COMOUT}/$nosjlogfile ]; then
  cp -p ${COMOUT}/$nosjlogfile $DATA/.
fi
if [ -s ${COMOUT}/$cormslogfile ]; then
  cp -p ${COMOUT}/$cormslogfile $DATA/.
fi
#export nosjlogfile=${DATA}/${PREFIXNOS}.jlogfile.${PDY}.${cycle}.log 
####  Log File To COMOUTcorms
#export cormslogfile=${DATA}/${PREFIXNOS}.corms.${PDY}.${cycle}.log
env
  
tarfile=${OFS}.${PDY}${cyc}.${envir}.${nosofs_ver%.*}.tar

########################################################
# Execute the script.
########################################################
#   echo "push model data to CO-OPS ftp server of tidepool"
  $HOMEnos/ush/nos_ofs_aws_wcoss.sh
########################################################

exit

cat $pgmout

msg="ENDED NORMALLY."
postmsg "$nosjlogfile" "$msg"

##############################
# Remove the Temporary working directory
##############################
#cd $DATA_IN
#rm -rf ${DATA}

if [ $envir == 'dev' ]; then
  RPTDIR=/lfs/h1/nos/ptmp/$LOGNAME/rpt/${nosofs_ver}
  cp -p ${RPTDIR}/${OFS}_aws_${cyc}.out ${RPTDIR}/${OFS}_aws_${cyc}.out.${pbsid}
  cp -p ${RPTDIR}/${OFS}_aws_${cyc}.err ${RPTDIR}/${OFS}_aws_${cyc}.err.${pbsid}
elif [ $envir == 'prod' -o $envir == 'para' ]; then 
  RPTDIR=/lfs/h1/nos/ptmp/$LOGNAME/rpt/${nosofs_ver}
  cp -p ${RPTDIR}/${OFS}_aws_${cyc}_${envir}.out ${RPTDIR}/${OFS}_aws_${cyc}_${envir}.out.${pbsid}
  cp -p ${RPTDIR}/${OFS}_aws_${cyc}_${envir}.err ${RPTDIR}/${OFS}_aws_${cyc}_${envir}.err.${pbsid}  
fi

date


