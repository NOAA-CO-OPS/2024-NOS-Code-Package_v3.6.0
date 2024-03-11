#!/bin/sh
# Script Name:   nos_ofs_launch.sh                                           
#
# Purpose:
#      This script sets up OFS configuration (i.e. path names, file names,
#      restart time, forecast time, and other parameters, etc.). 
#      A main NOS OFS control file "${PREFIXNOS}.OFS.ctl" (OFS=cbofs for CBOFS) is used.
#      The hot_restart time of current cycle run
#      is determined by the most recently available hot restart file.
#      The following tasks are conducted in this script:
#  1. OFS configurations
#  2. copy static files into work directory. 
#  3. define all input and output file names.
#
# Contact: Aijun Zhang
#       Email: aijun.zhang@noaa.gov
#       Phone: (301)240-533-0591
#
# USAGE:  
#     OFS:        the name of NOS OFS, e.g. cbofs, dbofs, or tbofs
#     TIME_START: time of nowcast end and time of forecast start. If this argument
#                 exists, real_time will be specified as "FALSE".    
# Scripts Parameters: 
#
# Modules and Files referenced:
#   /nwprod/util: setup.sh
#                 setpdy.sh
#    FIXofs:   ${PREFIXNOS}.cbofs.ctl
        
#Condition codes:
#  this program exits if any previous cycle run for an OFS (e.g. cbofs) is 
#  still running. This is checked by running nos_ofs_control.sh. It is important
#  to run NOS OFS under developmental mode since OFS run jobs submitted through llsubmit
#  is sometimes are delayed. This might not be an issue for operational run.
#
# Remarks   - Can be run interactively, or from LLsubmit                      
#           - use NCEP PDY utility.                                           
# 
# Modification History:  
#   L. Zheng
#        Purpose: For Upgraded NGOFS Implementation
#
#### END of Unix Script DOC BLOCK--------------------------------------------------- 
set -x
if [ $# -lt 2 ];  then
  echo " ***Error: You must supply at least two arguments for model run " 
  echo "Example: exnos_ofs_launch.sh.sms cbofs nowcast"
  msg="FATAL ERROR: supply at least two arguments for model run "
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg" 
  exit 1
fi

export OFS=$1
export runtype=$2
echo 'The script nos_ofs_launch.sh has started at UTC' `date `
echo 'The script nos_ofs_launch.sh has started at UTC' `date ` >> $cormslogfile 
echo 'The script nos_ofs_launch.sh has started at UTC' `date ` >> $jlogfile 
echo 'The script nos_ofs_launch.sh has started at UTC' `date ` >> $nosjlogfile 

#################################################################
# Run setup to initialize working directory and utility scripts
# Run setpdy and initialize PDY variables
#################################################################

# set from system PDY variable for operations
export time_nowcastend=$PDY${cyc}

#------------------------------------------------'
#  COPY Files into Work Directory
#------------------------------------------------' 
if [ ! -s ${FIXofs}/$GRIDFILE ]; then
  echo '${FIXofs}/$GRIDFILE is not found'
  echo 'please provide model grid file of ${FIXofs}/$GRIDFILE'
  echo 'please provide model grid file of ${FIXofs}/$GRIDFILE' >> $cormslogfile
  msg="FATAL ERROR: ${FIXofs}/$GRIDFILE does not exist, FATAL ERROR!"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
  exit 1
else
  cp -p ${FIXofs}/$GRIDFILE $DATA/.
  export err=$?; err_chk
  echo "${FIXofs}/$GRIDFILE is copied into working dir"  
fi

if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]; then
  if [ ! -d $DATA/outputs ]; then
    mkdir -p $DATA/outputs
  fi
  if [ ! -d $DATA/sflux ]; then
    mkdir -p $DATA/sflux
  fi

  if [ -s ${FIXofs}/$GRIDFILE_LL ]; then
    cp -p ${FIXofs}/$GRIDFILE_LL $DATA/.
  fi
  if [ -s ${FIXofs}/$Nudging_weight ]; then
    cp -p ${FIXofs}/$Nudging_weight $DATA/.
  fi
fi

if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]; then
  if [ ! -s ${HOMEnos}/sorc/ROMS.fd/ROMS/External/varinfo.yaml ]; then
    echo "ROMS varinfo.yaml is not found"
    echo "please provide file of ${HOMEnos}/sorc/ROMS.fd/ROMS/External/varinfo.yaml"
    echo "please provide file of ${HOMEnos}/sorc/ROMS.fd/ROMS/External/varinfo.yaml" >> $cormslogfile
    msg="FATAL ERROR: ${HOMEnos}/ROMS.fd/sorc/ROMS/External/varinfo.yaml does not exist, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    exit 2
  else
    cp -p ${HOMEnos}/sorc/ROMS.fd/ROMS/External/varinfo.yaml $DATA/.
    export err=$?; err_chk
    echo " ${HOMEnos}/sorc/ROMS.fd/ROMS/External/varinfo.yaml was copied into working dir"
  fi
fi

if [ $CREATE_TIDEFORCING -gt 0 -a $DBASE_WL_NOW != "OBS" ]; then
  if [ ! -s ${FIXofs}/$HC_FILE_OBC ]; then
    echo '${FIXofs}/$HC_FILE_OBC is not found'
    echo 'please provide file of ${FIXofs}/$HC_FILE_OBC'
    echo 'please provide file of ${FIXofs}/$HC_FILE_OBC' >> $cormslogfile
    msg="FATAL ERROR: ${FIXofs}/$HC_FILE_OBC does not exist, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    exit 3
  else
    cp -p ${FIXofs}/$HC_FILE_OBC $DATA/.
    export err=$?; err_chk
  fi
fi

if [ ${OCEAN_MODEL} != "ROMS" -a ${OCEAN_MODEL} != "roms" ]; then
  if [ -d ${FIXofs}/$VGRID_CTL -o ! -s ${FIXofs}/$VGRID_CTL ]; then
    echo "${FIXofs}/$VGRID_CTL is not found"
    echo "please provide file of ${FIXofs}/$VGRID_CTL"
    echo "please provide file of ${FIXofs}/$VGRID_CTL" >> $cormslogfile
  else
    cp -p ${FIXofs}/$VGRID_CTL $DATA/.
  fi
fi
if [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]; then
  if [ -d ${FIXofs}/$STA_EDGE_CTL -o ! -s ${FIXofs}/$STA_EDGE_CTL ]; then
    echo "${FIXofs}/$STA_EDGE_CTL is not found"
    echo "please provide file of ${FIXofs}/$STA_EDGE_CTL"
    echo "please provide file of ${FIXofs}/$STA_EDGE_CTL" >> $cormslogfile
  elif [ -s ${FIXofs}/$STA_EDGE_CTL ]; then
    cp -p ${FIXofs}/$STA_EDGE_CTL $DATA/.
  fi
fi

if [ -d ${FIXofs}/$NWM_REACHID_FILE -o ! -s ${FIXofs}/$NWM_REACHID_FILE ]; then
  echo "WARNING: ${FIXofs}/$NWM_REACHID_FILE is not found"
  echo "Use USGS observations as river forcing conditions" 
  echo "WARNING: ${FIXofs}/$NWM_REACHID_FILE is not found" >> $cormslogfile
  echo "Use USGS observations as river forcing conditions" >> $cormslogfile
#  echo "please provide file of ${FIXofs}/$NWM_REACHID_FILE"
#  echo "please provide file of ${FIXofs}/$NWM_REACHID_FILE" >> $cormslogfile
elif [ -s ${FIXofs}/$NWM_REACHID_FILE ]; then
  echo "${FIXofs}/$NWM_REACHID_FILE is found"
  echo "Use NWM products as river forcing conditions"
  echo "Use NWM products as river forcing conditions" >> $cormslogfile
  cp -p ${FIXofs}/$NWM_REACHID_FILE $DATA/.
fi


export pgm=${FIXofs}/$STA_OUT_CTL"_copy"
. prep_step

if [ ! -s ${FIXofs}/$STA_OUT_CTL ]; then
  echo '${FIXofs}/$STA_OUT_CTL is not found'
  echo 'please provide ROMS station control file of ${FIXofs}/$STA_OUT_CTL'
  echo 'please provide ROMS station control file of ${FIXofs}/$STA_OUT_CTL' >> $cormslogfile
  msg="FATAL ERROR: ${FIXofs}/$STA_OUT_CTL does not exist, FATAL ERROR!"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
  exit 5
else
  if [ ! -d ${FIXofs}/$STA_OUT_CTL -a  -s ${FIXofs}/$STA_OUT_CTL ]; then
    cp -p ${FIXofs}/$STA_OUT_CTL $DATA/.
    export err=$?; err_chk
    echo "${FIXofs}/$STA_OUT_CTL was copied into working dir"
  fi   
fi

if [ ! -s ${FIXofs}/$RUNTIME_CTL -o ! -f ${FIXofs}/$RUNTIME_CTL ]; then
  echo '${FIXofs}/$RUNTIME_CTL is not found'
  echo 'please provide ROMS control file of ${FIXofs}/$RUNTIME_CTL'
  echo 'please provide ROMS control file of ${FIXofs}/$RUNTIME_CTL' >> $cormslogfile
  msg="FATAL ERROR: ${FIXofs}/$RUNTIME_CTL does not exist, FATAL ERROR!"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
  exit 4
else
  cp -p ${FIXofs}/$RUNTIME_CTL $DATA/. 
  export err=$?; err_chk
  echo "${FIXofs}/$RUNTIME_CTL was copied into working dir"
fi

if [ -s ${FIXofs}/$RUNTIME_CTL_FOR -a -f ${FIXofs}/$RUNTIME_CTL_FOR ]; then
  cp -p ${FIXofs}/$RUNTIME_CTL_FOR $DATA/.
  echo " ${FIXofs}/$RUNTIME_CTL_FOR was copied into working dir"
fi

BIO_MODULE=${BIO_MODULE:-0}
if [ $BIO_MODULE -eq 1 ]; then
  if [ ! -s ${FIXofs}/${PREFIXNOS}.bio.in ]; then
    echo '${FIXofs}/${PREFIXNOS}.bio.in is not found'
    echo 'please provide ROMS control file of ${FIXofs}/${PREFIXNOS}.bio.in'
    echo 'please provide ROMS control file of ${FIXofs}/${PREFIXNOS}.bio.in' >> $cormslogfile
    msg="FATAL ERROR: ${FIXofs}/${PREFIXNOS}.bio.in does not exist, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    exit 4
  else
    cp -p ${FIXofs}/${PREFIXNOS}.bio.in $DATA/. 
    export err=$?; err_chk
    echo "${FIXofs}/${PREFIXNOS}.bio.in was copied into working dir"
  fi

  if [ ! -s ${FIXofs}/${RESPIRATE_RATE} ]; then
    echo "${FIXofs}/${RESPIRATE_RATE} is not found"
    echo "please provide ROMS control file of ${FIXofs}/${RESPIRATE_RATE}"
    echo "please provide ROMS control file of ${FIXofs}/${RESPIRATE_RATE}" >> $cormslogfile
    msg="FATAL ERROR: ${FIXofs}/${RESPIRATE_RATE} does not exist, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    exit 4
  else
    cp -p ${FIXofs}/${RESPIRATE_RATE} $DATA/. 
    export err=$?; err_chk
    echo "${FIXofs}/${RESPIRATE_RATE} was copied into working dir"
  fi
fi  

TS_NUDGING=${TS_NUDGING:-0}
if [ $TS_NUDGING -eq 1 ]; then
  if [ ! -s ${FIXofs}/${PREFIXNOS}.nudgcoef.nc ]; then
    echo '${FIXofs}/${PREFIXNOS}.nudgcoef.nc is not found'
    echo 'please provide ROMS control file of ${FIXofs}/${PREFIXNOS}.nudgcoef.nc'
    echo 'please provide ROMS control file of ${FIXofs}/${PREFIXNOS}.nudgcoef.nc' >> $cormslogfile
    msg="FATAL ERROR: ${FIXofs}/${PREFIXNOS}.nudgcoef.nc does not exist, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    exit 4
  else
    cp -p ${FIXofs}/${PREFIXNOS}.nudgcoef.nc $DATA/.
    export err=$?; err_chk
    echo "${FIXofs}/${PREFIXNOS}.nudgcoef.nc was copied into working dir"
  fi
fi

if [ -z "${OFS##wcofs_da*}" -a  "$runtype" = "nowcast" ]; then
  if [ ! -s ${FIXofs}/${PREFIXNOS}.nrm_i_rand.nc ]; then
     echo '${FIXofs}/${PREFIXNOS}.nrm_i_rand.nc is not found'
     echo 'please provide ROMS control file of ${FIXofs}/${PREFIXNOS}.nrm_i_rand.nc'
     echo 'please provide ROMS control file of ${FIXofs}/${PREFIXNOS}.nrm_i_rand.nc' >> $cormslogfile
     msg="FATAL ERROR: ${FIXofs}/${PREFIXNOS}.nrm_i_rand.nc does not exist, FATAL ERROR!"
     postmsg "$jlogfile" "$msg"
     postmsg "$nosjlogfile" "$msg"
     exit 5
  else
     cp -p ${FIXofs}/${PREFIXNOS}.nrm_i_rand.nc $DATA/.
     export err=$?; err_chk
     echo "${FIXofs}/${PREFIXNOS}.nrm_i_rand.nc was copied into working dir"
  fi
  if [ ! -s ${FIXofs}/${PREFIXNOS}.std_i.nc ]; then
     echo '${FIXofs}/${PREFIXNOS}.std_i.nc is not found'
     echo 'please provide ROMS control file of ${FIXofs}/${PREFIXNOS}.std_i.nc'
     echo 'please provide ROMS control file of ${FIXofs}/${PREFIXNOS}.std_i.nc' >> $cormslogfile
     msg="FATAL ERROR: ${FIXofs}/${PREFIXNOS}.std_i.nc does not exist, FATAL ERROR!"
     postmsg "$jlogfile" "$msg"
     postmsg "$nosjlogfile" "$msg"
     exit 5
  else
     cp -p ${FIXofs}/${PREFIXNOS}.std_i.nc $DATA/.
     export err=$?; err_chk
     echo "${FIXofs}/${PREFIXNOS}.std_i.nc was copied into working dir"
  fi
  if [ ! -s ${FIXofs}/${PREFIXNOS}.psas.in ]; then
     echo '${FIXofs}/${PREFIXNOS}.psas.in is not found'
     echo 'please provide ROMS control file of ${FIXofs}/${PREFIXNOS}.psas.in'
     echo 'please provide ROMS control file of ${FIXofs}/${PREFIXNOS}.psas.in' >> $cormslogfile
     msg="FATAL ERROR: ${FIXofs}/${PREFIXNOS}.psas.in does not exist, FATAL ERROR!"
     postmsg "$jlogfile" "$msg"
     postmsg "$nosjlogfile" "$msg"
     exit 5
  else
     cp -p ${FIXofs}/${PREFIXNOS}.psas.in $DATA/.
     export err=$?; err_chk
     echo "${FIXofs}/${PREFIXNOS}.psas.in was copied into working dir"
  fi
  if [ ! -s ${COMOUT}/obs.nc ]; then
     echo '${COMOUT}/obs.nc is not found'
     echo 'please provide observation file ${COMOUT}/obs.nc'
     echo 'please provide observation file ${COMOUT}/obs.nc' >> $cormslogfile
     msg="FATAL ERROR: ${COMOUT}/obs.nc does not exist, FATAL ERROR!"
     postmsg "$jlogfile" "$msg"
     postmsg "$nosjlogfile" "$msg"
     exit 6
  else
     cp -p ${COMOUT}/obs.nc $DATA/.
     export err=$?; err_chk
     echo "${COMOUT}/obs.nc was copied into working dir"
  fi
fi
export HH=$cyc
export PDY1=$PDY

##  For prep Only -----------------------------------'
if [ "$runtype" = "prep" ] || [ "$runtype" = "PREP" ]; then 
# copy all shared static files into DATA/WORK Dirctory

  if [ ! -s ${FIXnos}/$OBC_CLIM_FILE ]; then
    echo '${FIXnos}/$OBC_CLIM_FILE is not found'
    echo 'please provide OBC control file of ${FIXnos}/$OBC_CLIM_FILE'
    echo 'please provide OBC control file of ${FIXnos}/$OBC_CLIM_FILE' >> $cormslogfile
    msg="FATAL ERROR: ${FIXnos}/$OBC_CLIM_FILE does not exist, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    exit 1
  else
    cp -p ${FIXnos}/$OBC_CLIM_FILE $DATA/.
    export err=$?; err_chk
  fi

  if [ ! -s ${FIXnos}/$HC_FILE_NWLON ]; then
    echo '${FIXnos}/$HC_FILE_NWLON is not found'
    echo 'please provide OBC control file of ${FIXnos}/$HC_FILE_NWLON'
    echo 'please provide OBC control file of ${FIXnos}/$HC_FILE_NWLON' >> $cormslogfile
    msg="FATAL ERROR: ${FIXnos}/$HC_FILE_NWLON does not exist, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    exit 1
  else
    cp -p ${FIXnos}/$HC_FILE_NWLON $DATA/.
    export err=$?; err_chk
  fi

  if [ ! -s ${FIXnos}/$RIVER_CLIM_FILE ]; then
    echo '${FIXnos}/$RIVER_CLIM_FILE is not found'
    echo 'please provide OBC control file of ${FIXnos}/$RIVER_CLIM_FILE'
    echo 'please provide OBC control file of ${FIXnos}/$RIVER_CLIM_FILE' >> $cormslogfile
    msg="FATAL ERROR: ${FIXnos}/$RIVER_CLIM_FILE does not exist, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    exit 1
  else
    cp -p ${FIXnos}/$RIVER_CLIM_FILE $DATA/.
    export err=$?; err_chk
  fi
  if [ "${OFS,,}" != "lsofs" -a  "${OFS,,}" != "loofs" ]; then
    if [ ! -s ${FIXofs}/$OBC_CTL_FILE ]; then
      echo '${FIXofs}/$OBC_CTL_FILE is not found'
      echo 'please provide OBC control file of ${FIXofs}/$OBC_CTL_FILE'
      echo 'please provide OBC control file of ${FIXofs}/$OBC_CTL_FILE' >> $cormslogfile
      msg="FATAL ERROR: ${FIXofs}/$OBC_CTL_FILE does not exist, FATAL ERROR!"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      exit 1
    else
      cp -p ${FIXofs}/$OBC_CTL_FILE $DATA/.
      export err=$?; err_chk
    fi
  fi
  NFILE=`ls -al  ${FIXofs}/${PREFIXNOS}.obc.clim.ts.* | wc -l`
  if [ $NFILE -gt 0 ]; then
    cp -p ${FIXofs}/${PREFIXNOS}.obc.clim.ts.* $DATA
  fi

#  if [ ${RUN} != "NEGOFS" -o ${RUN} != "negofs" -o ${RUN} != "NWGOFS" -o ${RUN} != "nwgofs" ]; then
#    for tmpfile in `ls ${FIXofs}/${PREFIXNOS}.obc.clim.ts.*`
#    do
#      if [ -f ${tmpfile} ]; then
#        cp -p $tmpfile $DATA
#      fi
#    done
#  fi

  if [ ! -s ${FIXofs}/$RIVER_CTL_FILE ]; then
    echo '${FIXofs}/$RIVER_CTL_FILE is not found'
    echo 'please provide River control file of ${FIXofs}/$RIVER_CTL_FILE'
    echo 'please provide River control file of ${FIXofs}/$RIVER_CTL_FILE' >> $cormslogfile
    msg="FATAL ERROR: ${FIXofs}/$RIVER_CTL_FILE does not exist, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    exit 4
  else
    cp -p ${FIXofs}/$RIVER_CTL_FILE $DATA/.
    if [ "${OFS,,}" == "sscofs" ]; then               
       cp -p ${FIXofs}/$RIVER_CTL_FILE_CANADA $DATA/.
    fi
    export err=$?; err_chk
  fi

  echo '------------------------------------------------'
  echo '   Variables read from main control file        '
  echo '------------------------------------------------'
  echo OFS= ${RUN}
  echo GRIDFILE=$GRIDFILE
  echo DBASE_MET_NOW= $DBASE_MET_NOW
  echo DBASE_MET_FOR= $DBASE_MET_FOR
  echo DBASE_WL_NOW= $DBASE_WL_NOW
  echo DBASE_WL_FOR= $DBASE_WL_FOR
  echo DBASE_TS_NOW= $DBASE_TS_NOW
  echo DBASE_TS_FOR= $DBASE_TS_FOR
  echo OCEAN_MODEL=$OCEAN_MODEL
  echo LEN_FORECAST=$LEN_FORECAST
  echo IGRD_MET=$IGRD_MET
  echo IGRD_OBC=$IGRD_OBC
  echo BASE_DATE=$BASE_DATE
  echo TIME_START=$TIME_START
  echo minlon=$MINLON
  echo minlat=$MINLAT
  echo maxlon=$MAXLON
  echo maxlat=$MAXLAT
  echo IM=$IM
  echo JM=$JM
  echo NDTFAST=$NDTFAST
  echo KBm=$KBm
  echo THETA_S=$THETA_S
  echo THETA_B=$THETA_B
  echo TCLINE=$TCLINE
  echo NVTRANS=$NVTRANS
  echo NVSTR=$NVSTR
  echo CREATE_TIDEFORCING=$CREATE_TIDEFORCING
  echo HC_FILE_OBC=$HC_FILE_OBC
  echo HC_FILE_OFS=$HC_FILE_OFS
  echo RIVER_CTL_FILE=$RIVER_CTL_FILE
  echo OBC_CTL_FILE=$OBC_CTL_FILE
  echo '------------------------------------------------'

##--------------------------------------
# Determine Job Output Name on System
##-------------------------------------- 
#  if CREATE_TIDEFORCING < 0 for non-tidal domains such as Great Lakes
  if [ $CREATE_TIDEFORCING -eq 0 ]; then
    if [ -s ${FIXofs}/$HC_FILE_OFS ]; then
      cp -p ${FIXofs}/$HC_FILE_OFS  $HC_FILE_OFS
    else
      CREATE_TIDEFORCING=1
    fi
  else
    echo "This file is not required for non-tidal domains"
  fi
  export CREATE_TIDEFORCING

## -------------------------------------------------------------#
# CHECK RESTART FILE AND COMPUTE HOT RESTART TIME FROM 
# RESTART/INITIAL FILE OF PREVIOUS NOWCAST RUN
# COMPUTE TIME FOR NOWCAST AND FORECAST RUN TIME FOR MODEL RUNS
##--------------------------------------------------------------#

  echo "check availability of model restart file from previous run" >>  $jlogfile
  echo "check availability of model restart file from previous run" >>  $nosjlogfile
  echo "check availability of model restart file from previous run" >> $cormslogfile

  COLD_START="F"
  if [ -z "${OFS##wcofs_da*}" ]; then
    BACK_SEARCH=`expr $LEN_DA + 1`
  else
    BACK_SEARCH=49
  fi
  CURRENTTIME=`$NDATE -1 $time_nowcastend `
  YYYY=`echo $CURRENTTIME |cut -c1-4 `
  MM=`echo $CURRENTTIME |cut -c5-6 `
  DD=`echo $CURRENTTIME |cut -c7-8 `
  HH=`echo $CURRENTTIME |cut -c9-10 `
  RST_FILE=$COMOUTroot/${RUN}.$YYYY$MM$DD/${PREFIXNOS}.t${HH}z.$YYYY$MM$DD.rst.nowcast.nc
  if [ $OCEAN_MODEL == "SELFE" ]; then
    RST_FILE=$COMOUTroot/${RUN}.$YYYY$MM$DD/${PREFIXNOS}.t${HH}z.$YYYY$MM$DD.rst.nowcast.bin
  fi
  while [ ! -s $RST_FILE ]
  do
    CURRENTTIME=`$NDATE -1 $CURRENTTIME `
    if [ $CURRENTTIME -le ` $NDATE -$BACK_SEARCH $time_nowcastend ` ]; then # allow to search 2 days backward, wcofs_da back 3 days
      COLD_START="T"
      break
    fi
    YYYY=`echo $CURRENTTIME |cut -c1-4 `
    MM=`echo $CURRENTTIME |cut -c5-6 `
    DD=`echo $CURRENTTIME |cut -c7-8 `
    HH=`echo $CURRENTTIME |cut -c9-10 `
    RST_FILE=$COMOUTroot/${RUN}.$YYYY$MM$DD/${PREFIXNOS}.t${HH}z.$YYYY$MM$DD.rst.nowcast.nc
    if [ $OCEAN_MODEL == "SELFE" ]; then
      RST_FILE=$COMOUTroot/${RUN}.$YYYY$MM$DD/${PREFIXNOS}.t${HH}z.$YYYY$MM$DD.rst.nowcast.bin
    fi
  done

  if [ $COLD_START == "T" ]; then
    echo 'no valid hot restart file for the given time period' >> $cormslogfile
    echo 'no valid hot restart file for the given time period' >> $jlogfile
    echo 'no valid hot restart file for the given time period' >> $nosjlogfile
    INI_FILE=${FIXofs}/${PREFIXNOS}.init.nc
    if [ $OCEAN_MODEL == "SELFE" -o $OCEAN_MODEL == "selfe" ]; then
      INI_FILE=${FIXofs}/${PREFIXNOS}.init.bin
      BASE_DATE=` $NDATE -48 $time_nowcastend `
      export BASE_DATE
    fi 

# AJ 07/17/2014 prevent using initial condition file from fix
# For operational runs, no cold start is allowed
    echo "no valid hot restart file within previous 48 hours"
    echo "please check archive folder of, $COMOUT"
    echo "This normally occurs during Production Switch"
    echo "Please consult with CO-OPS Modeling Team if needed"
     # if err_exit is comment out below, it means cold start from a init file in FIXnos
     # for operations, OFS will stop if no good restart file is found
    err_exit "NO VALID RESTART FILE AVAILABLE. Please check $COMOUT."
  elif [ $COLD_START == "F" ]; then
    echo 'found valid hot restart file at time: ' $YYYY $MM $DD ${HH} >> $cormslogfile
    echo 'found valid hot restart file at time: ' $YYYY $MM $DD ${HH} >> $jlogfile
    echo 'found valid hot restart file at time: ' $YYYY $MM $DD ${HH} >> $nosjlogfile
    echo 'nowcast run from hot start '  >> $cormslogfile
    echo 'nowcast from hot restart file: ' ${PREFIXNOS}.t${HH}z.$YYYY$MM$DD.rst.nowcast.nc >> $cormslogfile
    echo 'nowcast from hot restart file: ' ${PREFIXNOS}.t${HH}z.$YYYY$MM$DD.rst.nowcast.nc >> $jlogfile
    echo 'nowcast from hot restart file: ' ${PREFIXNOS}.t${HH}z.$YYYY$MM$DD.rst.nowcast.nc >> $nosjlogfile
    INI_FILE=$RST_FILE
    if [ $OCEAN_MODEL == "SELFE" ]; then
      BASE_DATE=${YYYY}${MM}${DD}${HH}
      NH_NOWCAST=`$NHOUR $time_nowcastend $BASE_DATE `
      if [ $NH_NOWCAST -ge 48 ]; then
        INI_FILE=${FIXofs}/${PREFIXNOS}.init.bin
        COLD_START="T"
        BASE_DATE=` $NDATE -48 $time_nowcastend `
      fi       
      export BASE_DATE
    fi       
  fi

  YYYY=`echo $time_nowcastend | cut -c1-4 `
  MM=`echo $time_nowcastend | cut -c5-6 `
  DD=`echo $time_nowcastend | cut -c7-8 `
  HH=`echo $time_nowcastend | cut -c9-10 `
  export INI_FILE_ROMS=${PREFIXNOS}.${cycle}.${YYYY}${MM}${DD}.init.nowcast.nc
  if [ $OCEAN_MODEL == "SELFE" -o $OCEAN_MODEL == "selfe" ]; then
    export INI_FILE_ROMS=${PREFIXNOS}.${cycle}.${YYYY}${MM}${DD}.init.nowcast.bin
  fi 
##For DA cycle, take the proper time record considering the overlapping window
  if [ -z "${OFS##*_da*}" -a -z "${INI_FILE##*_da*}"  ]; then
    dummy=`expr $NRST / 3600` # sec to hour
      if [ $CURRENTTIME -le ` $NDATE -49 $time_nowcastend ` ]; then
        ((  NINI=24 / $dummy * 3 - 1 ))
      elif [ $CURRENTTIME -le ` $NDATE -25 $time_nowcastend ` ]; then
        ((  NINI=24 / $dummy * 2 - 1 ))
      else
        ((  NINI=24 / $dummy * 1 - 1 ))
      fi
    ncks -d ocean_time,$NINI,$NINI $INI_FILE $DATA/$INI_FILE_ROMS
  else
    cp -p $INI_FILE $DATA/$INI_FILE_ROMS
  fi

  if [ -s $INI_FILE_ROMS ]; then
    echo ${RUN} > Fortran_read_restart.ctl
    echo $OCEAN_MODEL  >> Fortran_read_restart.ctl
    echo $COLD_START  >> Fortran_read_restart.ctl
    echo $GRIDFILE  >> Fortran_read_restart.ctl
    echo ${INI_FILE_ROMS}  >> Fortran_read_restart.ctl    
    echo ${RUN}_time_initial.dat  >> Fortran_read_restart.ctl
    echo $time_nowcastend >> Fortran_read_restart.ctl
    echo $BASE_DATE >> Fortran_read_restart.ctl
    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]; then
      echo $ne_global  >> Fortran_read_restart.ctl
      echo $np_global >> Fortran_read_restart.ctl
      echo $ns_global >> Fortran_read_restart.ctl
      echo $nvrt >> Fortran_read_restart.ctl
    fi
    export pgm=nos_ofs_read_restart
. prep_step
    if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]; then 
      $EXECnos/nos_ofs_read_restart < Fortran_read_restart.ctl > Fortran_read_restart.log
      export err=$?
    elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]; then
      $EXECnos/nos_ofs_read_restart_fvcom < Fortran_read_restart.ctl > Fortran_read_restart.log
      export err=$?
    elif [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]; then
     echo "Do not run nos_ofs_read_restart_selfe" > Fortran_read_restart.log
     echo "COMPLETED SUCCESSFULLY" >> Fortran_read_restart.log
     echo $BASE_DATE 0 0.0 0.0d0 > ${RUN}_time_initial.dat
    fi    

    if grep "COMPLETED SUCCESSFULLY" Fortran_read_restart.log /dev/null 2>&1
    then
      echo "RESTART_TIME DONE 100" >> $cormslogfile
    else
      echo "RESTART_TIME  DONE 0" >> $cormslogfile
      echo "Please check Fortran_read_restart.log for details"
      err=3
      err_chk
    fi

    if [ $err -ne 0 ]; then
      echo "Execution of $pgm did not complete normally, FATAL ERROR!" >> $cormslogfile
      echo "Execution of $pgm did not complete normally, FATAL ERROR!"
      msg=" Execution of $pgm did not complete normally, FATAL ERROR!"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_chk
    else
      echo "Execution of $pgm completed normally" >> $cormslogfile
      echo "Execution of $pgm completed normally"
      msg=" Execution of $pgm completed normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
###################################################################
#     copy ${INI_FILE_ROMS to ${COMOUT}
###################################################################
      echo "Copying ${INI_FILE_ROMS} to ${COMOUT} " >> $cormslogfile
      cp -p ${INI_FILE_ROMS} ${COMOUT}/${INI_FILE_ROMS}
###################################################################
    fi

    read time_hotstart NTIMES DAY0 TIDE_START < ${RUN}_time_initial.dat
  else
    echo 'Model Initial file is not found'
    echo 'please provide model initial file'
    echo 'proper restart/initial file does not exist !!!' >> $cormslogfile
    msg="FATAL ERROR:proper restart/initial file does not exist, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    exit 2 
  fi  

  if [ $time_hotstart -ge $time_nowcastend ]; then
    echo 'time_hotstart is equal to or greater than time_nowcastend '
    echo 'read wroing restart file'
    echo 'check hot_restart file !!!'
    echo 'time_hotstart is equal to or greater than time_nowcastend ' >> $cormslogfile
    msg="FATAL ERROR:time_hotstart is equal to or greater than time_nowcastend, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    exit 3 
  fi   

  if [ $NTIMES -le 0 ]; then
    NRREC=0
  else
    NRREC=-1
  fi

  export DSTART_NOWCAST=$DAY0
  export time_hotstart NTIMES NRREC
  export TIDE_START

  echo "time_hotstart= $time_hotstart" >> $cormslogfile
  echo "DSTART_NOWCAST= $DSTART_NOWCAST" >> $cormslogfile
  echo "NTIMES= $NTIMES" >> $cormslogfile
  echo "TIDE_START= $TIDE_START " >> $cormslogfile
#compute forcastend time, sets the number of hours for reformatting
#number hours for forecast run

  export time_forecastend=`$NDATE $LEN_FORECAST $time_nowcastend`

# --------------------------------------------------------------------------------------
#  Define file names used for model run
# --------------------------------------------------------------------------------------
  YYYY=`echo $time_nowcastend | cut -c1-4 `
  MM=`echo $time_nowcastend |cut -c5-6 `
  DD=`echo $time_nowcastend |cut -c7-8 `
  HH=`echo $time_nowcastend |cut -c9-10 `
  if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]; then
    export NDEFHIS=${NDEFHIS:-${NHIS}}
    export NSTA=`expr $NSTA / ${DELT_MODEL%.*}`
    export NRST=`expr $NRST / ${DELT_MODEL%.*}`
    export NHIS=`expr $NHIS / ${DELT_MODEL%.*}`
    export NDEFHIS=`expr $NDEFHIS / ${DELT_MODEL%.*}`
    export NFLT=`expr $NFLT / ${DELT_MODEL%.*}`
    export NAVG=`expr $NAVG / ${DELT_MODEL%.*}`
    export NQCK=`expr $NQCK / ${DELT_MODEL%.*}`
    export NDEFQCK=`expr $NDEFQCK / ${DELT_MODEL%.*}`
  fi

  export NH_NOWCAST=`$NHOUR $time_nowcastend $time_hotstart`
  export NSTEP_NOWCAST=`expr $NH_NOWCAST \* 3600 / ${DELT_MODEL%.*}`
  export NTIMES_NOWCAST=$NSTEP_NOWCAST
  export NH_FORECAST=`$NHOUR $time_forecastend $time_nowcastend `
  export NSTEP_FORECAST=`expr $NH_FORECAST \* 3600 / ${DELT_MODEL%.*}`
  export NTIMES_FORECAST=$NSTEP_FORECAST      #for newer version than 859
  export PDY1=$YYYY$MM$DD
  export DSTART_FORECAST=`echo "scale=4;$DAY0+${NH_NOWCAST}/24.0" | bc`
  if [ $NH_NOWCAST -lt 1 ]; then
    echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON ${PDY1} FAILED 00"  >> $cormslogfile 
    echo "NOWCAST CYCLE IS: " $time_nowcastend" FATAL ERROR" >> $cormslogfile 
    echo "Hours of nowcast simulation is shorter than one hour" >> $cormslogfile
    echo "Hours of nowcast simulation is shorter than one hour"
    echo "current cycle nowcast/forecast runs stop"
    msg="FATAL ERROR:Hours of nowcast simulation is shorter than one hour, FATAL ERROR!!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    err=1; export err; err_chk
  fi

## For SELFE testing
# if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
# then
  echo $time_nowcastend > $COMOUT/time_nowcastend.${cycle}
  echo $time_hotstart > $COMOUT/time_hotstart.${cycle}
  echo $time_forecastend > $COMOUT/time_forecastend.${cycle}
  echo $BASE_DATE > $COMOUT/base_date.${cycle}
# fi

# check availability of real time data in DCOM (BURF)  
    YYYY=`echo $time_hotstart | cut -c1-4 `
    MM=`echo $time_hotstart |cut -c5-6 `
    DD=`echo $time_hotstart |cut -c7-8 `
    HH=`echo $time_hotstart |cut -c9-10 `
    BUFR_NOS=$DCOMINports/$YYYY$MM$DD/b001/$NOSBUFR
    BUFR_USGS=$DCOMINusgs/$YYYY$MM$DD/b001/$USGSBUFR
#    if [ $envir = dev ]; then
#      export maillist='nos.co-ops.modelingteam@noaa.gov'
#    fi
#    export maillist=${maillist:-'nco.spa@noaa.gov,nos.co-ops.modelingteam@noaa.gov'}
    if [ ! -s $BUFR_NOS ]; then
            export subject="Missing NOS BUFR Data for $PDY ${cycle} $job"
            echo "*************************************************************" > mailmsg
            echo "*** WARNING !! COULD NOT FIND NOS BUFR Data  *** " >> mailmsg
            echo "*************************************************************" >> mailmsg
            echo >> mailmsg
            echo "   $BUFR_NOS " >> mailmsg
            echo " climatologic dataset is used "  >> mailmsg 
            echo >> mailmsg
            echo "check availability of NOS BUFR FILE in DCOM" >> mailmsg
            cat mailmsg > $COMOUT/${RUN}.${cycle}.nosbufr.emailbody
            cat $COMOUT/${RUN}.${cycle}.nosbufr.emailbody | mail.py -s "$subject" $maillist -v
    fi
    if [ ! -s $BUFR_USGS ]; then
            export subject="Missing USGS BUFR Data for $PDY ${cycle} $job"
            echo "*************************************************************" > mailmsg
            echo "*** WARNING !! COULD NOT FIND USGS BUFR Data  *** " >> mailmsg
            echo "*************************************************************" >> mailmsg
            echo >> mailmsg
            echo "   $BUFR_USGS " >> mailmsg
            echo " climatologic dataset is used "  >> mailmsg
            echo >> mailmsg
            echo "check availability of USGS BUFR FILE in DCOM" >> mailmsg
            cat mailmsg > $COMOUT/${RUN}.${cycle}.usgsbufr.emailbody
            cat $COMOUT/${RUN}.${cycle}.usgsbufr.emailbody | mail.py -s "$subject" $maillist -v
    fi
fi

## -- End of prep Only --------------------------------'

export OBC_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY1}.obc.nc
export OBC_FORCING_FILE_EL=${PREFIXNOS}.${cycle}.${PDY1}.obc.el.nc
export OBC_FORCING_FILE_TS=${PREFIXNOS}.${cycle}.${PDY1}.obc.ts.nc
export RIVER_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY1}.river.nc
export OBC_TIDALFORCING_FILE=${PREFIXNOS}.${cycle}.${PDY1}.roms.tides.nc
export NUDG_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY1}.clim.nc

export OBC_FORCING_FILE_EL=${OBC_FORCING_FILE}
export OBC_FORCING_FILE_TS=${OBC_FORCING_FILE}

###export INI_FILE_NOWCAST=$INI_FILE_ROMS
export INI_FILE_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.init.nowcast.nc
export HIS_OUT_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.fields.nowcast.nc
export STA_OUT_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.stations.nowcast.nc
export RST_OUT_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.rst.nowcast.nc
export MET_NETCDF_1_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.met.nowcast.nc
export MET_NETCDF_2_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.hflux.nowcast.nc
export HIS_2D_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.surface.nowcast.nc
export HIS_2D_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.surface.forecast.nc
export INI_FILE_FORECAST=$RST_OUT_NOWCAST
export HIS_OUT_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.fields.forecast.nc
export STA_OUT_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.stations.forecast.nc
export RST_OUT_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.rst.forecast.nc
export MET_NETCDF_1_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.met.forecast.nc
export MET_NETCDF_2_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.hflux.forecast.nc
export MODEL_LOG_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.nowcast.log
export MODEL_LOG_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.forecast.log
export RUNTIME_CTL_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.nowcast.in
export RUNTIME_CTL_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.forecast.in
if [ -z "${OFS##wcofs_da*}" ]; then
  export COMOUTrst1=$COMrst/${OFS_NF}.${PDY1}
  export RST_OUT_NOWCAST_NF1=${OFS_NF}.t${HH}z.${PDY1}.rst.nowcast.nc
  NF_RST_TIME=`$NDATE -12 ${PDY1}${HH}`
  YYYY=`echo $NF_RST_TIME | cut -c1-4 `
  MM=`echo $NF_RST_TIME |cut -c5-6 `
  DD=`echo $NF_RST_TIME |cut -c7-8 `
  HH1=`echo $NF_RST_TIME |cut -c9-10 `
  PDY_NF=$YYYY$MM$DD
  HH_NF=$HH1
  export RST_OUT_NOWCAST_NF2=${OFS_NF}.t${HH_NF}z.${PDY_NF}.rst.nowcast.nc
  export COMOUTrst2=$COMrst/${OFS_NF}.$PDY_NF
fi
if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]; then
  export MET_NETCDF_1_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.met.nowcast.nc.tar
  export MET_NETCDF_1_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.met.forecast.nc.tar
  export OBC_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY1}.obc.tar
  export OBC_FORCING_FILE_EL=${PREFIXNOS}.${cycle}.${PDY1}.obc.el.tar
  export OBC_FORCING_FILE_TS=${PREFIXNOS}.${cycle}.${PDY1}.obc.ts.tar
  export RIVER_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY1}.river.th.tar
  export INI_FILE_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.init.nowcast.bin
  export RST_OUT_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.rst.nowcast.bin
  export RST_OUT_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.rst.forecast.bin
  export INI_FILE_FORECAST=$RST_OUT_NOWCAST
  export RUNTIME_MET_CTL_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.met_ctl.nowcast.in
  export RUNTIME_MET_CTL_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.met_ctl.forecast.in
  export RUNTIME_COMBINE_RST_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.combine.hotstart.nowcast.in
  export RUNTIME_COMBINE_NETCDF_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.combine.netcdf.nowcast.in
  export RUNTIME_COMBINE_NETCDF_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.combine.netcdf.forecast.in
  export RUNTIME_COMBINE_NETCDF_STA_NOWCAST=${PREFIXNOS}.${cycle}.${PDY1}.combine.netcdf.sta.nowcast.in
  export RUNTIME_COMBINE_NETCDF_STA_FORECAST=${PREFIXNOS}.${cycle}.${PDY1}.combine.netcdf.sta.forecast.in
elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]; then
  export RIVER_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY1}.river.nc.tar
fi
export RST_FILE=$RST_FILE
echo "Variable and parameter setup has been completed" >> $jlogfile
echo "Variable and parameter setup has been completed" >> $nosjlogfile

