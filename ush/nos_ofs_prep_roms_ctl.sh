#!/bin/sh
# Scripts Name:  nos_ofs_reformat_roms_ctl_forecast.sh
#
# Purpose:
#   This program is used to reformat ROMS runtime input parameter control file (ROMS.in).
#   some runtime parameters for ROMS need to be changed dynamically.
#
# Location:   ~/scripts
#
# Technical Contact:   	Aijun Zhang         Org:  NOS/CO-OPS
#                       Phone: 301-7132890 ext. 127  E-Mail: aijun.zhang@noaa.gov
#
#  Usage: ./nos_ofs_prep_roms_ctl.sh RUNTYPE 
#
# Input Parameters:
#  NNFILES  : Number of forcing files for this simulation
#
# Language:   Bourne Shell Script      
#
# Target Computer:  IBM Super Computer at NCEP
#
# Estimated Execution Time: 30s 
#
# Suboutines/Functions Called:
#     nos_ofs_reformat_ROMS_CTL.f   
#
# Input Files:
#    Standard ROMS runtime input file  
# Output Files:
#    ROMS RUNTIME input parameter control file:  
#    {OFS}_roms_{RUNTYPE}.in
#
# Libraries Used: see the makefile
#  
# Error Conditions:
#
# Remarks:
#
# Modification History:
#  (1). Degui Cao
#       Implementation                    01/14/2010
#
# ----------------------------------------------------------------------------------------
  echo 'The script nos_ofs_prep_roms_ctl.sh starts at time: ' `date `
set -x
RUN=$1
RUNTYPE=$2

echo "BEGIN SECTION OF GENERATING $OCEAN_MODEL CONTROL FILE for $RUNTYPE" >> $cormslogfile
if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
then
  DBASE=$DBASE_MET_NOW
  INI_FILE_ROMS=${INI_FILE_NOWCAST} 
  RST_OUT_ROMS=${RST_OUT_NOWCAST}
  HIS_OUT_ROMS=${HIS_OUT_NOWCAST}
  STA_OUT_ROMS=${STA_OUT_NOWCAST}
  MET_NETCDF_1=${MET_NETCDF_1_NOWCAST}
  MET_NETCDF_2=${MET_NETCDF_2_NOWCAST}
  NSTEP=${NSTEP_NOWCAST}
  NTIMES=${NTIMES_NOWCAST}
  RUNTIME_CONTROL=${RUNTIME_CTL_NOWCAST}
  HIS_2D_ROMS=${HIS_2D_NOWCAST}
  DSTART=${DSTART_NOWCAST}
elif [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]
then
  DBASE=$DBASE_MET_FOR
  INI_FILE_ROMS=${RST_OUT_NOWCAST} 
  RST_OUT_ROMS=${RST_OUT_FORECAST}
  HIS_OUT_ROMS=${HIS_OUT_FORECAST}
  STA_OUT_ROMS=${STA_OUT_FORECAST}
  MET_NETCDF_1=${MET_NETCDF_1_FORECAST}
  MET_NETCDF_2=${MET_NETCDF_2_FORECAST}
  NSTEP=${NSTEP_FORECAST}
  NTIMES=${NTIMES_FORECAST}
  RUNTIME_CONTROL=${RUNTIME_CTL_FORECAST}
  HIS_2D_ROMS=${HIS_2D_FORECAST}
  DSTART=${DSTART_FORECAST}
fi
NFFILES=1
if [ $DBASE == "RTMA" ]
then
  NFFILES=2
fi  
#if [ -n "${OFS##wcofs_da*}"  ]
#then
#  NRST=$NTIMES   #write restart file at the end of nowcast run to save time except for DA
#fi
if [ "$CONTINUE_FORECAST" == 'YES' ]; then
  if [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]; then
   echo "NRREC == -1 "  > ROMS_INPUT.dat
   echo "NRST == ${NRST}"  >> ROMS_INPUT.dat
   echo "NTIMES == $NTIMES "  >> ROMS_INPUT.dat 
   echo "ININAME == restart_continue_forecast.nc  "  >> ROMS_INPUT.dat
   BIO_MODULE=${BIO_MODULE:-0}
   if [ "$BIO_MODULE" -ne 1 ]; then
    echo "NFFILES == ${NFFILES} "  >> ROMS_INPUT.dat
    if [ ${NFFILES} -eq 1 ]; then   ### for version 859, river input and tidal forcing files are separated from forcing files
      echo "FRCNAME == ${MET_NETCDF_1} "  >> ROMS_INPUT.dat
      echo "TIDENAME == ${HC_FILE_OFS} "  >> ROMS_INPUT.dat
      echo "SSFNAME == ${RIVER_FORCING_FILE} "  >> ROMS_INPUT.dat
    elif [ ${NFFILES} -eq 2 ]; then
      echo "FRCNAME == ${MET_NETCDF_1} \ "  >> ROMS_INPUT.dat
      echo "           ${MET_NETCDF_2} " >> ROMS_INPUT.dat
      echo "TIDENAME == ${HC_FILE_OFS} "  >> ROMS_INPUT.dat
      echo "SSFNAME == ${RIVER_FORCING_FILE} "  >> ROMS_INPUT.dat
    fi
   elif [ "$BIO_MODULE" -eq 1 ]; then
    NFFILES=`expr ${NFFILES} + 1`
    echo "NFFILES == ${NFFILES} "  >> ROMS_INPUT.dat
    if [ ${NFFILES} -eq 2 ]; then   ### for version 859, river input and tidal forcing files are separated from forcing files
      echo "FRCNAME == ${MET_NETCDF_1} \ "  >> ROMS_INPUT.dat
      echo "       ${RESPIRATE_RATE} " >> ROMS_INPUT.dat
      echo "TIDENAME == ${HC_FILE_OFS} "  >> ROMS_INPUT.dat
      echo "SSFNAME == ${RIVER_FORCING_FILE} "  >> ROMS_INPUT.dat
    elif [ ${NFFILES} -eq 3 ]; then
      echo "FRCNAME == ${MET_NETCDF_1} \ "  >> ROMS_INPUT.dat
      echo "           ${MET_NETCDF_2} \ " >> ROMS_INPUT.dat
      echo "       ${RESPIRATE_RATE} " >> ROMS_INPUT.dat
     echo "TIDENAME == ${HC_FILE_OFS} "  >> ROMS_INPUT.dat
     echo "SSFNAME == ${RIVER_FORCING_FILE} "  >> ROMS_INPUT.dat
    fi
   fi 
  fi

else


echo 'The script nos_ofs_prep_roms_ctl.sh has started at UTC' `date -u +%Y%m%d%H`
echo 'The script nos_ofs_prep_roms_ctl.sh has started at UTC' `date -u +%Y%m%d%H` >> $jlogfile
echo 'The script nos_ofs_prep_roms_ctl.sh has started at UTC' `date -u +%Y%m%d%H` >> $nosjlogfile
if [ $envir = "prod" ]
then
  echo "TITLE = ${RUN} $RUNTYPE RUN in operational mode" > ROMS_INPUT.dat
  echo "TITLE = ${RUN} $RUNTYPE RUN in operational mode" >> $jlogfile
  echo "TITLE = ${RUN} $RUNTYPE RUN in operational mode" >> $nosjlogfile
  echo "MyAppCPP = ${RUN} $RUNTYPE RUN in operational mode" >> ROMS_INPUT.dat
elif [ $envir = "para" ]
then
  echo "TITLE = ${RUN} $RUNTYPE RUN in parallel mode" > ROMS_INPUT.dat
  echo "TITLE = ${RUN} $RUNTYPE RUN in parallel mode" >> $jlogfile
  echo "TITLE = ${RUN} $RUNTYPE RUN in parallel mode" >> $nosjlogfile
  echo "MyAppCPP = ${RUN} $RUNTYPE RUN in parallel mode" >> ROMS_INPUT.dat
else
  echo "TITLE = ${RUN} $RUNTYPE RUN in expermental mode" > ROMS_INPUT.dat
  echo "TITLE = ${RUN} $RUNTYPE RUN in expermental mode" >> $jlogfile
  echo "TITLE = ${RUN} $RUNTYPE RUN in expermental mode" >> $nosjlogfile
  echo "MyAppCPP = ${RUN} $RUNTYPE RUN in experimental mode" >> ROMS_INPUT.dat

fi
#echo "VARNAME = ${VARINFOFILE_ROMS} " >> ROMS_INPUT.dat
echo "VARNAME = varinfo.yaml " >> ROMS_INPUT.dat
echo "Lm == `expr ${IM} - 2`"  >> ROMS_INPUT.dat
echo "Mm == `expr ${JM} - 2` "  >> ROMS_INPUT.dat
echo " N == $KBm "  >> ROMS_INPUT.dat
echo "THETA_S == ${THETA_S} ">> ROMS_INPUT.dat
echo "THETA_B == ${THETA_B} ">> ROMS_INPUT.dat
echo "TCLINE == ${TCLINE} " >> ROMS_INPUT.dat
echo "NtileI == ${NTILE_I}  ! I-direction partition " >> ROMS_INPUT.dat
echo "NtileJ == ${NTILE_J}  ! J-direction partition " >> ROMS_INPUT.dat
echo "NTIMES == $NTIMES "  >> ROMS_INPUT.dat
echo "    DT == ${DELT_MODEL}.0d0 "  >> ROMS_INPUT.dat
echo "NDTFAST == $NDTFAST "  >> ROMS_INPUT.dat
if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
then
   echo "NRREC == $NRREC "  >> ROMS_INPUT.dat
   echo "NRST == $NRST "  >> ROMS_INPUT.dat
elif [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]
then
   echo "NRREC == -1 "  >> ROMS_INPUT.dat
   echo "NRST == `expr ${NRST} \* 1`"  >> ROMS_INPUT.dat
#   echo "NRST == 0 "  >> ROMS_INPUT.dat  # do not write restart file for forecast run 
fi
echo "NSTA == $NSTA "  >> ROMS_INPUT.dat
echo "NFLT == $NFLT "  >> ROMS_INPUT.dat
echo "NHIS == $NHIS "  >> ROMS_INPUT.dat
echo "NDEFHIS == $NDEFHIS "  >> ROMS_INPUT.dat
echo "NAVG == $NAVG "  >> ROMS_INPUT.dat
echo "NQCK == $NQCK "  >> ROMS_INPUT.dat
echo "NDEFQCK == $NDEFQCK "  >> ROMS_INPUT.dat
echo "TNU2 == $TNU2 "  >> ROMS_INPUT.dat
echo "VISC2 == $VISC2  ! m2/s"  >> ROMS_INPUT.dat
echo "VISC4 == $VISC4  ! m4/s"  >> ROMS_INPUT.dat
echo "RDRG2 == $RDRG2  ! nondimensional " >> ROMS_INPUT.dat
echo "Zob == $Zob    ! m " >> ROMS_INPUT.dat
echo "AKT_BAK == $AKT_BAK " >> ROMS_INPUT.dat
echo "AKV_BAK == $AKV_BAK " >> ROMS_INPUT.dat
echo "AKK_BAK == $AKK_BAK " >> ROMS_INPUT.dat
echo "AKP_BAK == $AKP_BAK " >> ROMS_INPUT.dat
echo "DCRIT == $DCRIT " >> ROMS_INPUT.dat

#if [ ${NRREC} -eq 0 ]
#then 
  echo "DSTART =  ${DSTART}d0       ! days" >> ROMS_INPUT.dat
#fi
echo "TIDE_START = $TIDE_START   ! days" >> ROMS_INPUT.dat
echo "TIME_REF = `echo $BASE_DATE |cut -c1-8`.0d0     ! yyyymmdd.dd" >> ROMS_INPUT.dat

echo "GRDNAME == ${GRIDFILE}  "  >> ROMS_INPUT.dat
echo "BRYNAME == ${OBC_FORCING_FILE}  "  >> ROMS_INPUT.dat
echo "ININAME == ${INI_FILE_ROMS}  "  >> ROMS_INPUT.dat

echo "RSTNAME == ${RST_OUT_ROMS} "  >> ROMS_INPUT.dat
echo "HISNAME == ${HIS_OUT_ROMS} "  >> ROMS_INPUT.dat
echo "QCKNAME == ${HIS_2D_ROMS} "  >> ROMS_INPUT.dat
echo "STANAME == ${STA_OUT_ROMS} "  >> ROMS_INPUT.dat
if [ -s $STA_OUT_CTL ]
then
  echo "SPOSNAM ==  ${STA_OUT_CTL} "  >> ROMS_INPUT.dat
fi
echo ${NFFILES}
BIO_MODULE=${BIO_MODULE:-0}
if [ $BIO_MODULE -ne 1 ]; then
 echo "NFFILES == ${NFFILES} "  >> ROMS_INPUT.dat
 if [ ${NFFILES} -eq 1 ]; then   ### for version 859, river input and tidal forcing files are separated from forcing files
   echo "FRCNAME == ${MET_NETCDF_1} "  >> ROMS_INPUT.dat
   echo "TIDENAME == ${HC_FILE_OFS} "  >> ROMS_INPUT.dat
   echo "SSFNAME == ${RIVER_FORCING_FILE} "  >> ROMS_INPUT.dat
 elif [ ${NFFILES} -eq 2 ]; then
   echo "FRCNAME == ${MET_NETCDF_1} \ "  >> ROMS_INPUT.dat
   echo "           ${MET_NETCDF_2} " >> ROMS_INPUT.dat
   echo "TIDENAME == ${HC_FILE_OFS} "  >> ROMS_INPUT.dat
   echo "SSFNAME == ${RIVER_FORCING_FILE} "  >> ROMS_INPUT.dat
#   = 3 and 4 is kept for version 90, and have to be modified for version 744
 elif [ ${NFFILES} -eq 3 ]; then
   echo "FRCNAME == ${HC_FILE_OFS} \ "  >> ROMS_INPUT.dat
   echo "           ${RIVER_FORCING_FILE} \ " >> ROMS_INPUT.dat
   echo "           ${MET_NETCDF_1} " >> ROMS_INPUT.dat
 elif [ ${NFFILES} -eq 4 ]; then
   echo "FRCNAME == ${HC_FILE_OFS} \ "  >> ROMS_INPUT.dat
   echo "           ${RIVER_FORCING_FILE} \ " >> ROMS_INPUT.dat
   echo "           ${MET_NETCDF_1} \ " >> ROMS_INPUT.dat
   echo "           ${MET_NETCDF_2} " >> ROMS_INPUT.dat
 fi
elif [ $BIO_MODULE -eq 1 ]; then
 NFFILES=`expr ${NFFILES} + 1`
 echo "NFFILES == ${NFFILES} "  >> ROMS_INPUT.dat
 if [ ${NFFILES} -eq 2 ]; then   ### for version 859, river input and tidal forcing files are separated from forcing files
   echo "FRCNAME == ${MET_NETCDF_1} \ "  >> ROMS_INPUT.dat
   echo "       ${RESPIRATE_RATE} " >> ROMS_INPUT.dat
   echo "TIDENAME == ${HC_FILE_OFS} "  >> ROMS_INPUT.dat
   echo "SSFNAME == ${RIVER_FORCING_FILE} "  >> ROMS_INPUT.dat
 elif [ ${NFFILES} -eq 3 ]; then
   echo "FRCNAME == ${MET_NETCDF_1} \ "  >> ROMS_INPUT.dat
   echo "           ${MET_NETCDF_2} \ " >> ROMS_INPUT.dat
   echo "       ${RESPIRATE_RATE} " >> ROMS_INPUT.dat

   echo "TIDENAME == ${HC_FILE_OFS} "  >> ROMS_INPUT.dat
   echo "SSFNAME == ${RIVER_FORCING_FILE} "  >> ROMS_INPUT.dat

#   = 4 and 5 is kept for version 90, and have to be modified for version 744
 elif [ ${NFFILES} -eq 4 ]; then
   echo "FRCNAME == ${HC_FILE_OFS} \ "  >> ROMS_INPUT.dat
   echo "           ${RIVER_FORCING_FILE} \ " >> ROMS_INPUT.dat
   echo "           ${MET_NETCDF_1} \ " >> ROMS_INPUT.dat
   echo "       ${RESPIRATE_RATE} " >> ROMS_INPUT.dat
 elif [ ${NFFILES} -eq 5 ]; then
   echo "FRCNAME == ${HC_FILE_OFS} \ "  >> ROMS_INPUT.dat
   echo "           ${RIVER_FORCING_FILE} \ " >> ROMS_INPUT.dat
   echo "           ${MET_NETCDF_1} \ " >> ROMS_INPUT.dat
   echo "           ${MET_NETCDF_2} \ " >> ROMS_INPUT.dat
   echo "           ${RESPIRATE_RATE} " >> ROMS_INPUT.dat
 fi
fi
TS_NUDGING=${TS_NUDGING:-0}
if [ $TS_NUDGING -eq 1 ]; then
   echo "CLMNAME == ${NUDG_FORCING_FILE} "  >> ROMS_INPUT.dat
fi

fi #CONTINUE_FORECAST LOOP

echo "ROMS_INPUT.dat " > reformat_ROMS_CTL.ctl
echo "${RUNTIME_CTL} " >> reformat_ROMS_CTL.ctl
echo "${RUN}_${OCEAN_MODEL}_${RUNTYPE}.in " >> reformat_ROMS_CTL.ctl
if [ ! -s $EXECnos/nos_ofs_reformat_ROMS_CTL -a  ! -x  $EXECnos/nos_ofs_reformat_ROMS_CTL ]
then
  echo "$EXECnos/nos_ofs_reformat_ROMS_CTL does not exist"
  msg="$EXECnos/nos_ofs_reformat_ROMS_CTL does not exist  "
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
  err=1;export err;err_chk
  exit
fi

$EXECnos/nos_ofs_reformat_ROMS_CTL < reformat_ROMS_CTL.ctl > nos_ofs_reformat_ROMS_CTL.log
export err=$?
if [ $err -ne 0 ]
then
  if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
  then
     echo "MODEL_CTL_NOWCAST DONE 0 " >> $cormslogfile
  else
     echo "MODEL_CTL_FORECAST DONE 0 " >> $cormslogfile
  fi

  echo "Running $EXECnos/nos_ofs_reformat_ROMS_CTL did not complete normally, FATAL ERROR!"
  msg="Running $EXECnos/nos_ofs_reformat_ROMS_CTL did not complete normally, FATAL ERROR!"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
  err_chk
else
  echo "Running $EXECnos/nos_ofs_reformat_ROMS_CTL completed normally"
  msg="Running $EXECnos/nos_ofs_reformat_ROMS_CTL  completed normally"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
  if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
  then
     echo "MODEL_CTL_NOWCAST DONE 100 " >> $cormslogfile
  else
     echo "MODEL_CTL_FORECAST DONE 100 " >> $cormslogfile
  fi
fi

## if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
## then
   cp -p ${DATA}/${RUN}_${OCEAN_MODEL}_${RUNTYPE}.in ${COMOUT}/${RUN}_${OCEAN_MODEL}_${RUNTYPE}.in 
   cp -p ${DATA}/${RUN}_${OCEAN_MODEL}_${RUNTYPE}.in ${COMOUT}/$RUNTIME_CONTROL 
## fi

echo "RUNTYPE=${RUNTYPE}" >> $cormslogfile
echo "  NSTEP=$NSTEP    " >> $cormslogfile
echo " NTIMES=$NTIMES   " >> $cormslogfile
echo " DSTART=$DSTART   " >> $cormslogfile
echo "END SECTION OF GENERATING $OCEAN_MODEL CONTROL FILE for $RUNTYPE" >> $cormslogfile
echo "GENERATING $OCEAN_MODEL CONTROL FILE for $RUNTYPE COMPLETED SUCCESSFULLY 100" >> $cormslogfile

echo "The script nos_ofs_reformat_roms_ctl.sh $RUNTYPE ends at time: " `date `
exit
