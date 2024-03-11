#!/bin/sh

# ############################################################################
#  Script Name:  exnos_ofs_continue_forecast.sh 
#  Purpose:                                                                   #
#  This is the main script to continue forecast simulations                   #
#  after system interruption not for model blowup due to dynamic instability
# Location:   ~/jobs
# Technical Contact:    Aijun Zhang         Org:  NOS/CO-OPS
#                       Phone: 240-533-0591
#                       E-Mail: aijun.zhang@noaa.gov
#
# Usage: 
#
# Input Parameters:
#  OFS 
#
# Modification History:
#      
# ##########################################################################

set -x
#PS4=" \${SECONDS} \${0##*/} L\${LINENO} + "
runtype='forecast'
#  Control Files For Model Run
if [ -s ${FIXofs}/${PREFIXNOS}.ctl ]
then
  . ${FIXofs}/${PREFIXNOS}.ctl
  if [ -n "$LSB_DJOB_NUMPROC" ] && [ $TOTAL_TASKS -ne $LSB_DJOB_NUMPROC ]; then
    err_exit "Number of tasks/CPUs ($LSB_DJOB_NUMPROC) does not meet job requirements (see ${FIXofs}/${PREFIXNOS}.ctl)."
  fi
else
  echo "${RUN} control file is not found, FATAL ERROR!"
  echo "please provide  ${RUN} control file of ${PREFIXNOS}.ctl in ${FIXofs}"
  msg="${RUN} control file is not found, FATAL ERROR!"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
  msg="please provide  ${RUN} control file of ${PREFIXNOS}.ctl in ${FIXofs}"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
  echo "${RUN} control file is not found, FATAL ERROR!"  >> $cormslogfile
  err_chk
fi
echo "run the launch script to set the NOS configuration"
. $USHnos/nos_ofs_launch.sh $OFS $runtype
export pgm="$USHnos/nos_ofs_launch.sh $OFS nowcast"
export err=$?
if [ $err -ne 0 ]
then
   echo "Execution of $pgm did not complete normally, FATAL ERROR!"
   echo "Execution of $pgm did not complete normally, FATAL ERROR!" >> $cormslogfile
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
fi
## check the latest restart file before OFS stopped
COLD_START="F"
if [ ! -d $DATA ]; then
  echo "working direction of the previous NF run is not found"
  echo "WARNING: Cannot continuous forecast"
  echo "Have to rerun the complete cycle of Nowcast/Forecast"
  exit
else
## save model output files generated before model breaking into a temporory folder
#  mkdir backup
#  cp -p *${OFS}?station*.nc backup/.
 
  if [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]; then
# rename the station file to avoid overwritten
    if [ -s ${OFS}_station_timeseries.nc ]; then
      mv ${OFS}_station_timeseries.nc ${OFS}_station_timeseries.nc.old
    fi
    NFILE=`ls -al *${OFS}_restart*.nc | wc -l`
    if [ $NFILE -gt 0 ]; then
      latest_restart_f=`ls -trl *${OFS}_restart*.nc | tail -1 | awk '{print $NF}' `
    fi
# Rename output files before continuing forecast to avoid overwriting
    NFILE=`ls -al *${OFS}_surface_????.nc | wc -l`
    if [ $NFILE -gt 0 ]; then
       $USHnos/nos_ofs_rename.sh $OFS $OCEAN_MODEL $runtype 2d "$time_nowcastend" "$time_nowcastend"
    fi
    NFILE=`ls -al *${OFS}_????.nc | wc -l`
    if [ $NFILE -gt 0 ]; then
       $USHnos/nos_ofs_rename.sh $OFS $OCEAN_MODEL $runtype 3d "$time_nowcastend " "$time_nowcastend"
    fi
  elif [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]; then
     NFILE=`ls -al *.rst.forecast*.nc | wc -l`
     if [ $NFILE -gt 0 ]; then
       latest_restart_f=`ls -trl  *.rst.forecast*.nc | tail -1 | awk '{print $NF}' `
     fi
# Rename output files before continuing forecast to avoid overwritten
     NFILE=`ls -al *.stations.forecast*.nc | wc -l`
     if [ $NFILE -gt 0 ]; then
       latest_station_f=`ls -trl  *.stations.forecast*.nc | tail -1 | awk '{print $NF}' `
       mv  $latest_station_f ${latest_station_f}.old
     fi     

     NFILE=`ls -al *.fields.forecast*.nc | wc -l`
     if [ $NFILE -gt 0 ]; then
       $USHnos/nos_ofs_rename.sh $OFS $OCEAN_MODEL $runtype 3d "$time_nowcastend" "$time_nowcastend"
     fi
     NFILE=`ls -al *.surface.forecast*.nc | wc -l`
     if [ $NFILE -gt 0 ]; then
       $USHnos/nos_ofs_rename.sh $OFS $OCEAN_MODEL $runtype 2d "$time_nowcastend" "$time_nowcastend"
     fi
  fi
fi
if [ ! -s  $latest_restart_f ]; then
  echo "no restart file is not found in the working dir"
  echo "WARNING: Cannot continuous forecast"
  echo "Have to rerun the complete cycle of Nowcast/Forecast"
  exit
else
  cp -p  $latest_restart_f restart_continue_forecast.nc
  export time_forecastend=`$NDATE $LEN_FORECAST $time_nowcastend`
  echo ${RUN} > Fortran_read_restart.ctl
  echo $OCEAN_MODEL  >> Fortran_read_restart.ctl
  echo $COLD_START  >> Fortran_read_restart.ctl
  echo $GRIDFILE  >> Fortran_read_restart.ctl
  echo restart_continue_forecast.nc  >> Fortran_read_restart.ctl    
  echo ${RUN}_time_initial.dat  >> Fortran_read_restart.ctl
  echo $time_forecastend >> Fortran_read_restart.ctl
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
     echo $BASE_DATE 0 0.0d0 0.0d0 > ${RUN}_time_initial.dat
  fi    
  if grep "COMPLETED SUCCESSFULLY" Fortran_read_restart.log /dev/null 2>&1
  then
       echo "RESTART_TIME DONE 100" >> $cormslogfile
  else
       echo "RESTART_TIME  DONE 0" >> $cormslogfile
       echo "Please check Fortran_read_restart.log for details"
       err=99
       exit
  fi

  read time_hotstart_new NTIMES DAY0 TIDE_START < ${RUN}_time_initial.dat
  export time_continue_forecast=$time_hotstart_new


# --------------------------------------------------------------------------------------
#  Define file names used for model run
# --------------------------------------------------------------------------------------
  export NH_FORECAST=`$NHOUR $time_forecastend $time_hotstart_new `
  export NSTEP_FORECAST=`expr $NH_FORECAST \* 3600 / ${DELT_MODEL}`
  export NTIMES_FORECAST=$NSTEP_FORECAST      #for newer version than 859
  export PDY1=$YYYY$MM$DD
  export DSTART_FORECAST=$DAY0
  export NRST=`expr $NRST / ${DELT_MODEL}`
  if [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]; then
    if [ $NH_FORECAST -gt 0 ]; then
      echo "Preparing FVCOM Control File for forecast"
      if [ -s $COMOUT/$RUNTIME_CTL_FORECAST ]; then
          cp -p $COMOUT/$RUNTIME_CTL_FORECAST ${RUNTIME_CTL_FORECAST}.ori
      fi
      $USHnos/nos_ofs_prep_fvcom_ctl.sh  $OFS forecast
      export err=$?
      if [ $err -ne 0 ]; then
        echo "Execution of forecast ctl did not complete normally, FATAL ERROR! "
        echo "Execution of forecast ctl did not complete normally, FATAL ERROR! " >> $cormslogfile
        msg=" Execution of forecast ctl did not complete normally, FATAL ERROR! "
        postmsg "$jlogfile" "$msg"
        err_chk
      else
        echo "Execution of forecast ctl completed normally"
        echo "Execution of forecast ctl completed normally" >> $cormslogfile
        msg=" Execution of forecast ctl completed normally"
        postmsg "$jlogfile" "$msg"
      fi
    fi
  elif [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]; then
    if [ $NH_FORECAST -gt 0 ]; then
      echo "Preparing ROMS Control File for forecast"
       if [ -s $COMOUT/$RUNTIME_CTL_FORECAST ]; then
          cp -p $COMOUT/$RUNTIME_CTL_FORECAST ${RUNTIME_CTL_FORECAST}.ori
          cp -p $COMOUT/$RUNTIME_CTL_FORECAST ${RUN}_${OCEAN_MODEL}_${runtype}.in
       fi
     
      if [ ! -s ${RUN}_${OCEAN_MODEL}_${runtype}.in ]; then
        if [ -s $COMOUT/$RUNTIME_CTL_FORECAST ]; then
          cp -p $COMOUT/$RUNTIME_CTL_FORECAST .
          cp -p $COMOUT/$RUNTIME_CTL_FORECAST  ${RUN}_${OCEAN_MODEL}_${runtype}.in
        fi
      fi
      cp -p ${RUN}_${OCEAN_MODEL}_${runtype}.in $RUNTIME_CTL
      $USHnos/nos_ofs_prep_roms_ctl.sh  $OFS forecast
      cp -p ${RUNTIME_CTL_FORECAST}.ori $COMOUT/$RUNTIME_CTL_FORECAST
      cp -p 
    fi
  fi
fi

if [ $NH_FORECAST -gt 0 ] 
then
#####    Run forecast simulation
  runtype='forecast'
  echo "     " >> $jlogfile 
  echo "     " >> $nosjlogfile 
  echo " Start nos_ofs_nowcast_forecast.sh $runtype at : `date`" >> $jlogfile
  echo " Start nos_ofs_nowcast_forecast.sh $runtype at : `date`" >> $nosjlogfile
  echo "Running nos_ofs_nowcast_forecast.sh $runtype at : `date`" >> $jlogfile
  echo "Running nos_ofs_nowcast_forecast.sh $runtype at : `date`" >> $nosjlogfile
  echo " Start nos_ofs_nowcast_forecast.sh $runtype at : `date`" 
  export pgm="$USHnos/nos_ofs_nowcast_forecast.sh $runtype"
  $USHnos/nos_ofs_nowcast_forecast.sh $runtype 
  export err=$?
  if [ $err -ne 0 ]; then
   echo "Execution of $pgm did not complete normally, FATAL ERROR!"
   echo "Execution of $pgm did not complete normally, FATAL ERROR!" >> $cormslogfile
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
  fi
  echo "end of nos_ofs_nowcast_forecast.sh $runtype"
  cp -p ${RUNTIME_CTL_FORECAST}.ori $COMOUT/$RUNTIME_CTL_FORECAST
  cp -p ${RUN}_${OCEAN_MODEL}_${runtype}.in $COMOUT/${RUNTIME_CTL_FORECAST}.rerun
# merge the two station output files to one
  if [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]; then
    if [ -s $STA_OUT_FORECAST ]; then
      mv ${STA_OUT_FORECAST} ${OFS}_station_timeseries.nc.new
    fi
    python ${PYnos}/mergeStationFiles.py ${OFS}_station_timeseries.nc.old ${OFS}_station_timeseries.nc.new $STA_OUT_FORECAST
    export err=$?
    if [ $err -ne 0 ]; then
      echo "FETAL ERROR : python module may not be loaded properly"
      err_chk
    fi  
  elif [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]; then
    NFILE=`ls -al *.stations.forecast*.nc | wc -l`
    if [ $NFILE -gt 0 ]; then
      latest_station_new=`ls -trl  *.stations.forecast*.nc | tail -1 | awk '{print $NF}' `
      mv ${latest_station_new} ${latest_station_new}.new
    fi 
    NFILE=`ls -al *.stations.forecast*.nc.old | wc -l`
    if [ $NFILE -gt 0 ]; then
      latest_station_f=`ls -trl  *.stations.forecast*.nc.old | tail -1 | awk '{print $NF}' `
    fi 
    python ${PYnos}/mergeStationFiles.py ${latest_station_f} ${latest_station_new}.new $STA_OUT_FORECAST
    export err=$?
    if [ $err -ne 0 ]; then
      echo "FETAL ERROR : python module may not be loaded properly"
      err_chk
    fi
  fi
else 
  echo "Continuous forecast hours from the previous run is  " $NH_FORECAST >> $jlogfile
  echo "That means the forecast simulation was already completed in the previous run " >> $jlogfile
  echo "no forecast simulation is needed " >> $jlogfile 
  echo "Continuous forecast hours from the previous run is  " $NH_FORECAST >> $nosjlogfile
  echo "That means the forecast simulation was already completed     " >> $nosjlogfile
  echo "no forecast simulation is needed " >> $nosjlogfile

  if [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]; then
    if [ -s ${OFS}_station_timeseries.nc.old ]; then
      mv ${OFS}_station_timeseries.nc.old  $STA_OUT_FORECAST
    fi
  elif [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]; then
    NFILE=`ls -al *.stations.forecast*.nc.old | wc -l`
    if [ $NFILE -gt 0 ]; then
      latest_station_f=`ls -trl  *.stations.forecast*.nc.old | tail -1 | awk '{print $NF}' `
      mv  $latest_station_f $STA_OUT_FORECAST
    fi
  fi
fi
##  archive forecast outputs 
export pgm="$USHnos/nos_ofs_archive.sh $runtype"
$USHnos/nos_ofs_archive.sh $runtype 
export err=$?
if [ $err -ne 0 ]; then
   echo "Execution of $pgm did not complete normally, FATAL ERROR!"
   echo "Execution of $pgm did not complete normally, FATAL ERROR!" >> $cormslogfile
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
fi

# if [ $envir = "dev" ]; then
#  # for development copy outputs to CO-OPS via sftp push 
#   $USHnos/nos_ofs_sftp.sh $runtype
# fi
if [ $SENDDBN = YES ]; then
  $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $nosjlogfile
fi

          echo "                                    "
          echo "END OF NOWCAST/FORECAST SUCCESSFULLY"
          echo "                                    "
###############################################################
