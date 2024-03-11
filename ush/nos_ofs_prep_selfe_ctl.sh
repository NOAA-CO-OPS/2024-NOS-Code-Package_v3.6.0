#!/bin/sh
# Scripts Name:  nos_ofs_prep_selfe_ctl.sh
#
# Purpose:
#   Some runtime parameters needed to be changed dynamically.
#   This program is to used generate runtime input control files which are used to run 
#   the J-JOB JNOS_OFS_NOWCAST_FCST.sms.prod 
#
# Location:   /nosofs_shared.v1.0.0/ush
#
# Technical Contact:   	Aijun Zhang         Org:  NOS/CO-OPS
#
# Usage: ./nos_ofs_prep_selfe_ctl.sh OFS RUNTYPE 
#
# Input Parameters:
#  RUN  : Name of OFS
#  RUNTYPE : nowcast|forecast
#
# Language:   Bourne Shell Script      
#
# Target Computer:  IBM Super Computer at NCEP
#
# Input Files:
#    Standard runtime input file  
# Output Files:
#    ${PREFIXNOS}.creofs.nowcast|forecast.${YYYY}${MM}${DD}.t${cyc}z.in
#    ${PREFIXNOS}.creofs.combine.netcdf.sta.nowcast|forecast.${YYYY}${MM}${DD}.t${cyc}z.in
#    ${PREFIXNOS}.creofs.met_ctl.nowcast|forecast.${YYYY}${MM}${DD}.t${cyc}z.in
#    ${PREFIXNOS}.creofs.combine.hotstart.nowcast.${YYYY}${MM}${DD}.t${cyc}z.in
# Modification History:
#
# -------------------------------------------------------------------------
function seton {
  set -x
}
function setoff {
  set +x
}
seton
  
echo 'The script nos_ofs_prep_selfe_ctl.sh starts at time: ' `date `

RUN=$1          
RUNTYPE=$2      

echo "BEGIN SECTION OF GENERATING $OCEAN_MODEL CONTROL FILE for $RUNTYPE" >> $cormslogfile
echo 'The script nos_ofs_prep_selfe_ctl.sh has started at UTC' `date -u +%Y%m%d%H`
echo 'The script nos_ofs_prep_selfe_ctl.sh has started at UTC' `date -u +%Y%m%d%H` >> $jlogfile

NC_OUT_INTERVAL=`expr $NHIS / ${DELT_MODEL%.*}`
NC_STA_INTERVAL=`expr $NSTA / ${DELT_MODEL%.*}`
#export nnh=`$NHOUR $time_nowcastend ${time_hotstart} `
nnh=3  
IHFSKIP_VALUE=`expr $nnh \* 3600 / ${DELT_MODEL%.*}`

if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
then
  IHOT_VALUE=1
  RUNTIME_CONTROL=${RUNTIME_CTL_NOWCAST}
  RUNTIME_MET_CONTROL=$RUNTIME_MET_CTL_NOWCAST
  RUNTIME_COMBINE_FIELD_CONTROL=$RUNTIME_COMBINE_NETCDF_NOWCAST
  RUNTIME_COMBINE_STA_CONTROL=$RUNTIME_COMBINE_NETCDF_STA_NOWCAST

  TIME_START=${time_hotstart}
  TIME_END=$time_nowcastend
  export yhst=`echo $time_hotstart |cut -c1-4`
  export mhst=`echo $time_hotstart |cut -c5-6`
  export dhst=`echo $time_hotstart |cut -c7-8`
  export hhst=`echo $time_hotstart |cut -c9-10`
  export nnh=`$NHOUR $TIME_END $TIME_START `
  export NTIME=`expr $nnh \* 3600 / ${DELT_MODEL%.*}`
  export NTIME_STA=`expr $nnh \* 3600 / ${NSTA}`

  export RNDAY_VALUE=$(echo "scale=4;$nnh / 24.0" | bc)
# AJ 04/11/12 bc command is for an arbitrary precision calculator  
# while "scale" specify the number if digits after the decimal point in 
#              the above expression
  RST_OUT_INTERVAL=`expr $nnh \* 3600 / ${DELT_MODEL%.*}`


elif [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]
then
  IHOT_VALUE=2
  RUNTIME_CONTROL=${RUNTIME_CTL_FORECAST}
  RUNTIME_MET_CONTROL=$RUNTIME_MET_CTL_FORECAST
  RUNTIME_COMBINE_FIELD_CONTROL=$RUNTIME_COMBINE_NETCDF_FORECAST
  RUNTIME_COMBINE_STA_CONTROL=$RUNTIME_COMBINE_NETCDF_STA_FORECAST
  TIME_START=${time_hotstart}
  TIME_END=$time_forecastend
  export yhst=`echo $time_hotstart |cut -c1-4`
  export mhst=`echo $time_hotstart |cut -c5-6`
  export dhst=`echo $time_hotstart |cut -c7-8`
  export hhst=`echo $time_hotstart |cut -c9-10`
  export nnh=`$NHOUR $TIME_END $TIME_START `
  export NTIME=`expr $nnh \* 3600 / ${DELT_MODEL%.*}`
  export RNDAY_VALUE=$(echo "scale=4;$nnh / 24.0" | bc)

  export hour_now=`$NHOUR $TIME_END $time_nowcastend `
  export NTIME_STA=`expr $hour_now \* 3600 / ${NSTA}`
  RST_OUT_INTERVAL=`expr $hour_now \* 3600 / ${DELT_MODEL%.*}`
fi

## run control file
    sed -e "s/cpp_lon_value/${CPP_LON_VALUE}/g" \
        -e "s/cpp_lat_value/${CPP_LAT_VALUE}/g" \
        -e "s/ihot_value/${IHOT_VALUE}/g" \
        -e "s/ihorcon_value/${IHORCON_VALUE}/g" \
        -e "s/ihdif_value/${IHDIF_VALUE}/g" \
        -e "s/idrag_value/${IDRAG_VALUE}/g" \
        -e "s/bfric_value/${BFRIC_VALUE}/g" \
        -e "s/ihhat_value/${IHHAT_VALUE}/g" \
        -e "s/inunfl_value/${INUNFL_VALUE}/g" \
        -e "s/h0_value/${MIN_DEPTH}/g" \
        -e "s/rnday_value/${RNDAY_VALUE}/g" \
        -e "s/dt_value/${EXTSTEP_SECONDS}/g" \
        -e "s/nws_value/${NWS_VALUE}/g" \
        -e "s/wtiminc_value/${EXTSTEP_SECONDS}/g" \
        -e "s/nrampwind_value/${NRAMPWIND_VALUE}/g" \
        -e "s/drampwind_value/${DRAMPWIND_VALUE}/g" \
        -e "s/iwindoff_value/${IWINDOFF_VALUE}/g" \
        -e "s/step_nu_value/${STEP_NU_VALUE}/g" \
        -e "s/nspool_value/${NC_OUT_INTERVAL}/g" \
        -e "s/ihfskip_value/${IHFSKIP_VALUE}/g" \
        -e "s/nspool_sta_value/${NC_STA_INTERVAL}/g" \
        -e "s/hotout_write_value/${RST_OUT_INTERVAL}/g" \
                              ${FIXofs}/${RUNTIME_CTL}        | \
    sed -n "/DUMMY/!p"               > runtime.tmp2

    sed -e "s/?/\//g"  runtime.tmp2 > ${RUN}_selfe_${RUNTYPE}.in 

   cp -p ${RUN}_selfe_${RUNTYPE}.in ${COMOUT}/$RUNTIME_CONTROL 
   rm -f runtime.tmp?

#CHECK FOR MET CONTROL FILE
    if [ -s $FIXofs/$RUNTIME_MET_CTL ]
    then
      echo "MET control files exist"
      cat $FIXofs/$RUNTIME_MET_CTL | sed -e s/HYYY/$yhst/g -e s/HM/$mhst/g -e s/HD/$dhst/g -e s/HH/$hhst/g \
        > $DATA/sflux_inputs.txt
      cp -p $DATA/sflux_inputs.txt ${COMOUT}/$RUNTIME_MET_CONTROL
    else
      msg="FATAL ERROR: No MET control file for $RUNTYPE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo " "
      echo "************************************************************** "
      echo "*** FATAL ERROR : No MET control file for $RUNTYPE  *** "
      echo "************************************************************** "
      echo " "
      echo $msg
      seton
#      touch err.${RUN}.$PDY1.t${HH}z
      err=1;export err;err_chk
    fi
# Prepare CONTROL FILE for combining station netcdf
    if [ -s $FIXofs/$RUNTIME_COMBINE_NETCDF_STA ]
    then
      NTIME_STA=`expr ${NTIME_STA} + 1`   ## AJ 4/11/12 output initial record
      sed -e s/NSTATION/$NSTATION/g \
          -e s/NVAR/$NVAR/g  \
	  -e s/NVRT/$nvrt/g  \
	  -e s/NTIME/${NTIME_STA}/g \
	  -e s/STA_CTLFILE/${STA_NETCDF_CTL}/g \
	  -e s/BASE_DATE/$BASE_DATE/g \
          -e s/PREFIXNOS/$PREFIXNOS/g \
	  $FIXofs/$RUNTIME_COMBINE_NETCDF_STA | \
         sed -n "/DUMMY/!p"               > runtime.tmp2
      sed -e "s/?/\//g"  runtime.tmp2 > ${COMOUT}/$RUNTIME_COMBINE_STA_CONTROL

#      cat $FIXofs/$RUNTIME_COMBINE_NETCDF_STA | sed -e s/NSTATION/$NSTATION/g -e s/NVAR/$NVAR/g -e s/NVRT/$nvrt/g -e s/NTIME/$hhst/g \
#        > ${COMOUT}/RUNTIME_COMBINE_FIELD_CONTROL
    else
      msg="FATAL ERROR: No control file for combining station outputs of $RUNTYPE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo " "
      echo "************************************************************** "
      echo "*** FATAL ERROR : No control file for combining station outputs of $RUNTYPE  *** "
      echo "************************************************************** "
      echo " "
      echo $msg
      seton
#      touch err.${RUN}.$PDY1.t${HH}z
      err=1;export err;err_chk
    fi


    if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
    then
      if [ -s $FIXofs/$RUNTIME_COMBINE_RST  ]
      then
        echo "hotstart combine control file exists"
        cat $FIXofs/$RUNTIME_COMBINE_RST  | sed -e s/TSP/$NTIME/g -e s/TOTAL_TASKS/$TOTAL_TASKS/g > $RUNTIME_COMBINE_RST_NOWCAST 
	cp -p $RUNTIME_COMBINE_RST_NOWCAST ${COMOUT}/.
      else
        msg="FATAL ERROR: No control file for hotstart combine"
        postmsg "$jlogfile" "$msg"
        postmsg "$nosjlogfile" "$msg"
        setoff
        echo ' '
        echo '************************************************************** '
        echo '*** FATAL ERROR : No control file for hotstart combine  *** '
        echo '************************************************************** '
        echo ' '
        echo $msg
        seton
        err=1;export err;err_chk
      fi
    fi
#   echo "HH PDY1 "  
#   echo $HH
#   echo $PDY1


  if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
  then
     echo "MODEL_CTL_NOWCAST DONE 100 " >> $cormslogfile
  else
     echo "MODEL_CTL_FORECAST DONE 100 " >> $cormslogfile
  fi


echo "RUNTYPE=${RUNTYPE}" >> $cormslogfile
echo "END SECTION OF GENERATING $OCEAN_MODEL CONTROL FILE for $RUNTYPE" >> $cormslogfile
echo "GENERATING $OCEAN_MODEL CONTROL FILE for $RUNTYPE COMPLETED SUCCESSFULLY 100" >> $cormslogfile

echo "The script nos_ofs_prep_selfe_ctl.sh $RUNTYPE ends at time: " `date `
exit
