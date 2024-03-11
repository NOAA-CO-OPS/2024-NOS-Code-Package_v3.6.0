#!/bin/sh
# Scripts Name:  nos_ofs_prep_fvcom_ctl.sh
#
# Purpose:
#   Some runtime parameters needed to be changed dynamically.
#   This program is to generate runtime input parameter file 
#
# Location:   ~/scripts
#
# Technical Contact:   	Degui Cao         Org:  NOS/CO-OPS
#
# Usage: ./nos_ofs_prep_fvcom_ctl.sh RUNTYPE 
#
# Input Parameters:
#  NNFILES  : Number of forcing files for this simulation
#
# Language:   Bourne Shell Script      
#
# Target Computer:  IBM Super Computer at NCEP
#
# Input Files:
#    Standard runtime input file  
# Output Files:
#    {OFS}_roms_{RUNTYPE}.in
#
# Modification History:
#
# -------------------------------------------------------------------------
  set -xa

  echo 'The script nos_ofs_prep_fvcom_ctl.sh starts at time: ' `date `

RUN=$1          
RUNTYPE=$2      

echo "BEGIN SECTION OF GENERATING $OCEAN_MODEL CONTROL FILE for $RUNTYPE" >> $cormslogfile
echo 'The script nos_ofs_prep_fvcom_ctl.sh has started at UTC' `date -u +%Y%m%d%H`
echo 'The script nos_ofs_prep_fvcom_ctl.sh has started at UTC' `date -u +%Y%m%d%H` >> $jlogfile

if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
then
  INI_FILE=${INI_FILE_NOWCAST} 
  RST_OUT_FILE=${RST_OUT_NOWCAST}
  STA_OUT_FILE=${STA_OUT_NOWCAST}
  MET_NETCDF_1=${MET_NETCDF_1_NOWCAST}
  MET_NETCDF_2=${MET_NETCDF_2_NOWCAST}
  RUNTIME_CONTROL=${RUNTIME_CTL_NOWCAST}
  TIME_START=${time_hotstart}
  TIME_END=$time_nowcastend
elif [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]
then
  INI_FILE=${RST_OUT_NOWCAST} 
  RST_OUT_FILE=${RST_OUT_FORECAST}
  STA_OUT_FILE=${STA_OUT_FORECAST}
  MET_NETCDF_1=${MET_NETCDF_1_FORECAST}
  MET_NETCDF_2=${MET_NETCDF_2_FORECAST}
  RUNTIME_CONTROL=${RUNTIME_CTL_FORECAST}
  if [ -f ${FIXofs}/$RUNTIME_CTL_FOR ]; then
     RUNTIME_CTL=$RUNTIME_CTL_FOR
  fi

  TIME_START=${time_nowcastend}
  TIME_END=$time_forecastend
#  RST_OUT_INTERVAL=`printf '%.0f' $RST_OUT_INTERVAL` ## convert decimal number to integer, abd get the nearest integer
  RST_OUT_INTERVAL=${RST_OUT_INTERVAL%.*} # just tuncate the integer part and remove the fractional part   
  RST_OUT_INTERVAL=`expr ${RST_OUT_INTERVAL} \* 1`
  RST_OUT_INTERVAL=`printf '%.1f' $RST_OUT_INTERVAL`
#  RST_OUT_INTERVAL=172800.0    # for forecast cycle write restart file every 2 days
  if [ "$CONTINUE_FORECAST" == "YES" ]; then
    INI_FILE=restart_continue_forecast.nc
    RST_OUT_FILE=${RST_OUT_FORECAST}
    STA_OUT_FILE=${STA_OUT_FORECAST}
    MET_NETCDF_1=${MET_NETCDF_1_FORECAST}
    MET_NETCDF_2=${MET_NETCDF_2_FORECAST}
    RUNTIME_CONTROL=${RUNTIME_CTL_FORECAST}
    if [ -f ${FIXofs}/$RUNTIME_CTL_FOR ]; then
      RUNTIME_CTL=$RUNTIME_CTL_FOR
    fi
    TIME_START=${time_continue_forecast}
    TIME_END=$time_forecastend
    RST_OUT_INTERVAL=${RST_OUT_INTERVAL%.*} # just tuncate the integer part and remove the fractional part   
    RST_OUT_INTERVAL=`printf '%.1f' $RST_OUT_INTERVAL`
  fi
fi

# Input parameter in control file
#    if [ $COLD_START == "T" ]
#    then
#         STARTUP_TYPE='coldstart'
#    else
         STARTUP_TYPE='hotstart'
#    fi

    if [ $STARTUP_TYPE == "coldstart" ]
    then
      if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
      then
      STARTUP_FILE='none'
      else
      STARTUP_FILE=$INI_FILE
      fi
   
      STARTUP_UV_TYPE='default'
      STARTUP_TURB_TYPE='default'
      STARTUP_TS_TYPE='constant' 

    else
      STARTUP_FILE=$INI_FILE
      STARTUP_UV_TYPE='set values'
      STARTUP_TURB_TYPE='set values'
      STARTUP_TS_TYPE='set values'
    fi

   yy1=`echo $TIME_START | cut -c1-4`
   mm1=`echo $TIME_START | cut -c5-6`
   dd1=`echo $TIME_START | cut -c7-8`
   hh1=`echo $TIME_START | cut -c9-10`
   START_DATE=`echo $yy1-$mm1-$dd1 $hh1:00:00` 

   yy2=`echo $TIME_END | cut -c1-4`
   mm2=`echo $TIME_END | cut -c5-6`
   dd2=`echo $TIME_END | cut -c7-8`
   hh2=`echo $TIME_END | cut -c9-10`
   END_DATE=`echo $yy2-$mm2-$dd2 $hh2:00:00`

    INPUT_DIR=$DATA
    OUTPUT_DIR=$DATA
    Surface_Forcing=${MET_NETCDF_1}
    OBC_FILE=$OBC_FORCING_FILE 
   echo "DATA DIR in =" $DATA 
## run control file
    sed -e "s/startuptype/$STARTUP_TYPE/g" \
        -e "s/rstfile/$STARTUP_FILE/g" \
        -e "s/uvtype/$STARTUP_UV_TYPE/g" \
        -e "s/turbtype/$STARTUP_TURB_TYPE/g" \
        -e "s/tstype/$STARTUP_TS_TYPE/g" \
        -e "s/startdate/$START_DATE/g" \
        -e "s/enddate/$END_DATE/g" \
        -e "s/srffile/${Surface_Forcing}/g" \
        -e "s/obcfile/${OBC_FILE}/g" \
        -e "s/obc_el_file/${OBC_FORCING_FILE_EL}/g" \
        -e "s/obc_ts_file/${OBC_FORCING_FILE_TS}/g" \
        -e "s/hfluxfile/${MET_NETCDF_2}/g" \
        -e "s/irpt/${IREPORT}/g" \
        -e "s/extsec/${EXTSTEP_SECONDS}/g" \
        -e "s/ispt/${ISPLIT}/g" \
        -e "s/mindepth/${MIN_DEPTH}/g" \
        -e "s/rstoutint/${RST_OUT_INTERVAL}/g" \
        -e "s/ncoutint/${NC_OUT_INTERVAL}/g" \
        -e "s/ncsfoutint/${NCSF_OUT_INTERVAL}/g" \
        -e "s/heatingll/${HEATING_LONGWAVE_LENGTHSCALE}/g" \
        -e "s/heatinglp/${HEATING_LONGWAVE_PERCTAGE}/g" \
        -e "s/heatingsl/${HEATING_SHORTWAVE_LENGTHSCALE}/g" \
        -e "s/rivernum/${NRIVERS}/g" \
        -e "s/sta_out_interval/$NC_STA_INTERVAL/g" \
        -e "s/nestingblock/${NESTING_BLOCKSIZE}/g" \
                              ${FIXofs}/${RUNTIME_CTL}        | \
#                              ${FIXofs}/${PREFIXNOS}.ngofs.run_control.nml        | \
    sed -n "/DUMMY/!p"               > runtime.tmp2

    sed -e "s/?/\//g"  runtime.tmp2 >${RUN}_fvcom_${RUNTYPE}.in 

   cp -p ${DATA}/${RUN}_fvcom_${RUNTYPE}.in ${COMOUT}/$RUNTIME_CONTROL 
   rm -f runtime.tmp?

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

echo "The script nos_ofs_prep_fvcom_ctl.sh $RUNTYPE ends at time: " `date `
exit
