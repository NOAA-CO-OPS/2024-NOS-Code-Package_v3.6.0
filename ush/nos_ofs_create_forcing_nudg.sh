#!/bin/sh
# Script Name:  nos_ofs_create_forcing_nudg.sh
#
# Purpose:
#   This program is used to generate the T/S nudging fields for NOS OFS from 
#   global or regional larger domain ocean model such as G-RTOFS, Navy's NCOM and HYCOM.
#   (DBASE_TS=RTOFS/NCOM/HYCOM set in OFS' main control file). The nudging field data
#   source follows the open boundary conditions for T/S to be consistent. 
#   The most recently available products for the given time is searched and read in.
#   Several horizontal interpolation methods (determined by IGRD_OBC) are implemented,
#   and linear method is used for vertical interpolation from RTOFS/NCOM/HYCOM vertical
#   coordinates to ROMS sigma coordinates. 

#   The missing variables are filled with a missing value of -99.9.
#
# Directory Location:   ~/scripts 
# Technical Contact:   	Aijun Zhang         Org:  NOS/CO-OPS
#                       Phone: 240-533-0591  E-Mail: aijun.zhang@noaa.gov
#
# Usage: ./nos_ofs_create_forcing_nudg.sh OFS 
#
# Input Parameters:
#  OFS:         Name of Operational Forecast System, e.g., TBOFS, CBOFS, DBOFS
#  TIME_START:  start time to grab data, e.g., YYYYMMDDHH (2008101500)
#  TIME_END:    end time to grab data, e.g., YYYYMMDDHH (2008101600)
#  DBASE_TS:    Data source Name of water level open boundary conditions
#
# Language:   Bourne Shell Script      
#
# Target Computer:  Super Computer at NCEP
#
# Estimated Execution Time: 300s 
#
# Suboutines/Functions Called:
#
# Input Files:
#   ROMS ocean model grid netCDF file  
# Output Files:
#   ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.clim.nc   
#   Fortran_nudg.log
#
# Libraries Used: see the makefile
#  
# Error Conditions:
#  
# Modification History:
#   (1) Implementation  01/10/2017
#
# -------------------------------------------------------------------------
set -x

echo 'The script nos_ofs_create_forcing_nudg.sh started at UTC' `date -u +%Y%m%d%H`
echo 'The script nos_ofs_create_forcing_nudg.sh started at UTC' `date -u +%Y%m%d%H` >> $jlogfile
echo 'The script nos_ofs_create_forcing_nudg.sh started at UTC' `date -u +%Y%m%d%H` >> $nosjlogfile

TIME_START=$time_hotstart
TIME_END=$time_forecastend

#  use existing ${DBASE_TS}_FILE from obc script to generate nudging fields
# --------------------------------------------------------------------------
# check if  ${DBASE_TS}_FILE exists in working directory
sleep 30  # waiting for RTOFS_FILE generated from nos_ofs_create_forcing_obc.sh
if [ -s RTOFS_FILE ]; then
  DBASE_TS='RTOFS'
  echo 'Using RTOFS_FILE in generating nudging fields'  >> $jlogfile
  echo 'Using RTFOS_FILE in generating nudging fields'  >> $nosjlogfile  
elif [ -s HYCOM_FILE ]; then
  DBASE_TS='HYCOM'
  echo 'Using HYCOM_FILE in generating nudging fields'  >> $jlogfile
  echo 'Using HYCOM_FILE in generating nudging fields'  >> $nosjlogfile
elif [ -s NCOM_FILE ]; then
  DBASE_TS='NCOM'
  echo 'Using NCOM_FILE in generating nudging fields'  >> $jlogfile
  echo 'Using NCOM_FILE in generating nudging fields'  >> $nosjlogfile
else
  echo 'FATAL ERROR: ${DBASE_TS}_FILE does not exist. Check obc_forcing results!'  >> $jlogfile
  echo 'FATAL ERROR: ${DBASE_TS}_FILE does not exist. Check obc_forcing results!'  >> $nosjlogfile
  err=1;export err;err_chk
  exit
fi

if [ -s Fortran_nudg.ctl ]; then
   rm -f Fortran_nudg.ctl Fortran_nudg.log
fi 
YYYY=`echo $TIME_START | cut -c1-4 `
  MM=`echo $TIME_START | cut -c5-6 `
  DD=`echo $TIME_START | cut -c7-8 `
  HH=`echo $TIME_START | cut -c9-10 `
NUDG_FORCING_FILE_LAST=${PREFIXNOS}.clim.${YYYY}${MM}${DD}.t${HH}z.nc
if [ "${OFS,,}" == "wcofs_da" ]; then
   TIME_LC=`$NDATE +48 $TIME_START`
   YYYY=`echo $TIME_LC | cut -c1-4 `       
   MM=`echo $TIME_LC | cut -c5-6 `
   DD=`echo $TIME_LC | cut -c7-8 `
   HH=`echo $TIME_LC | cut -c9-10 `
   NUDG_FORCING_FILE_LAST=${PREFIXNOS}.clim.${YYYY}${MM}${DD}.t${HH}z.nc
fi
if [ -s ${COMOUTroot}/${OFS}.$YYYY$MM$DD/$NUDG_FORCING_FILE_LAST ]; then
      cp -p ${COMOUTroot}/${OFS}.$YYYY$MM$DD/$NUDG_FORCING_FILE_LAST $DATA/.
fi
echo ${OFS} > Fortran_nudg.ctl
echo $OCEAN_MODEL >> Fortran_nudg.ctl
echo $DBASE_TS  >> Fortran_nudg.ctl
echo ${TIME_START}00 >> Fortran_nudg.ctl
echo ${TIME_END}00 >> Fortran_nudg.ctl
echo $IGRD_OBC >> Fortran_nudg.ctl
if [ $OCEAN_MODEL == "SELFE" -o $OCEAN_MODEL == "selfe" ]; then
    echo $GRIDFILE_LL >> Fortran_nudg.ctl
else
    echo $GRIDFILE >> Fortran_nudg.ctl
fi  
echo $NUDG_FORCING_FILE >> Fortran_nudg.ctl
echo $cormslogfile >> Fortran_nudg.ctl 
echo $BASE_DATE >> Fortran_nudg.ctl 
echo $MINLON  >> Fortran_nudg.ctl
echo $MINLAT >> Fortran_nudg.ctl 
echo $MAXLON >> Fortran_nudg.ctl 
echo $MAXLAT >> Fortran_nudg.ctl 
echo $KBm  >> Fortran_nudg.ctl
if [ $OCEAN_MODEL == "ROMS" -o $OCEAN_MODEL == "roms" ];  then
     echo $THETA_S >> Fortran_nudg.ctl 
     echo $THETA_B >> Fortran_nudg.ctl 
     echo $TCLINE >> Fortran_nudg.ctl 
     echo $NVTRANS  >> Fortran_nudg.ctl
     echo $NVSTR  >> Fortran_nudg.ctl
else    
     echo $VGRID_CTL >> Fortran_nudg.ctl
fi    
echo $NUDG_FORCING_FILE_LAST  >> Fortran_nudg.ctl 
export pgm=nos_ofs_create_forcing_nudg
. prep_step
startmsg
 $EXECnos/nos_ofs_create_forcing_nudg < Fortran_nudg.ctl > Fortran_nudg.log
 export err=$?
     
## output corms flag information
if grep "COMPLETED SUCCESSFULLY" Fortran_nudg.log /dev/null 2>&1
then
     echo "NUDGING FORCING COMPLETED SUCCESSFULLY 100" >> $cormslogfile
     echo "NUDGING_FORCING DONE 100 " >> $cormslogfile
else
     echo "NUDGING FORCING COMPLETED SUCCESSFULLY 0" >> $cormslogfile
     echo "NUDGING_FORCING DONE 0 " >> $cormslogfile
     err=1;export err;err_chk
fi
if [ $err -ne 0 ]; then
     echo "$pgm did not complete normally, FATAL ERROR!"
     msg="$pgm did not complete normally, FATAL ERROR!"
     postmsg "$jlogfile" "$msg"
     postmsg "$nosjlogfile" "$msg"
     err_chk
else
     echo "$pgm completed normally"
     msg="$pgm completed normally"
     postmsg "$jlogfile" "$msg"
     postmsg "$nosjlogfile" "$msg"
fi
if [ -f "$NUDG_FORCING_FILE" ]; then
    cp $NUDG_FORCING_FILE $COMOUT/$NUDG_FORCING_FILE
    if [ $SENDDBN = YES ]; then
      $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/$NUDG_FORCING_FILE
    fi
else
    echo "NO $NUDG_FORCING_FILE File Found"
fi

if [ -f Fortran_nudg.log ]
then
    cp Fortran_nudg.log $COMOUT/Fortran_nudg.t${cyc}z.log
else
    echo "NO Fortran_nudg.log Found"
fi

echo 'The script nos_ofs_create_forcing_nudg.sh ended at UTC' `date -u +%Y%m%d%H`

