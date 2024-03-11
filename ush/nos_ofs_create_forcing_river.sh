#!/bin/sh
#
# Script name:  nos_ofs_create_forcing_river.sh
#
# Purpose:
#   This program is used to read real time USGS river observations
#   from BUFR data files located in the NCEP/NCO 'data tank'.  The 
#   Fortran program relies on NCO BUFRLIB software. The NetCDF
#   river forcing file for ROMS is generated. The Bufr river files
#   in the given time period is read in and decoded, the missing
#   variables are filled with a missing value of -99.99.  The river
#   climatological data (multiple-year daily mean from USGS) are used
#   in the cases of either no real-time observation available in the
#   time period or the River_flag in the river control file is zero.
#
# Location:   ~/scripts
#
# Technical Contact:   	Aijun Zhang         Org:  NOS/CO-OPS
#                       Phone: (301)713-2890 ext. 127  
#                       E-Mail: aijun.zhang@noaa.gov
#
#                       John G.W. Kelley 
#                       Coast Survey Development Lab/MMAP
#                       NOAA/National Ocean Service
#                       John.Kelley@noaa.gov
#                       Phone: (603)862-1628  
#                       E-Mail: john.kelley@noaa.gov
#
# Usage: ./nos_ofs_create_forcing_river.sh ${RUN} 
#
# Input Parameters:
#  OFS:         Name of Name of Operational Forecast System, e.g., TBOFS, CBOFS, DBOFS
#  time_start:  Start time to grab data, YYYYMMDDHH (2008101500)
#  time_end:    End time to grab data, YYYYMMDDHH (2008101600)
#
# Language:   Bourne Shell Script      
#
# Target Computer: IBM Supper Computer at NCEP
#
# Estimated Execution Time: 120s 
#
# Suboutines/Functions Called:
#    nos_ofs_create_forcing_river.f
#      
# Input Files:
#                                
# Output Files:
#   ${PREFIXNOS}.${cycle}.$yyyy$mm$dd.river.nc
#   Fortran_river.log
#
# Libraries Used: see the makefile
#  
# Error Conditions:
#
# Revisions History:
#   (1)  Degui Cao     01/08/2010 
#        Implement in the CSS OS.        
#   (2)  Lianyuan Zheng  01/22/2021
#        If NWM data available, using NWM data for river forcing.
# -------------------------------------------------------------------

set -x

echo 'start running nos_ofs_create_forcing_river.sh'
TIME_START=${time_hotstart}
TIME_END=${time_forecastend}
echo ${RUN} 
echo ${TIME_START}
echo ${TIME_END}

#  For sure to get enough data, two more hours will be acquired.
#  AJ 03/20/2020, retrieve previous 24 hours observations 
TIME_START=` ${NDATE} -24 ${TIME_START} `
TIME_END=` ${NDATE} +1 ${TIME_END} `

YYYY=`echo ${TIME_START} | cut -c1-4 `
MM=`echo ${TIME_START} |cut -c5-6 `
DD=`echo ${TIME_START} |cut -c7-8 `
HH=`echo ${TIME_START} |cut -c9-10 `

YYYYE=`echo ${TIME_END} | cut -c1-4 `
MME=`echo ${TIME_END} |cut -c5-6 `
DDE=`echo ${TIME_END} |cut -c7-8 `
HHE=`echo ${TIME_END} |cut -c9-10 `
echo 'The script nos_ofs_create_forcing_river.sh starts at UTC' `date`
echo 'The script nos_ofs_create_forcing_river.sh starts at UTC' `date` >> ${jlogfile}
echo 'The script nos_ofs_create_forcing_river.sh starts at UTC' `date` >> ${nosjlogfile}

#  Find BUFR file at forecast project hour 00 of 
if [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]; then
  if [ ! -d RIVER ]; then
    mkdir RIVER
    chmod 755 RIVER
  fi
fi

rm -f Fortran_river.ctl Fortran_river.log
BIO_MODULE=${BIO_MODULE:-0}
echo ${RUN} > Fortran_river.ctl
echo ${OCEAN_MODEL} >> Fortran_river.ctl
echo ${TIME_START}00 >> Fortran_river.ctl
echo ${TIME_END}00 >> Fortran_river.ctl
echo ${GRIDFILE} >> Fortran_river.ctl
echo ${DCOMINusgs} >> Fortran_river.ctl
echo ${DCOMINports} >> Fortran_river.ctl
echo ${NOSBUFR} >> Fortran_river.ctl
echo ${USGSBUFR} >> Fortran_river.ctl
echo ${FIXofs} >> Fortran_river.ctl
echo ${RIVER_CTL_FILE} >> Fortran_river.ctl
echo ${RIVER_CLIM_FILE} >> Fortran_river.ctl
echo ${RIVER_FORCING_FILE} >> Fortran_river.ctl
echo ${BASE_DATE} >> Fortran_river.ctl 
echo ${KBm} >> Fortran_river.ctl
echo ${BIO_MODULE} >> Fortran_river.ctl
echo ${cormslogfile} >> Fortran_river.ctl 
echo ${COMOUTroot} >> Fortran_river.ctl
  echo ${PREFIXNOS}  >> Fortran_river.ctl
${USHnos}/nos_ofs_nwm_collectfile.sh ${RUN} ${TIME_START}00 ${TIME_END}00

#if [ -d ${FIXofs}/$NWM_REACHID_FILE -o ! -s ${FIXofs}/$NWM_REACHID_FILE ]; then
#  echo "WARNING: $NWM_REACHID_FILE is not found"
#  echo "Use USGS observations for river forcing conditions"
#elif [ -s ${FIXofs}/$NWM_REACHID_FILE ]; then
#   echo " NWM products for river forcing conditions"
#   ${USHnos}/nos_ofs_nwm_collectfile.sh ${RUN} ${TIME_START}00 ${TIME_END}00
#fi
if [ ! -s ${EXECnos}/nos_ofs_create_forcing_river -o \
     ! -x ${EXECnos}/nos_ofs_create_forcing_river ]; then
  echo 'River executable file: nos_ofs_create_forcing_river does not exist '
  echo 'Please check the file nos_ofs_create_forcing_river in ' ${EXECnos}
  echo ${RUN} exits from nos_ofs_create_forcing_river.sh

  echo 'River executable file does not exist ' >> ${cormslogfile}
  msg="River executable file does not exist"
  postmsg "${jlogfile}" "${msg}"
  postmsg "${nosjlogfile}" "${msg}"
  msg="Please check the executable file name for river focing in ${EXECnos}"
  postmsg "${jlogfile}" "${msg}"
  postmsg "${nosjlogfile}" "{$msg}"
  msg="${RUN} exits from nos_ofs_create_forcing_river.sh "
  postmsg "${jlogfile}" "${msg}"
  postmsg "${nosjlogfile}" "${msg}"
  err=1;export err;err_chk
  exit
fi   

export pgm="${EXECnos}/nos_ofs_create_forcing_river"
. prep_step

startmsg

${EXECnos}/nos_ofs_create_forcing_river < Fortran_river.ctl > Fortran_river.log
export err=$?

## added to process river data from Canada side
if [ $OFS == "LSOFS" -o $OFS == "lsofs" -o $OFS == "LOOFS" -o $OFS == "loofs" -o $OFS == "SSCOFS" -o $OFS == "sscofs" ]; then
  echo ${RUN} > Fortran_river_Canada.ctl
  echo $OCEAN_MODEL >> Fortran_river_Canada.ctl
  echo ${TIME_START}00 >> Fortran_river_Canada.ctl
  echo ${TIME_END}00 >> Fortran_river_Canada.ctl
  echo $GRIDFILE >> Fortran_river_Canada.ctl
  echo $DCOMINusgs >> Fortran_river_Canada.ctl
  echo $DCOMINports >> Fortran_river_Canada.ctl
  echo $NOSBUFR >> Fortran_river_Canada.ctl
  echo $CANADARVBUFR >> Fortran_river_Canada.ctl
  echo $FIXofs >> Fortran_river_Canada.ctl
  if [ $OFS != "SSCOFS" -a $OFS != "sscofs" ]; then    ########## machuan
    echo $RIVER_CTL_FILE >> Fortran_river_Canada.ctl
  else
    echo $RIVER_CTL_FILE_CANADA >> Fortran_river_Canada.ctl
  fi
#  echo $RIVER_CTL_FILE >> Fortran_river_Canada.ctl
  echo $RIVER_CLIM_FILE >> Fortran_river_Canada.ctl
  echo $RIVER_FORCING_FILE >> Fortran_river_Canada.ctl
  echo $BASE_DATE >> Fortran_river_Canada.ctl
  echo $KBm  >> Fortran_river_Canada.ctl
  echo ${BIO_MODULE} >> Fortran_river_Canada.ctl
  echo $cormslogfile >> Fortran_river_Canada.ctl
  echo $COMOUTroot >> Fortran_river_Canada.ctl
  echo ${PREFIXNOS}  >> Fortran_river_Canada.ctl
  $EXECnos/nos_ofs_create_forcing_river < Fortran_river_Canada.ctl > Fortran_river_Canada.log
fi



#  Update NWM river index file in fix folder
if [ -f version_new.dat ]; then
  while read -r ver; do
    filename=${FIXofs}/${PREFIXNOS}.${ver}.river.index
    if [ ! -f ${filename} ]; then
      echo "New river index is copied to fix folder"
      cp -p ${PREFIXNOS}.${ver}.river.index ${filename}
    fi
  done < "version_new.dat"
fi

if grep "COMPLETED SUCCESSFULLY"  Fortran_river.log /dev/null 2>&1
then
  echo "RIVER FORCING COMPLETED SUCCESSFULLY 100" >> $cormslogfile
  echo "RIVER_FORCING DONE 100 " >> ${cormslogfile}
  echo 'River Forcing generation is successful'
else
  echo "RIVER FORCING COMPLETED SUCCESSFULLY 0" >> $cormslogfile
  echo "RIVER_FORCING DONE 0 " >> ${cormslogfile}
  echo "River Forcing generation failed"
  msg="FATAL ERROR:River Forcing generation failed  "
  postmsg "${jlogfile}" "${msg}"
  postmsg "${nosjlogfile}" "${msg}"
  err=1;export err;err_chk
fi

if [ ${err} -ne 0 ]; then
  echo "${pgm} did not complete normally, FATAL ERROR!"
  msg="${pgm} did not complete normally, FATAL ERROR!"
  postmsg "${jlogfile}" "${msg}"
  postmsg "${nosjlogfile}" "${msg}"
  err_chk
else
  echo "${pgm} completed normally"
  msg="${pgm} completed normally"
  postmsg "${jlogfile}" "${msg}"
  postmsg "${nosjlogfile}" "${msg}"
fi

if [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]; then
  if [ -d 'RIVER' ]; then
    tar -cvf ${RIVER_FORCING_FILE} RIVER
  fi  
elif [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]; then
  tar -cvf ${RIVER_FORCING_FILE} selfe_flux.th selfe_temp.th selfe_salt.th
fi

if [ -f ${RIVER_FORCING_FILE} ]; then
  cp ${RIVER_FORCING_FILE} ${COMOUT}/${RIVER_FORCING_FILE}
  if [ ${SENDDBN} = YES ]; then
    ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE_NETCDF} ${job} \
    ${COMOUT}/${RIVER_FORCING_FILE}
  fi
else
  echo "No ${RIVER_FORCING_FILE} File Found"
fi

if [ -f Fortran_river.log ]; then
   cp Fortran_river.log ${COMOUT}/Fortran_river.t${cyc}z.log
else
  echo "NO Fortran_river.log Found"
fi
echo 'The script nos_ofs_create_forcing_river.sh ends at UTC' `date`


