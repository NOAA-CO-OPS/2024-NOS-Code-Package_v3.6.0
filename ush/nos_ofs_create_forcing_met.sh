#!/bin/sh
# Script Name:  nos_ofs_create_forcing_met.sh
#
# Purpose:
#   This program is used to read NCEP atmospheric operational products of grib2 files
#   such as NAM,GFS, and RTMA,etc. to generated surface forcing file for ROMS. The most recent available
#   products for the given time are are searched and used to generate meteorological forcing files. The wind vectors are rotated to earth coordinates.
#   The missing variables are filled with a missing value of -99999.0.
#
# Location:   ~/scripts 
# Technical Contact:   	Aijun Zhang         Org:  NOS/CO-OPS
#                       Phone: 240-533-0921  
#                       E-Mail: aijun.zhang@noaa.gov
#
#  Usage: ./nos_ofs_create_forcing_met.sh 
#
# Input Parameters:
#  RUNTYPE: "NOWCAST" or "FORECAST"
#  RUN:             Name of Operational Forecast System, e.g., cbofs, dbofs, tbofs
#  DBASE:           Name of NCEP operational meteological products such NAM, GFS, RTMA..
#  time_start:  start time to grab data, e.g., YYYYMMDDHH (2008101500)
#  time_end:    end time to grab data, e.g., YYYYMMDDHH (2008101600)
#  IGRD_MET: =0, The output is on the native AWIPS Grid 218 (coarse grid)
#        =1, The output is on the ocean model grid (ROMS), remesh routine (from Tom Gross and used by
#            Greg for GLOFS) is used for spatial interpolation. 
#        =2, The output is on the ocean model grid (ROMS), bicubic routine (from ROMS) 
#            is used for spatial interpolation. 
#        =3, The output is on the ocean model grid (ROMS), bilinear routine (from ROMS) 
#            is used for spatial interpolation. 
#        =4, on ocean model grid (rotated to local coordinates) interpolated using nature neighbors routine.
#
# Language:   Bourne Shell Script      
#
# Target Computer: IBM P757 Supter Computer at NCEP
#
# Estimated Execution Time: 1800s 
#
# Suboutines/Functions Called:
#     nos_ofs_create_forcing_met.f   
#
# Input Files:
#     ROMS ocean model grid netCDF file  
#
# Output Files:
##    ----  nowcast  ---- 
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.met.nowcast.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.hflux.nowcast.nc
#     Fortran.${RUN}.nowcast.log
#     ----  forecast ----
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.met.forecast.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.hflux.forecast.nc 
#     Fortran.${RUN}.forecast.log 
#
# Libraries Used: see the makefile
#  
# Error Conditions:
#
# Modification: 07/07/2019 by Aijun Zhang
# TMPDIR4 has to be added in the path due to FV3GFS path change from GFS
# remove "/" after ${COMINrtma} ${COMINnam} ${COMINgfs} etc. 
#    
#         
#
# -------------------------------------------------------------------------------
set -x
RUNTYPE=$1
DBASE=$2
TIME_START=$3
TIME_END=$4
echo "The script nos_ofs_create_forcing_met.sh $RUNTYPE starts at time: " `date `
echo "The script nos_ofs_create_forcing_met.sh $RUNTYPE starts at time: " `date ` >> $jlogfile
met_fore_round=${met_fore_round:-1}

if [ $OCEAN_MODEL == 'ROMS' -o $OCEAN_MODEL == 'roms' ]
then
  export EXFILE='nos_ofs_create_forcing_met'
elif [ $OCEAN_MODEL == 'FVCOM' -o $OCEAN_MODEL == 'SELFE' ]
then
  export EXFILE='nos_ofs_create_forcing_met_fvcom'
fi  
if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
then
#  DBASE=$DBASE_MET_NOW
#  TIME_START=${time_hotstart}
#  TIME_END=$time_nowcastend
  OUTPUTFILE=$MET_NETCDF_1_NOWCAST
  OUTPUTFILE1=$MET_NETCDF_2_NOWCAST
  HH=00
  HHH=000
  CYCLE_ORI=`echo $TIME_END |cut -c9-10 `
elif [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]
then
#  DBASE=$DBASE_MET_FOR
#  TIME_START=${time_nowcastend}
#  TIME_END=$time_forecastend
  OUTPUTFILE=$MET_NETCDF_1_FORECAST
  OUTPUTFILE1=$MET_NETCDF_2_FORECAST
  if (( $nfore==2 )); then
     export OUTPUTFILE=$MET_NETCDF_1_FORECAST$met_fore_round
     export OUTPUTFILE1=$MET_NETCDF_2_FORECAST$met_fore_round
  fi

  HH=60
  HHH=060
  CYCLE_ORI=`echo $TIME_START |cut -c9-10 `
fi
# make sure to get enough data, two additional hours will be acquired.
TIME_START_TMP=` $NDATE -3 $TIME_START `
DBASE_ORI=$DBASE
#rm -f *.$DBASE_MET_FOR
rm -f $DBASE
#cp -p $FIXnos/LAND.* $DATA
INPUTTIME=$TIME_START_TMP
#typeset -Z2 HH HH3 CYCLE
YYYY=`echo $TIME_START_TMP | cut -c1-4 `
MM=`echo $TIME_START_TMP |cut -c5-6 `
DD=`echo $TIME_START_TMP |cut -c7-8 `
CYCLE=`echo $TIME_START_TMP |cut -c9-10 `
LENGTH=` $NHOUR $TIME_END $TIME_START_TMP `
if [ $LENGTH -gt 60 ]; then
  LENGTH=60
fi
if [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]
then
  LENGTH=60
fi
exit_status=1
while [ $exit_status -ne 0 ]
do
  exit_status=0
  NPP=18
  VARNAME[0]='TMP'     #Temperature (K)
  VARNAME[1]='DPT'     #Dew point temperature (K)
  VARNAME[2]='UGRD'    #u-component of wind (m/s)
  VARNAME[3]='VGRD'    #v-component of wind (m/s)
  VARNAME[4]='RH'      #Relative humidity (%)
  if [ $DBASE == "HRRR" ]; then
    VARNAME[5]='MSLMA'   #Pressure reduced to MSL (Pa)
  else
    VARNAME[5]='PRMSL'   #Pressure reduced to MSL (Pa)
  fi
  VARNAME[6]='DLWRF'   #downward long wave radiation flux (W/m2)
  VARNAME[7]='ULWRF'   #upward long wave radiation flux (W/m2)
  VARNAME[8]='DSWRF'   #downward short wave radiation flux (W/m2)
  VARNAME[9]='USWRF'   #upward short wave radiation flux (W/m2)
  VARNAME[10]='LHTFL'  #Latent heat flux W/m2
  VARNAME[11]='SHTFL'  #Sensible heat flux W/m2
  VARNAME[12]='SPFH'   #Specific humidity (kg/kg)
  VARNAME[13]='TCDC'   #Total cloud cover percentage (%)
  VARNAME[14]='PRATE'  #Precipatation rate  kg/m2/s
  VARNAME[15]='APCP'   #Total Precipatation kg/m2
  VARNAME[16]='EVP'    #Evaporation (kg/m2)
  VARNAME[17]='PRES'   #Pressure (Pa)
  VARNAME[18]='WTMP'   #Wate Temperature (K)
  #VARNAME[19]='PEVP' 
  LEV[0]=':2 m above ground:'
  LEV[1]=':2 m above ground:'
  LEV[2]=':10 m above ground:'
  LEV[3]=':10 m above ground:'
  LEV[4]=':2 m above ground:'
  LEV[5]=':mean sea level:'
  LEV[6]=':surface:'
  LEV[7]=':surface:'
  LEV[8]=':surface:'
  LEV[9]=':surface:'
  LEV[10]=':surface:'
  LEV[11]=':surface:'
  LEV[12]=':2 m above ground:'
  LEV[13]=':entire atmosphere'
  LEV[14]=':surface:'
  LEV[15]=':surface:'
  LEV[16]=':surface:'
  LEV[17]=':surface:'
  LEV[18]=':surface:'
  #LEV[19]=':surface:'

  if [ $DBASE == "NDFD" ]; then 
     modelist="NDFD GFS25"
  elif [ $DBASE == "RTMA" ]; then
     modelist="RTMA NAM GFS25"
  elif [ $DBASE == "NAM" ]; then
     modelist="NAM GFS25"
  elif [ $DBASE == "GFS" ]; then
     modelist="GFS"
  elif [ $DBASE == "NAM4" ]; then
     modelist="NAM4 NAM GFS25"
  elif [ $DBASE == "GFS25" ]; then
     modelist="GFS25 GFS"
  elif [ $DBASE == "RAP" ]; then
     modelist="RAP NAM"
  elif [ $DBASE == "HRRR" ]; then
     modelist="HRRR GFS25"
  fi
 
  totalcheck=`echo "$modelist"|wc -w`
  numcheck=0
  for modelcheck in $modelist
  do
    export err=0
    DBASE=$modelcheck
    CURRENTTIME=$TIME_START_TMP
    CURRENTTIME=` $NDATE -12 $TIME_START_TMP `
    if [ $CURRENTTIME -gt $time_nowcastend ]; then
          CURRENTTIME=` $NDATE -12 $time_nowcastend `
    fi	  
    YYYY=`echo $CURRENTTIME | cut -c1-4 `
    MM=`echo $CURRENTTIME |cut -c5-6 `
    DD=`echo $CURRENTTIME |cut -c7-8 `
    TMPDATE=$YYYY$MM$DD
    YYYY=`echo $TIME_END | cut -c1-4 `
    MM=`echo $TIME_END |cut -c5-6 `
    DD=`echo $TIME_END |cut -c7-8 `
    ENDDATE=$YYYY$MM$DD
    TMPDATE0=$TMPDATE
    CURRENTTIME0=$CURRENTTIME
    rm -f tmp.out tmp1.out   
    if [ $DBASE == "RTMA" -o $DBASE == "RAP" -o $DBASE == "HRRR" ]; then
      while [ $TMPDATE -le $ENDDATE ]
      do 
        if [ $DBASE == "RTMA" ]; then
          ls -l ${COMINrtma}/rtma2p5.${TMPDATE}/rtma2p5.t*z.2dvaranl_ndfd.grb2 | awk '{print $NF}' >> tmp.out
        elif [ $DBASE == "RAP" ]; then
          while (( N < 24 ))
          do
            CYCLE=`echo $N | awk '{printf("%02i",$1)}'`
            TMPFILE9=${COMINrap}/rap.${TMPDATE}/rap.t${CYCLE}z.awp130pgrbf00.grib2
            if [ -s $TMPFILE9 ]; then
              echo $TMPFILE9 >> tmp.out
              EXIST_CYCLE=$CYCLE
              EXIST_DATE=$TMPDATE
            fi
            (( N = N + 1 ))
          done
        elif [ $DBASE == "HRRR" ]; then
          N=0
          while (( N < 24 ))
          do
            CYCLE=`echo $N | awk '{printf("%02i",$1)}'`
            TMPFILE9=${COMINhrrr}/hrrr.${TMPDATE}/conus/hrrr.t${CYCLE}z.wrfsfcf01.grib2
            if [ "${OFS,,}" == "ciofs" ]; then
              TMPFILE9=${COMINhrrr}/hrrr.${TMPDATE}/alaska/hrrr.t${CYCLE}z.wrfsfcf01.ak.grib2
            fi
            if [ -s $TMPFILE9 ]; then
              echo $TMPFILE9 >> tmp.out
              if [ $RUNTYPE == "nowcast" ] || [ $RUNTYPE == "NOWCAST" ]; then	 	      
                EXIST_CYCLE=$CYCLE
                EXIST_DATE=$TMPDATE		  
	      elif [ $RUNTYPE == "forecast" ] || [ $RUNTYPE == "FORECAST" ]; then
                TMPFILE10=${COMINhrrr}/hrrr.${TMPDATE}/conus/hrrr.t${CYCLE}z.wrfsfcf48.grib2
                if [ -s $TMPFILE10 ]; then
                  EXIST_CYCLE=$CYCLE
                  EXIST_DATE=$TMPDATE
	        fi 
	      fi  	
            fi
            (( N = N + 1 ))
          done

        fi
        CURRENTTIME=`$NDATE +24 $CURRENTTIME `
        YYYY=`echo $CURRENTTIME | cut -c1-4 `
        MM=`echo $CURRENTTIME |cut -c5-6 `
        DD=`echo $CURRENTTIME |cut -c7-8 `
        TMPDATE=$YYYY$MM$DD
        CURRENTTIME0=$YYYY$MM$DD'00'
      done
      if [ $DBASE == "RAP" ];  then
         N=1
         while (( N < 23 ))
         do
           FF=`echo $N | awk '{printf("%02i",$1)}'`
           TMPFILE9=${COMINrap}/rap.${EXIST_DATE}/rap.t${EXIST_CYCLE}z.awp130bgrbf${FF}.grib2
           if [ -s $TMPFILE9 ]; then
             echo $TMPFILE9 >> tmp.out
           fi
           (( N = N + 1 ))
         done
      fi
      if [ $DBASE == "HRRR" ];  then
        if [ $RUNTYPE == "nowcast" ] || [ $RUNTYPE == "NOWCAST" ]; then      
         N=1
         while (( N < 23 ))
         do
           FF=`echo $N | awk '{printf("%02i",$1)}'`
           TMPFILE9=${COMINhrrr}/hrrr.${EXIST_DATE}/conus/hrrr.t${EXIST_CYCLE}z.wrfsfcf${FF}.grib2
           if [ "${OFS,,}" == "ciofs" ]; then
              TMPFILE9=${COMINhrrr}/hrrr.${EXIST_DATE}/alaska/hrrr.t${EXIST_CYCLE}z.wrfsfcf${FF}.ak.grib2
           fi
           if [ -s $TMPFILE9 ]; then
             echo $TMPFILE9 >> tmp.out
           fi
           (( N = N + 1 ))
         done
        elif [ $RUNTYPE == "forecast" ] || [ $RUNTYPE == "FORECAST" ]; then
         N=1
         while (( N < 49 ))
	 do
	   FF=`echo $N | awk '{printf("%02i",$1)}'`
           TMPFILE9=${COMINhrrr}/hrrr.${EXIST_DATE}/conus/hrrr.t${EXIST_CYCLE}z.wrfsfcf${FF}.grib2
           if [ -s $TMPFILE9 ]; then
             echo $TMPFILE9 >> tmp.out
           fi
           (( N = N + 1 ))
         done
        fi
      fi
    elif [ $DBASE == "NAM" -o $DBASE == "NAM4" -o $DBASE == "GFS" -o $DBASE == "GFS25" ]; then
      while [ $TMPDATE -le $ENDDATE ]
      do
        if [ $DBASE == "NAM" ]; then
          if [ $OFS != "ciofs" ]; then
           ls -l ${COMINnam}/nam.${TMPDATE}/nam.t*.awip12*tm00.grib2 | awk '{print $NF}' >> tmp.out
          elif [ $OFS == "ciofs" ]; then
           ls -l ${COMINnam}/nam.${TMPDATE}/nam.t*.awp242*tm00.grib2 | awk '{print $NF}' >> tmp.out
          fi
        elif [ $DBASE == "NAM4" ]; then
          if [ $OFS != "ciofs" ]; then
           ls -l ${COMINnam}/nam.${TMPDATE}/nam.t*.conusnest.hiresf*tm00.grib2 | awk '{print $NF}' >> tmp.out
          elif [ $OFS == "ciofs" ]; then
           ls -l ${COMINnam}/nam.${TMPDATE}/nam.t*.alaskanest.hiresf*tm00.grib2 | awk '{print $NF}' >> tmp.out
          fi
        elif [ $DBASE == "GFS" ];  then
          ls -l ${COMINgfs}/gfs.${TMPDATE}/*/atmos/gfs.t*.pgrb2.0p50.f??? | awk '{print $NF}' >> tmp.out
        elif [ $DBASE == "GFS25" ]; then
          ls -l ${COMINgfs}/gfs.${TMPDATE}/*/atmos/gfs.t*.pgrb2.0p25.f??? | awk '{print $NF}' >> tmp.out
        fi
        CURRENTTIME=`$NDATE +24 $CURRENTTIME `
        YYYY=`echo $CURRENTTIME | cut -c1-4 `
        MM=`echo $CURRENTTIME |cut -c5-6 `
        DD=`echo $CURRENTTIME |cut -c7-8 `
        TMPDATE=$YYYY$MM$DD
      done
    fi
    if [ $DBASE != "NDFD" ]; then
      if [ -s tmp.out ]; then
        cp -p tmp.out $COMOUT/met_files_existed_${RUNTYPE}_${DBASE}_${cyc}.dat
        rm -f Fortran_file_search.ctl Fortran_file_search.log
        MET_FILE=${DBASE}_FILE_${RUNTYPE}.dat
#        echo $TIME_START > Fortran_file_search.ctl
#        echo $TIME_START_TMP > Fortran_file_search.ctl
	if [ $TIME_START_TMP -gt  $time_nowcastend ]; then
	  CURRENTTIME=` $NDATE -24 $time_nowcastend `
        else
	  CURRENTTIME=$TIME_START_TMP    	
	fi
        echo $CURRENTTIME >> Fortran_file_search.ctl
	echo $time_nowcastend >> Fortran_file_search.ctl
        echo $TIME_END >> Fortran_file_search.ctl
        echo tmp.out >> Fortran_file_search.ctl
        echo $MET_FILE >> Fortran_file_search.ctl
        if [ ! -s $EXECnos/nos_ofs_met_file_search -a  ! -x  $EXECnos/nos_ofs_met_file_search ]; then
         echo "$EXECnos/nos_ofs_met_file_search does not exist" 
         msg="$EXECnos/nos_ofs_met_file_search does not exist  "
         postmsg "$jlogfile" "$msg"
         err=1;export err;err_chk
         exit
        fi   
        export pgm=nos_ofs_met_file_search
        . prep_step
        startmsg
        $EXECnos/nos_ofs_met_file_search < Fortran_file_search.ctl > Fortran_file_search.log
        echo 'nos_ofs_met_file_search completed at time: ' `date`
        if grep "COMPLETED SUCCESSFULLY" Fortran_file_search.log /dev/null 2>&1
        then
          echo "MET FILE SEARCH ${RUNTYPE} ${DBASE} COMPLETED SUCCESSFULLY 100" >> $cormslogfile
          echo "$pgm completed normally"
          msg="$pgm completed normally"
          postmsg "$jlogfile" "$msg"
          export err=0
        else
          echo "MET FILE SEARCH ${RUNTYPE} ${DBASE} COMPLETED SUCCESSFULLY 0" >> $cormslogfile
          echo "WARNING:$pgm did not complete normally with $DBASE"
          msg="WARNING:$pgm did not complete normally with $DBASE"
          postmsg "$jlogfile" "$msg"
          export err=1
        fi
      else # tmp.out does not exist
        export err=1
      fi
    fi
##  START search NDFD
    if [ $DBASE == "NDFD" ]; then
# make sure to get enough data, two additional hours will be acquired.

      TIME_START_TMP=` $NDATE -3 $TIME_START `
      if [ $TIME_START_TMP -gt  $time_nowcastend ]; then
          CURRENTTIME=` $NDATE -24 $time_nowcastend `
      else
          CURRENTTIME=$TIME_START_TMP
      fi

      TIME_STARTm24=`$NDATE -24 $CURRENTTIME `
      DBASE_ORI=$DBASE
      INPUTTIME=$CURRENTTIME
      YYYY=`echo $CURRENTTIME | cut -c1-4 `
      MM=`echo $CURRENTTIME |cut -c5-6 `
      DD=`echo $CURRENTTIME |cut -c7-8 `
      CYCLE=`echo $CURRENTTIME |cut -c9-10 `
      NPP=7
      VARNAME[0]='WIND'
      VARNAME[1]='WDIR'
      VARNAME[2]='DPT'
      VARNAME[3]='TMP'
      VARNAME[4]='TCDC'
      VARNAME[5]='RH'
      VARNAME[6]='APCP'
      LEV[0]=':10 m above ground:'
      LEV[1]=':10 m above ground:'
      LEV[2]=':2 m above ground:'
      LEV[3]=':2 m above ground:'
      LEV[4]=':surface:'
      LEV[5]=':2 m above ground:'
      LEV[6]=':surface:'
      NREC_TMP=0
      NREC_DIR=0
      NREC_SPD=0
      NREC_DP=0
      NREC_RH=0
      NREC_CLD=0
      TIME_TMP=$CURRENTTIME
      YYYY=`echo $TIME_TMP | cut -c1-4 `
      MM=`echo $TIME_TMP |cut -c5-6 `
      DD=`echo $TIME_TMP |cut -c7-8 `
      CYCLE=`echo $TIME_TMP |cut -c9-10 `
      TMPGRB2=${RUN}_${DBASE}.grib2
      NCEPPRODDIR=${DCOMINndfd}'/'$YYYY$MM$DD/wgrbbul/ndfd_conus
      GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_tmp.grib2"
      if [ -s $GRB2FILE ]; then
        NREC_TMP=`$WGRIB2 $GRB2FILE -s | grep $YYYY$MM$DD$CYCLE | grep "hour" | wc -l`
      fi
      GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_wspd.grib2"
      if [ -s $GRB2FILE ]; then
        NREC_SPD=`$WGRIB2 $GRB2FILE -s | grep $YYYY$MM$DD$CYCLE | grep "hour" | wc -l`
      fi
      GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_wdir.grib2"
      if [ -s $GRB2FILE ]; then
        NREC_DIR=`$WGRIB2 $GRB2FILE -s | grep $YYYY$MM$DD$CYCLE | grep "hour" | wc -l`
      fi
      GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_dp.grib2"
      if [ -s $GRB2FILE ]; then
        NREC_DP=`$WGRIB2 $GRB2FILE -s | grep $YYYY$MM$DD$CYCLE | grep "hour" | wc -l`
      fi
      GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_rh.grib2"
      if [ -s $GRB2FILE ]; then
        NREC_RH=`$WGRIB2 $GRB2FILE -s | grep $YYYY$MM$DD$CYCLE | grep "hour" | wc -l`
      fi
      GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_sky.grib2"
      if [ -s $GRB2FILE ]; then
        NREC_CLD=`$WGRIB2 $GRB2FILE -s | grep $YYYY$MM$DD$CYCLE | grep "hour" | wc -l`
      fi
      echo nrec=$NREC_TMP $YYYY $MM $DD $CYCLE
      while [ $NREC_SPD -lt $LENGTH -o $NREC_DIR -lt $LENGTH -o $NREC_TMP -lt $LENGTH -o $NREC_DP -lt $LENGTH -o $NREC_RH -lt $LENGTH -o $NREC_CLD -lt $LENGTH ]
      do
        TIME_TMP=`$NDATE -1 $TIME_TMP `
        if [ $TIME_TMP -lt $TIME_STARTm24 ]; then
          echo "WARNING:No appropriate NDFD products are available. Try backup productus" 
          msg="WARNING:No appropriate NDFD products are available. Try backup products"
          postmsg "$jlogfile" "$msg"
          postmsg "$nosjlogfile" "$msg"
          export err=1
          break
        else
          YYYY=`echo $TIME_TMP | cut -c1-4 `
          MM=`echo $TIME_TMP |cut -c5-6 `
          DD=`echo $TIME_TMP |cut -c7-8 `
          CYCLE=`echo $TIME_TMP |cut -c9-10 `
          NCEPPRODDIR=${DCOMINndfd}'/'$YYYY$MM$DD/wgrbbul/ndfd_conus
          GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_tmp.grib2"
          if [ -s $GRB2FILE ]; then
            NREC_TMP=`$WGRIB2 $GRB2FILE -s | grep $YYYY$MM$DD$CYCLE | grep "hour" | wc -l`
          fi
          GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_wspd.grib2"
          if [ -s $GRB2FILE ]; then
            NREC_SPD=`$WGRIB2 $GRB2FILE -s | grep $YYYY$MM$DD$CYCLE | grep "hour" | wc -l`
          fi
          GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_wdir.grib2"
          if [ -s $GRB2FILE ]; then
            NREC_DIR=`$WGRIB2 $GRB2FILE -s | grep $YYYY$MM$DD$CYCLE | grep "hour" | wc -l`
          fi
          GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_dp.grib2"
          if [ -s $GRB2FILE ]; then
            NREC_DP=`$WGRIB2 $GRB2FILE -s | grep $YYYY$MM$DD$CYCLE | grep "hour" | wc -l`
          fi
          GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_rh.grib2"
          if [ -s $GRB2FILE ]; then
            NREC_RH=`$WGRIB2 $GRB2FILE -s | grep $YYYY$MM$DD$CYCLE | grep "hour" | wc -l`
          fi
          GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_sky.grib2"
          if [ -s $GRB2FILE ]; then
            NREC_CLD=`$WGRIB2 $GRB2FILE -s | grep $YYYY$MM$DD$CYCLE | grep "hour" | wc -l`
          fi
        fi
      done
      if [ $NREC_SPD -ge $LENGTH -a $NREC_DIR -ge $LENGTH -a $NREC_TMP -ge $LENGTH -a $NREC_DP -ge $LENGTH -a $NREC_RH -ge $LENGTH -a $NREC_CLD -ge $LENGTH ]; then
        echo "Using NDFD Products in ${NCEPPRODDIR}"  >> $cormslogfile
        echo "CYCLE of $CYCLE " on "${MM}/${DD}/${YYYY}" >> $cormslogfile
        echo "number of records= " $NREC_TMP $NREC_SPD $NREC_DIR $NREC_RH $NREC_DP $NREC_CLD  >> $cormslogfile
        echo ${NCEPPRODDIR}/"ndfd_conus_wspd.grib2" > met_files_used_${RUNTYPE}_${CYCLE_ORI}_${DBASE}.dat
        echo ${NCEPPRODDIR}/"ndfd_conus_wdir.grib2" >> met_files_used_${RUNTYPE}_${CYCLE_ORI}_${DBASE}.dat
        echo ${NCEPPRODDIR}/"ndfd_conus_tmp.grib2" >> met_files_used_${RUNTYPE}_${CYCLE_ORI}_${DBASE}.dat
        echo ${NCEPPRODDIR}/"ndfd_conus_dp.grib2" >> met_files_used_${RUNTYPE}_${CYCLE_ORI}_${DBASE}.dat
        echo ${NCEPPRODDIR}/"ndfd_conus_sky.grib2" >> met_files_used_${RUNTYPE}_${CYCLE_ORI}_${DBASE}.dat
        echo ${NCEPPRODDIR}/"ndfd_conus_rh.grib2" >> met_files_used_${RUNTYPE}_${CYCLE_ORI}_${DBASE}.dat
        echo ${NCEPPRODDIR}/"ndfd_conus_qpf.grib2" >> met_files_used_${RUNTYPE}_${CYCLE_ORI}_${DBASE}.dat
        cp -p met_files_used_${RUNTYPE}_${CYCLE_ORI}_${DBASE}.dat $COMOUT
        count=0
        while (( count < $NPP ))
        do
          rm -f tmp.txt $TMPGRB2 tmp_ndfd.txt ${VARNAME[count]}.$DBASE
          if [ $count -eq 0 ]; then
            GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_wspd.grib2"    # WIND
          elif [ $count -eq 1 ]; then
            GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_wdir.grib2"    # WDIR
          elif [ $count -eq 2 ]; then
            GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_dp.grib2"      # DPT
          elif [ $count -eq 3 ]; then
            GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_tmp.grib2"     # TMP
          elif [ $count -eq 4 ]; then
            GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_sky.grib2"     #TCDC
          elif [ $count -eq 5 ]; then
            GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_rh.grib2"      # RH
          elif [ $count -eq 6 ]; then
            GRB2FILE=${NCEPPRODDIR}/"ndfd_conus_qpf.grib2"     # APCP
          fi 
          $WGRIB2 $GRB2FILE -small_grib ${MINLON}:${MAXLON} ${MINLAT}:${MAXLAT} $TMPGRB2
          FHH=1
          while (( FHH < 180 ))
          do
            rm -f tmp_ndfd.txt 
            (( FN = 10#$FHH ))
            FPSTG="$FN hour fcst"
            $WGRIB2 $TMPGRB2 -s | grep "$YYYY$MM${DD}${CYCLE}:${VARNAME[count]}${LEV[count]}${FPSTG}" | sed '2,$d' | $WGRIB2 -i $TMPGRB2 -rpn "sto_1:-9999.9:rcl_1:merge:" -spread tmp_ndfd.txt
 #         $WGRIB2 $GRB2FILE -s | grep "$YYYY$MM${DD}${CYCLE}:${VARNAME[count]}${LEV[count]}${FPSTG}" | sed '2,$d' | $WGRIB2 -i $GRB2FILE -rpn "sto_1:-9999.9:rcl_1:merge:" -spread tmp_ndfd.txt

            if [ -s tmp_ndfd.txt ]; then
              cat tmp_ndfd.txt >> tmp.txt
            fi
            (( FHH = FHH + 1 ))
          done
          if [ -s tmp.txt ]; then
            cat tmp.txt >> ${VARNAME[count]}.$DBASE
          fi
          (( count = count + 1 ))
        done    
      fi
    fi  
##  END of search NDFD
    if [ $err -ne 0 ]; then
        echo "WARNING: $DBASE files not available. Checking backup in the order of $modelist"
        msg="WARNING: $DBASE files not available. Checking backup in the order of $modelist"
        postmsg "jlogfile" "$msg"
        postmsg "$nosjlogfile" "$msg"
# send email message to SPA and CO-OPS Modeling Team
#       export maillist=${maillist:-'nco.spa@noaa.gov,${PREFIXNOS}.co-ops.modelingteam@noaa.gov'}
        export subject="WARNING!!! COULD NOT FIND PRIMARY $DBASE MET FILES for $OFS"
        echo "*****************************************************************************************" > mailmsg
        echo "*** WARNING!!! COULD NOT FIND PRIMARY $DBASE MET FILES for $PDY t${cyc}z $job in $OFS ***" >> mailmsg
        echo "*****************************************************************************************" >> mailmsg
        echo >> mailmsg
        if [ $DBASE == NDFD ]; then
	  echo "Please check availability of expected $DBASE product files in ${NCEPPRODDIR}:" >> mailmsg
          echo "   ${NCEPPRODDIR}/ndfd_conus_wspd.grib2 "  >> mailmsg
          echo "   ${NCEPPRODDIR}/ndfd_conus_wdir.gribn2"  >> mailmsg
          echo "   ${NCEPPRODDIR}/ndfd_conus_dp.grib2   "  >> mailmsg
          echo "   ${NCEPPRODDIR}/ndfd_conus_tmp.grib2  "  >> mailmsg
          echo "   ${NCEPPRODDIR}/ndfd_conus_sky.grib2  "  >> mailmsg
          echo "   ${NCEPPRODDIR}/ndfd_conus_rh.grib2   "  >> mailmsg
          echo "   ${NCEPPRODDIR}/ndfd_conus_qpf.grib2  "  >> mailmsg
        else
	  echo "Please check availability of expected $DBASE product files ${DBASE,,}.$TMPDATE "
        fi	  
 	echo >> mailmsg
        echo "Automatically switched to backup  met file:"  >> mailmsg
        echo >> mailmsg
        cat mailmsg > $COMOUT/${RUN}.t${cyc}z.met.emailbody
        cat $COMOUT/${RUN}.t${cyc}z.met.emailbody | mail.py -s "$subject" $maillist -v
    else
        echo "Use $DBASE for meteorological forcing"
        msg="Use $DBASE for meteorological forcing"
        postmsg "$jlogfile" "$msg"
        break
    fi
    numcheck=`expr $numcheck + 1`
  done      # end of atmos prod check

  echo $numcheck $totalcheck 
  export DBASE=$modelcheck     ## Assign the existing prod checked to the DBASE
  if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]; then
    echo $DBASE > MET_DBASE.NOWCAST
  elif [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]; then
    echo $DBASE > MET_DBASE.FORECAST
  fi
  echo "The current DBASE is $DBASE "
  if [ $DBASE == "GFS" -o $DBASE == "GFS25" ]; then
    if [ $err -ne 0 ]; then
        echo "FATAL ERROR! $DBASE files not available. no backup for $DBASE "
        msg="FATAL ERROR! $DBASE files not available. no backup for $DBASE"
        postmsg "jlogfile" "$msg"
        postmsg "$nosjlogfile" "$msg"
        err_chk
    fi
  fi
  if [ $DBASE == "RTMA" ]; then
    NPP=7
    VARNAME[0]='PRES'
    VARNAME[1]='TMP'
    VARNAME[2]='DPT'
    VARNAME[3]='UGRD'
    VARNAME[4]='VGRD'
    VARNAME[5]='SPFH'
    VARNAME[6]='TCDC'
    LEV[0]=':surface:'
    LEV[1]=':2 m above ground:'
    LEV[2]=':2 m above ground:'
    LEV[3]=':10 m above ground:'
    LEV[4]=':10 m above ground:'
    LEV[5]=':2 m above ground:'
    LEV[6]=':entire atmosphere:'
  elif [ $DBASE == "NDFD" ]; then
    NPP=7
    VARNAME[0]='WIND'
    VARNAME[1]='WDIR'
    VARNAME[2]='DPT'
    VARNAME[3]='TMP'
    VARNAME[4]='TCDC'
    VARNAME[5]='RH'
    VARNAME[6]='APCP'
    LEV[0]=':10 m above ground:'
    LEV[1]=':10 m above ground:'
    LEV[2]=':2 m above ground:'
    LEV[3]=':2 m above ground:'
    LEV[4]=':surface:'
    LEV[5]=':2 m above ground:'
    LEV[6]=':surface:'
  elif [ $DBASE == "RAP" ]; then
    NPP=7
    VARNAME[0]='TMP'
    VARNAME[1]='DPT'
    VARNAME[2]='UGRD'
    VARNAME[3]='VGRD'
    VARNAME[4]='PRMSL'
    VARNAME[5]='SPFH'
    VARNAME[6]='TCDC'
#    VARNAME[7]='PRATE'
#    VARNAME[8]='APCP'
#    VARNAME[9]='DSWRF'
    LEV[0]=':2 m above ground:'
    LEV[1]=':2 m above ground:'
    LEV[2]=':10 m above ground:'
    LEV[3]=':10 m above ground:'
    LEV[4]=':mean sea level:'
    LEV[5]=':2 m above ground:'
    LEV[6]=':entire atmosphere:'
#    LEV[7]=':surface:'
#    LEV[8]=':surface:'
#    LEV[9]=':surface:'

#  elif [ $DBASE == "HRRR" ]; then
#    NPP=13
#    VARNAME[0]='TMP'
#    VARNAME[1]='DPT'
#    VARNAME[2]='UGRD'
#    VARNAME[3]='VGRD'
#    VARNAME[4]='PRMSL'
#    VARNAME[4]='PRES'
#    VARNAME[5]='SPFH'
#    VARNAME[6]='TCDC'
#    VARNAME[7]='PRATE'
#    VARNAME[8]='APCP'
#    VARNAME[9]='DSWRF'
#    VARNAME[10]='USWRF'
#    VARNAME[11]='ULWRF'
#    VARNAME[12]='LHTFL'
#    VARNAME[13]='RH'
#    LEV[0]=':2 m above ground:'
#    LEV[1]=':2 m above ground:'
#    LEV[2]=':10 m above ground:'
#    LEV[3]=':10 m above ground:'
#    LEV[4]=':surface:'
#    LEV[4]=':mean sea level:'
#    LEV[5]=':2 m above ground:'
#    LEV[6]=':entire atmosphere:'
#    LEV[7]=':surface:'
#    LEV[8]=':surface:'
#    LEV[9]=':surface:'
#    LEV[10]=':surface:'
#    LEV[11]=':surface:'
#    LEV[12]=':surface:'
#    LEV[13]=':2 m above ground:'
  else
    NPP=18
    VARNAME[0]='TMP'     #Temperature (K)
    VARNAME[1]='DPT'     #Dew point temperature (K)
    VARNAME[2]='UGRD'    #u-component of wind (m/s)
    VARNAME[3]='VGRD'    #v-component of wind (m/s)
    VARNAME[4]='RH'      #Relative humidity (%)
    VARNAME[5]='PRMSL'   #Pressure reduced to MSL (Pa)
    if [ $DBASE == "HRRR" ]; then
      VARNAME[5]='MSLMA'   #Pressure reduced to MSL (Pa)
    else
      VARNAME[5]='PRMSL'   #Pressure reduced to MSL (Pa)
    fi
    VARNAME[6]='DLWRF'   #downward long wave radiation flux (W/m2)
    VARNAME[7]='ULWRF'   #upward long wave radiation flux (W/m2)
    VARNAME[8]='DSWRF'   #downward short wave radiation flux (W/m2)
    VARNAME[9]='USWRF'   #upward short wave radiation flux (W/m2)
    VARNAME[10]='LHTFL'  #Latent heat flux W/m2
    VARNAME[11]='SHTFL'  #Sensible heat flux W/m2
    VARNAME[12]='SPFH'   #Specific humidity (kg/kg)
    VARNAME[13]='TCDC'   #Total cloud cover percentage (%)
    VARNAME[14]='PRATE'  #Precipatation rate  kg/m2/s
    VARNAME[15]='APCP'   #Total Precipatation kg/m2
    VARNAME[16]='EVP'    #Evaporation (kg/m2)
    VARNAME[17]='PRES'   #Pressure (Pa)
    VARNAME[18]='WTMP'   #Water Temperature (K)
    LEV[0]=':2 m above ground:'
    LEV[1]=':2 m above ground:'
    LEV[2]=':10 m above ground:'
    LEV[3]=':10 m above ground:'
    LEV[4]=':2 m above ground:'
    LEV[5]=':mean sea level:'
    LEV[6]=':surface:'
    LEV[7]=':surface:'
    LEV[8]=':surface:'
    LEV[9]=':surface:'
    LEV[10]=':surface:'
    LEV[11]=':surface:'
    LEV[12]=':2 m above ground:'
    LEV[13]=':entire atmosphere'
    LEV[14]=':surface:'
    LEV[15]=':surface:'
    LEV[16]=':surface:'
    LEV[17]=':surface:'
    LEV[18]=':surface:'
#    LEV[19]=':surface:'
  fi
  echo 'Time of the most recent available products is ' $DBASE 'at cycle' $YYYY $MM $DD $CYCLE
  echo 'Time of the most recent available products is ' $DBASE 'at cycle' $YYYY $MM $DD $CYCLE >> $jlogfile

  if [ $DBASE != "NDFD" ]; then
    rm -f *.$DBASE
    if [ -s $MET_FILE ]; then
      cp -p $MET_FILE $COMOUT/met_files_used_${RUNTYPE}_${CYCLE_ORI}_${DBASE}.dat
      if [ -s cmdfile ]; then rm cmdfile; fi
      echo "#!/bin/bash" > cmdfile
      exec 5<&0 < $MET_FILE
      while read GRB2FILE
      do
        read YYYY MM DD CYC HH
        echo "$USHnos/nos_ofs_create_forcing_met_mpmd.sh $GRB2FILE $HH $NPP" >>cmdfile
      done 3<&-

      # Store the VARNAME and LEV arrays to a file which will be sourced by the MPMD processes
      declare -p VARNAME LEV > var_lev_arrays
      chmod 755 $DATA/cmdfile
#      mpirun cfp cmdfile
      mpiexec -n 8 -ppn 8 cfp $DATA/cmdfile
      export err=$?; err_chk

      LAST_TMPDIR=""
      exec 5<&0 < $MET_FILE
      while read GRB2FILE
      do
        read YYYY MM DD CYC HH
        TMPDIR1=$(dirname ${GRB2FILE//./_})
        TMPDIR2=$(basename $TMPDIR1)
        TMPDIR3=$(dirname $TMPDIR1)
        TMPDIR4=$(basename $TMPDIR3)

        TMPDIR=${TMPDIR4}/${TMPDIR2}/$(basename ${GRB2FILE//./_})
#        TMPDIR=${TMPDIR2}/$(basename ${GRB2FILE//./_})
        count=0
        while (( count < $NPP )); do
          if [ -s $TMPDIR/${VARNAME[count]}.$DBASE ]; then
            cat $TMPDIR/${VARNAME[count]}.$DBASE >> ${VARNAME[count]}.$DBASE
          fi
          (( count++ ))
        done
        LAST_TMPDIR=$TMPDIR
      done 3<&-

      TMPGRB2=${RUN}_${DBASE}.grib2
      ln -sfn $LAST_TMPDIR/$TMPGRB2 $TMPGRB2

##  Begin reading land-sea mask
      LANDMASK=${LANDMASK:-0}
      if [ $LANDMASK -eq 1 ]; then
        $WGRIB2 $TMPGRB2 -s | grep "LAND:surface" | $WGRIB2 -i $TMPGRB2 -rpn "sto_1:-9999.9:rcl_1:merge:" -spread tmp.txt
        if [ -s tmp.txt ];    then
           mv tmp.txt $DBASE.LANDMASK
        fi
      fi
##  END reading land-sea mask
    else  # $MET_FILE does not exist
      echo "WARNING: no Meteorological product files are found in time period from $TIME_START_TMP to $TIME_END"
      msg="WARNING: no Meteorological product files are found in time period from $TIME_START_TMP to $TIME_END   "
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err=1;export err;err_chk
    fi  #$MET_FILE checking 
  fi #Non-NDFD cases
# special for LMHOFS, creating PRATE.$DBASE and EVP.$DBASE
  if [  "${OFS,,}" == "lmhofs" -o  "${OFS,,}" == "loofs" -o  "${OFS,,}" == "lsofs" ]; then
    $USHnos/nos_ofs_residual_water_calculation.sh $RUNTYPE
  fi 

  count=0
  while (( count < $NPP ))
  do
    if [ ! -s ${VARNAME[count]}.$DBASE ]
    then
      echo ${VARNAME[count]}.$DBASE
      rm -f ${VARNAME[count]}.$DBASE
    fi   
    (( count = count + 1 ))
  done
  if [ -s PRES.RTMA ];  then
    mv PRES.RTMA PRMSL.RTMA
  fi
  if [ -s MSLMA.RUC ]; then
    mv MSLMA.RUC PRMSL.RUC
  fi
  if [ -s MSLMA.HRRR ];  then
    mv MSLMA.HRRR PRMSL.HRRR
  fi
  echo "$EXFILE starts at time:" `date`
  rm -f Fortran_met.ctl ${DBASE}_Fortran.log
  echo ${RUN} > Fortran_met.ctl
  echo $DBASE >> Fortran_met.ctl
  echo $OCEAN_MODEL >> Fortran_met.ctl
  echo $TIME_START_TMP >> Fortran_met.ctl
  echo $TIME_END >> Fortran_met.ctl
  echo $IGRD_MET >> Fortran_met.ctl
  echo $TMPGRB2 >> Fortran_met.ctl
  echo $GRIDFILE >> Fortran_met.ctl
  echo $OUTPUTFILE >> Fortran_met.ctl
  echo $BASE_DATE >> Fortran_met.ctl 
  echo $MINLON  >> Fortran_met.ctl
  echo $MINLAT >> Fortran_met.ctl 
  echo $MAXLON >> Fortran_met.ctl 
  echo $MAXLAT >> Fortran_met.ctl 
  echo $SCALE_HFLUX >> Fortran_met.ctl 
  if [ ! -s $EXECnos/$EXFILE -o  ! -x  $EXECnos/$EXFILE ]; then
    echo "$EXECnos/$EXFILE does not exist"
    msg="$EXECnos/$EXFILE does not exist  "
    postmsg "$jlogfile" "$msg"
    err=1;export err;err_chk
    exit
  fi
  export pgm=$EXFILE
  . prep_step
  startmsg
  $EXECnos/$EXFILE < Fortran_met.ctl > ${DBASE}_Fortran.log
  echo "$EXFILE completed at time:" `date`
  export err=$?
  if [ $err -ne 0 ]; then
    echo "$pgm did not complete normally, FATAL ERROR!"
    msg="$pgm did not complete normally, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    exit_status=98
  else
    echo "$pgm completed normally"
    msg="$pgm completed normally"
    postmsg "$jlogfile" "$msg"
    exit_status=0
  fi
## output corms information
  if [ -s ${DBASE}_Fortran.log ]; then
    cp -p ${DBASE}_Fortran.log $COMOUT/$DBASE.${RUNTYPE}_Fortran.t${cyc}z.log
    if grep "COMPLETED SUCCESSFULLY" ${DBASE}_Fortran.log /dev/null 2>&1
    then
       exit_status=0
    else 
       exit_status=99
    fi
  fi
  if [ $exit_status -eq 0 ]; then
    echo "MET FORCING ${RUNTYPE} ${DBASE} COMPLETED SUCCESSFULLY 100" >> $cormslogfile
    if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]; then
      echo MET_NOWCAST DONE 100  >> $cormslogfile
    else
      echo MET_FORECAST DONE 100  >> $cormslogfile
    fi
    if [ $OCEAN_MODEL == 'SELFE' -o $OCEAN_MODEL == 'selfe' ]; then
      if [ -s sflux_air.nc ];   then 
       cp -p sflux_air.nc sflux_air_1.001.nc
       cp -p sflux_air.nc ${PREFIXNOS}.t${cyc}z.$PDY1.air.$RUNTYPE.nc
      fi
      if [ -s sflux_rad.nc ];   then
       cp -p sflux_rad.nc sflux_rad_1.001.nc 
       cp -p sflux_rad.nc ${PREFIXNOS}.t${cyc}z.$PDY1.flux.$RUNTYPE.nc
      fi
      if [ -s sflux_prc.nc ];   then
       cp -p sflux_prc.nc sflux_prc_1.001.nc
       cp -p sflux_prc.nc ${PREFIXNOS}.t${cyc}z.$PDY1.precip.$RUNTYPE.nc
      fi
    elif [ $OCEAN_MODEL == 'FVCOM' -o  $OCEAN_MODEL == 'fvcom' ]; then
      if [ $DBASE != "RTMA" -o $DBASE != "RAP" -o $DBASE != "NDFD"  -o $DBASE != "HRRR" ]; then
        if [ -s $OUTPUTFILE ]; then
          cp -p $OUTPUTFILE  $OUTPUTFILE1
        else
          echo 'surface forcing file ' $OUTPUTFILE 'is not generated'
        fi
      fi
    fi
  else
    exit_status=99
    if [ ! -z "${OFS##GFS*}" ]; then
#    if [ $DBASE != "GFS" ]; then
      echo "WARNING: meteorological forcing was not created from $DBASE"
      echo "There might be errors while executing $EXFILE"
      echo " trying final backup of GFS" 
      msg="WARNING: meteorological forcing was not created from $DBASE. Try final backup using GFS"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
# send email message to SPA and CO-OPS Modeling Team
#      export maillist=${maillist:-'nco.spa@noaa.gov,nos.co-ops.modelingteam@noaa.gov'}
      export subject="WARNING COULD NOT CREATE MET FORCING FROM PRIMARY SOURCE for $PDY t${cyc}z $job"
      echo "*************************************************************" > mailmsg
      echo "*** WARNING !! COULD NOT CREATE MET FORCING FILE FROM PRIMARY $DBASE  *** " >> mailmsg
      echo "There might be errors while executing $EXFILE "  >> mailmsg
      echo "Fore more information, please check ${DATA}/${DBASE}_Fortran.log  "  >> mailmsg
      echo "Please check availability of expected $DBASE product files in ${NCEPPRODDIR}:" >> mailmsg
      echo >> mailmsg
       echo "   ${NCEPPRODDIR}/ndfd_conus_wspd.grib2 "  >> mailmsg
       echo "   ${NCEPPRODDIR}/ndfd_conus_wdir.gribn2"  >> mailmsg
       echo "   ${NCEPPRODDIR}/ndfd_conus_dp.grib2   "  >> mailmsg
       echo "   ${NCEPPRODDIR}/ndfd_conus_tmp.grib2  "  >> mailmsg
       echo "   ${NCEPPRODDIR}/ndfd_conus_sky.grib2  "  >> mailmsg
       echo "   ${NCEPPRODDIR}/ndfd_conus_rh.grib2   "  >> mailmsg
       echo "   ${NCEPPRODDIR}/ndfd_conus_qpf.grib2  "  >> mailmsg
       echo >> mailmsg
      echo " primary met source is  $DBASE " >> mailmsg
      DBASE=GFS25     # GFS25 is used the final backup for all if other DBASE fails
      echo " Automatically switch to back up met file of GFS25"  >> mailmsg
      echo " Will check availability of ${COMINgfs}/gfs.${TMPDATE} "  >> mailmsg
      echo " The following GFS products are needed by $OFS "   >> mailmsg
      echo " ${COMINgfs}/gfs.${TMPDATE}/*/atmos/gfs.t*.pgrb2.0p25.f??? "   >> mailmsg
      echo >> mailmsg
      cat mailmsg > $COMOUT/${RUN}.t${cyc}z.met.emailbody
      cat $COMOUT/${RUN}.t${cyc}z.met.emailbody | mail.py -s "$subject" $maillist -v
    else
      echo "MET FORCING ${RUNTYPE} ${DBASE} COMPLETED SUCCESSFULLY 0" >> $cormslogfile
      if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]; then
        echo MET_NOWCAST DONE 0  >> $cormslogfile
      else
        echo MET_FORECAST DONE 0  >> $cormslogfile
      fi
      msg="FATAL ERROR !! in generating meteorological forcing"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      echo $msg
      echo "$OFS is abort in nos_ofs_create_forcing_met.sh"   
      err=1;export err;err_chk
    fi
  fi
done     # try backup plan if met forcing is not created

## Radiation heat flux from NAM is used if DBASE=RTMA since RTMA does not have radiation heat flux
if [ $OFS != "leofs" -a $OFS != "lmofs" -a $OFS != "loofs" -a $OFS != "lsofs" -a $OFS != "lhofs" -a $OFS != "lmhofs"  -a $OFS != "sscofs" ]; then
if [ $DBASE == "RTMA" -o $DBASE == "RAP" -o $DBASE == "NDFD"  -o $DBASE == "HRRR" ]; then
# DBASE='NAM'   AJ 06/14/2011 
  DBASE=$DBASE_MET_FOR
##  rm -f *.$DBASE
  NPP=17
  VARNAME[0]='TMP'     #Temperature (K)
  VARNAME[1]='DPT'     #Dew point temperature (K)
  VARNAME[2]='UGRD'    #u-component of wind (m/s)
  VARNAME[3]='VGRD'    #v-component of wind (m/s)
  VARNAME[4]='RH'      #Relative humidity (%)
  VARNAME[5]='PRMSL'   #Pressure reduced to MSL (Pa)
  VARNAME[6]='DLWRF'   #downward long wave radiation flux (W/m2)
  VARNAME[7]='ULWRF'   #upward long wave radiation flux (W/m2)
  VARNAME[8]='DSWRF'   #downward short wave radiation flux (W/m2)
  VARNAME[9]='USWRF'   #upward short wave radiation flux (W/m2)
  VARNAME[10]='LHTFL'  #Latent heat flux W/m2
  VARNAME[11]='SHTFL'  #Sensible heat flux W/m2
  VARNAME[12]='SPFH'   #Specific humidity (kg/kg)
  VARNAME[13]='TCDC'   #Total cloud cover percentage (%)
  VARNAME[14]='PRATE'  #Precipatation rate  kg/m2/s
  VARNAME[15]='APCP'   #Total Precipatation kg/m2
  VARNAME[16]='EVP'    #Evaporation (kg/m2)
  VARNAME[17]='PRES'
  VARNAME[18]='WTMP'
  #VARNAME[19]='PEVP'
  LEV[0]=':2 m above ground:'
  LEV[1]=':2 m above ground:'
  LEV[2]=':10 m above ground:'
  LEV[3]=':10 m above ground:'
  LEV[4]=':2 m above ground:'
  LEV[5]=':mean sea level:'
  LEV[6]=':surface:'
  LEV[7]=':surface:'
  LEV[8]=':surface:'
  LEV[9]=':surface:'
  LEV[10]=':surface:'
  LEV[11]=':surface:'
  LEV[12]=':2 m above ground:'
  LEV[13]=':entire atmosphere:'
  LEV[14]=':surface:'
  LEV[15]=':surface:'
  LEV[16]=':surface:'
  LEV[17]=':surface:'
  LEV[18]=':surface:'
  #LEV[19]=':surface:'


  HH=00
  YYYY=`echo $INPUTTIME | cut -c1-4 `
  MM=`echo $INPUTTIME |cut -c5-6 `
  DD=`echo $INPUTTIME |cut -c7-8 `
  CYCLE=`echo $INPUTTIME |cut -c9-10 `
## find first available GRIB2 file for the TIME_START_TMP
  NCEPPRODDIR=${COMINnam}/nam.$YYYY$MM$DD
  GRB2FILE=${NCEPPRODDIR}/"nam.t${CYCLE}z.awip12${HH}.tm00.grib2"
  if [ $DBASE == "NAM4" ]
  then
      NCEPPRODDIR=${COMINnam}/'nam.'$YYYY$MM$DD
      GRB2FILE=${NCEPPRODDIR}/"nam.t${CYCLE}z.conusnest.hiresf${HH}.tm00.grib2"
  fi  
  CURRENTTIME=$INPUTTIME
  while [ ! -s $GRB2FILE ]
  do
    echo 'grib2 file is ' $GRB2FILE 'not found'
    CURRENTTIME=`$NDATE -1 $CURRENTTIME `
    if [ $CURRENTTIME -le ` $NDATE -60 $INPUTTIME ` ]
    then
      echo 'no valid atmospheric operational products is available for the given time period'
      msg="FATAL ERROR: no valid atmospheric operational products is available for the given time period    "
      postmsg "$jlogfile" "$msg"
      err=1;export err;err_chk
      touch err.${RUN}.$PDY1.t${HH}z
      exit
    fi
    YYYY=`echo $CURRENTTIME | cut -c1-4 `
    MM=`echo $CURRENTTIME |cut -c5-6 `
    DD=`echo $CURRENTTIME |cut -c7-8 `
    CYCLE=`echo $CURRENTTIME |cut -c9-10 `
    if [ $DBASE == "NAM" ]
    then
       NCEPPRODDIR=${COMINnam}/nam.$YYYY$MM$DD
       GRB2FILE=${NCEPPRODDIR}/"nam.t${CYCLE}z.awip12${HH}.tm00.grib2"
       if [ $OFS == "ciofs" ];  then
          GRB2FILE=${NCEPPRODDIR}/"nam.t${CYCLE}z.awp242${HH}.tm00.grib2"
       fi

    elif [ $DBASE == "NAM4" ]
    then
       NCEPPRODDIR=${COMINnam}/'nam.'$YYYY$MM$DD
       GRB2FILE=${NCEPPRODDIR}/"nam.t${CYCLE}z.conusnest.hiresf${HH}.tm00.grib2"
       if [ $OFS == "ciofs" ];  then
       GRB2FILE=${NCEPPRODDIR}/"nam.t${CYCLE}z.alaskanest.hiresf${HH}.tm00.grib2"
       fi
    elif [ $DBASE == "GFS" ]
    then
       NCEPPRODDIR=${COMINgfs}/'gfs.'$YYYY$MM$DD/${CYCLE}
#       GRB2FILE=${NCEPPRODDIR}/"gfs.t${CYCLE}z.pgrb2.0p50.f${HHH}"
#      For GFSv16
       GRB2FILE=${COMINgfs}/gfs.$YYYY$MM$DD/${CYCLE}/atmos/gfs.t${CYCLE}z.pgrb2.0p50.f${HHH}
    elif [ $DBASE == "GFS25" ]
    then
       NCEPPRODDIR=${COMINgfs}/'gfs.'$YYYY$MM$DD/${CYCLE}
#       GRB2FILE=${NCEPPRODDIR}/"gfs.t${CYCLE}z.pgrb2.0p25.f${HHH}"
#      For GFSv16
       GRB2FILE=${COMINgfs}/gfs.$YYYY$MM$DD/${CYCLE}/atmos/gfs.t${CYCLE}z.pgrb2.0p25.f${HHH}
    fi  
  done
  echo 'Time of the most recent available products is ' $DBASE 'at cycle' $YYYY $MM $DD $CYCLE
########
  YYYY=`echo $CURRENTTIME | cut -c1-4 `
  MM=`echo $CURRENTTIME |cut -c5-6 `
  DD=`echo $CURRENTTIME |cut -c7-8 `
  TMPDATE=$YYYY$MM$DD
  YYYY=`echo $TIME_END | cut -c1-4 `
  MM=`echo $TIME_END |cut -c5-6 `
  DD=`echo $TIME_END |cut -c7-8 `
  ENDDATE=$YYYY$MM$DD
  if [ -s tmp.out ]
  then
     rm -f tmp.out
  fi   
  if [ -s tmp1.out ]
  then
     rm -f tmp1.out
  fi   
#  while [ $TMPDATE -le $ENDDATE ]
#  do 
      if [ $DBASE == "NAM" ]
      then
         if [ $OFS != "ciofs" ]; then
           ls -l ${COMINnam}/nam.${TMPDATE}/nam.t${CYCLE}z.awip12*tm00.grib2 | awk '{print $NF}' >> tmp.out
         elif [ $OFS == "ciofs" ]; then
           ls -l ${COMINnam}/nam.${TMPDATE}/nam.t${CYCLE}z.awp242*tm00.grib2 | awk '{print $NF}' >> tmp.out
         fi
      elif [ $DBASE == "NAM4" ]
      then
         if [ $OFS != "ciofs" ]; then
           ls -l ${COMINnam}/nam.${TMPDATE}/nam.t${CYCLE}z.conusnest.hiresf*.tm00.grib2 | awk '{print $NF}' >> tmp.out
         elif [ $OFS == "ciofs" ]; then
           ls -l ${COMINnam}/nam.${TMPDATE}/nam.t${CYCLE}z.alaskanest.hiresf*.tm00.grib2 | awk '{print $NF}' >> tmp.out
         fi
      elif [ $DBASE == "GFS" ]
      then
         ls -l ${COMINgfs}/gfs.${TMPDATE}/${CYCLE}/atmos/gfs.t${CYCLE}z.pgrb2.0p50.f??? | awk '{print $NF}' >> tmp.out
      elif [ $DBASE == "GFS25" ]
      then
         ls -l ${COMINgfs}/gfs.${TMPDATE}/${CYCLE}/atmos/gfs.t${CYCLE}z.pgrb2.0p25.f??? | awk '{print $NF}' >> tmp.out
      fi  
#
#      CURRENTTIME=`$NDATE +24 $CURRENTTIME `
#      YYYY=`echo $CURRENTTIME | cut -c1-4 `
#      MM=`echo $CURRENTTIME |cut -c5-6 `
#      DD=`echo $CURRENTTIME |cut -c7-8 `
#      TMPDATE=$YYYY$MM$DD
#  done
  if [ -s tmp.out ]
  then
     MET_FILE=${DBASE}_FILE_${RUNTYPE}.dat
     rm -f Fortran_file_search.ctl

     echo $TIME_START_TMP > Fortran_file_search.ctl
     echo $TIME_END >> Fortran_file_search.ctl
     echo tmp.out >> Fortran_file_search.ctl
     echo $MET_FILE >> Fortran_file_search.ctl

     echo "nos_ofs_met_file_search starts at time:" `date`
     $EXECnos/nos_ofs_met_file_search < Fortran_file_search.ctl
     echo "nos_ofs_met_file_search completed at time:" `date`
  fi   
  rm -f *.$DBASE
  if [ -s $MET_FILE ];  then
     exec 5<&0 < $MET_FILE
     while read GRB2FILE 
     do
       read YYYY MM DD CYC HH
       echo $GRB2FILE
       if [ -s $GRB2FILE ]; then
#         TMPDIR1=$(dirname ${GRB2FILE//./_})
#        TMPDIR2=$(basename $TMPDIR1)
#         TMPDIR=${TMPDIR2}/$(basename ${GRB2FILE//./_})
#        TMPGRB1=$TMPDIR/$(basename $GRB2FILE)_nos
        TMPGRB1=${DBASE}.t${CYC}z.f${HH}.grib2_nos
        TMPGRB2=${RUN}_${DBASE}.grib2
        if [ -s $TMPGRB1 ]; then
           rm -f $TMPGRB1
        fi
        if [ -s $TMPGRB2 ]; then
           rm -f $TMPGRB2
        fi
        $WGRIB2 $GRB2FILE | grep -F -f ${FIXnos}/${NET}.met.parmlist.dat | $WGRIB2 -i -grib ${TMPGRB1} $GRB2FILE
        $WGRIB2 $TMPGRB1 -small_grib ${MINLON}:${MAXLON} ${MINLAT}:${MAXLAT} $TMPGRB2 
        (( N = 10#$HH))
        (( N3 = N - 3 ))
        if [ $N3 -lt 0 ]
        then
          N3=0
        fi   
        count=0
        while (( count < $NPP ))
        do
          if [ $N = 0 ]; then
            FPSTG="anl"
          else 
            FPSTG="$N hour fcst"
          fi   
          if [ $count = 16 ]; then
#           FPSTG="${N3}-${N} hour fcst"
            FPSTG="" 
          fi   
          if [ $DBASE == 'GFS' -o $DBASE == 'GFS25' ]; then
            if [ $count -ge 6 -a $count -le 11 ]; then
              FPSTG=""
            fi   
          fi	     
          if [ $count -ge 13 -a $count -le 16 ]; then
 #         if [  $count = 13 -o $count = 14  -o $count = 15 ]; then
              FPSTG=""
          fi
          if [ $DBASE == 'RTMA' ]; then
            FPSTG="anl"
          fi   
          echo decoding ${VARNAME[count]}
          if [ -s tmp.txt ]; then
            rm -f tmp.txt
          fi
          $WGRIB2 $TMPGRB2 -s | grep "${VARNAME[count]}${LEV[count]}$FPSTG" | sed '2,$d' | $WGRIB2 -i $TMPGRB2 -rpn "sto_1:-9999.9:rcl_1:merge:" -spread tmp.txt
       
          if [ $DBASE == 'RUC' -a \( $count = 2 -o $count = 5 \) ]; then
            $WGRIB2 $TMP1GRB2 -s | grep "${VARNAME[count]}${LEV[count]}$FPSTG" | sed '2,$d' | $WGRIB2 -i $TMP1GRB2 -rpn "sto_1:-9999.9:rcl_1:merge:" -spread tmp.txt
          fi   
          if [ -s tmp.txt ]; then
            cat tmp.txt >> ${VARNAME[count]}.$DBASE
          fi	  
          (( count = count + 1 ))
        done
       fi
     done 3<&-  
  else
     echo "no Meteorological product files are found in time period from $TIME_START_TMP to $TIME_END"
     msg="FATAL ERROR: no Meteorological product files are found in time period from $TIME_START_TMP to $TIME_END   "
     postmsg "$jlogfile" "$msg"
  fi   
# special for LMHOFS, creating PRATE.$DBASE and EVP.$DBASE
  if [  "${OFS,,}" == "lmhofs" ]; then
    $USHnos/nos_ofs_residual_water_calculation.sh $RUNTYPE
  fi 

  count=0
  while (( count < $NPP))
  do
     if [ ! -s ${VARNAME[count]}.$DBASE ]
     then
        echo ${VARNAME[count]}.$DBASE
        rm -f ${VARNAME[count]}.$DBASE
     fi   
     (( count = count + 1 ))
   
  done
  rm -f Fortran.ctl ${DBASE}_Fortran.log
  echo ${RUN} > Fortran.ctl 
  echo $DBASE >> Fortran.ctl
  echo $OCEAN_MODEL >> Fortran.ctl
  echo $TIME_START_TMP >> Fortran.ctl
  echo $TIME_END >> Fortran.ctl
  echo $IGRD_MET >> Fortran.ctl
  echo $TMPGRB2 >> Fortran.ctl
  echo $GRIDFILE >> Fortran.ctl
  echo $OUTPUTFILE1 >> Fortran.ctl
  echo $BASE_DATE >> Fortran.ctl 
  echo $MINLON  >> Fortran.ctl
  echo $MINLAT >> Fortran.ctl 
  echo $MAXLON >> Fortran.ctl 
  echo $MAXLAT >> Fortran.ctl 
  echo $SCALE_HFLUX >> Fortran.ctl 
  echo $LANDMASK >> Fortran.ctl

  if [ ! -s $EXECnos/$EXFILE -o  ! -x  $EXECnos/$EXFILE ]
  then
  echo "$EXECnos/$EXFILE does not exist"
  msg="$EXECnos/$EXFILE does not exist  "
  postmsg "$jlogfile" "$msg"
  err=1;export err;err_chk
  exit
  fi

  export pgm=$EXFILE
  . prep_step

  startmsg
  echo "$EXFILE $RUNTYPE starts at time:" `date`
  $EXECnos/$EXFILE < Fortran.ctl > ${DBASE}_Fortran.log
  echo "$EXFILE $RUNTYPE completed at time:" `date`
  export err=$?
  if [ $OCEAN_MODEL == 'SELFE' -o  $OCEAN_MODEL == 'selfe' ]
  then
    if [ -s sflux_rad.nc ]
    then 
      cp -p sflux_rad.nc sflux_rad_1.001.nc
      cp -p sflux_rad.nc ${PREFIXNOS}.t${cyc}z.${PDY1}.flux.$RUNTYPE.nc
    fi
    if [ -s sflux_prc.nc ]
    then 
      cp -p sflux_prc.nc sflux_prc_1.001.nc
      cp -p sflux_prc.nc ${PREFIXNOS}.t${cyc}z.${PDY1}.precip.$RUNTYPE.nc
    fi
  elif [ $OCEAN_MODEL == 'FVCOM' -o  $OCEAN_MODEL == 'fvcom' ]
  then
     if [ ! -s $OUTPUTFILE1 ];then
        echo 'surface forcing file ' $OUTPUTFILE1 'is not generated'
     fi
      
#     if [ -s hflux_fvcom.nc ]
#     then
#        mv hflux_fvcom.nc $OUTPUTFILE1
#     else
#        echo 'surface forcing file ' $OUTPUTFILE1 'is not generate'
#     fi	  	

  fi
## output corms flag information
  if [ -s ${DBASE}_Fortran.log ]
  then
    cp -p ${DBASE}_Fortran.log $COMOUT/$DBASE.${RUNTYPE}_Fortran.t${cyc}z.log

  else
    echo "NO $DBASE.${RUNTYPE}_Fortran.t${cyc}z.log Found"
  fi
  if grep "COMPLETED SUCCESSFULLY" ${DBASE}_Fortran.log /dev/null 2>&1
  then
     echo "MET FORCING ${RUNTYPE} ${DBASE} COMPLETED SUCCESSFULLY 100" >> $cormslogfile
     if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
     then
        echo MET_NOWCAST DONE 100  >> $cormslogfile
     else
        echo MET_FORECAST DONE 100  >> $cormslogfile
     fi
  else
     echo "MET_FORCING ${RUNTYPE} ${DBASE} COMPLETED SUCCESSFULLY 0" >> $cormslogfile
     if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
     then
        echo MET_NOWCAST DONE 0  >> $cormslogfile
     else
        echo MET_FORECAST DONE 0  >> $cormslogfile
     fi
     export err=1
  fi

  if [ $err -ne 0 ]
  then
    echo "$pgm did not complete normally, FATAL ERROR!"
    msg="$pgm did not complete normally, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    err_chk
  else
    echo "$pgm completed normally"
    msg="$pgm completed normally"
    postmsg "$jlogfile" "$msg"
  fi


fi
fi

if [ $OCEAN_MODEL == 'SELFE' -o  $OCEAN_MODEL == 'selfe' ]
then
  tar -cvf ${OUTPUTFILE} sflux_air_1.001.nc sflux_rad_1.001.nc sflux_prc_1.001.nc
fi

   
if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
then

#  echo $DBASE > MET_DBASE.NOWCAST
  export pgm=$MET_NETCDF_1_NOWCAST"_copy"
  . prep_step

  if [ -f $MET_NETCDF_1_NOWCAST ]
  then
    cp $MET_NETCDF_1_NOWCAST $COMOUT/$MET_NETCDF_1_NOWCAST
    export err=$?; err_chk
  else
    echo "NO $MET_NETCDF_1_NOWCAST File Found"
    echo "NO $MET_NETCDF_1_NOWCAST File Found" >> $jlogfile
  fi

  export pgm=$MET_NETCDF_2_NOWCAST"_copy"
  . prep_step

  if [ -f $MET_NETCDF_2_NOWCAST ]
  then
    cp $MET_NETCDF_2_NOWCAST $COMOUT/$MET_NETCDF_2_NOWCAST
#    export err=$?; err_chk
  else
    echo "NO $MET_NETCDF_2_NOWCAST File Found"
    echo "NO $MET_NETCDF_2_NOWCAST File Found" >> $jlogfile
  fi

#  if [ $OCEAN_MODEL == 'ROMS' -o  $OCEAN_MODEL == 'roms' ]; then
#
#    if [ -f $INI_FILE_ROMS_NOWCAST ]; then
#      cp $INI_FILE_ROMS_NOWCAST $COMOUT/$INI_FILE_ROMS_NOWCAST
#      export err=$?; err_chk
#    else
#      echo "NO $INI_FILE_ROMS_NOWCAST File Found"
#      echo "NO $INI_FILE_ROMS_NOWCAST File Found" >> $jlogfile
#    fi
#  fi
elif [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]
then
#  if [ "$met_fore_round" == 2 ]; then
  if [ "$met_fore_round" == 2 -a $DBASE == 'NDFD' ]; then
     $USHnos/nos_ofs_shortwave_longwave_airpressure.sh
  elif [ "$met_fore_round" == 2 -a $DBASE != 'NDFD' ]; then
     $USHnos/nos_ofs_met_forecast_direct_merge.sh    
  fi

#  echo $DBASE > MET_DBASE.FORECAST
  export pgm=$MET_NETCDF_1_FORECAST"_copy"
  . prep_step

  if [ -f $MET_NETCDF_1_FORECAST ]
  then
    cp $MET_NETCDF_1_FORECAST $COMOUT/$MET_NETCDF_1_FORECAST
    export err=$?; err_chk

  else
    echo "NO $MET_NETCDF_1_FORECAST File Found"
    echo "NO $MET_NETCDF_1_FORECAST File Found" >> $jlogfile
  fi

  export pgm=$MET_NETCDF_2_FORECAST"_copy"
  . prep_step

  if [ -f $MET_NETCDF_2_FORECAST ]
  then
    cp $MET_NETCDF_2_FORECAST $COMOUT/$MET_NETCDF_2_FORECAST
  else
    echo "NO $MET_NETCDF_2_FORECAST File Found"
  fi


fi
#if [ $OCEAN_MODEL == 'SELFE' -o  $OCEAN_MODEL == 'selfe' ]
#then
#  tar -cvf ${OUTPUTFILE} sflux_air_1.001.nc sflux_rad_1.001.nc sflux_prc_1.001.nc
#  cp -p ${OUTPUTFILE} $COMOUT/. 
#fi
echo 'The script nos_ofs_create_forcing_met.sh completed at time: ' `date `
exit   
