#!/bin/sh
# Script Name:  nos_ofs_create_forcing_obc.sh
#
# Purpose:
#   This program is used to generate Open Boundary Condition file for NOS OFS from global or regional
#   larger domain ocean model such as G-RTOFS, Navy's NCOM and HYCOM, ETSS.
#   subtidal water levels are from NCEP Extra Tropical Storm Surge (ETSS)
#   gridded operational products of grib2 files. The temperature and salinity open boundary
#   conditions are generated from Navy Coastal Ocean Modeling (NCOM) operational products
#   if DBASE_TS=NCOM or from climatological dataset World Ocean Atlas 2005 if DBASE_TS=WOA05.
#   The most recently available products for the given time is searched and read in.
#   Several horizontal interpolation methods (determined by IGRD_OBC) are implemented,
#   and linear method is used for vertical interpolation from NCOM vertical coordinates
#   to ROMS sigma coordinates. Tidal forcing can also be generated from ADCIRC EC2001
#   database, and adjusted by the provided harmonic constants if needed.

#   The missing variables are filled with a missing value of -99999.0.
#
# Directory Location:   ~/scripts 
# Technical Contact:   	Aijun Zhang         Org:  NOS/CO-OPS
#                       Phone: 301-7132890 ext. 127  E-Mail: aijun.zhang@noaa.gov
#
# Usage: ./nos_ofs_create_forcing_obc.sh OFS 
#
# Input Parameters:
#  OFS:         Name of Operational Forecast System, e.g., TBOFS, CBOFS, DBOFS
#  TIME_START:  start time to grab data, e.g., YYYYMMDDHH (2008101500)
#  TIME_END:    end time to grab data, e.g., YYYYMMDDHH (2008101600)
#  DBASE_WL:    Data source Name of water level open boundary conditions
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
#   ${PREFIXNOS}.${cycle}.$yyyy$mm$dd.obc.nc   
#   ${PREFIXNOS}.roms.tides.nc
#   Fortran_OBC.log
#   Fortran_Tide.log
#
# Libraries Used: see the makefile
#  
# Error Conditions:
#  
# Modification History:
#  (1). Degui Cao
#       Implementation                    01/14/2010
#
# -------------------------------------------------------------------------
set -x

echo 'The script nos_ofs_create_forcing_obc.sh started at UTC' `date`
echo 'The script nos_ofs_create_forcing_obc.sh started at UTC' `date` >> $jlogfile
echo 'The script nos_ofs_create_forcing_obc.sh started at UTC' `date` >> $nosjlogfile

TIME_START=$time_hotstart
TIME_END=$time_forecastend
TIME_NOW=$time_nowcastend
DBASE_WL=$DBASE_WL_NOW
DBASE_TS=$DBASE_TS_NOW

#typeset -Z2 HH HH3 CYCLE
YYYY=`echo $TIME_START | cut -c1-4 `
MM=`echo $TIME_START |cut -c5-6 `
DD=`echo $TIME_START |cut -c7-8 `
HH=`echo $TIME_START |cut -c9-10 `
 
##  create nest OBC file for NEGOFS/NWGOFS
if [ "${OFS,,}" == "negofs" -o "${OFS,,}" == "nwgofs" ]; then
    echo $OFS > ${PREFIXNOS}.obc.ctl
    echo $OCEAN_MODEL >> ${PREFIXNOS}.obc.ctl
    echo $TIME_START >> ${PREFIXNOS}.obc.ctl
    echo $TIME_END >> ${PREFIXNOS}.obc.ctl
    echo $OBC_FORCING_FILE >> ${PREFIXNOS}.obc.ctl
    echo $cormslogfile >> ${PREFIXNOS}.obc.ctl
    echo $BASE_DATE >> ${PREFIXNOS}.obc.ctl
    echo $KBm >> ${PREFIXNOS}.obc.ctl
    echo '1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41' >> ${PREFIXNOS}.obc.ctl
    echo $NESTED_PARENT_OFS >> ${PREFIXNOS}.obc.ctl
    echo $COMINnestedparent >> ${PREFIXNOS}.obc.ctl

## added by Zheng on 07/17/2018
    echo $HOMEnos/fix/$NESTED_PARENT_OFS/${PREFIXNOS}.$NESTED_PARENT_OFS.${OFS}.obc.center.nc >> ${PREFIXNOS}.obc.ctl
## added by Zheng on 07/17/2018

    $EXECnos/nos_ofs_create_forcing_obc_fvcom_nest < ${PREFIXNOS}.obc.ctl  >> Fortran_OBC_${OFS}.log
    if grep "COMPLETED SUCCESSFULLY" Fortran_OBC_${OFS}.log /dev/null 2>&1
    then
       echo "${OFS} OBC FILE COMPLETED SUCCESSFULLY 100" >> $cormslogfile
       export err=0
    else
       echo " ${OFS} OBC COMPLETED SUCCESSFULLY 0" >> $cormslogfile
       echo "please check $NESTED_PARENT_OFS and $COMINnestedparent"
       echo "in jobs/JNOS_OFS_PREP"
       echo "Please check Fortran_OBC_${OFS}.log for more detail info."

       export err=1
    fi
#    export err=$?
    pgm='nos_ofs_create_forcing_obc_fvcom_nest'
    if [ $err -ne 0 ]
    then
       echo "Generating ${OFS} OBC forcing file did not complete normally, FATAL ERROR!"
       msg=" Generating ${OFS} OBC forcing file did not complete normally, FATAL ERROR!"
       postmsg "$jlogfile" "$msg"
       err_chk
    else
       echo "$pgm completed normally"
       msg="$pgm completed normally"
       postmsg "$jlogfile" "$msg"
    fi

    cp ${DATA}/${OBC_FORCING_FILE}  $COMOUT/.

    if [ $SENDDBN = YES ]; then
      $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/$OBC_FORCING_FILE
    fi

    echo "NEST OBC File Finished"
    exit 0
fi

##  create tidal forcing netCDF file for ROMS
if [ $OCEAN_MODEL == "ROMS" -o $OCEAN_MODEL == "roms" ]
then
 if [ $CREATE_TIDEFORCING -gt 0 -o ! -s $HC_FILE_OFS ]; then
  echo 'creating tidal forcing netCDF file for ROMS !!! '
  rm -f Fortran_Tide.ctl Fortran_Tide.log
  echo ${OFS} > Fortran_Tide.ctl
  echo $OCEAN_MODEL >> Fortran_Tide.ctl
  echo ${TIME_START}00 >> Fortran_Tide.ctl
  echo $GRIDFILE >> Fortran_Tide.ctl
  echo $HC_FILE_OBC >> Fortran_Tide.ctl
  echo $HC_FILE_OFS >> Fortran_Tide.ctl
  echo $BASE_DATE >> Fortran_Tide.ctl 

  export pgm=nos_ofs_create_forcing_obc_tides
  . prep_step

  startmsg

  $EXECnos/nos_ofs_create_forcing_obc_tides < Fortran_Tide.ctl > Fortran_Tide.log
  export err=$?

  if [ $err -ne 0 ]
  then
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

## output corms flag information
  if grep "COMPLETED SUCCESSFULLY" Fortran_Tide.log /dev/null 2>&1
  then
     echo "TIDAL FORCING COMPLETED SUCCESSFULLY 100" >> $cormslogfile
     echo "TIDAL_FORCING DONE 100 " >> $cormslogfile

  else
     echo "TIDAL FORCING COMPLETED SUCCESSFULLY 0" >> $cormslogfile
     echo "TIDAL_FORCING DONE 0 " >> $cormslogfile
  fi
 else
  echo "USE OLD TIDAL FORCING FILE 100" >> $cormslogfile
  echo "TIDAL_FORCING DONE 100 " >> $cormslogfile 
 fi
 if [ -f "$HC_FILE_OFS" ]; then
    cp $HC_FILE_OFS $COMOUT/$OBC_TIDALFORCING_FILE 
    if [ $SENDDBN = YES ]; then
      if [ "${OFS,,}" == "gomofs" -o "${OFS,,}" == "ciofs" ]; then
        $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF_LRG $job $COMOUT/$OBC_TIDALFORCING_FILE
      else
        $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/$OBC_TIDALFORCING_FILE
      fi
    fi
 fi
fi
# for sure to get enough data, two more hours will be acquired. 
#TIME_START=`$NDATE -1 $TIME_START `
#TIME_END=`$NDATE +1 $TIME_END `

#     Find and Process available RTOFS NetCDF files       
# --------------------------------------------------------------------------
if [ $DBASE_TS == 'RTOFS' ]; then

  COMTMP=$COMINrtofs_3d
  echo 'process G-RTOFS from : ' $TIME_START 'to ' $TIME_END
#  typeset -Z2 HH HH3 CYCLE
#  typeset -Z3 fhr
  TIME_STARTm1=`$NDATE -24 $TIME_START `   # increase for one day = 24 hours
  TIME_ENDp1=`$NDATE +24 $TIME_END `  
  TIME_ENDm3=`$NDATE -3 $TIME_END `
  YYYY=`echo $TIME_STARTm1 | cut -c1-4 `
  MM=`echo $TIME_STARTm1 |cut -c5-6 `
  DD=`echo $TIME_STARTm1 |cut -c7-8 `
  HH=`echo $TIME_STARTm1 |cut -c9-10 `
  CYCLE=00
  reg="US_east"
  if [ "${OFS,,}" == "creofs" -o "${OFS,,}" == "sfbofs" -o -z "${OFS##wcofs*}"  -o "${OFS,,}" == "sscofs" ]; then
    reg="US_west"
  elif [ "${OFS,,}" == "ciofs" ]; then
    reg="alaska"
  fi
  NCEPPRODDIR=${COMINrtofs_3d}'/rtofs.'$YYYY$MM$DD
  N  FILE9=`ls -l ${NCEPPRODDIR}/rtofs_glo_3dz_f*_6hrly_hvr_${reg}.nc | wc -l`
  if [ $NFILE9 -le 0 ]; then
      echo "WARNING: there is no RTOFS product found"
      echo "use HYCOM as backup"
#      if [ $envir = dev ]; then
#        export maillist='nos.co-ops.modelingteam@noaa.gov'
#      fi
#      export maillist=${maillist:-'nco.spa@noaa.gov,nos.co-ops.modelingteam@noaa.gov'}
      export subject="WARNING COULD NOT FOUND RTOFS FILE for $PDY t${cyc}z $job"
      echo "*************************************************************" > mailmsg
       echo "*** WARNING !! COULD NOT FIND RTOFS FILES  *** " >> mailmsg
       echo "*************************************************************" >> mailmsg
       echo >> mailmsg
       echo "   $NCEPPRODDIR " >> mailmsg
       echo " backup HYCOM is used "  >> mailmsg
       echo >> mailmsg
       echo "check availability of RTOFS FILE " >> mailmsg
       cat mailmsg > $COMOUT/${RUN}.t${cyc}z.rtofs.emailbody
       cat $COMOUT/${RUN}.t${cyc}z.rtofs.emailbody | mail.py -s "$subject" $maillist -v
  fi
  FILESIZE=260000000
  if [ $reg == "US_east" ]; then
     FILESIZE=260000000
  elif [ $reg == "US_west" ]; then
     FILESIZE=280000000
  elif [ $reg == "alaska" ]; then
     FILESIZE=130000000
  fi
  TMPDATE[1]='0000000000'
  TMPFILE[1]=''
  LASTDATE='0000000000'
  NFILE=0
  CURRENTTIME=$YYYY$MM${DD}00

  if [ -s RTOFS_FILE ]; then
     rm -f  RTOFS_FILE
  fi  
  NCEPPRODDIR=${COMINrtofs_3d}'/rtofs.'$YYYY$MM$DD
  NFILE9=`ls -l ${NCEPPRODDIR}/rtofs_glo_3dz_f*_6hrly_hvr_${reg}.nc | wc -l`
  if [ $NFILE9 -le 0 ]; then
    echo 'No 3D RTOFS is found in '${NCEPPRODDIR}
    CURRENTTIME=`$NDATE -24 $CURRENTTIME`
    YYYY=`echo $CURRENTTIME |cut -c1-4 `
    MM=`echo $CURRENTTIME |cut -c5-6 `
    DD=`echo $CURRENTTIME |cut -c7-8 `
    NCEPPRODDIR=${COMINrtofs_3d}'/rtofs.'$YYYY$MM$DD
    NFILE9=`ls -l ${NCEPPRODDIR}/rtofs_glo_3dz_f*_6hrly_hvr_${reg}.nc | wc -l`
    if [ $NFILE9 -le 0 ]; then
      echo 'No 3D RTOFS is found in '${NCEPPRODDIR}
      CURRENTTIME=`$NDATE -24 $YYYY$MM${DD}00 `
      YYYY=`echo $CURRENTTIME |cut -c1-4 `
      MM=`echo $CURRENTTIME |cut -c5-6 `
      DD=`echo $CURRENTTIME |cut -c7-8 `
      NCEPPRODDIR=${COMINrtofs_3d}'/rtofs.'$YYYY$MM$DD
      NFILE9=`ls -l ${NCEPPRODDIR}/rtofs_glo_3dz_f*_6hrly_hvr_${reg}.nc | wc -l`
    fi
  fi
  while [ $CURRENTTIME -le $TIME_NOW ]
  do 
     YYYY=`echo $CURRENTTIME |cut -c1-4 `
       MM=`echo $CURRENTTIME |cut -c5-6 `
       DD=`echo $CURRENTTIME |cut -c7-8 `
    CYCLE=`echo $CURRENTTIME |cut -c9-10 `
    NCEPPRODDIR=${COMINrtofs_3d}'/rtofs.'$YYYY$MM$DD
    N=1
    while (( N <= 192))
    do
#      fhr=$N
      fhr=`echo $N |  awk '{printf("%03i",$1)}'`
      CDFFILE=${NCEPPRODDIR}/rtofs_glo_3dz_f${fhr}_6hrly_hvr_${reg}.nc
      if [ -s $CDFFILE ]; then
        filesize=`wc -c $CDFFILE | awk '{print $1}' `
        if [ $filesize -gt $FILESIZE ]; then
	  echo 'NFILE= ' $NFILE
          TMPDATE1=`$NDATE +${fhr} $YYYY$MM${DD}00 `
	  if [ $TMPDATE1 -gt $LASTDATE ]; then
	     TMPDATE[NFILE]=$TMPDATE1
	     TMPFILE[NFILE]=$CDFFILE
	     LASTDATE=$TMPDATE1
             NFILE=`expr $NFILE + 1`
          else
             I=0
             while (( I < 10#$NFILE ))
             do
              if [ ${TMPDATE[I]} -eq $TMPDATE1 ]; then
	         TMPDATE[I]=$TMPDATE1
	         TMPFILE[I]=$CDFFILE
              fi   
              (( I = I + 1 ))
             done  
	  fi 
        fi 
      fi  
      (( N = N + 1 ))
    done  
    CURRENTTIME=`$NDATE +24 $CURRENTTIME `
  done
  TIME_STARTm3=`$NDATE -6 $TIME_START `   # increase for one day = 24 hours
  if [ $NFILE -ge 1 ]; then
    I=0
    MAXNFILE=0
    while (( I < 10#$NFILE ))
    do
       if [ ${TMPDATE[I]} -ge $TIME_STARTm3 -a ${TMPDATE[I]} -le $TIME_ENDp1 ]; then
         echo ${TMPFILE[I]} >> RTOFS_FILE
         MAXDATE=${TMPDATE[I]}
         MAXNFILE=`expr $MAXNFILE + 1 `
       fi   
       (( I = I + 1 ))
    done 
    
     if [ $MAXDATE -lt $TIME_ENDm3 ]; then
      echo 'WARNING: RTOFS products do not cover from : ' $TIME_START 'to ' $TIME_END
      echo "Please check RTOFS Products on " $COMINrtofs_3d
      echo "switching to backup products HYCOM for TS and ETSS for WL"
      export DBASE_TS='HYCOM'
      export DBASE_WL="ETSS"
    fi
  else
      echo 'WARNING: RTOFS products do not cover from : ' $TIME_START 'to ' $TIME_END
      echo "Please check RTOFS Products on " $COMINrtofs_3d
      echo "switching to backup products HYCOM for TS and ETSS for WL"
      export DBASE_TS='HYCOM'    
      export DBASE_WL="ETSS"
  fi
  if [ ! -s RTOFS_FILE ]; then
      echo 'WARNING: RTOFS products do not cover from : ' $TIME_START 'to ' $TIME_END
      echo "Please check RTOFS Products on " $COMINrtofs_3d
      echo "switching to backup products HYCOM for TS and ETSS for WL"
      export DBASE_TS='HYCOM'
      export DBASE_WL="ETSS"
  fi  
fi
# --------------------------------------------------------------------------
#     Find and Process available HYCOM NetCDF files       
# --------------------------------------------------------------------------
if [ $DBASE_TS == 'HYCOM' ]; then
  COMTMP=$DCOMINncom
  echo 'process G-HYCOM from : ' $TIME_START 'to ' $TIME_END
#  typeset -Z2 HH HH3 CYCLE
#  typeset -Z3 fhr
  TIME_STARTm1=`$NDATE -24 $TIME_START `   # increase for one day = 24 hours
  YYYY=`echo $TIME_STARTm1 | cut -c1-4 `
  MM=`echo $TIME_STARTm1 |cut -c5-6 `
  DD=`echo $TIME_STARTm1 |cut -c7-8 `
  HH=`echo $TIME_STARTm1 |cut -c9-10 `
  CYCLE=00
  reg=regp01
  if [ "${OFS,,}" == "creofs" -o "${OFS,,}" == "sfbofs" -o -z "${OFS##wcofs*}" ]; then
    reg=regp07
  elif [  "${OFS,,}"  == "ciofs" ]; then
    reg=regp06
  fi
  FILESIZE=176000000
  if [ $reg == "regp01" ]; then
     FILESIZE=57000000
  elif [ $reg == "regp07" ]; then
     FILESIZE=48000000
  elif [ $reg == "regp06" ]; then
    FILESIZE=90000000
  elif [ $reg == "regp17" ]; then
    FILESIZE=19000000 
  fi
  TMPDATE[1]='0000000000'
  TMPFILE[1]=''
  LASTDATE='0000000000'
  NFILE=0
  CURRENTTIME=$YYYY$MM${DD}00

  if [ -s HYCOM_FILE ]; then
     rm -f  HYCOM_FILE
  fi  
  NCOM_PRE="hycom_glb_${reg}_"
  NCOM_PRE_TIDE="hycom_glb_${reg}_tides_"

  NCEPPRODDIR=${COMTMP}'/'$YYYY$MM$DD/wgrdbul/navy_hycom
  NFILE9=`ls -l ${NCEPPRODDIR}/${NCOM_PRE}*.nc.gz | wc -l`
  if [ $NFILE9 -le 35 ]; then  #to forecast hour 72
    echo 'Not enough 3D HYCOM files are found in '${NCEPPRODDIR}
    CURRENTTIME=`$NDATE -24 $CURRENTTIME`
    YYYY=`echo $CURRENTTIME |cut -c1-4 `
    MM=`echo $CURRENTTIME |cut -c5-6 `
    DD=`echo $CURRENTTIME |cut -c7-8 `
    NCEPPRODDIR=${COMTMP}'/'$YYYY$MM$DD/wgrdbul
    NFILE9=`ls -l ${NCEPPRODDIR}/${NCOM_PRE}*.nc.gz | wc -l`
    if [ $NFILE9 -le 35 ]; then # to forecast hour 96 of previous day
      echo 'Not enough 3D HYCOM files are found in '${NCEPPRODDIR}
      CURRENTTIME=`$NDATE -24 $YYYY$MM${DD}00 `
      YYYY=`echo $CURRENTTIME |cut -c1-4 `
      MM=`echo $CURRENTTIME |cut -c5-6 `
      DD=`echo $CURRENTTIME |cut -c7-8 `
      NCEPPRODDIR=${COMTMP}'/'$YYYY$MM$DD/wgrdbul
      NFILE9=`ls -l ${NCEPPRODDIR}/${NCOM_PRE}*.nc.gz | wc -l`
    fi
  fi
  while [ $CURRENTTIME -le $TIME_NOW ]
  do 
     YYYY=`echo $CURRENTTIME |cut -c1-4 `
       MM=`echo $CURRENTTIME |cut -c5-6 `
       DD=`echo $CURRENTTIME |cut -c7-8 `
    CYCLE=`echo $CURRENTTIME |cut -c9-10 `

    NCEPPRODDIR=${COMTMP}'/'$YYYY$MM$DD/wgrdbul/navy_hycom
    N=1
    while (( N <= 144))
    do
#      fhr=$N
      fhr=`echo $N |  awk '{printf("%03i",$1)}'`
      CDFFILE=${NCEPPRODDIR}/${NCOM_PRE}$YYYY$MM$DD"00_t"${fhr}.nc.gz
      HYCOMFILE=${NCOM_PRE}$YYYY$MM$DD"00_t"${fhr}.nc
      if [ -s $CDFFILE ]; then
        filesize=`wc -c $CDFFILE | awk '{print $1}' `
        if [ $filesize -ge $FILESIZE ]; then
	  echo 'NFILE= ' $NFILE
          gunzip -c $CDFFILE > ${HYCOMFILE}
          TMPDATE1=`$NDATE +${fhr} $YYYY$MM${DD}00 `
	  if [ $TMPDATE1 -gt $LASTDATE ]; then
	     TMPDATE[NFILE]=$TMPDATE1
	     TMPFILE[NFILE]=$HYCOMFILE
	     LASTDATE=$TMPDATE1
             NFILE=`expr $NFILE + 1`
          else
             I=0
             while (( I < 10#$NFILE ))
             do
              if [ ${TMPDATE[I]} -eq $TMPDATE1 ]; then
	         TMPDATE[I]=$TMPDATE1
	         TMPFILE[I]=$HYCOMFILE
              fi   
              (( I = I + 1 ))
             done  
	  fi 
        fi 
      fi  
      (( N = N + 1 ))
    done  
    CURRENTTIME=`$NDATE +24 $CURRENTTIME `  # increase for one day = 24 hours
  done
  TIME_STARTm3=`$NDATE -6 $TIME_START `   
  if [ $NFILE -gt 1 ]; then
    I=0
    while (( I < 10#$NFILE ))
    do
      if [ ${TMPDATE[I]} -ge $TIME_STARTm3 -a ${TMPDATE[I]} -le $TIME_END ]; then
        echo ${TMPFILE[I]} >> HYCOM_FILE
      fi   
      (( I = I + 1 ))
    done  
  else 
     DBASE_TS='WOA05'
     DBASE_WL='ETSS'

     echo "WARNING: Neither RTOFS nor HYCOM model data are available" 
     echo "switching to backup products WOA05 for TS and ETSS for WL"
     echo "WARNING: Neither RTOFS nor HYCOM model data are available" >> $jlogfile 
     echo "switching to backup products WOA05 for TS and ETSS for WL" >> $jlogfile 

  fi  
fi
# --------------------------------------------------------------------------
#     End of process HYCOM forecast files       
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
#     Find and Process available NCOM forecast files by the provided start time       
# --------------------------------------------------------------------------
if [ $DBASE_TS == 'NCOM' ]; then
  echo 'Find Good NCOM files from : ' $time_hotstart 'to ' $time_forecastend
  if [ -s NCOM_FILE ]; then
    rm -f NCOM_FILE
  fi   
  YYYY=`echo $TIME_START | cut -c1-4 `
  MM=`echo $TIME_START |cut -c5-6 `
  DD=`echo $TIME_START |cut -c7-8 `
  HH=`echo $TIME_START |cut -c9-10 `
  CYCLE=00
  NCOM_PRE="ncom_glb_regp01_"
  NCOM_PRE_TIDE="ncom_glb_regp01_tides_"
  FILESIZE=570000000
  if [ "${OFS,,}" == "creofs" -o "${OFS,,}" == "sfbofs" -o "${OFS,,}" == "sscofs" -o -z "${OFS##wcofs*}" ]; then
    NCOM_PRE="ncom_glb_regp07_"
    NCOM_PRE_TIDE="ncom_glb_regp07_tides_"
    FILESIZE=460000000
  fi
  NCEPPRODDIR=${COMTMP}'/'$YYYY$MM$DD/wgrdbul
  NCOMFILE=${NCEPPRODDIR}/${NCOM_PRE}$YYYY$MM$DD${CYCLE}".nc.gz"
  NCOMTIDE=${NCEPPRODDIR}/${NCOM_PRE_TIDE}$YYYY$MM$DD${CYCLE}".nc.gz"
  INPUTTIME=$YYYY$MM$DD$CYCLE
  CURRENTTIME=$INPUTTIME
  if [ -s $NCOMFILE ]
  then
    filesize=`wc -c $NCOMFILE | awk '{print $1}' `
    if [ $filesize -lt $FILESIZE ]
    then
      echo 'Size of theNCOM file is too small'
      CURRENTTIME=`$NDATE -1 $CURRENTTIME `
      YYYY=`echo $CURRENTTIME | cut -c1-4 `
      MM=`echo $CURRENTTIME |cut -c5-6 `
      DD=`echo $CURRENTTIME |cut -c7-8 `
      CYCLE=`echo $CURRENTTIME |cut -c9-10 `
      NCEPPRODDIR=${COMTMP}'/'$YYYY$MM$DD/wgrdbul
#     NCOMFILE=${NCEPPRODDIR}/"ncom_glb_regp01_"$YYYY$MM$DD${CYCLE}".nc.gz"
#     NCOMTIDE=${NCEPPRODDIR}/"ncom_glb_regp01_tides_"$YYYY$MM$DD${CYCLE}".nc.gz"
      NCOMFILE=${NCEPPRODDIR}/${NCOM_PRE}$YYYY$MM$DD${CYCLE}".nc.gz"
      NCOMTIDE=${NCEPPRODDIR}/${NCOM_PRE_TIDE}$YYYY$MM$DD${CYCLE}".nc.gz"

    fi 
  fi  
  while [ ! -s $NCOMFILE ]
  do
    echo 'NCOM file is ' $NCOMFILE 'not found'
    CURRENTTIME=`$NDATE -1 $CURRENTTIME `
    if [ $CURRENTTIME -le ` $NDATE -72 $INPUTTIME ` ]
    then
       echo 'no valid NCOM operational products is available for the given time period'
       msg="FATAL ERROR: no valid NCOM operational products is available for the given time period   "
       postmsg "$jlogfile" "$msg"
       postmsg "$nosjlogfile" "$msg"
       DBASE_TS='WOA05'
       DBASE_WL='ETSS'
    fi
    YYYY=`echo $CURRENTTIME | cut -c1-4 `
    MM=`echo $CURRENTTIME |cut -c5-6 `
    DD=`echo $CURRENTTIME |cut -c7-8 `
    CYCLE=`echo $CURRENTTIME |cut -c9-10 `
    NCEPPRODDIR=${COMTMP}'/'$YYYY$MM$DD/wgrdbul
 #  NCOMFILE=${NCEPPRODDIR}/"ncom_glb_regp01_"$YYYY$MM$DD${CYCLE}".nc.gz"
 #  NCOMTIDE=${NCEPPRODDIR}/"ncom_glb_regp01_tides_"$YYYY$MM$DD${CYCLE}".nc.gz"
    NCOMFILE=${NCEPPRODDIR}/${NCOM_PRE}$YYYY$MM$DD${CYCLE}".nc.gz"
    NCOMTIDE=${NCEPPRODDIR}/${NCOM_PRE_TIDE}$YYYY$MM$DD${CYCLE}".nc.gz"

  done
  filesize=`wc -c $NCOMFILE | awk '{print $1}' `
  if [ $filesize -gt $FILESIZE ]
  then
    gunzip -c $NCOMFILE >  ${NCOM_PRE}$YYYY$MM$DD${CYCLE}.nc
    if [ -s $NCOMTIDE ]
    then
      gunzip -c $NCOMTIDE >  ${NCOM_PRE_TIDE}$YYYY$MM$DD${CYCLE}.nc
    fi
    echo ${NCOM_PRE}$YYYY$MM$DD${CYCLE}.nc  > NCOM_FILE
    echo ${NCOM_PRE_TIDE}$YYYY$MM$DD${CYCLE}.nc  >> NCOM_FILE
#   echo ncom_glb_regp01_"$YYYY$MM$DD${CYCLE}".nc > NCOM_FILE
#   echo ncom_glb_regp01_tides_"$YYYY$MM$DD${CYCLE}".nc >> NCOM_FILE
  fi  
  echo "first NCOM file : " $NCOMFILE " is found"
  echo "Time of the first available NCOM products is at cycle" $YYYY $MM $DD $CYCLE
  while [ $CURRENTTIME -le $TIME_NOW ]
  do 
    CURRENTTIME=`$NDATE +1 $CURRENTTIME `
    YYYY=`echo $CURRENTTIME | cut -c1-4 `
    MM=`echo $CURRENTTIME |cut -c5-6 `
    DD=`echo $CURRENTTIME |cut -c7-8 `
    CYCLE=`echo $CURRENTTIME |cut -c9-10 `
    NCEPPRODDIR=${COMTMP}'/'$YYYY$MM$DD/wgrdbul
#   NCOMFILE=${NCEPPRODDIR}/"ncom_glb_regp01_"$YYYY$MM$DD${CYCLE}".nc.gz"
#   NCOMTIDE=${NCEPPRODDIR}/"ncom_glb_regp01_tides_"$YYYY$MM$DD${CYCLE}".nc.gz"
    NCOMFILE=${NCEPPRODDIR}/${NCOM_PRE}$YYYY$MM$DD${CYCLE}".nc.gz"
    NCOMTIDE=${NCEPPRODDIR}/${NCOM_PRE_TIDE}$YYYY$MM$DD${CYCLE}".nc.gz"

    if [ -s $NCOMFILE ]
    then
      filesize=`wc -c $NCOMFILE | awk '{print $1}' `
      if [ $filesize -gt $FILESIZE ]
      then
        gunzip -c $NCOMFILE >  ${NCOM_PRE}$YYYY$MM$DD${CYCLE}.nc
        if [ -s $NCOMTIDE ]
        then
          gunzip -c $NCOMTIDE >  ${NCOM_PRE_TIDE}$YYYY$MM$DD${CYCLE}.nc
        fi

 #      gunzip -c $NCOMFILE >  ncom_glb_regp01_"$YYYY$MM$DD${CYCLE}".nc
 #      if [ -s $NCOMTIDE ]
 #      then
 #        gunzip -c $NCOMTIDE >  ncom_glb_regp01_tides_"$YYYY$MM$DD${CYCLE}".nc
 #      fi
 #      echo ncom_glb_regp01_"$YYYY$MM$DD${CYCLE}".nc >> NCOM_FILE
 #      echo ncom_glb_regp01_tides_"$YYYY$MM$DD${CYCLE}".nc >> NCOM_FILE
        echo ${NCOM_PRE}$YYYY$MM$DD${CYCLE}.nc  >> NCOM_FILE
        echo ${NCOM_PRE_TIDE}$YYYY$MM$DD${CYCLE}.nc  >> NCOM_FILE

      fi  
    fi  
  done  
fi
# --------------------------------------------------------------------------
#     End of searching NCOM forecast files       
#     create Open Boundary forcing File (subtidal WL, T, and S)for OFS
# --------------------------------------------------------------------------
if [ $DBASE_WL == RTOFS ]; then

  TMPDATE[1]='0000000000'
  TMPFILE[1]=''
  LASTDATE='0000000000'
  NFILE=0
  CURRENTTIME=`$NDATE -24 $TIME_START `   # increase for one day = 24 hours
#  CURRENTTIME=$TIME_START
  if [ -s RTOFS_FILE_WL ]; then
     rm -f  RTOFS_FILE_WL
  fi  
  NCEPPRODDIR=${COMINrtofs_2d}'/rtofs.'$YYYY$MM$DD
#  NFILE9=`ls -l ${NCEPPRODDIR}/rtofs_glo_2ds_f*_3hrly_diag.nc | wc -l`
  NFILE9=`ls -l ${NCEPPRODDIR}/rtofs_glo_2ds_f*_diag.nc | wc -l`
  if [ $NFILE9 -le 0 ]; then
    echo 'No 2D RTOFS is found in '${NCEPPRODDIR}
    CURRENTTIME=`$NDATE -24 $CURRENTTIME`
    YYYY=`echo $CURRENTTIME |cut -c1-4 `
    MM=`echo $CURRENTTIME |cut -c5-6 `
    DD=`echo $CURRENTTIME |cut -c7-8 `
    NCEPPRODDIR=${COMINrtofs_2d}'/rtofs.'$YYYY$MM$DD
    NFILE9=`ls -l ${NCEPPRODDIR}/rtofs_glo_2ds_f*_diag.nc | wc -l`
    if [ $NFILE9 -le 0 ]; then
      echo 'No 2D RTOFS is found in '${NCEPPRODDIR}
      CURRENTTIME=`$NDATE -24 $YYYY$MM${DD}00 `
      YYYY=`echo $CURRENTTIME |cut -c1-4 `
      MM=`echo $CURRENTTIME |cut -c5-6 `
      DD=`echo $CURRENTTIME |cut -c7-8 `
      NCEPPRODDIR=${COMINrtofs_2d}'/rtofs.'$YYYY$MM$DD
      NFILE9=`ls -l ${NCEPPRODDIR}/rtofs_glo_2ds_f*_diag.nc | wc -l`
    fi
  fi
  while [ $CURRENTTIME -le $TIME_NOW ]
  do 
     YYYY=`echo $CURRENTTIME |cut -c1-4 `
       MM=`echo $CURRENTTIME |cut -c5-6 `
       DD=`echo $CURRENTTIME |cut -c7-8 `
    CYCLE=`echo $CURRENTTIME |cut -c9-10 `
    NCEPPRODDIR=${COMINrtofs_2d}'/rtofs.'$YYYY$MM$DD
    N=1
    while (( N <= 144))
    do
#      fhr=$N
      fhr=`echo $N |  awk '{printf("%03i",$1)}'`
      CDFFILE=${NCEPPRODDIR}/rtofs_glo_2ds_f"${fhr}"_3hrly_diag.nc
      if [ ! -s $CDFFILE ]; then
        CDFFILE=${NCEPPRODDIR}/rtofs_glo_2ds_f"${fhr}"_diag.nc
      fi
      if [ -s $CDFFILE ]; then
         TMPDATE1=`$NDATE +${fhr} $YYYY$MM${DD}00 `
	 if [ $TMPDATE1 -gt $LASTDATE ]; then
	     TMPDATE[NFILE]=$TMPDATE1
	     TMPFILE[NFILE]=$CDFFILE
	     LASTDATE=$TMPDATE1
             NFILE=`expr $NFILE + 1`
          else
             I=0
             while (( I < 10#$NFILE ))
             do
              if [ ${TMPDATE[I]} -eq $TMPDATE1 ]; then
	         TMPDATE[I]=$TMPDATE1
	         TMPFILE[I]=$CDFFILE
              fi   
              (( I = I + 1 ))
             done  
         fi 
      fi  
      (( N = N + 1 ))
    done  
    CURRENTTIME=`$NDATE +24 $CURRENTTIME `
  done
  TIME_STARTm3=`$NDATE -6 $TIME_START `   # increase for one day = 24 hours
  I=0
  while (( I < 10#$NFILE ))
  do
   if [ ${TMPDATE[I]} -ge $TIME_STARTm3 -a ${TMPDATE[I]} -le $TIME_END ]; then
      echo ${TMPFILE[I]} >> RTOFS_FILE_WL
   fi   
   (( I = I + 1 ))
  done  
  if [ ! -s RTOFS_FILE_WL ]; then
    export DBASE_WL="ETSS"
      echo "WARNING: no RTOFS data avaiable for WL" 
      echo "         switching to backup products ETSS" 
      echo "WARNING: no RTOFS data avaiable for WL" >> $jlogfile  
      echo "         switching to backup products ETSS" >> $jlogfile  

  fi
fi


#  create subtidal water level open Boundary forcing File from ETSS
if [ $DBASE_WL == "ETSS" ]; then
  if [ -s ETSS_FILE ]; then
    rm -f ETSS_FILE
  fi   

# --------------------------------------------------------------------------
#     process ETSS using $WGRIB utility
#     Find the first available ETSS forecast file by the provided start time       
# --------------------------------------------------------------------------
  echo 'process ETSS from : ' $time_hotstart 'to ' $time_forecastend
  CYCLE=$HH
  cyc1=t${HH}z
  NCEPPRODDIR=${COMINetss}'/etss.'$YYYY$MM$DD
  GRB2FILE=${NCEPPRODDIR}/"etss.t"${CYCLE}"z.stormsurge.con2p5km.grib2"
  if [ ${OFS} == "ciofs" -o ${OFS} == "CIOFS" ]; then
    GRB2FILE=${NCEPPRODDIR}/"etss.t"${CYCLE}"z.stormsurge.ala3km.grib2"
  fi


  INPUTTIME=$YYYY$MM$DD$CYCLE
  CURRENTTIME=$INPUTTIME
  while [ ! -s $GRB2FILE ]
  do
    if [ $envir == "dev" ]; then
      echo 'grib2 file is ' $GRB2FILE 'not found'
    fi
    CURRENTTIME=`$NDATE -1 $CURRENTTIME `
    if [ $CURRENTTIME -le ` $NDATE -60 $INPUTTIME ` ]
    then
     if [ $DBASE_WL == "ETSS" ]; then
       echo 'FATAL ERROR: no valid ETSS operational products is available for the given time period'
       msg="FATAL ERROR: no valid ETSS operational products is available for the given time period   "
       postmsg "$jlogfile" "$msg"
       postmsg "$nosjlogfile" "$msg"
       err=1;export err;err_chk
       touch err.${OFS}.$PDY1.t${HH}z
       exit
     else
       echo "ETSS is not primary forcing source of subtidal water level open boundary conditions"
       echo "$DBASE_WL is primary source for subtidal water level open boundary conditions"
     fi
    fi
    YYYY=`echo $CURRENTTIME | cut -c1-4 `
    MM=`echo $CURRENTTIME |cut -c5-6 `
    DD=`echo $CURRENTTIME |cut -c7-8 `
    CYCLE=`echo $CURRENTTIME |cut -c9-10 `
    NCEPPRODDIR=${COMINetss}'/etss.'$YYYY$MM$DD
    GRB2FILE=${NCEPPRODDIR}/"etss.t"${CYCLE}"z.stormsurge.con2p5km.grib2"
    if [ ${OFS} == "ciofs" ]; then
      GRB2FILE=${NCEPPRODDIR}/"etss.t"${CYCLE}"z.stormsurge.ala3km.grib2"
    fi

#   NCEPPRODDIR=${COMINetss}'/gfs.'$YYYY$MM$DD
#   GRB2FILE=${NCEPPRODDIR}/"mdlsurgegrid."${CYCLE}"con"
  done
  echo "first ETSS grib2 file : " $GRB2FILE " is found"
  echo "Time of the first available ETSS products is at cycle" $YYYY $MM $DD $CYCLE
  echo $GRB2FILE > ETSS_FILE
  echo $CURRENTTIME >> ETSS_FILE
  while [ $CURRENTTIME -le $TIME_NOW ]
  do 
    CURRENTTIME=`$NDATE +1 $CURRENTTIME `
    YYYY=`echo $CURRENTTIME | cut -c1-4 `
    MM=`echo $CURRENTTIME |cut -c5-6 `
    DD=`echo $CURRENTTIME |cut -c7-8 `
    CYCLE=`echo $CURRENTTIME |cut -c9-10 `
    NCEPPRODDIR=${COMINetss}'/etss.'$YYYY$MM$DD
    GRB2FILE=${NCEPPRODDIR}/"etss.t"${CYCLE}"z.stormsurge.con2p5km.grib2"
    if [ ${OFS} == "ciofs" ]; then
      GRB2FILE=${NCEPPRODDIR}/"etss.t"${CYCLE}"z.stormsurge.ala3km.grib2"
    fi

    if [ -s $GRB2FILE ]; then
      echo $GRB2FILE >> ETSS_FILE
      echo $CURRENTTIME >> ETSS_FILE
    fi  
  done  
  rm -f ${OFS}.ETSS
  if [ -s ETSS_FILE ]; then
   exec 5<&0 < ETSS_FILE
   read GRB2FILE1
   read CURRENTTIME1
   while read GRB2FILE2 
   do
     read CURRENTTIME2
     LENGTH=` $NHOUR $CURRENTTIME2 $CURRENTTIME1 `
     echo 'LENGTH= ' $LENGTH
     N=1
     while (( N <= 10#$LENGTH))
     do
	   rm -f tmp.csv
	   $WGRIB2 $GRB2FILE1 -d $N -undefine out-box ${MINLON}:${MAXLON} ${MINLAT}:${MAXLAT} -spread tmp.csv
	   export err=$?
	   if [ $err -ne 0 ]
	   then
	     echo "$WGRIB2 $GRB2FILE1 -d $N -undefine out-box ${MINLON}:${MAXLON} ${MINLAT}:${MAXLAT} -spread tmp.csv did not complete normally, FATAL ERROR!"
	     msg="$WGRIB2 $GRB2FILE1 -d $N -undefine out-box ${MINLON}:${MAXLON} ${MINLAT}:${MAXLAT} -spread tmp.csv did not complete normally, FATAL ERROR!"
	     postmsg "$nosjlogfile" "$msg"
	     err_chk
	   else
	     echo "$WGRIB2 $GRB2FILE1 -d $N -undefine out-box ${MINLON}:${MAXLON} ${MINLAT}:${MAXLAT} -spread tmp.csv completed normally!"
	     msg="$WGRIB2 $GRB2FILE1 -d $N -undefine out-box ${MINLON}:${MAXLON} ${MINLAT}:${MAXLAT} -spread tmp.csv  completed normally!"
	     postmsg "$nosjlogfile" "$msg"
	   fi
	   cat tmp.csv >> ${OFS}.ETSS
	   (( N = N + 1 ))
     done
     CURRENTTIME1=$CURRENTTIME2
     GRB2FILE1=$GRB2FILE2 
   done 3<&-  

   if [ -s cmdfile ]; then rm cmdfile; fi

   N=1
   while (( N <= 97 ))
   do
     rm -f tmp${N}.csv
     echo "$WGRIB2 $GRB2FILE1 -d $N -undefine out-box ${MINLON}:${MAXLON} ${MINLAT}:${MAXLAT} -spread tmp${N}.csv" >>cmdfile
     (( N++ ))
   done

   mpirun cfp cmdfile
   export err=$?; err_chk

   N=1
   while (( N <= 97 ))
   do
     cat tmp${N}.csv >> ${OFS}.ETSS
     (( N++ ))
   done
  fi
fi
# --------------------------------------------------------------------------
#     End of decode ETSS grib2 forecast products
# --------------------------------------------------------------------------


#     begin to generate OBC forcing file
# --------------------------------------------------------------------------
if [ $DBASE_TS == "WOA05" ]; then
    DBASE_WL="ETSS"
fi
if [ -s Fortran_OBC.ctl ]; then
   rm -f Fortran_OBC.ctl Fortran_OBC.log
fi 
if [ "${OFS,,}" != "creofs" ]; then
   YYYY=`echo $TIME_START | cut -c1-4 `
     MM=`echo $TIME_START | cut -c5-6 `
     DD=`echo $TIME_START | cut -c7-8 `
     HH=`echo $TIME_START | cut -c9-10 `
   OBC_FORCING_FILE_LAST=${PREFIXNOS}.obc.${YYYY}${MM}${DD}.t${HH}z.nc
   if [ "${OFS,,}" == "wcofs_da" ]; then
      TIME_LC=`$NDATE +48 $TIME_START`
      YYYY=`echo $TIME_LC | cut -c1-4 `
      MM=`echo $TIME_LC | cut -c5-6 `
      DD=`echo $TIME_LC | cut -c7-8 `
      HH=`echo $TIME_LC | cut -c9-10 `
      OBC_FORCING_FILE_LAST=${PREFIXNOS}.obc.${YYYY}${MM}${DD}.t${HH}z.nc
   fi
   if [ -s ${COMOUTroot}/${OFS}.$YYYY$MM$DD/$OBC_FORCING_FILE_LAST ]; then
      cp -p ${COMOUTroot}/${OFS}.$YYYY$MM$DD/$OBC_FORCING_FILE_LAST $DATA/.
   fi  
else
   OBC_FORCING_FILE_LAST=${PREFIXNOS}.obc.${YYYY}${MM}${DD}.t${HH}z.tar
fi    
COMTMP=${COMTMP:-FakeOBSDIR}
BIO_MODULE=${BIO_MODULE:-0}
echo ${OFS} > Fortran_OBC.ctl
echo $OCEAN_MODEL >> Fortran_OBC.ctl
echo $DBASE_WL  >> Fortran_OBC.ctl
echo ${OFS}.ETSS  >> Fortran_OBC.ctl
echo $DBASE_TS  >> Fortran_OBC.ctl
echo $COMTMP  >> Fortran_OBC.ctl
echo $DCOMINports  >> Fortran_OBC.ctl
echo $NOSBUFR  >> Fortran_OBC.ctl
echo $USGSBUFR  >> Fortran_OBC.ctl
echo ${TIME_START}00 >> Fortran_OBC.ctl
echo ${TIME_END}00 >> Fortran_OBC.ctl
echo $IGRD_OBC >> Fortran_OBC.ctl
echo $FIXofs >> Fortran_OBC.ctl
if [ $OCEAN_MODEL == "SELFE" -o $OCEAN_MODEL == "selfe" ]; then
    echo $GRIDFILE_LL >> Fortran_OBC.ctl
else
    echo $GRIDFILE >> Fortran_OBC.ctl
fi  
if [ -z  $HC_FILE_OFS ];then  # if HC_FILE_OFS is not defined or empty
    echo 'fake_HC_FILE_OFS' >> Fortran_OBC.ctl
else
    echo $HC_FILE_OFS >> Fortran_OBC.ctl
fi
echo $OBC_CTL_FILE >> Fortran_OBC.ctl
echo $OBC_CLIM_FILE >> Fortran_OBC.ctl
if [ "${OFS,,}" == "leofs" -o "${OFS,,}" == "lmhofs" ]; then
     echo $CORRECTION_STATION_CTL >> Fortran_OBC.ctl
     echo $WL_OFFSET_OLD >> Fortran_OBC.ctl
fi
echo $OBC_FORCING_FILE >> Fortran_OBC.ctl
echo $cormslogfile >> Fortran_OBC.ctl 
echo $BASE_DATE >> Fortran_OBC.ctl 
echo $MINLON  >> Fortran_OBC.ctl
echo $MINLAT >> Fortran_OBC.ctl 
echo $MAXLON >> Fortran_OBC.ctl 
echo $MAXLAT >> Fortran_OBC.ctl 
echo $KBm  >> Fortran_OBC.ctl
if [ $OCEAN_MODEL == "ROMS" -o $OCEAN_MODEL == "roms" ];  then
     echo $THETA_S >> Fortran_OBC.ctl 
     echo $THETA_B >> Fortran_OBC.ctl 
     echo $TCLINE >> Fortran_OBC.ctl 
     echo $NVTRANS  >> Fortran_OBC.ctl
     echo $NVSTR  >> Fortran_OBC.ctl
     echo ${BIO_MODULE} >> Fortran_OBC.ctl 
else    
     echo $VGRID_CTL >> Fortran_OBC.ctl
fi    
if [ $OCEAN_MODEL == "SELFE" -o $OCEAN_MODEL == "selfe" ]; then
    echo $Nudging_weight >> Fortran_OBC.ctl
    echo $STEP_NU_VALUE >> Fortran_OBC.ctl
fi  
echo ${COMOUTroot} >> Fortran_OBC.ctl
echo $OBC_FORCING_FILE_LAST  >> Fortran_OBC.ctl 
echo ${PREFIXNOS} >> Fortran_OBC.ctl
export pgm=nos_ofs_create_forcing_obc
. prep_step
startmsg
if [ $OCEAN_MODEL == "ROMS" -o $OCEAN_MODEL == "roms" ]; then
    $EXECnos/nos_ofs_create_forcing_obc < Fortran_OBC.ctl > Fortran_OBC.log
    export err=$?
elif [ $OCEAN_MODEL == "SELFE" -o $OCEAN_MODEL == "selfe" ]; then
    $EXECnos/nos_ofs_create_forcing_obc_selfe < Fortran_OBC.ctl > Fortran_OBC.log
    if [ -s temp_nu.in ]; then
      cp -p temp_nu.in $COMOUT/.
    else
       echo "OBC temp nudging file was not created"   
    fi  
    if [ -s salt_nu.in ]; then
      cp -p salt_nu.in $COMOUT/.
    else
       echo "OBC salt nudging file was not created"   
    fi  
    if [ -s elev3D.th ]; then
      cp -p elev3D.th $COMOUT/.
    else
       echo "WL OBC file was not created"   
    fi  
    export err=$?
    tar -cvf ${OBC_FORCING_FILE} elev3D.th temp_nu.in salt_nu.in
    
elif [ $OCEAN_MODEL == "FVCOM" -o $OCEAN_MODEL == "fvcom" ]; then
  if [  "${OFS,,}" == "leofs" -o "${OFS,,}" == "lmhofs" ];  then
      CURRENTTIME=$time_nowcastend
##  allow to search back for maximum 2 days
      CURRENTTIMEm5=`$NDATE -48 $CURRENTTIME `
      YYYY=`echo $CURRENTTIME | cut -c1-4 `
        MM=`echo $CURRENTTIME |cut -c5-6 `
        DD=`echo $CURRENTTIME |cut -c7-8 `
        HH=`echo $CURRENTTIME |cut -c9-10 `
      OLDFILE=$COMOUTroot/${OFS}.$YYYY$MM$DD/$WL_OFFSET_OLD
      FILESAVE=$COMOUT/$WL_OFFSET_OLD.$YYYY$MM$DD.t${cyc}z
      while [ ! -s $OLDFILE -a $CURRENTTIME -gt $CURRENTTIMEm5 ]
      do
         CURRENTTIME=`$NDATE -1 $CURRENTTIME `
         YYYY=`echo $CURRENTTIME | cut -c1-4 `
           MM=`echo $CURRENTTIME |cut -c5-6 `
           DD=`echo $CURRENTTIME |cut -c7-8 `
           HH=`echo $CURRENTTIME |cut -c9-10 `
         OLDFILE=$COMOUTroot/${OFS}.$YYYY$MM$DD/$WL_OFFSET_OLD
      done
      if [ -s $OLDFILE ]; then
        cp -p $OLDFILE $DATA
      fi
      if [ ! -s $CORRECTION_STATION_CTL ]; then
        cp -p $FIXofs/$CORRECTION_STATION_CTL $DATA
      fi
      $EXECnos/nos_ofs_create_forcing_obc_fvcom_gl < Fortran_OBC.ctl > Fortran_OBC.log
      cp -p ${PREFIXNOS}.obc.avg-wl* $COMOUT/.
      cp -p $WL_OFFSET_OLD $COMOUT/.
      cp -p $WL_OFFSET_OLD $FILESAVE
  else
      $EXECnos/nos_ofs_create_forcing_obc_fvcom < Fortran_OBC.ctl > Fortran_OBC.log
  fi
  export err=$?
fi
## output corms flag information
if grep "COMPLETED SUCCESSFULLY" Fortran_OBC.log /dev/null 2>&1
then
     echo "OBC FORCING COMPLETED SUCCESSFULLY 100" >> $cormslogfile
     echo "OBC_FORCING DONE 100 " >> $cormslogfile
else
     echo "OBC FORCING COMPLETED SUCCESSFULLY 0" >> $cormslogfile
     echo "OBC_FORCING DONE 0 " >> $cormslogfile
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
if [ -f "$OBC_FORCING_FILE" ]; then
    cp $OBC_FORCING_FILE $COMOUT/$OBC_FORCING_FILE
    if [ $SENDDBN = YES ]; then
      $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/$OBC_FORCING_FILE
    fi
else
    echo "NO $OBC_FORCING_FILE File Found"
fi
#if [ -f $COMOUT/$OBC_FORCING_FILE_EL ]; then
#    if [ $SENDDBN = YES ]; then
#      $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/$OBC_FORCING_FILE_EL
#    fi
#fi
#if [ -f $COMOUT/$OBC_FORCING_FILE_TS ]  
#then
#    if [ $SENDDBN = YES ]; then
#      $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/$OBC_FORCING_FILE_TS
#    fi
#fi

if [ -f "$HC_FILE_OFS" ]
then
    cp $HC_FILE_OFS $COMOUT/$HC_FILE_OFS 
#    if [ $SENDDBN = YES ]; then
#      $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/$HC_FILE_OFS
#    fi
else
    echo "NO $HC_FILE_OFS File Found"
fi

if [ -f Fortran_OBC.log ]
then
    cp Fortran_OBC.log $COMOUT/Fortran_OBC.t${cyc}z.log
else
    echo "NO Fortran_OBC.log Found"
fi

if [ -f Fortran_Tide.log ]
then
    cp Fortran_Tide.log $COMOUT/Fortran_Tide.t${cyc}z.log
else
    echo "NO Fortran_Tide.log Found"
fi

if [ -f ${USGSBUFR} ]; then
    cp -p ${USGSBUFR}  $COMOUT/${USGSBUFR}.t${cyc}z
fi
if [ -f ${NOSBUFR} ]; then
    cp -p ${NOSBUFR}  $COMOUT/${NOSBUFR}.t${cyc}z
fi

if [ "${OFS,,}" == "sscofs" ]; then
    cp -p $OBC_FORCING_FILE ${OBC_FORCING_FILE}.ori
    echo $OBC_FORCING_FILE >> obc.file.name.python
    python $USHnos/pysh/nos_ofs_obc_cut.py
    cp $OBC_FORCING_FILE $COMOUT/$OBC_FORCING_FILE
fi 


echo 'The script nos_ofs_create_forcing_obc.sh ended at UTC' `date`

