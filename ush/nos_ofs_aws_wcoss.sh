#!/bin/sh
#  Script Name:  nos_ofs_aws_wcoss.sh
#  Purpose:                                                                   #
#  This script is to copy model files to corresonding directories after       #
#  successfully completing nowcast and forecast simulations and tar the       #
#  files for uploading to cloud                                               #
#  Technical Contact:   Aijun Zhang         Org:  NOS/CO-OPS                  #
#                       Phone: 240-533-0591                                   #
#                       E-Mail: aijun.zhang@noaa.gov                          #
#                                                                             #
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
#  Control Files For Model Run
if [ -s ${FIXofs}/${PREFIXNOS}.ctl ]
then
  . ${FIXofs}/${PREFIXNOS}.ctl
else
  echo "${RUN} control file is not found"
  echo "please provide  ${RUN} control file of ${PREFIXNOS}.ctl in ${FIXofs}"
  msg="${RUN} control file is not found"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
  echo "${RUN} control file is not found"  >> $cormslogfile
  err_chk
fi
set -xa
echo ' '
echo '  		    ****************************************'
echo '  		    *** NOS OFS AWS SCRIPT  ***        '
echo '  		    ****************************************'
echo ' '
echo "Starting nos_ofs_aws_wcoss.sh at : `date`"
cycle=t${cyc}z
###############################################################################

export OBC_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY}.obc.nc
export OBC_FORCING_FILE_EL=${PREFIXNOS}.${cycle}.${PDY}.obc.el.nc
export OBC_FORCING_FILE_TS=${PREFIXNOS}.${cycle}.${PDY}.obc.ts.nc
export RIVER_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY}.river.nc
export INI_FILE_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.init.nowcast.nc
export HIS_OUT_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.fields.nowcast.nc
export AVG_OUT_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.avg.nowcast.nc
export MOD_4DVAR=${PREFIXNOS}.${cycle}.${PDY}.mod.nc
export STA_OUT_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.stations.nowcast.nc
export RST_OUT_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.rst.nowcast.nc
export MET_NETCDF_1_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.met.nowcast.nc
export MET_NETCDF_2_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.hflux.nowcast.nc
export OBC_TIDALFORCING_FILE=${PREFIXNOS}.${cycle}.${PDY}.roms.tides.nc
export OBC_FORCING_FILE_NWGOFS_NOW=${PREFIXNOS}.${cycle}.${PDY}.nestnode.nwgofs.nowcast.nc
export OBC_FORCING_FILE_NEGOFS_NOW=${PREFIXNOS}.${cycle}.${PDY}.nestnode.negofs.nowcast.nc
export OBC_FORCING_FILE_NWGOFS_FOR=${PREFIXNOS}.${cycle}.${PDY}.nestnode.nwgofs.forecast.nc
export OBC_FORCING_FILE_NEGOFS_FOR=${PREFIXNOS}.${cycle}.${PDY}.nestnode.negofs.forecast.nc
export OBS_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY}.obs.nc

#export INI_FILE_FORECAST=$RST_OUT_NOWCAST
export HIS_OUT_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.fields.forecast.nc
export STA_OUT_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.stations.forecast.nc
export AVG_OUT_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.avg.forecast.nc
export RST_OUT_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.rst.forecast.nc
export MET_NETCDF_1_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.met.forecast.nc
export MET_NETCDF_2_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.hflux.forecast.nc
export MODEL_LOG_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.nowcast.log
export MODEL_LOG_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.forecast.log
export RUNTIME_CTL_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.nowcast.in
export RUNTIME_CTL_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.forecast.in
export NUDG_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY}.clim.nc
if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
then
  export MET_NETCDF_1_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.met.nowcast.nc.tar
  export MET_NETCDF_1_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.met.forecast.nc.tar
  export OBC_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY}.obc.tar
  export OBC_FORCING_FILE_EL=${PREFIXNOS}.${cycle}.${PDY}.obc.el.tar
  export OBC_FORCING_FILE_TS=${PREFIXNOS}.${cycle}.${PDY}.obc.ts.tar
  export RIVER_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY}.river.th.tar
  export INI_FILE_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.init.nowcast.bin
  export RST_OUT_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.rst.nowcast.bin
  export RST_OUT_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.rst.forecast.bin
#  export INI_FILE_FORECAST=$RST_OUT_NOWCAST
  export RUNTIME_MET_CTL_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.met_ctl.nowcast.in
  export RUNTIME_MET_CTL_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.met_ctl.forecast.in
  export RUNTIME_COMBINE_RST_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.combine.hotstart.nowcast.in
  export RUNTIME_COMBINE_NETCDF_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.combine.netcdf.nowcast.in
  export RUNTIME_COMBINE_NETCDF_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.combine.netcdf.forecast.in
  export RUNTIME_COMBINE_NETCDF_STA_NOWCAST=${PREFIXNOS}.${cycle}.${PDY}.combine.netcdf.sta.nowcast.in
  export RUNTIME_COMBINE_NETCDF_STA_FORECAST=${PREFIXNOS}.${cycle}.${PDY}.combine.netcdf.sta.forecast.in
elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
then
  export RIVER_FORCING_FILE=${PREFIXNOS}.${cycle}.${PDY}.river.nc.tar
fi
cd $COMOUT

STATUS_FILE=${RUN}.status
if [ $envir == "prod" -o  $envir == "para" ]; then
  STATUS_FILE=${RUN}.status_${cyc}
fi 
if [ -s ${RUN}.status_${cyc} ]; then
  STATUS_FILE=${RUN}.status_${cyc}
else
  STATUS_FILE=${RUN}.status
fi
# this should not be needed if script is run as the step following nowcast/forecast
Len_waitingtime=3600  # Maximum waiting time in seconds
waitingtime=0
until [ -s $STATUS_FILE ]
do
     echo "File $STATUS_FILE not found, waiting ..."
     echo "script $0 is sleeping at time : " ` date `
     sleep 300
     waitingtime=`expr $waitingtime + 300`
     if [ $waitingtime -ge $Len_waitingtime ]
     then
         echo "waiting time exceeds $Len_waitingtime seconds"
         echo "system exit"
         exit
     fi
done

if [ -s  $STATUS_FILE ]
then
   currenttime="${PDY}${cyc}"
   read CTIME < $STATUS_FILE
   until [ $CTIME == $currenttime ]
   do
     echo "time in $STATUS_FILE is not current cycle, waiting ..."
     echo "script $0 is sleeping at time : " ` date `
     sleep 300
     waitingtime=`expr $waitingtime + 300`
     if [ $waitingtime -ge $Len_waitingtime ]
     then
         echo "waiting time exceeds $Len_waitingtime seconds"
         echo "system exit"
         exit
     fi
     read CTIME < $STATUS_FILE
   done
else
  echo "${recent}/$STATUS_FILE does not exist !!"
  rm -f ${recent}/*
  echo "NCEP_PUSH DONE 0" >> $corms_local/$CORMSLOG
  exit
fi
# 1  copy nowcast output 
# 1.1 Nowcast log 
if [ -f ${MODEL_LOG_NOWCAST} ]
then
  cp -p  ${MODEL_LOG_NOWCAST} $DATA
fi
# 1.2 STA nowcast
if [ -f $STA_OUT_NOWCAST ]
then
  cp -p $STA_OUT_NOWCAST $DATA
fi
# 1.3 HIS nowcast 2D (if any) and 3D fields
nfile_2d=`ls ${PREFIXNOS}.${cycle}.${PDY}.2ds.n*.nc |wc -l`
if [ $nfile_2d -ge 1 ]; then
 cp -p ${PREFIXNOS}.${cycle}.${PDY}.2ds.n*.nc $DATA
fi

cp -p ${PREFIXNOS}.${cycle}.${PDY}.fields.n*.nc  $DATA

if [ -f $AVG_OUT_NOWCAST ]
then
  cp -p $AVG_OUT_NOWCAST $DATA
fi
if [ -f $MOD_4DVAR ]
then
  cp -p $MOD_4DVAR $DATA
fi

# 1.4 RST nowcast
dday=${PDY:6:2} #extract day only
if [ $dday = '01' -o $dday = '11' -o  $dday = '21' ]; then
  if [ ${cyc} = "00" -o ${cyc} = "03" ]; then
    cp -p $INI_FILE_NOWCAST $DATA
  fi
fi
if [ $OFS = 'wcofs_da' ]; then
  cp -p $INI_FILE_NOWCAST $DATA
  cp -p ${INI_FILE_NOWCAST}.new $DATA
fi
# 1.5 OBC Forcing 
if [ -f $OBC_FORCING_FILE ]
then
  cp -p $OBC_FORCING_FILE  $DATA
fi
if [ -f $OBC_FORCING_FILE_EL ]
then
 cp -p $OBC_FORCING_FILE_EL $DATA
fi
if [ -f $OBC_FORCING_FILE_TS ]
then
  cp -p $OBC_FORCING_FILE_TS $DATA
fi
if [ -f $NUDG_FORCING_FILE ]; then
  cp -p $NUDG_FORCING_FILE $DATA
fi

#if [ -f $OBC_TIDALFORCING_FILE ]
#then
#  cp -p $OBC_TIDALFORCING_FILE  $DATA
#fi
# 1.6 River Forcing 
if [ -f $RIVER_FORCING_FILE ]
then
   cp -p $RIVER_FORCING_FILE $DATA
fi
# 1.7 Surface Forcing 
if [ -f $MET_NETCDF_1_NOWCAST ]
then
  cp -p $MET_NETCDF_1_NOWCAST $DATA
fi
# 1.8 Surface Forcing 2
#if [ -f $MET_NETCDF_2_NOWCAST ]
#then
#  cp -p $MET_NETCDF_2_NOWCAST $DATA
#fi
# 1.9 Model runtime control file for nowcast
if [ -f $RUNTIME_CTL_NOWCAST ]
then
  cp -p $RUNTIME_CTL_NOWCAST $DATA
fi

if [ -f $OBC_FORCING_FILE_NWGOFS_NOW ]; then
   cp -p $OBC_FORCING_FILE_NWGOFS_NOW $DATA
fi

if [ -f $OBC_FORCING_FILE_NEGOFS_NOW ]; then
   cp -p $OBC_FORCING_FILE_NEGOFS_NOW  $DATA
fi
if [ -f $OBC_FORCING_FILE_NWGOFS_FOR ]; then
   cp -p $OBC_FORCING_FILE_NWGOFS_FOR  $DATA
fi
if [ -f $OBC_FORCING_FILE_NEGOFS_FOR ]; then
   cp -p $OBC_FORCING_FILE_NEGOFS_FOR  $DATA
fi
if [ -f $OBS_FORCING_FILE ]; then
   cp -p $OBS_FORCING_FILE  $DATA
fi


# --------------------------------------------------------------------------- #
# 2  copy forecast output
# 2.1 forecast log 
if [ -f ${MODEL_LOG_FORECAST} ]
then
  cp -p ${MODEL_LOG_FORECAST}  $DATA 
fi
# 2.2 STA FORECAST
if [ -f $STA_OUT_FORECAST ]
then
  cp -p $STA_OUT_FORECAST $DATA
fi
# 2.3 HIS FORECAST (Only transfer 48-hour forecast (2D if exist or 3D) during development)
nfile=`ls ${PREFIXNOS}.${cycle}.${PDY}.2ds.f*.nc |wc -l`
if [ $nfile -ge 1 ]; then
 cp -p ${PREFIXNOS}.${cycle}.${PDY}.2ds.f*.nc $DATA
fi
#if [ $nfile -ge 1 ]; then
#  I=0
#  while (( I < 49))
#  do
#    fhr3=`echo $I |  awk '{printf("%03i",$1)}'`
#    fileout=${PREFIXNOS}.${cycle}.${PDY}.2ds.f${fhr3}.nc
#    if [ -s $fileout ]; then
#      cp -p ${fileout} $DATA
#    fi
#    (( I = I + 1 ))
#  done
#
#fi
#  I=0
#  while (( I < 49))
#  do
#    fhr3=`echo $I |  awk '{printf("%03i",$1)}'`
#    fileout=${PREFIXNOS}.${cycle}.${PDY}.fields.f${fhr3}.nc
#    if [ -s $fileout ]; then
#      cp -p ${fileout} $DATA
#    fi
#    (( I = I + 1 ))
#  done
cp -p ${PREFIXNOS}.${cycle}.${PDY}.fields.f*.nc  $DATA
if [ -f $AVG_OUT_FORECAST ]
then
  cp -p $AVG_OUT_FORECAST $DATA
fi

# 2.4 Surface Forcing 
if [ -f $MET_NETCDF_1_FORECAST ]
then
  cp -p $MET_NETCDF_1_FORECAST $DATA
fi
# 2.5 Surface Forcing 2
#if [ -f $MET_NETCDF_2_FORECAST ]
#then
#  cp -p $MET_NETCDF_2_FORECAST  $DATA
#fi
# 2.6 Model runtime control file for FORECAST
if [ -f $RUNTIME_CTL_FORECAST ]
then
  cp -p $RUNTIME_CTL_FORECAST  $DATA
fi
# 2.7 CORMS FLAG file for forecast
if [ -f ${PREFIXNOS}.${cycle}.${PDY}.corms.log ]
then
  cp -p ${PREFIXNOS}.${cycle}.${PDY}.corms.log  $DATA
fi
# 2.8 jlog file for nowcast and forecast
if [ -f ${PREFIXNOS}.${cycle}.${PDY}.jlogfile.log ]
then
  cp -p ${PREFIXNOS}.${cycle}.${PDY}.jlogfile.log  $DATA
fi

if [ -f ${PREFIXNOS}.${cycle}.${PDY}.jlog.log ]
then
  cp -p ${PREFIXNOS}.${cycle}.${PDY}.jlog.log  $DATA
fi

# 2.9 OFS status file for nowcast and forecast
if [ -f $STATUS_FILE ]
then
  cp -p  $STATUS_FILE   $DATA
fi
STATUS_FILE=${RUN}.status
if [ -f $STATUS_FILE ]; then
  cp -p  $STATUS_FILE   $DATA
fi

## Tar folder to a tar file
cd $DATA
#tarfile=${OFS}.${PDY}${cyc}.${envir}.tar
tar -cf ${DATAROOT}/${tarfile} .
echo started uploading at `date`
#cd ~/s3test
#python s3_upload_file.py ${DATAROOT}/${tarfile} ${tarfile}
aws s3 cp ${DATAROOT}/${tarfile} s3://co-ops.nceptransfer/${tarfile}
export err=$?
if [ $err -ne 0 ]
then
  echo "File transfer to AWS did not complete normally"
  msg="File transfer to AWS did not complete normally"
 # postmsg "$jlogfile" "$msg"
  echo "AWS NOWCAST/FORECAST DONE 0" >> $cormslogfile
else
  echo "File transfer to AWS completed normally"
  msg="File transfer to AWS completed normally"
  #postmsg "$jlogfile" "$msg"
  echo "AWS NOWCAST/FORECAST DONE 100" >> $cormslogfile
fi
mv  ${DATAROOT}/${tarfile} $DATA/.
# --------------------------------------------------------------------------- #
# 4.  Ending output

  echo ' '
  echo "Ending nos_ofs_aws_wcoss.sh at : `date`"
  echo ' '
  echo '        *** End of NOS OFS AWS SCRIPT ***'
  echo ' '
