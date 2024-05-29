#!/bin/sh
#  Script Name:  nos_ofs_obs.sh
#  Purpose:                                                                   #
#  This script is to read and format observations for data assimilation       #
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
if [ -s ${FIXofs}/$GRIDFILE ]
then 
    cp ${FIXofs}/$GRIDFILE ./
fi
if [ -s ${FIXofs}/$MODELTIDE_HC ]
then
    cp ${FIXofs}/$MODELTIDE_HC ./
fi
#if [ -s ${FIXofs}/rtofsSeaLevelBias.txt ]
#then
#    cp ${FIXofs}/rtofsSeaLevelBias.txt ${COMOUT}/
#fi
set -xa
echo ' '
echo '  		    ****************************************'
echo '  		    *** NOS OFS OBS SCRIPT  ***        '
echo '  		    ****************************************'
echo ' '
echo "Starting nos_ofs_obs.sh at : `date`"
###############################################################################
time_nowcastend=${PDY}${cyc}
time_hotstart=`$NDATE -$LEN_DA $time_nowcastend`

# 0 prepare model tide file (apply node factors and equilirium arguments)
YYYY=`echo $time_nowcastend | cut -c1-4`
echo $MODELTIDE_HC > Fortran_Modeltide.ctl
echo $MODELTIDE >> Fortran_Modeltide.ctl
echo $YYYY >> Fortran_Modeltide.ctl
echo 1 >> Fortran_Modeltide.ctl

$EXECnos/nos_ofs_adjust_tides < Fortran_Modeltide.ctl  >> Fortran_Modeltide.log
export err=$?
pgm=nos_ofs_adjust_tides

if [ $err -ne 0 ]
then
    msg="$pgm did not complete normally, FATAL ERROR!"
    echo $msg
    echo $msg >> $nosjlogfile
    err_chk
else
    msg="$pgm completed normally"
    echo $msg
    echo $msg >> $nosjlogfile
fi
## run python scripts to process observations
cd $PYnos
# 1 SST Data
CURRENTDATE=$time_hotstart
while [ $CURRENTDATE -le $time_nowcastend ]
do
    YYYYMMDD=`echo $CURRENTDATE | cut -c1-8`
    python -u sst_cutout_multiSat.py $YYYYMMDD
    export err=$?
    pgm=sst_cutout_multiSat.py
    if [ $err -ne 0 ]; then
       msg="FATAL ERROR : $pgm did not complete normally!, python module may not be loaded "
       echo $msg
       echo $msg >> $nosjlogfile
       err_chk
    fi
    python -u timeave_granules.py $YYYYMMDD
    export err=$?
    pgm=timeave_granules.py
    if [ $err -ne 0 ]; then
       msg="FATAL ERROR : $pgm did not complete normally!, python module may not be loaded "
       echo $msg
       echo $msg >> $nosjlogfile
       err_chk
    fi

    CURRENTDATE=`$NDATE 24 $CURRENTDATE`
done
python -u d_sst_multiSat.py $PDY 2>>$nosjlogfile
export err=$?
pgm=d_sst_multiSat.py
if [ $err -ne 0 ]
then
    msg="FATAL ERROR : $pgm did not complete normally!, python module may not be loaded "
    echo $msg
    echo $msg >> $nosjlogfile
    err_chk
fi
SST_OBS=sst_super_obs_${PDY}.t${cyc}z.nc
#SST_OBS=t{cyc}z.${PDY}_sst_super_obs.nc
if [ -s ${DATA}/ObsFiles/$SST_OBS ]
then
  cp -p ${DATA}/ObsFiles/$SST_OBS ${COMOUT}
  echo "$SST_OBS file generated successfully"
  echo "SST OBS FILE $SST_OBS COMPLETED SUCCESSFULLY 100" >> $cormslogfile
  nobs=`ncdump -h ${DATA}/ObsFiles/$SST_OBS |grep 'datum =' |awk '{print $3}'`
  echo "Number of SST DATA POINTS: $nobs" 
  echo "SST_OBS DONE $nobs" >> $cormslogfile
else
  echo "WARNING: $SST_OBS file was not generated!"
  echo "SST_OBS DONE 0" >> $cormslogfile
fi
# --------------------------------------------------------------------------- #
# 2 HF Radar Data 
python -u d_hf.py $PDY 2>>$nosjlogfile
export err=$?
pgm=d_hf.py
if [ $err -ne 0 ]
then
    msg="FATAL ERROR: $pgm did not complete normally!, python module may not be loaded"
    echo $msg
    echo $msg >> $nosjlogfile
    err_chk
fi
HF_OBS=hf_obs_${PDY}.t${cyc}z.nc
#HF_OBS=t${cyc}z.${PDY}_hf_obs.nc

if [ -s ${DATA}/ObsFiles/$HF_OBS ]
then
  cp -p ${DATA}/ObsFiles/$HF_OBS ${COMOUT}
  echo "$HF_OBS file generated successfully"
  echo "HFR OBS FILE $HF_OBS COMPLETED SUCCESSFULLY 100" >> $cormslogfile
  nobs=`ncdump -h ${DATA}/ObsFiles/$HF_OBS |grep 'datum =' |awk '{print $3}'`
  echo "Number of HFR DATA POINTS: $nobs"
  echo "HFR_OBS DONE $nobs" >> $cormslogfile

else
  echo "WARNING: $HF_OBS file was not generated!"
#  echo "GET FILE FROM CO-OPS AWS"
#  cd /u/${LOGNAME}/s3test
#  python -u s3_download_file.py $HF_OBS ${DATA}/ObsFiles/$HF_OBS
  if [ -s ${DATA}/ObsFiles/$HF_OBS ]
  then
    cp -p ${DATA}/ObsFiles/$HF_OBS ${COMOUT}
    echo "$HF_OBS file downloaded successfully"
    echo "HFR_OBS DONE 100" >> $cormslogfile
  else
    echo "WARNING: $HF_OBS file was not downloaded!"
    echo "HFR_OBS DONE 0" >> $cormslogfile
  fi
fi
# --------------------------------------------------------------------------- #
# 3 Satellite Altimetry Data
cd $PYnos
python -u d_ssh.py $PDY 2>>$nosjlogfile
export err=$?
pgm=d_ssh.py
if [ $err -ne 0 ]
then
    msg="FATAL ERROR: $pgm did not complete normally!, python module may not be loaded properly"
    echo $msg
    echo $msg >> $nosjlogfile
#    err_chk
fi
##archive msl bias under fix for now; won't need it in operation
#if [ -s ${COMOUT}/rtofsSeaLevelBias.txt ]
#then
#    cp -up ${COMOUT}/rtofsSeaLevelBias.txt ${FIXofs}/
#fi
SSH_OBS=adt_obs_${PDY}.t${cyc}z.nc
#SSH_OBS=t${cyc}z.${PDY}_adt_obs.nc

if [ -s ${DATA}/ObsFiles/$SSH_OBS ]
then
  cp -p ${DATA}/ObsFiles/$SSH_OBS ${COMOUT}
  echo "$SSH_OBS file generated successfully" 
  echo "SSH OBS FILE $SSH_OBS COMPLETED SUCCESSFULLY 100" >> $cormslogfile
  nobs=`ncdump -h ${DATA}/ObsFiles/$SSH_OBS |grep 'datum =' |awk '{print $3}'`
  echo "Number of SSH DATA POINTS: $nobs"
  echo "SSH_OBS DONE $nobs" >> $cormslogfile
else
  echo "WARNING: $SSH_OBS file was not generated!"
  echo "SSH_OBS DONE 0" >> $cormslogfile
# send email message to SPA and CO-OPS Modeling Team
  export subject="WARNING: NO SSH FILES for $PDY t${cyc}z $job"
  echo "*************************************************************" > ${DATA}/mailmsg
  echo "*** WARNING !! $SSH_OBS file was not generated! ***" >> ${DATA}/mailmsg
  echo "This means no satellite ADT data are assimilated into WCOFS_DA." >> ${DATA}/mailmsg
  echo "Please check job output and satellite ADT files from previous 3 days!" >> ${DATA}/mailmsg
  echo "Satellite ADT DCOM files: ${DCOMROOT}/$PDY/wgrdbul/adt/rads_adt*.nc" >> ${DATA}/mailmsg
  echo "For missing satellite data including SST and ADT,please email espcoperations@noaa.gov and  obsproc_support@noaa.gov " >> ${DATA}/mailmsg 
  cat ${DATA}/mailmsg > $COMOUT/${RUN}.t${cyc}z.ssh.emailbody
  cat $COMOUT/${RUN}.t${cyc}z.ssh.emailbody | mail.py -s "$subject" $maillist -v
fi
# --------------------------------------------------------------------------- #
# 4 Merge all observations
python -u merge_obs.py $PDY 2>>$nosjlogfile
export err=$?
pgm=merge_obs.py
if [ $err -ne 0 ]
then
    msg="FATAL ERROR: $pgm did not complete normally! python module may not be loaded properly"
    echo $msg
    echo $msg >> $nosjlogfile
    err_chk
fi
cd ${DATA}/ObsFiles
obsfile=${PREFIXNOS}.obs.${PDY}.t${cyc}z.nc
#obsfile=${PREFIXNOS}.t${cyc}z.${PDY}.obs.nc

if [ -f $obsfile ]
then
  cp -p $obsfile ${COMOUT}/${PREFIXNOS}.t${cyc}z.${PDY}.obs.nc  
  cp -p $obsfile ${COMOUT}/obs.nc
  echo "$obsfile file generated successfully"
  echo "OBS_FILE DONE 100" >> $cormslogfile
else
  echo "OBS_FILE DONE 0" >> $cormslogfile
  echo "FATAL ERROR: Observation file was not generated!"
fi

# --------------------------------------------------------------------------- #
# 5 Ending output

  echo ' '
  echo "Ending nos_ofs_obs.sh at : `date`"
  echo ' '
  echo '        *** End of NOS OFS OBS SCRIPT ***'
  echo ' '
