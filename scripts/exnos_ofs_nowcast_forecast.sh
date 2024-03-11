#!/bin/sh

# ############################################################################
#  Script Name:  exnos_ofs_nowcast_forecast.sh.sms 
#  Purpose:                                                                   #
#  This is the main script is launch both nowcast and forecast simulations    #
# Location:   ~/jobs
# Technical Contact:    Aijun Zhang         Org:  NOS/CO-OPS
#                       Phone: 301-7132890 ext. 127
#                       E-Mail: aijun.zhang@noaa.gov
#
# Usage: 
#
# Input Parameters:
#  OFS 
#
# Modification History:
#     Degui Cao     02/18/2010   
# ##########################################################################

set -x
#PS4=" \${SECONDS} \${0##*/} L\${LINENO} + "

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
. $USHnos/nos_ofs_launch.sh $OFS nowcast
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

#####     Run nowcast simulation
runtype='nowcast'
echo "     " >> $jlogfile 
echo "     " >> $nosjlogfile 
echo " Start $runtype " >> $jlogfile
echo " Start $runtype " >> $nosjlogfile
echo "Making $runtype at : `date`" >> $jlogfile
echo "Making $runtype at : `date`" >> $nosjlogfile
echo "Making $runtype at : `date`"
export pgm="$USHnos/nos_ofs_nowcast_forecast.sh $runtype"
$USHnos/nos_ofs_nowcast_forecast.sh $runtype 
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

###  archive nowcast outputs
export pgm="$USHnos/nos_ofs_archive.sh $runtype"
$USHnos/nos_ofs_archive.sh $runtype
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

# if [ $envir = "dev" ]; then
#   $USHnos/nos_ofs_sftp.sh $runtype
# fi
 echo "end of $runtype"

if [ $LEN_FORECAST -gt 0 ] 
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
echo "end of nos_ofs_nowcast_forecast.sh $runtype"

##  archive forecast outputs 
export pgm="$USHnos/nos_ofs_archive.sh $runtype"
$USHnos/nos_ofs_archive.sh $runtype 
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

# if [ $envir = "dev" ]; then
#  # for development copy outputs to CO-OPS via sftp push 
#   $USHnos/nos_ofs_sftp.sh $runtype
# fi
if [ $SENDDBN = YES ]; then
  $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $nosjlogfile
fi
fi
          echo "                                    "
          echo "END OF NOWCAST/FORECAST SUCCESSFULLY"
          echo "                                    "
###############################################################
