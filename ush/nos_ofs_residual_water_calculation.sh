#!/bin/sh
# Script Name:  nos_ofs_create_wl_residual.sh
#  Purpose:                                                                   #
#  This script uses NCO command to caculate the modeled average water level   #
#  from restart file and compared the result with the measured average water 
#  level, which is calculated from fortran executable
#  nos_ofs_residual_water_calculation.  The water level difference will be 
#  spread into EVP.${DBASE} or PRATE.${DBASE}, which is equivalent to  
#  the "evap" or "precip" in the met netcdf file. 
#
#                                                                             #
#  Child scripts :                                                            #
#                                                                             #
#  The utililty script used:                                                  #
#                                                                             #
# Remarks :                                                                   #
# - For non-fatal errors output is written to the *.log file.                 #
#                                                                             #
# Language:  C shell script
# Input:
#   $RST_FILE (like nos.$OFS.rst.nowcast.$yyyy$mm$dd.t${cyc}z.nc

# Output:
#   ${OFS}_residual.dat (the modeled average water level)
#   PRATE.${DBASE} (nos_ofs_create_forcing_met.sh will change it to "precip")
#   EVP.${DBASE}  (nos_ofs_create_forcing_met.sh will change it to "evap")
#

#
# Technical Contact:    Aijun Zhang         Org:  NOS/CO-OPS                  #
#                       Phone: (240) 533-0591	                              #
#                       E-Mail: aijun.zhang@noaa.gov                          #



set -x
echo "start nos_ofs_create_wl_residual.sh at time:" `date`
RUNTYPE=$1
echo $RUNTYPE ${DBASE}
ls -al *.${DBASE}
if [ -s PRATE.${DBASE} ]; then
   cp PRATE.${DBASE} EVP.${DBASE}
elif [ -s TMP.${DBASE} ]; then
   cp TMP.${DBASE} PRATE.${DBASE}
   cp TMP.${DBASE} EVP.${DBASE}
fi   
if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]; then
   if [ -s  $FIXofs/$RESIDUAL_CTL ]; then	
     cp -p $FIXofs/$RESIDUAL_CTL $DATA
   fi
   if [ -s  $FIXofs/$RESIDUAL_CANADA_CTL ]; then
     cp -p $FIXofs/$RESIDUAL_CANADA_CTL $DATA
   fi    	 
     ncwa -x -v node -a node  $RST_FILE lake_average_over_node.nc
     model_average_zeta=$(ncdump -v zeta lake_average_over_node.nc | grep 'zeta =' | cut -f2- -d=   | awk '{print $1}')
     if [ -s ${PREFIXNOS}.wl.calculation.ctl ]; then
        rm ${PREFIXNOS}.wl.calculation.ctl
     fi
     echo $time_nowcastend >> ${PREFIXNOS}.wl.calculation.ctl   ### $START_TIME
     echo $DCOMINports >> ${PREFIXNOS}.wl.calculation.ctl   ### $NOSWLDIR
     echo $NOSBUFR  >>  ${PREFIXNOS}.wl.calculation.ctl   ### $NOSBUFR
     echo ${COMOUTroot} >>  ${PREFIXNOS}.wl.calculation.ctl   ### COMOUT00
     echo $RESIDUAL_CTL >>  ${PREFIXNOS}.wl.calculation.ctl 
     echo 'nos_'$OFS'_residual.dat' >>  ${PREFIXNOS}.wl.calculation.ctl  ###  this is the output file
     echo $model_average_zeta  >>  ${PREFIXNOS}.wl.calculation.ctl
     $EXECnos/nos_ofs_residual_water_calculation < ${PREFIXNOS}.wl.calculation.ctl > ${PREFIXNOS}.wl.calculation.log
     if [ -s nos_${OFS}_residual.dat ]; then
       export residual=$( < nos_${OFS}_residual.dat )
       cp nos_${OFS}_residual.dat nos_${OFS}_residual_nowcast.dat
       precip=$residual
     else
       msg="FATAL ERROR: nos_${OFS}_residual.dat doesn not exist for Nowcast"
       postmsg "$jlogfile" "$msg"
       postmsg "$nosjlogfile" "$msg"
       setoff
       echo ' '
       echo '******************************************************'
       echo '*** FATAL ERROR : nos_${OFS}_residual.dat is not found for Nowcast '
       echo 'ERRORS from execution of  nos_ofs_residual_water_calculation.sh '
       echo '******************************************************'
       echo ' '
       echo $msg
       seton
       err_exit "FATAL ERROR: nos_${OFS}_residual.dat does not exist for Nowcast"       
     fi     
elif [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]; then
     echo 0.0 > nos_${OFS}_residual.dat
     cp nos_${OFS}_residual.dat nos_${OFS}_residual_forecast.dat
     precip=0.0
fi

if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]; then
     read precip < nos_${OFS}_residual.dat
 		if [ "$precip" \> 0 ]
		then

			awk -F','  -v OFS=','   '{
			if ($1 ~ /lon/)
  				{ print $0 }
			else
 			{ print $1,$2,'$precip'}
			}' PRATE.${DBASE} >  PRATE.${DBASE}.GOOD


			awk -F','  -v OFS=','   '{
			if ($1 ~ /lon/)
 			 { print $0 }
			else
			 { print $1,$2,'0'}
			}' EVP.${DBASE} >  EVP.${DBASE}.GOOD

		else
			awk -F','  -v OFS=','   '{
			if ($1 ~ /lon/)
  				{ print $0 }
			else
 			{ print $1,$2,'0'}
			}' PRATE.${DBASE} >  PRATE.${DBASE}.GOOD


			awk -F','  -v OFS=','   '{
			if ($1 ~ /lon/)
 			 { print $0 }
			else
			 { print $1,$2,'$precip'}
			}' EVP.${DBASE} >  EVP.${DBASE}.GOOD
		fi  ### if "$precip" \> 0

                cp PRATE.${DBASE}.GOOD PRATE.${DBASE}
                cp EVP.${DBASE}.GOOD EVP.${DBASE}
elif [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]; then
		awk -F','  -v OFS=','   '{
		if ($1 ~ /lon/)
  		{ print $0 }
		else
 		{ print $1,$2,'0'}
		}' PRATE.${DBASE} >  PRATE.${DBASE}.GOOD


		awk -F','  -v OFS=','   '{
		if ($1 ~ /lon/)
  		{ print $0 }
		else
 		{ print $1,$2,'0'}
		}' EVP.${DBASE} >  EVP.${DBASE}.GOOD


        	cp PRATE.${DBASE}.GOOD PRATE.${DBASE}
        	cp EVP.${DBASE}.GOOD EVP.${DBASE}
fi  ### if nowcast or forecast

