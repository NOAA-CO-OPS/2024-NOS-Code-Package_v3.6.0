#!/bin/sh
#  Script Name:  exnos_ofs_nowcast_forecast.sms.prod
#  Purpose:                                                                   #
#  This script is to make nowcast or forecast simulation after running:       #
#  nos_ofs_create_forcing_river.sh, nos_ofs_create_forcing_obc.sh             #
#  nos_ofs_create_forcing_met_nowcast.sh nos_ofs_create_forcing_met_forecast.sh
#  nos_ofs_reformat_roms_ctl_nowcast.sh nos_ofs_reformat_roms_ctl_forecast.sh 
#                                                                             #
#  Child scripts :                                                            #
#                                                                             #
#  The utililty script used:                                                  #
#                                                                             #
# Remarks :                                                                   #
# - For non-fatal errors output is written to the *.log file.                 #
# - NOTE TO NCO: this script is I/O limited. To get the script to run in      #
#                approximately 1:15, 1 node  and 12 processors are optimal    #
#                                                                             #
# Language:  C shell script
# Nowcast  
# Input:
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.river.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.obc.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.met.nowcast.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.init.nowcast.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.hflux.nowcast.nc
#     ${RUN}_roms_nowcast.in
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.roms.tides.nc
# Output:
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.stations.nowcast.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.fields.nowcast.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.fields.forecast.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.rst.nowcast.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.roms.nowcast.log
# Forecast  
# Input:
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.river.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.obc.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.met.forecast.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.rst.nowcast.nc
#     ${RUN}_roms_forecast.in
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.roms.tides.nc
# Output:
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.stations.forecast.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.fields.forecast.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.rst.forecast.nc
#     ${PREFIXNOS}.t${cyc}z.$yyyy$mm$dd.roms.forecast.log
#
# Technical Contact:    Aijun Zhang       Org:  NOS/CO-OPS                    #
#                       Phone: 240-533-0591                                   #
#                       E-Mail: aijun.zhang@noaa.gov                          #
# Modification History:
#        
#           
# 
#                                                                             #
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

function seton {
  set -x
}
function setoff {
  set +x
}
seton

cd $DATA

echo ' '
echo '  		    ****************************************'
echo '  		    *** NOS OFS  NOWCAST/FORECAST SCRIPT ***'
echo '  		    ****************************************'
echo ' '
echo "Starting nos_ofs_nowcast_forecast.sh at : `date`"


#export MP_PGMMODEL=mpmd
#export MP_CMDFILE=cmdfile

RUNTYPE=$1 
RUN=$OFS 
if [ -s $COMOUT/time_hotstart.${cycle} ]; then
  read time_hotstart < $COMOUT/time_hotstart.${cycle}
  export time_hotstart
else
  echo "time_hotstart is not defined yet"
  echo "FATAL ERROR "
  echo "Please define time_hotstart "
  exit
fi

if [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
then 
  NCSF_OUT_INTERVAL=${NCSF_OUT_INTERVAL:-$NC_OUT_INTERVAL}
  if [ -s ${FIXofs}/${PREFIXNOS}_brf.nc ]
  then
     cp -p ${FIXofs}/${PREFIXNOS}_brf.nc $DATA/${RUN}_brf.nc
  fi

  if [ -s ${FIXofs}/${PREFIXNOS}_cor.dat ]
  then
     cp -p ${FIXofs}/${PREFIXNOS}_cor.dat $DATA/${RUN}_cor.dat
  fi
  if [ -s ${FIXofs}/${PREFIXNOS}_dep.dat ]
  then
     cp -p ${FIXofs}/${PREFIXNOS}_dep.dat $DATA/${RUN}_dep.dat
  fi
  if [ -s ${FIXofs}/${PREFIXNOS}_grd.dat ]
  then
     cp -p ${FIXofs}/${PREFIXNOS}_grd.dat $DATA/${RUN}_grd.dat
  fi
  if [ -s ${FIXofs}/${PREFIXNOS}_obc.dat ]
  then
     cp -p ${FIXofs}/${PREFIXNOS}_obc.dat $DATA/${RUN}_obc.dat
  fi
  if [ -s ${FIXofs}/${PREFIXNOS}_sigma.dat ]
  then
     cp -p ${FIXofs}/${PREFIXNOS}_sigma.dat $DATA/${RUN}_sigma.dat
  fi
  if [ -s ${FIXofs}/${PREFIXNOS}_spg.dat ]
  then
     cp -p ${FIXofs}/${PREFIXNOS}_spg.dat $DATA/${RUN}_spg.dat
  fi
  if [ -s ${FIXofs}/${PREFIXNOS}_station.dat ]
  then
     cp -p ${FIXofs}/${PREFIXNOS}_station.dat $DATA/${RUN}_station.dat
  fi
  if [ -s ${FIXofs}/${PREFIXNOS}_rivernamelist.nml ]
  then
     cp -p ${FIXofs}/${PREFIXNOS}_rivernamelist.nml $DATA/RIVERS_NAMELIST.nml
  fi
  if [ "${OFS,,}" == "ngofs" ]; then
     if [ -s ${FIXofs}/nos_${RUN}_nestnode_negofs.dat ]; then
        cp -p ${FIXofs}/nos_${RUN}_nestnode_negofs.dat $DATA/nos_${RUN}_nestnode_negofs.dat
     fi
     if [ -s ${FIXofs}/nos_${RUN}_nestnode_nwgofs.dat ]; then
        cp -p ${FIXofs}/nos_${RUN}_nestnode_nwgofs.dat $DATA/nos_${RUN}_nestnode_nwgofs.dat
     fi

  fi   
  if [ -d ${FIXofs}/$STA_EDGE_CTL -o ! -s ${FIXofs}/$STA_EDGE_CTL ]; then
    echo "${FIXofs}/$STA_EDGE_CTL is not found"
    echo "$STA_EDGE_CTL will be created in FVCOM "
    echo "FVCOM will take long time to finish for large model domain"
    echo "$STA_EDGE_CTL will be copied into ${FIXofs} for future use " 
  elif [ -s ${FIXofs}/$STA_EDGE_CTL ]; then
    cp -p ${FIXofs}/$STA_EDGE_CTL $DATA/.
  fi


elif [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
then 
  if [ ! -d $DATA/outputs ] 
  then
     mkdir -p $DATA/outputs
  fi
  if [ ! -d $DATA/sflux ] 
  then
     mkdir -p $DATA/sflux
  fi
  if [ -s $COMOUT/time_nowcastend.${cycle} ]; then
    read time_nowcastend < $COMOUT/time_nowcastend.${cycle}
    export time_nowcastend
  fi
  if [ -s $COMOUT/time_hotstart.${cycle} ]; then
    read time_hotstart < $COMOUT/time_hotstart.${cycle}
    export time_hotstart
  fi
  if [ -s $COMOUT/time_forecastend.${cycle} ]; then
    read time_forecastend < $COMOUT/time_forecastend.${cycle}
    export time_forecastend
  fi
  if [ -s $COMOUT/base_date.${cycle} ]; then
    read BASE_DATE < $COMOUT/base_date.${cycle}
    export BASE_DATE
  fi
    
  export ynet=`echo $time_nowcastend |cut -c1-4`
  export mnet=`echo $time_nowcastend |cut -c5-6`
  export dnet=`echo $time_nowcastend |cut -c7-8`
  export hnet=`echo $time_nowcastend |cut -c9-10`
  export nnh=`$NHOUR $time_nowcastend $time_hotstart`
  export tsnh=`expr $nnh \* 3600 / ${DELT_MODEL%.*}`

 # export tsnh=$(echo "(($nnh * 3600) / 90)" | bc)
  export nh=$(echo "scale=4;$nnh / 24.0" | bc)
  export nhfr=$(echo "scale=4;$nh + 2.0" | bc)
  echo "number hours nowcast= $nnh"
  echo "number days nowcast= $nh"

  export yhst=`echo $time_hotstart |cut -c1-4`
  export mhst=`echo $time_hotstart |cut -c5-6`
  export dhst=`echo $time_hotstart |cut -c7-8`
  export hhst=`echo $time_hotstart |cut -c9-10`

#static files

# global_to_local.prop is required because ParMETIS is disabled due to license issue
# if TOTAL_TASKS change, a new global_to_local.prop file has to be provided 
  if [ ! -s  $FIXofs/global_to_local.prop ]; then   # bypass ParMetis
      echo " FATAL ERROR : $FIXofs/global_to_local.prop is not found "
      echo please provide $FIXofs/global_to_local.prop
      exit
  else
      max_cpunum=-999
      exec 5<&0 <  $FIXofs/'global_to_local.prop'
      while read eleid num
      do
#        echo 'eleid= '$eleid $num
         if [ $num -gt $max_cpunum ]; then
            max_cpunum=$num
         fi
      done 3<&-
      max_cpunum=`expr $max_cpunum + 1`
      echo ' max_cpunum= ' $max_cpunum 'TOTAL_TASKS= ' $TOTAL_TASKS
      if [ $max_cpunum = $TOTAL_TASKS ]; then
         cp -p $FIXofs/global_to_local.prop $DATA
      else
        echo "FATAL ERROR"          
        echo "total number of cpu in global_to_local.prop is not equal TOTAL_TASK " $max_cpunum  $TOTAL_TASKS
        echo " total number of cpu in global_to_local.prop has to be  equal to TOTAL_TASK "
        echo "Please provide a new $FIXofs/global_to_local.prop "
        exit
      fi
  fi

  if [ -s $FIXofs/${PREFIXNOS}.albedo.gr3 ]
  then
  cp -p $FIXofs/${PREFIXNOS}.albedo.gr3 $DATA/albedo.gr3 
  fi

  if [ -s $FIXofs/${PREFIXNOS}.drag.gr3 ]
  then
  cp -p $FIXofs/${PREFIXNOS}.drag.gr3 $DATA/drag.gr3 
  fi

  if [ -s $FIXofs/${PREFIXNOS}.hgrid.ll ]
  then
  cp -p $FIXofs/${PREFIXNOS}.hgrid.ll $DATA/hgrid.ll
  fi

  if [ -s $FIXofs/${STA_OUT_CTL} ]
  then
  cp -p $FIXofs/${STA_OUT_CTL} $DATA/station.in 
#  cp -p $FIXofs/${PREFIXNOS}.station.in $DATA/station.in 
  fi

  if [ -s $FIXofs/${PREFIXNOS}.watertype.gr3 ]
  then
  cp -p $FIXofs/${PREFIXNOS}.watertype.gr3 $DATA/watertype.gr3 
  fi

  if [ -s $FIXofs/${PREFIXNOS}.diffmax.gr3 ]
  then
  cp -p $FIXofs/${PREFIXNOS}.diffmax.gr3  $DATA/diffmax.gr3 
  fi

  if [ -s $FIXofs/${PREFIXNOS}.estuary.gr3 ]
  then
  cp -p $FIXofs/${PREFIXNOS}.estuary.gr3 $DATA/estuary.gr3 
  fi

  if [ -s $FIXofs/${PREFIXNOS}.interpol.gr3 ]
  then
  cp -p $FIXofs/${PREFIXNOS}.interpol.gr3 $DATA/interpol.gr3 
  fi

  if [ -s $FIXofs/${PREFIXNOS}.t_nudge.gr3 ]
  then
  cp -p $FIXofs/${PREFIXNOS}.t_nudge.gr3 $DATA/t_nudge.gr3 
  fi

  if [ -s $FIXofs/${PREFIXNOS}.windrot_geo2proj.gr3 ]
  then
  cp -p $FIXofs/${PREFIXNOS}.windrot_geo2proj.gr3 $DATA/windrot_geo2proj.gr3 
  fi

  if [ -s $FIXofs/${PREFIXNOS}.diffmin.gr3 ]
  then
  cp -p $FIXofs/${PREFIXNOS}.diffmin.gr3   $DATA/diffmin.gr3 
  fi

  if [ -s $FIXofs/${PREFIXNOS}.hgrid.gr3 ]
  then
  cp -p $FIXofs/${PREFIXNOS}.hgrid.gr3 $DATA/hgrid.gr3 
  fi

  if [ -s $FIXofs/${PREFIXNOS}.s_nudge.gr3 ]
  then
  cp -p $FIXofs/${PREFIXNOS}.s_nudge.gr3 $DATA/s_nudge.gr3 
  fi

  if [ -s $FIXofs/${VGRID_CTL} ]
  then
  cp -p $FIXofs/${VGRID_CTL} $DATA/vgrid.in 
  fi

  if [ -s $FIXofs/${PREFIXNOS}.xlsc.gr3 ]
  then
  cp -p $FIXofs/${PREFIXNOS}.xlsc.gr3 $DATA/xlsc.gr3 
  fi
  if [ -s $FIXofs/${PREFIXNOS}.bctides.in ]
  then
  cp -p $FIXofs/${PREFIXNOS}.bctides.in $DATA/bctides.in 
  fi
fi

if [ "${OFS,,}" == "nwgofs" ]; then
     if [ -s ${FIXofs}/${PREFIXNOS}_dam_cell.dat ]; then
        cp -p ${FIXofs}/${PREFIXNOS}_dam_cell.dat $DATA/${RUN}_dam_cell.dat
     fi
     if [ -s ${FIXofs}/${PREFIXNOS}_dam_node.dat ]; then
        cp -p ${FIXofs}/${PREFIXNOS}_dam_node.dat $DATA/${RUN}_dam_node.dat
     fi
fi

if [ -s ${FIXofs}/${PREFIXNOS}_z0_vary.nc ]; then
   cp -p ${FIXofs}/${PREFIXNOS}_z0_vary.nc $DATA/${PREFIXNOS}_z0_vary.nc
fi


# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most (child) scripts

echo "Preparing input files for ${RUN} $RUNTYPE "
echo '-----------------------'
seton

if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
then
  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then 
#CHECK FOR MET CONTROL FILE
    if [ -s ${COMOUT}/$RUNTIME_MET_CTL_NOWCAST ]
    then
      echo "MET control files exist"
      cp -p ${COMOUT}/$RUNTIME_MET_CTL_NOWCAST $DATA/sflux/sflux_inputs.txt
    else
      msg="FATAL ERROR: No MET control file for Nowcast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '******************************************************'
      echo '*** FATAL ERROR : No MET control file for Nowcast  ***'
      echo '******************************************************'
      echo ' '
      echo $msg
      seton
#      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No MET control file for nowcast: ${COMOUT}/$RUNTIME_MET_CTL_NOWCAST"
    fi
  fi

# 1.a RIVER FORCING FILE 
  if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]
  then

    if [ -s $DATA/$RIVER_FORCING_FILE ]
    then
      echo " $DATA/$RIVER_FORCING_FILE existed "
    elif [ -s $COMOUT/$RIVER_FORCING_FILE ]
    then
      cp -p $COMOUT/$RIVER_FORCING_FILE $RIVER_FORCING_FILE
    else  
      msg="FATAL ERROR: NO RIVER FORCING FILE $RIVER_FORCING_FILE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**********************************************'
      echo '*** FATAL ERROR : NO $RIVER_FORCING_FILE   ***'
      echo '**********************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No river forcing file: $COMOUT/$RIVER_FORCING_FILE"
    fi
  elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
  then
    if [ ! -r $DATA/RIVER ]
    then

     if [ -s $DATA/${RIVER_FORCING_FILE} ]
     then
       echo " $DATA/$RIVER_FORCING_FILE existed "
       rm -fr $DATA/RIVER
       tar -xvf $DATA/${RIVER_FORCING_FILE}
     elif [ -s $COMOUT/${RIVER_FORCING_FILE} ]
     then
       rm -fr $DATA/RIVER
       cp -p $COMOUT/${RIVER_FORCING_FILE} $DATA/.
       tar -xvf $DATA/${RIVER_FORCING_FILE}
     else  
      msg="FATAL ERROR: NO RIVER FORCING FILE $RIVER_FORCING_FILE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**********************************************'
      echo '*** FATAL ERROR : NO $RIVER_FORCING_FILE   ***'
      echo '**********************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No river forcing file: ${RIVER_FORCING_FILE}"
     fi
    fi
  elif [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then 
     if [ -s $DATA/selfe_temp.th -a $DATA/selfe_flux.th -a $DATA/selfe_salt.th ]
     then
      echo "RIVER forcing files exist"
      cp -p $DATA/selfe_temp.th  $DATA/temp.th
      cp -p $DATA/selfe_flux.th  $DATA/flux.th
      cp -p $DATA/selfe_salt.th  $DATA/salt.th
     elif [ -s $COMOUT/${RIVER_FORCING_FILE} ]
     then
       cp -p $COMOUT/${RIVER_FORCING_FILE} $DATA/.
       tar -xvf $DATA/${RIVER_FORCING_FILE}
       cp -p $DATA/selfe_temp.th  $DATA/temp.th
       cp -p $DATA/selfe_flux.th  $DATA/flux.th
       cp -p $DATA/selfe_salt.th  $DATA/salt.th
    else
      msg="FATAL ERROR: No River Forcing For Nowcast/Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*************************************************************'
      echo '*** FATAL ERROR : NO River Forcing For Nowcast/Forecast   ***'
      echo '*************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No river forcing for nowcast/forecast: $COMOUT/${RIVER_FORCING_FILE}"
    fi
    
  fi
# 1.b OBC FORCING FILE 

  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    if [ -s $DATA/elev3D.th -a $DATA/salt_nu.in -a $DATA/temp_nu.in ]
    then
      echo "OBC forcing files exist"
    elif [ -s $COMOUT/$OBC_FORCING_FILE ]
    then
#      cp -p $COMOUT/$OBC_FORCING_FILE $DATA/$OBC_FORCING_FILE
      tar -xvf $COMOUT/$OBC_FORCING_FILE
    else
      msg="FATAL ERROR: No OBC Forcing For Nowcast/Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '************************************************************'
      echo '*** FATAL ERROR : NO OBC Forcing For Nowcast/Forecast    ***'
      echo '************************************************************'
      echo ' '
      echo $msg
      seton
#      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No OBC forcing for nowcast/forecast: $COMOUT/$OBC_FORCING_FILE"
    fi
   
  else
    if [ -f $DATA/$OBC_FORCING_FILE ]
    then
      echo "   $DATA/$OBC_FORCING_FILE existed "
    elif [ -s $COMOUT/$OBC_FORCING_FILE ]
    then
      cp -p $COMOUT/$OBC_FORCING_FILE $OBC_FORCING_FILE
# for SFBOFS
    elif [ -s $COMOUT/$OBC_FORCING_FILE_EL ]
    then
      cp -p $COMOUT/$OBC_FORCING_FILE_EL $OBC_FORCING_FILE_EL
      cp -p $COMOUT/$OBC_FORCING_FILE_TS $OBC_FORCING_FILE_TS

    else
      if [ ${RUN}=! "lsofs" -a ${RUN}=! "LSOFS" -a ${RUN}=! "loofs"  -a ${RUN}=! "LOOFS" ]; then	    
        msg="FATAL ERROR: NO OBC FORCING FILE $OBC_FORCING_FILE"
        postmsg "$jlogfile" "$msg"
        postmsg "$nosjlogfile" "$msg"
        setoff
        echo ' '
        echo '********************************************'
        echo '*** FATAL ERROR : NO $OBC_FORCING_FILE   ***'
        echo '********************************************'
        echo ' '
        echo $msg
        seton
        touch err.${RUN}.$PDY1.t${HH}z
        err_exit "No OBC forcing file: $COMOUT/$OBC_FORCING_FILE_EL"
      fi
    fi

  fi

# 1.c Meteorological Forcing For Nowcast 
  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    if [ ! -d $DATA/sflux ]; then
      mkdir -p $DATA/sflux
    else
      rm -f $DATA/sflux/*.nc  
    fi  
    if [ -s $COMOUT/$MET_NETCDF_1_NOWCAST ]
    then
      cd $DATA/sflux
      tar -xvf $COMOUT/$MET_NETCDF_1_NOWCAST 
      cd $DATA
    else
      msg="FATAL ERROR: NO Meteorological Forcing For Nowcast $MET_NETCDF_1_NOWCAST"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*************************************************************'
      echo '*** FATAL ERROR : NO Meteorological Forcing For Nowcast   ***'
      echo '*************************************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No meteorological forcing for nowcast: $COMOUT/$MET_NETCDF_1_NOWCAST"
    fi  
  else 
    if [ -s $DATA/$MET_NETCDF_1_NOWCAST ]
    then
      echo "   $DATA/$MET_NETCDF_1_NOWCAST existed "
    elif [ -s $COMOUT/$MET_NETCDF_1_NOWCAST ]
    then
      cp -p $COMOUT/$MET_NETCDF_1_NOWCAST $MET_NETCDF_1_NOWCAST
    else
      msg="FATAL ERROR: NO Meteorological Forcing For Nowcast $MET_NETCDF_1_NOWCAST"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*************************************************************'
      echo '*** FATAL ERROR : NO Meteorological Forcing For Nowcast   ***'
      echo '*************************************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No meteorological forcing for nowcast: $COMOUT/$MET_NETCDF_1_NOWCAST"
    fi
  fi
   
# 1.d Initial forcing For Nowcast
  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    if [ -f $DATA/$INI_FILE_NOWCAST ]
    then
      echo "   $DATA/$INI_FILE_NOWCAST exists"
      cp -p $DATA/$INI_FILE_NOWCAST $DATA/hotstart.in
    elif [ -s $COMOUT/$INI_FILE_NOWCAST ]
    then
      cp -p $COMOUT/$INI_FILE_NOWCAST $DATA/hotstart.in
    else
      msg="FATAL ERROR: NO intial forcing file For Nowcast $INI_FILE_NOWCAST"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**********************************************************'
      echo '*** FATAL ERROR : NO intial forcing file For Nowcast   ***'
      echo '**********************************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No initial forcing file for nowcast: $COMOUT/$INI_FILE_NOWCAST"
    fi
  else
    if [ -f $DATA/$INI_FILE_NOWCAST ]
    then
      echo "   $COMOUT/$INI_FILE_NOWCAST existed "
    elif [ -s $COMOUT/$INI_FILE_NOWCAST ]
    then
      cp -p $COMOUT/$INI_FILE_NOWCAST $INI_FILE_NOWCAST
    else
      msg="FATAL ERROR: NO intial forcing file For Nowcast $INI_FILE_NOWCAST"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**********************************************************'
      echo '*** FATAL ERROR : NO intial forcing file For Nowcast   ***'
      echo '**********************************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No initial forcing file for nowcast: $COMOUT/$INI_FILE_NOWCAST"
    fi
  fi

# 1.e HFlux For Nowcast
  if [ -f $DATA/$MET_NETCDF_2_NOWCAST ]
  then
      echo "   $DATA/$MET_NETCDF_2_NOWCAST existed "
  elif [ -s $COMOUT/$MET_NETCDF_2_NOWCAST ]
  then
      cp -p $COMOUT/$MET_NETCDF_2_NOWCAST $MET_NETCDF_2_NOWCAST
  fi

# 1.f Nowcast control file
  if [ -f $DATA/${RUN}_${OCEAN_MODEL}_nowcast.in ]
  then
    echo "   $DATA/${RUN}_${OCEAN_MODEL}_nowcast.in is found "
    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
    then
      cp -p $DATA/${RUN}_${OCEAN_MODEL}_nowcast.in $DATA/param.in
    elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
    then
      cp -p $DATA/${RUN}_${OCEAN_MODEL}_nowcast.in $DATA/${RUN}'_run.nml'
    fi  
  elif [ -s $COMOUT/${RUNTIME_CTL_NOWCAST} ]
  then
    echo "$COMOUT/${RUNTIME_CTL_NOWCAST} is found "
    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
    then
      cp -p $COMOUT/${RUNTIME_CTL_NOWCAST} $DATA/param.in
    elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
    then
      cp -p $COMOUT/${RUNTIME_CTL_NOWCAST} $DATA/${RUN}'_run.nml'
    elif [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]
    then
      cp -p $COMOUT/${RUNTIME_CTL_NOWCAST} ${RUN}_${OCEAN_MODEL}_nowcast.in
    fi  
  else
     echo "$DATA/${RUN}_${OCEAN_MODEL}_nowcast.in is not found"
     echo "$COMOUT/${RUNTIME_CTL_NOWCAST} is not found "
     msg="FATAL ERROR: MODEL runtime input file for nowcast is not found"
     postmsg "$jlogfile" "$msg"
     postmsg "$nosjlogfile" "$msg"
    setoff
    echo ' '
    echo '**********************************************************************'
    echo '*** FATAL ERROR : ROMS runtime input file for nowcast is not found ***'
    echo '**********************************************************************'
    echo ' '
    echo $msg
    seton
    touch err.${RUN}.$PDY1.t${HH}z
    err_exit "ROMS runtime input file for nowcast is not found: $COMOUT/${RUNTIME_CTL_NOWCAST}"
  fi

#1.i Tide data 
  if [ $CREATE_TIDEFORCING -ge 0 ]; then
    if [ -f $DATA/$HC_FILE_OFS ]; then
      echo "   $DATA/$HC_FILE_OFS linked "
    elif [ -s $COMOUT/$HC_FILE_OFS ]; then
      cp -p $COMOUT/$HC_FILE_OFS $HC_FILE_OFS
    fi
  fi
#1.j Nudging file
  TS_NUDGING=${TS_NUDGING:-0}
  if [ $TS_NUDGING -eq 1 ]; then
    if [ -f $COMOUT/$NUDG_FORCING_FILE ]; then
      cp -p $COMOUT/$NUDG_FORCING_FILE $NUDG_FORCING_FILE
    else
      echo "$COMOUT/$NUDG_FORCING_FILE is not found"
      msg="FATAL ERROR: T/S nudging is on but the forcing file is not found"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
    fi
  fi

  echo 'Ocean Model run starts at time: ' `date `
# --------------------------------------------------------------------------- #
# 2   Execute ocean model of ROMS; where ${RUN}_roms_nowcast.in is created by nos_ofs_reformat_roms_ctl.sh
  if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]
  then 
#    mpirun $EXECnos/${RUN}_roms_mpi ${RUN}_${OCEAN_MODEL}_nowcast.in >> ${MODEL_LOG_NOWCAST}
    if [ $OFS == wcofs_da ]; then
      mpiexec -n ${TOTAL_TASKS} --ppn 64 --cpu-bind depth --depth 2 $EXECnos/${RUN}_roms_mpi ${RUN}_${OCEAN_MODEL}_nowcast.in >> ${MODEL_LOG_NOWCAST}
    else	     
      mpiexec -n ${TOTAL_TASKS} $EXECnos/${RUN}_roms_mpi ${RUN}_${OCEAN_MODEL}_nowcast.in >> ${MODEL_LOG_NOWCAST}
    fi

    export err=$?
    if [ $err -ne 0 ]
    then
      echo "Running ocean model ${RUN}_roms_mpi for $RUNTYPE did not complete normally"
      msg="Running ocean model ${RUN}_roms_mpi for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_exit "$msg"
#    else
#      echo "Running ocean model ${RUN}_roms_mpi for $RUNTYPE completed normally"
#     msg="Running ocean model ${RUN}_roms_mpi for $RUNTYPE completed normally"
#      postmsg "$jlogfile" "$msg"
#      postmsg "$nosjlogfile" "$msg"
    fi

    rm -f corms.now corms.fcst 
    if [ -s ${MODEL_LOG_NOWCAST} ]
    then
      grep "ROMS/TOMS - Blows up" ${MODEL_LOG_NOWCAST} > corms.now
      grep "Blowing-up" ${MODEL_LOG_NOWCAST} >> corms.now
      grep "Abnormal termination: BLOWUP" ${MODEL_LOG_NOWCAST} >> corms.now
    fi
    if [ -s  corms.now ]
    then
       echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY FAILED 00"  >> $cormslogfile 
       echo "NOWCAST_RUN DONE 0"  >> $cormslogfile
#for development
#       cp -pr $DATA $DATA/../../.
       export err=99; err_chk
    else
       grep "ROMS/TOMS: DONE" ${MODEL_LOG_NOWCAST} > corms.now
       if [ -s  corms.now ]
       then
         echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY COMPLETED SUCCESSFULLY 100" >> $cormslogfile
         echo "NOWCAST_RUN DONE 100"  >> $cormslogfile
       fi
##    create a status file for CO-OPS side after DA cycle
      if [ -z "${OFS##*_da*}" ]
      then
        rm -f ${RUN}.status
      YYYY=`echo $time_nowcastend | cut -c1-4 `
        MM=`echo $time_nowcastend |cut -c5-6 `
        DD=`echo $time_nowcastend |cut -c7-8 `
        HH=`echo $time_nowcastend |cut -c9-10 `
        echo $YYYY$MM$DD$HH > ${RUN}.status
        cp ${RUN}.status $COMOUT/.
        cp -p ${RUN}.status $COMOUT/${RUN}.status_${cyc}
      fi
# save new nowcast restart file into archive directory for next cycle run
      NFILE=`ls -al *${OFS}.rst.nowcast*.nc | wc -l`
      if [ $NFILE -gt 0 ]; then
         latest_restart_f=`ls -al *${OFS}.rst.nowcast*.nc | tail -1 | awk '{print $NF}' `
         cp -p $latest_restart_f $COMOUT/$RST_OUT_NOWCAST
      fi
     
       if [ -f $DATA/$RST_OUT_NOWCAST ]
       then
         cp -p $DATA/$RST_OUT_NOWCAST $COMOUT/$RST_OUT_NOWCAST
         echo "   $RST_OUT_NOWCAST saved "
       fi
       if [ -z "${OFS##*_da*}" ]; then
         if [ ! -d ${COMOUTrst1} ]; then
           mkdir -p ${COMOUTrst1}
         fi
#         (( NINI=$LEN_DA * 3600 / $NRST - 2 )) #not needed after NCO4.4.7
         ncks -d ocean_time,-1 $RST_OUT_NOWCAST -O ${COMOUTrst1}/${RST_OUT_NOWCAST_NF1}
        echo "Saving last record in $RST_OUT_NOWCAST to ${COMOUTrst1}/${RST_OUT_NOWCAST_NF1}"
       fi  
    fi

## separate HIS output file into multiple smaller files
#AJ 02/26/2015       Im1=0
    Im1=0   #for new version of ROMS which doesn't ouput hour=0 (initial time)
    IQm=0
##For ROMS new solution output includes hour 0
    NRREC=`grep "NRREC ==" ${RUN}_${OCEAN_MODEL}_nowcast.in | awk '{print $3}'`
    if [ ${NRREC} -eq 0 ]; then
      Im1=-3
      IQm=-1
    fi
#    NHIS_INTERVAL=`expr $NHIS \* ${DELT_MODEL%.*}`
#    NQCK_INTERVAL=`expr $NQCK \* ${DELT_MODEL%.*}`
#    NHIS_INTERVAL=`expr $NHIS_INTERVAL / 3600`
#    NQCK_INTERVAL=`expr $NQCK_INTERVAL / 3600`

     NHIS_INTERVAL=`expr $NHIS / 3600`
     NQCK_INTERVAL=`expr $NQCK / 3600`
    if [ -f ${PREFIXNOS}.avg.nc ]; then 
      mv ${PREFIXNOS}.avg.nc ${PREFIXNOS}.t${cyc}z.${PDY}.avg.nowcast.nc
      cp -p ${PREFIXNOS}.t${cyc}z.${PDY}.avg.nowcast.nc ${COMOUT}/
    fi
    if [ -f damee4_fwd_outer0_0001.nc ]; then
      mv damee4_fwd_outer0_0001.nc ${PREFIXNOS}.t${cyc}z.${PDY}.fwd_outer0.nc
      cp -p ${PREFIXNOS}.t${cyc}z.${PDY}.fwd_outer0.nc ${COMOUT}/
    fi
    if [ -f damee4_fwd_outer1_0001.nc ]; then
      mv damee4_fwd_outer1_0001.nc ${PREFIXNOS}.t${cyc}z.${PDY}.fwd_outer1.nc
      cp -p ${PREFIXNOS}.t${cyc}z.${PDY}.fwd_outer1.nc ${COMOUT}/
    fi
    if [ -f damee4_fwd_outer2_0001.nc ]; then
      mv damee4_fwd_outer2_0001.nc ${PREFIXNOS}.t${cyc}z.${PDY}.fwd_outer2.nc
      cp -p ${PREFIXNOS}.t${cyc}z.${PDY}.fwd_outer2.nc ${COMOUT}/
    fi
    if [ -f ocean_mod.nc ]; then
      mv ocean_mod.nc ${PREFIXNOS}.t${cyc}z.${PDY}.mod.nc
      cp -p ${PREFIXNOS}.t${cyc}z.${PDY}.mod.nc ${COMOUT}/
    fi
#   copy back the new improved initial conditions
    initfile=${PREFIXNOS}.t${cyc}z.${PDY}.init.nowcast.nc
    if [ -f $initfile -a -z "${OFS##*_da*}" ]; then
      ncks -d ocean_time,2 $initfile ${COMOUT}/${initfile}.new
    fi
#######################
    NFILE=`ls -al *${OFS}*.fields.nowcast*.nc | wc -l`
    if [ $NFILE -gt 0 ]; then
      $USHnos/nos_ofs_rename.sh $OFS $OCEAN_MODEL $RUNTYPE 3d "$time_hotstart" "$time_nowcastend"
    fi
    NFILE=`ls -al ${PREFIXNOS}*.surface.nowcast*_????.nc | wc -l`
    if [ $NFILE -gt 0 ]; then
      $USHnos/nos_ofs_rename.sh $OFS $OCEAN_MODEL $RUNTYPE 2d "$time_hotstart" "$time_nowcastend"
    fi

############
  elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
  then
#    mpirun $EXECnos/fvcom_${RUN} --casename=$RUN > $MODEL_LOG_NOWCAST
    mpiexec -n ${TOTAL_TASKS} $EXECnos/fvcom_${RUN} --casename=$RUN > $MODEL_LOG_NOWCAST
   if [ -s ${DATA}/$STA_EDGE_CTL -a ! -s ${FIXofs}/$STA_EDGE_CTL ]; then
     cp -p ${DATA}/$STA_EDGE_CTL ${FIXofs}/$STA_EDGE_CTL
   fi
 
   export err=$?
    if [ $err -ne 0 ]
    then
      echo "Running ocean model for $RUNTYPE did not complete normally"
      msg="Running ocean model for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_exit "$msg"
    fi

    rm -f corms.now corms.fcst 
    if [ -s ${MODEL_LOG_NOWCAST} ]
    then
      grep "NaN" ${MODEL_LOG_NOWCAST} > corms.now
      grep "STOP" ${MODEL_LOG_NOWCAST} >> corms.now
      grep "failed" ${MODEL_LOG_NOWCAST} >> corms.now
      grep "Failed" ${MODEL_LOG_NOWCAST} >> corms.now
      grep "Blowing-up" ${MODEL_LOG_NOWCAST} >> corms.now
      grep "Abnormal termination: BLOWUP" ${MODEL_LOG_NOWCAST} >> corms.now
      if [ -s  corms.now ]
      then
       echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY FAILED 00"  >> $cormslogfile 
       echo "NOWCAST_RUN DONE 0"  >> $cormslogfile
       export err=99; err_chk
      else
       echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY COMPLETED SUCCESSFULLY 100" >> $cormslogfile
       echo "NOWCAST_RUN DONE 100"  >> $cormslogfile
# save new nowcast restart file into archive directory for next cycle run
       NFILE=`ls -al *${OFS}*_restart*.nc | wc -l`
       if [ $NFILE -gt 0 ]; then
         latest_restart_f=`ls -al *${OFS}*_restart*.nc | tail -1 | awk '{print $NF}' `
         cp -p $latest_restart_f $DATA/$RST_OUT_NOWCAST
         cp -p $latest_restart_f $COMOUT/$RST_OUT_NOWCAST
       fi

#       if [ -f $DATA/$RUN'_restart_0001.nc' ]
#       then
#         mv $DATA/$RUN'_restart_0001.nc' $DATA/$RST_OUT_NOWCAST
#         cp -p $DATA/$RST_OUT_NOWCAST $COMOUT/$RST_OUT_NOWCAST
#         echo "   $RST_OUT_NOWCAST saved "
#       fi
#       if [ -f $DATA/$RUN'_0001.nc' ]
#       then
#          cp -p  $DATA/$RUN'_0001.nc' $DATA/$HIS_OUT_NOWCAST
#       fi
       NFILE=`ls -al *${OFS}*_station_timeseries*.nc | wc -l`
       if [ $NFILE -gt 0 ]; then
	  latest_restart_f=`ls -al *${OFS}*station_timeseries*.nc | tail -1 | awk '{print $NF}' `
          cp -p $latest_restart_f $DATA/$STA_OUT_NOWCAST
          cp -p $latest_restart_f $COMOUT/$STA_OUT_NOWCAST
       fi
#       if [ -f $DATA/${RUN}*'_station_timeseries.nc' ]
#       then
#         mv $DATA/${RUN}*'_station_timeseries.nc' $DATA/$STA_OUT_NOWCAST
#       fi
#######################
       NFILE=`ls -al *${OFS}*_surface_????.nc | wc -l`
       if [ $NFILE -gt 0 ]; then
        $USHnos/nos_ofs_rename.sh $OFS $OCEAN_MODEL $RUNTYPE 2d "$time_hotstart" "$time_nowcastend"
       fi
       NFILE=`ls -al *${OFS}*_????.nc | wc -l`
       if [ $NFILE -gt 0 ]; then
        $USHnos/nos_ofs_rename.sh $OFS $OCEAN_MODEL $RUNTYPE 3d "$time_hotstart" "$time_nowcastend"
       fi

#NC_OUT_INTERVAL=`printf '%.0f' $NC_OUT_INTERVAL` ## convert decimal number to integer, and get the nearest integer
       NCSF_OUT_INTERVAL=${NCSF_OUT_INTERVAL%.*} # just tuncate the integer part and remove the fractional part   
       NHIS_INTERVAL=`expr $NC_OUT_INTERVAL / 3600`
       NQCK_INTERVAL=`expr $NCSF_OUT_INTERVAL / 3600`
       echo $NHIS_INTERVAL $NQCK_INTERVAL 
       Im1=0
       IQm=0
       I=1

       
      fi  
#      if [ "${OFS,,}" == "ngofs" ]; then
#        if [ -s nos_${RUN}_nestnode_negofs.nc ]; then
#          cp -p nos_${RUN}_nestnode_negofs.nc $COMOUT/${PREFIXNOS}.nestnode.negofs.nowcast.$PDY.t${cyc}z.nc
#        fi
#        if [ -s nos_${RUN}_nestnode_nwgofs.nc ]; then
#          cp -p nos_${RUN}_nestnode_nwgofs.nc $COMOUT/${PREFIXNOS}.nestnode.nwgofs.nowcast.$PDY.t${cyc}z.nc
#        fi
#      fi

    else
       echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY FAILED 00"  >> $cormslogfile 
       echo "NOWCAST_RUN DONE 0"  >> $cormslogfile
       export err=99; err_chk
    fi  
  elif [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    echo "nowcast simulation began at:  `date`" >> $nosjlogfile
#    mpirun $EXECnos/selfe_${RUN} > $MODEL_LOG_NOWCAST
    mpiexec -n ${TOTAL_TASKS} $EXECnos/selfe_${RUN} > $MODEL_LOG_NOWCAST
    export err=$?
    rm -f corms.now corms.fcst 
    if [ -s $DATA/mirror.out ]
    then
      grep "Run completed successfully" $DATA/mirror.out > corms.now
    fi
    if [ ! -s  corms.now ]
    then
       echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY FAILED 00"  >> $cormslogfile 
       echo "NOWCAST_RUN DONE 0"  >> $cormslogfile
       export err=99; err_chk
    else
       echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY COMPLETED SUCCESSFULLY 100" >> $cormslogfile
       echo "NOWCAST_RUN DONE 100"  >> $cormslogfile
    fi

    if [ $err -ne 0 ]
    then
      echo "Running ocean model for $RUNTYPE did not complete normally"
      msg="Running ocean model  for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running ocean model for $RUNTYPE completed normally"
      msg="Running ocean model  for $RUNTYPE completed normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
    fi
    echo "nowcast simulation completed at:  `date`" >> $nosjlogfile
## combine all hotstart files into a single restart file    
    if [ -s ${COMOUT}/$RUNTIME_COMBINE_RST_NOWCAST ]
    then
      echo "combine restart control files exists"
      cp -p ${COMOUT}/$RUNTIME_COMBINE_RST_NOWCAST $DATA/outputs/combine_hotstart.in
    else
      msg="FATAL ERROR: No combine restart control file"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '********************************************************'
      echo '*** FATAL ERROR : No combine restart control file    ***'
      echo '********************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No combine restart control file: ${COMOUT}/$RUNTIME_COMBINE_RST_NOWCAST"
    fi
#run combine hotstart executable
    cd $DATA/outputs
    $EXECnos/nos_ofs_combine_hotstart_out_selfe < ./combine_hotstart.in 
    export err=$?
    if [ $err -ne 0 ]; then
      echo "Running nos_ofs_combine_hotstart_out_selfe did not complete normally"
      msg="Running nos_ofs_combine_hotstart_out_selfe did not complete normally"
      postmsg "$jlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running nos_ofs_combine_hotstart_out_selfe completed normally"
      msg="Running nos_ofs_combine_hotstart_out_selfe completed normally"
      postmsg "$jlogfile" "$msg"
    fi
    if [ -s hotstart.in ]
    then
      cp -p hotstart.in  $COMOUT/$RST_OUT_NOWCAST
#      cp -p hotstart.in $DATA
      echo "NOWCAST RESTART FILES HAVE BEEN COMBINED SUCCESSFULLY 100" >> $cormslogfile
    else
      echo "NOWCAST RESTART FILES HAVE BEEN COMBINED SUCCESSFULLY 0" >> $cormslogfile
      msg="FATAL ERROR: No hotstart.in restart file for the next model run"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '************************************************************************'
      echo '*** FATAL ERROR : No hotstart.in restart file for the next model run ***'
      echo '************************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No hotstart.in restart file for the next model run"
    fi
# compute water level offset over previous 7 days at NOS NWLON stations
    echo "water level offset correction began at:  `date`" >> $nosjlogfile
    cd $DATA
    CURRENTTIME=$time_nowcastend
##  allow to search back for maximum 5 days
    CURRENTTIMEm5=`$NDATE -120 $CURRENTTIME `
    YYYY=`echo $CURRENTTIME | cut -c1-4 `
      MM=`echo $CURRENTTIME |cut -c5-6 `
      DD=`echo $CURRENTTIME |cut -c7-8 `
      HH=`echo $CURRENTTIME |cut -c9-10 `
    OLDFILE=$COMOUTroot/${OFS}.$YYYY$MM$DD/$WL_OFFSET_OLD
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
    else
       if [ -s $FIXofs/$WL_OFFSET_OLD ]; then
         cp $FIXofs/$WL_OFFSET_OLD $DATA
       else
         echo cannot find $WL_OFFSET_OLD in archive directory
         echo "please copy $WL_OFFSET_OLD in creofs.YYYYMMDD"  
         err_exit "Cannot find $FIXofs/$WL_OFFSET_OLD"
       fi
    fi
    if [ ! -s $CORRECTION_STATION_CTL ]; then
       cp -p $FIXofs/$CORRECTION_STATION_CTL $DATA
    fi

    if [ ! -s $GRIDFILE_LL ]; then
       cp -p $FIXofs/$GRIDFILE_LL $DATA 
    fi
    if [ ! -s $STA_NETCDF_CTL ]; then
       cp -p $FIXofs/$STA_NETCDF_CTL $DATA 
    fi
#    if [ ! -s $WL_OFFSET_OLD ]; then
#       cp -p $FIXofs/$WL_OFFSET_OLD $DATA 
#    fi
    
    rm -f tmpsta.ctl
    echo $COMOUTroot > tmpsta.ctl
    echo $DCOMINports >> tmpsta.ctl
    echo $NOSBUFR >> tmpsta.ctl
    echo $CORRECTION_STATION_CTL >> tmpsta.ctl
    echo $GRIDFILE_LL >> tmpsta.ctl
    echo $STA_NETCDF_CTL >> tmpsta.ctl
    echo $WL_OFFSET_OLD >> tmpsta.ctl
    echo $time_nowcastend >> tmpsta.ctl   #END_TIME='YYYYMMDDHH'
    echo $PREFIXNOS >> tmpsta.ctl
    $EXECnos/nos_creofs_wl_offset_correction < ./tmpsta.ctl > ./Fortran_offset.log
    if [ -s $DATA/Fortran_offset.log ]
    then
      grep "has completed successfully" $DATA/Fortran_offset.log > corms.now
    fi
    if [ ! -s corms.now ]
    then
       echo " Running nos_creofs_wl_offset_correction FAILED 00"  >> $cormslogfile 
#       export err=99; err_chk
    else
       echo " Running nos_creofs_wl_offset_correction COMPLETED SUCCESSFULLY 100"  >> $cormslogfile 
    fi

    if [ -s $WL_OFFSET_OLD ]; then
#       cp -p $WL_OFFSET_OLD $FIXofs/$WL_OFFSET_OLD
        cp -p $WL_OFFSET_OLD $COMOUT 
    fi
    echo "water level correction  completed at:  `date`" >> $nosjlogfile
# combine field outputs into a single NetCDF file
    echo "combine field nowcast simulation began at:  `date`" >> $nosjlogfile 
    cd $DATA/outputs
    rm -f listyes listno
    I=1
#    while [ $I -le 100 ]
     while (( I < 100 ))
    do
 
      file=$I'_0000_elev.61'
      echo $file
      if [ -s $file ]; then
         echo $file >> listyes
      fi	 
      (( I = I + 1 ))
    done

    if [ -s listyes ]; then
#       btmnow=`cat listyes | awk -F_ '{print $1}' | sort | head -1`
#       etmnow=`cat listyes | awk -F_ '{print $1}' | sort | tail -1`
       btmnow=`cat listyes | awk -F_ '{print $1}' |  head -1`
       etmnow=`cat listyes | awk -F_ '{print $1}' |  tail -1`
    fi
    if [ -s $FIXofs/$RUNTIME_COMBINE_NETCDF ]
    then
      if [ -s $FIXofs/$GRIDFILE_LL ]; then
        cp -p $FIXofs/$GRIDFILE_LL $DATA/outputs
      fi
      if [ -s $DATA/param.in ]; then
        cp -p $DATA/param.in $DATA/outputs
      else
        echo "$DATA/param.in does not exist"
        echo 'combine field netcdf code will fail!'
      fi
      if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]; then
         ncastfcast="n"
      fi	 	
      cat $FIXofs/$RUNTIME_COMBINE_NETCDF | sed -e s/BTM/$btmnow/g -e s/ETM/$etmnow/g \
      -e s/GRIDFILE/$GRIDFILE_LL/g \
      -e s/BASE_DATE/$BASE_DATE/g \
      -e s/TIME_NOWCASTEND/${time_nowcastend}/g \
      -e s/RUNTYPE/${ncastfcast}/g > combine_output.in
      cp -p combine_output.in ${COMOUT}/$RUNTIME_COMBINE_NETCDF_NOWCAST
    else
      msg="FATAL ERROR: No control file for combining field outputs of $RUNTYPE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo " "
      echo "******************************************************************************"
      echo "*** FATAL ERROR : No control file for combining field outputs of $RUNTYPE  ***"
      echo "******************************************************************************"
      echo " "
      echo $msg
      seton
#      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No control file for combining field outputs of $RUNTYPE: $FIXofs/$RUNTIME_COMBINE_NETCDF"
    fi
 
#run netcdf combine executable
#    cp -p $EXECnos/nos_ofs_combine_field_netcdf_selfe 
     tar -cvf - . | cat >/dev/null 
     ${EXECnos}/nos_ofs_combine_field_netcdf_selfe $PREFIXNOS
     
#    ${EXECnos}/nos_ofs_combine_field_netcdf_selfe

    export err=$?
    if [ $err -ne 0 ]; then
      echo "Running nos_ofs_combine_field_netcdf_selfe for $RUNTYPE did not complete normally"
      msg="Running nos_ofs_combine_field_netcdf_selfe for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running nos_ofs_combine_field_netcdf_selfe for $RUNTYPE completed normally"
      msg="Running nos_ofs_combine_field_netcdf_selfe for $RUNTYPE completed normally"
      postmsg "$jlogfile" "$msg"
    fi
    echo "combine field nowcast simulation completed at:  `date`" >> $nosjlogfile
# combine station outputs into a single NetCDF file for $RUNTYPE

    if [ -s ${COMOUT}/$RUNTIME_COMBINE_NETCDF_STA_NOWCAST ]
    then
      echo "combine restart control files exists"
      cp -p ${COMOUT}/$RUNTIME_COMBINE_NETCDF_STA_NOWCAST $DATA/outputs/combine_station_output.in
    else
      msg="FATAL ERROR: No combine netcdf station control file"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**************************************************************'
      echo '*** FATAL ERROR : No combine netcdf station control file   ***'
      echo '**************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No combine NetCDF station control file: ${COMOUT}/$RUNTIME_COMBINE_NETCDF_STA_NOWCAST"
    fi
    if [ -s $FIXofs/${STA_NETCDF_CTL} ]
    then
       cp -p $FIXofs/${STA_NETCDF_CTL} $DATA/outputs
    else
       echo "$FIXofs/${STA_NETCDF_CTL} does not rxist !!!"
       echo " combine station netCDF code will not run !!!"
       echo "Provide $FIXofs/${STA_NETCDF_CTL}"
    fi     
#run netcdf combine executable
    $EXECnos/nos_ofs_combine_station_netcdf_selfe <  ./combine_station_output.in
    export err=$?
    if [ $err -ne 0 ]; then
      echo "Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE did not complete normally"
      msg="Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE completed normally"
      msg="Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE completed normally"
      postmsg "$jlogfile" "$msg"
    fi
#move netcdf files + hotstart files to COMOUT
    if [ -s  Station.nc ]; then 
      mv  Station.nc ${DATA}/$STA_OUT_NOWCAST
#      cp -p  Station.nc ${COMOUT}/$STA_OUT_NOWCAST
    fi
#    if [ -s  combinefields.nc ]; then 
#      cp -p  combinefields.nc ${COMOUT}/$HIS_OUT_NOWCAST
#    fi
#    for combinefields in `ls ${DATA}/${PREFIXNOS}.creofs.fields.n*.nc`
#    do
#      cp -p ${combinefields} ${COMOUT}/.
#      cp -p ${combinefields} ${DATA}/.
#    done

    cd $DATA
    mv $DATA/outputs $DATA/outputs_nowcast
    mv mirror.out $DATA/outputs_nowcast
  fi
  echo 'Ocean Model run ends at time: ' `date `
#save 3D surface nowcast field output file into COMOUT
  for combinefields in `ls ${DATA}/${PREFIXNOS}*.fields.n*.nc`
  do
     cp -p ${combinefields} ${COMOUT}/.
  done
#save 2D surface nowcast field output file into COMOUT
  if [ -s ${DATA}/${PREFIXNOS}*.2ds.n001.nc ]; then
    for combinefields in `ls ${DATA}/${PREFIXNOS}*.2ds.n*.nc`
    do
      cp -p ${combinefields} ${COMOUT}/.
    done
  fi
#save nowcast station output file into COMOUT
  cp -p $DATA/$STA_OUT_NOWCAST  ${COMOUT}/$STA_OUT_NOWCAST
  if [ -s $DATA/${PREFIXNOS}.t${cyc}z.${PDY}.avg.nowcast.nc ]; then
    cp -p $DATA/${PREFIXNOS}.t${cyc}z.${PDY}.avg.n*.nc $COMOUT/
  fi 
fi


#### FORECAST
 
if [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]
then

  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then 
    if [ ! -d $DATA/outputs ]; then
      mkdir -p $DATA/outputs
    fi  
#CHECK FOR MET CONTROL FILE
    if [ -s ${COMOUT}/$RUNTIME_MET_CTL_FORECAST ]
    then
      echo "MET control files exist"
      cp -p ${COMOUT}/$RUNTIME_MET_CTL_FORECAST $DATA/sflux/sflux_inputs.txt
    else
      msg="FATAL ERROR: No MET control file for Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*******************************************************'
      echo '*** FATAL ERROR : No MET control file for Forecast  ***'
      echo '*******************************************************'
      echo ' '
      echo $msg
      seton
#      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No MET control file for forecast: ${COMOUT}/$RUNTIME_MET_CTL_FORECAST"
    fi

  fi
# 1.a RIVER FORCING FILE 
  if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]
  then
    if [ -s $DATA/$RIVER_FORCING_FILE ]
    then
      echo " $DATA/$RIVER_FORCING_FILE existed "
    elif [ -s $COMOUT/$RIVER_FORCING_FILE ]
    then
      cp -p $COMOUT/$RIVER_FORCING_FILE $RIVER_FORCING_FILE
    else  
      msg="FATAL ERROR: NO RIVER FORCING FILE $RIVER_FORCING_FILE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*********************************************'
      echo '*** FATAL ERROR : NO RIVER_FORCING_FILE   ***'
      echo '*********************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No river forcing file: $COMOUT/$RIVER_FORCING_FILE"
    fi
  elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
  then
    if [ -s $DATA/${RIVER_FORCING_FILE} ]
    then
      echo " $DATA/$RIVER_FORCING_FILE existed "
      rm -fr $DATA/RIVER
      tar -xvf $DATA/${RIVER_FORCING_FILE}
    elif [ -s $COMOUT/${RIVER_FORCING_FILE} ]
    then
      rm -fr $DATA/RIVER
      cp -p $COMOUT/${RIVER_FORCING_FILE} $DATA/.
      tar -xvf $DATA/${RIVER_FORCING_FILE}
    else  
      msg="FATAL ERROR: NO RIVER FORCING FILE $RIVER_FORCING_FILE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*********************************************'
      echo '*** FATAL ERROR : NO RIVER_FORCING_FILE   ***'
      echo '*********************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No river forcing file: $COMOUT/${RIVER_FORCING_FILE}"
    fi
  elif [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then 
     if [ -s $DATA/selfe_temp.th -a $DATA/selfe_flux.th -a $DATA/selfe_salt.th ]
     then
      echo "RIVER forcing files exist"
      cp -p $DATA/selfe_temp.th  $DATA/temp.th
      cp -p $DATA/selfe_flux.th  $DATA/flux.th
      cp -p $DATA/selfe_salt.th  $DATA/salt.th
     elif [ -s $COMOUT/${RIVER_FORCING_FILE} ]
     then
#       cp -p $COMOUT/${RIVER_FORCING_FILE} $DATA/.
       tar -xvf $COMOUT/${RIVER_FORCING_FILE}
       cp -p $DATA/selfe_temp.th  $DATA/temp.th
       cp -p $DATA/selfe_flux.th  $DATA/flux.th
       cp -p $DATA/selfe_salt.th  $DATA/salt.th
    else
      msg="FATAL ERROR: No River Forcing For Nowcast/Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*************************************************************'
      echo '*** FATAL ERROR : NO River Forcing For Nowcast/Forecast   ***'
      echo '*************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No river forcing for nowcast/forecast: $COMOUT/${RIVER_FORCING_FILE}"
    fi
  fi

# 1.b OBC FORCING FILE 
  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    if [ -s $DATA/elev3D.th -a $DATA/salt_nu.in -a $DATA/temp_nu.in ]
    then
      echo "OBC forcing files exist"
    elif [ -s $COMOUT/$OBC_FORCING_FILE ]
    then
#      cp -p $COMOUT/$OBC_FORCING_FILE $DATA/$OBC_FORCING_FILE
      tar -xvf $COMOUT/$OBC_FORCING_FILE
    else
      msg="FATAL ERROR: No OBC Forcing For Nowcast/Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '************************************************************'
      echo '*** FATAL ERROR : NO OBC Forcing For Nowcast/Forecast    ***'
      echo '************************************************************'
      echo ' '
      echo $msg
      seton
#      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No OBC forcing for nowcast/forecast: $COMOUT/$OBC_FORCING_FILE"
    fi
  else
    if [ -f $DATA/$OBC_FORCING_FILE ]
    then
      echo "   $DATA/$OBC_FORCING_FILE existed "
    elif [ -s $COMOUT/$OBC_FORCING_FILE ]
    then
      cp -p $COMOUT/$OBC_FORCING_FILE $OBC_FORCING_FILE
    elif [ -s $COMOUT/$OBC_FORCING_FILE_EL ]
    then
      cp -p $COMOUT/$OBC_FORCING_FILE_EL $OBC_FORCING_FILE_EL
      cp -p $COMOUT/$OBC_FORCING_FILE_TS $OBC_FORCING_FILE_TS
    else
     if [ ${RUN}=! "lsofs" -a ${RUN}=! "LSOFS" -a ${RUN}=! "loofs"  -a ${RUN}=! "LOOFS" ]; then	    
      msg="FATAL ERROR: NO OBC FORCING FILE $OBC_FORCING_FILE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*******************************************'
      echo '*** FATAL ERROR : NO OBC_FORCING_FILE   ***'
      echo '*******************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No OBC forcing file: $COMOUT/$OBC_FORCING_FILE_EL"
     fi 
    fi
  fi
# 1.c Meteorological Forcing For forecast 
  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    if [ ! -d $DATA/sflux ]; then
      mkdir -p $DATA/sflux
    else
      rm -f $DATA/sflux/*.nc
    fi  
    if [ -s $COMOUT/$MET_NETCDF_1_FORECAST ]
    then
      cd $DATA/sflux
      tar -xvf $COMOUT/$MET_NETCDF_1_FORECAST
      cd $DATA
    else
      msg="FATAL ERROR: NO Meteorological Forcing For forecast $MET_NETCDF_1_FORECAST"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*************************************************************'
      echo '*** FATAL ERROR : NO Meteorological Forcing For forecast  ***'
      echo '*************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No meteorological forcing for forecast: $COMOUT/$MET_NETCDF_1_FORECAST"
        
    fi  
  else
    if [ -f $DATA/$MET_NETCDF_1_FORECAST ]
    then
      echo "   $DATA/$MET_NETCDF_1_FORECAST existed "
    elif [ -s $COMOUT/$MET_NETCDF_1_FORECAST ]
    then
      cp -p $COMOUT/$MET_NETCDF_1_FORECAST $MET_NETCDF_1_FORECAST
    else
      msg="FATAL ERROR: NO Meteorological Forcing For forecast $MET_NETCDF_1_FORECAST"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**************************************************************'
      echo '*** FATAL ERROR : NO Meteorological Forcing For forecast   ***'
      echo '**************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No meteorological forcing for forecast: $COMOUT/$MET_NETCDF_1_FORECAST"
    fi  
  fi

# 1.g HFlux For Forecast
  if [ -s $DATA/$MET_NETCDF_2_FORECAST ]
  then
      echo "   $DATA/$MET_NETCDF_2_FORECAST existed "
  elif [ -s $COMOUT/$MET_NETCDF_2_FORECAST ]
  then
      cp -p $COMOUT/$MET_NETCDF_2_FORECAST $MET_NETCDF_2_FORECAST
  fi

# 1.h forecast cntl file
  if [ -f $DATA/${RUN}_${OCEAN_MODEL}_forecast.in ]
  then
    echo "   $DATA/${RUN}_${OCEAN_MODEL}_forecast.in existed "
    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
    then
      cp -p $DATA/${RUN}_${OCEAN_MODEL}_forecast.in $DATA/param.in
    elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
    then
      cp -p $DATA/${RUN}_${OCEAN_MODEL}_forecast.in $DATA/${RUN}'_run.nml'
    fi  
  elif [ -s $COMOUT/${RUNTIME_CTL_FORECAST} ]
  then
    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
    then
      cp -p $COMOUT/${RUNTIME_CTL_FORECAST} $DATA/param.in
    elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
    then
      cp -p $COMOUT/${RUNTIME_CTL_FORECAST} $DATA/${RUN}'_run.nml'
    elif [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]
    then
      cp -p $COMOUT/${RUNTIME_CTL_FORECAST} ${RUN}_${OCEAN_MODEL}_forecast.in
    fi  

  else
    msg="FATAL ERROR: MODEL runtime input file for forecast is not found"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    setoff
    echo ' '
    echo '******************************************************************'
    echo '*** FATAL ERROR : ROMS runtime input file for nowcast is not found'
    echo '******************************************************************'
    echo ' '
    echo $msg
    seton
    touch err.${RUN}.$PDY1.t${HH}z
    err_exit "ROMS runtime input file for nowcast is not found: $COMOUT/${RUNTIME_CTL_FORECAST}"
  fi

#1.h Tide data 
  if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]
  then
    if [ -f $DATA/$HC_FILE_OFS ]
    then
       echo "   $DATA/$HC_FILE_OFS existed "
    elif [ -s $COMOUT/$HC_FILE_OFS ]
    then
      cp -p $COMOUT/$HC_FILE_OFS $HC_FILE_OFS
    else
      msg="FATAL ERROR: Tide Constituent file for ROMS OBC is not found"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*****************************************************************'
      echo '*** FATAL ERROR : Tide Constituent file for ROMS OBC is not found'
      echo '*****************************************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "Tide constituent file for ROMS OBC is not found: $COMOUT/$HC_FILE_OFS"
    fi
  fi

#1.i Nowcast RST file 
  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then

    if [ -f $DATA/$RST_OUT_NOWCAST ]
    then
      cp -p  $DATA/$RST_OUT_NOWCAST  $DATA/hotstart.in
    elif [ -s $COMOUT/$RST_OUT_NOWCAST ]
    then
      cp -p $COMOUT/$RST_OUT_NOWCAST $DATA/hotstart.in
    else
      msg="FATAL ERROR: NO Restart file for Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**************************************************'
      echo '*** FATAL ERROR : NO Restart file for Forecast ***'
      echo '**************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No restart file for forecast: $COMOUT/$RST_OUT_NOWCAST"
    fi
  else
    if [ -f $DATA/$RST_OUT_NOWCAST ]
    then
      echo "   $DATA/$RST_OUT_NOWCAST existed " 
    elif [ -s $COMOUT/$RST_OUT_NOWCAST ]
    then
      cp -p $COMOUT/$RST_OUT_NOWCAST $RST_OUT_NOWCAST
    else
      msg="FATAL ERROR: NO Restart file for Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**************************************************'
      echo '*** FATAL ERROR : NO Restart file for Forecast ***'
      echo '**************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No restart file for forecast: $COMOUT/$RST_OUT_NOWCAST"
    fi
  fi 
# --------------------------------------------------------------------------- #
# 2   Execute ocean model of ROMS; where ${RUN}_roms_forecast.in is created by nos_ofs_reformat_roms_ctl.sh
  if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]; then
#     mpirun $EXECnos/${RUN}_roms_mpi ./${RUN}_${OCEAN_MODEL}_forecast.in >> ${MODEL_LOG_FORECAST}
     mpiexec -n ${TOTAL_TASKS} $EXECnos/${RUN}_roms_mpi ./${RUN}_${OCEAN_MODEL}_forecast.in >> ${MODEL_LOG_FORECAST}
    export err=$?
    if [ $err -ne 0 ]
    then
      echo "Running ocean model ${RUN}_roms_mpi for $RUNTYPE did not complete normally"
      msg="Running ocean model ${RUN}_roms_mpi for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_exit "$msg"
#    else
#      echo "Running ocean model ${RUN}_roms_mpi completed normally"
#      msg="Running ocean model ${RUN}_roms_mpi  completed normally"
#      postmsg "$jlogfile" "$msg"
#      postmsg "$nosjlogfile" "$msg"
    fi

    rm -f corms.fcst
    if [ -s ${MODEL_LOG_FORECAST} ]
    then
      grep "ROMS/TOMS - Blows up" ${MODEL_LOG_FORECAST} > corms.fcst
      grep "Blowing-up" ${MODEL_LOG_FORECAST} >> corms.fcst
      grep "Abnormal termination: BLOWUP" ${MODEL_LOG_FORECAST} >> corms.fcst
    fi
    if [ -s  corms.fcst ]
    then
      echo "${RUN} FORECAST RUN OF CYCLE t${HH}z ON $PDY FAILED 00"  >> $cormslogfile 
      echo "FORECAST_RUN DONE 0"  >> $cormslogfile
      export err=99; err_chk
    else
      echo "${RUN} FORECAST RUN  OF CYCLE t${HH}z ON $PDY COMPLETED SUCCESSFULLY 100" >> $cormslogfile
      echo "FORECAST_RUN DONE 100"  >> $cormslogfile
##    create a status file for CO-OPS side
      rm -f ${RUN}.status
      YYYY=`echo $time_nowcastend | cut -c1-4 `
        MM=`echo $time_nowcastend |cut -c5-6 `
        DD=`echo $time_nowcastend |cut -c7-8 `
        HH=`echo $time_nowcastend |cut -c9-10 `
       echo $YYYY$MM$DD$HH > ${RUN}.status
       cp ${RUN}.status $COMOUT/.
       cp -p ${RUN}.status $COMOUT/${RUN}.status_${cyc}
    fi
## separate HIS output file into multiple smaller files
#AJ 02/26/2015       Im1=0
    Im1=0   #for new version of ROMS which doesn't ouput hour=0 (initial time)
    IQm=0
#    I=1
#    while (( I < 168 ))
#    do
#       fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
#       file=${PREFIXNOS}.fields.forecast.$PDY.t${cyc}z_${fhr4}.nc
#       if [ -s $file ]; then
#         fhr3=`echo $Im1 |  awk '{printf("%03i",$1)}'`
#         fileout=${PREFIXNOS}.fields.f${fhr3}.$PDY.t${cyc}z.nc
#         mv $file $fileout
#         Im1=`expr $Im1 + 1`
#       fi
#       (( I = I + 1 ))
#    done
    if [ -f ${PREFIXNOS}*.avg.nc ]; then
      mv ${PREFIXNOS}*.avg.nc ${PREFIXNOS}t${cyc}z.${PDY}..avg.forecast.nc
    fi
#######################
    NFILE=`ls -al ${PREFIXNOS}*.fields.forecast*.nc | wc -l`
    if [ $NFILE -gt 0 ]; then
      $USHnos/nos_ofs_rename.sh $OFS $OCEAN_MODEL $RUNTYPE 3d "$time_nowcastend" "$time_nowcastend"
    fi
    NFILE=`ls -al ${PREFIXNOS}*.surface.forecast*.nc | wc -l`
    if [ $NFILE -gt 0 ]; then
      $USHnos/nos_ofs_rename.sh $OFS $OCEAN_MODEL $RUNTYPE 2d "$time_nowcastend" "$time_nowcastend"
    fi
  elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
  then
    rm -f $MODEL_LOG_FORECAST
#    mpirun $EXECnos/fvcom_${RUN} --casename=$RUN > $MODEL_LOG_FORECAST
    mpiexec -n ${TOTAL_TASKS} $EXECnos/fvcom_${RUN} --casename=$RUN > $MODEL_LOG_FORECAST
    export err=$?
    if [ $err -ne 0 ]
    then
      echo "Running ocean model for $RUNTYPE did not complete normally"
      msg="Running ocean model  for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_exit "$msg"
#    else
#      echo "Running ocean model for $RUNTYPE completed normally"
#      msg="Running ocean model  for $RUNTYPE completed normally"
#      postmsg "$jlogfile" "$msg"
#      postmsg "$nosjlogfile" "$msg"
    fi
    rm -f corms.fcst
    if [ -s ${MODEL_LOG_FORECAST} ]
    then
      grep "NaN" ${MODEL_LOG_FORECAST} > corms.fcst
      grep "STOP" ${MODEL_LOG_FORECAST} >> corms.fcst
      grep "failed" ${MODEL_LOG_FORECAST} >> corms.fcst
      grep "Failed" ${MODEL_LOG_FORECAST} >> corms.fcst
      grep "Blowing-up" ${MODEL_LOG_FORECAST} >> corms.fcst
      grep "Abnormal termination: BLOWUP" ${MODEL_LOG_FORECAST} >> corms.fcst
      if [ -s  corms.fcst ]
      then
        echo "${RUN} FORECAST RUN OF CYCLE t${HH}z ON $YYYY$MM$DD FAILED 00"  >> $cormslogfile 
        echo "FORECAST_RUN DONE 0"  >> $cormslogfile
        export err=99; err_chk
      else
        echo "${RUN} FORECAST RUN  OF CYCLE t${HH}z ON $YYYY$MM$DD COMPLETED SUCCESSFULLY 100" >> $cormslogfile
        echo "FORECAST_RUN DONE 100"  >> $cormslogfile
##    create a status file for CO-OPS side
        rm -f ${RUN}.status
        YYYY=`echo $time_nowcastend | cut -c1-4 `
          MM=`echo $time_nowcastend |cut -c5-6 `
          DD=`echo $time_nowcastend |cut -c7-8 `
          HH=`echo $time_nowcastend |cut -c9-10 `
        echo $YYYY$MM$DD$HH > ${RUN}.status
        cp ${RUN}.status $COMOUT/.
        cp -p ${RUN}.status $COMOUT/${RUN}.status_${cyc}

#        if [ -f $DATA/${RUN}'_0001.nc' ]
#        then
#         cp -p $DATA/${RUN}'_0001.nc' $DATA/$HIS_OUT_FORECAST
#         echo "  $HIS_OUT_FORECAST  saved "
#       fi
        if [ -f $DATA/${PREFIXNOS}*'_station_timeseries.nc' ]
        then
          mv $DATA/${PREFIXNOS}*'_station_timeseries.nc' $DATA/$STA_OUT_FORECAST
          echo "  $STA_OUT_FORECAST  saved "
        fi
        NFILE=`ls -al ${PREFIXNOS}*_surface_????.nc | wc -l`
        if [ $NFILE -gt 0 ]; then
          $USHnos/nos_ofs_rename.sh $OFS $OCEAN_MODEL $RUNTYPE 2d "$time_nowcastend" "$time_nowcastend"
        fi
        NFILE=`ls -al ${PREFIXNOS}_????.nc | wc -l`
        if [ $NFILE -gt 0 ]; then
          $USHnos/nos_ofs_rename.sh $OFS $OCEAN_MODEL $RUNTYPE 3d "$time_nowcastend " "$time_nowcastend"
        fi
        NC_OUT_INTERVAL=`printf '%.0f' $NC_OUT_INTERVAL` ## convert decimal number to integer, and get the nearest integer
        NCSF_OUT_INTERVAL=${NCSF_OUT_INTERVAL%.*} # just tuncate the integer part and remove the fractional part   
        NHIS_INTERVAL=`expr $NC_OUT_INTERVAL / 3600`
        NQCK_INTERVAL=`expr $NCSF_OUT_INTERVAL / 3600`

      fi
    else
        echo "${RUN} FORECAST RUN OF CYCLE t${HH}z ON $YYYY$MM$DD FAILED 00"  >> $cormslogfile 
        echo "FORECAST_RUN DONE 0"  >> $cormslogfile
        export err=99; err_chk
    fi  

#    if [ ${OFS} == "NGOFS" -o ${OFS} == "ngofs" ]; then
#       if [ -s nos_${RUN}_nestnode_negofs.nc ]; then
#          cp -p nos_${RUN}_nestnode_negofs.nc $COMOUT/${PREFIXNOS}.nestnode.negofs.forecast.$PDY.t${cyc}z.nc
#       fi
#       if [ -s nos_${RUN}_nestnode_nwgofs.nc ]; then
#          cp -p nos_${RUN}_nestnode_nwgofs.nc $COMOUT/${PREFIXNOS}.nestnode.nwgofs.forecast.$PDY.t${cyc}z.nc
#       fi
#    fi
 

  elif [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    echo "forecast simulation began at:  `date`" >> $nosjlogfile
#    mpirun $EXECnos/selfe_${OFS} > $MODEL_LOG_FORECAST
    mpiexec -n ${TOTAL_TASKS} $EXECnos/selfe_${OFS} > $MODEL_LOG_FORECAST    
    export err=$?
    rm -f corms.now corms.fcst 
    if [ -s $DATA/mirror.out ]
    then
      grep "Run completed successfully" $DATA/mirror.out > corms.fcst
    fi
    if [ ! -s  corms.fcst ]
    then
       echo "${OFS} FORECAST RUN OF CYCLE t${HH}z ON $YYYY$MM$DD FAILED 00"  >> $cormslogfile 
       echo "FORECAST_RUN DONE 0"  >> $cormslogfile
       export err=99; err_chk
    else
       echo "${OFS} FORECAST RUN OF CYCLE t${HH}z ON $YYYY$MM$DD COMPLETED SUCCESSFULLY 100" >> $cormslogfile
       echo "FORECAST_RUN DONE 100"  >> $cormslogfile
    fi

    if [ $err -ne 0 ]
    then
      echo "Running ocean model for $RUNTYPE did not complete normally"
      msg="Running ocean model  for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running ocean model for $RUNTYPE completed normally"
      msg="Running ocean model  for $RUNTYPE completed normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
    fi
    echo "forecast simulation completed at:  `date`" >> $nosjlogfile
# combine field outputs into a single NetCDF file for forecast
    echo "combine field forecast simulation began at:  `date`" >> $nosjlogfile
    cd $DATA/outputs
    rm -f listyes listno
    I=1
#    while [ $I -le 100 ]
    while (( I < 100 ))
    do
      file=$I'_0000_elev.61'
      if [ -s $file ]; then
         echo $file >> listyes
      fi	 
      (( I = I + 1 ))
    done
    if [ -s listyes ]; then
       btmnow=`cat listyes | awk -F_ '{print $1}' | head -1`
       etmnow=`cat listyes | awk -F_ '{print $1}' | tail -1`
    fi
    if [ -s $FIXofs/$RUNTIME_COMBINE_NETCDF ]
    then
      if [ -s $FIXofs/$GRIDFILE_LL ]; then
        cp -p $FIXofs/$GRIDFILE_LL $DATA/outputs
      fi
      if [ -s $DATA/param.in ]; then
        cp -p $DATA/param.in $DATA/outputs
      else
        echo $DATA/param.in does not exist
        echo 'combine field netcdf code will fail !!'	
      fi
      if [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]; then
         ncastfcast="f"
      fi	 	
      	
      cat $FIXofs/$RUNTIME_COMBINE_NETCDF | sed -e s/BTM/$btmnow/g -e s/ETM/$etmnow/g \
      -e s/GRIDFILE/$GRIDFILE_LL/g \
      -e s/BASE_DATE/$BASE_DATE/g  \
      -e s/TIME_NOWCASTEND/${time_nowcastend}/g \
      -e s/RUNTYPE/${ncastfcast}/g > combine_output.in
      cp -p combine_output.in ${COMOUT}/$RUNTIME_COMBINE_NETCDF_FORECAST

    else
      msg="FATAL ERROR: No control file for combining field outputs of $RUNTYPE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo " "
      echo "*****************************************************************************"
      echo "*** FATAL ERROR : No control file for combining field outputs of $RUNTYPE ***"
      echo "*****************************************************************************"
      echo " "
      echo $msg
      seton
#      touch err.${OFS}.$PDY1.t${HH}z
      err_exit "No control file for combining field outputs of $RUNTYPE: $FIXofs/$RUNTIME_COMBINE_NETCDF"
    fi
 
#run netcdf combine executable
#    cp -p $EXECnos/nos_ofs_combine_field_netcdf_selfe $DATA/outputs 
     tar -cvf - . | cat >/dev/null 
     $EXECnos/nos_ofs_combine_field_netcdf_selfe $PREFIXNOS
     
    export err=$?
    if [ $err -ne 0 ]; then
      echo "Running nos_ofs_combine_netcdf_out_selfe for $RUNTYPE did not complete normally"
      msg="Running nos_ofs_combine_netcdf_out_selfe for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running nos_ofs_combine_netcdf_out_selfe for $RUNTYPE completed normally"
      msg="Running nos_ofs_combine_netcdf_out_selfe for $RUNTYPE completed normally"
      postmsg "$jlogfile" "$msg"
    fi
    echo "combine field forecast simulation completed at:  `date`" >> $nosjlogfile
# combine station outputs
    if [ -s ${COMOUT}/$RUNTIME_COMBINE_NETCDF_STA_FORECAST ]
    then
      echo "combine netcdf station control files exists"
      cp -p ${COMOUT}/$RUNTIME_COMBINE_NETCDF_STA_FORECAST $DATA/outputs/combine_station_output.in
    else
      msg="FATAL ERROR: No combine netcdf station control file"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*************************************************************'
      echo '*** FATAL ERROR : No combine netcdf station control file  ***'
      echo '*************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No combine NetCDF station control file: ${COMOUT}/$RUNTIME_COMBINE_NETCDF_STA_FORECAST"
    fi
    if [ -s $FIXofs/${STA_NETCDF_CTL} ]
    then
       cp -p $FIXofs/${STA_NETCDF_CTL} $DATA/outputs
    else
       echo "$FIXofs/${STA_NETCDF_CTL} does not rxist !!!"
       echo " combine station netCDF code will not run !!!"
       echo "Provide $FIXofs/${STA_NETCDF_CTL}"
    fi     
 
#run netcdf combine executable
    $EXECnos/nos_ofs_combine_station_netcdf_selfe <  ./combine_station_output.in
    export err=$?
    if [ $err -ne 0 ]; then
      echo "Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE did not complete normally"
      msg="Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE completed normally"
      msg="Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE completed normally"
      postmsg "$jlogfile" "$msg"
    fi
    if [ -s  Station.nc ]; then 
      cp -p  Station.nc ${DATA}/$STA_OUT_FORECAST
    fi
#    if [ -s  combinefields.nc ]; then 
#      cp -p  combinefields.nc ${COMOUT}/$HIS_OUT_FORECAST
#    fi
#    for combinefields in `ls ${DATA}/${PREFIXNOS}.creofs.fields.f*.nc`
#    do
#      cp -p ${combinefields} ${COMOUT}/.
#      cp -p ${combinefields} ${DATA}/.
#    done

##    create a status file for CO-OPS side
      rm -f ${OFS}.status
      YYYY=`echo $time_nowcastend | cut -c1-4 `
        MM=`echo $time_nowcastend |cut -c5-6 `
        DD=`echo $time_nowcastend |cut -c7-8 `
        HH=`echo $time_nowcastend |cut -c9-10 `
       echo $YYYY$MM$DD$HH > ${OFS}.status
       cp ${OFS}.status $COMOUT/.
       cp -p ${RUN}.status $COMOUT/${RUN}.status_${cyc}
  
  fi
#save 3D forecast field output file into COMOUT
  cd $DATA
  for combinefields in `ls ${DATA}/${PREFIXNOS}*.fields.f*.nc`
  do
     cp -p ${combinefields} ${COMOUT}/.
  done

#save 2D surface forecast field output file into COMOUT
  if [ -s ${DATA}/${PREFIXNOS}*.2ds.f003.nc ]; then
    for combinefields in `ls ${DATA}/${PREFIXNOS}*.2ds.f*.nc`
    do
      cp -p ${combinefields} ${COMOUT}/.
    done
  fi
#save forecast station output file into COMOUT
  cp -p $DATA/$STA_OUT_FORECAST  ${COMOUT}/$STA_OUT_FORECAST
  if [ -s $DATA/${PREFIXNOS}*.avg.forecast.nc ]; then
    cp -p $DATA/${PREFIXNOS}*.avg.f*.nc $COMOUT/
  fi 

fi 

# --------------------------------------------------------------------------- #
# 4.  Ending output

  setoff
  echo ' '
  echo "Ending nos_ofs_nowcast_forecast.sh at : `date`"
  echo ' '
  echo '                     *** End of NOS OFS NOWCAST/FORECAST SIMULATIONS ***'
  echo ' '

# End of NOS OFS Nowcast script ------------------------------------------- #

