#!/bin/sh
#  Script Name:  nos_ofs_rename.sh
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
set -xa
OFS=$1
OCEAN_MODEL=$2
RUNTYPE=$3
FILETYPE=$4
time_hotstart=$5
time_nowcastend=$6

echo "Starting nos_ofs_rename.sh at : `date`"
###############################################################################
YYYY=`echo $time_nowcastend | cut -c1-4`
MM=`echo $time_nowcastend | cut -c5-6`
DD=`echo $time_nowcastend | cut -c7-8`
cyc=`echo $time_nowcastend | cut -c9-10`
cycle=t${cyc}z
day=$YYYY$MM$DD
if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]; then 
  if [ $FILETYPE == '2d' -o $FILETYPE == '2D' ]; then
    filehead=${PREFIXNOS}.${cycle}.${day}.2ds.n
  elif [ $FILETYPE == '3d' -o $FILETYPE == '3D' ]; then
    filehead=${PREFIXNOS}.${cycle}.${day}.fields.n
  fi
elif [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]; then
  if [ $FILETYPE == '2d' -o $FILETYPE == '2D' ]; then
    filehead=${PREFIXNOS}.${cycle}.${day}.2ds.f
  elif [ $FILETYPE == '3d' -o $FILETYPE == '3D' ]; then
    filehead=${PREFIXNOS}.${cycle}.${day}.fields.f
  fi
fi
filetail='nc'

if [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]; then
  if [ $FILETYPE == '2d' -o $FILETYPE == '2D' ]; then
    NFILE=`ls -al *${OFS}*_surface_????.nc | wc -l`
    if [ $NFILE -gt 0 ]; then
      echo $NFILE > tmp.out
      ls -al *${OFS}*_surface_????.nc | awk '{print $NF}' >> tmp.out
    fi
  elif [ $FILETYPE == '3d' -o $FILETYPE == '3D' ]; then
    NFILE=`ls -al *${OFS}_????.nc | wc -l`
    if [ $NFILE -gt 0 ]; then
      echo $NFILE > tmp.out
      ls -al *${OFS}_????.nc | awk '{print $NF}' >> tmp.out
    fi
  fi 
elif [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]; then
  if [ $FILETYPE == '2d' -o $FILETYPE == '2D' ]; then
     NFILE=`ls -al *${OFS}*.surface.${RUNTYPE}*_????.nc | wc -l`
     if [ $NFILE -gt 0 ]; then
       echo $NFILE > tmp.out
       ls -al *${OFS}*.surface.${RUNTYPE}*_????.nc  | awk '{print $NF}' >> tmp.out
     fi

  elif [ $FILETYPE == '3d' -o $FILETYPE == '3D' ]; then
     NFILE=`ls -al *${OFS}*.fields.${RUNTYPE}*_????.nc | wc -l`
     if [ $NFILE -gt 0 ]; then
       echo $NFILE > tmp.out
       ls -al *${OFS}*.fields.${RUNTYPE}*_????.nc  | awk '{print $NF}' >> tmp.out
     fi
  fi
fi

echo ${OFS} > Fortran_rename.ctl
echo $OCEAN_MODEL  >> Fortran_rename.ctl
echo $filehead  >> Fortran_rename.ctl
echo $filetail  >> Fortran_rename.ctl
echo $time_hotstart >> Fortran_rename.ctl
echo $time_nowcastend >> Fortran_rename.ctl
cat Fortran_rename.ctl  tmp.out > input.ctl
$EXECnos/nos_ofs_rename < input.ctl > nos_ofs_rename.log
export err=$?
if [ $err -ne 0 ]; then
  echo "Running $EXECnos/nos_ofs_rename did not complete normally, FATAL ERROR!"
  msg="Running $EXECnos/nos_ofs_rename did not complete normally, FATAL ERROR!"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
  err_chk
else
  echo "Running $EXECnos/nos_ofs_rename completed normally"
  msg="Running $EXECnos/nos_ofs_rename  completed normally"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
fi

# --------------------------------------------------------------------------- #
# 4.  Ending output

  echo ' '
  echo "Ending nos_ofs_rename.sh at : `date`"
  echo ' '
  echo ' '
