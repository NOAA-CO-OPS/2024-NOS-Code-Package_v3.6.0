set -ax
module load prod_util/2.0.13
PDY=`$NDATE  |cut -c 1-8`
#PDY=20240109
PDYm1=`$NDATE -24 |cut -c 1-8`
PDYm2=`$NDATE -48 |cut -c 1-8`
#PDY=`/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.5/exec/ips/ndate |cut -c 1-8`;
#PDYm1=`/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.5/exec/ips/ndate -24 |cut -c 1-8`;
#PDYm2=`/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.5/exec/ips/ndate -48 |cut -c 1-8`;
echo PDY=$PDY PDYm1=$PDYm1 PDYm2=$PDYm2

SOURCE_ROOT=/lfs/h1/ops/prod/com/nosofs/v3.5
#SOURCE_ROOT=/lfs/h1/nos/ptmp/$LOGNAME/com/nosofs/v3.6
#SOURCE_ROOT=/lfs/h1/nos/ptmp/nos.nosofs/com/nosofs/v3.6

TARGET_ROOT=/lfs/h1/nos/ptmp/$LOGNAME/com/nosofs/v3.6
#TARGET_ROOT=/lfs/h1/ops/para/com/nosofs/v3.5

#devname=$(cat /lfs/h1/ops/prod/config/prodmachinefile|grep backup|cut -d : -f2)
#prodname=$(cat /lfs/h1/ops/prod/config/prodmachinefile|grep primary|cut -d : -f2)
prodname=$(grep -i primary  /lfs/h1/ops/prod/config/prodmachinefile | cut -d":" -f2)
devname=$(grep -i backup  /lfs/h1/ops/prod/config/prodmachinefile | cut -d":" -f2)


echo $prodname $devname
devname=`echo $devname | cut -c 1-1`
prodname=`echo $prodname | cut -c 1-1`
SOURCE_IP=${prodname}login08
SOURCE_IP=dlogin08

echo " ***************************************"
echo "copying NGOFS,NEGOFS,NWGOFS,SFBOFS restart file/open boundary forcing condition files "
echo " from productions ... "
echo " ***************************************"

echo "copying cbofs dbofs tbofs ciofs sfbofs creofs ngofs2 from prod com... "
# copy restart files from development computer
#for OFS in creofs; do
for OFS in cbofs dbofs tbofs ciofs sfbofs creofs ngofs2 ; do
  SOURCE=${SOURCE_ROOT}/${OFS}.$PDY
  TARGET=$TARGET_ROOT/${OFS}.$PDY

  mkdir -p -m 775 $TARGET

if ssh ${SOURCE_IP} "ls -al ${SOURCE}/*.rst.nowcast* " ; then
  latest_restart_f=`ssh ${SOURCE_IP} "ls -al ${SOURCE}/*.rst.nowcast* "  | tail -1 | awk '{print $NF}' `
  cycle=` echo $latest_restart_f | awk -F '.' '{print $(NF-1)} ' `
  datenow=` echo $latest_restart_f | awk -F '.' '{print $(NF-2)} ' `
  tailf=` echo $latest_restart_f | awk -F '.' '{print $NF} ' `
  scp -p ${SOURCE_IP}:$latest_restart_f $TARGET/${OFS}.${cycle}.${datenow}.rst.nowcast.$tailf  
else
  echo no restart file is not found
fi     

#  scp -p ${SOURCE_IP}:${SOURCE}/*.rst.nowcast* $TARGET/.
#  scp -p ${SOURCE_IP}:${SOURCE}/*.obc* $TARGET/.
if [ $OFS == "creofs" ]; then
  scp -p ${SOURCE_IP}:${SOURCE}/nos.${OFS}.obc.${datenow}.${cycle}.tar $TARGET/${OFS}.${cycle}.${datenow}.obc.tar
else
  scp -p ${SOURCE_IP}:${SOURCE}/nos.${OFS}.obc.${datenow}.${cycle}.$tailf $TARGET/${OFS}.${cycle}.${datenow}.obc.$tailf
fi
done


echo "copying gomofs wcofs wcofs_da wcofs_free from prod com..."
for OFS in gomofs wcofs wcofs_da wcofs_free ; do
  SOURCE=${SOURCE_ROOT}/${OFS}.$PDY
  TARGET=$TARGET_ROOT/${OFS}.$PDY
  mkdir -p -m 775 $TARGET
if ssh ${SOURCE_IP} "ls -al ${SOURCE}/*.rst.nowcast* " ; then
  latest_restart_f=`ssh ${SOURCE_IP} "ls -al ${SOURCE}/*.rst.nowcast* "  | tail -1 | awk '{print $NF}' `
  cycle=` echo $latest_restart_f | awk -F '.' '{print $(NF-1)} ' `
  datenow=` echo $latest_restart_f | awk -F '.' '{print $(NF-2)} ' `
  tailf=` echo $latest_restart_f | awk -F '.' '{print $NF} ' `
  scp -p ${SOURCE_IP}:$latest_restart_f $TARGET/${OFS}.${cycle}.${datenow}.rst.nowcast.$tailf
#  scp -p ${SOURCE_IP}:$latest_restart_f $TARGET/.

else
  echo no restart file is not found
fi
scp -p ${SOURCE_IP}:${SOURCE}/nos.${OFS}.obc.${datenow}.${cycle}.$tailf $TARGET/${OFS}.${cycle}.${datenow}.obc.$tailf
scp -p ${SOURCE_IP}:${SOURCE}/nos.${OFS}.clim.${datenow}.${cycle}.$tailf $TARGET/${OFS}.${cycle}.${datenow}.clim.$tailf

done

#

echo "scp leofs lmhofs loofs lsofs from NOS developement area ..."
for OFS in leofs lmhofs loofs lsofs ; do   # copy restart files from developmental runs due to activating ice module
   SOURCE_ROOT=/lfs/h1/nos/ptmp/$LOGNAME/com/nosofs/v3.7

   SOURCE=${SOURCE_ROOT}/${OFS}.$PDY
   TARGET=$TARGET_ROOT/${OFS}.$PDY
   SOURCEm1=${SOURCE_ROOT}/${OFS}.$PDYm1
   TARGETm1=$TARGET_ROOT/${OFS}.$PDYm1
   SOURCEm2=${SOURCE_ROOT}/${OFS}.$PDYm2
   TARGETm2=$TARGET_ROOT/${OFS}.$PDYm2

   mkdir -p -m 775 $TARGET $TARGETm1 $TARGETm2
   if ssh ${SOURCE_IP} "ls -al ${SOURCE}/*.rst.nowcast* " ; then
      latest_restart_f=`ssh ${SOURCE_IP} "ls -al ${SOURCE}/*.rst.nowcast* "  | tail -1 | awk '{print $NF}' `
      cycle=` echo $latest_restart_f | awk -F '.' '{print $(NF-1)} ' `
      datenow=` echo $latest_restart_f | awk -F '.' '{print $(NF-2)} ' `
      tailf=` echo $latest_restart_f | awk -F '.' '{print $NF} ' `
      scp -p ${SOURCE_IP}:$latest_restart_f $TARGET/${OFS}.${cycle}.${datenow}.rst.nowcast.$tailf
   else
      echo no restart file is not found
   fi

#   scp -p ${SOURCE_IP}:${SOURCE}/nos.${OFS}.obc.${datenow}.${cycle}.$tailf $TARGET/${OFS}.${cycle}.${datenow}.obc.$tailf
#   for cyc in 00 06 12 18 ; do
#      scp -p ${SOURCE_IP}:${SOURCE}/nos.${OFS}.stations.nowcast.${datenow}.t${cyc}z.nc $TARGET/${OFS}.t${cyc}z.${datenow}.stations.nowcast.nc
#   done
#   for day1 in $PDYm1 $PDYm2 ; do
#    for cyc in 00 06 12 18 ; do
#      scp -p ${SOURCE_IP}:${SOURCE_ROOT}/${OFS}.${day1}/nos.${OFS}.stations.nowcast.${day1}.t${cyc}z.nc $TARGET_ROOT/${OFS}.${day1}/${OFS}.t${cyc}z.${day1}.stations.nowcast.nc
#    done      
#   done   
    scp -pr ${SOURCE_IP}:${SOURCE}/*station*   $TARGET/.
#   scp -pr ${SOURCE_IP}:${SOURCE}/*rst*   $TARGET/.
    scp -pr ${SOURCE_IP}:${SOURCE}/*obc*   $TARGET/.
    scp -pr ${SOURCE_IP}:${SOURCEm1}/*station* $TARGETm1/.
    scp -pr ${SOURCE_IP}:${SOURCEm2}/*station* $TARGETm2/.
done

echo "scp sscofs from NOS developmental runs ..."
for OFS in sscofs ; do   
  SOURCE_ROOT=/lfs/h1/nos/ptmp/$LOGNAME/com/nosofs/v3.7

  SOURCE=${SOURCE_ROOT}/${OFS}.$PDY
  TARGET=$TARGET_ROOT/${OFS}.$PDY
  mkdir -p -m 775 $TARGET 
  if ssh ${SOURCE_IP} "ls -al ${SOURCE}/*.rst.nowcast* " ; then
    latest_restart_f=`ssh ${SOURCE_IP} "ls -al ${SOURCE}/*.rst.nowcast* "  | tail -1 | awk '{print $NF}' `
    cycle=` echo $latest_restart_f | awk -F '.' '{print $(NF-1)} ' `
    datenow=` echo $latest_restart_f | awk -F '.' '{print $(NF-2)} ' `
    tailf=` echo $latest_restart_f | awk -F '.' '{print $NF} ' `
    scp -p ${SOURCE_IP}:$latest_restart_f $TARGET/${OFS}.${cycle}.${datenow}.rst.nowcast.$tailf					    
  else
   echo no restart file is not found
 fi
 scp -pr ${SOURCE_IP}:${SOURCE}/*obc*   $TARGET/.
# scp -p ${SOURCE_IP}:${SOURCE}/nos.${OFS}.obc.${datenow}.${cycle}.nc $TARGET/${OFS}.${cycle}.${datenow}.obc.nc
done
						#

echo " ***************************************"
echo " ***************************************"





