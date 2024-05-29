#!/bin/sh
#  Script Name:  nos_ofs_shortwave_longwave_airpressure.sh
#  Purpose:                                                                    #
#  This script is called only for "forecast" met generation when two           # 
#  DBASE_MET_FORE are used.  In the main control file, there is a ":" to       #
#  seperate the first and second DBASE. The first DBASE generated netcdf file  # 
#  is too short in time, but has full set of required variables.  The second   #
#  file is long enough, but does not have the required short_wave, long_wave   # 
#  and air_pressure. This script uses Parkinson and Washington, 1979 formulars # 
#  to calculate short_wave and long_wave for the second file. The air_pressure #
#  in the second file takes the same value of the first when the value is      #
#  available, and takes the last value from the first for the rest of time.
#
#
# * Parkinson, C. L., and W. M. Washington, 1979: A large-scale numerical model 
#  of sea ice. J. Geophys. Res.,84, 311-337.
#      
#
#
#  Child scripts :                                                            #
#                                                                             #
#  The utililty script used:                                                  #
#                                                                             #
# Remarks :                                                                   #
# - For non-fatal errors output is written to the *.log file.
# - if the second DBASE (like GFS) generated netcdf file already has all      
#   required variables,  this script should not be called.  Rather, 
#   nos_ofs_met_forecast_direct_merge.sh needs to be called                   #
#                                                                             #
# Language:  C shell script
# 
# Input:
#     $OUTPUTFILE (nos.lXXofs.met.forecast.yyyymmdd.tccz.nc2)                 #
#     $OUTPUTFILE1(nos.lXXofs.hflux.forecast.yyyymmdd.tccz.nc2)               #
#     $MET_NETCDF_1_FORECAST:1" (nos.lxxofs.met.forecast.yyyymmdd.tccz.nc1)
#     $MET_NETCDF_1_NOWCAST (nos.lxxofs.met.nowcast.yyyymmdd.tccz.nc          #

# Output:
#    $MET_NETCDF_1_FORECAST (new nos.lxxofs.met.forecast.yyyymmdd.tccz.nc)   #
#    $MET_NETCDF_2_FORECAST (new nos.lxxofs.hflux.forecast.yyyymmdd.tccz.nc) #

# Technical Contact:    Aijun Zhang         Org:  NOS/CO-OPS                  #
#                        Phone: (240) 533-0591	                              #
#                        E-Mail: aijun.zhang@noaa.gov                         #
#
#

########################################################################


MET_NETCDF_1=$OUTPUTFILE
MET_NETCDF_2=$OUTPUTFILE1

#echo "======== mmgp MET_NETCDF_1 ==" $MET_NETCDF_1
#echo "======== mmgp MET_NETCDF_2 ==" $MET_NETCDF_2

##  find the second met fore length
#num=$(ncdump -h $MET_NETCDF_1 | grep UNLIMI | cut -b 25- | cut -b -2 |  awk '{print $1}')
num=$(ncdump -h $MET_NETCDF_1 | grep UNLIMI | awk -F"(" '{print $2}' | awk -F" " '{print $1}')
nm=$(($num-1))

##  find the met now length
#numnow=$(ncdump -h $MET_NETCDF_1_NOWCAST | grep UNLIMI | cut -b 25- | cut -b -2 |  awk '{print $1}')
numnow=$(ncdump -h $MET_NETCDF_1_NOWCAST | grep UNLIMI | awk -F"(" '{print $2}' | awk -F" " '{print $1}')
nmnow=$(($numnow-1))


for k in net_heat_flux short_wave long_wave air_pressure ; do
#    ncks -v $k -d time,9 $MET_NETCDF_1_NOWCAST nnn.nc
    ncks -v $k -d time,$nmnow $MET_NETCDF_1_NOWCAST nnn.nc
    ncks -v air_temperature $MET_NETCDF_1  fff.nc
    ncrename -v  air_temperature,$k fff.nc

    for (( i=0; i<=$nm; i++ )) ;  do
         ncks -v $k -d time,$i fff.nc fff$(printf %04d $i).nc
         model_time=$(ncdump -v time fff$(printf %04d $i).nc  | sed -e '1,/data:/d' -e '$d' | grep 'time =' | cut -f2- -d=   | awk '{print $1}')
         ncap2 -s "time=time*0.0+$model_time" nnn.nc  nnn$(printf %04d $i).nc
         ncks -A -v $k  nnn$(printf %04d $i).nc  fff$(printf %04d $i).nc > foo.txt 2>&1
    done

    ncrcat  fff0* final.nc
    ncks -A final.nc $MET_NETCDF_1
    ncks -A final.nc $MET_NETCDF_2
    rm ff*.nc nn*.nc final.nc

done

ncatted -h -a history,global,d,, -a history_of_appended_files,global,d,,  $MET_NETCDF_1 fnew1
ncatted -h -a history,global,d,, -a history_of_appended_files,global,d,,  $MET_NETCDF_2 fnew2
cp fnew1 $MET_NETCDF_1
cp fnew2 $MET_NETCDF_2
rm fnew1 fnew2

stefan=567.0e-10
tk=273.15
ncap2 -s "long_wave=$stefan*(air_temperature+$tk)^4*(1.0-0.261*exp(-7.77e-4*($tk-(air_temperature+$tk))^2))*(1.0+0.275*cloud_cover)" $MET_NETCDF_1 fnew0

ncks -v long_wave fnew0 longwave
cp fnew0 fnew1

amsec=3600000.0
pi=3.1415926
rad=0.0174533  # pi/180.0

num=$(ncdump -h fnew1 | grep UNLIMI | cut -b 25- | cut -b -3 |  awk '{print $1}')
nm=$(($num-1))

for (( i=0; i<=$nm; i++ )) ;  do
    ncks -d time,$i fnew1 fff$(printf %04d $i).nc
    month=$(ncdump -v Times fff$(printf %04d $i).nc | grep -A 2 -B 2 "Times =" | cut -b 9- | cut -b -2 |  awk '{print $1}')
    day=$(ncdump -v Times fff$(printf %04d $i).nc | grep -A 2 -B 2 "Times =" | cut -b 12- | cut -b -2 |  awk '{print $1}')
    echo "==== month day ===" $month $day
    if [ $month == "01" ]; then
       date=$day
    elif [ $month == "02" ]; then
       date=`expr $day + 31`
    elif [ $month == "03" ]; then
       date=`expr $day + 59`
    elif [ $month == "04" ]; then
       date=`expr $day + 90`
     elif [ $month == "05" ]; then
       date=`expr $day + 120`
     elif [ $month == "06" ]; then
       date=`expr $day + 151`
     elif [ $month == "07" ]; then
       date=`expr $day + 181`
     elif [ $month == "08" ]; then
       date=`expr $day + 212`
     elif [ $month == "09" ]; then
       date=`expr $day + 243`
     elif [ $month == "10" ]; then
       date=`expr $day + 273`
     elif [ $month == "11" ]; then
       date=`expr $day + 304`
     elif [ $month == "12" ]; then
       date=`expr $day + 334`
     fi
    echo "=date=" $date
    timet=$(ncdump -v Itime2 fff$(printf %04d $i).nc | grep "Itime2 =" | cut -b 11- |   awk '{print $1}')
    ncap2 -s "cosz=sin(lat*$rad)*sin(23.44*cos((172.0-$date)*2.0*$pi/365.0)*$rad)+cos(lat*$rad)*cos(23.44*cos((172.0-$date)*2.0*$pi/365.0)*$rad)*cos(($timet/$amsec+12.0*lon/180.0-12.0)*$pi/12.0)" fff$(printf %04d $i).nc  kkk$(printf %04d $i).nc
    cdo -expr,'cosz=(cosz<0.0)?0.0:cosz;' kkk$(printf %04d $i).nc kkkk$(printf %04d $i).nc > trash 2>&1
    ncks -A kkkk$(printf %04d $i).nc  kkk$(printf %04d $i).nc  > trash 2>&1

   if [ -s kkkk$(printf %04d $i).nc ]; then
    rm  kkkk$(printf %04d $i).nc
   fi

    ncap2 -s  "where(SPQ < 0.0) SPQ = 0.005" kkk$(printf %04d $i).nc
    ncap2 -s "sw0=1353.0*cosz^2/((cosz+2.7)*(1.e5*SPQ /(0.622 + 0.378*SPQ))*1.e-5 +1.085*cosz+0.1)"  kkk$(printf %04d $i).nc     kkkk$(printf %04d $i).nc > trash 2>&1
    rm kkk$(printf %04d $i).nc
    cdo -expr,'sw0=(sw0<0.0)?0.0:sw0;' kkkk$(printf %04d $i).nc kkk$(printf %04d $i).nc > trash 2>&1
    ncks -A  kkk$(printf %04d $i).nc  kkkk$(printf %04d $i).nc  > trash 2>&1
    ncap2 -s "short_wave= sw0*(1.0-0.6*cloud_cover^3)" kkkk$(printf %04d $i).nc  kkkkk$(printf %04d $i).nc
    ncks -v short_wave  kkkkk$(printf %04d $i).nc  kkkkkk$(printf %04d $i).nc
    ncap2 -s  "where(short_wave > 1200.0) short_wave = 1200.0" kkkkkk$(printf %04d $i).nc
    ncap2 -s  "where(short_wave < 0.0) short_wave = 0.0" kkkkkk$(printf %04d $i).nc
    rm  kkkkk$(printf %04d $i).nc kkkk$(printf %04d $i).nc
done

ncrcat  kkkkkk*.nc final.nc
ncks -A -v short_wave  final.nc $MET_NETCDF_1  > trash 2>&1 
ncks -A -v long_wave  longwave $MET_NETCDF_1  > trash 2>&1
ncatted -h -a history,global,d,, -a history_of_appended_files,global,d,,  $MET_NETCDF_1  fnew2

#rm final.nc kkkk*  nn* ff*    
rm final.nc kkk*   ff*


filehrrr=$MET_NETCDF_1_FORECAST"1"
filendfd=fnew2
num=$(ncdump -h $filehrrr | grep UNLIMI | awk -F"(" '{print $2}' | awk -F" " '{print $1}')
#num=$(ncdump -h $filehrrr | grep UNLIMI | cut -b 25- | cut -b -2 |  awk '{print $1}')
nm=$(($num-1))
#num2=$(ncdump -h $filendfd | grep UNLIMI | cut -b 25- | cut -b -2 |  awk '{print $1}')
num2=$(ncdump -h $filendfd | grep UNLIMI | awk -F"(" '{print $2}' | awk -F" " '{print $1}')
nm2=$(($num2-1))
echo "== nm ==" $nm
echo "== nm2 ==" $nm2
#la=`expr $nm - 1`
la=$nm
nstart=0
nned=0
for k in  air_temperature ; do
   ncks -v $k -d time,0 $filehrrr  first.nc
   first=$(ncdump -v time first.nc  | sed -e '1,/data:/d' -e '$d' | grep 'time =' | cut -f2- -d=   | awk '{print $1}')
   ncks -v $k -d time,$la $filehrrr  last.nc
   last=$(ncdump -v time last.nc  | sed -e '1,/data:/d' -e '$d' | grep 'time =' | cut -f2- -d=   | awk '{print $1}')
   for (( i=0; i<=$nm2; i++ )) ; do
       ncks -v $k -d time,$i $filendfd  ndfd$(printf %04d $i).nc
       time_ndfd=$(ncdump -v time ndfd$(printf %04d $i).nc  | sed -e '1,/data:/d' -e '$d' | grep 'time =' | cut -f2- -d=   | awk '{print $1}')
       if [ $time_ndfd == $first ]; then
           nstart=$i
       fi
       if [ $time_ndfd == $last ]; then
           nend=$i
       fi
   done
done

rm ndfd*.nc first.nc last.nc 
nend=`expr $nend + 1`
nsecond=`expr $nm2 - $nend`

ncks -d time,0,$la  $filehrrr firstpart.nc
ncks -d time,$nend,$nm2  $filendfd secondpart.nc

k=air_pressure

ncks -v $k -d time,$la  firstpart.nc nnn.nc
ncks -x -v $k  secondpart.nc  second_nopressure.nc
ncks -v $k secondpart.nc  fff.nc

for (( i=0; i<=$nsecond; i++ )) ;  do
      ncks -v $k -d time,$i fff.nc fff$(printf %04d $i).nc
      model_time=$(ncdump -v time fff$(printf %04d $i).nc  | sed -e '1,/data:/d' -e '$d' | grep 'time =' | cut -f2- -d=   | awk '{print $1}')
      ncap2 -s "time=time*0.0+$model_time" nnn.nc  nnn$(printf %04d $i).nc
      ncks -A -v $k  nnn$(printf %04d $i).nc  fff$(printf %04d $i).nc > foo.txt 2>&1
done

ncrcat  fff0* second_pressure.nc
ncks -A second_pressure.nc second_nopressure.nc
cp second_nopressure.nc second_all.nc

ncrcat  firstpart.nc  second_all.nc final.nc

cp  final.nc  $MET_NETCDF_1_FORECAST
cp  final.nc  $MET_NETCDF_2_FORECAST


#rm fff* kkk* nnn*
rm fff*  nnn*

echo "=== mmgp MET_NETCDF_1_FORECAST ==" $MET_NETCDF_1_FORECAST
echo "=== mmgp MET_NETCDF_2_FORECAST ==" $MET_NETCDF_2_FORECAST





