#!/bin/sh

met1=$MET_NETCDF_1_FORECAST"1"
met2=$MET_NETCDF_1_FORECAST"2"

num1=$(ncdump -h $met1 | grep UNLIMI | awk -F"(" '{print $2}' | awk -F" " '{print $1}')
num2=$(ncdump -h $met2 | grep UNLIMI | awk -F"(" '{print $2}' | awk -F" " '{print $1}')
nm1=$(($num1-1))
nm2=$(($num2-1))
time0=$(ncks -v time -d time,${nm1} $met1 | sed -e '1,/data:/d' -e '$d' | grep 'time =' | cut -f2- -d=   | awk '{print $1}')

for k in $(seq 0 $nm2);
 do
 time=$(ncks -v time -d time,${k} $met2 | sed -e '1,/data:/d' -e '$d' | grep 'time =' | cut -f2- -d=   | awk '{print $1}')
   if (( $(echo "$time > $time0" |bc -l) )); then
        kk=$k
        break
    fi
done

 kkk=$(($kk+1))
ncea -F -d time,$kkk,$num2 $met2 met.03.nc
ncrcat $met1 met.03.nc met.all.nc
cp met.all.nc $MET_NETCDF_1_FORECAST
cp met.all.nc $MET_NETCDF_2_FORECAST
