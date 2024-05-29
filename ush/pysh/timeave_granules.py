#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Created on Thu Jun 18 14:18:28 2020

@author: Alexander.Kurapov
Modified by Zheng on 05/02/2023
"""

# Merge G18 granules in time:
# combine data in 3 hr intervals starting from time 3 hr

import numpy as np
import netCDF4 as n4
import os
import sys
import akPy
import datetime as dt
#import matplotlib as mplt
#import matplotlib.pyplot as plt

# Input argument #1, aka '20190724'
yyyymmdd = sys.argv[1]

yyyy = yyyymmdd[0:4]
thisDay = dt.datetime.strptime(yyyymmdd,'%Y%m%d')

outDir = os.environ["DATA"]+"/SST/" + yyyymmdd
outFile = outDir + '/' + yyyymmdd + '_sst_gta_NEP.nc'
inFile = outDir + '/' + yyyymmdd + '_sst_g18_NEP.nc'

if os.path.isfile(inFile):
   nc = n4.Dataset(inFile)
   t = nc.variables['time'][:] # in sec since obsRefDate
   timeUnits = nc.variables['time'].units
   sst_dtime = nc.variables['sst_dtime'][:]
   dtimeUnits = nc.variables['sst_dtime'].units
   sst = nc.variables['sea_surface_temperature'][:]
   sses_bias = nc.variables['sses_bias'][:]
   lon = nc.variables['lon'][:]
   lat = nc.variables['lat'][:]
   nc.close()
   obsRefDate = akPy.findDateInString(timeUnits)

   dd = (thisDay-obsRefDate).days
   thr = t / 3600 - 24*dd # now time is in hours, from 0 to 24

   nx=lon.size
   ny=lat.size
   ones2D = np.ones((ny,nx),dtype=np.int8) # to fill user_mask

   ngranules = 0

   # Define the output file:
   if os.path.isfile(outFile):
       os.remove(outFile)
                
   ncOUT = n4.Dataset(outFile,mode='w')

   ncOUT.createDimension('nx', nx)
   ncOUT.createDimension('ny', ny) 
   ncOUT.createDimension('time', None)

   lonVar = ncOUT.createVariable('lon',np.float32,('nx'))
   latVar = ncOUT.createVariable('lat',np.float32,('ny'))

   tVar = ncOUT.createVariable('time',np.float64, ('time',))
   tVar.units = timeUnits

   # Zheng add '20230502'
   dtimeVar = ncOUT.createVariable('sst_dtime',np.float64, ('time','ny','nx'))
   dtimeVar.units = dtimeUnits
   # Zheng add end '20230502'

   sstVar = ncOUT.createVariable \
   ('sea_surface_temperature',np.float32, ('time','ny','nx'))

   biasVar = ncOUT.createVariable \
   ('sses_bias',np.float32, ('time','ny','nx'))

   maskVar=ncOUT.createVariable('user_mask',np.int8, ('time','ny','nx'))

   # write lon and lat:
   lonVar[:] = lon
   latVar[:] = lat


   for HH in range(1,24,3):
       ii = np.argwhere(np.abs(thr-HH) <= 1.5).squeeze()
       print(HH)
       print(ii)
       if ii.size>0:
           if ii.size>1:
               sstNew=np.nanmean(sst[ii,:,:],axis=0)
               sbiasNew=np.nanmean(sses_bias[ii,:,:],axis=0)
               tNew=np.mean(t[ii]) # ave over original time

               # Zheng add '20230502'
               dtimeNew=np.nanmean(sst_dtime[ii,:,:],axis=0)
               # Zheng add end '20230502'

           else: # ii.size==1
               sstNew = sst[ii,:,:]
               sbiasNew = sses_bias[ii,:,:]
               tNew = t[ii]
        
           # write to the file:
           tVar[ngranules]=tNew
           sstVar[ngranules,:,:]=sstNew
           biasVar[ngranules,:,:]=sbiasNew
           maskVar[ngranules,:,:]=ones2D

           # Zheng add '20230502'
           dtimeVar[ngranules,:,:]=tNew
           # Zheng add end '20230502'

           ngranules+=1
           print('new granule: ' + str(ngranules))

   ncOUT.close()
else:
   print(('Warning: '+inFile+' does not exist!'))
#plt.figure()
#mngr = plt.get_current_fig_manager()
#mngr.window.setGeometry(50,50,700,545)    

#for k in range(ii.size):
#    plt.subplot(1,ii.size+1,k+1)
#    plt.pcolormesh(lon,lat,sst[ii[k],:,:],cmap="jet", vmin=8, vmax=25)
#
#plt.subplot(1,ii.size+1,4)
#plt.pcolormesh(lon,lat,sstNew,cmap="jet", vmin=8, vmax=25)
#
#plt.show()
