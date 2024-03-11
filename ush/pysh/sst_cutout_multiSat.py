#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 10 11:19:49 2019

@author: Alexander.Kurapov
Modified by Jiangtao on 01/30/2023
Modified by Zheng on 05/02/2023
"""

# import datetime as dt  # Python standard library datetime  module
import numpy as np
import netCDF4 as n4
import os
import sys
import datetime as dt
from urllib.parse import urlparse
import glob

def uri_validator(x):
    try:
        result = urlparse(x)
        return all([result.scheme, result.netloc, result.path])
    except:
        return False

# Input argument #1, aka '20190724'
yyyymmdd = sys.argv[1]

# Input argument #2, sat product name
# Choices: npp, n20, g18
#satName=sys.argv[2]

# Zheng modify '20230502', replacing npp and n20 with l3s
#sats=["npp","n20","g18"]
sats=["g18","l3s"]
# Zheng modify end '20230502' 

# url option:
d = dt.datetime.strptime(yyyymmdd,'%Y%m%d')
dJan1=dt.datetime(d.year,1,1,0,0)
dayOfYear = (d-dJan1).days+1
#lon_cut=[-146, -111];
#lat_cut=[18, 57];
lon_cut=[float(os.environ["MINLON"]), float(os.environ["MAXLON"])];
lat_cut=[float(os.environ["MINLAT"]), float(os.environ["MAXLAT"])];
inDatDir = os.environ["DCOMINsst"] + "/" + yyyymmdd + "/sst/"
outDir = os.environ["DATA"]+"/SST/"+yyyymmdd

for satName in sats:
   #inDatDirHead = "https://www.star.nesdis.noaa.gov/thredds/dodsC/gridG17ABINRTL3CWW00/"
    #inDatDir = inDatDirHead + yyyymmdd[0:4] + '/' + str(dayOfYear) + '/' 
    if satName == "npp":
        ftail = "*-OSPO-L3U_GHRSST-SSTsubskin-VIIRS_NPP-ACSPO*.nc"
    elif satName == "n20":
        ftail = "*-OSPO-L3U_GHRSST-SSTsubskin-VIIRS_N20-ACSPO*.nc"
    elif satName == "g18":
        ftail = "*-STAR-L3C_GHRSST-SSTsubskin-ABI_G18-ACSPO*.nc"

#  Zheng add '20230502'
    elif satName == "l3s":
        ftail = "*-L3S_GHRSST-SSTsubskin-LEO_*M_*.nc"
#  Zheng add end '20230502'

    outfile = outDir + "/" + yyyymmdd + "_sst_" + satName + "_NEP.nc"

# Read the data, select the granules in the vivinity of the WCOFS domain, 
# add the granule to the file 
    if not os.path.exists(outDir):
        os.makedirs(outDir)

    read_lonlat = 1
    ngranules = 0

    files=glob.glob(inDatDir+ftail)
    for fname in files:
        print('checking ' + ' ' + fname)
        if ( os.path.isfile(fname) | uri_validator(fname) ):
            try: 
                nc  = n4.Dataset(fname)
                if read_lonlat == 1:
                    lon = nc.variables['lon'][:]
                    lat = nc.variables['lat'][:]
                    timeUnits = nc.variables['time'].units
                    read_lonlat = 0
                
                    # find indices for the cutout:
                    ii = np.where((lon >= lon_cut[0]) & (lon <= lon_cut[1])) # array object
                    jj = np.where((lat >= lat_cut[0]) & (lat <= lat_cut[1]))
                    ii = ii[0] # the 1st element of ii returned on previous lines
                    jj = jj[0] # is the array object, the set of velues is its 1st element
                    i1 = ii[0]
                    j1=  jj[0] 
                    nx = ii.size
                    ny = jj.size
                
                    lon = lon[ii]
                    lat = lat[jj]
                
                    # Define the output file
                    print('Define outfile: ' + outfile)
                    if os.path.isfile(outfile):
                        os.remove(outfile)
                
                    ncOUT = n4.Dataset(outfile,mode='w')
                    ncOUT.createDimension('nx', nx)
                    ncOUT.createDimension('ny', ny) 
                    ncOUT.createDimension('time', None)
                    lonVar = ncOUT.createVariable('lon',np.float32,('nx'))
                    latVar = ncOUT.createVariable('lat',np.float32,('ny'))
                    tVar = ncOUT.createVariable('time',np.float64, ('time',))
                    sstVar = ncOUT.createVariable \
                    ('sea_surface_temperature',np.float32, ('time','ny','nx'))
                    biasVar = ncOUT.createVariable \
                    ('sses_bias',np.float32, ('time','ny','nx'))
                    maskVar=ncOUT.createVariable('user_mask',np.int8, ('time','ny','nx'))
                
                    qlVar=ncOUT.createVariable('quality_level',np.int8, ('time','ny','nx'))
                    l2pVar=ncOUT.createVariable('l2p_flags',np.int16, ('time','ny','nx'))
                        
                    # Zheng add '20230502'
                    dtimeVar=ncOUT.createVariable \
                    ('sst_dtime',np.float64,('time','ny','nx'))
                    # Zheng add end '20230502'

                    # - add attributes:                
                    tVar.units = timeUnits
                    dtimeVar.units = timeUnits

                    # - write lon and lat
                    lonVar[:] = lon
                    latVar [:] = lat

                    ones2D = np.ones((ny,nx),dtype=np.int8)
                
                # end of read_lonlat == 1:     
            
                # for each file (nc  = n4.Dataset(fname)), 
                # determine if the granule bounding box intersects
                # the sample area defined by lon - lat
                # If so, read the subsample:
                sst = nc.variables['sea_surface_temperature'][0,jj,ii]
                if not np.all(sst.mask):
                    print('... sampled')
                    ngranules = ngranules + 1
                    sst = sst - 273.15
                    sst = np.ma.filled(sst,np.nan) # fill masked locations w nan
                    sst[np.less(sst,0,where=~np.isnan(sst))] = np.nan
                    sses_bias = nc.variables['sses_bias'][0,jj,ii]
                    sses_bias = np.ma.filled(sses_bias,np.nan)
                    ql = nc.variables['quality_level'][0,jj,ii]
                    l2p = nc.variables['l2p_flags'][0,jj,ii]
                    t = nc.variables['time'][:]
                    t = float(t)

                    # Zheng add '20230502'
                    dtime = nc.variables['sst_dtime'][0,jj,ii]
                    dtime = np.ma.filled(dtime,np.nan)
                    # Zheng add end '20230502'

                    if satName == "l3s":
                        tVar[ngranules-1] = t + np.nanmean(dtime)
                    elif satName == "g18":
                        tVar[ngranules-1] = t

                    sstVar[ngranules-1,:,:] = sst
                    biasVar[ngranules-1,:,:] = sses_bias
                    maskVar[ngranules-1,:,:] = ones2D
                    qlVar[ngranules-1,:,:] = ql
                    l2pVar[ngranules-1,:,:] = l2p

                    # Zheng add '20230502'
                    dtimeVar[ngranules-1,:,:] = dtime
                    # Zheng add end '20230502'

            except Exception as error:
                print(f"{error}")
            # end of try:                        
        # end of if file exist
    if ( os.path.isfile(outfile)):
        ncOUT.close()
