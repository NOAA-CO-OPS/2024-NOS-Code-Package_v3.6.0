# -*- coding: utf-8 -*-
"""
Created on Mon Aug 12 11:58:57 2019

@author: Alexander.Kurapov
"""

import datetime as dt  # Python standard library datetime  module
#import matplotlib.pyplot as plt
import sys,getpass,os
import numpy as np
from write_obs import write_obs
from get_hf import get_hf
import akPy
import wcofs_lonlat_2_xy as wcofs

outDir = os.environ["DATA"]+"/ObsFiles/"
grdfile = os.environ["DATA"]+"/"+os.environ["GRIDFILE"]
offsetHr = int(os.environ["cyc"])
bdate=os.environ["BASE_DATE"]
dtSec=float(os.environ["DELT_MODEL"])
Nsur = int(os.environ["KBm"])
errUV = float(os.environ["ERR_V"]) # m/s. Note "obs_error" is the variance (errUV^2)
len_da=int(os.environ["LEN_DA"])
epsDOP = float(os.environ["EPSDOP"])
hmin = float(os.environ["HF_HMIN"])
#dtSec=90. # ROMS b/c time step (in sec). Data will be assigned at the closest 
          # model time instance
romsRefDate = dt.datetime(int(bdate[0:4]), int(bdate[4:6]), int(bdate[6:8]),0,0)
#yyyymmdd string, the date of the DA cycle:
ymdCycle = sys.argv[1]

#^^^ END INPUTS ^^^

# python date objects
dCycle = dt.datetime.strptime(ymdCycle,'%Y%m%d') 
dEND = dCycle.replace(hour = offsetHr)
dSTR = dEND - dt.timedelta(hours = len_da)
print("start date/time: " + dSTR.strftime("%Y-%m-%d %H:%M:%S"))
print("  end date/time: " + dEND.strftime("%Y-%m-%d %H:%M:%S"))

#stamp = dSTR.strftime("%Y%m%dt%Hz") + "-" + dEND.strftime("%Y%m%dt%Hz")
stamp = dEND.strftime("%Y%m%d.t%Hz")

outfile = outDir + "hf_obs_" + stamp + ".nc" 
if not os.path.exists(outDir):
    os.makedirs(outDir)

# read grid:
grd = akPy.readncvars(grdfile,['lon_rho','lat_rho','mask_rho','angle','h'])
xy = wcofs.wcofs_lonlat_2_xy(grd['lon_rho'],grd['lat_rho'],1)
grd['x_rho']=xy['x']
grd['y_rho']=xy['y']

# Prepare data as a dict object D, with entries:
# obs_type, obs_value, obs_lon, obs_lat, obs_depth, obs_time, obs_provenance
# Note: D['survey_time'] and D['Nobs'] are empty
D = get_hf(dSTR,dEND,grd,dtSec,romsRefDate,epsDOP,hmin)

## sort all elements of dict D based on key 'obs_time'
D = akPy.sortDict(D,'obs_time')

# Arrange the data in surveys, add Nobs and survey_time to D
# This also sorts out data by time, and within each survey - by type
# (generic for any data type)
D = akPy.add_surveyTime_Nobs(D) # adds / upgrades D['survey_time'] and D['Nobs']

# adds obs_Xgrid, obs_Ygrid to D
D = akPy.obs_ijpos(D,grd)
    
# S = superobs(D,grd)
# - rounding up the Xpos, Ypos indices yields indices of boxes where each 
# - observation belongs. E.g., Xpos between 0.5 and 1.5 belong to cell w  
# - X index 1 
#print('superobs...')
##S = akPy.superobs_surf(D,grd)
S=D

# update survey_time and Nobs
S = akPy.add_surveyTime_Nobs(S)

# add obs variance:
S['obs_variance'] = np.zeros(7,)
S['obs_variance'][3] = errUV*errUV
S['obs_variance'][4] = errUV*errUV
S['obs_error'] = errUV*errUV*np.ones(S['obs_value'].shape)

# add z coord info:
S['obs_depth'] = Nsur * np.ones(S['obs_value'].shape)
S['obs_Zgrid'] = np.zeros(S['obs_value'].shape)

# Create the output file and write S:
print ('writing output...')
write_obs(outfile,S,romsRefDate)
