# -*- coding: utf-8 -*-
"""
Created on Wed Jul 24 10:44:22 2019

@author: Alexander.Kurapov
Modified by Zheng on 05/02/2023
"""

import datetime as dt  # Python standard library datetime module
#import matplotlib.pyplot as plt
import sys
import numpy as np
from write_obs import write_obs
from get_sst_L3 import get_sst_L3
import akPy
import wcofs_lonlat_2_xy as wcofs
import os

class sstSet:
    def __init__(self,satName,obsErr,prov):
        self.satName = satName
        self.obsErr = obsErr
        self.provenance = prov

outDir = os.environ["DATA"]+"/ObsFiles/"
grdfile = os.environ["DATA"]+"/"+os.environ["GRIDFILE"]
offsetHr = int(os.environ["cyc"])
bdate=os.environ["BASE_DATE"]
dtSec=float(os.environ["DELT_MODEL"])
Nsur = int(os.environ["KBm"])
#errT = float(os.environ["ERR_TEMP"])
len_da=int(os.environ["LEN_DA"])

#yyyymmdd string, the date of the DA cycle:
ymdCycle = sys.argv[1]
#offsetHr = 3 # hours, the offset for daily assimilation cycles, w/ resp to the 
             # beginning of the day

romsRefDate = dt.datetime(int(bdate[0:4]), int(bdate[4:6]), int(bdate[6:8]),0,0)

#sstSets = {sstSet('npp',0.4,11),
#           sstSet('n20',0.4,12),
#           sstSet('gta',0.5,13)}

# Zheng add '20230502'
sstSets = {sstSet('l3s',0.4,11),
           sstSet('gta',0.5,13)}
# Zheng add end '20230502'

#^^^ END INPUTS ^^^

# python date objects
dCycle = dt.datetime.strptime(ymdCycle,'%Y%m%d') 
dEND = dCycle.replace(hour = offsetHr)
dSTR = dEND - dt.timedelta(hours = len_da)
print("start date/time: " + dSTR.strftime("%Y-%m-%d %H:%M:%S"))
print("  end date/time: " + dEND.strftime("%Y-%m-%d %H:%M:%S"))

stamp = dEND.strftime("%Y%m%d.t%Hz")
if not os.path.exists(outDir):
    os.makedirs(outDir)
outfile = outDir + "/sst_super_obs_" + stamp + ".nc" 

# read grid:
grd = akPy.readncvars(grdfile,['lon_rho','lat_rho','mask_rho'])
xy = wcofs.wcofs_lonlat_2_xy(grd['lon_rho'],grd['lat_rho'],1)
grd['x_rho']=xy['x']
grd['y_rho']=xy['y']

# Prepare data as a dict object D, with entries:
# obs_type, obs_value, obs_lon, obs_lat, obs_depth, obs_time, obs_provenance
# Note: D['survey_time'] and D['Nobs'] are empty
D = get_sst_L3(dSTR,dEND,grd,dtSec,sstSets,romsRefDate)

# sort all elements of dict D based on key 'obs_time'
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
print('superobs...')
S = akPy.superobs_surf(D,grd)

# update survey_time and Nobs
S = akPy.add_surveyTime_Nobs(S)

# add obs variance:
S['obs_variance'] = np.zeros(7,)
S['obs_error']=np.zeros(S['obs_value'].shape)
for sat in sstSets:
    errT = sat.obsErr
    jjj=np.argwhere(S['obs_provenance']==sat.provenance).squeeze()
    S['obs_error'][jjj] = errT*errT

# add z coord info:
S['obs_depth'] = Nsur * np.ones(S['obs_value'].shape)
S['obs_Zgrid'] = np.zeros(S['obs_value'].shape)

# Create the output file and write S:
print ('writing output...')
print(outfile)
write_obs(outfile,S,romsRefDate)

#akPy.plot_granules(D,grd)
#akPy.plot_granules(S,grd)
#plt.close('all')
