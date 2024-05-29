# -*- coding: utf-8 -*-
"""
Created on Mon Oct  7 14:02:55 2019

@author: Alexander.Kurapov
"""

# Version 2: add spreading

import datetime as dt  # Python standard library datetime  module
import sys
import os
import numpy as np
from write_obs import write_obs
from get_adt import get_adt
import akPy
import wcofs_lonlat_2_xy as wcofs
#import matplotlib.pyplot as plt

#yyyymmdd string, the date of the DA cycle:
ymdCycle = sys.argv[1]

outDir = os.environ["DATA"]+"/ObsFiles/"
grdfile = os.environ["FIXofs"] + "/" + os.environ["GRIDFILE"]
dtSec=float(os.environ["DELT_MODEL"])
offsetHr = int(os.environ["cyc"])
bdate=os.environ["BASE_DATE"]
Nsur = int(os.environ["KBm"])
errSSH = float(os.environ["ERR_SSH"])
len_da=int(os.environ["LEN_DA"])
romsRefDate = dt.datetime(int(bdate[0:4]), int(bdate[4:6]), int(bdate[6:8]),0,0)
#offsetHr = 3 # hours, the offset for daily assimilation cycles, w/ resp to the 
             # beginning of the day

#romsRefDate = dt.datetime(2016,1,1,0,0)
#dtSec=90. # ROMS b/c time step (in sec). Data will be assigned at the closest 
          # model time instance
#errSSH = 0.01 # m
#Nsur = 40

spreadSSH = True
spreadDistHrs = 12
spreadStepHrs = 3

#^^^ END INPUTS ^^^

# python date objects
dCycle = dt.datetime.strptime(ymdCycle,'%Y%m%d') 
dEND = dCycle.replace(hour = offsetHr)
dSTR = dEND - dt.timedelta(hours = len_da)
print("start date/time: " + dSTR.strftime("%Y-%m-%d %H:%M:%S"))
print("  end date/time: " + dEND.strftime("%Y-%m-%d %H:%M:%S"))

stamp = dSTR.strftime("%Y%m%dt%Hz") + "-" + dEND.strftime("%Y%m%dt%Hz")
stamp = dEND.strftime("%Y%m%d.t%Hz")
#if spreadSSH:
#    stamp += "_spread"

if not os.path.exists(outDir):
    os.makedirs(outDir)
outfile = outDir + "/adt_obs_" + stamp + ".nc"

# read grid:
grd = akPy.readncvars(grdfile,['lon_rho','lat_rho','mask_rho','h'])
xy = wcofs.wcofs_lonlat_2_xy(grd['lon_rho'],grd['lat_rho'],1)
grd['x_rho']=xy['x']
grd['y_rho']=xy['y']

# Prepare data as a dict object D, with entries:
# obs_type, obs_value, obs_lon, obs_lat, obs_depth, obs_time, obs_provenance
# Note: D['survey_time'] and D['Nobs'] are empty
# Note: get_adt assigns the mean alongtrack time to obs_time
D = get_adt(dSTR,dEND,grd,dtSec,romsRefDate,spreadSSH,spreadDistHrs,spreadStepHrs)

# sort all elements of dict D based on key 'obs_time'
D = akPy.sortDict(D,'obs_time')

# Arrange the data in surveys, add Nobs and survey_time to D
# This also sorts out data by time, and within each survey - by type
# (generic for any data type)
D = akPy.add_surveyTime_Nobs(D) # adds / upgrades D['survey_time'] and D['Nobs']

# adds obs_Xgrid, obs_Ygrid to D
D = akPy.obs_ijpos(D,grd)

# add obs variance:
D['obs_variance'] = np.zeros(7,)
D['obs_variance'][1] = errSSH*errSSH
D['obs_error'] = errSSH*errSSH*np.ones(D['obs_value'].shape)

# add z coord info:
D['obs_depth'] = Nsur * np.ones(D['obs_value'].shape)
D['obs_Zgrid'] = np.zeros(D['obs_value'].shape)

# Create the output file and write S:
print ('writing output...')
write_obs(outfile,D,romsRefDate)
#
##akPy.plot_granules(D,grd)
#akPy.plot_granules(S,grd)
##plt.close('all')

# PLOTTING:
#fig = plt.figure()
#mngr = plt.get_current_fig_manager()
#mngr.window.setGeometry(-1000,100,600,900)
#ax = fig.add_axes([0.1, 0.1, 0.8, 0.8])
#
#ax.plot(D['obs_lon'],D['obs_lat'],'.')
#plt.show()
