# -*- coding: utf-8 -*-
"""
Created on Wed Aug 14 10:11:56 2019

@author: Alexander.Kurapov
"""
def  add_surveyTime_Nobs_DTcheck(D,DT):
    # Add survey_time and Nobs to dictionary DD
    # The difference from akPy.add_surveyTime_Nobs is that
    # individual surveys are checked whether they are close enough to each
    # other (closer than 0.5*DT, where DT is the specified threshhold, in
    # our immediate application - ROMS 4DVAR time step). If they are too close
    # - merge into one survey

    import numpy as np

    eps = 0.5*DT/24/3600

    t = D['obs_time']
    tu = np.unique(t)
    nu = tu.size
    for k in range(nu-1):
        if (tu[k+1] - tu[k] < eps):
            ii = np.argwhere(t==tu[k+1]).squeeze()
            t[ii] = tu[k]
            tu[k+1] = tu[k]

    tsurvey = np.unique(tu)
    Nobs = np.ndarray((tsurvey.size,),int) # number of obs in each survey
    for j in range(len(tsurvey)):
        ii = np.argwhere(t == tsurvey[j] ).squeeze()
        Nobs[j] = ii.size

    D['obs_time'] = t
    D['survey_time'] = tsurvey
    D['Nobs'] = Nobs

    return D

import sys
import datetime as dt  # Python standard library datetime  module
import numpy as np
import akPy
from write_obs import write_obs
import getpass
import os

obsDir = os.environ["DATA"]+"/ObsFiles/"
offsetHr = int(os.environ["cyc"])
bdate=os.environ["BASE_DATE"]
dtSec=float(os.environ["DELT_MODEL"])
len_da=int(os.environ["LEN_DA"])
ofs=os.environ["OFS"]
prefixnos=os.environ["PREFIXNOS"]
ymdCycle = sys.argv[1]
romsRefDate = dt.datetime(int(bdate[0:4]), int(bdate[4:6]), int(bdate[6:8]),0,0)
#offsetHr = 3 # hours, the offset for daily assimilation cycles, w/ resp to the 
             # beginning of the day
#DT=90
#obsDir = "/gpfs/dell1/ptmp/"+getpass.getuser()+"/Obs/ObsFiles/"             

# time stamps:
dCycle = dt.datetime.strptime(ymdCycle,'%Y%m%d') 
dEND = dCycle.replace(hour = offsetHr)
dSTR = dEND - dt.timedelta(hours = len_da)
print("start date/time: " + dSTR.strftime("%Y-%m-%d %H:%M:%S"))
print("  end date/time: " + dEND.strftime("%Y-%m-%d %H:%M:%S"))

#stamp = dSTR.strftime("%Y%m%dt%Hz") + "-" + dEND.strftime("%Y%m%dt%Hz")
stamp = dEND.strftime("%Y%m%d.t%Hz")
outfile = obsDir + prefixnos  + ".obs." + stamp + ".nc"

# File list, obsfile:
# NOTE: TO MAKE SURE data within each survey are sorted by obs_type
# in obsfile provide altimetry first, then HF, then SST
obsfile = np.array([obsDir + "adt_obs_" + stamp + ".nc",
           obsDir + "hf_obs_" + stamp + ".nc",
           obsDir + "sst_super_obs_" + stamp + ".nc"])
nk=np.zeros((1,0))
for k in range(obsfile.size):
   if not os.path.isfile(obsfile[k]):
      print("WARNING "+obsfile[k]+" does not exist!")
      nk=np.append(nk,k)
      nk=nk.astype(np.int32)
if nk.size:
      obsfile = np.delete(obsfile,nk)
if not obsfile.size:
      print("FATAL ERROR: No obs files available!")
      sys.exit()

# exclude 'Nobs', 'survey_time', 'obs_variance' from list, recompute later:
varlist = np.array(['obs_type', 
                    'obs_provenance', 'obs_time', 'obs_lon', 'obs_lat', 
                    'obs_depth', 'obs_Xgrid', 'obs_Ygrid', 'obs_Zgrid', 
                    'obs_error', 'obs_value'])   

DD = akPy.readncvars(obsfile[0],varlist)
obs_var = akPy.readncvars(obsfile[0],np.array(['obs_variance']))['obs_variance']
k = 1
while (k<obsfile.size):
    D = akPy.readncvars(obsfile[k],varlist)
    obv = akPy.readncvars(obsfile[k],np.array(['obs_variance']))['obs_variance']
    obs_var = np.maximum(obs_var,obv)
    # append D to DD
    for varI in varlist:
        DD[varI] = np.append(DD[varI],D[varI])  
    k+=1

DD = akPy.sortDict(DD,'obs_time')
DD = add_surveyTime_Nobs_DTcheck(DD,dtSec)

DD['obs_variance'] = obs_var

    
write_obs(outfile,DD,romsRefDate)    
            
