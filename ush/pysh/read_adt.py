# -*- coding: utf-8 -*-
"""
Created on Mon Oct  7 16:50:07 2019

@author: Alexander.Kurapov
"""

def read_adt(dSTR,dEND,satIDs,lonLims,latLims,romsRefDate):

    import os
    import netCDF4 as n4
    import numpy as np
    import datetime as dt
    import akPy

    sshdir = os.environ["DCOMINssh"] + "/"
    outDir = os.environ["DATA"] + "/adt/"    
    day1 = dSTR.toordinal() # integer day
    day2 = dEND.toordinal()
    alt = {'obs_value':np.empty(0,),
             'obs_lon':np.empty(0,), 
             'obs_lat':np.empty(0,), 
            'obs_time':np.empty(0,),
            'obs_pass':np.empty(0,),
           'obs_cycle':np.empty(0,),
           'satNumber':np.empty(0,)}
    
    nsat = len(satIDs)
    
    for dayI in range(day1,day2+1):
        
        # read data in each daily file
        ymd = dt.datetime.fromordinal(dayI).strftime("%Y%m%d")
        dateNewYear=ymd[0:4] + "0101"
        dNewYear = dt.datetime.strptime(dateNewYear,'%Y%m%d')
        dayOfYear = dayI - dNewYear.toordinal() + 1 
        
        # for each satellite:
        # Note: sat data are here: /gpfs/dell1/nco/ops/dcom/dev/20191026/wgrdbul/adt
        for isat in range(0,nsat,1):
            satID = satIDs[isat]
            fdir = sshdir + ymd + '/wgrdbul/adt/' 
            ffname = "rads_adt_" + satID + "_" + ymd[0:4] + str(dayOfYear).rjust(3,'0') + ".nc"
            fname = fdir + ffname
            print(ffname)
            
            if os.path.isfile(fname):
                # read the file (single, day - single sat)
                print("read " + fname)

                # On 03/10/2024 Zheng added "try-except-else" to prevent the python running
                # from failure when the NC file is corrupted or unknown file format.                
                try:
                    nc = n4.Dataset(fname)
                    lon = nc.variables['lon'][:]
                    lat = nc.variables['lat'][:]
                    ssh = nc.variables['adt_egm2008'][:]
                    passAlt = nc.variables['pass'][:]
                    cycleAlt = nc.variables['cycle'][:]
                    t = nc.variables['time_mjd'][:]          # time in days since obsRefDate
                    timeUnits = nc.variables['time_mjd'].units
                    obsRefDate = akPy.findDateInString(timeUnits) # datetime object 
                    nc.close()
                except:
                    print("Warning: File " + fname + " is corrupted")
                else:
                    # - time in days since romsRefDate
                    romsObsOffset = (romsRefDate-obsRefDate).total_seconds()/24/3600
                    t = t - romsObsOffset 
        
                    ii = np.where((lon >= lonLims[0]) & (lon <= lonLims[1]) & 
                                  (lat >= latLims[0]) & (lat <= latLims[1])) # tuple object
                    ii = ii[0] # remove extra () from the tuple object => array revealed
                
                    if ii.size>0:
                        alt['obs_value']=np.append(alt['obs_value'],ssh[ii])
                        alt['obs_lon']=np.append(alt['obs_lon'],lon[ii])
                        alt['obs_lat']=np.append(alt['obs_lat'],lat[ii])
                        alt['obs_time']=np.append(alt['obs_time'],t[ii])
                        alt['obs_pass']=np.append(alt['obs_pass'],passAlt[ii])
                        alt['obs_cycle']=np.append(alt['obs_cycle'],cycleAlt[ii])
                        alt['satNumber']=np.append(alt['satNumber'],isat*np.ones(ii.shape))
                    
    return alt

def qcAlt(D,distQC,epsQC):
    # Inputs: D is a dictionary including keys 'obs_value','obs_provenance'
    # 'obs_lon', 'obs_lat'
    # NOTE: this function returns a vector of flags, it does not exclude points
    import numpy as np
    import akPy
    
    qcFlag = np.ones(D['obs_value'].shape,dtype = int)
    
    provU = np.unique(D['obs_provenance']) # => unique combinations of sat#, pass and cycle
    for prov in provU:
        it = np.argwhere( D['obs_provenance']==prov).squeeze()
        if it.size <3:
            # one- or two point track .. exclude
            qcFlag[it] = 0
        else:
            alt1 = akPy.subsampleDict(D,D['obs_value'].size,it)
            s = akPy.distAlongTrack(alt1['obs_lon'],alt1['obs_lat'])
            npnts = s.size

            for j in range(npnts):
                ds = s - s[j]

                #inei = np.argwhere( (np.absolute(ds)<=distQC) & (np.absolute(ds)>0) ).squeeze()
                inei = np.argwhere( (abs(ds)<=distQC) & (abs(ds)>0) ).squeeze()
                if inei.size>2:
                    dSSH=alt1['obs_value'][j]-np.nanmean(alt1['obs_value'][inei])
                    if (np.absolute(dSSH)>epsQC):
                        qcFlag[it[j]] = 0

    return qcFlag
  
