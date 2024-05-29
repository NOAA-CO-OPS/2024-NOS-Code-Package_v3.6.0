# -*- coding: utf-8 -*-
"""
Created on Mon Oct  7 14:15:53 2019

@author: Alexander.Kurapov
"""

def get_adt(dSTR,dEND,grd,dtSec,romsRefDate, \
               spreadObs,spreadDistHrs,spreadStepHrs):
# Prepare data as a dict object D, with entries:
# obs_type, obs_value, obs_lon, obs_lat, obs_depth, obs_time, obs_provenance
# time will be computed in days with reference to romsRefDate
    
    import os
    import netCDF4 as n4
    import numpy as np
    from read_adt import read_adt
    from read_adt import qcAlt
    import datetime as dt
    import wcofs_lonlat_2_xy as wcofs
    import akPy
    from scipy.interpolate import RegularGridInterpolator
    from adt_rtofs_offset import adt_rtofs_offset
#    import matplotlib.pyplot as plt
#    import getpass

#    romsTideFile = ("/gpfs/dell2/nos/noscrub/"+getpass.getuser()+
#           "/nwprod/nosofs.v3.2.1/fix/wcofs4-da/nos.wcofs4-da.modeltide.nc")    
    romsTideFile = (os.environ["DATA"] + "/" + os.environ["MODELTIDE"])
    mslBiasFile = (os.environ["COMOUT"] + "/rtofsSeaLevelBias.txt")
    satIDs=['3a','3b','c2','j3','sa']
    # Q/C parameters:
    distQC=40 # dist (km) over which the outlier is compared to the fit
    epsQC=0.2 # remove the point if dist to the fitting line is > than epsQC

    # lonLims and latLims deliniate the area where altimetry is
    # read. We allow a few degrees to west, north, and south 
    # to enable computation of the obs - rtofs bias in the vicinity 
    # of the western boundary
    lonLims = [np.min(grd['lon_rho'])-4,np.max(grd['lon_rho'])] 
    latLims = [np.min(grd['lat_rho'])-3,np.max(grd['lat_rho'])+3]
    
    # Ends of the assimilation interval, days w resp to romsRefDate
    t1 = (dSTR-romsRefDate).total_seconds()/24/3600
    t2 = (dEND-romsRefDate).total_seconds()/24/3600
        
    ## tt: times of every b/c time step in ROMS. The obs time will be adjusted
    ## to the closest 
    dtDay=dtSec/24/3600
    tt=np.arange(t1,t2+dtDay,dtDay) 
    
    ## Note: some passes /cycles can be split between the dates:
    ## accumulate all 4 days first, then sort by unique pass / cycle / sat Number
    ## Output time is in days w resp to romsRefDate
    D = read_adt(dSTR,dEND,satIDs,lonLims,latLims,romsRefDate)
    n = D['obs_value'].size
    
    print(np.max(D['obs_time']))

    xy = wcofs.wcofs_lonlat_2_xy(D['obs_lon'],D['obs_lat'],0)
    D['obs_x'] = xy['x']
    D['obs_y'] = xy['y']

    ## Provenance:
    ## Pattern to find data with unique times:
    ## write as provenance which includes the sat ID (first digit, corresp. 
    ## to satID position on the list),
    ## then pass (next 4 digits), then cycle(next 4 digits):
    ## e.g., 603410056 -> 6|0431|0056 
    ## (6 -> the 6th sat on the list ('sa'), pass 431, cycle 56 
    D['obs_provenance']=(D['satNumber']+1)*1.e+08+D['obs_pass']*1.e+4+D['obs_cycle']

    ## Before the data set is clipped to fit the assim window and the domain,
    ## compute bias between obs and RTOFS over the assimilation interval
    ## in a 400-km wide band along the western boundary (200 km west and 200 km
    ## east of the western boundary)
    #rtofsSeaLevelBias = \
    #   adt_rtofs_offset(D,dSTR,dEND,romsRefDate,satIDs,grd) - 0.25 
    #print('rtofsSeaLevelBias computed internally: ' + str(rtofsSeaLevelBias))
    #with open(mslBiasFile,'a') as f:
    #     dummy='{:12} {:1.3f}'.format(dEND.strftime("%Y %m %d"),rtofsSeaLevelBias)
    #     f.write(dummy)
    
    #rtofsSeaLevelBias = 0.2463 # based on 16 days of analysis in Oct-Nov 2019
    
    rtofsSeaLevelBias = 0.242 # cycle 20191115, also the 3-mo mean
    
    ## Crop data outside the domain:
    #- Diffuse the mask s.t. interior points at a specified distance from coast
    #- (in pnts, second intput in diffuse_mask) have masked values <1
    maskNew = akPy.diffuse_mask(grd['mask_rho'],6)    
    maskNew[:,0] = 0
    maskNew[:,-1] = 0
    maskNew[0,:] = 0
    maskNew[-1,:] = 0
    
    x_rho_1 = grd['x_rho'][0,:]
    y_rho_1 = grd['y_rho'][:,0]
    mskFun = RegularGridInterpolator((y_rho_1,x_rho_1),maskNew,
                                     method='linear',bounds_error=False)
    
    #- the second dimension, 1, is needed for hstack
    x = D['obs_x'].reshape(n,1)
    y = D['obs_y'].reshape(n,1)
    msk = mskFun(np.hstack([y,x]))
    
    ichk = np.argwhere( (D['obs_provenance']==100820050) & (D['obs_lat']>28.9) ).squeeze()
    print(D['obs_lat'][ichk])
    print(msk[ichk]) 
    
    msk[np.isnan(msk)]=-9999
    ii = np.argwhere( msk>0.9999 ).squeeze() 
    D=akPy.subsampleDict(D,D['obs_value'].size,ii)
    n = D['obs_value'].size
    t_survey = np.zeros(D['obs_value'].shape)
    
    #ichk = np.argwhere( (D['obs_provenance']==100670050) & (D['obs_lat']<22) ).squeeze()
    #print(D['obs_lat'][ichk])
    
    ## For each sat, pass, and cycle, adjust obs_time as the mean along the 
    ## track, write to alt['obs_time_survey']
    ## If the mean is inside the assimilation interval, then move survey time 
    ## to the closest model ROMS time instance
    ## Avoid using the same ROMS time instance twice (separate tracks, to preserve
    ## info about sat, pass, and cycle in obs_provenance)    
    provU = np.unique(D['obs_provenance']) # => unique combinations of sat#, pass and cycle
    for prov in provU:
        it = np.where( D['obs_provenance']==prov)
        t0 = np.mean(D['obs_time'][it])
        
        if ( (t0>=t1) & (t0<=t2) ):
            # the mean alongtrack time is within the assim window time limits
            # assign the time to the closest ROMS time
            iobs = np.argmin(np.abs(tt-t0))
            t_survey[it] = tt[iobs] 
            # exclude the element used: survey times will be unique to preserve 
            # sat-pass-cycle info
            tt = np.delete(tt,iobs) 
        else:
            t_survey[it] = t0 # t0 outside assim int limits, redefine now,
                              # crop later
    D['obs_time'] = t_survey

    ## Exlude data that lie outside the assim interval
    ii = np.where( (D['obs_time']>=t1) & (D['obs_time']<=t2) )
    D=akPy.subsampleDict(D,D['obs_value'].size,ii)
      
#    fig = plt.figure()
#    mngr = plt.get_current_fig_manager()
#    mngr.window.setGeometry(-1700,100,1000,700)
#    ax = fig.add_axes([0.1, 0.1, 0.8, 0.8])
#    ax.plot(alt['obs_time'],range(n),'.',c='b')
#    ax.plot(t_survey,range(n),'.',c='r')
#    plt.show()

    ## Adjust the mean ADT (from hycom)
    D['obs_value'] -= rtofsSeaLevelBias
    
    ## Quality control:
    qcFlag = qcAlt(D,distQC,epsQC) # 1: good points, 0 candidate to exclude
     
#    # Plot passes one by one and highlight flagged points:
#    provU = np.unique(D['obs_provenance'])
#    for prov in provU:
#        it = np.argwhere( D['obs_provenance']==prov).squeeze()
#        alt1 = akPy.subsampleDict(D,D['obs_value'].size,it)
#        s = akPy.distAlongTrack(alt1['obs_lon'],alt1['obs_lat'])
#        jjj=np.argwhere(qcFlag[it] == 0).squeeze()
#        
#        fig = plt.figure()
#        mngr = plt.get_current_fig_manager()
#        mngr.window.setGeometry(-1600,100,1100,600)
#        ax = fig.add_axes([0.1, 0.1, 0.8, 0.8])
#        ax.plot(s,alt1['obs_value'],'.r-')
#        if (jjj.size>0):
#            ax.plot(s[jjj],alt1['obs_value'][jjj],'*b')
#        plt.ylim(-0.5,0.7)
#        plt.xlim(0,5000)
#        ax.set_title(str(prov))
#        plt.show()
    
    inum = np.argwhere( qcFlag == 1).squeeze()
    D=akPy.subsampleDict(D,D['obs_value'].size,inum)
    print("after QC: ",np.max(D['obs_time']))
    
    
    ## Spread the data:
        
    if spreadObs:
     # assign the spread data to one of the tt instances, from which obs_time
     # instances have already been excluded (to avoid assimilating two tracks at
     # the same time)  
        tsp = list(range(-spreadDistHrs,spreadDistHrs+spreadStepHrs,spreadStepHrs))
        tsp.remove(0) # exclude 0
        
        provU = np.unique(D['obs_provenance']) # => unique combinations of sat#, pass and cycle
        for prov in provU:
            it = np.where( D['obs_provenance']==prov)
            t0 = np.mean(D['obs_time'][it])
        
            for dtHrs in tsp:
                tnew = t0 + dtHrs / 24
                if ( (tnew >=t1) & (tnew <=t2) ):                    
                    # assign the time to the closest ROMS time
                    # (not yet used by another track)
                    iobs = np.argmin(np.abs(tt-tnew))
                    t = tt[iobs] 
                
                    # exclude the last tt element used: survey times will be unique to preserve 
                    # sat-pass-cycle info
                    tt = np.delete(tt,iobs) 

                    fillShape = D['obs_time'][it].shape
                    bogusCycle = 9000+dtHrs
                    newProv = D['obs_provenance'][it]-D['obs_cycle'][it]+bogusCycle

                    # append to D:
                    D['obs_value']=np.append(D['obs_value'],D['obs_value'][it])
                    D['obs_lon']  =np.append(D['obs_lon']  ,D['obs_lon'][it])
                    D['obs_lat']  =np.append(D['obs_lat']  ,D['obs_lat'][it])
                    D['obs_time'] =np.append(D['obs_time'] ,np.full(fillShape,t))
                    D['obs_pass'] =np.append(D['obs_pass'] ,D['obs_pass'][it])
                    D['obs_cycle']=np.append(D['obs_cycle'],np.full(fillShape,bogusCycle))
                    D['satNumber']=np.append(D['satNumber'],D['satNumber'][it])
                    D['obs_x']    =np.append(D['obs_x']    ,D['obs_x'][it])
                    D['obs_y']    =np.append(D['obs_y']    ,D['obs_y'][it])
                    D['obs_provenance'] = np.append(D['obs_provenance'],newProv)
                    
    # recompute yx stack since the set was trimmed (QC)
    # (the second dimension, 1, is needed for hstack)
    n = D['obs_value'].size
    x = D['obs_x'].reshape(n,1)
    y = D['obs_y'].reshape(n,1)

    ## Add model tides:
    ncTide = n4.Dataset(romsTideFile)
    tide_amp = ncTide.variables['zeta_amp'][:]
    tide_phase = ncTide.variables['zeta_phase'][:]
    Tp = ncTide.variables['tide_period'][:] # hrs
    tideRefDate = ncTide.variables['zeta_phase'].ref_time #string '%Y-%m-%d %H:%M:%S'
    ncTide.close()
    
    
    tideRefDate = dt.datetime.strptime(tideRefDate,'%Y-%m-%d %H:%M:%S')
    
    tideMinusRomsTime = (tideRefDate-romsRefDate).total_seconds()/24/3600
    
    Tpd = Tp / 24 # tide period in days
    OMEGA = 2 * np.pi / Tpd    
    tide_phase = tide_phase * np.pi / 180.
  
    for ii in range(Tpd.size):
        ampFun = RegularGridInterpolator((y_rho_1,x_rho_1),tide_amp[ii,:,:],
                                       method='linear',bounds_error=False)
        cosFun = RegularGridInterpolator((y_rho_1,x_rho_1),np.cos(tide_phase[ii,:,:]),
                                       method='linear',bounds_error=False) 
        sinFun = RegularGridInterpolator((y_rho_1,x_rho_1),np.sin(tide_phase[ii,:,:]),
                                       method='linear',bounds_error=False)

        ampAlongTrack = ampFun(np.hstack([y,x]))
        cosAlongTrack = cosFun(np.hstack([y,x]))
        sinAlongTrack = sinFun(np.hstack([y,x]))

        ott = OMEGA[ii] * (D['obs_time'] - tideMinusRomsTime)
        
        D['obs_value'] += ampAlongTrack * ( 
                np.cos(ott) * cosAlongTrack + np.sin(ott) * sinAlongTrack ) 
        
    
    D.update( {'survey_time': np.empty(0,)} )
    D.update( {'Nobs': np.empty(0,)} )
    D.update( {'obs_type': np.ones(D['obs_value'].shape)} )
    
    return D
        
    
