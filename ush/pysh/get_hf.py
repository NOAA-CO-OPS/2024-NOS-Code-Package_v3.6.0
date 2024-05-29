# -*- coding: utf-8 -*-
"""
Created on Mon Aug 12 12:08:24 2019

@author: Alexander.Kurapov
"""

def get_hf(dSTR,dEND,grd,dtSec,romsRefDate,epsDOP,hmin):
# Prepare data as a dict object D, with entries:
# obs_type, obs_value, obs_lon, obs_lat, obs_depth, obs_time, obs_provenance
# time will be computed in days with reference to romsRefDate
    
    import netCDF4 as n4
    import numpy as np
    import datetime as dt
    import wcofs_lonlat_2_xy as wcofs
    import akPy
    from scipy.interpolate import RegularGridInterpolator
    import os
    import glob
    
    inDatDir1 = os.environ["DCOMINhf"]
    inDatDir2 = "wgrdbul/ndbc/"
    
    day1 = dSTR.toordinal() # integer day
    day2 = dEND.toordinal()
    
    t1 = (dSTR-romsRefDate).total_seconds()/24/3600
    t2 = (dEND-romsRefDate).total_seconds()/24/3600
    
    # tt: times of every b/c time step in ROMS. The obs time will be adjusted
    # to the closest 
    dtDay=dtSec/24/3600
    tt=np.arange(t1,t2+dtDay,dtDay) 
    
    x_rho = grd['x_rho']
    y_rho = grd['y_rho']
    x_rho_1 = x_rho[0,:]
    y_rho_1 = y_rho[:,0]
    
    # Exclude points near borders 
    maskWithBorders = grd['mask_rho']
    maskWithBorders[:,0] = 0
    maskWithBorders[:,-1] = 0
    maskWithBorders[0,:] = 0
    maskWithBorders[-1,:] = 0
    
    mskFun = RegularGridInterpolator((y_rho_1,x_rho_1),maskWithBorders,
                                     method='linear',bounds_error=False)
    
    hFun = RegularGridInterpolator((y_rho_1,x_rho_1),grd['h'],
                                     method='linear',bounds_error=False)
    
    cosaFun = RegularGridInterpolator((y_rho_1,x_rho_1),np.cos(grd['angle']),
                                       method='linear',bounds_error=False)
    
    sinaFun = RegularGridInterpolator((y_rho_1,x_rho_1),np.sin(grd['angle']),
                                       method='linear',bounds_error=False)
    
    D={      'obs_type':np.empty(0,), 
            'obs_value':np.empty(0,), 
              'obs_lon':np.empty(0,), 
              'obs_lat':np.empty(0,), 
            'obs_depth':np.empty(0,),
             'obs_time':np.empty(0,),
       'obs_provenance':np.empty(0,)
           }
    
    readHFcoord_yes = 1
    
    for dayI in range(day1,day2+1):
    
        ymd = dt.datetime.fromordinal(dayI).strftime("%Y%m%d")
        thisDateDir = (inDatDir1 + "/" + ymd + "/" + inDatDir2 + 
                      "*_hfr_uswc_6km_rtv_uwls_NDBC*")
        print ('File listing' + thisDateDir)
        files=glob.glob(thisDateDir) 
#        for hr in range(0,24):
#            ymdhm = ymd + f'{hr:02}' + "00_hfr_uswc_6km_rtv_uwls_NDBC.nc"
#            fname = thisDateDir + ymdhm

        for fname in files:            
            print ("Checking " + fname)
            if os.path.isfile(fname):
                print ("reading " + fname)
                nc  = n4.Dataset(fname)
                t = nc.variables['time'][:] # in sec since obsRefDate
                timeUnits = nc.variables['time'].units
                obsRefDate = akPy.findDateInString(timeUnits) # datetime object
                              
                # - time in days since romsRefDate
                romsObsOffset = (romsRefDate-obsRefDate).total_seconds()/24/3600
                t = t/24/3600 - romsObsOffset 
    #            
                if ((t >= t1) & (t <= t2)):
                    
                    if readHFcoord_yes:
                        
                        lon = nc.variables['lon'][:]
                        lat = nc.variables['lat'][:]
                        lon,lat = np.meshgrid(lon,lat) # 2d grid
                        nx,ny = lon.shape
                        lon = lon.reshape(nx*ny,1)     # redefine lon, lat as 1d vectors of 
                        lat = lat.reshape(nx*ny,1)     # the gridded lon-lat
                        xy = wcofs.wcofs_lonlat_2_xy(lon,lat,0)
                        x = xy['x']
                        y = xy['y']
                        cosa = cosaFun(np.hstack([y,x]))    
                        sina = sinaFun(np.hstack([y,x]))
                        
                        mskI = mskFun(np.hstack([y,x]))
                        hI   = hFun(np.hstack([y,x]))   
    #                    cosa = cosa.reshape(nx,ny)
    #                    sina = sina.reshape(nx,ny)
                        readHFcoord_yes = 0
 
                    uHF = nc.variables['u'][:].squeeze().reshape(nx*ny,)
                    vHF = nc.variables['v'][:].squeeze().reshape(nx*ny,)
                    DOPx = nc.variables['dopx'][:].squeeze().reshape(nx*ny,)
                    DOPy = nc.variables['dopy'][:].squeeze().reshape(nx*ny,)

    #                print(uHF.shape)
    #                print(vHF.shape)
    #                print(cosa.shape)
    #                print(sina.shape)

                    u= uHF*cosa+vHF*sina                  
                    v=-uHF*sina+vHF*cosa
                    ##Changes made on 2/8/2021 to exclude CR and Canada
                    inan = np.argwhere( (lat>45.7) & (lon>-125) ).squeeze()
                    u[inan]=np.nan
                    v[inan]=np.nan

                    iii = np.argwhere( (~np.isnan(u)) & (~np.isnan(v))   &
                                      (DOPx <=epsDOP) & (DOPy <= epsDOP) &
                                      (mskI>0.5) & (hI>hmin) ).squeeze()
                    
                    if iii.size>0 :
                        
                        iobs=np.argmin(np.abs(tt-t))
                        tRomsObs=tt[iobs]
                        
                        # - add u to D:
                        D['obs_type']=np.append(D['obs_type'],4*np.ones(iii.shape))
                        D['obs_value']=np.append(D['obs_value'],u[iii])
                        D['obs_lon']=np.append(D['obs_lon'],lon[iii])
                        D['obs_lat']=np.append(D['obs_lat'],lat[iii])
                        D['obs_depth']=np.append(D['obs_depth'],np.zeros(iii.shape))
                        D['obs_time']=np.append(D['obs_time'],tRomsObs*np.ones(iii.shape))
                        D['obs_provenance']=np.append(D['obs_provenance'],1*np.ones(iii.shape))
                    
                        # - add v to D:
                        D['obs_type']=np.append(D['obs_type'],5*np.ones(iii.shape))
                        D['obs_value']=np.append(D['obs_value'],v[iii])
                        D['obs_lon']=np.append(D['obs_lon'],lon[iii])
                        D['obs_lat']=np.append(D['obs_lat'],lat[iii])
                        D['obs_depth']=np.append(D['obs_depth'],np.zeros(iii.shape))
                        D['obs_time']=np.append(D['obs_time'],tRomsObs*np.ones(iii.shape))
                        D['obs_provenance']=np.append(D['obs_provenance'],1*np.ones(iii.shape))
                    # end "if iii.size>0
                # end "if ((t >= t1) & (t <= t2)):"    
                nc.close()
            # end "if os.path.isfile(fname):"
        # end "for hr in range(0,24):"    
    #for dayI in range(day1,day2+1):
    print('Number of u/v observation is '+ str(len(D['obs_time'])))    
    return D
    

