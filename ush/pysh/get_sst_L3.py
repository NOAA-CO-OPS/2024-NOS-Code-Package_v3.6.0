# -*- coding: utf-8 -*-
"""
Created on Thu Jul 25 10:18:45 2019

@author: Alexander.Kurapov
Modified by Zheng on 05/02/2023
"""

def get_sst_L3(dSTR,dEND,grd,dtSec,sstSets,romsRefDate):
# Prepare data as a dict object D, with entries:
# obs_type, obs_value, obs_lon, obs_lat, obs_depth, obs_time, obs_provenance
# time will be computed in days with reference to romsRefDate
    
# Note: sstSets is a set of objects of class sstSet   
        
    import netCDF4 as n4
    import numpy as np
    import datetime as dt
    import wcofs_lonlat_2_xy as wcofs
    import akPy
    from scipy.interpolate import RegularGridInterpolator
    import os
    sstPath = os.environ["DATA"]+"/SST/"
    
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
    D={      'obs_type':np.empty(0,), 
            'obs_value':np.empty(0,), 
              'obs_lon':np.empty(0,), 
              'obs_lat':np.empty(0,), 
            'obs_depth':np.empty(0,),
             'obs_time':np.empty(0,),
       'obs_provenance':np.empty(0,)
           }
    
    for sat in sstSets:
        
        # Unique provenance for npp, n20, and g17(gta): 11, 12, 13
        prov = sat.provenance
             
        for dayI in range(day1,day2+1):
        
            ymd = dt.datetime.fromordinal(dayI).strftime("%Y%m%d")
            fname = sstPath + ymd + '/' + ymd + "_sst_" + sat.satName + "_NEP.nc"
            print ("reading " + fname)
            if os.path.isfile(fname):
                nc  = n4.Dataset(fname)
                t = nc.variables['time'][:] # in sec since obsRefDate
                timeUnits = nc.variables['time'].units
                obsRefDate = akPy.findDateInString(timeUnits) # datetime object
            
                # - time in days since romsRefDate
                romsObsOffset = (romsRefDate-obsRefDate).total_seconds()/24/3600
                t = t/24/3600 - romsObsOffset 
            
                ii = np.where((t >= t1) & (t <= t2)) # tuple object
                ii = ii[0] # remove extra () from the tuple object => array revealed
            
                if ii.size>0:
                
                    lon = nc.variables['lon'][:]
                    lat = nc.variables['lat'][:]
                    lon,lat = np.meshgrid(lon,lat) # 2d grid
                    nx,ny = lon.shape
                    lon = lon.reshape(nx*ny,1)     # redefine lon, lat as 1d vectors of 
                    lat = lat.reshape(nx*ny,1)     # the gridded lon-lat
                
                    xy = wcofs.wcofs_lonlat_2_xy(lon,lat,0)
                    x = xy['x']
                    y = xy['y']
                
                    msk = mskFun(np.hstack([y,x]))
                
                    msk[np.isnan(msk)] = 0
                
                   # pick out numbers within the model ocean domain
                    t_in = t[ii] # vector
                    sst = nc.variables['sea_surface_temperature'][ii]

                   # Zheng add '20230502'
                   # if ( (sat.satName == "npp") | (sat.satName == "n20") ): 
                    if sat.satName == "l3s":
                   # Zheng add end '20230502'     
                        print("   " + sat.satName + ": subtract sses_bias...")
                        sses_bias = nc.variables['sses_bias'][ii]
                        sst = sst - sses_bias
                    # convert to a 1D array:
                    for it in range(0,len(ii),1):
                        print("it=" + str(it))
                        sst1 = sst[it,:,:]
                        sst1=sst1.reshape(nx*ny,)
                    
                        logi1 = (msk>0.5)
                        logi2 = ~np.isnan(sst1)
                    
                        iii = np.argwhere(logi1 & logi2).squeeze()
                    
                        if iii.size>0 :
                        
                            iobs=np.argmin(np.abs(tt-t_in[it]))
                            tRomsObs=tt[iobs]
                        
                          # exclude the last tt element used: survey times will be unique to preserve 
                          # sat-pass-cycle info
                            tt = np.delete(tt,iobs) 
                        
                            D['obs_type']=np.append(D['obs_type'],6*np.ones(iii.shape))
                            D['obs_value']=np.append(D['obs_value'],sst1[iii])
                            D['obs_lon']=np.append(D['obs_lon'],lon[iii])
                            D['obs_lat']=np.append(D['obs_lat'],lat[iii])
                            D['obs_depth']=np.append(D['obs_depth'],np.zeros(iii.shape))
                            D['obs_time']=np.append(D['obs_time'],tRomsObs*np.ones(iii.shape))
                            D['obs_provenance']=np.append(D['obs_provenance'],prov*np.ones(iii.shape))
                    
                nc.close()
            else:
                print("WARNING: "+fname+" does not exist!")
    return D
