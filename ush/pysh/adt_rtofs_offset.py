# -*- coding: utf-8 -*-
"""
Created on Thu Oct 31 15:22:02 2019

@author: Alexander.Kurapov
"""

def adt_rtofs_offset(D,dSTR,dEND,romsRefDate,satIDs,grd):
    import os
    import netCDF4 as n4
    import numpy as np
    import datetime as dt
    import akPy
    from scipy.interpolate import griddata
    from scipy.interpolate import RegularGridInterpolator
#    import matplotlib.pyplot as plt
#    rtofsdir = '/gpfs/tp2/nco/ops/com/rtofs/prod/' 
    rtofsdir = os.environ["COMINrtofs_2d"]
#    # indices of the NEP region in RTOFS:
# - for the cutout:
#    iNEP = range(0,521)
#    jNEP = range(0,1027)
# - for the original global sets:       
    iNEP = range(1700,2221)
    jNEP = range(1695,2722) 
   
    # Read RTOFS:
    # Plan for 3 hourly, read rtofs outside [dSTR,dEND], to include points
    # that can be slightly outside the lims 
    # Since there is no guarantee that in the future the DA cycle hr offset will be
    # come on every 3rd hour, start searching for the 3 hourly files stepping
    # well back (1 day back) from the beginning of the day of dSTR (see variable dd)
    # Step date dd every 3 hrs and read ssh within [ddInSTR, ddInEnd] 
    ddInSTR = dSTR - dt.timedelta(hours=3)
    ddInEND = dEND + dt.timedelta(hours=3)
    ddSTOP = dEND + dt.timedelta(hours=9)
    
    # first, compute the number of times in the RTOFS time ser
    # to be able to define a 3d array, dd starts well outside [ddInSTR,ddInEND]
    dd = dSTR.replace(hour = 0) - dt.timedelta(days=1)
    NT = 0
    while ( dd <= ddSTOP):
        if ( (dd>=ddInSTR) & (dd<=ddInEND) ):
            NT += 1    
        dd = dd + dt.timedelta(hours=3)
    print('rtofs time ser is ' + str(NT) + ' time records')
    
    # Cycle over the same dates/times again, and read ssh
    it=0
    dd = dSTR.replace(hour = 0) - dt.timedelta(days=1)
    while ( dd <= ddSTOP):
        if ( (dd>=ddInSTR) & (dd<=ddInEND) ):
            if (dd < dEND-dt.timedelta(hours=3)):
                ymd = dd.strftime('%Y%m%d')
                fff= str(dd.hour).zfill(3)
            else:
                # within 3 hrs of dEND, use longer forecast from previous cycle:
                dm1= dd - dt.timedelta(days=1)
                ymd = dm1.strftime('%Y%m%d')
                fff = str(dd.hour + 24).zfill(3)

            fname = rtofsdir+'/rtofs.'+ymd+'/rtofs_glo_2ds_f'+fff+'_3hrly_diag'+'.nc'
            print(fname)
            if os.path.isfile(fname): 
              print('read '+fname)
              nc = n4.Dataset(fname)
    
              if (it == 0):
                Lon = nc.variables['Longitude'][jNEP,iNEP]-360
                Lat = nc.variables['Latitude'][jNEP,iNEP]
                NY,NX = Lon.shape
                ssh = np.zeros((NT,NY,NX),dtype=float)
                t = np.zeros(NT)
                units = nc.variables['MT'].units
                rtofsRefDate = akPy.findDateInString(units)
    
              t[it] = nc.variables['MT'][:]
              ssh[it,:,:] = nc.variables['ssh'][0,jNEP,iNEP]
              it += 1
    
              nc.close()
                    
        dd = dd + dt.timedelta(hours=3)
    
    t += (rtofsRefDate-romsRefDate).days  # time (days) relative to romsRefDate
        
    # Select obs for mean computation (400-km band west and and east of the 
    # WCOFS western boundary), also clip in time to be inside t (which is 
    # defined as the assim interval + 3hr on each side)
    eta_rho,xi_rho = grd['x_rho'].shape
    ii = np.argwhere( (D['obs_time']>=t[0]) & (D['obs_time']<=t[-1]) &
                      (D['obs_y'] >= grd['y_rho'][0,0]) & 
                      (D['obs_y'] <= grd['y_rho'][eta_rho-1,0]) &
                      (D['obs_x'] <= grd['x_rho'][0,50]) &
                      (D['obs_x'] >= 2.*grd['x_rho'][0,0] - grd['x_rho'][0,50])).squeeze()
    
    D1 = akPy.subsampleDict(D,D['obs_value'].size,ii)
    
    # Interpolation:
    # - first, find fractional indices of obs points on the RTOFS cutout grid
    # (note this works only for a simple plaid RTOFS cutout, aka a curvilinear grid)
    II, JJ = np.meshgrid(range(NX),range(NY))
    II1 = np.reshape(II,(NX*NY,1))
    JJ1 = np.reshape(JJ,(NX*NY,1))
    print(Lon.shape)
    Lon1 = np.reshape(Lon,(NX*NY,1))
    Lat1 = np.reshape(Lat,(NX*NY,1))
    
    llRTOFS=np.hstack([Lon1,Lat1])
    
    lonD1 = np.expand_dims(D1['obs_lon'],1)
    latD1 = np.expand_dims(D1['obs_lat'],1)
    tD1   = np.expand_dims(D1['obs_time'],1)
    llD1  = np.hstack([lonD1,latD1])
    
    obs_i = griddata(llRTOFS,II1,llD1,method='linear')
    obs_j = griddata(llRTOFS,JJ1,llD1,method='linear')
    
    # then do 3d interpolation on a regular grid (time, 2nd index, 1st index)
    sshFun = RegularGridInterpolator((t,range(NY),range(NX)),ssh,
                                     method='linear',bounds_error=False)
    
    sshD1 = sshFun(np.hstack([tD1,obs_j,obs_i]))
    
    # Compute the bias (obs - RTOFS):
    BIAS = np.mean(D1['obs_value'] - sshD1)
    return BIAS
