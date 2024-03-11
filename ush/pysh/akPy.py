# -*- coding: utf-8 -*-
"""
Created on Thu Jul 25 12:02:21 2019

@author: Alexander.Kurapov
"""

def findDateInString(a):
    # Finds a date in string "a" (such as units attribute)
    # returns datetime object
    import re
    import datetime as dt
    
    match = re.search('\d{4}-\d{2}-\d{2}', a)
    date1 = dt.datetime.strptime(match.group(), '%Y-%m-%d')
    
    return date1

def sortDict(D,sortKey):

    import numpy as np
    
    isort = np.argsort(D[sortKey],kind='mergesort')
    for key in D:
        if D[key].size == D[sortKey].size:
            D[key] = D[key][isort] 
    
    return D

def subsampleDict(D,n,ii):

    B={}
    
    for key in D:
        if D[key].size == n:
            B[key] = D[key][ii]
        else:
            B[key] = D[key]    
    
    return B

def obs_ijpos(D,grd):

    from wcofs_lonlat_2_xy import wcofs_lonlat_2_xy
    from scipy import interpolate
    import numpy as np

    xy = wcofs_lonlat_2_xy(D['obs_lon'],D['obs_lat'],0)
    x = xy['x']
    y = xy['y']
    eta_rho,xi_rho=grd['x_rho'].shape
    
    f=interpolate.interp1d(grd['x_rho'][0,:],np.arange(0,xi_rho))
    D['obs_Xgrid'] = f(np.ma.filled(x))
    f=interpolate.interp1d(grd['y_rho'][:,0],np.arange(0,eta_rho))
    D['obs_Ygrid'] = f(np.ma.filled(y))
    
    return D

def plot_granules(D,grd):
    
    import matplotlib.pyplot as plt
    import math
    import numpy as np
    
    xlim1=grd['lon_rho'].min()
    xlim2=grd['lon_rho'].max()
    ylim1=grd['lat_rho'].min()
    ylim2=grd['lat_rho'].max()
    
    tsurvey = np.unique(np.unique(D['obs_time']))
    
    #Plot granules from D:
    nr = 3
    nc = math.ceil(tsurvey.size/3)        
    fig,axes = plt.subplots(nr,nc,sharex=True, sharey=True)
#    mngr = plt.get_current_fig_manager()
#    mngr.window.setGeometry(-1800,50,1700,1000)      
    fig.set_size_inches(18.5,9.5)
    for iax in np.arange(0,tsurvey.size):
        iN = np.argwhere(D['obs_time']==tsurvey[iax]).squeeze()
        if iN.size>1:
           kr = math.floor(iax/nc)
           kc = iax - kr * nc
           ax = axes[kr][kc]
           ax.scatter(D['obs_lon'][iN],D['obs_lat'][iN],
               s=3,marker='s',c=D['obs_value'][iN],vmin=7,vmax=25,cmap='jet')
           ax.set_xlim(xlim1,xlim2)
           ax.set_ylim(ylim1,ylim2)
           ax.contour(grd['lon_rho'],grd['lat_rho'],grd['mask_rho'],[0.49,0.5],
                      colors='k')   
        
    plt.show(block=False)
    
def add_surveyTime_Nobs(D):
    
    import numpy as np
    
    tsurvey = np.unique(D['obs_time'])
    Nobs = np.ndarray((tsurvey.size,),int) # number of obs in each survey
    
    for j in range(0,len(tsurvey)):
        ii = np.argwhere(D['obs_time'] == tsurvey[j] ).squeeze()
        Nobs[j] = ii.size
    
    D['survey_time'] = tsurvey
    D['Nobs'] = Nobs
    
    return D

def superobs_surf(D,grd):

    import numpy as np
    
    S = {      'obs_type':np.empty(0,), 
              'obs_value':np.empty(0,), 
                'obs_lon':np.empty(0,), 
                'obs_lat':np.empty(0,), 
              'obs_depth':np.empty(0,),
               'obs_time':np.empty(0,),
              'obs_Xgrid':np.empty(0,),
              'obs_Ygrid':np.empty(0,),
         'obs_provenance':np.empty(0,)
         }
    
    obs_i = np.round(D['obs_Xgrid']).astype(int)
    obs_j = np.round(D['obs_Ygrid']).astype(int)
    
    eta_rho,xi_rho=grd['mask_rho'].shape
    n = eta_rho*xi_rho
    
    for ts in D['survey_time']:
        isur = np.argwhere(D['obs_time']==ts)
        typeUnique = np.unique(D['obs_type'][isur])
        
        for type_i in typeUnique:
            ii = np.argwhere((D['obs_time'] == ts) & (D['obs_type']==type_i)).squeeze()
            
            count     = np.zeros((eta_rho,xi_rho))
            obs_value = np.zeros((eta_rho,xi_rho))
            obs_lon   = np.zeros((eta_rho,xi_rho))
            obs_lat   = np.zeros((eta_rho,xi_rho))
            obs_Xgrid = np.zeros((eta_rho,xi_rho))
            obs_Ygrid = np.zeros((eta_rho,xi_rho))

#            print(str(ts) + '   ' + str(type_i))
            if ii.size==1:
                 ii = [ii] 
            for k in ii:
                i = obs_i[k]
                j = obs_j[k]
                count[j,i]     += 1 
                obs_value[j,i] += D['obs_value'][k] 
                obs_lon[j,i]   += D['obs_lon'][k]
                obs_lat[j,i]   += D['obs_lat'][k]
                obs_Xgrid[j,i] += D['obs_Xgrid'][k]
                obs_Ygrid[j,i] += D['obs_Ygrid'][k]
                if (count[j,i] == 1):
                    prov = D['obs_provenance'][k]
                
            
            count     = count.reshape(n,)
            obs_value = obs_value.reshape(n,)
            obs_lon   = obs_lon.reshape(n,)
            obs_lat   = obs_lat.reshape(n,)
            obs_Xgrid = obs_Xgrid.reshape(n,)
            obs_Ygrid = obs_Ygrid.reshape(n,)
            
            iNot0 = np.argwhere(count>0).squeeze()
            count = count[iNot0]
            obs_value = obs_value[iNot0]
            obs_lon = obs_lon[iNot0]
            obs_lat = obs_lat[iNot0]
            obs_Xgrid = obs_Xgrid[iNot0]
            obs_Ygrid = obs_Ygrid[iNot0]
            
            obs_value  /= count
            obs_lon    /= count
            obs_lat    /= count 
            obs_Xgrid  /= count
            obs_Ygrid  /= count
    
            obs_type = type_i * np.ones(obs_value.shape)
            obs_time = ts     * np.ones(obs_value.shape)
            obs_prov = prov * np.ones(obs_value.shape)
            
            S['obs_type']  = np.append(S['obs_type'],obs_type) 
            S['obs_value'] = np.append(S['obs_value'],obs_value)
            S['obs_lon']   = np.append(S['obs_lon'],obs_lon)
            S['obs_lat']   = np.append(S['obs_lat'],obs_lat)         
            S['obs_time']  = np.append(S['obs_time'],obs_time)   
            S['obs_Xgrid']  = np.append(S['obs_Xgrid'],obs_Xgrid)   
            S['obs_Ygrid']  = np.append(S['obs_Ygrid'],obs_Ygrid)   
            S['obs_provenance'] = np.append(S['obs_provenance'],obs_prov)
        # end "for type_i in typeUnique:"
    # end "for ts in D['survey_time']:"

    return S        
    
def readncvars(fname,varlist):
    # read variables of nc file from the varlist, return as a dict object
    import netCDF4 as n4

    print("read " + fname)
    vars = {}
    nc = n4.Dataset(fname)
    for varI in varlist:
        vars[varI] = nc.variables[varI][:]
    
    nc.close()
    return vars

def diffuse_mask(mask,npnts):

    import copy
    
    # Note, originally, in mask, 1 = sea and 0 = land
    nn = mask.shape
    ny = nn[0]
    nx = nn[1]
    
    # orig, float numbers:
    mask_new = 1 - mask
    
    # new: use integers:
    #mask_new = (mask+0.1).astype(int)
    #mask_new = 1 - mask_new
    
    for it in range(npnts): # it from 0 to npnts - 1 (ie npts times)
      
        mask_old = copy.copy(mask_new)
        mask_new[:,1:nx]   += mask_old[:,0:nx-1]
        mask_new[:,0:nx-1] += mask_old[:,1:nx]
        mask_new[1:ny,:]   += mask_old[0:ny-1,:]
        mask_new[0:ny-1,:] += mask_old[1:ny,:]
        mask_new[mask_new>0] = 1
           
    mask_new = 1 - mask_new
    return mask_new

def sw_dist_km(lat,lon):
    # "PLANE SAILING" METHOD: works for small separations on the sphere
    
    import numpy as np
    
    n = lat.size
    ind = np.arange(n-1) # n-1 elements, from 0 to n-2
    
    iMore180 = np.argwhere(lon > 180).squeeze()
    lon[iMore180] -= 360 # => all in [-180 180]
    
    dlon = np.diff(lon)
    flag = np.argwhere(np.absolute(dlon) > 180).squeeze()
    dlon[flag]= -np.sign(dlon[flag] * ( 360 - np.absolute(dlon[flag]) ) )
    
    latRad = np.absolute(lat * np.pi / 180)
    dep    = np.cos( 0.5*(latRad[ind+1]+latRad[ind]) ) * dlon 
    dlat = np.diff(lat)
    dist = 111.12*np.sqrt(dlat*dlat + dep * dep)
    # note: 6370*np.pi/180 = 111.17; using 111.12 as in sea water routines
    
    return dist

def distAlongTrack(lon,lat):
    import numpy as np
    d = sw_dist_km(lat,lon)
    s = np.cumsum(d)         # distances from the 1st pnt of the pass
    s = np.insert(s,0,0.)    # adding 0 at the beginning of the list s
    return s


