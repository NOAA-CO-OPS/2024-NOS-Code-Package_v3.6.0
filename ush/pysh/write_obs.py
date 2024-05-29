# -*- coding: utf-8 -*-
"""
Created on Wed Jul 24 11:35:07 2019

@author: Alexander.Kurapov
"""

def write_obs(fname,S,romsRefDate):
    import os
    import netCDF4 as n4
    import numpy as np
    
    if os.path.isfile(fname):
        os.remove(fname)

    timeUnitsStr="days since " + romsRefDate.strftime("%Y-%m-%d %H:%M:%S")
        
    nc = n4.Dataset(fname,mode='w')

#
    nsurvey = S['survey_time'].size
    ndatum = S['obs_value'].size
    nc.createDimension('survey', nsurvey)
    nc.createDimension('state_variable', 7) 
    nc.createDimension('datum', ndatum)
    
# variables
    v={}
            
    v['spherical'] = nc.createVariable('spherical',np.int32)
    v['spherical'].long_name = 'grid type logical switch'
    v['spherical'].flag_values = np.arange(2,dtype='i4') # replaced original [0,1] on 6 Nov 2020
    v['spherical'].flag_meanings = 'Cartesian spherical'
    
    v['Nobs'] = nc.createVariable('Nobs',np.int32,('survey'))
    v['Nobs'].long_name = 'number of observations with the same survey time'
    
    v['survey_time'] = nc.createVariable('survey_time',np.float64,('survey'))
    v['survey_time'].long_name = 'survey time'
    v['survey_time'].units = timeUnitsStr
    v['survey_time'].calendar = 'gregorian'    
    
    v['obs_variance'] = nc.createVariable('obs_variance',np.float64,('state_variable'))
    v['obs_variance'].long_name = 'global temporal and spatial observation variance'
    
    v['obs_type'] = nc.createVariable('obs_type',np.int32,('datum'))
    v['obs_type'].long_name = 'model state variable associated with observations'
#    v['obs_type'].flag_values = [1,2,3,4,5,6,7] # Original line
    v['obs_type'].flag_values = np.arange(1,8,dtype='i4') # fix, 6 Nov 2020

    v['obs_type'].flag_meanings = 'zeta ubar vbar u v temperature salinity'
    
    v['obs_provenance'] = nc.createVariable('obs_provenance',np.int32,('datum'))
    v['obs_provenance'].long_name = 'observation origin'
    
    v['obs_time'] = nc.createVariable('obs_time',np.float64,('datum')) 
    v['obs_time'].long_name = 'time of observation'
    v['obs_time'].units = timeUnitsStr
    v['obs_time'].calendar = 'gregorian'  
    
    v['obs_lon'] = nc.createVariable('obs_lon',np.float64,('datum')) 
    v['obs_lon'].long_name = 'observation longitude'
    v['obs_lon'].units = 'degrees_east'
    v['obs_lon'].standard_name = 'longitude'  
    
    v['obs_lat'] = nc.createVariable('obs_lat',np.float64,('datum')) 
    v['obs_lat'].long_name = 'observation latitude'
    v['obs_lat'].units = 'degrees_north'
    v['obs_lat'].standard_name = 'latitude'  
    
    v['obs_depth'] = nc.createVariable('obs_depth',np.float64,('datum')) 
    v['obs_depth'].long_name = 'depth of observation'
    v['obs_depth'].units = 'meters'
    v['obs_depth'].negative = 'downwards'  
    
    v['obs_Xgrid'] = nc.createVariable('obs_Xgrid',np.float64,('datum')) 
    v['obs_Xgrid'].long_name = 'observation fractional x-grid location'
   
    v['obs_Ygrid'] = nc.createVariable('obs_Ygrid',np.float64,('datum')) 
    v['obs_Ygrid'].long_name = 'observation fractional y-grid location'
  
    v['obs_Zgrid'] = nc.createVariable('obs_Zgrid',np.float64,('datum')) 
    v['obs_Zgrid'].long_name = 'observation fractional z-grid location'
    
    v['obs_error'] = nc.createVariable('obs_error',np.float64,('datum')) 
    v['obs_error'].long_name = 'observation error variance'

    v['obs_value'] = nc.createVariable('obs_value',np.float64,('datum')) 
    v['obs_value'].long_name = 'observation value'
    
    # WRITING S:
    v['spherical'][:] = 1
    vlist=['Nobs','survey_time','obs_variance','obs_error',
           'obs_type','obs_provenance','obs_time','obs_lon','obs_lat',
           'obs_depth','obs_Xgrid','obs_Ygrid','obs_Zgrid','obs_value']
    for varName in vlist:
        v[varName][:] = S[varName]
     
    nc.close()

    
        
