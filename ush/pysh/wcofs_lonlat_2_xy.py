# -*- coding: utf-8 -*-
"""
Created on Thu Jul 25 17:17:38 2019

@author: Alexander.Kurapov
"""

def wcofs_lonlat_2_xy(lon,lat,regridYES):
    # Arrays of "local" coordinates x and y of WCOFS, which is 
    # a spherical grid in rotated coordinates with the pole at phi0,theta0
    # x increases along the local latitude
    # y increases in the direction opposite to local longitude
    #
    # if regridYES==1 then
    # x and y are regularized using meshgrid to eliminate roundup errors and
    # make x and y suitable to interp2. 

    # the displaced pole for the WCOFS grid

    import numpy as np

    theta0 =  37.4
    phi0   = -57.6

    # First, lonlat_2_rotgrd.m translated to python
    phi   = lon*np.pi/180
    theta = lat*np.pi/180
    phi0   = phi0*np.pi/180
    theta0 = theta0*np.pi/180

    phi1 = phi - phi0

    phi2=np.arctan2(np.sin(phi1)*np.cos(theta),
          np.cos(phi1)*np.cos(theta)*np.sin(theta0)-np.sin(theta)*np.cos(theta0))
    theta2=np.arcsin(np.cos(phi1)*np.cos(theta)*np.cos(theta0)+np.sin(theta)*np.sin(theta0))

    phi2=phi2*180/np.pi
    theta2=theta2*180/np.pi    
    
    xy = {}
    
    if regridYES:
        xy['x'] , xy['y'] = np.meshgrid(theta2[1,:],-phi2[:,1])
    
    else:
        xy['x']=theta2
        xy['y']=-phi2
            
    return xy
    