# -*- coding: utf-8 -*-
"""
Created on Tue Nov 10 16:04:19 2020

@author: Alexander.Kurapov
"""

# merge ROMS station files
# NOTES:
# (1) Before merging, the files are checked to ensure
# that the number of stations are the same in each file
# and that Ipos and Jpos coordinates are the same in each file
# (2) It only merges the netcdf arrays in which the time dimension is
# the leading dimension (in python / netcdf convention)

import netCDF4 as n4
import numpy as np
import shutil

# To run this script from the unix shell providing the file names 
# as arguments in the command line, uncomment lines, from "import sys"
# to "fnameOUT=sys...." and comment lines "fname1=.." thru "fnameOUT..." 

import sys,os
ocean_model=os.environ["OCEAN_MODEL"]
fname1=sys.argv[1]
fname2=sys.argv[2]
fnameOUT=sys.argv[3]

#InDir="/gpfs/dell2/nos/noscrub/Jiangtao.Xu/tmp/"
#fname1=InDir + "nos.wcofs.stations.forecast.20201110.t03z.nc.1"
#fname2=InDir + "nos.wcofs.stations.forecast.20201110.t03z.nc.2"
#fnameOUT=InDir + "test.nc"

if ocean_model.upper()=='ROMS':
   timeDimName='ocean_time'
elif ocean_model.upper()=='FVCOM':
   timeDimName='time'
else:
   timeDimName='time'
   print('WARNING: use default time dimension name "time"!') 

# END USER INPUTS ^^^^^^^^^^^^^^

nc=n4.Dataset(fname1,'r')
t1=nc.variables[timeDimName][:]
nc.close()
nt1=t1.size

nc=n4.Dataset(fname2,'r')
t2=nc.variables[timeDimName][:]
nc.close()
nt2=t2.size

print('merging...')
shutil.copy(fname1,fnameOUT)

# - find it0: the ocean_time dim position in file 1 
# corresponding to the initial time from file 2
dt=t1[1]-t1[0]
#it0=int(round((t2[0]-t1[0])/dt))
it0=int((t2[0]-t1[0])/dt)
ii=it0+np.arange(nt2,dtype=int)
        
nc1=n4.Dataset(fnameOUT,'r+')
nc2=n4.Dataset(fname2,'r')
        
# check every variable in the file
# if the variable is an array (has 1 or more dimensions),
# check if the 0th dimension (python convention) is
# = timeDimName (defined above, e.g., 'ocean_time')
       
# Note: below 'key' is any variable in the netcdf file
for key in nc2.variables:
    dims=nc2.variables[key].dimensions
    if (len(dims)>0 ):                
        if dims[0]==timeDimName:
            print(key)
                    
            # reading the array from the second file
            F=nc2.variables[key][:]
                    
            # assigning the values to the output file object                     
            # - note: apparently the next line works for any 
            # arrays of any number of dimensions
            # Here ii refers to the time variable (always the 0th 
            # dim)
            nc1.variables[key][ii]=F
nc1.close()
nc2.close()

