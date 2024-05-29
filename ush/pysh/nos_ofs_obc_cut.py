import netCDF4 as nc
import numpy as np

with open('obc.file.name.python', 'r') as f:
      lines = f.read().splitlines()
      for nfile in lines:
              print("== mmgp station is ==",nfile)

print(nfile)

ds = nc.Dataset(nfile, 'r+')

nobc=len(ds.dimensions['nobc'])
nk=len(ds.dimensions['siglay'])
t = ds['time'][:]
ntime=len(t)
h= ds['h'][:]
siglay=ds['siglay'][:]
temp=ds['obc_temp'][:]
temp1=temp

print(nobc,nk,ntime)
for tt in range(ntime):
    for n in range(nobc):
        for k in range(nk):
            if h[n]*(-1.0)*siglay[k,n] < 125.0:
                ds['obc_temp'][tt,k,n]=temp[tt,k,n]
                ttt=temp[tt,k,n]
                

            else:
                ds['obc_temp'][tt,k,n]=ttt

print("===== mmgp === obc file has been changed ===")
ds.close()




