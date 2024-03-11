!       Subroutine Name:  nos_ofs_met_write_netcdf_SELFE.f
!
!       Technical Contact(s):   Name:  Aijun Zhang
!                               Org:   NOS/CO-OPS/OD
!                               Phone: 240-533-0591
!                               E-Mail: aijun.zhang@noaa.gov
!
!       Abstract:  this subroutine is used to write surface meteological
!       forcing into NetCDF format for those SELFE-based OFS
!
!       History Log:
!           03/28/2019
!
!       Usage: call nos_ofs_write_netcdf_wind_SELFE from
!       nos_ofs_create_forcing_met_fvcom.f
!
!       Argument Input:
!               netcdf_file - the output NetCDF file
!               ncid       - NetCDF file identity integer
!               imode      - 1, 2, or 3 depending on netcdf_file status
!                IGRD       - indicator of horizontal interpolation method
!                            =0: no interpolation
!                            =1:  remesh using triangulation techniques
!                            =2: bicubic routine from ROMS
!                            =3: bilinear routine from ROMS
!                            =4: nature neighbours
!               IM     - the number of grid in x direction
!               JM      - the number of grid in y direction
!               base_date       - base date for the NetCDF file
!               lon             - longitude
!               lat             - latitude
!               time            - surface forcing time
!               uwind           - eastward_wind
!               vwind           - northward_wind
!               prmsl           - air_pressure_at_sea_level
!               stmp            - air_temperature
!               spfh            - specific_humidity
!               globalstr       - global attributes                                              
!                            
!       Argument Output:
!               netcdf_file     -  the output NetCDF file with values in
!                               targete variables


!  lf95 write_surface_forcing.f -I/usr/local/include -L/usr/local/lib -lnetcdf -o write_surface_forcing.x
c old
c      subroutine write_netCDF_surfaceforcing_ROMS(netcdf_file,ncid,
c     & imode,IGRD,IM,JM,base_date,lon,lat,time,uwind,vwind,pair,
c     &  tair,qair,swrad,lwrad,globalstr)

      subroutine nos_ofs_write_netcdf_wind_SELFE(netcdf_file,ncid,
     & imode,IGRD,IM,JM,base_date,lon,lat,time,uwind,vwind,prmsl,
     &  stmp,spfh,globalstr)

CC     x_rho = IM in x-direction/longitude
CC     y_rho = JM in y-dirrection/latitude

      include 'netcdf.inc'
      CHARACTER*120 TEXT,CNAME,netcdf_file
      INTEGER LEN,base_date(4),intval(4),CORNER(4),COUNT(4)
!! 
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      REAL START_TIME,END_TIME      

C Netcdf dimension ID
      integer ny_grid_dim, nx_grid_dim
      integer time_dim

!! variable ID
      integer time_id,vwind_id,uwind_id
      integer prmsl_id,stmp_id, spfh_id
      integer lon_id,lat_id
      dimension tmp(4)
      character globalstr(9)*120
      real lat(IM,JM),lon(IM,JM),uwind(IM,JM),vwind(IM,JM),
     & prmsl(IM,JM),stmp(IM,JM),spfh(IM,JM)
      logical uwind_L,vwind_L,prmsl_L,stmp_L,spfh_L

C save all dimention id and variable id for next call
      save ny_grid_dim, nx_grid_dim 
      save time_dim

!! variable ID
      save time_id,uwind_id,vwind_id
      save prmsl_id,stmp_id, spfh_id
      save lon_id,lat_id
      save uwind_L,vwind_L,prmsl_L,stmp_L,spfh_L

       print*, 'time_dim==', time_dim 

      if (imode .eq. 1)then 
C Set optional variable flags
      uwind_L = .TRUE.
      vwind_L = .TRUE.
      prmsl_L = .TRUE.
      stmp_L = .TRUE.
      spfh_L = .TRUE.

      print*, 'uwind(1,1)=', uwind(1,1)
      print*, 'vwind(1,1)=', vwind(1,1)
      print*, 'prmsl(1,1)=', prmsl(1,1)
      print*, 'stmp(1,1)=',  stmp(1,1)
      print*, 'spfh(1,1)=',  spfh(1,1)

      if(uwind(1,1) .le. 0)uwind_L = .FALSE.
      if(vwind(1,1) .le. 0)vwind_L = .FALSE.
      if(prmsl(1,1) .le. 0)prmsl_L = .FALSE.
      if(stmp(1,1) .le. 0)stmp_L = .FALSE.
      if(spfh(1,1) .le. 0)spfh_L = .FALSE.

      status = nf_create(trim(netcdf_file), NF_CLOBBER, ncid)
      if (status .ne. NF_NOERR)stop 'netcdf'
      write(6,*)'open netcdf file: ',trim(netcdf_file)

c define dimensions
      IF (IGRD .EQ. 0)THEN
        status = nf_def_dim(ncid, 'ny_grid', JM, ny_grid_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        status = nf_def_dim(ncid, 'nx_grid', IM, nx_grid_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        status = nf_def_dim(ncid,'time',NF_UNLIMITED,time_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define time dimensions: '
      ENDIF

c define variables
C define time 

      status = nf_def_var(ncid,'time', NF_REAL,1, 
     &       time_dim, time_id)
      if (status .ne. NF_NOERR)stop 'netcdf'

      status=nf_put_att_int(ncid, time_id,'base_date',NF_INT,
     &  4,base_date)
      if (status .ne. NF_NOERR)stop 'netcdf'

        WRITE(TEXT,'(a11,I4,2(a1,I2.2))')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3)
        LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, time_id,'units',
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_text(ncid,time_id,'standard_name',4,
     &  'time')
      if (status .ne. NF_NOERR)stop 'netcdf'

        TEXT='Time'
        LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, time_id,'long_name', 
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_real(ncid, time_id, '_FillValue',
     &NF_REAL, 1,-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

c define lat
        intval(3) = time_dim
        intval(2) = ny_grid_dim
        intval(1) = nx_grid_dim
      IF (IGRD .LE. 0)THEN
      status = nf_def_var(ncid, 'lon', NF_REAL, 2,
     &	intval,lon_id)
       if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lon_id, 'units', 12,
     & 'degrees_east')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lon_id, 'standard_name', 9,
     & 'longitude')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lon_id, 'long_name', 9, 
     &'Longitude')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_real(ncid, lon_id, '_FillValue', NF_REAL, 1,
     &-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

c define lon
      status = nf_def_var(ncid, 'lat', NF_REAL, 2,
     &	intval,lat_id)
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lat_id, 'units', 13,
     & 'degrees_north')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lat_id, 'standard_name', 8,
     & 'latitude')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lat_id, 'long_name', 9,
     &'Latitude')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_real(ncid, lat_id, '_FillValue', NF_REAL, 1,
     &-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'
      ENDIF

!! wind vector
      if(uwind_L)THEN
c        intval(3) = time_dim
c        intval(2) = ny_grid_dim
c        intval(1) = nx_grid_dim

        status = nf_def_var(ncid, 'uwind', NF_REAL, 3,intval,uwind_id)
        if (status .ne. NF_NOERR)stop 'netcdf'

        TEXT='m/s'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, uwind_id,'units',
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_text(ncid, uwind_id, 'standard_name',
     &13, 'eastward_wind')
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='Surface Eastward Air Velocity (10m AGL)'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, uwind_id,'long_name', 
     &LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_real(ncid, uwind_id, 
     &'_FillValue', NF_REAL, 1,-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

      endif

      if(vwind_L)THEN
      status = nf_def_var(ncid, 'vwind', NF_REAL, 3,intval,vwind_id)
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='m/s'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, vwind_id,'units',
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_text(ncid,vwind_id,'standard_name',
     &14, 'northward_wind')
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='Surface Northward Air Velocity (10m AGL)'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, vwind_id,'long_name',
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_real(ncid, vwind_id, 
     &'_FillValue', NF_REAL, 1,-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

      endif

! define prmsl
      if(prmsl_L)THEN 

      status = nf_def_var(ncid, 'prmsl', NF_REAL, 3,intval,prmsl_id)
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='Pa'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, prmsl_id,'units',
     &LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_text(ncid, prmsl_id, 'standard_name', 25,
     &'air_pressure_at_sea_level')
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='Pressure reduced to MSL'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, prmsl_id,'long_name', 
     &LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_real(ncid, prmsl_id, 
     &  '_FillValue', NF_REAL, 1,-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

      endif
      
! define stmp 
      if(stmp_L)THEN
      status = nf_def_var(ncid, 'stmp', NF_REAL, 3,intval,stmp_id)
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='K'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, stmp_id,'units',
     &LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_text(ncid, stmp_id, 'standard_name', 15,
     &'air_temperature')
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='Surface Air Temperature (2m AGL)'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, stmp_id,'long_name', 
     &LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_real(ncid, stmp_id, 
     &'_FillValue', NF_REAL, 1,-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

      endif

! define spfh
      if(spfh_L)THEN

      status = nf_def_var(ncid, 'spfh', NF_REAL, 3,intval,spfh_id)
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='1'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, spfh_id,'units',
     &LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_text(ncid, spfh_id, 'standard_name',
     &17, 'specific_humidity')
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='Surface Specific Humidity (2m AGL)'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, spfh_id,'long_name', 
     &LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_real(ncid, spfh_id, 
     &'_FillValue', NF_REAL, 1,-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

      endif

C Global Attributes
       TEXT=trim(globalstr(1))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid,NF_GLOBAL ,'type', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(2))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'title', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(3))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'source', 
     &       LEN,TRIM(TEXT))

      TEXT=trim(globalstr(4))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'grib2_file', 
     &       LEN,TRIM(TEXT))

      TEXT=trim(globalstr(5))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'grid_info', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(6))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'model_grid_file', 
     &       LEN,TRIM(TEXT))
     
      TEXT=trim(globalstr(7))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'history', 
     &       LEN,TRIM(TEXT))
!      TEXT='Created by Gregory Mott, OD/CO-OPS/NOS/NOAA'
      TEXT=trim(globalstr(8))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'reference', 
     &       LEN,TRIM(TEXT))
     
      status=nf_enddef(ncid)

C    write 2D fields lon,lat,mask,depth  
      IF(IGRD .LE. 0)THEN
         CORNER(1) = 1
         CORNER(2) = 1
         COUNT(1)=IM
         COUNT(2)=JM
         status=nf_put_vara_real(ncid,lon_id,CORNER,COUNT,lon)
         call check_err(status)
         status=nf_put_vara_real(ncid,lat_id,CORNER,COUNT,lat)
         call check_err(status)
	 write(*,*)'end of dimension and variable definition'
      ENDIF 
!!  end of variable definition

      elseif (imode.eq.2) then
c       Inquire of this file what the last itime written was
C       should be equal to the dimension of time
        status= nf_inq_dimlen(ncid,time_dim,itime)      
        if (status .ne. NF_NOERR)stop 'inquire time'

        itime=itime+1

          print*, 'itime==', itime
c scalars
        CORNER(1) = itime
        status=nf_put_var1_real(ncid,time_id,CORNER,time)
        if (status .ne. NF_NOERR)stop 'put time' 

        print*, 'airmet'

C    write 2D fields the real array defined in netcdf is 3-D =(x,y,time)
        CORNER(1) = 1
        CORNER(2) = 1
        CORNER(3) = itime
        COUNT(1)=IM
        COUNT(2)=JM
        COUNT(3)=1      !  for one time slice
	if(uwind_L)THEN 
          status=nf_put_vara_real(ncid,uwind_id,CORNER,COUNT,uwind)
          if (status .ne. NF_NOERR)stop 'write uwind'
        endif
	if(vwind_L)THEN
          status=nf_put_vara_real(ncid,vwind_id,CORNER,COUNT,vwind)
          if (status .ne. NF_NOERR)stop 'write vwind'
        endif
	if(prmsl_L)THEN
          status=nf_put_vara_real(ncid,prmsl_id,CORNER,COUNT,prmsl)
          if (status .ne. NF_NOERR)stop 'write prmsl'
        endif
	if(stmp_L)THEN
          status=nf_put_vara_real(ncid,stmp_id,CORNER,COUNT,stmp)
          if (status .ne. NF_NOERR)stop 'write stmp'
	endif
	if(spfh_L)then  
          status=nf_put_vara_real(ncid,spfh_id,CORNER,COUNT,spfh)
          if (status .ne. NF_NOERR)stop 'write spfh'
	endif
	
         print*, 'before return'
      elseif (imode.eq.3) then
        write(*,*)'The netCDF file is closed'
        status=NF_CLOSE(ncid)
      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      subroutine check_err(status)
      integer status
      include 'netcdf.inc'
      if (status .ne. NF_NOERR) then
      print *,'status=',status
      print *, nf_strerror(status)
      stop
      endif
      return
      end

!  lf95 write_surface_forcing.f -I/usr/local/include -L/usr/local/lib -lnetcdf -o write_surface_forcing.x
c old
c      subroutine write_netCDF_surfaceforcing_ROMS(netcdf_file,ncid,
c     & imode,IGRD,IM,JM,base_date,lon,lat,time,uwind,vwind,pair,
c     &  tair,qair,swrad,lwrad,globalstr)

      subroutine nos_ofs_write_netcdf_flux_SELFE(netcdf_file,ncid,
     & imode,IGRD,IM,JM,base_date,lon,lat,time,
     &  dlwrf,dswrf,globalstr)

CC     x_rho = IM in x-direction/longitude
CC     y_rho = JM in y-dirrection/latitude

      include 'netcdf.inc'
      CHARACTER*120 TEXT,CNAME,netcdf_file
      INTEGER LEN,base_date(4),intval(4),CORNER(4),COUNT(4)
!! 
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      REAL START_TIME,END_TIME      

C Netcdf dimension ID
      integer ny_grid_dim, nx_grid_dim
      integer time_dim

!! variable ID
      integer time_id,dlwrf_id,dswrf_id
      integer lon_id,lat_id
      dimension tmp(4)
      character globalstr(9)*120
      real lat(IM,JM),lon(IM,JM),dlwrf(IM,JM),dswrf(IM,JM)
      logical dlwrf_L,dswrf_L

C save all dimention id and variable id for next call
      save ny_grid_dim, nx_grid_dim 
      save time_dim

!! variable ID
      save time_id,dlwrf_id,dswrf_id
      save lon_id,lat_id
      save dlwrf_L,dswrf_L

       print*, 'time_dim==', time_dim 

      if (imode .eq. 1)then 
C Set optional variable flags
      dlwrf_L = .TRUE.
      dswrf_L = .TRUE.

      print*, 'dlwrf(1,1)=', dlwrf(1,1)
      print*, 'dswrf(1,1)=', dswrf(1,1)

      if(dswrf(1,1) .le. 0)dswrf_L = .FALSE.
      if(dlwrf(1,1) .le. 0)dlwrf_L = .FALSE.

      status = nf_create(trim(netcdf_file), NF_CLOBBER, ncid)
      if (status .ne. NF_NOERR)stop 'netcdf'
      write(6,*)'open netcdf file: ',trim(netcdf_file)

c define dimensions
      IF (IGRD .EQ. 0)THEN
        status = nf_def_dim(ncid, 'ny_grid', JM, ny_grid_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        status = nf_def_dim(ncid, 'nx_grid', IM, nx_grid_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        status = nf_def_dim(ncid,'time',NF_UNLIMITED,time_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define time dimensions: '
      ENDIF

c define variables
C define time 

      status = nf_def_var(ncid,'time', NF_REAL,1, 
     &       time_dim, time_id)
      if (status .ne. NF_NOERR)stop 'netcdf'

      status=nf_put_att_int(ncid, time_id,'base_date',NF_INT,
     &  4,base_date)
      if (status .ne. NF_NOERR)stop 'netcdf'

        WRITE(TEXT,'(a11,I4,2(a1,I2.2))')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3)
        LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, time_id,'units',
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_text(ncid,time_id,'standard_name',4,
     &  'time')
      if (status .ne. NF_NOERR)stop 'netcdf'

        TEXT='Time'
        LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, time_id,'long_name', 
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_real(ncid, time_id, '_FillValue',
     &NF_REAL, 1,-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

c define lat
        intval(3) = time_dim
        intval(2) = ny_grid_dim
        intval(1) = nx_grid_dim
      IF (IGRD .LE. 0)THEN
      status = nf_def_var(ncid, 'lon', NF_REAL, 2,
     &	intval,lon_id)
       if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lon_id, 'units', 12,
     & 'degrees_east')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lon_id, 'standard_name', 9,
     & 'longitude')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lon_id, 'long_name', 9, 
     &'Longitude')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_real(ncid, lon_id, '_FillValue', NF_REAL, 1,
     &-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

c define lon
      status = nf_def_var(ncid, 'lat', NF_REAL, 2,
     &	intval,lat_id)
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lat_id, 'units', 13,
     & 'degrees_north')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lat_id, 'standard_name', 8,
     & 'latitude')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lat_id, 'long_name', 9,
     &'Latitude')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_real(ncid, lat_id, '_FillValue', NF_REAL, 1,
     &-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'
      ENDIF

!! wind vector
      if(dlwrf_L)THEN
c        intval(3) = time_dim
c        intval(2) = ny_grid_dim
c        intval(1) = nx_grid_dim

        status = nf_def_var(ncid, 'dlwrf', NF_REAL, 3,intval,dlwrf_id)
        if (status .ne. NF_NOERR)stop 'netcdf'

        TEXT='W/m^2'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, dlwrf_id,'units',
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_text(ncid, dlwrf_id, 'standard_name',
     &40, 'surface_downwelling_longwave_flux_in_air')
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='Downward Long Wave Radiation Flux'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, dlwrf_id,'long_name', 
     &LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_real(ncid, dlwrf_id, 
     &'_FillValue', NF_REAL, 1,-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

      endif

      if(dswrf_L)THEN
      status = nf_def_var(ncid, 'dswrf', NF_REAL, 3,intval,dswrf_id)
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='W/m^2'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, dswrf_id,'units',
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_text(ncid,dswrf_id,'standard_name',
     &40, 'surface_downwelling_shortwave_flux_in_air')
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='Downward Short Wave Radiation Flux'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, dswrf_id,'long_name',
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_real(ncid, dswrf_id, 
     &'_FillValue', NF_REAL, 1,-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

      endif

C Global Attributes
       TEXT=trim(globalstr(1))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid,NF_GLOBAL ,'type', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(2))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'title', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(3))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'source', 
     &       LEN,TRIM(TEXT))

      TEXT=trim(globalstr(4))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'grib2_file', 
     &       LEN,TRIM(TEXT))

      TEXT=trim(globalstr(5))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'grid_info', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(6))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'model_grid_file', 
     &       LEN,TRIM(TEXT))
     
      TEXT=trim(globalstr(7))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'history', 
     &       LEN,TRIM(TEXT))
      TEXT='Created by Gregory Mott, OD/CO-OPS/NOS/NOAA'
      TEXT=trim(globalstr(8))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'reference', 
     &       LEN,TRIM(TEXT))
     
      status=nf_enddef(ncid)

C    write 2D fields lon,lat,mask,depth  
      IF(IGRD .LE. 0)THEN
         CORNER(1) = 1
         CORNER(2) = 1
         COUNT(1)=IM
         COUNT(2)=JM
         status=nf_put_vara_real(ncid,lon_id,CORNER,COUNT,lon)
         call check_err(status)
         status=nf_put_vara_real(ncid,lat_id,CORNER,COUNT,lat)
         call check_err(status)
	 write(*,*)'end of dimension and variable definition'
      ENDIF 
!!  end of variable definition

      elseif (imode.eq.2) then
c       Inquire of this file what the last itime written was
C       should be equal to the dimension of time
        status= nf_inq_dimlen(ncid,time_dim,itime)      
        if (status .ne. NF_NOERR)stop 'inquire time'

        itime=itime+1

          print*, 'itime==', itime
c scalars
        CORNER(1) = itime
        status=nf_put_var1_real(ncid,time_id,CORNER,time)
        if (status .ne. NF_NOERR)stop 'put time' 

        print*, 'fluxmet'


C    write 2D fields the real array defined in netcdf is 3-D =(x,y,time)
        CORNER(1) = 1
        CORNER(2) = 1
        CORNER(3) = itime
        COUNT(1)=IM
        COUNT(2)=JM
        COUNT(3)=1      !  for one time slice
	if(dlwrf_L)THEN 
          status=nf_put_vara_real(ncid,dlwrf_id,CORNER,COUNT,dlwrf)
          if (status .ne. NF_NOERR)stop 'write dlwrf'
        endif
	if(dswrf_L)THEN
          status=nf_put_vara_real(ncid,dswrf_id,CORNER,COUNT,dswrf)
          if (status .ne. NF_NOERR)stop 'write dswrf'
        endif
	
         print*, 'before return'
      elseif (imode.eq.3) then
        write(*,*)'The netCDF file is closed'
        status=NF_CLOSE(ncid)
      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
c      subroutine check_err(status)
c      integer status
c      include 'netcdf.inc'
c      if (status .ne. NF_NOERR) then
c      print *,'status=',status
c      print *, nf_strerror(status)
c      stop
c      endif
c      return
c      end

!  lf95 write_surface_forcing.f -I/usr/local/include -L/usr/local/lib -lnetcdf -o write_surface_forcing.x
c old
c      subroutine write_netCDF_surfaceforcing_ROMS(netcdf_file,ncid,
c     & imode,IGRD,IM,JM,base_date,lon,lat,time,uwind,vwind,pair,
c     &  tair,qair,swrad,lwrad,globalstr)

      subroutine nos_ofs_write_netcdf_prate_SELFE(netcdf_file,ncid,
     & imode,IGRD,IM,JM,base_date,lon,lat,time,prate,
     &globalstr)

CC     x_rho = IM in x-direction/longitude
CC     y_rho = JM in y-dirrection/latitude

      include 'netcdf.inc'
      CHARACTER*120 TEXT,CNAME,netcdf_file
      INTEGER LEN,base_date(4),intval(4),CORNER(4),COUNT(4)
!! 
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      REAL START_TIME,END_TIME      

C Netcdf dimension ID
      integer ny_grid_dim, nx_grid_dim
      integer time_dim

!! variable ID
      integer time_id,prate_id
      integer lon_id,lat_id
      dimension tmp(4)
      character globalstr(9)*120
      real lat(IM,JM),lon(IM,JM),prate(IM,JM)
      logical prate_L

C save all dimention id and variable id for next call
      save ny_grid_dim, nx_grid_dim 
      save time_dim

!! variable ID
      save time_id,prate_id
      save lon_id,lat_id
      save prate_L

       print*, 'time_dim==', time_dim 

      if (imode .eq. 1)then 
C Set optional variable flags
      prate_L = .TRUE.

      print*, 'prate(1,1)=', prate(1,1)

      if(prate(1,1) .le. 0)prate_L = .FALSE.

      status = nf_create(trim(netcdf_file), NF_CLOBBER, ncid)
      if (status .ne. NF_NOERR)stop 'netcdf'
      write(6,*)'open netcdf file: ',trim(netcdf_file)

c define dimensions
      IF (IGRD .EQ. 0)THEN
        status = nf_def_dim(ncid, 'ny_grid', JM, ny_grid_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        status = nf_def_dim(ncid, 'nx_grid', IM, nx_grid_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        status = nf_def_dim(ncid,'time',NF_UNLIMITED,time_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define time dimensions: '
      ENDIF

c define variables
C define time 

      status = nf_def_var(ncid,'time', NF_REAL,1, 
     &       time_dim, time_id)
      if (status .ne. NF_NOERR)stop 'netcdf'

      status=nf_put_att_int(ncid, time_id,'base_date',NF_INT,
     &  4,base_date)
      if (status .ne. NF_NOERR)stop 'netcdf'

        WRITE(TEXT,'(a11,I4,2(a1,I2.2))')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3)
        LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, time_id,'units',
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_text(ncid,time_id,'standard_name',4,
     &  'time')
      if (status .ne. NF_NOERR)stop 'netcdf'

        TEXT='Time'
        LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, time_id,'long_name', 
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_real(ncid, time_id, '_FillValue',
     &NF_REAL, 1,-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

c define lat
        intval(3) = time_dim
        intval(2) = ny_grid_dim
        intval(1) = nx_grid_dim
      IF (IGRD .LE. 0)THEN
      status = nf_def_var(ncid, 'lon', NF_REAL, 2,
     &	intval,lon_id)
       if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lon_id, 'units', 12,
     & 'degrees_east')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lon_id, 'standard_name', 9,
     & 'longitude')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lon_id, 'long_name', 9, 
     &'Longitude')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_real(ncid, lon_id, '_FillValue', NF_REAL, 1,
     &-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

c define lon
      status = nf_def_var(ncid, 'lat', NF_REAL, 2,
     &	intval,lat_id)
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lat_id, 'units', 13,
     & 'degrees_north')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lat_id, 'standard_name', 8,
     & 'latitude')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_text(ncid, lat_id, 'long_name', 9,
     &'Latitude')
      if (status .ne. NF_NOERR)stop 'netcdf'
      status = nf_put_att_real(ncid, lat_id, '_FillValue', NF_REAL, 1,
     &-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'
      ENDIF

!! wind vector
      if(prate_L)THEN
c        intval(3) = time_dim
c        intval(2) = ny_grid_dim
c        intval(1) = nx_grid_dim

        status = nf_def_var(ncid,'prate',NF_REAL,3,intval,prate_id)
        if (status .ne. NF_NOERR)stop 'netcdf'

        TEXT='kg/m^2/s'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, prate_id,'units',
     &       LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_text(ncid, prate_id, 'standard_name',
     &18, 'precipitation_flux')
      if (status .ne. NF_NOERR)stop 'netcdf'

      TEXT='Surface Precipitation Rate'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, prate_id,'long_name', 
     &LEN,TRIM(TEXT))
      if (status .ne. NF_NOERR)stop 'netcdf'

      status = nf_put_att_real(ncid, prate_id, 
     &'_FillValue', NF_REAL, 1,-9999.0)
      if (status .ne. NF_NOERR)stop 'netcdf'

      endif

C Global Attributes
       TEXT=trim(globalstr(1))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid,NF_GLOBAL ,'type', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(2))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'title', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(3))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'source', 
     &       LEN,TRIM(TEXT))

      TEXT=trim(globalstr(4))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'grib2_file', 
     &       LEN,TRIM(TEXT))

      TEXT=trim(globalstr(5))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'grid_info', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(6))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'model_grid_file', 
     &       LEN,TRIM(TEXT))
     
      TEXT=trim(globalstr(7))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'history', 
     &       LEN,TRIM(TEXT))
      TEXT='Created by Gregory Mott, OD/CO-OPS/NOS/NOAA'
      TEXT=trim(globalstr(8))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'reference', 
     &       LEN,TRIM(TEXT))
     
      status=nf_enddef(ncid)

C    write 2D fields lon,lat,mask,depth  
      IF(IGRD .LE. 0)THEN
         CORNER(1) = 1
         CORNER(2) = 1
         COUNT(1)=IM
         COUNT(2)=JM
         status=nf_put_vara_real(ncid,lon_id,CORNER,COUNT,lon)
         call check_err(status)
         status=nf_put_vara_real(ncid,lat_id,CORNER,COUNT,lat)
         call check_err(status)
	 write(*,*)'end of dimension and variable definition'
      ENDIF 
!!  end of variable definition

      elseif (imode.eq.2) then
c       Inquire of this file what the last itime written was
C       should be equal to the dimension of time
        status= nf_inq_dimlen(ncid,time_dim,itime)      
        if (status .ne. NF_NOERR)stop 'inquire time'

        itime=itime+1

          print*, 'itime==', itime
c scalars
        CORNER(1) = itime
        status=nf_put_var1_real(ncid,time_id,CORNER,time)
        if (status .ne. NF_NOERR)stop 'put time' 

        print*, 'precipmet'

C    write 2D fields the real array defined in netcdf is 3-D =(x,y,time)
        CORNER(1) = 1
        CORNER(2) = 1
        CORNER(3) = itime
        COUNT(1)=IM
        COUNT(2)=JM
        COUNT(3)=1      !  for one time slice
	if(prate_L)THEN 
          status=nf_put_vara_real(ncid,prate_id,CORNER,COUNT,prate)
          if (status .ne. NF_NOERR)stop 'write prate'
        endif
	
         print*, 'before return'
      elseif (imode.eq.3) then
        write(*,*)'The netCDF file is closed'
        status=NF_CLOSE(ncid)
      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
c      subroutine check_err(status)
c      integer status
c      include 'netcdf.inc'
c      if (status .ne. NF_NOERR) then
c      print *,'status=',status
c      print *, nf_strerror(status)
c      stop
c      endif
c      return
c      end
