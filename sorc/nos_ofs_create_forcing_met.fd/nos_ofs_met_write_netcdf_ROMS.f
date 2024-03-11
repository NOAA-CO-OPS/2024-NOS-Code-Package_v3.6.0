!       Program Name:  nos_ofs_met_write_netcdf_ROMS.f
!       
!       Technical Contact(s):   Name:  Aijun Zhang
!                               Org:   NOS/CO-OPS/OD
!                               Phone: 240-533-0591
!                               E-Mail: aijun.zhang@noaa.gov
!
!       Abstract:  this subroutine is used to write surface meteological
!       forcing into NetCDF format for those ROMS-based OFS
!
!       History Log:
!           03/28/2019
!
!
!       Usage: call write_netCDF_surfaceforcing_ROMS from
!       nos_ofs_create_forcing_met.f
!
!       Argument Input: 
!                netcdf_file - the output NetCDF file
!                imode      - equals 1, 2, or 3 depending on netcdf_file
!                             status
!                ncid       - NetCDF file identity integer   
!                IGRD       - indicator of horizontal interpolation method
!                            =0: no interpolation
!                            =1:  remesh using triangulation techniques
!                            =2: bicubic routine from ROMS
!                            =3: bilinear routine from ROMS
!                            =4: nature neighbours
!                IM         - Model grid index in x direction
!                JM          - Model grid index in y direction
!                base_date   - Base date for the NetCDF file  
!                lon        - longitude at ROMS model grid
!                lat        - latitude at ROMS model grid
!                frc_time   - surface forcing time
!                uwind      - u-component of 10m surface wind
!                vwind      - v-component of 10m surface wind
!                pair       - surface air pressure
!                tair       - surface air temperature
!                qair       - surface air relative humidity
!                swrad      - Net solar shortwave radiation flux                      
!                lwrad      - downward solar longwave radiation flux                 
!                rain       - Precipation rate
!                tcdc       - total cloud cover
!                evp        - evaporation
!                globalstr  - global attributes    
!
!      Argument Output:  
!               netcdf_file - the output NetCDF file with values in targeted
!               variables
!
!  lf95 write_surface_forcing.f -I/usr/local/include -L/usr/local/lib -lnetcdf -o write_surface_forcing.x
      subroutine write_netCDF_surfaceforcing_ROMS(netcdf_file,ncid,
     & imode,IGRD,IM,JM,base_date,lon,lat,frc_time,uwind,vwind,pair,
     &  tair,qair,swrad,lwrad,rain,tcdc,evp,globalstr)
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
      integer y_rho_dim,y_u_dim,y_v_dim
      integer x_rho_dim,x_u_dim,x_v_dim
      integer s_rho_dim,s_w_dim,time_dim
      integer one_dim,ocean_time_dim
!! variable ID
      integer frc_time_id,Uwind_id,Vwind_id,pair_id,tair_id,qair_id
      integer swrad_id,lwrad_id,rain_id,tcdc_id,evp_id
      integer lon_id,lat_id
      dimension tmp(4)
      character globalstr(9)*120
      real lat(IM,JM),lon(IM,JM),uwind(IM,JM),vwind(IM,JM),
     & pair(IM,JM),tair(IM,JM),qair(IM,JM),swrad(IM,JM),lwrad(IM,JM),
     & rain(IM,JM),tcdc(IM,JM),evp(IM,JM)

      logical Uwind_L,Vwind_L,pair_L,tair_L,qair_L
      logical swrad_L,lwrad_L,rain_L,tcdc_L,evp_L
C save all dimention id and variable id for next call
      save y_rho_dim,y_u_dim,y_v_dim
      save x_rho_dim,x_u_dim,x_v_dim
      save s_rho_dim,s_w_dim,time_dim
      save one_dim,ocean_time_dim
!! variable ID
      save frc_time_id,Uwind_id,Vwind_id,pair_id,tair_id,qair_id
      save swrad_id,lwrad_id,rain_id,tcdc_id,evp_id
      save lon_id,lat_id
      save Uwind_L,Vwind_L,pair_L,tair_L,qair_L
      save swrad_L,lwrad_L,rain_L,tcdc_L,evp_L
      if (imode .eq. 1)then 
C Set optional variable flags
      Uwind_L = .TRUE.
      Vwind_L = .TRUE.
      pair_L = .TRUE.
      tair_L = .TRUE.
      qair_L = .TRUE.
      swrad_L = .TRUE.
      lwrad_L = .TRUE.
      rain_L = .TRUE.
      tcdc_L = .TRUE.
      evp_L = .TRUE.
      if(Uwind(1,1) .le. 0)Uwind_L = .FALSE.
      if(Vwind(1,1) .le. 0)Vwind_L = .FALSE.
      if(pair(1,1) .le. 0)pair_L = .FALSE.
      if(tair(1,1) .le. 0)tair_L = .FALSE.
      if(qair(1,1) .le. 0)qair_L = .FALSE.
      if(swrad(1,1).le. 0)swrad_L = .FALSE.
      if(lwrad(1,1).le. 0)lwrad_L = .FALSE.
      if(rain(1,1) .le. 0)rain_L = .FALSE.
      if(tcdc(1,1) .le. 0)tcdc_L = .FALSE.
      if(evp(1,1) .le. 0)evp_L = .FALSE.

      status = nf_create(trim(netcdf_file), NF_CLOBBER, ncid)
      if (status .ne. NF_NOERR)stop 'netcdf'
      write(6,*)'open netcdf file: ',trim(netcdf_file)
c define dimensions
      IF (IGRD .EQ. 0)THEN
        status = nf_def_dim(ncid, 'y_rho', JM, y_rho_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define y_rho dimensions: '
        status = nf_def_dim(ncid, 'y_u', JM, y_u_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define y_u dimensions: '
        status = nf_def_dim(ncid, 'y_v', JM-1, y_v_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define y_v dimensions: '
        status = nf_def_dim(ncid, 'x_rho', IM, x_rho_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define x_rho dimensions: '
        status = nf_def_dim(ncid, 'x_u', IM-1, x_u_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define x_u dimensions: '
        status = nf_def_dim(ncid, 'x_v', IM, x_v_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define x_v dimensions: '
        status = nf_def_dim(ncid, 'one', 1, one_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define one dimensions: '
        status = nf_def_dim(ncid,'time',NF_UNLIMITED,time_dim)
        call check_err(status)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define time dimensions: '
      ELSE
        status = nf_def_dim(ncid, 'eta_rho', JM, y_rho_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define eta_rho dimensions: '
        status = nf_def_dim(ncid, 'eta_u', JM, y_u_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define eta_u dimensions: '
        status = nf_def_dim(ncid, 'eta_v', JM-1, y_v_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define eta_v dimensions: '
        status = nf_def_dim(ncid, 'xi_rho', IM, x_rho_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define xi_rho dimensions: '
        status = nf_def_dim(ncid, 'xi_u', IM-1, x_u_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define xi_u dimensions: '
        status = nf_def_dim(ncid, 'xi_v', IM, x_v_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define xi_v dimensions: '
        status = nf_def_dim(ncid, 'one', 1, one_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define one dimensions: '
        status = nf_def_dim(ncid,'time',NF_UNLIMITED,time_dim)
        call check_err(status)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define time dimensions: '
      ENDIF

c         define variables
C define frc_time 
        status = nf_def_var(ncid,'frc_time', NF_REAL,1, 
     &       time_dim, frc_time_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='surface forcing time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, frc_time_id,'long_name', 
     &       LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3),' ',base_date(4),':00:00'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, frc_time_id,'units', 
     &       LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='surface forcing time,scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, frc_time_id,'field', 
     &       LEN,TRIM(TEXT))
!        status=nf_put_att_int(ncid, frc_time_id,'base_date',NF_INT,
!     &  4,base_date)
!! define lon_native lat_native
        intval(3) = time_dim
        intval(2) = y_rho_dim
        intval(1) = x_rho_dim
      IF (IGRD .LE. 0)THEN
        status = nf_def_var(ncid, 'lon_native', NF_REAL, 2,
     &	intval,lon_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        tmp(1)=-133.459
        tmp(2)=-65.12556
        tmp(3)=-49.416
        tmp(4)=-152.8786
        status=nf_put_att_real(ncid, lon_id,'corners',
     &   NF_FLOAT,4,tmp)
     
        TEXT='longitude on native grid'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lon_id,'long_name', 
     &       LEN,TRIM(TEXT))
     
        TEXT='Lambert Conformal (secant, tangent, conical or bipolar)'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lon_id,'grid_type', 
     &       LEN,TRIM(TEXT))
        TEXT='degree_east'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lon_id,'units', 
     &       LEN,TRIM(TEXT))
        TEXT='lon, scalar'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lon_id,'field', 
     &       LEN,TRIM(TEXT))
        status=nf_put_att_real(ncid, lon_id,'Latin1',NF_FLOAT,1,25.0)
        status=nf_put_att_real(ncid, lon_id,'Latin2',NF_FLOAT,1,25.0)
        status=nf_put_att_real(ncid, lon_id,'Dx',NF_FLOAT,1,12.191)
        status=nf_put_att_real(ncid, lon_id,'Dy',NF_FLOAT,1,12.191)
        status=nf_put_att_real(ncid, lon_id,'LoV',NF_FLOAT,1,265.0)
        status=nf_put_att_real(ncid, lon_id,'Lo1',NF_FLOAT,1,226.541)
        status=nf_put_att_real(ncid, lon_id,'La1',NF_FLOAT,1,12.19)
	
        status = nf_def_var(ncid, 'lat_native', NF_REAL, 2,
     &	intval,lat_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        tmp(1)=12.19
        tmp(2)=14.34209
        tmp(3)=57.32843
        tmp(4)=54.56534
        status=nf_put_att_real(ncid, lat_id,'corners',
     &   NF_FLOAT,4,tmp)
        TEXT='latitude on native grid'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lat_id,'long_name', 
     &       LEN,TRIM(TEXT))
        TEXT='Lambert Conformal (secant, tangent, conical or bipolar)'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lat_id,'grid_type', 
     &       LEN,TRIM(TEXT))
        TEXT='degrees_north'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lat_id,'units', 
     &       LEN,TRIM(TEXT))
        TEXT='lat, scalar'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lat_id,'field', 
     &       LEN,TRIM(TEXT))
        status=nf_put_att_real(ncid, lat_id,'Latin1',NF_FLOAT,1,25.0)
        status=nf_put_att_real(ncid, lat_id,'Latin2',NF_FLOAT,1,25.0)
        status=nf_put_att_real(ncid, lat_id,'Dx',NF_FLOAT,1,12.191)
        status=nf_put_att_real(ncid, lat_id,'Dy',NF_FLOAT,1,12.191)
        status=nf_put_att_real(ncid, lat_id,'LoV',NF_FLOAT,1,265.0)
        status=nf_put_att_real(ncid, lat_id,'Lo1',NF_FLOAT,1,226.541)
        status=nf_put_att_real(ncid, lat_id,'La1',NF_FLOAT,1,12.19)
      ENDIF
!! wind vector
      if(Uwind_L)THEN
!        intval(3) = time_dim
!        intval(2) = y_rho_dim
!        intval(1) = x_rho_dim
        status = nf_def_var(ncid, 'Uwind', NF_REAL, 3,intval,Uwind_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='Operational products'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, Uwind_id,'production_status', 
     &       LEN,TRIM(TEXT))
        TEXT='u-component of 10m surface wind'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, Uwind_id,'long_name', 
     &       LEN,TRIM(TEXT))
        TEXT='meter second-1'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, Uwind_id,'units', 
     &       LEN,TRIM(TEXT))
        TEXT='frc_time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, Uwind_id,'time', 
     &       LEN,TRIM(TEXT))
        IF(IGRD .LE. 0)THEN
          TEXT='lon_native lat_native'
          LEN=LEN_TRIM(TEXT)
          status = nf_put_att_text(ncid, Uwind_id,'coordinates', 
     &       LEN,TRIM(TEXT))
        ENDIF
        TEXT='Uwind, scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, Uwind_id,'field', 
     &       LEN,TRIM(TEXT))
!        status = nf_put_att_real(ncid, Uwind_id, 
!     &  '_FillValue', NF_REAL, 1,-99999.0)
      endif
      if(Vwind_L)THEN
        status = nf_def_var(ncid, 'Vwind', NF_REAL, 3,intval,Vwind_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='Operational products'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, Vwind_id,'production_status', 
     &       LEN,TRIM(TEXT))
        TEXT='v-component of 10m surface wind'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, vwind_id,'long_name', 
     &       LEN,TRIM(TEXT))
        TEXT='meter second-1'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, vwind_id,'units', 
     &       LEN,TRIM(TEXT))
        TEXT='frc_time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, vwind_id,'time', 
     &       LEN,TRIM(TEXT))
        IF(IGRD .LE. 0)THEN
          TEXT='lon_native lat_native'
          LEN=LEN_TRIM(TEXT)
          status = nf_put_att_text(ncid, vwind_id,'coordinates', 
     &       LEN,TRIM(TEXT))
        ENDIF
        TEXT='Vwind, scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, vwind_id,'field', 
     &       LEN,TRIM(TEXT))
!        status = nf_put_att_real(ncid, vwind_id, 
!     &  '_FillValue', NF_REAL, 1,-99999.0)
      endif
! define pair
      if(Pair_L)THEN 
        status = nf_def_var(ncid, 'Pair', NF_REAL, 3,intval,pair_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='Operational products'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, pair_id,'production_status', 
     &       LEN,TRIM(TEXT))
        TEXT='surface air pressure'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, pair_id,'long_name', 
     &       LEN,TRIM(TEXT))
        TEXT='milibar'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, pair_id,'units', 
     &       LEN,TRIM(TEXT))
        TEXT='frc_time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, pair_id,'time', 
     &       LEN,TRIM(TEXT))
        IF(IGRD .LE. 0)THEN
          TEXT='lon_native lat_native'
          LEN=LEN_TRIM(TEXT)
          status = nf_put_att_text(ncid, pair_id,'coordinates', 
     &       LEN,TRIM(TEXT))
        ENDIF
        TEXT='Pair, scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, pair_id,'field', 
     &       LEN,TRIM(TEXT))
!        status = nf_put_att_real(ncid, pair_id, 
!     &  '_FillValue', NF_REAL, 1,-99999.0)
      endif
      
! define Tair
      if(Tair_L)THEN
        status = nf_def_var(ncid, 'Tair', NF_REAL, 3,intval,tair_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='Operational products'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, tair_id,'production_status', 
     &       LEN,TRIM(TEXT))
        TEXT='surface air temperature'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, tair_id,'long_name', 
     &       LEN,TRIM(TEXT))
        TEXT='Celsius'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, tair_id,'units', 
     &       LEN,TRIM(TEXT))
        TEXT='frc_time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, tair_id,'time', 
     &       LEN,TRIM(TEXT))
        IF(IGRD .LE. 0)THEN
          TEXT='lon_native lat_native'
          LEN=LEN_TRIM(TEXT)
          status = nf_put_att_text(ncid, tair_id,'coordinates', 
     &       LEN,TRIM(TEXT))
        ENDIF
        TEXT='Tair, scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, tair_id,'field', 
     &       LEN,TRIM(TEXT))
!        status = nf_put_att_real(ncid, tair_id, 
!     &  '_FillValue', NF_REAL, 1,-99999.0)
      endif
! define Qair
      if(qair_L)THEN
        status = nf_def_var(ncid, 'Qair', NF_REAL, 3,intval,Qair_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='Operational products'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, Qair_id,'production_status', 
     &       LEN,TRIM(TEXT))
        TEXT='surface air relative humidity'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, Qair_id,'long_name', 
     &       LEN,TRIM(TEXT))
        TEXT='percentage'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, Qair_id,'units', 
     &       LEN,TRIM(TEXT))
        TEXT='frc_time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, Qair_id,'time', 
     &       LEN,TRIM(TEXT))
        IF(IGRD .LE. 0)THEN
          TEXT='lon_native lat_native'
          LEN=LEN_TRIM(TEXT)
          status = nf_put_att_text(ncid, Qair_id,'coordinates', 
     &       LEN,TRIM(TEXT))
        ENDIF
        TEXT='Qair, scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, Qair_id,'field', 
     &       LEN,TRIM(TEXT))
!        status = nf_put_att_real(ncid, Qair_id, 
!     &  '_FillValue', NF_REAL, 1,-99999.0)
      endif
! define swrad
      if(swrad_L)THEN
        status = nf_def_var(ncid, 'swrad', NF_REAL, 3,intval,swrad_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='Operational products'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, swrad_id,'production_status', 
     &       LEN,TRIM(TEXT))
        TEXT='Net solar shortwave radiation flux'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, swrad_id,'long_name', 
     &       LEN,TRIM(TEXT))
        TEXT='Watts meter-2'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, swrad_id,'units', 
     &       LEN,TRIM(TEXT))
        TEXT='frc_time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, swrad_id,'time', 
     &       LEN,TRIM(TEXT))
        IF(IGRD .LE. 0)THEN
          TEXT='lon_native lat_native'
          LEN=LEN_TRIM(TEXT)
          status = nf_put_att_text(ncid, swrad_id,'coordinates', 
     &       LEN,TRIM(TEXT))
        ENDIF
        TEXT='swrad, scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, swrad_id,'field', 
     &       LEN,TRIM(TEXT))
        TEXT='downward flux, heating'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, swrad_id,'positive', 
     &       LEN,TRIM(TEXT))
        TEXT='upward flux, cooling'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, swrad_id,'negative', 
     &       LEN,TRIM(TEXT))
!        status = nf_put_att_real(ncid, swrad_id, 
!     &  '_FillValue', NF_REAL, 1,-99999.0)
      endif
! define lwrad
      if(lwrad_L)THEN
        status = nf_def_var(ncid, 'lwrad_down', NF_REAL, 3,intval,
     &	lwrad_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='Operational products'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lwrad_id,'production_status', 
     &       LEN,TRIM(TEXT))
        TEXT='downward solar longwave radiation flux'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lwrad_id,'long_name', 
     &       LEN,TRIM(TEXT))
        TEXT='Watts meter-2'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lwrad_id,'units', 
     &       LEN,TRIM(TEXT))
        TEXT='frc_time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lwrad_id,'time', 
     &       LEN,TRIM(TEXT))
        IF(IGRD .LE. 0)THEN
          TEXT='lon_native lat_native'
          LEN=LEN_TRIM(TEXT)
          status = nf_put_att_text(ncid, lwrad_id,'coordinates', 
     &       LEN,TRIM(TEXT))
        ENDIF
        TEXT='lwrad, scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lwrad_id,'field', 
     &       LEN,TRIM(TEXT))

        TEXT='downward flux, heating'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lwrad_id,'positive', 
     &       LEN,TRIM(TEXT))
        TEXT='upward flux, cooling'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, lwrad_id,'negative', 
     &       LEN,TRIM(TEXT))
!        status = nf_put_att_real(ncid, lwrad_id, 
!     &  '_FillValue', NF_REAL, 1,-99999.0)
      endif
! define rain
      if(rain_L)THEN
        status = nf_def_var(ncid, 'rain', NF_REAL, 3,intval,rain_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='Operational products'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, rain_id,'production_status', 
     &       LEN,TRIM(TEXT))
        TEXT='Precipation rate'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, rain_id,'long_name', 
     &       LEN,TRIM(TEXT))
        TEXT='kilogram meter-2 second-1'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, rain_id,'units', 
     &       LEN,TRIM(TEXT))
        TEXT='frc_time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, rain_id,'time', 
     &       LEN,TRIM(TEXT))
        IF(IGRD .LE. 0)THEN
          TEXT='lon_native lat_native'
          LEN=LEN_TRIM(TEXT)
          status = nf_put_att_text(ncid, rain_id,'coordinates', 
     &       LEN,TRIM(TEXT))
        ENDIF
        TEXT='rain, scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, rain_id,'field', 
     &       LEN,TRIM(TEXT))
!        status = nf_put_att_real(ncid, rain_id, 
!     &  '_FillValue', NF_REAL, 1,-99999.0)
      endif

! define tcdc
      if(tcdc_L)THEN
        status = nf_def_var(ncid, 'tcdc', NF_REAL, 3,intval,tcdc_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='Operational products'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, tcdc_id,'production_status',
     &       LEN,TRIM(TEXT))
        TEXT='total cloud cover'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, tcdc_id,'long_name',
     &       LEN,TRIM(TEXT))
        TEXT='percentage'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, tcdc_id,'units',
     &       LEN,TRIM(TEXT))
        TEXT='frc_time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, tcdc_id,'time',
     &       LEN,TRIM(TEXT))
        IF(IGRD .LE. 0)THEN
          TEXT='lon_native lat_native'
          LEN=LEN_TRIM(TEXT)
          status = nf_put_att_text(ncid, tcdc_id,'coordinates',
     &       LEN,TRIM(TEXT))
        ENDIF
        TEXT='tcdc, scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, tcdc_id,'field',
     &       LEN,TRIM(TEXT))
      endif
! define evp
      if(evp_L)THEN
        status = nf_def_var(ncid, 'evp', NF_REAL, 3,intval,evp_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='Operational products'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, evp_id,'production_status',
     &       LEN,TRIM(TEXT))
        TEXT='Evaporation'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, evp_id,'long_name',
     &       LEN,TRIM(TEXT))
        TEXT='kilogram meter-2'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, evp_id,'units',
     &       LEN,TRIM(TEXT))
        TEXT='frc_time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, evp_id,'time',
     &       LEN,TRIM(TEXT))
        IF(IGRD .LE. 0)THEN
          TEXT='lon_native lat_native'
          LEN=LEN_TRIM(TEXT)
          status = nf_put_att_text(ncid, evp_id,'coordinates',
     &       LEN,TRIM(TEXT))
        ENDIF
        TEXT='evp, scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, evp_id,'field',
     &       LEN,TRIM(TEXT))
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
!      TEXT='Created by Aijun Zhang, OD/CO-OPS/NOS/NOAA'
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
        itime=itime+1
c scalars
        CORNER(1) = itime
        iret=nf_put_var1_real(ncid,frc_time_id,CORNER,frc_time)

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
	if(pair_L)THEN
          status=nf_put_vara_real(ncid,pair_id,CORNER,COUNT,pair)
          if (status .ne. NF_NOERR)stop 'write pair'
        endif
	if(qair_L)THEN
          status=nf_put_vara_real(ncid,qair_id,CORNER,COUNT,qair)
          if (status .ne. NF_NOERR)stop 'write qair'
	endif
	if(tair_L)then  
          status=nf_put_vara_real(ncid,tair_id,CORNER,COUNT,tair)
          if (status .ne. NF_NOERR)stop 'write tair'
	endif
	if(swrad_L)then  
          status=nf_put_vara_real(ncid,swrad_id,CORNER,COUNT,swrad)
          if (status .ne. NF_NOERR)stop 'write swrad'
	endif
	if(lwrad_L)then  
          status=nf_put_vara_real(ncid,lwrad_id,CORNER,COUNT,lwrad)
          if (status .ne. NF_NOERR)stop 'write lwrad'
        endif  
	if(rain_L)then  
          status=nf_put_vara_real(ncid,rain_id,CORNER,COUNT,rain)
          if (status .ne. NF_NOERR)stop 'write rain'
        endif  
	if(tcdc_L)then  
          status=nf_put_vara_real(ncid,tcdc_id,CORNER,COUNT,tcdc)
          if (status .ne. NF_NOERR)stop 'write tcdc'
        endif  
	if(evp_L)then  
          status=nf_put_vara_real(ncid,evp_id,CORNER,COUNT,evp)
          if (status .ne. NF_NOERR)stop 'write evp'
        endif  
	
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
