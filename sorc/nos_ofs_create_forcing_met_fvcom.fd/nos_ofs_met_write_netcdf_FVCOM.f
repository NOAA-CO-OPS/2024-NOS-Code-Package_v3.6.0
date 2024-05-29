!       Subroutine Name:  nos_ofs_met_write_netcdf_FVCOM.f
!
!       Technical Contact(s):   Name:  Aijun Zhang
!                               Org:   NOS/CO-OPS/OD
!                               Phone: 240-533-0591
!                               E-Mail: aijun.zhang@noaa.gov
!       Edited by Mojgan Rostaminia, add specific humidity for ice
!       module  10/17/2019
!       Abstract:  this subroutine is used to write surface meteological
!       forcing into NetCDF format for those FVCOM-based OFS
!
!       History Log:
!           03/28/2019
!
!       Usage: call nos_ofs_met_write_netcdf_FVCOM from
!       nos_ofs_create_forcing_met_fvcom.f
!
!       Argument Input:
!                netcdf_file - the output NetCDF file
!                ncid       - NetCDF file identity integer
!                imode      - 1, 2, or 3 depending on netcdf_file status
!                IGRD       - indicator of horizontal interpolation
!                method
!                            =0: no interpolation
!                            =1:  remesh using triangulation techniques
!                            =2: bicubic routine from ROMS
!                            =3: bilinear routine from ROMS
!                            =4: nature neighbours
!
!               node_len        - number of node
!               nele_len        - number of element
!               base_date       - Base date for the NetCDF file                                 
!               nv              - nodes surrounding elements
!               lon             - Nodal latitude
!               lat             - Nodal longitude
!               lonc            - zonal longitude
!               latc            - zonal latitude
!               fcst_time       - surface forcing time (real)
!               Times           - surface forcing time (characters) 
!               U10             - Eastward Wind Speed at 10m height
!               V10             - Northward Wind Speed at 10m height
!               net_heat_flux   - Surface Net Heat Flux
!               air_temperature - Air Temperature
!               air_pressure    - Air Pressure
!               short_wave      - Net Short Wave Radiation
!               long_wave       - Downward Long Wave Radiation
!               relative_humidity -Surface air relative humidity
!               dew_point       - Dew Point
!               cloud_cover     - Cloud Cover
!               evap            - evaporation
!               precip          - precipitation
!               specific_humidity -Surface air specifichumidity(needed for ice) 
!               globalstr       - global attributes       
!       
!       Argument Output:  
!               netcdf_file     -  the output NetCDF file with values in
!                               targete variables

!

      subroutine nos_ofs_met_write_netcdf_FVCOM(netcdf_file,
     &  ncid,imode,IGRD,node_len,nele_len,base_date,nv,lon,
     &  lat,lonc,latc,fcst_time,Times,U10,V10,net_heat_flux,
     &  air_temperature,air_pressure,short_wave,long_wave,
     &  relative_humidity,dew_point,cloud_cover,evap,
     &  precip,SPQ,globalstr) ! 

      include 'netcdf.inc'
      CHARACTER*80 TEXT,CNAME,netcdf_file
      INTEGER LEN,base_date(4),intval(4),CORNER(4),COUNT(4)
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      CHARACTER*26 Times(1)
      INTEGER DATE_TIME(8)
      REAL START_TIME,END_TIME      
      character globalstr(9)*120
* error status return
      integer  iret,status,jtime

* netCDF id
      integer  ncid,IGRD

* dimension ids
      integer  node_dim,nele_dim,time_dim 
      integer  three_dim
      integer  DateStrLen_dim
c dimension lengths
      integer  node_len,nele_len,time_len 

c variable ids
      integer  Itime_id,Itime2_id,fcst_time_id
      integer  three
      integer  Times_id
      integer  lat_id,lon_id,latc_id,lonc_id
      integer  nv_id,U10_id,V10_id 
      integer  net_heat_flux_id, short_wave_id, long_wave_id 
      integer  air_pressure_id, air_temperature_id
      integer  relative_humidity_id, dew_point_id, cloud_cover_id
      integer  evap_id, precip_id, SPQ_id
 
c data variables
      integer Itime,Itime2
      real fcst_time
      integer  nv(nele_len,3)
      real lat(node_len),lon(node_len)
      real latc(nele_len),lonc(nele_len)
      real U10(nele_len), V10(nele_len) 
      real net_heat_flux(node_len)
      real short_wave(node_len),long_wave(node_len)
      real air_pressure(node_len), air_temperature(node_len)
      real relative_humidity(node_len),dew_point(node_len)
      real cloud_cover(node_len)
      real evap(node_len), precip(node_len),SPQ(node_len)
 
      logical U10_L,V10_L,net_heat_flux_L
      logical short_wave_L,long_wave_L,air_pressure_L,air_temperature_L
      logical relative_humidity_L, dew_point_L, cloud_cover_L
      logical evap_L, precip_L, SPQ_L      


C save all dimention id and variable id for next call
      save  node_dim,nele_dim,time_dim 
      save  Itime_id,Itime2_id,fcst_time_id,Times_id
      save  lat_id,lon_id,latc_id,lonc_id
      save  nv_id,U10_id,V10_id 
      save  net_heat_flux_id, short_wave_id, long_wave_id
      save  air_pressure_id,air_temperature_id
      save  relative_humidity_id,dew_point_id,cloud_cover_id
      save  U10_L,V10_L,net_heat_flux_L
      save  short_wave_L,long_wave_L,air_pressure_L,air_temperature_L
      save  specific_humidity_L,relative_humidity_L 
      save  dew_point_L, cloud_cover_L
      save  evap_id, precip_id,SPQ_id
      save  evap_L, precip_L,SPQ_L
!       write(*,*) 'in write netCDF.... ncid=',ncid
      if (imode.eq.1) then           ! Write the file message 
      write(*,*) 'Start netCDF write, imode, ncid = ', imode, ncid 
C Set optional variable flags
      U10_L = .TRUE.
      V10_L = .TRUE.
      net_heat_flux_L = .TRUE.
      short_wave_L = .TRUE.
      long_wave_L = .TRUE.
      air_pressure_L = .TRUE.
      air_temperature_L = .TRUE.
      relative_humidity_L = .TRUE.
      dew_point_L = .TRUE.
      cloud_cover_L = .TRUE.
      evap_L = .TRUE.
      precip_L = .TRUE.
      SPQ_L= .TRUE.
      
      if(U10(1) .lt. 0.5)U10_L = .FALSE.
      if(V10(1) .lt. 0.5)V10_L = .FALSE.
      if(net_heat_flux(1) .lt. 0.5)net_heat_flux_L = .FALSE.
      if(short_wave(1).lt. 0.5)short_wave_L = .FALSE.
      if(long_wave(1).lt. 0.5)long_wave_L = .FALSE.  
      if(air_pressure(1) .lt. 0.5)air_pressure_L = .FALSE.
      if(air_temperature(1) .lt. 0.5)air_temperature_L = .FALSE.
      if(relative_humidity(1) .lt. 0.5)relative_humidity_L = .FALSE.
      if(dew_point(1).lt. 0.5)dew_point_L = .FALSE.
      if(cloud_cover(1).lt. 0.5)cloud_cover_L = .FALSE. 
      if(SPQ(1) .lt. 0.5)SPQ_L = .FALSE.
!      if(evap(1) .lt. 0.5)evap_L = .FALSE.
!      if(precip(1) .lt. 0.5)precip_L = .FALSE.
      
      status=nf_create(trim(netcdf_file), NF_CLOBBER, ncid)
      if (status .ne. NF_NOERR) then
       write(*,*)'error in nf_create: ',trim(netcdf_file)
       stop 'netcdf'
      endif
c define dimensions
      iret = nf_def_dim(ncid,'node', node_len, node_dim)
      iret = nf_def_dim(ncid,'nele',nele_len,nele_dim)
      iret = nf_def_dim(ncid,'three',3,three_dim)
      iret = nf_def_dim(ncid,'time',NF_UNLIMITED,time_dim)
      iret = nf_def_dim(ncid, 'DateStrLen', 26, DateStrLen_dim)
      call check_err(iret)
c
      write(6,*) 'Start to define variables'

c define coordinate variables
      iret=nf_def_var(ncid,'Itime', NF_INT,1,time_dim,Itime_id)
      iret=nf_def_var(ncid,'Itime2', NF_INT,1,time_dim,Itime2_id)
      iret=nf_def_var(ncid,'time',NF_REAL,1,time_dim,
     1 fcst_time_id)
      intval(2) = time_dim
      intval(1) = DateStrLen_dim
      iret = nf_def_var(ncid, 'Times', NF_CHAR, 2,intval, 
     1Times_id)
      call check_err(iret)

c assign attributes
      WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3),' ',base_date(4),':00:00' 
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,itime_id,'units',LEN,TRIM(TEXT)) 
           call check_err(iret)
      TEXT='modified julian day(MJD)'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, itime_id,'format',LEN,TRIM(TEXT))
          call check_err(iret)
      iret = nf_put_att_text(ncid,itime_id, 'time_zone', 3, 'UTC')
          call check_err(iret)

      TEXT='msec since 00:00:00'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,Itime2_id,'units',LEN,TRIM(TEXT)) 
         call check_err(iret)
      iret=nf_put_att_text(ncid,Itime2_id, 'time_zone',3,'UTC')
         call check_err(iret)
      iret=nf_put_att_text(ncid,itime2_id, 'long_name',4,'time')
         call check_err(iret)      

      WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3),' ',base_date(4),':00:00'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,fcst_time_id,'units',LEN,TRIM(TEXT))
           call check_err(iret)
      TEXT='modified julian day(MJD)'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid, fcst_time_id,'format',
     & LEN,TRIM(TEXT))
          call check_err(iret)
      iret=nf_put_att_text(ncid,fcst_time_id,'time_zone',3,'UTC')
          call check_err(iret)
          
      TEXT='Calendar Date'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Times_id, 'long_name',
     &       LEN,TRIM(TEXT))
      TEXT='String: Calendar Time'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Times_id, 'format',
     &       LEN,TRIM(TEXT))
      TEXT='UTC'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Times_id, 'time_zone',
     &       LEN,TRIM(TEXT))
      
      intval(1) = node_dim
      iret = nf_def_var(ncid,'lat',NF_REAL,1,intval,lat_id)
             call check_err(iret)
      intval(1) = node_dim
      iret = nf_def_var(ncid,'lon',NF_REAL,1,intval,lon_id)
             call check_err(iret)
      TEXT='Nodal latitude'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,lat_id, 'long_name',len,TRIM(TEXT))
      TEXT='latitude'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,lat_id,'standard_name',len,TRIM(TEXT))
      TEXT='degrees_north'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,lat_id, 'units',len,TRIM(TEXT))
          call check_err(iret)

      TEXT='Nodal longitude'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,lon_id, 'long_name',len,TRIM(TEXT))
      TEXT='longitude'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,lon_id,'standard_name',len,
     & TRIM(TEXT))
      TEXT='degrees_east'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,lon_id, 'units',len,TRIM(TEXT))
            call check_err(iret)      
            
      intval(1) =nele_dim 
      intval(2) =3 
      iret = nf_def_var(ncid,'nv',NF_INT,2,intval,nv_id)
             call check_err(iret)
      TEXT='nodes surrounding elements'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,nv_id, 'long_name',len,TRIM(TEXT))
          call check_err(iret)
          
      intval(1) = nele_dim
      iret = nf_def_var(ncid,'latc',NF_REAL,1,intval,latc_id)
             call check_err(iret)
      intval(1) = nele_dim
      iret = nf_def_var(ncid,'lonc',NF_REAL,1,intval,lonc_id)
             call check_err(iret)
      TEXT='zonal latitude'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,latc_id, 'long_name',len,TRIM(TEXT))
      TEXT='latitude'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,latc_id, 'standard_name',len,
     & TRIM(TEXT))
      TEXT='degrees_north'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,latc_id, 'units',len,TRIM(TEXT))
          call check_err(iret)
 
      TEXT='zonal longitude'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,lonc_id, 'long_name',len,TRIM(TEXT))
      TEXT='longitude'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,lonc_id, 'standard_name',len,
     & TRIM(TEXT))
      TEXT='degrees_east'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,lonc_id, 'units',len,TRIM(TEXT))
            call check_err(iret) 

      IF(U10_L) THEN      
      intval(1) = nele_dim
      intval(2) = time_dim
      iret = nf_def_var(ncid,'U10',NF_REAL,2,intval,U10_id)
             call check_err(iret)
      TEXT='Eastward Wind Speed'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,U10_id, 'long_name',len,TRIM(TEXT))
         call check_err(iret)
      iret=nf_put_att_text(ncid,U10_id,'units',3,'m/s')
           call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, U10_id, 'grid', LEN, TRIM(TEXT))
          call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)          
      iret = nf_put_att_text(ncid, U10_id, 'coordinates', 
     1 LEN, TRIM(TEXT)) 
          call check_err(iret)
      iret = nf_put_att_text(ncid, U10_id, 'type', 4, 'data')             
      ENDIF

      IF(V10_L) THEN        
      intval(1) = nele_dim
      intval(2) = time_dim
      iret = nf_def_var(ncid,'V10',NF_REAL,2,intval,V10_id)
             call check_err(iret)
      TEXT='Northward Wind Speed'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,V10_id, 'long_name',len,TRIM(TEXT))
         call check_err(iret)
      iret=nf_put_att_text(ncid,V10_id,'units',3,'m/s')
           call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, V10_id, 'grid', LEN, TRIM(TEXT))
          call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)          
      iret = nf_put_att_text(ncid, V10_id, 'coordinates',
     1  LEN, TRIM(TEXT))  
          call check_err(iret)
      iret = nf_put_att_text(ncid, V10_id, 'type', 4, 'data')             
      ENDIF
      
      intval(1) = node_dim
      intval(2) = time_dim
      
      IF(net_heat_flux_L) THEN      
      iret = nf_def_var(ncid,'net_heat_flux',NF_REAL,2,
     c   intval,net_heat_flux_id) 
             call check_err(iret)
      TEXT='Surface Net Heat Flux'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,net_heat_flux_id, 'long_name',
     &             len,TRIM(TEXT))
         call check_err(iret)
      iret=nf_put_att_text(ncid,net_heat_flux_id,'units',5,'W m-2')
           call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,net_heat_flux_id,'grid',LEN,
     & TRIM(TEXT))
          call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)          
      iret=nf_put_att_text(ncid,net_heat_flux_id,'coordinates',
     & LEN, TRIM(TEXT))
          call check_err(iret)
      iret = nf_put_att_text(ncid, net_heat_flux_id, 'type', 4, 
     & 'data')             
      ENDIF

      IF(short_wave_L) THEN             
      iret = nf_def_var(ncid,'short_wave',NF_REAL,2,
     c   intval,short_wave_id)
             call check_err(iret)
      TEXT='Net Short Wave Radiation '
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,short_wave_id,'long_name',
     &                   len,TRIM(TEXT))
         call check_err(iret)
      iret=nf_put_att_text(ncid,short_wave_id,'units',5,'W m-2')
           call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,short_wave_id,'grid',LEN,TRIM(TEXT))
          call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,short_wave_id,'coordinates',
     1    LEN, TRIM(TEXT))      
          call check_err(iret)
      iret=nf_put_att_text(ncid,short_wave_id,'type',4,'data')             
      ENDIF             

      IF(long_wave_L) THEN             
      iret = nf_def_var(ncid,'long_wave',NF_REAL,2,
     c   intval,long_wave_id) 
             call check_err(iret)
      TEXT='Downward Long Wave Radiation'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,long_wave_id,'long_name',
     &             len,TRIM(TEXT))
         call check_err(iret)     
      iret=nf_put_att_text(ncid,long_wave_id,'units',5,'W m-2')
         call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,long_wave_id,'grid',LEN,
     1 TRIM(TEXT))
         call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,long_wave_id,
     1 'coordinates',LEN, TRIM(TEXT))
         call check_err(iret)
      iret = nf_put_att_text(ncid,long_wave_id,'type',4,'data')
         call check_err(iret)             
      ENDIF             

      IF(air_pressure_L) THEN             
      iret = nf_def_var(ncid,'air_pressure',NF_REAL,2,
     c   intval,air_pressure_id)
             call check_err(iret)
      TEXT='Air Pressure'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,air_pressure_id, 'long_name',
     &           len,TRIM(TEXT))
         call check_err(iret)
      iret=nf_put_att_text(ncid,air_pressure_id,'units',2,'Pa')
           call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,air_pressure_id,'grid',LEN,
     & TRIM(TEXT))
          call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)          
      iret=nf_put_att_text(ncid,air_pressure_id,
     1 'coordinates',LEN, TRIM(TEXT))
          call check_err(iret)
      iret=nf_put_att_text(ncid,air_pressure_id,'type',4,'data')
          call check_err(iret)             
      ENDIF             

      IF(air_temperature_L) THEN             
       iret = nf_def_var(ncid,'air_temperature',NF_REAL,2,intval,
     1  air_temperature_id)
             call check_err(iret)
      TEXT='Air Temperature'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,air_temperature_id, 'long_name',
     &           len,TRIM(TEXT))
         call check_err(iret)
      TEXT='Celsius Degree'
      LEN=LEN_TRIM(TEXT)         
      iret=nf_put_att_text(ncid,air_temperature_id,'units',LEN,
     1 TRIM(TEXT))
           call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,air_temperature_id,'grid',LEN,
     & TRIM(TEXT))
          call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)          
      iret=nf_put_att_text(ncid,air_temperature_id,
     1 'coordinates',LEN,TRIM(TEXT) )      
          call check_err(iret)
      iret=nf_put_att_text(ncid,air_temperature_id,'type',4,'data')
          call check_err(iret)             
      ENDIF             

      IF(relative_humidity_L) THEN             
      iret=nf_def_var(ncid,'relative_humidity',NF_REAL,2,intval,
     1 relative_humidity_id)
             call check_err(iret)
      TEXT='surface air relative humidity'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,relative_humidity_id,'long_name',
     1 len,TRIM(TEXT))
         call check_err(iret)
      TEXT='percentage'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,relative_humidity_id,'units',LEN,
     1 TRIM(TEXT))
           call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid,relative_humidity_id, 'grid',
     1  LEN, TRIM(TEXT))
          call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid,relative_humidity_id,  
     1 'coordinates',LEN, TRIM(TEXT))
          call check_err(iret)
      iret = nf_put_att_text(ncid,relative_humidity_id,'type',4,
     1  'data')             
      ENDIF             
      
      IF(dew_point_L) THEN             
      iret = nf_def_var(ncid,'dew_point',NF_REAL,2,intval,
     1  dew_point_id)
             call check_err(iret)
      TEXT='Dew Point'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,dew_point_id,'long_name',
     1 len,TRIM(TEXT))
         call check_err(iret)
      TEXT='Celsius Degree'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,dew_point_id,'units',LEN,
     1 TRIM(TEXT))
         call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid,dew_point_id, 'grid',
     1  LEN, TRIM(TEXT))
        call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid,dew_point_id,  
     1 'coordinates',LEN, TRIM(TEXT))
        call check_err(iret)
      iret = nf_put_att_text(ncid,dew_point_id,'type',4,
     1  'data')               
      ENDIF             

      IF(cloud_cover_L) THEN             
      iret = nf_def_var(ncid,'cloud_cover',NF_REAL,2,intval,
     1  cloud_cover_id)
             call check_err(iret)   
      TEXT='Total Cloud Cover'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,cloud_cover_id,'long_name',
     1 len,TRIM(TEXT))
         call check_err(iret)
      TEXT='Fraction'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,cloud_cover_id,'units',LEN,
     1 TRIM(TEXT))
         call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid,cloud_cover_id, 'grid',
     1  LEN, TRIM(TEXT))
        call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid,cloud_cover_id,  
     1 'coordinates',LEN, TRIM(TEXT))
        call check_err(iret)
      iret = nf_put_att_text(ncid,cloud_cover_id,'type',4,
     1  'data')               
      ENDIF


      IF(evap_L) THEN
      iret = nf_def_var(ncid,'evap',NF_REAL,2,
     c   intval,evap_id)
             call check_err(iret)
      TEXT='Evaporation'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,evap_id, 'long_name',
     &           len,TRIM(TEXT))
         call check_err(iret)
      iret=nf_put_att_text(ncid,evap_id,'units',5,'m s-1')
           call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,evap_id,'grid',LEN,
     & TRIM(TEXT))
          call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,evap_id,
     1 'coordinates',LEN, TRIM(TEXT))
          call check_err(iret)
      iret=nf_put_att_text(ncid,evap_id,'type',4,'data')
          call check_err(iret)
      ENDIF


      IF(precip_L) THEN
      iret = nf_def_var(ncid,'precip',NF_REAL,2,
     c   intval,precip_id)
             call check_err(iret)
      TEXT='Precipitation'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,precip_id, 'long_name',
     &           len,TRIM(TEXT))
         call check_err(iret)
      iret=nf_put_att_text(ncid,precip_id,'units',5,'m s-1')
           call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,precip_id,'grid',LEN,
     & TRIM(TEXT))
          call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,precip_id,
     1 'coordinates',LEN, TRIM(TEXT))
          call check_err(iret)
      iret=nf_put_att_text(ncid,precip_id,'type',4,'data')
          call check_err(iret)
      ENDIF
      
       IF(SPQ_L) THEN
      iret=nf_def_var(ncid,'SPQ',NF_REAL,2,intval,
     1 SPQ_id)
             call check_err(iret)
      TEXT='Specific Humidity'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,SPQ_id,'long_name',
     1 len,TRIM(TEXT))
         call check_err(iret)
      TEXT='kg/kg'
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,SPQ_id,'units',LEN,
     1 TRIM(TEXT))
           call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid,SPQ_id, 'grid',
     1  LEN, TRIM(TEXT))
          call check_err(iret)
      TEXT='FVCOM Spheric coordinates'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid,SPQ_id,
     1 'coordinates',LEN, TRIM(TEXT))
          call check_err(iret)
      iret = nf_put_att_text(ncid,SPQ_id,'type',4,
     1  'data')
      ENDIF

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
      TEXT=trim(globalstr(8))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'reference',
     &       LEN,TRIM(TEXT))
      TEXT='GeoReferenced'
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'CoordinateSystem',
     &       LEN,TRIM(TEXT))
      TEXT='none'
      LEN=LEN_TRIM(TEXT)
      status=nf_put_att_text(ncid,NF_GLOBAL,'CoordinateProjection',
     &       LEN,TRIM(TEXT))
      
C leave define mode
      iret = nf_enddef(ncid)
      call check_err(iret)
      CORNER(1) = 1
      COUNT(1)=node_len
      status=nf_put_vara_real(ncid,lat_id,CORNER,COUNT,lat)
      status=nf_put_vara_real(ncid,lon_id,CORNER,COUNT,lon)

      CORNER(1) = 1
      CORNER(2) = 1
      COUNT(1)=nele_len
      COUNT(2)=3
      status=nf_put_vara_INT(ncid,nv_id,CORNER,COUNT,nv)

      CORNER(1) = 1
      COUNT(1)=nele_len
      status=nf_put_vara_real(ncid,latc_id,CORNER,COUNT,latc)
      status=nf_put_vara_real(ncid,lonc_id,CORNER,COUNT,lonc)
c
       write(*,*)'end of NetCDF file definition, imode= ',imode,ncid 

      elseif (imode.eq.2) then         ! Write the data into the file 

      write(*,*) 'Start netCDF write, imode ncid= ',imode,ncid

      status= nf_inq_dimlen(ncid,time_dim,jtime)
      jtime=jtime+1
      write(*,*)'jtime= ',jtime 
      CORNER(1) = jtime 
      COUNT(1)=1
       Itime=int(fcst_time)
       status=nf_put_vara_int(ncid,Itime_id,CORNER,COUNT,Itime)
      if (status .ne. NF_NOERR)then
         write(*,*)'error in writing Itime into NetCDF file'
         stop 'write Itime'
      endif
      CORNER(1) = jtime 
      COUNT(1)=1 
      Itime2=int((fcst_time-Itime)*24*3600*1000)
      status=nf_put_vara_int(ncid,Itime2_id,CORNER,COUNT,Itime2)
      if (status .ne. NF_NOERR)then
         write(*,*)'error in writing Itime2 into NetCDF file'
         stop 'write Itime2'
      endif

      CORNER(1) = jtime 
      COUNT(1)=1
      status=nf_put_vara_real(ncid,fcst_time_id,CORNER,COUNT,
     1 fcst_time)
      if (status .ne. NF_NOERR)then
         write(*,*)'error in writing fcst_time into NetCDF file'
         stop 'write fcst_time'
      endif	 
      write(*,*)'time in =',fcst_time,Itime,Itime2
      CORNER(1) = 1
      CORNER(2) = jtime
      COUNT(1)=26
      COUNT(2)=1
      iret=nf_put_vara_text(ncid,Times_id,CORNER,COUNT,
     & Times)
      call check_err(iret)


      CORNER(1)=1
      CORNER(2) = jtime 
      COUNT(1)=nele_len
      COUNT(2)= 1 
      IF(U10_L) THEN      
      status=nf_put_vara_real(ncid,U10_id,CORNER,COUNT,U10) 
      call check_err(iret)
      ENDIF
      IF(V10_L) THEN       
      status=nf_put_vara_real(ncid,V10_id,CORNER,COUNT,V10)
      call check_err(iret)
      ENDIF

      CORNER(1)=1
      CORNER(2)=jtime 
      COUNT(1)=node_len
      COUNT(2)=1
      IF(net_heat_flux_L) THEN      
      status=nf_put_vara_real(ncid,net_heat_flux_id,CORNER,
     &             COUNT,net_heat_flux)
      call check_err(iret)
      ENDIF
      IF(short_wave_L) THEN      
      status=nf_put_vara_real(ncid,short_wave_id,CORNER,
     &             COUNT,short_wave)
      call check_err(iret)
      ENDIF
      IF(long_wave_L) THEN
      status=nf_put_vara_real(ncid,long_wave_id,CORNER,
     &             COUNT,long_wave)
      call check_err(iret)
      ENDIF
      IF(air_pressure_L)THEN      
      status=nf_put_vara_real(ncid,air_pressure_id,CORNER,
     &             COUNT,air_pressure)
      call check_err(iret)
      ENDIF
      IF(air_temperature_L)THEN      
      status=nf_put_vara_real(ncid,air_temperature_id,CORNER,
     &             COUNT,air_temperature)
      call check_err(iret)
      ENDIF 
      IF(dew_point_L)THEN      
      status=nf_put_vara_real(ncid,dew_point_id,CORNER,
     &             COUNT,dew_point)
      call check_err(iret)
      ENDIF 
      IF(cloud_cover_L)THEN      
      status=nf_put_vara_real(ncid,cloud_cover_id,CORNER,
     &             COUNT,cloud_cover)
      call check_err(iret)
      ENDIF       
!      write(*,*) 'in netCDF write.... end of imode= ', imode 

      IF(evap_L)THEN
      status=nf_put_vara_real(ncid,evap_id,CORNER,
     &             COUNT,evap)
      call check_err(iret)
      ENDIF

      IF(precip_L)THEN
      status=nf_put_vara_real(ncid,precip_id,CORNER,
     &             COUNT,precip)
      call check_err(iret)
      ENDIF
       IF(relative_humidity_L)THEN
      status=nf_put_vara_real(ncid,relative_humidity_id,CORNER,
     &             COUNT,relative_humidity)
      call check_err(iret)
      ENDIF
      IF(SPQ_L)THEN
      status=nf_put_vara_real(ncid,SPQ_id,CORNER,
     &             COUNT,SPQ)
      call check_err(iret)
      ENDIF
      write(*,*) 'in netCDF write.... end of imode= ', imode
      elseif (imode.eq.3) then             ! Close the netCDF
        iret = nf_close(ncid)
        write(*,*) ' The netCDF file is closed'
      endif


      return 
      end       
       
!      subroutine check_err(iret)
!      integer iret
!      include 'netcdf.inc'
!      if (iret .ne. NF_NOERR) then
!        print *, nf_strerror(iret)
!      stop
!      endif
!      end
