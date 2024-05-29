!--------------------------------------------------------------------------
!
!  Program Name:  nos_ofs_create_forcing_obc_fvcom_gl.fd/nos_ofs_obc_write_netcdf_fvcom_gl.f
!
!  Contact: NOS/CO-OPS Aijun Zhang
!           Phone: 240-533-0591   Email: aijun.zhang@noaa.gov
!
!  Abstract:  This is a SUBROUTINE to create a NetCDF file (imode=1),
!           store the data to NetCDF file (imode=2), and close the
!           NetCDF file (imode=3) for FVCOM-base Great Lakes OFS OBC 
!           forcing (such as LEOFS and LMHOFS).  This subroutine is called 
!           by program nos_ofs_create_forcing_obc_fvcom_gl.f.
!
!       Error Codes:
!           0  No Error
!         -33  Not a NetCDF ID
!         -34  Too many NetCDFs open
!         -35  NetCDF file exists && NC_NOCLOBBER
!         -36  Invalid Argument
!         -37  Write to read only
!         -38  Operation not allowed in data mode
!         -39  Operation not allowed in define mode
!         -40  Index exceeds dimension bound
!         -41  NC_MAX_DIMS exceeded
!         -42  String match to name in use
!         -43  Attribute not found
!         -44  NC_MAX_ATTRS exceeded
!         -45  Not a netcdf data type
!         -46  Invalid dimension id or name
!         -47  NC_UNLIMITED in the wrong index
!         -48  NC_MAX_VARS exceeded
!         -49  Variable not found
!         -50  Action prohibited on NC_GLOBAL varid
!         -51  Not a netcdf file
!         -52  In Fortran, string too short
!         -53  NC_MAX_NAME exceeded
!         -54  NC_UNLIMITED size already in use
!         -55  nc_rec op when there are no record vars
!         -56  Attempt to convert between text & numbers
!         -57  Edge+start exceeds dimension bound
!         -58  Illegal stride
!         -59  Attribute or variable name contains illegal characters
!         -60  Math result not representable
!         -61  Memory allocation (malloc) failure
!         -62  One or more variable sizes violate format constraints
!         -63  Invalid dimension size
!         -64  File likely truncated or possibly corrupted
!         -65  Unknown axis type
!
!  History Log:
!           03/28/2019
!
!  Usage:
!    Input File:
!          GRIDFILE: Model grid file which is provided at "fix" folder.
!    Output File: NETCDF_FILE
!          NETCDF_FILE: NetCDF file which provides OBC forcing data.
!
      subroutine nos_ofs_write_netCDF_obc_fvcom_gl(GRIDFILE,netcdf_file,
     & ncid,imode,time_len,node_len,nele_len,siglay_len,siglev_len,
     & base_date,Itime,Itime2,Times,h,lat,lon,latc,lonc,nv,siglay,
     & siglev,obc_nodes,elevation,obc_temp,obc_salinity,u,v,ua,va,
     &  partition,globalstr)
      include 'netcdf.inc'
      CHARACTER*120 TEXT,CNAME,netcdf_file,GRIDFILE
      INTEGER LEN,base_date(4),intval(4),CORNER(4),COUNT(4)
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      character globalstr(9)*120
* error status return
      integer  iret
* netCDF id
      integer  ncid,ncid1
* dimension ids
      integer, save ::  time_dim
      integer, save ::  nobc_dim
      integer, save ::  siglev_dim
      integer, save ::  nele_dim
      integer, save ::  three_dim
      integer, save ::  siglay_dim
      integer, save ::  DateStrLen_dim
* dimension lengths
      integer  time_len
      integer  node_len
      integer  nele_len
      integer  siglev_len
      integer  siglay_len
      integer  DateStrLen_len
      parameter (DateStrLen_len = 26)
* variable ids
      integer, save ::  Itime_id
      integer, save ::  Itime2_id
      integer, save ::  h_id
C       integer, save ::  hyw_id
C       integer, save ::  iint_id
      integer, save ::  lat_id
      integer, save ::  latc_id
      integer, save ::  lon_id
      integer, save ::  lonc_id
C       integer, save ::  nprocs_id
      integer, save ::  nv_id
      integer, save ::  partition_id
      integer, save ::  siglay_id
      integer, save ::  siglev_id
      integer, save ::  obc_temp_id
      integer, save ::  obc_salinity_id
      integer, save ::  time_id
      integer, save ::  u_id
      integer, save ::  ua_id
      integer, save ::  v_id
      integer, save ::  va_id
C       integer, save ::  x_id
C       integer, save ::  xc_id
C       integer, save ::  y_id
C       integer, save ::  yc_id
      integer, save ::  elevation_id   !!! machuan
      integer, save :: obc_nodes_id
      integer, save ::  Times_id
C       integer, save ::  weight_cell_id
C       integer, save ::  weight_node_id
!!* variable logical

      logical, save ::  h_L
C       logical, save ::  hyw_L
C       logical, save ::  iint_L
      logical, save ::  latc_L
      logical, save ::  lonc_L
C       logical, save ::  nprocs_L
      logical, save ::  nv_L
      logical, save ::  partition_L
!      logical, save ::  siglay_L
!      logical, save ::  siglev_L
      logical, save ::  obc_temp_L
      logical, save ::  obc_salinity_L
      logical, save ::  u_L
      logical, save ::  ua_L
      logical, save ::  v_L
      logical, save ::  va_L
C       logical, save ::  x_L
C       logical, save ::  xc_L
C       logical, save ::  y_L
C       logical, save ::  yc_L
      logical, save ::  elevation_L
C       logical, save ::  weight_cell_L
C       logical, save ::  weight_node_L


* data variables
      character Times(time_len)*DateStrLen_len 
      integer  Itime(time_len)
      integer  Itime2(time_len)
C       integer  IINT(time_len)
      integer obc_nodes(node_len)

      real  time(time_len)
      real  h(node_len)
C      real  hyw(node_len,siglev_len,time_len)
      real  lat(node_len)
      real  lon(node_len)
      real  latc(nele_len)
      real  lonc(nele_len)
      integer  nv(nele_len, 3)
      integer  partition(nele_len)
      real  siglay(node_len, siglay_len)
      real  siglev(node_len, siglev_len)
      real  elevation(node_len,time_len)
      real  obc_temp(node_len, siglay_len,time_len)
      real  obc_salinity(node_len, siglay_len,time_len)
      real  u(nele_len, siglay_len,time_len)
      real  v(nele_len, siglay_len,time_len)
      real  ua(nele_len,time_len)
      real  va(nele_len,time_len)
C       real  weight_node(node_len,time_len)
C       real  weight_cell(nele_len,time_len)

CCCCCCCCCCCCC 

      if (imode.eq.1) then           ! Write the file message 
      write(*,*)'start writing OBC NetCDF'
      h_L = .TRUE.
      latc_L = .TRUE.
      lonc_L = .TRUE.
      nv_L = .TRUE.
      partition_L = .TRUE.
!      siglay_L = .TRUE.
!      siglev_L = .TRUE.
      elevation_L = .TRUE. 
      obc_temp_L = .TRUE.
      obc_salinity_L = .TRUE.
      u_L = .TRUE.
      ua_L = .TRUE.
      v_L = .TRUE.
      va_L = .TRUE.
C       x_L = .TRUE.
C       xc_L = .TRUE.
C       y_L = .TRUE.
C       yc_L = .TRUE.
C       weight_cell_L = .TRUE.
C       weight_node_L = .TRUE.
      
C      IF (h(1) .le. 0) h_L = .FALSE.
      IF(latc(1) .le. 0) THEN
       latc_L = .FALSE.
       lonc_L = .FALSE.
      ENDIF
      IF(nv(1,1)) nv_L = .FALSE.
      IF(elevation(1,1) .le. 0) elevation_L = .FALSE. 
      IF(obc_temp(1,1,1) .le. 0) obc_temp_L = .FALSE.
      IF(obc_salinity(1,1,1) .le. 0) obc_salinity_L = .FALSE.
      IF(u(1,1,1) .le. 0) THEN
        u_L = .FALSE.
        v_L = .FALSE.
      ENDIF
      IF(ua(1,1) .le. 0) THEN
        ua_L = .FALSE.
        va_L = .FALSE.
      ENDIF
      IF(partition(1) .le. 0) partition_L = .FALSE.
* enter define mode
      iret = nf_create(trim(netcdf_file), NF_CLOBBER, ncid)
      call check_err(iret)
* define dimensions
      iret = nf_def_dim(ncid, 'time', NF_UNLIMITED, time_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'nobc', node_len, nobc_dim) ! machuan dim
      call check_err(iret)
      iret = nf_def_dim(ncid, 'siglev',siglev_len , siglev_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'nele',nele_len , nele_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'three', 3, three_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'siglay',siglay_len , siglay_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'DateStrLen', DateStrLen_len,
     1   DateStrLen_dim)
      call check_err(iret)
* define variables
      intval(1) = time_dim
      iret = nf_def_var(ncid, 'Itime', NF_INT, 1, 
     1intval, Itime_id)
      call check_err(iret)
      WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3),' ',base_date(4),':00:00'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime_id, 'units',
     &       LEN,TRIM(TEXT))
      TEXT='modified julian day(MJD)'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime_id, 'format',
     &       LEN,TRIM(TEXT))
      TEXT='UTC'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime_id, 'time_zone',
     &       LEN,TRIM(TEXT))
      call check_err(iret)
      
      iret = nf_def_var(ncid, 'Itime2', NF_INT, 1,
     1intval, Itime2_id)
      call check_err(iret)
      TEXT='mseconds from integer day'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime2_id, 'long_name',
     &       LEN,TRIM(TEXT))
      TEXT='msec since 00:00:00'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime2_id, 'units',
     &       LEN,TRIM(TEXT))
      TEXT='modified julian day(MJD)'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime2_id, 'format',
     &       LEN,TRIM(TEXT))
      TEXT='UTC'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime2_id, 'time_zone',
     &       LEN,TRIM(TEXT))
      call check_err(iret)
      
      iret = nf_def_var(ncid, 'time', NF_REAL, 1,intval, time_id)
      call check_err(iret)
      iret = nf_put_att_text(ncid, time_id, 'long_name', 4, 'time')
      call check_err(iret)
      iret = nf_put_att_text(ncid, time_id, 'units', 30, 
     1'days since 1858-11-17 00:00:00')
      call check_err(iret)
      iret = nf_put_att_text(ncid, time_id, 'format', 25, 
     1'modified julian day (MJD)')
      call check_err(iret)
      iret = nf_put_att_text(ncid, time_id, 'time_zone', 3, 'UTC')
      call check_err(iret)  
    
      intval(2) = time_dim
      intval(1) = DateStrLen_dim
      iret = nf_def_var(ncid, 'Times', NF_CHAR, 2,intval,Times_id)
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
      call check_err(iret)

      intval(1) = nobc_dim
      iret = nf_def_var(ncid, 'obc_nodes', NF_INT,1,intval,
     &  obc_nodes_id)
      call check_err(iret) 
      iret = nf_put_att_text(ncid, obc_nodes_id, 'long_name', 14,   
     1'node node node')
           
      intval(1) = nobc_dim    
      iret = nf_def_var(ncid, 'lat', NF_REAL, 1, intval, lat_id)
      call check_err(iret)
      iret = nf_put_att_text(ncid, lat_id, 'long_name', 14, 
     1'nodal latitude')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lat_id, 'standard_name', 8, 
     1'latitude')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lat_id, 'units', 13, 
     1 'degrees_north')
      call check_err(iret)
              
      iret = nf_def_var(ncid, 'lon', NF_REAL, 1, intval, lon_id)
      call check_err(iret)
      call check_err(iret)
      iret = nf_put_att_text(ncid, lon_id, 'long_name', 15, 
     1'nodal longitude')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lon_id, 'standard_name', 9, 
     1'longitude')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lon_id, 'units', 12, 
     1 'degrees_east')
      call check_err(iret)
      
      intval(2) = siglay_dim
      intval(1) = nobc_dim
      iret = nf_def_var(ncid, 'siglay',NF_REAL,2,intval,siglay_id)
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglay_id, 'long_name', 12, 
     1'Sigma Layers')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglay_id, 'standard_name', 30, 
     1'ocean_sigma/general_coordinate')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglay_id, 'positive', 2, 'up')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglay_id, 'valid_min', 2, '-1')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglay_id, 'valid_max', 1, '0')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglay_id, 'formula_terms', 32, 
     1'sigma: siglay eta: elevation depth: h')
     
      intval(2) = siglev_dim
      intval(1) = nobc_dim
      iret = nf_def_var(ncid, 'siglev', NF_REAL,2,intval,siglev_id)
      call check_err(iret) 
      iret = nf_put_att_text(ncid, siglev_id, 'long_name', 12, 
     1'Sigma Levels')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglev_id, 'standard_name', 30, 
     1'ocean_sigma/general_coordinate')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglev_id, 'positive', 2, 'up')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglev_id, 'valid_min', 2, '-1')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglev_id, 'valid_max', 1, '0')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglev_id, 'formula_terms', 31, 
     1'sigma:siglay eta: elevation depth: h')
      call check_err(iret)
      
!      intval(1) = time_dim
!      IF (iint_L) THEN
!        iret = nf_def_var(ncid, 'iint', NF_INT, 1, intval, iint_id)
!        call check_err(iret)
!        iret = nf_put_att_text(ncid, iint_id, 'long_name', 30, 
!     1'internal mode iteration number')
!        call check_err(iret)
!      ENDIF
 
      intval(3) = time_dim
      intval(2) = siglev_dim
      intval(1) = nobc_dim
      IF (h_L) THEN
        iret = nf_def_var(ncid, 'h', NF_REAL, 1,intval , h_id)
        call check_err(iret)
        iret = nf_put_att_text(ncid, h_id, 'long_name', 10, 'Bathymetry')
        call check_err(iret)
        iret = nf_put_att_text(ncid, h_id, 'standard_name', 27, 
     1'sea_floor_depth_below_geoid')
        call check_err(iret)
        iret = nf_put_att_text(ncid, h_id, 'units', 1, 'm')
        call check_err(iret)
        iret = nf_put_att_text(ncid, h_id, 'positive', 4, 'down')
        call check_err(iret)
        iret = nf_put_att_text(ncid, h_id, 'grid', 15, 'Bathymetry_Mesh')
        call check_err(iret)
        iret = nf_put_att_text(ncid, h_id, 'coordinates', 7, 'lat lon')
        call check_err(iret)
        iret = nf_put_att_text(ncid, h_id, 'type', 4, 'data')
        call check_err(iret)
      ENDIF
      
C       IF (hyw_L) THEN
C         iret = nf_def_var(ncid, 'hyw', NF_REAL, 3,intval , hyw_id)
C         call check_err(iret)
C         iret = nf_put_att_text(ncid, hyw_id, 'long_name', 30, 
C      1'hydro static vertical velocity')
C         call check_err(iret)
C         iret = nf_put_att_text(ncid, hyw_id, 'units', 3, 'm/s')
C         call check_err(iret)
C         iret = nf_put_att_text(ncid, hyw_id, 'grid', 10, 'fvcom_grid')
C         call check_err(iret)
C         iret = nf_put_att_text(ncid, hyw_id, 'coordinates', 7, 'lat lon')
C         call check_err(iret)
C         iret = nf_put_att_text(ncid, hyw_id, 'type', 4, 'data')
C         call check_err(iret)
C       ENDIF

      IF (latc_L) THEN    
        intval(1) = nele_dim
        iret = nf_def_var(ncid, 'latc', NF_REAL, 1, intval,latc_id)
        call check_err(iret)
        iret = nf_def_var(ncid, 'lonc', NF_REAL,1, intval, lonc_id)
        call check_err(iret)
        iret = nf_put_att_text(ncid, latc_id, 'long_name', 14, 
     1'zonal latitude')
        call check_err(iret)
        iret = nf_put_att_text(ncid, latc_id, 'standard_name', 8, 
     1'latitude')
        call check_err(iret)
        iret = nf_put_att_text(ncid, latc_id, 'units', 13, 
     1'degrees_north')
        call check_err(iret)

        iret = nf_put_att_text(ncid, lonc_id, 'long_name', 15, 
     1'zonal longitude')
        call check_err(iret)
        iret = nf_put_att_text(ncid, lonc_id, 'standard_name', 9, 
     1'longitude')
        call check_err(iret)
        iret = nf_put_att_text(ncid, lonc_id, 'units', 12, 
     1 'degrees_east')
        call check_err(iret)
      ENDIF
     
C       IF (nprocs_L) THEN     
C           iret = nf_def_var(ncid, 'nprocs', NF_INT, 0, 0, nprocs_id)
C           call check_err(iret)
C           iret = nf_put_att_text(ncid, nprocs_id, 'long_name', 20, 
C      1'number of processors')
C           call check_err(iret)
C       ENDIF
      
      IF(nv_L) THEN
          intval(2) = three_dim
          intval(1) = nele_dim
          iret = nf_def_var(ncid, 'nv', NF_INT, 2, intval, nv_id)
          call check_err(iret)
          iret = nf_put_att_text(ncid, nv_id, 'long_name', 25, 
     1'nodes surrounding element')
          call check_err(iret)
      ENDIF
      IF (partition_L) THEN
          intval(1) = nele_dim
          iret=nf_def_var(ncid,'partition',NF_INT,1,intval,partition_id)
          call check_err(iret)
          iret = nf_put_att_text(ncid, partition_id, 'long_name', 9, 
     1'partition')
          call check_err(iret)
      ENDIF      

      IF (elevation_L) THEN
          intval(2) = time_dim
          intval(1) = nobc_dim
          iret = nf_def_var(ncid, 'elevation', NF_REAL,2,intval,
     1            elevation_id)  
          call check_err(iret)
          iret = nf_put_att_text(ncid, elevation_id, 'standard_name', 21,   
     1'sea_surface_elevation')
          call check_err(iret)
          iret = nf_put_att_text(ncid, elevation_id, 'long_name', 23, 
     1'Water Surface Elevation')
          call check_err(iret)
          iret = nf_put_att_text(ncid, elevation_id, 'units', 6, 'meters')
          call check_err(iret)
          iret = nf_put_att_text(ncid, elevation_id, 'positive', 2, 'up')
          call check_err(iret)
          iret = nf_put_att_text(ncid, elevation_id, 'grid', 8, 'SSH_Mesh')
          call check_err(iret)
          iret = nf_put_att_text(ncid, elevation_id, 'coordinates', 7,  ! machuan
     1'sea_surface_elevation')
          call check_err(iret)
          iret = nf_put_att_text(ncid, elevation_id, 'type', 4, 'data')
          call check_err(iret)
      ENDIF
 
      intval(3) = time_dim
      intval(2) = siglay_dim
      intval(1) = nobc_dim

      IF(obc_temp_L) THEN
          iret = nf_def_var(ncid, 'obc_temp', NF_REAL,3,intval, obc_temp_id)
          call check_err(iret)
          iret = nf_put_att_text(ncid, obc_temp_id, 'long_name', 11, 
     1'temperature')
          call check_err(iret)
          iret = nf_put_att_text(ncid, obc_temp_id, 'standard_name', 21, 
     1'sea_water_temperature')
          call check_err(iret)
          iret = nf_put_att_text(ncid, obc_temp_id, 'units', 7, 'Celsius')
          call check_err(iret)
          iret = nf_put_att_text(ncid, obc_temp_id, 'grid', 10, 
     1 'fvcom_grid')
          call check_err(iret)
          iret = nf_put_att_text(ncid, obc_temp_id, 'coordinates',7,
     1 'lat lon')
          call check_err(iret)
          iret = nf_put_att_text(ncid, obc_temp_id, 'type', 4, 'data')
          call check_err(iret)
      ENDIF
      
      IF (obc_salinity_L) THEN
          iret=nf_def_var(ncid,'obc_salinity',NF_REAL,3,intval,
     &obc_salinity_id)
          call check_err(iret)
          iret = nf_put_att_text(ncid, obc_salinity_id, 'long_name', 8, 
     1'salinity')
          call check_err(iret)
          iret = nf_put_att_text(ncid, obc_salinity_id, 'standard_name', 18, 
     1'sea_water_salinity')
          call check_err(iret)
          iret = nf_put_att_text(ncid, obc_salinity_id, 'units', 3, 'PSU')
          call check_err(iret)
          iret = nf_put_att_text(ncid, obc_salinity_id, 'grid', 10, 
     1'fvcom_grid')
          call check_err(iret)
          iret = nf_put_att_text(ncid, obc_salinity_id, 'coordinates', 7, 
     1'lat lon')
          call check_err(iret)
          iret = nf_put_att_text(ncid, obc_salinity_id, 'type', 4, 'data')
          call check_err(iret)
      ENDIF
      
      IF (u_L) THEN
          intval(3) = time_dim
          intval(2) = siglay_dim
          intval(1) = nele_dim
          iret = nf_def_var(ncid, 'u', NF_REAL,3,intval, u_id)
          iret = nf_def_var(ncid, 'v', NF_REAL,3,intval, v_id)
          call check_err(iret)
          iret = nf_put_att_text(ncid, u_id, 'long_name', 23, 
     1'Eastward Water Velocity')
          call check_err(iret)
          iret = nf_put_att_text(ncid, u_id, 'units', 10, 'meters s-1')
          call check_err(iret)
          iret = nf_put_att_text(ncid, u_id, 'grid', 10, 'fvcom_grid')
          call check_err(iret)
          iret = nf_put_att_text(ncid, u_id, 'type', 4, 'data')
          call check_err(iret)
          iret = nf_put_att_text(ncid, v_id, 'long_name', 24, 
     1'Northward Water Velocity')
          call check_err(iret)
          iret = nf_put_att_text(ncid, v_id, 'units', 10, 'meters s-1')
          call check_err(iret)
          iret = nf_put_att_text(ncid, v_id, 'grid', 10, 'fvcom_grid')
          call check_err(iret)
          iret = nf_put_att_text(ncid, v_id, 'type', 4, 'data')
          call check_err(iret)
      ENDIF      
      IF (ua_L) THEN      
          intval(2) = time_dim
          intval(1) = nele_dim
          iret = nf_def_var(ncid, 'ua', NF_REAL,2,intval, ua_id)
          iret = nf_def_var(ncid, 'va', NF_REAL,2,intval, va_id)
          call check_err(iret)
          iret = nf_put_att_text(ncid, ua_id, 'long_name', 30, 
     1'Vertically Averaged x-velocity')
          call check_err(iret)
          iret = nf_put_att_text(ncid, ua_id, 'units', 10, 'meters s-1')
          call check_err(iret)
          iret = nf_put_att_text(ncid, ua_id, 'grid', 10, 'fvcom_grid')
          call check_err(iret)
          iret = nf_put_att_text(ncid, ua_id, 'type', 4, 'data')
          call check_err(iret)
          iret = nf_put_att_text(ncid, va_id, 'long_name', 30, 
     1'Vertically Averaged y-velocity')
          call check_err(iret)
          iret = nf_put_att_text(ncid, va_id, 'units', 10, 'meters s-1')
          call check_err(iret)
          iret = nf_put_att_text(ncid, va_id, 'grid', 10, 'fvcom_grid')
          call check_err(iret)
          iret = nf_put_att_text(ncid, va_id, 'type', 4, 'data')
          call check_err(iret)
      ENDIF
      
C Global Attributes
      TEXT=trim(globalstr(1))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid,NF_GLOBAL ,'type', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(2))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'title', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(3))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'data_source', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(4))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'Temp_source', 
     &       LEN,TRIM(TEXT))

      TEXT=trim(globalstr(5))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'model_grid_file', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(6))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'output_file', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(7))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'source_code', 
     &       LEN,TRIM(TEXT))
     
      TEXT=trim(globalstr(8))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'history', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(9))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'reference', 
     &       LEN,TRIM(TEXT))
      call check_err(iret)
      
c leave define mode
      iret = nf_enddef(ncid)
      call check_err(iret)
      print *,'end of define mode' 
c Write record variables
 
      CORNER(1) = 1 
      COUNT(1)=node_len
      iret=nf_put_vara_real(ncid,h_id,CORNER,COUNT,h)
      iret=nf_put_vara_real(ncid,lat_id,CORNER,COUNT,lat)
      iret=nf_put_vara_real(ncid,lon_id,CORNER,COUNT,lon)
      iret=nf_put_vara_int(ncid,obc_nodes_id,CORNER,COUNT,obc_nodes)
C       IF(nprocs_L)
C            iret=nf_put_vara_int(ncid,nprocs_id,1,1,nprocs)
C       ENDIF
C       IF(x_L) THEN
C         iret=nf_put_vara_real(ncid,x_id,CORNER,COUNT,x)
C         iret=nf_put_vara_real(ncid,y_id,CORNER,COUNT,y)
C       ENDIF

      CORNER(1) = 1
      CORNER(2) = 1
      COUNT(1)=node_len
      COUNT(2)=siglay_len
      iret=nf_put_vara_real(ncid,siglay_id,CORNER,COUNT,siglay)
      CORNER(1) = 1
      CORNER(2) = 1
      COUNT(1)=node_len
      COUNT(2)=siglev_len
      iret=nf_put_vara_real(ncid,siglev_id,CORNER,COUNT,siglev)

      elseif (imode.eq.2) then         ! Write the data into the file 
C     write time independent variables
      CORNER(1) = 1
      COUNT(1)=nele_len
      IF(latc_L) THEN
          iret=nf_put_vara_real(ncid,latc_id,CORNER,COUNT,latc)
          iret=nf_put_vara_real(ncid,lonc_id,CORNER,COUNT,lonc)
      ENDIF
C       IF(xc_L) THEN
C           iret=nf_put_vara_real(ncid,xc_id,CORNER,COUNT,xc)
C           iret=nf_put_vara_real(ncid,yc_id,CORNER,COUNT,yc)
C       ENDIF
      IF(partition_L) THEN
        iret=nf_put_vara_int(ncid,partition_id,CORNER,COUNT,partition)
      ENDIF
      IF(nv_L) THEN
          CORNER(1) = 1
          CORNER(2) = 1
          COUNT(1)=nele_len
          COUNT(2)=3
          iret=nf_put_vara_int(ncid,nv_id,CORNER,COUNT,nv)
      ENDIF

      iret= nf_inq_dimlen(ncid,time_dim,jtime)
      jtime=jtime+1
            
      DO N=1,time_len
C        iint(N)=jtime
        ITT=Itime2(N)/1000
        time(N)=Itime(N)+ITT/86400.
      ENDDO 

      CORNER(1) = jtime 
      COUNT(1)=time_len
      iret=nf_put_vara_int(ncid,Itime_id,CORNER,COUNT,Itime)
      iret=nf_put_vara_int(ncid,Itime2_id,CORNER,COUNT,Itime2)
      iret=nf_put_vara_real(ncid,time_id,CORNER,COUNT,time)
      call check_err(iret)

c      IF(iint_L)
c     & iret=nf_put_vara_int(ncid,iint_id,CORNER,COUNT,iint)

      CORNER(1) = 1
      CORNER(2) = jtime
      COUNT(1)=DateStrLen_len
      COUNT(2)=1
      iret=nf_put_vara_text(ncid,Times_id,CORNER,COUNT,Times)
      call check_err(iret)

      CORNER(1) = 1
      CORNER(2) = jtime
      COUNT(1)=node_len
      COUNT(2)=time_len
      iret=nf_put_vara_real(ncid,elevation_id,CORNER,COUNT,elevation)  !machuan
      iret=nf_put_vara_real(ncid,weight_node_id,CORNER,COUNT,
     1 weight_node)

      CORNER(1) = 1
      CORNER(2) = jtime
      COUNT(1)=nele_len
      COUNT(2)=time_len
      IF(ua_L) THEN
          iret=nf_put_vara_real(ncid,ua_id,CORNER,COUNT,ua)
          iret=nf_put_vara_real(ncid,va_id,CORNER,COUNT,va)
      ENDIF
C       IF(weight_cell_L)
C      & iret=nf_put_vara_real(ncid,weight_cell_id,CORNER,COUNT,
C      1 weight_cell)
     
      CORNER(1) = 1
      CORNER(2) = 1
      CORNER(3) = jtime
      COUNT(1)=node_len
      COUNT(2)=siglay_len
      COUNT(3)=time_len
      IF(obc_salinity_L)
     & iret=nf_put_vara_real(ncid,obc_salinity_id,CORNER,COUNT,
     & obc_salinity)

      IF(obc_temp_L)
     &   iret=nf_put_vara_real(ncid,obc_temp_id,CORNER,COUNT,obc_temp)
   
      IF (u_L) THEN 
          CORNER(1) = 1
          CORNER(2) = 1
          CORNER(3) = jtime
          COUNT(1)=nele_len
          COUNT(2)=siglay_len
          COUNT(3)=time_len
          iret=nf_put_vara_real(ncid,u_id,CORNER,COUNT,u)
          iret=nf_put_vara_real(ncid,v_id,CORNER,COUNT,v)
      ENDIF
     
C       IF(hyw_L) THEN     
C           CORNER(1) = 1
C           CORNER(2) = 1
C           CORNER(3) = jtime
C           COUNT(1)=node_len
C           COUNT(2)=siglev_len
C           COUNT(3)=time_len
C           iret=nf_put_vara_real(ncid,hyw_id,CORNER,COUNT,hyw)
C       ENDIF

      elseif (imode.eq.3) then             ! Close the netCDF  
      iret = nf_close(ncid)
      endif
      call check_err(iret)
      RETURN
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
