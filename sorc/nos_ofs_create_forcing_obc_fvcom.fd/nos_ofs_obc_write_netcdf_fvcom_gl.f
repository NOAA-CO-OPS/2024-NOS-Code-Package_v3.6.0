!--------------------------------------------------------------------------
!
!  Program Name:  nos_ofs_create_forcing_obc_fvcom.fd/nos_ofs_obc_write_netcdf_fvcom_gl.f
!
!  Contact: NOS/CO-OPS Aijun Zhang
!           Phone: 240-533-0591   Email: aijun.zhang@noaa.gov
!
!  Abstract:  This is a SUBROUTINE to create a NetCDF file (imode=1),
!           store the data to NetCDF file (imode=2), and close the
!           NetCDF file (imode=3) for FVCOM-base OFS OBC forcing (such
!           as SFBOFS). This subroutine is called by program
!           nos_ofs_create_forcing_obc_fvcom.f.
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
      SUBROUTINE NOS_OFS_WRITE_NETCDF_OBC_FVCOM_GL(GRIDFILE,NETCDF_FILE,
     &  NCID,IMODE,TIME_LEN,NODE_LEN,NELE_LEN,SIGLAY_LEN,SIGLEV_LEN,
     &  BASE_DATE,ITIME,ITIME2,TIMES,H,LAT,LON,LATC,LONC,NV,SIGLAY,
     &  SIGLEV,OBC_NODES,ELEVATION,OBC_TEMP,OBC_SALINITY,U,V,UA,VA,
     &  PARTITION,GLOBALSTR)
      include 'netcdf.inc'
      CHARACTER*120 TEXT,CNAME,netcdf_file,GRIDFILE
      INTEGER LRR,BASE_DATE(4),INTVAL(4),CORNER(4),COUNT(4)
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      CHARACTER GLOBALSTR(9)*120
cc error status return
      INTEGER  IRET
cc netCDF id
      INTEGER  NCID
cc dimension ids
      INTEGER, SAVE ::  TIME_DIM
      INTEGER, SAVE ::  NOBC_DIM
      INTEGER, SAVE ::  SIGLEV_DIM
      INTEGER, SAVE ::  NELE_DIM
      INTEGER, SAVE ::  THREE_DIM
      INTEGER, SAVE ::  SIGLAY_DIM
      INTEGER, SAVE ::  DATESTRLEN_DIM
cc dimension lengths
      INTEGER  TIME_LEN
      INTEGER  NODE_LEN
      INTEGER  NELE_LEN
      INTEGER  SIGLEV_LEN
      INTEGER  SIGLAY_LEN
      INTEGER  DATESTRLEN_LEN
      PARAMETER (DATESTRLEN_LEN = 26)

cc variable ids
      INTEGER, SAVE ::  ITIME_ID
      INTEGER, SAVE ::  ITIME2_ID
      INTEGER, SAVE ::  H_ID
      INTEGER, SAVE ::  LAT_ID
      INTEGER, SAVE ::  LATC_ID
      INTEGER, SAVE ::  LON_ID
      INTEGER, SAVE ::  LONC_ID
      INTEGER, SAVE ::  NV_ID
      INTEGER, SAVE ::  PARTITION_ID
      INTEGER, SAVE ::  SIGLAY_ID
      INTEGER, SAVE ::  SIGLEV_ID
      INTEGER, SAVE ::  OBC_TEMP_ID
      INTEGER, SAVE ::  OBC_SALINITY_ID
      INTEGER, SAVE ::  TIME_ID
      INTEGER, SAVE ::  U_ID
      INTEGER, SAVE ::  UA_ID
      INTEGER, SAVE ::  V_ID
      INTEGER, SAVE ::  VA_ID
      INTEGER, SAVE ::  ELEVATION_ID   !!! machuan
      INTEGER, SAVE :: OBC_NODES_ID
      INTEGER, SAVE ::  TIMES_ID

cc variable logical
      LOGICAL, SAVE ::  H_L
      LOGICAL, SAVE ::  LATC_L
      LOGICAL, SAVE ::  LONC_L
      LOGICAL, SAVE ::  NV_L
      LOGICAL, SAVE ::  PARTITION_L
      LOGICAL, SAVE ::  OBC_TEMP_L
      LOGICAL, SAVE ::  OBC_SALINITY_L
      LOGICAL, SAVE ::  U_L
      LOGICAL, SAVE ::  UA_L
      LOGICAL, SAVE ::  V_L
      LOGICAL, SAVE ::  VA_L
      LOGICAL, SAVE ::  ELEVATION_L

cc data variables
      CHARACTER TIMES(TIME_LEN)*DATESTRLEN_LEN 
      INTEGER  ITIME(TIME_LEN)
      INTEGER  ITIME2(TIME_LEN)
      INTEGER OBC_NODES(NODE_LEN)

      REAL  TIME(TIME_LEN)
      REAL  H(NODE_LEN)
      REAL  LAT(NODE_LEN)
      REAL  LON(NODE_LEN)
      REAL  LATC(NELE_LEN)
      REAL  LONC(NELE_LEN)
      INTEGER  NV(NELE_LEN, 3)
      INTEGER  PARTITION(NELE_LEN)
      REAL  SIGLAY(NODE_LEN, SIGLAY_LEN)
      REAL  SIGLEV(NODE_LEN, SIGLEV_LEN)
      REAL  ELEVATION(NODE_LEN,TIME_LEN)
      REAL  OBC_TEMP(NODE_LEN, SIGLAY_LEN,TIME_LEN)
      REAL  OBC_SALINITY(NODE_LEN, SIGLAY_LEN,TIME_LEN)
      REAL  U(NELE_LEN, SIGLAY_LEN,TIME_LEN)
      REAL  V(NELE_LEN, SIGLAY_LEN,TIME_LEN)
      REAL  UA(NELE_LEN,TIME_LEN)
      REAL  VA(NELE_LEN,TIME_LEN)

CCCCCCCCCCCCC 
      IF(IMODE.EQ.1) THEN           ! Write the file message 
        WRITE(*,*) 'Start writing OBC NetCDF'
        H_L=.TRUE.
        LATC_L=.TRUE.
        LONC_L=.TRUE.
        NV_L=.TRUE.
        PARTITION_L = .TRUE.
        ELEVATION_L = .TRUE. 
        OBC_TEMP_L = .TRUE.
        OBC_SALINITY_L = .TRUE.
        U_L = .TRUE.
        UA_L = .TRUE.
        V_L = .TRUE.
        VA_L = .TRUE.
      
        IF(LATC(1).LE.0) THEN
          LATC_L = .FALSE.
          LONC_L = .FALSE.
        ENDIF
        IF(NV(1,1)) NV_L = .FALSE.
        IF(ELEVATION(1,1).LE.0) ELEVATION_L = .FALSE. 
        IF(OBC_TEMP(1,1,1).LE.0) OBC_TEMP_L = .FALSE.
        IF(OBC_SALINITY(1,1,1).LE.0) OBC_SALINITY_L = .FALSE.
        IF(U(1,1,1).LE.0) THEN
          U_L = .FALSE.
          V_L = .FALSE.
        ENDIF
        IF(UA(1,1).LE.0) THEN
          UA_L = .FALSE.
          VA_L = .FALSE.
        ENDIF
        IF(PARTITION(1).LE.0) PARTITION_L = .FALSE.
cc enter define mode
        IRET=NF_CREATE(TRIM(NETCDF_FILE), NF_CLOBBER, NCID)
        CALL CHECK_ERR(IRET)

cc define dimensions
        IRET=NF_DEF_DIM(NCID,'nobc',NODE_LEN,NOBC_DIM) ! machuan dim
        CALL CHECK_ERR(IRET)
        IRET=NF_DEF_DIM(NCID,'nele',NELE_LEN ,NELE_DIM)
        CALL CHECK_ERR(IRET)
        IRET=NF_DEF_DIM(NCID,'siglay',SIGLAY_LEN ,SIGLAY_DIM)
        CALL CHECK_ERR(IRET)
        IRET=NF_DEF_DIM(NCID,'siglev',SIGLEV_LEN ,SIGLEV_DIM)
        CALL CHECK_ERR(IRET)
        IRET=NF_DEF_DIM(NCID,'three',3,THREE_DIM)
        CALL CHECK_ERR(IRET)
        IRET=NF_DEF_DIM(NCID,'DateStrLen',DATESTRLEN_LEN,
     &       DATESTRLEN_DIM)
        CALL CHECK_ERR(IRET)
        IRET=NF_DEF_DIM(NCID,'time',NF_UNLIMITED,TIME_DIM)
        CALL CHECK_ERR(IRET)

cc define variables
        INTVAL(1)=TIME_DIM
        IRET=NF_DEF_VAR(NCID,'Itime',NF_INT,1,INTVAL,ITIME_ID)
        CALL CHECK_ERR(IRET)
        WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)') 'days since ',
     &    base_date(1),'-',base_date(2),'-',base_date(3),' ',
     &    base_date(4),':00:00'
        LRR=LEN_TRIM(TEXT)
        IRET=NF_PUT_ATT_TEXT(NCID,ITIME_ID,'units',LRR,TRIM(TEXT))
        CALL CHECK_ERR(IRET)
        TEXT='modified julian day(MJD)'
        LRR=LEN_TRIM(TEXT)
        IRET=NF_PUT_ATT_TEXT(NCID,ITIME_ID,'format',LRR,TRIM(TEXT))
        CALL CHECK_ERR(IRET)
        TEXT='UTC'
        LRR=LEN_TRIM(TEXT)
        IRET=NF_PUT_ATT_TEXT(NCID,ITIME_ID,'time_zone',LRR,TRIM(TEXT))
        CALL CHECK_ERR(IRET)
      
        IRET=NF_DEF_VAR(NCID,'Itime2',NF_INT,1,INTVAL,ITIME2_ID)
        CALL CHECK_ERR(IRET)
        TEXT='mseconds from integer day'
        LRR=LEN_TRIM(TEXT)
        IRET=NF_PUT_ATT_TEXT(NCID,ITIME2_ID,'long_name',
     &       LRR,TRIM(TEXT))
        CALL CHECK_ERR(IRET)
        TEXT='msec since 00:00:00'
        LRR=LEN_TRIM(TEXT)
        IRET=NF_PUT_ATT_TEXT(NCID,ITIME2_ID,'units',LRR,TRIM(TEXT))
        CALL CHECK_ERR(IRET)
        TEXT='modified julian day(MJD)'
        LRR=LEN_TRIM(TEXT)
        IRET=NF_PUT_ATT_TEXT(NCID,ITIME2_ID,'format',LRR,TRIM(TEXT))
        CALL CHECK_ERR(IRET)
        TEXT='UTC'
        LRR=LEN_TRIM(TEXT)
        IRET=NF_PUT_ATT_TEXT(NCID,ITIME2_ID,'time_zone',
     &       LRR,TRIM(TEXT))
        CALL CHECK_ERR(IRET)
      
        IRET=NF_DEF_VAR(NCID,'time',NF_REAL,1,INTVAL,TIME_ID)
        CALL CHECK_ERR(IRET)
        TEXT='time'
        LRR=LEN_TRIM(TEXT)
        IRET=NF_PUT_ATT_TEXT(NCID,TIME_ID,'long_name',LRR,TRIM(TEXT))
        CALL CHECK_ERR(IRET)
        TEXT='days since 1858-11-17 00:00:00'
        LRR=LEN_TRIM(TEXT)
        IRET=NF_PUT_ATT_TEXT(NCID,TIME_ID,'units',LRR,TRIM(TEXT))
        CALL CHECK_ERR(IRET)
        TEXT='modified julian day (MJD)'
        LRR=LEN_TRIM(TEXT)
        IRET=NF_PUT_ATT_TEXT(NCID,TIME_ID,'format',LRR,TRIM(TEXT))
        CALL CHECK_ERR(IRET)
        IRET=NF_PUT_ATT_TEXT(NCID,TIME_ID,'time_zone',3,'UTC')
        CALL CHECK_ERR(IRET)  
    
        intval(2)=time_dim
        intval(1)=DateStrLen_dim
        iret=nf_def_var(ncid,'Times',NF_CHAR,2,intval,Times_id)
        call check_err(iret)
        TEXT='Calendar Date'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Times_id,'long_name',LRR,TRIM(TEXT))
        TEXT='String: Calendar Time'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Times_id,'format',LRR,TRIM(TEXT))
        TEXT='UTC'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid, Times_id,'time_zone',LRR,TRIM(TEXT))
        call check_err(iret)

        intval(1)=nobc_dim
        iret=nf_def_var(ncid,'obc_nodes',NF_INT,1,intval,obc_nodes_id)
        call check_err(iret) 
        iret=nf_put_att_text(ncid,obc_nodes_id,'long_name', 14,  
     &       'node node node')
           
        intval(1)=nobc_dim    
        iret=nf_def_var(ncid,'lat',NF_REAL,1,intval,lat_id)
        call check_err(iret)
        TEXT='nodal latitude'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,lat_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='latitude'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,lat_id,'standard_name', 
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='degrees_north'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid, lat_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
              
        iret=nf_def_var(ncid,'lon',NF_REAL,1,intval,lon_id)
        call check_err(iret)
        TEXT='nodal longitude'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,lon_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='longitude'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,lon_id,'standard_name',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='degrees_east'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,lon_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
      
        intval(2)=siglay_dim
        intval(1)=nobc_dim
        iret=nf_def_var(ncid,'siglay',NF_REAL,2,intval,siglay_id)
        call check_err(iret)
        TEXT='Sigma Layers'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,siglay_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='ocean_sigma/general_coordinate'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,siglay_id,'standard_name', 
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='up'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,siglay_id,'positive',LRR,TRIM(TEXT))
        call check_err(iret)
        iret=nf_put_att_text(ncid,siglay_id,'valid_min',2,'-1')
        call check_err(iret)
        iret=nf_put_att_text(ncid,siglay_id,'valid_max',1,'0')
        call check_err(iret)
        TEXT='sigma: siglay eta: elevation depth: h'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,siglay_id,'formula_terms', 
     &       LRR,TRIM(TEXT))
     
        intval(2)=siglev_dim
        intval(1)=nobc_dim
        iret=nf_def_var(ncid,'siglev',NF_REAL,2,intval,siglev_id)
        call check_err(iret) 
        TEXT='Sigma Levels'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,siglev_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='ocean_sigma/general_coordinate'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,siglev_id,'standard_name',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        iret=nf_put_att_text(ncid,siglev_id,'positive',2,'up')
        call check_err(iret)
        iret=nf_put_att_text(ncid,siglev_id,'valid_min',2,'-1')
        call check_err(iret)
        iret=nf_put_att_text(ncid,siglev_id,'valid_max',1,'0')
        call check_err(iret)
        TEXT='sigma: siglay eta: elevation depth: h'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,siglev_id,'formula_terms',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
      
        intval(3)=time_dim
        intval(2)=siglev_dim
        intval(1)=nobc_dim
        IF(h_L) THEN
          iret=nf_def_var(ncid,'h',NF_REAL,1,intval,h_id)
          call check_err(iret)
          TEXT='Bathymetry'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,h_id,'long_name',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='sea_floor_depth_below_geoid'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,h_id,'standard_name',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='m'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,h_id,'units',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='down'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,h_id,'positive',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='Bathymetry_Mesh'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,h_id,'grid',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='lat lon'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,h_id,'coordinates',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='data'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,h_id,'type',LRR,TRIM(TEXT))
          call check_err(iret)
        ENDIF
      
        IF(latc_L) THEN    
          intval(1)=nele_dim
          iret=nf_def_var(ncid,'latc',NF_REAL,1,intval,latc_id)
          call check_err(iret)
          TEXT='zonal latitude'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,latc_id,'long_name',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='latitude'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,latc_id,'standard_name', 
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='degrees_north'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,latc_id,'units',LRR,TRIM(TEXT))
          call check_err(iret)

          iret=nf_def_var(ncid,'lonc',NF_REAL,1,intval,lonc_id)
          call check_err(iret)
          TEXT='zonal longitude'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,lonc_id,'long_name',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='longitude'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,lonc_id,'standard_name',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='degrees_east'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,lonc_id,'units',LRR,TRIM(TEXT))
          call check_err(iret)
        ENDIF
     
        IF(nv_L) THEN
          intval(2)=three_dim
          intval(1)=nele_dim
          iret=nf_def_var(ncid,'nv',NF_INT,2,intval,nv_id)
          call check_err(iret)
          TEXT='nodes surrounding element'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,nv_id,'long_name',LRR,TRIM(TEXT))
          call check_err(iret)
        ENDIF

        IF(partition_L) THEN
          intval(1)=nele_dim
          iret=nf_def_var(ncid,'partition',NF_INT,1,intval,
     &         partition_id)
          call check_err(iret)
          TEXT='partition'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,partition_id,'long_name',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
        ENDIF      

        IF(elevation_L) THEN
          intval(2)=time_dim
          intval(1)=nobc_dim
          iret=nf_def_var(ncid,'elevation',NF_REAL,2,intval,
     &         elevation_id)  
          call check_err(iret)
          TEXT='sea_surface_elevation'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,elevation_id,'standard_name',   
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='Water Surface Elevation'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,elevation_id,'long_name',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='meters'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,elevation_id,'units',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='up'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,elevation_id,'positive',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='SSH_Mesh'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,elevation_id,'grid',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='sea_surface_elevation'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,elevation_id,'coordinates',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='data'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,elevation_id,'type',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
        ENDIF
 
        intval(3)=time_dim
        intval(2)=siglay_dim
        intval(1)=nobc_dim
        IF(obc_temp_L) THEN
          iret=nf_def_var(ncid,'obc_temp',NF_REAL,3,intval,obc_temp_id)
          call check_err(iret)
          TEXT='temperature'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,obc_temp_id,'long_name', 
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='sea_water_temperature'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,obc_temp_id,'standard_name',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='Celsius'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,obc_temp_id,'units',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='fvcom_grid'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,obc_temp_id,'grid',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='lat lon'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,obc_temp_id,'coordinates',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='data'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,obc_temp_id,'type',LRR,TRIM(TEXT))
          call check_err(iret)
        ENDIF
      
        IF(obc_salinity_L) THEN
          iret=nf_def_var(ncid,'obc_salinity',NF_REAL,3,intval,
     &         obc_salinity_id)
          call check_err(iret)
          TEXT='salinity'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,obc_salinity_id,'long_name',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='sea_water_salinity'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,obc_salinity_id,'standard_name',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='PSU'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,obc_salinity_id,'units',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='fvcom_grid'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,obc_salinity_id,'grid',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='lat lon'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,obc_salinity_id,'coordinates',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='data'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,obc_salinity_id,'type',
     &         LRR,TRIM(TEXT))
          call check_err(iret)
        ENDIF
      
        IF(u_L) THEN
          intval(3)=time_dim
          intval(2)=siglay_dim
          intval(1)=nele_dim
          iret=nf_def_var(ncid,'u',NF_REAL,3,intval,u_id)
          call check_err(iret)
          TEXT='Eastward Water Velocity'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,u_id,'long_name',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='meters s-1'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid, u_id,'units',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='fvcom_grid'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,u_id,'grid',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='data'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,u_id,'type',LRR,TRIM(TEXT))
          call check_err(iret)

          iret=nf_def_var(ncid,'v',NF_REAL,3,intval,v_id)
          call check_err(iret)
          TEXT='Northward Water Velocity'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,v_id,'long_name',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='meters s-1'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,v_id,'units',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='fvcom_grid'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,v_id,'grid',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='data'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,v_id,'type',LRR,TRIM(TEXT))
          call check_err(iret)
        ENDIF      

        IF(ua_L) THEN      
          intval(2)=time_dim
          intval(1)=nele_dim
          iret=nf_def_var(ncid,'ua',NF_REAL,2,intval,ua_id)
          call check_err(iret)
          TEXT='Vertically Averaged x-velocity'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,ua_id,'long_name',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='meters s-1'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,ua_id,'units',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='fvcom_grid'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,ua_id,'grid',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='data'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,ua_id,'type',LRR,TRIM(TEXT))
          call check_err(iret)

          iret=nf_def_var(ncid,'va',NF_REAL,2,intval,va_id)
          call check_err(iret)
          TEXT='Vertically Averaged y-velocity'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,va_id,'long_name',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='meters s-1'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,va_id,'units',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='fvcom_grid'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,va_id,'grid',LRR,TRIM(TEXT))
          call check_err(iret)
          TEXT='data'
          LRR=LEN_TRIM(TEXT)
          iret=nf_put_att_text(ncid,va_id,'type',LRR,TRIM(TEXT))
          call check_err(iret)
        ENDIF
      
CC  Global Attributes
        TEXT=trim(globalstr(1))
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'type',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(2))
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'title',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(3))
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'data_source', 
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(4))
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'Temp_source', 
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(5))
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'model_grid_file', 
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(6))
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'output_file', 
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(7))
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'source_code', 
     &       LRR,TRIM(TEXT))
        call check_err(iret)     
        TEXT=trim(globalstr(8))
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'history',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(9))
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'reference', 
     &       LRR,TRIM(TEXT))
        call check_err(iret)
      
cc  leave define mode
        iret=nf_enddef(ncid)
        call check_err(iret)
        WRITE(*,*) 'End of define mode' 

cc  Write record variables
        CORNER(1)=1 
        COUNT(1)=node_len
        iret=nf_put_vara_real(ncid,h_id,CORNER,COUNT,h)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,lat_id,CORNER,COUNT,lat)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,lon_id,CORNER,COUNT,lon)
        call check_err(iret)
        iret=nf_put_vara_int(ncid,obc_nodes_id,CORNER,COUNT,obc_nodes)
        call check_err(iret)

        CORNER(1)=1
        CORNER(2)=1
        COUNT(1)=node_len
        COUNT(2)=siglay_len
        iret=nf_put_vara_real(ncid,siglay_id,CORNER,COUNT,siglay)
        call check_err(iret)

        COUNT(2)=siglev_len
        iret=nf_put_vara_real(ncid,siglev_id,CORNER,COUNT,siglev)
        call check_err(iret)

      elseif(imode.eq.2) then         ! Write the data into the file 
cc  Write time independent variables
        CORNER(1)=1
        COUNT(1)=nele_len
        IF(latc_L) THEN
          iret=nf_put_vara_real(ncid,latc_id,CORNER,COUNT,latc)
          call check_err(iret)
          iret=nf_put_vara_real(ncid,lonc_id,CORNER,COUNT,lonc)
          call check_err(iret)
        ENDIF

        IF(partition_L) THEN
          iret=nf_put_vara_int(ncid,partition_id,CORNER,COUNT,
     &         partition)
          call check_err(iret)
        ENDIF

        IF(nv_L) THEN
          CORNER(1)=1
          CORNER(2)=1
          COUNT(1)=nele_len
          COUNT(2)=3
          iret=nf_put_vara_int(ncid,nv_id,CORNER,COUNT,nv)
          call check_err(iret)
        ENDIF

        iret=nf_inq_dimlen(ncid,time_dim,jtime)
        call check_err(iret)

        jtime=jtime+1            
        DO N=1,time_len
          ITT=Itime2(N)/1000
          time(N)=Itime(N)+ITT/86400.
        ENDDO 

        CORNER(1)=jtime 
        COUNT(1)=time_len
        iret=nf_put_vara_int(ncid,Itime_id,CORNER,COUNT,Itime)
        call check_err(iret)
        iret=nf_put_vara_int(ncid,Itime2_id,CORNER,COUNT,Itime2)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,time_id,CORNER,COUNT,time)
        call check_err(iret)

        CORNER(1)=1
        CORNER(2)=jtime
        COUNT(1)=DateStrLen_len
        COUNT(2)=1
        iret=nf_put_vara_text(ncid,Times_id,CORNER,COUNT,Times)
        call check_err(iret)

        COUNT(1)=node_len
        COUNT(2)=time_len
        iret=nf_put_vara_real(ncid,elevation_id,CORNER,COUNT,
     &       elevation)
        call check_err(iret)

        COUNT(1)=nele_len
        IF(ua_L) THEN
          iret=nf_put_vara_real(ncid,ua_id,CORNER,COUNT,ua)
          call check_err(iret)
          iret=nf_put_vara_real(ncid,va_id,CORNER,COUNT,va)
          call check_err(iret)
        ENDIF
     
        CORNER(1)=1
        CORNER(2)=1
        CORNER(3)=jtime
        COUNT(1)=node_len
        COUNT(2)=siglay_len
        COUNT(3)=time_len
        IF(obc_salinity_L) THEN
          iret=nf_put_vara_real(ncid,obc_salinity_id,CORNER,COUNT,
     &         obc_salinity)
          call check_err(iret)
        ENDIF

        IF(obc_temp_L) THEN
          iret=nf_put_vara_real(ncid,obc_temp_id,CORNER,COUNT,
     &         obc_temp)
          call check_err(iret)
        ENDIF
   
        IF(u_L) THEN 
          CORNER(1)=1
          CORNER(2)=1
          CORNER(3)=jtime
          COUNT(1)=nele_len
          COUNT(2)=siglay_len
          COUNT(3)=time_len
          iret=nf_put_vara_real(ncid,u_id,CORNER,COUNT,u)
          call check_err(iret)
          iret=nf_put_vara_real(ncid,v_id,CORNER,COUNT,v)
          call check_err(iret)
        ENDIF
     
      ELSEIF(IMODE.EQ.3) THEN             ! Close the netCDF  
        IRET=NF_CLOSE(NCID)
        CALL CHECK_ERR(IRET)
      ENDIF

      RETURN
      END


      SUBROUTINE CHECK_ERR(STATUS)
      include 'netcdf.inc'

      INTEGER STATUS
      IF(STATUS.NE.NF_NOERR) THEN
        WRITE(*,*) 'STATUS=',STATUS
        WRITE(*,*) NF_STRERROR(STATUS)
        STOP
      ENDIF

      RETURN
      END

