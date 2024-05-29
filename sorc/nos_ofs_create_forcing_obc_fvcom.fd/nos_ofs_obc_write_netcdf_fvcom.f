!--------------------------------------------------------------------------
!
!  Program Name:  nos_ofs_create_forcing_obc_fvcom.fd/nos_ofs_obc_write_netcdf_fvcom.f
!
!  Contact: NOS/CO-OPS Aijun Zhang
!           Phone: 240-533-0591   Email: aijun.zhang@noaa.gov
!
!  Abstract:  This is a SUBROUTINE to create a NetCDF file (imode=1), 
!           store the data to NetCDF file (imode=2), and close the 
!           NetCDF file (imode=3) for FVCOM-base OFS OBC forcing [used
!           for multiple boundary layers nesting FVCOM model (such as
!           NGOFS and INGOFS) to HYCOM or other outer model, 
!           such as NCOM, RTOFS]. This subroutine is called by program
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
      SUBROUTINE NOS_OFS_WRITE_NETCDF_OBC_FVCOM(GRIDFILE,NETCDF_FILE,
     & NCID,IMODE,TIME_LEN,NODE_LEN,NELE_LEN,SIGLAY_LEN,SIGLEV_LEN,
     & BASE_DATE,ITIME,ITIME2,TIMES,H,LAT,LON,LATC,LONC,NV,SIGLAY,
     & SIGLEV,ZETA,TEMP,SALINITY,U,V,UA,VA,PARTITION,GLOBALSTR,
     & HEOBC,SIGLAY_ELE)
      include 'netcdf.inc'

      CHARACTER*120 TEXT,CNAME,netcdf_file,GRIDFILE
      INTEGER LRR,base_date(4),intval(4),CORNER(4),COUNT(4)
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      character globalstr(9)*120
cc error status return
      integer  iret
cc netCDF id
      integer  ncid
cc dimension ids
      integer  time_dim
      integer  node_dim
      integer  siglev_dim
      integer  nele_dim
      integer  three_dim
      integer  siglay_dim
      integer  DateStrLen_dim
      save time_dim,node_dim,siglev_dim,nele_dim,three_dim
      save siglay_dim,DateStrLen_dim
cc dimension lengths
      integer  time_len
      integer  node_len
      integer  siglev_len
      integer  nele_len
      integer  three_len
      integer  siglay_len
      integer  DateStrLen_len
      parameter (DateStrLen_len = 26)
cc variable ids
      integer  Itime_id
      integer  Itime2_id
      integer  h_id
      integer  hyw_id
      integer  iint_id
      integer  lat_id
      integer  latc_id
      integer  lon_id
      integer  lonc_id
      integer  nprocs_id
      integer  nv_id
      integer  partition_id
      integer  salinity_id
      integer  siglay_id
      integer  siglev_id
      integer  temp_id
      integer  time_id
      integer  u_id
      integer  ua_id
      integer  v_id
      integer  va_id
      integer  x_id
      integer  xc_id
      integer  y_id
      integer  yc_id
      integer  zeta_id
      integer  Times_id
      integer  weight_cell_id
      integer  weight_node_id

cc added by zheng on 04/04/2018
      integer  he_id
      integer  siglayele_id
cc added by zheng on 04/04/2018

cc added by zheng on 03/22/2019 to save the netcdf variable id
      save Itime_id,Itime2_id,h_id,hyw_id,iint_id,lat_id,latc_id
      save lon_id,lonc_id,nprocs_id,nv_id,partition_id,salinity_id
      save siglay_id,siglev_id,temp_id,time_id,u_id,ua_id,v_id,va_id
      save x_id,xc_id,y_id,yc_id,zeta_id,Times_id,he_id
      save siglayele_id,weight_cell_id,weight_node_id
cc added by zheng on 03/22/2019 to save the netcdf variable id

cc data variables
      character*26 Times(time_len)
      integer  Itime(time_len)
      integer  Itime2(time_len)
      integer  IINT(time_len)
      real  time(time_len)
      real  h(node_len)
      real  hyw(node_len,siglev_len,time_len)

cc added by zheng on 04/04/2018
      real  heobc(nele_len)
      real  siglay_ele(nele_len, siglay_len)
cc added by zheng on 04/04/2018

      real  lat(node_len)
      real  lon(node_len)
      real  latc(nele_len)
      real  lonc(nele_len)
      integer  nprocs
      integer  nv(nele_len, 3)
      integer  partition(nele_len)
      real  siglay(node_len, siglay_len)
      real  siglev(node_len, siglev_len)
      real  x(node_len)
      real  y(node_len)
      real  xc(nele_len)
      real  yc(nele_len)
      real  zeta(node_len,time_len)
      real  temp(node_len, siglay_len,time_len)
      real  salinity(node_len, siglay_len,time_len)
      real  u(nele_len, siglay_len,time_len)
      real  v(nele_len, siglay_len,time_len)
      real  ua(nele_len,time_len)
      real  va(nele_len,time_len)
      real  weight_node(node_len,time_len)
      real  weight_cell(nele_len,time_len)

cc      write(*,*)'start writing OBC NetCDF'
      if(imode.eq.1) then           ! Write the file message 
        nprocs=256
        DO I=1,node_len
          x(I)=0.0
          y(I)=0.0
        ENDDO      
        DO I=1,nele_len
          xc(I)=0.0
          yc(I)=0.0
        ENDDO 

cc attribute vectors
cc enter define mode
        iret=nf_create(trim(netcdf_file),NF_CLOBBER,ncid)
        call check_err(iret)

cc define dimensions
        iret=nf_def_dim(ncid,'node',node_len,node_dim)
        call check_err(iret)
        iret=nf_def_dim(ncid,'nele',nele_len ,nele_dim)
        call check_err(iret)
        iret=nf_def_dim(ncid,'siglay',siglay_len ,siglay_dim)
        call check_err(iret)
        iret=nf_def_dim(ncid,'siglev',siglev_len ,siglev_dim)
        call check_err(iret)
        iret=nf_def_dim(ncid,'three',3,three_dim)
        call check_err(iret)
        iret=nf_def_dim(ncid,'DateStrLen',DateStrLen_len,
     &       DateStrLen_dim)
        call check_err(iret)
        iret=nf_def_dim(ncid,'time',NF_UNLIMITED,time_dim)
        call check_err(iret)

cc define variables
        intval(1)=time_dim
        iret=nf_def_var(ncid,'Itime',NF_INT,1,intval,Itime_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'Itime2',NF_INT,1,intval,Itime2_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'time',NF_REAL,1,intval,time_id)
        call check_err(iret)

        intval(2)=time_dim
        intval(1)=DateStrLen_dim
        iret=nf_def_var(ncid,'Times',NF_CHAR,2,intval,Times_id)
        call check_err(iret)
 
        iret=nf_def_var(ncid,'nprocs',NF_INT,0,0, nprocs_id)
        call check_err(iret)

        intval(1)=node_dim
        iret=nf_def_var(ncid,'h',NF_REAL,1,intval,h_id)
        call check_err(iret)

cc added by zheng on 04/04/2018
        intval(1)=nele_dim
        iret=nf_def_var(ncid,'h_center',NF_REAL,1,intval,he_id)
        call check_err(iret)
cc added by zheng on 04/04/2018

        intval(3)=time_dim
        intval(2)=siglev_dim
        intval(1)=node_dim
        iret=nf_def_var(ncid,'hyw',NF_REAL,3,intval,hyw_id)
        call check_err(iret)
      
        intval(1)=time_dim
        iret=nf_def_var(ncid,'iint',NF_INT,1,intval,iint_id)
        call check_err(iret)

        intval(1)=node_dim
        iret=nf_def_var(ncid,'lon',NF_REAL,1,intval,lon_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'lat',NF_REAL,1,intval,lat_id)
        call check_err(iret)

        intval(1)=nele_dim
        iret=nf_def_var(ncid,'lonc',NF_REAL,1,intval,lonc_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'latc',NF_REAL,1,intval,latc_id)
        call check_err(iret)

        intval(2)=three_dim
        intval(1)=nele_dim
        iret=nf_def_var(ncid,'nv',NF_INT,2,intval,nv_id)
        call check_err(iret)

        intval(1)=nele_dim
        iret=nf_def_var(ncid,'partition',NF_INT,1,intval,partition_id)
        call check_err(iret)

        intval(2)=siglay_dim
        intval(1)=node_dim
        iret=nf_def_var(ncid,'siglay',NF_REAL,2,intval,siglay_id)
        call check_err(iret)

        intval(2)=siglev_dim
        intval(1)=node_dim
        iret=nf_def_var(ncid,'siglev',NF_REAL,2,intval,siglev_id)
        call check_err(iret)

cc added by zheng on 04/04/2018
        intval(2)=siglay_dim
        intval(1)=nele_dim
        iret=nf_def_var(ncid,'siglay_center',NF_REAL,2,intval,
     &       siglayele_id)
        call check_err(iret)
cc added by zheng on 04/04/2018

        intval(3)=time_dim
        intval(2)=siglay_dim
        intval(1)=node_dim
        iret=nf_def_var(ncid,'temp',NF_REAL,3,intval,temp_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'salinity',NF_REAL,3,intval,salinity_id)
        call check_err(iret)

        intval(3)=time_dim
        intval(2)=siglay_dim
        intval(1)=nele_dim
        iret=nf_def_var(ncid,'u',NF_REAL,3,intval,u_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'v',NF_REAL,3,intval,v_id)
        call check_err(iret)

        intval(2)=time_dim
        intval(1)=nele_dim
        iret=nf_def_var(ncid,'ua',NF_REAL,2,intval,ua_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'va',NF_REAL,2,intval,va_id)
        call check_err(iret)

        intval(1)=node_dim
        iret=nf_def_var(ncid,'x',NF_REAL,1,intval,x_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'y',NF_REAL,1,intval,y_id)
        call check_err(iret)

        intval(1)=nele_dim
        iret=nf_def_var(ncid,'xc',NF_REAL,1,intval,xc_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'yc',NF_REAL,1,intval,yc_id)
        call check_err(iret)

        intval(2)=time_dim
        intval(1)=node_dim
        iret=nf_def_var(ncid,'zeta',NF_REAL,2,intval,zeta_id)
        call check_err(iret)

        intval(2)=time_dim
        intval(1)=nele_dim
        iret=nf_def_var(ncid,'weight_cell',NF_REAL,2,intval,
     &       weight_cell_id)
        call check_err(iret)

        intval(2)=time_dim
        intval(1)=node_dim
        iret=nf_def_var(ncid,'weight_node',NF_REAL,2,intval,
     &       weight_node_id)
        call check_err(iret)

cc assign attributes
        WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)') 'days since ',
     &    base_date(1),'-',base_date(2),'-',base_date(3),' ',
     &    base_date(4),':00:00'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='modified julian day(MJD)'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime_id,'format',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='UTC'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime_id,'time_zone',LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='mseconds from integer day'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime2_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='msec since 00:00:00'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime2_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='modified julian day(MJD)'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime2_id,'format',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='UTC'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime2_id,'time_zone',LRR,TRIM(TEXT))

        TEXT='Calendar Date'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Times_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='String: Calendar Time'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Times_id,'format',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='UTC'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Times_id,'time_zone',LRR,TRIM(TEXT))

        TEXT='Bathymetry'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,h_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='sea_floor_depth_below_geoid'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,h_id,'standard_name',LRR,TRIM(TEXT))
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

cc added by zheng on 04/04/2018
        TEXT='Bathymetry'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,he_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='sea_floor_depth_below_geoid'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,he_id,'standard_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='m'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,he_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='down'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,he_id,'positive',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='Bathymetry_Mesh'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,he_id,'grid',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='latc lonc'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,he_id,'coordinates',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='data'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,he_id,'type',LRR,TRIM(TEXT))
        call check_err(iret)
cc added by zheng on 04/04/2018

        TEXT='hydro static vertical velocity'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,hyw_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='m/s'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,hyw_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='fvcom_grid'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,hyw_id,'grid',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='lat lon'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,hyw_id,'coordinates',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='data'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,hyw_id,'type',LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='internal mode iteration number'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,iint_id,'long_name',LRR,TRIM(TEXT))
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
        iret=nf_put_att_text(ncid,lat_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='zonal latitude'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,latc_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='latitude'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,latc_id,'standard_name',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='degrees_north'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,latc_id,'units',LRR,TRIM(TEXT))
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

        TEXT='zonal longitude'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,lonc_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='longitude'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,lonc_id, 'standard_name',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='degrees_east'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid, lonc_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
  
        TEXT='number of processors'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,nprocs_id,'long_name',
     &       LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='nodes surrounding element'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,nv_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='partition'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,partition_id,'long_name',
     &       LRR,TRIM(TEXT))
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
        iret=nf_put_att_text(ncid,siglay_id,'positive',2,'up')
        call check_err(iret)
        iret=nf_put_att_text(ncid,siglay_id,'valid_min',2,'-1')
        call check_err(iret)
        iret=nf_put_att_text(ncid,siglay_id,'valid_max',1,'0')
        call check_err(iret)
        TEXT='sigma: siglay eta: zeta depth: h'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,siglay_id,'formula_terms',
     &       LRR,TRIM(TEXT))
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
        TEXT='sigma: siglay eta: zeta depth: h'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,siglev_id,'formula_terms',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
 
cc added by zheng on 04/04/2018
        TEXT='Sigma Layers'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,siglayele_id,'long_name',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='ocean_sigma/general_coordinate'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,siglayele_id,'standard_name',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        iret=nf_put_att_text(ncid,siglayele_id,'positive',2,'up')
        call check_err(iret)
        iret=nf_put_att_text(ncid,siglayele_id,'valid_min',2,'-1')
        call check_err(iret)
        iret=nf_put_att_text(ncid,siglayele_id,'valid_max',1,'0')
        call check_err(iret)
        TEXT='sigma: siglay eta: zeta depth: h'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,siglayele_id,'formula_terms',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
cc added by zheng on 04/04/2018

        TEXT='salinity'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,salinity_id,'long_name',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='sea_water_salinity'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,salinity_id,'standard_name',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='1e-3'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,salinity_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='fvcom_grid'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,salinity_id,'grid',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='lat lon'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,salinity_id,'coordinates',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='data'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,salinity_id,'type',LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='temperature'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,temp_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='sea_water_temperature'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,temp_id,'standard_name',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='degrees_C'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,temp_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='fvcom_grid'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,temp_id,'grid',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='lat lon'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,temp_id,'coordinates',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='data'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,temp_id,'type',LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='time'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,time_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='days since 1858-11-17 00:00:00'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,time_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='modified julian day (MJD)'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,time_id,'format',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='UTC'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,time_id,'time_zone',LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='Eastward Water Velocity'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,u_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='meters s-1'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,u_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='fvcom_grid'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,u_id,'grid',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='data'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,u_id,'type',LRR,TRIM(TEXT))
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

        TEXT='nodal x-coordinate'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,x_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='meters'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,x_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
 
        TEXT='zonal x-coordinate'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,xc_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='meters'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,xc_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='nodal y-coordinate'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,y_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='meters'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,y_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='zonal y-coordinate'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,yc_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='meters'
        LRR=LEN_TRIM(TEXT)
        iret = nf_put_att_text(ncid, yc_id, 'units',LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='zonal y-coordinate'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,zeta_id,'long_name',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='meters'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,zeta_id,'units',LRR,TRIM(TEXT))
        call check_err(iret)
        iret=nf_put_att_text(ncid,zeta_id,'positive',2,'up')
        call check_err(iret)
        TEXT='sea_surface_elevation'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,zeta_id,'standard_name', 
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='SSH_Mesh'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,zeta_id,'grid',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='lat lon'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid, zeta_id, 'coordinates',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='data'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,zeta_id,'type',LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='Nesting Weights at cells'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,weight_cell_id,'long_name',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='nondimensional'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,weight_cell_id,'units',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='fvcom_grid'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,weight_cell_id,'grid', 
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='data'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,weight_cell_id,'type',
     &       LRR,TRIM(TEXT))
        call check_err(iret)

        TEXT='Nesting Weights at nodes'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,weight_node_id,'long_name', 
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='nondimensional'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,weight_node_id,'units',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='fvcom_grid'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,weight_node_id,'grid',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT='data'
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,weight_node_id,'type',
     &       LRR,TRIM(TEXT))
        call check_err(iret)
 
cc Global Attributes
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
        iret=nf_put_att_text(ncid,NF_GLOBAL,'history',LRR,TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(9))
        LRR=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'reference',LRR,TRIM(TEXT))
        call check_err(iret)

cc leave define mode
        iret=nf_enddef(ncid)
        call check_err(iret)
        WRITE(*,*) 'end of define mode' 

cc write record variables
        iret=nf_put_vara_int(ncid,nprocs_id,1,1,nprocs)
        call check_err(iret)
        CORNER(1)=1 
        COUNT(1)=node_len
        iret=nf_put_vara_real(ncid,h_id,CORNER,COUNT,h)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,lat_id,CORNER,COUNT,lat)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,lon_id,CORNER,COUNT,lon)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,x_id,CORNER,COUNT,x)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,y_id,CORNER,COUNT,y)
        call check_err(iret)

        CORNER(1) = 1 
        COUNT(1)=nele_len
cc added by zheng on 04/04/2018
        iret=nf_put_vara_real(ncid,he_id,CORNER,COUNT,heOBC)
        call check_err(iret)
cc added by zheng on 04/04/2018
        iret=nf_put_vara_real(ncid,latc_id,CORNER,COUNT,latc)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,lonc_id,CORNER,COUNT,lonc)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,xc_id,CORNER,COUNT,xc)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,yc_id,CORNER,COUNT,yc)
        call check_err(iret)
        iret=nf_put_vara_int(ncid,partition_id,CORNER,COUNT,partition)
        call check_err(iret)

        CORNER(1) = 1
        CORNER(2) = 1
        COUNT(1)=nele_len
        COUNT(2)=3
        iret=nf_put_vara_int(ncid,nv_id,CORNER,COUNT,nv)
        call check_err(iret)

        CORNER(1) = 1
        CORNER(2) = 1
        COUNT(1)=node_len
        COUNT(2)=siglay_len
        iret=nf_put_vara_real(ncid,siglay_id,CORNER,COUNT,siglay)
        call check_err(iret)

cc added by zheng on 04/04/2018
        COUNT(1)=nele_len
        COUNT(2)=siglay_len
        iret=nf_put_vara_real(ncid,siglayele_id,CORNER,COUNT,
     &       siglay_ele)
        call check_err(iret)
cc added by zheng on 04/04/2018

        CORNER(1) = 1
        CORNER(2) = 1
        COUNT(1)=node_len
        COUNT(2)=siglev_len
        iret=nf_put_vara_real(ncid,siglev_id,CORNER,COUNT,siglev)
        call check_err(iret)

      elseif(imode.eq.2) then         ! Write the data into the file 
        iret=nf_inq_dimlen(ncid,time_dim,jtime)
        call check_err(iret)
        jtime=jtime+1
        DO I=1,node_len
          DO N=1,time_len
            weight_node(I,N)=1.0
            DO K=1,siglev_len
              hyw(I,K,N)=0.0
            ENDDO      
          ENDDO      
        ENDDO      

        DO I=1,nele_len
          DO N=1,time_len
            weight_cell(I,N)=1.0
          ENDDO      
        ENDDO 

        DO N=1,time_len
          iint(N)=jtime
          ITT=Itime2(N)/1000
          time(N)=Itime(N)+ITT/86400.0
        ENDDO	

        CORNER(1)=jtime 
        COUNT(1)=time_len
        iret=nf_put_vara_int(ncid,Itime_id,CORNER,COUNT,Itime)
        call check_err(iret)
        iret=nf_put_vara_int(ncid,Itime2_id,CORNER,COUNT,Itime2)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,time_id,CORNER,COUNT,time)
        call check_err(iret)
        iret=nf_put_vara_int(ncid,iint_id,CORNER,COUNT,iint)
        call check_err(iret)

        CORNER(1)=1
        CORNER(2)=jtime
        COUNT(1)=26
        COUNT(2)=1
        iret=nf_put_vara_text(ncid,Times_id,CORNER,COUNT,Times)
        call check_err(iret)

        CORNER(1)=1
        CORNER(2)=jtime
        COUNT(1)=node_len
        COUNT(2)=time_len
        iret=nf_put_vara_real(ncid,zeta_id,CORNER,COUNT,zeta)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,weight_node_id,CORNER,COUNT,
     &       weight_node)
        call check_err(iret)

        CORNER(1)=1
        CORNER(2)=jtime
        COUNT(1)=nele_len
        COUNT(2)=time_len
        iret=nf_put_vara_real(ncid,ua_id,CORNER,COUNT,ua)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,va_id,CORNER,COUNT,va)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,weight_cell_id,CORNER,COUNT,
     &       weight_cell)
        call check_err(iret)

        CORNER(1)=1
        CORNER(2)=1
        CORNER(3)=jtime
        COUNT(1)=node_len
        COUNT(2)=siglay_len
        COUNT(3)=time_len
        iret=nf_put_vara_real(ncid,salinity_id,CORNER,COUNT,salinity)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,temp_id,CORNER,COUNT,temp)
        call check_err(iret)

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

        CORNER(1)=1
        CORNER(2)=1
        CORNER(3)=jtime
        COUNT(1)=node_len
        COUNT(2)=siglev_len
        COUNT(3)=time_len
        iret=nf_put_vara_real(ncid,hyw_id,CORNER,COUNT,hyw)
        call check_err(iret)

      elseif(imode.eq.3) then             ! Close the netCDF  
        iret=nf_close(ncid)
        call check_err(iret)
      endif

      RETURN
      end
       
