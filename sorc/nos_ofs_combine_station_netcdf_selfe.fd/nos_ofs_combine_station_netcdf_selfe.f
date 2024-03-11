!===============================================================================
! Read in binary outputs (rank-specific) from parallel code and combine them into
! one global output in v5.0 format or netcdf format. Works for partial outputs.
! Gobal-local mappings are read in from separate files.
! Run this program inside the directory outputs/, where some of the input files below
! can be found.

! Inputs:
!        rank-specific binary files (from SELFE outputs); 
!        local_to_global_* (from SELFE outputs);
!        combine_output.in (1st line: elev.61 etc; 2nd line: start and end file indices;
!                          3rd line: inc (1 for netcdf option)); 
! Output: combined binary file (for nc file: e.g. *_salt.nc).
!
!  Compile on canopus01:
!  ifort -Bstatic -O3 -assume byterecl -o combine_output5_canopus combine_output5.f90 -Vaxlib -I/usr/local/include/ -L/usr/local/lib -lnetcdf

!  Compile on amb6402:
!  ifort -Bstatic -O3 -assume byterecl -o combine_output5_canopus combine_output5.f90 -Vaxlib -I/usr/local/netcdf/include/ -L/usr/local/netcdf/lib -lnetcdf
!
!  On Ranger:
!  pgf90 -O2 -mcmodel=medium  -Bstatic -o combine_output5_canopus combine_output5.f90 .....

!  History: (1) added netcdf option.
!===============================================================================


program sta_ncd 

!-------------------------------------------------------------------------------
!  use typebuffer
!  use netcdf

  implicit real(4)(a-h,o-z),integer(i-n)
  include 'netcdf.inc'
  character*30 file63
  character*12 it_char
  character*12 it_charb,it_chare
  character*48 start_time,version,variable_nm,variable_dim
  character*48 data_format
  character(72) :: fgb,fgb2,fdb  ! Processor specific global output file name
  integer :: lfgb,lfdb       ! Length of processor specific global output file name
  character(len= 4) :: a_4
  integer :: i, argc, iargc
  character(len=80) :: arg
  character*100 PREFIXNOS
! character*20 stname2
! character, allocatable :: stname1(:,:), stname2(:,:)
  character*4, allocatable :: stname1(:)
  character*20, allocatable :: stname2(:)
  character*20 stname2tmp
  character, allocatable :: stname2d(:,:)

  allocatable ne(:),np(:),nsproc(:),ihot_len(:)
  allocatable ztot(:),sigma(:),cs(:),outb(:,:,:),eta2(:)
  allocatable i34(:),nm(:,:),nm2(:,:),js(:,:),xctr(:),yctr(:),dpe(:)
  allocatable x(:),y(:),dp(:),kbp00(:),iplg(:,:),ielg(:,:),kbp01(:,:)
  allocatable xcj(:),ycj(:),dps(:)
  allocatable xlon(:),ylat(:),dpinstr(:),xspcs(:),yspcs(:)

! Station variables
  allocatable :: sta_out_gb(:,:),sta_out3d(:,:,:),zta_out3d(:,:,:),sta_out3d_gb(:,:,:),zta_out3d_gb(:,:,:)
  allocatable :: time(:), zeta_2d(:,:), uwind(:,:), vwind(:,:)
  allocatable :: S_3d(:,:,:), T_3d(:,:,:), u_3d(:,:,:), v_3d(:,:,:)
  allocatable :: S_3d_int(:,:), T_3d_int(:,:), u_3d_int(:,:), v_3d_int(:,:)
  allocatable :: offset(:), zeta_2da(:,:)
  allocatable :: tmp1(:,:,:)
  !netcdf variables
  character(len=50) fname
  integer :: time_dims(1),ele_dims(2),x_dims(1),sigma_dims(1),var2d_dims(2),var3d_dims(3), &
            &data_start_2d(2),data_start_3d(3),data_count_2d(2),data_count_3d(3),z_dims(1)
  integer :: nsta_dims(1),nstname_dims(2),nstname_dims1(1)
! AJ
  character(len=100) gridfile,parafile,base_date,buffer,sta_ctlfile
  integer :: iyr,imon,idd,ihh,clen
  CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20,TEXT*100
  CHARACTER globalstr(9)*120
  INTEGER DATE_TIME(8)
  INTEGER DAYS_PER_MONTH(12)
  DATA (DAYS_PER_MONTH(i),I=1,12) /31,28,31,30,31,30,31,31,30,31,30,31/ 
!AJ 
  argc=iargc()
!  call getarg(1, arg)
!  read(arg,'(i)') nout_sta
!  call getarg(2, arg)
!  read(arg,'(i)') nvar_sta
!  call getarg(3, arg)
!  read(arg,'(i)') nvrt
!  call getarg(4, arg)
!  read(arg,'(i)') ntime
!  call getarg(5, arg)
!  read(arg,'(a)') start_time
! read in from control file
      read(5,*) nout_sta 
      read(5,*) nvar_sta 
      read(5,*) nvrt 
      read(5,*) ntime 
      read(5,'(a100)')sta_ctlfile 
      read(5,'(i4,3I2)')iyr,imon,idd,ihh 
      read(5,'(a100)')PREFIXNOS
      WRITE(start_time,'(I4.4,a1,I2.2,a1,I2.2,a1,I2.2,a10)')iyr,'-',imon,'-',idd,' ',ihh,':00:00 UTC'
      WRITE(base_date,'(I4.4,a1,I2.2,a1,I2.2,a1,I2.2,a10)')iyr,'-',imon,'-',idd,' ',ihh,':00:00 UTC'
! Station info
! nout_sta=79
! nvar_sta=9
! nvrt=54
! ntime=480
! start_time="04/01/2007 00:00:00 UTC"

! {zy-
!  allocate(sta_out_gb(nout_sta,nvar_sta), &
!     &sta_out3d(nvrt,nout_sta,nvar_sta),sta_out3d_gb(nvrt,nout_sta,nvar_sta), &
!     &zta_out3d(nvrt,nout_sta,nvar_sta),zta_out3d_gb(nvrt,nout_sta,nvar_sta),stat=istat)

!  allocate(time(ntime), zeta_2d(nout_sta,ntime), uwind(nout_sta,ntime), vwind(nout_sta,ntime))
!  allocate(S_3d(nout_sta,nvrt,ntime), T_3d(nout_sta,nvrt,ntime), u_3d(nout_sta,nvrt,ntime), v_3d(nout_sta,nvrt,ntime))
!  allocate(S_3d_int(nout_sta,ntime), T_3d_int(nout_sta,ntime), u_3d_int(nout_sta,ntime), v_3d_int(nout_sta,ntime))
!  allocate(zta_out3d_gb(nout_sta,nvrt,ntime))

   allocate(time(ntime), zeta_2d(nout_sta,ntime), uwind(nout_sta,ntime), vwind(nout_sta,ntime))
   allocate(S_3d(nvrt,nout_sta,ntime), T_3d(nvrt,nout_sta,ntime), u_3d(nvrt,nout_sta,ntime), v_3d(nvrt,nout_sta,ntime))
   allocate(S_3d_int(nout_sta,ntime), T_3d_int(nout_sta,ntime), u_3d_int(nout_sta,ntime), v_3d_int(nout_sta,ntime))
   allocate(zta_out3d_gb(nvrt,nout_sta,ntime))
   allocate(xlon(nout_sta),ylat(nout_sta),xspcs(nout_sta),yspcs(nout_sta),dpinstr(nout_sta))
!  allocate(stname1(nout_sta,4),stname2(nout_sta,20))
   allocate(stname1(nout_sta),stname2(nout_sta))
   allocate(stname2d(20,nout_sta))

   allocate(tmp1(nout_sta,nvrt,ntime))
   allocate(zeta_2da(nout_sta,ntime))
   allocate(offset(nout_sta))

  open(500,file=trim(sta_ctlfile),status='old')   ! eta
  do i=1,nout_sta
!AJ    read(500,504) itmp,stname1(i),stname2tmp,xlon(i),ylat(i),xspcs(i),yspcs(i),dpinstr(i)
    read(500,*) itmp,stname1(i),stname2tmp,xlon(i),ylat(i),xspcs(i),yspcs(i),dpinstr(i)
    stname2(i)=stname2tmp
    write(*,504) itmp,stname1(i),stname2(i),xlon(i),ylat(i),xspcs(i),yspcs(i),dpinstr(i)
    stname2d(1:20,i)=stname2tmp(1:20)
!   write(*,505) stname2d(1:20,i)
    do j=1,20
      stname2d(j:j,i)=stname2tmp(j:j)
      write(*,505) stname2d(j:j,i)
    end do
  end do
504 format(i3,1x,a4,15x,a20,9x,f9.4,11x,f8.5,10x,f13.6,1x,f14.6,1x,f5.2)
505 format(a1)
  close(500)

  open(500,file='../'//trim(adjustL(PREFIXNOS))//'.stations.wl_correction.dat',status='old')   
  do i=1,nout_sta
    read(500,*) itmp,buffer,buffer,xlontmp,ylattmp,xtmp,ytmp,offset(i)
  enddo
  close(500)

! ? LOOP for time here?
      
  i=1
  open(501,file='staout_1',status='old')   ! eta
  open(503,file='staout_3',status='old')   ! eta
  open(504,file='staout_4',status='old')   ! eta
  open(505,file='staout_5',status='old')    ! tnd
  open(506,file='staout_6',status='old')    ! snd
  open(507,file='staout_7',status='old')    ! uu
  open(508,file='staout_8',status='old')    ! vv
  do i=1,ntime
    read(501,'(e14.6,6000(1x,e14.6))')time(i),zeta_2d(:,i)
    read(503,'(e14.6,6000(1x,e14.6))')time(i),uwind(:,i)
    read(504,'(e14.6,6000(1x,e14.6))')time(i),vwind(:,i)
    read(505,'(e14.6,6000(1x,e14.6))')time(i),T_3d_int(:,i)
    read(505,'(e14.6,10000(1x,e14.6))')time(i),T_3d(:,:,i),zta_out3d_gb(:,:,i)
    read(506,'(e14.6,6000(1x,e14.6))')time(i),S_3d_int(:,i)
    read(506,'(e14.6,10000(1x,e14.6))')time(i),S_3d(:,:,i),zta_out3d_gb(:,:,i)
    read(507,'(e14.6,6000(1x,e14.6))')time(i),u_3d_int(:,i)
    read(507,'(e14.6,10000(1x,e14.6))')time(i),u_3d(:,:,i),zta_out3d_gb(:,:,i)
    read(508,'(e14.6,6000(1x,e14.6))')time(i),v_3d_int(:,i)
    read(508,'(e14.6,10000(1x,e14.6))')time(i),v_3d(:,:,i),zta_out3d_gb(:,:,i)
    print *, 'i is ',i,' -- Time is ',time(i)
  end do
  close(501)
  close(505)
  close(506)
  close(507)
  close(508)
!  apply water level offset correction 
  do i=1,ntime
  do i2=1,nout_sta
   zeta_2da(i2,I)=zeta_2d(i2,i)-offset(i2)
  end do
  end do

!-----------------------------------------------------------------------
!  Set global attributes string of the NetCDF
!-----------------------------------------------------------------------
 1    format(I2.2,a1,I2.2,2x,I2.2,a1,i2.2,a1,I4)
      CALL DATE_AND_TIME(BIG_BEN(1),BIG_BEN(2),BIG_BEN(3),DATE_TIME)
      WRITE(CURRENT_TIME,1)DATE_TIME(5),':',DATE_TIME(6),DATE_TIME(2),'/',DATE_TIME(3),'/',DATE_TIME(1)
      globalstr(1)= 'Columbia River & Estuarine Operational Forecast System'
      globalstr(2)= 'Oregon Health & Science University'
      globalstr(3)= 'SELFE-MPI Version 3.1dc'
      globalstr(4)= 'http://www.ccalmr.ogi.edu/CORIE/modeling/selfe'
      globalstr(5)= 'Station/Point NetCDF format - CF-1.0'
      globalstr(6)= 'Hybrid SZ vertical coordinates, K=1 for bottom'
      globalstr(7)= 'Unstructured model grid: '
      globalstr(8)= 'Created at Eastern Local Time '//trim(CURRENT_TIME)
      globalstr(9)='Created by CO-OPS/NOS/NOAA'
 !-----------------------------------------------------------------------
! create NetCDF file
   iret = nf_create('Station.nc', NF_CLOBBER, ncid)

     ! def dim 
      iret = nf_def_dim(ncid, 'nvrt',nvrt, nz_dim)
      iret = nf_def_dim(ncid, 'station',nout_sta, nsta_dim)
      iret = nf_def_dim(ncid, 'nvar',nvar_sta, nvar_dim)
    
      iret = nf_def_dim(ncid, 'time', NF_UNLIMITED, ntime_dim)
!    iret = nf_def_dim(ncid, 'time', 1, ntime_dim)

      iret = nf_def_dim(ncid, 'clen', 20, nstanm_dim)

      nstname_dims(2)=nsta_dim
      nstname_dims(1)=nstanm_dim
      iret=nf_def_var(ncid,'name_station',NF_CHAR,2,nstname_dims,istat_id)
      iret=nf_put_att_text(ncid,istat_id,'long_name',12,'Station Name')

      nsta_dims(1)=nsta_dim
      iret=nf_def_var(ncid,'x',NF_REAL,1,nsta_dims,ixspcs_id)
      iret=nf_put_att_text(ncid,ixspcs_id,'long_name',20,'station x-coordinate')
      iret=nf_put_att_text(ncid,ixspcs_id,'units',6,'meters')

      iret=nf_def_var(ncid,'y',NF_REAL,1,nsta_dims,iyspcs_id)
      iret=nf_put_att_text(ncid,iyspcs_id,'long_name',20,'station y-coordinate')
      iret=nf_put_att_text(ncid,iyspcs_id,'units',6,'meters')

      iret=nf_def_var(ncid,'lon',NF_REAL,1,nsta_dims,ixlon_id)
      iret=nf_put_att_text(ncid,ixlon_id,'long_name',9,'Longitude')
      iret=nf_put_att_text(ncid,ixlon_id,'standard_name',9,'longitude')
      iret=nf_put_att_text(ncid,ixlon_id,'units',12,'degrees_east')

      iret=nf_def_var(ncid,'lat',NF_REAL,1,nsta_dims,iylat_id)
      iret=nf_put_att_text(ncid,iylat_id,'long_name',8,'Latitude')
      iret=nf_put_att_text(ncid,iylat_id,'standard_name',8,'latitude')
      iret=nf_put_att_text(ncid,iylat_id,'units',13,'degrees_north')
      iret=nf_put_att_text(ncid,iylat_id,'grid',15,'Bathymetry_Mesh')

      iret=nf_def_var(ncid,'h',NF_REAL,1,nsta_dims,ih_id)
      iret=nf_put_att_text(ncid,ih_id,'long_name',10,'Bathymetry')
      iret=nf_put_att_text(ncid,ih_id,'standard_name',11,'Water Depth')
      iret=nf_put_att_text(ncid,ih_id,'units',6,'meters')
      iret=nf_put_att_text(ncid,ih_id,'positive',4,'down')

      iret=nf_def_var(ncid,'offset',NF_REAL,1,nsta_dims,ioffset_id)
      iret=nf_put_att_text(ncid,ioffset_id,'long_name',18,'water level offset')
      iret=nf_put_att_text(ncid,ioffset_id,'standard_name',47,'mean difference of model minus obs. over 7 days')
      iret=nf_put_att_text(ncid,ioffset_id,'units',6,'meters')
      iret=nf_put_att_text(ncid,ioffset_id,'positive',2,'up')
   ! def var
       time_dims(1) = ntime_dim
        iret=nf_def_var(ncid,'time',NF_REAL,1,time_dims,itime_id)
        iret=nf_put_att_text(ncid,itime_id,'long_name',4,'Time')
      base_date=trim(adjustL(base_date))
      buffer='seconds since '//trim(base_date)
      clen=len_trim(buffer)
      iret=nf_put_att_text(ncid,itime_id,'units',clen,trim(buffer))
      iret=nf_put_att_text(ncid,itime_id,'base_date',len_trim(base_date),trim(base_date))
      iret=nf_put_att_text(ncid,itime_id,'standard_name',4,'time')
  
        var2d_dims(1)=nsta_dim; var2d_dims(2)=ntime_dim
        iret=nf_def_var(ncid,'zeta',NF_REAL,2,var2d_dims,ielev_id)
        iret=nf_put_att_text(ncid,ielev_id,'long_name',23, 'Water Surface Elevation')
        iret=nf_put_att_text(ncid,ielev_id,'units',6,'meters')
        iret=nf_put_att_text(ncid,ielev_id,'positive',2,'up')
!        iret=nf_put_att_text(ncid,ielev_id,'standard_name',21,'sea_surface_elevation')
        iret=nf_put_att_text(ncid,ielev_id,'standard_name',47,'Original modeled sea_surface_height_above_geoid')
        iret=nf_put_att_text(ncid,ielev_id,'type',4,'data')
        iret=nf_put_att_text(ncid,ielev_id,'coordinates',12,'time lat lon')
        iret=nf_put_att_text(ncid,ielev_id,'location',4,'node')
        iret=nf_put_att_real(ncid,ielev_id,'missing_value',NF_REAL,1,-9999.)

        var2d_dims(1)=nsta_dim; var2d_dims(2)=ntime_dim
        iret=nf_def_var(ncid,'zeta_adj',NF_REAL,2,var2d_dims,ieleva_id)
        iret=nf_put_att_text(ncid,ieleva_id,'long_name',23, 'Adjusted Water Surface Elevation')
        iret=nf_put_att_text(ncid,ieleva_id,'units',6,'meters')
        iret=nf_put_att_text(ncid,ieleva_id,'positive',2,'up')
        iret=nf_put_att_text(ncid,ieleva_id,'standard_name',47,'offset corrected sea_surface_height_above_geoid')
        iret=nf_put_att_text(ncid,ieleva_id,'type',4,'data')
        iret=nf_put_att_text(ncid,ieleva_id,'coordinates',12,'time lat lon')
        iret=nf_put_att_text(ncid,ieleva_id,'location',4,'node')
        iret=nf_put_att_real(ncid,ieleva_id,'missing_value',NF_REAL,1,-9999.)
        
        iret=nf_def_var(ncid,'uwind_speed',NF_REAL,2,var2d_dims,iwindu_id)
        iret=nf_put_att_text(ncid,iwindu_id,'long_name',21, 'Eastward wind velocity')
        iret=nf_put_att_text(ncid,iwindu_id,'units',5,'(m/s)')
        iret=nf_put_att_text(ncid,iwindu_id,'standard_name',13,'wind velocity')
        iret=nf_put_att_text(ncid,iwindu_id,'type',4,'data')
 !       iret=nf_put_att_text(ncid,iwindu_id,'missing_value',6,'-9999.')
        iret=nf_put_att_text(ncid,iwindu_id,'coordinates',12,'time lat lon')             
        iret=nf_put_att_text(ncid,iwindu_id,'location',4,'node')
        iret=nf_put_att_real(ncid,iwindu_id,'missing_value',NF_REAL,1,-9999.)

        iret=nf_def_var(ncid,'vwind_speed',NF_REAL,2,var2d_dims,iwindv_id)
        iret=nf_put_att_text(ncid,iwindv_id,'long_name',22, 'Northward wind velocity')
        iret=nf_put_att_text(ncid,iwindv_id,'units',5,'(m/s)')
        iret=nf_put_att_text(ncid,iwindv_id,'standard_name',13,'wind velocity')
        iret=nf_put_att_text(ncid,iwindv_id,'type',4,'data')
!        iret=nf_put_att_text(ncid,iwindv_id,'missing_value',6,'-9999.')
        iret=nf_put_att_text(ncid,iwindv_id,'coordinates',12,'time lat lon')             
        iret=nf_put_att_text(ncid,iwindv_id,'location',4,'node')
        iret=nf_put_att_real(ncid,iwindv_id,'missing_value',NF_REAL,1,-9999.)

        var3d_dims(1)=nsta_dim; var3d_dims(2)=nz_dim; var3d_dims(3)=ntime_dim
 !    var3d_dims(1)=nz_dim; var3d_dims(2)=nsta_dim; var3d_dims(3)=ntime_dim
        iret=nf_def_var(ncid,'u',NF_REAL,3,var3d_dims,iu_id)
        iret=nf_put_att_text(ncid,iu_id,'long_name',23,'Eastward Water Velocity')
        iret=nf_put_att_text(ncid,iu_id,'units',10,'meters s-1')
        iret=nf_put_att_text(ncid,iu_id,'type',4,'data')
!        iret=nf_put_att_text(ncid,iu_id,'missing_value',6,'-9999.')
        iret=nf_put_att_text(ncid,iu_id,'coordinates',15,'time nv lat lon')	      
        iret=nf_put_att_text(ncid,iu_id,'location',4,'node')
        iret=nf_put_att_real(ncid,iu_id,'missing_value',NF_REAL,1,-9999.)

        iret=nf_def_var(ncid,'v',NF_REAL,3,var3d_dims,iv_id)
        iret=nf_put_att_text(ncid,iv_id,'long_name',24,'Northward Water Velocity')
        iret=nf_put_att_text(ncid,iv_id,'units',10,'meters s-1')
        iret=nf_put_att_text(ncid,iv_id,'type',4,'data')
!        iret=nf_put_att_text(ncid,iv_id,'missing_value',6,'-9999.')
        iret=nf_put_att_text(ncid,iv_id,'coordinates',15,'time nv lat lon')	      
        iret=nf_put_att_text(ncid,iv_id,'location',4,'node')
        iret=nf_put_att_real(ncid,iv_id,'missing_value',NF_REAL,1,-9999.)
                
        iret=nf_def_var(ncid,'salinity',NF_REAL,3,var3d_dims,isalt_id)
        iret=nf_put_att_text(ncid,isalt_id,'long_name',8,'salinity')
        iret=nf_put_att_text(ncid,isalt_id,'standard_name',18,'sea_water_salinity')
        iret=nf_put_att_text(ncid,isalt_id,'units',4,'1e-3')
        iret=nf_put_att_text(ncid,isalt_id,'type',4,'data')
!        iret=nf_put_att_text(ncid,isalt_id,'missing_value',6,'-9999.')
        iret=nf_put_att_text(ncid,isalt_id,'coordinates',15,'time nv lat lon')	      
        iret=nf_put_att_text(ncid,isalt_id,'location',4,'node')
	iret=nf_put_att_real(ncid,isalt_id,'missing_value',NF_REAL,1,-9999.)
                
        iret=nf_def_var(ncid,'temp',NF_REAL,3,var3d_dims,itemp_id)
        iret=nf_put_att_text(ncid,itemp_id,'long_name',11,'temperature')
        iret=nf_put_att_text(ncid,itemp_id,'standard_name',21,'sea_water_temperature')
        iret=nf_put_att_text(ncid,itemp_id,'units',9,'degrees_C')
        iret=nf_put_att_text(ncid,itemp_id,'type',4,'data')
!        iret=nf_put_att_text(ncid,itemp_id,'missing_value',6,'-9999.')
        iret=nf_put_att_text(ncid,itemp_id,'coordinates',15,'time nv lat lon')	  
        iret=nf_put_att_text(ncid,itemp_id,'location',4,'node')
	iret=nf_put_att_real(ncid,itemp_id,'missing_value',NF_REAL,1,-9999.)
                
        iret=nf_def_var(ncid,'zval',NF_REAL,3,var3d_dims,izval_id)
        iret=nf_put_att_text(ncid,izval_id,'long_name',29, 'height of vertical grid level')
        iret=nf_put_att_text(ncid,izval_id,'units',6,'meters')
        iret=nf_put_att_text(ncid,izval_id,'coordinates',15,'time nv lat lon')    
        iret=nf_put_att_text(ncid,izval_id,'location',4,'node')
	iret=nf_put_att_real(ncid,izval_id,'missing_value',NF_REAL,1,-9999.)
!        iret=nf_put_att_text(ncid,izval_id,'missing_value',6,'-9999.')
      TEXT=trim(globalstr(1))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid,NF_GLOBAL ,'title',LEN,TRIM(TEXT))
      TEXT=trim(globalstr(2))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'model_developer',LEN,TRIM(TEXT))
      TEXT=trim(globalstr(3))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'source',LEN,TRIM(TEXT))
      TEXT=trim(globalstr(4))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'model_link',LEN,TRIM(TEXT))

      TEXT=trim(globalstr(5))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'type',LEN,TRIM(TEXT))

      TEXT=trim(globalstr(6))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'Vertical_type', LEN,TRIM(TEXT))
      TEXT=trim(globalstr(7))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'grid_type',LEN,TRIM(TEXT))
     
      TEXT=trim(globalstr(8))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'history',LEN,TRIM(TEXT))
!      TEXT='Created by Aijun Zhang, OD/CO-OPS/NOS/NOAA'
      TEXT=trim(globalstr(9))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'reference', LEN,TRIM(TEXT))
     iret=nf_enddef(ncid)
      ! put var ----------------
        N_timestep = ntime
        print*, 'N_timestep = ', N_timestep
!       print*, 'time =', time      

        data_start_2d(1)=1
        data_start_2d(2)=1
        data_count_2d(1)=20
        data_count_2d(2)=nout_sta
        iret=nf_put_vara_text(ncid,istat_id,data_start_2d,data_count_2d,stname2d)
!       iret=nf_put_vara_text(ncid,istat_id,1,nout_sta,stname2)

        iret=nf_put_vara_real(ncid,ixlon_id,1,nout_sta,xlon)   ! here: time(i)
        iret=nf_put_vara_real(ncid,iylat_id,1,nout_sta,ylat)   ! here: time(i)
        iret=nf_put_vara_real(ncid,ixspcs_id,1,nout_sta,xspcs)   ! here: time(i)
        iret=nf_put_vara_real(ncid,iyspcs_id,1,nout_sta,yspcs)   ! here: time(i)
 
        iret=nf_put_vara_real(ncid,ih_id,1,nout_sta,dpinstr)   ! here: time(i)
        ! time variable as an array
        iret=nf_put_vara_real(ncid,itime_id,1,N_timestep,time)   ! here: time(i)

        iiret=nf_put_vara_real(ncid,ioffset_id,1,nout_sta,offset)

        ! 2-D variables 
          data_start_2d(1)=1; data_start_2d(2)=1;
          data_count_2d(1)=nout_sta; data_count_2d(2)=N_timestep
  
            ! zeta_2d 
            iret=nf_put_vara_real(ncid,ielev_id,data_start_2d,data_count_2d,zeta_2d)

            ! zeta_2d 
            iret=nf_put_vara_real(ncid,ieleva_id,data_start_2d,data_count_2d,zeta_2da)

            ! uwind_2d, vwind_2d
            iret=nf_put_vara_real(ncid,iwindu_id,data_start_2d,data_count_2d,uwind)
            iret=nf_put_vara_real(ncid,iwindv_id,data_start_2d,data_count_2d,vwind)
       
        ! 3-D variables
          data_start_3d(1:2)=1; data_start_3d(3)=1
          data_count_3d(1)=nout_sta; data_count_3d(2)=nvrt; data_count_3d(3)=N_timestep
 !         data_count_3d(1)=nvrt; data_count_3d(2)=nout_sta; data_count_3d(3)=N_timestep

            Do IZ=1,nout_sta
	    DO JZ=1,nvrt
	    DO NZ=1,ntime
	      tmp1(IZ,JZ,NZ)=S_3d(JZ,IZ,NZ)
	    ENDDO
	    ENDDO
	    ENDDO
	      
            ! S, T
            iret=nf_put_vara_real(ncid,isalt_id,data_start_3d,data_count_3d,tmp1)
            Do IZ=1,nout_sta
	    DO JZ=1,nvrt
	    DO NZ=1,ntime
	      tmp1(IZ,JZ,NZ)=T_3d(JZ,IZ,NZ)
	    ENDDO
	    ENDDO
	    ENDDO

            iret=nf_put_vara_real(ncid,itemp_id,data_start_3d,data_count_3d,tmp1)
  
            ! u, vpwd
            Do IZ=1,nout_sta
	    DO JZ=1,nvrt
	    DO NZ=1,ntime
	      tmp1(IZ,JZ,NZ)=u_3d(JZ,IZ,NZ)
	    ENDDO
	    ENDDO
	    ENDDO
            iret=nf_put_vara_real(ncid,iu_id,data_start_3d,data_count_3d,tmp1)
            Do IZ=1,nout_sta
	    DO JZ=1,nvrt
	    DO NZ=1,ntime
	      tmp1(IZ,JZ,NZ)=v_3d(JZ,IZ,NZ)
	    ENDDO
	    ENDDO
	    ENDDO
            iret=nf_put_vara_real(ncid,iv_id,data_start_3d,data_count_3d,tmp1)
  
            ! Zval
            Do IZ=1,nout_sta
	    DO JZ=1,nvrt
	    DO NZ=1,ntime
	      tmp1(IZ,JZ,NZ)=zta_out3d_gb(JZ,IZ,NZ)
	    ENDDO
	    ENDDO
	    ENDDO
            iret=nf_put_vara_real(ncid,izval_id,data_start_3d,data_count_3d,tmp1)

     iret = nf_close(ncid)
 
     stop

end program sta_ncd
 
