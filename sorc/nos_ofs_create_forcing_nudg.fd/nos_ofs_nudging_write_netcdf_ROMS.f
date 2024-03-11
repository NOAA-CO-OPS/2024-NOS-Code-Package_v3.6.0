!       Subroutine Name: nos_ofs_nudging_write_netcdf_ROMS.f 
!
!       Technical Contact(s):   Name:  Aijun Zhang
!                               Org:   NOS/CO-OPS/OD
!                               Phone: 240-533-0591
!                               E-Mail: aijun.zhang@noaa.gov
!
!       Abstract:  this subroutine is used to write averaged climatologic 
!       variables into NetCDF format for ROMS-based OFS
!
!       History Log:
!           03/28/2019
!
!       Usage: call write_netCDF_nudging_fields_ROMS from
!               nos_ofs_create_forcing_nudg.f 
!
!       Argument Input:
!                netcdf_file - the output NetCDF file
!                ncid       - NetCDF file identity integer
!                imode      - 1, 2, or 3 depending on netcdf_file status
!                IM         - number of grid point in x direction                 
!                JM         - number of grid point in y direction
!                KM         -  Number of vertical levels for temperature and salinity of the OFS
!                NTm        -  Number of time record
!               base_date   - Base date for the NetCDF file
!               ocean_time  - time in days (real) since reference time
!               temp            - potential temperature
!               salt            - salinity
!                globalstr       - global attributes
!       
!       Argument Output:
!               netcdf_file - the output NetCDF file with values in targeted
!               variables
!
!               

!  lf95 write_surface_forcing.f -I/usr/local/include -L/usr/local/lib -lnetcdf -o write_surface_forcing.x
      subroutine write_netCDF_nudging_fields_ROMS(netcdf_file,ncid,
     & imode,IM,JM,KM,NT,base_date,ocean_time,temp,salt,globalstr)
CC     x_rho = IM in x-direction/longitude
CC     y_rho = JM in y-dirrection/latitude
      include 'netcdf.inc'
      CHARACTER*120 TEXT,netcdf_file
      INTEGER LEN,base_date(4),intval(4),CORNER(4),COUNT(4)
!! 
C Netcdf dimension ID
      integer xi_rho_dim,eta_rho_dim,s_rho_dim
      integer time_dim
!! variable ID
      integer time_id,temp_id,salt_id
      character globalstr(7)*120
      real temp(IM,JM,KM,NT),salt(IM,JM,KM,NT)
      real ocean_time(NT)
      integer status
C save all dimention id and variable id for next call
      save eta_rho_dim,xi_rho_dim,s_rho_dim
      save time_dim
!! variable ID
      save time_id,temp_id,salt_id
      if (imode .eq. 1)then 
C Set optional variable flags

      status = nf_create(trim(netcdf_file), NF_CLOBBER, ncid)
      if (status .ne. NF_NOERR)stop 'netcdf'
      write(6,*)'open netcdf file: ',trim(netcdf_file)
c define dimensions
      status = nf_def_dim(ncid, 'xi_rho', IM, xi_rho_dim)
      if (status .ne. NF_NOERR)stop 'netcdf'
      write(6,*)'define xi_rho dimensions: '
      status = nf_def_dim(ncid, 'eta_rho', JM, eta_rho_dim)
      if (status .ne. NF_NOERR)stop 'netcdf'
      write(6,*)'define eta_rho dimensions: '
      status = nf_def_dim(ncid, 's_rho', KM, s_rho_dim)
      if (status .ne. NF_NOERR)stop 'netcdf'
      write(6,*)'define s_rho dimensions: '
      status = nf_def_dim(ncid,'time',NF_UNLIMITED,time_dim)
      call check_err(status)
      if (status .ne. NF_NOERR)stop 'netcdf'
      write(6,*)'define time dimensions: '

c         define variables
C define frc_time 
        status = nf_def_var(ncid,'ocean_time', NF_REAL,1, 
     &       time_dim, time_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, time_id,'long_name', 
     &       LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3),' ',base_date(4),':00:00'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, time_id,'units', 
     &       LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
!! temp
      intval(1) = xi_rho_dim
      intval(2) = eta_rho_dim
      intval(3) = s_rho_dim
      intval(4) = time_dim
        status = nf_def_var(ncid, 'temp', NF_REAL, 4,intval,temp_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='potential temperature'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, temp_id,'long_name', 
     &       LEN,TRIM(TEXT))
        TEXT='Celsius'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, temp_id,'units', 
     &       LEN,TRIM(TEXT))
        TEXT='ocean_time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, temp_id,'time', 
     &       LEN,TRIM(TEXT))
        TEXT='temperature, scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, temp_id,'field',
     &       LEN,TRIM(TEXT))
!        status = nf_put_att_real(ncid, temp_id, 
!     &  '_FillValue', NF_REAL, 1,-99999.0)

        status = nf_def_var(ncid, 'salt', NF_REAL, 4,intval,salt_id)
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='salinity'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, salt_id,'long_name', 
     &       LEN,TRIM(TEXT))
        TEXT='ocean_time'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, salt_id,'time', 
     &       LEN,TRIM(TEXT))
        TEXT='salinity, scalar, series'
        LEN=LEN_TRIM(TEXT)
        status = nf_put_att_text(ncid, salt_id,'field', 
     &       LEN,TRIM(TEXT))
!        status = nf_put_att_real(ncid, salt_id, 
!     &  '_FillValue', NF_REAL, 1,-99999.0)

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
      status = nf_put_att_text(ncid, NF_GLOBAL,'interpolation', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(5))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'grid_file', 
     &       LEN,TRIM(TEXT))
     
      TEXT=trim(globalstr(6))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'history', 
     &       LEN,TRIM(TEXT))
!      TEXT='Created by Aijun Zhang, OD/CO-OPS/NOS/NOAA'
      TEXT=trim(globalstr(7))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'reference', 
     &       LEN,TRIM(TEXT))
     
      status=nf_enddef(ncid)
      if (status .ne. NF_NOERR) print*, 'end definition'
      print *, ncid, time_id,temp_id,salt_id
!!  end of variable definition
      elseif (imode.eq.2) then
c       Inquire of this file what the last itime written was
C       should be equal to the dimension of time
        print *, 'imode==2', ncid, time_id,temp_id,salt_id
        status= nf_inq_dimlen(ncid,time_dim,itime)      
        itime=itime+1
c scalars
        CORNER(1) = itime
        COUNT(1)= NT
        status=nf_put_vara_real(ncid,time_id,CORNER,COUNT,ocean_time)
        if (status .ne. NF_NOERR)stop 'error write time'
        print*,'ocean_time',ocean_time

C    write 3D fields the real array defined in netcdf is 4-D =(x,y,z,time)
        CORNER(1) = 1
        CORNER(2) = 1
        CORNER(3) = 1
        CORNER(4) = itime
        COUNT(1)=IM
        COUNT(2)=JM
        COUNT(3)=KM 
        COUNT(4)=NT
          status=nf_put_vara_real(ncid,temp_id,CORNER,COUNT,temp)
          print*, 'status',status
          if (status .ne. NF_NOERR)stop 'write temperature'
          status=nf_put_vara_real(ncid,salt_id,CORNER,COUNT,salt)
          if (status .ne. NF_NOERR)stop 'write salinity'
	
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
