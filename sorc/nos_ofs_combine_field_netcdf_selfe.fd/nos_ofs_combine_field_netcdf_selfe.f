!
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
!

program read_iwrite1
!-------------------------------------------------------------------------------
!  use typeSizes
!  use netcdf

      implicit real(4)(a-h,o-z),integer(i-n)
      include 'netcdf.inc'
!     parameter(nbyte=1)
      parameter(nbyte=4)
      character*120 filename,PREFIXNOS
      character*30 file63
      character*12 it_char
      character*12 it_charb,it_chare
      character*2 it_char2*3,it_char2hh
      character*48 start_time,version,variable_nm,variable_dim
      character*48 data_format
      character*1 ncastfcast
      character(72) :: fgb,fgb2,fdb  ! Processor specific global output file name
      character(len=48),save,dimension(26) :: outfile
      character(len=2) :: stringvalue
      integer :: iof(26)
      integer :: lfgb,lfdb       ! Length of processor specific global output file name
      character(len= 4) :: a_4
      allocatable ne(:),np(:),nsproc(:),ihot_len(:)
      allocatable ztot(:),sigma(:),cs(:),outb(:,:,:),eta2(:)
      allocatable :: offset(:), zeta_2da(:)
      allocatable sigma_z(:)
      allocatable i34(:),nm(:,:),nm2(:,:),js(:,:),xctr(:),yctr(:),dpe(:)
      allocatable x(:),y(:),dp(:),kbp00(:),iplg(:,:),ielg(:,:),kbp01(:,:)
      allocatable xlon(:),ylat(:)
      allocatable xcj(:),ycj(:),dps(:)

!  netcdf variables
      character(len=200) fname
      integer :: time_dims(1),ele_dims(2),x_dims(1),sigma_dims(1),var2d_dims(2),var3d_dims(3), &
     &           data_start_2d(2),data_start_3d(3),data_count_2d(2),data_count_3d(3),z_dims(1),sz_dims(1)
! AJ
      character(len=100) gridfile,parafile,base_date,buffer,date_file
      integer :: iyr,imon,idd,ihh,clen
      LOGICAL FEXIST
     
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20,TEXT*100
      CHARACTER globalstr(9)*120
      INTEGER DATE_TIME(8)
      INTEGER DAYS_PER_MONTH(12)
      DATA (DAYS_PER_MONTH(i),I=1,12) /31,28,31,30,31,30,31,31,30,31,30,31/ 
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
      globalstr(5)= 'gridded/field NetCDF file - CF-1.0'
      globalstr(6)= 'Hybrid SZ vertical coordinates, K=1 for bottom'
      globalstr(7)= 'Unstructured model grid: '
      globalstr(8)= 'Created at Eastern Local Time '//trim(CURRENT_TIME)
      globalstr(9)='Created by CO-OPS/NOS/NOAA'
!-----------------------------------------------------------------------
      CALL GETARG(1,PREFIXNOS)
!-------------------------------------------------------------------------------
! Aquire user inputs
!-------------------------------------------------------------------------------
      open(10,file='combine_output.in',status='old')
      read(10,*) ibgn,iend
      read(10,*) inc !inc=1: netcdf option
      read(10,'(a100)')gridfile 
      read(10,'(i4,3I2)')iyr,imon,idd,ihh 
      read(10,'(i4,3I2)')iyre,imone,idde,icyc 
      read(10,'(a1)')ncastfcast
      write(*,*) 'ncastfcast is:',ncastfcast
      WRITE(base_date,'(I4.4,a1,I2.2,a1,I2.2,a1,I2.2,a10)')iyr,'-',imon,'-',idd,' ',ihh,':00:00 UTC'
      write(it_char2hh,'(i2.2)') icyc
      write(*,*) 'it_char2hh is:',it_char2hh
      WRITE(date_file,'(I4.4,I2.2,I2.2)')iyre,imone,idde
      write(*,*) 'date_file is:',date_file
      close(10)

      noutput=26
      outfile(1)='elev.61'
      outfile(2)='pres.61'
      outfile(3)='airt.61'
      outfile(4)='shum.61'
      outfile(5)='srad.61'
      outfile(6)='flsu.61'
      outfile(7)='fllu.61'
      outfile(8)='radu.61'
      outfile(9)='radd.61'
      outfile(10)='flux.61'
      outfile(11)='evap.61'
      outfile(12)='prcp.61'
      outfile(13)='wind.62'
      outfile(14)='wist.62'
      outfile(15)='dahv.62'
      outfile(16)='vert.63'
      outfile(17)='temp.63'
      outfile(18)='salt.63'
      outfile(19)='conc.63'
      outfile(20)='tdff.63'
      outfile(21)='vdff.63'
      outfile(22)='kine.63'
      outfile(23)='mixl.63'
      outfile(24)='zcor.63'
      outfile(25)='qnon.63'
      outfile(26)='hvel.64'
      do i=1,noutput
        call get_param(trim(adjustl(outfile(i))),1,iof(i),tmp,stringvalue)
        if(iof(i)/=0.and.iof(i)/=1) then
          write(*,*)'Unknown output option',i,iof(i),outfile(i)
        endif
      enddo

! Read local_to_global_0000 for global info
      open(10,file='local_to_global_0000',status='old')
      read(10,*)ne_global,np_global,nvrt,nproc
      close(10)

      allocate(x(np_global),y(np_global),dp(np_global),kbp00(np_global), &
               np(0:nproc-1),ne(0:nproc-1),nm(ne_global,3),nm2(ne_global,3),eta2(np_global), &
               ztot(nvrt),sigma(nvrt),cs(nvrt),outb(np_global,nvrt,2),ihot_len(0:nproc-1),stat=istat)
      if(istat/=0) stop 'Allocation error (1)'
      allocate(xlon(np_global),ylat(np_global))
      allocate(sigma_z(nvrt))
      allocate(zeta_2da(np_global))
      allocate(offset(np_global))

      INQUIRE(FILE=trim(gridfile),EXIST=FEXIST)
      IF(.NOT.FEXIST)THEN
       WRITE(*,*)'model grid file '//trim(gridfile)//' not found'
       STOP 'nos_ofs_combine_field_netcdf_selfe.f'
      ENDIF  
      open(11,file=trim(gridfile),status='old')
      read(11,*)
      read(11,*)
      do i=1,np_global
        read(11,*)j,xlon(i),ylat(i)
      enddo
      close(11)
    
      INQUIRE(FILE='../'//trim(adjustL(PREFIXNOS))//'.fields.wl_correction.dat',EXIST=FEXIST)
      IF(.NOT.FEXIST)THEN
        WRITE(*,*)'water level offset file ../'//trim(PREFIXNOS)//'.fields.wl_correction.dat not found'
        do i=1,np_global
          offset(i)=0.0
        enddo
      ELSE
        close(500)
        open(500,file='../'//trim(adjustL(PREFIXNOS))//'.fields.wl_correction.dat',status='old')   
        do i=1,np_global
          read(500,*) itmp,xlontmp,ylattmp,offset(i)
        enddo
        close(500)
      ENDIF  

! Initialize outb for ivalid pts (below bottom etc)
      outb=-9999.

!-------------------------------------------------------------------------------
! Read rank-specific local_to_global*
!-------------------------------------------------------------------------------
   
! Read in local-global mappings from all ranks
      fdb='local_to_global_0000'
      lfdb=len_trim(fdb)

!Find max. for dimensioning
      np_max=0; ne_max=0
      do irank=0,nproc-1
        write(fdb(lfdb-3:lfdb),'(i4.4)') irank
        open(10,file=fdb,status='old')
        read(10,*) !global info
        read(10,*) !info
        read(10,*)ne(irank)
        do i=1,ne(irank)
          read(10,*)!j,ielg(irank,i)
        enddo !i
        read(10,*)np(irank)
        close(10)
        np_max=max(np_max,np(irank))
        ne_max=max(ne_max,ne(irank))
      enddo !irank

      allocate(iplg(0:nproc-1,np_max),kbp01(0:nproc-1,np_max), &
     &         ielg(0:nproc-1,ne_max),stat=istat)
      if(istat/=0) stop 'Allocation error (2)'

!Re-read
      do irank=0,nproc-1
        write(fdb(lfdb-3:lfdb),'(i4.4)') irank
        open(10,file=fdb,status='old')
        read(10,*) !global info
        read(10,*) !info
        read(10,*)ne(irank)
        do i=1,ne(irank)
          read(10,*)j,ielg(irank,i)
        enddo !i
        read(10,*)np(irank)
        do i=1,np(irank)
          read(10,*)j,iplg(irank,i)
        enddo
        read(10,*)itmp !sides
        do i=1,itmp
          read(10,*)
        enddo
        read(10,*) !'Header:'
        read(10,'(a)')data_format,version,start_time
        read(10,*)nrec,dtout,nspool,nvrt,kz,h0,h_s,h_c,theta_b,theta_f
        read(10,*)(ztot(k),k=1,kz-1),(sigma(k),k=1,nvrt-kz+1)
        read(10,*)np(irank),ne(irank),(x(iplg(irank,m)),y(iplg(irank,m)),dp(iplg(irank,m)),kbp01(irank,m),m=1,np(irank)), &
     &            (ntmp,(nm2(m,mm),mm=1,3),m=1,ne(irank))
        close(10)
!   Compute C(s) for output
        do klev=kz,nvrt
          k=klev-kz+1
          cs(k)=(1-theta_b)*sinh(theta_f*sigma(k))/sinh(theta_f)+ &
     &      theta_b*(tanh(theta_f*(sigma(k)+0.5))-tanh(theta_f*0.5))/2/tanh(theta_f*0.5)
        enddo !klev
!   Compute kbp00 (to avoid mismatch of indices)
        do m=1,np(irank)
          ipgb=iplg(irank,m)
          kbp00(ipgb)=kbp01(irank,m)
        enddo !m
!   Reconstruct connectivity table
        do m=1,ne(irank)
          iegb=ielg(irank,m)
          if(iegb>ne_global) stop 'Overflow!'
          do mm=1,3
            itmp=nm2(m,mm)
            if(itmp>np(irank).or.itmp<=0) then
              write(*,*)'Overflow:',m,mm,itmp
              stop
            endif
            nm(iegb,mm)=iplg(irank,itmp)
          enddo !mm
        enddo !m
      enddo !irank=0,nproc-1

! Compute record length for each rank-specific binary output per time step
      do irank=0,nproc-1
        ihot_len(irank)=nbyte*(2+np(irank))
        if(i23d.eq.2) then
          ihot_len(irank)=ihot_len(irank)+nbyte*ivs*np(irank)
        else
          do i=1,np(irank)
            do k=max0(1,kbp01(irank,i)),nvrt
              do m=1,ivs
                ihot_len(irank)=ihot_len(irank)+nbyte
              enddo !m
            enddo !k
          enddo !i
        endif
      enddo !irank

!-------------------------------------------------------------------------------
! Time iteration -- select "node" data
!-------------------------------------------------------------------------------
! Loop over input files
      write(it_charb,'(i12)') ibgn
      it_charb=adjustl(it_charb)  !place blanks at end
      it_lenb=len_trim(it_charb)  !length without trailing blanks
      write(it_chare,'(i12)') iend
      it_chare=adjustl(it_chare)  !place blanks at end
      it_lene=len_trim(it_chare)  !length without trailing blanks
      if(inc.eq.0) then  !binary output
        do iinput=ibgn,iend
          write(it_char,'(i12)')iinput
          it_char=adjustl(it_char)  !place blanks at end
          it_len=len_trim(it_char)  !length without trailing blanks
          fgb=it_char(1:it_len)//'_0000'; lfgb=len_trim(fgb);
!Write header to output files 
          open(65,file=it_char(1:it_len)//'_'//file63,status='replace')
          data_format='DataFormat v5.0'
          variable_nm=file63 !not important
          variable_dim=file63
          write(65,'(a48)',advance="no") data_format
          write(65,'(a48)',advance="no") version
          write(65,'(a48)',advance="no") start_time
          write(65,'(a48)',advance="no") variable_nm
          write(65,'(a48)',advance="no") variable_dim
          a_4 = transfer(source=nrec,mold=a_4)
          write(65,"(a4)",advance="no") a_4
          a_4 = transfer(source=dtout,mold=a_4)
          write(65,"(a4)",advance="no") a_4
          a_4 = transfer(source=nspool,mold=a_4)
          write(65,"(a4)",advance="no") a_4
          a_4 = transfer(source=ivs,mold=a_4)
          write(65,"(a4)",advance="no") a_4
          a_4 = transfer(source=i23d,mold=a_4)
          write(65,"(a4)",advance="no") a_4
!Vertical grid
          a_4 = transfer(source=nvrt,mold=a_4)
          write(65,'(a4)',advance="no") a_4
          a_4 = transfer(source=kz,mold=a_4)
          write(65,'(a4)',advance="no") a_4
          a_4 = transfer(source=h0,mold=a_4)
          write(65,'(a4)',advance="no") a_4
          a_4 = transfer(source=h_s,mold=a_4)
          write(65,'(a4)',advance="no") a_4
          a_4 = transfer(source=h_c,mold=a_4)
          write(65,'(a4)',advance="no") a_4
          a_4 = transfer(source=theta_b,mold=a_4)
          write(65,'(a4)',advance="no") a_4
          a_4 = transfer(source=theta_f,mold=a_4)
          write(65,'(a4)',advance="no") a_4
          do k=1,kz-1
            a_4 = transfer(source=ztot(k),mold=a_4)
            write(65,'(a4)',advance="no") a_4
          enddo
          do k=kz,nvrt
            kin=k-kz+1
            a_4 = transfer(source=sigma(kin),mold=a_4)
            write(65,'(a4)',advance="no") a_4
          enddo !k
!Horizontal grid
          a_4 = transfer(source=np_global,mold=a_4)
          write(65,'(a4)',advance="no") a_4
          a_4 = transfer(source=ne_global,mold=a_4)
          write(65,'(a4)',advance="no") a_4
          do m=1,np_global
            a_4 = transfer(source=x(m),mold=a_4)
            write(65,'(a4)',advance="no") a_4
            a_4 = transfer(source=y(m),mold=a_4)
            write(65,'(a4)',advance="no") a_4
            a_4 = transfer(source=dp(m),mold=a_4)
            write(65,'(a4)',advance="no") a_4
            a_4 = transfer(source=kbp00(m),mold=a_4)
            write(65,'(a4)',advance="no") a_4
          enddo !m=1,np
          do m=1,ne_global
            a_4 = transfer(source=3,mold=a_4)
            write(65,'(a4)',advance="no") a_4
            do mm=1,3
              a_4 = transfer(source=nm(m,mm),mold=a_4)
              write(65,'(a4)',advance="no") a_4
            enddo !mm
          enddo !m
        enddo!! iinput
      endif


      ihr=0
      do iinput=ibgn,iend
        write(it_char,'(i12)')iinput
        it_char=adjustl(it_char)  !place blanks at end
        it_len=len_trim(it_char)  !length without trailing blanks
        fgb=it_char(1:it_len)//'_0000'; lfgb=len_trim(fgb);
        fgb2=fgb
!        irank=0
!        irank=1                      ! based on Greg Seroka recommendation to make it work on WCOSS Phase 3
! The following irank 3 is based on AJ's comment:make it stop creating the extra bad files on Phase 3 after removing the old *.o files and recompiled, 05/28/2019
        irank=3                      
        file63=outfile(1)
        file63=adjustl(file63)
        write(fgb2(lfgb-3:lfgb),'(i4.4)') irank
        filename=fgb2(1:lfgb)//'_'//file63
        ihot_len(0)=nbyte*(2+np(0))
        ivs=1
        ihot_len(0)=ihot_len(0)+nbyte*ivs*np(0)
        call no_of_rec(filename,ihot_len(0),nrec)
        write(*,*) 'There are ',nrec,' records in file ',filename
        nrectmp=nrec
        do ispool=1,nrectmp
          do ii=1,noutput
            if (iof(ii).eq.1) then
              file63=outfile(ii)
              file63=adjustl(file63)
              lfile63=len_trim(file63)
!           Compute ivs and i23d
              if(file63((lfile63-1):lfile63).eq.'61'.or.file63((lfile63-1):lfile63).eq.'63') then
                ivs=1
              else if(file63((lfile63-1):lfile63).eq.'62'.or.file63((lfile63-1):lfile63).eq.'64') then
                ivs=2
              else
                print*, 'Unknown file type:',file63
              endif
              if(file63((lfile63-1):lfile63).eq.'61'.or.file63((lfile63-1):lfile63).eq.'62') then
                i23d=2
              else
                i23d=3
              endif
!           Compute record length for each rank-specific binary output per time step
              do irank=0,nproc-1
                ihot_len(irank)=nbyte*(2+np(irank))
                if(i23d.eq.2) then
                  ihot_len(irank)=ihot_len(irank)+nbyte*ivs*np(irank)
                else
                  do i=1,np(irank)
                    do k=max0(1,kbp01(irank,i)),nvrt
                      do m=1,ivs
                        ihot_len(irank)=ihot_len(irank)+nbyte
                      enddo !m
                    enddo !k
                  enddo !i
                endif
              enddo !irank
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              if (ii.eq.1) then
                ihr=ihr+1
                write(it_char2,'(i3.3)') ihr-1
              endif
!epm          if(inc.eq.1 .and. ii.eq.1) then  !netcdf output of metadata
              if(inc.eq.1) then  !netcdf output of metadata
                if (ii.eq.1) then
                  fname='../'//trim(adjustl(PREFIXNOS))//'.t'//it_char2hh//'z'//'.'//trim(date_file)//'.fields.'//ncastfcast//it_char2//'.nc'
                  write(*,*) 'fname is:',trim(fname)
!EM               fname=it_charb(1:it_lenb)//'_'//it_chare(1:it_lene)//'combinefields'//'.nc'
!AJ               fname='combinefields.nc'
                  iret = nf_create(trim(fname), NF_CLOBBER, ncid)
!               define dimensions
                  iret = nf_def_dim(ncid, 'node',np_global, node_dim)
                  iret = nf_def_dim(ncid, 'nele',ne_global, nele_dim)
                  iret = nf_def_dim(ncid, 'nface',3, nface_dim)
                  iret = nf_def_dim(ncid, 'nv',nvrt, nv_dim)
                  iret = nf_def_dim(ncid, 'sigma',nvrt-kz+1, nsigma_dim)
                  if(kz/=1) iret = nf_def_dim(ncid, 'nz',kz-1, nz_dim)
                  iret = nf_def_dim(ncid, 'time', NF_UNLIMITED, ntime_dim)
!               define variables
                  time_dims(1) = ntime_dim
                  iret=nf_def_var(ncid,'time',NF_REAL,1,time_dims,itime_id)
                  if(iret.ne.NF_NOERR) then
                    print*, nf_strerror(iret); stop
                  endif
                  iret=nf_put_att_text(ncid,itime_id,'long_name',4,'Time')
                  base_date=trim(adjustL(base_date))
                  buffer='seconds since '//trim(base_date)
                  clen=len_trim(buffer)
                  iret=nf_put_att_text(ncid,itime_id,'units',clen,trim(buffer))
                  iret=nf_put_att_text(ncid,itime_id,'base_date',len_trim(base_date),trim(base_date))
                  iret=nf_put_att_text(ncid,itime_id,'standard_name',4,'time')
!               write time stamps later
                  ele_dims(1)=nele_dim; ele_dims(2)=nface_dim
                  iret=nf_def_var(ncid,'ele',NF_INT,2,ele_dims,iele_id)
                  iret=nf_put_att_text(ncid,iele_id,'long_name',35,'Horizontal Triangular Element Table')
                  iret=nf_put_att_text(ncid,iele_id,'units',15,'non-dimensional')
                  x_dims(1)=node_dim
                  iret=nf_def_var(ncid,'x',NF_REAL,1,x_dims,ix_id)
                  iret=nf_put_att_text(ncid,ix_id,'long_name',18,'nodal x-coordinate')
                  iret=nf_put_att_text(ncid,ix_id,'units',6,'meters')
                  iret=nf_def_var(ncid,'y',NF_REAL,1,x_dims,iy_id)
                  iret=nf_put_att_text(ncid,iy_id,'long_name',18,'nodal y-coordinate')
                  iret=nf_put_att_text(ncid,iy_id,'units',6,'meters')
                  iret=nf_def_var(ncid,'lon',NF_REAL,1,x_dims,ixlon_id)
                  iret=nf_put_att_text(ncid,ixlon_id,'long_name',15,'nodal longitude')
                  iret=nf_put_att_text(ncid,ixlon_id,'units',12,'degrees_east')
                  iret=nf_def_var(ncid,'lat',NF_REAL,1,x_dims,iylat_id)
                  iret=nf_put_att_text(ncid,iylat_id,'long_name',14,'nodal latitude')
                  iret=nf_put_att_text(ncid,iylat_id,'units',13,'degrees_north')
                  iret=nf_def_var(ncid,'h',NF_REAL,1,x_dims,idepth_id)
                  iret=nf_put_att_text(ncid,idepth_id,'long_name',10,'Bathymetry')
                  iret=nf_put_att_text(ncid,idepth_id,'standard_name',27,'sea_floor_depth_below_geoid')
                  iret=nf_put_att_text(ncid,idepth_id,'units',1,'m')
                  iret=nf_put_att_text(ncid,idepth_id,'positive',6,'down')
                  iret=nf_put_att_text(ncid,idepth_id,'grid',15,'Bathymetry_Mesh')
                  iret=nf_put_att_text(ncid,idepth_id,'coordinates',7,'lat lon')
                  iret=nf_put_att_text(ncid,idepth_id,'type',4,'data')
                  sigma_dims(1)=nsigma_dim
                  iret=nf_def_var(ncid,'sigma',NF_REAL,1,sigma_dims,isigma_id)
                  iret=nf_put_att_text(ncid,isigma_id,'long_name',29,'S coordinates at whole levels')
                  iret=nf_put_att_text(ncid,isigma_id,'units',15,'non-dimensional')
                  iret=nf_put_att_text(ncid,isigma_id,'positive',2,'up')
                  iret=nf_def_var(ncid,'Cs',NF_REAL,1,sigma_dims,ics_id)
                  iret=nf_put_att_text(ncid,ics_id,'long_name',29,'Function C(s) at whole levels')
                  iret=nf_put_att_text(ncid,ics_id,'units',15,'non-dimensional')
                  iret=nf_put_att_text(ncid,ics_id,'positive',2,'up')
                  iret=nf_def_var(ncid,'offset',NF_REAL,1,x_dims,ioffset_id)
                  iret=nf_put_att_text(ncid,ioffset_id,'long_name',18,'water level offset')
                  iret=nf_put_att_text(ncid,ioffset_id,'standard_name',47,'mean difference of model minus obs. over 7 days')
                  iret=nf_put_att_text(ncid,ioffset_id,'units',6,'meters')
                  iret=nf_put_att_text(ncid,ioffset_id,'positive',2,'up')
                  if(kz/=1) then
                    z_dims(1)=nz_dim
                    iret=nf_def_var(ncid,'z',NF_REAL,1,z_dims,iz_id)
                    iret=nf_put_att_text(ncid,iz_id,'long_name',29,'Z coordinates at whole levels')
                    iret=nf_put_att_text(ncid,iz_id,'units',6,'meters')
                    iret=nf_put_att_text(ncid,iz_id,'positive',2,'up')
                    sz_dims(1)=nv_dim
                    iret=nf_def_var(ncid,'sigma_z',NF_REAL,1,sz_dims,isz_id)
                    iret=nf_put_att_text(ncid,isz_id,'long_name',34,'sigma(1-37 layers),Z(38-54 layers)')
                    iret=nf_put_att_text(ncid,isz_id,'units',15,'sigma or meters')
                    iret=nf_put_att_text(ncid,isz_id,'positive',2,'up')
                  endif !kz
                endif !ii equals 1
                file63=outfile(ii)
                file63=adjustl(file63)
                lfile63=len_trim(file63)
!             Compute ivs and i23d
                if(file63((lfile63-1):lfile63).eq.'61'.or.file63((lfile63-1):lfile63).eq.'63') then
                  ivs=1
                else if(file63((lfile63-1):lfile63).eq.'62'.or.file63((lfile63-1):lfile63).eq.'64') then
                  ivs=2
                else
                  print*, 'Unknown file type:',file63
                endif
                if(file63((lfile63-1):lfile63).eq.'61'.or.file63((lfile63-1):lfile63).eq.'62') then
                  i23d=2
                else
                  i23d=3
                endif
                if(i23d.eq.2) then
                  var2d_dims(1)=node_dim; var2d_dims(2)=ntime_dim
                  if (ii.eq.1 .and. iof(ii).eq.1) then
!                  if (iof(1).eq.1) then
                    iret=nf_def_var(ncid,'zeta',NF_REAL,2,var2d_dims,ielev_id)
                    iret=nf_put_att_text(ncid,ielev_id,'long_name',23,'Water Surface Elevation')
                    iret=nf_put_att_text(ncid,ielev_id,'units',6,'meters')
                    iret=nf_put_att_text(ncid,ielev_id,'positive',2,'up')
                    iret=nf_put_att_text(ncid,ielev_id,'standard_name',47,'Original modeled sea_surface_height_above_geoid')
                    iret=nf_put_att_text(ncid,ielev_id,'grid',15,'Bathymetry_Mesh')
                    iret=nf_put_att_text(ncid,ielev_id,'type',4,'data')
                    iret=nf_put_att_text(ncid,ielev_id,'coordinates',12,'time lat lon')
                    iret=nf_put_att_text(ncid,ielev_id,'location',4,'node')
                    iret=nf_put_att_real(ncid,ielev_id,'missing_value',NF_REAL,1,-9999.)
                    iret=nf_def_var(ncid,'zeta_adj',NF_REAL,2,var2d_dims,ieleva_id)
                    iret=nf_put_att_text(ncid,ieleva_id,'long_name',32,'Adjusted Water Surface Elevation')
                    iret=nf_put_att_text(ncid,ieleva_id,'units',6,'meters')
                    iret=nf_put_att_text(ncid,ieleva_id,'positive',2,'up')
                    iret=nf_put_att_text(ncid,ieleva_id,'standard_name',47,'offset corrected sea_surface_height_above_geoid')
                    iret=nf_put_att_text(ncid,ieleva_id,'grid',15,'Bathymetry_Mesh')
                    iret=nf_put_att_text(ncid,ieleva_id,'type',4,'data')
                    iret=nf_put_att_text(ncid,ieleva_id,'coordinates',12,'time lat lon')
                    iret=nf_put_att_text(ncid,ieleva_id,'location',4,'node')
                    iret=nf_put_att_real(ncid,ieleva_id,'missing_value',NF_REAL,1,-9999.)
                  endif
                  if (ii.eq.13 .and. iof(ii).eq.1) then
!                  if (iof(13).eq.1) then
                    iret=nf_def_var(ncid,'uwind_speed',NF_REAL,2,var2d_dims,iwindu_id)
                    iret=nf_put_att_text(ncid,iwindu_id,'long_name',22,'Eastward Wind Velocity')
                    iret=nf_put_att_text(ncid,iwindu_id,'units',10,'meters s-1')
                    iret=nf_put_att_text(ncid,iwindu_id,'grid',15,'Bathymetry_Mesh')
                    iret=nf_put_att_text(ncid,iwindu_id,'type',4,'data')
                    iret=nf_put_att_text(ncid,iwindu_id,'coordinates',12,'time lat lon')             
                    iret=nf_put_att_text(ncid,iwindu_id,'location',4,'node')
                    iret=nf_put_att_real(ncid,iwindu_id,'missing_value',NF_REAL,1,-9999.)
                    iret=nf_def_var(ncid,'vwind_speed',NF_REAL,2,var2d_dims,iwindv_id)
                    iret=nf_put_att_text(ncid,iwindv_id,'long_name',23,'Northward Wind Velocity')
                    iret=nf_put_att_text(ncid,iwindv_id,'units',10,'meters s-1')
                    iret=nf_put_att_text(ncid,iwindv_id,'grid',15,'Bathymetry_Mesh')
                    iret=nf_put_att_text(ncid,iwindv_id,'type',4,'data')
                    iret=nf_put_att_text(ncid,iwindv_id,'coordinates',12,'time lat lon')             
                    iret=nf_put_att_text(ncid,iwindv_id,'location',4,'node')
                    iret=nf_put_att_real(ncid,iwindv_id,'missing_value',NF_REAL,1,-9999.)
                  endif
                  if(ii.eq.2 .and. iof(ii).eq.1) then
!                  if(iof(2).eq.1) then
                    iret=nf_def_var(ncid,'Pair',NF_REAL,2,var2d_dims,iprmsl_id)
                    iret=nf_put_att_text(ncid,iprmsl_id,'long_name',23,'Pressure reduced to MSL')
                    iret=nf_put_att_text(ncid,iprmsl_id,'units',2,'Pa')
                    iret=nf_put_att_text(ncid,iprmsl_id,'grid',15,'Bathymetry_Mesh')
                    iret=nf_put_att_text(ncid,iprmsl_id,'type',4,'data')
                    iret=nf_put_att_text(ncid,iprmsl_id,'coordinates',12,'time lat lon')
                    iret=nf_put_att_text(ncid,iprmsl_id,'location',4,'node')
                    iret=nf_put_att_real(ncid,iprmsl_id,'missing_value',NF_REAL,1,-9999.)
                  endif 
                else !3D
                  var3d_dims(1)=node_dim; var3d_dims(2)=nv_dim; var3d_dims(3)=ntime_dim
                  if(file63((lfile63-1):lfile63).eq.'64') then
                  if(ii.eq.26 .and. iof(ii).eq.1) then
 !                   if(iof(26).eq.1) then
                      iret=nf_def_var(ncid,'u',NF_REAL,3,var3d_dims,iu_id)
                      iret=nf_put_att_text(ncid,iu_id,'long_name',23,'Eastward Water Velocity')
                      iret=nf_put_att_text(ncid,iu_id,'units',10,'meters s-1')
                      iret=nf_put_att_text(ncid,iu_id,'grid',15,'Bathymetry_Mesh')
                      iret=nf_put_att_text(ncid,iu_id,'type',4,'data')
                      iret=nf_put_att_text(ncid,iu_id,'coordinates',15,'time nv lat lon')
                      iret=nf_put_att_text(ncid,iu_id,'location',4,'node')
                      iret=nf_put_att_real(ncid,iu_id,'missing_value',NF_REAL,1,-9999.)
                      iret=nf_def_var(ncid,'v',NF_REAL,3,var3d_dims,iv_id)
                      iret=nf_put_att_text(ncid,iv_id,'long_name',24,'Northward Water Velocity')
                      iret=nf_put_att_text(ncid,iv_id,'units',10,'meters s-1')
                      iret=nf_put_att_text(ncid,iv_id,'grid',15,'Bathymetry_Mesh')
                      iret=nf_put_att_text(ncid,iv_id,'type',4,'data')
                      iret=nf_put_att_text(ncid,iv_id,'coordinates',15,'time nv lat lon')
                      iret=nf_put_att_text(ncid,iv_id,'location',4,'node')
                      iret=nf_put_att_real(ncid,iv_id,'missing_value',NF_REAL,1,-9999.)
                    endif
                  else
                    if(ii.eq.18 .and. iof(ii).eq.1) then
    !                if(iof(18).eq.1) then
                      iret=nf_def_var(ncid,'salinity',NF_REAL,3,var3d_dims,isalt_id)
                      iret=nf_put_att_text(ncid,isalt_id,'long_name',8,'salinity')
                      iret=nf_put_att_text(ncid,isalt_id,'standard_name',18,'sea_water_salinity')
                      iret=nf_put_att_text(ncid,isalt_id,'units',3,'psu')
                      iret=nf_put_att_text(ncid,isalt_id,'grid',15,'Bathymetry_Mesh')
                      iret=nf_put_att_text(ncid,isalt_id,'type',4,'data')
                      iret=nf_put_att_text(ncid,isalt_id,'coordinates',15,'time nv lat lon')
                      iret=nf_put_att_text(ncid,isalt_id,'location',4,'node')
                      iret=nf_put_att_real(ncid,isalt_id,'missing_value',NF_REAL,1,-9999.)
                    endif
                    if(ii.eq.17 .and. iof(ii).eq.1) then
    !                if(iof(17).eq.1) then
                      iret=nf_def_var(ncid,file63(1:lfile63-3),NF_REAL,3,var3d_dims,itemp_id)
                      iret=nf_put_att_text(ncid,itemp_id,'long_name',11,'temperature')
                      iret=nf_put_att_text(ncid,itemp_id,'standard_name',21,'sea_water_temperature')
                      iret=nf_put_att_text(ncid,itemp_id,'units',9,'degrees_C')
                      iret=nf_put_att_text(ncid,itemp_id,'grid',15,'Bathymetry_Mesh')
                      iret=nf_put_att_text(ncid,itemp_id,'type',4,'data')
                      iret=nf_put_att_text(ncid,itemp_id,'coordinates',15,'time nv lat lon')
                      iret=nf_put_att_text(ncid,itemp_id,'location',4,'node')
                      iret=nf_put_att_real(ncid,itemp_id,'missing_value',NF_REAL,1,-9999.)
                    endif
                    if(ii.eq.24 .and. iof(ii).eq.1) then
!                    if(iof(24).eq.1) then
                      iret=nf_def_var(ncid,'zval',NF_REAL,3,var3d_dims,izval_id)
                      iret=nf_put_att_text(ncid,izval_id,'long_name',29,'height of vertical grid level')
                      iret=nf_put_att_text(ncid,izval_id,'standard_name',19,'z-coordinate height')
                      iret=nf_put_att_text(ncid,izval_id,'units',6,'meters')
                      iret=nf_put_att_text(ncid,izval_id,'grid',15,'Bathymetry_Mesh')
                      iret=nf_put_att_text(ncid,izval_id,'type',4,'data')
                      iret=nf_put_att_text(ncid,izval_id,'coordinates',15,'time nv lat lon')    
                      iret=nf_put_att_text(ncid,izval_id,'location',4,'node')
                      iret=nf_put_att_real(ncid,izval_id,'missing_value',NF_REAL,1,-9999.)
                    endif
                  endif
                endif !2D or 3D
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
                TEXT=trim(globalstr(9))
                LEN=LEN_TRIM(TEXT)
                iret = nf_put_att_text(ncid, NF_GLOBAL,'reference', LEN,TRIM(TEXT))
!                iret = nf_enddef(ncid)
              endif !inc is 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            endif !iof of ii equals 1
          end do !ii=1,noutput
                iret = nf_enddef(ncid)
          do ii=1,noutput
            if(inc.eq.1 .and. ii.eq.1) then  !netcdf output of metadata
!           Write mode (header part only)
              data_start_2d(1:2)=1
              data_count_2d(1)=ne_global; data_count_2d(2)=3
              iret=nf_put_vara_int(ncid,iele_id,data_start_2d,data_count_2d,nm)
              iret=nf_put_vara_real(ncid,ix_id,1,np_global,x)
              iret=nf_put_vara_real(ncid,iy_id,1,np_global,y)
              iret=nf_put_vara_real(ncid,ixlon_id,1,np_global,xlon)
              iret=nf_put_vara_real(ncid,iylat_id,1,np_global,ylat)
              iret=nf_put_vara_real(ncid,idepth_id,1,np_global,dp)
              iret=nf_put_vara_real(ncid,isigma_id,1,nvrt-kz+1,sigma)
              iret=nf_put_vara_real(ncid,ics_id,1,nvrt-kz+1,cs)
              iret=nf_put_vara_real(ncid,ioffset_id,1,np_global,offset)
              if(kz/=1)then
                iret=nf_put_vara_real(ncid,iz_id,1,kz-1,ztot)
                iret=nf_put_vara_real(ncid,isz_id,1,nvrt-kz+1,sigma)          
                iret=nf_put_vara_real(ncid,isz_id,nvrt-kz+2,nvrt,ztot)
              endif
            endif
            if (iof(ii).eq.1) then
              file63=outfile(ii)
              file63=adjustl(file63)
              lfile63=len_trim(file63)
!           Compute ivs and i23d
              if(file63((lfile63-1):lfile63).eq.'61'.or.file63((lfile63-1):lfile63).eq.'63') then
                ivs=1
              else if(file63((lfile63-1):lfile63).eq.'62'.or.file63((lfile63-1):lfile63).eq.'64') then
                ivs=2
              else
                print*, 'Unknown file type:',file63
              endif
              if(file63((lfile63-1):lfile63).eq.'61'.or.file63((lfile63-1):lfile63).eq.'62') then
                i23d=2
              else
                i23d=3
              endif
!           Compute record length for each rank-specific binary output per time step
              do irank=0,nproc-1
                ihot_len(irank)=nbyte*(2+np(irank))
                if(i23d.eq.2) then
                  ihot_len(irank)=ihot_len(irank)+nbyte*ivs*np(irank)
                else
                  do i=1,np(irank)
                    do k=max0(1,kbp01(irank,i)),nvrt
                      do m=1,ivs
                        ihot_len(irank)=ihot_len(irank)+nbyte
                      enddo !m
                    enddo !k
                  enddo !i
                endif
                fgb2=fgb
                write(fgb2(lfgb-3:lfgb),'(i4.4)') irank
                open(63,file=fgb2(1:lfgb)//'_'//file63,access='direct',recl=ihot_len(irank),status='old')
                if(i23d.eq.2) then
                  read(63,rec=ispool)time,it,(eta2(iplg(irank,i)),i=1,np(irank)),((outb(iplg(irank,i),1,m),m=1,ivs),i=1,np(irank))
                else !3D
                  read(63,rec=ispool)time,it,(eta2(iplg(irank,i)),i=1,np(irank)), &
     &                               (((outb(iplg(irank,i),k,m),m=1,ivs),k=max0(1,kbp01(irank,i)),nvrt),i=1,np(irank))
                  do i=1,np(irank)
                    do k=1,kbp01(irank,i)-1
                      do m=1,ivs
                        outb(iplg(irank,i),k,m)=-9999.
                      end do
                    end do
                  end do
                endif
                close(63)
              enddo !irank
              do ino=1,np_global
                if ((dp(ino)+eta2(ino)).lt.h0) then
                  eta2(ino)=-9999.
                  if(i23d.eq.2) then
                    do m=1,ivs
                      outb(ino,1,m)=-9999.
                    end do
                  else !3D
                    do k=1,nvrt
                      do m=1,ivs
                        outb(ino,k,m)=-9999.
                      end do
                    end do
                  endif
                endif
                if(i23d.eq.3) then
                  do k=1,nvrt
                    do m=1,ivs
                      if (ii.ne.24) then
                        if (outb(ino,k,m)<(-9.0)) then
                          outb(ino,k,m)=-9999.
                        endif
                      endif
                    end do
                  end do
                endif
              end do !ino
              if(inc.eq.0) then !binary
                a_4 = transfer(source=time,mold=a_4)
                write(65,"(a4)",advance="no") a_4
                a_4 = transfer(source=it,mold=a_4)
                write(65,"(a4)",advance="no") a_4
                do i=1,np_global
                  a_4 = transfer(source=eta2(i),mold=a_4)
                  write(65,"(a4)",advance="no") a_4
                enddo !i
                do i=1,np_global
                  if(i23d.eq.2) then
                    do m=1,ivs
                      a_4 = transfer(source=outb(i,1,m),mold=a_4)
                      write(65,"(a4)",advance="no") a_4
                    enddo !m
                  else !i23d=3 
                    do k=max0(1,kbp00(i)),nvrt
                      do m=1,ivs
                        a_4 = transfer(source=outb(i,k,m),mold=a_4)
                        write(65,"(a4)",advance="no") a_4
                      enddo !m
                    enddo !k
                  endif !i23d
                enddo !i
              else !netcdf
!epm            if (iinput.eq.ibgn) then
!epm              ispool_base = (iinput-ibgn)*nrec
!epm            else
!epm              ispool_base = (iinput-ibgn)*nrec+1
!epm            endif
                ispool_base = 0
                print*, 'ispool_base=', ispool_base, ' iinput=', iinput, ' nrec=',nrec    
                if (ii.eq.1) then
                  iret=nf_put_vara_real(ncid,itime_id,1,1,time)
                endif
                if(i23d.eq.2) then
                  data_start_2d(1)=1; data_start_2d(2)=1
                  data_count_2d(1)=np_global; data_count_2d(2)=1
                  if (ii.eq.1 .and. iof(ii).eq.1) then
                    iret=nf_put_vara_real(ncid,ielev_id,data_start_2d,data_count_2d,outb(:,1,1))
                    do i=1,np_global
                       zeta_2da(i)=outb(i,1,1)-offset(i)
                    enddo
                    iret=nf_put_vara_real(ncid,ieleva_id,data_start_2d,data_count_2d,zeta_2da)
                  endif
                  if (ii.eq.13 .and. iof(ii).eq.1) then
                    iret=nf_put_vara_real(ncid,iwindu_id,data_start_2d,data_count_2d,outb(:,1,1))
                    iret=nf_put_vara_real(ncid,iwindv_id,data_start_2d,data_count_2d,outb(:,1,2))
                  endif
                  if (ii.eq.2 .and. iof(ii).eq.1) then
                    iret=nf_put_vara_real(ncid,iprmsl_id,data_start_2d,data_count_2d,outb(:,1,1))
                  endif
                else !3D
                  data_start_3d(1:2)=ispool_base+1; data_start_3d(3)=1
                  data_start_3d(1)=1; data_start_3d(1:2)=1; data_start_3d(3)=1  
                  data_count_3d(1)=np_global; data_count_3d(2)=nvrt; data_count_3d(3)=1
                  if(file63((lfile63-1):lfile63).eq.'64') then
                    if (ii.eq.26 .and. iof(ii).eq.1) then
                      iret=nf_put_vara_real(ncid,iu_id,data_start_3d,data_count_3d,outb(:,:,1))
                      iret=nf_put_vara_real(ncid,iv_id,data_start_3d,data_count_3d,outb(:,:,2))
                    endif
                  else
                    if (ii.eq.18 .and. iof(ii).eq.1) then
                      iret=nf_put_vara_real(ncid,isalt_id,data_start_3d,data_count_3d,outb(:,:,1))
                    endif
                    if (ii.eq.17 .and. iof(ii).eq.1) then
                      iret=nf_put_vara_real(ncid,itemp_id,data_start_3d,data_count_3d,outb(:,:,1))
                    endif
                    if (ii.eq.24 .and. iof(ii).eq.1) then
                      iret=nf_put_vara_real(ncid,izval_id,data_start_3d,data_count_3d,outb(:,:,1))
                    endif
                  endif
                endif !i23d
                print*, 'done record # ',ispool,' iinput=',iinput,' ii=',ii, ', ', file63
              endif !inc is 0 (binary) or 1 (netcdf)
            endif !iof of ii equals 1
          end do !ii=1,noutput
          if(inc.eq.1) then !netcdf
            iret = nf_close(ncid)
          endif
        end do !ispool=1,nrectmp
      end do !iinput=ibgn,iend

! Close output file
      if(inc.eq.0) then !binary
        close(65)
      endif
      print *,'nos_ofs_combine_field_netcdf_selfe.x completed successfully'
      stop
end program read_iwrite1



subroutine get_param(varname,vartype,ivarvalue,varvalue1,varvalue2)
implicit real(8)(a-h,o-z), integer(i-n)
character(*),intent(in) :: varname
 integer,intent(in) :: vartype
 integer,intent(out) :: ivarvalue
 real(8),intent(out) :: varvalue1
 character(len=100) parafile
character(len=2),intent(out) :: varvalue2
character(len=90) :: line_str,str_tmp,str_tmp2
str_tmp2=adjustl(varname)
lstr_tmp2=len_trim(str_tmp2)
open(15,file='../param.in',status='old')
!open(15,file=trim(parafile),status='old')
rewind(15)
line=0
do
line=line+1
read(15,'(a)',end=99)line_str
line_str=adjustl(line_str)
len_str=len_trim(line_str)
if(len_str.eq.0.or.line_str(1:1).eq.'!') cycle
loc=index(line_str,'=')
loc2=index(line_str,'!')
if(loc2/=0.and.loc2-1<loc+1) then
  write(*,*) 'READ_PARAM:'
  stop
endif
str_tmp=''
str_tmp(1:loc-1)=line_str(1:loc-1)
str_tmp=trim(str_tmp)
lstr_tmp=len_trim(str_tmp)
if(str_tmp(1:lstr_tmp).eq.str_tmp2(1:lstr_tmp2)) then
if(loc2/=0) then
str_tmp2=line_str(loc+1:loc2-1)
else
str_tmp2=line_str(loc+1:len_str)
endif
str_tmp2=adjustl(str_tmp2)
str_tmp2=trim(str_tmp2)
if(vartype.eq.0) then
varvalue2=str_tmp2(1:2)
else if(vartype.eq.1) then
read(str_tmp2,*)ivarvalue
else if(vartype.eq.2) then
read(str_tmp2,*)varvalue1
else
write(*,*)'read_param: unknown type:',vartype
endif
exit
endif
enddo
close(15)
return
99 close(15)
write(*,*)'Failed to find parameter:',varname
end subroutine get_param

      subroutine no_of_rec(ifile,irecl,i) 

      implicit none 

      integer*4       i
      integer         irecl
      character*120   ifile 
      character*512   string 

      open(10,file=ifile,access='direct',recl=irecl,err=900) 
      i = 999999 
      DO
        read(10,rec=i,err=110)string 
        exit 
110     i = i-1 
        if (i .gt. 0)cycle   
        i=-999
        exit
      ENDDO  
900   continue
999   close(10)
      print *,'recl= ',i 
      return 
      end
