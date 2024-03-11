C xlf nos_ofs_read_restart.f  -I/gpfs/c2/home/wx21az/netcdf-3.6.2/include 
C -L/gpfs/c2/home/wx21az/netcdf-3.6.2/lib -lnetcdf -o nos_ofs_read_restart

C The julian subroutine does not work correctly while Year < 1900
C A new subroutine from Jianhua Qi is used 
C ! the following parameters have to be changed if model grid is changed
C      parameter (ne_global=142684,np_global=74061,ns_global=216748)  
C      parameter (nvrt=54)
      parameter (nbyte=4)
      implicit real(8)(a-h,o-z),integer(i-n)
      character*120 OFS,OCEAN_MODEL*10,COLD_START*10
      character*120 FIN,FOUT,GRIDFILE,FIXnos,netcdf_file
      character*120 BUFFER,CMD*132,VNAME,ANAME
      character*120 START_TIME, END_TIME,CTIME*26
      CHARACTER globalstr(9)*120
      real*8 jday_start,jdaye,jbase_date,JULIAN,yearb,monthb,dayb,hourb
      real*8 jday,jday0
      real*8 double1
      allocatable idry_e(:),we(:,:),tsel(:,:,:),idry_s(:),
     1 su2(:,:),sv2(:,:)
      allocatable tsd(:,:),ssd(:,:),idry(:),eta2(:),tnd(:,:),snd(:,:)
      allocatable tem0(:,:),sal0(:,:),q2(:,:),xl(:,:),dfv(:,:),
     1 dfh(:,:)
      allocatable dfq1(:,:),dfq2(:,:),qnon(:,:),trel0(:,:,:),
     1 trel(:,:,:)

      integer  Vtransform
      integer  Vstretching
      real*8  theta_s
      real*8  theta_b
      real*8  Tcline
      real*8  hc
      real, allocatable ::   s_rho(:)
      real, allocatable ::   s_w(:)
      real, allocatable ::   Cs_r(:)
      real, allocatable ::   Cs_w(:)
      real, allocatable ::   h(:)
      real, allocatable :: lat_rho (:)
      real, allocatable :: lon_rho (:)
      real, allocatable :: lat_u (:)
      real, allocatable :: lon_u (:)
      real, allocatable :: lat_v (:)
      real, allocatable :: lon_v (:)
      
! temporary arrays
      real, allocatable :: tmp1d  (:)
      real, allocatable :: tmp2d  (:,:)
      real, allocatable :: tmp3d  (:,:,:)
      real, allocatable :: tmp4d  (:,:,:,:)
      real, allocatable :: tmp5d  (:,:,:,:,:)
      integer dimids(5),COUNT(5),DIMS(5),STATUS
      LOGICAL FEXIST,CHANGE_TIME
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      INTEGER BASE_DATE(4)
      INTEGER DAYS_PER_MONTH(12)
      DATA (DAYS_PER_MONTH(i),I=1,12) /
     &31,28,31,30,31,30,31,31,30,31,30,31/
      CHANGE_TIME=.FALSE.
      read(5,'(a120)')OFS
      read(5,'(a10)')OCEAN_MODEL
      read(5,'(a10)')COLD_START
      read(5,'(a120)')GRIDFILE
      read(5,'(a120)')FIN
      read(5,'(a120)')FOUT
      read(5,'(a120)')BUFFER
      START_TIME=trim(adjustL(BUFFER))
      read(START_TIME,'(I4,4I2)')IYRS,IMMS,IDDS,IHHS
      read(5,'(a120)')BUFFER
      do i=1,len_trim(BUFFER)
         if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
            BUFFER(i:I)=' '
	 endif    
      enddo
      BUFFER=trim(adjustL(BUFFER))
      read(BUFFER,'(I4,3i2)')base_date
      yearb=base_date(1)
      monthb=base_date(2)
      dayb=base_date(3)
      hourb=base_date(4)
      jbase_date=JULIAN(yearb,monthb,dayb,hourb)
      
      yearb=IYRS
      monthb=IMMS
      dayb=IDDS
      hourb=IHHS   
      day_start=JULIAN(yearb,monthb,dayb,hourb)-jbase_date
      read(5,*)ne_global
      read(5,*)np_global
      read(5,*)ns_global
      read(5,*)nvrt
      
      OPEN(10,file=trim(FOUT))
      print *,'FIN=',TRIM(FIN)
      INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
      IF(.NOT. FEXIST)THEN
         print *,trim(FIN)//' does not exist'
	 PRINT *,'hot restart file is not found'
	 STOP
      ENDIF
!      FIN='hotstart.in'
!      ntracers=0
!      ne_global=141667
!      np_global=73550
!      ns_global=215220
!      nvrt=54
      allocate(idry_e(ne_global),we(nvrt,ne_global),
     1   tsel(2,nvrt,ne_global),idry_s(ns_global),
     2   su2(nvrt,ns_global),sv2(nvrt,ns_global),tsd(nvrt,ns_global),
     3   ssd(nvrt,ns_global),idry(np_global),eta2(np_global),
     4   tnd(nvrt,np_global),snd(nvrt,np_global),tem0(nvrt,np_global),
     5   sal0(nvrt,np_global),q2(np_global,nvrt),xl(np_global,nvrt),
     6   dfv(np_global,nvrt),dfh(np_global,nvrt),dfq1(np_global,nvrt),
     7   dfq2(np_global,nvrt),qnon(nvrt,np_global),stat=istat )
      if(istat/=0) stop 'Allocation error (2)'
      if(ntracers==0) then
        allocate(trel0(1,1,1),trel(1,1,1),stat=istat)
      else
        allocate(trel0(ntracers,nvrt,ne_global),
     1 	   trel(ntracers,nvrt,ne_global),stat=istat)
      endif
      open(36,file=trim(FIN),form='unformatted',status='old')
      read(36) double1,iths,ifile
      ocean_time=double1/86400.
      print *,'time in the inital file= ',ocean_time,iths,ifile
      do i=1,ne_global
        read(36) itmp,idry_e(i),(we(j,i),tsel(1:2,j,i),
     1	   (trel0(l,j,i),trel(l,j,i),l=1,ntracers),j=1,nvrt)
      enddo !i
      do i=1,ns_global
         read(36) itmp,idry_s(i),(su2(j,i),sv2(j,i),tsd(j,i),
     1	   ssd(j,i),j=1,nvrt)
      enddo !i
      do i=1,np_global
         read(36) itmp,eta2(i),idry(i),(tnd(j,i),snd(j,i),tem0(j,i),
     1	   sal0(j,i),q2(i,j),xl(i,j),dfv(i,j),dfh(i,j),dfq1(i,j),
     2     dfq2(i,j),qnon(j,i),j=1,nvrt)
      enddo !i
      CLOSE(36)
      open(36,file='hotstart.dat')
      write(36,*) double1,iths,ifile
      do i=1,ne_global
        WRITE(36,*) itmp,idry_e(i),(we(j,i),tsel(1:2,j,i),
     1  (trel0(l,j,i),trel(l,j,i),l=1,ntracers),j=1,nvrt)
      enddo !i
      do i=1,ns_global
        WRITE(36,*) itmp,idry_s(i),(su2(j,i),sv2(j,i),tsd(j,i),
     1  ssd(j,i),j=1,nvrt)
      enddo !i
      do i=1,np_global
        WRITE(36,*) itmp,eta2(i),idry(i),(tnd(j,i),snd(j,i),tem0(j,i),
     1  sal0(j,i),q2(i,j),xl(i,j),dfv(i,j),dfh(i,j),dfq1(i,j),
     2  dfq2(i,j),qnon(j,i),j=1,nvrt)
      enddo !i
      close(36)


      open(36,file=trim(FIN)//'.new',form='unformatted',
     1status='UNKNOWN')
      IF(TRIM(COLD_START) .eq. "T")THEN  !! forced from cold start
        WRITE(*,*)'set zeros for elevation and currents 
     1  for cold start' 
        do i=1,ne_global
        do j=1,nvrt
          we(j,i)=0.0
        enddo !i
        enddo !i
        do i=1,ns_global
        do j=1,nvrt
          su2(j,i)=0.0
          sv2(j,i)=0.0
        enddo !i
        enddo !i
        do i=1,np_global
          eta2(i)=0.0
        enddo !i
        
      ENDIF
      double1=0.0
      write(36) double1,iths,ifile
      do i=1,ne_global
        WRITE(36) itmp,idry_e(i),(we(j,i),tsel(1:2,j,i),
     1  (trel0(l,j,i),trel(l,j,i),l=1,ntracers),j=1,nvrt)
      enddo !i
      do i=1,ns_global
        WRITE(36) itmp,idry_s(i),(su2(j,i),sv2(j,i),tsd(j,i),
     1  ssd(j,i),j=1,nvrt)
      enddo !i
      do i=1,np_global
        WRITE(36) itmp,eta2(i),idry(i),(tnd(j,i),snd(j,i),tem0(j,i),
     1  sal0(j,i),q2(i,j),xl(i,j),dfv(i,j),dfh(i,j),dfq1(i,j),
     2  dfq2(i,j),qnon(j,i),j=1,nvrt)
      enddo !i
      IYR=base_date(1)
      IMM=base_date(2)
      IDD=base_date(3)
      IHH=base_date(4)
      NTIMES=0   ! not used by SELFE
      tide_start=0.0  ! not used by SELFE
      day_hotrestart=0.0  ! the restart time for nowcast run is always set to zero because
!                         ! base_date=time_hotstart 
      WRITE(10,100)IYR,IMM,IDD,IHH,NTIMES,day_hotrestart
     1 ,tide_start,'d0' 
      CLOSE(10)
      WRITE(*,*)'reading time_hotstart is COMPLETED SUCCESSFULLY'
100   FORMAT(I4.4,3I2.2,2x,I10,f12.4,2x,F12.4,a2)		     
      END

