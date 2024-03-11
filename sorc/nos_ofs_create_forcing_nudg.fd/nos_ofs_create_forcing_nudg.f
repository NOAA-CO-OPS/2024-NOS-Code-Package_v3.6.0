C----------------------------------------------------------------------------------
C
C Program Name:  nos_ofs_create_forcing_nudging_field.f
C
C Directory:  .../nosofs_shared.v3.0.1/sorc/nos_ofs_create_forcing_nudging.fd/

C Purpose:    This Program is used to generated nudging 'climatology' T/S field for ROMS 
C             from climatological dataset WOA05 or RTOFS/HYCOM products in NCEP data tank.
C             The data on RTOFS/HYCOM grid is horizontally interpolated onto ROMS grid
C             using either remesh, nature neighbors, or bicubic, or bilinear routine,
C             and then vertically interpolated onto ROMS sigma vertical levels from
C             z-coordinate vertical levels linearly.

C             Refer to HPC_COMF Technical Report for more detail information.         
C             
C Current contact:   Aijun Zhang
C         Org:  NOS/CO-OPS/OD   Phone:  240-533-0591
C                    aijun.zhang@Noaa.gov 
C Attributes:
C  Language:  Fortran
C  Computer:  NCEP HPC  
CC
C  Compile command:  gmake -f makefile
C
C Subprograms called:   remesh, regrid, write_netCDF_field_ROMS, utility
C
C Input Data files:
C   "/dcom/us007003/20081120/wgrdbul/ncom_glb_reg1_2008112000.nc.gz"
C
C Usage:   nos_ofs_create_forcing_nudging_field  < Fortran_Nudging.ctl > Fortran_Nudging.log 
C
C
C Input Parameters:
C           OFS         : name of Operational Forecast System, e.g. CBOFS, TBOFS
C        Ocean_MODEL    : Name of numerical ocean model used in OFS, e.g. ROMS, FVCOM
C        DBASE_TS       : data source for temperature and salinity OBC
C        TIME_START     : Start time 
C        FORHRS         : Length of forecast time in hours
C        IGRD           : indicator of horizontal interpolation method
C                        =1:  remesh using triangulation techniques
C 		         =2: bicubic routine from ROMS
C			 =3: bilinear routine from ROMS
C                        =4: nature neighbours
C        FIXnos         : path to store NOS static data files 
C        GRIDFILE       : Grid file name of the OFS			   
C        OUTPUTFILE     : Output file name   
C        MINLON         :longitude of lower left/southwest corner to cover the OFS domain
C        MINLAT         :latitude of lower left /southwest corner to cover the OFS domain
C        MINLON         :longitude of upper right/northeast corner to cover the OFS domain
C        MINLAT         :latitude of  upper right/northeast corner to cover the OFS domain
C        KBm            : Number of vertical levels for temperature and salinity of the OFS 
C       THETA_S         :S-coordinate surface control parameter, [0 < theta_s < 20].
C       THETA_B         :S-coordinate bottom  control parameter, [0 < theta_b < 1].
C       TCLINE          :Width (m) of surface or bottom boundary layer in which
C                        higher vertical resolution is required during stretching.
C       NVTRANS         :S-coordinate transform function parameter (1 or 2)
C       NVSTR           :S-coordinate stretching function parameter (1,2,3,or 4)

C
C Output files: 
C    A netCDF T/S 3-d "climatology" file to be used as the nudging target fields in ROMS. 
C----------------------------------------------------------------------------------
      USE, INTRINSIC :: IEEE_ARITHMETIC
      parameter (NMAX=90000)
      include 'netcdf.inc'
      character*200 OFS,OCEAN_MODEL*10,DBASE_TS
      character*200 FIN,GRIDFILE,netcdf_file
      character*200 BUFFER,VNAME,ANAME
      character*200 FILE_NCOM(100),NCOMDIR
      character*200 FILE_NCOM_TIDE(100)
      character*200 NCOM_FILE
      character*200 CORMSLOG
      character*200 START_TIME, END_TIME
      character*200 CLIM_FILE_LAST,COMOUTroot
      real*8 jbase_date,JULIAN,yearb,monthb,dayb,hourb
      real*8 jday,jdays,jdaye
      real minlon,minlat,maxlat,maxlon,missvalue
      LOGICAL FEXIST
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      CHARACTER globalstr(7)*120
      INTEGER DATE_TIME(8)
      INTEGER BASE_DATE(4)
      INTEGER IPARENT(NMAX)
      INTEGER JPARENT(NMAX)
cc allocatable arrays for ROMS model
      real, allocatable :: lonm  (:,:)
      real, allocatable :: latm  (:,:)
      real, allocatable :: angm  (:,:)
      real, allocatable :: sigma(:)
      real, allocatable :: sigmaw(:)
      real, allocatable :: Cs_r(:)
      real, allocatable :: Cs_w(:)
      real, allocatable :: ZSIGMA(:)
      real, allocatable :: ZSIGMAW(:)
      real, allocatable :: maskm  (:,:)
      real, allocatable :: maskm_e(:,:)
      real, allocatable :: hm  (:,:)
      real, allocatable :: tempm  (:,:,:,:)
      real, allocatable :: saltm  (:,:,:,:)
      real, allocatable :: timem(:)
      real, allocatable :: cff1(:)
      real, allocatable :: cff2(:)
      real, allocatable :: cff3(:)

      real, allocatable :: Iout(:,:)
      real, allocatable :: Jout(:,:)
      integer, allocatable :: IFILL(:)
      integer, allocatable :: JFILL(:)
      integer, allocatable :: ISHORE(:)
      integer, allocatable :: JSHORE(:)
!  allocatable arrays for variables in NCOM file
      real, allocatable :: lon  (:,:)
      real, allocatable :: lat  (:,:)
      real, allocatable :: ts_time  (:)
      real, allocatable :: depth  (:)
      
      real, allocatable :: lonsub  (:,:)
      real, allocatable :: latsub  (:,:)
      real, allocatable :: masksub  (:,:)
      real, allocatable :: temp (:,:,:,:)
      real, allocatable :: salt (:,:,:,:)
      real, allocatable :: tempave (:,:,:)
      real, allocatable :: saltave (:,:,:)
! temporary arrays
      real, allocatable :: tmp1d  (:)
      real, allocatable :: tmp2d  (:,:)
      real, allocatable :: tmp3d  (:,:,:)
      real, allocatable :: tmp4d  (:,:,:,:)
      integer, allocatable :: ITMP3D(:,:,:)
      real, allocatable :: oned1(:)
      real, allocatable :: oned2(:)
      real, allocatable :: oned3(:)
      real, allocatable :: oned4(:)
      real, allocatable :: outm(:,:)
      real, allocatable :: tmp_t(:,:,:)
      real, allocatable :: tmp_s(:,:,:)
      real*4, allocatable :: XINP(:)
      real*4, allocatable :: YINP(:)
      real*4, allocatable :: ZINP(:)
      real*4, allocatable :: XOUT(:)
      real*4, allocatable :: YOUT(:)
      real*4, allocatable :: ZOUT(:)
      INTEGER :: status      ! Return status
      integer, allocatable :: weightnodes(:,:)  
      real, allocatable :: weights(:,:)       
      integer DIMS(4)

!-----------------------------------------------------------------------
!  read parameters from the Fortran control "Fortran_Nudging.ctl"
!-----------------------------------------------------------------------

      read(5,'(a200)')OFS
      read(5,'(a10)')OCEAN_MODEL
      read(5,'(a200)')DBASE_TS
      read(5,'(a200)')BUFFER
      START_TIME=trim(adjustL(BUFFER))
      read(START_TIME,'(I4,4I2)')IYRS,IMMS,IDDS,IHHS,IMNS

      read(5,'(a200)')BUFFER
      END_TIME=trim(adjustL(BUFFER))
      read(END_TIME,'(I4,4I2)')IYRE,IMME,IDDE,IHHE,IMNE
!      read(5,*)IYRS,IMMS,IDDS,IHHS
!      read(5,*)FORHRS
      read(5,*)IGRD

      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
          if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	    BUFFER(i:I)=' '
	  endif    
      enddo
      GRIDFILE=trim(adjustL(BUFFER))
      print *,'gridfile=',trim(gridfile)

      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
         if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	    BUFFER(i:I)=' '
	 endif    
      enddo
      netcdf_file=trim(adjustL(BUFFER))
      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
          if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	    BUFFER(i:I)=' '
	  endif    
      enddo
      CORMSLOG=trim(adjustL(BUFFER))
      print *,'CORMSLOG=',trim(CORMSLOG)
      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
         if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
            BUFFER(i:I)=' '
	 endif    
      enddo
      BUFFER=trim(adjustL(BUFFER))
      read(BUFFER,'(I4,3i2)')base_date
      read(5,*)minlon
      read(5,*)minlat
      read(5,*)maxlon
      read(5,*)maxlat
      read(5,*)KBm
      if (trim(OCEAN_MODEL) .eq. "ROMS" )then
         read(5,*)THETA_S
         read(5,*)THETA_B
         read(5,*)TCLINE
         read(5,*)NVTRANS   
         read(5,*)NVSTR
      endif  
      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
          if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	    BUFFER(i:I)=' '
	  endif    
      enddo
      CLIM_FILE_LAST=trim(adjustL(BUFFER))
!-----------------------------------------------------------------------
!  calculate base time, start time, and end time in days
!-----------------------------------------------------------------------
      ICORMS=43
      CLOSE(ICORMS)
      OPEN(ICORMS,FILE=trim(CORMSLOG),STATUS='OLD',POSITION='APPEND')
      WRITE(ICORMS,'(a)')'BEGIN SECTION OF GENERATING NUDGING FIELDS' 
      yearb=base_date(1)
      monthb=base_date(2)
      dayb=base_date(3)
      hourb=base_date(4)
      jbase_date=JULIAN(yearb,monthb,dayb,hourb)
      yearb=IYRS
      monthb=IMMS
      dayb=IDDS
      hourb=IHHS  
      jdays=JULIAN(yearb,monthb,dayb,hourb)
      day_start=jdays-jbase_date
      yearb=IYRE
      monthb=IMME
      dayb=IDDE
      hourb=IHHE 
      jdaye=JULIAN(yearb,monthb,dayb,hourb)
      day_end=jdaye-jbase_date
      print *,'base_date= ',base_date
      write(*,*)'domin=',minlon,minlat,maxlon,maxlat
      write(*,*)'model start & end time= ',day_start,day_end
!-----------------------------------------------------------------------
!   reading in ROMS model grid information  
!-----------------------------------------------------------------------
      DO I=1,4
        DIMS(I)=1
      ENDDO	
      IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
      ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
      IF (trim(OCEAN_MODEL) .EQ. "ROMS")THEN
        WRITE(*,*)'Reading ROMS grid file ...',trim(GRIDFILE)
        VNAME='lon_rho'
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
	IROMS=DIMS(1)
	JROMS=DIMS(2)
        NNMODEL=IROMS*JROMS
        ALLOCATE(lonm(IROMS,JROMS) )
        ALLOCATE(latm(IROMS,JROMS) )
        ALLOCATE(angm(IROMS,JROMS) )
        ALLOCATE(maskm(IROMS,JROMS) )
        ALLOCATE(hm(IROMS,JROMS) )
        ALLOCATE(sigma(KBm) )
        ALLOCATE(sigmaw(KBm+1) )
        ALLOCATE(Cs_r(KBm) )
        ALLOCATE(Cs_w(KBm+1) )
        ALLOCATE(ZSIGMA(KBm) )
        ALLOCATE(ZSIGMAW(KBm+1))

        IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO J=1,JROMS
	Do I=1,IROMS
	   lonm(I,J)=TMP4D(I,J,1,1)
	ENDDO
	ENDDO   
        VNAME='lat_rho'
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO J=1,JROMS
	Do I=1,IROMS
	   latm(I,J)=TMP4D(I,J,1,1)
	ENDDO
	ENDDO   
        VNAME='angle'
        ANAME='units'
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO J=1,JROMS
	Do I=1,IROMS
	   angm(I,J)=TMP4D(I,J,1,1)
	ENDDO
	ENDDO   
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
        LL=INDEX(trim(ANAME),'degree')
        IF (LL .GT. 0) THEN
          WRITE(*,*) 'FATAL ERROR: angle units should be in radians!'
          WRITE(ICORMS,'(a)') 'FATAL ERROR: angle units should be in radians!'
          STOP
        ENDIF
        VNAME='mask_rho'
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO J=1,JROMS
	Do I=1,IROMS
	   maskm(I,J)=TMP4D(I,J,1,1)
	ENDDO
	ENDDO   
        VNAME='h'
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO J=1,JROMS
	Do I=1,IROMS
	   hm(I,J)=TMP4D(I,J,1,1)
	ENDDO
	ENDDO   
!        STATUS=NF_CLOSE(NCID_GRD)
!
!-----------------------------------------------------------------------
!  Define S-Curves in domain [-1 < sc < 0] at vertical W- and
!  RHO-points.
!-----------------------------------------------------------------------
!
        ALLOCATE(cff1(KBm+1))
        ALLOCATE(cff2(KBm+1))
        ALLOCATE(cff3(KBm+1))
        cff=1.0/REAL(KBm)
        DO k=1,KBm
          sigmaW(k)=cff*(REAL(k-KBm)-1.0)
          sigma(k)=cff*(REAL(k-KBm)-0.5)
        END DO
          sigmaW(KBm+1)=0.0
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!  Report information about vertical S-levels. K=1 for bottom and K=KBm for surface
!-----------------------------------------------------------------------
      hmin=minval(hm)
      hmax=maxval(hm)
      hc=min(max(hmin,0.0),TCLINE)
      WRITE (*,10)
10    FORMAT (/,' Vertical S-coordinate System: ',/,/,  	      
     &  	  ' level   S-coord',10x,  		
     &  	  'at_hmin  over_slope     at_hmax',/)
      htest=hmin
      ele=0.0
      CALL sigma2Z_ROMS_FIX_new(sigmaW,htest,ele,KBm+1,ZSIGMAW,
     1          hc,theta_s,theta_b,TCline,nvtrans,nvstr)
      cff1=zsigmaw
      htest=0.5*(hmin+hmax)
      CALL sigma2Z_ROMS_FIX_new(sigmaW,htest,ele,KBm+1,ZSIGMAW,
     1          hc,theta_s,theta_b,TCline,nvtrans,nvstr)
      cff2=zsigmaw
      htest=hmax
      CALL sigma2Z_ROMS_FIX_new(sigmaW,htest,ele,KBm+1,ZSIGMAW,
     1          hc,theta_s,theta_b,TCline,nvtrans,nvstr)
      cff3=zsigmaw
      DO K=KBm+1,1,-1
         WRITE (*,15) k,sigmaW(k),cff1(k),cff2(k),cff3(k)
      ENDDO
15    FORMAT (i6,2f12.7,4x,3f12.3)
      write(*,*)'xi_rho & eta_rho of ROMS grid=',IROMS,JROMS
      ENDIF  !OCEAN_MODEL == 'ROMS'

C------------------------------------------------------------------
C     Read and process available NCOM forecast files by the provided start time       
C------------------------------------------------------------------
      IF ( (TRIM(DBASE_TS) .EQ. 'NCOM') 
     & .OR.(TRIM(DBASE_TS) .EQ. 'NCOMeast')
     & .OR.(TRIM(DBASE_TS) .EQ. 'AMSEAS') )THEN
         NCOM_FILE=TRIM(DBASE_TS)//"_FILE"
         IFILE_NCOM=0
         INQUIRE(FILE=trim(NCOM_FILE),EXIST=FEXIST)
         if(FEXIST)THEN
           CLOSE(60)
           OPEN(60,file=trim(NCOM_FILE))
           DO I=1,999
             READ(60,'(a100)',END=333)BUFFER
             FILE_NCOM(I)=trim(adjustL(BUFFER))
             READ(60,'(a100)',END=333)BUFFER
             FILE_NCOM_TIDE(I)=trim(adjustL(BUFFER))
           ENDDO
333        CLOSE(60)
	   IFILE_NCOM=I-1
         ENDIF
         WRITE(*,*)'Total number of NCOM FILES= ',IFILE_NCOM
	 IF(IFILE_NCOM .LT. 1)THEN
	     WRITE(*,*)'No NCOM file is available in this period'
             WRITE(ICORMS,'(a)')'NCOM FILE IS NOT FOUND' 
             WRITE(ICORMS,'(a)')'Use field from previous cycle'
	 ELSE
	 DO I=1,IFILE_NCOM
	    WRITE(*,*)'I=',I,TRIM(FILE_NCOM(I))
	 ENDDO
C-----------------------------------------------------------------------
C     Open and read all NCOM netCDF files available during start time to end time       
C-----------------------------------------------------------------------
         FIN=TRIM(FILE_NCOM(1))
         DO I=1,4
          DIMS(I)=1
         ENDDO	
         VNAME='salinity'
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IM=DIMS(1)
	 JM=DIMS(2)
	 KB=DIMS(3)
	 NT=DIMS(4)
	 WRITE(*,*)'NCOM IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
         IF (ALLOCATED(lon)) DEALLOCATE(lon)
         IF (ALLOCATED(lat)) DEALLOCATE(lat)
         IF (ALLOCATED(depth)) DEALLOCATE(depth)
         IF (ALLOCATED(ts_time)) DEALLOCATE(ts_time)
         ALLOCATE(lon(IM,JM) )
         ALLOCATE(lat(IM,JM) )
         ALLOCATE(depth(KB) )
         ALLOCATE(ts_time(NMAX) )
        DO I=1,4
          DIMS(I)=1
        ENDDO	
	 VNAME='lon'
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
         DO J=1,JM
         DO I=1,IM
            lon(i,j)=TMP4D(i,1,1,1)
 	    if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
         ENDDO
         ENDDO
        DO I=1,4
          DIMS(I)=1
        ENDDO	
	 VNAME='lat'
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
         DO J=1,JM
         DO I=1,IM
            lat(i,j)=TMP4D(J,1,1,1)
         ENDDO
         ENDDO
        DO I=1,4
          DIMS(I)=1
        ENDDO	
	 VNAME='depth'
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
         DO K=1,KB
            depth(K)=TMP4D(K,1,1,1)
         ENDDO
C-----------------------------------------------------------------------
C        select subdomain I and J index        
C-----------------------------------------------------------------------
         IMIN=99999
         IMAX=-9999
         JMIN=99999
         JMAX=-9999
         DO J=1,JM
         DO I=1,IM
           IF(lon(i,j) .GE. minlon .and. lon(i,j) .LE. maxlon
     &    .and. lat(i,j) .GE. minlat .and. lat(i,j) .LE. maxlat)then
             IF(I .LT. IMIN)IMIN=I 	  
             IF(I .GT. IMAX)IMAX=I 	  
             IF(J .LT. JMIN)JMIN=J 	  
             IF(J .GT. JMAX)JMAX=J
	   ENDIF
         ENDDO
         ENDDO	  	  
         ISUB=IMAX-IMIN+1	 
         JSUB=JMAX-JMIN+1	 
         write(*,*)'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX
C-----------------------------------------------------------------------
C allocate sizes of arrays for NCOM products 
C-----------------------------------------------------------------------
         NNSUB=ISUB*JSUB
         IF (ALLOCATED(lonsub)) DEALLOCATE(lonsub)
         IF (ALLOCATED(latsub)) DEALLOCATE(latsub)
         IF (ALLOCATED(temp)) DEALLOCATE(temp)
         IF (ALLOCATED(salt)) DEALLOCATE(salt)
         IF (ALLOCATED(tempave)) DEALLOCATE(tempave)
         IF (ALLOCATED(saltave)) DEALLOCATE(saltave)
         allocate(lonsub(ISUB,JSUB))
         allocate(latsub(ISUB,JSUB))
         allocate(temp(ISUB,JSUB,KB,NT*10))
         allocate(salt(ISUB,JSUB,KB,NT*10))
         allocate(tempave(ISUB,JSUB,KB))
         allocate(saltave(ISUB,JSUB,KB))
         DO J=1,JSUB
         DO I=1,ISUB
            lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
            latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
!	    write(77,'(2F12.4,2I6)')lonsub(i,j),latsub(i,j),I,J
         ENDDO
         ENDDO   
         ICOUNT=1
	 NREC=1
         TS_TIME(1)=-9999.0	 
	 DO IFILE=1,IFILE_NCOM
	   write(*,*)'reading NCOM NetCDF file...',IFILE
           FIN=TRIM(FILE_NCOM(IFILE))
	   VNAME='time'
	   ANAME='units'
           DO I=1,4
             DIMS(I)=1
           ENDDO	
           CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
	   NT=DIMS(1)
           CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
	   ANAME=trim(adjustL(ANAME))
	   LEN=LEN_TRIM(ANAME)
           LL=INDEX(ANAME,'minute')         
	   IF(LL .GT. 0)scale_time=1.0/1440.0
           LL=INDEX(ANAME,'hour')         
	   IF(LL .GT. 0)scale_time=1.0/24.0

	   ANAME='time_origin'
           CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
	   ANAME=trim(adjustL(ANAME))
           write(*,*)'time_origin=',trim(ANAME)
           read(ANAME,80)IYR,IMM,IDD,IHH
80         format(I4,1x,i2,1x,i2,1x,i2)
!           write(*,*)'basetime=',IYR,IMM,IDD,IHH
           yearb=IYR
           monthb=IMM
           dayb=IDD
           hourb=IHH
           jday=JULIAN(yearb,monthb,dayb,hourb)
           CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
           dummy=TMP4D(1,1,1,1)*scale_time+jday-jbase_date
	   N0=0
           DO N=1,NREC
             IF(TS_TIME(N) .GE. DUMMY)THEN
	       N0=N-1 
	       exit 
	     ENDIF  
           ENDDO	
82         ICOUNT=N0
           DO N=1,NT
             TIME_NCOM=TMP4D(N,1,1,1)*scale_time+jday-jbase_date
!             if(TIME_NCOM .GT. TS_TIME(ICOUNT) )THEN
               ICOUNT=ICOUNT+1
	       TS_TIME(ICOUNT)=TIME_NCOM
	       print *,'NCOM time=',ICOUNT,TS_TIME(ICOUNT)
 !            endif 
	   ENDDO   
           write(*,*)'first time= ',dummy,TS_TIME(NREC),NREC,N0
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open NCOM netCDF file of failed'
           ENDIF  
           IF (ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
           ALLOCATE(tmp1d(IM) )
           IF (ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
           ALLOCATE(tmp3d(IM,JM,NT) )
           IF (ALLOCATED(itmp3d)) DEALLOCATE(itmp3d)
           ALLOCATE(itmp3d(IM,JM,NT) )
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(IM,JM,KB,NT) )

           STATUS = NF_INQ_VARID(NCID,'water_temp',IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
           STATUS = NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
	   missvalue=IATT*scale+offset
!	   print *,'scale of temp=',scale,offset,missvalue
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
           ICOUNT=N0
           DO N=1,NT
	     ICOUNT=ICOUNT+1
           DO K=1,KB
           DO J=1,JSUB
           DO I=1,ISUB
            TEMP(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*scale+offset
            if(abs(TEMP(I,J,K,ICOUNT)-missvalue) .LE. 0.001)
     &	    TEMP(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO	
           STATUS = NF_INQ_VARID(NCID,'salinity',IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
           ICOUNT=N0
           DO N=1,NT
	     ICOUNT=ICOUNT+1
           DO K=1,KB
           DO J=1,JSUB
           DO I=1,ISUB
            SALT(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*scale+offset
            if(abs(SALT(I,J,K,ICOUNT)-missvalue) .LE. 0.001)
     &	    SALT(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO
           ENDDO

	   NREC=ICOUNT
           STATUS=NF_CLOSE(NCID)
	   
	 ENDDO !DO IFILE=1,IFILE_NCOM
	 NT=NREC 
	 write(*,*)'number of NCOM data NREC= ',NREC
         ENDIF !IFILE_NCOM > 1
!	 DO N=1,NT
!	  write(*,*)'N=',N,TS_TIME(N)
!	 ENDDO  
! delete all NCOM NetCDF files after reading them	
!	 DO IFILE=1,IFILE_NCOM
!           INQUIRE(FILE=TRIM(FILE_NCOM(IFILE)),EXIST=FEXIST)
!           if(FEXIST)THEN
!             CMD='rm -f '//TRIM(FILE_NCOM(IFILE))
!             call system(trim(CMD))
!	   endif  
!	 ENDDO  
      ENDIF    !DBASE_TS == NCOM
C-----------------------------------------------------------------------
C     Open and read all RTOFS netCDF files available during start time to end time       
C-----------------------------------------------------------------------
      IF ( TRIM(DBASE_TS) .EQ. 'RTOFS')THEN
         NCOM_FILE=TRIM(DBASE_TS)//"_FILE"
!         NCOM_FILE='RTOFS_FILE'
         IFILE_NCOM=0
         INQUIRE(FILE=trim(NCOM_FILE),EXIST=FEXIST)
         IF(FEXIST)THEN
           CLOSE(60)
           OPEN(60,file=trim(NCOM_FILE))
           DO I=1,999
              READ(60,'(a100)',END=335)BUFFER
              FILE_NCOM(I)=trim(adjustL(BUFFER))
           ENDDO
335        CLOSE(60)
	   IFILE_NCOM=I-1
         ENDIF
         WRITE(*,*)'Total number of RTOFS FILES= ',IFILE_NCOM
	 IF(IFILE_NCOM .LT. 1)THEN
	      WRITE(*,*)'No RTOFS file is available in this period'
              WRITE(ICORMS,'(a)')'RTOFS FILE IS NOT FOUND' 
              WRITE(ICORMS,'(a)')'Use field from previous cycle' 
	 ELSE
	 DO I=1,IFILE_NCOM
	    WRITE(*,*)'RTOFS I=',I,TRIM(FILE_NCOM(I))
	 ENDDO
C-----------------------------------------------------------------------
C     Open and read all RTOFS netCDF files available during start time to end time       
C-----------------------------------------------------------------------
         FIN=TRIM(FILE_NCOM(1))
         DO I=1,4
          DIMS(I)=1
         ENDDO	
         VNAME='salinity'
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IM=DIMS(1)
	 JM=DIMS(2)
	 KB=DIMS(3)
	 NT=DIMS(4)
	 WRITE(*,*)'RTOFS IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
         IF (ALLOCATED(lon)) DEALLOCATE(lon)
         IF (ALLOCATED(lat)) DEALLOCATE(lat)
         IF (ALLOCATED(depth)) DEALLOCATE(depth)
         IF (ALLOCATED(ts_time)) DEALLOCATE(ts_time)
         ALLOCATE(lon(IM,JM) )
         ALLOCATE(lat(IM,JM) )
         ALLOCATE(depth(KB) )
         ALLOCATE(ts_time(NMAX) )
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='Longitude'
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         WRITE(*,*)'DIM Number of Longitude ',NDIM
         IF(NDIM .EQ. 2)THEN
           DO J=1,JM
           DO I=1,IM
             lon(i,j)=TMP4D(i,j,1,1)
 	     if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
           ENDDO
           ENDDO
         ELSEIF(NDIM .EQ. 1)THEN
           DO J=1,JM
           DO I=1,IM
             lon(i,j)=TMP4D(i,1,1,1)
 	     if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
           ENDDO
           ENDDO
	 ENDIF  
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='Latitude'
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         WRITE(*,*)'DIM Number of Latitude ',NDIM
         IF(NDIM .EQ. 2)THEN
           DO J=1,JM
           DO I=1,IM
             lat(i,j)=TMP4D(i,j,1,1)
           ENDDO
           ENDDO
         ELSEIF(NDIM .EQ. 1)THEN
           DO J=1,JM
           DO I=1,IM
             lat(i,j)=TMP4D(J,1,1,1)
           ENDDO
           ENDDO
         ENDIF
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='Depth'
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         DO K=1,KB
            depth(K)=TMP4D(K,1,1,1)
         ENDDO
C-----------------------------------------------------------------------
C        select subdomain I and J index        
C-----------------------------------------------------------------------
         IMIN=99999
         IMAX=-9999
         JMIN=99999
         JMAX=-9999
         DO J=1,JM
         DO I=1,IM
           IF(lon(i,j) .GE. minlon .and. lon(i,j) .LE. maxlon
     &    .and. lat(i,j) .GE. minlat .and. lat(i,j) .LE. maxlat)then
             IF(I .LT. IMIN)IMIN=I 	  
             IF(I .GT. IMAX)IMAX=I 	  
             IF(J .LT. JMIN)JMIN=J 	  
             IF(J .GT. JMAX)JMAX=J
	   ENDIF
         ENDDO
         ENDDO	  	  
         ISUB=IMAX-IMIN+1	 
         JSUB=JMAX-JMIN+1	 
         write(*,*)'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX
C-----------------------------------------------------------------------
C allocate sizes of arrays for RTOFS products 
C-----------------------------------------------------------------------
         NNSUB=ISUB*JSUB
         IF (ALLOCATED(lonsub)) DEALLOCATE(lonsub)
         IF (ALLOCATED(latsub)) DEALLOCATE(latsub)
         IF (ALLOCATED(temp)) DEALLOCATE(temp)
         IF (ALLOCATED(salt)) DEALLOCATE(salt)
         IF (ALLOCATED(tempave)) DEALLOCATE(tempave)
         IF (ALLOCATED(saltave)) DEALLOCATE(saltave)
         allocate(lonsub(ISUB,JSUB))
         allocate(latsub(ISUB,JSUB))
         allocate(temp(ISUB,JSUB,KB,NT*40))
         allocate(salt(ISUB,JSUB,KB,NT*40))
         allocate(tempave(ISUB,JSUB,KB))
         allocate(saltave(ISUB,JSUB,KB))
         print *, 'after allocation',size(salt)
         DO J=1,JSUB
         DO I=1,ISUB
            lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
            latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
	    write(79,'(2F12.4,2I6)')lonsub(i,j),latsub(i,j),I,J
         ENDDO
         ENDDO  
	 close(79) 
         ICOUNT=0
	 NREC=0
         TIMELAST=-9999.0	 
	 DO IFILE=1,IFILE_NCOM
	   write(*,*)'reading RTOFS NetCDF file...',IFILE
           FIN=TRIM(FILE_NCOM(IFILE))
	   VNAME='MT'
	   ANAME='units'
           DO I=1,4
             DIMS(I)=1
           ENDDO	
           CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
	   NT=DIMS(1)
           CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
	   ANAME=trim(adjustL(ANAME))
	   LEN=LEN_TRIM(ANAME)
           LL=INDEX(ANAME,'minute')         
	   IF(LL .GT. 0)scale_time=1.0/1440.0
           LL=INDEX(ANAME,'hour')         
	   IF(LL .GT. 0)scale_time=1.0/24.0
           LL=INDEX(ANAME,'day')         
	   IF(LL .GT. 0)scale_time=1.0
           LL=INDEX(ANAME,'since')         
	   IF(LL .GT. 0)then
             read(ANAME(LL+6:LEN),80)IYR,IMM,IDD,IHH
             write(*,*)'basetime=',IYR,IMM,IDD,IHH
	   else
	     write(*,*)'there is error while reading base date'
	     stop
	   endif    
           yearb=IYR
           monthb=IMM
           dayb=IDD
           hourb=IHH
           jday=JULIAN(yearb,monthb,dayb,hourb)
           CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
           TIME_NCOM=TMP4D(1,1,1,1)*scale_time+jday-jbase_date
           IF(TIMELAST .LT. TIME_NCOM )THEN
               ICOUNT=ICOUNT+1
	       TS_TIME(ICOUNT)=TIME_NCOM
	       TIMELAST=TIME_NCOM 
           ENDIF
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open RTOFS netCDF file of failed'
           ENDIF  
           IF (ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
           ALLOCATE(tmp1d(IM) )
           IF (ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
           ALLOCATE(tmp3d(IM,JM,NT) )
           IF (ALLOCATED(itmp3d)) DEALLOCATE(itmp3d)
           ALLOCATE(itmp3d(IM,JM,NT) )
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(IM,JM,KB,NT) )
           STATUS = NF_INQ_VARID(NCID,'temperature',IDVAR)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
           DO N=1,NT
           DO K=1,KB
           DO J=1,JSUB
           DO I=1,ISUB
            TEMP(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
            if(abs(TEMP(I,J,K,ICOUNT)) .GE. 99.0)
     &	    TEMP(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO	
           STATUS = NF_INQ_VARID(NCID,'salinity',IDVAR)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
!           ICOUNT=N0
           DO N=1,NT
!	     ICOUNT=ICOUNT+1
           DO K=1,KB
           DO J=1,JSUB
           DO I=1,ISUB
            SALT(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
            if(abs(SALT(I,J,K,ICOUNT)) .GE. 99.0)
     &	    SALT(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO	

	   NREC=ICOUNT
           STATUS=NF_CLOSE(NCID)
         ENDDO !DO IFILE=1,IFILE_NCOM     
	 NT=NREC 
	 write(*,*)'number of RTOFS data NREC= ',NREC
         ENDIF !IFILE_NCOM >1
      ENDIF

C-----------------------------------------------------------------------
C     Open and read all HYCOM netCDF files available during start time to end time       
C-----------------------------------------------------------------------
      IF ( TRIM(DBASE_TS) .EQ. 'HYCOM')THEN
         NCOM_FILE=TRIM(DBASE_TS)//"_FILE"
!         NCOM_FILE='HYCOM_FILE'
         IFILE_NCOM=0
         INQUIRE(FILE=trim(NCOM_FILE),EXIST=FEXIST)
         IF(FEXIST)THEN
           CLOSE(60)
           OPEN(60,file=trim(NCOM_FILE))
           DO I=1,999
              READ(60,'(a100)',END=325)BUFFER
              FILE_NCOM(I)=trim(adjustL(BUFFER))
           ENDDO
325        CLOSE(60)
	   IFILE_NCOM=I-1
         ENDIF
         WRITE(*,*)'Total number of HYCOM FILES= ',IFILE_NCOM
	 IF(IFILE_NCOM .LT. 1)THEN
	      WRITE(*,*)'No HYCOM file is available in this period'
              WRITE(*,*)'use field from previous cycle'
              WRITE(ICORMS,'(a)')'HYCOM FILE IS NOT FOUND' 
              WRITE(ICORMS,'(a)')'USE field from previous cycle' 
	 ELSE  
	 DO I=1,IFILE_NCOM
	    WRITE(*,*)'HYCOM I=',I,TRIM(FILE_NCOM(I))
	 ENDDO
C-----------------------------------------------------------------------
C     Open and read all HYCOM netCDF files available during start time to end time       
C-----------------------------------------------------------------------
         FIN=TRIM(FILE_NCOM(1))
         DO I=1,4
          DIMS(I)=1
         ENDDO	
         VNAME='salinity'
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IM=DIMS(1)
	 JM=DIMS(2)
	 KB=DIMS(3)
	 NT=DIMS(4)
	 WRITE(*,*)'HYCOM IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
         IF (ALLOCATED(lon)) DEALLOCATE(lon)
         IF (ALLOCATED(lat)) DEALLOCATE(lat)
         IF (ALLOCATED(depth)) DEALLOCATE(depth)
         IF (ALLOCATED(ts_time)) DEALLOCATE(ts_time)
         ALLOCATE(lon(IM,JM) )
         ALLOCATE(lat(IM,JM) )
         ALLOCATE(depth(KB) )
         ALLOCATE(ts_time(NMAX) )
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='lon'
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         WRITE(*,*)'DIM Number of Longitude ',NDIM
         IF(NDIM .EQ. 2)THEN
           DO J=1,JM
           DO I=1,IM
             lon(i,j)=TMP4D(i,j,1,1)
 	     if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
           ENDDO
           ENDDO
         ELSEIF(NDIM .EQ. 1)THEN
           DO J=1,JM
           DO I=1,IM
             lon(i,j)=TMP4D(i,1,1,1)
 	     if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
           ENDDO
           ENDDO
	 ENDIF  
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='lat'
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         WRITE(*,*)'DIM Number of Latitude ',NDIM
         IF(NDIM .EQ. 2)THEN
           DO J=1,JM
           DO I=1,IM
             lat(i,j)=TMP4D(i,j,1,1)
           ENDDO
           ENDDO
         ELSEIF(NDIM .EQ. 1)THEN
           DO J=1,JM
           DO I=1,IM
             lat(i,j)=TMP4D(J,1,1,1)
           ENDDO
           ENDDO
         ENDIF
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='depth'
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         DO K=1,KB
            depth(K)=TMP4D(K,1,1,1)
         ENDDO
!	 CLOSE(79)
!	 OPEN(79,file='lonlat_HYCOM.dat')
!         DO I=1,IM
!         DO J=1,JM
!	    write(79,'(2F12.4,2I6)')lon(i,j),lat(i,j),I,J
!         ENDDO
!         ENDDO  
!	 close(79) 
C-----------------------------------------------------------------------
C        select subdomain I and J index        
C-----------------------------------------------------------------------
         IMIN=99999
         IMAX=-9999
         JMIN=99999
         JMAX=-9999
         DO J=1,JM
         DO I=1,IM
           IF(lon(i,j) .GE. minlon .and. lon(i,j) .LE. maxlon
     &    .and. lat(i,j) .GE. minlat .and. lat(i,j) .LE. maxlat)then
             IF(I .LT. IMIN)IMIN=I 	  
             IF(I .GT. IMAX)IMAX=I 	  
             IF(J .LT. JMIN)JMIN=J 	  
             IF(J .GT. JMAX)JMAX=J
	   ENDIF
         ENDDO
         ENDDO	  	  
         ISUB=IMAX-IMIN+1	 
         JSUB=JMAX-JMIN+1	 
         write(*,*)'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX
C-----------------------------------------------------------------------
C allocate sizes of arrays for HYCOM products 
C-----------------------------------------------------------------------
         NNSUB=ISUB*JSUB
         IF (ALLOCATED(lonsub)) DEALLOCATE(lonsub)
         IF (ALLOCATED(latsub)) DEALLOCATE(latsub)
         IF (ALLOCATED(temp)) DEALLOCATE(temp)
         IF (ALLOCATED(salt)) DEALLOCATE(salt)
         IF (ALLOCATED(tempave)) DEALLOCATE(tempave)
         IF (ALLOCATED(saltave)) DEALLOCATE(saltave)
         allocate(lonsub(ISUB,JSUB))
         allocate(latsub(ISUB,JSUB))
         allocate(temp(ISUB,JSUB,KB,NT*100))
         allocate(salt(ISUB,JSUB,KB,NT*100))
         allocate(tempave(ISUB,JSUB,KB))
         allocate(saltave(ISUB,JSUB,KB))
	 CLOSE(79)
	 OPEN(79,file='lonlat_HYCOM.dat')
         DO J=1,JSUB
         DO I=1,ISUB
            lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
            latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
	    write(79,'(2F12.4,2I6)')lonsub(i,j),latsub(i,j),I,J
         ENDDO
         ENDDO  
	 close(79) 
         ICOUNT=0
	 NREC=0
         TIMELAST=-9999.0	 
	 DO IFILE=1,IFILE_NCOM
	   write(*,*)'reading HYCOM NetCDF file...',IFILE
           FIN=TRIM(FILE_NCOM(IFILE))
	   VNAME='time'
	   ANAME='units'
           DO I=1,4
             DIMS(I)=1
           ENDDO	
           CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
	   NT=DIMS(1)
           CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
	   ANAME=trim(adjustL(ANAME))
	   LEN=LEN_TRIM(ANAME)
           LL=INDEX(ANAME,'minute')         
	   IF(LL .GT. 0)scale_time=1.0/1440.0
           LL=INDEX(ANAME,'hour')         
	   IF(LL .GT. 0)scale_time=1.0/24.0
           LL=INDEX(ANAME,'day')         
	   IF(LL .GT. 0)scale_time=1.0
           LL=INDEX(ANAME,'since')         
	   IF(LL .GT. 0)then
             read(ANAME(LL+6:LEN),80)IYR,IMM,IDD,IHH
             write(*,*)'basetime=',IYR,IMM,IDD,IHH
	   else
	     write(*,*)'there is error while reading base date'
	     stop
	   endif    
           yearb=IYR
           monthb=IMM
           dayb=IDD
           hourb=IHH
           jday=JULIAN(yearb,monthb,dayb,hourb)
           CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
           TIME_NCOM=TMP4D(1,1,1,1)*scale_time+jday-jbase_date
           IF(TIMELAST .LT. TIME_NCOM )THEN
               ICOUNT=ICOUNT+1
	       TS_TIME(ICOUNT)=TIME_NCOM
	       TIMELAST=TIME_NCOM 
           ENDIF

           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open HYCOM netCDF file of failed'
           ENDIF  
           IF (ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
           ALLOCATE(tmp1d(IM) )
           IF (ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
           ALLOCATE(tmp3d(IM,JM,NT) )
           IF (ALLOCATED(itmp3d)) DEALLOCATE(itmp3d)
           ALLOCATE(itmp3d(IM,JM,NT) )
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(IM,JM,KB,NT) )

           STATUS = NF_INQ_VARID(NCID,'water_temp',IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
           STATUS = NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
	   missvalue=IATT*scale+offset
	   print *,'scale of temp=',scale,offset,missvalue
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
           DO N=1,NT
           DO K=1,KB
           DO J=1,JSUB
           DO I=1,ISUB
            TEMP(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*scale+offset
            if(abs(TEMP(I,J,K,ICOUNT)-missvalue) .LE. 0.001)
     &	    TEMP(I,J,K,ICOUNT)=-99999.9
            if(TEMP(I,J,K,ICOUNT) .LT. 0.0)TEMP(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO
           STATUS = NF_INQ_VARID(NCID,'salinity',IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
           DO N=1,NT
           DO K=1,KB
           DO J=1,JSUB
           DO I=1,ISUB
            SALT(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*scale+offset
            if(abs(SALT(I,J,K,ICOUNT)-missvalue) .LE. 0.001)
     &	    SALT(I,J,K,ICOUNT)=-99999.9
            if(SALT(I,J,K,ICOUNT) .LT. 0.0)SALT(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO	

	   NREC=ICOUNT
           STATUS=NF_CLOSE(NCID)
         ENDDO      
	 NT=NREC 
	 write(*,*)'number of HYCOM data NREC= ',NREC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         CLOSE(79)
	 OPEN(79,file='salt_HYCOM.dat')
         DO N=1,1   !NT
         DO J=1,JSUB
         DO I=1,ISUB
	  IF (SALT(I,J,1,N) .GE. 0.0)THEN
            WRITE(79,'(F10.4,2I5,100F9.3)')TS_TIME(N),I,J,
     &	   lonsub(i,j),latsub(i,j),(SALT(I,J,K,N),K=1,KB)
          ENDIF
         ENDDO	
         ENDDO	
         ENDDO	
	 
         CLOSE(79)
	 OPEN(79,file='temp_HYCOM.dat')
         DO N=1,1 !NT
         DO J=1,JSUB
         DO I=1,ISUB
	  IF (TEMP(I,J,1,N) .GE. 0.0)THEN
            WRITE(79,'(F10.4,2I5,100F9.3)')TS_TIME(N),I,J,
     &	   lonsub(i,j),latsub(i,j),(TEMP(I,J,K,N),K=1,KB)
          ENDIF
         ENDDO	
         ENDDO	
         ENDDO
         CLOSE(79)
         ENDIF !IFILE_NCOM >1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	   	
      ENDIF

C-----------------------------------------------------------------------
C  End of reading date from  NCOM, or RTOFS, or HYCOM products 
C-----------------------------------------------------------------------

      IF (NT .GT. 1 .AND. TS_TIME(NT)-TS_TIME(1) .LT. 2.0) THEN
        write(*,*)' time period is less than 4 days'
        WRITE(ICORMS,'(a)')' time period is less than 4 days'
        WRITE(ICORMS,'(a)')'Generation of nudging fields failed'
        WRITE(ICORMS,'(a)')'stop in nos_ofs_create_nudging_field.f'
        stop 'stop in nos_ofs_create_nudging_field.f'
      ENDIF
c     calculate the time average of T/S fields
      TEMPAVE=SUM(TEMP,4)/NT
      SALTAVE=SUM(SALT,4)/NT
      KMAX=KB	
      K=1
      DO WHILE ( (depth(k) .LT. hmax) .AND. (K .LT. KB) )
         K=K+1
      ENDDO
      KMAX=K
      WRITE(*,*)'MAX Vertical Levels needed is ',KMAX,depth(KMAX),hmax

C-----------------------------------------------------------------------
C    begin to process nudging fields  
C-----------------------------------------------------------------------
      NTm=2 ! only write out start time and end time; can be modified later
            ! for specified time interval
      ALLOCATE(TIMEM(NTm))
      TIMEM(1)=day_start
      TIMEM(NTm)=day_end
      IF (ALLOCATED(tmp_t)) DEALLOCATE(tmp_t)
      IF (ALLOCATED(tmp_s)) DEALLOCATE(tmp_s)
      ALLOCATE(tmp_t(IROMS,JROMS,KMAX) )
      ALLOCATE(tmp_s(IROMS,JROMS,KMAX) )
      tmp_t=-99999.99
      tmp_s=-99999.99

      IF (ALLOCATED(tempm)) DEALLOCATE(tempm)
      IF (ALLOCATED(saltm)) DEALLOCATE(saltm)
      ALLOCATE(tempm(IROMS,JROMS,KBm,NTm) )
      ALLOCATE(saltm(IROMS,JROMS,KBm,NTm) )
      saltm=-99999.99
      tempm=-99999.99

      IF( IGRD .EQ. 2 .OR. IGRD .EQ. 3)THEN
	IF (ALLOCATED(Iout)) DEALLOCATE(Iout)
	IF (ALLOCATED(Jout)) DEALLOCATE(Jout)
	IF (ALLOCATED(outm)) DEALLOCATE(outm)
	IF (ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
        allocate(Iout(IROMS,JROMS))
        allocate(Jout(IROMS,JROMS))
        allocate(outm(IROMS,JROMS))
        allocate(TMP2D(ISUB,JSUB))
        if( IGRD .eq. 2)iflag=1
        if( IGRD .eq. 3)iflag=0
        CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &        IROMS,JROMS,lonm,latm,outm,Iout,Jout,0)
        print *,'Search coarser grid indeces directions are done!!!'
      ENDIF 
      IF( IGRD .EQ. 1 .OR. IGRD .EQ. 4)THEN
         IF (ALLOCATED(XINP)) DEALLOCATE(XINP)
         IF (ALLOCATED(YINP)) DEALLOCATE(YINP)
         IF (ALLOCATED(ZINP)) DEALLOCATE(ZINP)
         IF (ALLOCATED(XOUT)) DEALLOCATE(XOUT)
         IF (ALLOCATED(YOUT)) DEALLOCATE(YOUT)
         IF (ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
         IF (ALLOCATED(weightnodes)) DEALLOCATE(weightnodes)
         IF (ALLOCATED(weights)) DEALLOCATE(weights)
         IF (ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
         allocate(XINP(NNSUB) )
         allocate(YINP(NNSUB) )
         allocate(ZINP(NNSUB) )
         allocate(XOUT(NNMODEL) )
         allocate(YOUT(NNMODEL) )
         allocate(ZOUT(NNMODEL) )
         allocate(weightnodes(NNMODEL,3))
         allocate(weights(NNMODEL,3))
         allocate(TMP2D(IROMS,JROMS))
         CALL TWO2ONED(XOUT,lonm,IROMS,JROMS)
         CALL TWO2ONED(YOUT,latm,IROMS,JROMS)
         call search_output(ISUB,JSUB,lonsub,latsub,NNMODEL,XOUT,
     &    YOUT,nselect_parent,IPARENT,JPARENT)
         close(45)
         open(45,file='selected.dat')
         DO I=1,nselect_parent
           WRITE(45,4444)I,IPARENT(i),JPARENT(I),lonsub(IPARENT(i),JPARENT(i)),latsub(IPARENT(i),JPARENT(i))
         ENDDO
         close(45)

         open(45,file='subdomain.dat')
         DO J=1,JSUB
         DO I=1,ISUB
           WRITE(45,'(2I8,2x,2F12.4)')I,J,lonsub(I,J),latsub(I,J)
         ENDDO
         ENDDO
         close(45)
4444     format(3I6,2F12.4)
      ENDIF

c------------------------------------------------------------------
c------------------------------------------------------------------
C  processing Temperature and salinity
c------------------------------------------------------------------
300   CONTINUE
      DO K=1, KMAX
         IF (IGRD .EQ. 1 .OR. IGRD .EQ. 4)THEN  
             NDUM=0
             DO NS=1,NSELECT_PARENT
               IF( abs(TEMPAVE(IPARENT(NS),JPARENT(NS),K) ) .LE. 90.0 )THEN
                     NDUM=NDUM+1
	             XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                     YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
	             ZINP(ndum)=tempave(IPARENT(NS),JPARENT(NS),K)
               ENDIF	
             ENDDO	
	     NDATA=NDUM
	     print *,'NDATA= ',NDATA, 'at k',K,depth(K)
    	     IF (IGRD .EQ. 1 .AND. NDATA .GT. 3)THEN                                  
                write(*,*)'COMPUTE WEIGHTS AND NODES FOR REMESH !!!'
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     *          NNMODEL,XOUT,YOUT,ZOUT,weightnodes,weights,0)
             ENDIF
         ENDIF 

             IF (IGRD .EQ. 2 .or. IGRD .EQ. 3)THEN   !! spatial interpolation using bicubic or bilinear
                DO J=1,JSUB
                DO I=1,ISUB
                     tmp2d(I,J)=TEMPAVE(I,J,K)
                ENDDO
                ENDDO
                print *,'k=',K, size(tempave)
                if( IGRD .eq. 2)iflag=1
                if( IGRD .eq. 3)iflag=0
                CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &          IROMS,JROMS,lonm,latm,outm,Iout,Jout,1)
                DO J=1,JROMS
                DO I=1,IROMS
                     tmp_t(I,J,K)=outm(I,J)
                ENDDO
                ENDDO

                DO J=1,JSUB
                DO I=1,ISUB
                     tmp2d(I,J)=SALTAVE(I,J,K)
                ENDDO
                ENDDO
                CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &          IROMS,JROMS,lonm,latm,outm,Iout,Jout,1)
                DO J=1,JROMS
                DO I=1,IROMS
                   tmp_s(I,J,K)=outm(I,J)
                ENDDO
                ENDDO
  	     ELSEIF (IGRD .EQ. 1 .OR. IGRD .EQ. 4 )THEN   !! using remesh routine
                NDUM=0                                     !! nature neighbors spatial interpolation 
                DO NS=1,NSELECT_PARENT
                  IF( abs(tempave(IPARENT(NS),JPARENT(NS),K) ) .LE. 90.0 )THEN
                     NDUM=NDUM+1
	             XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                     YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
	             ZINP(ndum)=tempave(IPARENT(NS),JPARENT(NS),K)
                  ENDIF	
                ENDDO	
                IF (NDUM .GT. 3)THEN
                  IF(NDUM .NE. NDATA)THEN
                     WRITE(*,*)'NDUM of TEMP= ',NDUM,NDATA
	             WRITE(*,*)'NDATA is not equal to NDUM in Temp!'
	    	     NDATA=NDUM	
  	             IF (IGRD .EQ. 1 )                                  
     &               call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &               NNMODEL,XOUT,YOUT,ZOUT,weightnodes,weights,0)
	          ENDIF   
  	          IF (IGRD .EQ. 1 )THEN                                  !! using remesh
                     call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &               NNMODEL,XOUT,YOUT,ZOUT,weightnodes,weights,1)
    	          ELSEIF (IGRD .EQ. 4)THEN                               !! using nature neighbors
                     call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &               ,ZOUT)
                  ENDIF
                  CALL ONE2TWOD(ZOUT,TMP2D,IROMS,JROMS)
                  DO J=1,JROMS
                  DO I=1,IROMS
                     tmp_t(I,J,K)=TMP2D(I,J)
                  ENDDO
                  ENDDO
                ENDIF
                NDUM=0
                DO NS=1,NSELECT_PARENT
                  IF( abs(saltave(IPARENT(NS),JPARENT(NS),K) ) .LE. 90.0 )THEN
                     NDUM=NDUM+1
                     XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                     YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
                     ZINP(ndum)=saltave(IPARENT(NS),JPARENT(NS),K)
                  ENDIF
                ENDDO
                IF (NDUM .GT. 3)THEN
                  IF(NDUM .NE. NDATA)THEN
                     WRITE(*,*)'NDUM of SALT= ',NDUM,NDATA
	             WRITE(*,*)'NDATA is not equal to NDUM in Salt!'
	    	     NDATA=NDUM	
  	             IF (IGRD .EQ. 1 )                                  
     &               call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &               NNMODEL,XOUT,YOUT,ZOUT,weightnodes,weights,0)
	          ENDIF   
  	          IF (IGRD .EQ. 1 )THEN                                  !! using remesh
                     call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &               NNMODEL,XOUT,YOUT,ZOUT,weightnodes,weights,1)
    	          ELSEIF (IGRD .EQ. 4)THEN                               !! using nature neighbors
                     call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &               NNMODEL,XOUT,YOUT,ZOUT)
                  ENDIF
                  CALL ONE2TWOD(ZOUT,TMP2D,IROMS,JROMS)
                  DO J=1,JROMS   
                  DO I=1,IROMS
                     tmp_s(I,J,K)=TMP2D(I,J)
                  ENDDO
                  ENDDO
                ENDIF
             ENDIF
      ENDDO                 !! end loop of K
!      print *, 'done with regridding T/S',tmp_t(IROMS,JROMS,KB)
c----------------------------------------------------------------
C If surface layer water cells have invalid value fill from nearby
C coastal cells
c-----------------------------------------------------------------
      ALLOCATE(maskm_e(IROMS+2,JROMS+2))
      ALLOCATE(IFILL(NNMODEL/10))
      ALLOCATE(JFILL(NNMODEL/10))
      ALLOCATE(ISHORE(NNMODEL/50))
      ALLOCATE(JSHORE(NNMODEL/50))
C  Horizontally fill all water cells
        K=1   ! RTOFS surface
        maskm_e=0
        NDUM=0
        DO J=1,JROMS   !!mask water cell with no valid value from interpolation
        DO I=1,IROMS
           IF (maskm(I,J) .GT. 0.5 .AND.
     &        (tmp_t(I,J,K) .LT. -90.0 .OR. ISNAN(tmp_t(I,J,K))) ) THEN
               maskm_e(I+1,J+1)=0
               NDUM=NDUM+1
               IFILL(NDUM)=I
               JFILL(NDUM)=J
           ELSE
               maskm_e(I+1,J+1)=maskm(I,J)
           ENDIF
        ENDDO
        ENDDO
        print *, 'Number of cells to be filled', NDUM
        DO I=2,IROMS+1
           maskm_e(I,1)=maskm_e(I,2)
           maskm_e(I,JROMS+2)=maskm_e(I,JROMS+1)
        ENDDO
        DO J=1,JROMS+2
           maskm_e(1,J)=maskm_e(2,J)
           maskm_e(IROMS+2,J)=maskm_e(IROMS+1,J)
        ENDDO
        IF (NDUM .GT. 0) THEN
        NSHORE=0
        DO J=2,JROMS+1
        DO I=2,IROMS+1
           s=maskm_e(I-1,J-1)+maskm_e(I,J-1)+maskm_e(I+1,J-1)
     &      +maskm_e(I-1,J  )+maskm_e(I,J  )+maskm_e(I+1,J  )
     &      +maskm_e(I-1,J+1)+maskm_e(I,J+1)+maskm_e(I+1,J+1)
           if (maskm_e (I,J) .EQ. 1 .AND. s .LT. 9 ) then !.AND. 
!     &        tmp_t(I-1,J-1,K) .GT. -90.0 ) then
              NSHORE=NSHORE+1
              ISHORE(NSHORE)=I-1
              JSHORE(NSHORE)=J-1
           endif
        ENDDO
        ENDDO
        open(45,file='ShorePoints.dat')
        DO I=1,NSHORE
          write(45,*) ISHORE(I),JSHORE(I)
        ENDDO
        close(45)
        DO N=1,NDUM
           DISTMIN=1.0E20
           DO N1=1,NSHORE
              DIST=(lonm(ISHORE(N1),JSHORE(N1))-lonm(IFILL(N),JFILL(N)))**2
     &            +(latm(ISHORE(N1),JSHORE(N1))-latm(IFILL(N),JFILL(N)))**2
              IF (DIST < DISTMIN) THEN
                 DISTMIN=DIST
                 NS=N1
              ENDIF
           ENDDO
           DO K=1,KB
             tmp_t(IFILL(N),JFILL(N),K)=tmp_t(ISHORE(NS),JSHORE(NS),K)
             tmp_s(IFILL(N),JFILL(N),K)=tmp_s(ISHORE(NS),JSHORE(NS),K)
           END DO
           write(6,*) N, IFILL(N), JFILL(N), ISHORE(NS), JSHORE(NS)
        ENDDO
        ENDIF  !end of extrapolation
        print *,'tmp_t before vert', (tmp_t(1,1,K),K=1,KMAX)
        print *,(tmp_s(1,1,K),K=1,KMAX)
c------------------------------------------------------------------
C  vertically interpolating to ROMS sigma coordinate from z-coordinate of WOA05 or NCOM
C  K=1 for bottom and K=KBm for surface
c------------------------------------------------------------------
      IF (ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF (ALLOCATED(oned2)) DEALLOCATE(oned2)
      allocate(oned1(KB) )
      allocate(oned2(KB) )
      hmin=minval(hm)
      hmax=maxval(hm)
      hc=min(max(hmin,0.0),TCLINE)

      DO J=1,JROMS
      DO I=1,IROMS
         IF (maskm(I,J) > 0.5) THEN
            KMAX1=0
            DO K=1,KMAX
               IF (ABS(TMP_T(I,J,K)) .LE. 90.0 .AND.
     &             ABS(TMP_S(I,J,K)) .LE. 90.0) THEN
                 KMAX1=KMAX1+1     
	         oned1(kMAX1)=TMP_T(I,J,K)
	         oned2(kmax1)=TMP_S(I,J,K)
               ENDIF
            ENDDO	
            ele=0
             CALL sigma2Z_ROMS_FIX_new(sigma,hm(I,J),ele,KBm,
     1            ZSIGMA,hc,theta_s,theta_b,TCline,nvtrans,nvstr)

            DO K=KBm,1,-1  ! from surface to bottom
	       sdepth=ZSIGMA(K)
               IF (sdepth .LE. depth(1) )THEN
                   UTMP=oned1(1)
                   VTMP=oned2(1)
               ELSEIF(sdepth .GE. depth(KMAX1) )THEN
                   UTMP=oned1(KMAX1)
                   VTMP=oned2(KMAX1)
               ELSE
                   DO K9=1,KMAX1-1
                        IF( (sdepth .GE. depth(K9)) .and.
     1                  (sdepth .LT. depth(K9+1)) ) exit  
                   ENDDO
400                X1=depth(K9)
                   X2=depth(K9+1)
                   Y1=oned1(K9)
                   Y2=oned1(K9+1)
                   call linear(X1,Y1,X2,Y2,sdepth,UTMP)
                   Y1=oned2(K9)
                   Y2=oned2(K9+1)
                   call linear(X1,Y1,X2,Y2,sdepth,VTMP)
	       ENDIF 
	       TEMPM(I,J,K,NTm)=UTMP
	       SALTM(I,J,K,NTm)=VTMP   
               IF (K .EQ. KBm) THEN
                 IF(IEEE_IS_NAN(UTMP)) THEN
                    write(6,*) "Fatal Error: surface value is NaN!",I,N
                    STOP
                 END IF
               ELSE
                  IF (IEEE_IS_NAN(UTMP))
     1                TEMPM(I,J,K,NTm)=TEMPM(I,J,K+1,NTm)
                  IF (IEEE_IS_NAN(UTMP))
     1                SALTM(I,J,K,NTm)=SALTM(I,J,K+1,NTm)
               END IF
	    ENDDO
         END IF
      ENDDO
      ENDDO
      print*,'after vert', (tempm(1,1,K,NTm),K=1,KBm)
c------------------------------------------------------------------
C  end of vertical interpolation onto model vertical coordinate
c------------------------------------------------------------------
C  deallocate unused arrays
c------------------------------------------------------------------
      IF (ALLOCATED(lon)) DEALLOCATE(lon)
      IF (ALLOCATED(lat)) DEALLOCATE(lat)
      IF (ALLOCATED(lonsub)) DEALLOCATE(lonsub)
      IF (ALLOCATED(latsub)) DEALLOCATE(latsub)
      IF (ALLOCATED(masksub)) DEALLOCATE(masksub)
      IF (ALLOCATED(depth)) DEALLOCATE(depth)
      IF (ALLOCATED(temp)) DEALLOCATE(temp)
      IF (ALLOCATED(salt)) DEALLOCATE(salt)
      IF (ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
      IF (ALLOCATED(tmp2d)) DEALLOCATE(tmp2d)
      IF (ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
      IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
      IF (ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF (ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF (ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF (ALLOCATED(oned4)) DEALLOCATE(oned4)

!-----------------------------------------------------------------------
C  Set global attributes string of the NetCDF
!-----------------------------------------------------------------------
1     format(I2.2,a1,I2.2,2x,I2.2,a1,i2.2,a1,I4)
      CALL DATE_AND_TIME(BIG_BEN(1),BIG_BEN(2),BIG_BEN(3),DATE_TIME)
      WRITE(CURRENT_TIME,1)DATE_TIME(5),':',DATE_TIME(6),
     &DATE_TIME(2),'/',DATE_TIME(3),'/',DATE_TIME(1)
      globalstr(1)= trim(OCEAN_MODEL)
     & //' nudging netCDF file'
      globalstr(2)= trim(OFS)//' nudging netCDF file'
      globalstr(3)= 'T and S from '//trim(DBASE_TS)
      if(IGRD  .EQ. 1)then
        globalstr(4)= 'On '//trim(OCEAN_MODEL)
     & //' grid, using remesh spatial interpolation'
      elseif(IGRD  .EQ. 2)then
        globalstr(4)= 'On '//trim(OCEAN_MODEL)
     & //' grid, using bicubic spatial interpolation'
      elseif(IGRD  .EQ. 3)then
        globalstr(4)= 'On '//trim(OCEAN_MODEL)
     & //' grid, using bilinear spatial interpolation'
      elseif(IGRD  .EQ. 4)then
        globalstr(4)= 'On '//trim(OCEAN_MODEL)
     & //' grid, using nature neighbors spatial interpolation'
      endif
      globalstr(5)= 'GRID file: '//trim(GRIDFILE)
      
      globalstr(6)= 'Created at time '//trim(CURRENT_TIME)
      globalstr(7)='Created by Aijun Zhang, OD/CO-OPS/NOS/NOAA'
!      DO i=1,9
!      write(*,'(a80)')adjustL(trim(globalstr(I) ))
!      enddo	  
 !-----------------------------------------------------------------------

      call write_netCDF_nudging_fields_ROMS(netcdf_file,ncid,1, 
     & IROMS,JROMS,KBm,NTm,base_date,
     & timem,tempm,saltm,globalstr)

!!!! read variables from the previous cycle's nudging file
!      read(START_TIME,'(I4,4I2)')IYRS,IMMS,IDDS,IHHS,IMNS 
!      yearb=IYRS
!      monthb=IMMS
!      dayb=IDDS
!      hourb=IHHS    
!      jdays=JULIAN(yearb,monthb,dayb,hourb)
!      day_start=jdays-jbase_date
      FIN=trim(CLIM_FILE_LAST)
      INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
      IF(FEXIST)THEN
        WRITE(*,*) 'Read previous cycle clim file '//trim(FIN)
        VNAME='ocean_time'
        ANAME='units'
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF (ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
        IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp1d(DIMS(1)) )
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
        ANAME=trim(adjustL(ANAME))
        LEN=LEN_TRIM(ANAME)
        LL=INDEX(ANAME,'minute')         
        IF(LL .GT. 0)scale_time=1.0/1440.0
        LL=INDEX(ANAME,'hour')         
        IF(LL .GT. 0)scale_time=1.0/24.0
        LL=INDEX(ANAME,'day')         
        IF(LL .GT. 0)scale_time=1.0
        LL=INDEX(ANAME,'since')         
        IF(LL .GT. 0)then
         read(ANAME(LL+6:LEN),80)IYR,IMM,IDD,IHH
         write(*,*)'basetime=',IYR,IMM,IDD,IHH
        ELSE
	 write(*,*)'there is error while reading base date'
	 stop
        ENDIF    
        yearb=IYR
        monthb=IMM
        dayb=IDD
        hourb=IHH
        jday=JULIAN(yearb,monthb,dayb,hourb)
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO N=1,DIMS(1)
          tmp1d(N)=TMP4D(N,1,1,1)*scale_time+jday-jbase_date
        ENDDO
        IF (day_start < tmp1d(1) .OR. day_start > tmp1d(DIMS(1))) THEN
           WRITE(*,*) 'previous cycle running period doesnot include day_start'

           WRITE(ICORM,'(a)') 'previous running period doesnot include day_start'
           WRITE(ICORM,'(a)') 'CRITICAL FAILURE IN CREATING NUDGING FIELDS'
           STOP
        ENDIF
        N=1 
        DO while (tmp1d(N) .lt. day_start )
          N=N+1
        ENDDO
        NSELECT=N
        print *,'NSELECT =',NSELECT

        IF ( NSELECT .GT. 0 .AND. NSELECT .LE. DIMS(1) )THEN
        cff=(day_start-tmp1d(NSELECT-1))/(tmp1d(NSELECT)-tmp1d(NSELECT-1))
          VNAME='temp'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
          IF(STATUS .EQ. NF_NOERR)then
         
            IF (ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)) )
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
            IF(STATUS .EQ. NF_NOERR)then
              DO K=1,DIMS(3)
              DO J=1,DIMS(2)
              DO I=1,DIMS(1)
                TEMPM(I,J,K,1)=TMP4D(I,J,K,NSELECT-1)*cff+
     &                        TMP4D(I,J,K,NSELECT)*(1-cff)
              ENDDO
              ENDDO
              ENDDO
            ENDIF
          ENDIF
          VNAME='salt'
          DO I=1,4
            DIMS(I)=1
          ENDDO
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
          IF(STATUS .EQ. NF_NOERR)then

            IF (ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)) )
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
            IF(STATUS .EQ. NF_NOERR)then
              DO K=1,DIMS(3)
              DO J=1,DIMS(2)
              DO I=1,DIMS(1)
                SALTM(I,J,K,1)=TMP4D(I,J,K,NSELECT-1)*cff+
     &                        TMP4D(I,J,K,NSELECT)*(1-cff)
              ENDDO
              ENDDO
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ELSE
        write(*,*) 'No previous cycle data. Using the average'
        DO K=1,KBm
        DO J=1,JROMS
        DO I=1,IROMS
          TEMPM(I,J,K,1)=TEMPM(I,J,K,2)
          SALTM(I,J,K,1)=SALTM(I,J,K,2)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!!!! AJ
      write(*,*) 'size of tempm', size(tempm),size(saltm),size(timem)
      open(45,file='clim_TS.dat')
      write(45, '(8f10.2)') (((tempm(I,J,K,1),I=1,IROMS),J=1,JROMS),K=1,KBm)
      write(45, '(8f10.2)') (((saltm(I,J,K,1),I=1,IROMS),J=1,JROMS),K=1,KBm)
      close(45)

      call write_netCDF_nudging_fields_ROMS(netcdf_file,ncid,2,
     & IROMS,JROMS,KBm,NTm,base_date,
     & timem,tempm,saltm,globalstr)


      call write_netCDF_nudging_fields_ROMS(netcdf_file,ncid,3,
     & IROMS,JROMS,KBm,NTm,base_date,
     & timem,tempm,saltm,globalstr)

      write(*,*)'Nudging file is COMPLETED SUCCESSFULLY'

      WRITE(ICORMS,'(a)')'END SECTION OF GENERATING NUDGING FIELDS' 
      CLOSE(ICORMS)
      
      STOP
      END
     
      
      SUBROUTINE READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIMS,DIMS,TMP4D,ATT,MODE)
C -------------------------------------------------------------------
C  This Fortran subroutine is to read a variable or an attribute of 
C  a variable in a NetCDF file
C  Mode =0 : read dimension sizes of a variable
C  mode =1:  read a real variable
C  mode =2:  read an integer variable, but return a real variable
C  mode =3:  read a string variable, return with VNAME
C  mode =4:  read real type attribute of a variable, return with ATT
C  mode =5:  read an integer type attribute of a variable, return with ATT
C  mode =6:  read a string type attribute of a variable, return with ANAME      
C  mode =7:  read a double precision variable

C -------------------------------------------------------------------
      include 'netcdf.inc'
      character*200 FIN,VNAME,ANAME,BUFFER
      INTEGER DIMS(4),MODE,dimids(5),COUNT(4),STATUS
      REAL TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) )
      LOGICAL FEXIST
      integer, allocatable :: ITMP4D(:,:,:,:)
      REAL*8, allocatable :: DTMP4D(:,:,:,:)
      STATUS=0
      IF (MODE .EQ. 0)THEN
         DO I=1,4
            DIMS(I)=1
         ENDDO
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
           STATUS=-99
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     print *,'required variable ',trim(VNAME), ' is not found'
!	     stop
             ITMP=STATUS
             STATUS=NF_CLOSE(NCID)
             STATUS=ITMP
             RETURN
           ENDIF  

           STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
           status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
           do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
             write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
           enddo
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ELSEIF (MODE .EQ. 1)THEN
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
          STATUS=-99
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     print *,'required variable ',trim(VNAME), ' is not found'
             ITMP=STATUS
             STATUS=NF_CLOSE(NCID)
             STATUS=ITMP
             RETURN
!	     stop
           ENDIF  

           STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,ndims)
           status =NF_INQ_VARDIMID(NCID,IDVAR,dimids)
           do i=1,ndims
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),COUNT(i))
             IF (COUNT(i) .NE. DIMS(I) )THEN
	       WRITE(*,*)'Dimension of array does not match' 
               write(*,*) TRIM(VNAME),' dim ',i,' = ',COUNT(i)
               write(*,*)'DIMS(',I,')= ',DIMS(I),ndims
	     ENDIF  
           enddo
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
           STATUS=NF_CLOSE(NCID)
         ENDIF

      ELSEIF (MODE .EQ. 7)THEN
         allocate(DTMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) ))
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
          STATUS=-99
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     print *,'required variable ',trim(VNAME), ' is not found'
             ITMP=STATUS
             STATUS=NF_CLOSE(NCID)
             STATUS=ITMP
             RETURN
!	     stop
           ENDIF  
           STATUS = NF_GET_VAR_DOUBLE(NCID,IDVAR,DTMP4D)
           DO I1=1,DIMS(1)
           DO I2=1,DIMS(2)
           DO I3=1,DIMS(3)
           DO I4=1,DIMS(4)
	    TMP4D(I1,I2,I3,I4)=DTMP4D(I1,I2,I3,I4)
	   ENDDO
	   ENDDO
	   ENDDO
	   ENDDO
           IF (ALLOCATED(Dtmp4d)) DEALLOCATE(Dtmp4d)
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ELSEIF (MODE .EQ. 2)THEN
         IF (ALLOCATED(itmp4d)) DEALLOCATE(itmp4d)
         allocate(ITMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) ))
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
          STATUS=-99
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     print *,'required variable ',trim(VNAME), ' is not found'
!	     stop
             ITMP=STATUS
             STATUS=NF_CLOSE(NCID)
             STATUS=ITMP
             RETURN
             
           ENDIF  
           STATUS = NF_GET_VAR_INT(NCID,IDVAR,ITMP4D)
	   DO I=1,DIMS(1)
	   DO J=1,DIMS(2)
	   DO K=1,DIMS(3)
	   DO N=1,DIMS(4)
	     TMP4D(I,j,k,N)=ITMP4D(I,J,K,N)
	   ENDDO
	   ENDDO
	   ENDDO
	   ENDDO  
           IF (ALLOCATED(itmp4d)) DEALLOCATE(itmp4d)
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ELSEIF (MODE .EQ. 3)THEN
         allocate(ITMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) ))
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
          STATUS=-99
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     print *,'required variable ',trim(VNAME), ' is not found'
!	     stop
             ITMP=STATUS
             STATUS=NF_CLOSE(NCID)
             STATUS=ITMP
             RETURN
             
           ENDIF  
           STATUS = NF_GET_VAR_TEXT(NCID,IDVAR,BUFFER)
	   VNAME=TRIM(BUFFER)
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ELSEIF (MODE .EQ. 4)THEN
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
          STATUS=-99
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     print *,'required variable ',trim(VNAME), ' is not found'
!	     stop
             ITMP=STATUS
             STATUS=NF_CLOSE(NCID)
             STATUS=ITMP
             RETURN
             
           ENDIF  
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,TRIM(ANAME),ATT)
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ELSEIF (MODE .EQ. 5)THEN
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
          STATUS=-99
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     print *,'required variable ',trim(VNAME), ' is not found'
!	     stop
             ITMP=STATUS
             STATUS=NF_CLOSE(NCID)
             STATUS=ITMP
             RETURN
             
           ENDIF  
           STATUS = NF_GET_ATT_INT(NCID,IDVAR,TRIM(ANAME),IATT)
	   ATT=IATT
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ELSEIF (MODE .EQ. 6)THEN
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
          STATUS=-99
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     print *,'required variable ',trim(VNAME), ' is not found'
!	     stop
             ITMP=STATUS
             STATUS=NF_CLOSE(NCID)
             STATUS=ITMP
             RETURN
             
           ENDIF  
           STATUS = NF_GET_ATT_TEXT(NCID,IDVAR,TRIM(ANAME),BUFFER)
	   ANAME=TRIM(BUFFER)
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ENDIF	
      RETURN
      END

c  Search RTOFS grid points which include all the OB locations
c
c  Input:
c  im and jm are the search grid sizes
c  lon_rtf and lat_rtf are the longitudes and latitudes of the search grid
c  nn is the OFS OB active grid point number
c  lon_ob and lat_ob are the OFS OB active grid longitudes and latitudes
c
c  Output:
c  nselect is the numbe of search grids which are selected to interpolate
c  II and JJ are the I- and J-indexes of search grids what are selected

DEL,XOUT,YOUT,
	subroutine search_output(im,jm,lon_rtf,lat_rtf,nn,
     .	           lon_ob,lat_ob,nselect,II,JJ)
        parameter (NMAX=90000)

	logical   found,log1,log2,log3,log4
	dimension mask(im,jm),II(NMAX),JJ(NMAX)
	real*4    lon_rtf(im,jm),lat_rtf(im,jm)
	real*4    lon_ob(nn),lat_ob(nn)

c  Assign zeroes to mask       
	do i=1,im
	  do j=1,jm
	    mask(i,j)=0
	  end do
	end do

	nselect=0
	do k=1,nn
	  x0=real(lon_ob(k))
	  y0=real(lat_ob(k))
	  found=.FALSE.
	  do i=1,im-1
	    if(found) then
	      exit
	    end if
	    do j=1,jm-1
	      if(.not.found) then
		x1=lon_rtf(i,j)
		x2=lon_rtf(i+1,j)
	        y1=lat_rtf(i,j)
		y2=lat_rtf(i,j+1)
		log1=x1.le.x0
		log2=x0.lt.x2
		log3=y1.le.y0
	        log4=y0.lt.y2
		if(log1.and.log2.and.log3.and.log4) then
		  found=.TRUE.
		  do i0=i-2,i+3
		    do j0=j-2,j+3
		      if (i0.ge.1.and.i0.le.im.and.
     .                    j0.ge.1.and.j0.le.jm) then
		        if (mask(i0,j0).ne.1) then
		          nselect=nselect+1
		          ii(nselect)=i0
		          jj(nselect)=j0
		          mask(i0,j0)=1
		        end if
    		      end if
		    end do
		  end do
	        end if
	      end if
	    end do
	  end do
	end do
	print*,'OFS OB #:',nn,'  Selected RTOFS #:',nselect
	
	return
	end
