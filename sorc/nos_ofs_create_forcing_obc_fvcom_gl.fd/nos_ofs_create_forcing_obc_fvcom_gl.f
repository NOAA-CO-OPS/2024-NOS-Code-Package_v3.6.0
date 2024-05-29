C----------------------------------------------------------------------------------
C
C Program Name:  nos_ofs_create_forcing_obc_fvcom_gl.f
C
C Directory:  https://svnemc.ncep.noaa.gov/projects/nosofs_shared/trunk/
C                    sorc/nos_ofs_create_forcing_obc_fvcom.fd
C
C Purpose:    This Program is used to generated lateral open boundary condition files for FVCOM 
C             from observations. The program is modified from nos_ofs_create_forcing_obc_fvcom.f.
C             Great Lakes OFS uses no model output at lateral boundaries and have no tides. 

C             Refer to HPC_COMF Technical Report for more detail information.               

C Current contact:   Aijun Zhang
C             Org:  NOS/CO-OPS/OD   Phone:  301-713-2890 ext. 127 
C                    aijun.zhang@Noaa.gov 
C Attributes:
C  Language:  Fortran
C  Computer:  DEW/MIST at NCEP  
C
C  Compile command:  gmake -f makefile
C
C Subprograms called: sal78.f nos_ofs_obc_write_netcdf_fvcom.f utility.f
C
C    
C Usage:   nos_ofs_create_forcing_obc_fvcom_gl < Fortran_OBC.ctl > Fortran_OBC.log 
C
C
C Input Parameters:
C           OFS         : name of Operational Forecast System, e.g. CBOFS, TBOFS
C        Ocean_MODEL    : Name of numerical ocean model used in OFS, e.g. ROMS, FVCOM
C        DBASE_WL      : data source for nontidal water level OBC
C        DBASE_TS       : data source for temperature and salinity OBC
C        TIME_START     : Start time 
C        FORHRS         : Length of forecast time in hours
C        IGRD           : indicator of horizontal interpolation method
C                        =0: no interpolation
C                        =1:  remesh using triangulation techniques
C                          =2: bicubic routine from ROMS
C                           =3: bilinear routine from ROMS
C                        =4: nature neighbours
C        FIXofs         : path to store NOS static data files 
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
C
C Output files: 
C    A netCDF open boundary forcing file which contains the required variables. 
C----------------------------------------------------------------------------------
      parameter (NMAX=9000)
      include 'netcdf.inc'
      character*200 OFS,OCEAN_MODEL*10,DBASE_WL*20,DBASE_TS,FILE_TS
      character*200 FIN,FOUT,GRIDFILE,netcdf_file,FIXofs,ETSSFILE
      character*200 BUFFER,CMD*132,VNAME,ANAME,OBC_CTL_FILE,HC_FILE
      character*200 BUFFER1,HC_FILE_OFS,FILE_NCOM(100),NCOMDIR,NOSWLDIR
      character*200 FILE_NCOM_TIDE(100),CDATE*8,FNAME,FIN_TIDE,CHH*2
      character*200 FDIR,STYPE,VGRIDFILE,NCOM_FILE,NOSBUFR,USGSBUFR
      character*200 COMOUT00,WL_CORRECTION_CTL,PREFIXNOS,OBC_FORCING_FILE_LAST
      character*26,allocatable :: Times(:)
      integer, allocatable :: Itime(:)
      integer, allocatable :: Itime2(:)
      character*200 CORMSLOG,DBASE_TS_ORI,INIT_FILE_NOWCAST
      character*200 START_TIME, END_TIME,BUFRFILE,OBC_CLIM_FILE
      character*200 START_TIMEm1
      real*8 jday_start,jbase_date,JULIAN,yearb,monthb,dayb,hourb
      real*8 jday,jdays,jdaye,jday0,js_etss,je_etss
      real minlon,minlat,maxlat,maxlon,missvalue
      LOGICAL FEXIST,USGS_L
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      CHARACTER globalstr(9)*120
      INTEGER DATE_TIME(8)
      INTEGER BASE_DATE(4)
      integer grbunit,NCID,NCIDOUT
      real latsw,lonsw,LaD,LoV,dx_grb

c allocatable arrays for FVCOM model
      integer, allocatable :: nv(:,:)  
      integer, allocatable :: nvOBC(:,:)  
      real, allocatable :: lonm(:)
      real, allocatable :: latm(:)
      real, allocatable :: lonc(:)
      real, allocatable :: latc(:)
      real, allocatable :: sigma(:)
      real, allocatable :: siglay(:,:)
      real, allocatable :: siglev(:,:)
      real, allocatable :: ZSIGMA(:)
      real, allocatable :: maskm(:,:)
      real, allocatable :: hm(:)
      real, allocatable :: ubarOBC(:,:)
      real, allocatable :: vbarOBC(:,:)
      real, allocatable :: Iout(:,:)
      real, allocatable :: Jout(:,:)
      real, allocatable :: TIME_M(:)
      
      real, allocatable :: TIMEOBC(:)
      real, allocatable :: WLOBC(:,:)
      real, allocatable :: tempOBC(:,:,:)
      real, allocatable :: saltOBC(:,:,:)
      real, allocatable :: uOBC(:,:,:)
      real, allocatable :: vOBC(:,:,:)
      real, allocatable :: tempOBC_M(:,:,:)
      real, allocatable :: saltOBC_M(:,:,:)
      real, allocatable :: uOBC_M(:,:,:)
      real, allocatable :: vOBC_M(:,:,:)
      real, allocatable :: ueOBC_M(:,:,:)
      real, allocatable :: veOBC_M(:,:,:)

c  allocatable arrays for variables in NCOM file
      real, allocatable :: zeta_time(:)
      real, allocatable :: ts_time(:)

c temporary arrays
      real, allocatable :: tmp1d(:)
      real, allocatable :: tmp2d(:,:)
      real, allocatable :: tmp3d(:,:,:)
      real, allocatable :: tmp4d(:,:,:,:)
      real*8, allocatable :: dtmp4d(:,:,:,:)
      integer, allocatable :: itmp4d(:,:,:,:)
      real, allocatable :: oned1(:)
      real, allocatable :: oned2(:)
      real, allocatable :: oned3(:)
      real, allocatable :: oned4(:)
      real, allocatable :: outm(:,:)
      real*4, allocatable :: XINP(:)
      real*4, allocatable :: YINP(:)
      real*4, allocatable :: ZINP(:)
      real*4, allocatable :: XOUT(:)
      real*4, allocatable :: YOUT(:)
      real*4, allocatable :: ZOUT(:)
      INTEGER :: status      ! Return status
      integer, allocatable :: weightnodes(:,:)  
      integer, allocatable :: ITMP3D(:,:,:)
      real, allocatable :: weights(:,:)       
      integer n3(3),ITMP1D(2000)
      real w3(3)
      integer dimids(5),COUNT(4),DIMS(4)

c variables for real time observations
      CHARACTER*8  SUBSET,NAME                                  
      character*20 stnbufrid
      Real*8 DATES(5),RTIM(6),RPID
      Real*8 data1(2,500),data2(4,500),data3(5)
      real*8 xlocat(5)
      DATA BMISS /10E10/                                                
      equivalence(NAME,RPID)

c WL corrections
      character*200 CORRECTION_OLD
      real*4, allocatable :: WL_OFFSET(:)

c variables for OBC_CTL_FILE     
      integer, allocatable :: SID(:)
      character*20,allocatable :: NOS_ID(:)
      character*5,allocatable :: NWS_ID(:)
      character*4,allocatable :: AGENCY_ID(:)
      character*1,allocatable :: OBC_ID(:)
      real, allocatable :: DATUM(:)
      integer, allocatable :: WL_FLAG(:)
      integer, allocatable :: TS_FLAG(:)
      integer, allocatable ::BACKUP_SID(:)
      integer, allocatable :: GRIDID_STA(:)
      real, allocatable :: As(:)

      integer, allocatable :: GRIDID(:)
      integer, allocatable :: IOBC(:)
      integer, allocatable :: JOBC(:)
      integer, allocatable :: WL_STA(:)
      integer, allocatable :: TS_STA(:)
      integer, allocatable :: WL_SID_1(:)
      integer, allocatable :: WL_SID_2(:)
      integer, allocatable :: TS_SID_1(:)
      integer, allocatable :: TS_SID_2(:)
      real, allocatable :: WL_S_1(:)
      real, allocatable :: WL_S_2(:)
      real, allocatable :: TS_S_1(:)
      real, allocatable :: TS_S_2(:)
      integer, allocatable :: CU_STA(:)
      integer, allocatable :: CU_SID_1(:)
      integer, allocatable :: CU_SID_2(:)
      real, allocatable :: CU_S_1(:)
      real, allocatable :: CU_S_2(:)
      
      integer, allocatable :: NTR(:)
      integer, allocatable :: NTR_T(:)
      integer, allocatable :: NTR_S(:)
      real, allocatable :: RTIME(:,:)
      real, allocatable :: RTIME_T(:,:)
      real, allocatable :: RTIME_S(:,:)
      real, allocatable :: TIME_PRD(:)
      real, allocatable :: WL_PRD(:,:)
      real, allocatable :: WL_OBS(:,:)
      real, allocatable :: SWL_OBS(:,:)
      real, allocatable :: T_OBS(:,:) 
      real, allocatable :: S_OBS(:,:) 
      real, allocatable :: lonOBC(:) 
      real, allocatable :: latOBC(:) 
      real, allocatable :: loneOBC(:) 
      real, allocatable :: lateOBC(:) 
      real, allocatable :: hOBC(:) 
      real, allocatable :: AVGERR(:) 
      real, allocatable :: AVGERR_T(:) 
      real, allocatable :: AVGERR_S(:) 

      real, allocatable :: TIME_clim(:)
      real, allocatable :: T_clim(:,:,:) 
      real, allocatable :: S_clim(:,:,:) 
      integer, allocatable :: K_clim(:)
      real, allocatable :: depth_clim(:,:)

      real, allocatable :: ZKU(:)
      real, allocatable :: ZKL(:)
      integer, allocatable :: partition(:)
      
!-----------------------------------------------------------------------
!  read parameters from the Fortran control "Fortran_OBC.ctl"
!-----------------------------------------------------------------------
      read(5,'(a200)') OFS
      read(5,'(a10)') OCEAN_MODEL
      read(5,'(a20)') DBASE_WL
      read(5,'(a200)') ETSSFILE
      read(5,'(a200)') DBASE_TS
      DBASE_TS_ORI=DBASE_TS
      read(5,'(a200)') NCOMDIR
      read(5,'(a200)') NOSWLDIR
      read(5,'(a200)') BUFFER
      NOSBUFR=trim(adjustL(BUFFER))
      read(5,'(a200)') BUFFER
      USGSBUFR=trim(adjustL(BUFFER))
      read(5,'(a200)') BUFFER
      START_TIME=trim(adjustL(BUFFER))
      read(START_TIME,'(I4,4I2)') IYRS,IMMS,IDDS,IHHS,IMNS

      read(5,'(a200)') BUFFER
      END_TIME=trim(adjustL(BUFFER))
      read(END_TIME,'(I4,4I2)') IYRE,IMME,IDDE,IHHE,IMNE
      read(5,*) IGRD
      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif    
      enddo
      FIXofs=trim(adjustL(BUFFER))
      
      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif    
      enddo
      GRIDFILE=trim(adjustL(BUFFER))
      write(*,*) 'GRIDFILE=',trim(gridfile)

      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif    
      enddo
      HC_FILE_OFS=trim(adjustL(BUFFER))
      write(*,*) 'HC_FILE_OFS=',trim(HC_FILE_OFS)

      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif    
      enddo
      OBC_CTL_FILE=trim(adjustL(BUFFER))
      write(*,*) 'OBC_CTL_FILE=',trim(OBC_CTL_FILE)

      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif    
      enddo
      OBC_CLIM_FILE=trim(adjustL(BUFFER))
      write(*,*) 'OBC_CLIM_FILE=',trim(OBC_CLIM_FILE)

      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif
      enddo
      WL_CORRECTION_CTL=trim(adjustL(BUFFER))
      write(*,*) 'WL_CORRECTION_CTL= ', trim(WL_CORRECTION_CTL)

      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif
      enddo
      CORRECTION_OLD=trim(adjustL(BUFFER))
      write(*,*) 'OBC_WL_OFFSET_FILE=', trim(CORRECTION_OLD)

      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif    
      enddo
      netcdf_file=trim(adjustL(BUFFER))

      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif    
      enddo
      CORMSLOG=trim(adjustL(BUFFER))
      write(*,*) 'CORMSLOG=',trim(CORMSLOG)

      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif    
      enddo
      BUFFER=trim(adjustL(BUFFER))

      read(BUFFER,'(I4,3i2)') base_date
      read(5,*) minlon
      read(5,*) minlat
      read(5,*) maxlon
      read(5,*) maxlat
      read(5,*) KBm
      if(trim(OCEAN_MODEL).eq.'ROMS') then
        read(5,*) THETA_S
        read(5,*) THETA_B
        read(5,*) TCLINE
      else  
        read(5,'(a200)') BUFFER
        do i=1,len_trim(BUFFER)
          if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
            BUFFER(i:I)=' '
          endif    
        enddo
        VGRIDFILE=trim(adjustL(BUFFER))
      endif  
      read(5,'(a200)') COMOUT00
      read(5,'(a200)')OBC_FORCING_FILE_LAST
      read(5,'(a200)')PREFIXNOS
c-----------------------------------------------------------------------
c  calculate base time, start time, and end time in days
c-----------------------------------------------------------------------
      ICORMS=43
      CLOSE(ICORMS)
      OPEN(ICORMS,FILE=trim(CORMSLOG),STATUS='OLD',POSITION='APPEND')
      WRITE(ICORMS,'(a)') 'BEGIN SECTION OF GENERATING OBC' 
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
      write(*,*) 'base_date= ',base_date
      write(*,*) 'domain=',minlon,minlat,maxlon,maxlat
      write(*,*) 'model start & end time= ',day_start,day_end

c-----------------------------------------------------------------------
c   reading in FVCOM or SELFE model grid information
c-----------------------------------------------------------------------
      IF(trim(OCEAN_MODEL).EQ.'FVCOM'.OR.
     &   trim(OCEAN_MODEL).EQ.'SELFE') THEN
        WRITE(*,*) 'Reading Unstructured grid file:',trim(GRIDFILE)
        OPEN(40,file=trim(GRIDFILE))
        read(40,*)
        READ(40,*) NELEm,NODEm
        ALLOCATE(lonm(NODEm))
        ALLOCATE(latm(NODEm))
        ALLOCATE(lonc(NELEm))
        ALLOCATE(latc(NELEm))
        ALLOCATE(hm(NODEm))
        ALLOCATE(nv(NELEm,3))
        do n=1,NODEm
          read(40,*) im,lonm(n),latm(n),hm(n)
        enddo
        do n=1,nelem
          read(40,*) idum,idum,(nv(n,i),i=1,3)
          lonc(n)=(lonm(nv(n,1))+lonm(nv(n,2))+lonm(nv(n,3)))/3.0
          latc(n)=(latm(nv(n,1))+latm(nv(n,2))+latm(nv(n,3)))/3.0
        enddo
        CLOSE(40)
      ENDIF
      hmin=minval(hm)
      hmax=maxval(hm)

c -------------------------------------------------------------------
c   open and read OBC control file  
c -------------------------------------------------------------------
      WRITE(*,*) 'Reading OBC control file'
      OPEN(20,FILE=TRIM(OBC_CTL_FILE))
      READ(20,*) NSTA,NOBC,NEOBC,DELT 
      WRITE(*,*) 'NSTA= ',NSTA,'NOBC=',NOBC

c Arrays for real time observations
      ALLOCATE(SID(NSTA))
      allocate(NOS_ID(NSTA))
      allocate(NWS_ID(NSTA))
      allocate(AGENCY_ID(NSTA))
      allocate(DATUM(NSTA))
      allocate(WL_FLAG(NSTA))
      allocate(TS_FLAG(NSTA))
      allocate(BACKUP_SID(NSTA))
      allocate(GRIDID_STA(NSTA))
      allocate(As(NSTA))

      allocate(GRIDID(NOBC))
      allocate(IOBC(NOBC))
      allocate(JOBC(NEOBC))
      allocate(WL_STA(NOBC))
      allocate(TS_STA(NOBC))
      allocate(WL_SID_1(NOBC))
      allocate(WL_SID_2(NOBC))
      allocate(TS_SID_1(NOBC))
      allocate(TS_SID_2(NOBC))
      allocate(WL_S_1(NOBC))
      allocate(WL_S_2(NOBC))
      allocate(TS_S_1(NOBC))
      allocate(TS_S_2(NOBC))
      allocate(OBC_ID(NOBC))
      allocate(lonOBC(NOBC))
      allocate(latOBC(NOBC))
      allocate(loneOBC(NEOBC))
      allocate(lateOBC(NEOBC))
      allocate(hOBC(NOBC))
      allocate(nvOBC(NEOBC,3))
      allocate(partition(NEOBC))
      allocate(siglay(NOBC,KBm))
      allocate(siglev(NOBC,KBm+1))
      allocate(sigma(KBm+1))
      allocate(Zsigma(KBm+1))
      allocate(WL_OFFSET(NOBC))

      ALLOCATE(NTR(NSTA))
      ALLOCATE(NTR_T(NSTA))
      ALLOCATE(NTR_S(NSTA))
      allocate(RTIME(NSTA,NMAX))
      allocate(RTIME_T(NSTA,NMAX))
      allocate(RTIME_S(NSTA,NMAX))
      allocate(TIME_PRD(NMAX))
      allocate(WL_PRD(NSTA,NMAX))
      allocate(WL_OBS(NSTA,NMAX))
      allocate(SWL_OBS(NSTA,NMAX))
      allocate(T_OBS(NSTA,NMAX))
      allocate(S_OBS(NSTA,NMAX))

      ALLOCATE(K_clim(NSTA))
      allocate(TIME_clim(NMAX))
      allocate(depth_clim(NSTA,50)) 
      allocate(T_clim(NSTA,NMAX,50)) 
      allocate(S_clim(NSTA,NMAX,50)) 

      DO I=1,NEOBC
        partition(I)=1
      ENDDO    

      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF(ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF(ALLOCATED(oned4)) DEALLOCATE(oned4)
      allocate(oned1(NMAX))
      allocate(oned2(NMAX))
      allocate(oned3(NMAX))
      allocate(oned4(NMAX))

      DO
        READ(20,*) BUFFER 
        BUFFER=trim(adjustL(BUFFER))
        LL=len_trim(BUFFER)
        IF(LL.GT.0) EXIT
      ENDDO
      READ(20,*) BUFFER 
      DO N=1,NSTA
        READ(20,*) SID(N),NOS_ID(N),NWS_ID(N),AGENCY_ID(N),DATUM(N),
     &    WL_FLAG(N),TS_FLAG(N),BACKUP_SID(N),GRIDID_STA(N),As(N)
      ENDDO

      DO
        READ(20,*) BUFFER 
        BUFFER=trim(adjustL(BUFFER))
        LL=len_trim(BUFFER)
        IF(LL.GT.0) EXIT
      ENDDO
      READ(20,*) BUFFER
      DO N=1,NOBC
        READ(20,*) GRIDID(N),IOBC(N),WL_STA(N),WL_SID_1(N),
     &    WL_S_1(N),WL_SID_2(N),WL_S_2(N),TS_STA(N),TS_SID_1(N),
     &    TS_S_1(N),TS_SID_2(N),TS_S_2(N)
        lonOBC(N)=lonm(IOBC(N))
        latOBC(N)=latm(IOBC(N))
        hOBC(N)=hm(IOBC(N))
        if(lonOBC(N).GT.180.0) lonOBC(N)=lonOBC(N)-360.0
      ENDDO

      DO
        READ(20,*) BUFFER 
        BUFFER=trim(adjustL(BUFFER))
        LL=len_trim(BUFFER)
        IF(LL.GT.0) EXIT
      ENDDO
      READ(20,*) BUFFER 
      DO N=1,NEOBC
        READ(20,*) Idummy,JOBC(N)
        loneOBC(N)=lonc(JOBC(N))
        lateOBC(N)=latc(JOBC(N))
        DO L=1,3
          nvOBC(N,L)=nv(JOBC(N),L)
        ENDDO  
        if(loneOBC(N).GT.180.0) loneOBC(N)=loneOBC(N)-360.0
      ENDDO

      USGS_L=.FALSE.
      DO N=1,NSTA
        IF(TRIM(AGENCY_ID(N)).EQ.'USGS') USGS_L=.TRUE.
      ENDDO
      NREC=NINT((jdaye-jdays)*24/DELT)+1
      IF(ALLOCATED(TIME_M)) DEALLOCATE(TIME_M)
      allocate(TIME_M(NREC))
      DO N=1,NREC
        TIME_M(N)=(jdays-jbase_date)+(N-1)*DELT/24.0
      ENDDO

c-----------------------------------------------------------------------
c  Define S-Curves in domain [-1 < sc < 0] at vertical W- and
c  RHO-points.
c-----------------------------------------------------------------------
      write(*,*) 'vgrid=',trim(VGRIDFILE)
      OPEN(40,file=trim(VGRIDFILE))
      DO
        read(40,'(a200)',end=12) BUFFER
        write(*,*) 'BUFFER=',trim(buffer)
        if(BUFFER(1:1).eq.'!') THEN
          WRITE(*,*) 'Reading a comment line!'
          CYCLE
        else  
          do i=1,len_trim(BUFFER)
            if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
              BUFFER(i:I)=' '
            endif    
          enddo
          BUFFER=trim(adjustL(BUFFER))
          LL=len_trim(BUFFER)
          IND=INDEX(BUFFER,'NUMBER OF SIGMA LEVELS')
          IF(IND.GT.0) THEN
            IND=INDEX(BUFFER,'=')
            READ(BUFFER(IND+1:IND+4),*) KB
            WRITE(*,*) 'KB=',KB
            IF(KB.NE.KBm+1) THEN
              WRITE(*,*)'Vertical parameters KBm and KB are '//
     &          'not consistent' 
              WRITE(*,*) 'Check main ctl and vgrid files'
              STOP
            ENDIF    
          ENDIF

          IND=INDEX(BUFFER,'SIGMA COORDINATE TYPE')
          IF(IND.GT.0) THEN
            IND=INDEX(BUFFER,'=')
            READ(BUFFER(IND+1:LL),*) STYPE
            WRITE(*,*) 'STYPE=',trim(STYPE)   
          ENDIF

          IND=INDEX(BUFFER,'DU ')
          IF(IND.GT.0) THEN
            IND=INDEX(BUFFER,'=')
            READ(BUFFER(IND+1:LL),*) DU
            WRITE(*,*) 'DU=',DU   
          ENDIF

          IND=INDEX(BUFFER,'DL ')
          IF(IND.GT.0) THEN
            IND=INDEX(BUFFER,'=')
            READ(BUFFER(IND+1:LL),*) DL
            WRITE(*,*) 'DL=',DL   
          ENDIF

          IND=INDEX(BUFFER,'MIN CONSTANT DEPTH')
          IF(IND.GT.0) THEN
            IND=INDEX(BUFFER,'=')
            READ(BUFFER(IND+1:LL),*) HMIN1
            WRITE(*,*) 'HMIN=',HMIN1   
          ENDIF

          IND=INDEX(BUFFER,'KUU ')
          IF(IND.GT.0) THEN
            IND=INDEX(BUFFER,'=')
            READ(BUFFER(IND+1:LL),*) KU
            WRITE(*,*) 'KU=',KU
          ENDIF

          IND=INDEX(BUFFER,'KLL ')
          IF(IND.GT.0) THEN
            IND=INDEX(BUFFER,'=')
            READ(BUFFER(IND+1:LL),*) KL
            WRITE(*,*) 'KL=',KL
          ENDIF

          IND=INDEX(BUFFER,'ZKU ')
          IF(IND.GT.0) THEN
            ALLOCATE(ZKU(KU))   
            IND=INDEX(BUFFER,'=')
            READ(BUFFER(IND+1:LL),*) (ZKU(K),K=1,KU)
            WRITE(*,*) 'ZKU=',ZKU
          ENDIF

          IND=INDEX(BUFFER,'ZKL ')
          IF(IND.GT.0) THEN
            ALLOCATE(ZKL(KL)) 
            IND=INDEX(BUFFER,'=')
            READ(BUFFER(IND+1:LL),*) (ZKL(K),K=1,KL)
            WRITE(*,*) 'ZKL=',ZKL
          ENDIF
        endif
      ENDDO
12    CONTINUE
     
c-----------------------------------------------------------------------
c  Define sigma coordinates in domain [-1 < sc < 0] at vertical points.
c-----------------------------------------------------------------------
      IF(TRIM(STYPE).EQ.'UNIFORM') THEN
        DO I=1,NOBC
          DO K=1,KB
            siglev(I,K)=-((K-1)/FLOAT(KBm))
          ENDDO
        ENDDO
      ELSEIF(TRIM(STYPE).EQ.'GENERALIZED') THEN
        DO I=1,NOBC
          IF(hOBC(I)<HMIN1) THEN
            siglev(I,1)=0.0
            DL2=0.001;DU2=0.001
            DO K=1,KBm
              X1=DL2+DU2
              X1=X1*(KBm-K)/KBm
              X1=X1-DL2
              X1=TANH(X1)
              X2=TANH(DL2)
              X3=X2+TANH(DU2)
              siglev(I,K+1)=(X1+X2)/X3-1.0
            ENDDO
          ELSE
            siglev(I,1)=0.0
            DR=(HOBC(I)-DU-DL)/HOBC(I)/(KB-KU-KL-1)
            DO K=2,KU+1
              siglev(I,K)=siglev(I,K-1)-ZKU(K-1)/HOBC(I)
            ENDDO

            DO K=KU+2,KB-KL
              siglev(I,K)=siglev(I,K-1)-DR
            ENDDO

            KK=0
            DO K=KB-KL+1,KB
              KK=KK+1
              siglev(I,K)=siglev(I,K-1)-ZKL(KK)/HOBC(I)
            ENDDO
          ENDIF
        ENDDO
      ELSEIF(TRIM(STYPE).EQ.'TANH') THEN
        DO I=1,NOBC
          siglev(I,1)=0.0
        ENDDO

        DO K=1,KBM
          X1=DL+DU
          X1=X1*(KBM-K)/KBM
          X1=X1-DL
          X1=TANH(X1)
          X2=TANH(DL)
          X3=X2+TANH(DU)
          DO I=1,NOBC
            siglev(I,K+1)=(X1+X2)/X3-1.0
          ENDDO
        ENDDO
      ENDIF

      DO I=1,NOBC
        DO K=1,KBM      
          siglay(I,K)=0.5*(siglev(I,K)+siglev(I,K+1))
        ENDDO
      ENDDO            

      DO K=1,KB
        WRITE(*,*) 'K= ',K,(siglev(I,K),I=1,3)      
      ENDDO 
      IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
      IF(ALLOCATED(itmp4d)) DEALLOCATE(itmp4d)

c------------------------------------------------------------------
c  Begin to read in climatological temperature and salinity 
c  and temporally interpolated into equally interval of DELT 
c------------------------------------------------------------------
      NREC=NINT((jdaye-jdays)*24/DELT)+1
      DO N=1,NREC
        TIME_clim(N)=((jdays-jbase_date)*24.0+(N-1)*DELT)/24.0
      ENDDO
      NT_clim=NREC
      DO N=1,NSTA
        IYR=IYRS
        yearb=IYRS
        IF(TS_FLAG(N).GT.0) THEN
          BUFFER1=trim(PREFIXNOS)//'.obc.clim.ts.'
          BUFFER=TRIM(BUFFER1)//TRIM(NOS_ID(N))//'.dat'
          FIN=TRIM(BUFFER)
          write(*,*) 'FIN= ',trim(FIN)
          INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
          IF(.NOT.FEXIST) THEN
            WRITE(*,'(a)')'CRITICAL FAILURE IN CREATING OBC'
            WRITE(*,*)'Climatologic T&S file is not found'
            WRITE(*,*)'Please provide climatologic T&S file at '
            WRITE(*,*)'Station ', TRIM(NOS_ID(N))
            WRITE(ICORMS,'(a)')'CRITICAL FAILURE IN CREATING OBC' 
            STOP
          ELSE  
            yearb=IYRS
            monthb=1.0
            dayb=1.0
            hourb=0
            jday0=JULIAN(yearb,monthb,dayb,hourb)
            CLOSE(10)
            OPEN(10,FILE=TRIM(FIN))
            READ(10,*) K_clim(N)
            READ(10,*) (DEPTH_clim(N,K),K=1,K_clim(N))
            READ(10,*)
            ICOUNT=0
            DO
              READ(10,*,end=20) dummy,mon,IDD,
     &          (ONED1(K),ONED2(K),K=1,K_clim(N))
              ICOUNT=ICOUNT+1
              day0=jday0+dummy-jbase_date
              IF((ICOUNT.EQ.1).and.(day0.GT.day_start)) then
                ONED3(ICOUNT)=day_start
                DO K=1,K_clim(N)
                  T_clim(N,ICOUNT,K)=ONED1(K)
                  S_clim(N,ICOUNT,K)=ONED2(K)
                ENDDO
                ICOUNT=ICOUNT+1
              ENDIF

              ONED3(ICOUNT)=day0
              DO K=1,K_clim(N)
                T_clim(N,ICOUNT,K)=ONED1(K)
                S_clim(N,ICOUNT,K)=ONED2(K)
              ENDDO
            ENDDO
20          CONTINUE
            CLOSE(10)
 
            IF(ONED3(ICOUNT).LT.day_end) then
              ICOUNT=ICOUNT+1
              ONED3(ICOUNT)=day_end
              DO K=1,K_clim(N)
                T_clim(N,ICOUNT,K)=ONED1(K)
                S_clim(N,ICOUNT,K)=ONED2(K)
              ENDDO
            ENDIF
            WRITE(*,*) 'Time range= ',day_start,day_end,
     &        ONED3(1),ONED3(ICOUNT),ICOUNT
            DO K=1,K_clim(N)
              DO N1=1,ICOUNT
                oned1(N1)=T_clim(N,N1,K)
                oned2(N1)=S_clim(N,N1,K)
              ENDDO  
              CALL lineararray(NREC,TIME_clim,oned4,ICOUNT,
     &             oned3,oned1)
              DO N1=1,NREC
                T_clim(N,N1,K) =oned4(N1)
              ENDDO
              CALL lineararray(NREC,TIME_clim,oned4,ICOUNT,
     &             oned3,oned2)
              DO N1=1,NREC
                S_clim(N,N1,K) =oned4(N1)
              ENDDO
            ENDDO
          ENDIF   !file exist
        ENDIF   !TS_FLAG(N) .GT. 0
      ENDDO   !N=1,NSTA

c------------------------------------------------------------------
c   For GL, to make use of and fit in the original code
c   Make model conditions zero
c------------------------------------------------------------------
      ALLOCATE(zeta_time(NREC))
      ALLOCATE(ts_time(NREC))
      ALLOCATE(WLOBC(NOBC,NREC))
      ALLOCATE(TEMPOBC_M(NOBC,KBM,NREC))
      ALLOCATE(SALTOBC_M(NOBC,KBM,NREC))
      ALLOCATE(UEOBC_M(NEOBC,KBM,NREC))
      ALLOCATE(VEOBC_M(NEOBC,KBM,NREC))
      ALLOCATE(UBAROBC(NEOBC,NREC))
      ALLOCATE(VBAROBC(NEOBC,NREC))

      DO N=1,NREC
        zeta_time(N)=TIME_M(N)
        ts_time(N)=TIME_M(N)
        DO I=1,NOBC
          WLOBC(I,N)=0.0
          DO K=1,KBM
            TEMPOBC_M(I,K,N)=0.0
            SALTOBC_M(I,K,N)=0.0
          ENDDO
        ENDDO

        DO I=1,NEOBC
          UBAROBC(I,N)=0.0
          VBAROBC(I,N)=0.0
          DO K=1,KBM
            UEOBC_M(I,K,N)=0.0
            VEOBC_M(I,K,N)=0.0
          ENDDO
        ENDDO
      ENDDO
      NTMAX_WL=NREC
      NTMAX=NREC

c -------------------------------------------------------------------
c   Begin to process real time observations and tidal prediction
c   Do 10 days tidal prediction from day_start-2 to cover N/F periods
c ----------------------------------------------------------------------------
      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      allocate(oned1(NMAX))
      allocate(oned2(NMAX))

      DO I=1,NSTA
        NTR(I)=0
        NTR_T(I)=0
        NTR_S(I)=0
        DO N=1,NMAX
          RTIME(i,N)=-99999.9
          RTIME_T(i,N)=-99999.9
          RTIME_S(i,N)=-99999.9
          WL_OBS(i,N)=-99999.9
          WL_PRD(I,N)=0.0
          SWL_OBS(i,N)=-99999.9
          T_OBS(I,N)=-99999.9
          S_OBS(I,N)=-99999.9
        ENDDO
      ENDDO     

c ----------------------------------------------------------------------------
c    Process real time observations in BUFR files of NCEP data tank at NWLON stations
c ----------------------------------------------------------------------------
      WRITE(FOUT,201) 'DATA_OBC_',IYRS,IMMS,IDDS,IHHS,'.dat'
201   FORMAT(A9,I4.4,3I2.2,A4)
      FOUT=TRIM(OFS)//'_'//TRIM(adjustL(FOUT))
      CLOSE(15) 
      OPEN(15,file=TRIM(FOUT))
      write(15,'(A)') '       ID        lat       lon year mm
     & dd hr      EL       ATMP       SST    Salinity Conductivity'
      DO IZ=INT(day_start-1),INT(day_end)
        jday=IZ+jbase_date
        call CALENDARDATE(jday,yearb,monthb,dayb,hourb,
     &       IYR,IMM,IDD,IHH,IMN,ISEC)
        WRITE(BUFRFILE,700) '/',IYR,IMM,IDD,'/b001/'
700     FORMAT(A1,I4.4,2I2.2,A6)
        BUFRFILE=TRIM(NOSWLDIR)//trim(BUFRFILE)//trim(NOSBUFR) 
        INQUIRE(FILE=trim(BUFRFILE),EXIST=FEXIST)
        write(*,*) trim(BUFRFILE),FEXIST
        IF(FEXIST) THEN
          write(*,*) 'BUFR FILE= ',trim(BUFRFILE)
          LUNIN=11
          CLOSE(LUNIN)
          OPEN(LUNIN,file=trim(BUFRFILE),FORM='UNFORMATTED')

c -------------------OPEN AND READ THRU THE INPUT BUFR FILE ------------------
          CALL OPENBF(LUNIN,'IN',LUNIN)                                     

c --------------READ THROUGH THE MESSAGES/SUBSETS IN THE FILE------------------
          DO WHILE(IREADMG(LUNIN,SUBSET,IDATE).EQ.0)                       
            DO WHILE(IREADSB(LUNIN).EQ.0)                                    

c --------------READ THE INTERNAL DATE AND CHECK FOR REALISM---------------------
              CALL UFBINT(LUNIN,DATES,5,1,IRET,
     &             'YEAR MNTH DAYS HOUR MINU')
              IYR=NINT(DATES(1))
              IMM=NINT(DATES(2))
              IDD=NINT(DATES(3))                                           
              IHH=NINT(DATES(4))                                            
              IMN=NINT(DATES(5))                                            
              yearb=IYR
              monthb=IMM
              dayb=IDD
              hourb=REAL(IHH)+REAL(IMN/60.0)   
              dayj=JULIAN(yearb,monthb,dayb,hourb)-jbase_date

c  --------------READ THE TIDE GAUGE STATION INFO FROM BUFR FILE------------------
c  AJ 09/15/11 Use different CALL routine in order to handle long station IDs 
              CALL READLC(LUNIN, stnbufrid, 'RPID')
              CALL UFBINT(LUNIN,DATES,5,1,IRET,'CLAT CLON SELV')
              clat=DATES(1)
              clon=DATES(2)
              selv=DATES(3)
              DO I=1,NSTA 
                IF(trim(stnbufrid).EQ.TRIM(NWS_ID(I))) THEN
c  ----------------------------------------------------------------------------
c  GET SEA SURFACE TEMPERATURE DATA ALONG WITH DATA CHECK AND
c  TIME INCREMENT AND DISPLACEMENT INFORMATION
c  SST1 -- Sea Surface Temperature
c  QMST -- Sea Surface Data Check Flag
c  AWCK -- Tide Station Automated Water Data Check Flag
c  MWCK -- Tide Station Manual Water Data Check Flag
c  TPMI -- Time Period or Replacement
c  ----------------------------------------------------------------------------
                  LLEN=LEN_TRIM(BUFRFILE)
                  IF(BUFRFILE(LLEN-4:LLEN).EQ.'xx005') THEN      
                    CALL UFBINT(LUNIN,DATES,5,1,IRET,
     &                   'SST1 TMDB AWCK MWCK TPMI')
                  ELSEIF(BUFRFILE(LLEN-4:LLEN).EQ.'xx012') THEN
C   GET AIR TEMPERATURE AND SEA SURFACE TEMPERATURE DATA
                    CALL UFBINT(LUNIN,DATES,5,1,IRET,'WATM TMDB')
                  ENDIF    

                  if(DATES(1).ge.bmiss/2.0) then
                    SST=-99999.9
                  else    
                    SST=DATES(1)-273.15   !! Convert from Kelvin to deg C
                  endif
                  IF(SST.LT.0.0) SST=-99999.9  !! avoid bad values from RT data

                  if(DATES(2).ge.bmiss/2.0) then
                    ATMP=-99999.9
                  else    
                    ATMP=DATES(2)-273.15  !! Convert from Kelvin to deg C   
                  endif

                  IF(abs(SST).LT.40.00) THEN
                    IF(NTR_T(I).LT.1) THEN 
                      NTR_T(I)=NTR_T(I)+1
                      RTIME_T(I,NTR_T(I))=dayj
                      T_OBS(I,NTR_T(I))=SST
                    ELSE
                      IF(dayj.GT.RTIME_T(I,NTR_T(I))) THEN 
                        NTR_T(I)=NTR_T(I)+1
                        RTIME_T(I,NTR_T(I))=dayj
                        T_OBS(I,NTR_T(I))=SST
                      ENDIF
                    ENDIF
                  ENDIF

c  ----------------------------------------------------------------------------
c  GET CONDUCTIVITY AND SALINITY
c  ----------------------------------------------------------------------------
                  CALL UFBINT(LUNIN,DATES,5,1,IRET,'SALN WACN')
                  if(DATES(1).ge.bmiss/2.0) then
                    SALN=-99999.9
                  else    
                    SALN=DATES(1)
                  endif

                  if(DATES(2).ge.bmiss/2.0) then
                    COND=-99999.9        !     Unit of COND is mS/cm for NOS station
                  else    
                    COND=DATES(2)   
                  endif

                  IF(SALN.LT.-99999.0) THEN
                    IF((SST.GT.-99999.0).AND.(COND.GT.-99999.0)) THEN
                      SALN=SAL(COND,SST,0.0)
                    ENDIF   
                  ENDIF  
                  IF(SALN.LT.-0.5) SALN=-99999.9

                  IF(abs(SALN).LT.40.00) THEN
                    IF(NTR_S(I).LT.1) THEN 
	              NTR_S(I)=NTR_S(I)+1
                      RTIME_S(I,NTR_S(I))=dayj
	             S_OBS(I,NTR_S(I))=SALN
                    ELSE
                      IF(dayj.GT.RTIME_S(I,NTR_S(I))) THEN 
                        NTR_S(I)=NTR_S(I)+1
                        RTIME_S(I,NTR_S(I))=dayj
                        S_OBS(I,NTR_S(I))=SALN
                      ENDIF
                    ENDIF
                  ENDIF

c  -----------------------------------------------------------------------------
c  GET TIDAL ELEVATION WITH RESPECT TO CHART AND METEOROLOGICAL RESIDUAL TIDAL 
c  ELEVATION
c  ----------------------------------------------------------------------------
                  IF(BUFRFILE(LLEN-4:LLEN).EQ.'xx005') THEN      
                    CALL UFBINT(LUNIN,DATES,5,1,IRET,'TERC TIDER')
                  ELSEIF(BUFRFILE(LLEN-4:LLEN).EQ.'xx012') THEN 
                    CALL UFBINT(LUNIN,DATES,5,1,IRET,'TLLW TIDER')
                  ENDIF    

                  if(DATES(1).ge.bmiss/2.0) then
                    EL=-99999.9
                  else    
                    EL=DATES(1)  
                  endif

                  if(DATES(2).ge.bmiss/2.0) then
                    SWL=-99999.9
                  else    
                    SWL=DATES(2)   
                  endif

                  IF(abs(EL).LT.40.00) THEN
                    IF(NTR(I).LT.1) THEN 
                      NTR(I)=NTR(I)+1
                      RTIME(I,NTR(I))=dayj
                      WL_OBS(I,NTR(I))=EL-DATUM(I)  !convert WL from MLLW to MSL for NWLON stations
                      SWL_OBS(I,NTR(I))=SWL
                    ELSE
                      IF(dayj.GT.RTIME(I,NTR(I))) THEN 
                        NTR(I)=NTR(I)+1
                        RTIME(I,NTR(I))=dayj
                        WL_OBS(I,NTR(I))=EL-DATUM(I)
                        SWL_OBS(I,NTR(I))=SWL
                      ENDIF
                    ENDIF
                  ENDIF
                  write(15,780) trim(stnbufrid),clon,clat,IYR,IMM,
     &              IDD,IHH,IMN,EL,ATMP,SST,SALN,COND,SWL
                ENDIF   !station match
              ENDDO   ! I=1,NSTA
780           FORMAT(a20,1x,2F10.4,I5,4i3,10F12.4)
            ENDDO   !Do while
          ENDDO   !DO WHILE
          CALL CLOSBF(LUNIN)
          CLOSE(LUNIN)
        ENDIF   ! FEXIST
      ENDDO

c  ------------------------------------------------------------------------------
c   Decoding USGS BURF file if used
c  ------------------------------------------------------------------------------
      IF(USGS_L) THEN
        DO IZ=INT(day_start-1),INT(day_end)
          jday=IZ+jbase_date
          call CALENDARDATE(jday,yearb,monthb,dayb,hourb,
     &         IYR,IMM,IDD,IHH,IMN,ISEC)
          WRITE(BUFRFILE,700) '/',IYR,IMM,IDD,'/b001/'  
          BUFRFILE=TRIM(NOSWLDIR)//trim(BUFRFILE)//trim(USGSBUFR)
          INQUIRE(FILE=trim(BUFRFILE),EXIST=FEXIST)
          IF(FEXIST) THEN
            write(*,*) 'BUFR FILE= ',trim(BUFRFILE)
            LUNIN=11
            CLOSE(LUNIN)
            OPEN(LUNIN,file=trim(BUFRFILE),FORM='UNFORMATTED')
            CALL OPENBF(LUNIN,'IN',LUNIN)                                     

c  --------------READ THROUGH THE MESSAGES/SUBSETS IN THE FILE------------------
            DO WHILE(IREADMG(LUNIN,SUBSET,IDATE).EQ.0)                       
              DO WHILE(IREADSB(LUNIN).EQ.0)                                    
c  --------------READ THE INTERNAL DATE AND CHECK FOR REALISM---------------------
                CALL UFBINT(LUNIN,DATES,5,1,IRET,
     &               'YEAR MNTH DAYS HOUR MINU')
                IYR=NINT(DATES(1))
                IMM=NINT(DATES(2))
                IDD=NINT(DATES(3))                                           
                IHH=NINT(DATES(4))                                            
                IMN=NINT(DATES(5))                                            
                yearb=IYR
                monthb=IMM
                dayb=IDD
                hourb=REAL(IHH)+REAL(IMN/60.0) 
                dayj=JULIAN(yearb,monthb,dayb,hourb)-jbase_date
C  --------------READ THE TIDE GAUGE STATION INFO FROM BUFR FILE------------------
c  AJ 09/15/11 Use different CALL routine in order to handle long station IDs 
                CALL READLC(LUNIN, stnbufrid,'RPID')
                CALL UFBINT(LUNIN,DATES,5,1,IRET,'CLAT CLON SELV')
                clat=DATES(1)
                clon=DATES(2)
                selv=DATES(3)

                DO I=1,NSTA
                  IF(trim(stnbufrid).EQ.TRIM(NOS_ID(I))) THEN
C  ----------------------------------------------------------------------------
C   GET AIR TEMPERATURE AND SEA SURFACE TEMPERATURE DATA
                    CALL UFBINT(LUNIN,DATES,5,1,IRET,'SST1 TMDB')
                    if(DATES(1).ge.bmiss/2.0) then
                      SST=-99999.9
                    else    
                      SST=DATES(1)-273.15   !! Convert from Kelvin to deg C
                    endif
                    IF(SST.LT.0.0) SST=-99999.9

                    if(DATES(2).ge.bmiss/2.0) then
                      ATMP=-99999.9
                    else    
                      ATMP=DATES(2)-273.15  !! Convert from Kelvin to deg C
                    endif

                    IF(abs(SST).LT.40.00) THEN
                      IF(NTR_T(I).LT.1) THEN 
                        NTR_T(I)=NTR_T(I)+1
                        RTIME_T(I,NTR_T(I))=dayj
                        T_OBS(I,NTR_T(I))=SST
                      ELSE
                        IF(dayj.GT.RTIME_T(I,NTR_T(I))) THEN 
                          NTR_T(I)=NTR_T(I)+1
                          RTIME_T(I,NTR_T(I))=dayj
                          T_OBS(I,NTR_T(I))=SST
                        ENDIF
                      ENDIF
                    ENDIF

c  ------------------------------------------------------------------------------
c  GET RIVER STAGE DATA INCLUDING INFO ON USGS SENSOR TYPE 
c  Stage height is in meters
                    CALL UFBINT(LUNIN,DATES,5,1,IRET,'RSH29 STRV')
                    if(DATES(1).ge.bmiss/2.0) then
                      rsh29=-99999.9
                    else    
                      rsh29=DATES(1)  
                    endif
                    if(DATES(2).ge.bmiss/2.0) then
                      strv=-99999.9
                    else    
                      strv=DATES(2)  
                    endif

c  -----------------------------------------------------------------------------
c GET RIVER STAGE HEIGHT ABOVE NGVD 1929, STREAM VELOCITY, AND SALINITY
c  ----------------------------------------------------------------------------
                    CALL UFBINT(LUNIN,DATES,5,1,IRET,'RSHM DDRS')
                    if(DATES(1).ge.bmiss/2.0) then
                      EL=-99999.9
                    else    
                      EL=DATES(1)  
                    endif

                    IF(abs(EL).LT.40.0) THEN
                      IF(NTR(I).LT.1) THEN 
                        NTR(I)=NTR(I)+1
                        RTIME(I,NTR(I))=dayj
                        WL_OBS(I,NTR(I))=EL-DATUM(I)
                      ELSE
                        IF(dayj.GT.RTIME(I,NTR(I))) THEN 
                          NTR(I)=NTR(I)+1
                          RTIME(I,NTR(I))=dayj
                          WL_OBS(I,NTR(I))=EL-DATUM(I) 
c  Convert WL from USGS datum (e.g. NAVD88) to MSL for USGS stations
                        ENDIF
                      ENDIF
                    ENDIF

c  ----------------------------------------------------------------------------
c  GET CONDUCTIVITY AND SALINITY
c  Convert specific conductance C25,0 into conductivity. See Sea Bird Electronics, Inc
c  Fresh Water Conductivity Measurements using SBE-19 SEACAT Profiler
c  Specific C25,0 [us/cm]=(C/[1+0.02*(Temperature-25)] where C is in ms/cm 
c  ----------------------------------------------------------------------------
                    CALL UFBINT(LUNIN,DATES,5,1,IRET,'SALN WACN')
                    if(DATES(1).ge.bmiss/2.0) then
                      SALN=-99999.9
                    else    
                      SALN=DATES(1)
                    endif
                    if(DATES(2).ge.bmiss/2.0) then
                      COND=-99999.9        !! Unit of COND is uS/cm for USGS station
                    else    
                      COND=DATES(2)   
                    endif

                    IF(SALN.LT.-99999.0) THEN
                      IF((SST.GT.-99999.0).AND.(COND.GT.-99999.0)) THEN
                        COND=COND*(1.0+0.02*(SST-25.0))  !! Convert from uS/cm to mS/cm
                        SALN=SAL(COND,SST,0.0)
                      ENDIF   
                    ENDIF  
                    IF(SALN.LT.-0.5) SALN=-99999.9

                    IF(abs(SALN).LT.40.00) THEN
                      IF(NTR_S(I).LT.1) THEN 
                        NTR_S(I)=NTR_S(I)+1
                        RTIME_S(I,NTR_S(I))=dayj
                        S_OBS(I,NTR_S(I))=SALN
                      ELSE
                        IF(dayj.GT.RTIME_S(I,NTR_S(I))) THEN 
                          NTR_S(I)=NTR_S(I)+1
                          RTIME_S(I,NTR_S(I))=dayj
                          S_OBS(I,NTR_S(I))=SALN
                        ENDIF
                      ENDIF
                    ENDIF
                    write(15,780) trim(stnbufrid),clon,clat,IYR,IMM,
     &                IDD,IHH,IMN,EL,ATMP,SST,SALN,COND,SWL
                  ENDIF 
                ENDDO
              ENDDO
            ENDDO
            CALL CLOSBF(LUNIN)
            CLOSE(LUNIN)
          ENDIF   ! FEXIST
        ENDDO
      ENDIF

      CLOSE(15)
      INQUIRE(FILE=TRIM(FOUT),EXIST=FEXIST)
      IF(FEXIST) THEN
        CMD='sort '//TRIM(FOUT)//' > tmp.dat'
        call system(trim(CMD))
        CMD='uniq tmp.dat > tmp1.dat'
        call system(trim(CMD))
        CMD='cp tmp1.dat '//TRIM(FOUT)
        call system(trim(CMD))
      ENDIF     

c  -----------------------------------------------------------------------------
c  End of reading WL BUFR FILES and begin WL QC procedures
c  ----------------------------------------------------------------------------
      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF(ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF(ALLOCATED(oned4)) DEALLOCATE(oned4)
      allocate(oned1(NMAX))
      allocate(oned2(NMAX))
      allocate(oned3(NMAX))
      allocate(oned4(NMAX))

      DO I=1,NSTA
        IF(NTR(I).GE.2) THEN
          write(*,*) 'I=, NTR(I)= ',I,NTR(I)
          avg=0.0
          NTMP=0
          DO N=1,NTR(I)
            IF(ABS(WL_OBS(I,N)).LE.10.0) THEN
              NTMP=NTMP+1
              AVG=AVG+WL_OBS(I,N)
              ONED1(NTMP)=WL_OBS(I,N)
            ENDIF
          ENDDO 
          write(*,*) 'NTMP=', NTMP,(WL_OBS(I,K),K=1,5)
          IF(NTMP.GT.0) AVG=AVG/NTMP
          SD=0.0    
          IF(NTMP.GT.2) THEN
            DO N=1,NTMP
              SD=SD+(ONED1(N)-AVG)**2
            ENDDO 
            SD=SQRT(SD/(NTMP-1))
          ELSE
            SD=1.0
          ENDIF  
          BOUND_L=AVG-3.0*SD
          BOUND_U=AVG+3.0*SD 
          write(*,*) 'WL bound= ',BOUND_L,BOUND_U,AVG,NOS_ID(I)

          NTMP=0
          DO N=1,NTR(I)
            IF((WL_OBS(I,N).GE.BOUND_L).AND.
     &         (WL_OBS(I,N).LE.BOUND_U)) THEN
              NTMP=NTMP+1
              ONED1(NTMP)=WL_OBS(I,N)
              ONED2(NTMP)=RTIME(I,N)
            ENDIF
          ENDDO
          NTR(I)=NTMP
          DO N=1,NTMP
            WL_OBS(I,N)=ONED1(N) 
            RTIME(I,N)=ONED2(N)    
          ENDDO
        ENDIF   

c Gradient change checking Dh/Dt<0.7 meters/hour, and assume first data is good
        IF(NTR(I).GE.2) THEN
          NTMP=1
          ONED1(NTMP)=WL_OBS(I,1)
          ONED2(NTMP)=RTIME(I,1)
          DO N=2,NTR(I)
            GD=(WL_OBS(I,N)-ONED1(NTMP))/(RTIME(I,N)-ONED2(NTMP))/24.0
            IF(ABS(GD).LE.0.7) THEN
              NTMP=NTMP+1
              ONED1(NTMP)=WL_OBS(I,N)
              ONED2(NTMP)=RTIME(I,N)
            ENDIF
          ENDDO
          DO N=1,NTMP
            WL_OBS(I,N)=ONED1(N) 
            RTIME(I,N)=ONED2(N)    
          ENDDO
          NTR(I)=NTMP
          WRITE(*,*) I,' NTMP OF WL= ',NTMP,' NTR= ',NTR(I)
        ENDIF  
        BUFFER='NUMBER OF WL AT '//TRIM(NOS_ID(I))
        BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
        BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
        WRITE(ICORMS,*) TRIM(BUFFER),NTR(I)

c  -----------------------------------------------------------------------------
c  Begin QC procedures for Temperature
c  ----------------------------------------------------------------------------
        IF(NTR_T(I).GE.2) THEN
          avg=0.0
          NTMP=0
          DO N=1,NTR_T(I)
            IF(ABS(T_OBS(I,N)).LE.40.0) THEN    !Deg. C
              NTMP=NTMP+1
              AVG=AVG+T_OBS(I,N)
              ONED1(NTMP)=T_OBS(I,N)
            ENDIF
          ENDDO 
          IF(NTMP.GT.0) AVG=AVG/NTMP
          IF(NTMP.GT.2) THEN
            SD=0.0    
            DO N=1,NTMP
              SD=SD+(ONED1(N)-AVG)**2
            ENDDO 
            SD=SQRT(SD/(NTMP-1))
          ELSE
            SD=1.0
          ENDIF    
          BOUND_L=AVG-3.0*SD
          BOUND_U=AVG+3.0*SD 
          write(*,*) 'Temp bound = ',BOUND_L,BOUND_U,AVG,NOS_ID(I)

          NTMP=0
          DO N=1,NTR_T(I)
            IF((T_OBS(I,N).GE.BOUND_L).AND.
     &         (T_OBS(I,N).LE.BOUND_U)) THEN
              NTMP=NTMP+1
              ONED1(NTMP)=T_OBS(I,N)
              ONED2(NTMP)=RTIME_T(I,N)
            ENDIF
          ENDDO
          NTR_T(I)=NTMP
          DO N=1,NTMP
            T_OBS(I,N)=ONED1(N) 
            RTIME_T(I,N)=ONED2(N)    
          ENDDO 
        ENDIF  

c Gradient change checking  DT/Dt < 2.0 deg./hour, and assume first data is good
        IF(NTR_T(I).GE.2) THEN
          NTMP=1
          ONED1(NTMP)=T_OBS(I,1)
          ONED2(NTMP)=RTIME_T(I,1)
          DO N=2,NTR_T(I)  
            GD=(T_OBS(I,N)-ONED1(NTMP))/(RTIME_T(I,N)-ONED2(NTMP))/24.0
            IF( ABS(GD).LE.2.0) THEN
              NTMP=NTMP+1
              ONED1(NTMP)=T_OBS(I,N)
              ONED2(NTMP)=RTIME_T(I,N)
            ENDIF
          ENDDO
          WRITE(*,*) I,' NTMP of TEMP= ',NTMP,' NTR= ',NTR_T(I)
          NTR_T(I)=NTMP
          DO N=1,NTMP
            T_OBS(I,N)=ONED1(N)
            RTIME_T(I,N)=ONED2(N)    
          ENDDO
        ENDIF  

c  -----------------------------------------------------------------------------
c  Begin QC procedures for Salinity
c  ----------------------------------------------------------------------------
        IF(NTR_S(I).GE.2) THEN
          avg=0.0
          NTMP=0
          DO N=1,NTR_S(I)
            IF(ABS(S_OBS(I,N)).LE.40.0) THEN    ! PSU
              NTMP=NTMP+1
              AVG=AVG+S_OBS(I,N)
              ONED1(NTMP)=S_OBS(I,N)
            ENDIF
          ENDDO 
          IF(NTMP.GT.0) G=AVG/NTMP
          IF(NTMP.GT.2) THEN
            SD=0.0    
            DO N=1,NTMP
              SD=SD+(ONED1(N)-AVG)**2
            ENDDO 
            SD=SQRT(SD/(NTMP-1))
          ELSE
            SD=1.0
          ENDIF    
          BOUND_L=AVG-3.0*SD
          BOUND_U=AVG+3.0*SD 
          write(*,*) 'Salt bound = ',BOUND_L,BOUND_U,AVG,NOS_ID(I)
          NTMP=0
          DO N=1,NTR_S(I)
            IF((S_OBS(I,N).GE.BOUND_L).AND.
     &         (S_OBS(I,N).LE.BOUND_U)) THEN
              NTMP=NTMP+1
              ONED1(NTMP)=S_OBS(I,N)
              ONED2(NTMP)=RTIME_S(I,N)
            ENDIF
          ENDDO
          NTR_S(I)=NTMP
          DO N=1,NTMP
            S_OBS(I,N)=ONED1(N) 
            RTIME_S(I,N)=ONED2(N)    
          ENDDO 
        ENDIF  

        IF(NTR_S(I).GE.2) THEN
c Gradient change checking  DS/Dt < 3.0 deg./hour, and assume first data is good
          NTMP=1
          ONED1(NTMP)=S_OBS(I,1)
          ONED2(NTMP)=RTIME_S(I,1)
          DO N=2,NTR_S(I)  
            GD=(S_OBS(I,N)-ONED1(NTMP))/(RTIME_S(I,N)-ONED2(NTMP))/24.0
            IF(ABS(GD).LE.3.0) THEN
              NTMP=NTMP+1
              ONED1(NTMP)=S_OBS(I,N)
              ONED2(NTMP)=RTIME_S(I,N)
            ENDIF
          ENDDO
          WRITE(*,*) I,' NTMP of SALT= ',NTMP,' NTR_S= ',NTR_S(I)
          NTR_S(I)=NTMP
          DO N=1,NTMP
            S_OBS(I,N)=ONED1(N) 
            RTIME_S(I,N)=ONED2(N)    
          ENDDO
        ENDIF  
      ENDDO    

c  -----------------------------------------------------------------------------
c  End of  QC procedures
c  Write out observations for diagnosis
c  -------------------o---------------------------------------------------------
      IF(TRIM(DBASE_WL).eq.'OBS') THEN
        DO I=1,NSTA
          DO N=1,NTR(I)
            SWL_OBS(I,N)=WL_OBS(I,N)
          ENDDO
        ENDDO
      ENDIF

      DO I=1,NSTA
        FOUT=TRIM(NOS_ID(I))//'.obs'
        CLOSE(10)
        OPEN(10,file=TRIM(FOUT))
        DO N=1,NTR(I)
          IF(ABS(WL_OBS(I,N)).LT.10.0) THEN
            jday=RTIME(I,N)+jbase_date
            call CALENDARDATE(jday,yearb,monthb,dayb,hourb,
     &           IYR,IMM,IDD,IHH,IMN,ISEC)
            WRITE(10,900) RTIME(I,N),IYR,IMM,IDD,IHH,IMN,
     &        WL_PRD(I,N),WL_OBS(I,N),SWL_OBS(I,N),T_OBS(I,N)
          ENDIF
        ENDDO   !N=1,NTR
        write(*,*) 'STA= ',trim(NOS_ID(I)),' Number of WL obs= ',
     &    NTR(I)
        WRITE(ICORMS,*) 'STA= ',trim(NOS_ID(I)),
     &    ' Number of WL obs= ',NTR(I) 
      ENDDO    !I=1,NSTA
900   FORMAT(f10.4,1x,I5,4i3,4F12.4)

c-----------------------------------------------------------------------
c If there is no WL obs at primary station, then looking for its backup station
c-----------------------------------------------------------------------
      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF(ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF(ALLOCATED(oned4)) DEALLOCATE(oned4)
      IF(ALLOCATED(AVGERR)) DEALLOCATE(AVGERR)
      IF(ALLOCATED(AVGERR_T)) DEALLOCATE(AVGERR_T)
      IF(ALLOCATED(AVGERR_S)) DEALLOCATE(AVGERR_S)
      allocate(oned1(NMAX))
      allocate(oned2(NMAX))
      allocate(oned3(NMAX))
      allocate(oned4(NMAX))
      allocate(AVGERR(NSTA))
      allocate(AVGERR_T(NSTA))
      allocate(AVGERR_S(NSTA))

      DO I=1,NSTA
        IF(WL_FLAG(I).EQ.0) THEN
          BUFFER='NUMBER OF WL AT '//TRIM(NOS_ID(I))
          BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
          BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
          WRITE(ICORMS,*) TRIM(BUFFER),NTR(I)
          IF(NTR(I).LE.20) THEN  
cc  Above 20 can be changed depending upon how many data are required
            IBKP=BACKUP_SID(I)
            IF(IBKP.GT.0) THEN
              IF(NTR(IBKP).GT.20) THEN
                write(*,*) 'Using backup ',IBKP, ' for I= ',I
                NTR(I)=NTR(IBKP)
                DO N=1,NTR(I)
                  RTIME(I,N)=RTIME(IBKP,N)
                  WL_OBS(I,N)=AS(IBKP)*WL_OBS(IBKP,N)
                  SWL_OBS(I,N)=AS(IBKP)*SWL_OBS(IBKP,N)
                ENDDO  
              ELSE
                IBKP1=BACKUP_SID(IBKP)
                IF(IBKP1.GT.0) THEN
                  IF(NTR(IBKP1).GT.20) THEN
                    write(*,*) 'Use second backup ',IBKP, ' for I= ',I
                    NTR(I)=NTR(IBKP1)
                    DO N=1,NTR(I)
                      RTIME(I,N)=RTIME(IBKP1,N)
                      WL_OBS(I,N)=AS(IBKP1)*WL_OBS(IBKP1,N)
                      SWL_OBS(I,N)=AS(IBKP1)*SWL_OBS(IBKP1,N)
                    ENDDO
                  ELSE
                    WRITE(*,*) 'No sufficient primary and '//
     &                'back-up WL data for station',I
                    WRITE(ICORMS,*) 'No sufficient primary '//
     &                'and back-up WL data for station',I 
                    DO IZ=INT(day_start),INT(day_start-1),-1
                      jday=IZ+jbase_date
                      call CALENDARDATE(jday,yearb,monthb,dayb,hourb,
     &                     IYR,IMM,IDD,IHH,IMN,ISEC)
                      BUFFER1=trim(PREFIXNOS)//'.obc.avg-wl.'//
     &                  TRIM(NOS_ID(I))//'.dat'
                      WRITE(BUFRFILE,'(I4.4,2I2.2)') IYR,IMM,IDD 
                      FIN=trim(COMOUT00)//'/'//trim(OFS)//'.'//
     &                    trim(BUFRFILE)//trim(BUFFER1)                      
                      INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
                      IF(FEXIST) EXIT
                    ENDDO ! check current and previous day directory for the average wl file

c READ AVERAGED WL FROM PREVIOUS CYCLE
                    CLOSE(10)
                    OPEN(10,FILE=trim(FIN))
                    READ(10,*) DUMMY
                    DO N=1,NREC
                      RTIME(I,N)=TIME_M(N)
                      SWL_OBS(I,N)=DUMMY
                    ENDDO                                       
                  ENDIF
                ELSE
                  WRITE(*,*) 'No sufficient primary and back-up '//
     &              'WL data for station',I
                  WRITE(ICORMS,*) 'No sufficient primary and '//
     &              'back-up WL data for station',I 
                  DO IZ=INT(day_start),INT(day_start-1),-1
                    jday=IZ+jbase_date
                    call CALENDARDATE(jday,yearb,monthb,dayb,hourb,
     &                   IYR,IMM,IDD,IHH,IMN,ISEC)
                    BUFFER1=trim(PREFIXNOS)//'.obc.avg-wl.'//
     &                TRIM(NOS_ID(I))//'.dat'
                    WRITE(BUFRFILE,'(I4.4,2I2.2)') IYR,IMM,IDD 
                    FIN=trim(COMOUT00)//'/'//trim(OFS)//'.'//
     &                  trim(BUFRFILE)//trim(BUFFER1)                      
                    INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
                    IF(FEXIST) EXIT
                  ENDDO ! check current and previous day directory for the average wl file

c READ AVERAGED WL FROM PREVIOUS CYCLE
                  CLOSE(10)
                  OPEN(10,FILE=trim(FIN))
                  READ(10,*) DUMMY
                  DO N=1,NREC
                    RTIME(I,N)=TIME_M(N)
                    SWL_OBS(I,N)=DUMMY
                  ENDDO                                       
                ENDIF             
              ENDIF
            ENDIF
          ENDIF  

c-----------------------------------------------------------------------
c Use 6-hour ramping up  
c Interpolate into same time intreval as zeta_time  
c Filling missing values using linear interplolation 
c-----------------------------------------------------------------------
          IF(NTR(I).GT.20) THEN
            FOUT=TRIM(NOS_ID(I))//'.swl'
            CLOSE(10)
            OPEN(10,file=TRIM(FOUT))
            DO N=1,NTR(I)
              WRITE(10,35) RTIME(I,N),SWL_OBS(I,N)
            ENDDO
            CLOSE(10)
            N0=0
            AVG=0.0
            DO N1=1,NTR(I)
              IF(SWL_OBS(I,N1).GT.-10.0) THEN
                N0=N0+1
                ONED1(N0)=RTIME(I,N1)
                ONED2(N0)=SWL_OBS(I,N1)
                AVG=AVG+SWL_OBS(I,N1)
              ENDIF
            ENDDO

            IF(N0.NE.0) AVG=AVG/N0
            FIN=trim(PREFIXNOS)//'.obc.avg-wl.'//
     &        TRIM(NOS_ID(I))//'.dat'
            CLOSE(10)
            OPEN(10,FILE=trim(FIN))
            WRITE(10,*) AVG
            CLOSE(10)

            NTR(I)=N0
            DO N1=1,NTR(I)
              RTIME(I,N1)=ONED1(N1)
              SWL_OBS(I,N1)=ONED2(N1)
            ENDDO

            TIME1=RTIME(I,1)
            TIME2=RTIME(I,NTR(I))
            N0=0
            IF(TIME1.GT.zeta_time(1)) THEN
              N0=N0+1
              ONED1(N0)=zeta_time(1)
              ONED2(N0)=SWL_OBS(I,1)
            ENDIF
            DO N1=1,NTR(I)
              N0=N0+1
              ONED1(N0)=RTIME(I,N1)
              ONED2(N0)=SWL_OBS(I,N1)
            ENDDO  
            write(*,*) 'N0=',N0,' NTR(I)=',NTR(I)

            DO N1=2,NTMAX_WL
              IF((zeta_time(N1).GE.TIME2)) THEN
                N0=N0+1
                ONED1(N0)=zeta_time(N1)
                ONED2(N0)=SWL_OBS(I,NTR(I))
                EXIT
              ENDIF
            ENDDO
            write(*,*) 'N0=',N0,' NTR(I)=',NTR(I)
            NTR(I)=N0
            DO N1=1,NTR(I)
              RTIME(I,N1)=ONED1(N1)
              SWL_OBS(I,N1)=ONED2(N1)
            ENDDO
            TIME1=RTIME(I,1)
            TIME2=RTIME(I,NTR(I))
            WRITE(*,*) 'TIME1=',TIME1,' TIME2=',TIME2,zeta_time(1),
     &        zeta_time(NTMAX_WL),' NTR=',NTR(I),NTMAX_WL

            N0=0
            DO N1=1,NTMAX_WL
              IF((zeta_time(N1).GE.TIME1).AND.
     &           (zeta_time(N1).LE.TIME2)) THEN
                N0=N0+1
                ONED3(N0)=zeta_time(N1)
              ENDIF
            ENDDO
            IF(N0.LT.1) THEN
              WRITE(*,*) 'WARNING MESSAGE: Real time water level data is insufficient'
              WRITE(*,*) 'Please wait for 20 min and then check '
              WRITE(*,*) 'File size of ', TRIM(NOSBUFR),' and ',  TRIM(USGSBUFR)
              WRITE(*,*) 'rerun prep job' 
              WRITE(*,*) 'TIME1=',TIME1,' TIME2=',TIME2,zeta_time(1),
     &          zeta_time(NTMAX_WL),' NTR=',NTR(I),NTMAX_WL
              STOP
            ENDIF 

            NTMP=0 
            DO N=1,NTR(I)
              IF(ABS(SWL_OBS(I,N)).LE.3.0) THEN
                NTMP=NTMP+1
                ONED1(NTMP)=RTIME(I,N)
                ONED2(NTMP)=SWL_OBS(I,N)
              ENDIF  
            ENDDO  
            IF(NTMP.GT.2) THEN
              CALL lineararray(N0,ONED3,oned4,NTMP,ONED1,ONED2)
            ELSE
              DO N=1,N0
                oned4(N)=0.0
              ENDDO
            ENDIF  

c-----------------------------------------------------------------------
c  Separate bias as mean error + time varying difference
c-----------------------------------------------------------------------          
            AVG=0.0
            DO N=1,N0
              DO N1=1,NTMAX_WL
                IF(ABS(ONED3(N)-zeta_time(N1)).LT.1.0e-10) THEN
                  ONED1(N)=ONED4(N)-WLOBC(GRIDID_STA(I),N1)
c avg=SWL-ETSS at the corresponding grid
                  AVG=AVG+ONED1(N)
                  EXIT
                ENDIF       
              ENDDO
            ENDDO

            IF(N0.GT.0) AVG=AVG/N0
            AVGERR(I)=AVG
            write(*,*) 'Mean err of WL=', I, AVGERR(I)
            DO N=1,N0
              ONED4(N)=ONED1(N)-AVG
            ENDDO  

            IF(ONED3(N0).LT.day_end) THEN
              N0=N0+1
              ONED3(N0)=ONED3(N0-1)+6.0/24.0  
c Use 6-hour ramping up from last err' to zero     
              ONED4(N0)=0.0
            ENDIF
            CALL lineararray(NTMAX_WL,zeta_time,oned1,N0,ONED3,ONED4)
            DO N=1,NTMAX_WL
              SWL_OBS(I,N)=oned1(N)
              IF(zeta_time(N).GT.ONED3(N0)) SWL_OBS(I,N)=0.0 
            ENDDO
          ELSE
            AVGERR(I)=0.0
            DO N=1,NTMAX_WL
              SWL_OBS(I,N)=0.0
            ENDDO
          ENDIF      
        ENDIF   

c-----------------------------------------------------------------------
c  Process water temperature for stations TS_FLAG > 0
c-----------------------------------------------------------------------
        IF(TS_FLAG(I).EQ.0) THEN
           AVGERR_T(I)=0.0
           DO N=1,NTMAX
             T_OBS(I,N)=0.0
           ENDDO
        ELSEIF(TS_FLAG(I).EQ.1) THEN   !! need real time T & S observations
          IF(NTR_T(I).GT.5) THEN  
c  Above 5 can be changed depending upon how many data are required
            BUFFER='NUMBER OF TEMP AT '//TRIM(NOS_ID(I))
            BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
            BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
            WRITE(ICORMS,*)TRIM(BUFFER),NTR_T(I)
          ELSE  
            BUFFER='WARNING: no real time temp obs. at '//TRIM(NOS_ID(I))
            WRITE(*,*)TRIM(BUFFER)
            WRITE(*,*)'Check its backup'
            IBKP=BACKUP_SID(I)
            IF(IBKP.GT.0) THEN
              IF(NTR_T(IBKP).GT.5) THEN
                BUFFER='USING BACKUP STATION OF '//TRIM(NOS_ID(IBKP))
                WRITE(ICORMS,*) TRIM(BUFFER)
                write(*,*) 'Using first backup ',IBKP,' for T at I= ',I
                NTR_T(I)=NTR_T(IBKP)
                DO N=1,NTR_T(I)
                  RTIME_T(I,N)=RTIME_T(IBKP,N)
                  T_OBS(I,N)=T_OBS(IBKP,N)
                ENDDO  
              ELSE
                IBKP1=BACKUP_SID(IBKP)
                IF(IBKP1.GT.0) THEN
                  IF(NTR_T(IBKP1).GT.5) THEN
                    BUFFER='USING BACKUP STATION OF '//
     &                TRIM(NOS_ID(IBKP1))
                    WRITE(ICORMS,*) TRIM(BUFFER)
                    write(*,*) 'Use second backup ',IBKP,
     &                ' for T at I= ',I
                    NTR_T(I)=NTR_T(IBKP1)
                    DO N=1,NTR_T(I)
                      RTIME_T(I,N)=RTIME_T(IBKP1,N)
                      T_OBS(I,N)=T_OBS(IBKP1,N)
                    ENDDO
                  ELSE
                    write(*,*) 'WARNING: No observation is at second backup'
                    write(*,*) 'Use climatology for ',trim(NOS_ID(I))
                    BUFFER='NO OBSERVATIONS, USE CLIMATOLOGIC DATASET'
                    WRITE(ICORMS,*) TRIM(BUFFER)
                    NTR_T(I)=NT_CLIM
                    DO N=1,NTR_T(I)
                      RTIME_T(I,N)=TIME_CLIM(N)
                      T_OBS(I,N)=T_CLIM(I,N,1)
                    ENDDO
                  ENDIF         
                ELSE   
                  write(*,*) 'WARNING: No observation is at second backup'
                  write(*,*) 'Use climatology for ',trim(NOS_ID(I))
                  BUFFER='NO OBSERVATIONS, USE CLIMATOLOGIC DATASET'
                  WRITE(ICORMS,*) TRIM(BUFFER)
                  NTR_T(I)=NT_CLIM
                  DO N=1,NTR_T(I)
                    RTIME_T(I,N)=TIME_CLIM(N)
                    T_OBS(I,N)=T_CLIM(I,N,1)
                  ENDDO
                ENDIF         
              ENDIF
            ELSE     !! use climatology 
              write(*,*) 'Use climatology for I= ',I,trim(NOS_ID(I))
              NTR_T(I)=NT_CLIM
              DO N=1,NTR_T(I)
                RTIME_T(I,N)=TIME_CLIM(N)
                T_OBS(I,N)=T_CLIM(I,N,1)    !! use SST only
              ENDDO
            ENDIF         
          ENDIF
        ELSEIF(TS_FLAG(I).EQ.2) THEN   !! correction using climatological dataset
          WRITE(ICORMS,*) 'USE CLIMATOLOGIC DATASET'
          NTR_T(I)=NT_CLIM
          DO N=1,NTR_T(I)
            RTIME_T(I,N)=TIME_CLIM(N)
            T_OBS(I,N)=T_CLIM(I,N,1)        !! use SST only
          ENDDO
        ENDIF

        IF(TS_FLAG(I).GT.0) THEN     !! correction is needed
          FOUT=TRIM(NOS_ID(I))//'.temp'
          CLOSE(10)
          OPEN(10,file=TRIM(FOUT))
          DO N=1,NTR_T(I)
            WRITE(10,35)RTIME_T(I,N),T_OBS(I,N)
          ENDDO
          CLOSE(10)

c-----------------------------------------------------------------------
c  Separate bias as mean error + time varying difference
c-----------------------------------------------------------------------
          N0=0
          DO N1=1,NTR_T(I)
            IF(T_OBS(I,N1).GT.-10.0) THEN
              N0=N0+1
              ONED1(N0)=RTIME_T(I,N1)
              ONED2(N0)=T_OBS(I,N1)
            ENDIF
          ENDDO
          NTR_T(I)=N0
          DO N1=1,NTR_T(I)
            RTIME_T(I,N1)=ONED1(N1)
            T_OBS(I,N1)=ONED2(N1)
          ENDDO
          TIME1=RTIME_T(I,1)
          TIME2=RTIME_T(I,NTR_T(I))

          N0=0
          IF(TIME1.GT.TS_time(1)) THEN
            N0=N0+1
            ONED1(N0)=TS_time(1)
            ONED2(N0)=T_OBS(I,1)
          ENDIF
          DO N1=1,NTR_T(I)
            N0=N0+1
            ONED1(N0)=RTIME_T(I,N1)
            ONED2(N0)=T_OBS(I,N1)
          ENDDO  
          write(*,*) 'N0=',N0,' NTR_T(I)=',NTR_T(I)

          DO N1=2,NTMAX
            IF((TS_time(N1).GE.TIME2)) THEN
              N0=N0+1
              ONED1(N0)=TS_time(N1)
              ONED2(N0)=T_OBS(I,NTR_T(I))
              EXIT
            ENDIF
          ENDDO
          write(*,*) 'N0=',N0,' NTR_T(I)=',NTR_T(I)

          NTR_T(I)=N0
          DO N1=1,NTR_T(I)
            RTIME_T(I,N1)=ONED1(N1)
            T_OBS(I,N1)=ONED2(N1)
          ENDDO     
          TIME1=RTIME_T(I,1)
          TIME2=RTIME_T(I,NTR_T(I))
          WRITE(*,*) 'TIME1=',TIME1,' TIME2=',TIME2,TS_time(1),
     &      TS_time(NTMAX),' NTR_T=',NTR_T(I),NTMAX
    
          N0=0
          DO N1=1,NTMAX
            IF((TS_time(N1).GE.TIME1).AND.
     &         (TS_time(N1).LE.TIME2)) THEN
              N0=N0+1
              ONED3(N0)=TS_time(N1)
            ENDIF
          ENDDO

          IF(N0.LT.1) THEN
            WRITE(*,*) 'WARNING MESSAGE: Real time temperature data is insufficient'
            WRITE(*,*) 'Please wait for 20 minutes and then check '
            WRITE(*,*) 'File size of ', TRIM(NOSBUFR),' and ',  TRIM(USGSBUFR)
            WRITE(*,*) 'Rerun prep job' 
            write(*,*) 'I=',I,TIME1,TIME2,TS_time(1),TS_time(NTMAX)
          ENDIF  
          DO N=1,NTR_T(I)
            ONED1(N)=RTIME_T(I,N)
            ONED2(N)=T_OBS(I,N)
          ENDDO  
          CALL lineararray(N0,ONED3,oned4,NTR_T(I),ONED1,ONED2)
          AVG=0.0
          DO N=1,N0
            DO N1=1,NTMAX
              IF(ABS(ONED3(N)-TS_time(N1)).LT.1.0e-10) THEN
                ONED1(N)=ONED4(N)-TEMPOBC_M(GRIDID_STA(I),1,N1)
c avg=Tobs - NCOM at surface at the corresponding grid
                AVG=AVG+ONED1(N)
                EXIT
              ENDIF       
            ENDDO
          ENDDO
          IF(N0.GT.0) AVG=AVG/N0
          AVGERR_T(I)=AVG
          WRITE(*,*) 'Mean err of SST=',I,AVGERR_T(I)
          DO N=1,N0
            ONED4(N)=ONED1(N)-AVG   !! err = avg + err'
          ENDDO  
          IF(ONED3(N0).LT.day_end) THEN
            N0=N0+1
            ONED3(N0)=ONED3(N0-1)+6.0/24.0  
c Use 6-hour ramping up from last err' to zero     
            ONED4(N0)=0.0
          ENDIF
          CALL lineararray(NTMAX,TS_time,oned1,N0,ONED3,ONED4)
          DO N=1,NTMAX
            T_OBS(I,N)=oned1(N)
            IF(TS_time(N).GT.ONED3(N0)) T_OBS(I,N)=0.0 
          ENDDO
        ENDIF  

c-----------------------------------------------------------------------
c   Process salinity for stations TS_FLAG > 0
C-----------------------------------------------------------------------
        IF(TS_FLAG(I).EQ.0) THEN   !! no correction is needed
          AVGERR_S(I)=0.0
          DO N=1,NTMAX
            S_OBS(I,N)=0.0
          ENDDO
        ELSEIF(TS_FLAG(I).EQ.1) THEN   !! need real time T & S observations
          IF(NTR_S(I).GT.5) THEN
            BUFFER='NUMBER OF SALT AT '//TRIM(NOS_ID(I))
            BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
            BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
            WRITE(ICORMS,*) TRIM(BUFFER),NTR_S(I)
          ELSE 
            IBKP=BACKUP_SID(I)
            IF(IBKP.GT.0) THEN
              IF(NTR_S(IBKP).GT.5) THEN
                BUFFER='USING BACKUP STATION OF '//TRIM(NOS_ID(IBKP))
                WRITE(ICORMS,*) TRIM(BUFFER)
                write(*,*) 'Using backup ',IBKP, ' for SAL at I= ',I
                NTR_S(I)=NTR_S(IBKP)
                DO N=1,NTR_S(I)
                  RTIME_S(I,N)=RTIME_S(IBKP,N)
                  S_OBS(I,N)=S_OBS(IBKP,N)
                ENDDO  
              ELSE
                IBKP1=BACKUP_SID(IBKP)
                IF(IBKP1.GT.0) THEN
                  IF(NTR_S(IBKP1).GT.5) THEN
                    BUFFER='USING BACKUP STATION OF '//
     &                TRIM(NOS_ID(IBKP1))
                    WRITE(ICORMS,*) TRIM(BUFFER)
                    write(*,*) 'Use second backup ',IBKP, ' for I= ',I
                    NTR_S(I)=NTR_S(IBKP1)
                    DO N=1,NTR_S(I)
                      RTIME_S(I,N)=RTIME_S(IBKP1,N)
                      S_OBS(I,N)=S_OBS(IBKP1,N)
                    ENDDO
                  ELSE
                    write(*,*) 'Use salinity climatology for ',
     &                trim(NOS_ID(I))
                    NTR_S(I)=NT_CLIM
                    DO N=1,NTR_S(I)
                      RTIME_S(I,N)=TIME_CLIM(N)
                      S_OBS(I,N)=S_CLIM(I,N,1)    !! use SST only
                    ENDDO
                  ENDIF
                ELSE 
                  write(*,*) 'Use salinity climatology for ',
     &              trim(NOS_ID(I))
                  NTR_S(I)=NT_CLIM
                  DO N=1,NTR_S(I)
                    RTIME_S(I,N)=TIME_CLIM(N)
                    S_OBS(I,N)=S_CLIM(I,N,1)      !! use SST only
                  ENDDO
                ENDIF         
              ENDIF
            ELSE     !! use climatology 
              write(*,*) 'Use salinity climatology for I= ',
     &          I,trim(NOS_ID(I))
              NTR_S(I)=NT_CLIM
              DO N=1,NTR_S(I)
                RTIME_S(I,N)=TIME_CLIM(N)
                S_OBS(I,N)=S_CLIM(I,N,1)          !! use SST only
              ENDDO
            ENDIF         
          ENDIF
        ELSEIF(TS_FLAG(I).EQ.2) THEN   !! correction using climatological dataset
          NTR_S(I)=NT_CLIM
          DO N=1,NTR_S(I)
            RTIME_S(I,N)=TIME_CLIM(N)
            S_OBS(I,N)=S_CLIM(I,N,1)              !! use SST only
          ENDDO
        ENDIF  
         
        IF(TS_FLAG(I).GT.0) THEN !!  correction is needed
          FOUT=TRIM(NOS_ID(I))//'.salt'
          CLOSE(10)
          OPEN(10,file=TRIM(FOUT))
          DO N=1,NTR_S(I)
            WRITE(10,35) RTIME_S(I,N),S_OBS(I,N)
          ENDDO
          CLOSE(10)

c-----------------------------------------------------------------------
c  Separate bias as mean error + time varying difference
c-----------------------------------------------------------------------
          N0=0
          DO N1=1,NTR_S(I)
            IF(S_OBS(I,N1).GT.-0.1) THEN
              N0=N0+1
              ONED1(N0)=RTIME_S(I,N1)
              ONED2(N0)=S_OBS(I,N1)
            ENDIF
          ENDDO
          NTR_S(I)=N0
          DO N1=1,NTR_S(I)
            RTIME_S(I,N1)=ONED1(N1)
            S_OBS(I,N1)=ONED2(N1)
          ENDDO
          TIME1=RTIME_S(I,1)
          TIME2=RTIME_S(I,NTR_S(I))
          N0=0
          IF(TIME1.GT.TS_time(1)) THEN
            N0=N0+1
            ONED1(N0)=TS_time(1)
            ONED2(N0)=S_OBS(I,1)
          ENDIF
          DO N1=1,NTR_S(I)
            N0=N0+1
            ONED1(N0)=RTIME_S(I,N1)
            ONED2(N0)=S_OBS(I,N1)
          ENDDO  
          write(*,*) 'N0=',N0,' NTR_S(I)=',NTR_S(I)

          DO N1=2,NTMAX
            IF((TS_time(N1).GE.TIME2)) THEN
              N0=N0+1
              ONED1(N0)=TS_time(N1)
              ONED2(N0)=S_OBS(I,NTR_S(I))
              EXIT
            ENDIF
          ENDDO
          write(*,*) 'N0=',N0,' NTR_S(I)=',NTR_S(I)

          NTR_S(I)=N0
          DO N1=1,NTR_S(I)
            RTIME_S(I,N1)=ONED1(N1)
            S_OBS(I,N1)=ONED2(N1)
          ENDDO     
          TIME1=RTIME_S(I,1)
          TIME2=RTIME_S(I,NTR_S(I))
          WRITE(*,*) 'TIME1=',TIME1,' TIME2=',TIME2,TS_time(1),
     &      TS_time(NTMAX),' NTR_S=',NTR_S(I),NTMAX

          N0=0
          DO N1=1,NTMAX
            IF((TS_time(N1).GE.TIME1).AND.
     &         (TS_time(N1).LE.TIME2)) THEN
              N0=N0+1
              ONED3(N0)=TS_time(N1)
            ENDIF
          ENDDO
          IF(N0.LT.1) THEN
            WRITE(*,*) 'WARNING MESSAGE: Real time salinity data is insufficient'
            WRITE(*,*) 'Please wait for 20 minutes and then check'
            WRITE(*,*) 'File size of ', TRIM(NOSBUFR),' and ',  TRIM(USGSBUFR)
            WRITE(*,*) 'Rerun prep job' 
            write(*,*) 'I=',I,TIME1,TIME2,TS_time(1),TS_time(NTMAX)
            STOP
          ENDIF  
          DO N=1,NTR_S(I)
            ONED1(N)=RTIME_S(I,N)
            ONED2(N)=S_OBS(I,N)
          ENDDO  
          CALL lineararray(N0,ONED3,oned4,NTR_S(I),ONED1,ONED2)
          AVG=0.0
          DO N=1,N0
            DO N1=1,NTMAX
              IF(ABS(ONED3(N)-TS_time(N1)).LT.1.0e-10) THEN
                ONED1(N)=ONED4(N)-SALTOBC_M(GRIDID_STA(I),1,N1)
                AVG=AVG+ONED1(N)
                EXIT
              ENDIF       
            ENDDO
          ENDDO
          IF(N0.GT.0) AVG=AVG/N0
          AVGERR_S(I)=AVG
          WRITE(*,*) 'Mean err of SSS=',I,AVGERR_S(I)
          DO N=1,N0
            ONED4(N)=ONED1(N)-AVG
          ENDDO  
          IF(ONED3(N0).LT.day_end) THEN
            N0=N0+1
            ONED3(N0)=ONED3(N0-1)+6.0/24.0     
            ONED4(N0)=0.0
          ENDIF
          CALL lineararray(NTMAX,TS_time,oned1,N0,ONED3,ONED4)
          DO N=1,NTMAX
            S_OBS(I,N)=oned1(N)
            IF(TS_time(N).GT.ONED3(N0)) S_OBS(I,N)=0.0 
          ENDDO
        ENDIF  
      ENDDO

c Calculate  WL_OFFSET
      CALL WL_CORRECTION(OFS,COMOUT00,NOSWLDIR,NOSBUFR,
     &  WL_CORRECTION_CTL,CORRECTION_OLD,START_TIME,NOBC,
     &  WL_OFFSET)

c-----------------------------------------------------------------------
c  Correcting OBC by the difference between obs - ETSS/NCOM
c-----------------------------------------------------------------------
      DO I=1,NOBC
        ID1=WL_SID_1(I)
        ID2=WL_SID_2(I)
        sc1=WL_S_1(I)
        sc2=WL_S_2(I)
        IF(WL_STA(I).EQ.1) THEN
          IF(ID1.GT.0) THEN
            DO N=1,NTMAX_WL
              WLOBC(I,N)=WLOBC(I,N)+sc1*(AVGERR(ID1)+SWL_OBS(ID1,N))
            ENDDO
          ENDIF 
        ELSEIF(WL_STA(I).EQ.2) THEN
          IF(ID1.GT.0.AND.ID2.GT.0) THEN
            DO N=1,NTMAX_WL
              WLOBC(I,N)=WLOBC(I,N)+sc1*(AVGERR(ID1)+SWL_OBS(ID1,N))
     &                             +sc2*(AVGERR(ID2)+SWL_OBS(ID2,N))
            ENDDO
          ENDIF 
        ENDIF

c ADD WL CORRECTION
        DO N=1,NTMAX_WL
          WLOBC(I,N)=WLOBC(I,N)-WL_OFFSET(I)
        ENDDO
      
        ID1=TS_SID_1(I)
        ID2=TS_SID_2(I)
        sc1=TS_S_1(I)
        sc2=TS_S_2(I)
        IF(TS_STA(I).EQ.1) THEN
          IF(ID1.GT.0) THEN
            DO N=1,NTMAX
              DO K=1,KBm
                TEMPOBC_M(I,K,N)=TEMPOBC_M(I,K,N)+
     &            sc1*(AVGERR_T(ID1)+T_OBS(ID1,N))
                SALTOBC_M(I,K,N)=SALTOBC_M(I,K,N)+
     &            sc1*(AVGERR_S(ID1)+S_OBS(ID1,N))
              ENDDO
            ENDDO
          ENDIF 
        ELSEIF(TS_STA(I).EQ.2) THEN
          IF(ID1.GT.0.AND.ID2.GT.0) THEN
            DO N=1,NTMAX
              DO K=1,KBm
                TEMPOBC_M(I,K,N)=TEMPOBC_M(I,K,N)+
     &            sc1*(AVGERR_T(ID1)+T_OBS(ID1,N))+
     &            sc2*(AVGERR_T(ID2)+T_OBS(ID2,N))
                SALTOBC_M(I,K,N)=SALTOBC_M(I,K,N)+
     &            sc1*(AVGERR_S(ID1)+S_OBS(ID1,N))+
     &            sc2*(AVGERR_S(ID2)+S_OBS(ID2,N))
              ENDDO
            ENDDO
          ENDIF 
        ENDIF
      ENDDO     

      DO I=1,NOBC
        DO N=1,NTMAX
          DO K=1,KBm
            IF(SALTOBC_M(I,K,N).LE.0.0) THEN
              SALTOBC_M(I,K,N)=0.0
            ENDIF
          ENDDO
        ENDDO
      ENDDO        

c -------------------------------------------------------------------
c   Print OBC for evaluation
      OPEN(33,file='WL_OBC_ajdusted.dat')
      DO N=1,NTMAX_WL
        write(33,35) ZETA_TIME(N),(WLOBC(I,N),I=1,NOBC,10),
     &    WLOBC(NOBC,N)
      ENDDO
      CLOSE(33)

      OPEN(33,file='TEMP_OBC_ajdusted.dat')
      DO N=1,NTMAX
        DO I=1,NOBC
          write(33,35) TS_TIME(N),(TEMPOBC_M(I,K,N),K=1,KBm)
        enddo
      ENDDO
      CLOSE(33)

      OPEN(33,file='SALT_OBC_ajdusted.dat')
      DO N=1,NTMAX
        DO I=1,NOBC
          write(33,35) TS_TIME(N),(SALTOBC_M(I,K,N),K=1,KBm)
        ENDDO
      ENDDO
      CLOSE(33)
35    FORMAT(50F10.4)        

c-----------------------------------------------------------------------
c  Assign to corresponding open boundary variables, and then write into a netcdf file
c  Set global attributes string of the NetCDF
c-----------------------------------------------------------------------
1     format(I2.2,a1,I2.2,2x,I2.2,a1,i2.2,a1,I4)
      CALL DATE_AND_TIME(BIG_BEN(1),BIG_BEN(2),BIG_BEN(3),DATE_TIME)
      WRITE(CURRENT_TIME,1) DATE_TIME(5),':',DATE_TIME(6),
     &  DATE_TIME(2),'/',DATE_TIME(3),'/',DATE_TIME(1)
      globalstr(1)=trim(OCEAN_MODEL)//
     &  ' TIME SERIES ELEVATION FORCING FILE'
      globalstr(2)=trim(OFS)//' lateral open boundary netCDF file'
      globalstr(3)='Water level OBC from '//trim(DBASE_WL)//
     &  ' data source'
      globalstr(4)='T and S OBC from '//trim(DBASE_TS)//
     &  ' data source'
      IF(trim(DBASE_TS).ne.'OBS') 
     &  globalstr(5)=trim(DBASE_TS)//' data file: '//trim(FILE_TS)
      if(IGRD.EQ.0) then
        globalstr(6)='On native '//trim(DBASE_TS)//
     &               ' grid, No spatial interpolation'
      elseif(IGRD.EQ.1) then
        globalstr(6)='On '//trim(OCEAN_MODEL)//
     &               ' grid, using remesh spatial interpolation'
      elseif(IGRD.EQ.2) then
        globalstr(6)='On '//trim(OCEAN_MODEL)//
     &               ' grid, using bicubic spatial interpolation'
      elseif(IGRD.EQ.3) then
        globalstr(6)='On '//trim(OCEAN_MODEL)//
     &               ' grid, using bilinear spatial interpolation'
      elseif(IGRD.EQ.4) then
        globalstr(6)='On '//trim(OCEAN_MODEL)//
     &      ' grid, using nature neighbors spatial interpolation'
      endif
      globalstr(7)='GRID file: '//trim(GRIDFILE)
      globalstr(8)='Created at time '//trim(CURRENT_TIME)
      globalstr(9)='Created by Aijun Zhang, OD/CO-OPS/NOS/NOAA'
       
c-----------------------------------------------------------------------
c   Reading tide constituents at nodes along open boundary and 
c   Conduct tidal prediction
c-----------------------------------------------------------------------    
      WRITE(*,*) 'min & max WLOBC=',minval(WLOBC),maxval(WLOBC)
      WRITE(*,*) 'min & max T=',minval(tempOBC_M),maxval(tempOBC_M)
      WRITE(*,*) 'min & max S=',minval(saltOBC_M),maxval(saltOBC_M)
      WRITE(*,*) 'min & max U=',minval(ueOBC_M),maxval(ueOBC_M)
      WRITE(*,*) 'min & max V=',minval(veOBC_M),maxval(veOBC_M)
      WRITE(*,*) 'min & max UA=',minval(ubarOBC),maxval(ubarOBC)
      WRITE(*,*) 'min & max VA=',minval(vbarOBC),maxval(vbarOBC)
      WRITE(ICORMS,*) 'min & max WLOBC=',minval(WLOBC),maxval(WLOBC)
      WRITE(ICORMS,*) 'min & max T=',minval(tempOBC_M),
     &  maxval(tempOBC_M)
      WRITE(ICORMS,*) 'min & max S=',minval(saltOBC_M),
     &  maxval(saltOBC_M)
      WRITE(ICORMS,*) 'min & max U=',minval(ueOBC_M),maxval(ueOBC_M)
      WRITE(ICORMS,*) 'min & max V=',minval(veOBC_M),maxval(veOBC_M)
      WRITE(ICORMS,*) 'min & max UA=',minval(ubarOBC),maxval(ubarOBC)
      WRITE(ICORMS,*) 'min & max VA=',minval(vbarOBC),maxval(vbarOBC)
      
      write(*,*) 'Write out netcdf'
      IF(trim(OCEAN_MODEL).EQ.'FVCOM') THEN
        NREC=NINT((jdaye-jdays)*24/DELT)+1
        IF(ALLOCATED(TIME_M)) DEALLOCATE(TIME_M)
        IF(ALLOCATED(Times)) DEALLOCATE(Times)
        IF(ALLOCATED(Itime)) DEALLOCATE(Itime)
        IF(ALLOCATED(Itime2)) DEALLOCATE(Itime2)
        allocate(TIME_M(NREC))
        ALLOCATE(Times(NREC))
        ALLOCATE(Itime(NREC))
        ALLOCATE(Itime2(NREC))

        DO N=1,NREC
          TIME_M(N)=(jdays-jbase_date)+(N-1)*DELT/24.0
          jday=jdays+(N-1)*DELT/24.0
          call CALENDARDATE(jday,yearb,monthb,dayb,hourb,
     &         IYR,IMM,IDD,IHH,IMN,ISEC)
          WRITE(BUFFER,810) IYR,'-',IMM,'-',IDD,'T',IHH,':',IMN,
     &      ':00.000000'  
810       FORMAT(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A10)
          Times(N)=trim(adjustL(BUFFER))
          Itime(N)=INT(TIME_M(N)+0.001)
          Itime2(N)=INT((TIME_M(N)-Itime(N))*86400)*1000
!          WRITE(*,*) 'time=',N,Itime(N),Itime2(N),Times(N)
        ENDDO
 
        lat_eobc=1
        lon_eobc=1
        nv_obc=1
        wl_obc=1
        temp_obc=1
        salt_obc=1
        u_obc=-99999.9  ! variables turned off in netcdf writing
        v_obc=-99999.9
        ubar_obc=-99999.9
        vbar_obc=-99999.9
        partition_obc=-99999.9

        call nos_ofs_write_netCDF_obc_fvcom_gl(GRIDFILE,netcdf_file,
     &    ncidout,1,1,NOBC,NEOBC,KBm,KBm+1,base_date,
     &    Itime(1),Itime2(1),Times(1),hOBC,latOBC,lonOBC,lat_eobc,
     &    lon_eobc,nv_obc,siglay,siglev,IOBC,wl_obc,
     &    temp_obc,salt_obc,u_obc,v_obc,ubar_obc,vbar_obc,
     &    partition_obc,globalstr)

c imode=2
        DO N=1,NREC
          call nos_ofs_write_netCDF_obc_fvcom_gl(GRIDFILE,
     &      netcdf_file,ncidout,2,1,NOBC,NEOBC,KBm,KBm+1,base_date,
     &      Itime(N),Itime2(N),Times(N),hOBC,latOBC,lonOBC,lateOBC,
     &      loneOBC,nvOBC,siglay,siglev,IOBC,WLOBC(:,N),
     &      tempOBC_M(:,:,N),saltOBC_M(:,:,N),ueOBC_M(:,:,N),
     &      veOBC_M(:,:,N),ubarOBC(:,N),vbarOBC(:,N),partition,
     &      globalstr)
        ENDDO

c imode=3
        call nos_ofs_write_netCDF_obc_fvcom_gl(GRIDFILE,netcdf_file,
     &    ncidout,3,1,NOBC,NEOBC,KBm,KBm+1,base_date,
     &    Itime(1),Itime2(1),Times(1),hOBC,latOBC,lonOBC,lateOBC,
     &    loneOBC,nvOBC,siglay,siglev,IOBC,WLOBC(:,1),
     &    tempOBC_M(:,:,1),saltOBC_M(:,:,1),ueOBC_M(:,:,1),
     &    veOBC_M(:,:,1),ubarOBC(:,1),vbarOBC(:,1),partition,
     &    globalstr)

        CLOSE(67)
        OPEN(67,file='obc_fvcom.dat')
        WRITE(67,*) NREC,NOBC,NEOBC,KBm,KBm+1,base_date
        DO N=1,NREC
          WRITE(67,*) Times(N)
        ENDDO
        WRITE(67,*) WLOBC,tempOBC_M,saltOBC_M
        WRITE(67,*) ueOBC_M,veOBC_M,ubarOBC,vbarOBC
        CLOSE(67)

        write(*,*) 'OBC Forcing file is COMPLETED SUCCESSFULLY'
        WRITE(ICORMS,'(a)') 'WL OBC SOURCE IS ' //trim(DBASE_WL)
        WRITE(ICORMS,'(a)') 'T&S OBC SOURCE IS '//trim(DBASE_TS)
        WRITE(ICORMS,'(a)') 'END SECTION OF GENERATING OBC FILE' 
        CLOSE(ICORMS)
      ENDIF      
      STOP
      END
       
      SUBROUTINE READ_NETCDF(FIN,VNAME,ANAME,NDIMS,DIMS,TMP4D,
     &  ATT,MODE)
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
      INTEGER DIMS(4),MODE,dimids(5),COUNT(4)
      REAL TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) )
      LOGICAL FEXIST
      integer, allocatable :: ITMP4D(:,:,:,:)
      REAL*8, allocatable :: DTMP4D(:,:,:,:)

      IF(MODE.EQ.0) THEN
        DO I=1,4
          DIMS(I)=1
        ENDDO
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) trim(FIN)//' does not exist'
        ELSE  
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop  !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            write(*,*) 'Required variable ',trim(VNAME),
     &        ' is not found'
            stop
          ENDIF  

          STATUS=NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
          status=NF_INQ_VARDIMID(NCID,IDVAR,dimids)
          do i=1,NDIMS
            STATUS=NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
            write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
          enddo
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.1) THEN
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) trim(FIN)//' does not exist'
        ELSE  
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop  !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            write(*,*) 'Required variable ',trim(VNAME),
     &        ' is not found'
            stop
          ENDIF  

          STATUS=NF_INQ_VARNDIMS(NCID,IDVAR,ndims)
          status=NF_INQ_VARDIMID(NCID,IDVAR,dimids)
          do i=1,ndims
            STATUS=NF_INQ_DIMLEN(NCID,dimids(i),COUNT(i))
            IF(COUNT(i).NE.DIMS(I)) THEN
              WRITE(*,*) 'Dimension of array does not match' 
              write(*,*) TRIM(VNAME),' dim ',i,' = ',COUNT(i)
              write(*,*) 'DIMS(',I,')= ',DIMS(I),ndims
            ENDIF  
          enddo
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.2) THEN
        IF(ALLOCATED(itmp4d)) DEALLOCATE(itmp4d)
        allocate(ITMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) trim(FIN)//' does not exist'
        ELSE  
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop  !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          STATUS=NF_GET_VAR_INT(NCID,IDVAR,ITMP4D)
          DO I=1,DIMS(1)
            DO J=1,DIMS(2)
              DO K=1,DIMS(3)
                DO N=1,DIMS(4)
                  TMP4D(I,j,k,N)=ITMP4D(I,J,K,N)
                ENDDO
              ENDDO
            ENDDO
          ENDDO  
          IF(ALLOCATED(itmp4d)) DEALLOCATE(itmp4d)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.3) THEN
        allocate(ITMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) trim(FIN)//' does not exist'
        ELSE  
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop  !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          STATUS=NF_GET_VAR_TEXT(NCID,IDVAR,BUFFER)
          VNAME=TRIM(BUFFER)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.4) THEN
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) trim(FIN)//' does not exist'
        ELSE  
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop  !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,TRIM(ANAME),ATT)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.5) THEN
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) trim(FIN)//' does not exist'
        ELSE  
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop  !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,TRIM(ANAME),IATT)
          ATT=IATT
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.6) THEN
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) trim(FIN)//' does not exist'
        ELSE  
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop  !!'open netCDF file failed'
          ENDIF  
 
          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          STATUS=NF_GET_ATT_TEXT(NCID,IDVAR,TRIM(ANAME),BUFFER)
          ANAME=TRIM(BUFFER)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.7) THEN
        allocate(DTMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) trim(FIN)//' does not exist'
        ELSE
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop  !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            write(*,*) 'Required variable ',trim(VNAME),
     &        ' is not found'
            stop
          ENDIF  

          STATUS=NF_GET_VAR_DOUBLE(NCID,IDVAR,DTMP4D)
          DO I1=1,DIMS(1)
            DO I2=1,DIMS(2)
              DO I3=1,DIMS(3)
                DO I4=1,DIMS(4)
                  TMP4D(I1,I2,I3,I4)=DTMP4D(I1,I2,I3,I4)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
          IF(ALLOCATED(Dtmp4d)) DEALLOCATE(Dtmp4d)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ENDIF    

      RETURN
      END        

      SUBROUTINE CALENDARDATE(jday,yearb,monthb,dayb,hourb,
     &  iyr,imm,idd,ihh,imn,isec)      
      REAL*8 jday,yearb,monthb,dayb,hourb
      INTEGER iyr,imm,idd,ihh,imn,isec
      INTEGER DAYS_PER_MONTH(12)
      DATA (DAYS_PER_MONTH(i),I=1,12) /
     &     31,28,31,30,31,30,31,31,30,31,30,31/
       
      call GREGORIAN(jday,yearb,monthb,dayb,hourb)
      IYR=INT(yearb)
      IMM=int(monthb+0.001)
      IDD=INT(dayb+0.001)
      IHH=INT(hourb+0.001)
      IMN=INT((hourb-IHH)*60+0.1)
      ISEC=0
      IF(ISEC.EQ.60) THEN
        ISEC=0
        IMN=IMN+1
      ENDIF
      IF(IMN.EQ.60) THEN
        IMN=0
        IHH=IHH+1
      ENDIF
      IF(IHH.EQ.24) THEN
        IHH=0
        IDD=IDD+1
        IF(MOD(IYR,4).EQ.0) DAYS_PER_MONTH(2)=29   !! Leap Year
        IF(IDD.GT.DAYS_PER_MONTH(IMM)) THEN
          IDD=IDD-DAYS_PER_MONTH(IMM)
          IMM=IMM+1
          IF(IMM.GT.12) THEN
            IMM=IMM-12
            IYR=IYR+1
          ENDIF
        ENDIF
      ENDIF

      RETURN
      END
