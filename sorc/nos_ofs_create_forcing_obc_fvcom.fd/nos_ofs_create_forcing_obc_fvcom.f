C----------------------------------------------------------------------------------
C
C Program Name:  nos_ofs_create_forcing_obc_fvcom.f
C
C Directory:  /nos/save/wx21az/stratus/nosofs_shared.v1.0.0/sorc/nos_ofs_create_forcing_obc_fvcom.fd
C
C Purpose:    This Program is used to generated lateral open boundary condition files for FVCOM 
C             from either climatological dataset WOA05 or Navy Coastal Ocean Model (NCOM) products 
C             in CCS data tank, or ETSS nontidal water levels.
C             The data on NCOM grid is horizontally interpolated onto FVCOM open boundary grid points
C             using either remesh or nature neighbors (both bicubic and bilinear routine do not work),
C             and then vertically interpolated onto FVCOm vertical levels from NCOM 
C             z-coordinate vertical levels linearly.

C             Refer to HPC_COMF Technical Report for more detail information.               

C Current contact:   Aijun Zhang
C         Org:  NOS/CO-OPS/OD   Phone:  240-533-0591 
C                    aijun.zhang@Noaa.gov 
C Attributes:
C  Language:  Fortran
C  Computer:  DEW/MIST at NCEP  
C
C  Compile command:  gmake -f makefile
C
C Subprograms called: sal78.f nos_ofs_tideprediction.f nos_ofs_obc_write_netcdf_fvcom.f utility.f
C
C Input Data files:
C   "/dcom/us007003/20081120/wgrdbul/ncom_glb_reg1_2008112000.nc.gz"
C    
C Usage:   nos_ofs_create_forcing_obc_fvcom < Fortran_OBC.ctl > Fortran_OBC.log 
C
C
C Input Parameters:
C           OFS         : name of Operational Forecast System, e.g. CBOFS, TBOFS
C        Ocean_MODEL    : Name of numerical ocean model used in OFS, e.g. ROMS, FVCOM
C        DBASE_WWL      : data source for nontidal water level OBC
C        DBASE_TS       : data source for temperature and salinity OBC
C        TIME_START     : Start time 
C        FORHRS         : Length of forecast time in hours
C        IGRD           : indicator of horizontal interpolation method
C                        =1:  remesh using triangulation techniques
C 		         =2: bicubic routine from ROMS
C			 =3: bilinear routine from ROMS
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
      USE, INTRINSIC :: IEEE_ARITHMETIC
      PARAMETER (NMAX=9000)
      include 'netcdf.inc'
      CHARACTER*200 OFS,DBASE_TS,FILE_TS,TMPTXT,PREFIXNOS
      CHARACTER*200 FIN,FOUT,GRIDFILE,NETCDF_FILE,FIXOFS,ETSSFILE
      CHARACTER*200 BUFFER,CMD,VNAME,ANAME,OBC_CTL_FILE,HC_FILE,CTMP
      CHARACTER*200 BUFFER1,HC_FILE_OFS,FILE_NCOM(100),NCOMDIR
      CHARACTER*200 COMUSGS,COMPORTS
      CHARACTER*200 FILE_NCOM_TIDE(100),FNAME,FIN_TIDE
      CHARACTER*200 FDIR,STYPE,VGRIDFILE,NCOM_FILE,NOSBUFR,USGSBUFR
      CHARACTER*200 CORMSLOG,DBASE_TS_ORI,INIT_FILE_NOWCAST
      CHARACTER*200 START_TIME, END_TIME,BUFRFILE,OBC_CLIM_FILE
      CHARACTER*200 START_TIMEM1
      CHARACTER*200 OBC_FORCING_FILE_LAST,COMOUTroot
      CHARACTER*120 globalstr(9)
      CHARACTER*20 CURRENT_TIME,DBASE_WL
      CHARACTER*10 BIG_BEN(3),OCEAN_MODEL
      CHARACTER*8  CDATE
      CHARACTER*2  CHH
      CHARACTER*26,ALLOCATABLE :: TIMES(:)
      INTEGER, ALLOCATABLE :: ITIME(:)
      INTEGER, ALLOCATABLE :: ITIME2(:)
      REAL*8 JDAY_START,JBASE_DATE,JULIAN,YEARB,MONTHB,DAYB,HOURB
      REAL*8 JDAY,JDAYS,JDAYE,JDAY0,JS_ETSS,JE_ETSS
      REAL MINLON,MINLAT,MAXLAT,MAXLON,MISSVALUE
      LOGICAL FEXIST,USGS_L
      INTEGER DATE_TIME(8)
      INTEGER BASE_DATE(4)
      INTEGER DAYS_PER_MONTH(12)
      DATA (DAYS_PER_MONTH(i),I=1,12) /
     &  31,28,31,30,31,30,31,31,30,31,30,31/ 
      INTEGER GRBUNIT,NCID,NCIDOUT
      REAL LATSW,LONSW,LAD,LOV,DX_GRB
      INTEGER IPARENT(NMAX)
      INTEGER JPARENT(NMAX)

cc allocatable arrays for ROMS model
      INTEGER, ALLOCATABLE :: NV(:,:)  
      INTEGER, ALLOCATABLE :: NVOBC(:,:)  
      REAL, ALLOCATABLE :: LONM(:)
      REAL, ALLOCATABLE :: LATM(:)
      REAL, ALLOCATABLE :: LONC(:)
      REAL, ALLOCATABLE :: LATC(:)
      REAL, ALLOCATABLE :: SIGMA(:)
      REAL, ALLOCATABLE :: SIGLAY(:,:)
      REAL, ALLOCATABLE :: SIGLEV(:,:)
      REAL, ALLOCATABLE :: ZSIGMA(:),ZSIG_TMP(:)
      REAL, ALLOCATABLE :: MASKM(:,:)
      REAL, ALLOCATABLE :: HM(:)
      REAL, ALLOCATABLE :: UBAROBC(:,:)
      REAL, ALLOCATABLE :: VBAROBC(:,:)

cc temporary array for regrid
      REAL, ALLOCATABLE :: IOUT(:,:)
      REAL, ALLOCATABLE :: JOUT(:,:)
      REAL, ALLOCATABLE :: OUTM(:,:)
      REAL, ALLOCATABLE :: LON_IN(:,:)
      REAL, ALLOCATABLE :: LAT_IN(:,:)
      REAL, ALLOCATABLE :: TIME_M(:)     
      REAL, ALLOCATABLE :: TIMEOBC(:)
      REAL, ALLOCATABLE :: WLOBC(:,:)
      REAL, ALLOCATABLE :: TEMPOBC(:,:,:)
      REAL, ALLOCATABLE :: SALTOBC(:,:,:)
      REAL, ALLOCATABLE :: UOBC(:,:,:)
      REAL, ALLOCATABLE :: VOBC(:,:,:)
      REAL, ALLOCATABLE :: UEOBC(:,:,:)
      REAL, ALLOCATABLE :: VEOBC(:,:,:)
      REAL, ALLOCATABLE :: TEMPOBC_M(:,:,:)
      REAL, ALLOCATABLE :: SALTOBC_M(:,:,:)
      REAL, ALLOCATABLE :: UOBC_M(:,:,:)
      REAL, ALLOCATABLE :: VOBC_M(:,:,:)
      REAL, ALLOCATABLE :: UEOBC_M(:,:,:)
      REAL, ALLOCATABLE :: VEOBC_M(:,:,:)

cc allocatable arrays for variables in NCOM file
      REAL, ALLOCATABLE :: LON(:,:)
      REAL, ALLOCATABLE :: LAT(:,:)
      REAL, ALLOCATABLE :: ZETA_TIME(:)
      REAL, ALLOCATABLE :: TS_TIME(:)
      REAL, ALLOCATABLE :: DEPTH(:)
      
      REAL, ALLOCATABLE :: LONSUB(:,:)
      REAL, ALLOCATABLE :: LATSUB(:,:)
      REAL, ALLOCATABLE :: MASKSUB(:,:)
      REAL, ALLOCATABLE :: WL(:,:,:)
      REAL, ALLOCATABLE :: TEMP(:,:,:,:)
      REAL, ALLOCATABLE :: SALT(:,:,:,:)
      REAL, ALLOCATABLE :: U(:,:,:,:)
      REAL, ALLOCATABLE :: V(:,:,:,:)
      REAL, ALLOCATABLE :: TIDE_NCOM(:,:,:)
      REAL, ALLOCATABLE :: UBAR_RTOFS(:,:,:)
      REAL, ALLOCATABLE :: VBAR_RTOFS(:,:,:)

      REAL, ALLOCATABLE :: LON_ETSS(:,:)
      REAL, ALLOCATABLE :: LAT_ETSS(:,:)
      REAL, ALLOCATABLE :: WL_ETSS(:,:)

      REAL, ALLOCATABLE :: TIDE_AMP(:,:)
      REAL, ALLOCATABLE :: TIDE_EPOC(:,:)
      REAL, ALLOCATABLE :: TIDE_SPEED(:,:)
      CHARACTER,ALLOCATABLE :: STATIONID(:,:)
      CHARACTER,ALLOCATABLE :: CONSTITUENTS(:,:)
      REAL AMP(37),EPOC(37)

cc temporary arrays
      REAL, ALLOCATABLE :: TMP1D(:)
      REAL, ALLOCATABLE :: TMP2D(:,:)
      REAL, ALLOCATABLE :: TMP3D(:,:,:)
      REAL, ALLOCATABLE :: TMP4D(:,:,:,:)
      real, allocatable :: tmpp1(:),tmpp2(:),tmpp3(:),tmpp4(:)
      REAL*8, ALLOCATABLE :: DTMP4D(:,:,:,:)
      INTEGER, ALLOCATABLE :: ITMP4D(:,:,:,:)
      REAL, ALLOCATABLE :: ONED1(:)
      REAL, ALLOCATABLE :: ONED2(:)
      REAL, ALLOCATABLE :: ONED3(:)
      REAL, ALLOCATABLE :: ONED4(:)
      REAL*4, ALLOCATABLE :: XINP(:)
      REAL*4, ALLOCATABLE :: YINP(:)
      REAL*4, ALLOCATABLE :: ZINP(:)
      REAL*4, ALLOCATABLE :: XOUT(:)
      REAL*4, ALLOCATABLE :: YOUT(:)
      REAL*4, ALLOCATABLE :: ZOUT(:)
      INTEGER :: status      ! Return status
      INTEGER, ALLOCATABLE :: WEIGHTNODES(:,:)  
      INTEGER, ALLOCATABLE :: ITMP3D(:,:,:)
      REAL, ALLOCATABLE :: WEIGHTS(:,:)       
      INTEGER N3(3),ITMP1D(2000)
      REAL W3(3)
      INTEGER DIMIDS(5),COUNT(4),DIMS(4)

cc variables for real time observations
      CHARACTER*8  SUBSET,NAME                                  
      CHARACTER*20 STNBUFRID
      REAL*8 DATES(5),RTIM(6),RPID
      REAL*8 DATA1(2,500),DATA2(4,500),DATA3(5)
      REAL*8 XLOCAT(5)
      DATA BMISS /10E10/                                                
      equivalence(NAME,RPID)

cc variables for OBC_CTL_FILE     
      INTEGER, ALLOCATABLE :: SID(:)
      CHARACTER*20,ALLOCATABLE :: NOS_ID(:)
      CHARACTER*5,ALLOCATABLE :: NWS_ID(:)
      CHARACTER*4,ALLOCATABLE :: AGENCY_ID(:)
      CHARACTER*1,ALLOCATABLE :: OBC_ID(:)
      REAL, ALLOCATABLE :: DATUM(:)
      INTEGER, ALLOCATABLE :: WL_FLAG(:)
      INTEGER, ALLOCATABLE :: TS_FLAG(:)
      INTEGER, ALLOCATABLE ::BACKUP_SID(:)
      INTEGER, ALLOCATABLE :: GRIDID_STA(:)
      REAL, ALLOCATABLE :: AS(:)

      INTEGER, ALLOCATABLE :: GRIDID(:)
      INTEGER, ALLOCATABLE :: IOBC(:)
      INTEGER, ALLOCATABLE :: JOBC(:)
      INTEGER, ALLOCATABLE :: WL_STA(:)
      INTEGER, ALLOCATABLE :: TS_STA(:)
      INTEGER, ALLOCATABLE :: WL_SID_1(:)
      INTEGER, ALLOCATABLE :: WL_SID_2(:)
      INTEGER, ALLOCATABLE :: TS_SID_1(:)
      INTEGER, ALLOCATABLE :: TS_SID_2(:)
      REAL, ALLOCATABLE :: WL_S_1(:)
      REAL, ALLOCATABLE :: WL_S_2(:)
      REAL, ALLOCATABLE :: TS_S_1(:)
      REAL, ALLOCATABLE :: TS_S_2(:)
      INTEGER, ALLOCATABLE :: CU_STA(:)
      INTEGER, ALLOCATABLE :: CU_SID_1(:)
      INTEGER, ALLOCATABLE :: CU_SID_2(:)
      REAL, ALLOCATABLE :: CU_S_1(:)
      REAL, ALLOCATABLE :: CU_S_2(:)
      
      INTEGER, ALLOCATABLE :: NTR(:)
      INTEGER, ALLOCATABLE :: NTR_T(:)
      INTEGER, ALLOCATABLE :: NTR_S(:)
      REAL, ALLOCATABLE :: RTIME(:,:)
      REAL, ALLOCATABLE :: RTIME_T(:,:)
      REAL, ALLOCATABLE :: RTIME_S(:,:)
      REAL, ALLOCATABLE :: TIME_PRD(:)
      REAL, ALLOCATABLE :: WL_PRD(:,:)
      REAL, ALLOCATABLE :: WL_OBS(:,:)
      REAL, ALLOCATABLE :: SWL_OBS(:,:)
      REAL, ALLOCATABLE :: T_OBS(:,:) 
      REAL, ALLOCATABLE :: S_OBS(:,:) 
      REAL, ALLOCATABLE :: LONOBC(:) 
      REAL, ALLOCATABLE :: LATOBC(:) 
      REAL, ALLOCATABLE :: LONEOBC(:) 
      REAL, ALLOCATABLE :: LATEOBC(:) 
      REAL, ALLOCATABLE :: HOBC(:) 
      REAL, ALLOCATABLE :: AVGERR(:) 
      REAL, ALLOCATABLE :: AVGERR_T(:) 
      REAL, ALLOCATABLE :: AVGERR_S(:)

      REAL, ALLOCATABLE :: TIME_CLIM(:)
      REAL, ALLOCATABLE :: T_CLIM(:,:,:) 
      REAL, ALLOCATABLE :: S_CLIM(:,:,:) 
      INTEGER, ALLOCATABLE :: K_CLIM(:)
      REAL, ALLOCATABLE :: DEPTH_CLIM(:,:)

      REAL, ALLOCATABLE :: ZKU(:)
      REAL, ALLOCATABLE :: ZKL(:)

      INTEGER, ALLOCATABLE :: PARTITION(:)
      REAL, ALLOCATABLE :: HEOBC(:)
      REAL, ALLOCATABLE :: SIGLEV_ELE(:,:)
!  added by zheng on 04/04/2018
      REAL, ALLOCATABLE :: SIGLAY_ELE(:,:)
!  added by zheng on 04/04/2018

!-----------------------------------------------------------------------
!  read parameters from the Fortran control "Fortran_OBC.ctl"
!-----------------------------------------------------------------------      
      READ(5,'(A200)') OFS
      READ(5,'(A10)')  OCEAN_MODEL
      READ(5,'(A20)')  DBASE_WL
      READ(5,'(A200)') ETSSFILE
      READ(5,'(A200)') DBASE_TS
      DBASE_TS_ORI=DBASE_TS
      READ(5,'(A200)') NCOMDIR
      READ(5,'(A200)') COMPORTS
      READ(5,'(A200)') BUFFER
      NOSBUFR=TRIM(ADJUSTL(BUFFER))
      READ(5,'(A200)') BUFFER
      USGSBUFR=TRIM(ADJUSTL(BUFFER))

      READ(5,'(A200)') BUFFER
      START_TIME=TRIM(ADJUSTL(BUFFER))
      READ(START_TIME,'(I4,4I2)') IYRS,IMMS,IDDS,IHHS,IMNS

      READ(5,'(A200)') BUFFER
      END_TIME=TRIM(ADJUSTL(BUFFER))
      READ(END_TIME,'(I4,4I2)') IYRE,IMME,IDDE,IHHE,IMNE
      READ(5,*) IGRD
      IGRD_ORI=IGRD
      READ(5,'(A200)') BUFFER
      DO I=1,LEN_TRIM(BUFFER)
        IF(BUFFER(I:I).EQ."'".OR.BUFFER(I:I).EQ.'"') THEN
	  BUFFER(I:I)=' '
	ENDIF    
      ENDDO
      FIXOFS=TRIM(ADJUSTL(BUFFER))
      
      READ(5,'(A200)') BUFFER
      DO I=1,LEN_TRIM(BUFFER)
        IF(BUFFER(I:I).EQ."'".OR.BUFFER(I:I).EQ.'"') THEN
	  BUFFER(I:I)=' '
	ENDIF    
      ENDDO
      GRIDFILE=trim(adjustL(BUFFER))
      WRITE(*,*) 'GRIDFILE=',TRIM(GRIDFILE)

      READ(5,'(A200)') BUFFER
      DO I=1,LEN_TRIM(BUFFER)
        IF(BUFFER(I:I).EQ."'".OR.BUFFER(I:I).EQ.'"') THEN
	  BUFFER(I:I)=' '
	ENDIF    
      ENDDO
      HC_FILE_OFS=TRIM(ADJUSTL(BUFFER))
      WRITE(*,*) 'HC_FILE_OFS=',TRIM(HC_FILE_OFS)

      READ(5,'(A200)') BUFFER
      DO I=1,LEN_TRIM(BUFFER)
        IF(BUFFER(I:I).EQ."'".OR.BUFFER(I:I).EQ.'"') THEN
	  BUFFER(I:I)=' '
	ENDIF    
      ENDDO
      OBC_CTL_FILE=TRIM(ADJUSTL(BUFFER))
      WRITE(*,*) 'OBC_CTL_FILE=',TRIM(OBC_CTL_FILE)

      READ(5,'(A200)') BUFFER
      DO I=1,LEN_TRIM(BUFFER)
        IF(BUFFER(I:I).EQ."'".OR.BUFFER(I:I).EQ.'"') THEN
	  BUFFER(I:I)=' '
	ENDIF    
      ENDDO
      OBC_CLIM_FILE=TRIM(ADJUSTL(BUFFER))
      WRITE(*,*) 'OBC_CLIM_FILE=',TRIM(OBC_CLIM_FILE)

      READ(5,'(A200)') BUFFER
      DO I=1,LEN_TRIM(BUFFER)
        IF(BUFFER(I:I).EQ."'".OR.BUFFER(I:I).EQ.'"') THEN
	  BUFFER(I:I)=' '
	ENDIF    
      ENDDO
      NETCDF_FILE=TRIM(ADJUSTL(BUFFER))

      READ(5,'(A200)') BUFFER
      DO I=1,LEN_TRIM(BUFFER)
        IF(BUFFER(I:I).EQ."'".OR.BUFFER(I:I).EQ.'"') THEN
	  BUFFER(I:I)=' '
	ENDIF    
      ENDDO
      CORMSLOG=TRIM(ADJUSTL(BUFFER))
      WRITE(*,*) 'CORMSLOG=',TRIM(CORMSLOG)

      READ(5,'(A200)') BUFFER
      DO I=1,LEN_TRIM(BUFFER)
        IF(BUFFER(I:I).EQ."'".OR.BUFFER(I:I).EQ.'"') THEN
          BUFFER(I:I)=' '
	ENDIF    
      ENDDO
      BUFFER=TRIM(ADJUSTL(BUFFER))

      READ(BUFFER,'(I4,3I2)') BASE_DATE
      READ(5,*) MINLON
      READ(5,*) MINLAT
      READ(5,*) MAXLON
      READ(5,*) MAXLAT
      READ(5,*) KBM
      IF(TRIM(OCEAN_MODEL).EQ."ROMS") THEN
        READ(5,*) THETA_S
        READ(5,*) THETA_B
        READ(5,*) TCLINE
      ELSE  
        READ(5,'(A200)') BUFFER
        DO I=1,LEN_TRIM(BUFFER)
          IF(BUFFER(I:I).EQ."'".OR.BUFFER(I:I).EQ.'"') THEN
	    BUFFER(I:I)=' '
	  ENDIF    
        ENDDO
        VGRIDFILE=TRIM(ADJUSTL(BUFFER))
      ENDIF  

      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif    
      enddo
      COMOUTroot=trim(adjustL(BUFFER))

      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif    
      enddo
      OBC_FORCING_FILE_LAST=trim(adjustL(BUFFER))
      read(5,'(a200)')PREFIXNOS
!-----------------------------------------------------------------------
!  calculate base time, start time, and end time in days
!-----------------------------------------------------------------------
      ICORMS=43
      CLOSE(ICORMS)
      OPEN(ICORMS,FILE=trim(CORMSLOG),STATUS='OLD',POSITION='APPEND')
      TMPTXT=' '
      TMPTXT='BEGIN SECTION OF GENERATING OBC'
      WRITE(ICORMS,'(a)') trim(TMPTXT) 
      YEARB=BASE_DATE(1)
      MONTHB=BASE_DATE(2)
      DAYB=BASE_DATE(3)
      HOURB=BASE_DATE(4)
      JBASE_DATE=JULIAN(YEARB,MONTHB,DAYB,HOURB)

      YEARB=IYRS
      MONTHB=IMMS
      DAYB=IDDS
      HOURB=IHHS   !! do not need minutes to construct ETSS file name
      JDAYS=JULIAN(YEARB,MONTHB,DAYB,HOURB)
      DAY_START=JDAYS-JBASE_DATE

      YEARB=IYRE
      MONTHB=IMME
      DAYB=IDDE
      HOURB=IHHE   !! do not need minutes to construct ETSS file name
      JDAYE=JULIAN(YEARB,MONTHB,DAYB,HOURB)
      DAY_END=JDAYE-JBASE_DATE
      WRITE(*,*) 'BASE_DATE= ',BASE_DATE
      WRITE(*,*) 'DOMIN=',MINLON,MINLAT,MAXLON,MAXLAT
      WRITE(*,*) 'MODEL START & END TIME= ',DAY_START,DAY_END

!-----------------------------------------------------------------------
!   reading in NOS tide constituents at all NWLON stations
!-----------------------------------------------------------------------
      HC_FILE='nosofs.HC_NWLON.nc'
      INQUIRE(FILE=trim(HC_FILE),EXIST=FEXIST)
      IF(.NOT.FEXIST) THEN
        TMPTXT=' '
        TMPTXT='Harmonic Constant NetCDF file is not found'
        WRITE(*,*) trim(TMPTXT)
        TMPTXT=' '
        TMPTXT='Please check nosofs.HC_NWLON.nc in FIX directory'
        WRITE(*,*) trim(TMPTXT)
        TMPTXT=' '
        TMPTXT='Provide correct Harmonic Constant File Name'
        WRITE(*,*) trim(TMPTXT)
        TMPTXT=' '
        TMPTXT=TRIM(OFS)//' stop here'
        WRITE(*,*) trim(TMPTXT) 
        TMPTXT=' ' 
        TMPTXT='CRITICAL FAILURE IN CREATING OBC'
        WRITE(ICORMS,'(a)') trim(TMPTXT) 
        STOP
      ELSE  
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))

        STATUS=NF_OPEN(HC_FILE,NF_NOWRITE,NCID)
        STATUS=NF_INQ(NCID,NDIMS,NVARS,NGATTS,UNLIMDIMID)
cc   NDIMS=number of dimension parameters
cc   NVARS=number of total variables in the netcdf file
cc   NGATTS= number of global attributes
cc   UNLIMDIMID= dimension ID which is unlimited.
cc      (WRITE(*,*) 'NDIMS=',NDIMS,NVARS,NGATTS,UNLIMDIMID
        DO I=1,NDIMS
          STATUS=NF_INQ_DIM(NCID,i,BUFFER,ILATID)  !! extract dimension name
          STATUS=NF_INQ_DIMLEN(NCID,i,ILATID)
          IF(trim(BUFFER).eq.'Station') then
            NWLON_STA=ILATID
          ELSEIF(trim(BUFFER).eq.'Constituents') then
            NCON=ILATID
          ELSEIF(trim(BUFFER).eq.'staID') then
            NCHAR=ILATID
          ENDIF
        ENDDO

        VNAME='stationID'
        STATUS=NF_INQ_VARID(NCID,'stationID',IDVAR)
        STATUS=NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        STATUS=NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        DO I=1,NDIMS
          STATUS=NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
C          WRITE(*,*) TRIM(VNAME),' DIM ',I,' = ',DIMS(I)
        ENDDO
        ALLOCATE(stationID(DIMS(1),DIMS(2)))
        STATUS=NF_GET_VAR_TEXT(NCID,IDVAR,stationID)

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        STATUS=NF_INQ_VARID(NCID,'constituentName',IDVAR)
        STATUS=NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        STATUS=NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        DO I=1,NDIMS
          STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
        ENDDO
        ALLOCATE(CONSTITUENTS(DIMS(1),DIMS(2)))
        STATUS=NF_GET_VAR_TEXT(NCID,IDVAR,constituents)

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        STATUS=NF_INQ_VARID(NCID,'amplitude',IDVAR)
        STATUS=NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        STATUS=NF_INQ_VARDIMID(NCID,IDVAR,DIMIDS)
        DO I=1,NDIMS
          STATUS=NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
          WRITE(*,*) TRIM(VNAME),' DIM ',I,' = ',DIMS(I)
        ENDDO
        ALLOCATE(tide_amp(DIMS(1),DIMS(2)))
        ALLOCATE(tide_epoc(DIMS(1),DIMS(2)))
        STATUS=NF_INQ_VARID(NCID,'amplitude',IDVAR)
        STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tide_amp)
        STATUS=NF_INQ_VARID(NCID,'phase',IDVAR)
        STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tide_epoc)
        STATUS=NF_CLOSE(NCID)
      ENDIF

C-----------------------------------------------------------------------
C    Reading in FVCOM or SELFE model grid information
C-----------------------------------------------------------------------
      IF(TRIM(OCEAN_MODEL).EQ.'FVCOM'.OR.
     &   TRIM(OCEAN_MODEL).EQ.'SELFE') THEN
        WRITE(*,*) 'Reading Unstructured grid file: ',trim(GRIDFILE)
        OPEN(40,FILE=TRIM(GRIDFILE),STATUS='OLD')
        READ(40,*)
        READ(40,*) NELEM,NODEM
        ALLOCATE(LONM(NODEM))
        ALLOCATE(LATM(NODEM))
        ALLOCATE(LONC(NELEM))
	ALLOCATE(LATC(NELEM))
        ALLOCATE(HM(NODEM))
        ALLOCATE(NV(NELEM,3))

        DO N=1,NODEM
          READ(40,*) IM,LONM(N),LATM(N),HM(N)
        ENDDO
        DO N=1,NELEM
          READ(40,*) IDUM,IDUM,(NV(N,I),I=1,3)
          LONC(N)=(LONM(NV(N,1))+LONM(NV(N,2))+LONM(NV(N,3)))/3.0
          LATC(N)=(LATM(NV(N,1))+LATM(NV(N,2))+LATM(NV(N,3)))/3.0
        ENDDO
        CLOSE(40)
      ENDIF
      HMIN=MINVAL(HM)
      HMAX=MAXVAL(HM)

C-------------------------------------------------------------------
C    Open and read OBC control file  
C-------------------------------------------------------------------
      WRITE(*,*) 'Reading OBC control file'      
      OPEN(20,FILE=TRIM(OBC_CTL_FILE),STATUS='OLD')
      READ(20,*) NSTA,NOBC,NEOBC,DELT 
      WRITE(*,*) 'NSTA= ',NSTA,'NOBC=',NOBC

cc arrays for real time observations
      ALLOCATE(SID(NSTA))
      ALLOCATE(NOS_ID(NSTA))
      ALLOCATE(NWS_ID(NSTA))
      ALLOCATE(AGENCY_ID(NSTA))
      ALLOCATE(DATUM(NSTA))
      ALLOCATE(WL_FLAG(NSTA))
      ALLOCATE(TS_FLAG(NSTA))
      ALLOCATE(BACKUP_SID(NSTA))
      ALLOCATE(GRIDID_STA(NSTA))
      ALLOCATE(AS(NSTA))

      ALLOCATE(GRIDID(NOBC))
      ALLOCATE(IOBC(NOBC))
      ALLOCATE(JOBC(NEOBC))
      ALLOCATE(WL_STA(NOBC))
      ALLOCATE(TS_STA(NOBC))
      ALLOCATE(WL_SID_1(NOBC))
      ALLOCATE(WL_SID_2(NOBC))
      ALLOCATE(TS_SID_1(NOBC))
      ALLOCATE(TS_SID_2(NOBC))
      ALLOCATE(WL_S_1(NOBC))
      ALLOCATE(WL_S_2(NOBC))
      ALLOCATE(TS_S_1(NOBC))
      ALLOCATE(TS_S_2(NOBC))
      ALLOCATE(OBC_ID(NOBC))
      ALLOCATE(LONOBC(NOBC))
      ALLOCATE(LATOBC(NOBC))
      ALLOCATE(LONEOBC(NEOBC))
      ALLOCATE(LATEOBC(NEOBC))
      ALLOCATE(HOBC(NOBC))
      ALLOCATE(NVOBC(NEOBC,3))
      ALLOCATE(PARTITION(NEOBC))
      ALLOCATE(SIGLAY(NOBC,KBM))
      ALLOCATE(SIGLEV(NOBC,KBM+1))
      ALLOCATE(SIGMA(KBM+1))
      ALLOCATE(ZSIGMA(KBM+1))
      ALLOCATE(HEOBC(NEOBC))
      ALLOCATE(SIGLEV_ELE(NEOBC,KBM+1))
!  added by zheng on 04/04/2018
      ALLOCATE(SIGLAY_ELE(NEOBC,KBM))
!  added by zheng on 04/04/2018

      ALLOCATE(NTR(NSTA))
      ALLOCATE(NTR_T(NSTA))
      ALLOCATE(NTR_S(NSTA))
      ALLOCATE(RTIME(NSTA,NMAX))
      ALLOCATE(RTIME_T(NSTA,NMAX))
      ALLOCATE(RTIME_S(NSTA,NMAX))
      ALLOCATE(TIME_PRD(NMAX))
      ALLOCATE(WL_PRD(NSTA,NMAX))
      ALLOCATE(WL_OBS(NSTA,NMAX))
      ALLOCATE(SWL_OBS(NSTA,NMAX))
      ALLOCATE(T_OBS(NSTA,NMAX))
      ALLOCATE(S_OBS(NSTA,NMAX))

      ALLOCATE(K_CLIM(NSTA))
      ALLOCATE(TIME_CLIM(NMAX))
      ALLOCATE(DEPTH_CLIM(NSTA,50)) 
      ALLOCATE(T_CLIM(NSTA,NMAX,50)) 
      ALLOCATE(S_CLIM(NSTA,NMAX,50)) 
      DO I=1,NEOBC
        PARTITION(I)=1
      ENDDO	

      IF(ALLOCATED(ONED1)) DEALLOCATE(ONED1)
      IF(ALLOCATED(ONED2)) DEALLOCATE(ONED2)
      IF(ALLOCATED(ONED3)) DEALLOCATE(ONED3)
      IF(ALLOCATED(ONED4)) DEALLOCATE(ONED4)
      ALLOCATE(ONED1(NMAX))
      ALLOCATE(ONED2(NMAX))
      ALLOCATE(ONED3(NMAX))
      ALLOCATE(ONED4(NMAX))
      DO
14      READ(20,*) BUFFER 
        BUFFER=TRIM(ADJUSTL(BUFFER))
        LL=LEN_TRIM(BUFFER)
        IF(LL.EQ.0) CYCLE
        EXIT
      ENDDO
      READ(20,*) BUFFER 
      DO N=1,NSTA
        READ(20,*) SID(N),NOS_ID(N),NWS_ID(N),AGENCY_ID(N),DATUM(N),
     &	  WL_FLAG(N),TS_FLAG(N),BACKUP_SID(N),GRIDID_STA(N),As(N)
      ENDDO
      DO
16      READ(20,*) BUFFER 
        BUFFER=TRIM(ADJUSTL(BUFFER))
        LL=LEN_TRIM(BUFFER)
        IF(LL.EQ.0) CYCLE
        EXIT
      ENDDO
      READ(20,*) BUFFER
      DO N=1,NOBC
        READ(20,*) GRIDID(N),IOBC(N),WL_STA(N),WL_SID_1(N),
     &	  WL_S_1(N),WL_SID_2(N),WL_S_2(N),TS_STA(N),TS_SID_1(N),
     &    TS_S_1(N),TS_SID_2(N),TS_S_2(N)
        LONOBC(N)=LONM(IOBC(N))
        LATOBC(N)=LATM(IOBC(N))
        HOBC(N)=HM(IOBC(N))
        IF(LONOBC(N).GT.180.0) LONOBC(N)=LONOBC(N)-360.0
      ENDDO
      DO
17      READ(20,*) BUFFER 
        BUFFER=TRIM(ADJUSTL(BUFFER))
        LL=LEN_TRIM(BUFFER)
        IF(LL.EQ.0) CYCLE 
        EXIT
      ENDDO
      READ(20,*) BUFFER 

      DO N=1,NEOBC
        READ(20,*) IDUMMY,JOBC(N)
        LONEOBC(N)=LONC(JOBC(N))
        LATEOBC(N)=LATC(JOBC(N))
	DO L=1,3
	  NVOBC(N,L)=NV(JOBC(N),L)
	ENDDO  
        IF(LONEOBC(N).GT.180.0) LONEOBC(N)=LONEOBC(N)-360.0
      ENDDO

      DO I=1,NEOBC
        I1=NV(JOBC(I),1)
        I2=NV(JOBC(I),2)
        I3=NV(JOBC(I),3)
        HEOBC(I)=(HM(I1)+HM(I2)+HM(I3))/3.0
      ENDDO

      USGS_L=.FALSE.
      DO N=1,NSTA
        IF(TRIM(AGENCY_ID(N)).EQ."USGS" ) USGS_L=.TRUE.
      ENDDO

      NREC=NINT((JDAYE-JDAYS)*24/DELT)+1
      IF(ALLOCATED(TIME_M)) DEALLOCATE(TIME_M)
      ALLOCATE(TIME_M(NREC))
      DO N=1,NREC
	TIME_M(N)=(JDAYS-JBASE_DATE)+(N-1)*DELT/24.0
      ENDDO

C-----------------------------------------------------------------------
C   Define S-Curves in domain [-1 < sc < 0] at vertical W- and RHO-points.
C-----------------------------------------------------------------------
      WRITE(*,*) 'VGRID=',TRIM(VGRIDFILE)
      OPEN(40,FILE=TRIM(VGRIDFILE),STATUS='OLD')
      DO
11      READ(40,'(A200)',END=12) BUFFER
C      WRITE(*,*) 'BUFFER=',TRIM(BUFFER)
        IF(BUFFER(1:1).EQ.'!') THEN
C	WRITE(*,*) 'READING A COMMENT LINE'
 	  CYCLE

        ELSE  
          DO I=1,LEN_TRIM(BUFFER)
            IF(BUFFER(I:I).EQ."'".OR.BUFFER(I:I).EQ.'"') THEN
	      BUFFER(I:I)=' '
	    ENDIF    
          ENDDO
        BUFFER=TRIM(ADJUSTL(BUFFER))
	LL=LEN_TRIM(BUFFER)
        IND=INDEX(BUFFER,'NUMBER OF SIGMA LEVELS')
	IF(IND.GT.0) THEN
          IND=INDEX(BUFFER,'=')
	  READ(BUFFER(IND+1:IND+4),*) KB
C	  WRITE(*,*) 'KB=',KB 
	  IF(KB.NE.KBm+1) THEN
	    WRITE(*,*) 'Parameters KBm and KB are not consistent.'
            WRITE(*,*) 'Check main ctl and vgrid files'
	    STOP
	  ENDIF    
	ENDIF

        IND=INDEX(BUFFER,'SIGMA COORDINATE TYPE')
	IF(IND.GT.0) THEN
          IND=INDEX(BUFFER,'=')
	  READ(BUFFER(IND+1:LL),*) STYPE
C	  WRITE(*,*) 'STYPE=',STYPE   
	ENDIF
        IND=INDEX(BUFFER,'DU ')
	IF(IND.GT.0) THEN
          IND=INDEX(BUFFER,'=')
	  READ(BUFFER(IND+1:LL),*) DU
C	  WRITE(*,*) 'DU=',DU   
	ENDIF
        IND=INDEX(BUFFER,'DL ')
	IF(IND.GT.0) THEN
          IND=INDEX(BUFFER,'=')
	  READ(BUFFER(IND+1:LL),*)DL
C	  WRITE(*,*) 'DL=',DL   
	ENDIF
        IND=INDEX(BUFFER,'MIN CONSTANT DEPTH')
	IF(IND.GT.0) THEN
          IND=INDEX(BUFFER,'=')
	  READ(BUFFER(IND+1:LL),*) HMIN1
C	  WRITE(*,*) 'HMIN=',HMIN1   
	ENDIF
        IND=INDEX(BUFFER,'KUU ')
	IF(IND.GT.0) THEN
          IND=INDEX(BUFFER,'=')
	  READ(BUFFER(IND+1:LL),*) KU
C	  WRITE(*,*) 'KU=',KU
	ENDIF
        IND=INDEX(BUFFER,'KLL ')
	IF(IND.GT.0) THEN
          IND=INDEX(BUFFER,'=')
          READ(BUFFER(IND+1:LL),*) KL
C          WRITE(*,*) 'KL=',KL
        ENDIF
        IND=INDEX(BUFFER,'ZKU ')
	IF(IND.GT.0) THEN
   	  ALLOCATE(ZKU(KU))   
          IND=INDEX(BUFFER,'=')
          READ(BUFFER(IND+1:LL),*) (ZKU(K),K=1,KU)
C          WRITE(*,*) 'ZKU=',ZKU
	ENDIF
        IND=INDEX(BUFFER,'ZKL ')
	IF(IND.GT.0) THEN
	  ALLOCATE(ZKL(KL)) 
          IND=INDEX(BUFFER,'=')
	  READ(BUFFER(IND+1:LL),*) (ZKL(K),K=1,KL)
C	  WRITE(*,*) 'ZKL=',ZKL
	ENDIF

cccccccccccccc   machuan

        IND=INDEX(BUFFER,'SIGMA POWER ')
        IF(IND.GT.0) THEN
          IND=INDEX(BUFFER,'=')
          READ(BUFFER(IND+1:LL),*) P_SIGMA
         WRITE(*,*) '==== mmgp == p_sigma  ====',P_SIGMA
        ENDIF


        IND=INDEX(BUFFER,'SIGMA_POWER_DISTRIBUTION')
        IF(IND.GT.0) THEN
          IND=INDEX(BUFFER,'=')
          READ(BUFFER(IND+1:LL),*) P_SIGMA_DISTRIBUTION
         WRITE(*,*) '==== MMGP distribution==',P_SIGMA_DISTRIBUTION
        ENDIF 


cccccccccccccccccccccccccccccccccc  machuan        
       ENDIF
      ENDDO
12    CONTINUE
 

C-----------------------------------------------------------------------
C   Define sigma coordinates in domain [-1 < sc < 0] at vertical points.
C-----------------------------------------------------------------------
      IF(TRIM(STYPE).EQ.'UNIFORM') THEN
        DO I=1,NOBC
          DO K=1,KB
            SIGLEV(I,K)=-((K-1)/FLOAT(KBM))
          END DO
        END DO
cc  added by zheng on 04/04/2018
        DO I=1,NOBC
          DO K=1,KB-1
            SIGLAY(I,K)=0.5*(SIGLEV(I,K)+SIGLEV(I,K+1))
          END DO
        END DO
cc  added by zheng on 04/04/2018


cccccccccccc   machuan 

      ELSEIF(TRIM(STYPE).EQ.'GEOMETRIC') THEN

                write(*,*) "=== mmgp inside =="
        DO I=1,NOBC      
        IF(abs(P_SIGMA -1.0) < 1.0E-6)THEN
           DO K=1,KB
            SIGLEV(I,K) = -((K-1)/FLOAT(KB-1))**P_SIGMA_DISTRIBUTION
          END DO
        ELSE

           DO K=1,(KB+1)/2
            SIGLEV(I,K) = -((K-1)/FLOAT((KB+1)/2-1))**P_SIGMA/2
           END DO
           DO K=(KB+1)/2+1,KB
            SIGLEV(I,K) = ((KB-K)/FLOAT((KB+1)/2-1))**P_SIGMA/2-1.0
           END DO
        END IF   
        end do

        DO I=1,NOBC
          DO K=1,KB-1
            SIGLAY(I,K)=0.5*(SIGLEV(I,K)+SIGLEV(I,K+1))
          END DO
        END DO

cccccccccccccc   machuan



      ELSEIF(TRIM(STYPE).EQ.'GENERALIZED') THEN
        DO I=1,NOBC
          IF(hOBC(I)<HMIN1) THEN
            SIGLEV(I,1)=0.0
            DL2=0.001
            DU2=0.001
            DO K=1,KBM
              X1=DL2+DU2
              X1=X1*(KBM-K)/KBM
              X1=X1-DL2
              X1=TANH(X1)
              X2=TANH(DL2)
              X3=X2+TANH(DU2)
              SIGLEV(I,K+1)=(X1+X2)/X3-1.0
            END DO
          ELSE
            SIGLEV(I,1)=0.0
            DR=(HOBC(I)-DU-DL)/HOBC(I)/(KB-KU-KL-1)
            DO K=2,KU+1
              SIGLEV(I,K)=SIGLEV(I,K-1)-ZKU(K-1)/HOBC(I)
            END DO

            DO K=KU+2,KB-KL
              SIGLEV(I,K)=SIGLEV(I,K-1)-DR
            END DO

            KK=0
            DO K=KB-KL+1,KB
              KK=KK+1
              SIGLEV(I,K)=SIGLEV(I,K-1)-ZKL(KK)/HOBC(I)
            END DO
          END IF

cc  added by zheng on 04/04/2018
          DO K=1,KB-1
            SIGLAY(I,K)=0.5*(SIGLEV(I,K)+SIGLEV(I,K+1))
          END DO
cc  added by zheng on 04/04/2018
        END DO

      ELSEIF(TRIM(STYPE).EQ.'TANH') THEN
        DO I=1,NOBC
          SIGLEV(I,1)=0.0
        END DO
        DO K=1,KBM
          X1=DL+DU
          X1=X1*(KBM-K)/KBM
          X1=X1-DL
          X1=TANH(X1)
          X2=TANH(DL)
          X3=X2+TANH(DU)
          DO I=1,NOBC
            SIGLEV(I,K+1)=(X1+X2)/X3-1.0
          END DO
        END DO

cc  added by zheng on 04/04/2018
        DO I=1,NOBC
          DO K=1,KB-1
            SIGLAY(I,K)=0.5*(SIGLEV(I,K)+SIGLEV(I,K+1))
          END DO
        END DO
cc  added by zheng on 04/04/2018
      ENDIF

      DO K=1,KB
        WRITE(*,'(a3,i2,3f9.4)') 'K= ',K,(SIGLEV(I,K),I=1,3)      
      ENDDO

      IF(TRIM(STYPE).EQ.'UNIFORM') THEN
        DO I=1,NEOBC
          DO K=1,KB
            SIGLEV_ELE(I,K)=-((K-1)/FLOAT(KBM))
          END DO
        END DO
cc  added by zheng on 04/04/2018
        DO I=1,NEOBC
          DO K=1,KB-1
            SIGLAY_ELE(I,K)=0.5*(SIGLEV_ELE(I,K)+SIGLEV_ELE(I,K+1))
          END DO
        END DO
cc  added by zheng on 04/04/2018

ccccccccccccccccccccc machuan 

      ELSEIF(TRIM(STYPE).EQ.'GEOMETRIC') THEN
        DO I=1,NOBC
        IF(abs(P_SIGMA -1.0) < 1.0E-6)THEN
         DO K=1,KB
          SIGLEV_ELE(I,K) = -((K-1)/FLOAT(KB-1))**P_SIGMA_DISTRIBUTION
         END DO
        ELSE

           DO K=1,(KB+1)/2
            SIGLEV_ELE(I,K) = -((K-1)/FLOAT((KB+1)/2-1))**P_SIGMA/2
           END DO
           DO K=(KB+1)/2+1,KB
            SIGLEV_ELE(I,K) = ((KB-K)/FLOAT((KB+1)/2-1))**P_SIGMA/2-1.0
           END DO
        END IF
        end do


        DO I=1,NOBC
          DO K=1,KB-1
            SIGLAY_ELE(I,K)=0.5*(SIGLEV_ELE(I,K)+SIGLEV_ELE(I,K+1))
          END DO
        END DO

cccccccccccccccc  machuan


      ELSEIF(TRIM(STYPE).EQ.'GENERALIZED') THEN
        DO I=1,NEOBC
          IF(HEOBC(I)<HMIN1)THEN
            SIGLEV_ELE(I,1)=0.0
            DL2=0.001
            DU2=0.001
            DO K=1,KBm
              X1=DL2+DU2
              X1=X1*(KBm-K)/KBm
              X1=X1-DL2
              X1=TANH(X1)
              X2=TANH(DL2)
              X3=X2+TANH(DU2)
              SIGLEV_ELE(I,K+1)=(X1+X2)/X3-1.0
            END DO
          ELSE
            SIGLEV_ELE(I,1)=0.0
            DR=(HEOBC(I)-DU-DL)/HEOBC(I)/(KB-KU-KL-1)
            DO K=2,KU+1
              SIGLEV_ELE(I,K)=SIGLEV_ELE(I,K-1)-ZKU(K-1)/HEOBC(I)
            END DO

            DO K=KU+2,KB-KL
              SIGLEV_ELE(I,K)=SIGLEV_ELE(I,K-1)-DR
            END DO

            KK=0
            DO K=KB-KL+1,KB
              KK=KK+1
              SIGLEV_ELE(I,K)=SIGLEV_ELE(I,K-1)-ZKL(KK)/HEOBC(I)
            END DO
          END IF

cc  added by zheng on 04/04/2018
          DO K=1,KB-1
            SIGLAY_ELE(I,K)=0.5*(SIGLEV_ELE(I,K)+SIGLEV_ELE(I,K+1))
          END DO
cc  added by zheng on 04/04/2018
        END DO

      ELSEIF(TRIM(STYPE).EQ.'TANH') THEN
        DO I=1,NEOBC
          SIGLEV_ELE(I,1)=0.0
        END DO
        DO K=1,KBM
          X1=DL+DU
          X1=X1*(KBM-K)/KBM
          X1=X1-DL
          X1=TANH(X1)
          X2=TANH(DL)
          X3=X2+TANH(DU)
          DO I=1,NOBC
            SIGLEV_ELE(I,K+1)=(X1+X2)/X3-1.0
          END DO
        END DO

cc  added by zheng on 04/04/2018
        DO I=1,NEOBC
          DO K=1,KB-1
            SIGLAY_ELE(I,K)=0.5*(SIGLEV_ELE(I,K)+SIGLEV_ELE(I,K+1))
          END DO
        END DO
cc  added by zheng on 04/04/2018
      ENDIF

      IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
      IF(ALLOCATED(ITMP4D)) DEALLOCATE(ITMP4D)

C------------------------------------------------------------------
C   Begin to read in climatological temperature and salinity 
C   and temporally interpolated into equally interval of DELT 
C------------------------------------------------------------------
      NREC=NINT((JDAYE-JDAYS)*24/DELT)+1
      DO N=1,NREC
	TIME_CLIM(N)=((JDAYS-JBASE_DATE)*24.0+(N-1)*DELT)/24.0
      ENDDO

      NT_CLIM=NREC
      DO N=1,NSTA
        IYR=IYRS
        YEARB=IYRS
        IF(TS_FLAG(N).GT.0) THEN
          BUFFER1=trim(PREFIXNOS)//'.obc.clim.ts.'
          BUFFER=TRIM(BUFFER1)//TRIM(NOS_ID(N))//'.dat'
          FIN=TRIM(BUFFER)
          WRITE(*,*) 'FIN= ',TRIM(FIN)
          INQUIRE(FILE=TRIM(FIN),EXIST=FEXIST)
          IF(.NOT.FEXIST) THEN
            WRITE(*,*) 'Climatologic T&S file is not found'
            WRITE(*,*) 'Please provide climatologic T&S file at '
            WRITE(*,*) 'Station ', TRIM(NOS_ID(N))
            WRITE(ICORMS,'(a)') 'CRITICAL FAILURE IN CREATING OBC'
            STOP
          ELSE  
            YEARB=IYRS
            MONTHB=1.0
            DAYB=1.0
            HOURB=0
            JDAY0=JULIAN(YEARB,MONTHB,DAYB,HOURB)

            CLOSE(10)
            OPEN(10,FILE=TRIM(FIN),STATUS='OLD')
            READ(10,*) K_CLIM(N)
            READ(10,*) (DEPTH_CLIM(N,K),K=1,K_CLIM(N))
            READ(10,*)
            ICOUNT=0
            DO
18          READ(10,*,END=20) DUMMY,MON,IDD,
     &        (ONED1(K),ONED2(K),K=1,K_CLIM(N))
            ICOUNT=ICOUNT+1
            DAY0=JDAY0+DUMMY-JBASE_DATE
	    IF((ICOUNT.EQ.1).AND.(DAY0.GT.DAY_START)) THEN
              ONED3(ICOUNT)=DAY_START
              DO K=1,K_CLIM(N)
                T_CLIM(N,ICOUNT,K)=ONED1(K)
                S_CLIM(N,ICOUNT,K)=ONED2(K)
                IF(S_CLIM(N,ICOUNT,K).LE.0.05) S_CLIM(N,ICOUNT,K)=0.05
              ENDDO
              ICOUNT=ICOUNT+1
            ENDIF
            ONED3(ICOUNT)=DAY0
            DO K=1,K_clim(N)
              T_clim(N,ICOUNT,K)=ONED1(K)
              S_clim(N,ICOUNT,K)=ONED2(K)
              IF(S_CLIM(N,ICOUNT,K).LE.0.05) S_CLIM(N,ICOUNT,K)=0.05
            ENDDO
            ENDDO
20          CONTINUE
            CLOSE(10)

            IF(ONED3(ICOUNT).LT.DAY_END) THEN
              ICOUNT=ICOUNT+1
              ONED3(ICOUNT)=DAY_END
              DO K=1,K_CLIM(N)
                T_CLIM(N,ICOUNT,K)=ONED1(K)
                S_CLIM(N,ICOUNT,K)=ONED2(K)
                IF(S_CLIM(N,ICOUNT,K).LE.0.05) S_CLIM(N,ICOUNT,K)=0.05
              ENDDO
            ENDIF
            WRITE(*,*) 'TIME RANGE= ',DAY_START,DAY_END,
     &	      ONED3(1),ONED3(ICOUNT),ICOUNT

            DO K=1,K_clim(N)
              DO N1=1,ICOUNT
                ONED1(N1)=T_CLIM(N,N1,K)
                ONED2(N1)=S_CLIM(N,N1,K)
              ENDDO  

              if(allocated(tmpp1)) deallocate(tmpp1)
              if(allocated(tmpp2)) deallocate(tmpp2)
              allocate(tmpp1(1:NREC)); tmpp1=0.0
              allocate(tmpp2(1:NREC)); tmpp2=0.0
              if(allocated(tmpp3)) deallocate(tmpp3)
              if(allocated(tmpp4)) deallocate(tmpp4)
              allocate(tmpp3(1:ICOUNT)); tmpp3=0.0
              allocate(tmpp4(1:ICOUNT)); tmpp4=0.0
              DO N1=1,NREC
                tmpp1(N1)=TIME_clim(N1)
              End do
              DO N1=1,ICOUNT
                tmpp3(N1)=oned3(N1)
                tmpp4(N1)=oned1(N1)
              End do
              CALL LINEARARRAY(NREC,tmpp1,tmpp2,ICOUNT,tmpp3,tmpp4)
              DO N1=1,NREC
                T_CLIM(N,N1,K)=tmpp2(N1)
              ENDDO

              DO N1=1,ICOUNT
                tmpp4(N1)=oned2(N1)
              End do
              CALL LINEARARRAY(NREC,tmpp1,tmpp2,ICOUNT,tmpp3,tmpp4)
              DO N1=1,NREC
                S_CLIM(N,N1,K) =tmpp2(N1)
              ENDDO      
              deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
            ENDDO
          ENDIF
        ENDIF
      ENDDO

C------------------------------------------------------------------
C-------- BEGIN TO READ IN WOA05 NETCDF FILE ----------------------
C------------------------------------------------------------------
30    CONTINUE
      IF(TRIM(DBASE_TS).EQ.'WOA05') THEN
        FIN=TRIM(OBC_CLIM_FILE)
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          WRITE(*,*) 'WARNING:'
          WRITE(*,*) 'Climatologic T&S file (WOA) is not found'
          WRITE(*,*) 'Please provide climatologic T&S file of:'
          WRITE(*,*) TRIM(OBC_CLIM_FILE)//' in '//TRIM(FIXOFS)
          TMPTXT=' '
          TMPTXT='Climatologic T&S file (WOA) is not found'
          WRITE(ICORMS,'(a)') trim(TMPTXT)
          TMPTXT=' '
          TMPTXT=TRIM(OBC_CLIM_FILE)//' in '//TRIM(FIXOFS)
          WRITE(ICORMS,'(a)') TRIM(TMPTXT)
          WRITE(ICORMS,'(a)') 'CRITICAL FAILURE IN CREATING OBC' 
          STOP
        ENDIF  

        FILE_TS=TRIM(FIN)
        DO I=1,4
          DIMS(I)=1
        ENDDO
        VNAME='t0112an1'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IM=DIMS(1)
        JM=DIMS(2)
        KB=DIMS(3)
        NT=DIMS(4)
        IF(ALLOCATED(LON)) DEALLOCATE(LON)
        IF(ALLOCATED(LAT)) DEALLOCATE(LAT)
        IF(ALLOCATED(DEPTH)) DEALLOCATE(DEPTH)
        IF(ALLOCATED(TS_TIME)) DEALLOCATE(TS_TIME)
        ALLOCATE(LON(IM,JM))
        ALLOCATE(LAT(IM,JM))
        ALLOCATE(DEPTH(KB))
        ALLOCATE(TS_TIME(NT+2))

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='depth'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO K=1,KB
          DEPTH(K)=TMP4D(K,1,1,1)
        ENDDO
        IF(DEPTH(KB).LT.HMAX) DEPTH(KB)=HMAX
        DO K=1,KB
          WRITE(*,*) 'WOA5 DEPTH=',K,DEPTH(K)
        ENDDO

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='lon'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        WRITE(*,*) 'DIMS=',DIMS,NDIM
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO I=1,IM
          DO J=1,JM
            LON(I,J)=TMP4D(I,1,1,1)
 	    IF(LON(I,J).GT.180.0) LON(I,J)=LON(I,J)-360.0
          ENDDO
        ENDDO

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='lat'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO I=1,IM
          DO J=1,JM
            LAT(I,J)=TMP4D(J,1,1,1)
          ENDDO
        ENDDO
	 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc select subdomain I and J index        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IMIN=99999
        IMAX=-9999
        JMIN=99999
        JMAX=-9999
        DO I=1,IM
          DO J=1,JM
            IF(LON(I,J).GE.MINLON.AND.LON(I,J).LE.MAXLON.AND.
     &         LAT(I,J).GE.MINLAT.AND.LAT(I,J).LE.MAXLAT) THEN
              IF(I.LT.IMIN) IMIN=I 	  
              IF(I.GT.IMAX) IMAX=I 	  
              IF(J.LT.JMIN) JMIN=J 	  
              IF(J.GT.JMAX) JMAX=J
            ENDIF
          ENDDO
        ENDDO
        IMIN=IMIN-1
        IMAX=IMAX+1	  	  
        JMIN=JMIN-1
        JMAX=JMAX+1	  	  
        ISUB=IMAX-IMIN+1	 
        JSUB=JMAX-JMIN+1	 
        WRITE(*,*) 'SUBDOMAIN I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX	        

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc   Allocate sizes of arrays 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        NNSUB=ISUB*JSUB
        IF(ALLOCATED(LONSUB)) DEALLOCATE(LONSUB)
        IF(ALLOCATED(LATSUB)) DEALLOCATE(LATSUB)
        IF(ALLOCATED(MASKSUB)) DEALLOCATE(MASKSUB)
        IF(ALLOCATED(TEMP)) DEALLOCATE(TEMP)
        IF(ALLOCATED(SALT)) DEALLOCATE(SALT)
        IF(ALLOCATED(U)) DEALLOCATE(U)
        IF(ALLOCATED(V)) DEALLOCATE(V)
        ALLOCATE(LONSUB(ISUB,JSUB))
        ALLOCATE(LATSUB(ISUB,JSUB))
        ALLOCATE(MASKSUB(ISUB,JSUB))
        ALLOCATE(TEMP(ISUB,JSUB,KB,NT+2))
        ALLOCATE(SALT(ISUB,JSUB,KB,NT+2))
        ALLOCATE(U(ISUB,JSUB,KB,NT+2))
        ALLOCATE(V(ISUB,JSUB,KB,NT+2))

        DO J=1,JSUB
          DO I=1,ISUB
            LONSUB(I,J)=LON(IMIN+I-1,JMIN+J-1)
            LATSUB(I,J)=LAT(IMIN+I-1,JMIN+J-1)
	    WRITE(55,'(2F12.4,2I5)') LONSUB(I,J),LATSUB(I,J),I,J
          ENDDO
        ENDDO   
        CLOSE(55) 

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='time'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)

        YEARB=IYRS
        MONTHB=1
        DAYB=1
        HOURB=0
        JDAY=JULIAN(YEARB,MONTHB,DAYB,HOURB)
        TS_TIME(1)=JDAY-JBASE_DATE
        TS_TIME(NT+2)=370.0+JDAY-JBASE_DATE
        DO N=2,NT+1
          TS_TIME(N)=TMP4D(N-1,1,1,1)+JDAY-JBASE_DATE
        ENDDO

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='t0112an1'
        MISSVALUE=1.0E+20
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO I=1,ISUB
          DO J=1,JSUB
            DO K=1,KB
              DO N=2,NT+1
                TEMP(I,J,K,N)=TMP4D(IMIN+I-1,JMIN+J-1,K,N-1)
                IF(ABS(TEMP(I,J,K,N)-MISSVALUE).LE.0.001) 
     &            TEMP(I,J,K,N)=-99999.9
              ENDDO	
            ENDDO	
          ENDDO	
        ENDDO

        DO I=1,ISUB
          DO J=1,JSUB
            DO K=1,KB
              TEMP(I,J,K,1)=(TEMP(I,J,K,2)+TEMP(I,J,K,NT+1))/2.0
              TEMP(I,J,K,NT+2)=TEMP(I,J,K,1)
            ENDDO	
          ENDDO	
        ENDDO	

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='s0112an1'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO I=1,ISUB
          DO J=1,JSUB
            DO K=1,KB
              DO N=2,NT+1
                SALT(I,J,K,N)=TMP4D(IMIN+I-1,JMIN+J-1,K,N-1)
                IF(ABS(SALT(I,J,K,N)-MISSVALUE).LE.0.001)
     &            SALT(I,J,K,N)=-99999.9
              ENDDO	
            ENDDO	
          ENDDO	
        ENDDO

        DO I=1,ISUB
          DO J=1,JSUB
            DO K=1,KB
              SALT(I,J,K,1)=(SALT(I,J,K,2)+SALT(I,J,K,NT+1))/2.0
              SALT(I,J,K,NT+2)=SALT(I,J,K,1)
            ENDDO	
          ENDDO	
        ENDDO	

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='landsea'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO I=1,ISUB
          DO J=1,JSUB
            MASKSUB(I,J)=TMP4D(IMIN+I-1,JMIN+J-1,1,1)
          ENDDO	
        ENDDO	

        NT=NT+2
        DO I=1,ISUB
          DO J=1,JSUB
            DO K=1,KB
              DO N=1,NT
                U(I,J,K,N)=0.0
                V(I,J,K,N)=0.0
              ENDDO	
            ENDDO	
          ENDDO	
        ENDDO
        WRITE(*,*) 'WOA05 NT+2=',NT,'KB= ',KB 	
      ENDIF

C------------------------------------------------------------------
C     READ AND PROCESS AVAILABLE NCOM FORECAST FILES BY THE PROVIDED START TIME
C------------------------------------------------------------------
      IF(TRIM(DBASE_TS).EQ.'NCOM'.OR.
     &   TRIM(DBASE_TS).EQ.'NCOMeast'.OR.
     &   TRIM(DBASE_TS).EQ.'AMSEAS') THEN
        NCOM_FILE=TRIM(DBASE_TS)//"_FILE"
        IFILE_NCOM=0
        INQUIRE(FILE=TRIM(NCOM_FILE),EXIST=FEXIST)
        IF(FEXIST) THEN
          CLOSE(60)
          OPEN(60,FILE=TRIM(NCOM_FILE),STATUS='OLD')
          DO I=1,999
            READ(60,'(a200)',END=333) BUFFER
            FILE_NCOM(I)=TRIM(ADJUSTL(BUFFER))
            READ(60,'(A200)',END=333) BUFFER
            FILE_NCOM_TIDE(I)=TRIM(ADJUSTL(BUFFER))
          ENDDO
333       CLOSE(60)
          IFILE_NCOM=I-1
        ENDIF
        WRITE(*,*) 'Total number of NCOM FILES= ',IFILE_NCOM

        IF(IFILE_NCOM.EQ.0) THEN
          WRITE(*,*) 'No NCOM file is available in this period'
          WRITE(*,*) 'Use WOA05 climatological dataset to replace'
          WRITE(ICORMS,'(a)') 'WARNING: NCOM FILE IS NOT FOUND' 
          WRITE(ICORMS,'(a)') 'USE CLIMATOLOGIC BACKUP WOA05' 
          DBASE_TS='WOA05'
          IF(TRIM(DBASE_WL).EQ.'NCOM') DBASE_WL='ETSS'
          GOTO 30
        ENDIF  

        DO I=1,IFILE_NCOM
          WRITE(*,*) 'I=',I,TRIM(FILE_NCOM(I))
          WRITE(*,*) 'I=',I,TRIM(FILE_NCOM_TIDE(I))
        ENDDO

C-----------------------------------------------------------------------
C     Open and read all NCOM netCDF files available during start time to end time
C-----------------------------------------------------------------------
        FIN=TRIM(FILE_NCOM(1))
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='salinity'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IM=DIMS(1)
        JM=DIMS(2)
        KB=DIMS(3)
        NT=DIMS(4)
        WRITE(*,*) 'NCOM IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
        IF(ALLOCATED(LON)) DEALLOCATE(LON)
        IF(ALLOCATED(LAT)) DEALLOCATE(LAT)
        IF(ALLOCATED(DEPTH)) DEALLOCATE(DEPTH)
        IF(ALLOCATED(ZETA_TIME)) DEALLOCATE(ZETA_TIME)
        IF(ALLOCATED(TS_TIME)) DEALLOCATE(TS_TIME)
        ALLOCATE(LON(IM,JM))
        ALLOCATE(LAT(IM,JM))
        ALLOCATE(DEPTH(KB))
        ALLOCATE(ZETA_TIME(NMAX))
        ALLOCATE(TS_TIME(NMAX))

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='lon'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
        DO I=1,IM
          DO J=1,JM
            LON(I,J)=TMP4D(I,1,1,1)
            IF(LON(I,J).GT.180.0) LON(I,J)=LON(I,J)-360.0
          ENDDO
        ENDDO

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='lat'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
        DO I=1,IM
          DO J=1,JM
            LAT(I,J)=TMP4D(J,1,1,1)
          ENDDO
        ENDDO

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='depth'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
        DO K=1,KB
          DEPTH(K)=TMP4D(K,1,1,1)
        ENDDO

C-----------------------------------------------------------------------
C  select subdomain I and J index        
C-----------------------------------------------------------------------
        IMIN=99999
        IMAX=-9999
        JMIN=99999
        JMAX=-9999
        DO I=1,IM
          DO J=1,JM
            IF(LON(I,J).GE.MINLON.AND.LON(I,J).LE.MAXLON.AND.
     &         LAT(I,J).GE.MINLAT.AND.LAT(I,J).LE.MAXLAT) THEN
              IF(I.LT.IMIN) IMIN=I 	  
              IF(I.GT.IMAX) IMAX=I 	  
              IF(J.LT.JMIN) JMIN=J 	  
              IF(J.GT.JMAX) JMAX=J
            ENDIF
          ENDDO
        ENDDO	  	  
        ISUB=IMAX-IMIN+1	 
        JSUB=JMAX-JMIN+1	 
        WRITE(*,*) 'SUBDOMAIN I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX

C-----------------------------------------------------------------------
C   Allocate sizes of arrays for NCOM products 
C-----------------------------------------------------------------------
        NNSUB=ISUB*JSUB
        IF(ALLOCATED(LONSUB)) DEALLOCATE(LONSUB)
        IF(ALLOCATED(LATSUB)) DEALLOCATE(LATSUB)
        IF(ALLOCATED(TEMP)) DEALLOCATE(TEMP)
        IF(ALLOCATED(SALT)) DEALLOCATE(SALT)
        IF(ALLOCATED(U)) DEALLOCATE(U)
        IF(ALLOCATED(V)) DEALLOCATE(V)
        IF(ALLOCATED(WL)) DEALLOCATE(WL)
        IF(ALLOCATED(TIDE_NCOM)) DEALLOCATE(TIDE_NCOM)
        ALLOCATE(LONSUB(ISUB,JSUB))
        ALLOCATE(LATSUB(ISUB,JSUB))
        ALLOCATE(TEMP(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        ALLOCATE(SALT(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        ALLOCATE(U(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        ALLOCATE(V(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        ALLOCATE(WL(ISUB,JSUB,NT*IFILE_NCOM+10))
        ALLOCATE(TIDE_NCOM(ISUB,JSUB,NT*IFILE_NCOM+10))

        DO N=1,NT*IFILE_NCOM+10
          DO J=1,JSUB
            DO I=1,ISUB
              WL(I,J,N)=0.0
              TIDE_NCOM(I,J,N)=0.0
            ENDDO
          ENDDO
        ENDDO   

        CLOSE(77)
        OPEN(77,FILE='lonlat_NCOM.dat',STATUS='UNKNOWN')
        DO J=1,JSUB
          DO I=1,ISUB
            LONSUB(I,J)=LON(IMIN+I-1,JMIN+J-1)
            LATSUB(I,J)=LAT(IMIN+I-1,JMIN+J-1)
            WRITE(77,'(2F12.4,2I6)') LONSUB(I,J),LATSUB(I,J),I,J
          ENDDO
        ENDDO 
        CLOSE(77)  

        ICOUNT=1
        NREC=1
        TS_TIME(1)=-99999.9	 
        DO IFILE=1,IFILE_NCOM
          WRITE(*,*) 'Reading NCOM NetCDF file...',IFILE
          FIN=TRIM(FILE_NCOM(IFILE))
          VNAME='time'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
          IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
          ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          NT=DIMS(1)
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
          ANAME=TRIM(ADJUSTL(ANAME))
          LEN=LEN_TRIM(ANAME)
          LL=INDEX(ANAME,'minute')         
          IF(LL.GT.0) SCALE_TIME=1.0/1440.0
          LL=INDEX(ANAME,'hour')         
	  IF(LL.GT.0) SCALE_TIME=1.0/24.0

          ANAME='time_origin'
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
          WRITE(*,*) 'TIME_ORIGIN=',TRIM(ANAME)
          READ(ANAME,80) IYR,IMM,IDD,IHH
80        FORMAT(I4,1X,I2,1X,I2,1X,I2)
          YEARB=IYR
          MONTHB=IMM
          DAYB=IDD
          HOURB=IHH
          JDAY=JULIAN(YEARB,MONTHB,DAYB,HOURB)
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
          DUMMY=TMP4D(1,1,1,1)*SCALE_TIME+JDAY-JBASE_DATE
          N0=0
          DO N=1,NREC
            IF(TS_TIME(N).GE.DUMMY) THEN
              N0=N-1 
              EXIT
            ENDIF  
          ENDDO	
82        ICOUNT=N0

          DO N=1,NT
            TIME_NCOM=TMP4D(N,1,1,1)*SCALE_TIME+JDAY-JBASE_DATE
            ICOUNT=ICOUNT+1
            TS_TIME(ICOUNT)=TIME_NCOM
            WRITE(*,*) 'NCOM time=',ICOUNT,TS_TIME(ICOUNT)
          ENDDO   
          WRITE(*,*) 'FIRST TIME= ',DUMMY,TS_TIME(NREC),NREC,N0

          STATUS=NF_OPEN(TRIM(FIN),NF_NOWRITE, NCID)
          IF(STATUS.NE.NF_NOERR) THEN
	    WRITE(*,*) 'error message= ',status
	    STOP
          ENDIF  
          IF(ALLOCATED(TMP1D)) DEALLOCATE(TMP1D)
          ALLOCATE(TMP1D(IM))
          IF(ALLOCATED(TMP3D)) DEALLOCATE(TMP3D)
          ALLOCATE(TMP3D(IM,JM,NT))
          IF(ALLOCATED(ITMP3D)) DEALLOCATE(ITMP3D)
          ALLOCATE(ITMP3D(IM,JM,NT))
          IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
          ALLOCATE(TMP4D(IM,JM,KB,NT))

          STATUS=NF_INQ_VARID(NCID,'surf_el',IDVAR)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',SCALE)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',OFFSET)
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,TMP3D)
          MISSVALUE=IATT*SCALE+OFFSET
C          WRITE(*,*) 'First data=',TMP3D(1,1,1),TMP3D(2,1,1)
C          WRITE(*,*) 'factor=',SCALE,OFFSET,MISSVALUE

          ICOUNT=N0
          DO N=1,NT
	    ICOUNT=ICOUNT+1
            DO I=1,ISUB
              DO J=1,JSUB
                WL(I,J,ICOUNT)=TMP3D(IMIN+I-1,JMIN+J-1,N)*SCALE+
     &            OFFSET
                IF(ABS(WL(I,J,ICOUNT)-MISSVALUE).LE.0.001)
     &	          WL(I,J,ICOUNT)=-99999.9
                IF(IEEE_IS_NAN(WL(I,J,ICOUNT)))
     &             WL(I,J,ICOUNT)=-99999.9
              ENDDO	
            ENDDO	
          ENDDO	

          STATUS=NF_INQ_VARID(NCID,'water_temp',IDVAR)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
          MISSVALUE=IATT*SCALE+OFFSET

          ICOUNT=N0
          DO N=1,NT
            ICOUNT=ICOUNT+1
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  TEMP(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*
     &              SCALE+OFFSET
                  IF(ABS(TEMP(I,J,K,ICOUNT)-MISSVALUE).LE.0.001)
     &	            TEMP(I,J,K,ICOUNT)=-99999.9
                  IF(IEEE_IS_NAN(TEMP(I,J,K,ICOUNT)))
     &              TEMP(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO	

          STATUS=NF_INQ_VARID(NCID,'salinity',IDVAR)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
          MISSVALUE=IATT*SCALE+OFFSET

          ICOUNT=N0
          DO N=1,NT
            ICOUNT=ICOUNT+1
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  SALT(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*
     &              SCALE+OFFSET
                  IF(ABS(SALT(I,J,K,ICOUNT)-MISSVALUE).LE.0.001)
     &	            SALT(I,J,K,ICOUNT)=-99999.9
                  IF(IEEE_IS_NAN(SALT(I,J,K,ICOUNT)))
     &              SALT(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO
          ENDDO

          STATUS=NF_INQ_VARID(NCID,'water_u',IDVAR)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
          MISSVALUE=IATT*SCALE+OFFSET

          ICOUNT=N0
          DO N=1,NT
            ICOUNT=ICOUNT+1
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  U(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*
     &              SCALE+OFFSET
                  IF(ABS(U(I,J,K,ICOUNT)-MISSVALUE).LE.0.001)
     &	            U(I,J,K,ICOUNT)=-99999.9
                  IF(IEEE_IS_NAN(U(I,J,K,ICOUNT)))
     &              U(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO
          ENDDO

          STATUS=NF_INQ_VARID(NCID,'water_v',IDVAR)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
          MISSVALUE=IATT*SCALE+OFFSET

          ICOUNT=N0
          DO N=1,NT
            ICOUNT=ICOUNT+1
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  V(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*
     &              SCALE+OFFSET
                  IF(ABS(V(I,J,K,ICOUNT)-MISSVALUE).LE.0.001)
     &	            V(I,J,K,ICOUNT)=-99999.9
                  IF(IEEE_IS_NAN(V(I,J,K,ICOUNT)))
     &              V(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO
          ENDDO
          NREC=ICOUNT
          STATUS=NF_CLOSE(NCID)

cc Open NCOM tide file if it exists
          FIN=TRIM(FILE_NCOM_TIDE(IFILE))
          INQUIRE(FILE=TRIM(FIN),EXIST=FEXIST)
          IF(FEXIST) THEN
            STATUS=NF_OPEN(TRIM(FIN),NF_NOWRITE,NCID)
            IF(STATUS.NE.NF_NOERR) THEN
              WRITE(*,*) 'Error message= ',STATUS
              STOP
            ENDIF  
            STATUS=NF_INQ_VARID(NCID,'SEA_LEVEL',IDVAR)
            STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'FillValue',fillvalue)
            STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tmp3d)
C            WRITE(*,*) 'sea_level fillvalue=',FILLVALUE

            ICOUNT=N0
            DO N=1,NT
              ICOUNT=ICOUNT+1
              DO I=1,ISUB
                DO J=1,JSUB
                  TIDE_NCOM(I,J,ICOUNT)=TMP3D(IMIN+I-1,JMIN+J-1,N)
                  DIFF=WL(I,J,ICOUNT)-TIDE_NCOM(I,J,ICOUNT)
                  IF(ABS(TMP3D(IMIN+I-1,JMIN+J-1,N)).GT.29.0.OR.
     &              ABS(WL(I,J,ICOUNT)).GE.29.0) THEN
                    DIFF=-99999.9
	          ELSE  
cc  subtract tide from NCOM total water level
                    DIFF=WL(I,J,ICOUNT)-TIDE_NCOM(I,J,ICOUNT)
                  ENDIF
                  WRITE(26,100) N,I,J,LONSUB(I,J),LATSUB(I,J), 
     & 	            WL(I,J,ICOUNT),TIDE_NCOM(I,J,ICOUNT),DIFF
cc  nontidal WL is ~0.2 m lower from comparison made by Zizang
                  WL(I,J,ICOUNT)=DIFF+0.2
                ENDDO	
              ENDDO	      	
            ENDDO
100         FORMAT(3I8,10F12.4)
            STATUS=NF_CLOSE(NCID)
          ELSE
            WRITE(*,*) 'NCOM tide file does not match NCOM file'
            WRITE(*,*) 'Switch DBASE_WL=ETSS'
            WRITE(ICORMS,'(a)') 'Switch DBASE_WL to ETSS'
            DBASE_WL='ETSS'
          ENDIF	   
        ENDDO
        NT=NREC 
        WRITE(*,*) 'Number of NCOM data NREC= ',NREC
      ENDIF

C-----------------------------------------------------------------------
C     Open and read all RTOFS netCDF files available during start to end time 
C-----------------------------------------------------------------------
      IF(TRIM(DBASE_TS).EQ.'RTOFS') THEN
        NCOM_FILE='RTOFS_FILE'
        IFILE_NCOM=0
        INQUIRE(FILE=TRIM(NCOM_FILE),EXIST=FEXIST)
        IF(FEXIST) THEN
          CLOSE(60)
          OPEN(60,FILE=TRIM(NCOM_FILE),STATUS='OLD')
          DO I=1,999
            READ(60,'(A200)',END=335) BUFFER
            FILE_NCOM(I)=trim(adjustL(BUFFER))
          ENDDO
335       CLOSE(60)
          IFILE_NCOM=I-1
        ENDIF
        WRITE(*,*) 'Total number of RTOFS FILES= ',IFILE_NCOM

        IF(IFILE_NCOM.EQ.0) THEN
          WRITE(*,*) 'No RTOFS file is available in this period'
          WRITE(*,*) 'use HYCOM Backup to replace'
          DBASE_TS='HYCOM'
          WRITE(ICORMS,'(a)') 'RTOFS FILE IS NOT FOUND' 
          WRITE(ICORMS,'(a)') 'USE HYCOM BACKUP' 
          GOTO 30
        ENDIF  

        DO I=1,IFILE_NCOM
          WRITE(*,*) 'RTOFS I=',I,TRIM(FILE_NCOM(I))
        ENDDO

c-----------------------------------------------------------------------
c  Open and read all RTOFS netCDF files available during start to end time 
c-----------------------------------------------------------------------
        FIN=TRIM(FILE_NCOM(1))
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        VNAME='salinity'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IM=DIMS(1)
        JM=DIMS(2)
        KB=DIMS(3)
        NT=DIMS(4)
C        WRITE(*,*) 'RTOFS IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
        IF(ALLOCATED(LON)) DEALLOCATE(LON)
        IF(ALLOCATED(LAT)) DEALLOCATE(LAT)
        IF(ALLOCATED(DEPTH)) DEALLOCATE(DEPTH)
        IF(ALLOCATED(ZETA_TIME)) DEALLOCATE(ZETA_TIME)
        IF(ALLOCATED(TS_TIME)) DEALLOCATE(TS_TIME)
        ALLOCATE(LON(IM,JM))
        ALLOCATE(LAT(IM,JM))
        ALLOCATE(DEPTH(KB))
        ALLOCATE(ZETA_TIME(NMAX))
        ALLOCATE(TS_TIME(NMAX))

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='Longitude'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
C        WRITE(*,*) 'DIM Number of Longitude ',NDIM
        IF(NDIM.EQ.2) THEN
          DO I=1,IM
            DO J=1,JM
              LON(I,J)=TMP4D(I,J,1,1)
              IF(LON(I,J).GT.180.0) LON(I,J)=LON(I,J)-360.0
            ENDDO
          ENDDO
        ELSEIF(NDIM.EQ.1) THEN
          DO I=1,IM
            DO J=1,JM
              LON(I,J)=TMP4D(I,1,1,1)
              IF(LON(I,J).GT.180.0) LON(I,J)=LON(I,J)-360.0
            ENDDO
          ENDDO
        ENDIF  

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='Latitude'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
C        WRITE(*,*) 'DIM Number of Latitude ',NDIM
        IF(NDIM.EQ.2) THEN
          DO I=1,IM
            DO J=1,JM
              LAT(I,J)=TMP4D(I,J,1,1)
            ENDDO
          ENDDO
        ELSEIF(NDIM.EQ.1) THEN
          DO I=1,IM
            DO J=1,JM
              LAT(I,J)=TMP4D(J,1,1,1)
            ENDDO
          ENDDO
        ENDIF

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='Depth'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO K=1,KB
          DEPTH(K)=TMP4D(K,1,1,1)
        ENDDO

C-----------------------------------------------------------------------
C        select subdomain I and J index        
C-----------------------------------------------------------------------
        IMIN=99999
        IMAX=-9999
        JMIN=99999
        JMAX=-9999
        DO I=1,IM
          DO J=1,JM
            IF(LON(I,J).GE.MINLON.AND.LON(I,J).LE.MAXLON.AND.
     &         LAT(I,J).GE.MINLAT.AND.LAT(I,J).LE.MAXLAT) THEN
              IF(I.LT.IMIN) IMIN=I 	  
              IF(I.GT.IMAX) IMAX=I 	  
              IF(J.LT.JMIN) JMIN=J 	  
              IF(J.GT.JMAX) JMAX=J
	    ENDIF
          ENDDO
        ENDDO	  	  

        ISUB=IMAX-IMIN+1	 
        JSUB=JMAX-JMIN+1	 
        WRITE(*,*) 'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX

C-----------------------------------------------------------------------
C allocate sizes of arrays for NCOM products 
C-----------------------------------------------------------------------
        NNSUB=ISUB*JSUB
        IF(ALLOCATED(LONSUB)) DEALLOCATE(LONSUB)
        IF(ALLOCATED(LATSUB)) DEALLOCATE(LATSUB)
        IF(ALLOCATED(TEMP)) DEALLOCATE(TEMP)
        IF(ALLOCATED(SALT)) DEALLOCATE(SALT)
        IF(ALLOCATED(U)) DEALLOCATE(U)
        IF(ALLOCATED(V)) DEALLOCATE(V)
        IF(ALLOCATED(WL)) DEALLOCATE(WL)
        IF(ALLOCATED(UBAR_RTOFS)) DEALLOCATE(UBAR_RTOFS)
        IF(ALLOCATED(VBAR_RTOFS)) DEALLOCATE(VBAR_RTOFS)
        ALLOCATE(LONSUB(ISUB,JSUB))
        ALLOCATE(LATSUB(ISUB,JSUB))
        ALLOCATE(TEMP(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        ALLOCATE(SALT(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        ALLOCATE(U(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        ALLOCATE(V(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        ALLOCATE(WL(ISUB,JSUB,NT*IFILE_NCOM+10))
        ALLOCATE(UBAR_RTOFS(ISUB,JSUB,NT*IFILE_NCOM+10))
        ALLOCATE(VBAR_RTOFS(ISUB,JSUB,NT*IFILE_NCOM+10))

        DO N=1,NT*IFILE_NCOM+10
          DO J=1,JSUB
            DO I=1,ISUB
              WL(I,J,N)=0.0
              UBAR_RTOFS(I,J,N)=0.0
              VBAR_RTOFS(I,J,N)=0.0
            ENDDO
          ENDDO
        ENDDO   

        CLOSE(79)
        DO J=1,JSUB
          DO I=1,ISUB
            LONSUB(I,J)=LON(IMIN+I-1,JMIN+J-1)
            LATSUB(I,J)=LAT(IMIN+I-1,JMIN+J-1)
	    WRITE(79,'(2F12.4,2I6)') LONSUB(I,J),LATSUB(I,J),I,J
          ENDDO
        ENDDO  
        CLOSE(79) 

        ICOUNT=0
        NREC=0
        TIMELAST=-99999.9 
        DO IFILE=1,IFILE_NCOM
          WRITE(*,*) 'Reading RTOFS NetCDF file...',IFILE
          FIN=TRIM(FILE_NCOM(IFILE))
          VNAME='MT'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
          IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
          ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          NT=DIMS(1)
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
          ANAME=TRIM(ADJUSTL(ANAME))
          LEN=LEN_TRIM(ANAME)
          LL=INDEX(ANAME,'minute') 
          IF(LL.GT.0) scale_time=1.0/1440.0
          LL=INDEX(ANAME,'hour')
          IF(LL.GT.0) scale_time=1.0/24.0
          LL=INDEX(ANAME,'day')
          IF(LL.GT.0) scale_time=1.0
          LL=INDEX(ANAME,'since')
          IF(LL.GT.0) THEN
            read(ANAME(LL+6:LEN),80) IYR,IMM,IDD,IHH
            IF(IFILE.EQ.1) THEN
              WRITE(*,*) 'Basetime=',IYR,IMM,IDD,IHH
            END IF
          ELSE
            WRITE(*,*) 'There is error while reading base date'
            STOP
          ENDIF

          YEARB=IYR
          MONTHB=IMM
          DAYB=IDD
          HOURB=IHH
          JDAY=JULIAN(YEARB,MONTHB,DAYB,HOURB)
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
          TIME_NCOM=TMP4D(1,1,1,1)*SCALE_TIME+JDAY-JBASE_DATE
          IF(TIMELAST.LT.TIME_NCOM) THEN
            ICOUNT=ICOUNT+1
            TS_TIME(ICOUNT)=TIME_NCOM
            TIMELAST=TIME_NCOM
          ENDIF

          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) THEN
            WRITE(*,*) 'Error message= ',STATUS
            STOP
          ENDIF
          IF(ALLOCATED(TMP1D)) DEALLOCATE(TMP1D)
          ALLOCATE(TMP1D(IM))
          IF(ALLOCATED(TMP3D)) DEALLOCATE(TMP3D)
          ALLOCATE(TMP3D(IM,JM,NT))
          IF(ALLOCATED(ITMP3D)) DEALLOCATE(ITMP3D)
          ALLOCATE(ITMP3D(IM,JM,NT))
          IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
          ALLOCATE(TMP4D(IM,JM,KB,NT))

          STATUS=NF_INQ_VARID(NCID,'temperature',IDVAR)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  TEMP(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
                  IF(ABS(TEMP(I,J,K,ICOUNT)).GE.99.0)
     &	            TEMP(I,J,K,ICOUNT)=-99999.9
                  IF(IEEE_IS_NAN(TEMP(I,J,K,ICOUNT)))
     &              TEMP(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO	

          STATUS=NF_INQ_VARID(NCID,'salinity',IDVAR)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  SALT(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
                  IF(ABS(SALT(I,J,K,ICOUNT)).GE.99.0)
     &	            SALT(I,J,K,ICOUNT)=-99999.9
                  IF(IEEE_IS_NAN(SALT(I,J,K,ICOUNT)))
     &              SALT(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO	

          STATUS=NF_INQ_VARID(NCID,'u',IDVAR)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  U(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
                  IF(ABS(U(I,J,K,ICOUNT)).GE.99.0)
     &	            U(I,J,K,ICOUNT)=-99999.9
                  IF(IEEE_IS_NAN(U(I,J,K,ICOUNT)))
     &              U(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO	

          STATUS=NF_INQ_VARID(NCID,'v',IDVAR)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  V(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
                  IF(ABS(V(I,J,K,ICOUNT)).GE.99.0)
     &	            V(I,J,K,ICOUNT)=-99999.9
                  IF(IEEE_IS_NAN(V(I,J,K,ICOUNT)))
     &              V(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO

cc  calculate ubar and vbar from U and V
          DO I=1,ISUB
            DO J=1,JSUB
              UBAR_RTOFS(I,J,ICOUNT)=0.0
              VBAR_RTOFS(I,J,ICOUNT)=0.0
              KMAX1=0
              DO K=1,KB
                IF(ABS(U(I,J,K,ICOUNT)).LT.99.0) KMAX1=KMAX1+1
              ENDDO

              IF(KMAX1.EQ.1) THEN
                UBAR_RTOFS(I,J,ICOUNT)=U(I,J,KMAX1,ICOUNT)
                VBAR_RTOFS(I,J,ICOUNT)=V(I,J,KMAX1,ICOUNT)
              ELSEIF(KMAX1.GE.2) THEN
                DO K=1,KMAX1-1
                  UBAR_RTOFS(I,J,ICOUNT)=UBAR_RTOFS(I,J,ICOUNT)+
     &              (U(I,J,K+1,ICOUNT)+U(I,J,K,ICOUNT))/2.0*
     &              (DEPTH(K+1)-DEPTH(K))
                  VBAR_RTOFS(I,J,ICOUNT)=VBAR_RTOFS(I,J,ICOUNT)+
     &              (V(I,J,K+1,ICOUNT)+V(I,J,K,ICOUNT))/2.0*
     &              (DEPTH(K+1)-DEPTH(K))
                ENDDO
                UBAR_RTOFS(I,J,ICOUNT)=UBAR_RTOFS(I,J,ICOUNT)/
     &            DEPTH(KMAX1)
                VBAR_RTOFS(I,J,ICOUNT)=VBAR_RTOFS(I,J,ICOUNT)/
     &            DEPTH(KMAX1)
              END IF
            ENDDO	
          ENDDO	
cc  end calculate UBAR and VBAR
	
          NREC=ICOUNT
          STATUS=NF_CLOSE(NCID)
        ENDDO      
        NT=NREC 
        WRITE(*,*) 'Number of RTOFS data NREC= ',NREC
      ENDIF

C-----------------------------------------------------------------------
C   Find all HYCOM netCDF files available during start to end time 
C-----------------------------------------------------------------------
      IF(TRIM(DBASE_TS).EQ.'HYCOM') THEN
        NCOM_FILE='HYCOM_FILE'
        IFILE_NCOM=0
        INQUIRE(FILE=TRIM(NCOM_FILE),EXIST=FEXIST)
        IF(FEXIST) THEN
          CLOSE(60)
          OPEN(60,file=TRIM(NCOM_FILE),STATUS='OLD')
          DO I=1,999
            READ(60,'(a200)',END=325) BUFFER
            FILE_NCOM(I)=TRIM(ADJUSTL(BUFFER))
          ENDDO
325       CLOSE(60)
          IFILE_NCOM=I-1
        ENDIF
        WRITE(*,*) 'Total number of HYCOM FILES= ',IFILE_NCOM

        IF(IFILE_NCOM.EQ.0) THEN
          WRITE(*,*) 'WARNING:'
          WRITE(*,*) 'No HYCOM file is available in this period'
          WRITE(*,*) 'Use WOA05 climatological dataset to replace'
          DBASE_TS='WOA05'
          IF(TRIM(DBASE_WL).EQ.'HYCOM') DBASE_WL='ETSS'
          WRITE(ICORMS,'(a)') 'WARNING:'
          WRITE(ICORMS,'(a)') 'HYCOM FILE IS NOT FOUND' 
          WRITE(ICORMS,'(a)') 'USE CLIMATOLOGIC BACKUP WOA05' 
          GOTO 30
        ENDIF  

        DO I=1,IFILE_NCOM
          WRITE(*,*) 'HYCOM I=',I,TRIM(FILE_NCOM(I))
        ENDDO

C-----------------------------------------------------------------------
C   Read all HYCOM netCDF files available during start to end time 
C-----------------------------------------------------------------------
        FIN=TRIM(FILE_NCOM(1))
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        VNAME='salinity'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IM=DIMS(1)
        JM=DIMS(2)
        KB=DIMS(3)
        NT=DIMS(4)
C        WRITE(*,*) 'HYCOM IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT

        IF(ALLOCATED(LON)) DEALLOCATE(LON)
        IF(ALLOCATED(LAT)) DEALLOCATE(LAT)
        IF(ALLOCATED(DEPTH)) DEALLOCATE(DEPTH)
        IF(ALLOCATED(ZETA_TIME)) DEALLOCATE(ZETA_TIME)
        IF(ALLOCATED(TS_TIME)) DEALLOCATE(TS_TIME)
        ALLOCATE(LON(IM,JM))
        ALLOCATE(LAT(IM,JM))
        ALLOCATE(DEPTH(KB))
        ALLOCATE(ZETA_TIME(NMAX))
        ALLOCATE(TS_TIME(NMAX))

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='lon'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
C        WRITE(*,*) 'DIM Number of Longitude ',NDIM
        IF(NDIM.EQ.2) THEN
          DO I=1,IM
            DO J=1,JM
              LON(I,J)=TMP4D(I,J,1,1)
              IF(LON(I,J).GT.180.0) LON(I,J)=LON(I,J)-360.0
            ENDDO
          ENDDO
        ELSEIF(NDIM.EQ.1) THEN
          DO I=1,IM
            DO J=1,JM
              LON(I,J)=TMP4D(I,1,1,1)
 	      IF(LON(I,J).GT.180.0) LON(I,J)=LON(I,J)-360.0
            ENDDO
          ENDDO
        ENDIF  

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='lat'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
C        WRITE(*,*) 'DIM Number of Latitude ',NDIM
        IF(NDIM.EQ.2) THEN
          DO I=1,IM
            DO J=1,JM
              LAT(I,J)=TMP4D(I,J,1,1)
            ENDDO
          ENDDO
        ELSEIF(NDIM.EQ.1) THEN
          DO I=1,IM
            DO J=1,JM
              LAT(I,J)=TMP4D(J,1,1,1)
            ENDDO
          ENDDO
        ENDIF

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='depth'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO K=1,KB
          DEPTH(K)=TMP4D(K,1,1,1)
        ENDDO

C-----------------------------------------------------------------------
C    Select subdomain I and J index        
C-----------------------------------------------------------------------
        IMIN=99999
        IMAX=-9999
        JMIN=99999
        JMAX=-9999
        DO I=1,IM
          DO J=1,JM
            IF(LON(I,J).GE.MINLON.AND.LON(I,J).LE.MAXLON.AND.
     &         LAT(I,J).GE.MINLAT.AND.LAT(I,J).LE.MAXLAT) THEN
              IF(I.LT.IMIN) IMIN=I 	  
              IF(I.GT.IMAX) IMAX=I 	  
              IF(J.LT.JMIN) JMIN=J 	  
              IF(J.GT.JMAX) JMAX=J
            ENDIF
          ENDDO
        ENDDO	  	  
        ISUB=IMAX-IMIN+1	 
        JSUB=JMAX-JMIN+1	 
        WRITE(*,*) 'Subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX

C-----------------------------------------------------------------------
C   Allocate sizes of arrays for HYCOM products
C-----------------------------------------------------------------------
        NNSUB=ISUB*JSUB
        IF(ALLOCATED(LONSUB)) DEALLOCATE(LONSUB)
        IF(ALLOCATED(LATSUB)) DEALLOCATE(LATSUB)
        IF(ALLOCATED(TEMP)) DEALLOCATE(TEMP)
        IF(ALLOCATED(SALT)) DEALLOCATE(SALT)
        IF(ALLOCATED(U)) DEALLOCATE(U)
        IF(ALLOCATED(V)) DEALLOCATE(V)
        IF(ALLOCATED(WL)) DEALLOCATE(WL)
        IF(ALLOCATED(UBAR_RTOFS)) DEALLOCATE(UBAR_RTOFS)
        IF(ALLOCATED(VBAR_RTOFS)) DEALLOCATE(VBAR_RTOFS)
        ALLOCATE(LONSUB(ISUB,JSUB))
        ALLOCATE(LATSUB(ISUB,JSUB))
        ALLOCATE(TEMP(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        ALLOCATE(SALT(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        ALLOCATE(U(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        ALLOCATE(V(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        ALLOCATE(WL(ISUB,JSUB,NT*IFILE_NCOM+10))
        ALLOCATE(TIDE_NCOM(ISUB,JSUB,NT*IFILE_NCOM+10))
        ALLOCATE(UBAR_RTOFS(ISUB,JSUB,NT*IFILE_NCOM+10))
        ALLOCATE(VBAR_RTOFS(ISUB,JSUB,NT*IFILE_NCOM+10))

        DO N=1,NT*IFILE_NCOM+10
          DO J=1,JSUB
            DO I=1,ISUB
	      WL(I,J,N)=0.0
              UBAR_RTOFS(I,J,N)=0.0
              VBAR_RTOFS(I,J,N)=0.0
	      TIDE_NCOM(I,J,N)=0.0
            ENDDO
          ENDDO
        ENDDO   

        CLOSE(79)
        OPEN(79,FILE='LONLAT_HYCOM.DAT',STATUS='UNKNOWN')
        DO J=1,JSUB
          DO I=1,ISUB
            LONSUB(I,J)=LON(IMIN+I-1,JMIN+J-1)
            LATSUB(I,J)=LAT(IMIN+I-1,JMIN+J-1)
	    WRITE(79,'(2F12.4,2I6)') LONSUB(I,J),LATSUB(I,J),I,J
          ENDDO
        ENDDO  
        CLOSE(79)

        ICOUNT=0
        NREC=0
        TIMELAST=-99999.9
        DO IFILE=1,IFILE_NCOM
          WRITE(*,*) 'Reading HYCOM NetCDF file: ',IFILE
          FIN=TRIM(FILE_NCOM(IFILE))
          VNAME='time'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
          IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
          ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          NT=DIMS(1)
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
          ANAME=TRIM(ADJUSTL(ANAME))
          LEN=LEN_TRIM(ANAME)
          LL=INDEX(ANAME,'minute')         
          IF(LL.GT.0) SCALE_TIME=1.0/1440.0
          LL=INDEX(ANAME,'hour')         
          IF(LL.GT.0) SCALE_TIME=1.0/24.0
          LL=INDEX(ANAME,'day')         
          IF(LL.GT.0) SCALE_TIME=1.0
          LL=INDEX(ANAME,'since')         
          IF(LL.GT.0) THEN
            READ(ANAME(LL+6:LEN),80) IYR,IMM,IDD,IHH
            WRITE(*,*) 'Basetime=',IYR,IMM,IDD,IHH
          ELSE
            WRITE(*,*) 'There is error while reading base date'
            STOP
          ENDIF    

          YEARB=IYR
          MONTHB=IMM
          DAYB=IDD
          HOURB=IHH
          JDAY=JULIAN(YEARB,MONTHB,DAYB,HOURB)
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
          TIME_NCOM=TMP4D(1,1,1,1)*SCALE_TIME+JDAY-JBASE_DATE
          IF(TIMELAST.LT.TIME_NCOM) THEN
            ICOUNT=ICOUNT+1
            TS_TIME(ICOUNT)=TIME_NCOM
            TIMELAST=TIME_NCOM 
          ENDIF

          STATUS=NF_OPEN(TRIM(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) THEN
	    WRITE(*,*) 'Error message= ',STATUS
	    STOP
          ENDIF  

          IF(ALLOCATED(TMP1D)) DEALLOCATE(TMP1D)
          ALLOCATE(TMP1D(IM))
          IF(ALLOCATED(TMP3D)) DEALLOCATE(TMP3D)
          ALLOCATE(TMP3D(IM,JM,NT))
          IF(ALLOCATED(ITMP3D)) DEALLOCATE(ITMP3D)
          ALLOCATE(ITMP3D(IM,JM,NT))
          IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
          ALLOCATE(TMP4D(IM,JM,KB,NT))
          STATUS=NF_INQ_VARID(NCID,'surf_el',IDVAR)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tmp3d)
          MISSVALUE=IATT*SCALE+OFFSET
C          WRITE(*,*) 'First data=',tmp3d(1,1,1),tmp3d(2,1,1)
C          WRITE(*,*) 'factor=',SCALE,OFFSET,MISSVALUE

          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                IJJ=IMIN+I-1
                JJJ=JMIN+J-1
                WL(I,J,ICOUNT)=TMP3D(IJJ,JJJ,N)*SCALE+OFFSET
                IF(ABS(WL(I,J,ICOUNT)-MISSVALUE).LE.0.001)
     &	          WL(I,J,ICOUNT)=-99999.9
                IF(IEEE_IS_NAN(WL(I,J,ICOUNT)))
     &            WL(I,J,ICOUNT)=-99999.9
              ENDDO
            ENDDO
          ENDDO

          STATUS=NF_INQ_VARID(NCID,'water_temp',IDVAR)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
          MISSVALUE=IATT*SCALE+OFFSET
C          WRITE(*,*) 'scale of temp=',SCALE,OFFSET,MISSVALUE

          DO N=1,NT
           DO I=1,ISUB
            DO J=1,JSUB
             DO K=1,KB
              IJJ=IMIN+I-1
              JJJ=JMIN+J-1
              TEMP(I,J,K,ICOUNT)=TMP4D(IJJ,JJJ,K,N)*SCALE+OFFSET
              IF(ABS(TEMP(I,J,K,ICOUNT)-MISSVALUE).LE.0.001)
     &	        TEMP(I,J,K,ICOUNT)=-99999.9
              IF(IEEE_IS_NAN(TEMP(I,J,K,ICOUNT)))
     &          TEMP(I,J,K,ICOUNT)=-99999.9
             ENDDO	
            ENDDO	
           ENDDO	
          ENDDO

          STATUS=NF_INQ_VARID(NCID,'salinity',IDVAR)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
          MISSVALUE=IATT*SCALE+OFFSET

          DO N=1,NT
           DO I=1,ISUB
            DO J=1,JSUB
             DO K=1,KB
              IJJ=IMIN+I-1
              JJJ=JMIN+J-1
              SALT(I,J,K,ICOUNT)=TMP4D(IJJ,JJJ,K,N)*SCALE+OFFSET
              IF(ABS(SALT(I,J,K,ICOUNT)-MISSVALUE).LE.0.001)
     &	        SALT(I,J,K,ICOUNT)=-99999.9
              IF(IEEE_IS_NAN(SALT(I,J,K,ICOUNT)))
     &          SALT(I,J,K,ICOUNT)=-99999.9
             ENDDO	
            ENDDO	
           ENDDO	
          ENDDO	

          STATUS=NF_INQ_VARID(NCID,'water_u',IDVAR)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
          MISSVALUE=IATT*SCALE+OFFSET

          DO N=1,NT
           DO I=1,ISUB
            DO J=1,JSUB
             DO K=1,KB
              IJJ=IMIN+I-1
              JJJ=JMIN+J-1
              U(I,J,K,ICOUNT)=TMP4D(IJJ,JJJ,K,N)*SCALE+OFFSET
              IF(ABS(U(I,J,K,ICOUNT)-MISSVALUE).LE.0.001)
     &	        U(I,J,K,ICOUNT)=-99999.9
              IF(ABS(U(I,J,K,ICOUNT)).GE.99.0)
     &	        U(I,J,K,ICOUNT)=-99999.9
              IF(IEEE_IS_NAN(U(I,J,K,ICOUNT)))
     &          U(I,J,K,ICOUNT)=-99999.9
             ENDDO	
            ENDDO	
           ENDDO	
          ENDDO	

          STATUS=NF_INQ_VARID(NCID,'water_v',IDVAR)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
          MISSVALUE=IATT*SCALE+OFFSET

          DO N=1,NT
           DO I=1,ISUB
            DO J=1,JSUB
             DO K=1,KB
              IJJ=IMIN+I-1
              JJJ=JMIN+J-1
              V(I,J,K,ICOUNT)=TMP4D(IJJ,JJJ,K,N)*SCALE+OFFSET
              IF(ABS(V(I,J,K,ICOUNT)-MISSVALUE).LE.0.001)
     &	        V(I,J,K,ICOUNT)=-99999.9
              IF(ABS(V(I,J,K,ICOUNT)).GE.99.0)
     &	        V(I,J,K,ICOUNT)=-99999.9
              IF(IEEE_IS_NAN(V(I,J,K,ICOUNT)))
     &          V(I,J,K,ICOUNT)=-99999.9
             ENDDO	
            ENDDO	
           ENDDO	
          ENDDO

C   Calculate ubar and vbar from U and V
          DO I=1,ISUB
            DO J=1,JSUB
              UBAR_RTOFS(I,J,ICOUNT)=0.0
              VBAR_RTOFS(I,J,ICOUNT)=0.0
              KMAX1=0
              DO K=1,KB
                IF(ABS(U(I,J,K,ICOUNT)).LT.99.0) KMAX1=KMAX1+1
              ENDDO
              IF(KMAX1.EQ.1) THEN
                UBAR_RTOFS(I,J,ICOUNT)=U(I,J,KMAX1,ICOUNT)
                VBAR_RTOFS(I,J,ICOUNT)=V(I,J,KMAX1,ICOUNT)
              ELSE IF(KMAX1.GE.2) THEN
                DO K=1,KMAX1-1
                  UBAR_RTOFS(I,J,ICOUNT)=UBAR_RTOFS(I,J,ICOUNT)+
     &              (U(I,J,K+1,ICOUNT)+U(I,J,K,ICOUNT))/2.0*
     &              (DEPTH(K+1)-DEPTH(K))
                  VBAR_RTOFS(I,J,ICOUNT)=VBAR_RTOFS(I,J,ICOUNT)+
     &              (V(I,J,K+1,ICOUNT)+V(I,J,K,ICOUNT))/2.0*
     &              (DEPTH(K+1)-DEPTH(K))
                ENDDO
                UBAR_RTOFS(I,J,ICOUNT)=
     &            UBAR_RTOFS(I,J,ICOUNT)/DEPTH(KMAX1)
                VBAR_RTOFS(I,J,ICOUNT)=
     &            VBAR_RTOFS(I,J,ICOUNT)/DEPTH(KMAX1)
              END IF
            ENDDO	
          ENDDO	
C   End of calculating UBAR and VBAR
	
	  NREC=ICOUNT
          STATUS=NF_CLOSE(NCID)
        ENDDO      
        NT=NREC 
        WRITE(*,*) 'Number of HYCOM data NREC= ',NREC

        CLOSE(79)
        OPEN(79,FILE='salt_HYCOM.dat',STATUS='UNKNOWN')
        DO I=1,ISUB
          DO J=1,JSUB
	    IF(SALT(I,J,1,1).GE.0.0) THEN
              WRITE(79,'(F10.4,2I5,100F9.3)') TS_TIME(1),I,J,
     &	        LONSUB(I,J),LATSUB(I,J),(SALT(I,J,K,1),K=1,KB)
            ENDIF
          ENDDO	
        ENDDO	
        CLOSE(79)

        OPEN(79,FILE='temp_HYCOM.dat',STATUS='UNKNOWN')
        DO I=1,ISUB
          DO J=1,JSUB
	    IF(TEMP(I,J,1,1).GE.0.0) THEN
              WRITE(79,'(F10.4,2I5,100F9.3)') TS_TIME(1),I,J,
     &	        LONSUB(I,J),LATSUB(I,J),(TEMP(I,J,K,1),K=1,KB)
            ENDIF
          ENDDO	
        ENDDO	
        CLOSE(79)  	
      ENDIF

C-----------------------------------------------------------------------
C   End of reading native products of WOA05 or NCOM, or RTOFS, or HYCOM 
C-----------------------------------------------------------------------
      N=1
      DO WHILE ((TS_TIME(N).LT.DAY_START).AND.(N.LE.NT))
        N=N+1
      ENDDO
      NSTR=N-1
      IF(NSTR.EQ.0.OR.NSTR.GT.NT) THEN
        DO NN=NT,1,-1
          TS_TIME(NN+1)=TS_TIME(NN)
          DO I=1,ISUB
            DO J=1,JSUB
	      WL(I,J,NN+1)=WL(I,J,NN) 
              DO K=1,KB
                TEMP(I,J,K,NN+1)=TEMP(I,J,K,NN)
                SALT(I,J,K,NN+1)=SALT(I,J,K,NN)
                U(I,J,K,NN+1)=U(I,J,K,NN)
                V(I,J,K,NN+1)=V(I,J,K,NN)
              ENDDO	
            ENDDO	
          ENDDO
        ENDDO
        TS_TIME(1)=DAY_START
        NT=NT+1
        NSTR=1
      ENDIF

      IF(TS_TIME(NT).LT.DAY_END) THEN
        DIFF=DAY_END-TS_TIME(NT)
        IF(DIFF.LT.5.0) THEN 
C  Allow persistence for the maximum 5 days.
          TS_TIME(NT+1)=day_end
          DO I=1,ISUB
            DO J=1,JSUB
	      WL(I,J,NT+1)=WL(I,J,NT) 
              DO K=1,KB
                TEMP(I,J,K,NT+1)=TEMP(I,J,K,NT)
                SALT(I,J,K,NT+1)=SALT(I,J,K,NT)
                U(I,J,K,NT+1)=U(I,J,K,NT)
                V(I,J,K,NT+1)=V(I,J,K,NT)
              ENDDO	
            ENDDO	
          ENDDO
          NT=NT+1
        ELSE  
          WRITE(*,*) TRIM(DBASE_TS)//' data is shorter than day_end'
          WRITE(*,*) 'The forcast time period is not covered by NCOM'	  
          WRITE(*,*) 'USE CLIMATOLOGIC BACKUP WOA05' 
          TMPTXT=' '
          TMPTXT=TRIM(DBASE_TS)//' is too short'
          WRITE(ICORMS,'(a)') trim(TMPTXT)
          TMPTXT=' '
          TMPTXT='USE CLIMATOLOGIC BACKUP WOA05'
          WRITE(ICORMS,'(a)') trim(TMPTXT)
          DBASE_TS='WOA05'
          GOTO 30
        ENDIF  
      ENDIF    

      N=1
      DO WHILE ((TS_TIME(N).LT.DAY_END).AND.(N.LE.NT))
        N=N+1
      ENDDO
      NEND=N
      IF(N.GT.NT) THEN
	WRITE(*,*) ' Time period is not covered by dataset'	  
        TMPTXT=' '
        TMPTXT='Time period is not covered by dataset'
        WRITE(ICORMS,'(a)') trim(TMPTXT)
        TMPTXT=' '
        TMPTXT='Generation of OBC failed'
	WRITE(ICORMS,'(a)') trim(TMPTXT)
        TMPTXT=' '
        TMPTXT='Stop in nos_ofs_create_OBC.f'
	WRITE(ICORMS,'(a)') trim(TMPTXT)
	STOP
      ENDIF  	  
      WRITE(*,*) 'Start and End time of = ',
     &   TS_TIME(NSTR),TS_TIME(NEND),NSTR,NEND,DAY_START,DAY_END 		
      NTMAX=NEND-NSTR+1
      IF(ALLOCATED(TIMEOBC)) DEALLOCATE(TIMEOBC)
      ALLOCATE(TIMEOBC(NTMAX))
      DO N=1,NTMAX
        TIMEOBC(N)=TS_TIME(NSTR+N-1)
      ENDDO

      IF(TRIM(DBASE_WL).EQ.'NCOM'.OR.TRIM(DBASE_WL).EQ.'HYCOM') THEN
        NTMAX_WL=NTMAX
        IF(ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
        ALLOCATE(ZETA_TIME(NTMAX_WL))
        DO N=1,NTMAX_WL
          ZETA_TIME(N)=TIMEOBC(N)
        ENDDO
      ENDIF

      K=1
      DO WHILE ((DEPTH(K).LT.HMAX).AND.(K.LT.KB))
        K=K+1
      ENDDO
      KMAX=K
      WRITE(*,*) 'MAX VERTICAL LEVELS NEEDED IS ',KMAX,DEPTH(KMAX),HMAX

C-----------------------------------------------------------------------
C   Begin to process lateral open boundary conditions 
C-----------------------------------------------------------------------
      IF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
	IF(ALLOCATED(IOUT)) DEALLOCATE(IOUT)
	IF(ALLOCATED(JOUT)) DEALLOCATE(JOUT)
	IF(ALLOCATED(OUTM)) DEALLOCATE(OUTM)
	IF(ALLOCATED(LON_IN)) DEALLOCATE(LON_IN)
	IF(ALLOCATED(LAT_IN)) DEALLOCATE(LAT_IN)
	IF(ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
        ALLOCATE(IOUT(NOBC,1))
        ALLOCATE(JOUT(NOBC,1))
        ALLOCATE(OUTM(NOBC,1))
        ALLOCATE(LON_IN(NOBC,1))
        ALLOCATE(LAT_IN(NOBC,1))
        ALLOCATE(TMP2D(ISUB,JSUB))

        DO I=1,NOBC
          LON_IN(I,1)=LONOBC(I)
          LAT_IN(I,1)=LATOBC(I)
        ENDDO

        WRITE(*,*) 'Search coarser grid indices directions done!!!'
        IF(IGRD.EQ.2) IFLAG=1
        IF(IGRD.EQ.3) IFLAG=0
        CALL INTERP_REGRID(IFLAG,ISUB,JSUB,LONSUB,LATSUB,TMP2D,
     &       NOBC,1,LON_IN,LAT_IN,OUTM,IOUT,JOUT,0)
      ENDIF 

      IF(ALLOCATED(WLOBC)) DEALLOCATE(WLOBC)
      IF(ALLOCATED(TEMPOBC)) DEALLOCATE(TEMPOBC)
      IF(ALLOCATED(SALTOBC)) DEALLOCATE(SALTOBC)
      IF(ALLOCATED(UEOBC)) DEALLOCATE(UEOBC)
      IF(ALLOCATED(VEOBC)) DEALLOCATE(VEOBC)
      ALLOCATE(WLOBC(NOBC,NTMAX))
      ALLOCATE(TEMPOBC(NOBC,KB,NTMAX))
      ALLOCATE(SALTOBC(NOBC,KB,NTMAX))
      ALLOCATE(UEOBC(NEOBC,KB,NTMAX))
      ALLOCATE(VEOBC(NEOBC,KB,NTMAX))

      IF(ALLOCATED(TEMPOBC_M)) DEALLOCATE(TEMPOBC_M)
      IF(ALLOCATED(SALTOBC_M)) DEALLOCATE(SALTOBC_M)
      IF(ALLOCATED(UEOBC_M)) DEALLOCATE(UEOBC_M)
      IF(ALLOCATED(VEOBC_M)) DEALLOCATE(VEOBC_M)
      IF(ALLOCATED(UBAROBC)) DEALLOCATE(UBAROBC)
      IF(ALLOCATED(VBAROBC)) DEALLOCATE(VBAROBC)
      ALLOCATE(TEMPOBC_M(NOBC,KBM,NTMAX))
      ALLOCATE(SALTOBC_M(NOBC,KBM,NTMAX))
      ALLOCATE(UEOBC_M(NEOBC,KBM,NTMAX))
      ALLOCATE(VEOBC_M(NEOBC,KBM,NTMAX))
      ALLOCATE(UBAROBC(NEOBC,NTMAX))
      ALLOCATE(VBAROBC(NEOBC,NTMAX))

      IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
        IF(ALLOCATED(XINP)) DEALLOCATE(XINP)
        IF(ALLOCATED(YINP)) DEALLOCATE(YINP)
        IF(ALLOCATED(ZINP)) DEALLOCATE(ZINP)
        IF(ALLOCATED(XOUT)) DEALLOCATE(XOUT)
        IF(ALLOCATED(YOUT)) DEALLOCATE(YOUT)
        IF(ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
        IF(ALLOCATED(WEIGHTNODES)) DEALLOCATE(WEIGHTNODES)
        IF(ALLOCATED(WEIGHTS)) DEALLOCATE(WEIGHTS)
        ALLOCATE(XINP(NNSUB))
        ALLOCATE(YINP(NNSUB))
        ALLOCATE(ZINP(NNSUB))
        ALLOCATE(XOUT(NOBC))
        ALLOCATE(YOUT(NOBC))
        ALLOCATE(ZOUT(NOBC))
        ALLOCATE(WEIGHTNODES(NOBC,3))
        ALLOCATE(WEIGHTS(NOBC,3))
        CALL SEARCH_OUTPUT(ISUB,JSUB,LONSUB,LATSUB,NOBC,LONOBC,
     &    LATOBC,NSELECT_PARENT,IPARENT,JPARENT)
      ENDIF

c------------------------------------------------------------------
C   Processing Water Level along Open Boundary
c------------------------------------------------------------------
      IF(TRIM(DBASE_WL).EQ.'NCOM'.OR.TRIM(DBASE_WL).EQ.'HYCOM') THEN
        DO N=NSTR,NEND
	  N0=N-NSTR+1
          IF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
            DO I=1,ISUB
              DO J=1,JSUB
                TMP2D(I,J)=WL(I,J,N)
              ENDDO
            ENDDO
            IF(IGRD.EQ.2) IFLAG=1
            IF(IGRD.EQ.3) IFLAG=0
            CALL INTERP_REGRID(IFLAG,ISUB,JSUB,LONSUB,LATSUB,TMP2D,
     &        NOBC,1,LON_IN,LAT_IN,OUTM,IOUT,JOUT,1)
            DO I=1,NOBC
              WLOBC(I,N0)=OUTM(I,1)
	    ENDDO   
 	  ELSEIF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
            NDUM=0 
            DO NS=1,NSELECT_PARENT
              IF(WL(IPARENT(NS),JPARENT(NS),N).GT.-99.0) THEN
                NDUM=NDUM+1
                XINP(NDUM)=LONSUB(IPARENT(NS),JPARENT(NS))
                YINP(NDUM)=LATSUB(IPARENT(NS),JPARENT(NS))
                ZINP(NDUM)=WL(IPARENT(NS),JPARENT(NS),N)
              ENDIF
            ENDDO
	    NDATA=NDUM

            IF(IGRD.EQ.1) THEN
              CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &          NOBC,LONOBC,LATOBC,ZOUT,WEIGHTNODES,WEIGHTS,0)
              CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &          NOBC,LONOBC,LATOBC,ZOUT,WEIGHTNODES,WEIGHTS,1)
    	    ELSEIF(IGRD.EQ.4) THEN
              CALL INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &          NOBC,LONOBC,LATOBC,ZOUT)
            ENDIF
            DO I=1,NOBC
              WLOBC(I,N0)=ZOUT(I)
	    ENDDO   
          ENDIF
        ENDDO
      ENDIF

c------------------------------------------------------------------
C   Processing Temperature and Salinity along Open Boundary
c------------------------------------------------------------------
300   CONTINUE
      WRITE(*,*) 'Begin horizontal interpolation for T & S'
      DO K=1,KMAX
        IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN  
          NDUM=0
          DO NS=1,NSELECT_PARENT
            IF(ABS(TEMP(IPARENT(NS),JPARENT(NS),K,1)).LE.99.0) THEN
              NDUM=NDUM+1
	      XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
              YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
	      ZINP(ndum)=temp(IPARENT(NS),JPARENT(NS),K,1)
            ENDIF	
          ENDDO	
	  NDATA=NDUM
 
   	  IF(IGRD.EQ.1.AND.NDATA.GT.3) THEN                                  
            CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,LONOBC,
     &        LATOBC,ZOUT,WEIGHTNODES,WEIGHTS,0)
          ENDIF
        ENDIF 

        DO N=NSTR,NEND
	  N0=N-NSTR+1
          IF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
            DO I=1,ISUB
              DO J=1,JSUB
                TMP2D(I,J)=TEMP(I,J,K,N)
              ENDDO
            ENDDO
            IF(IGRD.EQ.2) IFLAG=1
            IF(IGRD.EQ.3) IFLAG=0
            CALL INTERP_REGRID(IFLAG,ISUB,JSUB,LONSUB,LATSUB,TMP2D,
     &        NOBC,1,LON_IN,LAT_IN,OUTM,IOUT,JOUT,1)
            DO I=1,NOBC
              TEMPOBC(I,K,N0)=OUTM(I,1)
            ENDDO

            DO I=1,ISUB
              DO J=1,JSUB
                TMP2D(I,J)=SALT(I,J,K,N)
              ENDDO
            ENDDO
            CALL INTERP_REGRID(IFLAG,ISUB,JSUB,LONSUB,LATSUB,TMP2D,
     &        NOBC,1,LON_IN,LAT_IN,OUTM,IOUT,JOUT,1)
            DO I=1,NOBC
              SALTOBC(I,K,N0)=OUTM(I,1)
            ENDDO
          ELSEIF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
            NDUM=0
            DO NS=1,NSELECT_PARENT
              IF(ABS(TEMP(IPARENT(NS),JPARENT(NS),K,N)).LE.90.0) THEN
                NDUM=NDUM+1
                XINP(NDUM)=LONSUB(IPARENT(NS),JPARENT(NS))
                YINP(NDUM)=LATSUB(IPARENT(NS),JPARENT(NS))
                ZINP(NDUM)=TEMP(IPARENT(NS),JPARENT(NS),K,N)
              ENDIF	
            ENDDO	

            IF(NDUM.LE.3) THEN
              WRITE(*,*) 'No parent data point at k=',K,DEPTH(K)
              WRITE(*,*) 'Extropolate upper level (k-1) to K'
              IF(K.GT.1) THEN 
                DO I=1,NOBC
                  TEMPOBC(I,K,N0)= TEMPOBC(I,K-1,N0)         
                ENDDO
              ELSE
                DO I=1,NOBC
                  TEMPOBC(I,K,N0)=-99999.9
                ENDDO
              ENDIF
            ELSE
              IF(NDUM.NE.NDATA) THEN
                WRITE(*,*) 'NDUM of TEMP= ',NDUM,NDATA
                WRITE(*,*) 'NDATA is not equal to NDUM in Temp!'
                NDATA=NDUM	
                IF(IGRD.EQ.1) THEN
                  CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &              LONOBC,LATOBC,ZOUT,WEIGHTNODES,WEIGHTS,0)
                ENDIF
              ENDIF   
 
              IF(IGRD.EQ.1) THEN
                CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &            LONOBC,LATOBC,ZOUT,WEIGHTNODES,WEIGHTS,1)
              ELSEIF(IGRD.EQ.4) THEN
                CALL INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &            NOBC,LONOBC,LATOBC,ZOUT)
              ENDIF
 
              DO I=1,NOBC
                TEMPOBC(I,K,N0)=ZOUT(I)
              ENDDO
            ENDIF
                
            NDUM=0 
            DO NS=1,NSELECT_PARENT
              IF(ABS(SALT(IPARENT(NS),JPARENT(NS),K,N)).LE.90.0) THEN
                NDUM=NDUM+1
                XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
                ZINP(ndum)=salt(IPARENT(NS),JPARENT(NS),K,N)
              ENDIF	
            ENDDO	

            IF(NDUM.LE.3) THEN
              WRITE(*,*) 'No parent data point at k=',K,DEPTH(K)
              WRITE(*,*) 'Extropolate upper level (k-1) to K'
              IF(K.GT.1) THEN
                DO I=1,NOBC
                  SALTOBC(I,K,N0)=SALTOBC(I,K-1,N0)
                ENDDO
              ELSE
                DO I=1,NOBC
                  SALTOBC(I,K,N0)=-99999.9
                ENDDO
              ENDIF
            ELSE
              IF(NDUM.NE.NDATA) THEN
                WRITE(*,*) 'NDUM of SALT= ',NDUM,NDATA
                WRITE(*,*) 'NDATA is not equal to NDUM in Salt!'
                NDATA=NDUM	
                IF(IGRD.EQ.1) THEN
                  CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &              LONOBC,LATOBC,ZOUT,WEIGHTNODES,WEIGHTS,0)
                ENDIF
              ENDIF   

              IF(IGRD.EQ.1) THEN
                CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &            LONOBC,LATOBC,ZOUT,WEIGHTNODES,WEIGHTS,1)
              ELSEIF(IGRD.EQ.4) THEN
                CALL INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &            NOBC,LONOBC,LATOBC,ZOUT)
              ENDIF

              DO I=1,NOBC
                SALTOBC(I,K,N0)=ZOUT(I)
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDDO

C------------------------------------------------------------------
C   Horizontally Interpolatiing U and V open boundary
C------------------------------------------------------------------
      IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
        CALL SEARCH_OUTPUT(ISUB,JSUB,LONSUB,LATSUB,NEOBC,LONEOBC,
     &    LATEOBC,NSELECT_PARENT,IPARENT,JPARENT)
        IF(ALLOCATED(XINP)) DEALLOCATE(XINP)
        IF(ALLOCATED(YINP)) DEALLOCATE(YINP)
        IF(ALLOCATED(ZINP)) DEALLOCATE(ZINP) 
        IF(ALLOCATED(XOUT)) DEALLOCATE(XOUT)
        IF(ALLOCATED(YOUT)) DEALLOCATE(YOUT)
        IF(ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
        IF(ALLOCATED(WEIGHTNODES)) DEALLOCATE(WEIGHTNODES)
        IF(ALLOCATED(WEIGHTS)) DEALLOCATE(WEIGHTS)
        ALLOCATE(XINP(NSELECT_PARENT))
        ALLOCATE(YINP(NSELECT_PARENT))
        ALLOCATE(ZINP(NSELECT_PARENT))  
        ALLOCATE(XOUT(NEOBC))
        ALLOCATE(YOUT(NEOBC))
        ALLOCATE(ZOUT(NEOBC))
        ALLOCATE(WEIGHTNODES(NEOBC,3))
        ALLOCATE(WEIGHTS(NEOBC,3))
      ELSEIF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
        IF(ALLOCATED(IOUT)) DEALLOCATE(IOUT)
        IF(ALLOCATED(JOUT)) DEALLOCATE(JOUT)
        IF(ALLOCATED(OUTM)) DEALLOCATE(OUTM)
	IF(ALLOCATED(LON_IN)) DEALLOCATE(LON_IN)
	IF(ALLOCATED(LAT_IN)) DEALLOCATE(LAT_IN)
        IF(ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
        ALLOCATE(IOUT(NEOBC,1))
        ALLOCATE(JOUT(NEOBC,1))
        ALLOCATE(OUTM(NEOBC,1))
        ALLOCATE(LON_IN(NEOBC,1))
        ALLOCATE(LAT_IN(NEOBC,1))
        ALLOCATE(TMP2D(ISUB,JSUB))

        DO I=1,NEOBC
          LON_IN(I,1)=LONEOBC(I)
          LAT_IN(I,1)=LATEOBC(I)
        ENDDO
        IF(IGRD.EQ.2) IFLAG=1
        IF(IGRD.EQ.3) IFLAG=0
        CALL INTERP_REGRID(IFLAG,ISUB,JSUB,LONSUB,LATSUB,TMP2D,
     &    NEOBC,1,LON_IN,LAT_IN,OUTM,IOUT,JOUT,0)
      ENDIF

      DO K=1,KMAX
        IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
          NDUM=0
          DO NS=1,NSELECT_PARENT
            IF(ABS(U(IPARENT(NS),JPARENT(NS),K,1)).LE.90.0) THEN
              NDUM=NDUM+1
              XINP(NDUM)=LONSUB(IPARENT(NS),JPARENT(NS))
              YINP(NDUM)=LATSUB(IPARENT(NS),JPARENT(NS))
              ZINP(NDUM)=U(IPARENT(NS),JPARENT(NS),K,1)
            ENDIF
          ENDDO
          NDATA=NDUM
C          WRITE(*,*) 'U Data number at vertical layer ',K,' is ',NDATA

          IF(IGRD.EQ.1) THEN
            WRITE(*,*) 'COMPUTE WEIGHTS AND NODES FOR REMESH !!!'
            CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NEOBC,
     &        LONEOBC,LATEOBC,ZOUT,WEIGHTNODES,WEIGHTS,0)
          ENDIF
        ENDIF

        DO N=NSTR,NEND
          N0=N-NSTR+1
          IF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
            DO I=1,ISUB
              DO J=1,JSUB
                TMP2D(I,J)=U(I,J,K,N)
              ENDDO
            ENDDO
            CALL INTERP_REGRID(IFLAG,ISUB,JSUB,LONSUB,LATSUB,TMP2D,
     &        NEOBC,1,LON_IN,LAT_IN,OUTM,IOUT,JOUT,1)
            DO I=1,NEOBC
              UEOBC(I,K,N0)=OUTM(I,1)
            ENDDO

            DO I=1,ISUB
              DO J=1,JSUB
                TMP2D(I,J)=V(I,J,K,N)
              ENDDO
            ENDDO
            CALL INTERP_REGRID(IFLAG,ISUB,JSUB,LONSUB,LATSUB,TMP2D,
     &        NEOBC,1,LON_IN,LAT_IN,OUTM,IOUT,JOUT,1)
            DO I=1,NEOBC
              VEOBC(I,K,N0)=OUTM(I,1)
            ENDDO
          ELSEIF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
            NDUM=0
            DO NS=1,NSELECT_PARENT
              IF(ABS(U(IPARENT(NS),JPARENT(NS),K,N)).LE.30.0) THEN
                NDUM=NDUM+1
                XINP(NDUM)=LONSUB(IPARENT(NS),JPARENT(NS))
                YINP(NDUM)=LATSUB(IPARENT(NS),JPARENT(NS))
                ZINP(NDUM)=U(IPARENT(NS),JPARENT(NS),K,N)
              ENDIF
            ENDDO

            IF(NDUM.LE.3) THEN
              WRITE(*,*) 'No parent data point at k=',K,DEPTH(K)
              WRITE(*,*) 'Extropolate upper level (k-1) to K'
              IF(K.GT.1) THEN
                DO I=1,NEOBC
                  UEOBC(I,K,N0)=UEOBC(I,K-1,N0)
                ENDDO
              ELSE
                DO I=1,NEOBC
                  UEOBC(I,K,N0)=-99999.9
                ENDDO
              ENDIF
            ELSE
              IF(NDUM.NE.NDATA) THEN
                WRITE(*,*) 'NDUM of U= ',NDUM,NDATA
                WRITE(*,*) 'NDATA is not equal to NDUM in U!'
                NDATA=NDUM
                IF(IGRD.EQ.1) THEN
                  CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NEOBC,
     &              LONEOBC,LATEOBC,ZOUT,WEIGHTNODES,WEIGHTS,0)
                ENDIF
              ENDIF

              IF(IGRD.EQ.1) THEN   
                CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NEOBC,
     &            LONEOBC,LATEOBC,ZOUT,WEIGHTNODES,WEIGHTS,1)
              ELSEIF(IGRD.EQ.4) THEN
                CALL INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &            NEOBC,LONEOBC,LATEOBC,ZOUT)
              ENDIF

              DO I=1,NEOBC
                UEOBC(I,K,N0)=ZOUT(I)
              ENDDO
            ENDIF

            NDUM=0
            DO NS=1,NSELECT_PARENT
              IF(ABS(V(IPARENT(NS),JPARENT(NS),K,N)).LE.30.0) THEN
                NDUM=NDUM+1
                XINP(NDUM)=LONSUB(IPARENT(NS),JPARENT(NS))
                YINP(NDUM)=LATSUB(IPARENT(NS),JPARENT(NS))
                ZINP(NDUM)=V(IPARENT(NS),JPARENT(NS),K,N)
              ENDIF
            ENDDO

            IF(NDUM.LE.3) THEN
              WRITE(*,*) 'No parent data point at k=',K,DEPTH(K)
              WRITE(*,*) 'Extropolate upper level (k-1) to K'
              IF(K.GT.1) THEN
                DO I=1,NEOBC
                  VEOBC(I,K,N0)=VEOBC(I,K-1,N0)
                ENDDO
              ELSE
                DO I=1,NEOBC
                  VEOBC(I,K,N0)=-99999.9
                ENDDO
              ENDIF
            ELSE
              IF(NDUM.NE.NDATA) THEN
                WRITE(*,*) 'NDUM of V= ',NDUM,NDATA
                WRITE(*,*) 'NDATA is not equal to NDUM in V!'
                NDATA=NDUM
                IF(IGRD.EQ.1) THEN
                  CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NEOBC,
     &              loneOBC,lateOBC,ZOUT,weightnodes,weights,0)
                ENDIF
              ENDIF

              IF(IGRD.EQ.1) THEN
                CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NEOBC,
     &            LONEOBC,LATEOBC,ZOUT,WEIGHTNODES,WEIGHTS,1)
              ELSEIF(IGRD.EQ.4) THEN
                CALL INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &            NEOBC,LONEOBC,LATEOBC,ZOUT)
              ENDIF

              DO I=1,NEOBC
                VEOBC(I,K,N0)=ZOUT(I)
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDDO

      DO I=1,NOBC
        DO N=1,NTMAX
          DO K=2,KMAX
            IF(ABS(TEMPOBC(I,K,N)).GE.90.0) THEN
              TEMPOBC(I,K,N)=TEMPOBC(I,K-1,N)
            ENDIF
            IF(ABS(SALTOBC(I,K,N)).GE.90.0) THEN
              SALTOBC(I,K,N)=SALTOBC(I,K-1,N)
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      DO I=1,NEOBC
        DO N=1,NTMAX
          DO K=2,KMAX
            IF(ABS(UEOBC(I,K,N)).GE.90.0) THEN
              UEOBC(I,K,N)=UEOBC(I,K-1,N)
            ENDIF
            IF(ABS(VEOBC(I,K,N)).GE.90.0) THEN
              VEOBC(I,K,N)=VEOBC(I,K-1,N)
            ENDIF
          ENDDO
        ENDDO
      ENDDO 

      CLOSE(45)
      OPEN(45,FILE='TS_NCOM.dat',STATUS='UNKNOWN')
      DO K=1,KMAX
        WRITE(45,'(I5,F9.3)') K,DEPTH(K)
      ENDDO
      TMPTXT=' '
      WRITE(TMPTXT,'(I5)') KMAX
      TMPTXT='(I5,'//trim(adjustl(TMPTXT))//'F10.2)'
      WRITE(*,*) 'FORMAT=',TRIM(TMPTXT)

      DO N=1,NTMAX
        WRITE(45,'(I5,2x,F9.3)') N,TIMEOBC(N)
        DO I=1,NOBC
          WRITE(45,trim(TMPTXT)) I,(tempOBC(I,K,N),K=1,KMAX)
          WRITE(45,trim(TMPTXT)) I,(saltOBC(I,K,N),K=1,KMAX)
        ENDDO
      ENDDO
      CLOSE(45)  

c------------------------------------------------------------------
C  Vertically interpolating WOA05 or NCOM z-coord. to model sigma layer
C  For FVCOM K=1 for surface and K=KBm for bottom
c------------------------------------------------------------------
      WRITE(*,*) 'Begin vertical interpolation'
      IF(ALLOCATED(ONED1)) DEALLOCATE(ONED1)
      IF(ALLOCATED(ONED2)) DEALLOCATE(ONED2)
      IF(ALLOCATED(ONED3)) DEALLOCATE(ONED3)
      IF(ALLOCATED(ONED4)) DEALLOCATE(ONED4)
      ALLOCATE(ONED1(KBM))
      ALLOCATE(ONED2(KMAX))
      ALLOCATE(ONED3(KMAX))
      ALLOCATE(ONED4(KMAX))
      IF(ALLOCATED(ZSIG_TMP)) DEALLOCATE(ZSIG_TMP)
      ALLOCATE(ZSIG_TMP(KBM))

      DO K=1,KMAX
        ONED2(K)=DEPTH(K)
      ENDDO	 
      DO N=1,NTMAX
        DO I=1,NOBC
          DO K=1,KBM
            SIGMA(K)=0.5*(SIGLEV(I,K)+SIGLEV(I,K+1))
            ZSIGMA(K)=SIGMA(K)*HOBC(I)
            IF(ZSIGMA(K).LT.0.0) ZSIGMA(K)=-ZSIGMA(K)
            ZSIG_TMP(K)=ZSIGMA(K)
          ENDDO

          DO K=1,KMAX
            IF(ABS(TEMPOBC(I,K,N)).GE.90.0.AND.K.GT.1) THEN
C              ONED3(K)=TEMPOBC(I,K-1,N)
              ONED3(K)=ONED3(K-1)
            ELSE
              ONED3(K)=TEMPOBC(I,K,N)
            ENDIF

            IF(ABS(SALTOBC(I,K,N)).GE.90.0.AND.K.GT.1) THEN
C              ONED4(K)=SALTOBC(I,K-1,N)
              ONED4(K)=ONED4(K-1)
            ELSE
              ONED4(K)=SALTOBC(I,K,N)
            ENDIF
          ENDDO	 

          CALL LINEARARRAY(KBM,ZSIG_TMP,ONED1,KMAX,ONED2,ONED3)
          DO K=1,KBM
            TEMPOBC_M(I,K,N)=ONED1(K)
          ENDDO
 
          CALL LINEARARRAY(KBM,ZSIG_TMP,ONED1,KMAX,oned2,ONED4)
          DO K=1,KBM
            SALTOBC_M(I,K,N)=ONED1(K)
          ENDDO
        ENDDO

        DO I=1,NEOBC
          DO K=1,KBM
            SIGMA(K)=0.5*(SIGLEV_ELE(I,K)+SIGLEV_ELE(I,K+1))
            ZSIGMA(K)=SIGMA(K)*HEOBC(I)
            IF(ZSIGMA(K).LT.0.0) ZSIGMA(K)=-ZSIGMA(K)
            ZSIG_TMP(K)=ZSIGMA(K)
          ENDDO

          DO K=1,KMAX
            IF(ABS(UEOBC(I,K,N)).GE.90.0.AND.K.GT.1) THEN
C              ONED3(K)=UEOBC(I,K-1,N)
              ONED3(K)=ONED3(K-1)
            ELSE
              ONED3(K)=UEOBC(I,K,N)
            ENDIF

            IF(ABS(VEOBC(I,K,N)).GE.90.0.AND.K.GT.1) THEN
C              ONED4(K)=VEOBC(I,K-1,N)
              ONED4(K)=ONED4(K-1)
            ELSE
              ONED4(K)=VEOBC(I,K,N)
            ENDIF
          ENDDO

          CALL LINEARARRAY(KBM,ZSIG_TMP,ONED1,KMAX,ONED2,ONED3)
          DO K=1,KBM
            UEOBC_M(I,K,N)=ONED1(K)
          ENDDO

          CALL LINEARARRAY(KBM,ZSIG_TMP,ONED1,KMAX,ONED2,ONED4)
          DO K=1,KBM
            VEOBC_M(I,K,N)=ONED1(K)
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE(ZSIG_TMP)
      WRITE(*,*) 'End vertical interpolation'

      CLOSE(45)
      OPEN(45,FILE='TS_NCOM_FVCOM.dat',STATUS='UNKNOWN')
      TMPTXT=' '
      WRITE(TMPTXT,'(I5)') KBM
      TMPTXT='(I5,'//trim(adjustl(TMPTXT))//'F10.2)'

      DO I=1,NOBC
        DO K=1,KBM
          SIGMA(K)=0.5*(SIGLEV(I,K)+SIGLEV(I,K+1))
          ZSIGMA(K)=SIGMA(K)*HOBC(I)
          IF(ZSIGMA(K).LT.0.0) ZSIGMA(K)=-ZSIGMA(K)
          WRITE(45,'(A4,I4,I3,f10.6,2f8.2)') 'I K=',I,K,SIGMA(K),
     &      ZSIGMA(K),HOBC(I)
        ENDDO

        DO N=1,NTMAX
          WRITE(45,'(I5,2X,F9.3)') N,TIMEOBC(N)
          WRITE(45,trim(TMPTXT)) I,(TEMPOBC_M(I,K,N),K=1,KBM)
          WRITE(45,trim(TMPTXT)) I,(SALTOBC_M(I,K,N),K=1,KBM)
        ENDDO
      ENDDO
      CLOSE(45)  
        
c------------------------------------------------------------------
C  End of vertical interpolation onto model vertical coordinate
C  Begin interpolating in time to DELT	
      NREC=NINT((JDAYE-JDAYS)*24/DELT)+1
      IF(ALLOCATED(TS_TIME)) DEALLOCATE(TS_TIME)
      ALLOCATE(TS_TIME(NTMAX))
      DO N=1,NTMAX
        TS_TIME(N)=TIMEOBC(N)
      ENDDO

      IF(ALLOCATED(ONED1)) DEALLOCATE(ONED1)
      IF(ALLOCATED(ONED2)) DEALLOCATE(ONED2)
      IF(ALLOCATED(ONED3)) DEALLOCATE(ONED3)
      IF(ALLOCATED(ONED4)) DEALLOCATE(ONED4)
      IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
      ALLOCATE(ONED1(NREC))
      ALLOCATE(ONED2(NREC))
      ALLOCATE(ONED3(NTMAX))
      ALLOCATE(ONED4(NTMAX))
      ALLOCATE(TMP4D(2,NOBC,KBM,NREC))

      DO I=1,NOBC
        DO K=1,KBM
          DO N=1,NTMAX
            ONED3(N)=TEMPOBC_M(I,K,N)
            ONED4(N)=SALTOBC_M(I,K,N)
          ENDDO
          CALL LINEARARRAY(NREC,TIME_M,ONED2,NTMAX,TS_TIME,ONED3)
          DO N=1,NREC
            TMP4D(1,I,K,N)=ONED2(N)
          ENDDO

          CALL LINEARARRAY(NREC,TIME_M,ONED2,NTMAX,TS_TIME,ONED4)
          DO N=1,NREC
            TMP4D(2,I,K,N)=ONED2(N)
          ENDDO
        ENDDO
      ENDDO

      IF(ALLOCATED(TEMPOBC_M)) DEALLOCATE(TEMPOBC_M)
      IF(ALLOCATED(SALTOBC_M)) DEALLOCATE(SALTOBC_M)
      ALLOCATE(TEMPOBC_M(NOBC,KBM,NREC))
      ALLOCATE(SALTOBC_M(NOBC,KBM,NREC))

      DO N=1,NREC
        DO I=1,NOBC
          DO K=1,KBM
            TEMPOBC_M(I,K,N)=TMP4D(1,I,K,N)
            SALTOBC_M(I,K,N)=TMP4D(2,I,K,N)
          ENDDO
        ENDDO
      ENDDO

      IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
      ALLOCATE(TMP4D(2,NEOBC,KBM,NREC))
      DO I=1,NEOBC
        DO K=1,KBM
          DO N=1,NTMAX
            ONED3(N)=UEOBC_M(I,K,N)
            ONED4(N)=VEOBC_M(I,K,N)
          ENDDO
          CALL LINEARARRAY(NREC,TIME_M,ONED2,NTMAX,TS_TIME,ONED3)
          DO N=1,NREC
            TMP4D(1,I,K,N)=ONED2(N)
          ENDDO

          CALL LINEARARRAY(NREC,TIME_M,ONED2,NTMAX,TS_TIME,ONED4)
          DO N=1,NREC
            TMP4D(2,I,K,N)=ONED2(N)
          ENDDO
        ENDDO
      ENDDO

      IF(ALLOCATED(UEOBC_M)) DEALLOCATE(UEOBC_M)
      IF(ALLOCATED(VEOBC_M)) DEALLOCATE(VEOBC_M)
      IF(ALLOCATED(UBAROBC)) DEALLOCATE(UBAROBC)
      IF(ALLOCATED(VBAROBC)) DEALLOCATE(VBAROBC)
      IF(ALLOCATED(TS_TIME)) DEALLOCATE(TS_TIME)
      ALLOCATE(UEOBC_M(NEOBC,KBM,NREC))
      ALLOCATE(VEOBC_M(NEOBC,KBM,NREC))
      ALLOCATE(UBAROBC(NEOBC,NREC))
      ALLOCATE(VBAROBC(NEOBC,NREC))
      ALLOCATE(TS_TIME(NREC))

      DO N=1,NREC
        TS_TIME(N)=TIME_M(N)
        DO I=1,NEOBC
          DO K=1,KBM
            UEOBC_M(I,K,N)=TMP4D(1,I,K,N) 
            VEOBC_M(I,K,N)=TMP4D(2,I,K,N) 
          ENDDO
        ENDDO
      ENDDO
      NTMAX=NREC

C   Computer ubar, and vbar at the center of elements for FVCOM
      IF(TRIM(OCEAN_MODEL).EQ.'FVCOM') THEN
        DO N=1,NTMAX
          DO I=1,NEOBC
            AVGU=0.0
            AVGV=0.0
            DO K=1,KBM
              AVGU=AVGU+UEOBC_M(I,K,N)
              AVGV=AVGV+VEOBC_M(I,K,N)
            ENDDO
            UBAROBC(I,N)=AVGU/KBM
            VBAROBC(I,N)=AVGV/KBM
          ENDDO
        ENDDO
      ENDIF	

C------------------------------------------------------------------
C   Deallocate unused arrays
c------------------------------------------------------------------
      IF(ALLOCATED(LON)) DEALLOCATE(LON)
      IF(ALLOCATED(LAT)) DEALLOCATE(LAT)
      IF(ALLOCATED(LONSUB)) DEALLOCATE(LONSUB)
      IF(ALLOCATED(LATSUB)) DEALLOCATE(LATSUB)
      IF(ALLOCATED(MASKSUB)) DEALLOCATE(MASKSUB)
      IF(ALLOCATED(DEPTH)) DEALLOCATE(DEPTH)
      IF(ALLOCATED(TEMP)) DEALLOCATE(TEMP)
      IF(ALLOCATED(SALT)) DEALLOCATE(SALT)
      IF(ALLOCATED(TMP1D)) DEALLOCATE(TMP1D)
      IF(ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
      IF(ALLOCATED(TMP3D)) DEALLOCATE(TMP3D)
      IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
      IF(ALLOCATED(ONED1)) DEALLOCATE(ONED1)
      IF(ALLOCATED(ONED2)) DEALLOCATE(ONED2)
      IF(ALLOCATED(ONED3)) DEALLOCATE(ONED3)
      IF(ALLOCATED(ONED4)) DEALLOCATE(ONED4)

c------------------------------------------------------------------
C   Processing subtidal Water Level open boundary from RTOFS
c------------------------------------------------------------------      
      IF(TRIM(DBASE_WL).EQ.'RTOFS') THEN
        IFILE_NCOM=0
        NCOM_FILE='RTOFS_FILE_WL'
        INQUIRE(FILE=TRIM(NCOM_FILE),EXIST=FEXIST)
        IF(FEXIST) THEN
          CLOSE(60)
          OPEN(60,FILE=TRIM(NCOM_FILE),STATUS='OLD')
          DO I=1,999
            READ(60,'(a200)',END=3335) BUFFER
            FILE_NCOM(I)=trim(adjustL(BUFFER))
          ENDDO
3335      CLOSE(60)
          IFILE_NCOM=I-1
        ENDIF
        WRITE(*,*) 'Total number of RTOFS WL FILES= ',IFILE_NCOM

        IF(IFILE_NCOM.EQ.0) THEN
          WRITE(*,*) 'No RTOFS WL file is available in this period'
          DBASE_TS='ETSS'
          WRITE(ICORMS,'(a)') 'RTOFS FILE IS NOT FOUND' 
          WRITE(ICORMS,'(a)') 'USE ETSS BACKUP' 
        ENDIF  

        DO I=1,IFILE_NCOM
          WRITE(*,*) 'RTOFS I=',I,TRIM(FILE_NCOM(I))
        ENDDO

C-----------------------------------------------------------------------
C   Read all RTOFS WL netCDF files available from start to end time
C-----------------------------------------------------------------------
        FIN=TRIM(FILE_NCOM(1))
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        VNAME='ssh'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IM=DIMS(1)
        JM=DIMS(2)
        NT=DIMS(3)
C        WRITE(*,*) 'RTOFS IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
        IF(ALLOCATED(LON)) DEALLOCATE(LON)
        IF(ALLOCATED(LAT)) DEALLOCATE(LAT)
        IF(ALLOCATED(ZETA_TIME)) DEALLOCATE(ZETA_TIME)
        ALLOCATE(LON(IM,JM))
        ALLOCATE(LAT(IM,JM))
        ALLOCATE(ZETA_TIME(NMAX))

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='Longitude'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
C        WRITE(*,*) 'DIM Number of Longitude ',NDIM
        IF(NDIM.EQ.2) THEN
          DO I=1,IM
            DO J=1,JM
              LON(I,J)=TMP4D(I,J,1,1)
              IF(LON(I,J).GT.180.0) LON(I,J)=LON(I,J)-360.0
            ENDDO
          ENDDO
        ELSEIF(NDIM.EQ.1) THEN
          DO I=1,IM
            DO J=1,JM
              LON(I,J)=TMP4D(I,1,1,1)
              IF(LON(I,J).GT.180.0) LON(I,J)=LON(I,J)-360.0
            ENDDO
          ENDDO
        ENDIF  

        VNAME='Latitude'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        IF(NDIM.EQ.2) THEN
          DO I=1,IM
            DO J=1,JM
              LAT(I,J)=TMP4D(I,J,1,1)
            ENDDO
          ENDDO
        ELSEIF(NDIM.EQ.1) THEN
          DO I=1,IM
            DO J=1,JM
              LAT(I,J)=TMP4D(J,1,1,1)
            ENDDO
          ENDDO
        ENDIF

C-----------------------------------------------------------------------
C   Select subdomain I and J index        
C-----------------------------------------------------------------------
        IMIN=99999
        IMAX=-9999
        JMIN=99999
        JMAX=-9999
        DO I=1,IM
          DO J=1,JM
            IF(LON(I,J).GE.MINLON.AND.LON(I,J).LE.MAXLON.AND.
     &         LAT(I,J).GE.MINLAT.AND.LAT(I,J).LE.MAXLAT) THEN
              IF(I.LT.IMIN) IMIN=I 	  
              IF(I.GT.IMAX) IMAX=I 	  
              IF(J.LT.JMIN) JMIN=J 	  
              IF(J.GT.JMAX) JMAX=J
            ENDIF
          ENDDO
        ENDDO	  	  
        ISUB=IMAX-IMIN+1	 
        JSUB=JMAX-JMIN+1	 
        WRITE(*,*) 'Subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX

C-----------------------------------------------------------------------
C   Allocate sizes of arrays for NCOM products
C-----------------------------------------------------------------------
        NNSUB=ISUB*JSUB
        IF(ALLOCATED(LONSUB)) DEALLOCATE(LONSUB)
        IF(ALLOCATED(LATSUB)) DEALLOCATE(LATSUB)
        IF(ALLOCATED(WL)) DEALLOCATE(WL)
        IF(ALLOCATED(UBAR_RTOFS)) DEALLOCATE(UBAR_RTOFS)
        IF(ALLOCATED(VBAR_RTOFS)) DEALLOCATE(VBAR_RTOFS)
        ALLOCATE(LONSUB(ISUB,JSUB))
        ALLOCATE(LATSUB(ISUB,JSUB))
        ALLOCATE(WL(ISUB,JSUB,NT*IFILE_NCOM+10))
        ALLOCATE(UBAR_RTOFS(ISUB,JSUB,NT*IFILE_NCOM+10))
        ALLOCATE(VBAR_RTOFS(ISUB,JSUB,NT*IFILE_NCOM+10))

        DO N=1,NT*IFILE_NCOM+10
          DO J=1,JSUB
            DO I=1,ISUB
              WL(I,J,N)=0.0
              UBAR_RTOFS(I,J,N)=0.0
              VBAR_RTOFS(I,J,N)=0.0
            ENDDO
          ENDDO
        ENDDO   

        CLOSE(77)
        DO J=1,JSUB
          DO I=1,ISUB
            LONSUB(I,J)=LON(IMIN+I-1,JMIN+J-1)
            LATSUB(I,J)=LAT(IMIN+I-1,JMIN+J-1)
            WRITE(77,'(2F12.4,2I6)') LONSUB(I,J),LATSUB(I,J),I,J
          ENDDO
        ENDDO 
        CLOSE(77)  

        ICOUNT=0
        NREC=0
        TIMELAST=-99999.9	 
        DO IFILE=1,IFILE_NCOM
          WRITE(*,*) 'Reading RTOFS NetCDF file...',IFILE
          FIN=TRIM(FILE_NCOM(IFILE))
          VNAME='MT'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
          IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
          ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          NT=DIMS(1)
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
          ANAME=trim(adjustL(ANAME))
          LEN=LEN_TRIM(ANAME)
          LL=INDEX(ANAME,'minute')         
          IF(LL.GT.0) SCALE_TIME=1.0/1440.0
          LL=INDEX(ANAME,'hour')         
          IF(LL.GT.0) SCALE_TIME=1.0/24.0
          LL=INDEX(ANAME,'day')         
          IF(LL.GT.0) SCALE_TIME=1.0
          LL=INDEX(ANAME,'since')         
          IF(LL.GT.0) THEN
            READ(ANAME(LL+6:LEN),80) IYR,IMM,IDD,IHH
            IF(IFILE.EQ.1) THEN
              WRITE(*,*) 'Basetime=',IYR,IMM,IDD,IHH
            END IF
          ELSE
            WRITE(*,*) 'There is error while reading base date'
            STOP
          ENDIF

          YEARB=IYR
          MONTHB=IMM
          DAYB=IDD
          HOURB=IHH
          JDAY=JULIAN(YEARB,MONTHB,DAYB,HOURB)
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
          TIME_RTOFS=TMP4D(1,1,1,1)*SCALE_TIME+JDAY-JBASE_DATE
          IF(TIMELAST.LT.TIME_RTOFS )THEN
            ICOUNT=ICOUNT+1
            ZETA_TIME(ICOUNT)=TIME_RTOFS
            TIMELAST=TIME_RTOFS 
          ENDIF

          VNAME='ssh'
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
          IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
          ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
          DO N=1,NT
            DO J=1,JSUB
              DO I=1,ISUB
                IJJ=IMIN+I-1
                JJJ=JMIN+J-1
                WL(I,J,ICOUNT)=TMP4D(IJJ,JJJ,N,1)
                IF(ABS(WL(I,J,ICOUNT)).GE.99.0) THEN
                  WL(I,J,ICOUNT)=-99999.9
                ELSE
                  WL(I,J,ICOUNT)=WL(I,J,ICOUNT)+0.25
CC  Based on developer, nontidal WL is about 25 cm lower.
                ENDIF  
              ENDDO
            ENDDO   
          ENDDO   

          VNAME='u_barotropic_velocity'
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
          IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
          ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
          DO N=1,NT
            DO J=1,JSUB
              DO I=1,ISUB
                IJJ=IMIN+I-1
                JJJ=JMIN+J-1
                UBAR_RTOFS(I,J,ICOUNT)=TMP4D(IJJ,JJJ,N,1)
                IF(ABS(UBAR_RTOFS(I,J,ICOUNT)).GE.99.0) THEN
                  UBAR_RTOFS(I,J,ICOUNT)=-99999.9
                ENDIF  
              ENDDO
            ENDDO   
          ENDDO 

          VNAME='v_barotropic_velocity'
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
          IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
          ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
          DO N=1,NT
            DO J=1,JSUB
              DO I=1,ISUB
                IJJ=IMIN+I-1
                JJJ=JMIN+J-1
                VBAR_RTOFS(I,J,ICOUNT)=TMP4D(IJJ,JJJ,N,1)
                IF(ABS(VBAR_RTOFS(I,J,ICOUNT)).GE.99.0) THEN
                  VBAR_RTOFS(I,J,ICOUNT)=-99999.9
                ENDIF  
              ENDDO
            ENDDO   
          ENDDO 

          NREC=ICOUNT
          STATUS=NF_CLOSE(NCID)
        ENDDO      
        NT=NREC 
        WRITE(*,*) 'Number of RTOFS WL data NREC= ',NREC

C-----------------------------------------------------------------------
C   End of reading native RTOFS WL products
C-----------------------------------------------------------------------
        NSTR=1
        N=1
        DO WHILE ((ZETA_TIME(N).LT.DAY_START).AND.(N.LE.NT))
          N=N+1
        ENDDO
        NSTR=N-1

        IF(NSTR.EQ.0.OR.NSTR.GT.NT) THEN
          DO NN=NT,1,-1
            ZETA_TIME(NN+1)=ZETA_TIME(NN)
            DO I=1,ISUB
              DO J=1,JSUB
	        WL(I,J,NN+1)=WL(I,J,NN) 
              ENDDO	
            ENDDO
          ENDDO
          ZETA_TIME(1)=DAY_START
          NT=NT+1
          NSTR=1
        ENDIF

        IF(ZETA_TIME(NT).LT.day_end) THEN
          DIFF=DAY_END-ZETA_TIME(NT)
          IF(DIFF.LT.5.0) THEN 
C   Allow persistence for the maximum 5 days. 
            ZETA_TIME(NT+1)=DAY_END
            DO I=1,ISUB
              DO J=1,JSUB
	        WL(I,J,NT+1)=WL(I,J,NT) 
              ENDDO	
            ENDDO
            NT=NT+1
          ELSE  
            WRITE(*,*) 'RTOFS WL data is too shorter than day_end'
            WRITE(*,*) 'The time period is not covered by RTOFS'	  
            TMPTXT=' '
            TMPTXT=TRIM(DBASE_WL)//' is too short'
            WRITE(ICORMS,'(a)') trim(TMPTXT)
            TMPTXT=' '
            TMPTXT='USE ETSS BACKUP'
            WRITE(ICORMS,'(a)') trim(TMPTXT)
            DBASE_WL='ETSS'
          ENDIF  
        ENDIF    

        N=1
        DO WHILE ((ZETA_TIME(N).LT.DAY_END).AND.(N.LT.NT))
          N=N+1
        ENDDO
        NEND=N

        IF(N.GT.NT) THEN
  	  WRITE(*,*) 'Time period is not covered by dataset'	  
          WRITE(ICORMS,'(a)') 'Time period is not covered'
          WRITE(ICORMS,'(a)') 'Generation of WL OBC failed'
          WRITE(ICORMS,'(a)') 'Stop in nos_ofs_create_OBC.f'
          STOP
        ENDIF  	  
        WRITE(*,*) 'Start and end times of RTOFS WL= ',
     &   ZETA_TIME(NSTR),ZETA_TIME(NEND),NSTR,NEND,DAY_START,DAY_END 		
        NTMAX_WL=NEND-NSTR+1
        IF(ALLOCATED(TIMEOBC)) DEALLOCATE(TIMEOBC)
        ALLOCATE(TIMEOBC(NTMAX_WL))
        DO N=1,NTMAX_WL
          TIMEOBC(N)=ZETA_TIME(NSTR+N-1)
        ENDDO
        IF(ALLOCATED(ZETA_TIME)) DEALLOCATE(ZETA_TIME)
        ALLOCATE(ZETA_TIME(NTMAX_WL))
        DO N=1,NTMAX_WL
          ZETA_TIME(N)=TIMEOBC(N)
        ENDDO

c------------------------------------------------------------------
C   Spatial interpolation for Water Level open boundary
c------------------------------------------------------------------
        IF(ALLOCATED(WLOBC)) DEALLOCATE(WLOBC)
        ALLOCATE(WLOBC(NOBC,NTMAX_WL))
        IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
          IF(ALLOCATED(XINP)) DEALLOCATE(XINP)
          IF(ALLOCATED(YINP)) DEALLOCATE(YINP)
          IF(ALLOCATED(ZINP)) DEALLOCATE(ZINP)
          IF(ALLOCATED(XOUT)) DEALLOCATE(XOUT)
          IF(ALLOCATED(YOUT)) DEALLOCATE(YOUT)
          IF(ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
          IF(ALLOCATED(WEIGHTNODES)) DEALLOCATE(WEIGHTNODES)
          IF(ALLOCATED(WEIGHTS)) DEALLOCATE(WEIGHTS)
          ALLOCATE(XINP(NNSUB))
          ALLOCATE(YINP(NNSUB))
          ALLOCATE(ZINP(NNSUB))
          ALLOCATE(XOUT(NOBC))
          ALLOCATE(YOUT(NOBC))
          ALLOCATE(ZOUT(NOBC))
          ALLOCATE(WEIGHTNODES(NOBC,3))
          ALLOCATE(WEIGHTS(NOBC,3))

          CALL SEARCH_OUTPUT(ISUB,JSUB,LONSUB,LATSUB,NOBC,LONOBC,
     &      LATOBC,NSELECT_PARENT,IPARENT,JPARENT)
        ENDIF

        IF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
          IF(ALLOCATED(IOUT)) DEALLOCATE(IOUT)
          IF(ALLOCATED(JOUT)) DEALLOCATE(JOUT)
          IF(ALLOCATED(OUTM)) DEALLOCATE(OUTM)
          IF(ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
          IF(ALLOCATED(LON_IN)) DEALLOCATE(LON_IN)
          IF(ALLOCATED(LAT_IN)) DEALLOCATE(LAT_IN)
          ALLOCATE(IOUT(NOBC,1))
          ALLOCATE(JOUT(NOBC,1))
          ALLOCATE(OUTM(NOBC,1))
          ALLOCATE(LON_IN(NOBC,1))
          ALLOCATE(LAT_IN(NOBC,1))
          ALLOCATE(TMP2D(ISUB,JSUB))

          DO I=1,NOBC
            LON_IN(I,1)=LONOBC(I)
            LAT_IN(I,1)=LATOBC(I)
          ENDDO
          IF(IGRD.EQ.2) IFLAG=1
          IF(IGRD.EQ.3) IFLAG=0
          CALL INTERP_REGRID(IFLAG,ISUB,JSUB,LONSUB,LATSUB,TMP2D,
     &      NOBC,1,LON_IN,LAT_IN,OUTM,IOUT,JOUT,0)
        ENDIF

        DO N=NSTR,NEND
          N0=N-NSTR+1
          IF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
            DO I=1,ISUB
              DO J=1,JSUB
                TMP2D(I,J)=WL(I,J,N)
              ENDDO
            ENDDO
            IF(IGRD.EQ.2) IFLAG=1
            IF(IGRD.EQ.3) IFLAG=0
            CALL INTERP_REGRID(IFLAG,ISUB,JSUB,LONSUB,LATSUB,TMP2D,
     &        NOBC,1,LON_IN,LAT_IN,OUTM,IOUT,JOUT,1)
            DO I=1,NOBC
              WLOBC(I,N0)=OUTM(I,1)
            ENDDO
          ELSEIF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
            NDUM=0
            DO NS=1,NSELECT_PARENT
              IF(ABS(WL(IPARENT(NS),JPARENT(NS),N)).LE.90.0) THEN
                NDUM=NDUM+1
                XINP(NDUM)=LONSUB(IPARENT(NS),JPARENT(NS))
                YINP(NDUM)=LATSUB(IPARENT(NS),JPARENT(NS))
                ZINP(NDUM)=WL(IPARENT(NS),JPARENT(NS),N)
              ENDIF	
            ENDDO	

            NDATA=NDUM
            IF(IGRD.EQ.1) THEN
              CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &          NOBC,LONOBC,LATOBC,ZOUT,WEIGHTNODES,WEIGHTS,0)
              CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &          NOBC,LONOBC,LATOBC,ZOUT,WEIGHTNODES,WEIGHTS,1)
    	    ELSEIF(IGRD.EQ.4) THEN
              CALL INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &          NOBC,lonOBC,latOBC,ZOUT)
            ENDIF
            DO I=1,NOBC
              WLOBC(I,N0)=ZOUT(I)
            ENDDO   
          ENDIF
        ENDDO

c------------------------------------------------------------------
C   Spatial interpolation for ubar and vbar of RTOFS onto open boundary
c------------------------------------------------------------------
        IF(ALLOCATED(UBAROBC)) DEALLOCATE(UBAROBC)
        IF(ALLOCATED(VBAROBC)) DEALLOCATE(VBAROBC)
        ALLOCATE(UBAROBC(NEOBC,NTMAX_WL))
        ALLOCATE(VBAROBC(NEOBC,NTMAX_WL))

        IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
          CALL SEARCH_OUTPUT(ISUB,JSUB,LONSUB,LATSUB,NOBC,LONOBC,
     &      LATOBC,NSELECT_PARENT,IPARENT,JPARENT)
 
          IF(ALLOCATED(XINP)) DEALLOCATE(XINP)
          IF(ALLOCATED(YINP)) DEALLOCATE(YINP)
          IF(ALLOCATED(ZINP)) DEALLOCATE(ZINP)
          IF(ALLOCATED(XOUT)) DEALLOCATE(XOUT)
          IF(ALLOCATED(YOUT)) DEALLOCATE(YOUT)
          IF(ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
          IF(ALLOCATED(WEIGHTNODES)) DEALLOCATE(WEIGHTNODES)
          IF(ALLOCATED(WEIGHTS)) DEALLOCATE(WEIGHTS)
          ALLOCATE(XINP(NSELECT_PARENT))
          ALLOCATE(YINP(NSELECT_PARENT))
          ALLOCATE(ZINP(NSELECT_PARENT))
          ALLOCATE(XOUT(NEOBC))
          ALLOCATE(YOUT(NEOBC))
          ALLOCATE(ZOUT(NEOBC))
          ALLOCATE(WEIGHTNODES(NEOBC,3))
          ALLOCATE(WEIGHTS(NEOBC,3))
        ENDIF

        IF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
          IF(ALLOCATED(IOUT)) DEALLOCATE(IOUT)
          IF(ALLOCATED(JOUT)) DEALLOCATE(JOUT)
          IF(ALLOCATED(OUTM)) DEALLOCATE(OUTM)
          IF(ALLOCATED(LON_IN)) DEALLOCATE(LON_IN)
          IF(ALLOCATED(LAT_IN)) DEALLOCATE(LAT_IN)
          IF(ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
          ALLOCATE(IOUT(NEOBC,1))
          ALLOCATE(JOUT(NEOBC,1))
          ALLOCATE(OUTM(NEOBC,1))
          ALLOCATE(LON_IN(NEOBC,1))
          ALLOCATE(LAT_IN(NEOBC,1))
          ALLOCATE(TMP2D(ISUB,JSUB))

          DO I=1,NEOBC
            LON_IN(I,1)=LONEOBC(I)
            LAT_IN(I,1)=LATEOBC(I)
          ENDDO
          IF(IGRD.EQ.2) IFLAG=1
          IF(IGRD.EQ.3) IFLAG=0
          CALL INTERP_REGRID(IFLAG,ISUB,JSUB,LONSUB,LATSUB,TMP2D,
     &      NEOBC,1,LON_IN,LAT_IN,OUTM,IOUT,JOUT,0)
        ENDIF

        DO N=NSTR,NEND
          N0=N-NSTR+1
          IF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
            DO I=1,ISUB
              DO J=1,JSUB
                TMP2D(I,J)=UBAR_RTOFS(I,J,N)
              ENDDO
            ENDDO
            CALL INTERP_REGRID(IFLAG,ISUB,JSUB,LONSUB,LATSUB,TMP2D,
     &        NEOBC,1,LON_IN,LAT_IN,OUTM,IOUT,JOUT,1)
            DO I=1,NEOBC
              UBAROBC(I,N0)=OUTM(I,1)
            ENDDO
 
            DO I=1,ISUB
              DO J=1,JSUB
                TMP2D(I,J)=VBAR_RTOFS(I,J,N)
              ENDDO
            ENDDO
            CALL INTERP_REGRID(IFLAG,ISUB,JSUB,LONSUB,LATSUB,TMP2D,
     &        NEOBC,1,LON_IN,LAT_IN,OUTM,IOUT,JOUT,1)
            DO I=1,NEOBC
              VBAROBC(I,N0)=OUTM(I,1)
            ENDDO
          ELSEIF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
            NDUM=0
            DO NS=1,NSELECT_PARENT
              ITMPP=IPARENT(NS)
              JTMPP=JPARENT(NS)
              IF(ABS(UBAR_RTOFS(ITMPP,JTMPP,N)).LE.90.0) THEN
                NDUM=NDUM+1
                XINP(NDUM)=LONSUB(ITMPP,JTMPP)
                YINP(NDUM)=LATSUB(ITMPP,JTMPP)
                ZINP(NDUM)=UBAR_RTOFS(ITMPP,JTMPP,N)
              ENDIF
            ENDDO
            NDATA=NDUM

            IF(IGRD.EQ.1) THEN
              CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NEOBC,
     *          LONEOBC,LATEOBC,ZOUT,WEIGHTNODES,WEIGHTS,0)
              CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NEOBC,
     *          LONEOBC,LATEOBC,ZOUT,WEIGHTNODES,WEIGHTS,1)
            ELSEIF(IGRD.EQ.4) THEN
              CALL INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     *          NEOBC,LONEOBC,LATEOBC,ZOUT)
            ENDIF

            DO I=1,NEOBC
              UBAROBC(I,N0)=ZOUT(I)
            ENDDO

            NDUM=0
            DO NS=1,NSELECT_PARENT
              ITMPP=IPARENT(NS)
              JTMPP=JPARENT(NS)
              IF(ABS(VBAR_RTOFS(ITMPP,JTMPP,N)).LE.90.0) THEN
                NDUM=NDUM+1
                XINP(NDUM)=LONSUB(ITMPP,JTMPP)
                YINP(NDUM)=LATSUB(ITMPP,JTMPP)
                ZINP(NDUM)=VBAR_RTOFS(ITMPP,JTMPP,N)
              ENDIF
            ENDDO
            NDATA=NDUM

            IF(IGRD.EQ.1) THEN
              CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NEOBC,
     *          LONEOBC,LATEOBC,ZOUT,WEIGHTNODES,WEIGHTS,0)
              CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NEOBC,
     *          LONEOBC,LATEOBC,ZOUT,WEIGHTNODES,WEIGHTS,1)
            ELSEIF(IGRD.EQ.4) THEN
              CALL INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     *          NEOBC,LONEOBC,LATEOBC,ZOUT)
            ENDIF

            DO I=1,NEOBC
              VBAROBC(I,N0)=ZOUT(I)
            ENDDO
          ENDIF
        ENDDO

C   Begin interpolating in time to DELT for UBAR and VBAR
        NREC=NINT((JDAYE-JDAYS)*24/DELT)+1
        IF(ALLOCATED(ONED1)) DEALLOCATE(ONED1)
        IF(ALLOCATED(ONED2)) DEALLOCATE(ONED2)
        IF(ALLOCATED(ONED3)) DEALLOCATE(ONED3)
        IF(ALLOCATED(ONED4)) DEALLOCATE(ONED4)
        IF(ALLOCATED(TMP3D)) DEALLOCATE(TMP3D)      
        ALLOCATE(ONED1(NREC))
        ALLOCATE(ONED2(NREC))
        ALLOCATE(ONED3(NTMAX_WL))
        ALLOCATE(ONED4(NTMAX_WL))
        ALLOCATE(TMP3D(NEOBC,NREC,2))         

        DO I=1,NEOBC
          DO N=1,NTMAX_WL
            ONED3(N)=UBAROBC(I,N)
            ONED4(N)=VBAROBC(I,N)
          ENDDO
          CALL LINEARARRAY(NREC,TIME_M,ONED2,NTMAX_WL,ZETA_TIME,ONED3)
          DO N=1,NREC
            TMP3D(I,N,1)=ONED2(N)
          ENDDO
          CALL LINEARARRAY(NREC,TIME_M,ONED2,NTMAX_WL,ZETA_TIME,ONED4)
          DO N=1,NREC
            TMP3D(I,N,2)=ONED2(N)
          ENDDO
        ENDDO    

        IF(ALLOCATED(UBAROBC)) DEALLOCATE(UBAROBC)
        IF(ALLOCATED(VBAROBC)) DEALLOCATE(VBAROBC)
        ALLOCATE(UBAROBC(NEOBC,NREC))
        ALLOCATE(VBAROBC(NEOBC,NREC))
        DO I=1,NEOBC
          DO N=1,NREC
            UBAROBC(I,N)=TMP3D(I,N,1)
            VBAROBC(I,N)=TMP3D(I,N,2)
          ENDDO
        ENDDO
      ENDIF

c------------------------------------------------------------------
C   Generate WL OBC from ETSS gridded products if DBASE_WL=ETSS
C   Read in ETSS from a ASCII file generated using wgrib2 utility
c------------------------------------------------------------------    
      IF(TRIM(DBASE_WL).EQ.'ETSS') THEN
        WRITE(*,*) 'GENERATE SUBTIDAL WL OBC FROM ETSS'      
        CLOSE(1)
        OPEN(1,FILE=TRIM(ETSSFILE),STATUS='OLD')
        NT=0
        NETSS=0
        IREC=0
        DO
2000    READ(1,'(A200)',END=2002) BUFFER
        LEN1=LEN_TRIM(TRIM(BUFFER)) 
        IF(LEN1.GT.0) THEN
          LL=INDEX(BUFFER,'ETSRG',BACK=.TRUE.)         
          IF(LL.GT.0) THEN
            NT=NT+1
            IF(IREC.GT.NETSS) THEN
              NETSS=IREC
              NMAX0=NT
            ENDIF
            IF(NT.GT.1) ITMP1D(NT-1)=IREC
            IREC=0
          ELSE
            IREC=IREC+1  
          ENDIF
        ENDIF
        ENDDO
2002    CONTINUE

        IF(IREC.GT.NETSS) THEN
          NETSS=IREC
          NMAX0=NT
        ENDIF
        IF(NT.GT.1) ITMP1D(NT)=IREC
        REWIND(1)
        WRITE(*,*) 'ETSS SUB DIMS NETSS=',NETSS,'  NT=',NT

        IF(ALLOCATED(LON_ETSS)) DEALLOCATE(LON_ETSS)
        IF(ALLOCATED(LAT_ETSS)) DEALLOCATE(LAT_ETSS)
        IF(ALLOCATED(WL_ETSS)) DEALLOCATE(WL_ETSS)
        IF(ALLOCATED(TIMEOBC)) DEALLOCATE(TIMEOBC)
        IF(ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
        ALLOCATE(LON_ETSS(NETSS,NT))
        ALLOCATE(LAT_ETSS(NETSS,NT))
        ALLOCATE(WL_ETSS(NETSS,NT))
        ALLOCATE(TIMEOBC(NT))

        DO N=1,NT
          DO I=1,NETSS
            WL_ETSS(I,N)=9999.0
          ENDDO
        ENDDO

        NREC=0
        TIMEOBC(1)=-99999.0
        DO N=1,NT
          READ(1,'(A200)') BUFFER
          BUFFER=TRIM(ADJUSTL(BUFFER))
C          WRITE(*,*) TRIM(BUFFER)
          LEN1=LEN_TRIM(trim(BUFFER))
          LL=INDEX(BUFFER,'=',BACK=.TRUE.)
          IF(BUFFER(LEN1-2:LEN1).EQ.'anl') THEN
            READ(BUFFER(LL+1:LEN1),'(I4,3I2)') IYR,IMM,IDD,IHH
          ELSEIF(BUFFER(LEN1-3:LEN1).EQ.'fcst') THEN 
            READ(BUFFER(LL+1:LEN1),'(I4,3I2,1x,I2)') IYR,IMM,
     &	      IDD,ICYC,IHH0
            IHH=ICYC+IHH0
          ENDIF
C          WRITE(*,*) 'ETSS forecast time= ',IYR,IMM,IDD,IHH,ICYC,IHH0

          YEARB=IYR
          MONTHB=IMM
          DAYB=IDD
          HOURB=IHH
          JDAY=JULIAN(YEARB,MONTHB,DAYB,HOURB)
          DAY0=JDAY-JBASE_DATE
          DO K=1,NREC
            IF(TIMEOBC(K).GE.DAY0) THEN
              NREC=K-1
              EXIT
            ENDIF  
          ENDDO  	 
500       CONTINUE
          NREC=NREC+1
          TIMEOBC(NREC)=DAY0
          DO I=1,ITMP1D(N)
            READ(1,'(A200)') BUFFER
            BUFFER=TRIM(ADJUSTL(BUFFER))
            READ(BUFFER,*) ALO,ALT,SWL      
            WL_ETSS(I,NREC)=SWL
            IF(ALO.GT.180.0) ALO=ALO-360.0
            LON_ETSS(I,NREC)=ALO
            LAT_ETSS(I,NREC)=ALT
          ENDDO
        ENDDO
        CLOSE(1)
        NT=NREC

c------------------------------------------------------------------
C   Find a ETSS time period to cover start to end time
c------------------------------------------------------------------
        IF(TIMEOBC(1).gt.day_start) TIMEOBC(1)=day_start
        IF(TIMEOBC(NT).lt.day_end) TIMEOBC(NT)=day_end
        NSTR=1
        N=1
        DO WHILE (TIMEOBC(N).LT.DAY_START)
          N=N+1
        ENDDO
        NSTR=N-1

        IF(NSTR.EQ.0.OR.NSTR.GT.NT) THEN
          WRITE(*,*) 'Time of first data is later than start time'
          WRITE(*,*) 'Time period is not covered by dataset'
          TMPTXT=' '
          TMPTXT=TRIM(DBASE_WL)//' does not cover forecast'
          WRITE(ICORMS,'(a)') trim(TMPTXT)
          TMPTXT=' '
          TMPTXT='CRITICAL FAULURE IN GENERATING WL OBC'
          WRITE(ICORMS,'(a)') trim(TMPTXT)
	  STOP
        ENDIF

        N=1
        DO WHILE (TIMEOBC(N).LT.DAY_END)
          N=N+1
        ENDDO
        NEND=N

        IF(N.GT.NT) THEN
	  WRITE(*,*) 'Time period is not covered by dataset'	  
          TMPTXT=' '
          TMPTXT=TRIM(DBASE_WL)//' does not cover forecast'
          WRITE(ICORMS,'(a)') trim(TMPTXT)
          TMPTXT=' '
          TMPTXT='CRITICAL FAULURE IN GENERATING WL OBC'
          WRITE(ICORMS,'(a)') trim(TMPTXT)
	  STOP
        ENDIF  	  
        WRITE(*,*) 'start and end times = ',
     &    TIMEOBC(NSTR),TIMEOBC(NEND),NSTR,NEND,DAY_START,DAY_END
        NTMAX_WL=NEND-NSTR+1

C -------------------------------------------------------------------
C  End of reading ETSS ASCII file and begin to interpolate onto 
C  open boundary grid points
C -------------------------------------------------------------------
        IF(ALLOCATED(ZETA_TIME)) DEALLOCATE(ZETA_TIME)
        ALLOCATE(ZETA_TIME(NTMAX_WL))
        DO N=1,NTMAX_WL
          ZETA_TIME(N)=TIMEOBC(NSTR+N-1)
        ENDDO
        IF(ALLOCATED(WLOBC)) DEALLOCATE(WLOBC)
        ALLOCATE(WLOBC(NOBC,NTMAX_WL))
        DO I=1,NOBC
          DO N=1,NTMAX_WL
            WLOBC(I,N)=0.0
          ENDDO
        ENDDO   

        IF(TRIM(DBASE_WL).EQ.'ETSS') THEN
          IF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
            WRITE(*,*) 'This option does not work for ETSS!!!!'
            WRITE(*,*) 'Please redefine IGRD=1 or 4 !!'
            IGRD=1
          ENDIF
        ENDIF
	 
        IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
          IF(ALLOCATED(XINP)) DEALLOCATE(XINP)
          IF(ALLOCATED(YINP)) DEALLOCATE(YINP)
          IF(ALLOCATED(ZINP)) DEALLOCATE(ZINP)
          IF(ALLOCATED(XOUT)) DEALLOCATE(XOUT)
          IF(ALLOCATED(YOUT)) DEALLOCATE(YOUT)
          IF(ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
          IF(ALLOCATED(WEIGHTNODES)) DEALLOCATE(WEIGHTNODES)
          IF(ALLOCATED(WEIGHTS)) DEALLOCATE(WEIGHTS)
          ALLOCATE(XINP(NETSS))
          ALLOCATE(YINP(NETSS))
          ALLOCATE(ZINP(NETSS))
          ALLOCATE(XOUT(NOBC))
          ALLOCATE(YOUT(NOBC))
          ALLOCATE(ZOUT(NOBC))
          ALLOCATE(WEIGHTNODES(NOBC,3))
          ALLOCATE(WEIGHTS(NOBC,3))
        ELSEIF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
          WRITE(*,*) 'This option is not available anymore!!!!'
          WRITE(*,*) 'Please redefine IGRD=1 or 4 !!'
          STOP
        ENDIF 
        NDATA=NETSS

        DO N=NSTR,NEND
          NDATA=ITMP1D(N)
          N0=N-NSTR+1
          IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
            NDUM=0
            DO I=1,NDATA
              IF(WL_ETSS(I,N).LT.999.0) THEN
                NDUM=NDUM+1
                XINP(NDUM)=LON_ETSS(I,N)
                YINP(NDUM)=LAT_ETSS(I,N)
                ZINP(NDUM)=WL_ETSS(I,N)
              ENDIF
            ENDDO
C            WRITE(*,*) 'NDATA=',NDATA,'NDUM=',NDUM,'N=',N 
            NDATA=NDUM

            IF(IGRD.EQ.1) THEN
              CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &          LONOBC,LATOBC,ZOUT,WEIGHTNODES,WEIGHTS,0)
              CALL INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &          LONOBC,LATOBC,ZOUT,WEIGHTNODES,WEIGHTS,1)
            ELSEIF(IGRD.EQ.4) THEN
              CALL INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &          NOBC,lonOBC,latOBC,ZOUT)
            ENDIF
 
            DO I=1,NOBC
              WLOBC(I,N0)=ZOUT(I)
            ENDDO
          ENDIF
        ENDDO
      ENDIF

C -------------------------------------------------------------------
C   End of processing for WL OBC from ETSS forecasts
C   Begin time interpolation to DELT	
c------------------------------------------------------------------
      NREC=NINT((JDAYE-JDAYS)*24/DELT)+1
      IF(ALLOCATED(ONED1)) DEALLOCATE(ONED1)
      IF(ALLOCATED(ONED2)) DEALLOCATE(ONED2)
      IF(ALLOCATED(ONED3)) DEALLOCATE(ONED3)
      IF(ALLOCATED(ONED4)) DEALLOCATE(ONED4)
      IF(ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
      ALLOCATE(ONED1(NREC))
      ALLOCATE(ONED2(NREC))
      ALLOCATE(ONED3(NTMAX_WL))
      ALLOCATE(ONED4(NTMAX_WL))
      ALLOCATE(TMP2D(NOBC,NREC))

      DO I=1,NOBC
        DO N=1,NTMAX_WL
          ONED3(N)=WLOBC(I,N)
        ENDDO       
        CALL LINEARARRAY(NREC,TIME_M,ONED2,NTMAX_WL,ZETA_TIME,ONED3)
        DO N=1,NREC
          TMP2D(I,N)=ONED2(N)
        ENDDO
      ENDDO

      IF(ALLOCATED(WLOBC)) DEALLOCATE(WLOBC)
      ALLOCATE(WLOBC(NOBC,NREC))
      IF(ALLOCATED(ZETA_TIME)) DEALLOCATE(ZETA_TIME)
      ALLOCATE(ZETA_TIME(NREC))

      DO N=1,NREC
        ZETA_TIME(N)=TIME_M(N)
        DO I=1,NOBC
          WLOBC(I,N)=TMP2D(I,N) 
        ENDDO
      ENDDO
      NTMAX_WL=NREC
     
C   For SFBOFS set nontidal water levels = 0 at upper rivers, 
C   but specified by observations
      IF((TRIM(OFS).EQ.'sfbofs').OR.(TRIM(OFS).EQ.'SFBOFS')) THEN	
        DO N=1,NREC
          DO I=95,105
            WLOBC(I,N)=0.0 
          ENDDO
        ENDDO

        DO N=1,NTMAX
          DO I=95,105
            DO K=1,KBm
              TEMPOBC_M(I,K,N)=0.0
              SALTOBC_M(I,K,N)=0.0
            ENDDO
          ENDDO
        ENDDO
      ENDIF   

C -------------------------------------------------------------------
C   End of processing for WL OBC from ETSS forecasts
C   Print OBC for evaluation
      CLOSE(33)
      OPEN(33,FILE='WL_OBC.dat',STATUS='UNKNOWN')
      DO N=1,NTMAX_WL
        WRITE(33,35) ZETA_TIME(N),(WLOBC(I,N),I=1,NOBC,10),
     &    WLOBC(NOBC,N)
      ENDDO
      CLOSE(33)

      OPEN(33,FILE='TEMP_OBC.dat',STATUS='UNKNOWN')
      DO N=1,NTMAX
        DO I=1,NOBC
          WRITE(33,35) TS_TIME(N),(TEMPOBC_M(I,K,N),K=1,KBM)
        ENDDO
      ENDDO
      CLOSE(33)

      OPEN(33,FILE='SALT_OBC.dat',STATUS='UNKNOWN')
      DO N=1,NTMAX
        DO I=1,NOBC
          WRITE(33,35) TS_TIME(N),(SALTOBC_M(I,K,N),K=1,KBM)
        ENDDO
      ENDDO
      CLOSE(33)

      OPEN(33,FILE='U_OBC.dat',STATUS='UNKNOWN')
      DO N=1,NTMAX
        WRITE(33,35) TS_TIME(N),(UEOBC_M(I,2,N),I=1,NEOBC,10),
     &    UEOBC_M(NEOBC,KBM,N)
      ENDDO
      CLOSE(33)

      OPEN(33,FILE='V_OBC.dat',STATUS='UNKNOWN')
      DO N=1,NTMAX
        WRITE(33,35) TS_TIME(N),(VEOBC_M(I,2,N),I=1,NEOBC,10),
     &    VEOBC_M(NEOBC,KBM,N)
      ENDDO
      CLOSE(33)
35    FORMAT(50F16.8)

C -------------------------------------------------------------------
C   End of processing for WL OBC from ETSS forecasts
C   Begin to process real time observations and tidal prediction
C   Do 10 days tidal prediction from day_start-2 to cover N/F periods
C -------------------------------------------------------------------
      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      ALLOCATE(ONED1(NMAX))
      ALLOCATE(ONED2(NMAX))

      KINDAT=2
      CONV=1.0
      XMAJOR=0.0
      DELT_PRD=0.1
      NREC=NINT((JDAYE-JDAYS+2)*24/DELT_PRD)+1
      DO N=1,NREC
        TIME_PRD(N)=(jdays-jbase_date-2)+(N-1)*DELT_PRD/24.0
      ENDDO	

      JDAY=JDAYS-2
      CALL GREGORIAN(JDAY,YEARB,MONTHB,DAYB,HOURB)
      IYR=INT(YEARB)
      IMM=INT(MONTHB+0.001)
      IDD=INT(DAYB+0.001)
      IHH=INT(HOURB+0.001)
      IMN=INT((HOURB-IHH)*60+0.1)
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
        IF(MOD(IYR,4).EQ.0) DAYS_PER_MONTH(2)=29
        IF(IDD.GT.DAYS_PER_MONTH(IMM)) THEN
          IDD=IDD-DAYS_PER_MONTH(IMM)
          IMM=IMM+1
          IF(IMM.GT.12) THEN
            IMM=IMM-12
            IYR=IYR+1
          ENDIF
        ENDIF
      ENDIF
      WRITE(START_TIMEm1,'(I4.4,4I2.2)') IYR,IMM,IDD,IHH,0

      DO I=1,NSTA
        NTR(I)=0
        NTR_T(I)=0
        NTR_S(I)=0
        DO N=1,NMAX
          RTIME(I,N)=-99999.9
          RTIME_T(I,N)=-99999.9
          RTIME_S(I,N)=-99999.9
          WL_OBS(I,N)=-99999.9
          WL_PRD(I,N)=-99999.9
          SWL_OBS(I,N)=-99999.9
          T_OBS(I,N)=-99999.9
          S_OBS(I,N)=-99999.9
        ENDDO
      ENDDO

      DO I=1,NSTA
        BUFFER=TRIM(adjustL(NOS_ID(I)))
        L1=LEN_TRIM(BUFFER)
        DO N=1,NWLON_STA
          L2=size(stationID,1)
          TMPTXT=' '
          DO N1=1,L2
            TMPTXT(N1:N1)=stationID(N1,N)
          END DO
          WRITE(BUFFER1,'(a)') TMPTXT(1:L2)
          BUFFER1=TRIM(adjustL(BUFFER1))
          IF(BUFFER(1:L1).EQ.BUFFER1(1:L1)) EXIT 
        ENDDO
110     CONTINUE
        WRITE(*,*) 'Conduct tide prediction at STATION: ',BUFFER(1:L1)

        IF(N.GT.NWLON_STA) THEN 
          WRITE(*,*) 'STATION ID IS NOT FOUND IN THE HC NetCDF FILE'
          IF(WL_FLAG(I).EQ.0) THEN
            WRITE(*,*) 'PLEASE ADD HC OF STATION '//TRIM(NOS_ID(I))
            WRITE(*,*) TRIM(OFS)//' STOP HERE'
            STOP
          ENDIF 		
 
          DO N=1,NREC
            WL_PRD(I,N)=-99999.9
          ENDDO
        ELSE   
          FOUT=TRIM(NOS_ID(I))//'.prd'
          DO K=1,37
            AMP(K)=tide_amp(N,K)
            EPOC(K)=tide_epoc(N,K)
          ENDDO
          CALL NOS_PRD(START_TIMEm1,END_TIME,KINDAT,DELT_PRD,CONV,
     &      XMAJOR,AMP,EPOC,FOUT,ONED1,ONED2)

          DO N=1,NREC
            WL_PRD(I,N)=ONED1(N)
          ENDDO
        ENDIF   
      ENDDO

C --------------------------------------------------------------------------
C    Process real-time data in BUFR files of NCEP data tank at NWLON stations
C --------------------------------------------------------------------------
      WRITE(FOUT,201) 'DATA_OBC_',IYRS,IMMS,IDDS,IHHS,'.dat' 
201   FORMAT(A9,I4.4,3I2.2,A4)
      FOUT=TRIM(OFS)//'_'//TRIM(adjustL(FOUT))
      CLOSE(15) 
      OPEN(15,file=TRIM(FOUT) )
cc      write(15,*) 'ID        lat     lon   year  mon day hour
cc     *  Disch   Tem SST   Salinity'
      DO IZ=INT(DAY_START-1),INT(DAY_END)
        JDAY=IZ+JBASE_DATE
        CALL GREGORIAN(JDAY,YEARB,MONTHB,DAYB,HOURB)
        IYR=INT(YEARB)
        IMM=INT(MONTHB+0.001)
        IDD=INT(DAYB+0.001)
        IHH=INT(HOURB+0.001)
        IMN=INT((HOURB-IHH)*60+0.1)
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
          IF(MOD(IYR,4).EQ.0) DAYS_PER_MONTH(2)=29
          IF(IDD.GT.DAYS_PER_MONTH(IMM)) THEN
            IDD=IDD-DAYS_PER_MONTH(IMM)
            IMM=IMM+1
            IF(IMM.GT.12) THEN
              IMM=IMM-12
              IYR=IYR+1
            ENDIF
          ENDIF
        ENDIF
        WRITE(BUFRFILE,700) '/',IYR,IMM,IDD,'/b001/'
700     FORMAT(A1,I4.4,2I2.2,A6)

        BUFRFILE=TRIM(COMPORTS)//TRIM(BUFRFILE)//TRIM(NOSBUFR) 
        INQUIRE(FILE=trim(BUFRFILE),EXIST=FEXIST)
        IF(.NOT.FEXIST) GOTO 860
        WRITE(CTMP,'(a1,I4.4,2I2.2)') '.',IYR,IMM,IDD
        WRITE(*,*) 'BUFR FILE= ',TRIM(BUFRFILE)
        CMD='cp '//trim(BUFRFILE)//'  '//TRIM(NOSBUFR)//trim(CTMP)
        CALL SYSTEM(trim(CMD))
        CMD='cp '//trim(BUFRFILE)//'  '//TRIM(NOSBUFR)
        CALL SYSTEM(trim(CMD))
        BUFRFILE=trim(NOSBUFR)

        LUNIN=11
	CLOSE(LUNIN)
        OPEN(LUNIN,file=trim(BUFRFILE),FORM='UNFORMATTED')
C-----------------OPEN AND READ THRU THE INPUT BUFR FILE ------------------
        CALL OPENBF(LUNIN,'IN',LUNIN)                                     

C-----------READ THROUGH THE MESSAGES/SUBSETS IN THE FILE------------------
        DO WHILE (IREADMG(LUNIN,SUBSET,IDATE).EQ.0)                       
          DO WHILE (IREADSB(LUNIN).EQ.0)                                    
c---------READ THE INTERNAL DATE AND CHECK FOR REALISM---------------------
C                           
            CALL UFBINT(LUNIN,DATES,5,1,IRET,
     &           'YEAR MNTH DAYS HOUR MINU')
            IYR=NINT(DATES(1))
            IMM=NINT(DATES(2))
            IDD=NINT(DATES(3))                                           
            IHH=NINT(DATES(4))                                            
            IMN=NINT(DATES(5))                                            
            YEARB=IYR
            MONTHB=IMM
            DAYB=IDD
            HOURB=REAL(IHH)+REAL(IMN/60.0)   
            DAYJ=JULIAN(YEARB,MONTHB,DAYB,HOURB)-JBASE_DATE

C---------READ THE TIDE GAUGE STATION INFO FROM BUFR FILE------------------
C  AJ 09/15/11 Use different CALL routine to handle the long station IDs 
            CALL READLC(LUNIN, stnbufrid, 'RPID')
            CALL UFBINT(LUNIN,DATES,5,1,IRET,'CLAT CLON SELV')
            CLAT=DATES(1)
            CLON=DATES(2)
            SELV=DATES(3)
            DO I=1,NSTA 
              IF(trim(stnbufrid).EQ.TRIM(NWS_ID(I))) THEN
C--------------------------------------------------------------------------
C  GET SEA SURFACE TEMPERATURE DATA ALONG WITH DATA CHECK AND
C  TIME INCREMENT AND DISPLACEMENT INFORMATION
C  SST1 -- Sea Surface Temperature (Kelvin degree)
C  QMST -- Sea Surface Data Check Flag
C  AWCK -- Tide Station Automated Water Data Check Flag
C  MWCK -- Tide Station Manual Water Data Check Flag
C  TPMI -- Time Period or Replacement
C--------------------------------------------------------------------------
                LLEN=LEN_TRIM(BUFRFILE)
                IF(BUFRFILE(LLEN-4:LLEN).EQ.'xx005') THEN      
                  CALL UFBINT(LUNIN,DATES,5,1,IRET,
     &	               'SST1 TMDB AWCK MWCK TPMI')
                ELSEIF(BUFRFILE(LLEN-4:LLEN).EQ.'xx012') THEN 
C  GET AIR TEMPERATURE AND SEA SURFACE TEMPERATURE DATA
                  CALL UFBINT(LUNIN,DATES,5,1,IRET,'WATM TMDB')
	        ENDIF	 

                IF(DATES(1).GE.BMISS/2.0) THEN
                  SST=-99999.99
                ELSE	  
                  SST=DATES(1)-273.15
                ENDIF

                IF(DATES(2).GE.BMISS/2.0) THEN
                  ATMP=-99999.99
                ELSE	  
                  ATMP=DATES(2)-273.15   
                ENDIF

                IF(ABS(SST).LT.40.00) THEN
                  IF(NTR_T(I).LT.1) THEN 
                    NTR_T(I)=NTR_T(I)+1
                    RTIME_T(I,NTR_T(I))=DAYJ
                    T_OBS(I,NTR_T(I))=SST
                  ELSE
                    IF(DAYJ.GT.RTIME_T(I,NTR_T(I))) THEN 
                      NTR_T(I)=NTR_T(I)+1
                      RTIME_T(I,NTR_T(I))=DAYJ
                      T_OBS(I,NTR_T(I))=SST
                    ENDIF
                  ENDIF
                ENDIF

C--------------------------------------------------------------------------
C  GET CONDUCTIVITY AND SALINITY
C--------------------------------------------------------------------------
                CALL UFBINT(LUNIN,DATES,5,1,IRET,'SALN WACN')
                IF(DATES(1).GE.BMISS/2.0) THEN
                  SALN=-99999.99
                ELSE	  
                  SALN=DATES(1)
                ENDIF

                IF(DATES(2).GE.BMISS/2.0) THEN
                  COND=-99999.99
                ELSE	  
                  COND=DATES(2)  ! Unit of COND is mS/cm at NOS station 
                ENDIF

                IF(ABS(SALN).GT.999.99) THEN
                  IF((ABS(SST).LT.999.99).AND.
     &               (ABS(COND).LT.999.99)) THEN
                    SALN=SAL(COND,SST,0.0)
                  ELSE
                    SALN=-99999.99
                  ENDIF   
                ENDIF
                IF(SALN.LT.-0.5) SALN=-99999.99

                IF(ABS(SALN).LT.40.00) THEN
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

C--------------------------------------------------------------------------
C  GET TIDAL ELEVATION WITH RESPECT TO CHART AND METEOROLOGICAL RESIDUAL 
C  TIDAL ELEVATION
C--------------------------------------------------------------------------
                IF(BUFRFILE(LLEN-4:LLEN).EQ.'xx005') THEN      
                  CALL UFBINT(LUNIN,DATES,5,1,IRET,'TERC TIDER')
                ELSEIF(BUFRFILE(LLEN-4:LLEN).EQ.'xx012') THEN 
                  CALL UFBINT(LUNIN,DATES,5,1,IRET,'TLLW TIDER')
                ENDIF	 

                IF(DATES(1).GE.BMISS/2.0) THEN
                  EL=-99999.99
                ELSE	  
                  EL=DATES(1) 
                ENDIF

                IF(DATES(2).GE.BMISS/2.0) THEN
                  SWL=-99999.99
                ELSE	  
                  SWL=DATES(2)
                ENDIF

                IF(ABS(EL).LT.40.00) THEN
                  IF(NTR(I).LT.1) THEN 
                    NTR(I)=NTR(I)+1
                    RTIME(I,NTR(I))=dayj
                    WL_OBS(I,NTR(I))=EL-DATUM(I)
                    SWL_OBS(I,NTR(I))=SWL
                  ELSE
                    IF(DAYJ.GT.RTIME(I,NTR(I))) THEN 
                      NTR(I)=NTR(I)+1
                      RTIME(I,NTR(I))=DAYJ
                      WL_OBS(I,NTR(I))=EL-DATUM(I)
                      SWL_OBS(I,NTR(I))=SWL
                    ENDIF
                  ENDIF
                ENDIF
                WRITE(15,780) trim(stnbufrid),clon,clat,IYR,IMM,
     &		  IDD,IHH,IMN,EL,ATMP,SST,SALN,COND
              ENDIF 
 	    ENDDO
780         FORMAT(a20,1x,2F10.4,I5,4i3,10F12.4)
800         CONTINUE
          ENDDO
        ENDDO
850     CONTINUE
        CALL CLOSBF(LUNIN)
        CLOSE(LUNIN)
860     CONTINUE
      ENDDO

C---------------------------------------------------------------------------
C   Decoding USGS BURF file if used
C---------------------------------------------------------------------------
      IF(.NOT.USGS_L) GOTO 9000
      DO IZ=INT(DAY_START-1),INT(DAY_END)
        JDAY=IZ+JBASE_DATE
        CALL GREGORIAN(JDAY,YEARB,MONTHB,DAYB,HOURB)
        IYR=INT(YEARB)
        IMM=INT(MONTHB+0.001)
        IDD=INT(DAYB+0.001)
        IHH=INT(HOURB+0.001)
        IMN=INT((HOURB-IHH)*60+0.1)
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
          IF(MOD(IYR,4).EQ.0) DAYS_PER_MONTH(2)=29
          IF(IDD.GT.DAYS_PER_MONTH(IMM)) THEN
            IDD=IDD-DAYS_PER_MONTH(IMM)
            IMM=IMM+1
            IF(IMM.GT.12) THEN
              IMM=IMM-12
              IYR=IYR+1
            ENDIF
          ENDIF
        ENDIF

        WRITE(BUFRFILE,700) '/',IYR,IMM,IDD,'/b001/'  
        BUFRFILE=TRIM(COMPORTS)//TRIM(BUFRFILE)//trim(USGSBUFR) 
        INQUIRE(FILE=trim(BUFRFILE),EXIST=FEXIST)
        IF(.NOT.FEXIST) GOTO 8860
        WRITE(*,*) 'BUFR FILE= ',TRIM(BUFRFILE)
        WRITE(CTMP,'(a1,I4.4,2I2.2)') '.',IYR,IMM,IDD
        CMD='cp -p '//trim(BUFRFILE)//'  '//TRIM(USGSBUFR)//trim(CTMP)
        CALL SYSTEM(trim(CMD))
        BUFRFILE=trim(USGSBUFR)//trim(CTMP)

        LUNIN=11
	CLOSE(LUNIN)
        OPEN(LUNIN,FILE=trim(BUFRFILE),FORM='UNFORMATTED')
        CALL OPENBF(LUNIN,'IN',LUNIN)                                     
C-----------READ THROUGH THE MESSAGES/SUBSETS IN THE FILE------------------
        DO WHILE(IREADMG(LUNIN,SUBSET,IDATE).EQ.0)                       
          DO WHILE(IREADSB(LUNIN).EQ.0)                                    
C---------READ THE INTERNAL DATE AND CHECK FOR REALISM---------------------
            CALL UFBINT(LUNIN,DATES,5,1,IRET,
     &           'YEAR MNTH DAYS HOUR MINU')
            IYR=NINT(DATES(1))
            IMM=NINT(DATES(2))
            IDD=NINT(DATES(3))                                           
            IHH=NINT(DATES(4))                                            
            IMN=NINT(DATES(5))                                            
            YEARB=IYR
            MONTHB=IMM
            DAYB=IDD
            HOURB=REAL(IHH)+REAL(IMN/60.0) 
            DAYJ=JULIAN(YEARB,MONTHB,DAYB,HOURB)-JBASE_DATE

C---------READ THE TIDE GAUGE STATION INFO FROM BUFR FILE------------------
            CALL READLC(LUNIN, stnbufrid, 'RPID')
            CALL UFBINT(LUNIN,DATES,5,1,IRET,'CLAT CLON SELV')
            CLAT=DATES(1)
            CLON=DATES(2)
            SELV=DATES(3)

            DO I=1,NSTA
              IF(trim(stnbufrid).EQ.TRIM(NOS_ID(I))) THEN
C-------------------------------------------------------------------------
C   GET AIR TEMPERATURE AND SEA SURFACE TEMPERATURE DATA
                CALL UFBINT(LUNIN,DATES,5,1,IRET,'SST1 TMDB')
                IF(DATES(1).GE.BMISS/2.0) THEN
                  SST=-99999.99
                ELSE	  
                  SST=DATES(1)-273.15
                ENDIF

                IF(DATES(2).GE.BMISS/2.0) THEN
                  ATMP=-99999.99
                ELSE	  
                  ATMP=DATES(2)-273.15
                ENDIF

                IF(ABS(SST).LT.40.00) THEN
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

C--------------------------------------------------------------------------
C   GET RIVER STAGE DATA INCLUDING INFO ON USGS SENSOR TYPE 
C   Stage height is in meters
                CALL UFBINT(LUNIN,DATES,5,1,IRET,'RSH29 STRV')
                IF(DATES(1).GE.BMISS/2.0) THEN
                  RSH29=-99999.99
                ELSE	  
                  RSH29=DATES(1)
                ENDIF

                IF(DATES(2).GE.BMISS/2.0) THEN
                  STRV=-99999.99
                ELSE	  
                  STRV=DATES(2)
                ENDIF

C--------------------------------------------------------------------------
C GET RIVER STAGE HEIGHT ABOVE NGVD 1929, STREAM VELOCITY, AND SALINITY
C--------------------------------------------------------------------------
                CALL UFBINT(LUNIN,DATES,5,1,IRET,'RSHM DDRS')
                IF(DATES(1).GE.BMISS/2.0) THEN
                  EL=-99999.99
                ELSE
                  EL=DATES(1)  
                ENDIF

                IF(ABS(EL).LT.40.00) THEN
                  IF(NTR(I).LT.1) THEN 
                    NTR(I)=NTR(I)+1
                    RTIME(I,NTR(I))=DAYJ
                    WL_OBS(I,NTR(I))=EL-DATUM(I)
                    SWL_OBS(I,NTR(I))=SWL
                  ELSE
                    IF(DAYJ.GT.RTIME(I,NTR(I))) THEN 
                      NTR(I)=NTR(I)+1
                      RTIME(I,NTR(I))=DAYJ
                      WL_OBS(I,NTR(I))=EL-DATUM(I)
                      SWL_OBS(I,NTR(I))=SWL
                    ENDIF
                  ENDIF
                ENDIF

C--------------------------------------------------------------------------
C   GET CONDUCTIVITY AND SALINITY
C   Convert specific conductance C25,0 of USGS into conductivity. 
C   See Sea Bird Electronics, Inc
C   Fresh Water Conductivity Measurements using SBE-19 SEACAT Profiler
C   specific C25,0 [us/cm]=(C/[1+0.02*(Temperature-25)] where C is in ms/cm 
C--------------------------------------------------------------------------
                CALL UFBINT(LUNIN,DATES,5,1,IRET,'SALN WACN')
                IF(DATES(1).GE.BMISS/2.0) THEN
                  SALN=-99999.99
                ELSE	  
                  SALN=DATES(1)
                ENDIF

                IF(DATES(2).GE.BMISS/2.0) THEN
                  COND=-99999.99
                else	  
                  COND=DATES(2)   
                endif

                IF(ABS(SALN).GT.999.99) THEN
                  IF((ABS(SST).LT.999.99).AND.
     &               (ABS(COND).LT.999.99)) THEN
                    COND=COND*(1.0+0.02*(SST-25.0))
                    SALN=SAL(COND,SST,0.0)
                  ENDIF   
                ENDIF
                IF(SALN.LT.-0.5) SALN=-99999.99  

                IF(ABS(SALN).LT.40.00) THEN
                  IF(NTR_S(I).LT.1) THEN 
                    NTR_S(I)=NTR_S(I)+1
                    RTIME_S(I,NTR_S(I))=DAYJ
                    S_OBS(I,NTR_S(I))=SALN
                  ELSE
                    IF(DAYJ.GT.RTIME_S(I,NTR_S(I))) THEN 
                      NTR_S(I)=NTR_S(I)+1
                      RTIME_S(I,NTR_S(I))=DAYJ
                      S_OBS(I,NTR_S(I))=SALN
                    ENDIF
                  ENDIF
                ENDIF
                WRITE(15,780) trim(stnbufrid),clon,clat,IYR,IMM,
     &		  IDD,IHH,IMN,EL,ATMP,SST,SALN,COND
              ENDIF 
 	    ENDDO
8800        CONTINUE
          ENDDO
        ENDDO
8850    CONTINUE
        CALL CLOSBF(LUNIN)
        CLOSE(LUNIN)
8860    CONTINUE
      ENDDO

9000  CONTINUE
      CLOSE(15)
      INQUIRE(FILE=TRIM(FOUT),EXIST=FEXIST)
      IF(FEXIST) THEN
        CMD='sort '//TRIM(FOUT)//' > tmp.dat'
        CALL SYSTEM(trim(CMD))
        CMD='uniq tmp.dat > tmp1.dat'
        CALL SYSTEM(trim(CMD))
        CMD='cp tmp1.dat '//TRIM(FOUT)
        CALL SYSTEM(trim(CMD))
      ENDIF	 

C--------------------------------------------------------------------------
C  End of reading WL BUFR FILES and begin WL QC procedures
C--------------------------------------------------------------------------
      IF(ALLOCATED(ONED1)) DEALLOCATE(ONED1)
      IF(ALLOCATED(ONED2)) DEALLOCATE(ONED2)
      IF(ALLOCATED(ONED3)) DEALLOCATE(ONED3)
      IF(ALLOCATED(ONED4)) DEALLOCATE(ONED4)
      ALLOCATE(ONED1(NMAX))
      ALLOCATE(ONED2(NMAX))
      ALLOCATE(ONED3(NMAX))
      ALLOCATE(ONED4(NMAX))

      DO I=1,NSTA
        IF(NTR(I).GE.2) THEN
          AVG=0.0
	  NTMP=0
	  DO N=1,NTR(I)
	    IF(ABS(WL_OBS(I,N)).LE.20.0) THEN
              NTMP=NTMP+1
              AVG=AVG+WL_OBS(I,N)
              ONED1(NTMP)=WL_OBS(I,N)
            ENDIF
          ENDDO 
 
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
          WRITE(*,*) 'Bound of WL = ',BOUND_L,BOUND_U,AVG,NOS_ID(I)

          NTMP=0
          DO N=1,NTR(I)
            IF((WL_OBS(I,N).GE.BOUND_L).AND. 
     &	       (WL_OBS(I,N).LE.BOUND_U)) THEN
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

C  Gradient change limit Dh/Dt<=0.7 m/hr and assume first data is good
        IF(NTR(I).GE.2) THEN
          NTMP=1
          ONED1(NTMP)=WL_OBS(I,1)
          ONED2(NTMP)=RTIME(I,1)
          DO N=2,NTR(I)  
            GD=(WL_OBS(I,N)-ONED1(NTMP))/
     &         (RTIME(I,N)- ONED2(NTMP))/24.0
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

        BUFFER='Before QC NUMBER OF WL AT '//TRIM(NOS_ID(I))
        BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
        BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
        TMPTXT=' '
        WRITE(TMPTXT,'(I5)') NTR(I)
        BUFFER=TRIM(BUFFER)//TRIM(TMPTXT)
        WRITE(*,*) TRIM(BUFFER)
        WRITE(ICORMS,'(A)') TRIM(BUFFER)

C------------------------------------------------------------------------
C   Begin QC procedures for Temperature
C------------------------------------------------------------------------
        IF(NTR_T(I).GE.2) THEN
          AVG=0.0
          NTMP=0
          DO N=1,NTR_T(I)
            IF(ABS(T_OBS(I,N)).LE.40.0) THEN
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
          WRITE(*,*) 'Bound of T = ',BOUND_L,BOUND_U,AVG,NOS_ID(I)

          NTMP=0
          DO N=1,NTR_T(I)
            IF((T_OBS(I,N).GE.BOUND_L).AND.
     &	       (T_OBS(I,N).LE.BOUND_U)) THEN
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

C  Gradient change limit DT/Dt<=2.0 deg/hr and assume first data is good
        IF(NTR_T(I).GE.2) THEN
          NTMP=1
          ONED1(NTMP)=T_OBS(I,1)
          ONED2(NTMP)=RTIME_T(I,1)
          DO N=2,NTR_T(I)  
	    GD=(T_OBS(I,N)-ONED1(NTMP))/
     &         (RTIME_T(I,N)-ONED2(NTMP))/24.0
            IF(ABS(GD).LE.2.0) THEN
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

C--------------------------------------------------------------------------
C   Begin QC procedures for Salinity
C--------------------------------------------------------------------------
        IF(NTR_S(I).GE.2) THEN
          AVG=0.0
          NTMP=0
          DO N=1,NTR_S(I)
            IF(ABS(S_OBS(I,N)).LE.40.0) THEN
              NTMP=NTMP+1
              AVG=AVG+S_OBS(I,N)
              ONED1(NTMP)=S_OBS(I,N)
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
          WRITE(*,*) 'Bound of SALT = ',BOUND_L,BOUND_U,AVG,NOS_ID(I)

          NTMP=0
          DO N=1,NTR_S(I)
            IF((S_OBS(I,N).GE.BOUND_L).AND.
     &	       (S_OBS(I,N).LE.BOUND_U)) THEN
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
C  Gradient change limit DS/Dt<=3.0 psu/hr and assume first data is good
          NTMP=1
          ONED1(NTMP)=S_OBS(I,1)
          ONED2(NTMP)=RTIME_S(I,1)
          DO N=2,NTR_S(I)  
            GD=(S_OBS(I,N)-ONED1(NTMP))/
     &         (RTIME_S(I,N)-ONED2(NTMP))/24.0
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

C--------------------------------------------------------------------------
C   End of  QC procedures
C   Begin detiding SWL=OBS -PRED
C--------------------------------------------------------------------------
      DO I=1,NSTA
        FOUT=TRIM(NOS_ID(I))//'.obs'
        CLOSE(10)
        OPEN(10,file=TRIM(FOUT))
        N0=1
        DO N=1,NTR(I)
          IF(ABS(WL_OBS(I,N)).LT.20.0) THEN 
          JDAY=RTIME(I,N)+JBASE_DATE
          CALL GREGORIAN(JDAY,YEARB,MONTHB,DAYB,HOURB)
          IYR=INT(YEARB)
          IMM=INT(MONTHB+0.001)
          IDD=INT(DAYB+0.001)
          IHH=INT(HOURB+0.001)
          IMN=INT((HOURB-IHH)*60+0.1)
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
            IF(MOD(IYR,4).EQ.0) DAYS_PER_MONTH(2)=29
            IF(IDD.GT.DAYS_PER_MONTH(IMM)) THEN
              IDD=IDD-DAYS_PER_MONTH(IMM)
              IMM=IMM+1
              IF(IMM.GT.12) THEN
                IMM=IMM-12
                IYR=IYR+1
              ENDIF
            ENDIF
          ENDIF

          DO N1=N0,NREC-1
            IF((RTIME(I,N).GE.TIME_PRD(N1)).AND.
     &	       (RTIME(I,N).LT.TIME_PRD(N1+1))) THEN
              X1=TIME_PRD(N1)
              X2=TIME_PRD(N1+1)
              Y1=WL_PRD(I,N1)
              Y2=WL_PRD(I,N1+1)
              CALL linear(X1,Y1,X2,Y2,RTIME(I,N),Y)
              SWL_OBS(I,N)=WL_OBS(I,N)-Y
              N0=N1
              EXIT 
            ENDIF
          ENDDO
          IF(N1.GT.NREC) SWL_OBS(I,N)=-99999.99
870       CONTINUE	     		
	   
          WRITE(10,900) RTIME(I,N),IYR,IMM,IDD,IHH,IMN,
     &	    WL_PRD(I,N1),WL_OBS(I,N),SWL_OBS(I,N),T_OBS(I,N)
          ENDIF
 890      CONTINUE    
        ENDDO
        TMPTXT=' '
        WRITE(TMPTXT,'(I5)') NTR(I)
        BUFFER=' '
        BUFFER='STA= '//trim(NOS_ID(I))//' Number of WL obs= '//
     &    trim(TMPTXT)
        WRITE(*,*) trim(BUFFER) 
        WRITE(ICORMS,'(A)') trim(BUFFER)
      ENDDO	
900   FORMAT(f10.4,1x,I5,4i3,4F12.4)

C-----------------------------------------------------------------------
C   If no WL obs at primary station, looking for its backup station
C-----------------------------------------------------------------------
      IF(ALLOCATED(ONED1)) DEALLOCATE(ONED1)
      IF(ALLOCATED(ONED2)) DEALLOCATE(ONED2)
      IF(ALLOCATED(ONED3)) DEALLOCATE(ONED3)
      IF(ALLOCATED(ONED4)) DEALLOCATE(ONED4)
      IF(ALLOCATED(AVGERR)) DEALLOCATE(AVGERR)
      IF(ALLOCATED(AVGERR_T)) DEALLOCATE(AVGERR_T)
      IF(ALLOCATED(AVGERR_S)) DEALLOCATE(AVGERR_S)
      ALLOCATE(ONED1(NMAX))
      ALLOCATE(ONED2(NMAX))
      ALLOCATE(ONED3(NMAX))
      ALLOCATE(ONED4(NMAX))
      ALLOCATE(AVGERR(NSTA))
      ALLOCATE(AVGERR_T(NSTA))
      ALLOCATE(AVGERR_S(NSTA))

      DO I=1,NSTA
        IF(WL_FLAG(I).NE.0) THEN
          AVGERR(I)=0.0
          DO N=1,NTMAX_WL
            SWL_OBS(I,N)=0.0
          ENDDO
        ELSE
          BUFFER='After QC, NUMBER OF WL AT '//TRIM(NOS_ID(I))
          BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
          BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
          TMPTXT=' '
          WRITE(TMPTXT,'(I5)') NTR(I)
          BUFFER=TRIM(BUFFER)//trim(TMPTXT)
          WRITE(*,*) trim(BUFFER)
          WRITE(ICORMS,'(A)') trim(BUFFER)
          IF(NTR(I).LE.20) THEN
C   Here 20 can be changed depending upon how many data are required
            IBKP=BACKUP_SID(I)
            IF(IBKP.GT.0) THEN
              IF(NTR(IBKP).GT.20) THEN
                WRITE(*,*) 'Using backup ',IBKP, ' for I= ',I
                NTR(I)=NTR(IBKP)
                DO N=1,NTR(I)
                  RTIME(I,N)=RTIME(IBKP,N)
                  WL_OBS(I,N)=AS(IBKP)*WL_OBS(IBKP,N)
                  IF((TRIM(NOS_ID(I)).EQ.'11337190').OR.
     &   	     (TRIM(NOS_ID(I)).EQ.'11455420').OR.
     &   	     (TRIM(NOS_ID(I)).EQ.'07381600').OR.
     &   	     (TRIM(NOS_ID(I)).EQ.'07374000')) THEN
                    SWL_OBS(I,N)=AS(IBKP)*SWL_OBS(IBKP,N)  
C   +0.22   offset adjustment for Port Chicago
                  ELSE
                    SWL_OBS(I,N)=AS(IBKP)*SWL_OBS(IBKP,N)
                  ENDIF
                ENDDO  
              ELSE
                IBKP1=BACKUP_SID(IBKP)
                IF(IBKP1.GT.0) THEN
                  IF(NTR(IBKP1).GT.20) THEN
                    WRITE(*,*) 'Use second backup ',IBKP, ' for I= ',I
                    NTR(I)=NTR(IBKP1)
                    DO N=1,NTR(I)
                      RTIME(I,N)=RTIME(IBKP1,N)
                      WL_OBS(I,N)=AS(IBKP1)*WL_OBS(IBKP1,N)
                      SWL_OBS(I,N)=AS(IBKP1)*SWL_OBS(IBKP1,N)
                    ENDDO
                  ENDIF
                ENDIF     	   
              ENDIF
            ENDIF

C  For USGS gauges of 11337190 and 11455420, use climatological 
C  nontidal water levels AJ 03/01/2013	      
            IF(NTR(I).LE.20) THEN
              NREC=NINT( (JDAYE-JDAYS)*24/DELT)+1
              IF(ALLOCATED(ONED1)) DEALLOCATE(ONED1)
              IF(ALLOCATED(ONED2)) DEALLOCATE(ONED2)
              IF(ALLOCATED(ONED3)) DEALLOCATE(ONED3)
              IF(ALLOCATED(ONED4)) DEALLOCATE(ONED4)
              ALLOCATE(ONED1(NMAX))
              ALLOCATE(ONED2(NMAX))
              ALLOCATE(ONED3(NMAX))
              ALLOCATE(ONED4(NMAX))

              IF((TRIM(NOS_ID(I)).EQ.'11337190').OR.
     &           (TRIM(NOS_ID(I)).EQ.'11455420').OR.
     &           (TRIM(NOS_ID(I)).EQ.'07381600').OR.
     &           (TRIM(NOS_ID(I)).EQ.'07374000')) THEN
                BUFFER1=trim(PREFIXNOS)//'.obc.clim.ts.'
                BUFFER=TRIM(BUFFER1)//TRIM(NOS_ID(I))//'.dat'
                FIN=TRIM(BUFFER)
                WRITE(*,*) 'FIN= ',TRIM(FIN)
                INQUIRE(FILE=TRIM(FIN),EXIST=FEXIST)
                YEARB=IYRS
                MONTHB=1.0
                DAYB=1.0
                HOURB=0
                JDAY0=JULIAN(YEARB,MONTHB,DAYB,HOURB)
                CLOSE(10)
                OPEN(10,FILE=TRIM(FIN),STATUS='OLD')
                READ(10,*) K_climtmp
                READ(10,*) (DEPTH_clim(N,K),K=1,K_climtmp)
                READ(10,*)
                ICOUNT=0
                DO
618              READ(10,*,END=620) DUMMY,MON,IDD,
     &            (ONED1(K),ONED2(K),K=1,K_CLIMTMP),DUMMY1
                 ICOUNT=ICOUNT+1
                 DAY0=JDAY0+DUMMY-JBASE_DATE
                 IF((ICOUNT.EQ.1).AND.(DAY0.GT.DAY_START)) THEN
                  ONED3(ICOUNT)=DAY_START
                  ONED4(ICOUNT)=DUMMY1
                  ICOUNT=ICOUNT+1
                 ENDIF
                 ONED3(ICOUNT)=DAY0
                 ONED4(ICOUNT)=DUMMY1
                ENDDO 
620             CONTINUE
                CLOSE(10)
 
                IF(ONED3(ICOUNT).LT.day_end) then
                  ICOUNT=ICOUNT+1
                  ONED3(ICOUNT)=day_end
                  ONED4(ICOUNT)=ONED4(ICOUNT-1)
                ENDIF

                IF(ALLOCATED(TMPP1)) DEALLOCATE(TMPP1)
                IF(ALLOCATED(TMPP2)) DEALLOCATE(TMPP2)
                ALLOCATE(TMPP1(1:NREC)); TMPP1=0.0
                ALLOCATE(TMPP2(1:NREC)); TMPP2=0.0
                IF(ALLOCATED(TMPP3)) DEALLOCATE(TMPP3)
                IF(ALLOCATED(TMPP4)) DEALLOCATE(TMPP4)
                ALLOCATE(TMPP3(1:ICOUNT)); TMPP3=0.0
                ALLOCATE(TMPP4(1:ICOUNT)); TMPP4=0.0
                DO N=1,NREC
                  TMPP1(N)=TIME_M(N)
                END DO
                DO N=1,ICOUNT
                  TMPP3(N)=ONED3(N)
                  TMPP4(N)=ONED4(N)
                END DO
                CALL lineararray(NREC,TMPP1,TMPP2,ICOUNT,TMPP3,TMPP4)
                NTR(I)=NREC
                DO N=1,NTR(I)
                  RTIME(I,N)=TIME_M(N)
C                  SWL_OBS(I,N)=ONED4(N)
                  SWL_OBS(I,N)=TMPP2(N)
                ENDDO
                DEALLOCATE(TMPP1,TMPP2,TMPP3,TMPP4)
              ENDIF
            ENDIF  
          ELSE
            WRITE(*,*) 'NTR(I)=',NTR(I),I
          ENDIF  

C-----------------------------------------------------------------------
C   Use 6-hour ramping up	
C   Interpolate into same time intreval as zeta_time  
C   Filling missing values using linear interplolation 
C-----------------------------------------------------------------------
          FOUT=TRIM(NOS_ID(I))//'.swl'
          CLOSE(10)
          OPEN(10,FILE=TRIM(FOUT),STATUS='UNKNOWN')
          DO N=1,NTR(I)
            WRITE(10,35) RTIME(I,N),SWL_OBS(I,N)
          ENDDO
          CLOSE(10)

C-----------------------------------------------------------------------
C   Separate bias as mean error + time varying difference
C-----------------------------------------------------------------------
          N0=0
          DO N1=1,NTR(I)
            IF(SWL_OBS(I,N1).GT.-10.0) THEN
              N0=N0+1
              ONED1(N0)=RTIME(I,N1)
              ONED2(N0)=SWL_OBS(I,N1)
            ENDIF
          ENDDO

          NTR(I)=N0
          DO N1=1,NTR(I)
            RTIME(I,N1)=ONED1(N1)
            SWL_OBS(I,N1)=ONED2(N1)
          ENDDO

          TIME1=RTIME(I,1)
          TIME2=RTIME(I,NTR(I))
          N0=0
          WRITE(*,*) 'I=',I,TRIM(NOS_ID(I)),NTR(I)
          WRITE(*,*) 'TIME1=',TIME1,' TIME2=',TIME2,ZETA_TIME(1),
     &	    ZETA_TIME(NTMAX_WL),' NTR=',NTR(I),NTMAX_WL

          N0=0
          DO N1=1,NTMAX_WL
            IF((ZETA_TIME(N1).GE.TIME1).AND.
     &	       (ZETA_TIME(N1).LE.TIME2)) THEN
              N0=N0+1
              ONED3(N0)=ZETA_TIME(N1)
            ENDIF
          ENDDO

          IF(N0.LT.5) THEN
            WRITE(*,*)' Number of real time data: N0=',N0
            WRITE(*,*) 'WARNING MESSAGE: Real time temperature data is insufficient'
            WRITE(*,*) 'Please wait for 20 minutes and then check'
            WRITE(*,*) 'File size of ',TRIM(NOSBUFR), ' and ',TRIM(USGSBUFR)
            WRITE(*,*) 'Rerun prep job' 
            WRITE(*,*) 'TIME1=',TIME1,' TIME2=',TIME2,ZETA_TIME(1),
     &	       ZETA_TIME(NTMAX_WL),' NTR=',NTR(I),NTMAX_WL
            WRITE(*,*) 'OBC file is not adjusted using real time data'
            AVGERR(I)=0.0
            DO N=1,NTMAX_WL
              SWL_OBS(I,N)=0.0
            ENDDO
          ELSE
            NTMP=0 
            DO N=1,NTR(I)
C  Added by Zheng on 08/05/2019
              WL_MAX=3.0
              IF(TRIM(NOS_ID(I)).EQ.'07374000') THEN
                WL_MAX=20.0
              END IF
C  End by Zheng adding
              IF(ABS(SWL_OBS(I,N)).LE.WL_MAX) THEN
                NTMP=NTMP+1
                ONED1(NTMP)=RTIME(I,N)
                ONED2(NTMP)=SWL_OBS(I,N)
              ENDIF  
            ENDDO  

            IF(NTMP.GT.2) THEN
              if(allocated(tmpp1)) deallocate(tmpp1)
              if(allocated(tmpp2)) deallocate(tmpp2)
              allocate(tmpp1(1:N0)); tmpp1=0.0
              allocate(tmpp2(1:N0)); tmpp2=0.0
              if(allocated(tmpp3)) deallocate(tmpp3)
              if(allocated(tmpp4)) deallocate(tmpp4)
              allocate(tmpp3(1:NTMP)); tmpp3=0.0
              allocate(tmpp4(1:NTMP)); tmpp4=0.0
              DO N=1,N0
                tmpp1(N)=ONED3(N)
              End do
              DO N=1,NTMP
                tmpp3(N)=ONED1(N)
                tmpp4(N)=ONED2(N)
              End do
C              CALL LINEARARRAY(N0,ONED3,ONED4,NTMP,ONED1,ONED2)
              CALL LINEARARRAY(N0,TMPP1,TMPP2,NTMP,TMPP3,TMPP4)
              DO N=1,N0
                oned4(N)=tmpp2(N)
              END DO
              deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
            ELSE
              DO N=1,N0
                ONED4(N)=0.0
              ENDDO
            ENDIF  	    

            AVG=0.0
            DO N=1,N0
              DO N1=1,NTMAX_WL
                IF(ONED3(N).EQ.zeta_time(N1)) THEN
                  ONED1(N)=ONED4(N)-WLOBC(GRIDID_STA(I),N1)
                  AVG=AVG+ONED1(N)
                  EXIT 
                ENDIF	  
              ENDDO
905           CONTINUE
            ENDDO

            IF(N0.GT.0) AVG=AVG/N0
            AVGERR(I)=AVG
            WRITE(*,*) 'mean diff. of obs and model WL OBC=',I,
     &        AVGERR(I),N0
            DO N=1,N0
              ONED4(N)=ONED1(N)-AVG
            ENDDO  
            IF(ONED3(N0).LT.DAY_END) THEN
              N0=N0+1
              ONED3(N0)=ONED3(N0-1)+6.0/24.0
C   Use 6-hour ramping up from last err' to zero	
              ONED4(N0)=0.0
            ENDIF

            if(allocated(tmpp1)) deallocate(tmpp1)
            if(allocated(tmpp2)) deallocate(tmpp2)
            allocate(tmpp1(1:NTMAX_WL)); tmpp1=0.0
            allocate(tmpp2(1:NTMAX_WL)); tmpp2=0.0
            if(allocated(tmpp3)) deallocate(tmpp3)
            if(allocated(tmpp4)) deallocate(tmpp4)
            allocate(tmpp3(1:N0)); tmpp3=0.0
            allocate(tmpp4(1:N0)); tmpp4=0.0
            DO N=1,NTMAX_WL
              tmpp1(N)=zeta_time(N)
            End do
            DO N=1,N0
              tmpp3(N)=ONED3(N)
              tmpp4(N)=ONED4(N)
            End do
C            CALL LINEARARRAY(NTMAX_WL,ZETA_TIME,ONED1,N0,ONED3,ONED4)
            CALL LINEARARRAY(NTMAX_WL,TMPP1,TMPP2,N0,TMPP3,TMPP4)
            DO N=1,NTMAX_WL
              SWL_OBS(I,N)=TMPP2(N)
              IF(ZETA_TIME(N).GT.ONED3(N0)) SWL_OBS(I,N)=0.0 
            ENDDO
            deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
          ENDIF	 
        ENDIF

C-----------------------------------------------------------------------
C   Process water temperature for stations TS_FLAG > 0
C-----------------------------------------------------------------------
        IF(TS_FLAG(I).EQ.0) THEN   !! No correction is needed
          AVGERR_T(I)=0.0
          DO N=1,NTMAX
            T_OBS(I,N)=0.0
          ENDDO
        ELSEIF(TS_FLAG(I).EQ.1) THEN !! need real time T & S observations
          IF(NTR_T(I).GT.5) THEN
C   5 can be changed depending upon how many data are required
            BUFFER='NUMBER OF TEMP AT '//TRIM(NOS_ID(I))
            BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
            BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
            TMPTXT=' '
            WRITE(TMPTXT,'(I5)') NTR_T(I)
            BUFFER=TRIM(BUFFER)//TRIM(TMPTXT)
            WRITE(ICORMS,'(A)') TRIM(BUFFER)
          ELSE  
            IBKP=BACKUP_SID(I)
            IF(IBKP.GT.0) THEN
              IF(NTR_T(IBKP).GT.5) THEN
                BUFFER='USING BACKUP STATION OF '//TRIM(NOS_ID(IBKP))
                WRITE(ICORMS,'(A)') TRIM(BUFFER)
                WRITE(*,*) 'Using first backup ',IBKP,' for T at I= ',I
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
                    WRITE(ICORMS,'(A)') TRIM(BUFFER)
                    WRITE(*,*) 'Use second backup ',IBKP, ' for I= ',I
                    NTR_T(I)=NTR_T(IBKP1)
                    DO N=1,NTR_T(I)
                      RTIME_T(I,N)=RTIME_T(IBKP1,N)
                      T_OBS(I,N)=T_OBS(IBKP1,N)
                    ENDDO
                  ENDIF
                ELSE   
                  WRITE(*,*) 'WARNING MESSAGE: No real time temp observation is at second backup'
                  WRITE(*,*) 'Use climatology for ',trim(NOS_ID(I))
                  BUFFER='NO OBSERVATIONS, USE CLIMATOLOGIC DATASET'
                  WRITE(ICORMS,'(A)') TRIM(BUFFER)

                  NTR_T(I)=NT_CLIM
                  DO N=1,NTR_T(I)
                    RTIME_T(I,N)=TIME_CLIM(N)
                    T_OBS(I,N)=T_CLIM(I,N,1)    !! use SST only
                  ENDDO
                ENDIF     	   
              ENDIF
            ELSE     !! Use climatology 
               WRITE(*,*) 'WARNING MESSAGE: No real time temp observation'
              WRITE(*,*) 'Use climatology for I= ',I,trim(NOS_ID(I))
              NTR_T(I)=NT_CLIM
              DO N=1,NTR_T(I)
                RTIME_T(I,N)=TIME_CLIM(N)
                T_OBS(I,N)=T_CLIM(I,N,1)    !! use SST only
              ENDDO
            ENDIF     	   
          ENDIF
        ELSEIF(TS_FLAG(I).EQ.2) THEN !! Correction using climatological dataset
          BUFFER='USE CLIMATOLOGIC DATASET'
          WRITE(ICORMS,'(A)') TRIM(BUFFER)
          NTR_T(I)=NT_CLIM
          DO N=1,NTR_T(I)
            RTIME_T(I,N)=TIME_CLIM(N)
            T_OBS(I,N)=T_CLIM(I,N,1)    !! use SST only
          ENDDO
        ENDIF

        IF(TS_FLAG(I).GT.0) THEN     !! correction is needed
          FOUT=TRIM(NOS_ID(I))//'.temp'
          CLOSE(10)
          OPEN(10,file=TRIM(FOUT),STATUS='UNKNOWN')
          DO N=1,NTR_T(I)
            WRITE(10,35) RTIME_T(I,N),T_OBS(I,N)
          ENDDO
          CLOSE(10)

C-----------------------------------------------------------------------
C   Separate bias as mean error + time varying difference
C-----------------------------------------------------------------------
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
          WRITE(*,*) 'N0=',N0,' NTR_T(I)=',NTR_T(I)

          DO N1=2,NTMAX
            IF((TS_time(N1).GE.TIME2)) THEN
              N0=N0+1
              ONED1(N0)=TS_time(N1)
              ONED2(N0)=T_OBS(I,NTR_T(I))
              EXIT 
            ENDIF
          ENDDO
923       CONTINUE
          WRITE(*,*) 'N0=',N0,' NTR_T(I)=',NTR_T(I)

          NTR_T(I)=N0
          DO N1=1,NTR_T(I)
            RTIME_T(I,N1)=ONED1(N1)
            T_OBS(I,N1)=ONED2(N1)
          ENDDO     
          TIME1=RTIME_T(I,1)
          TIME2=RTIME_T(I,NTR_T(I))
          WRITE(*,*) 'TIME1=',TIME1,' TIME2=',TIME2,TS_time(1),
     &	    TS_time(NTMAX),' NTR_T=',NTR_T(I),NTMAX
	     
          N0=0
          DO N1=1,NTMAX
            IF((TS_time(N1).GE.TIME1).AND.
     &	       (TS_time(N1).LE.TIME2)) THEN
              N0=N0+1
              ONED3(N0)=TS_time(N1)
            ENDIF
          ENDDO

          IF(N0.LT.1) THEN
            WRITE(*,*) 'WARNING MESSAGE: Real time temperature data is insufficient'
            WRITE(*,*) 'Please wait for 20 minutes and then check'
            WRITE(*,*) 'File size of ', TRIM(NOSBUFR),' and ',  TRIM(USGSBUFR)
            WRITE(*,*) 'Rerun prep job' 
            WRITE(*,*) 'I=',I,TIME1,TIME2,TS_time(1),TS_time(NTMAX)
          ENDIF  

          NNTMP=NTR_T(I)
          DO N=1,NNTMP
            ONED1(N)=RTIME_T(I,N)
            ONED2(N)=T_OBS(I,N)
          ENDDO  
          if(allocated(tmpp1)) deallocate(tmpp1)
          if(allocated(tmpp2)) deallocate(tmpp2)
          allocate(tmpp1(1:N0)); tmpp1=0.0
          allocate(tmpp2(1:N0)); tmpp2=0.0
          if(allocated(tmpp3)) deallocate(tmpp3)
          if(allocated(tmpp4)) deallocate(tmpp4)
          allocate(tmpp3(1:NNTMP)); tmpp3=0.0
          allocate(tmpp4(1:NNTMP)); tmpp4=0.0
          dO N=1,N0
            tmpp1(N)=ONED3(N)
          End do
          DO N=1,NNTMP
            tmpp3(N)=ONED1(N)
            tmpp4(N)=ONED2(N)
          End do
C          CALL LINEARARRAY(N0,ONED3,ONED4,NTR_T(I),ONED1,ONED2)
          CALL lineararray(N0,tmpp1,tmpp2,NNTMP,tmpp3,tmpp4)
          DO N=1,N0
            ONED4(N)=tmpp2(N)
          END DO
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)

          AVG=0.0
          DO N=1,N0
            DO N1=1,NTMAX
              IF(ABS(ONED3(N)-TS_time(N1)).LT.1.0e-10) THEN
                ONED1(N)=ONED4(N)-TEMPOBC_M(GRIDID_STA(I),1,N1)! avg=Tobs - NCOM surface at the corresponding grid
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
          IF(ONED3(N0).LT.DAY_END) THEN
            N0=N0+1
            ONED3(N0)=ONED3(N0-1)+6.0/24.0
C  Use 6-hour ramping up from last err' to zero	
            ONED4(N0)=0.0
          ENDIF
          if(allocated(tmpp1)) deallocate(tmpp1)
          if(allocated(tmpp2)) deallocate(tmpp2)
          allocate(tmpp1(1:NTMAX)); tmpp1=0.0
          allocate(tmpp2(1:NTMAX)); tmpp2=0.0
          if(allocated(tmpp3)) deallocate(tmpp3)
          if(allocated(tmpp4)) deallocate(tmpp4)
          allocate(tmpp3(1:N0)); tmpp3=0.0
          allocate(tmpp4(1:N0)); tmpp4=0.0
          DO N=1,NTMAX
            tmpp1(N)=TS_time(N)
          End do
          DO N=1,N0
            tmpp3(N)=ONED3(N)
            tmpp4(N)=ONED4(N)
          End do
          CALL lineararray(NTMAX,tmpp1,tmpp2,N0,tmpp3,tmpp4)
C          CALL LINEARARRAY(NTMAX,TS_TIME,ONED1,N0,ONED3,ONED4)

          DO N=1,NTMAX
            ONED1(N)=tmpp2(N)
            T_OBS(I,N)=oned1(N)
            IF(TS_time(N).GT.ONED3(N0)) T_OBS(I,N)=0.0 
          ENDDO
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
        ENDIF  

C-----------------------------------------------------------------------
C   Process salinity for stations TS_FLAG > 0
C-----------------------------------------------------------------------
        IF(TS_FLAG(I).EQ.0) THEN   !! No correction is needed
          AVGERR_S(I)=0.0
          DO N=1,NTMAX
            S_OBS(I,N)=0.0
          ENDDO
        ELSEIF(TS_FLAG(I).EQ.1) THEN !! Need real time T & S observations
          IF(NTR_S(I).GT.5) THEN
C   2 can be changed depending upon how many data are required to be available
            BUFFER='NUMBER OF SALT AT '//TRIM(NOS_ID(I))
            BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
            BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
            TMPTXT=' '
            WRITE(TMPTXT,'(I5)') NTR_S(I)
            BUFFER=TRIM(BUFFER)//TRIM(TMPTXT)
            WRITE(ICORMS,'(A)') TRIM(BUFFER)
          ELSE
            IBKP=BACKUP_SID(I)
            IF(IBKP.GT.0) THEN
              IF(NTR_S(IBKP).GT.5) THEN
                BUFFER='USING BACKUP STATION OF '//TRIM(NOS_ID(IBKP))
                WRITE(ICORMS,'(A)') TRIM(BUFFER)
                WRITE(*,*) 'Using backup ',IBKP, ' for SAL at I= ',I
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
                    WRITE(ICORMS,'(A)') TRIM(BUFFER)
                    WRITE(*,*) 'Use second backup ',IBKP, ' for I= ',I
                    NTR_S(I)=NTR_S(IBKP1)
                    DO N=1,NTR_S(I)
                      RTIME_S(I,N)=RTIME_S(IBKP1,N)
                      S_OBS(I,N)=S_OBS(IBKP1,N)
                    ENDDO
                  ENDIF
                ELSE 
                  WRITE(*,*) 'WARNING MESSAGE: No real time sal observation'
                  WRITE(*,*) 'Use sal climatology for ',TRIM(NOS_ID(I))
                  NTR_S(I)=NT_CLIM
                  DO N=1,NTR_S(I)
                    RTIME_S(I,N)=TIME_CLIM(N)
                    S_OBS(I,N)=S_CLIM(I,N,1)    !! use SST only
                  ENDDO
                ENDIF     	   
              ENDIF
            ELSE     !! Use climatology 
              WRITE(*,*) 'WARNING MESSAGE: No real time sal observation'
	      WRITE(*,*) 'Use sal climatology for I= ',I,
     &          TRIM(NOS_ID(I))
              NTR_S(I)=NT_CLIM
              DO N=1,NTR_S(I)
                RTIME_S(I,N)=TIME_CLIM(N)
                S_OBS(I,N)=S_CLIM(I,N,1)    !! use SST only
              ENDDO
            ENDIF     	   
          ENDIF
        ELSEIF(TS_FLAG(I).EQ.2) THEN !! Correction using climatological dataset
          NTR_S(I)=NT_CLIM
          DO N=1,NTR_S(I)
            RTIME_S(I,N)=TIME_CLIM(N)
            S_OBS(I,N)=S_CLIM(I,N,1)    !! use SST only
          ENDDO
        ENDIF
	    
        IF(TS_FLAG(I).GT.0) THEN !!  Correction is needed
          FOUT=TRIM(NOS_ID(I))//'.salt'
          CLOSE(10)
          OPEN(10,FILE=TRIM(FOUT),STATUS='UNKNOWN')
          DO N=1,NTR_S(I)
            WRITE(10,35) RTIME_S(I,N),S_OBS(I,N)
          ENDDO
          CLOSE(10)

C-----------------------------------------------------------------------
C  Separate bias as mean error + time varying difference
C-----------------------------------------------------------------------
          N0=0
          DO N1=1,NTR_S(I)
            IF(S_OBS(I,N1).GT.0.0) THEN
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

          DO N1=2,NTMAX
            IF((TS_time(N1).GE.TIME2)) THEN
              N0=N0+1
              ONED1(N0)=TS_time(N1)
              ONED2(N0)=S_OBS(I,NTR_S(I))
              EXIT
            ENDIF
          ENDDO
          NTR_S(I)=N0

          DO N1=1,NTR_S(I)
            RTIME_S(I,N1)=ONED1(N1)
            S_OBS(I,N1)=ONED2(N1)
          ENDDO     
          TIME1=RTIME_S(I,1)
          TIME2=RTIME_S(I,NTR_S(I))
          WRITE(*,*) 'TIME1=',TIME1,' TIME2=',TIME2,TS_time(1),
     &	    TS_time(NTMAX),' NTR_S=',NTR_S(I),NTMAX
          N0=0
          DO N1=1,NTMAX
            IF((TS_time(N1).GE.TIME1).AND.
     &	       (TS_time(N1).LE.TIME2)) THEN
              N0=N0+1
              ONED3(N0)=TS_time(N1)
            ENDIF
          ENDDO

          IF(N0.LT.1) THEN
            WRITE(*,*) 'WARNING MESSAGE:Real time salinity data is insufficient'
            WRITE(*,*) 'Please wait for 20 minutes and then check'
            WRITE(*,*) 'File size of ', TRIM(NOSBUFR), ' and ', TRIM(USGSBUFR)
            WRITE(*,*) 'Rerun prep job' 
            WRITE(*,*) 'I=',I,TIME1,TIME2,TS_time(1),TS_time(NTMAX)
          ENDIF  

          NNTMP=NTR_S(I)
          DO N=1,NNTMP
            ONED1(N)=RTIME_S(I,N)
            ONED2(N)=S_OBS(I,N)
          ENDDO  
          if(allocated(tmpp1)) deallocate(tmpp1)
          if(allocated(tmpp2)) deallocate(tmpp2)
          allocate(tmpp1(1:N0)); tmpp1=0.0
          allocate(tmpp2(1:N0)); tmpp2=0.0
          if(allocated(tmpp3)) deallocate(tmpp3)
          if(allocated(tmpp4)) deallocate(tmpp4)
          allocate(tmpp3(1:NNTMP)); tmpp3=0.0
          allocate(tmpp4(1:NNTMP)); tmpp4=0.0
          DO N=1,N0
            tmpp1(N)=ONED3(N)
          End do
          DO N=1,NNTMP
            tmpp3(N)=ONED1(N)
            tmpp4(N)=ONED2(N)
          End do
          CALL lineararray(N0,tmpp1,tmpp2,NNTMP,tmpp3,tmpp4)
C          CALL LINEARARRAY(N0,ONED3,ONED4,NTR_S(I),ONED1,ONED2)
          DO N=1,N0
            ONED4(N)=tmpp2(N)
          End do
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)

          AVG=0.0
          DO N=1,N0
            DO N1=1,NTMAX
              IF(ABS(ONED3(N)-TS_time(N1)).LT.1.0e-10) THEN
cc avg=Sobs - NCOM at the corresponding grid
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
            ONED4(N)=ONED1(N)-AVG   !! err = avg + err'
          ENDDO  

          IF(ONED3(N0).LT.day_end) THEN
            N0=N0+1
cc use 6-hour ramping up from last err' to zero
            ONED3(N0)=ONED3(N0-1)+6.0/24.0
            ONED4(N0)=0.0
          ENDIF
          if(allocated(tmpp1)) deallocate(tmpp1)
          if(allocated(tmpp2)) deallocate(tmpp2)
          allocate(tmpp1(1:NTMAX)); tmpp1=0.0
          allocate(tmpp2(1:NTMAX)); tmpp2=0.0
          if(allocated(tmpp3)) deallocate(tmpp3)
          if(allocated(tmpp4)) deallocate(tmpp4)
          allocate(tmpp3(1:N0)); tmpp3=0.0
          allocate(tmpp4(1:N0)); tmpp4=0.0
          DO N=1,NTMAX
            tmpp1(N)=TS_time(N)
          End do
          DO N=1,N0
            tmpp3(N)=ONED3(N)
            tmpp4(N)=ONED4(N)
          End do
          CALL lineararray(NTMAX,tmpp1,tmpp2,N0,tmpp3,tmpp4)
C          CALL LINEARARRAY(NTMAX,TS_TIME,ONED1,N0,ONED3,ONED4)
          DO N=1,NTMAX
            oned1(N)=tmpp2(N)
            S_OBS(I,N)=oned1(N)
            IF(TS_time(N).GT.ONED3(N0)) S_OBS(I,N)=0.0 
          ENDDO
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
        ENDIF  
      ENDDO

C-----------------------------------------------------------------------
C   Correcting OBC by the difference between obs - ETSS/NCOM
C-----------------------------------------------------------------------
      DO I=1,NOBC
        ID1=WL_SID_1(I)
        ID2=WL_SID_2(I)
        SC1=WL_S_1(I)
        SC2=WL_S_2(I)
        IF(WL_STA(I).EQ.1) THEN
          IF(ID1.GT.0) THEN
            DO N=1,NTMAX_WL
              WLOBC(I,N)=WLOBC(I,N)+SC1*(AVGERR(ID1)+SWL_OBS(ID1,N))
            ENDDO
          ENDIF 
        ELSEIF(WL_STA(I).EQ.2) THEN
          IF(ID1.GT.0.AND.ID2.GT.0) THEN
            DO N=1,NTMAX_WL
              WLOBC(I,N)=WLOBC(I,N)+SC1*(AVGERR(ID1)+SWL_OBS(ID1,N))
     &       	                   +SC2*(AVGERR(ID2)+SWL_OBS(ID2,N))
            ENDDO
          ENDIF 
        ENDIF
	 
        ID1=TS_SID_1(I)
        ID2=TS_SID_2(I)
        SC1=TS_S_1(I)
        SC2=TS_S_2(I)
        IF(TS_STA(I).EQ.1) THEN
         IF(ID1.GT.0) THEN
          DO N=1,NTMAX
           DO K=1,KBM
            TEMPOBC_M(I,K,N)=TEMPOBC_M(I,K,N)+
     &	      SC1*(AVGERR_T(ID1)+T_OBS(ID1,N))
cc Do not correct RTOFS salt OBC due to no obs. available near OB     
            IF((OFS(1:5).EQ.'ngofs').OR.(OFS(1:5).EQ.'NGOFS')) THEN
             AVGERR_S(ID1)=0.0
             S_OBS(ID1,N)=0.0
            ENDIF
            IF((TRIM(OFS).EQ.'sfbofs').OR.
     &         (TRIM(OFS).EQ.'SFBOFS')) THEN
             AVGERR_S(1)=0.0
             S_OBS(1,N)=0.0
            ENDIF
            SALTOBC_M(I,K,N)=SALTOBC_M(I,K,N)+
     &        SC1*(AVGERR_S(ID1)+S_OBS(ID1,N))
           ENDDO
          ENDDO
         ENDIF 
        ELSEIF(TS_STA(I).EQ.2) THEN
          IF(ID1.GT.0.AND.ID2.GT.0) THEN
            DO N=1,NTMAX
              DO K=1,KBm
                TEMPOBC_M(I,K,N)=TEMPOBC_M(I,K,N)+
     &	          SC1*(AVGERR_T(ID1)+T_OBS(ID1,N))+
     &            SC2*(AVGERR_T(ID2)+T_OBS(ID2,N))
                IF((OFS(1:5).EQ.'ngofs').OR.(OFS(1:5).EQ.'NGOFS')) THEN		
                  AVGERR_S(ID1)=0.0
                  S_OBS(ID1,N)=0.0
                  AVGERR_S(ID2)=0.0
                  S_OBS(ID2,N)=0.0
                ENDIF
                SALTOBC_M(I,K,N)=SALTOBC_M(I,K,N)+
     &            SC1*(AVGERR_S(ID1)+S_OBS(ID1,N))+
     &            SC2*(AVGERR_S(ID2)+S_OBS(ID2,N)) 
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

C -------------------------------------------------------------------
C   Print OBC for evaluation
      OPEN(33,FILE='WL_OBC_adjusted.dat',STATUS='UNKNOWN')
      DO N=1,NTMAX_WL
        WRITE(33,35) ZETA_TIME(N),(WLOBC(I,N),I=1,NOBC,10),
     &    WLOBC(224,N),WLOBC(NOBC,N)
      ENDDO
      CLOSE(33)

      OPEN(33,FILE='TEMP_OBC_adjusted.dat',STATUS='UNKNOWN')
      DO N=1,NTMAX
        DO I=1,NOBC
          WRITE(33,35) TS_TIME(N),(TEMPOBC_M(I,K,N),K=1,KBm)
        ENDDO
      ENDDO
      CLOSE(33)

      OPEN(33,FILE='SALT_OBC_adjusted.dat',STATUS='UNKNOWN')
      DO N=1,NTMAX
        DO I=1,NOBC
          WRITE(33,35) TS_TIME(N),(SALTOBC_M(I,K,N),K=1,KBm)
        ENDDO
      ENDDO
      CLOSE(33)

C-----------------------------------------------------------------------
C  Set global attributes string of the NetCDF
C-----------------------------------------------------------------------
1     FORMAT(I2.2,A1,I2.2,2X,I2.2,A1,I2.2,A1,I4)
      CALL DATE_AND_TIME(BIG_BEN(1),BIG_BEN(2),BIG_BEN(3),DATE_TIME)
      WRITE(CURRENT_TIME,1) DATE_TIME(5),':',DATE_TIME(6),
     &  DATE_TIME(2),'/',DATE_TIME(3),'/',DATE_TIME(1)
      globalstr(1)=TRIM(OCEAN_MODEL)//
     &  ' lateral open boundary forcing netCDF file'
      globalstr(2)=TRIM(OFS)//' lateral open boundary netCDF file'
      globalstr(3)='Water level OBC from '//TRIM(DBASE_WL)//
     &  ' data source'
      globalstr(4)='T and S OBC from '//TRIM(DBASE_TS)//
     &  ' data source'
      globalstr(5)=TRIM(DBASE_TS)//' data file: '//TRIM(FILE_TS)
      IF(IGRD_ORI.EQ.0) THEN
        globalstr(6)='On native '//TRIM(DBASE_TS)//
     &    ' grid, No spatial interpolation'
      ELSEIF(IGRD_ORI.EQ.1) THEN
        globalstr(6)='On '//TRIM(OCEAN_MODEL)//
     &    ' grid, using remesh spatial interpolation'
      ELSEIF(IGRD_ORI.EQ.2) THEN
        globalstr(6)='On '//trim(OCEAN_MODEL)//
     &    ' grid, using bicubic spatial interpolation'
      ELSEIF(IGRD_ORI.EQ.3) THEN
        globalstr(6)='On '//trim(OCEAN_MODEL)//
     &    ' grid, using bilinear spatial interpolation'
      ELSEIF(IGRD_ORI.EQ.4) THEN
        globalstr(6)='On '//trim(OCEAN_MODEL)//
     &    ' grid, using nature neighbors spatial interpolation'
      ENDIF
      globalstr(7)='GRID file: '//TRIM(GRIDFILE)      
      globalstr(8)='Created at time '//TRIM(CURRENT_TIME)
      globalstr(9)='Created by Aijun Zhang, OD/CO-OPS/NOS/NOAA'
	  
C-----------------------------------------------------------------------
C   Reading tide constituents along open boundary and conduct tidal prediction
C-----------------------------------------------------------------------
      INQUIRE(FILE=trim(HC_FILE_OFS),EXIST=FEXIST)
      IF(.NOT. FEXIST) THEN
        WRITE(*,*) 'Harmonic Constant NetCDF file is not found'
        WRITE(*,*) 'Provide correct Harmonic Constant File Name:'
        WRITE(*,*) TRIM(HC_FILE_OFS)
        WRITE(*,*) TRIM(OFS)//' stop here'
        WRITE(ICORMS,'(a)') 'CRITICAL FAILURE IN CREATING OBC' 
        STOP
      ELSE  
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        IF(ALLOCATED(constituents)) DEALLOCATE(constituents)
        STATUS=NF_OPEN(trim(HC_FILE_OFS),NF_NOWRITE,NCID)
        STATUS=NF_INQ(NCID,NDIMS,NVARS,NGATTS,UNLIMDIMID)

        STATUS=NF_INQ_VARID(NCID,'tide_names',IDVAR)
        STATUS=NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        STATUS=NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        DO I=1,NDIMS
          STATUS=NF_INQ_DIMLEN(NCID,DIMIDS(i),DIMS(i))
        ENDDO
        ALLOCATE(constituents(DIMS(1),DIMS(2)))
        STATUS=NF_GET_VAR_TEXT(NCID,IDVAR,constituents)

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        STATUS=NF_INQ_VARID(NCID,'tide_Eamp',IDVAR)
        STATUS=NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        STATUS=NF_INQ_VARDIMID(NCID,IDVAR,DIMIDS)
        DO I=1,NDIMS
          STATUS=NF_INQ_DIMLEN(NCID,DIMIDS(I),DIMS(I))
          WRITE(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
        ENDDO
 
        IF(ALLOCATED(TIDE_AMP)) DEALLOCATE(TIDE_AMP)
        IF(ALLOCATED(TIDE_EPOC)) DEALLOCATE(TIDE_EPOC)
        ALLOCATE(TIDE_AMP(DIMS(1),DIMS(2)))
        ALLOCATE(TIDE_EPOC(DIMS(1),DIMS(2)))
        STATUS=NF_INQ_VARID(NCID,'tide_Eamp',IDVAR)
        STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tide_amp)
	WRITE(*,*) 'min & max E_amp=',MINVAL(TIDE_AMP),
     &    MAXVAL(TIDE_AMP)
        STATUS=NF_INQ_VARID(NCID,'tide_Ephase',IDVAR)
        STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tide_epoc)
	WRITE(*,*) 'min & max E_pha=',MINVAL(TIDE_EPOC),
     &    MAXVAL(TIDE_EPOC)

C-------------------------------------------------------------------
C   Conduct tidal prediction at open boundary nodes
C-------------------------------------------------------------------
        IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
        IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
        ALLOCATE(ONED1(NMAX))
        ALLOCATE(ONED2(NMAX))

        KINDAT=2
        CONV=1.0
        XMAJOR=0.0
	FOUT='WL_PRED.out'
        NREC=NINT((JDAYE-JDAYS)*24/DELT)+1
        DO I=1,NOBC
	  WRITE(*,*) 'Tidal prediction at node= ',I,NREC
          DO K=1,37
	    AMP(K)=TIDE_AMP(I,K)
	    EPOC(K)=TIDE_EPOC(I,K)
          ENDDO
          FOUT='OBC.prd'
          CALL NOS_PRD(START_TIME,END_TIME,KINDAT,DELT,CONV,
     &      XMAJOR,AMP,EPOC,FOUT,ONED1,ONED2)
          DO N=1,NREC
            WLOBC(I,N)=WLOBC(I,N)+ONED1(N)   ! non-tide + tidal prediction
	  ENDDO
	  IF(I.EQ.1) THEN
            CLOSE(45)
            OPEN(45,FILE='node1.dat',STATUS='UNKNOWN')
            DO N=1,NREC
	      WRITE(45,'(3F12.5)') TIME_M(N),WLOBC(I,N),ONED1(N)
            ENDDO
          ELSEIF(I.EQ.NOBC) THEN
            CLOSE(45)
            OPEN(45,FILE='node_last.dat',STATUS='UNKNOWN')
            DO N=1,NREC
              WRITE(45,'(3F12.5)') Time_M(N),WLOBC(I,N),ONED1(N)
            ENDDO
          ENDIF
        ENDDO
        WRITE(*,*) 'WL OBC is done'   

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        STATUS=NF_INQ_VARID(NCID,'UA_amp',IDVAR)
        STATUS=NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        STATUS=NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        DO I=1,NDIMS
          STATUS=NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
          WRITE(*,*) TRIM(VNAME),' dim ',I,' = ',DIMS(I)
        ENDDO

        IF(ALLOCATED(TIDE_AMP)) DEALLOCATE(TIDE_AMP)
        IF(ALLOCATED(TIDE_EPOC)) DEALLOCATE(TIDE_EPOC)
        ALLOCATE(TIDE_AMP(DIMS(1),DIMS(2)))
        ALLOCATE(TIDE_EPOC(DIMS(1),DIMS(2)))
        STATUS=NF_INQ_VARID(NCID,'UA_amp',IDVAR)
        STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tide_amp)
        WRITE(*,*) 'min & max UA_amp=',MINVAL(TIDE_AMP),
     &    MAXVAL(TIDE_AMP)

        STATUS = NF_INQ_VARID(NCID,'UA_phase',IDVAR)
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tide_epoc)
        WRITE(*,*) 'min & max UA_pha=',MINVAL(TIDE_EPOC),
     &    MAXVAL(TIDE_EPOC)

        IF(ALLOCATED(ONED1)) DEALLOCATE(ONED1)
        IF(ALLOCATED(ONED2)) DEALLOCATE(ONED2)
        ALLOCATE(ONED1(NMAX))
        ALLOCATE(ONED2(NMAX))
        KINDAT=2
        CONV=1.0
        XMAJOR=0.0
        NREC=NINT((JDAYE-JDAYS)*24/DELT)+1
        DO I=1,NEOBC
          DO K=1,37
	    AMP(K)=TIDE_AMP(I,K)
	    EPOC(K)=TIDE_EPOC(I,K)
          ENDDO
          CALL NOS_PRD(START_TIME,END_TIME,KINDAT,DELT,CONV,
     &      XMAJOR,AMP,EPOC,FOUT,ONED1,ONED2)
          DO N=1,NREC
	    UBAROBC(I,N)=UBAROBC(I,N)+ONED1(N)
	  ENDDO
        ENDDO

        STATUS=NF_INQ_VARID(NCID,'VA_amp',IDVAR)
        STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tide_amp)
        WRITE(*,*) 'min & max VA_amp=',MINVAL(TIDE_AMP),
     &    MAXVAL(TIDE_AMP)

        STATUS=NF_INQ_VARID(NCID,'VA_phase',IDVAR)
        STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tide_epoc)
        WRITE(*,*) 'min & max VA_pha=',MINVAL(TIDE_EPOC),
     &    MAXVAL(TIDE_EPOC)
        KINDAT=2
        CONV=1.0
        XMAJOR=0.0
        DO I=1,NEOBC
          DO K=1,37
            AMP(K)=TIDE_AMP(I,K)
            EPOC(K)=TIDE_EPOC(I,K)
          ENDDO
          CALL NOS_PRD(START_TIME,END_TIME,KINDAT,DELT,CONV,
     &      XMAJOR,AMP,EPOC,FOUT,ONED1,ONED2)
          DO N=1,NREC
            VBAROBC(I,N)=VBAROBC(I,N)+ONED1(N)
          ENDDO
        ENDDO
	WRITE(*,*) 'UA & VA OBC is done'	   

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        IF(ALLOCATED(TMP3D)) DEALLOCATE(TMP3D)
        IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
	VNAME='U_amp'
        STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
        STATUS=NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        STATUS=NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        DO I=1,NDIMS
          STATUS=NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
          WRITE(*,*) TRIM(VNAME),' dim ',I,' = ',DIMS(I)
        ENDDO

        ALLOCATE(TMP3D(DIMS(1),DIMS(2),DIMS(3)))
        ALLOCATE(TMP4D(DIMS(1),DIMS(2),DIMS(3),2))
        STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
        STATUS=NF_GET_VAR_REAL(NCID,IDVAR,TMP3D)
        WRITE(*,*) 'min & max U_amp=',MINVAL(TMP3D),MAXVAL(TMP3D)
        DO I=1,DIMS(1)
          DO K=1,DIMS(2)
            DO N=1,DIMS(3)
              TMP4D(I,K,N,1)=TMP3D(I,K,N)
            ENDDO
          ENDDO
        ENDDO

        STATUS=NF_INQ_VARID(NCID,'U_phase',IDVAR)
        STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tmp3d)
        WRITE(*,*) 'min & max U_phase=',MINVAL(TMP3D),MAXVAL(TMP3D)
        DO I=1,DIMS(1)
          DO K=1,DIMS(2)
            DO N=1,DIMS(3)
              TMP4D(I,K,N,2)=TMP3D(I,K,N)
            ENDDO
          ENDDO
        ENDDO

        DO I=1,DIMS(1)
          DO K=1,DIMS(2)
            DO N=1,37
              AMP(N)=TMP4D(I,K,N,1)
              EPOC(N)=TMP4D(I,K,N,2)
            ENDDO
            CALL NOS_PRD(START_TIME,END_TIME,KINDAT,DELT,CONV,
     &        XMAJOR,AMP,EPOC,FOUT,ONED1,ONED2)
            DO N=1,NREC
              UEOBC_M(I,K,N)=UEOBC_M(I,K,N)+ONED1(N)
            ENDDO    
          ENDDO
        ENDDO
 
        STATUS=NF_INQ_VARID(NCID,'V_amp',IDVAR)
        STATUS=NF_GET_VAR_REAL(NCID,IDVAR,TMP3D)
	WRITE(*,*) 'min & max V_amp=',MINVAL(TMP3D),MAXVAL(TMP3D)
        DO I=1,DIMS(1)
          DO K=1,DIMS(2)
            DO N=1,DIMS(3)
              TMP4D(I,K,N,1)=TMP3D(I,K,N)
            ENDDO
          ENDDO
        ENDDO

        STATUS=NF_INQ_VARID(NCID,'V_phase',IDVAR)
        STATUS=NF_GET_VAR_REAL(NCID,IDVAR,TMP3D)
        WRITE(*,*) 'min & max V_phase=',MINVAL(TMP3D),MAXVAL(TMP3D)
        DO I=1,DIMS(1)
          DO K=1,DIMS(2)
            DO N=1,DIMS(3)
              TMP4D(I,K,N,2)=TMP3D(I,K,N)
            ENDDO
          ENDDO
        ENDDO

        DO I=1,DIMS(1)
          DO K=1,DIMS(2)
            DO N=1,37
              AMP(N)=tmp4d(I,K,N,1)
              EPOC(N)=tmp4d(I,K,N,2)
            ENDDO
            CALL NOS_PRD(START_TIME,END_TIME,KINDAT,DELT,CONV,
     &        XMAJOR,AMP,EPOC,FOUT,ONED1,ONED2)
            DO N=1,NREC
              VEOBC_M(I,K,N)=VEOBC_M(I,K,N)+ONED1(N)
            ENDDO     
          ENDDO
        ENDDO
        STATUS=NF_CLOSE(NCID)
      ENDIF
      WRITE(*,*) 'min & max WLOBC=',minval(WLOBC),maxval(WLOBC)
      WRITE(*,*) 'min & max T=',minval(tempOBC_M),maxval(tempOBC_M)
      WRITE(*,*) 'min & max S=',minval(saltOBC_M),maxval(saltOBC_M)
      WRITE(*,*) 'min & max U=',minval(ueOBC_M),maxval(ueOBC_M)
      WRITE(*,*) 'min & max V=',minval(veOBC_M),maxval(veOBC_M)
      WRITE(*,*) 'min & max UA=',minval(ubarOBC),maxval(ubarOBC)
      WRITE(*,*) 'min & max VA=',minval(vbarOBC),maxval(vbarOBC)
      WRITE(ICORMS,'(A16,2e16.8)') 'min & max WLOBC=',
     &  minval(WLOBC),maxval(WLOBC)
      WRITE(ICORMS,'(A12,2e16.8)') 'min & max T=',
     &  minval(tempOBC_M),maxval(tempOBC_M)
      WRITE(ICORMS,'(A12,2e16.8)') 'min & max S=',
     &  minval(saltOBC_M),maxval(saltOBC_M)
      WRITE(ICORMS,'(A12,2e16.8)') 'min & max U=',
     &  minval(ueOBC_M),maxval(ueOBC_M)
      WRITE(ICORMS,'(A12,2e16.8)') 'min & max V=',
     &  minval(veOBC_M),maxval(veOBC_M)
      WRITE(ICORMS,'(A13,2e16.8)') 'min & max UA=',
     &  minval(ubarOBC),maxval(ubarOBC)
      WRITE(ICORMS,'(A13,2e16.8)') 'min & max VA=',
     &  minval(vbarOBC),maxval(vbarOBC)
      
      WRITE(*,*) 'Write out netcdf'
      IF(TRIM(OCEAN_MODEL).EQ.'FVCOM') THEN
        NREC=NINT((JDAYE-JDAYS)*24/DELT)+1
        IF(ALLOCATED(TIME_M)) DEALLOCATE(TIME_M)
        IF(ALLOCATED(TIMES)) DEALLOCATE(TIMES)
        IF(ALLOCATED(ITIME)) DEALLOCATE(ITIME)
        IF(ALLOCATED(ITIME2)) DEALLOCATE(ITIME2)
        ALLOCATE(TIME_M(NREC))
        ALLOCATE(TIMES(NREC))
        ALLOCATE(ITIME(NREC))
        ALLOCATE(ITIME2(NREC))

        DO N=1,NREC
	  TIME_M(N)=(JDAYS-JBASE_DATE)+(N-1)*DELT/24.0
          JDAY=JDAYS+(N-1)*DELT/24.0
          CALL GREGORIAN(JDAY,YEARB,MONTHB,DAYB,HOURB)
          IYR=INT(YEARB)
          IMM=INT(MONTHB+0.001)
          IDD=INT(DAYB+0.001)
          IHH=INT(HOURB+0.001)
          IMN=INT((HOURB-IHH)*60+0.1)
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
            IF(MOD(IYR,4).EQ.0) DAYS_PER_MONTH(2)=29
            IF(IDD.GT.DAYS_PER_MONTH(IMM)) THEN
              IDD=IDD-DAYS_PER_MONTH(IMM)
              IMM=IMM+1
              IF(IMM.GT.12) THEN
                IMM=IMM-12
                IYR=IYR+1
              ENDIF
            ENDIF
          ENDIF
          WRITE(BUFFER,810)IYR,'-',IMM,'-',IDD,'T',IHH,':',IMN,
     &	    ':00.000000'  
810       FORMAT(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A10)
          TIMES(N)=TRIM(ADJUSTL(BUFFER))
          ITIME(N)=INT(TIME_M(N)+0.01)
          ITIME2(N)=INT((TIME_M(N)-ITIME(N))*86400)*1000
C          WRITE(*,*) 'Time=',N,ITIME(N),ITIME2(N),TIMES(N)
        ENDDO

cc  Final check if there is alnormal values or NaN in the OBC variables
cc  Check WLOBC
        DO I=1,NOBC
          DO N=1,NTMAX_WL
            IF((ABS(WLOBC(I,N)).GE.99.0).OR.
     &         (IEEE_IS_NAN(WLOBC(I,N)))) THEN
              WRITE(*,*) 'FATAL ERROR: Abnormal values ',
     &          'in WLOBC at I',I,'N= ',N 
              STOP
            ENDIF
          ENDDO
        ENDDO

C  check ubarOBC
        DO I=1,NEOBC
          DO N=1,NTMAX
            IF((ABS(UBAROBC(I,N)).GE.99.0).OR.
     &         (IEEE_IS_NAN(UBAROBC(I,N)))) THEN
C              WRITE(*,*) 'FATAL ERROR: Abnormal values ',
C     &          'in ubarOBC at I',I,'N= ',N
              UBAROBC(I,N)=0.0
            ENDIF
          ENDDO
        ENDDO

C  check vbarOBC
        DO I=1,NEOBC
          DO N=1,NTMAX
            IF((ABS(VBAROBC(I,N)).GE.99.0).OR.
     &         (IEEE_IS_NAN(VBAROBC(I,N)))) THEN
C              WRITE(*,*) 'FATAL ERROR: Abnormal values ',
C     &          'in vbarOBC at I',I,'N= ',N 
              VBAROBC(I,N)=0.0
            ENDIF
          ENDDO
        ENDDO

C Check TEMPOBC_M at surface K=1
        K=1
        DO I=1,NOBC
          DO N=1,NTMAX
            IF((ABS(TEMPOBC_M(I,K,N)).GE.99.0).OR.
     &         (IEEE_IS_NAN(TEMPOBC_M(I,K,N)))) THEN
              WRITE(6,*) 'FATAL ERROR: Abnormal values ',
     &          'in TEMPOBC at I',I,'N= ',N 
              STOP
            ENDIF
          ENDDO
        ENDDO

        DO I=1,NOBC
          DO K=2,KBm
            DO N=1,NTMAX
              IF((ABS(TEMPOBC_M(I,K,N)).GE.99.0).OR.
     &           (IEEE_IS_NAN(TEMPOBC_M(I,K,N)))) THEN
C                WRITE(*,*) 'FATAL ERROR: Abnormal values ',
C     &            'in TEMPOBC at I',I,'N= ',N,'K= ',K
C                WRITE(*,*)'Replace using good value at the above layer'
                TEMPOBC_M(I,K,N)=TEMPOBC_M(I,K-1,N)
              ENDIF
            ENDDO
          ENDDO
        ENDDO

C Check SALTOBC_M at surface K=1
        K=1
        DO I=1,NOBC
          DO N=1,NTMAX
            IF((ABS(SALTOBC_M(I,K,N)).GE.99.0).OR.
     &         (IEEE_IS_NAN(SALTOBC_M(I,K,N)))) THEN
              WRITE(*,*) 'FATAL ERROR: Abnormal values ',
     &          'in SALTOBC at I',I,'N= ',N 
              STOP
            ENDIF
          ENDDO
        ENDDO

        DO I=1,NOBC
          DO K=2,KBm
            DO N=1,NTMAX
              IF((ABS(SALTOBC_M(I,K,N)).GE.99.0).OR.
     &           (IEEE_IS_NAN(SALTOBC_M(I,K,N)))) THEN
C                WRITE(*,*) 'FATAL ERROR: Abnormal values ',
C     &            'in SALTOBC at I',I,'N= ',N,'K= ',K
C                WRITE(*,*)'Replace using good value at the above layer'
                SALTOBC_M(I,K,N)=SALTOBC_M(I,K-1,N)
              ENDIF
            ENDDO
          ENDDO
        ENDDO

C Check UeOBC_M at surface K=1
        K=1
        DO I=1,NEOBC
          DO N=1,NTMAX
            IF((ABS(UEOBC_M(I,K,N)).GE.99.0).OR.
     &         (IEEE_IS_NAN(UEOBC_M(I,K,N)))) THEN
C              WRITE(*,*) 'FATAL ERROR: Abnormal values ',
C     &          'in UEOBC at I',I,'N= ',N 
              UEOBC_M(I,K,N)=0.0
            ENDIF
          ENDDO
        ENDDO

        DO I=1,NEOBC
          DO K=2,KBm
            DO N=1,NTMAX
              IF((ABS(UEOBC_M(I,K,N)).GE.99.0).OR.
     &           (IEEE_IS_NAN(UEOBC_M(I,K,N)))) THEN
C                WRITE(*,*) 'FATAL ERROR: Abnormal values ',
C     &            'in UEOBC at I',I,'N= ',N,'K= ',K
C                WRITE(*,*)'Replace using good value at the above layer'
                UEOBC_M(I,K,N)=UEOBC_M(I,K-1,N)
              ENDIF
            ENDDO
          ENDDO
        ENDDO

C Check VeOBC_M at surface K=1
        K=1
        DO I=1,NEOBC
          DO N=1,NTMAX
            IF((ABS(VEOBC_M(I,K,N)).GE.99.0).OR.
     &         (IEEE_IS_NAN(VEOBC_M(I,K,N)))) THEN
C              WRITE(*,*) 'FATAL ERROR: Abnormal values ',
C     &          'in VEOBC at I',I,'N= ',N 
              VEOBC_M(I,K,N)=0.0
            ENDIF
          ENDDO
        ENDDO

        DO I=1,NEOBC
          DO K=2,KBm
            DO N=1,NTMAX
              IF((ABS(VEOBC_M(I,K,N)).GE.99.0).OR.
     &           (IEEE_IS_NAN(VEOBC_M(I,K,N)))) THEN
C                WRITE(*,*) 'FATAL ERROR: Abnormal values ',
C     &            'in VEOBC at I',I,'N= ',N,'K= ',K
C                WRITE(*,*)'Replace using good value at the above layer'
                VEOBC_M(I,K,N)=VEOBC_M(I,K-1,N)
              ENDIF
            ENDDO
          ENDDO
        ENDDO

        IF((OFS(1:5).EQ.'ngofs').OR.(OFS(1:5).EQ.'NGOFS')) THEN
          call nos_ofs_write_netCDF_obc_fvcom(GRIDFILE,netcdf_file,
     &    ncidout,1,1,NOBC,NEOBC,KBm,KBm+1,base_date,
     &    Itime(1),Itime2(1),Times(1),hOBC,latOBC,lonOBC,lateOBC,
     &    loneOBC,nvOBC,siglay,siglev,WLOBC(:,1),tempOBC_M(:,:,1),
     &    saltOBC_M(:,:,1),ueOBC_M(:,:,1),veOBC_M(:,:,1),
     &    ubarOBC(:,1),vbarOBC(:,1),partition,globalstr,
     &    heOBC,siglay_ele)

cc ZLY read variables at START_TIME from the previous cycle's OBC file
          FIN=trim(OBC_FORCING_FILE_LAST)
          INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
          IF(FEXIST) THEN
            VNAME='time'
            ANAME='units'
            DO IJR=1,4
              DIMS(IJR)=1
            ENDDO

            CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
            DO IJR=1,DIMS(1)
              tmp1d(IJR)=(tmp4d(IJR,1,1,1)-Itime(1))*24.0
            ENDDO
            
            time00=(Itime2(1)/1000)/3600.0
            NSELECT=0
	    DO IJR=1,DIMS(1)
	     if(abs(tmp1d(IJR)-time00).le.0.2) then
	       NSELECT=IJR
	     end if
	    end do
            write(*,*) 'NSELECT =',NSELECT
	    
            IF(NSELECT.GT.0.AND.NSELECT.LE.DIMS(1)) THEN
              VNAME='zeta'
              DO IJR=1,4
                DIMS(IJR)=1
              ENDDO	
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                WLOBC(IJR,1)=tmp4d(IJR,NSELECT,1,1)
              ENDDO

              VNAME='ua'
              DO IJR=1,4
                DIMS(IJR)=1
              ENDDO	
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                ubarOBC(IJR,1)=TMP4D(IJR,NSELECT,1,1)
              ENDDO

              VNAME='va'
              DO IJR=1,4
                DIMS(IJR)=1
              ENDDO	
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                vbarOBC(IJR,1)=TMP4D(IJR,NSELECT,1,1)
              ENDDO

              VNAME='temp'
              DO I=1,4
                DIMS(I)=1
              ENDDO	
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                DO JJR=1,DIMS(2)
                  tempOBC_M(IJR,JJR,1)=TMP4D(IJR,JJR,NSELECT,1)
                ENDDO
              ENDDO

              VNAME='salinity'
              DO I=1,4
                DIMS(I)=1
              ENDDO	
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                DO JJR=1,DIMS(2)
                  saltOBC_M(IJR,JJR,1)=TMP4D(IJR,JJR,NSELECT,1)
                ENDDO
              ENDDO

              VNAME='u'
              DO I=1,4
                DIMS(I)=1
              ENDDO	
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                DO JJR=1,DIMS(2)
                  ueOBC_M(IJR,JJR,1)=TMP4D(IJR,JJR,NSELECT,1)
                ENDDO
              ENDDO

              VNAME='v'
              DO I=1,4
                DIMS(I)=1
              ENDDO	
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                DO JJR=1,DIMS(2)
                  veOBC_M(IJR,JJR,1)=TMP4D(IJR,JJR,NSELECT,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF
cc End by ZLY read variables at START_TIME from the previous cycle's OBC file
           
          DO N=1,NREC
            call nos_ofs_write_netCDF_obc_fvcom(GRIDFILE,netcdf_file,
     &      ncidout,2,1,NOBC,NEOBC,KBm,KBm+1,base_date,
     &      Itime(N),Itime2(N),Times(N),hOBC,latOBC,lonOBC,lateOBC,
     &      loneOBC,nvOBC,siglay,siglev,WLOBC(:,N),tempOBC_M(:,:,N),
     &      saltOBC_M(:,:,N),ueOBC_M(:,:,N),veOBC_M(:,:,N),
     &      ubarOBC(:,N),vbarOBC(:,N),partition,globalstr,
     &      heOBC,siglay_ele)
          ENDDO

          call nos_ofs_write_netCDF_obc_fvcom(GRIDFILE,netcdf_file,
     &    ncidout,3,1,NOBC,NEOBC,KBm,KBm+1,base_date,
     &    Itime(1),Itime2(1),Times(1),hOBC,latOBC,lonOBC,lateOBC,
     &    loneOBC,nvOBC,siglay,siglev,WLOBC(:,1),tempOBC_M(:,:,1),
     &    saltOBC_M(:,:,1),ueOBC_M(:,:,1),veOBC_M(:,:,1),
     &    ubarOBC(:,1),vbarOBC(:,1),partition,globalstr,
     &    heOBC,siglay_ele)
        ELSE
          globalstr(1)='FVCOM TIME SERIES ELEVATION FORCING FILE'
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

cc ZLY read variables at START_TIME from the previous cycle's OBC file
          FIN=trim(OBC_FORCING_FILE_LAST)
          INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
          IF(FEXIST) THEN
            VNAME='time'
            ANAME='units'
            DO IJR=1,4
              DIMS(IJR)=1
            ENDDO

            CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
            DO IJR=1,DIMS(1)
              tmp1d(IJR)=(tmp4d(IJR,1,1,1)-Itime(1))*24.0
            ENDDO

            time00=(Itime2(1)/1000)/3600.0
            NSELECT=0
            DO IJR=1,DIMS(1)
             if(abs(tmp1d(IJR)-time00).le.0.2) then
               NSELECT=IJR
             end if
            end do
            write(*,*) 'NSELECT =',NSELECT
          
            IF(NSELECT.GT.0.AND.NSELECT.LE.DIMS(1)) THEN
             if(wl_obc.eq.1) then
              VNAME='elevation'
              DO IJR=1,4
                DIMS(IJR)=1
              ENDDO
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                WLOBC(IJR,1)=tmp4d(IJR,NSELECT,1,1)
              ENDDO
             end if

             if(ubar_obc.eq.1) then
              VNAME='ua'
              DO IJR=1,4
                DIMS(IJR)=1
              ENDDO
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                ubarOBC(IJR,1)=TMP4D(IJR,NSELECT,1,1)
              ENDDO
             end if

             if(vbar_obc.eq.1) then
              VNAME='va'
              DO IJR=1,4
                DIMS(IJR)=1
              ENDDO
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                vbarOBC(IJR,1)=TMP4D(IJR,NSELECT,1,1)
              ENDDO
             end if

             if(temp_obc.eq.1) then
              VNAME='obc_temp'
              DO I=1,4
                DIMS(I)=1
              ENDDO
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                DO JJR=1,DIMS(2)
                  tempOBC_M(IJR,JJR,1)=TMP4D(IJR,JJR,NSELECT,1)
                ENDDO
              ENDDO
             end if

             if(salt_obc.eq.1) then
              VNAME='obc_salinity'
              DO I=1,4
                DIMS(I)=1
              ENDDO
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                DO JJR=1,DIMS(2)
                  saltOBC_M(IJR,JJR,1)=TMP4D(IJR,JJR,NSELECT,1)
                ENDDO
              ENDDO
             end if

             if(u_obc.eq.1) then
              VNAME='u'
              DO I=1,4
                DIMS(I)=1
              ENDDO
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                DO JJR=1,DIMS(2)
                  ueOBC_M(IJR,JJR,1)=TMP4D(IJR,JJR,NSELECT,1)
                ENDDO
              ENDDO
             end if

             if(v_obc.eq.1) then
              VNAME='v'
              DO I=1,4
                DIMS(I)=1
              ENDDO
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
              IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
              ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
              CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
              DO IJR=1,DIMS(1)
                DO JJR=1,DIMS(2)
                  veOBC_M(IJR,JJR,1)=TMP4D(IJR,JJR,NSELECT,1)
                ENDDO
              ENDDO
             end if
            END IF
          END IF

          call nos_ofs_write_netCDF_obc_fvcom_gl(GRIDFILE,netcdf_file,
     &    ncidout,1,1,NOBC,NEOBC,KBm,KBm+1,base_date,
     &    Itime(1),Itime2(1),Times(1),hOBC,latOBC,lonOBC,lat_eobc,
     &    lon_eobc,nv_obc,siglay,siglev,IOBC,wl_obc,
     &    temp_obc,salt_obc,u_obc,v_obc,ubar_obc,vbar_obc,
     &    partition_obc,globalstr)

C  imode=2
          DO N=1,NREC
            call nos_ofs_write_netCDF_obc_fvcom_gl(GRIDFILE,
     &      netcdf_file,ncidout,2,1,NOBC,NEOBC,KBm,KBm+1,base_date,
     &      Itime(N),Itime2(N),Times(N),hOBC,latOBC,lonOBC,lateOBC,
     &      loneOBC,nvOBC,siglay,siglev,IOBC,WLOBC(:,N),
     &      tempOBC_M(:,:,N),saltOBC_M(:,:,N),ueOBC_M(:,:,N),
     &      veOBC_M(:,:,N),ubarOBC(:,N),vbarOBC(:,N),partition,
     &      globalstr)
          ENDDO

C  imode=3
          call nos_ofs_write_netCDF_obc_fvcom_gl(GRIDFILE,netcdf_file,
     &    ncidout,3,1,NOBC,NEOBC,KBm,KBm+1,base_date,
     &    Itime(1),Itime2(1),Times(1),hOBC,latOBC,lonOBC,lateOBC,
     &    loneOBC,nvOBC,siglay,siglev,IOBC,WLOBC(:,1),
     &    tempOBC_M(:,:,1),
     &    saltOBC_M(:,:,1),ueOBC_M(:,:,1),veOBC_M(:,:,1),
     &    ubarOBC(:,1),vbarOBC(:,1),partition,globalstr)
        ENDIF 

        CLOSE(67)
        OPEN(67,file='obc_fvcom.dat')
        WRITE(67,*) NREC,NOBC,NEOBC,KBm,KBm+1,base_date
        WRITE(67,*) 'Times'
        DO N=1,NREC
          WRITE(67,*) Times(N)
        ENDDO
        WRITE(67,*) 'WLOBC'
        WRITE(67,*) WLOBC
        WRITE(67,*) 'tempOBC'
        WRITE(67,*) tempOBC_M
        WRITE(67,*) 'saltOBC'
        WRITE(67,*) saltOBC_M
        WRITE(67,*) 'ueOBC'
        WRITE(67,*) ueOBC_M
        WRITE(67,*) 'veOBC'
        WRITE(67,*) veOBC_M
        WRITE(67,*) 'ubarOBC'
        WRITE(67,*) ubarOBC
        WRITE(67,*) 'vbarOBC'
        WRITE(67,*) vbarOBC
        CLOSE(67)

        IF((TRIM(OFS).EQ.'sfbofs').OR. 
     &     (TRIM(OFS).EQ.'SFBOFS')) THEN		
          CLOSE(68)
   	  OPEN(68,FILE='obc_sfbofs_el.dat',STATUS='UNKNOWN')
          DO N=1,NREC
            WRITE(68,3334) TIME_M(N),(WLOBC(I,N),I=1,NOBC,10),
     &	      WLOBC(97,N),WLOBC(103,N)
          ENDDO
          CLOSE(68)

          OPEN(68,FILE='obc_sfbofs_temp.dat',STATUS='UNKNOWN')
          DO N=1,NREC
            WRITE(68,3334) TIME_M(N),(tempOBC_M(I,1,N),I=1,NOBC,10),
     &	      tempOBC_M(97,1,N),tempOBC_M(103,1,N)
          ENDDO
          CLOSE(68)
 
          OPEN(68,FILE='obc_sfbofs_salt.dat',STATUS='UNKNOWN')
          DO N=1,NREC
            WRITE(68,3334) TIME_M(N),(saltOBC_M(I,1,N),I=1,NOBC,10),
     &	      saltOBC_M(97,1,N),saltOBC_M(103,1,N)
          ENDDO
          CLOSE(68)
        ENDIF	
3334    FORMAT(f13.4,100F10.3)
      ELSEIF(trim(OCEAN_MODEL).EQ."SELFE")THEN
        WRITE(*,*) 'create SELFE OBC file'
      ENDIF


      WRITE(*,*) 'OBC Forcing file is COMPLETED SUCCESSFULLY'
      WRITE(ICORMS,'(a)') 'WL OBC SOURCE IS ' //trim(DBASE_WL)
      WRITE(ICORMS,'(a)') 'T&S OBC SOURCE IS '//trim(DBASE_TS)
      WRITE(ICORMS,'(a)') 'END SECTION OF GENERATING OBC FILE' 
      CLOSE(ICORMS)
      
      STOP
      END
       
      SUBROUTINE READ_NETCDF(FIN,VNAME,ANAME,NDIMS,DIMS,TMP4D,ATT,MODE)
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
      include 'netcdf.inc'
      CHARACTER*200 FIN,VNAME,ANAME,BUFFER
      INTEGER DIMS(4),MODE,DIMIDS(5),COUNT(4)
      REAL TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) )
      LOGICAL FEXIST
      INTEGER, ALLOCATABLE :: ITMP4D(:,:,:,:)
      REAL*8, ALLOCATABLE :: DTMP4D(:,:,:,:)

      IF(MODE.EQ.0) THEN
        DO I=1,4
          DIMS(I)=1
        ENDDO
        INQUIRE(FILE=TRIM(FIN),EXIST=FEXIST)
        IF(.NOT. FEXIST) THEN
          WRITE(*,*) TRIM(FIN)//' does not exist'
        ELSE	
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) THEN
            WRITE(*,*) 'Error message= ',STATUS
            STOP  !!'open netCDF file failed'
          ENDIF  
          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) THEN
            WRITE(*,*) 'Error message= ',STATUS
            WRITE(*,*) 'Required variable ',TRIm(VNAME), 
     &        ' is not found'
            STOP
          ENDIF  

          STATUS=NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
          STATUS=NF_INQ_VARDIMID(NCID,IDVAR,DIMIDS)
          DO I=1,NDIMS
            STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
C            WRITE(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
          ENDDO
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.1) THEN
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          WRITE(*,*) TRIM(FIN)//' does not exist'
        ELSE	
          STATUS=NF_OPEN(TRIM(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) THEN
            WRITE(*,*) 'Error message= ',STATUS
            STOP  !!'open netCDF file failed'
          ENDIF  
          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) THEN
            WRITE(*,*) 'Error message= ',STATUS
            WRITE(*,*) 'Required variable ',TRIM(VNAME), 
     &        ' is not found'
            STOP
          ENDIF  

          STATUS=NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
          STATUS=NF_INQ_VARDIMID(NCID,IDVAR,DIMIDS)
          DO I=1,NDIMS
            STATUS=NF_INQ_DIMLEN(NCID,DIMIDS(I),COUNT(I))
            IF (COUNT(i).NE.DIMS(I)) THEN
              WRITE(*,*) 'Dimension of array does not match' 
              WRITE(*,*) TRIM(VNAME),' dim ',i,' = ',COUNT(i)
              WRITE(*,*) 'DIMS(',I,')= ',DIMS(I),ndims
            ENDIF  
          ENDDO
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.7) THEN
        ALLOCATE(DTMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        INQUIRE(FILE=TRIM(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          WRITE(*,*) TRIM(FIN)//' does not exist'
        ELSE	
          STATUS=NF_OPEN(TRIM(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) THEN
            WRITE(*,*) 'Error message= ',STATUS
            STOP  !!'open netCDF file failed'
          ENDIF  
          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) THEN
            WRITE(*,*) 'Error message= ',STATUS
            WRITE(*,*) 'Required variable ',TRIM(VNAME), 
     &        ' is not found'
            STOP
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
          IF(ALLOCATED(DTMP4D)) DEALLOCATE(DTMP4D)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.2) THEN
        IF(ALLOCATED(ITMP4D)) DEALLOCATE(ITMP4D)
        ALLOCATE(ITMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) ))
        INQUIRE(FILE=TRIM(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          WRITE(*,*) TRIM(FIN)//' does not exist'
        ELSE	
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) THEN
            WRITE(*,*) 'Error message= ',STATUS
            STOP  !!'open netCDF file failed'
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
          IF(ALLOCATED(ITMP4D)) DEALLOCATE(ITMP4D)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.3) THEN
        ALLOCATE(ITMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) ))
        INQUIRE(FILE=TRIM(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          WRITE(*,*) TRIM(FIN)//' does not exist'
        ELSE	
          STATUS=NF_OPEN(TRIM(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) THEN
            WRITE(*,*) 'error message= ',STATUS
            STOP  !!'open netCDF file failed'
          ENDIF  
          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          STATUS=NF_GET_VAR_TEXT(NCID,IDVAR,BUFFER)
          VNAME=TRIM(BUFFER)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.4) THEN
        INQUIRE(FILE=TRIM(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          WRITE(*,*) TRIM(FIN)//' does not exist'
        ELSE	
          STATUS=NF_OPEN(TRIM(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) THEN
            WRITE(*,*) 'Error message= ',STATUS
            stop  !!'open netCDF file failed'
          ENDIF  
          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,TRIM(ANAME),ATT)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.5) THEN
        INQUIRE(FILE=TRIM(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          WRITE(*,*) TRIM(FIN)//' does not exist'
        ELSE	
          STATUS=NF_OPEN(TRIM(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) THEN
            WRITE(*,*) 'Error message= ',STATUS
            STOP  !!'open netCDF file failed'
          ENDIF  
          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,TRIM(ANAME),IATT)
          ATT=IATT
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.6) THEN
        INQUIRE(FILE=TRIM(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          WRITE(*,*) TRIM(FIN)//' does not exist'
        ELSE	
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) THEN
            WRITE(*,*) 'Error message= ',STATUS
            STOP  !!'open netCDF file failed'
          ENDIF  
          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          STATUS=NF_GET_ATT_TEXT(NCID,IDVAR,TRIM(ANAME),BUFFER)
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

      SUBROUTINE SEARCH_OUTPUT(IM,JM,LON_RTF,LAT_RTF,NN,
     &	  LON_OB,LAT_OB,NSELECT,II,JJ)
      PARAMETER (NMAX=90000)

      LOGICAL FOUND,LOG1,LOG2,LOG3,LOG4
      REAL*4 LON_RTF(IM,JM),LAT_RTF(IM,JM)
      REAL*4 LON_OB(NN),LAT_OB(NN)
      DIMENSION MASK(IM,JM),II(NMAX),JJ(NMAX)

c  Assign zeroes to mask       
      DO I=1,IM
        DO J=1,JM
          MASK(I,J)=0
        END DO
      END DO

      NSELECT=0
      DO K=1,NN
        X0=REAL(LON_OB(K))
        Y0=REAL(LAT_OB(K))
        FOUND=.FALSE.
        DO I=1,IM-1
          IF(FOUND) THEN
            EXIT
          END IF
          DO J=1,JM-1
            IF(.NOT.FOUND) THEN
              X1=LON_RTF(I,J)
              X2=LON_RTF(I+1,J)
              Y1=LAT_RTF(I,J)
              Y2=LAT_RTF(I,J+1)
              LOG1=X1.LE.X0
              LOG2=X0.LT.X2
              LOG3=Y1.LE.Y0
              LOG4=Y0.LT.Y2
              IF(LOG1.AND.LOG2.AND.LOG3.AND.LOG4) THEN
                FOUND=.TRUE.
                DO I0=I-2,I+3
                  DO J0=J-2,J+3
                    IF(I0.GE.1.AND.I0.LE.IM.AND.
     &                 J0.GE.1.AND.J0.LE.JM) THEN
                      IF(MASK(I0,J0).NE.1) THEN
                        NSELECT=NSELECT+1
                        II(NSELECT)=I0
                        JJ(NSELECT)=J0
                        MASK(I0,J0)=1
                      END IF
                    END IF
                  END DO
                END DO
              END IF
            END IF
          END DO
        END DO
      END DO
      WRITE(*,*) 'OFS OB #:',NN,'  SELECTED RTOFS #:',NSELECT
	
      RETURN
      END
 
