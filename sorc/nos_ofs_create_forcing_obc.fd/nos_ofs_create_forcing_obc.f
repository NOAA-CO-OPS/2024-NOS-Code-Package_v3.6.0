C----------------------------------------------------------------------------------
C
C Program Name:  nos_ofs_create_forcing_obc.f
C
C Directory:  /nos/save/wx21az/stratus/nosofs_shared.v1.0.0/sorc/nos_ofs_create_forcing_obc.fd/

C Purpose:    This Program is used to generated lateral open boundary condition files for ROMS 
C             from climatological dataset WOA05 or Navy Coastal Ocean Model (NCOM) products 
C             in NCEP data tank, or ETSS nontidal water levels.
C             The data on NCOM grid is horizontally interpolated onto ROMS open boundary points
C             using either remesh, nature neighbors, or bicubic, or bilinear routine,
C             and then vertically interpolated onto ROMS sigma vertical levels from NCOM 
C             z-coordinate vertical levels linearly.

C             Refer to HPC_COMF Technical Report for more detail information.               
C             
C Current contact:   Aijun Zhang
C         Org:  NOS/CO-OPS/OD   Phone:  301-713-2890 ext. 127 
C                    aijun.zhang@Noaa.gov 
C Attributes:
C  Language:  Fortran
C  Computer:  DEW/MIST at NCEP  
CC
C  Compile command:  gmake -f makefile
C
C Subprograms called:   remesh, regrid, write_netCDF_OBC_ROMS, utility
C
C Input Data files:
C   "/dcom/us007003/20081120/wgrdbul/ncom_glb_reg1_2008112000.nc.gz"
C
C Usage:   nos_ofs_create_forcing_obc < Fortran_OBC.ctl > Fortran_OBC.log 
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
C
C Output files: 
C    A netCDF open boundary forcing file which contains the required variables. 
C----------------------------------------------------------------------------------
      USE, INTRINSIC :: IEEE_ARITHMETIC
      parameter (NMAX=90000)
      include 'netcdf.inc'
      character*2 CHH
      character*8 CDATE
      character*10 OCEAN_MODEL
      character*20 DBASE_WL
      character*132 CMD,TMPTXT
      character*200 OFS,DBASE_TS,FILE_TS
      character*200 FIN,FOUT,GRIDFILE,netcdf_file,FIXnos,ETSSFILE
      character*200 BUFFER,VNAME,ANAME,OBC_CTL_FILE,HC_FILE,CTMP
      character*200 BUFFER1,HC_FILE_OFS,NCOMDIR,NOSWLDIR
      character*200 FILE_NCOM_TIDE(100),FILE_NCOM(100),FNAME,FIN_TIDE
      character*200 FDIR,STYPE,VGRIDFILE,NCOM_FILE,NOSBUFR,USGSBUFR
      character*200 CORMSLOG,DBASE_TS_ORI,INIT_FILE_NOWCAST
      character*200 START_TIME, END_TIME,BUFRFILE,OBC_CLIM_FILE
      character*200 START_TIMEm1
      character*200 OBC_FORCING_FILE_LAST,COMOUTroot,PREFIXNOS
      real*8 jday_start,jbase_date,JULIAN,yearb,monthb,dayb,hourb
      real*8 jday,jday0,js_etss,je_etss,jdays,jdaye
      real minlon,minlat,maxlat,maxlon,missvalue
      LOGICAL FEXIST,USGS_L
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      CHARACTER globalstr(9)*120
      INTEGER DATE_TIME(8)
      INTEGER BASE_DATE(4)
      INTEGER DAYS_PER_MONTH(12)
      DATA (DAYS_PER_MONTH(I),I=1,12) /
     &  31,28,31,30,31,30,31,31,30,31,30,31/ 
      integer grbunit
      real latsw,lonsw,LaD,LoV,dx_grb
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
      real, allocatable :: maskm  (:,:)
      real, allocatable :: hm  (:,:)
      real, allocatable :: WLwest  (:,:)
      real, allocatable :: WLeast  (:,:)
      real, allocatable :: WLsouth  (:,:)
      real, allocatable :: WLnorth  (:,:)
      real, allocatable :: Tempwest  (:,:,:)
      real, allocatable :: Tempeast  (:,:,:)
      real, allocatable :: Tempsouth  (:,:,:)
      real, allocatable :: Tempnorth  (:,:,:)
      real, allocatable :: saltwest  (:,:,:)
      real, allocatable :: salteast  (:,:,:)
      real, allocatable :: saltsouth  (:,:,:)
      real, allocatable :: saltnorth  (:,:,:)

      real, allocatable :: u_south  (:,:,:)  
      real, allocatable :: u_north  (:,:,:) 
      real, allocatable :: u_west  (:,:,:)  
      real, allocatable :: u_east  (:,:,:)  

      real, allocatable :: v_south  (:,:,:) 
      real, allocatable :: v_north  (:,:,:) 
      real, allocatable :: v_west  (:,:,:) 
      real, allocatable :: v_east  (:,:,:)

      real, allocatable :: ubarwest  (:,:) !!! ubarwest respect to lat lon 
      real, allocatable :: ubareast  (:,:)
      real, allocatable :: ubarsouth  (:,:)
      real, allocatable :: ubarnorth  (:,:)
      real, allocatable :: vbarwest  (:,:)
      real, allocatable :: vbareast  (:,:)
      real, allocatable :: vbarsouth  (:,:)
      real, allocatable :: vbarnorth  (:,:)
      real, allocatable :: Iout(:,:)
      real, allocatable :: Jout(:,:)

!!!!!!!!!!!!!!!!!!!!!! ub_west respect with roms xi eta
      real, allocatable :: ub_west  (:,:)  
      real, allocatable :: ub_east  (:,:) 
      real, allocatable :: ub_south  (:,:) 
      real, allocatable :: ub_north  (:,:) 
      real, allocatable :: vb_west  (:,:) 
      real, allocatable :: vb_east  (:,:) 
      real, allocatable :: vb_south  (:,:)
      real, allocatable :: vb_north  (:,:) 

      real, allocatable :: uu_west  (:,:) 
      real, allocatable :: uu_east  (:,:) 
      real, allocatable :: uu_south  (:,:) 
      real, allocatable :: uu_north  (:,:) 
      real, allocatable :: vv_west  (:,:) 
      real, allocatable :: vv_east  (:,:) 
      real, allocatable :: vv_south  (:,:)
      real, allocatable :: vv_north  (:,:) 
      real, allocatable :: dosouth  (:,:,:) 
      real, allocatable :: donorth  (:,:,:) 
      real, allocatable :: dowest  (:,:,:) 
      real, allocatable :: doeast  (:,:,:)
      
      real, allocatable :: TIMEOBC  (:)
      real, allocatable :: WLOBC  (:,:)
      real, allocatable :: ubarOBC  (:,:)
      real, allocatable :: vbarOBC  (:,:)
      real, allocatable :: tempOBC  (:,:,:)
      real, allocatable :: saltOBC  (:,:,:)
      real, allocatable :: uOBC  (:,:,:)
      real, allocatable :: vOBC  (:,:,:)
      real, allocatable :: tempOBC_M  (:,:,:)
      real, allocatable :: saltOBC_M  (:,:,:)
      real, allocatable :: uOBC_M  (:,:,:)
      real, allocatable :: vOBC_M  (:,:,:)
      real, allocatable :: ueOBC_M  (:,:,:)
      real, allocatable :: veOBC_M  (:,:,:)

cc allocatable arrays for variables in NCOM file
      real, allocatable :: lon  (:,:)
      real, allocatable :: lat  (:,:)
      real, allocatable :: zeta_time  (:)
      real, allocatable :: ts_time  (:)
      real, allocatable :: depth  (:)
      
      real, allocatable :: lonsub  (:,:)
      real, allocatable :: latsub  (:,:)
      real, allocatable :: masksub  (:,:)
      real, allocatable :: WL  (:,:,:)
      real, allocatable :: ubar_rtofs  (:,:,:)
      real, allocatable :: vbar_rtofs  (:,:,:)
      real, allocatable :: temp (:,:,:,:)
      real, allocatable :: salt (:,:,:,:)
      real, allocatable :: u (:,:,:,:)
      real, allocatable :: v (:,:,:,:)
      real, allocatable :: tide_NCOM  (:,:,:)

      real, allocatable :: lon_ETSS  (:,:)
      real, allocatable :: lat_ETSS  (:,:)
      real, allocatable :: WL_ETSS  (:,:)

      real, allocatable :: tide_amp  (:,:)
      real, allocatable :: tide_epoc (:,:)
      real, allocatable :: tide_speed(:,:)
      character,allocatable :: stationID(:,:)
      character,allocatable :: constituents(:,:)
      real amp(37),epoc(37)

cc temporary arrays
      real, allocatable :: tmp1d(:)
      real, allocatable :: tmp2d(:,:)
      real, allocatable :: tmp3d(:,:,:)
      real, allocatable :: tmp4d(:,:,:,:)
      real, allocatable :: tmpp1(:),tmpp2(:),tmpp3(:),tmpp4(:)
      real*8, allocatable :: dtmp4d(:,:,:,:)
      integer, allocatable :: itmp4d(:,:,:,:)
      integer, allocatable :: ITMP3D(:,:,:)
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
      real, allocatable :: weights(:,:)       
      integer n3(3),ITMP1D(2000)
      real w3(3)
      integer dimids(5),COUNT(4),DIMS(4)

cc variables for real time observations
      CHARACTER*8  SUBSET,NAME                                  
      character*20 stnbufrid
      Real*8 DATES(5),RTIM(6),RPID
      Real*8 data1(2,500),data2(4,500),data3(5)
      real*8 xlocat(5)
      DATA BMISS /10E10/                                                
      equivalence(NAME,RPID)

CC variables for OBC_CTL_FILE     
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
      real, allocatable :: hOBC(:) 
      real, allocatable :: AVGERR(:) 
      real, allocatable :: AVGERR_T(:) 
      real, allocatable :: AVGERR_S(:) 

      real, allocatable :: TIME_clim(:)
      real, allocatable :: T_clim(:,:,:) 
      real, allocatable :: S_clim(:,:,:) 
      real, allocatable :: depth_clim(:,:)
      integer, allocatable :: K_clim(:)
     
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
      IGRD_ORI=IGRD
      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(I:I).eq."'".or.BUFFER(I:I).eq.'"') then
          BUFFER(I:I)=' '
        endif    
      enddo
      FIXnos=trim(adjustL(BUFFER))
      
      read(5,'(a200)') BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I).eq."'".or.BUFFER(i:I).eq.'"') then
          BUFFER(i:I)=' '
        endif    
      enddo
      GRIDFILE=trim(adjustL(BUFFER))
      write(*,*) 'gridfile=',trim(gridfile)

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
        if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
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
      if(trim(OCEAN_MODEL).eq."ROMS" ) then
        read(5,*) THETA_S
        read(5,*) THETA_B
        read(5,*) TCLINE
        read(5,*) NVTRANS   
        read(5,*) NVSTR
      endif  
      read(5,*) ioxyg

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
      read(5,'(a200)') PREFIXNOS
!-----------------------------------------------------------------------
!  calculate base time, start time, and end time in days
!-----------------------------------------------------------------------
      ICORMS=43
      CLOSE(ICORMS)
      OPEN(ICORMS,FILE=trim(CORMSLOG),STATUS='OLD',POSITION='APPEND')
      TMPTXT='BEGIN SECTION OF GENERATING OBC'
      WRITE(ICORMS,'(a)') TRIM(TMPTXT)
      yearb=base_date(1)
      monthb=base_date(2)
      dayb=base_date(3)
      hourb=base_date(4)
      jbase_date=JULIAN(yearb,monthb,dayb,hourb)

      yearb=IYRS
      monthb=IMMS
      dayb=IDDS
      hourb=IHHS   !! do not need minutes to construct ETSS file name
      jdays=JULIAN(yearb,monthb,dayb,hourb)
      day_start=jdays-jbase_date

      yearb=IYRE
      monthb=IMME
      dayb=IDDE
      hourb=IHHE   !! do not need minutes to construct ETSS file name
      jdaye=JULIAN(yearb,monthb,dayb,hourb)
      day_end=jdaye-jbase_date
      write(*,*) 'Base_date= ',base_date
      write(*,*) 'Domin=',minlon,minlat,maxlon,maxlat
      write(*,*) 'Model start & end time= ',day_start,day_end

!-----------------------------------------------------------------------
!   reading in NOS tide constituents at all NWLON stations
!-----------------------------------------------------------------------
      HC_FILE='nosofs.HC_NWLON.nc'
      INQUIRE(FILE=trim(HC_FILE),EXIST=FEXIST)
      IF(.NOT. FEXIST) THEN
        TMPTXT='Harmonic Constant NetCDF file is not found'
        WRITE(*,*) TRIM(TMPTXT)
        TMPTXT='Please check nosofs.HC_NWLON.nc in FIX directory'
        WRITE(*,*) TRIM(TMPTXT)
        WRITE(*,*) 'Provide correct Harmonic Constant File Name'
        WRITE(*,*) TRIM(OFS)//' stop here'
        WRITE(ICORMS,'(a)') 'CRITICAL FAILURE IN CREATING OBC' 
        STOP
      ELSE  
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))

        STATUS=NF_OPEN(HC_FILE,NF_NOWRITE,NCID)
        STATUS=NF_INQ(NCID,NDIMS,NVARS,NGATTS,UNLIMDIMID)
cc  NDIMS=number of dimension parameters
cc  NVARS=number of total variables in the netcdf file
cc  NGATTS= number of global attributes
cc  UNLIMDIMID= dimension ID which is unlimited.
        DO I=1,NDIMS
          STATUS = NF_INQ_DIM(NCID,I,BUFFER,ILATID)  !! extract dimension name
          STATUS = NF_INQ_DIMLEN(NCID,I,ILATID)
          IF(trim(BUFFER).eq.'Station') then
            NWLON_STA=ILATID
          ELSEIF((trim(BUFFER).eq.'Constituents')) then
            NCON=ILATID
          ELSEIF (trim(BUFFER).eq.'staID') then
            NCHAR=ILATID
          endif
        ENDDO

        VNAME='stationID'
        STATUS = NF_INQ_VARID(NCID,'stationID',IDVAR)
        STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        do i=1,NDIMS
          STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
          write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
        enddo
        allocate(stationID(DIMS(1),DIMS(2)))
        STATUS = NF_GET_VAR_TEXT(NCID,IDVAR,stationID)

        STATUS = NF_INQ_VARID(NCID,'constituentName',IDVAR)
        STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        do i=1,NDIMS
          STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
        enddo
        allocate(constituents(DIMS(1),DIMS(2)))
        STATUS = NF_GET_VAR_TEXT(NCID,IDVAR,constituents)

        VNAME='amplitude'
        STATUS = NF_INQ_VARID(NCID,'amplitude',IDVAR)
        STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        do i=1,NDIMS
          STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
          write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
        enddo
        allocate(tide_amp(DIMS(1),DIMS(2)) )
        allocate(tide_epoc(DIMS(1),DIMS(2)))
        STATUS = NF_INQ_VARID(NCID,'amplitude',IDVAR)
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tide_amp)
        STATUS = NF_INQ_VARID(NCID,'phase',IDVAR)
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tide_epoc)
        STATUS = NF_CLOSE(NCID)
      ENDIF

!-----------------------------------------------------------------------
!   reading in ROMS model grid information  
!-----------------------------------------------------------------------
      DO I=1,4
        DIMS(I)=1
      ENDDO	

      IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
      ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
      IF(trim(OCEAN_MODEL).EQ.'ROMS') THEN
        WRITE(*,*) 'Reading ROMS grid file: ',trim(GRIDFILE)
        VNAME='lon_rho'
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,
     &       TMP4D,ATT,0)
	IROMS=DIMS(1)
	JROMS=DIMS(2)
        NNMODEL=IROMS*JROMS
        ALLOCATE(lonm(IROMS,JROMS))
        ALLOCATE(latm(IROMS,JROMS))
        ALLOCATE(angm(IROMS,JROMS))
        ALLOCATE(maskm(IROMS,JROMS))
        ALLOCATE(hm(IROMS,JROMS))
        ALLOCATE(sigma(KBm))
        ALLOCATE(sigmaw(0:KBm))
        ALLOCATE(Cs_r(KBm))
        ALLOCATE(Cs_w(0:KBm))
        ALLOCATE(ZSIGMA(KBm))
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,
     &       TMP4D,ATT,1)
        DO I=1,IROMS
          Do J=1,JROMS
            lonm(I,J)=TMP4D(I,J,1,1)
          ENDDO
        ENDDO   

        VNAME='lat_rho'
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,
     &       TMP4D,ATT,1)
        DO I=1,IROMS
          Do J=1,JROMS
            latm(I,J)=TMP4D(I,J,1,1)
          ENDDO
        ENDDO   

        VNAME='angle'
        ANAME='units'
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,
     &       TMP4D,ATT,1)
        DO I=1,IROMS
          Do J=1,JROMS
            angm(I,J)=TMP4D(I,J,1,1)
          ENDDO
        ENDDO   
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,
     &       TMP4D,ATT,6)
        LL=INDEX(trim(ANAME),'degree')
        IF(LL.GT.0) THEN
          TMPTXT='FATAL ERROR: angle units should be in radians!'
          WRITE(*,*) TRIM(TMPTXT)
          WRITE(ICORMS,'(a)') TRIM(TMPTXT)
          STOP
        ENDIF

        VNAME='mask_rho'
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,
     &       TMP4D,ATT,1)
        DO I=1,IROMS
          Do J=1,JROMS
            maskm(I,J)=TMP4D(I,J,1,1)
          ENDDO
        ENDDO   

        VNAME='h'
        CALL READ_NETCDF(STATUS,GRIDFILE,VNAME,ANAME,NDIM,DIMS,
     &       TMP4D,ATT,1)
        DO I=1,IROMS
          Do J=1,JROMS
            hm(I,J)=TMP4D(I,J,1,1)
          ENDDO
        ENDDO   
!
!-----------------------------------------------------------------------
!  Define S-Curves in domain [-1 < sc < 0] at vertical W- and
!  RHO-points.
!-----------------------------------------------------------------------
        IF(theta_s.ne.0.0) THEN
          cff1=1.0/SINH(theta_s)
          cff2=0.5/TANH(0.5*theta_s)
        END IF
        Cs_w(0)=-1.0
        sigmaW(0)=-1.0
        cff=1.0/REAL(KBm)
        DO k=1,KBm
          sigmaW(k)=cff*REAL(k-KBm)
          sigma(k)=cff*(REAL(k-KBM)-0.5)
          IF(theta_s.ne.0.0) THEN
            Cs_w(k)=(1.0-theta_b)*cff1*SINH(theta_s*sigmaW(k))+
     &              theta_b*(cff2*TANH(theta_s*(sigmaW(k)+0.5))-0.5)
            Cs_r(k)=(1.0-theta_b)*cff1*SINH(theta_s*sigma(k))+                 
     &              theta_b*(cff2*TANH(theta_s*(sigma(k)+0.5))-0.5)
          ELSE
            Cs_w(k)=sigmaW(k)
            Cs_r(k)=sigma(k)
          END IF	
        END DO

!-----------------------------------------------------------------------
!  Report information about vertical S-levels. K=1 for bottom and K=KBm for surface
!-----------------------------------------------------------------------
        hmin=minval(hm)
        hmax=maxval(hm)
        hc=min(max(hmin,0.0),TCLINE)
        WRITE(*,10)
10      FORMAT (/,' Vertical S-coordinate System: ',/,/,  	      
     &  	  ' level   S-coord	Cs-curve',10x,  		
     &  	  'at_hmin  over_slope     at_hmax',/)
        IF(ALLOCATED(tmp2d)) DEALLOCATE(tmp2d)
        ALLOCATE(tmp2d(KBm,3))
        htest=hmin
        if(htest.le.0.0) htest=1.0
        ele=0.0
        CALL sigma2Z_ROMS_FIX_new(sigma,htest,ele,KBm,ZSIGMA,
     &       hc,theta_s,theta_b,TCline,nvtrans,nvstr)
        DO k=KBm,1,-1
          tmp2d(K,1)=Zsigma(K)
        ENDDO   

        htest=0.5*(hmin+hmax)
        if(htest.le.0.0) htest=50.0
        CALL sigma2Z_ROMS_FIX_new(sigma,htest,ele,KBm,ZSIGMA,
     &       hc,theta_s,theta_b,TCline,nvtrans,nvstr)
        DO k=KBm,1,-1
          tmp2d(K,2)=Zsigma(K)
        ENDDO   

        htest=hmax
        if(htest.le.0.0) htest=100.0
        CALL sigma2Z_ROMS_FIX_new(sigma,htest,ele,KBm,ZSIGMA,
     &       hc,theta_s,theta_b,TCline,nvtrans,nvstr)
        DO k=KBm,1,-1
          tmp2d(K,3)=Zsigma(K)
        ENDDO   

        DO k=KBm,1,-1
	  WRITE (*,15) k,sigmaW(k),Cs_w(k),(tmp2d(k,i),i=1,3)
        ENDDO	  
15      FORMAT(i6,2f12.7,4x,3f12.3)
        write(*,*) 'xi_rho & eta_rho of ROMS grid=',IROMS,JROMS

        DO I=1,IROMS
          DO J=1,JROMS
            if(maskm(i,j).gt.0)
     &	      write(56,'(2f12.4,2i5)')lonm(i,j),latm(i,j),i,j
          ENDDO
        ENDDO	
        close(56)
      ENDIF

C -------------------------------------------------------------------
C   open and read OBC control file  
C -------------------------------------------------------------------
      WRITE(*,*) 'Reading OBC control file'      
      OPEN(20,FILE=TRIM(OBC_CTL_FILE))
      READ(20,*) NSTA,NOBC,DELT 
      WRITE(*,*) 'NSTA= ',NSTA,'NOBC=',NOBC
! Arrays for real time observations
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
      allocate(JOBC(NOBC))
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
      allocate(hOBC(NOBC))

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

      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF(ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF(ALLOCATED(oned4)) DEALLOCATE(oned4)
      allocate(oned1(NMAX))
      allocate(oned2(NMAX))
      allocate(oned3(NMAX))
      allocate(oned4(NMAX))

      READ(20,*) BUFFER 
      READ(20,*) BUFFER 
      DO N=1,NSTA
        READ(20,*)SID(N),NOS_ID(N),NWS_ID(N),AGENCY_ID(N),DATUM(N),
     &	  WL_FLAG(N),TS_FLAG(N),BACKUP_SID(N),GRIDID_STA(N),As(N)
      ENDDO

      READ(20,*) BUFFER 
      READ(20,*) BUFFER 
      DO N=1,NOBC
        READ(20,*)GRIDID(N),IOBC(N),JOBC(N),WL_STA(N),WL_SID_1(N),
     &	  WL_S_1(N),WL_SID_2(N),WL_S_2(N),TS_STA(N),TS_SID_1(N),
     &    TS_S_1(N),TS_SID_2(N),TS_S_2(N),OBC_ID(N)
        lonOBC(N)=lonm(IOBC(N),JOBC(N) )
        latOBC(N)=latm(IOBC(N),JOBC(N) )
        hOBC(N)=hm(IOBC(N),JOBC(N) )
      ENDDO

      USGS_L=.FALSE.
      DO N=1,NSTA
        IF(TRIM(AGENCY_ID(N)).EQ."USGS" ) USGS_L=.TRUE.
      ENDDO

C------------------------------------------------------------------
C  Begin to read in climatological temperature and salinity 
C  and temporally interpolated into equally interval of DELT 
C------------------------------------------------------------------
      NREC=NINT( (day_end-day_start)*24/DELT)+1
      DO N=1,NREC
        TIME_clim(N)=(day_start*24.+(N-1)*DELT)/24.0
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
            WRITE(*,*) 'climatologic T&S file is not found'
            WRITE(*,*) 'please provide climatologic T&S file at '
            WRITE(*,*) 'station ', TRIM(NOS_ID(N))
            WRITE(ICORMS,'(a)') 'CRITICAL FAILURE IN CREATING OBC'
            STOP
          ELSE  
            yearb=IYRS
            monthb=1.0
            dayb=1.0
            hourb=0
            jday0=JULIAN(yearb,monthb,dayb,hourb)

            CLOSE(10)
            OPEN(10,FILE=TRIM(FIN) )
            READ(10,*) K_clim(N)
            READ(10,*) (DEPTH_clim(N,K),K=1,K_clim(N))
            READ(10,*)
            ICOUNT=0
            DO IITMP=1,10000
18            READ(10,*,end=20) dummy,mon,IDD,
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
20          CLOSE(10)

            IF(ONED3(ICOUNT).LT.day_end) then
              ICOUNT=ICOUNT+1
              ONED3(ICOUNT)=day_end
              DO K=1,K_clim(N)
                T_clim(N,ICOUNT,K)=ONED1(K)
                S_clim(N,ICOUNT,K)=ONED2(K)
              ENDDO
            ENDIF
            WRITE(*,*) 'time range= ',day_start,day_end,
     &	      ONED3(1),ONED3(ICOUNT),ICOUNT

            DO K=1,K_clim(N)
              DO N1=1,ICOUNT
                oned1(N1)=T_clim(N,N1,K)
                oned2(N1)=S_clim(N,N1,K)
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
              CALL lineararray(NREC,tmpp1,tmpp2,ICOUNT,tmpp3,tmpp4)
              DO N1=1,NREC
                T_clim(N,N1,K) =tmpp2(N1)
              ENDDO

              DO N1=1,ICOUNT
                tmpp4(N1)=oned2(N1)
              End do
              CALL lineararray(NREC,tmpp1,tmpp2,ICOUNT,tmpp3,tmpp4)

              DO N1=1,NREC
                S_clim(N,N1,K) =tmpp2(N1)
              ENDDO	      
              deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
            ENDDO	      
          ENDIF
        ENDIF
      ENDDO

cc If DBASE_TS is NCOM and no NCOM file found, 
cc Reassign DBASE_TS='WOA05'
      IF((TRIM(DBASE_TS).EQ.'NCOM').OR. 
     &   (TRIM(DBASE_TS).EQ.'NCOMeast').OR.
     &   (TRIM(DBASE_TS).EQ.'AMSEAS')) THEN
        NCOM_FILE=TRIM(DBASE_TS)//"_FILE"
        IFILE_NCOM=0
        INQUIRE(FILE=trim(NCOM_FILE),EXIST=FEXIST)
        if(FEXIST) THEN
          CLOSE(60)
          OPEN(60,file=trim(NCOM_FILE))
          DO I=1,999
            READ(60,'(a100)',END=336) BUFFER
            FILE_NCOM(I)=trim(adjustL(BUFFER))
            READ(60,'(a100)',END=336) BUFFER
            FILE_NCOM_TIDE(I)=trim(adjustL(BUFFER))
          ENDDO
336       CLOSE(60)
          IFILE_NCOM=I-1
        ENDIF
        WRITE(*,*) 'Total number of NCOM FILES= ',IFILE_NCOM

        IF(IFILE_NCOM.EQ.0) THEN
          WRITE(*,*) 'No NCOM file is available in this period'
          WRITE(*,*) 'use WOA05 climatological dataset to replace'
          WRITE(ICORMS,'(a)') 'NCOM FILE IS NOT FOUND'
          WRITE(ICORMS,'(a)') 'USE CLIMATOLOGIC BACKUP WOA05'
          DBASE_TS='WOA05'
          IF(TRIM(DBASE_WL).EQ.'NCOM') DBASE_WL='ETSS'
        ENDIF  
      ENDIF

C------------------------------------------------------------------
C-------- Begin to read in WOA05 NetCDF file ----------------------
C------------------------------------------------------------------
30    CONTINUE
      IF(TRIM(DBASE_TS).EQ.'WOA05') THEN
        FIN=TRIM(OBC_CLIM_FILE)
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          WRITE(*,*) 'Climatologic T&S file (WOA) is not found'
          WRITE(*,*) 'Please provide climatologic T&S file of:'
          WRITE(*,*) TRIM(OBC_CLIM_FILE)//' in '//TRIM(FIXnos)
          TMPTXT='Climatologic T&S file (WOA) is not found'
          WRITE(ICORMS,'(a)') TRIM(TMPTXT)
          TMPTXT=TRIM(OBC_CLIM_FILE)//' in '//TRIM(FIXnos)
          WRITE(ICORMS,'(a)') TRIM(TMPTXT)
          WRITE(ICORMS,'(a)') 'CRITICAL FAILURE IN CREATING OBC'
 	  STOP
	ENDIF  
        FILE_TS=TRIM(FIN)
        VNAME='t0112an1'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IM=DIMS(1)
        JM=DIMS(2)
        KB=DIMS(3)
        NT=DIMS(4)
        IF(ALLOCATED(lon)) DEALLOCATE(lon)
        IF(ALLOCATED(lat)) DEALLOCATE(lat)
        IF(ALLOCATED(depth)) DEALLOCATE(depth)
        IF(ALLOCATED(ts_time)) DEALLOCATE(ts_time)
        ALLOCATE(lon(IM,JM))
        ALLOCATE(lat(IM,JM))
        ALLOCATE(depth(KB))
        ALLOCATE(ts_time(NT+2))

        VNAME='lon'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        write(*,*) 'dims=',dims,NDIM
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO I=1,IM
          DO J=1,JM
            lon(i,j)=TMP4D(i,1,1,1)
 	    if(lon(i,j).gt.180.0) lon(i,j)=lon(i,j)-360.0
          ENDDO
        ENDDO

        VNAME='lat'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO I=1,IM
          DO J=1,JM
            lat(i,j)=TMP4D(J,1,1,1)
          ENDDO
        ENDDO
	 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
CC select subdomain I and J index        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IMIN=99999
        IMAX=-9999
        JMIN=99999
        JMAX=-9999
        DO I=1,IM
          DO J=1,JM
            IF(lon(i,j).GE.minlon.and.lon(i,j).LE.maxlon.and.
     &         lat(i,j).GE.minlat.and.lat(i,j).LE.maxlat) then
              IF(I.LT.IMIN) IMIN=I 	  
              IF(I.GT.IMAX) IMAX=I 	  
              IF(J.LT.JMIN) JMIN=J 	  
              IF(J.GT.JMAX) JMAX=J
            ENDIF
          ENDDO
        ENDDO
        IMIN=IMIN-1     !!extending WOA05 coverage
        IMAX=IMAX+1	  	  
        JMIN=JMIN-1
        JMAX=JMAX+1	  	  
        ISUB=IMAX-IMIN+1	 
        JSUB=JMAX-JMIN+1	 
        write(*,*) 'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX	        

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc allocate sizes of arrays 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        NNSUB=ISUB*JSUB
        IF(ALLOCATED(lonsub)) DEALLOCATE(lonsub)
        IF(ALLOCATED(latsub)) DEALLOCATE(latsub)
        IF(ALLOCATED(masksub)) DEALLOCATE(masksub)
        IF(ALLOCATED(temp)) DEALLOCATE(temp)
        IF(ALLOCATED(salt)) DEALLOCATE(salt)
        allocate(lonsub(ISUB,JSUB))
        allocate(latsub(ISUB,JSUB))
        allocate(masksub(ISUB,JSUB))
        allocate(temp(ISUB,JSUB,KB,240))
        allocate(salt(ISUB,JSUB,KB,240))

        DO J=1,JSUB
          DO I=1,ISUB
            lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
            latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
            write(55,'(2f12.4,2i5)') lonsub(i,j),latsub(i,j),i,j
          ENDDO
        ENDDO   
        close(55) 

        VNAME='depth'         
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO K=1,KB
          depth(K)=TMP4D(K,1,1,1)
        ENDDO

        VNAME='time'         
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)

        yearb=IYRS
        monthb=1
        dayb=1
        hourb=0
        jday=JULIAN(yearb,monthb,dayb,hourb)
        ts_time(1)=jday-jbase_date
        ts_time(NT+2)=370.0+jday-jbase_date  !! for crossing year problem zaj
        DO N=2,NT+1
          ts_time(N)=TMP4D(N-1,1,1,1)+jday-jbase_date
        ENDDO

        VNAME='t0112an1'
        missvalue=1.e+20
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO I=1,ISUB
          DO J=1,JSUB
            DO K=1,KB
              DO N=2,NT+1
                TEMP(I,J,K,N)=TMP4D(IMIN+I-1,JMIN+J-1,K,N-1)
                if(abs(TEMP(I,J,K,N)-missvalue).LE.0.001)
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

        VNAME='s0112an1'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO I=1,ISUB
          DO J=1,JSUB
            DO K=1,KB
              DO N=2,NT+1
                SALT(I,J,K,N)=TMP4D(IMIN+I-1,JMIN+J-1,K,N-1)
                if(abs(SALT(I,J,K,N)-missvalue).LE.0.001)
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

        VNAME='landsea'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO I=1,ISUB
          DO J=1,JSUB
            masksub(I,J)=TMP4D(IMIN+I-1,JMIN+J-1,1,1)
          ENDDO	
        ENDDO	
        DO I=1,ISUB
          DO J=1,JSUB
            DO K=1,KB
              write(*,*) 'K=',K,lonsub(I,J),latsub(I,J),masksub(I,J),
     &	        TEMP(I,J,K,2),SALT(I,J,K,2)
            ENDDO	
          ENDDO	
        ENDDO

        NT=NT+2
        WRITE(*,*) 'WOA05 NT+2=', NT 	
      ENDIF

C------------------------------------------------------------------
C     Read and process available NCOM forecast files by the provided start time       
C------------------------------------------------------------------
      IF((TRIM(DBASE_TS).EQ.'NCOM').OR. 
     &   (TRIM(DBASE_TS).EQ.'NCOMeast').OR.
     &   (TRIM(DBASE_TS).EQ.'AMSEAS')) THEN
        NCOM_FILE=TRIM(DBASE_TS)//"_FILE"
        IFILE_NCOM=0
        INQUIRE(FILE=trim(NCOM_FILE),EXIST=FEXIST)
        if(FEXIST) THEN
          CLOSE(60)
          OPEN(60,file=trim(NCOM_FILE))
          DO I=1,999
            READ(60,'(a100)',END=333) BUFFER
            FILE_NCOM(I)=trim(adjustL(BUFFER))
            READ(60,'(a100)',END=333) BUFFER
            FILE_NCOM_TIDE(I)=trim(adjustL(BUFFER))
          ENDDO
333       CLOSE(60)
          IFILE_NCOM=I-1
        ENDIF
        WRITE(*,*) 'Total number of NCOM FILES= ',IFILE_NCOM

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
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IM=DIMS(1)
        JM=DIMS(2)
        KB=DIMS(3)
        NT=DIMS(4)
        WRITE(*,*) 'NCOM IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
        IF(ALLOCATED(lon)) DEALLOCATE(lon)
        IF(ALLOCATED(lat)) DEALLOCATE(lat)
        IF(ALLOCATED(depth)) DEALLOCATE(depth)
        IF(ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
        IF(ALLOCATED(ts_time)) DEALLOCATE(ts_time)
        ALLOCATE(lon(IM,JM))
        ALLOCATE(lat(IM,JM))
        ALLOCATE(depth(KB))
        ALLOCATE(zeta_time(NMAX))
        ALLOCATE(ts_time(NMAX))

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='lon'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
        DO I=1,IM
          DO J=1,JM
            lon(i,j)=TMP4D(i,1,1,1)
 	    if(lon(i,j).gt.180.0) lon(i,j)=lon(i,j)-360.0
          ENDDO
        ENDDO

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='lat'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
        DO I=1,IM
          DO J=1,JM
            lat(i,j)=TMP4D(J,1,1,1)
          ENDDO
        ENDDO
 
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='depth'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
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
        DO I=1,IM
          DO J=1,JM
            IF(lon(i,j).GE.minlon.and.lon(i,j).LE.maxlon.and.
     &         lat(i,j).GE.minlat.and.lat(i,j).LE.maxlat) then
              IF(I.LT.IMIN) IMIN=I 	  
              IF(I.GT.IMAX) IMAX=I 	  
              IF(J.LT.JMIN) JMIN=J 	  
              IF(J.GT.JMAX) JMAX=J
            ENDIF
          ENDDO
        ENDDO	  	  
        ISUB=IMAX-IMIN+1	 
        JSUB=JMAX-JMIN+1	 
        write(*,*) 'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX

C-----------------------------------------------------------------------
C allocate sizes of arrays for NCOM products 
C-----------------------------------------------------------------------
        NNSUB=ISUB*JSUB
        IF(ALLOCATED(lonsub)) DEALLOCATE(lonsub)
        IF(ALLOCATED(latsub)) DEALLOCATE(latsub)
        IF(ALLOCATED(temp)) DEALLOCATE(temp)
        IF(ALLOCATED(salt)) DEALLOCATE(salt)
        IF(ALLOCATED(u)) DEALLOCATE(u)
        IF(ALLOCATED(v)) DEALLOCATE(v)
        IF(ALLOCATED(WL)) DEALLOCATE(WL)
        IF(ALLOCATED(tide_NCOM)) DEALLOCATE(tide_NCOM)
        allocate(lonsub(ISUB,JSUB))
        allocate(latsub(ISUB,JSUB))
        allocate(temp(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        allocate(salt(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        allocate(u(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        allocate(v(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        allocate(WL(ISUB,JSUB,NT*IFILE_NCOM+10))
        allocate(tide_NCOM(ISUB,JSUB,NT*IFILE_NCOM+10))

        DO N=1,NT*IFILE_NCOM+10
          DO J=1,JSUB
            DO I=1,ISUB
              WL(I,J,N)=0.0
              tide_NCOM(I,J,N)=0.0
            ENDDO
          ENDDO
        ENDDO   

        DO J=1,JSUB
          DO I=1,ISUB
            lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
            latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
          ENDDO
        ENDDO   
        ICOUNT=1
	NREC=1
        TS_TIME(1)=-99999.9	 
	DO IFILE=1,IFILE_NCOM
          write(*,*) 'Reading NCOM NetCDF file: ',IFILE
          FIN=TRIM(FILE_NCOM(IFILE))
          VNAME='time'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
          ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          NT=DIMS(1)
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,6)
          ANAME=trim(adjustL(ANAME))
          LEN=LEN_TRIM(ANAME)
          LL=INDEX(ANAME,'minute')         
          IF(LL.GT.0) scale_time=1.0/1440.0
          LL=INDEX(ANAME,'hour')         
          IF(LL.GT.0) scale_time=1.0/24.0

          ANAME='time_origin'
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,6)
          ANAME=trim(adjustL(ANAME))
          write(*,*) 'time_origin=',trim(ANAME)
          read(ANAME,80) IYR,IMM,IDD,IHH
80        format(I4,1x,i2,1x,i2,1x,i2)
          yearb=IYR
          monthb=IMM
          dayb=IDD
          hourb=IHH
          jday=JULIAN(yearb,monthb,dayb,hourb)
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,7)
          dummy=TMP4D(1,1,1,1)*scale_time+jday-jbase_date
	  N0=0
          DO N=1,NREC
            IF(TS_TIME(N).GE.DUMMY) THEN
              N0=N-1 
              EXIT
            ENDIF  
          ENDDO	

82        ICOUNT=N0
          DO N=1,NT
            TIME_NCOM=TMP4D(N,1,1,1)*scale_time+jday-jbase_date
            ICOUNT=ICOUNT+1
            TS_TIME(ICOUNT)=TIME_NCOM
            write(*,*) 'NCOM time=',ICOUNT,TS_TIME(ICOUNT)
          ENDDO   
          write(*,*) 'first time= ',dummy,TS_TIME(NREC),NREC,N0

          STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'error message= ',status
            stop  !!'open NCOM netCDF file of failed'
          ENDIF  
          IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
          ALLOCATE(tmp1d(IM))
          IF(ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
          ALLOCATE(tmp3d(IM,JM,NT))
          IF(ALLOCATED(itmp3d)) DEALLOCATE(itmp3d)
          ALLOCATE(itmp3d(IM,JM,NT))
          IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
          ALLOCATE(tmp4d(IM,JM,KB,NT))

          STATUS = NF_INQ_VARID(NCID,'surf_el',IDVAR)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS = NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp3d)
          write(*,*) 'first data=',tmp3d(1,1,1),tmp3d(2,1,1)

          missvalue=IATT*scale+offset
          write(*,*) 'factor=',scale,offset,missvalue
          ICOUNT=N0
          DO N=1,NT
            ICOUNT=ICOUNT+1
            DO I=1,ISUB
              DO J=1,JSUB
                WL(I,J,ICOUNT)=TMP3D(IMIN+I-1,JMIN+J-1,N)*scale+
     &            offset
                if(abs(WL(I,J,ICOUNT)-missvalue).LE.0.001)
     &	           WL(I,J,ICOUNT)=-99999.9
                if(IEEE_IS_NAN(WL(I,J,ICOUNT))) 
     &             WL(I,J,ICOUNT)=-99999.9
              ENDDO	
            ENDDO	
          ENDDO	

          STATUS = NF_INQ_VARID(NCID,'water_temp',IDVAR)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS = NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
          missvalue=IATT*scale+offset
          ICOUNT=N0
          DO N=1,NT
            ICOUNT=ICOUNT+1
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  TEMP(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*
     &              scale+offset
                  if(abs(TEMP(I,J,K,ICOUNT)-missvalue).LE.0.001)
     &	            TEMP(I,J,K,ICOUNT)=-99999.9
                  if(IEEE_IS_NAN(TEMP(I,J,K,ICOUNT)))
     &              TEMP(I,J,K,ICOUNT)=-99999.9
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
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  SALT(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*
     &              scale+offset
                  if(abs(SALT(I,J,K,ICOUNT)-missvalue).LE.0.001)
     &	            SALT(I,J,K,ICOUNT)=-99999.9
                  if(IEEE_IS_NAN(SALT(I,J,K,ICOUNT)))
     &              SALT(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO
          ENDDO

          STATUS = NF_INQ_VARID(NCID,'water_u',IDVAR)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
          ICOUNT=N0
          DO N=1,NT
            ICOUNT=ICOUNT+1
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  u(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*
     &              scale+offset
                  if(abs(u(I,J,K,ICOUNT)-missvalue).LE.0.001)
     &	            u(I,J,K,ICOUNT)=-99999.9
                  if(IEEE_IS_NAN(U(I,J,K,ICOUNT)))
     &              U(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO
          ENDDO

          STATUS = NF_INQ_VARID(NCID,'water_v',IDVAR)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
          ICOUNT=N0
          DO N=1,NT
            ICOUNT=ICOUNT+1
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  v(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*
     &              scale+offset
                  if(abs(v(I,J,K,ICOUNT)-missvalue).LE.0.001)
     &	            v(I,J,K,ICOUNT)=-99999.9
                  if(IEEE_IS_NAN(V(I,J,K,ICOUNT)))
     &              V(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO
          ENDDO

          NREC=ICOUNT
          STATUS=NF_CLOSE(NCID)

!! Open NCOM tide file if it exists
          FIN=TRIM(FILE_NCOM_TIDE(IFILE))
          INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
          IF(FEXIST) THEN
            STATUS = NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
            IF(STATUS.NE.NF_NOERR) then
              write(*,*) 'Error message= ',status
              stop  !!'open NCOM tide netCDF file of failed'
            ENDIF  
            STATUS=NF_INQ_VARID(NCID,'SEA_LEVEL',IDVAR)
            STATUS=NF_GET_ATT_REAL(NCID,IDVAR,'FillValue',fillvalue)
            STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tmp3d)
	    write(*,*) 'sea_level fillvalue=',fillvalue

            ICOUNT=N0
            DO N=1,NT
	      ICOUNT=ICOUNT+1
              DO I=1,ISUB
                DO J=1,JSUB
	          tide_NCOM(I,J,ICOUNT)=TMP3D(IMIN+I-1,JMIN+J-1,N)
                  if(abs(TMP3D(IMIN+I-1,JMIN+J-1,N)).GT.29.0.or.
     &               ABS(WL(I,J,ICOUNT)).GE.29.0) then
       	            diff=-99999.9
                  else  
cc subtract tide from NCOM total water level
                    diff=WL(I,J,ICOUNT)-tide_NCOM(I,J,ICOUNT)
                  endif
	          write(26,100) N,I,J,lonsub(i,j),latsub(i,j),
     & 	            WL(I,J,ICOUNT),tide_NCOM(I,J,ICOUNT),diff
                  WL(I,J,ICOUNT)=diff    
cc Note: nontidal WL is about 20 cm lower based on comparison made by Zizang
                ENDDO	
              ENDDO	      	
            ENDDO
100   FORMAT(3I8,10F12.4)
            STATUS=NF_CLOSE(NCID)
	  ELSE
            WRITE(*,*) 'NCOM tide file does not match NCOM file'
            WRITE(*,*) 'Switch DBASE_WL=ETSS'
            WRITE(ICORMS,'(a)') 'Switch DBASE_WL from NCOM to ETSS' 
            DBASE_WL='ETSS'
          ENDIF	   
        ENDDO
        NT=NREC 
        write(*,*) 'Number of NCOM data NREC= ',NREC
      ENDIF

C-----------------------------------------------------------------------
C  Open and read all RTOFS netCDF files available during start time to end time       
C-----------------------------------------------------------------------
      IF(TRIM(DBASE_TS).EQ.'RTOFS') THEN
        NCOM_FILE=TRIM(DBASE_TS)//"_FILE"
        IFILE_NCOM=0
        INQUIRE(FILE=trim(NCOM_FILE),EXIST=FEXIST)
        IF(FEXIST) THEN
          CLOSE(60)
          OPEN(60,file=trim(NCOM_FILE))
          DO I=1,999
            READ(60,'(a200)',END=335) BUFFER
            FILE_NCOM(I)=trim(adjustL(BUFFER))
          ENDDO
335       CLOSE(60)
          IFILE_NCOM=I-1
        ENDIF
        WRITE(*,*) 'Total number of RTOFS FILES= ',IFILE_NCOM

        IF(IFILE_NCOM.LT.1) THEN
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

C-----------------------------------------------------------------------
C  Open and read all RTOFS netCDF files available during start time to end time       
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
        WRITE(*,*) 'RTOFS IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
        IF(ALLOCATED(lon)) DEALLOCATE(lon)
        IF(ALLOCATED(lat)) DEALLOCATE(lat)
        IF(ALLOCATED(depth)) DEALLOCATE(depth)
        IF(ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
        IF(ALLOCATED(ts_time)) DEALLOCATE(ts_time)
        ALLOCATE(lon(IM,JM))
        ALLOCATE(lat(IM,JM))
        ALLOCATE(depth(KB))
        ALLOCATE(zeta_time(NMAX))
        ALLOCATE(ts_time(NMAX))

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='Longitude'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
C        WRITE(*,*) 'DIM Number of Longitude ',NDIM
        IF(NDIM.EQ.2) THEN
          DO I=1,IM
            DO J=1,JM
              lon(i,j)=TMP4D(i,j,1,1)
              if(lon(i,j).gt.180.0) lon(i,j)=lon(i,j)-360.0
            ENDDO
          ENDDO
        ELSEIF(NDIM.EQ.1) THEN
          DO I=1,IM
            DO J=1,JM
              lon(i,j)=TMP4D(i,1,1,1)
              if(lon(i,j).gt.180.0) lon(i,j)=lon(i,j)-360.0
            ENDDO
          ENDDO
        ENDIF  

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='Latitude'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        IF(NDIM.EQ.2) THEN
          DO I=1,IM
            DO J=1,JM
              lat(i,j)=TMP4D(i,j,1,1)
            ENDDO
          ENDDO
        ELSEIF(NDIM.EQ.1) THEN
          DO I=1,IM
            DO J=1,JM
              lat(i,j)=TMP4D(J,1,1,1)
            ENDDO
          ENDDO
        ENDIF

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='Depth'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
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
        DO I=1,IM
          DO J=1,JM
            IF(lon(i,j).GE.minlon.and.lon(i,j).LE.maxlon.and.
     &         lat(i,j).GE.minlat.and.lat(i,j).LE.maxlat) then
              IF(I.LT.IMIN) IMIN=I 	  
              IF(I.GT.IMAX) IMAX=I 	  
              IF(J.LT.JMIN) JMIN=J 	  
              IF(J.GT.JMAX) JMAX=J
            ENDIF
          ENDDO
        ENDDO	  	  

        ISUB=IMAX-IMIN+1	 
        JSUB=JMAX-JMIN+1	 
        write(*,*) 'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX

C-----------------------------------------------------------------------
C allocate sizes of arrays for RTOFS products 
C-----------------------------------------------------------------------
        NNSUB=ISUB*JSUB
        IF(ALLOCATED(lonsub)) DEALLOCATE(lonsub)
        IF(ALLOCATED(latsub)) DEALLOCATE(latsub)
        IF(ALLOCATED(temp)) DEALLOCATE(temp)
        IF(ALLOCATED(salt)) DEALLOCATE(salt)
        IF(ALLOCATED(u)) DEALLOCATE(u)
        IF(ALLOCATED(v)) DEALLOCATE(v)
        IF(ALLOCATED(WL)) DEALLOCATE(WL)
        IF(ALLOCATED(ubar_rtofs)) DEALLOCATE(ubar_rtofs)
        IF(ALLOCATED(vbar_rtofs)) DEALLOCATE(vbar_rtofs)
        allocate(lonsub(ISUB,JSUB))
        allocate(latsub(ISUB,JSUB))
        allocate(temp(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        allocate(salt(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        allocate(u(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        allocate(v(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        allocate(WL(ISUB,JSUB,NT*IFILE_NCOM+10))
        allocate(ubar_rtofs(ISUB,JSUB,NT*IFILE_NCOM+10))
        allocate(vbar_rtofs(ISUB,JSUB,NT*IFILE_NCOM+10))

        DO N=1,NT*IFILE_NCOM+10
          DO J=1,JSUB
            DO I=1,ISUB
              WL(I,J,N)=0.0
              ubar_rtofs(I,J,N)=0.0
              vbar_rtofs(I,J,N)=0.0
            ENDDO
          ENDDO
        ENDDO   

        DO J=1,JSUB
          DO I=1,ISUB
            lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
            latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
	    write(79,'(2F12.4,2I6)') lonsub(i,j),latsub(i,j),I,J
          ENDDO
        ENDDO  
        close(79) 

        ICOUNT=0
        NREC=0
        TIMELAST=-99999.9	 
        DO IFILE=1,IFILE_NCOM
          FIN=TRIM(FILE_NCOM(IFILE))
          VNAME='MT'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
          ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          NT=DIMS(1)
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,6)
          ANAME=trim(adjustL(ANAME))
          LEN=LEN_TRIM(ANAME)
          LL=INDEX(ANAME,'minute')         
          IF(LL.GT.0) scale_time=1.0/1440.0
          LL=INDEX(ANAME,'hour')         
          IF(LL.GT.0) scale_time=1.0/24.0
          LL=INDEX(ANAME,'day')         
          IF(LL.GT.0) scale_time=1.0
          LL=INDEX(ANAME,'since')         
          IF(LL.GT.0) then
            read(ANAME(LL+6:LEN),80) IYR,IMM,IDD,IHH
          else
            write(*,*) 'There is error while reading base date'
            stop
          endif    

          yearb=IYR
          monthb=IMM
          dayb=IDD
          hourb=IHH
          jday=JULIAN(yearb,monthb,dayb,hourb)
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,7)
          TIME_NCOM=TMP4D(1,1,1,1)*scale_time+jday-jbase_date
          IF(TIMELAST.LT.TIME_NCOM) THEN
            ICOUNT=ICOUNT+1
            TS_TIME(ICOUNT)=TIME_NCOM
            TIMELAST=TIME_NCOM 
          ENDIF

          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop  !!'open RTOFS netCDF file of failed'
          ENDIF  
          IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
          ALLOCATE(tmp1d(IM))
          IF(ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
          ALLOCATE(tmp3d(IM,JM,NT))
          IF(ALLOCATED(itmp3d)) DEALLOCATE(itmp3d)
          ALLOCATE(itmp3d(IM,JM,NT))
          IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
          ALLOCATE(tmp4d(IM,JM,KB,NT))

          STATUS = NF_INQ_VARID(NCID,'temperature',IDVAR)
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  TEMP(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
                  if(abs(TEMP(I,J,K,ICOUNT)).GE.99.0)
     &	            TEMP(I,J,K,ICOUNT)=-99999.9
                  if(IEEE_IS_NAN(TEMP(I,J,K,ICOUNT)))
     &              TEMP(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO	

          STATUS = NF_INQ_VARID(NCID,'salinity',IDVAR)
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  SALT(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
                  if(abs(SALT(I,J,K,ICOUNT)).GE.99.0)
     &	            SALT(I,J,K,ICOUNT)=-99999.9
                  if(IEEE_IS_NAN(SALT(I,J,K,ICOUNT)))
     &              SALT(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO	

          STATUS = NF_INQ_VARID(NCID,'u',IDVAR)   
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  U(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
                  if(abs(U(I,J,K,ICOUNT)).GE.99.0)
     &	            U(I,J,K,ICOUNT)=-99999.9
                  if(IEEE_IS_NAN(U(I,J,K,ICOUNT)))
     &              U(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO

          STATUS = NF_INQ_VARID(NCID,'v',IDVAR)
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  V(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
                  if(abs(V(I,J,K,ICOUNT)).GE.99.0)
     &	            V(I,J,K,ICOUNT)=-99999.9
                  if(IEEE_IS_NAN(V(I,J,K,ICOUNT)))
     &              V(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO

cc calculate ubar and vbar from U and V
          DO I=1,ISUB
            DO J=1,JSUB
              ubar_rtofs(I,J,ICOUNT)=0.0
              vbar_rtofs(I,J,ICOUNT)=0.0
              KMAX1=0
              DO K=1,KB
                if(abs(U(I,J,K,ICOUNT)).LT.99.0) KMAX1=KMAX1+1
              ENDDO

              IF(KMAX1.EQ.1) THEN
                ubar_rtofs(I,J,ICOUNT)=U(I,J,KMAX1,ICOUNT)
                vbar_rtofs(I,J,ICOUNT)=V(I,J,KMAX1,ICOUNT)
              ELSE IF(KMAX1.GE.2) THEN
                DO K=1,KMAX1-1
                  ubar_rtofs(I,J,ICOUNT)=ubar_rtofs(I,J,ICOUNT)+
     &              (U(I,J,K+1,ICOUNT)+U(I,J,K,ICOUNT))/2.0*
     &              (depth(K+1)-depth(k))
                  vbar_rtofs(I,J,ICOUNT)=vbar_rtofs(I,J,ICOUNT)+
     &              (V(I,J,K+1,ICOUNT)+V(I,J,K,ICOUNT))/2.0*
     &              (depth(K+1)-depth(k))
                ENDDO
                ubar_rtofs(I,J,ICOUNT)=ubar_rtofs(I,J,ICOUNT)/
     &            depth(KMAX1)
                vbar_rtofs(I,J,ICOUNT)=vbar_rtofs(I,J,ICOUNT)/
     &            depth(KMAX1)
              END IF
            ENDDO	
          ENDDO	
cc end calculate UBAR and VBAR
          NREC=ICOUNT
          STATUS=NF_CLOSE(NCID)
        ENDDO      
        NT=NREC 
        write(*,*) 'Number of RTOFS data NREC= ',NREC
      ENDIF

C-----------------------------------------------------------------------
C  Open and read all HYCOM netCDF files available during start time to end time       
C-----------------------------------------------------------------------
      IF(TRIM(DBASE_TS).EQ.'HYCOM' )THEN
        NCOM_FILE=TRIM(DBASE_TS)//"_FILE"
        IFILE_NCOM=0
        INQUIRE(FILE=trim(NCOM_FILE),EXIST=FEXIST)
        IF(FEXIST) THEN
          CLOSE(60)
          OPEN(60,file=trim(NCOM_FILE))
          DO I=1,999
            READ(60,'(a200)',END=325) BUFFER
            FILE_NCOM(I)=trim(adjustL(BUFFER))
          ENDDO
325       CLOSE(60)
          IFILE_NCOM=I-1
        ENDIF
        WRITE(*,*) 'Total number of HYCOM FILES= ',IFILE_NCOM

        IF(IFILE_NCOM.LT.1) THEN
          WRITE(*,*) 'No HYCOM file is available in this period'
          WRITE(*,*) 'use WOA05 climatological dataset to replace'
           
          DBASE_TS='WOA05'
          IF(TRIM(DBASE_WL).EQ.'HYCOM') DBASE_WL='ETSS'
          WRITE(ICORMS,'(a)')'HYCOM FILE IS NOT FOUND' 
          WRITE(ICORMS,'(a)')'Failed due to no RTOFS and HYCOM'
!          WRITE(ICORMS,'(a)')'USE CLIMATOLOGIC BACKUP WOA05' 
          WRITE(*,*)'FATAL ERROR: NO AVAILABLE RTOFS OR HYCOM FOR OBC'
          STOP
!          GOTO 30   turn off using WOA for open boundary conditions
        ENDIF  

        DO I=1,IFILE_NCOM
          WRITE(*,*) 'HYCOM I=',I,TRIM(FILE_NCOM(I))
        ENDDO

C-----------------------------------------------------------------------
C  Open and read all HYCOM netCDF files available during start time to end time       
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
        WRITE(*,*) 'HYCOM IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
        IF(ALLOCATED(lon)) DEALLOCATE(lon)
        IF(ALLOCATED(lat)) DEALLOCATE(lat)
        IF(ALLOCATED(depth)) DEALLOCATE(depth)
        IF(ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
        IF(ALLOCATED(ts_time)) DEALLOCATE(ts_time)
        ALLOCATE(lon(IM,JM))
        ALLOCATE(lat(IM,JM))
        ALLOCATE(depth(KB))
        ALLOCATE(zeta_time(NMAX))
        ALLOCATE(ts_time(NMAX))

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='lon'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        WRITE(*,*) 'DIM Number of Longitude ',NDIM
        IF(NDIM.EQ.2) THEN
          DO I=1,IM
            DO J=1,JM
              lon(i,j)=TMP4D(i,j,1,1)
              if(lon(i,j).gt.180.0) lon(i,j)=lon(i,j)-360.0
            ENDDO
          ENDDO
        ELSEIF(NDIM.EQ.1) THEN
          DO I=1,IM
            DO J=1,JM
              lon(i,j)=TMP4D(i,1,1,1)
              if(lon(i,j).gt.180.0) lon(i,j)=lon(i,j)-360.0
            ENDDO
          ENDDO
        ENDIF  

        DO I=1,4
         DIMS(I)=1
        ENDDO	
        VNAME='lat'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        WRITE(*,*) 'DIM Number of Latitude ',NDIM
        IF(NDIM.EQ.2) THEN
          DO I=1,IM
            DO J=1,JM
              lat(i,j)=TMP4D(i,j,1,1)
            ENDDO
          ENDDO
        ELSEIF(NDIM.EQ.1) THEN
          DO I=1,IM
            DO J=1,JM
              lat(i,j)=TMP4D(J,1,1,1)
            ENDDO
          ENDDO
        ENDIF

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='depth'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
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
        DO I=1,IM
          DO J=1,JM
            IF(lon(i,j).GE.minlon.and.lon(i,j).LE.maxlon.and.
     &         lat(i,j).GE.minlat.and.lat(i,j).LE.maxlat) then
              IF(I.LT.IMIN) IMIN=I 	  
              IF(I.GT.IMAX) IMAX=I 	  
              IF(J.LT.JMIN) JMIN=J 	  
              IF(J.GT.JMAX) JMAX=J
            ENDIF
          ENDDO
        ENDDO	  	  
        ISUB=IMAX-IMIN+1	 
        JSUB=JMAX-JMIN+1	 
        write(*,*) 'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX

C-----------------------------------------------------------------------
C allocate sizes of arrays for HYCOM products 
C-----------------------------------------------------------------------
        NNSUB=ISUB*JSUB
        IF(ALLOCATED(lonsub)) DEALLOCATE(lonsub)
        IF(ALLOCATED(latsub)) DEALLOCATE(latsub)
        IF(ALLOCATED(temp)) DEALLOCATE(temp)
        IF(ALLOCATED(salt)) DEALLOCATE(salt)
        IF(ALLOCATED(u)) DEALLOCATE(u)
        IF(ALLOCATED(v)) DEALLOCATE(v)
        IF(ALLOCATED(WL)) DEALLOCATE(WL)
        IF(ALLOCATED(ubar_rtofs)) DEALLOCATE(ubar_rtofs)
        IF(ALLOCATED(vbar_rtofs)) DEALLOCATE(vbar_rtofs)
        allocate(lonsub(ISUB,JSUB))
        allocate(latsub(ISUB,JSUB))
        allocate(temp(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        allocate(salt(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        allocate(u(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        allocate(v(ISUB,JSUB,KB,NT*IFILE_NCOM+10))
        allocate(WL(ISUB,JSUB,NT*IFILE_NCOM+10))
        allocate(tide_NCOM(ISUB,JSUB,NT*IFILE_NCOM+10))
        allocate(ubar_rtofs(ISUB,JSUB,NT*IFILE_NCOM+10))
        allocate(vbar_rtofs(ISUB,JSUB,NT*IFILE_NCOM+10))

        DO N=1,NT*IFILE_NCOM+10
          DO J=1,JSUB
            DO I=1,ISUB
              WL(I,J,N)=0.0
              tide_NCOM(I,J,N)=0.0
              ubar_rtofs(I,J,N)=0.0
              vbar_rtofs(I,J,N)=0.0
            ENDDO
          ENDDO
        ENDDO   

        CLOSE(79)
        OPEN(79,file='lonlat_HYCOM.dat')
        DO J=1,JSUB
          DO I=1,ISUB
            lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
            latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
	    write(79,'(2F12.4,2I6)') lonsub(i,j),latsub(i,j),I,J
          ENDDO
        ENDDO  
        close(79) 

        ICOUNT=0
        NREC=0
        TIMELAST=-99999.9	 
        DO IFILE=1,IFILE_NCOM
          write(*,*) 'Reading HYCOM NetCDF file...',IFILE
          FIN=TRIM(FILE_NCOM(IFILE))
          VNAME='time'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
          ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          NT=DIMS(1)
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,6)
          ANAME=trim(adjustL(ANAME))
          LEN=LEN_TRIM(ANAME)
          LL=INDEX(ANAME,'minute')         
          IF(LL.GT.0) scale_time=1.0/1440.0
          LL=INDEX(ANAME,'hour')         
          IF(LL.GT.0) scale_time=1.0/24.0
          LL=INDEX(ANAME,'day')         
          IF(LL.GT.0) scale_time=1.0
          LL=INDEX(ANAME,'since')         
          IF(LL.GT.0) then
            read(ANAME(LL+6:LEN),80) IYR,IMM,IDD,IHH
            write(*,*) 'basetime=',IYR,IMM,IDD,IHH
          else
            write(*,*) 'there is error while reading base date'
            stop
          endif    

          yearb=IYR
          monthb=IMM
          dayb=IDD
          hourb=IHH
          jday=JULIAN(yearb,monthb,dayb,hourb)
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,7)
          TIME_NCOM=TMP4D(1,1,1,1)*scale_time+jday-jbase_date
          IF(TIMELAST.LT.TIME_NCOM) THEN
            ICOUNT=ICOUNT+1
            TS_TIME(ICOUNT)=TIME_NCOM
            TIMELAST=TIME_NCOM 
          ENDIF

          STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop  !! 'open HYCOM netCDF file of failed'
          ENDIF  

          IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
          ALLOCATE(tmp1d(IM))
          IF(ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
          ALLOCATE(tmp3d(IM,JM,NT))
          IF(ALLOCATED(itmp3d)) DEALLOCATE(itmp3d)
          ALLOCATE(itmp3d(IM,JM,NT))
          IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
          ALLOCATE(tmp4d(IM,JM,KB,NT))

          STATUS = NF_INQ_VARID(NCID,'surf_el',IDVAR)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS = NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp3d)
          missvalue=IATT*scale+offset
          write(*,*) 'Factor=',scale,offset,missvalue
          write(*,*) 'First data=',tmp3d(1,1,1),tmp3d(2,1,1)

          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                WL(I,J,ICOUNT)=TMP3D(IMIN+I-1,JMIN+J-1,N)*scale+offset
                if(abs(WL(I,J,ICOUNT)-missvalue).LE.0.001) then
     	          WL(I,J,ICOUNT)=-99999.9
                else
                  IF(OFS.EQ.'gomofs'.or.OFS.eq.'GoMOFS'.or.
     &              OFS.eq.'GOMOFS') THEN
                    WL(I,J,ICOUNT)=WL(I,J,ICOUNT)+0.62 !suggested by developer
                  ENDIF
                endif
                if(IEEE_IS_NAN(WL(I,J,ICOUNT)))
     &            WL(I,J,ICOUNT)=-99999.9
              ENDDO	
            ENDDO	
          ENDDO	

          STATUS = NF_INQ_VARID(NCID,'water_temp',IDVAR)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS = NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
          missvalue=IATT*scale+offset
          write(*,*) 'Scale of temp=',scale,offset,missvalue

          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  TEMP(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*
     &              scale+offset
                  if(abs(TEMP(I,J,K,ICOUNT)-missvalue).LE.0.001)
     &	            TEMP(I,J,K,ICOUNT)=-99999.9
                  if(IEEE_IS_NAN(TEMP(I,J,K,ICOUNT)))
     &              TEMP(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO

          STATUS = NF_INQ_VARID(NCID,'salinity',IDVAR)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)

          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  SALT(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*
     &              scale+offset
                  if(abs(SALT(I,J,K,ICOUNT)-missvalue).LE.0.001)
     &	            SALT(I,J,K,ICOUNT)=-99999.9
                  if(IEEE_IS_NAN(SALT(I,J,K,ICOUNT)))
     &              SALT(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO	

          STATUS = NF_INQ_VARID(NCID,'water_u',IDVAR)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)

          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  U(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*
     &              scale+offset
                  if(abs(U(I,J,K,ICOUNT)-missvalue).LE.0.001)
     &	            U(I,J,K,ICOUNT)=-99999.9
                  if(abs(U(I,J,K,ICOUNT)).GE.99.0)
     &	            U(I,J,K,ICOUNT)=-99999.9
                  if(IEEE_IS_NAN(U(I,J,K,ICOUNT)))
     &              U(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO	

          STATUS = NF_INQ_VARID(NCID,'water_v',IDVAR)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
          STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
          STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)

          DO N=1,NT
            DO I=1,ISUB
              DO J=1,JSUB
                DO K=1,KB
                  v(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*
     &              scale+offset
                  if(abs(V(I,J,K,ICOUNT)-missvalue).LE.0.001)
     &	            V(I,J,K,ICOUNT)=-99999.9
                  if(abs(V(I,J,K,ICOUNT)).GE.99.0)
     &	            V(I,J,K,ICOUNT)=-99999.9
                  if(IEEE_IS_NAN(V(I,J,K,ICOUNT)))
     &              v(I,J,K,ICOUNT)=-99999.9
                ENDDO	
              ENDDO	
            ENDDO	
          ENDDO

cc calculate ubar and vbar from U and V
          DO I=1,ISUB
            DO J=1,JSUB
              ubar_rtofs(I,J,ICOUNT)=0.0
              vbar_rtofs(I,J,ICOUNT)=0.0
              KMAX1=0
              DO K=1,KB
                if(abs(U(I,J,K,ICOUNT)).LT.99.0) KMAX1=KMAX1+1
              ENDDO

              IF(KMAX1.EQ.1) THEN
                ubar_rtofs(I,J,ICOUNT)=U(I,J,KMAX1,ICOUNT)
                vbar_rtofs(I,J,ICOUNT)=V(I,J,KMAX1,ICOUNT)
              ELSE IF(KMAX1.GE.2) THEN
                DO K=1,KMAX1-1
                  ubar_rtofs(I,J,ICOUNT)=ubar_rtofs(I,J,ICOUNT)+
     &              (U(I,J,K+1,ICOUNT)+U(I,J,K,ICOUNT))/2.0*
     &              (depth(K+1)-depth(k))
                  vbar_rtofs(I,J,ICOUNT)=vbar_rtofs(I,J,ICOUNT)+
     &              (V(I,J,K+1,ICOUNT)+V(I,J,K,ICOUNT))/2.0*
     &              (depth(K+1)-depth(k))
                ENDDO
                ubar_rtofs(I,J,ICOUNT)=ubar_rtofs(I,J,ICOUNT)/
     &            depth(KMAX1)
                vbar_rtofs(I,J,ICOUNT)=vbar_rtofs(I,J,ICOUNT)/
     &            depth(KMAX1)
              END IF	
            ENDDO	
          ENDDO	
cc end calculate UBAR and VBAR
	
          NREC=ICOUNT
          STATUS=NF_CLOSE(NCID)
        ENDDO      
        NT=NREC 
        write(*,*) 'Number of HYCOM data NREC= ',NREC

        CLOSE(79)
        OPEN(79,file='salt_HYCOM.dat')
        DO N=1,1
          DO I=1,ISUB
            DO J=1,JSUB
              IF(SALT(I,J,1,N).GE.0.0) THEN
                WRITE(79,'(F10.4,2I5,100F9.3)')TS_TIME(N),I,J,
     &	          lonsub(i,j),latsub(i,j),(SALT(I,J,K,N),K=1,KB)
              ENDIF
            ENDDO	
          ENDDO	
        ENDDO	
	 
        CLOSE(79)
        OPEN(79,file='temp_HYCOM.dat')
        DO N=1,1
          DO I=1,ISUB
            DO J=1,JSUB
              IF(TEMP(I,J,1,N).GE.0.0) THEN
                WRITE(79,'(F10.4,2I5,100F9.3)')TS_TIME(N),I,J,
     &	          lonsub(i,j),latsub(i,j),(TEMP(I,J,K,N),K=1,KB)
              ENDIF
            ENDDO	
          ENDDO	
        ENDDO
        CLOSE(79)
      ENDIF

C-----------------------------------------------------------------------
C  End of reading date from WOA05 or NCOM, or RTOFS, or HYCOM products 
C-----------------------------------------------------------------------
      NSTR=1
      N=1
      DO WHILE ((TS_TIME(N).LT.day_start).and.(N.LT.NT)) 
        N=N+1
      ENDDO
      NSTR=N-1
      IF(NSTR.LE.0.OR.NSTR.GT.NT) THEN  !AJ 08/13/2010 extrapolation
        DO NN=NT,1,-1
          TS_TIME(NN+1)=TS_TIME(NN)
          DO I=1,ISUB
            DO J=1,JSUB
	      WL(I,J,NN+1)=WL(I,J,NN) 
              DO K=1,KB
                TEMP(I,J,K,NN+1)=TEMP(I,J,K,NN)
                SALT(I,J,K,NN+1)=SALT(I,J,K,NN)
              ENDDO	
            ENDDO	
          ENDDO
        ENDDO
        TS_TIME(1)=day_start
        NT=NT+1
        NSTR=1
      ENDIF

      IF(TS_TIME(NT).LT.day_end) THEN
        diff=day_end-TS_TIME(NT)
        IF(diff.LT.5.0) THEN 
C AJ ZHANG 03/18/2010, allow persistence for the maximum 5 days. 
          TS_TIME(NT+1)=day_end
          DO I=1,ISUB
            DO J=1,JSUB
              WL(I,J,NT+1)=WL(I,J,NT) 
              DO K=1,KB
                TEMP(I,J,K,NT+1)=TEMP(I,J,K,NT)
                SALT(I,J,K,NT+1)=SALT(I,J,K,NT)
              ENDDO	
            ENDDO	
          ENDDO
	  NT=NT+1
	ELSE  
          write(*,*) 'NCOM data is too shorter than day_end'
          TMPTXT='The forcast time period is not covered by NCOM'
          write(*,*) TRIM(TMPTXT)
          WRITE(ICORMS,'(a)') TRIM(DBASE_TS)//' is too short'
          WRITE(ICORMS,'(a)') 'USE CLIMATOLOGIC BACKUP WOA05'
          WRITE(ICORMS,'(a)') 'FATAL ERROR:no global RTOFS and HYCOM for OBC' 
          WRITE(*,*)'FATAL ERROR: no global RTOFS and HYCOM for OBC'
          DBASE_TS='WOA05'
          STOP 
        ENDIF  
      ENDIF 

      N=1
      DO WHILE ((TS_TIME(N).LT.day_end).and.(N.LT.NT)) 
        N=N+1
      ENDDO
      NEND=N 
      IF(N.GT.NT) THEN
        write(*,*) 'Time period is not covered by dataset'
        TMPTXT='Time period is not covered by dataset'
        WRITE(ICORMS,'(a)') TRIM(TMPTXT)
        WRITE(ICORMS,'(a)') 'Generation of OBC failed'
        WRITE(ICORMS,'(a)') 'stop in nos_ofs_create_OBC.f'
        stop  !!'stop in nos_ofs_create_OBC.f'
      ENDIF  	  
      write(*,*) 'Start and end time of = ',
     &  TS_time(NSTR),TS_time(NEND),NSTR,NEND,day_start,day_end 		
      NTMAX=NEND-NSTR+1
      IF(ALLOCATED(TIMEOBC)) DEALLOCATE(TIMEOBC)
      ALLOCATE(TIMEOBC(NTMAX))
      DO N=1,NTMAX
        TIMEOBC(N)=TS_TIME(NSTR+N-1)
      ENDDO
      IF(TRIM(DBASE_WL).EQ.'NCOM'.OR.TRIM(DBASE_WL).EQ.'HYCOM') THEN
        NTMAX_WL=NTMAX
        IF(ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
        ALLOCATE(zeta_time(NTMAX_WL))
        DO N=1,NTMAX_WL
          zeta_time(N)=TIMEOBC(N)
        ENDDO
      ENDIF	
      KMAX=KB	
      K=1
      DO WHILE ((depth(k).LT.hmax).AND.(K.LT.KB))
        K=K+1
      ENDDO
      KMAX=K

      WRITE(*,*) 'MAX Vertical Levels needed is ',KMAX,
     &  depth(KMAX),hmax
C-----------------------------------------------------------------------
C  End of computing time coverage and maximum vertical layer 
C  Begin to process lateral open boundary conditions 
C-----------------------------------------------------------------------
      IF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
	IF(ALLOCATED(Iout)) DEALLOCATE(Iout)
	IF(ALLOCATED(Jout)) DEALLOCATE(Jout)
	IF(ALLOCATED(outm)) DEALLOCATE(outm)
	IF(ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
        allocate(Iout(NOBC,1))
        allocate(Jout(NOBC,1))
        allocate(outm(NOBC,1))
        allocate(TMP2D(ISUB,JSUB))
        TMPTXT='Search coarser grid indeces directions are done!!!'
        write(*,*) TRIM(TMPTXT)
        if(IGRD.eq.2) iflag=1
        if(IGRD.eq.3) iflag=0
        CALL iNTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &       NOBC,1,lonOBC,latOBC,outm,Iout,Jout,0)
      ENDIF 

      IF(ALLOCATED(WLOBC)) DEALLOCATE(WLOBC)
      ALLOCATE(WLOBC(NOBC,NTMAX))
      IF(ALLOCATED(tempOBC)) DEALLOCATE(tempOBC)
      ALLOCATE(tempOBC(NOBC,KB,NTMAX))
      IF(ALLOCATED(saltOBC)) DEALLOCATE(saltOBC)
      ALLOCATE(saltOBC(NOBC,KB,NTMAX))
      IF(ALLOCATED(uOBC)) DEALLOCATE(uOBC)  
      ALLOCATE(uOBC(NOBC,KB,NTMAX))  
      IF(ALLOCATED(vOBC)) DEALLOCATE(vOBC) 
      ALLOCATE(vOBC(NOBC,KB,NTMAX)) 

      IF(ALLOCATED(tempOBC_M)) DEALLOCATE(tempOBC_M)
      ALLOCATE(tempOBC_M(NOBC,KBm,NTMAX))
      IF(ALLOCATED(saltOBC_M)) DEALLOCATE(saltOBC_M)
      ALLOCATE(saltOBC_M(NOBC,KBm,NTMAX))
      IF(ALLOCATED(uOBC_M)) DEALLOCATE(uOBC_M) 
      ALLOCATE(uOBC_M(NOBC,KBm,NTMAX)) 
      IF(ALLOCATED(vOBC_M)) DEALLOCATE(vOBC_M) 
      ALLOCATE(vOBC_M(NOBC,KBm,NTMAX)) 

      IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
        IF(ALLOCATED(XINP)) DEALLOCATE(XINP)
        IF(ALLOCATED(YINP)) DEALLOCATE(YINP)
        IF(ALLOCATED(ZINP)) DEALLOCATE(ZINP)
        IF(ALLOCATED(XOUT)) DEALLOCATE(XOUT)
        IF(ALLOCATED(YOUT)) DEALLOCATE(YOUT)
        IF(ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
        IF(ALLOCATED(weightnodes)) DEALLOCATE(weightnodes)
        IF(ALLOCATED(weights)) DEALLOCATE(weights)
        allocate(XINP(NNSUB))
        allocate(YINP(NNSUB))
        allocate(ZINP(NNSUB))
        allocate(XOUT(NOBC))
        allocate(YOUT(NOBC))
        allocate(ZOUT(NOBC))
        allocate(weightnodes(NOBC,3))
        allocate(weights(NOBC,3))
        call search_output(ISUB,JSUB,lonsub,latsub,NOBC,lonOBC,
     &    latOBC,nselect_parent,IPARENT,JPARENT)
      ENDIF

c------------------------------------------------------------------
C  Processing Water Level open boundary
c------------------------------------------------------------------
      IF(TRIM(DBASE_WL).EQ.'NCOM'.OR.TRIM(DBASE_WL).EQ.'HYCOM') THEN
        DO N=NSTR,NEND
	  N0=N-NSTR+1
          IF(IGRD.EQ.2.or.IGRD.EQ.3) THEN   
cc Spatial interpolation using bicubic or bilinear
            DO I=1,ISUB
              DO J=1,JSUB
                tmp2d(I,J)=WL(I,J,N)
              ENDDO
            ENDDO
            if(IGRD.eq.2) iflag=1
            if(IGRD.eq.3) iflag=0
            CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &           NOBC,1,lonOBC,latOBC,outm,Iout,Jout,1)
            DO I=1,NOBC
              WLOBC(I,N0)=outm(I,1)
              IF(I.GT.1) THEN
                IF(ABS(WLOBC(I,N0)).GT.999.0) THEN
                  TMPTXT='No valid value after regrid interpolation'
C                  WRITE(*,*) TRIM(TMPTXT)
                  TMPTXT='Use valid value from nearby water point'
C                  WRITE(*,*) TRIM(TMPTXT)
                  IF(ABS(WLOBC(I-1,N0)).LT.999.0) THEN
                    WLOBC(I,N0)=WLOBC(I-1,N0)
                  ENDIF
                ENDIF 
              ENDIF  
	    ENDDO   
 	  ELSEIF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
cc Using remesh routine or nature neighbors spatial interpolation
            NDUM=0 
            DO NS=1,NSELECT_PARENT
              IF(WL(IPARENT(NS),JPARENT(NS),N).GT.-99.0) THEN
                NDUM=NDUM+1
                XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
                ZINP(ndum)=WL(IPARENT(NS),JPARENT(NS),N)
              ENDIF	
            ENDDO	
	    NDATA=NDUM
            IF(IGRD.EQ.1) THEN           !! using remesh
              call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     *             NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,0)
              call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     *             NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,1)
            ELSEIF(IGRD.EQ.4) THEN       !! using nature neighbors
              call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     *             NOBC,lonOBC,latOBC,ZOUT)
            ENDIF
            DO I=1,NOBC
              WLOBC(I,N0)=ZOUT(I)
	    ENDDO   
          ENDIF
        ENDDO  ! end of N LOOP
      ENDIF

c------------------------------------------------------------------
C  processing Ubar and Vbar open boundary
c------------------------------------------------------------------
      IF(TRIM(DBASE_TS).EQ.'RTOFS'.OR.TRIM(DBASE_TS).EQ.'HYCOM') THEN
        IF(ALLOCATED(ubarOBC)) DEALLOCATE(ubarOBC)
        IF(ALLOCATED(vbarOBC)) DEALLOCATE(vbarOBC)
        ALLOCATE(ubarOBC(NOBC,NTMAX))   
        ALLOCATE(vbarOBC(NOBC,NTMAX)) 
        DO N=NSTR,NEND
          N0=N-NSTR+1
          IF(IGRD.EQ.2.or.IGRD.EQ.3) THEN   
cc Spatial interpolation using bicubic or bilinear
            DO I=1,ISUB
              DO J=1,JSUB
                tmp2d(I,J)=ubar_rtofs(I,J,N)
              ENDDO
            ENDDO
            if(IGRD.eq.2) iflag=1
            if(IGRD.eq.3) iflag=0
            CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &           NOBC,1,lonOBC,latOBC,outm,Iout,Jout,1)
            DO I=1,NOBC
              ubarOBC(I,N0)=outm(I,1)
              IF(I.GT.1) THEN
                IF(ABS(ubarOBC(I,N0)).GT.999.0) THEN
                  TMPTXT='No valid value after regrid interpolation'
C                  WRITE(*,*) TRIM(TMPTXT)
                  TMPTXT='Use valid value from nearby water point'
C                  WRITE(*,*) TRIM(TMPTXT)
                  IF(ABS(ubarOBC(I-1,N0)).LT.999.0) THEN
                    ubarOBC(I,N0)=ubarOBC(I-1,N0)
                  ENDIF
                ENDIF   
              ENDIF
            ENDDO

            DO I=1,ISUB
              DO J=1,JSUB
                tmp2d(I,J)=vbar_rtofs(I,J,N)
              ENDDO
            ENDDO
            CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &           NOBC,1,lonOBC,latOBC,outm,Iout,Jout,1)
            DO I=1,NOBC
              vbarOBC(I,N0)=outm(I,1)
              IF(I.GT.1) THEN
                IF(ABS(vbarOBC(I,N0)).GT.999.0) THEN
                  TMPTXT='No valid value after regrid interpolation'
C                  WRITE(*,*) TRIM(TMPTXT)
                  TMPTXT='Use valid value from nearby water point'
C                  WRITE(*,*) TRIM(TMPTXT)
                  IF(ABS(vbarOBC(I-1,N0)).LT.999.0) THEN
                    vbarOBC(I,N0)=vbarOBC(I-1,N0)
                  ENDIF
                ENDIF   
              ENDIF
            ENDDO

 	  ELSEIF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN   
cc Using remesh routine or nature neighbors spatial interpolation
            NDUM=0
            DO NS=1,NSELECT_PARENT
              IF(abs(ubar_rtofs(IPARENT(NS),JPARENT(NS),N)).LE.
     &           90.0) THEN
                NDUM=NDUM+1
                XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
                ZINP(ndum)=ubar_rtofs(IPARENT(NS),JPARENT(NS),N)
              ENDIF	
            ENDDO	
            NDATA=NDUM
            IF(IGRD.EQ.1) THEN           !! using remesh
              call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &             NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,0)
              call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &             NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,1)
            ELSEIF(IGRD.EQ.4) THEN       !! using nature neighbors
              call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &             NOBC,lonOBC,latOBC,ZOUT)
            ENDIF

            DO I=1,NOBC
              ubarOBC(I,N0)=ZOUT(I)
            ENDDO
            NDUM=0                                     
            DO NS=1,NSELECT_PARENT
              IF(abs(vbar_rtofs(IPARENT(NS),JPARENT(NS),N)).LE.
     &           90.0) THEN
                NDUM=NDUM+1
                XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
                ZINP(ndum)=vbar_rtofs(IPARENT(NS),JPARENT(NS),N)
              ENDIF	
            ENDDO	

            NDATA=NDUM
            IF(IGRD.EQ.1) THEN           !! using remesh
              call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     *             NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,0)
              call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     *              NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,1)
    	    ELSEIF(IGRD.EQ.4) THEN       !! using nature neighbors
              call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     *             NOBC,lonOBC,latOBC,ZOUT)
            ENDIF
            DO I=1,NOBC
              vbarOBC(I,N0)=ZOUT(I)
            ENDDO
          ENDIF
        ENDDO  ! end of N LOOP

        close(55)
        open(55,file='ubar_rtofs.dat')
        write(55,334)1,(TIMEOBC(N),N=1,NTMAX)
        DO I=1,NOBC
          write(55,334)I,(ubarOBC(I,N),N=1,NTMAX)
        ENDDO
334     FORMAT(I5,200F8.3)   
        close(55)

        open(55,file='vbar_rtofs.dat')
        write(55,334)1,(TIMEOBC(N),N=1,NTMAX)
        DO I=1,NOBC
          write(55,334)I,(vbarOBC(I,N),N=1,NTMAX)
        ENDDO
      ENDIF

c------------------------------------------------------------------
C  Processing Temperature and salinity open boundary
c------------------------------------------------------------------
300   CONTINUE
      DO K=1,KMAX
        IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN  
          NDUM=0
          DO NS=1,NSELECT_PARENT
            IF(abs(TEMP(IPARENT(NS),JPARENT(NS),K,1)).LE.90.0) THEN
              NDUM=NDUM+1
              XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
              YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
              ZINP(ndum)=temp(IPARENT(NS),JPARENT(NS),K,1)
            ENDIF	
          ENDDO	
          NDATA=NDUM
          write(*,*) 'NDATA= ',NDATA, 'at k',K,depth(K)
          IF(IGRD.EQ.1.AND.NDATA.GT.3) THEN                                  
            write(*,*) 'COMPUTE WEIGHTS AND NODES FOR REMESH !!!'
            call INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     *           lonOBC,latOBC,ZOUT,weightnodes,weights,0)
          ENDIF
        ENDIF 

        DO N=NSTR,NEND
          N0=N-NSTR+1
          IF(IGRD.EQ.2.or.IGRD.EQ.3) THEN   
cc Spatial interpolation using bicubic or bilinear
            DO I=1,ISUB
              DO J=1,JSUB
                tmp2d(I,J)=TEMP(I,J,K,N)
              ENDDO
            ENDDO
            if(IGRD.eq.2) iflag=1
            if(IGRD.eq.3) iflag=0
            CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &           NOBC,1,lonOBC,latOBC,outm,Iout,Jout,1)
            DO I=1,NOBC
              tempOBC(I,K,N0)=outm(I,1)
              IF(I.GT.1) THEN
                IF(ABS(tempOBC(I,K,N0)).GT.999.0) THEN
                  TMPTXT='No valid value after regrid interpolation'
C                  WRITE(*,*) TRIM(TMPTXT)
                  TMPTXT='Use valid value from nearby water point'
C                  WRITE(*,*) TRIM(TMPTXT)
C                  write(*,*) tempOBC(I-1,K,N0),tempOBC(I,K,N0)
                  IF(ABS(tempOBC(I-1,K,N0)).LT.999.0) THEN
                    tempOBC(I,K,N0)=tempOBC(I-1,K,N0)
                  ENDIF
                ENDIF   
              ENDIF
            ENDDO

            DO I=1,ISUB
              DO J=1,JSUB
                tmp2d(I,J)=SALT(I,J,K,N)
              ENDDO
            ENDDO
            CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &           NOBC,1,lonOBC,latOBC,outm,Iout,Jout,1)
            DO I=1,NOBC
              saltOBC(I,K,N0)=outm(I,1)
              IF(I.GT.1) THEN
                IF(ABS(saltOBC(I,K,N0)).GT.999.0) THEN
                  TMPTXT='No valid value after regrid interpolation'
C                  WRITE(*,*) TRIM(TMPTXT)
                  TMPTXT='Use valid value from nearby water point'
C                  WRITE(*,*) TRIM(TMPTXT)
                  IF(ABS(saltOBC(I-1,K,N0)).LT.999.0) THEN
                    saltOBC(I,K,N0)=saltOBC(I-1,K,N0)
                  ENDIF
                ENDIF   
              ENDIF
            ENDDO

            DO I=1,ISUB
              DO J=1,JSUB
                tmp2d(I,J)=U(I,J,K,N)
              ENDDO
            ENDDO
            CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &           NOBC,1,lonOBC,latOBC,outm,Iout,Jout,1)
            DO I=1,NOBC
              uOBC(I,K,N0)=outm(I,1)
              IF(I.GT.1) THEN 
                IF(ABS(uOBC(I,K,N0)).GT.999.0) THEN
                  TMPTXT='No valid value after regrid interpolation'
C                  WRITE(*,*) TRIM(TMPTXT)
                  TMPTXT='Use valid value from nearby water point'
C                  WRITE(*,*) TRIM(TMPTXT)
                  IF(ABS(uOBC(I-1,K,N0)).LT.999.0) THEN
                    uOBC(I,K,N0)=uOBC(I-1,K,N0)
                  ENDIF
                ENDIF   
              ENDIF
            ENDDO

            DO I=1,ISUB
              DO J=1,JSUB
                tmp2d(I,J)=V(I,J,K,N)
              ENDDO
            ENDDO
            CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &           NOBC,1,lonOBC,latOBC,outm,Iout,Jout,1)
            DO I=1,NOBC
              vOBC(I,K,N0)=outm(I,1)
              IF(I.GT.1) THEN 
                IF(ABS(vOBC(I,K,N0)).GT.999.0) THEN
                  TMPTXT='No valid value after regrid interpolation'
C                  WRITE(*,*) TRIM(TMPTXT)
                  TMPTXT='Use valid value from nearby water point'
C                  WRITE(*,*) TRIM(TMPTXT)
                  IF(ABS(vOBC(I-1,K,N0)).LT.999.0) THEN
                    vOBC(I,K,N0)=vOBC(I-1,K,N0)
                  ENDIF
                ENDIF   
              ENDIF 
            ENDDO

          ELSEIF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN   
cc Using remesh routine or nature neighbors spatial interpolation
            NDUM=0
            DO NS=1,NSELECT_PARENT
              IF(abs(temp(IPARENT(NS),JPARENT(NS),K,N)).LE.90.0) THEN
                NDUM=NDUM+1
                XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
                ZINP(ndum)=temp(IPARENT(NS),JPARENT(NS),K,N)
              ENDIF	
            ENDDO	
            IF(NDUM.EQ.0) THEN
              WRITE(*,*) 'There is no parent data point at k=',
     &          K,depth(k)
              write(*,*) 'Extrapolate upper level (k-1) to K' 
              DO I=1,NOBC
                tempOBC(I,K,N0)= tempOBC(I,K-1,N0)         
              ENDDO
            ELSE
              IF(NDUM.NE.NDATA) THEN
                WRITE(*,*) 'NDUM of TEMP= ',NDUM,NDATA
                WRITE(*,*) 'NDATA is not equal to NDUM in Temp!'
                NDATA=NDUM	
                IF(IGRD.EQ.1)                                  
     &            call INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &                 lonOBC,latOBC,ZOUT,weightnodes,weights,0)
              ENDIF   

              IF(IGRD.EQ.1) THEN         !! using remesh
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &               lonOBC,latOBC,ZOUT,weightnodes,weights,1)
              ELSEIF(IGRD.EQ.4) THEN     !! using nature neighbors
                call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &               NOBC,lonOBC,latOBC,ZOUT)
              ENDIF
              DO I=1,NOBC
                tempOBC(I,K,N0)=ZOUT(I)
              ENDDO
            ENDIF

            NDUM=0 
            DO NS=1,NSELECT_PARENT
              IF(abs(salt(IPARENT(NS),JPARENT(NS),K,N)).LE.90.0) THEN
                NDUM=NDUM+1
                XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
                ZINP(ndum)=salt(IPARENT(NS),JPARENT(NS),K,N)
              ENDIF	
            ENDDO	
            IF(NDUM.EQ.0) THEN
              WRITE(*,*) 'There is no parent data point at k=',
     &              K,depth(k)
              write(*,*) 'Extrapolate upper level (k-1) to K' 
              DO I=1,NOBC
                saltOBC(I,K,N0)=saltOBC(I,K-1,N0)         
              ENDDO
            ELSE
              IF(NDUM.NE.NDATA) THEN
                WRITE(*,*) 'NDUM of SALT= ',NDUM,NDATA
                WRITE(*,*) 'NDATA is not equal to NDUM in Salt!'
                NDATA=NDUM	
                IF(IGRD.EQ.1 )                                  
     &            call INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &                 lonOBC,latOBC,ZOUT,weightnodes,weights,0)
              ENDIF   

              IF(IGRD.EQ.1) THEN         !! using remesh
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &               lonOBC,latOBC,ZOUT,weightnodes,weights,1)
              ELSEIF(IGRD.EQ.4) THEN     !! using nature neighbors
                call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &               NOBC,lonOBC,latOBC,ZOUT)
              ENDIF
              DO I=1,NOBC
                saltOBC(I,K,N0)=ZOUT(I)
              ENDDO
            ENDIF

            NDUM=0
            DO NS=1,NSELECT_PARENT
              IF(abs(U(IPARENT(NS),JPARENT(NS),K,N)).LE.30.0) THEN
                NDUM=NDUM+1
                XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
                ZINP(ndum)=U(IPARENT(NS),JPARENT(NS),K,N)
              ENDIF	
            ENDDO	
            IF(NDUM.EQ.0) THEN
              WRITE(*,*) 'There is no parent data point at k=',
     &          K,depth(k)
              write(*,*) 'Extrapolate upper level (k-1) to K' 
              DO I=1,NOBC
                uOBC(I,K,N0)=uOBC(I,K-1,N0)         
              ENDDO
            ELSE
              IF(NDUM.NE.NDATA) THEN
                WRITE(*,*) 'NDUM of U= ',NDUM,NDATA
                WRITE(*,*) 'NDATA is not equal to NDUM in U!'
                NDATA=NDUM	
                IF(IGRD.EQ.1)                                  
     &            call INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &                 lonOBC,latOBC,ZOUT,weightnodes,weights,0)
              ENDIF   
              IF(IGRD.EQ.1) THEN         !! using remesh
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &               lonOBC,latOBC,ZOUT,weightnodes,weights,1)
              ELSEIF(IGRD.EQ.4) THEN     !! using nature neighbors
                call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &               NOBC,lonOBC,latOBC,ZOUT)
              ENDIF
              DO I=1,NOBC
                uOBC(I,K,N0)=ZOUT(I)
              ENDDO
            ENDIF

            NDUM=0
            DO NS=1,NSELECT_PARENT
              IF(abs(V(IPARENT(NS),JPARENT(NS),K,N)).LE.30.0) THEN
                NDUM=NDUM+1
                XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
                ZINP(ndum)=V(IPARENT(NS),JPARENT(NS),K,N)
              ENDIF	
            ENDDO	
            IF(NDUM.EQ.0) THEN
              WRITE(*,*) 'There is no parent data point at k=',
     &          K,depth(k)
              write(*,*) 'Extrapolate upper level (k-1) to K' 
              DO I=1,NOBC
                vOBC(I,K,N0)=vOBC(I,K-1,N0)         
              ENDDO
            ELSE
              IF(NDUM.NE.NDATA) THEN
                WRITE(*,*) 'NDUM of V= ',NDUM,NDATA
                WRITE(*,*) 'NDATA is not equal to NDUM in V!'
                NDATA=NDUM	
                IF(IGRD.EQ.1)                                  
     &            call INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &                 lonOBC,latOBC,ZOUT,weightnodes,weights,0)
              ENDIF   
              IF(IGRD.EQ.1) THEN         !! using remesh
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,NOBC,
     &               lonOBC,latOBC,ZOUT,weightnodes,weights,1)
              ELSEIF(IGRD.EQ.4) THEN     !! using nature neighbors
                call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &                 NOBC,lonOBC,latOBC,ZOUT)
              ENDIF
              DO I=1,NOBC
                vOBC(I,K,N0)=ZOUT(I)
              ENDDO
            ENDIF
          ENDIF	    	     
        ENDDO
      ENDDO                 !! end loop of K

      DO I=1,NOBC
        DO N=1,NTMAX
          DO K=2,KMAX
            IF(ABS(tempOBC(I,K,N)).GE.90.0) THEN
              tempOBC(I,K,N)=tempOBC(I,K-1,N)
            ENDIF
            IF(ABS(saltOBC(I,K,N)).GE.90.0) THEN
              saltOBC(I,K,N)=saltOBC(I,K-1,N)
            ENDIF
            IF(ABS(uOBC(I,K,N)).GE.90.0) THEN
              uOBC(I,K,N)=uOBC(I,K-1,N)
            ENDIF
            IF(ABS(vOBC(I,K,N)).GE.90.0) THEN
              vOBC(I,K,N)=vOBC(I,K-1,N)
            ENDIF
          ENDDO
        ENDDO
      ENDDO	    

C------------------------------------------------------------------
C  vertically interpolating to ROMS sigma coordinate from z-coordinate of WOA05 or NCOM
C  K=1 for bottom and K=KBm for surface
c------------------------------------------------------------------
      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      allocate(oned1(KB))
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      allocate(oned2(KB))
      hmin=minval(hm)
      hmax=maxval(hm)
      hc=min(max(hmin,0.0),TCLINE)

      DO N=1,NTMAX
        DO I=1,NOBC
          KMAX1=0
          DO K=1,KMAX
            IF(ABS(TEMPOBC(I,K,N)).LE.90.0.AND.
     &         ABS(SALTOBC(I,K,N)).LE.90.0) THEN
              KMAX1=KMAX1+1     
              oned1(kMAX1)=TEMPOBC(I,K,N)
              oned2(kmax1)=SALTOBC(I,K,N)
            ENDIF
          ENDDO	

          ele=0
          CALL sigma2Z_ROMS_FIX_new(sigma,hOBC(i),ele,KBm,ZSIGMA,
     &         hc,theta_s,theta_b,TCline,nvtrans,nvstr)

          DO K=KBm,1,-1      ! from surface to bottom
            sdepth=ZSIGMA(K)
            IF(sdepth.LE.depth(1)) THEN
              UTMP=oned1(1)
              VTMP=oned2(1)
            ELSEIF(sdepth.GE.depth(KMAX1)) THEN
              UTMP=oned1(KMAX1)
              VTMP=oned2(KMAX1)
            ELSE
              DO K9=1,KMAX1-1
                IF((sdepth.GE.depth(K9)).and.
     &             (sdepth.LT.depth(K9+1))) then
                   exit
                endif
              ENDDO
400           X1=depth(K9)
              X2=depth(K9+1)
              Y1=oned1(K9)
              Y2=oned1(K9+1)
              call linear(X1,Y1,X2,Y2,sdepth,UTMP)
              Y1=oned2(K9)
              Y2=oned2(K9+1)
              call linear(X1,Y1,X2,Y2,sdepth,VTMP)
            ENDIF 
            TEMPOBC_M(I,K,N)=UTMP
            SALTOBC_M(I,K,N)=VTMP   
            IF(K.EQ.KBm) THEN
              IF(IEEE_IS_NAN(UTMP)) THEN
                write(*,*) 'Fatal Error: surface value is NaN!',I,N
                STOP
              END IF
            ELSE
              IF(IEEE_IS_NAN(UTMP))
     &          TEMPOBC_M(I,K,N)=TEMPOBC_M(I,K+1,N)
              IF(IEEE_IS_NAN(UTMP))
     &          SALTOBC_M(I,K,N)=SALTOBC_M(I,K+1,N)
            END IF
	  ENDDO
        ENDDO
      ENDDO

      DO N=1,NTMAX
        DO I=1,NOBC
          KMAX1=0
          DO K=1,KMAX
            IF(ABS(uOBC(I,K,N)).LE.90.0.AND.
     &         ABS(vOBC(I,K,N)).LE.90.0) THEN
              KMAX1=KMAX1+1     
              oned1(kMAX1)=uOBC(I,K,N)
              oned2(kmax1)=vOBC(I,K,N)
            ENDIF
          ENDDO	
          ele=0
          CALL sigma2Z_ROMS_FIX_new(sigma,hOBC(i),ele,KBm,ZSIGMA,
     &         hc,theta_s,theta_b,TCline,nvtrans,nvstr)
          DO K=KBm,1,-1  ! from surface to bottom
            sdepth=ZSIGMA(K)
            IF(sdepth.LE.depth(1)) THEN
              UTMP=oned1(1)
              VTMP=oned2(1)
            ELSEIF(sdepth.GE.depth(KMAX1)) THEN
              UTMP=oned1(KMAX1)
              VTMP=oned2(KMAX1)
            ELSE
              DO K9=1,KMAX1-1
                IF((sdepth.GE.depth(K9)).and.
     &             (sdepth.LT.depth(K9+1))) then
                  exit
                end if
              ENDDO
4001          X1=depth(K9)
              X2=depth(K9+1)
              Y1=oned1(K9)
              Y2=oned1(K9+1)
              call linear(X1,Y1,X2,Y2,sdepth,UTMP)
              Y1=oned2(K9)
              Y2=oned2(K9+1)
              call linear(X1,Y1,X2,Y2,sdepth,VTMP)
            ENDIF
            uOBC_M(I,K,N)=UTMP
            vOBC_M(I,K,N)=VTMP
            IF(K.EQ.KBm) THEN
              IF(IEEE_IS_NAN(UTMP)) THEN
                write(*,*) "Fatal Error: surface value is NaN!",I,N
                STOP
              END IF
            ELSE
              IF(IEEE_IS_NAN(UTMP)) uOBC_M(I,K,N)=uOBC_M(I,K+1,N)
              IF(IEEE_IS_NAN(UTMP)) vOBC_M(I,K,N)=vOBC_M(I,K+1,N)
            END IF
          enddo
        ENDDO
      ENDDO

c------------------------------------------------------------------
C End of vertical interpolation onto model vertical coordinate
C End of temperature and salinity OBC processing
C Begin time interpolation to DELT for ubar/vbar
c------------------------------------------------------------------
      IF(TRIM(DBASE_TS).EQ.'RTOFS'.OR.
     &   TRIM(DBASE_TS).EQ.'HYCOM') THEN
        NREC=NINT((day_end-day_start)*24/DELT)+1
        IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
        allocate(oned1(NREC))
        IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
        allocate(oned2(NREC))
        IF(ALLOCATED(oned3)) DEALLOCATE(oned3)
        allocate(oned3(NTMAX))
        IF(ALLOCATED(oned4)) DEALLOCATE(oned4)
        allocate(oned4(NTMAX))
        IF(ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
        allocate(TMP2D(NOBC,NREC))
 
        DO N=1,NREC
          ONED1(N)=day_start+(N-1)*DELT/24.0
        ENDDO
        DO I=1,NOBC
          DO N=1,NTMAX
            oned3(N)=ubarOBC(I,N)
          ENDDO

          if(allocated(tmpp1)) deallocate(tmpp1)
          if(allocated(tmpp2)) deallocate(tmpp2)
          allocate(tmpp1(1:NREC)); tmpp1=0.0
          allocate(tmpp2(1:NREC)); tmpp2=0.0
          if(allocated(tmpp3)) deallocate(tmpp3)
          if(allocated(tmpp4)) deallocate(tmpp4)
          allocate(tmpp3(1:NTMAX)); tmpp3=0.0
          allocate(tmpp4(1:NTMAX)); tmpp4=0.0
          DO N=1,NREC
            tmpp1(N)=ONED1(N)
          End do
          DO N=1,NTMAX
            tmpp3(N)=TIMEOBC(N)
            tmpp4(N)=ONED3(N)
          End do
          CALL lineararray(NREC,tmpp1,tmpp2,NTMAX,tmpp3,tmpp4)
          DO N=1,NREC
            TMP2D(I,N)=tmpp2(N)
          ENDDO
        ENDDO
        IF(ALLOCATED(ubarOBC)) DEALLOCATE(ubarOBC)
        allocate(ubarOBC(NOBC,NREC))
        DO N=1,NREC
          DO I=1,NOBC
            ubarOBC(I,N)=TMP2D(I,N)
          ENDDO
        ENDDO

        DO I=1,NOBC
          DO N=1,NTMAX
            oned3(N)=vbarOBC(I,N)
            tmpp4(N)=ONED3(N)
          ENDDO
          CALL lineararray(NREC,tmpp1,tmpp2,NTMAX,tmpp3,tmpp4)
          DO N=1,NREC
            TMP2D(I,N)=tmpp2(N)
          ENDDO
        ENDDO
        deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
        IF(ALLOCATED(vbarOBC)) DEALLOCATE(vbarOBC)
        allocate(vbarOBC(NOBC,NREC))
        DO N=1,NREC
          DO I=1,NOBC
            vbarOBC(I,N)=TMP2D(I,N)
          ENDDO
        ENDDO
      ENDIF

c------------------------------------------------------------------
C Begin time interpolation to DELT for T/S, U/V
c------------------------------------------------------------------
      IF(ALLOCATED(ts_time)) DEALLOCATE(ts_time)
      ALLOCATE(ts_time(NTMAX))
      DO N=1,NTMAX
        ts_time(N)=TIMEOBC(N)
      ENDDO
      WRITE(*,*) 'T & S OBC time interpolation'
      
      NREC=NINT((day_end-day_start)*24/DELT)+1
      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      allocate(oned1(NREC))
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      allocate(oned2(NREC))
      IF(ALLOCATED(oned3)) DEALLOCATE(oned3)
      allocate(oned3(NMAX))
      IF(ALLOCATED(oned4)) DEALLOCATE(oned4)
      allocate(oned4(NMAX))
      IF(ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
      allocate(TMP4D(4,NOBC,KBm,NREC))

      DO N=1,NREC
        ONED1(N)=day_start+(N-1)*DELT/24.0
      ENDDO

      DO I=1,NOBC
        DO K=1,KBm
          DO N=1,NTMAX
            oned3(N)=TEMPOBC_M(I,K,N)
            oned4(N)=SALTOBC_M(I,K,N)
          ENDDO	
          if(allocated(tmpp1)) deallocate(tmpp1)
          if(allocated(tmpp2)) deallocate(tmpp2)
          allocate(tmpp1(1:NREC)); tmpp1=0.0
          allocate(tmpp2(1:NREC)); tmpp2=0.0
          if(allocated(tmpp3)) deallocate(tmpp3)
          if(allocated(tmpp4)) deallocate(tmpp4)
          allocate(tmpp3(1:NTMAX)); tmpp3=0.0
          allocate(tmpp4(1:NTMAX)); tmpp4=0.0
          DO N=1,NREC
            tmpp1(N)=ONED1(N)
          End do
          DO N=1,NTMAX
            tmpp3(N)=TS_TIME(N)
            tmpp4(N)=ONED3(N)
          End do
          CALL lineararray(NREC,tmpp1,tmpp2,NTMAX,tmpp3,tmpp4)
          DO N=1,NREC
            TMP4D(1,I,K,N)=tmpp2(N)
          ENDDO
          DO N=1,NTMAX
            tmpp4(N)=ONED4(N)
          End do
          CALL lineararray(NREC,tmpp1,tmpp2,NTMAX,tmpp3,tmpp4)
          DO N=1,NREC
            TMP4D(2,I,K,N)=tmpp2(N)
          ENDDO
          DO N=1,NTMAX
            oned3(N)=uOBC_M(I,K,N)
            oned4(N)=vOBC_M(I,K,N)
          ENDDO
          DO N=1,NTMAX
            tmpp4(N)=ONED3(N)
          End do
          CALL lineararray(NREC,tmpp1,tmpp2,NTMAX,tmpp3,tmpp4)
          DO N=1,NREC
            TMP4D(3,I,K,N)=tmpp2(N)
          ENDDO
          DO N=1,NTMAX
            tmpp4(N)=ONED4(N)
          End do
          CALL lineararray(NREC,tmpp2,tmpp2,NTMAX,tmpp3,tmpp4)
          DO N=1,NREC
            TMP4D(4,I,K,N)=tmpp2(N)
          ENDDO
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
        ENDDO
      ENDDO

      IF(ALLOCATED(TEMPOBC_M)) DEALLOCATE(TEMPOBC_M)
      allocate(TEMPOBC_M(NOBC,KBm,NREC))
      IF(ALLOCATED(SALTOBC_M)) DEALLOCATE(SALTOBC_M)
      allocate(SALTOBC_M(NOBC,KBm,NREC))
      IF(ALLOCATED(TS_TIME)) DEALLOCATE(TS_TIME)
      allocate(TS_TIME(NREC))
      IF(ALLOCATED(uOBC_M)) DEALLOCATE(uOBC_M)
      allocate(uOBC_M(NOBC,KBm,NREC))
      IF(ALLOCATED(vOBC_M)) DEALLOCATE(vOBC_M)
      allocate(vOBC_M(NOBC,KBm,NREC))

      DO N=1,NREC
        ts_time(N)=ONED1(N)
        DO I=1,NOBC
          DO K=1,KBm
            TEMPOBC_M(I,K,N)=TMP4D(1,I,K,N) 
            SALTOBC_M(I,K,N)=TMP4D(2,I,K,N) 
            uOBC_M(I,K,N)=TMP4D(3,I,K,N)
            vOBC_M(I,K,N)=TMP4D(4,I,K,N)
          ENDDO
        ENDDO
      ENDDO
      NTMAX=NREC

c------------------------------------------------------------------
C Deallocate unused arrays
c------------------------------------------------------------------
      IF(ALLOCATED(lon)) DEALLOCATE(lon)
      IF(ALLOCATED(lat)) DEALLOCATE(lat)
      IF(ALLOCATED(lonsub)) DEALLOCATE(lonsub)
      IF(ALLOCATED(latsub)) DEALLOCATE(latsub)
      IF(ALLOCATED(masksub)) DEALLOCATE(masksub)
      IF(ALLOCATED(depth)) DEALLOCATE(depth)
      IF(ALLOCATED(temp)) DEALLOCATE(temp)
      IF(ALLOCATED(salt)) DEALLOCATE(salt)
      IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
      IF(ALLOCATED(tmp2d)) DEALLOCATE(tmp2d)
      IF(ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
      IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF(ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF(ALLOCATED(oned4)) DEALLOCATE(oned4)

c------------------------------------------------------------------
C  Processing subtidal Water Level open boundary from RTOFS
c------------------------------------------------------------------      
      IF(TRIM(DBASE_WL).EQ.'RTOFS') THEN
        IFILE_NCOM=0
        NCOM_FILE='RTOFS_FILE_WL'
        INQUIRE(FILE=trim(NCOM_FILE),EXIST=FEXIST)
        IF(FEXIST) THEN
          CLOSE(60)
          OPEN(60,file=trim(NCOM_FILE))
          DO I=1,999
            READ(60,'(a200)',END=3335) BUFFER
            FILE_NCOM(I)=trim(adjustL(BUFFER))
          ENDDO
3335      CLOSE(60)
          IFILE_NCOM=I-1
        ENDIF
        WRITE(*,*) 'Total number of RTOFS WL FILES= ',IFILE_NCOM
        IF(IFILE_NCOM.LT.1) THEN
          WRITE(*,*) 'No RTOFS WL file is available in this period'
          DBASE_TS='ETSS'
          WRITE(ICORMS,'(a)') 'RTOFS FILE IS NOT FOUND' 
          WRITE(ICORMS,'(a)') 'USE ETSS BACKUP' 
        ENDIF  

        DO I=1,IFILE_NCOM
	  WRITE(*,*) 'RTOFS I=',I,TRIM(FILE_NCOM(I))
        ENDDO

C-----------------------------------------------------------------------
C Read all RTOFS WL netCDF files available between start and end time       
C-----------------------------------------------------------------------
        FIN=TRIM(FILE_NCOM(1))
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='ssh'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IM=DIMS(1)
        JM=DIMS(2)
        NT=DIMS(3)
        WRITE(*,*) 'RTOFS IM=',IM,'JM= ',JM,'NT= ',NT
        IF(ALLOCATED(lon)) DEALLOCATE(lon)
        ALLOCATE(lon(IM,JM))
        IF(ALLOCATED(lat)) DEALLOCATE(lat)
        ALLOCATE(lat(IM,JM))
        IF(ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
        ALLOCATE(zeta_time(NMAX))

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        VNAME='Longitude'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        IF(NDIM.EQ.2) THEN
          DO I=1,IM
            DO J=1,JM
              lon(i,j)=TMP4D(i,j,1,1)
 	      if(lon(i,j).gt.180.0) lon(i,j)=lon(i,j)-360.0
            ENDDO
          ENDDO
        ELSEIF(NDIM.EQ.1) THEN
          DO I=1,IM
            DO J=1,JM
              lon(i,j)=TMP4D(i,1,1,1)
              if(lon(i,j).gt.180.0) lon(i,j)=lon(i,j)-360.0
            ENDDO
          ENDDO
        ENDIF  

        VNAME='Latitude'
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        IF(NDIM.EQ.2) THEN
          DO I=1,IM
            DO J=1,JM
              lat(i,j)=TMP4D(i,j,1,1)
            ENDDO
          ENDDO
        ELSEIF(NDIM.EQ.1) THEN
          DO I=1,IM
            DO J=1,JM
              lat(i,j)=TMP4D(J,1,1,1)
            ENDDO
          ENDDO
        ENDIF

C-----------------------------------------------------------------------
C  Select subdomain I and J index        
C-----------------------------------------------------------------------
        IMIN=99999
        IMAX=-9999
        JMIN=99999
        JMAX=-9999
        DO I=1,IM
          DO J=1,JM
            IF(lon(i,j).GE.minlon.and.lon(i,j).LE.maxlon.and.
     &         lat(i,j).GE.minlat.and.lat(i,j).LE.maxlat) then
              IF(I.LT.IMIN) IMIN=I 	  
              IF(I.GT.IMAX) IMAX=I 	  
              IF(J.LT.JMIN) JMIN=J 	  
              IF(J.GT.JMAX) JMAX=J
	    ENDIF
          ENDDO
        ENDDO	  	  
        ISUB=IMAX-IMIN+1	 
        JSUB=JMAX-JMIN+1	 
        write(*,*) 'Subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX

C-----------------------------------------------------------------------
C Allocate sizes of arrays for RTOFS products 
C-----------------------------------------------------------------------
        NNSUB=ISUB*JSUB
        IF(ALLOCATED(lonsub)) DEALLOCATE(lonsub)
        allocate(lonsub(ISUB,JSUB))
        IF(ALLOCATED(latsub)) DEALLOCATE(latsub)
        allocate(latsub(ISUB,JSUB))
        IF(ALLOCATED(WL)) DEALLOCATE(WL)
        allocate(WL(ISUB,JSUB,NT*IFILE_NCOM+10))
        IF(ALLOCATED(ubar_rtofs)) DEALLOCATE(ubar_rtofs)
        allocate(ubar_rtofs(ISUB,JSUB,NT*IFILE_NCOM+10))
        IF(ALLOCATED(vbar_rtofs)) DEALLOCATE(vbar_rtofs)
        allocate(vbar_rtofs(ISUB,JSUB,NT*IFILE_NCOM+10))

        DO N=1,NT*IFILE_NCOM+10
          DO J=1,JSUB
            DO I=1,ISUB
              WL(I,J,N)=0.0
              ubar_rtofs(I,J,N)=0.0
              vbar_rtofs(I,J,N)=0.0
            ENDDO
          ENDDO
        ENDDO   
        DO J=1,JSUB
          DO I=1,ISUB
            lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
            latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
	    write(77,'(2F12.4,2I6)') lonsub(i,j),latsub(i,j),I,J
          ENDDO
        ENDDO 
        close(77)  

        ICOUNT=0
        NREC=0
        TIMELAST=-99999.9	 
        DO IFILE=1,IFILE_NCOM
          write(*,*) 'Reading RTOFS NetCDF file: ',IFILE
          FIN=TRIM(FILE_NCOM(IFILE))
          VNAME='MT'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
          ALLOCATE(tmp3d(DIMS(1),DIMS(2),DIMS(3)))
          IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
          ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),3))
          NT=DIMS(1)
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,6)
          ANAME=trim(adjustL(ANAME))
          LEN=LEN_TRIM(ANAME)
          LL=INDEX(ANAME,'minute')         
          IF(LL.GT.0) scale_time=1.0/1440.0
          LL=INDEX(ANAME,'hour')         
          IF(LL.GT.0) scale_time=1.0/24.0
          LL=INDEX(ANAME,'day')         
          IF(LL.GT.0) scale_time=1.0
          LL=INDEX(ANAME,'since')         
          IF(LL.GT.0) then
            read(ANAME(LL+6:LEN),80) IYR,IMM,IDD,IHH
            write(*,*) 'basetime=',IYR,IMM,IDD,IHH
          else
            write(*,*) 'There is error while reading base date'
            stop
          endif    

          yearb=IYR
          monthb=IMM
          dayb=IDD
          hourb=IHH
          jday=JULIAN(yearb,monthb,dayb,hourb)
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,7)
          TIME_RTOFS=TMP4D(1,1,1,1)*scale_time+jday-jbase_date
          IF(TIMELAST.LT.TIME_RTOFS) THEN
            ICOUNT=ICOUNT+1
            ZETA_TIME(ICOUNT)=TIME_RTOFS
            TIMELAST=TIME_RTOFS 
          ENDIF

          VNAME='ssh'
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
          ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,1)
          DO N=1,NT
            DO J=1,JSUB
              DO I=1,ISUB
                WL(i,j,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,N,1)
                if(abs(WL(I,J,ICOUNT)).GE.99.0) THEN
                  WL(I,J,ICOUNT)=-99999.9
                ELSE
                  IF(OFS.EQ.'gomofs'.or.OFS.eq.'GoMOFS'.or.
     &               OFS.eq.'GOMOFS') THEN
                    WL(I,J,ICOUNT)=WL(I,J,ICOUNT)+0.62 !Suggested by developer
                  ELSE
                    WL(I,J,ICOUNT)=WL(I,J,ICOUNT)+0.25 !nontidal WL is about 25 cm lower
                  ENDIF	
                ENDIF  
              ENDDO
            ENDDO   
          ENDDO

          VNAME='u_barotropic_velocity'
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
          ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,1)
          DO N=1,NT
            DO J=1,JSUB
              DO I=1,ISUB
                ubar_rtofs(i,j,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,N,1)
                if(abs(ubar_rtofs(I,J,ICOUNT)).GE.99.0) THEN
                  ubar_rtofs(I,J,ICOUNT)=-99999.9
                ENDIF  
              ENDDO
            ENDDO   
          ENDDO 

          VNAME='v_barotropic_velocity'
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
          ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,1)
          DO N=1,NT
            DO J=1,JSUB
              DO I=1,ISUB
                vbar_rtofs(i,j,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,N,1)
                if(abs(vbar_rtofs(I,J,ICOUNT)).GE.99.0) THEN
                  vbar_rtofs(I,J,ICOUNT)=-99999.9
                ENDIF  
              ENDDO
            ENDDO   
          ENDDO 
 
          NREC=ICOUNT
          STATUS=NF_CLOSE(NCID)
        ENDDO      
        NT=NREC 
        write(*,*) 'Number of RTOFS WL data NREC= ',NREC
        IF(NREC.LE.0) THEN
          write(*,*) 'RTOFS WL data is too shorter than day_end'
          write(*,*) 'The time period is not covered by RTOFS'	
          WRITE(ICORMS,'(a)') TRIM(DBASE_WL)//' is too short'
          WRITE(ICORMS,'(a)') 'USE ETSS BACKUP' 
          DBASE_WL='ETSS'
          GOTO 1500
        ENDIF  

C-----------------------------------------------------------------------
C  End of reading date from RTOFS WL products 
C-----------------------------------------------------------------------
        NSTR=1
        DO N=1,NT
          write(*,*) 'Time= ',N,zeta_time(N)
          IF(zeta_TIME(N).gt.day_start) then
            NSTR=N-1
            EXIT
          ENDIF
        ENDDO	
1330    continue
        IF(NSTR.LE.0.OR.NSTR.GT.NT) THEN  !AJ 08/13/2010 extrapolation
          DO NN=NT,1,-1
            ZETA_TIME(NN+1)=ZETA_TIME(NN)
            DO I=1,ISUB
              DO J=1,JSUB
                WL(I,J,NN+1)=WL(I,J,NN) 
                ubar_rtofs(I,J,NN+1)=ubar_rtofs(I,J,NN)
                vbar_rtofs(I,J,NN+1)=vbar_rtofs(I,J,NN)
              ENDDO	
            ENDDO
          ENDDO
          ZETA_TIME(1)=day_start
          NT=NT+1
          NSTR=1
        ENDIF

        IF(ZETA_TIME(NT).LT.day_end) THEN
          diff=day_end-ZETA_TIME(NT)
          IF(diff.LT.5.0) THEN 
C AJ ZHANG 03/18/2010, allow persistence for the maximum 5 days. 
            ZETA_TIME(NT+1)=day_end
            DO I=1,ISUB
              DO J=1,JSUB
                WL(I,J,NT+1)=WL(I,J,NT) 
                ubar_rtofs(I,J,NT+1)=ubar_rtofs(I,J,NT)
                vbar_rtofs(I,J,NT+1)=vbar_rtofs(I,J,NT)
              ENDDO	
            ENDDO
            NT=NT+1
          ELSE  
            write(*,*) 'RTOFS WL data is too shorter than day_end'
            write(*,*) 'The time period is not covered by RTOFS'	  
            WRITE(ICORMS,'(a)') TRIM(DBASE_WL)//' is too short'
            WRITE(ICORMS,'(a)') 'USE ETSS BACKUP' 
            DBASE_WL='ETSS'
          ENDIF  
        ENDIF    
        DO N=1,NT
          if(ZETA_TIME(N).GE.day_end) then
            NEND=n
            EXIT
          ENDIF
        ENDDO	
1440    continue
        IF(N.GT.NT) THEN
          write(*,*) 'Time period is not covered by dataset'	  
          WRITE(ICORMS,'(a)') 'Time period is not covered'
          WRITE(ICORMS,'(a)') 'Generation of WL OBC failed'
          WRITE(ICORMS,'(a)') 'Stop in nos_ofs_create_OBC.f'
          stop   !'stop in nos_ofs_create_OBC.f'
        ENDIF  	  
        write(*,*) 'Start and end time of RTOFS WL= ',
     &    ZETA_time(NSTR),ZETA_time(NEND),NSTR,NEND,day_start,day_end 		
        NTMAX_WL=NEND-NSTR+1
        IF(ALLOCATED(TIMEOBC)) DEALLOCATE(TIMEOBC)
        ALLOCATE(TIMEOBC(NTMAX_WL))
        DO N=1,NTMAX_WL
          TIMEOBC(N)=ZETA_TIME(NSTR+N-1)
        ENDDO
        IF(ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
        ALLOCATE(zeta_time(NTMAX_WL))
        DO N=1,NTMAX_WL
          zeta_time(N)=TIMEOBC(N)
        ENDDO

c------------------------------------------------------------------
C  spatial interpolation for Water Level open boundary
c------------------------------------------------------------------
        IF(ALLOCATED(WLOBC)) DEALLOCATE(WLOBC)
        ALLOCATE(WLOBC(NOBC,NTMAX_WL))
        IF(ALLOCATED(ubarOBC)) DEALLOCATE(ubarOBC)
        ALLOCATE(ubarOBC(NOBC,NTMAX_WL))   
        IF(ALLOCATED(vbarOBC)) DEALLOCATE(vbarOBC)
        ALLOCATE(vbarOBC(NOBC,NTMAX_WL))       

        IF(IGRD.EQ.2.OR.IGRD.EQ.3) THEN
          IF(ALLOCATED(Iout)) DEALLOCATE(Iout)
          allocate(Iout(NOBC,1))
          IF(ALLOCATED(Jout)) DEALLOCATE(Jout)
          allocate(Jout(NOBC,1))
          IF(ALLOCATED(outm)) DEALLOCATE(outm)
          allocate(outm(NOBC,1))
          IF(ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
          allocate(TMP2D(ISUB,JSUB))
          write(*,*) 'Search coarser grid indeces for WL!'

          if(IGRD.eq.2) iflag=1
          if(IGRD.eq.3) iflag=0
          CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &         NOBC,1,lonOBC,latOBC,outm,Iout,Jout,0)
        ENDIF

        IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
          IF(ALLOCATED(XINP)) DEALLOCATE(XINP)
          allocate(XINP(NNSUB))
          IF(ALLOCATED(YINP)) DEALLOCATE(YINP)
          allocate(YINP(NNSUB))
          IF(ALLOCATED(ZINP)) DEALLOCATE(ZINP)
          allocate(ZINP(NNSUB))
          IF(ALLOCATED(XOUT)) DEALLOCATE(XOUT)
          allocate(XOUT(NOBC))
          IF(ALLOCATED(YOUT)) DEALLOCATE(YOUT)
          allocate(YOUT(NOBC))
          IF(ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
          allocate(ZOUT(NOBC))
          IF(ALLOCATED(weightnodes)) DEALLOCATE(weightnodes)
          allocate(weightnodes(NOBC,3))
          IF(ALLOCATED(weights)) DEALLOCATE(weights)
          allocate(weights(NOBC,3))

          call search_output(ISUB,JSUB,lonsub,latsub,NOBC,lonOBC,
     &         latOBC,nselect_parent,Iparent,Jparent)
        ENDIF

        DO N=NSTR,NEND
          N0=N-NSTR+1
          IF(IGRD.EQ.2.or.IGRD.EQ.3) THEN
!! Spatial interpolation using bicubic or bilinear
            DO I=1,ISUB
              DO J=1,JSUB
                tmp2d(I,J)=WL(I,J,N)
              ENDDO
            ENDDO
            if(IGRD.eq.2) iflag=1
            if(IGRD.eq.3) iflag=0
            CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &           NOBC,1,lonOBC,latOBC,outm,Iout,Jout,1)
            DO I=1,NOBC
              WLOBC(I,N0)=outm(I,1)
              IF(I.GT.1) THEN
                IF(ABS(WLOBC(I,N0)).GT.999.0) THEN
                  TMPTXT='No valid value after regrid interpolation'
C                  WRITE(*,*) TRIM(TMPTXT)
                  TMPTXT='Use valid value from nearby water point'
C                  WRITE(*,*) TRIM(TMPTXT)
                  IF(ABS(WLOBC(I-1,N0)).LT.999.0) THEN
                    WLOBC(I,N0)=WLOBC(I-1,N0)
                  ENDIF
                ENDIF   
              ENDIF
            ENDDO

            DO I=1,ISUB
              DO J=1,JSUB
                tmp2d(I,J)=ubar_rtofs(I,J,N)
              ENDDO
            ENDDO
            CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &           NOBC,1,lonOBC,latOBC,outm,Iout,Jout,1)
            DO I=1,NOBC
              ubarOBC(I,N0)=outm(I,1)
              IF(I.GT.1) THEN
                IF(ABS(ubarOBC(I,N0)).GT.999.0) THEN
                  TMPTXT='No valid value after regrid interpolation'
C                  WRITE(*,*) TRIM(TMPTXT)
                  TMPTXT='Use valid value from nearby water point'
C                  WRITE(*,*) TRIM(TMPTXT)
                  IF(ABS(ubarOBC(I-1,N0)).LT.999.0) THEN
                    ubarOBC(I,N0)=ubarOBC(I-1,N0)
                  ENDIF
                ENDIF   
              ENDIF
            ENDDO

            DO I=1,ISUB
              DO J=1,JSUB
                tmp2d(I,J)=vbar_rtofs(I,J,N)
              ENDDO
            ENDDO
            CALL INTERP_REGRID(iflag,ISUB,JSUB,lonsub,latsub,TMP2D,
     &           NOBC,1,lonOBC,latOBC,outm,Iout,Jout,1)
            DO I=1,NOBC
              vbarOBC(I,N0)=outm(I,1)
              IF(I.GT.1) THEN
                IF(ABS(vbarOBC(I,N0)).GT.999.0) THEN
                  TMPTXT='No valid value after regrid interpolation'
C                  WRITE(*,*) TRIM(TMPTXT)
                  TMPTXT='Use valid value from nearby water point'
C                  WRITE(*,*) TRIM(TMPTXT)
                  IF(ABS(vbarOBC(I-1,N0)).LT.999.0) THEN
                    vbarOBC(I,N0)=vbarOBC(I-1,N0)
                  ENDIF
                ENDIF   
              ENDIF
            ENDDO

          ELSEIF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN   
cc Using remesh routine or nature neighbors spatial interpolation 
            NDUM=0 
            DO NS=1,NSELECT_PARENT
              IF(abs(WL(IPARENT(NS),JPARENT(NS),N)).LE.90.0) THEN
                NDUM=NDUM+1
                XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
                ZINP(ndum)=WL(IPARENT(NS),JPARENT(NS),N)
              ENDIF	
            ENDDO	

	    NDATA=NDUM
            IF(IGRD.EQ.1) THEN           !! using remesh
              call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &             NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,0)
              call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &             NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,1)
            ELSEIF(IGRD.EQ.4) THEN       !! using nature neighbors
              call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &             NOBC,lonOBC,latOBC,ZOUT)
            ENDIF
            DO I=1,NOBC
              WLOBC(I,N0)=ZOUT(I)
            ENDDO
   
            IF(TRIM(DBASE_WL).EQ.'RTOFS') THEN
              NDUM=0       !! nature neighbors spatial interpolation 
              DO NS=1,NSELECT_PARENT
                IF(abs(ubar_rtofs(IPARENT(NS),JPARENT(NS),N)).LE.
     &             90.0) THEN
                  NDUM=NDUM+1
                  XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                  YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
                  ZINP(ndum)=ubar_rtofs(IPARENT(NS),JPARENT(NS),N)
                ENDIF
              ENDDO

              NDATA=NDUM
              IF(IGRD.EQ.1) THEN         !! using remesh
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &               NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,0)
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &               NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,1)
              ELSEIF(IGRD.EQ.4) THEN     !! using nature neighbors
                call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &               NOBC,lonOBC,latOBC,ZOUT)
              ENDIF
              DO I=1,NOBC
                ubarOBC(I,N0)=ZOUT(I)
              ENDDO

              NDUM=0         !! nature neighbors spatial interpolation 
              DO NS=1,NSELECT_PARENT
                IF(abs(vbar_rtofs(IPARENT(NS),JPARENT(NS),N)).LE.
     &             90.0)THEN
                  NDUM=NDUM+1
                  XINP(ndum)=lonsub(IPARENT(NS),JPARENT(NS))
                  YINP(ndum)=latsub(IPARENT(NS),JPARENT(NS))
                  ZINP(ndum)=vbar_rtofs(IPARENT(NS),JPARENT(NS),N)
                ENDIF	
              ENDDO	
              NDATA=NDUM
              IF(IGRD.EQ.1) THEN         !! using remesh
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &               NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,0)
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &               NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,1)
              ELSEIF(IGRD.EQ.4) THEN     !! using nature neighbors
                call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &               NOBC,lonOBC,latOBC,ZOUT)
              ENDIF
              DO I=1,NOBC
                vbarOBC(I,N0)=ZOUT(I)
	      ENDDO
            ENDIF
          ENDIF
        ENDDO  ! end of N LOOP
      ENDIF    ! end of IF (DBASE_WL .EQ. 'RTOFS')

c------------------------------------------------------------------
C  Generate WL OBC from ETSS gridded products if DBASE_WL = ETSS
C  Read in ETSS from a ASCII file generated using wgrib2 utility
c------------------------------------------------------------------
1500  CONTINUE
      IF(TRIM(DBASE_WL).EQ.'ETSS') THEN
        WRITE(*,*) 'GENERATE SUBTIDAL WL OBC FROM ETSS'      
        CLOSE(1)
        OPEN(1,FILE=TRIM(ETSSFILE),STATUS='OLD' )
        NT=0
        NETSS=0
        irec=0

        DO WHILE (TRIM(DBASE_WL).EQ.'ETSS')
2000      read(1,'(a200)',end=2002) BUFFER
          LEN1=LEN_TRIM(trim(BUFFER)) 
          IF(LEN1.GT.0) THEN
            LL=INDEX(BUFFER,'ETSRG',BACK=.TRUE.)         
            IF(LL.GT.0) THEN
              NT=NT+1
              if(irec.GT.NETSS) then
                NETSS=irec
                NMAX0=NT
              endif
              IF(NT.GT.1) ITMP1D(NT-1)=irec
              irec=0
            ELSE
              irec=irec+1  
            ENDIF
          ENDIF
        END DO
2002    CONTINUE
        if(irec.GT.NETSS) then
          NETSS=irec
          NMAX0=NT
        endif
        IF(NT.GT.1) ITMP1D(NT)=irec
        REWIND(1)
        WRITE(*,*) 'ETSS SUB DIMS NETSS=',NETSS,'NT=',NT
        IF(ALLOCATED(lon_ETSS)) DEALLOCATE(lon_ETSS)
        allocate(lon_ETSS(NETSS,NT))
        IF(ALLOCATED(lat_ETSS)) DEALLOCATE(lat_ETSS)
        allocate(lat_ETSS(NETSS,NT))
        IF(ALLOCATED(WL_ETSS)) DEALLOCATE(WL_ETSS)
        allocate(WL_ETSS(NETSS,NT))
        IF(ALLOCATED(TIMEOBC)) DEALLOCATE(TIMEOBC)
        ALLOCATE(TIMEOBC(NT) )
        IF(ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)

        DO N=1,NT
          DO I=1,NETSS
            WL_ETSS(I,N)=9999.0
          ENDDO
        ENDDO
        NREC=0
        TIMEOBC(1)=-99999.0
        DO N=1,NT
          read(1,'(a200)') BUFFER
          BUFFER=trim(adjustL(BUFFER))
          LEN1=LEN_TRIM(trim(BUFFER))
          LL=INDEX(BUFFER,'=',BACK=.TRUE.)
          if(BUFFER(LEN1-2:LEN1).EQ.'anl') then
            READ(BUFFER(LL+1:LEN1),'(I4,3I2)') IYR,IMM,IDD,IHH
C            write(*,*) 'ETSS anl time= ',IYR,IMM,IDD,IHH
          elseif(BUFFER(LEN1-3:LEN1).EQ.'fcst') then 
            READ(BUFFER(LL+1:LEN1),'(I4,3I2,1x,I2)') IYR,IMM,
     &        IDD,ICYC,IHH0
            IHH=ICYC+IHH0
C            write(*,*) 'ETSS fct time= ',IYR,IMM,IDD,IHH,ICYC,IHH0
          endif

          yearb=IYR
          monthb=IMM
          dayb=IDD
          hourb=IHH   !! do not need minutes to construct ETSS file name
          jday=JULIAN(yearb,monthb,dayb,hourb)
          day0=jday-jbase_date
          DO K=1,NREC
            IF(TIMEOBC(K).GE.day0) THEN
              NREC=K-1
              EXIT
            ENDIF  
          ENDDO  	 
500       CONTINUE
          NREC=NREC+1
          TIMEOBC(NREC)=day0

          DO I=1,ITMP1D(N)  !!   NETSS
            read(1,'(a200)') BUFFER
            BUFFER=trim(adjustL(BUFFER))
            READ(BUFFER,*) alo,alt,swl      
            WL_ETSS(I,NREC)=swl
            if(alo.gt.180.0) alo=alo-360.0
            lon_ETSS(I,NREC)=alo
            lat_ETSS(I,NREC)=alt
          ENDDO
        ENDDO
        CLOSE(1)
        NT=NREC    
        
c------------------------------------------------------------------
C  End of Reading in ETSS, and find a time period to cover from start time to end time 
c------------------------------------------------------------------
        IF(TIMEOBC(1).gt.day_start) TIMEOBC(1)=day_start
        IF(TIMEOBC(NT).lt.day_end) TIMEOBC(NT)=day_end
        NSTR=1
        DO N=1,NT
          IF(TIMEOBC(N).gt.day_start) then
            NSTR=N-1
            exit 
          ENDIF
        ENDDO	
630     continue
        IF(NSTR.LE.0.OR.NSTR.GT.NT) THEN
          write(*,*) 'Time of first data is later than start time'
          write(*,*) 'Time period is not covered by dataset'
          TMPTXT=TRIM(DBASE_WL)//' does not cover forecast'
          WRITE(ICORMS,'(a)') TRIM(TMPTXT)
          TMPTXT='CRITICAL FAULURE IN GENERATING WL OBC'
          WRITE(ICORMS,'(a)') TRIM(TMPTXT)
          stop   !! 'stop in nos_ofs_create_OBC.f '
        ENDIF  	  

        DO N=1,NT
          if(TIMEOBC(N).ge.day_end) then
            NEND=n
            EXIT
          ENDIF
        ENDDO	
640     continue	  
        IF(N.GT.NT) THEN
          write(*,*) 'Time period is not covered by dataset'	  
          TMPTXT=TRIM(DBASE_WL)//' does not cover forecast'
          WRITE(ICORMS,'(a)') TRIM(TMPTXT)
          TMPTXT='CRITICAL FAULURE IN GENERATING WL OBC'
          WRITE(ICORMS,'(a)') TRIM(TMPTXT)
          stop   !! 'stop in nos_ofs_create_OBC.f '
        ENDIF  	  
        write(*,*) 'Start and end time of = ',TIMEOBC(NSTR),
     &    TIMEOBC(NEND),NSTR,NEND,day_start,day_end 		
        NTMAX_WL=NEND-NSTR+1

C -------------------------------------------------------------------
C  End of reading ETSS ASCII file and begin to interpolate onto 
C  open boundary model grid points
C ----------------------------------------------------------------------------
        IF(ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
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
          IF(IGRD.EQ.2.or.IGRD.EQ.3) THEN   
!! Spatial interpolation using bicubic or bilinear
            WRITE(*,*) 'This option does not work for ETSS!!!!'
            WRITE(*,*) 'Please redefine IGRD=1 or 4 !!'
            IGRD=1
          ENDIF
        ENDIF

        IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN
          IF(ALLOCATED(XINP)) DEALLOCATE(XINP)
          allocate(XINP(NETSS))
          IF(ALLOCATED(YINP)) DEALLOCATE(YINP)
          allocate(YINP(NETSS))
          IF(ALLOCATED(ZINP)) DEALLOCATE(ZINP)
          allocate(ZINP(NETSS))
          IF(ALLOCATED(XOUT)) DEALLOCATE(XOUT)
          allocate(XOUT(NOBC))
          IF(ALLOCATED(YOUT)) DEALLOCATE(YOUT)
          allocate(YOUT(NOBC))
          IF(ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
          allocate(ZOUT(NOBC))
          IF(ALLOCATED(weightnodes)) DEALLOCATE(weightnodes)
          allocate(weightnodes(NOBC,3))
          IF(ALLOCATED(weights)) DEALLOCATE(weights)
          allocate(weights(NOBC,3))
        ELSEIF(IGRD.EQ.2.or.IGRD.EQ.3) THEN   
!! Spatial interpolation using bicubic or bilinear
          WRITE(*,*) 'This option does not work for ETSS!!!!'
          WRITE(*,*) 'Please redefine IGRD=1 or 4 !!'
          STOP
        ENDIF 
        NDATA=NETSS
        DO N=NSTR,NEND
          NDATA=ITMP1D(N)
          N0=N-NSTR+1
          IF(IGRD.EQ.1.OR.IGRD.EQ.4) THEN        
!! Using remesh or nature neighbors spatial interpolation
            NDUM=0
            DO I=1,NDATA
              IF(WL_ETSS(I,N).LT.999.0) THEN
                NDUM=NDUM+1
                XINP(ndum)=lon_ETSS(I,N)
                YINP(ndum)=lat_ETSS(I,N)
                ZINP(ndum)=WL_ETSS(I,N)
              ENDIF	
            ENDDO	

C            WRITE(*,*) 'NDATA=',NDATA,'NDUM=',NDUM,'N=',N 
            NDATA=NDUM
            IF(IGRD.EQ.1) THEN           !! using remesh
              call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &             NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,0)
              call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &             NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,1)
            ELSEIF(IGRD.EQ.4) THEN       !! using nature neighbors
              call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &             NOBC,lonOBC,latOBC,ZOUT)
            ENDIF
            DO I=1,NOBC
              WLOBC(I,N0)=ZOUT(I)
            ENDDO
          ENDIF	
        ENDDO
      ENDIF

C------------------------------------
C  end of WL horizontal interpolation onto model grid along open boundary coordinate
C begin time interpolation to DELT	
c------------------------------------------------------------------
      NREC=NINT((day_end-day_start)*24/DELT)+1
      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      allocate(oned1(NREC))
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      allocate(oned2(NREC))
      IF(ALLOCATED(oned3)) DEALLOCATE(oned3)
      allocate(oned3(NTMAX_WL))
      IF(ALLOCATED(oned4)) DEALLOCATE(oned4)
      allocate(oned4(NTMAX_WL))
      IF(ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
      allocate(TMP2D(NOBC,NREC))

      DO N=1,NREC
        ONED1(N)=day_start+(N-1)*DELT/24.0
      ENDDO
      DO I=1,NOBC
        DO N=1,NTMAX_WL
          oned3(N)=WLOBC(I,N)
        ENDDO	
        if(allocated(tmpp1)) deallocate(tmpp1)
        if(allocated(tmpp2)) deallocate(tmpp2)
        allocate(tmpp1(1:NREC)); tmpp1=0.0
        allocate(tmpp2(1:NREC)); tmpp2=0.0
        if(allocated(tmpp3)) deallocate(tmpp3)
        if(allocated(tmpp4)) deallocate(tmpp4)
        allocate(tmpp3(1:NTMAX_WL)); tmpp3=0.0
        allocate(tmpp4(1:NTMAX_WL)); tmpp4=0.0
        DO N=1,NREC
          tmpp1(N)=ONED1(N)
        End do
        DO N=1,NTMAX_WL
          tmpp3(N)=ZETA_TIME(N)
          tmpp4(N)=oned3(N)
        End do
        CALL lineararray(NREC,tmpp1,tmpp2,NTMAX_WL,tmpp3,tmpp4)
        DO N=1,NREC
          TMP2D(I,N)=tmpp2(N)
        ENDDO
      ENDDO	

      IF(ALLOCATED(WLOBC)) DEALLOCATE(WLOBC)
      allocate(WLOBC(NOBC,NREC))
      DO N=1,NREC
        DO I=1,NOBC
          WLOBC(I,N)=TMP2D(I,N)
        ENDDO
      ENDDO
      IF(TRIM(DBASE_WL).EQ.'RTOFS') THEN
        DO I=1,NOBC
          DO N=1,NTMAX_WL
            oned3(N)=ubarOBC(I,N)
            tmpp4(N)=oned3(N)
          ENDDO
          CALL lineararray(NREC,tmpp1,tmpp2,NTMAX_WL,tmpp3,tmpp4)
          DO N=1,NREC
            TMP2D(I,N)=tmpp2(N)
          ENDDO
        ENDDO
        IF(ALLOCATED(ubarOBC)) DEALLOCATE(ubarOBC)
        allocate(ubarOBC(NOBC,NREC))
        DO N=1,NREC
          DO I=1,NOBC
            ubarOBC(I,N)=TMP2D(I,N)
          ENDDO
        ENDDO
        DO I=1,NOBC
          DO N=1,NTMAX_WL
            oned3(N)=vbarOBC(I,N)
            tmpp4(N)=oned3(N)
          ENDDO
          CALL lineararray(NREC,tmpp1,tmpp2,NTMAX_WL,tmpp3,tmpp4)
          DO N=1,NREC
            TMP2D(I,N)=tmpp2(N)
          ENDDO
        ENDDO
        IF(ALLOCATED(vbarOBC)) DEALLOCATE(vbarOBC)
        allocate(vbarOBC(NOBC,NREC))
        DO N=1,NREC
          DO I=1,NOBC
            vbarOBC(I,N)=TMP2D(I,N)
          ENDDO
        ENDDO
        deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
      ENDIF
      IF(ALLOCATED(ZETA_TIME)) DEALLOCATE(ZETA_TIME)
      allocate(ZETA_TIME(NREC))
      DO N=1,NREC
        zeta_time(N)=ONED1(N)
      ENDDO
      NTMAX_WL=NREC

C -------------------------------------------------------------------
C   Print OBC for evaluation
      CLOSE(33)
      OPEN(33,file='Latlon_OBC.dat')
      DO N=1,NOBC
        write(33,'(3I8,20F12.4)') N,IOBC(N),JOBC(N),
     &    lonOBC(N),latOBC(N)
      ENDDO
      CLOSE(33)
      
      OPEN(33,file='WL_OBC.dat')
      DO N=1,NTMAX_WL
        write(33,35) ZETA_TIME(N),(WLOBC(I,N),I=1,10),WLOBC(NOBC,N)
      ENDDO
      CLOSE(33)

      OPEN(33,file='TEMP_OBC.dat')
      DO N=1,NTMAX
        DO I=1,NOBC
          write(33,35) TS_TIME(N),(TEMPOBC_M(I,K,N),K=1,KBm)  ! K=KBm for surface
        ENDDO
      ENDDO
      CLOSE(33)

      OPEN(33,file='SALT_OBC.dat')
      DO N=1,NTMAX
        DO I=1,NOBC
          write(33,35) TS_TIME(N),(SALTOBC_M(I,K,N),K=1,KBm)
        ENDDO
      ENDDO
      CLOSE(33)

      OPEN(33,file='UV_OBC.dat')
      DO N=1,NTMAX
        DO I=1,NOBC
          write(33,35) TS_TIME(N),(uOBC_M(I,K,N),K=1,KBm)
          write(33,35) TS_TIME(N),(vOBC_M(I,K,N),K=1,KBm)
        ENDDO
      ENDDO
      CLOSE(33)
35    FORMAT(50F10.4)	
      
C -------------------------------------------------------------------
C   End of processing for WL OBC from ETSS forecasts
C   Begin to process real time observations and tidal prediction
C ----------------------------------------------------------------------------
      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      allocate(oned1(NMAX))
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      allocate(oned2(NMAX))
      KINDAT=2
      CONV=1.0
      XMAJOR=0.0
      DELT_PRD=0.1
      NREC=NINT((jdaye-jdays+2)*24/DELT_PRD)+1  !one more day for prediction
      DO N=1,NREC
        TIME_PRD(N)=(jdays-jbase_date-2)+(N-1)*DELT_PRD/24.0
      ENDDO	
      jday=jdays-2
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
        IF(MOD(IYR,4).EQ.0) DAYS_PER_MONTH(2)=29   !!    Leap Year
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
          RTIME(i,N)=-99999.9
          RTIME_T(i,N)=-99999.9
          RTIME_S(i,N)=-99999.9
          WL_OBS(i,N)=-99999.9
          WL_PRD(I,N)=-99999.9
          SWL_OBS(i,N)=-99999.9
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
          IF(BUFFER(1:L1).EQ.BUFFER1(1:L1)) THEN
            EXIT
          ENDIF
        ENDDO
110     CONTINUE
        write(*,*) 'Conduct tide prediction at STATION: ',BUFFER(1:L1)
        IF(N.GT.NWLON_STA) THEN 
          WRITE(*,*) 'STATION ID IS NOT FOUND IN THE HC NetCDF FILE'
          IF(WL_FLAG(I).EQ.0) THEN
            WRITE(*,*) 'PLEASE ADD HC OF STATION '//TRIM(NOS_ID(I))
            WRITE(*,*)TRIM(OFS)//' STOP HERE'
            STOP
          ENDIF 		
        ELSE   
          FOUT=TRIM(NOS_ID(I))//'.prd'
          DO K=1,37
            AMP(K)=tide_amp(N,K)
            EPOC(K)=tide_epoc(N,K)
            if(BUFFER(1:L1).EQ.'8651370') then
              write(*,*) K,AMP(K),EPOC(K)
            endif  
          ENDDO
          CALL NOS_PRD(START_TIMEm1,END_TIME,KINDAT,DELT_PRD,CONV,
     &         XMAJOR,AMP,EPOC,FOUT,ONED1,ONED2)
          DO N=1,NREC
            WL_PRD(I,N)=ONED1(N)
          ENDDO
        ENDIF   
      ENDDO	   

C ----------------------------------------------------------------------------
C  Process real time observations in BUFR files of NCEP data tank at NWLON stations
C ----------------------------------------------------------------------------
      WRITE(FOUT,201) 'DATA_OBC_',IYRS,IMMS,IDDS,IHHS,'.dat'  
201   FORMAT(A9,I4.4,3I2.2,A4)
      FOUT=TRIM(OFS)//'_'//TRIM(adjustL(FOUT))
      CLOSE(15) 
      OPEN(15,file=TRIM(FOUT) )
      write(15,*) 'ID        lat     lon   year  mon day hour
     &  Disch   Tem SST   Salinity'
      DO IZ=INT(day_start),INT(day_end)
        jday=IZ+jbase_date
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
          IF(MOD(IYR,4).EQ.0) DAYS_PER_MONTH(2)=29   !!    Leap Year
          IF(IDD.GT.DAYS_PER_MONTH(IMM)) THEN
            IDD=IDD - DAYS_PER_MONTH(IMM)
            IMM=IMM+1
            IF(IMM.GT.12) THEN
              IMM=IMM-12
              IYR=IYR+1
            ENDIF
          ENDIF
        ENDIF
        WRITE(BUFRFILE,700) '/',IYR,IMM,IDD,'/b001/'
700     FORMAT(A1,I4.4,2I2.2,A6)
        BUFRFILE=TRIM(NOSWLDIR)//trim(BUFRFILE)//trim(NOSBUFR) 
        INQUIRE(FILE=trim(BUFRFILE),EXIST=FEXIST)
        IF(.NOT.FEXIST) GOTO 860
        write(*,*) 'BUFR FILE= ',trim(BUFRFILE)
        write(CTMP,'(a1,I4.4,2I2.2)') '.',IYR,IMM,IDD
        CMD='cp '//trim(BUFRFILE)//'  '//TRIM(NOSBUFR)//trim(CTMP)
        call system(trim(CMD))
        CMD='cp '//trim(BUFRFILE)//'  '//TRIM(NOSBUFR)
        call system(trim(CMD))
        BUFRFILE=trim(NOSBUFR)

        LUNIN=11
	CLOSE(LUNIN)
        OPEN(LUNIN,file=trim(BUFRFILE),FORM='UNFORMATTED')

C  -------------------OPEN AND READ THRU THE INPUT BUFR FILE ------------------       
        CALL OPENBF(LUNIN,'IN',LUNIN)                                     

C  --------------READ THROUGH THE MESSAGES/SUBSETS IN THE FILE------------------
C  
        DO WHILE(IREADMG(LUNIN,SUBSET,IDATE).EQ.0)
          DO WHILE(IREADSB(LUNIN).EQ.0)
c  --------------READ THE INTERNAL DATE AND CHECK FOR REALISM---------------------
C                           
            CALL UFBINT(LUNIN,DATES,5,1,IRET,
     &           'YEAR MNTH DAYS HOUR MINU')
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
            CALL READLC(LUNIN, stnbufrid, 'RPID')
            CALL UFBINT(LUNIN,DATES,5,1,IRET,'CLAT CLON SELV')
            clat=DATES(1)
            clon=DATES(2)
            selv=DATES(3)
            DO I=1,NSTA 
              IF(trim(stnbufrid).EQ.TRIM(NWS_ID(I))) THEN
C  ----------------------------------------------------------------------------
C  GET SEA SURFACE TEMPERATURE DATA ALONG WITH DATA CHECK AND
C  TIME INCREMENT AND DISPLACEMENT INFORMATION
C  SST1 -- Sea Surface Temperature
C  QMST -- Sea Surface Data Check Flag
C  AWCK -- Tide Station Automated Water Data Check Flag
C  MWCK -- Tide Station Manual Water Data Check Flag
C  TPMI -- Time Period or Replacement
C  ----------------------------------------------------------------------------
                LLEN=LEN_TRIM(BUFRFILE)
                IF(BUFRFILE(LLEN-4:LLEN).EQ.'xx005') THEN
                  CALL UFBINT(LUNIN,DATES,5,1,IRET,
     &	               'SST1 TMDB AWCK MWCK TPMI')
                ELSEIF(BUFRFILE(LLEN-4:LLEN).EQ.'xx012') THEN 
C   GET AIR TEMPERATURE AND SEA SURFACE TEMPERATURE DATA
                  CALL UFBINT(LUNIN,DATES,5,1,IRET,'WATM TMDB')
                ENDIF	 
                if(DATES(1).ge.bmiss/2.0) then
                  SST=-99999.99
                else	  
                  SST=DATES(1)-273.15  !! Convert from Kelvin to deg C
                endif
                IF (SST .LT. 0.0 )SST=-99999.99   !! avoid bad values from real time observations
                if(DATES(2).ge.bmiss/2.0) then
                  ATMP=-99999.99
                else	  
                  ATMP=DATES(2)-273.15  !! Convert from Kelvin to deg C   
                endif

	        IF(abs(SST).LT.40.00) THEN
                  IF(NTR_T(I).LT.1) THEN 
                    NTR_T(I)=NTR_T(I)+1
                    RTIME_T(I,NTR_T(I) )=dayj
                    T_OBS(I,NTR_T(I))=SST
                  ELSE
                    IF(dayj.GT.RTIME_T(I,NTR_T(I))) THEN 
                      NTR_T(I)=NTR_T(I)+1
                      RTIME_T(I,NTR_T(I) )=dayj
                      T_OBS(I,NTR_T(I))=SST
                    ENDIF
                  ENDIF
                ENDIF

C  ----------------------------------------------------------------------------
C  GET CONDUCTIVITY AND SALINITY
C  ----------------------------------------------------------------------------
                CALL UFBINT(LUNIN,DATES,5,1,IRET,'SALN WACN')
                if(DATES(1).ge.bmiss/2.0) then
                  SALN=-99999.99
                else	  
                  SALN=DATES(1)
                endif

                if(DATES(2).ge.bmiss/2.0) then
                  COND=-99999.99        !     Unit of COND is mS/cm for NOS station
                else	  
                  COND=DATES(2)   
                endif

                IF(abs(SALN).GT.999.99) THEN
	          IF((abs(SST).LT.999.99).AND.
     &               (abs(COND).LT.999.99)) THEN
                    SALN=SAL(COND,SST,0.0)
                  ELSE
                    SALN=-99999.99
                  ENDIF   
                ENDIF  
                IF (SALN .LT. -0.5 )SALN=-99999.99   !! avoid bad values from real time observations

                IF(abs(SALN).LT.40.00) THEN
                  IF(NTR_S(I).LT.1) THEN 
                    NTR_S(I)=NTR_S(I)+1
                    RTIME_S(I,NTR_S(I) )=dayj
                    S_OBS(I,NTR_S(I))=SALN
                  ELSE
                    IF(dayj.GT.RTIME_S(I,NTR_S(I))) THEN 
                      NTR_S(I)=NTR_S(I)+1
                      RTIME_S(I,NTR_S(I) )=dayj
                      S_OBS(I,NTR_S(I))=SALN
                    ENDIF
                  ENDIF
                ENDIF

C  -----------------------------------------------------------------------------
C  GET TIDAL ELEVATION WITH RESPECT TO CHART AND METEOROLOGICAL RESIDUAL TIDAL 
C  ELEVATION
C  ----------------------------------------------------------------------------
                IF(BUFRFILE(LLEN-4:LLEN).EQ.'xx005') THEN      
                  CALL UFBINT(LUNIN,DATES,5,1,IRET,'TERC TIDER')
                ELSEIF(BUFRFILE(LLEN-4:LLEN).EQ.'xx012') THEN 
                  CALL UFBINT(LUNIN,DATES,5,1,IRET,'TLLW TIDER')
                ENDIF	 

                if(DATES(1).ge.bmiss/2.0) then
                  EL=-99999.99
                else	  
                  EL=DATES(1)  
                endif

                if(DATES(2).ge.bmiss/2.0) then
                  SWL=-99999.99
                else	  
                  SWL=DATES(2)   
                endif

                IF(abs(EL).LT.40.00) THEN
                  IF(NTR(I).LT.1) THEN 
                    NTR(I)=NTR(I)+1
                    RTIME(I,NTR(I) )=dayj
                    WL_OBS(I,NTR(I))=EL-DATUM(I)
                    SWL_OBS(I,NTR(I))=SWL
                  ELSE
                    IF(dayj.GT.RTIME(I,NTR(I))) THEN 
                      NTR(I)=NTR(I)+1
                      RTIME(I,NTR(I) )=dayj
                      WL_OBS(I,NTR(I))=EL-DATUM(I)
                      SWL_OBS(I,NTR(I))=SWL
                    ENDIF
                  ENDIF
                ENDIF
                write(15,780) trim(stnbufrid),clon,clat,IYR,IMM,
     &		  IDD,IHH,IMN,EL,ATMP,SST,SALN,COND
              ENDIF 
            ENDDO
780         FORMAT(a10,1x,2F10.4,I5,4i3,10F12.4)
800         CONTINUE
          ENDDO
        ENDDO
850     CONTINUE
        CALL CLOSBF(LUNIN)
        CLOSE(LUNIN)
860     CONTINUE
      ENDDO

c-------------------------------------------------------------------------------
C  Decoding USGS BURF file if used
C------------------------------------------------------------------------------
      IF(.NOT.USGS_L) GOTO 9000
      DO IZ=INT(day_start),INT(day_end)
        jday=IZ+jbase_date
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
          IF(MOD(IYR,4).EQ.0) DAYS_PER_MONTH(2)=29   !!    Leap Year
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
        BUFRFILE=TRIM(NOSWLDIR)//trim(BUFRFILE)//trim(USGSBUFR) 
        INQUIRE(FILE=trim(BUFRFILE),EXIST=FEXIST)
        IF(.NOT.FEXIST) GOTO 8860
        write(*,*) 'BUFR FILE= ',trim(BUFRFILE)
        write(CTMP,'(a1,I4.4,2I2.2)') '.',IYR,IMM,IDD
        CMD='cp -p '//trim(BUFRFILE)//'  '//TRIM(USGSBUFR)//trim(CTMP)
        call system(trim(CMD) )
        BUFRFILE=trim(USGSBUFR)//trim(CTMP)
        LUNIN=11
        CLOSE(LUNIN)
        OPEN(LUNIN,file=trim(BUFRFILE),FORM='UNFORMATTED')
        CALL OPENBF(LUNIN,'IN',LUNIN)                                     
C  --------------READ THROUGH THE MESSAGES/SUBSETS IN THE FILE------------------
        DO WHILE(IREADMG(LUNIN,SUBSET,IDATE).EQ.0)                       
          DO WHILE(IREADSB(LUNIN).EQ.0)                                    
c  --------------READ THE INTERNAL DATE AND CHECK FOR REALISM---------------------
            CALL UFBINT(LUNIN,DATES,5,1,IRET,
     &           'YEAR MNTH DAYS HOUR MINU')
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
            CALL READLC(LUNIN, stnbufrid, 'RPID')
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
                  SST=-99999.99
                else	  
                  SST=DATES(1)-273.15  !! Convert from Kelvin to deg C
                endif
                IF (SST .LT. 0.0 )SST=-99999.99   !! avoid bad values from real time observations
                if(DATES(2).ge.bmiss/2.0) then
                  ATMP=-99999.99
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
C  GET RIVER STAGE DATA INCLUDING INFO ON USGS SENSOR TYPE 
c   Stage height is in meters
                CALL UFBINT(LUNIN,DATES,5,1,IRET,'RSH29 STRV')
                if(DATES(1).ge.bmiss/2.0) then
                  rsh29=-99999.99
                else	  
                  rsh29=DATES(1)  
                endif

                if(DATES(2).ge.bmiss/2.0) then
                  strv=-99999.99
                else	  
                  strv=DATES(2)  
                endif

C  -----------------------------------------------------------------------------
C GET RIVER STAGE HEIGHT ABOVE NGVD 1929, STREAM VELOCITY, AND SALINITY
C  ----------------------------------------------------------------------------
                CALL UFBINT(LUNIN,DATES,5,1,IRET,'RSHM DDRS')
                if(DATES(1).ge.bmiss/2.0) then
                  EL=-99999.99
                else	  
                  EL=DATES(1)  
                endif

                IF(abs(EL).LT.40.00) THEN
                  IF(NTR(I).LT.1) THEN 
                    NTR(I)=NTR(I)+1
                    RTIME(I,NTR(I) )=dayj
                    WL_OBS(I,NTR(I))=EL-DATUM(I)
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

C  ----------------------------------------------------------------------------
C  GET CONDUCTIVITY AND SALINITY
C  Convert specific conductance C25,0 of USGS into conductivity. See Sea Bird Electronics, Inc
C  Fresh Water Conductivity Measurements using SBE-19 SEACAT Profiler
C  Specific C25,0 [us/cm]=(C/[1+0.02*(Temperature-25)]  where C is in ms/cm 
C  ----------------------------------------------------------------------------
                CALL UFBINT(LUNIN,DATES,5,1,IRET,'SALN WACN')
                if(DATES(1).ge.bmiss/2.0) then
                  SALN=-99999.99
                else	  
                  SALN=DATES(1)
                endif

                if(DATES(2).ge.bmiss/2.0) then
                  COND=-99999.99     ! Unit of COND is uS/cm for USGS station
                else	  
                  COND=DATES(2)   
                endif

                IF(abs(SALN).GT.999.99) THEN
                  IF((abs(SST).LT.999.99).AND.
     &               (abs(COND).LT.999.99)) THEN
                    COND=COND*(1.0+0.02*(SST-25.0))  !! convert from uS/cm to mS/cm
                    SALN=SAL(COND,SST,0.0)
                  ENDIF   
                ENDIF  
                IF (SALN .LT. -0.5 )SALN=-99999.99   !! avoid bad values from real time observations

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
     &	          IDD,IHH,IMN,EL,ATMP,SST,SALN,COND
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

C  ------------------------------------------------------------------------------
9000  CONTINUE      
      CLOSE(15)
      INQUIRE(FILE=TRIM(FOUT),EXIST=FEXIST)
      IF(FEXIST) THEN
        CMD='sort '//TRIM(FOUT)//' > tmp.dat'
        call system(trim(CMD) )
        CMD='uniq tmp.dat > tmp1.dat'
        call system(trim(CMD) )
        CMD='cp tmp1.dat '//TRIM(FOUT)
        call system(trim(CMD) )
      ENDIF	 
      
C  -----------------------------------------------------------------------------
C  End of reading WL BUFR FILES 
C  Begin WL QC procedures
C  ----------------------------------------------------------------------------
      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF(ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF(ALLOCATED(oned4)) DEALLOCATE(oned4)
      allocate(oned1(NMAX))
      allocate(oned2(NMAX))
      allocate(oned3(NMAX))
      allocate(oned4(NMAX))

      DO I=1,NSTA
        IF(NTR(I).GE.20) THEN
          avg=0.0
          NTMP=0
          DO N=1,NTR(I)
            IF(ABS(WL_OBS(I,N)).LE.10.0) THEN
              NTMP=NTMP+1
              AVG=AVG+WL_OBS(I,N)
              ONED1(NTMP)=WL_OBS(I,N)
            ENDIF
          ENDDO 
          IF(NTMP.GT.0) AVG=AVG/NTMP
          SD=0.0    
          IF(NTMP.GT.20) THEN
            DO N=1,NTMP
              SD=SD+(ONED1(N)-AVG)**2
            ENDDO 
            SD=SQRT(SD/(NTMP-1))
          ELSE
            SD=1.0
          ENDIF  
          BOUND_L=AVG-3.0*SD
          BOUND_U=AVG+3.0*SD 
          write(*,*) 'Bound of WL= ',BOUND_L,BOUND_U,AVG,NOS_ID(I)

          NTMP=0
          DO N=1,NTR(I)
            IF((WL_OBS(I,N).GE.BOUND_L).AND. 
     &	       (WL_OBS(I,N).LE.BOUND_U)) THEN
              NTMP=NTMP+1
              ONED1(NTMP)=WL_OBS(I,N)
              ONED2(NTMP)=RTIME(I,N)
            ENDIF
          ENDDO
          NTR(I)= NTMP

          DO N=1,NTMP
            WL_OBS(I,N)=ONED1(N) 
            RTIME(I,N)=ONED2(N)    
          ENDDO
        ENDIF   
        
C Gradient change checking  Dh/Dt < 0.7 meters/hour, and assume first data is good
        IF(NTR(I).GE.2) THEN
          NTMP=1
          ONED1(NTMP)=WL_OBS(I,1)
          ONED2(NTMP)=RTIME(I,1)
          DO N=2,NTR(I)  
            GD=(WL_OBS(I,N)-ONED1(NTMP))/(RTIME(I,N)-ONED2(NTMP))/24.0
            IF(ABS(GD).LE.0.7)THEN
              NTMP=NTMP+1
              ONED1(NTMP)=WL_OBS(I,N)
              ONED2(NTMP)=RTIME(I,N)
            ENDIF
          ENDDO
          
          DO N=1,NTMP
            WL_OBS(I,N)=ONED1(N) 
            RTIME(I,N)=ONED2(N)    
          ENDDO
          NTR(I)= NTMP
          WRITE(*,*) I,' NTMP OF WL= ',NTMP,' NTR= ',NTR(I)
	ENDIF  
        
        BUFFER='NUMBER OF WL AT '//TRIM(NOS_ID(I))
        BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
        BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
        TMPTXT=' '
        WRITE(TMPTXT,'(I5)') NTR(I)
        BUFFER=TRIM(BUFFER)//TRIM(TMPTXT)
        WRITE(*,*) TRIM(BUFFER)
        WRITE(ICORMS,'(A)') TRIM(BUFFER)

C  -----------------------------------------------------------------------------
C  Begin QC procedures for Temperature
C  ----------------------------------------------------------------------------
        IF(NTR_T(I).GE.2) THEN
          avg=0.0
          NTMP=0
          DO N=1,NTR_T(I)
            IF(ABS(T_OBS(I,N)).LE.40.0) THEN  !Deg. C
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
          write(*,*) 'Bound of T = ',BOUND_L,BOUND_U,AVG,NOS_ID(I)

          NTMP=0
          DO N=1,NTR_T(I)
            IF((T_OBS(I,N).GE.BOUND_L).AND.
     &	       (T_OBS(I,N).LE.BOUND_U)) THEN
              NTMP=NTMP+1
              ONED1(NTMP)=T_OBS(I,N)
              ONED2(NTMP)=RTIME_T(I,N)
            ENDIF
          ENDDO

          NTR_T(I)= NTMP
          DO N=1,NTMP
            T_OBS(I,N)=ONED1(N) 
            RTIME_T(I,N)=ONED2(N)    
          ENDDO 
        ENDIF  

C Gradient change checking  DT/Dt < 2.0 deg./hour, and assume first data is good
        IF(NTR_T(I).GE.2) THEN
          NTMP=1
          ONED1(NTMP)=T_OBS(I,1)
          ONED2(NTMP)=RTIME_T(I,1)
          DO N=2,NTR_T(I)  
            GD=(T_OBS(I,N)-ONED1(NTMP))/(RTIME_T(I,N)-ONED2(NTMP))/
     &         24.0
            IF(ABS(GD).LE.2.0) THEN
              NTMP=NTMP+1
              ONED1(NTMP)=T_OBS(I,N)
              ONED2(NTMP)=RTIME_T(I,N)
            ENDIF
          ENDDO
          WRITE(*,*) I,' NTMP of TEMP= ',NTMP,' NTR= ',NTR_T(I)

          NTR_T(I)= NTMP
          DO N=1,NTMP
            T_OBS(I,N)=ONED1(N) 
            RTIME_T(I,N)=ONED2(N)    
          ENDDO
        ENDIF  
        
C  -----------------------------------------------------------------------------
C  Begin QC procedures for Salinity
C  ----------------------------------------------------------------------------
        IF(NTR_S(I).GE.2) THEN
          avg=0.0
          NTMP=0
          DO N=1,NTR_S(I)
            IF(ABS(S_OBS(I,N)).LE.40.0) THEN  ! PSU
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
          write(*,*) 'Bund of SALT = ',BOUND_L,BOUND_U,AVG,NOS_ID(I)

          NTMP=0
          DO N=1,NTR_S(I)
            IF((S_OBS(I,N).GE.BOUND_L).AND.
     &	       (S_OBS(I,N).LE.BOUND_U)) THEN
              NTMP=NTMP+1
              ONED1(NTMP)=S_OBS(I,N)
              ONED2(NTMP)=RTIME_S(I,N)
            ENDIF
          ENDDO
          NTR_S(I)= NTMP
          DO N=1,NTMP
            S_OBS(I,N)=ONED1(N) 
            RTIME_S(I,N)=ONED2(N)    
          ENDDO 
        ENDIF  
        
C Gradient change checking DS/Dt < 3.0 deg./hour, and assume first data is good
        IF(NTR_S(I).GE.2) THEN
          NTMP=1
          ONED1(NTMP)=S_OBS(I,1)
          ONED2(NTMP)=RTIME_S(I,1)
          DO N=2,NTR_S(I)  
            GD=(S_OBS(I,N)-ONED1(NTMP))/(RTIME_S(I,N)-ONED2(NTMP))/
     &         24.0
            IF(ABS(GD).LE.3.0) THEN
              NTMP=NTMP+1
              ONED1(NTMP)=S_OBS(I,N)
              ONED2(NTMP)=RTIME_S(I,N)
            ENDIF
          ENDDO
          WRITE(*,*) I,' NTMP of SALT= ',NTMP,' NTR_S= ',NTR_S(I)
          NTR_S(I)= NTMP
          DO N=1,NTMP
            S_OBS(I,N)=ONED1(N) 
            RTIME_S(I,N)=ONED2(N)    
          ENDDO
        ENDIF
      ENDDO	
      
C  -----------------------------------------------------------------------------
C  End of  QC procedures
C  Begin detiding SWL=OBS -PRED
C  ----------------------------------------------------------------------------
      DO I=1,NSTA
        FOUT=TRIM(NOS_ID(I))//'.obs'
        CLOSE(10)
        OPEN(10,file=TRIM(FOUT))
        N0=1
        DO N=1,NTR(I)
         IF(ABS(WL_OBS(I,N)) .LT. 99999.99) THEN
          jday=RTIME(I,N)+jbase_date
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
            IF(MOD(IYR,4).EQ.0) DAYS_PER_MONTH(2)=29   !!    Leap Year
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
870       CONTINUE
          IF(N1.GT.NREC) SWL_OBS(I,N)=-99999.99
	   
          WRITE(10,900) RTIME(I,N),IYR,IMM,IDD,IHH,IMN,
     &	    WL_PRD(I,N1),WL_OBS(I,N),SWL_OBS(I,N),T_OBS(I,N)
         ENDIF    
        ENDDO
        write(*,*) 'STA= ',trim(NOS_ID(I)),' Number of WL obs= ',
     &  NTR(I)
      ENDDO	
900   FORMAT(f10.4,1x,I5,4i3,4F12.4)
      
C-----------------------------------------------------------------------
C  If there is no WL obs at primary station, then looking for its backup station
C-----------------------------------------------------------------------
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
        IF(WL_FLAG(I).NE.0) THEN
          AVGERR(I)=0.0
          DO N=1,NTMAX_WL
            SWL_OBS(I,N)=0.0
          ENDDO
        ELSE
          BUFFER='NUMBER OF WL AT '//TRIM(NOS_ID(I))
          BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
          BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
          TMPTXT=' '
          WRITE(TMPTXT,'(I5)') NTR(I)
          BUFFER=TRIM(BUFFER)//TRIM(TMPTXT)
          WRITE(ICORMS,'(A)') TRIM(BUFFER)
          IF(NTR(I).LE.20) THEN  
cc  20 can be changed depending upon how many data are required to be available
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
                IF((IBKP1.GT.0).AND.(NTR(IBKP1).GT.20)) THEN
                  write(*,*) 'Use second backup ',IBKP,' for I= ',I
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
          
c-----------------------------------------------------------------------
C Use 6-hour ramping up	
C Interpolate into same time intreval as zeta_time  
C Filling missing values using linear interplolation 
c-----------------------------------------------------------------------
          FOUT=TRIM(NOS_ID(I))//'.swl'
          CLOSE(10)
          OPEN(10,file=TRIM(FOUT))
          DO N=1,NTR(I)
            WRITE(10,35)RTIME(I,N),SWL_OBS(I,N)
          ENDDO
          CLOSE(10)

C-----------------------------------------------------------------------
C Separate bias as mean error + time varying difference
C-----------------------------------------------------------------------
          N0=0
          DO N1=1,NTR(I)
            IF(SWL_OBS(I,N1).GT.-10.0) THEN
              N0=N0+1
              ONED1(N0)= RTIME(I,N1)
              ONED2(N0)= SWL_OBS(I,N1)
            ENDIF
          ENDDO
          NTR(I)=N0

          DO N1=1,NTR(I)
            RTIME(I,N1)=ONED1(N1)
            SWL_OBS(I,N1)=ONED2(N1)
          ENDDO

          TIME1=RTIME(I,1)
          TIME2=RTIME(I,NTR(I))
          WRITE(*,*) 'TIME1=',TIME1,' TIME2=',TIME2,zeta_time(1),
     &	    zeta_time(NTMAX_WL),' NTR=',NTR(I),NTMAX_WL
          N0=0
          IF(TIME1.GT.zeta_time(1)) THEN
            N0=N0+1
            ONED1(N0)= zeta_time(1)
            ONED2(N0)= SWL_OBS(I,1)
          ENDIF
          DO N1=1,NTR(I)
            N0=N0+1
            ONED1(N0)= RTIME(I,N1)
            ONED2(N0)= SWL_OBS(I,N1)
          ENDDO  
          write(*,*) 'N0=',N0,' NTR(I)=',NTR(I)

          DO N1=2,NTMAX_WL
            IF((zeta_time(N1).GE.TIME2)) THEN
              N0=N0+1
              ONED1(N0)= zeta_time(N1)
              ONED2(N0)= SWL_OBS(I,NTR(I))
              EXIT
            ENDIF
          ENDDO
903       CONTINUE
          write(*,*) 'N0=',N0,' NTR(I)=',NTR(I)

          NTR(I)=N0
          DO N1=1,NTR(I)
            RTIME(I,N1)=ONED1(N1)
            SWL_OBS(I,N1)=ONED2(N1)
          ENDDO     
          TIME1=RTIME(I,1)
          TIME2=RTIME(I,NTR(I))
          WRITE(*,*) 'TIME1=',TIME1,' TIME2=',TIME2,zeta_time(1),
     &	    zeta_time(NTMAX_WL),' NTR=',NTR(I),NTMAX_WL

          N0=0
          DO N1=1,NTMAX_WL
            IF((zeta_time(N1).GE.TIME1).AND.
     &	       (zeta_time(N1).LE.TIME2)) THEN
              N0=N0+1
              ONED3(N0)=zeta_time(N1)
            ENDIF
          ENDDO

          IF(N0.LT.2) THEN
            WRITE(*,*) 'Real time water level data is insufficient'
            WRITE(*,*) 'TIME1=',TIME1,' TIME2=',TIME2,zeta_time(1),
     &	       zeta_time(NTMAX_WL),' NTR=',NTR(I),NTMAX_WL
            WRITE(*,*) 'No correction'
            AVGERR(I)=0.0
            DO N=1,NTMAX_WL
              SWL_OBS(I,N)=0.0
            ENDDO
          ELSE
            NTMP=0 
            DO N=1,NTR(I)
              IF(ABS(SWL_OBS(I,N)).LE.3.0) THEN
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
              CALL lineararray(N0,tmpp1,tmpp2,NTMP,tmpp3,tmpp4)
              DO N=1,N0
                oned4(N)=tmpp2(N)
              END DO
              deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
            ELSE
              DO N=1,N0
                oned4(N)=0.0
              ENDDO
            ENDIF  	    

            AVG=0.0
            DO N=1,N0
              DO N1=1,NTMAX_WL
!                IF(ONED3(N).EQ.zeta_time(N1)) THEN
                IF(ABS(ONED3(N)-zeta_time(N1)).LE.1.0e-10) THEN
cc avg=SWL-ETSS at the corresponding grid
                  ONED1(N)=ONED4(N)-WLOBC(GRIDID_STA(I),N1) 
                  AVG=AVG+ONED1(N)
                  EXIT
                ENDIF	  
              ENDDO
905           CONTINUE
            ENDDO

            IF(N0.GT.0) AVG=AVG/N0
            AVGERR(I)=AVG
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
            CALL lineararray(NTMAX_WL,tmpp1,tmpp2,N0,tmpp3,tmpp4)
            DO N=1,NTMAX_WL
              SWL_OBS(I,N)=tmpp2(N)
              IF(zeta_time(N).GT.ONED3(N0)) SWL_OBS(I,N)=0.0 
            ENDDO
            deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
          ENDIF	 
        ENDIF	 

C-----------------------------------------------------------------------
C  Process water temperature for stations TS_FLAG > 0
C-----------------------------------------------------------------------
        IF(TS_FLAG(I).EQ.0) THEN   !! no correction is needed
          AVGERR_T(I)=0.0
          DO N=1,NTMAX
            T_OBS(I,N)=0.0
          ENDDO
        ELSEIF(TS_FLAG(I).EQ.1) THEN  !! need real time T & S observations
          IF(NTR_T(I).GT.5) THEN  !!  5 can be changed depending upon how many data are required to be available
            BUFFER='NUMBER OF TEMP AT '//TRIM(NOS_ID(I))
            BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
            BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
            TMPTXT=' '
            WRITE(TMPTXT,'(I5)') NTR_T(I)
            BUFFER=TRIM(BUFFER)//TRIM(TMPTXT)
            WRITE(ICORMS,'(A)') TRIM(BUFFER)
          ELSE  
            BUFFER='WARNING: No real-time data at STATION OF '//TRIM(NOS_ID(I))
            WRITE(ICORMS,'(A)') TRIM(BUFFER)

            IBKP=BACKUP_SID(I)
            IF(IBKP.GT.0) THEN
              IF(NTR_T(IBKP).GT.5) THEN
                BUFFER='USING BACKUP STATION OF '//TRIM(NOS_ID(IBKP))
                WRITE(ICORMS,'(A)') TRIM(BUFFER)
                write(*,*) 'Using first backup ',IBKP,' for T at I= ',I
                NTR_T(I)=NTR_T(IBKP)
                DO N=1,NTR_T(I)
                  RTIME_T(I,N)=RTIME_T(IBKP,N)
                  T_OBS(I,N)=T_OBS(IBKP,N)
                ENDDO  
              ELSE
                IBKP1=BACKUP_SID(IBKP)
                IF((IBKP1.GT.0).AND.(NTR_T(IBKP1).GT.5)) THEN
                  BUFFER='USING BACKUP STATION OF '//
     &                   TRIM(NOS_ID(IBKP1))
                  WRITE(ICORMS,'(A)') TRIM(BUFFER)
                  write(*,*) 'Use second backup ',IBKP, ' for I= ',I
                  NTR_T(I)=NTR_T(IBKP1)
                  DO N=1,NTR_T(I)
                    RTIME_T(I,N)=RTIME_T(IBKP1,N)
                    T_OBS(I,N)=T_OBS(IBKP1,N)
                  ENDDO
                ELSE   
                  write(*,*) 'WARNING:'
                  write(*,*) 'No observation is at second backup'
                  write(*,*) 'Use climatology for ',trim(NOS_ID(I))
                  BUFFER='NO OBSERVATIONS, USE CLIMATOLOGIC DATASET'
                  WRITE(ICORMS,'(A)') TRIM(BUFFER)

                  NTR_T(I)=NT_CLIM
                  DO N=1,NTR_T(I)
                    RTIME_T(I,N)=TIME_CLIM(N)
                    T_OBS(I,N)=T_CLIM(I,N,1)    !! use SST only
                  ENDDO
                ENDIF     	   
	      ENDIF
            ELSE     !! use climatology 
              write(*,*)'WARNING: No real-time observation'
              write(*,*) 'Use climatology for I= ',I,trim(NOS_ID(I))
              NTR_T(I)=NT_CLIM
              DO N=1,NTR_T(I)
                RTIME_T(I,N)=TIME_CLIM(N)
                T_OBS(I,N)=T_CLIM(I,N,1)    !! use SST only
              ENDDO
            ENDIF     	   
          ENDIF
        ELSEIF(TS_FLAG(I).EQ.2) THEN !! correction using climatological dataset
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
          OPEN(10,file=TRIM(FOUT))
          DO N=1,NTR_T(I)
            WRITE(10,35)RTIME_T(I,N),T_OBS(I,N)
          ENDDO
          CLOSE(10)

C-----------------------------------------------------------------------
C  Separate bias as mean error + time varying difference
C-----------------------------------------------------------------------
          N0=0
          DO N1=1,NTR_T(I)
            IF(T_OBS(I,N1).GT.-10.0) THEN
              N0=N0+1
              ONED1(N0)= RTIME_T(I,N1)
              ONED2(N0)= T_OBS(I,N1)
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
            ONED1(N0)= TS_time(1)
            ONED2(N0)= T_OBS(I,1)
          ENDIF

          DO N1=1,NTR_T(I)
            N0=N0+1
            ONED1(N0)= RTIME_T(I,N1)
            ONED2(N0)= T_OBS(I,N1)
          ENDDO  
          write(*,*) 'N0=',N0,' NTR_T(I)=',NTR_T(I)

          DO N1=2,NTMAX
            IF(TS_time(N1).GE.TIME2) THEN
              N0=N0+1
              ONED1(N0)= TS_time(N1)
              ONED2(N0)= T_OBS(I,NTR_T(I))
              EXIT
            ENDIF
          ENDDO
923       CONTINUE
          write(*,*) 'N0=',N0,' NTR_T(I)=',NTR_T(I)

          NTR_T(I)=N0
          DO N1=1,NTR_T(I)
            RTIME_T(I,N1)=ONED1(N1)
            T_OBS(I,N1)=ONED2(N1)
          ENDDO     
          TIME1=RTIME_T(I,1)
          TIME2=RTIME_T(I,NTR_T(I))
          WRITE(*,*) 'TIME1=',TIME1,' TIME2=',TIME2,TS_time(1),
     &	     TS_time(NTMAX),'NTR_T=',NTR_T(I),NTMAX

          N0=0
          DO N1=1,NTMAX
            IF((TS_time(N1).GE.TIME1).AND.
     &	       (TS_time(N1).LE.TIME2)) THEN
              N0=N0+1
              ONED3(N0)=TS_time(N1)
            ENDIF
          ENDDO

          IF(N0.LT.1) THEN
            WRITE(*,*) 'WARNING MESSAGE: real time water temperature is insufficient'
            WRITE(*,*) 'Please wait for 20 min and then check '
            WRITE(*,*) 'File size of ', TRIM(NOSBUFR),' and ', TRIM(USGSBUFR)
            WRITE(*,*) 'Rerun prep job' 
            write(*,*) 'I=',I,TIME1,TIME2,TS_time(1),TS_time(NTMAX)
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
          CALL lineararray(N0,tmpp1,tmpp2,NNTMP,tmpp3,tmpp4)
          DO N=1,N0
            ONED4(N)=tmpp2(N)
          END DO
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)

          AVG=0.0
          DO N=1,N0
            DO N1=1,NTMAX
C!              IF(ONED3(N).EQ.TS_time(N1)) THEN
               IF(ABS(ONED3(N)-TS_time(N1)).LT.1.0e-10) THEN
cc avg=Tobs - NCOM at the corresponding grid
                ONED1(N)=ONED4(N)-TEMPOBC_M(GRIDID_STA(I),KBm,N1)
                AVG=AVG+ONED1(N)
                EXIT
              ENDIF	  
            ENDDO
925         CONTINUE
          ENDDO

          IF(N0.GT.0) AVG=AVG/N0
          AVGERR_T(I)=AVG
          DO N=1,N0
            ONED4(N)=ONED1(N)-AVG   !! err = avg + err'
          ENDDO  
          IF(ONED3(N0).LT.day_end) THEN
            N0=N0+1
cc Use 6-hour ramping up from last err' to zero
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
          DO N=1,NTMAX
            ONED1(N)=tmpp2(N)
          End do
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)

          DO N=1,NTMAX
            T_OBS(I,N)=oned1(N)
            IF(TS_time(N).GT.ONED3(N0) )T_OBS(I,N)=0.0 
          ENDDO
        ENDIF  

C-----------------------------------------------------------------------
C  Process salinity for stations TS_FLAG > 0
C-----------------------------------------------------------------------
        IF(TS_FLAG(I).EQ.0) THEN !! no correction is needed
          AVGERR_S(I)=0.0
          DO N=1,NTMAX
            S_OBS(I,N)=0.0
          ENDDO
        ELSEIF(TS_FLAG(I).EQ.1) THEN !! need real time T & S observations
          IF(NTR_S(I).GT.5) THEN  !!  5 can be changed depending upon how many data are required to be available
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
                write(*,*) 'Using backup ',IBKP,' for SAL at I= ',I
	        NTR_S(I)=NTR_S(IBKP)
                DO N=1,NTR_S(I)
                  RTIME_S(I,N)=RTIME_S(IBKP,N)
                  S_OBS(I,N)=S_OBS(IBKP,N)
                ENDDO  
              ELSE
                IBKP1=BACKUP_SID(IBKP)
                IF((IBKP1.GT.0).AND.(NTR_S(IBKP1).GT.5)) THEN
                  BUFFER='USING BACKUP STATION OF '//
     &                   TRIM(NOS_ID(IBKP1))
                  WRITE(ICORMS,'(A)') TRIM(BUFFER)
                  write(*,*) 'Use second backup ',IBKP, 'for I= ',I
                  NTR_S(I)=NTR_S(IBKP1)
                  DO N=1,NTR_S(I)
                    RTIME_S(I,N)=RTIME_S(IBKP1,N)
                    S_OBS(I,N)=S_OBS(IBKP1,N)
                  ENDDO
                ELSE 
                  WRITE(*,*) 'WARNING: no real-time salinity'
                  write(*,*) 'Use sal climatology for ',trim(NOS_ID(I))
                  NTR_S(I)=NT_CLIM
                  DO N=1,NTR_S(I)
                    RTIME_S(I,N)=TIME_CLIM(N)
                    S_OBS(I,N)=S_CLIM(I,N,1)    !! use SST only
                  ENDDO
                ENDIF     	   
              ENDIF
            ELSE     !! use climatology 
              WRITE(*,*) 'WARNING: no real-time salinity'
              write(*,*) 'Use sal climatology for I= ',
     &          I,trim(NOS_ID(I))
              NTR_S(I)=NT_CLIM
              DO N=1,NTR_S(I)
                RTIME_S(I,N)=TIME_CLIM(N)
                S_OBS(I,N)=S_CLIM(I,N,1)    !! use SST only
              ENDDO
            ENDIF     	   
          ENDIF
        ELSEIF(TS_FLAG(I).EQ.2) THEN !! correction using climatological dataset
          NTR_S(I)=NT_CLIM
          DO N=1,NTR_S(I)
            RTIME_S(I,N)=TIME_CLIM(N)
            S_OBS(I,N)=S_CLIM(I,N,1)    !! use SST only
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

C-----------------------------------------------------------------------
C  Separate bias as mean error + time varying difference
C-----------------------------------------------------------------------
          N0=0
          DO N1=1,NTR_S(I)
            IF(S_OBS(I,N1).GT.0.0) THEN
              N0=N0+1
              ONED1(N0)= RTIME_S(I,N1)
              ONED2(N0)= S_OBS(I,N1)
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
            ONED1(N0)= TS_time(1)
            ONED2(N0)= S_OBS(I,1)
          ENDIF
          DO N1=1,NTR_S(I)
            N0=N0+1
            ONED1(N0)= RTIME_S(I,N1)
            ONED2(N0)= S_OBS(I,N1)
          ENDDO  
          write(*,*) 'N0=',N0,' NTR_S(I)=',NTR_S(I)

          DO N1=2,NTMAX
            IF((TS_time(N1).GE.TIME2)) THEN
              N0=N0+1
              ONED1(N0)= TS_time(N1)
              ONED2(N0)= S_OBS(I,NTR_S(I))
              EXIT
            ENDIF
          ENDDO
8923      CONTINUE
          write(*,*) 'N0=',N0,' NTR_S(I)=',NTR_S(I)
          NTR_S(I)=N0

          DO N1=1,NTR_S(I)
            RTIME_S(I,N1)=ONED1(N1)
            S_OBS(I,N1)=ONED2(N1)
          ENDDO     
          TIME1=RTIME_S(I,1)
          TIME2=RTIME_S(I,NTR_S(I))
          WRITE(*,*) 'TIME1=',TIME1,'TIME2=',TIME2,TS_time(1),
     &	     TS_time(NTMAX),'NTR_S=',NTR_S(I),NTMAX

          N0=0
          DO N1=1,NTMAX
            IF((TS_time(N1).GE.TIME1).AND.
     &	       (TS_time(N1).LE.TIME2)) THEN
              N0=N0+1
              ONED3(N0)=TS_time(N1)
            ENDIF
          ENDDO

          IF(N0.LT.1) THEN
            WRITE(*,*) 'WARNING MESSAGE: Real time salinity data is insufficient'
            WRITE(*,*) 'Please wait for 20 minutes and then check'
            WRITE(*,*) 'File size of ',TRIM(NOSBUFR), ' and ',TRIM(USGSBUFR)
            WRITE(*,*) 'Rerun prep job' 
            write(*,*) 'I=',I,TIME1,TIME2,TS_time(1),TS_time(NTMAX)
            STOP
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
          DO N=1,N0
            ONED4(N)=tmpp2(N)
          End do
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)

          AVG=0.0
          DO N=1,N0
            DO N1=1,NTMAX
C!              IF(ONED3(N).EQ.TS_time(N1)) THEN
              IF(ABS(ONED3(N)-TS_time(N1)).LT.1.0e-10) THEN
cc avg=Sobs - NCOM at the corresponding grid
                ONED1(N)=ONED4(N)-SALTOBC_M(GRIDID_STA(I),KBm,N1)
                AVG=AVG+ONED1(N)
                EXIT
              ENDIF	  
            ENDDO
8925        CONTINUE
          ENDDO

          IF(N0.GT.0) AVG=AVG/N0
          AVGERR_S(I)=AVG
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
          DO N=1,NTMAX
            oned1(N)=tmpp2(N)
            S_OBS(I,N)=oned1(N)
            IF(TS_time(N).GT.ONED3(N0)) S_OBS(I,N)=0.0 
          ENDDO
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
        ENDIF
      ENDDO

c-----------------------------------------------------------------------
C  Correcting OBC by the difference between obs - ETSS
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
	 
        ID1=TS_SID_1(I)
        ID2=TS_SID_2(I)
        sc1=TS_S_1(I)
        sc2=TS_S_2(I)
        IF(TS_STA(I).EQ.1) THEN
          IF(ID1.GT.0) THEN
            DO N=1,NTMAX
              DO K=1,KBm
                TEMPOBC_M(I,K,N)=TEMPOBC_M(I,K,N)+
     &	          sc1*(AVGERR_T(ID1)+T_OBS(ID1,N))
                SALTOBC_M(I,K,N)=SALTOBC_M(I,K,N)+
     &	          sc1*(AVGERR_S(ID1)+S_OBS(ID1,N))
              ENDDO
            ENDDO
          ENDIF 
        ELSEIF(TS_STA(I).EQ.2) THEN
          IF(ID1.GT.0.AND.ID2.GT.0) THEN
            DO N=1,NTMAX
              DO K=1,KBm
                TEMPOBC_M(I,K,N)=TEMPOBC_M(I,K,N)+
     &	          sc1*(AVGERR_T(ID1)+T_OBS(ID1,N))+
     &            sc2*(AVGERR_T(ID2)+T_OBS(ID2,N))
                SALTOBC_M(I,K,N)=SALTOBC_M(I,K,N)+
     &	          sc1*(AVGERR_S(ID1)+S_OBS(ID1,N))+
     &            sc2*(AVGERR_S(ID2)+S_OBS(ID2,N))
              ENDDO
            ENDDO
          ENDIF 
        ENDIF
      ENDDO 	 

C -------------------------------------------------------------------
C   Print OBC for evaluation
      CLOSE(33)
      OPEN(33,file='WL_OBC_ajdusted.dat')
      DO N=1,NTMAX_WL
        write(33,35) ZETA_TIME(N),(WLOBC(I,N),I=1,20),WLOBC(NOBC,N)
      ENDDO
      CLOSE(33)

      OPEN(33,file='TEMP_OBC_ajdusted.dat')
      DO N=1,NTMAX
        DO I=1,NOBC
          write(33,35) TS_TIME(N),(TEMPOBC_M(I,K,N),K=1,KBm)
        ENDDO
      ENDDO
      CLOSE(33)

      OPEN(33,file='SALT_OBC_ajdusted.dat')
      DO N=1,NTMAX
        DO I=1,NOBC
          DO K=1,KBm
cc Specify minimum salinity as 0.02 psu for coastal waters
            IF(SALTOBC_M(I,K,N).LE.0.02) SALTOBC_M(I,K,N)=0.02
          ENDDO
        ENDDO
      ENDDO
      DO N=1,NTMAX
        DO I=1,NOBC
          write(33,35) TS_TIME(N),(SALTOBC_M(I,K,N),K=1,KBm)
        ENDDO
      ENDDO
      CLOSE(33)
	    
C-----------------------------------------------------------------------
C  Assign to corresponding open boundary variables, and then write into a netcdf file
C-----------------------------------------------------------------------     
      IF(ALLOCATED(WLsouth)) DEALLOCATE(WLsouth)
      IF(ALLOCATED(WLnorth)) DEALLOCATE(WLnorth)
      IF(ALLOCATED(WLwest)) DEALLOCATE(WLwest)
      IF(ALLOCATED(WLeast)) DEALLOCATE(WLeast)
      ALLOCATE(WLsouth(IROMS,NTMAX_WL))
      ALLOCATE(WLnorth(IROMS,NTMAX_WL))
      ALLOCATE(WLwest(JROMS,NTMAX_WL))
      ALLOCATE(WLeast(JROMS,NTMAX_WL))

      IF(ALLOCATED(tempsouth)) DEALLOCATE(tempsouth)
      IF(ALLOCATED(tempnorth)) DEALLOCATE(tempnorth)
      IF(ALLOCATED(tempwest)) DEALLOCATE(tempwest)
      IF(ALLOCATED(tempeast)) DEALLOCATE(tempeast)
      ALLOCATE(tempsouth(IROMS,KBm,NTMAX))
      ALLOCATE(tempnorth(IROMS,KBm,NTMAX))
      ALLOCATE(tempwest(JROMS,KBm,NTMAX))
      ALLOCATE(tempeast(JROMS,KBm,NTMAX))

      IF(ALLOCATED(saltsouth)) DEALLOCATE(saltsouth)
      IF(ALLOCATED(saltnorth)) DEALLOCATE(saltnorth)
      IF(ALLOCATED(saltwest)) DEALLOCATE(saltwest)
      IF(ALLOCATED(salteast)) DEALLOCATE(salteast)
      ALLOCATE(saltsouth(IROMS,KBm,NTMAX))
      ALLOCATE(saltnorth(IROMS,KBm,NTMAX))
      ALLOCATE(saltwest(JROMS,KBm,NTMAX))
      ALLOCATE(salteast(JROMS,KBm,NTMAX))

      IF(ALLOCATED(u_south)) DEALLOCATE(u_south) 
      IF(ALLOCATED(u_north)) DEALLOCATE(u_north) 
      IF(ALLOCATED(u_west)) DEALLOCATE(u_west) 
      IF(ALLOCATED(u_east)) DEALLOCATE(u_east) 
      ALLOCATE(u_south(IROMS,KBm,NTMAX))  
      ALLOCATE(u_north(IROMS,KBm,NTMAX)) 
      ALLOCATE(u_west(JROMS,KBm,NTMAX))
      ALLOCATE(u_east(JROMS,KBm,NTMAX)) 

      IF(ALLOCATED(v_south)) DEALLOCATE(v_south)
      IF(ALLOCATED(v_north)) DEALLOCATE(v_north)
      IF(ALLOCATED(v_west)) DEALLOCATE(v_west) 
      IF(ALLOCATED(v_east)) DEALLOCATE(v_east)  
      ALLOCATE(v_south(IROMS,KBm,NTMAX)) 
      ALLOCATE(v_north(IROMS,KBm,NTMAX)) 
      ALLOCATE(v_west(JROMS,KBm,NTMAX)) 
      ALLOCATE(v_east(JROMS,KBm,NTMAX)) 

      IF(ALLOCATED(dosouth)) DEALLOCATE(dosouth)
      IF(ALLOCATED(donorth)) DEALLOCATE(donorth)
      IF(ALLOCATED(dowest)) DEALLOCATE(dowest)
      IF(ALLOCATED(doeast)) DEALLOCATE(doeast)
      ALLOCATE(dosouth(IROMS,KBm,NTMAX))
      ALLOCATE(donorth(IROMS,KBm,NTMAX))
      ALLOCATE(dowest(JROMS,KBm,NTMAX))
      ALLOCATE(doeast(JROMS,KBm,NTMAX))

      IF(ALLOCATED(ubarsouth)) DEALLOCATE(ubarsouth) 
      IF(ALLOCATED(ubarnorth)) DEALLOCATE(ubarnorth) 
      IF(ALLOCATED(ubarwest)) DEALLOCATE(ubarwest) 
      IF(ALLOCATED(ubareast)) DEALLOCATE(ubareast) 
      ALLOCATE(ubarsouth(IROMS,NTMAX))
      ALLOCATE(ubarnorth(IROMS,NTMAX))
      ALLOCATE(ubarwest(JROMS,NTMAX))
      ALLOCATE(ubareast(JROMS,NTMAX))

      IF(ALLOCATED(vbarsouth)) DEALLOCATE(vbarsouth) 
      IF(ALLOCATED(vbarnorth)) DEALLOCATE(vbarnorth) 
      IF(ALLOCATED(vbarwest)) DEALLOCATE(vbarwest) 
      IF(ALLOCATED(vbareast)) DEALLOCATE(vbareast) 
      ALLOCATE(vbarsouth(IROMS,NTMAX))
      ALLOCATE(vbarnorth(IROMS,NTMAX))
      ALLOCATE(vbarwest(JROMS,NTMAX))
      ALLOCATE(vbareast(JROMS,NTMAX))

      IF(ALLOCATED(ub_south)) DEALLOCATE(ub_south) 
      IF(ALLOCATED(ub_north)) DEALLOCATE(ub_north) 
      IF(ALLOCATED(ub_west)) DEALLOCATE(ub_west) 
      IF(ALLOCATED(ub_east)) DEALLOCATE(ub_east) 
      ALLOCATE(ub_south(IROMS,NTMAX)) 
      ALLOCATE(ub_north(IROMS,NTMAX))
      ALLOCATE(ub_west(JROMS,NTMAX)) 
      ALLOCATE(ub_east(JROMS,NTMAX)) 

      IF(ALLOCATED(vb_south)) DEALLOCATE(vb_south) 
      IF(ALLOCATED(vb_north)) DEALLOCATE(vb_north) 
      IF(ALLOCATED(vb_west)) DEALLOCATE(vb_west) 
      IF(ALLOCATED(vb_east)) DEALLOCATE(vb_east) 
      ALLOCATE(vb_south(IROMS,NTMAX)) 
      ALLOCATE(vb_north(IROMS,NTMAX))
      ALLOCATE(vb_west(JROMS,NTMAX)) 
      ALLOCATE(vb_east(JROMS,NTMAX)) 

      IF(ALLOCATED(uu_south)) DEALLOCATE(uu_south) 
      IF(ALLOCATED(uu_north)) DEALLOCATE(uu_north) 
      IF(ALLOCATED(uu_west)) DEALLOCATE(uu_west) 
      IF(ALLOCATED(uu_east)) DEALLOCATE(uu_east) 
      ALLOCATE(uu_south(IROMS-1,NTMAX)) 
      ALLOCATE(uu_north(IROMS-1,NTMAX)) 
      ALLOCATE(uu_west(JROMS,NTMAX)) 
      ALLOCATE(uu_east(JROMS,NTMAX)) 

      IF(ALLOCATED(vv_south)) DEALLOCATE(vv_south) 
      IF(ALLOCATED(vv_north)) DEALLOCATE(vv_north) 
      IF(ALLOCATED(vv_west)) DEALLOCATE(vv_west) 
      IF(ALLOCATED(vv_east)) DEALLOCATE(vv_east) 
      ALLOCATE(vv_south(IROMS,NTMAX)) 
      ALLOCATE(vv_north(IROMS,NTMAX)) 
      ALLOCATE(vv_west(JROMS-1,NTMAX)) 
      ALLOCATE(vv_east(JROMS-1,NTMAX)) 

      DO N=1,NTMAX_WL
        DO I=1,IROMS
          WLsouth(I,N)=0.0
          WLnorth(I,N)=0.0
        ENDDO
      ENDDO

      DO N=1,NTMAX
        DO I=1,IROMS
          ubarsouth(I,N)=0.0
          ubarnorth(I,N)=0.0
          vbarsouth(I,N)=0.0
          vbarnorth(I,N)=0.0
          ub_south(I,N)=0.0 
          ub_north(I,N)=0.0 
          vb_south(I,N)=0.0 
          vb_north(I,N)=0.0 
        ENDDO
      ENDDO

      DO N=1,NTMAX_WL
        DO I=1,JROMS
          WLwest(I,N)=0.0
          WLeast(I,N)=0.0
        ENDDO
      ENDDO

      DO N=1,NTMAX
        DO I=1,JROMS
          ubarwest(I,N)=0.0
          ubareast(I,N)=0.0
          vbarwest(I,N)=0.0
          vbareast(I,N)=0.0

          ub_west(I,N)=0.0 
          ub_east(I,N)=0.0 
          vb_west(I,N)=0.0 
          vb_east(I,N)=0.0 
        ENDDO
      ENDDO

      DO N=1,NTMAX
        DO I=1,IROMS
          DO K=1,KBm
            tempsouth(i,k,n)=-99999.99
            saltsouth(i,k,n)=-99999.99
            tempnorth(i,k,n)=-99999.99
            saltnorth(i,k,n)=-99999.99
            u_south(i,k,n)=-99999.99
            v_south(i,k,n)=-99999.99  
            u_north(i,k,n)=-99999.99 
            v_north(i,k,n)=-99999.99 
            dosouth(i,k,n)=-99999.99
            donorth(i,k,n)=-99999.99
          ENDDO
        ENDDO
      ENDDO

      DO N=1,NTMAX
        DO I=1,JROMS
          DO K=1,KBm
            tempwest(i,k,n)=-99999.99
            saltwest(i,k,n)=-99999.99
            tempeast(i,k,n)=-99999.99
            salteast(i,k,n)=-99999.99
            u_east(i,k,n)=-99999.99  
            v_east(i,k,n)=-99999.99  
            u_west(i,k,n)=-99999.99 
            v_west(i,k,n)=-99999.99 
            doeast(i,k,n)=-99999.99
            dowest(i,k,n)=-99999.99
          ENDDO
        ENDDO
      ENDDO

      NW_SOUTH=0
      NW_NORTH=0
      NW_WEST=0
      NW_EAST=0
      DO I=1,NOBC
        DO N=1,NTMAX
          DO K=1,KBm
            IF(SALTOBC_M(I,K,N).LE.0.02) SALTOBC_M(I,K,N)=0.02
          ENDDO
        ENDDO
      ENDDO

C   Add for the 1-Term DO simulation
C   Constants used in calculating DO saturation concentration
      A1_do = -173.4292;
      A2_do =  249.6339;
      A3_do =  143.3483;
      A4_do =  -21.8492;
      B1_do =   -0.033096;
      B2_do =    0.014259;
      B3_do =   -0.0017000;

      DO I=1,NOBC
        ele=0
        IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
        IF(ALLOCATED(zsigma)) DEALLOCATE(ZSIGMA)
        ALLOCATE(tmp1d(Kbm+1))
        ALLOCATE(ZSIGMA(Kbm+1))

        DO K=1,KBM+1
          tmp1d(K)=sigmaW(K-1)
        ENDDO        
        CALL sigma2Z_ROMS_FIX_new(tmp1d,hOBC(i),ele,KBm+1,ZSIGMA,
     &       hc,theta_s,theta_b,TCline,nvtrans,nvstr)
        IF(TRIM(OBC_ID(I)).EQ.'S') THEN
          NW_SOUTH=NW_SOUTH+1
          DO N=1,NTMAX_WL
            WLsouth(IOBC(I),N)=WLOBC(I,N)
          ENDDO
          DO N=1,NTMAX
            ubarsouth(IOBC(I),n)=0.0  
            vbarsouth(IOBC(I),n)=0.0  
            DO K=1,KBm
              tempsouth(IOBC(I),k,n)=TEMPOBC_M(I,K,N)  
              saltsouth(IOBC(I),k,n)=SALTOBC_M(I,K,N)
cc Rotated from NS and EW to XI and ETA directions
              u_south(IOBC(I),k,n)=uOBC_M(I,k,n)*
     &          cos(angm(IOBC(I),JOBC(I)))+
     &          vOBC_M(I,k,n)*sin(angm(IOBC(I),JOBC(I)))
              v_south(IOBC(I),k,n)=-uOBC_M(I,k,n)*
     &          sin(angm(IOBC(I),JOBC(I)))+
     &          vOBC_M(I,k,n)*cos(angm(IOBC(I),JOBC(I)))
              ubarsouth(IOBC(I),n)=ubarsouth(IOBC(I),n)+
     &          uOBC_M(I,K,N)*(ZSIGMA(k)-ZSIGMA(k+1))  
              vbarsouth(IOBC(I),n)=vbarsouth(IOBC(I),n)+
     &          vOBC_M(I,K,N)*(ZSIGMA(k)-ZSIGMA(k+1))  

              S_do=saltsouth(IOBC(I),k,n)
              T_do=tempsouth(IOBC(I),k,n)
              TK_do=(T_do+273.15)*1.00024
              TK100=TK_do/100.0
              C_do=A1_do+A2_do*(100.0/TK_do)+A3_do*LOG(TK100)+
     &             A4_do*TK100+S_do*(B1_do+B2_do*TK100+
     &             B3_do*(TK100*TK100))
	      dosouth(IOBC(I),k,n)=EXP(C_do)*1427.6/32.0
            ENDDO
            ubarsouth(IOBC(I),n)=ubarsouth(IOBC(I),n)/hOBC(I)
            vbarsouth(IOBC(I),n)=vbarsouth(IOBC(I),n)/hOBC(I)
          ENDDO

          do N=1,NTMAX
            ub_south(IOBC(I),n)=ubarsouth(IOBC(I),n)*
     &         cos(angm(IOBC(I),JOBC(I)))+
     &         vbarsouth(IOBC(I),n)*sin(angm(IOBC(I),JOBC(I)))
            vb_south(IOBC(I),n)=-ubarsouth(IOBC(I),n)*
     &         sin(angm(IOBC(I),JOBC(I)))+
     &         vbarsouth(IOBC(I),n)*cos(angm(IOBC(I),JOBC(I)))
          enddo

          IF(TRIM(DBASE_WL).EQ.'RTOFS') THEN
            DO N=1,NTMAX_WL
              ub_south(IOBC(I),n)=ubarOBC(I,n)*
     &           cos(angm(IOBC(I),JOBC(I)))+
     &           vbarOBC(I,n)*sin(angm(IOBC(I),JOBC(I)))
              vb_south(IOBC(I),n)=-ubarOBC(I,n)*
     &           sin(angm(IOBC(I),JOBC(I)))+
     &           vbarOBC(I,n)*cos(angm(IOBC(I),JOBC(I)))
            ENDDO
          ENDIF
 
	ELSEIF(TRIM(OBC_ID(I)).EQ.'N') THEN
          NW_NORTH=NW_NORTH+1
          DO N=1,NTMAX_WL
            WLnorth(IOBC(I),n)=WLOBC(I,N)
          ENDDO
          DO N=1,NTMAX
            ubarnorth(IOBC(I),n)=0.0 
            vbarnorth(IOBC(I),n)=0.0
  	    DO K=1,KBm   !! FOR ROMS, K=1 for bottom, K=KBm for surface
              tempnorth(IOBC(I),k,n)=TEMPOBC_M(I,K,N)
              saltnorth(IOBC(I),k,n)=SALTOBC_M(I,K,N)
              u_north(IOBC(I),k,n)=uOBC_M(I,k,n)*
     &          cos(angm(IOBC(I),JOBC(I)))+
     &          vOBC_M(I,k,n)*sin(angm(IOBC(I),JOBC(I)))
              v_north(IOBC(I),k,n)=-uOBC_M(I,k,n)*
     &          sin(angm(IOBC(I),JOBC(I)))+
     &          vOBC_M(I,k,n)*cos(angm(IOBC(I),JOBC(I)))
              ubarnorth(IOBC(I),n)=ubarnorth(IOBC(I),n)+
     &          uOBC_M(I,K,N)*(ZSIGMA(k)-ZSIGMA(k+1))
              vbarnorth(IOBC(I),n)=vbarnorth(IOBC(I),n)+
     &          vOBC_M(I,K,N)*(ZSIGMA(k)-ZSIGMA(k+1))  

	      S_do=saltnorth(IOBC(I),k,n)
              T_do=tempnorth(IOBC(I),k,n)
              TK_do=(T_do+273.15)*1.00024
	      TK100=TK_do/100.0
              C_do=A1_do+A2_do*(100.0/TK_do)+A3_do*LOG(TK100)+
     &             A4_do*TK100+S_do*(B1_do+B2_do*TK100+
     &             B3_do*(TK100*TK100))
	      donorth(IOBC(I),k,n)=EXP(C_do)*1427.6/32.0
            ENDDO
            ubarnorth(IOBC(I),n)=ubarnorth(IOBC(I),n)/hOBC(I)
            vbarnorth(IOBC(I),n)=vbarnorth(IOBC(I),n)/hOBC(I)
          ENDDO
          
cc Rotate Velocity from true-north east to ROMS grid
          do N=1,NTMAX
            ub_north(IOBC(I),n)=ubarnorth(IOBC(I),n)*
     &         cos(angm(IOBC(I),JOBC(I)))+
     &         vbarnorth(IOBC(I),n)*sin(angm(IOBC(I),JOBC(I)))
            vb_north(IOBC(I),n)=-ubarnorth(IOBC(I),n)*
     &         sin(angm(IOBC(I),JOBC(I)))+
     &         vbarnorth(IOBC(I),n)*cos(angm(IOBC(I),JOBC(I)))
          enddo
 
          IF(TRIM(DBASE_WL).EQ.'RTOFS') THEN
            DO N=1,NTMAX_WL
              ub_north(IOBC(I),n)=ubarOBC(I,n)*
     &           cos(angm(IOBC(I),JOBC(I)))+
     &           vbarOBC(I,n)*sin(angm(IOBC(I),JOBC(I)))
              vb_north(IOBC(I),n)=-ubarOBC(I,n)*
     &           sin(angm(IOBC(I),JOBC(I)))+
     &           vbarOBC(I,n)*cos(angm(IOBC(I),JOBC(I)))
            ENDDO
          ENDIF
 
	ELSEIF(TRIM(OBC_ID(I)).EQ.'W') THEN
          NW_WEST=NW_WEST+1
          DO N=1,NTMAX_WL
            WLwest(JOBC(I),N)=WLOBC(I,N)
          ENDDO
          DO N=1,NTMAX
            ubarwest(JOBC(I),n)=0.0  
            vbarwest(JOBC(I),n)=0.0  
            DO K=1,KBm
              tempwest(JOBC(I),k,n)=TEMPOBC_M(I,K,N)
              saltwest(JOBC(I),k,n)=SALTOBC_M(I,K,N)
              u_west(JOBC(I),k,n)=uOBC_M(I,k,n)*
     &          cos(angm(IOBC(I),JOBC(I)))+
     &          vOBC_M(I,k,n)*sin(angm(IOBC(I),JOBC(I)))
              v_west(JOBC(I),k,n)=-uOBC_M(I,k,n)*
     &          sin(angm(IOBC(I),JOBC(I)))+
     &          vOBC_M(I,k,n)*cos(angm(IOBC(I),JOBC(I)))
              ubarwest(JOBC(I),n)=ubarwest(JOBC(I),n)+
     *          uOBC_M(I,K,N)*(ZSIGMA(k)-ZSIGMA(k+1))  
              vbarwest(JOBC(I),n)=vbarwest(JOBC(I),n)+
     *          vOBC_M(I,K,N)*(ZSIGMA(k)-ZSIGMA(k+1))  

              S_do=saltwest(JOBC(I),k,n)
              T_do=tempwest(JOBC(I),k,n)
              TK_do=(T_do+273.15)*1.00024
              TK100=TK_do/100.0
              C_do=A1_do + A2_do*(100.0/TK_do)+A3_do*LOG(TK100)+
     &          A4_do*TK100+S_do*(B1_do+B2_do*TK100+
     &          B3_do*(TK100*TK100))
	      dowest(JOBC(I),k,n)=EXP(C_do)*1427.6/32.0
            ENDDO
            ubarwest(JOBC(I),n)=ubarwest(JOBC(I),n)/hOBC(I)
            vbarwest(JOBC(I),n)=vbarwest(JOBC(I),n)/hOBC(I)
          ENDDO

          do N=1,NTMAX
            ub_west(JOBC(I),n)=ubarwest(JOBC(I),n)*
     &         cos(angm(IOBC(I),JOBC(I)))+
     &         vbarwest(JOBC(I),n)*sin(angm(IOBC(I),JOBC(I)))
            vb_west(JOBC(I),n)=-ubarwest(JOBC(I),n)*
     &         sin(angm(IOBC(I),JOBC(I)))+
     &         vbarwest(JOBC(I),n)*cos(angm(IOBC(I),JOBC(I)))
          enddo

          IF(TRIM(DBASE_WL).EQ.'RTOFS') THEN
            DO N=1,NTMAX_WL
              ub_west(JOBC(I),n)=ubarOBC(I,n)*
     &           cos(angm(IOBC(I),JOBC(I)))+
     &           vbarOBC(I,n)*sin(angm(IOBC(I),JOBC(I)))
              vb_west(JOBC(I),n)=-ubarOBC(I,n)*
     &           sin(angm(IOBC(I),JOBC(I)))+
     &           vbarOBC(I,n)*cos(angm(IOBC(I),JOBC(I)))
            ENDDO
          ENDIF
 
	ELSEIF(TRIM(OBC_ID(I)).EQ.'E') THEN
          NW_EAST=NW_EAST+1
          DO N=1,NTMAX_WL
            WLeast(JOBC(I),n)=WLOBC(I,N)
          ENDDO
          DO N=1,NTMAX
            ubareast(JOBC(I),n)=0.0  
            vbareast(JOBC(I),n)=0.0  
            DO K=1,KBm
              tempeast(JOBC(I),k,n)=TEMPOBC_M(I,K,N)
              salteast(JOBC(I),k,n)=SALTOBC_M(I,K,N)
              u_east(JOBC(I),k,n)=uOBC_M(I,k,n)*
     &          cos(angm(IOBC(I),JOBC(I)))+
     &          vOBC_M(I,k,n)*sin(angm(IOBC(I),JOBC(I)))
              v_east(JOBC(I),k,n)=-uOBC_M(I,k,n)*
     &          sin(angm(IOBC(I),JOBC(I)))+
     &          vOBC_M(I,k,n)*cos(angm(IOBC(I),JOBC(I)))
              ubareast(JOBC(I),n)=ubareast(JOBC(I),n)+
     *          uOBC_M(I,K,N)*(ZSIGMA(k)-ZSIGMA(k+1))  
              vbareast(JOBC(I),n)=vbareast(JOBC(I),n)+
     *          vOBC_M(I,K,N)*(ZSIGMA(k)-ZSIGMA(k+1))  

              S_do=salteast(JOBC(I),k,n)
              T_do=tempeast(JOBC(I),k,n)
              TK_do=(T_do + 273.15)*1.00024
              TK100=TK_do/100.0
              C_do=A1_do+A2_do*(100.0/TK_do)+A3_do*LOG(TK100)+
     &          A4_do*TK100+S_do*(B1_do+B2_do*TK100+
     &          B3_do*(TK100*TK100))
	      doeast(JOBC(I),k,n)=EXP(C_do)*1427.6/32.0
            ENDDO
            ubareast(JOBC(I),n)=ubareast(JOBC(I),n)/hOBC(I)
            vbareast(JOBC(I),n)=vbareast(JOBC(I),n)/hOBC(I)
          ENDDO

          do N=1,NTMAX
            ub_east(JOBC(I),n)=ubareast(JOBC(I),n)*
     &         cos(angm(IOBC(I),JOBC(I)))+
     &         vbareast(JOBC(I),n)*sin(angm(IOBC(I),JOBC(I)))
            vb_east(JOBC(I),n)=-ubareast(JOBC(I),n)*
     &         sin(angm(IOBC(I),JOBC(I)))+
     &         vbareast(JOBC(I),n)*cos(angm(IOBC(I),JOBC(I)))
          enddo

          IF(TRIM(DBASE_WL).EQ.'RTOFS') THEN
            DO N=1,NTMAX_WL   
              ub_east(JOBC(I),n)=ubarOBC(I,n)*
     &           cos(angm(IOBC(I),JOBC(I)))+
     &           vbarOBC(I,n)*sin(angm(IOBC(I),JOBC(I)))
              vb_east(JOBC(I),n)=-ubarOBC(I,n)*
     &           sin(angm(IOBC(I),JOBC(I)))+
     &           vbarOBC(I,n)*cos(angm(IOBC(I),JOBC(I)))
            ENDDO
          ENDIF
        ENDIF
      ENDDO       

      do n=1,NTMAX
        do i=1,iroms-1
          uu_south(i,n)=(ub_south(i,n)+ub_south(i+1,n))/2.0
          uu_north(i,n)=(ub_north(i,n)+ub_north(i+1,n))/2.0
          if(OFS .EQ.'gomofs'.or.OFS.eq.'GoMOFS'.or.
     &       OFS.eq.'GOMOFS') then
            uu_south(i,n)=0.5*uu_south(i,n)  !!! this if block machuan /05/02/16
            uu_north(i,n)=0.5*uu_north(i,n)
          endif
        enddo 
        do i=1,iroms
          vv_south(i,n)=vb_south(i,n)
          vv_north(i,n)=vb_north(i,n)
          if(OFS.EQ.'gomofs'.or.OFS.eq.'GoMOFS'.or.
     &       OFS.eq.'GOMOFS') then
            vv_south(i,n)=0.5* vv_south(i,n)  !!! this if block machuan /05/02/16
            vv_north(i,n)= 0.5* vv_north(i,n)
          endif    
        enddo
        do j=1,jroms
          uu_east(j,n)=ub_east(j,n)
          uu_west(j,n)=ub_west(j,n)
          if(OFS.EQ.'gomofs'.or.OFS.eq.'GoMOFS'.or.
     &       OFS.eq.'GOMOFS') then
            uu_east(j,n)= 0.5 *  uu_east(j,n)    !!! this if block machuan /05/02/16
            uu_west(j,n)= 0.5 *  uu_west(j,n)
          endif
        enddo
        do j=1,jroms-1
          vv_east(j,n)=(vb_east(j,n)+vb_east(j+1,n))/2.0
          vv_west(j,n)=(vb_west(j,n)+vb_west(j+1,n))/2.0
          if(OFS.EQ.'gomofs'.or.OFS.eq.'GoMOFS'.or.
     &       OFS.eq.'GOMOFS') then
            vv_east(j,n)=0.5*vv_east(j,n)   !!!  this if block machuan /05/02/16
            vv_west(j,n)=0.5*vv_west(j,n)
          endif 
        enddo
      enddo
      write(*,*) 'NW_SOUTH= ',NW_SOUTH
      write(*,*) 'NW_NORTH= ',NW_NORTH
      write(*,*) 'NW_WEST= ',NW_WEST
      write(*,*) 'NW_EAST= ',NW_EAST

C-----------------------------------------------------------------------
C  Set global attributes string of the NetCDF
C-----------------------------------------------------------------------
1     format(I2.2,a1,I2.2,2x,I2.2,a1,i2.2,a1,I4)
      CALL DATE_AND_TIME(BIG_BEN(1),BIG_BEN(2),BIG_BEN(3),DATE_TIME)
      WRITE(CURRENT_TIME,1) DATE_TIME(5),':',DATE_TIME(6),
     &      DATE_TIME(2),'/',DATE_TIME(3),'/',DATE_TIME(1)
      globalstr(1)=trim(OCEAN_MODEL)
     &   //' lateral open boundary forcing netCDF file'
      globalstr(2)=trim(OFS)//' lateral open boundary netCDF file'
      globalstr(3)='Water level OBC from '//trim(DBASE_WL)//
     &   ' data source'
      globalstr(4)='T and S OBC from '//trim(DBASE_TS)//
     &   ' data source'
      globalstr(5)=trim(DBASE_TS)//' data file '
      if(IGRD_ORI.EQ.0) then
        globalstr(6)='On native '//trim(DBASE_TS)//
     &     ' grid, No spatial interpolation'
      elseif(IGRD_ORI.EQ.1) then
        globalstr(6)='On '//trim(OCEAN_MODEL)//
     &     ' grid, using remesh spatial interpolation'
      elseif(IGRD_ORI.EQ.2) then
        globalstr(6)='On '//trim(OCEAN_MODEL)//
     &     ' grid, using bicubic spatial interpolation'
      elseif(IGRD_ORI.EQ.3) then
        globalstr(6)='On '//trim(OCEAN_MODEL)//
     &     ' grid, using bilinear spatial interpolation'
      elseif(IGRD_ORI.EQ.4) then
        globalstr(6)='On '//trim(OCEAN_MODEL)//
     &     ' grid, using nature neighbors spatial interpolation'
      endif
      globalstr(7)='GRID file: '//trim(GRIDFILE)
      globalstr(8)='Created at time '//trim(CURRENT_TIME)
      globalstr(9)='Created by Aijun Zhang, OD/CO-OPS/NOS/NOAA'

      NT_ZETA=NTMAX_WL
      NT_TS=NTMAX
      v2d_time=-99999.9
      v3d_time=-99999.9
      ubar_west=-99999.9
      ubar_east=-99999.9
      ubar_south=-99999.9
      ubar_north=-99999.9
      vbar_west=-99999.9
      vbar_east=-99999.9
      vbar_south=-99999.9
      vbar_north=-99999.9
      zeta_west=-99999.9
      zeta_east=-99999.9
      zeta_south=-99999.9
      zeta_north=-99999.9
      temp_west=-99999.9
      temp_east=-99999.9
      temp_south=-99999.9
      temp_north=-99999.9
      salt_west=-99999.9
      salt_east=-99999.9
      salt_south=-99999.9
      salt_north=-99999.9
      u3_west=-99999.9
      u3_east=-99999.9
      u3_south=-99999.9
      u3_north=-99999.9
      v3_west=-99999.9
      v3_east=-99999.9
      v3_south=-99999.9
      v3_north=-99999.9
      do_west=-99999.9
      do_east=-99999.9
      do_south=-99999.9
      do_north=-99999.9
      
      IF(NW_SOUTH.GT.0) THEN
        zeta_south=1
        temp_south=1
        salt_south=1
        ubar_south=1
        vbar_south=1
        u3_south=1
        v3_south=1
        do_south=1
      ENDIF 	

      IF(NW_north.GT.0) THEN
        zeta_north=1
        temp_north=1
        salt_north=1
        ubar_north=1
        vbar_north=1
        u3_north=1
        v3_north=1
        do_north=1
      ENDIF 	

      IF(NW_west.GT.0) THEN
        zeta_west=1
	TEMP_west=1
	SALT_west=1
        ubar_west=1
        vbar_west=1
        u3_west=1
        v3_west=1
        do_west=1
      ENDIF 	

      IF(NW_east.GT.0) THEN
        zeta_east=1
	TEMP_east=1
	SALT_east=1
        ubar_east=1
        vbar_east=1
        u3_east=1
        v3_east=1
        do_east=1
      ENDIF

      call write_netCDF_OBC_ROMS(netcdf_file,ncid,1,ioxyg,
     &  IROMS,JROMS,KBm,NT_ZETA,NT_TS,NT_TS,NT_TS,NT_TS,
     &  base_date,zeta_time,ts_time,ts_time,ts_time,ts_time,
     &  zeta_west,zeta_east,zeta_south,zeta_north,
     &  ubar_west,ubar_east,ubar_south,ubar_north,
     &  vbar_west,vbar_east,vbar_south,vbar_north,
     &  u3_west,u3_east,u3_south,u3_north,
     &  v3_west,v3_east,v3_south,v3_north,
     &  temp_west,temp_east,temp_south,temp_north,
     &  salt_west,salt_east,salt_south,salt_north,
     &  do_west,do_east,do_south,do_north,globalstr)

cc AJ read variables at START_TIME from the previous cycle's OBC file
      write(*,*) 'Start to read previous cycle OBC file'
      read(START_TIME,'(I4,4I2)') IYRS,IMMS,IDDS,IHHS,IMNS 
      yearb=IYRS
      monthb=IMMS
      dayb=IDDS
      hourb=IHHS    
      jdays=JULIAN(yearb,monthb,dayb,hourb)
      day_start=jdays-jbase_date
      FIN=trim(OBC_FORCING_FILE_LAST)
      INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
      IF(FEXIST) THEN
        VNAME='zeta_time'
        ANAME='units'
        DO I=1,4
          DIMS(I)=1
        ENDDO
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp1d(DIMS(1)))
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
        ANAME=trim(adjustL(ANAME))
        LEN=LEN_TRIM(ANAME)
        LL=INDEX(ANAME,'minute')         
        IF(LL.GT.0) scale_time=1.0/1440.0
        LL=INDEX(ANAME,'hour')         
        IF(LL.GT.0) scale_time=1.0/24.0
        LL=INDEX(ANAME,'day')         
        IF(LL.GT.0) scale_time=1.0
        LL=INDEX(ANAME,'since')         
        IF(LL.GT.0) then
          read(ANAME(LL+6:LEN),80) IYR,IMM,IDD,IHH
          write(*,*) 'Basetime=',IYR,IMM,IDD,IHH
        else
	  write(*,*) 'There is error while reading base date'
	  stop
        endif    
        yearb=IYR
        monthb=IMM
        dayb=IDD
        hourb=IHH
        jday=JULIAN(yearb,monthb,dayb,hourb)
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO N=1,DIMS(1)
          tmp1d(N)=TMP4D(N,1,1,1)*scale_time+jday-jbase_date
        ENDDO
        N=1 
        DO while (abs(tmp1d(N)-day_start).GT.0.001)
          N=N+1
        ENDDO
        NSELECT=N

        VNAME='temp_time'
        ANAME='units'
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp1d(DIMS(1)))
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
        ANAME=trim(adjustL(ANAME))
        LEN=LEN_TRIM(ANAME)
        LL=INDEX(ANAME,'minute')         
        IF(LL.GT.0) scale_time=1.0/1440.0
        LL=INDEX(ANAME,'hour')         
        IF(LL.GT.0) scale_time=1.0/24.0
        LL=INDEX(ANAME,'day')         
        IF(LL.GT.0) scale_time=1.0
        LL=INDEX(ANAME,'since')         
        IF(LL.GT.0) then
          read(ANAME(LL+6:LEN),80) IYR,IMM,IDD,IHH
          write(*,*) 'Temp basetime=',IYR,IMM,IDD,IHH
        else
	  write(*,*) 'There is error while reading temp base date'
	  stop
        endif    
        yearb=IYR
        monthb=IMM
        dayb=IDD
        hourb=IHH
        jday=JULIAN(yearb,monthb,dayb,hourb)
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO N=1,DIMS(1)
          tmp1d(N)=TMP4D(N,1,1,1)*scale_time+jday-jbase_date
        ENDDO
        N=1 
        DO while (abs(tmp1d(N)-day_start).GT.0.001)
          N=N+1
        ENDDO
        NSELECT_T=N

        VNAME='salt_time'
        ANAME='units'
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp1d(DIMS(1)))
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
        ANAME=trim(adjustL(ANAME))
        LEN=LEN_TRIM(ANAME)
        LL=INDEX(ANAME,'minute')         
        IF(LL.GT.0) scale_time=1.0/1440.0
        LL=INDEX(ANAME,'hour')         
        IF(LL.GT.0) scale_time=1.0/24.0
        LL=INDEX(ANAME,'day')         
        IF(LL.GT.0) scale_time=1.0
        LL=INDEX(ANAME,'since')         
        IF(LL.GT.0) then
          read(ANAME(LL+6:LEN),80) IYR,IMM,IDD,IHH
          write(*,*) 'Salt basetime=',IYR,IMM,IDD,IHH
        else
	  write(*,*) 'There is error while reading salt base date'
	  stop
        endif    
        yearb=IYR
        monthb=IMM
        dayb=IDD
        hourb=IHH
        jday=JULIAN(yearb,monthb,dayb,hourb)
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO N=1,DIMS(1)
          tmp1d(N)=TMP4D(N,1,1,1)*scale_time+jday-jbase_date
        ENDDO
        N=1 
        DO while (abs(tmp1d(N)-day_start).GT.0.001)
          N=N+1
        ENDDO
        NSELECT_S=N

        VNAME='v2d_time'
        ANAME='units'
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
        IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
        IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp1d(DIMS(1)))
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
        ANAME=trim(adjustL(ANAME))
        LEN=LEN_TRIM(ANAME)
        LL=INDEX(ANAME,'minute')         
        IF(LL.GT.0) scale_time=1.0/1440.0
        LL=INDEX(ANAME,'hour')         
        IF(LL.GT.0) scale_time=1.0/24.0
        LL=INDEX(ANAME,'day')         
        IF(LL.GT.0) scale_time=1.0
        LL=INDEX(ANAME,'since')         
        IF(LL.GT.0) then
          read(ANAME(LL+6:LEN),80) IYR,IMM,IDD,IHH
          write(*,*)'V2d basetime=',IYR,IMM,IDD,IHH
        else
	  write(*,*) 'There is error while reading V2d base date'
	  stop
        endif    
        yearb=IYR
        monthb=IMM
        dayb=IDD
        hourb=IHH
        jday=JULIAN(yearb,monthb,dayb,hourb)
        CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
        DO N=1,DIMS(1)
          tmp1d(N)=TMP4D(N,1,1,1)*scale_time+jday-jbase_date
        ENDDO
        N=1 
        DO while (abs(tmp1d(N)-day_start).GT.0.001)
          N=N+1
        ENDDO
        NSELECT_V=N

        write(*,*) 'NSELECT =',NSELECT,NSELECT_T,NSELECT_S,NSELECT_V
        write(*,*) 'Time Dim = ',dims(1)
        IF(NSELECT.GT.0.AND.NSELECT.LE.DIMS(1)) THEN
          VNAME='zeta_west'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            write(*,*) 'Dims west= ',dims
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                WLWEST(I,1)=TMP4D(I,NSELECT,1,1)
              ENDDO
            ENDIF
          ENDIF

          VNAME='zeta_east'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            write(*,*) 'Dims east= ',dims
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                WLEAST(I,1)=TMP4D(I,NSELECT,1,1)
              ENDDO
            ENDIF
          ENDIF

          VNAME='zeta_north'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            write(*,*) 'Dims north= ',dims
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                WLNORTH(I,1)=TMP4D(I,NSELECT,1,1)
              ENDDO
            ENDIF
          ENDIF

          VNAME='zeta_south'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            write(*,*) 'Dims south= ',dims
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                WLSOUTH(I,1)=TMP4D(I,NSELECT,1,1)
              ENDDO
            ENDIF
          ENDIF

          VNAME='ubar_west'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                uu_WEST(I,1)=TMP4D(I,NSELECT_V,1,1)
              ENDDO
            ENDIF
          ENDIF

          VNAME='ubar_east'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                uu_EAST(I,1)=TMP4D(I,NSELECT_V,1,1)
              ENDDO
            ENDIF
          ENDIF

          VNAME='ubar_north'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                uu_NORTH(I,1)=TMP4D(I,NSELECT_V,1,1)
              ENDDO
            ENDIF
          ENDIF

          VNAME='ubar_south'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                uu_SOUTH(I,1)=TMP4D(I,NSELECT_V,1,1)
              ENDDO
            ENDIF
          ENDIF

          VNAME='vbar_west'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                vv_WEST(I,1)=TMP4D(I,NSELECT_V,1,1)
              ENDDO
            ENDIF
          ENDIF

          VNAME='vbar_east'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                vv_EAST(I,1)=TMP4D(I,NSELECT_V,1,1)
              ENDDO
            ENDIF
          ENDIF

          VNAME='vbar_north'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)) )
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                vv_NORTH(I,1)=TMP4D(I,NSELECT_V,1,1)
              ENDDO
            ENDIF
          ENDIF

          VNAME='vbar_south'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                vv_SOUTH(I,1)=TMP4D(I,NSELECT_V,1,1)
              ENDDO
            ENDIF
          ENDIF

          VNAME='temp_west'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                DO K=1,DIMS(2)
                  TEMPWEST(I,k,1)=TMP4D(I,K,NSELECT_T,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF

          VNAME='temp_east'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                DO K=1,DIMS(2)
                  TEMPEAST(I,K,1)=TMP4D(I,K,NSELECT_T,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF

          VNAME='temp_north'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                DO K=1,DIMS(2)
                  TEMPNORTH(I,K,1)=TMP4D(I,K,NSELECT_T,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF

          VNAME='temp_south'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                DO K=1,DIMS(2)
                  TEMPSOUTH(I,K,1)=TMP4D(I,K,NSELECT_T,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF

          VNAME='salt_west'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                DO K=1,DIMS(2)
                  SALTWEST(I,k,1)=TMP4D(I,K,NSELECT_S,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF

          VNAME='salt_east'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                DO K=1,DIMS(2)
                  SALTEAST(I,K,1)=TMP4D(I,K,NSELECT_S,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF

          VNAME='salt_north'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                DO K=1,DIMS(2)
                  SALTNORTH(I,K,1)=TMP4D(I,K,NSELECT_S,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF

          VNAME='salt_south'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                DO K=1,DIMS(2)
                  SALTSOUTH(I,K,1)=TMP4D(I,K,NSELECT_S,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF

          VNAME='do_west'
          ANAME='units'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                DO K=1,DIMS(2)
                  doWEST(I,k,1)=TMP4D(I,K,NSELECT_T,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF

          VNAME='do_east'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                DO K=1,DIMS(2)
                  doEAST(I,K,1)=TMP4D(I,K,NSELECT_T,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF

          VNAME='do_north'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                DO K=1,DIMS(2)
                  doNORTH(I,K,1)=TMP4D(I,K,NSELECT_T,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF

          VNAME='do_south'
          DO I=1,4
            DIMS(I)=1
          ENDDO	
          CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &         TMP4D,ATT,0)
          IF(STATUS.EQ.NF_NOERR) then
            IF(ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
            IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
            ALLOCATE(tmp1d(DIMS(1)))
            ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
            CALL READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIM,DIMS,
     &           TMP4D,ATT,1)
            IF(STATUS.EQ.NF_NOERR) then
              DO I=1,DIMS(1)
                DO K=1,DIMS(2)
                  doSOUTH(I,K,1)=TMP4D(I,K,NSELECT_T,1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
      ALLOCATE(tmp4d(IROMS,KBm,NTMAX,2))
      DO N=1,NTMAX
        DO I=1,IROMS
          DO K=1,KBm
            tmp4d(i,k,n,1)=u_south(i,k,n)
            tmp4d(i,k,n,2)=u_north(i,k,n)
          ENDDO
        ENDDO
      ENDDO
      IF(ALLOCATED(u_south)) DEALLOCATE(u_south)
      IF(ALLOCATED(u_north)) DEALLOCATE(u_north)
      ALLOCATE(u_south(IROMS-1,KBm,NTMAX))
      ALLOCATE(u_north(IROMS-1,KBm,NTMAX))
      DO N=1,NTMAX
        DO I=1,IROMS-1
          DO K=1,KBm
            if(tmp4d(i,k,n,1).gt.-9999.0.and.
     &         tmp4d(i+1,k,n,1).gt.-9999.0) then
              u_south(i,k,n)=(tmp4d(i,k,n,1)+tmp4d(i+1,k,n,1))/2.0
            elseif(tmp4d(i,k,n,1).gt.-9999.0.and.
     &             tmp4d(i+1,k,n,1).le.-9999.0) then
              u_south(i,k,n)=tmp4d(i,k,n,1)
            elseif(tmp4d(i,k,n,1).LE.-9999.0.and.
     &             tmp4d(i+1,k,n,1).gt.-9999.0) then
              u_south(i,k,n)=tmp4d(i+1,k,n,1)
            elseif(tmp4d(i,k,n,1).le.-9999.0.and.
     &              tmp4d(i+1,k,n,1).le.-9999.0) then
              u_south(i,k,n)=-99999.99
            endif

            if(tmp4d(i,k,n,2).gt.-9999.0.and.
     &         tmp4d(i+1,k,n,2).gt.-9999.0) then
              u_north(i,k,n)=(tmp4d(i,k,n,2)+tmp4d(i+1,k,n,2))/2.0
            elseif(tmp4d(i,k,n,2).gt.-9999.0.and.
     &             tmp4d(i+1,k,n,2).le.-9999.0) then
              u_north(i,k,n)=tmp4d(i,k,n,2)
            elseif(tmp4d(i,k,n,2).LE.-9999.0.and.
     &             tmp4d(i+1,k,n,2).gt.-9999.0) then
              u_north(i,k,n)=tmp4d(i+1,k,n,2)
            elseif(tmp4d(i,k,n,2).le.-9999.0.and.
     &             tmp4d(i+1,k,n,2).le.-9999.0) then
              u_north(i,k,n)=-99999.99
            endif
          ENDDO
        ENDDO
      ENDDO
 
      IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
      ALLOCATE(tmp4d(JROMS,KBm,NTMAX,2))
      DO N=1,NTMAX
        DO I=1,JROMS
          DO K=1,KBm
            tmp4d(i,k,n,1)=v_east(i,k,n)
            tmp4d(i,k,n,2)=v_west(i,k,n)
          ENDDO
        ENDDO
      ENDDO

      IF(ALLOCATED(v_east)) DEALLOCATE(v_east)
      IF(ALLOCATED(v_west)) DEALLOCATE(v_west)
      ALLOCATE(v_east(JROMS-1,KBm,NTMAX))
      ALLOCATE(v_west(JROMS-1,KBm,NTMAX))
      DO N=1,NTMAX
        DO I=1,JROMS-1
          DO K=1,KBm
            if(tmp4d(i,k,n,1).gt.-9999.0.and.
     &         tmp4d(i+1,k,n,1).gt.-9999.0) then
              v_east(i,k,n)=(tmp4d(i,k,n,1)+tmp4d(i+1,k,n,1))/2.0
            elseif(tmp4d(i,k,n,1).gt.-9999.0.and.
     &             tmp4d(i+1,k,n,1).le.-9999.0) then
              v_east(i,k,n)=tmp4d(i,k,n,1)
            elseif(tmp4d(i,k,n,1).LE.-9999.0.and.
     &             tmp4d(i+1,k,n,1).gt.-9999.0) then
              v_east(i,k,n)=tmp4d(i+1,k,n,1)
            elseif(tmp4d(i,k,n,1).le.-9999.0.and.
     &             tmp4d(i+1,k,n,1).le.-9999.0) then
              v_east(i,k,n)=-99999.99
            endif

            if(tmp4d(i,k,n,2).gt.-9999.0.and.
     &         tmp4d(i+1,k,n,2).gt.-9999.0) then
              v_west(i,k,n)=(tmp4d(i,k,n,2)+tmp4d(i+1,k,n,2) )/2.0
            elseif(tmp4d(i,k,n,2).gt.-9999.0.and.
     &             tmp4d(i+1,k,n,2).le.-9999.0) then
              v_west(i,k,n)=tmp4d(i,k,n,2)
            elseif(tmp4d(i,k,n,2).LE.-9999.0.and.
     &             tmp4d(i+1,k,n,2).gt.-9999.0) then
              v_west(i,k,n)=tmp4d(i+1,k,n,2)
            elseif(tmp4d(i,k,n,2).le.-9999.0.and.
     &             tmp4d(i+1,k,n,2).le.-9999.0) then
              v_west(i,k,n)=-99999.99
            endif
          ENDDO
        ENDDO
      ENDDO

      call write_netCDF_OBC_ROMS(netcdf_file,ncid,2,ioxyg,
     &  IROMS,JROMS,KBm,NT_ZETA,NT_TS,NT_TS,NT_TS,NT_TS,
     &  base_date,zeta_time,ts_time,ts_time,ts_time,ts_time,
     &  WLwest,WLeast,WLsouth,WLnorth,
     &  uu_west,uu_east,uu_south,uu_north,
     &  vv_west,vv_east,vv_south,vv_north,
     &  u_west,u_east,u_south,u_north,
     &  v_west,v_east,v_south,v_north,
     &  tempwest,tempeast,tempsouth,tempnorth,
     &  saltwest,salteast,saltsouth,saltnorth,
     &  dowest,doeast,dosouth,donorth,globalstr)

      call write_netCDF_OBC_ROMS(netcdf_file,ncid,3,ioxyg,
     &  IROMS,JROMS,KBm,NT_ZETA,NT_TS,NT_TS,NT_TS,NT_TS,
     &  base_date,zeta_time,ts_time,ts_time,ts_time,ts_time,
     &  zeta_west,zeta_east,zeta_south,zeta_north,
     &  uu_west,uu_east,uu_south,uu_north,
     &  vv_west,vv_east,vv_south,vv_north,
     &  u_west,u_east,u_south,u_north,
     &  v_west,v_east,v_south,v_north,
     &  temp_west,temp_east,temp_south,temp_north,
     &  salt_west,salt_east,salt_south,salt_north,
     &  do_west,do_east,do_south,do_north,globalstr)

      write(*,*) 'OBC Forcing file is COMPLETED SUCCESSFULLY'
      WRITE(ICORMS,'(a)') 'END SECTION OF GENERATING OBC FILE'
      CLOSE(ICORMS)
      
      STOP
      END
     
      
      SUBROUTINE READ_NETCDF(STATUS,FIN,VNAME,ANAME,NDIMS,DIMS,
     &  TMP4D,ATT,MODE)
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
      character*200 FIN,VNAME,ANAME,BUFFER
      INTEGER DIMS(4),MODE,dimids(5),COUNT(4),STATUS
      REAL TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4))
      LOGICAL FEXIST
      integer, allocatable :: ITMP4D(:,:,:,:)
      REAL*8, allocatable :: DTMP4D(:,:,:,:)

      STATUS=0
      IF(MODE.EQ.0) THEN
        DO I=1,4
          DIMS(I)=1
        ENDDO
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) 'File '//trim(FIN)//' does not exist'
          STATUS=-99
        ELSE	
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop  !!'Open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Required variable ',trim(VNAME), 
     &        ' is not found'
            ITMP=STATUS
            STATUS=NF_CLOSE(NCID)
            STATUS=ITMP
            RETURN
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
          STATUS=-99
        ELSE	
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop   !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            write(*,*) 'Required variable ',trim(VNAME),
     &        ' is not found'
            ITMP=STATUS
            STATUS=NF_CLOSE(NCID)
            STATUS=ITMP
            RETURN
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
          STATUS=-99
        ELSE	
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop   !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            write(*,*) 'Required variable ',trim(VNAME), 
     &        ' is not found'
            ITMP=STATUS
            STATUS=NF_CLOSE(NCID)
            STATUS=ITMP
            RETURN
          ENDIF  

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
          STATUS=-99
        ELSE	
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop   !!'open netCDF file failed'
          ENDIF  
 
          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            write(*,*) 'Required variable ',trim(VNAME), 
     &        ' is not found'
            ITMP=STATUS
            STATUS=NF_CLOSE(NCID)
            STATUS=ITMP
            RETURN
          ENDIF  

          STATUS=NF_GET_VAR_TEXT(NCID,IDVAR,BUFFER)
          VNAME=TRIM(BUFFER)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.4) THEN
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) trim(FIN)//' does not exist'
          STATUS=-99
        ELSE	
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop   !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            write(*,*) 'Required variable ',trim(VNAME),
     &        ' is not found'
            ITMP=STATUS
            STATUS=NF_CLOSE(NCID)
            STATUS=ITMP
            RETURN
          ENDIF  
          STATUS=NF_GET_ATT_REAL(NCID,IDVAR,TRIM(ANAME),ATT)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.5) THEN
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) trim(FIN)//' does not exist'
          STATUS=-99
        ELSE	
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop   !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            write(*,*) 'Required variable ',trim(VNAME),
     &        ' is not found'
            ITMP=STATUS
            STATUS=NF_CLOSE(NCID)
            STATUS=ITMP
            RETURN
          ENDIF  
          STATUS=NF_GET_ATT_INT(NCID,IDVAR,TRIM(ANAME),IATT)
          ATT=IATT
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.6) THEN
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) trim(FIN)//' does not exist'
          STATUS=-99
        ELSE	
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop   !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            write(*,*) 'Required variable ',trim(VNAME),
     &        ' is not found'
            ITMP=STATUS
            STATUS=NF_CLOSE(NCID)
            STATUS=ITMP
            RETURN
          ENDIF  
          STATUS=NF_GET_ATT_TEXT(NCID,IDVAR,TRIM(ANAME),BUFFER)
          ANAME=TRIM(BUFFER)
          STATUS=NF_CLOSE(NCID)
        ENDIF
      ELSEIF(MODE.EQ.7) THEN
        allocate(DTMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
        IF(.NOT.FEXIST) THEN
          write(*,*) trim(FIN)//' does not exist'
          STATUS=-99
        ELSE	
          STATUS=NF_OPEN(trim(FIN),NF_NOWRITE,NCID)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            stop   !!'open netCDF file failed'
          ENDIF  

          STATUS=NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
          IF(STATUS.NE.NF_NOERR) then
            write(*,*) 'Error message= ',status
            write(*,*) 'Required variable ',trim(VNAME), 
     &        ' is not found'
            ITMP=STATUS
            STATUS=NF_CLOSE(NCID)
            STATUS=ITMP
            RETURN
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

      subroutine search_output(im,jm,lon_rtf,lat_rtf,nn,
     &	         lon_ob,lat_ob,nselect,II,JJ)
      parameter(NMAX=90000)

      logical found,log1,log2,log3,log4
      dimension mask(im,jm),II(NMAX),JJ(NMAX)
      real*4 lon_rtf(im,jm),lat_rtf(im,jm)
      real*4 lon_ob(nn),lat_ob(nn)

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
                    if(i0.ge.1.and.i0.le.im.and.
     &                 j0.ge.1.and.j0.le.jm) then
                      if(mask(i0,j0).ne.1) then
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
      write(*,*) 'OFS OB #:',nn,'  Selected RTOFS #:',nselect
	
      return
      end

