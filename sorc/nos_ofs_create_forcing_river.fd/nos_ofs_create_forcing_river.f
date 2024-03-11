C-----------------------------------------------------------------------------
C Fortran Program Name:  nos_ofs_create_forcing_river.f
C
C Directory: NOSOFS_path/sorc/nos_ofs_create_forcing_river.fd 
C
c Technical Contact:  Aijun Zhang         Org:  NOS/CO-OPS
c                     Phone: (240)533-0591  
C                     E-Mail: aijun.zhang@noaa.gov
c
c Abstract:
c   This program is used to generate river forcing file for NOS OFS 
c   from USGS & NOS real time river observations from BUFR data files
C   located in the NCEP/NCO 'data tank'.  The Fortran program relies
c   on NCO BUFRLIB software. The NetCDF river forcing file for ROMS
c   is generated.  The Bufr river files in the given time period is 
c   read in and decoded, missing variables are filled with a value
c   of -99.99.  The river climatological data (multiple-year daily
C   mean from USGS) are used in the cases of either no real-time 
C   observed data available in the time period or the River_flag in 
C   the river control file is zero.  
C
C Attributes:
C  Language:  Fortran
C  Computer:  NCEP Central Computer Systems (CCS)CIRRUS/STRATUS 
C
C  Compile command: see makefile: Make_nos_ofs_create_forcing_river
C
C Input Data files:
C
C Output files: ${OFS}.river.$YYYY$MM$DD.t${HH}z.nc
C
C Definitions and Units of Selected Variables
C   BUFR MNEMONICS
C     CLAT = Latitude (Coarse Accuracy), degrees
C     CLON = Longitude (Coarse Accuracy), degrees
C     DAYS = Day
C     DCHG = River Discharge, m3/s
C     DDDC = USGS Discharge Sensor Type, numeric
C     DDPC = USGS Precipitation Sensor Type, numeric
C     DDRS = USGS River Stage Height Sensor Type, numeric
C     DDWD = USGS Wind Direction Sensor Type, numeric
C     DDWS = USGS Wind Speed Sensor Type, numeric
C     HOUR = Hour
C     MINU = Minute
C     MNTH = Month
C     RSHM = River Stage Height (Gage Height, meters
C     RSH29= River Stage Height (Elev.) Above NGVD 1929, meters
C     SALN = Salinity, parts per thousand
C     SELV = Height of Station, meters
C     SST1 = Sea Temperature, Kelvin
C     STRV = Stream Velocity, m/s
c     TMDB = Air Temperature/Dry Bulb Temperature, Kelvin
C     TOPC = Total Precipitation/Total Water Equivalent
C     YEAR = Year
C     WACN = Specific Conductance, siemens/meter
C     WDIR = Wind Direction, degrees true
C     WSPD = Wind Speed, m/s
C  History:
C    Modified by Zheng LY on 10/30/2018
C    Modified by Zheng to include NWM data
C---------------------------------------------------------------------
      program nos_ofs_create_forcing_river
      parameter (NTMAX=100000)
      include 'netcdf.inc'
      character*7  TXT_TMP
      character*10 BIG_BEN(3)
      character*20 DBASE_WL,CURRENT_TIME
      character*120 globalstr(9)
      character*200 OFS,DBASE,OCEAN_MODEL,CMD
      character*200 BUFFER,HC_FILE,FOUT,GRIDFILE,netcdf_file
      character*200 RIVER_CTL_FILE,BUFRFILE,FIXofs,RIVER_CLIMFILE
      CHARACTER*200 START_TIME,END_TIME,NOSBUFR,USGSBUFR,PREFIXNOS      
      character*200 CORMSLOG,BUFFER1,VNAME,COMUSGS,COMPORTS,COMOUT00
      real*8 jdays,jdaye,jbase_date,JULIAN,yearb,monthb,dayb,hourb
      real*8 jday,jday0,js_etss,je_etss
      real minlon,minlat,maxlat,maxlon
      LOGICAL FEXIST
      INTEGER DATE_TIME(8)
      INTEGER BASE_DATE(4)
      INTEGER DAYS_PER_MONTH(12)
      INTEGER NWMUSED
      DATA (DAYS_PER_MONTH(i),I=1,12) /
     .  31,28,31,30,31,30,31,31,30,31,30,31/ 

cc allocatable arrays for RIVER 
      integer, allocatable :: riverID(:)
      character*20,allocatable :: USGS_ID(:)
      character*5,allocatable :: NWS_ID(:)
      character*20,allocatable :: AGENCY_ID(:)
      real, allocatable ::Q_min(:)
      real, allocatable ::Q_max(:)
      real, allocatable :: Q_mean(:)
      real, allocatable ::T_min(:)
      real, allocatable ::T_max(:)
      real, allocatable :: T_mean(:)
      integer, allocatable :: Q_CTL(:)
      integer, allocatable :: T_CTL(:)
      character,allocatable :: stationID(:,:)
      character*46,allocatable :: river_names(:)
      character*26,allocatable :: Times(:)
      integer, allocatable :: Itime(:)
      integer, allocatable :: Itime2(:)      
      integer, allocatable :: river  (:)
      integer, allocatable :: river_Xposition  (:)
      integer, allocatable :: river_Eposition  (:)
      integer, allocatable :: river_direction  (:)
      integer, allocatable :: river_flag(:)
      real, allocatable :: river_sign(:)
      real, allocatable :: river_Vshape(:,:)
      integer, allocatable :: riverID_Q(:)
      integer, allocatable :: riverID_T(:)
      real, allocatable :: Q_Scale(:)
      real, allocatable :: T_Scale(:)

      integer, allocatable :: NTR(:)
      integer, allocatable :: NTR_Q(:)
      integer, allocatable :: NTR_T(:)
      integer, allocatable :: NTR_S(:)
      integer, allocatable :: NTR_H(:)
      integer, allocatable :: NTR_F(:)        
      real, allocatable :: RTIME(:)
      real, allocatable :: RTIME_Q(:,:)
      real, allocatable :: RTIME_T(:,:)
      real, allocatable :: RTIME_S(:,:)
      real, allocatable :: RTIME_H(:,:)
      real, allocatable :: RTIME_F(:,:)  

c  Added by Zheng to include NWM output
      real, allocatable :: NWMTIME(:)
      real, allocatable :: NWM_Q(:,:)
      integer, allocatable :: IUS(:)
c  End of added by Zheng

      real, allocatable :: river_q(:,:)
      real, allocatable :: river_t(:,:)
      real, allocatable :: river_s(:,:)
      real, allocatable :: river_p(:,:)
      real, allocatable :: river_h(:,:)
      real, allocatable :: river_f(:,:)   
      real, allocatable :: river_time(:)
      real, allocatable :: river_transport(:,:)
      real, allocatable :: river_temp(:,:,:)
      real, allocatable :: river_salt(:,:,:)
      real, allocatable :: river_pass(:,:,:)
      real, allocatable :: river_diso(:,:,:)

      real oned1(NTMAX),oned2(NTMAX),oned3(NTMAX)
      INTEGER Ioned1(NTMAX),Ioned2(NTMAX)
      real, allocatable :: rtime_clim(:)
      real, allocatable :: river_q_clim(:,:)
      real, allocatable :: river_t_clim(:,:)
      real, allocatable :: river_s_clim(:,:)
      real, allocatable :: river_p_clim(:,:)
      real, allocatable :: tmp1d1(:)
      real, allocatable :: tmp1d2(:)
      real, allocatable :: tmp2d1(:,:)
      real, allocatable :: tmp2d2(:,:)
      real, allocatable :: tmp2d3(:,:)
      real, allocatable :: tmp3d(:,:,:)
      integer dimids(5),COUNT(4),DIMS(4)
      DIMENSION rating_h(1000),rating_flow(1000) 
      real, allocatable :: tmpp1(:),tmpp2(:),tmpp3(:),tmpp4(:)	
      real dchgd

C -----------------------------------------------------------------------------------
C  Declarations
      CHARACTER*8  SUBSET,name                                   
      character*16 stnbufrid
      Real*8 DATES(5),RTIM(6),xlocat(5),data3(5)
      Real*8 data1(2,500),data2(4,500)
      integer two,three
      DATA LUNIN /11/                                                   
      DATA BMISS /10E10/                                                
      data two/2/,three/3/
      equivalence(name,rpid)

C -------read variables in Fortran Control file Fortran_river.ctl------
      read(5,'(a120)') OFS
      read(5,'(a10)') OCEAN_MODEL
      read(5,'(a120)') BUFFER
      START_TIME=trim(adjustL(BUFFER))
      read(START_TIME,'(I4,4I2)') IYRS,IMMS,IDDS,IHHS,IMNS
      read(5,'(a120)') BUFFER
      END_TIME=trim(adjustL(BUFFER))
      read(END_TIME,'(I4,4I2)') IYRE,IMME,IDDE,IHHE,IMNE
      read(5,'(a120)') BUFFER
      do I=1,len_trim(BUFFER)
        if(BUFFER(I:I) .eq. "'" .or. BUFFER(I:I) .eq. '"') then
	  BUFFER(I:I)=' '
        endif    
      enddo
      GRIDFILE=trim(adjustL(BUFFER))
      write(*,*) 'gridfile=',trim(gridfile)

      read(5,'(a120)') BUFFER
      do I=1,len_trim(BUFFER)
        if(BUFFER(I:I) .eq. "'" .or. BUFFER(I:I) .eq. '"') then
          BUFFER(I:I)=' '
        endif    
      enddo
      COMUSGS=trim(adjustL(BUFFER))

      read(5,'(a120)') BUFFER
      do I=1,len_trim(BUFFER)
        if(BUFFER(I:I) .eq. "'" .or. BUFFER(I:I) .eq. '"') then
          BUFFER(I:I)=' '
        endif
      enddo
      COMPORTS=trim(adjustL(BUFFER))

      read(5,'(a120)')BUFFER
      do I=1,len_trim(BUFFER)
        if(BUFFER(I:I) .eq. "'" .or. BUFFER(I:I) .eq. '"') then
          BUFFER(I:I)=' '
        endif    
      enddo
      NOSBUFR=trim(adjustL(BUFFER))

      read(5,'(a120)') BUFFER
      do I=1,len_trim(BUFFER)
        if(BUFFER(I:I) .eq. "'" .or. BUFFER(I:I) .eq. '"') then
          BUFFER(I:I)=' '
        endif    
      enddo
      USGSBUFR=trim(adjustL(BUFFER))
      
      read(5,'(a120)') BUFFER
      do I=1,len_trim(BUFFER)
        if(BUFFER(I:I) .eq. "'" .or. BUFFER(I:I) .eq. '"') then
          BUFFER(I:I)=' '
        endif    
      enddo
      FIXofs=trim(adjustL(BUFFER))

      read(5,'(a120)') BUFFER
      do I=1,len_trim(BUFFER)
        if(BUFFER(I:I) .eq. "'" .or. BUFFER(I:I) .eq. '"') then
          BUFFER(I:I)=' '
        endif    
      enddo
      RIVER_CTL_FILE=trim(adjustL(BUFFER))

      read(5,'(a120)') BUFFER
      do I=1,len_trim(BUFFER)
        if(BUFFER(I:I) .eq. "'" .or. BUFFER(I:I) .eq. '"') then
          BUFFER(I:I)=' '
        endif
      enddo
      RIVER_CLIMFILE=trim(adjustL(BUFFER))

      read(5,'(a120)') BUFFER
      do I=1,len_trim(BUFFER)
        if(BUFFER(I:I) .eq. "'" .or. BUFFER(I:I) .eq. '"') then
          BUFFER(I:I)=' '
        endif    
      enddo
      netcdf_file=trim(adjustL(BUFFER))

      read(5,'(a120)') BUFFER
      do I=1,len_trim(BUFFER)
        if(BUFFER(I:I) .eq. "'" .or. BUFFER(I:I) .eq. '"') then
          BUFFER(I:I)=' '
        endif    
      enddo
      BUFFER=trim(adjustL(BUFFER))

      read(BUFFER,'(I4,3I2)') base_date
      read(5,*) KBm
      read(5,*) ioxyg

      read(5,'(a120)') BUFFER
      do I=1,len_trim(BUFFER)
        if(BUFFER(I:I) .eq. "'" .or. BUFFER(I:I) .eq. '"') then
          BUFFER(I:I)=' '
        endif    
      enddo
      CORMSLOG=trim(adjustL(BUFFER))

      read(5,'(a120)') BUFFER
      do I=1,len_trim(BUFFER)
        if(BUFFER(I:I) .eq. "'" .or. BUFFER(I:I) .eq. '"') then
          BUFFER(I:I)=' '
        endif
      enddo
      COMOUT00=trim(adjustL(BUFFER))
      read(5,'(a200)')BUFFER
      PREFIXNOS=trim(adjustL(BUFFER))
C   WRITE OUT INPUT PARAMETERS
      WRITE(*,*) 'OFS= ',TRIM(OFS)
      WRITE(*,*) 'OCEAN_MODEL= ',TRIM(OCEAN_MODEL)
      WRITE(*,*) 'START TIME= ',IYRS,IMMS,IDDS,IHHS
      WRITE(*,*) 'END TIME= ',IYRE,IMME,IDDE,IHHE
      WRITE(*,*) 'MODEL GRID FILE= ',TRIM(GRIDFILE)
      WRITE(*,*) 'DIRECTORY OF CONTROL FILE: ',trim(FIXofs) 
      WRITE(*,*) 'RIVER CONTROL FILE= ',TRIM(RIVER_CTL_FILE)
      WRITE(*,*) 'RIVER CLIMATOLOGIC FILE: '//TRIM(RIVER_CLIMFILE)
      WRITE(*,*) 'OUTPUT RIVER FORCING FILE IS ',trim(netcdf_file) 
      WRITE(*,*) 'BASE DATE = ',base_date
      WRITE(*,*) 'MODEL VERTICAL LAYERS = ',KBm
      WRITE(*,*) 'CORMSLOG= ',trim(CORMSLOG)

C ------End of reading Fortran Control file Fortran_river.ctl-----------
C ------Compute julian date of start and end time ----------------------
      yearb=dble(base_date(1))
      monthb=dble(base_date(2))
      dayb=dble(base_date(3))
      hourb=dble(base_date(4))
      jbase_date=JULIAN(yearb,monthb,dayb,hourb)

      yearb=dble(IYRS)
      monthb=dble(IMMS)
      dayb=dble(IDDS)
      hourb=dble(IHHS)
      jdays=JULIAN(yearb,monthb,dayb,hourb)
      day_start=real(jdays-jbase_date)

      yearb=dble(IYRE)
      monthb=dble(IMME)
      dayb=dble(IDDE)
      hourb=dble(IHHE)
      jdaye=JULIAN(yearb,monthb,dayb,hourb)
      day_end=real(jdaye-jbase_date)

C --------- read in ROMS model grid information
      ICORMS=43
      CLOSE(ICORMS)
      INQUIRE(FILE=trim(CORMSLOG),EXIST=FEXIST)
      IF(.NOT.FEXIST) THEN
        WRITE(*,*) 'CORMS FLAG LOG FILE NAME IS: '//TRIM(CORMSLOG)
        WRITE(*,*) 'CORMS FLAG LOG FILE IS NOT FOUND'
        WRITE(*,*) 'PLEASE CHECK THE LOCATION OF THIS FILE '
        WRITE(*,*) 'STOP IN nos_ofs_create_forcing_river.f'
        STOP
      ENDIF

      OPEN(ICORMS,FILE=trim(CORMSLOG),STATUS='OLD',POSITION='APPEND')
      WRITE(ICORMS,'(a)') 'BEGIN SECTION OF GENERATING RIVER FILE' 

      INQUIRE(FILE=trim(GRIDFILE),EXIST=FEXIST)
      IF(.NOT.FEXIST) THEN
        WRITE(*,*) 'MODEL GRID FILE NAME IS: '//TRIM(GRIDFILE)
        WRITE(*,*) 'MODEL GRID FILE IS NOT FOUND'
        WRITE(*,*) 'PLEASE CHECK THE LOCATION OF THIS FILE '
        WRITE(*,*) 'STOP IN nos_ofs_create_forcing_river.f'
        STOP
      ENDIF

      IF(TRIM(OCEAN_MODEL).EQ."ROMS") THEN
        WRITE(*,*) 'Reading ROMS grid file: '//trim(GRIDFILE)
        iret=NF_OPEN(trim(GRIDFILE),NF_NOWRITE, NCID_GRD)
        IF(iret.NE.NF_NOERR) then
	  write(*,*) 'Error message= ',iret
	  stop
        ENDIF  
        iret=NF_INQ(NCID_GRD,NDIMS,NVARS,NGATTS,IDUNLIMDIM)
        call check_err(iret)
        DO I=1,NDIMS
          iret=NF_INQ_DIM(NCID_GRD,I,BUFFER,ILATID)
          call check_err(iret)
          iret=NF_INQ_DIMLEN(NCID_GRD,I,latid)
          call check_err(iret)
          if(trim(BUFFER).eq.'eta_rho') JROMS=latid
          if(trim(BUFFER).eq.'xi_rho') IROMS=latid
          if(trim(BUFFER).eq.'s_rho') KBm=latid
        ENDDO
        iret=NF_CLOSE(NCID_GRD)
        call check_err(iret)
      ENDIF

C ------Read variables from river control file to define river attributes-----
      WRITE(*,*) 'READING River CTL file of: '//TRIM(RIVER_CTL_FILE)
      INQUIRE(FILE=trim(RIVER_CTL_FILE),EXIST=FEXIST)
      IF(.NOT.FEXIST) THEN
        WRITE(*,*) 'RIVER CONTROl FILE NAME IS: '//
     .    TRIM(RIVER_CTL_FILE)
        WRITE(*,*) 'RIVER CONTROL FILE IS NOT FOUND'
        WRITE(*,*) 'PLEASE CHECK THE LOCATION OF THIS FILE '
        WRITE(*,*) 'STOP IN nos_ofs_create_forcing_river.f'
        STOP
      ENDIF

      OPEN(9,file=TRIM(adjustL(RIVER_CTL_FILE)))
      DO
        READ(9,*) BUFFER
        BUFFER=trim(adjustL(BUFFER))
        LL=len_trim(BUFFER)
        IF(LL.GT.0) EXIT 
      ENDDO
      READ(9,*) NIJ,NRIVERS,DELT

cc Allocate arrays for RIVER 
      ALLOCATE(riverID(NRIVERS))
      ALLOCATE(USGS_ID(NRIVERS))
      ALLOCATE(NWS_ID(NRIVERS))
      ALLOCATE(AGENCY_ID(NRIVERS))
      ALLOCATE(Q_min(NRIVERS))
      ALLOCATE(Q_max(NRIVERS))
      ALLOCATE(Q_mean(NRIVERS))
      ALLOCATE(T_min(NRIVERS))
      ALLOCATE(T_max(NRIVERS))
      ALLOCATE(T_mean(NRIVERS))
      ALLOCATE(Q_CTL(NRIVERS))
      ALLOCATE(T_CTL(NRIVERS))

      ALLOCATE(RTIME(NTMAX))
      ALLOCATE(RTIME_Q(NRIVERS,NTMAX))
      ALLOCATE(RTIME_T(NRIVERS,NTMAX))
      ALLOCATE(RTIME_S(NRIVERS,NTMAX))
      ALLOCATE(RTIME_H(NRIVERS,NTMAX))
      ALLOCATE(RTIME_F(NRIVERS,NTMAX))  

c  Added by Zheng to include NWM output
      IF(ALLOCATED(NWMTIME)) DEALLOCATE(NWMTIME)
      ALLOCATE(NWMTIME(NTMAX)); NWMTIME=0.0
      IF(ALLOCATED(NWM_Q)) DEALLOCATE(NWM_Q)
      ALLOCATE(NWM_Q(NRIVERS,NTMAX)); NWM_Q=0.0
      IF(ALLOCATED(IUS)) DEALLOCATE(IUS)
      ALLOCATE(IUS(NRIVERS)); IUS=1
      write(*,*) 'Begin to collect NWM data!'
      CALL nos_ofs_nwm_river(base_date,nrivers,numnwm,ius,
     .  nwmtime,nwm_q,PREFIXNOS)
      write(*,*) 'The available NWM output: ',numnwm

      nwmused=0
      if(numnwm.gt.2) then
        write(*,*) 'NWM data start time: ',nwmtime(1),day_start
        write(*,*) 'NWM data end time: ',nwmtime(numnwm),day_end
        if(nwmtime(1).lt.real(day_start).and.
     .     nwmtime(numnwm).gt.real(day_end)) then
          nwmused=1
        end if
      end if
      write(*,*) 'nwmused=',nwmused
c  End of added by Zheng 

      ALLOCATE(NTR(NRIVERS))
      ALLOCATE(NTR_Q(NRIVERS))
      ALLOCATE(NTR_T(NRIVERS))
      ALLOCATE(NTR_S(NRIVERS))
      ALLOCATE(NTR_H(NRIVERS))
      ALLOCATE(NTR_f(NRIVERS))        
      ALLOCATE(river_q(NRIVERS,NTMAX))
      ALLOCATE(river_t(NRIVERS,NTMAX))
      ALLOCATE(river_s(NRIVERS,NTMAX))
      ALLOCATE(river_p(NRIVERS,NTMAX))
      ALLOCATE(river_h(NRIVERS,NTMAX))
      ALLOCATE(river_f(NRIVERS,NTMAX)) 
      ALLOCATE(river_names(NIJ))
      ALLOCATE(river(NIJ))
      ALLOCATE(river_Xposition(NIJ))
      ALLOCATE(river_Eposition(NIJ))
      ALLOCATE(river_direction(NIJ))
      ALLOCATE(river_flag(NIJ))
      ALLOCATE(river_sign(NIJ))
      ALLOCATE(riverID_Q(NIJ))
      ALLOCATE(Q_Scale(NIJ))
      ALLOCATE(riverID_T(NIJ))
      ALLOCATE(T_Scale(NIJ))
      ALLOCATE(river_Vshape(NIJ,KBm))

      ALLOCATE(river_time(NTMAX))
      ALLOCATE(Times(NTMAX))
      ALLOCATE(Itime(NTMAX))
      ALLOCATE(Itime2(NTMAX))
      ALLOCATE(river_transport(NIJ,NTMAX))
      ALLOCATE(river_temp(NIJ,KBm,NTMAX))
      ALLOCATE(river_salt(NIJ,KBm,NTMAX))
      ALLOCATE(river_pass(NIJ,KBm,NTMAX))
      ALLOCATE(river_diso(NIJ,KBm,NTMAX))
      ALLOCATE(tmp2d1(NIJ,NTMAX))
      ALLOCATE(tmp2d2(NIJ,NTMAX))

      DO
        READ(9,*) BUFFER
        BUFFER=trim(adjustL(BUFFER))
        LL=len_trim(BUFFER)
        IF(LL.GT.0) EXIT
      ENDDO
      
      DO I=1,NRIVERS
        READ(9,*) riverID(I),USGS_ID(I),NWS_ID(I),AGENCY_ID(I),
     .    Q_min(i),Q_max(I),Q_mean(I),T_min(i),T_max(I),T_mean(I),
     .    Q_CTL(I),T_CTL(I) 
      ENDDO

C  Cut off NWM data to maximum and minimum river discharge rate
      DO I=1,NRIVERS
        DO J=1,NUMNWM
          IF(NWM_Q(I,J).GT.Q_MAX(I)) NWM_Q(I,J)=Q_MAX(I)
          IF(NWM_Q(I,J).LT.Q_MIN(I)) NWM_Q(I,J)=Q_MIN(I)
        END DO
      END DO

C---- Get ride of blank lines -----------
      DO      
        READ(9,*) BUFFER
        BUFFER=trim(adjustL(BUFFER))
        LL=len_trim(BUFFER)
        IF(LL.GT.0) EXIT
      ENDDO

      READ(9,*) BUFFER
      DO I=1,NIJ
        READ(9,*) river(I),river_Xposition(I),river_Eposition(I),
     .	  river_direction(I),river_flag(I),RiverID_Q(I),Q_Scale(I),
     .    RiverID_T(I),T_scale(I),river_names(I)
        WRITE(*,*) river(I),river_Xposition(I),river_Eposition(I),
     .	  river_direction(I),river_flag(I),river_names(I)
        river_names(I)=USGS_ID(RiverID_Q(I))
      ENDDO

      DO I=1,NRIVERS
        NTR(I)=0
        NTR_Q(I)=0
        NTR_T(I)=0
        NTR_S(I)=0
        NTR_H(I)=0
        NTR_F(I)=0          
        DO N=1,NTMAX
	  RTIME(N)=-99.99
          RTIME_Q(i,N)=-99.99
          RTIME_T(i,N)=-99.99
          RTIME_S(i,N)=-99.99
          RTIME_H(i,N)=-99.99
	  river_q(i,N)=-99.99
	  river_t(I,N)=-99.99
	  river_s(I,N)=-99.99
          river_H(I,N)=-99.99
          river_f(I,N)=-99.99 
        ENDDO
      ENDDO	 

CC ---------Specify river_time-----------------------
      NREC=NINT((jdaye-jdays)*24/DELT)+1
      DO N=1,NREC
        river_time(N)=jdays+(N-1)*DELT/24.0-jbase_date
        IF(trim(OCEAN_MODEL).EQ."FVCOM") THEN
          jday=jdays+(N-1)*DELT/24.0
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
            IF(MOD(IYR,4).EQ.0) DAYS_PER_MONTH(2)=29
            IF(IDD.GT.DAYS_PER_MONTH(IMM)) THEN
              IDD=IDD-DAYS_PER_MONTH(IMM)
              IMM=IMM+1
              IF(IMM .GT.12) THEN
                IMM=IMM-12
                IYR=IYR+1
              ENDIF
            ENDIF
          ENDIF
          WRITE(BUFFER,810) IYR,'-',IMM,'-',IDD,'T',IHH,':',IMN,
     .	    ':00.000000'  
810       FORMAT(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A10)
          Times(N)=trim(adjustL(BUFFER))
          Itime(N)=INT(river_time(N)+0.01)
          Itime2(N)=INT((river_time(N)-Itime(N))*86400)*1000
        ENDIF
      ENDDO

C--Read in climatological (multiple-year daily mean) river data----
C--and process to equally-spaced time series-----------------------
      INQUIRE(FILE=trim(RIVER_CLIMFILE),EXIST=FEXIST)
      IF(.NOT.FEXIST) THEN
        WRITE(*,*) 'RIVER Climatologic NetCDF FILE: '//
     .    TRIM(RIVER_CLIMFILE)//' IS NOT FOUND !!'
        WRITE(*,*) 'Please check in FIX directory'
        WRITE(*,*) 'Provide correct River Climatologic File Name'
        WRITE(*,*) TRIM(OFS)//' stopped !!! '
        STOP
      ELSE
        DO I=1,4
          DIMS(I)=1
        ENDDO
        VNAME='stationID'
        STATUS = NF_OPEN(RIVER_CLIMFILE,NF_NOWRITE,NCID)
        STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
        STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        DO I=1,NDIMS
          STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
C          write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
        enddo

        allocate(stationID(DIMS(1),DIMS(2)))
        STATUS = NF_GET_VAR_TEXT(NCID,IDVAR,stationID)
        STATUS = NF_INQ_VARID(NCID,'discharge',IDVAR)
        STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        DO I=1,NDIMS
          STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
        ENDDO
        NCLIM_STA=DIMS(2)
        NCLIM=DIMS(1)+10 
C        write(*,*) 'Dims of discharge= ',DIMS(1),DIMS(2)
        IF(ALLOCATED(river_t_clim)) DEALLOCATE(river_t_clim)
        allocate(river_t_clim(DIMS(1),DIMS(2)))
        IF(ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
        allocate(tmp3d(NCLIM,DIMS(2),3))
        IF(ALLOCATED(rtime_clim)) DEALLOCATE(rtime_clim)
        allocate(rtime_clim(NCLIM))

        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,river_t_clim)
        DO I=1,DIMS(1)
          DO J=1,DIMS(2)
            tmp3d(I,J,1)=river_t_clim(I,J)
          ENDDO
        ENDDO

        STATUS = NF_INQ_VARID(NCID,'temperature',IDVAR)
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,river_t_clim)
        DO I=1,DIMS(1)
          DO J=1,DIMS(2)
            tmp3d(I,J,2)=river_t_clim(I,J)
          ENDDO
        ENDDO

        STATUS = NF_INQ_VARID(NCID,'salinity',IDVAR)
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,river_t_clim)
        DO I=1,DIMS(1)
          DO J=1,DIMS(2)
            tmp3d(I,J,3)=river_t_clim(I,J)
          ENDDO
        ENDDO

        yearb=dble(IYRS)
        monthb=1.0d0
        dayb=1.0d0
        hourb=0.0d0
        day0=JULIAN(yearb,monthb,dayb,hourb)-jbase_date
        DO I=1,NCLIM
          rtime_clim(I)=day0+I-1
        ENDDO
        
C  Repeat first 10 day climatological values for crossing-year forecasts    
        DO I=DIMS(1)+1,NCLIM
          I0=I-DIMS(1)
          DO J=1,DIMS(2)
            tmp3d(I,J,1)=tmp3d(I0,J,1)
            tmp3d(I,J,2)=tmp3d(I0,J,2)
            tmp3d(I,J,3)=tmp3d(I0,J,3)
          ENDDO
        ENDDO

        IF(ALLOCATED(river_t_clim)) DEALLOCATE(river_t_clim)       
        IF(ALLOCATED(river_s_clim)) DEALLOCATE(river_s_clim)
        IF(ALLOCATED(river_p_clim)) DEALLOCATE(river_p_clim)
        allocate(river_q_clim(NRIVERS,NCLIM))
        allocate(river_t_clim(NRIVERS,NCLIM))
        allocate(river_s_clim(NRIVERS,NCLIM))
        allocate(river_p_clim(NRIVERS,NCLIM))

        DO I=1,NRIVERS
          BUFFER=TRIM(adjustL(USGS_ID(I)))
          L1=LEN_TRIM(BUFFER)
          DO J=1,NCLIM_STA
            L2=size(stationID,1)
            BUFFER1=' '
            DO IIK=1,L2
              BUFFER1(IIK:IIK)=stationID(IIK,J)
            END DO
            BUFFER1=TRIM(adjustL(BUFFER1))
            IF(BUFFER(1:L1).EQ.BUFFER1(1:L1)) EXIT 
          ENDDO

          WRITE(*,*) 'RIVER CLIM. AT STATION: ',BUFFER(1:L1)
          IF(J.GT.NCLIM_STA) THEN
            IF(Q_CTL(I).LE.2) THEN
              WRITE(*,*) 'STATION ID IS NOT FOUND IN RIVER CLIM. FILE'
              WRITE(*,*) 'PLEASE ADD RIVER CLIM. DATA OF STATION:'
              WRITE(*,*) ' -----'//TRIM(USGS_ID(I))//'  ----'
              WRITE(*,*) TRIM(OFS)//' STOP HERE'
              STOP
            ELSE
              DO N=1,NCLIM
                river_q_clim(I,N)=-99.9
                river_t_clim(I,N)=-99.9
                river_s_clim(I,N)=-99.9
              ENDDO
            ENDIF   
          ELSE
            DO N=1,NCLIM
              river_q_clim(I,N)=tmp3d(N,J,1)
              river_t_clim(I,N)=tmp3d(N,J,2)
              river_s_clim(I,N)=tmp3d(N,J,3)
            ENDDO
          ENDIF
        ENDDO
        STATUS = NF_CLOSE(NCID)
      ENDIF 

C     Process USGS real time river observation of BUFR file
      yearb=dble(IYRS)
      monthb=dble(IMMS)
      dayb=dble(IDDS)
      hourb=dble(IHHS)
      WRITE(FOUT,201) 'USGS_RIVER_',IYRS,IMMS,IDDS,IHHS,'.dat'
201   FORMAT(A11,I4.4,3I2.2,A4)
      FOUT=TRIM(OFS)//'_'//TRIM(adjustL(FOUT)) 
      OPEN(38,file=TRIM(FOUT))
      write(38,*) 'ID        lat     lon   year  mon day hour  '//
     .  'Disch   Stage   Precip   Tair   SST   Sal  Wspd  Wdir'
      
      WRITE(*,*) 'Looping through available BUFR files to decode' 
      WRITE(*,*) 'start and end time=',day_start,DAY_END
      DO IZ=INT(day_start),INT(DAY_END)
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

        WRITE(BUFRFILE,200) '/',IYR,IMM,IDD,'/b001/'  
200     FORMAT(A1,I4.4,2I2.2,A6) 
        BUFRFILE=TRIM(COMUSGS)//trim(BUFRFILE)//trim(USGSBUFR)
        INQUIRE(FILE=trim(BUFRFILE),EXIST=FEXIST)
C        IF(.NOT.FEXIST) GOTO 288
        IF(FEXIST) THEN
          write(*,*) 'BUFR FILE= ',trim(BUFRFILE)
  
C  -----OPEN AND READ THRU THE INPUT BUFR FILE ------------------
          CLOSE(LUNIN)
          OPEN(LUNIN,file=trim(BUFRFILE),FORM='UNFORMATTED')
          CALL OPENBF(LUNIN,'IN',LUNIN)

C  -----READ THROUGH THE MESSAGES/SUBSETS IN THE FILE------------
          DO WHILE (IREADMG(LUNIN,SUBSET,IDATE).EQ.0)
            DO WHILE(IREADSB(LUNIN).EQ.0)
              CALL UFBINT(LUNIN,DATES,5,1,IRET,
     .          'YEAR MNTH DAYS HOUR MINU')
              IYR = NINT(DATES(1))
              IMM = NINT(DATES(2))
              IDD = NINT(DATES(3))
              IHH = NINT(DATES(4))
              IMN = NINT(DATES(5))
              yearb=dble(IYR)
              monthb=dble(IMM)
              dayb=dble(IDD)
              hourb=dble(REAL(IHH)+REAL(IMN/60.0))   
              dayj=JULIAN(yearb,monthb,dayb,hourb)-jbase_date

C  -------READ THE USGS GAUGE STATION INFO FROM BUFR FILE--------
C  Use different CALL routine to handle long station IDs 
              CALL READLC(LUNIN,stnbufrid,'RPID')
              CALL UFBINT(LUNIN,XLOCAT,5,1,IRET,'CLAT CLON SELV')
              clat=xlocat(1)
              clon=xlocat(2)
              selv=xlocat(3)

C  -------CHECK USGS STATION ID AGAINST NOS OFS RIVER STATION LIST---- 
              DO I=1,NRIVERS
                IF(trim(stnbufrid).EQ.trim(USGS_ID(I))) THEN

C   GET AIR TEMPERATURE AND SEA SURFACE TEMPERATURE DATA
                  CALL UFBINT(LUNIN,data3,5,1,IRET,'TMDB SST1')
                  ATMP_ori=data3(1)
                  SST_ori=data3(2)
                  if(data3(1).GE.BMISS/2.0) then
                    ATMP=-99.99
                  else
                    ATMP=data3(1)-273.15
                  endif

                  if(data3(2).GE.BMISS/2.0) then
                    sst=-99.99
                  else
                    sst=data3(2)-273.15
C  Remove bad values for real time observation
                    IF(SST.LT.0.0) SST=-99.99
                  endif

                  IF(sst.GT. -90.0) THEN
C  Temp is cut off at maximum and minimum values
                    if(sst.GT.T_max(I)) sst=T_max(I)
                    if(sst.LT.T_min(I)) sst=T_min(I)

                    IF(NTR_T(I).LT.1) THEN
                      NTR_T(I)=NTR_T(I)+1
                      RTIME_T(I,NTR_T(I))=dayj
                      RIVER_T(I,NTR_T(I))=sst
                    ELSE
                      IF(dayj.GT.RTIME_T(I,NTR_T(I))) THEN
                        NTR_T(I)=NTR_T(I)+1
                        RTIME_T(I,NTR_T(I))=dayj
                        RIVER_T(I,NTR_T(I))=sst
                      ENDIF
                    ENDIF
                  ENDIF
               
C  GET RIVER DISCHARGE DATA INCLUDING INFO ON USGS SENSOR TYPE
                  CALL UFBINT(LUNIN,data1,2,500,nlev,'DCHG DDDC')
                  dchgd_ori=data1(1,1)
                  if(data1(1,1).GE.BMISS/2.0) then
                    dchgd=-99.99
                  else
                    dchgd=data1(1,1)
C  Disch is cut off at Maximum and minimum values
                    if(dchgd.GT.Q_max(I)) dchgd=Q_max(I)
                    if(dchgd.LT.Q_min(I)) dchgd=Q_min(I)

                    IF(NTR_Q(I).LT.1) THEN
                      NTR_Q(I)=NTR_Q(I)+1
                      RTIME_Q(I,NTR_Q(I))=dayj
                      RIVER_Q(I,NTR_Q(I))=dchgd
                    ELSE
                      IF(dayj.GT.RTIME_Q(I,NTR_Q(I))) THEN
                        NTR_Q(I)=NTR_Q(I)+1
                        RTIME_Q(I,NTR_Q(I))=dayj
                        RIVER_Q(I,NTR_Q(I))=dchgd
                      ENDIF
                    ENDIF 
                  endif

                  if(data1(2,1).GE.BMISS/2.0) then
                    dchgi=-99.99
                  else
                    dchgi=data1(2,1)
                  endif

C  GET RIVER STAGE DATA INCLUDING INFO ON USGS SENSOR TYPE 
C  Stage height is in meters
                  CALL UFBint(LUNIN,data1,2,500,nlev1,'RSHM DDRS')
                  rstgd_ori=data1(1,1)
                  rstgdi_ori=data1(2,1)
                  if(data1(1,1).GE.BMISS/2.0) then
                    rstgd=-99.99
                  else
                    rstgd=data1(1,1)
                    IF(NTR_H(I).LT.1) THEN
                      NTR_H(I)=NTR_H(I)+1
                      RTIME_H(I,NTR_H(I))=dayj
                      RIVER_H(I,NTR_H(I))=rstgd
                    ELSE
                      IF(dayj.GT.RTIME_H(I,NTR_H(I))) THEN
                        NTR_H(I)=NTR_H(I)+1
                        RTIME_H(I,NTR_H(I))=dayj
                        RIVER_H(I,NTR_H(I))=rstgd
                      ENDIF
                    ENDIF 
                  endif

                  if(data1(2,1).GE.BMISS/2.0) then
                    rstgdi=-99.99
                  else
                    rstgdi=data1(2,1)
                  endif

C  GET RIVER STAGE HEIGHT ABOVE NGVD 1929, STREAM VELOCITY, AND SALINITY
C  Stage height is in meters
                  CALL UFBINT(LUNIN,data3,5,1,IRET,
     .              'RSH29 STRV SALN WACN')
                  rsh29_ori=data3(1)
                  saln_ori=data3(3)
                  cond_ori=data3(4)
                  if(data3(1).GE.BMISS/2.0) then
                    RSH29=-99.99
                  else
                    rsh29=data3(1)
                  endif

                  if(data3(2).GE.BMISS/2.0) then
                    strv=-99.99
                  else
                    strv=data3(2)
                  endif

                  if(data3(3).GE.BMISS/2.0) then
                    saln=-99.99
                  else
                    saln=data3(3)
                  endif

                  if(data3(4).GE.BMISS/2.0) then
                    cond=-99.99
                  else
                    cond=data3(4)
                  endif

                  IF(SALN.EQ.-99.99) THEN
                    IF((SST.NE.-99.99).AND.(COND.NE.-99.99)) THEN
C  Convert uS/cm to mS/cm
                      COND=COND*(1.0+0.02*(SST-25.0))
C  Calculate salinity from SST and conductivity
                      SALN=SAL(COND,SST,0.0)
                    ENDIF   
                  ENDIF 

C  Avoid saln is negative or zero
                  IF(SALN.LT.-0.5) SALN=0.002
                  IF(SALN.GT.-99.0) THEN
                    IF(NTR_S(I).LT.1) THEN
                      NTR_S(I)=NTR_S(I)+1
                      RTIME_S(I,NTR_S(I))=dayj
                      RIVER_S(I,NTR_S(I))=SALN
                    ELSE 
                      IF(dayj.GT.RTIME_S(I,NTR_S(I))) THEN
                        NTR_S(I)=NTR_S(I)+1
                        RTIME_S(I,NTR_S(I))=dayj
                        RIVER_S(I,NTR_S(I))=SALN
                      ENDIF
                    ENDIF
                  ENDIF

C  Convert stage height to discharge if Q_CTL=2. 
C  In this case, the rating table file has to exist in FIXofs
                  IF(Q_CTL(I).EQ.2) THEN
                    CLOSE(34)
                    NTR_Q(I)=NTR_H(I)
                    BUFFER=trim(USGS_ID(I))//'_rating_table.dat'
                    BUFFER=trim(FIXofs)//'/'//trim(BUFFER)
                    INQUIRE(FILE=trim(BUFFER),EXIST=FEXIST)
                    IF(.NOT.FEXIST) THEN
                      WRITE(*,*) 'Flow rating table file : '//
     .                  trim(BUFFER)//' IS NOT FOUND!!'
                      WRITE(*,*) 'Please check in FIXofs directory'
                      WRITE(*,*) 'Provide River flow rating table '//
     .                  'File '//TRIM(OFS)//'. stopped !!! '
                      STOP
                    ENDIF  

                    open(34,file=trim(BUFFER),STATUS='OLD')
                    ITMP=1
                    read(34,*)
                    read(34,*)
                    DO ITMP=1,10000
                      read(34,*,end=620) rating_h(itmp),
     .                  rating_flow(itmp)
                    END DO
620	            CLOSE(34)
                    NTABLE=ITMP-1

                    DO ITMP=1,NTR_H(I)
                      RTIME_Q(I,ITMP)=RTIME_H(I,ITMP)
                      IF(RIVER_H(I,ITMP).LE.-99.0) THEN
                        RIVER_Q(I,ITMP)=-99.99
                      ELSE
C  Convert from meter to feet
                        XNEW=RIVER_H(I,ITMP)*3.28 
                        IF(XNEW.LT.rating_h(1)) THEN
                          RIVER_Q(I,ITMP)=rating_flow(1)
                        ELSEIF(XNEW.GT.rating_h(NTABLE)) THEN
                          RIVER_Q(I,ITMP)=rating_flow(NTABLE)
                        ELSE
                          if(allocated(tmpp1)) deallocate(tmpp1)
                          if(allocated(tmpp2)) deallocate(tmpp2)
                          allocate(tmpp1(1:NTABLE)); tmpp1=0.0
                          allocate(tmpp2(1:NTABLE)); tmpp2=0.0
                          DO NPP=1,NTABLE
                            tmpp1(NPP)=rating_h(NPP)
                            tmpp2(NPP)=rating_flow(NPP)
                          End do
                          CALL spline(NTABLE,tmpp1,tmpp2,XNEW,YNEW)
                          deallocate(tmpp1,tmpp2)
                        ENDIF
C  Convert from cfs to cms
                        RIVER_Q(I,ITMP)=YNEW*0.0283168
                      ENDIF
                    ENDDO    
                  ENDIF

C GET TOTAL PRECIPITATION AND SENSOR TYPE INOF
                  CALL UFBINT(LUNIN,data1,2,500,nlev2,'TOPC DDPC')
                  if(data1(1,1).ge.BMISS/2.0) then
                    precp=-99.99
                  else
                    precp=data1(1,1)
                  endif

                  if(data1(2,1).GE.BMISS/2.0) then
                    precpi=-99.99
                  else
                    precpi=data1(2,1)
                  endif

C GET SURFACE WIND DIRECTION AND SPEED DATA AND INFO ON SENSOR TYPES
                  CALL UFBint(LUNIN,data2,4,500,nlev3,
     .              'WDIR WSPD DDWD DDWS')
                  if(data2(1,1).GE.BMISS/2.0) then
                    wdir=-99.99
                  else
                    wdir=data2(1,1)
                  endif

                  if(data2(2,1).GE.BMISS/2.0) then
                    wspd=-99.99
                  else
                    wspd=data2(2,1)
                  endif

C  WRITE OUT SELECTED OBS FROM USGS GAUGE STATIONS
                  write(38,60) trim(stnbufrid),clat,clon,iyr,IMM,
     .              idd,ihh,IMN,dchgd,rstgd,precp,atmp,sst,saln,
     .              cond,wspd,wdir,rsh29
                ENDIF
              ENDDO 
            ENDDO
          ENDDO
60        FORMAT(a20,1x,2f8.2,1x,i4,4(1x,i2.2),1x,f12.2,1x,f7.3,1x,
     .      f10.6,6f7.2,1x,f7.2)
      
C  WHEN FINISHED MAKE SURE ALL BUFFERS ARE FLUSHED THEN EXIT            
          CALL CLOSBF(LUNIN)
          CLOSE(LUNIN)
        END IF
C288     CONTINUE
      ENDDO
      CLOSE(38)                                              
C      CLOSE(17)                                              
      
      INQUIRE(FILE=TRIM(FOUT),EXIST=FEXIST)
      IF(FEXIST) THEN
        CMD='sort -u '//TRIM(FOUT)//' > tmp.dat'
        call system(trim(CMD) )
        CMD='cp tmp.dat '//TRIM(FOUT)
        call system(trim(CMD) )
        call system('rm -f tmp.dat tmp1.dat' )
      ENDIF	 

      DO I=1,NRIVERS
        WRITE(*,*) 'I= ',I,trim(USGS_ID(I)),NTR_Q(I),NTR_T(I),
     .    NTR_S(I),NTR_H(I)
      ENDDO	  

C  Process NWLON/PORTS realtime observations in BUFR files of NCEP data tank
      WRITE(FOUT,302) 'NOS_WL_',IYRS,IMMS,IDDS,IHHS,'.dat'  
302   FORMAT(A7,I4.4,3I2.2,A4)
      FOUT=TRIM(OFS)//'_'//TRIM(adjustL(FOUT))
      CLOSE(15) 
      OPEN(15,file=TRIM(FOUT),STATUS='unknown')
      write(15,*) 'ID        lat     lon   year  mon day hour  '//
     .  'Disch   Tem SST   Salinity'
      WRITE(*,*) 'Looping through available BUFR files to decode' 

      DO IZ=INT(day_start),INT(DAY_END)
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
        WRITE(BUFRFILE,310) '/',IYR,IMM,IDD,'/b001/'  
310     FORMAT(A1,I4.4,2I2.2,A6)

        BUFRFILE=TRIM(COMPORTS)//trim(BUFRFILE)//trim(NOSBUFR)
        INQUIRE(FILE=trim(BUFRFILE),EXIST=FEXIST)
C        IF(.NOT.FEXIST) GOTO 460
        IF(FEXIST) THEN
          WRITE(*,*) 'BUFR FILE= '//trim(BUFRFILE)
          LUNIN=11
          CLOSE(LUNIN)

C  --------OPEN AND READ THRU THE INPUT BUFR FILE ------------------       
          OPEN(LUNIN,file=trim(BUFRFILE),FORM='UNFORMATTED',
     .      STATUS='OLD')
          CALL OPENBF(LUNIN,'IN',LUNIN)

C  --------READ THROUGH THE MESSAGES/SUBSETS IN THE FILE------------  
          DO WHILE(IREADMG(LUNIN,SUBSET,IDATE).EQ.0)
            DO WHILE(IREADSB(LUNIN).EQ.0)
C  --------READ THE INTERNAL DATE AND CHECK FOR REALISM-------------
              CALL UFBINT(LUNIN,DATES,5,1,IRET,
     .          'YEAR MNTH DAYS HOUR MINU')
              IYR = NINT(DATES(1))
              IMM = NINT(DATES(2))
              IDD = NINT(DATES(3))                                           
              IHH = NINT(DATES(4))                                            
              IMN = NINT(DATES(5))                                            
              yearb=dble(IYR)
              monthb=dble(IMM)
              dayb=dble(IDD)
              hourb=dble(REAL(IHH)+REAL(IMN/60.0))   
              dayj=JULIAN(yearb,monthb,dayb,hourb)-jbase_date

C  --------READ THE TIDE GAUGE STATION INFO FROM BUFR FILE----------
c  Use different CALL routine to handle long station IDs 
              CALL READLC(LUNIN,stnbufrid,'RPID')
              CALL UFBINT(LUNIN,DATES,5,1,IRET,'CLAT CLON SELV')
              clat=DATES(1)
              clon=DATES(2)
              selv=DATES(3)
              DO I=1,NRIVERS
                IF(trim(stnbufrid).EQ.TRIM(NWS_ID(I))) THEN
C  GET SEA SURFACE TEMPERATURE DATA ALONG WITH DATA CHECK AND
C  TIME INCREMENT AND DISPLACEMENT INFORMATION
C  SST1 -- Sea Surface Temperature
C  QMST -- Sea Surface Data Check Flag
C  AWCK -- Tide Station Automated Water Data Check Flag
C  MWCK -- Tide Station Manual Water Data Check Flag
C  TPMI -- Time Period or Replacement
                  LLEN=LEN_TRIM(BUFRFILE)
C   GET AIR TEMPERATURE AND SEA SURFACE TEMPERATURE DATA
                  IF(BUFRFILE(LLEN-4:LLEN).EQ.'xx005') THEN      
                    CALL UFBINT(LUNIN,DATES,5,1,IRET,
     .	              'SST1 TMDB AWCK MWCK TPMI')
                  ELSEIF(BUFRFILE(LLEN-4:LLEN).EQ.'xx012') THEN
                    CALL UFBINT(LUNIN,DATES,5,1,IRET,'WATM TMDB')
                  ENDIF	 

                  IF(NTR_T(I).LE.10) THEN
                    if(DATES(1).GE.BMISS/2.0) then
                      SST=-99.99
                    else
C  Convert temperature from Kelvin to Celcius
                      SST=DATES(1)-273.15
C  Remove bad realtime observation data
                      IF (SST.LT.0.0) SST=-99.99
                    endif

                    IF(SST .GT. -90.0)THEN
C  Temp is cut off between minimum and maximum values
                      if(sst.GT.T_max(I)) sst=T_max(I)
                      if(sst.LT.T_min(I)) sst=T_min(I)
                      IF(NTR_T(I) .LT. 1) THEN
                        NTR_T(I)=NTR_T(I)+1
                        RTIME_T(I,NTR_T(I))=dayj
                        RIVER_T(I,NTR_T(I))=sst
                      ELSE
                        IF(dayj.GT.RTIME_T(I,NTR_T(I))) THEN
                          NTR_T(I)=NTR_T(I)+1
                          RTIME_T(I,NTR_T(I))=dayj
                          RIVER_T(I,NTR_T(I))=sst
                        ENDIF
                      ENDIF 
                    endif

                    if(DATES(2).GE.BMISS/2.0) then
                      ATMP=-99.99
                    else	  
                      ATMP=DATES(2)   
                    endif
                  END IF

C  GET CONDUCTIVITY AND SALINITY
                  CALL UFBINT(LUNIN,DATES,5,1,IRET,'SALN WACN')
                  if(DATES(1).GE.BMISS/2.0) then
                    SALN=-99.99
                  else	  
                    SALN=DATES(1)
                  endif

                  if(DATES(2).GE.BMISS/2.0) then
                    COND=-99.99
                  else	  
                    COND=DATES(2)   
                  endif

                  IF(SALN.EQ.-99.99) THEN
                    IF((SST.NE.-99.99).AND.(COND.NE.-99.99)) THEN
                      SALN=SAL(COND,SST,0.0)
                    ENDIF   
                  ENDIF

                  IF(SALN.GT.-99.00) THEN
                    IF(NTR_S(I).LT.1) THEN
                      NTR_S(I)=NTR_S(I)+1
                      RTIME_S(I,NTR_S(I))=dayj
                      RIVER_S(I,NTR_S(I))=saln
                    ELSE
                      IF(dayj.GT.RTIME_S(I,NTR_S(I))) THEN
                        NTR_S(I)=NTR_S(I)+1
                        RTIME_S(I,NTR_S(I))=dayj
                        RIVER_S(I,NTR_S(I))=saln
                      ENDIF
                    ENDIF
                  ENDIF  

C  GET TIDAL ELEVATION WITH RESPECT TO CHART AND METEOROLOGICAL 
C  RESIDUAL TIDAL ELEVATION
                  IF(BUFRFILE(LLEN-4:LLEN).EQ.'xx005') THEN      
                    CALL UFBINT(LUNIN,DATES,5,1,IRET,'TERC TIDER')
                  ELSEIF(BUFRFILE(LLEN-4:LLEN).EQ.'xx012') THEN 
                    CALL UFBINT(LUNIN,DATES,5,1,IRET,'TLLW TIDER')
                  ENDIF	 

                  IF(DATES(1).GE.BMISS/2.0) THEN
                    EL=-99.99
                  ELSE	
                    EL=DATES(1)  
                    IF(NTR_H(I).LT.1) THEN
                      NTR_H(I)=NTR_H(I)+1
                      RTIME_H(I,NTR_H(I))=DAYJ
                      RIVER_H(I,NTR_H(I))=EL
                    ELSE
                      IF(DAYJ.GT.RTIME_H(I,NTR_H(I))) THEN
                        NTR_H(I)=NTR_H(I)+1
                        RTIME_H(I,NTR_H(I))=DAYJ
                        RIVER_H(I,NTR_H(I))=EL
                      ENDIF
                    ENDIF
                  ENDIF

                  IF(DATES(2).GE.BMISS/2.0) THEN
                    SWL=-99.99
                  ELSE	
                    SWL=DATES(2)   
                  ENDIF

!!!!!!!!!!!!!  machuan added the following block becase NCEP BUFR team
!!!!!          encoding Niagara River water level as TERC and
!!!           SPOC DDRS for river flow for both Niagara and St.Lawrence

                CALL UFBINT(LUNIN,DATES,5,1,IRET,'TERC')
                if(DATES(1).GE.BMISS/2.0) then
                  EL=-99.99
                else
                  EL=DATES(1)
                  IF(NTR_H(I).LT.1) THEN
                    NTR_H(I)=NTR_H(I)+1
                    RTIME_H(I,NTR_H(I))=dayj
                    RIVER_H(I,NTR_H(I))=EL
                  ELSE
                    IF(dayj.GT.RTIME_H(I,NTR_H(I))) THEN
                      NTR_H(I)=NTR_H(I)+1
                      RTIME_H(I,NTR_H(I))=dayj
                      RIVER_H(I,NTR_H(I))=EL
                    ENDIF
                  ENDIF
                endif
                if(DATES(2).GE.BMISS/2.0) then
                  SWL=-99.99
                else
                  SWL=DATES(2)
                endif

                CALL UFBint(LUNIN,data1,2,500,nlev1,'RSHM DDRS')
                rstgd_ori=data1(1,1)
                rstgdi_ori=data1(2,1)
                if(data1(1,1).GE.BMISS/2.0) then
                  rstgd=-99.99
                else
                  rstgd=data1(1,1)
                  IF(NTR_H(I).LT.1) THEN
                    NTR_H(I)=NTR_H(I)+1
                    RTIME_H(I,NTR_H(I))=dayj
                    RIVER_H(I,NTR_H(I))=rstgd
                  ELSE
                    IF(dayj.GT.RTIME_H(I,NTR_H(I))) THEN
                      NTR_H(I)=NTR_H(I)+1
                      RTIME_H(I,NTR_H(I))=dayj
                      RIVER_H(I,NTR_H(I))=rstgd
                    ENDIF
                  ENDIF
                endif
                if(data1(2,1).GE.BMISS/2.0) then
                  rstgdi=-99.99
                else
                  rstgdi=data1(2,1)
                endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  SPOC flow at Niagra and St. Lawrance

                CALL UFBint(LUNIN,data1,2,500,nlev1,'SPOC DDRS')
                rflow_ori=data1(1,1)
                rflowi_ori=data1(2,1)
                if(data1(1,1).GE.BMISS/2.0) then
                  rflow=-99.99
                else
                  rflow=data1(1,1)
                  IF(NTR_F(I).LT.1) THEN
                    NTR_F(I)=NTR_F(I)+1
                    RTIME_F(I,NTR_F(I))=dayj
                    RIVER_F(I,NTR_F(I))=rflow
                  ELSE
                    IF(dayj.GT.RTIME_F(I,NTR_F(I))) THEN
                      NTR_F(I)=NTR_F(I)+1
                      RTIME_F(I,NTR_F(I))=dayj
                      RIVER_F(I,NTR_F(I))=rflow
                    ENDIF
                  ENDIF
                endif

                if(data1(2,1).GE.BMISS/2.0) then
                  rflowi=-99.99
                else
                  rflowi=data1(2,1)
                endif

!!!!!  following calculate the rivers dischage from stage and flow

                IF(trim(stnbufrid).EQ."BRKQ6".or.trim(stnbufrid)
     &           .EQ."YNTN6") THEN
                 IF(NTR_Q(I).LT.1) THEN
                   rflow=RIVER_F(I,1)
                   rstgd=RIVER_H(I,1)
                  if (rflow.gt.-90.0.and.rstgd.gt.-90.0) then
                    NTR_Q(I)=NTR_Q(I)+1
                    RTIME_Q(I,NTR_Q(I))=dayj
                    IF(trim(stnbufrid).EQ."BRKQ6") THEN
                        flow=0.4566*rflow + 0.1399
                        area=1973.1*rstgd - 125358
                    ENDIF

                    IF(trim(stnbufrid).EQ."YNTN6") THEN
                        flow=0.76*rflow + 0.19
                        area=437.85*rstgd - 25063
                    ENDIF
                    dchgd=flow*area
                    if(dchgd.GT.Q_max(I)) dchgd=Q_max(I) !! Disch cut
                    if(dchgd.LT.Q_min(I)) dchgd=Q_min(I) !! Disch cut

                    RIVER_Q(I,NTR_Q(I))=dchgd
                    write(*,*) "ST,NTR,F,A,disc ",stnbufrid,NTR_Q(I),
     *              flow,area,dchgd
                  endif
                 ELSE
                  IF(dayj.GT.RTIME_Q(I,NTR_Q(I))) THEN
                    rflow=RIVER_F(I,NTR_q(I)+1)
                    rstgd=RIVER_H(I,NTR_q(I)+1)
                    if (rflow.gt.-90.0.and.rstgd.gt.-90.0) then
                      NTR_Q(I)=NTR_Q(I)+1

                      IF(trim(stnbufrid).EQ."BRKQ6") THEN
                           flow=0.4566*rflow + 0.1399
                           area=1973.1*rstgd - 125358
                      endif

                      IF(trim(stnbufrid).EQ."YNTN6") THEN
                           flow=0.76*rflow + 0.19
                           area=437.85*rstgd - 25063
                      endif

                      dchgd=flow*area

                      if(dchgd.GT.Q_max(I)) dchgd=Q_max(I)  !! Disch cut
                      if(dchgd.LT.Q_min(I)) dchgd=Q_min(I)  !! Disch cut

                      RTIME_Q(I,NTR_Q(I))=dayj
                      RIVER_Q(I,NTR_Q(I))=dchgd

                      write(*,*) "ST,NTR,F,A,disc ",stnbufrid,NTR_Q(I),
     *                flow,area,dchgd

                    endif
                  ENDIF
                 ENDIF

                 if(data1(2,1).GE.BMISS/2.0) then
                  dchgi=-99.99
                 else
                  dchgi=data1(2,1)
                 endif

                ENDIF


!!!!!! end the block of addtion river discharge calculation from flow
!and stage 


C GET SURFACE WIND DIRECTION AND SPEED DATA AND INFO ON SENSOR TYPES
                  CALL UFBINT(LUNIN,DATA2,4,500,NLEV3,
     .              'WDIR WSPD DDWD DDWS')
                  IF(DATA2(1,1).GE.BMISS/2.0) THEN
                    WDIR=-99.99
                  ELSE
                    WDIR=DATA2(1,1)
                  ENDIF

                  IF(DATA2(2,1).GE.BMISS/2.0) THEN
                    WSPD=-99.99
                  ELSE
                    WSPD=DATA2(2,1)
                  ENDIF
                  WRITE(15,60) TRIM(STNBUFRID),CLAT,CLON,IYR,IMM,
     .              IDD,IHH,IMN,DCHGD,RSTGD,PRECP,ATMP,SST,SALN,
     .              COND,WSPD,WDIR
                ENDIF 
              ENDDO
            ENDDO
          ENDDO
          CALL CLOSBF(LUNIN)
          CLOSE(LUNIN)
        END IF
C460     CONTINUE
C500     CONTINUE
      ENDDO
      CLOSE(15)

      INQUIRE(FILE=TRIM(FOUT),EXIST=FEXIST)
      IF(FEXIST) THEN
        CMD='sort -u '//TRIM(FOUT)//' > tmp.dat'
        call system(trim(CMD))
        CMD='cp tmp.dat '//TRIM(FOUT)
        call system(trim(CMD))
        call system('rm -f tmp.dat tmp1.dat')
      ENDIF	 
                                                                   
      DO I=1,NRIVERS
        WRITE(*,*) 'I= ',I,trim(USGS_ID(I)),NTR_Q(I),NTR_T(I),
     .    NTR_S(I),NTR_H(I)
      ENDDO	  

C  Special treatment for CREOFS
      IF(TRIM(OFS).EQ."creofs") THEN
        I0=0
        DO I=1,NRIVERS
          IF(TRIM(USGS_ID(I)).EQ.'14128870') I0=I
        ENDDO

        WRITE(*,*) 'Process estimated river discharge from BPA'
        IF((I0.GE.1).AND.(I0.LE.NRIVERS)) THEN
          NTR_Q(I0)=0
          DO N=1,NTMAX
            RTIME_Q(I0,N)=0.0
            RIVER_Q(I0,N)=-99.99
            RIVER_T(I0,N)=-99.99
            RIVER_S(I0,N)=-99.99
          ENDDO

          IF(NTR_Q(I0).LE.0) THEN
C  Use "14105700" as backup
            IZ=0
            DO I=1,NRIVERS
              IF(trim(USGS_ID(I)).EQ.'14105700') IZ=I
            ENDDO
            WRITE(*,*) 'Use backup station of 14105700 IZ= ',IZ
 
            IF(NTR_Q(IZ).GT.0) THEN
              NTR_Q(I0)=NTR_Q(IZ)
              DO N=1,NTR_Q(I0)
                RTIME_Q(I0,N)=RTIME_Q(IZ,N)
                RIVER_Q(I0,N)=RIVER_Q(IZ,N)
              ENDDO
            ENDIF
          ENDIF

          DO N=1,NTR_Q(I0)
            WRITE(*,*) N,RTIME_Q(I0,N),RIVER_Q(I0,N)
          ENDDO
        ENDIF
        WRITE(*,*) 'Processing BPA river data completed'
      ENDIF

C     Process real time river observations, interpolate to 
C     equal time interval series, and store to river_q and river_t 
      DO I=1,NRIVERS
        ICOUNT=0
	CLOSE(50)
	OPEN(50,file=TRIM(USGS_ID(I))//'_river.obs')
        DO N=1,NTR_Q(I)
          IF(abs(River_q(I,N)+99.99).GT.0.01) THEN
            ICOUNT=ICOUNT+1
            oned1(ICOUNT)=RTIME_Q(I,N)
            oned2(ICOUNT)=River_q(I,N)
            WRITE(50,'(I6,F14.4,F12.2)') ICOUNT,oned1(ICOUNT),
     .        oned2(ICOUNT)
          ENDIF
        ENDDO
        CLOSE(50)

        WRITE(TXT_TMP,'(I7)') ICOUNT
        BUFFER='NUMBER OF DISCHARGE AT '//TRIM(USGS_ID(I))
        BUFFER=TRIM(BUFFER)//' from '//TRIM(START_TIME)//' to '
        BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '//TXT_TMP
        WRITE(*,*) TRIM(BUFFER)
        WRITE(ICORMS,'(A)') TRIM(BUFFER)
          
        IF((ICOUNT.LT.20).OR.(Q_CTL(I).EQ.0)) THEN
          WRITE(*,*) 'WARNING: no realtime data at '//TRIM(USGS_ID(I))
          WRITE(*,*) 'USE climatological river dataset'
          WRITE(ICORMS,'(A)') 'USE Climatologic river dataset'
          DO N=1,NCLIM
            if(river_q_clim(I,N).LE.-999.0) river_q_clim(I,N)=0.0
            oned2(N)=river_q_clim(I,N)
          ENDDO

          if(allocated(tmpp1)) deallocate(tmpp1)
          if(allocated(tmpp2)) deallocate(tmpp2)
          allocate(tmpp1(1:NREC)); tmpp1=0.0
          allocate(tmpp2(1:NREC)); tmpp2=0.0
          if(allocated(tmpp3)) deallocate(tmpp3)
          if(allocated(tmpp4)) deallocate(tmpp4)
          allocate(tmpp3(1:NCLIM)); tmpp3=0.0
          allocate(tmpp4(1:NCLIM)); tmpp4=0.0

          DO N=1,NREC
            tmpp1(N)=river_time(N)
          End do

          DO N=1,NCLIM
            tmpp3(N)=rtime_clim(N)
            tmpp4(N)=oned2(N)
          End do

          CALL lineararray(NREC,tmpp1,tmpp2,NCLIM,tmpp3,tmpp4)
          DO N=1,NREC
            river_q(I,N)=tmpp2(N)
          ENDDO
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
        ELSE  
          WRITE(*,*) 'Use realtime river observations'
          WRITE(ICORMS,'(A)') 'Use realtime river observations'
C  Calculate mean discharges to replace instantaneous values to get
C  better persist discharges for forecast cycle, it is critical for
C  tidal rivers case.
          SUMTMP=0.0
          DO N=1,ICOUNT
            SUMTMP=SUMTMP+oned2(N)
          ENDDO
          AVG_DISCH=SUMTMP/ICOUNT
  
          if(oned1(ICOUNT).LT.day_end) then
            ICOUNT=ICOUNT+1
            oned1(ICOUNT)=day_end
            oned2(ICOUNT)=AVG_DISCH
          endif

          if(allocated(tmpp1)) deallocate(tmpp1)
          if(allocated(tmpp2)) deallocate(tmpp2)
          allocate(tmpp1(1:NREC)); tmpp1=0.0
          allocate(tmpp2(1:NREC)); tmpp2=0.0
          if(allocated(tmpp3)) deallocate(tmpp3)
          if(allocated(tmpp4)) deallocate(tmpp4)
          allocate(tmpp3(1:ICOUNT)); tmpp3=0.0
          allocate(tmpp4(1:ICOUNT)); tmpp4=0.0

          DO N=1,NREC
            tmpp1(N)=river_time(N)
          End do

          DO N=1,ICOUNT
            tmpp3(N)=oned1(N)
            tmpp4(N)=oned2(N)
          End do

          CALL lineararray(NREC,tmpp1,tmpp2,ICOUNT,tmpp3,tmpp4)
          DO N=1,NREC
            river_q(I,N)=tmpp2(N)
          ENDDO
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
        ENDIF 
      ENDDO

c  Added by Zheng to replace the river real time data with
c  NWM output
!      IF(NWMUSED.EQ.1) THEN
      IF(NWMUSED.EQ.1.AND.MAXVAL(IUS).EQ.1) THEN
        WRITE(*,*) 'Use NWM output to provide river flux'
        WRITE(ICORMS,'(A)') 'Use NWM output to provide river flux'
        DO I=1,NRIVERS
c  Only consider rivers located in NWM model domain
          IF(IUS(I).GT.0) THEN          
            IF(ALLOCATED(TMPP1)) DEALLOCATE(TMPP1)
            IF(ALLOCATED(TMPP2)) DEALLOCATE(TMPP2)
            ALLOCATE(TMPP1(1:NREC)); TMPP1=0.0
            ALLOCATE(TMPP2(1:NREC)); TMPP2=0.0
            IF(ALLOCATED(TMPP3)) DEALLOCATE(TMPP3)
            IF(ALLOCATED(TMPP4)) DEALLOCATE(TMPP4)
            ALLOCATE(TMPP3(1:NUMNWM)); TMPP3=0.0
            ALLOCATE(TMPP4(1:NUMNWM)); TMPP4=0.0
          
            DO N=1,NREC
              TMPP1(N)=RIVER_TIME(N)
            END DO
            DO N=1,NUMNWM
              TMPP3(N)=NWMTIME(N)
              TMPP4(N)=NWM_Q(I,N)
            End do

            CALL LINEARARRAY(NREC,TMPP1,TMPP2,NUMNWM,TMPP3,TMPP4)
            DO N=1,NREC
              RIVER_Q(I,N)=TMPP2(N)
            ENDDO
            DEALLOCATE(TMPP1,TMPP2,TMPP3,TMPP4)

            IF(I.EQ.1) THEN
              CLOSE(50)
              OPEN(50,FILE='NWM_ori.dat',status='unknown')
              DO N=1,NUMNWM
                WRITE(50,'(f11.4,f7.1)') NWMTIME(N),NWM_Q(I,N)
              END DO
              CLOSE(50)

              OPEN(50,FILE='NWM_int.dat',status='unknown')
              DO N=1,NREC
                WRITE(50,'(f11.4,f7.1)') RIVER_TIME(N),RIVER_Q(I,N)
              END DO
              CLOSE(50)
            END IF
          END IF
        END DO
      ELSE
        WRITE(*,*) 'Use USGS data to provide river flux'
        WRITE(ICORMS,'(A)') 'Use USGS data to provide river flux'
      END IF
c  End of added by Zheng

C Search the number of rivers that are used to specify temperature
      ICOUNT=1
      Ioned1(ICOUNT)=RiverID_T(1)
      DO I=2,NIJ
        I0=0
        DO J=1,ICOUNT
          IF(RiverID_T(I).NE.Ioned1(J)) I0=I0+1
        ENDDO

        IF(I0.EQ.ICOUNT) THEN	
          ICOUNT=ICOUNT+1
          Ioned1(ICOUNT)=RiverID_T(I)
        ENDIF
      ENDDO
      NRIVERT=ICOUNT
      WRITE(*,*) 'Number of sta for T & S is ',NRIVERT

      DO I=1,NRIVERT
        ID=Ioned1(I)
        ICOUNT=0

        CLOSE(50)
        OPEN(50,file=TRIM(USGS_ID(ID))//'_river_temp.obs',
     .    status='unknown')
        avg=0.0
        DO N=1,NTR_T(ID)
          IF(River_T(ID,N).GT.-10.00) THEN
            ICOUNT=ICOUNT+1
            oned1(ICOUNT)=RTIME_T(ID,N)
            oned2(ICOUNT)=River_T(ID,N)
            AVG=AVG+River_T(ID,N)
          ENDIF
        ENDDO

        IF(ICOUNT.GT.0) AVG=AVG/ICOUNT
        IF(ICOUNT.GT.2) THEN
          SD=0.0    
          DO N=1,ICOUNT
            SD=SD+(ONED2(N)-AVG)**2
          ENDDO 
          SD=SQRT(SD/(ICOUNT-1))
        ELSE
          SD=1.0
        ENDIF    

        BOUND_L=AVG-3.0*SD
        BOUND_U=AVG+3.0*SD 
        WRITE(*,*) 'Bound of T = ',BOUND_L,BOUND_U,AVG,ID

        ICOUNT=0
        DO N=1,NTR_T(ID)
          IF((River_T(ID,N).GE.BOUND_L).AND.
     .       (River_T(ID,N).LE.BOUND_U)) THEN
            ICOUNT=ICOUNT+1
            oned1(ICOUNT)=RTIME_T(ID,N)
            oned2(ICOUNT)=River_T(ID,N)
            AVG=AVG+River_T(ID,N)
            WRITE(50,'(I6,F14.4,F12.2)') ICOUNT,oned1(ICOUNT),
     .        oned2(ICOUNT)
          ENDIF
        ENDDO

        WRITE(*,*) 'Number of good T obs = ',ICOUNT,trim(USGS_ID(ID))
        WRITE(TXT_TMP,'(I7)') ICOUNT
        BUFFER='NUMBER OF RIVER TEMP AT '//TRIM(USGS_ID(ID))
        BUFFER=TRIM(BUFFER)//' from '//TRIM(START_TIME)//' to '
        BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '//TXT_TMP
        WRITE(ICORMS,'(A)') TRIM(BUFFER)
        WRITE(*,*) TRIM(BUFFER)

        IF((ICOUNT.LT.2).OR.(T_CTL(ID).EQ.0)) THEN
          WRITE(*,*) 'WARNING: no realtime temp data available'
          WRITE(*,*) 'Use climatological river temp dataset'
          WRITE(ICORMS,'(A)') 'Use climatological river temp dataset'
          DO N=1,NCLIM
            if(river_t_clim(ID,N).le.-99.0) river_t_clim(ID,N)=0.0
            oned2(N)=river_t_clim(ID,N)
          ENDDO

          if(allocated(tmpp1)) deallocate(tmpp1)
          if(allocated(tmpp2)) deallocate(tmpp2)
          allocate(tmpp1(1:NREC)); tmpp1=0.0
          allocate(tmpp2(1:NREC)); tmpp2=0.0
          if(allocated(tmpp3)) deallocate(tmpp3)
          if(allocated(tmpp4)) deallocate(tmpp4)
          allocate(tmpp3(1:NCLIM)); tmpp3=0.0
          allocate(tmpp4(1:NCLIM)); tmpp4=0.0

          DO N=1,NREC
            tmpp1(N)=river_time(N)
          End do

          DO N=1,NCLIM
            tmpp3(N)=rtime_clim(N)
            tmpp4(N)=oned2(N)
          End do

          CALL lineararray(NREC,tmpp1,tmpp2,NCLIM,tmpp3,tmpp4)
          DO N=1,NREC
            river_T(ID,N)=tmpp2(N)
          ENDDO
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
        ELSE  
          WRITE(*,*) 'Use realtime river temp observations'
          WRITE(ICORMS,'(A)') 'Use realtime river temp observations'
          SUMTMP=0.0
          DO N=1,ICOUNT
            SUMTMP=SUMTMP+oned2(N)
          ENDDO
          AVG_T=SUMTMP/ICOUNT

          if(oned1(ICOUNT).LT.day_end) then
            ICOUNT=ICOUNT+1
            oned1(ICOUNT)=day_end
            oned2(ICOUNT)=AVG_T
          endif

          if(allocated(tmpp1)) deallocate(tmpp1)
          if(allocated(tmpp2)) deallocate(tmpp2)
          allocate(tmpp1(1:NREC)); tmpp1=0.0
          allocate(tmpp2(1:NREC)); tmpp2=0.0
          if(allocated(tmpp3)) deallocate(tmpp3)
          if(allocated(tmpp4)) deallocate(tmpp4)
          allocate(tmpp3(1:ICOUNT)); tmpp3=0.0
          allocate(tmpp4(1:ICOUNT)); tmpp4=0.0

          DO N=1,NREC
            tmpp1(N)=river_time(N)
          End do

          DO N=1,ICOUNT
            tmpp3(N)=oned1(N)
            tmpp4(N)=oned2(N)
          End do

          CALL lineararray(NREC,tmpp1,tmpp2,ICOUNT,tmpp3,tmpp4)
          DO N=1,NREC
            river_T(ID,N)=tmpp2(N)
          ENDDO
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
        ENDIF
	 
        ICOUNT=0
        DO N=1,NTR_S(ID)
          IF(River_S(ID,N).GE.0.0) THEN
            ICOUNT=ICOUNT+1
            oned1(ICOUNT)=RTIME_S(ID,N)
            oned2(ICOUNT)=River_S(ID,N)
          ENDIF
        ENDDO

        WRITE(*,*) 'Number of good S obs = ',ICOUNT,trim(USGS_ID(ID))
        WRITE(TXT_TMP,'(I7)') ICOUNT
        BUFFER='NUMBER OF RIVER SALT AT '//TRIM(USGS_ID(ID))
        BUFFER=TRIM(BUFFER)//' from '//TRIM(START_TIME)//' to '
        BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '//TXT_TMP
        WRITE(ICORMS,'(A)') TRIM(BUFFER)
        IF((ICOUNT.LT.2).OR.(T_CTL(ID).EQ.0)) THEN
          WRITE(*,*)'WARNING: no good data available' 
          BUFFER='USE climatological river salinity dataset'
          WRITE(*,*) TRIM(BUFFER)
          WRITE(ICORMS,'(A)') TRIM(BUFFER)
          DO N=1,NCLIM
            if(river_s_clim(ID,N).le.-99.0) river_s_clim(ID,N)=0.0
            oned2(N)=river_s_clim(ID,N)
          ENDDO

          if(allocated(tmpp1)) deallocate(tmpp1)
          if(allocated(tmpp2)) deallocate(tmpp2)
          allocate(tmpp1(1:NREC)); tmpp1=0.0
          allocate(tmpp2(1:NREC)); tmpp2=0.0
          if(allocated(tmpp3)) deallocate(tmpp3)
          if(allocated(tmpp4)) deallocate(tmpp4)
          allocate(tmpp3(1:NCLIM)); tmpp3=0.0
          allocate(tmpp4(1:NCLIM)); tmpp4=0.0
          DO N=1,NREC
            tmpp1(N)=river_time(N)
          End do
          DO N=1,NCLIM
            tmpp3(N)=rtime_clim(N)
            tmpp4(N)=oned2(N)
          End do

          CALL lineararray(NREC,tmpp1,tmpp2,NCLIM,tmpp3,tmpp4)
          DO N=1,NREC
            river_S(ID,N)=tmpp2(N)
          ENDDO
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
        ELSE  
          BUFFER='Use real-time river salinity observations'
          WRITE(*,*) TRIM(BUFFER)
          WRITE(ICORMS,'(A)') TRIM(BUFFER)
          SUMTMP=0.0
          DO N=1,ICOUNT
            SUMTMP=SUMTMP+oned2(N)
          ENDDO
          AVG_S=SUMTMP/ICOUNT

          if(oned1(ICOUNT).LT.day_end) then
            ICOUNT=ICOUNT+1
            oned1(ICOUNT)=day_end
            oned2(ICOUNT)=AVG_S
          endif

          if(allocated(tmpp1)) deallocate(tmpp1)
          if(allocated(tmpp2)) deallocate(tmpp2)
          allocate(tmpp1(1:NREC)); tmpp1=0.0
          allocate(tmpp2(1:NREC)); tmpp2=0.0
          if(allocated(tmpp3)) deallocate(tmpp3)
          if(allocated(tmpp4)) deallocate(tmpp4)
          allocate(tmpp3(1:ICOUNT)); tmpp3=0.0
          allocate(tmpp4(1:ICOUNT)); tmpp4=0.0
          DO N=1,NREC
            tmpp1(N)=river_time(N)
          End do
          DO N=1,ICOUNT
            tmpp3(N)=oned1(N)
            tmpp4(N)=oned2(N)
          End do

          CALL lineararray(NREC,tmpp1,tmpp2,ICOUNT,tmpp3,tmpp4)
          DO N=1,NREC
            river_S(ID,N)=tmpp2(N)
          ENDDO
          deallocate(tmpp1,tmpp2,tmpp3,tmpp4)
        ENDIF
      ENDDO	

C---------------------------------------------------------------------
C    Calculate Q and T at MODEL grid locations 
C---------------------------------------------------------------------
      A1_DISO = -173.4292;
      A2_DISO =  249.6339;
      A3_DISO =  143.3483;
      A4_DISO =  -21.8492;
      B1_DISO = -0.033096;
      B2_DISO =  0.014259;
      B3_DISO = -0.001700;

      DO I=1,NIJ
        DO N=1,NREC
          river_transport(I,N)=river_q(RiverID_Q(I),N)*Q_Scale(I)
          if(river_T(RiverID_T(I),N).lt.1.0) then
            river_T(RiverID_T(I),N)=1.0
          end if
          tmp2d1(I,N)=river_T(RiverID_T(I),N)*T_Scale(I)
          tmp2d2(I,N)=0.005
          DO K=1,KBm 
            river_temp(I,K,N)=river_T(RiverID_T(I),N)*T_Scale(I)
            river_salt(I,K,N)=0.005
            S_DISO=river_salt(I,K,N)
            T_DISO=river_temp(I,K,N)
            TK_DISO=(T_DISO+273.15)*1.00024
            TK100=TK_DISO/100.0
            C_DISO=A1_DISO+A2_DISO*(100.0/TK_DISO)+
     .        A3_DISO*LOG(TK100)+A4_DISO*TK100+S_DISO*(B1_DISO+
     .        B2_DISO*TK100+B3_DISO*(TK100*TK100))
            river_diso(I,k,n)=EXP(C_DISO)*1427.6/32.0
          ENDDO       
        ENDDO	 
      ENDDO     

C---------------------------------------------------------------------
C     River profile in vertical direction
      WRITE(*,*) 'Computing River vertical Profile Distribution'
      IF(trim(OCEAN_MODEL).EQ."ROMS") THEN
        DO K=1,KBm
          x=k-1
          river_Vshape(1,K)=x*SQRT(x)
        ENDDO

        Bmax=maxval(river_Vshape(1,:))
        DO K=1,KBm
          river_Vshape(1,K)=river_Vshape(1,K)/bmax
   	  river_Vshape(1,K)=1.0-1.0/(1+KBm*river_Vshape(1,K)*
     .	    river_Vshape(1,K))
        ENDDO

        asum=sum(river_Vshape(1,1:KBm))
        DO K=1,KBm
          river_Vshape(1,K)=river_Vshape(1,k)/asum
        ENDDO

        DO I=2,NIJ
          DO K=1,KBm
            river_Vshape(I,K)=river_Vshape(1,k)
          ENDDO	
        ENDDO
      ENDIF	 	

C  Set global attributes string of the NetCDF
      CALL DATE_AND_TIME(BIG_BEN(1),BIG_BEN(2),BIG_BEN(3),DATE_TIME)
      WRITE(CURRENT_TIME,800) DATE_TIME(5),':',DATE_TIME(6),
     .  DATE_TIME(2),'/',DATE_TIME(3),'/',DATE_TIME(1)
800   format(I2.2,a1,I2.2,2x,I2.2,a1,i2.2,a1,I4)
      globalstr(1)=trim(OCEAN_MODEL)//' river forcing netCDF file'
      globalstr(2)=trim(OFS)//' River forcing netCDF file'
      globalstr(3)='River discharge from USGS real time or daily mean'
      globalstr(4)='River T from USGS real time or daily mean, Salt=0'
      globalstr(5)='GRID file: '//trim(GRIDFILE)
      globalstr(6)='River Forcing file: '//trim(netcdf_file)
      globalstr(7)='Created using nos_ofs_create_river_ROMS.sh'
      globalstr(8)='Created at time '//trim(CURRENT_TIME)
      globalstr(9)='Created by Aijun Zhang, OD/CO-OPS/NOS/NOAA'
       
C   ---------Wrtite into NetCDF output file for ROMS, FVCOM, or SELFE----
      IF(trim(OCEAN_MODEL).EQ."ROMS") THEN
        WRITE(*,*) 'Output river forcing for ROMS'
        call nos_ofs_write_netCDF_river_ROMS(GRIDFILE,netcdf_file,
     .    ncid,IROMS,JROMS,KBm,NREC,NIJ,base_date,
     .    river_names,river,river_Xposition,river_Eposition,
     .    river_direction,river_flag,river_sign,river_Vshape,
     .    river_time,river_transport,river_temp,river_salt,
     .    river_pass,river_diso,globalstr,ioxyg)
        write(*,*) 'River forcing file has been generated successfully'
        write(*,*) 'COMPLETED SUCCESSFULLY'
        WRITE(ICORMS,'(a)') 'RIVER FORCING COMPLETED SUCCESSFULLY 100'
      ELSEIF(trim(OCEAN_MODEL).EQ."FVCOM") THEN
        WRITE(*,*) 'Output river forcing for FVCOM'
        DO I=1,NIJ
          WRITE(FOUT,'(I12)') river_Xposition(I) 
          FOUT=trim(adjustL(FOUT))
	  FOUT='./RIVER/'//TRIM(FOUT)//'.nc' 
	  imode=1
          call nos_ofs_write_netCDF_river_FVCOM(GRIDFILE,FOUT,
     .      ncid,imode,1,1,base_date,river_names(I),Itime(1),
     .      Itime2(1),Times(1),river_transport(I,1),tmp2d1(I,1),
     .      tmp2d2(I,1),globalstr)

	  imode=2
          DO N=1,NREC
            call nos_ofs_write_netCDF_river_FVCOM(GRIDFILE,FOUT,
     .        ncid,imode,1,1,base_date,river_names(I),Itime(N),
     .        Itime2(N),Times(N),river_transport(I,N),tmp2d1(I,N),
     .        tmp2d2(I,N),globalstr)
          ENDDO

	  imode=3
          call nos_ofs_write_netCDF_river_FVCOM(GRIDFILE,FOUT,
     .      ncid,imode,1,1,base_date,river_names(I),Itime(1),
     .      Itime2(1),Times(1),river_transport(I,1),tmp2d1(I,1),
     .      tmp2d2(I,1),globalstr)  
	ENDDO  
        write(*,*) 'River forcing file has been generated successfully'
        write(*,*) 'COMPLETED SUCCESSFULLY'
        WRITE(ICORMS,'(a)')'RIVER FORCING COMPLETED SUCCESSFULLY 100'
      ELSEIF(trim(OCEAN_MODEL).EQ."SELFE") THEN
        WRITE(*,*) 'Output river forcing for SELFE'
        ALLOCATE(tmp2d3(NRIVERT,3))
        OPEN(40,file='selfe_flux.th',status='unknown')
        OPEN(41,file='selfe_temp.th',status='unknown')
        OPEN(42,file='selfe_salt.th',status='unknown')
        DO N=2,NREC  ! from first time step
          time_sec=river_time(N)*86400.0
          IF(time_sec.GT.0.50) THEN
            WRITE(40,600) time_sec,(-river_transport(I,N),I=1,NIJ)
            WRITE(41,601) time_sec,(tmp2d1(I,N),I=1,NIJ)
            WRITE(42,601) time_sec,(tmp2d2(I,N),I=1,NIJ)
          ENDIF  
        ENDDO 
        CLOSE(40) 
        CLOSE(41) 
        CLOSE(42) 
        write(*,*) 'River forcing file has been generated successfully'
        write(*,*) 'COMPLETED SUCCESSFULLY'
        WRITE(ICORMS,'(a)') 'RIVER FORCING COMPLETED SUCCESSFULLY 100'
      ENDIF
600   FORMAT(F12.0,50F12.2)
601   FORMAT(F12.0,50F12.4)
      WRITE(ICORMS,'(a)') 'END SECTION OF GENERATING RIVER FILE' 
      CLOSE(ICORMS)

      STOP
      END


C--------------------------------------------------------
C      THIS FUNCTION IS A 'FRONT END' TO THE SALINITY ROUTINE
C      GIVEN IN UNESCO TECHNICAL PAPES IN MARINE SCIENCE, NO 44.
C
C      THE CODE FOR THESE ROUTINES WAS GENEROUSLY SUPPLIED
C      BY WOOD'S HOLE.
C
C      THE QUANTITY WHICH IS USUALLY KNOWN AS DEPTH IN OUR
C      SOFTWARE IS, IN FACT, PRESSURE.
C ****************************************************************
C UNITS:
C     PRESSURE        P        DECIBARS
C     TEMPERATURE     T        DEG CELSIUS (IPTS-68)
C     CONDUCTIVITY    CND      RATIO mS/cm    (M=0)
C     SALINITY        SAL78    (PSS-78)  (M=0)
C CHECKVALUES:
C     SAL78=1.888091 :CND= 40.0000,T=40 DEG C,P= 10000 DECIBARS: M= 1
C     SAL78=40.00000 :CND=1.888091,T=40 DEG C,P=10000 DECIBARS:  M=0
C-------------------------------------------------------

      FUNCTION SAL(C,T,P)
C
C      C35150 IS THE CONDUCTIVITY OF STANDARD SEAWATER AT 35 PRACTICAL
C      SALINITY UNITS, 15 DEGREES, AND ATMOSPHERIC PRESSURE
      SAVE
      DATA C35150 / 42.914 /

      IF(ABS(C).GE. 0.01) THEN  
C      ALLOW FOR EFFECTS OF TEMPERATURE AND PRESSURE ON THE
C      CONDUCTIVITY CELL
        G=(C*(1.-6.5E-06*(T-2.8)+1.5E-8*(P-3000.)))/C35150
        SAL=SAL78(G,T,P,0)
      ELSE
C      ERROR - CONDUCTIVITY EFFECTIVELY ZERO
       SAL=0.0
      ENDIF

      RETURN
      END


C-----------------------------------------------------
C      FUNCTION TO CALCULATE CONDUCTIVITY FROM S,T AND P
C      USING THE ABOVE-MENTIONED ROUTINES
C-----------------------------------------------------

      FUNCTION CONDUC(S,T,P)
      SAVE
      DATA C35150 / 42.914 /

      G=SAL78(S,T,P,1)
      CONDUC=G*C35150/(1.0-6.5E-06*(T-2.8)+1.5E-8*(P-3000.0))

      RETURN
      END


C--------------------------------------------------
C      FUNCTION TO CALCULATE SIGMA-T, GIVEN THE SALINITY AND
C      TEMPERATURE.
C---------------------------------------------------

      FUNCTION SIGMAT(S,T)
      SV=SVAN(S,T,0.0,SIG)
      SIGMAT=SIG

      RETURN
      END


C=====================================================================
C      START OF UNESCO ROUTINES
C=====================================================================
C  SEPT. 28 1983
C ADD TF(S,P) FREEZING PT.
C WHOI CTD GROUP DISK FILE SPEC=BLUE::CTDA:<CTDEV.PRSW>PHYPROPSW.FOR
C
C  TITLE: ALGORITHMS FOR OCEANOGRAPHIC COMPUTATIONS
C N FOFONOFF & R MILLARD
C SAL78 FCN ********** MAR 28 1983 *******
      REAL FUNCTION SAL78(CND,T,P,M)
Czaj    CND=Conductivity / C(35,15,0) while unit of C is ms/cm, when m=0
czaj    CND=salinity unit is PPT, when M=1
C ********************************************************************
C     THE CONDUCTIVITY RATIO (CND)=1.0000000 FOR SALINITY = 35 PSS-78
C     TEMPERATURE=15.0 DEGREE CELSIUS, AND ATMOSPHERIC PRESSURE.
C********************************************************************
C
C FUNCTION TO CONVERT CONDUCTIVITY RATIO TO SALINITY (M = 0)
C SALINITY TO CONDUCTIVITY RATIO (M = 1,CND BECOMES INPUT SALINITY)
C*****************************************************************
C  REFERENCES:   ALSO LOCATED IN UNESCO REPORT # 37 1981
C  PRACTICAL SALINITY SCALE 1978: E.L. LEWIS IEEE OCEAN ENG. JAN. 1980
C *******************************************************************
C UNITS:
C     PRESSURE        P        DECIBARS
C     TEMPERATURE     T        DEG CELSIUS (IPTS-68)
C     CONDUCTIVITY    CND      RATIO     (M=0)
C     SALINITY        SAL78    (PSS-78)  (M=0)
C CHECKVALUES:
C     SAL78=1.888091 :CND= 40.0000,T=40 DEG C,P= 10000 DECIBARS: M= 1
C     SAL78=40.00000 :CND=1.888091,T=40 DEG C,P=10000 DECIBARS:  M=0
C*******************************************************************
C SAL78 RATIO: RETURNS ZERO FOR CONDUCTIVITY RATIO:  < 0.0005
C SAL78: RETURNS ZERO FOR SALINITY:  < 0.02
C ****************************************************************
C INTERNAL FUNCTIONS
C ****************************************************************
C  PRACTICAL SALINITY SCALE 1978 DEFINITION WITH TEMPERATURE CORRECTION
C  XT=T-15.0 : XR=SQRT(RT)

      SAL(XR,XT)=((((2.7081*XR-7.0261)*XR+14.0941)*XR+25.3851)*XR-
     .  0.1692)*XR+0.0080+(XT/(1.0+0.0162*XT))*(((((-0.0144*XR+
     .  0.0636)*XR-0.0375)*XR-0.0066)*XR-0.0056)*XR+0.0005)
C  DSAL(XR,XT)  FUNCTION FOR DERIVATIVE OF SAL(XR,XT) WITH XR.
      DSAL(XR,XT)=((((13.5405*XR-28.1044)*XR+42.2823)*XR+50.7702)*XR-
     .  0.1692)+(XT/(1.0+0.0162*XT))*((((-0.0720*XR+0.2544)*XR-
     .  0.1125)*XR-0.0132)*XR-0.0056)

C  FUNCTION RT35: C(35,T,0)/C(35,15,0) VARIATION WITH TEMPERATURE
C  WITH TEMPERATURE.
      RT35(XT)=(((1.0031E-9*XT-6.9698E-7)*XT+1.104259E-4)*XT+
     .  2.00564E-2)*XT+0.6766097

C POLYNOMIALS OF RP: C(S,T,P)/C(S,T,0) VARIATION WITH PRESSURE
C  C(XP) POLYNOMIAL CORRESPONDS TO A1-A3 CONSTANTS: LEWIS 1980
      C(XP)=((3.989E-15*XP-6.370E-10)*XP+2.070E-5)*XP
      B(XT)=(4.464E-4*XT+3.426E-2)*XT+1.0

C  A(XT) POLYNOMIAL CORRESPONDS TO B3 AND B4 CONSTANTS: LEWIS 1980
      A(XT)=-3.107E-3*XT+0.4215

C*******************************************************************
C ZERO SALINITY/CONDUCTIVITY TRAP
      SAL78=0.0

C ***********************************************
      DT=T-15.0
C SELECT BRANCH FOR SALINITY (M=0) OR CONDUCTIVITY (M=1)
      IF(M.NE.1) THEN
C ************************************************
C CONVERT CONDUCTIVITY TO SALINITY
        R=CND
        RT=R/(RT35(T)*(1.0+C(P)/(B(T)+A(T)*R)))
        RT= SQRT(ABS(RT))
        SAL78=SAL(RT,DT)

C zaj extended to low salinity in estuarine based on EPA's formulation
        a0=0.008
        b0=0.0005
        X=400.0*RT
        Y=100.0*RT
        F=DT/(1.0+0.0162*DT)
        DELTS=a0/(1+1.5*X+X*X)+b0*F/(1+Y**0.5+Y**1.5)
        SAL78=SAL78-DELTS  
      ELSE
C *********  END OF CONDUCTIVITY TO SALINITY SECTION ***********
C *******************************************************
C INVERT SALINITY TO CONDUCTIVITY BY THE
C  NEWTON-RAPHSON ITERATIVE METHOD.
C ******************************************************
C FIRST APPROXIMATION
10      RT=SQRT(CND/35.0)
        SI=SAL(RT,DT)
        N=0
        DELS=99.9

C  ITERATION LOOP BEGINS HERE WITH A MAXIMUM OF 10 CYCLES
        DO WHILE((DELS.GT.1.0E-4).AND.(N.LT.10))
          RT=RT+(CND - SI)/DSAL(RT,DT)
          SI=SAL(RT,DT)
          N=N+1
          DELS=ABS(SI-CND)
        ENDDO

C ****************************END OF ITERATION LOOP ********
C COMPUTE CONDUCTIVITY RATIO
        RTT=RT35(T)*RT*RT
        AT=A(T)
        BT=B(T)
        CP=C(P)
        CP=RTT*(CP+BT)
        BT=BT-RTT*AT

C SOLVE QUADRATIC EQUATION FOR R: R=RT35*RT*(1+C/AR+B)
        R=SQRT(ABS(BT*BT+4.0*AT*CP))-BT

C CONDUCTIVITY RETURN
        SAL78=0.5*R/AT
      ENDIF

      RETURN
      END


      REAL FUNCTION SVAN(S,T,P0,SIGMA)
C  MODIFIED RCM
C ******************************************************
C SPECIFIC VOLUME ANOMALY (STERIC ANOMALY) BASED ON 1980 EQUATION
C OF STATE FOR SEAWATER AND 1978 PRACTICAL SALINITY SCALE.
C REFERENCES
C MILLERO, ET AL (1980) DEEP-SEA RES.,27A,255-264
C MILLERO AND POISSON 1981,DEEP-SEA RES.,28A PP 625-629.
C BOTH ABOVE REFERENCES ARE ALSO FOUND IN UNESCO REPORT 38 (1981)
C UNITS:
C       PRESSURE        P0       DECIBARS
C       TEMPERATURE     T        DEG CELSIUS (IPTS-68)
C       SALINITY        S        (IPSS-78)
C       SPEC. VOL. ANA. SVAN     M**3/KG *1.0E-8
C       DENSITY ANA.    SIGMA    KG/M**3
C ******************************************************************
C CHECK VALUE: SVAN=981.3021 E-8 M**3/KG.  FOR S = 40 (IPSS-78) ,
C T = 40 DEG C, P0= 10000 DECIBARS.
C CHECK VALUE: SIGMA = 59.82037  KG/M**3 FOR S = 40 (IPSS-78) ,
C T = 40 DEG C, P0= 10000 DECIBARS.
C *******************************************************
      REAL P,T,S,SIG,SR,R1,R2,R3,R4
      REAL A,B,C,D,E,A1,B1,AW,BW,K,K0,KW,K35
C EQUIV
      EQUIVALENCE (E,D,B1),(BW,B,R3),(C,A1,R2)
      EQUIVALENCE (AW,A,R1),(KW,K0,K)
C ********************
C DATA
      DATA R3500,R4/1028.1063,4.8314E-4/
      DATA DR350/28.106331/
      SAVE
C   R4 IS REFERED TO AS  C  IN MILLERO AND POISSON 1981
C CONVERT PRESSURE TO BARS AND TAKE SQUARE ROOT SALINITY.
      P=P0/10.
      SR = SQRT(ABS(S))
C *********************************************************
C PURE WATER DENSITY AT ATMOSPHERIC PRESSURE
C   BIGG P.H.,(1967) BR. J. APPLIED PHYSICS 8 PP 521-537.
C
      R1 = ((((6.536332E-9*T-1.120083E-6)*T+1.001685E-4)*T-
     .  9.095290E-3)*T+6.793952E-2)*T-28.263737
C SEAWATER DENSITY ATM PRESS.
C  COEFFICIENTS INVOLVING SALINITY
C  R2 = A   IN NOTATION OF MILLERO AND POISSON 1981
      R2 = (((5.3875E-9*T-8.2467E-7)*T+7.6438E-5)*T-4.0899E-3)*T+
     .  8.24493E-1
C  R3 = B  IN NOTATION OF MILLERO AND POISSON 1981
      R3 = (-1.6546E-6*T+1.0227E-4)*T-5.72466E-3
C  INTERNATIONAL ONE-ATMOSPHERE EQUATION OF STATE OF SEAWATER
      SIG = (R4*S + R3*SR + R2)*S + R1
C SPECIFIC VOLUME AT ATMOSPHERIC PRESSURE
      V350P = 1.0/R3500
      SVA = -SIG*V350P/(R3500+SIG)
      SIGMA=SIG+DR350
C  SCALE SPECIFIC VOL. ANAMOLY TO NORMALLY REPORTED UNITS
      SVAN=SVA*1.0E+8
      IF(P.EQ.0.0) RETURN
C ******************************************************************
C ******  NEW HIGH PRESSURE EQUATION OF STATE FOR SEAWATER ********
C ******************************************************************
C        MILLERO, ET AL , 1980 DSR 27A, PP 255-264
C               CONSTANT NOTATION FOLLOWS ARTICLE
C********************************************************
C COMPUTE COMPRESSION TERMS
      E = (9.1697E-10*T+2.0816E-8)*T-9.9348E-7
      BW = (5.2787E-8*T-6.12293E-6)*T+3.47718E-5
      B = BW + E*S
C
      D = 1.91075E-4
      C = (-1.6078E-6*T-1.0981E-5)*T+2.2838E-3
      AW = ((-5.77905E-7*T+1.16092E-4)*T+1.43713E-3)*T-0.1194975
      A = (D*SR + C)*S + AW
C
      B1 = (-5.3009E-4*T+1.6483E-2)*T+7.944E-2
      A1 = ((-6.1670E-5*T+1.09987E-2)*T-0.603459)*T+54.6746
      KW = (((-5.155288E-5*T+1.360477E-2)*T-2.327105)*T+
     .   148.4206)*T-1930.06
      K0 = (B1*SR + A1)*S + KW
C EVALUATE PRESSURE POLYNOMIAL
C ***********************************************
C   K EQUALS THE SECANT BULK MODULUS OF SEAWATER
C   DK=K(S,T,P)-K(35,0,P)
C  K35=K(35,0,P)
C ***********************************************
      DK = (B*P + A)*P + K0
      K35  = (5.03217E-5*P+3.359406)*P+21582.27
      GAM=P/K35
      PK = 1.0 - GAM
      SVA = SVA*PK + (V350P+SVA)*P*DK/(K35*(K35+DK))
C  SCALE SPECIFIC VOL. ANAMOLY TO NORMALLY REPORTED UNITS
      SVAN=SVA*1.0E+8
      V350P = V350P*PK
C  ****************************************************
C COMPUTE DENSITY ANAMOLY WITH RESPECT TO 1000.0 KG/M**3
C  1) DR350: DENSITY ANAMOLY AT 35 (IPSS-78), 0 DEG. C AND 0 DECIBARS
C  2) DR35P: DENSITY ANAMOLY 35 (IPSS-78), 0 DEG. C ,  PRES. VARIATION
C  3) DVAN : DENSITY ANAMOLY VARIATIONS INVOLVING SPECFIC VOL. ANAMOLY
C ********************************************************************
C CHECK VALUE: SIGMA = 59.82037  KG/M**3 FOR S = 40 (IPSS-78),
C T = 40 DEG C, P0= 10000 DECIBARS.
C *******************************************************
      DR35P=GAM/V350P
      DVAN=SVA/(V350P*(V350P+SVA))
      SIGMA=DR350+DR35P-DVAN

      RETURN
      END


      REAL FUNCTION DEPTH(P,LAT)
C ********************************
C DEPTH IN METERS FROM PRESSURE IN DECIBARS USING
C SAUNDERS AND FOFONOFF'S METHOD.
C DEEP-SEA RES., 1976,23,109-111.
C FORMULA REFITTED FOR 1980 EQUATION OF STATE
C UNITS:
C       PRESSURE        P        DECIBARS
C       LATITUDE        LAT      DEGREES
C       DEPTH           DEPTH    METERS
C CHECKVALUE: DEPTH = 9712.653 M FOR P=10000 DECIBARS, LATITUDE=30 DEG
C     ABOVE FOR STANDARD OCEAN: T=0 DEG. CELSUIS ; S=35 (IPSS-78)
C
      REAL LAT
C
      X = SIN(LAT/57.29578)
C**************************
      X = X*X
C GR= GRAVITY VARIATION WITH LATITUDE: ANON (1970) BULLETIN GEODESIQUE
      GR = 9.780318*(1.0+(5.2788E-3+2.36E-5*X)*X) + 1.092E-6*P
      DEPTH = (((-1.82E-15*P+2.279E-10)*P-2.2512E-5)*P+9.72659)*P
      DEPTH=DEPTH/GR

      RETURN
      END


      REAL FUNCTION TF(S,P)
C   FUNCTION TO COMPUTE THE FREEZING POINT OF SEAWATER
C
C   REFERENCE: UNESCO TECH. PAPERS IN THE MARINE SCIENCE NO. 28. 1978
C   EIGHTH REPORT JPOTS
C   ANNEX 6 FREEZING POINT OF SEAWATER F.J. MILLERO PP.29-35.
C
C  UNITS:
C         PRESSURE      P          DECIBARS
C         SALINITY      S          PSS-78
C         TEMPERATURE   TF         DEGREES CELSIUS
C         FREEZING PT.
C************************************************************
C  CHECKVALUE: TF= -2.588567 DEG. C FOR S=40.0, P=500. DECIBARS
      TF=(-.0575+1.710523E-3*SQRT(ABS(S))-2.154996E-4*S)*S-7.53E-4*P

      RETURN
      END


      REAL FUNCTION CPSW(S,T,P0)
C ****************************
C UNITS:
C       PRESSURE        P0       DECIBARS
C       TEMPERATURE     T        DEG CELSIUS (IPTS-68)
C       SALINITY        S        (IPSS-78)
C       SPECIFIC HEAT   CPSW     J/(KG DEG C)
C********************************************************
C REF: MILLERO ET AL,1973,JGR,78,4499-4507
C       MILLERO ET AL, UNESCO REPORT NO. 38 1981 PP. 99-188.
C PRESSURE VARIATION FROM LEAST SQUARES POLYNOMIAL
C DEVELOPED BY FOFONOFF 1980.
C CHECK VALUE: CPSW = 3849.500 J/(KG DEG. C) FOR S = 40 (IPSS-78),
C T = 40 DEG C, P0= 10000 DECIBARS
C   SCALE PRESSURE TO BARS
      P=P0/10.
C**************************
C SQRT SALINITY FOR FRACTIONAL TERMS
      SR = SQRT(ABS(S))
C SPECIFIC HEAT CP0 FOR P=0 (MILLERO ET AL ,UNESCO 1981)
      A = (-1.38385E-3*T+0.1072763)*T-7.643575
      B = (5.148E-5*T-4.07718E-3)*T+0.1770383
      C = (((2.093236E-5*T-2.654387E-3)*T+0.1412855)*T-
     .   3.720283)*T+4217.4
      CP0 = (B*SR + A)*S + C
C CP1 PRESSURE AND TEMPERATURE TERMS FOR S = 0
      A = (((1.7168E-8*T+2.0357E-6)*T-3.13885E-4)*T+1.45747E-2)*T-
     .   0.49592
      B = (((2.2956E-11*T-4.0027E-9)*T+2.87533E-7)*T-1.08645E-5)*T+
     .   2.4931E-4
      C = ((6.136E-13*T-6.5637E-11)*T+2.6380E-9)*T-5.422E-8
      CP1 = ((C*P+B)*P+A)*P
C CP2 PRESSURE AND TEMPERATURE TERMS FOR S > 0
      A = (((-2.9179E-10*T+2.5941E-8)*T+9.802E-7)*T-1.28315E-4)*T+
     .   4.9247E-3
      B = (3.122E-8*T-1.517E-6)*T-1.2331E-4
      A = (A+B*SR)*S
      B = ((1.8448E-11*T-2.3905E-9)*T+1.17054E-7)*T-2.9558E-6
      B = (B+9.971E-8*SR)*S
      C = (3.513E-13*T-1.7682E-11)*T+5.540E-10
      C = (C-1.4300E-12*T*SR)*S
      CP2 = ((C*P+B)*P+A)*P
C SPECIFIC HEAT RETURN
      CPSW = CP0 + CP1 + CP2

      RETURN
      END


      REAL FUNCTION ATG(S,T,P)
C ****************************
C ADIABATIC TEMPERATURE GRADIENT DEG C PER DECIBAR
C REF: BRYDEN,H.,1973,DEEP-SEA RES.,20,401-408
C UNITS:
C       PRESSURE        P        DECIBARS
C       TEMPERATURE     T        DEG CELSIUS (IPTS-68)
C       SALINITY        S        (IPSS-78)
C       ADIABATIC       ATG      DEG. C/DECIBAR
C CHECKVALUE: ATG=3.255976E-4 C/DBAR FOR S=40 (IPSS-78),
C T=40 DEG C,P0=10000 DECIBARS
      DS = S - 35.0
      ATG = (((-2.1687E-16*T+1.8676E-14)*T-4.6206E-13)*P+
     .   ((2.7759E-12*T-1.1351E-10)*DS+((-5.4481E-14*T+
     .   8.733E-12)*T-6.7795E-10)*T+1.8741E-8))*P+
     .   (-4.2393E-8*T+1.8932E-6)*DS+
     .   ((6.6228E-10*T-6.836E-8)*T+8.5258E-6)*T+3.5803E-5

      RETURN
      END


      REAL FUNCTION THETA(S,T0,P0,PR)
C ***********************************
C TO COMPUTE LOCAL POTENTIAL TEMPERATURE AT PR
C USING BRYDEN 1973 POLYNOMIAL FOR ADIABATIC LAPSE RATE
C AND RUNGE-KUTTA 4-TH ORDER INTEGRATION ALGORITHM.
C REF: BRYDEN,H.,1973,DEEP-SEA RES.,20,401-408
C FOFONOFF,N.,1977,DEEP-SEA RES.,24,489-491
C UNITS:
C       PRESSURE        P0       DECIBARS
C       TEMPERATURE     T0       DEG CELSIUS (IPTS-68)
C       SALINITY        S        (IPSS-78)
C       REFERENCE PRS   PR       DECIBARS
C       POTENTIAL TMP.  THETA    DEG CELSIUS
C CHECKVALUE: THETA= 36.89073 C,S=40 (IPSS-78),T0=40 DEG C,
C P0=10000 DECIBARS,PR=0 DECIBARS
C
C      SET-UP INTERMEDIATE TEMPERATURE AND PRESSURE VARIABLES
      P=P0
      T=T0
C**************
      H = PR - P
      XK = H*ATG(S,T,P)
      T = T + 0.5*XK
      Q = XK
      P = P + 0.5*H
      XK = H*ATG(S,T,P)
      T = T + 0.29289322*(XK-Q)
      Q = 0.58578644*XK + 0.121320344*Q
      XK = H*ATG(S,T,P)
      T = T + 1.707106781*(XK-Q)
      Q = 3.414213562*XK - 4.121320344*Q
      P = P + 0.5*H
      XK = H*ATG(S,T,P)
      THETA = T + (XK-2.0*Q)/6.0

      RETURN
      END


      REAL FUNCTION SVEL(S,T,P0)
C *******************************
C SOUND SPEED SEAWATER CHEN AND MILLERO 1977,JASA,62,1129-1135
C UNITS:
C       PRESSURE        P0       DECIBARS
C       TEMPERATURE     T        DEG CELSIUS (IPTS-68)
C       SALINITY        S        (IPSS-78)
C       SOUND SPEED     SVEL     METERS/SECOND
C CHECKVALUE: SVEL=1731.995 M/S, S=40 (IPSS-78),T=40 DEG C,P=10000 DBAR
C
      EQUIVALENCE (A0,B0,C0),(A1,B1,C1),(A2,C2),(A3,C3)
C
C   SCALE PRESSURE TO BARS
      P=P0/10.
C**************************
      SR = SQRT(ABS(S))
C S**2 TERM
      D = 1.727E-3 - 7.9836E-6*P
C S**3/2 TERM
      B1 = 7.3637E-5 +1.7945E-7*T
      B0 = -1.922E-2 -4.42E-5*T
      B = B0 + B1*P
C S**1 TERM
      A3 = (-3.389E-13*T+6.649E-12)*T+1.100E-10
      A2 = ((7.988E-12*T-1.6002E-10)*T+9.1041E-9)*T-3.9064E-7
      A1 = (((-2.0122E-10*T+1.0507E-8)*T-6.4885E-8)*T-1.2580E-5)*T+
     .     9.4742E-5
      A0 = (((-3.21E-8*T+2.006E-6)*T+7.164E-5)*T-1.262E-2)*T+1.389
      A = ((A3*P+A2)*P+A1)*P+A0
C S**0 TERM
      C3 = (-2.3643E-12*T+3.8504E-10)*T-9.7729E-9
      C2 = (((1.0405E-12*T-2.5335E-10)*T+2.5974E-8)*T-1.7107E-6)*T+
     .     3.1260E-5
      C1 = (((-6.1185E-10*T+1.3621E-7)*T-8.1788E-6)*T+6.8982E-4)*T+
     .     0.153563
      C0 = ((((3.1464E-9*T-1.47800E-6)*T+3.3420E-4)*T-5.80852E-2)*T+
     .     5.03711)*T+1402.388
      C = ((C3*P+C2)*P+C1)*P+C0
C SOUND SPEED RETURN
      SVEL = C + (A+B*SR+D*S)*S

      RETURN
      END


C---------------------------------------------------------------------
C Subroutine Name:  nos_ofs_write_netCDF_river_ROMS
C
C Purpose:  This subroutine is used to generated netCDF river 
C           forcing file for ROMS fromdata set of river discharge, 
C           temperature, salinity, and dissolved oxygen.
C
C Current contact:   Aijun Zhang
C         Org:  NOS/CO-OPS/OD   Phone:  301-713-2890 ext. 127
C                    aijun.zhang@Noaa.gov
C Attributes:
C  Language:  Fortran
C  Computer:  DEW/MIST at NCEP
C
C  Compile command:
C
C Input Data files:
C
C
C Input Parameters:
C        GRIDFILE       : Model Grid file name of the OFS
C        NETCDF_FILE    : File name for output
C        xi_rho_len     : dimension size in x-direction/longitude
C        eta_rho_len    : dimension size in y-direction/latitude
C        s_rho_len      : dimension size of vertical coordinate
C        time_len       : length of river data time series
C        river_len      : number of locations to specify river input
C        base_date      : reference date of model simulation time
C        river_names    : River long name
C        river          : river identification number
C       river_Xposition : river XI-position at RHO-points
C       river_Eposition : river ETA-position at RHO-points
C       river_direction : river runoff direction. 0 along xi/x axis; 1 along eta/y axis   
C         river_flag    : river runoff tracer flag; 0 all tracers are off; 1: only temperature is on
C                         2: only salinity is on; 3: both temperature and salinity are on.
C        river_sign     : 
C        river_Vshape   : river runoff mass transport verticaldistribution/profile
C        river_time     : river runoff time
C      river_transport  : river runoff vertically integrated mass transport
C       river_temp      : river runoff potential temperature
C       river_salt      : river runoff salinity
C       river_pass         
C       river_Oxyg      : river runoff dissolved oxygen concentration
C
C Output files:
C   netcdf_file         : Output netCDF file name
C----------------------------------------------------------------------------------
      
      subroutine nos_ofs_write_netCDF_river_ROMS(GRIDFILE,netcdf_file,
     .  ncid,xi_rho_len,eta_rho_len,s_rho_len,time_len,river_len,
     .  base_date,river_names,river,river_Xposition,river_Eposition,
     .  river_direction,river_flag,river_sign,river_Vshape,river_time,
     .  river_transport,river_temp,river_salt,river_pass,
     .  river_diso,globalstr,ioxyg)
      parameter (NTMAX=100000)
      include 'netcdf.inc'
      CHARACTER*120 TEXT,CNAME,netcdf_file,GRIDFILE,CHRR
      INTEGER LEN,LCH
      INTEGER base_date(4),intval(4),CORNER(4),COUNT(4)
      CHARACTER(LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      character globalstr(9)*120
      integer iret
      integer ncid

C dimension ids
      integer xi_rho_dim
      integer xi_u_dim
      integer xi_v_dim
      integer eta_rho_dim
      integer eta_u_dim
      integer eta_v_dim
      integer s_rho_dim
      integer river_dim
      integer time_dim
      integer charlength_dim
C dimension lengths
      integer xi_rho_len
      integer xi_u_len
      integer xi_v_len
      integer eta_rho_len
      integer eta_u_len
      integer eta_v_len
      integer s_rho_len
      integer river_len
      integer time_len
C variable ids
      integer river_names_id
      integer river_id
      integer river_Xposition_id
      integer river_Eposition_id
      integer river_direction_id
      integer river_flag_id
      integer river_sign_id
      integer river_Vshape_id
      integer river_time_id
      integer river_transport_id
      integer river_temp_id
      integer river_salt_id
      integer river_pass_id
      integer river_diso_id

C data variables
      character*46 river_names(river_len)
      integer river(river_len)
      integer river_Xposition(river_len)
      integer river_Eposition(river_len)
      integer river_direction(river_len)
      integer river_flag(river_len)
      real river_sign(river_len)
      real river_Vshape(river_len, s_rho_len)
      real river_time(NTMAX)
      real river_transport(river_len,NTMAX)
      real river_temp(river_len,s_rho_len,NTMAX)
      real river_salt(river_len,s_rho_len,NTMAX)
      real river_pass(river_len,s_rho_len,NTMAX)
      real river_diso(river_len,s_rho_len,NTMAX)

C attribute vectors for ROMS
      xi_u_len=xi_rho_len-1
      xi_v_len=xi_rho_len
      eta_u_len=eta_rho_len
      eta_v_len=eta_rho_len-1
      s_w_len=s_rho_len+1

C enter define mode
      iret = nf_create(trim(netcdf_file),NF_CLOBBER,ncid)
      call check_err(iret)

C define dimensions
      iret=nf_def_dim(ncid,'xi_rho',xi_rho_len,xi_rho_dim)
      call check_err(iret)
      iret=nf_def_dim(ncid,'xi_u',xi_u_len,xi_u_dim)
      call check_err(iret)
      iret=nf_def_dim(ncid,'xi_v',xi_v_len,xi_v_dim)
      call check_err(iret)
      iret=nf_def_dim(ncid,'eta_rho',eta_rho_len,eta_rho_dim)
      call check_err(iret)
      iret=nf_def_dim(ncid,'eta_u',eta_u_len,eta_u_dim)
      call check_err(iret)
      iret=nf_def_dim(ncid,'eta_v',eta_v_len,eta_v_dim)
      call check_err(iret)
      iret=nf_def_dim(ncid,'s_rho',s_rho_len,s_rho_dim)
      call check_err(iret)
      iret=nf_def_dim(ncid,'river',river_len,river_dim)
      call check_err(iret)
      iret=nf_def_dim(ncid,'charlength',46,charlength_dim)
      call check_err(iret)
      iret=nf_def_dim(ncid,'time',time_len,time_dim)
      call check_err(iret)

C define variables
      intval(2)=river_dim
      intval(1)=charlength_dim
      iret=nf_def_var(ncid,'river_names',NF_CHAR,2,intval, 
     .     river_names_id)
      call check_err(iret)

      intval(1)=river_dim
      iret=nf_def_var(ncid,'river',NF_INT,1,intval,river_id)
      call check_err(iret)

      iret=nf_def_var(ncid,'river_Xposition',NF_INT,1,intval,
     .     river_Xposition_id)
      call check_err(iret)

      iret=nf_def_var(ncid,'river_Eposition',NF_INT,1,intval,
     .     river_Eposition_id)
      call check_err(iret)

      iret=nf_def_var(ncid,'river_direction',NF_INT,1,intval,
     .     river_direction_id)
      call check_err(iret)

      iret=nf_def_var(ncid,'river_flag',NF_INT,1,intval,
     .     river_flag_id)
      call check_err(iret)
      
      iret=nf_def_var(ncid,'river_sign',NF_REAL,1,intval, 
     .     river_sign_id)
      call check_err(iret)

      intval(2)=s_rho_dim
      intval(1)=river_dim
      iret=nf_def_var(ncid,'river_Vshape',NF_REAL,2,intval,
     .     river_Vshape_id)
      call check_err(iret)

      intval(1)=time_dim
      iret=nf_def_var(ncid,'river_time',NF_REAL,1,intval, 
     .     river_time_id)
      call check_err(iret)

      intval(2)=time_dim
      intval(1)=river_dim
      iret=nf_def_var(ncid,'river_transport',NF_REAL,2,intval,
     .     river_transport_id)
      call check_err(iret)

      intval(3)=time_dim
      intval(2)=s_rho_dim
      intval(1)=river_dim
      iret=nf_def_var(ncid,'river_temp',NF_REAL,3,intval, 
     .     river_temp_id)
      call check_err(iret)

      iret=nf_def_var(ncid,'river_salt',NF_REAL,3,intval, 
     .     river_salt_id)
      call check_err(iret)

      iret=nf_def_var(ncid,'river_pass',NF_REAL,3,intval,
     .     river_pass_id)
      call check_err(iret)

      if(ioxyg.eq.1) then
        iret=nf_def_var(ncid,'river_Oxyg',NF_REAL,3,intval,
     .       river_diso_id)
        call check_err(iret)
      endif

C assign attributes
      chrr='River Names'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_names_id,'long_name',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='char*46'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_names_id,'units',lch,trim(chrr))
      call check_err(iret)

      chrr='river runoff identification number'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_id,'long_name',lch,trim(chrr)) 
      call check_err(iret)
      chrr='nondimensional'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_id,'units',lch,trim(chrr))
      call check_err(iret)
      chrr='river, scalar'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_id,'field',lch,trim(chrr))
      call check_err(iret)

      chrr='river XI-position at RHO-points'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_Xposition_id,'long_name',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='nondimensional'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_Xposition_id,'units',lch,
     .     trim(chrr))
      call check_err(iret)
      iret=nf_put_att_int(ncid,river_Xposition_id,'valid_min',
     .     NF_INT,1,1)
      call check_err(iret)
      iret=nf_put_att_int(ncid,river_Xposition_id,'valid_max', 
     .     NF_INT,1,xi_u_len)
      call check_err(iret)
      chrr='river_Xposition, scalar'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_Xposition_id,'field',lch,
     .     trim(chrr))
      call check_err(iret)

      chrr='river ETA-position at RHO-points'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_Eposition_id,'long_name',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='nondimensional'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_Eposition_id,'units',lch,
     .     trim(chrr))
      call check_err(iret)
      iret=nf_put_att_int(ncid,river_Eposition_id,'valid_min',
     .     NF_INT,1,1)
      call check_err(iret)
      iret=nf_put_att_int(ncid,river_Eposition_id,'valid_max',
     .     NF_INT,1,eta_v_len)
      call check_err(iret)
      chrr='river_Eposition, scalar'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_Eposition_id,'field',lch,
     .     trim(chrr))
      call check_err(iret)

      chrr='river runoff direction'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_direction_id,'long_name',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='nondimensional'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_direction_id,'units',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='river_direction, scalar'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_direction_id,'field',lch,
     .     trim(chrr))
      call check_err(iret)

      chrr='river runoff tracer flag'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_flag_id,'long_name',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='all tracers are off'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_flag_id,'option_0',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='only temperature is on'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_flag_id,'option_1',lch, 
     .     trim(chrr))
      call check_err(iret)
      chrr='only salinity is on'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_flag_id,'option_2',lch, 
     .     trim(chrr))
      call check_err(iret)
      chrr='both temperature and salinity are on'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_flag_id,'option_3',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='nondimensional'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_flag_id,'units',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='river_flag, scalar'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_flag_id,'field',lch,
     .     trim(chrr))
      call check_err(iret)

      chrr='River mass transport ambient sign'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_sign_id,'long_name',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='nondimensional'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_sign_id,'units',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='river_sign, scalar, series'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_sign_id,'field',lch,
     .     trim(chrr))
      call check_err(iret)

      chrr='river runoff mass transport vertical profile'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_Vshape_id,'long_name',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='nondimensional'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_Vshape_id,'units',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='river_Vshape, scalar'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_Vshape_id,'field',lch,
     .     trim(chrr))
      call check_err(iret)

      chrr='river runoff time'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_time_id,'long_name',lch,
     .     trim(chrr))
      call check_err(iret)

      WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)') 'days since ',base_date(1),
     .  '-',base_date(2),'-',base_date(3),' ',base_date(4),':00:00'
      LCH=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,river_time_id,'units',lch,trim(text))
      call check_err(iret)
      chrr='river_time, scalar, series'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid, river_time_id, 'field',lch,
     .     trim(chrr))
      call check_err(iret)

      chrr='river runoff vertically integrated mass transport'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_transport_id,'long_name',lch, 
     .     trim(chrr))
      call check_err(iret)
      chrr='meter3 second-1'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_transport_id,'units',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='river_transport, scalar, series'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_transport_id,'field',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='river_time'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_transport_id,'time',lch,
     .     trim(chrr))
      call check_err(iret)

      chrr='river runoff potential temperature'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_temp_id,'long_name',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='Celsius'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_temp_id,'units',lch,trim(chrr)) 
      call check_err(iret)
      chrr='river_temp, scalar, series'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_temp_id,'field',lch,trim(chrr))
      call check_err(iret)
      chrr='river_time'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_temp_id,'time',lch,trim(chrr))
      call check_err(iret)

      chrr='river runoff salinity'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_salt_id,'long_name',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='PSU'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_salt_id,'units',lch,trim(chrr))
      call check_err(iret)
      chrr='river_salt, scalar, series'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_salt_id,'field',lch,trim(chrr))
      call check_err(iret)
      chrr='river_time'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_salt_id,'time',lch,trim(chrr))
      call check_err(iret)

      chrr='River runoff passive tracer'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_pass_id,'long_name',lch,
     .     trim(chrr))
      call check_err(iret)
      chrr='PPM'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_pass_id,'units',lch,trim(chrr))
      call check_err(iret)
      chrr='river_pass, scalar, series'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_pass_id,'field',lch,trim(chrr))
      call check_err(iret)
      chrr='river_time'
      lch=len_trim(chrr)
      iret=nf_put_att_text(ncid,river_pass_id,'time',lch,trim(chrr))
      call check_err(iret)

      if(ioxyg .eq. 1) then
        chrr='River runoff oxygen concentration'
        lch=len_trim(chrr)
        iret=nf_put_att_text(ncid,river_diso_id,'long_name',lch,
     .       trim(chrr))
        call check_err(iret)
        chrr='mmole/m^3'
        lch=len_trim(chrr)
        ire =nf_put_att_text(ncid,river_diso_id,'units',lch,trim(chrr))
        call check_err(iret)
        chrr='river_Oxyg, scalar, series'
        lch=len_trim(chrr)
        iret=nf_put_att_text(ncid,river_diso_id,'field',lch,trim(chrr))
        call check_err(iret)
        chrr='river_time'
        lch=len_trim(chrr)
        iret=nf_put_att_text(ncid,river_diso_id,'time',lch,trim(chrr))
        call check_err(iret)
      endif

C Global Attributes
      TEXT=trim(globalstr(1))
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,NF_GLOBAL,'type',LEN,TRIM(TEXT))
      TEXT=trim(globalstr(2))
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,NF_GLOBAL,'title',LEN,TRIM(TEXT))
      TEXT=trim(globalstr(3))
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,NF_GLOBAL,'data_source',LEN,TRIM(TEXT))
      TEXT=trim(globalstr(4))
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,NF_GLOBAL,'Temp_source',LEN,TRIM(TEXT))
      TEXT=trim(globalstr(5))
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,NF_GLOBAL,'model_grid_file', 
     .     LEN,TRIM(TEXT))
      TEXT=trim(globalstr(6))
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,NF_GLOBAL,'output_file',LEN,TRIM(TEXT))
      TEXT=trim(globalstr(7))
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,NF_GLOBAL,'source_code',LEN,TRIM(TEXT))
      TEXT=trim(globalstr(8))
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,NF_GLOBAL,'history',LEN,TRIM(TEXT))
      TEXT=trim(globalstr(9))
      LEN=LEN_TRIM(TEXT)
      iret=nf_put_att_text(ncid,NF_GLOBAL,'reference',LEN,TRIM(TEXT))

      iret=nf_enddef(ncid)
      call check_err(iret)

C------------------------------------------------------
C    End of variable definition, leave define mode
C    write associate variables
C------------------------------------------------------
      CORNER(1)=1
      CORNER(2)=1
      COUNT(1)=46
      COUNT(2)=river_len
      iret=nf_put_vara_text(ncid,river_names_id,CORNER,COUNT,
     .     river_names)
      call check_err(iret)

      CORNER(1)=1
      COUNT(1)=river_len
      iret=nf_put_vara_int(ncid,river_id,CORNER,COUNT,river)
      call check_err(iret)
      
      iret=nf_put_vara_int(ncid,river_Xposition_id,CORNER,COUNT,
     .     river_Xposition)
      call check_err(iret)

      iret=nf_put_vara_int(ncid,river_Eposition_id,CORNER,COUNT,
     .     river_Eposition)
      call check_err(iret)

      iret=nf_put_vara_int(ncid,river_direction_id,CORNER,COUNT,
     .     river_direction)
      call check_err(iret)

      iret=nf_put_vara_int(ncid,river_flag_id,CORNER,COUNT,
     .     river_flag)
      call check_err(iret)
     
      CORNER(1)=1
      CORNER(2)=1
      COUNT(1)=river_len
      COUNT(2)=s_rho_len
      iret=nf_put_vara_real(ncid,river_Vshape_id,CORNER,COUNT,
     .     river_Vshape)
      call check_err(iret)

      CORNER(1)=1
      COUNT(1)=time_len
      iret=nf_put_vara_real(ncid,river_time_id,CORNER,COUNT,
     .     river_time)
      call check_err(iret)

      CORNER(1)=1
      CORNER(2)=1
      COUNT(1)=river_len
      COUNT(2)=time_len
      iret=nf_put_vara_real(ncid,river_transport_id,CORNER,COUNT,
     .     river_transport)
      call check_err(iret)

      CORNER(1)=1
      CORNER(2)=1
      CORNER(3)=1
      COUNT(1)=river_len
      COUNT(2)=s_rho_len
      COUNT(3)=time_len
      iret=nf_put_vara_real(ncid,river_temp_id,CORNER,COUNT,
     .     river_temp)
      call check_err(iret)

      iret=nf_put_vara_real(ncid,river_salt_id,CORNER,COUNT,
     .     river_salt)
      call check_err(iret)

      if(ioxyg.eq.1) then
        iret=nf_put_vara_real(ncid,river_diso_id,CORNER,COUNT,
     .       river_diso)
        call check_err(iret)
      endif
      
      iret=nf_close(ncid)
      call check_err(iret)

      return
      end


C---------------------------------------------------------------------
C Subroutine Name:  nos_ofs_write_netCDF_river_FVCOM
C
C Directory:  NOSOFS-PATH/sorc/nos_ofs_create_forcing_river.fd
C
C Purpose:  This subroutine is used to generated netCDF river 
C           forcing file for FVCOM from data set of river discharge, 
C           temperature, and salinity.
C
C Current contact:   Aijun Zhang
C         Org:  NOS/CO-OPS/OD   Phone:  301-713-2890 ext. 127
C                    aijun.zhang@Noaa.gov
C Attributes:
C  Language:  Fortran
C  Computer:  DEW/MIST at NCEP
C
C  Compile command:
C
C Input Data files:
C
C
C Input Parameters:
C        GRIDFILE       : Model Grid file name of the OFS
C        NETCDF_FILE    : File name for output
C        time_len       : length of river data time series
C        river_len      : number of locations to specify river input
C        base_date      : reference date of model simulation time
C        river_names    : River long name
C        river_time     : river runoff time
C      river_flux  : river runoff vertically integrated mass transport
C       river_temp      : river runoff potential temperature
C       river_salt      : river runoff salinity
C
C Output files:
C   netcdf_file         : Output netCDF file name
C----------------------------------------------------------------------
      
      subroutine nos_ofs_write_netCDF_river_FVCOM(GRIDFILE,netcdf_file,
     .  ncid,imode,time_len,river_len,base_date,river_names,Itime,
     .  Itime2,Times,river_flux,river_temp,river_salt,globalstr)

      include 'netcdf.inc'
      CHARACTER*120 TEXT,CNAME,netcdf_file,GRIDFILE
      INTEGER LEN,LCH
      INTEGER base_date(4),intval(4),CORNER(4),COUNT(4)
      CHARACTER(LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      character globalstr(9)*120
      parameter (namelen_len=46)
      parameter (DateStrLen_len=26)

      integer iret
      integer ncid
C dimension ids
      integer river_dim
      integer time_dim
      integer namelen_dim
      integer DateStrLen_dim
C dimension lengths
      integer river_len
      integer time_len
C variable ids
      integer Itime_id
      integer Itime2_id
      integer Times_id
      integer river_names_id
      integer river_time_id
      integer river_flux_id
      integer river_temp_id
      integer river_salt_id

C data variables
      character*46 river_names(river_len)
      character*26 Times(time_len)
      integer Itime(time_len)
      integer Itime2(time_len)
      real river_time(time_len)
      real river_flux(river_len,time_len)
      real river_temp(river_len,time_len)
      real river_salt(river_len,time_len)
      real river_pass(river_len,time_len)

C Save variable
      save time_dim
      save Itime_id,Itime2_id,Times_id
      save river_time_id,river_flux_id,river_temp_id,river_salt_id

      DO I=1,time_len
        river_time(I)=Itime(I)+Itime2(I)/86400000.0
      ENDDO

      if(imode.eq.1) then
C Enter define mode
        netcdf_file=trim(adjustL(netcdf_file))
        iret=nf_create(trim(netcdf_file),NF_CLOBBER,ncid)
        call check_err(iret)

C Define dimensions
        iret=nf_def_dim(ncid,'rivers',river_len,river_dim)
        call check_err(iret)
        iret=nf_def_dim(ncid,'time',NF_UNLIMITED,time_dim)
        call check_err(iret)
        iret=nf_def_dim(ncid,'namelen',namelen_len,namelen_dim)
        call check_err(iret)
        iret=nf_def_dim(ncid,'DateStrLen',26,DateStrLen_dim)
        call check_err(iret)

C Define variables
        intval(2)=river_dim
        intval(1)=namelen_dim
        iret=nf_def_var(ncid,'river_names',NF_CHAR,2,intval, 
     .       river_names_id)
        call check_err(iret)

        intval(1)=time_dim
        iret=nf_def_var(ncid,'Itime',NF_INT,1,intval,Itime_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'Itime2',NF_INT,1,intval,Itime2_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'time',NF_REAL,1,intval,river_time_id)
        call check_err(iret)

        intval(2)=time_dim
        intval(1)=DateStrLen_dim
        iret=nf_def_var(ncid,'Times',NF_CHAR,2,intval,Times_id)
        call check_err(iret)
 
        intval(2)=time_dim
        intval(1)=river_dim
        iret=nf_def_var(ncid,'river_flux',NF_REAL,2,intval,
     .       river_flux_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'river_temp',NF_REAL,2,intval, 
     .       river_temp_id)
        call check_err(iret)
        iret=nf_def_var(ncid,'river_salt',NF_REAL,2,intval, 
     .       river_salt_id)
        call check_err(iret)

C Assign attributes
        text='River Names'
        lch=len_trim(text)
        iret=nf_put_att_text(ncid,river_names_id,'long_name',lch,
     .       trim(text))
        call check_err(iret)
        text='char*46'
        lch=len_trim(text)
        iret=nf_put_att_text(ncid,river_names_id,'units',lch,
     .       trim(text))
        call check_err(iret)

        TEXT='Julian Day from Base Date'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime_id,'long_name',lch,
     .       TRIM(TEXT))
        call check_err(iret)
        WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)') 'days since ',
     .    base_date(1),'-',base_date(2),'-',base_date(3),' ',
     .    base_date(4),':00:00'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime_id,'units',lch,TRIM(TEXT))
        call check_err(iret)
        TEXT='modified julian day(MJD)'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime_id,'format',lch,TRIM(TEXT))
        call check_err(iret)
        TEXT='UTC'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime_id,'time_zone',lch,
     .       TRIM(TEXT))
        call check_err(iret)

        TEXT='mseconds from integer day'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime2_id,'long_name',lch,
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT='msec since 00:00:00'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime2_id,'units',lch,TRIM(TEXT))
        TEXT='modified julian day(MJD)'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime2_id,'format',lch,TRIM(TEXT))
        call check_err(iret)
        TEXT='UTC'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Itime2_id,'time_zone',lch,
     .       TRIM(TEXT))
        call check_err(iret)

        TEXT='Calendar Date'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Times_id,'long_name',lch,
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT='String: Calendar Time'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Times_id,'format',lch,TRIM(TEXT))
        call check_err(iret)
        TEXT='UTC'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,Times_id,'time_zone',lch,
     .       TRIM(TEXT))
        call check_err(iret)

        TEXT='river runoff time'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_time_id,'long_name',lch,
     .       TRIM(TEXT))
        call check_err(iret)
        WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)') 'days since ',
     .    base_date(1), '-',base_date(2),'-',base_date(3),' ',
     .    base_date(4),':00:00'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_time_id,'units',lch,
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT='river_time, scalar, series'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_time_id,'field',lch,
     .       TRIM(TEXT))
        call check_err(iret)

        TEXT='river runoff volume flux'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_flux_id,'long_name',lch, 
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT='m^3s^-1'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_flux_id,'units',lch,
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT='river_transport, scalar, series'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_flux_id,'field',lch,
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT='m^3s^-1'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_flux_id,'time',lch,
     .       TRIM(TEXT))
        call check_err(iret)
      
        TEXT='river runoff temperature'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_temp_id,'long_name',lch, 
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT='Celsius'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_temp_id,'units',lch,
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT='river_temp, scalar, series'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_temp_id,'field',lch,
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT='river_time'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_temp_id,'time',lch,
     .       TRIM(TEXT))
        call check_err(iret)

        TEXT='river runoff salinity'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_salt_id,'long_name',lch, 
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT='PSU'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_salt_id,'units',lch,
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT='river_salt, scalar, series'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_salt_id,'field',lch,
     .       TRIM(TEXT)) 
        call check_err(iret)
        TEXT='river_time'
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,river_salt_id,'time',lch,
     .       TRIM(TEXT)) 
        call check_err(iret)

C Global Attributes
        TEXT=trim(globalstr(1))
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'type',lch,TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(2))
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'title',lch,TRIM(TEXT)) 
        call check_err(iret)
        TEXT=trim(globalstr(3))
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'data_source',lch, 
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(4))
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'Temp_source',lch, 
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(5))
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'model_grid_file',lch,
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(6))
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'output_file',lch,
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(7))
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'source_code',lch, 
     .       TRIM(TEXT))
        call check_err(iret)     
        TEXT=trim(globalstr(8))
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'history',lch, 
     .       TRIM(TEXT))
        call check_err(iret)
        TEXT=trim(globalstr(9))
        lch=LEN_TRIM(TEXT)
        iret=nf_put_att_text(ncid,NF_GLOBAL,'reference',lch,
     .       TRIM(TEXT))
        call check_err(iret)

        iret=nf_enddef(ncid)
        call check_err(iret)
C------------------------------------------------------
C    end of variable definition, leave define mode
C    write associate variables
C------------------------------------------------------
        CORNER(1)=1
        CORNER(2)=1
        COUNT(1)=46
        COUNT(2)=river_len
        iret=nf_put_vara_text(ncid,river_names_id,CORNER,COUNT,
     .       river_names)
        call check_err(iret)
      elseif(imode.eq.2) then 
        iret=nf_inq_dimlen(ncid,time_dim,jtime)
        call check_err(iret)
        jtime=jtime+1

        CORNER(1)=jtime 
        COUNT(1)=1
        iret=nf_put_vara_int(ncid,Itime_id,CORNER,COUNT,Itime)
        call check_err(iret)
        iret=nf_put_vara_int(ncid,Itime2_id,CORNER,COUNT,Itime2)
        call check_err(iret)

        CORNER(1)=1
        CORNER(2)=jtime
        COUNT(1)=26
        COUNT(2)=1
        iret=nf_put_vara_text(ncid,Times_id,CORNER,COUNT,Times)
        call check_err(iret)

        CORNER(1)=jtime
        COUNT(1)=time_len
        iret=nf_put_vara_real(ncid,river_time_id,CORNER,COUNT,
     .       river_time)
        call check_err(iret)

        CORNER(1)=1
        CORNER(2)=jtime
        COUNT(1)=river_len
        COUNT(2)=time_len
        iret=nf_put_vara_real(ncid,river_flux_id,CORNER,COUNT,
     .       river_flux)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,river_temp_id,CORNER,COUNT,
     .       river_temp)
        call check_err(iret)
        iret=nf_put_vara_real(ncid,river_salt_id,CORNER,COUNT,
     .       river_salt)
        call check_err(iret)

      elseif(imode.eq.3) then
        iret=nf_close(ncid)
        call check_err(iret)
      endif

      return
      end
       

      subroutine check_err(iret)
      integer iret
      include 'netcdf.inc'

      if (iret .ne. NF_NOERR) then
        write(*,*)  nf_strerror(iret)
        stop
      endif

      return
      end
