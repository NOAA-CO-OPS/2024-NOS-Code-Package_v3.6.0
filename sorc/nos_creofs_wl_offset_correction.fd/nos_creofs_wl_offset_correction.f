!       Program Name:  nos_creofs_wl_offset_correction.f
!
!       Technical Contact(s):   Name:  Aijun Zhang
!                               Org:   NOS/CO-OPS/OD
!                               Phone: 240-533-0591
!                               E-Mail: aijun.zhang@noaa.gov
!
!       Abstract:  this program is used to ajust model water level at 
!       open bounday with observed data in nos_ofs_nowcast_forecast.sh
!
!       Usage: nos_creofs_wl_offset_correction < tmpsta.ctl
!
!       History Log:
!           03/27/2019 
!
!       Input files: (from tmpsta.ctl)
!                  creofs.wl.correction.ctl  
!                  creofs.hgrid.ll    
!                  creofs.station.info
!                  creofs.wl.correction.old                    
!                  BUFF water level file from
!                  /gpfs/tp1/nco/ops/dcom/us007003
!
!       Output files: 
!                 creofs.fields.wl_correction.dat 
!               creofs.stations.wl_correction.dat
!

      parameter (NMAX=9000)
      include 'netcdf.inc'
      character*200 OFS,GRIDFILE_LL,STA_NETCDF_CTL,CORRECTION_OLD
      character*200 FIN,FOUT,NOSBUFR,USGSBUFR,NOSWLDIR,PREFIXNOS
      character*200 BUFFER,CMD*132,VNAME,ANAME
      character*200 START_TIME,END_TIME,BUFRFILE,TMPFILE,COMOUT00
      real*8 jday_start,jbase_date,JULIAN,yearb,monthb,dayb,hourb
      real*8 jday,jday0,jdays,jdaye
      integer, allocatable :: NTR(:)
      integer, allocatable :: NTR_T(:)
      integer, allocatable :: NTR_S(:)
      integer, allocatable :: NODE_STA(:)
      real, allocatable :: RTIME(:,:)
      real, allocatable :: RTIME_T(:,:)
      real, allocatable :: RTIME_S(:,:)
      real, allocatable :: WL_OBS(:,:)
      real, allocatable :: SWL_OBS(:,:)
      real, allocatable :: T_OBS(:,:) 
      real, allocatable :: S_OBS(:,:) 
      real, allocatable :: lonSTA(:) 
      real, allocatable :: latSTA(:) 
      real, allocatable :: DATUM(:)
      real, allocatable :: AVG_OBS(:)
      real, allocatable :: weights(:,:)
      real, allocatable :: WL_OFFSET(:)
      real, allocatable :: WL_OFFSET_OLD(:)
      real, allocatable :: WL_OFFSET_NODE(:)

C  for model station NetCDF
      real, allocatable :: lon(:)
      real, allocatable :: lat(:)
      real, allocatable :: time_model(:)
      real, allocatable :: WL_MODEL(:,:)
      real, allocatable :: AVG_MODEL(:)
      real, allocatable :: tmp4d  (:,:,:,:)
      integer dimids(5),COUNT(4),DIMS(4)
      LOGICAL FEXIST,USGS_L
      real*4, allocatable :: oned1(:)
      real*4, allocatable :: oned2(:)

      character*10,allocatable :: NOS_ID(:)
      character*10,allocatable :: NOS_ID_OLD(:)
      character*5,allocatable :: NWS_ID(:)
      character*4,allocatable :: AGENCY_ID(:)
      character(len=200) :: arg
      INTEGER BASE_DATE(4)
      INTEGER DAYS_PER_MONTH(12)
      DATA (DAYS_PER_MONTH(i),I=1,12) /
     &31,28,31,30,31,30,31,31,30,31,30,31/ 
! variables for real time observations
      CHARACTER*8  SUBSET,NAME                                  
      character*20 stnbufrid
      Real*8 DATES(5),RTIM(6),RPID
      Real*8 data1(2,500),data2(4,500),data3(5)
      real*8 xlocat(5)
      DATA BMISS /10E10/                                                
C ----------------------------------------------------------------------------
C    process real time observations in BUFR files of NCEP data tank at NWLON stations
C ----------------------------------------------------------------------------
      OFS='creofs'
C      COMOUT00='/ptmp/wx21az/creofs.v1.0.0/dev'
!      call getarg(1, arg)
!      read(arg,'(a)')FIN
!      call getarg(2, arg)
!      read(arg,'(a)') START_TIME
!      call getarg(3, arg)
!      read(arg,'(a)') END_TIME

      read(5,'(a100)')COMOUT00
      read(5,'(a100)')NOSWLDIR
      read(5,'(a100)')NOSBUFR
      read(5,'(a100)')FIN
      read(5,'(a100)')GRIDFILE_LL
      read(5,'(a100)')STA_NETCDF_CTL
      read(5,'(a100)')CORRECTION_OLD
      read(5,'(a100)')END_TIME
      read(END_TIME,'(I4,4I2)')IYRE,IMME,IDDE,IHHE
      read(5,'(a100)')PREFIXNOS
      yearb=IYRE
      monthb=IMME
      dayb=IDDE
      hourb=IHHE   !! do not need minutes to construct ETSS file name
      jdaye=JULIAN(yearb,monthb,dayb,hourb)
      jdays=jdaye-7.0
      call GREGORIAN(jdays,yearb,monthb,dayb,hourb)
      IYR=INT(yearb)
      IMM=int(monthb+0.001)
      IDD=INT(dayb+0.001)
      IHH=INT(hourb+0.001)
      IMN=INT((hourb-IHH)*60+0.1)
      ISEC=0
      IF(ISEC .EQ. 60)THEN
         ISEC=0
         IMN=IMN+1
      ENDIF
      IF(IMN .EQ. 60)THEN
         IMN=0
         IHH=IHH+1
      ENDIF
      IF(IHH .EQ. 24)THEN
         IHH=0
         IDD=IDD+1
         IF(MOD(IYR,4) .EQ. 0)DAYS_PER_MONTH(2)=29   !!    Leap Year
         IF(IDD .GT. DAYS_PER_MONTH(IMM) )THEN
            IDD=IDD - DAYS_PER_MONTH(IMM)
            IMM=IMM+1
            IF(IMM .GT. 12)THEN
               IMM=IMM-12
               IYR=IYR+1
            ENDIF
         ENDIF
      ENDIF
      IYRS=IYR
      IMMS=IMM
      IDDS=IDD
      IHHS=IHH
      IMNS=IMN
      WRITE(START_TIME,'(I4.4,4I2.2)')IYRS,IMMS,IDDS,IHHS
      print *,'start time=',IYRS,IMMS,IDDS,IHHS
      print *,'end time=',IYRE,IMME,IDDE,IHHE
      print *,'starttime=',trim(START_TIME)
      print *,'endtime=',trim(END_TIME)

      base_date(1)=IYRS
      base_date(2)=1
      base_date(3)=1
      base_date(4)=0
      yearb=base_date(1)
      monthb=base_date(2)
      dayb=base_date(3)
      hourb=base_date(4)
      jbase_date=JULIAN(yearb,monthb,dayb,hourb)
      print *,'base_date= ',base_date

      day_start=jdays-jbase_date
      day_end=jdaye-jbase_date
      
      print *,'fin=',trim(FIN)
      OPEN(20,FILE=TRIM(FIN) )
      READ(20,*)NSTA,NNODE
      PRINT *,'NSTA=',NSTA,'NNODE=',NNODE
      allocate(NOS_ID(NSTA) )
      allocate(NOS_ID_OLD(NSTA) )
      allocate (NWS_ID(NSTA))
      allocate (AGENCY_ID(NSTA))
      ALLOCATE( NTR(NSTA) )
      ALLOCATE( NTR_T(NSTA) )
      ALLOCATE( NTR_S(NSTA) )
      allocate ( RTIME(NSTA,NMAX) )
      allocate ( RTIME_T(NSTA,NMAX) )
      allocate ( RTIME_S(NSTA,NMAX) )
      allocate ( WL_OBS(NSTA,NMAX) )
      allocate ( WL_MODEL(NSTA,NMAX) )
      allocate ( T_OBS(NSTA,NMAX) )
      allocate ( S_OBS(NSTA,NMAX) )
      allocate ( DATUM(NSTA))
      allocate ( lonSTA(NSTA))
      allocate ( latSTA(NSTA))
      allocate ( AVG_OBS(NSTA))
      allocate ( AVG_MODEL(NSTA))
      allocate ( NODE_STA(NSTA))
      allocate ( weights(NNODE,NSTA) )
      allocate ( WL_OFFSET(NSTA))
      allocate ( WL_OFFSET_OLD(NSTA))
      allocate ( WL_OFFSET_NODE(NNODE))
C     initiate NTR
      NTR=0
      NTR_T=0
      NTR_S=0
      DO N=1,NSTA
        READ(20,*)IDUMMY,NOS_ID(N),NWS_ID(N),
     1	AGENCY_ID(N),lonSTA(N),latSTA(N),DATUM(N)
      ENDDO 
      DO N=1,NNODE
         READ(20,*)IDUMMY,(weights(N,I),I=1,NSTA)
      ENDDO	 
      CLOSE(20)
      OPEN(20,FILE=TRIM(CORRECTION_OLD) )
        DO N=1,NSTA
          READ(20,*)IDUMMY,NOS_ID_OLD(N),WL_OFFSET_OLD(N)
	  print *,'old offset=',IDUMMY,NOS_ID_OLD(N),WL_OFFSET_OLD(N)
      ENDDO 
      CLOSE(20)


      WRITE(FOUT,201)'WL_',IYRS,IMMS,IDDS,IHHS,
     &   '.dat'  
201     FORMAT(A3,I4.4,3I2.2,A4)
      print *,'fout=',TRIM(adjustL(FOUT))
      FOUT=TRIM(adjustL(FOUT))
      CLOSE(15) 
      OPEN(15,file=TRIM(FOUT) )
      write(15,*) 'ID        lat     lon   year  mon day hour
     *  Disch   Tem SST   Salinity'
      DO IZ=INT(day_start),INT(day_end)
        jday=IZ+jbase_date
        call GREGORIAN(jday,yearb,monthb,dayb,hourb)
        IYR=INT(yearb)
        IMM=int(monthb+0.001)
        IDD=INT(dayb+0.001)
        IHH=INT(hourb+0.001)
        IMN=INT((hourb-IHH)*60+0.1)
        ISEC=0
        IF(ISEC .EQ. 60)THEN
           ISEC=0
           IMN=IMN+1
        ENDIF
        IF(IMN .EQ. 60)THEN
           IMN=0
           IHH=IHH+1
        ENDIF
        IF(IHH .EQ. 24)THEN
          IHH=0
          IDD=IDD+1
          IF(MOD(IYR,4) .EQ. 0)DAYS_PER_MONTH(2)=29   !!    Leap Year
          IF(IDD .GT. DAYS_PER_MONTH(IMM) )THEN
             IDD=IDD - DAYS_PER_MONTH(IMM)
             IMM=IMM+1
             IF(IMM .GT. 12)THEN
                IMM=IMM-12
                IYR=IYR+1
             ENDIF
          ENDIF
        ENDIF
        WRITE(BUFRFILE,700)'/',IYR,IMM,IDD,'/b001/'
700     FORMAT(A1,I4.4,2I2.2,A6)
        BUFRFILE=TRIM(NOSWLDIR)//trim(BUFRFILE)//trim(NOSBUFR) 
        INQUIRE(FILE=trim(BUFRFILE),EXIST=FEXIST)
        print *,'BUFR FILE= ',trim(BUFRFILE)
        IF(FEXIST) THEN
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
          CALL UFBINT(LUNIN,DATES,5,1,IRET,'YEAR MNTH DAYS HOUR MINU')
          IYR = NINT(DATES(1))
          IMM = NINT(DATES(2))
          IDD = NINT(DATES(3))                                           
          IHH = NINT(DATES(4))                                            
          IMN = NINT(DATES(5))                                            
          yearb=IYR
          monthb=IMM
          dayb=IDD
          hourb=REAL(IHH)+REAL(IMN/60.0)   
          dayj=JULIAN(yearb,monthb,dayb,hourb)-jbase_date
!	  print *,'time=',dayj,IYR,IMM,IDD,IHH
          IF (dayj .GE. day_start .and. dayj .LE. day_end) THEN
C  --------------READ THE TIDE GAUGE STATION INFO FROM BUFR FILE------------------
c  AJ 09/15/11 Use different CALL routine in order to handle long station IDs 
          CALL READLC(LUNIN, stnbufrid, 'RPID')
!          CALL UFBINT(LUNIN,rpid,1,1,iret,'RPID')
          CALL UFBINT(LUNIN,DATES,5,1,IRET,'CLAT CLON SELV')
          clat=DATES(1)
          clon=DATES(2)
          selv=DATES(3)
          DO I=1,NSTA 
             IF(trim(stnbufrid) .EQ. TRIM(NWS_ID(I)) )THEN
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
               IF(BUFRFILE(LLEN-4:LLEN) .EQ. 'xx005')THEN      
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,
     &	               'SST1 TMDB AWCK MWCK TPMI')
               ELSEIF(BUFRFILE(LLEN-4:LLEN) .EQ. 'xx012')THEN 
C   GET AIR TEMPERATURE AND SEA SURFACE TEMPERATURE DATA
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,'WATM TMDB')
	       ENDIF	 
               if(DATES(1).eq.bmiss)then
                    SST=-99.99
	       else	  
                    SST=DATES(1)-273.15  !! Convert from Kelvin to deg C
               endif
               if(DATES(2).eq.bmiss)then
                    ATMP=-99.99
	       else	  
                    ATMP=DATES(2)-273.15  !! Convert from Kelvin to deg C   
               endif
	       IF (SST .GT. -90.0) THEN
               IF(    (NTR_T(I) .LT. 1)
     &	           .or. (dayj .GT. RTIME_T(I,NTR_T(I)) ) ) THEN
	       NTR_T(I)=NTR_T(I)+1
               RTIME_T(I,NTR_T(I) )=dayj
	       T_OBS(I,NTR_T(I))=sst
               ENDIF
               ENDIF

C  ----------------------------------------------------------------------------
C  GET CONDUCTIVITY AND SALINITY
C  ----------------------------------------------------------------------------
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,'SALN WACN')
                 if(DATES(1).eq.bmiss)then
                    SALN=-99.99
	         else	  
                    SALN=DATES(1)
                 endif
                 if(DATES(2).eq.bmiss)then
                    COND=-99.99        !     Unit of COND is mS/cm for NOS station
	         else	  
                    COND=DATES(2)   
                 endif
	         IF (SALN .GT. -90.0) THEN
                 IF(    (NTR_S(I) .LT. 1)
     &	             .or. (dayj .GT. RTIME_S(I,NTR_S(I)) ) ) THEN 
	         NTR_S(I)=NTR_S(I)+1
                 RTIME_S(I,NTR_S(I) )=dayj
	         S_OBS(I,NTR_S(I))=SALN
                 ENDIF
                 ENDIF

C  -----------------------------------------------------------------------------
C  GET TIDAL ELEVATION WITH RESPECT TO CHART AND METEOROLOGICAL RESIDUAL TIDAL 
C  ELEVATION
C  ----------------------------------------------------------------------------
               IF(BUFRFILE(LLEN-4:LLEN) .EQ. 'xx005')THEN      
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,'TERC TIDER')
               ELSEIF(BUFRFILE(LLEN-4:LLEN) .EQ. 'xx012')THEN 
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,'TLLW TIDER')
	       ENDIF	 
                 if(DATES(1).eq.bmiss)then
                    EL=-99.99
	         else	  
                    EL=DATES(1)  
                 endif
                 if(DATES(2).eq.bmiss)then
                    SWL=-99.99
	         else	  
                    SWL=DATES(2)   
                 endif
	         IF (EL .GT. -90.0) THEN
                 IF(    (NTR(I) .LT. 1)
     &	             .or. (dayj .GT. RTIME(I,NTR(I)) ) ) THEN
	         NTR(I)=NTR(I)+1
                 RTIME(I,NTR(I) )=dayj
	         WL_OBS(I,NTR(I))=EL-DATUM(I)
                 ENDIF
                 ENDIF
                 write(15,780)trim(stnbufrid),clon,clat,IYR,IMM,IDD,
     &		 IHH,IMN,EL,ATMP,SST,SALN,COND
             ENDIF 
 	  ENDDO
780       FORMAT(a10,1x,2F10.4,I5,4i3,10F12.4)
C  ------------------------------------------------------------------------------
        ENDIF  ! day_start<=dayj<=day_end
        ENDDO
        ENDDO
        CALL CLOSBF(LUNIN)
        CLOSE(LUNIN)
        ENDIF ! FEXIST
      ENDDO ! IZ=INT(day_start),INT(day_end)
C  -----------------------------------------------------------------------------
C  End of reading WL BUFR FILES and begin WL QC procedures
C  ----------------------------------------------------------------------------
      IF (ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF (ALLOCATED(oned2)) DEALLOCATE(oned2)
      allocate(oned1(NMAX) )
      allocate(oned2(NMAX) )
      DO I=1,NSTA
        IF (NTR(I) .GE. 2)THEN
          avg=0.0
	  NTMP=0
	  DO N=1,NTR(I)
	    IF (ABS(WL_OBS(I,N)) .LE. 10.0)THEN
!	    IF (WL_OBS(I,N) .NE. -99.99)THEN
	       NTMP=NTMP+1
	       AVG=AVG+WL_OBS(I,N)
	       ONED1(NTMP)=WL_OBS(I,N)
	    ENDIF
	  ENDDO 
	  IF(NTMP .GT. 0)AVG=AVG/NTMP
	  SD=0.0    
          IF(NTMP .GT. 2)THEN
  	    DO N=1,NTMP
	      SD=SD+(ONED1(N)-AVG)**2
	    ENDDO 
	    SD=SQRT(SD/(NTMP-1))
	  ELSE
	    SD=1.0
	  ENDIF  
	  BOUND_L=AVG-3.0*SD
	  BOUND_U=AVG+3.0*SD 
          write(*,*)'bound= ',BOUND_L,BOUND_U,AVG,NOS_ID(I)
	  NTMP=0
	  DO N=1,NTR(I)
	    IF ( (WL_OBS(I,N) .GE. BOUND_L) .AND. 
     &	        (WL_OBS(I,N) .LE. BOUND_U)  )THEN
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
C gradient change checking  Dh/Dt < 0.7 meters/hour, and assume first data is good
        NREC=INT( (day_end-day_start)*86400/DELT)+1
        IF (NTR(I) .GE. 2)THEN
	  NTMP=1
          ONED1(NTMP)=WL_OBS(I,1)
          ONED2(NTMP)=RTIME(I,1)
	  DO N=2,NTR(I)  
!	   GD=(WL_OBS(I,N)-WL_OBS(I,N-1))/(RTIME(I,N)-RTIME(I,N-1))/24.
	   GD=(WL_OBS(I,N)-ONED1(NTMP))/(RTIME(I,N)- ONED2(NTMP))/24.
	   IF ( ABS(GD) .LE. 0.7  )THEN
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
	  WRITE(*,*)I,'NTMP OF WL= ',NTMP,'NTR= ',NTR(I)
	ENDIF  
        BUFFER='NUMBER OF WL AT '//TRIM(NOS_ID(I))
        BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
        WRITE(*,*)TRIM(BUFFER),NTR(I)
        BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
        WRITE(*,*)TRIM(BUFFER),NTR(I)

        avg=0.0
        NTMP=0
	close(44)
	OPEN(44,file=trim(NOS_ID(I))//'.obs')
!        print *,'sta name=',trim(NOS_ID(I))//'.obs',NTR(I)
        DO N=1,NTR(I)
          AVG=AVG+WL_OBS(I,N)
	  NTMP=NTMP+1
          jday=RTIME(1,N)+jbase_date
          call GREGORIAN(jday,yearb,monthb,dayb,hourb)
          IYR=INT(yearb)
          IMM=int(monthb+0.001)
          IDD=INT(dayb+0.001)
          IHH=INT(hourb+0.001)
          IMN=INT((hourb-IHH)*60+0.1)
          write(44,*)trim(NOS_ID(I)),IYR,IMM,IDD,IHH,IMN,WL_OBS(I,N)
        ENDDO 
        IF(NTMP .GT. 0)THEN
	  AVG=AVG/NTMP
	ELSE
	  AVG=-99.9
	ENDIF    
	AVG_OBS(I)=AVG
        PRINT *,' MEAN WL OBS.= ',AVG,'num. of obs=',NTMP
      ENDDO
      CLOSE(15) 
      CLOSE(44) 
      INQUIRE(FILE=TRIM(FOUT),EXIST=FEXIST)
      IF(FEXIST)THEN
         CMD='sort '//TRIM(FOUT)//' > tmp.dat'
         call system(trim(CMD) )
         CMD='uniq tmp.dat > tmp1.dat'
         call system(trim(CMD) )
         CMD='cp tmp1.dat '//TRIM(FOUT)
         call system(trim(CMD) )
  !       call system('rm -f tmp.dat tmp1.dat' )
      ENDIF	 
C ----------------------------------------------------------------------------
C    process CREOFS station NetCDF outputs
C ----------------------------------------------------------------------------
      currenttime=day_start
      NREC=0
      timelast=-9999
      DO WHILE (currenttime .LE. day_end)
      jday=currenttime+jbase_date
      call GREGORIAN(jday,yearb,monthb,dayb,hourb)
      IYR=INT(yearb)
      IMM=int(monthb+0.001)
      IDD=INT(dayb+0.001)
      IHH=INT(hourb+0.001)
      IMN=INT((hourb-IHH)*60+0.1)
      ISEC=0
      IF(ISEC .EQ. 60)THEN
        ISEC=0
        IMN=IMN+1
      ENDIF
      IF(IMN .EQ. 60)THEN
        IMN=0
        IHH=IHH+1
      ENDIF
      IF(IHH .EQ. 24)THEN
        IHH=0
        IDD=IDD+1
        IF(MOD(IYR,4) .EQ. 0)DAYS_PER_MONTH(2)=29   !!    Leap Year
        IF(IDD .GT. DAYS_PER_MONTH(IMM) )THEN
          IDD=IDD - DAYS_PER_MONTH(IMM)
          IMM=IMM+1
          IF(IMM .GT. 12)THEN
             IMM=IMM-12
             IYR=IYR+1
          ENDIF
        ENDIF
      ENDIF
      WRITE(TMPFILE,'(I4.4,2I2.2)')IYR,IMM,IDD
      TMPFILE=trim(COMOUT00)//'/'//trim(OFS)//'.'//trim(TMPFILE)
      WRITE(BUFFER,701)IYR,IMM,IDD,'.t',IHH,'z.nc'

      BUFFER=trim(PREFIXNOS)//'.stations.nowcast.'//trim(BUFFER)
!      BUFFER=trim(OFS)//'.stations.nowcast.'//trim(BUFFER)
!     write(*,*)'file=',trim(TMPFILE)
!     write(*,*)'buffer=',trim(BUFFER)
      TMPFILE=trim(TMPFILE)//'/'//trim(BUFFER)
      INQUIRE(FILE=trim(TMPFILE),EXIST=FEXIST)
      IF(FEXIST)THEN
        STATUS = NF_OPEN(trim(TMPFILE), NF_NOWRITE, NCID)
        STATUS = NF_INQ(NCID,NDIMS,NVARS,NGATTS,UNLIMDIMID)
        DO I=1,4
          DIMS(I)=1
        ENDDO	     

        VNAME='zeta'
        STATUS = NF_INQ_VARID(NCID,trim(VNAME),IDVAR)
        STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        do i=1,NDIMS
          STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
          write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
        enddo
        NSTA_MODEL=DIMS(1) 
        NT_MODEL=DIMS(2)
        IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
        
        IF (ALLOCATED(lon)) DEALLOCATE(lon)
        IF (ALLOCATED(lat)) DEALLOCATE(lat)
        IF (ALLOCATED(time_model)) DEALLOCATE(time_model)
        allocate ( lon(NSTA_MODEL))
        allocate ( lat(NSTA_MODEL))
        allocate ( time_model(NT_MODEL))
!        IF (ALLOCATED(zeta)) DEALLOCATE(zeta)
!        IF (.NOT. ALLOCATED(lon))allocate ( lon(NSTA_MODEL))
!        IF (.NOT. ALLOCATED(lat))allocate ( lat(NSTA_MODEL))
!        IF (.NOT. ALLOCATED(zeta))
!     1  allocate ( zeta(NSTA_MODEL,NMAX))
!        IF (.NOT. ALLOCATED(AVG_model))
!     1  allocate ( AVG_model(NSTA_MODEL))
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
        VNAME='lon'
        STATUS = NF_INQ_VARID(NCID,trim(VNAME),IDVAR)
!        STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
!        status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,lon)
        VNAME='lat'
        STATUS = NF_INQ_VARID(NCID,trim(VNAME),IDVAR)
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,lat)
        DO N=1,NSTA
          distmin=99999999.
          if(lonSTA(N) .gt. 180.0)lonSTA(N)=lonSTA(N)-360.0
          DO I=1,NSTA_MODEL
            if(lon(I) .gt. 180.0)lon(I)=lon(I)-360.0
            call dist(lat(I),lon(I),latSTA(N),lonSTA(N),dis)    
            if(dis.le.distmin) then
   		distmin=dis
   		node_find=I
            endif
          ENDDO  
          NODE_STA(N)=node_find
        ENDDO  
        VNAME='time'	     
        STATUS = NF_INQ_VARID(NCID,trim(VNAME),IDVAR)
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,time_model)
        STATUS = NF_GET_ATT_TEXT(NCID,IDVAR,'units',BUFFER)
        BUFFER=trim(adjustL(BUFFER))
        LEN=LEN_TRIM(BUFFER)
        LL=INDEX(BUFFER,'seconds')	   
        IF(LL .GT. 0)scale_time=1.0/86400.0
        LL=INDEX(BUFFER,'minute')	  
        IF(LL .GT. 0)scale_time=1.0/1440.0
        LL=INDEX(BUFFER,'hour') 	
        IF(LL .GT. 0)scale_time=1.0/24.0
        LL=INDEX(BUFFER,'day')         
        IF(LL .GT. 0)scale_time=1.0
        LL=INDEX(BUFFER,'since')	 
        READ(BUFFER(LL+6:LEN),'(I4,1x,I2,1x,I2,1x,I2,1x,I2)')
     1  IYR,IMM,IDD,IHH,IMN
        WRITE(*,*)'base date=',IYR,IMM,IDD,IHH,IMN   
        yearb=IYR
        monthb=IMM
        dayb=IDD
        hourb=IHH+IMN/60.0  
        jday0=JULIAN(yearb,monthb,dayb,hourb)
        DO N=1,NT_MODEL
          time_model(N)=time_model(N)*scale_time+jday0-jbase_date
          if (time_model(N) .GT. timelast)THEN
            NREC=NREC+1
            DO I=1,NSTA
              WL_MODEL(I,NREC)=tmp4d(NODE_STA(I),N,1,1)
            ENDDO 
            timelast=time_model(N)
          ENDIF 
        ENDDO	 
        write(*,*)'file=',trim(TMPFILE)
      ENDIF  
      currenttime=currenttime+1/24.
701   FORMAT(I4.4,2I2.2,A2,I2.2,A4)
      END DO ! WHILE (currenttime .LE. day_end)
      WRITE(*,*)'NREC=',NREC,timelast
      DO I=1,NSTA
        AVG=0.0
        DO N=1,NREC
          AVG=AVG+WL_MODEL(I,N)
	ENDDO
	IF(NREC .GT. 0)THEN
	  AVG=AVG/NREC
	ELSE
	  AVG=-99.9
	ENDIF     
	AVG_model(I)=AVG
	IF( (ABS(AVG_OBS(I)) .GT. 90.0) .OR. 
     1	    (ABS(AVG_MODEL(I)) .GT. 90.0) )THEN
          WL_OFFSET(I)=WL_OFFSET_OLD(I)
	ELSE   
          WL_OFFSET(I)=AVG_MODEL(I)-AVG_OBS(I)
	ENDIF  
        WRITE(*,*)I,trim(NOS_ID(I)),AVG_OBS(I),AVG_MODEL(I)
     1 ,WL_OFFSET(I),WL_OFFSET_OLD(I)
      ENDDO
1000  format(I4,A10,3F12.4)
      close(21)
      open(21,file=trim(PREFIXNOS)//'.fields.wl_correction.dat')
      close(20)
      open(20,file=trim(GRIDFILE_LL))
      WRITE(*,*)'reading grid file= ',trim(GRIDFILE_LL)
      read(20,*)
      read(20,*)NELE,NODES
      write(*,*)'NELE= ',NELE,' NODES = ',NODES
      IF(NODES .NE. NNODE)THEN
        WRITE(*,*)'# of NODE in grid file does not match ',NODES
        WRITE(*,*)'# of NODE in field file ',NNODE
	STOP
      ENDIF	
      IF (ALLOCATED(lon)) DEALLOCATE(lon)
      IF (ALLOCATED(lat)) DEALLOCATE(lat)
      allocate ( lon(NODES))
      allocate ( lat(NODES))
      DO N=1,NODES
        read(20,*)idummy,lon(N),lat(N)
      ENDDO	
      DO N=1,NNODE
        correction=0.0
        DO I=1,NSTA
         correction=correction+weights(N,I)*WL_OFFSET(I)
        ENDDO
	WL_OFFSET_NODE(N)=correction	
	write(21,'(I8,3F14.6)')N,lon(N),lat(N),correction 
      ENDDO	 

      close(20)
      open(20,file=trim(CORRECTION_OLD) )
      DO I=1,NSTA
         WRITE(20,'(I8,a10,3F12.4)')I,trim(NOS_ID(I)),
     1	 WL_OFFSET(I),AVG_OBS(I),AVG_MODEL(I)
      ENDDO       

      close(21)
      open(21,file=trim(PREFIXNOS)//'.stations.wl_correction.dat')

      close(20)
      ISTA=0
      open(20,file=trim(STA_NETCDF_CTL))
      DO
      READ(20,*,end=1100)buffer
      ISTA=ISTA+1
      END DO
1100  CONTINUE
      REWIND(20)        
      PRINT *,'ISTA=',ISTA
      IF (ALLOCATED(lonSTA)) DEALLOCATE(lonSTA)
      IF (ALLOCATED(latSTA)) DEALLOCATE(latSTA)
      IF (ALLOCATED(NODE_STA)) DEALLOCATE(NODE_STA)
      IF (ALLOCATED(WL_OFFSET)) DEALLOCATE(WL_OFFSET)
      IF (ALLOCATED(NOS_ID)) DEALLOCATE(NOS_ID)
      IF (ALLOCATED(NWS_ID)) DEALLOCATE(NWS_ID)
      allocate ( lonSTA(ISTA))
      allocate ( latSTA(ISTA))
      allocate ( NODE_STA(ISTA))
      allocate ( WL_OFFSET(ISTA))
      allocate(NOS_ID(ISTA) )
      allocate (NWS_ID(ISTA))
      DO N=1,ISTA
        READ(20,*)Idummy,NWS_ID(N),NOS_ID(N),lonSTA(N),latSTA(N),
     1	x,y,h,NODE_STA(N)
        correction=WL_OFFSET_NODE(NODE_STA(N))
        WRITE(21,1110)N,trim(NWS_ID(N)),trim(NOS_ID(N)),lonSTA(N),
     1	latSTA(N),x,y,WL_OFFSET_NODE(NODE_STA(N))	

!        distmin=99999999.
!        if(lonSTA(N) .gt. 180.0)lonSTA(N)=lonSTA(N)-360.0
!        DO I=1,NNODE
!           if(lon(I) .gt. 180.0)lon(I)=lon(I)-360.0
!           call dist(lat(I),lon(I),latSTA(N),lonSTA(N),dis)    
!           if(dis.le.distmin) then
!   		distmin=dis
!   		node_find=I
!           endif
!        ENDDO  
!        NODE_STA(N)=node_find
!        WRITE(*,1110)N,trim(NWS_ID(N)),trim(NOS_ID(N)),lonSTA(N),
!     1	latSTA(N),x,y,h,NODE_STA(N)	
      ENDDO
      close(20)
      close(21)
      write(*,*)'nos_creofs_wl_offset_correction.f 
     1  has completed successfully'
1110  FORMAT(I5,a8,2x,a12,2x,f10.4,2x,f10.4,2F14.5,F8.2,2x,I10)
      STOP
      END
