!       Subroutine Name: nos_ofs_wlobc_offset_correction.f 
!
!       Technical Contact(s):   Name:  Aijun Zhang
!                               Org:   NOS/CO-OPS/OD
!                               Phone: 240-533-0591
!                               E-Mail: aijun.zhang@noaa.gov
!
!       Abstract:  this subroutine is used to calculate Great Lake's
!       water offset with observation data
!
!       History Log:
!           03/28/2019
!
!       Usage: call WL_CORRECTION from
!                       nos_ofs_create_forcing_obc_fvcom_gl.f
!
!       Argument Input:
!               OFS - name of OFS
!               COMOUT00        - the location to output  
!               NOSWLDIR        - NOS Water Lever Dir
!               NOSBUFR         - xx012
!               FIN             - the water level control file like
!                                 ${PREFIXNOS}.wlobc.correction.ctl
!               CORRECTION_OLD  - the old water level control file like
!                                 ${PREFIXNOS}.wlobc.correction.old
!               END_TIME        - the finish time 
!               NOBC            - the number of OBC grid
!
!       Argument Output:  
!               WL_OFFSET       - the water level offset value
!


      SUBROUTINE WL_CORRECTION(OFS,COMOUT00,NOSWLDIR,NOSBUFR,FIN,
     & CORRECTION_OLD,END_TIME,NOBC,WL_OFFSET)
      parameter (NMAX=9000)
      include 'netcdf.inc'
      character*200 OFS,COMOUT00,CORRECTION_OLD,END_TIME
      character*200 FIN,FOUT,NOSBUFR,USGSBUFR,NOSWLDIR
      character*200 BUFFER,CMD*132,VNAME,ANAME
      character*200 START_TIME,BUFRFILE,TMPFILE
      real*8 jday_start,jbase_date,JULIAN,yearb,monthb,dayb,hourb
      real*8 jday,jday0,jdays,jdaye,currenttime
      integer, intent (in) :: NOBC
      real, intent (out) :: WL_OFFSET(NOBC)
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
      real, allocatable :: WL_OFFSET_STA(:)
      real, allocatable :: WL_OFFSET_OLD(:)

C  for model station NetCDF
      real, allocatable :: lon(:)
      real, allocatable :: lat(:)
      real, allocatable :: time_model(:)
      real, allocatable :: Mtime(:)
      real, allocatable :: WL_MODEL(:,:)
      real, allocatable :: AVG_MODEL(:)
      real, allocatable :: tmp4d  (:,:,:,:)
      integer dimids(5),COUNT(4),DIMS(4)
      LOGICAL FEXIST,USGS_L
      real, allocatable :: oned1(:)
      real, allocatable :: oned2(:)
      real, allocatable :: oned3(:)

      character*10,allocatable :: NOS_ID(:)
      character*10,allocatable :: NOS_ID_OLD(:)
      character*5,allocatable :: NWS_ID(:)
      character*4,allocatable :: AGENCY_ID(:)
      character(len=200) :: arg
      INTEGER BASE_DATE(4)
      INTEGER DAYS_PER_MONTH(12)
      DATA (DAYS_PER_MONTH(i),I=1,12) /
     &      31,28,31,30,31,30,31,31,30,31,30,31/ 

c variables for real time observations
      CHARACTER*8  SUBSET,NAME                                  
      character*20 stnbufrid
      Real*8 DATES(5),RTIM(6),RPID
      Real*8 data1(2,500),data2(4,500),data3(5)
      real*8 xlocat(5)
      DATA BMISS /10E10/                                                
      DATA NDAYS /2/
c ----------------------------------------------------------------------------
c  Process real time observations in BUFR files of NCEP data tank at NWLON stations
c ----------------------------------------------------------------------------
      read(END_TIME,'(I4,4I2)') IYRE,IMME,IDDE,IHHE
      yearb=IYRE
      monthb=IMME
      dayb=IDDE
      hourb=IHHE   !! do not need minutes to construct ETSS file name
      jdaye=JULIAN(yearb,monthb,dayb,hourb)
      jdays=jdaye-NDAYS
      call GREGORIAN(jdays,yearb,monthb,dayb,hourb)
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

      IYRS=IYR
      IMMS=IMM
      IDDS=IDD
      IHHS=IHH
      IMNS=IMN
      WRITE(START_TIME,'(I4.4,4I2.2)') IYRS,IMMS,IDDS,IHHS
      write(*,*) 'Start time=',IYRS,IMMS,IDDS,IHHS
      write(*,*) 'End time=',IYRE,IMME,IDDE,IHHE
      write(*,*) 'Starttime=',trim(START_TIME)
      write(*,*) 'Endtime=',trim(END_TIME)

      base_date(1)=IYRS
      base_date(2)=1
      base_date(3)=1
      base_date(4)=0
      yearb=base_date(1)
      monthb=base_date(2)
      dayb=base_date(3)
      hourb=base_date(4)
      jbase_date=JULIAN(yearb,monthb,dayb,hourb)
      write(*,*) 'base_date= ',base_date
      day_start=jdays-jbase_date
      day_end=jdaye-jbase_date
      
      write(*,*) 'fin=',trim(FIN)
      OPEN(20,FILE=TRIM(FIN) )
      READ(20,*) NSTA,NNODE
      write(*,*) 'NSTA=',NSTA,' NNODE=', NNODE
      IF(NNODE.ne.NOBC) THEN
        write(*,*) 'NNODE and NOBC not consistent!! NNODE= ',
     &    NNODE,'NOBC= ',NOBC
        STOP
      ENDIF

      allocate(NOS_ID(NSTA))
      allocate(NOS_ID_OLD(NSTA))
      allocate(NWS_ID(NSTA))
      allocate(AGENCY_ID(NSTA))
      ALLOCATE(NTR(NSTA))
      ALLOCATE(NTR_T(NSTA))
      ALLOCATE(NTR_S(NSTA))
      allocate(RTIME(NSTA,NMAX))
      allocate(RTIME_T(NSTA,NMAX))
      allocate(RTIME_S(NSTA,NMAX))
      allocate(WL_OBS(NSTA,NMAX))
      allocate(WL_MODEL(NSTA,NMAX))
      allocate(T_OBS(NSTA,NMAX))
      allocate(S_OBS(NSTA,NMAX))
      allocate(DATUM(NSTA))
      allocate(lonSTA(NSTA))
      allocate(latSTA(NSTA))
      allocate(AVG_OBS(NSTA))
      allocate(AVG_MODEL(NSTA))
      allocate(NODE_STA(NSTA))
      allocate(weights(NNODE,NSTA))
      allocate(WL_OFFSET_STA(NSTA))
      allocate(WL_OFFSET_OLD(NNODE))

c     initiate NTR
      NTR=0
      NTR_T=0
      NTR_S=0      
      DO N=1,NSTA
        READ(20,*) IDUMMY,NOS_ID(N),NWS_ID(N),
     &	  AGENCY_ID(N),lonSTA(N),latSTA(N),DATUM(N)
      ENDDO 
      DO N=1,NNODE
        READ(20,*) IDUMMY,(weights(N,I),I=1,NSTA)
      ENDDO
      CLOSE(20)

      INQUIRE(FILE=trim(CORRECTION_OLD),EXIST=FEXIST)
      IF(FEXIST) THEN
        OPEN(20,FILE=TRIM(CORRECTION_OLD))
        DO N=1,NNODE
          READ(20,*) IDUMMY,WL_OFFSET_OLD(N)
          write(*,*) 'Old offset=',IDUMMY,WL_OFFSET_OLD(N)
        ENDDO 
        CLOSE(20)
      ELSE
        DO N=1,NNODE
          WL_OFFSET_OLD(N)=0.0
        ENDDO
        write(*,*) trim(CORRECTION_OLD),' not exist; '//
     &    'default WL_OFFSET_OLD to 0'
      ENDIF

      WRITE(FOUT,201) 'WL_',IYRS,IMMS,IDDS,IHHS,
     &   '.dat'  
201   FORMAT(A3,I4.4,3I2.2,A4)
      write(*,*) 'fout=',TRIM(adjustL(FOUT))
      FOUT=TRIM(adjustL(FOUT))
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
          IF(MOD(IYR,4).EQ.0) DAYS_PER_MONTH(2)=29   !!  Leap Year
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
        BUFRFILE=TRIM(NOSWLDIR)//trim(BUFRFILE)//trim(NOSBUFR) 
        INQUIRE(FILE=trim(BUFRFILE),EXIST=FEXIST)
        IF(FEXIST) THEN
          write(*,*) 'BUFR FILE= ',trim(BUFRFILE)
          LUNIN=11
          CLOSE(LUNIN)
          OPEN(LUNIN,file=trim(BUFRFILE),FORM='UNFORMATTED')

c  -------------------OPEN AND READ THRU THE INPUT BUFR FILE ------------------       
          CALL OPENBF(LUNIN,'IN',LUNIN)

c  --------------READ THROUGH THE MESSAGES/SUBSETS IN THE FILE------------------
          DO WHILE(IREADMG(LUNIN,SUBSET,IDATE).EQ.0)                       
            DO WHILE(IREADSB(LUNIN).EQ.0)                                    

c  --------------READ THE INTERNAL DATE AND CHECK FOR REALISM-------------------
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
              IF(dayj.GE.day_start.and.dayj.LE.day_end) THEN  
c  --------------READ THE TIDE GAUGE STATION INFO FROM BUFR FILE-------------
c  AJ 09/15/11 Use different CALL routine in order to handle long station IDs 
                CALL READLC(LUNIN, stnbufrid, 'RPID')
                CALL UFBINT(LUNIN,DATES,5,1,IRET,'CLAT CLON SELV')
                clat=DATES(1)
                clon=DATES(2)
                selv=DATES(3)
                DO I=1,NSTA 
                  IF(trim(stnbufrid).EQ.TRIM(NWS_ID(I))) THEN
c  GET SEA SURFACE TEMPERATURE DATA ALONG WITH DATA CHECK AND
c  TIME INCREMENT AND DISPLACEMENT INFORMATION
c  SST1 -- Sea Surface Temperature
c  QMST -- Sea Surface Data Check Flag
c  AWCK -- Tide Station Automated Water Data Check Flag
c  MWCK -- Tide Station Manual Water Data Check Flag
c  TPMI -- Time Period or Replacement
                    LLEN=LEN_TRIM(BUFRFILE)
                    IF(BUFRFILE(LLEN-4:LLEN).EQ.'xx005') THEN      
                      CALL UFBINT(LUNIN,DATES,5,1,IRET,
     &	                'SST1 TMDB AWCK MWCK TPMI')
                    ELSEIF(BUFRFILE(LLEN-4:LLEN).EQ.'xx012') THEN
c  GET AIR TEMPERATURE AND SEA SURFACE TEMPERATURE DATA
                      CALL UFBINT(LUNIN,DATES,5,1,IRET,'WATM TMDB')
                    ENDIF	 

                    if(DATES(1).ge.bmiss/2.0) then
                      SST=-99999.9
                    else	  
                      SST=DATES(1)-273.15  !! Convert from Kelvin to deg C
                    endif

                    if(DATES(2).ge.bmiss/2.0) then
                      ATMP=-99999.9
                    else	  
                      ATMP=DATES(2)-273.15  !! Convert from Kelvin to deg C   
                    endif

                    IF(SST.GT.-90.0) THEN
                      IF(NTR_T(I).LT.1) then
	                NTR_T(I)=NTR_T(I)+1
                        RTIME_T(I,NTR_T(I))=dayj
	                T_OBS(I,NTR_T(I))=sst
     	              elseif(dayj.GT.RTIME_T(I,NTR_T(I))) THEN
	                NTR_T(I)=NTR_T(I)+1
                        RTIME_T(I,NTR_T(I))=dayj
                        T_OBS(I,NTR_T(I))=sst
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

                    IF(SALN.GT.-90.0) THEN
                      IF(NTR_S(I).LT.1) then
                        NTR_S(I)=NTR_S(I)+1
                        RTIME_S(I,NTR_S(I))=dayj
                        S_OBS(I,NTR_S(I))=SALN
     	              elseif(dayj.GT.RTIME_S(I,NTR_S(I))) THEN
                        NTR_S(I)=NTR_S(I)+1
                        RTIME_S(I,NTR_S(I))=dayj
                        S_OBS(I,NTR_S(I))=SALN
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

                    IF(EL.GT.-90.0) THEN
                      IF(NTR(I).LT.1) THEN
                        NTR(I)=NTR(I)+1
                        RTIME(I,NTR(I))=dayj
                        WL_OBS(I,NTR(I))=EL-DATUM(I)
                      ELSEIF(dayj.GT.RTIME(I,NTR(I))) THEN
                        NTR(I)=NTR(I)+1
                        RTIME(I,NTR(I))=dayj
                        WL_OBS(I,NTR(I))=EL-DATUM(I)
                      ENDIF
                    ENDIF
                    write(15,780) trim(stnbufrid),clon,clat,
     &	              IYR,IMM,IDD,IHH,IMN,EL,ATMP,SST,SALN,COND
                  ENDIF 
                ENDDO
780             FORMAT(a10,1x,2F10.4,I5,4i3,10F12.4)
              ENDIF    ! day_start<=dayj<=day_end
            ENDDO    ! IREADSB
          ENDDO    ! IREADMG
          CALL CLOSBF(LUNIN)
          CLOSE(LUNIN)
        ENDIF   ! FEXIST
      ENDDO   ! IZ=INT(day_start),INT(day_end)

c  -----------------------------------------------------------------------------
c  End of reading WL BUFR FILES and begin WL QC procedures
c  ----------------------------------------------------------------------------
      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      allocate(oned1(NMAX))
      allocate(oned2(NMAX))
      DO I=1,NSTA
        IF(NTR(I).GE.2) THEN
          avg=0.0
	  NTMP=0
	  DO N=1,NTR(I)
	    IF(ABS(WL_OBS(I,N)).LE.10.0) THEN
              NTMP=NTMP+1
              AVG=AVG+WL_OBS(I,N)
              ONED1(NTMP)=WL_OBS(I,N)
            ENDIF
          ENDDO 
          WRITE(6,*) '# of data point ||>=10: ',NTR(I)-NTMP
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
     &	       (WL_OBS(I,N).LE.BOUND_U)) THEN
              NTMP=NTMP+1
              ONED1(NTMP)=WL_OBS(I,N)
              ONED2(NTMP)=RTIME(I,N)
            ENDIF
          ENDDO
          WRITE(6,*) '# of data point out of bound (+/-3sd):  ',
     &      NTR(I)-NTMP
  	  NTR(I)=NTMP
	  DO N=1,NTMP
            WL_OBS(I,N)=ONED1(N) 
            RTIME(I,N)=ONED2(N)    
          ENDDO
        ENDIF   

c Gradient change checking  Dh/Dt < 0.7 meters/hour, and assume first data is good
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
          WRITE(*,*) '# of data point rate of change > 0.7m/h:  ',
     &      NTR(I)-NTMP
          DO N=1,NTMP
            WL_OBS(I,N)=ONED1(N) 
            RTIME(I,N)=ONED2(N)    
          ENDDO
          NTR(I)=NTMP
        ENDIF  
        BUFFER='NUMBER OF WL AT '//TRIM(NOS_ID(I))
        BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
        BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
        WRITE(*,*) TRIM(BUFFER),NTR(I)

        avg=0.0
        NTMP=0
	close(44)
	OPEN(44,file=trim(NOS_ID(I))//'.obs')
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
          write(44,*) trim(NOS_ID(I)),IYR,IMM,IDD,IHH,IMN,WL_OBS(I,N)
        ENDDO 
        IF(NTMP.GT.0) THEN
          AVG=AVG/NTMP
        ELSE
          AVG=-99999.9
        ENDIF    
        AVG_OBS(I)=AVG
        write(*,*) ' MEAN WL OBS.= ',AVG,' num. of obs=',NTMP
      ENDDO
      CLOSE(15) 
      CLOSE(44) 

      INQUIRE(FILE=TRIM(FOUT),EXIST=FEXIST)
      IF(FEXIST) THEN
        CMD='sort '//TRIM(FOUT)//' > tmp.dat'
        call system(trim(CMD))
        CMD='uniq tmp.dat > tmp1.dat'
        call system(trim(CMD))
        CMD='cp tmp1.dat '//TRIM(FOUT)
        call system(trim(CMD))
      ENDIF	 

c ----------------------------------------------------------------------------
c  Process model station NetCDF outputs
c ----------------------------------------------------------------------------
      open(10,file='model_out.dat')
      IF(ALLOCATED(Mtime)) DEALLOCATE(Mtime)
      allocate(Mtime(NDAYS*240+1))
      currenttime=day_start
      NREC=0
      timelast=-9999
      DO WHILE (currenttime.LE.day_end+0.01)
        jday=currenttime+jbase_date
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

        WRITE(TMPFILE,'(I4.4,2I2.2)') IYR,IMM,IDD
        TMPFILE=trim(COMOUT00)//'/'//trim(OFS)//'.'//trim(TMPFILE)
        WRITE(BUFFER,'(A2,I2.2,A2,I4.4,2I2.2)') '.t',IHH,'z.',IYR,IMM,IDD
        BUFFER=trim(OFS)//trim(BUFFER)//'.stations.nowcast.nc'
        TMPFILE=trim(TMPFILE)//'/'//trim(BUFFER)
        INQUIRE(FILE=trim(TMPFILE),EXIST=FEXIST)
        IF(FEXIST) THEN
          STATUS=NF_OPEN(trim(TMPFILE),NF_NOWRITE,NCID)
          CALL check_err(STATUS)
          STATUS=NF_INQ(NCID,NDIMS,NVARS,NGATTS,UNLIMDIMID)
          CALL check_err(STATUS)
          DO I=1,4
            DIMS(I)=1
          ENDDO	     

          VNAME='zeta'
          STATUS=NF_INQ_VARID(NCID,trim(VNAME),IDVAR)
          CALL check_err(STATUS)
          STATUS=NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
          CALL check_err(STATUS)
          STATUS=NF_INQ_VARDIMID(NCID,IDVAR,dimids)
          CALL check_err(STATUS)
          do i=1,NDIMS
            STATUS=NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
            CALL check_err(STATUS)
            write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
          enddo
          NSTA_MODEL=DIMS(1) 
          NT_MODEL=DIMS(2)
          IF(ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
          ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)        
          CALL check_err(STATUS)

          IF(ALLOCATED(lon)) DEALLOCATE(lon)
          IF(ALLOCATED(lat)) DEALLOCATE(lat)
          IF(ALLOCATED(time_model)) DEALLOCATE(time_model)
          allocate(lon(NSTA_MODEL))
          allocate(lat(NSTA_MODEL))
          allocate(time_model(NT_MODEL))
          VNAME='lon'
          STATUS=NF_INQ_VARID(NCID,trim(VNAME),IDVAR)
          CALL check_err(STATUS)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,lon)
          CALL check_err(STATUS)

          VNAME='lat'
          STATUS=NF_INQ_VARID(NCID,trim(VNAME),IDVAR)
          CALL check_err(STATUS)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,lat)
          CALL check_err(STATUS)
          DO N=1,NSTA
            distmin=99999999.
            if(lonSTA(N).gt.180.0) lonSTA(N)=lonSTA(N)-360.0
            DO I=1,NSTA_MODEL
              if(lon(I).gt.180.0) lon(I)=lon(I)-360.0
              call dist(lat(I),lon(I),latSTA(N),lonSTA(N),dis)
              if(dis.le.distmin) then
   		distmin=dis
   		node_find=I
              endif
            ENDDO  
            NODE_STA(N)=node_find
          ENDDO  

          VNAME='time'	     
          STATUS=NF_INQ_VARID(NCID,trim(VNAME),IDVAR)
          CALL check_err(STATUS)
          STATUS=NF_GET_VAR_REAL(NCID,IDVAR,time_model)
          CALL check_err(STATUS)
          STATUS=NF_GET_ATT_TEXT(NCID,IDVAR,'units',BUFFER)
          CALL check_err(STATUS)
          BUFFER=trim(adjustL(BUFFER))
          LEN=LEN_TRIM(BUFFER)
          LL=INDEX(BUFFER,'seconds')	   
          IF(LL.GT.0) scale_time=1.0/86400.0
          LL=INDEX(BUFFER,'minute')	  
          IF(LL.GT.0) scale_time=1.0/1440.0
          LL=INDEX(BUFFER,'hour') 	
          IF(LL.GT.0) scale_time=1.0/24.0
          LL=INDEX(BUFFER,'day')         
          IF(LL.GT.0) scale_time=1.0
          LL=INDEX(BUFFER,'since')	 
          READ(BUFFER(LL+6:LEN),'(I4,1x,I2,1x,I2,1x,I2,1x,I2)')
     &      IYR,IMM,IDD,IHH,IMN
          WRITE(*,*) 'Base date=',IYR,IMM,IDD,IHH,IMN   
          yearb=IYR
          monthb=IMM
          dayb=IDD
          hourb=IHH+IMN/60.0  
          jday0=JULIAN(yearb,monthb,dayb,hourb)
          DO N=1,NT_MODEL
            time_model(N)=time_model(N)*scale_time+jday0-jbase_date
            if(time_model(N).GE.day_start.and.
     &         time_model(N).LE.day_end) THEN
              if(time_model(N).GT.timelast) THEN
                NREC=NREC+1
                Mtime(NREC)=time_model(N)
                DO I=1,NSTA
                  WL_MODEL(I,NREC)=tmp4d(NODE_STA(I),N,1,1)
                ENDDO 
                timelast=time_model(N)
                write(10,'(F15.8,2F10.3)') time_model(N),
     &            (WL_MODEL(I,NREC),i=1,NSTA)
              ENDIF 
            ENDIF
          ENDDO	 
          write(*,*) 'File=',trim(TMPFILE)
        ENDIF  
 
        currenttime=currenttime+1/24.0
      END DO ! WHILE (currenttime .LE. day_end)
      WRITE(*,*) 'NREC=',NREC,timelast
c  Interpolate to the same time as observation
      IF(ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF(ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF(ALLOCATED(oned3)) DEALLOCATE(oned3)
      allocate(oned1(NDAYS*240+1))
      allocate(oned2(NDAYS*240+1))
      allocate(oned3(NDAYS*240+1))
      DO I=1,NSTA
        ICOUNT=NTR(I)
        write(*,*) 'I,Icount', I, ICOUNT
        DO N1=1,ICOUNT
          oned1(N1)=RTIME(I,N1)
        END DO
        DO N1=1,NREC
          oned3(N1)=WL_MODEL(I,N1)
        ENDDO
        CALL lineararray(ICOUNT,oned1,oned2,NREC,Mtime,oned3)
        DO N1=1,ICOUNT
          WL_MODEL(I,N1)=oned2(N1)
          write(10,'(F15.8,2F10.3)') oned1(N1),oned2(N1),WL_OBS(I,N1)
        END DO
      END DO    

      DO I=1,NSTA
        AVG=0.0
        DO N=1,NTR(I)
          AVG=AVG+WL_MODEL(I,N)
	ENDDO
	IF(NTR(I).GT.0) THEN
	  AVG=AVG/NTR(I)
	ELSE
	  AVG=-99999.9
	ENDIF     
	AVG_model(I)=AVG
       
	IF((ABS(AVG_OBS(I)).GT.90.0).OR.
     &	   (ABS(AVG_MODEL(I)).GT.90.0).OR.
     &      NTR(I).LE.NDAYS*240*0.8) THEN    !80% valid data
          WL_OFFSET_STA(I)=-99999.9
	ELSE   
          WL_OFFSET_STA(I)=AVG_MODEL(I)-AVG_OBS(I)
	ENDIF  
        WRITE(*,*) I,trim(NOS_ID(I)),AVG_OBS(I),AVG_MODEL(I),
     &    WL_OFFSET_STA(I)
      ENDDO
1000  format(I4,A10,3F12.4)

      DO I=1,NNODE
        WL_OFFSET(I)=0.0
        DO N=1,NSTA
          WL_OFFSET(I)=WL_OFFSET(I)+WL_OFFSET_STA(N)*weights(I,N)
        ENDDO
        IF(ABS(WL_OFFSET(I)).GT.90.0) WL_OFFSET(I)=WL_OFFSET_OLD(I)
      ENDDO
      close(20)
      open(20,file=trim(CORRECTION_OLD) )
      DO I=1,NNODE
        WRITE(20,'(I8,2F12.4)') I,WL_OFFSET(I),WL_OFFSET_OLD(I)
      ENDDO

      write(*,*) 'nos_creofs_wl_offset_correction.f'//
     &  'has completed successfully'
1110  FORMAT(I5,a8,2x,a12,2x,f10.4,2x,f10.4,2F14.5,F8.2,2x,I10)

      RETURN
      END

