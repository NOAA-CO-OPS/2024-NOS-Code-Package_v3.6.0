      CHARACTER*120 FIN,FOUT,BUFFER,TIME_START,TIME_END,PRODNAME
      CHARACTER*120 CTMP(2000),CTMP1(2000)
      CHARACTER*10 nowcastend,mettime
      DIMENSION TIME(2000)
      CHARACTER*100 frmt
      real*8 jdays,jdaye,jbase_date,JULIAN,yearb,monthb,dayb,hourb
      real*8 jday,jday0,jtmp(2000),jtmp1(2000)
      READ(5,100)BUFFER
      TIME_START=TRIM(BUFFER)
      read(TIME_START,'(I4,4I2)')IYRS,IMMS,IDDS,IHHS,IMNS
      yearb=IYRS
      monthb=IMMS
      dayb=IDDS
      hourb=IHHS   
      jdays=JULIAN(yearb,monthb,dayb,hourb)
      READ(5,100)BUFFER
      nowcastend=TRIM(BUFFER(1:10))
      READ(5,100)BUFFER
      TIME_END=TRIM(BUFFER)
      read(TIME_END,'(I4,4I2)')IYRE,IMME,IDDE,IHHE,IMNE
      yearb=IYRE
      monthb=IMME
      dayb=IDDE
      hourb=IHHE   
      jdaye=JULIAN(yearb,monthb,dayb,hourb)
!      READ(5,100)BUFFER
!      TIME_END=TRIM(BUFFER)
!      read(TIME_END,'(I4,4I2)')IYR,IMM,IDD,IHH,IMN
!      yearb=IYR
!      monthb=IMM
!      dayb=IDD
!      hourb=IHH  
!      jbase_date=JULIAN(yearb,monthb,dayb,hourb)
      READ(5,100)FIN
      READ(5,100)FOUT
      PRINT *,'start time=',IYRS,IMMS,IDDS,IHHS,IMNS
      PRINT *,'end time=  ',IYRE,IMME,IDDE,IHHE,IMNE
      print *,trim(FIN)
      print *,trim(FOUT)
      OPEN(2,file=trim(FOUT) )
      OPEN(1,file=trim(FIN))
      READ(1,100,END=99)BUFFER
      buffer=adjustl(buffer)
      lenstr=len_trim(BUFFER)
      WRITE (frmt,40) lenstr
40    FORMAT ('(a',I3.3,')')
!      write(*,*)'frmt=',trim(frmt)
      write(*,frmt)'buffer=',trim(buffer) 
      IND=INDEX(BUFFER,'nam')
      IF(IND .GT. 0 )PRODNAME='nam'
      IND=INDEX(BUFFER,'pgrb2.0p50')
      IF(IND .GT. 0)PRODNAME='gfs'
      IND=INDEX(BUFFER,'pgrb2.0p25')
      IF(IND .GT. 0)PRODNAME='gfs25'


      IND=INDEX(BUFFER,'rtma2p5')
      IF(IND .GT. 0)PRODNAME='rtma'
      IND=INDEX(BUFFER,'ndfd_')
      IF(IND .GT. 0)PRODNAME='ndfd'
      IND=INDEX(BUFFER,'rap')
      IF(IND .GT. 0)PRODNAME='rap'
      IND=INDEX(BUFFER,'hrrr')
      IF(IND .GT. 0)PRODNAME='hrrr'

      write(*,*)'PRODNAME=',trim(PRODNAME)
      BACKSPACE(1)
      ICOUNT=1
      DO
10    READ(1,100,END=30)BUFFER    
      IF (TRIM(PRODNAME) .eq. 'nam')THEN
        IND=INDEX(BUFFER,'nam.')
	IND_DATE=IND+4
	READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
        IND=INDEX(BUFFER,'nam.t')
	IND_CYC=IND+5
	READ(BUFFER(IND_CYC:IND_CYC+1),'(I2)')ICYC
        IND=INDEX(BUFFER,'awip12')
        IF(IND .GT. 0)THEN
  	  IND_HOUR=IND+6
          READ(BUFFER(IND_HOUR:IND_HOUR+1),*)IHH
!	  READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')IHH
	ENDIF
        IND=INDEX(BUFFER,'awp242')
        IF(IND .GT. 0)THEN
  	  IND_HOUR=IND+6
          READ(BUFFER(IND_HOUR:IND_HOUR+1),*)IHH
!	  READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')IHH
	ENDIF
  
        IND=INDEX(BUFFER,'hiresf')
        IF(IND .GT. 0)THEN
	  IND_HOUR=IND+6
	  READ(BUFFER(IND_HOUR:IND_HOUR+1),*)IHH
!          READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')IHH
	  IF (IHH .EQ. 0)CYCLE    !for NAM4 U&V at hour 00 are reset to 0 
	ENDIF  

	PRINT *,'date=',IYR,IMM,IDD,ICYC,IHH
	WRITE(mettime,'(I4.4,3I2.2)')IYR,IMM,IDD,ICYC
        write(*,*)mettime,nowcastend
        IF (mettime . gt. nowcastend) CYCLE  
!not use forecast files later than current OFS run cycle. e.g. run 09z cycle will not use product from t12z cycle NAM GFS .

      ELSEIF (TRIM(PRODNAME) .eq. 'gfs')THEN
        IND=INDEX(BUFFER,'gfs.')
        IND_DATE=IND+4
        READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
        IND=INDEX(BUFFER,'gfs.t')
        IND_CYC=IND+5
        READ(BUFFER(IND_CYC:IND_CYC+1),'(I2)')ICYC
        IND=INDEX(BUFFER,'pgrb2.0p50.f')
        IND_HOUR=IND+12
        READ(BUFFER(IND_HOUR:IND_HOUR+2),*)IHH
!       READ(BUFFER(IND_HOUR:IND_HOUR+2),'(I3)')IHH
!	PRINT *,'date=',IYR,IMM,IDD,ICYC,IHH
      ELSEIF (TRIM(PRODNAME) .eq. 'gfs25')THEN
        IND=INDEX(BUFFER,'gfs.')
        IND_DATE=IND+4
        READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
        IND=INDEX(BUFFER,'gfs.t')
        IND_CYC=IND+5
        READ(BUFFER(IND_CYC:IND_CYC+1),'(I2)')ICYC
        IND=INDEX(BUFFER,'pgrb2.0p25.f')
        IND_HOUR=IND+12
        READ(BUFFER(IND_HOUR:IND_HOUR+2),'(I3)')IHH
	WRITE(mettime,'(I4.4,3I2.2)')IYR,IMM,IDD,ICYC
        write(*,*)mettime
        IF (mettime . gt. nowcastend) CYCLE  
 !not use forecast files later than current OFS run cycle. e.g. run 09z cycle will not use product from t12z cycle NAM GFS .
      ELSEIF (TRIM(PRODNAME) .eq. 'rtma')THEN
        ICYC=0
        IND=INDEX(BUFFER,'rtma2p5.')
	IND1=IND+8
	IND_DATE=IND+8
	READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
        IND=INDEX(BUFFER,'rtma2p5.t')
	IND_HOUR=IND+9
	READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')IHH
!	PRINT *,'date=',IYR,IMM,IDD,ICYC,IHH
      ELSEIF (TRIM(PRODNAME) .eq. 'ndfd')THEN
        IHH=0
        IND=INDEX(BUFFER,'ndfd_')
	IND_DATE=IND-17
        IF (IND_DATE .LE. 0)THEN
            WRITE(*,*)'error to find NDFD file name'
            WRITE(*,*)'STOP at nos_ofs_met_file_search.f'
            STOP
        ELSE
          WRITE(*,*)BUFFER(IND_DATE:IND_DATE+7)
	  READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
!          PRINT *,'date=',IYR,IMM,IDD
          IND=INDEX(BUFFER,'grib2')
	  IND_HOUR=IND-3
!          write(*,*)BUFFER(IND_HOUR:IND_HOUR+1)
	  READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')ICYC
	  PRINT *,'date=',IYR,IMM,IDD,ICYC,IHH
        ENDIF
      ELSEIF (TRIM(PRODNAME) .eq. 'rap')THEN
        ICYC=0
        IND=INDEX(BUFFER,'rap.')
	IND_DATE=IND+4
	READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
        IND=INDEX(BUFFER,'rap.t')
	IND_HOUR=IND+5
	READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')ICYC
        IND=INDEX(BUFFER,'grbf')
	IND_HOUR=IND+4
	READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')IHH
      ELSEIF (TRIM(PRODNAME) .eq. 'hrrr')THEN
        ICYC=0
        IND=INDEX(BUFFER,'hrrr.')
        IND_DATE=IND+5
        READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
        IND=INDEX(BUFFER,'hrrr.t')
        IND_HOUR=IND+6
        READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')ICYC
        IND=INDEX(BUFFER,'wrfsfcf')
        IND_HOUR=IND+7
        READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')IHH
!	PRINT *,'date=',IYR,IMM,IDD,ICYC,IHH
      ENDIF
      yearb=IYR
      monthb=IMM
      dayb=IDD
      hourb=ICYC+IHH   
      jtmp(ICOUNT)=JULIAN(yearb,monthb,dayb,hourb)
      CTMP(ICOUNT)=TRIM(BUFFER)
        write(*,*)ICOUNT,trim(CTMP(ICOUNT))
      ICOUNT=ICOUNT+1
      ENDDO   
30    CONTINUE
      NREC=ICOUNT-1
      print *,'NREC=',NREC
CC  This is added to prevent using incomplete cycle which provide shorter
CC  time coverage than previous cycle
      NREC0=NREC
      DO I=NREC0,1,-1
         IF(jtmp(I) .GT. jtmp(NREC0)) THEN
           NREC=I
           EXIT
         ENDIF
      ENDDO
      print *,'New NREC=',NREC
      ICOUNT=1
      CTMP1(1)=CTMP(NREC)
      jtmp1(1)=jtmp(NREC)
      DO I=NREC,1,-1
	 IF(jtmp(I) .LT. jtmp1(ICOUNT) ) THEN
           ICOUNT=ICOUNT+1
           jtmp1(ICOUNT)=jtmp(I)
           CTMP1(ICOUNT)=CTMP(I)
 !        WRITE(*,*)ICOUNT,TRIM(ADJUSTL(CTMP1(ICOUNT)))
	 ENDIF
      ENDDO
      PRINT *,'ICOUNT=',ICOUNT
      DO I=1,ICOUNT
        I1=ICOUNT-I+1
        jtmp(I)=jtmp1(I1)
        CTMP(I)=CTMP1(I1)
      ENDDO
      DO I=1,ICOUNT
        jtmp1(I)=jtmp(I)
        CTMP1(I)=CTMP(I)
 !        WRITE(*,*)I,TRIM(ADJUSTL(CTMP1(I)))
      ENDDO
 
      NSTR=1
      DO I=1,ICOUNT
         IF( jtmp1(I) .EQ. jdays )THEN
	    NSTR=I
	    exit 
         ELSEIF( jtmp1(I) .GT. jdays )THEN
	    NSTR=I-1
	    exit 
	 ENDIF
      ENDDO	
130   continue
      print *,'NSTR=',NSTR
      IF(NSTR .LE. 0 )NSTR=1
      IF(NSTR .GT. ICOUNT )THEN
        WRITE(*,*)'TIME of MET products is greater than TIME_START'
        WRITE(*,*)'no appropriate MET FILE is available'
	STOP 'stop in nos_ofs_met_file_search.f'
      ENDIF  
      IF(jtmp1(ICOUNT) .LT. jdaye )THEN
        IF (TRIM(PRODNAME) .ne. 'ndfd')THEN
          WRITE(*,*)'Time of MET products is less than TIME_END'
          WRITE(*,*)'no appropriate MET FILE is available'
        ENDIF
	 NEND=ICOUNT
      ELSE
         DO I=1,ICOUNT
           IF( jtmp1(I) .GE. jdaye )THEN
	      NEND=I
	      exit 
	   ENDIF
         ENDDO
      ENDIF	 	
140   continue
      NFILE=NEND-NSTR+1
      PRINT *,'NFILE=',NEND-NSTR+1,NSTR,NEND     
      PRINT *,'FILE START=',TRIM(CTMP1(NSTR))
      PRINT *,'FILE END  =',TRIM(CTMP1(NEND))
      jday0=jdays
      IF (TRIM(PRODNAME) .eq. 'ndfd')THEN
         NSTR=1
         NEND=ICOUNT
      ENDIF
      DO I=NSTR,NEND	      	                                                        
        BUFFER=TRIM(ADJUSTL(CTMP1(I)))
        lenstr=len_trim(BUFFER)
        WRITE (frmt,40) lenstr
        IF (TRIM(PRODNAME) .eq. 'nam')THEN
          IND=INDEX(BUFFER,'nam.')
	  IND_DATE=IND+4
	  READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
          IND=INDEX(BUFFER,'nam.t')
	  IND_CYC=IND+5
	  READ(BUFFER(IND_CYC:IND_CYC+1),'(I2)')ICYC
          IND=INDEX(BUFFER,'awip12')
          IF(IND .GT. 0)THEN
  	    IND_HOUR=IND+6
	    READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')IHH
	  ENDIF  
          IND=INDEX(BUFFER,'awp242')
          IF(IND .GT. 0)THEN
  	    IND_HOUR=IND+6
!            READ(BUFFER(IND_HOUR:IND_HOUR+1),*)IHH
	    READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')IHH
	  ENDIF

          IND=INDEX(BUFFER,'hiresf')
          IF(IND .GT. 0)THEN
	    IND_HOUR=IND+6
	    READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')IHH
	  ENDIF  
        ELSEIF (TRIM(PRODNAME) .eq. 'gfs')THEN
          IND=INDEX(BUFFER,'gfs.')
          IND_DATE=IND+4
          READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
          IND=INDEX(BUFFER,'gfs.t')
          IND_CYC=IND+5
          READ(BUFFER(IND_CYC:IND_CYC+1),'(I2)')ICYC
          IND=INDEX(BUFFER,'pgrb2.0p50.f')
          IND_HOUR=IND+12
          READ(BUFFER(IND_HOUR:IND_HOUR+2),'(I3)')IHH

        ELSEIF (TRIM(PRODNAME) .eq. 'gfs25')THEN
          IND=INDEX(BUFFER,'gfs.')
          IND_DATE=IND+4
          READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
          IND=INDEX(BUFFER,'gfs.t')
          IND_CYC=IND+5
          READ(BUFFER(IND_CYC:IND_CYC+1),'(I2)')ICYC
          IND=INDEX(BUFFER,'pgrb2.0p25.f')
          IND_HOUR=IND+12
          READ(BUFFER(IND_HOUR:IND_HOUR+2),'(I3)')IHH

        ELSEIF (TRIM(PRODNAME) .eq. 'rtma')THEN
          IND=INDEX(BUFFER,'rtma2p5.')
	  IND_DATE=IND+8
	  READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
          IND=INDEX(BUFFER,'rtma2p5.t')
	  IND_HOUR=IND+9
	  READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')ICYC
	  IHH=0
!        ELSEIF (TRIM(PRODNAME) .eq. 'ndfd')THEN
!          ICYC=0
!          IND=INDEX(BUFFER,'ndfd2a.')
!	  IND_DATE=IND+6
!	  READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
!          IND=INDEX(BUFFER,'ndfd2.t')
!	  IND_HOUR=IND+6
!	  READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')ICYC
!	  IHH=0
        ELSEIF (TRIM(PRODNAME) .eq. 'ndfd')THEN
          IHH=0
          IND=INDEX(BUFFER,'ndfd')
          IND_DATE=IND-17
          IF (IND_DATE .LE. 0)THEN
            WRITE(*,*)'error to find NDFD file name'
            WRITE(*,*)'STOP'
            STOP
          ELSE
            READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
!            IND=INDEX(BUFFER,'grib2')
!            IND_HOUR=IND-3
!            READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')ICYC
            ICYC=0
            PRINT *,'date=',IYR,IMM,IDD,ICYC,IHH
          ENDIF
        ELSEIF (TRIM(PRODNAME) .eq. 'rap')THEN
          ICYC=0
          IND=INDEX(BUFFER,'rap.')
	  IND_DATE=IND+4
	  READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
          IND=INDEX(BUFFER,'rap.t')
	  IND_HOUR=IND+5
	  READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')ICYC
          IND=INDEX(BUFFER,'grbf')
	  IND_HOUR=IND+4
	  READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')IHH
        ELSEIF (TRIM(PRODNAME) .eq. 'hrrr')THEN
          ICYC=0
          IND=INDEX(BUFFER,'hrrr.')
          IND_DATE=IND+5
          READ(BUFFER(IND_DATE:IND_DATE+7),'(I4,2I2)')IYR,IMM,IDD
          IND=INDEX(BUFFER,'hrrr.t')
          IND_HOUR=IND+6
          READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')ICYC
          IND=INDEX(BUFFER,'wrfsfcf')
          IND_HOUR=IND+7
          READ(BUFFER(IND_HOUR:IND_HOUR+1),'(I2)')IHH


        ENDIF
        IF (TRIM(PRODNAME) .eq. 'nam' .or. (TRIM(PRODNAME) .eq. 'gfs') 
     2 .or. (TRIM(PRODNAME) .eq. 'gfs25') )THEN
          yearb=IYR
          monthb=IMM
          dayb=IDD
          hourb=ICYC+IHH   
          jday=JULIAN(yearb,monthb,dayb,hourb)
	  IF (jday .GE. jday0)THEN
            WRITE(2,frmt)TRIM(ADJUSTL(CTMP1(I)))
	    WRITE(2,'(I4.4,3(1x,I2.2),1x,I3.3)')IYR,IMM,IDD,ICYC,IHH
!	    jday0=jday0+3.0/24.
	  ENDIF
        ELSEIF (TRIM(PRODNAME) .eq. 'ndfd')THEN
          yearb=IYR
          monthb=IMM
          dayb=IDD
          hourb=ICYC+IHH
          jday=JULIAN(yearb,monthb,dayb,hourb)
          IF (jday .LE. jday0)THEN
            WRITE(2,frmt)TRIM(ADJUSTL(CTMP1(I)))
            WRITE(2,'(I4.4,3(1x,I2.2),1x,I3.3)')IYR,IMM,IDD,ICYC,IHH
          ENDIF
	ELSE    
          WRITE(2,frmt)TRIM(ADJUSTL(CTMP1(I)))
	  WRITE(2,'(I4.4,3(1x,I2.2),1x,i3.3)')IYR,IMM,IDD,ICYC,IHH
	ENDIF  

      ENDDO
      IF(NFILE .LE. 0)THEN
         WRITE(*,*)'no appropriate MET FILE is found'
      ELSE
         WRITE(*,*)'Number of ',NFILE,'MET FILE is found'
         WRITE(*,*)'COMPLETED SUCCESSFULLY'
      ENDIF	 
      	
100   FORMAT(a120)
99    STOP
      END
