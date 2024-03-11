!     
!     Documentation  for nos_pred.f 
!     
!----------------------------------------------------------------------------------
!     
!     Fortran Program Name: nos_pred.f
!     
!     Directory Location:     /COMF/oqcs/sorc
!     
!     Technical Contact(s):   Name:  Aijun Zhang             Org:  NOS/CO-OPS
!     Phone: 301-713-2890x127      E-Mail: aijun.zhang@noaa.gov
!     
!     Abstract:
!     This program is modified from pred.f of Chris Zervas so that 
!     it can make prediction of multiple years. 
!     Change call CONCTJ and CONJTC to call julian.
!     Also call equarg.f to calculate XODE and VPU, instead of reading 
!     from data file 'yr'.
!     
!     
!     Usage:   nos_pred "$BEGINDATE" "$ENDDATE" $KINDAT $DELT $CONV $XMAJOR $filein $fileout
!     
!     Called by 
!     
!     Input Parameters:  
!     BEGINDATE="200801011230"
!     ENDDATE=  "200812311230"
!     KINDAT=1, for current prediction; =2 for water level prediction
!     DELT is time interval of output time series in hours
!     CONV: Units convertion of predicted variable
!     XMAJOR is principle current direction in degrees
!     filein is input file name which includes tide constituents
!     fileout is output name which contains predicted water level or current time series
!     
!     wl
!     2003 01 02 00 00 00     0.5085
!     2003 01 02 00 06 00     0.5169
!     sp        dir  
!     2003 01 02 00 00 00     0.7091   258.0500
!     2003 01 02 00 06 00     0.7237   258.3601
!     
!     1    2    1.   0.    0              ! nsta ipredk conv tconv il2
!     tss.out                                 ! Output time series file
!     0  4  15 0  6 30 0 0.1 1998 1998 106.0   IEL,IMMS,IDDS,TIME,IMME,IDDE,TIMEL,DELT,IYRS,IYRE,XMAJOR
!     Harmonic Analysis of Data in  325j4b05.dat                                      
!     29-Day H.A.  Beginning  4-15-1998  at Hour 17.30  along 106 degrees             
!     12718
!     1931621828140022115186641641117973376 68733224 81743495 5868 163
!     2    0   0  804  92    0   0 36211666  4431590    0   0 24821455
!     3  3513257  6521961    0   0  5803436  6463317    0   0    0   0
!     0   0    0   0    0   0  3113546 15863554  8262104  1122127
!     5  213  13 39053385    0   0    0   0 26691641    0   0 38082138
!     6 28911704    0   0
!     Harmonic Analysis of Data in  325j4b05.dat                                      
!     29-Day H.A.  Beginning  4-15-1998  at Hour 17.30  along 196 degrees             
!     -5820
!     1 57222694 14073452 22192976 1203 154 47261638 1292 478 26243445
!     2    0   0  658 796    0   0  4302938  6232349    0   0  2953258
!     3   563430   403046    0   0   92 316  1023593    0   0    0   0
!     4    0   0    0   0    0   0   49 617  251 639   833422   113482
!     5   34 800  398 178    0   0    0   0  3172976    0   0  3833513
!     6 12661394    0   0C
!     
!     Language:     lf95         
!     
!     Compiling/Linking Syntax:    ncepxlf nos_pred.f -o nos_pred.x
!     
!     Target Computer:   COMF machine, DEW/MIST at NCEP
!     
!     Estimated Execution Time: 
!     
!     Input Files:
!     Name             Directory  Location                   Description
!     
!     Output Files:
!     Name            Directory Location                    Description
!     
!     Libraries Used:     
!     
!     Error Conditions:
!     
!     Revisions:
!     Date                  Author                Description
!     09-26-2008              A ZHANG         put subroutines of "julian.f and gregorian.f in 
!     and commentted out the following include statements
!     include './library/julian.f'
!     include './library/gregorian.f'
!     For thansition to NCEP 
!     Remarks: 
!     
!     -----------------------------------------------------------------------------------------------------




!     PROGRAM PREDK7

!     Purpose:
!     1. Calculates residuals
!     2. Calculates a predicted series
!     **************************************************************
!     
!     UNIT=10 is the output file written in ASCII format.
!     UNIT=11 is the input observations in CDF or ASCII format.
!     
!     
!     
!     UNIT=         PATH NAME=                    INPUT      OUTPUT
!     
!     5       Redirected std. input (< pathname)   X
!     6                                                       X
!     10                                                       X
!     11                                            X
!     ************************************************************************************
!     NSTA =     NUMBER OF STATIONS TO PREDICT
!     CONV =     FACTOR FOR CONVERTING PREDICTED TIME SERIES TO NEW UNITS
!     **** Conversion options for time ***
!     
!     TCONV = 0   NO CONVERSION OF PREDICTED TIMES
!     TCONV = TIME MERIDIAN FOR WHICH THE KAPPA PRIMES IN THE HARMONIC
!     CONSTANTS WERE DERIVED. THIS OPTION IS USED TO CONVERT
!     THE TIMES TO GREENWICH IF THE CONSTANTS WERE CALCULATED
!     FOR A LOCAL TIME MERIDIAN. A REASON FOR USING THIS OPTION
!     IS IF COMPARISONS WITH ACTUAL DATA IS REQUIRED WHICH IS
!     IN GREENWICH TIME AND THE HARMONIC CONSTANTS WERE OBTAINED
!     FROM THE PREDICTION BRANCH WHERE HARMONIC CONSTANTS ARE
!     FOR LOCAL MERIDIANS ALWAYS
!     
!     TCONV changed to hours to shift predicted time series 
!     (positive = later) --- Chris Zervas (7/97)
!     
c     **** conversion option for using 2mn2 in the harmonic constants  
c     file versus the standard L2
c     
c     ***note*** it is node (33) which is re-calculated
c     
c     IL2 = 0 --- use the standard <L2> harmonic constants
c     1 --- use the <2MN2> harmonic constants
c     
!     
!*****************************************************************
!     
!     **NOTE(1)** USE FORMAT NO. 531 FOR HARMONIC CONSTANTS
!     TEMPORARY FORMULATION FORMAT 532 IS USED
!     
!******************************************************************
!     
!     
!     IPREDK = 0 -- USED TO CALCULATE THE DIFFERENCES BETWEEN A SET OF
!     PREDICTED AND OBSERVED SERIES. THIS CALCULATION IS
!     DONE USING AN INPUT OF OBSERVED VALUES IN ASCII
!     FORMAT AND OUTPUT IS WRITTEN IN AN ASCII FILE
!     
!     IPREDK = 1 -- USED TO CALCULATE THE DIFFERENCES BETWEEN A SET OF
!     PREDICTED AND OBSERVED SERIES. THIS CALCULATION IS
!     DONE USING AN INPUT OF OBSERVED VALUES IN CDF
!     FORMAT AND OUTPUT IS WRITTEN IN AN ASCII FILE
!     
!     **NOTE(1)** STARTING TIME FOR PREDICTIONS IS SET EQUAL TO
!     THE STARTING TIME DEFINED BY THE VALUE OF ISTART
!     **NOTE(2)** NOS1 IS CALCULATED BY THE TIMES OF THE OBSERVED
!     SERIES
!     
!     
!     IPRED = 2 -- USED TO CALCULATE A PREDICTED SERIES
!     
!     **NOTE(3)** THE PREDICTIONS AND DIFFERENCES CAN NOT BE PERFORMED
!     ACROSS 2 YEARS
!     **NOTE(4)** THE YEARS FOR EACH NSTA MUST BE THE SAME
!     **NOTE(5)** USE FORMAT NO. 532 FOR HARMONIC CONSTANTS
!     ***************************** modified IEL to match KINDAT
!     IEL =    THE ELEMENT OF THE DATA SERIES TO PERFORM CALCULATIONS
!     1   MAJOR/MINOR COMPONENTS OF VECTOR VARIABLE (I.E. CURRENT)
!     2   SCALAR VARIABLE (I.E. TIDAL HEIGHT)
!     3   TEMPERATURE (CDF INPUT FIELD)
!     4   CONDUCTIVITY (CDF INPUT FIELD)
!     6   PRESSURE (CDF INPUT FIELD)
!     IYRS=YEAR OF THE FIRST DATA POINT(CAN NOT GO ACROSS YRS)
!     IMMS=MONTH OF FIRST DATA POINT
!     IDDS=DAY OF FIRST DATA POINT
!     TIME=TIME OF FIRST DATA POINT
!     MON=MONTH OF LAST DATA POINT
!     IDDE=DAY OF LAST DATA POINT
!     TIMEL=TIME OF LAST DATA POINT
!     DELT= desired time interval in hours, delt=0.1 for 6 minutes data
!     NOS1=NUMBER OF DATA POINTS PER HOUR
!     XMAJOR ---- AXIS FOR MAJOR/MINOR COMPONENTS (MAKE SURE HARMONIC
!     CONSTANTS WERE DERIVED ALONG THIS AXIS)
!     IF A 0.0 IS READ --- 0 DEGRESS TRUE IS
!     ASSUMED. ALWAYS READ IN THE CONSTITUENTS
!     FOR THE MAJOR AXIS FIRST.
!///////////////////////////////////////////////////////////////
!     
!     
!     
      SUBROUTINE NOS_PRD(START_TIME,END_TIME,IEL,DELT,CONV,
     &     XMAJOR,AMP9,EPOC9,cdfout,STORN,STORX)  
      PARAMETER (MXDIM=9000)
      PARAMETER (XMISS=999.)
      character*80 cdfin,cdfout,BUFFER*100,START_TIME,END_TIME
      CHARACTER*10   ALIST
      CHARACTER*80   HEAD(2)
      DIMENSION   XDATA(14,50),ALIST(37),IHEAD(34),DAT(6),SUM(6),SMN(6)
     &     ,TIM(MXDIM),SPEED(MXDIM),DIREC(MXDIM),STORX(MXDIM)
     &     ,STORN(MXDIM)
      DIMENSION   AMP9(37),EPOC9(37)
      common/virt1/
     &     A(37),AMP(37),EPOC(37),XODE(114),VPU(114),
     &     XCOS(1025),ARG(37),TABHR(24),ANG(37),SPD0(37),
     &     EPOCH(37),AMPA(37),IYR(15),NUM(15),ISTA(6),NO(6),
     &     JODAYS(12),C(37)
!     
      real*8 jday,jday0,jday1,jbase_date,JULIAN,yearb,monthb,dayb,hourb
      COMMON /XDATA/XDATA
      DIMENSION TSTART(4)
!CCCCCCCCCCCCC
      common /speeds/spd
      common /names/lable
      real*8 spd(180),fff(180),vau(180)
      character*10 lable(180)
      integer order(37)
      logical LOUNT
      data order/1,3,2,5,21,4,22,28,24,30,13,25,12,8,16,11,27,15,14,
     &     35,37,36,34,33,20,18,10,9,19,17,32,26,7,29,6,23,31/

!CCCCCCCCCCCCC
      REAL NOS1
!     DATA (ALIST(I),I=1,37) /'M(2)','S(2)','N(2)','K(1)','M(4)','O(1)',
!     &'M(6)','MK(3)','S(4)','MN(4)','NU(2)','S(6)','MU(2)','2N(2)','OO(1
!     &)','LAMDA(2)','S(1)','M(1)','J(1)','MM','SSA','SA','MSF','MF','RHO
!     &(1)','Q(1)','T(2)','R(2)','2Q(1)','P(1)','2SM(2)','M(3)','L(2)','2
!     &MK(3)','K(2)','M(8)','MS(4)'/
      DATA (ALIST(I),I=1,37) /'M(2)      ','S(2)      ','N(2)      ',
     &     'K(1)      ','M(4)      ','O(1)      ',
     &     'M(6)      ','MK(3)     ','S(4)      ',
     &     'MN(4)     ','NU(2)     ','S(6)      ',
     &     'MU(2)     ','2N(2)     ','OO(1)     ',
     &     'LAMDA(2)  ','S(1)      ','M(1)      ',
     &     'J(1)      ','MM        ','SSA       ',
     &     'SA        ','MSF       ','MF        ',
     &     'RHO(1)    ','Q(1)      ','T(2)      ',
     &     'R(2)      ','2Q(1)     ','P(1)      ',
     &     '2SM(2)    ','M(3)      ','L(2)      ',
     &     '2MK3(3)   ','K(2)      ','M(8)      ',
     &     'MS(4)     '/
      LIN = 5
      LOUT = 6
 
      print *,'test', trim(START_TIME),trim(END_TIME)
      do i=1,37
        print *,'i=',i,amp9(i),epoc9(i)
      enddo


!     
!     read (lin,*)   nsta,ipredk,conv,tconv,il2
      NSTA=1
      IPREDK=2
      CONV=1.0
      TCONV=0.0
      IL2=0
!     
      IF (IPREDK.GT.2.OR.IPREDK.LT.0)   then
         print *,'Error in IPREDK value'
         stop
      endif
!     
!     DEVELOP COSINE TABLE
!     
      H = 0.00153398078789
      DO I=1,1024
         XCOS(I) = COS(H*(I-1))
      ENDDO
      XCOS(1025) = 0.0
      ms0 = 1
      CON = 1024. / 90.
!     ---
      doJOB: DO JOB=1,NSTA
!        read(lin,'(a80)')cdfout
!        READ (LIN,*) IEL,IMMS,IDDS,TIME,IMME,IDDE,TIMEL,DELT,IYRS,IYRE,
!        &  XMAJOR
!        CALL GETARG(1,BUFFER)
         READ(START_TIME,'(i4,4I2)')IYRS,IMMS,IDDS,IHHS,MNS
         TIME=IHHS+MNS/60.
!        CALL GETARG(2,BUFFER)
         READ(END_TIME,'(i4,4I2)')IYRE,IMME,IDDE,IHHE,MNE
         TIMEL=IHHE+MNE/60.
!        CALL GETARG(3,BUFFER)
!        READ(BUFFER,*)IEL
!        CALL GETARG(4,BUFFER)
!        READ(BUFFER,*)DELT
!        CALL GETARG(5,BUFFER)
!        READ(BUFFER,*)CONV
!        CALL GETARG(6,BUFFER)
!        READ(BUFFER,*)XMAJOR
!        CALL GETARG(7,cdfin)
!        CALL GETARG(8,cdfout)
!        call ncrght(cdfin,nct)
         yearb=IYRS
         monthb=1.
         dayb=1.
         hourb=0.
         jbase_date=JULIAN(yearb,monthb,dayb,hourb)
!        stop
         NOS1=1.0/DELT             !!!  data points per hour
!        write(6,*)'run pred.f from ',IYRS,IMMS,IDDS,IHHS, ' to ',
!        &   IYRE,IMME,IDDE,IHHE
!        CALL COMPIN(MXDIM,NOS1,IYRS,IMMS,IDDS,TIME,IYRE,IMME,IDDE,TIMEL,
!        1DELT,NPTS,TSTART)
         yearb=IYRS
         monthb=IMMS
         dayb=IDDS
         hourb=IHHS+MNS/60.0
         jday0=JULIAN(yearb,monthb,dayb,hourb)
         yearb=IYRE
         monthb=IMME
         dayb=IDDE
         hourb=IHHE+MNE/60.0
         jday1=JULIAN(yearb,monthb,dayb,hourb)
         TSTART(4) = IYRS
         TSTART(3) = jday0-jbase_date
         TSTART(2) = IHHS
         TSTART(1) =MNS
         NPTS=INT((jday1-jday0)*24/DELT+1+0.1)
!**** 
         do119: DO
            CIND = NOS1
!           open(30,file=trim(cdfin) )
            AMP(1:37)=AMP9(1:37)
            EPOC(1:37)=EPOC9(1:37)

!           READ (30,550)  HEAD(1),HEAD(2)
!           READ (30,532)DATUM,ISTA(1),NO(1),(AMP(J),EPOC(J),J=1,7),
!           & ISTA(2),NO(2),
!           & (AMP(J),EPOC(J),J=8,14),ISTA(3),NO(3),(AMP(J),EPOC(J),J=15,21),
!           & ISTA(4),NO(4),(AMP(J),EPOC(J),J=22,28),ISTA(5),NO(5),(AMP(J),
!           & EPOC(J),J=29,35),ISTA(6),NO(6),(AMP(J),EPOC(J),J=36,37)
!           DO 113 L = 1,5
!           IF (ISTA(L).NE.ISTA(L+1)) THEN            
!           PRINT *,'31H STATION NUMBERS NOT CONSISTENT' 
!           STOP
!           ENDIF
!           ENDDO
!           DO L = 1,6
!           IF (NO(L).NE.L) THEN
!           PRINT *,'27H STATION CARDS OUT OF ORDER'
!           RETURN
!           ENDIF
!           ENDDO
!     
!           CONVERT CONSTANTS IF TCONV IS NOT EQUAL TO ZERO
!           TCONV IS THE TIME MERIDIAN
!     
!           IF (TCONV.EQ.0.0)   GO TO 120
            ifTCONV: IF (TCONV .NE. 0.0) THEN
!     
               PRINT '(1X,4H    )'
               PRINT *,'(A80)',HEAD(1),HEAD(2)
               PRINT '(1X,4H    )'
               WRITE (LOUT,'(A,F6.2,A)')' Values of the Epochs before '
     &              ,TCONV,' hour time shift'
!     
               DO J=1,37
                  IF (AMP(J).EQ.0.0)CYCLE
                  EPOC(J) = EPOC(J) + A(J)*TCONV
                  DO
                     IF (EPOC(J).LE.360.)EXIT   
                     EPOC(J) = EPOC(J) - 360.0
                  ENDDO
               ENDDO 
            ENDIF ifTCONV
             if(ms0.ne.2) THEN 
!              CCCCCCCusing equarg replace yrcrds.f
               length=int(jday1-jday0)+1
               call equarg (37,IYRS,1,1,365,lable(1),fff(1),vau(1))
!              call equarg (37,IYRS,IMMS,IDDS,length,lable(1),fff(1),vau(1))
               do j=1,37
!                 VPU(J)=VAU(ORDER(J))
!                 XODE(J)=FFF(ORDER(J))
                  VPU(J)=VAU(J)
                  XODE(J)=FFF(J)
!     
!                 WRITE(6,'(I2,2x,A10,1x,F12.7,2X,F5.1,2X,F6.4,2x,F10.5,2x,F8.2)')
!                 &  J, ALIST(J),A(J),VPU(J),XODE(J),fff(j),vau(j)  !!AMP(J),EPOC(j)
               end do
            ENDIF
!CCCCCCCCCCCCCC
            C(1:37) = A(1:37) * (CON/CIND)
!           SET UP TABLES FOR NON-ZERO CONSTITUENTS
            K = 0
            DO J = 1,37
               IF (AMP(J).EQ.0.0) CYCLE
               K = K + 1
               AMPA(K) = AMP(J) * XODE(J)
               TEMX = VPU(J) - EPOC(J)
               IF (TEMX .LT. 0.) TEMX = TEMX + 360.
               EPOCH(K) = TEMX * CON
               SPD0(K) = C(J)
            ENDDO
            NOCON = K
!     
!           OPERATING TABLES NOW STORED AS AMPA(K),EPOCH(K),SPD0(K)
!     
!****       CHECK LENGTH OF SERIES
!     
            IF (NPTS.GT.MXDIM)THEN   
               NPTS = MXDIM
            ENDIF
!           WRITE (LOUT,''(/' Total number of prediction times = ',I10))   
!           &   NPTS
!     
!****      DETERMINE FIRST HOUR OF TIME PERIOD  at 00:00 of Jan. 1, first=0.0
!     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCmodified by zaj on May 13, 2004
!           CALL CONCTJ (ILJD,IMMS,IDDS,IYRS)
!           FIRST=((ILJD-1)*24.+TIME)* CIND
            FIRST=(jday0-jbase_date)*24.* CIND
!           write(6,*)'Time of first prediction=',IYRS,IMMS,IDDS,IHHS,MNS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
            STORX(1:MXDIM) = 0.
            KOUNT = 0
            KT = 0
!     
!     ---
            doK: DO K = 1,NPTS
               LOUNT=.FALSE.
!**** THE PREDICTION/TIME STEP IS STORED IN VARIABLE PRED
               PRED = 0.0
               IF (KOUNT <= 0) THEN
                  KOUNT = 1
                  LOUNT=.TRUE.
              ENDIF
               do231: DO
                  ifNOCON: IF (NOCON /= 0)  THEN
                     IF (LOUNT) THEN
                       LOUNT = .FALSE.
                       DO J = 1,NOCON
                           ARGU = SPD0(J) * FIRST + EPOCH(J)
                           ARG(J) = AMOD(ARGU,4096.)
                        ENDDO
                     ELSE
                        LOUNT=.TRUE.
                        doJ: DO J = 1,NOCON
                           ARG(J) = ARG(J) + SPD0(J)
                           DO
                              IF (ARG(J).LT.4096.) CYCLE doJ
                              ARG(J) = ARG(J) - 4096.
                           ENDDO
                        ENDDO doJ
                     ENDIF
                    do374: DO J = 1,NOCON
                        IF (ARG(J) <= 1024.) THEN
                           ANG(J) = ARG(J)
                           NP = ANG(J) + 1.5
                           PRED = PRED + AMPA(J) * XCOS(NP)
                           CYCLE do374
                        ELSE
                           IF (ARG(J)  > 2048. ) THEN
                              IF (ARG(J) > 3072. ) THEN 
                                 ANG(J) = 4096. - ARG(J)
                                 NP = ANG(J) + 1.5
                                 PRED = PRED + AMPA(J) * XCOS(NP)
                                 CYCLE do374
                              ELSE
                                 ANG(J) = ARG(J) - 2048.
                                 NP = ANG(J) + 1.5
                                 PRED = PRED - AMPA(J) * XCOS(NP) 
                                 CYCLE do374
                              ENDIF
                           ELSE
                              ANG(J) = 2048. - ARG(J)
                             NP = ANG(J) + 1.5
                              PRED = PRED - AMPA(J) * XCOS(NP)
                              CYCLE do374
                           ENDIF
                        ENDIF 
                     ENDDO do374
                  ENDIF ifNOCON                    
                  IF (K.NE.NPTS) EXIT do231 
                  IF (IPREDK.EQ.0) EXIT do231 
                  IF (KT.EQ.1) THEN
                     CKSUM = CHECK - PRED
                     EXIT do231
                  ENDIF
                  FIRST = FIRST + NPTS - 1.
                  KT = 1
                  CHECK = PRED
                  PRED = 0.0
                ENDDO do231
!     
!****          CONVERT RESULTS ACCORDING TO CONV
!     
               PRED = (PRED + DATUM) * CONV
!     
!****          STORE THE RESULTS
!          
               STORX(K) = PRED
            ENDDO doK
            NOS2 = 0
            IF(IEL.EQ.1) NOS2 = 1
            IF(IEL.EQ.1.AND.XMAJOR.NE.0) NOS2 = 2
            IF (NOS2.NE.0) THEN
               IF(ms0.EQ.2) THEN
                  IF(NOS2.EQ.1) THEN
                     PRINT *,' Harmonic Constants (East)'
                  ELSE
                     PRINT *,' Harmonic Constants (Minor Axis) ------'
                  ENDIF
               ELSE
                  IF(NOS2.EQ.1) THEN
                     PRINT *,' Harmonic Constants (North)'
                  ELSE
                     PRINT *,' Harmonic Constants (Major Axis) ------'
                  ENDIF
               ENDIF
            ENDIF 
            PRINT *,'    '
!           IF(TCONV.EQ.0.0) PRINT 5007
!           PRINT 5902

            DO IZ =1,37
               IF(AMP(IZ).EQ.0.0) THEN
!           PRINT *,'(1X,A10)', ALIST(IZ)
               ELSE        
!           PRINT *,'((1X,A10,2X,F7.3,3X,F6.2))',ALIST(IZ),AMP(IZ),EPOC(IZ)
               ENDIF
            ENDDO
            IF (NOS2 > 0 ) THEN
               IF (ms0 == 2) EXIT do119
               ms0 = 2
            ENDIF
!     
!****       STORES THE MAJOR AXIS
!     
            STORN(1:NPTS) = STORX(1:NPTS)
            IF (NOS2 > 0) THEN
               CYCLE  do119
            ELSE 
               EXIT do119
            ENDIF
         ENDDO do119
!     
!****    FORM NEW DATA ARRAY
!     
         call ncrght(cdfout,nct)
         open(10,file=cdfout(1:nct),status='unknown',form='formatted')
         idet = 0
         IPRED = 1
         jday=jday0
      
!        ---
         DO
            call GREGORIAN(jday,yearb,monthb,dayb,hourb)
            IYEAR=INT(yearb)
            ICM=int(monthb+0.001)
            ICD=INT(dayb+0.001)
            IHR=INT(hourb+0.001)
            IMN=INT((hourb-IHR)*60+0.01)
            ISEC=0
            dayj=jday-jbase_date+1.
            IF(IEL.NE.1) THEN 
               write(10,'(f10.5,I5,4I3,4f10.4)')dayj,IYEAR,ICM,ICD,IHR
     &              ,IMN,storn(ipred)
            ELSE
               CALL VELDIR(STORN(IPRED),STORX(IPRED),DR,SP)
               DR = DR + XMAJOR
               IF (DR.GT.360.)   DR = DR - 360.
               u=sp*sin(dr*3.1415926/180.)
               v=sp*cos(dr*3.1415926/180.)
!              write(10,'(f10.5,I5,4I3,4f10.4)')dayj,IYEAR,ICM,ICD,IHR,IMN,sp,dr,u,v
               write(10,'(i4,5i3.2,2f11.4)')IYEAR,ICM,ICD,IHR,IMN,ISEC
     &            ,sp,dr 
            ENDIF
            IPRED = IPRED + 1
            IF (IPRED.LE.NPTS)THEN  
               jday=jday+delt/24. 
            ELSE
               EXIT
            ENDIF
         ENDDO 
!     ---
         ms0 = 1
      ENDDO doJOB
      CLOSE(10)
!     
      RETURN
!     PRINT *,'28H YEAR NUMBERS NOT CONSISTENT' 
!     STOP
!     PRINT *,'24H YEAR CARDS OUT OF ORDER' 
!     STOP
      RETURN
      END
