C ifort adj_tides.f -o adj_tides
C -I$NETCDF_INC -L$NETCDF_LIB -lnetcdff -L$LIBnos -lnosutil
C Purpose:    This Program is used to adjust harmonic constants
C             (amplitude and phase) on model grid for specified year.
C             The resulted harmonic constants can be used to add tides
C             or detide modeled water levels. 
C             Nodal factors and equilibrium arguments for the middle of
C             each year (day 183 or 184) are used in the same calender
C             year regardless of the length of time series. 
C             This is consistent with CO-OPS tidal prediction programs
c input: HC_FILE !filename for the input harmonic constants file
c        FOUT    !filename for the output harmonic constants file
c        IYEAR   !year for calculating nodal factors and equilibrium arguments
c        IADJ    !=0 to take out nodal factor and equilibrium adjustment
c                !=1 to apply nodal factors and equilibrium arguments
c                !=2 to change phase reference time only



      include 'netcdf.inc'
      parameter (nspd=37)
      real*8 a(nspd),xode(nspd),vpu(nspd), fff(180),vau(180)
      real*8,allocatable:: tide_period(:),Eampm(:,:,:),Epham(:,:,:)
      real*8 YR, MR, DR, HR, JULIAN, jday, jdayb, cyc
      character*10 alist(nspd),labl(180)
      character*120 BUFFER,HC_FILE,FOUT,command
      character*19 ref_time
      character*10,allocatable:: tidenames(:)

CCCCCCCCCCCCCC  used by equarg.f
      DATA (ALIST(I),I=1,37) /'M(2)      ','S(2)      ','N(2)      ',
     1                        'K(1)      ','M(4)      ','O(1)      ',
     2                        'M(6)      ','MK(3)     ','S(4)      ',
     3                        'MN(4)     ','NU(2)     ','S(6)      ',
     4                        'MU(2)     ','2N(2)     ','OO(1)     ',
     5                        'LAMBDA(2) ','S(1)      ','M(1)      ',
     6                        'J(1)      ','MM        ','SSA       ',
     7                        'SA        ','MSF       ','MF        ',
     8                        'RHO(1)    ','Q(1)      ','T(2)      ',
     9                        'R(2)      ','2Q(1)     ','P(1)      ',
     1                        '2SM(2)    ','M(3)      ','L(2)      ',
     2                        '2MK(3)   ','K(2)      ','M(8)      ',
     3                        'MS(4)     '/
      DATA (A(I), I=1,37)/ 28.9841042d0,  30.0000000d0,  28.4397295d0,
     115.0410686d0,57.9682084d0,13.9430356d0,86.9523127d0,44.0251729d0,
     260.0000000d0,57.4238337d0,28.5125831d0,90.0000000d0,27.9682084d0,
     327.8953548d0,16.1391017d0,29.4556253d0,15.0000000d0,14.4966939d0,
     415.5854433d0, 0.5443747d0, 0.0821373d0, 0.0410686d0, 1.0158958d0,
     5 1.0980331d0,13.4715145d0,13.3986609d0,29.9589333d0,30.0410667d0,
     612.8542862d0,14.9589314d0,31.0158958d0,43.4761563d0,29.5284789d0,
     742.9271398d0,30.0821373d0, 115.9364169d0,58.9841042d0/

      read(5,'(a120)')BUFFER
      do i=1,len_trim(BUFFER)
         if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
            BUFFER(i:I)=' '
         endif
      enddo
      HC_FILE=trim(adjustL(BUFFER))
      read(5,'(a120)')BUFFER
      do i=1,len_trim(BUFFER)
        if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
            BUFFER(i:I)=' '
         endif
      enddo
      FOUT=trim(adjustL(BUFFER))
c      read(5,'(a120)')BUFFER
c      do i=1,len_trim(BUFFER)
c         if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
c            BUFFER(i:I)=' '
c         endif
c      enddo
c      BUFFER=trim(adjustL(BUFFER))
c      read(BUFFER,'(I4,3i2)')base_date
      read(5,*) IYR
      read(5,*) IADJ
      IF (IADJ .eq. 1) THEN
        print *, 'Adjust tides for ', IYR
      ELSEIF (IADJ .eq. 2) THEN
        print *, 'Adjust for phase only for ', IYR
      ELSEIF (IADJ .eq. 0) THEN
        print *, 'Take out tidal adjustments for ', IYR
      ELSE 
        print *, 'ERROR: IADJ=', IAJ, 'not defined!'
        stop
      ENDIF
      YR=IYR
      MR=1
      DR=1
      HR=0
      jday=JULIAN(YR,MR,DR,HR)
      do i=1,nspd
         call name (a(i),labl(i),isub,inum,1)
      enddo
      call equarg (37,IYR,1,1,365,labl(1),fff(1),vau(1))
      WRITE(*,*)'node factor and equilibrium arguments for year: ',IYR
      DO j=1,37
        VPU(J)=VAU(J)
        XODE(J)=FFF(J)
C        WRITE(6,'(I2,2x,A10,1x,F12.7,2X,F6.1,2X,F9.4)')
C     1  J, ALIST(J),A(J),VPU(J),XODE(J)
      ENDDO
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
CC read in Harmonic Constiutents from HC_FILE
      STATUS = NF_OPEN(trim(HC_FILE),NF_NOWRITE, NCID)
      IF(STATUS .NE. NF_NOERR)then
           print *,'error message= ',status
           stop 'open HC_FILE file failed'
      ENDIF
      command='cp '//trim(HC_FILE)//' '//trim(FOUT)
      print*, command
      call system(command) 
      STATUS = NF_OPEN(trim(FOUT),NF_WRITE, NCID_OUT)
      IF(STATUS .NE. NF_NOERR)then
           print *,'error message= ',status
           stop 'open FOUT file failed'
      ENDIF
      STATUS = NF_INQ(NCID,NDIMS,NVARS,NGATTS,IDUNLIMDIM)
      DO I=1,NDIMS
          STATUS = NF_INQ_DIM(NCID,i,BUFFER,ILATID)
          STATUS = NF_INQ_DIMLEN(NCID,i,latid)
          if(trim(BUFFER) .eq. 'eta_rho')JROMS=latid
          if(trim(BUFFER) .eq. 'xi_rho')IROMS=latid
          if(trim(BUFFER) .eq. 'tide_period')NC=latid
          if(trim(BUFFER) .eq. 'ten')NCHAR=latid
      ENDDO
      print *, 'dimensions', IROMS, JROMS, NC, NCHAR
      ALLOCATE (tide_period(NC),tidenames(NC) )
      ALLOCATE (Eampm(IROMS,JROMS,NC),Epham(IROMS,JROMS,NC))
      Epham=0.0
      Eampm=0.0
      STATUS = NF_INQ_VARID(NCID,'tidal_constituents',IDVAR)
      STATUS = NF_GET_VAR_TEXT(NCID,IDVAR,tidenames)
      IF(STATUS .NE. NF_NOERR)then
         print *, 'error in reading tide names'
      ENDIF
      STATUS = NF_INQ_VARID(NCID,'tide_period',IDVAR)
      STATUS = NF_GET_VAR_DOUBLE(NCID,IDVAR,tide_period)
      IF(STATUS .NE. NF_NOERR)then
         print *, 'error in reading tide period'
      ENDIF
      STATUS = NF_INQ_VARID(NCID,'zeta_amp',IDVAR)
      STATUS = NF_GET_VAR_DOUBLE(NCID,IDVAR,Eampm) 
      IF(STATUS .NE. NF_NOERR)then
         print *, 'error in reading amp'
      ENDIF
      STATUS = NF_INQ_VARID(NCID,'zeta_phase',IDVAR)
      STATUS = NF_GET_VAR_DOUBLE(NCID,IDVAR,Epham) 
      IF(STATUS .NE. NF_NOERR)then
         print *, 'error in reading phase'
      ENDIF
      STATUS = NF_GET_ATT_TEXT(NCID,IDVAR,'ref_time',BUFFER)
      IF(STATUS .NE. NF_NOERR)then
         print *, 'error in reading ref_time'
         jdayb=jday
      ELSE
         print *, BUFFER
         READ(BUFFER(1:10),'(I4,2(1x,I2))')IYRR,IMMR,IDDR
         YR=IYRR
         MR=IMMR
         DR=IDDR
         HR=0
         jdayb=JULIAN(YR,MR,DR,HR)
      ENDIF
      DO N=1,NC
         print *, 'adjust ', tidenames(N)
         DO NN=1,nspd
            if (abs(tide_period(n) - 360./a(NN)) .lt. 10E-5) then
               xfac=XODE(NN)
               efac=VPU(NN)
               print *, N, tidenames(N),tide_period(n),xfac, efac
c               print *, NN, labl(NN)
               exit
            endif
         ENDDO
         cyc=(jday-jdayb)*24/tide_period(n)  ! tide_period in hours
         Pha_Ref=(cyc-floor(cyc))*360.
c         print *,'phase shift due to different ref_time ',Pha_Ref
         DO J=1,JROMS
         DO I=1,IROMS
C adjust tides for specified year based on harmonic constants
           IF (IADJ .eq. 1) THEN
             Eampm(I,J,N)=Eampm(I,J,N)*xfac
             Epham(I,J,N)=Epham(I,J,N)-efac
             IF(Epham(I,J,N).LT.-180.0)Epham(I,J,N)=Epham(I,J,N)+360.0
             IF(Epham(I,J,N).GT.180.0)Epham(I,J,N)=Epham(I,J,N)-360.0
C Take out adjustments from amp and phase (relative to specified time) in
C a specified year to get the harmonic constants
           ELSEIF (IADJ .eq. 0) THEN
             Eampm(I,J,N)=Eampm(I,J,N)/xfac
             Epham(I,J,N)=Epham(I,J,N)-Pha_Ref+efac
             IF(Epham(I,J,N).LT.-180.0)Epham(I,J,N)=Epham(I,J,N)+360.0
             IF(Epham(I,J,N).GT.180.0)Epham(I,J,N)=Epham(I,J,N)-360.0
C Adjust phase reference time from specified time to beginning of year
           ELSEIF (IADJ .eq. 2) THEN
             Epham(I,J,N)=Epham(I,J,N)-Pha_Ref
             IF(Epham(I,J,N).LT.-180.0)Epham(I,J,N)=Epham(I,J,N)+360.0
             IF(Epham(I,J,N).GT.180.0)Epham(I,J,N)=Epham(I,J,N)-360.0
           ELSE
             print *, 'ERROR: IADJ not defined!'
             stop
           ENDIF
         ENDDO
         ENDDO
      ENDDO

C     Write adjusted harmonic constants to the output file FOUT
      WRITE(ref_time,'(I4,"-01-01 00:00:00")') IYR
      print *, 'ref_time= ', ref_time
      STATUS = NF_INQ_VARID(NCID_OUT,'zeta_amp',IDVAR)
      STATUS = NF_PUT_VAR_DOUBLE(NCID_OUT,IDVAR,Eampm)
      IF(STATUS .NE. NF_NOERR)then
         print *, 'error in writing amp'
      ENDIF
      STATUS = NF_INQ_VARID(NCID_OUT,'zeta_phase',IDVAR)
      STATUS = NF_PUT_VAR_DOUBLE(NCID_OUT,IDVAR,Epham)
      IF(STATUS .NE. NF_NOERR)then
         print *, 'error in writing phase'
      ENDIF
      STATUS = NF_INQ_ATTID(NCID_OUT,IDVAR,'ref_time',NATT)
      IF(STATUS .NE. NF_NOERR)then
        print *, "error in requiring ref_time attribute"
        IF (IADJ .NE. 0) THEN
          STATUS=NF_REDEF(NCID_OUT)
          STATUS=NF_PUT_ATT_TEXT(NCID_OUT,IDVAR,'ref_time',19,ref_time)
          STATUS=NF_ENDDEF(NCID_OUT)
        ENDIF
      ELSE
        IF (IADJ .EQ. 0) THEN
          print *, "remove ref_time for zeta_phase"
          STATUS=NF_REDEF(NCID_OUT)
          STATUS=NF_DEL_ATT(NCID_OUT,IDVAR,'ref_time')
          STATUS=NF_ENDDEF(NCID_OUT)
        ELSE
          print *, "rewrite ref_time for zeta_phase"
          STATUS=NF_PUT_ATT_TEXT(NCID_OUT,IDVAR,'ref_time',19,ref_time)
        ENDIF
      ENDIF
      STATUS = NF_CLOSE(NCID)
      STATUS = NF_CLOSE(NCID_OUT)
      END 
