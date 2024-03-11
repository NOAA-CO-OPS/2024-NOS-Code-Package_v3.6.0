!       Subroutine Name: equarg.f 
!
!  Contact: NOS/CO-OPS Aijun Zhang
!           Phone: 240-533-0591   Email: aijun.zhang@noaa.gov
!
!       Abstract:  this subroutine is used to calculate equilibrium
!       arguments and node factors.
!
!       History Log:
!           04/01/2019
!
!       Usage: call equarg from nos_ofs_tideprediction.f 
!
!       Argument Input:
!                nsped   -  number of constituents to be calculated
!                IYR    -   month of first data point
!                ICM    -  day  of first data point
!                ICD    -  year of first data point
!                length -  length of series to be analyzed (in days)
!                label   - names of the tidal constituents
!
!        Argument Output:
!               fff     - node factor for each constituent 
!               vau     - equilibrium argument for each constitent


      BLOCK DATA
      PARAMETER (MXDIM=200000)
      integer (kind=4), parameter :: max_constants = 175
CCCCCCCCCCCCCC
      common /speeds/spd
      common /names/label
      common /mmss/ms
      character*10 label(180)
      double precision spd(180)
      integer ms(180)
CCCCCCCCCCCCCC
      common/datec/idtbc(12,2),iltbc(12,2)
      common/datej/idtbj(12),iltbj(12)
      common/virt1/
     1            A(37),AMP(37),EPOC(37),XODE(114),VPU(114),
     2            XCOS(1025),ARG(37),TABHR(24),ANG(37),SPD0(37),
     3            EPOCH(37),AMPA(37),IYR(15),NUM(15),ISTA(6),NO(6),
     4            JODAYS(12),C(37)
      DATA ((IDTBC(I,J),J=1,2),I=1,12)/1,31,32,59,60,90,91,120,121,151,
     1                                  152,181,182,212,213,243,244,273,
     2                                  274,304,305,334,335,365/
      DATA  ((ILTBC(I,J),J=1,2),I=1,12)/1,31,32,60,61,91,92,121,122,
     1                              152,153,182,183,213,214,244,245,
     2                                    274,275,305,306,335,336,366/
      DATA (IDTBJ(I),I=1,12)/1,32,60,91,121,152,182,213,244,274,305,335/
      DATA (ILTBJ(I),I=1,12)/1,32,61,92,122,153,183,214,245,275,306,336/
      DATA(TABHR(I), I=1,24)/  -24.,  720., 1392., 2136., 2856., 3600.,
     1    4320., 5064., 5808., 6528., 7272., 7992.,  -24.,  720., 1416.,
     2    2160., 2880., 3624., 4344., 5088., 5832., 6552., 7296., 8016./
      DATA (A(I), I=1,37)/        28.9841042,  30.0000000,  28.4397295,
     1   15.0410686,  57.9682084,  13.9430356,  86.9523127,  44.0251729,
     2   60.0000000,  57.4238337,  28.5125831,  90.0000000,  27.9682084,
     3   27.8953548,  16.1391017,  29.4556253,  15.0000000,  14.4966939,
     4   15.5854433,   0.5443747,   0.0821373,   0.0410686,   1.0158958,
     5    1.0980331,  13.4715145,  13.3986609,  29.9589333,  30.0410667,
     6   12.8542862,  14.9589314,  31.0158958,  43.4761563,  29.5284789,
     7   42.9271398,  30.0821373, 115.9364169,  58.9841042/

      DATA (JODAYS(LL),LL=1,12)/31,28,31,30,31,30,31,31,30,31,30,31/
CCCCCCCCCCCCCCCCCCCCCCCC
      DATA (SPD(I), I=1,max_constants)/           
     1 28.9841042d0,  30.0000000d0,  28.4397295d0,  15.0410686d0, 
     2 57.9682084d0,  13.9430356d0,  86.9523127d0,  44.0251729d0, 
     3 60.0000000d0,  57.4238337d0,  28.5125831d0,  90.0000000d0, 
     4 27.9682084d0,  27.8953548d0,  16.1391017d0,  29.4556253d0, 
     5 15.0000000d0,  14.4966939d0,  15.5854433d0,   0.5443747d0, 
     6  0.0821373d0,   0.0410686d0,   1.0158958d0,   1.0980331d0, 
     7 13.4715145d0,  13.3986609d0,  29.9589333d0,  30.0410667d0, 
     8 12.8542862d0,  14.9589314d0,  31.0158958d0,  43.4761563d0, 
     9 29.5284789d0,  42.9271398d0,  30.0821373d0, 115.9364169d0, 
     1 58.9841042d0,  12.9271398d0,  14.0251729d0,  14.5695476d0, 
     2 15.9748272d0,  16.0569644d0,  30.5443747d0,  27.4238337d0, 
     3 28.9019669d0,  29.0662415d0,  26.8794590d0,  26.9523126d0, 
     4 27.4966873d0,  31.0980331d0,  27.8039338d0,  28.5947204d0, 
     5 29.1483788d0,  29.3734880d0,  30.7086493d0,  43.9430356d0, 
     6 45.0410686d0,  42.3827651d0,  59.0662415d0,  58.4397295d0, 
     7 57.4966873d0,  56.9523127d0,  58.5125831d0,  56.8794590d0, 
     8 59.5284789d0,  71.3668693d0,  71.9112440d0,  73.0092770d0, 
     9 74.0251728d0,  74.1073100d0,  72.9271398d0,  71.9933813d0, 
     1 72.4649023d0,  88.9841042d0,  86.4079380d0,  87.4238337d0, 
     2 87.9682084d0,  85.3920421d0,  85.8635632d0,  88.5125831d0, 
     3 87.4966873d0,  89.0662415d0,  85.9364168d0,  86.4807916d0, 
     4 88.0503457d0, 100.3509735d0, 100.9046318d0, 101.9112440d0, 
     5103.0092771d0, 116.4079380d0, 116.9523127d0, 117.9682084d0, 
     6114.8476674d0, 115.3920422d0, 117.4966873d0, 115.4648958d0, 
     7116.4807916d0, 117.0344500d0, 118.0503457d0, 129.8887360d0, 
     8130.4331108d0, 130.9774855d0, 131.9933813d0, 144.3761464d0, 
     9144.9205211d0, 145.3920422d0, 145.9364169d0, 146.4807916d0, 
     1146.9523127d0, 160.9774855d0, 174.3761464d0, 174.9205211d0, 
     2175.4648958d0, 175.9364169d0,  14.9178647d0,  15.0821353d0, 
     3 15.1232059d0,  15.5125897d0,  30.6265119d0,  27.3416965d0, 
     4 42.9271397d0,  60.0821373d0,  16.1391016d0,  12.8450026d0, 
     5 26.9615963d0,  27.5059710d0,  28.6040041d0,  57.5059710d0, 
     6 58.5218668d0,  85.4013258d0,  85.9457005d0,  86.4900752d0, 
     7 87.5059710d0,  88.5218668d0, 115.4741794d0, 116.4900752d0, 
     8117.5059710d0, 146.4900752d0, 175.4741794d0,  41.9205276d0, 
     9 15.1232058d0,  14.8767942d0,  30.0000001d0,  29.9178627d0, 
     1 30.1642746d0,  29.9178666d0,  59.9589333d0,  59.9178627d0, 
     2 60.2464119d0,  59.8767999d0,  28.9430356d0,   1.0569644d0, 
     3  0.5490165d0,   0.5079479d0,   0.0410667d0,   0.1232059d0, 
     4  0.1642746d0,   0.2464118d0,   0.3285841d0,   0.4106864d0, 
     5  0.4928237d0,   0.9856473d0,  45.0000000d0,  75.0000000d0, 
     6 27.8860712d0,  30.0410686d0,  43.4807981d0,  44.9589314d0, 
     7 45.1232059d0,  56.3258007d0,  56.8701754d0,  57.8860712d0, 
     8105.0000000d0, 120.0000000d0, 150.0000000d0 /

! constituent names in same order as spd
! Yes, I know trailing spaces are not needed, it just makes
! all the names line up nicely.
      DATA (label(I),I=1,max_constants) / 
     1 "M(2)      ","S(2)      ","N(2)      ","K(1)      ", 
     2 "M(4)      ","O(1)      ","M(6)      ","MK(3)     ", 
     3 "S(4)      ","MN(4)     ","NU(2)     ","S(6)      ", 
     4 "MU(2)     ","2N(2)     ","OO(1)     ","LAMBDA(2) ", 
     5 "S(1)      ","M(1)      ","J(1)      ","MM        ", 
     6 "SSA       ","SA        ","MSF       ","MF        ", 
     7 "RHO(1)    ","Q(1)      ","T(2)      ","R(2)      ", 
     8 "2Q(1)     ","P(1)      ","2SM(2)    ","M(3)      ", 
     9 "L(2)      ","2MK(3)    ","K(2)      ","M(8)      ", 
     1 "MS(4)     ","SIGMA(1)  ","MP(1)     ","CHI(1)    ", 
     2 "2PO(1)    ","SO(1)     ","MSN(2)    ","MNS(2)    ", 
     3 "OP(2)     ","MKS(2)    ","2NS(2)    ","MLN2S(2)  ", 
     4 "2ML2S(2)  ","SKM(2)    ","2MS2K(2)  ","MKL2S(2)  ", 
     5 "M2(KS)(2) ","2SN(MK)(2)","2KM(SN)(2)","SO(3)     ", 
     6 "SK(3)     ","NO(3)     ","MK(4)     ","SN(4)     ", 
     7 "2MLS(4)   ","3MS(4)    ","ML(4)     ","N(4)      ", 
     8 "SL(4)     ","MNO(5)    ","2MO(5)    ","2MK(5)    ", 
     9 "MSK(5)    ","3KM(5)    ","2MP(5)    ","3MP(5)    ", 
     1 "MNK(5)    ","2SM(6)    ","2MN(6)    ","MSN(6)    ", 
     2 "2MS(6)    ","2NMLS(6)  ","2NM(6)    ","MSL(6)    ", 
     3 "2ML(6)    ","MSK(6)    ","2MLNS(6)  ","3MLS(6)   ", 
     4 "2MK(6)    ","2MNO(7)   ","2NMK(7)   ","2MSO(7)   ", 
     5 "MSKO(7)   ","2MSN(8)   ","3MS(8)    ","2(MS)(8)  ", 
     6 "2(MN)(8)  ","3MN(8)    ","2MSL(8)   ","4MLS(8)   ", 
     7 "3ML(8)    ","3MK(8)    ","2MSK(8)   ","2(MN)K(9) ", 
     8 "3MNK(9)   ","4MK(9)    ","3MSK(9)   ","4MN(10)   ", 
     9 "M(10)     ","3MNS(10)  ","4MS(10)   ","3MSL(10)  ", 
     1 "3M2S(10)  ","4MSK(11)  ","4MNS(12)  ","5MS(12)   ", 
     2 "4MSL(12)  ","4M2S(12)  ","TK(1)     ","RP(1)     ", 
     3 "KP(1)     ","THETA(1)  ","KJ(2)     ","OQ(2)     ", 
     4 "MO(3)     ","SK(4)     ","2KO(1)    ","2OK(1)    ", 
     5 "2NK2S(2)  ","MNK2S(2)  ","2KN2S(2)  ","MNKS(4)   ", 
     6 "KN(4)     ","3NKS(6)   ","2NMKS(6)  ","2MNKS(6)  ", 
     7 "MKN(6)    ","NSK(6)    ","3MNKS(8)  ","2MNK(8)   ", 
     8 "MSNK(8)   ","2MNSK(10) ","3MNKS(12) ","2NP(3)    ", 
     9 "2KP(1)    ","2PK(1)    ","KP(2)     ","2SK(2)    ", 
     1 "2KS(2)    ","2TS(2)    ","ST(4)     ","3SK(4)    ", 
     2 "3KS(4)    ","3TS(4)    ","SO(2)     ","SO(0)     ", 
     3 ".5MF      ",".5MSF     ","ST        ","3SA       ", 
     4 "4SA       ","6SA       ","8SA       ","10SA      ", 
     5 "12SA      ","24SA      ","S(3)      ","S(5)      ", 
     6 "O(2)      ","SK(2)     ","NK(3)     ","SP(3)     ", 
     7 "K(3)      ","NO(4)     ","MO(4)     ","SO(4)     ", 
     8 "S(7)      ","S(8)      ","S(10)     "  /

! constituent subscripts in same order as spd
      DATA (MS(I),I=1,max_constants)/          
     1 2,  2,  2,  1,  4,  1,  6,  3,  4,  4,  2,  6,  2,  2,  1, 
     2 2,  1,  1,  1,  0,  0,  0,  0,  0,  1,  1,  2,  2,  1,  1, 
     3 2,  3,  2,  3,  2,  8,  4,  1,  1,  1,  1,  1,  2,  2,  2, 
     4 2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  4,  4, 
     5 4,  4,  4,  4,  4,  5,  5,  5,  5,  5,  5,  5,  5,  6,  6, 
     6 6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  7,  7,  7,  7,  8, 
     7 8,  8,  8,  8,  8,  8,  8,  8,  8,  9,  9,  9,  9, 10, 10, 
     810, 10, 10, 10, 11, 12, 12, 12, 12,  1,  1,  1,  1,  2,  2, 
     9 3,  4,  1,  1,  2,  2,  2,  4,  4,  6,  6,  6,  6,  6,  8, 
     1 8,  8, 10, 12,  3,  1,  1,  2,  2,  2,  2,  4,  4,  4,  4, 
     2 2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  3,  5,  2, 
     3 2,  3,  3,  3,  4,  4,  4,  7,  8, 10/

      end
      subroutine equarg (nsped,IYR,ICM,ICD,length,label,fff,vau)
c
c     program to calculate equilibrium arguments and node factors
c     by e.e.long      (slight revision by b.b.parker)
c       major revisions by len hickman 6/11/86 to make this a subroutine
c       of lsqha.  v is calculated for the beginning of the series.
c       u and f are adjusted to the midpoint of the series (regardless
c       of length).
*     More revisions by Geoff French 9/1/86
c
      real*8 jday,jday0,jday1,jbase_date,JULIAN,yearb,monthb,dayb,hourb
      real*8 speed,spd,fff,vau
      character*10 lname,label
      dimension label(180),vau(180),fff(180)
      dimension spd(180),vuu(180),vou(180)
      common/locat/tm,gonl
      common/costx/cxx(30),oex(5)
      common/fad/ipick
      common/vee/tml,con,u,q,ui
      common/boxa/s,xl,pm,pl,sl,ps,plm,skyn,vi,v,xi,vpp
      common/boxb/vp,p,aul,aum,cra,cqa
      common/boxs/aw,ai,ae,ae1,asp

*     ******************************************************************
*     *                                                                *
*     *   nsped = number of constituents to be calculated              *
*     *                                                                *
*     *                                                                *
*     *     ICM  = month of first data point                        *
*     *     ICD    = day  of first data point                         *
*     *     IYR   = year of first data point                         *
*     *     length  = length of series to be analyzed (in days)        *
*     *                                                                *
*     ******************************************************************
      yearb=IYR
      monthb=1.
      dayb=1.
      hourb=0.
      jbase_date=JULIAN(yearb,monthb,dayb,hourb)

      iyer =IYR
      dayb = 0.0
      daym = 183.0
      grbs = 0.0
      grms = 12.0

      if (mod(iyer,4) .eq. 0) then
        daym = 184.0
        grms = 0.0
      end if

      xyer = iyer

*     compute basic astronimical constants for the time period wanted
      call astro(xyer,dayb,daym,grbs,grms)

      tm = 0.0
      gonl = 0.0
      tml = 0.0

*     look up constituent parameters by matching the name
      do 100 j = 1,nsped
      lname = label(j)
      speed = 0.0
      call name (speed,lname,isub,inum,2)
      spd(j) = speed
      call vanduf (speed,e,f,2)
      fff(j) = f
      vou(j) = e
  100 continue

!      month = ICM
!      iday = ICD
!60      call CONCTJ(juday,month,iday,iyer)
60    yearb=IYR
      monthb=ICM
      dayb=ICD
      hourb=0.
      jday=JULIAN(yearb,monthb,dayb,hourb)
      dayj=jday-jbase_date+1
      do 200 j = 1,nsped
      vau(j) = vou(j) + spd(j)*(dayj-1)*24.0
  200 continue
      call twopi(vau,nsped)
C commentted out in order to use same node factor and equilibrium arguments in the same year regardless of
C the length of time series
C Aijun Zhang      
!      daybb = dayj
!      grbss = 0.0
!      grmss = 0.0
!      daymm = dayj + length/2

 !     call astro (xyer,daybb,daymm,grbss,grmss)

 !     do 277 iz = 1,nsped
 !     speed = spd(iz)
 !     call vanduf(speed,e,f,2)
 !     vau(iz) = e
 ! 277 fff(iz) = f

*     round node to 3 decimals, vo+u to 1 place
      do 222 mt = 1,nsped
      fff(mt) = anint(fff(mt)*1000.) * 0.001
      vau(mt) = anint(vau(mt)*10.) * 0.1
  222 continue

      return
      end
!***********************************************************************
      subroutine relate (aug,test,result)
        real (kind=4), intent(in) :: aug,test
        real (kind=4), intent(out) :: result

        if (aug >= test) then
          result = aug - test
        else
          result = aug + test
        end if

      end subroutine relate

!***********************************************************************
! convert X and Y coordinates to amplitude and direction.
      subroutine fitan (x,y,dir,amp)
      implicit none
      real (kind=4), intent(in) :: x,y
      real (kind=4), intent(out) :: amp,dir
! check for small values of x and y
      if (abs(x) < 1e-5 .and. abs(y) < 1e-5) then
        amp = 0.0
        dir = 0.0
      else
!   compute amplitude
        amp = sqrt(x*x+y*y)
!   compute direction and convert from radians to degrees
        dir = atan2(x,y) * (90.0/asin(1.0))
        if (x < 0.0) then
!     if angle is negative, make it positive by adding 360 degrees
          dir = dir + 360.0
        end if
      end if
      end subroutine fitan
************************************************************************
      subroutine twopi (aug,io)
      dimension aug(io)
      do 114 mo = 1,io
      zat = aug(mo)/360.0
      if (zat .lt. 0.0)then
         aug(mo) = ((zat - aint(zat)) + 1.00)*360.0
      else
         aug(mo) = (zat - aint(zat))*360.0
      endif
C      if(zat ) 77,88,88
C   77 aug(mo) = ((zat - aint(zat)) + 1.00)*360.0
C      go to 114
C   88 aug(mo) = (zat - aint(zat))*360.0
  114 continue
      return
      end
************************************************************************
      subroutine astro (xyer,dayb,daym,grbs,grms)
      implicit real*4(a-h,o-z)
      common/locat/tm,gonl
      common/costx/cxx(30),oex(5)
      common/boxa/s,xl,pm,pl,sl,ps,plm,skyn,vi,v,xi,vpp
      common/boxb/vp,p,aul,aum,cra,cqa
      common/vee/tml,con,u,q,ui
      common/boxs/aw,ai,ae,ae1,asp
      common/boxxs/vib,vb,xib,vpb,vppb,cxsb,cxpb,cxhb,cxp1b

      pinv = 57.29578
      nyear = ifix(xyer)
      call orbit(xcen,xsx,xpx,xhx,xp1x,xnx,oex,t,xyer,5)
      xw = 23.4522944 - .0130125*t - .00000164*t**2 + .000000503*t**3
      xi = 5.14537628
      aw = xw*0.0174533
      ai = xi*0.0174533
      ae = 0.0548997
      ae1 = 0.01675104 - 0.0000418*t - .000000126*t**2
      asp = .46022931
      do 30 noe = 1,30
   30 cxx(noe) = 0.0
      if(dayb.gt.0.0) dayb = dayb - 1.0
      if(daym.gt.0.0) daym = daym - 1.0
      doby = 0.0
      amit = 0.0
      ami = xyer - xcen
      cplex = xcen/400.0 + 0.0001
      dicf = cplex - aint(cplex)

!      if(ami.eq.0.0) go to 32
!      xcet = xcen + 1.0
!      cdif = xyer - xcet
!      doby = cdif/4.0 + 0.0001
!      amit = aint(doby)
!      if(dicf.lt.0.001) amit = amit + 1.0
!   32 farm = 0.25*ami

      if(ami.ne.0.0) then
        xcet = xcen + 1.0
        cdif = xyer - xcet
        doby = cdif/4.0 + 0.0001
        amit = aint(doby)
         if(dicf.lt.0.001) amit = amit + 1.0
      end if

      farm = 0.25*ami

      farx = farm - aint(farm)
      cxx(1) = xsx + 129.384820*ami + 13.1763968*(dayb + amit) + 0.54901
     16532*grbs
      cxx(2) = xpx + 40.6624658*ami + 0.111404016*(dayb + amit) + 0.0046
     141834*grbs
      cxx(3) = xhx - 0.238724988*ami + 0.985647329*(dayb + amit) + 0.041
     1068639*grbs
      cxx(4) = xp1x + 0.01717836*ami + 0.000047064*(dayb + amit) + 0.000
     1001961*grbs
      cxx(5) = xpx + 40.6624658*ami + 0.111404016*(daym + amit) + 0.0046
     141834*grms
      cxx(6) = xnx - 19.3281858*ami - 0.052953934*(daym + amit) - 0.0022
     106414*grms
   40 cxx(7) = xpx + 40.6624658*ami + 0.111404016*(daym + amit) + 0.0046
     141834*grbs
      cxx(8) = xnx - 19.328185764*ami - 0.0529539336*(dayb + amit) - 0.0
     106414*grbs
      call twopi(cxx, 8)
   41 do 100 ii = 1,8
  100 cxx(ii) = float(ifix(cxx(ii)*100.0 + 0.5))*0.01
      ang = cxx(8)
      call table6(vib,vb,xib,vpb,vppb,xx,xx,xx,xx,xx,ang,anb,atb)
      cxx(26) = vib
      cxx(27) = vb
      cxx(28) = xib
      cxx(29) = vpb
      cxx(30) = vppb
      cxsb = cxx(1)
      cxpb = cxx(2)
      cxhb = cxx(3)
      cxp1b= cxx(4)
      ang = cxx(6)
      call table6(vi,v,xi,vp,vpp,cig,cvx,cex,pvc,pvcp,ang,an,at)
      cxx(9 ) = vi
      cxx(10) = v
      cxx(11) = xi
      cxx(12) = vp
      cxx(13) = vpp
  230 do 333 ii = 9,13
  333 cxx(ii) = float(ifix(cxx(ii)*100.0 + 0.5))*0.01
      pgx = cxx(5) - cxx(11)
      pgx = float(ifix(pgx*100.0 + 0.5))*0.01
      call twopi(pgx, 1)
      xpg = pgx*0.0174533
      cxx(14) = pgx
      raxe = sin(2.0*xpg)
      raxn = (cos(0.5*at)**2/(6.0*sin(0.5*at)**2)) - cos(2.0*xpg)
      rxx = 0.0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      if(raxe.eq.0.0.or.raxn.eq.0.0) go to 232
!      rax = raxe/raxn
!      if(rax.gt.3450.0) go to 232
!        rxx   = atan(rax )*pinv
!      cxx(22) = rxx
!  232 cra = sqrt(1.0 - 12.0*(sin(0.5*at)**2/cos(0.5*at)**2)*cos(2.0*xpg)
!     1 + 36.0*(sin(0.5*at)**4/cos(0.5*at)**4))


      if(raxe*raxn.ne.0.0) then
        rax = raxe/raxn
         if(rax.le.3450.0) then
            rxx   = atan(rax )*pinv
            cxx(22) = rxx
         end if
      end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       cra = sqrt(1.0 -12.0*(sin(0.5*at)**2/cos(0.5*at)**2)*cos(2.0*xpg)
     1 + 36.0*(sin(0.5*at)**4/cos(0.5*at)**4))


      um2 = 2.0*(cxx(11) - cxx(10))
      cxx(21) = um2
      cxx(24) = cra
      ul2 = um2 - rxx
  404 ul2 = ul2 + 180.0
  405 cxx(15) = ul2
      zes = (5.0*cos(aw) - 1.0)*sin(xpg)
      zec = (7.0*cos(aw) + 1.0)*cos(xpg)
      call fitan(zes,zec,qxx,spxx,2)
      cxx(23) = qxx
      crav = 0.5*um2 + qxx + 090.0
      cxx(16) = crav
      cqa = sqrt(0.25 + 1.5*((cos(aw)/cos(0.5*aw)**2)*cos(2.0*xpg)) + 2.
     125*(cos(aw)**2/cos(0.5*aw)**4))
      cxx(25) = cqa
      do 444 iii = 14,23
  444 cxx(iii) = float(ifix(cxx(iii)*100.0 + 0.5 ))*0.01
      pm   = cxx(1)
      pl   = cxx(2)
      sl   = cxx(3)
      ps   = cxx(4)
      plm  = cxx(5)
      skyn = cxx(6)
      vi   = cxx(9)
      v    = cxx(10)
      xi   = cxx(11)
      vp   = cxx(12)
      vpp  = cxx(13)
      p    = cxx(14)
      aul  = cxx(15)
      aum  = cxx(16)
      cra = cxx(24)
      cqa = cxx(25)
      u = v*0.0174533
      q = p*0.0174533
      ui = vi*0.0174533
      return
      end
************************************************************************
      subroutine vanduf (speed,e,f,itype)
*     Order of constituents is same as in NAMES common
      real*8 spd
      dimension spd(180),ms(180)
      common/locat/tm,gonl
      common/fad/ipick
      common/vee/tml,con,u,q,ui
      common/boxa/s,xl,pm,pl,sl,ps,plm,skyn,vi,v,xi,vpp
      common/boxb/vp,p,aul,aum,cra,cqa
      common/boxs/aw,ai,ae,ae1,asp
      common /speeds/spd
      common /mmss/ms

      double precision speed
! function definitions for all node factor computations

      f201() = ((cos(0.5*aw)**4*cos(0.5*ai)**4)/(cos(0.5*ui)**4))
      f203() = 1.0
      f205() = 1.0/sqrt(0.8965*sin(2.0*ui)**2 + 0.6001*sin(2.0*ui)
     & *cos(u) + 0.1006)
      f206() = 1.0/sqrt(19.0444*sin(ui)**4 + 2.7702*sin(ui)**2
     & *cos(2.0*u) + 0.0981)
      f207() = ((cos(0.5*aw)**4*cos(0.5*ai)**4)/(cos(0.5*ui)**4))
     & *(1.0/cra)
      f214() = (sin(2.0*aw)*(1.0 - 1.5*sin(ai)**2))/sin(2.0*ui)
      f215() = ((sin(aw)*cos(0.5*aw)**2*cos(0.5*ai)**4)/(sin(ui)
     & *cos(0.5*ui)**2))*(1.0/cqa)
      f216() = (sin(aw)*sin(.5*aw)**2*cos(.5*ai)**4)/(sin(ui)
     & *sin(.5*ui)**2)
      f218() = (sin(aw)*cos(.5*aw)**2*cos(.5*ai)**4)/
     & (sin(ui)*cos(.5*ui)**2)
      f221() = ((cos(0.5*aw)**4*cos(0.5*ai)**4)/(cos(0.5*ui)**4))**2
      f222() = ((cos(0.5*aw)**4*cos(0.5*ai)**4)/(cos(0.5*ui)**4))**3
      f223() = ((cos(0.5*aw)**4*cos(0.5*ai)**4)/(cos(0.5*ui)**4))**4
      f226() = (cos(0.5*aw )**6*cos(0.5*ai)**6)/cos(0.5*ui)**6
      f228() = ((cos(0.5*aw)**4*cos(0.5*ai)**4)/(cos(0.5*ui)**4))**1
     & *(1./sqrt(0.8965*sin(2.0*ui)**2 + 0.6001*sin(2.0*ui)*cos(u)
     & + 0.1006))
      f229() = ((cos(0.5*aw)**4*cos(0.5*ai)**4)/(cos(0.5*ui)**4))**2*
     & (1./sqrt(0.8965*sin(2.0*ui)**2 + 0.6001*sin(2.0*ui)*cos(u)
     & + 0.1006))
      f233() = (sin(aw)**2*cos(0.5*ai)**4)/sin(ui)**2
      f235() = ((2./3.- sin(aw)**2)*(1.- 1.5*sin(ai)**2))/(2./3.- 
     & sin(ui)**2)
      f238() = ((sin(aw)*cos(0.5*aw)**2*cos(0.5*ai)**4)/(sin(ui)
     & *cos(0.5*ui)**2))**2
      f251() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**2*
     & (1./sqrt(19.0444*sin(ui)**4 + 2.7702*sin(ui)**2*cos(2.*u) +
     & .0981)**2)
      f252a() = sqrt(1.0 - 12.0*(sin(.5*ui)**2/cos(.5*ui)**2)*
     & cos(2.0*q) + 36.0*(sin(.5*ui)**4/cos(.5*ui)**4))
      f252() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**2*
     & (1./sqrt(19.0444*sin(ui)**4 + 2.7702*sin(ui)**2*cos(2.*u) + 
     & .0981))*(1./f252a())
      f253() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**1*
     & (1./sqrt(19.0444*sin(ui)**4 + 2.7702*sin(ui)**2*cos(2.*u) +
     & .0981)**2)
      f254() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**2*
     & (1./sqrt(19.0444*sin(ui)**4 + 2.7702*sin(ui)**2*cos(2.*u) +
     & .0981))
      f258() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**1*
     & ((sin(aw)*cos(.5*aw)**2*cos(.5*ai)**4)/(sin(ui)*cos(.5*ui)**2))
      f259() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**1*
     & (1./sqrt(19.0444*sin(ui)**4 + 2.7702*sin(ui)**2*cos(2.*u) +
     & .0981))
      f261() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**3*
     & (1./sqrt(1.0 - 12.0*(sin(.5*ui)**2/cos(.5*ui)**2)*cos(2.0*q)
     & + 36.0*(sin(.5*ui)**4/cos(.5*ui)**4)))
      f263() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**2*
     & (1./sqrt(1.0 - 12.0*(sin(.5*ui)**2/cos(.5*ui)**2)*cos(2.0*q)
     & + 36.0*(sin(.5*ui)**4/cos(.5*ui)**4)))
      f266() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**2 *
     &((sin(aw)*cos(.5*aw)**2*cos(.5*ai)**4)/(sin(ui)*cos(.5*ui)**2))
      f270() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**1*(1./
     & sqrt(19.0444*sin(ui)**4 + 2.7702*sin(ui)**2*cos(2.*u) + .0981))
     & *(1./sqrt(.8965*sin(2.*ui)**2 + .6001*sin(2.*ui)*cos(u) +
     & 0.1006))
      f278() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**4*(1./
     & sqrt(1.0 - 12.0*(sin(.5*ui)**2/cos(.5*ui)**2)*cos(2.0*q) + 
     &      36.0*(sin(.5*ui)**4/cos(.5*ui)**4)))
      f286() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**3*
     & ((sin(aw)*cos(.5*aw)**2*cos(.5*ai)**4)/(sin(ui)*cos(.5*ui)**2))
      f287() = ((cos(0.5*aw)**4*cos(0.5*ai)**4)/(cos(0.5*ui)**4))**3*
     & (1./sqrt(0.8965*sin(2.0*ui)**2 + 0.6001*sin(2.0*ui)*cos(u) +
     & 0.1006))
      f289() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**1*(1./ 
     &sqrt(19.0444*sin(ui)**4 + 2.7702*sin(ui)**2*cos(2.*u) + .0981))*
     &((sin(aw)*cos(.5*aw)**2*cos(.5*ai)**4)/(sin(ui)*cos(.5*ui)**2))
      f296() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**5*(1.
     & /sqrt(1.0 - 12.0*(sin(.5*ui)**2/cos(.5*ui)**2)*cos(2.0*q) + 
     & 36.0*(sin(.5*ui)**4/cos(.5*ui)**4)))
      f298() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**3*
     & (1./sqrt(19.0444*sin(ui)**4 + 2.7702*sin(ui)**2*cos(2.*u) +
     & .0981))
      f300() = ((cos(0.5*aw)**4*cos(0.5*ai)**4)/(cos(0.5*ui)**4))**4*
     & (1./sqrt(0.8965*sin(2.0*ui)**2 + 0.6001*sin(2.0*ui)*cos(u)
     & + 0.1006))
      f304() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**5
      f319() =  ((sin(2.*aw)*(1.0 - 1.5*sin(ai)**2))/sin(2.0*ui))*
     & (1./sqrt(.8965*sin(2.*ui)**2 + .6011*sin(2.*ui)*cos(u) +
     & 0.1006))
      f323() = ((sin(aw)*cos(.5*aw)**2*cos(.5*ai)**4)/(sin(ui)*
     & cos(.5*ui)**2))**1 *(1.0/sqrt(0.8965*sin(2.0*ui)**2 + 0.6001*
     & sin(2.0*ui)*cos(u)+0.1006))**2
      f324() = ((sin(aw)*cos(.5*aw)**2*cos(.5*ai)**4)/(sin(ui)*
     & cos(.5*ui)**2))**2*(1.0/sqrt(0.8965*sin(2.0*ui)**2 + 0.6001*
     & sin(2.0*ui)*cos(u) + 0.1006))
      f335() = ((cos(.5*aw)**4*cos(.5*ai)**4)/(cos(.5*ui)**4))**4*
     & (1./sqrt(19.0444*sin(ui)**4 + 2.7702*sin(ui)**2*cos(2.*u) 
     & + .0981))
      f341() = 1.0/(.8965*sin(2.0*ui)**2 + .6001*sin(2.0*ui)*cos(u)
     &  + .1006)
      f345() = (1.0/sqrt(19.0444*sin(ui)**4+2.7702*sin(ui)**2*
     & cos(2.0*u)+0.0981))**2
      f349() = (1.0/sqrt(19.0444*sin(ui)**4+2.7702*sin(ui)**2*
     & cos(2.0*u)+0.0981))**3
      f353() = 0.31920/(sin(ui) - sin(ui)**3)
      f365() =((sin(aw)*cos(.5*aw)**2*cos(.5*ai)**4)/(sin(ui)*
     & cos(.5*ui)**2))**2
      f367() = ((cos(0.5*aw)**4*cos(0.5*ai)**4)/cos(0.5*ui)**4 )*
     & (1.0/sqrt(0.8965*sin(2.0*ui)**2 + 0.6001*sin(2.0*ui)*
     & cos(u) + 0.1006))
      f369() = 1.0/(sqrt(19.0444*sin(ui)**4 + 2.7702*sin(ui)**2*
     & cos(2.0*u) +0.0981)*sqrt(0.8965*sin(2.0*ui)**2 + 0.6001*
     & sin(2.0*ui)*cos(u) + 0.1006))
      f370() = ((cos(0.5*aw)**4*cos(0.5*ai)**4)/(cos(0.5*ui)**4))*
     &((sin(aw)*cos(0.5*aw)**2*cos(.5*ai)**4)/
     & (sin(ui)*cos(.5*ui)**2))**2

      con = sl + tml

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      do 600 j = 1,175
!      ipick = j
!      if(speed.eq.spd(j)) go to 611
!  600 continue
!      ipick = 176
!611   continue

      ipick = 176
       do 600 j = 1,175
          ipick = j
            if(speed.eq.spd(j)) then
              exit
            end if
600   continue

!!!!!!!!!!!!!!!!!!!!!!!!!!!

      select case (ipick)

! constituent M(2)      2T - 2s + 2h + 2xi - 2v
      case (1)
        e = 2.0*(con - pm + xi - v)
        f = f201()
 
! constituent S(2)      2T
      case (2)
        e = 2.0*tml
        f = f203()
 
! constituent N(2)      2T - 3s + 2h + p + 2xi - 2v
      case (3)
        e = 2.0*(con + xi - v) - 3.0*pm + pl
        f = f201()
 
! constituent K(1)      T + h - 90 - v'
      case (4)
        e = con - vp + 90.0
        f = f205()

! constituent M(4)      4T - 4s + 4h + 4xi - 4v
      case (5)
        e = 4.0*(con - pm + xi - v)
        f = f221()
 
! constituent O(1)      T - 2s + h + 90 + 2xi - v
      case (6)
        e = con - v - 2.0*(pm - xi) - 90.0
        f = f218()
 
! constituent M(6)      6T - 6s + 6h + 6xi - 6v
      case (7)
        e = 6.0*(con - pm + xi - v)
        f = f222()
 
! constituent MK(3)     3T - 2s + 3h - 90 + 2xi - 2v - v'
      case (8)
       e = 2.0*(con - pm + xi - v) + ( con - vp + 90.0)
       f = f228()
 
! constituent S(4)      4T
      case (9)
        e = 4.0*tml
        f = f203()
 
! constituent MN(4)     4T - 5s + 4h + p + 4xi - 4v
      case (10)
        e = 4.0*(con + xi - v) + pl - 5.0*pm
        f = f221()
 
! constituent NU(2)     2T - 3s + 4h - p + 2xi - 2v
      case (11)
        e = 2.0*(con + xi - v + sl) - 3.0*pm - pl
        f = f201()
 
! constituent S(6)      6T
      case (12)
        e = 6.0*tml
        f = f203()
 
! constituent MU(2)     2T - 4s + 4h + 2xi - 2v
      case (13)
       e = 2.0*(con + xi - v + sl) - 4.0*pm
       f = f201()
 
! constituent 2N(2)     2T - 4s + 2h + 2p + 2xi - 2v
      case (14)
       e = 2.0*(con + xi - v + pl) - 4.0*pm
       f = f201()
 
! constituent OO(1)     T + 2s + h - 90 - 2xi - v
      case (15)
       e = con - v + 2.0*(pm - xi)+ 90.0
       f = f216()
 
! constituent LAMBDA(2) 2T - s + p + 180 + 2xi - 2v
      case (16)
        e = 2.0*(con + xi - v - sl) - pm + pl + 180.0
        f = f201()
 
! constituent S(1)      T
      case (17)
        e = tml + 180.0
        f = f203()
 
! constituent M(1)      T - s + h + p - 90 + xi - v + Q
      case (18)
        e = con - pm + aum
        f = f215()
 
! constituent J(1)      T + s + h - p - 90 - v
      case (19)
        e = con + pm - pl - v + 90.0
        f = f214()
 
! constituent MM        s - p
      case (20)
        e = pm - pl
        f = f235()
 
! constituent SSA       2h
      case (21)
        e = 2.0*sl
        f = f203()
 
! constituent SA        h
      case (22)
       e = sl
       f = f203()
 
! constituent MSF       2s - 2h
      case (23)
        e = 2.0*tml - 2.0*(con - pm + xi - v)
        f = f201()
 
! constituent MF        2s - 2xi
      case (24)
        e = 2.0*(pm - xi)
        f = f233()
 
! constituent RHO(1)    T - 3s + 3h - p + 90 + 2xi - v
      case (25)
        e = con - v - 3.0*pm + 2.0*xi - pl + 2.0*sl - 90.0
        f = f218()
 
! constituent Q(1)      T - 3s + h + p + 90.0 + 2xi - v
      case (26)
        e = con - v - 3.0*pm + 2.0*xi + pl - 90.0
        f = f218()
 
! constituent T(2)      2T - h + p1
      case (27)
         e = 2.0*tml - (sl - ps)
        f = f203()
 
! constituent R(2)      2T + h - p1 + 180.0
      case (28)
        e = sl - ps + 180.0 + 2.0*tml
        f = f203()
 
! constituent 2Q(1)     T - 4s + h + 2p + 90.0 + 2xi - v
      case (29)
        e = con - v - 4.0*pm + 2.0*xi + 2.0*pl - 90.0
        f = f218()
 
! constituent P(1)      T - h + 90.0
      case (30)
       e = tml + 270.0 - sl
       f = f203()
 
! constituent 2SM(2)    2T + 2s - 2h - 2xi + 2v
      case (31)
        e = 4.0*tml - 2.0*(con - pm + xi - v)
        f = f201()
 
! constituent M(3)      3T - 3s + 3h + 3xi - 3v
      case (32)
        e = 3.0*(con - pm + xi - v) + 180.0
        f = f226()
 
! constituent L(2)      2T - s + 2h - p + 180.0 + 2xi - 2v - R
      case (33)
        e = 2.0*con - pm - pl + aul
        f = ((cos(0.5*aw)**4*cos(0.5*ai)**4)/(cos(0.5*ui)**4))*(1.0/cra)
 
! constituent 2MK(3)    3T - 4s + 3h + 90.0 + 4xi - 4v - v'
      case (34)
        e = 4.0*(con - pm + xi - v) - (con - vp + 90.0)
        f = f229()
 
! constituent K(2)      2T + 2h - 2v"
      case (35)
        e = 2.0*con - vpp
        f = f206()
 
! constituent M(8)      8T - 8s + 8h + 8xi - 8v
      case (36)
       e = 8.0*(con - pm + xi - v)
       f = f223()
 
! constituent MS(4)     4T - 2s + 2h + 2xi - 2v
      case (37)
       e = 2.0*(con - pm + xi - v) + 2.0*tml
       f = f201()
 
! constituent SIGMA(1)
      case (38)
        e = 2.0*(con - v - 2.0*(pm - xi) - 90.0) - (tml+270.0 - sl)
        f = f238()
 
! constituent MP(1)
      case (39)
       e = 2.0*(con - pm + xi - v) - (tml + 270.0 - sl)
       f = f201()
 
! constituent CHI(1)
      case (40)
        e = con + 2.0*sl - v - pm - pl + 90.0
        f = f258()
 
! constituent 2PO(1)
      case (41)
        e = 2.0*(tml + 270.0 - sl) - (con - v - 2.0*(pm - xi) - 90.0)
        f = f218()
 
! constituent SO(1)
      case (42)
        e = 2.0*tml - (con - v - 2.0*(pm - xi) - 90.0)
        f = f218()
 
! constituent MSN(2)
      case (43)
         e = 2.0*tml + pm - pl
         f = f221()
 
! constituent MNS(2)
      case (44)
        e = 4.0*(con + xi - v) - 5.0*pm - 2.0*tml  + pl
        f = f221()
 
! constituent OP(2)
      case (45)
        e = con - v - 2.0*(pm - xi) + tml - sl + 180.0
        f = f218()
 
! constituent MKS(2)
      case (46)
        e = 4.0*con - 2.0*(pm - xi + v + tml) - vpp
        f = f259()
 
! constituent 2NS(2)
      case (47)
        e = 4.0*(con + xi - v) - 6.0*pm + 2.0*(pl - tml)
        f = f221()
 
! constituent MLN2S(2)
      case (48)
        e = 6.0*(con - pm) + 4.0*(xi - v - tml) + aul
        f = f261()
 
! constituent 2ML2S(2)
      case (49)
        e = 6.0*con - 5.0*pm + 4.0*(xi - v - tml) - pl + aul
        f = f261()
 
! constituent SKM(2)
      case (50)
        e = 2.0*(tml + pm - xi + v) - vpp
        f = f259()
 
! constituent 2MS2K(2)
      case (51)
        e = 4.0*(xi - pm - v) + 2.0*(tml + vpp)
        f = f251()
 
! constituent MKL2S(2)
      case (52)
        e = 6.0*con - 4.0*tml - 3.0*pm + 2.0*(xi - v) - pl - vpp + aul
        f = f252()
 
! constituent M2(KS)(2)
      case (53)
        e = 6.0*con - 4.0*tml + 2.0*(xi - v - pm - vpp)
        f = f253()
 
! constituent 2SN(MK)(2)
      case (54)
       e = 4.0*tml - 2.0*con + pl - pm + vpp
       f = f254()
 
! constituent 2KM(SN)(2)
      case (55)
        e = 4.0*con - 2.0*(tml + vpp) + pm - pl
        f = f251()
 
! constituent SO(3)
      case (56)
        e = 2.0*tml + con - v - 2.0*(pm - xi) - 90.0
        f = f218()
 
! constituent SK(3)
      case (57)
       e = 2.0*tml + con - vp + 90.0
       f = f205()
 
! constituent NO(3)
      case (58)
        e = 3.0*(con - v) + 4.0*xi - 5.0*pm + pl - 90.0
        f = f258()
 
! constituent MK(4)
      case (59)
        e = 4.0*con - 2.0*(pm - xi + v) - vpp
        f = f259()
 
! constituent SN(4)
       case (60)
        e = 2.0*(tml + con + xi - v) - 3.0*pm + pl
        f = f201()
 
! constituent 2MLS(4)
      case (61)
       e = 6.0*con - 5.0*pm + 4.0*(xi - v) - 2.0*tml - pl + aul
       f = f261()
 
! constituent 3MS(4)
      case (62)
       e = 6.0*(con - pm + xi - v) - 2.0*tml
       f = f222()
 
! constituent ML(4)
      case (63)
        e = 4.0*con - 3.0*pm + 2.0*(xi - v) - pl + aul
        f = f263()
 
! constituent N(4)
      case (64)
        e = 4.0*(con + xi - v) - 6.0*pm + 2.0*pl
        f = f221()
 
! constituent SL(4)
      case (65)
        e = 2.0*(con + tml) - pm - pl + aul
        f = f207()
 
! constituent MNO(5)
       case (66)
        e = 5.0*(con - v) - 7.0*pm + 6.0*xi + pl - 90.0
        f = f266()
 
! constituent 2MO(5)
      case (67)
       e = 5.0*(con - v) + 6.0*(xi - pm) - 90.0
       f = f266()
 
! constituent 2MK(5)
      case (68)
       e = 5.0*con + 4.0*(xi - pm - v) - vp + 90.0
       f = f229()
 
! constituent MSK(5)
      case (69)
        e = 3.0*con + 2.0*(xi - pm - v + tml) - vp + 90.0
        f = f228()
 
! constituent 3KM(5)
      case (70)
        e = 5.0*con + 2.0*(xi - pm - v) - (vp + vpp) + 90.0
        f = f270()
 
! constituent 2MP(5)
      case (71)
       e = 4.0*(con - pm + xi - v) + tml - sl + 270.0
       f = f221()
 
! constituent 3MP(5)
      case (72)
        e = 6.0*(con - pm + xi - v) - tml + sl - 270.0
        f = f222()
 
! constituent MNK(5)
      case (73)
        e = 5.0*(con - pm) + 4.0*(xi - v) + pl - vp + 90.0
        f = f229()
 
! constituent 2SM(6)
      case (74)
        e = 2.0*(con - pm + xi - v) + 4.0*tml
         f = f201()
 
! constituent 2MN(6)
      case (75)
        e = 6.0*(con + xi - v) - 7.0*pm + pl
        f = f222()
 
! constituent MSN(6)
      case (76)
        e = 4.0*(con + xi - v) - 5.0*pm + 2.0*tml + pl
        f = f221()
 
! constituent 2MS(6)
      case (77)
        e = 4.0*(con - pm + xi - v) + 2.0*tml
        f = f221()
 
! constituent 2NMLS(6)
      case (78)
        e = 8.0*con - 9.0*pm + 6.0*(xi - v) - 2.0*tml + pl + aul
        f = f278()
 
! constituent 2NM(6)
      case (79)
        e = 6.0*(con + xi - v) - 8.0*pm + 2.0*pl
        f = f222()
 
! constituent MSL(6)
      case (80)
       e = 4.0*con - 3.0*pm + 2.0*(xi - v + tml) - pl + aul
       f = f263()
 
! constituent 2ML(6)
      case (81)
        e = 6.0*con - 5.0*pm + 4.0*(xi - v) - pl + aul
        f = f261()
 
! constituent MSK(6)
      case (82)
       e = 4.0*con + 2.0*(xi - pm - v + tml) - vpp
       f = f259()
 
! constituent 2MLNS(6)
      case (83)
        e = 8.0*(con - pm) + 6.0*(xi - v) - 2.0*tml + aul
        f = f278()
 
! constituent 3MLS(6)
      case (84)
       e = 8.0*con - 7.0*pm + 6.0*(xi - v) - 2.0*tml - pl + aul
       f = f278()
 
! constituent 2MK(6)
      case (85)
        e = 6.0*con + 4.0*(xi - pm - v) - vpp
        f = f254()
 
! constituent 2MNO(7)
      case (86)
       e = 7.0*(con - v) - 9.0*pm + 8.0*xi + pl - 90.0
       f = f286()
 
! constituent 2NMK(7)
      case (87)
       e = 7.0*con + 6.0*(xi - v) - 8.0*pm + 2.0*pl - vp + 90.0
       f = f287()
 
! constituent 2MSO(7)
      case (88)
        e = 5.0*(con - v) + 6.0*(xi - pm) + 2.0*tml - 90.0
        f = f266()
 
! constituent MSKO(7)
      case (89)
       e = 5.0*con + 4.0*(xi - pm) - 3.0*v + 2.0*tml - vpp - 90.0
       f = f289()
 
! constituent 2MSN(8)
      case (90)
        e = 6.0*con - 7.0*pm + 6.0*(xi - v) + 2.0*tml + pl
        f = f222()
 
! constituent 3MS(8)
      case (91)
        e = 6.0*(con - pm + xi - v) + 2.0*tml
        f = f222()
 
! constituent 2(MS)(8)
      case (92)
        e = 4.0*(con - pm + xi - v) + 4.0*tml
        f = f221()
 
! constituent 2(MN)(8)
      case (93)
        e = 8.0*(con + xi - v) - 10.0*pm + 2.0*pl
        f = f223()
 
! constituent 3MN(8)
      case (94)
        e = 8.0*(con + xi - v) - 9.0*pm + pl
        f = f223()
 
! constituent 2MSL(8)
      case (95)
       e = 6.0*con - 5.0*pm + 4.0*(xi - v) + 2.0*tml - pl + aul
       f = f261()
 
! constituent 4MLS(8)
      case (96)
       e = 10.0*con - 9.0*pm + 8.0*(xi - v) - 2.0*tml - pl + aul
       f = f296()
 
! constituent 3ML(8)
      case (97)
        e = 8.0*con - 7.0*pm + 6.0*(xi - v) - pl + aul
        f = f278()
 
! constituent 3MK(8)
      case (98)
        e = 8.0*con + 6.0*(xi - pm - v) - vpp
        f = f298()
 
! constituent 2MSK(8)
      case (99)
        e = 6.0*con + 4.0*(xi - pm - v) + 2.0*tml - vpp
        f = f254()
 
! constituent 2(MN)K(9)
      case (100)
       e = 9.0*con - 10.0*pm + 8.0*(xi - v) + 2.0*pl - vp + 90.0
       f = f300()
 
! constituent 3MNK(9)
      case (101)
        e = 9.0*(con - pm) + 8.0*(xi - v) + pl - vp + 90.0
        f = f300()
 
! constituent 4MK(9)
      case (102)
        e = 9.0*con + 8.0*(xi - pm - v) - vp + 90.0
        f = f300()
 
! constituent 3MSK(9)
      case (103)
       e = 7.0*con + 6.0*(xi - pm - v) + 2.0*tml - vp + 90.0
       f = f287()
 
! constituent 4MN(10)
      case (104)
        e = 10.0*(con + xi - v) - 11.0*pm + pl
        f = f304()
 
! constituent M(10)
      case (105)
        e = 10.0*(con - pm + xi - v)
        f = f304()
 
! constituent 3MNS(10)
      case (106)
       e = 8.0*(con + xi - v) - 9.0*pm + pl + 2.0*tml
       f = f223()
 
! constituent 4MS(10)
      case (107)
       e = 8.0*(con - pm + xi - v) + 2.0*tml
       f = f223()
 
! constituent 3MSL(10)
      case (108)
        e = 8.0*con - 7.0*pm + 6.0*(xi - v) - pl + aul
        f = f278()
 
! constituent 3M2S(10)
      case (109)
       e = 6.0*(con - pm + xi - v) + 4.0*tml
       f = f222()
 
! constituent 4MSK(11)
      case (110)
       e = 9.0*con + 8.0*(xi - pm - v) + 2.0*tml - vp + 90.0
       f = f300()
 
! constituent 4MNS(12)
      case (111)
       e = 10.0*(con + xi - v) - 11.0*pm + pl
       f = f304()
 
! constituent 5MS(12)
      case (112)
        e = 10.0*(con - pm + xi - v) + 2.0*tml
        f = f304()
 
! constituent 4MSL(12)
      case (113)
        e = 10.0*con - 9.0*pm + 8.0*(xi - v) + 2.0*tml - pl + aul
        f = f296()
 
! constituent 4M2S(12)
      case (114)
       e = 8.0*(con - pm + xi - v) + 4.0*tml
       f = f223()
 
! constituent TK(1)
      case (115)
        e = tml - 2.0*sl + ps - 90.0
        f = f203()
 
! constituent RP(1)
      case (116)
       e = con + sl - ps + 90.0
       f = f203()
 
! constituent KP(1)
      case (117)
       e = con + 2.0*sl + 90.0
       f = f203()
 
! constituent THETA(1)
      case (118)
       e = tml + pm - sl + pl - v + 90.0
       f = f258()
 
! constituent KJ(2)
      case (119)
       e = 2.0*con + pm - v - vp - pl + 180.0
       f = f319()
 
! constituent OQ(2)
      case (120)
        e = 2.0*(con - v) - 5.0*pm + 4.0*xi + pl + 180.0
        f = f238()
 
! constituent MO(3)
       case (121)
        e = 3.0*(con - v) - 4.0*(pm - xi) - 90.0
        f = f258()
 
! constituent SK(4)
      case (122)
        e = 2.0*(con + tml) - vpp
        f = f206()
 
! constituent 2KO(1)
      case (123)
       e = con + 2.0*(pm - xi - vp) + v + 270.0
       f = f323()
 
! constituent 2OK(1)
      case (124)
        e = con - 2.0*v - 4.0*(pm - xi) + vp - 270.0
        f = f324()
 
! constituent 2NK2S(2)
      case (125)
       e = 6.0*(con - pm) + 4.0*(xi - v - tml) + 2.0*pl - vpp
       f = f254()
 
! constituent MNK2S(2)
      case (126)
       e = 6.0*con - 5.0*pm + 4.0*(xi - v - tml) + pl - vpp
       f = f254()
 
! constituent 2KN2S(2)
      case (127)
       e = 6.0*con - 4.0*tml - 3.0*pm + 2.0*(xi - v - vpp) + pl
       f = f253()
 
! constituent MNKS(4)
      case (128)
       e = 6.0*con - 5.0*pm + 4.0*(xi - v) - 2.0*tml + pl - vpp
       f = f254()
 
! constituent KN(4)
      case (129)
       e = 4.0*con - 3.0*pm + 2.0*(xi - v) + pl - vpp
       f = f259()
 
! constituent 3NKS(6)
      case (130)
       e = 8.0*con - 9.0*pm + 6.0*(xi - v) + 3.0*pl - 2.0*tml - vpp
       f = f298()
 
! constituent 2NMKS(6)
      case (131)
       e = 8.0*(con - pm) + 6.0*(xi - v) + 2.0*(pl - tml) - vpp
       f = f298()
 
! constituent 2MNKS(6)
      case (132)
       e = 8.0*con - 7.0*pm + 6.0*(xi - v) - 2.0*tml + pl - vpp
       f = f298()
 
! constituent MKN(6)
      case (133)
       e = 6.0*con - 5.0*pm + 4.0*(xi - v) + pl - vpp
       f = f254()
 
! constituent NSK(6)
      case (134)
       e = 4.0*con - 3.0*pm + 2.0*(xi - v + tml) + pl - vpp
       f = f259()
 
! constituent 3MNKS(8)
      case (135)
       e = 10.0*con - 9.0*pm + 8.0*(xi - v) - 2.0*tml + pl - vpp
       f = f335()
 
! constituent 2MNK(8)
      case (136)
       e = 8.0*con - 7.0*pm + 6.0*(xi - v) + pl - vpp
       f = f298()
 
! constituent MSNK(8)
      case (137)
       e = 6.0*con - 5.0*pm + 4.0*(xi - v) - 2.0*tml + pl - vpp
       f = f254()
 
! constituent 2MNSK(10)
      case (138)
       e = 8.0*con - 7.0*pm + 6.0*(xi - v) + 2.0*tml + pl - vpp
       f = f298()
 
! constituent 3MNKS(12)
      case (139)
       e = 10.0*con - 9.0*pm + 8.0*(xi - v) + 2.0*tml + pl - vpp
       f = f335()
 
! constituent 2NP(3)
      case (140)
       e = 4.0*(con + xi - v) - 6.0*pm + 2.0*pl + sl - tml - 270.0
       f = f221()
 
! constituent 2KP(1)
      case (141)
       e = 3.0*sl - 2.0*vp - tml - 90.0
       f = f341()
 
! constituent 2PK(1)
      case (142)
       e = tml + vp - 3.0*sl + 90.0
       f = f205()
 
! constituent KP(2)
      case (143)
       e = 2.0*tml - vp
       f = f205()
 
! constituent 2SK(2)
      case (144)
         e = 2.0*(tml - sl) + vpp
        f = f206()
 
! constituent 2KS(2)
      case (145)
        e = 2.0*(tml - vpp) + 4.0*sl
        f = f345()
 
! constituent 2TS(2)
      case (146)
       e = 2.0*tml - 2.0*(sl - ps)
       f = f203()
 
! constituent ST(4)
      case (147)
       e = 4.0*tml + ps - sl
       f = f203()
 
! constituent 3SK(4)
      case (148)
       e = 4.0*tml - 2.0*sl + vpp
       f = f206()
 
! constituent 3KS(4)
      case (149)
        e = 4.0*tml + 6.0*sl - 3.0*vpp
        f = f349()
 
! constituent 3TS(4)
      case (150)
        e = 4.0*tml - 3.0*(sl - ps)
        f = f203()
 
! constituent SO(2)
      case (151)
        e = con - v - 2.0*(pm - xi) + tml + 90.0
        f = f218()
 
! constituent SO(0)
      case (152)
       e = 2.0*(pm - xi) + v + tml - con - 90.0
       f = f218()
 
! constituent .5MF
      case (153)
       e = pm - xi - 90.0
       f = f353()
 
! constituent .5MSF
      case (154)
       e = pm - sl
       f = f235()
 
! constituent ST
      case (155)
       e = sl - ps
       f = f203()
 
! constituent 3SA
      case (156)
       e = 3.0*sl
        f = f203()
 
! constituent 4SA
      case (157)
       e = 4.0*sl
       f = f203()
 
! constituent 6SA
      case (158)
        e = 6.0*sl
        f = f203()
 
! constituent 8SA
      case (159)
        e = 8.0*sl
        f = f203()
 
! constituent 10SA
      case (160)
       e = 10.0*sl
       f = f203()
 
! constituent 12SA
      case (161)
        e = 12.0*sl
        f = f203()
 
! constituent 24SA
      case (162)
        e = 24.0*sl
        f = f203()
 
! constituent S(3)
       case (163)
        e = 3.0*tml + 180.0
        f = f203()
 
! constituent S(5)
      case (164)
       e = 5.0*tml + 180.0
       f = f203()
 
! constituent O(2)
      case (165)
       e = 2.0*(con - v) - 4.0*(pm - xi) - 180.0
       f = f365()
 
! constituent SK(2)
      case (166)
       e = tml + con - vp + 270.0
       f = f205()
 
! constituent NK(3)
      case (167)
        e = 3.0*(con - pm) + 2.0*(xi - v) + pl - vp + 90.0
        f = f367()
 
! constituent SP(3)
      case (168)
       e = 3.0*tml + 270.0 - sl
       f = f203()
 
! constituent K(3)
      case (169)
       e = 3.0*con - vpp - vp + 90.0
       f = f369()
 
! constituent NO(4)
      case (170)
        e = 4.0*(con - v) + 6.0*xi - 7.0*pm + pl - 180.0
        f = f370()
 
! constituent MO(4)
      case (171)
        e = 4.0*(con - v) - 6.0*(pm - xi) - 180.0
        f = f370()
 
! constituent SO(4)
      case (172)
        e = 2.0*(tml + con - v) - 4.0*(pm - xi) - 180.0
        f = f365()
 
! constituent S(7)
      case (173)
         e = 7.0*tml + 180.0
         f = f203()
 
! constituent S(8)
      case (174)
         e = 8.0*tml
         f = f203()
 
! constituent S(10)
      case (175)
        e = 10.0*tml
        f = f203()
 
      case default
        print 500, speed
500     format (/' *** (v + u) not computed for constituent of speed',
     &  f12.7,'  ****',' Execution terminated')
        stop
      end select

      if (itype == 2) then
        e = e + float(ms(ipick))*gonl - spd(ipick)*(tm/15.0)
      end if

! reduce v+u to range 0 <= e < 360 degrees
!      e = twopi(e,1)
      call twopi(e,1)
      f = 1.0/f

      end subroutine vanduf
************************************************************************
      subroutine name (spdd,itag,isub,inum,icode)
c     this subroutine identifies the constituent by its speed,
*     name label or constituent number,
c     and makes it availabel for labeling.
*     ICODE = 1, by speed
*     ICODE = 2, by label
*     ICODE = 3, by number
c     it also determine the subscript of the constituent
c        order of constituent speeds***  m(2),n(2),s(2),o(1),k(1),k(2)
c      l(2),2n(2)r(2),t(2),lambda(2),mu(2),nu(2),j(1),m(1),oo(1),p(1)
c      q(1),2q(1),rho(1),m(4),m(6),m(8),s(4),s(6),m(3),s(1),mk(3),2mk(3)
c      mn(4),ms(4),2sm(2),mf,msf,mm,sa,ssa
      real*8 spd,spdd
      common /mmss/ip
      common /speeds/spd
      common /names/label
      character*10 label(180)*10,itag
      dimension spd(180),ip(180)

    1 format(10x,'Constituent of speed ',f12.7,' not in list.')
    2 format(10x,'Constituent ', a10,' not in the list.')
    3 format(10x,'Constituent no.',i4,' not in list.')

      if (icode.eq.1) then
*       search by speed
        do j = 1,175
          if(spdd.ne.spd(j)) cycle
          itag = label(j)
          isub = ip(j)
          inum = j
          return
        enddo
        print 1,spdd
      else if (icode.eq.2) then
*       search by name
        do i = 1,175
          if(itag.ne.label(i)) cycle
          spdd = spd(i)
          isub = ip(i)
          inum = i
          return
        enddo
        print 2, itag
      else if (icode.eq.3) then
*       search by number
        if (i.gt.0.and.i.le.175) then
          itag = label(inum)
          spdd = spd(inum)
          isub = ip(inum)
          return
        end if
        write(*,*)'get wrong icode, icode must be 1 or 2'
        stop
        print 3, inum
      end if

      stop '**** Execution terminated in NAME (illegal icode) ****'

      end
************************************************************************
      subroutine orbit (xcen,xsx,xpx,xhx,xp1x,xnx,oex,t,xyer,nnn)
      implicit real*4(a-h,o-z)
      dimension oex(nnn)
      s = 13.1763968
      p = 0.1114040
      xh = 0.9856473
      p1 = 0.0000471
      xn = -.0529539
      xcan = xyer*0.01 + 0.001
      xcen = aint(xcan)*100.0
      t = -3.0
      yr = 2.5
      gat = 1600.0



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      do 10 jk = 1,30
!      gp = gat/400.0 + 0.00001
!      col = gp - aint(gp)

!      if(col.lt.0.010) go to 11
!      if(gat.eq.xcen) go to 12
!      yr = yr - 1.0
!      go to 9
!   11 if( gat.eq.xcen) go to 12
!    9 gat = gat + 100.0
!   10 continue
!   12 t = (gat - 1900.0)*0.01

!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      do 10 jk = 1,30
      gp = gat/400.0 + 0.00001
      col = gp - aint(gp)
         if(col.lt.0.010) then
                if( gat.eq.xcen) then
                   exit
                end if
            gat = gat + 100.0
         else
                if( gat.eq.xcen) then
                    exit
                end if
           yr = yr - 1.0
           gat = gat + 100.0
 
         end if
10      continue


         t = (gat - 1900.0)*0.01

!!!!!!!!!!!!!!!!




      oex(1) = 270.437422 + 307.892*t + 0.002525*t**2 + .00000189*t**3 +
     1 yr*s
      oex(2) = 334.328019 + 109.032206*t - 0.01034444*t**2 - .0000125*t*
     1*3 + yr*p
      oex(3) = 279.696678 + 0.768925*t + .0003205*t**2 + yr*xh
      oex(4) = 281.220833 + 1.719175*t + 0.0004528*t**2 + .00000333*t**3
     1 + yr*p1
      oex(5) = 259.182533 - 134.142397*t + .00210556*t**2 + .00000222*t*
     1*3 + yr*xn
      call twopi(oex,5)
      do 100 i = 1,5
  100 oex(i) = float(ifix(oex(i)*100.0 + 0.5))*0.01
      xsx = oex(1)
      xpx = oex(2)
      xhx = oex(3)
      xp1x = oex(4)
      xnx = oex(5)
      return
      end
************************************************************************
      subroutine table6(vi,v,xi,vp,vpp,cig,cvx,cex,pvc,pvcp,ang,an,at)
      common/boxs/aw,ai,ae,ae1,asp
      v = 0.0
      xi = 0.0
      vp = 0.0
      vpp = 0.0
      an = ang*0.0174533
      ax = ang
      eye = cos(ai)*cos(aw) - sin(ai)*sin(aw)*cos(an)
      c9 = acos(eye)*57.2957795
      vi = float(ifix(c9*100.0 + 0.5))*0.01
      cig = vi*0.0174533
      at = cig

!!!!!!!!!!!!!!!!!!!!!!!!!
!      if(cig.eq.0.0) go to 230
!      if(ax.eq.0.0.or.ax.eq.180.0) go to 230
!      vxxe = sin(ai)*sin(an)
!      vxxn = cos(ai)*sin(aw) + sin(ai)*cos(aw)*cos(an)
!      if(vxxe.eq.0.0.or.vxxn.eq.0.0) go to 201
!      vxx = vxxe/vxxn
!      c10 = atan(vxx)*57.2957795
!      v = float(ifix(c10*100.0 + 0.5))*0.01
!      if(ax.gt.180.0.and.v.gt.0.0) v = -1.0*v
!  201 cvx = v*0.0174533
!      term = sin(ai)*(cos(aw)/sin(aw))
!      exx = term*(sin(an)/cos(an)) + (cos(ai) - 1.0)*sin(an)
!      if(exx.eq.0.0) go to 202
!      ezz = term + cos(ai)*cos(an) + (sin(an)**2/cos(an))
!      if(ezz.eq.0.0) go to 202
!      exez = exx/ezz
!      if(exez.gt.3450.0) go to 202
!      c11 = atan(exez)*57.2957795
!      xi = float(ifix(c11*100.0 + 0.5))*0.01
!      if(ax.gt.180.0.and.xi.gt.0.0) xi = -1.0*xi
!  202 cex = xi*0.0174533
!      a22 = (0.5 + 0.75*ae**2)*sin(2.0*cig)
!      b22 = (0.5 + 0.75*ae1**2)*sin(2.0*aw)*asp
!      vpxe = a22*sin(cvx)
!      vpxn = a22*cos(cvx) + b22
!      if(vpxe.eq.0.0.or.vpxn.eq.0.0) go to 203
!      vpx = vpxe/vpxn
!      if(vpx.gt.3450.0) go to 203
!      vp = atan(vpx)*57.2957795
!      if(ax.gt.180.0.and.vp.gt.0.0) vp = -1.0*vp
!  203 pvc = vp*0.0174533
!      a47 = (0.5 + 0.75*ae**2)*sin(cig)**2
!      b47 = (0.5 + 0.75*ae1**2)*asp*sin(aw)**2
!      vpye = a47*sin(2.0*cvx)
!      vpyn = a47*cos(2.0*cvx) + b47
!      if(vpye.eq.0.0.or.vpyn.eq.0.0) go to 204
!      vpy = vpye/vpyn
!      if(vpy.gt.3450.0) go to 204
!      vpp = atan(vpy)*57.2957795
!      if(ax.gt.180.0.and.vpp.gt.0.0) vpp = -1.0*vpp
!  204 pvcp = vpp*0.0174533
!  230 return
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if(cig.eq.0.0) then
        return
      end if

      if(ax.eq.0.0.or.ax.eq.180.0) then
        return
      end if

      vxxe = sin(ai)*sin(an)
      vxxn = cos(ai)*sin(aw) + sin(ai)*cos(aw)*cos(an)

      if(vxxe*vxxn.ne.0.0) then
        vxx = vxxe/vxxn
        c10 = atan(vxx)*57.2957795
        v = float(ifix(c10*100.0 + 0.5))*0.01
        if(ax.gt.180.0.and.v.gt.0.0) v = -1.0*v
      end if

      cvx = v*0.0174533

      term = sin(ai)*(cos(aw)/sin(aw))
      exx = term*(sin(an)/cos(an)) + (cos(ai) - 1.0)*sin(an)

      if(exx.ne.0.0) then 
        ezz = term + cos(ai)*cos(an) + (sin(an)**2/cos(an))
         if(ezz.ne.0.0) then
            exez = exx/ezz
             if(exez.le.3450.0) then
               c11 = atan(exez)*57.2957795
               xi = float(ifix(c11*100.0 + 0.5))*0.01
                if(ax.gt.180.0.and.xi.gt.0.0) xi = -1.0*xi
             end if
        end if
      end if

      cex = xi*0.0174533

      a22 = (0.5 + 0.75*ae**2)*sin(2.0*cig)
      b22 = (0.5 + 0.75*ae1**2)*sin(2.0*aw)*asp
      vpxe = a22*sin(cvx)
      vpxn = a22*cos(cvx) + b22
      if(vpxe*vpxn.ne.0.0) then
         vpx = vpxe/vpxn
          if(vpx.le.3450.0) then
             vp = atan(vpx)*57.2957795
              if(ax.gt.180.0.and.vp.gt.0.0) vp = -1.0*vp
          end if
      end if
  
      pvc = vp*0.0174533


      a47 = (0.5 + 0.75*ae**2)*sin(cig)**2
      b47 = (0.5 + 0.75*ae1**2)*asp*sin(aw)**2
      vpye = a47*sin(2.0*cvx)
      vpyn = a47*cos(2.0*cvx) + b47

      if(vpye*vpyn.ne.0.0) then
        vpy = vpye/vpyn
           if(vpy.le.3450.0) then
              vpp = atan(vpy)*57.2957795
              if(ax.gt.180.0.and.vpp.gt.0.0) vpp = -1.0*vpp
           end if
      end if 

      pvcp = vpp*0.0174533

      return

      end
