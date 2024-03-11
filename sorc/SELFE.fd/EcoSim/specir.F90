SUBROUTINE spec_ir(Tair, Pair, Hair, cloud, Uwind, Vwind)
USE bio_param
USE eclight
USE biology
USE elfe_glbl, only : nea,pi,xlon_el,ylat_el,idry_e
IMPLICIT NONE
SAVE
real(r8), intent(in) :: cloud(nea)
real(r8), intent(in) :: Hair(nea)
real(r8), intent(in) :: Tair(nea)
real(r8), intent(in) :: Pair(nea)
real(r8), intent(in) :: Uwind(nea)
real(r8), intent(in) :: Vwind(nea)
real(r8) :: am = 1.0_r8
real(r8) :: betalam = 0.55_r8
real(r8) :: p0 = 29.92_r8
real(r8) :: rex = -1.6364_r8
real(r8) :: roair = 1200.0_r8
real(r8) :: rn = 1.341_r8
real(r8) :: vis = 15.0_r8
real(r8) :: wv = 1.5_r8
integer :: i, iband, ic, j, nc
integer :: iday
real(r8) :: Dangle, Hangle, LatRad, LonRad
real(r8) :: cff, cff1, cff2
real(r8) :: alpha, beta, gamma, theta, rtheta, rthetar
real(r8) :: atra, gtra, otra, rtra, wtra
real(r8) :: alg, arg, asymp, cosunz, Fa
real(r8) :: frh, rh, rlam, rlogc
real(r8) :: rm, rmin, rmo, rmp, rod, rof
real(r8) :: ros, rospd, rosps, rpls
real(r8) :: sumx, sumx2, sumxy, sumy
real(r8) :: taa, tas, to3, wa, wspeed, zenith
real(r8), dimension(NBands) :: Fo, Edir, Edif, Ed, qlam
real(r8), dimension(3) :: a_arr, dndr
real(r8), dimension(3) :: ro = (/ 0.03_r8, 0.24_r8, 2.00_r8 /)
real(r8), dimension(3) :: r_arr = (/ 0.10_r8, 1.00_r8, 10.0_r8 /)
Dangle=23.44_r8*COS((172.0_r8-yday)*2.0_r8*pi/365.25_r8)
Dangle=Dangle*deg2rad
Hangle=(12.0_r8-hour)*pi/12.0_r8
cff=1.0E-9_r8/(6.6256E-34_r8*2.998E8_r8*6.023E17_r8)
DO iband=1,NBands
qlam(iband)=ec_wave_ab(iband)*cff
END DO
cff=(1.0_r8+0.0167_r8*COS(2.0_r8*pi*(yday-3.0_r8)/365.0_r8))**2
DO iband=1,NBands
Fo(iband)=ec_Fobar(iband)*cff
END DO
DO i=1,nea
if(idry_e(i)==1) cycle
LatRad=ylat_el(i)*deg2rad
LonRad=xlon_el(i)*deg2rad
to3=(235.0_r8+(150.0_r8+40.0_r8* &
& SIN(0.9865_r8*(yday-30.0_r8)*deg2rad)+ &
& 20.0_r8*SIN(3.0_r8*LonRad))* &
& SIN(1.28_r8*LatRad)*SIN(1.28_r8*LatRad))* &
& 0.001_r8
cosunz=SIN(LatRad)*SIN(Dangle)+ &
& COS(LatRad)*COS(Dangle)*COS(Hangle-xlon_el(i)*deg2rad)
zenith=ACOS(cosunz)
theta=zenith*rad2deg
IF ((theta.ge.0.0_r8).and.(theta.le.90.0_r8)) THEN
rm=1.0_r8/(cosunz+0.50572_r8*(96.07995_r8-theta)**rex)
rmp=rm*(Pair(i)*0.02952756_r8)/p0
rmo=(1.0_r8+22.0_r8/6370.0_r8)/ &
& SQRT(cosunz*cosunz+44.0_r8/6370.0_r8)
wspeed=SQRT(Uwind(i)*Uwind(i)+Vwind(i)*Vwind(i))
rh=Hair(i)
IF (rh.ge.100.0_r8) rh=99.9_r8
frh=((2.0_r8-rh*0.01_r8)/ &
(6.0_r8*(1.0_r8-rh*0.01_r8)))**0.333_r8
a_arr(1)=2000.0_r8*am*am
a_arr(2)=5.866_r8*(wspeed-2.2_r8)
a_arr(3)=0.01527_r8*(wspeed-2.2_r8)*0.05_r8
IF (a_arr(2).lt.0.5_r8) a_arr(2)=0.5_r8
IF (a_arr(3).lt.0.000014_r8) a_arr(3)=0.000014_r8
cff=1.0_r8/frh
DO nc=1,3
dndr(nc)=0.0_r8
DO ic=1,3
arg=LOG(r_arr(nc)/(frh*ro(ic)))
dndr(nc)=dndr(nc)+a_arr(ic)*EXP(-arg*arg)*cff
END DO
END DO
sumx=0.0_r8
sumy=0.0_r8
sumxy=0.0_r8
sumx2=0.0_r8
DO ic=1,3
cff1=LOG10(r_arr(ic))
cff2=LOG10(dndr(ic))
sumx=sumx+cff1
sumy=sumy+cff2
sumxy=sumxy+cff1*cff2
sumx2=sumx2+cff1*cff1
END DO
gamma=sumxy/sumx2
rlogc=sumy/3.0_r8-gamma*sumx/3.0_r8
alpha=-(gamma+3.0_r8)
beta=(3.91_r8/vis)*betalam**alpha
IF (alpha.gt.1.2_r8) THEN
asymp=0.65_r8
ELSE IF (alpha .lt. 0.0_r8) THEN
asymp=0.82_r8
ELSE
asymp=-0.14167_r8*alpha+0.82_r8
END IF
wa=(-0.0032_r8*am+0.972_r8)*EXP(0.000306_r8*rh)
alg=LOG(1.0_r8-asymp)
Fa=1.0_r8-0.5_r8* &
& EXP((alg*(1.459_r8+alg*(0.1595_r8+alg*0.4129_r8))+ &
& alg*(0.0783_r8+alg*(-0.3824_r8-alg*0.5874_r8))* &
& cosunz)*cosunz)
IF (wspeed.gt.4.0_r8) THEN
IF (wspeed.le.7.0_r8) THEN
rof=roair*(0.00062_r8+0.00156_r8/wspeed)* &
& 0.000022_r8*wspeed*wspeed-0.00040_r8
ELSE
rof=(roair*(0.00049_r8+0.000065_r8*wspeed)* &
& 0.000045_r8-0.000040_r8)*wspeed*wspeed
END IF
rosps=0.057_r8
ELSE
rof=0.0_r8
rosps=0.066_r8
END IF
IF ((theta.lt.40.0_r8).or.(wspeed.lt.2.0_r8)) THEN
IF (theta.eq.0.0_r8) THEN
rospd=0.0211_r8
ELSE
rtheta=zenith
rthetar=ASIN(SIN(rtheta)/rn)
rmin=rtheta-rthetar
rpls=rtheta+rthetar
rospd=0.5_r8*((SIN(rmin)*SIN(rmin))/ &
& (SIN(rpls)*SIN(rpls))+ &
& (TAN(rmin)*TAN(rmin))/ &
& (TAN(rpls)*TAN(rpls)))
END IF
ELSE
rospd=0.0253_r8*EXP((-0.000714_r8*wspeed+0.0618_r8)* &
& (theta-40.0_r8))
END IF
rod=rospd+rof
ros=rosps+rof
DO iband=1,NBands
rlam=ec_wave_ab(iband)*0.001_r8
rtra=EXP(-rmp/(115.6406_r8*rlam**4-1.335_r8*rlam**2))
otra=EXP(-ec_aoz(iband)*to3*rmo)
arg=beta*rm*rlam**(-alpha)
atra=EXP(-arg)
taa=EXP(-(1.0_r8-wa)*arg)
tas=EXP(-wa*arg)
gtra=EXP((-1.41_r8*ec_ag(iband)*rmp)/ &
& ((1.0_r8+118.3_r8*ec_ag(iband)*rmp)**0.45_r8))
wtra=EXP((-0.2385_r8*ec_aw(iband)*wv*rm)/ &
& ((1.0_r8+20.07_r8*ec_aw(iband)*wv*rm)**0.45_r8))
Edir(iband)=Fo(iband)*cosunz*rtra*otra*atra*gtra* &
& wtra*(1.0_r8-rod)
Edif(iband)=(1.0_r8-ros)* &
& Fo(iband)*cosunz*gtra*wtra*otra* &
& (taa*0.5_r8*(1.0_r8-rtra**0.95_r8)+ &
& taa*Fa*(1.0_r8-tas)*rtra**1.5_r8)
IF (cloud(i).gt.0.25_r8) THEN
Ed(iband)=(Edir(iband)+Edif(iband))* &
& (1.0_r8-0.75_r8*cloud(i)**3.4_r8)
Edif(iband)=Ed(iband)* &
& (0.3_r8+0.7_r8*cloud(i)**2.0_r8)
ELSE
Ed(iband)=Edir(iband)+Edif(iband)
END IF
SpecIr(i,iband)=Ed(iband)*10.0_r8*qlam(iband)
cff1=COS(ASIN((SIN(zenith))/rn))
cff2=Edif(iband)/Ed(iband)
avcos(i,iband)=cff1*(1.0_r8-cff2)+0.86_r8*cff2
END DO
ELSE
DO iband=1,NBands
SpecIr(i,iband)=0.0_r8
avcos(i,iband)=0.66564_r8
END DO
END IF
END DO
RETURN
END SUBROUTINE spec_ir
