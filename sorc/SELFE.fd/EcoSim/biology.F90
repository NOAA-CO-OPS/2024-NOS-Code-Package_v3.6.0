MODULE biology
USE bio_param
USE eclight
IMPLICIT NONE
SAVE
integer :: BioIter
integer, dimension(Nzoo) :: zoo_sp
logical :: RtUVR_flag
logical :: NFIX_flag
logical :: Regen_flag
integer :: NIT_flag, DENIT_flag, REAER_flag
real(r8), dimension(Nphy) :: HsNO3 = -99.d0
real(r8), dimension(Nphy) :: HsNH4 = -99.d0
real(r8), dimension(Nphy) :: HsSiO = -99.d0
real(r8), dimension(Nphy) :: HsPO4 = -99.d0
real(r8), dimension(Nphy) :: HsFe = -99.d0
real(r8), dimension(Nphy) :: GtALG_max = -99.d0
real(r8), dimension(Nphy) :: PhyTbase = -99.d0
real(r8), dimension(Nphy) :: PhyTfac = -99.d0
real(r8), dimension(Nphy) :: BET_ = -99.d0
real(r8), dimension(Nphy) :: maxC2nALG = -99.d0
real(r8), dimension(Nphy) :: minC2nALG = -99.d0
real(r8), dimension(Nphy) :: C2nALGminABS = -99.d0
real(r8), dimension(Nphy) :: maxC2SiALG = -99.d0
real(r8), dimension(Nphy) :: minC2SiALG = -99.d0
real(r8), dimension(Nphy) :: C2SiALGminABS = -99.d0
real(r8), dimension(Nphy) :: maxC2pALG = -99.d0
real(r8), dimension(Nphy) :: minC2pALG = -99.d0
real(r8), dimension(Nphy) :: C2pALGminABS = -99.d0
real(r8), dimension(Nphy) :: maxC2FeALG = -99.d0
real(r8), dimension(Nphy) :: minC2FeALG = -99.d0
real(r8), dimension(Nphy) :: C2FeALGminABS = -99.d0
real(r8), dimension(Nphy) :: qu_yld = -99.d0
real(r8), dimension(Nphy) :: E0_comp = -99.d0
real(r8), dimension(Nphy) :: E0_inhib = -99.d0
real(r8), dimension(Nphy) :: inhib_fac = -99.d0
real(r8), dimension(Nphy) :: C2CHL_max = -99.d0
real(r8), dimension(Nphy) :: mxC2Cl = -99.d0
real(r8), dimension(Nphy) :: b_C2Cl = -99.d0
real(r8), dimension(Nphy) :: mxC2Cn = -99.d0
real(r8), dimension(Nphy) :: b_C2Cn = -99.d0
real(r8), dimension(Nphy) :: mxPacEff = -99.d0
real(r8), dimension(Nphy) :: b_PacEff = -99.d0
real(r8), dimension(Nphy) :: mxChlB = -99.d0
real(r8), dimension(Nphy) :: b_ChlB = -99.d0
real(r8), dimension(Nphy) :: mxChlC = -99.d0
real(r8), dimension(Nphy) :: b_ChlC = -99.d0
real(r8), dimension(Nphy) :: mxPSC = -99.d0
real(r8), dimension(Nphy) :: b_PSC = -99.d0
real(r8), dimension(Nphy) :: mxPPC = -99.d0
real(r8), dimension(Nphy) :: b_PPC = -99.d0
real(r8), dimension(Nphy) :: mxLPUb = -99.d0
real(r8), dimension(Nphy) :: b_LPUb = -99.d0
real(r8), dimension(Nphy) :: mxHPUb = -99.d0
real(r8), dimension(Nphy) :: b_HPUb = -99.d0
real(r8), dimension(Nphy) :: FecDOC = -99.d0
real(r8), dimension(Nphy,Nfec) :: FecPEL = -99.d0
real(r8), dimension(Nphy) :: FecCYC = -99.d0
real(r8), dimension(Nphy) :: ExALG = -99.d0
real(r8), dimension(Nphy) :: WS = -99.d0
real(r8), dimension(Nphy) :: HsGRZ = -99.d0
real(r8), dimension(Nphy) :: basalPhy = -99.d0
real(r8), dimension(Nphy) :: QPhy = -99.d0
real(r8), dimension(NPhy) :: gamaPhy = -99.d0
real(r8), dimension(Nphy) :: MinRefuge = -99.d0
real(r8), dimension(Nphy) :: RefugeDep = -99.d0
real(r8), dimension(Nphy) :: Norm_Vol = -99.d0
real(r8), dimension(Nphy) :: Norm_Surf = -99.d0
real(r8), dimension(Nphy) :: HsDOP = -99.d0
real(r8), dimension(Nphy) :: C2pALKPHOS = -99.d0
real(r8), dimension(Nphy) :: HsDON = -99.d0
real(r8), dimension(Nphy) :: C2nNupDON = -99.d0
real(r8), dimension(Nbac) :: HsDOC_ba = -99.d0
real(r8), dimension(Nbac) :: GtBAC_max = -99.d0
real(r8), dimension(Nbac) :: BacTbase = -99.d0
real(r8), dimension(Nbac) :: BacTfac = -99.d0
real(r8), dimension(Nbac) :: basalBac = -99.d0
real(r8), dimension(Nbac) :: QBac = -99.d0
real(r8), dimension(Nbac) :: GEE0C = -99.d0
real(r8), dimension(Nbac) :: HsBacO = -99.d0
real(r8) :: C2nBAC = -99.d0
real(r8) :: C2pBAC = -99.d0
real(r8) :: C2FeBAC = -99.d0
real(r8) :: BacDOC = -99.d0
real(r8) :: BacPEL = -99.d0
real(r8) :: BacCYC = -99.d0
real(r8) :: ExBAC_c = -99.d0
real(r8) :: ExBacC2N = -99.d0
real(r8) :: Bac_Ceff = -99.d0
real(r8) :: RtNIT = -99.d0
real(r8) :: HsNIT = -99.d0
real(r8) :: QN = -99.d0
real(r8), dimension(Ndom) :: cDOCfrac_c = -99.d0
real(r8) :: RtUVR_DIC = -99.d0
real(r8) :: RtUVR_DOC = -99.d0
real(r8), dimension(Nfec) :: WF = -99.d0
real(r8), dimension(Nfec) :: RegTbase = -99.d0
real(r8), dimension(Nfec) :: RegTfac = -99.d0
real(r8), dimension(Nfec) :: RegCR = -99.d0
real(r8), dimension(Nfec) :: RegNR = -99.d0
real(r8), dimension(Nfec) :: RegSR = -99.d0
real(r8), dimension(Nfec) :: RegPR = -99.d0
real(r8), dimension(Nfec) :: RegFR = -99.d0
real(r8), dimension(Nzoo) :: ZooDOC = -99.d0
real(r8), dimension(Nzoo,Nfec) :: ZooPEL = -99.d0
real(r8), dimension(Nzoo) :: ZooCYC = -99.d0
real(r8), dimension(Nzoo,Nphy) :: DeltaZoo = -99.d0
real(r8), dimension(Nzoo,Nphy) :: EfcCap = -99.d0
real(r8), dimension(Nzoo) :: HsZoo = -99.d0
real(r8), dimension(Nzoo) :: EfcPrd = -99.d0
real(r8), dimension(Nzoo) :: ExZOO = -99.d0
real(r8), dimension(Nzoo) :: GZ = -99.d0
real(r8), dimension(Nzoo) :: basalZoo = -99.d0
real(r8), dimension(Nzoo) :: QZoo = -99.d0
real(r8), dimension(Nzoo) :: etaZoo = -99.d0
real(r8) :: omegaO2C = -99.d0
real(r8) :: omegaO2N = -99.d0
real(r8) :: omegaO2NDenit = -99.d0
real(r8) :: omegaS2O = -99.d0
real(r8) :: QWind = -99.d0
real(r8) :: psiWind = -99.d0
real(r8) :: alfaWind = -99.d0
real(r8) :: RtDenit = -99.d0
real(r8) :: MDenit = -99.d0
real(r8) :: reox_COD = -99.d0
real(r8) :: QCOD = -99.d0
real(r8) :: HsCOD = -99.d0
real(r8) :: pCO2a = -99.d0
real(r8) :: pH = -99.d0
real(r8), allocatable :: SpecIr(:,:)
real(r8), allocatable :: avcos(:,:)
real(r8), parameter :: DLAM = 5.0_r8
real(r8), parameter :: SMALL = 1.0e-6_r8
real(r8), parameter :: VSMALL = 1.0e-14_r8
real(r8), parameter :: LARGE = 1.0e+10_r8
real(r8), parameter :: VLARGE = 1.0e+20_r8
integer, parameter :: ilab=1
integer, parameter :: irct=2
integer, parameter :: ichl=1
integer, parameter :: isfc=1
integer, parameter :: iffc=2
real(r8), dimension(Nphy) :: ImaxC2nALG = -99.d0
real(r8), dimension(Nphy) :: ImaxC2SiALG = -99.d0
real(r8), dimension(Nphy) :: ImaxC2pALG = -99.d0
real(r8), dimension(Nphy) :: ImaxC2FeALG = -99.d0
real(r8) :: N2cBAC = -99.d0
real(r8) :: P2cBAC = -99.d0
real(r8) :: Fe2cBAC = -99.d0
real(r8), dimension(Nbac) :: HsNH4_ba = -99.d0
real(r8), dimension(Nbac) :: HsPO4_ba = -99.d0
real(r8), dimension(Nbac) :: HsFe_ba = -99.d0
real(r8) :: R_ExBAC_c = -99.d0
real(r8) :: ExBAC_n = -99.d0
real(r8) :: Frac_ExBAC_n = -99.d0
real(r8) :: I_Bac_Ceff = -99.d0
real(r8), dimension(NBands) :: wavedp = -99.d0
real(r8), dimension(Ndom) :: aDOC410 = -99.d0
real(r8), dimension(Ndom) :: aDOC300 = -99.d0
real(r8), parameter :: sec2day=1.0_r8/86400.0_r8
CONTAINS
SUBROUTINE initialize_biology
USE bio_param
USE eclight
integer :: ibac, iband, ifec, iphy, ng
integer :: izoo
ibac=0
DO iphy=1,Nphy
GtALG_max(iphy)=GtALG_max(iphy)*sec2day
ExALG(iphy)=ExALG(iphy)*sec2day
HsGRZ(iphy)=HsGRZ(iphy)*sec2day
WS(iphy)=WS(iphy)*sec2day
basalPhy(iphy)=basalPhy(iphy)*sec2day
END DO
DO ibac=1,Nbac
GtBAC_max(ibac)=GtBAC_max(ibac)*sec2day
basalBac(ibac)=basalBac(ibac)*sec2day
END DO
DO ifec=1,Nfec
WF(ifec)=WF(ifec)*sec2day
END DO
RtNIT=RtNIT*sec2day
RtDENIT=RtDENIT*sec2day
DO izoo=1,Nzoo
ExZOO(izoo)= ExZOO(izoo)*sec2day
GZ(izoo)=GZ(izoo)*sec2day
basalZoo(izoo)=basalZoo(izoo)*sec2day
END DO
reox_COD=reox_COD*sec2day
DO iphy=1,Nphy
IF (maxC2nALG(iphy).gt.SMALL) THEN
ImaxC2nALG(iphy)=1.0_r8/maxC2nALG(iphy)
ELSE
ImaxC2nALG(iphy)=0.0_r8
END IF
IF (maxC2SiALG(iphy).gt.SMALL) THEN
ImaxC2SiALG(iphy)=1.0_r8/maxC2SiALG(iphy)
ELSE
ImaxC2SiALG(iphy)=0.0_r8
END IF
IF (maxC2pALG(iphy).gt.SMALL) THEN
ImaxC2pALG(iphy)=1.0_r8/maxC2pALG(iphy)
ELSE
ImaxC2pALG(iphy)=0.0_r8
END IF
IF (maxC2FeALG(iphy).gt.SMALL) THEN
ImaxC2FeALG(iphy)=1.0_r8/maxC2FeALG(iphy)
ELSE
ImaxC2FeALG(iphy)=0.0_r8
END IF
END DO
DO ibac=1,Nbac
HsNH4_ba(ibac)=HsDOC_ba(ibac)/C2nBAC
HsPO4_ba(ibac)=HsDOC_ba(ibac)/C2pBAC
HsFe_ba (ibac)=HsDOC_ba(ibac)/C2FeBAC
END DO
N2cBAC=1.0_r8/C2nBAC
P2cBAC=1.0_r8/C2pBAC
Fe2cBAC=1.0_r8/C2FeBAC
I_Bac_Ceff=1.0_r8/Bac_Ceff
R_ExBAC_c=1.0_r8/(1.0_r8-ExBAC_c)
ExBAC_n=ExBAC_c*C2nBAC/ExBacC2N
Frac_ExBAC_n=1.0_r8-ExBAC_n
RtUVR_DIC=RtUVR_DIC/3600.0_r8
RtUVR_DOC=RtUVR_DOC/3600.0_r8
IF (Regen_flag) THEN
DO ifec=1,Nfec
RegCR(ifec)=RegCR(ifec)*sec2day
RegNR(ifec)=RegNR(ifec)*sec2day
RegPR(ifec)=RegPR(ifec)*sec2day
RegFR(ifec)=RegFR(ifec)*sec2day
RegSR(ifec)=RegSR(ifec)*sec2day
END DO
ELSE
DO ifec=1,Nfec
RegCR(ifec)=0.0_r8
RegNR(ifec)=0.0_r8
RegPR(ifec)=0.0_r8
RegFR(ifec)=0.0_r8
RegSR(ifec)=0.0_r8
END DO
END IF
DO iband=1,NBands
wavedp(iband)=(550.0_r8/(397.0_r8+REAL(iband,r8)*DLAM))
END DO
aDOC410(ilab)=aDOC(ilab,1)*EXP(0.014_r8*(ec_wave_ab(1)-410.0_r8))
aDOC410(irct)=aDOC(irct,1)*EXP(0.025_r8*(ec_wave_ab(1)-410.0_r8))
aDOC300(ilab)=EXP(0.0145_r8*(410.0_r8-300.0_r8))
aDOC300(irct)=EXP(0.0145_r8*(410.0_r8-300.0_r8))
RETURN
END SUBROUTINE initialize_biology
END MODULE biology
