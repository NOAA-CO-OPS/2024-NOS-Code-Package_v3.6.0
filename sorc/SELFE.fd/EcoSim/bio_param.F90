MODULE bio_param
IMPLICIT NONE
SAVE
#ifdef USE_SINGLE
integer, parameter :: r8 = 4
#else
integer, parameter :: r8 = 8
#endif
integer, parameter :: NBands = 60
integer, parameter :: NAT = 2
integer, parameter :: Nbac = 1
integer, parameter :: Ndom = 1
integer, parameter :: Nfec = 1
integer, parameter :: Nphy = 1
integer, parameter :: Npig = 7
integer, parameter :: Nzoo = 1
integer, parameter, dimension(7,7) :: PIG = reshape ( &
& (/ 1, 1, 1, 1, 1, 1, 1, &
& 0, 0, 0, 0, 0, 0, 0, &
& 0, 0, 0, 0, 0, 0, 0, &
& 0, 0, 0, 0, 0, 0, 0, &
& 0, 0, 0, 0, 0, 0, 0, &
& 0, 0, 0, 0, 0, 0, 0, &
& 0, 0, 0, 0, 0, 0, 0 /), &
& (/ 7, 7 /) )
integer, parameter, dimension(Nphy) :: PHY = (/ 1 /)
integer, parameter :: IRON=0
integer, parameter :: CDOC=0
integer :: itemp
integer :: isalt
integer, pointer :: idbio(:)
integer :: iBacC(Nbac)
integer :: iBacN(Nbac)
integer :: iBacP(Nbac)
integer :: iBacF(Nbac)
integer :: iCDMC(Ndom)
integer :: iDOMC(Ndom)
integer :: iDOMN(Ndom)
integer :: iDOMP(Ndom)
integer :: iFecC(Nfec)
integer :: iFecN(Nfec)
integer :: iFecP(Nfec)
integer :: iFecF(Nfec)
integer :: iFecS(Nfec)
integer :: iPhyC(Nphy)
integer :: iPhyN(Nphy)
integer :: iPhyP(Nphy)
integer :: iPhyF(Nphy)
integer :: iPhyS(Nphy)
integer :: iPigs(Nphy,Npig)
integer :: iNO3_
integer :: iNH4_
integer :: iPO4_
integer :: iFeO_
integer :: iSiO_
integer :: iDIC_
integer :: FirstPig
integer :: iZooC(Nzoo)
integer :: iZooN(Nzoo)
integer :: iZooP(Nzoo)
integer :: iDO_
integer :: iCOD_
character (len=16), dimension(Nbac) :: BacName
character (len=11), dimension(Ndom) :: DomName
character (len=13), dimension(Nfec) :: FecName
character (len=21), dimension(Nphy) :: PhyName
character (len=21), dimension(Nzoo) :: ZooName
character (len=39), dimension(7) :: PigName = &
& (/ 'chlorophyll-a ', &
& 'chlorophyll-b ', &
& 'chlorophyll-c ', &
& 'photosythetic carotenoids ', &
& 'photoprotective carotenoids ', &
& 'low urobilin phycoeurythin carotenoids ', &
& 'high urobilin phycoeurythin carotenoids' /)
real(r8) :: day, month, year, hour, minutes, seconds, yday
real(r8), parameter :: deg2rad = 3.14159265/180._r8
real(r8), parameter :: rad2deg = 180._r8/3.14159265
real(r8) :: Cp = 3985.0_r8
real(r8) :: Csolar = 700.0_r8
real(r8) :: Eradius = 6371315.0_r8
real(r8) :: StefBo = 5.67E-8_r8
real(r8) :: emmiss = 0.97_r8
real(r8) :: rhow = 1000.0_r8
real(r8) :: g = 9.81_r8
real(r8) :: gorho0
real(r8) :: vonKar = 0.41_r8
real(r8), dimension(5) :: lmd_mu1 = &
& (/ 0.35_r8, 0.6_r8, 1.0_r8, 1.5_r8, 1.4_r8 /)
real(r8), dimension(5) :: lmd_mu2 = &
& (/ 23.0_r8, 20.0_r8, 17.0_r8, 14.0_r8, 7.9_r8 /)
real(r8), dimension(5) :: lmd_r1 = &
& (/ 0.58_r8, 0.62_r8, 0.67_r8, 0.77_r8, 0.78_r8 /)
common /param/ NBIT
integer :: NBIT
CONTAINS
SUBROUTINE initialize_param
integer :: i, j
NBIT=5+(Nbac*3)+(Ndom*3)+(Nfec*4)+(Nphy*3)+(Nzoo*3)+2
DO i=1,Nphy
IF (PHY(i).le.2) NBIT=NBIT+1
END DO
DO j=1,Npig
DO i=1,Nphy
IF (PIG(PHY(i),j).eq.1) NBIT=NBIT+1
END DO
END DO
IF (IRON==1) NBIT=NBIT+1+Nbac*1+Nfec*1+Nphy*1
IF (CDOC==1) NBIT=NBIT+Ndom*1
RETURN
END SUBROUTINE
SUBROUTINE initialize_scalars
integer :: i, ic, j
character (len=21), dimension(7) :: PhyGroups = &
& (/ 'small diatom ', &
& 'large diatom ', &
& 'small dinoflagellate ', &
& 'large dinoflagellate ', &
& 'synechococcus ', &
& 'small prochlorococcus', &
& 'large prochlorococcus' /)
allocate ( idbio(NBIT) )
ic=0
DO i=1,NBIT
idbio(i)=ic+i
END DO
iDIC_=ic+1
IF(IRON==1)THEN
iFeO_=ic+2
iNH4_=ic+3
iNO3_=ic+4
iPO4_=ic+5
iSiO_=ic+6
ic=ic+6
ELSE
iNH4_=ic+2
iNO3_=ic+3
iPO4_=ic+4
iSiO_=ic+5
ic=ic+5
END IF
DO i=1,Nbac
IF(IRON==1)THEN
iBacC(i)=ic+1
iBacF(i)=ic+2
iBacN(i)=ic+3
iBacP(i)=ic+4
ic=ic+4
ELSE
iBacC(i)=ic+1
iBacN(i)=ic+2
iBacP(i)=ic+3
ic=ic+3
END IF
END DO
IF(CDOC==1.AND.Ndom==2)THEN
iCDMC(1)=ic+1
iDOMC(1)=ic+2
iDOMN(1)=ic+3
iDOMP(1)=ic+4
iCDMC(2)=ic+5
iDOMC(2)=ic+6
iDOMN(2)=ic+7
ic=ic+7
ELSEIF(CDOC==1.AND.Ndom==1)THEN
iCDMC(1)=ic+1
iDOMC(1)=ic+2
iDOMN(1)=ic+3
iDOMP(1)=ic+4
ic=ic+4
ELSEIF (CDOC==0.AND.Ndom==2)THEN
iDOMC(1)=ic+1
iDOMN(1)=ic+2
iDOMP(1)=ic+3
iDOMC(2)=ic+4
iDOMN(2)=ic+5
ic=ic+5
ELSE
iDOMC(1)=ic+1
iDOMN(1)=ic+2
iDOMP(1)=ic+3
ic=ic+3
ENDIF
DO i=1,Nfec
IF(IRON==1)THEN
iFecC(i)=ic+1
iFecF(i)=ic+2
iFecN(i)=ic+3
iFecP(i)=ic+4
iFecS(i)=ic+5
ic=ic+5
ELSE
iFecC(i)=ic+1
iFecN(i)=ic+2
iFecP(i)=ic+3
iFecS(i)=ic+4
ic=ic+4
END IF
END DO
DO i=1,Nphy
IF(IRON==1)THEN
iPhyC(i)=ic+1
iPhyF(i)=ic+2
iPhyN(i)=ic+3
iPhyP(i)=ic+4
ic=ic+4
ELSE
iPhyC(i)=ic+1
iPhyN(i)=ic+2
iPhyP(i)=ic+3
ic=ic+3
ENDIF
END DO
DO i=1,Nphy
IF (PHY(i).le.2) THEN
ic=ic+1
iPhyS(i)=ic
ELSE
iPhyS(i)=0
END IF
END DO
FirstPig=ic+1
DO j=1,Npig
DO i=1,Nphy
iPigs(i,j)=0
IF (PIG(PHY(i),j).eq.1) THEN
ic=ic+1
iPigs(i,j)=ic
END IF
END DO
END DO
DO i=1,Nzoo
iZooC(i)=ic+1
iZooN(i)=ic+2
iZooP(i)=ic+3
ic=ic+3
END DO
iDO_=ic+1
iCOD_=ic+2
itemp = ic+3
isalt = ic+4
DO i=1,Nphy
PhyName(i)=PhyGroups(PHY(i))
END DO
DO i=1,Nbac
WRITE (BacName(i),'(a,1x,i1)') 'Bacteria Group', i
END DO
DO i=1,Ndom
WRITE (DomName(i),'(a,1x,i1)') 'DOM Group', i
END DO
DO i=1,Nfec
WRITE (FecName(i),'(a,1x,i1)') 'Fecal Group', i
END DO
DO i=1,Nzoo
WRITE (ZooName(i),'(a,1x,i1)') 'Zooplankton Group', i
END DO
RETURN
END SUBROUTINE initialize_scalars
END MODULE bio_param
