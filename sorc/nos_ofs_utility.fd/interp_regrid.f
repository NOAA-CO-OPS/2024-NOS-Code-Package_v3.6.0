      SUBROUTINE interp_regrid (iflag,Nx, Ny, Xinp, Yinp, Ainp,                       
     &                   IM,JM,Xout,Yout,Aout,Iout,Jout,INIT)
!
!svn $Id: regrid.F 139 2008-01-10 00:17:29Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2008 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine interpolates gridded data, Ainp, to model locations    !
!  Xout and Yout.                                                      !
!                                                                      !
!  On Input:                                                           !
!     iflag      Interpolation flag (0: linear, 1: cubic).             !
!     Nx         X-dimension size for gridded data, Ainp.              !
!     Ny         Y-dimension size for gridded data, Ainp.              !
!     Ainp       Gridded data to interpolate from.                     !
!     Amin       Gridded data minimum value.                           !
!     Amax       Gridded data maximum value.                           !
!     IM         X-dimension size for model grid, Aout.                !
!     JM         Y-dimension size for model grid, Aout.                !
!     Xout       X-locations to interpolate.                           !
!     Yout       Y-locations to interpolate.                           !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Aout       Interpolated field.                                   !
!                                                                      !
!=======================================================================
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: iflag,INIT
      integer, intent(in) :: Nx, Ny
      integer, intent(in) :: IM, JM
!
!      real, intent(inout) :: Amin, Amax
!
      real, intent(in) :: Xinp(Nx,Ny)
      real, intent(in) :: Yinp(Nx,Ny)
      real, intent(in) :: Ainp(Nx,Ny)

      real, intent(in) :: Xout(IM,JM)
      real, intent(in) :: Yout(IM,JM)
      real, intent(out):: Aout(IM,JM)
!
!  Local variable declarations
!
      integer I,J,ic,jc
      real, dimension(IM,JM) :: Iout
      real, dimension(IM,JM) :: Jout
      real, dimension(Nx,Ny) :: angle
      real, parameter :: IJspv = 0.0
      real my_min, my_max, Xmin, Xmax, Ymin, Ymax
      logical rectangular
      integer I0,J0
!
!  Determine "rectangular" switch.
!
!        print *,'running regridding iflag=',iflag
!	print *,'rank of inp arrays= ',Nx,Ny
!	print *,' rank of lon= ',shape(Xout),size(shape(Xout))
        rectangular=.FALSE.
        IF (size(shape(Xinp)) .eq.1) THEN
          rectangular=.TRUE.
        ELSE
          ic=1
          DO i=2,Nx
            IF (Yinp(i,1).eq.Yinp(1,1)) THEN
              ic=ic+1
            END IF
          END DO
        END IF
        IF (size(shape(Yinp)) .eq.1) THEN
          rectangular=.TRUE.
        ELSE
          jc=1
          DO j=2,Ny
            IF (Xinp(1,j).eq.Xinp(1,1)) THEN
              jc=jc+1
            END IF
          END DO
        END IF
        IF (((ic.ne.0).and.(ic.eq.Nx)).and.                             
     &      ((jc.ne.0).and.(jc.eq.Ny))) THEN
          rectangular=.TRUE.
        END IF
!
 !  Set input gridded data rotation angle.
      DO i=1,Nx
        DO j=1,Ny
          angle(i,j)=0.0
        END DO
      END DO


!
!
!-----------------------------------------------------------------------
!  Check if gridded data contains model grid.
!-----------------------------------------------------------------------
!
!  Determine minimum and maximum positions.
!
        Xmin=1.0E+35
        Xmax=-1.0E+35
        Ymin=1.0E+35
        Ymax=-1.0E+35
        DO j=1,Ny
          DO i=1,Nx
            Xmin=MIN(Xmin,Xinp(i,j))
            Xmax=MAX(Xmax,Xinp(i,j))
            Ymin=MIN(Ymin,Yinp(i,j))
            Ymax=MAX(Ymax,Yinp(i,j))
          END DO
        END DO
!
      IF( (Minval(Xout).lt.Minval(Xinp)).or.                                      
     &    (Maxval(Xout).gt.Maxval(Xinp)).or.                                      
     &    (Minval(Yout).lt.Minval(Yinp)).or.                                      
     &    (Maxval(Yout).gt.Maxval(Yinp)) ) THEN
          WRITE (*,10)
     &	   Minval(Xout), Maxval(Xout), Minval(Yout), Maxval(Yout),                     
     &     Minval(Xinp), Maxval(Xinp), Minval(Yinp), Maxval(Yinp)
 10       FORMAT (/, ' REGRID - input gridded data does not contain',   
     &               ' model grid:', /,                                 
     &            /,10x,'Gridded:  LonMin = ',f9.4,' LonMax = ',f9.4,   
     &            /,10x,'          LatMin = ',f9.4,' LatMax = ',f9.4,   
     &            /,10x,'Model:    LonMin = ',f9.4,' LonMax = ',f9.4,   
     &            /,10x,'          LatMin = ',f9.4,' LatMax = ',f9.4)
!        exit_flag=4
!AJ        RETURN
      END IF
!
!-----------------------------------------------------------------------
!  Interpolate (bilinear or bicubic) to requested positions.
!-----------------------------------------------------------------------
!
!  Find fractional indices (Iout,Jout) of the grid cells in Ainp
!  containing positions to intepolate.
!
      IF (INIT .LE. 0)THEN
         CALL hindices ( 1,1,Nx,1,Ny,1,Nx,1,Ny, 
     &               angle, Xinp, Yinp,                    
     &               1,IM,1,JM,1,IM,1,JM,                         
     &               Xout, Yout,                           
     &               Iout, Jout,                           
     &               IJspv, rectangular)
!        print *,'Search coarser grid indeces directions are done!!!'
	RETURN
      ELSE
        IF (iflag.eq.0) THEN
          CALL linterp2d (1, 1, Nx, 1, Ny,                  
     &                  Xinp, Yinp, Ainp,                  
     &                  1, IM, 1, JM,                
     &                  1, IM, 1, JM,                
     &                  Iout, Jout, Xout, Yout,            
     &                  Aout, my_min, my_max)
        ELSE IF (iflag.eq.1) THEN
!         print *, 'inpterpolating data using bicubic spline mthod...' 
          CALL cinterp2d (1, 1, Nx, 1, Ny,                  
     &                  Xinp, Yinp, Ainp,                  
     &                  1, IM, 1, JM,                
     &                  1, IM, 1, JM,                
     &                  Iout, Jout, Xout, Yout,            
     &                  Aout, my_min, my_max)
        END IF
!        print *,'interpolation is done'
      ENDIF
      RETURN
      END  

      SUBROUTINE linterp2d (ng, LBx, UBx, LBy, UBy,                    
     &                      Xinp, Yinp, Finp,                          
     &                      LBi, UBi, LBj, UBj,                        
     &                      Istr, Iend, Jstr, Jend,                    
     &                      Iout, Jout,                                
     &                      Xout, Yout,                                
     &                      Fout, Fmin, Fmax)
!
!=======================================================================
!                                                                      !
!  Given any gridded 2D field, Finp, this routine linearly interpolate !
!  to locations (Xout,Yout).  To facilitate the  interpolation  within !
!  any irregularly gridded 2D field,  the fractional grid cell indices !
!  (Iout,Jout) with respect Finp are needed at input.  Notice that the !
!  routine "hindices" can be used to compute these indices.            !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng         Nested grid number.                                   !
!     LBx        I-dimension lower bound of gridded field, Finp.       !
!     UBx        I-dimension upper bound of gridded field, Finp.       !
!     LBy        J-dimension lower bound of gridded field, Finp.       !
!     UBy        J-dimension upper bound of gridded field, Finp.       !
!     Xinp       X-locations of gridded field, Finp.                   !
!     Yinp       Y-locations of gridded field, Finp.                   !
!     Finp       2D field to interpolate from.                         !
!     LBi        I-dimension Lower bound of data to interpolate, Fout. !
!     UBi        I-dimension Upper bound of data to interpolate, Fout. !
!     LBj        J-dimension Lower bound of data to interpolate, Fout. !
!     UBj        J-dimension Upper bound of data to interpolate, Fout. !
!     Istr       Starting data I-index to interpolate, Fout.           !
!     Iend       Ending   data I-index to interpolate, Fout.           !
!     Jstr       Starting data J-index to interpolate, Fout.           !
!     Jend       Ending   data J-index to interpolate, Fout.           !
!     Iout       I-fractional Xinp grid cell containing Xout.          !
!     Jout       J-fractional Yinp grid cell containing Yout.          !
!     Xout       X-locations to interpolate, Fout.                     !
!     Yout       Y-locations to interpolate, Fout.                     !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Fout       Interpolated 2D field.                                !
!     Fmin       Interpolated field minimum value.                     !
!     Fmax       Interpolated field maximum value.                     !
!                                                                      !
!=======================================================================
!
    !  USE mod_param
    !  USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, LBx, UBx, LBy, UBy
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: Istr, Iend, Jstr, Jend
!
      real, intent(in) :: Xinp(LBx:UBx,LBy:UBy)
      real, intent(in) :: Yinp(LBx:UBx,LBy:UBy)
      real, intent(in) :: Finp(LBx:UBx,LBy:UBy)

      real, intent(in) :: Iout(LBi:UBi,LBj:UBj)
      real, intent(in) :: Jout(LBi:UBi,LBj:UBj)
      real, intent(in) :: Xout(LBi:UBi,LBj:UBj)
      real, intent(in) :: Yout(LBi:UBi,LBj:UBj)

      real, intent(out) :: Fout(LBi:UBi,LBj:UBj)
!
      real, intent(out) :: Fmin, Fmax
!
!  Local variable declarations.
!
      integer :: i, i1, i2, j, j1, j2

      real :: p1, p2, q1, q2
      real,dimension(0:1,0:1):: f
!
!-----------------------------------------------------------------------
!  Linearly interpolate requested field
!-----------------------------------------------------------------------
!
      Fmin=1.0E+35
      Fmax=-1.0E+35
      DO j=Jstr,Jend
        DO i=Istr,Iend
          i1=INT(Iout(i,j))
          i2=i1+1
          j1=INT(Jout(i,j))
          j2=j1+1
          IF (((LBx.le.i1).and.(i1.le.UBx)).and.                       
     &        ((LBy.le.j1).and.(j1.le.UBy))) THEN
!            print*, I,J,Finp(i1,j1),Finp(i2,j1),Finp(i1,j2),Finp(i2,j2)
            p2=REAL(i2-i1)*(Iout(i,j)-REAL(i1))
            q2=REAL(j2-j1)*(Jout(i,j)-REAL(j1))
            p1=1.0-p2
            q1=1.0-q2
            k1=0
            sum=0.0
            IF (Finp(i1,j1) .GT. -9999.0) THEN
                 k1=k1+1
                 sum=sum+Finp(i1,j1)
            END IF
            IF (Finp(i2,j1) .GT. -9999.0) THEN
                 k1=k1+1
                 sum=sum+Finp(i2,j1)
            END IF
            IF (Finp(i1,j2) .GT. -9999.0) THEN
                  k1=k1+1
                  sum=sum+Finp(i1,j2)
            END IF
            IF (Finp(i2,j2) .GT. -9999.0) THEN
                  k1=k1+1
                  sum=sum+Finp(i2,j2)
            END IF
            f(0,0)=Finp(i1,j1)
            f(1,0)=Finp(i2,j1)
            f(0,1)=Finp(i1,j2)
            f(1,1)=Finp(i2,j2)
            IF (k1 .LT. 4 .and. k1 .GT. 0) THEN
                if (Finp(i1,j1) .LT. -9999.0) f(0,0)=sum/k1
                if (Finp(i2,j1) .LT. -9999.0) f(1,0)=sum/k1
                if (Finp(i1,j2) .LT. -9999.0) f(0,1)=sum/k1
                if (Finp(i2,j2) .LT. -9999.0) f(1,1)=sum/k1
            ELSEIF (k1 .EQ. 0) THEN
!                WRITE(6,*) 'No valid values at 
!     & surrounding points for',i,j 
                Fout(i,j)=-99999.9
!                WRITE(*,*)'set to -99999.9'
                GO TO 777
            END IF
            Fout(i,j)=p1*q1*f(0,0)+ p2*q1*f(1,0)+                               
     &                p2*q2*f(1,1)+ p1*q2*f(0,1)
            Fmin=MIN(Fmin,Fout(i,j))
            Fmax=MAX(Fmax,Fout(i,j))
          END IF
777       CONTINUE
        END DO
      END DO

      END SUBROUTINE linterp2d

      SUBROUTINE cinterp2d (ng, LBx, UBx, LBy, UBy,                    
     &                      Xinp, Yinp, Finp,                          
     &                      LBi, UBi, LBj, UBj,                        
     &                      Istr, Iend, Jstr, Jend,                    
     &                      Iout, Jout,                                
     &                      Xout, Yout,                                
     &                      Fout, Fmin, Fmax)
!
!=======================================================================
!                                                                      !
!  Given any gridded 2D field,  Finp, at locations (Xinp,Yinp) this    !
!  routine performs bicubic interpolation at locations (Xout,Yout).    !
!  To facilitate the interpolation within any  irregularly  gridded    !
!  field, the fractional grid cell indices (Iout,Jout) with respect    !
!  Finp are needed at input. Notice that the routine "hindices" can    !
!  be used to compute these indices.                                   !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng         Nested grid number.                                   !
!     LBx        I-dimension lower bound of gridded field, Finp.       !
!     UBx        I-dimension upper bound of gridded field, Finp.       !
!     LBy        J-dimension lower bound of gridded field, Finp.       !
!     UBy        J-dimension upper bound of gridded field, Finp.       !
!     Xinp       X-locations of gridded field, Finp.                   !
!     Yinp       Y-locations of gridded field, Finp.                   !
!     Finp       2D field to interpolate from.                         !
!     LBi        I-dimension Lower bound of data to interpolate, Fout. !
!     UBi        I-dimension Upper bound of data to interpolate, Fout. !
!     LBj        J-dimension Lower bound of data to interpolate, Fout. !
!     UBj        J-dimension Upper bound of data to interpolate, Fout. !
!     Istr       Starting data I-index to interpolate, Fout.           !
!     Iend       Ending   data I-index to interpolate, Fout.           !
!     Jstr       Starting data J-index to interpolate, Fout.           !
!     Jend       Ending   data J-index to interpolate, Fout.           !
!     Iout       I-fractional Xinp grid cell containing Xout.          !
!     Jout       J-fractional Yinp grid cell containing Yout.          !
!     Xout       X-locations to interpolate, Fout.                     !
!     Yout       Y-locations to interpolate, Fout.                     !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Fout       Interpolated 2D field.                                !
!     Fmin       Interpolated field minimum value.                     !
!     Fmax       Interpolated field maximum value.                     !
!                                                                      !
!=======================================================================
!
  !    USE mod_param
  !    USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, LBx, UBx, LBy, UBy
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: Istr, Iend, Jstr, Jend
!
      real, intent(in) :: Xinp(LBx:UBx,LBy:UBy)
      real, intent(in) :: Yinp(LBx:UBx,LBy:UBy)
      real, intent(in) :: Finp(LBx:UBx,LBy:UBy)

      real, intent(in) :: Iout(LBi:UBi,LBj:UBj)
      real, intent(in) :: Jout(LBi:UBi,LBj:UBj)
      real, intent(in) :: Xout(LBi:UBi,LBj:UBj)
      real, intent(in) :: Yout(LBi:UBi,LBj:UBj)

      real, intent(out) :: Fout(LBi:UBi,LBj:UBj)
!
      real, intent(out) :: Fmin, Fmax
!
!  Local variable declarations.
!
      integer i, ic, iter, i1, i2, j, jc, j1, j2

      real :: a11, a12, a21, a22
      real :: e11, e12, e21, e22
      real :: cff, d1, d2, dfc, dx, dy, eta, xi, xy, yx
      real :: f0, fx, fxx, fxxx, fxxy, fxy, fxyy, fy, fyy, fyyy   

      real, parameter :: C01 = 1.0/48.0
      real, parameter :: C02 = 1.0/32.0
      real, parameter :: C03 = 0.0625                  ! 1/16
      real, parameter :: C04 = 1.0/6.0
      real, parameter :: C05 = 0.25
      real, parameter :: C06 = 0.5
      real, parameter :: C07 = 0.3125                  ! 5/16
      real, parameter :: C08 = 0.625                   ! 5/8
      real, parameter :: C09 = 1.5
      real, parameter :: C10 = 13.0/24.0

      real, parameter :: LIMTR = 3.0
      real, parameter :: spv = -9999.0            ! HGA need work

      real, dimension(-1:2,-1:2) :: dfx, dfy, f
!
!-----------------------------------------------------------------------
!  Interpolates requested field locations (Xout,Yout).
!-----------------------------------------------------------------------
!
      Fmin=1.0E+35
      Fmax=-1.0E+35
      DO j=Jstr,Jend
        DO i=Istr,Iend
          i1=INT(Iout(i,j))
          i2=i1+1
          j1=INT(Jout(i,j))
          j2=j1+1
          IF (((LBx.le.i1).and.(i1.le.UBx)).and.                        
     &        ((LBy.le.j1).and.(j1.le.UBy))) THEN
!
!  Determine local fractional coordinates (xi,eta) corresponding to
!  the target point (Xout,Yout) on the grid (Xinp,Yinp). Here, "xi"
!  and "eta" are defined, in such a way, that xi=eta=0 corresponds
!  to the middle of the cell (i1:i1+1,j1:j1+1), while xi=+/-1/2 and
!  eta=+/-1/2 (any combination +/- signs) corresponds to the four
!  corner points of the cell. Inside the cell it is assumed that
!  (Xout,Yout) are expressed via bi-linear functions of (xi,eta),
!  where term proportional to xi*eta does not vanish because
!  coordinate transformation may be at least weakly non-orthogonal
!  due to discretization errors. The associated non-linear system
!  is solved by iterative method of Newton.
!
            xy=Xinp(i2,j2)-Xinp(i1,j2)-Xinp(i2,j1)+Xinp(i1,j1)
            yx=Yinp(i2,j2)-Yinp(i1,j2)-Yinp(i2,j1)+Yinp(i1,j1)
            dx=Xout(i,j)-0.25*(Xinp(i2,j2)+Xinp(i1,j2)+              
     &                            Xinp(i2,j1)+Xinp(i1,j1))
            dy=Yout(i,j)-0.25*(Yinp(i2,j2)+Yinp(i1,j2)+              
     &                            Yinp(i2,j1)+Yinp(i1,j1))
!
!  The coordinate transformation matrix:
!
!           e11 e12
!           e21 e22
!
!  contains derivatives of (Xinp,Yinp) with respect to (xi,eta). Because
!  the coordinates may be non-orthogonal (at least due to discretization
!  errors), the nonlinear system
!
!           e11*xi+e12*eta+xy*xi*eta=dx
!           e21*xi+e22*eta+yx*xi*eta=dy
!
!  needs to be solved in order to retain symmetry.
!
            e11=0.5*(Xinp(i2,j2)-Xinp(i1,j2)+Xinp(i2,j1)-Xinp(i1,j1))
            e12=0.5*(Xinp(i2,j2)+Xinp(i1,j2)-Xinp(i2,j1)-Xinp(i1,j1))
            e21=0.5*(Yinp(i2,j2)-Yinp(i1,j2)+Yinp(i2,j1)-Yinp(i1,j1))
            e22=0.5*(Yinp(i2,j2)+Yinp(i1,j2)-Yinp(i2,j1)-Yinp(i1,j1))
!
            cff=1.0/(e11*e22-e12*e21)
            xi=cff*(e22*dx-e12*dy)
            eta=cff*(e11*dy-e21*dx)
!
            DO iter=1,4
              d1=dx-e11*xi-e12*eta-xy*xi*eta
              d2=dy-e21*xi-e22*eta-yx*xi*eta
              a11=e11+xy*eta
              a12=e12+xy*xi
              a21=e21+yx*eta
              a22=e22+yx*xi
              cff=1.0/(a11*a22-a12*a21)
              xi =xi +cff*(a22*d1-a12*d2)
              eta=eta+cff*(a11*d2-a21*d1)
            END DO
!
!  Algorithm below is equivalent to the one above, except that special
!  care is taken to avoid interpolation accross land. This is achieved
!  by shortening the stencil and reducing order of polynomial, if
!  extreme points of the stencil touch land. This is achieved by
!  expressing all f0,fx,fy,...,fxyy in terms of values of interpolated
!  field at the four corners of central cell (which already checked to
!  stay away from land), and eight one-sided differences dfx,dfy (see
!  below) in such a way that field values at the extreme points of the
!  12-point stencil do not participate directly into f0,fx,...,fxyy.
!  Should an extreme point of the stencil touch land, thus making it
!  impossible to compute the corresponding one-sided difference, this
!  difference is retracted toward the center of the stencil.
!
!  Optionally, a slope-limiting algorithm may be employed to prevent
!  spurious oscillations of the interpolant. This is a valuable property,
!  if dealing with rough data, however, as a side effect, it turns off
!  high-order interpolation in the vicinity of extrema.
!
!  The slope-limiting algorithm employed here checks that two consecutive
!  elementary differences, "dfx" and "dfc" have the same sign and differ
!  in magnitude by no more than factor of 3.
!
            f(0,0)=Finp(i1,j1)
            f(1,0)=Finp(i2,j1)
            f(0,1)=Finp(i1,j2)
            f(1,1)=Finp(i2,j2)
! Fill in possible bad values for the four corners
            k1=0
            sum=0.0
            IF (Finp(i1,j1) .GT. -9999.0) THEN
                 k1=k1+1
                 sum=sum+Finp(i1,j1)
            END IF
            IF (Finp(i2,j1) .GT. -9999.0) THEN
                 k1=k1+1
                 sum=sum+Finp(i2,j1)
            END IF
            IF (Finp(i1,j2) .GT. -9999.0) THEN
                  k1=k1+1
                  sum=sum+Finp(i1,j2)
            END IF
            IF (Finp(i2,j2) .GT. -9999.0) THEN
                  k1=k1+1
                  sum=sum+Finp(i2,j2)
            END IF
            IF (k1 .LT. 4 .and. k1 .GT. 0) THEN
                if (Finp(i1,j1) .LT. spv) f(0,0)=sum/k1
                if (Finp(i2,j1) .LT. spv) f(1,0)=sum/k1
                if (Finp(i1,j2) .LT. spv) f(0,1)=sum/k1
                if (Finp(i2,j2) .LT. spv) f(1,1)=sum/k1
            ELSEIF (k1 .EQ. 0) THEN
!                WRITE(6,*) 'No valid values at
!     & surrounding points for',i,j
!                WRITE(*,*)'set to -99999.9'
                Fout(i,j)=-99999.9
                GO TO 777
            END IF
!
            dfc=f(1,1)-f(0,1)
            IF (i1+2.gt.UBx) THEN
              dfx(1,1)=dfc
            ELSE IF (Finp(i1+2,j2) .LT. spv) THEN
              dfx(1,1)=dfc
            ELSE
              dfx(1,1)=Finp(i1+2,j2)-f(1,1)
              IF ((dfx(1,1)*dfc).lt.0.0) THEN
                dfx(1,1)=0.0
              ELSE IF (ABS(dfx(1,1)).gt.(LIMTR*ABS(dfc))) THEN
                dfx(1,1)=LIMTR*dfc
              END IF
            END IF
!
            dfc=f(1,0)-f(0,0)
            IF ((i1+2).gt.UBx) THEN
              dfx(1,0)=dfc
            ELSE IF (Finp(i1+2,j1) .LT. spv) THEN
              dfx(1,0)=dfc
            ELSE
              dfx(1,0)=Finp(i1+2,j1)-f(1,0)
              IF ((dfx(1,0)*dfc).lt.0.0) THEN
                dfx(1,0)=0.0
              ELSE IF (ABS(dfx(1,0)).gt.(LIMTR*ABS(dfc))) THEN
                dfx(1,0)=LIMTR*dfc
              END IF
            END IF
!
            dfc=f(1,1)-f(0,1)
            IF (i1-1.lt.1) THEN
              dfx(0,1)=dfc
            ELSE IF (Finp(i1-1,j2) .LT. spv) THEN
              dfx(0,1)=dfc
            ELSE
              dfx(0,1)=f(0,1)-Finp(i1-1,j2)
              IF ((dfx(0,1)*dfc).lt.0.0) THEN
                dfx(0,1)=0.0
              ELSE IF (ABS(dfx(0,1)).gt.(LIMTR*ABS(dfc))) THEN
                dfx(0,1)=LIMTR*dfc
              END IF
            END IF
!
            dfc=f(1,0)-f(0,0)
            IF (i1-1.lt.1) THEN
              dfx(0,0)=dfc
            ELSE IF (Finp(i1-1,j1) .LT. spv) THEN
              dfx(0,0)=dfc
            ELSE
              dfx(0,0)=f(0,0)-Finp(i1-1,j1)
              IF ((dfx(0,0)*dfc).lt.0.0) THEN
                dfx(0,0)=0.0
              ELSE IF (ABS(dfx(0,0)).gt.(LIMTR*ABS(dfc))) THEN
                dfx(0,0)=LIMTR*dfc
              END IF
            END IF
!
            dfc=f(1,1)-f(1,0)
            IF (j1+2.gt.UBy) THEN
              dfy(1,1)=dfc
            ELSE IF (Finp(i2,j1+2) .LT. spv) THEN
              dfy(1,1)=dfc
            ELSE
              dfy(1,1)=Finp(i2,j1+2)-f(1,1)
              IF ((dfy(1,1)*dfc).lt.0.0) THEN
                dfy(1,1)=0.0
              ELSEIF (ABS(dfy(1,1)).gt.(LIMTR*ABS(dfc))) THEN
                dfy(1,1)=LIMTR*dfc
              END IF
            END IF
!
            dfc=f(0,1)-f(0,0)
            IF (j1+2.gt.UBy) THEN
              dfy(0,1)=dfc
            ELSE IF (Finp(i1,j1+2) .LT. spv) THEN
              dfy(0,1)=dfc
            ELSE
              dfy(0,1)=Finp(i1,j1+2)-f(0,1)
              IF ((dfy(0,1)*dfc).lt.0.0) THEN
                dfy(0,1)=0.0
              ELSE IF (ABS(dfy(0,1)).gt.(LIMTR*ABS(dfc))) THEN
                dfy(0,1)=LIMTR*dfc
              END IF
            END IF
!
            dfc=f(1,1)-f(1,0)
            IF (j1-1.lt.1) THEN
              dfy(1,0)=dfc
            ELSE IF (Finp(i2,j1-1) .LT. spv) THEN
              dfy(1,0)=dfc
            ELSE
              dfy(1,0)=f(1,0)-Finp(i2,j1-1)
              IF ((dfy(1,0)*dfc).lt.0.0) THEN
                dfy(1,0)=0.0
              ELSE IF (ABS(dfy(1,0)).gt.(LIMTR*ABS(dfc))) THEN
                dfy(1,0)=LIMTR*dfc
              END IF
            END IF
!
            dfc=f(0,1)-f(0,0)
            IF (j1-1.lt.1) THEN
              dfy(0,0)=dfc
            ELSE IF (Finp(i1,j1-1) .LT. spv) THEN
              dfy(0,0)=dfc
            ELSE
              dfy(0,0)=f(0,0)-Finp(i1,j1-1)
              IF ((dfy(0,0)*dfc).lt.0.0) THEN
                dfy(0,0)=0.0
              ELSEIF (ABS(dfy(0,0)).gt.(LIMTR*ABS(dfc))) THEN
                dfy(0,0)=LIMTR*dfc
              END IF
            END IF
!
            f0=C05*(f(1,1)+f(1,0)+f(0,1)+f(0,0))-                       
     &         C02*(dfx(1,1)+dfx(1,0)-dfx(0,1)-dfx(0,0)+                
     &              dfy(1,1)-dfy(1,0)+dfy(0,1)-dfy(0,0))

            fx=C10*(f(1,1)-f(0,1)+f(1,0)-f(0,0))-                       
     &         C01*(dfx(1,1)+dfx(1,0)+dfx(0,1)+dfx(0,0))-               
     &         C03*(dfy(1,1)-dfy(0,1)-dfy(1,0)+dfy(0,0))

            fy=C10*(f(1,1)-f(1,0)+f(0,1)-f(0,0))-                       
     &         C01*(dfy(1,1)+dfy(0,1)+dfy(1,0)+dfy(0,0))-               
     &         C03*(dfx(1,1)-dfx(1,0)-dfx(0,1)+dfx(0,0))

            fxy=f(1,1)-f(1,0)-f(0,1)+f(0,0)

            fxx=C05*(dfx(1,1)-dfx(0,1)+dfx(1,0)-dfx(0,0))

            fyy=C05*(dfy(1,1)-dfy(1,0)+dfy(0,1)-dfy(0,0))

            fxxx=C06*(dfx(1,1)+dfx(1,0)+dfx(0,1)+dfx(0,0))-             
     &           f(1,1)+f(0,1)-f(1,0)+f(0,0)

            fyyy=C06*(dfy(1,1)+dfy(0,1)+dfy(1,0)+dfy(0,0))-             
     &           f(1,1)+f(1,0)-f(0,1)+f(0,0)

            fxxy=C06*(dfx(1,1)-dfx(0,1)-dfx(1,0)+dfx(0,0))

            fxyy=C06*(dfy(1,1)-dfy(1,0)-dfy(0,1)+dfy(0,0))
            Fout(i,j)=f0+                                               
     &                fx*xi+                                            
     &                fy*eta+                                           
     &                C06*fxx*xi*xi+                                    
     &                fxy*xi*eta+                                       
     &                C06*fyy*eta*eta+                                  
     &                C04*fxxx*xi*xi*xi+                                
     &                C06*fxxy*xi*xi*eta+                               
     &                C04*fyyy*eta*eta*eta+                             
     &                C06*fxyy*xi*eta*eta
            Fmin=MIN(Fmin,Fout(i,j))
            Fmax=MAX(Fmax,Fout(i,j))
          END IF
777       CONTINUE
        END DO
      END DO

      RETURN
      END SUBROUTINE cinterp2d

      SUBROUTINE hindices (ng, LBi, UBi, LBj, UBj,                      
     &                     Is, Ie, Js, Je,                              
     &                     angler, Xgrd, Ygrd,                          
     &                     LBm, UBm, LBn, UBn,                          
     &                     Ms, Me, Ns, Ne,                              
     &                     Xpos, Ypos, Ipos, Jpos,                      
     &                     IJspv, rectangular)
!
!=======================================================================
!                                                                      !
!  Given any geographical locations Xpos and Ypos, this routine finds  !
!  the corresponding array cell indices (Ipos, Jpos) of gridded  data  !
!  Xgrd and Ygrd containing each requested location. This indices are  !
!  usually used for interpolation.                                     !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng          Nested grid number.                                  !
!     LBi         I-dimension Lower bound of gridded data.             !
!     UBi         I-dimension Upper bound of gridded data.             !
!     LBj         J-dimension Lower bound of gridded data.             !
!     UBj         J-dimension Upper bound of gridded data.             !
!     Is          Starting gridded data I-index to search.             !
!     Ie          Ending   gridded data I-index to search.             !
!     Js          Starting gridded data J-index to search.             !
!     Je          Ending   gridded data J-index to search.             !
!     angler      Gridded data angle between X-axis and true EAST      !
!                   (radians).                                         !
!     Xgrd        Gridded data X-locations (usually, longitude).       !
!     Ygrd        Gridded data Y-locations (usually, latitude).        !
!     LBm         I-dimension Lower bound of requested locations.      !
!     UBm         I-dimension Upper bound of requested locations.      !
!     LBn         J-dimension Lower bound of requested locations.      !
!     UBn         J-dimension Upper bound of requested locations.      !
!     Ms          Starting requested locations I-index to search.      !
!     Me          Ending   requested locations I-index to search.      !
!     Ns          Starting requested locations J-index to search.      !
!     Ne          Ending   requested locations J-index to search.      !
!     Xpos        Requested X-locations to process (usually longitude).!
!     Ypos        Requested Y-locations to process (usually latitude). !
!     IJspv       Unbounded special value to assign.                   !
!     rectangular Logical switch indicating that gridded data has a    !
!                   plaid distribution.                                !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Ipos       Fractional I-cell index containing locations in data. !
!     Jpos       Fractional J-cell index containing locations in data. !
!                                                                      !
!  Calls:    Try_Range                                                 !
!                                                                      !
!=======================================================================
!
  !    USE mod_param
  !    USE mod_scalars
!
!  Imported variable declarations.
!
      implicit none
      logical, intent(in) :: rectangular
      integer, intent(in) :: ng
      integer, intent(in) :: LBi, UBi, LBj, UBj, Is, Ie, Js, Je
      integer, intent(in) :: LBm, UBm, LBn, UBn, Ms, Me, Ns, Ne

      real, intent(in) :: IJspv
!
      real, intent(in) :: angler(LBi:UBi,LBj:UBj)
      real, intent(in) :: Xgrd(LBi:UBi,LBj:UBj)
      real, intent(in) :: Ygrd(LBi:UBi,LBj:UBj)

      real, intent(in) :: Xpos(LBm:UBm,LBn:UBn)
      real, intent(in) :: Ypos(LBm:UBm,LBn:UBn)

      real, intent(out) :: Ipos(LBm:UBm,LBn:UBn)
      real, intent(out) :: Jpos(LBm:UBm,LBn:UBn)
!
!  Local variable declarations.
!
      logical :: found, foundi, foundj,try_range,inside,spherical

      integer :: Imax, Imin, Jmax, Jmin, i, i0, j, j0, mp, np

      real :: aa2, ang, bb2, diag2, dx, dy, phi
      real :: xfac, xpp, yfac, ypp
      real, parameter :: Eradius = 6371315.0   !meters
      real, parameter :: pi = 3.141592653589  
      real, parameter :: deg2rad = pi / 180.0
      spherical=.TRUE.
!
!-----------------------------------------------------------------------
!  Determine grid cell indices containing requested position points.
!  Then, interpolate to fractional cell position.
!-----------------------------------------------------------------------
!
      DO np=Ns,Ne
        DO mp=Ms,Me
          Ipos(mp,np)=IJspv
          Jpos(mp,np)=IJspv
!
!  The gridded data has a plaid distribution so the search is trivial.
!
          IF (rectangular) THEN
            foundi=.FALSE.
            I_LOOP : DO i=LBi,UBi-1
              IF ((Xgrd(i  ,1).le.Xpos(mp,np)).and.                     
     &            (Xgrd(i+1,1).gt.Xpos(mp,np))) THEN
                Imin=i
                foundi=.TRUE.
                EXIT I_LOOP
              END IF
            END DO I_LOOP
            foundj=.FALSE.
            J_LOOP : DO j=LBj,UBj-1
              IF ((Ygrd(1,j  ).le.Ypos(mp,np)).and.                     
     &            (Ygrd(1,j+1).gt.Ypos(mp,np))) THEN
                Jmin=j
                foundj=.TRUE.
                EXIT J_LOOP
              END IF
            END DO J_LOOP
            found=foundi.and.foundj
!
!  Check each position to find if it falls inside the whole domain.
!  Once it is stablished that it inside, find the exact cell to which
!  it belongs by successively dividing the domain by a half (binary
!  search).
!
          ELSE
            found=try_range(ng, LBi, UBi, LBj, UBj,                    
     &                      Xgrd, Ygrd,                                
     &                      Is, Ie, Js, Je,                            
     &                      Xpos(mp,np), Ypos(mp,np))
            IF (found) THEN
              Imin=Is
              Imax=Ie
              Jmin=Js
              Jmax=Je
              DO while (((Imax-Imin).gt.1).or.((Jmax-Jmin).gt.1))
                IF ((Imax-Imin).gt.1) THEN
                  i0=(Imin+Imax)/2
                  found=try_range(ng, LBi, UBi, LBj, UBj,              
     &                            Xgrd, Ygrd,                          
     &                            Imin, i0, Jmin, Jmax,                
     &                            Xpos(mp,np), Ypos(mp,np))
                  IF (found) THEN
                    Imax=i0
                  ELSE
                    Imin=i0
                  END IF
                END IF
                IF ((Jmax-Jmin).gt.1) THEN
                  j0=(Jmin+Jmax)/2
                  found=try_range(ng, LBi, UBi, LBj, UBj,               
     &                            Xgrd, Ygrd,                           
     &                            Imin, Imax, Jmin, j0,                 
     &                            Xpos(mp,np), Ypos(mp,np))
                  IF (found) THEN
                    Jmax=j0
                  ELSE
                    Jmin=j0
                  END IF
                END IF
              END DO
              found=(Is.le.Imin).and.(Imin.le.Ie).and.                  
     &              (Is.le.Imax).and.(Imax.le.Ie).and.                  
     &              (Js.le.Jmin).and.(Jmin.le.Je).and.                  
     &              (Js.le.Jmax).and.(Jmax.le.Je)
            END IF
          END IF
!
!  Knowing the correct cell, calculate the exact indices, accounting
!  for a possibly rotated grid.  If spherical, convert all positions
!  to meters first.
!
          IF (found) THEN
            IF (spherical) THEN
              yfac=Eradius*deg2rad
              xfac=yfac*COS(Ypos(mp,np)*deg2rad)
              xpp=(Xpos(mp,np)-Xgrd(Imin,Jmin))*xfac
              ypp=(Ypos(mp,np)-Ygrd(Imin,Jmin))*yfac
            ELSE
              xfac=1.0
              yfac=1.0
              xpp=Xpos(mp,np)-Xgrd(Imin,Jmin)
              ypp=Ypos(mp,np)-Ygrd(Imin,Jmin)
            END IF
!
!  Use Law of Cosines to get cell parallelogram "shear" angle.
!
            diag2=((Xgrd(Imin+1,Jmin)-Xgrd(Imin,Jmin+1))*xfac)**2+      
     &            ((Ygrd(Imin+1,Jmin)-Ygrd(Imin,Jmin+1))*yfac)**2
            aa2=((Xgrd(Imin,Jmin)-Xgrd(Imin+1,Jmin))*xfac)**2+          
     &          ((Ygrd(Imin,Jmin)-Ygrd(Imin+1,Jmin))*yfac)**2
            bb2=((Xgrd(Imin,Jmin)-Xgrd(Imin,Jmin+1))*xfac)**2+          
     &          ((Ygrd(Imin,Jmin)-Ygrd(Imin,Jmin+1))*yfac)**2
            phi=ASIN((diag2-aa2-bb2)/(2.0*SQRT(aa2*bb2)))
!
!  Transform float position into curvilinear coordinates. Assume the
!  cell is rectanglar, for now.
!
            ang=angler(Imin,Jmin)
            dx=xpp*COS(ang)+ypp*SIN(ang)
            dy=ypp*COS(ang)-xpp*SIN(ang)
!
!  Correct for parallelogram.
!
            dx=dx+dy*TAN(phi)
            dy=dy/COS(phi)
!
!  Scale with cell side lengths to translate into cell indices.
!
            dx=MIN(MAX(0.0,dx/SQRT(aa2)),1.0)
            dy=MIN(MAX(0.0,dy/SQRT(bb2)),1.0)
            Ipos(mp,np)=REAL(Imin)+dx
            Jpos(mp,np)=REAL(Jmin)+dy
          END IF
        END DO
      END DO

      RETURN
      END SUBROUTINE hindices

      LOGICAL FUNCTION try_range (ng,LBi,UBi,LBj,UBj,Xgrd,Ygrd,   
     &                            Imin, Imax, Jmin, Jmax, Xo, Yo)
!
!=======================================================================
!                                                                      !
!  Given a grided domain with matrix coordinates Xgrd and Ygrd, this   !
!  function finds if the point (Xo,Yo)  is inside the box defined by   !
!  the requested corners (Imin,Jmin) and (Imax,Jmax). It will return   !
!  logical switch  try_range=.TRUE.  if (Xo,Yo) is inside, otherwise   !
!  it will return false.                                               !
!                                                                      !
!  Calls:   inside                                                     !
!                                                                      !
!=======================================================================
!
   !!   USE mod_param
!
!  Imported variable declarations.
!
      implicit none
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj
      integer, intent(in) :: Imin, Imax, Jmin, Jmax

      real, intent(in) :: Xgrd(LBi:UBi,LBj:UBj)
      real, intent(in) :: Ygrd(LBi:UBi,LBj:UBj)

      real, intent(in) :: Xo, Yo
!
!  Local variable declarations.
!
      integer ::  Nb, i, j, shft, ic
      logical inside
      real, dimension(2*(Jmax-Jmin+Imax-Imin)+1) :: Xb, Yb
!
!-----------------------------------------------------------------------
!  Define closed polygon.
!-----------------------------------------------------------------------
!
!  Note that the last point (Xb(Nb),Yb(Nb)) does not repeat first
!  point (Xb(1),Yb(1)).  Instead, in function inside, it is implied
!  that the closing segment is (Xb(Nb),Yb(Nb))-->(Xb(1),Yb(1)). In
!  fact, function inside sets Xb(Nb+1)=Xb(1) and Yb(Nb+1)=Yb(1).
!
      Nb=2*(Jmax-Jmin+Imax-Imin)
      shft=1-Imin
      DO i=Imin,Imax-1
        Xb(i+shft)=Xgrd(i,Jmin)
        Yb(i+shft)=Ygrd(i,Jmin)
      END DO
      shft=1-Jmin+Imax-Imin
      DO j=Jmin,Jmax-1
        Xb(j+shft)=Xgrd(Imax,j)
        Yb(j+shft)=Ygrd(Imax,j)
      END DO
      shft=1+Jmax-Jmin+2*Imax-Imin
      DO i=Imax,Imin+1,-1
        Xb(shft-i)=Xgrd(i,Jmax)
        Yb(shft-i)=Ygrd(i,Jmax)
      END DO
      shft=1+2*Jmax-Jmin+2*(Imax-Imin)
      DO j=Jmax,Jmin+1,-1
        Xb(shft-j)=Xgrd(Imin,j)
        Yb(shft-j)=Ygrd(Imin,j)
      END DO
!
!-----------------------------------------------------------------------
!  Check if point (Xo,Yo) is inside of the defined polygon.
!-----------------------------------------------------------------------
!
      try_range=inside(Nb, Xb, Yb, Xo, Yo)
      RETURN
      END FUNCTION try_range

      LOGICAL FUNCTION inside (Nb, Xb, Yb, Xo, Yo)
!
!=======================================================================
!                                                                      !
!  Given the vectors Xb and Yb of size Nb, defining the coordinates    !
!  of a closed polygon,  this function find if the point (Xo,Yo) is    !
!  inside the polygon.  If the point  (Xo,Yo)  falls exactly on the    !
!  boundary of the polygon, it still considered inside.                !
!                                                                      !
!  This algorithm does not rely on the setting of  Xb(Nb)=Xb(1) and    !
!  Yb(Nb)=Yb(1).  Instead, it assumes that the last closing segment    !
!  is (Xb(Nb),Yb(Nb)) --> (Xb(1),Yb(1)).                               !
!                                                                      !
!  Reference:                                                          !
!                                                                      !
!    Reid, C., 1969: A long way from Euclid. Oceanography EMR,         !
!      page 174.                                                       !
!                                                                      !
!  Algorithm:                                                          !
!                                                                      !
!  The decision whether the point is  inside or outside the polygon    !
!  is done by counting the number of crossings from the ray (Xo,Yo)    !
!  to (Xo,-infinity), hereafter called meridian, by the boundary of    !
!  the polygon.  In this counting procedure,  a crossing is counted    !
!  as +2 if the crossing happens from "left to right" or -2 if from    !
!  "right to left". If the counting adds up to zero, then the point    !
!  is outside.  Otherwise,  it is either inside or on the boundary.    !
!                                                                      !
!  This routine is a modified version of the Reid (1969) algorithm,    !
!  where all crossings were counted as positive and the decision is    !
!  made  based on  whether the  number of crossings is even or odd.    !
!  This new algorithm may produce different results  in cases where    !
!  Xo accidentally coinsides with one of the (Xb(k),k=1:Nb) points.    !
!  In this case, the crossing is counted here as +1 or -1 depending    !
!  of the sign of (Xb(k+1)-Xb(k)).  Crossings  are  not  counted if    !
!  Xo=Xb(k)=Xb(k+1).  Therefore, if Xo=Xb(k0) and Yo>Yb(k0), and if    !
!  Xb(k0-1) < Xb(k0) < Xb(k0+1),  the crossing is counted twice but    !
!  with weight +1 (for segments with k=k0-1 and k=k0). Similarly if    !
!  Xb(k0-1) > Xb(k0) > Xb(k0+1), the crossing is counted twice with    !
!  weight -1 each time.  If,  on the other hand,  the meridian only    !
!  touches the boundary, that is, for example, Xb(k0-1) < Xb(k0)=Xo    !
!  and Xb(k0+1) < Xb(k0)=Xo, then the crossing is counted as +1 for    !
!  segment k=k0-1 and -1 for segment k=k0, resulting in no crossing.   !
!                                                                      !
!  Note 1: (Explanation of the logical condition)                      !
!                                                                      !
!  Suppose  that there exist two points  (x1,y1)=(Xb(k),Yb(k))  and    !
!  (x2,y2)=(Xb(k+1),Yb(k+1)),  such that,  either (x1 < Xo < x2) or    !
!  (x1 > Xo > x2).  Therefore, meridian x=Xo intersects the segment    !
!  (x1,y1) -> (x2,x2) and the ordinate of the point of intersection    !
!  is:                                                                 !
!                                                                      !
!                 y1*(x2-Xo) + y2*(Xo-x1)                              !
!             y = -----------------------                              !
!                          x2-x1                                       !
!                                                                      !
!  The mathematical statement that point  (Xo,Yo)  either coinsides    !
!  with the point of intersection or lies to the north (Yo>=y) from    !
!  it is, therefore, equivalent to the statement:                      !
!                                                                      !
!         Yo*(x2-x1) >= y1*(x2-Xo) + y2*(Xo-x1),   if   x2-x1 > 0      !
!  or                                                                  !
!         Yo*(x2-x1) <= y1*(x2-Xo) + y2*(Xo-x1),   if   x2-x1 < 0      !
!                                                                      !
!  which, after noting that  Yo*(x2-x1) = Yo*(x2-Xo + Xo-x1) may be    !
!  rewritten as:                                                       !
!                                                                      !
!        (Yo-y1)*(x2-Xo) + (Yo-y2)*(Xo-x1) >= 0,   if   x2-x1 > 0      !
!  or                                                                  !
!        (Yo-y1)*(x2-Xo) + (Yo-y2)*(Xo-x1) <= 0,   if   x2-x1 < 0      !
!                                                                      !
!  and both versions can be merged into  essentially  the condition    !
!  that (Yo-y1)*(x2-Xo)+(Yo-y2)*(Xo-x1) has the same sign as x2-x1.    !
!  That is, the product of these two must be positive or zero.         !
!                                                                      !
!=======================================================================
!
!  Imported variable declarations.
!
      integer, intent(in) :: Nb

      real, intent(in) :: Xo, Yo

!      real, intent(inout) :: Xb(:), Yb(:)  ! this statement does not work 
      real, intent(inout) :: Xb(Nb+1), Yb(Nb+1)
!
!  Local variable declarations.
!
      integer, parameter :: Nstep =128

      integer :: crossings, i, inc, k, kk, nc

      integer, dimension(Nstep) :: Sindex

      real :: dx1, dx2, dxy
!
!-----------------------------------------------------------------------
!  Find intersections.
!-----------------------------------------------------------------------
!
!  Set crossings counter and close the contour of the polygon.
!
      crossings=0
      Xb(Nb+1)=Xb(1)
      Yb(Nb+1)=Yb(1)
!
!  The search is optimized.  First select the indices of segments
!  where Xb(k) is different from Xb(k+1) and Xo falls between them.
!  Then, further investigate these segments in a separate loop.
!  Doing it in two stages takes less time because the first loop is
!  pipelined.
!
      DO kk=0,Nb-1,Nstep
        nc=0
        DO k=kk+1,MIN(kk+Nstep,Nb)
          IF (((Xb(k+1)-Xo)*(Xo-Xb(k)).ge.0.0).and.                  
     &        (Xb(k).ne.Xb(k+1))) THEN
            nc=nc+1
            Sindex(nc)=k
          END IF
        END DO
        DO i=1,nc
          k=Sindex(i)
          IF (Xb(k).ne.Xb(k+1)) THEN
            dx1=Xo-Xb(k)
            dx2=Xb(k+1)-Xo
            dxy=dx2*(Yo-Yb(k))-dx1*(Yb(k+1)-Yo)
            inc=0
            IF ((Xb(k).eq.Xo).and.(Yb(k).eq.Yo)) THEN
              crossings=1
              goto 10
            ELSE IF (((dx1.eq.0.0).and.(Yo.ge.Yb(k  ))).or.          
     &              ((dx2.eq.0.0).and.(Yo.ge.Yb(k+1)))) THEN
              inc=1
            ELSE IF ((dx1*dx2.gt.0.0).and.                           
     &              ((Xb(k+1)-Xb(k))*dxy.ge.0.0)) THEN  ! see note 1
              inc=2
            END IF
            IF (Xb(k+1).gt.Xb(k)) THEN
              crossings=crossings+inc
            ELSE
              crossings=crossings-inc
            END IF
          END IF
        END DO
      END DO
!
!  Determine if point (Xo,Yo) is inside of closed polygon.
!
  10  IF (crossings.eq.0) THEN
        inside=.FALSE.
      ELSE
        inside=.TRUE.
      END IF
      RETURN
      END FUNCTION inside
