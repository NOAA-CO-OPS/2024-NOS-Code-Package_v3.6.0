module elfe_glbl
implicit none
public
#ifdef USE_SINGLE
integer,parameter :: rkind = 4
#else
integer,parameter :: rkind = 8
#endif
type :: llist_type
integer :: rank
integer :: id=0
type(llist_type),pointer :: next=>null()
end type llist_type
type :: bt_type
integer :: rank
integer :: l0
integer :: i0gb
integer :: isbndy
integer :: j0
integer :: adv
integer :: iegb
integer :: jvrt
real(rkind) :: dtbk
real(rkind) :: vis
real(rkind) :: rt
real(rkind) :: rt2
real(rkind) :: ut,vt,wt
real(rkind) :: xt,yt,zt
real(rkind) :: sclr(4)
real(rkind) :: gcor0(3)
real(rkind) :: frame0(3,3)
end type bt_type
integer,save :: bt_mpitype
real(rkind),save :: s1_mxnbt
real(rkind),save :: s2_mxnbt
integer,save :: mxnbt
integer,save :: ivcor
integer,save :: nvrt
integer,save :: kz
integer,save :: nsig
real(rkind),save :: h_s,h_c,theta_b,theta_f,s_con1
real(rkind),save,allocatable :: ztot(:)
real(rkind),save,allocatable :: sigma(:)
real(rkind),save,allocatable :: cs(:)
real(rkind),save,allocatable :: dcs(:)
integer,save :: ne_global
integer,save :: ne
integer,save :: neg
integer,save :: nea
integer,save,allocatable :: ielg(:)
type(llist_type),save,pointer :: iegl(:)
integer,save,allocatable :: iegrpv(:)
integer,save :: nx(3,2)
integer,save,allocatable :: nm(:,:)
integer,save,allocatable :: nmgb(:,:)
integer,save,allocatable :: iself(:,:)
integer,save,allocatable :: ic3(:,:)
integer,save,allocatable :: ic3gb(:,:)
integer,save,allocatable :: js(:,:)
real(rkind),save,allocatable :: ssign(:,:)
real(rkind),save,allocatable :: area(:)
real(rkind),save,allocatable :: radiel(:)
real(rkind),save,allocatable :: xctr(:),yctr(:),zctr(:)
real(rkind),save,allocatable :: xlon_el(:),ylat_el(:)
real(rkind),save,allocatable :: dpe(:)
integer,save,allocatable :: kbe(:)
integer,save,allocatable :: idry_e(:),idry_e0(:)
integer,save,allocatable :: interpol(:)
integer,save,allocatable :: lqk(:)
integer,save,allocatable :: ie_kr(:)
integer,save,allocatable :: krvel(:)
real(rkind),save,allocatable :: ze(:,:)
real(rkind),save,allocatable :: dl(:,:,:)
real(rkind),save,allocatable :: eframe(:,:,:)
real(rkind),save,allocatable :: xel(:,:),yel(:,:)
integer,save :: np_global
integer,save :: np
integer,save :: npg
integer,save :: npa
integer,save,allocatable :: iplg(:)
type(llist_type),save,pointer :: ipgl(:)
real(rkind),save,allocatable :: xnd(:),ynd(:),znd(:)
real(rkind),save,allocatable :: xlon(:),ylat(:)
real(rkind),save,allocatable :: dp(:),dp00(:)
integer,save,allocatable :: nnegb(:),inegb(:,:)
integer,save,allocatable :: nne(:),ine(:,:)
integer,save,allocatable :: nnp(:),inp(:,:)
integer,save,allocatable :: isbnd(:,:)
integer,save,allocatable :: ibnd_ext_int(:)
real(rkind),save,allocatable :: edge_angle(:,:)
integer,save,allocatable :: isbnd_global(:)
integer,save,allocatable :: kfp(:),kbp(:),kbp00(:),kbp_e(:)
integer,save,allocatable :: idry(:)
integer,save,allocatable :: iback(:)
real(rkind),save,allocatable :: hmod(:)
real(rkind),save,allocatable :: znl(:,:)
real(rkind),save,allocatable :: pframe(:,:,:)
integer,save :: ns_global
integer,save :: ns
integer,save :: nsg
integer,save :: nsa
integer,save,allocatable :: islg(:)
type(llist_type),save,pointer :: isgl(:)
integer,save,allocatable :: is(:,:)
integer,save,allocatable :: isidenode(:,:)
real(rkind),save,allocatable :: xcj(:),ycj(:),zcj(:)
real(rkind),save,allocatable :: dps(:)
real(rkind),save,allocatable :: distj(:)
real(rkind),save,allocatable :: delj(:)
integer,save,allocatable :: isbs(:)
integer,save,allocatable :: isbs_global(:)
integer,save,allocatable :: kbs(:)
integer,save,allocatable :: idry_s(:)
integer,save,allocatable :: isidenei(:,:),isidenei2(:,:)
real(rkind),save,allocatable :: zs(:,:)
real(rkind),save,allocatable :: side_ac(:,:,:)
real(rkind),save,allocatable :: side_x(:,:)
real(rkind),save,allocatable :: sframe(:,:,:)
integer,save :: nope_global
integer,save :: neta_global
integer,save :: nope
integer,save :: neta
integer,save :: mnond
integer,save :: mnond_global
integer,save,allocatable :: iopelg(:)
integer,save,allocatable :: iopegl(:,:)
integer,save,allocatable :: nond(:)
integer,save,allocatable :: iond(:,:)
integer,save,allocatable :: nond_global(:)
integer,save,allocatable :: iond_global(:,:)
real(rkind),save,allocatable :: cwidth(:)
real(rkind),save,allocatable :: tth(:,:,:),sth(:,:,:),trth(:,:)
integer,save :: nland_global
integer,save :: nvel_global
integer,save :: nland
integer,save :: nvel
integer,save :: mnlnd
integer,save :: mnlnd_global
integer,save,allocatable :: nlnd_global(:)
integer,save,allocatable :: ilnd_global(:,:)
integer,save,allocatable :: nlnd(:)
integer,save,allocatable :: ilnd(:,:)
real(rkind),save,allocatable :: tsel(:,:,:)
real(rkind),save,allocatable :: tr_el(:,:,:)
real(rkind),save,allocatable :: bdy_frc(:,:,:)
real(rkind),save,allocatable :: flx_sf(:,:)
real(rkind),save,allocatable :: flx_bt(:,:)
real(rkind),save,allocatable :: hdif(:,:)
real(rkind),save,allocatable :: tem0(:,:)
real(rkind),save,allocatable :: sal0(:,:)
real(rkind),save,allocatable :: trel0(:,:,:)
real(rkind),save,allocatable :: eta1(:)
real(rkind),save,allocatable :: eta2(:)
real(rkind),save,allocatable :: we(:,:)
real(rkind),save,allocatable :: we_fv(:,:)
real(rkind),save,allocatable :: su2(:,:),sv2(:,:)
real(rkind),save,allocatable :: ufg(:,:,:),vfg(:,:,:)
real(rkind),save,allocatable :: tsd(:,:)
real(rkind),save,allocatable :: ssd(:,:)
real(rkind),save,allocatable :: tnd(:,:)
real(rkind),save,allocatable :: snd(:,:)
real(rkind),save,allocatable :: prho(:,:)
real(rkind),save,allocatable :: q2(:,:)
real(rkind),save,allocatable :: xl(:,:)
real(rkind),save,allocatable :: xlmin2(:)
real(rkind),save,allocatable :: uu2(:,:),vv2(:,:),ww2(:,:)
real(rkind),save,allocatable :: bdef(:)
real(rkind),save,allocatable :: bdef1(:)
real(rkind),save,allocatable :: bdef2(:)
real(rkind),save,allocatable :: dfh(:,:)
real(rkind),save,allocatable :: dfv(:,:)
integer,save,allocatable :: itier_nd(:,:)
real(rkind),save,allocatable :: akrmat_nd(:,:,:)
real(rkind),save,allocatable :: albedo(:)
real(rkind),save,allocatable :: z_r(:)
real(rkind),save,allocatable :: tem1(:)
real(rkind),save,allocatable :: sal1(:)
real(rkind),save,allocatable :: cspline_ypp(:,:)
real(rkind),save,allocatable :: cspline_ypp_nd(:,:,:)
real(rkind),save,allocatable :: cspline_ypp_sd(:,:,:)
real(rkind),save,allocatable :: rho_mean(:,:)
real(rkind),save,allocatable :: Cdp(:)
real(rkind),save,allocatable :: windx(:),windy(:)
# ifdef USE_WWM
real(rkind),save,allocatable :: out_wwm(:,:),wwave_force(:,:,:)
# endif
# ifdef USE_NAPZD
real(rkind),save,allocatable :: Bio_bdef(:,:)
# endif
# ifdef USE_HA
integer :: MNHARF
logical CHARMV
# endif
integer, parameter :: nbyte=4
integer, parameter :: mnout=150
integer, parameter :: mirec=1109000000
integer,save :: iwrite
character(len=48),save :: start_time,version,data_format='DataFormat v5.0'
character(len=12),save :: ifile_char
character(len=48),save,dimension(mnout) :: outfile,variable_nm,variable_dim
integer,save :: ics,ihot,ihfskip,nrec,nspool,igmp,noutgm,ifile,ifile_len,noutput,ifort12(100)
integer,save,dimension(mnout) :: ichan,irec,iof
character(len=48),save :: a_48
character(len=16),save :: a_16
character(len= 8),save :: a_8
character(len= 4),save :: a_4
character(len=1000),save :: errmsg
character(len=2),save :: mid,stab
real(rkind),save :: ubd0,ubd1,ubd2,ubd3,ubd4,ubd5, &
ubs0,ubs1,ubs2,ubs4,ubs5,ubs6, &
a2_cm03,schk,schpsi
integer,save :: nonhydro,iupwind_t,nz_r,ieqstate,imm,kr_co,indvel,ihconsv,isconsv,ihdif, &
&ntracers,mntr,ntracers2
integer,save :: mnei
integer,save :: mnei_kr
real(rkind),save :: h0,q2min,dt,tempmin,tempmax,saltmin,saltmax,vis_coe1,vis_coe2,h_bcc1, &
&velmin_btrack,rmaxvel
logical,save :: lm2d
real(rkind),parameter :: small1=1.e-6
real(rkind),parameter :: small2=small1*100
real(rkind),parameter :: pi=3.141592653589793
real(rkind),parameter :: grav=9.81
real(rkind),parameter :: rho0=1000.
real(rkind),parameter :: rearth=6378206.4
real(rkind),parameter :: omega_e=7.292e-5
integer,parameter :: mxtimer=20
real(rkind),save :: wtimer(0:mxtimer,2)
character(72) :: fdb
integer :: lfdb
contains
subroutine release_gl(n,gl_ll)
implicit none
integer,intent(in) :: n
type(llist_type),pointer :: gl_ll(:)
integer i
do i=1,n
call release_llist(gl_ll(i)%next)
enddo
deallocate(gl_ll)
nullify(gl_ll)
end subroutine release_gl
recursive subroutine release_llist(llentry)
implicit none
type(llist_type),pointer :: llentry
if(associated(llentry)) then
call release_llist(llentry%next)
deallocate(llentry)
nullify(llentry)
endif
end subroutine release_llist
end module elfe_glbl
