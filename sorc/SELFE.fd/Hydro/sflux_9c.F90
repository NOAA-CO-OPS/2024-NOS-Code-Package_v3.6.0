subroutine surf_fluxes (time, u_air, v_air, p_air, &
& t_air, q_air, shortwave_d, &
& sen_flux, lat_flux, longwave_u, longwave_d, &
& tau_xz, tau_yz, &
#ifdef PREC_EVAP
& precip_flux, evap_flux, &
#endif
& nws, fluxsu00, srad00)
use elfe_glbl, only : rkind, npa, uu2, vv2, tnd, snd, &
& kfp, idry, nvrt, ivcor,ipgl,fdb,lfdb
use elfe_msgp, only : myrank,parallel_abort
implicit none
real(rkind), intent(in) :: time, fluxsu00, srad00
real(rkind), dimension(npa), intent(in) :: &
& u_air, v_air, p_air, t_air, q_air
real(rkind), dimension(npa), intent(out) :: &
& shortwave_d, sen_flux, lat_flux, longwave_u, longwave_d, &
& tau_xz, tau_yz
integer, intent(in) :: nws
#ifdef PREC_EVAP
real(rkind), dimension(npa), intent(out) :: &
& precip_flux, evap_flux
#endif
integer num_nodes, i_node, sfc_lev,ne_global,np_global,itmp
logical dry
real(rkind), parameter :: t_freeze = 273.15
real(rkind), parameter :: stefan = 5.67e-8
real(rkind), parameter :: emissivity = 1.0
integer, parameter :: printit = 1000
character, parameter :: grid_file*50 = 'sflux.gr3'
real(rkind) :: x_tmp, y_tmp, sflux_frac
integer i_node_tmp
logical, save :: first_call = .true.
num_nodes = npa
fdb='sflux1_0000'
lfdb=len_trim(fdb)
write(fdb(lfdb-3:lfdb),'(i4.4)') myrank
open(38,file='outputs/'//fdb,status='unknown')
rewind(38)
write(38,*)
write(38,*) 'enter surf_fluxes'
call get_rad (time, shortwave_d, longwave_d)
#ifdef PREC_EVAP
call get_precip_flux (time, precip_flux)
#endif
write(38,*)
write(38,*) 'surf_fluxes: time = ', time
write(38,*) 'first_call = ', first_call
write(38,*) 'num_nodes = ', num_nodes
do i_node = 1, num_nodes
if (ivcor .eq. -1) then
sfc_lev = kfp(i_node)
else
sfc_lev = nvrt
endif
if (mod(i_node-1,printit) .eq. 0) then
write(38,*)
write(38,*) 'i_node, sfc u, v, T = ', i_node, &
#ifndef SELFE
& uu2(i_node, sfc_lev), &
& vv2(i_node, sfc_lev), &
& tnd(i_node, sfc_lev)
#else /* SELFE */
& uu2(sfc_lev,i_node), &
& vv2(sfc_lev,i_node), &
& tnd(sfc_lev,i_node)
#endif /* SELFE */
write(38,*) 'u, v, p, T, q (air) = ', u_air(i_node), &
& v_air(i_node), p_air(i_node), t_air(i_node), &
& q_air(i_node)
endif
enddo
write(38,*) 'above turb_fluxes'
call turb_fluxes (num_nodes, &
& u_air, v_air, p_air, t_air, q_air, &
& sen_flux, lat_flux, &
#ifdef PREC_EVAP
& evap_flux, &
#endif
& tau_xz, tau_yz)
write(38,*) 'below turb_fluxes'
write(38,*) 'calculating longwave_u'
do i_node = 1, num_nodes
if (ivcor .eq. -1) then
sfc_lev = kfp(i_node)
else
sfc_lev = nvrt
endif
longwave_u(i_node) = emissivity * stefan * &
#ifndef SELFE
& ( t_freeze + tnd(i_node, sfc_lev) ) ** 4
#else /* SELFE */
& ( t_freeze + tnd(sfc_lev,i_node) ) ** 4
#endif /* SELFE */
enddo
if (nws .eq. 3) then
open (96, file=grid_file, status='old')
read(96,*)
read(96,*) ne_global,np_global
do i_node = 1, np_global
read(96,*) i_node_tmp, x_tmp, y_tmp, sflux_frac
if(ipgl(i_node)%rank==myrank) then
itmp=ipgl(i_node)%id
sen_flux(itmp) = sflux_frac * fluxsu00
shortwave_d(itmp) = sflux_frac * srad00
lat_flux(itmp) = 0.0
longwave_u(itmp) = 0.0
longwave_d(itmp) = 0.0
#ifdef PREC_EVAP
precip_flux(itmp) = 0.0
evap_flux(itmp) = 0.0
#endif
endif
enddo
close(96)
endif
do i_node = 1, num_nodes
if (mod(i_node-1,printit) .eq. 0) then
dry = &
& ( (ivcor .eq. -1) .and. (kfp(i_node) .eq. 0) ) &
& .or. &
& ( (ivcor .ne. -1) .and. (idry(i_node) .eq. 1) )
if (.not. dry) then
write(38,*)
write(38,*) 'i_node = ', i_node
write(38,*) 'net_sfc_flux_d = ', &
& - sen_flux(i_node) - lat_flux(i_node) &
& - ( longwave_u(i_node) - longwave_d(i_node) )
write(38,*) 'shortwave_d = ', shortwave_d(i_node)
write(38,*) 'longwave_d, longwave_u = ', &
& longwave_d(i_node), longwave_u(i_node)
write(38,*) 'sen_flux, lat_flux = ', &
& sen_flux(i_node), lat_flux(i_node)
#ifdef PREC_EVAP
write(38,*) 'precip_flux, evap_flux = ', &
& precip_flux(i_node), evap_flux(i_node)
#endif
else
write(38,*)
write(38,*) 'i_node = ', i_node
write(38,*) 'dry!'
endif
endif
enddo
close(38)
first_call = .false.
return
end
subroutine turb_fluxes (num_nodes, &
& u_air, v_air, p_air, t_air, q_air, &
& sen_flux, lat_flux, &
#ifdef PREC_EVAP
& evap_flux, &
#endif
& tau_xz, tau_yz)
use elfe_glbl, only : rkind, uu2, vv2, tnd, snd, &
& kfp, idry, nvrt, ivcor,errmsg
use elfe_msgp, only : myrank,parallel_abort
implicit none
integer, intent(in) :: num_nodes
real(rkind), dimension(num_nodes), intent(in) :: &
& u_air, v_air, p_air, t_air, q_air
real(rkind), dimension(num_nodes), intent(out) :: &
& sen_flux, lat_flux, tau_xz, tau_yz
#ifdef PREC_EVAP
real(rkind), dimension(num_nodes), intent(out) :: &
& evap_flux
#endif
integer i_node, iter, sfc_lev
integer, parameter :: max_iter = 10
real(rkind) u_star, theta_star, q_star, z_0, monin
real(rkind) zeta_u, zeta_t, one_third, w_star, mix_ratio
real(rkind) t_v, speed, psi_m, psi_h
real(rkind) re, z_0_t, e_sfc, q_sfc, rho_air, rb
real(rkind) theta_air, theta_v_air, delta_theta, delta_q
real(rkind) delta_theta_v, theta_v_star, speed_res, tau
real(rkind) speed_air, speed_water, esat_flat_r
real(rkind), parameter :: speed_air_warn = 50.0
real(rkind), parameter :: speed_air_stop = 100.0
real(rkind), parameter :: speed_water_warn = 5.0
real(rkind), parameter :: speed_water_stop = 20.0
real(rkind), parameter :: z_t = 2.0
real(rkind), parameter :: z_u = 10.0
real(rkind), parameter :: a1 = 0.013
real(rkind), parameter :: a2 = 0.11
real(rkind), parameter :: b1 = 2.67
real(rkind), parameter :: b2 = -2.57
real(rkind), parameter :: nu = 1.46e-5
real(rkind), parameter :: beta = 1.0
real(rkind), parameter :: g = 9.81
real(rkind), parameter :: z_i = 1000.0
real(rkind), parameter :: karman = 0.4
real(rkind), parameter :: zeta_m = -1.574
real(rkind), parameter :: zeta_h = -0.465
real(rkind), parameter :: t_freeze = 273.15
real(rkind), parameter :: epsilon_r = 0.6220
real(rkind), parameter :: c_p_air = 1004.0
real(rkind), parameter :: latent = 2.501e6
real(rkind), parameter :: r_air = 287.0
integer, parameter :: printit = 1000
logical converged, dry
write(38,*) 'enter turb_fluxes'
one_third = 1.0 / 3.0
do i_node = 1, num_nodes
if (mod(i_node-1,printit) .eq. 0) then
write(38,*)
write(38,*) 'i_node = ', i_node
endif
dry = &
& ( (ivcor .eq. -1) .and. (kfp(i_node) .eq. 0) ) &
& .or. &
& ( (ivcor .ne. -1) .and. (idry(i_node) .eq. 1) )
if (.not. dry) then
if (ivcor .eq. -1) then
sfc_lev = kfp(i_node)
else
sfc_lev = nvrt
endif
#ifndef SELFE
e_sfc = (1.0 - 0.000537 * snd(i_node, sfc_lev)) &
& * esat_flat_r(tnd(i_node, sfc_lev) + t_freeze)
#else /* SELFE */
e_sfc = (1.0 - 0.000537 * snd(sfc_lev,i_node)) &
& * esat_flat_r(tnd(sfc_lev,i_node) + t_freeze)
#endif /* SELFE */
q_sfc = epsilon_r * e_sfc &
& / ( p_air(i_node) - e_sfc * (1.0 - epsilon_r) )
mix_ratio = q_air(i_node) / (1.0 - q_air(i_node))
theta_air = (t_air(i_node) + t_freeze) + 0.0098*z_t
theta_v_air = theta_air * (1.0 + 0.608 * mix_ratio)
delta_theta = theta_air - &
#ifndef SELFE
& (tnd(i_node, sfc_lev) + t_freeze)
#else /* SELFE */
& (tnd(sfc_lev,i_node) + t_freeze)
#endif /* SELFE */
delta_q = q_air(i_node) - q_sfc
delta_theta_v = delta_theta * (1.0 + 0.608 * mix_ratio) &
& + 0.608 * theta_air * delta_q
t_v = (t_air(i_node) + t_freeze) * (1.0 + 0.608 * mix_ratio)
rho_air = p_air(i_node) / (r_air * t_v)
if (mod(i_node-1,printit) .eq. 0) then
write(38,*) 'e_sfc, q_sfc, mix_ratio = ', &
& e_sfc, q_sfc, mix_ratio
write(38,*) 'theta_air, theta_v_air, delta_theta = ', &
& theta_air, theta_v_air, delta_theta
write(38,*) 'delta_q, delta_theta_v, t_v = ', &
& delta_q, delta_theta_v, t_v
write(38,*) 'rho_air = ', rho_air
endif
speed_air = sqrt( u_air(i_node)*u_air(i_node) + &
& v_air(i_node)*v_air(i_node) )
speed_water &
#ifndef SELFE
& = sqrt( uu2(i_node, sfc_lev)*uu2(i_node, sfc_lev) + &
& vv2(i_node, sfc_lev)*vv2(i_node, sfc_lev) )
#else /* SELFE */
& = sqrt( uu2(sfc_lev,i_node)*uu2(sfc_lev,i_node) + &
& vv2(sfc_lev,i_node)*vv2(sfc_lev,i_node) )
#endif /* SELFE */
if (speed_air .gt. speed_air_stop) then
write(errmsg,*) 'speed_air exceeds ', speed_air_stop
call parallel_abort(errmsg)
else if (speed_air .gt. speed_air_warn) then
write(12,*)
write(12,*) 'speed_air exceeds ', speed_air_warn
endif
if (speed_water .gt. speed_water_stop) then
write(errmsg,*) 'speed_water exceeds ', speed_water_stop,i_node,uu2(sfc_lev,i_node),vv2(sfc_lev,i_node)
call parallel_abort(errmsg)
else if (speed_water .gt. speed_water_warn) then
write(12,*)
write(12,*) 'speed_water exceeds ', speed_water_warn,i_node
endif
u_star = 0.06
w_star = 0.5
if (delta_theta_v .ge. 0) then
speed = &
& max( sqrt( &
#ifndef SELFE
& (u_air(i_node) - uu2(i_node, sfc_lev))**2 + &
& (v_air(i_node) - vv2(i_node, sfc_lev))**2 ), &
#else /* SELFE */
& (u_air(i_node) - uu2(sfc_lev,i_node))**2 + &
& (v_air(i_node) - vv2(sfc_lev,i_node))**2 ), &
#endif /* SELFE */
& 0.1_rkind)
else
speed = &
#ifndef SELFE
& sqrt( (u_air(i_node) - uu2(i_node, sfc_lev))**2 + &
& (v_air(i_node) - vv2(i_node, sfc_lev))**2 + &
#else /* SELFE */
& sqrt( (u_air(i_node) - uu2(sfc_lev,i_node))**2 + &
& (v_air(i_node) - vv2(sfc_lev,i_node))**2 + &
#endif /* SELFE */
& (beta * w_star)**2 )
endif
do iter = 1, 5
z_0 = a1 * u_star * u_star / g + a2 * nu / u_star
u_star = karman * speed / log(z_u/z_0)
enddo
rb = g * z_u * delta_theta_v / (theta_v_air * speed * speed)
if (rb .ge. 0) then
zeta_u = rb * log(z_u/z_0) &
& / (1.0 - 0.5*min(rb,0.19_rkind))
else
zeta_u = rb * log(z_u/z_0)
endif
monin = z_u / zeta_u
zeta_t = z_t / monin
if (mod(i_node-1,printit) .eq. 0) then
write(38,*) 'speed, z_0, u_star = ', &
& speed, z_0, u_star
write(38,*) 'zeta_u, zeta_t, monin = ', &
& zeta_u, zeta_t, monin
endif
iter = 0
converged = .false.
100 continue
iter = iter + 1
z_0 = a1 * u_star * u_star / g + a2 * nu / u_star
re = u_star * z_0 / nu
z_0_t = z_0 / exp(b1 * (re**0.25) + b2)
if (mod(i_node-1,printit) .eq. 0) then
write(38,*) 're, z_0, z_0_t = ', &
& re, z_0, z_0_t
endif
zeta_u = z_u / monin
zeta_t = z_t / monin
if (zeta_t .gt. 2.5) then
converged = .true.
zeta_t = 2.5
monin = z_t / zeta_t
zeta_u = z_u / monin
if (mod(i_node-1,printit) .eq. 0) then
write(38,*) 'limiting zeta_u, zeta_t, monin!'
endif
endif
if (zeta_u .lt. zeta_m) then
u_star = speed * karman &
& / ( log(zeta_m*monin/z_0) &
& - psi_m(zeta_m) &
& + psi_m(z_0/monin) &
& + 1.14 * ((-zeta_u)**(one_third) - &
& (-zeta_m)**(one_third)) )
else if (zeta_u .lt. 0.0) then
u_star = speed * karman &
& / ( log(z_u/z_0) &
& - psi_m(zeta_u) &
& + psi_m(z_0/monin) &
& )
else if (zeta_u .le. 1.0) then
u_star = speed * karman &
& / ( log(z_u/z_0) + 5.0*zeta_u &
& - 5.0*z_0/monin &
& )
else
u_star = speed * karman &
& / ( log(monin/z_0) + 5.0 &
& + 5.0*log(zeta_u) &
& - 5.0*z_0/monin &
& + zeta_u - 1.0 )
endif
if (zeta_t .lt. zeta_h) then
theta_star = karman * delta_theta &
& / ( log(zeta_h*monin/z_0_t) &
& - psi_h(zeta_h) &
& + psi_h(z_0_t/monin) &
& + 0.8 * ((-zeta_h)**(-one_third) - &
& (-zeta_t)**(-one_third)) )
q_star = karman * delta_q &
& / ( log(zeta_h*monin/z_0_t) &
& - psi_h(zeta_h) &
& + psi_h(z_0_t/monin) &
& + 0.8 * ((-zeta_h)**(-one_third) - &
& (-zeta_t)**(-one_third)) )
else if (zeta_t .lt. 0.0) then
theta_star = karman * delta_theta &
& / ( log(z_t/z_0_t) &
& - psi_h(zeta_t) &
& + psi_h(z_0_t/monin) &
& )
q_star = karman * delta_q &
& / ( log(z_t/z_0_t) &
& - psi_h(zeta_t) &
& + psi_h(z_0_t/monin) &
& )
else if (zeta_t .lt. 1.0) then
theta_star = karman * delta_theta &
& / ( log(z_t/z_0_t) &
& + 5.0*zeta_t &
& - 5.0*z_0_t/monin &
& )
q_star = karman * delta_q &
& / ( log(z_t/z_0_t) &
& + 5.0*zeta_t &
& - 5.0*z_0_t/monin &
& )
else
theta_star = karman * delta_theta &
& / ( log(monin/z_0_t) + 5.0 &
& + 5.0*log(zeta_t) &
& - 5.0*z_0_t/monin &
& + zeta_t - 1.0 )
q_star = karman * delta_q &
& / ( log(monin/z_0_t) + 5.0 &
& + 5.0*log(zeta_t) &
& - 5.0*z_0_t/monin &
& + zeta_t - 1.0 )
endif
theta_v_star = theta_star * (1.0 + 0.608 * mix_ratio) &
& + 0.608 * theta_air * q_star
monin = theta_v_air * u_star * u_star &
& / (karman * g * theta_v_star)
if (delta_theta_v .ge. 0.0) then
speed = &
& max( sqrt( &
#ifndef SELFE
& (u_air(i_node) - uu2(i_node, sfc_lev))**2 + &
& (v_air(i_node) - vv2(i_node, sfc_lev))**2 ), &
#else /* SELFE */
& (u_air(i_node) - uu2(sfc_lev,i_node))**2 + &
& (v_air(i_node) - vv2(sfc_lev,i_node))**2 ), &
#endif /* SELFE */
& 0.1_rkind)
else
w_star = (-g * theta_v_star * u_star * z_i / theta_v_air) &
& ** one_third
speed = &
#ifndef SELFE
& sqrt( (u_air(i_node) - uu2(i_node, sfc_lev))**2 + &
& (v_air(i_node) - vv2(i_node, sfc_lev))**2 + &
#else /* SELFE */
& sqrt( (u_air(i_node) - uu2(sfc_lev,i_node))**2 + &
& (v_air(i_node) - vv2(sfc_lev,i_node))**2 + &
#endif /* SELFE */
& (beta * w_star)**2 )
endif
if (mod(i_node-1,printit) .eq. 0) then
write(38,*) 'iter, u_star, q_star, theta_star = ', &
& iter, u_star, q_star, theta_star
write(38,*) 'iter, theta_v_star, monin, speed = ', &
& iter, theta_v_star, monin, speed
write(38,*) 'iter, zeta_u, zeta_t = ', &
& iter, zeta_u, zeta_t
endif
if (.not. converged .and. iter .lt. max_iter) goto 100
sen_flux(i_node) = - rho_air * c_p_air * u_star * theta_star
lat_flux(i_node) = - rho_air * latent * u_star * q_star
#ifdef PREC_EVAP
evap_flux(i_node) = - rho_air * u_star * q_star
#endif
speed_res = &
#ifndef SELFE
& sqrt( (u_air(i_node) - uu2(i_node, sfc_lev))**2 + &
& (v_air(i_node) - vv2(i_node, sfc_lev))**2 )
#else /* SELFE */
& sqrt( (u_air(i_node) - uu2(sfc_lev,i_node))**2 + &
& (v_air(i_node) - vv2(sfc_lev,i_node))**2 )
#endif /* SELFE */
if (speed_res .gt. 0.0) then
tau = rho_air * u_star * u_star * speed_res / speed
tau_xz(i_node) = - tau &
#ifndef SELFE
& * (u_air(i_node) - uu2(i_node, sfc_lev)) &
#else /* SELFE */
& * (u_air(i_node) - uu2(sfc_lev,i_node)) &
#endif /* SELFE */
& / speed_res
tau_yz(i_node) = - tau &
#ifndef SELFE
& * (v_air(i_node) - vv2(i_node, sfc_lev)) &
#else /* SELFE */
& * (v_air(i_node) - vv2(sfc_lev,i_node)) &
#endif /* SELFE */
& / speed_res
else
tau_xz(i_node) = 0.0
tau_yz(i_node) = 0.0
endif
if (mod(i_node-1,printit) .eq. 0) then
write(38,*) 'sen_flux, lat_flux = ', &
& sen_flux(i_node), lat_flux(i_node)
write(38,*) 'tau_xz, tau_yz = ', &
& tau_xz(i_node), tau_yz(i_node)
endif
endif
enddo
write(38,*) 'exit turb_fluxes'
return
end
function esat_flat_r(t)
use elfe_glbl, only : rkind
implicit none
real(rkind) :: esat_flat_r
real(rkind), intent(in) :: t
real(rkind) :: t_eff
real(rkind), parameter :: &
& c0= 6.11583699e+02, c1= 0.444606896e+02, &
& c2= 0.143177157e+01, c3= 0.264224321e-01, &
& c4= 0.299291081e-03, c5= 0.203154182e-05, &
& c6= 0.702620698e-08, c7= 0.379534310e-11, &
& c8=-0.321582393e-13
t_eff = max(-85._rkind,t-273.16)
esat_flat_r = c0+t_eff*(c1+t_eff*(c2+t_eff*(c3+t_eff*(c4+t_eff*&
& (c5+t_eff*(c6+t_eff*(c7+t_eff*c8)))))))
return
end
function psi_m(zeta)
use elfe_glbl, only : rkind
implicit none
real(rkind) :: psi_m
real(rkind), intent(in) :: zeta
real(rkind) :: chi, half_pi
half_pi = 2.0 * atan(1._rkind)
chi = (1.0 - 16.0 * zeta)**0.25
psi_m = 2.0 * log( 0.5 * (1.0 + chi) ) + &
& log( 0.5 * (1.0 + chi*chi) ) - &
& 2.0 * atan(chi) + half_pi
return
end
function psi_h(zeta)
use elfe_glbl, only : rkind
implicit none
real(rkind) :: psi_h
real(rkind), intent(in) :: zeta
real(rkind) :: chi
chi = (1.0 - 16.0 * zeta)**0.25
psi_h = 2.0 * log( 0.5 * (1.0 + chi*chi) )
return
end
subroutine rotate_winds (u, v, num_nodes_out)
use elfe_glbl, only : rkind,ipgl
use elfe_msgp, only : myrank
implicit none
integer num_nodes_out
real(rkind) u(num_nodes_out), v(num_nodes_out)
integer i_node, i_node_tmp, alloc_stat,ne_global,np_global
real(rkind) x_tmp, y_tmp, speed, dir,tmp
real(rkind) pi, deg_to_rad
real(rkind), save, allocatable, dimension(:) :: &
& rotate_angle
character, parameter :: rot_file*50 = 'windrot_geo2proj.gr3'
logical, save :: first_call = .true.
pi = 4.0 * atan(1.0_rkind)
deg_to_rad = pi / 180.0_rkind
if (first_call) then
allocate (rotate_angle(num_nodes_out), stat=alloc_stat)
call check_allocation('rotate_angle', 'rotate_winds', &
& alloc_stat)
open(10, file=rot_file, status='old')
read(10,*)
read(10,*)ne_global,np_global
do i_node =1, np_global
read(10,*) i_node_tmp, x_tmp, y_tmp, tmp
if(ipgl(i_node)%rank==myrank) rotate_angle(ipgl(i_node)%id)=tmp*deg_to_rad
enddo
close(10)
endif
do i_node =1, num_nodes_out
dir = atan2(-u(i_node),-v(i_node))
speed = sqrt(u(i_node)*u(i_node) + v(i_node)*v(i_node))
dir = dir + rotate_angle(i_node)
u(i_node) = -speed * sin(dir)
v(i_node) = -speed * cos(dir)
enddo
first_call = .false.
return
end
subroutine check_allocation(variable, location, status)
use elfe_glbl, only : errmsg
use elfe_msgp, only : parallel_abort
implicit none
character(*), intent(in) :: variable, location
integer, intent(in) :: status
if (status .ne. 0) then
write(errmsg,*) 'allocation error in: ', location,'; for: ', variable
call parallel_abort(errmsg)
endif
end subroutine check_allocation
module netcdf_io
use elfe_glbl, only : rkind
implicit none
integer, parameter :: max_files = 1
integer, parameter :: max_times = 10000
type dataset_info
character name*50
logical :: exist = .false.
integer :: num_files = 0
integer :: nx = 0
integer :: ny = 0
integer :: num_nodes = 0
integer :: num_elems = 0
#ifndef NO_TR_15581 /* TR_15581 is implemented; default */
real(rkind), allocatable, dimension(:,:) :: lon, lat
real(rkind), allocatable, dimension(:,:) :: weight
integer, allocatable, dimension(:) :: node_i, node_j
integer, allocatable, dimension(:,:) :: node_num, elem_nodes
integer, allocatable, dimension(:) :: in_elem_for_out_node
#else /* TR_15581 is NOT implemented */
real(rkind), pointer, dimension(:,:) :: lon, lat
real(rkind), pointer, dimension(:,:) :: weight
integer, pointer, dimension(:) :: node_i, node_j
integer, pointer, dimension(:,:) :: node_num, elem_nodes
integer, pointer, dimension(:) :: in_elem_for_out_node
#endif /* NO_TR_15581 block */
integer :: num_times = 0
real(rkind), dimension(max_times) :: times
integer, dimension(max_times) :: file_num_for_time
integer, dimension(max_times) :: time_num_for_time
integer, dimension(max_files) :: jdate_for_file
real(rkind) :: max_window_hours
real(rkind) :: relative_weight
logical :: fail_if_missing
end type dataset_info
integer :: start_year = -9999
integer :: start_month = -9999
integer :: start_day = -9999
real(rkind) :: start_hour = -9999.0
real(rkind) :: utc_start = -9999.0
integer :: start_jdate
real(rkind) :: start_frac_jdate = -9999.0
real(rkind) :: air_1_relative_weight = 1.0
real(rkind) :: air_2_relative_weight = 2.0
real(rkind) :: air_1_max_window_hours = 50000.0
real(rkind) :: air_2_max_window_hours = 50000.0
logical :: air_1_fail_if_missing = .true.
logical :: air_2_fail_if_missing = .false.
character (len=50) :: air_1_file = 'sflux_air_1'
character (len=50) :: air_2_file = 'sflux_air_2'
character (len=50) :: uwind_name = 'uwind'
character (len=50) :: vwind_name = 'vwind'
character (len=50) :: prmsl_name = 'prmsl'
character (len=50) :: stmp_name = 'stmp'
character (len=50) :: spfh_name = 'spfh'
real(rkind) :: rad_1_relative_weight = 1.0
real(rkind) :: rad_2_relative_weight = 2.0
real(rkind) :: rad_1_max_window_hours = 50000.0
real(rkind) :: rad_2_max_window_hours = 50000.0
logical :: rad_1_fail_if_missing = .true.
logical :: rad_2_fail_if_missing = .false.
character (len=50) :: rad_1_file = 'sflux_rad_1'
character (len=50) :: rad_2_file = 'sflux_rad_2'
character (len=50) :: dlwrf_name = 'dlwrf'
character (len=50) :: dswrf_name = 'dswrf'
real(rkind) :: prc_1_relative_weight = 1.0
real(rkind) :: prc_2_relative_weight = 2.0
real(rkind) :: prc_1_max_window_hours = 50000.0
real(rkind) :: prc_2_max_window_hours = 50000.0
logical :: prc_1_fail_if_missing = .true.
logical :: prc_2_fail_if_missing = .false.
character (len=50) :: prc_1_file = 'sflux_prc_1'
character (len=50) :: prc_2_file = 'sflux_prc_2'
character (len=50) :: prate_name = 'prate'
namelist /sflux_inputs/ &
& start_year, start_month, start_day, start_hour, utc_start, &
& air_1_relative_weight, air_2_relative_weight, &
& air_1_max_window_hours, air_2_max_window_hours, &
& air_1_fail_if_missing, air_2_fail_if_missing, &
& air_1_file, air_2_file, &
& uwind_name, vwind_name, prmsl_name, stmp_name, spfh_name, &
& rad_1_relative_weight, rad_2_relative_weight, &
& rad_1_max_window_hours, rad_2_max_window_hours, &
& rad_1_fail_if_missing, rad_2_fail_if_missing, &
& rad_1_file, rad_2_file, &
& dlwrf_name, dswrf_name, &
& prc_1_relative_weight, prc_2_relative_weight, &
& prc_1_max_window_hours, prc_2_max_window_hours, &
& prc_1_fail_if_missing, prc_2_fail_if_missing, &
& prc_1_file, prc_2_file, &
& prate_name
end module netcdf_io
subroutine get_wind (time, u_air_node, v_air_node, p_air_node, &
& t_air_node, q_air_node)
use elfe_glbl, only : rkind, npa,fdb,lfdb
use elfe_msgp, only : myrank,parallel_abort
use netcdf_io
implicit none
real(rkind), intent(in) :: time
real(rkind), dimension(npa), intent(out) :: &
& u_air_node, v_air_node, p_air_node, t_air_node, q_air_node
integer num_nodes_out
logical, save :: first_call = .true.
type(dataset_info), save :: dataset_1, dataset_2
real(rkind) time_now
real(rkind), parameter :: secs_per_day = 86400.0
real(rkind), parameter :: t_freeze = 273.15
character data_name*50
num_nodes_out = npa
fdb='sflux2_0000'
lfdb=len_trim(fdb)
write(fdb(lfdb-3:lfdb),'(i4.4)') myrank
open(39,file='outputs/'//fdb,status='unknown')
rewind(39)
write(39,*)
write(39,*) 'get_wind: time = ', time
write(39,*) 'first_call = ', first_call
write(39,*) 'num_nodes_out = ', num_nodes_out
if (first_call) then
call get_sflux_inputs ()
dataset_1%name = air_1_file
dataset_1%max_window_hours = air_1_max_window_hours
dataset_1%fail_if_missing = air_1_fail_if_missing
dataset_1%relative_weight = air_1_relative_weight
dataset_2%name = air_2_file
dataset_2%max_window_hours = air_2_max_window_hours
dataset_2%fail_if_missing = air_2_fail_if_missing
dataset_2%relative_weight = air_2_relative_weight
if(myrank==0) then
write(16,*)
write(16,*) 'get_wind: sflux_inputs'
write(16,*) ' start_year = ', start_year
write(16,*) ' start_month = ', start_month
write(16,*) ' start_day = ', start_day
write(16,*) ' start_hour = ', start_hour
write(16,*) ' utc_start = ', utc_start
write(16,*) ' start_frac_jdate = ', start_frac_jdate
write(16,*) ' air_1_file = ', &
& trim(air_1_file)
write(16,*) ' air_1_max_window_hours = ', &
& air_1_max_window_hours
write(16,*) ' air_1_fail_if_missing = ', &
& air_1_fail_if_missing
write(16,*) ' air_1_relative_weight = ', &
& air_1_relative_weight
write(16,*) ' air_2_file = ', &
& trim(air_2_file)
write(16,*) ' air_2_max_window_hours = ', &
& air_2_max_window_hours
write(16,*) ' air_2_fail_if_missing = ', &
& air_2_fail_if_missing
write(16,*) ' air_2_relative_weight = ', &
& air_2_relative_weight
write(16,*) ' uwind_name = ', &
& trim(uwind_name)
write(16,*) ' vwind_name = ', &
& trim(vwind_name)
write(16,*) ' prmsl_name = ', &
& trim(prmsl_name)
write(16,*) ' stmp_name = ', &
& trim(stmp_name)
write(16,*) ' spfh_name = ', &
& trim(spfh_name)
endif
call get_dataset_info (dataset_1)
call get_dataset_info (dataset_2)
endif
time_now = start_frac_jdate + time/secs_per_day
write(39,*) 'current jdate = ', time_now
data_name = trim(uwind_name)
call combine_sflux_data &
& (time_now, dataset_1, dataset_2, &
& dataset_1%exist, dataset_2%exist, &
& data_name, u_air_node, &
& num_nodes_out)
data_name = trim(vwind_name)
call combine_sflux_data &
& (time_now, dataset_1, dataset_2, &
& dataset_1%exist, dataset_2%exist, &
& data_name, v_air_node, &
& num_nodes_out)
data_name = trim(prmsl_name)
call combine_sflux_data &
& (time_now, dataset_1, dataset_2, &
#ifndef MM5 /* MM5 not defined */
& dataset_1%exist, dataset_2%exist, &
#else /* MM5 is defined */
& dataset_1%exist, .false., &
#endif /* end of MM5 block */
& data_name, p_air_node, &
& num_nodes_out)
data_name = trim(stmp_name)
call combine_sflux_data &
& (time_now, dataset_1, dataset_2, &
& dataset_1%exist, dataset_2%exist, &
& data_name, t_air_node, &
& num_nodes_out)
data_name = trim(spfh_name)
call combine_sflux_data &
& (time_now, dataset_1, dataset_2, &
& dataset_1%exist, dataset_2%exist, &
& data_name, q_air_node, &
& num_nodes_out)
t_air_node = t_air_node - t_freeze
call rotate_winds (u_air_node, v_air_node, num_nodes_out)
close(39)
first_call = .false.
return
end
subroutine get_rad (time, shortwave_d, longwave_d)
use elfe_glbl, only : rkind, npa,fdb,lfdb,albedo
use elfe_msgp, only : myrank,parallel_abort
use netcdf_io
implicit none
real(rkind), intent(in) :: time
real(rkind), dimension(npa), intent(out) :: &
& longwave_d, shortwave_d
integer num_nodes_out, i_node
logical, save :: first_call = .true.
type(dataset_info), save :: dataset_1, dataset_2
real(rkind) time_now
real(rkind), parameter :: secs_per_day = 86400.0
character data_name*50
num_nodes_out = npa
fdb='sflux3_0000'
lfdb=len_trim(fdb)
write(fdb(lfdb-3:lfdb),'(i4.4)') myrank
open(40,file='outputs/'//fdb,status='unknown')
rewind(40)
write(40,*)
write(40,*) 'get_rad: time = ', time
write(40,*) 'first_call = ', first_call
write(40,*) 'num_nodes_out = ', num_nodes_out
if (first_call) then
call get_sflux_inputs ()
dataset_1%name = rad_1_file
dataset_1%max_window_hours = rad_1_max_window_hours
dataset_1%fail_if_missing = rad_1_fail_if_missing
dataset_1%relative_weight = rad_1_relative_weight
dataset_2%name = rad_2_file
dataset_2%max_window_hours = rad_2_max_window_hours
dataset_2%fail_if_missing = rad_2_fail_if_missing
dataset_2%relative_weight = rad_2_relative_weight
if(myrank==0) then
write(16,*)
write(16,*) 'get_rad: sflux_inputs'
write(16,*) ' start_year = ', start_year
write(16,*) ' start_month = ', start_month
write(16,*) ' start_day = ', start_day
write(16,*) ' start_hour = ', start_hour
write(16,*) ' utc_start = ', utc_start
write(16,*) ' start_frac_jdate = ', start_frac_jdate
write(16,*) ' rad_1_file = ', &
& trim(rad_1_file)
write(16,*) ' rad_1_max_window_hours = ', &
& rad_1_max_window_hours
write(16,*) ' rad_1_fail_if_missing = ', &
& rad_1_fail_if_missing
write(16,*) ' rad_1_relative_weight = ', &
& rad_1_relative_weight
write(16,*) ' rad_2_file = ', &
& trim(rad_2_file)
write(16,*) ' rad_2_max_window_hours = ', &
& rad_2_max_window_hours
write(16,*) ' rad_2_fail_if_missing = ', &
& rad_2_fail_if_missing
write(16,*) ' rad_2_relative_weight = ', &
& rad_2_relative_weight
write(16,*) ' dlwrf_name = ', &
& trim(dlwrf_name)
write(16,*) ' dswrf_name = ', &
& trim(dswrf_name)
endif
call get_dataset_info (dataset_1)
call get_dataset_info (dataset_2)
endif
time_now = start_frac_jdate + time/secs_per_day
write(40,*) 'current jdate = ', time_now
data_name = trim(dlwrf_name)
call combine_sflux_data &
& (time_now, dataset_1, dataset_2, &
& dataset_1%exist, dataset_2%exist, &
& data_name, longwave_d, &
& num_nodes_out)
data_name = trim(dswrf_name)
call combine_sflux_data &
& (time_now, dataset_1, dataset_2, &
& dataset_1%exist, dataset_2%exist, &
& data_name, shortwave_d, &
& num_nodes_out)
write(40,*) 'reducing shortwave'
do i_node = 1, num_nodes_out
shortwave_d(i_node) = &
& max( (1.0- albedo(i_node))*shortwave_d(i_node), &
& 0.0_rkind)
enddo
close(40)
first_call = .false.
return
end
subroutine get_precip_flux (time, precip_flux)
use elfe_glbl, only : rkind, npa,fdb,lfdb
use elfe_msgp, only : myrank,parallel_abort
use netcdf_io
implicit none
real(rkind), intent(in) :: time
real(rkind), dimension(npa), intent(out) :: precip_flux
integer num_nodes_out, i_node
logical, save :: first_call = .true.
type(dataset_info), save :: dataset_1, dataset_2
real(rkind) time_now
real(rkind), parameter :: secs_per_day = 86400.0
character data_name*50
num_nodes_out = npa
fdb='sflux4_0000'
lfdb=len_trim(fdb)
write(fdb(lfdb-3:lfdb),'(i4.4)') myrank
open(41,file='outputs/'//fdb,status='unknown')
rewind(41)
write(41,*)
write(41,*) 'get_precip_flux: time = ', time
write(41,*) 'first_call = ', first_call
write(41,*) 'num_nodes_out = ', num_nodes_out
if (first_call) then
call get_sflux_inputs ()
dataset_1%name = prc_1_file
dataset_1%max_window_hours = prc_1_max_window_hours
dataset_1%fail_if_missing = prc_1_fail_if_missing
dataset_1%relative_weight = prc_1_relative_weight
dataset_2%name = prc_2_file
dataset_2%max_window_hours = prc_2_max_window_hours
dataset_2%fail_if_missing = prc_2_fail_if_missing
dataset_2%relative_weight = prc_2_relative_weight
if(myrank==0) then
write(16,*)
write(16,*) 'get_precip_flux: sflux_inputs'
write(16,*) ' start_year = ', start_year
write(16,*) ' start_month = ', start_month
write(16,*) ' start_day = ', start_day
write(16,*) ' start_hour = ', start_hour
write(16,*) ' utc_start = ', utc_start
write(16,*) ' start_frac_jdate = ', start_frac_jdate
write(16,*) ' prc_1_file = ', &
& trim(prc_1_file)
write(16,*) ' prc_1_max_window_hours = ', &
& prc_1_max_window_hours
write(16,*) ' prc_1_fail_if_missing = ', &
& prc_1_fail_if_missing
write(16,*) ' prc_1_relative_weight = ', &
& prc_1_relative_weight
write(16,*) ' prc_2_file = ', &
& trim(prc_2_file)
write(16,*) ' prc_2_max_window_hours = ', &
& prc_2_max_window_hours
write(16,*) ' prc_2_fail_if_missing = ', &
& prc_2_fail_if_missing
write(16,*) ' prc_2_relative_weight = ', &
& prc_2_relative_weight
write(16,*) ' prate_name = ', &
& trim(prate_name)
endif
call get_dataset_info (dataset_1)
call get_dataset_info (dataset_2)
endif
time_now = start_frac_jdate + time/secs_per_day
write(41,*) 'current jdate = ', time_now
data_name = trim(prate_name)
call combine_sflux_data &
& (time_now, dataset_1, dataset_2, &
& dataset_1%exist, dataset_2%exist, &
& data_name, precip_flux, &
& num_nodes_out)
close(41)
first_call = .false.
return
end
subroutine get_dataset_info (info)
use elfe_glbl, only : rkind, npa, xlon, ylat
use elfe_msgp, only : myrank,parallel_abort
use netcdf_io
implicit none
type(dataset_info), intent(inout) :: info
character file_name*50, get_file_name*50, data_name*50
integer file_num, alloc_stat, num_nodes_out
num_nodes_out = npa
file_num = 1
file_name = get_file_name(info%name, file_num)
inquire(file=file_name, exist=info%exist)
if(myrank==0) then
write(16,*)
write(16,*) 'netCDF dataset and existence: ', &
& trim(info%name), info%exist
write(16,*)
endif
if ( (.not. info%exist) .and. (info%fail_if_missing) ) then
call halt_error ('missing dataset: ' // info%name)
endif
if (info%exist) then
if(myrank==0) write(16,*) 'getting additional info for: ', info%name
call get_times_etc (info%name, info%times, &
& info%file_num_for_time, &
& info%time_num_for_time, &
& info%num_times, &
& info%num_files, info%jdate_for_file, &
& info%nx, info%ny, max_times, max_files)
allocate (info%lon(info%nx,info%ny), &
& info%lat(info%nx,info%ny), &
& stat=alloc_stat)
call check_allocation('lon/lat', 'get_dataset_info', &
& alloc_stat)
data_name = 'lon'
call read_coord (file_name, data_name, info%lon, &
& info%nx, info%ny)
data_name = 'lat'
call read_coord (file_name, data_name, info%lat, &
& info%nx, info%ny)
call fix_coords (info%lon, info%lat, info%nx, info%ny)
info%num_nodes = info%nx * info%ny
info%num_elems = (info%nx - 1) * (info%ny - 1) * 2
allocate (info%node_i(info%num_nodes), &
& info%node_j(info%num_nodes), &
& info%node_num(info%nx,info%ny), &
& info%elem_nodes(info%num_elems,3), &
& info%in_elem_for_out_node(num_nodes_out), &
& stat=alloc_stat)
call check_allocation('integer grid variables', &
& 'get_dataset_info', alloc_stat)
allocate (info%weight(num_nodes_out,3), &
& stat=alloc_stat)
call check_allocation('real grid variables', &
& 'get_dataset_info', alloc_stat)
call list_nodes (info%node_i, info%node_j, info%node_num, &
& info%num_nodes, info%nx, info%ny)
call list_elems (info%elem_nodes, info%node_num, &
& info%nx, info%ny, info%num_elems)
call get_weight (info%lon, info%lat, xlon, ylat, &
& info%elem_nodes, info%node_i, info%node_j, &
& info%nx, info%ny, info%num_elems, &
& info%num_nodes, &
& num_nodes_out, info%in_elem_for_out_node, &
& info%weight)
endif
return
end
character*3 function char_num (num)
implicit none
integer, intent(in) :: num
character char*3
10 format ('00', i1)
20 format ('0', i2)
30 format (i3)
if (num .le. 9) then
write(char,10) num
else if (num .le. 99) then
write(char,20) num
else if (num .le. 999) then
write(char,30) num
else
call halt_error ('get_char_num: num too large!')
endif
char_num = char
return
end
character*50 function get_file_name (dataset_name, num)
implicit none
integer, intent(in) :: num
character, intent(in) :: dataset_name*50
character char_num*3
character, parameter :: prefix*6 = 'sflux/'
character, parameter :: suffix*3 = '.nc'
get_file_name = prefix // trim(dataset_name) // '.' // &
& char_num(num) // suffix
return
end
subroutine check_err(iret)
implicit none
integer iret
include 'netcdf.inc'
if (iret .ne. NF_NOERR) then
call halt_error (nf_strerror(iret))
endif
return
end
INTEGER FUNCTION JD(YYYY,MM,DD)
IMPLICIT NONE
INTEGER, INTENT(IN) :: YYYY,MM,DD
JD=DD-32075+1461*(YYYY+4800+(MM-14)/12)/4 &
& +367*(MM-2-((MM-14)/12)*12)/12-3* &
& ((YYYY+4900+(MM-14)/12)/100)/4
RETURN
END
subroutine get_times_etc (dataset_name, times, &
& file_num_for_time, &
& time_num_for_time, &
& num_times, &
& num_files, jdate_for_file, &
& nx, ny, max_times, max_files)
use elfe_glbl, only : rkind
use elfe_msgp, only : myrank
implicit none
character, intent(in) :: dataset_name*50
integer, intent(in) :: max_times, max_files
integer, intent(out) :: num_times, num_files, nx, ny
real(rkind), intent(out), dimension(max_times) :: times
integer, intent(out), dimension(max_times) :: &
& file_num_for_time, time_num_for_time
integer, intent(out), dimension(max_files) :: jdate_for_file
integer file_num, num_file_times, nx_test, ny_test
logical have_file, repeat, at_end
character file_name*50, get_file_name*50
integer, parameter :: max_file_times = 10000
real(rkind) test_time, file_times(max_file_times)
integer i_time, repeat_num
file_num = 0
do
file_num = file_num + 1
file_name = get_file_name(dataset_name, file_num)
inquire(file=file_name, exist=have_file)
if (.not. have_file) exit
num_files = file_num
enddo
if (num_files .gt. max_files) then
call halt_error ('num_files exceeds max_files!')
endif
file_num = 1
file_name = get_file_name(dataset_name, file_num)
call get_dims (file_name, nx, ny)
do file_num = 1, num_files
file_name = get_file_name(dataset_name, file_num)
call get_dims (file_name, nx_test, ny_test)
if ( (nx_test .ne. nx) .or. (ny_test .ne. ny) ) then
call halt_error ('nx and/or ny mismatch!')
endif
call get_file_times (file_name, file_times, &
& jdate_for_file(file_num), &
& max_file_times, num_file_times)
if (num_file_times .gt. max_times) then
call halt_error ('num_file_times exceeds max_times!')
endif
if (file_num .eq. 1) then
do i_time = 1, num_file_times
times(i_time) = real(jdate_for_file(file_num),rkind) &
& + file_times(i_time)
file_num_for_time(i_time) = file_num
time_num_for_time(i_time) = i_time
enddo
num_times = num_file_times
else
do i_time = 1, num_file_times
test_time = real(jdate_for_file(file_num),rkind) &
& + file_times(i_time)
call check_times (test_time, times, num_times, &
& repeat_num, repeat, at_end)
if (repeat) then
file_num_for_time(repeat_num) = file_num
time_num_for_time(repeat_num) = i_time
else if (at_end) then
num_times = num_times + 1
if (num_times .gt. max_times) then
call halt_error ('num_times exceeds max_times!')
endif
times(num_times) = test_time
file_num_for_time(num_times) = file_num
time_num_for_time(num_times) = i_time
endif
enddo
endif
enddo
if(myrank==0) then
write(16,*) 'num_files = ', num_files
write(16,*) 'num_times = ', num_times
write(16,*) 'first time = ', times(1)
write(16,*) 'last time = ', times(num_times)
endif
return
end
subroutine get_file_times (file_name, file_times, &
& file_julian_date, max_file_times, &
& num_file_times)
use elfe_glbl, only : rkind
implicit none
include 'netcdf.inc'
character, intent(in) :: file_name*50
integer, intent(in) :: max_file_times
integer, intent(out) :: num_file_times, file_julian_date
real(rkind), intent(out), dimension(max_file_times) :: &
& file_times
real, dimension(max_file_times) :: file_times_tmp
integer ncid, iret, time_dim, time_id, i_time
character data_name*50, attr_name*50
integer, allocatable, dimension(:) :: base_date
integer day, month, year, jd, n_base_date, allocate_stat
iret = nf_open(file_name, NF_NOWRITE, ncid)
call check_err(iret)
data_name = 'time'
iret = nf_inq_varid(ncid, data_name, time_id)
call check_err(iret)
iret = nf_inq_vardimid (ncid, time_id, time_dim)
call check_err(iret)
iret = nf_inq_dimlen(ncid, time_dim, num_file_times)
call check_err(iret)
iret = nf_get_var_real(ncid, time_id, file_times_tmp)
call check_err(iret)
do i_time = 1, num_file_times
file_times(i_time) = real(file_times_tmp(i_time),rkind)
enddo
attr_name = 'base_date'
iret = nf_inq_attlen (ncid, time_id, attr_name, n_base_date)
if (n_base_date .lt. 3) then
call halt_error ('insufficient fields in base_date!')
endif
allocate(base_date(n_base_date), stat=allocate_stat)
call check_allocation('base_date', 'get_file_times', &
& allocate_stat)
iret = nf_get_att_int(ncid, time_id, attr_name, base_date)
year = base_date(1)
month = base_date(2)
day = base_date(3)
file_julian_date = jd(year,month,day)
deallocate(base_date)
iret = nf_close(ncid)
call check_err(iret)
return
end
subroutine check_times (test_time, times, num_times, &
& repeat_num, repeat, at_end)
use elfe_glbl, only : rkind
implicit none
integer, intent(in) :: num_times
real(rkind), intent(in) :: test_time
real(rkind), intent(in), dimension(num_times) :: times
logical, intent(out) :: repeat, at_end
integer, intent(out) :: repeat_num
real(rkind), parameter :: time_eps = 0.001
integer i_time
repeat = .false.
do i_time = 1, num_times
if (abs(test_time - times(i_time)) .le. time_eps) then
repeat = .true.
repeat_num = i_time
endif
enddo
at_end = ((test_time - (times(num_times) + time_eps)) .gt. 0.0)
return
end
subroutine get_dims (file_name, nx, ny)
implicit none
include 'netcdf.inc'
character, intent(in) :: file_name*50
integer, intent(out) :: nx, ny
integer ncid, iret, nx_dim, ny_dim, dim_ids(3), test_var_id
character, parameter :: test_variable*50 = 'lat'
iret = nf_open(file_name, NF_NOWRITE, ncid)
call check_err(iret)
iret = nf_inq_varid(ncid, test_variable, test_var_id)
call check_err(iret)
iret = nf_inq_vardimid (ncid, test_var_id, dim_ids)
call check_err(iret)
nx_dim = dim_ids(1)
iret = nf_inq_dimlen(ncid, nx_dim, nx)
call check_err(iret)
ny_dim = dim_ids(2)
iret = nf_inq_dimlen(ncid, ny_dim, ny)
call check_err(iret)
iret = nf_close(ncid)
call check_err(iret)
return
end
subroutine halt_error (message)
use elfe_msgp, only : parallel_abort
implicit none
character(*), intent(in) :: message
call parallel_abort(message)
return
end
subroutine read_coord (file_name, data_name, coord, &
& nx, ny)
use elfe_glbl, only : rkind
implicit none
include 'netcdf.inc'
character, intent(in) :: file_name*50, data_name*50
integer, intent(in) :: nx, ny
real(rkind), intent(out), dimension(nx,ny) :: coord
integer iret, ncid, var_id, i, j
real coord_tmp(nx,ny)
iret = nf_open(file_name, NF_NOWRITE, ncid)
call check_err(iret)
iret = nf_inq_varid(ncid, data_name, var_id)
call check_err(iret)
iret = nf_get_var_real (ncid, var_id, coord_tmp)
call check_err(iret)
do j = 1, ny
do i = 1, nx
coord(i,j) = real(coord_tmp(i,j),rkind)
enddo
enddo
iret = nf_close(ncid)
call check_err(iret)
return
end
subroutine read_data (file_name, data_name, data, &
& nx, ny, time_num)
use elfe_glbl, only : rkind
implicit none
include 'netcdf.inc'
character(*), intent(in) :: file_name, data_name
integer, intent(in) :: nx, ny, time_num
real(rkind), intent(out), dimension(nx,ny) :: data
integer iret, ncid, var_id, i, j
integer data_start(3), data_count(3)
real data_tmp(nx,ny)
data_start(1) = 1
data_start(2) = 1
data_start(3) = time_num
data_count(1) = nx
data_count(2) = ny
data_count(3) = 1
iret = nf_open(file_name, NF_NOWRITE, ncid)
call check_err(iret)
iret = nf_inq_varid(ncid, data_name, var_id)
call check_err(iret)
iret = nf_get_vara_real(ncid, var_id, data_start, &
& data_count, data_tmp)
call check_err(iret)
do j = 1, ny
do i = 1, nx
data(i,j) = real(data_tmp(i,j),rkind)
enddo
enddo
iret = nf_close(ncid)
call check_err(iret)
return
end
subroutine list_nodes (node_i, node_j, node_num, &
& num_nodes, nx, ny)
implicit none
integer, intent(in) :: nx, ny, num_nodes
integer, intent(out), dimension(num_nodes) :: node_i, node_j
integer, intent(out), dimension(nx,ny) :: node_num
integer i_node, i, j
i_node = 0
do j = 1, ny
do i = 1, nx
i_node = i_node + 1
node_i(i_node) = i
node_j(i_node) = j
node_num(i,j) = i_node
enddo
enddo
return
end
subroutine list_elems (elem_nodes, node_num, &
& nx, ny, num_elems)
implicit none
integer, intent(in) :: nx, ny, num_elems
integer, intent(in), dimension(nx,ny) :: node_num
integer, intent(out), dimension(num_elems,3) :: elem_nodes
integer i, j, i_elem
i_elem = 0
do j = 1, ny-1
do i = 1, nx-1
i_elem = i_elem + 1
elem_nodes(i_elem,1) = node_num(i,j)
elem_nodes(i_elem,2) = node_num(i+1,j+1)
elem_nodes(i_elem,3) = node_num(i,j+1)
i_elem = i_elem + 1
elem_nodes(i_elem,1) = node_num(i,j)
elem_nodes(i_elem,2) = node_num(i+1,j)
elem_nodes(i_elem,3) = node_num(i+1,j+1)
enddo
enddo
return
end
subroutine get_weight (x_in, y_in, x_out, y_out, &
& elem_nodes, node_i, node_j, &
& nx, ny, num_elems, num_nodes, &
& num_nodes_out, in_elem_for_out_node, &
& weight)
use elfe_glbl, only : rkind,errmsg
use elfe_msgp, only : myrank,parallel_abort
implicit none
integer, intent(in) :: nx, ny, num_elems, num_nodes
integer, intent(in) :: num_nodes_out
integer, intent(in), dimension(num_nodes) :: node_i, node_j
integer, intent(in), dimension(num_elems,3) :: elem_nodes
real(rkind), intent(in), dimension(nx,ny) :: x_in, y_in
real(rkind), intent(in), dimension(num_nodes_out) :: &
& x_out, y_out
real(rkind), intent(out), &
& dimension(num_nodes_out,3) :: weight
integer, intent(out), dimension(num_nodes_out) :: &
& in_elem_for_out_node
real(rkind) area_in(num_elems)
integer i_elem, i_node
integer i1, j1, i2, j2, i3, j3
integer last_elem, top, floor
real(rkind) x1, y1, x2, y2, x3, y3, x4, y4
real(rkind) a1, a2, a3, aa, ae
real(rkind) ae_min
real(rkind), parameter :: epsilon = 1.0e-10
real(rkind), parameter :: bad_point_flag = -9.9e20
logical zero_ae, completed_check
do i_elem = 1, num_elems
i1 = node_i(elem_nodes(i_elem,1))
j1 = node_j(elem_nodes(i_elem,1))
x1 = x_in(i1,j1)
y1 = y_in(i1,j1)
i2 = node_i(elem_nodes(i_elem,2))
j2 = node_j(elem_nodes(i_elem,2))
x2 = x_in(i2,j2)
y2 = y_in(i2,j2)
i3 = node_i(elem_nodes(i_elem,3))
j3 = node_j(elem_nodes(i_elem,3))
x3 = x_in(i3,j3)
y3 = y_in(i3,j3)
area_in(i_elem) = 0.5 * &
& ( (x1-x3)*(y2-y3) + (x3-x2)*(y1-y3) )
enddo
last_elem = 0
top = 1
floor = 1
do i_node = 1, num_nodes_out
ae_min = 1.0e25
in_elem_for_out_node(i_node) = 0
zero_ae = .false.
100 continue
if (last_elem .eq. 0) then
i_elem = 1
else if ( (top .eq. 0) .and. (floor .eq. 0) ) then
i_elem = last_elem
top = i_elem
floor = i_elem
else if (last_elem .eq. floor) then
if (top .eq. num_elems) then
i_elem = floor - 1
floor = floor - 1
else
i_elem = top + 1
top = top + 1
endif
else if (last_elem .eq. top) then
if (floor .eq. 1) then
i_elem = top + 1
top = top + 1
else
i_elem = floor - 1
floor = floor - 1
endif
else
write(errmsg,*) 'search error in get_weight!'
call parallel_abort(errmsg)
endif
i1 = node_i(elem_nodes(i_elem,1))
j1 = node_j(elem_nodes(i_elem,1))
x1 = x_in(i1,j1)
y1 = y_in(i1,j1)
i2 = node_i(elem_nodes(i_elem,2))
j2 = node_j(elem_nodes(i_elem,2))
x2 = x_in(i2,j2)
y2 = y_in(i2,j2)
i3 = node_i(elem_nodes(i_elem,3))
j3 = node_j(elem_nodes(i_elem,3))
x3 = x_in(i3,j3)
y3 = y_in(i3,j3)
x4 = x_out(i_node)
y4 = y_out(i_node)
a1 = (x4-x3)*(y2-y3) + (x2-x3)*(y3-y4)
a2 = (x4-x1)*(y3-y1) - (y4-y1)*(x3-x1)
a3 = (y4-y1)*(x2-x1) - (x4-x1)*(y2-y1)
aa = abs(a1) + abs(a2) + abs(a3)
if (area_in(i_elem) .gt. 0.0) then
ae = abs(aa - 2.0*area_in(i_elem)) &
& / (2.0*area_in(i_elem))
else
ae = 1.0e25
endif
zero_ae = (ae .lt. epsilon)
if (ae .lt. ae_min) then
ae_min = ae
in_elem_for_out_node(i_node) = i_elem
endif
completed_check = (floor .eq. 1) .and. (top .eq. num_elems)
last_elem = i_elem
if ( (.not. completed_check) .and. (.not. zero_ae) ) goto 100
if (in_elem_for_out_node(i_node) .eq. 0) then
write(errmsg,*)'Could not find suitable element in input ', &
'grid for output node #', i_node, &
'x_out, y_out = ', x_out(i_node), y_out(i_node), &
'in_elem_for_out_node, ae_min = ', &
in_elem_for_out_node(i_node), ae_min
call parallel_abort(errmsg)
endif
i_elem = in_elem_for_out_node(i_node)
i1 = node_i(elem_nodes(i_elem,1))
j1 = node_j(elem_nodes(i_elem,1))
x1 = x_in(i1,j1)
y1 = y_in(i1,j1)
i2 = node_i(elem_nodes(i_elem,2))
j2 = node_j(elem_nodes(i_elem,2))
x2 = x_in(i2,j2)
y2 = y_in(i2,j2)
i3 = node_i(elem_nodes(i_elem,3))
j3 = node_j(elem_nodes(i_elem,3))
x3 = x_in(i3,j3)
y3 = y_in(i3,j3)
if ( (x1 .le. bad_point_flag) .or. &
& (x2 .le. bad_point_flag) .or. &
& (x3 .le. bad_point_flag) ) then
write(errmsg,*)'Bad x-y flag from input grid', &
'for output node #', i_node, &
'x_out, y_out = ', x_out(i_node), y_out(i_node), &
'in_elem_for_out_node, ae_min = ', &
in_elem_for_out_node(i_node), ae_min
call parallel_abort(errmsg)
endif
x4 = x_out(i_node)
y4 = y_out(i_node)
weight(i_node,1) = ( (x4-x3)*(y2-y3) + (x2-x3)*(y3-y4) ) &
& / ( 2.0*area_in(i_elem) )
weight(i_node,2) = ( (x4-x1)*(y3-y1) - (y4-y1)*(x3-x1) ) &
& / ( 2.0*area_in(i_elem) )
weight(i_node,3) = ( -(x4-x1)*(y2-y1) + (y4-y1)*(x2-x1) ) &
& / ( 2.0*area_in(i_elem) )
top = 0
floor = 0
enddo
return
end
subroutine fix_coords (lon, lat, nx, ny)
use elfe_glbl, only : rkind
implicit none
integer, intent(in) :: nx, ny
real(rkind), intent(inout), dimension(nx,ny) :: &
& lon, lat
real(rkind) pi, deg_to_rad
integer i, j
pi = 4.0 * atan(1.0_rkind)
deg_to_rad = pi / 180.0
do j = 1, ny
do i = 1, nx
if (lon(i,j) .gt. 180.0) lon(i,j) = lon(i,j) - 360.0
enddo
enddo
do j = 1, ny
do i = 1, nx
lon(i,j) = lon(i,j) * deg_to_rad
lat(i,j) = lat(i,j) * deg_to_rad
enddo
enddo
return
end
subroutine get_sflux_inputs ()
use elfe_glbl, only : rkind
use netcdf_io
implicit none
character, parameter :: &
& sflux_inputs_file*50 = 'sflux/sflux_inputs.txt'
real(rkind), parameter :: hours_per_day = 24.0
integer jd
logical, save :: first_call = .true.
logical exst
if (first_call) then
first_call = .false.
inquire(file=sflux_inputs_file, exist=exst)
if (.not. exst) &
& call halt_error ('you must have sflux_inputs_file!')
open (unit=77, file=sflux_inputs_file, status='old')
read(77, nml=sflux_inputs)
close (unit = 77)
if (start_year .lt. -9000) call halt_error &
& ('sflux_inputs_file: you must supply a value for start_year')
if (start_month .lt. -9000) call halt_error &
& ('sflux_inputs_file: you must supply a value for start_month')
if (start_day .lt. -9000) call halt_error &
& ('sflux_inputs_file: you must supply a value for start_day')
if (start_hour .lt. -9000.0) call halt_error &
& ('sflux_inputs_file: you must supply a value for start_hour')
if (utc_start .lt. -9000.0) call halt_error &
& ('sflux_inputs_file: you must supply a value for utc_start')
start_jdate = jd(start_year,start_month,start_day)
start_frac_jdate = real(start_jdate,rkind) &
& + (start_hour + utc_start) / hours_per_day
endif
return
end
subroutine get_bracket (time, input_times, time_num_1, &
& got_bracket, num_times)
use elfe_glbl, only : rkind
implicit none
integer, intent(in) :: num_times
real(rkind), intent(in) :: time
real(rkind), intent(in), dimension(num_times) :: &
& input_times
integer, intent(out) :: time_num_1
logical, intent(out) :: got_bracket
time_num_1 = 0
got_bracket = .false.
do
time_num_1 = time_num_1 + 1
got_bracket = ( input_times(time_num_1) .le. time .and. &
& time .le. input_times(time_num_1+1) )
if (got_bracket .or. (time_num_1 .eq. num_times-1)) exit
enddo
return
end
subroutine get_sflux_data (time_now, info, data_name, &
& data_out, got_suitable_bracket, &
& num_nodes_out)
use elfe_glbl, only : rkind,errmsg
use elfe_msgp, only : myrank,parallel_abort
use netcdf_io
implicit none
real(rkind), intent(in) :: time_now
type(dataset_info), intent(in) :: info
character(*), intent(in) :: data_name
integer, intent(in) :: num_nodes_out
real(rkind), intent(out), dimension(num_nodes_out) :: &
& data_out
logical, intent(out) :: got_suitable_bracket
real(rkind), parameter :: hours_per_day = 24.0
real(rkind), dimension(info%nx,info%ny) :: data_tmp
real(rkind) window_hours
integer time_num_1, time_num_2
logical got_bracket
character(len=100) :: msg_tmp
call get_bracket (time_now, info%times, time_num_1, &
& got_bracket, info%num_times)
window_hours = hours_per_day * ( info%times(time_num_1+1) - &
& info%times(time_num_1) )
got_suitable_bracket = got_bracket .and. &
& (window_hours .lt. info%max_window_hours)
if ( (.not. got_suitable_bracket) .and. &
& (info%fail_if_missing) ) then
write(errmsg,*) 'no appropriate time exists for: ', info%name, &
'time_now = ', time_now, 'first time available = ', &
& info%times(1),'last time available = ', &
& info%times(info%num_times), 'got_bracket = ', got_bracket, &
'got_suitable_bracket = ', got_suitable_bracket
if (got_bracket) then
write(msg_tmp,*) ', window_hours = ', window_hours, &
'max_window_hours = ', info%max_window_hours, &
'time_1 = ', info%times(time_num_1), &
'time_2 = ', info%times(time_num_1+1)
errmsg=errmsg//msg_tmp
endif
call parallel_abort(errmsg)
endif
if (got_suitable_bracket) then
time_num_2 = time_num_1 + 1
call interp_time &
& (info%name, time_now, data_name, &
& info%times(time_num_1), &
& info%file_num_for_time(time_num_1), &
& info%time_num_for_time(time_num_1), &
& info%times(time_num_2), &
& info%file_num_for_time(time_num_2), &
& info%time_num_for_time(time_num_2), &
& data_tmp, info%nx, info%ny )
call interp_grid (data_out, data_tmp, info%weight, &
& info%in_elem_for_out_node, &
& info%elem_nodes, info%num_elems, &
& info%node_i, info%node_j, &
& info%num_nodes, num_nodes_out, &
& info%nx, info%ny)
endif
return
end
subroutine interp_time (dataset_name, time, data_name, &
& time_1, file_num_1, file_time_1, &
& time_2, file_num_2, file_time_2, &
& data_out, nx, ny)
use elfe_glbl, only : rkind
implicit none
integer, intent(in) :: file_num_1, file_time_1, &
& file_num_2, file_time_2, &
& nx, ny
character(*), intent(in) :: dataset_name, data_name
real(rkind), intent(in) :: time, time_1, time_2
real(rkind), intent(out), dimension(nx,ny) :: data_out
character file_name_1*50, file_name_2*50, get_file_name*50
real(rkind), dimension(nx,ny) :: data_1, data_2
real(rkind) time_ratio
integer i, j
file_name_1 = get_file_name(dataset_name, file_num_1)
file_name_2 = get_file_name(dataset_name, file_num_2)
call read_data (file_name_1, data_name, data_1, &
& nx, ny, file_time_1)
call read_data (file_name_2, data_name, data_2, &
& nx, ny, file_time_2)
time_ratio = (time - time_1) / (time_2 - time_1)
do j = 1, ny
do i = 1, nx
data_out(i,j) = data_1(i,j) &
& + (data_2(i,j) - data_1(i,j)) * time_ratio
enddo
enddo
return
end
subroutine interp_grid (data_out, data_in, weight, &
& in_elem_for_out_node, &
& elem_nodes, num_elems, &
& node_i, node_j, &
& num_nodes, num_nodes_out, &
& nx, ny)
use elfe_glbl, only : rkind
implicit none
integer, intent(in) :: num_nodes, num_nodes_out, num_elems
integer, intent(in) :: nx, ny
real(rkind), intent(in), dimension(num_nodes_out,3) :: &
& weight
integer, intent(in), dimension(num_nodes) :: node_i, node_j
real(rkind), intent(in), dimension(nx,ny) :: data_in
real(rkind), intent(out), dimension(num_nodes_out) :: &
& data_out
integer, intent(in), dimension(num_elems,3) :: elem_nodes
integer, intent(in), dimension(num_nodes_out) :: &
& in_elem_for_out_node
integer i_node, i_elem, i1, i2, i3, j1, j2, j3
do i_node = 1, num_nodes_out
i_elem = in_elem_for_out_node(i_node)
i1 = node_i(elem_nodes(i_elem,1))
j1 = node_j(elem_nodes(i_elem,1))
i2 = node_i(elem_nodes(i_elem,2))
j2 = node_j(elem_nodes(i_elem,2))
i3 = node_i(elem_nodes(i_elem,3))
j3 = node_j(elem_nodes(i_elem,3))
data_out(i_node) = data_in(i1,j1) * weight(i_node,1) &
& + data_in(i2,j2) * weight(i_node,2) &
& + data_in(i3,j3) * weight(i_node,3)
enddo
return
end
subroutine combine_sflux_data (time_now, info_1, info_2, &
& read_1, read_2, &
& data_name, data_out, &
& num_nodes_out)
use elfe_glbl, only : rkind
use netcdf_io
implicit none
real(rkind), intent(in) :: time_now
type(dataset_info), intent(in) :: info_1, info_2
character(*), intent(in) :: data_name
logical, intent(in) :: read_1, read_2
integer, intent(in) :: num_nodes_out
real(rkind), intent(out), dimension(num_nodes_out) :: &
& data_out
real(rkind), dimension(num_nodes_out) :: data_1, data_2
real(rkind) local_weight_2, sum_weights
logical got_data_1, got_data_2, bad_node_2
integer i_node
got_data_1 = .false.
got_data_2 = .false.
if (read_1) then
call get_sflux_data (time_now, info_1, data_name, &
& data_1, got_data_1, num_nodes_out)
endif
if (read_2) then
call get_sflux_data (time_now, info_2, data_name, &
& data_2, got_data_2, num_nodes_out)
endif
if (.not. got_data_1) then
call halt_error ('missing data_1: ' // data_name)
else
if (.not. got_data_2) then
data_out = data_1
else
do i_node = 1, num_nodes_out
bad_node_2 = ( &
& info_2%weight(i_node,1) .lt. 0.0 .or. &
& info_2%weight(i_node,1) .gt. 1.0 .or. &
& info_2%weight(i_node,2) .lt. 0.0 .or. &
& info_2%weight(i_node,2) .gt. 1.0 .or. &
& info_2%weight(i_node,3) .lt. 0.0 .or. &
& info_2%weight(i_node,3) .gt. 1.0 &
& )
if (bad_node_2) then
local_weight_2 = 0.0
else
local_weight_2 = info_2%relative_weight
endif
sum_weights = info_1%relative_weight + local_weight_2
data_out(i_node) = &
& ( info_1%relative_weight * data_1(i_node) + &
& local_weight_2 * data_2(i_node) ) / &
& sum_weights
enddo
endif
endif
return
end
