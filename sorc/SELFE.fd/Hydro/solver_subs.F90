subroutine solve_jcg(itime,moitn,mxitn,rtol,s,x,b,bc,lbc)
#ifdef USE_MPIMODULE
use mpi
#endif
use elfe_glbl, only : rkind,np,npa,wtimer,iplg,ipgl,mnei,nnp,inp,errmsg
use elfe_msgp
implicit none
#ifndef USE_MPIMODULE
include 'mpif.h'
#endif
integer,intent(in) :: itime
integer,intent(in) :: moitn,mxitn
real(rkind),intent(in) :: rtol
real(rkind),intent(in) :: s(np,0:(mnei+1))
real(rkind),intent(inout) :: x(npa)
real(rkind),intent(in) :: b(np)
real(rkind),intent(in) :: bc(npa)
logical,intent(in) :: lbc(npa)
integer :: itn,ip,jp,j
real(rkind) :: rdotrl,rdotr,rdotzl,rdotz,old_rdotz,beta,alphal,alpha,cwtmp
real(rkind) :: rtol2,rdotr0
real(rkind) :: z(np),r(np),p(npa),sp(np)
integer :: inz(mnei+1,np),nnz(np)
real(rkind) :: snz(0:(mnei+1),np),bb(np)
nnz=0
do ip=1,np
if(lbc(ip)) then
if(bc(ip)<-9998) call parallel_abort('JCG: wrong b.c.')
snz(0,ip)=1
x(ip)=bc(ip)
bb(ip)=bc(ip)
else
snz(0,ip)=s(ip,0)
bb(ip)=b(ip)
do j=1,nnp(ip)
jp=inp(ip,j)
if(lbc(jp)) then
if(bc(jp)<-9998) call parallel_abort('JCG: wrong b.c. (2)')
bb(ip)=bb(ip)-s(ip,j)*bc(jp)
else
nnz(ip)=nnz(ip)+1
inz(nnz(ip),ip)=jp
snz(nnz(ip),ip)=s(ip,j)
endif
enddo
endif
enddo
itn=0
rdotrl=0
do ip=1,np
if(snz(0,ip)==0) call parallel_abort('JCG: zero diagonal')
sp(ip)=snz(0,ip)*x(ip)
do j=1,nnz(ip)
sp(ip)=sp(ip)+snz(j,ip)*x(inz(j,ip))
enddo
r(ip)=bb(ip)-sp(ip)
if(associated(ipgl(iplg(ip))%next)) then
if(ipgl(iplg(ip))%next%rank<myrank) cycle
endif
rdotrl=rdotrl+r(ip)*r(ip)
enddo
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_allreduce(rdotrl,rdotr,1,rtype,MPI_SUM,comm,ierr)
#ifdef INCLUDE_TIMING
wtimer(7,2)=wtimer(7,2)+mpi_wtime()-cwtmp
#endif
rtol2=rtol*rtol
rdotr0=rdotr
if(rdotr0<0) then
write(errmsg,*)'JCG: 0 initial error:',rdotr0
call parallel_abort(errmsg)
endif
if(rdotr0==0) return
if(myrank==0) then
write(33,'(//a,i8)') '********CG Solve at timestep ',itime
write(33,'(a,i6,2e14.6)') &
'Itn, 2Norm, Rnorm: ',itn,sqrt(rdotr),sqrt(rdotr/rdotr0)
endif
do
if(rdotr<=rtol2*rdotr0) then
if(myrank==0) write(33,*)'JCG converged in ',itn,' iterations'
exit
endif
if(itn>=mxitn) then
if(myrank==0) write(33,*)'JCG did not converge in ',mxitn,' iterations'
exit
endif
do ip=1,np
z(ip)=r(ip)/snz(0,ip)
enddo
rdotzl=0
do ip=1,np
if(associated(ipgl(iplg(ip))%next)) then
if(ipgl(iplg(ip))%next%rank<myrank) cycle
endif
rdotzl=rdotzl+r(ip)*z(ip)
enddo
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_allreduce(rdotzl,rdotz,1,rtype,MPI_SUM,comm,ierr)
#ifdef INCLUDE_TIMING
wtimer(7,2)=wtimer(7,2)+mpi_wtime()-cwtmp
#endif
itn=itn+1
if(itn==1) then
p(1:np)=z(1:np)
else
if(old_rdotz==0) call parallel_abort('JCG: old_rdotz=0')
beta=rdotz/old_rdotz
p(1:np)=z(1:np)+beta*p(1:np)
endif
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call exchange_p2d(p)
#ifdef INCLUDE_TIMING
wtimer(7,2)=wtimer(7,2)+mpi_wtime()-cwtmp
#endif
alphal=0
do ip=1,np
sp(ip)=snz(0,ip)*p(ip)
do j=1,nnz(ip)
sp(ip)=sp(ip)+snz(j,ip)*p(inz(j,ip))
enddo
if(associated(ipgl(iplg(ip))%next)) then
if(ipgl(iplg(ip))%next%rank<myrank) cycle
endif
alphal=alphal+p(ip)*sp(ip)
enddo
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_allreduce(alphal,alpha,1,rtype,MPI_SUM,comm,ierr)
#ifdef INCLUDE_TIMING
wtimer(7,2)=wtimer(7,2)+mpi_wtime()-cwtmp
#endif
if(alpha==0) call parallel_abort('JCG: division by zero')
alpha=rdotz/alpha
x=x+alpha*p
r=r-alpha*sp
old_rdotz=rdotz
rdotrl=0
do ip=1,np
if(associated(ipgl(iplg(ip))%next)) then
if(ipgl(iplg(ip))%next%rank<myrank) cycle
endif
rdotrl=rdotrl+r(ip)*r(ip)
enddo
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_allreduce(rdotrl,rdotr,1,rtype,MPI_SUM,comm,ierr)
#ifdef INCLUDE_TIMING
wtimer(7,2)=wtimer(7,2)+mpi_wtime()-cwtmp
#endif
if(mod(itn,moitn)==0.and.myrank==0) write(33,'(a,i6,2e14.6)') &
'Itn, 2Norm, Rnorm: ',itn,sqrt(rdotr),sqrt(rdotr/rdotr0)
enddo
end subroutine solve_jcg
subroutine solve_jcg_qnon(itime,moitn,mxitn,rtol,nvrt1,mnei1,np1,npa1,ihydro,qmatr,qhat,qir)
#ifdef USE_MPIMODULE
use mpi
#endif
use elfe_glbl
use elfe_msgp
implicit none
#ifndef USE_MPIMODULE
include 'mpif.h'
#endif
integer,intent(in) :: itime
integer,intent(in) :: moitn,mxitn
integer,intent(in) :: nvrt1,mnei1,np1,npa1
integer,intent(in) :: ihydro(npa1)
real(rkind),intent(in) :: rtol
real(rkind),intent(in) :: qmatr(nvrt1,-1:1,0:(mnei1+1),np1)
real(rkind),intent(out) :: qhat(nvrt1,npa1)
real(rkind),intent(in) :: qir(nvrt1,np1)
integer :: itn,ip,jp,j,k,kin,l,nd,ndim
real(rkind) :: rdotrl,rdotr,rdotzl,rdotz,old_rdotz,beta,alphal,alpha,cwtmp,tmp,threshold_rat
real(rkind) :: rtol2,rdotr0,dx_min2,dz_max2,dist2,rat_max2,rat_max2_gb
real(rkind) :: zz(nvrt1,np1),rr(nvrt1,np1),pp(nvrt1,npa1),sp(nvrt1,np1)
real(rkind) :: alow(nvrt1),bdia(nvrt1),cupp(nvrt1),gam(nvrt1),soln(nvrt1,nvrt1),rrhs(nvrt1,nvrt1)
real(rkind) :: blockj(nvrt1,nvrt1,np)
logical :: lhbc(npa1)
logical :: large_rat(np1)
do ip=1,npa
lhbc(ip)=idry(ip)==1.or.ihydro(ip)==1
enddo
rat_max2=-1
threshold_rat=0.4
large_rat(:)=.false.
do ip=1,np
if(lhbc(ip)) cycle
dx_min2=1.e25
do j=1,nnp(ip)
nd=inp(ip,j)
dist2=(xnd(ip)-xnd(nd))**2+(ynd(ip)-ynd(nd))**2
dx_min2=min(dx_min2,dist2)
enddo
dz_max2=-1
do k=kbp(ip),nvrt-1
dist2=(znl(k+1,ip)-znl(k,ip))**2
dz_max2=max(dz_max2,dist2)
enddo
if(dx_min2<=0) call parallel_abort('CG2: dx_min2<=0')
tmp=dz_max2/dx_min2
rat_max2=max(rat_max2,tmp)
if(sqrt(tmp)>=threshold_rat) large_rat(ip)=.true.
if(large_rat(ip)) cycle
ndim=nvrt-kbp_e(ip)
do k=kbp_e(ip),nvrt-1
kin=k-kbp_e(ip)+1
if(k/=kbp_e(ip)) alow(kin)=qmatr(k,-1,0,ip)
bdia(kin)=qmatr(k,0,0,ip)
cupp(kin)=qmatr(k,1,0,ip)
enddo
rrhs=0
do l=1,ndim
rrhs(l,l)=1
enddo
call tridag(nvrt,nvrt,ndim,ndim,alow,bdia,cupp,rrhs,soln,gam)
blockj(kbp_e(ip):(nvrt-1),kbp_e(ip):(nvrt-1),ip)=soln(1:ndim,1:ndim)
do k=kbp_e(ip),nvrt-1
do l=kbp_e(ip),nvrt-1
if(abs(blockj(k,l,ip)-blockj(l,k,ip))>1.e-4) then
write(errmsg,*)'Not symmetric:',iplg(ip),k,l,blockj(k,l,ip)-blockj(l,k,ip)
call parallel_abort(errmsg)
endif
enddo
enddo
enddo
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_allreduce(rat_max2,rat_max2_gb,1,rtype,MPI_MAX,comm,ierr)
if(myrank==0) then
write(29,'(//a,i8)') '********CG2 Solve at timestep ',itime
write(29,*)'done pre-conditioner'
if(rat_max2_gb>0) write(16,*)'Max. vertical/horizontal ratio and threshold=',sqrt(rat_max2_gb),threshold_rat
endif
#ifdef INCLUDE_TIMING
wtimer(7,2)=wtimer(7,2)+mpi_wtime()-cwtmp
#endif
qhat=0
rdotrl=0
rr=0
do ip=1,np
if(lhbc(ip)) cycle
do k=kbp_e(ip),nvrt-1
if(qmatr(k,0,0,ip)<=0) call parallel_abort('JCG2: zero diagonal')
rr(k,ip)=qir(k,ip)
if(associated(ipgl(iplg(ip))%next)) then
if(ipgl(iplg(ip))%next%rank<myrank) cycle
endif
rdotrl=rdotrl+rr(k,ip)*rr(k,ip)
enddo
enddo
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_allreduce(rdotrl,rdotr,1,rtype,MPI_SUM,comm,ierr)
#ifdef INCLUDE_TIMING
wtimer(7,2)=wtimer(7,2)+mpi_wtime()-cwtmp
#endif
rtol2=rtol*rtol
rdotr0=rdotr
if(rdotr0<0) call parallel_abort('JCG2: 0 initial error')
if(rdotr0==0) return
itn=0
if(myrank==0) then
write(29,'(a,i6,3e14.6)')'Itn, 2Norm, Rnorm, tol2: ',itn,rdotr,rdotr/rdotr0,rtol2
endif
do
if(rdotr<=rtol2*rdotr0) then
if(myrank==0) write(29,*)'JCG2 converged in ',itn,' iterations'
exit
endif
if(itn>=mxitn) then
if(myrank==0) write(29,*)'JCG2 did not converge in ',mxitn,' iterations'
exit
endif
zz=0
do ip=1,np
if(lhbc(ip)) cycle
do k=kbp_e(ip),nvrt-1
if(large_rat(ip)) then
zz(k,ip)=rr(k,ip)/qmatr(k,0,0,ip)
else
do l=kbp_e(ip),nvrt-1
zz(k,ip)=zz(k,ip)+blockj(k,l,ip)*rr(l,ip)
enddo
endif
enddo
enddo
rdotzl=0
do ip=1,np
if(lhbc(ip)) cycle
if(associated(ipgl(iplg(ip))%next)) then
if(ipgl(iplg(ip))%next%rank<myrank) cycle
endif
do k=kbp_e(ip),nvrt-1
rdotzl=rdotzl+rr(k,ip)*zz(k,ip)
enddo
enddo
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_allreduce(rdotzl,rdotz,1,rtype,MPI_SUM,comm,ierr)
#ifdef INCLUDE_TIMING
wtimer(7,2)=wtimer(7,2)+mpi_wtime()-cwtmp
#endif
itn=itn+1
if(itn==1) then
pp(:,1:np)=zz(:,1:np)
else
if(old_rdotz==0) call parallel_abort('JCG: old_rdotz=0')
beta=rdotz/old_rdotz
pp(:,1:np)=zz(:,1:np)+beta*pp(:,1:np)
endif
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call exchange_p3dw(pp)
#ifdef INCLUDE_TIMING
wtimer(7,2)=wtimer(7,2)+mpi_wtime()-cwtmp
#endif
alphal=0
sp=0
do ip=1,np
if(lhbc(ip)) cycle
do k=kbp_e(ip),nvrt-1
do l=-1,1
if(k+l<kbp_e(ip).or.k+l>=nvrt) cycle
do j=0,nnp(ip)
if(j==0) then
nd=ip
else
nd=inp(ip,j)
endif
if(lhbc(nd).or.k+l==nvrt) cycle
sp(k,ip)=sp(k,ip)+qmatr(k,l,j,ip)*pp(k+l,nd)
enddo
enddo
if(associated(ipgl(iplg(ip))%next)) then
if(ipgl(iplg(ip))%next%rank<myrank) cycle
endif
alphal=alphal+pp(k,ip)*sp(k,ip)
enddo
enddo
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_allreduce(alphal,alpha,1,rtype,MPI_SUM,comm,ierr)
#ifdef INCLUDE_TIMING
wtimer(7,2)=wtimer(7,2)+mpi_wtime()-cwtmp
#endif
if(alpha==0) call parallel_abort('JCG2: division by zero')
alpha=rdotz/alpha
qhat=qhat+alpha*pp
rr=rr-alpha*sp
old_rdotz=rdotz
rdotrl=0
do ip=1,np
if(lhbc(ip)) cycle
if(associated(ipgl(iplg(ip))%next)) then
if(ipgl(iplg(ip))%next%rank<myrank) cycle
endif
do k=kbp_e(ip),nvrt-1
rdotrl=rdotrl+rr(k,ip)*rr(k,ip)
enddo
enddo
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_allreduce(rdotrl,rdotr,1,rtype,MPI_SUM,comm,ierr)
#ifdef INCLUDE_TIMING
wtimer(7,2)=wtimer(7,2)+mpi_wtime()-cwtmp
#endif
if(mod(itn,moitn)==0.and.myrank==0) write(29,'(a,i6,3e14.6)') &
'Itn, 2Norm, Rnorm, tol2: ',itn,rdotr,rdotr/rdotr0,rtol2
enddo
end subroutine solve_jcg_qnon
subroutine tridag(nmax,nvec,n,nc,a,b,c,r,u,gam)
use elfe_glbl, only : rkind,errmsg
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
integer, intent(in) :: nmax,nvec,n,nc
real(rkind), dimension(nmax), intent(in) :: a,b,c
real(rkind), dimension(nmax,nvec), intent(in) :: r
real(rkind), dimension(nmax), intent(out) :: gam
real(rkind), dimension(nmax,nvec), intent(out) :: u
if(n<1) call parallel_abort('TRIDAG: n must be >= 1')
if(nc>nvec) call parallel_abort('TRIDAG: Increase # of columns')
if(b(1)==0d0) call parallel_abort('TRIDAG: b(1)=0')
bet=b(1)
do i=1,nc
u(1,i)=r(1,i)/bet
enddo
do j=2,n
gam(j)=c(j-1)/bet
bet=b(j)-a(j)*gam(j)
if(bet.eq.0d0) call parallel_abort('TRIDAG: failed')
do i=1,nc
u(j,i)=(r(j,i)-a(j)*u(j-1,i))/bet
enddo
enddo
do j=n-1,1,-1
do i=1,nc
u(j,i)=u(j,i)-gam(j+1)*u(j+1,i)
enddo
enddo
end subroutine tridag
