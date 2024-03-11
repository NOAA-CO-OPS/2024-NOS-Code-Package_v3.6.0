subroutine do_transport_tvd(it,imod,up_tvd,tvd_mid,flimiter,ntr,ifltype, &
&itetype,isatype,itrtype,tobc,sobc,trobc,difnum_max_l)
#ifdef USE_MPIMODULE
use mpi
#endif
use elfe_glbl
use elfe_msgp
implicit real(rkind)(a-h,o-z),integer(i-n)
#ifndef USE_MPIMODULE
include 'mpif.h'
#endif
integer, intent(in) :: it
integer, intent(in) :: imod
logical, intent(in) :: up_tvd
character(len=2), intent(in) :: tvd_mid,flimiter
integer, intent(in) :: ntr
integer, intent(in) :: ifltype(max(1,nope_global)),itetype(max(1,nope_global)), &
&isatype(max(1,nope_global)),itrtype(max(1,nope_global))
real(rkind), intent(in) :: tobc(nope_global),sobc(nope_global),trobc(nope_global)
real(rkind), intent(out) :: difnum_max_l
real(rkind), allocatable :: trel_tmp(:,:,:)
real(rkind), allocatable :: flx_adv(:,:,:)
real(rkind), allocatable :: flx_mod(:,:,:,:)
real(rkind), allocatable :: up_rat(:,:,:,:)
real(rkind), allocatable :: swild2(:,:)
real(rkind), allocatable :: swild3(:,:,:)
dimension sne(nvrt,3),area_e(nvrt),psumtr(ntr),delta_tr(ntr),adv_tr(ntr), &
&alow(nvrt),bdia(nvrt),cupp(nvrt),rrhs(nvrt,ntr),soln(nvrt,ntr),gam(nvrt), &
&swild(max(3,nvrt)),swild4(3,2),trel_tmp_outside(ntr),nwild(2)
logical, save :: first_call
allocate(trel_tmp(ntr,nvrt,nea),flx_adv(2,nvrt,nsa),flx_mod(ntr,nvrt,2,ns), &
up_rat(ntr,nvrt,2,nsa),stat=istat)
if(istat/=0) call parallel_abort('Transport: fail to allocate')
flx_adv=-1.e34
do j=1,ns
if(idry_s(j)==1) cycle
do k=kbs(j)+1,nvrt
if(is(j,2)==0.and.isbs(j)<=0) then
flx_adv(1,k,j)=0
cycle
endif
if(ics==1) then
vnor1=su2(k,j)*sframe(1,1,j)+sv2(k,j)*sframe(2,1,j)
vnor2=su2(k-1,j)*sframe(1,1,j)+sv2(k-1,j)*sframe(2,1,j)
else
vnor1=su2(k,j)
vnor2=su2(k-1,j)
endif
flx_adv(1,k,j)=(zs(k,j)-zs(k-1,j))*distj(j)*(vnor1+vnor2)/2
enddo
khh2=0
do l=1,2
ie=is(j,l)
if(ie/=0.and.idry_e(max(1,ie))==0.and.kbe(max(1,ie))>khh2) khh2=kbe(ie)
enddo
if(khh2==0) then
write(errmsg,*)'Transport: cannot find the higher bottom:',j,ielg(is(j,1:2)),is(j,1:2)
call parallel_abort(errmsg)
endif
if(kbs(j)>khh2) then
write(errmsg,*)'Transport: side index > elemnt:',kbs(j),khh2
call parallel_abort(errmsg)
endif
do k=kbs(j)+1,khh2-1
if(flx_adv(1,k,j)/=0) then
write(errmsg,*)'Transport: Non-zero hvel below element bottom:',k,ielg(is(j,1:2)),flx_adv(1,k,j)
call parallel_abort(errmsg)
endif
enddo
enddo
do i=1,ne
if(idry_e(i)==1) cycle
n1=nm(i,1)
n2=nm(i,2)
n3=nm(i,3)
isd1=js(i,1)
isd2=js(i,2)
isd3=js(i,3)
if(kbe(i)==0) then
write(errmsg,*)'Transport: Impossible 95 (2)'
call parallel_abort(errmsg)
endif
do l=kbe(i),nvrt
if(ics==1) then
xcon=(ynd(n2)-ynd(n1))*(znl(l,n3)-znl(l,n1))-(ynd(n3)-ynd(n1))*(znl(l,n2)-znl(l,n1))
ycon=(xnd(n3)-xnd(n1))*(znl(l,n2)-znl(l,n1))-(xnd(n2)-xnd(n1))*(znl(l,n3)-znl(l,n1))
zcon=area(i)*2
else
call cross_product(xel(2,i)-xel(1,i),yel(2,i)-yel(1,i),znl(l,n2)-znl(l,n1), &
& xel(3,i)-xel(1,i),yel(3,i)-yel(1,i),znl(l,n3)-znl(l,n1), &
& xcon,ycon,zcon)
endif
area_e(l)=sqrt(xcon**2+ycon**2+zcon**2)/2
if(area_e(l)==0) then
write(errmsg,*)'Transport: Zero area (2):',i,l
call parallel_abort(errmsg)
endif
sne(l,1)=xcon/area_e(l)/2
sne(l,2)=ycon/area_e(l)/2
sne(l,3)=zcon/area_e(l)/2
enddo
do k=kbe(i),nvrt
if(k==kbe(i)) then
dot1=we_fv(kbe(i),i)
else
if(ics==1) then
dot1=(su2(k,isd1)+su2(k,isd2)+su2(k,isd3))/3*sne(k,1)+ &
& (sv2(k,isd1)+sv2(k,isd2)+sv2(k,isd3))/3*sne(k,2)+we_fv(k,i)*sne(k,3)
else
do j=1,3
isd=js(i,j)
call project_hvec(su2(k,isd),sv2(k,isd),sframe(:,:,isd),eframe(:,:,i),swild4(j,1),swild4(j,2))
enddo
dot1=sum(swild4(1:3,1))/3*sne(k,1)+sum(swild4(1:3,2))/3*sne(k,2)+we_fv(k,i)*sne(k,3)
endif
endif
flx_adv(2,k,i)=dot1*area_e(k)
enddo
j0=0
do j=1,3
isd=js(i,j)
if(isbs(isd)>0.and.ifltype(max(1,isbs(isd)))==0) j0=j
enddo
do k=kbe(i)+1,nvrt
if(j0/=0) then
flx_adv(2,k,i)=0; flx_adv(2,k-1,i)=0
isd0=js(i,j0)
sum1=0
do j=1,2
isd=js(i,nx(j0,j))
sum1=sum1+ssign(i,nx(j0,j))*flx_adv(1,k,isd)
enddo
flx_adv(1,k,isd0)=-sum1/ssign(i,j0)
endif
swild(k)=flx_adv(2,k,i)-flx_adv(2,k-1,i)
do j=1,3
tmp=ssign(i,j)*flx_adv(1,k,js(i,j))
swild(k)=swild(k)+tmp
enddo
enddo
enddo
allocate(swild2(nvrt,nsa),stat=istat)
if(istat/=0) call parallel_abort('Transport: fail to allocate (2)')
swild2(:,:)=flx_adv(1,:,:)
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call exchange_s3dw(swild2)
#ifdef INCLUDE_TIMING
wtimer(9,2)=wtimer(9,2)+mpi_wtime()-cwtmp
#endif
flx_adv(1,:,:)=swild2(:,:)
swild2(:,:)=flx_adv(2,:,:)
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call exchange_e3dw(swild2)
#ifdef INCLUDE_TIMING
wtimer(9,2)=wtimer(9,2)+mpi_wtime()-cwtmp
#endif
flx_adv(2,:,:)=swild2(:,:)
deallocate(swild2)
if(up_tvd) then
allocate(swild3(ntr,nvrt,nsa),stat=istat)
if(istat/=0) call parallel_abort('Transport: fail to allocate swild3')
endif
do i=1,ntr
do j=1,2
flx_mod(i,1:nvrt,j,1:ns)=flx_adv(j,1:nvrt,1:ns)
enddo
enddo
it_sub=0
time_r=dt
difnum_max_l=0
loop11: do
it_sub=it_sub+1
if(up_tvd) then
up_rat=-1.e34
ntot_v=0
do i=1,ne
if(idry_e(i)==1) cycle
up_rat(:,:,2,i)=-1
do k=kbe(i)+1,nvrt-1
if(flx_adv(2,k,i)<-1.e33) then
write(errmsg,*)'Transport: Left out vertical flux (3):',i,k
call parallel_abort(errmsg)
else if(flx_adv(2,k,i)>0) then
kup=k
kdo=k+1
else
kup=k+1
kdo=k
endif
psum=0
psumtr(1:ntr)=0
if(flx_adv(2,kup,i)<-1.e33.or.flx_adv(2,kup-1,i)<-1.e33) then
write(errmsg,*)'Left out vertical flux (4):',i,kup
call parallel_abort(errmsg)
endif
if(flx_adv(2,kup,i)<0.and.kup/=nvrt) then
psum=psum+abs(flx_adv(2,kup,i))
psumtr(1:ntr)=psumtr(1:ntr)+abs(flx_adv(2,kup,i))*(tr_el(1:ntr,kup+1,i)-tr_el(1:ntr,kup,i))
endif
if(flx_adv(2,kup-1,i)>0.and.kup/=kbe(i)+1) then
psum=psum+abs(flx_adv(2,kup-1,i))
psumtr(1:ntr)=psumtr(1:ntr)+abs(flx_adv(2,kup-1,i))*(tr_el(1:ntr,kup-1,i)-tr_el(1:ntr,kup,i))
endif
do j=1,3
jsj=js(i,j)
ie=ic3(i,j)
if(flx_adv(1,kup,jsj)<-1.e33) then
write(errmsg,*)'Left out horizontal flux (5):',jsj,kup
call parallel_abort(errmsg)
endif
if(ie/=0.and.idry_e(max(1,ie))==0.and.ssign(i,j)*flx_adv(1,kup,jsj)<0) then
psum=psum+abs(flx_adv(1,kup,jsj))
psumtr(1:ntr)=psumtr(1:ntr)+abs(flx_adv(1,kup,jsj))*(tr_el(1:ntr,kup,ie)-tr_el(1:ntr,kup,i))
endif
enddo
if(tvd_mid.eq.'AA') then
do j=1,ntr
tmp=(tr_el(j,kup,i)-tr_el(j,kdo,i))*abs(flx_adv(2,k,i))
if(abs(tmp)>1.e-20) up_rat(j,k,2,i)=psumtr(j)/tmp
enddo
else if(tvd_mid.eq.'CC') then
do j=1,ntr
tmp=(tr_el(j,kup,i)-tr_el(j,kdo,i))*psum
if(abs(tmp)>1.e-20) up_rat(j,k,2,i)=psumtr(j)/tmp
enddo
else
write(errmsg,*)'Unknown tvd_mid:',tvd_mid
call parallel_abort(errmsg)
endif
if(flux_lim(up_rat(1,k,2,i),flimiter)>0.1) ntot_v=ntot_v+1
enddo
enddo
ntot_h=0
do i=1,ns
if(idry_s(i)==1) cycle
up_rat(:,:,1,i)=-1
if(is(i,2)==0.or.(is(i,2)/=0.and.idry_e(max(1,is(i,2)))==1).or.idry_e(is(i,1))==1) cycle
kb1=min(kbe(is(i,1)),kbe(is(i,2)))
kb=max(kbe(is(i,1)),kbe(is(i,2)))
do k=kb1+1,kb-1
if(flx_adv(1,k,i)/=0) then
write(errmsg,*)'Pls zero out the excess layers:',flx_adv(1,k,i),i,is(i,1),is(i,2),k,kb1,kb
call parallel_abort(errmsg)
endif
enddo
do k=kb+1,nvrt
if(flx_adv(1,k,i)<-1.e33) then
write(errmsg,*)'Left out horizontal flux (3):',i,k
call parallel_abort(errmsg)
else if(flx_adv(1,k,i)>0) then
iup=is(i,1); ido=is(i,2)
else
iup=is(i,2); ido=is(i,1)
endif
psum=0
psumtr(1:ntr)=0
if(flx_adv(2,k,iup)<-1.e33.or.flx_adv(2,k-1,iup)<-1.e33) then
write(errmsg,*)'Left out vertical flux (6):',iup,k
call parallel_abort(errmsg)
endif
if(flx_adv(2,k,iup)<0.and.k/=nvrt) then
psum=psum+abs(flx_adv(2,k,iup))
psumtr(1:ntr)=psumtr(1:ntr)+abs(flx_adv(2,k,iup))*(tr_el(1:ntr,k+1,iup)-tr_el(1:ntr,k,iup))
endif
if(flx_adv(2,k-1,iup)>0.and.k>kbe(iup)+1) then
psum=psum+abs(flx_adv(2,k-1,iup))
psumtr(1:ntr)=psumtr(1:ntr)+abs(flx_adv(2,k-1,iup))*(tr_el(1:ntr,k-1,iup)-tr_el(1:ntr,k,iup))
endif
do j=1,3
jsj=js(iup,j)
ie=ic3(iup,j)
if(ie<0) then
write(errmsg,*)'TVD: upwind element outside:',iplg(isidenode(i,1:2))
call parallel_abort(errmsg)
endif
if(flx_adv(1,k,jsj)<-1.e33) then
write(errmsg,*)'Left out horizontal flux (6):',jsj,k
call parallel_abort(errmsg)
endif
if(ie/=0.and.idry_e(max(1,ie))==0.and.ssign(iup,j)*flx_adv(1,k,jsj)<0) then
psum=psum+abs(flx_adv(1,k,jsj))
psumtr(1:ntr)=psumtr(1:ntr)+abs(flx_adv(1,k,jsj))*(tr_el(1:ntr,k,ie)-tr_el(1:ntr,k,iup))
endif
enddo
if(tvd_mid.eq.'AA') then
do j=1,ntr
tmp=(tr_el(j,k,iup)-tr_el(j,k,ido))*abs(flx_adv(1,k,i))
if(abs(tmp)>1.e-20) up_rat(j,k,1,i)=psumtr(j)/tmp
enddo
else
do j=1,ntr
tmp=(tr_el(j,k,iup)-tr_el(j,k,ido))*psum
if(abs(tmp)>1.e-20) up_rat(j,k,1,i)=psumtr(j)/tmp
enddo
endif
if(flux_lim(up_rat(1,k,1,i),flimiter)>0.1) ntot_h=ntot_h+1
enddo
enddo
if(ntr==2) then
swild3(:,:,:)=up_rat(:,:,1,:)
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call exchange_s3d_2(swild3)
#ifdef INCLUDE_TIMING
wtimer(9,2)=wtimer(9,2)+mpi_wtime()-cwtmp
#endif
up_rat(:,:,1,:)=swild3(:,:,:)
swild3(:,:,:)=up_rat(:,:,2,:)
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call exchange_e3d_2(swild3)
#ifdef INCLUDE_TIMING
wtimer(9,2)=wtimer(9,2)+mpi_wtime()-cwtmp
#endif
up_rat(:,:,2,:)=swild3(:,:,:)
else if(ntr==ntracers) then
swild3(:,:,:)=up_rat(:,:,1,:)
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call exchange_s3d_tr2(swild3)
#ifdef INCLUDE_TIMING
wtimer(9,2)=wtimer(9,2)+mpi_wtime()-cwtmp
#endif
up_rat(:,:,1,:)=swild3(:,:,:)
swild3(:,:,:)=up_rat(:,:,2,:)
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call exchange_e3d_tr2(swild3)
#ifdef INCLUDE_TIMING
wtimer(9,2)=wtimer(9,2)+mpi_wtime()-cwtmp
#endif
up_rat(:,:,2,:)=swild3(:,:,:)
else
call parallel_abort('Transport: unknown tracer number')
endif
do i=1,ne
if(idry_e(i)==1) cycle
do k=kbe(i)+1,nvrt-1
if(flx_adv(2,k,i)>0) then
kup=k
else
kup=k+1
endif
delta_tr(1:ntr)=0
do l=0,1
if(flx_adv(2,kup-l,i)*(1-2*l)>0) then
do j=1,ntr
rat=up_rat(j,kup-l,2,i)
if(rat<-1.e33) then
write(errmsg,*)'Left out (1):',i,kup-l,rat,it_sub,j
call parallel_abort(errmsg)
else if(rat/=0) then
tmp=flux_lim(rat,flimiter)/rat/2
if(tmp<0.or.tmp>1) then
write(errmsg,*)'Flux limiting failed (1):',tmp,rat,flx_adv(2,kup-l,i),l,kup
call parallel_abort(errmsg)
endif
delta_tr(j)=delta_tr(j)+tmp
endif
enddo
endif
enddo
do j=1,3
jsj=js(i,j)
ie=ic3(i,j)
if(ssign(i,j)*flx_adv(1,kup,jsj)>0) then
do jj=1,ntr
rat=up_rat(jj,kup,1,jsj)
if(rat<-1.e33) then
write(errmsg,*)'Left out (3):',i,j,kup,rat,jj
call parallel_abort(errmsg)
else if(rat/=0) then
tmp=flux_lim(rat,flimiter)/rat/2
if(tmp<0.or.tmp>1) then
write(errmsg,*)'Flux limiting failed (3):',tmp,rat,jj
call parallel_abort(errmsg)
endif
delta_tr(jj)=delta_tr(jj)+tmp
endif
enddo
endif
enddo
do j=1,ntr
flx_mod(j,k,2,i)=flx_adv(2,k,i)*(1-flux_lim(up_rat(j,k,2,i),flimiter)/2+delta_tr(j))
enddo
enddo
enddo
do i=1,ns
if(idry_s(i)==1.or.is(i,2)==0.or.idry_e(is(i,1))==1) cycle
if(idry_e(is(i,2))==1) cycle
kb=max(kbe(is(i,1)),kbe(is(i,2)))
do k=kb+1,nvrt
if(flx_adv(1,k,i)>0) then
iup=is(i,1)
else
iup=is(i,2)
endif
delta_tr(1:ntr)=0
do l=0,1
if(flx_adv(2,k-l,iup)*(1-2*l)>0) then
do j=1,ntr
rat=up_rat(j,k-l,2,iup)
if(rat<-1.e33) then
write(errmsg,*)'Left out (5):',iup,k-l,rat,j
call parallel_abort(errmsg)
else if(rat/=0) then
tmp=flux_lim(rat,flimiter)/rat/2
if(tmp<0.or.tmp>1) then
write(errmsg,*)'Flux limiting failed (5):',tmp,rat,j
call parallel_abort(errmsg)
endif
delta_tr(j)=delta_tr(j)+tmp
endif
enddo
endif
enddo
do j=1,3
jsj=js(iup,j)
ie=ic3(iup,j)
if(ssign(iup,j)*flx_adv(1,k,jsj)>0) then
do jj=1,ntr
rat=up_rat(jj,k,1,jsj)
if(rat<-1.e33) then
write(errmsg,*)'Left out (7):',iup,ielg(ie),k,rat,jj
call parallel_abort(errmsg)
else if(rat/=0) then
tmp=flux_lim(rat,flimiter)/rat/2
if(tmp<0.or.tmp>1) then
write(errmsg,*)'Flux limiting failed (7):',tmp,rat,jj
call parallel_abort(errmsg)
endif
delta_tr(jj)=delta_tr(jj)+tmp
endif
enddo
endif
enddo
do j=1,ntr
flx_mod(j,k,1,i)=flx_adv(1,k,i)*(1-flux_lim(up_rat(j,k,1,i),flimiter)/2+delta_tr(j))
enddo
enddo
enddo
endif
if(up_tvd.or.it_sub==1) then
dtbl=time_r
ie01=0
lev01=0
in_st=0
do i=1,ne
if(idry_e(i)==1) cycle
do k=kbe(i)+1,nvrt
psumtr(1:ntr)=0
if(up_tvd) then
if(k/=nvrt.and.flx_mod(1,k,2,i)<0) then
psumtr(1:ntr)=psumtr(1:ntr)+abs(flx_mod(1:ntr,k,2,i))
endif
if(k-1/=kbe(i).and.flx_mod(1,k-1,2,i)>0) then
psumtr(1:ntr)=psumtr(1:ntr)+abs(flx_mod(1:ntr,k-1,2,i))
endif
endif
do j=1,3
jsj=js(i,j)
ie=ic3(i,j)
do jj=1,ntr
if(flx_mod(jj,k,1,jsj)<-1.e33) then
write(errmsg,*)'Left out horizontal flux (10):',i,k,j,jj
call parallel_abort(errmsg)
endif
enddo
do jj=1,ntr
if(ie/=0.and.idry_e(max(1,ie))==0.or.ie==0.and.isbs(jsj)>0) then
if(ssign(i,j)*flx_mod(1,k,1,jsj)<0) then
psumtr(jj)=psumtr(jj)+abs(flx_mod(jj,k,1,jsj))
endif
endif
enddo
enddo
vj=area(i)*(ze(k,i)-ze(k-1,i))
do jj=1,ntr
if(psumtr(jj)/=0) then
tmp=vj/psumtr(jj)*(1-1.e-6)
if(tmp<dtbl) then
dtbl=tmp
ie01=i; lev01=k; in_st=jj
endif
endif
enddo
enddo
enddo
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_allreduce(dtbl,dtb,1,rtype,MPI_MIN,comm,ierr)
#ifdef INCLUDE_TIMING
wtimer(9,2)=wtimer(9,2)+mpi_wtime()-cwtmp
#endif
if(dtb<=0.or.dtb>time_r) then
write(errmsg,*)'Transport: Illegal sub step:',dtb,time_r
call parallel_abort(errmsg)
endif
if(up_tvd.and.myrank==0) write(18,*)it,it_sub,dtb
endif
dtb=min(dtb,time_r)
time_r=time_r-dtb
trel_tmp(1:ntr,:,:)=tr_el(1:ntr,:,:)
do i=1,ne
if(idry_e(i)==1) cycle
n1=nm(i,1)
n2=nm(i,2)
n3=nm(i,3)
ndim=nvrt-kbe(i)
do k=kbe(i)+1,nvrt
kin=k-kbe(i)
alow(kin)=0
cupp(kin)=0
bigv=area(i)*(ze(k,i)-ze(k-1,i))
if(bigv<=0) then
write(errmsg,*)'Negative volume: ',bigv,i,k
call parallel_abort(errmsg)
endif
bdia(kin)=1
if(k<nvrt) then
av_df=(dfh(n1,k)+dfh(n2,k)+dfh(n3,k))/3
av_dz=(ze(k+1,i)-ze(k-1,i))/2
if(av_dz<=0) then
write(errmsg,*)'Impossible 111'
call parallel_abort(errmsg)
endif
tmp=area(i)*dtb*av_df/av_dz/bigv
cupp(kin)=cupp(kin)-tmp
bdia(kin)=bdia(kin)+tmp
endif
if(k>kbe(i)+1) then
av_df=(dfh(n1,k-1)+dfh(n2,k-1)+dfh(n3,k-1))/3
av_dz=(ze(k,i)-ze(k-2,i))/2
if(av_dz<=0) then
write(errmsg,*)'Impossible 112'
call parallel_abort(errmsg)
endif
tmp=area(i)*dtb*av_df/av_dz/bigv
alow(kin)=alow(kin)-tmp
bdia(kin)=bdia(kin)+tmp
endif
psumtr(1:ntr)=0
adv_tr(1:ntr)=trel_tmp(1:ntr,k,i)
if(ntr>1.and.flx_mod(1,k,2,i)*flx_mod(2,k,2,i)<0) then
write(errmsg,*)'Left out vertical flux (0):',i,k,flx_mod(1:2,k,2,i)
call parallel_abort(errmsg)
endif
do jj=1,ntr
if(flx_mod(jj,k,2,i)<-1.e33) then
write(errmsg,*)'Left out vertical flux:',i,k,flx_mod(jj,k,2,i),jj
call parallel_abort(errmsg)
endif
enddo
if(k/=nvrt.and.flx_mod(1,k,2,i)<0) then
if(up_tvd) then
do jj=1,ntr
psumtr(jj)=psumtr(jj)+abs(flx_mod(jj,k,2,i))
adv_tr(jj)=adv_tr(jj)+dtb/bigv*abs(flx_adv(2,k,i))*(trel_tmp(jj,k+1,i)-trel_tmp(jj,k,i))
enddo
else
tmp=abs(flx_mod(1,k,2,i))*dtb/bigv
cupp(kin)=cupp(kin)-tmp
bdia(kin)=bdia(kin)+tmp
endif
endif
if(k-1/=kbe(i).and.flx_mod(1,k-1,2,i)>0) then
if(up_tvd) then
do jj=1,ntr
psumtr(jj)=psumtr(jj)+abs(flx_mod(jj,k-1,2,i))
adv_tr(jj)=adv_tr(jj)+dtb/bigv*abs(flx_adv(2,k-1,i))*(trel_tmp(jj,k-1,i)-trel_tmp(jj,k,i))
enddo
else
tmp=abs(flx_mod(1,k-1,2,i))*dtb/bigv
alow(kin)=alow(kin)-tmp
bdia(kin)=bdia(kin)+tmp
endif
endif
if(up_tvd) then
if(k/=nvrt) then
do jj=1,ntr
adv_tr(jj)=adv_tr(jj)+dtb/bigv*abs(flx_adv(2,k,i))*(trel_tmp(jj,k,i)-trel_tmp(jj,k+1,i))* &
&flux_lim(up_rat(jj,k,2,i),flimiter)/2
enddo
endif
if(k-1/=kbe(i)) then
do jj=1,ntr
adv_tr(jj)=adv_tr(jj)+dtb/bigv*abs(flx_adv(2,k-1,i))*(trel_tmp(jj,k,i)-trel_tmp(jj,k-1,i))* &
&flux_lim(up_rat(jj,k-1,2,i),flimiter)/2
enddo
endif
endif
do j=1,3
jsj=js(i,j)
iel=ic3(i,j)
if(iel/=0) then
if(idry_e(iel)==1) cycle
trel_tmp_outside(:)=trel_tmp(:,k,iel)
else
if(isbs(jsj)<=0.or.ssign(i,j)*flx_mod(1,k,1,jsj)>=0) cycle
ibnd=isbs(jsj)
nwild(1:2)=0
do ll=1,2
ndo=isidenode(jsj,ll)
do lll=1,2
if(isbnd(lll,ndo)==ibnd) then
nwild(ll)=isbnd(-lll,ndo)
exit
endif
enddo
enddo
ind1=nwild(1); ind2=nwild(2);
if(ind1==0.or.ind2==0) then
write(errmsg,*)'Cannot find a local index'
call parallel_abort(errmsg)
endif
if(imod==0) then
if(itetype(ibnd)==0) then
trel_tmp_outside(1)=trel_tmp(1,k,i)
else if(itetype(ibnd)==1.or.itetype(ibnd)==2) then
trel_tmp_outside(1)=tobc(ibnd)*tth(ibnd,1,1)+(1-tobc(ibnd))*trel_tmp(1,k,i)
else if(itetype(ibnd)==3) then
tmp=(tem0(k,isidenode(jsj,1))+tem0(k-1,isidenode(jsj,2)))/2
trel_tmp_outside(1)=tobc(ibnd)*tmp+(1-tobc(ibnd))*trel_tmp(1,k,i)
else if(itetype(ibnd)==4) then
tmp=(tth(ibnd,ind1,k)+tth(ibnd,ind1,k-1)+tth(ibnd,ind2,k)+tth(ibnd,ind2,k-1))/4
trel_tmp_outside(1)=tobc(ibnd)*tmp+(1-tobc(ibnd))*trel_tmp(1,k,i)
else
write(errmsg,*)'TRASNPORT: INVALID VALUE FOR ITETYPE'
call parallel_abort(errmsg)
endif
if(isatype(ibnd)==0) then
trel_tmp_outside(2)=trel_tmp(2,k,i)
else if(isatype(ibnd)==1.or.isatype(ibnd)==2) then
trel_tmp_outside(2)=sobc(ibnd)*sth(ibnd,1,1)+(1-sobc(ibnd))*trel_tmp(2,k,i)
else if(isatype(ibnd)==3) then
tmp=(sal0(k,isidenode(jsj,1))+sal0(k-1,isidenode(jsj,2)))/2
trel_tmp_outside(2)=sobc(ibnd)*tmp+(1-sobc(ibnd))*trel_tmp(2,k,i)
else if(isatype(ibnd)==4) then
tmp=(sth(ibnd,ind1,k)+sth(ibnd,ind1,k-1)+sth(ibnd,ind2,k)+sth(ibnd,ind2,k-1))/4
trel_tmp_outside(2)=sobc(ibnd)*tmp+(1-sobc(ibnd))*trel_tmp(2,k,i)
else
write(errmsg,*)'TRASNPORT: INVALID VALUE FOR ISATYPE'
call parallel_abort(errmsg)
endif
else
if(itrtype(ibnd)==0) then
trel_tmp_outside(:)=trel_tmp(:,k,i)
else if(itrtype(ibnd)==2) then
trel_tmp_outside(:)=trobc(ibnd)*trth(:,ibnd)+(1-trobc(ibnd))*trel_tmp(:,k,i)
else if(itrtype(ibnd)==3) then
trel_tmp_outside(:)=trobc(ibnd)*trel0(:,k,i)+(1-trobc(ibnd))*trel_tmp(:,k,i)
else
write(errmsg,*)'TRASNPORT: INVALID VALUE FOR ITRTYPE'
call parallel_abort(errmsg)
endif
endif
endif
if(ntr>1.and.flx_mod(1,k,1,jsj)*flx_mod(2,k,1,jsj)<0) then
write(errmsg,*)'Left out horizontal flux (0):',i,j,k,flx_mod(1:2,k,1,jsj)
call parallel_abort(errmsg)
endif
do jj=1,ntr
if(flx_mod(jj,k,1,jsj)<-1.e33) then
write(errmsg,*)'Left out horizontal flux:',i,j,k,flx_mod(jj,k,1,jsj),jj
call parallel_abort(errmsg)
endif
enddo
if(ssign(i,j)*flx_mod(1,k,1,jsj)<0) then
do jj=1,ntr
psumtr(jj)=psumtr(jj)+abs(flx_mod(jj,k,1,jsj))
adv_tr(jj)=adv_tr(jj)+dtb/bigv*abs(flx_adv(1,k,jsj))*(trel_tmp_outside(jj)-trel_tmp(jj,k,i))
enddo
endif
if(up_tvd) then
do jj=1,ntr
adv_tr(jj)=adv_tr(jj)+dtb/bigv*abs(flx_adv(1,k,jsj))*(trel_tmp(jj,k,i)-trel_tmp_outside(jj))* &
&flux_lim(up_rat(jj,k,1,jsj),flimiter)/2
enddo
endif
enddo
do jj=1,ntr
if(1-dtb/bigv*psumtr(jj)<0) then
write(errmsg,*)'Courant # condition violated:',i,k,1-dtb/bigv*psumtr(jj),jj
call parallel_abort(errmsg)
endif
enddo
rrhs(kin,1:ntr)=adv_tr(1:ntr)
rrhs(kin,1:ntr)=rrhs(kin,1:ntr)+dtb*bdy_frc(1:ntr,k,i)
if(ihdif/=0) then
do j=1,3
jsj=js(i,j)
iel=ic3(i,j)
if(iel==0.or.idry_e(max(1,iel))==1) cycle
nd1=isidenode(jsj,1)
nd2=isidenode(jsj,2)
hdif_tmp=(hdif(k,nd1)+hdif(k,nd2)+hdif(k-1,nd1)+hdif(k-1,nd2))/4
av_h=(znl(k,nd1)-znl(k-1,nd1)+znl(k,nd2)-znl(k-1,nd2))/2
if(av_h<=0) call parallel_abort('TRANSPORT: Height<=0')
difnum=dtb/bigv*hdif_tmp/delj(jsj)*av_h*distj(jsj)
if(difnum>difnum_max_l) difnum_max_l=difnum
rrhs(kin,1:ntr)=rrhs(kin,1:ntr)+difnum*(trel_tmp(1:ntr,k,iel)-trel_tmp(1:ntr,k,i))
enddo
endif
if(k==nvrt) rrhs(kin,1:ntr)=rrhs(kin,1:ntr)+area(i)*dtb*flx_sf(1:ntr,i)/bigv
if(k==kbe(i)+1) rrhs(kin,1:ntr)=rrhs(kin,1:ntr)-area(i)*dtb*flx_bt(1:ntr,i)/bigv
enddo
call tridag(nvrt,ntr,ndim,ntr,alow,bdia,cupp,rrhs,soln,gam)
do k=kbe(i)+1,nvrt
kin=k-kbe(i)
if(imod==0) then
tr_el(1:2,k,i)=soln(kin,1:2)
if(ihconsv/=0) tr_el(1,k,i)=max(tempmin,min(tempmax,soln(kin,1)))
if(isconsv/=0) tr_el(2,k,i)=max(saltmin,min(saltmax,soln(kin,2)))
else
# ifdef USE_NAPZD
do ibio=1,ntr
tr_el(ibio,k,i)=max(soln(kin,ibio),0.0)
Bio_bdef(k,i)=Bio_bdef(k,i)+tr_el(ibio,k,i)-soln(kin,ibio)
enddo
# else
tr_el(1:ntr,k,i)=soln(kin,1:ntr)
# endif
endif
enddo
do k=1,kbe(i)
tr_el(1:ntr,k,i)=tr_el(1:ntr,kbe(i)+1,i)
enddo
enddo
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call exchange_e3d_tr(tr_el)
#ifdef INCLUDE_TIMING
wtimer(9,2)=wtimer(9,2)+mpi_wtime()-cwtmp
#endif
if(time_r<1.e-8) exit loop11
end do loop11
if(difnum_max_l>0.5) write(12,*)'Transport: diffusion # exceeds 0.5:',it,imod,difnum_max_l
if(up_tvd) then
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_allreduce(ntot_h,ntot_hgb,1,itype,MPI_SUM,comm,ierr)
call mpi_allreduce(ntot_v,ntot_vgb,1,itype,MPI_SUM,comm,ierr)
#ifdef INCLUDE_TIMING
wtimer(9,2)=wtimer(9,2)+mpi_wtime()-cwtmp
#endif
if(myrank==0) &
write(16,*)'Total # of vertical and S faces limited = ',ntot_hgb,ntot_vgb
endif
if(myrank==0) write(17,*)it,it_sub
deallocate(trel_tmp,flx_adv,flx_mod,up_rat)
if(up_tvd) deallocate(swild3)
end subroutine do_transport_tvd
function flux_lim(ss,flimiter)
use elfe_glbl, only : rkind,errmsg
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
character(len=2) :: flimiter
if(flimiter.eq.'SB') then
flux_lim=max(0.d0,min(1.d0,2*ss),min(2.d0,ss))
else if(flimiter.eq.'MM') then
flux_lim=max(0.d0,min(1.d0,ss))
else if(flimiter.eq.'OS') then
flux_lim=max(0.d0,min(2.d0,ss))
else if(flimiter.eq.'VL') then
flux_lim=(ss+abs(ss))/(1+abs(ss))
else
write(errmsg,*)'flux_lim: Unknown limiter:',flimiter
call parallel_abort(errmsg)
endif
end function flux_lim
