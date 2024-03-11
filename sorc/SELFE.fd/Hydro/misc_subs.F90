subroutine levels1(iths,it)
#ifdef USE_MPIMODULE
use mpi
#endif
use elfe_glbl
use elfe_msgp
implicit real(rkind)(a-h,o-z),integer(i-n)
#ifndef USE_MPIMODULE
include 'mpif.h'
#endif
integer, intent(in) :: iths,it
dimension idry2(npa),idry_s2(nsa),idry_e2(nea),out2(12+nvrt),sutmp(nvrt),svtmp(nvrt)
dimension isdf(nsa),inew(nsa),icolor(npa),icolor2(nsa),swild2(2,nvrt)
real(rkind),allocatable :: swild(:,:,:)
logical :: srwt_xchng,prwt_xchng
logical :: srwt_xchng_gb,prwt_xchng_gb
logical :: cwtime,lmorph
cwtime=it/=iths
if(it==iths) then
idry_e=0
do i=1,nea
do j=1,3
nd=nm(i,j)
if(eta2(nd)+dp(nd)<=h0) then
idry_e(i)=1
exit
endif
enddo
enddo
endif
idry_e2=idry_e
if(it/=iths) then
do i=1,np
if(dp(i)+eta2(i)<=h0) idry_e2(ine(i,1:nne(i)))=1
enddo
call exchange_e2di(idry_e2)
istop=0
itr=0
loop15: do
itr=itr+1
if(itr>100) call parallel_abort('LEVELS1: Too many iterations in wet/dry')
icolor=0
icolor2=0
do i=1,ns
if(is(i,2)/=0) then; if(idry_e2(is(i,1))+idry_e2(is(i,2))==1) then
icolor(isidenode(i,1:2))=1
icolor2(i)=1
endif; endif
enddo
call exchange_p2di(icolor)
call exchange_s2di(icolor2)
nsdf=0
do i=1,nsa
if(icolor2(i)==1) then
nsdf=nsdf+1
isdf(nsdf)=i
endif
enddo
call mpi_allreduce(nsdf,nsdf_gb,1,itype,MPI_SUM,comm,ierr)
if(nsdf_gb==0) exit loop15
srwt_xchng=istop==1
call mpi_allreduce(srwt_xchng,srwt_xchng_gb,1,MPI_LOGICAL,MPI_LAND,comm,ierr)
if(srwt_xchng_gb) then
if(myrank==0) write(16,*)'doing final extrapolation in levels1...'
icolor=0
inew=0
do i=1,nsdf
isd=isdf(i)
if(is(isd,1)<0.or.is(isd,2)<0) cycle
if(is(isd,1)==0.or.is(isd,2)==0) then
write(errmsg,*)'LEVELS1: bnd side (2):',is(isd,:),iplg(isidenode(isd,1:2))
call parallel_abort(errmsg)
endif
if(idry_e2(is(isd,1))+idry_e2(is(isd,2))/=1) cycle
if(idry_e2(is(isd,1))==1) then
ie=is(isd,1)
else
ie=is(isd,2)
endif
n1=isidenode(isd,1)
n2=isidenode(isd,2)
nodeA=nm(ie,1)+nm(ie,2)+nm(ie,3)-n1-n2
if(icolor(nodeA)==1) cycle
icolor(nodeA)=1
if(nodeA>np) cycle
inun=0
do j=1,nne(nodeA)
ie2=ine(nodeA,j)
id=iself(nodeA,j)
isd2=js(ie2,id)
if(icolor2(isd2)==1) then
if(ics==1) then
tmp=su2(nvrt,isd2)*sframe(1,1,isd2)+sv2(nvrt,isd2)*sframe(2,1,isd2)
else
tmp=su2(nvrt,isd2)
endif
flux_t=-tmp*ssign(ie2,id)
if(flux_t>0) then
n1=isidenode(isd2,1)
n2=isidenode(isd2,2)
etm=max(eta2(n1),eta2(n2))
if(etm+dp(nodeA)>h0) then
inun=1
exit
endif
endif
endif
enddo
if(inun==1) then
eta2(nodeA)=max(eta2(nodeA),-dp(nodeA)+2*h0)
do j=1,nne(nodeA)
ie2=ine(nodeA,j)
id=iself(nodeA,j)
isd2=js(ie2,id)
if(icolor2(isd2)==1) then
do l=1,3
nd=nm(ie2,l)
if(eta2(nd)+dp(nd)<=h0) then
write(errmsg,*)'LEVELS1: Failed to wet element:',ielg(ie2),iplg(nodeA)
call parallel_abort(errmsg)
endif
enddo
idry_e2(ie2)=0
do l=1,2
id1=js(ie2,nx(id,l))
if(ics==1) then
swild2(1,1:nvrt)=su2(1:nvrt,isd2)
swild2(2,1:nvrt)=sv2(1:nvrt,isd2)
else
dot11=dot_product(sframe(1:3,1,isd2),sframe(1:3,1,id1))
dot21=dot_product(sframe(1:3,2,isd2),sframe(1:3,1,id1))
swild2(1,1:nvrt)=su2(1:nvrt,isd2)*dot11+sv2(1:nvrt,isd2)*dot21
dot12=dot_product(sframe(1:3,1,isd2),sframe(1:3,2,id1))
dot22=dot_product(sframe(1:3,2,isd2),sframe(1:3,2,id1))
swild2(2,1:nvrt)=su2(1:nvrt,isd2)*dot12+sv2(1:nvrt,isd2)*dot22
endif
if(inew(id1)==0) then
su2(1:nvrt,id1)=swild2(1,1:nvrt)
sv2(1:nvrt,id1)=swild2(2,1:nvrt)
inew(id1)=1
else
su2(1:nvrt,id1)=su2(1:nvrt,id1)+swild2(1,1:nvrt)
sv2(1:nvrt,id1)=sv2(1:nvrt,id1)+swild2(2,1:nvrt)
inew(id1)=inew(id1)+1
endif
enddo
endif
enddo
endif
enddo
call exchange_e2di(idry_e2)
call exchange_p2d(eta2)
srwt_xchng=.false.
do i=1,ns
if(inew(i)/=0) then
srwt_xchng=.true.
su2(1:nvrt,i)=su2(1:nvrt,i)/inew(i)
sv2(1:nvrt,i)=sv2(1:nvrt,i)/inew(i)
endif
enddo
istop=2
go to 991
endif
istop=1
do i=1,nsdf
isd=isdf(i)
do j=1,2
nd=isidenode(isd,j)
if(eta2(nd)+dp(nd)<=h0) then
istop=0
do l=1,nne(nd)
ie=ine(nd,l)
if(ie>0) idry_e2(ie)=1
enddo
endif
enddo
enddo
call exchange_e2di(idry_e2)
inew=0
srwt_xchng=.false.
do i=1,nsdf
isd=isdf(i)
if(is(isd,1)<0.or.is(isd,2)<0) cycle
if(is(isd,1)==0.or.is(isd,2)==0) then
write(errmsg,*)'LEVELS1: bnd side:',is(isd,:),iplg(isidenode(isd,1:2))
call parallel_abort(errmsg)
endif
if(idry_e2(is(isd,1))+idry_e2(is(isd,2))/=1) cycle
if(idry_e2(is(isd,1))==1) then
ie=is(isd,1)
else
ie=is(isd,2)
endif
n1=isidenode(isd,1)
n2=isidenode(isd,2)
nodeA=nm(ie,1)+nm(ie,2)+nm(ie,3)-n1-n2
l0=lindex(nodeA,ie)
if(l0==0.or.nodeA==n1.or.nodeA==n2) then
write(errmsg,*)'Frontier node outside, or on the interface:', &
&l0,iplg(nodeA),iplg(n1),iplg(n2),itr,it,iths
write(12,*)'LEVELS1: fatal error message'
do l=1,ns
if(icolor2(l)==1) then
write(12,*)l,iplg(isidenode(l,1:2))
write(12,*)l,ielg(is(l,1:2)),idry_e2(is(l,1:2)),idry_e(is(l,1:2))
endif
enddo
do l=1,nea
write(12,*)l,idry_e2(l),idry_e(l)
enddo
call parallel_abort(errmsg)
endif
if(eta2(nodeA)+dp(nodeA)>h0) then
do j=1,3
nd=nm(ie,j)
if(eta2(nd)+dp(nd)<=h0) then
write(errmsg,*)'Failed to wet element (13):',ielg(ie),iplg(nd),iplg(nodeA)
call parallel_abort(errmsg)
endif
enddo
srwt_xchng=.true.
istop=0
idry_e2(ie)=0
do j=1,2
id1=js(ie,nx(l0,j))
if(icolor2(id1)==0) then
if(ics==1) then
swild2(1,1:nvrt)=su2(1:nvrt,isd)
swild2(2,1:nvrt)=sv2(1:nvrt,isd)
else
dot11=dot_product(sframe(1:3,1,isd),sframe(1:3,1,id1))
dot21=dot_product(sframe(1:3,2,isd),sframe(1:3,1,id1))
swild2(1,1:nvrt)=su2(1:nvrt,isd)*dot11+sv2(1:nvrt,isd)*dot21
dot12=dot_product(sframe(1:3,1,isd),sframe(1:3,2,id1))
dot22=dot_product(sframe(1:3,2,isd),sframe(1:3,2,id1))
swild2(2,1:nvrt)=su2(1:nvrt,isd)*dot12+sv2(1:nvrt,isd)*dot22
endif
if(inew(id1)==0) then
su2(1:nvrt,id1)=swild2(1,1:nvrt)
sv2(1:nvrt,id1)=swild2(2,1:nvrt)
inew(id1)=1
else
su2(1:nvrt,id1)=su2(1:nvrt,id1)+swild2(1,1:nvrt)
sv2(1:nvrt,id1)=sv2(1:nvrt,id1)+swild2(2,1:nvrt)
inew(id1)=inew(id1)+1
endif
endif
enddo
endif
enddo
do i=1,ns
if(inew(i)/=0) then
su2(1:nvrt,i)=su2(1:nvrt,i)/inew(i)
sv2(1:nvrt,i)=sv2(1:nvrt,i)/inew(i)
endif
enddo
991 continue
call mpi_allreduce(srwt_xchng,srwt_xchng_gb,1,MPI_LOGICAL,MPI_LOR,comm,ierr)
if(srwt_xchng_gb) then
call exchange_e2di(idry_e2)
allocate(swild(2,nvrt,nsa),stat=istat)
if(istat/=0) call parallel_abort('Levels1: fail to allocate (9)')
swild(1,:,:)=su2(:,:)
swild(2,:,:)=sv2(:,:)
#ifdef INCLUDE_TIMING
if(cwtime) cwtmp=mpi_wtime()
#endif
call exchange_s3d_2(swild)
#ifdef INCLUDE_TIMING
if(cwtime) wtimer(10,2)=wtimer(10,2)+mpi_wtime()-cwtmp
#endif
su2(:,:)=swild(1,:,:)
sv2(:,:)=swild(2,:,:)
deallocate(swild)
endif
idry2=1
do i=1,nea
if(idry_e2(i)==0) idry2(nm(i,1:3))=0
enddo
call exchange_p2di(idry2)
do i=1,nea
inew(i)=0
do j=1,3
if(idry2(nm(i,j))==1) inew(i)=1
enddo
enddo
srwt_xchng=.false.
do i=1,ns
if(.not.(idry_e2(is(i,1))==1.and.(is(i,2)==0.or.is(i,2)>0.and.idry_e2(max(1,is(i,2)))==1))) cycle
iwet=0
do j=1,2
ie=is(i,j)
if(ie>0.and.idry_e2(max(1,ie))==1.and.inew(max(1,ie))==0) iwet=1
enddo
if(iwet==1) then
sutmp=0; svtmp=0; icount=0
do m=1,2
ie=is(i,m)
if(ie<=0) cycle
do jj=1,3
isd2=js(ie,jj)
if(is(isd2,1)>0.and.idry_e2(max(1,is(isd2,1)))==0.or. &
&is(isd2,2)>0.and.idry_e2(max(1,is(isd2,2)))==0) then
icount=icount+1
if(ics==1) then
swild2(1,1:nvrt)=su2(1:nvrt,isd2)
swild2(2,1:nvrt)=sv2(1:nvrt,isd2)
else
dot11=dot_product(sframe(1:3,1,isd2),sframe(1:3,1,i))
dot21=dot_product(sframe(1:3,2,isd2),sframe(1:3,1,i))
swild2(1,1:nvrt)=su2(1:nvrt,isd2)*dot11+sv2(1:nvrt,isd2)*dot21
dot12=dot_product(sframe(1:3,1,isd2),sframe(1:3,2,i))
dot22=dot_product(sframe(1:3,2,isd2),sframe(1:3,2,i))
swild2(2,1:nvrt)=su2(1:nvrt,isd2)*dot12+sv2(1:nvrt,isd2)*dot22
endif
sutmp(1:nvrt)=sutmp(1:nvrt)+swild2(1,1:nvrt)
svtmp(1:nvrt)=svtmp(1:nvrt)+swild2(2,1:nvrt)
endif
enddo
enddo
if(icount/=0) then
srwt_xchng=.true.
su2(1:nvrt,i)=sutmp(1:nvrt)/icount
sv2(1:nvrt,i)=svtmp(1:nvrt)/icount
endif
endif
enddo
idry_e2=inew
call mpi_allreduce(srwt_xchng,srwt_xchng_gb,1,MPI_LOGICAL,MPI_LOR,comm,ierr)
if(srwt_xchng_gb) then
allocate(swild(2,nvrt,nsa),stat=istat)
if(istat/=0) call parallel_abort('Levels1: fail to allocate (8)')
swild(1,:,:)=su2(:,:)
swild(2,:,:)=sv2(:,:)
#ifdef INCLUDE_TIMING
if(cwtime) cwtmp=mpi_wtime()
#endif
call exchange_s3d_2(swild)
#ifdef INCLUDE_TIMING
if(cwtime) wtimer(10,2)=wtimer(10,2)+mpi_wtime()-cwtmp
#endif
su2(:,:)=swild(1,:,:)
sv2(:,:)=swild(2,:,:)
deallocate(swild)
endif
call parallel_barrier
if(istop==2) exit loop15
end do loop15
if(myrank==0) then
write(16,*)'see fort.7 for # of iterations used in LEVELS1...'
write(7,*)it,itr
endif
endif
do i=1,np
if(dp(i)+eta2(i)<=h0) idry_e2(ine(i,1:nne(i)))=1
enddo
call exchange_e2di(idry_e2)
idry2=1; idry_s2=1
do i=1,nea
if(idry_e2(i)==0) then
idry2(nm(i,1:3))=0
idry_s2(js(i,1:3))=0
endif
enddo
call exchange_p2di(idry2)
call exchange_s2di(idry_s2)
do i=1,nsa
if(idry_s2(i)==1) then
su2(1:nvrt,i)=0
sv2(1:nvrt,i)=0
endif
enddo
do i=1,npa
if(idry2(i)==1) then
eta2(i)=min(0.d0,-dp(i))
endif
enddo
# ifdef SED_MORPH
lmorph=.true.
# else
lmorph=.false.
# endif
iback=0
do i=1,npa
if(eta2(i)<=h0-h_s) then
write(errmsg,*)'Deep depth dry:',iplg(i)
call parallel_abort(errmsg)
endif
if(idry2(i)==1) then
kbp(i)=0
else
if(dp(i)+eta2(i)<=h0) then
write(errmsg,*)'levels1: (2):',i,dp(i)+eta2(i)
call parallel_abort(errmsg)
endif
do k=kz,nvrt
kin=k-kz+1
if(hmod(i)<=h_c) then
iback(i)=1
znl(k,i)=sigma(kin)*(hmod(i)+eta2(i))+eta2(i)
else if(eta2(i)<=-h_c-(hmod(i)-h_c)*theta_f/s_con1) then
write(errmsg,*)'Pls choose a larger h_c (2):',eta2(i),h_c
call parallel_abort(errmsg)
else
znl(k,i)=eta2(i)*(1+sigma(kin))+h_c*sigma(kin)+(hmod(i)-h_c)*cs(kin)
endif
enddo
if(dp(i)<=h_s) then
kbp(i)=kz
else
if(imm>0.or.it==iths.or.lmorph) then
kbp(i)=0
do k=1,kz-1
if(-dp(i)>=ztot(k).and.-dp(i)<ztot(k+1)) then
kbp(i)=k
exit
endif
enddo
if(kbp(i)==0) then
write(errmsg,*)'Cannot find a bottom level for node (3):',i
call parallel_abort(errmsg)
endif
endif
if(kbp(i)>=kz.or.kbp(i)<1) then
write(errmsg,*)'Impossible 92:',kbp(i),kz,i
call parallel_abort(errmsg)
endif
znl(kbp(i),i)=-dp(i)
do k=kbp(i)+1,kz-1
znl(k,i)=ztot(k)
enddo
endif
do k=kbp(i)+1,nvrt
if(znl(k,i)-znl(k-1,i)<=0) then
write(errmsg,*)'Inverted z-levels at:',i,k,znl(k,i)-znl(k-1,i),eta2(i),hmod(i)
call parallel_abort(errmsg)
endif
enddo
endif
enddo
kbe=0
do i=1,nea
if(idry_e2(i)/=0) cycle
n1=nm(i,1); n2=nm(i,2); n3=nm(i,3)
if(idry2(n1)/=0.or.idry2(n2)/=0.or.idry2(n3)/=0) then
write(errmsg,*)'level1: Element-node inconsistency (0):',ielg(i),idry_e(i), &
iplg(nm(i,1:3)),idry2(nm(i,1:3))
call parallel_abort(errmsg)
endif
kbe(i)=max0(kbp(n1),kbp(n2),kbp(n3))
do k=kbe(i),nvrt
ze(k,i)=(znl(k,n1)+znl(k,n2)+znl(k,n3))/3
if(k>=kbe(i)+1) then; if(ze(k,i)-ze(k-1,i)<=0) then
write(errmsg,*)'Weird element:',k,i,ze(k,i),ze(k-1,i)
call parallel_abort(errmsg)
endif; endif
enddo
enddo
do i=1,nsa
kbs(i)=0
if(idry_s2(i)==0) then
n1=isidenode(i,1)
n2=isidenode(i,2)
if(idry2(n1)/=0.or.idry2(n2)/=0) then
write(errmsg,*)'Side-node inconsistency:',it,islg(i),'node:',iplg(n1),iplg(n2), &
eta2(n1),eta2(n2),idry2(n1),idry2(n2),';element:', &
(is(i,j),ielg(is(i,j)),idry_e2(is(i,j)),j=1,2)
call parallel_abort(errmsg)
endif
if(dps(i)+(eta2(n1)+eta2(n2))/2<=h0) then
write(errmsg,*)'Weird side:',islg(i),iplg(n1),iplg(n2),eta2(n1),eta2(n2)
call parallel_abort(errmsg)
endif
kbs(i)=max0(kbp(n1),kbp(n2))
do k=kbs(i),nvrt
zs(k,i)=(znl(k,n1)+znl(k,n2))/2
if(k>=kbs(i)+1) then; if(zs(k,i)-zs(k-1,i)<=0) then
write(errmsg,*)'Weird side:',k,iplg(n1),iplg(n2),znl(k,n1),znl(k,n2),znl(k-1,n1),znl(k-1,n2)
call parallel_abort(errmsg)
endif; endif
enddo
endif
enddo
prwt_xchng=.false.
if(it/=iths) then
do i=1,npa
if(idry(i)==1.and.idry2(i)==0) then
if(.not.prwt_xchng.and.i>np) prwt_xchng=.true.
if(i>np) cycle
do k=1,nvrt
uu2(k,i)=0
vv2(k,i)=0
ttmp=0
stmp=0
icount=0
do j=1,nnp(i)
nd=inp(i,j)
if(idry(nd)==0) then
icount=icount+1
uu2(k,i)=uu2(k,i)+uu2(k,nd)
vv2(k,i)=vv2(k,i)+vv2(k,nd)
ttmp=ttmp+tnd(k,nd)
stmp=stmp+snd(k,nd)
endif
enddo
if(icount==0) then
else
uu2(k,i)=uu2(k,i)/icount
vv2(k,i)=vv2(k,i)/icount
tnd(k,i)=ttmp/icount
snd(k,i)=stmp/icount
endif
enddo
endif
enddo
endif
srwt_xchng=.false.
if(it/=iths) then
do i=1,nsa
if(idry_s(i)==1.and.idry_s2(i)==0) then
if(.not.srwt_xchng.and.i>ns) srwt_xchng=.true.
if(i>ns) cycle
n1=isidenode(i,1)
n2=isidenode(i,2)
do k=1,nvrt
ttmp=0
stmp=0
icount=0
do j=1,2
ie=is(i,j)
if(ie/=0) then
if(ie<0) call parallel_abort('levels1: ghost element')
do jj=1,3
isd=js(ie,jj)
if(isd/=i.and.idry_s(isd)==0) then
icount=icount+1
ttmp=ttmp+tsd(k,isd)
stmp=stmp+ssd(k,isd)
endif
enddo
endif
enddo
if(icount==0) then
else
tsd(k,i)=ttmp/icount
ssd(k,i)=stmp/icount
endif
enddo
endif
enddo
endif
if(nproc>1) then
#ifdef INCLUDE_TIMING
if(cwtime) cwtmp=mpi_wtime()
#endif
call mpi_allreduce(prwt_xchng,prwt_xchng_gb,1,MPI_LOGICAL,MPI_LOR,comm,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('levels1: allreduce prwt_xchng_gb',ierr)
call mpi_allreduce(srwt_xchng,srwt_xchng_gb,1,MPI_LOGICAL,MPI_LOR,comm,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('levels1: allreduce srwt_xchng_gb',ierr)
#ifdef INCLUDE_TIMING
if(cwtime) wtimer(10,2)=wtimer(10,2)+mpi_wtime()-cwtmp
#endif
if(prwt_xchng_gb) then
allocate(swild(4,nvrt,nsa),stat=istat)
if(istat/=0) call parallel_abort('Levels0: fail to allocate swild')
swild(1,:,1:npa)=uu2(:,:)
swild(2,:,1:npa)=vv2(:,:)
swild(3,:,1:npa)=tnd(:,:)
swild(4,:,1:npa)=snd(:,:)
#ifdef INCLUDE_TIMING
if(cwtime) cwtmp=mpi_wtime()
#endif
call exchange_p3d_4(swild)
#ifdef INCLUDE_TIMING
if(cwtime) wtimer(10,2)=wtimer(10,2)+mpi_wtime()-cwtmp
#endif
uu2(:,:)=swild(1,:,1:npa)
vv2(:,:)=swild(2,:,1:npa)
tnd(:,:)=swild(3,:,1:npa)
snd(:,:)=swild(4,:,1:npa)
deallocate(swild)
endif
if(srwt_xchng_gb) then
allocate(swild(2,nvrt,nsa),stat=istat)
if(istat/=0) call parallel_abort('Levels0: fail to allocate swild')
swild(1,:,:)=tsd(:,:)
swild(2,:,:)=ssd(:,:)
#ifdef INCLUDE_TIMING
if(cwtime) cwtmp=mpi_wtime()
#endif
call exchange_s3d_2(swild)
#ifdef INCLUDE_TIMING
if(cwtime) wtimer(10,2)=wtimer(10,2)+mpi_wtime()-cwtmp
#endif
tsd(:,:)=swild(1,:,:)
ssd(:,:)=swild(2,:,:)
deallocate(swild)
endif
endif
idry=idry2
idry_s=idry_s2
idry_e=idry_e2
end subroutine levels1
subroutine levels0(iths,it)
#ifdef USE_MPIMODULE
use mpi
#endif
use elfe_glbl
use elfe_msgp
implicit real(rkind)(a-h,o-z),integer(i-n)
#ifndef USE_MPIMODULE
include 'mpif.h'
#endif
integer, intent(in) :: iths,it
integer :: idry_new(npa),idry_s_new(nsa)
dimension swild2(2)
real(rkind),allocatable :: swild(:,:,:)
logical :: srwt_xchng,prwt_xchng
logical :: srwt_xchng_gb,prwt_xchng_gb
logical :: cwtime,lmorph
cwtime=it/=iths
# ifdef SED_MORPH
lmorph=.true.
# else
lmorph=.false.
# endif
iback=0
do i=1,npa
if(dp(i)+eta2(i)<=h0) then
idry_new(i)=1
if(dp(i)>=h_s) then
write(errmsg,*)'Deep depth dry:',i
call parallel_abort(errmsg)
endif
kbp(i)=0
else
idry_new(i)=0
do k=kz,nvrt
kin=k-kz+1
if(hmod(i)<=h_c) then
iback(i)=1
znl(k,i)=sigma(kin)*(hmod(i)+eta2(i))+eta2(i)
else if(eta2(i)<=-h_c-(hmod(i)-h_c)*theta_f/s_con1) then
write(errmsg,*)'Pls choose a larger h_c (1):',eta2(i),h_c
call parallel_abort(errmsg)
else
znl(k,i)=eta2(i)*(1+sigma(kin))+h_c*sigma(kin)+(hmod(i)-h_c)*cs(kin)
endif
enddo
if(dp(i)<=h_s) then
kbp(i)=kz
else
if(imm>0.or.it==iths.or.lmorph) then
kbp(i)=0
do k=1,kz-1
if(-dp(i)>=ztot(k).and.-dp(i)<ztot(k+1)) then
kbp(i)=k
exit
endif
enddo
if(kbp(i)==0) then
write(errmsg,*)'Cannot find a bottom level for node (3):',i
call parallel_abort(errmsg)
endif
endif
if(kbp(i)>=kz.or.kbp(i)<1) then
write(errmsg,*)'Impossible 92:',kbp(i),kz,i
call parallel_abort(errmsg)
endif
znl(kbp(i),i)=-dp(i)
do k=kbp(i)+1,kz-1
znl(k,i)=ztot(k)
enddo
endif
do k=kbp(i)+1,nvrt
if(znl(k,i)-znl(k-1,i)<=0) then
write(errmsg,*)'Inverted z-levels at:',i,k,znl(k,i)-znl(k-1,i),eta2(i),hmod(i)
call parallel_abort(errmsg)
endif
enddo
endif
enddo
do i=1,nea
idry_e(i)=max0(idry_new(nm(i,1)),idry_new(nm(i,2)),idry_new(nm(i,3)))
enddo
idry_s_new(1:npa)=idry(:)
idry=1
do i=1,np
do j=1,nne(i)
ie=ine(i,j)
if(idry_e(ie)==0) then
idry(i)=0; exit
endif
enddo
enddo
#ifdef INCLUDE_TIMING
if(cwtime) cwtmp=mpi_wtime()
#endif
call exchange_p2di(idry)
#ifdef INCLUDE_TIMING
if(cwtime) wtimer(10,2)=wtimer(10,2)+mpi_wtime()-cwtmp
#endif
#ifdef DEBUG
do i=1,npa
if(idry(i)==1) cycle
if(eta2(i)+dp(i)<=h0) then
write(errmsg,*)'levels0: weird wet node:',iplg(i),eta2(i),dp(i),idry_new(i)
call parallel_abort(errmsg)
endif
if(i>np) cycle
ifl=0
do j=1,nne(i)
ie=ine(i,j)
if(idry_e(ie)==0) then
ifl=1; exit
endif
enddo
if(ifl==0) then
write(errmsg,*)'Node-element inconsistency:',iplg(i),idry(i),(idry_e(ine(i,j)),j=1,nne(i))
call parallel_abort(errmsg)
endif
enddo
#endif
kbe=0
do i=1,nea
if(idry_e(i)/=0) cycle
n1=nm(i,1); n2=nm(i,2); n3=nm(i,3)
if(idry(n1)/=0.or.idry(n2)/=0.or.idry(n3)/=0) then
write(errmsg,*)'level0: Element-node inconsistency (0):',ielg(i),idry_e(i), &
iplg(nm(i,1:3)),idry(nm(i,1:3)),idry_new(nm(i,1:3))
call parallel_abort(errmsg)
endif
kbe(i)=max0(kbp(n1),kbp(n2),kbp(n3))
do k=kbe(i),nvrt
ze(k,i)=(znl(k,n1)+znl(k,n2)+znl(k,n3))/3
if(k>=kbe(i)+1) then; if(ze(k,i)-ze(k-1,i)<=0) then
write(errmsg,*)'Weird element:',k,i,ze(k,i),ze(k-1,i)
call parallel_abort(errmsg)
endif; endif
enddo
enddo
prwt_xchng=.false.
if(it/=iths) then
do i=1,npa
if(idry_s_new(i)==1.and.idry(i)==0) then
if(.not.prwt_xchng.and.i>np) prwt_xchng=.true.
if(i>np) cycle
do k=1,nvrt
uu2(k,i)=0
vv2(k,i)=0
ttmp=0
stmp=0
icount=0
do j=1,nnp(i)
nd=inp(i,j)
if(idry_s_new(nd)==0) then
icount=icount+1
uu2(k,i)=uu2(k,i)+uu2(k,nd)
vv2(k,i)=vv2(k,i)+vv2(k,nd)
ttmp=ttmp+tnd(k,nd)
stmp=stmp+snd(k,nd)
endif
enddo
if(icount==0) then
else
uu2(k,i)=uu2(k,i)/icount
vv2(k,i)=vv2(k,i)/icount
tnd(k,i)=ttmp/icount
snd(k,i)=stmp/icount
endif
enddo
endif
enddo
endif
idry_s_new=1
do i=1,ns
do j=1,2
ie=is(i,j)
if(ie/=0.and.idry_e(max(1,ie))==0) idry_s_new(i)=0
enddo
enddo
#ifdef INCLUDE_TIMING
if(cwtime) cwtmp=mpi_wtime()
#endif
call exchange_s2di(idry_s_new)
#ifdef INCLUDE_TIMING
if(cwtime) wtimer(10,2)=wtimer(10,2)+mpi_wtime()-cwtmp
#endif
#ifdef DEBUG
do i=1,nea
if(idry_e(i)/=0) cycle
do j=1,3
isd=js(i,j)
if(idry_s_new(isd)/=0) then
write(errmsg,*)'Element-side inconsistency:',ielg(i),islg(isd),idry_s_new(isd)
call parallel_abort(errmsg)
endif
enddo
enddo
do i=1,ns
if(idry_s_new(i)==1) cycle
ifl=0
do j=1,2
ie=is(i,j)
if(ie/=0.and.idry_e(max(1,ie))==0) then
ifl=1; exit
endif
enddo
if(ifl==0) then
write(errmsg,*)'Side-element inconsistency:',islg(i),idry_s_new(i), &
(is(i,j),idry_e(is(i,j)),j=1,2)
call parallel_abort(errmsg)
endif
enddo
#endif
do i=1,nsa
n1=isidenode(i,1)
n2=isidenode(i,2)
kbs(i)=0
if(idry_s_new(i)==0) then
if(idry(n1)/=0.or.idry(n2)/=0) then
write(errmsg,*)'Side-node inconsistency:',it,islg(i),'node:',iplg(n1),iplg(n2), &
eta2(n1),eta2(n2),idry(n1),idry(n2),';element:', &
(is(i,j),ielg(is(i,j)),idry_e(is(i,j)),j=1,2)
call parallel_abort(errmsg)
endif
if(dps(i)+(eta2(n1)+eta2(n2))/2<=h0) then
write(errmsg,*)'Weird side:',islg(i),iplg(n1),iplg(n2),eta2(n1),eta2(n2)
call parallel_abort(errmsg)
endif
kbs(i)=max0(kbp(n1),kbp(n2))
do k=kbs(i),nvrt
zs(k,i)=(znl(k,n1)+znl(k,n2))/2
if(k>=kbs(i)+1) then; if(zs(k,i)-zs(k-1,i)<=0) then
write(errmsg,*)'Weird side:',k,iplg(n1),iplg(n2),znl(k,n1),znl(k,n2),znl(k-1,n1),znl(k-1,n2)
call parallel_abort(errmsg)
endif; endif
enddo
endif
enddo
srwt_xchng=.false.
if(it/=iths) then
do i=1,nsa
if(idry_s(i)==1.and.idry_s_new(i)==0) then
if(.not.srwt_xchng.and.i>ns) srwt_xchng=.true.
if(i>ns) cycle
n1=isidenode(i,1)
n2=isidenode(i,2)
do k=1,nvrt
su2(k,i)=0
sv2(k,i)=0
ttmp=0
stmp=0
icount=0
do j=1,2
ie=is(i,j)
if(ie/=0) then
if(ie<0) call parallel_abort('levels0: ghost element')
do jj=1,3
isd=js(ie,jj)
if(idry_s(isd)==0) then
icount=icount+1
if(ics==1) then
swild2(1)=su2(k,isd)
swild2(2)=sv2(k,isd)
else
dot11=dot_product(sframe(1:3,1,isd),sframe(1:3,1,i))
dot21=dot_product(sframe(1:3,2,isd),sframe(1:3,1,i))
swild2(1)=su2(k,isd)*dot11+sv2(k,isd)*dot21
dot12=dot_product(sframe(1:3,1,isd),sframe(1:3,2,i))
dot22=dot_product(sframe(1:3,2,isd),sframe(1:3,2,i))
swild2(2)=su2(k,isd)*dot12+sv2(k,isd)*dot22
endif
su2(k,i)=su2(k,i)+swild2(1)
sv2(k,i)=sv2(k,i)+swild2(2)
ttmp=ttmp+tsd(k,isd)
stmp=stmp+ssd(k,isd)
endif
enddo
endif
enddo
if(icount==0) then
else
su2(k,i)=su2(k,i)/icount
sv2(k,i)=sv2(k,i)/icount
tsd(k,i)=ttmp/icount
ssd(k,i)=stmp/icount
endif
enddo
endif
enddo
endif
idry_s=idry_s_new
if(nproc>1) then
#ifdef INCLUDE_TIMING
if(cwtime) cwtmp=mpi_wtime()
#endif
call mpi_allreduce(prwt_xchng,prwt_xchng_gb,1,MPI_LOGICAL,MPI_LOR,comm,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('levels0: allreduce prwt_xchng_gb',ierr)
call mpi_allreduce(srwt_xchng,srwt_xchng_gb,1,MPI_LOGICAL,MPI_LOR,comm,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('levels0: allreduce srwt_xchng_gb',ierr)
#ifdef INCLUDE_TIMING
if(cwtime) wtimer(10,2)=wtimer(10,2)+mpi_wtime()-cwtmp
#endif
if(prwt_xchng_gb.or.srwt_xchng_gb) then
allocate(swild(4,nvrt,nsa),stat=istat)
if(istat/=0) call parallel_abort('Levels0: fail to allocate swild')
endif
if(prwt_xchng_gb) then
swild(1,:,1:npa)=uu2(:,:)
swild(2,:,1:npa)=vv2(:,:)
swild(3,:,1:npa)=tnd(:,:)
swild(4,:,1:npa)=snd(:,:)
#ifdef INCLUDE_TIMING
if(cwtime) cwtmp=mpi_wtime()
#endif
call exchange_p3d_4(swild)
#ifdef INCLUDE_TIMING
if(cwtime) wtimer(10,2)=wtimer(10,2)+mpi_wtime()-cwtmp
#endif
uu2(:,:)=swild(1,:,1:npa)
vv2(:,:)=swild(2,:,1:npa)
tnd(:,:)=swild(3,:,1:npa)
snd(:,:)=swild(4,:,1:npa)
endif
if(srwt_xchng_gb) then
swild(1,:,:)=su2(:,:)
swild(2,:,:)=sv2(:,:)
swild(3,:,:)=tsd(:,:)
swild(4,:,:)=ssd(:,:)
#ifdef INCLUDE_TIMING
if(cwtime) cwtmp=mpi_wtime()
#endif
call exchange_s3d_4(swild)
#ifdef INCLUDE_TIMING
if(cwtime) wtimer(10,2)=wtimer(10,2)+mpi_wtime()-cwtmp
#endif
su2(:,:)=swild(1,:,:)
sv2(:,:)=swild(2,:,:)
tsd(:,:)=swild(3,:,:)
ssd(:,:)=swild(4,:,:)
endif
if(prwt_xchng_gb.or.srwt_xchng_gb) deallocate(swild)
endif
end subroutine levels0
subroutine nodalvel(ifltype)
#ifdef USE_MPIMODULE
use mpi
#endif
use elfe_glbl
use elfe_msgp
implicit real(rkind)(a-h,o-z),integer(i-n)
#ifndef USE_MPIMODULE
include 'mpif.h'
#endif
integer, intent(in) :: ifltype(max(1,nope_global))
logical :: ltmp,ltmp2
dimension swild(2),swild2(nvrt,2),swild3(nvrt),nwild(3),swild5(3,2)
real(rkind), allocatable :: swild4(:,:,:)
ufg=0; vfg=0
do i=1,nea
do k=1,nvrt
do j=1,3
nwild(1)=js(i,j)
nwild(2)=js(i,nx(j,1))
nwild(3)=js(i,nx(j,2))
if(ics==1) then
ufg(k,i,j)=su2(k,nwild(2))+su2(k,nwild(3))-su2(k,nwild(1))
vfg(k,i,j)=sv2(k,nwild(2))+sv2(k,nwild(3))-sv2(k,nwild(1))
else
do m=1,3
swild5(m,1)=su2(k,nwild(m))*dot_product(sframe(:,1,nwild(m)),eframe(:,1,i))+ &
&sv2(k,nwild(m))*dot_product(sframe(:,2,nwild(m)),eframe(:,1,i))
swild5(m,2)=su2(k,nwild(m))*dot_product(sframe(:,1,nwild(m)),eframe(:,2,i))+ &
&sv2(k,nwild(m))*dot_product(sframe(:,2,nwild(m)),eframe(:,2,i))
enddo
ufg(k,i,j)=swild5(2,1)+swild5(3,1)-swild5(1,1)
vfg(k,i,j)=swild5(2,2)+swild5(3,2)-swild5(1,2)
endif
ufg(k,i,j)=max(-rmaxvel,min(rmaxvel,ufg(k,i,j)))
vfg(k,i,j)=max(-rmaxvel,min(rmaxvel,vfg(k,i,j)))
enddo
enddo
enddo
if(indvel<=0) then
uu2=0; vv2=0; ww2=0
do i=1,np
if(idry(i)==1) cycle
do k=kbp(i),nvrt
weit_w=0
icount=0
do j=1,nne(i)
ie=ine(i,j)
id=iself(i,j)
if(idry_e(ie)==0) then
icount=icount+1
if(ics==1) then
uu2(k,i)=uu2(k,i)+ufg(k,ie,id)
vv2(k,i)=vv2(k,i)+vfg(k,ie,id)
else
uu2(k,i)=uu2(k,i)+ufg(k,ie,id)*dot_product(eframe(:,1,ie),pframe(:,1,i))+ &
&vfg(k,ie,id)*dot_product(eframe(:,2,ie),pframe(:,1,i))
vv2(k,i)=vv2(k,i)+ufg(k,ie,id)*dot_product(eframe(:,1,ie),pframe(:,2,i))+ &
&vfg(k,ie,id)*dot_product(eframe(:,2,ie),pframe(:,2,i))
endif
endif
if(interpol(ie)==1) then
if(idry_e(ie)==1) then
swild(1)=0
else
kbb=kbe(ie)
swild3(kbb:nvrt)=ze(kbb:nvrt,ie)
swild2(kbb:nvrt,1)=we(kbb:nvrt,ie)
call vinter(nvrt,2,1,znl(k,i),kbb,nvrt,k,swild3,swild2,swild,ibelow)
endif
else
swild(1)=we(k,ie)
endif
ww2(k,i)=ww2(k,i)+swild(1)*area(ie)
weit_w=weit_w+area(ie)
enddo
if(icount==0) then
write(errmsg,*)'Isolated wet node (8):',i
call parallel_abort(errmsg)
else
uu2(k,i)=uu2(k,i)/icount
vv2(k,i)=vv2(k,i)/icount
endif
ww2(k,i)=ww2(k,i)/weit_w
enddo
do k=1,kbp(i)-1
uu2(k,i)=0
vv2(k,i)=0
ww2(k,i)=0
enddo
enddo
else
uu2=0; vv2=0; ww2=0
do i=1,np
if(idry(i)==1) cycle
icase=2
do j=1,nne(i)
ie=ine(i,j)
if(interpol(ie)==1) icase=1
enddo
do k=kbp(i),nvrt
weit=0
weit_w=0
do j=1,nne(i)
ie=ine(i,j)
id=iself(i,j)
if(isbnd(1,i)/=0) then
limit=1
else
limit=2
endif
do l=2,limit,-1
isd=js(ie,nx(id,l))
ltmp=isbnd(1,i)>0.and.ifltype(max(1,isbnd(1,i)))/=0.or. &
isbnd(2,i)>0.and.ifltype(max(1,isbnd(2,i)))/=0
if(ltmp) then
nfac=0
ltmp2=isbnd(1,i)>0.and.ifltype(max(1,isbnd(1,i)))/=0.and.isbs(isd)==isbnd(1,i).or. &
isbnd(2,i)>0.and.ifltype(max(1,isbnd(2,i)))/=0.and.isbs(isd)==isbnd(2,i)
if(ltmp2) nfac=1
else
nfac=1
endif
if(icase==1) then
if(idry_s(isd)==1) then
swild(1:2)=0
else
kbb=kbs(isd)
if(ics==1) then
swild2(kbb:nvrt,1)=su2(kbb:nvrt,isd)
swild2(kbb:nvrt,2)=sv2(kbb:nvrt,isd)
else
swild2(kbb:nvrt,1)=su2(kbb:nvrt,isd)*dot_product(sframe(:,1,isd),pframe(:,1,i))+&
&sv2(kbb:nvrt,isd)*dot_product(sframe(:,2,isd),pframe(:,1,i))
swild2(kbb:nvrt,2)=su2(kbb:nvrt,isd)*dot_product(sframe(:,1,isd),pframe(:,2,i))+&
&sv2(kbb:nvrt,isd)*dot_product(sframe(:,2,isd),pframe(:,2,i))
endif
swild3(kbb:nvrt)=zs(kbb:nvrt,isd)
call vinter(nvrt,2,2,znl(k,i),kbb,nvrt,k,swild3,swild2,swild,ibelow)
endif
else
if(ics==1) then
swild(1)=su2(k,isd)
swild(2)=sv2(k,isd)
else
swild(1)=su2(k,isd)*dot_product(sframe(:,1,isd),pframe(:,1,i))+&
&sv2(k,isd)*dot_product(sframe(:,2,isd),pframe(:,1,i))
swild(2)=su2(k,isd)*dot_product(sframe(:,1,isd),pframe(:,2,i))+&
&sv2(k,isd)*dot_product(sframe(:,2,isd),pframe(:,2,i))
endif
endif
uu2(k,i)=uu2(k,i)+swild(1)/distj(isd)*nfac
vv2(k,i)=vv2(k,i)+swild(2)/distj(isd)*nfac
weit=weit+1/distj(isd)*nfac
enddo
if(interpol(ie)==1) then
if(idry_e(ie)==1) then
swild(1)=0
else
kbb=kbe(ie)
swild3(kbb:nvrt)=ze(kbb:nvrt,ie)
swild2(kbb:nvrt,1)=we(kbb:nvrt,ie)
call vinter(nvrt,2,1,znl(k,i),kbb,nvrt,k,swild3,swild2,swild,ibelow)
endif
else
swild(1)=we(k,ie)
endif
ww2(k,i)=ww2(k,i)+swild(1)*area(ie)
weit_w=weit_w+area(ie)
enddo
if(weit==0) then
write(errmsg,*)'nodalvel: Isolated open bnd node:',iplg(i),isbnd(1:2,i)
call parallel_abort(errmsg)
endif
uu2(k,i)=uu2(k,i)/weit
vv2(k,i)=vv2(k,i)/weit
ww2(k,i)=ww2(k,i)/weit_w
enddo
do k=1,kbp(i)-1
uu2(k,i)=0
vv2(k,i)=0
ww2(k,i)=0
enddo
enddo
endif
allocate(swild4(3,nvrt,npa),stat=istat)
if(istat/=0) call parallel_abort('nodalvel: fail to allocate')
swild4(1,:,:)=uu2(:,:)
swild4(2,:,:)=vv2(:,:)
swild4(3,:,:)=ww2(:,:)
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call exchange_p3d_3(swild4)
#ifdef INCLUDE_TIMING
wtimer(10,2)=wtimer(10,2)+mpi_wtime()-cwtmp
#endif
uu2(:,:)=swild4(1,:,:)
vv2(:,:)=swild4(2,:,:)
ww2(:,:)=swild4(3,:,:)
deallocate(swild4)
end subroutine nodalvel
subroutine vinter(nmax1,nmax2,nc,zt,k1,k2,k3,za,sint,sout,ibelow)
use elfe_glbl, only : rkind,errmsg
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
integer, intent(in) :: nmax1,nmax2,nc,k1,k2,k3
real(rkind), intent(in) :: zt,za(nmax1),sint(nmax1,nmax2)
real(rkind), dimension(:), intent(out) :: sout(nmax2)
integer, intent(out) :: ibelow
logical :: first_call
first_call=.true.
if(k1>k2) then
write(errmsg,*)'k1>k2 in vinter()'
call parallel_abort(errmsg)
endif
if(zt<za(k1)) then
ibelow=1
sout(1:nc)=sint(k1,1:nc)
else
ibelow=0
if(zt==za(k1)) then
sout(1:nc)=sint(k1,1:nc)
else if(zt>=za(k2)) then
sout(1:nc)=sint(k2,1:nc)
else
kout=0
if(k3<k1.or.k3>k2) then
l1=k1; l2=k2-1
else
if(zt<za(k3)) then
l1=k1; l2=k3-1
else
l1=k3; l2=k2-1
endif
endif
do k=l1,l2
if(zt>=za(k).and.zt<=za(k+1)) then
kout=k
exit
endif
enddo
if(kout==0.or.za(kout+1)-za(kout)==0) then
write(errmsg,*)'Failed to find a level in vinter():',kout,zt,(za(k),k=k1,k2)
call parallel_abort(errmsg)
endif
zrat=(zt-za(kout))/(za(kout+1)-za(kout))
sout(1:nc)=sint(kout,1:nc)*(1-zrat)+sint(kout+1,1:nc)*zrat
endif
endif
first_call=.false.
end subroutine vinter
function eqstate(tem2,sal2)
use elfe_glbl, only: rkind,tempmin,tempmax,saltmin,saltmax,errmsg,ifort12
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z),integer(i-n)
real(rkind), intent(in) :: tem2,sal2
den(t,s)=1000-0.157406+6.793952E-2*t-9.095290E-3*t**2 &
&+1.001685E-4*t**3-1.120083E-6*t**4+6.536332E-9*t**5+ &
&s*(0.824493-4.0899E-3*t+&
&7.6438E-5*t**2-8.2467E-7*t**3+5.3875E-9*t**4)+&
&sqrt(s)**3*(-5.72466E-3+1.0227E-4*t-1.6546E-6*t**2)+&
&4.8314E-4*s**2
tem=tem2; sal=sal2
if(tem<-98.or.sal<-98) then
write(errmsg,*)'EQSTATE: Impossible dry (7):',tem,sal
call parallel_abort(errmsg)
endif
if(tem<tempmin.or.tem>tempmax.or.sal<saltmin.or.sal>saltmax) then
if(ifort12(6)==0) then
ifort12(6)=1
write(12,*)'Invalid temp. or salinity for density:',tem,sal
endif
tem=max(tempmin,min(tem,tempmax))
sal=max(saltmin,min(sal,saltmax))
endif
eqstate=den(tem,sal)
if(eqstate<980) then
write(errmsg,*)'Weird density:',eqstate,tem,sal
call parallel_abort(errmsg)
endif
end function eqstate
subroutine asm(i,j,vd,td,qd1,qd2)
use elfe_glbl
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
integer, intent(in) :: i,j
real(rkind), intent(out) :: vd,td,qd1,qd2
if(j<kbp(i).or.j>nvrt) then
write(errmsg,*)'Wrong input level:',j
call parallel_abort(errmsg)
endif
if(j==kbp(i).or.j==nvrt) then
drho_dz=0
else
drho_dz=(prho(j+1,i)-prho(j-1,i))/(znl(j+1,i)-znl(j-1,i))
endif
bvf=grav/rho0*drho_dz
Gh=xl(i,j)**2/2/q2(i,j)*bvf
Gh=min(max(Gh,-0.28_rkind),0.0233_rkind)
if(stab.eq.'GA') then
sh=0.49393/(1-34.676*Gh)
sm=(0.39327-3.0858*Gh)/(1-34.676*Gh)/(1-6.1272*Gh)
cmiu=sqrt(2.d0)*sm
cmiup=sqrt(2.d0)*sh
cmiu1=sqrt(2.d0)*0.2
cmiu2=sqrt(2.d0)*0.2
else if(stab.eq.'KC') then
Ghp=Gh
sh=0.4939/(1-30.19*Ghp)
sm=(0.392+17.07*sh*Ghp)/(1-6.127*Ghp)
cmiu=sqrt(2.d0)*sm
cmiup=sqrt(2.d0)*sh
cmiu1=cmiu/schk
cmiu2=cmiu/schpsi
else
write(errmsg,*)'Unknown ASM:',mid
call parallel_abort(errmsg)
endif
vd=cmiu*xl(i,j)*sqrt(q2(i,j))
td=cmiup*xl(i,j)*sqrt(q2(i,j))
qd1=cmiu1*xl(i,j)*sqrt(q2(i,j))
qd2=cmiu2*xl(i,j)*sqrt(q2(i,j))
end subroutine asm
function rint_lag(mnv,Nmin,Nmax,m,k,sigma,sigmap,sigma_prod,psi,gam,coef)
use elfe_glbl, only : rkind,errmsg
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
integer, intent(in) :: mnv,Nmin,Nmax,m,k
real(rkind), intent(in) :: sigma(mnv),sigmap(mnv,10),sigma_prod(mnv,mnv,-4:4),psi(mnv)
real(rkind), intent(out) :: gam(mnv),coef(0:mnv)
if(Nmin>=Nmax.or.Nmax>mnv.or.Nmin<1) then
write(errmsg,*)'Check inputs in rint_lag:',Nmin,Nmax
call parallel_abort(errmsg)
endif
if(k>Nmax-1.or.k<Nmin) then
write(errmsg,*)'Wrong k:',k
call parallel_abort(errmsg)
endif
if(m<1) then
write(errmsg,*)'m<1',m
call parallel_abort(errmsg)
endif
if(m>3) then
write(errmsg,*)'m>3 not covered presently'
call parallel_abort(errmsg)
endif
if(2*m+1>10) then
write(errmsg,*)'Re-dimension sigmap'
call parallel_abort(errmsg)
endif
j1=max0(Nmin,k-m)
j2=min0(Nmax,k+m)
if(j1>=j2) then
write(errmsg,*)'Weird indices:',j1,j2
call parallel_abort(errmsg)
endif
rint_lag=0
do i=j1,j2
id=0
do j=j1,j2
if(j/=i) then
id=id+1
gam(id)=-sigma(j)
endif
enddo
if(id/=j2-j1.or.id>2*m) then
write(errmsg,*)'Miscount:',id,j2-j1,m
call parallel_abort(errmsg)
endif
if(id==1) then
coef(0)=gam(1); coef(1)=1
else if(id==2) then
coef(0)=gam(1)*gam(2)
coef(1)=gam(1)+gam(2)
coef(2)=1
else if(id==3) then
coef(0)=gam(1)*gam(2)*gam(3)
coef(1)=gam(1)*(gam(2)+gam(3))+gam(2)*gam(3)
coef(2)=gam(1)+gam(2)+gam(3)
coef(3)=1
else if(id==4) then
coef(0)=gam(1)*gam(2)*gam(3)*gam(4)
coef(1)=gam(1)*gam(2)*(gam(3)+gam(4))+(gam(1)+gam(2))*gam(3)*gam(4)
coef(2)=gam(1)*(gam(2)+gam(3))+(gam(1)+gam(3))*gam(4)+gam(2)*(gam(3)+gam(4))
coef(3)=gam(1)+gam(2)+gam(3)+gam(4)
coef(4)=1
else if(id==5) then
coef(0)=gam(1)*gam(2)*gam(3)*gam(4)*gam(5)
coef(1)=gam(1)*gam(2)*gam(3)*gam(4)+gam(1)*gam(2)*gam(3)*gam(5)+gam(1)*gam(2)*gam(4)*gam(5)+ &
&gam(1)*gam(3)*gam(4)*gam(5)+gam(2)*gam(3)*gam(4)*gam(5)
coef(2)=gam(1)*gam(2)*gam(3)+gam(1)*gam(2)*gam(4)+gam(1)*gam(2)*gam(5)+gam(1)*gam(3)*gam(4)+ &
&gam(1)*gam(3)*gam(5)+gam(1)*gam(4)*gam(5)+gam(2)*gam(3)*gam(4)+gam(2)*gam(3)*gam(5)+ &
&gam(2)*gam(4)*gam(5)+gam(3)*gam(4)*gam(5)
coef(3)=gam(1)*gam(2)+gam(1)*gam(3)+gam(1)*gam(4)+gam(1)*gam(5)+gam(2)*gam(3)+ &
&gam(2)*gam(4)+gam(2)*gam(5)+gam(3)*gam(4)+gam(3)*gam(5)+gam(4)*gam(5)
coef(4)=gam(1)+gam(2)+gam(3)+gam(4)+gam(5)
coef(5)=1
else if(id==6) then
coef(0)=gam(1)*gam(2)*gam(3)*gam(4)*gam(5)*gam(6)
coef(1)=gam(1)*gam(2)*gam(3)*gam(4)*gam(5)+gam(1)*gam(2)*gam(3)*gam(4)*gam(6)+&
&gam(1)*gam(2)*gam(3)*gam(5)*gam(6)+gam(1)*gam(2)*gam(4)*gam(5)*gam(6)+ &
&gam(1)*gam(3)*gam(4)*gam(5)*gam(6)+gam(2)*gam(3)*gam(4)*gam(5)*gam(6)
coef(2)=gam(1)*gam(2)*gam(3)*gam(4)+gam(1)*gam(2)*gam(3)*gam(5)+gam(1)*gam(2)*gam(3)*gam(6)+ &
&gam(1)*gam(2)*gam(4)*gam(5)+gam(1)*gam(2)*gam(4)*gam(6)+gam(1)*gam(2)*gam(5)*gam(6)+ &
&gam(1)*gam(3)*gam(4)*gam(5)+gam(1)*gam(3)*gam(4)*gam(6)+gam(1)*gam(3)*gam(5)*gam(6)+ &
&gam(1)*gam(4)*gam(5)*gam(6)+gam(2)*gam(3)*gam(4)*gam(5)+gam(2)*gam(3)*gam(4)*gam(6)+ &
&gam(2)*gam(3)*gam(5)*gam(6)+gam(2)*gam(4)*gam(5)*gam(6)+gam(3)*gam(4)*gam(5)*gam(6)
coef(3)=gam(1)*gam(2)*gam(3)+gam(1)*gam(2)*gam(4)+gam(1)*gam(2)*gam(5)+ &
&gam(1)*gam(2)*gam(6)+gam(1)*gam(3)*gam(4)+gam(1)*gam(3)*gam(5)+gam(1)*gam(3)*gam(6)+ &
&gam(1)*gam(4)*gam(5)+gam(1)*gam(4)*gam(6)+gam(1)*gam(5)*gam(6)+gam(2)*gam(3)*gam(4)+ &
&gam(2)*gam(3)*gam(5)+gam(2)*gam(3)*gam(6)+gam(2)*gam(4)*gam(5)+gam(2)*gam(4)*gam(6)+ &
&gam(2)*gam(5)*gam(6)+gam(3)*gam(4)*gam(5)+gam(3)*gam(4)*gam(6)+gam(3)*gam(5)*gam(6)+ &
&gam(4)*gam(5)*gam(6)
coef(4)=gam(1)*gam(2)+gam(1)*gam(3)+gam(1)*gam(4)+gam(1)*gam(5)+gam(1)*gam(6)+ &
&gam(2)*gam(3)+gam(2)*gam(4)+gam(2)*gam(5)+gam(2)*gam(6)+gam(3)*gam(4)+gam(3)*gam(5)+ &
&gam(3)*gam(6)+gam(4)*gam(5)+gam(4)*gam(6)+gam(5)*gam(6)
coef(5)=gam(1)+gam(2)+gam(3)+gam(4)+gam(5)+gam(6)
coef(6)=1
else
write(errmsg,*)'Not covered:',id
call parallel_abort(errmsg)
endif
sum1=0
do l=0,id
sum1=sum1+coef(l)/(l+1)*(sigmap(k+1,l+1)-sigmap(k,l+1))
enddo
if(abs(i-k)>4) then
write(errmsg,*)'sigma_prod index out of bound (2)'
call parallel_abort(errmsg)
endif
rint_lag=rint_lag+psi(i)/sigma_prod(Nmin,k,i-k)*sum1
enddo
end function rint_lag
function lindex_s(i,ie)
use elfe_glbl, only : rkind,js
implicit real(rkind)(a-h,o-z), integer(i-n)
integer, intent(in) :: i,ie
l0=0
do l=1,3
if(js(ie,l)==i) then
l0=l
exit
endif
enddo
lindex_s=l0
end function lindex_s
function covar(kr_co,hh)
use elfe_glbl, only : rkind,errmsg
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
if(hh<0) then
write(errmsg,*)'Negative hh in covar:',hh
call parallel_abort(errmsg)
endif
if(kr_co==1) then
covar=-hh
else if(kr_co==2) then
if(hh==0) then
covar=0
else
covar=hh*hh*log(hh)
endif
else if(kr_co==3) then
covar=hh*hh*hh
else if(kr_co==4) then
h2=hh*hh
covar=-h2*h2*hh
else
write(errmsg,*)'Unknown covariance function option:',kr_co
call parallel_abort(errmsg)
endif
end function covar
subroutine eval_cubic_spline(npts,xcor,yy,ypp,npts2,xout,ixmin,xmin,xmax,yyout)
use elfe_glbl, only : rkind,errmsg
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
integer, intent(in) :: npts,npts2,ixmin
real(rkind), intent(in) :: xcor(npts),yy(npts),ypp(npts),xout(npts2),xmin,xmax
real(rkind), intent(out) :: yyout(npts2)
if(xmin>xmax) then
yyout=yy(1); return
endif
do i=1,npts2
ifl=0
xtmp=min(xout(i),xmax)
if(ixmin==0) then
xtmp=max(xtmp,xmin)
else
if(xout(i)<xcor(1)) then
yyout(i)=yy(1); cycle
endif
endif
do j=1,npts-1
if(xtmp>=xcor(j).and.xtmp<=xcor(j+1)) then
ifl=1
aa=(xcor(j+1)-xtmp)/(xcor(j+1)-xcor(j))
bb=1-aa
cc=(aa*aa*aa-aa)*(xcor(j+1)-xcor(j))/6
dd=(bb*bb*bb-bb)*(xcor(j+1)-xcor(j))/6
yyout(i)=aa*yy(j)+bb*yy(j+1)+cc*ypp(j)+dd*ypp(j+1)
exit
endif
enddo
if(ifl==0) then
write(errmsg,*)'EVAL_CUBIC: Falied to find:',i,xtmp,xmin,xmax
call parallel_abort(errmsg)
endif
enddo
end subroutine eval_cubic_spline
subroutine cubic_spline(npts,xcor,yy,yp1,yp2,ypp)
use elfe_glbl, only : rkind,errmsg
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
integer, intent(in) :: npts
real(rkind), intent(in) :: xcor(npts),yy(npts),yp1,yp2
real(rkind), intent(out) :: ypp(npts)
real(rkind) :: alow(npts),bdia(npts),cupp(npts),rrhs(npts,1),gam(npts)
do k=1,npts
if(k==1) then
bdia(k)=(xcor(k+1)-xcor(k))/3
if(bdia(k)==0) then
write(errmsg,*)'CUBIC_SP: bottom problem:',xcor(k+1),xcor(k)
call parallel_abort(errmsg)
endif
cupp(k)=bdia(k)/2
rrhs(k,1)=(yy(k+1)-yy(k))/(xcor(k+1)-xcor(k))-yp1
else if(k==npts) then
bdia(k)=(xcor(k)-xcor(k-1))/3
if(bdia(k)==0) then
write(errmsg,*)'CUBIC_SP: surface problem:',xcor(k),xcor(k-1)
call parallel_abort(errmsg)
endif
alow(k)=bdia(k)/2
rrhs(k,1)=-(yy(k)-yy(k-1))/(xcor(k)-xcor(k-1))+yp2
else
bdia(k)=(xcor(k+1)-xcor(k-1))/3
alow(k)=(xcor(k)-xcor(k-1))/6
cupp(k)=(xcor(k+1)-xcor(k))/6
if(alow(k)==0.or.cupp(k)==0) then
write(errmsg,*)'CUBIC_SP: middle problem:',xcor(k),xcor(k-1),xcor(k+1)
call parallel_abort(errmsg)
endif
rrhs(k,1)=(yy(k+1)-yy(k))/(xcor(k+1)-xcor(k))-(yy(k)-yy(k-1))/(xcor(k)-xcor(k-1))
endif
enddo
call tridag(npts,1,npts,1,alow,bdia,cupp,rrhs,ypp,gam)
end subroutine cubic_spline
subroutine do_cubic_spline(npts,xcor,yy,yp1,yp2,npts2,xout,ixmin,xmin,xmax,yyout)
use elfe_glbl, only : rkind,errmsg
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
integer, intent(in) :: npts,npts2,ixmin
real(rkind), intent(in) :: xcor(npts),yy(npts),yp1,yp2,xout(npts2),xmin,xmax
real(rkind), intent(out) :: yyout(npts2)
real(rkind) :: ypp(npts)
call cubic_spline(npts,xcor,yy,yp1,yp2,ypp)
call eval_cubic_spline(npts,xcor,yy,ypp,npts2,xout,ixmin,xmin,xmax,yyout)
end subroutine do_cubic_spline
subroutine mean_density
use elfe_glbl
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
real(rkind) :: swild(nvrt)
real(rkind), allocatable :: swild2(:,:,:)
allocate(swild2(nvrt,nea,2),stat=istat)
rho_mean=-99
if(iupwind_t==0) then
do i=1,npa
if(idry(i)==1) cycle
if(znl(kbp(i),i)<z_r(1)) then
call parallel_abort('MISC: 2.node depth too big for ts.ic')
endif
call eval_cubic_spline(nz_r,z_r,tem1,cspline_ypp(1:nz_r,1),nvrt-kbp(i)+1,znl(kbp(i):nvrt,i), &
&0,z_r(1),z_r(nz_r),swild2(kbp(i):nvrt,i,1))
call eval_cubic_spline(nz_r,z_r,sal1,cspline_ypp(1:nz_r,2),nvrt-kbp(i)+1,znl(kbp(i):nvrt,i), &
&0,z_r(1),z_r(nz_r),swild2(kbp(i):nvrt,i,2))
if(Cdp(i)/=0) then
swild2(kbp(i),i,1:2)=swild2(kbp(i)+1,i,1:2)
endif
do k=1,kbp(i)-1
swild2(k,i,1:2)=swild2(kbp(i),i,1:2)
enddo
do k=1,nvrt
rho_mean(k,i)=eqstate(swild2(k,i,1),swild2(k,i,2))
enddo
enddo
else
do i=1,nea
if(idry_e(i)==1) cycle
if(ze(kbe(i),i)<z_r(1)) then
call parallel_abort('MISC: 2.ele. depth too big for ts.ic')
endif
do k=kbe(i)+1,nvrt
swild(k)=(ze(k,i)+ze(k-1,i))/2
enddo
call eval_cubic_spline(nz_r,z_r,tem1,cspline_ypp(1:nz_r,1),nvrt-kbe(i),swild(kbe(i)+1:nvrt), &
&0,z_r(1),z_r(nz_r),swild2(kbe(i)+1:nvrt,i,1))
call eval_cubic_spline(nz_r,z_r,sal1,cspline_ypp(1:nz_r,2),nvrt-kbe(i),swild(kbe(i)+1:nvrt), &
&0,z_r(1),z_r(nz_r),swild2(kbe(i)+1:nvrt,i,2))
do k=1,kbe(i)
swild2(k,i,1:2)=swild2(kbe(i)+1,i,1:2)
enddo
do k=1,nvrt
rho_mean(k,i)=eqstate(swild2(k,i,1),swild2(k,i,2))
enddo
enddo
endif
deallocate(swild2)
end subroutine mean_density
function kronecker(i,j)
implicit integer(i-n)
integer, intent(in) :: i,j
if(i==j) then
kronecker=1
else
kronecker=0
endif
end function kronecker
subroutine hgrad_nodes(ihbnd,nvrt1,npa1,nsa1,var_nd,dvar_dxy)
use elfe_glbl
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
integer, intent(in) :: ihbnd
integer, intent(in) :: nvrt1,npa1,nsa1
real(rkind), intent(in) :: var_nd(nvrt1,npa1)
real(rkind), intent(out) :: dvar_dxy(2,nvrt1,nsa1)
real(rkind) :: hp_int(nvrt1,npa1),swild(nvrt1),swild2(nvrt1,4)
integer :: nwild(3)
hp_int=0
do i=1,npa
if(idry(i)==1) cycle
call cubic_spline(nvrt-kbp(i)+1,znl(kbp(i):nvrt,i),var_nd(kbp(i):nvrt,i),0._rkind,0._rkind,swild)
hp_int(kbp(i):nvrt,i)=swild(1:(nvrt-kbp(i)+1))
enddo
dvar_dxy=0
do i=1,ns
if(idry_s(i)==1) cycle
node1=isidenode(i,1); node2=isidenode(i,2)
eta_min=min(znl(nvrt,node1),znl(nvrt,node2))
zmax=max(znl(kbp(node1),node1),znl(kbp(node2),node2))
if(-zmax>h_bcc1) then
ibot_fl=0
else
ibot_fl=1
endif
call eval_cubic_spline(nvrt-kbp(node1)+1,znl(kbp(node1):nvrt,node1),var_nd(kbp(node1):nvrt,node1), &
&hp_int(kbp(node1):nvrt,node1),nvrt-kbs(i)+1,zs(kbs(i):nvrt,i),ibot_fl,zmax,eta_min,swild)
swild2(kbs(i):nvrt,1)=swild(1:(nvrt-kbs(i)+1))
call eval_cubic_spline(nvrt-kbp(node2)+1,znl(kbp(node2):nvrt,node2),var_nd(kbp(node2):nvrt,node2), &
&hp_int(kbp(node2):nvrt,node2),nvrt-kbs(i)+1,zs(kbs(i):nvrt,i),ibot_fl,zmax,eta_min,swild)
swild2(kbs(i):nvrt,2)=swild(1:(nvrt-kbs(i)+1))
if(is(i,2)==0.and.ihbnd==0) then
swild2(kbs(i):nvrt,3:4)=0
if(ics==1) then
xn1=xnd(node1)
yn1=ynd(node1)
xn2=xnd(node2)
yn2=ynd(node2)
else
call project_pt('g2l',xnd(node1),ynd(node1),znd(node1), &
&(/xcj(i),ycj(i),zcj(i)/),sframe(:,:,i),xn1,yn1,tmp)
call project_pt('g2l',xnd(node2),ynd(node2),znd(node2), &
&(/xcj(i),ycj(i),zcj(i)/),sframe(:,:,i),xn2,yn2,tmp)
endif
x43=yn2-yn1
y43=xn1-xn2
else if(is(i,2)==0.and.ihbnd/=0) then
ie=is(i,1)
node3=sum(nm(ie,1:3))-node1-node2
if(idry(node3)==1) then
write(errmsg,*)'hgrad_nodes: node3 dry',iplg(node3),ielg(ie)
call parallel_abort(errmsg)
endif
nwild=0
do j=1,3
if(j<=2) then
nd=isidenode(i,j)
else
nd=node3
endif
do jj=1,3
if(nm(ie,jj)==nd) then
nwild(j)=jj; exit
endif
enddo
if(nwild(j)==0) then
write(errmsg,*)'hgrad_nodes: no index found:',iplg(nd),ielg(ie)
call parallel_abort(errmsg)
endif
enddo
eta_min=znl(nvrt,node3)
zmax=znl(kbp(node3),node3)
if(-zmax>h_bcc1) then
ibot_fl=0
else
ibot_fl=1
endif
call eval_cubic_spline(nvrt-kbp(node3)+1,znl(kbp(node3):nvrt,node3),var_nd(kbp(node3):nvrt,node3), &
&hp_int(kbp(node3):nvrt,node3),nvrt-kbs(i)+1,zs(kbs(i):nvrt,i),ibot_fl,zmax,eta_min,swild)
swild2(kbs(i):nvrt,3)=swild(1:(nvrt-kbs(i)+1))
do k=kbs(i),nvrt
do j=1,3
dvar_dxy(1:2,k,i)=dvar_dxy(1:2,k,i)+swild2(k,j)*dl(ie,nwild(j),1:2)
enddo
if(ics==2) then
call project_hvec(dvar_dxy(1,k,i),dvar_dxy(2,k,i),eframe(:,:,ie),sframe(:,:,i),tmp1,tmp2)
dvar_dxy(1,k,i)=tmp1
dvar_dxy(2,k,i)=tmp2
endif
enddo
else
node3=sum(nm(is(i,1),1:3))-node1-node2
node4=sum(nm(is(i,2),1:3))-node1-node2
if(ics==1) then
xn3=xnd(node3)
yn3=ynd(node3)
xn4=xnd(node4)
yn4=ynd(node4)
else
call project_pt('g2l',xnd(node3),ynd(node3),znd(node3), &
&(/xcj(i),ycj(i),zcj(i)/),sframe(:,:,i),xn3,yn3,tmp)
call project_pt('g2l',xnd(node4),ynd(node4),znd(node4), &
&(/xcj(i),ycj(i),zcj(i)/),sframe(:,:,i),xn4,yn4,tmp)
endif
x43=xn4-xn3
y43=yn4-yn3
if(idry(node3)==1.or.idry(node4)==1) then
swild2(kbs(i):nvrt,3:4)=0
else
eta_min=min(znl(nvrt,node3),znl(nvrt,node4))
zmax=max(znl(kbp(node3),node3),znl(kbp(node4),node4))
if(-zmax>h_bcc1) then
ibot_fl=0
else
ibot_fl=1
endif
call eval_cubic_spline(nvrt-kbp(node3)+1,znl(kbp(node3):nvrt,node3),var_nd(kbp(node3):nvrt,node3), &
&hp_int(kbp(node3):nvrt,node3),nvrt-kbs(i)+1,zs(kbs(i):nvrt,i),ibot_fl,zmax,eta_min,swild)
swild2(kbs(i):nvrt,3)=swild(1:(nvrt-kbs(i)+1))
call eval_cubic_spline(nvrt-kbp(node4)+1,znl(kbp(node4):nvrt,node4),var_nd(kbp(node4):nvrt,node4), &
&hp_int(kbp(node4):nvrt,node4),nvrt-kbs(i)+1,zs(kbs(i):nvrt,i),ibot_fl,zmax,eta_min,swild)
swild2(kbs(i):nvrt,4)=swild(1:(nvrt-kbs(i)+1))
endif
endif
if(ihbnd==0.or.is(i,2)/=0) then
if(ics==1) then
xn1=xnd(node1)
yn1=ynd(node1)
xn2=xnd(node2)
yn2=ynd(node2)
else
call project_pt('g2l',xnd(node1),ynd(node1),znd(node1), &
&(/xcj(i),ycj(i),zcj(i)/),sframe(:,:,i),xn1,yn1,tmp)
call project_pt('g2l',xnd(node2),ynd(node2),znd(node2), &
&(/xcj(i),ycj(i),zcj(i)/),sframe(:,:,i),xn2,yn2,tmp)
endif
delta1=(xn2-xn1)*y43-x43*(yn2-yn1)
if(delta1==0) then
write(errmsg,*)'hgrad_nodes failure:',iplg(node1),iplg(node2)
call parallel_abort(errmsg)
endif
do k=kbs(i),nvrt
dvar_dxy(1,k,i)=(y43*(swild2(k,2)-swild2(k,1))-(yn2-yn1)*(swild2(k,4)-swild2(k,3)))/delta1
dvar_dxy(2,k,i)=((xn2-xn1)*(swild2(k,4)-swild2(k,3))-x43*(swild2(k,2)-swild2(k,1)))/delta1
enddo
endif
enddo
end subroutine hgrad_nodes
subroutine update_bdef(time,x0,y0,dep,vel)
use elfe_glbl, only: rkind
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
real(rkind), intent(in) :: time,x0,y0
real(rkind), intent(out) :: dep,vel(3)
dep=min(1.,7.-real((x0+time)))
vel(1)=-1
vel(2)=0
vel(3)=0
end subroutine update_bdef
subroutine project_pt(dir,xi,yi,zi,origin0,frame0,xo,yo,zo)
use elfe_glbl, only: rkind
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z),integer(i-n)
character(len=3), intent(in) :: dir
real(rkind), intent(in) :: xi,yi,zi,origin0(3),frame0(3,3)
real(rkind), intent(out) :: xo,yo,zo
real(rkind) :: wild(3)
if(dir.eq.'g2l') then
wild(1:3)=(xi-origin0(1))*frame0(1,1:3)+(yi-origin0(2))*frame0(2,1:3)+ &
&(zi-origin0(3))*frame0(3,1:3)
else if(dir.eq.'l2g') then
wild(1:3)=origin0(1:3)+xi*frame0(1:3,1)+yi*frame0(1:3,2)+ &
&zi*frame0(1:3,3)
else
call parallel_abort('PROJECT_PT: unknown flag')
endif
xo=wild(1)
yo=wild(2)
zo=wild(3)
end subroutine project_pt
subroutine project_hvec(u0,v0,frame0,frameout,u1,v1)
use elfe_glbl, only: rkind
implicit real(rkind)(a-h,o-z),integer(i-n)
real(rkind), intent(in) :: u0,v0,frame0(3,3),frameout(3,3)
real(rkind), intent(out) ::u1,v1
u1=u0*dot_product(frame0(:,1),frameout(:,1))+v0*dot_product(frame0(:,2),frameout(:,1))
v1=u0*dot_product(frame0(:,1),frameout(:,2))+v0*dot_product(frame0(:,2),frameout(:,2))
end subroutine project_hvec
subroutine cross_product(x1,y1,z1,x2,y2,z2,x3,y3,z3)
use elfe_glbl, only : rkind
implicit none
real(rkind),intent(in) :: x1,y1,z1,x2,y2,z2
real(rkind),intent(out) :: x3,y3,z3
x3=y1*z2-y2*z1
y3=x2*z1-x1*z2
z3=x1*y2-x2*y1
end subroutine cross_product
subroutine compute_ll(xg,yg,zg,rlon,rlat)
use elfe_glbl, only : rkind,pi,errmsg
use elfe_msgp, only : parallel_abort
implicit none
real(rkind),intent(in) :: xg,yg,zg
real(rkind),intent(out) :: rlon,rlat
real(rkind) :: rad
rad=sqrt(xg*xg+yg*yg+zg*zg)
if(rad==0.or.abs(zg)>rad) then
write(errmsg,*)'COMPUTE_LL: rad=0:',xg,yg,zg,rad
call parallel_abort(errmsg)
endif
rlon=atan2(yg,xg)
rlat=asin(zg/rad)
end subroutine compute_ll
subroutine zonal_flow
use elfe_glbl
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
dimension swild10(3,3)
alpha_zonal=0
omega_zonal=2*pi/12/86400
gh0=grav*5960
u00_zonal=20
do i=1,nsa
n1=isidenode(i,1); n2=isidenode(i,2)
call compute_ll(xcj(i),ycj(i),zcj(i),xtmp,ytmp)
uzonal=u00_zonal*(cos(ytmp)*cos(alpha_zonal)+cos(xtmp)*sin(ytmp)*sin(alpha_zonal))
vmer=-u00_zonal*sin(xtmp)*sin(alpha_zonal)
swild10(1:3,1:3)=(pframe(:,:,n1)+pframe(:,:,n2))/2
call project_hvec(uzonal,vmer,swild10(1:3,1:3),sframe(:,:,i),utmp,vtmp)
su2(:,i)=utmp
sv2(:,i)=vtmp
enddo
do i=1,npa
gh=gh0-(rearth*omega_e*u00_zonal+u00_zonal**2/2)* &
&(sin(ylat(i))*cos(alpha_zonal)-cos(xlon(i))*cos(ylat(i))*sin(alpha_zonal))**2
eta2(i)=gh/grav
uzonal=u00_zonal*(cos(ylat(i))*cos(alpha_zonal)+cos(xlon(i))*sin(ylat(i))*sin(alpha_zonal))
vmer=-u00_zonal*sin(xlon(i))*sin(alpha_zonal)
uu2(:,i)=uzonal
vv2(:,i)=vmer
enddo
ww2=0
do i=1,nea
do j=1,3
nd=nm(i,j)
uzonal=u00_zonal*(cos(ylat(nd))*cos(alpha_zonal)+cos(xlon(nd))*sin(ylat(nd))*sin(alpha_zonal))
vmer=-u00_zonal*sin(xlon(nd))*sin(alpha_zonal)
call project_hvec(uzonal,vmer,pframe(:,:,nd),eframe(:,:,i),utmp,vtmp)
ufg(:,i,j)=utmp
vfg(:,i,j)=vtmp
enddo
enddo
we=0
we_fv=0
end subroutine zonal_flow
function u_compactzonal(rlat,u00_zonal)
use elfe_glbl, only : rkind,errmsg,pi
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
real(rkind), intent(in) :: rlat,u00_zonal
xe=0.3
phib=-pi/6
phie=pi/2
x=xe*(rlat-phib)/(phie-phib)
if(x<=0) then
b1=0
else
b1=exp(-1/x)
endif
if(xe-x<=0) then
b2=0
else
b2=exp(-1/(xe-x))
endif
u_compactzonal=u00_zonal*b1*b2*exp(4/xe)
end function u_compactzonal
subroutine area_coord(ifl,nnel,gcor0,frame0,xt,yt,arco)
use elfe_glbl
use elfe_msgp, only : parallel_abort
implicit real(rkind)(a-h,o-z), integer(i-n)
integer, intent(in) :: ifl
integer, intent(in) :: nnel
real(rkind), intent(in) :: gcor0(3),frame0(3,3)
real(rkind), intent(inout) :: xt,yt
real(rkind), intent(out) :: arco(3)
real(rkind) :: wild(3,2)
do j=1,3
nd=nm(nnel,j)
if(ics==1) then
wild(j,1)=xnd(nd)
wild(j,2)=ynd(nd)
else
call project_pt('g2l',xnd(nd),ynd(nd),znd(nd),gcor0,frame0,wild(j,1),wild(j,2),tmp)
endif
enddo
arco(1)=signa(xt,wild(2,1),wild(3,1),yt,wild(2,2),wild(3,2))/area(nnel)
arco(2)=signa(wild(1,1),xt,wild(3,1),wild(1,2),yt,wild(3,2))/area(nnel)
arco(3)=1-arco(1)-arco(2)
tmpmin=minval(arco)
if(ifl==1.and.tmpmin<=0) then
indx=0
tmpmax=-1
do j=1,3
if(arco(j)>tmpmax) then
tmpmax=arco(j)
indx=j
endif
if(arco(j)<=0) arco(j)=1.e-4
enddo
if(indx==0) call parallel_abort('AREA_COORD: failed')
tmpsum=0
do j=1,3
if(j/=indx) tmpsum=tmpsum+arco(j)
enddo
arco(indx)=1-tmpsum
if(arco(indx)<=0) then
write(errmsg,*)'AREA_COORD: failed to fix',arco(1:3)
call parallel_abort(errmsg)
endif
xt=dot_product(wild(:,1),arco)
yt=dot_product(wild(:,2),arco)
endif
end subroutine area_coord
