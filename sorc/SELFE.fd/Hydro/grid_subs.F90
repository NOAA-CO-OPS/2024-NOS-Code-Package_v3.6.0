function lindex(node,ie)
use elfe_glbl
use elfe_msgp, only : parallel_abort
implicit none
integer :: lindex
integer,intent(in) :: node,ie
integer :: j
lindex=0
do j=1,3
if(node==nm(ie,j)) lindex=j
enddo
end function lindex
function signa(x1,x2,x3,y1,y2,y3)
use elfe_glbl, only : rkind,errmsg
implicit none
real(rkind) :: signa
real(rkind),intent(in) :: x1,x2,x3,y1,y2,y3
signa=((x1-x3)*(y2-y3)-(x2-x3)*(y1-y3))/2d0
end function signa
subroutine dump_hgrid
use elfe_glbl
use elfe_msgp
implicit none
integer, parameter :: maxbuf=max(100,3)
integer :: ie,ip,i,j,k,ngb1,ngb2,isd,isdgb,iegb1,iegb2
integer :: ibuf1(maxbuf),ibuf2(maxbuf),ibuf3(maxbuf)
type(llist_type),pointer :: llp
fdb='helem_0000'
lfdb=len_trim(fdb)
write(fdb(lfdb-3:lfdb),'(i4.4)') myrank
open(10,file='outputs/'//fdb,status='unknown')
write(10,'(a,4i10)') '#',nea,ne,neg
do ie=1,nea
j=0
llp=>iegl(ielg(ie))
do
j=j+1
if(j>maxbuf) call parallel_abort('Increase buffer size in dump_hgrid (1)')
ibuf1(j)=llp%rank
ibuf2(j)=llp%id
llp=>llp%next
if(.not.associated(llp)) exit
enddo
if(ie<=ne) then
write(10,'(a,2i8,4e14.6)') 'Element ',ie,ielg(ie),xctr(ie),yctr(ie),zctr(ie),dpe(ie)
else
write(10,'(a,2i8,4e14.6)') '# Element ',ie,ielg(ie),xctr(ie),yctr(ie),zctr(ie),dpe(ie)
endif
write(10,'(a,3i8)') '####NODE: ',(iplg(nm(ie,k)),k=1,3)
do k=1,3
if(ic3(ie,k)>0) then
ibuf3(k)=ielg(ic3(ie,k))
elseif(ic3(ie,k)<0) then
if(ie<=ne) then
write(errmsg,*)'Resident element having wrong nbr:',ie,ielg(ie),myrank
call parallel_abort(errmsg)
endif
ibuf3(k)=ic3(ie,k)
else
ibuf3(k)=0
endif
if(nm(ie,k)<=0.or.js(ie,k)<=0) then
write(errmsg,*)'Check nm or js:',ielg(ie),(nm(ie,ip),js(ie,ip),ip=1,3)
call parallel_abort(errmsg)
endif
enddo
write(10,'(a,3i8)') '####IC3: ',(ibuf3(k),k=1,3)
write(10,'(a,3i8)') '####JS: ',(islg(js(ie,k)),k=1,3)
write(10,'(a,3i8)') '####SSIGN: ',(int(ssign(ie,k)),k=1,3)
write(10,'(a,64i8)') '####PList:',(ibuf1(k),ibuf2(k),k=1,j)
enddo
close(10)
fdb='hnode_0000'
lfdb=len_trim(fdb)
write(fdb(lfdb-3:lfdb),'(i4.4)') myrank
open(10,file='outputs/'//fdb,status='unknown')
write(10,'(a,4i10)') '#',npa,np,npg
do ip=1,npa
j=0
llp=>ipgl(iplg(ip))
do
j=j+1
if(j>maxbuf) call parallel_abort('Increase buffer size in dump_hgrid: node (1)')
ibuf1(j)=llp%rank
ibuf2(j)=llp%id
llp=>llp%next
if(.not.associated(llp)) exit
enddo
if(nnp(ip)>maxbuf.or.nne(ip)>maxbuf) call parallel_abort('Increase buffer size in dump_hgrid: node (2)')
do k=1,nne(ip)
if(ine(ip,k)>0) then
ibuf3(k)=ielg(ine(ip,k))
elseif(ine(ip,k)<0) then
if(ip<=np) then
write(errmsg,*)'Surrounding element outside:',ine(ip,k),iplg(ip),k
call parallel_abort(errmsg)
endif
ibuf3(k)=ine(ip,k)
else
write(errmsg,*)'Surrounding element not exist:',ine(ip,k),iplg(ip),k
call parallel_abort(errmsg)
endif
enddo
if(ip<=np) then
write(10,'(a,2i8,4e14.6,2i4,50(i8,i4))') 'Node ',ip,iplg(ip),xnd(ip),ynd(ip),znd(ip),dp(ip), &
isbnd(-2:2,ip),nne(ip),(ibuf3(k),iself(ip,k),k=1,nne(ip))
else
write(10,'(a,2i8,4e14.6,2i4,50(i8,i4))') '# Node ',ip,iplg(ip),xnd(ip),ynd(ip),znd(ip),dp(ip), &
isbnd(-2:2,ip),nne(ip),(ibuf3(k),iself(ip,k),k=1,nne(ip))
endif
write(10,'(a,64i8)') '####PList:',(ibuf1(k),ibuf2(k),k=1,j)
do k=1,nnp(ip)
if(inp(ip,k)>0) then
ibuf3(k)=iplg(inp(ip,k))
elseif(inp(ip,k)<0) then
if(ip<=np) then
write(errmsg,*)'Surrounding node outside:',inp(ip,k),iplg(ip),k
call parallel_abort(errmsg)
endif
ibuf3(k)=inp(ip,k)
else
write(errmsg,*)'Surrounding node not exist:',inp(ip,k),iplg(ip),k
call parallel_abort(errmsg)
endif
enddo
write(10,'(a,64i8)')'Nbr nodes:',(ibuf3(k),k=1,nnp(ip))
enddo
close(10)
fdb='hside_0000'
lfdb=len_trim(fdb)
write(fdb(lfdb-3:lfdb),'(i4.4)') myrank
open(10,file='outputs/'//fdb,status='unknown')
write(10,'(a,4i10)') '#',nsa,ns,nsg
do isd=1,nsa
isdgb=islg(isd)
ngb1=iplg(isidenode(isd,1)); ngb2=iplg(isidenode(isd,2));
if(is(isd,1)>0) then
iegb1=ielg(is(isd,1))
else if(is(isd,1)<0) then
iegb1=is(isd,1)
else
write(errmsg,*)'is(:,1) =0:',ngb1,ngb2
call parallel_abort(errmsg)
endif
if(is(isd,2)>0) then; iegb2=ielg(is(isd,2)); else; iegb2=is(isd,2); endif;
j=0
llp=>isgl(islg(isd))
do
j=j+1
ibuf1(j)=llp%rank
ibuf2(j)=llp%id
llp=>llp%next
if(.not.associated(llp)) exit
enddo
if(isd<=ns) then
write(10,'(a,6i8,7e14.6,i4)') 'Side ',isd,isdgb,ngb1,ngb2,iegb1,iegb2, &
xcj(isd),ycj(isd),zcj(isd),dps(isd),distj(isd),sframe(1,1,isd),sframe(2,1,isd),isbs(isd)
else
write(10,'(a,6i8,7e14.6,i4)') '# Side', isd,isdgb,ngb1,ngb2,iegb1,iegb2, &
xcj(isd),ycj(isd),zcj(isd),dps(isd),distj(isd),sframe(1,1,isd),sframe(2,1,isd),isbs(isd)
endif
write(10,'(a,64i8)') '####PList:',(ibuf1(k),ibuf2(k),k=1,j)
enddo
close(10)
fdb='bndinfo_0000'
lfdb=len_trim(fdb)
write(fdb(lfdb-3:lfdb),'(i4.4)') myrank
open(10,file='outputs/'//fdb,status='unknown')
write(10,'(a,i10)') 'Open bnd:',nope
do i=1,nope
write(10,*)'open bnd #',i,iopelg(i),(iplg(iond(i,j)),j=1,nond(i))
enddo
write(10,'(a,i10)') 'Land bnd:',nland
do i=1,nland
write(10,*)'land bnd #',i,(iplg(ilnd(i,j)),j=1,nlnd(i))
enddo
close(10)
if(myrank==0) then
open(32,file='outputs/global_to_local.prop',status='unknown')
write(32,'(i8,1x,i4)')(ie,iegrpv(ie),ie=1,ne_global)
close(32)
endif
return
end subroutine dump_hgrid
subroutine partition_hgrid
#ifdef USE_MPIMODULE
use mpi
#endif
use elfe_glbl
use elfe_msgp
implicit none
#ifndef USE_MPIMODULE
include 'mpif.h'
#endif
integer :: wgtflag,numflag,ncon,ndims,options(0:2)
integer :: mxnedge,nedge,ntedge,edgecut
integer,allocatable :: vtxdist(:),xadj(:),adjncy(:),part(:)
real(4),allocatable :: xyz(:),xproj(:),yproj(:)
integer,allocatable :: nlev(:),vwgt(:),adjwgt(:)
real(4),allocatable :: tpwgts(:),ubvec(:)
integer :: i,j,k,l,ip,ie,je,iegb,jegb,stat,kbetmp
real(rkind) :: xtmp,ytmp,dtmp,tmp,etmp,ptmp,stmp
integer,allocatable :: eprocv(:),neproc(:),neprocsum(:)
logical :: found
open(14,file='hgrid.gr3',status='old')
read(14,*); read(14,*) ne_global,np_global
close(14)
if(allocated(iegrpv)) deallocate(iegrpv)
allocate(iegrpv(ne_global),stat=stat)
if(stat/=0) call parallel_abort('partition: iegrpv allocation failure')
if(nproc==1) then
ne=ne_global
iegrpv=0
return
endif
allocate(neproc(0:nproc-1),stat=stat)
if(stat/=0) call parallel_abort('partition: neproc allocation failure')
allocate(neprocsum(0:nproc-1),stat=stat)
if(stat/=0) call parallel_abort('partition: neprocsum allocation failure')
ne=ne_global/nproc
neprocsum(0)=0
do l=0,nproc-2
neproc(l)=ne
neprocsum(l+1)=neprocsum(l)+ne
enddo
neproc(nproc-1)=ne_global-neprocsum(nproc-1)
do i=1,ne_global
iegrpv(i)=min((i-1)/ne,nproc-1)
enddo
ne=neproc(myrank)

call aquire_hgrid(.false.)

if(allocated(xproj)) deallocate(xproj)
if(allocated(yproj)) deallocate(yproj)
allocate(xproj(npa),yproj(npa),stat=stat)
if(stat/=0) call parallel_abort('partition: xproj allocation failure')
if(ics==1) then
xproj=xnd; yproj=ynd
else
do i=1,npa
if(rearth+znd(i)<=0) then
write(errmsg,*)'PARTITION: remove south pole:',i,rearth+znd(i)
call parallel_abort(errmsg)
endif
xproj(i)=2*rearth*xnd(i)/(rearth+znd(i))
yproj(i)=2*rearth*ynd(i)/(rearth+znd(i))
enddo
endif

if(1) then !read in partitioning
  open(10,file='global_to_local.prop',status='old')
  do i=1,ne_global
    read(10,*)l,iegrpv(i)
  enddo !i
  close(10)

else !bypass ParMETIS

allocate(adjncy(100),stat=stat)
if(stat/=0) call parallel_abort('partition: adjncy(100) allocation failure')
ntedge=0
mxnedge=0
do ie=1,ne
iegb=ielg(ie)
nedge=0
adjncy=0
do j=1,3
ip=nm(ie,j)
do k=1,nne(ip)
jegb=ielg(ine(ip,k))
if(jegb/=iegb) then
found=.false.
do l=1,nedge
if(adjncy(l)==jegb) then
found=.true.
exit
endif
enddo
if(.not.found) then
nedge=nedge+1
adjncy(nedge)=jegb
endif
endif
enddo
enddo
ntedge=ntedge+nedge
mxnedge=max(mxnedge,nedge)
enddo
deallocate(adjncy)
wgtflag = 3
ncon = 4
if(allocated(xadj)) deallocate(xadj); allocate(xadj(nea+1),stat=stat);
if(stat/=0) call parallel_abort('partition: xadj allocation failure')
if(allocated(adjncy)) deallocate(adjncy); allocate(adjncy(ntedge),stat=stat)
if(stat/=0) call parallel_abort('partition: adjncy allocation failure')
if(allocated(xyz)) deallocate(xyz); allocate(xyz(2*nea),stat=stat)
if(stat/=0) call parallel_abort('partition: xyz allocation failure')
if(allocated(nlev)) deallocate(nlev); allocate(nlev(nea),stat=stat)
if(stat/=0) call parallel_abort('partition: nlev allocation failure')
if(wgtflag==2.or.wgtflag==3) then
if(allocated(vwgt)) deallocate(vwgt); allocate(vwgt(nea*ncon),stat=stat)
if(stat/=0) call parallel_abort('partition: vwgt allocation failure')
endif
if(wgtflag==1.or.wgtflag==3) then
if(allocated(adjwgt)) deallocate(adjwgt); allocate(adjwgt(ntedge),stat=stat)
if(stat/=0) call parallel_abort('partition: adjwgt allocation failure')
endif
do ie=1,nea
xtmp=0d0
ytmp=0d0
dtmp=5.d10
do j=1,3
ip=nm(ie,j)
xtmp=xtmp+xproj(ip)/3.d0
ytmp=ytmp+yproj(ip)/3.d0
if(dp(ip)<dtmp) dtmp=dp(ip)
enddo
xyz(2*(ie-1)+1)=xtmp
xyz(2*(ie-1)+2)=ytmp
if(wgtflag==2.or.wgtflag==3) then
if(dtmp<=0) then
kbetmp=nvrt
else if(dtmp<=h_s) then
kbetmp=kz
else
kbetmp=0
do j=1,kz-1
if(-dtmp>=ztot(j).and.-dtmp<ztot(j+1)) then
kbetmp=j
exit
endif
enddo
endif
nlev(ie)=max(1,nvrt-kbetmp)
etmp=1d0; ptmp=0d0; stmp=0d0;
do j=1,3
ip=nm(ie,j)
ptmp=ptmp+1d0/dble(nne(ip))
if(ic3(ie,j)/=0) then
stmp=stmp+0.5d0
else
stmp=stmp+1.0d0
endif
enddo
etmp=etmp*dble(nlev(ie))
ptmp=ptmp*dble(nlev(ie))
stmp=stmp*dble(nlev(ie))
vwgt(ncon*(ie-1)+1)=nint(etmp)
vwgt(ncon*(ie-1)+2)=nint(ptmp)
vwgt(ncon*(ie-1)+3)=nint(stmp)
vwgt(ncon*(ie-1)+4)=1
endif
enddo
adjncy=0
xadj(1)=1
do ie=1,ne
iegb=ielg(ie)
nedge=0
do j=1,3
ip=nm(ie,j)
do k=1,nne(ip)
je=ine(ip,k)
jegb=ielg(je)
if(jegb/=iegb) then
found=.false.
do l=xadj(ie),xadj(ie)+nedge-1
if(adjncy(l)==jegb) then
if(wgtflag==1.or.wgtflag==3) adjwgt(l)=nlev(je)*(2*3-3)
found=.true.
exit
endif
enddo
if(.not.found) then
adjncy(xadj(ie)+nedge)=jegb
if(wgtflag==1.or.wgtflag==3) adjwgt(xadj(ie)+nedge)=nlev(je)*(2*3-1)
nedge=nedge+1
endif
endif
enddo
enddo
xadj(ie+1)=xadj(ie)+nedge
enddo
allocate(vtxdist(nproc+1),stat=stat)
if(stat/=0) call parallel_abort('partition: vtxdist allocation failure')
call mpi_allgather(neprocsum(myrank)+1,1,itype,vtxdist,1,itype,comm,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('partition: mpi_allgather',ierr)
vtxdist(nproc+1)=ne_global+1
numflag = 1
ndims = 2
allocate(tpwgts(ncon*nproc),stat=stat)
if(stat/=0) call parallel_abort('partition: tpwgts allocation failure')
tpwgts=1.0/real(nproc)
allocate(ubvec(ncon),stat=stat)
if(stat/=0) call parallel_abort('partition: ubvec allocation failure')
ubvec=1.01
#ifdef DEBUG
options(0)=1
options(1)=15
options(2)=15
if(myrank==0) write(*,'(/a)') 'ParMETIS Partitioning:'
#else
options(0)=1
options(1)=0
options(2)=15
#endif
allocate(part(ne),stat=stat)
if(stat/=0) call parallel_abort('partition: part allocation failure')
call ParMETIS_V3_PartGeomKway(vtxdist,xadj,adjncy,vwgt,adjwgt,wgtflag, &
numflag,ndims,xyz,ncon,nproc,tpwgts,ubvec,options, &
edgecut,part,comm)
part=part-1
call mpi_allgatherv(part,ne,itype,iegrpv,neproc,neprocsum,itype,comm,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('partition: mpi_allgatherv',ierr)
deallocate(vtxdist,xadj,adjncy,part)
deallocate(xyz,nlev,tpwgts,ubvec)
if(wgtflag==2.or.wgtflag==3) deallocate(vwgt)
if(wgtflag==1.or.wgtflag==3) deallocate(adjwgt)

endif !bypass ParMETIS

deallocate(neproc,neprocsum,xproj,yproj)
end subroutine partition_hgrid

subroutine aquire_vgrid
use elfe_glbl
use elfe_msgp
implicit none
integer :: i,j,k,l,jki,stat,kin
real(rkind) :: buf1(100),hmod2,zz
ivcor=2
if(lm2d) then
nvrt=2; kz=1; nsig=2; h_s=1.e6; h_c=h_s
theta_b=0; theta_f=1.e-4; s_con1=sinh(theta_f)
allocate(ztot(nvrt),sigma(nvrt),cs(nvrt),dcs(nvrt),stat=stat)
if(stat/=0) call parallel_abort('VGRID: ztot allocation failure')
ztot(kz)=-h_s
sigma(1)=-1
sigma(nsig)=0
else
open(19,file='vgrid.in',status='old',iostat=stat)
if(stat/=0) call parallel_abort('AQUIRE_VGIRD: open(19) failure')
read(19,*) nvrt,kz,h_s
if(nvrt<3) call parallel_abort('nvrt<3')
if(kz<1.or.kz>nvrt-2) then
write(errmsg,*)'Wrong kz:',kz
call parallel_abort(errmsg)
endif
if(h_s<10) then
write(errmsg,*)'h_s needs to be larger:',h_s
call parallel_abort(errmsg)
endif
allocate(ztot(nvrt),sigma(nvrt),cs(nvrt),dcs(nvrt),stat=stat)
if(stat/=0) call parallel_abort('AQUIRE_VGIRD: ztot allocation failure')
read(19,*)
do k=1,kz-1
read(19,*)j,ztot(k)
if(k>1.and.ztot(k)<=ztot(k-1).or.ztot(k)>=-h_s) then
write(errmsg,*)'z-level inverted:',k
call parallel_abort(errmsg)
endif
enddo
read(19,*)
ztot(kz)=-h_s
nsig=nvrt-kz+1
read(19,*)
read(19,*)h_c,theta_b,theta_f
if(h_c<5.or.h_c>=h_s) then
write(errmsg,*)'h_c needs to be larger:',h_c
call parallel_abort(errmsg)
endif
if(theta_b<0.or.theta_b>1) then
write(errmsg,*)'Wrong theta_b:',theta_b
call parallel_abort(errmsg)
endif
if(theta_f<=0) then
write(errmsg,*)'Wrong theta_f:',theta_f
call parallel_abort(errmsg)
endif
s_con1=sinh(theta_f)
sigma(1)=-1
sigma(nsig)=0
read(19,*)
do k=kz+1,nvrt-1
kin=k-kz+1
read(19,*) j,sigma(kin)
if(sigma(kin)<=sigma(kin-1).or.sigma(kin)>=0) then
write(errmsg,*)'Check sigma levels at:',k,sigma(kin),sigma(kin-1)
call parallel_abort(errmsg)
endif
enddo
read(19,*)
close(19)
endif
do k=1,nsig
cs(k)=(1-theta_b)*sinh(theta_f*sigma(k))/sinh(theta_f)+ &
&theta_b*(tanh(theta_f*(sigma(k)+0.5))-tanh(theta_f*0.5))/2/tanh(theta_f*0.5)
dcs(k)=(1-theta_b)*theta_f*cosh(theta_f*sigma(k))/sinh(theta_f)+ &
&theta_b*theta_f/2/tanh(theta_f*0.5)/cosh(theta_f*(sigma(k)+0.5))**2
enddo
if(myrank==0) then
open(10,file='sample_z.out',status='replace')
write(10,*)'Sample z coordinates'
buf1(1)=h_s; buf1(2)=h_c; buf1(2:11)=(/(10*(i-1),i=2,11)/); buf1(12:28)=(/(200+50*(i-12), i=12,28)/)
write(10,*)'h_c= ',h_c,', h_s=',h_s
do i=1,28
write(10,*)'Depth= ',buf1(i)
do k=kz,nvrt
kin=k-kz+1
hmod2=min(buf1(i),h_s)
if(hmod2<=h_c) then
zz=sigma(kin)*hmod2
else
zz=h_c*sigma(kin)+(hmod2-h_c)*cs(kin)
endif
write(10,*)k,zz
enddo
enddo
close(10)
endif
end subroutine aquire_vgrid
subroutine aquire_hgrid(full_aquire)
#ifdef USE_MPIMODULE
use mpi
#endif
use elfe_glbl
use elfe_msgp
implicit none
#ifndef USE_MPIMODULE
include 'mpif.h'
#endif
logical,intent(in) :: full_aquire
integer :: i,j,k,l,ii,jj,irank,ip,jp,ie,je,ic,iegb,jegb,ngb1,ngb2,ipgb,isgb,icount,new,id,isd
integer,allocatable :: ibuf(:),isbuf(:),irbuf(:)
real(rkind),allocatable :: dbuf1(:),dbuf2(:)
real(rkind),allocatable :: x_global(:),y_global(:)
type(llist_type),pointer :: llp,node,nodep,side,sidep
logical :: found,local1,local2,found1,found2
real(rkind),parameter :: deg2rad=pi/180.
integer :: n1,n2,n3,n4,jsj,nt,nn,nscnt,nsgcnt
real(rkind) :: ar1,ar2,ar3,ar4,signa
real(rkind) :: xtmp,ytmp,dptmp,thetan,egb1,egb2,egb
integer :: ipre
integer,allocatable :: neproc(:)
integer,allocatable :: npproc(:)
integer,allocatable :: nsproc(:)
integer,allocatable :: jsgb(:,:)
integer,allocatable :: nnpgb(:),inpgb(:,:)
integer,allocatable :: iselfgb(:,:)
integer :: npi
integer,allocatable :: ieg(:)
integer,allocatable :: ipg(:)
integer,allocatable :: isg(:)
integer :: mneii
integer :: mnopep
integer :: stat
integer :: intvalue
real(rkind) :: realvalue
character(len=2) :: stringvalue
call get_param('ipre',1,ipre,realvalue,stringvalue)
if(nproc>1.and.ipre/=0) &
call parallel_abort('AQUIRE_HGRID: ipre/=0 is not enabled for nproc>1')
call get_param('ntracers',1,ntracers,realvalue,stringvalue)
if(ntracers<0) call parallel_abort('AQUIRE_HGRID: check ntracers')
mntr=max(ntracers,2)
ntracers2=ntracers+2
call get_param('nonhydro',1,nonhydro,realvalue,stringvalue)
if(nonhydro/=0.and.nonhydro/=1) call parallel_abort('AQUIRE_HGRID: check nonhydro')
call get_param('indvel',1,indvel,realvalue,stringvalue)
call get_param('imm',1,imm,realvalue,stringvalue)
call get_param('ihot',1,ihot,realvalue,stringvalue)
call get_param('ics',1,ics,realvalue,stringvalue)
if(ics/=1.and.ics/=2) then
write(errmsg,*) 'AQUIRE_HGRID: Unknown ics',ics
call parallel_abort(errmsg)
endif
open(14,file='hgrid.gr3',status='old',iostat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: open(14) failure')
read(14,*); read(14,*) ne_global,np_global
if(allocated(nmgb)) deallocate(nmgb); allocate(nmgb(ne_global,3),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: nmgb allocation failure')
do i=1,np_global; read(14,*); enddo;
do i=1,ne_global
read(14,*) iegb,j,(nmgb(iegb,k),k=1,3)
if(j/=3) then
write(errmsg,*) 'AQUIRE_HGRID: Unknown type of element',iegb,j
call parallel_abort(errmsg)
endif
enddo
if(allocated(nnegb)) deallocate(nnegb); allocate(nnegb(np_global),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: nnegb allocation failure')
nnegb=0
do iegb=1,ne_global
do k=1,3
ipgb=nmgb(iegb,k)
nnegb(ipgb)=nnegb(ipgb)+1
enddo
enddo
if(myrank==0) then
found=.false.
do ipgb=1,np_global
if(nnegb(ipgb)==0) then
found=.true.
write(11,*) 'Hanging node:',ipgb
endif
enddo
if(found) call parallel_abort('AQUIRE_HGRID: check fort.11 for hanging nodes')
endif
mnei=0
do ipgb=1,np_global
mnei=max(mnei,nnegb(ipgb))
enddo
if(allocated(inegb)) deallocate(inegb); allocate(inegb(np_global,mnei),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: inegb allocation failure')
if(allocated(iselfgb)) deallocate(iselfgb); allocate(iselfgb(np_global,mnei),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: inegb allocation failure')
nnegb=0
do iegb=1,ne_global
do k=1,3
ipgb=nmgb(iegb,k)
nnegb(ipgb)=nnegb(ipgb)+1
inegb(ipgb,nnegb(ipgb))=iegb
iselfgb(ipgb,nnegb(ipgb))=k
enddo
enddo
if(allocated(ic3gb)) deallocate(ic3gb); allocate(ic3gb(ne_global,3),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: ic3gb allocation failure')
do iegb=1,ne_global
do k=1,3
ic3gb(iegb,k)=0
ngb1=nmgb(iegb,nx(k,1))
ngb2=nmgb(iegb,nx(k,2))
do l=1,nnegb(ngb1)
jegb=inegb(ngb1,l)
if(jegb/=iegb.and.(nmgb(jegb,1)==ngb2.or.nmgb(jegb,2)==ngb2.or.nmgb(jegb,3)==ngb2)) &
ic3gb(iegb,k)=jegb
enddo
enddo
enddo
if(allocated(jsgb)) deallocate(jsgb); allocate(jsgb(ne_global,3),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: jsgb allocation failure')
ns_global=0
do iegb=1,ne_global
do j=1,3
if(ic3gb(iegb,j)==0.or.iegb<ic3gb(iegb,j)) then
ns_global=ns_global+1
jsgb(iegb,j)=ns_global
if(ic3gb(iegb,j)/=0) then
jegb=ic3gb(iegb,j)
l=0
do k=1,3
if(ic3gb(jegb,k)==iegb) then
l=k
exit
endif
enddo
if(l==0) then
write(errmsg,'(a,10i6)') 'AQUIRE_HGRID: Wrong ball info', &
iegb,j,ngb1,ngb2,ns_global
call parallel_abort(errmsg)
endif
jsgb(jegb,l)=ns_global
endif
endif
enddo
enddo
if(ns_global.lt.ne_global.or.ns_global.lt.np_global) then
write(errmsg,*) &
'AQUIRE_HGRID: weird grid with ns_global < ne_global or ns_global < np_global', &
np_global,ne_global,ns_global
call parallel_abort(errmsg)
endif
if(full_aquire) then
if(myrank==0) then
write(16,'(/a,4i10)')'Global Grid Size (ne,np,ns,nvrt): ',ne_global,np_global,ns_global,nvrt
endif
call parallel_barrier
endif
if(allocated(isbnd_global)) deallocate(isbnd_global);
allocate(isbnd_global(np_global),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: isbnd_global allocation failure')
isbnd_global=0;
rewind(14); read(14,*); read(14,*);
do i=1,np_global; read(14,*); enddo;
do i=1,ne_global; read(14,*); enddo;
read(14,*) nope_global
read(14,*) neta_global
mnond_global=0
nt=0
do k=1,nope_global
read(14,*) nn
mnond_global=max(mnond_global,nn);
nt=nt+nn
do i=1,nn; read(14,*); enddo;
enddo
if(neta_global/=nt) then
write(errmsg,*) 'neta_global /= total # of open bnd nodes',neta_global,nt
call parallel_abort(errmsg)
endif
if(allocated(nond_global)) deallocate(nond_global);
allocate(nond_global(nope_global),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: nond_global allocation failure')
if(allocated(iond_global)) deallocate(iond_global);
allocate(iond_global(nope_global,mnond_global),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: iond_global allocation failure')
if(allocated(x_global)) deallocate(x_global);
allocate(x_global(np_global),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: x_global allocation failure')
if(allocated(y_global)) deallocate(y_global);
allocate(y_global(np_global),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: y_global allocation failure')
rewind(14); read(14,*); read(14,*);
do i=1,np_global; read(14,*)j,x_global(i),y_global(i); enddo;
do i=1,ne_global; read(14,*); enddo;
read(14,*); read(14,*);
nond_global=0; iond_global=0;
do k=1,nope_global
read(14,*) nn
do i=1,nn
read(14,*) ipgb
nond_global(k)=nond_global(k)+1
iond_global(k,nond_global(k))=ipgb
isbnd_global(ipgb)=k
enddo
if(iond_global(k,1)==iond_global(k,nond_global(k))) then
write(errmsg,*) 'Looped open bnd:',k
call parallel_abort(errmsg)
endif
enddo
deallocate(x_global,y_global)
rewind(14); read(14,*); read(14,*);
do i=1,np_global; read(14,*); enddo;
do i=1,ne_global; read(14,*); enddo;
read(14,*); read(14,*);
do k=1,nope_global; read(14,*) nn; do i=1,nn; read(14,*); enddo; enddo;
read(14,*) nland_global
read(14,*) nvel_global
mnlnd_global=0
nt=0
do k=1,nland_global
read(14,*) nn
mnlnd_global=max(mnlnd_global,nn)
nt=nt+nn
do i=1,nn; read(14,*); enddo;
enddo
if(nvel_global/=nt) then
write(errmsg,*) 'AQUIRE_HGRID: nvel_global /= total # of land bnd nodes', &
nvel_global,nt
call parallel_abort(errmsg)
endif
if(allocated(nlnd_global)) deallocate(nlnd_global);
allocate(nlnd_global(nland_global),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: nlnd_global allocation failure')
if(allocated(ilnd_global)) deallocate(ilnd_global);
allocate(ilnd_global(nland_global,mnlnd_global),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: ilnd_global allocation failure')
rewind(14); read(14,*); read(14,*);
do i=1,np_global; read(14,*); enddo;
do i=1,ne_global; read(14,*); enddo;
read(14,*); read(14,*);
do k=1,nope_global; read(14,*) nn; do i=1,nn; read(14,*); enddo; enddo;
read(14,*); read(14,*);
nlnd_global=0; ilnd_global=0;
do k=1,nland_global
read(14,*) nn
do i=1,nn
read(14,*) ipgb
nlnd_global(k)=nlnd_global(k)+1
ilnd_global(k,nlnd_global(k))=ipgb
if(isbnd_global(ipgb)==0) isbnd_global(ipgb)=-1
enddo
enddo
if(allocated(isbs_global)) deallocate(isbs_global);
allocate(isbs_global(ns_global),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: isbs_global allocation failure')
isbs_global=0
if(allocated(nnpgb)) deallocate(nnpgb);
allocate(nnpgb(np_global),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: nnpgb allocation failure')
if(allocated(inpgb)) deallocate(inpgb);
allocate(inpgb(np_global,mnei+1),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: inpgb allocation failure')
do i=1,np_global
if(isbnd_global(i)/=0) then
icount=0
do j=1,nnegb(i)
ie=inegb(i,j)
ii=iselfgb(i,j)
if(ic3gb(ie,nx(ii,2))==0) then
icount=icount+1
inegb(i,1)=ie
iselfgb(i,1)=ii
endif
enddo
if(icount/=1) then
write(errmsg,*)'Illegal bnd node',i
call parallel_abort(errmsg)
endif
endif
nnpgb(i)=2
inpgb(i,1)=nmgb(inegb(i,1),nx(iselfgb(i,1),1))
inpgb(i,2)=nmgb(inegb(i,1),nx(iselfgb(i,1),2))
do j=2,nnegb(i)
new=ic3gb(inegb(i,j-1),nx(iselfgb(i,j-1),1))
if(new==0) then
write(errmsg,*)'Incomplete ball',i
call parallel_abort(errmsg)
endif
inegb(i,j)=new
ii=0
do l=1,3
if(nmgb(new,l)==i) ii=l
enddo
if(ii==0) then
write(errmsg,*)'Failed to find local index:',i,new
call parallel_abort(errmsg)
endif
iselfgb(i,j)=ii
if(isbnd_global(i)==0.and.j==nnegb(i)) then
if(nmgb(new,nx(ii,2))/=inpgb(i,1)) then
write(errmsg,*)'Broken ball:',i
call parallel_abort(errmsg)
endif
else
nnpgb(i)=nnpgb(i)+1
if(nnpgb(i)>mnei+1) then
write(errmsg,*)'Too many neighbor nodes:',i,mnei
call parallel_abort(errmsg)
endif
inpgb(i,nnpgb(i))=nmgb(new,nx(ii,2))
endif
enddo
enddo
close(14)
allocate(neproc(0:nproc-1),stat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: neproc allocation failure')
neproc=0
allocate(npproc(0:nproc-1),stat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: npproc allocation failure')
npproc=0
allocate(nsproc(0:nproc-1),stat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: nsproc allocation failure')
nsproc=0
if(associated(iegl)) call release_gl(ne_global,iegl)
allocate(iegl(ne_global),stat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: iegl allocation failure')
if(associated(ipgl)) call release_gl(np_global,ipgl)
allocate(ipgl(np_global),stat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: ipgl allocation failure')
if(associated(isgl)) call release_gl(ns_global,isgl)
allocate(isgl(ns_global),stat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: isgl allocation failure')
if(nproc>1) then
do iegb=1,ne_global
irank=iegrpv(iegb)
neproc(irank)=neproc(irank)+1
ie=neproc(irank)
iegl(iegb)%rank=irank
iegl(iegb)%id=ie
do k=1,3
ipgb=nmgb(iegb,k)
if(ipgl(ipgb)%id==0) then
npproc(irank)=npproc(irank)+1
ipgl(ipgb)%rank=irank
ipgl(ipgb)%id=npproc(irank)
else
found=.false.
node=>ipgl(ipgb)
loopn: do
if(node%rank==irank) then
found=.true.
exit loopn
endif
nodep=>node
node=>node%next
if(.not.associated(node)) exit loopn
enddo loopn
if(.not.found) then
npproc(irank)=npproc(irank)+1
allocate(node,stat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: node allocation failure')
node=llist_type(irank,npproc(irank),null())
nodep%next=>node
endif
endif
isgb=jsgb(iegb,k)
if(isgl(isgb)%id==0) then
nsproc(irank)=nsproc(irank)+1
isgl(isgb)%rank=irank
isgl(isgb)%id=nsproc(irank)
else
found=.false.
side=>isgl(isgb)
loops: do
if(side%rank==irank) then
found=.true.
exit loops
endif
sidep=>side
side=>side%next
if(.not.associated(side)) exit loops
enddo loops
if(.not.found) then
nsproc(irank)=nsproc(irank)+1
allocate(side,stat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: side allocation failure')
side=llist_type(irank,nsproc(irank),null())
sidep%next=>side
endif
endif
enddo
enddo
else
do iegb=1,ne_global
neproc(0)=neproc(0)+1
iegl(iegb)%rank=0
iegl(iegb)%id=iegb
enddo
do ipgb=1,np_global
npproc(0)=npproc(0)+1
ipgl(ipgb)%rank=0
ipgl(ipgb)%id=ipgb
enddo
do isgb=1,ns_global
nsproc(0)=nsproc(0)+1
isgl(isgb)%rank=0
isgl(isgb)%id=isgb
enddo
endif
if(nproc>1) then
call swap_llrank(np_global,ipgl)
call swap_llrank(ns_global,isgl)
endif
if(nproc>1) then
call sort_llrank(np_global,ipgl)
call sort_llrank(ns_global,isgl)
endif
ne=neproc(myrank)
np=npproc(myrank)
ns=nsproc(myrank)
if(allocated(neproc)) deallocate(neproc)
if(allocated(npproc)) deallocate(npproc)
if(allocated(nsproc)) deallocate(nsproc)
if(nproc>1) then
npi=0
do ipgb=1,np_global
if(ipgl(ipgb)%rank==myrank.and.associated(ipgl(ipgb)%next)) npi=npi+1
enddo
if(allocated(ieg)) deallocate(ieg); allocate(ieg(npi*mnei),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: ieg allocation failure')
neg=0; ieg=0;
iegloop: do iegb=1,ne_global
if(iegrpv(iegb)==myrank) cycle iegloop
do j=1,3
ipgb=nmgb(iegb,j)
if(ipgl(ipgb)%rank==myrank) then
neg=neg+1
if(neg>npi*mnei) then
write(errmsg,*)'Overflow in ieg:',npi*mnei
call parallel_abort(errmsg)
endif
ieg(neg)=iegb
cycle iegloop
endif
enddo
enddo iegloop
if(allocated(ipg)) deallocate(ipg); allocate(ipg(neg*3),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: ipg allocation failure')
if(allocated(isg)) deallocate(isg); allocate(isg(neg*3),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: isg allocation failure')
npg=0; ipg=0;
nsg=0; isg=0;
do i=1,neg
iegb=ieg(i)
do j=1,3
ipgb=nmgb(iegb,j)
if(ipgl(ipgb)%rank/=myrank) then
found=.false.
kn: do k=1,npg
if(ipg(k)==ipgb) then
found=.true.
exit kn
endif
enddo kn
if(.not.found) then
npg=npg+1
if(npg>neg*3) then
write(errmsg,*)'Overflow in ipg:',neg*3
call parallel_abort(errmsg)
endif
ipg(npg)=ipgb
endif
endif
isgb=jsgb(iegb,j)
if(isgl(isgb)%rank/=myrank) then
found=.false.
ks: do k=1,nsg
if(isg(k)==isgb) then
found=.true.
exit ks
endif
enddo ks
if(.not.found) then
nsg=nsg+1
if(nsg>neg*3) then
write(errmsg,*)'Overflow in isg:',neg*3
call parallel_abort(errmsg)
endif
isg(nsg)=isgb
endif
endif
enddo
enddo
else
neg=0
npg=0
nsg=0
endif
nea=ne+neg
npa=np+npg
nsa=ns+nsg
if(full_aquire) then
allocate(isbuf(9)); allocate(irbuf(9*nproc));
isbuf(1)=nea; isbuf(2)=ne; isbuf(3)=neg;
isbuf(4)=npa; isbuf(5)=np; isbuf(6)=npg;
isbuf(7)=nsa; isbuf(8)=ns; isbuf(9)=nsg;
call mpi_gather(isbuf,9,itype,irbuf,9,itype,0,comm,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('AQUIRE_HGRID: gather subdomain size',ierr)
if(myrank==0) then
write(16,'(/a)') '**********Augmented Subdomain Sizes**********'
write(16,'(10a)') ' rank', &
' nea',' ne',' neg', &
' npa',' np',' npg', &
' nsa',' ns',' nsg'
do i=0,nproc-1
write(16,'(i5,9i8)') i, &
irbuf(9*i+1),irbuf(9*i+2),irbuf(9*i+3), &
irbuf(9*i+4),irbuf(9*i+5),irbuf(9*i+6), &
irbuf(9*i+7),irbuf(9*i+8),irbuf(9*i+9)
enddo
endif
call parallel_barrier
deallocate(isbuf,irbuf)
endif
if(allocated(ielg)) deallocate(ielg); allocate(ielg(nea),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: ielg allocation failure')
do iegb=1,ne_global
if(iegl(iegb)%rank==myrank) ielg(iegl(iegb)%id)=iegb
enddo
if(nproc>1) ielg(ne+1:nea)=ieg(1:neg)
do i=1,neg
iegb=ieg(i)
allocate(llp,stat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: aug-element allocation failure')
llp=llist_type(iegl(iegb)%rank,iegl(iegb)%id,iegl(iegb)%next)
iegl(iegb)%rank=myrank
iegl(iegb)%id=ne+i
iegl(iegb)%next=>llp
enddo
if(allocated(iplg)) deallocate(iplg); allocate(iplg(npa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: iplg allocation failure')
do ipgb=1,np_global
if(ipgl(ipgb)%rank==myrank) iplg(ipgl(ipgb)%id)=ipgb
enddo
if(nproc>1) iplg(np+1:npa)=ipg(1:npg)
do i=1,npg
ipgb=ipg(i)
allocate(llp,stat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: aug-node allocation failure')
llp=llist_type(ipgl(ipgb)%rank,ipgl(ipgb)%id,ipgl(ipgb)%next)
ipgl(ipgb)%rank=myrank
ipgl(ipgb)%id=np+i
ipgl(ipgb)%next=>llp
enddo
if(allocated(islg)) deallocate(islg); allocate(islg(nsa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: islg allocation failure')
do isgb=1,ns_global
if(isgl(isgb)%rank==myrank) islg(isgl(isgb)%id)=isgb
enddo
if(nproc>1) islg(ns+1:nsa)=isg(1:nsg)
do i=1,nsg
isgb=isg(i)
allocate(llp,stat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: aug-side allocation failure')
llp=llist_type(isgl(isgb)%rank,isgl(isgb)%id,isgl(isgb)%next)
isgl(isgb)%rank=myrank
isgl(isgb)%id=ns+i
isgl(isgb)%next=>llp
enddo
if(allocated(ieg)) deallocate(ieg)
if(allocated(ipg)) deallocate(ipg)
if(allocated(isg)) deallocate(isg)
if(allocated(nm)) deallocate(nm); allocate(nm(nea,3),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: nm allocation failure')
if(allocated(ic3)) deallocate(ic3); allocate(ic3(nea,3),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: ic3 allocation failure')
if(allocated(js)) deallocate(js); allocate(js(nea,3),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: js allocation failure')
do ie=1,nea
iegb=ielg(ie)
do j=1,3
nm(ie,j)=ipgl(nmgb(iegb,j))%id
js(ie,j)=isgl(jsgb(iegb,j))%id
jegb=ic3gb(iegb,j)
if(jegb/=0.and.iegl(max(1,jegb))%rank==myrank) then
ic3(ie,j)=iegl(jegb)%id
else
ic3(ie,j)=-jegb
endif
enddo
enddo
if(allocated(nne)) deallocate(nne); allocate(nne(npa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: nne allocation failure')
if(allocated(ine)) deallocate(ine); allocate(ine(npa,mnei),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: ine allocation failure')
if(allocated(iself)) deallocate(iself); allocate(iself(npa,mnei),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: iself allocation failure')
do ip=1,npa
ipgb=iplg(ip)
nne(ip)=nnegb(ipgb)
do k=1,nnegb(ipgb)
iegb=inegb(ipgb,k)
if(iegl(iegb)%rank==myrank) then
ine(ip,k)=iegl(iegb)%id
else
ine(ip,k)=-iegb
endif
iself(ip,k)=iselfgb(ipgb,k)
enddo
enddo
if(allocated(nnp)) deallocate(nnp); allocate(nnp(npa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: nnp allocation failure')
if(allocated(inp)) deallocate(inp); allocate(inp(npa,mnei+1),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: inp allocation failure')
do ip=1,npa
ipgb=iplg(ip)
nnp(ip)=nnpgb(ipgb)
do k=1,nnpgb(ipgb)
new=inpgb(ipgb,k)
if(ipgl(new)%rank==myrank) then
inp(ip,k)=ipgl(new)%id
else
inp(ip,k)=-new
endif
enddo
enddo
if(allocated(jsgb)) deallocate(jsgb)
if(allocated(nnpgb)) deallocate(nnpgb)
if(allocated(inpgb)) deallocate(inpgb)
if(allocated(iselfgb)) deallocate(iselfgb)
if(allocated(xnd)) deallocate(xnd); allocate(xnd(npa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: x allocation failure')
if(allocated(ynd)) deallocate(ynd); allocate(ynd(npa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: y allocation failure')
if(allocated(znd)) deallocate(znd); allocate(znd(npa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: z allocation failure')
znd=0
if(allocated(dp)) deallocate(dp); allocate(dp(npa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: dp allocation failure')
if(allocated(xlon)) deallocate(xlon); allocate(xlon(npa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: xlon allocation failure')
if(allocated(ylat)) deallocate(ylat); allocate(ylat(npa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: ylat allocation failure')
open(14,file='hgrid.gr3',status='old',iostat=stat)
if(stat/=0) call parallel_abort('AQUIRE_HGRID: open(14) failure')
read(14,*); read(14,*);
do i=1,np_global
read(14,*) ipgb,xtmp,ytmp,dptmp
node=>ipgl(ipgb)
if(node%rank==myrank) then
ii=node%id
if(ics==1) then
xnd(ii)=xtmp
ynd(ii)=ytmp
dp(ii)=dptmp
else
xlon(ii)=xtmp*deg2rad
ylat(ii)=ytmp*deg2rad
dp(ii)=dptmp
xnd(ii)=rearth*cos(ylat(ii))*cos(xlon(ii))
ynd(ii)=rearth*cos(ylat(ii))*sin(xlon(ii))
znd(ii)=rearth*sin(ylat(ii))
endif
endif
enddo
close(14)
if(full_aquire) then
if(allocated(xctr)) deallocate(xctr); allocate(xctr(nea),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: xctr allocation failure')
if(allocated(yctr)) deallocate(yctr); allocate(yctr(nea),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: yctr allocation failure')
if(allocated(zctr)) deallocate(zctr); allocate(zctr(nea),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: zctr allocation failure')
if(allocated(area)) deallocate(area); allocate(area(nea),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: area allocation failure')
if(allocated(radiel)) deallocate(radiel); allocate(radiel(nea),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: radiel allocation failure')
zctr=0
if(allocated(dpe)) deallocate(dpe); allocate(dpe(nea),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: dpe allocation failure')
if(allocated(eframe)) deallocate(eframe); allocate(eframe(3,3,nea),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: eframe allocation failure')
if(allocated(xel)) deallocate(xel); allocate(xel(3,nea),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: xel allocation failure')
if(allocated(yel)) deallocate(yel); allocate(yel(3,nea),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: yel allocation failure')
eframe=0
thetan=-1.e10
do ie=1,nea
xctr(ie)=0d0
yctr(ie)=0d0
dpe(ie)=1.d10
do j=1,3
xctr(ie)=xctr(ie)+xnd(nm(ie,j))/3.d0
yctr(ie)=yctr(ie)+ynd(nm(ie,j))/3.d0
if(ics==2) zctr(ie)=zctr(ie)+znd(nm(ie,j))/3.d0
if(dp(nm(ie,j))<dpe(ie)) dpe(ie)=dp(nm(ie,j))
enddo
if(ics==1) then
xel(1:3,ie)=xnd(nm(ie,1:3))
yel(1:3,ie)=ynd(nm(ie,1:3))
else
call compute_ll(xctr(ie),yctr(ie),zctr(ie),ar1,ar2)
eframe(1,1,ie)=-sin(ar1)
eframe(2,1,ie)=cos(ar1)
eframe(3,1,ie)=0
eframe(1,2,ie)=-cos(ar1)*sin(ar2)
eframe(2,2,ie)=-sin(ar1)*sin(ar2)
eframe(3,2,ie)=cos(ar2)
call cross_product(eframe(1,1,ie),eframe(2,1,ie),eframe(3,1,ie), &
& eframe(1,2,ie),eframe(2,2,ie),eframe(3,2,ie), &
& eframe(1,3,ie),eframe(2,3,ie),eframe(3,3,ie))
egb1=eframe(1,3,ie)*xctr(ie)+eframe(2,3,ie)*yctr(ie)+eframe(3,3,ie)*zctr(ie)
if(egb1<=0) then
write(errmsg,*)'AQUIRE_HGRID: orientation wrong:',ielg(ie),egb1,xlon(nm(ie,1:3)), &
&ylat(nm(ie,1:3)),xnd(nm(ie,1:3)),ynd(nm(ie,1:3)),znd(nm(ie,1:3))
call parallel_abort(errmsg)
endif
xtmp=dot_product(eframe(1:3,1,ie),eframe(1:3,3,ie))
ytmp=dot_product(eframe(1:3,1,ie),eframe(1:3,2,ie))
dptmp=dot_product(eframe(1:3,3,ie),eframe(1:3,2,ie))
ar1=max(abs(xtmp),abs(ytmp),abs(dptmp))
if(ar1>1.e-7) then
write(errmsg,*)'AQUIRE_HGRID: axes wrong',ielg(ie),xtmp,ytmp,dptmp
call parallel_abort(errmsg)
endif
if(ar1>thetan) thetan=ar1
do j=1,3
nn=nm(ie,j)
xel(j,ie)=(xnd(nn)-xctr(ie))*eframe(1,1,ie)+(ynd(nn)-yctr(ie))*eframe(2,1,ie)+ &
&(znd(nn)-zctr(ie))*eframe(3,1,ie)
yel(j,ie)=(xnd(nn)-xctr(ie))*eframe(1,2,ie)+(ynd(nn)-yctr(ie))*eframe(2,2,ie)+ &
&(znd(nn)-zctr(ie))*eframe(3,2,ie)
enddo
endif
area(ie)=signa(xel(1,ie),xel(2,ie),xel(3,ie),yel(1,ie),yel(2,ie),yel(3,ie))
if(area(ie)<=0.d0) then
write(errmsg,'(a,2i8)') 'AQUIRE_HGRID: negative area at',ie,ielg(ie)
call parallel_abort(errmsg)
endif
radiel(ie)=sqrt(area(ie)/pi)
enddo
if(ics==2) then
call mpi_reduce(thetan,dptmp,1,rtype,MPI_MAX,0,comm,ierr)
if(myrank==0) then
write(16,*)'Max. dot product of 3 axes=',real(dptmp)
endif
endif
if(allocated(is)) deallocate(is); allocate(is(nsa,2),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: is allocation failure')
if(allocated(isidenode)) deallocate(isidenode); allocate(isidenode(nsa,2),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: isidenode allocation failure')
if(allocated(xcj)) deallocate(xcj); allocate(xcj(nsa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: xcj allocation failure')
if(allocated(ycj)) deallocate(ycj); allocate(ycj(nsa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: ycj allocation failure')
if(allocated(zcj)) deallocate(zcj); allocate(zcj(nsa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: zcj allocation failure')
zcj=0
if(allocated(dps)) deallocate(dps); allocate(dps(nsa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: dps allocation failure')
if(allocated(distj)) deallocate(distj); allocate(distj(nsa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: distj allocation failure')
do ie=1,nea
iegb=ielg(ie)
do j=1,3
jsj=js(ie,j)
n1=nm(ie,nx(j,1))
n2=nm(ie,nx(j,2))
if(ic3(ie,j)==0 &
.or.(ic3(ie,j)>0.and.iegb<ielg(max(1,ic3(ie,j)))) &
.or.(ic3(ie,j)<0.and.iegb<iabs(ic3(ie,j)))) then
is(jsj,1)=ie
is(jsj,2)=ic3(ie,j)
isidenode(jsj,1)=n1
isidenode(jsj,2)=n2
else
is(jsj,1)=ic3(ie,j)
is(jsj,2)=ie
isidenode(jsj,1)=n2
isidenode(jsj,2)=n1
endif
xcj(jsj)=(xnd(n1)+xnd(n2))/2d0
ycj(jsj)=(ynd(n1)+ynd(n2))/2d0
if(ics==2) zcj(jsj)=(znd(n1)+znd(n2))/2
dps(jsj)=(dp(n1)+dp(n2))/2d0
distj(jsj)=sqrt((xnd(n2)-xnd(n1))**2+(ynd(n2)-ynd(n1))**2+(znd(n2)-znd(n1))**2)
if(distj(jsj)==0) then
write(errmsg,*) 'AQUIRE_HGRID: Zero side',jsj
call parallel_abort(errmsg)
endif
enddo
enddo
if(allocated(ssign)) deallocate(ssign); allocate(ssign(nea,3),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: ssign allocation failure')
do ie=1,nea
iegb=ielg(ie)
do j=1,3
jsj=js(ie,j)
je=is(jsj,1)
if(je==0) then
write(errmsg,*)'First element empty:',ie,iegb,j
call parallel_abort(errmsg)
else if(je>0) then
jegb=ielg(je)
else
jegb=-je
endif
if(iegb==jegb) then
ssign(ie,j)=1
else
ssign(ie,j)=-1
endif
enddo
enddo
if(allocated(sframe)) deallocate(sframe); allocate(sframe(3,3,nsa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: sframe allocation failure')
thetan=-1.e10
realvalue=-1.e10
do j=1,nsa
n1=isidenode(j,1)
n2=isidenode(j,2)
if(ics==1) then
thetan=atan2(xnd(n1)-xnd(n2),ynd(n2)-ynd(n1))
sframe(1,1,j)=cos(thetan)
sframe(2,1,j)=sin(thetan)
sframe(1,2,j)=-sframe(2,1,j)
sframe(2,2,j)=sframe(1,1,j)
else
ar1=xnd(n2)-xnd(n1)
ar2=ynd(n2)-ynd(n1)
ar3=znd(n2)-znd(n1)
ar4=sqrt(ar1*ar1+ar2*ar2+ar3*ar3)
if(ar4==0) then
write(errmsg,*)'AQUIRE_HGRID: 0 ys-vector',iplg(isidenode(j,1:2))
call parallel_abort(errmsg)
endif
sframe(1,2,j)=ar1/ar4
sframe(2,2,j)=ar2/ar4
sframe(3,2,j)=ar3/ar4
ar4=sqrt(xcj(j)**2+ycj(j)**2+zcj(j)**2)
if(ar4==0) then
write(errmsg,*)'AQUIRE_HGRID: 0 zs-vector',iplg(isidenode(j,1:2))
call parallel_abort(errmsg)
endif
sframe(1,3,j)=xcj(j)/ar4
sframe(2,3,j)=ycj(j)/ar4
sframe(3,3,j)=zcj(j)/ar4
egb1=abs(dot_product(sframe(1:3,2,j),sframe(1:3,3,j)))
if(egb1>realvalue) realvalue=egb1
call cross_product(sframe(1,2,j),sframe(2,2,j),sframe(3,2,j), &
& sframe(1,3,j),sframe(2,3,j),sframe(3,3,j),ar1,ar2,ar3)
ar4=sqrt(ar1*ar1+ar2*ar2+ar3*ar3)
if(ar4==0) then
write(errmsg,*)'AQUIRE_HGRID: 0 xs-vector',iplg(isidenode(j,1:2))
call parallel_abort(errmsg)
endif
sframe(1,1,j)=ar1/ar4
sframe(2,1,j)=ar2/ar4
sframe(3,1,j)=ar3/ar4
if(j<=ns) then
ie=is(j,1)
egb1=dot_product(sframe(1:3,3,j),eframe(1:3,3,ie))-1
if(abs(egb1)>thetan) thetan=abs(egb1)
endif
endif
enddo
if(ics==2) then
call mpi_reduce(thetan,xtmp,1,rtype,MPI_MAX,0,comm,ierr)
call mpi_reduce(realvalue,ytmp,1,rtype,MPI_MAX,0,comm,ierr)
if(myrank==0) then
write(16,*)'Max. deviation between ze and zs axes=',real(xtmp)
write(16,*)'Max. dot prod. between ys and zs axes=',real(ytmp)
endif
endif
if(allocated(iopegl)) deallocate(iopegl); allocate(iopegl(0:1,nope_global),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: iopegl allocation failure')
iopegl=0
nope=0
neta=0
mnond=0
do k=1,nope_global
j=0
nn=nond_global(k)
n1=iond_global(k,1)
local1=(myrank==ipgl(n1)%rank)
if(local1) then
iopegl(0,k)=iopegl(0,k)+1
nope=nope+1
neta=neta+1
j=j+1
mnond=max(mnond,j);
endif
do i=2,nn
n2=iond_global(k,i)
local2=(myrank==ipgl(n2)%rank)
if(.not.local1.and.local2) then
iopegl(0,k)=iopegl(0,k)+1
nope=nope+1; j=0;
endif
if(local2) then
j=j+1
neta=neta+1
mnond=max(mnond,j);
endif
n1=n2
local1=local2
enddo
enddo
mnopep=0; do k=1,nope_global; mnopep=max(mnopep,iopegl(0,k)); enddo;
if(allocated(iopegl)) deallocate(iopegl); allocate(iopegl(0:mnopep,nope_global),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: iopegl allocation failure')
if(allocated(iopelg)) deallocate(iopelg); allocate(iopelg(nope),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: iopelg allocation failure')
if(allocated(nond)) deallocate(nond); allocate(nond(nope),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: nond allocation failure')
if(allocated(iond)) deallocate(iond); allocate(iond(nope,mnond),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: iond allocation failure')
nond=0; iopegl=0;
jj=nope
nope=0
do k=1,nope_global
nn=nond_global(k)
n1=iond_global(k,1)
local1=(myrank==ipgl(n1)%rank)
if(local1) then
nope=nope+1
if(nope>jj) call parallel_abort('AQUIRE_HGRID: nope>jj')
iopelg(nope)=k
iopegl(0,k)=iopegl(0,k)+1
if(iopegl(0,k)>mnopep) call parallel_abort('AQUIRE_HGRID: iopegl(0,k)>mnopep')
iopegl(iopegl(0,k),k)=nope
nond(nope)=nond(nope)+1
if(nond(nope)>mnond) then
write(errmsg,*)'AQUIRE_HGRID: nond(nope)>mnond',nond(nope),mnond
call parallel_abort(errmsg)
endif
iond(nope,nond(nope))=ipgl(n1)%id
endif
do i=2,nn
n2=iond_global(k,i)
local2=(myrank==ipgl(n2)%rank)
if(.not.local1.and.local2) then
nope=nope+1
if(nope>jj) call parallel_abort('AQUIRE_HGRID: nope>jj (2)')
iopelg(nope)=k
iopegl(0,k)=iopegl(0,k)+1
if(iopegl(0,k)>mnopep) call parallel_abort('AQUIRE_HGRID: iopegl(0,k)>mnopep (2)')
iopegl(iopegl(0,k),k)=nope
endif
if(local2) then
nond(nope)=nond(nope)+1
if(nond(nope)>mnond) call parallel_abort('AQUIRE_HGRID: nond(nope)>mnond (2)')
iond(nope,nond(nope))=ipgl(n2)%id
endif
n1=n2
local1=local2
enddo
enddo
nland=0
nvel=0
mnlnd=0
do k=1,nland_global
j=0
nn=nlnd_global(k)
n1=ilnd_global(k,1)
local1=(myrank==ipgl(n1)%rank)
if(local1) then
nland=nland+1
nvel=nvel+1
j=j+1
mnlnd=max(mnlnd,j);
endif
do i=2,nn
n2=ilnd_global(k,i)
local2=(myrank==ipgl(n2)%rank)
if(.not.local1.and.local2) then; nland=nland+1; j=0; endif;
if(local2) then
j=j+1
nvel=nvel+1
mnlnd=max(mnlnd,j);
endif
n1=n2
local1=local2
enddo
enddo
if(allocated(nlnd)) deallocate(nlnd); allocate(nlnd(nland),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: nlnd allocation failure')
if(allocated(ilnd)) deallocate(ilnd); allocate(ilnd(nland,mnlnd),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: ilnd allocation failure')
nlnd=0; ilnd=0;
nland=0
do k=1,nland_global
nn=nlnd_global(k)
n1=ilnd_global(k,1)
local1=(myrank==ipgl(n1)%rank)
if(local1) then
nland=nland+1
nlnd(nland)=nlnd(nland)+1
ilnd(nland,nlnd(nland))=ipgl(n1)%id
endif
do i=2,nn
n2=ilnd_global(k,i)
local2=(myrank==ipgl(n2)%rank)
if(.not.local1.and.local2) nland=nland+1
if(local2) then
nlnd(nland)=nlnd(nland)+1
ilnd(nland,nlnd(nland))=ipgl(n2)%id
endif
n1=n2
local1=local2
enddo
enddo
if(allocated(isbnd)) deallocate(isbnd); allocate(isbnd(-2:2,npa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: isbnd allocation failure')
isbnd=0
do k=1,nope_global
do j=1,nond_global(k)
ipgb=iond_global(k,j)
if(ipgl(ipgb)%rank==myrank) then
ip=ipgl(ipgb)%id
if(isbnd(1,ip)==0) then
isbnd(1,ip)=k
isbnd(-1,ip)=j
else if(isbnd(2,ip)==0) then
isbnd(2,ip)=k
isbnd(-2,ip)=j
else
write(errmsg,*)'agquire_hgrid: node on more than 2 open bnds:',ipgb
call parallel_abort(errmsg)
endif
endif
enddo
enddo
do k=1,nland_global
do j=1,nlnd_global(k)
ipgb=ilnd_global(k,j)
if(ipgl(ipgb)%rank==myrank) then
ip=ipgl(ipgb)%id
if(isbnd(1,ip)==0) isbnd(1,ip)=-1
endif
enddo
enddo
if(allocated(isbs)) deallocate(isbs); allocate(isbs(nsa),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: isbs allocation failure')
isbs=0
do i=1,nope_global
do j=1,nond_global(i)-1
n1=iond_global(i,j)
n2=iond_global(i,j+1)
if(ipgl(n1)%rank/=myrank.or.ipgl(n2)%rank/=myrank) cycle
n3=ipgl(n1)%id
n4=ipgl(n2)%id
do ii=1,nne(n3)
ie=ine(n3,ii)
if(ie>0) then
k=0
do jj=1,3
isd=js(ie,jj)
if((isidenode(isd,1)==n3.or.isidenode(isd,2)==n3).and. &
(isidenode(isd,1)==n4.or.isidenode(isd,2)==n4)) then
k=isd; exit
endif
enddo
if(k>0) then
if(is(k,2)/=0) then
write(errmsg,*)'aquire_hgrid: impossible (1)',n1,n2,is(k,1:2),ielg(is(k,1:2)),ielg(ie),iplg(isidenode(isd,1:2))
call parallel_abort(errmsg)
endif
isbs(k)=i; exit
endif
endif
enddo
enddo
enddo
do i=1,nsa
if(is(i,2)==0.and.isbs(i)==0) isbs(i)=-1
enddo
if(allocated(ibuf)) deallocate(ibuf)
if(myrank==0) then
write(16,'(/a)') '**********Global Boundary Sizes**********'
write(16,'(4a)') ' nope',' neta',' nland',' nvel'
write(16,'(4i8)') nope_global,neta_global,nland_global,nvel_global
endif
allocate(isbuf(4),irbuf(4*nproc),stat=stat);
if(stat/=0) call parallel_abort('AQUIRE_HGRID: isbuf allocation failure')
isbuf(1)=nope; isbuf(2)=neta; isbuf(3)=nland; isbuf(4)=nvel;
call mpi_gather(isbuf,4,itype,irbuf,4,itype,0,comm,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('AQUIRE_HGRID: gather subdomain bnd size',ierr)
if(myrank==0) then
write(16,'(/a)') '**********Augmented Subdomain Boundary Sizes**********'
write(16,'(5a)') ' rank',' nope',' neta',' nland',' nvel'
do i=0,nproc-1
write(16,'(5i8)') i,irbuf(4*i+1),irbuf(4*i+2),irbuf(4*i+3),irbuf(4*i+4)
enddo
write(16,*)
endif
deallocate(isbuf,irbuf)
call parallel_barrier
if(nproc==1.and.ipre/=0) call write_obe
endif
contains
subroutine swap_llrank(n,llarray)
implicit none
integer,intent(in) :: n
type(llist_type),pointer :: llarray(:)
type(llist_type),pointer :: llp
type(llist_type) :: lltmp
integer :: i
do i=1,n
if(llarray(i)%rank==myrank) cycle
llp=>llarray(i)%next
do
if(.not.associated(llp)) exit
if(llp%rank==myrank) then
lltmp=llarray(i)
llarray(i)%rank=llp%rank
llarray(i)%id=llp%id
llp%rank=lltmp%rank
llp%id=lltmp%id
exit
endif
llp=>llp%next
enddo
enddo
end subroutine swap_llrank
subroutine sort_llrank(n,llarray)
implicit none
integer,intent(in) :: n
type(llist_type),pointer :: llarray(:)
type(llist_type),pointer :: llp
integer :: i,j,k,t1(100),t2(100)
do i=1,n
if(.not.associated(llarray(i)%next)) cycle
if(.not.associated(llarray(i)%next%next)) cycle
k=0
llp=>llarray(i)%next
do
k=k+1
t1(k)=llp%rank
t2(k)=llp%id
llp=>llp%next
if(.not.associated(llp)) exit
enddo
call sort(k,t1,t2)
llp=>llarray(i)%next
do j=1,k
llp%rank=t1(j)
llp%id=t2(j)
llp=>llp%next
enddo
enddo
end subroutine sort_llrank
subroutine sort(n,ra,rb)
implicit none
integer :: n, l, ir, rra, rrb, i, j
integer :: ra(n)
integer,optional :: rb(n)
l = n/2 + 1
ir = n
10 continue
if (l.gt.1)then
l=l-1
rra = ra(l)
if(present(rb)) rrb=rb(l)
else
rra=ra(ir)
ra(ir)=ra(1)
if(present(rb)) then
rrb=rb(ir)
rb(ir)=rb(1)
endif
ir=ir-1
if (ir.eq.1) then
ra(1)=rra
if(present(rb)) rb(1)=rrb
return
endif
endif
i=l
j=l+l
20 if (j.le.ir) then
if (j.lt.ir) then
if(ra(j).lt.ra(j+1)) j=j+1
endif
if (rra.lt.ra(j)) then
ra(i)=ra(j)
if(present(rb)) rb(i)=rb(j)
i=j
j=j+j
else
j=ir+1
endif
go to 20
endif
ra(i)=rra
if(present(rb)) rb(i)=rrb
go to 10
end subroutine sort
end subroutine aquire_hgrid
