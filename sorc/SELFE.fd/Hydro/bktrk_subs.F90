subroutine init_inter_btrack
#ifdef USE_MPIMODULE
use mpi
#endif
use elfe_glbl
use elfe_msgp
implicit none
#ifndef USE_MPIMODULE
include 'mpif.h'
#endif
integer :: blockl(2),types(2),nmm
#if MPIVERSION==1
integer :: displ(2),base
#elif MPIVERSION==2
integer(kind=MPI_ADDRESS_KIND) :: displ(2),base
#endif
type(bt_type) :: bttmp
call mpi_allreduce(nsa,nmm,1,itype,MPI_MAX,comm,ierr)
mxnbt=s1_mxnbt*nmm*nvrt
blockl(1)=8
#if MPIVERSION==1
call mpi_address(bttmp%rank,displ(1),ierr)
#elif MPIVERSION==2
call mpi_get_address(bttmp%rank,displ(1),ierr)
#endif
if(ierr/=MPI_SUCCESS) call parallel_abort('INIT_INTER_BTRACK: mpi_get_address',ierr)
types(1)=itype
blockl(2)=26
#if MPIVERSION==1
call mpi_address(bttmp%dtbk,displ(2),ierr)
#elif MPIVERSION==2
call mpi_get_address(bttmp%dtbk,displ(2),ierr)
#endif
if(ierr/=MPI_SUCCESS) call parallel_abort('INIT_INTER_BTRACK: mpi_get_address',ierr)
types(2)=rtype
base=displ(1)
displ(1)=displ(1)-base
displ(2)=displ(2)-base
#if MPIVERSION==1
call mpi_type_struct(2,blockl,displ,types,bt_mpitype,ierr)
#elif MPIVERSION==2
call mpi_type_create_struct(2,blockl,displ,types,bt_mpitype,ierr)
#endif
if(ierr/=MPI_SUCCESS) call parallel_abort('INIT_INTER_BTRACK: type_create',ierr)
call mpi_type_commit(bt_mpitype,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INIT_INTER_BTRACK: type_commit',ierr)
end subroutine init_inter_btrack
subroutine inter_btrack(itime,nbt,btlist)
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
integer,intent(in) :: nbt
type(bt_type),intent(inout) :: btlist(mxnbt)
integer :: stat,i,ii,j,ie,irank,nnbrq,inbr,nbts,nbtd
integer :: mxbtsend,mxbtrecv,mnbt
real(rkind) :: xt,yt,zt,uuint,vvint,wwint,ttint,ssint
logical :: lexit,bt_donel,bt_done
integer :: icw
real(rkind) :: cwtmp
integer :: ncmplt,icmplt(nproc)
integer,allocatable :: nbtsend(:),ibtsend(:,:),nbtrecv(:),ibtrecv(:,:)
#if MPIVERSION==1
integer,allocatable :: bbtsend(:),bbtrecv(:)
#endif
integer,allocatable :: btsend_type(:),btsend_rqst(:),btsend_stat(:,:)
integer,allocatable :: btrecv_type(:),btrecv_rqst(:),btrecv_stat(:,:)
type(bt_type),allocatable :: btsendq(:),btrecvq(:),bttmp(:),btdone(:)
#ifdef DEBUG
integer,save :: ncalls=0
#endif
#ifdef DEBUG
ncalls=ncalls+1
fdb='interbtrack_0000'
lfdb=len_trim(fdb)
write(fdb(lfdb-3:lfdb),'(i4.4)') myrank
if(ncalls==1) then
open(30,file='outputs/'//fdb,status='replace')
else
open(30,file='outputs/'//fdb,status='old',position='append')
endif
write(30,'(a,3i6)') 'INTER_BTRACK START: ',itime,nbt
#endif
icw=4
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_allreduce(nbt,mnbt,1,itype,MPI_MAX,comm,ierr)
#ifdef INCLUDE_TIMING
wtimer(4,2)=wtimer(4,2)+mpi_wtime()-cwtmp
#endif
mnbt=mnbt*s2_mxnbt
allocate(btsendq(mnbt),btrecvq(mnbt*nnbr),bttmp(mnbt),btdone(mnbt),stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: type bt_type allocation failure')
allocate(nbtsend(nnbr),ibtsend(mnbt,nnbr), &
nbtrecv(nnbr),ibtrecv(mnbt,nnbr), &
btsend_type(nnbr),btsend_rqst(nnbr),btsend_stat(MPI_STATUS_SIZE,nnbr), &
btrecv_type(nnbr),btrecv_rqst(nnbr),btrecv_stat(MPI_STATUS_SIZE,nnbr),stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: comm data allocation failure')
#if MPIVERSION==1
allocate(bbtsend(mnbt),bbtrecv(mnbt),stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: bbtsend/recv allocation failure')
bbtsend=1; bbtrecv=1;
#endif
nbts=nbt
btsendq(1:nbts)=btlist(1:nbts)
nbtd=0
outer_loop: do
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
nbtsend=0
do i=1,nbts
irank=iegrpv(btsendq(i)%iegb)
inbr=ranknbr(irank)
if(inbr==0) then
write(errmsg,*) 'INTER_BTRACK: bt to non-neighbor!',irank
call parallel_abort(errmsg)
endif
nbtsend(inbr)=nbtsend(inbr)+1
if(nbtsend(inbr)>mnbt) call parallel_abort('bktrk_subs: overflow (1)')
ibtsend(nbtsend(inbr),inbr)=i-1
enddo
do inbr=1,nnbr
if(nbtsend(inbr)/=0) then
#if MPIVERSION==1
call mpi_type_indexed(nbtsend(inbr),bbtsend,ibtsend(1,inbr),bt_mpitype, &
btsend_type(inbr),ierr)
#elif MPIVERSION==2
call mpi_type_create_indexed_block(nbtsend(inbr),1,ibtsend(1,inbr),bt_mpitype, &
btsend_type(inbr),ierr)
#endif
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: create btsend_type',ierr)
call mpi_type_commit(btsend_type(inbr),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: commit btsend_type',ierr)
endif
enddo
do inbr=1,nnbr
call mpi_irecv(nbtrecv(inbr),1,itype,nbrrank(inbr),700,comm,btrecv_rqst(inbr),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: irecv 700',ierr)
enddo
do inbr=1,nnbr
call mpi_isend(nbtsend(inbr),1,itype,nbrrank(inbr),700,comm,btsend_rqst(inbr),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: isend 700',ierr)
enddo
call mpi_waitall(nnbr,btrecv_rqst,btrecv_stat,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: waitall recv 700',ierr)
call mpi_waitall(nnbr,btsend_rqst,btsend_stat,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: waitall send 700',ierr)
i=0
nnbrq=0;
do inbr=1,nnbr
if(nbtrecv(inbr)/=0) then
nnbrq=nnbrq+1
if(nbtrecv(inbr)>mnbt) call parallel_abort('bktrk_subs: overflow (3)')
do j=1,nbtrecv(inbr); ibtrecv(j,inbr)=i+j-1; enddo;
i=i+nbtrecv(inbr)
#if MPIVERSION==1
call mpi_type_indexed(nbtrecv(inbr),bbtrecv,ibtrecv(1,inbr),bt_mpitype, &
btrecv_type(inbr),ierr)
#elif MPIVERSION==2
call mpi_type_create_indexed_block(nbtrecv(inbr),1,ibtrecv(1,inbr),bt_mpitype, &
btrecv_type(inbr),ierr)
#endif
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: create btrecv_type',ierr)
call mpi_type_commit(btrecv_type(inbr),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: commit btrecv_type',ierr)
endif
enddo
if(i>mnbt*nnbr) call parallel_abort('bktrk_subs: overflow (2)')
do inbr=1,nnbr
if(nbtsend(inbr)/=0) then
call mpi_isend(btsendq(1)%rank,1,btsend_type(inbr),nbrrank(inbr),701, &
comm,btsend_rqst(inbr),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: isend 701',ierr)
else
btsend_rqst(inbr)=MPI_REQUEST_NULL
endif
enddo
do inbr=1,nnbr
if(nbtrecv(inbr)/=0) then
call mpi_irecv(btrecvq(1)%rank,1,btrecv_type(inbr),nbrrank(inbr),701, &
comm,btrecv_rqst(inbr),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: irecv 701',ierr)
else
btrecv_rqst(inbr)=MPI_REQUEST_NULL
endif
enddo
#ifdef INCLUDE_TIMING
wtimer(icw,2)=wtimer(icw,2)+mpi_wtime()-cwtmp
#endif
nbts=0
inner_loop: do
if(nnbrq==0) exit inner_loop
#ifdef DEBUG
write(30,'(a)') 'INNER LOOP'
#endif
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_waitsome(nnbr,btrecv_rqst,ncmplt,icmplt,btrecv_stat,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: waitsome recv 701',ierr)
#ifdef INCLUDE_TIMING
wtimer(icw,2)=wtimer(icw,2)+mpi_wtime()-cwtmp
#endif
do ii=1,ncmplt
inbr=icmplt(ii)
do j=1,nbtrecv(inbr)
i=ibtrecv(j,inbr)+1
ie=iegl(btrecvq(i)%iegb)%id
call btrack(btrecvq(i)%l0,btrecvq(i)%i0gb,btrecvq(i)%isbndy,btrecvq(i)%j0, &
&btrecvq(i)%adv,btrecvq(i)%gcor0,btrecvq(i)%frame0,btrecvq(i)%dtbk,btrecvq(i)%vis, &
&btrecvq(i)%rt,btrecvq(i)%rt2,btrecvq(i)%ut,btrecvq(i)%vt,btrecvq(i)%wt, &
&ie,btrecvq(i)%jvrt,btrecvq(i)%xt,btrecvq(i)%yt,btrecvq(i)%zt, &
&btrecvq(i)%sclr,lexit)
if(lexit) then
nbts=nbts+1
btrecvq(i)%iegb=ielg(ie)
if(nbts>mnbt) call parallel_abort('bktrk_subs: overflow (5)')
bttmp(nbts)=btrecvq(i)
else
nbtd=nbtd+1
btrecvq(i)%iegb=ielg(ie)
if(nbtd>mnbt) call parallel_abort('bktrk_subs: overflow (4)')
btdone(nbtd)=btrecvq(i)
endif
enddo
enddo
nnbrq=nnbrq-ncmplt
enddo inner_loop
#ifdef DEBUG
write(30,'(a)') 'DONE INNER LOOP'
#endif
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
call mpi_waitall(nnbr,btsend_rqst,btsend_stat,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: waitall send 701',ierr)
do inbr=1,nnbr
if(nbtsend(inbr)/=0) then
call mpi_type_free(btsend_type(inbr),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: free btsend_type',ierr)
endif
enddo
do inbr=1,nnbr
if(nbtrecv(inbr)/=0) then
call mpi_type_free(btrecv_type(inbr),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: free btrecv_type',ierr)
endif
enddo
#ifdef DEBUG
write(30,'(a,4i6)') 'CYCLE OUTER: ',itime,nbts,nbtd
#endif
bt_donel=(nbts==0)
call mpi_allreduce(bt_donel,bt_done,1,MPI_LOGICAL,MPI_LAND,comm,ierr)
#ifdef INCLUDE_TIMING
wtimer(icw,2)=wtimer(icw,2)+mpi_wtime()-cwtmp
#endif
if(bt_done) exit outer_loop
btsendq(1:nbts)=bttmp(1:nbts)
enddo outer_loop
deallocate(btsendq,btrecvq,bttmp,stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: bt type deallocation failure (1)')
#ifdef INCLUDE_TIMING
cwtmp=mpi_wtime()
#endif
#ifdef DEBUG
write(30,'(a)') 'Start all-rank communication'
#endif
deallocate(nbtsend,ibtsend,nbtrecv,ibtrecv, &
btsend_type,btsend_rqst,btsend_stat, &
btrecv_type,btrecv_rqst,btrecv_stat)
#if MPIVERSION==1
deallocate(bbtsend,bbtrecv)
#endif
allocate(nbtsend(0:nproc-1),stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: nbtsend allocation failure')
nbtsend=0
do i=1,nbtd
irank=btdone(i)%rank
if(irank/=myrank) nbtsend(irank)=nbtsend(irank)+1
enddo
mxbtsend=0; do irank=0,nproc-1; mxbtsend=max(mxbtsend,nbtsend(irank)); enddo;
allocate(ibtsend(mxbtsend,0:nproc-1),stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: ibtsend allocation failure')
#if MPIVERSION==1
allocate(bbtsend(mxbtsend),stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: bbtsend allocation failure')
bbtsend=1
#endif
nbtsend=0
do i=1,nbtd
irank=btdone(i)%rank
if(irank/=myrank) then
nbtsend(irank)=nbtsend(irank)+1
ibtsend(nbtsend(irank),irank)=i-1
endif
enddo
#ifdef DEBUG
write(30,'(a,66i6)') 'INTER_BTRACK -- NBTSEND: ', &
itime,(nbtsend(irank),irank=0,nproc-1)
#endif
allocate(nbtrecv(0:nproc-1),stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: nbtrecv allocation failure')
call mpi_alltoall(nbtsend,1,itype,nbtrecv,1,itype,comm,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: alltoall nbtsend',ierr)
#ifdef DEBUG
write(30,'(a,66i6)') 'INTER_BTRACK -- NBTRECV: ', &
itime,(nbtrecv(irank),irank=0,nproc-1)
#endif
mxbtrecv=0; do irank=0,nproc-1; mxbtrecv=max(mxbtrecv,nbtrecv(irank)); enddo;
allocate(ibtrecv(mxbtrecv,0:nproc-1),stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: ibtrecv allocation failure')
#if MPIVERSION==1
allocate(bbtrecv(mxbtrecv),stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: bbtrecv allocation failure')
bbtrecv=1
#endif
allocate(btsend_type(0:nproc-1),btsend_rqst(0:nproc-1), &
btsend_stat(MPI_STATUS_SIZE,0:nproc-1), &
btrecv_type(0:nproc-1),btrecv_rqst(0:nproc-1), &
btrecv_stat(MPI_STATUS_SIZE,0:nproc-1),stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: bt type/rqst/stat allocation failure')
do irank=0,nproc-1
if(nbtsend(irank)/=0) then
if(irank==myrank) call parallel_abort('INTER_BTRACK: self communication (1)')
#if MPIVERSION==1
call mpi_type_indexed(nbtsend(irank),bbtsend,ibtsend(1,irank),bt_mpitype, &
btsend_type(irank),ierr)
#elif MPIVERSION==2
call mpi_type_create_indexed_block(nbtsend(irank),1,ibtsend(1,irank),bt_mpitype, &
btsend_type(irank),ierr)
#endif
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: create btsend_type',ierr)
call mpi_type_commit(btsend_type(irank),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: commit btsend_type',ierr)
endif
enddo
i=0
do irank=0,nproc-1
if(nbtrecv(irank)/=0) then
if(irank==myrank) call parallel_abort('INTER_BTRACK: self communication (2)')
do j=1,nbtrecv(irank); ibtrecv(j,irank)=i+j-1; enddo;
i=i+nbtrecv(irank)
#if MPIVERSION==1
call mpi_type_indexed(nbtrecv(irank),bbtrecv,ibtrecv(1,irank),bt_mpitype, &
btrecv_type(irank),ierr)
#elif MPIVERSION==2
call mpi_type_create_indexed_block(nbtrecv(irank),1,ibtrecv(1,irank),bt_mpitype, &
btrecv_type(irank),ierr)
#endif
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: create btrecv_type',ierr)
call mpi_type_commit(btrecv_type(irank),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: commit btrecv_type',ierr)
endif
enddo
if(i>mxnbt) call parallel_abort('INTER_BTRACK: overflow (6)')
do irank=0,nproc-1
if(nbtsend(irank)/=0) then
call mpi_isend(btdone(1)%rank,1,btsend_type(irank),irank,711, &
comm,btsend_rqst(irank),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: isend 711',ierr)
else
btsend_rqst(irank)=MPI_REQUEST_NULL
endif
enddo
do irank=0,nproc-1
if(nbtrecv(irank)/=0) then
call mpi_irecv(btlist(1)%rank,1,btrecv_type(irank),irank,711, &
comm,btrecv_rqst(irank),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: irecv 711',ierr)
else
btrecv_rqst(irank)=MPI_REQUEST_NULL
endif
enddo
call mpi_waitall(nproc,btrecv_rqst,btrecv_stat,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: waitall recv 711',ierr)
call mpi_waitall(nproc,btsend_rqst,btsend_stat,ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: waitall send 711',ierr)
do ii=1,nbtd
irank=btdone(ii)%rank
if(irank==myrank) then
i=i+1
if(i>mxnbt) call parallel_abort('INTER_BTRACK: overflow (7)')
btlist(i)=btdone(ii)
#ifdef DEBUG
write(30,*)'Back to myself!'
#endif
endif
enddo
if(i/=nbt) call parallel_abort('bktrk_subs: mismatch (1)')
do irank=0,nproc-1
if(nbtsend(irank)/=0) then
call mpi_type_free(btsend_type(irank),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: free btsend_type',ierr)
endif
enddo
do irank=0,nproc-1
if(nbtrecv(irank)/=0) then
call mpi_type_free(btrecv_type(irank),ierr)
if(ierr/=MPI_SUCCESS) call parallel_abort('INTER_BTRACK: free btrecv_type',ierr)
endif
enddo
deallocate(btdone,stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: btdone deallocation failure')
deallocate(nbtsend,ibtsend,nbtrecv,ibtrecv, &
btsend_type,btsend_rqst,btsend_stat, &
btrecv_type,btrecv_rqst,btrecv_stat,stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: rqst/stat deallocation failure')
#if MPIVERSION==1
deallocate(bbtsend,bbtrecv,stat=stat)
if(stat/=0) call parallel_abort('INTER_BTRACK: bbtsend/recv deallocation failure')
#endif
#ifdef INCLUDE_TIMING
wtimer(icw,2)=wtimer(icw,2)+mpi_wtime()-cwtmp
#endif
#ifdef DEBUG
close(99)
#endif
end subroutine inter_btrack
subroutine btrack(l_ns,ipsgb,ifl_bnd,j0,iadvf,gcor0,frame0,dtbk, &
&vis_coe,time_rm,time_rm2,uuint,vvint,wwint,nnel,jlev,xt,yt,zt,sclr,iexit)
use elfe_glbl
use elfe_msgp, only : parallel_abort,myrank
implicit real(rkind)(a-h,o-z),integer(i-n)
integer, intent(in) :: l_ns,ipsgb,ifl_bnd,j0,iadvf
real(rkind), intent(in) :: gcor0(3),frame0(3,3),dtbk,vis_coe
real(rkind), intent(inout) :: time_rm,time_rm2,uuint,vvint,wwint,xt,yt,zt
integer, intent(inout) :: nnel,jlev
real(rkind), intent(out) :: sclr(4)
logical, intent(out) :: iexit
real(rkind) :: vxl(3,2),vyl(3,2),vzl(3,2),vxn(3),vyn(3),vzn(3)
real(rkind) :: arco(3),t_xi(6),s_xi(6),sig(3),subrat(4),ztmp(nvrt), &
&swild(10),swild2(nvrt,10),swild3(nvrt)
real(rkind) :: al_beta(mnei_kr+3,4),uvdata(mnei_kr,2)
logical :: lrk
iexit=.false.
lrk=iadvf==2.and.time_rm2>0
if(iadvf==1.or.iadvf==0.or.lrk) then
x0=xt
y0=yt
z0=zt
idt=0
do
idt=idt+1
if(time_rm2>0) then
dtb=time_rm2
time_rm2=-99
else
dtb=min(dtbk,time_rm)
endif
if(dtb<=0) call parallel_abort('BTRACK: dtb<=0')
xt=x0-dtb*uuint
yt=y0-dtb*vvint
zt=z0-dtb*wwint
call quicksearch(1,idt,l_ns,ipsgb,gcor0,frame0,dtb,x0,y0,z0,nnel,jlev, &
&xt,yt,zt,trm,iflqs1,kbpl,arco,zrat,ztmp,vis_coe,uuint,vvint,wwint, &
&uuint1,vvint1,wwint1)
if(iflqs1==2) then
if(time_rm2>0) call parallel_abort('BTRACK: just in')
time_rm2=-99
iexit=.true.; return
endif
if(iflqs1==3) then
if(trm<=0) call parallel_abort('BTRACK: trm<=0')
time_rm2=trm
time_rm=time_rm-(dtb-trm)
iexit=.true.; return
endif
uuint=uuint1; vvint=vvint1; wwint=wwint1
vmag=sqrt(uuint**2+vvint**2)
if(vmag<=velmin_btrack.or.iflqs1==1) then
time_rm=0; exit
endif
if(lrk) then
time_rm=time_rm-dtb
exit
endif
time_rm=time_rm-(dtb-trm)
if(time_rm<=1.e-6*dt) exit
x0=xt
y0=yt
z0=zt
end do
endif
if(iadvf==2.and.time_rm>=1.e-6*dt) then
x0=xt
y0=yt
z0=zt
iadptive=0
idt=0
dtb=min(dtbk,time_rm)
if(dtb<=0) call parallel_abort('BTRACK: dtb<=0 (2a)')
nnel0=nnel; jlev0=jlev
do
idt=idt+1
xt=x0-0.5*dtb*uuint
yt=y0-0.5*dtb*vvint
zt=z0-0.5*dtb*wwint
nnel=nnel0; jlev=jlev0
call quicksearch(2,idt,l_ns,ipsgb,gcor0,frame0,0.5*dtb,x0,y0,z0,nnel,jlev, &
&xt,yt,zt,trm,iflqs1,kbpl,arco,zrat,ztmp,vis_coe,uuint,vvint,wwint, &
&uuint1,vvint1,wwint1)
if(iflqs1==2) then
if(iadptive/=0) call parallel_abort('BTRACK: adp. wrong')
time_rm2=-99
iexit=.true.; return
endif
if(iflqs1==3) then
if(iadptive>=1) then
write(errmsg,*)'BTRACK: iadptive>=1:',iadptive,0.5*dtb,trm
call parallel_abort(errmsg)
endif
if(trm<=0) call parallel_abort('BTRACK: trm<=0 (2a)')
dtb=dtb-2*trm
dtb=dtb*(1-1.e-3)
if(dtb<=0) call parallel_abort('BTRACK: dtb<=0 (2b)')
iadptive=iadptive+1
cycle
endif
iadptive=0
uuint=uuint1; vvint=vvint1; wwint=wwint1
vmag=sqrt(uuint**2+vvint**2)
if(vmag<=velmin_btrack.or.iflqs1==1) exit
xt=x0-dtb*uuint
yt=y0-dtb*vvint
zt=z0-dtb*wwint
nnel=nnel0; jlev=jlev0
call quicksearch(3,idt,l_ns,ipsgb,gcor0,frame0,dtb,x0,y0,z0,nnel,jlev, &
&xt,yt,zt,trm,iflqs1,kbpl,arco,zrat,ztmp,vis_coe,uuint,vvint,wwint, &
&uuint1,vvint1,wwint1)
if(iflqs1==2) then
time_rm2=-99
iexit=.true.; return
endif
if(iflqs1==3) then
if(trm<=0) call parallel_abort('BTRACK: trm<=0 (2b)')
time_rm2=trm
time_rm=time_rm-(dtb-trm)
iexit=.true.; return
endif
uuint=uuint1; vvint=vvint1; wwint=wwint1
vmag=sqrt(uuint**2+vvint**2)
if(vmag<=velmin_btrack.or.iflqs1==1) exit
time_rm=time_rm-dtb
if(time_rm<=1.e-6*dt) exit
dtb=min(dtbk,time_rm)
x0=xt
y0=yt
z0=zt
nnel0=nnel; jlev0=jlev
end do
endif
if(l_ns==3) return
if(ifl_bnd/=1.and.krvel(nnel)==1) then
if(nnel>ne) then
time_rm=1.e-8*dt
iexit=.true.; return
else
ie=ie_kr(nnel)
if(ie==0) then
write(errmsg,*)'Out of Kriging zone:',ielg(nnel)
call parallel_abort(errmsg)
endif
npp=itier_nd(ie,0)
do i=1,npp
nd=itier_nd(ie,i)
if(idry(nd)==1) then
uvdata(i,1)=0
uvdata(i,2)=0
else
uvdata(i,1)=uu2(jlev,nd)*(1-zrat)+uu2(jlev-1,nd)*zrat
uvdata(i,2)=vv2(jlev,nd)*(1-zrat)+vv2(jlev-1,nd)*zrat
endif
enddo
do i=1,npp+3
al_beta(i,1:2)=0
do j=1,npp
al_beta(i,1:2)=al_beta(i,1:2)+akrmat_nd(ie,i,j)*uvdata(j,1:2)
enddo
enddo
uuint=al_beta(npp+1,1)+al_beta(npp+2,1)*xt+al_beta(npp+3,1)*yt
vvint=al_beta(npp+1,2)+al_beta(npp+2,2)*xt+al_beta(npp+3,2)*yt
do i=1,npp
nd=itier_nd(ie,i)
rr=sqrt((xnd(nd)-xt)**2+(ynd(nd)-yt)**2)
covar2=covar(kr_co,rr)
uuint=uuint+al_beta(i,1)*covar2
vvint=vvint+al_beta(i,2)*covar2
enddo
endif
endif
ttint=0; ssint=0
sclr=0
if(zrat<0.or.zrat>1) then
write(errmsg,*)'BTRACK: zrat wrong:',jlev,zrat
call parallel_abort(errmsg)
endif
if(iupwind_t/=0) return
if(lqk(nnel)==1) then
ifl=0
do i=1,4
if(i<=3) then
n1=nm(nnel,i)
n2=js(nnel,nx(i,2))
n3=js(nnel,nx(i,1))
if(ics==1) then
xn1=xnd(n1); yn1=ynd(n1)
xn2=xcj(n2); yn2=ycj(n2)
xn3=xcj(n3); yn3=ycj(n3)
else
call project_pt('g2l',xnd(n1),ynd(n1),znd(n1),gcor0,frame0,xn1,yn1,tmp)
call project_pt('g2l',xcj(n2),ycj(n2),zcj(n2),gcor0,frame0,xn2,yn2,tmp)
call project_pt('g2l',xcj(n3),ycj(n3),zcj(n3),gcor0,frame0,xn3,yn3,tmp)
endif
aa1=signa(xt,xn2,xn3,yt,yn2,yn3)
aa2=signa(xn1,xt,xn3,yn1,yt,yn3)
aa3=signa(xn1,xn2,xt,yn1,yn2,yt)
subrat(i)=min(aa1,aa2,aa3)/area(nnel)
if(subrat(i)>=-small2) then
ifl=1
sig(1)=aa1*4/area(nnel)
sig(2)=aa2*4/area(nnel)
sig(1)=max(0._rkind,min(1._rkind,sig(1)))
sig(2)=max(0._rkind,min(1._rkind,sig(2)))
if(sig(1)+sig(2)>1) then
sig(3)=0
sig(2)=1-sig(1)
else
sig(3)=1-sig(1)-sig(2)
endif
do jj=1,3
if(jj==1) then
kbb=kbp(n1)
swild3(kbb:nvrt)=znl(kbb:nvrt,n1)
swild2(kbb:nvrt,1)=tnd(kbb:nvrt,n1)
swild2(kbb:nvrt,2)=snd(kbb:nvrt,n1)
else if(jj==2) then
kbb=kbs(n2)
swild3(kbb:nvrt)=zs(kbb:nvrt,n2)
swild2(kbb:nvrt,1)=tsd(kbb:nvrt,n2)
swild2(kbb:nvrt,2)=ssd(kbb:nvrt,n2)
else
kbb=kbs(n3)
swild3(kbb:nvrt)=zs(kbb:nvrt,n3)
swild2(kbb:nvrt,1)=tsd(kbb:nvrt,n3)
swild2(kbb:nvrt,2)=ssd(kbb:nvrt,n3)
endif
call vinter(nvrt,10,2,zt,kbb,nvrt,jlev,swild3,swild2,swild,ibelow)
t_xi(jj)=swild(1); s_xi(jj)=swild(2)
enddo
ttint=t_xi(1)*sig(1)+t_xi(2)*sig(2)+t_xi(3)*sig(3)
ssint=s_xi(1)*sig(1)+s_xi(2)*sig(2)+s_xi(3)*sig(3)
exit
endif
else
n1=js(nnel,1)
n2=js(nnel,2)
n3=js(nnel,3)
if(ics==1) then
xn1=xcj(n1); yn1=ycj(n1)
xn2=xcj(n2); yn2=ycj(n2)
xn3=xcj(n3); yn3=ycj(n3)
else
call project_pt('g2l',xcj(n1),ycj(n1),zcj(n1),gcor0,frame0,xn1,yn1,tmp)
call project_pt('g2l',xcj(n2),ycj(n2),zcj(n2),gcor0,frame0,xn2,yn2,tmp)
call project_pt('g2l',xcj(n3),ycj(n3),zcj(n3),gcor0,frame0,xn3,yn3,tmp)
endif
aa1=signa(xt,xn2,xn3,yt,yn2,yn3)
aa2=signa(xn1,xt,xn3,yn1,yt,yn3)
aa3=signa(xn1,xn2,xt,yn1,yn2,yt)
subrat(i)=min(aa1,aa2,aa3)/area(nnel)
if(subrat(i)>=-small2) then
ifl=1
sig(1)=aa1*4/area(nnel)
sig(2)=aa2*4/area(nnel)
sig(1)=max(0._rkind,min(1._rkind,sig(1)))
sig(2)=max(0._rkind,min(1._rkind,sig(2)))
if(sig(1)+sig(2)>1) then
sig(3)=0
sig(2)=1-sig(1)
else
sig(3)=1-sig(1)-sig(2)
endif
do jj=1,3
isd=js(nnel,jj)
kbb=kbs(isd)
swild3(kbb:nvrt)=zs(kbb:nvrt,isd)
swild2(kbb:nvrt,1)=tsd(kbb:nvrt,isd)
swild2(kbb:nvrt,2)=ssd(kbb:nvrt,isd)
call vinter(nvrt,10,2,zt,kbb,nvrt,jlev,swild3,swild2,swild,ibelow)
t_xi(jj)=swild(1); s_xi(jj)=swild(2)
enddo
ttint=t_xi(1)*sig(1)+t_xi(2)*sig(2)+t_xi(3)*sig(3)
ssint=s_xi(1)*sig(1)+s_xi(2)*sig(2)+s_xi(3)*sig(3)
exit
endif
endif
enddo
if(ifl==0) then
write(errmsg,*)'BTRACK: Not in any sub-element',ielg(nnel),(subrat(i),i=1,4),xt,yt
call parallel_abort(errmsg)
endif
else if(lqk(nnel)==2) then
do i=1,3
nd=nm(nnel,i)
isd=js(nnel,i)
call eval_cubic_spline(nvrt-kbp(nd)+1,znl(kbp(nd):nvrt,nd),tnd(kbp(nd):nvrt,nd), &
&cspline_ypp_nd(1,kbp(nd):nvrt,nd),1,zt,0,znl(kbp(nd),nd),znl(nvrt,nd),vxl(i,1))
call eval_cubic_spline(nvrt-kbp(nd)+1,znl(kbp(nd):nvrt,nd),snd(kbp(nd):nvrt,nd), &
&cspline_ypp_nd(2,kbp(nd):nvrt,nd),1,zt,0,znl(kbp(nd),nd),znl(nvrt,nd),vxl(i,2))
call eval_cubic_spline(nvrt-kbs(isd)+1,zs(kbs(isd):nvrt,isd),tsd(kbs(isd):nvrt,isd), &
&cspline_ypp_sd(1,kbs(isd):nvrt,isd),1,zt,0,zs(kbs(isd),isd),zs(nvrt,isd),vyl(i,1))
call eval_cubic_spline(nvrt-kbs(isd)+1,zs(kbs(isd):nvrt,isd),ssd(kbs(isd):nvrt,isd), &
&cspline_ypp_sd(2,kbs(isd):nvrt,isd),1,zt,0,zs(kbs(isd),isd),zs(nvrt,isd),vyl(i,2))
tnd_max=maxval(tnd(kbp(nd):nvrt,nd))
tnd_min=minval(tnd(kbp(nd):nvrt,nd))
vxl(i,1)=max(tnd_min,min(tnd_max,vxl(i,1)))
snd_max=maxval(snd(kbp(nd):nvrt,nd))
snd_min=minval(snd(kbp(nd):nvrt,nd))
vxl(i,2)=max(snd_min,min(snd_max,vxl(i,2)))
tsd_max=maxval(tsd(kbs(isd):nvrt,isd))
tsd_min=minval(tsd(kbs(isd):nvrt,isd))
vyl(i,1)=max(tsd_min,min(tsd_max,vyl(i,1)))
ssd_max=maxval(ssd(kbs(isd):nvrt,isd))
ssd_min=minval(ssd(kbs(isd):nvrt,isd))
vyl(i,2)=max(ssd_min,min(ssd_max,vyl(i,2)))
enddo
ttint=0; ssint=0
do i=1,3
nd=nm(nnel,i)
isd=js(nnel,i)
in1=nx(i,1)
in2=nx(i,2)
ttint=ttint+vxl(i,1)*(2*arco(i)*arco(i)-arco(i))+vyl(i,1)*4*arco(in1)*arco(in2)
ssint=ssint+vxl(i,2)*(2*arco(i)*arco(i)-arco(i))+vyl(i,2)*4*arco(in1)*arco(in2)
enddo
t_max=max(maxval(vxl(1:3,1)),maxval(vyl(1:3,1)))
t_min=min(minval(vxl(1:3,1)),minval(vyl(1:3,1)))
s_max=max(maxval(vxl(1:3,2)),maxval(vyl(1:3,2)))
s_min=min(minval(vxl(1:3,2)),minval(vyl(1:3,2)))
ttint=max(t_min,min(t_max,ttint))
ssint=max(s_min,min(s_max,ssint))
endif
do i=1,3
nd=nm(nnel,i)
vxl(i,1)=dfv(nd,jlev)*(1-zrat)+dfv(nd,jlev-1)*zrat
vxl(i,2)=dfh(nd,jlev)*(1-zrat)+dfh(nd,jlev-1)*zrat
enddo
dfvint=sum(vxl(1:3,1)*arco(1:3))
dfhint=sum(vxl(1:3,2)*arco(1:3))
sclr(1)=ttint; sclr(2)=ssint
sclr(3)=dfvint; sclr(4)=dfhint
end subroutine btrack
subroutine quicksearch(idx,itr,l_ns,ipsgb,gcor0,frame0,time,x0,y0,z0,nnel, &
&jlev,xt,yt,zt,trm,nfl,kbpl,arco,zrat,ztmp,vis_coe,uuint0,vvint0,wwint0, &
&uuint,vvint,wwint)
use elfe_glbl
use elfe_msgp, only : myrank,parallel_abort
implicit real(rkind)(a-h,o-z),integer(i-n)
integer, intent(in) :: idx,itr,l_ns,ipsgb
real(rkind), intent(in) :: gcor0(3),frame0(3,3),time,x0,y0,z0,vis_coe,uuint0,vvint0,wwint0
integer, intent(inout) :: nnel,jlev
real(rkind), intent(inout) :: xt,yt,zt
integer, intent(out) :: nfl,kbpl
real(rkind), intent(out) :: trm,arco(3),zrat,ztmp(nvrt),uuint,vvint,wwint
real(rkind) :: wild(10,2),wild2(10,2)
real(rkind) :: vxl(3,2),vyl(3,2),vzl(3,2),vxn(3),vyn(3),vzn(3)
xt00=xt
yt00=yt
nnel00=nnel
jlev00=jlev
if(idry_e(nnel)==1) then
write(errmsg,*)'QUICKSEARCH: Starting element is dry:',idry_e(nnel)
call parallel_abort(errmsg)
endif
nfl=0
trm=time
nel=nnel
xcg=x0; ycg=y0
pathl=sqrt((xt-xcg)**2+(yt-ycg)**2)
if(pathl==0.or.trm==0) then
write(errmsg,*)'QUICKSEARCH: Zero path',idx,itr,l_ns,ipsgb,ielg(nel),jlev, &
&x0,y0,xt,yt,xcg,ycg,time,uuint0,vvint0,wwint0
call parallel_abort(errmsg)
endif
call area_coord(0,nel,gcor0,frame0,xt,yt,arco)
ar_min2=minval(arco)
if(ar_min2>-small1) then
if(ar_min2<=0) call area_coord(1,nel,gcor0,frame0,xt,yt,arco)
nnel=nel
trm=0
go to 400
endif
wild=0; wild2=0
nel_j=0
do j=1,3
jd1=nm(nel,nx(j,1))
jd2=nm(nel,nx(j,2))
if(ics==1) then
xn1=xnd(jd1); yn1=ynd(jd1)
xn2=xnd(jd2); yn2=ynd(jd2)
else
call project_pt('g2l',xnd(jd1),ynd(jd1),znd(jd1),gcor0,frame0,xn1,yn1,zn1)
call project_pt('g2l',xnd(jd2),ynd(jd2),znd(jd2),gcor0,frame0,xn2,yn2,zn2)
endif
ar1=signa(xcg,xn1,xt,ycg,yn1,yt)
ar2=signa(xcg,xt,xn2,ycg,yt,yn2)
wild2(j,1)=ar1; wild2(j,2)=ar2
if(ar1>0.and.ar2>0) then
call intersect2(xcg,xt,xn1,xn2,ycg,yt,yn1,yn2,iflag,xin,yin,tt1,tt2)
wild(j,1)=tt1; wild(j,2)=tt2; wild(3+j,1)=xin; wild(3+j,2)=yin
if(iflag/=1) then
if(ics==1) then
xcg2=xcg; ycg2=ycg; zcg2=0; xt2=xt; yt2=yt; zt2=0
else
call project_pt('l2g',xcg,ycg,0.d0,gcor0,frame0,xcg2,ycg2,zcg2)
call project_pt('l2g',xt,yt,0.d0,gcor0,frame0,xt2,yt2,zt2)
endif
write(errmsg,*)'QUICKSEARCH: Found no intersecting edges (1):',idx,itr, &
&ielg(nel),xcg2,ycg2,zcg2,xt2,yt2,zt2,ar_min1,ar_min2,wild(1:3,1:2),wild(4:6,1:2),ar1,ar2, &
&xcg,ycg,xt,yt,time,trm,jlev,uuint0,vvint0,wwint0,jlev00
call parallel_abort(errmsg)
else
nel_j=j; exit
endif
endif
enddo
if(nel_j==0) then
if(ics==1) then
xcg2=xcg; ycg2=ycg; zcg2=0; xt2=xt; yt2=yt; zt2=0
else
call project_pt('l2g',xcg,ycg,0.d0,gcor0,frame0,xcg2,ycg2,zcg2)
call project_pt('l2g',xt,yt,0.d0,gcor0,frame0,xt2,yt2,zt2)
endif
write(errmsg,*)'QUICKSEARCH: no intersecting edge; start ID (node/side/elem)=', &
&l_ns,'; start gb. node/side/elem #=',ipsgb,'; start level=',jlev,'; current elem=',ielg(nel), &
&'; cg (local) coord.=',xcg2,ycg2,zcg2,'; end coord.=',xt2,yt2,zt2, &
&'; signed areas (cg,1,t)@ nodes followed by (cg,t,2)@ nodes=',wild2(1:3,1:2), &
&'; xcg,ycg,xt,yt=',xcg,ycg,xt,yt, &
&'; time step from cg to t=',time,'; time remaining=',trm, &
&'; min. area coord. for cg, t=',ar_min1,ar_min2,'; input vel=',uuint0,vvint0,wwint0, &
&idx,itr,jlev00
call parallel_abort(errmsg)
endif
if(ic3(nel,nel_j)<0) then
xt=x0; yt=y0; zt=z0; nnel=nel
nfl=2; return
endif
zin=z0
it=0
loop4: do
it=it+1
if(it>1000) then
if(ifort12(3)==0) then
ifort12(3)=1
write(12,*)'QUICKSEARCH: Death trap reached'
endif
nfl=1
xt=xin
yt=yin
zt=zin
nnel=nel
trm=0
exit loop4
endif
md1=nm(nel,nx(nel_j,1))
md2=nm(nel,nx(nel_j,2))
dist=sqrt((xin-xt)**2+(yin-yt)**2)
tmp=min(1._rkind,dist/pathl)
zin=zt-tmp*(zt-zin)
trm=trm*tmp
pathl=dist
if(dist==0.or.trm==0) then
call area_coord(1,nel,gcor0,frame0,xt,yt,arco)
nnel=nel
trm=0
exit loop4
endif
if(ic3(nel,nel_j)<0) then
nfl=3
xt=xin
yt=yin
zt=zin
nnel=nel
trm=min(trm,time)
nnel=nel
return
endif
lit=0
if(ic3(nel,nel_j)==0.or.idry_e(max(1,ic3(nel,nel_j)))==1) then
lit=1
isd=js(nel,nel_j)
if(isidenode(isd,1)+isidenode(isd,2)/=md1+md2) then
write(errmsg,*)'QUICKSEARCH: Wrong side'
call parallel_abort(errmsg)
endif
eps=1.e-2
if(ics==1) then
xctr3=xctr(nel); yctr3=yctr(nel)
else
call project_pt('g2l',xctr(nel),yctr(nel),zctr(nel),gcor0,frame0,xctr3,yctr3,tmp)
endif
xin=(1-eps)*xin+eps*xctr3
yin=(1-eps)*yin+eps*yctr3
xcg=xin
ycg=yin
if(ics==1) then
vtan=su2(jlev,isd)*sframe(1,2,isd)+sv2(jlev,isd)*sframe(2,2,isd)
xvel=vtan*sframe(1,2,isd)
yvel=vtan*sframe(2,2,isd)
else
call project_hvec(0.d0,sv2(jlev,isd),sframe(:,:,isd),frame0,xvel,yvel)
endif
zvel=(ww2(jlev,md1)+ww2(jlev,md2))/2
xt=xin-xvel*trm
yt=yin-yvel*trm
zt=zin-zvel*trm
hvel=sqrt(xvel**2+yvel**2)
if(hvel<=velmin_btrack) then
nfl=1
xt=xin
yt=yin
zt=zin
nnel=nel
trm=0
exit loop4
endif
pathl=hvel*trm
endif
if(lit==0) nel=ic3(nel,nel_j)
call area_coord(0,nel,gcor0,frame0,xt,yt,arco)
ar_min1=minval(arco)
if(ar_min1>-small1) then
if(ar_min1<=0) call area_coord(1,nel,gcor0,frame0,xt,yt,arco)
nnel=nel
trm=0
exit loop4
endif
wild=0; wild2=0
nel_j=0
do j=1,3
jd1=nm(nel,nx(j,1))
jd2=nm(nel,nx(j,2))
if(jd1==md1.and.jd2==md2.or.jd2==md1.and.jd1==md2) cycle
if(ics==1) then
xn1=xnd(jd1); yn1=ynd(jd1)
xn2=xnd(jd2); yn2=ynd(jd2)
else
call project_pt('g2l',xnd(jd1),ynd(jd1),znd(jd1),gcor0,frame0,xn1,yn1,tmp)
call project_pt('g2l',xnd(jd2),ynd(jd2),znd(jd2),gcor0,frame0,xn2,yn2,tmp)
endif
ar1=signa(xcg,xn1,xt,ycg,yn1,yt)
ar2=signa(xcg,xt,xn2,ycg,yt,yn2)
wild2(j,1)=ar1; wild2(j,2)=ar2
if(ar1>0.and.ar2>0) then
call intersect2(xcg,xt,xn1,xn2,ycg,yt,yn1,yn2,iflag,xin,yin,tt1,tt2)
wild(j,1)=tt1; wild(j,2)=tt2; wild(3+j,1)=xin; wild(3+j,2)=yin
if(iflag/=1) then
if(ics==1) then
xcg2=xcg; ycg2=ycg; zcg2=0; xt2=xt; yt2=yt; zt2=0
else
call project_pt('l2g',xcg,ycg,0.d0,gcor0,frame0,xcg2,ycg2,zcg2)
call project_pt('l2g',xt,yt,0.d0,gcor0,frame0,xt2,yt2,zt2)
endif
write(errmsg,*)'QUICKSEARCH: Failed to find next edge (2):',lit,idx,itr,l_ns,ipsgb, &
&xcg2,ycg2,zcg2,xt2,yt2,zt2,ielg(nel),iplg(md1),iplg(md2),ar_min1, &
&wild(1:3,1:2),wild(4:6,1:2),ar1,ar2,xcg,ycg,xt,yt,time,trm,uuint0,vvint0,wwint0
call parallel_abort(errmsg)
else
nel_j=j;
cycle loop4
endif
endif
enddo
if(nel_j==0) then
if(ics==1) then
xcg2=xcg; ycg2=ycg; zcg2=0; xt2=xt; yt2=yt; zt2=0
else
call project_pt('l2g',xcg,ycg,0.d0,gcor0,frame0,xcg2,ycg2,zcg2)
call project_pt('l2g',xt,yt,0.d0,gcor0,frame0,xt2,yt2,zt2)
endif
write(errmsg,*)'QUICKSEARCH: no intersecting edge (2): ',idx,itr,l_ns,ipsgb,ielg(nel), &
&xcg2,ycg2,zcg2,xt2,yt2,zt2,wild2(1:3,1:2),xcg,ycg,xt,yt,time,trm,uuint0,vvint0,wwint0
call parallel_abort(errmsg)
endif
end do loop4
400 continue
if(idry_e(nnel)==1) then
write(errmsg,*)'QUICKSEARCH: Ending element is dry:',ielg(nnel)
call parallel_abort(errmsg)
endif
call area_coord(0,nnel,gcor0,frame0,xt,yt,arco)
n1=nm(nnel,1)
n2=nm(nnel,2)
n3=nm(nnel,3)
etal=eta2(n1)*arco(1)+eta2(n2)*arco(2)+eta2(n3)*arco(3)
dep=dp(n1)*arco(1)+dp(n2)*arco(2)+dp(n3)*arco(3)
if(etal+dep<=h0) then
write(errmsg,*)'QUICKSEARCH: Weird wet element in quicksearch:',ielg(nnel),eta2(n1),eta2(n2),eta2(n3)
call parallel_abort(errmsg)
endif
do k=kz,nvrt
kin=k-kz+1
hmod2=min(dep,h_s)
if(hmod2<=h_c) then
ztmp(k)=sigma(kin)*(hmod2+etal)+etal
else if(etal<=-h_c-(dep-h_c)*theta_f/s_con1) then
write(errmsg,*)'QUICKSEARCH: Pls choose a larger h_c (2):',etal,h_c
call parallel_abort(errmsg)
else
ztmp(k)=etal*(1+sigma(kin))+h_c*sigma(kin)+(hmod2-h_c)*cs(kin)
endif
if(k==kz) ztmp(k)=-hmod2
if(k==nvrt) ztmp(k)=etal
enddo
if(dep<=h_s) then
kbpl=kz
else
kbpl=0
do k=1,kz-1
if(-dep>=ztot(k).and.-dep<ztot(k+1)) then
kbpl=k
exit
endif
enddo
if(kbpl==0) then
write(errmsg,*)'QUICKSEARCH: Cannot find a bottom level at foot:',dep
call parallel_abort(errmsg)
endif
ztmp(kbpl)=-dep
do k=kbpl+1,kz-1
ztmp(k)=ztot(k)
enddo
endif
do k=kbpl+1,nvrt
if(ztmp(k)-ztmp(k-1)<=0) then
write(errmsg,*)'QUICKSEARCH: Inverted z-level in quicksearch:',ielg(nnel),etal,dep,ztmp(k)-ztmp(k-1)
call parallel_abort(errmsg)
endif
enddo
if(zt<=ztmp(kbpl)) then
zt=ztmp(kbpl)
zrat=1
jlev=kbpl+1
else if(zt>=ztmp(nvrt)) then
zt=ztmp(nvrt)
zrat=0
jlev=nvrt
else
jlev=0
do k=kbpl,nvrt-1
if(zt>=ztmp(k).and.zt<=ztmp(k+1)) then
jlev=k+1
exit
endif
enddo
if(jlev==0) then
write(errmsg,*)'QUICKSEARCH: Cannot find a vert. level:',zt,etal,dep,(ztmp(k),k=kbpl,nvrt)
call parallel_abort(errmsg)
endif
zrat=(ztmp(jlev)-zt)/(ztmp(jlev)-ztmp(jlev-1))
endif
if(zrat<0.or.zrat>1) then
write(errmsg,*)'QUICKSEARCH: Sigma coord. wrong (4):',jlev,zrat
call parallel_abort(errmsg)
endif
if(indvel==-1) then
do j=1,3
nd=nm(nnel,j)
isd=js(nnel,j)
if(ics==1) then
vxn(j)=su2(jlev,isd)*(1-zrat)+su2(jlev-1,isd)*zrat
vyn(j)=sv2(jlev,isd)*(1-zrat)+sv2(jlev-1,isd)*zrat
else
call project_hvec(su2(jlev,isd),sv2(jlev,isd),sframe(:,:,isd),frame0,uj,vj)
call project_hvec(su2(jlev-1,isd),sv2(jlev-1,isd),sframe(:,:,isd),frame0,uj1,vj1)
vxn(j)=uj*(1-zrat)+uj1*zrat
vyn(j)=vj*(1-zrat)+vj1*zrat
endif
vzn(j)=ww2(jlev,nd)*(1-zrat)+ww2(jlev-1,nd)*zrat
enddo
uuint=vxn(1)*(1-2*arco(1))+vxn(2)*(1-2*arco(2))+vxn(3)*(1-2*arco(3))
vvint=vyn(1)*(1-2*arco(1))+vyn(2)*(1-2*arco(2))+vyn(3)*(1-2*arco(3))
wwint=vzn(1)*arco(1)+vzn(2)*arco(2)+vzn(3)*arco(3)
else
do j=1,3
nd=nm(nnel,j)
do l=1,2
lev=jlev+l-2
if(ics==1) then
uu=uu2(lev,nd); vv=vv2(lev,nd)
uf=ufg(lev,nnel,j); vf=vfg(lev,nnel,j)
else
call project_hvec(uu2(lev,nd),vv2(lev,nd),pframe(:,:,nd),frame0,uu,vv)
call project_hvec(ufg(lev,nnel,j),vfg(lev,nnel,j),eframe(:,:,nnel),frame0,uf,vf)
endif
vxl(j,l)=(1-vis_coe)*uu+vis_coe*uf
vyl(j,l)=(1-vis_coe)*vv+vis_coe*vf
vzl(j,l)=ww2(lev,nd)
enddo
enddo
do j=1,3
vxn(j)=vxl(j,2)*(1-zrat)+vxl(j,1)*zrat
vyn(j)=vyl(j,2)*(1-zrat)+vyl(j,1)*zrat
vzn(j)=vzl(j,2)*(1-zrat)+vzl(j,1)*zrat
enddo
uuint=vxn(1)*arco(1)+vxn(2)*arco(2)+vxn(3)*arco(3)
vvint=vyn(1)*arco(1)+vyn(2)*arco(2)+vyn(3)*arco(3)
wwint=vzn(1)*arco(1)+vzn(2)*arco(2)+vzn(3)*arco(3)
endif
end subroutine quicksearch
subroutine intersect2(x1,x2,x3,x4,y1,y2,y3,y4,iflag,xin,yin,tt1,tt2)
use elfe_glbl, only: rkind
implicit real(rkind)(a-h,o-z), integer(i-n)
real(rkind), parameter :: small=0.0
real(rkind), intent(in) :: x1,x2,x3,x4,y1,y2,y3,y4
integer, intent(out) :: iflag
real(rkind), intent(out) :: xin,yin,tt1,tt2
tt1=-1000
tt2=-1000
xin=-1.e25; yin=xin
iflag=0
delta=(x2-x1)*(y3-y4)-(y2-y1)*(x3-x4)
delta1=(x3-x1)*(y3-y4)-(y3-y1)*(x3-x4)
delta2=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
if(delta/=0) then
tt1=delta1/delta
tt2=delta2/delta
if(tt2>=-small.and.tt2<=1+small) then
iflag=1
xin=x3+(x4-x3)*tt2
yin=y3+(y4-y3)*tt2
endif
endif
end subroutine intersect2
