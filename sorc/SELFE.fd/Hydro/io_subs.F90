subroutine write_header1
#ifdef USE_MPIMODULE
use mpi
#endif
use elfe_glbl
use elfe_msgp
implicit none
#ifndef USE_MPIMODULE
include 'mpif.h'
#endif
integer :: i,k,m,mm,ivs,i23d,ipgb,iegb,irecm,kin
character(72) :: fgb
integer :: lfgb
character(len=48) :: variable_out
do i=1,noutput
irec(i)=0
ichan(i)=100+i
if(i>=13.and.i<=15.or.i==26) then
ivs=2
else
ivs=1
endif
if(i<=15) then
i23d=2
else
i23d=3
endif
if(iof(i)==1) then
fgb=ifile_char(1:ifile_len)//'_0000'; lfgb=len_trim(fgb);
write(fgb(lfgb-3:lfgb),'(i4.4)') myrank
open(ichan(i),file='outputs/'//(fgb(1:lfgb)//'_'//outfile(i)),status='replace')
write(ichan(i),'(a48)',advance="no") data_format
write(ichan(i),'(a48)',advance="no") version
write(ichan(i),'(a48)',advance="no") start_time
write(ichan(i),'(a48)',advance="no") variable_nm(i)
write(ichan(i),'(a48)',advance="no") variable_dim(i)
irec(i)=irec(i)+48/nbyte
a_4 = transfer(source=nrec,mold=a_4)
write(ichan(i),"(a4)",advance="no") a_4
a_4 = transfer(source=real(dt*nspool),mold=a_4)
write(ichan(i),"(a4)",advance="no") a_4
a_4 = transfer(source=nspool,mold=a_4)
write(ichan(i),"(a4)",advance="no") a_4
a_4 = transfer(source=ivs,mold=a_4)
write(ichan(i),"(a4)",advance="no") a_4
a_4 = transfer(source=i23d,mold=a_4)
write(ichan(i),"(a4)",advance="no") a_4
irec(i)=irec(i)+5
a_4 = transfer(source=nvrt,mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
a_4 = transfer(source=kz,mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
a_4 = transfer(source=real(h0),mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
a_4 = transfer(source=real(h_s),mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
a_4 = transfer(source=real(h_c),mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
a_4 = transfer(source=real(theta_b),mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
a_4 = transfer(source=real(theta_f),mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
irec(i)=irec(i)+7
do k=1,kz-1
a_4 = transfer(source=real(ztot(k)),mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
enddo
do k=kz,nvrt
kin=k-kz+1
a_4 = transfer(source=real(sigma(kin)),mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
enddo
irec(i)=irec(i)+nvrt
a_4 = transfer(source=np,mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
a_4 = transfer(source=ne,mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
irec(i)=irec(i)+2
do m=1,np
a_4 = transfer(source=real(xnd(m)),mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
a_4 = transfer(source=real(ynd(m)),mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
a_4 = transfer(source=real(dp00(m)),mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
a_4 = transfer(source=kbp00(m),mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
irec(i)=irec(i)+4
enddo
do m=1,ne
a_4 = transfer(source=3,mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
irec(i)=irec(i)+1
do mm=1,3
a_4 = transfer(source=nm(m,mm),mold=a_4)
write(ichan(i),'(a4)',advance="no") a_4
enddo
irec(i)=irec(i)+3
enddo
irecm=48/nbyte*5+5+7+nvrt+4*np+4*ne
if(i23d==2) then
irecm=irecm+(2+np+np*ivs)*nrec
else
irecm=irecm+(2+np+np*nvrt*ivs)*nrec
endif
if(irecm>mirec) then
write(errmsg,*)'Output file too large',i,irecm
call parallel_abort(errmsg)
endif
endif
enddo
igmp=0
if(noutgm==1) then
igmp=(4+3+3)*8/nbyte
fgb=ifile_char(1:ifile_len)//'_0000'; lfgb=len_trim(fgb);
write(fgb(lfgb-3:lfgb),'(i4.4)') myrank
open(100,file='outputs/'//(fgb(1:lfgb)//'_test.60'), &
access='direct',recl=nbyte,status='replace')
write(100,rec=igmp+1) nrec
write(100,rec=igmp+2) ns
write(100,rec=igmp+3) real(dt*nspool)
write(100,rec=igmp+4) nspool
write(100,rec=igmp+5) 1
igmp=igmp+5
close(100)
endif
end subroutine write_header1
subroutine write_header0(iths,it)
#ifdef USE_MPIMODULE
use mpi
#endif
use elfe_glbl
use elfe_msgp
implicit none
#ifndef USE_MPIMODULE
include 'mpif.h'
#endif
integer,intent(in) :: iths,it
integer :: i,k,m,mm,ivs,i23d,ipgb,iegb,ipgb_rec,iegb_rec,irecm,kin
real(rkind) :: cwtmp
character(len=48) :: variable_out
logical :: cwtime
cwtime=it/=iths
do i=1,noutput
irec(i)=0
ichan(i)=100+i
if(i>=13.and.i<=15.or.i==26) then
ivs=2
else
ivs=1
endif
if(i<=15) then
i23d=2
else
i23d=3
endif
if(iof(i)==1) then
#ifdef INCLUDE_TIMING
#endif
call parallel_rrsync(1)
#ifdef INCLUDE_TIMING
#endif
if(myrank==0) then
open(ichan(i),file=ifile_char(1:ifile_len)//'_'//outfile(i), &
access='direct',recl=nbyte,status='replace')
else
open(ichan(i),file=ifile_char(1:ifile_len)//'_'//outfile(i), &
access='direct',recl=nbyte,status='old')
endif
if(myrank==0) then
do m=1,48/nbyte
write(ichan(i),rec=irec(i)+m) data_format(nbyte*(m-1)+1:nbyte*m)
enddo
do m=1,48/nbyte
write(ichan(i),rec=irec(i)+48/nbyte+m) version(nbyte*(m-1)+1:nbyte*m)
enddo
do m=1,48/nbyte
write(ichan(i),rec=irec(i)+2*48/nbyte+m) start_time(nbyte*(m-1)+1:nbyte*m)
enddo
variable_out=variable_nm(i)
do m=1,48/nbyte
write(ichan(i),rec=irec(i)+3*48/nbyte+m) variable_out(nbyte*(m-1)+1:nbyte*m)
enddo
variable_out=variable_dim(i)
do m=1,48/nbyte
write(ichan(i),rec=irec(i)+4*48/nbyte+m) variable_out(nbyte*(m-1)+1:nbyte*m)
enddo
endif
irec(i)=irec(i)+5*48/nbyte
if(myrank==0) then
write(ichan(i),rec=irec(i)+1) nrec
write(ichan(i),rec=irec(i)+2) real(dt*nspool)
write(ichan(i),rec=irec(i)+3) nspool
write(ichan(i),rec=irec(i)+4) ivs
write(ichan(i),rec=irec(i)+5) i23d
endif
irec(i)=irec(i)+5
if(myrank==0) then
write(ichan(i),rec=irec(i)+1) nvrt
write(ichan(i),rec=irec(i)+2) kz
write(ichan(i),rec=irec(i)+3) real(h0)
write(ichan(i),rec=irec(i)+4) real(h_s)
write(ichan(i),rec=irec(i)+5) real(h_c)
write(ichan(i),rec=irec(i)+6) real(theta_b)
write(ichan(i),rec=irec(i)+7) real(theta_f)
endif
irec(i)=irec(i)+7
if(myrank==0) then
do k=1,kz-1
write(ichan(i),rec=irec(i)+k) real(ztot(k))
enddo
do k=kz,nvrt
kin=k-kz+1
write(ichan(i),rec=irec(i)+k) real(sigma(kin))
enddo
endif
irec(i)=irec(i)+nvrt
if(myrank==0) then
write(ichan(i),rec=irec(i)+1) np_global
write(ichan(i),rec=irec(i)+2) ne_global
endif
irec(i)=irec(i)+2
do m=1,np
ipgb=iplg(m)
if(associated(ipgl(ipgb)%next)) then
if(ipgl(ipgb)%next%rank<myrank) cycle
endif
ipgb_rec=irec(i)+4*(ipgb-1)
write(ichan(i),rec=ipgb_rec+1)real(xnd(m))
write(ichan(i),rec=ipgb_rec+2)real(ynd(m))
write(ichan(i),rec=ipgb_rec+3)real(dp(m))
write(ichan(i),rec=ipgb_rec+4)kbp00(m)
enddo
irec(i)=irec(i)+4*np_global
do m=1,ne
iegb=ielg(m)
iegb_rec=irec(i)+4*(iegb-1)
write(ichan(i),rec=iegb_rec+1) 3
do mm=1,3
write(ichan(i),rec=iegb_rec+1+mm)iplg(nm(m,mm))
enddo
enddo
irec(i)=irec(i)+4*ne_global
close(ichan(i))
#ifdef INCLUDE_TIMING
#endif
call parallel_rrsync(-1)
#ifdef INCLUDE_TIMING
#endif
irecm=48/nbyte*5+5+7+nvrt+4*np_global+4*ne_global
if(i23d==2) then
irecm=irecm+(2+np_global+np_global*ivs)*nrec
else
irecm=irecm+(2+np_global+np_global*nvrt*ivs)*nrec
endif
if(irecm>mirec) then
write(errmsg,*)'Output file too large',i,irecm
call parallel_abort(errmsg)
endif
endif
enddo
igmp=0
if(noutgm==1) then
igmp=(4+3+3)*8/nbyte
if(myrank==0) then
open(100,file=ifile_char(1:ifile_len)//'_test.60', &
access='direct',recl=nbyte,status='replace')
write(100,rec=igmp+1) nrec
write(100,rec=igmp+2) ns_global
write(100,rec=igmp+3) real(dt*nspool)
write(100,rec=igmp+4) nspool
write(100,rec=igmp+5) 1
close(100)
endif
igmp=igmp+5
endif
#ifdef INCLUDE_TIMING
#endif
call parallel_barrier
#ifdef INCLUDE_TIMING
#endif
end subroutine write_header0
subroutine write_obe
use elfe_glbl
use elfe_msgp
implicit none
integer :: i,j
open(32,file='sidecenters.bp')
if(iwrite.eq.0) then
write(32,*) 'Sidegrid'
write(32,*) ns
else
write(32,"(a)",advance="no") 'Sidegrid\n'
write(32,"(i12,a)",advance="no") ns,'\n'
endif
do i=1,ns
if(iwrite.eq.0) then
write(32,*) i,xcj(i),ycj(i),real(dps(i))
else
write(32,"(i12,a,f19.9,a,f19.9,a,f12.6,a)",advance="no") &
i," ",xcj(i)," ",ycj(i)," ",real(dps(i)),'\n'
endif
enddo
close(32)
open(32,file='centers.bp')
if(iwrite.eq.0) then
write(32,*) 'centers pts'
write(32,*) ne
else
write(32,"(a)",advance="no") 'centers pts\n'
write(32,"(i12,a)",advance="no") ne,'\n'
endif
do i=1,ne
if(iwrite.eq.0) then
write(32,*) i,xctr(i),yctr(i),real(dpe(i))
else
write(32,"(i12,a,f19.9,a,f19.9,a,f12.6,a)",advance="no") &
i," ",xctr(i)," ",yctr(i)," ",real(dpe(i)),'\n'
endif
enddo
close(32)
end subroutine write_obe
subroutine report_timers
#ifdef USE_MPIMODULE
use mpi
#endif
use elfe_glbl, only : rkind,mxtimer,wtimer
use elfe_msgp
implicit none
#ifndef USE_MPIMODULE
include 'mpif.h'
#endif
integer :: i
real(rkind) :: wavg1(0:mxtimer),wavg2(0:mxtimer)
real(rkind) :: wbuf(2,0:mxtimer)
real(rkind) :: wmin1(2,0:mxtimer),wmin2(2,0:mxtimer)
real(rkind) :: wmax1(2,0:mxtimer),wmax2(2,0:mxtimer)
do i=3,13; wtimer(2,2)=wtimer(2,2)+wtimer(i,2); enddo;
wtimer(0,2)=wtimer(1,2)+wtimer(2,2)
wtimer(:,1)=wtimer(:,1)-wtimer(:,2)
call mpi_allreduce(wtimer(0,1),wavg1,mxtimer+1,rtype,MPI_SUM,comm,ierr)
wavg1=wavg1/dble(nproc)
call mpi_allreduce(wtimer(0,2),wavg2,mxtimer+1,rtype,MPI_SUM,comm,ierr)
wavg2=wavg2/dble(nproc)
wbuf(1,:)=wtimer(:,1); wbuf(2,:)=myrank;
call mpi_allreduce(wbuf,wmin1,mxtimer+1,MPI_2DOUBLE_PRECISION,MPI_MINLOC,comm,ierr)
call mpi_allreduce(wbuf,wmax1,mxtimer+1,MPI_2DOUBLE_PRECISION,MPI_MAXLOC,comm,ierr)
wbuf(1,:)=wtimer(:,2); wbuf(2,:)=myrank;
call mpi_allreduce(wbuf,wmin2,mxtimer+1,MPI_2DOUBLE_PRECISION,MPI_MINLOC,comm,ierr)
call mpi_allreduce(wbuf,wmax2,mxtimer+1,MPI_2DOUBLE_PRECISION,MPI_MAXLOC,comm,ierr)
if(myrank==0) then
open(36,file='timer.out',form='formatted',status='replace')
write(36,'(2a)') '# ','********** Timer Index Mapping **********'
write(36,'(2a)') '# ','00 -- Total'
write(36,'(2a)') '# ','01 -- Init Section'
write(36,'(2a)') '# ','02 -- Timestepping Section'
write(36,'(2a)') '# ','03 -- Forcings & Prep Section'
write(36,'(2a)') '# ','04 -- Backtracking Section'
write(36,'(2a)') '# ','05 -- Turbulence Closure Section'
write(36,'(2a)') '# ','06 -- Matrix Preparation Section'
write(36,'(2a)') '# ','07 -- Wave-Cont. Solver Section'
write(36,'(2a)') '# ','08 -- Momentum Eqs. Solve Section'
write(36,'(2a)') '# ','09 -- Transport Section'
write(36,'(2a)') '# ','10 -- Recomputing Levels Section'
write(36,'(2a)') '# ','11 -- Conservation Check Section'
write(36,'(2a)') '# ','12 -- Global Output Section'
write(36,'(2a)') '# ','13 -- Hotstart Section'
write(36,'(/)')
write(36,'(2a)') '# ','********** Average, Min & Max Times in secs **********'
write(36,'(11a)') 'ID', &
' CompT',' MinCompT',' RankMinCompT',' MaxCompT',' RankMaxCompT', &
' CommT',' MinCommT',' RankMinCommT',' MaxCommT',' RankMaxCommT'
do i=0,13
write(36,'(i2.2,2(e13.6,2(e13.6,i13)))') i, &
wavg1(i),wmin1(1,i),int(wmin1(2,i)),wmax1(1,i),int(wmax1(2,i)), &
wavg2(i),wmin2(1,i),int(wmin2(2,i)),wmax2(1,i),int(wmax2(2,i))
enddo
if(nproc>1) close(36)
endif
call parallel_rrsync(1)
if(nproc>1) &
open(36,file='timer.out',form='formatted',status='old',position='append')
if(myrank==0) then
write(36,'(/)')
write(36,'(a)') '# ********** Computation Times (sec) For Each MPI Task **********'
write(36,'(a)') '# ********** Rows = Ranks; Columns = Timers **********'
write(36,'(a,20i13)') '# Rank',(i,i=0,13)
endif
write(36,'(a,i4.4,20e13.6)') '# ',myrank,(wtimer(i,1),i=0,13)
if(nproc>1) close(36)
call parallel_rrsync(2)
if(nproc>1) &
open(36,file='timer.out',form='formatted',status='old',position='append')
if(myrank==0) then
write(36,'(/)')
write(36,'(a)') '# ********** Communication Times For Each MPI Task **********'
write(36,'(a)') '# ********** Rows = Ranks; Columns = Timers **********'
write(36,'(a,20i13)') '# Rank',(i,i=0,13)
endif
write(36,'(a,i4.4,20e13.6)') '# ',myrank,(wtimer(i,2),i=0,13)
if(nproc>1) close(36)
call parallel_rrsync(-2)
end subroutine report_timers
subroutine get_param(varname,vartype,ivarvalue,varvalue1,varvalue2)
use elfe_glbl, only : rkind,errmsg
use elfe_msgp, only : parallel_abort,myrank
implicit real(rkind)(a-h,o-z), integer(i-n)
character(*),intent(in) :: varname
integer,intent(in) :: vartype
integer,intent(out) :: ivarvalue
real(rkind),intent(out) :: varvalue1
character(len=2),intent(out) :: varvalue2
character(len=90) :: line_str,str_tmp,str_tmp2
str_tmp2=adjustl(varname)
lstr_tmp2=len_trim(str_tmp2)
open(15,file='param.in',status='old')
rewind(15)
line=0
do
line=line+1
read(15,'(a)',end=99)line_str
line_str=adjustl(line_str)
len_str=len_trim(line_str)
if(len_str==0.or.line_str(1:1)=='!') cycle
loc=index(line_str,'=')
loc2=index(line_str,'!')
if(loc2/=0.and.loc2-1<loc+1) call parallel_abort('READ_PARAM:')
str_tmp=''
str_tmp(1:loc-1)=line_str(1:loc-1)
str_tmp=trim(str_tmp)
lstr_tmp=len_trim(str_tmp)
if(str_tmp(1:lstr_tmp)==str_tmp2(1:lstr_tmp2)) then
if(loc2/=0) then
str_tmp2=line_str(loc+1:loc2-1)
else
str_tmp2=line_str(loc+1:len_str)
endif
str_tmp2=adjustl(str_tmp2)
str_tmp2=trim(str_tmp2)
if(vartype==0) then
varvalue2=str_tmp2(1:2)
#ifdef DEBUG
if(myrank==0) write(86,*)varname,' = ',varvalue2
#endif
else if(vartype==1) then
read(str_tmp2,*)ivarvalue
#ifdef DEBUG
if(myrank==0) write(86,*)varname,' = ',ivarvalue
#endif
else if(vartype==2) then
read(str_tmp2,*)varvalue1
#ifdef DEBUG
if(myrank==0) write(86,*)varname,' = ',real(varvalue1)
#endif
else
write(errmsg,*)'read_param: unknown type:',vartype
call parallel_abort(errmsg)
endif
exit
endif
enddo
close(15)
return
99 close(15)
write(errmsg,*)'Failed to find parameter:',varname
call parallel_abort(errmsg)
end subroutine get_param
