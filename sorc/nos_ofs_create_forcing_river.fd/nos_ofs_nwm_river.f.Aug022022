        subroutine nos_ofs_nwm_river(bdate,nriv0,ntime,ius,rtime,rdis,PREFIXNOS)
        parameter(nwm_max=1000)
        include 'netcdf.inc'	

        logical found
        integer, allocatable :: Index_riv(:),Id_riv(:)
        integer, allocatable :: feature_id(:),Ius_riv(:)
        real*8, allocatable :: riv_dis(:)
        character*120 flinp(500),fname,text,label
        character*10 ofs
        character*3 version,version0
        integer bdate(4)
        integer retval,retv1,retv2
        real rtime(nwm_max),rdis(nriv0,nwm_max)
        integer ius(nriv0)
        double precision time_scale,time,ttt
        double precision riv_scale,riv_offset
        character*200 PREFIXNOS
C  Assign initial values to ius, rtime and rdis
        ntime=0
        do i=1,nwm_max
          rtime(i)=0.0
        end do

        do j=1,nriv0
          ius(j)=1
          do i=1,nwm_max
            rdis(j,i)=0.0
          end do
        end do

C  Compute the Julian Day corresponding to base_date
        call datetojd(bdate(1),bdate(2),bdate(3),jdbase)

        version0='   '
        open(2,file='version_new.dat',status='unknown')
        open(1,file='nwm_input.ctl',status='old')
        read(1,'(a10)') ofs

c  Convert OFS name input from upper case to lower case
        nlen=len_trim(ofs)
        do i=1,nlen
          ic=ichar(ofs(i:i))
          if(ic.ge.65.and.ic.le.90) then
            ofs(i:i)=char(ic+32)
          end if
        end do
C        write(*,*) 'The OFS is: ',trim(ofs)

        read(1,*) inum
        if(inum.gt.1000) then
          write(*,*) 'The file number is more than defined array.'
          write(*,*) 'Please reassign flinp array size!'
          return
        end if

        do i=1,inum
          read(1,'(a120)') flinp(i)
        end do
        close(1)
        write(*,*) 'All nwm files have been readed successfully!'

        open(12,file=trim(ofs)//'_nwm.dat',status='unknown')
c  For creofs, the day_start is -1
        tstart=-2.0001
        ntime=0

c  Find the NWM version 
        do i=1,inum
          istart=index(trim(flinp(i)),'/',back=.true.)
          ilength=len(trim(flinp(i)))

          retval=nf_open(trim(flinp(i)),nf_nowrite,ncid)
          if(retval.ne.nf_noerr) then
            write(*,*) 'NWM File: ',trim(flinp(i)),' is damage'
            call handle_err(retval)
            cycle
          else
            write(*,*) 'NWM File: ',flinp(i)(istart+1:ilength)
          end if
          
          lenn=0
          label='model_version'
          retv1=nf_inq_attid(ncid,nf_global,trim(label),lenn)
          if(retval.ne.nf_noerr) call handle_err(retval)

          label='NWM_version_number'
          retv2=nf_inq_attid(ncid,nf_global,trim(label),lenn)
          if(retval.ne.nf_noerr) call handle_err(retval)

          version='v11'
          if(retv1.eq.nf_noerr) then
            label='model_version'
            text=' '
            retval=nf_get_att_text(ncid,nf_global,trim(label),text)
            if(retval.ne.nf_noerr) call handle_err(retval)
            lenn=len_trim(text)
            version='v'//text(lenn-2:lenn-2)//text(lenn:lenn)
          else if(retv2.eq.nf_noerr) then
            label='NWM_version_number'
            text=' '
            retval=nf_get_att_text(ncid,nf_global,trim(label),text)
            if(retval.ne.nf_noerr) call handle_err(retval)
            lenn=len_trim(text)
            version='v'//text(lenn-2:lenn-2)//text(lenn:lenn)
          end if

          if(version(1:3).ne.version0(1:3)) then
            write(*,*) 'NWM version: ',trim(version)
            version0(1:3)=version(1:3)
            write(2,'(a3)') version(1:3)
          end if

c  Read river feature_id index number
          fname=trim(adjustL(PREFIXNOS))//'.'//trim(version)//
     .          '.river.index'
          inquire(file=trim(fname),exist=found)
          if(.not.found) then
            fname=trim(adjustL(PREFIXNOS))//'.nwm.reach.dat'
            inquire(file=trim(fname),exist=found)
            if(.not.found) then
              write(*,*) 'The feature_id corresponding to river ',
     .          'input file does not exist, please check!'
              close(12)
              close(2)
              return
            end if

            open(1,file=trim(fname),status='old')
            read(1,*)
            read(1,*) nriv

            if(nriv.ne.nriv0) then
              write(*,*) nriv,nriv0
              write(*,*) 'The number of rivers defined in river'
              write(*,*) 'control file is different from that'
              write(*,*) 'defined in NWM river station file. RETURN!'
              return
            end if
 
            if(allocated(Index_riv)) deallocate(Index_riv)
            allocate(Index_riv(1:nriv)); Index_riv=0
            if(allocated(Id_riv)) deallocate(Id_riv)
            allocate(Id_riv(1:nriv)); Id_riv=0
            if(allocated(Ius_riv)) deallocate(Ius_riv)
            allocate(Ius_riv(1:nriv)); Ius_riv=1
            do j=1,nriv
              read(1,*) Id_riv(j),Ius_riv(j)
            end do
            close(1)

c  Find NWM feature_id dimension
            retval=nf_inq_dimid(ncid,'feature_id',mmm_id)
            if(retval.ne.nf_noerr) call handle_err(retval)
            retval=nf_inq_dimlen(ncid,mmm_id,mmm)
            if(retval.ne.nf_noerr) call handle_err(retval)
            if(allocated(feature_id)) deallocate(feature_id)
            allocate(feature_id(1:mmm)); feature_id=0
C            write(*,*) 'NWM has feature_id number: ',mmm

c  Find NWM feature_id array
            retval=nf_inq_varid(ncid,'feature_id',item_id)
            if(retval.ne.nf_noerr) call handle_err(retval)
            retval=nf_get_var_int(ncid,item_id,feature_id)
            if(retval.ne.nf_noerr) call handle_err(retval)

c  Find river feature_id number
            do ii=1,nriv
              do jj=1,mmm
                if(feature_id(jj).eq.Id_riv(ii)) then
                  Index_riv(ii)=jj
                  exit
                end if
              end do
            end do
 
c  Save river feature_id number to file
            fname=trim(adjustL(PREFIXNOS))//'.'//trim(version)//
     .            '.river.index'
            open(1,file=trim(fname),status='unknown')
            write(1,'(i4)') nriv
            do j=1,nriv
              write(1,'(i3,i10,i2)') j,Index_riv(j),Ius_riv(j)
            end do
            close(1)
          else
            open(1,file=trim(fname),status='old')
            read(1,*) nriv
            if(nriv.ne.nriv0) then
              write(*,*) nriv,nriv0
              write(*,*) 'The number of rivers defined in river'
              write(*,*) 'control file is different from that' 
              write(*,*) 'defined in NWM river station file. RETURN!'
              return
            end if

c  Read river feature_id number from the file
            if(allocated(Index_riv)) deallocate(Index_riv)
            allocate(Index_riv(1:nriv)); Index_riv=0
            If(allocated(Ius_riv)) deallocate(Ius_riv)
            allocate(Ius_riv(1:nriv)); Ius_riv=1
            do j=1,nriv
              read(1,*) itmp,Index_riv(j),Ius_riv(j)
            end do
            close(1)
          end if
C          write(*,*) 'River feature_id numbers have been readded!'
 
          do j=1,nriv0
            Ius(j)=Ius_riv(j)
          end do

          if(allocated(riv_dis)) deallocate(riv_dis)
          allocate(riv_dis(1:nriv)); riv_dis=0.0d0

c  Get time correspoding to the NWM file
          retval=nf_inq_varid(ncid,'time',item_id)
          if(retval .eq. nf_noerr) then
            retval=nf_get_var_int(ncid,item_id,itmp)
          end if

          retval=nf_get_att_text(ncid,item_id,'units',text)
          if(retval .ne. nf_noerr) call handle_err(retval)
          
          lld=index(trim(text),'day')
          llm=index(trim(text),'minute')
          lls=index(trim(text),'second')
          if(lld.ne.0) then
            time_scale=1.0d0
          elseif(llm.ne.0) then
            time_scale=1.0d0/(24.0*60.0)
          elseif(lls.ne.0) then
            time_scale=1.0d0/(24.0*3600.0)
          else
            write(*,*) 'Time scale is wrong, return!'
            return
          end if

          ll=index(trim(text),'since ')
          read(text(ll+6:ll+9),'(i)') ny0
          read(text(ll+11:ll+12),'(i)') nm0
          read(text(ll+14:ll+15),'(i)') nd0

          call datetojd(ny0,nm0,nd0,nj0)
          time=nj0*1.0d0+itmp*time_scale
          ttt=dble(nj0-jdbase)+itmp*time_scale-dble(bdate(4)/24.0)
C          write(*,*) 'ttt=',ttt
          
          call jdtodate(int(time),ny1,nm1,nd1)
          nh1=int((time-int(time))*24+0.5)

c  Get scale_factor and add_offset values
          retval=nf_inq_varid(ncid,'streamflow', item_id)
          if(retval .ne. nf_noerr) call handle_err(retval)
          retval=nf_get_att_double(ncid,item_id,'scale_factor',
     .      riv_scale)
          if(retval .ne. nf_noerr) call handle_err(retval)
          retval=nf_get_att_double(ncid,item_id,'add_offset',
     .      riv_offset)
          if(retval .ne. nf_noerr) call handle_err(retval)

c  Get stream values of each rivers
          do j=1,nriv
            iitmp=index_riv(j)
            retval=nf_get_vara_int(ncid,item_id,iitmp,1,itmp)
            if(retval .ne. nf_noerr) call handle_err(retval)
            riv_dis(j)=dble(itmp*1.0d0)*riv_scale+riv_offset
          end do

          if(real(ttt).gt.tstart) then
C            write(*,*) 'ttt=',real(ttt),'  tstart=',tstart
            ntime=ntime+1
            rtime(ntime)=real(ttt)
            do j=1,nriv
              rdis(j,ntime)=real(riv_dis(j))
            end do
            tstart=real(ttt)
          end if

c  Close NWM NetCDF file
          retval=nf_close(ncid)
          if(retval .ne. nf_noerr) call handle_err(retval)

c  Output NWM data
          write(12,100) ny1,nm1,nd1,nh1,riv_dis(1:nriv)
        end do
        close(12)
        close(2)
100     format(i4,3(1x,i2.2),100f10.1)
        write(*,*) 'NWM data have been collected!'

        return
        end


        subroutine datetojd(yy,mm,dd,jd)
        integer yy,mm,dd,jd

        jd=dd-32075+1461*(yy+4800+(mm-14)/12)/4+
     .    367*(mm-2-(mm-14)/12*12)/12-3*((yy+4900+(mm-14)/12)/100)/4

        return
        end


        subroutine jdtodate(jd,yy,mm,dd)
        integer yy,mm,dd,jd

        nl=jd+68569
        nn=4*nl/146097
        nl=nl-(146097*nn+3)/4
        ni=4000*(nl+1)/1461001
        nl=nl-1461*ni/4+31
        nj=80*nl/2447
        nk=nl-2447*nj/80
        nl=nj/11
        nj=nj+2-12*nl
        ni=100*(nn-49)+ni+nl

        yy=ni
        mm=nj
        dd=nk

        return
        end


      	subroutine handle_err(errcode)
      	implicit none
      	include 'netcdf.inc'
      	integer errcode

      	write(*,*) 'error: ', nf_strerror(errcode)

      	return
      	end

