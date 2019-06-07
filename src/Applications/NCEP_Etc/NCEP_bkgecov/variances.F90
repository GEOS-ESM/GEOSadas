subroutine variances(mycases,numcases,npes,mype)
  use type_kinds, only: fp_kind,single
  use variables,  only: nlat,nlon,nsig
  use variables,  only: sfvar,vpvar,tvar,qvar,nrhvar,cvar,psvar,ozvar
  use variables,  only: qivar,qlvar,qrvar,qsvar
  use variables,  only: db_prec,anaclw
  use variables,  only: tcon,vpcon,pscon
  use variables,  only: grdsf,grdvp,gridt,grdc,gridq,grdrh,gridp,grdoz
  use variables,  only: gridqi,gridql,gridqr,gridqs
  use variables,  only: smoothdeg
  use variables,  only: hydromet
  use postmod,    only: smoothlat
#ifndef ibm_sp
  use m_mpif
#endif
  implicit none
#ifdef ibm_sp
  include 'mpif.h'
#endif

  character(255) grdfile
  integer ncfggg,iret
  integer i,j,k,m,nn,kk,k2,ibin,ll,ierror,mycases,numcases,mpi_rtype,mype
  integer npes
  real(fp_kind) norm
  real(fp_kind),allocatable,dimension(:,:):: balvar
  real(fp_kind),dimension(25,nsig):: qcount,qamp
  real(fp_kind),dimension(25):: qcavg
  real(single),allocatable,dimension(:,:,:) :: expvart
  real(single),allocatable,dimension(:,:,:) :: expvarvp
  real(single),allocatable,dimension(:,:) :: expvarsp

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  if (mype==0) write(6,*) 'IN ROUTINE TO CALCULATE VARIANCES'

  allocate(balvar(nlat,nlon))
  allocate(expvart (nlat,nsig,3))
  allocate(expvarvp(nlat,nsig,3))
  allocate(expvarsp(nlat,3))

  if (hydromet) then
     qivar=0.
     qlvar=0.
     qrvar=0.
     qsvar=0.
  endif
  qvar=0.
  cvar=0.
  ozvar=0. 
  nrhvar=0.
  sfvar=0.
  vpvar=0.
  tvar=0.
  psvar=0.
  ibin=25
  qcount=0.
  qamp=0.
  expvart=0
  expvarvp=0
  expvarsp=0

  do nn=1,mycases

#ifdef gmao_intf
    call m_rdgrids(nn,npes,mype,mycases)
#else
!    call convert2grid(nn,npes,mype,mycases) ! does not exist anymore
#endif

    do m=1,nsig
      balvar=0.
      do k=1,nsig
        do j=1,nlon
          do i=1,nlat
            balvar(i,j)=balvar(i,j)+tcon(i,m,k)*grdsf(i,j,k)
          end do
        end do
      end do
      do j=1,nlon
        do i=1,nlat
          expvart(i,m,1)=expvart(i,m,1)+gridt(i,j,m)**2
          expvart(i,m,2)=expvart(i,m,2)+balvar(i, j)**2
          gridt(i,j,m)=gridt(i,j,m)-balvar(i,j)
          expvart(i,m,3)=expvart(i,m,3)+gridt(i,j,m)**2
        end do
      end do

      balvar=0.
      do j=1,nlon
        do i=1,nlat
          balvar(i,j)=balvar(i,j)+vpcon(i,m)*grdsf(i,j,m)
        end do
      end do
      do j=1,nlon
        do i=1,nlat
          expvarvp(i,m,1)=expvarvp(i,m,1)+grdvp(i,j,m)**2
          expvarvp(i,m,2)=expvarvp(i,m,2)+balvar(i, j)**2
          grdvp(i,j,m)=grdvp(i,j,m)-balvar(i,j)
          expvarvp(i,m,3)=expvarvp(i,m,3)+grdvp(i,j,m)**2
        end do
      end do
    end do ! end do m levs

    balvar=0.
    do j=1,nlon
      do i=1,nlat
        do k=1,nsig
          balvar(i,j)=balvar(i,j)+pscon(i,k)*grdsf(i,j,k)
        end do
      end do
    end do
    do j=1,nlon
      do i=1,nlat
        expvarsp(i,1)=expvarsp(i,1)+gridp(i,j)**2
        expvarsp(i,2)=expvarsp(i,2)+balvar(i,j)**2
        gridp(i,j)=gridp(i,j)-balvar(i,j)
        expvarsp(i,3)=expvarsp(i,3)+gridp(i,j)**2
      end do
    end do

! compute variances for balanced and unbalanced variables
    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          sfvar(i,k)=sfvar(i,k)+grdsf(i,j,k)*grdsf(i,j,k)
          vpvar(i,k)=vpvar(i,k)+grdvp(i,j,k)*grdvp(i,j,k)
          tvar(i,k)=tvar(i,k)+gridt(i,j,k)*gridt(i,j,k)
          qvar(i,k)=qvar(i,k)+gridq(i,j,k)*gridq(i,j,k)
          ozvar(i,k)=ozvar(i,k)+grdoz(i,j,k)*grdoz(i,j,k)
        end do
      end do
    end do
    if (hydromet) then
       do k=1,nsig
         do j=1,nlon
           do i=1,nlat
             qivar(i,k)=qivar(i,k)+gridqi(i,j,k)*gridqi(i,j,k)
             qlvar(i,k)=qlvar(i,k)+gridql(i,j,k)*gridql(i,j,k)
             qrvar(i,k)=qrvar(i,k)+gridqr(i,j,k)*gridqr(i,j,k)
             qsvar(i,k)=qsvar(i,k)+gridqs(i,j,k)*gridqs(i,j,k)
           end do
         end do
       end do
    endif
    do j=1,nlon
      do i=1,nlat
        psvar(i)=psvar(i)+gridp(i,j)*gridp(i,j)
      end do
    end do
    if(anaclw)then
      do k=1,nsig
        do j=1,nlon
          do i=1,nlat
            cvar(i,k)=cvar(i,k)+grdc(i,j,k)*grdc(i,j,k)
          end do
        end do
      end do
    else
      cvar=0.
    endif

    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          ll=grdrh(i,j,k)*20.0+1
          ll=min0(max0(1,ll),25)
          qamp(ll,k)=qamp(ll,k)+gridq(i,j,k)**2.
          qcount(ll,k)=qcount(ll,k)+1.0
        end do
      end do
    end do
  end do ! enddo over cases for mype

  call mpi_allreduce((sfvar),sfvar,nlat*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((vpvar),vpvar,nlat*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((tvar),tvar,nlat*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((qvar),qvar,nlat*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  if (hydromet) then
     call mpi_allreduce((qivar),qivar,nlat*nsig,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce((qlvar),qlvar,nlat*nsig,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce((qrvar),qrvar,nlat*nsig,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce((qsvar),qsvar,nlat*nsig,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
  endif
  call mpi_allreduce((cvar),cvar,nlat*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((ozvar),ozvar,nlat*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((psvar),psvar,nlat,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((qamp),qamp,ibin*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((qcount),qcount,ibin*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((expvart),expvart,nlat*nsig*3,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror) 
  call mpi_allreduce((expvarvp),expvarvp,nlat*nsig*3,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror) 
  call mpi_allreduce((expvarsp),expvarsp,nlat*3,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror) 

  norm=1/float(nlon*numcases)
  sfvar=sfvar*norm
  vpvar=vpvar*norm
  tvar=tvar*norm
  qvar=qvar*norm
  if (hydromet) then
     qivar=qivar*norm
     qlvar=qlvar*norm
     qrvar=qrvar*norm
     qsvar=qsvar*norm
  endif
  cvar=cvar*norm
  ozvar=ozvar*norm
  psvar=psvar*norm
  expvart=expvart*norm
  expvarvp=expvarvp*norm
  expvarsp=expvarsp*norm

  call smoothlat(sfvar,nsig,smoothdeg)
  call smoothlat(vpvar,nsig,smoothdeg)
  call smoothlat(tvar,nsig,smoothdeg)
  call smoothlat(qvar,nsig,smoothdeg)
  if (hydromet) then
     call smoothlat(qivar,nsig,smoothdeg)
     call smoothlat(qlvar,nsig,smoothdeg)
     call smoothlat(qrvar,nsig,smoothdeg)
     call smoothlat(qsvar,nsig,smoothdeg)
  endif
  call smoothlat(cvar,nsig,smoothdeg)
  call smoothlat(ozvar,nsig,smoothdeg)
  call smoothlat(psvar,1,smoothdeg)
  qcavg=0.
  do k=1,nsig
    do ll=1,ibin
      if(qcount(ll,k).ne.0) then
        qamp(ll,k)=qamp(ll,k)/qcount(ll,k)
        nrhvar(ll,k)=qamp(ll,k)
        qcavg(ll)=qcavg(ll)+qcount(ll,k)/float(nsig)
      end if
    end do
  end do

! dtk diagnostic:
  if(mype==0) then
    do ll=1,25
      write(59,*) 'avg qcount for bin ',ll,' = ',qcavg(ll)
      write(60,*),'qcount for bin at lev=44 for bin',ll,' = ',qcount(ll,44)
    end do
  end if

  do k=1,nsig
  do i=2,nlat-1
    if(expvart(i,k,2).gt.expvart(i,k,1))then
      write(128,'(1x,a10,2i4,3(f10.5))')'bal>tot@',i,k,expvart(i,k,2), &
                                        expvart(i,k,1),expvart(i,k,1)/expvart(i,k,2)
    endif
  enddo
  enddo
   
  if (mype==0) then

   grdfile='balvar_sp.grd'
   ncfggg=len_trim(grdfile)
   call baopenwt(22,grdfile(1:ncfggg),iret)
   call wryte(22,4*nlat*nsig*3,expvart)
   call wryte(22,4*nlat*nsig*3,expvarvp)
   call wryte(22,4*nlat*3,expvarsp)
   call baclose(22,iret)

  end if

  deallocate(balvar)
  deallocate(expvart)
  deallocate(expvarvp)
  deallocate(expvarsp)

  return
end subroutine variances
