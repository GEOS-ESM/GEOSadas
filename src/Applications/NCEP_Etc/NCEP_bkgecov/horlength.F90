subroutine horlength(mycases,numcases,npes,mype)

  use type_kinds,only: fp_kind
  use postmod,only: smoothlat,smver
  use variables,only: nlat,nlon,nsig,lgaus,lsmver,vsmth
  use variables,only: tvar,qvar,cvar,psvar,vpvar,sfvar,ozvar
  use variables,only: qivar,qlvar,qrvar,qsvar
  use variables,only: sfhln,vphln,thln,qhln,chln,pshln,ozhln
  use variables,only: qihln,qlhln,qrhln,qshln
  use variables,only: smoothdeg,db_prec,anaclw
  use variables,only: tcon,vpcon,pscon
  use variables,only: grdsf,grdvp,gridt,grdc,gridq,grdrh,gridp,grdoz
  use variables,only: gridqi,gridql,gridqr,gridqs
  use variables,only: hydromet
  use specgrid, only: jcap,init_spec_vars,destroy_spec_vars  
  use specgrid, only: init_spec_grid,destroy_spec_grid

#ifndef ibm_sp
  use m_mpif
#endif
  implicit none
#ifdef ibm_sp
  include 'mpif.h'
#endif

  integer i,j,k,m,kk,k2,ierror,mycases,numcases,mpi_rtype,mype,nn
  integer i2,i2m1,npes
  real(fp_kind) norm
  real(fp_kind) r_small,r_lim1,r_lim2
  real(fp_kind),dimension(nlat,nsig):: sflap,vplap,tlap,qlap,clap,ozlap
  real(fp_kind),allocatable,dimension(:,:):: qilap,qllap,qrlap,qslap
  real(fp_kind),dimension(nlat):: pslap
  real(fp_kind),dimension(nlat,nlon):: wrkgrid

  real(fp_kind),allocatable,dimension(:,:):: balvar

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  r_small=1.e-8
  r_lim1= 5.e+5
  r_lim2= 2.5e+3

  allocate(balvar(nlat,nlon))
  if (hydromet) then
     allocate(qilap(nlat,nsig), &
              qllap(nlat,nsig), &
              qrlap(nlat,nsig), &
              qslap(nlat,nsig)  )
  endif

  if (mype==0) write(6,*) 'IN ROUTINE TO CALCULATE HORIZ LENGTH SCALES'
  call flush(6)

  sflap=0.
  vplap=0.
  tlap=0.
  qlap=0.
  clap=0.
  ozlap=0.
  pslap=0.
  if (hydromet) then
     qilap=0.
     qllap=0.
     qrlap=0.
     qslap=0.
  endif

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
          gridt(i,j,m)=gridt(i,j,m)-balvar(i,j)
        end do
      end do
  if (mype==0) write(6,*) 'HORIZ LENGTH SCALES pass 1'
  call flush(6)

      balvar=0.
      do j=1,nlon
        do i=1,nlat
          balvar(i,j)=balvar(i,j)+vpcon(i,m)*grdsf(i,j,m)
        end do
      end do
      do j=1,nlon
        do i=1,nlat
          grdvp(i,j,m)=grdvp(i,j,m)-balvar(i,j)
        end do
      end do
    end do ! end do m levs
  if (mype==0) write(6,*) 'HORIZ LENGTH SCALES pass 2'
  call flush(6)

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
        gridp(i,j)=gridp(i,j)-balvar(i,j)
      end do
    end do

#ifdef gmao_intf
    call init_spec_vars(jcap)
    if(lgaus)then
      call init_spec_grid(nlat,nlon,lgaus)
    endif
#endif
  if (mype==0) write(6,*) 'HORIZ LENGTH SCALES pass 3'
  call flush(6)

    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          wrkgrid(i,j)=grdsf(i,j,k)/sqrt(sfvar(i,k))
        end do
      end do 

      if(lgaus)then
        call m_speclap(wrkgrid,1)
      else
        call m_calap(wrkgrid,1)
      endif

      do j=1,nlon
        do i=2,nlat-1
          sflap(i,k)=sflap(i,k)+wrkgrid(i,j)**2.
        end do
      end do
    end do
 
  if (mype==0) write(6,*) 'HORIZ LENGTH SCALES pass 4'
  call flush(6)
    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          wrkgrid(i,j)=grdvp(i,j,k)/sqrt(vpvar(i,k))
        end do
      end do
      if(lgaus)then
        call m_speclap(wrkgrid,1)
      else
        call m_calap(wrkgrid,1)
      endif
      do j=1,nlon
        do i=2,nlat-1
          vplap(i,k)=vplap(i,k)+wrkgrid(i,j)**2.
        end do
      end do
    end do

  if (mype==0) write(6,*) 'HORIZ LENGTH SCALES pass 5'
  call flush(6)
    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          wrkgrid(i,j)=gridt(i,j,k)/sqrt(tvar(i,k))
        end do
      end do
      if(lgaus)then
        call m_speclap(wrkgrid,2)
      else
        call m_calap(wrkgrid,2)
      endif
      do j=1,nlon
        do i=2,nlat-1
          tlap(i,k)=tlap(i,k)+wrkgrid(i,j)**2.
        end do
      end do
    end do
  if (mype==0) write(6,*) 'HORIZ LENGTH SCALES pass 6'
  call flush(6)

    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          wrkgrid(i,j)=gridq(i,j,k)/sqrt(qvar(i,k))
        end do
      end do
      if(lgaus)then 
        call m_speclap(wrkgrid,2)
      else
        call m_calap(wrkgrid,2)
      endif  
      do j=1,nlon
        do i=2,nlat-1
          qlap(i,k)=qlap(i,k)+wrkgrid(i,j)**2.
        end do
      end do
    end do

    if (hydromet) then
    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          wrkgrid(i,j)=max(r_small,gridqi(i,j,k))/max(r_small,sqrt(qivar(i,k)))
        end do
      end do
      if(lgaus)then 
        call m_speclap(wrkgrid,2)
      else
        call m_calap(wrkgrid,2)
      endif  
      do j=1,nlon
        do i=2,nlat-1
          qilap(i,k)=qilap(i,k)+wrkgrid(i,j)**2.
        end do
      end do
    end do

    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          wrkgrid(i,j)=max(r_small,gridql(i,j,k))/max(r_small,sqrt(qlvar(i,k)))
        end do
      end do
      if(lgaus)then 
        call m_speclap(wrkgrid,2)
      else
        call m_calap(wrkgrid,2)
      endif  
      do j=1,nlon
        do i=2,nlat-1
          qllap(i,k)=qllap(i,k)+wrkgrid(i,j)**2.
        end do
      end do
    end do

    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          wrkgrid(i,j)=max(r_small,gridqr(i,j,k))/max(r_small,sqrt(qrvar(i,k)))
        end do
      end do
      if(lgaus)then 
        call m_speclap(wrkgrid,2)
      else
        call m_calap(wrkgrid,2)
      endif  
      do j=1,nlon
        do i=2,nlat-1
          qrlap(i,k)=qrlap(i,k)+wrkgrid(i,j)**2.
        end do
      end do
    end do

    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          wrkgrid(i,j)=max(r_small,gridqs(i,j,k))/max(r_small,sqrt(qsvar(i,k)))
        end do
      end do
      if(lgaus)then 
        call m_speclap(wrkgrid,2)
      else
        call m_calap(wrkgrid,2)
      endif  
      do j=1,nlon
        do i=2,nlat-1
          qslap(i,k)=qslap(i,k)+wrkgrid(i,j)**2.
        end do
      end do
    end do
    endif ! <hydromet>

  if (mype==0) write(6,*) 'HORIZ LENGTH SCALES pass 7'
  call flush(6)
    if(anaclw)then
    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          wrkgrid(i,j)=max(r_small,grdc(i,j,k))/max(r_small,sqrt(cvar(i,k)))
        end do
      end do
      if(lgaus)then
        call m_speclap(wrkgrid,2)
      else 
        call m_calap(wrkgrid,2)
      endif
      do j=1,nlon
        do i=2,nlat-1
          clap(i,k)=clap(i,k)+wrkgrid(i,j)**2.
        end do
      end do
    end do
    endif
  if (mype==0) write(6,*) 'HORIZ LENGTH SCALES pass 8'
  call flush(6)

    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          wrkgrid(i,j)=grdoz(i,j,k)/sqrt(ozvar(i,k))
        end do
      end do
      if(lgaus)then
        call m_speclap(wrkgrid,2)
      else 
        call m_calap(wrkgrid,2)
      endif
      do j=1,nlon
        do i=2,nlat-1
          ozlap(i,k)=ozlap(i,k)+wrkgrid(i,j)**2.
        end do
      end do
    end do

    do j=1,nlon
      do i=1,nlat
        wrkgrid(i,j)=gridp(i,j)/sqrt(psvar(i))
      end do
    end do
    if(lgaus)then
      call m_speclap(wrkgrid,2)
    else
      call m_calap(wrkgrid,2)
    endif
    do j=1,nlon
      do i=2,nlat-1
        pslap(i)=pslap(i)+wrkgrid(i,j)**2.
      end do
    end do

#ifdef gmao_intf
    if(lgaus)then 
      call destroy_spec_grid
    endif
    call destroy_spec_vars
#endif

  end do !end do cases on mype (n)
  if (mype==0) then
      write(6,*) 'HORIZ LENGTH SCALES pass 9'
      call flush(6)
  endif 

  call mpi_allreduce((sflap),sflap,nlat*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((vplap),vplap,nlat*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((tlap),tlap,nlat*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((qlap),qlap,nlat*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  if (hydromet) then
     call mpi_allreduce((qilap),qilap,nlat*nsig,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce((qllap),qllap,nlat*nsig,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce((qrlap),qrlap,nlat*nsig,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce((qslap),qslap,nlat*nsig,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
  endif
  call mpi_allreduce((clap),clap,nlat*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((ozlap),ozlap,nlat*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((pslap),pslap,nlat,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  if (mype==0) write(6,*) 'HORIZ LENGTH SCALES pass 10'
  call flush(6)

  norm=1/float(nlon*numcases)
  sflap=sflap*norm
  vplap=vplap*norm
  tlap=tlap*norm
  qlap=qlap*norm
  if (hydromet) then
     qilap=qilap*norm
     qllap=qllap*norm
     qrlap=qrlap*norm
     qslap=qslap*norm
  endif
  clap=clap*norm
  ozlap=ozlap*norm
  pslap=pslap*norm

  sflap(1,:)=sflap(2,:)
  sflap(nlat,:)=sflap(nlat-1,:)
  vplap(1,:)=vplap(2,:)
  vplap(nlat,:)=vplap(nlat-1,:)
  tlap(1,:)=tlap(2,:)
  tlap(nlat,:)=tlap(nlat-1,:)
  qlap(1,:)=qlap(2,:)
  qlap(nlat,:)=qlap(nlat-1,:)
  if (hydromet) then
     qilap(1,:)=qilap(2,:)
     qilap(nlat,:)=qilap(nlat-1,:)
     qllap(1,:)=qllap(2,:)
     qllap(nlat,:)=qllap(nlat-1,:)
     qrlap(1,:)=qrlap(2,:)
     qrlap(nlat,:)=qrlap(nlat-1,:)
     qslap(1,:)=qslap(2,:)
     qslap(nlat,:)=qslap(nlat-1,:)
  endif
  clap(1,:)=clap(2,:)
  clap(nlat,:)=clap(nlat-1,:)
  ozlap(1,:)=ozlap(2,:)
  ozlap(nlat,:)=ozlap(nlat-1,:)
  pslap(1)=pslap(2)
  pslap(nlat)=pslap(nlat-1)

  do k=1,nsig
    do i=1,nlat

      sfhln(i,k)=(8./sflap(i,k))**.25
      vphln(i,k)=(8./vplap(i,k))**.25
      thln(i,k)=(8./tlap(i,k))**.25
      qhln(i,k)=(8./qlap(i,k))**.25
      ozhln(i,k)=(8./ozlap(i,k))**.25

    end do
  end do

  if (hydromet) then
     do k=1,nsig
       do i=1,nlat
         qihln(i,k)=(8./qilap(i,k))**.25
         qlhln(i,k)=(8./qllap(i,k))**.25
         qrhln(i,k)=(8./qrlap(i,k))**.25
         qshln(i,k)=(8./qslap(i,k))**.25
       end do
     end do

!!   Put bounds on cloud water horizontal scales
    do k=1,nsig
      do i=1,nlat
        qihln(i,k)=max(min(r_lim1,qihln(i,k)),r_lim2)
        qlhln(i,k)=max(min(r_lim1,qlhln(i,k)),r_lim2)
        qrhln(i,k)=max(min(r_lim1,qrhln(i,k)),r_lim2)
        qshln(i,k)=max(min(r_lim1,qshln(i,k)),r_lim2)
      end do
    end do
  endif

  if(anaclw)then
    do k=1,nsig
      do i=1,nlat
        chln(i,k)=(8./clap(i,k))**.25
      end do
    end do
  else
    chln=0
  endif

  do i=1,nlat
    pshln(i)=(8./pslap(i))**.25
  end do


  call smoothlat(sfhln,nsig,smoothdeg)
  call smoothlat(vphln,nsig,smoothdeg)
  call smoothlat(thln,nsig,smoothdeg)
  call smoothlat(qhln,nsig,smoothdeg)
  if (hydromet) then
     call smoothlat(qihln,nsig,smoothdeg)
     call smoothlat(qlhln,nsig,smoothdeg)
     call smoothlat(qrhln,nsig,smoothdeg)
     call smoothlat(qshln,nsig,smoothdeg)
  endif
  call smoothlat(chln,nsig,smoothdeg)
  call smoothlat(ozhln,nsig,smoothdeg)
  call smoothlat(pshln,1,smoothdeg)

  if(lsmver .and. mype==0)then
    call smver(sfhln,vsmth,nlat,nsig)
    call smver(vphln,vsmth,nlat,nsig)
    call smver(thln,vsmth,nlat,nsig)
    call smver(qhln,vsmth,nlat,nsig)
    if (hydromet) then
       call smver(qihln,vsmth,nlat,nsig)
       call smver(qlhln,vsmth,nlat,nsig)
       call smver(qrhln,vsmth,nlat,nsig)
       call smver(qshln,vsmth,nlat,nsig)
    endif
    call smver(chln,vsmth,nlat,nsig)
    call smver(ozhln,vsmth,nlat,nsig)
  endif

  if (hydromet) then
     deallocate(qilap, &
                qllap, &
                qrlap, &
                qslap  )
  endif
  deallocate(balvar)

  if (mype==0) write(6,*) 'ENDING THE ROUTINE TO CALCULATE HORIZ LENGTH SCALES'
  call flush(6)

  return
end subroutine horlength
