module smoothvsc_mod
public smoothvsc
interface smoothvsc; module procedure    &
   smoothvsc_ ; end interface
contains
subroutine smoothvsc_(nvars,sfvc,vpvc,tvc,qvc,cvc,ozvc,qivc,qlvc,qrvc,qsvc)
  use type_kinds,only: fp_kind,double
  use postmod, only: ndeg,nasm
  use variables,only: nlat,nsig,sfvln,vpvln,tvln,qvln,cvln,ozvln
  use variables,only: qivln,qlvln,qrvln,qsvln
  use variables,only: hydromet
  implicit none

  integer nvars
  real(double),pointer,dimension(:,:,:),intent(in):: sfvc,vpvc,tvc,qvc,cvc,ozvc
  real(double),pointer,dimension(:,:,:),intent(in):: qivc,qlvc,qrvc,qsvc

  real(fp_kind),dimension(nlat,nsig*nvars):: vsc_out

  real(double),dimension(nsig,nasm):: table
  real(double),dimension(nasm)::sum
  real(fp_kind) amin,scale
  integer i,j,l,k,k2,k3,k4,k5,ll,kkkk

  real(fp_kind),dimension(nsig,ndeg):: alv
  real(fp_kind),dimension(nsig):: be,rate,dssv,vwl
  real(fp_kind) samp,fact,ak,delta,awgt
  real(fp_kind) turn(ndeg,ndeg)
  real(fp_kind) w(nsig)
  real(double)  weights(nsig)
  integer nav
  integer idx

  vwl=1.3
  call rfdpar1(be,rate,ndeg)
  call rfdpar2(be,rate,turn,samp,ndeg)

  scale=.01
  nav=530
  vsc_out=0.

  do l=1,nsig
    do i=1,nasm
      vwl=scale*float(i)
      w=0.
      w(l)=1.
      call rfdparv(vwl,rate,alv,nsig,ndeg)
      do k=1,nsig
        dssv(k)=sqrt(samp*vwl(k))
      enddo

      call smoothz(w,nsig,1,ndeg,alv,be,dssv,1)
      call smoothz(w,nsig,1,ndeg,alv,be,dssv,2)
      fact=1./w(l)
      do k=1,nsig
        table(k,i)=w(k)*fact
      enddo
    enddo

    awgt=10.0
    do k=1-l,nsig-l
      weights(l+k)=exp(-(awgt*k*k)/(nsig*nsig))
    end do
!!    print *,weights

    do ll=1,nvars
! ll=1  z
! ll=2  d
! ll=3  t
! ll=4  q
! ll=5  clw
! ll=6  oz
! ll=7  qi
! ll=8  ql
! ll=9  qr
! ll=10 qs
      do j=1,nlat
        ak=0.
        amin=999.
        sum=0.
        do i=1,nav
          do k=1-l,nsig-l
            if (ll.eq.1) then
              sum(i)=sum(i)+weights(l+k)*(sfvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.2) then
              sum(i)=sum(i)+weights(l+k)*(vpvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.3) then
              sum(i)=sum(i)+weights(l+k)*(tvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.4) then
              sum(i)=sum(i)+weights(l+k)*(qvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.5) then
              sum(i)=sum(i)+weights(l+k)*(cvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.6) then
              sum(i)=sum(i)+weights(l+k)*(ozvc(j,l,l+k)-table(l+k,i))**2.
            else if (nvars>6 .and. ll.eq.7) then
              sum(i)=sum(i)+weights(l+k)*(qivc(j,l,l+k)-table(l+k,i))**2.
            else if (nvars>7 .and. ll.eq.8) then
              sum(i)=sum(i)+weights(l+k)*(qlvc(j,l,l+k)-table(l+k,i))**2.
            else if (nvars>8 .and. ll.eq.9) then
              sum(i)=sum(i)+weights(l+k)*(qrvc(j,l,l+k)-table(l+k,i))**2.
            else if (nvars>9 .and. ll.eq.10) then
              sum(i)=sum(i)+weights(l+k)*(qsvc(j,l,l+k)-table(l+k,i))**2.
            end if
          enddo
          if(sum(i) < amin)then
            amin=sum(i)
            kkkk=i
          endif
        enddo
        i=kkkk
        ak=float(i)
        delta=0.
        if(i > 1 .and. i < nav)delta=.5*(sum(i-1)-sum(i+1))/ &
          (sum(i+1)+sum(i-1)-2.*sum(i))
        vsc_out(j,(ll-1)*nsig+l)=scale*(ak+delta)
      enddo !enddo nlat

    enddo !enddo ll

  enddo ! end l -- nsig

  do k=1,nsig
    do i=1,nlat
      sfvln(i,k)=vsc_out(i,k)
      vpvln(i,k)=vsc_out(i,nsig+k)
      tvln(i,k)=vsc_out(i,2*nsig+k)
      qvln(i,k)=vsc_out(i,3*nsig+k)
      cvln(i,k)=vsc_out(i,4*nsig+k)
      ozvln(i,k)=vsc_out(i,5*nsig+k)
    end do
  end do
  if (hydromet) then
     do k=1,nsig
       do i=1,nlat
         idx=5
         idx=idx+1
         qivln(i,k)=vsc_out(i,idx*nsig+k)
         idx=idx+1
         qlvln(i,k)=vsc_out(i,idx*nsig+k)
         idx=idx+1
         qrvln(i,k)=vsc_out(i,idx*nsig+k)
         idx=idx+1
         qsvln(i,k)=vsc_out(i,idx*nsig+k)
       end do
     end do
  endif

  return
end subroutine smoothvsc_
end module smoothvsc_mod

subroutine vertlength(mycases,numcases,npes,mype)
  use type_kinds,only: fp_kind,double
  use postmod,only: smoothlat,smver
  use variables,only: nsig,nlat,nlon,lsmver,vsmc
  use variables,only: smoothdeg,anaclw
  use variables,only: sfvln,vpvln,tvln,qvln,cvln,ozvln,tcon,vpcon
  use variables,only: qivln,qlvln,qrvln,qsvln
  use variables,only: grdsf,grdvp,gridt,grdc,gridq,grdrh,gridp,grdoz
  use variables,only: gridqi,gridql,gridqr,gridqs
  use variables,only: hydromet
  use smoothvsc_mod, only: smoothvsc

#ifndef ibm_sp
  use m_mpif
#endif
  implicit none
#ifdef ibm_sp
  include 'mpif.h'
#endif

  integer,intent(in):: mype,mycases,numcases,npes
  integer i,j,k,m,ierror,mpi_rtype,nn,idx
  integer nvars
  
  real(fp_kind) norm
  real(fp_kind) r_small
  real(double),pointer,dimension(:,:,:):: sfvc,vpvc,tvc,qvc,cvc,ozvc
  real(double),pointer,dimension(:,:,:):: qivc,qlvc,qrvc,qsvc
  real(double),allocatable,dimension(:,:):: diag
  real(fp_kind),allocatable,dimension(:,:):: balvar

  mpi_rtype=mpi_real8
  r_small=1.e-8

  allocate(sfvc(nlat,nsig,nsig), &
           vpvc(nlat,nsig,nsig), &
           tvc (nlat,nsig,nsig), &
           qvc (nlat,nsig,nsig), &
           cvc (nlat,nsig,nsig), &
           ozvc(nlat,nsig,nsig)  )
  if (hydromet) then
     nvars=10
     allocate(qivc(nlat,nsig,nsig), &
              qlvc(nlat,nsig,nsig), &
              qrvc(nlat,nsig,nsig), &
              qsvc(nlat,nsig,nsig) )
  else
     nvars=6
  endif
  allocate(diag(nsig,nvars))

  if (mype==0) write(6,*) 'IN ROUTINE TO CALCULATE VERT LENGTH SCALES'

! notice that a lot of this routine needs to be done in double precision, there
! are problems with the vertical recursive filter bits if stuff is done in single
! precision

  allocate(balvar(nlat,nlon))

  sfvc=0.
  vpvc=0.
  tvc=0.
  qvc=0.
  
  if (hydromet) then
     qivc=0.
     qlvc=0.
     qrvc=0.
     qsvc=0.
  endif
  cvc=0.
  ozvc=0.

  do nn=1,mycases

#ifdef gmao_intf
    call m_rdgrids(nn,npes,mype,mycases)
#else
!    call convert2grid(nn,npes,mype,mycases) ! does not exist anymore
#endif
  if (mype==0) write(6,*) 'vertlength, nn = ', nn

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

! get vertical correlation matrices

    do m=1,nsig
      do k=1,nsig
        do j=1,nlon
          do i=2,nlat-1
            sfvc(i,k,m)=sfvc(i,k,m)+grdsf(i,j,k)*grdsf(i,j,m)
            vpvc(i,k,m)=vpvc(i,k,m)+grdvp(i,j,k)*grdvp(i,j,m)
            tvc(i,k,m)=tvc(i,k,m)+gridt(i,j,k)*gridt(i,j,m)
            qvc(i,k,m)=qvc(i,k,m)+gridq(i,j,k)*gridq(i,j,m)
            ozvc(i,k,m)=ozvc(i,k,m)+grdoz(i,j,k)*grdoz(i,j,m)
          end do
        end do
      end do
    end do

    if (hydromet) then
       do m=1,nsig
         do k=1,nsig
           do j=1,nlon
             do i=2,nlat-1
               qivc(i,k,m)=qivc(i,k,m)+gridqi(i,j,k)*gridqi(i,j,m)
               qlvc(i,k,m)=qlvc(i,k,m)+gridql(i,j,k)*gridql(i,j,m)
               qrvc(i,k,m)=qrvc(i,k,m)+gridqr(i,j,k)*gridqr(i,j,m)
               qsvc(i,k,m)=qsvc(i,k,m)+gridqs(i,j,k)*gridqs(i,j,m)
             end do
           end do
         end do
       end do
    endif


    if(anaclw)then
      do m=1,nsig
      do k=1,nsig
        do j=1,nlon
          do i=2,nlat-1
            cvc(i,k,m)=cvc(i,k,m)+grdc(i,j,k)*grdc(i,j,m)
          end do
        end do
      end do
      end do
    else
      do m=1,nsig
      do i=2,nlat-1
        cvc(i,m,m)=1.0
      end do
      end do
    endif

  end do !end do cases on mype (n)
  if (mype==0) write(6,*) 'vertlength, done handling separate cases'

  call mpi_allreduce((sfvc),sfvc,nlat*nsig*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((vpvc),vpvc,nlat*nsig*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((tvc),tvc,nlat*nsig*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((qvc),qvc,nlat*nsig*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  if (hydromet) then
     call mpi_allreduce((qivc),qivc,nlat*nsig*nsig,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce((qlvc),qlvc,nlat*nsig*nsig,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce((qrvc),qrvc,nlat*nsig*nsig,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce((qsvc),qsvc,nlat*nsig*nsig,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
  endif
  call mpi_allreduce((cvc),cvc,nlat*nsig*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((ozvc),ozvc,nlat*nsig*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)

  norm=1/float(nlon*numcases)
  sfvc=sfvc*norm
  vpvc=vpvc*norm
  tvc=tvc*norm
  qvc=qvc*norm
  if (hydromet) then
     qivc=qivc*norm
     qlvc=qlvc*norm
     qrvc=qrvc*norm
     qsvc=qsvc*norm
  endif
  cvc=cvc*norm
  ozvc=ozvc*norm
  if (mype==0) write(6,*) 'vertlength, after all-reduce'

  do i=2,nlat-1
    do k=1,nsig
      idx=1
      diag(k,idx)=sqrt(sfvc(i,k,k))
      idx=idx+1
      diag(k,idx)=sqrt(vpvc(i,k,k))
      idx=idx+1
      diag(k,idx)=sqrt(tvc(i,k,k))
      idx=idx+1
      diag(k,idx)=sqrt(max(r_small,qvc(i,k,k)))
      idx=idx+1
      diag(k,idx)=sqrt(max(r_small,cvc(i,k,k)))
      idx=idx+1
      diag(k,idx)=sqrt(ozvc(i,k,k))
      if (hydromet) then
         idx=idx+1
         diag(k,idx)=sqrt(max(r_small,qivc(i,k,k)))
         idx=idx+1
         diag(k,idx)=sqrt(max(r_small,qlvc(i,k,k)))
         idx=idx+1
         diag(k,idx)=sqrt(max(r_small,qrvc(i,k,k)))
         idx=idx+1
         diag(k,idx)=sqrt(max(r_small,qsvc(i,k,k)))
      endif
    end do
    do m=1,nsig
      do k=1,nsig
        idx=1
        sfvc(i,k,m)=sfvc(i,k,m)/(diag(k,idx)*diag(m,idx))
        idx=idx+1
        vpvc(i,k,m)=vpvc(i,k,m)/(diag(k,idx)*diag(m,idx))
        idx=idx+1
        tvc(i,k,m)=tvc(i,k,m)/(diag(k,idx)*diag(m,idx))
        idx=idx+1
        qvc(i,k,m)=qvc(i,k,m)/(diag(k,idx)*diag(m,idx))
        idx=idx+1
        cvc(i,k,m)=cvc(i,k,m)/(diag(k,idx)*diag(m,idx))
        idx=idx+1
        ozvc(i,k,m)=ozvc(i,k,m)/(diag(k,idx)*diag(m,idx))
        if (hydromet) then
           idx=idx+1
           qivc(i,k,m)=qivc(i,k,m)/(diag(k,idx)*diag(m,idx))
           idx=idx+1
           qlvc(i,k,m)=qlvc(i,k,m)/(diag(k,idx)*diag(m,idx))
           idx=idx+1
           qrvc(i,k,m)=qrvc(i,k,m)/(diag(k,idx)*diag(m,idx))
           idx=idx+1
           qsvc(i,k,m)=qsvc(i,k,m)/(diag(k,idx)*diag(m,idx))
        endif
      end do
    end do
  end do !end do over lat

  do m=1,nsig
    do k=1,nsig
      sfvc(1,k,m)=sfvc(2,k,m)
      sfvc(nlat,k,m)=sfvc(nlat-1,k,m)
      vpvc(1,k,m)=vpvc(2,k,m)
      vpvc(nlat,k,m)=vpvc(nlat-1,k,m)
      tvc(1,k,m)=tvc(2,k,m)
      tvc(nlat,k,m)=tvc(nlat-1,k,m)
      qvc(1,k,m)=qvc(2,k,m)
      qvc(nlat,k,m)=qvc(nlat-1,k,m)
      if (hydromet) then
         qivc(1,k,m)=qivc(2,k,m)
         qivc(nlat,k,m)=qivc(nlat-1,k,m)
         qlvc(1,k,m)=qlvc(2,k,m)
         qlvc(nlat,k,m)=qlvc(nlat-1,k,m)
         qrvc(1,k,m)=qrvc(2,k,m)
         qrvc(nlat,k,m)=qrvc(nlat-1,k,m)
         qsvc(1,k,m)=qsvc(2,k,m)
         qsvc(nlat,k,m)=qsvc(nlat-1,k,m)
      endif
      cvc(1,k,m)=cvc(2,k,m)
      cvc(nlat,k,m)=cvc(nlat-1,k,m)
      ozvc(1,k,m)=ozvc(2,k,m)
      ozvc(nlat,k,m)=ozvc(nlat-1,k,m)
    end do
  end do

  call smoothvsc(nvars,sfvc,vpvc,tvc,qvc,cvc,ozvc,qivc,qlvc,qrvc,qsvc)
  if (mype==0) write(6,*) 'vertlength, after smoothvsc'

! The vert. length scale arrays get loaded in smoothvsc, now smooth
! them

  call smoothlat(sfvln,nsig,smoothdeg)
  call smoothlat(vpvln,nsig,smoothdeg)
  call smoothlat(tvln,nsig,smoothdeg)
  call smoothlat(qvln,nsig,smoothdeg)
  if (hydromet) then
     call smoothlat(qivln,nsig,smoothdeg)
     call smoothlat(qlvln,nsig,smoothdeg)
     call smoothlat(qrvln,nsig,smoothdeg)
     call smoothlat(qsvln,nsig,smoothdeg)
  endif
  call smoothlat(cvln,nsig,smoothdeg)
  call smoothlat(ozvln,nsig,smoothdeg)
  if (mype==0) write(6,*) 'vertlength, after smoothlat'

  if(lsmver .and. mype==0)then
    call smver(sfvln,vsmc,nlat,nsig)
    call smver(vpvln,vsmc,nlat,nsig)
    call smver(tvln,vsmc,nlat,nsig)
    call smver(qvln,vsmc,nlat,nsig)
    if (hydromet) then
       call smver(qivln,vsmc,nlat,nsig)
       call smver(qlvln,vsmc,nlat,nsig)
       call smver(qrvln,vsmc,nlat,nsig)
       call smver(qsvln,vsmc,nlat,nsig)
    endif
    call smver(cvln,vsmc,nlat,nsig)
    call smver(ozvln,vsmc,nlat,nsig)
    if (mype==0) write(6,*) 'vertlength, after smver'
  endif

  deallocate(diag)
  if (hydromet) then
     deallocate(qivc, &
                qlvc, &
                qrvc, &
                qsvc )
  endif
  deallocate(balvar)
  deallocate(sfvc, &
             vpvc, &
             tvc , &
             qvc , &
             cvc , &
             ozvc  )

  return
end subroutine vertlength

