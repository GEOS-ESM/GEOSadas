subroutine vertlength(mycases,numcases,npes,mype)
  use type_kinds,only: fp_kind,double
  use postmod,only: smoothlat,smver
  use variables,only: nsig,nlat,nlon,lsmver,vsmc
  use variables,only: smoothdeg,anaclw
  use variables,  only: db_prec
  use variables,only: sfvln,vpvln,tvln,qvln,cvln,ozvln,tcon,vpcon
  use variables,only: qivln,qlvln,qrvln,qsvln
  use variables,only: ijn,displs_g,iglobal,ltosi,ltosj
  use variables,only: lat1,lon1
  use variables, only: filunit,biasrm,bbt,bbs,bbv,bbp
  use delmod,    only: delvars,hydrobias

#ifndef ibm_sp
  use m_mpif
#endif
  implicit none
#ifdef ibm_sp
  include 'mpif.h'
#endif

  integer,intent(in):: mype,mycases,numcases,npes
  integer i,j,k,m,n,nn,kk,k2,ibin,ll,ierror,mpi_rtype
  integer :: mype_work,mm1,ni1,ni2
  
  real(fp_kind) :: r_norm
  real(fp_kind),dimension(lat1,lon1,nsig) :: grdsf,grdvp,gridt,gridq,grdrh,grdoz,grdc
  real(fp_kind),dimension(lat1,lon1,nsig) :: gridqi,gridql,gridqr,gridqs
  real(fp_kind),dimension(lat1,lon1):: gridp
  real(fp_kind),dimension(lat1,lon1,nsig,nsig):: sf4,vp4,t4,q4,qi4,ql4,qr4,qs4,oz4,cw4
  real(fp_kind),allocatable,dimension(:):: work1
  real(fp_kind),allocatable,dimension(:,:):: workgrd
  real(double),dimension(nlat,nsig,nsig):: sfvc,vpvc,tvc,qvc,qivc,qlvc,qrvc,qsvc,cvc,ozvc
  real(double),dimension(nsig,10):: diag
  real(double) :: r_small

!  mpi_rtype=mpi_real8
  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

! notice that a lot of this routine needs to be done in double precision, there
! are problems with the vertical recursive filter bits if stuff is done in single
! precision

  if (mype==0) write(6,*) 'IN ROUTINE TO CALCULATE VERT LENGTH SCALES'

  mype_work=0
  mm1=mype+1
  r_norm=1./float(numcases)
  r_small=1.e-8

  allocate(workgrd(nlat,nlon))
  allocate(work1(iglobal))
  workgrd=0.
  work1=0.

  sfvc=0.
  vpvc=0.
  tvc=0.
  qvc=0.
  qivc=0.
  qlvc=0.
  qrvc=0.
  qsvc=0.
  cvc=0.
  ozvc=0.

  sf4=0. ; vp4=0. ; t4=0. ; q4=0. ; oz4=0. ; cw4=0.
  qi4=0. ; ql4=0. ;qr4=0. ;qs4=0.

  filunit = 1000*(mype+1)+1
  open(filunit,form='unformatted')
  rewind(filunit)

!  do nn=1,mycases
  do nn=1,numcases
    read(filunit) grdsf,grdvp,gridt,gridp,gridq,grdrh,gridqi,gridql,gridqr,gridqs,grdoz,grdc
    call delvars  (grdsf,grdvp,gridt,gridp,gridq,grdoz,grdc,mype)
    call hydrobias(gridqi,gridql,gridqr,gridqs)
    do m=1,nsig
      do k=1,nsig
        do j=1,lon1
          do i=1,lat1
            sf4(i,j,k,m) = sf4(i,j,k,m) + grdsf(i,j,k)*grdsf(i,j,m)
            vp4(i,j,k,m) = vp4(i,j,k,m) + grdvp(i,j,k)*grdvp(i,j,m)
             t4(i,j,k,m) =  t4(i,j,k,m) + gridt(i,j,k)*gridt(i,j,m)
             q4(i,j,k,m) =  q4(i,j,k,m) + gridq(i,j,k)*gridq(i,j,m)
             qi4(i,j,k,m)=  qi4(i,j,k,m)+ gridqi(i,j,k)*gridqi(i,j,m)
             ql4(i,j,k,m)=  ql4(i,j,k,m)+ gridql(i,j,k)*gridql(i,j,m)
             qr4(i,j,k,m)=  qr4(i,j,k,m)+ gridqr(i,j,k)*gridqr(i,j,m)
             qs4(i,j,k,m)=  qs4(i,j,k,m)+ gridqs(i,j,k)*gridqs(i,j,m)
            oz4(i,j,k,m) = oz4(i,j,k,m) + grdoz(i,j,k)*grdoz(i,j,m)
            cw4(i,j,k,m) = cw4(i,j,k,m) +  grdc(i,j,k)* grdc(i,j,m)
          end do
          end do
        end do
    end do
  end do !end do cases on mype (n)
  close(filunit)

  sf4=sf4*r_norm
  vp4=vp4*r_norm
   t4= t4*r_norm
   q4= q4*r_norm
   qi4=qi4*r_norm
   ql4=ql4*r_norm
   qr4=qr4*r_norm
   qs4=qs4*r_norm
  oz4=oz4*r_norm
  cw4=cw4*r_norm


! Need to convert full subdomain corrleation matrices into arrays
! That contain zonal mean

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(sf4(1,1,k,n),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
              sfvc(i,k,n) = sfvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  workgrd=0.
  work1=0.
  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(vp4(1,1,k,n),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
              vpvc(i,k,n) = vpvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(t4(1,1,k,n),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
              tvc(i,k,n) = tvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(q4(1,1,k,n),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
              qvc(i,k,n) = qvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(qi4(1,1,k,n),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
              qivc(i,k,n) = qivc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(ql4(1,1,k,n),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
              qlvc(i,k,n) = qlvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(qr4(1,1,k,n),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
              qrvc(i,k,n) = qrvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(qs4(1,1,k,n),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
              qsvc(i,k,n) = qsvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(oz4(1,1,k,n),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
              ozvc(i,k,n) = ozvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(cw4(1,1,k,n),ijn(mm1),mpi_rtype,&
                       work1,ijn,displs_g,mpi_rtype,&
                       mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
              cvc(i,k,n) = cvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if(.not. anaclw) then !! ML_ Need to revisit this logic (cvc(i,k,m))
    cvc = 0.0
    do k=1,nsig
      do i=1,nlat
        cvc(i,k,k)=1.0
      end do
    end do
  endif


  if(mype==mype_work) then 
    do i=1,nlat
      do k=1,nsig
        diag(k,1)=sqrt(sfvc(i,k,k))
        diag(k,2)=sqrt(vpvc(i,k,k))
        diag(k,3)=sqrt(tvc(i,k,k))
        diag(k,4)=sqrt(max(r_small,qvc(i,k,k)))
        diag(k,5)=sqrt(max(r_small,qivc(i,k,k)))
        diag(k,6)=sqrt(max(r_small,qlvc(i,k,k)))
        diag(k,7)=sqrt(max(r_small,qrvc(i,k,k)))
        diag(k,8)=sqrt(max(r_small,qsvc(i,k,k)))
        diag(k,9)=sqrt(cvc(i,k,k))
        diag(k,10)=sqrt(ozvc(i,k,k))
      end do
      do m=1,nsig
        do k=1,nsig
          sfvc(i,k,m)=sfvc(i,k,m)/(diag(k,1)*diag(m,1))
          vpvc(i,k,m)=vpvc(i,k,m)/(diag(k,2)*diag(m,2))
          tvc(i,k,m)=tvc(i,k,m)/(diag(k,3)*diag(m,3))
          qvc(i,k,m)=qvc(i,k,m)/(diag(k,4)*diag(m,4))
          qivc(i,k,m)=qivc(i,k,m)/(diag(k,5)*diag(m,5))
          qlvc(i,k,m)=qlvc(i,k,m)/(diag(k,6)*diag(m,6))
          qrvc(i,k,m)=qrvc(i,k,m)/(diag(k,7)*diag(m,7))
          qsvc(i,k,m)=qsvc(i,k,m)/(diag(k,8)*diag(m,8))
          cvc(i,k,m)=cvc(i,k,m)/(diag(k,9)*diag(m,9))
          ozvc(i,k,m)=ozvc(i,k,m)/(diag(k,10)*diag(m,10))
        end do
       end do
    end do !end do over lat
    call smoothvsc(sfvc,vpvc,tvc,qvc,qivc,qlvc,qrvc,qsvc,cvc,ozvc)

!   Make sure that vertical scales for cloud water are real values, else set to rh
    do k=1,nsig
      do i=1,nlat
        cvln(i,k)=max(min(10.0,cvln(i,k)),0.1)
      end do
    end do

!   The vert. length scale arrays get loaded in smoothvsc, now smooth
!   them

    call smoothlat(sfvln,nsig,smoothdeg)
    call smoothlat(vpvln,nsig,smoothdeg)
    call smoothlat(tvln,nsig,smoothdeg)
    call smoothlat(qvln,nsig,smoothdeg)
    call smoothlat(qivln,nsig,smoothdeg)
    call smoothlat(qlvln,nsig,smoothdeg)
    call smoothlat(qrvln,nsig,smoothdeg)
    call smoothlat(qsvln,nsig,smoothdeg)
    call smoothlat(cvln,nsig,smoothdeg)
    call smoothlat(ozvln,nsig,smoothdeg)
  endif  ! end if mype_work 

  call mpi_bcast(sfvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(vpvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(tvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qivln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qlvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qrvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qsvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(ozvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(cvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)

  if(lsmver .and. mype==0)then
    call smver(sfvln,vsmc,nlat,nsig)
    call smver(vpvln,vsmc,nlat,nsig)
    call smver(tvln,vsmc,nlat,nsig)
    call smver(qvln,vsmc,nlat,nsig)
    call smver(qivln,vsmc,nlat,nsig)
    call smver(qlvln,vsmc,nlat,nsig)
    call smver(qrvln,vsmc,nlat,nsig)
    call smver(qsvln,vsmc,nlat,nsig)
    call smver(cvln,vsmc,nlat,nsig)
    call smver(ozvln,vsmc,nlat,nsig)
  endif

  deallocate(workgrd,work1)

  return
end subroutine vertlength

subroutine smoothvsc(sfvc,vpvc,tvc,qvc,qivc,qlvc,qrvc,qsvc,cvc,ozvc)
  use type_kinds,only: fp_kind,double
  use postmod, only: ndeg,nasm
  use variables,only: nlat,nsig,sfvln,vpvln,tvln,qvln,qivln,qlvln,qrvln,qsvln,cvln,ozvln
  implicit none

  real(double),dimension(nlat,nsig,nsig),intent(in):: sfvc,vpvc,tvc,qvc,qivc,qlvc,qrvc,qsvc,cvc,ozvc
!  real(fp_kind),dimension(nlat,nsig*6):: vsc_out
  real(fp_kind),dimension(nlat,nsig*10):: vsc_out

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

!    do ll=1,6
    do ll=1,10
! ll=1 z
! ll=2 d
! ll=3 t
! ll=4 q
! ll=5 clw
! ll=6 oz
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
              sum(i)=sum(i)+weights(l+k)*(qivc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.6) then
              sum(i)=sum(i)+weights(l+k)*(qlvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.7) then
              sum(i)=sum(i)+weights(l+k)*(qrvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.8) then
              sum(i)=sum(i)+weights(l+k)*(qsvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.9) then
              sum(i)=sum(i)+weights(l+k)*(cvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.10) then
              sum(i)=sum(i)+weights(l+k)*(ozvc(j,l,l+k)-table(l+k,i))**2.
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
      qivln(i,k)=vsc_out(i,4*nsig+k)
      qlvln(i,k)=vsc_out(i,5*nsig+k)
      qrvln(i,k)=vsc_out(i,6*nsig+k)
      qsvln(i,k)=vsc_out(i,7*nsig+k)
      cvln(i,k)=vsc_out(i,8*nsig+k)
      ozvln(i,k)=vsc_out(i,9*nsig+k)
    end do
  end do

  return
end subroutine smoothvsc
