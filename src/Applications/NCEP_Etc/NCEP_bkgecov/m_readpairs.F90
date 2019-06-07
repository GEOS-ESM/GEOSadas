subroutine m_readpairs(tskcases,npes,mype,numcases,mycases)

  use type_kinds, only: fp_kind

  use m_GsiGrided, only : GsiGrided
  use m_GsiGrided, only : GsiGrided_init
  use m_GsiGrided, only : GsiGrided_read
  use m_GsiGrided, only : GsiGrided_clean

  use variables,only: nlat,nlon,nsig,sigi,sigl,ak5,bk5
  use variables,only: vsmth,vsmc
  use variables,only: na,nb,filename,db_prec
  use variables,only: bbt,bbs,bbv,bbp
  use variables,only: bboz,bbq,bbqi,bbql,bbqr,bbqs
  use variables,only: filunit
  use variables,only: grdsf,grdvp,gridt,grdc,gridq,grdrh,gridp,grdoz
  use variables,only: gridqi,gridql,gridqr,gridqs
  use variables,only: nGlat,nGlon,glats,glons
  use variables,only: rearth
  use variables,only: biasrm,bbiasz,bbiasd,bbiast,bcorrz,bcorrd,bcorrt,bbiasp,bcorrp
  use variables,only: hydromet
  use variables,only: vcoeffs,hcoeffs

  use compact_diffs,only: init_compact_diffs
  use compact_diffs,only: create_cdiff_coefs
  use compact_diffs,only: inisph
  use compact_diffs,only: destroy_cdiff_coefs

#ifndef ibm_sp
  use m_mpif
#endif

  implicit none

#ifdef ibm_sp
  include 'mpif.h'
#endif

  integer, intent(in) :: tskcases,npes,mype,numcases 
  integer, intent(out) :: mycases

! local variables
  integer ierror,mpi_rtype
  integer i,j,k,total,n,nymd1,nhms1,nymd2,nhms2,nl
  real(fp_kind),dimension(nsig) :: vcoef,vsmall
  type(GsiGrided) :: ob1,ob2

  real(fp_kind) ps0,det

  real(fp_kind),allocatable,dimension(:,:,:,:):: bfactz,bfactd,bfactt
  real(fp_kind),allocatable,dimension(:,:,:):: bfactp

  logical ice
  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  mycases=0
  ps0=1./101.324
  bbp=0.
  bbt=0.
  bbs=0.
  bbv=0.
  bboz=0.
  bbq=0.
  if (hydromet) then
     bbqi=0.
     bbql=0.
     bbqr=0.
     bbqs=0.
  endif


   call init_compact_diffs(nGlat,nGlon)
   call create_cdiff_coefs(nGlon,glons)
!  print*,'created compact coefficients '
   call inisph(rearth,glats(2),nGlon,nGlat-2)
!  print*,'Initialized spherical stuff'

  if(biasrm) then 
    allocate(bfactz(nlat,nlon,nsig,4))
    allocate(bfactd(nlat,nlon,nsig,4))
    allocate(bfactt(nlat,nlon,nsig,4))
    allocate(bfactp(nlat,nlon,4))
    bfactz=0.
    bfactd=0.
    bfactt=0.
    bfactp=0.
  endif


  do n=1,tskcases

    total=npes*(n-1)+mype+1

    if(total.le.numcases)then
      mycases=mycases+1


      if(mype==total) print*,'reading file ', na(total)
      call GsiGrided_init(ob1)
      call GsiGrided_read(na(total),nymd1,nhms1,ob1,mype)

      ak5=ob1%ak
      bk5=ob1%bk
      do k=1,nsig
        sigl(k)=0.5*((ak5(k+1)+ak5(k))*ps0+bk5(k+1)+bk5(k))
        sigi(k)=ak5(k)*ps0+bk5(k)
      end do
      sigi(nsig+1)=ak5(nsig+1)*ps0+bk5(nsig+1)

      gridt   = ob1%vt
      grdvp   = ob1%vp
      grdsf   = ob1%sf
      grdc    = ob1%cw
      gridp   = ob1%ps
      grdoz   = ob1%oz

      ice=.true.
      gridq  = ob1%q
      if (hydromet) then
         gridqi = ob1%qi
         gridql = ob1%ql
         gridqr = ob1%qr
         gridqs = ob1%qs
      endif
      call genqsat(ob1%vt,gridq,nlat,nlon,&
                   ob1%ps,ice,sigl,ak5,bk5)

      do k=1,nsig
      do j=1,nlon
      do i=1,nlat
        if( abs(gridq(i,j,k)).gt.0. ) then
          ob1%q(i,j,k)=ob1%q(i,j,k)/gridq(i,j,k)
        else
          ob1%q(i,j,k)=0.0
        endif
      end do
      end do
      end do

      call GsiGrided_init(ob2)
      call GsiGrided_read(nb(total),nymd2,nhms2,ob2,mype)
      if(nymd1/=nymd2 .or. nhms1/=nhms2 ) then
        call GsiGrided_clean(ob2)
        call mpi_finalize(ierror)
        stop
      end if

      ice=.true.
      grdrh = ob2%q
      call genqsat(ob2%vt,grdrh,nlat,nlon,&
                   ob2%ps,ice,sigl,ak5,bk5)

      do k=1,nsig
      do j=1,nlon
      do i=1,nlat
        if( abs(grdrh (i,j,k)).gt.0.) then
          ob2%q(i,j,k)=ob2%q(i,j,k)/grdrh(i,j,k)
        else
          ob2%q(i,j,k)=0.0
        endif
      end do
      end do
      end do

      grdrh=0.5*(ob1%q +ob2%q)

      do k=1,nsig
      do j=1,nlon
      do i=1,nlat
         if(ob1%q(i,j,k) <-0.25) ob1%q(i,j,k)=-0.25
         if(ob1%q(i,j,k) > 1.25) ob1%q(i,j,k)= 1.25
         if(ob2%q(i,j,k) <-0.25) ob2%q(i,j,k)=-0.25
         if(ob2%q(i,j,k) > 1.25) ob2%q(i,j,k)= 1.25
      end do
      end do
      end do

      grdc    = ob1%cw - ob2%cw
      grdoz   = ob1%oz - ob2%oz
      gridq   = ob1%q  - ob2%q
      if (hydromet) then
         gridqi  = ob1%qi - ob2%qi
         gridql  = ob1%ql - ob2%ql
         gridqr  = ob1%qr - ob2%qr
         gridqs  = ob1%qs - ob2%qs
      endif
      grdrh   = 0.5*(ob1%q +ob2%q)

      if( biasrm ) then 
        do k=1,nsig
          do j=1,nlon
            do i=1,nlat
              bfactz(i,j,k,1)=bfactz(i,j,k,1)+(ob1%sf(i,j,k)*ob2%sf(i,j,k))
              bfactz(i,j,k,2)=bfactz(i,j,k,2)+(ob2%sf(i,j,k)*ob2%sf(i,j,k))
              bfactz(i,j,k,3)=bfactz(i,j,k,3)+ob1%sf(i,j,k)
              bfactz(i,j,k,4)=bfactz(i,j,k,4)+ob2%sf(i,j,k)

              bfactd(i,j,k,1)=bfactd(i,j,k,1)+(ob1%vp(i,j,k)*ob2%vp(i,j,k))
              bfactd(i,j,k,2)=bfactd(i,j,k,2)+(ob2%vp(i,j,k)*ob2%vp(i,j,k))
              bfactd(i,j,k,3)=bfactd(i,j,k,3)+ob1%vp(i,j,k)
              bfactd(i,j,k,4)=bfactd(i,j,k,4)+ob2%vp(i,j,k)

              bfactt(i,j,k,1)=bfactt(i,j,k,1)+(ob1%vt(i,j,k)*ob2%vt(i,j,k))
              bfactt(i,j,k,2)=bfactt(i,j,k,2)+(ob2%vt(i,j,k)*ob2%vt(i,j,k))
              bfactt(i,j,k,3)=bfactt(i,j,k,3)+ob1%vt(i,j,k)
              bfactt(i,j,k,4)=bfactt(i,j,k,4)+ob2%vt(i,j,k)
            enddo
          enddo 
        enddo

        do j=1,nlon
          do i=1,nlat
             bfactp(i,j,1)=bfactp(i,j,1)+(ob1%ps(i,j)*ob2%ps(i,j))
             bfactp(i,j,2)=bfactp(i,j,2)+(ob2%ps(i,j)*ob2%ps(i,j))
             bfactp(i,j,3)=bfactp(i,j,3)+ob1%ps(i,j)
             bfactp(i,j,4)=bfactp(i,j,4)+ob2%ps(i,j)
          enddo
        enddo
      else
        gridt   = ob1%vt - ob2%vt
        grdvp   = ob1%vp - ob2%vp
        grdsf   = ob1%sf - ob2%sf
        gridp   = ob1%ps - ob2%ps
      
        filunit = 1000*(mype+1)+n
        open(filunit,form='unformatted')
        rewind(filunit)

        if (hydromet) then
           write(filunit)grdsf,grdvp,gridt,gridp,gridq,gridqi,gridql,gridqr,gridqs,grdrh,grdoz,grdc
        else
           write(filunit)grdsf,grdvp,gridt,gridp,gridq,grdrh,grdoz,grdc
        endif

        close(filunit)

        bbp(:,:  )=bbp(:,:  )+gridp(:,:  )
        bbt(:,:,:)=bbt(:,:,:)+gridt(:,:,:)
        bbv(:,:,:)=bbv(:,:,:)+grdvp(:,:,:)
        bbs(:,:,:)=bbs(:,:,:)+grdsf(:,:,:)
        bbq(:,:,:)=bbq(:,:,:)+gridq(:,:,:)
        if (hydromet) then
           bbqi(:,:,:)=bbqi(:,:,:)+gridqi(:,:,:)
           bbql(:,:,:)=bbql(:,:,:)+gridql(:,:,:)
           bbqr(:,:,:)=bbqr(:,:,:)+gridqr(:,:,:)
           bbqs(:,:,:)=bbqs(:,:,:)+gridqs(:,:,:)
        endif
        bboz(:,:,:)=bboz(:,:,:)+grdoz(:,:,:)


      endif 

      call GsiGrided_clean(ob1)
      call GsiGrided_clean(ob2)

    end if
  end do
  if(mype==0) print*, 'Pass bias removal'


  nl = nlat*nlon
  if (biasrm) then 

    call mpi_allreduce((bfactz),bfactz,4*nl*nsig,mpi_rtype,mpi_sum, &
                        mpi_comm_world,ierror)
    call mpi_allreduce((bfactd),bfactd,4*nl*nsig,mpi_rtype,mpi_sum, &
                        mpi_comm_world,ierror)
    call mpi_allreduce((bfactt),bfactt,4*nl*nsig,mpi_rtype,mpi_sum, &
                        mpi_comm_world,ierror)
    call mpi_allreduce((bfactp),bfactp,4*nl     ,mpi_rtype,mpi_sum, &
                        mpi_comm_world,ierror)

    allocate(bcorrz(nlat,nlon,nsig))
    allocate(bcorrd(nlat,nlon,nsig))
    allocate(bcorrt(nlat,nlon,nsig))
    allocate(bcorrp(nlat,nlon))
    allocate(bbiasz(nlat,nlon,nsig))
    allocate(bbiasd(nlat,nlon,nsig))
    allocate(bbiast(nlat,nlon,nsig))
    allocate(bbiasp(nlat,nlon))

    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          do n=1,4
            bfactz(i,j,k,n) = bfactz(i,j,k,n)/float(numcases)
            bfactd(i,j,k,n) = bfactd(i,j,k,n)/float(numcases)
            bfactt(i,j,k,n) = bfactt(i,j,k,n)/float(numcases)
          enddo     
          if(abs(bfactz(i,j,k,2)-bfactz(i,j,k,4)**2) > 1.e-26)then
             bcorrz(i,j,k)=(bfactz(i,j,k,1)-bfactz(i,j,k,3)*bfactz(i,j,k,4)) &
                            /(bfactz(i,j,k,2)-bfactz(i,j,k,4)**2.)
          else
             bcorrz(i,j,k)= 1. 
          end if
          bbiasz(i,j,k)=bfactz(i,j,k,3)-bcorrz(i,j,k)*bfactz(i,j,k,4)

          if(abs(bfactd(i,j,k,2)-bfactd(i,j,k,4)**2) > 1.e-26)then
            bcorrd(i,j,k)=(bfactd(i,j,k,1)-bfactd(i,j,k,3)*bfactd(i,j,k,4)) &
                          /(bfactd(i,j,k,2)-bfactd(i,j,k,4)**2.)
          else
            bcorrd(i,j,k)= 1. 
          end if
          bbiasd(i,j,k)=bfactd(i,j,k,3)-bcorrd(i,j,k)*bfactd(i,j,k,4)

          if(abs(bfactt(i,j,k,2)-bfactt(i,j,k,4)**2) > 1.e-26)then
            bcorrt(i,j,k)=(bfactt(i,j,k,1)-bfactt(i,j,k,3)*bfactt(i,j,k,4)) &
                          /(bfactt(i,j,k,2)-bfactt(i,j,k,4)**2.)
          else
            bcorrt(i,j,k)= 1. 
          end if
          bbiast(i,j,k)=bfactt(i,j,k,3)-bcorrt(i,j,k)*bfactt(i,j,k,4)
        enddo
      enddo
    enddo
    do j=1,nlon
      do i=1,nlat
        do n=1,4
          bfactp(i,j,n) = bfactp(i,j,n)/float(numcases)
        enddo
        if(abs(bfactp(i,j,2)-bfactp(i,j,4)**2) > 1.e-26)then
          bcorrp(i,j)=(bfactp(i,j,1)-bfactp(i,j,3)*bfactp(i,j,4)) &
                      /(bfactp(i,j,2)-bfactp(i,j,4)**2.)
        else
          bcorrp(i,j)= 1. 
        end if
        bbiasp(i,j)=bfactp(i,j,3)-bcorrp(i,j)*bfactp(i,j,4)
      enddo
    enddo 

    deallocate(bfactz,bfactd,bfactt,bfactp)

    mycases = 0 
    do n=1,tskcases
       total=npes*(n-1)+mype+1
       if(total.le.numcases)then
          mycases=mycases+1
          filunit = 1000*(mype+1)+n
          open(filunit,form='unformatted')
          rewind(filunit)

          call GsiGrided_init(ob1)
          call GsiGrided_read(na(total),nymd1,nhms1,ob1,mype)
          call GsiGrided_init(ob2)
          call GsiGrided_read(nb(total),nymd1,nhms1,ob2,mype)

          do k=1,nsig
            do j=1,nlon
              do i=1,nlat
                grdsf(i,j,k)=ob1%sf(i,j,k)-bcorrz(i,j,k)*ob2%sf(i,j,k)-bbiasz(i,j,k)
                grdvp(i,j,k)=ob1%vp(i,j,k)-bcorrd(i,j,k)*ob2%vp(i,j,k)-bbiasd(i,j,k)
                gridt(i,j,k)=ob1%vt(i,j,k)-bcorrt(i,j,k)*ob2%vt(i,j,k)-bbiast(i,j,k)
              enddo
            enddo
          enddo
          do j=1,nlon
            do i=1,nlat
              gridp(i,j)=ob1%ps(i,j)-bcorrp(i,j)*ob2%ps(i,j)-bbiasp(i,j)
            enddo 
          enddo


          if (hydromet) then
             write(filunit)grdsf,grdvp,gridt,gridp,gridq,gridqi,gridql,gridqr,gridqs,grdrh,grdoz,grdc
          else
             write(filunit)grdsf,grdvp,gridt,gridp,gridq,grdrh,grdoz,grdc
          endif
          close(filunit)
          call GsiGrided_clean(ob1)
          call GsiGrided_clean(ob2)
       endif
    enddo
    deallocate(bcorrz,bcorrd,bcorrt,bcorrp)
    deallocate(bbiasz,bbiasd,bbiast,bbiasp)
  else 
    call mpi_allreduce((bbp),bbp,nl,mpi_rtype,mpi_sum, &
                        mpi_comm_world,ierror)
    call mpi_allreduce((bbt),bbt,nl*nsig,mpi_rtype,mpi_sum, &
                        mpi_comm_world,ierror)
    call mpi_allreduce((bbs),bbs,nl*nsig,mpi_rtype,mpi_sum, &
                        mpi_comm_world,ierror)
    call mpi_allreduce((bbv),bbv,nl*nsig,mpi_rtype,mpi_sum, &
                        mpi_comm_world,ierror)
    call mpi_allreduce((bbq),bbq,nl*nsig,mpi_rtype,mpi_sum, &
                        mpi_comm_world,ierror)
    if (hydromet) then
       call mpi_allreduce((bbqi),bbqi,nl*nsig,mpi_rtype,mpi_sum, &
                           mpi_comm_world,ierror)
       call mpi_allreduce((bbql),bbql,nl*nsig,mpi_rtype,mpi_sum, &
                           mpi_comm_world,ierror)
       call mpi_allreduce((bbqr),bbqr,nl*nsig,mpi_rtype,mpi_sum, &
                           mpi_comm_world,ierror)
       call mpi_allreduce((bbqs),bbqs,nl*nsig,mpi_rtype,mpi_sum, &
                           mpi_comm_world,ierror)
    endif
    call mpi_allreduce((bboz),bboz,nl*nsig,mpi_rtype,mpi_sum, &
                        mpi_comm_world,ierror)

    bbp = bbp/float(numcases)
    bbt = bbt/float(numcases)
    bbs = bbs/float(numcases)
    bbv = bbv/float(numcases)
    bbq = bbq/float(numcases)
    if (hydromet) then
       bbqi = bbqi/float(numcases)
       bbql = bbql/float(numcases)
       bbqr = bbqr/float(numcases)
       bbqs = bbqs/float(numcases)
    endif
    bboz = bboz/float(numcases)
  endif
  if(mype==0) print*, 'Pass bias removal 2'

  call destroy_cdiff_coefs
! if(mype==0)then
!   print*,'ak5 bk5 sigi sigl'
!   do k = 1,nsig
!     print*,'k= ',k,ak5(k),bk5(k),sigi(k),sigl(k)
!   enddo
!   print*,'k= ',k,ak5(k),bk5(k),sigi(k) ! print k+1
! endif
  call mpi_bcast(ak5, nsig+1,mpi_rtype,0,mpi_comm_world,ierror)
  call mpi_bcast(bk5, nsig+1,mpi_rtype,0,mpi_comm_world,ierror)
  call mpi_bcast(sigi,nsig+1,mpi_rtype,0,mpi_comm_world,ierror)
  call mpi_bcast(sigl,nsig  ,mpi_rtype,0,mpi_comm_world,ierror)


! Prepare horizontal smoothing coefficients
  vcoef = hcoeffs(1:nsig)
  write(12,*) 'Coeffs for Horizontal Smoothing'
  do k=1,nsig
     write(12,*)k,sigl(k),vcoef
  end do

  call getvsm_(nsig,mype,vcoef,vsmth)
  call m3minv(vsmth,nsig,det)

  if(mype==0)then
    do k=1,nsig
      vsmall(k)=0
      do j=1,nsig
        vsmall(k)=vsmall(k)+vsmth(j,k)
      enddo
    enddo
  endif

! Prepare vertical smoothing coefficients
  vcoef = vcoeffs(1:nsig)
  write(14,*) 'Coeffs for Vertical Smoothing'
  do k=1,nsig
     write(42,*)k,sigl(k),vcoef
  end do

  call getvsm_(nsig,mype,vcoef,vsmc)
  call m3minv(vsmc,nsig,det)
  if(mype==0) print*, 'Done readpairs'

  return
end subroutine m_readpairs

subroutine getvsm_(nsig,mype,vcoef,vsmth)
  use type_kinds,only : fp_kind,single,double
  use variables, only : sigi,sigl 
  implicit none
  integer,intent(in) :: nsig
  integer,intent(in) :: mype
  real(fp_kind),dimension(nsig),intent(in) :: vcoef 
  real(fp_kind),dimension(nsig,nsig),intent(out) :: vsmth

  integer k
  real(fp_kind) :: vf
   
  vsmth=0.0_fp_kind

! if(mype==0)then
!   print*,'sigi sigl'
!   do k = 1,nsig
!      print*,'k= ',k,sigi(k),sigl(k)
!   enddo
!   print*,'k= ',k,sigi(k)
! endif
  do k=1,nsig
    vsmth(k,k)=1.
    vf=vcoef(k)*(sigi(k+1)-sigi(k))
    if(k > 1)then
      if(abs(sigl(k)-sigl(k-1))<1.e-10) then
        print *, 'something is fishy: 1', sigl(k)-sigl(k-1)
      endif
      vsmth(k-1,k)=-vf/(sigl(k)-sigl(k-1))
      vsmth(k,k)=vsmth(k,k)-vsmth(k-1,k)
    endif
    if(k < nsig)then
      if(abs(sigl(k+1)-sigl(k))<1.e-10) then
        print *, 'something is fishy: 2', sigl(k+1)-sigl(k)
      endif
      vsmth(k+1,k)=-vf/(sigl(k+1)-sigl(k))
      vsmth(k,k)=vsmth(k,k)-vsmth(k+1,k)
    endif
  enddo

end subroutine getvsm_



  SUBROUTINE M3MINV(A,N,D)
    use type_kinds, only: fp_kind
    REAL(fp_kind), DIMENSION(1):: A
    INTEGER, DIMENSION(n*(n+1)/2):: L,M
    REAL(fp_kind) BIGA,HOLD,norm
      norm = -9999.
!  find largest diagonal element
      do i=1,n
      ix=i+(i-1)*n
      if(a(ix) > norm)norm=a(ix)
      end do
      do i=1,n*n
        a(i)=a(i)/norm
      end do
!C
!C        SEARCH FOR LARGEST ELEMENT
!C
      D=1.E0
      NK=-N
      DO 80 K=1,N
      NK=NK+N
      L(K)=K
      M(K)=K
      KK=NK+K
      BIGA=A(KK)
      DO 20 J=K,N
      IZ=N*(J-1)
      DO 20 I=K,N
      IJ=IZ+I
   10 IF( ABS(BIGA)- ABS(A(IJ))) 15,20,20
   15 BIGA=A(IJ)
      L(K)=I
      M(K)=J
   20 CONTINUE
!C
!C        INTERCHANGE ROWS
!C
      J=L(K)
      IF(J-K) 35,35,25
   25 KI=K-N
      DO 30 I=1,N
      KI=KI+N
      HOLD=-A(KI)
      JI=KI-K+J
      A(KI)=A(JI)
   30 A(JI) =HOLD
!C
!C        INTERCHANGE COLUMNS
!C
   35 I=M(K)
      IF(I-K) 45,45,38
   38 JP=N*(I-1)
      DO 40 J=1,N
      JK=NK+J
      JI=JP+J
      HOLD=-A(JK)
      A(JK)=A(JI)
   40 A(JI) =HOLD
!C
!C        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS
!C        CONTAINED IN BIGA)
!C
   45 IF(BIGA) 48,46,48
   46 D=0.E0
      RETURN
   48 DO 55 I=1,N
      IF(I-K) 50,55,50
   50 IK=NK+I
      A(IK)=A(IK)/(-BIGA)
   55 CONTINUE
!C
!C
!C        REDUCE MATRIX
!C
      DO 65 I=1,N
      IK=NK+I
      HOLD=A(IK)
      IJ=I-N
      DO 65 J=1,N
      IJ=IJ+N
      IF(I-K) 60,65,60
   60 IF(J-K) 62,65,62
   62 KJ=IJ-I+K
      A(IJ)=HOLD*A(KJ)+A(IJ)
   65 CONTINUE
!C
!C        DIVIDE ROW BY PIVOT
!C
      KJ=K-N
      DO 75 J=1,N
      KJ=KJ+N
      IF(J-K) 70,75,70
   70 A(KJ)=A(KJ)/BIGA
   75 CONTINUE
!C
!C        PRODUCT OF PIVOTS
!C
!   determinant too big not calculated properly 
!      D=D*BIGA
!C
!C        REPLACE PIVOT BY RECIPROCAL
!C
      A(KK)=1.E0/BIGA
   80 CONTINUE
!C
!C        FINAL ROW AND COLUMN INTERCHANGE
!C
      K=N
  100 K=(K-1)
      IF(K) 150,150,105
  105 I=L(K)
      IF(I-K) 120,120,108
  108 JQ=N*(K-1)
      JR=N*(I-1)
      DO 110 J=1,N
      JK=JQ+J
      HOLD=A(JK)
      JI=JR+J
      A(JK)=-A(JI)
  110 A(JI) =HOLD
  120 J=M(K)
      IF(J-K) 100,100,125
  125 KI=K-N
      DO 130 I=1,N
      KI=KI+N
      HOLD=A(KI)
      JI=KI-K+J
      A(KI)=-A(JI)
  130 A(JI) =HOLD
      GO TO 100

  150 continue
      do i=1,n*n
        a(i)=a(i)/norm
      end do
      return
  END SUBROUTINE M3MINV
