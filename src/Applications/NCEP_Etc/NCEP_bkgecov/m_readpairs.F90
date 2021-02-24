subroutine m_readpairs(npe,mype,numcases,mycases)

  use type_kinds, only: fp_kind,single
  use m_die, only: die

  use m_GsiGrided, only : GsiGrided
  use m_GsiGrided, only : GsiGrided_init
  use m_GsiGrided, only : GsiGrided_read
  use m_GsiGrided, only : GsiGrided_clean

  use variables,only: nlat,nlon,nsig,sigi,sigl,ak5,bk5,iglobal
  use variables,only: lat1,lon1,ijn,displs_g,ltosi,ltosj
  use variables,only: vsmth,vsmc
  use variables,only: na,nb,filename,db_prec
  use variables,only: bbt,bbs,bbv,bbp
  use variables,only: bboz,bbq,bbqi,bbql,bbqr,bbqs
  use variables,only: filunit
  use variables,only: nGlat,nGlon,glats,glons
  use variables,only: rearth
  use variables, only: create_bias_g
  use variables, only: hcoeffs,vcoeffs
  use variables, only: n2d,n3d
  use variables, only: hydromet
  use variables, only: nreaders
  use variables, only: readperts
  use variables, only: rhbounds

  use compact_diffs,only: init_compact_diffs
  use compact_diffs,only: create_cdiff_coefs
  use compact_diffs,only: inisph
  use compact_diffs,only: destroy_cdiff_coefs
  use comm_mod, only: levs_id,nvar_id,grid2sub,nsig1o,spec_send,&
        disp_spec

! HISTORY:
!   24Apr2020 Todling - rearranged this code considerably. Using
!                       4 or 8 readers allows the code to process
!                       a sample of 32 members in 10 min. The ideal
!                       mode of exercising this software is to now 
!                       let it work directly from the NMC-perturbation,
!                       instead of having to read pairs of forecasts 
!                       and calculating the perturbations internally.
!                       With 16 readers a 370 sample-case of 721x1152
!                       resolution can now be process in less the 25 min,
!                       this is to be contrasted with the 7.5 hours it 
!                       took to do the same amount of work.
!                       The numbers about apply to 72 levels, jcap=512,
!                       25 vertical; all these parameters contribute to
!                       the time it takes to run this software.

#ifndef ibm_sp
  use m_mpif
#endif

  implicit none

#ifdef ibm_sp
  include 'mpif.h'
#endif

  integer, intent(in) :: npe,mype,numcases 
  integer, intent(out) :: mycases

! local variables
  character(len=*), parameter :: myname="m_readpairs"
  integer ierror,mpi_rtype,iret
  integer :: mm1,nsig1,ii,ns,nskip
  real(fp_kind),dimension(nsig) :: vcoef,vsmall
  type(GsiGrided) :: ob1

  real(fp_kind) ps0,det

  real(single), allocatable,dimension(:,:)   :: z4all
  integer,allocatable::nprocs(:)

  logical ice,itest

  nsig1 = nsig+1
  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  if (readperts) then
    if(mype==0) write(6,*) 'READING NMC-like DIFFERENCE FIELDS'
  else
    if(mype==0) write(6,*) 'READING FORECAST PAIRS'
  endif

  mycases=numcases
  ps0=1./101.324

  call create_bias_g(lat1,lon1,nsig)

  mm1=mype+1
  filunit = 1000*(mype+1)+1
  open(filunit,form='unformatted',action='write')
  rewind(filunit)

  call init_()

  allocate(z4all(iglobal,n3d*nsig+n2d))

  if(nreaders>npe) then
    if(readperts) then
       nreaders = 1
    else
       nreaders = 2
    endif
  endif
  if(mod(npe,nreaders)/=0) then
    call die(myname,': nreaders should divide npe whole, aborting',99)
  endif
  nskip=2
  if(readperts) nskip=1
  allocate(nprocs(nreaders))
  do ii=1,nreaders
     nprocs(ii)=(ii-1)*npe/nreaders
  enddo
  ii=0;ns=1
  do while (ns<=numcases)
     do ii=1,nreaders,nskip
        if(ns<=numcases) then
          if (readperts) then
            call read_(ns,nprocs(ii),-1)
          else
            call read_(ns,nprocs(ii),nprocs(ii+1))
          endif
        endif
        ns=ns+1
     enddo
     call mpi_barrier(mpi_comm_world,iret) ! shouldn''t need this barrier
     ns=ns-nreaders/nskip
     do ii=1,nreaders,nskip
        if(ns<=numcases) then
           if (readperts) then
             call scatter_(ns,nprocs(ii),-1)
           else
             call scatter_(ns,nprocs(ii),nprocs(ii+1))
           endif
        endif
        ns=ns+1
     enddo
     call mpi_barrier(mpi_comm_world,iret) ! shouldn''t need this barrier
  enddo
  deallocate(nprocs)

  deallocate(z4all)

  call mpi_barrier(mpi_comm_world,iret)
  close(filunit)

  call final_()

  return
contains

 subroutine init_
  call init_compact_diffs(nGlat,nGlon)
  call create_cdiff_coefs(nGlon,glons)
  call inisph(rearth,glats(2),nGlon,nGlat-2)
 end subroutine init_

 subroutine read_(n,proc1,proc2)
  integer, intent(in) :: n, proc1, proc2

  integer i,j,k,kk,ii,nymd1,nhms1,nymd2,nhms2,nl
  integer k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12

    if(mype/=proc1.and.mype/=proc2) return

    if (mype==proc1) then 
      if (readperts) then
        write(6,'(a,1x,i5,1x,a)') 'reading file on PE= ', mype, trim(filename(n))
      else
        write(6,'(i5)') numcases
        write(6,'(2(a,1x,i5),1x,2a)') 'PE= ', mype, ' reading case ', n,' file: ',trim(filename(na(n)))
      endif
      call GsiGrided_init(ob1)
      if (readperts) then
        call GsiGrided_read(n,nymd1,nhms1,ob1,mype)
      else
        call GsiGrided_read(na(n),nymd1,nhms1,ob1,mype)
      endif
      ak5=ob1%ak
      bk5=ob1%bk
      do k=1,nsig
        sigl(k)=0.5*((ak5(k+1)+ak5(k))*ps0+bk5(k+1)+bk5(k))
        sigi(k)=ak5(k)*ps0+bk5(k)
      end do
      sigi(nsig+1)=ak5(nsig+1)*ps0+bk5(nsig+1)
    end if 
    if (mype==proc2) then 
!     write(6,'(a,1x,i5,1x,a)') 'reading file on PE= ', mype, trim(filename(nb(n)))
      write(6,'(2(a,1x,i5),1x,2a)') 'PE= ', mype, ' reading case ', n,' file: ',trim(filename(nb(n)))
      call GsiGrided_init(ob1)
      call GsiGrided_read(nb(n),nymd1,nhms1,ob1,mype)
    end if
      
!   call mpi_barrier(mpi_comm_world,iret)

    if (mype==proc1 .or. mype==proc2) then 
      do k=1,nsig
        k1=nsig
        k2=2*nsig
        k3=3*nsig
        k4=4*nsig
        k5=5*nsig
        k6=6*nsig
        k7=7*nsig
        k8=8*nsig
        k9=9*nsig
        k10=10*nsig
        k11=11*nsig
        ii =0
        do j=1,nlon
        do i=1,nlat
           ii = ii +1 
           z4all(ii,k)   =ob1%sf(i,j,k)
           z4all(ii,k1+k)=ob1%vp(i,j,k)
           z4all(ii,k2+k)=ob1%vt(i,j,k)
           z4all(ii,k3+k)=ob1%q(i,j,k)
           z4all(ii,k4+k)=ob1%qi(i,j,k)
           z4all(ii,k5+k)=ob1%ql(i,j,k)
           z4all(ii,k6+k)=ob1%qr(i,j,k)
           z4all(ii,k7+k)=ob1%qs(i,j,k)
           z4all(ii,k8+k)=ob1%oz(i,j,k)
           z4all(ii,k9+k)=ob1%cw(i,j,k)
           z4all(ii,k10+k)=ob1%rh(i,j,k)
           z4all(ii,k11+k)=ob1%mrh(i,j,k)
        end do
        end do
      end do
      k12=12*nsig
      ii=0
      do j=1,nlon
      do i=1,nlat
         ii = ii+1
         z4all(ii,k12+1)=ob1%ps(i,j)
      enddo 
      end do
      call GsiGrided_clean(ob1)
    endif 

 end subroutine read_

 subroutine scatter_(n,proc1,proc2)
  integer, intent(in) :: n, proc1, proc2

  integer i,j,k,nl

! local variables
  real(fp_kind),allocatable,dimension(:,:,:) :: sf1,sf2,vp1,vp2,t1,t2,q1,q2,     &
                                                qi1,qi2,ql1,ql2,qr1,qr2,qs1,qs2, & 
                                                rh1,rh2,oz1,oz2,cw1,cw2,mrh1,mrh2
  real(fp_kind),allocatable,dimension(:,:)   :: ps1,ps2
  real(single), allocatable,dimension(:,:)   :: z41,z42
  real(fp_kind),allocatable,dimension(:,:,:) :: grdq1,grdq2,rh

  allocate(sf1(lat1,lon1,nsig), &
           vp1(lat1,lon1,nsig), &
            t1(lat1,lon1,nsig), &
            q1(lat1,lon1,nsig), &
           qi1(lat1,lon1,nsig), &
           ql1(lat1,lon1,nsig), &
           qr1(lat1,lon1,nsig), &
           qs1(lat1,lon1,nsig), &
           rh1(lat1,lon1,nsig), &
          mrh1(lat1,lon1,nsig), &
           oz1(lat1,lon1,nsig), &
           cw1(lat1,lon1,nsig), &
           ps1(lat1,lon1)       )

  allocate(z41(iglobal,nsig1o))
  call mpi_scatterv(z4all,spec_send,disp_spec,mpi_rtype,&
                    z41,spec_send(mm1),mpi_rtype,proc1, &
                    mpi_comm_world,ierror)

  if (.not.readperts) then
    allocate(sf2(lat1,lon1,nsig), &
             vp2(lat1,lon1,nsig), &
              t2(lat1,lon1,nsig), &
              q2(lat1,lon1,nsig), &
             qi2(lat1,lon1,nsig), &
             ql2(lat1,lon1,nsig), &
             qr2(lat1,lon1,nsig), &
             qs2(lat1,lon1,nsig), &
             rh2(lat1,lon1,nsig), &
            mrh2(lat1,lon1,nsig), &
             oz2(lat1,lon1,nsig), &
             cw2(lat1,lon1,nsig), &
             ps2(lat1,lon1)       )

    allocate(z42(iglobal,nsig1o))
    call mpi_scatterv(z4all,spec_send,disp_spec,mpi_rtype,&
                      z42,spec_send(mm1),mpi_rtype,proc2, & 
                      mpi_comm_world,ierror)
   endif

    call mpi_bcast(sigl,nsig1,mpi_rtype,proc1,mpi_comm_world,ierror)
    call mpi_bcast(sigi,nsig1,mpi_rtype,proc1,mpi_comm_world,ierror)
    call mpi_bcast( ak5,nsig1,mpi_rtype,proc1,mpi_comm_world,ierror)
    call mpi_bcast( bk5,nsig1,mpi_rtype,proc1,mpi_comm_world,ierror)

    if (allocated(z41)) then
       call grid2sub(reshape(z41,(/nlat,nlon,nsig1o/)),sf1,vp1,t1,q1,qi1,ql1,qr1,qs1,oz1,cw1,rh1,mrh1,ps1)
       deallocate(z41)
    endif
    if (allocated(z42)) then
       call grid2sub(reshape(z42,(/nlat,nlon,nsig1o/)),sf2,vp2,t2,q2,qi2,ql2,qr2,qs2,oz2,cw2,rh2,mrh2,ps2)
       deallocate(z42)
    endif

    allocate(rh(lat1,lon1,nsig))
   
    if (readperts) then

       q1=rh1
       rh=mrh1

       if(rhbounds(1)>-900.0) then
          where(q1 <rhbounds(1)) q1=rhbounds(1)
       endif
       if(rhbounds(2)< 900.0) then
          where(q1 >rhbounds(2)) q1=rhbounds(2)
       endif

    else

      ice=.true.
      allocate(grdq1(lat1,lon1,nsig))
      grdq1= q1
      call genqsat(t1,grdq1,lat1,lon1,ps1,ice,sigl,ak5,bk5)
      allocate(grdq2(lat1,lon1,nsig))
      grdq2= q2
      call genqsat(t2,grdq2,lat1,lon1,ps2,ice,sigl,ak5,bk5)
   
      do k=1,nsig
        do j=1,lon1
          do i=1,lat1
            if( abs(grdq1(i,j,k)).gt.0. ) then
              q1(i,j,k)=q1(i,j,k)/grdq1(i,j,k)
            else
              q1(i,j,k)=0.0
            endif
            if( abs(grdq2(i,j,k)).gt.0. ) then
              q2(i,j,k)=q2(i,j,k)/grdq2(i,j,k)
            else
              q2(i,j,k)=0.0
            endif
          end do
        end do
      end do
      deallocate(grdq1,grdq2)
   
      rh=0.5*(q1+q2)

      do k=1,nsig
        do j=1,lon1
          do i=1,lat1
            if(q1(i,j,k) < rhbounds(1)) q1(i,j,k)=rhbounds(1)
            if(q1(i,j,k) > rhbounds(2)) q1(i,j,k)=rhbounds(2)
            if(q2(i,j,k) < rhbounds(1)) q2(i,j,k)=rhbounds(1)
            if(q2(i,j,k) > rhbounds(2)) q2(i,j,k)=rhbounds(2)
          end do
        end do
      end do

    cw1 = cw1 - cw2
    oz1 = oz1 - oz2
    q1  = q1  - q2
    qi1 = qi1 - qi2
    ql1 = ql1 - ql2
    qr1 = qr1 - qr2
    qs1 = qs1 - qs2
    t1  = t1  - t2
    vp1 = vp1 - vp2
    sf1 = sf1 - sf2
    ps1 = ps1 - ps2

    deallocate(sf2,&
               vp2,&
                t2,&
                q2,&
               qi2,&
               ql2,&
               qr2,&
               qs2,&
               rh2,&
              mrh2,&
               oz2,&
               cw2,&
               ps2 )


    endif

    write(filunit) sf1,vp1,t1,ps1,q1,rh,qi1,ql1,qr1,qs1,oz1,cw1
    deallocate(rh)

    if(proc1==1 .and. mype==0) print*,'store for bias calculation '
    bbt(:,:,:) =bbt(:,:,:) +t1(:,:,:)
    bbs(:,:,:) =bbs(:,:,:) +sf1(:,:,:)
    bbv(:,:,:) =bbv(:,:,:) +vp1(:,:,:)
    bbq(:,:,:) =bbq(:,:,:) +q1(:,:,:)
    bbqi(:,:,:)=bbqi(:,:,:)+qi1(:,:,:)
    bbql(:,:,:)=bbql(:,:,:)+ql1(:,:,:)
    bbqr(:,:,:)=bbqr(:,:,:)+qr1(:,:,:)
    bbqs(:,:,:)=bbqs(:,:,:)+qs1(:,:,:)
    bboz(:,:,:)=bboz(:,:,:)+oz1(:,:,:)
    bbp(:,:  ) =bbp(:,:  ) +ps1(:,:  )

    deallocate(sf1,&
               vp1,&
                t1,&
                q1,&
               qi1,&
               ql1,&
               qr1,&
               qs1,&
               rh1,&
              mrh1,&
               oz1,&
               cw1,&
               ps1 )

 end subroutine scatter_

 subroutine final_
  integer j,k
  bbp = bbp/float(numcases)
  bbt = bbt/float(numcases)
  bbs = bbs/float(numcases)
  bbv = bbv/float(numcases)
  bbq = bbq/float(numcases)
  bbql = bbql/float(numcases)
  bbqi = bbqi/float(numcases)
  bbqr = bbqr/float(numcases)
  bbqs = bbqs/float(numcases)
  bboz = bboz/float(numcases)

  if(mype==0)then

!   Prepare horizontal smoothing coefficients
    vcoef=hcoeffs(1:nsig)
    do k=1,nsig
      write(12,*)k,sigl(k),vcoef(k)
    end do

    call getvsm_(nsig,vcoef,vsmth)
    call m3minv(vsmth,nsig,det)

    do k=1,nsig
      vsmall(k)=0
      do j=1,nsig
        vsmall(k)=vsmall(k)+vsmth(j,k)
      enddo
    enddo

!   Prepare vertical smoothing coefficients
    vcoef = vcoeffs(1:nsig)
    do k=1,nsig
      write(14,*)k,sigl(k),vcoef(k)
    enddo
    call getvsm_(nsig,vcoef,vsmc)
    call m3minv(vsmc,nsig,det)

  endif !mype=0
 end subroutine final_

end subroutine m_readpairs

subroutine getvsm_(nsig,vcoef,vsmth)
  use type_kinds,only : fp_kind,single,double
  use variables, only : sigi,sigl 
  implicit none
  integer,intent(in) :: nsig
  real(fp_kind),dimension(nsig),intent(in) :: vcoef 
  real(fp_kind),dimension(nsig,nsig),intent(out) :: vsmth

  integer k
  real(fp_kind) :: vf
   
  vsmth=0

  do k=1,nsig
    vsmth(k,k)=1.
    vf=vcoef(k)*(sigi(k+1)-sigi(k))
    if(k > 1)then
      vsmth(k-1,k)=-vf/(sigl(k)-sigl(k-1))
      vsmth(k,k)=vsmth(k,k)-vsmth(k-1,k)
    endif
    if(k < nsig)then
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
