subroutine calcbal(mycases,numcases,npes,mype)
  use type_kinds,only: fp_kind
  use postmod,only: smoothlat
  use variables,only: nlat,nlon,nsig,fnm0
  use variables,only: tcon,vpcon,pscon
  use variables,only: smoothdeg,db_prec
  use variables,only: grdsf,grdvp,gridt,gridp

#ifndef ibm_sp
  use m_mpif
#endif
  implicit none
#ifdef ibm_sp
  include 'mpif.h'
#endif

  character(255) grdfile
  integer ncfggg,iret
  integer i,j,k,m,n,ierror,mpi_rtype
  integer,intent(in):: mycases,numcases,npes,mype

  real(fp_kind),allocatable,dimension(:,:,:):: zz3_av,zt3_av
  real(fp_kind),allocatable,dimension(:,:)  :: zz2_av,zd2_av
!  real(fp_kind),allocatable,dimension(:)  :: zp2_av
  real(fp_kind),allocatable,dimension(:,:)  :: zp2_av

! variables for ESSL/LAPACK routines
  real(fp_kind),dimension(4*nsig):: auxsv
  real(fp_kind),dimension(nsig):: sval
  real(fp_kind),dimension(nsig,nsig):: sigmtx,matred,bmtx,matrix
  real(fp_kind) norm
  real(fp_kind),dimension(5*nsig):: work1
  real(fp_kind) :: svmax,svdpec
  integer :: fnm,indx
  integer info

  norm=1/float(nlon*numcases)
  fnm=abs(fnm0)

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  allocate(zz3_av(nlat,nsig,nsig))
  allocate(zt3_av(nlat,nsig,nsig))
  allocate(zz2_av(nlat,nsig))
  allocate(zd2_av(nlat,nsig))
  allocate(zp2_av(nlat,nsig))
!  allocate(zp2_av(nlat))

  zz3_av=0.
  zt3_av=0.
  zd2_av=0.
  zz2_av=0.
  zp2_av=0.


  if (mype==0) write(6,*) 'IN ROUTINE TO CALCULATE BALANCE PROJECTIONS'

! 1) Get projection from SF -> T & SF -> VP
  do n=1,mycases

#ifdef gmao_intf
    call m_rdgrids(n,npes,mype,mycases)
#else
!    call convert2grid(n,npes,mype,mycases) ! does not exist anymore
#endif

    do m=1,nsig
      do k=1,nsig
        do j=1,nlon
          do i=2,nlat-1
! streamfunction-streamfunction
            zz3_av(i,k,m)=zz3_av(i,k,m)+grdsf(i,j,k)*grdsf(i,j,m)
! streamfunction-temparature
            zt3_av(i,k,m)=zt3_av(i,k,m)+grdsf(i,j,k)*gridt(i,j,m)
          end do
        end do
      end do
    end do

    do k=1,nsig
      do j=1,nlon
        do i=2,nlat-1
! streamfunction-velocity potential
          zd2_av(i,k)=zd2_av(i,k)+grdsf(i,j,k)*grdvp(i,j,k)
! streamfunction-streamfunction
          zz2_av(i,k)=zz2_av(i,k)+grdsf(i,j,k)*grdsf(i,j,k)
! streamfunction-surface-pressure
          zp2_av(i,k)=zp2_av(i,k)+grdsf(i,j,k)*gridp(i,j)
        end do
      end do
    end do
!    do j=1,nlon
!      do i=2,nlat-1
!        zp2_av(i)=zp2_av(i)+grdsf(i,j,1)*gridp(i,j)
!      end do
!    end do

  end do ! end do n cases

  call mpi_allreduce((zz3_av),zz3_av,nlat*nsig*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((zt3_av),zt3_av,nlat*nsig*nsig,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((zz2_av),zz2_av,nlat*nsig     ,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((zd2_av),zd2_av,nlat*nsig     ,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
  call mpi_allreduce((zp2_av),zp2_av,nlat*nsig      ,mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
!  call mpi_allreduce((zp2_av),zp2_av,nlat          ,mpi_rtype,mpi_sum, &
!       mpi_comm_world,ierror)

  zz3_av=zz3_av*norm
  zt3_av=zt3_av*norm
  zz2_av=zz2_av*norm
  zd2_av=zd2_av*norm
  zp2_av=zp2_av*norm

! Smooth correlation matrices in latitudinal direction
  call smoothlat(zz3_av,nsig*nsig,smoothdeg)
  call smoothlat(zt3_av,nsig*nsig,smoothdeg)
  call smoothlat(zz2_av,nsig,smoothdeg)
  call smoothlat(zd2_av,nsig,smoothdeg)
  call smoothlat(zp2_av,nsig,smoothdeg)
!  call smoothlat(zp2_av,1,smoothdeg)

  if (mype==0) then
 
   grdfile='tst_sp.grd'
   ncfggg=len_trim(grdfile)
   call baopenwt(22,grdfile(1:ncfggg),iret)
   call wryte(22,4*nlat*nsig*nsig,zt3_av)
   call baclose(22,iret)
 
   grdfile='stst_sp.grd'
   ncfggg=len_trim(grdfile)
   call baopenwt(22,grdfile(1:ncfggg),iret)
   call wryte(22,4*nlat*nsig*nsig,zz3_av)
   call baclose(22,iret)
 
  end if

! invert 3d streamfunction-streamfunction correlation matrix for computing
! temperature balance projections
  matrix=0.
  do i=2,nlat-1
    do m=1,nsig
      do k=1,nsig
        matrix(k,m)=zz3_av(i,k,m)
      end do
    end do
! BMTX = U(transpose)
    bmtx=0.0
    do j=1,nsig
      bmtx(j,j)=1.0
    end do
    sval=0.0
! get singular values
#ifdef _LAPACK_
    if (db_prec) then
      call dgesvd('S','S',nsig,nsig,matrix,nsig,sval,bmtx,nsig, &
                  matred,nsig,work1,5*nsig,info)
    else
      call sgesvd('S','S',nsig,nsig,matrix,nsig,sval,bmtx,nsig, &
                  matred,nsig,work1,5*nsig,info)
    end if
    if(info.ne.0)then
       write(6,*)'something is wrong in SVD'; stop 30
    endif
#else
    if (db_prec) then
      call dgesvf(12,matrix,nsig,bmtx,nsig,nsig,sval,nsig,nsig,&
                auxsv,4*nsig)
    else
      call sgesvf(12,matrix,nsig,bmtx,nsig,nsig,sval,nsig,nsig,&
                  auxsv,4*nsig)
    end if
#endif

! keep leading singular values
    sigmtx=0.0
    if ( fnm0 < 0 ) then 
       indx = nsig - fnm + 1
    else
       indx = 1
    endif
    do k=indx,indx+fnm-1
      if (sval(k) == 0) write(6,*)' zero singular value' 
      sigmtx(k,k)=1./sval(k)
    end do

    svmax=0
    svdpec=0
    do k=1,nsig
      svmax=svmax+sigmtx(k,k)
      if(k>=indx .and. k<=indx+fnm-1) svdpec=svdpec+sigmtx(k,k) 
    enddo
    svdpec=svdpec/svmax
   
    if(mype==0)then
      write(127,*)'*** lat=',i,' modes:',fnm0,'percentage:',svdpec
      write(127,'(7(1x,e15.8))')sval
    endif

! perform matrix multiplication sval*V(transpose)
#ifdef _LAPACK_
!    matred = 0.
!    do j = 1,nsig
!      do k = 1,nsig
!         matred(k,j) = sigmtx(k,k)*bmtx(j,k)
!          matrix(j,k) = sigmtx(k,k)*bmtx(j,k)
!      enddo
!    enddo

    matrix = 0.0
    if (db_prec) then
      call dgemm ('N','N',nsig,nsig,nsig,1.0,sigmtx,nsig,matred,nsig,0.0,matrix,nsig)
    else
      call sgemm ('N','N',nsig,nsig,nsig,1.0,sigmtx,nsig,matred,nsig,0.0,matrix,nsig)
    end if
    matred = matrix
#else
    if (db_prec) then
      call dgemul(sigmtx,nsig,'N',matrix,nsig,'T',matred,nsig,nsig,nsig,nsig)
    else
      call sgemul(sigmtx,nsig,'N',matrix,nsig,'T',matred,nsig,nsig,nsig,nsig)
    end if
#endif

    matrix=0.0

#ifdef _LAPACK_
    if (db_prec) then
      call dgemm ('N','N',nsig,nsig,nsig,1.0,bmtx,nsig,matred,nsig,0.0,matrix,nsig)
    else
      call sgemm ('N','N',nsig,nsig,nsig,1.0,bmtx,nsig,matred,nsig,0.0,matrix,nsig)
    end if
#else
    if (db_prec) then
      call dgemul(bmtx,nsig,'T',matred,nsig,'N',matrix,nsig,nsig,nsig,nsig)
    else
      call sgemul(bmtx,nsig,'T',matred,nsig,'N',matrix,nsig,nsig,nsig,nsig)
    end if
#endif

! load back into original 3d array
    do m=1,nsig
      do k=1,nsig
        zz3_av(i,k,m)=matrix(k,m)
      end do
    end do
  end do ! enddo lat

  tcon=0.
  do n=1,nsig
    do m=1,nsig
      do i=2,nlat-1
        do k=1,nsig
          tcon(i,n,m)=tcon(i,n,m)+zz3_av(i,m,k)*zt3_av(i,k,n)
        end do
      end do
    end do
! fill 'pole points'
    do k=1,nsig
      tcon(1,n,k)=tcon(2,n,k)
      tcon(nlat,n,k)=tcon(nlat-1,n,k)
    end do
  end do !end do n

! velocity potential constraint
  vpcon=0
  do k=1,nsig
    do i=2,nlat-1
      vpcon(i,k)=vpcon(i,k)+zd2_av(i,k)/zz2_av(i,k)
    end do
! fill 'pole points'
    vpcon(1,k)=vpcon(2,k)
    vpcon(nlat,k)=vpcon(nlat-1,k)
  end do

  pscon=0.
!  do i=2,nlat-1
!      pscon(i,1)=pscon(i,1)+zp2_av(i)/zz2_av(i,1)
!  enddo
!  pscon(   1,1)=pscon(     2,1)
!  pscon(nlat,1)=pscon(nlat-1,1)

  do i=2,nlat-1
  do m=1,nsig
  do k=1,nsig
        pscon(i,m)=pscon(i,m)+zz3_av(i,m,k)*zp2_av(i,k)
  enddo
  enddo
  enddo
  pscon(   1,1:nsig)=pscon(     2,1:nsig)
  pscon(nlat,1:nsig)=pscon(nlat-1,1:nsig)

  deallocate(zz3_av,zt3_av)
  deallocate(zz2_av,zd2_av)
  deallocate(zp2_av)

  return
end subroutine calcbal

