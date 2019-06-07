module m_stmGrid
  implicit none
  private
  public :: stmGrid_dump
  public :: stmGrid_load
!  type stmGrid
!    private
!    ...
!  end type stmGrid
  interface stmGrid_dump; module procedure dump_; end interface
  interface stmGrid_load; module procedure load_; end interface
  integer,parameter :: LINEAR=0
  integer,parameter :: LEVELS=1
  integer,parameter :: IVERS0 =0
  integer,parameter :: I0100x =256
contains
#include "assert.H"
!-----------------------------------------------------------------------
subroutine dump_(irmx,jrmx,kmax,mtv2,nstm,slonn,slatn,datan)
  use m_ioutil,only : luavail
  use m_die,only : assert_
  implicit none
  integer,intent(in) :: irmx,jrmx,kmax,mtv2,nstm
  real,dimension(nstm),intent(in) :: slonn,slatn
  real,dimension(irmx,jrmx,mtv2,nstm),intent(in) :: datan

  character(len=*),parameter :: myname_='m_stmGrid::dump_()'
  integer :: istm,k,lu
  character(len=80) :: fname
  real,dimension(kmax) :: zdef
  real :: delt,slog,sigm

  delt=(log(.001)-log(1000.))/(kmax-1)
  do k=1,kmax
    slog=log(1000.)+(k-1)*delt
    sigm=exp(slog)
    zdef(k)=1000.*sigm
  end do

  do istm=1,nstm
    write(fname,'(a,i2.2,a)') 'HDAT',istm,'.dmp'
    lu=luavail()
    open(lu,file=fname,form='unformatted',	&
    		access='sequential' ,	&
		status='unknown'	)

    write(lu) IVERS0,I0100x
    write(lu) irmx,jrmx,kmax,mtv2,slonn(istm),slatn(istm)
    write(lu) irmx,LINEAR
    write(lu) slonn(istm)-20.,1.
    write(lu) jrmx,LINEAR
    write(lu) slatn(istm)-20.,1.
    write(lu) kmax,LEVELS
    write(lu) zdef

    do k=1,mtv2
      write(lu) datan(:,:,k,istm)
    end do
    close(lu)
  end do
end subroutine dump_
!-----------------------------------------------------------------------
subroutine load_(irmx,jrmx,kmax,mtv2,nstm,slonn,slatn,datan)
  use m_ioutil,only : luavail
  use m_die,only : assert_
  implicit none
  integer,intent(in) :: irmx,jrmx,kmax,mtv2,nstm
  real,dimension(nstm),intent(out) :: slonn,slatn
  real,dimension(irmx,jrmx,mtv2,nstm),intent(out) :: datan

  character(len=*),parameter :: myname_='m_stmGrid::load_()'
  integer :: istm,k,lu
  character(len=80) :: fname
  real,dimension(kmax) :: zdef
  real :: is
  integer :: ivers,i0100
  integer :: irx,jrx,kmx,mtv,igrid,jgrid,kgrid
  real :: slon,slat,dlon,dlat

  do istm=1,nstm
    write(fname,'(a,i2.2,a)') 'HDAT',istm,'.dmp'
    lu=luavail()
    open(lu,file=fname,form='unformatted',	&
    		access='sequential' ,	&
		status='old'		)

    read(lu) ivers,i0100
    	ASSERT(ivers==IVERS0)
    	ASSERT(i0100==I0100x)
    read(lu) irx,jrx,kmx,mtv,slonn(istm),slatn(istm)
    	ASSERT(irx==irmx)
    	ASSERT(jrx==jrmx)
    	ASSERT(kmx==kmax)
    	ASSERT(mtv==mtv2)
    read(lu) irx,igrid
    	ASSERT(irx==irmx)
    	ASSERT(igrid==LINEAR)
    read(lu) slon,dlon

    read(lu) jrx,jgrid
    	ASSERT(jrx==jrmx)
    	ASSERT(jgrid==LINEAR)
    read(lu) slat,dlat

    read(lu) kmx,kgrid
    	ASSERT(kmx==kmax)
    	ASSERT(kgrid==LEVELS)
    read(lu) zdef(1:kmax)

    do k=1,mtv2
      read(lu) datan(:,:,k,istm)
    end do
    close(lu)
  end do
end subroutine load_
end module m_stmGrid
