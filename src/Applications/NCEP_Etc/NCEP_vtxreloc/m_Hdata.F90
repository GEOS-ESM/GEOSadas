module m_Hdata
!-- all components of HDATA object
  implicit none
  private
      
 	! Data of m_HDATA
!!  public :: IMAX,JMAX,KMAX
  public :: GLON		! GLON(IMAX)
  public :: GLAT		! GLAT(JMAX)
  public :: AKI,SI		! AKI(KMAX+1),SI(KMAX+1)
  public :: AKL,SL		! AKL(KMAX  ),SL(KMAX  )
!!  public :: MTV			! size(HDATA,3) := 2+4*KMAX
  public :: HDATA		! HDATA(imax,jmax,mHDATA)
  public :: wdata		! wdata(imax,jmax,kmax,2)

 	! interfaces of m_HDATA
  public :: HDATA_init		! setup HDATA object
  public :: HDATA_clean		! clean up HDATA object
  public :: HDATA_getptrs	! get variable references to HDATA/wdata
  public :: getptrs_		! get variable references to HDATA/wdata

  public :: ptr_zs,ptr_ps	! aliases to 2-d zsfc and lnps
  public :: ptr_tv,ptr_qw	! aliases to 3-d virt. T and w.v.m.r.
  public :: ptr_di,ptr_ze	! aliases to 3-d divr. and vort.
  public :: ptr_uw,ptr_vw	! aliases to 3-d u-wind and v-wind.

! data components:
  integer,save :: IMAX
  integer,save :: JMAX
  integer,save :: KMAX
  integer,save :: MTV

  real,allocatable,dimension(:),save :: GLON,GLAT	! lon-lat
  real,allocatable,dimension(:),save :: AKI,SI,AKL,SL	! bot.up ak-bk

  real,allocatable,dimension(:,:,:  ),save,target :: HDATA
  real,allocatable,dimension(:,:,:,:),save,target :: wdata

  interface Hdata_init; module procedure init_; end interface
  interface Hdata_clean; module procedure clean_; end interface
  interface Hdata_getptrs; module procedure getptrs_; end interface

	! variable references
  integer,save :: iZS,iPS
  integer,save :: iTV,jTV,kTV
  integer,save :: iDZ,jDZ,kDZ
  integer,save :: iQW,jQW,kQW
  integer,save :: iUW,iVW

    character(len=*),parameter :: myname='m_Hdata'
  logical,save :: initialized_ = .false.

#include "mytrace.H"
contains
  subroutine init_(imax_,jmax_,kmax_)
    use m_die,only : die
    implicit none
    integer,intent(in) :: imax_
    integer,intent(in) :: jmax_
    integer,intent(in) :: kmax_
    character(len=*),parameter :: myname_=myname//'.init_'
_ENTRY_
    if(initialized_) call die(myname_,'already initialized')
    initialized_=.true.

    IMAX=imax_
    JMAX=jmax_
    KMAX=kmax_
    MTV=2+4*KMAX

    allocate(GLON(IMAX),GLAT(JMAX))
    allocate(AKI(KMAX+1),SI(KMAX+1))
    allocate(AKL(KMAX  ),SL(KMAX  ))

    allocate(HDATA(IMAX,JMAX,MTV))
    allocate(wdata(IMAX,JMAX,KMAX,2))

    iZS = 1
    iPS = 2

    iTV = iPS + 1
    kTV = 1			! stride
    jTV = iPS + KMAX*kTV	! (iTV:jTV:kTV)

    iDZ = jTV + 1
    kDZ = 2			! (iDZ+0:jDZ-1:2) for di
    jDZ = jTV + KMAX*kDZ	! (iDZ+1:jDZ-0:2) for ze

    iQW = jDZ + 1
    kQW = 1			! stride
    jQW = jDZ + KMAX*kQW	! (iQW:jQW:kQW)

    iUW = 1
    iVW = 2
_EXIT_
  end subroutine init_

  function ptr_zs() result(ptr)
    use m_die,only : die
    implicit none
    real,pointer,dimension(:,:) :: ptr
    character(len=*),parameter :: myname_=myname//'.ptr_zs'
_ENTRY_
    if(.not.initialized_) call die(myname_,'uninitialized object')
    ptr => HDATA(:,:,iZS)
_EXIT_
  end function ptr_zs

  function ptr_ps() result(ptr)
    use m_die,only : die
    implicit none
    real,pointer,dimension(:,:) :: ptr
    character(len=*),parameter :: myname_=myname//'.ptr_ps'
_ENTRY_
    if(.not.initialized_) call die(myname_,'uninitialized object')
    ptr => HDATA(:,:,iPS)
_EXIT_
  end function ptr_ps

  function ptr_tv() result(ptr)
    use m_die,only : die
    implicit none
    real,pointer,dimension(:,:,:) :: ptr
    character(len=*),parameter :: myname_=myname//'.ptr_tv'
_ENTRY_
    if(.not.initialized_) call die(myname_,'uninitialized object')
    ptr => HDATA(:,:,iTV:jTV:kTV)
_EXIT_
  end function ptr_tv

  function ptr_di() result(ptr)
    use m_die,only : die
    implicit none
    real,pointer,dimension(:,:,:) :: ptr
    character(len=*),parameter :: myname_=myname//'.ptr_di'
_ENTRY_
    if(.not.initialized_) call die(myname_,'uninitialized object')
    ptr => HDATA(:,:,iDZ+0:jDZ-1:KDZ)
_EXIT_
  end function ptr_di

  function ptr_ze() result(ptr)
    use m_die,only : die
    implicit none
    real,pointer,dimension(:,:,:) :: ptr
    character(len=*),parameter :: myname_=myname//'.ptr_ze'
_ENTRY_
    if(.not.initialized_) call die(myname_,'uninitialized object')
    ptr => HDATA(:,:,iDZ+1:jDZ-0:KDZ)
_EXIT_
  end function ptr_ze

  function ptr_uw() result(ptr)
    use m_die,only : die
    implicit none
    real,pointer,dimension(:,:,:) :: ptr
    character(len=*),parameter :: myname_=myname//'.ptr_uw'
_ENTRY_
    if(.not.initialized_) call die(myname_,'uninitialized object')
    ptr => wdata(:,:,:,iUW)
_EXIT_
  end function ptr_uw

  function ptr_vw() result(ptr)
    use m_die,only : die
    implicit none
    real,pointer,dimension(:,:,:) :: ptr
    character(len=*),parameter :: myname_=myname//'.ptr_vw'
_ENTRY_
    if(.not.initialized_) call die(myname_,'uninitialized object')
    ptr => wdata(:,:,:,iVW)
_EXIT_
  end function ptr_vw

  function ptr_qw() result(ptr)
    use m_die,only : die
    implicit none
    real,pointer,dimension(:,:,:) :: ptr
    character(len=*),parameter :: myname_=myname//'.ptr_qw'
_ENTRY_
    if(.not.initialized_) call die(myname_,'uninitialized object')
    ptr => HDATA(:,:,iQW:jQW:kQW)
_EXIT_
  end function ptr_qw

  subroutine getptrs_(h,w,zs,ps,tv,qw,di,ze,uw,vw)
!-- used to avoid chained dependencies of proc() on m_HDATA.

    use m_die,only : die
    implicit none
    real,target,dimension(:,:,:),intent(in) :: h
    real,target,dimension(:,:,:,:),intent(in) :: w
    real,pointer,dimension(:,:) :: zs,ps
    real,pointer,dimension(:,:,:) :: tv,qw,di,ze,uw,vw

!       Note that target arguments h and w are not required to be the
!   same variables HDATA and wDATA in m_HDATA, as long as the same
!   structure (see iZS, iPS, etc.) is assumed.

! Usecase:
!   program main
!   use m_HDATA,only : h => HDATA, w => wdata
!   use m_HDATA,only : getptrs => HDATA_getptrs
!   call proc(h,w,getptrs)	! require an explicit interface
!   stop
!   contains
!   subroutine proc(h,w,getptrs)
!   implicit none
!   real,target,dimension(:,:,:),intent(inout) :: h
!   real,target,dimension(:,:,:,:),intent(inout) :: w
!   interface
!     subroutine getptrs(h,w,zs,ps,tv,qw,di,ze,uw,vw)
!       implicit none
!       real,target,dimension(:,:,:),intent(in) :: h
!       real,target,dimension(:,:,:,:),intent(in) :: w
!	real,pointer,dimension(:,:) :: zs,ps
!	real,pointer,dimension(:,:,:) :: tv,qw,di,ze,uw,vw
!     end subroutine getptrs
!   end interface
!   call getptrs(h,w,zs,ps,tv,qw,di,ze,uw,vw)
!   end subroutine proc
!   end program main

    character(len=*),parameter :: myname_=myname//'.getptrs_'
_ENTRY_
    if(.not.initialized_) call die(myname_,'uninitialized object')

    zs => h(:,:,iZS)
    ps => h(:,:,iPS)
    tv => h(:,:,iTV:jTV:kTV)
    qw => h(:,:,iQW:jQW:kQW)
    di => h(:,:,iDZ+0:jDZ-1:kDZ)
    ze => h(:,:,iDZ+1:jDZ-0:kDZ)

    uw => w(:,:,:,iUW)
    vw => w(:,:,:,iVW)
_EXIT_
  end subroutine getptrs_

  subroutine clean_()
    use m_die,only : die
    implicit none

    character(len=*),parameter :: myname_=myname//'.clean_'
_ENTRY_
    if(.not.initialized_) call die(myname_,'uninitialized object')
    initialized_=.false.

    deallocate(HDATA)
    deallocate(wdata)

    deallocate(GLON,GLAT)
    deallocate(AKI,SI)
    deallocate(AKL,SL)

    IMAX=0
    JMAX=0
    KMAX=0
    MTV =0
_EXIT_
  end subroutine clean_
end module m_Hdata
