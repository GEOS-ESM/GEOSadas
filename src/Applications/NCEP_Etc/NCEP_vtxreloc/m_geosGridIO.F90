module m_geosGridIO

!   A geosGridIO is a single instance object of a GMAO's GEOS FV state
! with an input (I) and an output (O) leg.  A gesoGridIO object is
! created by opening an input file and a read_() process for the
! creation of a m_HDATA with the same class of grid definition but in a
! different variable space.  The object is then changed to an output
! state, by updating the known input state to a given m_HDATA through a
! write_() process.
!
! Lifecyle: []_open() -> []_getdims() -> []_getgrids() -> []_read()
!           -> []_write() -> []_close() .
!
!   Procedures []_open(), []_read(), []_write(), and []_close() may
! change the state of this module object.
! 
! !REVISION HISTORY:
!  22Jun2009 Todling - update interface to dyn_get (GEOS-5 vector)
!  24Nov2009 Todling - update interface to dyn_put (GEOS-5 vector)
!  30Jun2010 Kokron  - Added forceflip to dyn_get to get the relocator working again
!

  use m_dyn,only : dyn_vect
  implicit none
  private
  public :: geosGRID
  public :: geosGridIO_open
  public :: geosGridIO_getdims
  public :: geosGridIO_getgrids
  public :: geosGridIO_read
  public :: geosGridIO_write
  public :: geosGridIO_close

  integer,parameter :: geosGRID=1
  interface geosGridIO_open; module procedure open_; end interface
  interface geosGridIO_read; module procedure read_; end interface
  interface geosGridIO_getdims; module procedure getdims_; end interface
  interface geosGridIO_getgrids; module procedure getgrids_; end interface
  interface geosGridIO_write; module procedure write_; end interface
  interface geosGridIO_close; module procedure close_; end interface

      	! Cached grid dimensions and grid definitions
      integer,save :: IMAX_,JMAX_
      REAL,   ALLOCATABLE,save :: GLAT_(:),GLON_(:)
      integer,save :: KMAX_
      REAL,   ALLOCATABLE,save :: AKI_(:),BKI_(:)
      REAL,   ALLOCATABLE,save :: AKL_(:),BKL_(:)

      integer,save :: nymd_,nhms_
      integer,save :: nstep,freq

      type(dyn_vect),save :: wf
      logical,save :: swapgrid_=.false.
      logical,save :: initialized_=.false.
      integer,parameter :: IPREC_default = 0	! for 32 bits output

      character(len=*),parameter :: myname='m_geosGridIO'

#define DEBUG_TRACE
#include "assert.H"
#include "mytrace.H"
contains
      
      subroutine open_(file,swapgrid,nymd,nhms)
      use m_dyn,only : dyn_get
      use m_gsiGriddedState,only : fvGvec
      use m_die,only : die,assert_
      implicit none
      character(len=*),intent(in) :: file
      logical,optional,intent(in) :: swapgrid
      integer,optional,intent(in) :: nymd,nhms

      integer :: ier
      character(len=*),parameter :: myname_=myname//'.open_'
_ENTRY_

      if(initialized_) call die(myname_,'already initialized')
      initialized_=.true.

      swapgrid_=.false.
      if(present(swapgrid)) swapgrid_=swapgrid

      nymd_=0
      if(present(nymd)) nymd_=nymd
      nhms_=0
      if(present(nhms)) nhms_=nhms

      if (fvGvec == 5) then
         call dyn_get(file,nymd_,nhms_,wf,ier, &
              nstep=nstep,timidx=1,freq=freq,vectype=fvGvec,forceflip=.true.)
      else
         call dyn_get(file,nymd_,nhms_,wf,ier, &
              nstep=nstep,timidx=1,freq=freq,vectype=fvGvec)
      end if

	if(ier/=0) call die(myname_, &
		'dyn_get("'//trim(file)//'") error',ier)

! Cache dimension information

      imax_=wf%grid%im
      jmax_=wf%grid%jm
      kmax_=wf%grid%km

  ASSERT(imax_==size(wf%grid%lon))
  ASSERT(jmax_==size(wf%grid%lat))
  ASSERT(kmax_==size(wf%grid%ak(:))-1)

      ALLOCATE ( GLAT_(JMAX_),GLON_(IMAX_) )
      ALLOCATE ( AKI_(KMAX_+1),BKI_(KMAX_+1) )
      ALLOCATE ( AKL_(KMAX_  ),BKL_(KMAX_  ) )

      GLON_(:)=wf%grid%lon(:)
      GLAT_(:)=wf%grid%lat(:)
      if(swapgrid_) GLAT_(1:jmax_)=GLAT_(jmax_:1:-1)

      AKI_(:) =wf%grid%ak(:)
      if(swapgrid_) AKI_(1:kmax_+1)=AKI_(kmax_+1:1:-1)
      BKI_(:) =wf%grid%bk(:)
      if(swapgrid_) BKI_(1:kmax_+1)=BKI_(kmax_+1:1:-1)

      AKL_(1:KMAX_)=(AKI_(1:KMAX_)+AKI_(2:KMAX_+1))/2.
      BKL_(1:KMAX_)=(BKI_(1:KMAX_)+BKI_(2:KMAX_+1))/2.
_EXIT_
      end subroutine open_

      subroutine getdims_(imax,jmax,kmax)
	implicit none
	integer,optional,intent(out) :: imax,jmax,kmax
	character(len=*),parameter :: myname_=myname//'.getdims_'
_ENTRY_
	if(present(imax)) imax=IMAX_
	if(present(jmax)) jmax=JMAX_
	if(present(kmax)) kmax=KMAX_
_EXIT_
      end subroutine getdims_

      subroutine getgrids_(glon,glat,aki,bki,si,akl,bkl,sl)
	implicit none
	real,optional,dimension(:),intent(out) :: glon,glat
	real,optional,dimension(:),intent(out) :: aki,bki,si,akl,bkl,sl

	character(len=*),parameter :: myname_=myname//'.getdims_'
_ENTRY_

	if(present(glon)) glon(:)=glon_(:)
	if(present(glat)) glat(:)=glat_(:)

	if(present(aki)) aki(:)=aki_(:)
	if(present(bki)) bki(:)=bki_(:)
	if(present( si))  si(:)=bki_(:)

	if(present(akl)) akl(:)=akl_(:)
	if(present(bkl)) bkl(:)=bkl_(:)
	if(present( sl))  sl(:)=bkl_(:)
_EXIT_
      end subroutine getgrids_

      subroutine read_(HDATA,wdata,getptrs)
      use m_gsiGriddedState,only : gsiGriddedState_convert
      use m_die,only : die
      implicit none
      real,target,dimension(:,:,:),intent(out) :: HDATA
      real,target,dimension(:,:,:,:),intent(out) :: wdata
      include "getptrs.h"

! local variables

! aliases of the input (Hdata,wdata) state variables
  real,pointer,dimension(:,:) :: zsfc,lnps
  real,pointer,dimension(:,:,:) :: tv,qw,di,ze,uw,vw

  integer :: im_w
  real,pointer,dimension(:) :: w_lat,w_aki,w_bki
  real,pointer,dimension(:,:) :: w_phis,w_psfc
  real,pointer,dimension(:,:,:) :: w_pt,w_qw,w_uw,w_vw

  character(len=*),parameter :: myname_=myname//'.read_'
_ENTRY_
  if(.not.initialized_) call die(myname_,'not initialized')

      call getptrs(HDATA,wdata,zsfc,lnps,tv,qw,di,ze,uw,vw)

      	! Convert wf in geos dynvect to ncep variables.
      	
        im_w = wf%grid%im
	w_lat => wf%grid%lat
	w_aki => wf%grid%ak
	w_bki => wf%grid%bk

        w_phis => wf%phis(:,:)
        w_psfc => wf%ps(:,:)
	w_pt   => wf%pt(:,:,:)
	w_qw   => wf%q(:,:,:,1)
	w_uw   => wf%u(:,:,:)
	w_vw   => wf%v(:,:,:)

      call gsiGriddedState_convert(im_w,w_lat,w_aki,w_bki, &
        w_phis,w_psfc,w_pt,w_qw,w_uw,w_vw, &
	zsfc,lnps,tv,qw,di,ze,uw,vw)

      if(swapgrid_) call swaplat_(zsfc); nullify(zsfc)
      if(swapgrid_) call swaplat_(lnps); nullify(lnps)
      if(swapgrid_) call swapynz_(tv)  ; nullify(tv)
      if(swapgrid_) call swapynz_(qw)  ; nullify(qw)
      if(swapgrid_) call swapynz_(di)  ; nullify(di)
      if(swapgrid_) call swapynz_(ze)  ; nullify(ze)
      if(swapgrid_) call swapynz_(uw)  ; nullify(uw)
      if(swapgrid_) call swapynz_(vw)  ; nullify(vw)

      nullify(w_lat) ; nullify(w_aki) ; nullify(w_bki)
      nullify(w_phis); nullify(w_psfc)
      nullify(w_pt)  ; nullify(w_qw)
      nullify(w_uw)  ; nullify(w_vw)
_EXIT_
      end subroutine read_

      subroutine swaplat_(v)
        implicit none
	real,dimension(:,:),intent(inout) :: v
	integer :: m
	m=size(v,2)
	v(:,1:m:+1)=v(:,m:1:-1)
      end subroutine swaplat_
      subroutine swapynz_(v)
        implicit none
	real,dimension(:,:,:),intent(inout) :: v
	integer :: m,n
	m=size(v,2)
	n=size(v,3)
	v(:,1:m:+1,1:n:+1)=v(:,m:1:-1,n:1:-1)
      end subroutine swapynz_

      subroutine write_(file,HDATA,wdata,getptrs,prec)
      use m_dyn,only : dyn_put
      use m_gsiGriddedState,only : gsiGriddedState_increto
      use m_gsiGriddedState,only : gsiGriddedState_invconv
      use m_gsiGriddedState,only : fvGvec
      use m_die,only : die
      implicit none
      character(len=*),intent(in) :: file ! output filename
      real,target,dimension(:,:,:),intent(in) :: HDATA
      real,target,dimension(:,:,:,:),intent(in) :: wdata
      include "getptrs.h"
      integer,optional,intent(in) :: prec

! local variables

! aliases of the input (Hdata,wdata) state variables
  real,pointer,dimension(:,:) :: zsfc,lnps
  real,pointer,dimension(:,:,:) :: tv,qw,di,ze,uw,vw

! aliases of the output (wf) grids and state variables
  integer :: im_w
  real,pointer,dimension(:) :: w_lat,w_aki,w_bki
  real,pointer,dimension(:,:) :: w_psfc
  real,pointer,dimension(:,:,:) :: w_dp,w_pt,w_qw,w_uw,w_vw

  real,allocatable,dimension(:,:  ) :: lnpsb
  real,allocatable,dimension(:,:,:) :: tvb,qwb,dib,zeb

  integer :: iprec,ier
  character(len=*),parameter :: myname_=myname//'.write_'
_ENTRY_

  if(.not.initialized_) call die(myname_,'not initialized')

  iprec=IPREC_default
  if(present(prec)) iprec=prec

! alias variables in (HDATA,wdata)
      call getptrs(HDATA,wdata,zsfc,lnps,tv,qw,di,ze,uw,vw)

! allocate variables to store input (Hdata,wdata) variables
      allocate(lnpsb(imax_,jmax_))
      allocate(tvb(imax_,jmax_,kmax_),qwb(imax_,jmax_,kmax_))
      allocate(dib(imax_,jmax_,kmax_),zeb(imax_,jmax_,kmax_))

! Make a copy of (Hdata,wdata) variables
      lnpsb=lnps
        if(swapgrid_) call swaplat_(lnpsb)
      tvb=tv
        if(swapgrid_) call swapynz_(tvb)
      qwb=qw
        if(swapgrid_) call swapynz_(qwb)
      dib=di
        if(swapgrid_) call swapynz_(dib)
      zeb=ze
        if(swapgrid_) call swapynz_(zeb)

! Nullify aliases to disconnect from (Hdata,wdata) to avoid making
! accidental access.
      nullify(zsfc); nullify(lnps)
      nullify(tv)  ; nullify(qw)
      nullify(di)  ; nullify(ze)
      nullify(uw)  ; nullify(vw)

! compute increments of wf in dyn_vect variable space.  i.e.
!   wf += (lnpsb,tvb,qwb,dib,zeb)

        im_w = wf%grid%im
	w_lat => wf%grid%lat
	w_aki => wf%grid%ak
	w_bki => wf%grid%bk

        w_psfc => wf%ps(:,:)
	w_dp   => wf%delp(:,:,:)
	w_pt   => wf%pt(:,:,:)
	w_qw   => wf%q(:,:,:,1)
	w_uw   => wf%u(:,:,:)
	w_vw   => wf%v(:,:,:)

#define _INCRETO_
#ifndef _INCRETO_
      call gsiGriddedState_invconv(im_w,w_lat,w_aki,w_bki, &
        w_psfc,w_dp,w_pt,w_qw,w_uw,w_vw, lnpsb,tvb,qwb,dib,zeb)
#else
      call gsiGriddedState_increto(im_w,w_lat,w_aki,w_bki, &
        w_psfc,w_dp,w_pt,w_qw,w_uw,w_vw, lnpsb,tvb,qwb,dib,zeb)
#endif

	nullify(w_lat); nullify(w_aki); nullify(w_bki)
	nullify(w_psfc)
	nullify(w_dp) ; nullify(w_pt) ; nullify(w_qw)
	nullify(w_uw) ; nullify(w_vw)

! there is no need to keep these increments
      deallocate(lnpsb)
      deallocate(tvb,qwb)
      deallocate(dib,zeb)

_TRACE_('calling dyn_put()')
      call dyn_put(file,nymd_,nhms_,iprec,wf,ier,nstep=nstep,vectype=fvGvec)
	if(ier/=0) call die(myname_, &
		'dyn_put("'//trim(file)//'") error',ier)
_EXIT_
      end subroutine write_

      subroutine close_()
	use m_dyn,only : dyn_clean
	use m_die,only : die
        implicit none
	character(len=*),parameter :: myname_=myname//'.close_'
_ENTRY_
  if(.not.initialized_) call die(myname_,'not initialized')
  initialized_=.false.

	call dyn_clean(wf)
	deallocate(AKI_,BKI_)
	deallocate(AKL_,BKL_)
	deallocate(GLON_,GLAT_)

	imax_=0
	jmax_=0
	kmax_=0
	nymd_=0
	nhms_=0
	nstep=0
	freq =0
_EXIT_
      end subroutine close_

      end module m_geosGridIO
