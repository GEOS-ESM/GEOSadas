      module m_specGridIO

! m_specGridIO is a module of NCEP spectral grid data IO.  It supports
! only a single instance with both an input (I) leg and an output (O)
! leg.  A specGridIO object is expected to be []_open()'ed for input
! the []_create()'d for output, as in this usecase:
!
!! USEd interfaces
!	use m_[],only : []_open,[]_getdims,[]_getgrids
!	use m_[],only : []_read,[]_create,[]_write,[]_close
!	implicit none
!
!! Declarations
!	real,allocatable,dimension(:) :: glon,glat
!	real,allocatable,dimension(:) :: aki,si,akl,sl
!	real,allocatable,dimension(:,:,:) :: hdata
!	real,allocatable,dimension(:,:,:,:) :: wdata
!	integer :: ITIM			! File hour tag
!	integer :: IMAX,JMAX,KMAX	! grid dimensions
!	integer :: IUNIT,KUNIT		! I/O units for pre-connected
!					! file units.
!! Read control parameters
!	read(5,*) ITIM,IMAX,JMAX
!
!! Set pre-connected I/O units
!	IUNIT=19+ITIM/3
!	KUNIT=50+ITIM
!
!! The I/O process is now began.  The first file is copied to the
!! second file with the same grid and the same data (hopefully).
!	call []_open(ITIM,setimax=IMAX,setjmax=JMAX,unit=IUNIT)
!	call []_getdims(imax=IMAX,jmax=JMAX,kmax=KMAX)
!       allocate(GLON(IMAX ),GLAT(JMAX ))
!       allocate(AKI(KMAX+1), SI(KMAX+1))
!       allocate(AKL(KMAX  ), SL(KMAX  ))
!	allocate(HDATA(IMAX,JMAX,2+4*KMAX))
!	allocate(WDATA(IMAX,JMAX,KMAX,2))
!	call []_getgrids(glon=GLON,glat=GLAT,aki=AKI,si=SI, &
!					    akl=AKL,sl=SL)
!	call []_read(HDATA,WDATA)
!	call []_create(ITIM,unit=KUNIT)
!	call []_write(HDATA,WDATA)
!	call []_close()
!! All done.

      implicit none
      private
      
      public :: SPECGRID
      public :: specGridIO_open
      public :: specGridIO_getdims
      public :: specGridIO_getgrids
      public :: specGridIO_read
      public :: specGridIO_create
      public :: specGridIO_write
      public :: specGridIO_close

      interface specGridIO_open; module procedure open_; end interface
      interface specGridIO_read; module procedure read_; end interface
      interface specGridIO_getdims; module procedure
     &	getdims_; end interface
      interface specGridIO_getgrids; module procedure
     &	getgrids_; end interface
      interface specGridIO_create; module procedure
     &	create_; end interface
      interface specGridIO_write; module procedure write_; end interface
      interface specGridIO_close; module procedure close_; end interface

      integer,parameter :: SPECGRID = 0

      	! Synoptic time in hours of the day.
      integer,save :: ITIM_	! saved ITIM for verification

      	! NCEP spec-grid dimensions
      integer,save :: IMAX_,JMAX_
      REAL,   ALLOCATABLE,save :: GLAT_(:),GLON_(:)
      integer,save :: KMAX_
      REAL,   ALLOCATABLE,save :: AKI_(:),SI_(:)
      REAL,   ALLOCATABLE,save :: AKL_(:),SL_(:)

      integer,save :: IUNIT	! unit for read
      integer,save :: KUNIT	! unit for write

      	! spec-file header buffers
      CHARACTER*8,save :: LAB(4)
      integer  *4,save :: IDATE(4)
      REAL     *4,save :: FHOUR,DUMMY(245)

      	! spec grid data buffer
      integer,save :: MWAVE,MAXWV,MAXWV2
      integer,save :: MTV1,IKMAX
      integer,save :: ITRAC

      REAL*4,ALLOCATABLE,save :: SPEC(:,:)	! (MAXWV2,MTV1+IKMAX)

      character(len=*),parameter :: myname='m_specGridIO'
      logical,save :: initialized_=.false.

#include "assert.H"
#include "mytrace.H"
      contains
      subroutine open_(itim,setimax,setjmax,unit)
      use m_die,only : die,assert_
      implicit none
      integer,intent(in) :: itim	! synoptic time in 2-digit hours
      integer,intent(in) :: setimax,setjmax	! set imax and jmax
      integer,optional,intent(in) :: unit

      integer :: I,K
      integer :: MAXWV22
      character(len=*),parameter :: myname_=myname//'.open_'
      _ENTRY_

      if(initialized_) call die(myname_,'already initialized')
      initialized_=.true.

      IMAX_=setimax
      JMAX_=setjmax
      	ASSERT(mod(JMAX_,2)==0)

      ITIM_=ITIM
      IUNIT=19+ITIM/3
      if(present(unit)) IUNIT=unit

	! The input unit is always opened
      READ(IUNIT) LAB
      READ(IUNIT) FHOUR,(IDATE(I),I=1,4),DUMMY

      MWAVE=DUMMY(202)	! nint()?
      KMAX_=DUMMY(203)
      ITRAC=DUMMY(214)
      IKMAX=(ITRAC-2)*KMAX_
      MTV1 =KMAX_*5+2

!!      if(verbose) then
!!        WRITE(6,210) (IDATE(I),I=1,4),FHOUR,(DUMMY(K),K=1,2*KMAX_+1)
!!      else
        WRITE(6,210) (IDATE(I),I=1,4),FHOUR
!!      endif
210   FORMAT(5X,' INPUT DATE AND FCST HOUR ',4I5,F7.1/(2X,G13.6))

      MAXWV=(MWAVE+1)*(MWAVE+2)/2
      MAXWV2=2*MAXWV
      MAXWV22=MAXWV2+1

      ALLOCATE ( GLAT_(JMAX_ ),GLON_(IMAX_ ) )
      ALLOCATE ( AKI_(KMAX_+1), SI_(KMAX_+1) )
      ALLOCATE ( AKL_(KMAX_  ), SL_(KMAX_  ) )

      	! define a gaussian-latitude grid (GLAT_) and and
	! equal-interval longitude grid (GLON_).

      call setglonglat(IMAX_,JMAX_,GLON_,GLAT_)

      AKI_(1:KMAX_+1)=0.
       SI_(1:KMAX_+1)=DUMMY(1:KMAX_+1)

      K=KMAX_+1
      AKL_(1:KMAX_)=0.
       SL_(1:KMAX_)=DUMMY(K+1:K+KMAX_)

!      DO K=1,KMAX+1
!	AKI(k)=0.
!         SI(K)=DUMMY(K)
!      END DO
!      DO K=1,KMAX
!	AKL(k)=0.
!         SL(K)=DUMMY(KMAX+1+K)
!      END DO

      ALLOCATE ( SPEC(MAXWV2,MTV1+IKMAX) )
      _EXIT_
      end subroutine open_

      subroutine getdims_(imax,jmax,kmax)
	use m_die,only : die
	implicit none
	integer,optional,intent(out) :: imax,jmax,kmax
	character(len=*),parameter :: myname_=myname//'.getdims_'
      _ENTRY_
        if(.not.initialized_) call die(myname_,'uninitialized object')
	if(present(imax)) imax=IMAX_
	if(present(jmax)) jmax=JMAX_
	if(present(kmax)) kmax=KMAX_
      _EXIT_
      end subroutine getdims_

      subroutine getgrids_(glon,glat,aki,bki,si,akl,bkl,sl)
	use m_die,only : die
	implicit none
	real,optional,dimension(:),intent(out) :: glon,glat
	real,optional,dimension(:),intent(out) :: aki,bki,si,akl,bkl,sl
	character(len=*),parameter :: myname_=myname//'.getgrids_'
      _ENTRY_
        if(.not.initialized_) call die(myname_,'uninitialized object')

	if(present(glon)) glon(:)=glon_(:)
	if(present(glat)) glat(:)=glat_(:)

	if(present(aki)) aki(:)=aki_(:)
	if(present(bki)) bki(:)= si_(:)
	if(present( si))  si(:)= si_(:)

	if(present(akl)) akl(:)=akl_(:)
	if(present(bkl)) bkl(:)= sl_(:)
	if(present( sl))  sl(:)= sl_(:)
      _EXIT_
      end subroutine getgrids_

      subroutine read_(HDATA,wdata, getptrs)
      use m_die,only : die,assert_
      use m_show,only : show,showwav
      implicit none
      real,target,dimension(:,:,:),intent(out) :: HDATA
      real,target,dimension(:,:,:,:),intent(out) :: wdata
      include "getptrs.h"

	! workspace
      REAL,   ALLOCATABLE :: WORK_8(:)
      REAL,   ALLOCATABLE :: WK_S1(:,:),WK_S2(:,:),WK_G(:,:,:),
     1                       WK_G2(:,:,:),WK_G3(:,:,:),WK_G4(:,:,:)

      integer :: I,K,MAXWV22,JHF,MTV,NCNT,NW,IH
      character(len=*),parameter :: myname_=myname//'.read_'
      _ENTRY_
      if(.not.initialized_) call die(myname_,'uninitialized object')
      if(IUNIT<0) call die(myname_,'invalid IUNIT',IUNIT)

      MAXWV22=MAXWV2+1
      JHF=JMAX_/2
      MTV=KMAX_*4+2

      	ASSERT(MTV==size(HDATA,3))

      ALLOCATE ( WORK_8(MAXWV22) )
      ALLOCATE ( WK_S1(MAXWV2,KMAX_),WK_S2(MAXWV2,KMAX_) )
      ALLOCATE ( WK_G(IMAX_,JMAX_,KMAX_),WK_G2(IMAX_,JMAX_,KMAX_) )
      ALLOCATE ( WK_G3(IMAX_,JMAX_,KMAX_),WK_G4(IMAX_,JMAX_,KMAX_) )

! read z_sfc
      NCNT = 1
      READ(IUNIT) SPEC(:,NCNT)

      DO NW=1,MAXWV2
        WORK_8(NW) = SPEC(NW,NCNT)
      END DO
      WORK_8(MAXWV2+1)=0.

      ! store z_sfc
      call SPTEZ(0,MWAVE,4,IMAX_,JMAX_,WORK_8,HDATA(:,:,1),+1)

! read log_p_sfc, in log_kPa
      NCNT = NCNT + 1
      READ(IUNIT) (SPEC(NW,NCNT),NW=1,MAXWV2)

      DO NW=1,MAXWV2
        WORK_8(NW) = SPEC(NW,NCNT)
      END DO
      WORK_8(MAXWV2+1)=0.

      ! store log_p_sfc in log_kPa
      call SPTEZ(0,MWAVE,4,IMAX_,JMAX_,WORK_8,HDATA(:,:,2),+1)

! read Tv
      DO K=1,KMAX_
        NCNT = NCNT + 1
        READ(IUNIT) (SPEC(NW,NCNT),NW=1,MAXWV2)
        DO NW=1,MAXWV2
          WK_S1(NW,K) = SPEC(NW,NCNT)
        END DO
      END DO

      ! store Tv(1:KMAX)
      IH=2
      CALL SPTEZM(0,MWAVE,4,IMAX_,JMAX_,KMAX_,WK_S1,
     &  HDATA(:,:,IH+1:IH+KMAX_),+1)

      DO 230 K=1,KMAX_
      NCNT = NCNT + 1
      READ(IUNIT) (SPEC(NW,NCNT),NW=1,MAXWV2)

      DO NW=1,MAXWV2
        WK_S1(NW,K) = SPEC(NW,NCNT)
      END DO

      NCNT = NCNT + 1
      READ(IUNIT) (SPEC(NW,NCNT),NW=1,MAXWV2)

      DO NW=1,MAXWV2
        WK_S2(NW,K) = SPEC(NW,NCNT)
      END DO

230   CONTINUE

      CALL SPTEZM(0,MWAVE,4,IMAX_,JMAX_,KMAX_,WK_S1,WK_G,+1)
      CALL SPTEZM(0,MWAVE,4,IMAX_,JMAX_,KMAX_,WK_S2,WK_G2,+1)
      CALL SPTEZMV(0,MWAVE,4,IMAX_,JMAX_,KMAX_,WK_S1,WK_S2,
     &             WK_G3,WK_G4,+1)

      IH=2+KMAX_
      ! store divr(:kmax)
      HDATA(:,:,IH+1:IH+KMAX_+KMAX_:2)=WK_G(:,:,:)
      ! store vort(:kmax)
      HDATA(:,:,IH+2:IH+KMAX_+KMAX_:2)=WK_G2(:,:,:)
      ! store uwnd(:kmax)
      wdata(:,:,:,1)=WK_G3(:,:,:)
      ! store vwnd(:kmax)
      wdata(:,:,:,2)=WK_G4(:,:,:)

C.. CONVERT DI, ZE TO U,V and U,V TO DI ZE again for confirm

      DO 240 K=1,KMAX_
      NCNT = NCNT + 1
      READ(IUNIT) (SPEC(NW,NCNT),NW=1,MAXWV2)
      DO NW=1,MAXWV2
        WK_S1(NW,K) = SPEC(NW,NCNT)
      END DO
240   CONTINUE

      IH=2+KMAX_+2*KMAX_
      ! store q(:kmax)
      CALL SPTEZM(0,MWAVE,4,IMAX_,JMAX_,KMAX_,WK_S1,
     &  HDATA(:,:,IH+1:IH+KMAX_),+1)

      DO 270 K=1,KMAX_
      NCNT = NCNT + 1
      READ(IUNIT) (SPEC(NW,NCNT),NW=1,MAXWV2)
270   CONTINUE
c
      DO 280 K=1,IKMAX
      NCNT = NCNT + 1
      READ(IUNIT) (SPEC(NW,NCNT),NW=1,MAXWV2)
280   CONTINUE

      _SHOWX_('NCNT', NCNT)

      DEALLOCATE ( WORK_8, WK_S1, WK_S2, WK_G )
      DEALLOCATE ( WK_G2, WK_G3, WK_G4 )

      call show(HDATA,'hdata')
      call showwav(SPEC,'wave')

      _EXIT_
      end subroutine read_

      subroutine create_(ITIM,unit)
      use m_die,only : die,assert_
      implicit none
      integer,intent(in) :: ITIM
      integer,optional,intent(in) :: unit

      integer :: I
      character(len=*),parameter :: myname_=myname//'.create_'
      _ENTRY_
      if(.not.initialized_) call die(myname_,'uninitialized object')
      if(IUNIT<0) call die(myname_,'Unexpected IUNIT',IUNIT)
	! open_() must be called before create_().

	ASSERT(ITIM==ITIM_)

      KUNIT=50+ITIM
      if(present(unit)) KUNIT=unit

      WRITE(KUNIT) LAB
      print*,myname_,'(): LAB1=',LAB
      print*,myname_,'(): LAB2=',FHOUR,IDATE(1:4),DUMMY

      WRITE(KUNIT)FHOUR,(IDATE(I),I=1,4),DUMMY
      WRITE(6,210) (IDATE(I),I=1,4),FHOUR
210   FORMAT(5X,' INPUT DATE AND FCST HOUR ',4I5,F7.1/(2X,G13.6))
      _EXIT_
      end subroutine create_

      subroutine write_(HDATA,wdata,getptrs)
!--- Write physical space HDATA to spectral space file unit KUNIT.
      use m_die,only : die,assert_
      implicit none
			! gridded fields
      real,target,dimension(:,:,:),intent(in) :: HDATA
      real,target,dimension(:,:,:,:),intent(in) :: wdata
      include "getptrs.h"

! local variables
      integer :: I,J,K,NCNT2,NW
      integer :: MTV
      REAL(4),ALLOCATABLE :: WORK_3(:)
      REAL,   ALLOCATABLE :: WK_S1(:,:),WK_S2(:,:),WK_G(:,:,:)

      integer :: IH	! var-lev, the 3rd dimension index of HDATA
      character(len=*),parameter :: myname_=myname//'.write_'
      _ENTRY_

      if(.not.initialized_) call die(myname_,'uninitialized object')
      if(KUNIT<0) call die(myname_,'invalid KUNIT',KUNIT)

	! Configuring workspace variables.  In particular
      	!   size(skip2,1) == 2*((MWAVE+1)*(MWAVE+2)/2)	! size(skip2,1)
	!   size(skip2,2) == MTV1+IKMAX

      MTV=size(HDATA,3)
#ifdef DEBUG_TRACE
      print*,myname_,'(): KUNIT=',KUNIT
      print*,myname_,'(): IMAX,JMAX,KMAX=',IMAX_,JMAX_,KMAX_
      print*,myname_,'(): ITRAC,MTV=',ITRAC,MTV
      print*,myname_,'(): MWAVE,MAXWV2=',MWAVE,MAXWV2
      print*,myname_,'(): size(SKIP2,1)=',size(SPEC,1),MAXWV2,
     &					2*((MWAVE+1)*(MWAVE+2)/2)
      print*,myname_,'(): size(SKIP2,2)=',size(SPEC,2),
     &	MTV1+IKMAX,MTV1,IKMAX
#endif

! output z_sfc
      NCNT2 = 1
      WRITE(KUNIT)(SPEC(NW,NCNT2),NW=1,MAXWV2)

      ALLOCATE ( WORK_3(MAXWV2) )
      ALLOCATE ( WK_S1(MAXWV2,KMAX_),WK_S2(MAXWV2,KMAX_) )
      ALLOCATE ( WK_G(IMAX_,JMAX_,KMAX_) )

! output log(p_sfc) in log_kPa

#ifdef DEBUG_TRACE
      print*,myname_,'(): HDATA(PS) =',exp(minval(HDATA(:,:,2))),
     &			        exp(maxval(HDATA(:,:,2)))
#endif
      CALL G2SPC(KUNIT,MWAVE,IMAX_,JMAX_,HDATA(:,:,2))

      	! write spec of tv
      IH=2
#ifdef DEBUG_TRACE
      print*,myname_,'(): HDATA(TV) =',IH+1,IH+KMAX_
#endif
      CALL SPTEZM(0,MWAVE,4,IMAX_,JMAX_,KMAX_,WK_S1,
     &            HDATA(:,:,IH+1:IH+KMAX_),-1)

      DO K=1,KMAX_
        DO I=1,MAXWV2
          WORK_3(I)=WK_S1(I,K)
        END DO
        WRITE(KUNIT) (WORK_3(NW),NW=1,MAXWV2)
#ifdef DEBUG_TRACE
      print*,myname_,'(): ',K,sum(WORK_3(:))
#endif
      END DO

      	! write spec of (di,vo)
      IH=2+KMAX_
#ifdef DEBUG_TRACE
      print*,myname_,'(): HDATA(DI,ZE), ',IH+1,IH+2*KMAX_
#endif
      DO K=1,KMAX_
      DO J=1,JMAX_
      DO I=1,IMAX_
        WK_G(I,J,K)=HDATA(I,J,KMAX_+1+2*K)
      END DO
      END DO
      END DO

      CALL SPTEZM(0,MWAVE,4,IMAX_,JMAX_,KMAX_,WK_S1,
     &            WK_G,-1)

      DO K=1,KMAX_
      DO J=1,JMAX_
      DO I=1,IMAX_
        WK_G(I,J,K)=HDATA(I,J,KMAX_+2+2*K)
      END DO
      END DO
      END DO

      CALL SPTEZM(0,MWAVE,4,IMAX_,JMAX_,KMAX_,WK_S2,
     &            WK_G,-1)

      DO K=1,KMAX_
        DO I=1,MAXWV2
          WORK_3(I)=WK_S1(I,K)
        END DO
        WRITE(KUNIT) (WORK_3(NW),NW=1,MAXWV2)
#ifdef DEBUG_TRACE
      print*,'specgrid(_specwrite), divr=',sum(WORK_3(:))
#endif
        DO I=1,MAXWV2
          WORK_3(I)=WK_S2(I,K)
        END DO
        WRITE(KUNIT) (WORK_3(NW),NW=1,MAXWV2)
#ifdef DEBUG_TRACE
      print*,'specgrid(_specwrite), vort=',sum(WORK_3(:))
#endif
      END DO

      	! write spec of q
      IH=2+3*KMAX_
#ifdef DEBUG_TRACE
      print*,myname_,'(): HDATA, ',IH+1,IH+KMAX_
#endif
      CALL SPTEZM(0,MWAVE,4,IMAX_,JMAX_,KMAX_,WK_S1,
     &            HDATA(:,:,IH+1:IH+KMAX_),-1)

      DO K=1,KMAX_
        DO I=1,MAXWV2
          WORK_3(I)=WK_S1(I,K)
        END DO
        WRITE(KUNIT) (WORK_3(NW),NW=1,MAXWV2)
#ifdef DEBUG_TRACE
      print*,myname_,'specgrid(_specwrite), qwmr=',sum(WORK_3(:))
#endif
      END DO

      	! Write out additional fields on spectral grid for trace
	! variables (oz, etc.).

      NCNT2 = 2+4*KMAX_	! skip zs, ps, tv, (di,vo), and q.
#ifdef DEBUG_TRACE
      print*,myname_,'(): SKIP2, ',NCNT2+1,NCNT2+KMAX_
      print*,myname_,'(): SKIP2, ',2+4*KMAX_+1,2+4*KMAX_+KMAX_
#endif
      DO K=NCNT2+1,NCNT2+KMAX_	! write KMAX records of oz.
        WRITE(KUNIT)(SPEC(NW,K),NW=1,MAXWV2)
#ifdef DEBUG_TRACE
        print*,myname_,'(): ozon=',sum(SPEC(1:MAXWV2,K))
#endif
      END DO

      NCNT2 = NCNT2+KMAX_
#ifdef DEBUG_TRACE
      print*,myname_,'(): SKIP2, ',NCNT2+1,NCNT2+IKMAX
      print*,myname_,'(): SKIP2, ',2+5*KMAX_+1,2+5*KMAX_+IKMAX
#endif
      DO K=NCNT2+1,NCNT2+IKMAX	! write IKMAX reocrds of other traces.
        WRITE(KUNIT)(SPEC(NW,K),NW=1,MAXWV2)
#ifdef DEBUG_TRACE
        print*,myname_,'(): trac=',sum(SPEC(1:MAXWV2,K))
#endif
      END DO

      	! deallocate workspace variables.
      DEALLOCATE ( WORK_3 )
      DEALLOCATE ( WK_S1, WK_S2, WK_G )

#ifdef DEBUG_TRACE
      print*,myname_,'(): done.'
#endif
      RETURN
      _EXIT_
      END SUBROUTINE write_
      
      subroutine close_()
	use m_die,only : die
        implicit none

	character(len=*),parameter :: myname_=myname//'.close_'
      _ENTRY_
	if(.not.initialized_) call die(myname_,'uninitialized object')
	initialized_=.false.

	deallocate(SPEC)
	deallocate(AKI_,SI_)
	deallocate(AKL_,SL_)
	deallocate(GLON_,GLAT_)

	IMAX_=0
	JMAX_=0
	KMAX_=0
        ITIM_=-1

	if(IUNIT>0) close(IUNIT)
	if(KUNIT>0) close(KUNIT)
      _EXIT_
      end subroutine close_

      end module m_specGridIO
