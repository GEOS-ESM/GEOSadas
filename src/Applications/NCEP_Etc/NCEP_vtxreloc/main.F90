program main
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: RELOCATE_MV_NVORTEX_t254l64
!   PROGMMR: Dennis Keyser  ORG: NP22       DATE: 2000-07-20
!
! ABSTRACT: RELOCATES HURRICANE VORTEX IN GLOBAL MODEL.
!   THIS PROGRAM CONTAINS THE FOLLOWING STEPS:
!   1) CONVERTS THE GLOBAL SPECTRAL COEFS TO GAUSSIAN GRID
!      AND DEFINES A 40x40 DEG AREAS AROUND THE REPORTED HURRICANES.
!   2) USING GFDL PROCEDURE SEPARATES THE HURRICANE DISTURBANCE FROM
!      THE ENVIRONMENTAL FIELD AND MOVE THE HURRICANE DISTURBANCE TO
!      THE OBSERVATIONAL LOCATION.
!   3) CONVERTS THE GAUSSIAN GRID TO GLOBAL SPECTRAL COEFS.
!
! PROGRAM HISTORY LOG:
! 2000-04-25  QINGFU LIU
! 2000-06-14  DENNIS KEYSER -- ADDED CALLS TO W3TAGB AND W3TAGE
!        AND CALLS TO ERREXIT FOR NON-ZERO STOP CONDITIONS.
! 2006-07-12  JOE STASSI -- ADDED MAXERR AND ERRCNT TO SUBR HURR_MESS
!        TO PREVENT INFINITE LOOP ON INPUT FILE READ ERROR.
! 2006-08-09  Jing Guo -- Added "END SUBROUTINE xxxx" to all routines.
!             Plan to add more maintenance features, such as, "IMPLICIT
!             NONE", etc.
!
! USAGE:
!   INPUT FILES:
!
!     UNIT 11    THE CURRENT TC VITAL FILE
!     UNIT 20    THE SIGMA FILE AT TIME t-3
!     UNIT 21    THE SIGMA FILE AT (CURRENT) TIME t
!     UNIT 22    THE SIGMA FILE AT TIME t+3
!     UNIT 30    MODEL VORTEX CENTER LOCATION AT TIME t-3,t,t+3
!
!   SUBPROGRAMS CALLED:
!     UNIQUE     - modules     BOUND_QLIU  fft99      sig_p_convt1
!                  SEPAR_QLIU  WNLIT       FDUMP      H12
!                  I1MACH      J4SAVE      XGETUA     WNLSM
!                  WNNLS       XERABT      XERCTL     XERPRT
!                  XERROR      XERRWV      XERSAV     srotm
!                  srotmg      rodist_qliu amatrix_qliu
!     LIBRARY:
!       W3LIB    - W3TAGB      W3TAGE      ERREXIT
!
!
!   EXIT STATES:
!     COND =  0 - SUCCESSFUL RUN
!     COND = 56 - NO TC VITAL DATA (OR TC VITAL IS EMPTY)
!
! REMARKS: NONE.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM-SP
!
!$$$
!
!234567890123456789012345678901234567890123456789012345678901234567890

  use m_Hdata,only : Hdata_init
  use m_Hdata,only : Hdata_clean
  use m_Hdata,only : HDATA_getptrs => getptrs_
  use m_Hdata,only : GLON,GLAT
  use m_Hdata,only : AKI,SI,AKL,SL
  use m_Hdata,only : HDATA,wdata

  use m_specGridIO,only : SPECGRID
  use m_specGridIO,only : specGridIO_open
  use m_specGridIO,only : specGridIO_getdims
  use m_specGridIO,only : specGridIO_getgrids
  use m_specGridIO,only : specGridIO_read
  use m_specGridIO,only : specGridIO_create
  use m_specGridIO,only : specGridIO_write
  use m_specGridIO,only : specGridIO_close

  use m_geosGridIO,only : GEOSGRID
  use m_geosGridIO,only : geosGridIO_open
  use m_geosGridIO,only : geosGridIO_getdims
  use m_geosGridIO,only : geosGridIO_getgrids
  use m_geosGridIO,only : geosGridIO_read
  use m_geosGridIO,only : geosGridIO_write
  use m_geosGridIO,only : geosGridIO_close

  use m_hurr,only : HURR_loadmess
  use m_hurr,only : HURR_relocate

  use m_die,only : die

#include "mytrace.H"
  implicit none
  integer :: itim,imax,jmax,kmax,ioClass
  integer :: IUNIT,KUNIT
  character(len=4) :: fnum

  character(len=*),parameter :: myname='main'
  character(len=*),parameter :: myname_=myname
_ENTRY_

  ioClass=SPECGRID
  ioClass=GEOSGRID
  read(5,*) ioClass,itim,imax,jmax
  IUNIT=19+ITIM/3		! input unit
  KUNIT=50+ITIM			! output unit

#ifdef DEBUG_TRACE
  print*,myname_,': ITIM,IMAX,JMAX,ioClass =',ITIM,IMAX,JMAX,ioClass
  print*,myname_,': IUNIT,KUNIT =',IUNIT,KUNIT
#endif

_TRACE_("reading")
  select case(ioClass)
  case(SPECGRID)
    call specGridIO_open(itim,setimax=imax,setjmax=jmax,unit=IUNIT)
    call specGridIO_getdims(imax=imax,jmax=jmax,kmax=kmax)
    call Hdata_init(imax,jmax,kmax)
    call specGridIO_getgrids(	glon=glon,glat=glat, &
				aki=aki,si=si,akl=akl,sl=sl)
    call specGridIO_read( HDATA,wdata, HDATA_getptrs )

  case(GEOSGRID)
    
    write(fnum,'(i4)') IUNIT
    call geosGridIO_open('fort.'//trim(adjustl(fnum)),swapgrid=.true.)
    call geosGridIO_getdims(imax=imax,jmax=jmax,kmax=kmax)
    call Hdata_init(imax,jmax,kmax)
    call geosGridIO_getgrids(	glon=glon,glat=glat, &
				aki=aki,bki=si,akl=akl,bkl=sl)
    call geosGridIO_read( HDATA,wdata, HDATA_getptrs)

  case default
    call die(myname,'unknown ioClass',ioClass)
  end select

_TRACE_("loadmess()")
	! Get hurrican messages
  CALL HURR_loadMESS(ITIM,IMAX,JMAX,GLON,GLAT)

_TRACE_("relocate()")
!!#define _NODELTA_
#ifndef _NODELTA_
      	! Relocate storm centers in (HDATA,wdata)
  CALL HURR_RELocate(ITIM,IMAX,JMAX,KMAX,size(HDATA,3), &
	GLON,GLAT,AKI,SI,AKL,SL,HDATA,wdata,HDATA_getptrs)
#endif

_TRACE_("writing()")
  select case(ioClass)
  case(SPECGRID)
    call specGridIO_create(itim,unit=KUNIT)
    call specGridIO_write(HDATA,wdata, HDATA_getptrs )
    call specGridIO_close()

  case(GEOSGRID)
    write(fnum,'(i4)') KUNIT
    call geosGridIO_write('fort.'//trim(adjustl(fnum)), &
      HDATA,wdata, HDATA_getptrs)
    call geosGridIO_close()

  end select

  call Hdata_clean()
_EXIT_
end program main
