module m_hurr
  implicit none
  private
  public :: hurr_loadmess
  public :: hurr_relocate

  interface hurr_loadmess; module procedure loadmess_; end interface
  interface hurr_relocate; module procedure relocate_; end interface

  integer,parameter :: IRX=41,JRX=41
  character(len=*),parameter :: myname='m_hurr'
#include "mytrace.H"
contains

subroutine loadmess_(itim,imax,jmax,glon,glat)
   implicit none
   integer,intent(in) :: itim
   integer,intent(in) :: imax,jmax
   real,intent(in) :: glon(IMAX)
   real,intent(in) :: glat(JMAX)
   character(len=*),parameter :: myname_=myname//'.loadmess_'
_ENTRY_
   CALL HURR_MESS(itim,IMAX,JMAX,GLON,GLAT)
#ifdef DEBUG_TRACE
   call show_(myname_)
#endif
_EXIT_
end subroutine loadmess_

subroutine relocate_(itim,imax,jmax,kmax,mtv,glon,glat, &
			aki,si,akl,sl,hdata,wdata,getptrs)

  use m_cutdm,only : cutdm_allstm
  use m_show,only : show,showlev,showstm
  use m_stmGrid,only : stmGrid_dump
  use m_hurrmess, only: KSTM,SLON_N,SLAT_N

  implicit none
  integer,intent(in) :: itim
  integer,intent(in) :: imax,jmax,kmax,mtv
  real,dimension(imax),intent(in) :: glon
  real,dimension(jmax),intent(in) :: glat
  real,dimension(kmax+1),intent(in) :: aki,si
  real,dimension(kmax  ),intent(in) :: akl,sl
  real,target,dimension(imax,jmax,mtv),intent(inout) :: HDATA
  real,target,dimension(imax,jmax,kmax,2),intent(inout) :: wDATA
  include "getptrs.h"

! local variables
  real,allocatable,dimension(:,:,:,:) :: HDAT
  real,allocatable,dimension(:,:) :: PSLB,glon2,glat2
  integer :: mtv2,mtv3
  character(len=*),parameter :: myname_=myname//'.relocate_'
_ENTRY_

  MTV2=KMAX*6+3
  MTV3=(2*KMAX+1)*6+3

  allocate(PSLB(imax,jmax))
  allocate(GLON2(imax,jmax))
  allocate(GLAT2(imax,jmax))
  allocate(HDAT(IRX,JRX,MTV2,KSTM))

#ifdef DEBUG_TRACE
   call show_(myname_)
#endif

  call cutdm_allstm(imax,jmax,glon,glat,kmax,MTV,HDATA,wdata,	&
     &	PSLB,IRX,JRX,MTV2,KSTM,SLON_N(1:KSTM),SLAT_N(1:KSTM),	&
     &  HDAT(:,:,:,1:KSTM))

#ifdef DEBUG_TRACE
      call showlev(PSLB,'pslb')
      call show(HDATA,'hdata')
      call showstm(HDAT(:,:,:,1:KSTM),'hdat')
      call stmGrid_dump(IRX,JRX,KMAX,MTV2,KSTM,			&
     &  SLON_N(1:KSTM),SLAT_N(1:KSTM),HDAT(:,:,:,1:KSTM))
#endif

      	! Relocate storm centers in HDATA
  call setglonxglat_(IMAX,JMAX,glon,glat,glon2,glat2)
  CALL HURR_REL(ITIM,IMAX,JMAX,KMAX,MTV,glon2,glat2,aki,SI,akl,SL, &
     &  PSLB,HDATA,IRX,JRX,MTV2,KSTM,HDAT,MTV3)

_SHOWX_('minval(PSL/main)', exp(minval(HDATA(:,:,2))) )
_SHOWX_('maxval(PSL/main)', exp(maxval(HDATA(:,:,2))) )
#ifdef DEBUG_TRACE
      call show(HDATA,'hdata_post')
#endif

	deallocate(HDAT)
	deallocate(PSLB)
	deallocate(GLON2)
	deallocate(GLAT2)
_EXIT_
end subroutine relocate_
subroutine setglonxglat_(imax,jmax,glon,glat,glon2,glat2)
  implicit none
  integer,intent(in) :: imax,jmax
  real,dimension(imax),intent(in) :: glon
  real,dimension(jmax),intent(in) :: glat
  real,dimension(imax,jmax),intent(out) :: glon2,glat2
  character(len=*),parameter :: myname_=myname//'.setglonxglat'
  integer :: j
_ENTRY_
  do j=1,size(glon2,2)
    glon2(:,j)=glon(:)
    glat2(:,j)=glat(j)
  end do
_EXIT_
end subroutine setglonxglat_
subroutine show_(name)
  use m_hurrmess, only: KSTM,ST_NAME,IC_N,JC_N,SLON_N,SLAT_N,TCVT
  implicit none
  character(len=*),intent(in) :: name
  integer :: k
  write(*,'(1x,2a,i3)') name,'(): storm tracks, KSTM =',KSTM
  do k=1,KSTM
    write(*,'(i3,2f7.2,2i5,1x,a,1x,a)') k,slon_n(k),slat_n(k), &
    	ic_n(k),jc_n(k),st_name(k),trim(tcvt(k))
  enddo
end subroutine show_
end module m_hurr
