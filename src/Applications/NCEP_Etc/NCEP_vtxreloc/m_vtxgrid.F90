module m_vtxgrid
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_vtxgrid
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2014-06-03
!
! abstract: vortex grid tools
!
! program history log:
!   2014-06-03  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

  implicit none
  private	! except
  public :: vtxgrid_nmiss       ! number of grid points missing the 40x40 grid mesh of observed storm
  public :: vtxgrid_lonDiff     ! lonDiff(a,b) := b-a, in degree angles, within the period of 360 considered.
  public :: vtxgrid_lonModulo   ! _lonModulo(xlon[, reflon]) := [reflon+]modulo(xlon[-reflon],360)

  public :: vtxgrid_lonNear     ! the nearest index location in an array, of the second argument.
                                ! > ilon=vtxgrid_lonIndex(glon(1:IMAX), clon)
  public :: vtxgrid_lonLower    ! the lower index location in an array, of the second argument.

  public :: vtxgrid_lonRange    ! lower-left and upper-right logitude corners of the modeled vortex grid,
                                ! about a given reference longitude point, clon_new
                                ! > call vtxgrid_lonRange(glon_min,glon_max, glon(ING(1:IB),JNG(1:IB)), reflon=clon_new)
                                ! > glat_min = minval( glat(ING(1:IB),JNG(1:IB)) )
                                ! > glat_max = maxval( glat(ING(1:IB),JNG(1:IB)) )

  real,parameter:: DEG_in_RAD=4.*atan(1.)/180.
  real,parameter:: RAD_in_DEG=1./DEG_IN_RAD
  real,parameter:: R360DEG   =360.

!#include "assert.H"

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='m_vtxgrid'

contains
function vtxgrid_lonDiff(clon,glon) result(dlon)
        ! lonDiff = dlon := (glon .minus. clon + 180.) .modulo. 360. - 180.)
        ! Note: it is NOT clon.minus.glon, BUT glon.minus.clon.
  implicit none
  real:: dlon   ! defined in range [-180.,180.].  The value at the end point is
                ! either -180 or +180, depending on the value of glon relative
                ! to clon.
  real,intent(in):: clon,glon

  real(kind=kind(dlon)):: rad_clon,cos_clon,sin_clon
  real(kind=kind(dlon)):: rad_glon,cos_glon,sin_glon

  !print'(a,2f10.3)','DEG_in_RAD=',DEG_in_RAD,3.1415926/180.
  !print'(a,2f10.3)','RAD_in_DEG=',RAD_in_DEG,180.00/3.1415926

  rad_clon=clon *DEG_in_RAD
  cos_clon=cos(rad_clon)
  sin_clon=sin(rad_clon)

  rad_glon = glon *DEG_in_RAD
  cos_glon = cos(rad_glon)
  sin_glon = sin(rad_glon)

  ! dlon = glon - clon, in a trignomitry based algorithm.

  dlon = atan2( cos_clon*sin_glon - sin_clon*cos_glon,  &   ! sin(g-c) = sin(g)*cos(c)-cos(g)*sin(c)
                cos_clon*cos_glon + sin_clon*sin_glon)      ! cos(g-c) = cos(g)*cos(c)+sin(g)*sin(c)
  dlon = dlon *RAD_in_DEG                                   ! d := "g-c" = atan2(sin(g-c),cos(g-c))
end function vtxgrid_lonDiff

function vtxgrid_lonNear(glon,xlon) result(ilon)
  implicit none
  integer:: ilon    ! xlon location (the nearst) in glon(:)
  real,dimension(:),intent(in   ):: glon        ! longitude values
  real,             intent(in   ):: xlon        ! longitude values

  integer:: i,n
  real(kind=kind(xlon)):: dlon,vlon

  ilon=0
  n=size(glon)
  if(n<1) return

  dlon=abs(vtxgrid_lonDiff(xlon,glon(1)))
  vlon=dlon
  ilon=1
  do i=2,n
    dlon=abs(vtxgrid_lonDiff(xlon,glon(i)))
    if(vlon>dlon) then
      vlon=dlon
      ilon=i
    endif
  enddo
end function vtxgrid_lonNear

function vtxgrid_lonLower(glon,xlon) result(ilon)
  implicit none
  integer:: ilon    ! xlon location (lower to the left) in glon(:)
  real,dimension(:),intent(in   ):: glon        ! longitude values
  real,             intent(in   ):: xlon        ! longitude values

  integer:: i,n
  real(kind=kind(xlon)):: dlon,zlon

  ilon=0
  n=size(glon)
  if(n<1) return

  if(glon(n)>glon(1)) then
    zlon=glon(1)+modulo(xlon-glon(1),R360DEG)
    ilon=n
    do i=1,n
      dlon=zlon-glon(i)
      if(dlon<0) exit
      ilon=i
    enddo
  else
    zlon=glon(1)-modulo(glon(1)-xlon,R360DEG)
    ilon=n
    do i=1,n
      dlon=zlon-glon(i)
      if(dlon>0) exit
      ilon=i
    enddo
  endif
end function vtxgrid_lonLower

function vtxgrid_lonModulo(alon, reflon) result(zlon)
  implicit none
  real :: zlon        ! 
  real,intent(in):: alon        ! longitude values
  real,optional,intent(in):: reflon      ! a reference longitude location

  zlon=0.
  if(present(reflon)) zlon=reflon

  zlon = zlon + modulo(alon-zlon, R360DEG)
end function vtxgrid_lonModulo

subroutine vtxgrid_lonRange(glon_min,glon_max, glon, reflon)
  implicit none
  real,             intent(  out):: glon_min    ! lower-left corner
  real,             intent(  out):: glon_max    ! upper_right corner
  real,dimension(:),intent(in   ):: glon        ! longitude values
  real,optional    ,intent(in   ):: reflon      ! a reference longitude location

  integer:: i,n
  real(kind=kind(glon_min)):: xlon,rlon

  glon_min=0.
  glon_max=0.
  n=size(glon)
  if(n<1) return

  rlon=glon(1)          ! use glon(1) to ensure the range of glon(:) is
                        ! about itself.
  if(present(reflon)) rlon=reflon

  xlon = rlon + vtxgrid_lonDiff(rlon,glon(1))
  glon_min=xlon
  glon_max=xlon

  do i=2,n
    xlon = rlon + vtxgrid_lonDiff(rlon,glon(i))
    if(xlon<glon_min) glon_min=xlon
    if(xlon>glon_max) glon_max=xlon
  enddo
end subroutine vtxgrid_lonRange

function vtxgrid_nmiss(glat,glon, alat,alon) result(imiss)
  implicit none
  integer:: imiss
  real,dimension(:),intent(in):: glat,glon
  real,dimension(:),intent(in):: alat
  real,dimension(:),intent(in):: alon

  integer:: IB,IX,JX,I

  imiss=0

  IB=size(glon)
       ! ASSERT(size(glat)==size(glon))

  JX=size(alat)
  IX=size(alon)

  do I = 1,IB
    if( vtxgrid_lonDiff(ALON( 1),GLON(I))<0. .or. GLAT(I)-ALAT( 1)<0. .or. &
        vtxgrid_lonDiff(ALON(IX),GLON(I))>0. .or. GLAT(I)-ALAT(JX)>0. ) imiss=imiss+1
  enddo
end function vtxgrid_nmiss
end module m_vtxgrid
