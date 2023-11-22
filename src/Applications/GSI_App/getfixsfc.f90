program getfixsfc
! This program converts the relevant data from the NCEP surface file
! to netcdf - this can then be converted to a convenient cubed-grid
! resolution to serve as input data to JEDI. 
! This program only cares about three fields:
!  vegetation fraction; vegetation type and surface type
! These are typically needed by CRTM.
! GEOS has an untested implementation in the land model that
! can produce two of these fields; the third one (surface type)
! is wired to CRMT types so it is hard for GEOS to come up with.
! This info should really come from the other sources.
! 
! To complete the connect with JEDI-CRTM the following must be 
! done at script level:
!
! - mpirun -np 6 Regrid_Util.x -i ncepsfc.nc4 -o crtmsfc_c90.nc4 \
!                              -ogrid PE90x540-CF -method "VOTE"
! - reset_time.x crtmsfc_c90.nc4 nymd nhms -9
! 
! The date parameters above should correspond to the time in the 
! backgrounds of JEDI; the final file, crtmsfc_c90.nc4, should be
! ready for use in JEDI.
!
! Todling, 13Apr2023
use sfcio_module
use m_gsifixsfc
implicit none
  character(len=*), parameter :: myname = 'getfixsfc'
  type(sfcio_head):: head
  type(sfcio_data):: data
  real(8),allocatable :: lats(:),lons(:),levs(:)
  type(gsifixsfc_vars) vr
  integer nlat,nlon,nlev,nymd,nhms,rc
  real(8) dlat,dlon
  logical, parameter :: fake3dfile=.false.

! Read in from original NCEP file
  call sfcio_srohdc(34,'ncepsfc',head,data,rc)
  print *,myname,': Using NCEP surface file, version: ', head%ivs
  nymd = head%idate(4)*10000+head%idate(2)*100+head%idate(3)
  nhms = head%idate(1)*10000

  nlon=head%lonb
  nlat=head%latb!+2
  if(fake3dfile) then
    nlev=72
  else
    nlev=1
  endif
  call gsifixsfc_vars_init(vr,nlon,nlat,nlev)

! add poles
  call SP2NP_(vr%vfrac,data%vfrac)
  call SP2NP_(vr%vtype,data%vtype)
  call SP2NP_(vr%stype,data%stype)
! flip longitudes to GEOS complaint
  call hflip_(vr%vfrac)
  call hflip_(vr%vtype)
  call hflip_(vr%stype)

! call wrtgrads_

  allocate(lats(nlat),lons(nlon),levs(nlev))
  call get_coords_
  if(nlev>1) then
    call gsifixsfc_write ('ncepsfc.nc4',vr,lats,lons,rc,levs=levs,nymd=nymd,nhms=nhms)
  else
    call gsifixsfc_write ('ncepsfc.nc4',vr,lats,lons,rc,nymd=nymd,nhms=nhms)
  endif
  deallocate(lats,lons,levs)
contains

  subroutine SP2NP_(rbufr,sfcin)
!-------------------------------------------------------------------------
! NCEP native Gaussian gridded fields have latitudes reversed and the poles
! excluded. This routine reverses latitudes and adds poles.
  implicit none
  real(4),dimension(:,:),intent(inout) :: rbufr
  real,dimension(:,:),intent(in ) :: sfcin
  integer :: im,jm,j
  character(len=*), parameter :: IAm='SP2NP_'

  im=size(rbufr,1)
  jm=size(rbufr,2)
  if(jm>size(sfcin,2))then
     rbufr(:, 1)=sum(sfcin(:,jm-2))/im  ! add South Pole points
  else
     rbufr(:, 1)=sfcin(:,jm)            ! add South Pole points
  endif
  do j=2,jm-1
                                ! for j :=    2,   3, ..., jm-2,jm-1
                                !  jm-j == jm-2,jm-3, ...,    2,   1
    rbufr(:,j)=sfcin(:,jm-j)
  end do
  if(jm>size(sfcin,2))then
     rbufr(:,jm)=sum(sfcin(:,   1))/im  ! add North Pole points
  else
     rbufr(:,jm)=sfcin(:,   1)          ! add North Pole points
  endif
  end subroutine SP2NP_

  subroutine wrtgrads_
     integer lu
     real(4), allocatable :: tmp(:,:)

     lu=10
     allocate(tmp(nlon,nlat))
     call baopenwt(lu,'ncepsfc.grd',rc)
     tmp=vr%vfrac; call wryte(lu,4*nlat*nlon,tmp)
     tmp=vr%vtype; call wryte(lu,4*nlat*nlon,tmp)
     tmp=vr%stype; call wryte(lu,4*nlat*nlon,tmp)
     call baclose(lu,rc)
     deallocate(tmp)

     lu=11
     open(lu,file='ncepsfc.ctl',form='formatted')
     write(lu,'(2a)') 'DSET  ^', 'ncepsfc.grd'
     write(lu,'(2a)') 'TITLE ', 'gsi fixed sfc fields'
     write(lu,'(a,2x,e13.6)') 'UNDEF', 1.E+15 ! any other preference for this?
     write(lu,'(a,2x,i4,2x,a,2x,f5.1,2x,f9.6)') 'XDEF',nlon, 'LINEAR', -180.0, 360./nlon
     write(lu,'(a,2x,i4,2x,a,2x,f5.1,2x,f9.6)') 'YDEF',nlat, 'LINEAR',  -90.0, 180./nlat
     write(lu,'(a,2x,i4,2x,a,100(1x,f10.5))')      'ZDEF', 1, 'LEVELS', 1000.0
     write(lu,'(a,2x,i4,2x,a)')   'TDEF', 1, 'LINEAR 12:00Z04JUL1776 6hr' ! any date suffices
     write(lu,'(a,2x,i4)')        'VARS', 3
     write(lu,'(a,1x,2(i4,1x),a)') 'vfrac', 1,0, 'vegetation fraction'
     write(lu,'(a,1x,2(i4,1x),a)') 'vtype', 1,0, 'vegetation type'
     write(lu,'(a,1x,2(i4,1x),a)') 'stype', 1,0, 'soil type'
     write(lu,'(a)') 'ENDVARS'
     close(lu)
 end subroutine wrtgrads_

 subroutine hflip_ ( q )
   implicit none
   integer  im,jm,i,j
   real, intent(inout) :: q(:,:)
   real, allocatable   :: dum(:)
   im=size(q,1)
   jm=size(q,2)
   allocate ( dum(im) )
   do j=1,jm
   do i=1,im/2
      dum(i) = q(i+im/2,j)
      dum(i+im/2) = q(i,j)
   enddo
      q(:,j) = dum(:)
   enddo
   deallocate ( dum )
 end subroutine hflip_

 subroutine get_coords_
  integer i

  levs(1) = 1000.0 ! hPa -- irrelevant
  dlat = 180./nlat
  do i=1,nlat ! should be guassian lats
     lats(i) = -90.0 + (i-1)*dlat 
  enddo
  dlon = 360./nlon
  do i=1,nlon
     lons(i) = -180.0 + (i-1)*dlon 
  enddo
  do i = 1,nlev
     levs(i) = float(i)
  enddo

 end subroutine get_coords_

end program getfixsfc
