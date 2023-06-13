module m_gsifixsfc
use netcdf
implicit none
private

public :: gsifixsfc_vars_init
public :: gsifixsfc_vars_final
public :: gsifixsfc_vars_comp
public :: gsifixsfc_vars_copy
public :: gsifixsfc_vars
public :: gsifixsfc_dims
public :: gsifixsfc_read
public :: gsifixsfc_write
public :: gsifixsfc_getpointer

type gsifixsfc_vars
   logical :: initialized=.false.
   integer :: nlon,nlat,nlev
   real(4),pointer,dimension(:,:)   :: vfrac
   real(4),pointer,dimension(:,:)   :: vtype
   real(4),pointer,dimension(:,:)   :: stype
   real(4),pointer,dimension(:,:,:) :: dummy
   real(4),pointer,dimension(:,:)   :: v2d
   real(4),pointer,dimension(:,:,:) :: v3d
end type gsifixsfc_vars

character(len=*), parameter :: myname = 'm_gsifixsfc'

real, parameter :: undef = 1.0e15
integer, parameter :: nv2d = 3
character(len=5),parameter :: cvars2d(nv2d) = (/ 'vfrac', 'vtype', 'stype' /)
integer, parameter :: nv3d = 1
character(len=5),parameter :: cvars3d(nv3d) = (/ 'dummy' /)

interface gsifixsfc_dims; module procedure    &
  read_dims_ ; end interface
interface gsifixsfc_read; module procedure    &
  read_ ; end interface
interface gsifixsfc_write; module procedure    &
  write_ ; end interface
interface gsifixsfc_vars_init; module procedure    &
  init_vars_ ; end interface
interface gsifixsfc_vars_final; module procedure    &
  final_vars_ ; end interface
interface gsifixsfc_vars_comp; module procedure    &
  comp_vars_ ; end interface
interface gsifixsfc_vars_copy; module procedure    &
  copy_ ; end interface
interface gsifixsfc_getpointer 
  module procedure get_pointer_2d_
end interface

contains

subroutine read_dims_ (fname,nlat,nlon,nlev,rc, myid,root)
  implicit none
  character(len=*), intent(in)    :: fname ! input filename
  integer, intent(out) :: rc
  integer, intent(out) :: nlat,nlon,nlev
  integer, intent(in), optional :: myid, root

! This will be the netCDF ID for the file and data variable.
  integer :: ncid, varid, ier
  integer :: mype_,root_

! Local variables
  character(len=*), parameter :: myname_ = myname//"::dims_"
  logical :: verbose
   
! Return code (status)
  rc=0; mype_=0; root_=0
  if(present(myid) .and. present(root) ) then
     mype_ = myid
     root_ = root
  endif
 
! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
! the file.

  call check_( nf90_open(fname, NF90_NOWRITE, ncid), rc, mype_, root_ )
  if(rc/=0) return

! Read global attributes
  call check_( nf90_inq_dimid(ncid, "lon", varid), rc, mype_, root_)
  call check_( nf90_inquire_dimension(ncid, varid, len=nlon), rc, mype_, root_ )
  call check_( nf90_inq_dimid(ncid, "lat", varid), rc, mype_, root_ )
  call check_( nf90_inquire_dimension(ncid, varid, len=nlat), rc, mype_, root_ )
  nlev=1
! call check_( nf90_inq_dimid(ncid, "lev", varid), rc, mype_, root_ )
! call check_( nf90_inquire_dimension(ncid, varid, len=nlev), rc, mype_, root_ )

! Close the file, freeing all resources.
  call check_( nf90_close(ncid), rc, mype_, root_ )

  return

end subroutine read_dims_

subroutine read_ (fname,bvars,rc, myid,root)
  implicit none
  character(len=*), intent(in)    :: fname ! input filename
  type(gsifixsfc_vars),intent(inout) :: bvars ! background error variables
  integer, intent(out) :: rc
  integer, intent(in), optional :: myid,root ! accommodate MPI calling programs

! This will be the netCDF ID for the file and data variable.
  integer :: ncid, varid

! Local variables
  character(len=*), parameter :: myname_ = myname//"::read_"
  character(len=4) :: cindx
  integer :: nv,nl,nlat,nlon,nlev
  integer :: ndims_, nvars_, ngatts_, unlimdimid_
  integer :: nlat_,nlon_,nlev_
  integer :: mype_,root_
  real(4), allocatable :: data_in(:,:,:)
  logical :: verbose
  logical :: init_
  
   
! Return code (status)
  rc=0; mype_=0; root_=0
  verbose=.true.
  init_=.false.
  if(present(myid).and.present(root) )then
    if(myid/=root) verbose=.false.
    mype_ = myid
    root_ = root
  endif
 
! Get dimensions
  call read_dims_ (fname,nlat_,nlon_,nlev_,rc, mype_,root_)

  init_ = bvars%initialized
  if ( init_ ) then
!   Set dims
    nlat=bvars%nlat
    nlon=bvars%nlon
    nlev=bvars%nlev

!   Consistency check
!   if (nlon_ /= nlon .or. nlat_ /=nlat .or. nlev_/=nlev ) then
    if (nlon_ /= nlon .or. nlat_ /=nlat ) then
       rc=1
       if(myid==root) then
         print *, 'nlat(file) = ', nlat_, 'nlat(required) = ', nlat
         print *, 'nlon(file) = ', nlon_, 'nlon(required) = ', nlon
         print *, 'nlev(file) = ', nlev_, 'nlev(required) = ', nlev
         print *, myname_,  'Inconsistent dimensions, aborting ... '
       endif
       return
    endif
  else
!   Set dims
    nlat=nlat_
    nlon=nlon_
    nlev=nlev_
    call init_vars_(bvars,nlon,nlat,nlev)
  endif

! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
! the file.

  call check_( nf90_open(fname, NF90_NOWRITE, ncid), rc, mype_, root_ )
  if(rc/=0) return

! Read global attributes
! call check_( nf90_inquire(ncid, ndims_, nvars_, ngatts_, unlimdimid_), rc, mype_, root_ )
! call check_( nf90_inq_dimid(ncid, "lon", varid), rc, mype_, root_ )
! call check_( nf90_inquire_dimension(ncid, varid, len=nlon_), rc, mype_, root_ )
! call check_( nf90_inq_dimid(ncid, "lat", varid), rc, mype_, root_ )
! call check_( nf90_inquire_dimension(ncid, varid, len=nlat_), rc, mype_, root_ )
! call check_( nf90_inq_dimid(ncid, "lev", varid), rc, mype_, root_ )
! call check_( nf90_inquire_dimension(ncid, varid, len=nlev_), rc, mype_, root_ )

! Write out lat/lon fields
  allocate(data_in(nlon,nlat,1))
  do nv = 1, nv2d
     call check_( nf90_inq_varid(ncid, trim(cvars2d(nv)), varid), rc, mype_, root_ )
     call check_( nf90_get_var(ncid, varid, data_in(:,:,1)), rc, mype_, root_ )
     if(trim(cvars2d(nv))=="vfrac" ) then
        bvars%vfrac = data_in(:,:,1)
     endif
     if(trim(cvars2d(nv))=="vtype") then 
        bvars%vtype = data_in(:,:,1)
     endif
     if(trim(cvars2d(nv))=="stype") then 
        bvars%stype = data_in(:,:,1)
     endif
  enddo
  deallocate(data_in)

  if(nlev>1)then
    allocate(data_in(nlon,nlat,nlev))
    do nv = 1, nv3d
       call check_( nf90_inq_varid(ncid, trim(cvars3d(nv)), varid), rc, mype_, root_ )
       call check_( nf90_get_var(ncid, varid, data_in(:,:,:)), rc, mype_, root_ )
       if(trim(cvars3d(nv))=="dummy" ) then
          bvars%dummy = data_in(:,:,:)
       endif
    enddo
    deallocate(data_in)
  endif

! Close the file, freeing all resources.
  call check_( nf90_close(ncid), rc, mype_, root_ )

  if(verbose) print *,"*** Finish reading file: ", trim(fname)

  return

end subroutine read_

subroutine write_ (fname,bvars,lats,lons,rc, levs,nymd,nhms,myid,root)
  implicit none
  character(len=*), intent(in)    :: fname ! input filename
  type(gsifixsfc_vars),intent(in)    :: bvars ! background error variables
  real(8), intent(in) :: lats(:)           ! latitudes  per GSI: increase index from South to North Pole
  real(8), intent(in) :: lons(:)           ! longitudea per GSI: increase index from East to West
  integer, intent(out) :: rc
  real(8), intent(in), optional :: levs(:)
  integer, intent(in), optional :: nymd,nhms   ! date/time
  integer, intent(in), optional :: myid,root   ! accommodate MPI calling programs

  character(len=*), parameter :: myname_ = myname//"::read_"
  integer :: NDIMS

! When we create netCDF files, variables and dimensions, we get back
! an ID for each one.
  character(len=4) :: cindx
  character(len=50):: timunits
  character(len=256):: long_name
  integer :: ncid
  integer :: x_dimid, y_dimid, z_dimid, t_dimid
  integer :: lon_varid, lat_varid, lev_varid, tim_varid, varid
  integer :: ii,jj,nl,nv,nn,nlat,nlon,nlev
  integer :: mype_,root_
  integer :: nymd_,nhms_,ntinc
  integer, allocatable :: dimids(:), start(:)
  integer, allocatable :: varid2d(:),varid3d(:)
  logical :: verbose
  
! This is the data array we will write. It will just be filled with
! a progression of integers for this example.
  real(4), allocatable :: data_out(:,:,:)

! Return code (status)
  rc=0; mype_=0; root_=0
  verbose=.true.
  if(present(myid).and.present(root) )then
    if(myid/=root) verbose=.false.
    mype_ = myid
    root_ = root
  endif

  if (present(levs) ) then
    NDIMS = 4
  else
    NDIMS = 3
  endif
  allocate(dimids(NDIMS), start(NDIMS))

! Date/time
  if ( present(nymd) .and. present(nhms) ) then
    nymd_ = nymd
    nhms_ = nhms
    write(timunits,'(a,i4.4,a,i2.2,a,i2.2,a,i2.2,a)') &
          "minutes since ", &
          nymd_/10000, '-', mod(nymd_,10000)/100, '-', mod(nymd_,100), &
          " ", nhms_/10000, ":00:00"
  else
    nymd_ = 17760704
    nhms_ = 210000
    timunits = "minutes since 1776-07-04 21:00:00"
  endif
  print *, trim(timunits)
  ntinc = 10000

! Set dims
  nlat=bvars%nlat
  nlon=bvars%nlon
  nlev=bvars%nlev

! Always check the return code of every netCDF function call. In
! this example program, wrapping netCDF calls with "call check()"
! makes sure that any return which is not equal to nf90_noerr (0)
! will print a netCDF error message and exit.

! Create the netCDF file. The nf90_clobber parameter tells netCDF to
! overwrite this file, if it already exists.
  call check_( nf90_create(fname, NF90_CLOBBER, ncid), rc, mype_, root_ )
  if(rc/=0) return

! Define the dimensions. NetCDF will hand back an ID for each. 
  call check_( nf90_def_dim(ncid, "lat", nlat, y_dimid), rc, mype_, root_ )
  call check_( nf90_def_dim(ncid, "lon", nlon, x_dimid), rc, mype_, root_ )
  if(present(levs)) then
    call check_( nf90_def_dim(ncid, "lev", nlev, z_dimid), rc, mype_, root_ )
  endif
  call check_( nf90_def_dim(ncid, "time", nf90_unlimited, t_dimid), rc, mype_, root_ )

  call check_( nf90_def_var(ncid, "lon", NF90_DOUBLE, x_dimid, lon_varid), rc, mype_, root_ )
  call check_( nf90_def_var(ncid, "lat", NF90_DOUBLE, y_dimid, lat_varid), rc, mype_, root_ )
  if(present(levs)) then
    call check_( nf90_def_var(ncid, "lev", NF90_DOUBLE, z_dimid, lev_varid), rc, mype_, root_ )
  endif
  call check_( nf90_def_var(ncid, "time",NF90_INT,    t_dimid, tim_varid), rc, mype_, root_ )

  call check_( nf90_put_att(ncid, lat_varid, "long_name", "latitude"      ), rc, mype_, root_ )
  call check_( nf90_put_att(ncid, lat_varid, "units"    , "degrees_north" ), rc, mype_, root_ )
  call check_( nf90_put_att(ncid, lon_varid, "long_name", "longitude"     ), rc, mype_, root_ )
  call check_( nf90_put_att(ncid, lon_varid, "units"    , "degrees_east"  ), rc, mype_, root_ )
  if(present(levs)) then
    call check_( nf90_put_att(ncid, lev_varid, "coordinate", "eta"), rc, mype_, root_ )
    call check_( nf90_put_att(ncid, lev_varid, "long_name", "vertical level"), rc, mype_, root_ )
    call check_( nf90_put_att(ncid, lev_varid, "positive", "down"), rc, mype_, root_ )
    call check_( nf90_put_att(ncid, lev_varid, "standard_name", "model_layers"), rc, mype_, root_ )
    call check_( nf90_put_att(ncid, lev_varid, "units", "layer"), rc, mype_, root_ )
  endif

  call check_( nf90_put_att(ncid, tim_varid, "begin_date" , nymd_    ), rc, mype_, root_ )
  call check_( nf90_put_att(ncid, tim_varid, "begin_time" , nhms_    ), rc, mype_, root_ )
  call check_( nf90_put_att(ncid, tim_varid, "long_name"  , "time"   ), rc, mype_, root_ )
  call check_( nf90_put_att(ncid, tim_varid, "time_increment", ntinc ), rc, mype_, root_ )
  call check_( nf90_put_att(ncid, tim_varid, "units", trim(timunits) ), rc, mype_, root_ )

! The dimids array is used to pass the IDs of the dimensions of
! the variables. Note that in fortran arrays are stored in
! column-major format.
  if (ndims==3) then
    dimids =  (/ x_dimid, y_dimid, t_dimid /)
    start  =  (/ 1, 1, 1 /)
  else
    dimids =  (/ x_dimid, y_dimid, z_dimid, t_dimid /)
    start  =  (/ 1, 1, 1, 1 /)
  endif

! Define variables.
  allocate(varid2d(nv2d))
  do nv = 1, nv2d
     if(trim(cvars2d(nv))=="vfrac") then
        long_name = "vegetation_fraction"
     endif
     if(trim(cvars2d(nv))=="vtype") then 
        long_name = "vegetation_type"
     endif
     if(trim(cvars2d(nv))=="stype") then 
        long_name = "surface_type"
     endif
     ! alphabetically ordered
     call check_( nf90_def_var(ncid, trim(cvars2d(nv)), NF90_REAL, (/ x_dimid, y_dimid, t_dimid /), varid2d(nv)), rc, mype_, root_ )
     call check_( nf90_put_att(ncid, varid2d(nv), "_FillValue", undef)                                          , rc, mype_, root_ )
     call check_( nf90_put_att(ncid, varid2d(nv), "add_offset",   0.0)                                          , rc, mype_, root_ )
     call check_( nf90_put_att(ncid, varid2d(nv), "fmissing_value", undef)                                      , rc, mype_, root_ )
     call check_( nf90_put_att(ncid, varid2d(nv), "long_name", trim(long_name))                                 , rc, mype_, root_ )
     call check_( nf90_put_att(ncid, varid2d(nv), "missing_value", undef)                                       , rc, mype_, root_ )
     call check_( nf90_put_att(ncid, varid2d(nv), "scale_factor", 1.0)                                          , rc, mype_, root_ )
     call check_( nf90_put_att(ncid, varid2d(nv), "standard_name", trim(long_name))                             , rc, mype_, root_ )
     call check_( nf90_put_att(ncid, varid2d(nv), "units", "1")                                                 , rc, mype_, root_ )
     call check_( nf90_put_att(ncid, varid2d(nv), "valid_range", (/-undef,undef/))                              , rc, mype_, root_ )
     call check_( nf90_put_att(ncid, varid2d(nv), "vmax",  undef)                                               , rc, mype_, root_ )
     call check_( nf90_put_att(ncid, varid2d(nv), "vmin", -undef)                                               , rc, mype_, root_ )
  enddo

  if(nlev>1) then
    allocate(varid3d(nv3d))
    do nv = 1, nv3d
       if(trim(cvars3d(nv))=="dummy") then
          long_name = "dymmy4jedi_compliance"
       endif
       ! alphabetically ordered
       call check_( nf90_def_var(ncid, trim(cvars3d(nv)), NF90_REAL, (/ x_dimid, y_dimid, z_dimid, t_dimid /), varid3d(nv)) &
                                                                                                                , rc, mype_, root_ )
       call check_( nf90_put_att(ncid, varid3d(nv), "_FillValue", undef)                                        , rc, mype_, root_ )
       call check_( nf90_put_att(ncid, varid3d(nv), "add_offset",   0.0)                                        , rc, mype_, root_ )
       call check_( nf90_put_att(ncid, varid3d(nv), "fmissing_value", undef)                                    , rc, mype_, root_ )
       call check_( nf90_put_att(ncid, varid3d(nv), "long_name", trim(long_name))                               , rc, mype_, root_ )
       call check_( nf90_put_att(ncid, varid3d(nv), "missing_value", undef)                                     , rc, mype_, root_ )
       call check_( nf90_put_att(ncid, varid3d(nv), "scale_factor", 1.0)                                        , rc, mype_, root_ )
       call check_( nf90_put_att(ncid, varid3d(nv), "standard_name", trim(long_name))                           , rc, mype_, root_ )
       call check_( nf90_put_att(ncid, varid3d(nv), "units", "1")                                               , rc, mype_, root_ )
       call check_( nf90_put_att(ncid, varid3d(nv), "valid_range", (/-undef,undef/))                            , rc, mype_, root_ )
       call check_( nf90_put_att(ncid, varid3d(nv), "vmax",  undef)                                             , rc, mype_, root_ )
       call check_( nf90_put_att(ncid, varid3d(nv), "vmin", -undef)                                             , rc, mype_, root_ )
    enddo
  endif

! End define mode. This tells netCDF we are done defining metadata.
  call check_( nf90_enddef(ncid), rc, mype_, root_ )

! Write coordinate variables data
  call check_( nf90_put_var(ncid, lon_varid, lons ), rc, mype_, root_ )
  call check_( nf90_put_var(ncid, lat_varid, lats ), rc, mype_, root_ )
  if(present(levs)) then
     call check_( nf90_put_var(ncid, lev_varid, levs), rc, mype_, root_ )
  endif
  call check_( nf90_put_var(ncid, tim_varid, (/0/)), rc, mype_, root_ )

! Write out lat/lon fields
  allocate(data_out(nlon,nlat,1))
  do nv = 1, nv2d
     if(trim(cvars2d(nv))=="vfrac") then
        data_out(:,:,1) = bvars%vfrac
     endif
     if(trim(cvars2d(nv))=="vtype") then 
        data_out(:,:,1) = bvars%vtype
     endif
     if(trim(cvars2d(nv))=="stype") then 
        data_out(:,:,1) = bvars%stype
     endif
     call check_( nf90_put_var(ncid, varid2d(nv), data_out(:,:,1)), rc, mype_, root_ )
  enddo
  deallocate(data_out)

  if(nlev>1) then
    allocate(data_out(nlon,nlat,nlev))
    do nv = 1, nv3d
       if(trim(cvars3d(nv))=="dummy") then
          data_out = bvars%dummy
       endif
     call check_( nf90_put_var(ncid, varid3d(nv), data_out(:,:,:)), rc, mype_, root_ )
    enddo
    deallocate(data_out)
  endif

! Global attributes

! Close file
  call check_( nf90_close(ncid), rc, mype_, root_ )

  deallocate(varid2d)
  deallocate(dimids,start)

  print *, "*** Finish writing file ", fname

  return

end subroutine write_

subroutine init_vars_(vr,nlon,nlat,nsig)

  integer,intent(in) :: nlon,nlat,nsig
  type(gsifixsfc_vars) vr

  if(vr%initialized) return

  vr%nlon=nlon 
  vr%nlat=nlat
  vr%nlev=nsig

! allocate single precision arrays
  allocate(vr%vfrac(nlon,nlat),vr%vtype(nlon,nlat),vr%stype(nlon,nlat))
  if (nsig>1) then
     allocate(vr%dummy(nlon,nlat,nsig))
     vr%dummy=0.0d0
  endif
  vr%initialized=.true.
  end subroutine init_vars_

  subroutine final_vars_(vr)
  type(gsifixsfc_vars) vr
! deallocate arrays
  if(.not. vr%initialized) return
  if(associated(vr%dummy)) deallocate(vr%dummy)
  deallocate(vr%vfrac,vr%vtype,vr%stype)
  vr%initialized=.false.
end subroutine final_vars_

subroutine comp_vars_(va,vb,rc, myid,root)
  type(gsifixsfc_vars) va
  type(gsifixsfc_vars) vb
  integer, intent(out) :: rc
  integer, intent(in), optional :: myid,root        ! accommodate MPI calling programs
  character(len=*), parameter :: myname_ = myname//"::comp_vars_"
  integer :: ii,jj,ier(50)
  logical :: verbose, failed
  real :: tolerance = 10.e-10
!
  rc=0
  verbose=.true.
  if(present(myid).and.present(root) )then
    if(myid/=root) verbose=.false.
  endif
! Consistency check
  if (va%nlon/=vb%nlon .or. va%nlat/=vb%nlat .or. va%nlev/=vb%nlev ) then
     rc=1
     if(myid==root) then
       print *, 'nlat(va) = ', va%nlat, 'nlat(vb) = ', vb%nlat
       print *, 'nlon(va) = ', va%nlon, 'nlon(vb) = ', vb%nlon
       print *, 'nlev(va) = ', va%nlev, 'nlev(vb) = ', vb%nlev
       print *, myname_,  'Inconsistent dimensions, aborting ... '
     endif
     return
  endif

  ii=0;ier=0
  ii=ii+1; if(abs(sum(va%vfrac - vb%vfrac)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%vtype - vb%vtype)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%stype - vb%stype)) >tolerance) ier(ii)=ii
  failed=.false.
  do jj=1,ii
     if(ier(jj)/=0.and.verbose) then
       print *, 'Found field ', jj, ' not to match'
       failed=.true.
     endif
  enddo
  if (.not.failed) then
       if(verbose) print *, 'Comp finds all fields to match'
  endif
end subroutine comp_vars_

subroutine copy_(ivars,ovars,hydro)
  type(gsifixsfc_vars) ivars
  type(gsifixsfc_vars) ovars
  logical, intent(in), optional :: hydro

  logical wrtall,hydro_

  hydro_=.true.
  wrtall=.true.
  if (ovars%nlon/=ivars%nlon .or. &
      ovars%nlat/=ivars%nlat      ) then
      print*, 'copy_: Trying to copy inconsistent vectors, aborting ...'
      call exit(1)
  endif
  if ( ovars%nlev/=ivars%nlev ) then
     wrtall=.false.
  endif
  if(present(hydro)) then
    hydro_ = hydro
  endif

  ovars%vfrac   = ivars%vfrac
  ovars%vtype   = ivars%vtype
  ovars%stype   = ivars%stype

end subroutine copy_

subroutine get_pointer_2d_ (vname, bvars, ptr, rc )
implicit none
character(len=*), intent(in) :: vname
type(gsifixsfc_vars) bvars
real(4),pointer,intent(inout) :: ptr(:,:)
integer,intent(out) :: rc
character(len=5) :: var
rc=-1
!
var='vfrac'
if(trim(vname)==trim(var)) then
  ptr => bvars%vfrac
  rc=0
  return
endif
!
var='vtype'
if(trim(vname)==trim(var)) then
  ptr => bvars%vtype
  rc=0
  return
endif
!
var='stype'
if(trim(vname)==trim(var)) then
  ptr => bvars%stype
  rc=0
  return
endif
end subroutine get_pointer_2d_

subroutine check_(status,rc, myid, root)
    integer, intent ( in) :: status
    integer, intent (out) :: rc
    integer, intent ( in) :: myid, root
    rc=0
    if(status /= nf90_noerr) then 
      if(myid==root) print *, trim(nf90_strerror(status))
      rc=999
    end if
end subroutine check_  

end module m_gsifixsfc
