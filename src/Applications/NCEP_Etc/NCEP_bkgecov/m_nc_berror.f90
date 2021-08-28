module m_nc_berror
use netcdf
implicit none
private

public :: berror_vars
public :: read_nc_berror
public :: write_nc_berror

type berror_vars
   integer :: nlon,nlat,nsig
   real(4),allocatable,dimension(:,:,:):: tcon
   real(4),allocatable,dimension(:,:)  :: sfvar,vpvar,tvar,qvar,cvar,nrhvar,ozvar
   real(4),allocatable,dimension(:,:)  :: qivar,qlvar,qrvar,qsvar
   real(4),allocatable,dimension(:,:)  :: sfhln,vphln,thln,qhln,chln,ozhln
   real(4),allocatable,dimension(:,:)  :: qihln,qlhln,qrhln,qshln
   real(4),allocatable,dimension(:,:)  :: sfvln,vpvln,tvln,qvln,cvln,ozvln
   real(4),allocatable,dimension(:,:)  :: qivln,qlvln,qrvln,qsvln
   real(4),allocatable,dimension(:,:)  :: vpcon,pscon,varsst,corlsst
   real(4),allocatable,dimension(:)    :: psvar,pshln
end type berror_vars

integer, parameter :: nv1d = 2
character(len=4),parameter :: cvars1d(nv1d) = (/ 'ps  ', 'hps ' /)

integer, parameter :: nv2d = 33
character(len=5),parameter :: cvars2d(nv2d) = (/ &
                                              'sf   ', 'hsf  ', 'vsf  ', &
                                              'vp   ', 'hvp  ', 'vvp  ', &
                                              't    ', 'ht   ', 'vt   ', &
                                              'q    ', 'hq   ', 'vq   ', &
                                              'qi   ', 'hqi  ', 'vqi  ', &
                                              'ql   ', 'hql  ', 'vql  ', &
                                              'qr   ', 'hqr  ', 'vqr  ', &
                                              'qs   ', 'hqs  ', 'vqs  ', &
                                              'oz   ', 'hoz  ', 'voz  ', &
                                              'cw   ', 'hcw  ', 'vcw  ', &
                                              'pscon', 'vpcon', 'nrh  '  &
                                              /)

integer, parameter :: nvmll = 1  ! meriodional, level, level
character(len=4),parameter :: cvarsMLL(nvmll) = (/ 'tcon' /)

integer, parameter :: nv2dx = 2
character(len=7),parameter :: cvars2dx(nv2dx) = (/ 'sst    ', 'sstcorl' /)

contains

subroutine read_nc_berror (fname,bvars)
  implicit none
  character(len=*), intent(in)    :: fname ! input filename
  type(berror_vars),intent(inout) :: bvars ! background error variables

! This will be the netCDF ID for the file and data variable.
  integer :: ncid, varid

! Local variables
  character(len=4) :: cindx
  integer nv,nl,nlat,nlon,nlev
  real(4), allocatable :: data_in(:,:,:)
   
! Set dims
  nlat=bvars%nlat
  nlon=bvars%nlon
  nlev=bvars%nsig

! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
! the file.

  call check( nf90_open(fname, NF90_NOWRITE, ncid) )

! Read data to file
  allocate(data_in(1,nlat,1))
  do nv = 1, nv1d
     call check( nf90_inq_varid(ncid, trim(cvars1d(nv)), varid) )
     call check( nf90_get_var(ncid, varid, data_in(1,:,1)))
     if(trim(cvars1d(nv))=="ps"  ) bvars%psvar = data_in(1,:,1)
     if(trim(cvars1d(nv))=="hps" ) bvars%pshln = data_in(1,:,1)
  enddo
  deallocate(data_in)
  allocate(data_in(1,nlat,nlev))
  do nv = 1, nv2d
     call check( nf90_inq_varid(ncid, trim(cvars2d(nv)), varid) )
     call check( nf90_get_var(ncid, varid, data_in(1,:,:)) )

     if(trim(cvars2d(nv))=="sf" ) bvars%sfvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hsf") bvars%sfhln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vsf") bvars%sfvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="vp" ) bvars%vpvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hvp") bvars%vphln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vvp") bvars%vpvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="t"  ) bvars%tvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="ht" ) bvars%thln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vt" ) bvars%tvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="q"  ) bvars%qvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hq" ) bvars%qhln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vq" ) bvars%qvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="qi" ) bvars%qivar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hqi") bvars%qihln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vqi") bvars%qivln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="ql" ) bvars%qlvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hql") bvars%qlhln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vql") bvars%qlvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="qr" ) bvars%qrvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hqr") bvars%qrhln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vqr") bvars%qrvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="nrh") bvars%nrhvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="qs" ) bvars%qsvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hqs") bvars%qshln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vqs") bvars%qsvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="cw" ) bvars%cvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hcw") bvars%chln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vcw") bvars%cvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="oz" ) bvars%ozvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hoz") bvars%ozhln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="voz") bvars%ozvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="pscon") bvars%pscon = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vpcon") bvars%vpcon = data_in(1,:,:)
!
  enddo

! Get matrix NLATxNLEVxNLEV that has been written as NLEV 2d-fields 
  do nv = 1, nvmll
     do nl = 1, nlev
        write(cindx,'(i4.4)') nl
        if(trim(cvarsMLL(nv))=="tcon") then
           call check( nf90_inq_varid(ncid, trim(cvarsMLL(nv))//cindx, varid) )
           call check( nf90_get_var(ncid, varid, data_in(1,:,:)) )
           bvars%tcon(:,:,nl) = data_in(1,:,:)
        endif
     enddo
  enddo
  deallocate(data_in)

! Write out lat/lon fields
  allocate(data_in(nlon,nlat,1))
  do nv = 1, nv2dx
     call check( nf90_inq_varid(ncid, trim(cvars2dx(nv)), varid) )
     call check( nf90_get_var(ncid, varid, data_in(:,:,1)) )
     if(trim(cvars2dx(nv))=="sst"     ) then
        bvars%varsst = transpose(data_in(:,:,1))
     endif
     if(trim(cvars2dx(nv))=="sstcorl" ) then 
        bvars%corlsst = transpose(data_in(:,:,1))
     endif
  enddo
  deallocate(data_in)

! Close the file, freeing all resources.
  call check( nf90_close(ncid) )

  print *,"*** Finish reading file: ", trim(fname)

  return

contains
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check  
end subroutine read_nc_berror

subroutine write_nc_berror (fname,bvars,plevs,lats,lons)
  implicit none
  character(len=*), intent(in)    :: fname ! input filename
  type(berror_vars),intent(in)    :: bvars ! background error variables
  real(4), intent(in) :: lats(:)           ! latitudes  per GSI: increase index from South to North Pole
  real(4), intent(in) :: lons(:)           ! longitudea per GSI: increase index from East to West
  real(4), intent(in) :: plevs(:)

  integer, parameter :: NDIMS = 3

! When we create netCDF files, variables and dimensions, we get back
! an ID for each one.
  character(len=4) :: cindx
  integer :: ncid, dimids(NDIMS)
  integer :: x_dimid, y_dimid, z_dimid
  integer :: lon_varid, lat_varid, lev_varid
  integer :: ii,jj,nl,nv,nn,nlat,nlon,nlev
  integer, allocatable :: varid1d(:), varid2d(:), varid2dx(:), varidMLL(:)
  
! This is the data array we will write. It will just be filled with
! a progression of integers for this example.
  real(4), allocatable :: data_out(:,:,:)

! Set dims
  nlat=bvars%nlat
  nlon=bvars%nlon
  nlev=bvars%nsig

! Always check the return code of every netCDF function call. In
! this example program, wrapping netCDF calls with "call check()"
! makes sure that any return which is not equal to nf90_noerr (0)
! will print a netCDF error message and exit.

! Create the netCDF file. The nf90_clobber parameter tells netCDF to
! overwrite this file, if it already exists.
  call check( nf90_create(fname, NF90_CLOBBER, ncid) )

! Define the dimensions. NetCDF will hand back an ID for each. 
  call check( nf90_def_dim(ncid, "lon", nlon, x_dimid) )
  call check( nf90_def_dim(ncid, "lat", nlat, y_dimid) )
  call check( nf90_def_dim(ncid, "lev", nlev, z_dimid) )

  call check( nf90_def_var(ncid, "lon", NF90_REAL, x_dimid, lon_varid) )
  call check( nf90_def_var(ncid, "lat", NF90_REAL, y_dimid, lat_varid) )
  call check( nf90_def_var(ncid, "lev", NF90_REAL, z_dimid, lev_varid) )

  call check( nf90_put_att(ncid, lon_varid, "units", "degress") )
  call check( nf90_put_att(ncid, lat_varid, "units", "degress") )
  call check( nf90_put_att(ncid, lev_varid, "units", "hPa") )

! The dimids array is used to pass the IDs of the dimensions of
! the variables. Note that in fortran arrays are stored in
! column-major format.
  dimids =  (/ x_dimid, y_dimid, z_dimid /)

! Define variables.
  allocate(varid1d(nv1d))
  do nv = 1, nv1d
     call check( nf90_def_var(ncid, trim(cvars1d(nv)), NF90_REAL, (/ y_dimid /), varid1d(nv)) )
  enddo
  allocate(varid2d(nv2d))
  do nv = 1, nv2d
     call check( nf90_def_var(ncid, trim(cvars2d(nv)), NF90_REAL, (/ y_dimid, z_dimid /), varid2d(nv)) )
  enddo
  allocate(varidMLL(nlev*nvmll))
  nn=0
  do nv = 1, nvmll
     do nl = 1, nlev
        nn=nn+1
        write(cindx,'(i4.4)') nl
        call check( nf90_def_var(ncid, trim(cvarsMLL(nv))//cindx, NF90_REAL, (/ y_dimid, z_dimid /), varidMLL(nn)) )
     enddo
  enddo
  allocate(varid2dx(nv2dx))
  do nv = 1, nv2dx
     call check( nf90_def_var(ncid, trim(cvars2dx(nv)), NF90_REAL, (/ x_dimid, y_dimid /), varid2dx(nv)) )
  enddo

! End define mode. This tells netCDF we are done defining metadata.
  call check( nf90_enddef(ncid) )

! Write coordinate variables data
  call check( nf90_put_var(ncid, lon_varid, lons ) )
  call check( nf90_put_var(ncid, lat_varid, lats ) )
  call check( nf90_put_var(ncid, lev_varid, plevs) )

! Write data to file
  allocate(data_out(1,nlat,1))
  do nv = 1, nv1d
     if(trim(cvars1d(nv))=="ps"  ) data_out(1,:,1) = bvars%psvar
     if(trim(cvars1d(nv))=="hps" ) data_out(1,:,1) = bvars%pshln
     call check( nf90_put_var(ncid, varid1d(nv), data_out(1,:,1)))
  enddo
  deallocate(data_out)
  allocate(data_out(1,nlat,nlev))
  do nv = 1, nv2d
     if(trim(cvars2d(nv))=="sf" ) data_out(1,:,:) = bvars%sfvar
     if(trim(cvars2d(nv))=="hsf") data_out(1,:,:) = bvars%sfhln
     if(trim(cvars2d(nv))=="vsf") data_out(1,:,:) = bvars%sfvln
!
     if(trim(cvars2d(nv))=="vp" ) data_out(1,:,:) = bvars%vpvar
     if(trim(cvars2d(nv))=="hvp") data_out(1,:,:) = bvars%vphln
     if(trim(cvars2d(nv))=="vvp") data_out(1,:,:) = bvars%vpvln
!
     if(trim(cvars2d(nv))=="t"  ) data_out(1,:,:) = bvars%tvar
     if(trim(cvars2d(nv))=="ht" ) data_out(1,:,:) = bvars%thln
     if(trim(cvars2d(nv))=="vt" ) data_out(1,:,:) = bvars%tvln
!
     if(trim(cvars2d(nv))=="q"  ) data_out(1,:,:) = bvars%qvar
     if(trim(cvars2d(nv))=="hq" ) data_out(1,:,:) = bvars%qhln
     if(trim(cvars2d(nv))=="vq" ) data_out(1,:,:) = bvars%qvln
!
     if(trim(cvars2d(nv))=="qi" ) data_out(1,:,:) = bvars%qivar
     if(trim(cvars2d(nv))=="hqi") data_out(1,:,:) = bvars%qihln
     if(trim(cvars2d(nv))=="vqi") data_out(1,:,:) = bvars%qivln
!
     if(trim(cvars2d(nv))=="ql" ) data_out(1,:,:) = bvars%qlvar
     if(trim(cvars2d(nv))=="hql") data_out(1,:,:) = bvars%qlhln
     if(trim(cvars2d(nv))=="vql") data_out(1,:,:) = bvars%qlvln
!
     if(trim(cvars2d(nv))=="qr" ) data_out(1,:,:) = bvars%qrvar
     if(trim(cvars2d(nv))=="hqr") data_out(1,:,:) = bvars%qrhln
     if(trim(cvars2d(nv))=="vqr") data_out(1,:,:) = bvars%qrvln
!
     if(trim(cvars2d(nv))=="nrh") data_out(1,:,:) = bvars%nrhvar
     if(trim(cvars2d(nv))=="qs" ) data_out(1,:,:) = bvars%qsvar
     if(trim(cvars2d(nv))=="hqs") data_out(1,:,:) = bvars%qshln
     if(trim(cvars2d(nv))=="vqs") data_out(1,:,:) = bvars%qsvln
!
     if(trim(cvars2d(nv))=="cw" ) data_out(1,:,:) = bvars%cvar
     if(trim(cvars2d(nv))=="hcw") data_out(1,:,:) = bvars%chln
     if(trim(cvars2d(nv))=="vcw") data_out(1,:,:) = bvars%cvln
!
     if(trim(cvars2d(nv))=="oz" ) data_out(1,:,:) = bvars%ozvar
     if(trim(cvars2d(nv))=="hoz") data_out(1,:,:) = bvars%ozhln
     if(trim(cvars2d(nv))=="voz") data_out(1,:,:) = bvars%ozvln
!
     if(trim(cvars2d(nv))=="pscon") data_out(1,:,:) = bvars%pscon
     if(trim(cvars2d(nv))=="vpcon") data_out(1,:,:) = bvars%vpcon
!
     call check( nf90_put_var(ncid, varid2d(nv), data_out(1,:,:)) )
  enddo

! Choose to write out NLATxNLEVxNLEV vars as to facilitate visualization
  nn=0
  do nv = 1, nvmll
     do nl = 1, nlev
        nn = nn + 1
        write(cindx,'(i4.4)') nl
        if(trim(cvarsMLL(nv))=="tcon") data_out(1,:,:) = bvars%tcon(:,:,nl)
        call check( nf90_put_var(ncid, varidMLL(nn), data_out(1,:,:)) )
     enddo
  enddo
  deallocate(data_out)

! Write out lat/lon fields
  allocate(data_out(nlon,nlat,1))
  do nv = 1, nv2dx
     if(trim(cvars2dx(nv))=="sst"     ) then
        data_out(:,:,1) = transpose(bvars%varsst)
     endif
     if(trim(cvars2dx(nv))=="sstcorl" ) then 
        data_out(:,:,1) = transpose(bvars%corlsst)
     endif
     call check( nf90_put_var(ncid, varid2dx(nv), data_out(:,:,1)) )
  enddo
  deallocate(data_out)

! Close file
  call check( nf90_close(ncid) )

  deallocate(varidMLL)
  deallocate(varid2d)
  deallocate(varid1d)

  print *, "*** Finish writing file ", fname

  return
contains
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check  
end subroutine write_nc_berror

end module m_nc_berror
