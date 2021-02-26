module m_fvHeader

  use type_kinds, only: double
  use variables, only : filename
  use m_die, only: die

  implicit none
  private ! except

  public :: fvHeader_read

  interface fvHeader_read; module procedure    &
     read_; end interface 

  character(len=*),parameter :: myname='m_fvHeader'
  integer, parameter :: nch = 256
  integer, parameter :: READ_ONLY = 1

contains

  subroutine read_(loop,nymd,nhms,mype) 

  implicit none

  integer,intent(in)  :: loop,mype
  integer,intent(out) :: nymd, nhms

  character(len=nch)              :: title, source, contact, levunits
  character(len=nch), allocatable :: vname(:), vtitle(:), vunits(:)
                                                                                
  character(len=*),parameter :: myname_=myname//':read_'
  integer :: ierror
  integer :: fid, err, ngatts, timinc
  integer :: im, jm, km, lm, nvars
  integer, allocatable :: kmvar(:), yyyymmdd(:), hhmmss(:)

  real(double) :: amiss
  real(double), allocatable :: lat(:), lon(:), lev(:)
  real(double), allocatable :: valid_range(:,:), packing_range(:,:)

!  Open the file
!  -------------
    call GFIO_Open ( trim(filename(loop)), READ_ONLY, fid, err )
    if ( err .ne. 0 ) then
      write(6,*)'error to call GFIO_Open ', err
      write(6,*)trim(filename(loop))
      call mpi_finalize(ierror)
      stop     
    end if

!  Get dimensions
!  --------------
    call GFIO_DimInquire ( fid, im, jm, km, lm, nvars, ngatts, err)
    if ( err .ne. 0 .or. lm .ne. 1) then
      write(6,*)'error to call GFIO_DimInquire or lm != 1'
      call mpi_finalize(ierror)
      stop     
    end if

    call init_()

!  Get file attributes
!  -------------------
    call GFIO_Inquire ( fid, im, jm, km, lm, nvars,     &
                       title, source, contact, amiss,  &
                       lon, lat, lev, levunits,        &
                       yyyymmdd, hhmmss, timinc,       &
                       vname, vtitle, vunits, kmvar,   &
                       valid_range , packing_range, err )

    if ( err .ne. 0 ) then
      write(6,*)'error to call GFIO_Inquire '
      call clean_()
      call mpi_finalize(ierror)
      stop     
    end if

    if(loop == 1 .and. mype == 0) then
      write(6,*)'nlat_fv= ',jm,' nlon_fv= ',im,' km_fv= ',km
      write(6,*)'dlat= ',lat(2)-lat(1), ' dlon= ',lon(2)-lon(1)
    endif

    nymd = yyyymmdd(lm)
    nhms = hhmmss(lm)

    call clean_() 
    call GFIO_close ( fid, err )

    contains

     subroutine init_()
       allocate (lat(jm), lon(im), lev(km),    &
                 yyyymmdd(lm), hhmmss(lm),     &
                 vname(nvars), vunits(nvars),  &
                 vtitle(nvars), kmvar(nvars),  &
                 valid_range(2,nvars),         &
                 packing_range(2,nvars))
     end subroutine init_

     subroutine clean_()             
      deallocate (lat,lon,lev,yyyymmdd,hhmmss,   &
                  vname,vunits, vtitle,kmvar,    &
                  valid_range, packing_range)
     end subroutine clean_

  end subroutine read_ 

end module m_fvHeader
