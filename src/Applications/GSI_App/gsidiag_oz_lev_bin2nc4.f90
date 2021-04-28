program convert_oz_lev_diag

  use kinds,only: r_quad, r_single,i_kind

  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d, nc_diag_chaninfo_dim_set, nc_diag_chaninfo

  implicit none

  real,parameter::     missing = -9.99e9
  integer,parameter:: imissing = -999999

  integer nargs, iargc, n
  character*256, allocatable ::   arg(:)


  integer i
!!!  real(r_quad)                         ::  ret_var
!!!  real(r_quad)                         ::  ret_stddev

! commandline variables
  logical                              ::  debug
  integer                              ::  npred_read
  logical                              ::  append_suffix

  character*256 infn, outfn
  logical linfile, loutfile

  integer,parameter                    ::  inlun = 51
  integer,parameter                    ::  outlun= 52
  integer,parameter                    ::  nllun = 53

  integer strlen, iflag
  integer iobs, ilev,ii

! ozone stuff
  integer :: nlevs,iint,ioff0,ob_count
  character(20) :: isis,obstype
  character(20) :: dplat
  integer :: jiter,ianldate,ireal,irdim
  real(r_single),allocatable,dimension(:) :: pobs,grs4,err4
  integer(i_kind),allocatable,dimension(:) :: iouse
  integer :: nobs
  integer(i_kind),allocatable,dimension(:,:) :: idiagbuf
  real(r_single) ,allocatable, dimension(:,:) :: diagbuf
  real(r_single), allocatable, dimension(:,:,:) :: rdiagbuf
  integer :: stop_output

  nargs = iargc()
  if( nargs.eq.0 ) then
    call usage
  else
    debug = .false.
    append_suffix = .false.

    allocate(arg(nargs))
    do n=1,nargs
      call getarg(n,arg(n))
    enddo
    do n=1,nargs
      if (trim(arg(n)).eq.'-debug'     ) debug=.true.
      if (trim(arg(n)).eq.'-append_nc4') append_suffix=.true.
    enddo
  endif


  if (debug) write(*,*)'Debugging on - Verbose Printing'

  ! get infn from command line
  call getarg(nargs, infn)

  strlen = len(trim(infn))

  write(*,'(a,a)')'Input bin diag:  ',trim(infn)
  inquire(file=trim(infn), exist=linfile)
  if (.not. linfile) then
    write(*,'(a)')trim(infn) // ' does not exist - exiting'
    call abort
  endif

  if (.not. append_suffix) then
    outfn = infn(1:strlen-3) // 'nc4'  ! assumes GMAO diag filename format ending with .bin, and replaces it
  else
    outfn = infn(1:strlen) // '.nc4'    ! if not GMAO format, use append_suffix = .true. in namelist
                                       !   to simply append infile with .nc4 suffix
  endif

  write(*,'(a,a)')'Output NC4 diag: ',trim(outfn)
  inquire(file=trim(outfn), exist=loutfile)
  if (loutfile) write(*,'(a)')'WARNING: ' // trim(outfn) // ' exists - overwriting'

  iflag = 0
  ii = 0
  open(inlun,file=infn,form='unformatted',convert='big_endian')
  call nc_diag_init(outfn)
  ! stupid way to count the number of obs, so you stop before you hit the last ioff0
  call count_obs( inlun, ob_count )
  open(inlun,file=infn,form='unformatted',convert='big_endian')
  call read_oz_header( inlun, isis, obstype, dplat, jiter, nlevs, ianldate, iint, ireal, irdim, iflag, ioff0 )
  call nc_diag_header( "date_time",             ianldate  )
  call nc_diag_header( "Satellite_Sensor",      trim(isis)      )
  call nc_diag_header( "Satellite",        trim(dplat)    )
  call nc_diag_header( "Observation_type", trim(obstype)  )
  allocate(pobs(nlevs),grs4(nlevs),err4(nlevs),iouse(nlevs))
  ! remove duplicates at the end in binary by stopping by weird offset. 
  stop_output = ob_count-(5*ioff0+1)
  do while (iflag .ge. 0) ! iflag == 0 means the end of the file
    read(inlun,iostat=iflag) nobs
    allocate(idiagbuf(iint,nobs))
    allocate(diagbuf(ireal,nobs))
    allocate(rdiagbuf(irdim,1,nobs))
    read(inlun,iostat=iflag) idiagbuf(:,1:nobs), diagbuf(:,1:nobs), rdiagbuf(:,1,1:nobs)
    do iobs = 1,nobs
        if(.not. ii >= stop_output) then
           call nc_diag_metadata( "Latitude",                      diagbuf(1,iobs)    )
           call nc_diag_metadata( "Longitude",                     diagbuf(2,iobs)    )
           call nc_diag_metadata( "MPI_Task_Number",               idiagbuf(1,iobs)   )
           call nc_diag_metadata( "Time",                          diagbuf(3,iobs)    )
           call nc_diag_metadata( "Inverse_Observation_Error",     rdiagbuf(3,1,iobs) )
           call nc_diag_metadata( "Observation",                   rdiagbuf(1,1,iobs) )
           call nc_diag_metadata( "Obs_Minus_Forecast_adjusted",   rdiagbuf(2,1,iobs) )
           call nc_diag_metadata( "Obs_Minus_Forecast_unadjusted", rdiagbuf(2,1,iobs) )
           call nc_diag_metadata( "Reference_Pressure",            rdiagbuf(4,1,iobs) )
           call nc_diag_metadata( "Input_Observation_Error",       rdiagbuf(6,1,iobs) ) 
           ii = ii+1
        endif
    enddo !nobs
    deallocate(idiagbuf, diagbuf, rdiagbuf)
    if (iflag .lt. 0) cycle

  enddo
deallocate(pobs,grs4,err4,iouse)
! finalize NCDIAG
  call nc_diag_write
  close(inlun)
end program convert_oz_lev_diag

subroutine read_oz_header( inlun, isis,obstype, dplat,jiter, nlevs,ianldate,iint,ireal,irdim,iflag,ioff0 )
  implicit none
  integer,intent(in) ::  inlun
  character(20),intent(out) :: isis
  character(10),intent(out) :: dplat
  character(10),intent(out) :: obstype
  integer, intent(out) :: jiter,nlevs,ianldate,ireal,irdim,iint
  integer,intent(inout) :: iflag
  integer,intent(out) ::ioff0
  read(inlun,iostat=iflag) isis,dplat,obstype,jiter,nlevs,ianldate,iint,ireal,irdim,ioff0
end subroutine read_oz_header
subroutine count_obs( inlun, ob_count )
  use kinds,only: r_quad, r_single,i_kind
  implicit none
  integer :: inlun,ob_count
! ozone stuff
  integer :: nlevs,iint,ioff0
  character(20) :: isis,obstype
  character(20) :: dplat
  integer :: jiter,ianldate,ireal,irdim
  real(r_single),allocatable,dimension(:) :: pobs,grs4,err4
  integer(i_kind),allocatable,dimension(:) :: iouse
  integer :: nobs
  integer(i_kind),allocatable,dimension(:,:) :: idiagbuf
  real(r_single) ,allocatable, dimension(:,:) :: diagbuf
  real(r_single), allocatable, dimension(:,:,:) :: rdiagbuf
  integer :: iflag
  ob_count = 0
  call read_oz_header( inlun, isis, obstype, dplat, jiter, nlevs, ianldate, iint, ireal, irdim, iflag, ioff0 )
  do while (iflag .ge. 0) ! iflag == 0 means the end of the file
    read(inlun,iostat=iflag) nobs
    allocate(idiagbuf(iint,nobs))
    allocate(diagbuf(ireal,nobs))
    allocate(rdiagbuf(irdim,1,nobs))
    read(inlun,iostat=iflag) idiagbuf(:,1:nobs), diagbuf(:,1:nobs), rdiagbuf(:,1,1:nobs)
    ob_count = ob_count + nobs
    deallocate(idiagbuf, diagbuf, rdiagbuf)
    if (iflag .lt. 0) cycle

  enddo
  close(inlun)
end subroutine count_obs
subroutine usage
     write(6,100)
100  format( "Usage:  ",/,/ &
             "  gsidiag_oz_lev_bin2nc4.x <options> <filename>",/,/ &
             "where options:",/ &
             "  -debug              :  Set debug verbosity",/ &
             "  -append_nc4         :  Append .nc4 suffix, instead of replace last three",/ &
             "                             characters (default: replaced)",/ &
             "                             Note:  The GMAO diag files end with .bin or .nc4,",/ &
             "                               whiobs is where fixed 3-char truncation originates",/,/,/ &
             "  Example:",/ &
             "     gsidiag_oz_lev_bin2nc4.x nc_4emily.diag_mls55_aura_ges.20161202_00z.bin",/ &
             "  Output file:",/ &
             "     nc_4emily.diag_mls55_aura_ges.20161202_00z.nc4",/ &
    )
    stop
end subroutine usage
