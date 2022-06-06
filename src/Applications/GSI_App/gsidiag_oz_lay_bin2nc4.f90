program convert_oz_diag

  use kinds,only: r_quad, r_single,i_kind

  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d, nc_diag_chaninfo_dim_set, nc_diag_chaninfo

  implicit none

  real,parameter::     missing = -9.99e9
  integer,parameter:: imissing = -999999

  integer nargs, iargc, n
  character*256, allocatable ::   arg(:)


  integer i

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
  integer(i_kind),allocatable,dimension(:,:) :: idiagbuf
  real(r_single) ,allocatable, dimension(:,:) :: diagbuf
  real(r_single), allocatable, dimension(:,:,:) :: rdiagbuf
  integer :: nobs
  integer(i_kind) :: max_nobs
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
  ii = 1
  open(inlun,file=infn,form='unformatted',convert='big_endian')
  call count_obs( inlun, ob_count,max_nobs )
  close(inlun)
  write(*,*)'ob_count',ob_count
  open(inlun,file=infn,form='unformatted',convert='big_endian')
  call read_oz_header( inlun, isis, obstype, dplat, jiter, nlevs, ianldate, iint, ireal, irdim, iflag, ioff0 )
  !setup the stuff output on the number of levels this thing has. Unless this is SBUV or OMI with efficiency factors, nlevs=1
  allocate(pobs(nlevs),grs4(nlevs),err4(nlevs),iouse(nlevs))
  read(inlun,iostat=iflag) pobs,grs4,err4,iouse
  !going with a single allocate slice and dump immediately. Keeping big arrays in memory, or allocating/deallocating alot broke things.
  allocate(idiagbuf(iint,max_nobs),diagbuf(ireal,max_nobs),rdiagbuf(irdim,nlevs,max_nobs))
  call nc_diag_init(outfn)
  call nc_diag_header("date_time",ianldate )
  call nc_diag_header("Number_of_state_vars", imissing ) 

  ii=1
  do while (iflag .ge. 0) ! iflag == 0 means the end of the file
    read(inlun,iostat=iflag) nobs
    if (iflag .lt. 0) cycle
    read(inlun,iostat=iflag) idiagbuf(:,1:nobs), diagbuf(:,1:nobs),rdiagbuf(:,:,1:nobs)!rtmp,rtmp,rtmp! rdiagbuf_(:,:,nobs_now:nobs_now+nobs)
     do iobs = 1,nobs
      do ilev = 1,nlevs
        call nc_diag_metadata("MPI_Task_Number", idiagbuf(1,iobs)          )
        call nc_diag_metadata("Latitude",        diagbuf(1,iobs)       )
        call nc_diag_metadata("Longitude",       diagbuf(2,iobs)       )
        call nc_diag_metadata("Time",            diagbuf(3,iobs) )
        call nc_diag_metadata("Reference_Pressure",     100.0*pobs(ilev)      )
        if ( iouse(ilev) < 1.0 .or. rdiagbuf(3,ilev,iobs)< 10*tiny(rdiagbuf(3,ilev,iobs))) then
          call nc_diag_metadata("Analysis_Use_Flag",      0          )
        else
          call nc_diag_metadata("Analysis_Use_Flag",      1         )
        endif
        call nc_diag_metadata("Observation",            rdiagbuf(1,ilev,iobs) )
        call nc_diag_metadata("Inverse_Observation_Error",    rdiagbuf(3,ilev,iobs) )
        call nc_diag_metadata("Obs_Minus_Forecast_adjusted",  rdiagbuf(2,ilev,iobs) )
        call nc_diag_metadata("Obs_Minus_Forecast_unadjusted",rdiagbuf(2,ilev,iobs) )
        call nc_diag_metadata("Solar_Zenith_Angle", rdiagbuf(7,ilev,iobs) )
        call nc_diag_metadata("Scan_Position",      rdiagbuf(5,ilev,iobs) )
        call nc_diag_metadata("Row_Anomaly_Index", rdiagbuf(6,ilev,iobs)  )
        ii=ii+1
      enddo !nlev
    enddo !nobs
    write(*,*)'dafuq',ii

  enddo
  close(inlun)

! finalize NCDIAG
  call nc_diag_write
  deallocate(idiagbuf, diagbuf, rdiagbuf)
  deallocate(pobs,grs4,err4,iouse)
end program convert_oz_diag

! counts all obs in the binary file that is catted together from files dumped on each processor
subroutine count_obs( inlun, ob_count,max_nobs )
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
  integer :: max_nobs
  integer :: iflag
  ob_count = 0
  max_nobs = 0
  call read_oz_header( inlun, isis, obstype, dplat, jiter, nlevs, ianldate, iint, ireal, irdim, iflag, ioff0 )
  allocate(pobs(nlevs),grs4(nlevs),err4(nlevs),iouse(nlevs))
  read(inlun,iostat=iflag) pobs,grs4,err4,iouse
  do while (iflag .ge. 0) ! iflag == 0 means the end of the file
    read(inlun,iostat=iflag) nobs
    if(nobs>max_nobs) then
       max_nobs = nobs
    endif
    if(iflag /= 0) cycle
    allocate(idiagbuf(iint,nobs))
    allocate(diagbuf(ireal,nobs))
    allocate(rdiagbuf(irdim,nlevs,nobs))
    read(inlun,iostat=iflag) idiagbuf(:,1:nobs), diagbuf(:,1:nobs), rdiagbuf(:,:,1:nobs)
    ob_count = ob_count + nlevs*nobs
    deallocate(idiagbuf, diagbuf, rdiagbuf)
    if (iflag .ge. 0) cycle

  enddo
  deallocate(pobs,grs4,err4,iouse)
end subroutine count_obs

! read_oz_header reads the metadata contained in the binary header
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
subroutine usage
     write(6,100)
100  format( "Usage:  ",/,/ &
             "  gsidiag_oz_lay_bin2nc4.x <options> <filename>",/,/ &
             "where options:",/ &
             "  -debug              :  Set debug verbosity",/ &
             "  -append_nc4         :  Append .nc4 suffix, instead of replace last three",/ &
             "                             characters (default: replaced)",/ &
             "                             Note:  The GMAO diag files end with .bin or .nc4,",/ &
             "                               whiobs is where fixed 3-char truncation originates",/,/,/ &
             "  Example:",/ &
             "     gsidiag_oz_lay_bin2nc4.x nc_4emily.diag_omi_aura_ges.20161202_00z.bin",/ &
             "  Output file:",/ &
             "     nc_4emily.diag_omi_aura_ges.20161202_00z.nc4",/ &
    )
    stop
end subroutine usage
