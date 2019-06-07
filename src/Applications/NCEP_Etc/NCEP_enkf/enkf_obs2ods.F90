program enkf_obs2ods
use m_odsmeta, only: ktps2m,ktTv,ktqq,kttpw,ktGPSb,ktTb,kto3,&
                     ktus10,ktuu,ktvv,sats,nsats,idsats
use m_odsmeta, only: X_PASSIVE,X_ADV_LOCAL
use m_ods
use m_odsxsup, only : getodsmeta
use m_odsxsup, only : ncepQCXval
use m_Sndx,   only: setSndx
use m_ioutil, only: luavail 
use m_die,    only: die
use m_chars,  only: lowercase

implicit none

! !REVISION HISTORY:
!
!     31Mar2017 Todling  Initial code
!     12Apr2017 Diniz    Filled in initial stub
!     20Apr2017 Todling  Proper definition of meta data

character(len=*),parameter :: myname = 'enkf_obs2ods'

integer,parameter :: i_kind=4
integer,parameter :: r_single=4
!--------------------------------------------------------------------------------------------
!  The following is to be moved out of here into self contained structure file
!--------------------------------------------------------------------------------------------
! Structure for observation sensitivity information output
type obsense_header
  sequence
  integer(i_kind) :: idate              ! Base date (initial date)
  integer(i_kind) :: obsnum             ! Observation number (total)
  integer(i_kind) :: convnum            ! Observation number (conventional)
  integer(i_kind) :: oznum              ! Observation number (ozone)
  integer(i_kind) :: satnum             ! Observation number (satellite)
  integer(i_kind) :: npred              ! Number of predictors for bias correction
  integer(i_kind) :: nanals             ! Number of members
end type obsense_header

! Type definition for observation sensitivity information file
type obsense_info
  sequence
  real(r_single)  :: obfit_prior        ! Observation fit to the first guess
  real(r_single)  :: obfit_post         ! Observation fit after assimilation
  real(r_single)  :: obsprd_prior       ! Spread of observation prior
  real(r_single)  :: ensmean_obnobc     ! Ensemble mean first guess (no bias correction)
  real(r_single)  :: ensmean_ob         ! Ensemble mean first guess (bias corrected)
  real(r_single)  :: ob                 ! Observation value
  real(r_single)  :: oberrvar           ! Observation error variance
  real(r_single)  :: lon                ! Longitude
  real(r_single)  :: lat                ! Latitude
  real(r_single)  :: pres               ! Pressure
  real(r_single)  :: time               ! Observation time
  real(r_single)  :: oberrvar_orig      ! Original error variance
  integer(i_kind) :: stattype           ! Observation type
  character(len=20) :: obtype           ! Observation element / Satellite name
  integer(i_kind) :: indxsat            ! Satellite index (channel)
  integer(i_kind) :: iused              ! Index to indicate which obs were used (0=used)
  real(r_single)  :: osense_kin         ! Observation sensitivity (kinetic energy) [J/kg]
  real(r_single)  :: osense_dry         ! Observation sensitivity (Dry total energy) [J/kg]
  real(r_single)  :: osense_moist       ! Observation sensitivity (Moist total energy) [J/kg]
end type obsense_info
!--------------------------------------------------------------------------------------------

integer, parameter :: MFILES=1
character(len=256) :: infiles(MFILES)
character(len=256) :: outfile
character(len=3)   :: norm

type(obsense_header) header
type(obsense_info),allocatable:: contents(:)
type(ods_vect) :: ods

character(len=20),allocatable,dimension(:) :: nusis   ! sensor/instrument/satellite indicator
integer(i_kind),allocatable,dimension(:) :: nuchan    ! satellite channel
integer(i_kind),allocatable,dimension(:) :: iuse_rad  ! use to turn off satellite radiance data
integer(i_kind),allocatable,dimension(:) :: icld_det  ! Use this channel in cloud detection
real(r_single), allocatable,dimension(:) :: varch     ! variance for clear radiance each satellite channel
real(r_single), allocatable,dimension(:) :: varch_cld ! variance for cloudy radiance
real(r_single), allocatable,dimension(:) :: ermax_rad ! error maximum (qc)
real(r_single), allocatable,dimension(:) :: b_rad     ! variational b value
real(r_single), allocatable,dimension(:) :: pg_rad    ! variational pg value (only used for
                                                      !    certain instruments.
                                                      !    Set to greater than zero to use
integer :: nqcmax
integer :: nymd,nhms
integer :: nobs
integer :: ichn
integer :: ier

call init_(infiles,outfile,norm)

call open_satinfo_()

nobs=0
call read_enkf_obfile_(nobs,nymd=nymd,nhms=nhms)
print *, 'Found ', nobs, ' in file ', trim(infiles(1))

call init_enkf_obfile_(nobs)
nqcmax=17
call ods_init( ods, nobs, ier, nqc=nqcmax )
 if (ier/=0) then
    call die(myname,'ods_init() error')
 end if
call getodsmeta( ods )
call ncepQCXval( ods )

call read_enkf_obfile_(nobs)
call enkf2ods_(nobs)
print*, 'in main, nobs: ', ods%data%nobs

call ods_put ( outfile, 'post_analysis', nymd, nhms, ods, ier )
call ods_clean( ods, ier )

call final_enkf_obfile_

contains

subroutine enkf2ods_(nobs)

  integer,intent(in) :: nobs
  integer ii,iks,iobs

  integer n, kidsat, isat, indx
  logical satknown

  character(8),allocatable, dimension(:)   :: station
  character(len=20) isis
  character(len=10) dplat

  allocate(station(nobs))

  iks=0;iobs=0
  do ii=1,nobs

     select case(adjustl(trim(contents(ii)%obtype)))
     case ('ps')
        iks=iks+1
        iobs=iobs+1
        ods%data%lat(iobs)   = contents(ii)%lat          ! latitute     of obs (degrees)
        ods%data%lon(iobs)   = contents(ii)%lon          ! longitude    of obs (degrees)
        ods%data%lev(iobs)   = contents(ii)%pres         ! level        of obs (hPa)
        ods%data%kx(iobs)    = contents(ii)%stattype     ! data source index
        ods%data%kt(iobs)    = ktps2m                    ! data type   index
        ods%data%ks(iobs)    = iks                       ! sounding    index
        ods%data%xm(iobs)    = obs_missing               ! atomic metadata (depends on kx)
        ods%data%time(iobs)  = contents(ii)%time * 60    ! time (relative to the input/output)
        ods%data%obs(iobs)   = contents(ii)%ob           ! observation value (units depend on kt)
        ods%data%OmF(iobs)   = contents(ii)%obfit_prior  ! obs minus forecast (O-F) innovations
        ods%data%OmA(iobs)   = contents(ii)%obfit_post   ! obs minus analysis (O-A) residuals
        ods%data%qchist(iobs)= 0                         ! On-line QC history flag
        select case (contents(ii)%iused)
           case (1)
              ods%data%qcexcl(iobs)= 0                   ! Used observation
           case (0)
              ods%data%qcexcl(iobs)= X_PASSIVE           ! Not used by forward EnKF solver
           case(-1)
              ods%data%qcexcl(iobs)= X_ADV_LOCAL         ! Not contribute to ob-impact in backward EnKF solver
        end select
        !ods%data%Xvec(iobs)  = contents(ii)%osense_dry   ! dry-norm sensitivity
                             ! contents(ii)%osense_kin   ! kinetic-energy sensitivity
                             ! contents(ii)%osense_moist ! wet-norm sensitivity
     case ('t')
        iks=iks+1
        iobs=iobs+1
        ods%data%lat(iobs)   = contents(ii)%lat          ! latitute     of obs (degrees)
        ods%data%lon(iobs)   = contents(ii)%lon          ! longitude    of obs (degrees)
        ods%data%lev(iobs)   = contents(ii)%pres         ! level        of obs (hPa)
        ods%data%kx(iobs)    = contents(ii)%stattype     ! data source index
        ods%data%kt(iobs)    = ktTv                      ! data type   index
        ods%data%ks(iobs)    = iks                       ! sounding    index
        ods%data%xm(iobs)    = obs_missing               ! atomic metadata (depends on kx)
        ods%data%time(iobs)  = contents(ii)%time * 60    ! time (relative to the input/output)
        ods%data%obs(iobs)   = contents(ii)%ob           ! observation value (units depend on kt)
        ods%data%OmF(iobs)   = contents(ii)%obfit_prior  ! obs minus forecast (O-F) innovations
        ods%data%OmA(iobs)   = contents(ii)%obfit_post   ! obs minus analysis (O-A) residuals
        ods%data%qchist(iobs)= 0                         ! On-line QC history flag
        select case (contents(ii)%iused)
           case (1)
              ods%data%qcexcl(iobs)= 0                   ! Used observation
           case (0)
              ods%data%qcexcl(iobs)= X_PASSIVE           ! Not used by forward EnKF solver
           case(-1)
              ods%data%qcexcl(iobs)= X_ADV_LOCAL         ! Not contribute to ob-impact in backward EnKF solver
        end select
     case ('q')
        iks=iks+1
        iobs=iobs+1
        ods%data%lat(iobs)   = contents(ii)%lat          ! latitute     of obs (degrees)
        ods%data%lon(iobs)   = contents(ii)%lon          ! longitude    of obs (degrees)
        ods%data%lev(iobs)   = contents(ii)%pres         ! level        of obs (hPa)
        ods%data%kx(iobs)    = contents(ii)%stattype     ! data source index
        ods%data%kt(iobs)    = ktqq                      ! data type   index
        ods%data%ks(iobs)    = iks                       ! sounding    index
        ods%data%xm(iobs)    = obs_missing               ! atomic metadata (depends on kx)
        ods%data%time(iobs)  = contents(ii)%time * 60    ! time (relative to the input/output)
        ods%data%obs(iobs)   = contents(ii)%ob           ! observation value (units depend on kt)
        ods%data%OmF(iobs)   = contents(ii)%obfit_prior  ! obs minus forecast (O-F) innovations
        ods%data%OmA(iobs)   = contents(ii)%obfit_post   ! obs minus analysis (O-A) residuals
        ods%data%qchist(iobs)= 0                         ! On-line QC history flag
        select case (contents(ii)%iused)
           case (1)
              ods%data%qcexcl(iobs)= 0                   ! Used observation
           case (0)
              ods%data%qcexcl(iobs)= X_PASSIVE           ! Not used by forward EnKF solver
           case(-1)
              ods%data%qcexcl(iobs)= X_ADV_LOCAL         ! Not contribute to ob-impact in backward EnKF solver
        end select
        !ods%data%Xvec(iobs)  = contents(ii)%osense_dry   ! dry-norm sensitivity
                             ! contents(ii)%osense_kin   ! kinetic-energy sensitivity
                             ! contents(ii)%osense_moist ! wet-norm sensitivity
     case ('pw')
        iks=iks+1
        iobs=iobs+1
        ods%data%lat(iobs)   = contents(ii)%lat          ! latitute     of obs (degrees)
        ods%data%lon(iobs)   = contents(ii)%lon          ! longitude    of obs (degrees)
        ods%data%lev(iobs)   = contents(ii)%pres         ! level        of obs (hPa)
        ods%data%kx(iobs)    = contents(ii)%stattype     ! data source index
        ods%data%kt(iobs)    = kttpw                     ! data type   index
        ods%data%ks(iobs)    = iks                       ! sounding    index
        ods%data%xm(iobs)    = obs_missing               ! atomic metadata (depends on kx)
        ods%data%time(iobs)  = contents(ii)%time * 60    ! time (relative to the input/output)
        ods%data%obs(iobs)   = contents(ii)%ob           ! observation value (units depend on kt)
        ods%data%OmF(iobs)   = contents(ii)%obfit_prior  ! obs minus forecast (O-F) innovations
        ods%data%OmA(iobs)   = contents(ii)%obfit_post   ! obs minus analysis (O-A) residuals
        ods%data%qchist(iobs)= 0                         ! On-line QC history flag
        select case (contents(ii)%iused)
           case (1)
              ods%data%qcexcl(iobs)= 0                   ! Used observation
           case (0)
              ods%data%qcexcl(iobs)= X_PASSIVE           ! Not used by forward EnKF solver
           case(-1)
              ods%data%qcexcl(iobs)= X_ADV_LOCAL         ! Not contribute to ob-impact in backward EnKF solver
        end select
        !ods%data%Xvec(iobs)  = contents(ii)%osense_dry   ! dry-norm sensitivity
                             ! contents(ii)%osense_kin   ! kinetic-energy sensitivity
                             ! contents(ii)%osense_moist ! wet-norm sensitivity
     case ('gps')
        iks=iks+1
        iobs=iobs+1
        ods%data%lat(iobs)   = contents(ii)%lat          ! latitute     of obs (degrees)
        ods%data%lon(iobs)   = contents(ii)%lon          ! longitude    of obs (degrees)
        ods%data%lev(iobs)   = contents(ii)%pres         ! level        of obs (hPa)
        ods%data%kx(iobs)    = contents(ii)%stattype     ! data source index
        ods%data%kt(iobs)    = ktGPSb                    ! data type   index
        ods%data%ks(iobs)    = iks                       ! sounding    index
        ods%data%xm(iobs)    = obs_missing               ! atomic metadata (depends on kx)
        ods%data%time(iobs)  = contents(ii)%time * 60    ! time (relative to the input/output)
        ods%data%obs(iobs)   = contents(ii)%ob           ! observation value (units depend on kt)
        ods%data%OmF(iobs)   = contents(ii)%obfit_prior  ! obs minus forecast (O-F) innovations
        ods%data%OmA(iobs)   = contents(ii)%obfit_post   ! obs minus analysis (O-A) residuals
        ods%data%qchist(iobs)= 0                         ! On-line QC history flag
        select case (contents(ii)%iused)
           case (1)
              ods%data%qcexcl(iobs)= 0                   ! Used observation
           case (0)
              ods%data%qcexcl(iobs)= X_PASSIVE           ! Not used by forward EnKF solver
           case(-1)
              ods%data%qcexcl(iobs)= X_ADV_LOCAL         ! Not contribute to ob-impact in backward EnKF solver
        end select
        !ods%data%Xvec(iobs)  = contents(ii)%osense_dry   ! dry-norm sensitivity
                             ! contents(ii)%osense_kin   ! kinetic-energy sensitivity
                             ! contents(ii)%osense_moist ! wet-norm sensitivity
     case ('u','v')
        iks=iks+1
        iobs=iobs+1
        ods%data%lat(iobs)   = contents(ii)%lat          ! latitute     of obs (degrees)
        ods%data%lon(iobs)   = contents(ii)%lon          ! longitude    of obs (degrees)
        ods%data%lev(iobs)   = contents(ii)%pres         ! level        of obs (hPa)
        ods%data%kx(iobs)    = contents(ii)%stattype     ! data source index
        ods%data%kt(iobs)    = ktuu                      ! data type   index ATTENTION: both U and V in U
        ods%data%ks(iobs)    = iks                       ! sounding    index
        ods%data%xm(iobs)    = obs_missing               ! atomic metadata (depends on kx)
        ods%data%time(iobs)  = contents(ii)%time * 60    ! time (relative to the input/output)
        ods%data%obs(iobs)   = contents(ii)%ob           ! observation value (units depend on kt)
        ods%data%OmF(iobs)   = contents(ii)%obfit_prior  ! obs minus forecast (O-F) innovations
        ods%data%OmA(iobs)   = contents(ii)%obfit_post   ! obs minus analysis (O-A) residuals
        ods%data%qchist(iobs)= 0                         ! On-line QC history flag
        select case (contents(ii)%iused)
           case (1)
              ods%data%qcexcl(iobs)= 0                   ! Used observation
           case (0)
              ods%data%qcexcl(iobs)= X_PASSIVE           ! Not used by forward EnKF solver
           case(-1)
              ods%data%qcexcl(iobs)= X_ADV_LOCAL         ! Not contribute to ob-impact in backward EnKF solver
        end select
        !ods%data%Xvec(iobs)  = contents(ii)%osense_dry   ! dry-norm sensitivity
                             ! contents(ii)%osense_kin   ! kinetic-energy sensitivity
                             ! contents(ii)%osense_moist ! wet-norm sensitivity
     case ('spd')
        iks=iks+1
        iobs=iobs+1
        ods%data%lat(iobs)   = contents(ii)%lat          ! latitute     of obs (degrees)
        ods%data%lon(iobs)   = contents(ii)%lon          ! longitude    of obs (degrees)
        ods%data%lev(iobs)   = contents(ii)%pres         ! level        of obs (hPa)
        ods%data%kx(iobs)    = 120!contents(ii)%indxsat      ! data source index
        ods%data%kt(iobs)    = ktus10                    ! data type   index
        ods%data%ks(iobs)    = iks                       ! sounding    index
        ods%data%xm(iobs)    = obs_missing               ! atomic metadata (depends on kx)
        ods%data%time(iobs)  = contents(ii)%time * 60    ! time (relative to the input/output)
        ods%data%obs(iobs)   = contents(ii)%ob           ! observation value (units depend on kt)
        ods%data%OmF(iobs)   = contents(ii)%obfit_prior  ! obs minus forecast (O-F) innovations
        ods%data%OmA(iobs)   = contents(ii)%obfit_post   ! obs minus analysis (O-A) residuals
        ods%data%qchist(iobs)= 0                         ! On-line QC history flag
        select case (contents(ii)%iused)
           case (1)
              ods%data%qcexcl(iobs)= 0                   ! Used observation
           case (0)
              ods%data%qcexcl(iobs)= X_PASSIVE           ! Not used by forward EnKF solver
           case(-1)
              ods%data%qcexcl(iobs)= X_ADV_LOCAL         ! Not contribute to ob-impact in backward EnKF solver
        end select
        !ods%data%Xvec(iobs)  = contents(ii)%osense_dry   ! dry-norm sensitivity
                             ! contents(ii)%osense_kin   ! kinetic-energy sensitivity
                             ! contents(ii)%osense_moist ! wet-norm sensitivity
     case ('oz')
        iks=iks+1
        iobs=iobs+1
        ods%data%lat(iobs)   = contents(ii)%lat          ! latitute     of obs (degrees)
        ods%data%lon(iobs)   = contents(ii)%lon          ! longitude    of obs (degrees)
        ods%data%lev(iobs)   = contents(ii)%pres         ! level        of obs (hPa)
        ods%data%kx(iobs)    = 330!contents(ii)%indxsat      ! data source index (wired to MLS)
        ods%data%kt(iobs)    = kto3                      ! data type   index
        ods%data%ks(iobs)    = iks                       ! sounding    index
        ods%data%xm(iobs)    = obs_missing               ! atomic metadata (depends on kx)
        ods%data%time(iobs)  = contents(ii)%time * 60    ! time (relative to the input/output)
        ods%data%obs(iobs)   = contents(ii)%ob           ! observation value (units depend on kt)
        ods%data%OmF(iobs)   = contents(ii)%obfit_prior  ! obs minus forecast (O-F) innovations
        ods%data%OmA(iobs)   = contents(ii)%obfit_post   ! obs minus analysis (O-A) residuals
        ods%data%qchist(iobs)= 0                         ! On-line QC history flag
        select case (contents(ii)%iused)
           case (1)
              ods%data%qcexcl(iobs)= 0                   ! Used observation
           case (0)
              ods%data%qcexcl(iobs)= X_PASSIVE           ! Not used by forward EnKF solver
           case(-1)
              ods%data%qcexcl(iobs)= X_ADV_LOCAL         ! Not contribute to ob-impact in backward EnKF solver
        end select
        !ods%data%Xvec(iobs)  = contents(ii)%osense_dry   ! dry-norm sensitivity
                             ! contents(ii)%osense_kin   ! kinetic-energy sensitivity
                             ! contents(ii)%osense_moist ! wet-norm sensitivity
     case default ! CAUTION: this assumes everything else as radiance

!       print *, 'obfit_prior   = ', contents(ii)%obfit_prior    
!       print *, 'obsprd_prior  = ', contents(ii)%obsprd_prior   
!       print *, 'ensmean_obnobc= ', contents(ii)%ensmean_obnobc 
!       print *, 'ensmean_ob    = ', contents(ii)%ensmean_ob     
!       print *, 'ob            = ', contents(ii)%ob             
!       print *, 'oberrvar      = ', contents(ii)%oberrvar       
!       print *, 'lon           = ', contents(ii)%lon            
!       print *, 'lat           = ', contents(ii)%lat            
!       print *, 'pres          = ', contents(ii)%pres           
!       print *, 'time          = ', contents(ii)%time           
!       print *, 'oberrvar_orig = ', contents(ii)%oberrvar_orig  
!       print *, 'stattype      = ', contents(ii)%stattype       
!       print *, 'obtype        = ', contents(ii)%obtype       
!       print *, 'indxsat       = ', contents(ii)%indxsat        
!       print *, 'osense_kin    = ', contents(ii)%osense_kin     
!       print *, 'osense_dry    = ', contents(ii)%osense_dry     
!       print *, 'osense_moist  = ', contents(ii)%osense_moist   
 
!       Split the observing system name in sensor and platform
        indx = scan(contents(ii)%obtype,'_') ! CAUTION: assumes there is only one underscore separating sensor and platform
        isis  = lowercase(contents(ii)%obtype(     1:indx-1))
        dplat = lowercase(contents(ii)%obtype(indx+1:      ))

        if (isis=='avhrr3') isis = 'avhrr' ! CAUTION: hardwired: needs further work

!       Identify satellite ID
        call getsatid_(isat,isis_=contents(ii)%obtype,dplat_=dplat)

!       Check if the satellite ID is in ODS database
        satknown = .false.
        kidsat   = isat
        do n = 1, nsats
           if(trim(sats(n))==trim(isis))then
              satknown = .true.
              kidsat   = kidsat+idsats(n)
              exit
           else
             cycle
           endif
        end do
        if (.not.satknown .or. kidsat==0) then
           print *, myname, ': Cannot identify satellite type = ', trim(contents(ii)%obtype(1:indx-1)), ' kidsat = ', kidsat
           call exit(1)
        endif
        !print *, 'will read *'//trim(dplat)//'* for ', trim(contents(ii)%obtype), ' isat = ', isat, ' kidsat = ', kidsat
        !print*,contents(ii)%stattype, contents(ii)%obtype, contents(ii)%indxsat

        call get_channel_(contents(ii)%obtype,contents(ii)%stattype,ichn)

        iks=iks+1
        iobs=iobs+1
        ods%data%lat(iobs)   = contents(ii)%lat          ! latitute     of obs (degrees)
        ods%data%lon(iobs)   = contents(ii)%lon          ! longitude    of obs (degrees)
        ods%data%lev(iobs)   = ichn !contents(ii)%indxsat      ! satellite channel number
!       ods%data%kx(iobs)    = contents(ii)%stattype     ! data source index
        ods%data%kx(iobs)    = kidsat                    ! data source index
        ods%data%kt(iobs)    = ktTb                      ! data type   index
        ods%data%ks(iobs)    = iks                       ! sounding    index
        ods%data%xm(iobs)    = obs_missing               ! atomic metadata (depends on kx)
        ods%data%time(iobs)  = contents(ii)%time * 60    ! time (relative to the input/output)
        ods%data%obs(iobs)   = contents(ii)%ob           ! observation value (units depend on kt)
        ods%data%OmF(iobs)   = contents(ii)%obfit_prior  ! obs minus forecast (O-F) innovations
        ods%data%OmA(iobs)   = contents(ii)%obfit_post   ! obs minus analysis (O-A) residuals
        ods%data%qchist(iobs)= 0                         ! On-line QC history flag
        select case (contents(ii)%iused)
           case (1)
              ods%data%qcexcl(iobs)= 0                   ! Used observation
           case (0)
              ods%data%qcexcl(iobs)= X_PASSIVE           ! Not used by forward EnKF solver
           case(-1)
              ods%data%qcexcl(iobs)= X_ADV_LOCAL         ! Not contribute to ob-impact in backward EnKF solver
        end select
        !ods%data%Xvec(iobs)  = contents(ii)%osense_dry   ! dry-norm sensitivity
                             ! contents(ii)%osense_kin   ! kinetic-energy sensitivity
                             ! contents(ii)%osense_moist ! wet-norm sensitivity
     end select

     select case (norm)  
       case('txe')
         ods%data%xm  (iobs)  = sqrt(contents(ii)%oberrvar)! place sigO in xm slot
         ods%data%Xvec(iobs)  = contents(ii)%osense_dry    ! dry-norm sensitivity
       case('kxe')
         ods%data%xm  (iobs)  = sqrt(contents(ii)%oberrvar)! place sigO in xm slot
         ods%data%Xvec(iobs)  = contents(ii)%osense_kin    ! kinetic-energy sensitivity
       case('twe')
         ods%data%xm  (iobs)  = sqrt(contents(ii)%oberrvar)! place sigO in xm slot
         ods%data%Xvec(iobs)  = contents(ii)%osense_moist  ! wet-norm sensitivity
       case default 
         ods%data%Xvec(iobs)  = sqrt(contents(ii)%oberrvar)! sigO
     end select
  enddo

  if(iobs==0) then
     print *, myname, ': No observations extracted, aborting ... '
     call exit(1)
  else
     print *, 'Number of observations extracted: ', iobs
  endif

  ods%data%nobs=iobs

! Fix longitudes so they become ODS-compatible
! --------------------------------------------
  where ( ods%data%lon(1:nobs) > 180 ) 
     ods%data%lon(1:nobs) = ods%data%lon(1:nobs) - 360.
  end where

! call setsndx (ods%data%ks(1:nobs),ods%data%kx(1:nobs),station(1:nobs))

end subroutine enkf2ods_

subroutine read_enkf_obfile_(nobstot,nymd,nhms)
  integer,intent(inout) :: nobstot
  integer,intent(  out),optional :: nymd,nhms
  integer lu, ii

  lu=luavail()
  open(lu,file=trim(infiles(1)),form='unformatted')
  read(lu) header
  if(nobstot==0) then
    nobstot = header%obsnum
    nymd = header%idate/100
    nhms = (header%idate-nymd*100)*10000
    close(lu)
    return
  endif
  do ii=1,nobs
     read(lu) contents(ii)
  enddo
  close(lu)

end subroutine read_enkf_obfile_

subroutine init_enkf_obfile_(nobs)
  integer, intent(in) :: nobs
  allocate(contents(nobs))
end subroutine init_enkf_obfile_

subroutine final_enkf_obfile_
  deallocate(contents)
end subroutine final_enkf_obfile_

subroutine init_(ifiles,ofile,norm)

   character(len=*), intent(inout) :: ifiles(:)
   character(len=*), intent(out)   :: ofile
   character(len=*), intent(out)   :: norm

   integer iret, i, iarg, argc, iargc
   character(len=255) :: argv
   integer nfiles

!  Parse command line
!  ------------------
   ofile = 'DEFAULT'
   norm  = 'null'
   argc =  iargc()
   if ( argc .lt. 1 ) call usage_()

   iarg = 0
   nfiles = 0

   do i = 1, 32767
      iarg = iarg + 1
      if ( iarg .gt. argc ) exit
      call GetArg ( iarg, argv )
      select case (argv)
        case ("-norm")
          if ( iarg+1 .gt. argc ) call usage_()
          iarg = iarg + 1
          call GetArg ( iarg, norm )
        case ("-o")
          if ( iarg+1 .gt. argc ) call usage_()
          iarg = iarg + 1
          call GetArg ( iarg, ofile )
        case default
          nfiles = nfiles + 1
          if ( nfiles .gt. mfiles ) call die(myname,'too many obs sens files')
          infiles(nfiles) = argv
      end select
   end do

   if ( nfiles .lt. 1 ) call usage_()

end subroutine init_

subroutine getsatid_(myisat,isis_,dplat_)
!  Riped from ODS; mildly changed interface
!  RT: some heck that needs more work
!  RT: this needs serious attention as it is becoming a huge heck now (3/30/09)
   implicit none
   integer, intent(out) :: myisat
   character(len=20), intent(in), optional :: isis_ ! sensor/instrument/satellite id  ex.amsua_n15
   character(len=10), intent(in), optional :: dplat_

   integer ios, i

   myisat = 0  ! take fixed sat index as in idsats

!  select case( trim(ladjust(dplat_)) )
!  case ('aura')
!     myisat = 999
!     return
!  end select

!  first handle precip types
   i = index('pcp',isis_(1:3))
   if (i>0) then
      i = index('trmm',dplat_(1:4))
      if (i>0) then
         if (dplat_(6:8) == 'lnd') then
            myisat = 1
         else if (dplat_(6:8) == 'ocn') then
            myisat = 2
         else
            myisat = 0
         end if
         return
      else
         read(dplat_(5:6),'(i2)',iostat=ios)myisat
         return
      end if
   end if

!  Need to distinguish between AMSUA from AQUA and METOP for example 
!  (NOAA sats are already distinguished)
   i = index('metop',dplat_(1:5))
   if(i>0)then
      myisat = 25 + iachar(dplat_(7:7)) - iachar('a')  
      return
   endif
   i = index('tiros',dplat_(1:5))
   if(i>0)then
      myisat = 5
      return
   endif
!  i = index('aqua',dplat_(1:4))
!  if(i>0)then
!     return
!  endif
!  the "word" fgnm stands for the platforms dmsp/goes/noaa/meteosat this needs generalization
!  e.g. dmsp -> f15   goes -> g12   noaa -> n18  meteosat -> m09
   i = index('fgnm',dplat_(1:1))
   if(i>0)then; read(dplat_(2:3),'(i2)',iostat=ios)myisat; endif

end subroutine getsatid_

subroutine open_satinfo_

 character(len=1):: cflg
 character(len=120) crecord

 integer,allocatable,dimension(:) :: jused
 integer istat,j,n,lu,nlines,nused,istats

 allocate(nusis(1),nuchan(1),iuse_rad(1),&
          icld_det(1),varch(1),varch_cld(1),&
          ermax_rad(1),b_rad(1),pg_rad(1))

 lu=luavail()
 open(lu,file='satinfo',form='formatted')
 nlines=0;nused=0
 read1:  do
    read(lu,'(a1,a120)',iostat=istat) cflg,crecord
    if (istat /= 0) exit
    nlines=nlines+1
    if (cflg == '!') cycle
    read(crecord,*,iostat=istat) nusis(1),nuchan(1),iuse_rad(1),&
         varch(1),varch_cld(1),ermax_rad(1),b_rad(1),pg_rad(1),icld_det(1)
    if(iuse_rad(1)>0) nused = nused + 1
 end do read1
 rewind(lu)
 allocate(jused(nlines))
 nlines=0
 read2:  do
    read(lu,'(a1,a120)',iostat=istat) cflg,crecord
    if (istat /= 0) exit
    nlines=nlines+1
    if (cflg == '!') cycle
    read(crecord,*,iostat=istat) nusis(1),nuchan(1),iuse_rad(1),&
         varch(1),varch_cld(1),ermax_rad(1),b_rad(1),pg_rad(1),icld_det(1)
    jused(nlines) = iuse_rad(1)
 end do read2
 deallocate(nusis,nuchan,iuse_rad,&
            icld_det,varch,varch_cld,&
            ermax_rad,b_rad,pg_rad)

! allocate for actual read-and-keep
  allocate(nusis(nused),nuchan(nused),iuse_rad(nused),&
           icld_det(nused),varch(nused),varch_cld(nused),&
           ermax_rad(nused),b_rad(nused),pg_rad(nused))
  rewind(lu)
  j=0
  do n=1,nlines
     read(lu,'(a1,a120)') cflg,crecord
     if (cflg == '!') cycle
     if (jused(n)>0) then
         j=j+1
         read(crecord,*,iostat=istat) nusis(j),nuchan(j),iuse_rad(j),&
              varch(j),varch_cld(j),ermax_rad(j),b_rad(j),pg_rad(j),icld_det(j)
         if(j<20) print *, nusis(j), nuchan(j)
     else
         read(crecord,*,iostat=istat)
     endif
  enddo
  close(lu)

end subroutine open_satinfo_

subroutine close_satinfo_
 deallocate(nusis,nuchan,iuse_rad,&
            icld_det,varch,varch_cld,&
            ermax_rad,b_rad,pg_rad)
end subroutine close_satinfo_

subroutine get_channel_(obtype,stattype,ichn)
character(len=*),intent(in) :: obtype
integer,intent(in) :: stattype
integer,intent(out):: ichn
integer nb,n
integer, allocatable, dimension(:) :: find
ichn=-1
nb=minloc(nusis,1,mask=nusis==obtype)
n=count(nusis==obtype)
if(n==0) then
  print *, 'error in get_channel'
  stop
endif
allocate(find(n))
find=nuchan(nb:nb+n-1)
ichn=find(stattype)
if (ichn<=0) then
   print *, nb,n, ichn, obtype, stattype
   print *, find, size(nusis)
   print *, 'error, aborting'
   stop
endif
deallocate(find)
end subroutine get_channel_

subroutine usage_ 
   print *
   print *, 'Usage:'
   print *
   print *, 'enkf_obs2ods.x [-o ID] obs_sens_file'
   print *
   print *, 'where'
   print *
   print *,'-o ID             use ID for naming output files'
   print *,'                    (default: DEFAULT)'
   print *,'-norm NORM        twe,txe,kxe (DEFAUL: null)'
   print *,' obs_sens_file    observation sensitivity EnKF output'
   print *
   print *
   print *, 'NOTE: obs_sens file assumed to contain obs for a single syn time'
   print *
   call exit(1)
end subroutine usage_ 

end program enkf_obs2ods
