!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_ana --- Implements the AOD Analyzer model
!
! !INTERFACE:
!

   MODULE  m_ana

! !USES:

   use  Chem_BundleMod                      ! Gridded AOD Fields

   use  m_ods                               ! ODS vector
   use  m_AISolver                          ! Main interface to PSAS solver
   use  m_getAI, only: GetAI_setAOD_sigF    ! for PSAS AOD errors

   use  m_inpak90                           ! resources
   use  m_mpout                             ! log messages to stdout
   use  m_die                               ! error messages
   use  m_zeit                              ! Timing

   use  m_odsmeta, only: ktWW, ktAOD, ktLogAOD  
   use  m_obs, only: log_transf, eps        ! whether analysis variable is AOD or 
                                            !  log-transformed AOD.

   Implicit NONE

!
! !PUBLIC MEMBER FUNCTIONS:
!
   PRIVATE
   PUBLIC  Analyzer                         ! Produces gridded analysis
                                            !  increments

!
! !DESCRIPTION: This module implements the {\tt Analyzer} for the
!               Physical-space/Finite-volume DAS. Given called controled
!  innovations and first guess, it uses PSAS to produce an analyzed
!  state.
!
! !REVISION HISTORY:
!
!  13feb2010  da Silva  Adapted from GEOS-4 analyzer for AOD.
!
!EOP
!-------------------------------------------------------------------------

!BOC

!   Resource file
!   -------------
    character(len=*), parameter :: rcenvar = 'ANARC'   ! environment RC var
    character(len=*), parameter :: rcfname = 'ana.rc'  ! default RC file name
    character(len=255)          :: thisrc              ! actual rc file name

!   Analysis level assigned to AOD observations
!   TO DO: Change psas.rc as to correctly interpret channels as levels
!   ------------------------------------------------------------------
!!!     real*8, parameter          :: levAOD = 850.      ! (hPa)

!EOC

CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOdP
!
! !ROUTINE: Analyzer --- Produces AOD analysis increments from QC innovations
!
! !INTERFACE:
!
    subroutine Analyzer ( ods, nobs_ana, im_so, jm_so, dy_a, chAOD, do_avk, y_k, skipPSAS )

! !USES:
!
    use  m_GetAI, only : GetAI                 ! PSAS interface

    Implicit NONE

!
! !INPUT PARAMETERS:
!
    type(ods_vect), intent(inout) ::  ods       ! ods vector: QC'ed O-F

    integer,           intent(in) ::  nobs_ana  ! no. of obs to analyze

    integer,           intent(in) ::  im_so, jm_so ! superob dimension

    real*8,            intent(in) ::  chAOD(2)  ! Range of AOD waenumbers (nm) to analyze

    logical,        intent(in)    ::  do_avk    ! whether to calculate averagig kernel

    logical,        intent(in), OPTIONAL ::  skipPSAS  ! skip PSAS call

!
! !INPUT/OUTPUT PARAMETERS:
!
    type(Chem_Bundle), intent(inout) ::  dy_a   ! AOD analysis increments
    type(Chem_Bundle), intent(inout) ::  y_k    ! Approximate averaging kernel

!
! !DESCRIPTION: This routine implements the {\tt Analyzer} for the PSAS 
!  Aerosol Optical Depth (PSAS/AOD) system. Given quality controlled
!  innovations and first guess, it uses PSAS to produce an analyzed state.
!  This AOD implementation updates one layer at a time, each layer
!  corresponding to a single wavenumber (and not vertical layer!).
!
!  The actual analysis variable can be AOD or log-transformed AOD as determined
!  by the Observer. The analysis increment by this routine will be consistently
!  on the same variable.
!
! !REVISION HISTORY:
!
!  13feb2010  da Silva   Adapted for AOD from GEOS-4 code.
!
!EOP
!-------------------------------------------------------------------------

    character(len=*), parameter ::  myname = 'Analyzer'


!                          ----------------
!                          LOCAL WORK SPACE
!                          ----------------

!   Control variable coordinates
!   ----------------------------
    integer              :: im, jm, km

!   Local ODS for channel subseting
!   -------------------------------
    type(ODS_vect)       :: ods_

!   Workspace
!   ---------
    real*8, allocatable  :: tlev(:)        ! levs passed to PSAS 
    real*8, allocatable  :: ttim(:)        ! PSAS time array != ODS time
    real*8, allocatable  :: tomf(:)        ! OmF array passed to PSAS (maybe
                                           !     a scaled version of ODS%OmF
    real*8, allocatable  :: txmso(:)       ! SuperOb weights (set to 1 for now)
    real*8, allocatable  :: txvec(:)       ! PSAS CG solution
    real*8, allocatable  :: tlon(:)        ! longitudes (degrees)
    real*8, allocatable  :: tlat(:)        ! latitudes (degrees)
    integer, allocatable :: tkt(:)         ! Data types accepted by PSAS
    integer, allocatable :: tkx(:)         ! Data sources accepted by PSAS
    integer, allocatable :: tks(:)         ! sounding index 

!   Analysis increment attributes (flat, obs-like data structure)
!   -------------------------------------------------------------
    integer              :: ninc
    real*8, allocatable  :: alon(:), alat(:), alev(:), ainc(:)
    integer, allocatable :: akts(:)

    real :: channel

!   Etc
!   ---
    character(len=255)   :: msg
    integer              :: rc, ios, n, i, j, k, nbins
    logical              :: doPSAS              ! if true, call PSAS

!                          ----------------

!   Check if any obs to analyze
!   ---------------------------
    if ( nobs_ana .eq. 0 ) then
         call warn ( myname, 'no observations: setting w_a=w_f.' )
         return
    end if

!   Grid dimensions
!   ----------------
    im = dy_a%grid%im;  jm = dy_a%grid%jm;   km = dy_a%grid%km

!   By default, it will call PSAS
!   -----------------------------
    if ( present(skipPSAS) ) then
       doPSAS = .not. skipPSAS
    else
       doPSAS = .true.
    end if

!   Datatype check
!   --------------
    if ( log_transf ) then
       if ( any(ods%data%kt(1:nobs_ana) .ne. ktLogAOD) ) &
            call die ( myname, 'must only pass LogAOD data' )
    else
       if ( any(ods%data%kt(1:nobs_ana) .ne. ktAOD) ) &
            call die ( myname, 'must only pass AOD data' )
    end if

!   Generate coordinates for PSAS analysis increments using flat,
!   observation-like vectors of attributes.
!   ------------------------------------------------------------
    ninc = im * jm
    allocate ( alon(ninc), alat(ninc), alev(ninc), akts(ninc), ainc(ninc), &
         stat=ios )
    if ( ios .ne. 0 ) call die ( myname, 'allocate error' )

!   Pretend that AOD data are MIXR data
!   ------------------------------------
    n = 0
    do j = 1, jm
       do i = 1, im
          n = n + 1
          akts(n) = ktWW     ! to trick PSAS into thinking that this is moisture
          alon(n) = dy_a%grid%lon(i,1)
          alat(n) = dy_a%grid%lat(1,j)
       end do
    end do

!   Initialize increments to zero
!   -----------------------------
    dy_a%qa(1)%data3d(:,:,:) = 0.0
    
!   Either call PSAS for producing the analysis increments ...
!   -----------------------------------------------------------
    if ( doPSAS ) then

!      Set errors to be used by superobbing
!      ------------------------------------
       call GetAI_setAOD_sigF(.True.)

!      Loop over channels, analyzing one at a time for economy
!      -------------------------------------------------------
       do k = 1, km

          channel = dy_a%grid%lev(k)
          alev = channel  ! ainc for this channel

!         Analyze only those channels in range
!         ------------------------------------
          if ( (dy_a%grid%lev(k) < chAod(1)) .OR. &
               (dy_a%grid%lev(k) > chAod(2))      )  cycle

!         Select observations for this channel
!         ------------------------------------
          call ODS_Select ( ods, nobs_ana, n, rc, & 
               lev = channel, qcexcl = 0, odss=ods_ )
          if ( rc /=0 ) call die(myname,'cannot perform ODS select')
          print *, myname//": Analyzing channel ", int(channel), " with ", &
                n, " out of ", nobs_ana

!         No obs, nothing to do
!         ---------------------
          if ( n==0 ) then
               dy_a%qa(1)%data3d(:,:,k) = 0.0
               if ( do_avk ) y_k%qa(1)%data3d(:,:,k) = 0.0
               call ODS_Clean(ods_,rc)
               cycle
          end if

!         Set attributes to be passed to PSAS. Here we trick PSAS into thinking
!         that we are analyzing  moisture data from RAOB at 850 hPa
!         TO DO: Update PSAS rc file so that we it can recognize kx corresponding
!                to different AOD instruments: MODIS, MISR, etc
!         ---------------------------------------------------------------------
          allocate ( tlev(n), ttim(n), tkt(n), tkx(n), tomf(n), txmso(n), &
                     txvec(n), tlon(n), tlat(n), tks(n), stat = ios )
          if ( ios .ne. 0 ) call die ( myname, 'allocate error' )       

!         Compute super obs and set obs attributes
!         Superob box is hardwired to 1/4 to size of gridbox
!         --------------------------------------------------
          call superOb_ ( im_so, jm_so, n, ttim, txmso, &
                          tlev, txvec, tlon, tlat, tomf, tkt, tkx,  &
                          tks, nbins, ods_ )

!         Calculate analysis increments with PSAS
!         ---------------------------------------
          print *, myname//": After SuperOb, analyzing channel ", &
                   int(channel), " with ", &
                   nbins, " out of ", nobs_ana
                                                                                  call zeit_ci ( 'getAI' )
          call getAI ( ninc, ainc, alat, alon, alev, akts,                     &
                       nbins, txvec, tlat, tlon, & 
                       tlev, ttim, tkx, tks, tkt, txmso, tomf )
                                                                                  call zeit_co ( 'getAI' )

          print *, '>>> ainc = ', minval(ainc), maxval(ainc)

          dy_a%qa(1)%data3d(:,:,k) = reshape(ainc,(/ im, jm /))

!         Averaging kernel
!         ----------------
          if ( do_avk ) then
                                                                                  call zeit_ci ( 'getAI' )
               tomf = 1.0
               call getAI ( ninc, ainc, alat, alon, alev, akts, &
                            nbins, txvec, tlat, tlon,           & 
                            tlev, ttim, tkx, tks, tkt, txmso, tomf )
                                                                                  call zeit_co ( 'getAI' )
               y_k%qa(1)%data3d(:,:,k) = reshape(ainc,(/ im, jm /))

          end if

!         Clear some memory
!         -----------------
          deallocate ( tlev, ttim, tkt, tkx, tomf, txmso, &
                       txvec, tlon, tlat, tks )
          call ODS_Clean(ods_,rc)

       end do ! loop over channels

    else
       
       print *, myname//': skipping PSAS ...'

    end if ! doing PSAS

!   Free memory of local workspace no longer needed
!   -----------------------------------------------
    deallocate ( akts, ainc, alon, alat, alev )  ! done with this

!   All done
!   --------
    return

  end subroutine Analyzer



!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: superOb_ --- Bins obs on lat/lon boxes
!
! !INTERFACE:


    subroutine superOb_ ( im, jm, nobs, ttim, txmso, &
                          tlev, txvec, tlon, tlat, tomf, tkt, tkx,  &
                          tks, nbins, ods )

    Implicit NONE
    integer, intent(in)            :: im, jm
    integer, intent(in)            :: nobs

!   Workspace (used to hold superobbed data)
!   ----------------------------------------
    real*8, intent(inout)    :: ttim(:)        ! PSAS time array != ODS time
    real*8, intent(inout)    :: txmso(:)       ! SuperOb weights (set to 1 for now)
    real*8, intent(inout)    :: tlev(:)        ! level
    real*8, intent(inout)    :: txvec(:)       ! temporary XVEC 
    real*8, intent(inout)    :: tlon(:)        ! temporary LON
    real*8, intent(inout)    :: tlat(:)        ! temporary LAT
    real*8, intent(inout)    :: tomf(:)        ! temporary OMF
    integer, intent(inout) :: tkt(:)         ! temporary KT
    integer, intent(inout) :: tkx(:)         ! temporary KX
    integer, intent(inout) :: tks(:)         ! temporary KS

    type(ods_vect), intent(inout)  :: ods
    integer, intent(out)           :: nbins ! number of binned observations


!
! !DESCRIPTION: Bins the skin temperature on lat/lon boxes with
!               resolution given by the state vector dimensions im/jm.
!  It also applies a simple background test on the O-F.
!
! !REVISION HISTORY:
!
!  29Mar2002 da Silva   First crack.
!  04Mar2003 Todling    Redefined kt to be ktSkinT instead of ktWW.
!  28mar2003 da Silva   Quasi-equal area grid.
!
!EOP
!-------------------------------------------------------------------------

  character(len=*), parameter ::  myname = 'superOb_'

  real*8 dlon, dlon_j, dlat, pi, d2r, r2d, alfa, y, ytresh, rnorm
  integer n, i, j, im_j, ier
  character(len=255) :: msg
  real*8 :: xobs(nobs),yobs(nobs),zobs(nobs) ! x-y-z

! Accumulators
! ------------
  real*8    :: accum(im,jm)
  real*8    :: xcoord(im,jm), ycoord(im,jm), zcoord(im,jm)
  real*8    :: gain(nobs), sigF(nobs), sigOc(nobs), sigOu(nobs)
  real*8    :: weight(im,jm)
  real*8    :: channel
  real*8    :: r8_lon(nobs), r8_lat(nobs), r8_lev(nobs)

! Check level consistency
! -----------------------
  channel = ods%data%lev(1)
  if ( any(ods%data%lev(1:nobs) /= channel ) ) then
     print *, myname//": levels are not the same, lev = ", &
          minval(ods%data%lev), maxval(ods%data%lev)
     call die(myname,'failed level test',ier)
  end if

! Type conversion
! ---------------
  r8_lon(1:nobs) = ods%data%lon(1:nobs)
  r8_lat(1:nobs) = ods%data%lat(1:nobs)
  r8_lev(1:nobs) = ods%data%lev(1:nobs)

! Get sigOs from PSAS to and compute gain
! ---------------------------------------
  tkt = 7 ! to trick psas
  call PSAS_sigs ( nobs, r8_lat, r8_lon, r8_lev, &
                   ods%data%kx, tkt, sigF, sigOc, sigOu )
  gain = sigF**2 / ( sigF**2 + sigOc**2 + sigOu**2 ) ! scalar gain

! Compute cartesian (x,y,z) coord. on unity sphere:
!            x = cos(lat) * cos(lon)
!            y = cos(lat) * sin(lon)
!            z = sin(lat)
! ------------------------------------------------
  call LL2XYZ ( r8_lon, r8_lat, nobs, xobs, yobs, zobs, ier )
  if(ier.ne.0) call die(myname,'error on return from LL2XYZ()',ier)

! Initialize
! ----------
  accum  = 0.0
  weight  = 0.0
  xcoord = 0.0
  ycoord = 0.0
  zcoord = 0.0
  ytresh = 30. ! quasi-equal area poleward of this, hardwired for now

! For each good observation, bin it
! ---------------------------------
  dlon = 360.0 / im
  dlat = 180.0 / ( jm - 1 ) 
  pi   = 4. * atan(1.0)
  d2r  =  pi / 180.
  r2d  = 180./ pi
  alfa = (im-1.0) / cos(d2r*ytresh)
  do n = 1, nobs

    j = 1 + nint ( (r8_lat(n)+ 90.)/dlat ) 
    y =  -90.0 + (j-1) * dlat

    if ( ods%data%qcexcl(n) .eq. 0 ) then

!   Determine quasi equal area zonal mesh size
!   ------------------------------------------
    if ( abs (y) < ytresh ) then
         dlon_j = dlon
    else
         im_j = min ( im, int(1+alfa*cos(d2r*y) ) )
         dlon_j = 360. / im_j
    end if

!   Find grid box for observations
!   ------------------------------
    i = 1 + nint ( (r8_lon(n)+180.)/dlon_j )

     if ( i .gt. im ) i = i - im ! wrap around

     if ( i<1 .or. i>im .or. j<1 .or. j>jm ) then
          write(msg,'(a,4i4,2f10.4)') 'invalid i/j - strange!', &
               i, im, j, jm, r8_lon(n), r8_lat(n)
          call die ( myname, trim(msg) ) ! should never happen 
     end if

     accum(i,j)  =  accum(i,j) + gain(n) * ods%data%omf(n)
     xcoord(i,j) = xcoord(i,j) + gain(n) * xobs(n)
     ycoord(i,j) = ycoord(i,j) + gain(n) * yobs(n)
     zcoord(i,j) = zcoord(i,j) + gain(n) * zobs(n)
     weight(i,j)  =  weight(i,j) + gain(n)

    end if

  end do

!!!  if ( count(weight>0.0) > nobs ) &
!!!       call die ( myname, 'too many reduced obs - strange!' )

! Next, create stream of superobs for PSAS
! ----------------------------------------
  n = 0
  do j = 1, jm
     do i = 1, im
        if ( weight(i,j) > 0.0 ) then
           n = n + 1

!          Determine quasi equal area zonal mesh size
!          ------------------------------------------
           y =  -90.0 + (j-1) * dlat
           if ( abs (y) < ytresh ) then
              dlon_j = dlon
           else
              im_j = min ( im, int(1+alfa*cos(d2r*y) ) )
              dlon_j = 360. / im_j
           end if

           ttim(n) = 0.0
           tlev(n) = channel
           tkx(n)  = 7     ! fake RAOB
           tkt(n)  = ktAOD ! Note: it may seem strange but we really
                           !       mean ktAOD, even when analyzing
                           !       the log-transform (ktLAOD). The reason
                           !       is that  getAI will turn this into
                           !       moisture anyway to trick PSAS.
           tks(n)  = n
           txmso(n) = 1.0 ! do not reset as weights are already large enough
           tomf(n) = accum(i,j) / weight(i,j)

!          mean cartesian coordinates (may not be on unit sphere)
!          ------------------------------------------------------
           xcoord(i,j) = xcoord(i,j) / weight(i,j)
           ycoord(i,j) = ycoord(i,j) / weight(i,j)
           zcoord(i,j) = zcoord(i,j) / weight(i,j)

!          Project averaged coordinates back on unit sphere
!          ------------------------------------------------
           rnorm = sqrt( xcoord(i,j)**2 + ycoord(i,j)**2 + zcoord(i,j)**2 )
           xcoord(i,j) = xcoord(i,j) / rnorm
           ycoord(i,j) = ycoord(i,j) / rnorm
           zcoord(i,j) = zcoord(i,j) / rnorm

!          Mean lon/lat
!          ------------
           tlon(n) = r2d * atan2(ycoord(i,j), xcoord(i,j) )
           tlat(n) = r2d * asin(zcoord(i,j))

!!!           tlon(n) = -180.0 + (i-1) * dlon_j
!!!           tlat(n) =  y

        end if
     end do
  end do

  nbins = n

#ifdef DEBUG

  open(77,file='superob.txt')
  do i = 1, n
     write(77,*) tlon(i), tlat(i), tomf(i)
  end do
  close(77)

#endif

  end subroutine superOb_

#if 0

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: InitPSASerr__ --- Generate sigf for all control analysis variables
!
! !INTERFACE:

    subroutine initPSASerr_()

! !USES:

   use  m_sigFi_lookups, only : sigFi_store ! Sigf redefinition for PSAS
   use  m_sigFi_lookups, only : mixre_defined, hghte_defined
   use  m_sigFi_lookups, only : MIXRE, HGHTE

    Implicit NONE

!
! !OUTPUT PARAMETERS:
!

!
! !DESCRIPTION: Creates 3D gridded fcst error stdv table
!               and passes it to PSAS.
!
!  IMPORTANT: This is a copy
!
!
! !REVISION HISTORY:
!
!  17feb2010  da Silva  Adapted for AOD analysis
!
!EOP
!-------------------------------------------------------------------------

    character(len=*), parameter ::  myname_ = 'setPSASerr_'

!   3D array for sigf (for now, depends on pressure only)
!   -----------------

    real*8 :: sigf(2,2,4)  ! need only four lon/lat pairs per level

!   table definition:
!   ----------------
    real*8 :: p(4) = (/ 870., 660., 550., 470. /)
    real*8 :: s(4) = (/ 0.45, 0.45, 0.45, 0.45 /)

    integer :: i, j, k


!   Check if already defined
!   ------------------------
    if ( mixre_defined .or. hghte_defined ) return

!   Get fcst error stdv
!   -------------------
    do i = 1, 2
       do j = 1, 2
          do k = 1, 4
               sigf(i,j,k) = s(k)
          end do
       end do
    end do

!   tell PSAS to use values in sigf to override resource
!   ----------------------------------------------------
    call sigFi_store ( HGHTE, 2, 2, 4, p, sigf ) ! not needed for AOD analsis
    call sigFi_store ( MIXRE, 2, 2, 4, p, sigf )

    write(*,*) &
      myname_//': Defined fcst error std dev for PSAS, sigF = ', &
      s(:)

  end subroutine initPSASerr_

#endif

end module m_ana

