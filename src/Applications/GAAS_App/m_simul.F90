!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_simul --- Implements aerosol simulator
!
! !INTERFACE:
!

   MODULE m_simul

! !USES:

   use  Chem_Mod

   use  m_const, only: UNDEF                ! missing value

   use  m_odsmeta, only: ktAOD, ktLogAOD, X_SIMUL, H_SIMUL

   use  m_ods                               ! ODS vector

   use  m_die                               ! abnormal exiting
   use  m_mpout                             ! log messages to stdio

   Implicit NONE


!
! !PUBLIC MEMBER FUNCTIONS:
!   use  m_const,   only: UNDEF              ! missing value


   PRIVATE
   PUBLIC  Simulator                        ! Simulates observations and
                                            !  computes o-Hw

!
! !DESCRIPTION: This module implements the {\tt Simulator} which ``simulates''
!               the observations from the model state. In the current
!  version the model fields are simply interpolated to observation location
!  using the {\tt m\_insitu} module.
!
! !REVISION HISTORY:
!
!  03oct2005  da Silva  Derived from GEOS-4 simulator (SPMD version).
!
!EOP
!-------------------------------------------------------------------------

        real*8, parameter :: tol = 1e-5

CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Simulator --- Simulate observations from model fields
!
! !INTERFACE:
!
   subroutine Simulator ( w_f, y, Iv, nobs, noqc, log_transf )

! !USES:

    Implicit NONE

!
! !INPUT PARAMETERS:
!
    integer,           intent(in)   :: nobs  ! number of observations
    type(Chem_Bundle),  intent(in)  :: w_f   ! model fields

    logical, intent(in), optional   :: noqc  ! if true, do not write qc marks
    logical, intent(in), optional   :: log_transf  ! if true, variable is 
                                                   ! log-transformed AOD
                                                   ! (default is false)


!
! !INPUT/OUTPUT PARAMETERS:
!
    type(obs_vect),   intent(inout) :: y     ! observation vector
!
! !OUTPUT PARAMETERS:
!
    real,             intent(inout) :: Iv(nobs) ! observation residual

!
!
! !DESCRIPTION: This routine simulates observations from the model state,
!               and computes the residuals between observation and
!               simulated observation, viz.
!  $$
!                        v = w^o - Hw
!  $$
!  which is the observed-minus-forecast residual (innovation) when $w = w^f$.
!  In the current version of the system, the observation operator $H$
!  is simply the (linear) interpolation operator implemented in
!  {\tt m\_insitu}.
!
! !REVISION HISTORY:
!
!  03oct2005  da Silva  Derived from GEOS-4 simulator (SPMD version).
!
!EOP
!-------------------------------------------------------------------------

    character(len=*), parameter :: myname = 'Simulator'

    integer i, m, n, ier, ios, opt, nobs_kt, nobs_u, nobs_v
    logical qc

    integer, allocatable  ::  kid(:), ks(:)
    real*8,  allocatable  ::  lat(:), lon(:), lev(:), obs(:), xm(:)
    real*8,  allocatable  ::  conf(:), Iw(:)
    logical, allocatable  ::  simulated(:)

    integer :: ktAOD_

    character(len=255) msg

!   Nobs=0 check
!   ------------
    if ( nobs .eq. 0 ) then
       call warn ( myname, 'no observations, nothing to do.' )
       return
    end if

!   Variable is AOD or log-transformed AOD
!   --------------------------------------
    ktAOD_ = ktAOD ! by default variable is AOD
    if  ( present(log_transf) ) then
       if ( log_transf ) ktAOD_ = ktLogAOD
    end if

!   Allocate local workspace
!   ------------------------
    allocate ( simulated(nobs), lat(nobs), lon(nobs), lev(nobs),   &
               kid(nobs), ks(nobs), obs(nobs), xm(nobs), Iw(nobs), &
               conf(nobs), stat=ios )
    if ( ios .ne. 0 ) call die ( myname, 'cannot allocate memory' )

!   Decide whether simulator should write qc marks
!   ----------------------------------------------
    if ( .not. present(noqc) ) then
          qc = .true.
    else
          qc = ( .not. noqc )
    end if

    simulated = .false.  ! start by setting all obs as not yet simulated
    opt = 0              ! default interpolation option, for now

!   Aerosol Optical Depth
!   ---------------------
    nobs_kt = 0
    call Gather_ ( ktAOD_, nobs_kt )
    if ( nobs_kt .eq. 0 ) then
         if ( ktAOD_ == ktAOD )    call warn ( myname, 'no AOD observations' )
         if ( ktAOD_ == ktLogAOD ) call warn ( myname, 'no log-transformed AOD observations' )
    else
       call Insitu_ ( w_f, lon, lat, lev, nobs_kt, 'aod', opt,   &
                      Iw, conf, ier )
       if ( ier .ne. 0 ) then
            print *, '*** error *** on return from Insitu_, rc = ', ier
            call die ( myname, 'could not simulate AOD' )
       end if
       call Scatter_ ( '    AOD', 1, nobs_kt, simulated )
    end if

    if ( qc ) then    ! the counts in this section only make sense if
                      ! qc marks were actually written by the simulator

!   If an observation could not be simulated, then exclude it
!   ---------------------------------------------------------
       n = 0
       do i = 1, nobs
             if ( .not. simulated(i) ) then
                  n = n + 1
                  y%qcexcl(i) = X_SIMUL
             end if
       end do

       if ( n .gt. 0 ) then
          write(msg,'(i8,a,i8,a)') n, ' obs (out of ', nobs, &
                                      ') have not been simulated'
          call mpout_log ( myname, trim(msg) )
       end if

    end if

!   De-allocate memory
!   ------------------
    deallocate ( simulated, lat, lon, lev,  &
                 kid, ks, obs, xm, Iw, conf,  &
                 stat=ios )
    if ( ios .ne. 0 ) call die ( myname, 'cannot de-allocate memory' )

!   All done
!   --------
    return


CONTAINS  ! ................... internals follow .........................

      subroutine Gather_ ( this_kt, nobs_kt )

        implicit NONE

        integer, intent(in)   :: this_kt     ! data index to gather

        integer, intent(inout):: nobs_kt     ! no. of obs with this_kt.

!
!   Gather those observations with data index "this_kt".
!

        character(len=*), parameter :: myname = 'm_simul::Gather_'
        integer i, n

        n = nobs_kt        ! this is initialized outside
        do i = 1, nobs
           if ( y%kt(i) .eq. this_kt ) then
              n = n + 1
              if ( n.lt.1 .or. n.gt.nobs ) call die ( myname, 'out of bounds' )
              lat(n) = y%lat(i)
              lon(n) = y%lon(i)
              lev(n) = y%lev(i)
              obs(n) = y%obs(i)
              xm(n)  = y%xm(i)
              ks(n)  = y%ks(i)
              kid(n) = i
           end if
        end do

        nobs_kt = n

       end subroutine Gather_


      subroutine Scatter_ ( vname, n1, n2, simulated )

        implicit NONE

        character(len=*), intent(in)  :: vname  ! qunatity name, e.g., heights
        integer, intent(in)           :: n1, n2 ! scatter interpolated values
                                                !  with indices between
                                                !  n1 and n2
        logical, intent(out) :: simulated(:)    ! simulated obs

!
!       Scatter interpolated observations according to confidence level.
!

        integer i, n, nfail
        character(len=255) msg

        nfail = 0
        do n = n1, n2

           i = kid(n)

           if ( conf(n) .gt. 1.0-tol ) then   ! reliable interpolation

                Iv(i)      = obs(n) - Iw(n)
                simulated(i) =  .true.

           else if ( conf(n) .lt. tol ) then  ! unreliable interpolation

                Iv(i)      = UNDEF
                if ( qc ) then
                   if ( y%qcexcl(i) .eq. 0 ) y%qcexcl(i) = X_SIMUL
                end if
                simulated(i) =  .false.
                nfail        = nfail + 1

           else                               ! so-so interpolation

                Iv(i)      = obs(n) - Iw(n)
                if ( qc ) then
                   if ( y%qchist(i) .eq. 0 ) y%qchist(i) = H_SIMUL ! QC will take care of this
                end if
                simulated(i) =  .true.

           end if

        end do

       n = n2 - n1 + 1
       call vect_stat ( myname, 'obs/'//vname(4:), obs(n1:n2) ,   n )
       call vect_stat ( myname, 'bkg/'//vname(4:), Iw(n1:n2),   n )
       call vect_stat ( myname, 'o-f/'//vname(4:), obs(n1:n2)-Iw(n1:n2),  n )
       if ( nfail .gt. 0 ) then
            write(msg,'(i8,a,i8,a)') nfail, vname//' obs (out of ', &
                            n2-n1+1, ') could not be simulated'
            call mpout_log ( myname, trim(msg) )
       end if

      end subroutine Scatter_

    end subroutine Simulator

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!-BOP
!
! !IROUTINE:  Insitu_ --- Simulates insitu aerosol observations
!
! !INTERFACE:
!
      subroutine Insitu_ ( w_f, lon, lat, lev, nobs, which, opt, &
                            Iw_f, conf, rc )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(Chem_Bundle), intent(in)   :: w_f       ! Gridded model fields
      integer, intent(in)          :: nobs      ! Number of observations
      real*8, intent(in)             :: lon(nobs) ! longitude in degrees [-180,+180]
      real*8, intent(in)             :: lat(nobs) ! latitude  in degrees [-90,90]
      real*8, intent(in)             :: lev(nobs) ! level in Hpa

      character(len=*), intent(in) :: which     ! Which variable to simulate,
                                                !  case-insensitive options are:
                                                !
                                                ! which        description
                                                ! -----  -----------------------                                                ! aod    Aerosol optical depth

      integer, intent(in)          :: opt       ! Interpolation option:
                                                !   0  default

! !OUTPUT PARAMETERS:
!
      real*8, intent(out)           :: Iw_f(nobs) ! Simulated insitu vector
      real*8, intent(out)           :: conf(nobs) ! Confidence of interpolated
                                                !  values:
                                                !  = 1   reliable interpolation
                                                !  = 0   could not interpolate,
                                                !         no interpolated value
                                                !         is set at this point
                                                ! Any other value between 0 and
                                                ! 1 means partial success.
                                                ! Although a value is produced
                                                ! in such cases, one should
                                                ! apply a more stringent QC for
                                                ! these points.

      integer, intent(out)         :: rc        ! Error return code:
                                                !  0   all is well
                                                !  1   non-supported variable
                                                !


! !DESCRIPTION: This routine uses a simple nearest neighbor algorithm to 
!               compute the insitu vector.
!
!               The input lon coordinate can be either [-180, 180] or [0, 360]
!
! !REVISION HISTORY:
!
!  03oct2005  da Silva  Derived from GEOS-4 simulator
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'm_simul::Insitu_'

   integer :: i, j, i1, i2, im, j1, j2, jm, km, ier, n, k, iq
   real*8, allocatable :: lons(:), lats(:), levs(:)
   real*8, pointer :: tau(:,:,:)   ! 3-D AOD summed over species

   conf = 0.0  ! we are optmists

!  Require 1 tracer with total AOD
!  -------------------------------
   if ( (w_f%reg%nq .ne. 1) .or. ( .not. w_f%reg%doing_XX) ) then
      rc = -1
      return
   end if

!  Initialize local variables
!  --------------------------
   rc = 0

   i1 = w_f%grid%i1; i2 = w_f%grid%i2
   j1 = w_f%grid%j1; j2 = w_f%grid%j2
   km = w_f%grid%km
   im = i2 - i1 + 1  ! could be local
   jm = j2 - j1 + 1  ! could be local

!  Local memory
!  ------------
   allocate ( tau(i1:i2,j1:j2,km),stat = ier )
   if ( ier /= 0 ) then
      rc = 2
      return
   end if

!  Aerosol optical depth
!  ---------------------
   if ( trim(which) == 'aod' ) then

!     If necessary swap x-grid so that lon in [0,360) as required by 
!      interpolation package
!     TO DO: fix the interpolation package
!     --------------------------------------------------------------
      tau = w_f%qa(1)%data3d
      if ( nint(w_f%grid%lon(1,1)) .eq. -180 ) then
         do k = 1, km
            call shift180Lon2D_ ( tau(:,:,k), im, jm )
         end do
      else if ( nint(w_f%grid%lon(1,1)) .eq. 0 ) then
         continue ! GEOS-4 fields started at the prime meridian
      else
         rc = 3
         return
      end if

!     Interpolate to obs location
!     ---------------------------
      if ( km > 1 ) then
         call Interp3d_ ( w_f%grid, lon, lat, lev, nobs, im, jm, km, tau,  &
                          0, Iw_f, conf, rc )
      else
         call Interp2d_ ( w_f%grid, lon, lat, nobs, im, jm, tau(:,:,1),  &
                          Iw_f, conf, rc )
      end if
      if ( rc /= 0 ) then
         rc = 4
         return
      end if

!  TO DO: Other observables 
!  ------------------------
   else

      Iw_f = UNDEF
      conf = 0.0
 
   end if

   deallocate ( tau, stat = ier )
   if ( ier /= 0 ) then
      rc = 3
      return
   end if
   

!  All done
!  --------
   rc = 0
   return

CONTAINS

    subroutine shift180Lon2D_ ( c, im, jm )
    integer, intent(in) :: im, jm
    real*8, intent(inout) :: c(im,jm)
    real*8 :: cj(im)
    integer :: m(4), n(4), imh, j
    imh = nint(im/2.)
    m = (/ 1,      imh, 1+imh,    im   /)
    n = (/ 1,   im-imh, 1+im-imh, im   /)
    do j = 1, jm
       cj(n(1):n(2)) = c(m(3):m(4),j)
       cj(n(3):n(4)) = c(m(1):m(2),j)
       c(:,j) = cj
    end do
    return
    end subroutine shift180Lon2D_

    end subroutine Insitu_



!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Interp2d_--- Interpolates 2D field to observation locations
!
! !INTERFACE:
!
      subroutine Interp2d_ ( grid, lon, lat, nobs,  &
                             im, jm, field,          &
                             Iw, conf, rc )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(Chem_Grid), intent(in) :: grid     ! Grid specification
      integer, intent(in)        :: nobs      ! Number of observations
      real*8, intent(in)         :: lon(nobs) ! longitude in degrees [-180,+180]
      real*8, intent(in)         :: lat(nobs) ! latitude  in degrees [-90,90]

      integer, intent(in)        :: im        ! zonal dimension
      integer, intent(in)        :: jm        ! meridional dimension

      real*8, intent(in)         :: field(im,jm) ! field


! !OUTPUT PARAMETERS:
!
      real*8, intent(out)        :: Iw(nobs)    ! Interpolated values
      real*8, intent(out)        :: conf(nobs)  ! Confidence of interpolated
                                                !  values:
                                                !  = 1   reliable interpolation
                                                !  = 0   could not interpolate,
                                                !         no interpolated value
                                                !         is set at this point
                                                ! Any other value between 0 and
                                                ! 1 means partial success.
                                                ! Although a value is produced
                                                ! in such cases, one should
                                                ! apply a more stringent QC for
                                                ! these points.

      integer, intent(out)         :: rc        ! Error return code:
                                                !  0   all is well
                                                !  1   non-supported variable
                                                !

! !DESCRIPTION: This routine interpolates gridded model fields to observation
!               locations. This is the 2D version.
!
!               The input lon coordinate can be either [-180, 180] or [0, 360];
!               the grid must be in [0,360), though.
!
!
! !SEE ALSO:
!
!              Module m_insitu which uses the same linear interpolation algorithm.
!
!
! !REVISION HISTORY:
!
!  10feb2010  da Silva  Simplifield m_interp routine for AOD interpolation
!
!EOP
!-------------------------------------------------------------------------
     character(len=*), parameter :: myname = 'Interp_Field'


! Local
      integer i, j, k, nob
      real*8    obs_lon, o_lon, o_lat
      real*8    m_dlon, m_dlat
      real*8    alfa, beta, gama
      real*8    q_0, q_1
      logical found

      real*8 a11         !W-S
      real*8 a12         !W-N
      real*8 a21         !E-S
      real*8 a22         !E-N
      real*8 a00         !temp

      integer i1, i2

!                         ------

      rc = 0

      if ( nobs .eq. 0 ) return    ! nothing to do (keep this)

     m_dlon = float(im) / 360.
     m_dlat = float(jm-1) / 180.

!    Loop over observations
!    ----------------------
     do nob = 1, nobs

!            Longitude
!            ---------
             if( lon(nob) .lt. 0.) then
                obs_lon = lon(nob) + 360.
             else
                obs_lon = lon(nob)
             endif
             o_lon = 1. + obs_lon * m_dlon
             i   = min(im, int( o_lon ))
             alfa  = o_lon - i
             if(i .eq. im) then
                i1 = im
                i2 = 1
             else
                i1 = i
                i2 = i + 1
             endif
        
!            Latitude
!            --------
             o_lat = 1. + (lat(nob) + 90.) * m_dlat
             j   = min( jm-1, int( o_lat ) )
             beta  = o_lat - j

             a11 = field(i1,j)
             a21 = field(i2,j)
             a12 = field(i1,j+1)
             a22 = field(i2,j+1)
             a00 = a11 + alfa * ( a21 - a11 )

!            Do NOT Tolerate UNDEFs
!            ----------------------
             if (abs(a11-UNDEF) .lt. 0.01 .or. abs(a21-UNDEF) .lt. 0.01 .or. &
                 abs(a12-UNDEF) .lt. 0.01 .or. abs(a22-UNDEF) .lt. 0.01) then
  		Iw(nob) = UNDEF
                conf(nob) = 0.
             else
                Iw(nob) = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )
                conf(nob) = 1.
             endif

       end do ! loop over obs

      rc = 0
      return

     end subroutine Interp2d_

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Interp3d_--- Interpolates 3D field to observation locations
!
! !INTERFACE:
!
      subroutine Interp3d_ ( grid, lon, lat, lev, nobs,  &
                           im, jm, km, field,          &
                           opt, Iw, conf, rc )

! !USES:
!
      Implicit NONE

! !INPUT PARAMETERS:
!

      type(Chem_Grid), intent(in) :: grid     ! Grid specification
      integer, intent(in)        :: nobs      ! Number of observations
      real*8, intent(in)         :: lon(nobs) ! longitude in degrees [-180,+180]
      real*8, intent(in)         :: lat(nobs) ! latitude  in degrees [-90,90]
      real*8, intent(in)         :: lev(nobs) ! level in Hpa

      integer, intent(in)        :: im        ! zonal dimension
      integer, intent(in)        :: jm        ! meridional dimension
      integer, intent(in)        :: km        ! vertical dimension: 
                                              ! = 1 for 2D fields
                                              ! = km for mid-layer fields
                                              ! = km+1 for edge fields

      real*8, intent(in)         :: field(im,jm,km) ! field

      integer, intent(in)        :: opt       ! vertical interpolation option:
                                              ! = 0  (log, default)
                                              ! = 1  (linear)

! !OUTPUT PARAMETERS:
!
      real*8, intent(out)        :: Iw(nobs)    ! Interpolated values
      real*8, intent(out)        :: conf(nobs)  ! Confidence of interpolated
                                                !  values:
                                                !  = 1   reliable interpolation
                                                !  = 0   could not interpolate,
                                                !         no interpolated value
                                                !         is set at this point
                                                ! Any other value between 0 and
                                                ! 1 means partial success.
                                                ! Although a value is produced
                                                ! in such cases, one should
                                                ! apply a more stringent QC for
                                                ! these points.

      integer, intent(out)         :: rc        ! Error return code:
                                                !  0   all is well
                                                !  1   non-supported variable
                                                !

! !DESCRIPTION: This routine interpolates gridded model fields to observation
!               locations. All interoplation is linear with log(P) in the
!               vertical. Be sure to check the confidence values for each
!               of the interpolated values: {\tt conf=0} if {\tt lev(nobs)} 
!               is below/above the lowest/highest model level; $conf=1$ otherwise.
!               A large value undef is given if interpolation is not successful.
!
!               The input lon coordinate can be either [-180, 180] or [0, 360];
!               the grid must be in [0,360), though.
!
!
! !SEE ALSO:
!
!              Module m_insitu which uses the same linear interpolation algorithm.
!
!
! !REVISION HISTORY:
!
!  10feb2010  da Silva  Simplifield m_interp routine for AOD interpolation
!
!EOP
!-------------------------------------------------------------------------
     character(len=*), parameter :: myname = 'Interp_Field'


! Local
      integer i, j, k, nob
      real*8    obs_lon, o_lon, o_lat
      real*8    m_dlon, m_dlat
      real*8    alfa, beta, gama
      real*8    pm_0, pm_1, pmm_1
      real*8    pe_0, pe_1
      real*8    q_0, q_1
      logical found

      real*8 a11         !W-S
      real*8 a12         !W-N
      real*8 a21         !E-S
      real*8 a22         !E-N
      real*8 a00         !temp

      integer i1, i2
      real*8 tolb        ! tolerance for levels bracketing

      real*8 fill1, fill2

!                         ------

      rc = 0

      if ( nobs .eq. 0 ) return    ! nothing to do (keep this)

!    Make sure vertical level is in ascending order
!    ----------------------------------------------
     do k = 2, km
        if ( grid%lev(k) .le. grid%lev(k-1) ) then
           rc = 1
           return
        end if
     end do

!    Interpolation for 3D variables only for now
!    -------------------------------------------
     if( km < 2) then
         rc = 2
         return
     end if

     tolb = 10.*epsilon(1.)
     m_dlon = float(im) / 360.
     m_dlat = float(jm-1) / 180.

!    Loop over observations
!    ----------------------
     do nob = 1, nobs

!       Make sure levels are in range
!       -----------------------------
        if ( lev(nob) < grid%lev(1) .or. lev(nob) > grid%lev(km) ) then
           Iw(nob) = UNDEF
           conf(nob) = 0.0
           cycle
        end if

!       Vertical interpolation
!       ----------------------
        found = .false.
        k = km-1  ! from bottom to top
        do while ( .not. found )

             pm_0 = grid%lev(k)
             pm_1 = grid%lev(k+1)

!            Locating the levels bracketing lev(nob) with some tolerance
!            -----------------------------------------------------------
             if( lev(nob) .gt. pm_1 * (1. + tolb) ) then
                 k = 0
             elseif(pm_0 * (1. - tolb) .lt. lev(nob) .and.   &
                    lev(nob) .le. pm_1 * (1. + tolb) ) then
                 found = .true.
             else
                 k = k - 1
             endif

!            Level is outise bounds of grid
!            ------------------------------
             if( k .eq. 0 ) then
                 found = .true.
             end if

         enddo

!        Next, horizontal interpolation
!        ------------------------------
         if ( k==0 ) then
                 Iw(nob) = UNDEF
                 conf(nob) = 0.

         else

!            Longitude
!            ---------
             if( lon(nob) .lt. 0.) then
                obs_lon = lon(nob) + 360.
             else
                obs_lon = lon(nob)
             endif
             o_lon = 1. + obs_lon * m_dlon
             i   = min(im, int( o_lon ))
             alfa  = o_lon - i
             if(i .eq. im) then
                i1 = im
                i2 = 1
             else
                i1 = i
                i2 = i + 1
             endif
        
!            Latitude
!            --------
             o_lat = 1. + (lat(nob) + 90.) * m_dlat
             j   = min( jm-1, int( o_lat ) )
             beta  = o_lat - j

             a11 = field(i1,j,  k)
             a21 = field(i2,j,  k)
             a12 = field(i1,j+1,k)
             a22 = field(i2,j+1,k)
             a00 = a11 + alfa * ( a21 - a11 )
             q_0 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

             Iw(nob) = 0.0
             fill1 = 0
             fill2 = 0

!            Tolerate some nearby UNDEFs (level k)
!            -------------------------------------
             if (abs(a11-UNDEF) .lt. 0.01 .or. abs(a21-UNDEF) .lt. 0.01 .or. &
                 abs(a12-UNDEF) .lt. 0.01 .or. abs(a22-UNDEF) .lt. 0.01) then
                fill1 = a11  
		Iw(nob) = UNDEF
             endif

             a11 = field(i1,j,  k+1)
             a21 = field(i2,j,  k+1)
             a12 = field(i1,j+1,k+1)
             a22 = field(i2,j+1,k+1)
             a00 = a11 + alfa * ( a21 - a11 )
             q_1 = a00 + beta * ( a12 + alfa * ( a22 - a12 )  -  a00 )

!            Tolerate some nearby UNDEFs (level k+1)
!            ---------------------------------------
             if (abs(a11-UNDEF) .lt. 0.01 .or. abs(a21-UNDEF) .lt. 0.01 .or. &
                 abs(a12-UNDEF) .lt. 0.01 .or. abs(a22-UNDEF) .lt. 0.01) then
         	fill2 = a11  
		Iw(nob) = UNDEF
             endif

             if ( opt == 0 ) then
                gama = (log(lev(nob))-log(pm_0))/(log(pm_1)-log(pm_0))
             else
                gama = (   (lev(nob))-   (pm_0))/(   (pm_1)-   (pm_0))
             end if

             if ( abs(Iw(nob)-UNDEF) .lt. 0.01 ) then
                if ( abs(fill1-UNDEF) .lt. 0.01 .or. abs(fill2-UNDEF) .lt. 0.01 ) then
                   Iw(nob) = UNDEF
		else
                   if ( grid%im*grid%jm .ne. im*jm ) then
                      Iw(nob) = UNDEF 
                   else
 		      Iw(nob) = fill1  + gama * ( fill2 - fill1 )
                   end if
	        end if
             else
                Iw(nob) = q_0 + gama * ( q_1 - q_0 )
             end if

             if ( Iw(nob) .eq. UNDEF ) then
                conf(nob) = 0.
             else
                conf(nob) = 1.
             end if

          endif

       end do ! loop over obs

      rc = 0
      return

     end subroutine Interp3d_

  end MODULE m_simul
