!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_obs --- Implements the Observer model
!
! !INTERFACE:
!

   MODULE  m_obs

! !USES:

   use  Chem_BundleMod                      ! Gridded state
   use  m_ods                               ! ODS vector
   use  m_simul                             ! Simulate observations
   use  m_sqc                               ! Statistical On-line QC
   use  m_die                               ! abnormal exiting
   use  m_mpout                             ! log messages to stdout
   use  m_zeit                              ! timer
   use  m_inpak90                           ! Resource management (mpeu)
   use  m_duplicate, only: dupobs           ! Duplication removal procedure

   use  m_odsmeta, only: H_YELLOW
   use  m_odsmeta, only: X_BAD_LOC
   use  m_odsmeta, only: X_NOT_ANAVAR
   use  m_odsmeta, only: X_OBS_FILL
   use  m_odsmeta, only: X_PASSIVE
   use  m_odsmeta, only: X_RED
   use  m_odsmeta, only: X_THIN
   use  m_odsmeta, only: X_UNPHYSICAL
   use  m_odsmeta, only: X_UNDERG
   use  m_odsmeta, only: ktAOD, ktLogAOD

   Implicit NONE

!
! !PUBLIC MEMBER FUNCTIONS:
!
   PRIVATE
   PUBLIC  observer                         ! Produces quality controled
   PUBLIC  reducer                          ! Produces quality controled
                                            !  observations (and O-F).

   PUBLIC log_transf
   PUBLIC eps 

!
! !DESCRIPTION: This module implements the {\tt Observer} for the
!               Physical-space/Finite-volume DAS. It performs the
!  following functions:
!  \bi
!       \item Read observations from ODS files
!       \item Anchor and unbias TOVS retrieals
!       \item Perform several sanity checks
!       \item Simulate observations from first guess and
!             compute innovations (O-F)
!       \item Quality Control observations
!  \ei
!
! !REVISION HISTORY:
!
!  10oct1999  da Silva  Initial code.
!  13dec1999  da Silva  Implemented boxes, removed ptop_all, ptop_mix.
!  14nov2000  Dee       Handle TPW observations
!  14dec2000  da Silva  Increased mBoxes from 32 to 128.
!  05jun2002  Dee       Convert heights to layer-mean virtual temperatures
!  09feb2010  da Silva  Adapted from old GEOS-4 observer for AOD.
!
!EOP
!-------------------------------------------------------------------------

    logical :: do_sqc = .true.      ! whether or not to statistical qc
    logical :: log_transf = .false. ! whether to log-transform AOD
    real    :: eps                  ! eps for log-transform
    logical :: do_dupelim = .true.  ! duplicate elimination?

!   Resource file
!   -------------
    character(len=*), parameter :: rcenvar = 'OBSRC'   ! environment RC var
    character(len=*), parameter :: rcfname = 'obs.rc'  ! default RC file name
    character(len=255)          :: obsrc               ! actual rc file name

!   Boxes for: Red List, Yellow List and Passive Data
!   -------------------------------------------------
    integer, parameter  :: mBoxes = 128          ! max. no. of Boxes per test
    real                :: Boxes(2,6,mBoxes,3)   ! Boxes for passive check
    integer             :: nBoxes(3) = 0         ! actual no. of boxes
    logical             :: outside(3) = .false.  ! set obs INSIDE boxes passive

CONTAINS

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Observer --- Simulate and QC observations
!
! !INTERFACE:
!
    subroutine Observer ( nymd, nhms, w_f, nobs, ods, nobs_good, &
                          convertOb2AOD ) ! optionals

! !USES:
!
    Implicit NONE

!
! !INPUT PARAMETERS:
!
                                             ! Synoptic time:
    integer,          intent(in) :: nymd     !  Year-month-day, e.g., 19971012
    integer,          intent(in) :: nhms     !  hour-minute-sec, e.g., 120000

    type(Chem_Bundle), intent(in) :: w_f     ! first guess fields

    integer,        intent(in)   :: nobs     ! total no. of obs

    logical,optional,intent(in)  :: convertOb2AOD  ! allow handling log(AOD) in input obs

!
! !INPUT/OUTPUT PARAMETERS:
!
    type(ods_vect), intent(inout) :: ods      ! ods vector: on output, QC flags
                                              ! and O-F attributes are set.
                                              ! The "OBS" attribute can also
                                              ! be reset.
!
! !OUTPUT PARAMETERS:
!
    integer,        intent(out) :: nobs_good  ! no. of obs passing QC

!
! !DESCRIPTION: This routine implements the {\tt Observer} for the
!               Physical-space/Finite-volume DAS. It performs the
!  following functions:
!  \bi
!       \item Simulate observations from first guess and
!             compute innovations (O-F)
!       \item Quality Control observations
!  \ei
!
!  On output the ODS vector {\tt ODS} is re-ordered such that {\tt nobs\_good}
!  observations passing {\em Quality Control are moved to the front.}
!
! !REVISION HISTORY:
!
!  30Jul1999  da Silva   First PSAS-FV DAS version vaguely based on GEOS-2
!                         version.
!  12nov1999  da Silva   Changed argument: from expid to odsfile.
!  15nov1999  da Silva   Bug fixes during unit testing:
!                        - added zeit() calls;
!                        - fixed nobs bug after nobs_get()
!  23dec1999  da Silva   Introduced m_odsmeta, new qc flags.
!                        Reading of ODS file has been taken outside.
!  03mar2000  da Silva   Marked RH as not an analysis variable for
!                        compatibility with SQC v1.5.
!  14nov2000  Dee        Handle TPW data
!  31Oct2001  Todling    Bug fix (nobs=0 did not set nobs_good);
!                        Added DupObs to remove duplicate observations.
!  05jun2002  Dee        Convert heights to layer-mean virtual temperatures
!
!EOP
!-------------------------------------------------------------------------

!BOC

    character(len=*), parameter ::  myname = 'Observer'

    character(len=255)          ::   msg

    integer :: ier, nset, n

    integer, pointer :: qcx(:), qch(:), kt(:)  ! short hand for ODS attributes
    real*8 :: lev(nobs)                 ! short hand for ODS attributes


!   Nobs=0 check
!   ------------
    if ( nobs .eq. 0 ) then
       nobs_good = 0
       call warn ( myname, 'no observations, nothing to do.' )
       return
    else
       qch  => ods%data%qchist   ! short hand for history   flag
       qcx  => ods%data%qcexcl   ! short hand for exclusion flag
       kt   => ods%data%kt       ! short hand for data type index
       lev(1:nobs)  = ods%data%lev(1:nobs)      ! short hand for data level
    end if

    call zeit_ci ( myname )

!   Initialize this package
!   -----------------------
    call Init_()

    if (present(convertOb2AOD)) then
       if ( convertOb2AOD ) then
          ods%data%obs = max(eps,exp(ods%data%obs)-eps)
          ods%data%kt  = ktAOD
       endif
    endif

!   Standardize channels
!   --------------------
    call AOD_StandardChannels (ods%data%lev, nobs)

!   Set all qchist to zero for now (or else SQC will get confused)
!   --------------------------------------------------------------
    if ( do_sqc ) then
       ods%data%qchist(1:nobs) = 0
       call warn(myname,'setting all qc_hist attributes to zero before Q/C')
    end if

!   Sanity tests
!   -------------
    call Sanity_Check  ( ods%data, nobs )

!   Exclude Red-listed observations
!   -------------------------------
    call ODS_MaskOut ( ods%data, nobs, Boxes(:,:,:,1), nBoxes(1),         &
                       X_RED, qcx(1:nobs), nset, &
                       outside(1) )
    if ( nset .gt. 0 ) then
       write(msg,'(i8,a)') nset, ' observations have been "red-listed"'
       call mpout_log(myname,msg)
    else
       write(msg,'(a)') ' NO observation has been "red-listed"'
       call mpout_log(myname,msg)
    end if


!   Mark as suspect Yellow-listed observations
!   ------------------------------------------
    call ODS_MaskOut ( ods%data, nobs, Boxes(:,:,:,2), nBoxes(2),         &
                       H_YELLOW, qch(1:nobs), nset, &
                       outside(2) )
    if ( nset .gt. 0 ) then
       write(msg,'(i8,a)') nset, ' observations have been "yellow-listed"'
       call mpout_log(myname,msg)
    end if

!   Simulate AOD observations (log-transform may be performed later,
!   here we still have AOD)
!   ---------------------------------------------------------------

                                                                             call zeit_ci ( 'simulator' )
    call Simulator ( w_f, ods%data, ods%data%omf, nobs )
                                                                             call zeit_co ( 'simulator' )

!   Set observations PASSIVE as per user request
!   --------------------------------------------
    call ODS_MaskOut ( ods%data, nobs, Boxes(:,:,:,3), nBoxes(3),   &
                       X_PASSIVE, qcx(1:nobs), nset, &
                       outside(3) )
    if ( nset .gt. 0 ) then
       write(msg,'(i8,a)') nset, ' observations set as PASSIVE'
    else
       write(msg,'(a)') ' NO observation set as PASSIVE'
    end if
    call mpout_log(myname,msg)

!   Set as non-analysis variables all good obs, except q
!   ----------------------------------------------------
    n = nobs
    where ( qcx(1:n) .eq. 0 .and. kt(1:n) .ne. ktAOD )

            qcx(1:n) = X_NOT_ANAVAR

    end where

!   Flag out duplicated data
!   ------------------------
    if ( do_dupelim ) then
                                                                                 call zeit_ci ( 'DupObs' )
         call DupObs ( ods%data, nobs )
                                                                                 call zeit_co ( 'DupObs' )
    end if

!   Log transform AOD
!   -----------------
    if ( log_transf ) then
       where ( (ods%data%qcexcl == 0 .or. ods%data%qcexcl == X_PASSIVE) &
               .AND. ods%data%kt==ktAOD )
          ods%data%omf = log(max(eps,eps+ods%data%obs)) &
                       - log(max(eps,eps+ods%data%obs-ods%data%omf))
          ods%data%obs = log(max(eps,eps+ods%data%obs))
          ods%data%kt  = ktLogAOD
       end where
    end if

!   Statistical Quality Control (SQC)
!   ---------------------------------
    if ( do_sqc ) then
                                                                                 call zeit_ci ( 'SQC' )
         call SQC ( ods%data, nobs, nobs_good )
                                                                                 call zeit_co ( 'SQC' )

    else

         call ODS_Select ( ods, nobs, nobs_good, ier, qcexcl=0 )
         if ( ier/=0 ) call die ( myname, 'trouble in ods_select (alloc)' )

    end if

!   All done
!   --------
    call zeit_co ( myname )
    return

!EOC

  end subroutine Observer

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Reducer --- Reduce ODS vector by Red Listing and/or Thinning
!
! !INTERFACE:
!
    subroutine Reducer ( nymd, nhms, nobs, ods, nobs_good,  &
                         rcfile )                           ! optional

! !USES:
!
    Implicit NONE

!
! !INPUT PARAMETERS:
!
                                             ! Synoptic time:
    integer,          intent(in) :: nymd     !  Year-month-day, e.g., 19971012
    integer,          intent(in) :: nhms     !  hour-minute-sec, e.g., 120000

    integer,        intent(in)   :: nobs     ! total no. of obs

    character(len=*), intent(in), OPTIONAL :: rcfile  ! resource file name;
                                             ! Default: reducer.rc

!
! !INPUT/OUTPUT PARAMETERS:
!
    type(ods_vect), intent(inout) :: ods      ! ods vector: on output, QC flags
                                              ! and O-F attributes are set.
                                              ! The "OBS" attribute can also
                                              ! be reset.
!
! !OUTPUT PARAMETERS:
!
    integer,        intent(out) :: nobs_good  ! no. of obs not redlisted

!
! !DESCRIPTION: This routine eliminates observations from the input
!               ODS vector {\tt ODS} by Red Listing or Thinning them.
!  Also, Yellow listed observations are marked as suspect.
!  Thinning/red/yellow listing options by specified on the resource file.
!
!  On output the ODS vector {\tt ODS} is re-ordered such that {\tt nobs\_good}
!  observations which have not been eliminated are moved to the front.
!
! !REVISION HISTORY:
!
!  30Apr2001  da Silva   Derived from Observer().
!
!EOP
!-------------------------------------------------------------------------

!BOC

    character(len=*), parameter ::  myname = 'Reducer'

    character(len=255)          ::   msg, redrc

    integer :: ier, nset, n

    integer, pointer :: qcx(:), qch(:), kt(:)  ! short hand for ODS attributes
    real*8 :: lev(nobs)                 ! short hand for ODS attributes



!   Nobs=0 check
!   ------------
    if ( nobs .eq. 0 ) then
       call warn ( myname, 'no observations, nothing to do.' )
       return
    else
       qch  => ods%data%qchist   ! short hand for history   flag
       qcx  => ods%data%qcexcl   ! short hand for exclusion flag
       kt   => ods%data%kt       ! short hand for data type index
       lev(1:nobs)  = ods%data%lev(1:nobs)  ! short hand for data level
    end if

    call zeit_ci ( myname )


!   Initialize this package
!   -----------------------
    if ( present(rcfile) ) then
         redrc = rcfile
    else
         redrc = 'reducer.rc'
    end if
    call Init_ ( redrc )


!   Exclude Red-listed observations
!   -------------------------------
    call ODS_MaskOut ( ods%data, nobs, Boxes(:,:,:,1), nBoxes(1),         &
                       X_RED, qcx(1:nobs), nset, &
                       outside(1) )
    if ( nset .gt. 0 ) then
       write(msg,'(i8,a)') nset, ' observations have been "red-listed"'
       call mpout_log(myname,msg)
    end if


!   Mark as suspect Yellow-listed observations
!   ------------------------------------------
    call ODS_MaskOut ( ods%data, nobs, Boxes(:,:,:,2), nBoxes(2),         &
                       H_YELLOW, qch(1:nobs), nset, &
                       outside(2) )
    if ( nset .gt. 0 ) then
       write(msg,'(i8,a)') nset, ' observations have been "yellow-listed"'
       call mpout_log(myname,msg)
    end if

!   Move good observations to front
!   -------------------------------
    call ODS_Select ( ods, nobs, nobs_good, ier,        &
                      qcexcl_list=(/ X_THIN, X_RED /),  &
                      complement=.true. )
    if ( ier .ne. 0 ) call die ( myname, 'cannot select' )

!   All done
!   --------
    call zeit_co ( myname )
    return

!EOC

  end subroutine Reducer


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!
! !IROUTINE: Init_ --- Initializes the observer module
!
! !INTERFACE:
!
    subroutine Init_ ( rcfile )

! !USES:
!
    Implicit NONE

! !INPUT PARAMETERS:

    character(len=*), intent(in), OPTIONAL :: rcfile
                                    ! resource file name; when specified
                                    ! we assume the Reducer is being
                                    ! initialized

!
! !DESCRIPTION: Loads user options from resource file.
!
! !REVISION HISTORY:
!
!  08oct1999  da Silva  First crack.
!  13dec1999  da Silva  Implemented passive data table.
!  30Apr2001  da Silva  Extensions for the reducer
!  30Jul2004  Todling   Replaced i90_release by i90_fullrelease.
!
!-------------------------------------------------------------------------

    character(len=*), parameter ::  myname = 'Observer::Init_'


    character(len=255)  :: ParType, ParDesc, answer

    real    :: BoxId(mBoxes)  ! work space to hold box id
    integer :: mAtt2 = 2 * 6  ! twice no. of attributes:
                              !   (kx,kt,lat,lon,lev,time)
    integer :: nAtt2          ! actual no. of attributes: must equal mAtt2


    integer ier
    real    tmp


!   Load resource
!   -------------
    if ( present(rcfile) ) then
         call I90_LoadF ( rcfile, ier )
         if ( ier .ne. 0 ) call die ( myname,  &
           'cannot find REDUCER resource file ' // trim(rcfile) )
    else
         call I90_LoadRC ( rcenvar, rcfname, obsrc, ier )
         if ( ier .ne. 0 ) call die ( myname,  &
           'cannot find OBSERVER resource file ' // trim(obsrc) )
    end if

!   Load Red List table from resource file
!   --------------------------------------
    call i90_gtab ( 'Red*List::', ParType, ParDesc, &
                    mBoxes, nBoxes(1), BoxId, &
                    mAtt2, nAtt2, Boxes(:,:,:,1), ier )
    if ( ier .ne. 0 ) then
         call warn ( myname, 'cannot read Red List table' )
         nBoxes(1) = 0
    else if ( mAtt2 .ne. nAtt2 ) then
         call die ( myname, 'inconsistent Red List table' )
    end if
    if ( trim(ParType) .eq. 'outside' .or. &
         trim(ParType) .eq. 'OUTSIDE' ) then
         outside(1) = .true.  ! obs outside box will be passive
    end if

    if ( nBoxes(1) .le. 0 ) call warn ( myname, 'no Red List' )

!   Load Yellow List table from resource file
!   ------------------------------------------
    call i90_gtab ( 'Yellow*List::', ParType, ParDesc, &
                    mBoxes, nBoxes(2), BoxId, &
                    mAtt2, nAtt2, Boxes(:,:,:,2), ier )
    if ( ier .ne. 0 ) then
         call warn ( myname, 'cannot read Yellow List table' )
         nBoxes(2) = 0
    else if ( mAtt2 .ne. nAtt2 ) then
         call die ( myname, 'inconsistent Yellow List table' )
    end if
    if ( trim(ParType) .eq. 'outside' .or. &
         trim(ParType) .eq. 'OUTSIDE' ) then
         outside(2) = .true.  ! obs outside box will be passive
    end if

    if ( nBoxes(2) .le. 0 ) call warn ( myname, 'no Yellow List' )


!   Stop here if initializing reducer
!   ---------------------------------
    if ( present(rcfile) ) then
         call I90_fullRelease(ier)   ! release resources
           if ( ier .ne. 0 ) call die ( myname,  &
             'cannot release REDUCER resource file ' // trim(rcfile) )
         return
    end if


!   Load Passive Data table from resource file
!   -------------------------------------------
    call i90_gtab ( 'PassiveData*List::', ParType, ParDesc, &
                    mBoxes, nBoxes(3), BoxId, &
                    mAtt2, nAtt2, Boxes(:,:,:,3), ier )
    if ( ier .ne. 0 ) then
         call warn ( myname, 'cannot read Passive Data table' )
         nBoxes(3) = 0
    else if ( mAtt2 .ne. nAtt2 ) then
         call die ( myname, 'inconsistent Passive Data table' )
    end if
    if ( trim(ParType) .eq. 'outside' .or. &
         trim(ParType) .eq. 'OUTSIDE' ) then
         outside(3) = .true.  ! obs outside box will be passive
    end if

    if ( nBoxes(3) .le. 0 ) call warn ( myname, 'no Passive Data List' )

!   Do we want to perform duplicate elimination?
!   -------------------------------------------
    call i90_label ( 'do_duplicate_elimination:', ier )
    if ( ier .eq. 0 ) then
          call i90_gtoken ( answer, ier )
          if ( ier .eq. 0 ) then
             if ( answer(1:1) .eq. 'n' .or. &
                  answer(1:1) .eq. 'N'  ) then
                  do_dupelim = .false.
             else
                  do_dupelim = .true.
             end if
          end if
    end if

    if ( .not. do_dupelim ) &
         call warn ( myname, 'skipping duplicate elimination' )

!   Do we want to perform qc?
!   -------------------------
    call i90_label ( 'do_online_quality_control:', ier )
    if ( ier .eq. 0 ) then
          call i90_gtoken ( answer, ier )
          if ( ier .eq. 0 ) then
             if ( answer(1:1) .eq. 'n' .or. &
                  answer(1:1) .eq. 'N'  ) then
                  do_sqc = .false.
             else
                  do_sqc = .true.
             end if
          end if
    end if

    if ( .not. do_sqc ) &
         call warn ( myname, 'skipping online quality control' )

!   Do we want to log-transform AOD
!   -------------------------------
    log_transf = .false.
    call i90_label ( 'do_log_transform_aod:', ier )
    if ( ier .eq. 0 ) then
       call i90_gtoken ( answer, ier )
       if ( ier .eq. 0 ) then
          if ( answer(1:1) .eq. 'y' .or. &
               answer(1:1) .eq. 'Y'  ) then
             log_transf = .true.
          end if
       end if
    end if

!   eps for log transform
!   ---------------------    
    if ( log_transf ) then
      call i90_label ( 'eps_for_log_transform_aod:', ier )
      eps = i90_gfloat ( ier )
      if ( ier /= 0 ) then
         call die(myname,'cannot read eps for log transform')
      else if ( eps <= 0 ) then
         call die(myname,'eps for log transform must be positive')
      end if
   end if
   
   if ( log_transf ) then
         call warn ( myname, 'performing AOD log-transform' )
   else
         call warn ( myname, 'skipping AOD log-transform' )
   end if

!   release resources
!   -----------------
    call I90_Release()


    end subroutine Init_


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Sanity_Check --- Performs sanity check on observations
!
! !INTERFACE:
!
   subroutine  Sanity_Check  ( y, nobs )

! !USES:

    Implicit NONE

!
! !INPUT PARAMETERS:
!
    integer,          intent(in)    :: nobs  ! number of observations
!
! !INPUT/OUTPUT PARAMETERS:
!
    type(obs_vect),   intent(inout) :: y     ! observation vector

!
! !DESCRIPTION: This routine applies the following sanity checks:
!  \bn
!      \item Whether lat/lon is within range
!      \item Whether FILL values are present
!  \en
!  On output, observations failing any of these tests are recorded
!  in the {\tt qc\_flag\_x}.
!
! !REVISION HISTORY:
!
!  08oct1999  da Silva   First PSAS-FV version based on GEOS-2 code
!                        by da Silva, Todling and Dee.
!  15nov1999  da Silva   Added nmix, nall for better warning message,
!                        Added kid/ks checks, physical bounds test.
!  13dec1999  da Silva   Removed test for top level as "black list"
!                        mechanism can take care of this.
!  16May2000  da Silva   Eliminated ks test because DAOTOVS have gaps.
!  28May2000  da Silva   Kept ks<1 test.
!  08Apr2002  Meta/Yelena  add lower limit to SLP check based on
!                          preprocessing range check value.
!  23Jul2002 E. Yeh     Removed level check from lat/lon bounding check
!
!EOP
!-------------------------------------------------------------------------

    character(len=*), parameter :: myname = 'Sanity_Check'

    character(len=255) :: msg
    integer  i, j, n, m, ta, tb
    integer  nstart, nexcl
    real*8     fill, tol


   nstart = count ( y%qcexcl(1:nobs) .eq. 0 ) ! clear at start

!   Check whether lat/lon is within bounds
!   --------------------------------------
    n = 0
    do i = 1, nobs
       if ( y%qcexcl(i) .eq. 0 ) then
          if ( y%lat(i) .gt. 90.  .OR. y%lat(i) .lt. -90.  .or.  &
               y%lon(i) .gt. 180. .OR. y%lon(i) .lt. -180. ) then
             n = n + 1
             y%qcexcl(i) = X_BAD_LOC
          end if
       end if
    end do
    if ( n .ne. 0 ) then
       write(msg,'(a,i8,a)') 'detected ', n, &
            ' observations with lat/lon out of range'
       call mpout_log ( myname, trim(msg) )
    end if


!   Eliminate observations with FILL values
!   ---------------------------------------
    n = 0
    fill = OBS_MISSING
    tol = 0.001 * fill
    do i = 1, nobs
       if ( y%qcexcl(i) .eq. 0 ) then
          if ( abs(y%obs(i)-fill) .lt. tol ) then   ! OBS has FILL
             n  = n + 1
             y%qcexcl(i) = X_OBS_FILL
          end if
       end if
    end do
    if ( n .ne. 0 ) then
       write(msg,'(a,i8,a)') 'detected ', n, &
                             ' observations with MISSING values'
       call mpout_log ( myname, trim(msg) )
    end if

!   Eliminate negative AOD
!   ----------------------
    do i = 1, nobs
       if ( y%qcexcl(i) .eq. 0 .and.  y%kt(i) .eq. ktAOD) then
          if ( y%obs(i) .lt. 0.0 ) then   
             n  = n + 1
             y%qcexcl(i) = X_OBS_FILL ! reuse this for now
          end if
       end if
    end do
    if ( n .ne. 0 ) then
       write(msg,'(a,i8,a)') 'detected ', n, &
                             ' AOD observations with NEGATIVE values'
       call mpout_log ( myname, trim(msg) )
    end if
    

!   Check validity of kid/ks
!   ------------------------
    do i = 1, nobs
       if ( y%kid(i) .lt. 1 .or. y%kid(i) .gt. nobs ) then
            write(msg,'(a,i8,a,i8)' ) 'invalid "kid = ', y%kid(i), &
                               '" for observation ', i
            call die ( myname, msg )
       end if
       if ( y%ks(i) .lt. 1 ) then
            write(msg,'(a,i8,a,i8)' ) 'invalid "ks = ', y%ks(i), &
                               '" for observation ', i
            call die ( myname, msg )
       end if
    end do

!   Check plausibility of observed values for some kt's
!   Note: Need its own flags, bounds are not very tight
!   ---------------------------------------------------
    n = 0
    do i = 1, nobs
       if ( y%qcexcl(i) .eq. 0 ) then
            if ( y%kt(i) .eq. ktAOD ) then ! winds
                 if ( abs(y%obs(i)) .gt. 20. ) then
                      n = n + 1
                      y%qcexcl(i) = X_UNPHYSICAL
                 end if
            end if
       end if
   end do
   if ( n .ne. 0 ) then
       write(msg,'(i8,a,i8,a)') n, ' observations (out of ', nstart, &
            ') detected with unphysical values'
       call mpout_log ( myname, trim(msg) )
    end if

!  Overall summary
!  ---------------
   nexcl = nstart - count ( y%qcexcl(1:nobs) .eq. 0 )
   if ( nexcl .gt. 0 ) then
       write(msg,'(i8,a,i8,a)') nexcl, ' obs (out of ', nstart, &
                 ') have been excluded'
       call mpout_log ( myname, trim(msg) )
   end if


!  All done
!  --------
   end subroutine Sanity_Check

end module m_obs

