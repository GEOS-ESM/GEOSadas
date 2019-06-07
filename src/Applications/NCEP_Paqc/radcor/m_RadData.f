!=======================================================================

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_RadData -- Defines radcor data structure
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      module m_RadData 
      use    m_RadSort, only : IndexSet
      use    m_convert, only : Dew_Point
      implicit    NONE
      private   ! except

      public ::
     .   radcor_profiles,    ! Radcor data structure ...
     .   radcor_meta,        ! ... for   meta-data only
     .   radcor_lists,       ! ... for   list data only
     .   radcor_vector,      ! ... for vector data only
     .   radcor_profile,     ! Data structure for a sounding profile of
     .   profile_by_vector,  ! ... data arranged by vector
     .   profile_by_levels,  ! ... by level
     .   Rad_IndexSet,       ! Set sorting indices
     .   Rad_LevSort,        ! Sort each sounding by level 
     .   Rad_Reorder,        ! Reorder mass and wind data structure
     .   Rad_Init,           ! Initialize data structures
     .   Rad_Clean,          ! Deallocate arrays and ...
     .   Rad_Nullify,        ! ... nullify pointers in data structures
     .   SetPath,            ! Compute lat/lon and sun angle of the path
     .   SetDrift,           ! Set balloon drift data
     .   SetupProf,          ! Set up profile data structure
     .   UpdateProf,         ! Update the data structure, radcor_profile
     .   UpdateProfs         ! ... and radcor_profiles

      public ::
     .   kt_Humidity,        ! Preferred data type index for humidity
     .   QC_OK,              ! Quality control (QC) value for OK
     .   QC_Suspect,         ! ... suspect
     .   QC_Rejected,        ! ... rejected observation
     .   QC_NotTested,       ! QC value denoting no test
     .   QC_AcceptList,      ! List of acceptable QC marks
     .   QC_Max,             ! Max value for radcor QC 
     .   Height,             ! Data type index (kt) for height
     .   UWind,              ! ... u-wind
     .   VWind,               ! ... v-wind
     .   NPresDigits,        ! Number significant digits for pressure
     .   RadCode_AMiss,      ! Missing radiation
     .   AMiss,              ! Missing value for reals (DAO standard)
     .   IMiss,              ! Missing value for integers
     .   LTHist_IMiss,       ! Launch time history code for missing value
     .   LTHist_File,        ! ... value from observation file
     .   LTHist_ObTime,      ! ... value from observation time
     .   LTHist_RObTime,     ! ... value from the reported observation time
     .   ETHist_IMiss,       ! Elapsed time history code for missing value
     .   ETHist_File,        ! ... value from observation file
     .   ETHist_RRProfile,   ! ... value from assumed rise rate profiles
     .   ETHist_CARDS        ! ... value from CARDS data set.

      interface Rad_Init     ! Initialize the radcor data structure ...
         module procedure
     .      Init_Profiles,   ! ... entirely (except profile data)
     .      Init_Meta,       ! ... only for meta-data for each sounding
     .      Init_Lists,      ! ... only for the lists
     .      Init_Vector,     ! ... only for mass data
     .      Init_Profile     ! ... profile data
      end interface
      interface Rad_IndexSet ! Set sorting indices
         module procedure
     .      IndexSet_All,
     .      IndexSet
      end interface
      interface Rad_LevSort  ! Sort each sounding by level
         module procedure
     .      LevSort_All,
     .      LevSort_Vector,
     .      LevSort_
      end interface
      interface Rad_Reorder  ! Reorder
         module procedure
     .      Reorder_All,
     .      Reorder_Vector,
     .      Reorder_Vector2
      end interface
      interface Rad_Clean     ! Clean up the radcor data structure ...
         module procedure 
     .      Clean_Profiles,   ! ... entirely (except profile data)
     .      Clean_Meta,       ! ... only for meta-data for each sounding
     .      Clean_Lists,      ! ... only for the lists
     .      Clean_Vector,     ! ... only for vector data
     .      Clean_Profile     ! ... profile data
      end interface
      interface Rad_Nullify   ! Nullify the radcor data structure for ...
         module procedure
     .      Nullify_Profiles, ! ... entirely (except profile data)
     .      Nullify_Meta,     ! ... only for meta-data for each sounding
     .      Nullify_Lists,    ! ... only for the lists
     .      Nullify_Vector,   ! ... only for vector data
     .      Nullify_Profile   ! ... profile data
      end interface
! 
! !REVISION HISTORY:
!     24Apr2003  C. Redder  Original code
!     16Jun2003  C. Redder  Revised the data structure, radcor_wind, by 
!                           replacing the components U and V with Obs and
!                           kt
!     16Jul2003  C. Redder  Added the components, Indx, LevM and LevE to
!                           the data structures, radcor_mass and radcor_wind
!     15Aug2003  C. Redder  Added the module procedures, Reorder_Mass2 and
!                           Reorder_Wind2 to the generic interface,
!                           Rad_Rorder2.  Added the external interface
!                           IndexSet to the internal interface,
!                           Rad_IndexSet.
!     08Aug2003  C. Redder  Added the routines, Profile_Init, Profile_Clean,
!                           and Nullify_Profile.
!     27Oct2003  C. Redder  Added the public routines,  Rad2Prof and
!                           ReSetETime.
!     01Nov2003  C. Redder  Added the component, QM, to the data structure
!                           radcor_vector.
!     28Jan2003  C. Redder  Renamed routine from SetETime to SetDrift and
!                           added the component, StnID, to the data 
!                           structure radcor_profile.
!     03Feb2004  C. Redder  Renamed the components, Lev, LevM and LevE to
!                           P, P_m and P_e in the data structure,
!                           radcor_vector.
!     11Feb2004  C. Redder  Added the components, kx and kx_file, to the
!                           data structure, radcor_meta.
!     17Feb2004  C. Redder  Renamed the component, QM to QC_file in the
!                           data structure, radcor_meta.  Added the 
!                           sub-data structure, radcor_lists, to the
!                           structure, radcor_profiles.  Added the
!                           routines, Init_Lists, Clean_Lists and
!                           Nullify_Lists to the generic interfaces,
!                           Rad_Init, Rad_Clean, and Rad_Nullify.
!     08Mar2004  C. Redder  Added parameter constant ETHist_CARDS as a
!                           public entity.
!     21Apr2004  C. Redder  Added the components, P_Missing and
!                           ObMissing to the data structure, radcor_vector.
!     02Jun2004  C. Redder  Added the components, FG and FGMissing to the
!                           data structure, radcor_vector.
!     07Jul2004  C. Redder  Added the component, ObMissing, to the type,
!                           profile_by_vector
!     09Aug2004  C. Redder  Added the componenet, LRate, to the data 
!                           structure, profile_by_levels.  Added the 
!                           public constant, VIZ_kxList.
!     31Oct2006  C. Redder  Added public constant, kt_Humidity and use
!                           statement for module m_convert.
!     21Nov2006  C. Redder  Moved the declarations for kt_names, kt_units,
!                           kx_names, rcode_names, kx_RaobList and 
!                           MetaStr_Raobs to the module m_RadLists.
!                           Renamed VIZ_kxList to VIZ_rkx_List and moved
!                           it to module, m_VIZReg.  Moved rkx_RS80_57H
!                           and rkx_RS80_Marwin to module m_VaiUtil.
!     20Mar2007  C. Redder  Initialized the components of all types to
!                           0 or null.
!EOP
!-----------------------------------------------------------------
      character (len=*), parameter :: MyModule = 'm_RadData'
      character (len=*), parameter :: BLK = ' '

!     Parameters for data structure
!     -----------------------------
      real,    parameter ::
     .   AMiss       =  1.0e15, ! Fill value for real (DAO standard)
     .   dZMin_LRate = 250.0    ! Min thickness required to set lapse rate
      integer, parameter ::
     .   IMiss       = -10000,  ! ... and integer.
     .   nks_Def     =  1000,
     .   nkt_Def     =  10,
     .   NLev_Def    =  255,
     .   NObs_Def    =  nks_Def * NLev_Def,
     .   LenID       =  8,
     .   NPresDigits =  5       ! Number of significant digits in pres obs

      type profile_by_vector
         integer :: 
     .      NVct      =  0,      ! Size of the vectors
     .      NObs      =  0       ! Number of obs (<= NVct)
         integer, dimension (:), pointer ::
     .      LevIndx   => null(), ! Index to list entries in type profile_by_levels
     .      kt        => null(), ! Data type index
     .      QC        => null()  ! Quality control markers (see below for list)
         logical, dimension (:), pointer ::
     .      Mask      => null(), ! = .true./.false. to accept/reject
     .      ObMissing => null()  ! = .true. if observation is missing 
         real,    dimension (:), pointer ::
     .      DLat      => null(), ! Drift latitude
     .      DLon      => null(), ! ... and logitude (-90 = 90W)
     .      P         => null(), ! Pressure (hPa)
     .      logP      => null(), ! log of pressure
     .      ETime     => null(), ! Elapsed time (i.e. time since launch, s)
     .      Obs       => null(), ! Observed values at each level
     .      T         => null()  ! Temperature (deg K)
      end type

      type profile_by_levels
         integer :: 
     .      NVct      =  0,      ! Size of the vectors
     .      NLev      =  0       ! Number of levels (<= NVct)
         integer, dimension (:), pointer ::
     .      StrList   => null()  ! List of entries from type profile_by_vector
         logical, dimension (:), pointer ::
     .      Mask      => null()  ! = .true./.false. to accept/reject
         real,    dimension (:), pointer ::
     .      DLat      => null(), ! Drift latitude
     .      DLon      => null(), ! ... and longitude (-90 = 90W)
     .      P         => null(), ! Pressure (hPa)
     .      logP      => null(), ! log of pressure
     .      ETime     => null(), ! Elapsed time (i.e. time since launch, s)
     .      RRate     => null(), ! Balloon rise rate (m/s)
     .      Z         => null(), ! Geopotential height (m)
     .      T         => null(), ! Temperature (deg K)
     .      LRate     => null(), ! Lapse rate (degK / m)
     .      TV        => null(), ! Virtual temperature (before corr, deg K)
     .      TV2       => null(), ! Virtual temperature (after corr, deg K)
     .      RH        => null(), ! Relative humidity (%)
     .      MR        => null(), ! Mixing ratio (g/kg)
     .      U         => null(), ! Zonal
     .      V         => null(), ! ... and meridional wind (m/s)
     .      SE        => null(), ! Solar elevation angle (deg above horizon)
     .      TCor      => null(), ! Temperature correction (deg K or C)
     .      TCorAcc   => null(), ! Accumulated sum of temp corr (deg K)
     .      ZCor      => null()  ! Geopotential height correction (m)
      end type
!     Note: The corrected temperature is obtained by subtracting TCorAcc
!           from the uncorrected temperature.

      type radcor_profile
         character (len=LenID) ::
     .      StnID     =  BLK     ! WMO station ID
         integer :: 
     .      Date      =  0,      ! Synoptic date (YYYYMMDD)
     .      Time      =  0,      ! ... and time (HHMMSS)
     .      LTime     =  0,      ! Balloon launch time since syn. time (s)
     .      LTHist    =  0,      ! History mark of launch time (see below).
     .      ETHist    =  0,      ! History mark of elapsed times (see below).
     .      rkx       =  0,      ! Instrument type (WMO code)
     .      RadCode   =  0,      ! Radiation bias correction code (WMO code)
     .      iks       =  0,      ! Sounding index number
     .      ksLoc     =  0,      ! Location within the radcor data structure
     .      ksLen     =  0,      ! ... length of segment with the profile data
     .      ktTemp    =  0,      ! Data type index for the temperature and
     .      ktHum     =  0       ! ... humidity variable selected for radcor
         real    ::                                 ! computations.
     .      Lat       =  0,      ! Station latitude (degrees, -90 = 90S)
     .      Lon       =  0,      ! ... longitude    (degrees, -90 = 90W)
     .      Elev      =  0       ! ... elevation    (m)
         type ( profile_by_vector ) :: by_Vector
         type ( profile_by_levels ) :: by_Levels
      end type

!     ... sounding info
!     -----------------
      type radcor_meta
         integer ::
     .      NVct      =  0,      ! Size of each array
     .      Nks       =  0,      ! Number of soundings
     .      Date      =  0,      ! Synoptic date  (YYYYMMDD)
     .      Time      =  0       ! ... and time (HHMMSS)
         character (len=LenID), dimension (:), pointer ::
     .      StnID     => null()  ! WMO station ID
         real,    dimension (:), pointer ::
     .      Lon       => null(), ! Station latitude (degrees, -90 = 90S)
     .      Lat       => null(), ! ... longitude    (degrees, -90 = 90W)
     .      Elev      => null()  ! ... elevation    (m)
         integer, dimension (:), pointer ::
     .     ObTime     => null(), ! Observation time since synoptic time (s)
     .      LTime     => null(), ! Balloon launch time since synoptic time (s)
     .      LTHist    => null(), ! History mark of launch time (see below).
     .      ETHist    => null(), ! History mark of elapsed times (see below).
     .      kx        => null(), ! GMAO data source index
     .      kx_file   => null(), ! Data source index for the given file
     .      rkx       => null(), ! Instrument type (WMO code)
     .      RadCode   => null(), ! Radiation bias correction code (WMO code)
     .      ksLoc     => null(), ! Location ...
     .      ksLen     => null()  ! ... and amount of profile data
      end type

      type radcor_lists
         integer ::
     .      kt_max       =  0,   ! Maximum value for GMAO data type index (kt) 
     .      kx_max       =  0,   ! ... GMAO data source index (kx)
     .      rkx_max      =  0,   ! ... WMO instrument type
     .      rcode_max    =  0,   ! ... WMO radiation bias correction codes
     .      kx_max_file  =  0,   ! ... kx defined for the input file
     .      qcx_max_file =  0    ! ... QC exclusion flag defined for the  
         character (len=255), dimension (:), pointer :: !  input file
     .      kt_names  => null(), ! GMAO data type names
     .      kt_units  => null(), ! ... and units
     .      kx_names  => null(), ! GMAO data source names
     .      kx_meta   => null(), ! ... and information for meta-data
     .      rkx_names => null(), ! WMO instrument type names
     .      rcode_names    => null(), ! WMO radiation bias correction codes
     .      kx_names_file  => null(), ! Data type names 
     .      kx_meta_file   => null(), ! ... and meta-data
     .      qcx_names_file => null()  ! and qcx flags as defined for the
      end type                        !    input file.

!     ... observations (mass and wind)
!     --------------------------------
      type radcor_vector
         integer ::
     .      NVct      =  0,      ! Size of each array
     .      NObs      =  0       ! Total number of observations
         real,    dimension (:), pointer ::
     .      DLat      => null(), ! Drift latitude
     .      DLon      => null(), ! ... and logitude (-90 = 90W)
     .      P         => null(), ! Pressure (hPa)
     .      ETime     => null(), ! Elapsed time (i.e. time since launch, s)
     .      QC_file   => null(), ! Quality control markers from file
     .      Obs       => null(), ! Observed mass value
     .      FG        => null()  ! First guess or forecast value
         logical, dimension (:), pointer ::
     .      P_Missing => null(), ! = .true. for missing values
     .      ObMissing => null(), ! ... observation values
     .      FGMissing => null()  ! ... first guess or forecast values.
         integer, dimension (:), pointer ::
     .      Indx      => null(), ! Indices
     .      P_m       => null(), ! Mantessa 
     .      P_e       => null(), ! ... and exponent of pressure
     .      kt        => null(), ! Data type index
     .      QC        => null()  ! QC markers (see below for table)
      end type

!     ... entire data structure (except scratch space)
!     ------------------------------------------------
      type radcor_profiles
         character (len=255)    :: FileType = BLK
         character (len=255)    ::  ObsFile = BLK
         character (len=255)    :: MetaFile = BLK
         integer                :: OptionSet ! 0 for default 
         type ( radcor_meta   ) :: Meta
         type ( radcor_lists  ) :: Lists
         type ( radcor_vector ) :: Vector
      end type

!     Notes: The data in the structure, Vector, must be sorted in
!            descending order of pressure level within each sounding.
!            The elapsed time for all underground obs must be set to zero,
!            and the rise rate is set to the value at launch time
!     --------------------------------------------------------------------

!     Data type index for humidity
!     ----------------------------
      integer, parameter ::
     .   kt_Humidity      = Dew_Point

!     WMO radation correction code for missing
!     ----------------------------------------
      integer, parameter ::
     .   RadCode_AMiss    = 15

!     History marks for launch time
!     -----------------------------
      integer, parameter ::
     .   LTHist_IMiss     = -1,
     .   LTHist_File      =  0,
     .   LTHist_ObTime    =  1,
     .   LTHist_RObTime   =  2

!     ... and for elapsed time 
!     ------------------------
      integer, parameter ::
     .   ETHist_IMiss     = -1,
     .   ETHist_File      =  0,
     .   ETHist_RRProfile =  1,
     .   ETHist_CARDS     =  2,
     .   ETHist_Min       = -1,
     .   ETHist_Max       =  2

!     Quality control markers denoting ...
!     ------------------------------------
      integer, parameter ::
     .   QC_Max           =  2, ! Max value for Radcor QC
     .   QC_OK            =  0, ! OK or valid
     .   QC_Suspect       =  1, ! ... suspect
     .   QC_Rejected      =  2, ! ... rejected observation
     .   QC_NotTested     = -1, ! No QC test perfomred
     .   QC_ListSz        =  2  ! Size of ... 
      integer, parameter ::     ! ... the list of acceptable QC marks
     .   QC_AcceptList ( QC_ListSz ) = (/ QC_OK, QC_Suspect /)

!     data type indices (kt, see module m_convert for temp and humidity)
!     ------------------------------------------------------------------
      integer, parameter ::
     .   UWind  = 4,
     .   VWind  = 5,
     .   Height = 6

!     Status codes
!     ------------
      integer, parameter ::
     .   No_Error         = 0,  ! Valid error status; no error
     .   Alloc_Error      = 5,  ! Allocation error
     .   Dealloc_Error    = 6   ! Deallocation error

      contains

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  SetPath - Set parameters related to the radiosonde path
!
! !INTERFACE:
      subroutine SetPath ( profile )
      use m_SunAlt, only : SunAlt
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profile ), intent ( inout ) ::
     .   profile   ! Data for the selected profile (sounding)
!
! !DESCRIPTION:
!     This routine computes the latitude, longitude and solar angle
!     of each point of the radiosonde path.    This routine assumes
!     that the routine, SetETime, has been called.
!
! !REVISION HISTORY:
!     24Nov2003  C. Redder  Initial code
!EOP
!.................................................................

      integer :: NLev, SYMDH, LTime
      real,    dimension (:), pointer :: ETime, U, V, DLat, DLon, SE

      NLev    =  profile % by_Levels % NLev
      ETime   => profile % by_Levels % ETime ( : NLev )
      DLat    => profile % by_Levels % DLat  ( : NLev )
      DLon    => profile % by_Levels % DLon  ( : NLev )
      SE      => profile % by_Levels % SE    ( : NLev )

      SYMDH   =             profile  % Date * 100
     .        +             profile  % Time / 10000
      LTime   =             profile  % LTime
     .        +  60 * mod ( profile  % Time,  10000 ) / 100
     .        +       mod ( profile  % Time,    100 )
      call SunAlt ( DLat, DLon, SYMDH, LTime, ETime, SE )

      return
      end subroutine SetPath
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  SetDrift - Set balloon drift data (i.e. ellapsed time and drift coordinates)
!
! !INTERFACE:
      subroutine SetDrift ( profile, use_fdata )
      use m_soundings, only : Z2RRate, Z2Time, ZT2RRate, LLPath
!
! !USES:
      implicit NONE
!
! !INPUT PARAMETERS:
      logical, optional,       intent ( in )    ::
     .   use_fdata ! = .true. to use elapse time from the input data
                   !   file.  Default: use_fdata = .true. if elasped
                   !   time is from data file or computed from rise
!                  !   rate profile
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profile ), intent ( inout ) ::
     .   profile   ! Data for the selected profile (sounding)
!
! !DESCRIPTION:
!     This routine sets the ellapsed time, if desired, to values from
!     the rise rate based on empirical equations and then computes the
!     balloon drift lat/lon.  This routine assumes that the routine,
!     SetupProf, has been called.
!
! !REVISION HISTORY:
!     27Oct2003  C. Redder  Initial code
!     28Jan2003  C. Redder  Renamed routine from SetETime to SetDrift.
!                           Enabled routine to compute drift lat/lon
!EOP
!.................................................................
      real    :: StnElev, StnLat, StnLon
      integer :: rkx, NLev, iLev, NVObs, iOb, ETHist_Code
      real,    dimension (:), pointer :: Z, RRate, ETime, ETime_v, U, V,
     .                                   DLat, DLat_v, DLon, DLon_v
      integer, dimension (:), pointer :: LevIndx
      logical :: reset

!     Determine if time should be reset
!     ---------------------------------
      ETHist_Code = profile % ETHist
      reset       = ETHist_Code .lt. ETHist_RRProfile .or.
     .              ETHist_Code .gt. ETHist_Max
c      reset = .false.
      if ( ETHist_Code .eq. ETHist_File .or.
     .     ETHist_Code .eq. ETHist_RRProfile ) then
         if ( present ( use_fdata )) reset = .not. use_fdata
      end if

      NLev    =  profile % by_Levels % NLev
      Z       => profile % by_Levels % Z     ( : NLev ) 
      RRate   => profile % by_Levels % RRate ( : NLev ) 
      ETime   => profile % by_Levels % ETime ( : NLev ) 
      rkx     =  profile % rkx
      StnElev =  profile % Elev

!     Use rise-rate equations based on empirical
!     fits to derive elapsed time for each ob
!     ------------------------------------------
      if ( reset ) then
         if ( rkx .eq. 32 ) then  ! Special case for Chinese GZZ sondes
            call Z2RRate ( Z, 'GZZ',   RRate )
         else
            call Z2RRate ( Z, ' ',     RRate )
         end if
         call Z2Time     ( Z, StnElev, RRate, ETime )
         profile % ETHist = ETHist_RRProfile
      else
         call ZT2RRate   ( Z, StnElev, ETime, RRate )
      end if

!     Determine the coordinates of the balloon path
!     ---------------------------------------------
      NLev    =  profile % by_Levels % NLev
      StnLat  =  profile % Lat
      StnLon  =  profile % Lon
      ETime   => profile % by_Levels % ETime ( : NLev )
      U       => profile % by_Levels % U     ( : NLev )
      V       => profile % by_Levels % V     ( : NLev )
      DLat    => profile % by_Levels % DLat  ( : NLev )
      DLon    => profile % by_Levels % DLon  ( : NLev )
      call LLPath ( StnLat, StnLon, ETime, U, V, DLat, DLon )

!     Store elapsed time to vector portion
!     ------------------------------------
      NVObs   =  profile % by_Vector % NObs
      LevIndx => profile % by_Vector % LevIndx ( : NVObs )
      DLat_v  => profile % by_Vector % DLat    ( : NVObs )
      DLon_v  => profile % by_Vector % DLon    ( : NVObs )
      ETime_v => profile % by_Vector % ETime   ( : NVObs )
      do iOb  = 1, NVObs
         iLev = LevIndx ( iOb )
         DLat_v  ( iOb ) = DLat  ( iLev )
         DLon_v  ( iOb ) = DLon  ( iLev )
         ETime_v ( iOb ) = ETime ( iLev )
      end do

      return
      end subroutine SetDrift
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  UpdateProfs - Update the profile data structure, radcor_profiles
!
! !INTERFACE:
      subroutine UpdateProfs ( profile, profiles )
      use m_convert, only : toHVar, toTVar, ktMask, SelectHEq,
     .                      kt_AT_Update => Air_Temperature,
     .                      kt_RH_Update => Relative_Humidity
!
! !USES:
      implicit NONE
!
! !INPUT PARAMETERS:
      type ( radcor_profile  ), intent ( in )    ::
     .   profile  ! Data structure to be updated
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profiles ), intent ( inout ) ::
     .   profiles ! Data structure to be updated
!
! !DESCRIPTION:
!     For given temperature corrections, this routine updates the
!     profiles in the data structure, radcor_profiles.
!
! !REVISION HISTORY:
!     20Nov2003  C. Redder  Initial code
!     20Nov2006  C. Redder  Modified code to handle the humidity 
!                           conversion when the raob instrument type
!                           is Vaisala.
!     07Dec2006  C. Redder  Renamed kt_RH and kt_AT to kt_RH_Setup
!                           and kt_AT_Setup
!EOP
!.................................................................

      integer :: ksBeg, ksEnd, NVObs, iOb, rkx, SVP_Eqtn
      logical :: mask_in, save_drift
      real,    dimension (:), pointer :: P_v, T_v,
     .                                   DLat_r, DLon_r, ETime_r, Obs_r,
     .                                   DLat_v, DLon_v, ETime_v, Obs_v
      integer, dimension (:), pointer :: QC_v, kt_v
      logical, dimension (:), pointer :: Mask_v

!     Extract location of the mass ...
!     --------------------------------
      ksBeg =  profile % ksLoc
      NVObs =  profile % ksLen
      ksEnd =  ksBeg + NVObs - 1

!     Copy the observations to the radcor data structure
!     --------------------------------------------------
      Obs_r  => profiles %    Vector % Obs    ( ksBeg : ksEnd )
      Obs_v  => profile  % by_Vector % Obs    (       : NVObs )
      do iOb = 1, NVObs
         Obs_r   ( iOb ) = Obs_v   ( iOb )
      end do

!     ... and, if desired, the drift data into the radcor data structure
!     ------------------------------------------------------------------
      save_drift = .true.
      DLat_r  => profiles %    Vector % DLat  ( ksBeg : ksEnd )
      DLon_r  => profiles %    Vector % DLon  ( ksBeg : ksEnd )
      ETime_r => profiles %    Vector % ETime ( ksBeg : ksEnd )
      DLat_v  => profile  % by_Vector % DLat  (       : NVObs )
      DLon_v  => profile  % by_Vector % DLon  (       : NVObs )
      ETime_v => profile  % by_Vector % ETime (       : NVObs )
      if ( save_drift ) then
         do iOb = 1, NVObs
            DLat_r  ( iOb ) = DLat_v  ( iOb )
            DLon_r  ( iOb ) = DLon_v  ( iOb )
            ETime_r ( iOb ) = ETime_v ( iOb )
         end do
      end if

!     Convert back to the original variables for humidity
!     ---------------------------------------------------
      rkx      = profile % rkx
      SVP_Eqtn = SelectHEq ( rkx )
      P_v    => profile % by_Vector % P    ( : NVObs )
      T_v    => profile % by_Vector % T    ( : NVObs )
      kt_v   => profile % by_Vector % kt   ( : NVObs )
      Mask_v => profile % by_Vector % Mask ( : NVObs )
      QC_v   => profile % by_Vector % QC   ( : NVObs )
      do iOb = 1, NVObs
         Mask_v ( iOb ) = QC_v ( iOb ) .ne. QC_Rejected
      end do
      call toHVar ( P_v, T_v, kt_v, Mask_v, kt_RH_Update, Obs_r,
     .              backwd   = .true.,
     .              SVP_Eqtn =  SVP_Eqtn )

!     ... for temperature
!     -------------------
      do iOb = 1, NVObs
         Mask_v ( iOb ) = QC_v ( iOb ) .ne. QC_Rejected
      end do
      call ktMask ( kt_v, Mask_v )
      call toTVar ( P_v,  kt_v, Mask_v, kt_AT_Update, Obs_r,
     .              backwd = .true. )

      return
      end subroutine UpdateProfs

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  UpdateProf - Update the data structure, radcor_profile
!
! !INTERFACE:
      subroutine UpdateProf ( profile )
      use m_convert,     only : ktMask
      use m_soundings,   only : CalcZ,   dYdx
      use m_humidity,    only : RHTtoVP, VPtoMR, CheckMR
      use m_temperature, only : ATtoVT
      use m_stdatm,      only : StdZ2T
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profile  ), intent ( inout ) ::
     .   profile  ! Data structure to be updated
!
! !DESCRIPTION:
!     For given temperature corrections, this routine updates the
!     profiles in the data structures, radcor_profile.  The routine
!     resets the temperature and heights corrections to zero.
!
! !REVISION HISTORY:
!     20Nov2003  C. Redder  Initial code
!     07Jul2004  C. Redder  Modified code to apply corrections to all
!                           temperature and height data except those with
!                           missing values regardless of the QC mark.
!     11Aug2004  C. Redder  Added code to recompute lapse rate.
!     07Dec2006  C. Redder  Renamed kt_RH and kt_AT to kt_RH_Update
!                           and kt_AT_Update
!     09Aug2007  C. Redder  Fixed bugged by initializing NVObs before 
!                           accessing it in other statements.
!EOP
!.................................................................

      integer, parameter ::
     .   kt_Height = 6,
     .   kt_UWind  = 4,
     .   kt_VWind  = 5
      integer :: NVObs, iOb, NLev, iLev
      logical :: mask_in
      real    :: LRate_def
      real,    dimension (:), pointer :: P_v, P, logP, Obs_v, LRate,
     .                                   T_v, T, RH, Z, MR, TV, TVCor,
     .                                   TV2, SE, TCor, TCorAcc, ZCor
      integer, dimension (:), pointer :: QC_v, LevIndx,
     .                                   kt_v, StrList
      logical, dimension (:), pointer :: Mask_v, ObMissing_v

!     Add temperature corrections to the temperature at each level
!     ------------------------------------------------------------
      NLev    =  profile % by_Levels % NLev
      T       => profile % by_Levels % T       ( : NLev )
      TCor    => profile % by_Levels % TCor    ( : NLev )
      TCorAcc => profile % by_Levels % TCorAcc ( : NLev )
      do iLev = 1, NLev
         T ( iLev ) = T ( iLev ) - TCorAcc ( iLev )
      end do

!     ... and by vector
!     -----------------
      NVObs   =  profile % by_Vector % NObs
      LevIndx => profile % by_Vector % LevIndx ( : NVObs )
      T_v     => profile % by_Vector % T       ( : NVObs )
      do iOb  = 1, NVObs
         iLev = LevIndx ( iOb )
         T_v ( iOb ) = T  ( iLev )
      end do

!     For all levels, compute the mixing ratio
!     ----------------------------------------
      P       => profile % by_Levels % P  ( : NLev )
      RH      => profile % by_Levels % RH ( : NLev )
      MR      => profile % by_Levels % MR ( : NLev )
      call RHTtoVP (  RH,  T, P, MR )
      call  VPtoMR ( (MR),    P, MR ) 
      call CheckMR (       T, P, MR )

!     ... virtual temperature
!     -----------------------
      TV2 => profile % by_Levels % TV2 ( : NLev )
      call ATtoVT ( T, MR, TV2 )

!     ... virtual temperature corrections
!     -----------------------------------
      TV    => profile % by_Levels % TV   ( : NLev )
      TVCor => profile % by_Levels % TV   ( : NLev ) ! Use TV as scratch
      do iLev = 1, NLev
         TVCor ( iLev ) = TV ( iLev ) - TV2 ( iLev )
      end do

!     ... height correction corrections
!     ---------------------------------
      logP => profile % by_Levels % logP  ( : NLev )
      ZCor => profile % by_Levels % ZCor  ( : NLev )
      call CalcZ ( logP, TVCor, ZCor, logP = .true. )

!     ... heights
!     ---------------
      Z   => profile % by_Levels % Z ( : NLev )
      do iLev = 1, NLev
         Z ( iLev ) = Z ( iLev ) - ZCor ( iLev )
      end do

!     Recompute the lapse rate
!     ------------------------
      LRate     => profile % by_Levels % LRate ( : NLev )
      LRate_def = ( StdZ2T ( dZMin_LRate ) - StdZ2T ( 0.0 ))
     .          /            dZMin_LRate
      call dYdx ( Z, T, dZMin_LRate, LRate, c = LRate_def )

!     Update the temperature obs in the ob vector
!     -------------------------------------------
      Mask_v      => profile % by_Vector % Mask      ( : NVObs )
      QC_v        => profile % by_Vector % QC        ( : NVObs )
      kt_v        => profile % by_Vector % kt        ( : NVObs )
      Obs_v       => profile % by_Vector % Obs       ( : NVObs )
      ObMissing_v => profile % by_Vector % ObMissing ( : NVObs )
      do iOb = 1, NVObs
c          Mask_v ( iOb ) = QC_v ( iOb ) .ne. QC_Rejected
          Mask_v ( iOb ) = .not. ObMissing_v ( iOb )
      end do
      call ktMask ( kt_v, Mask_v )
      do iOb = 1, NVObs
         iLev    = LevIndx ( iOb )
         mask_in = Mask_v  ( iOb )
         if ( mask_in ) Obs_v ( iOb ) = Obs_v ( iOb ) - TCorAcc ( iLev )
      end do

!     ... for heights
!     ---------------
      do iOb = 1, NVObs
         iLev    =  LevIndx ( iOb )
         mask_in = .not. ObMissing_v ( iOb ) .and.
     .              kt_v ( iOb ) .eq. kt_Height
c         mask_in = QC_v ( iOb ) .ne. QC_Rejected .and.
c     .             kt_v ( iOb ) .eq. kt_Height
         if ( mask_in ) Obs_v ( iOb ) = Obs_v ( iOb ) - ZCor    ( iLev )
      end do

!     Reset TCor and ZCor
!     -------------------
      TV      ( : NLev ) = TV2 ( : NLev )
      TCor    ( : NLev ) = 0.0
      TCorAcc ( : NLev ) = 0.0
      ZCor    ( : NLev ) = 0.0

      return
      end subroutine UpdateProf

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  SetupProf - Get profile data from radcor data structure.
!
! !INTERFACE:
      subroutine SetupProf ( iks, profiles, profile )
      use m_convert,     only : SelectVar, Humidity, toHVar, toTVar,
     .                          kt_AT_Setup => Air_Temperature,
     .                          kt_RH_Setup => Relative_Humidity,
     .                          SelectHEq
      use m_soundings,   only : CheckP,  SetPList, logP2Y, ExtrapM,
     .                          CalcZ,   P2LogP,   dYdx
      use m_humidity,    only : RHTtoVP, VPtoMR, CheckMR
      use m_temperature, only : ATtoVT
      use m_stdatm,      only : StdP2Z,  StdZ2T
!
! !USES:
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                  intent ( in )    ::
     .   iks      ! index number for the selected profile (sounding)
      type ( radcor_profiles ), intent ( in )    ::
     .   profiles ! data structure for all profiles (soundings)
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profile  ), intent ( inout ) ::
     .   profile  ! ... for the selected profile (sounding)
!
! !DESCRIPTION:
!     This routine gets some of the profile data from the radcor
!     data structure.
!
! !REVISION HISTORY:
!     20Nov2003  C. Redder  Initial code
!     28Jan2004  C. Redder  Set drift lat/lon from routine input and
!                           added the component, StnID, to the data 
!                           structure, radcor_profile.
!     08Jul2004  C. Redder  Added the component, ObMissing, to the
!                           data structure, profile_by_vector. 
!     11Aug2004  C. Redder  Added code to compute lapse rate.
!     20Nov2006  C. Redder  Modified code to handle the humidity 
!                           conversion when the raob instrument type
!                           is Vaisala.
!EOP
!.................................................................

      real,    parameter ::
     .   RH_Default = 50.0
      integer, parameter ::
     .   kt_Height = 6,
     .   kt_UWind  = 4,
     .   kt_VWind  = 5
      integer :: ksBeg, ksEnd, NVObs, kt_Temp, kt_Humidity,
     .           iOb, NLev, iLev, rkx, SVP_Eqtn
      real    :: PLat,  PLon, PMin, LRate_def, StnElev
      real,    dimension (:), pointer :: DLat_v, DLat_r, DLat,
     .                                   DLon_v, DLon_r, DLon,
     .                                   P_r, P_v, P, logP_v, logP,
     .                                   ETime_r, ETime_v, ETime,
     .                                   Obs_r, Obs_v,
     .                                   T_v, T, RH, LRate,
     .                                   Z, U, V, MR, TV, TV2,
     .                                   SE, TCor, TCorAcc, ZCor, RRate
      integer, dimension (:), pointer :: QC_r, QC_v, QC, LevIndx,
     .                                   kt_r, kt_v, kt, StrList
      logical, dimension (:), pointer :: Mask, Mask_v, ObMissing_v,
     .                                                 ObMissing_r

!     Extract location of the mass ...
!     --------------------------------
      ksBeg =  profiles % Meta % ksLoc ( iks )
      NVObs =  profiles % Meta % ksLen ( iks )
      ksEnd =  ksBeg + NVObs - 1

!     Save meta data information
!     --------------------------
      profile % StnID   = profiles % Meta % StnID   ( iks )
      profile % Lat     = profiles % Meta % Lat     ( iks )
      profile % Lon     = profiles % Meta % Lon     ( iks )
      profile % Elev    = profiles % Meta % Elev    ( iks )
      profile % Date    = profiles % Meta % Date
      profile % Time    = profiles % Meta % Time
      profile % LTime   = profiles % Meta % LTime   ( iks )
      profile % LTHist  = profiles % Meta % LTHist  ( iks )
      profile % ETHist  = profiles % Meta % ETHist  ( iks )
      profile % rkx     = profiles % Meta % rkx     ( iks )
      profile % RadCode = profiles % Meta % RadCode ( iks )
      profile % iks     = iks     
      profile % ksLoc   = ksBeg
      profile % ksLen   = NVObs

!     Isolate the section of the radcor data structure
!     with the data for the desired profile
!     ------------------------------------------------
      DLat_r       => profiles % Vector % DLat      ( ksBeg : ksEnd )
      DLon_r       => profiles % Vector % DLon      ( ksBeg : ksEnd )
      P_r          => profiles % Vector % P         ( ksBeg : ksEnd )
      kt_r         => profiles % Vector % kt        ( ksBeg : ksEnd )
      QC_r         => profiles % Vector % QC        ( ksBeg : ksEnd )
      ETime_r      => profiles % Vector % ETime     ( ksBeg : ksEnd )
      Obs_r        => profiles % Vector % Obs       ( ksBeg : ksEnd )
      ObMissing_r  => profiles % Vector % ObMissing ( ksBeg : ksEnd )

!     Transfer data from radcor to profile data structure
!     ---------------------------------------------------
      DLat_v       => profile  % by_Vector % DLat      ( : NVObs ) 
      DLon_v       => profile  % by_Vector % DLon      ( : NVObs ) 
      P_v          => profile  % by_Vector % P         ( : NVObs ) 
      logP_v       => profile  % by_Vector % logP      ( : NVObs ) 
      ETime_v      => profile  % by_Vector % ETime     ( : NVObs )
      kt_v         => profile  % by_Vector % kt        ( : NVObs ) 
      QC_v         => profile  % by_Vector % QC        ( : NVObs )
      Obs_v        => profile  % by_Vector % Obs       ( : NVObs )
      T_v          => profile  % by_Vector % T         ( : NVObs ) 
      Mask_v       => profile  % by_Vector % Mask      ( : NVObs ) 
      ObMissing_v  => profile  % by_Vector % ObMissing ( : NVObs ) 
      do iOb = 1, NVObs
         DLat_v      ( iOb ) = DLat_r      ( iOb )
         DLon_v      ( iOb ) = DLon_r      ( iOb )
         P_v         ( iOb ) = P_r         ( iOb )
         kt_v        ( iOb ) = kt_r        ( iOb )
         QC_v        ( iOb ) = QC_r        ( iOb )
         ETime_v     ( iOb ) = ETime_r     ( iOb )
         Obs_v       ( iOb ) = Obs_r       ( iOb )
         ObMissing_v ( iOb ) = ObMissing_r ( iOb )
      end do

!     Check pressure for reasonable values
!     ------------------------------------
      call CheckP ( P_v, QC_Rejected, QC_v )

!     Generate a list of pressure levels
!     ----------------------------------
      LevIndx => profile  % by_Vector % LevIndx ( : NVObs )
      StrList => profile  % by_Levels % StrList ( : NVObs )
      P       => profile  % by_Levels % P       ( : NVObs )
      call SetPList  ( P_v, NLev, P, StrList, LevIndx )

      DLat    => profile  % by_Levels % DLat    ( : NVObs )
      DLon    => profile  % by_Levels % DLon    ( : NVObs )
      ETime   => profile  % by_Levels % ETime   ( : NVObs )
      do iLev = 1, NLev
         iOb  = StrList ( iLev )
         DLat  ( iLev ) = DLat_v  ( iOb )
         DLon  ( iLev ) = DLon_v  ( iOb )
         ETime ( iLev ) = ETime_v ( iOb )
      end do

!     Calculate the log of pressure
!     -----------------------------
      P       => profile  % by_Levels % P       ( : NLev )
      logP    => profile  % by_Levels % logP    ( : NLev ) 
      call P2logP ( P, logP )
      do iOb = 1, NVObs
         iLev = LevIndx ( iOb )
         logP_v ( iOb ) = logP ( iLev )
      end do

!     Get temperature profile
!     -----------------------
      do iOb = 1, NVObs
         Mask_v  ( iOb ) = QC_v ( iOb ) .ne. QC_Rejected
      end do
      call toTVar   ( P_v, kt_v, Mask_v, kt_AT_Setup, Obs_v )
      kt_Temp = SelectVar ( P_v, kt_v, Mask_v )
      profile % ktTemp = kt_Temp

      do iOb = 1, NVObs
         iLev = LevIndx ( iOb )
         Mask_v ( iOb ) = Mask_v ( iOb ) .and.
     .                    kt_v   ( iOb ) .eq. kt_Temp
      end do

      T  => profile % by_Levels % T  ( : NLev )
      call logP2Y  ( logP_v, Mask_v, Obs_v, logP, T, logP = .true. )
      call ExtrapM ( logP_v, Mask_v, Obs_v, logP, T, logP = .true.,
     .                                               var  =  8 )
      do iOb = 1, NVObs
         iLev = LevIndx ( iOb )
         T_v ( iOb ) = T ( iLev )
      end do

!     ... and humidity profile
!     ------------------------
      rkx      =  profile % rkx
      SVP_Eqtn =  SelectHEq ( rkx )
      RH       => profile % by_Levels % RH ( : NLev )
      do iOb = 1, NVObs
         Mask_v ( iOb ) = QC_v ( iOb ) .ne. QC_Rejected
      end do
      call toHVar ( P_v, T_v, kt_v, Mask_v, kt_RH_Setup, Obs_v,
     .              SVP_Eqtn = SVP_Eqtn )
      kt_Humidity = SelectVar ( P_v, kt_v, Mask_v, var = Humidity )
      profile % ktHum = kt_Humidity

      do iOb = 1, NVObs
         Mask_v ( iOb ) = Mask_v ( iOb ) .and.
     .                    kt_v   ( iOb ) .eq. kt_Humidity
      end do
      call logP2Y ( logP_v, Mask_v, Obs_v, logP, RH,
     .              C = RH_Default, logP = .true. )

!     For all levels, compute the mixing ratio
!     ----------------------------------------
      MR => profile % by_Levels % MR ( : NLev )
      call RHTtoVP (  RH,  T, P, MR )
      call  VPtoMR ( (MR),    P, MR ) 
      call CheckMR (       T, P, MR )

!     ... virtual temperature
!     -----------------------
      TV => profile % by_Levels % TV ( : NLev )
      call ATtoVT ( T, MR, TV )

!     ... geopotential heights
!     ------------------------
      Z           => profile % by_Levels % Z  ( : NLev )
      StnElev     =  profile % Elev
c      iOb_Closest = 0               ! For anchor search for ob level
c      do iOb = 1, NVObs             !   closest to station elevation
c         if ( QC_v ( iOb ) .ne. QC_Rejected .and.
c     .        kt_v ( iOb ) .eq. kt_Height ) then
c            Delta = abs ( Obs_v ( iOb ) - StnElev )
c            if ( iOb_Closest .ne. 0 ) then
c               if ( Delta .ge. DeltaLast ) exit
c               if ( Delta .lt. DeltaMin  ) then
c                  DeltaMin = Delta
c                  iOb_Closest = iOb
c               end if
c            else
c               DeltaMin = Delta
c               iOb_Closest = iOb
c            end if
c            DeltaLast = Delta
c         end if
c      end do
c      if ( iOb_Closest .ne. 0 ) then
c         iLev_Closest = LevIndx (  iOb_Closest )
c         logP0        = logP    ( iLev_Closest )
c         TV0          = TV      ( iLev_Closest )
c         Z0           = Obs_v   (  iOb_Closest )
c      else
c         logP0        = LogP   ( 1 )
c         TV0          = TV     ( 1 )
c         Z0           = StdP2Z ( P ( 1 ))
c      end if
c      call CalcZ ( logP0, TV0, Z0, logP, TV, Z, logP = .true. )
      call CalcZ ( logP, TV, Z, logP = .true. )
      do iOb = 1, NVObs
         Mask_v ( iOb ) = QC_v ( iOb ) .ne. QC_Rejected .and.
     .                    kt_v ( iOb ) .eq. kt_Height
      end do
      call logP2Y ( logP_v, Mask_v, Obs_v, logP, Z,
     .              YExt = (Z), logP = .true. )
c      call logP2Y (    P_v, Mask_v, Obs_v,    P, Z,
c     .              YExt = (Z), logP = .false. )

!     ... lapse rate
!     --------------
      LRate     => profile % by_Levels % LRate ( : NLev )
      LRate_def = ( StdZ2T ( dZMin_LRate ) - StdZ2T ( 0.0 ))
     .          /            dZMin_LRate
      call dYdx ( Z, T, dZMin_LRate, LRate, c = LRate_def )


!     ... u-winds
!     -----------
      U  => profile % by_Levels % U  ( : NLev )
      do iOb = 1, NVObs
         Mask_v ( iOb ) = QC_v ( iOb ) .ne. QC_Rejected .and.
     .                    kt_v ( iOb ) .eq. kt_UWind
      end do
      call logP2Y ( logP_v, Mask_v, Obs_v, logP, U, logP = .true. )

!     ... and v-winds
!     ---------------
      V  => profile % by_Levels % V  ( : NLev )
      do iOb = 1, NVObs
         Mask_v ( iOb ) = QC_v ( iOb ) .ne. QC_Rejected .and.
     .                    kt_v ( iOb ) .eq. kt_VWind
      end do
      call logP2Y ( logP_v, Mask_v, Obs_v, logP, V, logP = .true. )

!     Set all derived winds to zero that are at levels above
!     the highest with a valid ob value (consistent with NCEP)
!     --------------------------------------------------------
      PMin = minval ( P_v ( : NVObs ), Mask = Mask_v ( : NVObs ))
      do iLev = NLev, 1, -1
         if ( P ( iLev ) .lt. PMin ) then
            U ( iLev ) = 0.0
            V ( iLev ) = 0.0
         else
            exit
         end if
      end do

!     Set defaults for the remaining undefined parameters
!     ---------------------------------------------------
      TV2     => profile % by_Levels % TV2
      SE      => profile % by_Levels % SE
      TCor    => profile % by_Levels % TCor
      TCorAcc => profile % by_Levels % TCorAcc
      ZCor    => profile % by_Levels % ZCor
      do iLev = 1, NLev
         TV2      ( iLev ) = TV ( iLev )
         SE       ( iLev ) = -90.0
         TCor     ( iLev ) =   0.0
         TCorAcc  ( iLev ) =   0.0
c         TCor     ( iLev ) =   1.0
         ZCor     ( iLev ) =   0.0
      end do

!     Save number of levels and obs in profile
!     ----------------------------------------
      profile % by_Levels % NLev = NLev
      profile % by_Vector % NObs = NVObs

c      if ( nint(profile%Lat*100.0).eq. 4269 .and.
c     .     nint(profile%Lon*100.0).eq.-7383) then
c         do iLev=1, NLev
c            print *, iLev, P(iLev) 
c         end do
c      end if

      return
      end subroutine SetupProf

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_All --- Reorder vector data structure (except sorting indices)
! 
! !INTERFACE:
      subroutine Reorder_All ( This, reverse )
!
! !USES
      use m_AdvError, only : WPErr, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      logical, optional,        intent (in)    ::
     .   reverse       ! = .true. to reorder in reverse.
                       !   Default: reverse = .false.
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profiles ), intent (inout) ::
     .   This          ! data structure
!
! !DESCRIPTION:
!     This routine sorts the vector compoment of the radcor data structure
!     except the sorting indices (i.e. the component Indx)
!
! !REVISION HISTORY: 
!     21Jul2003  C. Redder  Original code
!     12Nov2003  C. Redder  Replace data structure Mass and Wind with only
!                           Vector
!EOP
!-------------------------------------------------------------------------

      call Reorder_Vector ( This % Vector, reverse = reverse )

      return
      end subroutine Reorder_All
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_Vector --- Reorder vector data structure (except sorting indices)
! 
! !INTERFACE:
      subroutine Reorder_Vector ( This, reverse )
!
! !USES
      use m_AdvError, only : WPErr, ErrStat
!
! !INPUT PARAMETERS:
      logical, optional,      intent (in)    ::
     .   reverse      ! = .true. to reorder in reverse.
                      !   Default: reverse = .false.
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_vector ), intent (inout) ::
     .   This         ! data structure
!
! !DESCRIPTION:
!     This routine sorts the vector compoment of the radcor data structure
!     except the sorting indices (i.e. the component Indx)
!
! !REVISION HISTORY: 
!     21Jul2003  C. Redder  Original code
!     15Aug2003  C. Redder  Replaced code with call to Reorder_Mass2
!     12Nov2003  C. Redder  Renamed routine from Recorder_Mass to 
!                           Reorder_Vector.  Replace mass data structure
!                           with the data structure, radcor_vector
!EOP
!-------------------------------------------------------------------------

      call Reorder_Vector2 ( ( This % Indx ), This, reverse )

      return
      end subroutine Reorder_Vector
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_Vector2 --- Reorder vector data structure (except sorting indices)
! 
! !INTERFACE:
      subroutine Reorder_Vector2 ( Indx, This, reverse )
!
! !USES
      use m_AdvError, only : WPErr, ErrStat
!
! !INPUT PARAMETERS:
      integer, dimension (:), intent (in)    ::
     .   Indx         ! Sorting indices.
      logical, optional,      intent (in)    ::
     .   reverse      ! = .true. to reorder in reverse.
                      !   Default: reverse = .false.
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_vector ), intent (inout) ::
     .   This         ! data structure
!
! !DESCRIPTION:
!     This routine sorts the vector compoment of the radcor data structure.
!
! !REVISION HISTORY: 
!     15Aug2003  C. Redder  Original code
!     12Nov2003  C. Redder  Renamed routine from Recorder_Mass to 
!                           Reorder_Vector.  Replace mass data structure
!                           with the data structure, radcor_vector
!     01Dec2003  C. Redder  Added the component, QM
!     28Jan2004  C. Redder  Added the components for drift lat/lon
!     03Feb2004  C. Redder  Renamed the components, Lev, LevM and LevE to
!                           P, P_m and P_e in the data structure,
!                           radcor_vector.
!     17Feb2004  C. Redder  Renamed the component, QM to QC_file
!     21Apr2004  C. Redder  Added the components, P_Missing and ObMissing
!     02Jun2004  C. Redder  Added the components, FG and FGMissing
!EOP
!-------------------------------------------------------------------------

      integer :: NObs, iOb
      logical :: reverse_

      reverse_ = .false.
      if ( present ( reverse )) reverse_ = reverse

      NObs = size ( Indx )
      if ( reverse_ ) then
         This % DLat           ( 1 : NObs )
     .      = This % DLat      ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % DLon           ( 1 : NObs )
     .      = This % DLon      ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % P              ( 1 : NObs )
     .      = This % P         ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % ETime          ( 1 : NObs )
     .      = This % ETime     ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % QC_file        ( 1 : NObs )
     .      = This % QC_file   ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % Obs            ( 1 : NObs )
     .      = This % Obs       ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % FG             ( 1 : NObs )
     .      = This % FG        ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % P_Missing      ( 1 : NObs )
     .      = This % P_Missing ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % ObMissing      ( 1 : NObs )
     .      = This % ObMissing ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % FGMissing      ( 1 : NObs )
     .      = This % FGMissing ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % P_m            ( 1 : NObs )
     .      = This % P_m       ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % P_e            ( 1 : NObs )
     .      = This % P_e       ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % kt             ( 1 : NObs )
     .      = This % kt        ((/( Indx ( iOb ), iOb = 1, NObs )/))
         This % QC             ( 1 : NObs )
     .      = This % QC        ((/( Indx ( iOb ), iOb = 1, NObs )/))
 
      else
         This % DLat           ( Indx ( 1 : NObs )) 
     .      = This % DLat      ((/(        iOb,   iOb = 1, NObs )/))
         This % DLon           ( Indx ( 1 : NObs )) 
     .      = This % DLon      ((/(        iOb,   iOb = 1, NObs )/))
         This % P              ( Indx ( 1 : NObs )) 
     .      = This % P         ((/(        iOb,   iOb = 1, NObs )/))
         This % ETime          ( Indx ( 1 : NObs ))
     .      = This % ETime     ((/(        iOb,   iOb = 1, NObs )/))
         This % QC_file        ( Indx ( 1 : NObs ))
     .      = This % QC_file   ((/(        iOb,   iOb = 1, NObs )/))
         This % Obs            ( Indx ( 1 : NObs ))
     .      = This % Obs       ((/(        iOb,   iOb = 1, NObs )/))
         This % P_Missing      ( Indx ( 1 : NObs ))
     .      = This % P_Missing ((/(        iOb,   iOb = 1, NObs )/))
         This % ObMissing      ( Indx ( 1 : NObs ))
     .      = This % ObMissing ((/(        iOb,   iOb = 1, NObs )/))
         This % P_m            ( Indx ( 1 : NObs ))
     .      = This % P_m       ((/(        iOb,   iOb = 1, NObs )/))
         This % P_e            ( Indx ( 1 : NObs ))
     .      = This % P_e       ((/(        iOb,   iOb = 1, NObs )/))
         This % kt             ( Indx ( 1 : NObs ))
     .      = This % kt        ((/(        iOb,   iOb = 1, NObs )/))
         This % QC             ( Indx ( 1 : NObs ))
     .      = This % QC        ((/(        iOb,   iOb = 1, NObs )/))

      end if

      return
      end subroutine Reorder_Vector2
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  IndexSet_All --- Initialize indices of radcor data structure
! 
! !INTERFACE:
      subroutine IndexSet_All ( This )
!
! !USES
      use m_RadSort, only : IndexSet
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profiles ),  intent (inout) ::
     .   This         ! radcor data structure
!
! !DESCRIPTION:
!
! !REVISION HISTORY: 
!     17Jul2003  C. Redder  Original code
!     12Nov2003  C. Redder  Replace data structures radcor_mass and
!                           radcor_wind with radcor_vector
!EOP
!-------------------------------------------------------------------------

      call IndexSet ( This % Vector % Indx )

      return
      end subroutine IndexSet_All
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  LevSort_All --- Sort radcor data structure
! 
! !INTERFACE:
      subroutine LevSort_All ( This, stat )
!
! !USES
      use m_AdvError, only : WPErr, ErrStat
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profiles ),  intent (inout) ::
     .   This         ! radcor data structure
!
! !OUTPUT PARAMETER:
      integer,                   intent   (out) ::
     .   stat         ! returned status code
!
! !DESCRIPTION:
!
! !REVISION HISTORY: 
!     17Jul2003  C. Redder  Original code
!     12Nov2003  C. Redder  Replace data structures radcor_mass and
!                           radcor_wind with radcor_vector
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter :: MyName = MyModule
     .                                     // '::LevSort_All'

      call LevSort_Vector ( This % Meta, This % Vector, stat )
      if ( stat .ne. 0 ) then
         call WPErr  ( MyName, ErrStat ( 'LevSort_Vector' ))
         return
      end if

      return
      end subroutine LevSort_All
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  LevSort_Vector --- Sort mass data structure by level
! 
! !INTERFACE:
      subroutine LevSort_Vector ( Meta, This, stat )
!
! !USES
      use m_AdvError, only : WPErr, ErrStat
      use m_RadSort,  only : R2ManExp
      implicit NONE
!
! !INPUT PARAMETERS:
      type ( radcor_meta   ),  intent (in)    ::
     .   Meta         ! radcor meta-data structure
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_vector ),  intent (inout) ::
     .   This         ! radcor mass data structure
!
! !OUTPUT PARAMETER:
      integer,               intent   (out) ::
     .   stat         ! returned status code
!
! !DESCRIPTION:
!
! !REVISION HISTORY: 
!     17Jul2003  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter :: MyName = MyModule
     .                                     // '::LevSort_Vector'

      call R2ManExp ( NPresDigits,
     .                This % Indx,
     .                This % P,
     .                This % P_m,
     .                This % P_e )
      call LevSort_ ( Meta % ksLoc,
     .                Meta % ksLen,
     .                This % Indx,
     .                This % P_m,
     .                This % P_e, stat )
      if ( stat .ne. 0 ) then
         call WPErr  ( MyName, ErrStat ( 'LevSort_' ))
         return
      end if

      return
      end subroutine LevSort_Vector
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  LevSort_ --- Sort each sounding by level
! 
! !INTERFACE:
      subroutine LevSort_ ( Loc, Len, Indx, Man, Exp, stat )
!
! !USES
      use m_AdvError, only : WPErr, ErrStat
      use m_RadSort,  only : IndexSort
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,  intent (in),    dimension (:) ::
     .   Loc,   ! Navigators that give the locations and ...
     .   Len,   ! ... lengths of all segments 
     .   Man,   ! Pressure levels expressed as mantessas
     .   Exp    ! ... and exponents
!
! !INPUT/OUTPUT PARAMETERS:
      integer,  intent (inout), dimension (:) ::
     .   Indx   ! Sorting indices
!
! !OUTPUT PARAMETER:
      integer,  intent   (out) ::
     .   stat   ! returned status code from allocation statement
!
! !DESCRIPTION:
!     This routine sorts by level the data for each sounding.  The data 
!     for each sounding is assumed to be contained within a segmemt.  The
!     levels are expressed as mantessa and exponents to eliminate
!     uncertainty due to round-off error.
!
! !REVISION HISTORY: 
!     17Jul2003  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------
      character (len=*), parameter :: MyName = MyModule
     .                                     // '::LevSort_'
      integer :: nks, iks, iBeg, iEnd, iOb, iiOb
      integer :: ThisMan, ThisExp, LastMan, LastExp
      logical :: sorted

!     Default status
!     --------------
      stat = 0

!     For each sounding ...
!     ---------------------
      nks = min ( size ( Loc ), size ( Len ))
      do iks = 1, nks

!        ... isolate the data in the structure
!        -------------------------------------
         iBeg    =  Loc   ( iks )
         iEnd    =  Len   ( iks ) + iBeg - 1
         if ( iEnd .ge. iBeg ) then
            iiOb    =  Indx  ( iBeg )
            LastMan =  Man   ( iiOb )
            LastExp =  Exp   ( iiOb )
         end if

!        ... determine if the data is already sorted
!        -------------------------------------------
         do iOb  =  iBeg + 1, iEnd
            iiOb =  Indx  (  iOb )
            ThisMan = Man ( iiOb )
            ThisExp = Exp ( iiOb )
            sorted  = ThisExp .le. LastExp
            if ( ThisExp .eq. LastExp )
     .         sorted = sorted .and. ThisMan .le. LastMan
            if ( .not. sorted ) exit
            LastMan = ThisMan
            LastExp = ThisExp
         end do

!        ... and if not, the sort the data by decending values of pressure
!        -----------------------------------------------------------------
         if ( .not. sorted ) then
            call IndexSort ( Indx ( iBeg : iEnd ), Man, stat,
     .                       descend = .true. )
            if ( stat .ne. 0 ) then
               call WPErr ( MyName, ErrStat ( 'IndexSort' )
     .                        // '\n   Key = Man ' )
               return
            end if
            call IndexSort ( Indx ( iBeg : iEnd ), Exp, stat,
     .                       descend = .true. )
            if ( stat .ne. 0 ) then
               call WPErr ( MyName, ErrStat ( 'IndexSort' )
     .                        // '\n   Key = Exp ' )
               return
            end if
         end if
      end do

      return
      end subroutine LevSort_

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Init_Profiles --- Allocates necessary memory and initializes variables
! 
! !INTERFACE:
      subroutine Init_Profiles ( nks,  NObs, kx_max_file, qcx_max_file,
     .                           this, stat )
!
! !USES
      use m_AdvError, only : WPErr, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS: 
      integer,                   intent (in)    ::
     .   nks,          ! Number of soundings
     .   NObs,         ! Number of observations
     .    kx_max_file, ! Data type index (kx)
     .   qcx_max_file  ! ... and QC marks defined for input files 
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profiles ),  intent (inout) ::
     .   this          ! Data structure
!
! !OUTPUT PARAMETER:
      integer,                   intent   (out) ::
     .   stat          ! returned status code from allocation statement
!
! !DESCRIPTION:
!     This routine allocates memory for the data structure, radcor_profiles.
!     This routine also initializes the data structure parameters.
!
! !REVISION HISTORY: 
!     24Apr2003  C. Redder  Original code
!     12Nov2003  C. Redder  Renamed Radcor_Init to Init_Profiles.  Changed
!                           data structure from radcor to radcor_profiles.
!                           Replaced required arguments, NMObs and NWObs
!                           with NObs.
!     17Feb2004  C. Redder  Added the reqired input arguments, kx_max_file
!                           and qcx_max_file.  Added the sub-data
!                           structure, radcor_lists, to the input data
!                           structure.
!     31Mar2004  C. Redder  Added the component, OptionSet.
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Init_Profiles'
      integer :: istat

      stat = 0

      call Init_Meta   ( nks,  this % Meta,   stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Init_Meta'   ))
         return
      end if

      call Init_Lists  ( kx_max_file,         qcx_max_file,
     .                         this % Lists,  stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Init_Lists'  ))
         call Clean_Meta  ( this % Meta  )
         return
      end if

      call Init_Vector ( NObs, this % Vector, stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Init_Vector' ))
         call Clean_Lists ( this % Lists )
         call Clean_Meta  ( this % Meta  )
         return
      end if

      this % FileType  = ' '
      this %  ObsFile  = ' '
      this % MetaFile  = ' '
      this % OptionSet = 0

!     All done
!     --------
      return

      end subroutine Init_Profiles

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Init_Meta --- Allocates necessary memory and initializes variables
! 
! !INTERFACE:
      subroutine Init_Meta ( nks, this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Alloc
      implicit NONE
!
! !INPUT PARAMETERS: 
      integer,              intent (in)    ::
     .   nks    ! Number of soundings
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_meta ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer,              intent   (out) ::
     .   stat   ! returned status code from allocation statement
!
! !DESCRIPTION:
!     This routine allocates memory for the auxiliary data structure,
!     radcor_meta.  This routine also initializes the data structure
!     parameters.
!
! !REVISION HISTORY: 
!     24Apr2003  C. Redder  Original code
!     26Jul2003  C. Redder  Set the component, nks, to 0
!     12Nov2003  C. Redder  Rename routine from Meta_Init to Init_Meta
!     11Feb2004  C. Redder  Added the components, kx and kx_file, to the
!                           data structure, radcor_meta.
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Init_Meta' 
      integer :: nvct, nnks

!     Define parameters for data structure
!     ------------------------------------
      nnks  = nks
      if ( nnks .lt. 0 ) nnks = nks_Def
      nvct  = max ( 1, nnks )     

!     ... store parameters for data structure
!     ---------------------------------------
      this % NVct = nvct
      this % nks  = 0
      this % Date = 0
      this % Time = 0

!     Allocate memory
!     ---------------
      allocate ( this % StnID    ( nvct ),
     .           this % Lat      ( nvct ),
     .           this % Lon      ( nvct ),
     .           this % Elev     ( nvct ),
     .           this % ObTime   ( nvct ),
     .           this %  LTime   ( nvct ),
     .           this %  LTHist  ( nvct ),
     .           this %  ETHist  ( nvct ),
     .           this %  kx      ( nvct ),
     .           this %  kx_file ( nvct ),
     .           this % rkx      ( nvct ),
     .           this % RadCode  ( nvct ),
     .           this % ksLoc    ( nvct ),
     .           this % ksLen    ( nvct ),
     .           stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName,
     .                Alloc ( stat, 'this%StnID,'
     .                           // 'this%Lat,'
     .                           // 'this%Lon,'
     .                           // 'this%Elev,'
     .                           // 'this%ObTime,'
     .                           // 'this%LTime,'
     .                           // 'this%LTHist,'
     .                           // 'this%ETHist,'
     .                           // 'this% kx,'
     .                           // 'this% kx_file,'
     .                           // 'this%rkx,'
     .                           // 'this%RadCode,'
     .                           // 'this%ksLoc,'
     .                           // 'this%ksLen', nvct ))
         return
      end if

!     All done
!     --------
      return

      end subroutine Init_Meta

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Init_Lists --- Allocates necessary memory and initializes variables
! 
! !INTERFACE:
      subroutine Init_Lists ( kx_max_file, qcx_max_file, this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Alloc, Array
      use m_RadLists, only : kt_names, kt_units, kx_names, rkx_names,
     .                       rcode_names, kx_RaobList, MetaStr_Raobs
      implicit NONE
!
! !INPUT PARAMETERS: 
      integer,               intent (in)    ::
     .    kx_max_file,     ! Maximum value for the data type index (kx)
     .   qcx_max_file      ! ... and quality control as defined for
!                          !   the input file
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_lists ), intent (inout) ::
     .   this              ! Data structure
!
! !OUTPUT PARAMETER:
      integer,               intent   (out) ::
     .   stat ! returned status code from allocation statement
!
! !DESCRIPTION:
!     This routine allocates memory for the auxiliary data structure,
!     radcor_lists.  This routine also initializes the data structure
!     parameters.
!
! !REVISION HISTORY: 
!     17Feb2004  C. Redder  Original code
!     20Nov2006  C. Redder  Added use statement to access module,
!                           m_RadLists for defining kt_names, kx_names,
!                           rkx_names, rcode_names, kx_RaobList and
!                           MetaStr_Raobs.
!     02Feb2007  C. Redder  Revised the routine so that all arrays are
!                           allocated in one command which otherwise
!                           would result in an execution error (on
!                           dirac in particular)
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName        = MyModule // '::Init_Lists' 
      integer, parameter ::
     .   nkt           = size (    kt_names ),
     .   nkx           = size (    kx_names ),
     .   nrkx          = size (   rkx_names ),
     .   nrcodes       = size ( rcode_names ), 
     .   kx_RaobListSz = size ( kx_RaobList )
      integer :: nkx_file, nqcx_file, istat, iList
      logical :: set_kx_file, set_qcx_file

!     Set dimensions 
!     --------------
      nkx_file     =  kx_max_file
      set_kx_file  =  nkx_file .lt. 0
      if (  set_kx_file )  nkx_file = nkx
      nkx_file     = max ( 1,  nkx_file )     

      nqcx_file    =  qcx_max_file
      set_qcx_file = nqcx_file .lt. 0
      if ( set_qcx_file ) nqcx_file = 1
      nqcx_file    = max ( 1, nqcx_file )

!     Store parameters for data structure
!     -----------------------------------
      this %  kt_max   =  nkt
      this %  kx_max   =  nkx
      this % rkx_max   = nrkx
      this % rcode_max = nrcodes
      this %  kx_max_file =  nkx_file
      this % qcx_max_file = nqcx_file

!     Allocate memory 
!     ---------------
      allocate ( this % kt_names       ( nkt  ),
     .           this % kt_units       ( nkt  ),
     .           this % kx_names       ( nkx  ),
     .           this % kx_meta        ( nkx  ),
     .           this % rkx_names      ( nrkx ),
     .           this % rcode_names    ( nrcodes ),
     .           this % kx_names_file  ( nkx_file ),
     .           this % kx_meta_file   ( nkx_file ),
     .           this % qcx_names_file ( nqcx_file ),
     .           stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, trim ( Alloc ( stat, 
     .                   Array ( 'this%kt_names,', nkt )
     .                // Array ( 'this%kt_units',  nkt )
     .                // Array ( 'this%kx_names',  nkx )
     .                // Array ( 'this%kx_meta',   nkx )
     .                // Array ( 'this%rkx_names', nrkx )
     .                // Array ( 'this%rcode_names',    nrcodes  )
     .                // Array ( 'this%kx_names_file',  nkx_file )
     .                // Array ( 'this%kx_meta_file',   nkx_file )
     .                // Array ( 'this%qcz_names_file', nkx_file ))))
         return
      end if

!     Initialize the lists
!     --------------------
      this % kt_names  ( : nkt  ) = kt_names  ( : nkt  )
      this % kt_units  ( : nkt  ) = kt_units  ( : nkt  )
      this % kx_names  ( : nkx  ) = kx_names  ( : nkx  )
      this % kx_meta   ( : nkx  ) = ' '
      do iList = 1, kx_RaobListSz 
         this %  kx_meta ( kx_RaobList ( iList )) = MetaStr_Raobs
      end do
      this % rkx_names ( : nrkx ) = rkx_names ( : nrkx )
      this % rcode_names       ( : nrcodes ) = rcode_names ( : nrcodes )
      if ( set_kx_file ) then
         this % kx_names_file ( : nkx )      = this % kx_names ( : nkx )
         this % kx_meta_file  ( : nkx )      = this % kx_meta  ( : nkx )
      else
         this % kx_names_file ( : nkx_file ) = ' '
         this % kx_meta_file  ( : nkx_file ) = ' '
      end if
      this % qcx_names_file   ( : nqcx_file) = ' '
 
!     All done
!     --------
      return

      end subroutine Init_Lists

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Init_Vector --- Allocates necessary memory and initializes variables
! 
! !INTERFACE:
      subroutine Init_Vector ( NObs, this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Alloc
      implicit NONE
!
! !INPUT PARAMETERS: 
      integer,                intent (in)    ::
     .   NObs   ! Number of observations
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_vector ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer,                intent   (out) ::
     .   stat   ! returned status code from allocation statement
!
! !DESCRIPTION:
!     This routine allocates memory for the auxiliary data structure,
!     radcor_vector.  This routine also initializes the data structure
!     parameters.
!
! !REVISION HISTORY: 
!     24Apr2003  C. Redder  Original code
!     16Jul2003  C. Redder  Added the components, Indx, LevM and LevE
!     26Jul2003  C. Redder  Set the component, NObs, to 0
!     12Nov2003  C. Redder  Renamed Mass_Init to Init_Vector.  Changed data
!                           structure from radcor_mass to radcor_vector.
!     01Dec2003  C. Redder  Added the component, QM
!     28Jan2004  C. Redder  Added the components for drift lat/lon
!     03Feb2004  C. Redder  Renamed the components, Lev, LevM and LevE to
!                           P, P_m and P_e in the data structure,
!                           radcor_vector.
!     17Feb2004  C. Redder  Renamed the component, QM to QC_file
!     21Apr2004  C. Redder  Added the components, P_Missing and ObMissing
!     02Jun2004  C. Redder  Added the component, FG and FGMissing
!
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Mass_Init' 
      integer :: nvct, nnobs

!     Set and ...
!     -----------
      nnobs = NObs
      if ( nnobs .lt. 0 ) nnobs = nobs_Def
      nvct  = max ( 1, nnobs )

!     ... store parameters for data structure
!     ---------------------------------------
      this % NVct = nvct
      this % NObs = 0

!     Allocate memory
!     ---------------
      allocate ( this % DLat      ( nvct ),
     .           this % DLon      ( nvct ),
     .           this % P         ( nvct ),
     .           this % ETime     ( nvct ),
     .           this % QC_file   ( nvct ),
     .           this % Obs       ( nvct ),
     .           this % FG        ( nvct ),
     .           this % P_Missing ( nvct ),
     .           this % ObMissing ( nvct ),
     .           this % FGMissing ( nvct ),
     .           this % Indx      ( nvct ),
     .           this % P_m       ( nvct ),
     .           this % P_e       ( nvct ),
     .           this % kt        ( nvct ),
     .           this % QC        ( nvct ),
     .           stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName,
     .                Alloc ( stat, 'this%DLat,'
     .                           // 'this%DLon,'
     .                           // 'this%P,'
     .                           // 'this%ETime,'
     .                           // 'this%QC_file,'
     .                           // 'this%Obs,'
     .                           // 'this%FG,'
     .                           // 'this%P_Missing,'
     .                           // 'this%ObMissing,'
     .                           // 'this%FGMissing,'
     .                           // 'this%Indx,'
     .                           // 'this%P_m,'
     .                           // 'this%P_e,'
     .                           // 'this%kt,'
     .                           // 'this%QC', nvct ))
         return
      end if

!     All done
!     --------
      return

      end subroutine Init_Vector

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Init_Profile --- Allocates necessary memory and initializes variables
! 
! !INTERFACE:
      subroutine Init_Profile ( NLev_Max, NObs_Max, this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS: 
      integer,                 intent (in)    ::
     .   NLev_Max,  ! Maximum number of levels
     .   NObs_Max   ! ... and observations in a profile
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profile ), intent (inout) ::
     .   this       ! Data structure
!
! !OUTPUT PARAMETER:
      integer,                 intent   (out) ::
     .   stat       ! returned status code from allocation statement
!
! !DESCRIPTION:
!     This routine allocates memory for the auxiliary data structure,
!     radcor_profile.  This routine also initializes the data structure
!     parameters.
!
! !REVISION HISTORY: 
!     15Oct2003  C. Redder  Original code
!     12Nov2003  C. Redder  Renamed Profile_Init to Init_Profile.
!     28Jan2004  C. Redder  Added the component, StnID, to the data 
!                           structure, radcor_profile.
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Profile_Init' 
      integer :: istat

      stat = 0

      call Init_by_Vector ( NObs_Max, this % by_Vector, stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Init_by_Vector' ))
         return
      end if

      call Init_by_Levels ( NLev_Max, this % by_Levels, stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Init_by_Levels' ))
         call Clean_by_Vector ( this % by_Vector )
         return
      end if

      this % StnID   = ' '
      this % Date    = 0
      this % Time    = 0
      this % LTime   = 0
      this % rkx     = 0
      this % RadCode = 0
      this % iks     = 0
      this % ksLoc   = 0
      this % ksLen   = 0
      this % ktTemp  = 0
      this % ktHum   = 0
      this % Lat     = 0.0
      this % Lon     = 0.0
      this % Elev    = 0.0

      return
      end subroutine Init_Profile

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Init_by_Vector --- Allocates necessary memory for the data structure profile_by_vector.
! 
! !INTERFACE:
      subroutine Init_by_Vector ( NObs_Max, this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Alloc
      implicit NONE
!
! !INPUT PARAMETERS: 
      integer,                    intent (in)    ::
     .   NObs_Max ! Maximum number of observations
!
! !INPUT/OUTPUT PARAMETERS:
      type ( profile_by_vector ), intent (inout) ::
     .   this     ! Data structure
!
! !OUTPUT PARAMETER:
      integer,                    intent   (out) ::
     .   stat     ! returned status code from allocation statement
!
! !DESCRIPTION:
!     This routine allocates memory for the auxiliary data structure,
!     profile_by_vector.  This routine also initializes the data
!     structure parameters.
!
! !REVISION HISTORY: 
!     15Oct2003  C. Redder  Original code
!     12Nov2003  C. Redder  Renamed routine from Init_by_MStream to
!                           Init_by_Vector.  Replace the data structure,
!                           radcor_by_stream with radcor_by_vector.
!     28Jan2004  C. Redder  Added the components for drift lat/lon
!     08Jul2004  C. Redder  Added the component, ObMissing.
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Init_by_Vector' 
      integer :: nvct, NPObs

!     Set and ...
!     -----------
      NPObs = NObs_Max
      if ( NPObs .lt. 0 ) NPObs = nkt_Def * NLev_Def
      nvct = max ( 1, NPObs )

!     ... store parameters for data structure
!     ---------------------------------------
      this % NVct = nvct
      this % NObs = 0

!     Allocate memory
!     ---------------
      allocate ( this % LevIndx   ( nvct ),
     .           this % kt        ( nvct ),
     .           this % QC        ( nvct ),
     .           this % Mask      ( nvct ),
     .           this % ObMissing ( nvct ),
     .           this % DLat      ( nvct ),
     .           this % DLon      ( nvct ),
     .           this % P         ( nvct ),
     .           this % logP      ( nvct ),
     .           this % ETime     ( nvct ),
     .           this % Obs       ( nvct ),
     .           this % T         ( nvct ),
     .           stat = stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName,
     .                Alloc ( stat, 'this%LevIndx,'
     .                           // 'this%kt,'
     .                           // 'this%QC,'
     .                           // 'this%Mask,'
     .                           // 'this%ObMissing,'
     .                           // 'this%DLat,'
     .                           // 'this%DLon,'
     .                           // 'this%P,'
     .                           // 'this%logP,'
     .                           // 'this%ETime,'
     .                           // 'this%Obs,'
     .                           // 'this%T', nvct ))
         return
      end if

!     All done
!     --------
      return

      end subroutine Init_by_Vector

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Init_by_Levels --- Allocates necessary memory for the data structure profile_by_levels
! 
! !INTERFACE:
      subroutine Init_by_Levels ( NLev_Max, this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Alloc
      implicit NONE
!
! !INPUT PARAMETERS: 
      integer,                    intent (in)  ::
     .   NLev_Max ! Maximum number of levels
!
! !INPUT/OUTPUT PARAMETERS:
      type ( profile_by_levels ), intent (inout) ::
     .   this     ! Data structure
!
! !OUTPUT PARAMETER:
      integer,                    intent   (out) ::
     .   stat     ! returned status code from allocation statement
!
! !DESCRIPTION:
!     This routine allocates memory for the auxiliary data structure,
!     profile_by_levels.  This routine also initializes the
!     data structure parameters.
!
! !REVISION HISTORY: 
!     15Oct2003  C. Redder  Original code
!     07Nov2003  C. Redder  Renamed routine from Init_by_MLevels to
!                           Init_by_Levels.  Data type name changed from
!                           profile_by_mass_levels to profile_by_all_levels.
!     09Aug2004  C. Redder  Added the componenet, LRate, to the data 
!                           structure.
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Init_by_Levels' 
      integer :: nvct, MNLev

!     Set and ...
!     -----------
      MNLev = NLev_Max
      if ( MNLev .lt. 0 ) MNLev = NLev_Def
      nvct  = max ( 1, MNLev )     

!     ... store parameters for data structure
!     ---------------------------------------
      this % NVct = nvct
      this % NLev = 0

!     Allocate memory
!     ---------------
      allocate ( this % StrList ( nvct ),
     .           this % Mask    ( nvct ),
     .           this % DLat    ( nvct ),
     .           this % DLon    ( nvct ),
     .           this % P       ( nvct ),
     .           this % logP    ( nvct ),
     .           this % ETime   ( nvct ),
     .           this % RRate   ( nvct ),
     .           this % Z       ( nvct ),
     .           this % LRate   ( nvct ),
     .           this % T       ( nvct ),
     .           this % TV      ( nvct ),
     .           this % TV2     ( nvct ),
     .           this % RH      ( nvct ),
     .           this % MR      ( nvct ),
     .           this % U       ( nvct ),
     .           this % V       ( nvct ),
     .           this % SE      ( nvct ),
     .           this % TCor    ( nvct ),
     .           this % TCorAcc ( nvct ),
     .           this % ZCor    ( nvct ),
     .           stat = stat )

      if ( stat .ne. 0 ) then
         call WPErr ( MyName,
     .                Alloc ( stat, 'this%StrList,'
     .                           // 'this%Mask,'
     .                           // 'this%DLat,'
     .                           // 'this%DLon,'
     .                           // 'this%P,'
     .                           // 'this%logP,'
     .                           // 'this%ETime,'
     .                           // 'this%RRate,'
     .                           // 'this%Z,'
     .                           // 'this%T,'
     .                           // 'this%LRate,'
     .                           // 'this%TV,'
     .                           // 'this%TV2,'
     .                           // 'this%RH,'
     .                           // 'this%MR,'
     .                           // 'this%U,'
     .                           // 'this%V,'
     .                           // 'this%SE,'
     .                           // 'this%TCor,'
     .                           // 'this%TCorAcc,'
     .                           // 'this%ZCor', nvct ))
         return
      end if

!     All done
!     --------
      return

      end subroutine Init_by_Levels

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Clean_Profiles --- Deallocate memory
! 
! !INTERFACE:
      subroutine Clean_Profiles ( this, stat )
!
! !USES
      use m_AdvError, only : PErr, Dealloc, ErrStat
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profiles ),  intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer, optional,         intent  (out) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status code from the
                !   deallocate statements are ignored
!
! !DESCRIPTION:
!     This routine deallocates memory for the data structure of type
!     radcor_profiles.
!
! !REVISION HISTORY: 
!     24Apr2003  C. Redder  Original code
!     12Nov2003  C. Redder  Renamed routine from Radcor_Clean to
!                           Clean_Profiles.  Replaced the data structure
!                           radcor with radcor_profiles.
!     17Feb2004  C. Redder  Added the sub-data structure, radcor_lists,
!                           to the input data stucture, radcor_profiles.
!     31Mar2004  C. Redder  Added the component, OptionSet.
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Profiles'
      logical :: check_stat

!     Clear parameters
!     ----------------
      this % FileType  = ' '
      this %  ObsFile  = ' '
      this % MetaFile  = ' '
      this % OptionSet = 0

      check_stat = present ( stat )

!     Deallocate
!     ----------
      call Clean_Meta   ( this % Meta,   stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName, ErrStat ( 'Clean_Meta'   ) // '\W' )
            return
         end if
      end if

      call Clean_Lists  ( this % Lists,  stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName, ErrStat ( 'Clean_Lists'  ) // '\W' )
            return
         end if
      end if

      call Clean_Vector ( this % Vector, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName, ErrStat ( 'Clean_Vector' ) // '\W' )
            return
         end if
      end if

!     All done
!     --------
      return

      end subroutine Clean_Profiles

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Clean_Meta --- Deallocate memory
! 
! !INTERFACE:
      subroutine Clean_Meta ( this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_meta ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer, optional,    intent   (out) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status codes from the 
                !   deallocate statements are ignored
!
! !DESCRIPTION:
!     This routine deallocates memory for the data structure of type
!     radcor_meta
!
! !REVISION HISTORY: 
!     24Apr2003  C. Redder  Original code
!     08Oct2003  C. Redder  Combined all deallocation statements into one.
!     12Nov2003  C. Redder  Renamed routine from Meta_Clean to Clean_Meta.
!     11Feb2004  C. Redder  Added the components, kx and kx_file, to the
!                           data structure, radcor_meta.
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Meta'
      logical :: check_stat
      integer :: istat

!     Clear parameters
!     ----------------
      this % NVct = 0
      this % nks  = 0
      this % Date = 0
      this % Time = 0

!     Initialize status options and codes
!     -----------------------------------
      check_stat    = present ( stat )
      if ( check_stat ) stat = No_Error

!     Deallocate memory
!     -----------------
      deallocate ( this % StnID,
     .             this % Lat,
     .             this % Lon,
     .             this % Elev,
     .             this % ObTime,
     .             this %  LTime,
     .             this % LTHist,
     .             this % ETHist,
     .             this %  kx,
     .             this %  kx_file,
     .             this % rkx,
     .             this % RadCode,
     .             this % ksLoc,
     .             this % ksLen,
     .             stat = istat )
      if ( istat .ne. No_Error .and.  check_stat ) then
         call WPErr ( MyName,
     .                Dealloc ( stat, 'this%StnID,'
     .                             // 'this%Lat,'
     .                             // 'this%Lon,'
     .                             // 'this%Elev,'
     .                             // 'this%ObTime,'
     .                             // 'this%LTime,'
     .                             // 'this%LTHist,'
     .                             // 'this%ETHist,'
     .                             // 'this%kx,'
     .                             // 'this%kx_file,'
     .                             // 'this%rkx,'
     .                             // 'this%RadCode,'
     .                             // 'this%ksLoc,'
     .                             // 'this%ksLen' ))
         stat = istat
         return
      end if

!     Reinitialize pointers
!     ---------------------
      call Nullify_Meta ( this )

!     All done
!     --------
      return

      end subroutine Clean_Meta

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Clean_Lists --- Deallocate memory
! 
! !INTERFACE:
      subroutine Clean_Lists ( this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_lists ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer, optional,     intent   (out) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status codes from the 
                !   deallocate statements are ignored
!
! !DESCRIPTION:
!     This routine deallocates memory for the data structure of type
!     radcor_lists
!
! !REVISION HISTORY: 
!     17Feb2004  C. Redder  Original code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Lists'
      logical :: check_stat
      integer :: istat

!     Clear parameters
!     ----------------
      this %  kt_max = 0
      this %  kx_max = 0
      this % rkx_max = 0
      this % rcode_max    = 0
      this %  kx_max_file = 0
      this % qcx_max_file = 0

!     Initialize status options and codes
!     -----------------------------------
      check_stat    = present ( stat )
      if ( check_stat ) stat = No_Error

!     Deallocate memory
!     -----------------
      deallocate ( this %  kt_names,
     .             this %  kt_units,
     .             this %  kx_names,
     .             this %  kx_meta,
     .             this % rkx_names,
     .             this % rcode_names,
     .             this %  kx_names_file,
     .             this %  kx_meta_file,
     .             this % qcx_names_file,
     .             stat = istat )
      if ( istat .ne. No_Error .and.  check_stat ) then
         call WPErr ( MyName,
     .                Dealloc ( stat, 'this%kt_names,'
     .                             // 'this%kt_units,'
     .                             // 'this%kx_names,'
     .                             // 'this%kx_meta,'
     .                             // 'this%rkx_names,'
     .                             // 'this%rcode_names,'
     .                             // 'this%kx_names_file,'
     .                             // 'this%kx_meta_file,'
     .                             // 'this%qcx_names_file' ))
         stat = istat
         return
      end if

!     Reinitialize pointers
!     ---------------------
      call Nullify_Lists ( this )

!     All done
!     --------
      return

      end subroutine Clean_Lists

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Clean_Vector --- Deallocate memory
! 
! !INTERFACE:
      subroutine Clean_Vector ( this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_vector ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer, optional,      intent   (out) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status code from the
                !   deallocate statements are ignored
!
! !DESCRIPTION:
!     This routine deallocates memory for the data structure of type
!     radcor_vector.
!
! !REVISION HISTORY: 
!     24Apr2003  C. Redder  Original code
!     09Jul2003  C. Redder  Fixed bug by initializing istat to 0
!     16Jul2003  C. Redder  Added the components, Indx, LevM and LevE
!     08Oct2003  C. Redder  Combined all deallocation statements into one.
!     12Nov2003  C. Redder  Renamed routine from Mass_Clean to
!                           Clean_Vector.  Replaced the data structure
!                           radcor_mass with radcor_vector.
!     01Dec2003  C. Redder  Added the component, QM
!     28Jan2004  C. Redder  Added the components for drift lat/lon
!     03Feb2004  C. Redder  Renamed the components, Lev, LevM and LevE to
!                           P, P_m and P_e in the data structure,
!                           radcor_vector.
!     17Feb2004  C. Redder  Renamed the component, QM to QC_file
!     21Apr2004  C. Redder  Added the components, P_Missing and ObMissing
!     02Jun2004  C. Redder  Added the component, FG and FGMissing
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Vector'
      logical :: check_stat
      integer :: istat

!     Clear parameters
!     ----------------
      this % NVct = 0
      this % NObs = 0

!     Initialize status options and codes
!     -----------------------------------
      check_stat    = present ( stat )
      if ( check_stat ) stat = No_Error

!     Deallocate memory
!     -----------------
      deallocate ( this % DLat,
     .             this % DLon,
     .             this % P,
     .             this % ETime,
     .             this % QC_file,
     .             this % Obs,
     .             this % FG,
     .             this % P_Missing,
     .             this % ObMissing,
     .             this % FGMissing,
     .             this % Indx,
     .             this % P_m,
     .             this % P_e,
     .             this % kt,
     .             this % QC,
     .             stat = istat )
      if ( istat .ne. No_Error .and.  check_stat ) then
         call WPErr ( MyName,
     .                Dealloc ( stat, 'this%DLat,'
     .                             // 'this%DLon,'
     .                             // 'this%P,'
     .                             // 'this%ETime,'
     .                             // 'this%QC_file,'
     .                             // 'this%Obs,'
     .                             // 'this%FG,'
     .                             // 'this%P_Missing,'
     .                             // 'this%ObMissing,'
     .                             // 'this%FGMissing,'
     .                             // 'this%Indx,'
     .                             // 'this%P_m,'
     .                             // 'this%P_e,'
     .                             // 'this%kt,'
     .                             // 'this%QC' ))
         stat = istat
         return
      end if

!     Reinitialize pointers
!     ---------------------
      call Nullify_Vector ( this )

!     All done
!     --------
      return

      end subroutine Clean_Vector

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Clean_Profile --- Deallocate memory
! 
! !INTERFACE:
      subroutine Clean_Profile ( this, stat )
!
! !USES
      use m_AdvError, only : PErr, Dealloc, ErrStat
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profile ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer, optional,       intent   (out) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status code from the
                !   deallocate statements are ignored
!
! !DESCRIPTION:
!     This routine deallocates memory for the data structure of type
!     radcor_profile.
!
! !REVISION HISTORY: 
!     12Nov2003  C. Redder  Original code
!     28Jan2004  C. Redder  Added the component, StnID, to the data 
!                           structure, radcor_profile.
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Profile'
      logical :: check_stat

!     Clear parameters
!     ----------------
      this % StnID   = ' '
      this % Date    = 0
      this % Time    = 0
      this % LTime   = 0
      this % rkx     = 0
      this % RadCode = 0
      this % iks     = 0
      this % ksLoc   = 0
      this % ksLen   = 0
      this % ktTemp  = 0
      this % ktHum   = 0
      this % Lat     = 0.0
      this % Lon     = 0.0
      this % Elev    = 0.0 

      check_stat = present ( stat )

!     Deallocate
!     ----------
      call Clean_by_Vector ( this % by_Vector, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName, ErrStat ( 'Clean_by_Vector' ) // '\W' )
            return
         end if
      end if

      call Clean_by_Levels  ( this % by_Levels, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName, ErrStat ( 'Clean_by_Levels'  ) // '\W' )
            return
         end if
      end if

      return
      end subroutine Clean_Profile

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Clean_by_Vector --- Deallocate memory
! 
! !INTERFACE:
      subroutine Clean_by_Vector ( this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( profile_by_vector ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer, optional,          intent   (out) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status code from the
                !   deallocate statements are ignored
!
! !DESCRIPTION:
!     This routine deallocates memory for the data structure of type
!     profile_by_vector.
!
! !REVISION HISTORY: 
!     12Nov2003  C. Redder  Original code
!     28Jan2004  C. Redder  Added the components for drift lat/lon
!     08Jul2004  C. Redder  Added the component, ObMissing.
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_by_Vector'
      logical :: check_stat
      integer :: istat

!     Clear parameters
!     ----------------
      this % NVct    = 0
      this % NObs    = 0

!     Initialize status options and codes
!     -----------------------------------
      check_stat  = present ( stat )
      if ( check_stat ) stat = No_Error

!     Deallocate memory
!     -----------------
      deallocate ( this % LevIndx,
     .             this % kt,
     .             this % QC,
     .             this % Mask,
     .             this % ObMissing,
     .             this % DLat,
     .             this % DLon,
     .             this % P,
     .             this % logP,
     .             this % ETime,
     .             this % Obs,
     .             this % T,
     .             stat = istat )
      if ( istat .ne. No_Error .and.  check_stat ) then
         call WPErr ( MyName,
     .                Dealloc ( stat, 'this%LevIndx,'
     .                             // 'this%kt,'
     .                             // 'this%QC,'
     .                             // 'this%Mask,'
     .                             // 'this%ObMissing,'
     .                             // 'this%DLat,'
     .                             // 'this%DLon,'
     .                             // 'this%P,'
     .                             // 'this%logP,'
     .                             // 'this%ETime,'
     .                             // 'this%Obs,'
     .                             // 'this%T' ))
         stat = istat
         return
      end if

!     Reinitialize pointers
!     ---------------------
      call Nullify_by_Vector ( this )

!     All done
!     --------
      return

      end subroutine Clean_by_Vector

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Clean_by_Levels --- Deallocate memory
! 
! !INTERFACE:
      subroutine Clean_by_Levels ( this, stat )
!
! !USES
      use m_AdvError, only : WPErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( profile_by_levels ), intent (inout) ::
     .   this   ! Data structure
!
! !OUTPUT PARAMETER:
      integer, optional,          intent   (out) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status code from the
                !   deallocate statements are ignored
!
! !DESCRIPTION:
!     This routine deallocates memory for the data structure of type
!     profile_by_levels.
!
! !REVISION HISTORY: 
!     07Nov2003  C. Redder  Original code
!     09Aug2004  C. Redder  Added the componenet, LRate, to the data 
!                           structure.
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_by_Levels'
      logical :: check_stat
      integer :: istat

!     Clear parameters
!     ----------------
      this % NVct    = 0
      this % NLev    = 0

!     Initialize status options and codes
!     -----------------------------------
      check_stat  = present ( stat )
      if ( check_stat ) stat = No_Error

!     Deallocate memory
!     -----------------
      deallocate ( this % StrList,
     .             this % Mask,
     .             this % DLat,
     .             this % DLon,
     .             this % P,
     .             this % logP,
     .             this % ETime,
     .             this % RRate,
     .             this % Z,
     .             this % T,
     .             this % LRate,
     .             this % TV,
     .             this % TV2,
     .             this % RH,
     .             this % MR,
     .             this % U,
     .             this % V,
     .             this % SE,
     .             this % TCor,
     .             this % TCorAcc,
     .             this % ZCor,
     .             stat = istat )
      if ( istat .ne. No_Error .and.  check_stat ) then
         call WPErr ( MyName,
     .                Dealloc ( stat, 'this%StrList,'
     .                             // 'this%Mask,'
     .                             // 'this%DLat,'
     .                             // 'this%DLon,'
     .                             // 'this%P,'
     .                             // 'this%logP,'
     .                             // 'this%ETime,'
     .                             // 'this%RRate,'
     .                             // 'this%Z,'
     .                             // 'this%T,'
     .                             // 'this%LRate,'
     .                             // 'this%TV,'
     .                             // 'this%TV2,'
     .                             // 'this%RH,'
     .                             // 'this%MR,'
     .                             // 'this%U,'
     .                             // 'this%V,'
     .                             // 'this%SE,'
     .                             // 'this%TCor,'
     .                             // 'this%TCorAcc,'
     .                             // 'this%ZCor' ))
         stat = istat
         return
      end if

!     Reinitialize pointers
!     ---------------------
      call Nullify_by_Levels ( this )

!     All done
!     --------
      return

      end subroutine Clean_by_Levels

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Profiles - Initializes pointers in the input data structure
!
! !INTERFACE:
      subroutine Nullify_Profiles ( this )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profiles ), intent ( inout ) ::
     .   this  ! data structure to be nullified
!
! !REVISION HISTORY:
!     12Nov2003  C. Redder  Initial code
!     17Feb2004  C. Redder  Added the sub-data structure, radcor_lists,
!                           to the input data stucture, radcor_profiles.
!EOP
!.................................................................

      call Nullify_Meta   ( this % Meta   )
      call Nullify_Lists  ( this % Lists  )
      call Nullify_Vector ( this % Vector )

      return
      end subroutine Nullify_Profiles

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Meta - Initializes pointers
!
! !INTERFACE:
      subroutine Nullify_Meta ( this )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_meta ), intent ( inout ) ::
     .   this  ! data structure to be nullified
!
! !REVISION HISTORY:
!     24Apr2003  C. Redder  Initial code
!     11Feb2004  C. Redder  Added the components, kx and kx_file, to the
!                           data structure, radcor_meta.
!     26Jan2007  C. Redder  Added the component, put_prof, to the data
!                           structure, radcor_meta.
!EOP
!.................................................................

      nullify ( this % StnID,
     .          this % Lon,
     .          this % Lat,
     .          this % Elev,
     .          this % ObTime,
     .          this %  LTime,
     .          this %  LTHist,
     .          this %  ETHist,
     .          this %  kx,
     .          this %  kx_file,
     .          this % rkx,
     .          this % RadCode,
     .          this % ksLoc,
     .          this % ksLen )

      return
      end subroutine Nullify_Meta

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Lists - Initializes pointers
!
! !INTERFACE:
      subroutine Nullify_Lists ( this )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_lists ), intent ( inout ) ::
     .   this  ! data structure to be nullified
!
! !REVISION HISTORY:
!     17Feb2004  C. Redder  Initial code
!EOP
!.................................................................

      nullify ( this %  kt_names,
     .          this %  kt_units,
     .          this %  kx_names,
     .          this %  kx_meta,
     .          this % rkx_names,
     .          this % rcode_names,
     .          this %  kx_names_file,
     .          this %  kx_meta_file,
     .          this % qcx_names_file )

      return
      end subroutine Nullify_Lists

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Vector - Initializes pointers
!
! !INTERFACE:
      subroutine Nullify_Vector ( this )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_vector ), intent ( inout ) ::
     .   this  ! data structure to be nullified
!
! !REVISION HISTORY:
!     24Apr2003  C. Redder  Initial code
!     16Jul2003  C. Redder  Added the components, Indx, LevM and LevE
!     12Nov2003  C. Redder  Renamed routine from nullify_mass to
!                           nullify_vector.  Replaced data structure,
!                           radcor_mass with radcor_vector
!     01Dec2003  C. Redder  Added the component, QM
!     28Jan2004  C. Redder  Added the components for drift lat/lon
!     03Feb2004  C. Redder  Renamed the components, Lev, LevM and LevE to
!                           P, P_m and P_e in the data structure,
!                           radcor_vector.
!     17Feb2004  C. Redder  Renamed the component, QM to QC_file
!     21Apr2004  C. Redder  Added the components, P_Missing and ObMissing
!     02Jun2004  C. Redder  Added the component, FG and FGMissing
!EOP
!.................................................................

      nullify ( this % DLat,
     .          this % DLon,
     .          this % P,
     .          this % ETime,
     .          this % QC_file,
     .          this % Obs,
     .          this % FG,
     .          this % P_Missing,
     .          this % ObMissing,
     .          this % FGMissing,
     .          this % Indx,
     .          this % P_m,
     .          this % P_e,
     .          this % kt,
     .          this % QC )

      return
      end subroutine Nullify_Vector

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Profile - Initializes pointers
!
! !INTERFACE:
      subroutine Nullify_Profile ( this )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profile ), intent ( inout ) ::
     .   this  ! data structure to be nullified
!
! !REVISION HISTORY:
!     12Nov2003  C. Redder  Initial code
!EOP
!.................................................................

      call Nullify_by_Vector ( this % by_Vector )
      call Nullify_by_Levels ( this % by_Levels )

      return
      end subroutine Nullify_Profile

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_by_Vector - Initializes pointers
!
! !INTERFACE:
      subroutine Nullify_by_Vector ( this )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( profile_by_vector ), intent ( inout ) ::
     .   this  ! data structure to be nullified
!
! !REVISION HISTORY:
!     12Nov2003  C. Redder  Initial code
!     08Jul2004  C. Redder  Added the component, ObMissing.
!EOP
!.................................................................

      nullify ( this % LevIndx,
     .          this % kt,
     .          this % QC,
     .          this % Mask,
     .          this % ObMissing,
     .          this % DLat,
     .          this % DLon,
     .          this % P,
     .          this % logP,
     .          this % ETime,
     .          this % Obs,
     .          this % T )

      return
      end subroutine Nullify_by_Vector

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_by_Levels - Initializes pointers
!
! !INTERFACE:
      subroutine Nullify_by_Levels ( this )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( profile_by_levels ), intent ( inout ) ::
     .   this  ! data structure to be nullified
!
! !REVISION HISTORY:
!     07Nov2003  C. Redder  Initial code
!     09Aug2004  C. Redder  Added the componenet, LRate, to the data 
!                           structure.
!EOP
!.................................................................

      nullify ( this % StrList,
     .          this % Mask,
     .          this % DLat,
     .          this % DLon,
     .          this % P,
     .          this % logP,
     .          this % ETime,
     .          this % RRate,
     .          this % Z,
     .          this % T,
     .          this % LRate,
     .          this % TV,
     .          this % TV2,
     .          this % RH,
     .          this % MR,
     .          this % U,
     .          this % V,
     .          this % SE,
     .          this % TCor,
     .          this % TCorAcc,
     .          this % ZCor )

      return
      end subroutine Nullify_by_Levels
!....................................................................

      end module m_RadData
!====================================================================
