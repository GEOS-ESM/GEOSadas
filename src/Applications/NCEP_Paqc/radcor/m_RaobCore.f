!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_RaobCore -- Utility routines for reading RaobCore data set.
!
! !DESCRIPTION:
!     This module contains the utility routines for reading the RaobCore
!     data set.  For more information about the dataset go to
!
!         http://www.univie.ac.at/theoret-met/research/raobcore/
!
! !INTERFACE:
!
      module   m_RaobCore
      use      m_SunAlt,  only : CDS_Julian => Julian
      use      m_RadData, only : NPDigits   => NPresDigits
      implicit NONE
      private	            ! except
      public :: 
     .   RaobCore_Header,   ! Derived type containing the header info
     .   RaobCore_StnInfo,  ! ... the station information
     .   RaobCore_StnNav,   ! ... with the corresponding navigators
     .   RaobCore_Breaks,   ! ... the adjustment breaks
     .   RaobCore_SynAdj,   ! ... the adjustments for a synoptic date/time
     .   RaobCore_StnAdj,   ! ... the adjustments for stations and a synoptic date
     .   RaobCore_StnAdj1,  ! ... the adjustments for a station and synoptic date
     .   RaobCore_FData,    ! ... all data from a data file
     .   RaobCore_SynDAdj   ! ... all adjustments for a synoptic date

      public ::
     .   RC_Get,            ! Read RaobCore data from a given file
     .   RC_Nullify,        ! Nullify ...
     .   RC_Clean,          ! ... reset and deallocate ...
     .   RC_Init,           ! ... initialize and allocate the data structure
     .   RC_StnAdj,         ! Get adjustments for stations and a synoptic date
     .   RC_StnAdj1         ! ... for a station and synoptic date

      interface RC_Nullify
         module procedure
     .      Nullify_Header, ! ... for header info 
     .      Nullify_StnInfo,! ... for station info
     .      Nullify_StnNav, ! ... for station navigators
     .      Nullify_Breaks, ! ... for adjustment breaks
     .      Nullify_SynAdj, ! ... for adjustments 
     .      Nullify_StnAdj, ! ... for adjustments for stations and a synoptic date
     .      Nullify_StnAdj1,! ... for adjustments for a station and synoptic date 
     .      Nullify_FData,  ! ... for file data
     .      Nullify_SynDAdj ! ... for all adjustments
      end interface
      interface RC_Clean
         module procedure
     .      Clean_Header,   ! ... for header info 
     .      Clean_StnInfo,  ! ... for station info
     .      Clean_StnNav,   ! ... for station navigators
     .      Clean_Breaks,   ! ... for adjustment breaks
     .      Clean_SynAdj,   ! ... for adjustments 
     .      Clean_StnAdj,   ! ... for adjustments for stations and a synoptic date
     .      Clean_StnAdj1,  ! ... for adjustments for a station and synoptic date
     .      Clean_FData,    ! ... for file data
     .      Clean_SynDAdj   ! ... for all adjustments
      end interface
      interface RC_Init
         module procedure
     .      Init_Header,    ! ... for header info 
     .      Init_StnInfo,   ! ... for station info
     .      Init_StnNav,    ! ... for station navigators
     .      Init_Breaks,    ! ... for adjustment breaks
     .      Init_SynAdj,    ! ... for adjustments 
     .      Init_StnAdj,    ! ... for adjustments for stations and a synoptic date
     .      Init_StnAdj1,   ! ... for adjustments for a station and synoptic date
     .      Init_FData,     ! ... for file data
     .      Init_SynDAdj    ! ... for all adjustments
      end interface
      interface RC_Get
         module procedure
     .      GetRC_FData,    ! ... for the entire data structure
     .      GetRC_SynDAdj   ! ... for a given synoptic date
      end interface
      interface RC_StnAdj
         module procedure
     .      Get_StnAdj_w1,
     .      Get_StnAdj
      end interface
      interface RC_StnAdj1
         module procedure
     .      Get_StnAdj1
      end interface
!
! !REVISION HISTORY:
!     08Mar2007  C. Redder  Original code
!EOP
!-----------------------------------------------------------------

!     Special characters
!     ------------------
      character (  len  =   1 ), parameter ::
     .   BLK  =  achar ( 32 ),  ! blank
     .   EOL  =  achar ( 10 ),  ! end of line mark
     .   Dash = '-'             ! dash or minus sign

!     Error status and handling
!     -------------------------
      character (len=*), parameter :: MyModule = 'm_RaobCore'

!     Parameters for the RaobCore records
!     -----------------------------------
      integer, parameter ::
     .   LVersionTag     = 32,     ! String length of version tag
     .   LStnID          = 5,      ! ... station ID
     .   LDate           = 8,
     .   LRecord_Max     = 120,    ! ... maximum record length in data file
     .   LAdjData        = 120,    ! ... line segment containing break data
     .   LFName          = 255,    ! ... length of file name
     .   NRecords_Max    = 1000000,! Maximum number of records in file
     .!   NTBreaksMax_Def =  5000,  ! Default maximum number of total breaks
     .   NTBreaksMax_Def = 15000,  ! Default maximum number of total breaks
     .!   NSBreaksMax_Def = 10,     ! ... for a single station.
     .   NSBreaksMax_Def = 50,     ! ... for a single station.
     .   NHTokens_Basic  = 3,      ! Number of basic tokens in header
     .   NSTokens_Basic  = 6,      ! ... in a record of station info
     .   NBTokens_Basic  = 1,      ! ... in a record of break adjustment data
     .   NSyn_Def        = 2       ! Default number of synoptic time per day
      character (len=*), parameter ::              ! (i.e. excludes levels)
     .   Break_Def       = '0.00', ! default value for a break
     .   StnID_Filler    =  BLK,   ! filler for WMO station ID
     .   Comment_Char    = '%'     ! comment character
      real,    parameter ::
     .   LatMax  =   90.001,       ! Maximum and
     .   LatMin  =  -90.001,       !  ... minimum values for the latitude
     .   LonMax  =  180.001,       !  ... and longitude
     .   LonMin  = -180.001
!     Status codes
!     ------------
      integer, parameter ::
     .   No_Error        = 0,      ! Valid error status; no error
     .   No_LUAvail      = 1,      ! No FORTRAN logical unit number
     .   Open_Error      = 2,      ! Error in opening a file
     .   Read_Error      = 3,      ! Error in reading a file
     .   Alloc_Error     = 6,      ! Allocation error
     .   Dealloc_Error   = 7,      ! Deallocation error
     .   Close_Error     = 8,      ! Error in closing a file
     .   Missing_Record  = 9,      ! Missing record 
     .   Bad_Record      = 10,     ! Bad record
     .   Bad_Template    = 12,     ! Bad file template
     .   No_Header       = 14,     ! No header in file
     .   Bad_Header      = 15,     ! Bad header in file 
     .   Bad_Argument    = 16      ! Bad input argument

!     Data structures for the header information including ...
!     --------------------------------------------------------
      type RaobCore_Header
         character  (len=LFName   )   ::
     .      FileName    =  BLK      ! ... data filename
         character  (len=LVersionTag) ::
     .      VersionTag  =  BLK      ! ... version tag 
         character  (len=LAdjData   ) ::
     .      Adj_Def     =  BLK      ! ... default adjustments (= 0.00 deg C)
         integer ::
     .      NStn        =  0,       ! ... number of stations
     .      NLevelsMax  =  0,       ! ... the maximum
     .      NLevels     =  0        ! ... and actual number of levels
         real,                      pointer, dimension (:) ::
     .      Levels      => null()   ! ... levels of the adjustments (hPa or mb)
      end type

!     ... station information including ...
!     -------------------------------------
      type RaobCore_StnInfo
         integer ::
     .      NStnMax     =  0,       ! ... the maximum and ...
     .      NStn        =  0        ! ... the actual number of stations
         character (len=LStnID),    pointer, dimension (:) ::
     .      StnID       => null()   ! ... WMO station ID
         integer,                   pointer, dimension (:) ::
     .      Indx        => null()   ! ... file or sort index number 
         real,                      pointer, dimension (:) ::
     .      Lat         => null(),  ! ... latitude  (deg)
     .      Lon         => null()   ! ... longitude (deg, -90 = 90W)
      end type

!     ... info for one station including ...
!     --------------------------------------
      type RaobCore_Stn1Info
         character (len=LRecord_Max ) ::
     .      TextLine    = BLK       ! ... text line containing the data
         character (len=LStnID) ::
     .      StnID       = BLK       ! ... WMO station ID
         integer                ::
     .      Indx,                   ! ... file index (=0 if not listed in file)
     .      NBreaks,                ! ... number of breaks
     .      NSyn                    ! ... number of synoptic times
         real                   ::                  ! record processed
     .      Lat,                    ! ... latitude  (deg)
     .      Lon                     ! ... longitude (deg, -90 = 90W)
         logical                ::           
     .      breaks_exist            ! ... = .true. if breaks exist (NBreaks>0)
      end type

!     ... navigators for each station defining the corresponding
!         array segments in the data structure, RaobCore_Breaks  
!     ----------------------------------------------------------
      type RaobCore_StnNav
         integer,                   pointer, dimension (:) ::
     .      NBreaks     => null(),  ! ... number of breaks
     .      Loc         => null()   ! ... array location of the first break
      end type

!     ... detected breaks for ...
!     ---------------------------
      type RaobCore_Breaks
         integer ::
     .      NTBrMax     =  0,       ! ... the maximum and ...
     .      NTBreaks    =  0        ! ... the actual total number of breaks
         integer,                   pointer, dimension (:) ::
     .      BreakDates  => null()   ! ... synoptic date of breaks (YYYYMMDD)
         character  (len=LAdjData), pointer, dimension (:) ::
     .      AdjData     => null()   ! ... all adjustments at each level
      end type

!     ... adjustment information including ...
!     ----------------------------------------
      type RaobCore_SynAdj
         integer ::
     .      SynDate     =  0        ! ... synoptic date of breaks (YYYYMMDD)
         integer,                   pointer, dimension (:) ::
     .      HistFlag    => null()   ! ... history flag for AdjData
     .                              !    (see below for valid values)
         character (len=LAdjData),  pointer, dimension (:) ::
     .      AdjData     => null()   ! ... all adjustments at each level
      end type
      integer, parameter ::         ! HistFlag in RaobCore_SynAdj is set to ...
     .   FromFile = 0,              ! ... if AdjData is set from data file
     .   DefUsed  = 1               ! ... if AdjData is set to default adjs

!     ... adjustments information at given stations and a synoptic date including ...
!     -------------------------------------------------------------------------------
      type RaobCore_StnAdj         
         type ( RaobCore_StnInfo ) ::
     .      StnInfo                 ! ... station info and
         integer ::
     .      NLevelsMax  =  0,       ! ... the maximum and ...
     .      NLevels     =  0,       ! ... actual number of levels
     .      NTAdjMax    =  0,       ! ... the maximum and
     .      NTAdj       =  0,       ! ... actual total number of adjustments
     .      SynDate     =  0        ! ... synoptic date
         real,                      pointer, dimension (:) ::
     .      Levels      => null (), ! ... list of levels
     .      Adj_00Z     => null (), ! ... adjustments at 0 ...
     .      Adj_12Z     => null ()  ! ... and 12 UTC
      end type

!     ... at a given station and synoptic time
!     ----------------------------------------
      type RaobCore_StnAdj1
         integer ::
     .      NLevelsMax  = 0,        ! ... the maximum and ...
     .      NLevels     = 0,        ! ... actual number of levels
     .      Indx        = 0,        ! ... file index (=0 if not listed in file)
     .      SynDate     = 0         ! ... synoptic date
         character (len=LStnID) ::
     .      StnID       = BLK       ! ... WMO station ID
         real                   ::
     .      Lat         = 0.0,      ! ... latitude
     .      Lon         = 0.0       ! ... longitude (-90=90W)
         real,                      pointer, dimension (:) ::
     .      Levels      => null (), ! ... list of pressure levels
     .      Adj_00Z     => null (), ! ... list of adjustments at 00 UTC
     .      Adj_12Z     => null (), ! ... and 12 UTC
     .      Adj                     ! ... for a given profile
      end type

!     ... all data from data file
!     ---------------------------
      type RaobCore_FData
         type ( RaobCore_Header   ) :: Header
         type ( RaobCore_StnInfo  ) :: StnInfo
         type ( RaobCore_StnNav   ) :: StnNav
         type ( RaobCore_Breaks   ) :: Breaks_00Z, Breaks_12Z
      end type

!     ... all data for a given synoptic date
!     --------------------------------------
      type RaobCore_SynDAdj
         type ( RaobCore_Header   ) :: Header
         type ( RaobCore_StnInfo  ) :: StnInfo
         type ( RaobCore_SynAdj   ) :: SynAdj_00Z, SynAdj_12Z
      end type

!     ... all data from data file for a single station (for internal use)
!     -------------------------------------------------------------------
      type RaobCore_FData1
         type ( RaobCore_Header   ) :: Header
         type ( RaobCore_Stn1Info ) :: Stn1Info
         type ( RaobCore_Breaks   ) :: Breaks_00Z, Breaks_12Z
      end type

      contains

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Header - Initializes pointers in the header data structure
!
! !INTERFACE:
      subroutine Nullify_Header ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_Header ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     12Feb2007  C. Redder  Initial code
!
!EOP
!.................................................................

      nullify ( This % Levels )

      return
      end subroutine Nullify_Header

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_StnInfo - Initializes pointers in the data structure for station info
!
! !INTERFACE:
      subroutine Nullify_StnInfo ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnInfo ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     12Feb2007  C. Redder  Initial code
!
!EOP
!.................................................................

      nullify ( This % StnID,
     .          This % Indx,
     .          This % Lat,
     .          This % Lon )

      return
      end subroutine Nullify_StnInfo

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Stn1Info - Initializes pointers in the data structure for info of a single station
!
! !INTERFACE:
      subroutine Nullify_Stn1Info ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_Stn1Info ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     12Feb2007  C. Redder  Initial code
!
!EOP
!.................................................................

      continue ! No pointers in this data structure
 
      return
      end subroutine Nullify_Stn1Info

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_StnNav - Initializes pointers in the data structure for station navigators
!
! !INTERFACE:
      subroutine Nullify_StnNav ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnNav ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     12Feb2007  C. Redder  Initial code
!
!EOP
!.................................................................

      nullify ( This % NBreaks,
     .          This % Loc )

      return
      end subroutine Nullify_StnNav

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Breaks - Initializes pointers in the data structure for listing breaks
!
! !INTERFACE:
      subroutine Nullify_Breaks ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_Breaks ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     12Feb2007  C. Redder  Initial code
!
!EOP
!.................................................................

      nullify ( This % BreakDates,
     .          This % AdjData )

      return
      end subroutine Nullify_Breaks

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_SynAdj - Initializes pointers in the data structure for storing adjustments at a synoptic time
! !INTERFACE:
      subroutine Nullify_SynAdj ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_SynAdj ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     12Feb2007  C. Redder  Initial code
!
!EOP
!.................................................................

      nullify ( This % HistFlag,
     .          This % AdjData )

      return
      end subroutine Nullify_SynAdj

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_StnAdj - Initializes pointers in the data structure for storing adjustments for stations and a synoptic date
! !INTERFACE:
      subroutine Nullify_StnAdj ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnAdj ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     09Mar2007  C. Redder  Initial code
!
!EOP
!.................................................................

      call Nullify_StnInfo ( This % StnInfo )
      nullify ( This % Levels,
     .          This % Adj_00Z,
     .          This % Adj_12Z )

      return
      end subroutine Nullify_StnAdj

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_StnAdj1 - Initializes pointers in the data structure for storing adjustments for a station and synoptic date
! !INTERFACE:
      subroutine Nullify_StnAdj1 ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnAdj1 ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     07Mar2007  C. Redder  Initial code
!
!EOP
!.................................................................

      nullify ( This % Levels,
     .          This % Adj_00Z,
     .          This % Adj_12Z,
     .          This % Adj )

      return
      end subroutine Nullify_StnAdj1

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_FData1 - Initializes pointers in the file data structure 
!
! !INTERFACE:
      subroutine Nullify_FData1 ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_FData1 ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     22Feb2007  C. Redder  Initial code
!EOP
!.................................................................

      call Nullify_Header   ( This % Header     )
      call Nullify_Stn1Info ( This % Stn1Info   )
      call Nullify_Breaks   ( This % Breaks_00Z )
      call Nullify_Breaks   ( This % Breaks_12Z )

      return
      end subroutine Nullify_FData1

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_FData - Initializes pointers in the entire file data structure
!
! !INTERFACE:
      subroutine Nullify_FData ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_FData ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     12Feb2007  C. Redder  Initial code
!EOP
!.................................................................

      call Nullify_Header  ( This % Header     )
      call Nullify_StnInfo ( This % StnInfo    )
      call Nullify_StnNav  ( This % StnNav     )
      call Nullify_Breaks  ( This % Breaks_00Z )
      call Nullify_Breaks  ( This % Breaks_12Z )

      return
      end subroutine Nullify_FData

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_SynDAdj - Initializes pointers in the adjustments for the synoptic date
!
! !INTERFACE:
      subroutine Nullify_SynDAdj ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_SynDAdj ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     12Feb2007  C. Redder  Initial code
!EOP
!.................................................................

      call Nullify_Header  ( This % Header     )
      call Nullify_StnInfo ( This % StnInfo    )
      call Nullify_SynAdj  ( This % SynAdj_00Z )
      call Nullify_SynAdj  ( This % SynAdj_12Z )

      return
      end subroutine Nullify_SynDAdj

!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_Header - Initializes and deallocates header data structure
!
! !INTERFACE:
      subroutine Clean_Header ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_Header ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     13Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Header'
      logical :: check_stat
      integer :: istat

      This % FileName   = BLK
      This % VersionTag = BLK
      This % Adj_Def    = BLK
      This % NStn       = 0
      This % NLevelsMax = 0
      This % NLevels    = 0

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

!     Deallocate memory
!     -----------------
      deallocate ( This % Levels, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%Levels' )) // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_Header ( This )

      return
      end subroutine  Clean_Header
!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_StnInfo - Initializes and deallocates data structure for station info
!
! !INTERFACE:
      subroutine Clean_StnInfo ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnInfo ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     13Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_StnInfo'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % NStnMax = 0
      This % NStn    = 0

!     Deallocate memory
!     -----------------
      deallocate ( This % StnID,
     .             This % Indx,
     .             This % Lat,
     .             This % Lon, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%StnID,'
     .                                    // 'This%Indx,'
     .                                    // 'This%Lat,'
     .                                    // 'This%Lon' )) // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_StnInfo ( This )

      return
      end subroutine  Clean_StnInfo
!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_Stn1Info - Initializes and deallocates data structure for info of a single station
!
! !INTERFACE:
      subroutine Clean_Stn1Info ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_Stn1Info ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     22Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Stn1Info'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % TextLine      =  BLK
      This % StnID         =  BLK
      This % Indx          =  0
      This % NBreaks       =  0
      This % NSyn          =  0
      This % Lat           =  0.0
      This % Lon           =  0.0
      This % breaks_exist  = .false.

!     Deallocate memory
!     -----------------
      continue ! no pointers in this data structure
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%Null,'
     .                                    // 'This%Null' )) // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_Stn1Info ( This )

      return
      end subroutine  Clean_Stn1Info
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_StnNav - Initializes and deallocates data structure for station navigators
!
! !INTERFACE:
      subroutine Clean_StnNav ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnNav ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     13Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_StnNav'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

!     Deallocate memory
!     -----------------
      deallocate ( This % NBreaks,
     .             This % Loc, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%NBreaks,'
     .                                    // 'This%Loc' )) // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_StnNav ( This )

      return
      end subroutine  Clean_StnNav
!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_Breaks - Initializes and deallocates data structure for listing breaks
!
! !INTERFACE:
      subroutine Clean_Breaks ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_Breaks ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     13Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Breaks'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % NTBrMax   = 0
      This % NTBreaks  = 0

!     Deallocate memory
!     -----------------
      deallocate ( This % BreakDates,
     .             This % AdjData, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%BreakDates,'
     .                                    // 'This%AdjData' )) // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_Breaks ( This )

      return
      end subroutine  Clean_Breaks
!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_SynAdj - Initializes and deallocates data structure for storing adjustments at a synoptic time.
!
! !INTERFACE:
      subroutine Clean_SynAdj ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_SynAdj ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     13Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_SynAdj'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % SynDate   = 0

!     Deallocate memory
!     -----------------
      deallocate ( This % HistFlag,
     .             This % AdjData, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%HistFlag,'
     .                                    // 'This%AdjData' )) // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_SynAdj ( This )

      return
      end subroutine  Clean_SynAdj

!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_StnAdj - Initializes and deallocates data structure for storing adjustments for stations and synoptic date.
!
! !INTERFACE:
      subroutine Clean_StnAdj ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, ErrStat, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnAdj ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,         intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     07Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_StnAdj'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % NLevelsMax  = 0
      This % NLevels     = 0
      This % NTAdjMax    = 0
      This % NTAdj       = 0
      This % SynDate     = 0

!     Clean up station information
!     ----------------------------
      call Clean_StnInfo ( This % StnInfo,    stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_StnInfo' ) // '\W' )
            return
         end if
      end if

!     Deallocate memory
!     -----------------
      deallocate ( This % Levels,
     .             This % Adj_00Z,
     .             This % Adj_12Z, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%Levels,'
     .                                    // 'This%Adj_00Z,'
     .                                    // 'This%Adj_12Z' )) // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_StnAdj ( This )

      return
      end subroutine  Clean_StnAdj
!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_StnAdj1 - Initializes and deallocates data structure for storing adjustments for a station and synoptic date.
!
! !INTERFACE:
      subroutine Clean_StnAdj1 ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnAdj1 ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,         intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     07Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_StnAdj1'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % NLevelsMax  = 0
      This % NLevels     = 0
      This % Indx        = 0
      This % SynDate     = 0
      This % StnID       = BLK
      This % Lat         = 0.0
      This % Lon         = 0.0

!     Deallocate memory
!     -----------------
      deallocate ( This % Levels,
     .             This % Adj_00Z,
     .             This % Adj_12Z,
     .             This % Adj, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%Levels,'
     .                                    // 'This%Adj_00Z,'
     .                                    // 'This%Adj_12Z,'
     .                                    // 'This%Adj' )) // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_StnAdj1 ( This )

      return
      end subroutine  Clean_StnAdj1
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_FData - Initialize and deallocates data structure for data file
!
! !INTERFACE:
!
      subroutine Clean_FData ( This, stat )
!
! !USES:
      use m_AdvError, only : PErr, ErrStat
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_FData ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     13Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_FData'
      logical :: check_stat

      check_stat = present ( stat )

      call Clean_Header  ( This % Header,     stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_Header' )  // '\W' )
            return
         end if
      end if

      call Clean_StnInfo ( This % StnInfo,    stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_StnInfo' ) // '\W' )
            return
         end if
      end if

      call Clean_StnNav  ( This % StnNav,     stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_StnNav' )  // '\W' )
            return
         end if
      end if

      call Clean_Breaks  ( This % Breaks_00Z, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_Breaks' )  // '\W' )
            return
         end if
      end if

      call Clean_Breaks  ( This % Breaks_12Z, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_Breaks' )  // '\W' )
            return
         end if
      end if

      return
      end subroutine  Clean_FData
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_FData1 - Initializes and deallocates data structure for data file at a single station
!
! !INTERFACE:
!
      subroutine Clean_FData1 ( This, stat )
!
! !USES:
      use m_AdvError, only : PErr, ErrStat
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_FData1 ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     22Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_FData1'
      logical :: check_stat

      check_stat = present ( stat )

      call Clean_Header   ( This % Header,     stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_Header'   ) // '\W' )
            return
         end if
      end if

      call Clean_Stn1Info ( This % Stn1Info,   stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_Stn1Info' ) // '\W' )
            return
         end if
      end if

      call Clean_Breaks   ( This % Breaks_00Z, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_Breaks' )  // '\W' )
            return
         end if
      end if

      call Clean_Breaks   ( This % Breaks_12Z, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_Breaks' )  // '\W' )
            return
         end if
      end if

      return
      end subroutine  Clean_FData1
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_SynDAdj - Initialize and deallocates data structure for a synoptic date
!
! !INTERFACE:
!
      subroutine Clean_SynDAdj ( This, stat )
!
! !USES:
      use m_AdvError, only : PErr, ErrStat
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_SynDAdj ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     13Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_SynDAdj'
      logical :: check_stat

      check_stat = present ( stat )

      call Clean_Header  ( This % Header,     stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_Header' )  // '\W' )
            return
         end if
      end if

      call Clean_StnInfo ( This % StnInfo,    stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_StnInfo' ) // '\W' )
            return
         end if
      end if

      call Clean_SynAdj  ( This % SynAdj_00Z, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_SynAdj'  )  // '\W' )
            return
         end if
      end if

      call Clean_SynAdj  ( This % SynAdj_12Z, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_SynAdj'  )  // '\W' )
            return
         end if
      end if

      return
      end subroutine  Clean_SynDAdj
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_Header - Initializes structure for header
!
! !INTERFACE:
      subroutine Init_Header ( NLevelsMax, This, stat,  ! required and
     .                         hdr_data )               ! optional args
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                  intent (in)    ::
     .   NLevelsMax           ! Maximum number of levels
      type ( RaobCore_Header ), intent (in), optional ::
     .   hdr_data             ! Header data to be copied to output arg
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_Header ), intent (inout) ::
     .   This                 ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                  intent (out)   ::
     .   stat                 ! Error status code.  If no error,
                              ! then zero is returned
! !REVISION HISTORY:
!     13Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_Header'

      This % FileName   =  BLK
      This % VersionTag =  BLK
      This % Adj_Def    =  BLK
      This % NStn       =  0
      This % NLevelsMax =  NLevelsMax
      This % NLevels    =  0

!     Allocate memory
!     ---------------
      allocate ( This % Levels  ( NLevelsMax ), stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'This%Levels',
     .                                      NLevelsMax ) // '\W' )
         stat = Alloc_Error
         return
      end if

!     If desired, copy data from input to output argument
!     --------------------------------------------------- 
      if ( present ( hdr_data )) call Copy_Header ( hdr_data, This )

      return
      end subroutine Init_Header

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_StnInfo - Initializes structure for station info
!
! !INTERFACE:
      subroutine Init_StnInfo ( NStnMax, This, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NStnMax               ! Maximum number of stations
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnInfo ), intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     13Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_StnInfo'

      This % NStnMax    =  NStnMax
      This % NStn       =  0

!     Allocate memory
!     ---------------
      allocate ( This % StnID ( NStnMax ),
     .           This % Indx  ( NStnMax ),
     .           This % Lat   ( NStnMax ),
     .           This % Lon   ( NStnMax ), stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'This%StnID,'
     .                                  // 'This%Indx,'
     .                                  // 'This%Lat,'
     .                                  // 'This%Lon',
     .                                      NStnMax ) // '\W' )
         stat = Alloc_Error
         return
      end if

      return
      end subroutine Init_StnInfo

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_Stn1Info - Initializes structure for info at a single station
!
! !INTERFACE:
      subroutine Init_Stn1Info ( This, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_Stn1Info ), intent (inout) ::
     .   This                   ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                    intent (out)   ::
     .   stat                   ! Error status code.  If no error,
                                ! then zero is returned
! !REVISION HISTORY:
!     22Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_Stn1Info'
      integer :: N

      stat = No_Error

      This % TextLine      =  BLK
      This % StnID         =  BLK
      This % Indx          =  0
      This % NBreaks       =  0
      This % NSyn          =  NSyn_Def
      This % Lat           =  0.0
      This % Lon           =  0.0
      This % breaks_exist  = .false.

!     Allocate memory
!     ---------------
      continue ! no pointers in this data structure      
      if ( stat .ne. No_Error ) then
         N = 0
         call PErr ( MyName, Alloc ( stat, 'This%Null,'
     .                                  // 'This%Null,',
     .                                      N ) // '\W' )
         stat = Alloc_Error
         return
      end if

      return
      end subroutine Init_Stn1Info

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_StnNav - Initializes structure for station navigators
!
! !INTERFACE:
      subroutine Init_StnNav ( NStnMax, This, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NStnMax               ! Maximum number of stations
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnNav ),  intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     13Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_StnNav'

!     Allocate memory
!     ---------------
      allocate ( This % NBreaks ( NStnMax ),
     .           This % Loc     ( NStnMax ), stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'This%NBreaks,'
     .                                  // 'This%Loc',
     .                                      NStnMax ) // '\W' )
         stat = Alloc_Error
         return
      end if

      return
      end subroutine Init_StnNav

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_Breaks - Initializes structure for listing breaks
!
! !INTERFACE:
      subroutine Init_Breaks ( NTBrMax, This, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NTBrMax               ! Maximum total number of breaks
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_Breaks ),  intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     13Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_Breaks'

      This % NTBrMax    =  NTBrMax
      This % NTBreaks   =  0

      allocate ( This % BreakDates ( NTBrMax ),
     .           This % AdjData    ( NTBrMax ), stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'This%BreakDates,'
     .                                  // 'This%AdjData',
     .                                      NTBrMax ) // '\W' )
         stat = Alloc_Error
         return
      end if

      return
      end subroutine Init_Breaks

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_SynAdj - Initializes structure for adjustment info
!
! !INTERFACE:
      subroutine Init_SynAdj ( NStnMax, This, stat,
     .                         syn_date )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NStnMax               ! Maximum total number of stations
      integer, optional,         intent (in)    ::
     .   syn_date              ! Syn date to be copied to output arg
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_SynAdj ),  intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     13Feb2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_SynAdj'

      This % SynDate    =  0

      allocate ( This % HistFlag   ( NStnMax ),
     .           This % AdjData    ( NStnMax ), stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'This%HistFlag,'
     .                                  // 'This%AdjData',
     .                                      NStnMax ) // '\W' )
         stat = Alloc_Error
         return
      end if

      if ( present ( syn_date )) This % SynDate = syn_date
      return
      end subroutine Init_SynAdj

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_StnAdj - Initializes structure for adjustment info at a station and synoptic date
!
! !INTERFACE:
      subroutine Init_StnAdj ( NLevelsMax, NStnMax, This, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat, Array
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NLevelsMax,           ! Maximum total number of levels
     .   NStnMax               ! ... and stations
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnAdj ),  intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     07Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_StnAdj'
      integer :: NTAdjMax, dstat

      NTAdjMax = NLevelsMax * NStnMax

      This % NLevelsMax =  NLevelsMax
      This % NLevels    =  0
      This % NTAdjMax   =  NTAdjMax
      This % NTAdj      =  0
      This % SynDate    =  0

      call Init_StnInfo ( NStnMax,    This % StnInfo,      stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_StnInfo' ) // '\W' )
         return
      end if

      allocate ( This % Levels  ( NLevelsMax ),
     .           This % Adj_00Z ( NTAdjMax   ),
     .           This % Adj_12Z ( NTAdjMax   ),    stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName,
     .               Alloc ( stat,
     .                       Array ( 'This%Levels',  NLevelsMax )
     .                    // Array ( 'This%Adj_00Z',   NTAdjMax )
     .                    // Array ( 'This%Adj_12Z',   NTAdjMax ))
     .                    // '\W' )
         call Clean_StnInfo  ( This % StnInfo )
         stat = Alloc_Error
         return
      end if

      return
      end subroutine Init_StnAdj

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_StnAdj1 - Initializes structure for adjustment info for a station and synoptic date
!
! !INTERFACE:
      subroutine Init_StnAdj1 ( NLevelsMax, This, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NLevelsMax            ! Maximum total number of levels
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnAdj1 ), intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     07Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_StnAdj1'

      This % NLevelsMax =  NLevelsMax
      This % NLevels    =  0
      This % Indx       =  0
      This % SynDate    =  0
      This % StnID      =  BLK
      This % Lat        =  0.0
      This % Lon        =  0.0

      allocate ( This % Levels  ( NLevelsMax ),
     .           This % Adj_00Z ( NLevelsMax ),
     .           This % Adj_12Z ( NLevelsMax ),
     .           This % Adj     ( NLevelsMax ), stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'This%Levels,'
     .                                  // 'This%Adj_00Z,'
     .                                  // 'This%Adj_12Z,'
     .                                  // 'This%Adj',
     .                                      NLevelsMax ) // '\W' )
         stat = Alloc_Error
         return
      end if

      return
      end subroutine Init_StnAdj1

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_FData - Initializes structure for data file contents
!
! !INTERFACE:
      subroutine Init_FData ( NLevelsMax, NStnMax, NTBreaksMax,
     .                        This, stat,      ! required and
     .                        hdr_data )       ! optional args
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NLevelsMax,           ! Maximum number of levels
     .   NStnMax,              ! ... stations
     .   NTBreaksMax           ! ... total breaks listed
      type ( RaobCore_Header ), intent (in), optional ::
     .   hdr_data             ! Header data to be copied to output arg
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_FData ),  intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     01Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_FData'

      call Init_Header  ( NLevelsMax, This % Header,       stat,
     .                    hdr_data = hdr_data )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Header'  ) // '\W' )
         return
      end if

      call Init_StnInfo ( NStnMax,    This % StnInfo,      stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_StnInfo' ) // '\W' )
         call Clean_Header  ( This % Header  )
         return
      end if

      call Init_StnNav ( NStnMax,     This % StnNav,       stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_StnNav'  ) // '\W' )
         call Clean_Header  ( This % Header  )
         call Clean_StnInfo ( This % StnInfo )
         return
      end if

      call Init_Breaks ( NTBreaksMax, This % Breaks_00Z,   stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Breaks' )  // '\W' )
         call Clean_Header  ( This % Header  )
         call Clean_StnInfo ( This % StnInfo )
         call Clean_StnNav  ( This % StnNav  )
         return
      end if

      call Init_Breaks ( NTBreaksMax, This % Breaks_12Z,   stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Breaks' )  // '\W' )
         call Clean_Header  ( This % Header  )
         call Clean_StnInfo ( This % StnInfo )
         call Clean_StnNav  ( This % StnNav  )
         call Clean_Breaks  ( This % Breaks_00Z )
         return
      end if

      return
      end subroutine Init_FData

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_FData1 - Initializes structure for data file contents for a single station
!
! !INTERFACE:
      subroutine Init_FData1 ( NLevelsMax, NTBreaksMax, This, stat,
     .                         hdr_data )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NLevelsMax,           ! Maximum number of levels
     .   NTBreaksMax           ! ... total breaks listed
      type ( RaobCore_Header ),  intent (in), optional ::
     .   hdr_data              ! Header data to be copied to output arg
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_FData1 ), intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                  intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     01Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_FData1'

      call Init_Header   ( NLevelsMax, This % Header,       stat,
     .                     hdr_data = hdr_data )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Header'   ) // '\W' )
         return
      end if

      call Init_Stn1Info (             This % Stn1Info,     stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Stn1Info' ) // '\W' )
         call Clean_Header  ( This % Header  )
         return
      end if

      call Init_Breaks   ( NTBreaksMax, This % Breaks_00Z,  stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Breaks' )   // '\W' )
         call Clean_Header   ( This % Header   )
         call Clean_Stn1Info ( This % Stn1Info )
         return
      end if

      call Init_Breaks   ( NTBreaksMax, This % Breaks_12Z,  stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Breaks' )   // '\W' )
         call Clean_Header   ( This % Header     )
         call Clean_Stn1Info ( This % Stn1Info   )
         call Clean_Breaks   ( This % Breaks_00Z )
         return
      end if

      return
      end subroutine Init_FData1

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_SynDAdj - Initializes structure for adjustment info for a synoptic date
!
! !INTERFACE:
      subroutine Init_SynDAdj ( NLevelsMax, NStnMax, This, stat,
     .                          hdr_data, syn_date )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NLevelsMax,           ! Maximum number of levels
     .   NStnMax               ! ... stations
      type ( RaobCore_Header ),  intent (in), optional ::
     .   hdr_data              ! Header data to be copied to output arg
      integer,                   intent (in), optional ::
     .   syn_date              ! Syn date to be copied to output arg
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_SynDAdj ), intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     01Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_SynDAdj'

      call Init_Header  ( NLevelsMax, This % Header,      stat,
     .                    hdr_data = hdr_data )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Header'  ) // '\W' )
         return
      end if

      call Init_StnInfo ( NStnMax,    This % StnInfo,     stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_StnInfo' ) // '\W' )
         call Clean_Header  ( This % Header  )
         return
      end if

      call Init_SynAdj  ( NStnMax,     This % SynAdj_00Z, stat,
     .                    syn_date = syn_date )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Breaks' ) // '\W' )
         call Clean_Header  ( This % Header  )
         call Clean_StnInfo ( This % StnInfo )
         return
      end if

      call Init_SynAdj  ( NStnMax,     This % SynAdj_12Z, stat,
     .                    syn_date = syn_date )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Breaks' ) // '\W' )
         call Clean_Header  ( This % Header  )
         call Clean_StnInfo ( This % StnInfo )
         call Clean_SynAdj  ( This % SynAdj_00Z )
         return
      end if

      return
      end subroutine Init_SynDAdj

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  GetRC_FData () --- Get the entire RaobCore data
! 
! !DESCRIPTION:
!     This routine retrieves RAOBCORE data and stores it into a data
!     structure of derived type, RaobCore_FData.
!
! !INTERFACE:
      subroutine GetRC_FData ( File, FData, stat,  ! Required and
     .                         check_adj )         ! optional arguments
      use m_AdvError,          only : PErr, ErrStat
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len = *),      intent (in)    ::
     .   File                 ! Name of input file.
      logical, optional,        intent (in)    ::
     .   check_adj            ! = .true. to check all adjustment data
!                             !   Default: .false.
! !OUTPUT PARAMETERS: 
      type ( RaobCore_FData ),  intent (inout) :: 
     .   FData                ! RAOBCORE data
      integer,                  intent (out)   ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     07Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter :: MyName =    MyModule
     .                                      // '::Get_FData'
      integer :: SynDate = 20010101
      type ( RaobCore_SynDAdj ) :: SynDAdj

      call GetRC_All ( File,    SynDate,
     .                 SynDAdj, FData, stat,
     .                 no_syndadj = .true.,
     .                 no_fdata   = .false.,
     .                 check_adj  =  check_adj )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'GetRC_All' ) // '\W' )
         return
      end if

      return
      end subroutine GetRC_FData
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  GetRC_SynDAdj () --- Get the RaobCore data for a synoptic date
! 
! !DESCRIPTION:
!     This routine retrieves RAOBCORE data for a given synoptic data and
!     stores it into a data structure of derived type, RaobCore_SynDAdj.
!
! !INTERFACE:
      subroutine GetRC_SynDAdj ( File, SynDate, SynDAdj, stat, ! Required and
     .                           check_adj )                   ! optional arguments
      use m_AdvError,            only : PErr, ErrStat
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len = *),       intent (in)    ::
     .   File                  ! Name of input file.
      integer,                   intent (in)    ::
     .   SynDate               ! Synoptic data (YYYYMMDD format)
      logical, optional,         intent (in)    ::
     .   check_adj             ! = .true. to check all adjustment data
!                              !   Default: .false.
! !OUTPUT PARAMETERS: 
      type ( RaobCore_SynDAdj ), intent (inout) :: 
     .   SynDAdj               ! RAOBCORE data
      integer,                   intent (out)   ::
     .   stat                  ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     07Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter :: MyName =    MyModule
     .                                      // '::Get_SynDAdj'
      type ( RaobCore_FData ) :: FData

      call GetRC_All ( File,    SynDate,
     .                 SynDAdj, FData, stat,
     .                 no_syndadj = .false.,
     .                 no_fdata   = .true.,
     .                 check_adj  =  check_adj )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'GetRC_All' ) // '\W' )
         return
      end if

      return
      end subroutine GetRC_SynDAdj
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  GetRC_All () --- Get the RaobCore data from file
! 
! !DESCRIPTION:
!     This routine retrieves RaobCore data and stores it into a
!     data structure of derived type, RaobCore_FData and/or 
!     RaobCore_SynDAdj.  This routine is internal.
!
! !INTERFACE:
      subroutine GetRC_All ( FileName,   SynDate,
     .                       SynDAdj,    FData, stat, ! Required &
     .                       no_syndadj, no_fdata,    ! optional arguments
     .                       check_adj )
      use m_SunAlt,   only : Check_Date
      use m_AdvError, only : ItoA, PErr, ErrStat, WPErr
      use m_SysIO,    only : LUAvail
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len = *),       intent (in)    ::
     .   FileName    ! Name of RaobCore file.
      integer,                   intent (in)    ::
     .   SynDate     ! Synoptic date (YYYYMMDD, for argument, SynDAdj)
      logical, optional,         intent (in)    ::
     .   no_syndadj, ! = .true. to prevent storage of syn adjustment data
     .   no_fdata,   ! ... file data 
     .   check_adj   ! ... to check all adjustment data
!                    !   Defaults for all optional args = .false.
! !OUTPUT PARAMETERS:
      type ( RaobCore_FData   ), intent (inout) :: 
     .   FData       ! CARDS meta-data
      type ( RaobCore_SynDAdj ), intent (inout) :: 
     .   SynDAdj     ! ... observation data
      integer,                   intent (out)   ::
     .   stat        ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     20Feb2007  C. Redder  Origional code.
!
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName = MyModule // '::GetRC_All'

      logical :: acquire_syndadj, acquire_fdata, next_station, rescan
      logical :: eof, eor
      integer :: lu
      integer :: iScan
      integer :: NTBreaksMax, NSBreaksMax, NSBreaksRec, NSBreaksMax2
      integer :: NTBreaks,    NSBreaks,    iBreakRec
      integer :: NStn, iStn, NSyn
      integer :: NLevelsMax
      integer :: iRec, LRec, iLine, LLine
      character(len=LRecord_Max) :: ThisRecord, ThisLine
      character(len=510) :: Tail1, Tail2, Tail2a, Message
      type ( RaobCore_Header ) :: Header
      type ( RaobCore_FData1 ) :: FData1

!     Implement options
!     -----------------
      acquire_syndadj = .true.
      if ( present ( no_syndadj )) acquire_syndadj = .not. no_syndadj
      acquire_fdata   = .true.
      if ( present ( no_fdata   )) acquire_fdata   = .not. no_fdata

!     Default status
!     --------------
      stat = No_Error

!     Check the argument SynDate
!     --------------------------
      if ( Check_Date ( SynDate, Message ) .ne. No_Error ) then
         call WPErr ( MyName, trim ( Message )
     .        // '\n\C   Date = ' // trim ( ItoA ( SynDate )))
         stat = Bad_Argument
         return
      end if

!     Determine an available logical unit
!     -----------------------------------
      lu = LUAvail()
      if ( lu .lt. 0 ) then
         stat = No_LUAvail
         call PErr ( MyName, 'No logical units available for the '
     .                    // 'file, ' // trim ( FileName ) // '\W'  )
         return
      end if

!     Scan the input file to determine the number of
!     adjustments and store the data.  Rescan the file if
!     the storage space is insufficient using the numbers
!     from the first scan to reinitialize the storage space
!     -----------------------------------------------------
      NTBreaksMax = NTBreaksMax_Def
      NSBreaksMax = NSBreaksMax_Def
      ScanLoop : do  iScan = 1, 2

!        Open input file
!        ---------------
         open ( unit   =  lu,
     .          file   =  FileName,
     .          form   = 'formatted',
     .          status = 'old',
     .          access = 'sequential',
     .          recl   =  LRecord_Max,
     .          iostat =  stat )
         Tail1 = '\n   File = ' // trim ( FileName )
         if ( stat .ne. No_Error ) then
            call PErr ( MyName, 'Open error (iostat = '
     .                        // trim ( ItoA ( stat ) ) // ') \C'
     .                        // Tail1 )
            stat = Open_Error
            exit ScanLoop
         end if

!        Read each record
!        ----------------
         iLine      = 0
         RecordLoop : do iRec = 1, NRecords_Max

!           Read file line
!           --------------
            call ReadLn ( lu, LRec, ThisRecord, eor, eof, stat )
            if ( stat .ne. No_Error ) then
               if ( iLine .gt. 0 ) then
                  call PErr ( MyName, 'Read error (iostat = '
     .                           //    trim ( ItoA ( stat )) // ') \C'
     .                           //    Tail2a )
               else
                  call PErr ( MyName, 'Read error (iostat = '
     .                           //    trim ( ItoA ( stat ) )
     .                           // ') at beginning of file \C'
     .                           //    Tail1  )
               end if
               stat = Read_Error
               exit ScanLoop
            end if

!           Exit loop if end of file has been reached
!           -----------------------------------------
            if ( eof ) exit RecordLoop
            Tail2  = trim ( Tail1 )
     .         // '\n   Line = '          // trim ( ThisRecord )
            Tail2a = trim ( Tail1 )
     .         // '\n   Previous line = ' // trim ( ThisRecord )

!           Exit with error status if entire record was not read
!           ----------------------------------------------------
            if ( .not. eor ) then
               call PErr  ( MyName, 'Did not read entire line \C'
     .                            // Tail2 )
               stat = Bad_Header
               exit ScanLoop
            end if

!           Search for comment character
!           ----------------------------
            LLine = scan ( ThisRecord ( : LRec ), Comment_Char ) - 1
            if ( LLine .lt. 0 ) then
               LLine = len_trim ( ThisRecord ( : LRec  ))
            else
               LLine = len_trim ( ThisRecord ( : LLine ))
            end if

!           Ignore record if there is no data
!           ---------------------------------
            if ( LLine .eq. 0 ) cycle RecordLoop

!           Otherwise process this line
!           ---------------------------
            ThisLine = ThisRecord ( : LLine )

!           ... as a header if this is the first line
!           -----------------------------------------
            iLine = iLine + 1
            if ( iLine .eq. 1 ) then
               call Get_Header ( ThisLine, Header, stat )
               if ( stat .ne. No_Error ) then
                  call WPErr ( MyName, ErrStat ( 'Get_Header', stat ))
                  exit ScanLoop
               end if
               Header % FileName = FileName

!              ... and initialize the necessary data structures
!              ------------------------------------------------
               NLevelsMax = Header % NLevelsMax
               call Init_FData1 ( NLevelsMax, NSBreaksMax, 
     .                            FData1, stat,
     .                            hdr_data = Header )
               if ( stat .ne. No_Error ) then
                  call WPErr    ( MyName,
     .                            ErrStat ( 'Init_FData1', stat ))
                  exit ScanLoop
               end if

               NStn       = Header % NStn
               if ( acquire_fdata   ) then
                  call Init_FData   ( NLevelsMax, NStn, NTBreaksMax,
     .                                FData,   stat,
     .                                hdr_data = Header )
                  if ( stat   .ne. No_Error ) then
                     call WPErr ( MyName,
     .                            ErrStat ( 'Init_FData',   stat ))
                     exit ScanLoop
                  end if
               end if
               if ( acquire_syndadj ) then
                  call Init_SynDAdj ( NLevelsMax, NStn,
     .                                SynDAdj, stat,
     .                                hdr_data = Header,
     .                                syn_date = SynDate )
                  if ( stat   .ne. No_Error ) then
                     call WPErr ( MyName,
     .                            ErrStat ( 'Init_SynDAdj', stat ))
                     exit ScanLoop
                  end if
               end if

!              ... and flags and counters
!              --------------------------
               NSBreaksMax2 = 0
               iStn         = 0
               next_station = .true.

            else

!              ... or as a record with station information
!              -------------------------------------------
               if    ( next_station ) then
                  call Get_Stn1Info ( ThisLine ( : LLine ), Header,
     .                                FData1 % Stn1Info, stat )
                  if ( stat   .ne. No_Error ) then
                     call WPErr ( MyName,
     .                            ErrStat ( 'Get_Stn1Info', stat )
     .                         // Tail2 )
                     exit ScanLoop
                  end if

!                 Ensure that no record is missing
!                 --------------------------------
                  iStn            = iStn + 1
                  if ( iStn .ne. FData1 % Stn1Info % Indx ) then
                     call WPErr ( MyName,
     .                           'Record containing station '
     .                       //  'information with index number '
     .                       //   trim ( ItoA ( iStn ))
     .                       // ' is missing or is out-of-order. '
     .                       //   Tail2 )
                     stat = Missing_Record
                     exit ScanLoop
                  end if

!                 Initialize counters and list sizes
!                 ----------------------------------
                  NSyn         =  FData1 % Stn1Info % NSyn
                  NSBreaks     =  FData1 % Stn1Info % NBreaks 
                  NSBreaksMax2 =  max ( NSBreaksMax2, NSBreaks )
                  NSBreaksRec  =  NSyn     * NSBreaks
                  iBreakRec    =  0

!                 Go to next station if there are no breaks
!                 -----------------------------------------
                  next_station =  NSBreaks .eq. 0
               else

!                 ... or with break information
!                 -----------------------------
                  iBreakRec    = iBreakRec + 1

!                 ... information about the breaks
!                 --------------------------------
                  call Get_Breaks ( ThisLine ( : LLine ), FData1, stat,
     .                              reset     = iBreakRec .eq. 1,
     .                              check_adj = check_adj )
                  if ( stat .ne. No_Error ) then
                     call WPErr ( MyName,
     .                            ErrStat ( 'Get_Breaks', stat )
     .                         // Tail2 )
                     exit ScanLoop
                  end if

                  next_station = iBreakRec .eq. NSBreaksRec

               end if

!              ... save data in output arguments
!              ---------------------------------
               if ( next_station ) then
                  if ( acquire_fdata   ) then
                     call Copy_to_FData   ( FData1,
     .                                      FData,   stat )
                     if ( stat .ne. No_Error ) then
                        call WPErr ( MyName,
     .                               ErrStat ( 'Copy_to_FData',
     .                                          stat ))
                        exit ScanLoop
                     end if
                  end if
                  if ( acquire_syndadj ) then
                     call Copy_to_SynDAdj ( FData1,  SynDate,
     .                                      SynDAdj, stat )
                     if ( stat .ne. No_Error ) then
                        call WPErr ( MyName,
     .                               ErrStat ( 'Copy_to_SynDAdj',
     .                                          stat ))
                        exit ScanLoop
                     end if
                  end if
               end if
            end if
         end do RecordLoop         

!        Close input file and ...
!        ------------------------
         close (lu)

!        ... clean up local data structures
!        ------------------------------
         call Clean_Header (  Header, stat )
         if ( stat .ne. No_Error ) then
            call WPErr ( MyName, ErrStat ( 'Clean_Header', stat )
     .                           // Tail2 )
            exit ScanLoop
         end if
         call Clean_FData1 (  FData1, stat )
         if ( stat .ne. No_Error ) then
            call WPErr ( MyName, ErrStat ( 'Clean_FData1', stat )
     .                        // Tail2 )
            exit ScanLoop
         end if

!        Determine if a rescan is necessary
!        ----------------------------------
         if ( acquire_fdata ) then
            NTBreaks = sum ( FData % StnNav % NBreaks ( : NStn ))
            rescan = NTBreaks     .gt. NTBreaksMax
     .          .or. NSBreaksMax2 .gt. NSBreaksMax
         else
            rescan = NSBreaksMax2 .gt. NSBreaksMax
         end if

!        If so, clean up for second scan
!        -------------------------------
         if ( rescan ) then
            NSBreaksMax = NSBreaksMax2
            if ( acquire_fdata   ) then
               NTBreaksMax = NTBreaks
               call Clean_FData   ( FData,   stat )
               if ( stat   .ne. No_Error ) then
                  call WPErr ( MyName, ErrStat ( 'Clean_FData', stat ))
                  exit ScanLoop
               end if
            end if
            if ( acquire_syndadj ) then
               call Clean_SynDAdj ( SynDAdj, stat )
               if ( stat   .ne. No_Error ) then
                  call WPErr ( MyName, ErrStat ( 'Clean_SynDAdj', stat))
                  exit ScanLoop
               end if
            end if

         else ! Otherwise exit loop
              ! -------------------
            exit ScanLoop
         end if
      end do ScanLoop

!     Make sure that the data is sorted properly
!     ------------------------------------------
      if ( acquire_fdata   .and. stat .eq. No_Error ) then
         call SortR_FData   ( FData,  stat  )
         if ( stat   .ne. No_Error) then
            call WPErr ( MyName, ErrStat ( 'SortR_FData',   stat ))
         end if
      end if
      if ( acquire_syndadj .and. stat .eq. No_Error ) then
         call SortR_SynDAdj ( SynDAdj, stat )
         if ( stat   .ne. No_Error ) then
            call WPErr ( MyName, ErrStat ( 'SortR_SynDAdj', stat ))
         end if
      end if

!     Clean up from error status
!     --------------------------
      if ( stat .ne. No_Error ) then
         call Clean_Header  ( Header  )
         call Clean_FData1  ( FData1  )
         if ( acquire_fdata   ) call Clean_FData   ( FData   )
         if ( acquire_syndadj ) call Clean_SynDAdj ( SynDAdj )
         return
      end if

      return
      end subroutine GetRC_All

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_Header () --- Get RaobCore header information from a text line
! 
! !DESCRIPTION:
!     This routine retrieves header data from a text line as read from
!     the input file and initializes the data structure for the header
!     information. This routine is low level.
!
! !INTERFACE:
      subroutine Get_Header ( TextLine, Header, stat )
      use m_AtoX,     only : AtoI, AtoF
      use m_AdvError, only : WPErr, ItoA, ErrStat
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*),        intent (in) ::
     .   TextLine             ! Text line containing the header info
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_Header ), intent (inout) ::     
     .   Header               ! Header information
!
! !OUTPUT PARAMETERS: 
      integer,                  intent (out) ::
     .   stat                 ! Return status
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     20Feb2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_Header'

      integer :: NTokens, NTokens_Min, iBeg, iEnd, iToken, LToken
      integer :: iChar, LLine, NLevels
      integer :: INum, NStn, NLev
      real    :: RNum
      character (len=len_trim(TextLine)) :: Token, VerTag, CNStn

      LLine = len_trim ( TextLine )

      iEnd    = 0 
      iToken  = 0
      NLevels = 0

!     Scan the line ...
!     -----------------
      ScanLoop : do iChar = 1, LLine

!        ... for the next token
!        ----------------------
         iBeg   = verify ( TextLine ( iEnd + 1 : LLine ), BLK ) + iEnd

!        ... and exit loop if there are no more tokens
!        ---------------------------------------------
         if ( iEnd  .ge. iBeg ) exit ScanLoop

!        ... otherwise determine the string length 
!        -----------------------------------------
         LToken = scan   ( TextLine ( iBeg : LLine ), BLK ) - 1
         if ( LToken .lt. 0 ) LToken = LLine - iBeg + 1

!        ... and end position of the token
!        ---------------------------------
         iEnd   = iBeg   + LToken - 1

!        ... before processing the token as ...
!        --------------------------------------
         iToken = iToken + 1
         Token  = TextLine ( iBeg : iEnd )
         if      ( iToken .eq. 1 ) then  ! ... the version tag
            VerTag = Token
         else if ( iToken .eq. 2 ) then  ! ... the number of stations
            CNStn  = Token
         else if ( iToken .eq. 3 ) then  ! ... the number of levels
            NLev   = AtoI ( Token, stat )
            if ( stat .ne. 0 ) NLev = -1
            if ( stat .ne. 0 .or.
     .           NLev .lt. 0 ) then
               call WPErr ( MyName, 'Bad value for the number of '
     .                     //       'levels (='
     .                     // trim ( Token ) // ')' )
               stat = Bad_Header
               return
            else

!              Initialize the header
!              ---------------------
               call Init_Header  ( NLev, Header, stat )
               if ( stat .ne. 0 ) then
                  call WPErr ( MyName, ErrStat ( 'Init_Header', stat ))
                  return
               end if

!              ... and convert the number of stations to an integer
!              ---------------------------------------------------- 
               NStn   = AtoI ( CNStn, stat )
               if ( stat .ne. 0 .or.
     .              NStn .lt. 0 ) then
                  call WPErr ( MyName, 'Bad value for the number of '
     .                        //       'stations (= '
     .                        // trim ( CNStn ) // ') \C' )
                  call Clean_Header ( Header )
                  stat = Bad_Header
                  return
               end if
               Header % NStn       = NStn
               Header % NLevels    = NLev
               Header % VersionTag = VerTag
               NLevels = 0
            end if
         else                            ! ... an entry to the list of
            RNum = AtoF ( Token ( : LToken ), stat ) ! pressure levels
            if ( stat .ne. 0 .or.
     .           RNum .lt. 0 ) then
               call WPErr ( MyName, 'Bad pressure level (='
     .                            // trim ( Token ) // ') ' )
               call Clean_Header ( Header )
               stat = Bad_Header
               return
            end if

!           Store the list
!           --------------
            NLevels = NLevels + 1
            if ( NLevels .le. Header % NLevelsMax ) then
               Header  % Levels ( NLevels ) = RNum
               if ( NLevels .gt. 1 ) then
                  Header % Adj_Def = trim ( Header % Adj_Def ) // BLK
     .                            // Break_Def
               else
                  Header % Adj_Def = Break_Def
               end if
            end if
         end if
      end do ScanLoop

!     Check to ensure that sufficient number of header tokens exist
!     -------------------------------------------------------------
      NTokens_Min = NHTokens_Basic + NLevels
      if ( iToken .lt. NTokens_Min ) then
         call WPErr ( MyName, 'The number of header tokens (= '
     .                     //  trim ( ItoA ( iToken )) 
     .                     // ') is less than the required amount (='
     .                     //  trim ( ItoA ( NTokens_Min )) // ')' ) 
         call Clean_Header ( Header )
         stat = Bad_Header
         return
      end if

!     Check to ensure that the number of tokens for
!     pressure levels is consistent with header information
!     ------------------------------------------------------
      if ( NLevels .ne. Header % NLevels ) then
         call WPErr ( MyName, 'The number of pressure level entries (='
     .                     //  trim ( ItoA ( NLevels ))
     .                     // ') is inconsistent with the value (='
     .                     //  trim ( ItoA ( Header % NLevels )) 
     .                     // ') given in the header ' )
         call Clean_Header ( Header )
         stat = Bad_Header
         return
      end if

      return
      end subroutine Get_Header

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Copy_Header () --- Copy contents of one header to another
! 
! !DESCRIPTION:
!     This routine copies the contents of the data structure of type
!     RaobCore_Header.  The pointers in the output argument are assumed
!     to have been initialized by the allocation statement.
!
! !INTERFACE:
!
      subroutine Copy_Header ( HeaderIn, HeaderOut )
!
! !INPUT PARAMETERS: 
      implicit NONE
      type ( RaobCore_Header ), intent (in)    :: HeaderIn
!
! !OUTPUT PARAMETERS: 
      type ( RaobCore_Header ), intent (inout) :: HeaderOut
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     21Feb2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------

      integer :: NLevels, NLevelsMax

      NLevelsMax = size ( HeaderOut %  Levels )
      NLevels    = min  ( HeaderIn  % NLevels, NLevelsMax )
      HeaderOut  % FileName   = HeaderIn % FileName
      HeaderOut  % VersionTag = HeaderIn % VersionTag
      HeaderOut  % Adj_Def    = HeaderIn % Adj_Def
      HeaderOut  % NStn       = HeaderIn % NStn
      HeaderOut  % NLevelsMax =            NLevelsMax
      HeaderOut  % NLevels    = HeaderIn % NLevels
      HeaderOut  % Levels ( : NLevels )
     .                        = HeaderIn %  Levels ( : NLevels )

      return
      end subroutine Copy_Header
!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_Stn1Info () --- Get RaobCore information for one station from a text line
! 
! !DESCRIPTION:
!     This routine retrieves info for one station from a text line
!     as read from the input file.  This routine is low level.
!
! !INTERFACE:
      subroutine Get_Stn1Info ( TextLine, Header, Stn1Info, stat )
      use m_AtoX,     only : AtoI, AtoF
      use m_AdvError, only : WPErr, ItoA, ErrStat
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*),          intent (in) ::
     .   TextLine               ! Text line containing the header info
      type ( RaobCore_Header ),   intent (in) ::
     .   Header                 ! Header information
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_Stn1Info ), intent (inout) ::     
     .   Stn1Info               ! Station information
!
! !OUTPUT PARAMETERS: 
      integer,                    intent (out)   ::
     .   stat                   ! Return status
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     20Feb2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_Stn1Info'

      integer :: NTokens, NTokens_Min, iBeg, iEnd, iToken, LToken
      integer :: iChar, LLine, INum, NStn, iBeg_StnID, iChar_StnID
      real    :: RNum
      character (len=len_trim(TextLine)) :: Token
      character (len=LStnID) :: StnID

      LLine = len_trim ( TextLine )

      iEnd    = 0 
      iToken  = 0

!     Initialize the data structure 
!     -----------------------------
      call Init_Stn1Info ( Stn1Info, stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Init_Stn1Info', stat ))
         return
      end if

!     Scan the line ...
!     -----------------
      ScanLoop : do iChar = 1, LLine

!        ... for the next token
!        ----------------------
         iBeg   = verify ( TextLine ( iEnd + 1 : LLine ), BLK ) + iEnd

!        ... and exit loop if there are no more tokens
!        ---------------------------------------------
         if ( iEnd  .ge. iBeg ) exit ScanLoop

!        ... otherwise determine the string length 
!        -----------------------------------------
         LToken = scan   ( TextLine ( iBeg : LLine ), BLK ) - 1
         if ( LToken .lt. 0 ) LToken = LLine - iBeg + 1

!        ... and end position of the token
!        ---------------------------------
         iEnd = iBeg + LToken - 1

!        ... before processing the token as ...
!        --------------------------------------
         iToken = iToken + 1
         Token  = TextLine ( iBeg : iEnd )
         if      ( iToken .eq. 1 ) then  ! ... the file index number
            NStn = Header % NStn
            INum = AtoI ( Token ( : LToken ), stat )
            if ( stat .ne. 0 .or.
     .           INum .lt. 1 .or.
     .           INum .gt. NStn ) then
               call WPErr ( MyName, 'Bad value for the station '
     .                         //   'index number (='
     .                         //    trim ( Token ) // ')'
     .                         // '\n   Valid range = [1,'
     .                         //    trim ( ItoA ( NStn )) // '] ' )
               stat = Bad_Record
               return
            end if
            Stn1Info % Indx = INum

         else if ( iToken .eq. 2 ) then  ! ... the station ID
            if ( LToken .gt. LStnID ) then
               call WPErr ( MyName, 'WMO station ID (='
     .                         //    trim ( Token ) // ') is too long' )
               stat = Bad_Record
               return
            end if
            iBeg_StnID = LStnID - LToken + 1
            do iChar_StnID = 1, iBeg_StnID - 1
               StnID ( iChar_StnID : iChar_StnID ) = StnID_Filler
            end do
            StnID ( iBeg_StnID : ) = Token
            Stn1Info % StnID       = StnID

         else if ( iToken .eq. 3 ) then  ! ... the latitude
            RNum = AtoF ( Token ( : LToken ), stat )
            if ( stat .ne. 0      .or.
     .           RNum .gt. LatMax .or.
     .           RNum .lt. LatMin ) then
               call WPErr ( MyName, 'Bad value for the station '
     .                       //     'latitude (='
     .                       //      trim ( Token ) // ')' )
               stat = Bad_Header
               return
            end if
            Stn1Info % Lat = RNum

         else if ( iToken .eq. 4 ) then  ! ... the longitude
            RNum = AtoF ( Token ( : LToken ), stat )
            if ( stat .ne. 0      .or.
     .           RNum .gt. LonMax .or.
     .           RNum .lt. LonMin ) then
               call WPErr ( MyName, 'Bad value for the station '
     .                       //     'longitude (='
     .                       //      trim ( Token ) // ')' )
               stat = Bad_Header
               return
            end if
            Stn1Info % Lon = RNum

         else if ( iToken .eq. 5 ) then  ! ... the flag indicating the
            if ( Token .ne. 'Y' .and.    !     existence of breaks
     .           Token .ne. 'N' ) then
               call WPErr ( MyName, 'Bad value for the flag (="'
     .                           //  trim ( Token )
     .                           // '") indicating the '
     .                           // 'existence of breaks.  '
     .                           // 'The flag must be "Y" or "N".' )
               stat = Bad_Record
               return
            end if
            Stn1Info % breaks_exist = Token .eq. 'Y'

         else if ( iToken .eq. 6 ) then  ! ... the number of breaks
            NStn = Header % NStn
            INum = AtoI ( Token ( : LToken ), stat )
            if ( stat .ne. 0 .or.
     .           INum .lt. 0 ) then
               call WPErr ( MyName, 'Bad value for the number of '
     .                       //     'station breaks (='
     .                       //      trim ( Token ) // ')' )
               stat = Bad_Record
               return
            end if
            Stn1Info % NBreaks = INum

         end if
      end do ScanLoop

!     Save text line
!     --------------
      Stn1Info % TextLine = TextLine

!     Check to ensure that the correct number of header tokens exist
!     --------------------------------------------------------------
      NTokens_Min = NSTokens_Basic
      if ( iToken .ne. NTokens_Min ) then
         call WPErr ( MyName, 'The number of tokens in the record (='
     .                     //  trim ( ItoA ( iToken )) 
     .                     // ') for station information is not '
     .                     // 'equal to the expected number (='
     .                     //  trim ( ItoA ( NTokens_Min )) // ')' ) 
         stat = Bad_Record
         return
      end if

      return
      end subroutine Get_Stn1Info

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_Breaks () --- Get RaobCore break information from a text line
! 
! !DESCRIPTION:
!     This routine retrieves break data from a text line as read from
!     the input file and stores the data in a target structure of type
!     RaobCore_FData1.  This routine is low level.
!
! !INTERFACE:
      subroutine Get_Breaks ( TextLine,  FData1, stat,
     .                        check_adj, reset )
      use m_AtoX,     only : AtoI
      use m_AdvError, only : WPErr, ItoA, ErrStat
      use m_SunAlt,   only : Check_Date
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*),        intent (in) ::
     .   TextLine             ! Text line containing the header info
      logical, optional,        intent (in) ::
     .   check_adj,           ! = .true. to check adjustments
     .   reset                ! = .true. to reset counters.
!                             !   Defaults for both arg = .false.
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_FData1 ), intent (inout) ::     
     .   FData1               ! Target data structure
!
! !OUTPUT PARAMETERS: 
      integer,                  intent (out) ::
     .   stat                 ! Return status
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     26Feb2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_Breaks'

      integer :: NTokens, NTokens_Min, iBeg, iEnd, iToken, LToken
      integer :: iChar, LLine, INum, NYMD, NYMD_Last, NYMD_00Z
      integer :: iBeg_List, NAdj, NLevels
      integer :: NSBreaks_00Z, NSBreaks_12Z, iBreak, NBreaks
      logical :: reset_counters, check_adjustments
      real    :: RNum
      real, dimension (0) :: Adj
      character (len=len_trim(TextLine)) :: Token
      character (len=LDate) :: SynDate
      character (len=2)     :: SynTime
      character (len=255)   :: Message

!     Implement options
!     -----------------
      reset_counters    = .false.
      if ( present ( reset )) reset_counters = reset
      if ( reset_counters ) then
         FData1 % Breaks_00Z % NTBreaks = 0
         FData1 % Breaks_12Z % NTBreaks = 0
      end if
      check_adjustments = .false.
      if ( present ( check_adj )) check_adjustments = check_adj

!     Initialize counters
!     -------------------
      LLine   = len_trim ( TextLine )
      iEnd    = 0 
      iToken  = 0

!     Scan the line ...
!     -----------------
      ScanLoop : do iChar = 1, LLine

!        ... for the next token
!        ----------------------
         iBeg   = verify ( TextLine ( iEnd + 1 : LLine ), BLK ) + iEnd

!        ... and exit loop if there are no more tokens
!        ---------------------------------------------
         if ( iEnd  .ge. iBeg ) exit ScanLoop

!        ... before processing the token as ...
!        --------------------------------------
         iToken = iToken + 1
         if      ( iToken .eq. 1 ) then  ! ... synoptic data/time of break
            LToken  = LDate + 2
            iEnd    = iBeg  + LToken - 1
            Token   = TextLine ( iBeg : iEnd )
            SynDate = Token (           : LDate )
            SynTime = Token ( LDate + 1 : LDate + 2 )

            NYMD    = AtoI ( SynDate, stat )
            if ( stat .ne. 0 ) then
               call WPErr ( MyName, 'Bad synoptic date/time (='
     .                         //    trim ( Token ) // ')'
     .                         // '\n   Station record = '
     .                         //       trim ( FData1 % Stn1Info
     .                                       % TextLine ))
               stat = Bad_Record
               return
            end if

            if ( Check_Date ( NYMD, Message ) .ne. 0 ) then
               call WPErr ( MyName, trim ( Message )
     .              // '\n\C   Date tag       = ' // trim ( SynDate )
     .              //   '\n   Station record = '
     .              //         trim ( FData1 % Stn1Info % TextLine ))
               stat = Bad_Record
               return
            end if
            if      ( SynTime .ne. '00' .and.
     .                SynTime .ne. '12' ) then
               call WPErr ( MyName, 'Bad synoptic date/time (='
     .                         //    trim ( Token ) // ')'
     .                         // '\n   Station record = '
     .                         //    trim ( FData1 % Stn1Info
     .                                    % TextLine ))
               stat = Bad_Record
               return
            end if

         else                            ! ... list of levels
            iBeg      = iEnd  + 1
            LToken    = LLine - iBeg + 1
            iEnd      = LLine
            iBeg_List = iBeg
            exit ScanLoop                ! The remaining tokens are to be
                                         !   processed as one string.
         end if
      end do ScanLoop

!     Check to ensure that the correct number of header tokens exist
!     --------------------------------------------------------------
      NTokens_Min = NBTokens_Basic + 1
      if ( iToken .lt. NTokens_Min ) then
         call WPErr ( MyName, 'The number of tokens in the record (='
     .                //       trim ( ItoA ( iToken )) 
     .                //      ') is less than the required number '
     .                // '\n   Station record = '
     .                //       trim ( FData1 % Stn1Info % TextLine ))
         stat = Bad_Record
         return
      end if

!     Store into data stucture
!     ------------------------ 
      NSBreaks_00Z = FData1 % Breaks_00Z % NTBreaks 
      NSBreaks_12Z = FData1 % Breaks_12Z % NTBreaks 

!     ... data at 00 UTC
!     ------------------
      if      ( SynTime .eq. '00' ) then
         if ( NSBreaks_00Z .gt. NSBreaks_12Z ) then
            call WPErr ( MyName, 'Bad synoptic date/time (='
     .                      //    SynDate // SynTime // ').  Expecting '
     .                      //   'a record valid at 12 UTC '
     .                      // '\n   Station record = '
     .                      //    trim ( FData1 % Stn1Info
     .                                 % TextLine ))
            stat = Missing_Record
            return
         end if
         NSBreaks_00Z =       NSBreaks_00Z + 1
         iBreak       = min ( NSBreaks_00Z,
     .                        FData1 % Breaks_00Z % NTBrMax )
         FData1 % Breaks_00Z % NTBreaks              = NSBreaks_00Z
         FData1 % Breaks_00Z % BreakDates ( iBreak ) = NYMD 
         FData1 % Breaks_00Z % AdjData    ( iBreak )
     .                = TextLine ( iBeg : LLine )

!     ... and at 12 UTC
!     -----------------
      else if ( SynTime .eq. '12' ) then
         if ( NSBreaks_12Z .ge. NSBreaks_00Z ) then
            call WPErr ( MyName, 'Bad synoptic date/time (='
     .                      //    SynDate // SynTime // ').  Expecting '
     .                      //   'a record valid at 00 UTC '
     .                      // '\n   Station record = '
     .                      //    trim ( FData1 % Stn1Info
     .                                 % TextLine ))
            stat = Missing_Record
            return
         end if
         iBreak   = min ( NSBreaks_00Z, FData1 % Breaks_00Z % NTBrMax )
         NYMD_00Z = FData1 % Breaks_00Z % BreakDates ( iBreak )
         if ( NYMD .ne. NYMD_00Z ) then
            call WPErr ( MyName, 'Bad synoptic date/time (='
     .                      //    trim ( Token ) // ').  Expecting '
     .                      //   'the date tag to be '
     .                      //    trim ( ItoA ( NYMD_00Z ))
     .                      // '\n   Station record = '
     .                      //    trim ( FData1 % Stn1Info
     .                                 % TextLine ))
            stat = Missing_Record
            return
         end if

         NSBreaks_12Z =       NSBreaks_12Z + 1
         iBreak       = min ( NSBreaks_12Z,
     .                        FData1 % Breaks_12Z % NTBrMax )
         FData1 % Breaks_12Z % NTBreaks              = NSBreaks_12Z
         FData1 % Breaks_12Z % BreakDates ( iBreak ) = NYMD 
         FData1 % Breaks_12Z % AdjData    ( iBreak )
     .                = TextLine ( iBeg : LLine )
      end if

!     If desired, check the adjustments
!     ---------------------------------
      if ( check_adjustments ) then
         call Get_Adj ( TextLine ( iBeg : LLine ), NAdj, Adj, stat )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'Get_Adj', stat )
     .                     // '\n   Station record = '
     .                     //    trim ( FData1 % Stn1Info
     .                                % TextLine ))
            return
         end if
         NLevels = FData1 % Header % NLevels
         if ( NAdj .ne. NLevels ) then
            call WPErr ( MyName, 'The number of adjustments (='
     .                      //    trim ( ItoA ( NAdj ))
     .                      //   ') is not equal to the number of '
     .                      //   'levels listed in the header (='
     .                      //    trim ( ItoA ( NLevels )) // ').'
     .                      // '\n   Station record = '
     .                      //    trim ( FData1 % Stn1Info
     .                                 % TextLine ))
            stat = Bad_Record
            return
         end if
      end if

!     Ensure that the breaks are sorted according to time
!     ----------------------------------------------------
c      NBreaks = FData1 % Stn1Info % NBreaks
c      if ( NSBreaks_12Z .eq. NBreaks .and.
c     .     NSBreaks_12Z .gt. 1 ) then
c         sorted    = .true.
c         NYMD_Last =  FData1 % Breaks_00Z % BreakDates ( 1 )
c         do iBreak =  2, NBreaks
c            NYMD      =  FData1 % Breaks_12Z % BreakDates ( iBreak )
c            sorted    =  sorted .and. NYMD .gt. NYMD_Last
c            if ( .not. sorted ) exit
c            NYMD_Last =  NYMD
c         end do
c      end if

c      if ( .not. sorted ) then

c      end if

      return
      end subroutine Get_Breaks

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_Adj () --- Get list of RaobCore temperature adjustments from a text line.
! 
! !DESCRIPTION:
!     This routine retrieves a list of RaobCore temperature adjustments
!     from a text line and stores the information in an array of floating
!     point numbers.  This routine is low level.
!     as read from the input file.  This routine is low level.
!
! !INTERFACE:
      subroutine Get_Adj ( TextLine, NAdj, Adj, stat, reverse )
      use m_AtoX,     only : AtoF
      use m_AdvError, only : WPErr, ItoA, ErrStat
!
!INPUT PARAMETERS: 
      implicit NONE
      character (len=*),          intent (in) ::
     .   TextLine               ! Text line containing the header info
      logical, optional,          intent (in) ::
     .   reverse                ! = .true. return the adjustments in reverse
!                               !   order.  Default: reverse = .true.
! !OUTPUT PARAMETERS: 
      integer,                    intent (out) ::
     .   NAdj                   ! The number of adjustments retrieved
      real, dimension (:),        intent (out) ::
     .   Adj                    ! The retrieved adjustments
      integer,                    intent (out) ::
     .   stat                   ! Return status
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     09Mar2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_Adj'

      logical :: reverse_order
      integer :: NTokens, NTokensMax, iBeg, iEnd, iToken, LToken
      integer :: iChar, iDash, LLine, iAdj, iAdj1
      real    :: RNum
      character (len=len_trim(TextLine)) :: Token

      reverse_order = .false.
      if ( present ( reverse )) reverse_order = reverse

      LLine      = len_trim ( TextLine )

      iEnd       = 0
      iToken     = 0
      NTokensMax = size ( Adj )

!     Scan the line ...
!     -----------------
      ScanLoop : do iChar = 1, LLine

!        ... for the next token
!        ----------------------
         iBeg = verify ( TextLine ( iEnd + 1 : LLine ), BLK ) + iEnd

!        ... and exit loop if there are no more tokens
!        ---------------------------------------------
         if ( iEnd  .ge. iBeg ) exit ScanLoop

!        ... otherwise determine the string length 
!        -----------------------------------------
         LToken = scan   ( TextLine ( iBeg : LLine ), BLK ) - 1
         if ( LToken .lt. 0 ) LToken = LLine - iBeg + 1

!        ... and end position of the token
!        ---------------------------------
         iEnd = iBeg + LToken - 1

!        ... check for extra dash (i.e. minus sign) as a token separator
!        ---------------------------------------------------------------
         iDash  = scan   ( TextLine ( iBeg + 1 : iEnd ), Dash )
         if ( iDash .gt. 0 ) then
            LToken = iDash
            iEnd   = iBeg  + LToken - 1
         end if

!        ... before processing the token as ...
!        --------------------------------------
         iToken = iToken + 1
         Token  = TextLine ( iBeg : iEnd )

!        ... temperature adjustment
!        --------------------------
         RNum = AtoF ( Token ( : LToken ), stat )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, 'Bad adjustment value (='
     .                         // trim ( Token ) // ') ' )
            stat = Bad_Record
            return
         end if

!        Store the list
!        --------------
         iAdj = iToken
         if ( reverse_order ) iAdj = NTokensMax - iToken + 1
         if ( iAdj .ge. 1 .and.
     .        iAdj .le. NTokensMax ) then
            Adj ( iAdj ) = RNum
         end if

      end do ScanLoop

!     Save number of values
!     ---------------------
      NAdj = iToken

!     Ensure that the first value is in the first output array element
!     ----------------------------------------------------------------
      if ( reverse_order .and. NAdj .lt. NTokensMax ) then
         iAdj1 = NTokensMax - NAdj + 1
         do iAdj = 1, NAdj
            Adj ( iAdj ) = Adj ( iAdj + iAdj1 - 1 )
         end do
      end if

      return
      end subroutine Get_Adj

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Copy_to_FData () --- Copy data from FData1 to FData data structure
! 
! !DESCRIPTION:
!     This routine copies FData1 info for one station to the data 
!     structure FData
!
! !INTERFACE:
      subroutine Copy_to_FData ( FData1, FData, stat )
      use m_AdvError, only : WPErr, ItoA, ErrStat
!
! !INPUT PARAMETERS: 
      implicit NONE
      type ( RaobCore_FData1 ),   intent (in) ::
     .   FData1                 ! Input data structure for one station
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_FData ),    intent (inout) ::     
     .   FData                  ! Output data structure
!
! !OUTPUT PARAMETERS: 
      integer,                    intent (out) ::
     .   stat                   ! Return status
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     28Feb2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------
      integer :: iBreak, iTBreak, Loc, NStn
      integer :: NSBreaks, NSBreaksMax, NTBreaks, NTBreaksMax

      stat = No_Error

      NStn        = FData  % StnInfo  % NStn + 1
      if ( NStn .eq. 1 ) then
         Loc      = 1
      else
         Loc      = FData  % StnNav   % Loc     ( NStn - 1 )
     .            + FData  % StnNav   % NBreaks ( NStn - 1 )
      end if
      NSBreaks    = FData1 % Stn1Info % NBreaks

      FData % StnInfo % NStn             = NStn
      FData % StnInfo % StnID   ( NStn ) = FData1 % Stn1Info % StnID
      FData % StnInfo % Indx    ( NStn ) = FData1 % Stn1Info % Indx
      FData % StnInfo % Lat     ( NStn ) = FData1 % Stn1Info % Lat
      FData % StnInfo % Lon     ( NStn ) = FData1 % Stn1Info % Lon
      FData % StnNav  % Loc     ( NStn ) = Loc
      FData % StnNav  % NBreaks ( NStn ) = NSBreaks

      NSBreaksMax = FData1 % Breaks_00Z % NTBrMax 
      NTBreaksMax = FData  % Breaks_00Z % NTBrMax

      iTBreak     = Loc
      do iBreak = 1, NSBreaks
         if ( iBreak  .le. NSBreaksMax  .and.
     .        iTBreak .le. NTBreaksMax ) then
            FData     % Breaks_00Z % BreakDates ( iTBreak )
     .       = FData1 % Breaks_00Z % BreakDates (  iBreak )
            FData     % Breaks_00Z % AdjData    ( iTBreak )
     .       = FData1 % Breaks_00Z % AdjData    (  iBreak )
            FData     % Breaks_12Z % BreakDates ( iTBreak )
     .       = FData1 % Breaks_12Z % BreakDates (  iBreak )
            FData     % Breaks_12Z % AdjData    ( iTBreak )
     .       = FData1 % Breaks_12Z % AdjData    (  iBreak )
         end if
         iTBreak = iTBreak + 1
      end do

      NTBreaks   = iTBreak - 1
      FData % Breaks_00Z % NTBreaks = NTBreaks
      FData % Breaks_12Z % NTBreaks = NTBreaks

      return
      end subroutine Copy_to_FData

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Copy_to_SynDAdj () --- Copy data from FData1 to SynDAdj data structure
! 
! !DESCRIPTION:
!     This routine copies FData1 info for one station to the data 
!     structure SynDAdj
!
! !INTERFACE:
      subroutine Copy_to_SynDAdj ( FData1, SynDate, SynDAdj, stat )
      use m_Range,    only : Range2
      use m_AdvError, only : WPErr, ItoA, ErrStat
!
! !INPUT PARAMETERS: 
      implicit NONE
      type ( RaobCore_FData1  ),   intent (in) ::
     .   FData1                  ! Input data structure for one station
      integer,                     intent (in) ::
     .   SynDate                 ! Synoptic date of desired data
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_SynDAdj ),   intent (inout) ::     
     .   SynDAdj                 ! Output data structure
!
! !OUTPUT PARAMETERS: 
      integer,                     intent (out) ::
     .   stat                    ! Return status
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     28Feb2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------

      integer :: iBreak, NSBreaks, NStn, SynNav(3)
      integer, dimension (:), pointer :: SynDates

      stat = No_Error

      NStn = SynDAdj % StnInfo % NStn + 1

      SynDAdj % StnInfo % NStn           = NStn
      SynDAdj % StnInfo % StnID ( NStn ) = FData1 % Stn1Info % StnID
      SynDAdj % StnInfo % Indx  ( NStn ) = FData1 % Stn1Info % Indx
      SynDAdj % StnInfo % Lat   ( NStn ) = FData1 % Stn1Info % Lat
      SynDAdj % StnInfo % Lon   ( NStn ) = FData1 % Stn1Info % Lon

      NSBreaks = FData1 % Stn1Info % NBreaks

      if      ( NSBreaks .eq. 0 ) then
         SynDAdj    % SynAdj_00Z % HistFlag ( NStn )
     .     = DefUsed
         SynDAdj    % SynAdj_12Z % HistFlag ( NStn )
     .     = DefUsed
         SynDAdj    % SynAdj_00Z % AdjData  ( NStn )
     .     = FData1 % Header % Adj_Def
         SynDAdj    % SynAdj_12Z % AdjData  ( NStn )
     .     = FData1 % Header % Adj_Def

      else if ( NSBreaks .eq. 1 ) then
         SynDAdj    % SynAdj_00Z % HistFlag ( NStn )
     .     = FromFile
         SynDAdj    % SynAdj_12Z % HistFlag ( NStn )
     .     = FromFile
         SynDAdj    % SynAdj_00Z % AdjData  ( NStn )
     .     = FData1 % Breaks_00Z % AdjData  ( 1 )
         SynDAdj    % SynAdj_12Z % AdjData  ( NStn )
     .     = FData1 % Breaks_12Z % AdjData  ( 1 )

      else
         SynDates => FData1 % Breaks_00Z % BreakDates ( : NSBreaks )
         SynNav   =  Range2 ( SynDates, SynDate )
         iBreak   =  max ( SynNav (3), 1 )
         SynDAdj    % SynAdj_00Z % HistFlag ( NStn )
     .     = FromFile
         SynDAdj    % SynAdj_12Z % HistFlag ( NStn )
     .     = FromFile
         SynDAdj    % SynAdj_00Z % AdjData  ( NStn )
     .     = FData1 % Breaks_00Z % AdjData  ( iBreak )
         SynDAdj    % SynAdj_12Z % AdjData  ( NStn )
     .     = FData1 % Breaks_12Z % AdjData  ( iBreak )

      end if

      return
      end subroutine Copy_to_SynDAdj

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Sort_StnInfo () --- Sort the arrays in StnInfo according to the WMO station ID
! 
! !DESCRIPTION:
!     This routine sorts the arrays in StnInfo according the WMO
!     station ID.  This routine is low level.
!
! !INTERFACE:
      subroutine Sort_StnInfo ( This, Indx, presorted, stat )
      use m_AdvError, only : WPErr, ErrStat
      use m_RadSort,  only : IndexSort
!
! !INPUT PARAMETERS:
      implicit NONE
      type ( RaobCore_StnInfo ),  intent (in)    ::
     .   This                   ! Input data structure
!
! !INPUT/OUTPUT PARAMETERS:
      integer,                    intent (inout) ::
     .   Indx (:)               ! Sorting indicies.
!
! !OUTPUT PARAMETERS: 
      logical,                    intent (out)   ::
     .   presorted              ! = .true. if the data was already sorted
      integer,                    intent (out)   ::
     .   stat                   ! Return status
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     05Mar2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Sort_StnInfo'

      integer :: iStn, NStn, iiStn
      character (len=LStnID) :: StnID, StnID_Last
      character (len=LStnID), dimension (:), pointer :: StnIDs

      stat = No_Error

      NStn = This % NStn
      if ( NStn .eq. 0 ) return

!     Test to determine if the arrays sorted according to WMO station ID
!     ------------------------------------------------------------------
      presorted     = .true.
      StnID_Last =  This % StnID ( 1 )
      StnIDs     => This % StnID ( : NStn )
      do iStn = 2, NStn
         iiStn = Indx    ( iStn )
         StnID = StnIDs ( iiStn )
         if ( llt ( StnID, StnID_Last )) then
            presorted = .false.
            exit
         end if
         StnID_Last = StnID
      end do

!     Return if they are properly sorted
!     ----------------------------------
      if ( presorted ) return

!     ... sort if they are not
!     ------------------------
      call IndexSort ( Indx, StnIDs, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'IndexSort', stat ))
         stat = Alloc_Error
         return
      end if

      return
      end subroutine Sort_StnInfo

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: SortR_FData () --- Sort and rearrange the arrays in FData according to the WMO station ID
! 
! !DESCRIPTION:
!     This routine sorts and rearranges the arrays in FData according
!     the WMO station ID.  This routine is low level.
!
! !INTERFACE:
      subroutine SortR_FData ( This, stat )
      use m_AdvError, only : WPErr, ErrStat
!
! !INPUT/OUTPUT PARAMETERS:
      implicit NONE
      type ( RaobCore_FData ),    intent (inout) ::
     .   This                   ! Input data structure with the arrays in
                                !   the substructures StnInfo (except
!                               !   Indx) and StnNav rearranged on output
! !OUTPUT PARAMETERS: 
      integer,                    intent (out)   ::
     .   stat                   ! Return status
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     05Mar2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName = MyModule // '::SortR_FData'

      integer :: NStn
      logical :: presorted

      stat = No_Error

      NStn = This % StnInfo % NStn
      if ( NStn .eq. 0 ) return

!     Get the sorting indices
!     -----------------------
      call Sort_StnInfo ( This % StnInfo,
     .                    This % StnInfo % Indx,
     .                    presorted, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'Sort_StnInfo', stat ))
         stat = Alloc_Error
         return
      end if

!     ... and return if already sorted
!     --------------------------------
      if ( presorted ) return

!     ... or otherwise rearrange
!     --------------------------
      call Reorder_StnInfo ( This % StnInfo,
     .                       This % StnInfo % Indx,
     .                       This % StnInfo )
      call Reorder_StnNav  ( This % StnNav,
     .                       This % StnInfo % Indx,
     .                       This % StnNav )

      return
      end subroutine SortR_FData

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: SortR_SynDAdj () --- Sort and rearrange the arrays in SynDAdj according to the WMO station ID
! 
! !DESCRIPTION:
!     This routine sorts and rearranges the arrays in SynDAdj according
!     the WMO station ID.  This routine is low level.
!
! !INTERFACE:
      subroutine SortR_SynDAdj ( This, stat )
      use m_AdvError, only : WPErr, ErrStat
!
! !INPUT/OUTPUT PARAMETERS:
      implicit NONE
      type ( RaobCore_SynDAdj ),  intent (inout) ::
     .   This                   ! Input data structure with the arrays in
                                !   all substructures (except
!                               !   StnInfo % Indx) on output
! !OUTPUT PARAMETERS: 
      integer,                    intent (out)   ::
     .   stat                   ! Return status
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     05Mar2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::SortR_SynDAdj'

      integer :: NStn
      logical :: presorted

      stat = No_Error

      NStn = This % StnInfo % NStn
      if ( NStn .eq. 0 ) return

!     Get the sorting indices
!     -----------------------
      call Sort_StnInfo ( This % StnInfo,
     .                    This % StnInfo % Indx,
     .                    presorted, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'Sort_StnInfo', stat ))
         stat = Alloc_Error
         return
      end if

!     ... and return if already sorted
!     --------------------------------
      if ( presorted ) return

!     ... or otherwise rearrange
!     --------------------------
      call Reorder_StnInfo ( This % StnInfo,
     .                       This % StnInfo % Indx,
     .                       This % StnInfo )
      call Reorder_SynAdj  ( This % SynAdj_00Z,
     .                       This % StnInfo % Indx,
     .                       This % SynAdj_00Z )
      call Reorder_SynAdj  ( This % SynAdj_12Z,
     .                       This % StnInfo % Indx,
     .                       This % SynAdj_12Z )

      return
      end subroutine SortR_SynDAdj

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_StnInfo - Rearrange the data structure, StnInfo
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_StnInfo ( This1, Indx, This2, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      type (RaobCore_StnInfo), intent (in)    ::
     .   This1               ! Input data structure
      integer,                 intent (in)    ::
     .   Indx (:)            ! Sorting indices
      logical, optional,       intent (in)    ::
     .   reverse             ! = .true. to reverse the reordering 
                             !   Default: reverse = .false.
!
! !OUTPUT PARAMETERS:
      type (RaobCore_StnInfo), intent (inout) ::
     .   This2               ! Data structure with all arrays rearranged
!                            !   (except Indx)
!
! Note: size of Indx determines the size of the arrays in the data
!       structure
!
! !REVISION HISTORY:
!     05Mar2007  C. Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      integer :: iStn, NStn
      logical :: forward

      forward = .true.
      if ( present ( reverse )) forward = .not. reverse 
      NStn    = size ( Indx )

      if ( forward ) then
         This2 % StnID   ( : NStn )
     .      = (/( This1 % StnID   ( Indx ( iStn )), iStn = 1, NStn )/)
         This2 % Lat     ( : NStn )
     .      = (/( This1 % Lat     ( Indx ( iStn )), iStn = 1, NStn )/)
         This2 % Lon     ( : NStn )
     .      = (/( This1 % Lon     ( Indx ( iStn )), iStn = 1, NStn )/)

      else
         This2 % StnID   ( Indx ( : NStn ))
     .      = (/( This1 % StnID   ( iStn ), iStn = 1, NStn )/)
         This2 % Lat     ( Indx ( : NStn ))
     .      = (/( This1 % Lat     ( iStn ), iStn = 1, NStn )/)
         This2 % Lon     ( Indx ( : NStn ))
     .      = (/( This1 % Lon     ( iStn ), iStn = 1, NStn )/)
      end if 

      return
      end subroutine Reorder_StnInfo

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_StnNav - Rearrange the data structure, StnNav
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_StnNav ( This1, Indx, This2, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      type (RaobCore_StnNav),  intent (in)    ::
     .   This1               ! Input data structure
      integer,                 intent (in)    ::
     .   Indx (:)            ! Sorting indices
      logical, optional,       intent (in)    ::
     .   reverse             ! = .true. to reverse the reordering 
                             !   Default: reverse = .false.
!
! !OUTPUT PARAMETERS:
      type (RaobCore_StnNav),  intent (inout) ::
     .   This2               ! Data structure with all arrays rearranged
!
! Note: size of Indx determines the size of the arrays in the data
!       structure
!
! !REVISION HISTORY:
!     05Mar2007  C. Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      integer :: iStn, NStn
      logical :: forward

      forward = .true.
      if ( present ( reverse )) forward = .not. reverse 
      NStn    = size ( Indx )

      if ( forward ) then
         This2 % Loc     ( : NStn )
     .      = (/( This1 % Loc     ( Indx ( iStn )), iStn = 1, NStn )/)
         This2 % NBreaks ( : NStn )
     .      = (/( This1 % NBreaks ( Indx ( iStn )), iStn = 1, NStn )/)

      else
         This2 % Loc     ( Indx ( : NStn ))
     .      = (/( This1 % Loc     ( iStn ), iStn = 1, NStn )/)
         This2 % NBreaks ( Indx ( : NStn ))
     .      = (/( This1 % NBreaks ( iStn ), iStn = 1, NStn )/)
      end if 

      return
      end subroutine Reorder_StnNav

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_SynAdj - Rearrange the data structure, SynAdj
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_SynAdj ( This1, Indx, This2, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      type (RaobCore_SynAdj),   intent (in)    ::
     .   This1                ! Input data structure
      integer,                  intent (in)    ::
     .   Indx (:)             ! Sorting indices
      logical, optional,        intent (in)    ::
     .   reverse              ! = .true. to reverse the reordering 
                              !   Default: reverse = .false.
!
! !OUTPUT PARAMETERS:
      type (RaobCore_SynAdj),  intent (inout) ::
     .   This2               ! Data structure with all arrays rearranged
!
! Note: size of Indx determines the size of the arrays in the data
!       structure
!
! !REVISION HISTORY:
!     05Mar2007  C. Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      integer :: iStn, NStn
      logical :: forward

      forward = .true.
      if ( present ( reverse )) forward = .not. reverse 
      NStn    = size ( Indx )

      if ( forward ) then
         This2 % HistFlag ( : NStn )
     .      = (/( This1 % HistFlag ( Indx ( iStn )), iStn = 1, NStn )/)
         This2 % AdjData  ( : NStn )
     .      = (/( This1 % AdjData  ( Indx ( iStn )), iStn = 1, NStn )/)

      else
         This2 % HistFlag ( Indx ( : NStn ))
     .      = (/( This1 % HistFlag ( iStn ), iStn = 1, NStn )/)
         This2 % AdjData  ( Indx ( : NStn ))
     .      = (/( This1 % AdjData  ( iStn ), iStn = 1, NStn )/)
      end if 

      return
      end subroutine Reorder_SynAdj

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  ReadLn () --- Read one text line of CARDS data
! 
! !DESCRIPTION:
!     This routine retrieves one text line of text data.  If the given
!     character string has insufficient length, then the first portion
!     of the line is stored.  This routine is low level.
!
! !INTERFACE:
!
      subroutine ReadLn ( lu, LLine, Line, eor, eof, stat )
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,             intent (in)  ::
     .   lu                ! FORTRAN logical unit number
!
! !OUTPUT PARAMETERS: 
      integer,             intent (out) ::
     .   LLine             ! Line of line or number of character read
      character (len = *), intent (out) ::
     .   Line              ! Line of data
      logical,             intent (out)  ::
     .   eor,              ! = .true. if end-of-record has been reached
     .   eof               ! = .true. if end-of-file has been reached
      integer,             intent (out) ::
     .   stat              ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     20Feb2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------

!     Read line
!     ---------
      read ( unit    =  lu,
     .       fmt     = '(a)',
     .       advance = 'no',
     .       size    =  LLine,
     .       eor     =  10,
     .       end     =  20,
     .       iostat  =  stat ) Line

      eor = .false.
      eof = .false.
      read ( unit    =  lu,    ! Advance to next record
     .       fmt     = '()',
     .       advance = 'yes',
     .       end     =  20,
     .       iostat  =  stat )
      return

!     End of record
!     -------------
 10   continue
      if ( stat .lt. 0 ) stat = 0
      eor = .true.
      eof = .false.
      return

!     End of file
!     -----------
 20   continue
      if ( stat .lt. 0 ) stat = 0
      eor = .true.
      eof = .true.

      return
      end subroutine ReadLn

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_StnAdj_w1 () --- Get adjustments for given stations and a synoptic time and initialize the argument of derived type, RaobCore_StnAdj1
! 
! !DESCRIPTION:
!     This routine retrieves the adjustments for given stations and a synoptic
!     time from the data structure of derived type, RaobCore_SynDAdj.  This
!     routine also initilized the argument of derived type, RaobCore_StnAdj1.
!
! !INTERFACE:
!
      subroutine Get_StnAdj_w1 ( StnID, Lat, Lon, SynDAdj,
     .                           StnAdj1, StnAdj, stat )
      use m_AdvError,  only : WPErr, ErrStat
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*),         intent (in)    ::
     .   StnID    (:)          ! WMO station ID
      real,                      intent (in)    ::
     .   Lat      (:),         ! Latitude
     .   Lon      (:)          ! Longitude (-90=90W)
      type ( RaobCore_SynDAdj ), intent (in)    ::
     .   SynDAdj               ! Adjustment data valid for a synoptic date
!
! !OUTPUT PARAMETERS: 
      type ( RaobCore_StnAdj1 ), intent (inout) ::
     .   StnAdj1               ! Adjustment data for a station and synptic date.
      type ( RaobCore_StnAdj  ), intent (inout) ::
     .   StnAdj                ! Adjustment data for stations and a synoptic date.
      integer ::
     .   stat                  ! Returned error status
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Mar2007  Redder    Origional code.
! EOP
!..............................................................

      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_StnAdj_w1'
      integer :: NLevelsMax, NLevels, iLev

      call Get_StnAdj ( StnID, Lat, Lon, SynDAdj, StnAdj, stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Get_StnAdj', stat ))
         return
      end if

      NLevelsMax = StnAdj % NLevelsMax
      call Init_StnAdj1 ( NLevelsMax, StnAdj1, stat )
      if ( stat .ne. 0 ) then
         call WPErr ( MyName, ErrStat ( 'Init_StnAdj1', stat ))
         call Clean_StnAdj ( StnAdj )
         return
      end if

      NLevels = StnAdj  % NLevels
      do iLev = 1, NLevels
         StnAdj1 % Levels ( iLev ) = StnAdj % Levels ( iLev )
      end do
      StnAdj1 % NLevels = NLevels
      StnAdj1 % SynDate = StnAdj % SynDate

      return
      end subroutine Get_StnAdj_w1
!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_StnAdj () --- Get adjustments for given stations and a synoptic time
! 
! !DESCRIPTION:
!     This routine retrieves the adjustments for given stations and a synoptic
!     time from the data structure of derived type, RaobCore_SynDAdj.
!
! !INTERFACE:
!
      subroutine Get_StnAdj ( StnID, Lat, Lon, SynDAdj, StnAdj, stat )
      use m_Range,     only : Range
      use m_ellipsoid, only : InvSph
      use m_AdvError,  only : WPErr, ErrStat, ItoA
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*),         intent (in)    ::
     .   StnID    (:)          ! WMO station ID
      real,                      intent (in)    ::
     .   Lat      (:),         ! Latitude
     .   Lon      (:)          ! Longitude (-90=90W)
      type ( RaobCore_SynDAdj ), intent (in)    ::
     .   SynDAdj               ! Adjustment data valid for a synoptic date
!
! !OUTPUT PARAMETERS: 
      type ( RaobCore_StnAdj  ), intent (inout) ::
     .   StnAdj                ! Adjustment data for stations and a synoptic date
      integer ::
     .   stat                  ! Returned error status
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Mar2007  Redder    Origional code.
!     09Jul2007  Redder    Modified code to skip station with bad WMO ID
!                          rather than return to calling routine with 
!                          error status.
! EOP
!..............................................................

      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_StnAdj'
      logical :: reverse, better_station, DistMin_set
      integer :: HFlag, iLoc, iLoc2, iLocH, NDup
      integer :: NStn, NStn_RC, NAdj, iStn, iStn_RC
      integer :: StnNav (2), iText, iBeg, iEnd, iChar
      integer :: NLevelsMax, NLevelsMax2, NLevels, iLev, iiLev
      real    :: Lat1   (1), Lon1   (1), Lat2(1), Lon2(1)
      real    :: AZim12 (1), AZim21 (1), Dist(1), DistMin, Level
      character (len=1) :: Char
      character (len=LAdjData) :: AdjData
      character (len=LStnID) :: StnID_
      character (len=LStnID), dimension (:), pointer :: StnID_RC
      integer, pointer :: HistFlag (:), Indx_RC (:)
      real,    pointer :: Lat_RC   (:), Lon_RC  (:)

!     Initialize output data structure
!     --------------------------------
      NLevels = SynDAdj % Header % NLevels
      NStn    = min ( size ( StnID ), size ( Lat ), size ( Lon ))
      call Init_StnAdj ( NLevels, NStn, StnAdj, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'Init_StnAdj', stat ))
         return
      end if

!     Ensure that lower levels are listed first in output arrays
!     ----------------------------------------------------------
      reverse = SynDAdj % Header % Levels ( 1 ) .lt. 
     .          SynDAdj % Header % Levels ( NLevels )

!     Set internal pointers
!     ---------------------
      NStn_RC    =  SynDAdj % StnInfo    % NStn
      StnID_RC   => SynDAdj % StnInfo    % StnID    ( : NStn_RC )
      Indx_RC    => SynDAdj % StnInfo    % Indx     ( : NStn_RC )
      HistFlag   => SynDAdj % SynAdj_00Z % HistFlag ( : NStn_RC )
      Lat_RC     => SynDAdj % StnInfo    % Lat      ( : NStn_RC )
      Lon_RC     => SynDAdj % StnInfo    % Lon      ( : NStn_RC )

!     For each station in the input list ...
!     --------------------------------------
      do iStn = 1, NStn

!        Ensure that the input Station ID is valid
!        -----------------------------------------
         if ( len_trim ( StnID ( iStn )) .gt. LStnID ) then
            StnNav = (/ 1, 0 /)

         else

!           ... with no leading zeroes
!           --------------------------
            StnID_ = StnID  ( iStn )
            Char   = StnID_ ( 1 : 1 )
            if ( Char .eq. '0' ) then
               StnID_ ( 1:1 ) = BLK
               do iChar = 2, LStnID - 1
                  Char = StnID_ ( iChar : iChar )
                  if ( Char .ne. '0' ) exit
                  StnID_ ( iChar : iChar ) = BLK
               end do
            end if

!           ... locate the station ID
!           -------------------------
            StnNav =  Range  ( StnID_RC, StnID_ )
         end if

!        For the case when station is not found ...
!        ------------------------------------------
         NDup   =  StnNav ( 2 )
         if      ( NDup .eq. 0 ) then
            iLoc = 0

!        ... when the station is found
!        -----------------------------
         else if ( NDup .ge. 1 ) then
            iLoc = StnNav ( 1 )

!           ... when more that one station is found with the desired ID
!           -----------------------------------------------------------
            if ( NDup .gt. 1 ) then

!              ... find a station with adj data from file
!              ------------------------------------------
               iLocH = iLoc
               do iStn_RC = iLoc, iLoc + NDup - 1
                  HFlag = HistFlag ( iStn_RC ) 
                  if ( HFlag .eq. FromFile ) then
                     iLocH = iStn_RC
                     exit
                  end if
               end do

!              ... and choose the closest one with adj data from the file
!              ----------------------------------------------------------
               iLoc2       =  StnNav ( 1 )
               iLoc        =  iLocH
               DistMin_set = .false.
               do iStn_RC  = iLoc2, iLoc2 + NDup - 1
                  HFlag    =  HistFlag ( iStn_RC ) 
                  if ( HFlag   .eq. FromFile .and.
     .                 iStn_RC .ne. iLocH ) then
                     if ( .not. DistMin_set ) then
                        Lat1 (1) = Lat    ( iStn )
                        Lon1 (1) = Lon    ( iStn )
                        Lat2 (1) = Lat_RC ( iLocH )
                        Lon2 (1) = Lon_RC ( iLocH )
                        call InvSph ( Lat1, Lon1,   Lat2,   Lon2,
     .                                Dist, AZim12, AZim21 )
                        DistMin  = Dist ( 1 )
                     end if

                     Lat2 (1) = Lat_RC    ( iStn_RC )
                     Lon2 (1) = Lon_RC    ( iStn_RC )
                     call InvSph    ( Lat1, Lon1,   Lat2,   Lon2,
     .                                Dist, AZim12, AZim21 )
                     better_station = Dist ( 1 ) .lt. DistMin
                     if ( better_station ) then
                        DistMin = Dist ( 1 )
                        iLoc    = iStn_RC
                     end if
                  end if
               end do
            end if
         end if

!        Convert the adjustments from character to real
!        ----------------------------------------------
         iBeg = ( iStn - 1 ) * NLevels + 1
         iEnd =   iBeg       + NLevels - 1
         if ( iLoc .gt. 0 ) then

!           ... for 00 UTC data
!           -------------------
            AdjData =  SynDAdj % SynAdj_00Z % AdjData ( iLoc )
            call Get_Adj ( AdjData, NAdj,
     .                     StnAdj % Adj_00Z ( iBeg : iEnd ),
     .                     stat, reverse = reverse )
            if ( stat .ne. No_Error ) then
               call WPErr ( MyName, ErrStat ( 'Get_Adj', stat )
     .                  // '\C\n   Text = ' // trim ( AdjData )
     .                  //   '\n   Synoptic time of adj data = 00 UTC' )
               call Clean_StnAdj ( StnAdj )
               return
            end if
            if ( NAdj .ne. NLevels ) then
               call WPErr ( MyName, ' The number of tokens in the text ' 
     .                        //    '(=' // trim ( ItoA ( NAdj )) 
     .                        //    ') is not equal to the number of '
     .                        //    'levels (='
     .                        //     trim ( ItoA ( NLevels )) // ').'
     .                        // '\C\n   Station ID = '
     .                        //         trim ( StnID_ )
     .                        //   '\n   Text       = '
     .                        //         trim ( AdjData )
     .                        //   '\n   Synoptic time of adj data = '
     .                        //        '00 UTC' )
               call Clean_StnAdj ( StnAdj )
               stat = Bad_Record
               return
            end if

!           ... 12 UTC data
!           ---------------
            AdjData =  SynDAdj % SynAdj_12Z % AdjData ( iLoc )
            call Get_Adj ( AdjData, NAdj,
     .                     StnAdj % Adj_12Z ( iBeg : iEnd ),
     .                     stat, reverse = reverse )
            if ( stat .ne. No_Error ) then
               call WPErr ( MyName, ErrStat ( 'Get_Adj', stat )
     .                  // '\C\n   Text = ' // trim ( AdjData )
     .                  //   '\n   Synoptic time of adj data = 12 UTC' )
               call Clean_StnAdj ( StnAdj )
               return
            end if
            if ( NAdj .ne. NLevels ) then
               call WPErr ( MyName, ' The number of tokens in the text ' 
     .                        //    '(=' // trim ( ItoA ( NAdj ))
     .                        //    ') is not equal to the number of '
     .                        //    'levels (='
     .                        //     trim ( ItoA ( NLevels )) // ').'
     .                        // '\C\n   Station ID = '
     .                        //         trim ( StnID_ )
     .                        //   '\n   Text       = '
     .                        //         trim ( AdjData )
     .                        //   '\n   Synoptic time of adj data = '
     .                        //        '12 UTC' )
               call Clean_StnAdj ( StnAdj )
               stat = Bad_Record
               return
            end if

!           ... save station info to output data structure
!           ----------------------------------------------
            StnAdj % StnInfo % Indx  ( iStn ) = Indx_RC  ( iLoc )
            StnAdj % StnInfo % StnID ( iStn ) = StnID_RC ( iLoc )
            StnAdj % StnInfo % Lat   ( iStn ) = Lat_RC   ( iLoc )
            StnAdj % StnInfo % Lon   ( iStn ) = Lon_RC   ( iLoc )
         else

!           For the case when the station is not found
!           ------------------------------------------
            StnAdj % Adj_00Z ( iBeg : iEnd )  = 0.0    ! ... set adjustments
            StnAdj % Adj_12Z ( iBeg : iEnd )  = 0.0    !       to 0.0
            StnAdj % StnInfo % Indx  ( iStn ) = 0      ! ... set file index to 0
            StnAdj % StnInfo % StnID ( iStn ) = StnID_         ! ... use input
            StnAdj % StnInfo % Lat   ( iStn ) = Lat   ( iStn ) !      args
            StnAdj % StnInfo % Lon   ( iStn ) = Lon   ( iStn ) ! 
         end if
         StnAdj % NTAdj = StnAdj % NTAdj + NLevels
      end do

!     Save list of levels in the proper order
!     ---------------------------------------
      StnAdj % NLevels = NLevels
      if ( reverse ) then
         do iLev  = 1, NLevels
            Level = SynDAdj % Header % Levels ( iLev )
            iiLev = iLev
            if ( reverse ) iiLev = NLevels - iLev + 1
            StnAdj % Levels ( iiLev ) = Level
         end do
      end if

!     Save synoptic date to output data structure
!     -------------------------------------------
      StnAdj % SynDate = SynDAdj % SynAdj_00Z % SynDate

      return
      end subroutine Get_StnAdj
!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_StnAdj1 () --- Get adjustments for a given stations and synoptic time
! 
! !DESCRIPTION:
!     This routine retrieves the adjustments for given stations and a synoptic
!     time from the data structure of derived type, RaobCore_StnAdj and
!     copies the contents to the data stucture of type RaobCore_StnAdj1.
!     This routine assumes that output data sturcture was initialized by
!     the routine, Get_StnAdj_w1 currently with the other input data
!     structure of type RaobCore_StnAdj.
!
! !INTERFACE:
!
      subroutine Get_StnAdj1 ( iStn, StnAdj, StnAdj1 )
      use m_AdvError,  only : WPErr, ErrStat
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,                   intent (in)    ::
     .   iStn                  ! Array index number of station info
      type ( RaobCore_StnAdj  ), intent (in)    ::
     .   StnAdj                ! The input data structure
!
! !OUTPUT PARAMETERS: 
      type ( RaobCore_StnAdj1 ), intent (inout) ::
     .   StnAdj1               ! Adjustment data for a station and synptic date.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Mar2007  Redder    Origional code.
! EOP
!..............................................................

      integer :: NLevels, iLev, iAdj0, iAdj

      NLevels = StnAdj1 % NLevels
      iAdj0   = NLevels * ( iStn - 1 )
      do iLev = 1, NLevels
         iAdj = iLev + iAdj0
         StnAdj1 % Adj_00Z ( iLev ) = StnAdj % Adj_00Z ( iAdj )
         StnAdj1 % Adj_12Z ( iLev ) = StnAdj % Adj_12Z ( iAdj )
      end do
      StnAdj1 % Indx  = StnAdj % StnInfo % Indx  ( iStn )
      StnAdj1 % StnID = StnAdj % StnInfo % StnID ( iStn )
      StnAdj1 % Lat   = StnAdj % StnInfo % Lat   ( iStn )
      StnAdj1 % Lon   = StnAdj % StnInfo % Lon   ( iStn )

      return
      end subroutine Get_StnAdj1
!..............................................................

      end module m_RaobCore
*====================================================================
