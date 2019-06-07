!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_CARDS -- Utility routines for reading CARDS data set from NCDC
!
! !DESCRIPTION:
!     This module contains the utility routines for reading the CARDS
!     data set.
!
! !INTERFACE:
!
      module   m_CARDS
      use      m_SunAlt,  only : CDS_Julian => Julian
      use      m_RadData, only : NPDigits   => NPresDigits,
     .                           LTHist_File, LTHist_ObTime
      implicit NONE
      private	       ! except
      public :: 
     .   CARDS_data,   ! Derived type containing the cards synoptic data
     .   CARDS_meta,   ! ... the header or meta-data information
     .   CARDS_tsdata, ! Derived type containing the cards time-series data
     .   CARDS_tsmeta, ! ... the header or meta-data information
     .   CARDS_obs,    ! ... the observation values
     .   CARDS_qc,     ! ... and the corresponding quality control flags
     .   IMiss,        ! ... missing value for integer
     .   AMiss,        ! ... and real data in the header.
     .   LT_File,      ! ... launch time flag indicating value from file
     .   LT_ObTime     ! ... ... indicating value missing and set to ob time 
      public ::
     .   CDS_Select,   ! Selected by Julian hour
     .   CDS_Prep,     ! Sort by WMO ID and, if desired, select by Julian hour
     .   CDS_Acquire,  ! Get and sort CARDS data by station and time tag
     .   CDS_Get,      ! Read CARDS data from given file
     .   CDS_Put,      ! Write header information to given file.
     .   CDS_Nullify,  ! Nullify ...
     .   CDS_Clean,    ! ... reset and deallocate ...
     .   CDS_Init,     ! ... initialize and allocate CARDS data structure
     .   CDS_Reorder,  ! Reorders meta-data
     .   CDS_ETime,    ! Extract elapsed time from CARDS data structure
     .   CDS_Index,    ! Select a report based on CARDS header info
     .   CDS_Julian    ! Calculate Julian day (see module m_SunAlt)

      interface CDS_Select
         module procedure
     .      Select_Data
      end interface
      interface CDS_Prep
         module procedure
     .      Prep_Data
      end interface
      interface CDS_Nullify
         module procedure
     .      Nullify_Data,   ! for entire synoptic data structure
     .      Nullify_Meta,   ! ... meta-data structure
     .      Nullify_TSData, ! for entire time-series data structure
     .      Nullify_TSMeta  ! ... meta-data structure
      end interface
      interface CDS_Clean
         module procedure
     .      Clean_Data,     ! for entire synoptic data structure
     .      Clean_Meta,     ! ... meta-data structure
     .      Clean_TSData,   ! for entire time-series data structure
     .      Clean_TSMeta    ! ... meta-data structure
      end interface
      interface CDS_Init
         module procedure
     .      Init_Data,      ! for entire synoptic data structure
     .      Init_Meta,      ! ... meta-data structure
     .      Init_TSData,    ! for entire time-series data structure
     .      Init_TSMeta     ! ... meta-data structure
      end interface
      interface CDS_Acquire
         module procedure
     .      Acquire_Data,   ! acquire and sort synoptic data
     .      Acquire_TSData  ! ... time-series data
      end interface
      interface CDS_Get
         module procedure
     .      Get_Data,       ! for entire synoptic data structure
     .      Get_Meta,       ! ... synoptic meta-data structure
     .      Get_Header,     ! ... header information
     .      Get_TSData,     ! for entire time-series data structure
     .      Get_TSMeta      ! ... time-series meta-data structure
      end interface
      interface CDS_Put
         module procedure
     .      Put_Header      ! for header information
      end interface
      interface CDS_Reorder
         module procedure
     .      Reorder_Meta1,
     .      Reorder_Meta2,
     .      Reorder_TSMeta1,
     .      Reorder_TSMeta2 
      end interface
      interface CDS_Index
         module procedure
     .      CDS_TSIndex
      end interface
      interface CDS_ETime
         module procedure
     .      CDS_ETimeTS,
     .      CDS_ETimeS 
      end interface
!
! !REVISION HISTORY:
!     14May2002  C. Redder  Original code
!     08Aug2002  C. Redder  Removed parameter statements defining EOL and BLK
!     13Dec2002  C. Redder  Set string length for WMO station ID via constant
!                           LStnID.  Removed generic interface, CDS_Unsort.
!                           Renamed generic interface, CDS_Sort to CDS_Reorder,
!     06Jan2003  C. Redder  Changed use m_Julian to use m_SunAlt
!     30Jan2003  C. Redder  Replaced the component Date and Time with
!                           DateHr in the data structure CARDS_meta. 
!     04Feb2004  C. Redder  Renamed the component, Lev, to P and added the
!                           components, P_m and P_e to the data structure,
!                           CARDS_obs.  Renamed the component, Lev to P in
!                           the data structure, CARDS_qc. Set launch time in
!                           seconds.  Changed the component, ETime, from
!                           integer to real in the data structure, CARDS_obs.
!     30Aug2006  C. Redder  Added the public routine, CDS_ETime.  Added
!                           the component, Mask, to the data structure,
!                           CARDS_qc.
!     20Dec2006  C. Redder  Replaced the component DateHr with Date and
!                           Hour in the data structure CARDS_meta. 
!     29Jan2007  C. Redder  Added the components, SynDate and SynHr,
!                           to the data structure CARDS_meta.
!     14Mar2007  C. Redder  Add the types, CARDS_tsdata and CARDS_tsmeta,
!                           to accomodate time-series data.  Added
!                           the appropriate module procedures to the
!                           interfaces, CDS_Nullify, CDS_Clean, CDS_Init, 
!                           and CDS_Reorder to handle the time-series
!                           data.  Made the routine CDS_Acquire into 
!                           an interface.
!     30Apr2007 C. Redder   Initialize the data structures and added the
!                           components, FileName, LTHist and QC_Rep, to
!                           the variables of types CARDS_meta and
!                           CARDS_tsmeta.  Combined the components,
!                           Date and Hour, to DateHr in the variable
!                           type CARDS_meta.  Made CDS_ETime a generic
!                           interface.
!EOP
!-----------------------------------------------------------------

!     Special characters
!     ------------------
      character (  len  =   1 ), parameter ::
     .   BLK = achar (  32 ),  ! blank
     .   EOL = achar (  10 )   ! end of line  mark

!     Constants for missing
!     ---------------------
      real,    parameter ::
     .   AMiss    =  10.0e10   ! Missing buffer data
      integer, parameter ::
     .   IMiss    = -10000     ! Missing integer attribute 

!     Error status and handling
!     -------------------------
      character (len=*), parameter :: MyModule = 'm_CARDS'

!     Parameters for the CARDS records
!     --------------------------------
      integer, parameter ::
     .   MaxNRecLev  = 175,     ! Maximum number of levels in a record
     .   LHeader     = 108,     ! String length for header section
     .   LRecLev     = 56,      ! ... and each level in the CARDS report
     .   LStnID      = 5        ! ... and for each WMO station ID
      integer, parameter ::
     .   MaxLRec     = LHeader  + MaxNRecLev * LRecLev, ! Maximum record length
     .   MaxNRec     = 100000   ! Maximum number of records in a file

!     Data parameters
!     ---------------
      integer, parameter ::
     .   NPresDigits = NPDigits ! Number of significant digits in pres ob

!     Data source table (kx) for all NCDC observation types
!     -----------------------------------------------------
      integer, parameter :: ObsTypeMax = 12
      integer, dimension  ( ObsTypeMax ) ::
     .    kxTable = (/ 7, 12, 0, 10, 0, 285, 8, 0, 11, 11, 7, 7 /)

!     Flags for launch time history
!     -----------------------------
      integer, parameter ::
     .   LT_File         = LTHist_File,
     .   LT_ObTime       = LTHist_ObTime

!     Status codes
!     ------------
      integer, parameter ::
     .   No_Error        = 0,   ! Valid error status; no error
     .   No_LUAvail      = 1,   ! No FORTRAN logical unit number
     .   Open_Error      = 2,   ! Error in opening a file
     .   Read_Error      = 3,   ! Error in reading a file
     .   Write_Error     = 4,   ! Error in writing to a file
     .   Buffer_Error    = 5,   ! Buffer error (not enough space)
     .   Alloc_Error     = 6,   ! Allocation error
     .   Dealloc_Error   = 7,   ! Deallocation error
     .   Close_Error     = 8,   ! Error in closing a file
     .   Missing_Record  = 9,   ! Missing record 
     .   Bad_Record      = 10,  ! Bad record
     .   Bad_Level       = 11,  ! Bad level
     .   Bad_Template    = 12,  ! Bad file template
     .   Bad_Argument    = 13,  ! Bad input subroutine argument
     .   No_Header       = 14   ! No header in file
      integer, parameter ::
     .   Accept          = 0

!     Data structures for sounding header information including ...
!     -------------------------------------------------------------
      type CARDS_meta
         character (len=255) ::
     .      FileName    = BLK   ! ... name of CARDS data file
         integer ::
     .      SynDate     = 0,    ! ... synoptic date (YYYYMMDD format)
     .      SynHr       = 0,    ! ... and hour (HH format)
     .      NReportsMax = 0,    ! ... the maximum and ...
     .      NReports    = 0,    ! ... the actual number of reports
     .      NTLevelsMax = 0,    ! ... the maximum and ...
     .      NTLevels    = 0     ! ... the actual total number of levels among ...
         character (len=LStnID), pointer, dimension (:) :: !  all soundings
     .      StnID    => null()  ! ... WMO station ID
         real,              pointer, dimension (:) ::
     .      Lat      => null(), ! ... latitude  (deg)
     .      Lon      => null(), ! ... longitude (deg, -90 = 90W)
     .      Elev     => null()  ! ... station elevation (m)
         integer,           pointer, dimension (:) ::
     .      DateHr   => null(), ! ... date and hour (YYYYMMDDHH format)
     .      LTime    => null(), ! ... launch time (in seconds since date/hour)
     .      LTHist   => null(), ! ... launch time history flag
     .      kx       => null(), ! ... DAO data source index
     .      Type     => null(), ! ... NCDC instrument type
     .      QC_Rep   => null(), ! ... QC for entire report (see below)
     .      NLevels  => null(), ! ... number of levels
     .      Loc      => null()  ! ... array location of lowest level data
                                !     in type CARDS_obs and CARDS_qc
      end type

!     ... time-series sounding header information including ...
!     ---------------------------------------------------------
      type CARDS_tsmeta
         character (len=255) ::
     .      FileName    = BLK   ! ... name of CARDS data file
         integer ::
     .      NReportsMax = 0,    ! ... the maximum and ...
     .      NReports    = 0,    ! ... the actual number of reports
     .      NTLevelsMax = 0,    ! ... the maximum and ...
     .      NTLevels    = 0     ! ... the actual total number of levels among
         character (len=LStnID), pointer, dimension (:) :: !  all soundings
     .      StnID    => null()  ! ... WMO station ID
         real,              pointer, dimension (:) ::
     .      Lat      => null(), ! ... latitude  (deg)
     .      Lon      => null(), ! ... longitude (deg, -90 = 90W)
     .      Elev     => null()  ! ... station elevation (m)
         integer,           pointer, dimension (:) ::
     .      DateHr   => null(), ! ... date and hour (YYYYMMDDHH format)
     .      LTime    => null(), ! ... launch time (in seconds since date/hour)
     .      LTHist   => null(), ! ... launch time history flag
     .      kx       => null(), ! ... DAO data source index
     .      Type     => null(), ! ... NCDC instrument type
     .      QC_Rep   => null(), ! ... QC for entire report (see below)
     .      NLevels  => null(), ! ... number of levels
     .      Loc      => null()  ! ... array location of lowest level data
                                !     in type CARDS_obs and CARDS_qc
      end type

!         Notes: (1) QC_Rep < 0 if the entire report is unacceptable
!                           = 0 ... acceptable (default value)
!                           > 0 ... preferable (higher the number, the more
!                                               preferable)
!                (2) For type CARDS_tsmeta, LTime and DateHr are set so
!                           that 0 <= LTime < 3559.

!     ... observed values for ...
!     ---------------------------
      type CARDS_obs
         integer ::
     .      NTLevelsMax = 0,    ! ... the maximum and ...
     .      NTLevels    = 0     ! ... the actual total number of levels among 
         integer,           pointer, dimension (:) ::     !   all reports
     .      P_m      => null(), ! Mantessa and
     .      P_e      => null()  ! ... exponent of pressure.
         real,              pointer, dimension (:) ::
     .      ETime    => null(), ! ... elapse time (in sec since launch time)
     .      P        => null(), ! ... pressure level (hPa or mb)
     .      Height   => null(), ! ... geopotential height (m)
     .      Temp     => null(), ! ... temperature (in deg K)
     .      RH       => null(), ! ... relative humidity (in %)
     .      DewPt    => null(), ! ... dew point (in deg K)
     .      UWind    => null(), ! ... zonal wind component(in m/sec)
     .      VWind    => null()  ! ... meridional wind component (in m/sec)
      end type

!     ... the corresponding quality control (QC) values.  For the
!         list of valid QC values, see the internal routine, CARDS_QC
!     ---------------------------------------------------------------
      type CARDS_qc
         integer ::
     .      NTLevelsMax = 0, NTLevels = 0
         logical,           pointer, dimension (:) ::
     .      Mask     => null()  ! Initialized to .true. in the routine, Get_CDat.
         integer,           pointer, dimension (:) ::
     .      ETime => null(), P     => null(), Height => null(),
     .      Temp  => null(), RH    => null(), DewPt  => null(),
     .      UWind => null(), VWind => null()
      end type

!     Entire report for synoptic data
!     -------------------------------
      type CARDS_data
          type ( CARDS_meta   ) :: Meta
          type ( CARDS_obs    ) :: Obs
          type ( CARDS_qc     ) :: QC
      end type

!     ... and time-series data
!     ------------------------
      type CARDS_tsdata
          type ( CARDS_tsmeta ) :: TSMeta
          type ( CARDS_obs    ) :: Obs
          type ( CARDS_qc     ) :: QC
      end type


!     Data structures for use in by routine Prep_Data
!     and all other routine called by Prep__Data
!     -----------------------------------------------
      integer, parameter ::
     .   NBRecMax = 51,        ! Max number of blocks of data
     .   NRepMax  = 10         ! Max number of reports

!     ... for storing records (i.e NCDC data blocks)
!     ----------------------------------------------
      type CARDS_records
         integer :: NBRec,
     .              LRec    ( NBRecMax ),
     .              NRecLev ( NBRecMax ),
     .              NAddRec ( NBRecMax )
         character (len=MaxLRec + 255) :: Record  ( NBRecMax )
         character (len=255)           :: InFiles ( NBRecMax )
         character (len=LStnID  +  10) :: ProfTag ( NBRecMax )
      end type

!     ... for arranging records as reports
!     ------------------------------------ 
      type CARDS_repinfo
         integer ::       NReports, NRepRec ( NRepMax ),
     .                              RecList ( NRepMax, NBRecMax )
         character (len=255)     :: InFiles ( NRepMax )
         character (len=3)       :: CAdd    ( NRepMax, NBRecMax )    
         character (len=LHeader) :: Header  ( NRepMax )
      end type

      contains

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Data - Initializes pointers in the entire data structure
!
! !INTERFACE:
      subroutine Nullify_Data ( CData )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_data ), intent( inout ) ::
     .   CData  ! CARDS data
!
! !REVISION HISTORY:
!     06May2002  C. Redder  Initial code
!EOP
!.................................................................

      call Nullify_Meta ( CData % Meta )
      call Nullify_Obs  ( CData % Obs  )
      call Nullify_QC   ( CData % QC   )

      return
      end subroutine Nullify_Data

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_TSData - Initializes pointers in the entire time-series data structure
!
! !INTERFACE:
      subroutine Nullify_TSData ( TSData )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_tsdata ), intent( inout ) ::
     .   TSData  ! CARDS time-series data
!
! !REVISION HISTORY:
!     14Mar2007  C. Redder  Initial code
!EOP
!.................................................................

      call Nullify_TSMeta ( TSData % TSMeta )
      call Nullify_Obs    ( TSData % Obs    )
      call Nullify_QC     ( TSData % QC     )

      return
      end subroutine Nullify_TSData

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Meta - Initializes pointers in the meta-data structure
!
! !INTERFACE:
      subroutine Nullify_Meta ( Meta )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_meta ), intent( inout ) ::
     .   Meta  ! CARDS meta-data
!
! !REVISION HISTORY:
!     06May2002  C. Redder  Initial code
!     30Jan2003  C. Redder  Replaced the component Date and Time with
!                           DateHr in the data structure CARDS_meta. 
!     20Dec2006  C. Redder  Replaced the component DateHr with Date and
!                           Hour in the data structure CARDS_meta. 
!     24Apr2007  C. Redder  Added the component LTHist and QC_Rep
!                           Combined the components, Date and Hour,
!                           to DateHr.
!EOP
!.................................................................

      nullify ( Meta % StnID,
     .          Meta % Lat,
     .          Meta % Lon,
     .          Meta % Elev,
     .          Meta % DateHr,
     .          Meta % LTime,
     .          Meta % LTHist,
     .          Meta % kx,
     .          Meta % Type,
     .          Meta % QC_Rep,
     .          Meta % NLevels,
     .          Meta % Loc )

      return
      end subroutine Nullify_Meta

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_TSMeta - Initializes pointers in the time-series meta-data structure
!
! !INTERFACE:
      subroutine Nullify_TSMeta ( TSMeta )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_tsmeta ), intent( inout ) ::
     .   TSMeta  ! CARDS meta-data
!
! !REVISION HISTORY:
!     14Mar2007  C. Redder  Initial code
!     24Apr2007  C. Redder  Added the components LTHist and QC_Rep
!EOP
!.................................................................

      nullify ( TSMeta % StnID,
     .          TSMeta % Lat,
     .          TSMeta % Lon,
     .          TSMeta % Elev,
     .          TSMeta % DateHr,
     .          TSMeta % LTime,
     .          TSMeta % LTHist,
     .          TSMeta % kx,
     .          TSMeta % Type,
     .          TSMeta % QC_Rep,
     .          TSMeta % NLevels,
     .          TSMeta % Loc )

      return
      end subroutine Nullify_TSMeta

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Obs - Initializes pointers in the obs-data structure
!
! !INTERFACE:
      subroutine Nullify_Obs ( Obs )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_obs ), intent( inout ) ::
     .   Obs  ! CARDS observation data
!
! !REVISION HISTORY:
!     06May2002  C. Redder  Initial code
!     04Feb2004  C. Redder  Renamed component, Lev, to P and added the
!                           components, P_m and P_e to the data structure,
!                           CARDS_obs.
!EOP
!.................................................................

      nullify ( Obs % P_m,
     .          Obs % P_e,
     .          Obs % ETime,
     .          Obs % P,
     .          Obs % Height,
     .          Obs % Temp,
     .          Obs % RH,
     .          Obs % DewPt,
     .          Obs % UWind,
     .          Obs % VWind )

      return
      end subroutine Nullify_Obs

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_QC - Initializes pointers in the QC data structure
!
! !INTERFACE:
      subroutine Nullify_QC ( QC )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_qc ), intent( inout ) ::
     .   QC  ! CARDS quality control data
!
! !REVISION HISTORY:
!     06May2002  C. Redder  Initial code
!     04Feb2004  C. Redder  Renamed component, Lev, to P.
!     30Aug2006  C. Redder  Added the component, Mask, to the data
!                           structure of the input argument.
!EOP
!.................................................................

      nullify ( QC % Mask,
     .          QC % ETime,
     .          QC % P,
     .          QC % Height,
     .          QC % Temp,
     .          QC % RH,
     .          QC % DewPt,
     .          QC % UWind,
     .          QC % VWind )

      return
      end subroutine Nullify_QC

!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_Data - Initialize and deallocates data structure
!
! !INTERFACE:
!
      subroutine Clean_Data ( CData, stat )
!
! !USES:
      use m_AdvError, only : PErr, Dealloc, ErrStat
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_data ), intent (inout) ::
     .   CData ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     06May2002  C. Redder  Initial code
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     08Jan2003  C. Redder  Made the argument, stat, optional
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter :: MyName = MyModule // '::Clean_Data'
      logical :: check_stat

      check_stat = present ( stat )
      call Clean_Meta ( CData % Meta, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName, ErrStat ( 'Clean_Meta' ) // '\W' )
            return
         end if
      end if

      call Clean_Obs  ( CData % Obs,  stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName, ErrStat ( 'Clean_Obs'  ) // '\W' )
            return
         end if
      end if

      call Clean_QC   ( CData % QC,   stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName, ErrStat ( 'Clean_QC'   ) // '\W' )
            return
         end if
      end if

      return
      end subroutine  Clean_Data
!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_TSData - Initialize and deallocates time-series data structure
!
! !INTERFACE:
!
      subroutine Clean_TSData ( TSData, stat )
!
! !USES:
      use m_AdvError, only : PErr, Dealloc, ErrStat
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_tsdata ), intent (inout) ::
     .   TSData ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,     intent (out  ) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status code from the
                !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     14Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .    MyName = MyModule // '::Clean_TSData'
      logical :: check_stat

      check_stat = present ( stat )
      call Clean_TSMeta ( TSData % TSMeta, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName, ErrStat ( 'Clean_TSMeta' ) // '\W' )
            return
         end if
      end if

      call Clean_Obs    ( TSData % Obs,    stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName, ErrStat ( 'Clean_Obs'    ) // '\W' )
            return
         end if
      end if

      call Clean_QC     ( TSData % QC,     stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName, ErrStat ( 'Clean_QC'     ) // '\W' )
            return
         end if
      end if

      return
      end subroutine  Clean_TSData
!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_Meta - Initializes and deallocates meta-data structure
!
! !INTERFACE:
      subroutine Clean_Meta ( Meta, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_meta ), intent (inout) ::
     .   Meta  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     06May2002  C. Redder  Initial code
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     08Jan2003  C. Redder  Made the argument, stat, optional and fixed
!                           bug to ensure that the returned status is
!                           initialized.
!     24Apr2007  C. Redder  Added the components, FileName, LTHist and
!                           QC_Rep.  Combined the components, Date and
!                           Hour to DateHr.
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter :: MyName = MyModule // '::Clean_Meta'
      logical :: check_stat
      integer :: istat

      Meta % FileName    = BLK
      Meta % NReports    = 0
      Meta % NReportsMax = 0
      Meta % NTLevels    = 0
      Meta % NTLevelsMax = 0
      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

!     Deallocate memory
!     -----------------
      if ( associated ( Meta % StnID    ))
     .     deallocate ( Meta % StnID,   stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Meta%StnID'   ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Meta % Lat      ))
     .     deallocate ( Meta % Lat,     stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Meta%Lat'     ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Meta % Lon      ))
     .     deallocate ( Meta % Lon,     stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Meta%Lon'     ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Meta % Elev     ))
     .     deallocate ( Meta % Elev,    stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Meta%Elev'    ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Meta % DateHr   ))
     .     deallocate ( Meta % DateHr,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Meta%DateHr'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Meta % LTime    ))
     .     deallocate ( Meta % LTime,   stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Meta%LTime'   ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Meta % LTHist   ))
     .     deallocate ( Meta % LTHist,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Meta%LTHist'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Meta % kx       ))
     .     deallocate ( Meta % kx,      stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Meta%kx'      ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Meta % Type     ))
     .     deallocate ( Meta % Type,    stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Meta%Type'    ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Meta % QC_Rep   ))
     .     deallocate ( Meta % QC_Rep,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Meta%QC_Rep'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Meta % NLevels  ))
     .     deallocate ( Meta % NLevels, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Meta%NLevels' ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Meta % Loc      ))
     .     deallocate ( Meta % Loc,     stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Meta%Loc'     ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      call Nullify_Meta ( Meta )
      return
      end subroutine  Clean_Meta
!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_TSMeta - Initializes and deallocates time-series meta-data structure
!
! !INTERFACE:
      subroutine Clean_TSMeta ( TSMeta, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_tsmeta ), intent (inout) ::
     .   TSMeta ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,     intent (out  ) ::
     .   stat   ! Error return code.  If no error, then zero is returned.
                !   If not present, then the status code from the
                !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     14Mar2007  C. Redder  Initial code
!     24Apr2007  C. Redder  Added the components, FileName LTHist and QC_Rep
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_TSMeta'
      logical :: check_stat
      integer :: istat

      TSMeta % FileName    = BLK
      TSMeta % NReports    = 0
      TSMeta % NReportsMax = 0
      TSMeta % NTLevels    = 0
      TSMeta % NTLevelsMax = 0
      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

!     Deallocate memory
!     -----------------
      if ( associated ( TSMeta % StnID    ))
     .     deallocate ( TSMeta % StnID,   stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               Dealloc ( istat, 'TSMeta%StnID'   ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( TSMeta % Lat      ))
     .     deallocate ( TSMeta % Lat,     stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               Dealloc ( istat, 'TSMeta%Lat'     ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( TSMeta % Lon      ))
     .     deallocate ( TSMeta % Lon,     stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               Dealloc ( istat, 'TSMeta%Lon'     ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( TSMeta % Elev     ))
     .     deallocate ( TSMeta % Elev,    stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               Dealloc ( istat, 'TSMeta%Elev'    ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( TSMeta % DateHr   ))
     .     deallocate ( TSMeta % DateHr,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               Dealloc ( istat, 'TSMeta%DateHr'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( TSMeta % LTime    ))
     .     deallocate ( TSMeta % LTime,   stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               Dealloc ( istat, 'TSMeta%LTime'   ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( TSMeta % LTHist     ))
     .     deallocate ( TSMeta % LTHist,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               Dealloc ( istat, 'TSMeta%LTHist'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( TSMeta % kx       ))
     .     deallocate ( TSMeta % kx,      stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               Dealloc ( istat, 'TSMeta%kx'      ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( TSMeta % Type     ))
     .     deallocate ( TSMeta % Type,    stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               Dealloc ( istat, 'TSMeta%Type'    ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( TSMeta % QC_Rep   ))
     .     deallocate ( TSMeta % QC_Rep,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               Dealloc ( istat, 'TSMeta%QC_Rep'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( TSMeta % NLevels  ))
     .     deallocate ( TSMeta % NLevels, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               Dealloc ( istat, 'TSMeta%NLevels' ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( TSMeta % Loc      ))
     .     deallocate ( TSMeta % Loc,     stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               Dealloc ( istat, 'TSMeta%Loc'     ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      call Nullify_TSMeta ( TSMeta )
      return
      end subroutine  Clean_TSMeta
!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_Obs - Initializes and deallocates obs-data structure
!
! !INTERFACE:
      subroutine Clean_Obs ( Obs, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_obs ), intent (inout) ::
     .   Obs   ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,  intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     06May2002  C. Redder  Initial code
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     08Jan2003  C. Redder  Made the argument, stat, optional and fixed
!                           bug to ensure that the returned status is
!                           initialized.
!     04Feb2004  C. Redder  Renamed component, Lev, to P and added the
!                           components, P_m and P_e to the data structure,
!                           CARDS_obs.
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter :: MyName = MyModule // '::Clean_Obs'
      logical :: check_stat
      integer :: istat

      Obs % NTLevels    = 0
      Obs % NTLevelsMax = 0
      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      if ( associated ( Obs  % P_m    ))
     .     deallocate ( Obs  % P_m,   stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Obs%P_m'    ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Obs  % P_e    ))
     .     deallocate ( Obs  % P_e,   stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Obs%P_e'    ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Obs  % ETime  ))
     .     deallocate ( Obs  % ETime, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Obs%ETime'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Obs % P       ))
     .     deallocate ( Obs % P,      stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Obs%P'      ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Obs % Height  ))
     .     deallocate ( Obs % Height, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Obs%Height' ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Obs % Temp    ))
     .     deallocate ( Obs % Temp,   stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Obs%Temp'   ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Obs % RH      ))
     .     deallocate ( Obs % RH,     stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Obs%RH'     ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Obs % DewPt   ))
     .     deallocate ( Obs % DewPt,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Obs%DewPt'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Obs % UWind   ))
     .     deallocate ( Obs % UWind,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Obs%UWind'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( Obs % VWind   ))
     .     deallocate ( Obs % VWind,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'Obs%VWind'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      call Nullify_Obs ( Obs )
      return
      end subroutine  Clean_Obs

!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_QC - Initializes and deallocates QC data structure
!
! !INTERFACE:
      subroutine Clean_QC ( QC, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_qc ), intent (inout) ::
     .   QC    ! Quality contro (QC) stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional, intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     06May2002  C. Redder  Initial code
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     08Jan2003  C. Redder  Made the argument, stat, optional and fixed
!                           bug to ensure that the returned status is
!                           initialized.
!     04Feb2004  C. Redder  Renamed component, Lev, to P.
!     30Aug2006  C. Redder  Added the component, Mask, to the data
!                           structure of the input argument.
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter :: MyName = MyModule // '::Clean_QC'
      logical :: check_stat
      integer :: istat

      QC % NTLevels    = 0
      QC % NTLevelsMax = 0
      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      if ( associated ( QC % Mask    ))
     .     deallocate ( QC % Mask,   stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'QC%Mask'   ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( QC % ETime   ))
     .     deallocate ( QC % ETime,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'QC%ETime'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( QC % P       ))
     .     deallocate ( QC % P,      stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'QC%P'      ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( QC % Height  ))
     .     deallocate ( QC % Height, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'QC%Height' ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( QC % Temp    ))
     .     deallocate ( QC % Temp,   stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'QC%Temp'   ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( QC % RH      ))
     .     deallocate ( QC % RH,     stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'QC%RH'     ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( QC % DewPt   ))
     .     deallocate ( QC % DewPt,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'QC%DewPt'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( QC % UWind   ))
     .     deallocate ( QC % UWind,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'QC%UWind'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      if ( associated ( QC % VWind   ))
     .     deallocate ( QC % VWind,  stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName, Dealloc ( istat, 'QC%VWind'  ) // '\W' )
         stat = Dealloc_Error
         return
      end if

      call Nullify_QC ( QC )
      return
      end subroutine  Clean_QC

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_Data - Initializes structure for CARDS data
!
! !INTERFACE:
      subroutine Init_Data ( NReports, NTLevels, CData, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,             intent (in)    ::
     .   NReports,       ! Number of reports
     .   NTLevels        ! Total number of levels among all reports
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_data ), intent (inout) ::
     .   CData           ! CARDS data structure
!
! !OUTPUT PARAMETERS:
      integer,             intent (out)   ::
     .   stat     ! Error status code.  If no error, then zero is returned
!
! !REVISION HISTORY:
!     02May2002  C. Redder  Initial code
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     08Jan2003  C. Redder  Changed call to Clean_Meta and Clean_Obs
!                           to ignore the returned status code.
!     15Feb2007  C. Redder  Removed the optional attribute from the 
!                           argument, stat.
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: MyName = MyModule // '::Init_Data'

      call Init_Meta ( NReports, NTLevels, CData % Meta, stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Meta' ) // '\W' )
         return
      end if

      call Init_Obs  (          NTLevels, CData % Obs,  stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Obs'  ) // '\W' )
         call Clean_Meta ( CData % Meta )
         return
      end if

      call Init_QC   (          NTLevels, CData % QC,   stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_QC'   ) // '\W' )
         call Clean_Meta ( CData % Meta )
         call Clean_Obs  ( CData % Obs  )
         return
      end if

      return
      end subroutine Init_Data

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_TSData - Initializes structure for CARDS time-series data
!
! !INTERFACE:
      subroutine Init_TSData ( NReports, NTLevels, TSData, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,             intent   (in)    ::
     .   NReports,       ! Number of reports
     .   NTLevels        ! Total number of levels among all reports
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_tsdata ), intent (inout) ::
     .   TSData           ! CARDS data structure
!
! !OUTPUT PARAMETERS:
      integer,             intent   (out)   ::
     .   stat     ! Error status code.  If no error, then zero is returned
!
! !REVISION HISTORY:
!     14Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_TSData'

      call Init_TSMeta ( NReports, NTLevels, TSData % TSMeta, stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_TSMeta' ) // '\W' )
         return
      end if

      call Init_Obs    (          NTLevels, TSData % Obs,     stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Obs'    ) // '\W' )
         call Clean_TSMeta ( TSData % TSMeta )
         return
      end if

      call Init_QC     (          NTLevels, TSData % QC,      stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_QC'     ) // '\W' )
         call Clean_TSMeta ( TSData % TSMeta )
         call Clean_Obs    ( TSData % Obs    )
         return
      end if

      return
      end subroutine Init_TSData

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_Meta - Initialize and allocate structure for CARDS meta-data
!
! !INTERFACE:
      subroutine Init_Meta ( NReports, NTLevels, Meta, stat )
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,            intent (in)    ::
     .   NReports, ! Number of reports
     .   NTLevels  ! Total number of levels among all reports
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_meta ), intent (inout) ::
     .   Meta     ! meta-data structure
!
! !OUTPUT PARAMETERS:
      integer,            intent (out)   ::
     .   stat     ! Error status code.  If no error, then zero is returned
!
! !REVISION HISTORY:
!     06May2002  C. Redder  Initial code
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     08Jan2003  C. Redder  Changed call to Clean_Meta to ignore returned
!                           status, and then commented out the call
!     30Jan2003  C. Redder  Replaced the component Date and Time with
!                           DateHr in the data structure CARDS_meta. 
!     20Dec2006  C. Redder  Replaced the component DateHr with Date and
!                           Hour in the data structure CARDS_meta. 
!     24Apr2007  C. Redder  Added the components, LTHist and QC_Rep
!                           Combined the components, Date and Hour to
!                           DateHr.
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: MyName = MyModule // '::Init_Meta'

!     Clean up (and ignore return status) 
!     ----------------------------------
!      call Clean_Meta ( Meta )

!     Store array sizes 
!     ----------------- 
      Meta % NReportsMax = NReports
      Meta % NTLevelsMax = NTLevels

!     Allocate memory
!     ---------------
      allocate ( Meta % StnID   ( NReports ),
     .           Meta % Lat     ( NReports ),
     .           Meta % Lon     ( NReports ),
     .           Meta % Elev    ( NReports ),
     .           Meta % DateHr  ( NReports ),
     .           Meta % LTime   ( NReports ),
     .           Meta % LTHist  ( NReports ),
     .           Meta % kx      ( NReports ),
     .           Meta % Type    ( NReports ),
     .           Meta % QC_Rep  ( NReports ),
     .           Meta % NLevels ( NReports ),
     .           Meta % Loc     ( NReports ), stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'Meta%StnID,'
     .                                  // 'Meta%Lat,'
     .                                  // 'Meta%Lon,'
     .                                  // 'Meta%Elev,'
     .                                  // 'Meta%DateHr,'
     .                                  // 'Meta%LTime,'
     .                                  // 'Meta%LTHist,'
     .                                  // 'Meta%kx,'
     .                                  // 'Meta%Type,'
     .                                  // 'Meta%QC_Rep,'
     .                                  // 'Meta%NLevels,'
     .                                  // 'Meta%Loc',
     .                                      NReports ) // '\W' )
         stat = Alloc_Error
         return
      end if
      return
      end subroutine Init_Meta

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_TSMeta - Initialize and allocate structure for time-series CARDS meta-data
!
! !INTERFACE:
      subroutine Init_TSMeta ( NReports, NTLevels, TSMeta, stat )
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,            intent (in)    ::
     .   NReports, ! Number of reports
     .   NTLevels  ! Total number of levels among all reports
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_tsmeta ), intent (inout) ::
     .   TSMeta     ! meta-data structure
!
! !OUTPUT PARAMETERS:
      integer,            intent (out)   ::
     .   stat     ! Error status code.  If no error, then zero is returned
!
! !REVISION HISTORY:
!     14Mar2007  C. Redder  Initial code
!     24Apr2007  C. Redder  Added the components, LTHist and QC_Rep
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_TSMeta'

!     Clean up (and ignore return status) 
!     ----------------------------------
!      call Clean_TSMeta ( TSMeta )

!     Store array sizes 
!     ----------------- 
      TSMeta % NReportsMax = NReports
      TSMeta % NTLevelsMax = NTLevels

!     Allocate memory
!     ---------------
      allocate ( TSMeta % StnID   ( NReports ),
     .           TSMeta % Lat     ( NReports ),
     .           TSMeta % Lon     ( NReports ),
     .           TSMeta % Elev    ( NReports ),
     .           TSMeta % DateHr  ( NReports ),
     .           TSMeta % LTime   ( NReports ),
     .           TSMeta % LTHist  ( NReports ),
     .           TSMeta % kx      ( NReports ),
     .           TSMeta % Type    ( NReports ),
     .           TSMeta % QC_Rep  ( NReports ),
     .           TSMeta % NLevels ( NReports ),
     .           TSMeta % Loc     ( NReports ), stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'TSMeta%StnID,'
     .                                  // 'TSMeta%Lat,'
     .                                  // 'TSMeta%Lon,'
     .                                  // 'TSMeta%Elev,'
     .                                  // 'TSMeta%DateHr,'
     .                                  // 'TSMeta%LTime,'
     .                                  // 'TSMeta%LTHist,'
     .                                  // 'TSMeta%kx,'
     .                                  // 'TSMeta%Type,'
     .                                  // 'TSMeta%QC_Rep,'
     .                                  // 'TSMeta%NLevels,'
     .                                  // 'TSMeta%Loc',
     .                                      NReports ) // '\W' )
         stat = Alloc_Error
         return
      end if
      return
      end subroutine Init_TSMeta

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_Obs - Initialize and allocate structure for CARDS obs-data
!
! !INTERFACE:
      subroutine Init_Obs ( NTLevels, Obs, stat )
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,            intent (in)    ::
     .   NTLevels ! Total number of levels among all reports
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_obs ), intent (inout) ::
     .   Obs      ! Observation data structure
!
! !OUTPUT PARAMETERS:
      integer,            intent (out)   ::
     .   stat     ! Error status code.  If no error, then zero is returned
!
! !REVISION HISTORY:
!     06May2002  C. Redder  Initial code
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     08Jan2003  C. Redder  Changed call to Clean_Obs to ignore returned
!                           status, and then commented out the call
!     04Feb2004  C. Redder  Renamed component, Lev, to P and added the
!                           components, P_m and P_e to the data structure,
!                           CARDS_obs.
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter :: MyName = MyModule // '::Init_Obs'

!     Clean up (and ignore returned status)
!     -------------------------------------
!      call Clean_Obs ( Obs, stat )

!     Store array size
!     ---------------- 
      Obs % NTLevelsMax = NTLevels

!     Allocate memory
!     ---------------
      allocate ( Obs % P_m    ( NTLevels ),
     .           Obs % P_e    ( NTLevels ),
     .           Obs % ETime  ( NTLevels ),
     .           Obs % P      ( NTLevels ),
     .           Obs % Height ( NTLevels ),
     .           Obs % Temp   ( NTLevels ),
     .           Obs % RH     ( NTLevels ),
     .           Obs % DewPt  ( NTLevels ),
     .           Obs % UWind  ( NTLevels ),
     .           Obs % VWind  ( NTLevels ), stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'Obs%ETime,'
     .                                  // 'Obs%P,'
     .                                  // 'Obs%Height,'
     .                                  // 'Obs%Temp,'
     .                                  // 'Obs%RH,'
     .                                  // 'Obs%DewPt,'
     .                                  // 'Obs%UWind,'
     .                                  // 'Obs%VWind',
     .                                      NTLevels ) // '\W' )
         stat = Alloc_Error
         return
      end if
      return
      end subroutine Init_Obs

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_QC - Initialize and allocate structure for CARDS QC-data
!
! !INTERFACE:
      subroutine Init_QC ( NTLevels, QC, stat )
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,            intent (in)    ::
     .   NTLevels ! Total number of levels among all reports
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_qc ),  intent (inout) ::
     .   QC       ! Quality control data structure
!
! !OUTPUT PARAMETERS:
      integer,            intent (out)   ::
     .   stat     ! Error status code.  If no error, then zero is returned
!
! !REVISION HISTORY:
!     06May2002  C. Redder  Initial code
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     08Jan2003  C. Redder  Changed call to Clean_QC to ignore returned
!                           status, and then commented out the call
!     04Feb2004  C. Redder  Renamed component, Lev, to P.
!     30Aug2006  C. Redder  Added the component, Mask, to the data
!                           structure of the input argument.
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter :: MyName = MyModule // '::Init_QC'

!     Clean up (and ignore returned status)
!     -------------------------------------
!      call Clean_QC ( QC, stat )

!     Store array size 
!     ---------------- 
      QC % NTLevelsMax = NTLevels

!     Allocate memory
!     ---------------
      allocate ( QC % Mask   ( NTLevels ),
     .           QC % ETime  ( NTLevels ),
     .           QC % P      ( NTLevels ),
     .           QC % Height ( NTLevels ),
     .           QC % Temp   ( NTLevels ),
     .           QC % RH     ( NTLevels ),
     .           QC % DewPt  ( NTLevels ),
     .           QC % UWind  ( NTLevels ),
     .           QC % VWind  ( NTLevels ), stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'QC%Mask,'
     .                                  // 'QC%ETime,'
     .                                  // 'QC%P,'
     .                                  // 'QC%Height,'
     .                                  // 'QC%Temp,'
     .                                  // 'QC%RH,'
     .                                  // 'QC%DewPt,'
     .                                  // 'QC%UWind,'
     .                                  // 'QC%VWind',
     .                                      NTLevels ) // '\W' )
         stat = alloc_Error
         return
      end if
      return
      end subroutine Init_QC

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Acquire_Data () --- Get and sort CARDS synoptic data
! 
! !DESCRIPTION:
!     The routine gets the CARDS synoptic data for a given file name
!     and sorts data by time and station ID.
!
! !INTERFACE:
      subroutine Acquire_Data ( File, CData, stat )
      use        m_RadSort,   only : IndexSet, IndexSort
      use        m_AdvError,  only : ErrStat,  WPErr, Alloc
      use        m_SunAlt,    only : Julian, CalDHr
      use        m_RadTplate, only : StrTemplate

!
! !INPUT/OUTPUT PARAMETERS:
      character (len=*),    intent (in)    ::
     .   File               ! Name of input file
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_data ),  intent (inout) :: 
     .   CData              ! CARDS synoptic data
!
! !OUTPUT PARAMETERS:
      integer,              intent (out)   ::
     .   stat               ! Returned status
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     28Jan2004  C. Redder  Origional code adapted from ods_etime
!!    30Aug2006  C. Redder  Moved subroutine from the main driver,
!                           radcor, in file radcor.f  Changed data
!                           structure of argument, CARDS from
!                           CARDS_space.  Changed subroutine name
!                           Aquire_CDS to CDS_Acquire.  Modified the
!                           names of called subrouitnes.
!     20Dec2006  C. Redder  Replaced the component DateHr with Date and
!                           Hour in the data structure CARDS_meta. 
!     29Jan2007  C. Redder  Modified routine so that date/hour and 
!                           launch time are modified in the routine 
!                           Get_Data
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .    MyName = MyModule // '::Acquire_Data' 
      character (len=500) :: Tail
      integer :: iRep, NReports, NLev_Max, LTime
      integer, dimension (:), allocatable :: Indx

!     Read CARDS data from file
!     -------------------------
      call Get_Data ( File, CData, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'Get_Data', stat ))
         return
      end if
      Tail = '\n   CDS File  = ' // File

!     Allocate local scratch space
!     ----------------------------
      NReports  = CData % Meta % NReports
      allocate  ( Indx ( NReports ), stat = stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, Alloc ( stat, 'Indx', NReports ))
         go to 10
      end if

!     If no reports exist, the clean up and return
!     --------------------------------------------
      if ( NReports .le. 0 ) then
         deallocate ( Indx )
         return
      end if

!     Calculate the Julian hour for each report
!     -----------------------------------------
      do iRep = 1, NReports
         LTime = CData % Meta % LTime ( iRep )
         if ( LTime .eq. IMiss ) CData % Meta % LTime ( iRep ) = 0
      end do

!     Sort by time
!     ------------
      call IndexSet  ( Indx )
      call IndexSort ( Indx, CData % Meta % LTime ( : NReports ),
     .                       rstat = stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'IndexSort' ))
         deallocate  ( Indx )
         go to 10
      end if

!     ... by station id
!     -----------------
      call IndexSort ( Indx, CData % Meta % StnID ( : NReports ),
     .                       rstat = stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'IndexSort' ))
         deallocate  ( Indx )
         go to 10
      end if

!     ... and then rearrange the reports
!     ----------------------------------
      call Reorder_Meta2 ( CData % Meta, Indx )

!     Clean up
!     --------
      deallocate ( Indx )

!     ... after error status
!     ----------------------
 10   continue
      if ( stat .ne. No_Error ) then
         call Clean_Data ( CData )
      end if

      return
      end subroutine Acquire_Data
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Acquire_TSData () --- Get and sort CARDS data
! 
! !DESCRIPTION:
!     The routine gets the CARDS time-series data for a given file name
!     and sorts data by synoptic data/hour.
!
! !INTERFACE:
      subroutine Acquire_TSData ( File, TSData, stat )
      use        m_RadSort,   only : IndexSet, IndexSort
      use        m_AdvError,  only : ErrStat,  WPErr, Alloc
      use        m_SunAlt,    only : Julian, CalDHr
      use        m_RadTplate, only : StrTemplate

!
! !INPUT/OUTPUT PARAMETERS:
      character (len=*),     intent (in)    ::
     .   File                ! Name of input file
!
! !INPUT/OUTPUT PARAMETERS:
      type ( CARDS_tsdata ), intent (inout) :: 
     .   TSData              ! CARDS time-series data
!
! !OUTPUT PARAMETERS:
      integer,               intent (out)   ::
     .   stat                ! Returned status
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     14Mar2007  C. Redder  Origional code
!     19Apr2007  C. Redder  Added the launch time as a sort key 
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .    MyName = MyModule // '::Acquire_TSData' 
      character (len=500) :: Tail
      character (len=LStnID) :: StnID1, StnID2
      integer :: iRep, NReports, NLev_Max, LTime
      integer, dimension (:), allocatable :: Indx

!     Read CARDS data from file
!     -------------------------
      call Get_TSData ( File, TSData, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'Get_TSData', stat ))
         return
      end if
      Tail = '\n   CDS File  = ' // File

!     Allocate local scratch space
!     ----------------------------
      NReports  = TSData % TSMeta % NReports
      allocate  ( Indx ( NReports ), stat = stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, Alloc ( stat, 'Indx', NReports ))
         go to 10
      end if

!     If no reports exist, the clean up and return
!     --------------------------------------------
      if ( NReports .le. 0 ) then
         deallocate ( Indx )
         return
      end if

!     Sort by launch time
!     -------------------
      call IndexSet  ( Indx )
      call IndexSort ( Indx, TSData % TSMeta % LTime  ( : NReports ),
     .                       rstat = stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'IndexSort' ))
         deallocate  ( Indx )
         go to 10
      end if

!     ... by observation date/hour
!     ----------------------------
      call IndexSort ( Indx, TSData % TSMeta % DateHr ( : NReports ),
     .                       rstat = stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'IndexSort' ))
         deallocate  ( Indx )
         go to 10
      end if

!     ... by station id
!     -----------------
      call IndexSort ( Indx, TSData % TSMeta % StnID  ( : NReports ),
     .                       rstat = stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'IndexSort' ))
         deallocate  ( Indx )
         go to 10
      end if

!     ... and then rearrange the reports
!     ----------------------------------
      call Reorder_TSMeta2 ( TSData % TSMeta, Indx )

!     Clean up
!     --------
      deallocate ( Indx )

!     ... after error status
!     ----------------------
 10   continue
      if ( stat .ne. No_Error ) then
         call Clean_TSData ( TSData )
      end if

      return
      end subroutine Acquire_TSData
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_Data () --- Get the entire CARDS synoptic data
! 
! !DESCRIPTION:
!     This routine retrieves CARDS synoptic data and stores it into
!     a data structure of derived type, CARDS_data.  The routine will
!     accept a report only if the latitude, longitude, station
!     elevation, the data and time tag and the data source index can
!     be determined.  Any level without a valid pressure value will
!     not be accepted.
!
! !INTERFACE:
      subroutine Get_Data ( File, CData, stat,    ! required &
     .                      repl )                ! optional arguments
      use m_AdvError,       only : PErr, ErrStat
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len = *), intent (in)  ::
     .   File              ! Name of input file.
      logical,             intent (in), optional ::
     .   repl              ! = .true. to store replacement levels (if they
                           !   exist) rather than the original.  Default
!                          !   = .false.
! !OUTPUT PARAMETERS: 
      type ( CARDS_data ), intent (out) :: 
     .   CData             ! CARDS synoptic data
      integer,             intent (out) ::
     .   stat              ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     06May2002  C. Redder  Origional code.
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     14Mar2007  C. Redder  Made internal changes to accommodate changes
!                           in the interface to the internal routine,
!                           Get_CDat.
! EOP
!-------------------------------------------------------------------------

      type ( CARDS_tsmeta ) :: TSMeta
      character (len=*), parameter :: MyName = MyModule // '::Get_Data'

      call Get_CDat ( File, CData % Meta, TSMeta,
     .                      CData % Obs,
     .                      CData % QC,
     .                      stat,
     .                      no_tsmeta = .true., repl = repl ) 
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Get_CDat' ) // '\W' )
         return
      end if

      return
      end subroutine Get_Data
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_Meta () --- Get the CARDS synoptic meta-data
! 
! !DESCRIPTION:
!     This routine retrieves CARDS synoptic meta-data and stores it
!     into a data structure of derived type, CARDS_meta.  The routine
!     will accept a report only if the latitude, longitude, station
!     elevation, the data and time tag and the data source index can
!     be determined.  Any level without a valid pressure value will
!     not be accepted.
!
! !INTERFACE:
!
      subroutine Get_Meta ( File, Meta, stat )
      use m_AdvError,       only : PErr, ErrStat
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len = *), intent (in)  ::
     .   File              ! Name of input file.
!
! !OUTPUT PARAMETERS: 
      type ( CARDS_meta ), intent (out) :: 
     .   Meta              ! CARDS synoptic meta-data
      integer,             intent (out) ::
     .   stat              ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     06May2002  C. Redder  Origional code.
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     14Mar2007  C. Redder  Made internal changes to accommodate changes
!                           in the interface to the internal routine,
!                           Get_CDat.
! EOP
!-------------------------------------------------------------------------

      type ( CARDS_tsmeta ) :: TSMeta
      type ( CARDS_obs )    :: Obs
      type ( CARDS_qc  )    :: QC
      character (len=*), parameter :: MyName = MyModule // '::Get_Meta'

      call Get_CDat ( File, Meta, TSMeta, Obs, QC, stat,
     .                no_tsmeta = .true.,
     .                no_obs    = .true.,
     .                no_qc     = .true. )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Get_CDat' ) // '\W' )
         return
      end if

      return
      end subroutine Get_Meta
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_TSData () --- Get the entire CARDS time-series data
! 
! !DESCRIPTION:
!     This routine retrieves CARDS time-series data and stores it into
!     a data structure of derived type, CARDS_data.  The routine will
!     accept a report only if the latitude, longitude, station
!     elevation, the data and time tag and the data source index can
!     be determined.  Any level without a valid pressure value will
!     not be accepted.
!
! !INTERFACE:
      subroutine Get_TSData ( File, TSData, stat,    ! required &
     .                        repl )                 ! optional arguments
      use m_AdvError,         only : PErr, ErrStat
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len = *), intent (in)  ::
     .   File              ! Name of input file.
      logical,             intent (in), optional ::
     .   repl              ! = .true. to store replacement levels (if they
                           !   exist) rather than the original.  Default
!                          !   = .false.
! !OUTPUT PARAMETERS: 
      type ( CARDS_tsdata ), intent (out) :: 
     .   TSData            ! CARDS time-series data
      integer,             intent (out) ::
     .   stat              ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     14Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------

      type ( CARDS_meta ) :: Meta
      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_TSData'

      call Get_CDat ( File, Meta, TSData % TSMeta,
     .                            TSData % Obs,
     .                            TSData % QC,
     .                            stat,
     .                            no_meta = .true., repl = repl ) 
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Get_CDat' ) // '\W' )
         return
      end if

      return
      end subroutine Get_TSData
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_TSMeta () --- Get the CARDS time-series meta-data
! 
! !DESCRIPTION:
!     This routine retrieves CARDS time-series meta-data and stores it
!     into a data structure of derived type, CARDS_meta.  The routine
!     will accept a report only if the latitude, longitude, station
!     elevation, the data and time tag and the data source index can
!     be determined.  Any level without a valid pressure value will
!     not be accepted.
!
! !INTERFACE:
!
      subroutine Get_TSMeta ( File, TSMeta, stat )
      use m_AdvError,       only : PErr, ErrStat
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len = *), intent (in)  ::
     .   File              ! Name of input file.
!
! !OUTPUT PARAMETERS: 
      type ( CARDS_tsmeta ), intent (out) :: 
     .   TSMeta            ! CARDS meta-data
      integer,             intent (out) ::
     .   stat              ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     14Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------

      type ( CARDS_meta ) :: Meta
      type ( CARDS_obs  ) :: Obs
      type ( CARDS_qc   ) :: QC
      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_TSMeta'

      call Get_CDat ( File, Meta, TSMeta, Obs, QC, stat,
     .                no_meta = .true.,
     .                no_obs  = .true.,
     .                no_qc   = .true. )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Get_CDat' ) // '\W' )
         return
      end if

      return
      end subroutine Get_TSMeta
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_CDat () --- Get the CARDS data
! 
! !DESCRIPTION:
!     This routine retrieves CARDS data and stores it into any of 
!     the data structures of the derived types that are in the 
!     argument list.  The routine will accept a report only if the
!     latitude, longitude, station elevation, the data and time tag
!     and the data source index can be determined.  Any level without
!     a valid pressure value will not be accepted.
!
! !INTERFACE:
      subroutine Get_CDat ( File,
     .                      Meta,    TSMeta,    Obs,    QC,
     .                      stat,                              ! Required &
     .                      no_meta, no_tsmeta, no_obs, no_qc, ! optional
     .                      repl )                             ! arguments
      use m_AtoX,         only : AtoI
      use m_SunAlt,       only : Julian, CalDHr, Check_DateTime
      use m_AdvError,     only : ItoA, PErr, Alloc, ErrStat, WPErr
      use m_SysIO,        only : LUAvail
      use m_RadSort,      only : R2ManExp
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len = *),   intent (in)  ::
     .   File                ! Name of CARDS file.
      logical,               intent (in), optional  ::
     .   no_meta,            ! = .true. to prevent storage of any meta data
     .   no_tsmeta,          ! ... time-series meta data
     .   no_obs,             ! ... observation data
     .   no_qc,              ! ... quality control data.  Defaults = .false.
     .   repl                ! = .true. to store replacement levels (if they
                             !   exist) rather than the original.  Default
!                            !   = .false.
! !OUTPUT PARAMETERS:
      type ( CARDS_meta ),   intent (inout) :: 
     .   Meta                ! CARDS synoptic meta-data
      type ( CARDS_tsmeta ), intent (inout) :: 
     .   TSMeta               ! CARDS time-series meta-data
      type ( CARDS_obs  ),   intent (inout) :: 
     .   Obs                 ! ... observation data
      type ( CARDS_qc   ),   intent (inout) :: 
     .   QC                  ! ... quality control data
      integer,               intent (out) ::
     .   stat                ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     02May2002  C. Redder  Origional code.
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     13Dec2002  C. Redder  Set string length for WMO station ID via constant
!                           LStnID
!     06Jan2003  C. Redder  Changed use m_Julian to use m_SunAlt
!     08Jan2003  C. Redder  Changed call to clean up routines to ignore
!                           returned status
!     15Jan2003  C. Redder  Changed use module from m_ioutil to m_SysIO
!     30Jan2003  C. Redder  Replaced the component Date and Time with
!                           DateHr in the data structure CARDS_meta. 
!     04Feb2004  C. Redder  Renamed the component, Lev, to P and added the
!                           components, P_m and P_e to the data structure,
!                           CARDS_obs.  Renamed the component, Lev, to P
!                           in the data structure, CARDS_qc.  Set launch
!                           time in seconds.  Changed the component, ETime,
!                           from integer to real in the data structure,
!                           CARDS_Obs. 
!     30Aug2006  C. Redder  Added the component, Mask, to the data
!                           structure, CARDS_qc.
!     17Dec2006  C. Redder  Modified an error messages.  Made additional
!                           checks on date/time tags in header information
!                           Replaced the component DateHr with Date and
!                           Hour in the data structure CARDS_meta. 
!     29Jan2007  C. Redder  Added code to modify data/hour and launch
!                           time so that all observation have a common
!                           synoptic date/hour. Revised algorthm that
!                           handles replacement levels.
!     15Feb2007  C. Redder  Change the intent attribute from out to
!                           inout for the arguments, Meta, Obs and QC.
!                           (in order to prevent possible run-time 
!                           errors from deallocation statement)
!     14Mar2007  C. Redder  Added the arguments, TSMeta and no_tsmeta
!                           Ensured that stat is defined
!     24Apr2007  C. Redder  Added the components, FileName, LTHist and 
!                           QC_Rep, to the arguments, TSMeta and Meta.
!                           Combined the components, Date and Hour, to
!                           DateHr in the argument, Meta.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter :: MyName = MyModule // '::Get_CDat'

      real,    parameter ::
     .   Temp0     = 273.16    ! Temperature in deg K at 0 deg C
      integer, parameter ::
     .   DefDHr    = 3,        ! Delta hour defining the time window
     .   LLineMax  = 108       ! Maximum length of each line in the
                               !   input data file
      logical :: accept_report, last_report_accepted, eof, eor
      logical :: new_report
      logical :: get_meta, get_tsmeta, get_obs, get_qc
      integer :: NReports, NTLevels, NLevels, iRep
      integer :: lu, iScan, iLine, iret, iret1, iret2
      real    :: Lat,  Lon,  Elev
      integer :: Date, Hour, Minute
      integer :: LTime, LTime1, LHour, LMinute, LMin, LTHist
      integer :: ObJHr, ObDate, ObHr, ObDHr (2)
      integer :: ObsType, IType, kx, Loc
      integer :: iLevBeg, iLevEnd
      integer :: iRec, LRec, LRec1
      integer :: NAddRec, NAddRec_File, iRecLev, NRecLev
      logical :: accept_level, last_level_accepted
      logical :: repl_may_exist, get_repl, repl_level, valid_repl
      integer :: LvlQual,  RLvlQual
      integer :: ETime,    ETime_QC
      real    :: Press,    Temp,    Height,    DewPt,    RH,    DPtDep
      integer :: Press_QC, Temp_QC, Height_QC, DewPt_QC, RH_QC, Wind_QC
      integer :: QC_Effort, Data_Source, iQC
      real    :: WndDir,   WndSpd,  UWind,     VWind
      real    :: pi, Deg2Rad, RadDir
      real    :: WndSpd1,  WndSpd2, WndDir1, WndDir2
      integer :: Wind_QC1, Wind_QC2
      character (len=MaxLRec) :: Record
      character (len=LHeader) :: Header
      character (len=LRecLev) :: ThisLev
      character (len=LStnID)  :: WMO_ID
      character (len=10)      :: CNum, CWndSp
      character (len=2)       :: CNumQC
      integer, dimension (:), allocatable :: JulHr

!     Implement options
!     -----------------
      get_repl   = .false.
      if ( present ( repl      )) get_repl   =       repl
      get_meta   = .true.
      if ( present ( no_meta   )) get_meta   = .not. no_meta
      get_tsmeta = .true.
      if ( present ( no_tsmeta )) get_tsmeta = .not. no_tsmeta
      get_obs    = .true.
      if ( present ( no_obs    )) get_obs    = .not. no_obs
      get_qc     = .true.
      if ( present ( no_qc     )) get_qc     = .not. no_qc

!     Conversion factor from degrees to radians
!     -----------------------------------------
      pi      = 2.0d0 * atan2 ( 1.0d0, 0.0d0 )
      Deg2Rad = pi / 180.0d0

!     Default status
!     --------------
      stat = No_Error

!     Determine an available logical unit
!     -----------------------------------
      lu = LUAvail()
      if ( lu .lt. 0 ) then
         stat = No_LUAvail
         call PErr ( MyName, 'No logical units available for the '
     .                    // 'file, ' // trim ( File ) // '\W'  )
         return
      end if

!     Scan the input file twice first to determine the number of
!     cards reports and the second time to extract the data
!     ---------------------------------------------------------
      ScanLoop : do  iScan = 1, 2

!        Allocate space for raob header information
!        ------------------------------------------
         if ( iScan .eq. 2 ) then
            if ( get_meta   ) then
               call Init_Meta  ( NReports, NTLevels, Meta,   stat )
               if ( stat .ne. No_Error ) then
                  call PErr  ( MyName,
     .                         ErrStat ( 'Init_Meta'   ) // '\W' )
                  go to 10
               end if
               Meta   % FileName = File
            end if
            if ( get_tsmeta ) then
               call Init_TSMeta ( NReports, NTLevels, TSMeta, stat )
               if ( stat .ne. No_Error ) then
                  call PErr  ( MyName,
     .                         ErrStat ( 'Init_TSMeta' ) // '\W' )
                  go to 10
               end if
               TSMeta % FileName = File
            end if
            if ( get_obs    ) then
               call Init_Obs   (           NTLevels, Obs,    stat )
               if ( stat .ne. No_Error ) then
                  call PErr  ( MyName,
     .                         ErrStat ( 'Init_Obs'    ) // '\W' )
                  go to 10
               end if
            end if
            if ( get_qc     ) then
               call Init_QC    (           NTLevels, QC,     stat )
               if ( stat .ne. No_Error ) then
                  call PErr  ( MyName,
     .                         ErrStat ( 'Init_QC'     ) // '\W' )
                  go to 10
               end if
            end if
         end if

!        Open CARDS file
!        ---------------
         open ( unit   =  lu,
     .          file   =  File,
     .          form   = 'formatted',
     .          status = 'old',
     .          access = 'sequential',
     .          recl   =  MaxLRec,
     .          iostat =  stat )
         if ( stat .ne. 0 ) then
            call PErr ( MyName, 'Open error (iostat = '
     .                     //    trim ( ItoA ( stat ) ) // ')\C'
     .                     // '\n   File = ' // trim ( File ) )
            stat = Open_Error
            go to 10
         end if

!        Initialize counters
!        -------------------
         last_report_accepted = .false.
         new_report      = .true.
         repl_may_exist  = .false.
         NAddRec         =  0
         NReports        =  0
         NTLevels        =  0
         NLevels         =  0

!        For each text line in the file ...
!        ----------------------------------
         RecordLoop : do iRec = 1, MaxNRec
            call ReadLn ( lu, LRec, Record, eor, eof, stat )
            if ( stat .ne. 0 ) then
               call PErr  ( MyName, 'Read error (iostat = '
     .                         //    trim ( ItoA ( stat ) ) // ') \C'
     .                         // '\n   File = ' // trim ( File ))
               stat = Read_Error
               go to 10
            end if
            if ( eof ) exit RecordLoop

!           Ignore any comment line (i.e. record beginning with "%")
!           --------------------------------------------------------
            if ( Record ( 1 : 1 ) .eq. '%' )
     .         cycle RecordLoop

!           Else exit with error status if record does not begin with a '#'
!           --------------------------------------------------------------
            if ( Record ( 1 : 1 ) .ne. '#' ) then
               call PErr ( MyName, 'Record found that does not begin '
     .                         //  'with % or #  \W \n'
     .                         //'\C   Header = ' // Record ( :LHeader)
     .                         //'\n   File   = ' // File )
               stat = Bad_Record
               go to 10
            end if

!           Start processing the header information for the current record
!           --------------------------------------------------------------
            Header = Record ( 1 : LHeader )

!           Number of records remaining for this
!           report (not including the current one)
!           --------------------------------------           
            NAddRec_File = AtoI ( Header ( 103:105 ), iret )
            if ( iret .ne. 0 ) then
               call PErr ( MyName, 'Cannot get the number of '
     .                         //  'additional records \W \n'
     .                         //'\C   Header = ' // Header
     .                         //'\n   File   = ' // File )
               stat = Bad_Record
               go to 10
            end if

!           Check to ensure that records within a report are not out of order
!           -----------------------------------------------------------------
            if ( .not. new_report .and. 
     .            NAddRec_File .ne. NAddRec ) then
               call PErr ( MyName, 'A record within a report '
     .                         //  'is out of order.  \W \n'
     .                         //'\C   Header = ' // Header
     .                         //'\n   File   = ' // File )
               stat = Bad_Record
               go to 10
            end if

!           Get the number of levels in the record
!           --------------------------------------
            NRecLev  = AtoI ( Record ( 106:108 ), iret )
            if ( iret .ne. 0 ) then
               call PErr ( MyName, 'Cannot get the number of levels '
     .                         //  'in the record  \W \n'
     .                         //'\C   Header = ' // Header
     .                         //'\n   File   = ' // File )
               stat = Bad_Record
               go to 10
            end if

!           ... and make sure it is consistent
!           ----------------------------------
            if ( NRecLev      .ne. MaxNRecLev .and.
     .           NAddRec_File .ne. 0 .or.
     .           NRecLev      .gt. MaxNRecLev ) then
               call  PErr ( MyName, 'The number of levels in a record '
     .                          //  'exceeds the maximum (= '
     .                          //   trim ( ItoA ( MaxNRecLev ))
     .                          //  ') or is not the maximum when '
     .                          //  'the number of additional records '
     .                          //  'is non-zero  \W \n'
     .                          //'\C   Header = ' // Header
     .                          //'\n   File   = ' // File )
               stat = Bad_Record
               go to 10
            end if

!           Make sure that the record length is
!           consistent with the header information
!           --------------------------------------
            LRec1 = LHeader + NRecLev * LRecLev
            if ( LRec1 .lt. LRec ) 
     .         LRec = max ( LRec1, len_trim ( Record ( : LRec ))) 
            if ( LRec1 .gt. LRec ) then
               call PErr ( MyName, 'The record length (='
     .                      //      trim ( ItoA ( LRec )) // ') '
     .                      //     'does not match the value '
     .                      //     'predicted from header '
     .                      //     'information (='
     .                      //      trim ( ItoA ( LRec1 )) // ').\W'
     .                      // '\n\C   Input file = '//trim ( File  )
     .                      //   '\n   Header     = '//trim ( Header))
               stat = Bad_Record
               go to 10
            end if
            LRec = LRec1

!           Scan header only if new sonde
!           -----------------------------
            if ( new_report ) then

!              Process any remaining header information from last report
!              ---------------------------------------------------------
               if ( iScan .eq. 2 ) then
                  if ( get_meta   ) then
                     if ( last_report_accepted )
     .                  Meta   % NLevels ( NReports ) = NLevels
                  end if
                  if ( get_tsmeta ) then
                     if ( last_report_accepted )
     .                  TSMeta % NLevels ( NReports ) = NLevels
                  end if
               endif

               accept_report = .true.        ! Accept this sonde until a
                                             !   reason is found to reject
!              WMO identification tag
!              ----------------------
               WMO_ID = Header ( 2:6 )
               if ( Header ( 7:7 )    .ne. '0' ) accept_report = .false.

!              Latitude (required)
!              -------------------
               CNum = Header ( 17:23 )
               if ( CNum ( 1:7 )  .eq. '9999999' ) then
                  accept_report = .false.
               else
                  Lat  = real ( AtoI ( CNum ( 1:7 ), iret )) / 100000.0
                  if ( iret           .ne.  0  ) accept_report = .false.
                  if ( Header (24:24) .eq. 'S' ) Lat = -Lat
               endif              

!              Longitude (required)
!              --------------------
               CNum = Header ( 25:32 )
               if ( CNum ( 1:8 ) .eq. '99999999' ) then
                  accept_report = .false.
               else
                  Lon  = real ( AtoI ( CNum ( 1:8 ), iret )) / 100000.0
                  if ( iret           .ne.  0  ) accept_report = .false.
                  if ( Header (33:33) .eq. 'W' ) Lon = -Lon
               endif

!              Station elevation (required)
!              ----------------------------
               CNum = Header ( 34:38 )
               if ( CNum ( 1:5 ) .eq. '99999' ) then
                  accept_report = .false.
               else
                  Elev = real ( AtoI ( CNum ( 1:5 ), iret )) / 10.0
                  if ( iret .ne.  0 ) accept_report = .false.
               endif

!              Date and hour (required)
!              ------------------------
               CNum = Header ( 39:48 )
               if ( CNum (9:10) .eq. '99' ) then
                  accept_report = .false.
               else
                  Date   = AtoI ( CNum ( 1:8 ), iret1 )
                  Hour   = AtoI ( CNum ( 9:10), iret2 )
                  if ( iret1 .ne. 0 .or.
     .                 iret2 .ne. 0 ) then
                        accept_report = .false.
                  else
                     if ( Check_DateTime ( Date, Hour ) .ne. 0 )
     .                  accept_report = .false.
                     Minute =   Hour * 60
                  end if
               end if

!              Launch time
!              -----------
               CNum  = Header ( 49:52 )
               LTime = IMiss
               if ( CNum (1:4)  .ne. '9999' ) then
                  LHour   = AtoI ( CNum ( 1:2 ), iret1 ) 
                  LMinute = AtoI ( CNum ( 3:4 ), iret2 ) 
                  if ( iret1   .eq. 0  .and.
     .                 iret2   .eq. 0 ) then
                  if ( LHour   .ge. 0  .and.
     .                 LHour   .le. 23 .and.
     .                 LMinute .ge. 0  .and.
     .                 LMinute .le. 59 ) then
                     LMin     = LHour * 60.0 + LMinute
                     if       ( LMin - Minute  .gt.  12 * 60 ) then
                        LTime = LMin - Minute - 24 * 60
                     else if  ( LMin - Minute  .le. -12 * 60 ) then
                        LTime = LMin - Minute + 24 * 60
                     else
                        LTime = LMin - Minute
                     endif
                     if ( abs ( LTime ) .gt. 60 ) ! ... range check.  LTime
     .                  LTime = IMiss             !   must be within 1 hour
                  end if                          !   of Time.
                  end if
               end if
               if ( LTime .ne. IMiss ) then
                  LTime  = LTime * 60             ! Convert to seconds
                  LTHist = LT_File                ! Set history flag
               else
                  LTime  = 0                      ! Set to ob time
                  LTHist = LT_ObTime
               end if

!              Data source index (kx) from NCDC observation type
!              (Skip satellite and aircraft data for now.)
!              -------------------------------------------------
               CNum  = Header ( 62:63 )
               if ( CNum (1:2) .eq. '99' ) then
                  accept_report = .false.
               else
                  ObsType = AtoI ( CNum ( 1:2 ), iret )
                  if ( iret    .ne. 0 ) ObsType = 0
                  if ( ObsType .ge. 1 .and.
     .                 ObsType .le. 12 ) then
                     kx = kxTable ( ObsType )
                  else
                     kx = 0
                  end if
                  if ( kx .eq. 0 ) accept_report = .false.
               endif

!              NCDC Instrument type
!              --------------------
               CNum  = Header ( 85:87 )
               IType = IMiss
               if ( CNum (1:3)  .ne. '999' ) then
                  IType = AtoI ( CNum ( 1:3 ), iret )
                  if ( iret .ne. 0 ) IType = IMiss
               end if

!              QC Effort
!              ---------
               CNum  = Header ( 88:88 )
               QC_Effort = AtoI ( CNum ( 1:1 ), iret )
               if ( iret .ne. 0 ) QC_Effort  = 9

!              Data source/format
!              ------------------
               CNum  = Header ( 89:90 )
               Data_Source = AtoI ( CNum ( 1:2 ), iret )
               if ( iret .ne. 0 ) Data_Source = 99

!              Reject report if conversion to CARDS QC is not possible
!              -------------------------------------------------------
               iQC   = CARDS_iQC ( '00', 1, QC_Effort, Data_Source )
               if ( iQC .eq. -1 ) accept_report = .false.

!              If header info is OK, 
!              ---------------------
               if ( accept_report ) then

!                 ... accept the sonde
!                 --------------------
                  NReports  = NReports  + 1

!                 ... and save the header info during the second scan
!                 ---------------------------------------------------
                  Loc = NTLevels + 1
                  if ( iScan .eq. 2 .and. get_meta   ) then
                     Meta   % StnID  ( NReports ) = WMO_ID
                     Meta   % Lat    ( NReports ) = Lat
                     Meta   % Lon    ( NReports ) = Lon
                     Meta   % Elev   ( NReports ) = Elev
                     Meta   % DateHr ( NReports ) = Date * 100 + Hour
                     Meta   % LTime  ( NReports ) = LTime
                     Meta   % LTHist ( NReports ) = LTHist
                     Meta   % kx     ( NReports ) = kx
                     Meta   % Type   ( NReports ) = IType
                     Meta   % QC_Rep ( NReports ) = Accept
                     Meta   % Loc    ( NReports ) = Loc
                  end if
                  if ( iScan .eq. 2 .and. get_tsmeta ) then
                     TSMeta % StnID  ( NReports ) = WMO_ID
                     TSMeta % Lat    ( NReports ) = Lat
                     TSMeta % Lon    ( NReports ) = Lon
                     TSMeta % Elev   ( NReports ) = Elev
                     TSMeta % DateHr ( NReports ) = Date * 100 + Hour
                     TSMeta % LTime  ( NReports ) = LTime
                     TSMeta % LTHist ( NReports ) = LTHist
                     TSMeta % kx     ( NReports ) = kx
                     TSMeta % Type   ( NReports ) = IType
                     TSMeta % QC_Rep ( NReports ) = Accept
                     TSMeta % Loc    ( NReports ) = Loc
                  end if
                  NLevels  = 0
                  last_report_accepted = .true.
               else
                  last_report_accepted = .false.
               end if

!              No level has yet to be accepted in new report
!              ---------------------------------------------
               last_level_accepted = .false.
            end if

!           Update for next record
!           ----------------------
            new_report = NAddRec_File .le. 0
            NAddRec    = NAddRec_File - 1

!           If report is accepted ...
!           -------------------------
            if ( accept_report ) then

!              ... process each level of the data section
!              ------------------------------------------
               RecLevLoop: do iRecLev = 1, NRecLev
                  iLevBeg = LHeader + LRecLev * ( iRecLev - 1 ) + 1
                  iLevEnd = iLevBeg + LRecLev - 1
                  ThisLev = Record ( iLevBeg : iLevEnd )

!                 Get the level quality
!                 ---------------------
                  CNum    = ThisLev ( 1:1 )
                  LvlQual = AtoI ( CNum ( 1:1 ), iret )
                  if ( iret .ne. 0 ) then
                     call PErr ( MyName, 'Bad entry for the quality '
     .                              //   'indicator for a level \C'
     .                              // '\n   Header = ' // Header
     .                              // '\n   Level  = ' // ThisLev
     .                              // '\n   File   = ' // File )
                     stat = Bad_Level
                     go to 10
                  end if

!                 Current level is to be accepted if either ...
!                 1) replacememt level is desired and current
!                      level is in fact a valid eplacement level
!                 2) current level is not a replacement level
!                 ----------------------------------------------
                  valid_repl   = LvlQual .eq. 8 .and. repl_may_exist
                  accept_level = valid_repl     .and. get_repl .or. ! 1)
     .                           LvlQual .ne. 8                     ! 2)

!                 Elapsed time (sec) since launch time
!                 ------------------------------------
                  CNum   = ThisLev (  2:6  )
                  CNumQC = ThisLev ( 41:42 )
                  ETime  = AtoI   ( CNum ( 1:5 ), iret )
                  if ( CNum ( 1:5 ) .eq. '99999' .or.
     .                 iret         .ne.  0 ) then
                     ETime    = IMiss
                     ETime_QC = 9
                  else
                     ETime    = 60 * ( ETime / 100 )
     .                        + mod  ( ETime,  100 )
                     ETime_QC = CARDS_iQC ( CNumQC,    1,
     .                                      QC_Effort, Data_Source )
                  end if

!                 Pressure (hPa, required)
!                 ------------------------
                  CNum   = ThisLev (  7:12 )
                  CNumQC = ThisLev ( 43:44 )
                  Press  = real ( AtoI ( CNum ( 1:6 ), iret ))
                  if ( CNum ( 1:6 ) .eq. '999999' .or.
     .                 iret         .ne.  0 ) then
                     Press    = AMiss
                     Press_QC = 9
                     accept_level = .false.
                  else
                     Press    = Press / 100.0
                     Press_QC = CARDS_iQC ( CNumQC,    2,
     .                                      QC_Effort, Data_Source )
                  end if
                  if ( Press_QC .eq. 9 )      ! If pressure is missing skip
     .               accept_level = .false.   ! this level, regardless
                                              ! ---------------------------
!                 Height (m)
!                 ----------
                  CNum   = ThisLev ( 13:19 )
                  CNumQC = ThisLev ( 45:46 )
                  Height = real ( AtoI ( CNum ( 1:7 ), iret ))
                  if ( CNum .eq. '-999999' .or.
     .                 iret .ne.  0 ) then
                     Height    = AMiss
                     Height_QC = 9
                  else
                     Height    = Height
                     Height_QC = CARDS_iQC ( CNumQC,    3,
     .                                       QC_Effort, Data_Source )
                  end if

!                 Temperature (deg K)
!                 -------------------
                  CNum   = ThisLev ( 20:24 )
                  CNumQC = ThisLev ( 47:48 )
                  Temp   = real ( AtoI ( CNum ( 1:5 ), iret ))
                  if ( CNum ( 1:5 ) .eq. '+9999' .or.
     .                 iret         .ne.  0 ) then
                     Temp      = AMiss
                     Temp_QC   = 9
                  else
                     Temp      = Temp / 10.0 + Temp0      ! ... in deg K
                     Temp_QC   = CARDS_iQC ( CNumQC,    4,!-------------
     .                                       QC_Effort, Data_Source )
                  end if

!                 Relative humidity (%)
!                 ---------------------
                  CNum   = ThisLev ( 25:28 )
                  CNumQC = ThisLev ( 49:50 )
                  RH     = real ( AtoI ( CNum ( 1:4 ), iret ))
                  if ( CNum ( 1:4 ) .eq. '9999' .or.
     .                 iret         .ne.  0 ) then
                     RH        = AMiss
                     RH_QC     = 9
                  else
                     RH        = RH / 10.0
                     RH_QC     = CARDS_iQC ( CNumQC,    5,
     .                                       QC_Effort, Data_Source )
                  end if

!                 Dew point (deg K)
!                 -----------------
                  CNum   = ThisLev ( 29:31 )
                  CNumQC = ThisLev ( 51:52 )
                  DPtDep = real ( AtoI ( CNum ( 1:3 ), iret ))
                  if ( CNum ( 1:3 ) .eq. '999' .or.
     .                 iret         .ne.  0 ) then
                     DPtDep    = AMiss
                     DewPt_QC  = 9
                  else
                     DPtDep    = DPtDep / 10.0 ! Dew point depression
                     DewPt_QC  = CARDS_iQC ( CNumQC,    6,
     .                                       QC_Effort, Data_Source )
                  end if
                  if ( DewPt_QC .ne. 9 .and.   ! ... converted to dew point
     .                 Temp_QC  .ne. 9 ) then  !   in deg K
                     DewPt     = Temp - DPtDep
                  else
                     DewPt     = AMiss
                     DewPt_QC  = 9
                  end if

!                 Zonal and merional components of wind (m/s)
!                 -------------------------------------------
                  CNumQC = ThisLev ( 53:54 )
                  CNum   = ThisLev ( 32:34 )
                  CWndSp = ThisLev ( 35:38 )
                  WndDir = real ( AtoI ( CNum   ( 1:3 ), iret1 ))
                  WndSpd = real ( AtoI ( CWndSp ( 1:4 ), iret2 ))
                  if ( CNum   ( 1:3 ) .eq.  '999' .or.
     .                 CWndSp ( 1:4 ) .eq. '9999' .or.
     .                 iret1          .ne.  0     .or.
     .                 iret2          .ne.  0 ) then
                     WndDir    = AMiss
                     WndSpd    = AMiss
                     Wind_QC   = 9
                  else
                     WndDir    =  WndDir
                     WndSpd    =  WndSpd / 10.0
                     Wind_QC   =  CARDS_iQC ( CNumQC,    7,
     .                                        QC_Effort, Data_Source )
                  end if

                  if ( Wind_QC .ne. 9 ) then
                     RadDir    =  Deg2Rad * WndDir
                     UWind     = -WndSpd * sin ( RadDir )
                     VWind     = -WndSpd * cos ( RadDir )
                  else
                     UWind     =  AMiss
                     VWind     =  AMiss
                  end if

!                 Some of the data was erroneously given a qc value 
!                 of 3 due to a software bug at NCDC.  Therefore, ...
!                 ---------------------------------------------------
                  if ( .not. valid_repl ) then
                     WndDir1  = WndDir
                     WndSpd1  = WndSpd
                     Wind_QC1 = Wind_QC

!                 ... if a valid replacement level exists ...
!                 -------------------------------------------
                  else 
                     WndDir2  = WndDir
                     WndSpd2  = WndSpd
                     Wind_QC2 = Wind_QC

!                    ... and if the qc for the unmodified and replacement
!                    level is 3 ("eroneous value") and 4 ("corrected value")
!                    -------------------------------------------------------
                     if ( Wind_QC1 .eq. 3 .and.
     .                    Wind_QC2 .eq. 4 ) then

!                       ... and the differences are insignificant
!                       -----------------------------------------
                        if ( abs ( WndDir1 - WndDir2 ) .le. 5.0 .and.
     .                       abs ( WndSpd1 - WndSpd2 ) .le. 1.0 ) then
      
!                          ... then set the qc to 1 ("correct value")
!                          ------------------------------------------ 
                           Wind_QC     = 1

!                          ... and if necessary, modify the qc for
!                              the last accepted unmodified level
!                              -----------------------------------
                           if ( last_level_accepted .and.
     .                          iScan .eq. 2        .and.
     .                          get_qc ) then
                              QC % UWind  ( NTLevels ) = Wind_QC
                              QC % VWind  ( NTLevels ) = Wind_QC
                           end if
                        end if
                     end if
                  end if

!                 If level is to be accepted ...
!                 ------------------------------
                  if ( accept_level ) then
                     repl_level  = valid_repl
                     if ( .not. repl_level .or. 
     .                    .not. last_level_accepted ) then
                        NLevels  = NLevels  + 1
                        NTLevels = NTLevels + 1
                     end if
                     if ( iScan .eq. 2 ) then
                        if ( get_obs ) then
                           Obs % ETime  ( NTLevels ) = real ( ETime )
                           Obs % P      ( NTLevels ) = Press
                           Obs % Height ( NTLevels ) = Height
                           Obs % Temp   ( NTLevels ) = Temp
                           Obs % RH     ( NTLevels ) = RH
                           Obs % DewPt  ( NTLevels ) = DewPt
                           Obs % UWind  ( NTLevels ) = UWind
                           Obs % VWind  ( NTLevels ) = VWind
                        end if
                        if ( get_qc  ) then
                           QC  % Mask   ( NTLevels ) = .true.
                           QC  % ETime  ( NTLevels ) = ETime_QC
                           QC  % P      ( NTLevels ) = Press_QC
                           QC  % Height ( NTLevels ) = Height_QC
                           QC  % Temp   ( NTLevels ) = Temp_QC
                           QC  % RH     ( NTLevels ) = RH_QC
                           QC  % DewPt  ( NTLevels ) = DewPt_QC
                           QC  % UWind  ( NTLevels ) = Wind_QC
                           QC  % VWind  ( NTLevels ) = Wind_QC
                        end if
                     end if
                  end if

!                 For the next record level...
!                 ----------------------------
                  last_level_accepted = accept_level

!                 ... determine if a replacement
!                     can exist at the next level
!                 -------------------------------
                  if      ( LvlQual .eq. 2 .or.
     .                      LvlQual .eq. 4 .or.
     .                      LvlQual .eq. 6 ) then
                     repl_may_exist  = .true.
                  else if ( LvlQual .eq. 8 ) then
                     repl_may_exist  = .false. 
                  else
                     repl_may_exist  = .false.
                  end if

               end do RecLevLoop
            end if
         end do RecordLoop

!        Update meta data for last report
!        --------------------------------
         if ( iScan .eq. 2  ) then
            if ( get_meta   ) then
               if ( last_report_accepted )
     .            Meta   % NLevels ( NReports ) = NLevels
               Meta   % NReports = NReports
               Meta   % NTLevels = NTLevels
            end if
            if ( get_tsmeta ) then
               if ( last_report_accepted )
     .            TSMeta % NLevels ( NReports ) = NLevels
               TSMeta % NReports = NReports
               TSMeta % NTLevels = NTLevels
            end if
            if ( get_obs    ) then
               Obs  % NTLevels   = NTLevels
               call R2ManExp ( NPresDigits, 
     .                         Obs % P   ( : NTLevels ),
     .                         Obs % P_m ( : NTLevels ),
     .                         Obs % P_e ( : NTLevels ))
            end if
            if ( get_qc    ) then
               QC   % NTLevels   = NTLevels
            end if
         end if

         close ( lu )
      end do ScanLoop

!     Modify the date/hour and launch time
!     so that all reports have a common date/hour
!     -------------------------------------------
      if ( get_meta ) then

!        Allocate local scratch space
!        ----------------------------
         NReports  = Meta % NReports
         allocate  ( JulHr ( NReports ), stat = stat )
         if ( stat .ne. No_Error ) then
            call WPErr ( MyName, Alloc ( stat, 'JulHr', NReports ))
            go to 10
         end if

!        Calculate the Julian hour for each report
!        -----------------------------------------
         do iRep = 1, NReports
            Date =       Meta % DateHr ( iRep ) / 100
            Hour = mod ( Meta % DateHr ( iRep ),  100 )
            JulHr ( iRep ) = Julian ( Date, Hour )
         end do

!        Calculate the common data/time
!        ------------------------------
         if ( NReports .gt. 0 ) then
            ObJHr    = ( maxval ( JulHr ) + minval ( JulHr ) + 1 ) / 2
            ObDHr    =   CalDHr  ( ObJHr )
            ObDate   =   ObDHr  ( 1 )
            ObHr     =   ObDHr  ( 2 )
         else
            ObDate   = 0
            ObHr     = 0
         end if

!        ... and modify the data/hour and launch time accordingly
!        --------------------------------------------------------
         do iRep = 1, NReports
            Meta % DateHr ( iRep ) = ObDate * 100 + ObHr
            LTime = Meta % LTime ( iRep )
            if ( LTime .ne. IMiss )
     .         Meta % LTime ( iRep ) 
     .            = LTime + 3600 * ( JulHr ( iRep ) - ObJHr )
         end do

         Meta % SynDate = ObDate 
         Meta % SynHr   = ObHr

!        Clean up
!        --------
         deallocate ( JulHr )
      end if

!     Modify the date/hour and launch time for time-series data
!     so that launch times have values between 0 and 59 min
!     ---------------------------------------------------------
      if ( get_tsmeta ) then
         NReports = TSMeta % NReports
         do iRep = 1, NReports
            LTime = TSMeta % LTime ( iRep )
            if ( LTime .ne. IMiss ) then
            if ( LTime .lt.    0   .or.
     .           LTime .ge. 3600  ) then
               Date   =       TSMeta % DateHr ( iRep ) / 100
               Hour   = mod ( TSMeta % DateHr ( iRep ),  100 )
               LTime1 = modulo ( LTime, 3600 )
               ObJHr  =   Julian ( Date, Hour )
     .                + ( LTime - LTime1 ) / 3600 
               ObDHr  = CalDHr ( ObJHr )
               TSMeta % DateHr ( iRep ) = ObDHr ( 1 ) * 100
     .                                  + ObDHr ( 2 )
               TSMeta % LTime  ( iRep ) = LTime1
            end if
            end if
         end do
      end if

!     Clean up if error status
!     ------------------------
 10   continue
      if ( stat .ne. No_Error ) then
         if ( get_meta   ) call Clean_Meta   (   Meta )
         if ( get_tsmeta ) call Clean_TSMeta ( TSMeta )
         if ( get_obs    ) call Clean_Obs    ( Obs    )
         if ( get_qc     ) call Clean_QC     ( QC     )
      end if

      return
      end subroutine Get_CDat

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  CARDS_iQC () --- Convert to CARDS compatible quality control (QC) marks
! 
! !DESCRIPTION:
!     This function converts a given QC flag to a value that is CARDS
!     compatible which is set by the Complex Quality Control.  The
!     lists of returned values and their meanings is as follows:
!           0 - unchecked value
!           1 - correct value
!           2 - suspect value
!           3 - erroneous value
!           4 - corrected value (original qc is set to 3)
!           5 - calculated value (original value was missing, i.e = 9)
!           9 - missing value
!     If the conversion is impossible for the given input parameters, 
!     QC_Effor and Data_Source, then -1 is returned.  This routine is
!     low level.
!
! !INTERFACE:
!
      function CARDS_iQC ( QC, Element, QC_Effort, Data_Source )
      use m_AtoX, only : AtoI
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len = *), intent (in)  ::
     .   QC                ! Input QC value
      integer,             intent (in)  ::
     .   Element           ! Integer code for element.
                           !   1 = elapsed time
                           !   2 = pressure
                           !   3 = height
                           !   4 = temperature,
                           !   5 = relative humidity
                           !   6 = dew point depression
                           !   7 = wind speed
                           !   8 = wind direction.
      integer,             intent (in)  ::
     .   QC_Effort         ! Quality control effort
                           !   0 = NCDC QC after  Dec 1992
                           !   1 = NCDC QC before Jan 1993
                           !   2 = QC at source
                           !   3 = CARDS QC from CQC
                           !   4 = NCDC Radiation Corrections Dataset (DSI-6348)
                           !   5-7 = reserved
                           !   8 = no QC
                           !   9 = unknown
                           ! note: Algorithm implemented only for values
                           !       0, 1 (for Data_Sounce = 1, only), 3, and 8
      integer,             intent (in)  ::
     .   Data_Source       ! Two digit integer NN for the data source
                           !  /format where NN corresponds to the format
                           !   DSI63NN.  Valid range 00-99.
!
! !OUTPUT PARAMETERS: 
      integer ::
     .   CARDS_iQC         ! CARDS compatible QC.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     13Dec2006  Redder    Origional code.
! EOP
!....................................................................

      integer iQC, iiQC, iret

      integer, dimension ( 0 : 99 ), parameter ::
     .   QC_NCDC_Mass =  (/ 1, 5, 5,            ! QC =  0 -  2,  NCDC QC after
     .                    ( 4, iiQC = 1, 38 ),  !    =  3 - 40   1992
     .                    ( 9, iiQC = 1, 10 ),  !    = 41 - 50
     .                      9, 9,               !    = 51 - 52
     .                    ( 3, iiQC = 1, 32 ),  !    = 53 - 84
     .                      2, 2,               !    = 85 - 86
     .                    ( 3, iiQC = 1,  3 ),  !    = 87 - 89
     .                      1, 1, 2, 2, 3, 3,   !    = 90 - 95
     .                      4, 4, 0, 0 /),      !    = 96 - 99
     .   QC_NCDC_Wind =  (/ 1, 5, 5,            ! QC =  0 -  2,
     .                    ( 4, iiQC = 1, 38 ),  !    =  3 - 40
     .                    ( 9, iiQC = 1, 10 ),  !    = 41 - 50
     .                      9, 9,               !    = 51 - 52
     .                    ( 3, iiQC = 1,  4 ),  !    = 53 - 57
     .                    ( 2, iiQC = 1,  4 ),  !    = 57 - 60
     .                    ( 3, iiQC = 1, 29 ),  !    = 61 - 89
     .                      1, 1, 2, 2, 3, 3,   !    = 90 - 95
     .                      4, 4, 0, 0 /)       !    = 96 - 99     .
      integer, dimension ( 0 : 99 ), parameter ::
     .   QC_NCDC_pre93 = (/ 1, 2, 2, 3, 4, 5, 4,! QC =  0 -  6 NCDC QC before
     .                      9, 0, 0,            !       7 -  9 1992 for 
     .                    ( 0, iiQC = 1, 90 ) /)!      10 - 99 format DSI6301
      integer, dimension ( 0 : 99 ), parameter ::
     .   QC_CARDS      = (/ 0, 1, 2, 3, 4, 5, 0,! QC =  0 -  6 CARDS QC used
     .                      0, 0, 9,            !       7  - 9 to set invalid
     .                    ( 0, iiQC = 1, 90 ) /)!      10 - 99 values to 0

      iQC = -1 ! default

      if      ( QC_Effort .eq. 0 ) then
         iQC = AtoI ( QC, iret )
         if ( iret    .eq.  0 .and.
     .        iQC     .ge.  0 .and.
     .        iQC     .le. 99 ) then
            if ( Element .le.  6 ) iQC = QC_NCDC_Mass ( iQC )
            if ( Element .ge.  7 ) iQC = QC_NCDC_Wind ( iQC )
         else
            iQC = 0
         end if

      else if ( QC_Effort .eq. 1 ) then
         if ( Data_Source .eq. 1 ) then
            iQC = AtoI ( QC, iret )
            if ( iret     .eq.  0 .and.
     .           iQC      .ge.  0 .and.
     .           iQC      .le. 99 ) then
               if ( Data_Source .eq. 1 ) iQC = QC_NCDC_pre93 ( iQC )
            else
               iQC = 0
            end if
         end if
      else if ( QC_Effort .eq. 3 ) then
         iQC = AtoI ( QC, iret )
         if ( iret .ne. 0 ) iQC = 0

!      else if ( QC_Effort .eq. 8 ) then ! too many bad errors
!         iQC = 0                        !  discovered when 
      end if                             !  QC_Effort = 8

      CARDS_iQC = iQC

      return
      end function CARDS_iQC

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
!
!     02May2002  Redder    Origional code.
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

      end subroutine ReadLn

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Put_Header () ---  Write header info to beginning of new file
! 
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Put_Header ( Outfile, stat, BegDate, EndDate, ! Required and
     .                        replace )                        ! optional
      use m_SunAlt,   only : Julian, Check_DateTime            ! arguments
      use m_AdvError, only : ItoA, PErr, WPErr
      use m_SysIO,    only : LUAvail
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len = *), intent (in)  ::
     .   OutFile           ! Output file name
!     Note: Output files are opened with "status" set to "new"
      integer,             intent (in), optional  ::
     .   BegDate (:),      ! The earliest and
     .   EndDate (:)       ! ... latest date and hour allowed for a
                           !   selected report.  XDate(1) = YYYYMMDD
                           !   and XDate(2) = HH
      logical,             intent (in), optional ::
     .   replace           ! = .true. to delete the output file (if it
                           !   pre-exists) and replace it with a new file. 
!                          !   Default: replace is set to .false.
! !OUTPUT PARAMETERS:
      integer,             intent (out) ::
     .   stat              ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08May2002  C. Redder  Origional code.
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     06Jan2003  C. Redder  Changed use m_Julian to use m_SunAlt
!     15Jan2003  C. Redder  Changed use module from m_ioutil to m_SysIO
!     20Dec2006  C. Redder  Replaced the arguments, JMin and JMax, with
!                           BegDate and EndDate.
! EOP
!....................................................................
      character (len=*), parameter ::
     .   MyName = MyModule // '::Put_Header'
      character (len=MaxLRec + 255) :: Record
      character (len=12) Status
      character (len=80) Temp, Message
      integer :: LRec, LTemp, Date, Hour, lu_out, JMin, JMax
      logical :: replace_file

!     Implement options
!     -----------------
      replace_file = .false.
      if ( present ( replace )) replace_file = replace

!     Header indicator
!     ----------------
      Record ( :2 ) = '% '
      LRec          =  2

!     Begin date/time
!     ---------------
      if ( present ( BegDate )) then
         Date  = BegDate (1)
         Hour  = BegDate (2)
         if ( Check_DateTime ( Date, Hour, Message ) .ne. 0 ) then
            call WPErr ( MyName, trim ( Message )
     .       // '\n\C   Begin date tag = ' // trim ( ItoA ( BegDate(1)))
     .       //   '\n     ... hour     = ' // trim ( ItoA ( BegDate(2)))
     .       //   '\n   Output file    = ' // trim        ( OutFile  ))
            stat = Bad_Argument
            return
         end if
         write ( Temp, '("Begin date/time: ",a," ",i2.2,"z")' )
     .      trim ( ItoA ( Date )), Hour
         LTemp = len_trim ( Temp )
         Record ( LRec + 1: LRec + LTemp + 3 ) = Temp ( : LTemp )
         LRec  = LRec + LTemp + 3 ! Account for trailing blank
      end if

!     End date/time
!     -------------
      if ( present ( EndDate )) then
         Date  = EndDate (1)
         Hour  = EndDate (2)
         if ( Check_DateTime ( Date, Hour, Message ) .ne. 0 ) then
            call WPErr ( MyName, trim ( Message )
     .       // '\n\C   End date tag = ' // trim ( ItoA ( EndDate(1)))
     .       //   '\n   ... hour     = ' // trim ( ItoA ( EndDate(2)))
     .       //   '\n   Output file  = ' // trim        ( OutFile  ))
            stat = Bad_Argument
            return 
         end if
         write ( Temp, '("End date/time: ",a," ",i2.2,"z")' )
     .      trim ( ItoA ( Date )), Hour
         LTemp = len_trim ( Temp )
         Record ( LRec + 1: LRec + LTemp + 3 ) = Temp ( : LTemp )
         LRec  = LRec + LTemp + 3 ! Account for trailing blank
      end if

!     Ensure that the datas are consistent
!     ------------------------------------
      if ( present ( BegDate ) .and.
     .     present ( EndDate )) then
         JMin = Julian ( BegDate (1), BegDate(2))
         JMax = Julian ( EndDate (1), EndDate(2))
         if ( JMin .gt. JMax ) then
            write (Temp,'(2(i2.2))')  BegDate(2), EndDate(2)
            call WPErr ( MyName, 'The begin date (='         
     .                   //       trim ( ItoA ( BegDate(1))) 
     .                   //                        Temp(1:2) 
     .                   //      ') is after the end date (='
     .                   //       trim ( ItoA ( EndDate(1)))
     .                   //                        Temp(3:4) 
     .                   // '). \n   Output file   = '
     .                   //          trim ( OutFile )) 
            stat = Bad_Argument
            return
         end if
      end if

!     Determine another available logical unit for the output file
!     ------------------------------------------------------------
      lu_out = LUAvail()
      if ( lu_out .lt. 0 ) then
         call PErr ( MyName, 'No logical units available for '
     .                    // 'the output file. \W ' )
         stat = No_LUAvail
         return
      end if

!     ... open the next output file
!     -----------------------------
      Status = 'new'
      if ( replace_file ) Status = 'replace'
      open ( unit     =  lu_out,
     .       file     =  OutFile,
     .       form     = 'formatted',
     .       access   = 'sequential',
     .       status   =  Status,
     .       recl     =  MaxLRec,
     .       iostat   =  stat )
      if ( stat .ne. 0 ) then
         call PErr ( MyName,
     .              'Open error (iostat = '
     .         //    trim ( ItoA ( stat ) ) // ') \C'
     .         // '/n   Output file = ' // trim ( OutFile ))
         stat = Open_Error
         return
      end if

!     Write the header to file
!     ------------------------
      write ( unit   = lu_out,
     .        fmt    = '(a)',
     .        iostat = stat ) Record ( : LRec )
      if ( stat .ne. 0 ) then
         call PErr ( MyName,
     .              'Write error (iostat = '
     .         //    trim ( ItoA ( stat )) // ') \C'
     .         // '\n   Output file = ' // trim ( OutFile ))
         close ( lu_out )
         stat = Write_Error
         return
      end if

!     ... and close
!     -------------
      close ( lu_out )

      return
      end subroutine Put_Header
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_Header () ---  Get header info from file
! 
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Get_Header ( Infile, stat, BegDate, EndDate )
      use m_SunAlt,   only : Julian, Check_DateTime
      use m_AtoX,     only : AtoI
      use m_AdvError, only : ItoA, PErr, WPErr
      use m_SysIO,    only : LUAvail
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len = *), intent (in)  ::
     .   InFile            ! Output file name
!
! !OUTPUT PARAMETERS:
      integer,             intent (out), optional  ::
     .   BegDate (:),      ! The earliest and
     .   EndDate (:)       ! ... latest date and hour allowed for a
                           !   selected report.  XDate(1) = YYYYMMDD
                           !   and XDate(2) = HH
      integer,             intent (out) ::
     .   stat              ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08May2002  C. Redder  Origional code.
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     06Jan2003  C. Redder  Changed use m_Julian to use m_SunAlt
!     15Jan2003  C. Redder  Changed use module from m_ioutil to m_SysIO
!     20Dec2006  C. Redder  Replaced the arguments, JMin and JMax, with
!                           BegDate and EndDate.
! EOP
!....................................................................

      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_Header'
      character (len=MaxLRec + 255) :: Record
      character (len=10) Temp
      character (len=50) Label
      integer :: LRec, LTemp, LLabel, iLabel, iBeg
      integer :: Date,  Hour, lu_in,  iret1,  iret2, JMin, JMax
      logical :: eor, eof

!     Determine another available logical unit for the input file
!     -----------------------------------------------------------
      lu_in = LUAvail()
      if ( lu_in .lt. 0 ) then
         call PErr ( MyName, 'No logical units available for '
     .                    // 'the input file. \W ' )
         stat = No_LUAvail
         return
      end if

!     ... open the next input file
!     ----------------------------
      open ( unit     =  lu_in,
     .       file     =  InFile,
     .       form     = 'formatted',
     .       access   = 'sequential',
     .       status   = 'old',
     .       recl     =  MaxLRec,
     .       iostat   =  stat )
      if ( stat .ne. 0 ) then
         call PErr ( MyName,
     .              'Open error (iostat = '
     .         //    trim ( ItoA ( stat ) ) // ') \C'
     .         // '\n   Output file = ' // trim ( InFile ))
         stat = Open_Error
         return
      end if

!     ... read the record
!     -------------------
      call ReadLn ( lu_in, LRec, Record, eor, eof, stat )
      if ( stat .ne. 0 ) then
         call PErr ( MyName, 'Read error (iostat = '
     .                  //    trim ( ItoA ( stat ) ) // ') \C'
     .                  // '\n   Input file = ' // trim ( InFile))
         close ( lu_in )
         stat = Read_Error
         return
      end if

!     ... and close
!     -------------
      close ( lu_in )

!     Check to ensure that the header exists in the first record
!     ----------------------------------------------------------
      if ( eof ) then
         stat = No_Header
      else if ( Record ( 1:1 ) .ne. '%' ) then
         stat = No_Header
      else 
         stat = 0
      end if
      if ( stat .ne. 0 ) then
         call PErr ( MyName, 'Header not found in input file \C'
     .                  // '\n   Input file = ' // trim ( InFile))
         return
      end if

!     Begin date/time
!     ---------------
      if ( present ( BegDate )) then
         BegDate (1) =  IMiss
         BegDate (2) =  IMiss
          Label  = 'Begin date/time: '
         LLabel  =  len_trim ( Label ) + 1
         iLabel  =  index ( Record ( :LRec ), Label ( :LLabel ))
         if ( iLabel .ne. 0 ) then
            iBeg = iLabel + LLabel
            Temp = Record ( iBeg : iBeg + 8 - 1 )
            Date = AtoI ( Temp ( : 8 ), iret1 )
            iBeg = iLabel + LLabel + 9
            Temp = Record ( iBeg : iBeg + 2 - 1 )
            Hour = AtoI ( Temp ( : 2 ), iret2 )
            if ( iret1 .eq. 0 .and.
     .           iret2 .eq. 0 .and.
     .           Check_DateTime ( Date, Hour ) .eq. 0 ) then
               BegDate (1) = Date
               BegDate (2) = Hour
            end if
         end if
      end if

!     End date/time
!     -------------
      if ( present ( EndDate )) then
         EndDate (1) = IMiss
         EndDate (2) = IMiss
          Label  = 'End date/time: '
         LLabel  =  len_trim ( Label ) + 1
         iLabel  =  index ( Record ( :LRec ), Label ( :LLabel ))
         if ( iLabel .ne. 0 ) then
            iBeg = iLabel + LLabel
            Temp = Record ( iBeg : iBeg + 8 - 1 )
            Date = AtoI ( Temp ( : 8 ), iret1 )
            iBeg = iLabel + LLabel + 9
            Temp = Record ( iBeg : iBeg + 2 - 1 )
            Hour = AtoI ( Temp ( : 2 ), iret2 )
            if ( iret1 .eq. 0 .and.
     .           iret2 .eq. 0 .and.
     .           Check_DateTime ( Date, Hour ) .eq. 0 ) then
               EndDate (1) = Date
               EndDate (2) = Hour
            end if
         end if
      end if

!     Ensure that the datas are consistent
!     ------------------------------------
      if ( present ( BegDate ) .and.
     .     present ( EndDate )) then
         JMin = Julian ( BegDate (1), BegDate(2))
         JMax = Julian ( EndDate (1), EndDate(2))
         if ( JMin .gt. JMax ) then
            write (Temp,'(2(i2.2))')  BegDate(2), EndDate(2)
            call WPErr ( MyName, 'The begin date (='         
     .                   //       trim ( ItoA ( BegDate(1)))
     .                   //                        Temp(1:2)
     .                   //      ') is after the end date (='
     .                   //       trim ( ItoA ( EndDate(1)))
     .                   //                        Temp(3:4) 
     .                   // '). \n   Input file   = '
     .                   //          trim ( InFile )) 
            stat = Bad_Argument
            return
         end if
      end if

      return
      end subroutine Get_Header

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  CDS_TSIndex () --- Select report from a CARDs time-series dataset
! 
! !DESCRIPTION:
!     This function selects report(s) based on the WMO station ID,
!     synoptic date/hour and launch time.  If no acceptable report
!     is found, then the returned report index number is set to zero.
!
! !INTERFACE:
      function CDS_TSIndex ( TSMeta, StnID, DateHr, LTime )
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( CARDS_tsmeta ), intent (in) :: 
     .   TSMeta            ! CARDS time-series meta-data structure
      character (len=*),     intent (in) ::
     .   StnID             ! WMO station ID
      integer,               intent (in) ::
     .   DateHr,           ! Synoptic date hour (YYYYMMDDHH)
     .   LTime             ! Lauch time (seconds since DateHR)
!
! !OUTPUT PARAMETERS:
      integer, dimension (2) ::
     .   CDS_TSIndex       ! (1) = Returned report index
                           ! (2) = The number of reports with the same
!                          !       station ID and temporal parameters
! !SEE ALSO:
!
! !REVISION HISTORY:
!     20Apr2007  C. Redder  Origional code
! EOP
!-------------------------------------------------------------------------
      character (len = len ( TSMeta % StnID )),
     .         dimension (:), pointer :: CStnID
      integer, dimension (:), pointer :: CDateHr, CLTime
      integer :: NCReports

      NCReports =  TSMeta % NReports
      CStnID    => TSMeta % StnID  ( : NCReports )
      CDateHr   => TSMeta % DateHr ( : NCReports )
      CLTime    => TSMeta % LTime  ( : NCReports )

      CDS_TSIndex = CDS_Index1 ( CStnID, CDateHr, CLTime,
     .                            StnID,  DateHr,  LTime )
      return
      end function CDS_TSIndex
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  CDS_Index1 () --- Select report(s) from a CARDs dataset
! 
! !DESCRIPTION:
!     This function locates the report(s) that have the desired WMO
!     station ID, synoptic date/hour and launch time.  If none is 
!     found, then the function searches for the report with the 
!     desired station ID but with temporal parameters closest to the
!     desired parameters within a certain tolerance.  If no acceptable
!     report is found, then returned report index number is set to zero.
!     This is a low-level routine.
!
! !INTERFACE:
      function CDS_Index1 ( CStnID, CDateHr, CLTime,
     .                       StnID,  DateHr,  LTime )
      use  m_Range,     only : Range2
      use  m_SunAlt,    only : Julian, CalDHr
!
! !INPUT PARAMETERS:
      implicit   NONE
      character (len=*),     intent (in) ::
     .  CStnID  (:),       ! WMO station IDs
     .   StnID
      integer,               intent (in) ::
     .  CDateHr (:),       ! Synoptic date hour (YYYYMMDDHH)
     .   DateHr,
     .  CLTime  (:),       ! Lauch time (seconds since DateHR,
     .   LTime             !             [0,3600 s) )
!
! !OUTPUT PARAMETERS:
      integer, dimension (2) ::
     .   CDS_Index1        ! (1) = Returned report index 
                           ! (2) = The number of reports with the same
!                          !       station ID and temporal parameters
! !SEE ALSO:
!
! !REVISION HISTORY:
!     18Apr2007  C. Redder  Origional code
! EOP
!-------------------------------------------------------------------------

      integer, parameter :: DTimeMax = 3600
      integer :: NCReports, LRange (3), iSBeg, iSEnd, SLen,
     .                                  iCBeg, iCEnd, CLen,
     .                                  iDBeg, iDEnd
      integer :: Date, Hour, LTime1, ObJHr, ObDHr (2), DateHr1
      integer :: DTime1,   CLTime1, ObJHr1
      integer :: DTime2 ,  CLTime2, ObJHr2
      integer :: CDateHr0, CLTime0
      integer :: iReport,  NReports

!     Set index to default
!     --------------------
      CDS_Index1 = 0

!     Match the station id
!     --------------------
      NCReports =  min ( size ( CStnID  ),
     .                   size ( CDateHr ),
     .                   size  (CLTime  ))
      LRange    =  Range2 ( CStnID, StnID )
      iSBeg     =  LRange (1)
       SLen     =  LRange (2)
      iSEnd     =  LRange (3)

!     Return if no ncdc station exists
!     --------------------------------
      if ( SLen .le. 0 ) return

!     Ensure that the launch time is between 0 and 59 min
!     and adjust the ob date/hour and launch time accordingly
!     -------------------------------------------------------
      LTime1  = LTime
      Date    =       DateHr / 100
      Hour    = mod ( DateHr,  100 )
      ObJHr   = Julian ( Date, Hour )
      DateHr1 = DateHr
      if ( LTime .lt. 0 .or.
     .     LTime .ge. 3600 ) then
         LTime1  = modulo  ( LTime, 3600 )
         ObJHr   = ObJHr + ( LTime - LTime1 ) / 3600 
         ObDHr   = CalDHr ( ObJHr )
         DateHr1 = ObDHr ( 1 ) * 100 + ObDHr ( 2 )
      end if

!     Match the observation date/hour
!     -------------------------------
      LRange    =  Range2 ( CDateHr, DateHr1, iSBeg, SLen )
      iCBeg     =  LRange (1)
       CLen     =  LRange (2)
      iCEnd     =  LRange (3)

!     ... and launch time
!     -------------------
      if ( CLen .gt. 0 ) then
         LRange =  Range2 ( CLTime, LTime1, iCBeg, CLen )
         iCBeg  =  LRange (1)
          CLen  =  LRange (2)
         iCEnd  =  LRange (3)
      end if

!     If the report(s) with the desired launch time is not found
!     ----------------------------------------------------------
      if ( CLen .eq. 0 ) then

!        ... then determine the report with ...
!        --------------------------------------
         iDBeg   = min ( iSEnd, iCEnd )
         iDEnd   = max ( iSBeg, iDBeg + 1 )
         Date    =       CDateHr ( iDBeg ) / 100
         Hour    = mod ( CDateHr ( iDBeg ),  100 )
         ObJHr1  = Julian ( Date, Hour )
         CLTime1 = CLTime ( iDBeg ) - 3600 * ( ObJHr - ObJHr1 )
         DTime1  = abs ( LTime1 - CLTime1 )
         Date    =       CDateHr ( iDEnd ) / 100
         Hour    = mod ( CDateHr ( iDEnd ),  100 )
         ObJHr2  = Julian ( Date, Hour )
         CLTime2 = CLTime ( iDBeg ) - 3600 * ( ObJHr - ObJHr2 )
         DTime2  = abs ( LTime1 - CLTime2 )

!        ... the closest time tag that is within tolerance
!        ------------------------------------------------- 
         if    ( DTime1 .le. DTime2   ) then
            if ( DTime1 .le. DTimeMax ) then
               iReport  = iDBeg
            else
               iReport  = 0
            end if
         else
            if ( DTime2 .le. DTimeMax ) then
               iReport  = iDEnd
            else
               iReport  = 0
            end if
         end if

!        Return if no acceptable report is found
!        ---------------------------------------
         if ( iReport .eq. 0 ) return

!        Determine the location and number of reports
!        ... with the same observation date/hour
!        --------------------------------------------
         CDateHr0   = CDateHr ( iReport )
         LRange     = Range2  ( CDateHr, CDateHr0, iSBeg, SLen )
         iCBeg      = LRange  (1)
          CLen      = LRange  (2)

!        ... and launch time
!        -------------------
         if ( CLen .gt. 1 ) then
            CLTime0 = CLTime  ( iReport )
            LRange  = Range2  ( CLTime,  CLTime0,  iCBeg, CLen )
            iCBeg   = LRange  (1)
             CLen   = LRange  (2)
         end if

      end if

!     Save the desired parameters
!     ---------------------------
      CDS_Index1 (1) = iCBeg
      CDS_Index1 (2) =  CLen

      return
      end function CDS_Index1
     
!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  CDS_ETime1TS () --- Derive elapsed data from CARDS time-series data
! 
! !DESCRIPTION:
!     This routine derives the elapsed time from the CARDS data
!     by interpolation and/or extrapolation.
!
! !INTERFACE:
      subroutine CDS_ETime1TS ( TSData, iCReport, Z_sfc, Height, ETime )
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( CARDS_tsdata ), intent (in) :: 
     .   TSData          ! CARDS data
!
! !INPUT PARAMETERS:
      integer,             intent (in)  ::
     .   iCReport        ! Index number of CARDS report    
      real,                intent (in)  ::
     .   Z_sfc,          ! From non-CARDS data, surface height (m)
     .   Height (:)      ! ... and height at each level (m)
!
! !OUTPUT PARAMETERS:
      real,                intent (inout) ::
     .   ETime  (:)      ! ... derived elapsed time (s)
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     30Apr2007  C. Redder  Origional code
! EOP
!-------------------------------------------------------------------------

      logical, dimension (:), pointer :: CMask
      integer, dimension (:), pointer :: CETime_QC, CHeight_QC
      real,    dimension (:), pointer :: CETime,    CHeight
      integer :: NCReports, NCLevels, iBegRep, iEndRep, stat
      real :: CZ_sfc

      NCReports = TSData % TSMeta % NReports
      if ( iCReport .ge. 1 .and.
     .     iCReport .le. NCReports ) then
         iBegRep  =  TSData % TSMeta % Loc     ( iCReport )
         NCLevels =  TSData % TSMeta % NLevels ( iCReport )
         CZ_sfc   =  TSData % TSMeta % Elev    ( iCReport )
      else
         iBegRep  =  1
         NCLevels =  0
         CZ_sfc   =  0.0
      end if
      iEndRep     =  iBegRep + NCLevels - 1

      CMask       => TSData % QC  % Mask   ( iBegRep : iEndRep )
      CETime_QC   => TSData % QC  % ETime  ( iBegRep : iEndRep )
      CHeight_QC  => TSData % QC  % Height ( iBegRep : iEndRep )
      CETime      => TSData % Obs % ETime  ( iBegRep : iEndRep )
      CHeight     => TSData % Obs % Height ( iBegRep : iEndRep )

      call CDS_ETMask ( CZ_sfc, CHeight, CHeight_QC,
     .                          CETime,  CETime_QC, CMask )
      call CDS_ETime1 ( CMask,  CZ_sfc,  CHeight,   CETime,
     .                           Z_sfc,   Height,    ETime, stat )

      return
      end subroutine CDS_ETime1TS
!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  CDS_ETimeTS () --- Derive elapsed data from CARDS time-series data
! 
! !DESCRIPTION:
!     This routine derives the elapsed time from the CARDS data
!     by interpolation and/or extrapolation.
!
! !INTERFACE:
      subroutine CDS_ETimeTS ( TSData, RadProf, iCReport )
      use        m_RadData,   only : ETHist_CARDS, radcor_profile
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( CARDS_tsdata ),   intent (in)    :: 
     .   TSData              ! CARDS data
      integer, optional,       intent (in)    ::
     .   iCReport            ! Index number of CARDS report    
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profile ), intent (inout) ::
     .   RadProf             ! Data structure for the selected
!                              profile.
! !SEE ALSO:
!
! !REVISION HISTORY:
!     01May2007  C. Redder  Origional code
! EOP
!-------------------------------------------------------------------------

      character ( len = LStnID ), dimension (:), pointer :: CStnID
      logical, dimension (:), pointer :: CMask
      integer, dimension (:), pointer ::
     .   CETime_QC, CHeight_QC, CDateHr, CLTime
      real,    dimension (:), pointer ::
     .    CETime,    CHeight, ETime, Height
      character (len = len ( RadProf % StnID )) ::  StnID
      integer :: NCReports, NCLevels, iBegRep, iEndRep, iCRep, stat,
     .           DateHr, LTime, NLevels, iCReps(2)
      real :: CZ_sfc, Z_sfc

!     Use predefined report index number
!     ----------------------------------
      NCReports = TSData % TSMeta % NReports
      iCRep     = 0
      if ( present ( iCReport )) then
         if ( iCReport .ge. 1 .and.
     .        iCReport .le. NCReports ) then
            iCRep =  iCReport
         else
            iCRep =  0
         end if

!     ... or obtained it from the in-house routine
!     --------------------------------------------
      else
         CStnID   => TSData  % TSMeta % StnID  ( : NCReports )
         CDateHr  => TSData  % TSMeta % DateHr ( : NCReports )
         CLTime   => TSData  % TSMeta % LTime  ( : NCReports )
          StnID   =  RadProf % StnID
          DateHr  =  RadProf % Date * 100 + RadProf % Time / 10000
          LTime   =  RadProf % LTime
         iCReps   =  CDS_Index1 ( CStnID, CDateHr, CLTime,
     .                             StnID,  DateHr,  LTime )
         iCRep    = iCReps ( 1 )
         if ( iCReps ( 2 ) .gt. 1 )
     .      iCRep = iCReps ( 1 ) + iCReps ( 2 ) - 1
      end if

!     Return if report is not found
!     -----------------------------
      if ( iCRep .eq. 0 ) return

!     Get navigators for CARDS report
!     -------------------------------
      iBegRep     =  TSData  % TSMeta % Loc     ( iCRep )
      NCLevels    =  TSData  % TSMeta % NLevels ( iCRep )
      CZ_sfc      =  TSData  % TSMeta % Elev    ( iCRep )
      iEndRep     =  iBegRep + NCLevels - 1

!     ... and isolate the desired array segments
!     ------------------------------------------
      CMask       => TSData  % QC  % Mask   ( iBegRep : iEndRep )
      CETime_QC   => TSData  % QC  % ETime  ( iBegRep : iEndRep )
      CHeight_QC  => TSData  % QC  % Height ( iBegRep : iEndRep )
      CETime      => TSData  % Obs % ETime  ( iBegRep : iEndRep )
      CHeight     => TSData  % Obs % Height ( iBegRep : iEndRep )

!     Set parameters concerning the target (i.e. non-CARDS) profile
!     -------------------------------------------------------------
      NLevels     =  RadProf % by_Levels % NLev
      Z_sfc       =  RadProf % Elev
      Height      => RadProf % by_Levels % Z     ( : NLevels )
      ETime       => RadProf % by_Levels % ETime ( : NLevels )

!     Mask out unwanted CARDS data
!     ----------------------------
      call CDS_ETMask ( CZ_sfc, CHeight, CHeight_QC,
     .                          CETime,  CETime_QC, CMask )

!     ... and check the CARDS profile and generate the elapsed time
!     -------------------------------------------------------------
      call CDS_ETime1 ( CMask,  CZ_sfc,  CHeight,   CETime,
     .                           Z_sfc,   Height,    ETime, stat )

!     Indicate that elapsed time is from CARDS data base
!     --------------------------------------------------
      if ( stat .eq. 0 ) RadProf % ETHist = ETHist_CARDS

      return
      end subroutine CDS_ETimeTS
!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  CDS_ETMask () --- Set mask based on QC for height and elapsed time
! 
! !DESCRIPTION:
!     This routine sets the mask based on the QC for the height and elapsed
!     time.  If no QC was done (i.e. QC=0), then the mask is set to false. 
!     This is a low level routine
!
! !INTERFACE:
      subroutine CDS_ETMask ( Height_sfc, Height, Height_QC,
     .                                    ETime,   ETime_QC, Mask )
!
! !INPUT PARAMETERS:
      implicit   NONE
      real,                intent (in)  ::
     .   Height_sfc,       ! Height at surface ...
     .   Height    (:),    ! ... and upper levels (m)
     .   ETime     (:)     ! Elapsed time (s)
      integer,             intent (in)  ::
     .   Height_QC (:),    ! QC flags for height ...
     .   ETime_QC  (:)     ! ... and elapsed time (m)
!
! !OUTPUT PARAMETERS:
      logical,             intent (out) ::
     .   Mask      (:)     ! Mask (= .true. if ob is valid. .false.
     .                     !   otherwise) 
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     30Apr2007  C. Redder  Origional code
! EOP
!-------------------------------------------------------------------------

      integer, parameter ::
     .   QCListSz        = 3,
     .!   QCListSz        = 4,
     .   QCMin           = 0,
     .   QCMax           = 9
      integer, parameter ::
     .   QCList ( QCListSz ) = (/ 1, 4, 5 /)
!     .   QCList ( QCListSz ) = (/ 0, 1, 4, 5 /)
      logical :: valid_QC ( QCMin : QCMax ), valid_etime
      integer :: NLevels, iLev, E_QC, Z_QC, iList


!     Create logical table for valid/invalid NCDC QC marks
!     ----------------------------------------------------
      valid_QC = .false.
      do iList = 1, QCListSz
         valid_QC ( QCList ( iList )) = .true.
      end do

!     Determine the number of levels
!     ------------------------------
      NLevels = min ( size ( Height    ),
     .                size ( Height_QC ),
     .                size ( ETime     ),
     .                size ( ETime_QC  ),
     .                size ( Mask     ))
      if ( NLevels .eq. 0 ) return

!     And mask out the level below the surface
!     or with a bad elapse time or height ob
!     ----------------------------------------
      do iLev = 1, NLevels
         E_QC = ETime_QC  ( iLev )
         Z_QC = Height_QC ( iLev )
         Mask ( iLev )    = .false.
         if ( E_QC .ge. QCMin .and.
     .        E_QC .le. QCMax )
     .      Mask ( iLev ) =  valid_QC ( E_QC )
         if ( Z_QC .ge. QCMin .and.
     .        Z_QC .le. QCMax ) then
            Mask ( iLev ) =  valid_QC ( Z_QC ) .and. Mask ( iLev )
         else
            Mask ( iLev ) = .false.
         end if
         if ( Mask ( iLev ))
     .      Mask ( iLev ) = Height ( iLev ) .ge. Height_sfc - 0.5
     .        ! Heights are reported in whole meters
      end do

      return
      end subroutine CDS_ETMask
!.................................................................
!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  CDS_ETime1 () --- Derive elapsed data from CARDS data
! 
! !DESCRIPTION:
!     This routine derives the elapsed time from the CARDS data
!     by interpolation and/or extrapolation.  The routine
!     checks to ensure that sufficient data exists to generate 
!     robust values.  If no data is generated then the return
!     status is set to a non-zero value and the elapsed time
!     is set to the value on input.  This is a low level routine
!
! !INTERFACE:
      subroutine CDS_ETime1 ( CMask, CZ_sfc, CHeight, CETime,
     .                                Z_sfc,  Height,  ETime, stat )
      use        m_soundings, only : Z2Y
!
! !INPUT PARAMETERS:
      implicit   NONE
      logical,             intent (in)  ::
     .   CMask   (:)     ! Mask (= .true. if level is valid, .false.
      real,                intent (in)  ::       ! otherise)
     .   CZ_sfc,         ! From CARDS data, surface height (m),
     .   CHeight (:),    ! ... height at each level (m)
     .   CETime  (:),    ! ... reported elapsed time (s)
     .    Z_sfc,         ! From non-CARDS data, surface height (m)
     .    Height (:)     ! ... and height at each level (m)
!
! !OUTPUT PARAMETERS:
      real,                intent (inout) ::
     .    ETime  (:)     ! ... derived elapsed time (s)
      integer,             intent (out)   ::
     .    stat           ! Return status code (=0 if ET is successfully
!                        !     derived)

! !SEE ALSO:
!
! !REVISION HISTORY:
!     30Apr2007  C. Redder  Origional code
! EOP
!-------------------------------------------------------------------------

      real,    parameter ::
     .   dPThr           = 0.3,
     .   ExtrapThr       = 0.2,
     .   ZExt_Max        = 5000.0 + 0.5,
     .   ZDiff_Max       = 5000.0 + 0.5, ! in m
     .   ETDiff_Min      =  300.0 - 0.5  ! in sec
      logical  :: accept, thin_layer, valid_etime
      integer, parameter :: DTimeMax = 3600
      real    :: ZZ, ZBottom, ZTop
      real    :: CZ, CZTop, CZBottom, CZBelow
      real    :: CET,  CETBottom, CETTop, RRTop
      real    :: ET_sfc (1), ETThr
      integer :: NCLevels, iLev, NLevels, NLevels_Below
      integer :: iTop, iBottom, iExtrap

      stat = 1
      NCLevels = min ( size ( CMask   ),
     .                 size ( CHeight ),
     .                 size ( CETime  ))
      NLevels  = min ( size (  Height ),
     .                 size (  ETime  ))

!     Find the top and bottom levels
!     ------------------------------
      iBottom =  NCLevels + 1
      do iLev = 1, NCLevels
         if ( CMask ( iLev )) then
            iBottom = iLev
            exit
         end if
      end do
      iTop    =  0
      do iLev = NCLevels, 1, -1
         if ( CMask ( iLev )) then
            iTop = iLev
            exit
         end if
      end do

!     Check if at least two levels contain valid elapsed time and that
!     the vertical distance between two successive valid levels does not
!     exceed a certain threshold..
!     ----------------------------------------------------------------
      accept  = iTop .gt. iBottom
      CZBelow = CHeight ( iBottom )
      do iLev = iBottom + 1, iTop
         if ( CMask ( iLev )) then
            CZ = CHeight ( iLev )
            if ( CZ - CZBelow .gt. ZDiff_Max ) then
               accept = .false.
               exit
            end if
            CZBelow = CZ
         end if
      end do

!     Ensure sufficient vertical distance between the top and bottom
!     --------------------------------------------------------------
      if ( accept ) then
          ZTop     = Height  ( NLevels )
          ZBottom  = Z_sfc
         CETTop    = CETime  ( iTop )
         CETBottom = CETime  ( iBottom )
         CZTop     = CHeight ( iTop    )
         CZBottom  = CHeight ( iBottom )
         accept    = (( ZTop     - CZTop      .le. ZExt_Max    .and.
     .                 CETTop    - CETBottom  .gt. ETDiff_Min ) .or.
     .                ( CZTop   .ge. ZTop ))
     .          .and. ( CZBottom - ZBottom    .le.  ZDiff_Max )
      end if

!     Return if sounding info is not acceptable
!     -----------------------------------------
      if ( .not. accept ) return

!     Interpolate the elapsed time onto levels of the profile
!     -------------------------------------------------------
      call Z2Y ( CHeight, CMask, CETime, Height, ETime )

!     If necessary, interpolate below the lowest valid CARDS level
!     assuming that the elapsed time is zero at the surface.
!     ------------------------------------------------------------
      NLevels_Below = 0
      do iLev = 1, NLevels
         ZZ = Height ( iLev )
         if ( ZZ .ge. CZBottom ) exit
         NLevels_Below = NLevels_Below + 1
      end do
      if ( NLevels_Below .gt. 0 )
     .   call Z2Y ( (/ CZ_sfc,  CZBottom /),
     .              (/ .true.,  .true.   /),
     .              (/  0.0 ,  CETBottom /),
     .               Height ( : NLevels_Below ),
     .               ETime  ( : NLevels_Below ))

!     If extrapolation is to be performed then two levels are
!     necessry.  Find the second level just below the top
!     to ensure that the extrapolation is stable and robust.
!     -----------------------------------------------------
      ETThr     = CETTop - ETDiff_Min
      iExtrap   = iBottom
      do iLev   = iTop - 1, iBottom + 1, -1 
         if ( CMask ( iLev )) then
            CET = CETime ( iLev )
            if ( CET .le. ETThr ) then
               iExtrap = iLev
               exit
            end if
         end if
      end do
      RRTop = ( CZTop - CHeight ( iExtrap )) / ( CETTop - CET )

!     Extrapolate
!     -----------
      do iLev = 1, NLevels 
         ZZ = Height ( iLev )
         if ( ZZ .gt. CZTop    )
     .      ETime ( iLev ) = CETTop    + ( ZZ - CZTop    ) / RRTop
      end do

!     Set elapsed time to 0 for all underground observations and
!     adjust all other levels to ensure continuity at the surface
!     -----------------------------------------------------------
      call Z2Y ( Height, ETime, (/ Z_sfc /), ET_sfc )

      do iLev = 1, NLevels
         ZZ = Height ( iLev )
         if ( ZZ .ge. Z_sfc ) then
            ETime ( iLev ) = ETime ( iLev ) - ET_sfc (1)
         else
            ETime ( iLev ) = 0.0
         end if
      end do

      stat = 0

      return
      end subroutine CDS_Etime1

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  CDS_ETimeS () --- Extract elapsed time from CARDS data structure
! 
! !DESCRIPTION:
!     This routine matches the given profile with an NCDC CARDS report
!     and then extracts and intepolated/extrapolates the elapsed time
!     from the matched NCDC CARDS report.
!
! !INTERFACE:
      subroutine CDS_ETimeS ( CARDS, Profile )
      use        m_Range,     only : Range2, Range
      use        m_soundings, only : Z2Y
      use        m_SunAlt,    only : Julian
      use        m_RadData,   only : ETHist_CARDS, radcor_profile
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( CARDS_data ),     intent (in)    :: 
     .   CARDS             ! CARDS data
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profile ), intent (inout) ::
     .   Profile           ! Data structure for the selected profile.
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     28Jan2004  C. Redder  Origional code adapted from ods_etime
!     30Aug2006  C. Redder  Moved subroutine from the main driver,
!                           radcor, in file radcor.f  Changed data
!                           structure of argument, CARDS from
!                           CARDS_space.  Removed the subroutine's
!                           dependence of Mask within the data
!                           structure, radcor_profiles. 
!    30Jan2007   C. Redder  Made changes in determining if the 
!                           CARDS profile is acceptable and
!                           selecting levels for extrapolation.
!    30Apr2007   C. Redder  Changed the routine name from CDS_ETime 
!                           to CDS_ETimeS
! EOP
!-------------------------------------------------------------------------

      integer, parameter ::
     .   QCListSz        = 4,
     .   QCMin           = 0,
     .   QCMax           = 9
      real,    parameter ::
     .   dPThr           = 0.3,
     .   ExtrapThr       = 0.2,
     .   ZExt_Max        = 5000.0 + 0.5,
     .   ZDiff_Max       = 5000.0 + 0.5, ! in m
     .   ETDiff_Min      =  300.0 - 0.5  ! in sec
      integer, parameter ::
     .   QCList ( QCListSz ) = (/ 0, 1, 4, 5 /)
      logical  :: valid_QC ( QCMin : QCMax ), accept, thin_layer,
     .                                        valid_etime
      integer, parameter :: DTimeMax = 3600
      character (len = len ( Profile      % StnID )) ::  StnID
      character (len = len ( CARDS % Meta % StnID )),
     .   dimension (:), pointer                      :: CStnID
      logical, dimension (:), pointer :: CMask
      integer, dimension (:), pointer :: CLTime,  CETime_QC, CHeight_QC
      real,    dimension (:), pointer :: CHeight, CETime, Height, ETime
      real    :: ZZ, ZBottom, ZTop, Z_sfc
      real    :: CZ, CZTop, CZBottom, CZBelow, CZ_sfc
      real    :: CET,  CETBottom, CETTop, RRTop
      real    :: ET_sfc (1), ETThr
      integer :: LRange (3), NCReports, iCBeg, iCEnd, CLen, LTime,
     .           TRange (2), NCLevels, iLev, CE_QC, CZ_QC, NLevels,
     .           NLevels_Below
      integer :: DTime1, DTime2, Time0
      integer :: SynDate, SynHr, CSynJHr, SynJHr
      integer :: iBeg0, Len0, iEnd0, iRep, iList, iTop, iBottom, iExtrap
      integer :: iBegR, LenR, iEndR

!     Match the station id
!     --------------------
      StnID     =  Profile % StnID
      NCReports =  CARDS % Meta % NReports
      CStnID    => CARDS % Meta % StnID ( : NCReports )
      LRange    =  Range2 ( CStnID, StnID )
      iCBeg     =  LRange (1)
       CLen     =  LRange (2)
      iCEnd     =  LRange (3)

!     Return if no ncdc station exists
!     --------------------------------
      if ( CLen .le. 0 ) return

!     If the station id is in the ncdc list
!     -------------------------------------
      SynDate =  CARDS % Meta % SynDate
      SynHr   =  CARDS % Meta % SynHr
      CSynJHr =  Julian ( SynDate, SynHr )
      SynDate =  Profile % Date
      SynHr   =  Profile % Time
       SynJHr =  Julian ( SynDate, SynHr, HHMMSS = .true. )
      CLTime  => CARDS % Meta % LTime ( : NCReports )
       LTime  =  Profile      % LTime - 3600 * ( CSynJHr - SynJHr )

!     ... then determine the report with ...
!     --------------------------------------
      TRange = Range ( CLTime, LTime, iCBeg, CLen )
      iBeg0  = min ( iCEnd, TRange ( 1 ))
      Len0   = TRange ( 2 )
      iEnd0  = max ( iCBeg, iBeg0 + Len0 - 1 )
      Time0  = LTime
      DTime1 = abs ( Time0 - CLTime ( iBeg0 ))
      DTime2 = abs ( Time0 - CLTime ( iEnd0 ))

!     ... closest time tag that is within tolerance
!     --------------------------------------------- 
      iBegR  =  0
      LenR   =  0
      if    ( DTime1 .le. DTime2   ) then
         if ( DTime1 .le. DTimeMax ) then
             iRep = iBeg0
            iBegR = CARDS % Meta % Loc     ( iRep )
             LenR = CARDS % Meta % NLevels ( iRep )
         else
            iBegR = 0
             LenR = 0
         end if
      else
         if ( DTime2 .le. DTimeMax ) then
             iRep = iEnd0
            iBegR = CARDS % Meta % Loc     ( iRep )
             LenR = CARDS % Meta % NLevels ( iRep )
         else
            iBegR = 0
             LenR = 0
         end if
      end if
      iEndR = iBegR + LenR - 1

!     Return if no report exist near the desired launch time
!     ------------------------------------------------------
      if ( iBegR .le. 0 ) return

!     Create logical table for valid/invalid NCDC QC marks
!     ----------------------------------------------------
      valid_QC = .false.
      do iList = 1, QCListSz
         valid_QC ( QCList ( iList )) = .true.
      end do

!     And mask out the level below the surface
!     or with a bad elapse time or height ob
!     ----------------------------------------
      NCLevels   =  LenR
      CZ_sfc     =  CARDS % Meta % Elev   ( iRep )
      CMask      => CARDS % QC   % Mask   ( iBegR:  iEndR )
      CETime_QC  => CARDS % QC   % ETime  ( iBegR : iEndR )
      CHeight_QC => CARDS % QC   % Height ( iBegR : iEndR )
      CHeight    => CARDS % Obs  % Height ( iBegR : iEndR )
      do iLev = 1, NCLevels
         CE_QC = CETime_QC  ( iLev )
         CZ_QC = CHeight_QC ( iLev )
         CMask ( iLev )    = .false.
         if ( CE_QC .ge. QCMin .and.
     .        CE_QC .le. QCMax )
     .      CMask ( iLev ) =  valid_QC ( CE_QC )
         if ( CZ_QC .ge. QCMin .and.
     .        CZ_QC .le. QCMax ) then
            CMask ( iLev ) =  valid_QC ( CZ_QC ) .and. CMask ( iLev )
         else
            CMask ( iLev ) = .false.
         end if
         if ( CMask ( iLev ))
     .      CMask ( iLev ) = CHeight ( iLev ) .ge. CZ_sfc - 0.5
     .        ! Heights are reported in whole meters
      end do

!     Find the top and bottom levels
!     ------------------------------
      CETime  => CARDS % Obs % Etime  ( iBegR : iEndR )
      iBottom =  NCLevels + 1
      do iLev = 1, NCLevels
         if ( CMask ( iLev )) then
            iBottom = iLev
            exit
         end if
      end do
      iTop    =  0
      do iLev = NCLevels, 1, -1
         if ( CMask ( iLev )) then
            iTop = iLev
            exit
         end if
      end do

!     Check if at least two levels contain valid elapsed time and that
!     the vertical distance between two successive valid levels does not
!     exceed a certain threshold..
!     ----------------------------------------------------------------
      accept  = iTop .gt. iBottom
      CZBelow = CHeight ( iBottom )
      do iLev = iBottom + 1, iTop
         if ( CMask ( iLev )) then
            CZ = CHeight ( iLev )
            if ( CZ - CZBelow .gt. ZDiff_Max ) then
               accept = .false.
               exit
            end if
            CZBelow = CZ
         end if
      end do

!     Set parameters concerning the target (i.e. non-CARDS) profile
!     -------------------------------------------------------------
      NLevels =  Profile % by_Levels % NLev
      Z_sfc   =  Profile % Elev
      Height  => Profile % by_Levels % Z     ( : NLevels )
      ETime   => Profile % by_Levels % ETime ( : NLevels )

!     Ensure sufficient vertical distance between the top and bottom
!     --------------------------------------------------------------
      if ( accept ) then
          ZTop     = Height  ( NLevels )
          ZBottom  = Z_sfc
         CETTop    = CETime  ( iTop )
         CETBottom = CETime  ( iBottom )
         CZTop     = CHeight ( iTop    )
         CZBottom  = CHeight ( iBottom )
         accept    = (( ZTop     - CZTop      .le. ZExt_Max    .and.
     .                 CETTop    - CETBottom  .gt. ETDiff_Min ) .or.
     .                ( CZTop   .ge. ZTop ))
     .          .and. ( CZBottom - ZBottom    .le.  ZDiff_Max )
      end if

!     Return if sounding info is not acceptable
!     -----------------------------------------
      if ( .not. accept ) return

!     Interpolate the elapsed time onto levels of the profile
!     -------------------------------------------------------
      call Z2Y ( CHeight, CMask, CETime, Height, ETime )

!     If necessary, interpolate below the lowest valid CARDS level
!     assuming that the elapsed time is zero at the surface.
!     ------------------------------------------------------------
      NLevels_Below = 0
      do iLev = 1, NLevels
         ZZ = Height ( iLev )
         if ( ZZ .ge. CZBottom ) exit
         NLevels_Below = NLevels_Below + 1
      end do
      if ( NLevels_Below .gt. 0 )
     .   call Z2Y ( (/ CZ_sfc,  CZBottom /),
     .              (/ .true.,  .true.   /),
     .              (/  0.0 ,  CETBottom /),
     .               Height ( : NLevels_Below ),
     .               ETime  ( : NLevels_Below ))

!     If extrapolation is to be performed then two levels are
!     necessry.  Find the second level just below the top
!     to ensure that the extrapolation is stable and robust.
!     -----------------------------------------------------
      ETThr     = CETTop - ETDiff_Min
      iExtrap   = iBottom
      do iLev   = iTop - 1, iBottom + 1, -1 
         if ( CMask ( iLev )) then
            CET = CETime ( iLev )
            if ( CET .le. ETThr ) then
               iExtrap = iLev
               exit
            end if
         end if
      end do
      RRTop = ( CZTop - CHeight ( iExtrap )) / ( CETTop - CET )

!     Extrapolate
!     -----------
      do iLev = 1, NLevels 
         ZZ = Height ( iLev )
         if ( ZZ .gt. CZTop    )
     .      ETime ( iLev ) = CETTop    + ( ZZ - CZTop    ) / RRTop
      end do

!     Set elapsed time to 0 for all underground observations and
!     adjust all other levels to ensure continuity at the surface
!     -----------------------------------------------------------
      call Z2Y ( Height, ETime, (/ Z_sfc /), ET_sfc )

      do iLev = 1, NLevels
         ZZ = Height ( iLev )
         if ( ZZ .ge. Z_sfc ) then
            ETime ( iLev ) = ETime ( iLev ) - ET_sfc (1)
         else
            ETime ( iLev ) = 0.0
         end if
      end do

!     Indicate that elapsed time is from CARDS data base
!     --------------------------------------------------
      Profile % ETHist = ETHist_CARDS

      return
      end subroutine CDS_ETimeS
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Select_Data () ---  Select data by Julian hour
! 
! !DESCRIPTION:
!     This routine reads data from a list of files, selects data whose
!     data/time within a given range,  and appends (or, if desired, 
!     overwrites) the data to a file.  The range is set by Julian hours
!     which is defined as JHr = 24 * ( JDay - 1 ) + Hour where JHr is
!     the Julian hour, JDay is the Julian day from the routine
!     CDS_Julian and Hour is the hour (GMT) of the day.
!
! !INTERFACE:
      subroutine Select_Data ( InFiles, Outfile,
     .                         BegDate, EndDate, stat, ! Required and
     .                         replace, header )       ! optional arguments
      use m_SunAlt,   only : Julian, Check_DateTime                             
      use m_AtoX,     only : AtoI
      use m_AdvError, only : ItoA, WPErr, PErr, ErrStat
      use m_SysIO,    only : LUAvail
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len = *), intent (in)  ::
     .   InFiles (:),      ! Names of inputs files with the CARDS data.
     .   OutFile           ! Output file name
      integer,             intent (in)  ::
     .   BegDate (:),      ! The earliest and
     .   EndDate (:)       ! ... latest date and hour allowed for a
                           !   selected report.  XDate(1) = YYYYMMDD
                           !   and XDate(2) = HH
      logical,             intent (in), optional ::
     .   replace,          ! = .true. to delete the output file (if it
     .                     !   pre-exists) and replace it with a new file. 
     .   header            ! = .true. to replace the file and insert a header.
!                          !  Default for both options is set to false.
! !OUTPUT PARAMETERS:
      integer,             intent (out) ::
     .   stat              ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08May2002  C. Redder  Origional code.
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     06Jan2003  C. Redder  Changed use m_Julian to use m_SunAlt
!     15Jan2003  C. Redder  Changed use module from m_ioutil to m_SysIO
!     17Dec2006  C. Redder  Modified error messages and innsured that
!                           the actual record length matches the value
!                           predicted from header information.  Added
!                           further checks to synoptic dates and hours
!                           (from the input files).  Replaced the
!                           arguments, JMin and JMax, with BegDate and
!                           EndDate.
! EOP
!....................................................................

      character (len=*), parameter ::
     .   MyName = MyModule // '::Select_Data'
      integer, parameter :: MaxLLine = LHeader
      logical :: eor, eof, replace_file, add_header
      integer :: lu_in, lu_out, iret, iret1, iret2, LLine
      integer :: NFiles, iFile, LRec, LRec1, iRec, NRecLev
      integer :: Date, Hour, JHr, JMin, JMax
      character (len=12 ) :: CNum, Status
      character (len=255) :: InFile
      character (len=MaxLRec + 255) :: Record
      character (len=MaxLLine     ) :: Line
      character (len=80)            :: Message
      character (len=300+LHeader)   :: Tail
      character (len=LHeader)       :: Headr

!     Implement options
!     -----------------
      replace_file = .false.
      if ( present ( replace )) replace_file = replace
      add_header   = .false.
      if ( present ( header  )) add_header  = header

!     Convert the begin data/time 
!     ---------------------------
      if ( Check_DateTime ( BegDate(1),
     .                      BegDate(2), Message ) .ne. 0 ) then
         call WPErr ( MyName, trim ( Message )
     .     // '\n\C   Begin date tag = ' // trim ( ItoA ( BegDate(1)))
     .     //   '\n     ... hour     = ' // trim ( ItoA ( BegDate(2))))
         stat = Bad_Argument
         return
      end if
      JMin = Julian ( BegDate(1), BegDate(2))

!     ... and end date/time to Julian hours
!     -------------------------------------
      if ( Check_DateTime ( EndDate(1),
     .                      EndDate(2), Message ) .ne. 0 ) then
         call WPErr ( MyName, trim ( Message ) 
     .     // '\n\C   End date tag = ' // trim ( ItoA ( EndDate(1)))
     .     //   '\n     . hour     = ' // trim ( ItoA ( EndDate(2))))
         stat = Bad_Argument
         return
      end if
      JMax = Julian ( EndDate(1), EndDate(2))

!     Ensure that the datas are consistent
!     ------------------------------------
      if ( JMin .gt. JMax ) then
         write (CNum,'(2(i2.2))')  BegDate(2), EndDate(2)
         call WPErr ( MyName, 'The begin date (='         
     .                //       trim ( ItoA ( BegDate(1))) // CNum(1:2)
     .                //      ') is after the end date (='
     .                //       trim ( ItoA ( EndDate(1))) // CNum(3:4)
     .                // '). ')
         stat = Bad_Argument
         return
      end if

!     Add header (if desired)
!     -----------------------
      if ( add_header ) then
         call Put_Header ( Outfile, stat, BegDate, EndDate,
     .                     replace = .true. )
         if ( stat .ne. 0 ) then
            call PErr ( MyName, ErrStat ( 'StrTemplate', stat ) // '\W') 
            return
         end if
         replace_file = .false. ! File has ready been replaced
      end if

!     Determine another available logical unit for the output file
!     ------------------------------------------------------------
      lu_out = LUAvail()
      if ( lu_out .lt. 0 ) then
         call PErr ( MyName, 'No logical units available for '
     .                    // 'the output file. \W ' )
         stat = No_LUAvail
         return
      end if

!     ... open the next output file
!     -----------------------------
      Status = 'unknown'
      if ( replace_file ) Status = 'replace'
      open ( unit     =  lu_out,
     .       file     =  OutFile,
     .       form     = 'formatted',
     .       access   = 'sequential',
     .       status   =  Status,
     .       position = 'append',
     .       recl     =  MaxLRec,
     .       iostat   =  stat )
      if ( stat .ne. 0 ) then
         call PErr ( MyName,
     .              'Open error (iostat = '
     .         //    trim ( ItoA ( stat ) ) // ') \C'
     .         // '\n   Output file = ' // trim ( OutFile ))
         stat = Open_Error
         return
      end if

      lu_in   = -1

!     For each input file ...
!     -----------------------
      NFiles = size ( InFiles )
      FileLoop : do iFile = 1, NFiles
         InFile = InFiles ( iFile )

!        Determine an available logical unit
!        -----------------------------------
         if ( lu_in .eq. -1 ) then
            lu_in = LUAvail()
            if ( lu_in .lt. 0 ) then
               call PErr ( MyName, 'No logical units available for '
     .                          // 'the input files. \W ' )
               close ( lu_out )
               stat = No_LUAvail
               return
            end if
         end if

!        Open input data file
!        ---------------------
         open ( unit   =  lu_in,
     .          file   =  InFile,
     .          form   = 'formatted',
     .          status = 'old',
     .          access = 'sequential',
     .          recl   =  MaxLRec,
     .          iostat =  stat )
         if ( stat .ne. 0 ) then
            call PErr ( MyName, 'Open error (iostat = '
     .                     //       trim ( ItoA ( stat ) ) // ') \C'
     .                     // '\n   Input file = ' // trim ( InFile ))
            close ( lu_out )
            stat = Open_Error
            return
         end if

!        For each record in the input file ...
!        -------------------------------------
         RecordLoop : do iRec = 1, MaxNRec

!           ... read the record
!           -------------------
            call ReadLn ( lu_in, LRec, Record, eor, eof, stat )
            if ( stat .ne. 0 ) then
               call PErr ( MyName, 'Read error (iostat = '
     .                        //    trim ( ItoA ( stat ) ) // ') \C'
     .                        // '\n   Input file = ' // trim ( InFile))
               close ( lu_in  )
               close ( lu_out )
               stat = Read_Error
               return
            end if
            if ( eof ) exit RecordLoop

!           Ignore any comment line (i.e. line beginning with "%")
!           ------------------------------------------------------
            if ( Record ( 1:1 ) .eq. '%' ) 
     .         cycle RecordLoop

!           Check header line 
!           ------------------
            Line  = Record ( : LHeader )
            LLine = len_trim ( Line )
            if ( LLine .lt. LHeader ) then
               call PErr ( MyName, 'Incomplete header. \C'
     .                        // '\n   Input file = ' // trim ( InFile )
     .                        // '\n   Header     = ' // trim ( Line  ))
               close ( lu_in  )
               close ( lu_out )
               stat = Bad_Record
               return
            end if
            Headr  = Line ( : LHeader )
            Tail   =  '\n\C   Input file = '// trim ( InFile )
     .             //   '\n   Header     = '// trim ( Headr )

!           Number of levels in the record
!           ------------------------------
            CNum    = Line ( 106:108 )
            NRecLev = AtoI ( CNum ( 1 : 3 ), iret )
            if ( iret .ne.   0 ) then
               call WPErr ( MyName, 'Cannot get the number of levels '
     .                       //     'from the header. '
     .                       //      trim ( Tail ))
               close ( lu_in  )
               close ( lu_out )
               stat = Bad_Record
               return
            end if
            if ( NRecLev .lt. 1 .or.
     .           NRecLev .gt. MaxNRecLev ) then
               call WPErr ( MyName, 'The number of records (='
     .                       //      trim ( ItoA ( NRecLev )) // ') '
     .                       //     'is outside the range of [1:'
     .                       //      trim ( ItoA ( MaxNRecLev )) // '].'
     .                       //      trim ( Tail ))
               if ( lu_out .ne. -1 ) close ( lu_out ) 
               close ( lu_in )
               stat = Bad_Record
               return
            end if

!           Check to determine if the entire record was read
!           ------------------------------------------------
            LRec1 = LHeader + NRecLev * LRecLev
            if ( LRec1 .lt. LRec ) 
     .         LRec = max ( LRec1, len_trim ( Record ( : LRec ))) 
            if ( LRec1 .ne. LRec ) then
               call WPErr ( MyName, 'The record length (='
     .                       //      trim ( ItoA ( LRec )) // ') '
     .                       //     'does not match the value '
     .                       //     'predicted from header '
     .                       //     'information (='
     .                       //      trim ( ItoA ( LRec1 )) // ').'
     .                       //      trim ( Tail ))
               close ( lu_in  )
               close ( lu_out )
               stat = Bad_Record
               return
            end if
            LRec = LRec1

!           Date and time tags
!           ------------------
            CNum   = Line ( 39:48 )
            if ( CNum ( 9:10 ) .eq. '99' ) CNum ( 9:10 ) = '12'
            Date = AtoI ( CNum ( 1:8  ), iret1 )
            Hour = AtoI ( CNum ( 9:10 ), iret2 )
            if ( iret1 .ne. 0 . or.
     .           iret2 .ne. 0 ) then
               call WPErr ( MyName, 'Cannot get the date/time tags '
     .                       //     'from the header. '
     .                       // '\C\n   Date/hour = ' // CNum ( 1:10 )
     .                       //         trim ( Tail ))
               close ( lu_in  )
               close ( lu_out )
               stat = Bad_Record
               return
            end if
            if ( Check_DateTime ( Date, Hour, Message ) .ne. 0 ) then
               call WPErr ( MyName, trim ( Message )
     .              // '\C\n   Date/hour = ' // CNum ( 1:10 )
     .              //         trim ( Tail ))
               close ( lu_in )
               close ( lu_out )
               stat = Bad_Record
               return
            end if
            JHr = Julian ( Date, Hour )

!           Write the entire record 
!           -----------------------
            if ( JHr .ge. JMin .and.
     .           JHr .le. JMax ) then
               write ( unit   = lu_out,
     .                 fmt    = '(a)',
     .                 iostat = stat ) Record ( : LRec )
               if ( stat .ne. 0 ) then
                  call PErr ( MyName,
     .                       'Write error (iostat = '
     .                 //     trim ( ItoA ( stat )) // ') \C'
     .                 // '\n   Output file = ' // trim ( OutFile )
     .                 //     trim ( Tail ))
                  close ( lu_in  )
                  close ( lu_out )
                  stat = Write_Error
                  return
               end if
            end if
         end do RecordLoop

!        Close input file
!        ----------------
         close (lu_in)

      end do FileLoop
      close (lu_out)

      return
      end subroutine Select_Data
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Prep_Data () ---  Sort CARDS data by WMO station ID
! 
! !DESCRIPTION:
!     This routine reads CARDS data from a list of files and then
!     prepares and writes the data to one or more files whose names
!     are based on a given template.  This template can contain the
!     descriptor "%s" which refers to the WMO station ID and thus a
!     station sort can be performed.  No other descriptors are allowed.
!     This routine is ideal for a CARDS database sorted by stations but
!     the data for one station may be split among several files.  This
!     routine can also select reports based on the Julian hours which
!     is defined as JHr = 24 * ( JDay - 1 ) + Hour where JHr is the
!     Julian hour, JDay is the Julian day from the routine CDS_Julian
!     and Hour is the hour (GMT) of the day.
!
! !INTERFACE:
      subroutine Prep_Data ( InFiles, TPlate, stat,     ! Required &
     .                       BegDate, EndDate )         ! optional
      use m_SunAlt,       only : Julian, Check_DateTime ! arguments
      use m_AtoX,         only : AtoI
      use m_AdvError,     only : ItoA, WPErr, PErr, ErrStat
      use m_SysIO,        only : LUAvail
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len = *), intent (in)  ::
     .   InFiles (:),      ! Names of inputs files with the CARDS data.
     .   TPlate            ! Template of output file name
!     Note: Output files are opened with "status" set to "unknown" 
!                                    and "position"   to "append"
      integer,             intent (in), optional ::
     .   BegDate (:),      ! The earliest and
     .   EndDate (:)       ! ... latest date and hour allowed for a
                           !   selected report.  XDate(1) = YYYYMMDD
                           !   and XDate(2) = HH
! !OUTPUT PARAMETERS:
      integer,             intent (out) ::
     .   stat              ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     07May2002  C. Redder  Origional code.
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     13Dec2002  C. Redder  Set string length for WMO station ID via constant
!                           LStnID
!     06Jan2003  C. Redder  Changed use m_Julian to use m_SunAlt
!     15Jan2003  C. Redder  Changed use module from m_ioutil to m_SysIO
!     05Dec2006  C. Redder  Bug fixed by initializing StnIDLast.  Increase
!                           NBRecMax from 7 to 50
!     13Dec2006  C. Redder  Buf fixed by comparing entire header of
!                           successive records and sorting according
!                           to NAddRec.  Removed the optional argument
!                           psort.  Sorting records within profiles is
!                           now performed automatically.  Added further
!                           checks to the synoptic date and time (from
!                           the input files).  Replaced the optional
!                           arguments, JMin and JMax, with BegDate and
!                           EndDate.
!     08Jan2007  C. Redder  Enhanced routine to properly handle records
!                           among different reports but corresspond to
!                           the same profile.
!
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter :: MyName = MyModule // '::Prep_Data'
      integer, parameter :: MaxLLine = LHeader

      type ( CARDS_records ) :: Buffer
      type ( CARDS_repinfo ) :: RepInfo
      logical :: eor, eof, new_profile
      integer :: lu_in, lu_out, iret, iret1, iret2, iChar, LLine
      integer :: NFiles, iFile, NRecLev
      integer :: LRec, LRec1, iRec, LBRec, iBRec, NBRec, NAddRec
      integer :: iRep, NReports, iRepRec, NRepRec
      integer :: Date, Hour, JHr, JHrMax, JHrMin, JHrLast
      character (len=LStnID ) :: StnID, StnIDLast, StnIDF, StnIDLastF
      character (len=10 ) :: SynTag, SynTagLast
      character (len=12 ) :: CNum
      character (len=255) :: InFile, OutFile, BInfile
      character (len=80)  :: Message
      character (len=10 + LStnID) :: ProfTag, ProfTagLast
      character (len=MaxLLine) :: Line
      character (len=LHeader)  :: Header, BHeader

!     Impliment option to select by begin date/hour
!     ---------------------------------------------
      JHrMin = -huge ( 1 )
      if ( present ( BegDate )) then
         if ( Check_DateTime ( BegDate(1),
     .                         BegDate(2), Message ) .ne. 0 ) then
            call WPErr ( MyName, trim ( Message )
     .           // '\n\C   Begin date tag = '
     .           //         trim ( ItoA ( BegDate(1)))
     .           //   '\n     ... time tag = '
     .           //         trim ( ItoA ( BegDate(2))))
            stat = Bad_Argument
            return
         end if
         JHrMin = Julian ( BegDate(1), BegDate(2))
      end if
c      if ( present ( JMin )) JHrMin = JMin

!     ... end data/hour
!     -----------------
      JHrMax =  huge ( 1 )
      if ( present ( EndDate )) then
         if ( Check_DateTime ( EndDate(1),
     .                         EndDate(2), Message ) .ne. 0 ) then
            call WPErr ( MyName, trim ( Message )
     .           // '\n\C   End date tag = '
     .           //         trim ( ItoA ( EndDate(1)))
     .           //   '\n   ... time tag = '
     .           //         trim ( ItoA ( EndDate(2))))
            stat = Bad_Argument
            return
         end if
         JHrMax = Julian ( EndDate(1), EndDate(2))
      end if
c      if ( present ( JMax )) JHrMax = JMax 

!     Ensure that the data are consistent
!     -----------------------------------
      if ( present ( BegDate ) .and.
     .     present ( EndDate ) .and.
     .     JHrMin .gt. JHrMax ) then
         write (CNum,'(2(i2.2))')  BegDate(2), EndDate(2)
         call WPErr ( MyName, 'The begin date (='         
     .                //       trim ( ItoA ( BegDate(1))) // CNum(1:2)
     .                //      ') is after the end date (='
     .                //       trim ( ItoA ( EndDate(1))) // CNum(3:4)
     .                // '). ')
         stat = Bad_Argument
         return
      end if

!     Initialize
!     ----------
c      Buffer % Indx = -1
      Buffer % NBRec =  0
      StnID          = ' '
      StnIDLast      = ' '
      StnIDLastF     = ' '
      SynTagLast     = ' '
      ProfTagLast    = ' '
      JHrLast        = -1
      lu_in          = -1
      lu_out         = -1
      NBRec          =  0

!     For each input file ...
!     -----------------------
      NFiles = size ( InFiles )
      FileLoop : do iFile = 1, NFiles
         InFile = InFiles ( iFile )

!        Determine an available logical unit
!        -----------------------------------
         if ( lu_in .eq. -1 ) then
            lu_in = LUAvail()
            if ( lu_in .lt. 0 ) then
               call PErr ( MyName, 'No logical units available for '
     .                          // 'the input files. \W ' )
               if ( lu_out .ne. -1 ) close ( lu_out ) 
               stat = No_LUAvail
               return
            end if
         end if

!        Open input data file
!        --------------------
         open ( unit   =  lu_in,
     .          file   =  InFile,
     .          form   = 'formatted',
     .          status = 'old',
     .          access = 'sequential',
     .          recl   =  MaxLRec,
     .          iostat =  stat )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, 'Open error (iostat = '
     .                    //      trim ( ItoA ( stat )) // ')'
     .                    // '\n\C   Input file = ' // trim ( InFile ))
            if ( lu_out .ne. -1 ) close ( lu_out ) 
            stat = Open_Error
            return
         end if

!        For each record in the input file ...
!        -------------------------------------
         RecordLoop : do iRec = 1, MaxNRec

!           ... ensure that sufficient buffer space exists
!           ----------------------------------------------
            if ( NBRec + 1 .gt. NBRecMax ) then
               call WPErr ( MyName, 'Maximum number of records (= '
     .                       //      trim ( ItoA ( NBRecMax ) ) // ') '
     .                       //     'in the buffer has been exceeded '
     .                       // '\n\C   Input file = '//trim ( InFile))
               if ( lu_out .ne. -1 ) close ( lu_out ) 
               close ( lu_in )
               stat = Buffer_Error
               return
            end if

!           Read the record to buffer
!           -------------------------
            call ReadLn ( lu_in, LRec, Buffer % Record ( NBRec + 1 ),
     .                    eor, eof, stat )
            if ( stat .ne. 0 ) then
               call WPErr ( MyName, 'Read error (iostat = '
     .                       //      trim ( ItoA ( stat ) ) // ')'
     .                       // '\n\C   Input file = '//trim ( InFile))
               if ( lu_out .ne. -1 ) close ( lu_out ) 
               close ( lu_in )
               stat = Read_Error
               return
            end if
            if ( eof ) exit RecordLoop

!           Ignore any comment line (i.e. record beginning with "%")
!           --------------------------------------------------------
            if ( Buffer % Record ( NBRec + 1 ) ( 1 : 1 ) .eq. '%' ) 
     .         cycle RecordLoop

!           Check header line
!           -----------------
            Line  = Buffer % Record ( NBRec + 1 ) ( : LHeader )
            LLine = len_trim ( Line )
            if ( LLine .lt. LHeader ) cycle RecordLoop
            Header = Line ( : LHeader )

!           WMO station ID (fill in blanks with zeroes for file names)
!           ----------------------------------------------------------
            StnID = Line ( 2:6 )
            do iChar = 1, LStnID
               if ( StnID ( iChar:iChar ) .eq. ' ' )
     .              StnID ( iChar:iChar )  =   '0'
            end do

!           Number of levels in the record
!           ------------------------------
            CNum    = Header ( 106:108 )
            NRecLev = AtoI   ( CNum ( 1 : 3 ), iret )
            if ( iret .ne.   0 )             cycle RecordLoop
            if ( NRecLev .lt. 1 .or.
     .           NRecLev .gt. MaxNRecLev )   cycle RecordLoop

!           Check to determine if the entire record was read
!           ------------------------------------------------
            LRec1 = LHeader + NRecLev * LRecLev
            if ( LRec1 .lt. LRec )
     .         LRec = max ( LRec1, 
     .                len_trim ( Buffer %
     .                           Record ( NBRec + 1 ) ( : LRec ))) 
            if ( LRec1 .ne. LRec )           cycle RecordLoop
            LRec = LRec1

!           Date and time tags
!           ------------------
            CNum = Header ( 39:48 )
            if ( CNum ( 9:10 ) .eq. '99' ) CNum ( 9:10 ) = '12'
            Date = AtoI ( CNum ( 1: 8 ), iret1 )
            Hour = AtoI ( CNum ( 9:10 ), iret2 )
            if ( iret1 .ne. 0 .or.
     .           iret2 .ne. 0 )              cycle RecordLoop
            if ( Check_DateTime ( Date, Hour, Message ) .ne. 0 ) 
     .                                       cycle RecordLoop
            SynTag = CNum ( 1:10 )
            JHr    = Julian ( Date, Hour )

!           Number of additional records in the profile
!           -------------------------------------------
            CNum    = Header ( 103:105 )
            NAddRec = AtoI ( CNum ( 1:3 ), iret )
            if ( iret .ne.   0 )             cycle RecordLoop

!           Ensure that there is enough space in the buffer
!           to accomodate all records in the profile
!           -----------------------------------------------
            if ( NAddRec .gt. NBRecMax - 1 ) cycle RecordLoop

!           Ensure a reasonable value for NAddRec
!           -------------------------------------
            if ( NAddRec .lt. 0 )            cycle RecordLoop

!           Ensure that all records but the last
!           have the maximum number of levels
!           ------------------------------------
            if ( NAddRec .ne. 0 .and.
     .           NRecLev .ne. MaxNRecLev )   cycle RecordLoop

!           Add record information to local buffer data structure
!           -----------------------------------------------------
            NBRec = NBRec + 1
            Buffer % NBRec             = NBRec
            Buffer % LRec    ( NBRec ) = LRec
            Buffer % NRecLev ( NBRec ) = NRecLev
            Buffer % NAddRec ( NBRec ) = NAddRec
            Buffer % InFiles ( NBRec ) = InFile
            Buffer % ProfTag ( NBRec ) = StnID // SynTag

!           Determine if the currect record contains data for a new profile
!           ---------------------------------------------------------------
            ProfTag = StnID // SynTag
            new_profile = ProfTag .ne. ProfTagLast
 
!           If new profile ...
!           ------------------
            if ( new_profile ) then

!              ... and if the date/time tag for the last
!                  profile is within the desired window
!              -----------------------------------------
               if ( JHrLast .ge. JHrMin .and.
     .              JHrLast .le. JHrMax ) then

!                 ... determine the reports from profile records
!                 ----------------------------------------------
                  call SetReports( ProfTagLast, Buffer, RepInfo, stat )
                  if ( stat .ne. 0 ) then
                     call PErr ( MyName, ErrStat ( 'SetReports', stat ))
                     if ( lu_out .ne. -1 ) close ( lu_out ) 
                     close ( lu_in )
                     stat = Read_Error
                     return
                  end if
                  NReports = RepInfo % NReports

!                 ... If stations have changed ...
!                 --------------------------------
                  StnIDF = StnIDLast
                  if ( StnIDF   .ne. StnIDLastF .and.
     .                 NReports .gt. 0 ) then

!                    ... determine another available logical unit
!                        for the output files (if necessary)
!                    --------------------------------------------
                     if ( lu_out .eq. -1 ) then
                        lu_out = LUAvail()
                        if ( lu_out .lt. 0 ) then
                           call PErr ( MyName, 'No logical units '
     .                                // 'available for the '
     .                                // 'output files.  \W ' )
                           stat = No_LUAvail
                           close ( lu_in )
                           return
                        end if
                     else
!                       Logical unit already assigned is assumed
!                       to be connected to an opened output file. 
!                       ----------------------------------------
                        close (lu_out)
                     end if

!                    ... generate the name of the next output file
!                    --------------------------------------------
                     call StrTPlate ( TPlate, StnIDF, OutFile, stat )
                     if ( stat .ne. No_Error ) then
                        call WPErr ( MyName,
     .                               ErrStat ( 'StrTemplate', stat ) 
     .                          // '\n\C   WMO station ID = '
     .                          //      trim ( StnIDF )
     .                          //   '\n   Template   = '
     .                          //      trim ( TPlate )
     .                          //   '\n   Input file = '
     .                          //      trim ( InFile ))
                        close ( lu_in )
                        return
                     end if

!                    ... open the next output file
!                    -----------------------------
                     open ( unit     =  lu_out,
     .                      file     =  OutFile,
     .                      form     = 'formatted',
     .                      access   = 'sequential',
     .                      position = 'append',
     .                      recl     =  MaxLRec,
     .                      iostat   =  stat )
                     if ( stat .ne. 0 ) then
                        call WPErr ( MyName,
     .                              'Open error (iostat = '
     .                    //         trim ( ItoA ( stat ) ) // ')'
     .                    // '\n\C   Input file = '
     .                    //         trim ( InFile  )
     .                    //   '\n   Output file = '
     .                    //         trim ( OutFile ))
                        close ( lu_in )
                        stat = Open_Error
                        return
                     end if

!                    ... store for next file to be opened
!                    ------------------------------------
                     StnIDLastF = StnIDF
                  end if

!                 For each report ...
!                 -------------------
                  RepSortLoop : do iRep = 1, NReports
                     BInFile    = RepInfo % InFiles ( iRep )
                     BHeader    = RepInfo % Header  ( iRep )
                     NRepRec    = RepInfo % NRepRec ( iRep )

!                    ... sort
!                    --------
                     RecSortLoop : do iRepRec = 1, NRepRec
                        iBRec   = RepInfo % RecList ( iRep, iRepRec )

!                       ... replace NAddRec in record that is
!                           to be written to the output file
!                       -------------------------------------
                        Buffer % Record ( iBRec ) ( 103 : 105 )
     .                          = RepInfo % CAdd    ( iRep, iRepRec )

!                       ... and write each record in the proper
!                           order (except the last since it the
!                           first record for the next profile)
!                       ---------------------------------------
                        LBRec   = Buffer % LRec ( iBRec )
                        write ( unit   = lu_out,
     .                          fmt    = '(a)',
     .                          iostat = stat )
     .                          Buffer % Record ( iBRec ) ( : LBRec )
                        if ( stat .ne. 0 ) then
                           call WPErr ( MyName,
     .                                 'Write error (iostat = '
     .                          //      trim ( ItoA ( stat )) // ')'
     .                          // '\n\C   Input file = '
     .                          //      trim ( BInFile )
     .                          //   '\n   Header     = '
     .                          //      trim ( BHeader ))
                           if ( lu_out .ne. -1 ) close ( lu_out ) 
                           close ( lu_in )
                           stat = Write_Error
                           return
                        end if
                     end do RecSortLoop
                  end do RepSortLoop
               end if

!              ... flush the buffer except the last record
!                  (since it is the first for the next profile)
!              ------------------------------------------------ 
               Buffer % Record  ( 1 ) = Buffer % Record  ( NBRec )
               Buffer % LRec    ( 1 ) = Buffer % LRec    ( NBRec ) 
               Buffer % NRecLev ( 1 ) = Buffer % NRecLev ( NBRec ) 
               Buffer % NAddRec ( 1 ) = Buffer % NAddRec ( NBRec ) 
               Buffer % InFiles ( 1 ) = Buffer % InFiles ( NBRec )
               Buffer % ProfTag ( 1 ) = Buffer % ProfTag ( NBRec )
               NBRec  = 1
            end if

!           Store for next record to be read
!           --------------------------------
            StnIDLast   = StnID
            SynTagLast  = SynTag
            ProfTagLast = StnIDLast // SynTagLast
            JHrLast     = JHr

         end do RecordLoop

!        Close input file
!        ----------------
         close (lu_in)

      end do FileLoop

!     Write the remaining records to file
!     if the date/time tag for the last
!     profile is within the desired window
!     ------------------------------------
      if ( JHr .ge. JHrMin .and.
     .     JHr .le. JHrMax ) then

!        ... determine the reports from profile records
!        ----------------------------------------------
         call SetReports( ProfTag, Buffer, RepInfo, stat )
         if ( stat .ne. 0 ) then
            call PErr  ( MyName, ErrStat ( 'SetReports', stat ))
            if ( lu_out .ne. -1 ) close ( lu_out ) 
            close ( lu_in )
            stat = Read_Error
            return
         end if
         NReports = RepInfo % NReports

!        ... If stations have changed ...
!        --------------------------------
         if ( StnIDF   .ne. StnIDLastF .and.
     .        NReports .gt. 0 ) then

!           ... determine another available logical unit
!               for the output files (if necessary)
!           --------------------------------------------
            if ( lu_out .eq. -1 ) then
               lu_out = LUAvail()
               if ( lu_out .lt. 0 ) then
                  call PErr ( MyName, 'No logical units '
     .                       // 'available for the '
     .                       // 'output files.  \W ' )
                  stat = No_LUAvail
                  close ( lu_in )
                  return
               end if
            else
!              Logical unit already assigned is assumed
!              to be connected to an opened output file. 
!              ----------------------------------------
               close (lu_out)
            end if

!           .. generate the name of the next output file
!           --------------------------------------------
            call StrTPlate ( TPlate, StnIDF, OutFile, stat ) 
            if ( stat .ne. No_Error ) then
               call WPErr ( MyName,
     .                         ErrStat ( 'StrTemplate', stat ) 
     .                 // '\n\C   WMO station ID = ' // StnIDF
     .                 //   '\n   Template   = '
     .                 //      trim ( TPlate )
     .                 //   '\n   Input file = '
     .                 //      trim ( InFile ))
               close ( lu_in )
               return
            end if

!           ... open the next output file
!           -----------------------------
            open ( unit     =  lu_out,
     .             file     =  OutFile,
     .             form     = 'formatted',
     .             access   = 'sequential',
     .             position = 'append',
     .             recl     =  MaxLRec,
     .             iostat   =  stat )
            if ( stat .ne. 0 ) then
               call WPErr ( MyName,
     .                     'Open error (iostat = '
     .           //         trim ( ItoA ( stat ) ) // ')'
     .           // '\n\C    Input file = ' // trim ( InFile  )
     .           //   '\n   Output file = ' // trim ( OutFile))
               close ( lu_in )
               stat = Open_Error
               return
            end if
         end if

!        For each report ...
!        -------------------
         Last_RepSortLoop : do iRep = 1, NReports
            BInFile    = RepInfo % InFiles  ( iRep )
            BHeader    = RepInfo % Header   ( iRep )
            NRepRec    = RepInfo % NRepRec  ( iRep )

!           ... sort
!           --------
            Last_RecSortLoop : do iRepRec = 1, NRepRec
               iBRec   = RepInfo % RecList ( iRep, iRepRec )

!              ... replace NAddRec in record that is
!                  to be written to the output file
!              -------------------------------------
               Buffer % Record ( iBRec ) ( 103 : 105 )
     .                = RepInfo % CAdd     ( iRep, iRepRec )

!              ... and write each record in the proper order
!              ---------------------------------------------
               LBRec   = Buffer % LRec ( iBRec )
               write ( unit   = lu_out,
     .                 fmt    = '(a)',
     .                 iostat = stat )
     .                 Buffer % Record ( iBRec ) ( : LBRec )
               if ( stat .ne. 0 ) then
                  call WPErr ( MyName,
     .                        'Write error (iostat = '
     .                 //      trim ( ItoA ( stat )) // ')'
     .                 // '\n\C   Input file = '
     .                 //      trim ( BInFile )
     .                 //   '\n   Header     = '
     .                 //      trim ( BHeader ))
                  if ( lu_out .ne. -1 ) close ( lu_out ) 
                  close ( lu_in )
                  stat = Write_Error
                  return
               end if
            end do Last_RecSortLoop
         end do Last_RepSortLoop
      end if

!     Close output file
!     -----------------
      if ( lu_out .ne. -1 ) close (lu_out)

      return
      end subroutine Prep_Data

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: SetReports () - Get report info from buffer data structure
! 
! !DESCRIPTION:
!     This routine scans the buffer data structure for a given 
!     profile and returns the necessary information to write the
!     records in the proper order to an output file.  This 
!     routine scans only the first records with the same profile
!     tag.  This routine was created only for use in this module.
!
! !INTERFACE:
      subroutine SetReports ( ProfTag, Buffer, RepInfo, stat )
      use m_AdvError, only : ItoA, ErrStat, WPErr
      use m_AtoX,     only : AtoI
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len=*),    intent (in)  ::
     .   ProfTag          ! Tag for given profile
      type (CARDS_records), intent (in)  ::
     .   Buffer           ! All records for a single profile.
!
! !OUTPUT PARAMETERS:
      type (CARDS_repinfo), intent (out) ::
     .   RepInfo          ! Information for all reports
      integer,              intent (out) ::
     .   stat             ! The returned status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     10Jan2007  C. Redder  Original code
! EOP
!....................................................................

      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_RepInfo'
      integer, parameter :: ZMissing = -999999,
     .                      ZDiffMax = 5000
      character (len=*), parameter :: CZMissing = '-999999'

      integer :: LRec, NBRec, iRec, iBegLev, iEndLev, iBRec
      integer :: iBToken, iEToken
      integer :: iretB, iretT, iretE
      integer :: NAddRec, iAddRec, NAddMax_, NAddMin_
      integer :: NReports, ThisReport, iRep, iRep1, iRep2
      character (len=LRecLev) :: TopLevel, BotLevel,
     .                           TopLevels ( NBRecMax ),
     .                           BotLevels ( NBRecMax )
      character (len=LHeader) :: Header, HeaderLast
      character (len=7      ) :: ZToken
      character (len=255    ) :: InFile
      logical :: new_header, new_report, keep_report,
     .           equivalent_records ( NRepMax ),
     .           top_record, bottom_record
      integer :: TZ, BZ, EZ, TZLast,
     .           TopZ    ( NBRecMax ),
     .           BotZ    ( NBRecMax ),
     .           Elev    ( NRepMax ),
     .           ZMin    ( NRepMax ),
     .           Indx    ( NRepMax, 0 : NBRecMax - 1 ),
     .           ZBotRec ( NRepMax, 0 : NBRecMax - 1 ),
     .           ZTopRec ( NRepMax, 0 : NBRecMax - 1 ),
     .           NAddMax ( NRepMax )


!     Default status
!     --------------
      stat = 0

!     For each record (or NCDC block) encountered ...
!     -----------------------------------------------
      HeaderLast = ' '
      NReports   =  0
      NBRec      =  Buffer % NBRec
      RecordLoop : do iRec = 1, NBRec

!        ... process only the records that have the desired profile tag
!        --------------------------------------------------------------
         if ( Buffer  % ProfTag ( iRec ) .ne. ProfTag ) cycle RecordLoop

!        ... get ...
!        -----------
         InFile   = Buffer  % InFiles ( iRec ) ! input filename
         Header   = Buffer  % Record  ( iRec ) ( : LHeader ) ! header info
         LRec     = Buffer  % LRec    ( iRec ) ! length of record
         NAddRec  = Buffer  % NAddRec ( iRec ) ! number of additional records
         iBegLev  = LHeader + 1                ! obs at bottom level and
         iEndLev  = iBegLev + LRecLev - 1
         BotLevel = Buffer  % Record  ( iRec ) ( iBegLev : iEndLev )
         BotLevels ( iRec ) = BotLevel
         iBegLev  = LRec    - LRecLev + 1      ! ... top level in record
         iEndLev  = iBegLev + LRecLev - 1
         TopLevel = Buffer  % Record  ( iRec ) ( iBegLev : iEndLev )
         TopLevels ( iRec ) = TopLevel
         TZ       = AtoI ( TopLevel ( 13:19 ), iretT ) ! height at top and
         BZ       = AtoI ( BotLevel ( 13:19 ), iretB ) ! .... bottom levels
         if ( iretT .ne. 0 ) TZ = ZMissing
         if ( iretB .ne. 0 ) BZ = ZMissing 

!        ... if necessary, search the entire record for the highest ...
!        --------------------------------------------------------------
         if ( TZ .eq. ZMissing ) then
            do iBegLev = LRec - 2 * LRecLev + 1, LHeader, -LRecLev
               iEndLev = iBegLev + LRecLev
               iBToken = iBegLev + 13 - 1
               iEToken = iBegLev + 19 - 1
                ZToken = Buffer  % Record ( iRec )( iBToken : iEToken )
                if ( ZToken .ne. CZMissing ) then
                   TZ  = AtoI ( ZToken, iretT )
                   if ( iretT .eq. 0 ) then
                      TopLevel
     .                 = Buffer  % Record ( iRec )( iBegLev : iEndLev )
     .                                                      
                      exit
                   end if 
                end if
            end do
         end if

!        ... and lowest levels with valid height values
!        -------------------------------------------------
         if ( BZ .eq. ZMissing ) then
            do iBegLev = LHeader + LRecLev + 1, LRec, LRecLev
               iEndLev = iBegLev + LRecLev
               iBToken = iBegLev + 13 - 1
               iEToken = iBegLev + 19 - 1
                ZToken = Buffer  % Record ( iRec )( iBToken : iEToken )
                if ( ZToken .ne. CZMissing ) then
                   BZ  = AtoI ( ZToken, iretB )
                   if ( iretB .eq. 0 ) then
                      BotLevel
     .                 = Buffer  % Record ( iRec )( iBegLev : iEndLev )
                      exit
                   end if 
                end if
            end do
         end if

!        ... skip record if height value at
!            top or bottom is still missing
!        ----------------------------------
         if ( TZ .ne. ZMissing .and.
     .        BZ .ne. ZMissing ) then
            BotZ ( iRec ) = BZ
            TopZ ( iRec ) = TZ
         else
            cycle RecordLoop
         end if


!        ... new report ...
!        ------------------
         new_header = Header ( : 102 ) .ne. HeaderLast ( : 102 )
         if ( new_header ) then
            ThisReport = 0
            do iRep = 1, NReports - 1
               if ( RepInfo % Header ( iRep ) ( : 102 ) .eq.
     .                        Header          ( : 102 )) then
                  ThisReport = iRep
                  exit 
               end if
            end do
         end if

!        ... indicated by header not already encountered
!        -----------------------------------------------
         new_report = ThisReport .eq. 0
         if ( new_report ) then
            NReports = NReports + 1
            if ( NReports .gt. NRepMax ) then
               call WPErr ( MyName, 'Maximum number of '
     .                       //     'reports (='
     .                       //      trim ( ItoA ( NRepMax ))
     .                       //     ') in the buffer has been '
     .                       //     'exceeded '
     .                       // '\n\C   Input file  = '
     .                       //      trim ( InFile )
     .                       //   '\n   Last header = '
     .                       //      trim ( Header ))
               stat = Buffer_Error
               return
            end if
            ThisReport = NReports

!           Save info for report
!           --------------------
            EZ = AtoI ( Header ( 34:38 ), iretE )
            if ( iretE .ne. 0     ) EZ = 0
            if ( EZ    .eq. 99999 ) EZ = 0
            NAddMax ( ThisReport )    =  NAddRec
            Elev    ( ThisReport )    =  EZ
            ZMin    ( ThisReport )    =  TZ
            Indx    ( ThisReport, : ) = -1
            ZTopRec ( ThisReport, : ) =  ZMissing
            ZBotRec ( ThisReport, : ) =  ZMissing
            equivalent_records ( ThisReport ) = .true.

!           ... so initialize output data structure for this report
!           -------------------------------------------------------
            RepInfo % InFiles ( ThisReport ) =  InFile
            RepInfo % Header  ( ThisReport ) =  Header
         end if

         top_record    = NAddRec .eq. 0
         bottom_record = BZ - Elev ( ThisReport ) .le. ZDiffMax 

!        ... and include the record only if consistent
!        ---------------------------------------------
         iBRec = Indx ( ThisReport, NAddRec )
         if ( iBRec .eq. -1 ) then
            Indx       ( ThisReport, NAddRec ) = iRec
            ZTopRec    ( ThisReport, NAddRec ) = TZ
            ZBotRec    ( ThisReport, NAddRec ) = BZ
            ZMin ( ThisReport ) 
     .         = min   ( ZMin    ( ThisReport ), BZ      )
            NAddMax    ( ThisReport )
     .         = max   ( NAddMax ( ThisReport ), NAddRec )
         else
            if ( .not. top_record    ) then
               equivalent_records ( ThisReport )
     .            =    equivalent_records ( ThisReport ) 
     .           .and. TopLevel .eq. TopLevels ( iBRec )
            end if
            if ( .not. bottom_record ) then
               equivalent_records ( ThisReport )
     .            =    equivalent_records ( ThisReport ) 
     .           .and. BotLevel .eq. BotLevels ( iBRec )
            end if
         end if

         HeaderLast = Header
      end do RecordLoop

!     For each report ...
!     -------------------
      iRep2 = 0
      ReportLoop : do iRep1 = 1, NReports

!        ... keep the report only if the sounding
!            begins near the lowest level
!        ----------------------------------------
         keep_report = ZMin ( iRep1 ) - Elev ( iRep1 ) .le. ZDiffMax
         if ( keep_report ) then
            iRep2 = iRep2 + 1

            NAddMax_ = NAddMax ( iRep1 )
            NAddMin_ = 0
            TZLast   = ZTopRec ( iRep1, NAddMax_ )
            do iAddRec = NAddMax_ - 1, 0, -1
               iBRec   = Indx    ( iRep1, iAddRec )
               BZ      = ZBotRec ( iRep1, iAddRec )

!              ... check for missing records in the report
!              -------------------------------------------
               if      ( iBRec .eq.     -1 ) then
                  NAddMin_ = iAddRec + 1
                  exit

!              ... and for height consistency
!              ------------------------------
               else if ( BZ    .lt. TZLast ) then
                  NAddMin_ = iAddRec + 1
                  exit
               end if
               TZLast = ZTopRec ( iRep1, iAddRec )
            end do

!           Use only the bottom record if all records
!           with duplicate header are not equivalent
!           -----------------------------------------
            if ( .not. equivalent_records ( iRep1 ))
     .         NAddMin_ = NAddMax ( iRep1 )

!           Save the necessary info in the output data stucture
!           ---------------------------------------------------
            RepInfo % InFiles ( iRep2 )
     .                        = RepInfo % InFiles ( iRep1 )
            RepInfo % Header  ( iRep2 )
     .                        = RepInfo % Header  ( iRep1 )
            RepInfo % NRepRec ( iRep2 )
     .                        = NAddMax_ - NAddMin_ + 1
            iRec = 0
            do iAddRec = NAddMax_, NAddMin_, -1
               iRec    = iRec + 1
               RepInfo % RecList ( iRep2, iRec    )
     .                 = Indx    ( iRep1, iAddRec )
               write ( RepInfo % CAdd ( iRep2, iRec ), '(i3.3)')
     .             iAddRec - NAddMin_
            end do 
         end if
      end do ReportLoop
      RepInfo % NReports = iRep2

      return
      end subroutine SetReports

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  StrTPlate () - Evaluate the %s desciptor in a GRaDS style string template 
! 
! !DESCRIPTION:
!     This routine evaluates the %s descriptor in a GrADS style string 
!     template.  This routine is a simplified version of the routine
!     StrTemplate in the module m_StrTemplate and was adapted for use
!     only within this module.
!
! !INTERFACE:
      subroutine StrTPlate ( TPlate, XID, Str, stat )
      use m_AdvError, only : ItoA, PErr, ErrStat
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len=*), intent (in) ::
     .   TPlate,         ! Template
     .   XID             ! The string substituting every occurence of "%s".
!                        !   Trailing blanks will be ignored.
! !OUTPUT PARAMETERS:
      character (len=*), intent(out) ::
     .   Str             ! The output string
      integer,           intent(out) ::
     .   stat            ! The returned status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     10May2002  C. Redder  Adapted from the module m_StrTemplate..
!     08Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
! EOP
!....................................................................

      character (len=*), parameter :: MyName = MyModule // '::TPlate'
      integer :: i,i1,i2,m,k
      integer :: ln_tmpl,ln_str
      integer :: istp,kstp
      character(len=1) :: c0,c1,c2

      ln_tmpl = len_trim ( TPLate ) ! size of the format template
      ln_str  = len ( Str )         ! size of the output string
      stat    = 0
      Str     = ' '
      i       = 0
      istp    = 1
      k       = 1
      kstp    = 1

      CharLoop : do while ( i+istp .le. ln_tmpl ) ! A loop over all char-
                                                  !   acters in TPlate
         if ( k .gt. ln_Str ) exit                ! truncate the output here.

         i  = i + istp
         c0 = TPlate (i:i)

         select case (c0)
         case ('%')
            c1 = ' '
            i1 = i+1
            if ( i1 <= ln_tmpl ) c1 = TPlate ( i1 : i1 )

            select case (c1)
            case ('s')
               istp      = 2
               m         = min ( k + len_trim ( XID ) - 1, ln_str )
               Str (k:m) = XID
               k         = m+1
               cycle

            case ('%')
               istp      =  2
               Str(k:k)  = '%'
               k         =  k + 1 ! kstp=1
               cycle

            case default
               call PErr ( MyName, 'Invalid template entry, \C'
     .                          //  trim ( TPlate (i:)))
               stat = Bad_Template
               return
            end select ! for case (c1)
                       ! -------------
         case default

            istp         = 1
            Str ( k:k )  = TPlate ( i:i )
            k            = k+1

         end select    ! for case (c0)
                       ! -------------
      end do CharLoop

      return
      end subroutine StrTPlate
!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_Meta1 - Rearrange meta-data
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_Meta1 ( Meta1, Indx, Meta2, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      type (CARDS_meta), intent (in) ::
     .   Meta1           ! Meta-data
      integer,           intent (in), dimension (:) ::
     .   Indx            ! Sorting indices
      logical,           intent (in), optional ::
     .   reverse         ! = .true. to reverse the reordering 
                         !   Default: reverse = .false.
!
! !OUTPUT PARAMETERS:
      type (CARDS_meta), intent (inout) ::
     .   Meta2           ! Rearranged meta-data
!
! !REVISION HISTORY:
!     14May2002  C. Redder  Orininal code
!     13Dec2002  C. Redder  Added the optional argument, reverse
!     30Jan2003  C. Redder  Replaced the component Date and Time with
!                           DateHr in the data structure CARDS_meta. 
!     20Dec2006  C. Redder  Replaced the component DateHr with Date and
!                           Hour in the data structure CARDS_meta. 
!     24Apr2007  C. Redder  Added the components, LTHist and QC_Rep
!                           Combined the components, Date and Hour,
!                           to DateHr.
!
!EOP
!-------------------------------------------------------------------------

      integer :: iRep, NRep
      logical :: forward

      forward = .true.
      if ( present ( reverse )) forward = .not. reverse 
      NRep    = size ( Indx )

      if ( forward ) then
         Meta2 % StnID   ( : NRep )
     .      = (/( Meta1 % StnID   ( Indx ( iRep )), iRep = 1, NRep )/)
         Meta2 % Lat     ( : NRep )
     .      = (/( Meta1 % Lat     ( Indx ( iRep )), iRep = 1, NRep )/)
         Meta2 % Lon     ( : NRep )
     .      = (/( Meta1 % Lon     ( Indx ( iRep )), iRep = 1, NRep )/)
         Meta2 % Elev    ( : NRep )
     .      = (/( Meta1 % Elev    ( Indx ( iRep )), iRep = 1, NRep )/)
         Meta2 % DateHr  ( : NRep )
     .      = (/( Meta1 % DateHr  ( Indx ( iRep )), iRep = 1, NRep )/)
         Meta2 % LTime   ( : NRep )
     .      = (/( Meta1 % LTime   ( Indx ( iRep )), iRep = 1, NRep )/)
         Meta2 % LTHist  ( : NRep )
     .      = (/( Meta1 % LTHist  ( Indx ( iRep )), iRep = 1, NRep )/)
         Meta2 % kx      ( : NRep )
     .      = (/( Meta1 % kx      ( Indx ( iRep )), iRep = 1, NRep )/)
         Meta2 % Type    ( : NRep )
     .      = (/( Meta1 % Type    ( Indx ( iRep )), iRep = 1, NRep )/)
         Meta2 % QC_Rep  ( : NRep )
     .      = (/( Meta1 % QC_Rep  ( Indx ( iRep )), iRep = 1, NRep )/)
         Meta2 % NLevels ( : NRep )
     .      = (/( Meta1 % NLevels ( Indx ( iRep )), iRep = 1, NRep )/)
         Meta2 % Loc     ( : NRep )
     .      = (/( Meta1 % Loc     ( Indx ( iRep )), iRep = 1, NRep )/)

      else
         Meta2 % StnID   ( Indx ( : NRep ))
     .      = (/( Meta1 % StnID   ( iRep ), iRep = 1, NRep )/)
         Meta2 % Lat     ( Indx ( : NRep ))
     .      = (/( Meta1 % Lat     ( iRep ), iRep = 1, NRep )/)
         Meta2 % Lon     ( Indx ( : NRep ))
     .      = (/( Meta1 % Lon     ( iRep ), iRep = 1, NRep )/)
         Meta2 % Elev    ( Indx ( : NRep ))
     .      = (/( Meta1 % Elev    ( iRep ), iRep = 1, NRep )/)
         Meta2 % DateHr  ( Indx ( : NRep ))
     .      = (/( Meta1 % DateHr  ( iRep ), iRep = 1, NRep )/)
         Meta2 % LTime   ( Indx ( : NRep ))
     .      = (/( Meta1 % LTime   ( iRep ), iRep = 1, NRep )/)
         Meta2 % LTHist  ( Indx ( : NRep ))
     .      = (/( Meta1 % LTHist  ( iRep ), iRep = 1, NRep )/)
         Meta2 % kx      ( Indx ( : NRep ))
     .      = (/( Meta1 % kx      ( iRep ), iRep = 1, NRep )/)
         Meta2 % Type    ( Indx ( : NRep ))
     .      = (/( Meta1 % Type    ( iRep ), iRep = 1, NRep )/)
         Meta2 % QC_Rep  ( Indx ( : NRep ))
     .      = (/( Meta1 % QC_Rep  ( iRep ), iRep = 1, NRep )/)
         Meta2 % NLevels ( Indx ( : NRep ))
     .      = (/( Meta1 % NLevels ( iRep ), iRep = 1, NRep )/)
         Meta2 % Loc     ( Indx ( : NRep ))
     .      = (/( Meta1 % Loc     ( iRep ), iRep = 1, NRep )/)

      end if

      return
      end subroutine Reorder_Meta1

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_Meta2 - Rearrange meta-data
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_Meta2 ( Meta, Indx, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      integer,           intent (in), dimension (:) ::
     .   Indx            ! Sorting indices
      logical,           intent (in), optional ::
     .   reverse         ! = .true. to reverse the reordering 
                         !   Default: reverse = .false.
!
! !INPUT/OUTPUT PARAMETERS:
      type (CARDS_meta), intent (inout) ::
     .   Meta            ! Rearranged meta-data
!
! !REVISION HISTORY:
!     14May2002  C Redder  Orininal code
!     13Dec2002  C Redder  Added the optional argument, reverse.
!
!EOP
!-------------------------------------------------------------------------

      call Reorder_Meta1 ( Meta, Indx, Meta, reverse = reverse )

      return
      end subroutine Reorder_Meta2

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_TSMeta1 - Rearrange time-series meta-data
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_TSMeta1 ( TSMeta1, Indx, TSMeta2, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      type (CARDS_tsmeta), intent (in) ::
     .   TSMeta1           ! Time-series meta-data
      integer,             intent (in), dimension (:) ::
     .   Indx              ! Sorting indices
      logical,             intent (in), optional ::
     .   reverse           ! = .true. to reverse the reordering 
                           !   Default: reverse = .false.
!
! !OUTPUT PARAMETERS:
      type (CARDS_tsmeta), intent (inout) ::
     .   TSMeta2           ! Rearranged time-series meta-data
!
! !REVISION HISTORY:
!     14May2002  C. Redder  Orininal code
!     24Apr2007  C. Redder  Added the components, LTHist and QC_Rep
!
!EOP
!-------------------------------------------------------------------------

      integer :: iRep, NRep
      logical :: forward

      forward = .true.
      if ( present ( reverse )) forward = .not. reverse 
      NRep    = size ( Indx )

      if ( forward ) then
         TSMeta2 % StnID   ( : NRep )
     .      = (/( TSMeta1 % StnID   ( Indx ( iRep )), iRep = 1, NRep )/)
         TSMeta2 % Lat     ( : NRep )
     .      = (/( TSMeta1 % Lat     ( Indx ( iRep )), iRep = 1, NRep )/)
         TSMeta2 % Lon     ( : NRep )
     .      = (/( TSMeta1 % Lon     ( Indx ( iRep )), iRep = 1, NRep )/)
         TSMeta2 % Elev    ( : NRep )
     .      = (/( TSMeta1 % Elev    ( Indx ( iRep )), iRep = 1, NRep )/)
         TSMeta2 % DateHr  ( : NRep )
     .      = (/( TSMeta1 % DateHr  ( Indx ( iRep )), iRep = 1, NRep )/)
         TSMeta2 % LTime   ( : NRep )
     .      = (/( TSMeta1 % LTime   ( Indx ( iRep )), iRep = 1, NRep )/)
         TSMeta2 % LTHist   ( : NRep )
     .      = (/( TSMeta1 % LTHist  ( Indx ( iRep )), iRep = 1, NRep )/)
         TSMeta2 % kx      ( : NRep )
     .      = (/( TSMeta1 % kx      ( Indx ( iRep )), iRep = 1, NRep )/)
         TSMeta2 % Type    ( : NRep )
     .      = (/( TSMeta1 % Type    ( Indx ( iRep )), iRep = 1, NRep )/)
         TSMeta2 % QC_Rep  ( : NRep )
     .      = (/( TSMeta1 % QC_Rep  ( Indx ( iRep )), iRep = 1, NRep )/)
         TSMeta2 % NLevels ( : NRep )
     .      = (/( TSMeta1 % NLevels ( Indx ( iRep )), iRep = 1, NRep )/)
         TSMeta2 % Loc     ( : NRep )
     .      = (/( TSMeta1 % Loc     ( Indx ( iRep )), iRep = 1, NRep )/)

      else
         TSMeta2 % StnID   ( Indx ( : NRep ))
     .      = (/( TSMeta1 % StnID   ( iRep ), iRep = 1, NRep )/)
         TSMeta2 % Lat     ( Indx ( : NRep ))
     .      = (/( TSMeta1 % Lat     ( iRep ), iRep = 1, NRep )/)
         TSMeta2 % Lon     ( Indx ( : NRep ))
     .      = (/( TSMeta1 % Lon     ( iRep ), iRep = 1, NRep )/)
         TSMeta2 % Elev    ( Indx ( : NRep ))
     .      = (/( TSMeta1 % Elev    ( iRep ), iRep = 1, NRep )/)
         TSMeta2 % DateHr  ( Indx ( : NRep ))
     .      = (/( TSMeta1 % DateHr  ( iRep ), iRep = 1, NRep )/)
         TSMeta2 % LTime   ( Indx ( : NRep ))
     .      = (/( TSMeta1 % LTime   ( iRep ), iRep = 1, NRep )/)
         TSMeta2 % LTHist  ( Indx ( : NRep ))
     .      = (/( TSMeta1 % LTHist  ( iRep ), iRep = 1, NRep )/)
         TSMeta2 % kx      ( Indx ( : NRep ))
     .      = (/( TSMeta1 % kx      ( iRep ), iRep = 1, NRep )/)
         TSMeta2 % Type    ( Indx ( : NRep ))
     .      = (/( TSMeta1 % Type    ( iRep ), iRep = 1, NRep )/)
         TSMeta2 % QC_Rep  ( Indx ( : NRep ))
     .      = (/( TSMeta1 % QC_Rep  ( iRep ), iRep = 1, NRep )/)
         TSMeta2 % NLevels ( Indx ( : NRep ))
     .      = (/( TSMeta1 % NLevels ( iRep ), iRep = 1, NRep )/)
         TSMeta2 % Loc     ( Indx ( : NRep ))
     .      = (/( TSMeta1 % Loc     ( iRep ), iRep = 1, NRep )/)

      end if

      return
      end subroutine Reorder_TSMeta1

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_TSMeta2 - Rearrange time-series meta-data
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_TSMeta2 ( TSMeta, Indx, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      integer,             intent (in), dimension (:) ::
     .   Indx              ! Sorting indices
      logical,             intent (in), optional ::
     .   reverse           ! = .true. to reverse the reordering 
                           !   Default: reverse = .false.
!
! !INPUT/OUTPUT PARAMETERS:
      type (CARDS_tsmeta), intent (inout) ::
     .   TSMeta            ! Rearranged time-series meta-data
!
! !REVISION HISTORY:
!     14Mar2007  C Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      call Reorder_TSMeta1 ( TSMeta, Indx, TSMeta, reverse = reverse )

      return
      end subroutine Reorder_TSMeta2

!....................................................................
      end module m_CARDS
*====================================================================
