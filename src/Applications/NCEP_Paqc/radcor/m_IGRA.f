!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_IGRA -- Utility routines for reading IGRA data set from NCDC
!
! !DESCRIPTION:
!     This module contains the utility routines for reading the IGRA
!     (Integrated Global Radiosonde Archive) data set from NCDC.
!
! !INTERFACE:
!
      module   m_IGRA
      use      m_RadData, only : LTHist_File, LTHist_ObTime
      implicit NONE
      private	              ! except
      public :: 
     .   IGRA_Header,         ! Derived type containing the header info
     .   IGRA_Obs,            ! ... the observation data
     .   IGRA_QC,             ! ... with quality control (QC) info
     .   IGRA_IO,             ! ... with input/output (IO) flags
     .   IGRA_Wind,           ! ... with calculated quanities at wind,
     .   IGRA_SigT,           ! ... standard and significant thermo levels
     .   IGRA_Sounding,       ! ... raw sounding data
     .   IGRA_Profile,        ! ... processed profile data
     .   IGRA_Stations,       ! ... list of stations
     .   IGRA_Station,        ! ... info for one station
     .   LT_File,             ! Launch time flag indicating value from file
     .   LT_ObTime            ! ... value missing and set to ob time

      public ::
     .   IGRA_Nullify,        ! Nullify ...
     .   IGRA_Clean,          ! ... reset and deallocate ...
     .   IGRA_Init,           ! ... initialize and allocate the data structure
     .   IGRA_Open,           ! Open an IGRA file
     .   IGRA_Get,            ! Read  data from an IGRA file
     .   IGRA_Put             ! Write data to an IGRA file

      interface IGRA_Nullify
         module procedure
     .      Nullify_Header,   ! ... for header info 
     .      Nullify_Sounding, ! ... for raw sounding data
     .      Nullify_Profile,  ! ... for profile data
     .      Nullify_Stations  ! ... for list of stations
      end interface
      interface IGRA_Clean
         module procedure
     .      Clean_Header,     ! ... for header info 
     .      Clean_Sounding,   ! ... for raw sounding data
     .      Clean_Profile,    ! ... for profile data
     .      Clean_Stations    ! ... for list of stations
      end interface
      interface IGRA_Init
         module procedure
     .      Init_Header,      ! ... for header info 
     .      Init_Sounding,    ! ... for raw sounding data
     .      Init_Profile,     ! ... for profile data
     .      Init_Stations     ! ... for list of stations
      end interface
      interface IGRA_Get
         module procedure
     .      Get_Sounding,
     .      Get_Stations,
     .      Get_StnMeta,
     .      Get_Profile,
     .      Get_RadVect,
     .      Get_RadMeta
      end interface
      interface IGRA_Put
         module procedure
     .      Put_Sounding,
     .      Put_Profile,
     .      Put_RadVect
      end interface

!
! !REVISION HISTORY:
!     24Apr2007  C. Redder  Original code
!EOP
!-----------------------------------------------------------------

!     Error status and handling
!     -------------------------
      character (len=*), parameter :: MyModule = 'm_IGRA'

!     Special characters
!     ------------------
      character (  len  =   1 ), parameter ::
     .   BLK = achar (  32 ),    ! blank
     .   EOL = achar (  10 )     ! end of line  mark

!     Parameters for the IGRA records
!     -------------------------------
      integer, parameter ::
     .   LHeader     = 24,       ! String length for header section
     .   LRecLev     = 36,       ! ... and each level in the CARDS report
     .   LRecMax     = 40,       ! ... maximum record length
     .   LStnID      = 5,        ! ... and for each WMO station ID
     .   LStnName    = 35,       ! ... and its name
     .   LStnRec     = 81,       ! ... and record containing its info
     .   LRightMar   = 25,       ! ... and assume right margin
     .   LDateHr     = 10        ! ... and data/hour tag
      integer, parameter ::
     .   NVar_IGRA   = 5,        ! Number of variables at each level (excl pres)
     .   NLevMax_Def = 255,      ! Default maximum number of records
     .   NStnMax_Def = 2000,     ! Default maximum number of stations
     .   MaxNRec     = 100000    ! Maximum number of records in a file

      real,    parameter ::
     .   Temp0_K     =  273.16
      integer, parameter ::
     .   IMiss       = -9999,
     .   IO_OK       = 0,
     .   IO_Rejected = 2
      character (len=*), parameter ::
     .   CMiss_LTime =  '9999',
     .   CError_Data = '-8888',
     .   CMiss_Data  = '-9999'
      character (len=*), parameter ::
     .   Header_Marker   = '#',  ! marker indicating beginning of header
     .   StnID_Filler    =  BLK, ! filler for WMO station ID
     .   Comment_Char    = '%'   ! comment character

!     Level types
!     -----------
      character (len=1), parameter ::
     .   StanLevel  = '1',       ! Standard pressure level (major type)
     .   SigTLevel  = '2',       ! Significant thermodynamic level (major type)
     .   WindLevel  = '3',       ! Additional wind level (major type)
     .   Surface    = '1',       ! Surface level (minor type)
     .   Tropopause = '2',       ! Tropopause level (minor type)
     .   OtherType  = '0'        ! Other (minor type)

!     Status codes
!     ------------
      integer, parameter ::
     .   No_Error        = 0,    ! Valid error status; no error
     .   No_LUAvail      = 1,    ! No FORTRAN logical unit number
     .   Open_Error      = 2,    ! Error in opening a file
     .   Read_Error      = 3,    ! Error in reading a file
     .   Write_Error     = 4,    ! Error in writing to a file
     .   Buffer_Error    = 5,    ! Buffer error (not enough space)
     .   Alloc_Error     = 6,    ! Allocation error
     .   Dealloc_Error   = 7,    ! Deallocation error
     .   Close_Error     = 8,    ! Error in closing a file
     .   Missing_Record  = 9,    ! Missing record 
     .   Bad_Header      = 10,   ! Bad header 
     .   Bad_Record      = 11,   ! Bad record
     .   Bad_Level       = 12,   ! Bad level
     .   Bad_Template    = 13,   ! Bad file template
     .   Bad_Argument    = 14,   ! Bad input subroutine argument
     .   Bad_Value       = 15,   ! Bad value
     .   No_Header       = 16    ! No header in file

!     Flags for launch time history
!     -----------------------------
      integer, parameter ::
     .   LT_File         = LTHist_File,
     .   LT_ObTime       = LTHist_ObTime

!     Data structures for header information including ...
!     ----------------------------------------------------
      type IGRA_Header
         character (len=LStnID)  ::
     .      StnID   =  BLK       ! ... WMO station ID
         integer                 ::
     .      DateHr  =  0,        ! ... synoptic date (YYYYMMDDHH format)
     .      LTime   =  0,        ! ... launch time (s from Date/Hr)
     .      LTHist  =  0,
     .      NLevels =  0         ! ... number of levels in profile
      end type

!     ... for observtation data
!     -------------------------
      type IGRA_Obs
         integer ::
     .      NLevMax =  0,
     .      NLevels =  0
         logical,                  dimension (:), pointer ::
     .      Mask    => null()    ! = .true. if valid
         integer,                  dimension (:), pointer ::
     .      Indx    => null(),   ! ... sorting indices
     .      LevList => null()    ! ... list of accepted levels
         real,                     dimension (:), pointer ::
     .      Pres    => null(),   ! ... pressure (mb)
     .      Height  => null(),   ! ... geopotential height flag (m)
     .      Temp    => null(),   ! ... temperature (deg K)
     .      DewPt   => null(),   ! ... dew point   (deg K)
     .      WDir    => null(),   ! ... wind direction (0-360 deg)
     .      WSpeed  => null(),   ! ... wind speed  (m/s)
     .      P       => null(),   ! ... interpolated pressure (mb)
     .      logP    => null(),   ! ... and its natural logarithm
     .      H       => null(),   ! ... and heights at all levels (mb)
     .      UWind   => null(),   ! ... zonal wind (m/s)
     .      VWind   => null(),   ! ... meridional wind (m/s)
     .      ETime   => null(),   ! ... elapsed time since launch (s)
     .      DLat    => null(),   ! ... drift latitude (deg)
     .      DLon    => null()    ! ... and longitude (deg, -90=90W)

         character (len=1),        dimension (:), pointer ::
     .      MajorLType => null(),! ... major level type (see above)
     .      MinorLType => null() ! ... minor level type (see above)
      end type

!     ... and its QC info
!     -------------------
      type IGRA_QC
         integer ::
     .      NLevMax =  0,
     .      NLevels =  0
         integer,                  dimension (:), pointer ::
     .      Pres    => null(),   ! Quality control (QC) flag
     .      Height  => null(),   ! ... = QC_OK if corresponding ob is valid
     .      Temp    => null(),   !     = QC_Suspect  ... suspect
     .      DewPt   => null(),   !     = QC_Rejected ... rejected
     .      Wind    => null()
      end type

!     ... and its IO info
!     -------------------
      type IGRA_IO
         integer ::
     .      NLevMax =  0,
     .      NLevels =  0
         integer,                  dimension (:), pointer ::
     .      Pres    => null(),   ! Input/output (IO) flag
     .      Height  => null(),   ! ... = IO_OK if use of IO is OK
     .      Temp    => null(),   !     = IO_Rejected ... rejected
     .      DewPt   => null(),
     .      Wind    => null()
      end type

!     ... and derived or vertically interpolated mass quanitites for
!         major level types 1 (standard) and 2 (significant thermodynamic)
!     -------------------------------------------------------------------
      type IGRA_SigT
         integer ::
     .      NLevMax =  0,
     .      NLevels =  0
         logical,                  dimension (:), pointer ::
     .      Mask    => null()    ! = .true. if valid, = .false. if rejected
         integer,                  dimension (:), pointer ::
     .      Indx    => null(),   ! Sorting indices
     .      LevList => null(),   ! List of levels in type IGRA_Obs
     .      IPres   => null()    ! Pressure (x 100 mb) in integer form
         real,                     dimension (:), pointer ::
     .      Pres    => null(),   ! Pressure obs (mb or hPa),
     .      Height  => null(),   ! Height obs (m),
     .      Temp    => null(),   ! Temperature obs (deg K),
     .      DewPt   => null(),   ! Dew point obs (deg K),
     .      logP    => null(),   ! Natural log of pressure
     .      H       => null(),   ! Interpolated heights (m)
     .      T       => null(),   ! ... temperature (deg K)
     .      VT      => null(),   ! ..  vertual temperature (deg K)
     .      DP      => null(),   ! ... dew point (deg K)
     .      RH      => null(),   ! Relative humidity (%),
     .      MR      => null()    ! Mixing ratio (g/kg)
      end type

!     ... and derived or vertically interpolated mass quanitites
!         for major level type 3 (additional wind)
!     ----------------------------------------------------------
      type IGRA_Wind
         integer ::
     .      NLevMax =  0,
     .      NLevels =  0
         logical,                  dimension (:), pointer ::
     .      Mask    => null()    ! = .true. if valid, = .false. if rejected
         integer,                  dimension (:), pointer ::
     .      Indx    => null(),   ! Sorting indices
     .      LevList => null(),   ! List of levels in type IGRA_Obs
     .      IHeight => null(),   ! Height (m) and ...
     .      IPres   => null()    ! ... pressure (x 100 mb) in integer form
         real,                     dimension (:), pointer ::
     .      Pres    => null(),   ! Pressure (mb or hPa),
     .      Height  => null(),   ! Height (m)
     .      logP    => null()    ! Natural log of pressure

      end type

!     ... raw unprocessing sounding data
!     ----------------------------------
      type IGRA_Sounding
         logical                 ::
     .      initialized
     .              = .false.    ! = .true. if intialized
         integer                 ::
     .      NLevMax =  0
         character (len=LHeader) ::
     .      Header  =  BLK       ! Entire record containing header info
         character (len=LRecLev),  dimension (:), pointer ::
     .      DataR   => null()    ! All records containing obs data
         type (IGRA_Header)      ::
     .      HdrInfo              ! Header info from first record
      end type

!     ... processing profile data
!     ---------------------------
      type IGRA_Profile
         logical                 ::
     .      initialized
     .              = .false.    ! = .true. if intialized
         type ( IGRA_Header )    :: Header
         type ( IGRA_Obs    )    :: Obs
         type ( IGRA_QC     )    :: QC
         type ( IGRA_IO     )    :: IO
         type ( IGRA_sigT   )    :: SigT
         type ( IGRA_Wind   )    :: Wind
      end type

!     ... list of stations with info
!     ------------------------------
      type IGRA_Stations
         integer ::
     .      NStnMax =  0,        ! ... maximum and
     .      NStn    =  0         ! ... actual number of stations
         character (len=LStnID),   dimension (:), pointer ::
     .      StnID   => null()    ! ... WMO station ID
         character (len=LStnName), dimension (:), pointer ::
     .      StnName => null()    ! ... and its name
         real,                     dimension (:), pointer ::
     .      Lat     => null(),   ! ... latitude
     .      Lon     => null(),   ! ... longitude (-90 = 90W)
     .      Elev    => null()    ! ... elev (m)
      end type

!     ... info for a single station
!     -----------------------------
      type IGRA_Station
         character (len=LStnID)   :: StnID   = BLK
         character (len=LStnName) :: StnName = BLK
         real :: Lat = 0.0, Lon = 0.0, Elev = 0.0
      end type

!     ... info for a single level
!     ---------------------------
      type IGRA_Level
         logical :: accept_level
         real    :: Pres, Height, Temp, DewPt, WDir, WSpeed
         integer :: QC_P, QC_H, QC_T, QC_DP, QC_Wind
         integer :: IO_P, IO_H, IO_T, IO_DP, IO_Wind
         character(len=1) :: MajorLType, MinorLType
      end type

      contains

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  IGRA_Open () --- Open an IGRA file
! 
! !DESCRIPTION:
!     This routine opens an IGRA file in read (or write) mode.  This routine
!     will prevent overwriting an pre-existing file.
!
! !INTERFACE:
      subroutine IGRA_Open ( File, mode, lu, stat,
     .                       MaxRL )
      use m_TextUtil,     only : LowerCase
      use m_AdvError,     only : ItoA, PErr, ErrStat, WPErr
      use m_SysIO,        only : LUAvail
!
! !INPUT PARAMETERS:
      implicit NONE
      character ( len=*),     intent (in)  ::
     .   File,                ! File to be opened
     .   mode                 ! Mode ("read" or "write")
      integer, optional,      intent (in)  ::
     .   MaxRL                ! Maximum record length. Default: 
!
! !OUTPUT PARAMETERS:
      integer,                intent (out) ::
     .   lu,                  ! Fortran logical unit number
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     15Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName = MyModule // '::IGRA_Open'

      logical :: file_exists
      integer :: LMode, iBeg, iEnd, MaxRecL
      character (len=len(mode)) :: mode_
      character (len=10) :: status

!     Default status
!     --------------
      stat = No_Error

      inquire ( file   = File,
     .          exist  = file_exists,
     .          iostat = stat )
      if ( stat .ne. 0 ) then
         call PErr ( MyName, 'Inquire error (iostat = '
     .                  //    trim ( ItoA ( stat ) ) // ')\C'
     .                  // '\n   File = ' // trim ( File ))
         lu   = -1
         stat = Open_Error
         return
      end if

!     Implement mode
!     --------------
      iEnd  = len_trim ( mode )
      iBeg  = scan ( mode ( : iEnd ), BLK, back = .true. ) + 1
      mode_ = LowerCase ( mode ( iBeg : iEnd ))
      if      ( mode_ .eq. 'read'  ) then
         if ( .not. file_exists ) then
            call WPErr ( MyName, 'File to be opened in "read" mode '
     .                    //     'does not exist. '
     .                    // '\C\n   File = ' // trim ( File ))
            stat = Open_Error
            return
         end if
         status = 'old'
      else if ( mode_ .eq. 'write' ) then
         if (       file_exists ) then
            call WPErr ( MyName, 'File to be opened in "write" mode '
     .                    //     'already exists. '
     .                    // '\C\n   File = ' // trim ( File ))
            stat = Open_Error
            return
         end if
         status = 'new'
      else
         call WPErr ( MyName, 'Mode (= ' // mode ( iBeg : iEnd )
     .                 //      ') must be "read" or "write"'
     .                 // '\C\n   File = ' // trim ( File ))
         stat = Bad_Argument
         return
      end if


!     Determine an available logical unit
!     -----------------------------------
      lu = LUAvail()
      if ( lu .lt. 0 ) then
         stat = No_LUAvail
         call PErr ( MyName, 'No logical units available for the '
     .                    // 'file, ' // trim ( File ) // '\W'  )
         return
      end if

!     Implement record length
!     -----------------------
      MaxRecL = LRecMax
      if ( present ( MaxRL )) MaxRecL = MaxRL
      MaxRecL = MaxRecL + LRightMar  ! Account for assumed right margin

!     Open IGRA file
!     --------------
      open ( unit   =  lu,
     .       file   =  File,
     .       form   = 'formatted',
     .       status =  status,
     .       access = 'sequential',
     .       recl   =  MaxRecL,
     .       iostat =  stat )
      if ( stat .ne. 0 ) then
         call PErr ( MyName, 'Open error (iostat = '
     .                  //    trim ( ItoA ( stat ) ) // ')\C'
     .                  // '\n   File = ' // trim ( File ))
         lu   = -1
         stat = Open_Error
         return
      end if
      
      return
      end subroutine IGRA_Open

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_Stations () --- Get the IGRA stations list from file
! 
! !DESCRIPTION:
!     This routine retrieves the IGRA stations list and info from file
!
! !INTERFACE:
      subroutine Get_Stations ( File, This, stat )
      use m_RadSort,      only : IndexSort, IndexSet
      use m_AdvError,     only : ItoA, PErr, ErrStat, WPErr, Alloc
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len=*),      intent (in)  ::
     .   File                 ! Name of input file
!
! !OUTPUT PARAMETERS:
      type ( IGRA_Stations ), intent (inout) :: 
     .   This                 ! Station list and info
      integer,                intent (out) ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     15Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_Stations'
      integer, parameter ::
     .   LFileRec = LStnRec + LRightMar

      integer, dimension (:), allocatable :: Indx
      logical :: eof, eor, rescan
      integer :: NStn, NStnMax
      integer :: iRec, LRec, iLine, LLine, iScan, lu
      character(len=LStnRec)  :: ThisLine
      character(len=LFileRec) :: ThisRecord
      character(len=510)      :: Tail1, Tail2, Tail2a, Message

      NStnMax = NStnMax_Def
      ScanLoop : do iScan = 1, 2

!        Open input file
!        ---------------
         call IGRA_Open ( File, 'read', lu, stat,
     .                    MaxRL = LStnRec )
         if ( stat .ne. No_Error ) then
            call WPErr ( MyName,
     .                   ErrStat ( 'IGRA_Open', stat ))
            go to 10
         end if
         Tail1 = '\n   File = ' // trim ( File )

!        Initialize output data structure
!        --------------------------------
         call Init_Stations ( NStnMax, This, stat )
         if ( stat .ne. No_Error ) then
            call WPErr ( MyName,
     .                   ErrStat ( 'IGRA_Open', stat ) // Tail1 )
            go to 10
         end if

!        Read each record
!        ----------------
         iLine = 0
         RecordLoop : do iRec = 1, MaxNRec

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
               go to 10
            end if
            iLine = iLine + 1

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
               go to 10
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

!           Add station info to output argument
!           -----------------------------------
            call Add_Station ( ThisLine, This, stat )
            if ( stat .ne. No_Error ) then
               call WPErr ( MyName,
     .                      ErrStat ( 'Add_Station', stat ) // Tail2 )
               go to 10
            end if

         end do RecordLoop

!        Determine if a rescan is necessary
!        ----------------------------------
         rescan = This % NStn .gt. NStnMax
         if ( rescan ) then

!           If so, save the number of stations listed in file
!           -------------------------------------------------
            NStnMax = This % NStn

!           ... reset the data structure
!           ----------------------------
            call Clean_Stations ( This, stat )
            if ( stat .ne. No_Error ) then
               call WPErr ( MyName,
     .                      ErrStat ( 'Clean_Stations', stat )
     .                   // Tail2 )
               go to 10
            end if

!           ... and close the file
!           ----------------------
            close ( lu )

         else

!           Otherwise close
!           ---------------
            close ( lu )

!           ... and exit loop and return
!           --------------------------
            exit ScanLoop
         end if

      end do ScanLoop

!     Sort and reorder according to station ID
!     ----------------------------------------
      NStn = This % NStn
      allocate ( Indx ( NStn ), stat = stat )
      call IndexSet  ( Indx ( : NStn ))
      call IndexSort ( Indx ( : NStn ), This % StnID, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'IndexSort', stat ))
         go to 10
      end if
      call Reorder_Stations2 ( This, Indx ( : NStn ))

!     Clean up if error status
!     ------------------------
 10   continue
      if ( stat .ne. No_Error ) then
         close ( lu )
         call Clean_Stations ( This )
      end if

      return
      end subroutine Get_Stations

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Add_Station () --- Add station info from text line
! 
! !DESCRIPTION:
!     This routine add station info from text line
!
! !INTERFACE:
      subroutine Add_Station ( TextLine, This, stat )
      use m_AdvError, only : ErrStat, WPErr
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len=*),      intent (in)    ::
     .   TextLine             ! Text line containing the info
!
! !OUTPUT PARAMETERS:
      type ( IGRA_Stations ), intent (inout) :: 
     .   This                 ! Sounding data
      integer,                intent (out)   ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     15Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName = MyModule // '::Add_Station'
      type ( IGRA_Station ) :: StnInfo
      integer :: NStn, NStnMax

!     
      call Get_Station ( TextLine, StnInfo, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName,
     .                ErrStat ( 'Get_Station', stat ))
         return
      end if

      NStn    = This % NStn + 1
      NStnMax = This % NStnMax
      if ( NStn .le. NStnMax ) then
         This % StnID   ( NStn ) = StnInfo % StnID
         This % StnName ( NStn ) = StnInfo % StnName
         This % Lat     ( NStn ) = StnInfo % Lat
         This % Lon     ( NStn ) = StnInfo % Lon
         This % Elev    ( NStn ) = StnInfo % Elev
      end if

      This % NStn = NStn

      return
      end subroutine Add_Station
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_Station () --- Get station info from text line
! 
! !DESCRIPTION:
!     This routine gets station info from text line
!
! !INTERFACE:
      subroutine Get_Station ( TextLine, This, stat )
      use m_AtoX,     only : AtoF
      use m_AdvError, only : ItoA, PErr, ErrStat, WPErr
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len=*),      intent (in)    ::
     .   TextLine             ! Text line containing the info
!
! !OUTPUT PARAMETERS:
      type ( IGRA_Station ),  intent (inout) :: 
     .   This                 ! Sounding data
      integer,                intent (out)   ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     15Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_Station'
      integer, parameter ::
     .   LLat  = 6,
     .   LLon  = 7,
     .   LElev = 4
      character (len=max ( LLat, LLon, LElev )) :: Token
      logical :: bad_value
      integer :: iBeg, iEnd
      real :: RNum

      stat  = No_Error

!     Get WMO station ID
!     ------------------
      iBeg  = 5
      iEnd  = iBeg + LStnID - 1
      This  % StnID = TextLine   ( iBeg : iEnd )
      if ( len_trim ( This % StnID ) .le. 0 ) then
         call WPErr ( MyName, ' WMO station ID is blank. ' )
         stat = Bad_Record
         return
      end if


!     ... station name
!     ----------------
      iBeg  = iEnd + 3
      iEnd  = iBeg + LStnName - 1
      This  % StnName = TextLine ( iBeg : iEnd )

!     ... latitude
!     ------------
      iBeg  = iEnd + 2
      iEnd  = iBeg + LLat - 1
      Token = TextLine ( iBeg : iEnd )
      RNum  = AtoF ( Token, stat )
      if ( stat .ne. 0 ) then
         bad_value = .true.
      else
         bad_value = RNum .gt.  90.005
     .          .or. RNum .lt. -90.005 
     .          .or. len_trim ( Token ) .eq. 0
      end if
      if ( bad_value ) then
         call WPErr ( MyName, ' Bad value for latitude (='
     .                       // trim ( Token ) // ')' )
         stat = Bad_Record
         return
      end if
      This % Lat = RNum

!     ... longitude
!     -------------
      iBeg  = iEnd + 2
      iEnd  = iBeg + LLon - 1
      Token = TextLine ( iBeg : iEnd )
      RNum  = AtoF ( Token, stat )
      if ( stat .ne. 0 ) then
         bad_value = .true.
      else
         bad_value = RNum .gt.  360.005
     .          .or. RNum .lt. -180.005 
     .          .or. len_trim ( Token ) .eq. 0
      end if
      if ( bad_value ) then
         call WPErr ( MyName, ' Bad value for longitude (='
     .                       // trim ( Token ) // ')' )
         stat = Bad_Record
         return
      end if
      This % Lon = RNum

!     ... elevation
!     -------------
      iBeg  = iEnd + 2
      iEnd  = iBeg + LElev - 1
      Token = TextLine ( iBeg : iEnd )
      RNum  = AtoF ( Token, stat )
      if ( stat .ne. 0 ) then
         bad_value = .true.
      else
         bad_value = RNum .gt. 6000.1
     .          .or. RNum .lt. -500.1 
     .          .or. len_trim ( Token ) .eq. 0
      end if
      if ( bad_value ) then
         call WPErr ( MyName, ' Bad value for elevation (='
     .                       // trim ( Token ) // ')' )
         stat = Bad_Record
         return
      end if
      This % Elev = RNum

      return
      end subroutine Get_Station
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_Sounding () --- Get the IGRA sounding data for one report.
! 
! !DESCRIPTION:
!     This routine retrieves IGRA sounding data for one report. 
!
! !INTERFACE:
      subroutine Get_Sounding ( lu, This, stat, eof )

      use m_AdvError,     only : ItoA, PErr, ErrStat, WPErr
!
! !INPUT PARAMETERS:
      implicit NONE
      integer,                intent (in)  ::
     .   lu                   ! Logical unit for input file
!
! !OUTPUT PARAMETERS:
      type ( IGRA_Sounding ), intent (inout) :: 
     .   This                 ! Sounding data
      integer,                intent (out) ::
     .   stat                 ! Status code
      logical,                intent (out) ::
     .   eof                  ! = .true. if end-of-file has been reached
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     15Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName   = MyModule // '::Get_Sounding'
      integer, parameter ::
     .   LFileRec = LRecMax + LRightMar

      logical :: eor, initialized
      integer :: NLevMax, iLev, NLevels, NLevelsH
      integer :: iRec, LRec, iLine, LLine
      character(len=LRecMax)  :: ThisLine
      character(len=LFileRec) :: ThisRecord
      character(len=510) :: Tail, Message
      type ( IGRA_Header ) :: Header

!     Default status
!     --------------
      stat = No_Error

!     Read each record
!     ----------------
      initialized = This % initialized
      iLine       = 0
      NLevels     = 0
      NLevelsH    = 0
      RecordLoop : do iRec = 1, MaxNRec

!        Read file line
!        --------------
         call ReadLn ( lu, LRec, ThisRecord, eor, eof, stat )
         if ( stat .ne. No_Error ) then
            if ( iLine .gt. 0 ) then
               call PErr ( MyName, 'Read error (iostat = '
     .                        //    trim ( ItoA ( stat )) // ') \C'
     .                        // '\n   Previous line = '
     .                        //    trim ( ThisLine )
     .                        // '\n   Header record = '
     .                        //    trim ( This % Header ))
            else
               if ( initialized ) then
                  NLevels = This % HdrInfo % NLevels
                  if ( NLevels .gt. 0 ) then
                     ThisLine = This % DataR ( NLevels )
                     call WPErr ( MyName, 'Read error (iostat = '
     .                        //           trim ( ItoA ( stat ))
     .                        //  ')\C\n   Previous line = '
     .                        //           trim ( ThisLine )
     .                        //     '\n   Last header   = '
     .                        //           trim ( This % Header ))
                  else
                     call WPErr ( MyName, 'Read error (iostat = '
     .                        //           trim ( ItoA ( stat )) 
     .                        //  ')\C\n   Last header   = '
     .                        //           trim ( This % Header ))
                  end if
               else
                  call WPErr ( MyName, 'Read error (iostat = '
     .                        //        trim ( ItoA ( stat ))
     .                        //     ') at the beginning of the file ' )
               end if
            end if
            stat = Read_Error
            go to 10
         end if

!        Exit loop if end of file has been reached
!        -----------------------------------------
         if ( eof ) exit RecordLoop
         Tail   = '\n   Line = ' // trim ( ThisRecord )

!        Exit with error status if entire record was not read
!        ----------------------------------------------------
         if ( .not. eor ) then
            call PErr  ( MyName, 'Did not read entire line \C'
     .                         // Tail )
            stat = Bad_Header
            go to 10
         end if

!        Search for comment character
!        ----------------------------
         LLine = scan ( ThisRecord ( : LRec ), Comment_Char ) - 1
         if ( LLine .lt. 0 ) then
            LLine = len_trim ( ThisRecord ( : LRec  ))
         else
            LLine = len_trim ( ThisRecord ( : LLine ))
         end if

!        Ignore record if there is no data
!        ---------------------------------
         if ( LLine .eq. 0 ) cycle RecordLoop

!        Otherwise process this line
!        ---------------------------
         ThisLine = ThisRecord ( : LLine )

!        ... as a header if this is the first line
!        -----------------------------------------
         iLine = iLine + 1
         if ( iLine .eq. 1 ) then

!           An error is assumed if line does not begin with a marker
!           --------------------------------------------------------
            if ( ThisLine ( : 1 ) .ne. Header_Marker ) then
               call WPErr    ( MyName,
     .                        'Assumed header record begins without '
     .                     // 'the ' // Header_Marker
     .                     //  trim ( Tail ))               
               stat = Bad_Header
               go to 10
            end if

!           Get header info
!           ---------------
            call Get_Header ( ThisLine, Header, stat )
            if ( stat .ne. No_Error ) then
               call WPErr ( MyName, ErrStat ( 'Get_Header', stat )
     .                       )
               go to 10
            end if
            NLevelsH = Header % NLevels

!           If input argument in not initialized
!           or if space is insufficient then ...
!           ------------------------------------
            NLevMax  = This   % NLevMax
            if ( NLevelsH .gt. NLevMax .or.
     .          .not. initialized ) then

!              ... clean up and deallocate ...
!              ---------------------------
               if ( initialized ) then
                  call Clean_Sounding ( This, stat )
                  if ( stat .ne. No_Error ) then
                     call WPErr ( MyName,
     .                            ErrStat ( 'Clean_Sounding', stat )
     .                         // trim ( Tail ))
                     go to 10
                  end if
                  NLevMax = NLevMax_Def + NLevelsH
               else
                  NLevMax = NLevMax_Def
               end if

!              ... and reinitialize with sufficient storage space
!              --------------------------------------------------
               call Init_Sounding  ( NLevMax, This, stat )
               if ( stat .ne. No_Error ) then
                  call WPErr    ( MyName,
     .                            ErrStat ( 'Init_Sounding', stat )
     .                         // trim ( Tail ))
                  go to 10
               end if
            end if

!           Copy header info to output argument
!           -----------------------------------
            call Copy_Header ( Header, This % HdrInfo )
            This % Header = ThisLine
         else

!           A data record is assumed, so check to ensure
!           that no header marker begin the record
!           --------------------------------------------
            if ( ThisLine ( : 1 ) .eq. Header_Marker ) then 
               call WPErr    ( MyName,
     .                        'Header marker encountered before the '
     .                   //   'records of all levels in the current '
     .                   //   'sounding were read '
     .                   // '\n   Last header = '
     .                   //    trim ( This % Header )
     .                   //    trim ( Tail ))
               stat = Missing_Record
               go to 10
            end if

!           Update the number of levels
!           ---------------------------
            NLevels = NLevels + 1
            This % DataR ( NLevels ) = ThisLine

         end if

!        Exit if data for all levels have been stored
!        --------------------------------------------  
         if ( NLevels .eq. NLevelsH ) exit RecordLoop

      end do RecordLoop

!     Clean up if error status
!     ------------------------ 
 10   continue
      if ( stat .ne. No_Error ) then
         call Clean_Sounding ( This )
      end if

      return
      end subroutine Get_Sounding

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Put_Sounding () --- Put (write) a IGRA sounding data for one report.
! 
! !DESCRIPTION:
!     This routine writes a IGRA sounding data for one report. 
!
! !INTERFACE:
      subroutine Put_Sounding ( lu, This, stat )

      use m_AdvError, only : ItoA, PErr, ErrStat, WPErr
!
! !INPUT PARAMETERS:
      implicit NONE
      integer,                intent (in)  ::
     .   lu                   ! Logical unit for input file
      type ( IGRA_Sounding ), intent (in)  :: 
     .   This                 ! Sounding data
!
! !OUTPUT PARAMETERS:
      integer,                intent (out) ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     19Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName   = MyModule // '::Put_Sounding'
      integer :: iLev, NLevels

!     Write header
!     ------------
      call WriteLn ( lu, This % Header, stat ) 
      if ( stat .ne. 0 ) then
         call PErr ( MyName, 'Write error (iostat = '
     .                  //    trim ( ItoA ( stat )) // ') \C'
     .                  // '\n   Header record = '
     .                  //    trim ( This % Header ))
         stat = Write_Error
         return 
      end if

!     ... and data portion of sounding to output file
!     -----------------------------------------------
      NLevels = This % HdrInfo % NLevels
      do iLev = 1, NLevels
         call WriteLn ( lu, This % DataR ( iLev ), stat ) 
         if ( stat .ne. 0 ) then
            call PErr ( MyName, 'Write error (iostat = '
     .                     //    trim ( ItoA ( stat )) // ') \C'
     .                     // '\n   Line          = '
     .                     //    trim ( This % DataR ( iLev ))
     .                     // '\n   Header record = '
     .                     //    trim ( This % Header ))
            stat = Write_Error
            return 
         end if
      end do

      return
      end subroutine Put_Sounding
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_Profile () --- Get the IGRA profile data for one report.
! 
! !DESCRIPTION:
!     This routine retrieves IGRA profile data for one report from the input
!     argument of type IGRA_Sounding. 
!
! !INTERFACE:
      subroutine Get_Profile ( StnMeta, SData, PData, stat )

      use m_AdvError,     only : PErr, ErrStat, WPErr
!
! !INPUT PARAMETERS:
      implicit NONE
      type ( IGRA_Station  ),   intent (in)    ::
     .   StnMeta
      type ( IGRA_Sounding ),   intent (in)    :: 
     .   SData                ! IGRA sounding data
!
! !OUTPUT PARAMETERS:
      type ( IGRA_Profile ),  intent (inout) :: 
     .   PData                ! IGRA profile data
      integer,                  intent (out) ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!..............................................................
      character (len=*), parameter ::
     .   MyName   = MyModule // '::Get_Profile'
      logical :: initialized
      integer :: NLevels, NLevMax, iLev, NPLevels
      character (len=500):: Tail

      Tail = '\C\n   Header = ' // trim ( SData % Header )

!     Initialize or reinitialize if more space is required
!     ----------------------------------------------------
      NLevels = SData % HdrInfo % NLevels
      NLevMax = PData % Obs     % NLevMax
      if ( NLevels .gt. NLevMax ) then
         initialized = NLevMax .gt. 0
         if ( initialized ) then
            call Clean_Profile ( PData, stat )
            if ( stat .ne. No_Error ) then
               call WPErr ( MyName, ErrStat ( 'Clean_Profile', stat )
     .                           // trim ( Tail ))
               return
            end if
            NLevMax = NLevMax_Def + NLevels
         else
            NLevMax = NLevMax_Def
         end if

         call Init_Profile ( NLevMax, PData, stat )
         if ( stat .ne. No_Error ) then
            call WPErr ( MyName, ErrStat ( 'Init_Profile', stat )
     .                        // trim ( Tail ))
            return
         end if
      else

!        Reset counters in data structure
!        --------------------------------
         call Reset_Profile ( PData )

      end if

!     Copy header info to output arg
!     ------------------------------
      call Copy_Header ( SData % HdrInfo, PData % Header )

!     Extract data for each level
!     ---------------------------
      do iLev =  1, NLevels
         call Add_Level ( iLev, SData % DataR ( iLev ), PData, stat )
         if ( stat .ne. No_Error ) then
            call WPErr ( MyName, ErrStat ( 'Add_Level', stat )
     .                   // '\C\n   Record = '
     .                   //         trim ( SData % DataR ( iLev ))
     .                   //         trim ( Tail ))
            return
         end if
      end do

!     ... and prepare the data for external use
!     -----------------------------------------
      call Prep_Profile ( StnMeta, PData, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'Prep_Profile', stat )
     .                     // trim ( Tail ))
         return
      end if

! get radcor profile?

      return
      end subroutine Get_Profile
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Put_Profile () --- Put the IGRA profile data for one report.
! 
! !DESCRIPTION:
!     This routine put IGRA profile data for one report to the
!     output argument of the IGRA_Sounding.
!
! !INTERFACE:
      subroutine Put_Profile ( PData, SData, stat )

      use m_AdvError,     only : PErr, ErrStat, WPErr
!
! !INPUT/OUTPUT PARAMETERS:
      implicit NONE
      type ( IGRA_Profile ),    intent (inout) :: 
     .   PData                ! IGRA profile data
      type ( IGRA_Sounding ),   intent (inout) :: 
     .   SData                ! IGRA sounding data
!
! !OUTPUT PARAMETERS:
      integer,                  intent (out)   ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!..............................................................
      character (len=*), parameter ::
     .   MyName   = MyModule // '::Put_Profile'
      integer :: NLevels, NLevMax, iLevel, iSLevel
      character (len=500):: Tail

      Tail = '\C\n   Header = ' // trim ( SData % Header )

!     Extract data for each level
!     ---------------------------
      NLevels = PData % Obs % NLevels
      do iLevel =  1, NLevels
         iSLevel = PData % Obs % LevList ( iLevel )
         call Flush_Level ( iLevel, PData,
     .                              SData % DataR ( iSLevel ), stat )
         if ( stat .ne. No_Error ) then
            call WPErr ( MyName, ErrStat ( 'Flush_Level', stat )
     .                   // '\C\n   Record = '
     .                   //         trim ( SData % DataR ( iSLevel ))
     .                   //         trim ( Tail ))
            return
         end if
      end do

      return
      end subroutine Put_Profile
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_RadVect () --- Get the IGRA data for the output argument of type, Radcor_profiles.
! 
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Get_RadVect ( PData, Radcor, stat )

      use m_AdvError,     only : PErr, ErrStat, WPErr
      use m_RadData,      only : radcor_profiles, QC_OK,
     .                           Height, UWind, VWind, NPresDigits,
     .                           Rad_Init, Rad_Clean
      use m_convert,      only : Dew_Point, Air_Temperature
      use m_RadSort,      only : R2ManExp
!
! !INPUT PARAMETERS:
      implicit NONE
      type ( IGRA_Profile ),    intent (in)    :: 
     .   PData                ! Radcor data
!
! !OUTPUT PARAMETERS:
      type ( radcor_profiles ), intent (inout) :: 
     .   Radcor               ! IGRA sounding data
      integer,                  intent (out) ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!..............................................................
      character (len=*), parameter ::
     .   MyName   = MyModule // '::Get_RadVect'
      logical :: initialized
      integer :: NLevels, NLevMax, iLev, iiLev, iVar, QC
      integer :: NObs,    NObsMax, iOb
      integer :: kt
      real :: Ob, StnElev
      real,    dimension (:), pointer ::
     .   P, H, U, V, ETime, DLat, DLon, Obs
      integer, dimension (:), pointer ::
     .   P_m, P_e, QCs

!     Default status
!     --------------
      stat    = No_Error

!     Initialize or reinitialize if more space is required
!     ----------------------------------------------------
      NLevels = PData  % Obs    % NLevels
      NObs    = NLevels * NVar_IGRA
      NObsMax = Radcor % Vector % NVct
      if ( NObs .gt. NObsMax ) then
         initialized = NObsMax .gt. 0
         if ( initialized ) then
            call Rad_Clean ( Radcor % Vector, stat )
            if ( stat .ne. No_Error ) then
               call WPErr ( MyName, ErrStat ( 'Rad_Clean', stat ))
               return
            end if
            NLevMax = NLevMax_Def + NLevels
         else
            NLevMax = NLevMax_Def
         end if

         NObsMax = NLevMax * NVar_IGRA
         call Rad_Init ( NObsMax, Radcor % Vector, stat )
         if ( stat .ne. No_Error ) then
            call WPErr ( MyName, ErrStat ( 'Rad_Init', stat ))
            return
         end if
      end if

!     Set aliases for pointers
!     ------------------------
      DLat   => PData % Obs % DLat  ( : NLevels )
      DLon   => PData % Obs % DLon  ( : NLevels )
      ETime  => PData % Obs % ETime ( : NLevels )
      P      => PData % Obs % P     ( : NLevels )
      H      => PData % Obs % H     ( : NLevels )

!     For each variable ...
!     ---------------------
      Radcor % Vector % NObs = NObs
      do iVar = 1, NVar_IGRA

!        ... selected the arrays and kt appropriate for the variable
!        -----------------------------------------------------------
         if      ( iVar .eq. 1 ) then
            Obs => PData % Obs % Height ( : NLevels )
            QCs => PData % QC  % Height ( : NLevels )
            kt  =  Height
         else if ( iVar .eq. 2 ) then
            Obs => PData % Obs % Temp   ( : NLevels )
            QCs => PData % QC  % Temp   ( : NLevels )
            kt  =  Air_Temperature
         else if ( iVar .eq. 3 ) then
            Obs => PData % Obs % DewPt  ( : NLevels )
            QCs => PData % QC  % DewPt  ( : NLevels )
            kt  =  Dew_Point
         else if ( iVar .eq. 4 ) then
            Obs => PData % Obs % UWind  ( : NLevels )
            QCs => PData % QC  %  Wind  ( : NLevels )
            kt  =  UWind
         else if ( iVar .eq. 5 ) then
            Obs => PData % Obs % VWind  ( : NLevels )
            QCs => PData % QC  %  Wind  ( : NLevels )
            kt  =  VWind
         end if

!        ... and put the data into radcor arrays
!        ---------------------------------------
         iOb     = iVar
         do iLev = 1, NLevels
            Ob = Obs ( iLev )
            QC = QCs ( iLev )
            Radcor % Vector % DLat      ( iOb ) =  DLat  ( iLev )
            Radcor % Vector % DLon      ( iOb ) =  DLon  ( iLev )
            Radcor % Vector % P         ( iOb ) =  P     ( iLev )
            Radcor % Vector % ETime     ( iOb ) =  ETime ( iLev )
            Radcor % Vector % QC_file   ( iOb ) =  QC
            Radcor % Vector % Obs       ( iOb ) =  Ob
            Radcor % Vector % FG        ( iOb ) =  Ob
            Radcor % Vector % P_Missing ( iOb ) = .false.
            Radcor % Vector % ObMissing ( iOb ) =  QC .ne. QC_OK
            Radcor % Vector % FGMissing ( iOb ) = .true.
            Radcor % Vector % kt        ( iOb ) =  kt
            Radcor % Vector % QC        ( iOb ) =  QC
            iOb = iOb + NVar_IGRA
         end do
      end do

!     Express pressure an integer mantessa and exponent
!     ------------------------------------------------- 
      P   => Radcor % Vector % P   ( : NObs )
      P_m => Radcor % Vector % P_m ( : NObs )
      P_e => Radcor % Vector % P_e ( : NObs )
      call R2ManExp ( NPresDigits, P, P_m, P_e )

!     Save navigator info in the meta data section
!     --------------------------------------------
      Radcor % Meta % ksLoc ( 1 ) = 1
      Radcor % Meta % ksLen ( 1 ) = NObs
     
      return
      end subroutine Get_RadVect
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Put_RadVect () --- Put the radcor data for the output argument of type, IGRA_Profile.
! 
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Put_RadVect ( RadCor, PData, stat )

      use m_AdvError,     only : PErr, ErrStat, WPErr
      use m_RadData,      only : radcor_profiles, QC_Rejected
      use m_soundings,    only : Wind_UVtoSD
!
! !INPUT PARAMETERS:
      implicit NONE
      type ( radcor_profiles ), intent (in)    :: 
     .   Radcor               ! Radcor data
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Profile ),    intent (inout) :: 
     .   PData                ! IGRA profile data
!
! !OUTPUT PARAMETERS:
      integer,                  intent (out)   ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!..............................................................

      character (len=*), parameter ::
     .   MyName   = MyModule // '::Put_RadVect'
      logical :: initialized
      integer :: NLevels, NLevMax, iLev, iVar, Rad_QC, IGRA_QC
      integer :: NObs,    NObsMax, iOb
      real :: Ob, StnElev
      logical, dimension (:), pointer ::
     .   Mask
      real,    dimension (:), pointer ::
     .   P, H, U, V, WDir, WSpeed, ETime, DLat, DLon, Obs
      integer, dimension (:), pointer ::
     .   QCs

!     Default status
!     --------------
      stat    = No_Error

      NLevels = PData  % Obs    % NLevels
      NObs    = NLevels * NVar_IGRA

!     Set aliases for pointers
!     ------------------------
      DLat   => Radcor % Vector % DLat  ( : NObs )
      DLon   => Radcor % Vector % DLon  ( : NObs )
      ETime  => Radcor % Vector % ETime ( : NObs )
      P      => Radcor % Vector % P     ( : NObs )

!     For each variable ...
!     ---------------------
      do iVar = 1, NVar_IGRA

!        ... select the arrays appropriate for the variable
!        --------------------------------------------------
         if      ( iVar .eq. 1 ) then
            Obs => PData % Obs % Height ( : NLevels )
            QCs => PData % QC  % Height ( : NLevels )
         else if ( iVar .eq. 2 ) then
            Obs => PData % Obs % Temp   ( : NLevels )
            QCs => PData % QC  % Temp   ( : NLevels )
         else if ( iVar .eq. 3 ) then
            Obs => PData % Obs % DewPt  ( : NLevels )
            QCs => PData % QC  % DewPt  ( : NLevels )
         else if ( iVar .eq. 4 ) then
            Obs => PData % Obs % UWind  ( : NLevels )
            QCs => PData % QC  %  Wind  ( : NLevels )
         else if ( iVar .eq. 5 ) then
            Obs => PData % Obs % VWind  ( : NLevels )
            QCs => PData % QC  %  Wind  ( : NLevels )
         end if

!        ... then copy the data accordingly
!        ----------------------------------
         iOb     = iVar
         do iLev = 1, NLevels
            IGRA_QC = QCs ( iLev )
            if ( IGRA_QC .ne. QC_Rejected ) then
               Obs ( iLev ) = Radcor % Vector % Obs      ( iOb )
               QCs ( iLev ) = Radcor % Vector % QC       ( iOb )
            end if
            if ( iVar .eq. 1 ) then
               PData    % Obs % DLat  ( iLev ) = DLat    ( iOb )
               PData    % Obs % DLon  ( iLev ) = DLon    ( iOb )
               PData    % Obs % ETime ( iLev ) = ETime   ( iOb )
               if ( PData % QC % Pres ( iLev ) .ne. QC_Rejected )
     .            PData % Obs % Pres  ( iLev ) = P       ( iOb )
               PData    % Obs % P     ( iLev ) = P       ( iOb )
               PData    % Obs % logP  ( iLev ) = log ( P ( iOb ))
               PData    % Obs % H     ( iLev ) = Obs     ( iOb )
            end if
            iOb = iOb + NVar_IGRA
         end do
      end do

!     Convert winds to speed and direction
!     ------------------------------------
      Mask   => PData % Obs % Mask   ( : NLevels )
      U      => PData % Obs % UWind  ( : NLevels )
      V      => PData % Obs % VWind  ( : NLevels )
      WDir   => PData % Obs % WDir   ( : NLevels )
      WSpeed => PData % Obs % WSpeed ( : NLevels )
      do iLev = 1, NLevels
         Mask ( iLev ) = PData % QC % Wind ( iLev ) .ne. QC_Rejected
      end do
      call Wind_UVtoSD ( U, V, Mask, WSpeed, WDir )

      return
      end subroutine Put_RadVect
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Prep_Profile () --- Prepare the IGRA profile data
! 
! !DESCRIPTION:
!     This routine prepares IGRA profile data for one report
!
! !INTERFACE:
      subroutine Prep_Profile ( StnMeta, PData, stat )

      use m_AdvError,  only : PErr, ErrStat, WPErr
      use m_stdatm,    only : StdAtmZ, StdZ2P
      use m_soundings, only : Z2Y, logP2Y, Wind_SDtoUV, Z2Time, LLPath
      use m_RadData,   only : QC_OK
!
! !INPUT PARAMETERS:
      implicit NONE
      type ( IGRA_Station ),    intent (in)    ::
     .   StnMeta
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Profile ),    intent (inout) :: 
     .   PData                ! IGRA profile data
!
! !OUTPUT PARAMETERS:
      integer,                  intent (out)   ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     25Mar2007  C. Redder  Origional code.
! EOP
!..............................................................
      character (len=*), parameter ::
     .   MyName   = MyModule // '::Prep_Profile'
      integer :: NLevels, NSLevels, NWLevels, iLev, iSLev, iWLev, iLLev
      integer :: QC
      real    :: StnLat, StnLon, StnElev
      real,      dimension (:), pointer ::
     .   SPres,  SHeight, SlogP, WPres, WHeight, WlogP, P, H, U, V,
     .           WSpeed, WDir, logP, DLat, DLon, ETime
      integer,   dimension (:), pointer ::
     .   SIPres, WIPres, SList, WList, Indx, Wind_QC
      logical,   dimension (:), pointer ::
     .   Mask
      real    :: Ext_logP (2), logP_Off 
      logical :: choose_sigT

      stat = 0.0

!     Get and compute data for the components SigT
!     --------------------------------------------
      call Get_SigT ( PData % Obs, PData % QC, PData % SigT, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'Get_SigT', stat ))
         return
      end if

!     ... and Wind
!     ------------
      call Get_Wind ( PData % Obs, PData % QC, PData % Wind, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'Get_Wind', stat ))
         return
      end if

      NSLevels =  PData % SigT % NLevels
      SPres    => PData % SigT % Pres    ( : NSLevels )
      SHeight  => PData % SigT % Height  ( : NSLevels )
      SlogP    => PData % SigT % logP    ( : NSLevels )
      SIPres   => PData % SigT % IPres   ( : NSLevels )
      SList    => PData % SigT % LevList ( : NSLevels )

      NWLevels =  PData % Wind % NLevels
      WPres    => PData % Wind % Pres    ( : NWLevels )
      WHeight  => PData % Wind % Height  ( : NWLevels )
      WlogP    => PData % Wind % logP    ( : NWLevels )
      WIPres   => PData % Wind % IPres   ( : NWLevels )
      WList    => PData % Wind % LevList ( : NWLevels )

      NLevels  =  PData % Obs  % Nlevels
      Indx     => PData % Obs  % Indx    ( :  NLevels )
      P        => PData % Obs  % P       ( :  NLevels )
      logP     => PData % Obs  % logP    ( :  NLevels )
      H        => PData % Obs  % H       ( :  NLevels )
      Mask     => PData % Obs  % Mask    ( :  NLevels )
      U        => PData % Obs  % UWind   ( :  NLevels )
      V        => PData % Obs  % VWind   ( :  NLevels )
      Wind_QC  => PData % QC   %  Wind   ( :  NLevels )
      WSpeed   => PData % Obs  % WSpeed  ( :  NLevels )
      WDir     => PData % Obs  % WDir    ( :  NLevels )
      ETime    => PData % Obs  % ETime   ( :  NLevels )
      DLat     => PData % Obs  % DLat    ( :  NLevels )
      DLon     => PData % Obs  % DLon    ( :  NLevels )

!     Use standard atmospheric for extrapolation
!     ------------------------------------------
      call StdAtmZ ( WHeight, P = WPres )
      do iLev = 1, NWLevels
         WlogP ( iLev ) = log ( WPres ( iLev )) 
      end do
!
!     and use mass pressure values for interpolation at wind height levels
!     --------------------------------------------------------------------
      call Z2Y ( SHeight, SlogP, WHeight, WlogP, YExt = (WlogP))

!     Correction due to problem in extrapolation in Z2Y
!     when the ranges of the two sets do not overlap
!     -------------------------------------------------
      Ext_logP = 0.0
      if ( NSLevels .gt. 0 .and.
     .     NWLevels .gt. 0 ) then
         if      ( WHeight ( 1 ) .gt. SHeight ( NSLevels )) then
            Ext_logP ( 1 ) = log ( StdZ2P ( WHeight ( 1 )))
            Ext_logP ( 2 ) = log ( StdZ2P ( SHeight ( NSLevels )))
         else if ( SHeight ( 1 ) .gt. WHeight ( NWLevels )) then
            Ext_logP ( 1 ) = log ( StdZ2P ( WHeight ( NWLevels )))
            Ext_logP ( 2 ) = log ( StdZ2P ( SHeight ( 1 )))
         end if
      end if
      logP_Off = Ext_logP ( 1 ) - Ext_logP ( 2 )

!     Keep only the three most significant digits
!     -------------------------------------------
      do iLev = 1, NWLevels
         WlogP ( iLev ) = WlogP ( iLev ) + logP_Off
         WPres ( iLev ) = exp ( WlogP ( iLev ))
         if      ( WPres ( iLev ) .ge. 99.5 ) then
            WPres ( iLev ) = nint ( WPres ( iLev ))
         else if ( WPres ( iLev ) .ge. 9.95 ) then
            WPres ( iLev ) = real ( nint ( WPres ( iLev ) *  10.0 ))
     .                     /  10.0
         else
            WPres ( iLev ) = real ( nint ( WPres ( iLev ) * 100.0 ))
     .                     / 100.0
         endif
         WIPres ( iLev ) = nint ( WPres ( iLev ) * 100.0 )
      end do

!     Merge data from sigG and wind reports into the Obs profile
!     ----------------------------------------------------------
      iSLev   = 0
      iWLev   = 0
      NLevels = NSLevels + NWLevels
      do iLev = 1, NLevels
         if ( iSLev .lt. NSLevels .and.
     .        iWLev .lt. NWLevels ) then
            choose_SigT = SIPres ( iSLev + 1 ) .ge.
     .                    WIPres ( iWLev + 1 ) 
         else
            choose_SigT = iSLev .lt. NSLevels
         end if
         if ( choose_SigT ) then
            iSLev           = iSLev + 1
            iLLev           = SList   ( iSLev )
            Indx  (  iLev ) = iLLev
            SList ( iSLev ) =  iLev
            P     ( iLLev ) = SPres   ( iSLev ) 
            logP  ( iLLev ) = SlogP   ( iSLev )
            H     ( iLLev ) = SHeight ( iSLev )
         else
            iWLev           = iWLev + 1
            iLLev           = WList   ( iWLev )
            Indx  (  iLev ) = iLLev
            WList ( iWLev ) =  iLev
            P     ( iLLev ) = WPres   ( iWLev )
            logP  ( iLLev ) = WlogP   ( iWLev )
            H     ( iLLev ) = WHeight ( iWLev )
         end if
      end do
      call Reorder_Obs2 ( PData % Obs, Indx )
      call Reorder_QC2  ( PData % QC,  Indx )
      call Reorder_IO2  ( PData % IO,  Indx )

!     Convert winds to U and V
!     ------------------------
      do iLev = 1, NLevels
         Mask ( iLev ) = Wind_QC ( iLev ) .eq. QC_OK
      end do
      call Wind_SDtoUV ( WSpeed, WDir, Mask, U, V )

!     Interpolate u and v-winds to fill in missing values
!     ---------------------------------------------------
      call logP2Y ( logP, Mask, (U), logP, U, C = 0.0, logP = .true. )
      call logP2Y ( logP, Mask, (V), logP, V, C = 0.0, logP = .true. )
      do iLev = 1, NLevels
         if ( Mask ( iLev )) exit
         U ( iLev ) = 0.0
         V ( iLev ) = 0.0
      end do
      do iLev = NLevels, 1, -1
         if ( Mask ( iLev )) exit
         U ( iLev ) = 0.0
         V ( iLev ) = 0.0
      end do

!     Compute the elapsed time (assuming a constant rise rate of 5 m/s)
!     -----------------------------------------------------------------
      StnLat   =  StnMeta % Lat
      StnLon   =  StnMeta % Lon
      StnElev  =  StnMeta % Elev
      ETime    => PData % Obs % ETime ( : NLevels )
      ETime    =  5.0 
      call Z2Time ( H, StnElev, ( ETime ), ETime )

!     ... and the drift lat/lons
!     --------------------------
      call LLPath ( StnLat, StnLon, ETime, U, V, DLat, DLon )

      return
      end subroutine Prep_Profile
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_SigT () --- Get the derived IGRA SigT data from input argument of type, IGRA_Obs.
! 
! !DESCRIPTION:
!     This routine computes the IGRA_sigT data from the input argument of
!     type IGRA_Obs.
!
! !INTERFACE:
      subroutine Get_SigT ( Obs, QC, SigT, stat )

      use m_AdvError,    only : PErr, ErrStat, WPErr
      use m_RadData,     only : QC_OK
      use m_RadSort,     only : IndexSet, IndexSort
      use m_soundings,   only : logP2Y, CalcZ
      use m_stdatm,      only : StdAtmP, StdAtmZ
      use m_convert,     only : toHVar, Dew_Point, Relative_Humidity
      use m_humidity,    only : RHTtoVP, VPtoMR, CheckMR
      use m_temperature, only : ATtoVT
!
! !INPUT PARAMETERS:
      implicit NONE
      type ( IGRA_Obs ),      intent (in)    :: 
     .   Obs                  ! IGRA obs data
      type ( IGRA_QC  ),      intent (in)    :: 
     .   QC                   ! IGRA qc data
!
! !OUTPUT PARAMETERS:
      type ( IGRA_SigT ),     intent (inout) :: 
     .   SigT                 ! IGRA significant thermodynamic data
      integer,                intent   (out) ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!..............................................................

      character (len=*), parameter ::
     .   MyName   = MyModule // '::Get_SigT'
      real,    parameter ::
     .   RH_Default = 50.0

      logical :: sorted
      integer :: iLev, NLevels, NSLevels, iSLev
      integer :: LastPres, ThisPres, kt_in, kt_out
      real,    dimension (:), pointer :: logP, Height, Temp, DewPt,
     .                                   T, Pres, MR, RH, VT, H, P
      integer, dimension (:), pointer :: Indx, IPres
      logical, dimension (:), pointer :: Mask
      character (len=1) :: LevType

      stat = No_Error

!     Extract the data from the input argument of type, IGRA_Obs
!     ----------------------------------------------------------
      sorted   = .true.
      NSLevels =  0
      NLevels  =  Obs % NLevels
      do iLev = 1, NLevels
         LevType = Obs % MajorLType ( iLev )
         if ( LevType .eq. StanLevel .or.
     .        LevType .eq. SigTLevel ) then
            NSLevels = NSLevels + 1
            ThisPres = nint ( Obs % Pres ( iLev ) * 100.0 )
            SigT % Mask    ( NSLevels ) = .true.
            SigT % LevList ( NSLevels ) =  iLev
            SigT % IPres   ( NSLevels ) =  ThisPres
            SigT % Pres    ( NSLevels ) =  Obs  % Pres   ( iLev )
            SigT % Height  ( NSLevels ) =  Obs  % Height ( iLev )
            SigT % Temp    ( NSLevels ) =  Obs  % Temp   ( iLev )
            SigT % DewPt   ( NSLevels ) =  Obs  % DewPt  ( iLev )
            SigT % logP    ( NSLevels ) =  log (  Obs % Pres ( iLev ))
            SigT % H       ( NSLevels ) =  SigT % Height ( iLev )
            SigT % T       ( NSLevels ) =  SigT % Temp   ( iLev )
            SigT % VT      ( NSLevels ) =  SigT % Temp   ( iLev )
            SigT % DP      ( NSLevels ) =  SigT % DewPt  ( iLev )
            SigT % RH      ( NSLevels ) =  SigT % DewPt  ( iLev )
            SigT % MR      ( NSLevels ) =  SigT % DewPt  ( iLev )
            if ( sorted .and. NSLevels .gt. 1 )
     .           sorted = ThisPres .le. LastPres
            LastPres = ThisPres
         end if
      end do
      SigT % NLevels = NSLevels

!     If necessary, sort the data according to decreasing pressure
!     ------------------------------------------------------------ 
      call IndexSet ( SigT % Indx ( :NSLevels ))
      Indx  => SigT % Indx  ( : NSLevels )
      IPres => SigT % IPres ( : NSLevels )
      if ( .not. sorted ) then
         call IndexSort ( Indx ( : NSLevels ), IPres, stat,
     .                    descend = .true. )
         if ( stat .ne. No_Error ) then
            call WPErr ( MyName, ErrStat ( 'IndxSort', stat ))
            stat = Alloc_Error
            return
         end if
         call Reorder_SigT2 ( SigT, Indx )
      end if

!     Get standard atmospheric data for extrapolation
!     -----------------------------------------------
      P => SigT % Pres ( : NSLevels )
      call StdAtmP ( P, Z = SigT % H ( : NSLevels ),
     .                  T = SigT % T ( : NSLevels ))

!     To fill in missing values, interpolate temperature
!     --------------------------------------------------
      do iSLev = 1, NSLevels
         iLev  = SigT % LevList ( iSLev )
         SigT % Mask ( iSLev ) = QC % Temp ( iLev ) .eq. QC_OK
      end do
      Mask => SigT % Mask ( : NSLevels )
      logP => SigT % logP ( : NSLevels )
      Temp => SigT % Temp ( : NSLevels )
      T    => SigT % T    ( : NSLevels )
      call logP2Y  ( logP, Mask, Temp, logP, T,
     .               logP = .true.,
     .               YExt = (T))

!     ... and humidity data
!     ---------------------
      RH    => SigT % RH    ( : NSLevels )
      DewPt => SigT % DewPt ( : NSLevels )
      do iSLev = 1, NSLevels
         iLev  = SigT % LevList ( iSLev )
         SigT % Mask ( iSLev ) = QC % DewPt ( iLev ) .eq. QC_OK
         SigT % RH   ( iSLev ) = DewPt ( iSLev )
      end do
      kt_in  = Dew_Point
      kt_out = Relative_Humidity
      call toHVar ( P, T, Mask, kt_in, kt_out, RH )
      call logP2Y ( logP, Mask, (RH), logP, RH,
     .              C = RH_Default, logP = .true. )

!     For all levels, compute the mixing ratio
!     ----------------------------------------
      MR => SigT % MR ( : NSLevels )
      call RHTtoVP (  RH,  T, P, MR )
      call  VPtoMR ( (MR),    P, MR ) 
      call CheckMR (       T, P, MR )

!     ... virtual temperature
!     -----------------------
      VT => SigT % VT ( : NSLevels )
      call ATtoVT ( T, MR, VT )

!     ... geopotential heights
!     ------------------------
      H      => SigT % H      ( : NSLevels )
      Height => SigT % Height ( : NSLevels )
      call CalcZ ( logP, VT, H, logP = .true. )
      do iSLev = 1, NSLevels
         iLev  = SigT % LevList ( iSLev )
         SigT % Mask ( iSLev ) = QC % Height ( iLev ) .eq. QC_OK
      end do
      call logP2Y ( logP, Mask, Height, logP, H,
     .              YExt = (H), logP = .true. )

      return
      end subroutine Get_SigT

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Put_SigT () --- Put the IGRA_SigT data to output argument of type IGRA_Obs.
! 
! !DESCRIPTION:
!     This routine put the IGRA_SigT data to the output argument
!     of type IGRA_Obs
!
! !INTERFACE:
      subroutine Put_SigT ( SigT, QC, Obs, stat )

      use m_AdvError,    only : PErr, ErrStat, WPErr
      use m_RadData,     only : QC_OK
!
! !INPUT PARAMETERS:
      implicit NONE
      type ( IGRA_SigT ),     intent (in)    :: 
     .   SigT                 !  IGRA significant thermodynamic data
      type ( IGRA_QC   ),     intent (in)    :: 
     .   QC                   ! IGRA QC data
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Obs  ),     intent (inout) :: 
     .   Obs                  ! IGRA obs profile data
!
! !OUTPUT PARAMETERS:
      integer,                intent   (out) ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!..............................................................

      character (len=*), parameter ::
     .   MyName   = MyModule // '::Put_SigT'

      integer :: iLev, NSLevels, iSLev

      stat = No_Error

      NSLevels =  SigT % NLevels
      do iSLev = 1, NSLevels
         iLev  = SigT % LevList ( iSLev )
         Obs  % Pres   ( iLev ) = SigT % Pres    ( iSLev )
         Obs  % Height ( iLev ) = SigT % Height  ( iSLev )
         Obs  % Temp   ( iLev ) = SigT % Temp    ( iSLev )
         Obs  % DewPt  ( iLev ) = SigT % DewPt   ( iSLev )
      end do

      return
      end subroutine Put_SigT

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_Wind () --- Get the derived IGRA_Wind data from the input argument of type IGRA_Obs.
! 
! !DESCRIPTION:
!     This routine computes the IGRA_SigT data from the input argument 
!     of type, IGRA_Wind.
!
! !INTERFACE:
      subroutine Get_Wind ( Obs, QC, Wind, stat )

      use m_AdvError,    only : PErr, ErrStat, WPErr
      use m_RadData,     only : QC_OK
      use m_RadSort,     only : IndexSet, IndexSort
!
! !INPUT PARAMETERS:
      implicit NONE
      type ( IGRA_Obs ),      intent (in)    :: 
     .   Obs                  ! IGRA profile obs data
      type ( IGRA_QC  ),      intent (in)    :: 
     .   QC                   ! IGRA QC data
!
! !OUTPUT PARAMETERS:
      type ( IGRA_Wind ),     intent (inout) :: 
     .   Wind                 ! IGRA wind data
      integer,                intent   (out) ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!..............................................................

      character (len=*), parameter ::
     .   MyName   = MyModule // '::Get_Wind'
      real,    parameter ::
     .   RH_Default = 50.0

      logical :: sorted
      integer :: iLev, NLevels, NWLevels, iWLev
      integer :: LastHght, ThisHght, kt_in, kt_out
      real,    dimension (:), pointer :: Height, Pres
      integer, dimension (:), pointer :: Indx, IHeight
      logical, dimension (:), pointer :: Mask
      character (len=1) :: LevType

      stat = No_Error

!     Extract the data from the input argument of type, IGRA_Obs
!     ----------------------------------------------------------
      sorted   = .true.
      NWLevels =  0
      NLevels  =  Obs % NLevels
      do iLev = 1, NLevels
         LevType = Obs % MajorLType ( iLev )
         if ( LevType .eq. WindLevel ) then
            NWLevels = NWLevels + 1
            ThisHght = nint ( Obs % Height ( iLev ))
            Wind % Mask    ( NWLevels ) = .true.
            Wind % LevList ( NWLevels ) =  iLev
            Wind % IHeight ( NWLevels ) =  ThisHght
            Wind % Pres    ( NWLevels ) =  Obs  % Pres   ( iLev )
            Wind % Height  ( NWLevels ) =  Obs  % Height ( iLev )
            if ( sorted .and. NWLevels .gt. 1 )
     .           sorted = ThisHght .ge. LastHght
            LastHght = ThisHght
         end if
      end do
      Wind % NLevels = NWLevels

!     If necessary, sort the data according to decreasing pressure
!     ------------------------------------------------------------ 
      call IndexSet ( Wind % Indx ( :NWLevels ))
      Indx    => Wind % Indx    ( : NWLevels )
      IHeight => Wind % IHeight ( : NWLevels )
      if ( .not. sorted ) then
         call IndexSort ( Indx ( : NWLevels ), IHeight, stat,
     .                    descend = .false. )
         if ( stat .ne. No_Error ) then
            call WPErr ( MyName, ErrStat ( 'IndxSort', stat ))
            stat = Alloc_Error
            return
         end if
         call Reorder_Wind2 ( Wind, Indx )
      end if

      return
      end subroutine Get_Wind
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Put_Wind () --- Put the IGRA_Wind data to output argument of type IGRA_Obs.
! 
! !DESCRIPTION:
!     This routine put the IGRA_Wind data to the output argument
!     of type IGRA_Obs
!
! !INTERFACE:
      subroutine Put_Wind ( Wind, QC, Obs, stat )

      use m_AdvError,    only : PErr, ErrStat, WPErr
      use m_RadData,     only : QC_OK
!
! !INPUT PARAMETERS:
      implicit NONE
      type ( IGRA_Wind ),     intent (in)    :: 
     .   Wind                 !  IGRA wind data
      type ( IGRA_QC   ),     intent (in)    :: 
     .   QC                   ! IGRA QC data
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Obs  ),     intent (inout) :: 
     .   Obs                  ! IGRA obs profile data
!
! !OUTPUT PARAMETERS:
      integer,                intent   (out) ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!..............................................................

      character (len=*), parameter ::
     .   MyName   = MyModule // '::Put_Wind'

      integer :: iLev, NWLevels, iWLev

      stat = No_Error

      NWLevels =  Wind % NLevels
      do iWLev = 1, NWLevels
         iLev  = Wind % LevList ( iWLev )
         Obs % Height ( iLev ) = Wind % Height  ( iWLev )
      end do

      return
      end subroutine Put_Wind

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Reset_Profile () --- Reset counters in the input argument of type, IGRA_Profile
! 
! !DESCRIPTION:
!     This routine resets the counters in the input argument of type, IGRA_Profile
!
! !INTERFACE:
      subroutine Reset_Profile ( This )
!
! !INPUT PARAMETERS:
      implicit NONE
!                             !   adding the level.  Default: reset = .false.
! !OUTPUT PARAMETERS:
      type ( IGRA_Profile ),  intent (inout) :: 
     .   This                 ! Profile data
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------
     
      This % Obs  % NLevels = 0
      This % QC   % NLevels = 0
      This % IO   % NLevels = 0
      This % sigT % NLevels = 0
      This % Wind % NLevels = 0

      return
      end subroutine Reset_Profile
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Add_Level () --- Add level info from text line
! 
! !DESCRIPTION:
!     This routine adds level info from text line
!
! !INTERFACE:
      subroutine Add_Level ( iLevel, TextLine, This, stat )
      use m_AdvError, only : ErrStat, WPErr
!
! !INPUT PARAMETERS:
      implicit NONE
      integer,                intent (in)    ::
     .   iLevel               ! Level index number 
      character (len=*),      intent (in)    ::
     .   TextLine             ! Text line containing the info
!
! !OUTPUT PARAMETERS:
      type ( IGRA_Profile ),  intent (inout) :: 
     .   This                 ! Profile data
      integer,                intent (out)   ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName = MyModule // '::Add_Level'
      type ( IGRA_Level ) :: LvlInfo
      integer :: NLevels, NLevMax

      call Get_Level ( TextLine, LvlInfo, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'Get_Level', stat ))
         return
      end if

      NLevMax = This % Obs    % NLevMax
      if ( LvlInfo % accept_level ) then
         NLevels = This % Obs % NLevels + 1
         if ( NLevels .le. NLevMax ) then
            This % Obs % LevList    ( NLevels ) = iLevel
            This % Obs % Pres       ( NLevels ) = LvlInfo % Pres
            This % QC  % Pres       ( NLevels ) = LvlInfo % QC_P
            This % IO  % Pres       ( NLevels ) = LvlInfo % IO_P
            This % Obs % Height     ( NLevels ) = LvlInfo % Height
            This % QC  % Height     ( NLevels ) = LvlInfo % QC_H
            This % IO  % Height     ( NLevels ) = LvlInfo % IO_H
            This % Obs % Temp       ( NLevels ) = LvlInfo % Temp
            This % QC  % Temp       ( NLevels ) = LvlInfo % QC_T
            This % IO  % Temp       ( NLevels ) = LvlInfo % IO_T
            This % Obs % DewPt      ( NLevels ) = LvlInfo % DewPt
            This % QC  % DewPt      ( NLevels ) = LvlInfo % QC_DP
            This % IO  % DewPt      ( NLevels ) = LvlInfo % IO_DP
            This % Obs % WDir       ( NLevels ) = LvlInfo % WDir
            This % Obs % WSpeed     ( NLevels ) = LvlInfo % WSpeed
            This % QC  % Wind       ( NLevels ) = LvlInfo % QC_Wind
            This % IO  % Wind       ( NLevels ) = LvlInfo % IO_Wind
            This % Obs % MajorLType ( NLevels ) = LvlInfo % MajorLType
            This % Obs % MinorLType ( NLevels ) = LvlInfo % MinorLType
         end if
         This % Obs    % NLevels = NLevels
         This % QC     % NLevels = NLevels
         This % IO     % NLevels = NLevels
      end if

      return
      end subroutine Add_Level
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Flush_Level () --- Flush level info to text line
! 
! !DESCRIPTION:
!     This routine flushes level info to text line
!
! !INTERFACE:
      subroutine Flush_Level ( iLevel, This, TextLine, stat )
      use m_AdvError, only : ErrStat, WPErr
!
! !INPUT PARAMETERS:
      implicit NONE
      integer,                intent (in)    ::
     .   iLevel
      type ( IGRA_Profile ),  intent (in)    :: 
     .   This                 ! Profile data
!
! !INPUT/OUTPUT PARAMETERS:
      character (len=*),      intent (inout) ::
     .   TextLine             ! Text line containing the info
!
! !OUTPUT PARAMETERS:
      integer,                intent (out)   ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName = MyModule // '::Flush_Level'
      type ( IGRA_Level ) :: LvlInfo
      integer :: NLevels, NLevMax

      NLevMax = This % Obs % NLevMax
      if ( iLevel .le. NLevMax ) then
         LvlInfo % Pres       = This % Obs % Pres       ( iLevel )
         LvlInfo % QC_P       = This % QC  % Pres       ( iLevel )
         LvlInfo % IO_P       = This % IO  % Pres       ( iLevel )
         LvlInfo % Height     = This % Obs % Height     ( iLevel )
         LvlInfo % QC_H       = This % QC  % Height     ( iLevel )
         LvlInfo % IO_H       = This % IO  % Height     ( iLevel )
         LvlInfo % Temp       = This % Obs % Temp       ( iLevel )
         LvlInfo % QC_T       = This % QC  % Temp       ( iLevel )
         LvlInfo % IO_T       = This % IO  % Temp       ( iLevel )
         LvlInfo % DewPt      = This % Obs % DewPt      ( iLevel )
         LvlInfo % QC_DP      = This % QC  % DewPt      ( iLevel )
         LvlInfo % IO_DP      = This % IO  % DewPt      ( iLevel )
         LvlInfo % WDir       = This % Obs % WDir       ( iLevel )
         LvlInfo % WSpeed     = This % Obs % WSpeed     ( iLevel )
         LvlInfo % QC_Wind    = This % QC  % Wind       ( iLevel )
         LvlInfo % IO_Wind    = This % IO  % Wind       ( iLevel )
         LvlInfo % MajorLType = This % Obs % MajorLType ( iLevel )
         LvlInfo % MinorLType = This % Obs % MinorLType ( iLevel )
      end if

      call Put_Level ( LvlInfo, TextLine, stat )
      if ( stat .ne. No_Error ) then
         call WPErr ( MyName, ErrStat ( 'Put_Level', stat ))
         return
      end if

      return
      end subroutine Flush_Level
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_Level () --- Get level info from text line
! 
! !DESCRIPTION:
!     This routine gets level info from text line
!
! !INTERFACE:
      subroutine Get_Level ( TextLine, This, stat )
      use m_AtoX,     only : AtoI
      use m_AdvError, only : ItoA, PErr, ErrStat, WPErr
      use m_RadData,  only : QC_OK, QC_Rejected
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len=*),    intent (in)    ::
     .   TextLine           ! Text line containing the info
!
! !OUTPUT PARAMETERS:
      type ( IGRA_Level ),  intent (inout) :: 
     .   This               ! Level info
      integer,              intent (out)   ::
     .   stat               ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_Level'
      integer, parameter ::
     .   LPres     = 6,
     .   LHeight   = 5,
     .   LTemp     = 5,
     .   LDewPtDep = 5,
     .   LWDir     = 5,
     .   LWSpeed   = 5
      character (len=max ( LPres, LHeight, LTemp, LDewPtDep,
     .                     LWDir, LWSpeed )) :: Token
      logical :: bad_value
      integer :: iBeg, iEnd, iTBeg, iTEnd, jTBeg, iNum
      real :: RNum

      stat  = No_Error

!     Major level type indicator
!     --------------------------
      iBeg  = 1
      iEnd  = 1
      Token = TextLine ( iBeg : iEnd )
      if      ( Token ( : 1 ) .ne. StanLevel .and.
     .          Token ( : 1 ) .ne. SigTLevel .and.
     .          Token ( : 1 ) .ne. WindLevel ) then
         if ( Token ( : 1 ) .eq. BLK ) then
            call WPErr ( MyName, 'Token for major level type '
     .                        // 'does not exist in assumed location' )
            stat = Bad_Record
            return
         else
            call WPErr ( MyName, 'Bad token for major level type (= '
     .                         // trim ( Token ) // ')' )
            stat = Bad_Record
            return
         end if
      end if
      This  % MajorLType = Token

!     Minor level type indicator
!     --------------------------
      iBeg  = 2
      iEnd  = 2
      Token = TextLine ( iBeg : iEnd )
      if ( Token ( : 1 ) .ne. Surface    .and.
     .     Token ( : 1 ) .ne. Tropopause .and.
     .     Token ( : 1 ) .ne. OtherType ) then
         if  ( Token ( : 1 ) .eq. BLK ) then
            call WPErr ( MyName, 'Token for minor level type '
     .                        // 'does not exist in assumed location' )
            stat = Bad_Record
            return
         else
            call WPErr ( MyName, 'Bad token for minor level type (= '
     .                      // trim ( Token ) // ')' )
            stat = Bad_Record
            return
         end if
      end if
      This  % MinorLType = Token

!     Pressure level
!     --------------
      iBeg  = iEnd + 1
      iEnd  = iBeg + LPres - 1
      Token = TextLine ( iBeg : iEnd )
      iTBeg = verify ( Token ( : LPres ), BLK )
      jTBeg = max ( 1, iTBeg )
      if ( Token ( jTBeg : LPres ) .eq. CError_Data .or.
     .     Token ( jTBeg : LPres ) .eq. CMiss_Data ) then
         This % QC_P = QC_Rejected
         This % Pres = 1000.0
      else
         INum = AtoI ( Token, stat )
         if      ( stat  .ne. 0 ) then
            call WPErr ( MyName, 'Bad pressure value (= '
     .                         // trim ( Token ) // ')' )
            stat = Bad_Record
            return
         else if ( iTBeg .eq. 0 ) then
            call WPErr ( MyName, 'Token for pressure level '
     .                        // 'does not exist in assumed location' )
            stat = Bad_Record
            return
         end if
         This % QC_P = QC_OK
         This % Pres = real ( INum ) / 100.0
      end if
      This % IO_P = This % QC_P

!     Height
!     ------
      iBeg  = iEnd + 2
      iEnd  = iBeg + LHeight - 1
      Token = TextLine ( iBeg : iEnd )
      iTBeg = verify ( Token ( : LHeight ), BLK )
      jTBeg = max ( 1, iTBeg )
      if ( Token ( jTBeg : LHeight ) .eq. CError_Data .or.
     .     Token ( jTBeg : LHeight ) .eq. CMiss_Data ) then
         This % QC_H   = QC_Rejected
         This % Height = 0.0
      else
         INum = AtoI ( Token, stat )
         if      ( stat  .ne. 0 ) then
            call WPErr ( MyName, 'Bad height value (= '
     .                         // trim ( Token ) // ')' )
            stat = Bad_Record
            return
         else if ( iTBeg .eq. 0 ) then
            call WPErr ( MyName, 'Token for height '
     .                        // 'does not exist in assumed location' )
            stat = Bad_Record
            return
         end if
         This % QC_H   = QC_OK
         This % Height = real ( INum )
      end if
      This % IO_H = This % QC_H

!     Temperature
!     -----------
      iBeg  = iEnd + 2
      iEnd  = iBeg + LTemp - 1
      Token = TextLine ( iBeg : iEnd )
      iTBeg = verify ( Token ( : LTemp ), BLK )
      jTBeg = max ( 1, iTBeg )
      if ( Token ( jTBeg : LTemp ) .eq. CError_Data .or.
     .     Token ( jTBeg : LTemp ) .eq. CMiss_Data ) then
         This % QC_T = QC_Rejected
         This % Temp = Temp0_K
      else
         INum = AtoI ( Token, stat )
         if      ( stat  .ne. 0 ) then
            call WPErr ( MyName, 'Bad temperature value (= '
     .                         // trim ( Token ) // ')' )
            stat = Bad_Record
            return
         else if ( iTBeg .eq. 0 ) then
            call WPErr ( MyName, 'Token for temperature '
     .                        // 'does not exist in assumed location' )
            stat = Bad_Record
            return
         end if
         This % QC_T = QC_OK
         This % Temp = real ( INum ) / 10.0 + Temp0_K
      end if
      This % IO_T = This % QC_T

!     Dew point
!     ---------
      iBeg  = iEnd + 2
      iEnd  = iBeg + LDewPtDep - 1
      Token = TextLine ( iBeg : iEnd )
      iTBeg = verify ( Token ( : LDewPtDep ), BLK )
      jTBeg = max ( 1, iTBeg )
      if ( Token ( jTBeg : LDewPtDep ) .eq. CError_Data .or.
     .     Token ( jTBeg : LDewPtDep ) .eq. CMiss_Data ) then
         This % QC_DP = QC_Rejected
         This % DewPt = This % Temp - 10.0
      else
         INum = AtoI ( Token, stat )
         if      ( stat  .ne. 0 ) then
            call WPErr ( MyName, 'Bad value for dew point dep (= '
     .                         // trim ( Token ) // ')' )
            stat = Bad_Record
            return
         else if ( iTBeg .eq. 0 ) then
            call WPErr ( MyName, 'Token for dew point dep '
     .                        // 'does not exist in assumed location' )
            stat = Bad_Record
            return
         end if
         if ( This % QC_T .eq. QC_OK ) then
            This % QC_DP = QC_OK
            This % DewPt = This % Temp - real ( INum ) / 10.0
         else
            This % QC_DP = QC_Rejected
            This % DewPt = This % Temp - 10.0
         end if
      end if
      This % IO_DP = This % QC_DP

!     Wind direction
!     --------------
      iBeg  = iEnd + 1
      iEnd  = iBeg + LWDir - 1
      Token = TextLine ( iBeg : iEnd )
      iTBeg = verify ( Token ( : LWDir ), BLK )
      jTBeg = max ( 1, iTBeg )
      if ( Token ( jTBeg : LWDir ) .eq. CError_Data .or.
     .     Token ( jTBeg : LWDir ) .eq. CMiss_Data ) then
         This % QC_Wind = QC_Rejected
         This % WDir    = 0.0
      else
         INum = AtoI ( Token, stat )
         if      ( stat  .ne. 0 ) then
            call WPErr ( MyName, 'Bad value for wind direction (= '
     .                         // trim ( Token ) // ')' )
            stat = Bad_Record
            return
         else if ( iTBeg .eq. 0 ) then
            call WPErr ( MyName, 'Token for wind direction '
     .                        // 'does not exist in assumed location' )
            stat = Bad_Record
            return
         end if
         This % QC_Wind = QC_OK
         This % WDir = real ( INum )
      end if

!     Wind speed
!     ----------
      iBeg  = iEnd + 1
      iEnd  = iBeg + LWSPeed - 1
      Token = TextLine ( iBeg : iEnd )
      iTBeg = verify ( Token ( : LWSpeed ), BLK )
      jTBeg = max ( 1, iTBeg )
      if ( Token ( jTBeg : LWSpeed ) .eq. CError_Data .or.
     .     Token ( jTBeg : LWSpeed ) .eq. CMiss_Data ) then
         This % QC_Wind = QC_Rejected
         This % WSpeed  = 0.0
      else
         INum = AtoI ( Token, stat )
         if      ( stat  .ne. 0 ) then
            call WPErr ( MyName, 'Bad value for wind speed (= '
     .                         // trim ( Token ) // ')' )
            stat = Bad_Record
            return
         else if ( iTBeg .eq. 0 ) then
            call WPErr ( MyName, 'Token for wind speed '
     .                        // 'does not exist in assumed location' )
            stat = Bad_Record
            return
         end if
!        Do not validate if the wind direction was rejected 
!        --------------------------------------------------
         This % WSpeed  = real ( INum ) / 10.0
      end if
      This % IO_Wind = This % QC_Wind


      This % accept_level = ( This % QC_P       .eq. QC_OK     .and. 
     .                      ( This % MajorLType .eq. StanLevel .or.
     .                        This % MajorLType .eq. SigTLevel ))
     .                 .or. ( This % QC_H       .eq. QC_OK     .and.
     .                        This % MajorLType .eq. WindLevel )

      return
      end subroutine Get_Level
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Put_Level () --- Put (write) level info to text line
! 
! !DESCRIPTION:
!     This routine put and replaces level info to the text line.  Any value
!     with an IO flag of IO_Rejected or QC flag of QC_Rejected will be ignored
!     and therefore will not replace the corresponding value in the text line.
!     The only values to be replaced are height, temperature, humidity and
!     wind at standard and surface levels.  At additional wind levels, only
!     winds will be replaced.
!
! !INTERFACE:
      subroutine Put_Level ( This, TextLine, stat )
      use m_AdvError, only : ItoA, PErr, ErrStat, WPErr
      use m_RadData,  only : QC_OK, QC_Rejected
!
! !INPUT PARAMETERS:
      implicit NONE
      type ( IGRA_Level ),  intent (in)    :: 
     .   This               ! Level info
!
! !INPUT/OUTPUT PARAMETERS:
      character (len=*),    intent (inout) ::
     .   TextLine           ! Text line containing the info 
!
! !OUTPUT PARAMETERS:
      integer,              intent (out)   ::
     .   stat               ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName = MyModule // '::Put_Level'
      integer, parameter ::
     .   LPres     = 6,
     .   LHeight   = 5,
     .   LTemp     = 5,
     .   LDewPtDep = 5,
     .   LWDir     = 5,
     .   LWSpeed   = 5
      character (len=1) :: MajorType, MinorType
      character (len=LRecLev) :: Text
      logical :: print_value
      character (len=15) :: Value
      integer :: iBeg, iEnd, iTBeg, iTEnd, jTBeg, INum
      real :: RNum

      stat  = No_Error


!     Major level type indicator
!     --------------------------
      MajorType = This % MajorLType
      if ( MajorType .ne. StanLevel .and.
     .     MajorType .ne. SigTLevel .and.
     .     MajorType .ne. WindLevel ) then
         if ( MajorType .eq. BLK ) then
            call WPErr ( MyName, 'Major level type is blank' )
            stat = Bad_Value
            return
         else
            call WPErr ( MyName, 'Bad major level type (= '
     .                         // MajorType // ')' )
            stat = Bad_Value
            return
         end if
      end if

!     Minor level type indicator
!     --------------------------
      MinorType = This % MinorLType
      if ( MinorType .ne. Surface    .and.
     .     MinorType .ne. Tropopause .and.
     .     MinorType .ne. OtherType ) then
         if ( MajorType .eq. BLK ) then
            call WPErr ( MyName, 'Minor level type is blank' )
            stat = Bad_Value
            return
         else
            call WPErr ( MyName, 'Bad minor level type (= '
     .                         // MinorType // ')' )
            stat = Bad_Value
            return
         end if
      end if

      Text = TextLine

!     Print data at standard or significant thermodynamic level including ...
!     -----------------------------------------------------------------------
      if ( MajorType .eq. StanLevel .or.
     .     MajorType .eq. SigTLevel ) then

!        ... height
!        ----------
         print_value = This % QC_H .ne. QC_Rejected .and.
     .                 This % IO_H .ne. IO_Rejected
         if ( print_value ) then
            RNum = This % Height
            if ( RNum .lt. -9999.4 .or.
     .           RNum .gt. 99999.4 ) then
               write ( Value, '(e15.7)') RNum
               call WPErr ( MyName, 'Height value (= ' // Value
     .                         // ') to be written is out of range ' )
               stat = Bad_Value
               return
            end if
            INum = nint ( RNum )
            write ( Text ( 10 : 14 ), '(i5)') INum
         end if

!        ... temperature
!        ---------------
         print_value = This % QC_T .ne. QC_Rejected .and.
     .                 This % IO_T .ne. IO_Rejected
         if ( print_value ) then
            RNum = ( This % Temp - Temp0_K ) * 10.0
            if ( RNum .lt. -9999.4 .or.
     .           RNum .gt. 99999.4 ) then
               write ( Value, '(e15.7)') RNum
               call WPErr ( MyName, 'Temperature value (= ' // Value
     .                         // ') to be written is out of range ' )
               stat = Bad_Value
               return
            end if
            INum = nint ( RNum )
            write ( Text ( 16 : 20 ), '(i5)') INum
         end if

!        ... dew point depression
!        ------------------------
         print_value = This % QC_T  .ne. QC_Rejected .and.
     .                 This % QC_DP .ne. QC_Rejected .and.
     .                 This % IO_DP .ne. IO_Rejected
         if ( print_value ) then
            RNum = ( This % Temp - This % DewPt ) * 10.0
            if ( RNum .lt. -9999.4 .or.
     .           RNum .gt. 99999.4 ) then
               write ( Value, '(e15.7)') RNum
               call WPErr ( MyName, 'Dew point depression (= ' // Value
     .                         // ') to be written is out of range ' )
               stat = Bad_Value
               return
            end if
            INum = nint ( RNum )
            write ( Text ( 22 : 26 ), '(i5)') INum
         end if
      end if

!     ... wind 
!     --------
      print_value = This % QC_Wind .ne. QC_Rejected .and.
     .              This % IO_Wind .ne. IO_Rejected

!     ... direction
!     -------------
      if ( print_value ) then
         RNum = This % WDir
         if ( RNum .lt. -9999.4 .or.
     .        RNum .gt. 99999.4 ) then
            write ( Value, '(e15.7)') RNum
            call WPErr ( MyName, 'Wind direction (= ' // Value
     .                      // ') to be written is out of range ' )
            stat = Bad_Value
            return
         end if
         INum = nint ( RNum )
         write ( Text ( 27 : 31 ), '(i5)') INum
      end if

!     ... and speed
!     -------------
      if ( print_value ) then
         RNum = This % WSpeed * 10.0
         if ( RNum .lt. -9999.4 .or.
     .        RNum .gt. 99999.4 ) then
            write ( Value, '(e15.7)') RNum
            call WPErr ( MyName, 'Wind speed (= ' // Value
     .                      // ') to be written is out of range ' )
            stat = Bad_Value
            return
         end if
         INum = nint ( RNum )
         write ( Text ( 32 : 36 ), '(i5)') INum
      end if

      TextLine = Text

      return
      end subroutine Put_Level
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_StnMeta () --- Get the IGRA meta data for a station.
! 
! !DESCRIPTION:
!     This routine retrieves IGRA station meta data for a given WMO station
!     ID from a station list
!
! !INTERFACE:
      subroutine Get_StnMeta ( StnID, StnList, StnMeta, stat )

      use m_AdvError, only : ItoA, PErr, ErrStat, WPErr
      use m_Range,    only : Range
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len=*),        intent (in)    ::
     .   StnID
      type ( IGRA_Stations ),   intent (in)    :: 
     .   StnList              ! IGRA station list
!
! !OUTPUT PARAMETERS:
      type ( IGRA_Station ),    intent (inout) :: 
     .   StnMeta              ! IGRA station meta data
!
! !OUTPUT PARAMETERS:
      integer,                  intent (out)   ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Mar2007  C. Redder  Origional code.
! EOP
!..............................................................

      character (len=*), parameter ::
     .   MyName   = MyModule // '::Get_StnMeta'
      integer :: iStn, NStn, StnNav(2)
      logical :: get_meta
      character (len=LStnID), dimension (:), pointer :: StnIDs

      stat = No_Error

!     Get new meta-data (if station has changed)
!     ------------------------------------------
      get_meta = StnID .ne. StnMeta % StnID
      if ( .not. get_meta ) return

!     Find station in IGRA list
!     -------------------------
      NStn   =  StnList % NStn
      StnIDs => StnList % StnID ( : NStn )
      StnNav =  Range ( StnIDs, StnID )
      if ( StnNav (2) .eq. 0 ) then
         call WPErr    ( MyName, 'Station identified in IGRA dataset '
     .                    //     'is not found in the IGRA list of '
     .                    //     'stations '
     .                    // '\C\n   WMO station ID = '
     .                    //         trim ( StnID ))
         stat = Bad_Header
         return
      else if ( StnNav (2) .gt. 1 ) then
         call WPErr    ( MyName, 'Station identified in IGRA dataset '
     .                    //     'is listed more than once in the '
     .                    //     'IGRA list of stations '
     .                    // '\C\n   WMO station ID = '
     .                    //         trim ( StnID ))
         stat = Bad_Header
         return
      end if
      iStn = StnNav ( 1 )

!     Save the meta-data
!     ------------------
      StnMeta % StnID   = StnID
      StnMeta % StnName = StnList % StnName ( iStn )
      StnMeta % Lat     = StnList % Lat     ( iStn )
      StnMeta % Lon     = StnList % Lon     ( iStn )
      StnMeta % Elev    = StnList % Elev    ( iStn )

      return
      end subroutine Get_StnMeta
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Get_RadMeta () --- Get the IGRA meta data for one report.
! 
! !DESCRIPTION:
!     This routine retrieves IGRA meta (i.e. header) data for one report from
!     the sounding and station data. 
!
! !INTERFACE:
      subroutine Get_RadMeta ( Header, StnMeta, RadMeta, stat )

      use m_AdvError,     only : ItoA, PErr, ErrStat, WPErr
      use m_RadData,      only : radcor_meta, LTHist_File,
     .                           Rad_Init,    LTHist_ObTime
      use m_Range,        only : Range
!
! !INPUT/OUTPUT PARAMETERS:
      implicit NONE
      type ( IGRA_Header ),     intent (in)    :: 
     .   Header               ! IGRA header
      type ( IGRA_Station ),    intent (in)    :: 
     .   StnMeta              ! IGRA station meta-data
      type ( radcor_meta ),     intent (inout) :: 
     .   RadMeta              ! IGRA sounding data
!
! !OUTPUT PARAMETERS:
      integer,                  intent (out) ::
     .   stat                 ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     24Apr2007  C. Redder  Origional code.
! EOP
!..............................................................

      character (len=*), parameter ::
     .   MyName   = MyModule // '::Get_RadMeta'
      logical :: initialized, get_all_meta
      integer :: Nks
      character (len=LStnID), dimension (:), pointer :: StnIDs
      character (len=LStnID) :: StnID

!     Default status
!     --------------
      stat = No_Error

!     Initialize data structure (if necessary)
!     ---------------------------------------
      initialized = RadMeta % NVct .eq. 1
      if ( .not. initialized ) then
         Nks      = 1
         call Rad_Init ( Nks, RadMeta, stat )
         if ( stat .ne. 0 ) then            
            call WPErr ( MyName, ErrStat ( 'Rad_Init', stat ))
            stat = Alloc_Error
            return
         end if
      end if      

!     Save temporal meta-data 
!     -----------------------
      RadMeta % Nks           = 1
      RadMeta % Date          = Header  % DateHr / 100
      RadMeta % Time          = mod ( Header % DateHr, 100 ) * 10000 
      RadMeta % ObTime  ( 1 ) = 0
      RadMeta %  LTime  ( 1 ) = Header  % LTime
      RadMeta % LTHist  ( 1 ) = Header  % LTHist
      RadMeta % ksLoc   ( 1 ) = 1
      RadMeta % ksLen   ( 1 ) = 0

!     Save the remaining meta-data
!     ----------------------------
      RadMeta % StnID   ( 1 ) = StnMeta % StnID
      RadMeta % Lat     ( 1 ) = StnMeta % Lat
      RadMeta % Lon     ( 1 ) = StnMeta % Lon
      RadMeta % Elev    ( 1 ) = StnMeta % Elev
      RadMeta % kx      ( 1 ) = 7
      RadMeta % kx_file ( 1 ) = 7
      RadMeta % rkx     ( 1 ) = IMiss
      RadMeta % RadCode ( 1 ) = IMiss

      return
      end subroutine Get_RadMeta

!....................................................................

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
      use m_SunAlt,   only : Check_DateTime
      use m_AdvError, only : WPErr, ItoA, ErrStat
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*),        intent (in) ::
     .   TextLine             ! Text line containing the header info
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Header ),     intent (inout) ::     
     .   Header               ! Header information
!
! !OUTPUT PARAMETERS: 
      integer,                  intent (out) ::
     .   stat                 ! Return status
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     24Apr2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName = MyModule // '::Get_Header'

      integer :: iBeg, iEnd, iret1, iret2
      integer :: LLine, NLevels, DateHr, Date, Hour
      integer :: Minute, LHour, LMinute, LMin, LTime, LTHist, LToken
      character (len=LStnID) :: StnID
      character (len=80)     :: Message
      character (len=len_trim(TextLine)) :: Token, VerTag, CNStn

      LLine   = len_trim ( TextLine )

!     Retrieve the number of levels (required)
!     ----------------------------------------
      iBeg    = LLine - 4 + 1
      iEnd    = LLine
      Token   = TextLine ( iBeg : iEnd )
      NLevels = AtoI ( Token ( : 4 ), stat )
      if ( stat .ne. No_Error .or. len_trim ( Token ) .eq. 0 ) then
         call WPErr ( MyName, ' Bad value for the number of levels (='
     .                    //       Token // ') \C'
     .                    // '\n   Header = ' // trim ( TextLine )) 
         stat = Bad_Header
         return
      end if

!     ... WMO station ID (required)
!     -----------------------------
      iBeg    = 2
      iEnd    = iBeg + LStnID - 1
      StnID   = TextLine ( iBeg : iEnd )
      if ( len_trim ( StnID ) .eq. 0 ) then
         call WPErr ( MyName, ' Bad WMO station ID (=' // Token // ' \C'
     .                    // '\n   Header = ' // trim ( TextLine )) 
         stat = Bad_Header
         return
      end if

!     ... date/hour (required)
!     ------------------------
      iBeg    = iEnd + 1
      iEnd    = iBeg + LDateHr - 1
      Token   = TextLine ( iBeg : iEnd )
      LToken  = len_trim ( Token )
      DateHr  = AtoI ( Token ( : LDateHr ), stat )
      if ( stat        .ne. No_Error .or.
     .     LToken      .ne. LDateHr  .or.
     .     Token ( : ) .eq. BLK ) then
         call WPErr ( MyName, ' Bad value for the date/hour (='
     .                    //       Token // ') \C'
     .                    // '\n   Header = ' // trim ( TextLine )) 
         stat = Bad_Header
         return
      end if
      Date    =       DateHr / 100
      Hour    = mod ( DateHr,  100 )
      if ( Check_DateTime ( Date, Hour, Message ) .ne. 0 ) then
         call WPErr ( MyName, Message
     .                  // '\n   Header = ' // trim ( TextLine )) 
         stat = Bad_Header
         return
      end if
      Minute =   Hour * 60

!     ...launch time
!     --------------
      iBeg   = iEnd + 1
      iEnd   = iBeg + 4 - 1
      Token  = TextLine ( iBeg : iEnd )
      LTime  = IMiss
      if ( Token ( : 4 )  .ne. '9999' ) then
         LHour   = AtoI ( Token ( 1:2 ), iret1 ) 
         LMinute = AtoI ( Token ( 3:4 ), iret2 ) 
         if ( iret1   .eq. 0  .and.
     .        iret2   .eq. 0 ) then
         if ( LHour   .ge. 0  .and.
     .        LHour   .le. 23 .and.
     .        LMinute .ge. 0  .and.
     .        LMinute .le. 59 ) then
            LMin     = LHour * 60.0 + LMinute
            if       ( LMin - Minute  .gt.  12 * 60 ) then
               LTime = LMin - Minute - 24 * 60
            else if  ( LMin - Minute  .le. -12 * 60 ) then
               LTime = LMin - Minute + 24 * 60
            else
               LTime = LMin - Minute
            end if
            if ( abs ( LTime ) .gt. 60 ) ! ... range check.  LTime
     .         LTime = IMiss             !   must be within 1 hour
         end if                          !   of Time.
         end if
      end if
      if ( LTime .ne. IMiss ) then
         LTime  = LTime * 60
         LTHist = LT_File
      else
         LTime  = 0
         LTHist = LT_ObTime
      end if

      Header % StnID   = StnID
      Header % DateHr  = DateHr
      Header % LTime   = LTime
      Header % LTHist  = LTHist
      Header % NLevels = NLevels

      return
      end subroutine Get_Header

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  WriteLn () --- Write one text line of data
! 
! !DESCRIPTION:
!     This routine writes one text line of text data.
!
! !INTERFACE:
!
      subroutine WriteLn ( lu, Line, stat )
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,             intent (in)  ::
     .   lu                ! FORTRAN logical unit number
      character (len = *), intent (in)  ::
     .   Line              ! Line of data
!
! !OUTPUT PARAMETERS: 
      integer,             intent (out) ::
     .   stat              ! Status code
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     20Feb2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------

!     Write line
!     ---------
      write ( unit    =  lu,
     .        fmt     = '(a)',
     .        advance = 'yes',
     .        iostat  =  stat ) trim ( Line )
      if ( stat .ne. 0 ) return

      return
      end subroutine WriteLn 

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  ReadLn () --- Read one text line of data
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
! !ROUTINE:  Copy_Header () --- Copy contents of one header to another
! 
! !DESCRIPTION:
!     This routine copies the contents of the data structure of type
!     IGRA_Header.
!
! !INTERFACE:
!
      subroutine Copy_Header ( HeaderIn, HeaderOut )
!
! !INPUT PARAMETERS: 
      implicit NONE
      type ( IGRA_Header ), intent (in)    :: HeaderIn
!
! !OUTPUT PARAMETERS: 
      type ( IGRA_Header ), intent (inout) :: HeaderOut
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     24Apr2007  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------

      integer :: NLevels, NLevelsMax

      HeaderOut  % StnID      = HeaderIn % StnID
      HeaderOut  % DateHr     = HeaderIn % DateHr
      HeaderOut  % LTime      = HeaderIn % LTime
      HeaderOut  % LTHist     = HeaderIn % LTHist
      HeaderOut  % NLevels    = HeaderIn % NLevels

      return
      end subroutine Copy_Header
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_Header - Initializes the argument of type IGRA_Header
!
! !INTERFACE:
      subroutine Init_Header ( NLevMax, This, stat,  ! required and
     .                         hdr_data )            ! optional args
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,              intent (in)    ::
     .   NLevMax          ! Maximum number of levels
      type ( IGRA_Header ), intent (in), optional ::
     .   hdr_data         ! Header data to be copied to output arg
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Header ), intent (inout) ::
     .   This             ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,              intent (out)   ::
     .   stat             ! Error status code.  If no error,
                          ! then zero is returned
! !REVISION HISTORY:
!     24Apr2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_Header'

      stat = No_Error

      This % StnID      =  BLK
      This % DateHr     =  0
      This % LTime      =  0
      This % LTHist     =  0
      This % NLevels    =  0

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
! !ROUTINE:  Init_Obs - Initializes the argument of type IGRA_Obs
!
! !INTERFACE:
      subroutine Init_Obs ( NLevMax, This, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NLevMax               ! Maximum total number of levels
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Obs ),         intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_Obs'

      This % NLevMax =  NLevMax
      This % NLevels =  0

      allocate ( This % Mask       ( NLevMax ),
     .           This % Indx       ( NLevMax ),
     .           This % LevList    ( NLevMax ),
     .           This % Pres       ( NLevMax ),
     .           This % Height     ( NLevMax ),
     .           This % Temp       ( NLevMax ),
     .           This % DewPt      ( NLevMax ),
     .           This % WDir       ( NLevMax ),
     .           This % WSpeed     ( NLevMax ),
     .           This % P          ( NLevMax ),
     .           This % logP       ( NLevMax ),
     .           This % H          ( NLevMax ),
     .           This % UWind      ( NLevMax ),
     .           This % VWind      ( NLevMax ),
     .           This % ETime      ( NLevMax ),
     .           This % DLat       ( NLevMax ),
     .           This % DLon       ( NLevMax ),
     .           This % MajorLType ( NLevMax ),
     .           This % MinorLType ( NLevMax ),
     .           stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'This%Mask,'
     .                                  // 'This%Indx,'
     .                                  // 'This%LevList,'
     .                                  // 'This%Pres ,'
     .                                  // 'This%Height,'
     .                                  // 'This%Temp,'
     .                                  // 'This%DewPt,'
     .                                  // 'This%WDir,'
     .                                  // 'This%WSpeed,'
     .                                  // 'This%P,'
     .                                  // 'This%logP,'
     .                                  // 'This%H,'
     .                                  // 'This%UWind,'
     .                                  // 'This%VWind,'
     .                                  // 'This%ETime,'
     .                                  // 'This%DLat,'
     .                                  // 'This%DLon,'
     .                                  // 'This%MajorLType,'
     .                                  // 'This%MinorLType',
     .                                      NLevMax ) // '\W' )
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
! !ROUTINE:  Init_QC -  Initializes the argument of type IGRA_QC
!
! !INTERFACE:
      subroutine Init_QC ( NLevMax, This, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NLevMax               ! Maximum total number of levels
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_QC ),          intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_QC'

      This % NLevMax =  NLevMax
      This % NLevels =  0

      allocate ( This % Pres   ( NLevMax ),
     .           This % Height ( NLevMax ),
     .           This % Temp   ( NLevMax ),
     .           This % DewPt  ( NLevMax ),
     .           This % Wind   ( NLevMax ),
     .           stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'This%Pres ,'
     .                                  // 'This%Height,'
     .                                  // 'This%Temp,'
     .                                  // 'This%DewPt,'
     .                                  // 'This%Wind',
     .                                      NLevMax ) // '\W' )
         stat = Alloc_Error
         return
      end if

      return
      end subroutine Init_QC

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_IO -  Initializes the argument of type IGRA_IO
!
! !INTERFACE:
      subroutine Init_IO ( NLevMax, This, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NLevMax               ! Maximum total number of levels
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_IO ),          intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_IO'

      This % NLevMax =  NLevMax
      This % NLevels =  0

      allocate ( This % Pres   ( NLevMax ),
     .           This % Height ( NLevMax ),
     .           This % Temp   ( NLevMax ),
     .           This % DewPt  ( NLevMax ),
     .           This % Wind   ( NLevMax ),
     .           stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'This%Pres ,'
     .                                  // 'This%Height,'
     .                                  // 'This%Temp,'
     .                                  // 'This%DewPt,'
     .                                  // 'This%Wind',
     .                                      NLevMax ) // '\W' )
         stat = Alloc_Error
         return
      end if

      return
      end subroutine Init_IO

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_SigT - Initializes the argument of type IGRA_SigT
!
! !INTERFACE:
      subroutine Init_SigT ( NLevMax, This, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NLevMax               ! Maximum total number of levels
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_SigT ),        intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_SigT'

      This % NLevMax =  NLevMax
      This % NLevels =  0

      allocate ( This % Mask       ( NLevMax ),
     .           This % Indx       ( NLevMax ),
     .           This % LevList    ( NLevMax ),
     .           This % IPres      ( NLevMax ),
     .           This % Pres       ( NLevMax ),
     .           This % Height     ( NLevMax ),
     .           This % Temp       ( NLevMax ),
     .           This % DewPt      ( NLevMax ),
     .           This % logP       ( NLevMax ),
     .           This % H          ( NLevMax ),
     .           This % T          ( NLevMax ),
     .           This % VT         ( NLevMax ),
     .           This % DP         ( NLevMax ),
     .           This % MR         ( NLevMax ),
     .           This % RH         ( NLevMax ),
     .           stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'This%Mask,'
     .                                  // 'This%Indx,'
     .                                  // 'This%LevList,'
     .                                  // 'This%Pres ,'
     .                                  // 'This%IPres ,'
     .                                  // 'This%Height,'
     .                                  // 'This%Temp,'
     .                                  // 'This%DewPt,'
     .                                  // 'This%logP,'
     .                                  // 'This%H,'
     .                                  // 'This%T,'
     .                                  // 'This%VT,'
     .                                  // 'This%DP,'
     .                                  // 'This%MR,'
     .                                  // 'This%RH',
     .                                      NLevMax ) // '\W' )
         stat = Alloc_Error
         return
      end if

      return
      end subroutine Init_SigT

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_Wind - Initializes the argument of type IGRA_Wind
!
! !INTERFACE:
      subroutine Init_Wind ( NLevMax, This, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NLevMax               ! Maximum total number of levels
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Wind ),        intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_Wind'

      This % NLevMax =  NLevMax
      This % NLevels =  0

      allocate ( This % Mask       ( NLevMax ),
     .           This % Indx       ( NLevMax ),
     .           This % LevList    ( NLevMax ),
     .           This % IHeight    ( NLevMax ),
     .           This % IPres      ( NLevMax ),
     .           This % Pres       ( NLevMax ),
     .           This % Height     ( NLevMax ),
     .           This % logP       ( NLevMax ),
     .           stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'This%Mask,'
     .                                  // 'This%Indx,'
     .                                  // 'This%LevList,'
     .                                  // 'This%IHeight,'
     .                                  // 'This%IPres,'
     .                                  // 'This%Pres,'
     .                                  // 'This%Height,'
     .                                  // 'This%logP',
     .                                      NLevMax ) // '\W' )
         stat = Alloc_Error
         return
      end if

      return
      end subroutine Init_Wind

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_Sounding - Initializes the argument of type IGRA_Sounding
!
! !INTERFACE:
      subroutine Init_Sounding ( NLevMax, This, stat, ! required and
     .                           hdr_data )           ! optional args
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat, Array
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NLevMax               ! Maximum total number of levels
       type ( IGRA_Header ),     intent (in), optional ::
     .   hdr_data              ! Header data to be copied to output arg
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Sounding ),    intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_Sounding'
      integer :: NTAdjMax, dstat

      This % NLevMax =  NLevMax
      This % Header  =  BLK

      call Init_Header ( NLevMax, This % HdrInfo, stat,
     .                   hdr_data = hdr_data )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Header' ) // '\W' )
         return
      end if

      allocate ( This % DataR   ( NLevMax ), stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName,
     .               Alloc ( stat, 'This%DataR', NLevMax ) // '\W' )
         call Clean_Header ( This % HdrInfo )
         stat = Alloc_Error
         return
      end if

      This % initialized = .true.

      return
      end subroutine Init_Sounding

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_Profile - Initializes the argument of type IGRA_Profile
!
! !INTERFACE:
      subroutine Init_Profile ( NLevMax, This, stat, ! required and
     .                          hdr_data )           ! optional args
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                 intent (in)    ::
     .   NLevMax             ! Maximum number of levels
      type ( IGRA_Header ),    intent (in), optional ::
     .   hdr_data            ! Header data to be copied to output arg
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Profile ),   intent (inout) ::
     .   This                ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                 intent (out)   ::
     .   stat                ! Error status code.  If no error,
                             ! then zero is returned
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_Profile'

      call Init_Header ( NLevMax, This % Header, stat,
     .                   hdr_data = hdr_data )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Header' ) // '\W' )
         return
      end if

      call Init_Obs    ( NLevMax, This % Obs,    stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Obs'    ) // '\W' )
         call Clean_Header  ( This % Header )
         return
      end if

      call Init_QC     ( NLevMax, This % QC,     stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_QC'     ) // '\W' )
         call Clean_Header  ( This % Header )
         call Clean_Obs     ( This % Obs    )
         return
      end if

      call Init_IO     ( NLevMax, This % IO,     stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_IO'     ) // '\W' )
         call Clean_Header  ( This % Header )
         call Clean_Obs     ( This % Obs    )
         call Clean_QC      ( This % QC     )
         return
      end if

      call Init_SigT   ( NLevMax, This % SigT,   stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_SigT'   ) // '\W' )
         call Clean_Header  ( This % Header )
         call Clean_Obs     ( This % Obs    )
         call Clean_QC      ( This % QC     )
         call Clean_IO      ( This % IO     )
         return
      end if

      call Init_Wind   ( NLevMax, This % Wind,   stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, ErrStat ( 'Init_Wind'   ) // '\W' )
         call Clean_Header  ( This % Header )
         call Clean_Obs     ( This % Obs    )
         call Clean_QC      ( This % QC     )
         call Clean_IO      ( This % IO     )
         call Clean_SigT    ( This % SigT   )
         return
      end if

      This % initialized = .true.

      return
      end subroutine Init_Profile

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init_Stations -  Initializes the argument of type IGRA_Stations
!
! !INTERFACE:
      subroutine Init_Stations ( NStnMax, This, stat )
!
! !USES:
      use m_AdvError,     only : PErr, Alloc, ErrStat
      implicit NONE
!
! !INPUT PARAMETERS:
      integer,                   intent (in)    ::
     .   NStnMax               ! Maximum total number of stations
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Stations ),    intent (inout) ::
     .   This                  ! Data structure to be initialized
!
! !OUTPUT PARAMETERS:
      integer,                   intent (out)   ::
     .   stat                  ! Error status code.  If no error,
                               ! then zero is returned
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Init_Stations'

      This % NStnMax =  NStnMax
      This % NStn    =  0

      allocate ( This % StnID   ( NStnMax ),
     .           This % StnName ( NStnMax ),
     .           This % Lat     ( NStnMax ),
     .           This % Lon     ( NStnMax ),
     .           This % Elev    ( NStnMax ), stat = stat )
      if ( stat .ne. No_Error ) then
         call PErr ( MyName, Alloc ( stat, 'This%StnID,'
     .                                  // 'This%StnName ,'
     .                                  // 'This%Lat,'
     .                                  // 'This%Lon,'
     .                                  // 'This%Elev',
     .                                      NStnMax ) // '\W' )
         stat = Alloc_Error
         return
      end if

      return
      end subroutine Init_Stations

!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_Header - Initialize and deallocates the argument of type IGRA_Header
!
! !INTERFACE:
      subroutine Clean_Header ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Header ),  intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,     intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     24Apr2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Header'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % StnID     = BLK
      This % DateHr    = 0
      This % LTime     = 0
      This % LTHist    = 0
      This % NLevels   = 0

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
! !ROUTINE:  Clean_Obs - Initialize and deallocates the argument of type IGRA_Obs
!
! !INTERFACE:
      subroutine Clean_Obs ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Obs ),  intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,  intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Obs'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % NLevMax   = 0
      This % NLevels   = 0

!     Deallocate memory
!     -----------------
      deallocate ( This % Mask,
     .             This % Indx,
     .             This % LevList,
     .             This % Pres,
     .             This % Height,
     .             This % Temp,
     .             This % DewPt,
     .             This % WDir,
     .             This % WSpeed,
     .             This % P,
     .             This % logP,
     .             This % H,
     .             This % UWind,
     .             This % VWind,
     .             This % ETime,
     .             This % DLat,
     .             This % DLon,
     .             This % MajorLType,
     .             This % MinorLType,
     .             stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%Mask,'
     .                                    // 'This%Indx,'
     .                                    // 'This%LevList,'
     .                                    // 'This%Pres,'
     .                                    // 'This%Height,'
     .                                    // 'This%Temp,'
     .                                    // 'This%DewPt,'
     .                                    // 'This%WDir,'
     .                                    // 'This%WSpeed,'
     .                                    // 'This%P,'
     .                                    // 'This%logP,'
     .                                    // 'This%H,'
     .                                    // 'This%UWind,'
     .                                    // 'This%VWind,'
     .                                    // 'This%ETime,'
     .                                    // 'This%DLat,'
     .                                    // 'This%DLon,'
     .                                    // 'This%MajorLType,'
     .                                    // 'This%MinorLType' ))
     .                                    // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_Obs ( This )

      return
      end subroutine  Clean_Obs

!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_QC - Initialize and deallocates the argument of type IGRA_QC
!
! !INTERFACE:
      subroutine Clean_QC ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_QC ),  intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional, intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_QC'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % NLevMax   = 0
      This % NLevels   = 0

!     Deallocate memory
!     -----------------
      deallocate ( This % Pres,
     .             This % Height,
     .             This % Temp,
     .             This % DewPt,
     .             This % Wind, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%Pres,'
     .                                    // 'This%Height,'
     .                                    // 'This%Temp,'
     .                                    // 'This%DewPt,'
     .                                    // 'This%Wind' )) // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_QC ( This )

      return
      end subroutine  Clean_QC
!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_IO - Initialize and deallocates the argument of type IGRA_IO
!
! !INTERFACE:
      subroutine Clean_IO ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_IO ),  intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional, intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_IO'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % NLevMax   = 0
      This % NLevels   = 0

!     Deallocate memory
!     -----------------
      deallocate ( This % Pres,
     .             This % Height,
     .             This % Temp,
     .             This % DewPt,
     .             This % Wind, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%Pres,'
     .                                    // 'This%Height,'
     .                                    // 'This%Temp,'
     .                                    // 'This%DewPt,'
     .                                    // 'This%Wind' )) // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_IO ( This )

      return
      end subroutine  Clean_IO
!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_SigT - Initialize and deallocates the argument of type IGRA_SigT
!
! !INTERFACE:
      subroutine Clean_SigT ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_SigT ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,  intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     20Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_SigT'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % NLevMax   = 0
      This % NLevels   = 0

!     Deallocate memory
!     -----------------
      deallocate ( This % Mask,
     .             This % Indx,
     .             This % LevList,
     .             This % IPres,
     .             This % Pres,
     .             This % Height,
     .             This % Temp,
     .             This % DewPt,
     .             This % logP,
     .             This % H,
     .             This % T,
     .             This % VT,
     .             This % DP,
     .             This % RH,
     .             This % MR,
     .             stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%Mask,'
     .                                    // 'This%Indx,'
     .                                    // 'This%LevList,'
     .                                    // 'This%IPres,'
     .                                    // 'This%Pres,'
     .                                    // 'This%Height,'
     .                                    // 'This%Temp,'
     .                                    // 'This%DewPt,'
     .                                    // 'This%logP,'
     .                                    // 'This%H,'
     .                                    // 'This%T,'
     .                                    // 'This%VT,'
     .                                    // 'This%DP,'
     .                                    // 'This%RH,'
     .                                    // 'This%MR' ))
     .                                    // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_SigT ( This )

      return
      end subroutine  Clean_SigT

!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_Wind - Initialize and deallocates the argument of type IGRA_Wind
!
! !INTERFACE:
      subroutine Clean_Wind ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Wind ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,  intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     20Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Wind'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % NLevMax   = 0
      This % NLevels   = 0

!     Deallocate memory
!     -----------------
      deallocate ( This % Mask,
     .             This % Indx,
     .             This % LevList,
     .             This % IHeight,
     .             This % IPres,
     .             This % Pres,
     .             This % Height,
     .             This % logP,
     .             stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%Mask,'
     .                                    // 'This%Indx,'
     .                                    // 'This%LevList,'
     .                                    // 'This%IHeight,'
     .                                    // 'This%IPres,'
     .                                    // 'This%Pres,'
     .                                    // 'This%Height,'
     .                                    // 'This%logP'
     .                                    // '\W' )))
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_Wind ( This )

      return
      end subroutine  Clean_Wind

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_Sounding - Initialize and deallocates the argument of type IGRA_Sounding
!
! !INTERFACE:
!
      subroutine Clean_Sounding ( This, stat )
!
! !USES:
      use m_AdvError, only : PErr, ErrStat, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Sounding ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,      intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Sounding'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )

      call Clean_Header  ( This % HdrInfo, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_Header' ) // '\W' )
            return
         end if
      end if
      This % NLevMax  = 0
      This % Header   = BLK

!     Deallocate memory
!     -----------------
      deallocate ( This % DataR, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'DataR' )) // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_Sounding ( This )

      This % initialized = .false.

      return
      end subroutine  Clean_Sounding
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_Profile - Initialize and deallocates the argument of type IGRA_Profile
!
! !INTERFACE:
!
      subroutine Clean_Profile ( This, stat )
!
! !USES:
      use m_AdvError, only : PErr, ErrStat
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Profile ), intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional,     intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Profile'
      logical :: check_stat

      check_stat = present ( stat )

      call Clean_Header  ( This % Header, stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_Header' ) // '\W' )
            return
         end if
      end if

      call Clean_Obs     ( This % Obs,    stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_Obs'    ) // '\W' )
            return
         end if
      end if

      call Clean_QC      ( This % QC,     stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_QC'     )  // '\W' )
            return
         end if
      end if

      call Clean_IO      ( This % IO,     stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_IO'     )  // '\W' )
            return
         end if
      end if

      call Clean_SigT    ( This % SigT,   stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_SigT'   )  // '\W' )
            return
         end if
      end if

      call Clean_Wind    ( This % Wind,   stat )
      if ( check_stat ) then
         if ( stat .ne. No_Error ) then
            call PErr ( MyName,
     .                  ErrStat ( 'Clean_Wind'   )  // '\W' )
            return
         end if
      end if

      This % initialized = .false.

      return
      end subroutine  Clean_Profile
!.................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Clean_Stations - Initialize and deallocates the argument of type IGRA_Stations
!
! !INTERFACE:
      subroutine Clean_Stations ( This, stat )
!
! !USES: 
      use m_AdvError, only : PErr, Dealloc
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Stations ),  intent (inout) ::
     .   This  ! Data stucture to be deallocated
!
! !OUTPUT PARAMETERS:
      integer, optional, intent (out  ) ::
     .   stat  ! Error return code.  If no error, then zero is returned.
               !   If not present, then the status code from the
               !   deallocate statements are ignored
!
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!EOP
!-------------------------------------------------------------------------
      character(len=*), parameter ::
     .   MyName = MyModule // '::Clean_Stations'
      logical :: check_stat
      integer :: istat

      check_stat = present ( stat )
      istat      = No_Error
      if ( check_stat ) stat = No_Error

      This % NStnMax   = 0
      This % NStn      = 0

!     Deallocate memory
!     -----------------
      deallocate ( This % StnID,
     .             This % StnName,
     .             This % Lat,
     .             This % Lon,
     .             This % Elev, stat = istat )
      if ( istat .ne. No_Error .and. check_stat ) then
         call PErr ( MyName,
     .               trim ( Dealloc ( istat, 'This%StnID,'
     .                                    // 'This%StnName,'
     .                                    // 'This%Lat,'
     .                                    // 'This%Lon,'
     .                                    // 'This%Elev' )) // '\W' )
         stat = Dealloc_Error
         return
      end if

!     Nullify pointers
!     ----------------
      call Nullify_Stations ( This )

      return
      end subroutine  Clean_Stations
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Header - Initializes pointers in the argument of type IGRA_Header
!
! !INTERFACE:
      subroutine Nullify_Header ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Header ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!
!EOP
!.................................................................

      continue ! No pointers in this data structure
 
      return
      end subroutine Nullify_Header

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Obs - Initializes pointers in the argument of type IGRA_Obs
!
! !INTERFACE:
      subroutine Nullify_Obs ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Obs ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!
!EOP
!.................................................................

      nullify ( This % Mask,
     .          This % Indx,
     .          This % LevList,
     .          This % Pres,
     .          This % Height,
     .          This % Temp,
     .          This % DewPt,
     .          This % WDir,
     .          This % WSpeed,
     .          This % P,
     .          This % logP,
     .          This % H,
     .          This % UWind,
     .          This % VWind,
     .          This % ETime,
     .          This % DLat,
     .          This % DLon,
     .          This % MajorLType,
     .          This % MinorLType )

      return
      end subroutine Nullify_Obs

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_QC - Initializes pointers in the argument of type IGRA_QC 
!
! !INTERFACE:
      subroutine Nullify_QC ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_QC ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Initial code
!
!EOP
!.................................................................

      nullify ( This % Pres,
     .          This % Height,
     .          This % Temp,
     .          This % DewPt,
     .          This % Wind )

      return
      end subroutine Nullify_QC

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_IO - Initializes pointers in the argument of type IGRA_IO
!
! !INTERFACE:
      subroutine Nullify_IO ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_IO ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Initial code
!
!EOP
!.................................................................

      nullify ( This % Pres,
     .          This % Height,
     .          This % Temp,
     .          This % DewPt,
     .          This % Wind )

      return
      end subroutine Nullify_IO

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_SigT - Initializes pointers in the argument of type IGRA_SigT
!
! !INTERFACE:
      subroutine Nullify_SigT ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_SigT ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Initial code
!
!EOP
!.................................................................

      nullify ( This % Mask,
     .          This % Indx,
     .          This % LevList,
     .          This % IPres,
     .          This % Pres,
     .          This % Height,
     .          This % Temp,
     .          This % DewPt,
     .          This % logP,
     .          This % H,
     .          This % T,
     .          This % VT,
     .          This % DP,
     .          This % RH,
     .          This % MR )

      return
      end subroutine Nullify_SigT

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Wind - Initializes pointers in the argument of type IGRA_Wind
!
! !INTERFACE:
      subroutine Nullify_Wind ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Wind ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Initial code
!
!EOP
!.................................................................

      nullify ( This % Mask,
     .          This % Indx,
     .          This % LevList,
     .          This % IHeight,
     .          This % IPres,
     .          This % Pres,
     .          This % Height,
     .          This % logP )

      return
      end subroutine Nullify_Wind

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Sounding - Initializes pointers in the argument of type IGRA_Sounding 
!
! !INTERFACE:
      subroutine Nullify_Sounding ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Sounding ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!EOP
!.................................................................

      call Nullify_Header   ( This % HdrInfo )
      nullify ( This % DataR )      

      return
      end subroutine Nullify_Sounding

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Profile - Initializes pointers in the argument of type IGRA_Profile 
!
! !INTERFACE:
      subroutine Nullify_Profile ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Profile ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     15Mar2007  C. Redder  Initial code
!EOP
!.................................................................

      call Nullify_Header ( This % Header )
      call Nullify_Obs    ( This % Obs    )
      call Nullify_QC     ( This % QC     )
      call Nullify_IO     ( This % IO     )
      call Nullify_SigT   ( This % SigT   )
      call Nullify_Wind   ( This % Wind   )
 
      return
      end subroutine Nullify_Profile

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Nullify_Stations - Initializes pointers in the argument of type IGRA_Stations 
!
! !INTERFACE:
      subroutine Nullify_Stations ( This )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( IGRA_Stations ), intent( inout ) ::
     .   This  ! Data structure to be nullified
!
! !REVISION HISTORY:
!     16Mar2007  C. Redder  Initial code
!
!EOP
!.................................................................

      nullify ( This % StnID,
     .          This % StnName,
     .          This % Lat,
     .          This % Lon,
     .          This % Elev )

      return
      end subroutine Nullify_Stations

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_Obs1 - Rearrange the data in the argument of type IGRA_Obs
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_Obs1 ( This1, Indx, This2, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      type (IGRA_Obs),      intent (in) ::
     .   This1              ! Data to be rearranged
      integer,              intent (in), dimension (:) ::
     .   Indx               ! Sorting indices
      logical,              intent (in), optional ::
     .   reverse            ! = .true. to reverse the reordering 
                            !   Default: reverse = .false.
!
! !OUTPUT PARAMETERS:
      type (IGRA_Obs),      intent (inout) ::
     .   This2              ! Rearranged data
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      integer :: iLev, NLev
      logical :: forward
      character(len=1), dimension (:), pointer ::
     .   MajorLType1, MinorLType1,
     .   MajorLType2, MinorLType2

      forward = .true.
      if ( present ( reverse )) forward = .not. reverse 
      NLev    = size ( Indx )

      MajorLType1 => This1 % MajorLType ( : NLev )
      MajorLType2 => This2 % MajorLType ( : NLev )
      MinorLType1 => This1 % MinorLType ( : NLev )
      MinorLType2 => This2 % MinorLType ( : NLev )
      if ( forward ) then
         This2 % Mask    ( : NLev )
     .      = (/( This1 % Mask    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % LevList ( : NLev )
     .      = (/( This1 % LevList ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Pres    ( : NLev )
     .      = (/( This1 % Pres    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Height  ( : NLev )
     .      = (/( This1 % Height  ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Temp    ( : NLev )
     .      = (/( This1 % Temp    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % DewPt   ( : NLev )
     .      = (/( This1 % DewPt   ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % WDir    ( : NLev )
     .      = (/( This1 % WDir    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % WSpeed  ( : NLev )
     .      = (/( This1 % WSpeed  ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % P       ( : NLev )
     .      = (/( This1 % P       ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % logP    ( : NLev )
     .      = (/( This1 % logP    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % H       ( : NLev )
     .      = (/( This1 % H       ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % UWind   ( : NLev )
     .      = (/( This1 % UWind   ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % VWind   ( : NLev )
     .      = (/( This1 % VWind   ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % ETime   ( : NLev )
     .      = (/( This1 % ETime   ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % DLat    ( : NLev )
     .      = (/( This1 % DLat    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % DLon    ( : NLev )
     .      = (/( This1 % DLon    ( Indx ( iLev )), iLev = 1, NLev )/)
         MajorLType2     ( : NLev )
     .      = (/( MajorLType1     ( Indx ( iLev )), iLev = 1, NLev )/)
         MinorLType2     ( : NLev )
     .      = (/( MinorLType1     ( Indx ( iLev )), iLev = 1, NLev )/)
      else
         This2 % Mask    ( Indx ( : NLev ))
     .      = (/( This1 % Mask  ( iLev ), iLev = 1, NLev )/)
         This2 % LevList ( Indx ( : NLev ))
     .      = (/( This1 % LevList ( iLev ), iLev = 1, NLev )/)
         This2 % Pres    ( Indx ( : NLev ))
     .      = (/( This1 % Pres    ( iLev ), iLev = 1, NLev )/)
         This2 % Height  ( Indx ( : NLev ))
     .      = (/( This1 % Height  ( iLev ), iLev = 1, NLev )/)
         This2 % Temp    ( Indx ( : NLev ))
     .      = (/( This1 % Temp    ( iLev ), iLev = 1, NLev )/)
         This2 % DewPt   ( Indx ( : NLev ))
     .      = (/( This1 % DewPt   ( iLev ), iLev = 1, NLev )/)
         This2 % WDir    ( Indx ( : NLev ))
     .      = (/( This1 % WDir    ( iLev ), iLev = 1, NLev )/)
         This2 % WSpeed  ( Indx ( : NLev ))
     .      = (/( This1 % WSpeed  ( iLev ), iLev = 1, NLev )/)
         This2 % P       ( Indx ( : NLev ))
     .      = (/( This1 % P       ( iLev ), iLev = 1, NLev )/)
         This2 % logP    ( Indx ( : NLev ))
     .      = (/( This1 % logP    ( iLev ), iLev = 1, NLev )/)
         This2 % H       ( Indx ( : NLev ))
     .      = (/( This1 % H       ( iLev ), iLev = 1, NLev )/)
         This2 % UWind   ( Indx ( : NLev ))
     .      = (/( This1 % UWind   ( iLev ), iLev = 1, NLev )/)
         This2 % VWind   ( Indx ( : NLev ))
     .      = (/( This1 % VWind   ( iLev ), iLev = 1, NLev )/)
         This2 % ETime   ( Indx ( : NLev ))
     .      = (/( This1 % ETime   ( iLev ), iLev = 1, NLev )/)
         This2 % DLat    ( Indx ( : NLev ))
     .      = (/( This1 % DLat    ( iLev ), iLev = 1, NLev )/)
         This2 % DLon    ( Indx ( : NLev ))
     .      = (/( This1 % DLon    ( iLev ), iLev = 1, NLev )/)
         MajorLType2     ( Indx ( : NLev ))
     .      = (/( MajorLType2     ( iLev ), iLev = 1, NLev )/)
         MinorLType2     ( Indx ( : NLev ))
     .      = (/( MinorLType2     ( iLev ), iLev = 1, NLev )/)
      end if

      return
      end subroutine Reorder_Obs1

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_Obs2 - Rearrange the data in the argument of type IGRA_Obs
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_Obs2 ( This, Indx, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      integer,              intent (in), dimension (:) ::
     .   Indx             ! Sorting indices
      logical,              intent (in), optional ::
     .   reverse          ! = .true. to reverse the reordering 
                          !   Default: reverse = .false.
!
! !INPUT/OUTPUT PARAMETERS:
      type (IGRA_Obs),      intent (inout) ::
     .   This             ! Rearranged data
!
! !REVISION HISTORY:
!     21Mar2007  C Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      call Reorder_Obs1 ( This, Indx, This, reverse = reverse )

      return
      end subroutine Reorder_Obs2

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_QC1 - Rearrange the data in the argument of type IGRA_QC
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_QC1 ( This1, Indx, This2, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      type (IGRA_QC),       intent (in) ::
     .   This1              ! Data to be rearranged
      integer,              intent (in), dimension (:) ::
     .   Indx               ! Sorting indices
      logical,              intent (in), optional ::
     .   reverse            ! = .true. to reverse the reordering 
                            !   Default: reverse = .false.
!
! !OUTPUT PARAMETERS:
      type (IGRA_QC),       intent (inout) ::
     .   This2              ! Rearranged data
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      integer :: iLev, NLev
      logical :: forward

      forward = .true.
      if ( present ( reverse )) forward = .not. reverse 
      NLev    = size ( Indx )

      if ( forward ) then
         This2 % Pres    ( : NLev )
     .      = (/( This1 % Pres    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Height  ( : NLev )
     .      = (/( This1 % Height  ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Temp    ( : NLev )
     .      = (/( This1 % Temp    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % DewPt   ( : NLev )
     .      = (/( This1 % DewPt   ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Wind    ( : NLev )
     .      = (/( This1 % Wind    ( Indx ( iLev )), iLev = 1, NLev )/)
      else
         This2 % Pres    ( Indx ( : NLev ))
     .      = (/( This1 % Pres    ( iLev ), iLev = 1, NLev )/)
         This2 % Height  ( Indx ( : NLev ))
     .      = (/( This1 % Height  ( iLev ), iLev = 1, NLev )/)
         This2 % Temp    ( Indx ( : NLev ))
     .      = (/( This1 % Temp    ( iLev ), iLev = 1, NLev )/)
         This2 % DewPt   ( Indx ( : NLev ))
     .      = (/( This1 % DewPt   ( iLev ), iLev = 1, NLev )/)
         This2 % Wind    ( Indx ( : NLev ))
     .      = (/( This1 % Wind    ( iLev ), iLev = 1, NLev )/)
      end if

      return
      end subroutine Reorder_QC1

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_QC2 - Rearrange the data in the argument of type IGRA_QC
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_QC2 ( This, Indx, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      integer,              intent (in), dimension (:) ::
     .   Indx             ! Sorting indices
      logical,              intent (in), optional ::
     .   reverse          ! = .true. to reverse the reordering 
                          !   Default: reverse = .false.
!
! !INPUT/OUTPUT PARAMETERS:
      type (IGRA_QC),       intent (inout) ::
     .   This             ! Rearranged data
!
! !REVISION HISTORY:
!     26Mar2007  C Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      call Reorder_QC1 ( This, Indx, This, reverse = reverse )

      return
      end subroutine Reorder_QC2

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_IO1 - Rearrange the data in the argument of type IGRA_IO
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_IO1 ( This1, Indx, This2, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      type (IGRA_IO),       intent (in) ::
     .   This1              ! Data to be rearranged
      integer,              intent (in), dimension (:) ::
     .   Indx               ! Sorting indices
      logical,              intent (in), optional ::
     .   reverse            ! = .true. to reverse the reordering 
                            !   Default: reverse = .false.
!
! !OUTPUT PARAMETERS:
      type (IGRA_IO),       intent (inout) ::
     .   This2              ! Rearranged data
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      integer :: iLev, NLev
      logical :: forward

      forward = .true.
      if ( present ( reverse )) forward = .not. reverse 
      NLev    = size ( Indx )

      if ( forward ) then
         This2 % Pres    ( : NLev )
     .      = (/( This1 % Pres    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Height  ( : NLev )
     .      = (/( This1 % Height  ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Temp    ( : NLev )
     .      = (/( This1 % Temp    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % DewPt   ( : NLev )
     .      = (/( This1 % DewPt   ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Wind    ( : NLev )
     .      = (/( This1 % Wind    ( Indx ( iLev )), iLev = 1, NLev )/)
      else
         This2 % Pres    ( Indx ( : NLev ))
     .      = (/( This1 % Pres    ( iLev ), iLev = 1, NLev )/)
         This2 % Height  ( Indx ( : NLev ))
     .      = (/( This1 % Height  ( iLev ), iLev = 1, NLev )/)
         This2 % Temp    ( Indx ( : NLev ))
     .      = (/( This1 % Temp    ( iLev ), iLev = 1, NLev )/)
         This2 % DewPt   ( Indx ( : NLev ))
     .      = (/( This1 % DewPt   ( iLev ), iLev = 1, NLev )/)
         This2 % Wind    ( Indx ( : NLev ))
     .      = (/( This1 % Wind    ( iLev ), iLev = 1, NLev )/)
      end if

      return
      end subroutine Reorder_IO1

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_IO2 - Rearrange the data in the argument of type IGRA_IO
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_IO2 ( This, Indx, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      integer,              intent (in), dimension (:) ::
     .   Indx             ! Sorting indices
      logical,              intent (in), optional ::
     .   reverse          ! = .true. to reverse the reordering 
                          !   Default: reverse = .false.
!
! !INPUT/OUTPUT PARAMETERS:
      type (IGRA_IO),       intent (inout) ::
     .   This             ! Rearranged data
!
! !REVISION HISTORY:
!     26Mar2007  C Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      call Reorder_IO1 ( This, Indx, This, reverse = reverse )

      return
      end subroutine Reorder_IO2

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_SigT1 - Rearrange the data in the argument of type IGRA_SigT
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_SigT1 ( This1, Indx, This2, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      type (IGRA_SigT),     intent (in) ::
     .   This1              ! Data to be rearranged
      integer,              intent (in), dimension (:) ::
     .   Indx               ! Sorting indices
      logical,              intent (in), optional ::
     .   reverse            ! = .true. to reverse the reordering 
                            !   Default: reverse = .false.
!
! !OUTPUT PARAMETERS:
      type (IGRA_SigT),     intent (inout) ::
     .   This2              ! Rearranged data
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      integer :: iLev, NLev
      logical :: forward

      forward = .true.
      if ( present ( reverse )) forward = .not. reverse 
      NLev    = size ( Indx )

      if ( forward ) then
         This2 % Mask    ( : NLev )
     .      = (/( This1 % Mask    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % LevList ( : NLev )
     .      = (/( This1 % LevList ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % IPres   ( : NLev )
     .      = (/( This1 % IPres   ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Pres    ( : NLev )
     .      = (/( This1 % Pres    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Height  ( : NLev )
     .      = (/( This1 % Height  ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Temp    ( : NLev )
     .      = (/( This1 % Temp    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % DewPt   ( : NLev )
     .      = (/( This1 % DewPt   ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % logP    ( : NLev )
     .      = (/( This1 % logP    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % H       ( : NLev )
     .      = (/( This1 % H       ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % T       ( : NLev )
     .      = (/( This1 % T       ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % VT      ( : NLev )
     .      = (/( This1 % VT      ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % DP      ( : NLev )
     .      = (/( This1 % DP      ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % RH      ( : NLev )
     .      = (/( This1 % RH      ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % MR      ( : NLev )
     .      = (/( This1 % MR      ( Indx ( iLev )), iLev = 1, NLev )/)
      else
         This2 % Mask    ( Indx ( : NLev ))
     .      = (/( This1 % Mask  ( iLev ), iLev = 1, NLev )/)
         This2 % LevList ( Indx ( : NLev ))
     .      = (/( This1 % LevList ( iLev ), iLev = 1, NLev )/)
         This2 % IPres   ( Indx ( : NLev ))
     .      = (/( This1 % IPres   ( iLev ), iLev = 1, NLev )/)
         This2 % Pres    ( Indx ( : NLev ))
     .      = (/( This1 % Pres    ( iLev ), iLev = 1, NLev )/)
         This2 % Height  ( Indx ( : NLev ))
     .      = (/( This1 % Height  ( iLev ), iLev = 1, NLev )/)
         This2 % Temp    ( Indx ( : NLev ))
     .      = (/( This1 % Temp    ( iLev ), iLev = 1, NLev )/)
         This2 % DewPt   ( Indx ( : NLev ))
     .      = (/( This1 % DewPt   ( iLev ), iLev = 1, NLev )/)
         This2 % logP    ( Indx ( : NLev ))
     .      = (/( This1 % logP    ( iLev ), iLev = 1, NLev )/)
         This2 % H       ( Indx ( : NLev ))
     .      = (/( This1 % H       ( iLev ), iLev = 1, NLev )/)
         This2 % T       ( Indx ( : NLev ))
     .      = (/( This1 % T       ( iLev ), iLev = 1, NLev )/)
         This2 % VT      ( Indx ( : NLev ))
     .      = (/( This1 % VT      ( iLev ), iLev = 1, NLev )/)
         This2 % DP      ( Indx ( : NLev ))
     .      = (/( This1 % DP      ( iLev ), iLev = 1, NLev )/)
         This2 % RH      ( Indx ( : NLev ))
     .      = (/( This1 % RH      ( iLev ), iLev = 1, NLev )/)
         This2 % MR      ( Indx ( : NLev ))
     .      = (/( This1 % MR      ( iLev ), iLev = 1, NLev )/)
      end if

      return
      end subroutine Reorder_SigT1

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_SigT2 - Rearrange the data in the argument of type IGRA_SigT
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_SigT2 ( This, Indx, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      integer,              intent (in), dimension (:) ::
     .   Indx             ! Sorting indices
      logical,              intent (in), optional ::
     .   reverse          ! = .true. to reverse the reordering 
                          !   Default: reverse = .false.
!
! !INPUT/OUTPUT PARAMETERS:
      type (IGRA_SigT),     intent (inout) ::
     .   This             ! Rearranged data
!
! !REVISION HISTORY:
!     21Mar2007  C Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      call Reorder_SigT1 ( This, Indx, This, reverse = reverse )

      return
      end subroutine Reorder_SigT2

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_Wind1 - Rearrange the data in the argument of type IGRA_Wind
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_Wind1 ( This1, Indx, This2, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      type (IGRA_Wind),     intent (in) ::
     .   This1              ! Data to be rearranged
      integer,              intent (in), dimension (:) ::
     .   Indx               ! Sorting indices
      logical,              intent (in), optional ::
     .   reverse            ! = .true. to reverse the reordering 
                            !   Default: reverse = .false.
!
! !OUTPUT PARAMETERS:
      type (IGRA_Wind),     intent (inout) ::
     .   This2              ! Rearranged data
!
! !REVISION HISTORY:
!     21Mar2007  C. Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      integer :: iLev, NLev
      logical :: forward

      forward = .true.
      if ( present ( reverse )) forward = .not. reverse 
      NLev    = size ( Indx )

      if ( forward ) then
         This2 % Mask    ( : NLev )
     .      = (/( This1 % Mask    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % LevList ( : NLev )
     .      = (/( This1 % LevList ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % IHeight ( : NLev )
     .      = (/( This1 % IHeight ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Pres    ( : NLev )
     .      = (/( This1 % Pres    ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % Height  ( : NLev )
     .      = (/( This1 % Height  ( Indx ( iLev )), iLev = 1, NLev )/)
         This2 % logP    ( : NLev )
     .      = (/( This1 % logP    ( Indx ( iLev )), iLev = 1, NLev )/)
      else
         This2 % Mask    ( Indx ( : NLev ))
     .      = (/( This1 % Mask    ( iLev ), iLev = 1, NLev )/)
         This2 % LevList ( Indx ( : NLev ))
     .      = (/( This1 % LevList ( iLev ), iLev = 1, NLev )/)
         This2 % IHeight ( Indx ( : NLev ))
     .      = (/( This1 % IHeight ( iLev ), iLev = 1, NLev )/)
         This2 % Pres    ( Indx ( : NLev ))
     .      = (/( This1 % Pres    ( iLev ), iLev = 1, NLev )/)
         This2 % Height  ( Indx ( : NLev ))
     .      = (/( This1 % Height  ( iLev ), iLev = 1, NLev )/)
         This2 % logP    ( Indx ( : NLev ))
     .      = (/( This1 % logP    ( iLev ), iLev = 1, NLev )/)
      end if

      return
      end subroutine Reorder_Wind1

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_Wind2 - Rearrange the data in the argument of type IGRA_Wind
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_Wind2 ( This, Indx, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      integer,              intent (in), dimension (:) ::
     .   Indx             ! Sorting indices
      logical,              intent (in), optional ::
     .   reverse          ! = .true. to reverse the reordering 
                          !   Default: reverse = .false.
!
! !INPUT/OUTPUT PARAMETERS:
      type (IGRA_Wind),     intent (inout) ::
     .   This             ! Rearranged data
!
! !REVISION HISTORY:
!     21Mar2007  C Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      call Reorder_Wind1 ( This, Indx, This, reverse = reverse )

      return
      end subroutine Reorder_Wind2

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_Stations1 - Rearrange the data in the argument of type IGRA_Stations
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_Stations1 ( This1, Indx, This2, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      type (IGRA_Stations), intent (in) ::
     .   This1              ! Data to be rearranged
      integer,              intent (in), dimension (:) ::
     .   Indx               ! Sorting indices
      logical,              intent (in), optional ::
     .   reverse            ! = .true. to reverse the reordering 
                            !   Default: reverse = .false.
!
! !OUTPUT PARAMETERS:
      type (IGRA_Stations), intent (inout) ::
     .   This2              ! Rearranged data
!
! !REVISION HISTORY:
!     14Mar2007  C. Redder  Orininal code
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
         This2 % StnName ( : NStn )
     .      = (/( This1 % StnName ( Indx ( iStn )), iStn = 1, NStn )/)
         This2 % Lat     ( : NStn )
     .      = (/( This1 % Lat     ( Indx ( iStn )), iStn = 1, NStn )/)
         This2 % Lon     ( : NStn )
     .      = (/( This1 % Lon     ( Indx ( iStn )), iStn = 1, NStn )/)
         This2 % Elev    ( : NStn )
     .      = (/( This1 % Elev    ( Indx ( iStn )), iStn = 1, NStn )/)

      else
         This2 % StnID   ( Indx ( : NStn ))
     .      = (/( This1 % StnName ( iStn ), iStn = 1, NStn )/)
         This2 % StnName ( Indx ( : NStn ))
     .      = (/( This1 % StnName ( iStn ), iStn = 1, NStn )/)
         This2 % Lat     ( Indx ( : NStn ))
     .      = (/( This1 % Lat     ( iStn ), iStn = 1, NStn )/)
         This2 % Lon     ( Indx ( : NStn ))
     .      = (/( This1 % Lon     ( iStn ), iStn = 1, NStn )/)
         This2 % Elev    ( Indx ( : NStn ))
     .      = (/( This1 % Elev    ( iStn ), iStn = 1, NStn )/)
      end if

      return
      end subroutine Reorder_Stations1

!..............................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Reorder_Stations2 - Rearrange the data in the argument of type IGRA_Stations
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Reorder_Stations2 ( This, Indx, reverse )
!
! !INPUT PARAMETERS:
      implicit none
      integer,              intent (in), dimension (:) ::
     .   Indx             ! Sorting indices
      logical,              intent (in), optional ::
     .   reverse          ! = .true. to reverse the reordering 
                          !   Default: reverse = .false.
!
! !INPUT/OUTPUT PARAMETERS:
      type (IGRA_Stations), intent (inout) ::
     .   This             ! Rearranged data
!
! !REVISION HISTORY:
!     15Mar2007  C Redder  Orininal code
!
!EOP
!-------------------------------------------------------------------------

      call Reorder_Stations1 ( This, Indx, This, reverse = reverse )

      return
      end subroutine Reorder_Stations2

!...................................................................
      end module m_IGRA
!====================================================================
