!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_RadDataF -- Utilities for extracting raob data from data files
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      module m_RadDataF
      use m_RadData, only : radcor_profiles
!ods      use m_RadODS,  only : ods_info
      use m_RadBufr, only : prep_info, prep_options
      implicit    NONE
      private   ! except

      public ::
     .   Inq_Raobs,          ! Inquire routine for obtaining obs
     .   Get_Raobs,          ! Get raob data
     .   Put_Raobs,          ! Put raob data to buffer file
     .   Clean_Raobs,        ! Clean up data structures 
     .   Set_Options,        ! Set the options in file_options.
     .   file_info,          ! File information
     .   file_options,       ! Options for reading/writing to file
     .   radcor_profiles     ! Radcor data structure

      interface Clean_Raobs
         module procedure
     .      Clean_Raobs_
      end interface
      interface Inq_Raobs
         module procedure
     .      Inq_Raobs_
      end interface
      interface Get_Raobs
         module procedure
     .      Get_Raobs_
      end interface
      interface Put_Raobs
         module procedure
     .      Put_Raobs_
      end interface
      interface Set_Options
         module procedure
     .       Init_Options,
     .       Copy_Options
      end interface
! 
! !REVISION HISTORY:
!     16Apr2003  C. Redder  Original code
!     13Nov2003  C. Redder  Replaced the data structure radcor with
!                           radcor_profiles.
!     20Apr2004  C. Redder  Added generic interface, SetOptions, and
!                           data structure, file_options
!EOP
!-----------------------------------------------------------------

      character (len=*), parameter :: MyModule = 'm_RadDataF'

      type file_info
!ods         type (  ods_info    ) :: ODS
         type ( prep_info    ) :: Prep
      end type

      type file_options
         type ( prep_options ) :: Prep
      end type

      contains
!....................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Init_Options () -- initialize option flags for reading/writing to data files
!
! !DESCRIPTION:
!     The routine initializes the contents of the input data structure.
!
! !INTERFACE:
      subroutine Init_Options ( this )
      use m_RadBufr, only : Set_PrepOpt => Set_Options
      implicit   NONE
!
! !OUTPUT PARAMETERS:
      type ( file_options ), intent   (out), optional ::
     .   this              ! Output data structure.
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     20Apr2004  C. Redder   Origional code
!
! EOP
!-------------------------------------------------------------------------

      call Set_PrepOpt ( this % Prep )

      return
      end subroutine Init_Options

!....................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Copy_Options () --- Copy option flags for reading/writing to data files
!
! !DESCRIPTION:
!     The routine copies the contents from the input data structure to
!     the output data structure if it is present
!
! !INTERFACE:
      subroutine Copy_Options ( this_in, this_out )
      use m_RadBufr, only : Set_PrepOpt => Set_Options
!
! !INPUT PARAMETERS:
      implicit   NONE
      type ( file_options ), intent (in)  ::
     .   this_in 
!
! !OUTPUT PARAMETERS:
      type ( file_options ), intent (out) ::
     .   this_out            ! Output data structure.
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     20Apr2004  C. Redder   Origional code
!
! EOP
!-------------------------------------------------------------------------

      call Set_PrepOpt ( this_in % Prep, this_out % Prep )

      return
      end subroutine Copy_Options

!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Inq_Raobs_ () --- Inquire data from data file (ODS or Prep)
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Inq_Raobs_ ( File, Type, stat, NSyn, DTBeg, DTEnd )
!ods      use m_RadODS,   only : ODS_Inq,  Get_ODS  => Get_Raobs
      use m_RadBufr,  only : Bufr_Inq, Get_Prep => Get_Raobs
      use m_AdvError, only : ErrStat,  Alloc, WPErr, ItoDate
      use m_TextUtil, only : LowerCase
!
! !INPUT PARAMETERS:
      implicit   NONE
      character (len=*),  intent (in)  ::
     .   File,            ! Data filename
     .   Type             ! File type (i.e. 'ODS' or 'Prep',
     .                    !   case insensitive)
!
! !OUTPUT PARAMETERS:
      integer, optional,  intent (out) ::
     .   NSyn,            ! Number of synoptic times per day
     .   DTBeg, DTEnd     ! Date/time tag (in YYYYMMHHHH format) for the
     .                    ! ... latest synoptic Julian hour on file
      integer,            intent (out) ::
     .   stat             ! Returned status code
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     03Jul2003  C. Redder   Original code
!     19Feb2004  C. Redder   Made file type case insensitive
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName  = MyModule // '::Inq_Raobs_'

      integer :: DTBeg_, DTEnd_, NSyn_
      character ( len = 500 ) :: Tail
      character ( len =  80 ) :: Prep_Type, ODS_Type = "!?*", Type_

      Tail = '\n   file  = \C'   // File

!     Get parameters
!     --------------
      call Bufr_Inq ( Prep_Type = Prep_Type )    
!ods      call ODS_Inq  ( ODS_Type  = ODS_Type  )
      Prep_Type = LowerCase ( Prep_Type )
!ods      ODS_Type  = LowerCase (  ODS_Type )

!     Get data from the ODS file
!     --------------------------
      Type_ = LowerCase ( Type )
      if      ( Type_ .eq. ODS_Type  ) then
!ods         call ODS_Inq  ( File, stat, NSyn  = NSyn_,
!ods     .                               DTBeg = DTBeg_,
!ods     .                               DTEnd = DTEnd_ )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'ODS_Inq',  stat ))
            return
         end if

!     ... or "Prep" data file
!     -----------------------
      else if ( Type_ .eq. Prep_Type ) then
         call Bufr_Inq ( File, stat, SynTag = DTBeg_ )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'Bufr_Inq', stat ))
            return
         end if
         DTEnd_ = DTBeg_
         NSyn_  = 1

!     ... and other type results in an error
!    ---------------------------------------
      else
         call WPErr ( MyName, 'Invalid file type, ' // trim ( Type )
     .                      // Tail ) 
         stat = 1
         return
      end if

      if ( present ( NSyn  ))  NSyn  = NSyn_
      if ( present ( DTBeg ))  DTBeg = DTBeg_
      if ( present ( DTEnd ))  DTEnd = DTEnd_

      return
      end subroutine Inq_Raobs_
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Clean_Raobs_ () --- Clean up data structure (ODS or Prep)
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Clean_Raobs_ ( FInfo, Obs, stat )
!ods      use m_RadODS,   only : ODS_Inq,  Clean_ODS  => Clean_Raobs
      use m_RadBufr,  only : Bufr_Inq, Clean_Prep => Clean_Raobs
      use m_RadData,  only : radcor_profiles
      use m_AdvError, only : ErrStat,  Alloc, WPErr, ItoDate
      use m_TextUtil, only : LowerCase
!
! !INPUT/OUTPUT PARAMETERS:
      type ( file_info ),       intent (inout) ::
     .   FInfo                  ! Information about the data file which is
                                !   used to write the data to the input file
      type ( radcor_profiles ), intent (inout) ::
     .   Obs                    ! Raob data structure
! !OUTPUT PARAMETERS:
      integer,                  intent (out)   ::
     .   stat                   ! Returned status code
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     03Jul2003  C. Redder   Original code
!     13Nov2003  C. Redder   Replaced the data structure radcor with
!                            radcor_profiles.
!     19Feb2004  C. Redder   Made file type case insensitive
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName  = MyModule // '::Clean_Raobs_'
      character ( len = 500 ) :: Tail
      character ( len =  80 ) :: Prep_Type, ODS_Type = "!?*", Type

      Tail = '\n   Input file  = \C'    // trim ( Obs % ObsFile )

!     Get parameters
!     --------------
      call Bufr_Inq ( Prep_Type = Prep_Type )
!ods      call ODS_Inq  ( ODS_Type  = ODS_Type  )
      Prep_Type = LowerCase ( Prep_Type )
!ods      ODS_Type  = LowerCase (  ODS_Type )

!     Get data from the ODS file
!     --------------------------
      Type = LowerCase ( Obs % FileType )
      if      ( Type .eq. ODS_Type  ) then
!ods         call Clean_ODS  ( FInfo % ODS, Obs, stat )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'Clean_ODS',  stat ))
            return
         end if

!     ... or "Prep" data file
!     -----------------------
      else if ( Type .eq. Prep_Type ) then
         call Clean_Prep ( FInfo % Prep, Obs, stat )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'Clean_Prep', stat ))
            return
         end if

!     ... and other type results in an error
!    ---------------------------------------
      else
         call WPErr ( MyName, 'Invalid file type, ' // trim ( Type )
     .                      // Tail ) 
         stat = 1
         return
      end if

      return
      end subroutine Clean_Raobs_
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Get_Raobs_ () --- Read data from input file (ODS or Prep)
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Get_Raobs_ ( File, Type, NYMDH, FInfo, Obs, stat,
     .                        Options )
!ods      use m_RadODS,    only : ODS_Inq,  Get_ODS  => Get_Raobs
      use m_RadBufr,   only : Bufr_Inq, Get_Prep => Get_Raobs
      use m_RadData,   only : radcor_profiles
      use m_AdvError,  only : ErrStat,  Alloc, WPErr, ItoDate
      use m_TextUtil,  only : LowerCase
!
! !INPUT PARAMETERS:
      implicit   NONE
      character (len=*),        intent (in)    ::
     .   File,                  ! Input filename 
     .   Type                   ! File type (i.e. 'ODS' or 'Prep', 
     .                          !   case insensitive)
      integer,                  intent (in)    ::
     .   NYMDH                  ! Synoptic date and time for the desired
                                !   data, in YYYYMMDDHH format.
      type ( file_options),     intent (in), optional ::
     .   Options                ! Options for reading/writing.
!
! !OUTPUT PARAMETERS:
      type ( file_info ),       intent (inout) ::
     .   FInfo                  ! Information about the data file which
                                !   is used to write the data to the
                                !   input file
      type ( radcor_profiles ), intent (inout) ::
     .   Obs                    ! Raob data structure
      integer,                  intent (out)   ::
     .   stat                   ! Returned status code
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     03Jul2003  C. Redder   Original code
!     13Nov2003  C. Redder   Replaced the data structure radcor with
!                            radcor_profiles.
!     19Feb2004  C. Redder   Made file type case insensitive
!     04Mar2004  C. Redder   Added optional arguments, PName and 
!                            updated_QC.
!     01Apr2004  C. Redder   Removed the optional arguments, PName and
!                            updated_QM and added the optional argument,
!                            Options.
!     01Jun2004  C. Redder   Fixed bug in implementing options
!
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter ::
     .   MyName  = MyModule // '::Get_Raobs_'
      type ( file_options )   :: Opt
      integer :: NYMD, NHMS
      character ( len = 500 ) :: Tail
      character ( len =  80 ) :: Prep_Type, ODS_Type = "!?*", Type_

      NYMD = NYMDH / 100
      NHMS = mod ( NYMDH, 100 ) * 10000

      Tail = '\n   Date/time = \C' // ItoDate ( NYMD, NHMS )
     .    // '\n   File      = '   // File

!     Get parameters
!     --------------
      call Bufr_Inq ( Prep_Type = Prep_Type )    
!ods      call ODS_Inq  ( ODS_Type  = ODS_Type  )
      Prep_Type = LowerCase ( Prep_Type )
!ods      ODS_Type  = LowerCase (  ODS_Type )

!     Implement options
!     -----------------
      call Init_Options ( Opt )
      if ( present ( Options )) call Copy_Options ( Options, Opt )

!     Get data from the ODS file
!     --------------------------
      Type_     = LowerCase ( Type )
      if      ( Type_ .eq. ODS_Type  ) then
!ods         call Get_ODS  ( File, NYMD, NHMS, FInfo % ODS,  Obs, stat )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'Get_ODS',  stat ))
            return
         end if

!     ... or "Prep" data file
!     -----------------------
      else if ( Type_ .eq. Prep_Type ) then
         call Get_Prep ( File, NYMD, NHMS, FInfo % Prep, Obs, stat,
     .                   Options = Opt % Prep )
c         call Get_Prep ( File, NYMDH, FInfo % Prep, Obs, stat )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'Get_Prep', stat ))
            return
         end if

!     ... and other type results in an error
!    ---------------------------------------
      else
         call WPErr ( MyName, 'Invalid file type, ' // trim ( Type )
     .                      // Tail ) 
         stat = 1
         return
      end if

      return
      end subroutine Get_Raobs_
!.................................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Put_Raobs_ () --- Write data to output file (ODS or Prep)
!
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine Put_Raobs_ ( Tplate, NYMDH, FInfo, Obs, stat,
     .                        ExpID,  cleanup, Options )
!ods      use m_RadODS,    only : ODS_Inq,  Put_ODS  => Put_Raobs
      use m_RadBufr,   only : Bufr_Inq, Put_Prep => Put_Raobs
      use m_RadData,   only : radcor_profiles
      use m_AdvError,  only : ErrStat,  Alloc, WPErr, ItoDate
      use m_RadTplate, only : StrTemplate
      use m_TextUtil,  only : LowerCase
!
! !INPUT PARAMETERS:
      implicit   NONE
      integer,                  intent (in)    ::
     .   NYMDH                  ! Synoptic date and time for the
!                               !    desired data, in YYYYMMDDHH format.
      character ( len = * ),    intent (in), optional ::
     .   ExpID            
      logical, optional,        intent (in)    ::
     .   cleanup                ! = .true. to deallocate and reset 
                                !    input data structure.  Default:
                                !    cleanup = .false.
      type ( file_options ),    intent (in), optional ::
     .   Options                ! Other file options for writing
! !INPUT/OUTPUT PARAMETERS:
      character (len=*),        intent (inout) ::
     .   Tplate                 ! On input,  template of output filename. 
                                ! On output, generated output filename 
      type ( file_info ),       intent (inout) ::
     .   FInfo                  ! Information about the data file which
                                !   is used to write the data to the
                                !   output file
      type ( radcor_profiles ), intent (inout) ::
     .   Obs                    ! Raob data structure
                                !   On output, both data structures are
                                !   reset if the optional arguent,
!                               !   cleanup = .true.
! !OUTPUT PARAMETERS:
      integer,                  intent (out)   ::
     .   stat                   ! Returned status code
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!     14Jul2003  C. Redder   Original code
!     13Nov2003  C. Redder   Replaced the data structure radcor with
!                            radcor_profiles.
!     19Feb2004  C. Redder   Made file type case insensitive
!     01Jun2004  C. Redder   Fixed bug in implementing options
!
! EOP
!-------------------------------------------------------------------------

      character (len=*), parameter ::
     .   MyName  = MyModule // '::Put_Raobs_'
      type ( file_options ) :: Opt
      integer :: NYMD, NHMS, iBeg, Len, iEnd
      logical :: set_id
      character ( len = 255 ) :: InFile, OutFile, ExpID_
      character ( len = 500 ) :: Tail
      character ( len =  80 ) :: Prep_Type, ODS_Type = "!?*", Type

      NYMD   = NYMDH / 100
      NHMS   = mod ( NYMDH, 100 ) * 10000
      InFile  = Obs % ObsFile
      Tail   = '\n   Date/time     = \C'  // ItoDate ( NYMD, NHMS )
     .      // '\n   Input file    = '    // trim ( InFile )
     .      // '\n   File template = '    // trim ( Tplate )

!     Implement options
!     -----------------
      call Init_Options ( Opt )
      if ( present ( Options )) call Copy_Options ( Options, Opt )

!     Get the ID tag
!     --------------
      if ( present ( ExpID )) then
         ExpID_ = ExpID
      else
         iBeg   = scan ( InFile, '/', back = .true. ) + 1
          Len   = scan ( InFile ( iBeg : ), '.' ) - 1
         if ( Len .eq. 0 )
     .      Len = len_trim ( InFile ( iBeg : ) )
         iEnd   = iBeg + Len - 1
         ExpID_ = InFile ( iBeg : iEnd )
      end if

!     Generate file name from template
!     --------------------------------
      call StrTemplate  ( OutFile, TPlate,
     .                    xid  = trim ( ExpID_ ),
     .                    nymd = NYMD,
     .                    nhms = NHMS, stat = stat )
      if ( stat .ne. 0 ) then
          call WPErr ( MyName, ErrStat ( 'StrTemplate', stat )
     .                      // Tail ) 
          return
      end if

      Tail   = '\n   Date/time   = \C'  // ItoDate ( NYMD, NHMS )
     .      // '\n   Input file  = '    // trim ( InFile  )
     .      // '\n   Output file = '    // trim ( OutFile )

!     Get parameters
!     --------------
      call Bufr_Inq ( Prep_Type = Prep_Type )    
!ods      call ODS_Inq  ( ODS_Type  = ODS_Type  )
      Prep_Type = LowerCase ( Prep_Type )
!ods      ODS_Type  = LowerCase (  ODS_Type )

!     Write data to the ODS file
!     --------------------------
      Type = LowerCase ( Obs % FileType )
      if      ( Type .eq. ODS_Type  ) then
!ods         call Put_ODS  ( OutFile, NYMD, NHMS, FInfo % ODS,  Obs, stat,
!ods     .                   cleanup = cleanup )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'Put_ODS',  stat ))
            return
         end if

!     ... or "Prep" data file
!     -----------------------
      else if ( Type .eq. Prep_Type ) then
         call Put_Prep ( OutFile, NYMD, NHMS, FInfo % Prep, Obs, stat,
     .                   Options = Opt % Prep, 
     .                   cleanup = cleanup )
         if ( stat .ne. 0 ) then
            call WPErr ( MyName, ErrStat ( 'Put_Prep', stat ))
            return
         end if

!     ... and other type results in an error
!    ---------------------------------------
      else
         call WPErr ( MyName, 'Invalid file type, ' // trim ( Type )
     .                      // Tail ) 
         stat = 1
         return
      end if

      stat   = 0
      Tplate = OutFile
      return
      end subroutine Put_Raobs_
!.................................................................
      end module m_RadDataF
!====================================================================
