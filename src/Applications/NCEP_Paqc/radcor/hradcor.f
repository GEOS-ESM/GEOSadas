!====================================================================

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !PROGRAM: hradcor --  Apply modified RADCOR to soundings
!
! !DESCRIPTION:
!     Apply a modified version of RADCOR to raob soundings.  The
!     correction applied is the NCEP RADCOR correction with the
!     correction at vernal equinox subtracted.
!
!
! !INTERFACE:
!
      program  hradcor

      use m_RadBufr,   only : radcor_profiles, prep_info, prep_options,
     .                        Set_Options, Get_Raobs, Put_Raobs,
     .                        Set_Reason
      use m_AdvError,  only : WDie, ItoA, ItoDate, ErrStat, Alloc
      use m_RadData,   only : radcor_profile, SetupProf,
     .                        UpdateProf, UpdateProfs, SetPath,
     .                        Rad_Init, Rad_Clean, SetDrift,
     .                        ETHist_CARDS
      use m_RadNCEP,   only : ncepTab
      use m_RadTplate, only : StrTemplate
      use m_VaiUtil,   only : Vai_rkxList
      implicit NONE
      character (len=*), parameter :: MyModule = 'hradcor'
      character (len=*), parameter :: MyProg   =  MyModule
      integer, parameter :: No_Error = 0

      type ( prep_options    ) :: PrepOptions
      type ( radcor_profiles ) :: Obs
      type ( prep_info       ) :: Prep
      type ( radcor_profile  ) :: Profile

      character (len=255) :: tplate, prepinfile, outfile, ExpID
      character (len=500) :: Tail 
      integer :: stat, NYMD, NHMS
      integer :: iBeg, iEnd, Len, iSonde, NSondes, NObs_Max

! !REVISION HISTORY:
!     ~2007      C. Redder  Original code
!     23Jan2014  Meta       Add code to set reason code, using a different value
!                           for hradcor and raobcore
! EOP
!-------------------------------------------------------------------------


!     Initialize package
!     ------------------
      call Set_Options ( PrepOptions )
      PrepOptions % ThisProgName = 'RADCOR'
      call Set_Reason ( 102 )

!     Get command-line parameters
!     ---------------------------
      call Init ( tplate, prepinfile )

!     Read data from input data file.
!     -------------------------------
      call Get_Raobs ( prepinfile, Prep, Obs, stat, PrepOptions )
      if ( stat .ne. No_Error ) then
         call WDie ( MyProg, ErrStat ( 'Get_Raobs' ))
      end if

!     Get file atributes for file name generation from templates
!     ----------------------------------------------------------
      iBeg   = scan ( prepinfile, '/', back = .true. ) + 1
       Len   = scan ( prepinfile ( iBeg : ), '_.' ) - 1
      if ( Len .eq. 0 )
     .   Len = len_trim ( prepinfile ( iBeg : ) )
      iEnd   = iBeg + Len - 1
      ExpID  = prepinfile ( iBeg : iEnd )
      NYMD = Obs % Meta % Date
      NHMS = Obs % Meta % Time

!     Supplementary info for error message generation
!     -----------------------------------------------
      Tail     = '\n   Infile      =   '  // trim    ( prepinfile )
     .        // '\n   Date/time   = \C'  // ItoDate ( NYMD, NHMS )

!     Set up data structure to process each profile
!     ---------------------------------------------
      NSondes  = Obs % Meta % Nks
      NObs_Max = 0
      if ( NSondes .gt. 0 )
     .   NObs_Max = maxval ( Obs % Meta % ksLen ( : NSondes ))
      call Rad_Init ( NObs_Max, NObs_Max, Profile, stat )
      if ( stat .ne. No_Error ) then
         call WDie ( MyProg, ErrStat ( 'Rad_Init' ) // Tail )
      end if

      do iSonde = 1, NSondes
         call SetupProf   (  iSonde, Obs,      Profile  )
         call SetDrift             ( Profile )
         call CorrectHT            ( Profile )
         call UpdateProf           ( Profile )
         call UpdateProfs          ( Profile, Obs )
!         if ( write_report (iSonde) .or. Profile % rkx .eq. 51 )
!     .      write (*,'(1x,i3,1x,a,1x,i3,1x,i2 )')
!     .         iSonde, profile%stnid,
!     .         profile%rkx,profile%ethist
c            write (*,
c     .       '(1x,i3,1x,a,1x,i3,1x,2(1x,i2),2(1x,i5 ),2(1x,f7.2))')
c     .         iSonde, profile%stnid,
c     .         profile%rkx,profile%ethist,
c     .         profile%lthist,
c     .         profile%ltime,
c     .         nint(profile%elev),
c     .         profile%lat,
c     .         profile%lon
      end do

!     Generate output file name from template
!     ---------------------------------------
      call StrTemplate  ( outfile, tplate,
     .                    xid  = trim ( ExpID ),
     .                    nymd = NYMD,
     .                    nhms = NHMS, stat = stat )
      if ( stat .ne. 0 ) then
         call WDie ( MyProg, ErrStat ( 'StrTemplate', stat )
     .                    // Tail ) 
      end if

!     Write observation vector to file
!     --------------------------------
c      outfile = tplate
      call Put_Raobs ( outfile, Prep, Obs, stat,
     .                 Options      = PrepOptions )
      if ( stat .ne. 0 ) then
         call WDie ( MyProg, ErrStat ( 'Put_Raobs' ) // Tail )
      end if

      call exit (0)
      contains

!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CorrectHT - Correct and recorrect temperature based on season
!
! !INTERFACE:
      subroutine CorrectHT ( Profile )
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profile ), intent ( inout ) ::
     .   Profile   ! Data for the selected profile (sounding)
!
! !DESCRIPTION:
!     This routine computes the difference between the correction
!     at vernal equinox and that for the synoptic date with
!     common launch (or observation times)
!
! !REVISION HISTORY:
!     12Jun2007  C. Redder  Initial code
!EOP
!.................................................................

      integer :: SYMD, NLev, iLev
      real, dimension (:), pointer :: TCor, TCorAcc


      NLev    =  Profile % by_Levels % NLev
      TCor    => Profile % by_Levels % TCor    ( : NLev )
      TCorAcc => Profile % by_Levels % TCorAcc ( : NLev )

      call CorrectT ( 19980321, Profile )
      do iLev = 1, NLev
         TCorAcc ( iLev ) = TCorAcc ( iLev ) - TCor ( iLev )
      end do
      SYMD = Profile % Date
      call CorrectT ( SYMD,     Profile )
      do iLev = 1, NLev
         TCorAcc ( iLev ) = TCorAcc ( iLev ) + TCor ( iLev )
      end do

      return
      end subroutine CorrectHT
!.................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CorrectT - Correct temperature
!
! !INTERFACE:
      subroutine CorrectT ( SynDate, Profile )
!
! !USES:
      implicit NONE
! !INPUT PARAMETERS:
      integer,                 intent (in)      ::
     .   SynDate   ! Synoptic date (YYYYMMDD format)
!
! !INPUT/OUTPUT PARAMETERS:
      type ( radcor_profile ), intent ( inout ) ::
     .   Profile   ! Data for the selected profile (sounding)
!
! !DESCRIPTION:
!     This routine computes the correction for a given synoptic
!     date
!
! !REVISION HISTORY:
!     15Jun2007  C. Redder  Initial code
!EOP
!.................................................................

      integer, parameter ::
     .   Vai_rkxListSz = size        ( Vai_rkxList   ),
     .   Vai_rkxMin    = Vai_rkxList ( 1 ),
     .   Vai_rkxMax    = Vai_rkxList ( Vai_rkxListSz )
      logical ::
     .   rkx_flag_init = .false.,
     .   rkx_Vaisala ( Vai_rkxMin : Vai_rkxMax ) = .false.
      logical :: Vaisala
      integer :: irkx, rkx, SYMD, NLev, iLev
      real, dimension (:), pointer :: P, TCor, SE

!     Initialize logical flags for a Vaisala types
!     --------------------------------------------
      if ( .not. rkx_flag_init ) then
          do irkx = 1, Vai_rkxListSz  
             rkx_Vaisala ( Vai_rkxList ( irkx )) = .true.
          end do
          rkx_flag_init = .true.
      end if

!     Get and/or locate the necessary data
!     ------------------------------------
      rkx     =  Profile % rkx
      NLev    =  Profile % by_Levels % NLev
      P       => Profile % by_Levels % P       ( : NLev )
      SE      => Profile % by_Levels % SE      ( : NLev )
      TCor    => Profile % by_Levels % TCor    ( : NLev )

!     Determine if the radiosonde is a Vaisala (RS-80 or later)
!     ---------------------------------------------------------
      Vaisala = .false.
      if ( rkx .ge. Vai_rkxMin .and.
     .     rkx .le. Vai_rkxMax ) Vaisala = rkx_Vaisala ( rkx )

!     No correction if it is a Vaisala instrument
!     -------------------------------------------
      if      ( Vaisala ) then
         TCor =  0.0

!     ... or Chinese GZZ radiosonde (launched in 2001 or later)
!     ---------------------------------------------------------
      else if ( rkx .eq. 32 .and.
     .          SynDate .ge. 20010101 ) then
         TCor =  0.0

      else

!        ... otherwise compute the solar elevation
!            angle and radcor correction
!        -----------------------------------------
         SYMD           = Profile % Date
         Profile % Date = SynDate
         call SetPath ( Profile )
         call ncepTab ( rkx, P, SE, TCor, PCutoff = .true. )
         Profile % Date = SYMD

      end if

      return
      end subroutine CorrectT
!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Init() --- Initializer
!
! !DESCRIPTION:
!     This propgram reads command line arguments to come up with several
!     operating parameters. For command line syntax, see below
!
! !INTERFACE: 
      subroutine Init ( tplate, prepinfile )
      use m_AdvError, only : Arr,  WDie
      use m_AtoX,     only : AtoI, AtoXErr
      use m_TextUtil, only : LowerCase
      use m_RadBufr,  only : Bufr_Inq
!
! !OUTPUT PARAMETERS:
      implicit    NONE
      character (len=*), intent (out) ::
     .   tplate,         ! The template for the output filenames.
     .   prepinfile      ! The input filenames.
      character (len=*), parameter ::
     .   Usage = '\n\H{12}'
     .   // MyProg // '.x [ -o TPLATE ] prepfile \n\L{4}'
     .   // '\n'
     .   // 'Required parameters: \n'
     .   // '   prepfile  - \H{}name of input prep file \n'
     .   // '\n'
     .   // 'Optional parameters: \n'
     .   // '   -o TPLATE - Specify GrADS-like template to TPLATE '
     .   //                'for output file name generation.  The '
     .   //                'following descriptors are allowed: %s, '
     .   //                '%y4, %m2, %m3, %d2, and %h2.  See the '
     .   //                'module, m_RadTplate, for a complete '
     .   //                'list of valid descriptors and their '
     .   //                'meanings. Default: %s.' // MyProg
     .   //                '.%y4%m2%d2 where %s is replaced by the '
     .   //                'experiment ID as derived from the input '
     .   //                'filename. '
!
! \label{Sample:Init}
!
! !BUGS:
!
! !SEE ALSO: 
!
! !FILES USED:  
!
! !REVISION HISTORY:
!     22Aug2006  C. Redder  Original code.
!EOP
!-------------------------------------------------------------------------

      character (len=*), parameter :: MyName = MyModule // '::Init'

!     Size of buffer and file names
!     -----------------------------
      integer     BufSize
      parameter ( BufSize = 255 )

!     Functions referenced for ...
!     ----------------------------
      integer     IArgC ! ... extracting the number of command-line arguments

!     Extracting command-line input
!     -----------------------------
      integer     NArg, iArg
      integer     iArg_TPlate, iArg_InFile
      character   Buffer * ( BufSize )
      integer     len_buf
      logical     in_list

!     Other variables
!     ---------------
      integer     stat, nfiles

!...........................................

!     Number of arguments in command-line input
!     -----------------------------------------
      NArg = IArgC ()
      if ( NArg .le. 0 ) call WDie ( 'Usage', Usage )

!     Initialize
!     ----------
      iArg_TPlate   =  0
      iArg          =  0
      nfiles        =  0

!     Scan for arguments
!     ------------------
      do while ( iArg .lt. NArg ) 
         iArg = iArg + 1
         call GetArg ( iArg, Buffer )

!        Option for ...
!        --------------

!        ... setting the template for the output filenames
!        -------------------------------------------------
         if      ( Buffer .eq. '-o'         ) then
            iArg          =  iArg + 1
            iArg_TPlate   =  iArg

!        A dash by itself produces an error
!        ----------------------------------
         else if ( Buffer ( : 1 ) .eq. '-'  ) then
            len_buf = Len_Trim ( Buffer )
            call WDie ( MyName, 'Illegal option, '
     .                        // Buffer ( : len_buf ),
     .                           Usage = Usage )

!        Non-optional argument is assumed to be an input filename
!        --------------------------------------------------------
         else
            iArg_InFile = iArg
            nfiles      = nfiles + 1
         end if
      end do

!     Check to ensure that there is only one filename.
!     ------------------------------------------------
      if      ( nfiles .lt. 1 ) then
         call WDie ( MyName, 'No input prep filenam specified.',
     .               Usage = Usage )
      else if ( nfiles .gt. 1 ) then
         call WDie ( MyName, 'Only one input prep file is allowed.',
     .               Usage = Usage )
      else
         call GetArg ( iArg_InFile, Buffer )
      end if
      prepinfile = Buffer

!     Check to determine if the command line input ends
!     with an option expecting another argument
!     -------------------------------------------------
      if ( iArg_TPlate  .gt. NArg ) then
         call WDie ( MyName, 'Command-line input ends with an '
     .                    // 'option expecting another argument',
     .               Usage = Usage )
      end if

!     Get the output file tamplate
!     ----------------------------
      tplate = '%s.' // MyProg // '.%y4%m2%d2_%h2Z'
      if ( iArg_TPlate .gt. 0 ) then
         call GetArg ( iArg_TPlate, Buffer )
         if ( Buffer ( : 1 ) .eq. "-" ) then
            call WDie ( MyName,  'Illegal file name '
     .                         // Trim ( Buffer ), Usage = Usage )
         end if
         tplate = Buffer
      end if

      return
      end subroutine Init
!...................................................................
      end program hradcor
!====================================================================
