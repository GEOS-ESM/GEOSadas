!====================================================================

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !PROGRAM: raobcore -- Correct temperatures based on the scheme of Haimberger (2007)
!
! !DESCRIPTION:
!   Haimberger, L., 2007:  Homogenization of Radiosonde Temperature Time
!   Series Using Innovation Statistics, Journal of Climate, in press.
!
! !INTERFACE:
!
      program  raobcore

      use m_RadBufr,   only : radcor_profiles, prep_info, prep_options,
     .                        Set_Options, Get_Raobs, Put_Raobs,
     .                        Set_Reason
      use m_AdvError,  only : WDie, ItoA, ItoDate, ErrStat, Alloc
      use m_RaobCore,  only : RaobCore_SynDAdj, RaobCore_StnAdj,
     .                        RaobCore_StnAdj1, RC_Get, RC_StnAdj,
     .                        RC_StnAdj1
      use m_RadData,   only : radcor_profile, SetupProf,
     .                        UpdateProf, UpdateProfs, SetPath,
     .                        Rad_Init, Rad_Clean, SetDrift
      use m_RadTplate, only : StrTemplate
      implicit NONE
      character (len=*), parameter :: MyModule = 'raobcore'
      character (len=*), parameter :: MyProg   =  MyModule
      integer, parameter :: No_Error = 0

      type ( prep_options    ) :: PrepOptions
      type ( radcor_profiles ) :: Obs
      type ( prep_info       ) :: Prep
      type ( radcor_profile  ) :: Profile

      character (len=255) :: tplate,  rtplate, rfile, prepinfile,
     .                       outfile, ExpID
      character (len=500) :: Tail 
      integer :: stat, nymd, nhms
      integer :: iBeg, iEnd, Len, iSonde, NSondes, NObs_Max
      logical :: use_fdrift, check_adj

!     Variables for processing RaobCore data file
!     -------------------------------------------
      type ( RaobCore_SynDAdj ) :: SynDAdj
      type ( RaobCore_StnAdj  ) :: StnAdj
      type ( RaobCore_StnAdj1 ) :: StnAdj1

! !REVISION HISTORY:
!     ~2007      C. Redder  Original code
!     23Jan2014  Meta       Add code to set reason code, using a different value
!                           for hradcor and raobcore
! EOP
!-------------------------------------------------------------------------

!     Initialize package
!     ------------------
      call Set_Options ( PrepOptions )
      PrepOptions % ThisProgName   = 'RADCOR'
      PrepOptions % write_drift    = .true.
      call Set_Reason ( 101 )

!     Get command-line parameters
!     ---------------------------
      call Init ( tplate, rtplate, prepinfile )

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

!     Generate RaobCore file name from template
!     -----------------------------------------
      call StrTemplate  ( rfile, rtplate,
     .                    xid  = trim ( ExpID ),
     .                    nymd = NYMD,
     .                    nhms = NHMS, stat = stat )
      if ( stat .ne. 0 ) then
         call WDie ( MyProg, ErrStat ( 'StrTemplate', stat )
     .                    // Tail ) 
      end if

!     ... get the RaobCore data
!     -------------------------
      check_adj = .false.
      call RC_Get ( rfile, NYMD, SynDAdj, stat,
     .              check_adj = check_adj )
      if ( stat .ne. No_Error ) then
         call WDie ( MyProg, ErrStat ( 'RC_Get', stat )
     .                    // Tail )
      end if
      Tail = trim ( Tail ) // '\n   RaobCore file  = ' // rfile

!     ... for each station listed in the prep buffer file
!     ---------------------------------------------------
      call RC_StnAdj ( Obs % Meta % StnID ( : NSondes ),
     .                 Obs % Meta % Lat   ( : NSondes ),
     .                 Obs % Meta % Lon   ( : NSondes ),
     .                 SynDAdj,
     .                 StnAdj1, StnAdj, stat )
      if ( stat .ne. No_Error ) then
         call WDie ( MyProg, ErrStat ( 'RC_StnAdj', stat )
     .                    // Tail )
      end if

      use_fdrift  = .true.
      do iSonde = 1, NSondes
         call SetupProf   ( iSonde, Obs, Profile  )
         call RC_StnAdj1  ( iSonde, StnAdj, StnAdj1 )
         if ( use_fdrift    ) then
            call SetDrift          ( Profile,
     .                               use_fdata = use_fdrift )
            call SetPath           ( Profile )
         end if
         call CorrectT             ( StnAdj1, Profile )
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
! !ROUTINE:  CorrectT - Determine temperature adjustments for the given profile
!
! !INTERFACE:
      subroutine CorrectT ( StnAdj1, Profile )
      use m_soundings, only : LogP2Y
!
! !USES:
      implicit NONE
!
! !INPUT/OUTPUT PARAMETERS:
      type ( RaobCore_StnAdj1 ), intent ( inout ) ::
     .   StnAdj1   ! Adjustment data.  On output, StnAdj1 % Adj is set
      type ( radcor_profile   ), intent ( inout ) ::
     .   Profile   ! Data for the selected profile (sounding)
!
! !DESCRIPTION:
!     This routine applying the temperature corrections generated
!     by the scheme of Haimberger (2007)
!
! !REVISION HISTORY:
!     13Mar2007  C. Redder  Initial code
!     12Jul2007  C. Redder  Added temporary fix to ensure that the 
!                           temperature adjustments at lower levels is
!                           zero.  
!EOP
!.................................................................

      real, parameter :: P_RC_Max = 924.99 ! mb
      integer :: Hour, NLevels_RC, iLev, NLevels
      real    :: Fact, AdjDel, PP
      real, dimension (:), pointer :: Adj, Adj_00Z, Adj_12Z, TCor,
     .                                TCorAcc, P_RC, P

      NLevels_RC =  StnAdj1 %  NLevels
      P_RC       => StnAdj1 %   Levels ( : NLevels_RC )
      Adj_00Z    => StnAdj1 %  Adj_00Z ( : NLevels_RC )
      Adj_12Z    => StnAdj1 %  Adj_12Z ( : NLevels_RC )
      Hour       =  Profile % Time / 10000

!     Determine the interpolation factor for the given synoptic hour
!     --------------------------------------------------------------
      if      ( Hour .eq. 23 .or.  Hour .eq.  0 ) then
         Fact =  0.0
      else if ( Hour .eq. 11 .or.  Hour .eq. 12 ) then
         Fact =  1.0
      else if ( Hour .ge.  1 .and. Hour .le. 10 ) then
         Fact =  real ( Hour )      / real ( 11 )
      else if ( Hour .ge. 13 .and. Hour .le. 22 ) then
         Fact =  real ( 23 - Hour ) / real ( 11 )
      end if

!     ... and interpolate (in time)
!     -----------------------------
      Adj     => StnAdj1 % Adj     ( : NLevels_RC )
      do iLev = 1, NLevels_RC

!        Set all temperature adjustments at a below 925 mb to zero (tempo-
!        rary fix to a bug in the software generating the temp adjustments)
!        ------------------------------------------------------------------
         if ( P_RC ( iLev ) .gt. P_RC_Max ) then
            Adj ( iLev ) = 0.0
         else
            AdjDel       = Adj_12Z ( iLev ) - Adj_00Z ( iLev )
            Adj ( iLev ) = Adj_00Z ( iLev ) + Fact * AdjDel
         end if
      end do

      NLevels    =  Profile % by_Levels % NLev
      P          => Profile % by_Levels % P       ( : NLevels )
      TCor       => Profile % by_Levels % TCor    ( : NLevels )
      TCorAcc    => Profile % by_Levels % TCorAcc ( : NLevels )

!     Determine the adjustments at all levels
!     (mandatory and significant) in the profile
!     ------------------------------------------
      call logP2Y ( P_RC, Adj, P, TCor, YExt = TCorAcc )
      do iLev = 1, NLevels
         TCorAcc ( iLev ) = TCorAcc ( iLev ) + TCor ( iLev )
      end do

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
      subroutine Init ( tplate, rtplate, prepinfile )
      use m_AdvError, only : Arr,  WDie
      use m_AtoX,     only : AtoI, AtoXErr
      use m_TextUtil, only : LowerCase
      use m_RadBufr,  only : Bufr_Inq
!
! !OUTPUT PARAMETERS:
      implicit    NONE
      character (len=*), intent (out) ::
     .   tplate,         ! The template for the output filenames.
     .   rtplate,        ! ... for RaobCore data file
     .   prepinfile      ! The input filenames.
      character (len=*), parameter ::
     .    tplate_Def =  '%s.' // MyProg // '.%y4%m2%d2_%h2Z',
     .   rtplate_Def =  'biascor-1.4.t'
      character (len=*), parameter ::
     .   Usage = '\n\H{12}'
     .   // MyProg // '.x [ -o TPLATE -r RTPLATE ] '
     .   //            'prepfile \n\L{4}'
     .   // '\n'
     .   // 'Required parameters: \n'
     .   // '   prepfile(s) - \H{}name of input prep file \n'
     .   // '\n'
     .   // 'Optional parameters: \n'
     .   // '   -o TPLATE -   Specify GrADS-like template to TPLATE '
     .   //                  'for output file name generation.  The '
     .   //                  'following descriptors are allowed: %s, '
     .   //                  '%y4, %m2, %m3, %d2, and %h2.  See the '
     .   //                  'module, m_RadTplate, for a complete '
     .   //                  'list of valid descriptors and their '
     .   //                  'meanings. Default: ' // tplate_Def
     .   //                  'where %s is replaced by the experiment '
     .   //                  'ID as derived from the input filename. \n'
     .   // '   -r RTPLATE -  Specify GrADS-like template to RTPLATE '
     .   //                  'for RaobCore input file name generation'
     .   //                  'for bias adjustment information. '
     .   //                  'Default: ' // rtplate_Def
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
!     20Feb2007  C. Redder  Original code.
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
      integer     iArg_TPlate, iArg_RTPlate, iArg_InFile
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
      iArg_RTPlate  =  0
      iArg          =  0
      nfiles        =  0

!     Scan for arguments
!     ------------------
      do while ( iArg .lt. NArg ) 
         iArg = iArg + 1
         call GetArg ( iArg, Buffer )

!        Option for ...
!        --------------

!        ... setting the template of the RaobCore input file
!        ---------------------------------------------------
         if      ( Buffer .eq. '-r'         ) then
            iArg          =  iArg + 1
            iArg_RTPlate  =  iArg

!        ... setting the template for the output filenames
!        -------------------------------------------------
         else if ( Buffer .eq. '-o'         ) then
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
         call WDie ( MyName, 'No input prep filename specified.',
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
      if ( iArg_TPlate  .gt. NArg .or.
     .     iArg_RTPlate .gt. NArg ) then
         call WDie ( MyName, 'Command-line input ends with an '
     .                    // 'option expecting another argument',
     .               Usage = Usage )
      end if

!     Get name of input RaobCore input file
!     -------------------------------------
      rtplate = rtplate_Def
      if ( iArg_RTPlate .gt. 0 ) then
         call GetArg ( iArg_RTPlate, Buffer )
         if ( Buffer ( : 1 ) .eq. "-" ) then
            call WDie ( MyName,  'Illegal file name '
     .                         // Trim ( Buffer ), Usage = Usage )
         end if
         rtplate = Buffer
      end if

!     ... the output file tamplate
!     ----------------------------
      tplate = tplate_Def
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
      end program raobcore
!====================================================================
