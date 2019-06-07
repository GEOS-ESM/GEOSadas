!====================================================================

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
! BOP
! !MODULE: m_PText -- Routines for printing text messages 
!
! !DESCRIPTION:
!     This module contains the utility routines for the printing text
!     messages for given routine and file connected to a given Fortran
!     logical unit.  The routine name and/or logical unit may be omitted.
!     The default logical unit is standard error.  Another routine, PError,
!     is available to print a message to standard error.
!
!     Prior to writing messages the routines embedded translates C-language
!     escape sequences into ASCII characters.  These routines recognizes
!     the escape sequences, \verb|\n| and \verb|\r|, as paragraph and line
!     breaks.  Within each paragraph, the routine performs word wrap-around
!     for excessively long lines.  For example, the subroutine call,
! \begin{verbatim}
!
!     call PError ( 'Usage', 'RS80_57H.x [-o tplate] infile(s) \n\n'
!    .                    // 'Required parameters: \n'
!    .                    //    '   infile(s) - \Ha list input ODS files '
!    .                    // '\n\n'
!    .                    // 'Optional parameters: \n'
!    .                    //    '   -o tplate - Specify GrADS-like '
!    .                    // 'template to tplate for output file name '
!    .                    // 'generation.  The following descriptors '
!    .                    // 'are allowed: %s, %y4, %m2, %m3, %d2, and '
!    .                    // '%h2.  See the module, m_StrTemplate, in '
!    .                    // 'the mpeu library for a complete list of '
!    .                    // 'valid descriptors and their meanings '
!    .                    // 'Default: %s.RS80_57H.%y4%m2%d2.ods.'
!    .                    // 'Note: the descriptor, %s, corresponds to '
!    .                    // 'the expid as derived from the first '
!    .                    // 'input ODS file. \W')
!
! \end{verbatim}
!     would produce the output,
!
! \begin{verbatim}
! Usage(): RS80_57H.x [-o tplate] infile(s)
! Usage():
! Usage(): Required parameters:
! Usage():    infile(s) - a list input ODS files
! Usage():
! Usage(): Optional parameters:
! Usage():    -o tplate - Specify GrADS-like template to tplate for output file
! Usage(): name generation.  The following descriptors are allowed: %s, %y4,
! Usage(): %m2, %m3, %d2, and %h2.  See the module, m_StrTemplate, in the mpeu
! Usage(): library for a complete list of valid descriptors and their meanings
! Usage(): Default: %s.RS80_57H.%y4%m2%d2.ods.Note: the descriptor, %s,
! Usage(): corresponds to the expid as derived from the first input ODS file.
!
!     Several non-C escape sequences are available to customize the output
!     for user requirements.  The list is as follows:
! \begin{verbatim}
!
!      \C  - Set mode to conventional (Default)
!      \W  - Enable word wrap around
!      \L  - Set left margin (Default: 1)
!      \R  - Set right margin (Default: 79)
!      \I  - Set indentation (Default: 0) 
!      \H  - Set reverse or hanging indentation (Default: 0)
!
! \end{verbatim}
!     The escape sequences affect the current and following paragraphs until
!     overwritten by other succeeding sequences, and each sequence affect the
!     entire current paragraph.  If a paragraph contains two conflicting
!     sequences then the last one takes effect for the entire paragraph.  For
!     example, modified lines in the subroutine call
! \begin{verbatim}
!
!    .       ...          //    '\C   -o tplate - Specify GrADS-like ' ...
!    .                     ...
!    .       ...          // 'input ODS file. \W')
! \end{verbatim}
!     in the previous example would produce identical output.  Note that
!     even though the sequence \verb|\W| appears at the end, the sequence
!     \verb|\C| never affects the paragraph even though the sequence is the
!     first character.
!
!     The sequences, \verb|\L|, \verb|\R|, \verb|\I| and \verb|\H|, allow
!     the calling routine to set margins and indentations in number of
!     characters, which also automatically enables the word wrap-around.
!     The number can be specified within a pair of braces (i.e. \verb|{| and
!     \verb|}|) which must immediately follow or the entire segment will be
!     ignored.  For example, to set the left margin to 4 in all but the first
!     line, the sequence verb|\L{4}| can added as shown the following portion
!     of the subroutine call,
! \begin{verbatim}
!
!     call PError ( 'Usage', 'RS80_57H.x [-o tplate] infile(s) \n\n'
!    .                    // 'Required parameters: \L{4} \n' ...
!
! \end{verbatim}
!     The resulting output would be,
! \begin{verbatim}
!
! Usage(): RS80_57H.x [-o tplate] infile(s)
! Usage():
! Usage():    Required parameters:
! Usage():       infile(s) - a list input ODS files
! Usage():
! Usage():    Optional parameters:
! Usage():       -o tplate - Specify GrADS-like template to tplate for output
! Usage():    file name generation.  The following descriptors are allowed: %s,
! Usage():    %y4, %m2, %m3, %d2, and %h2.  See the module, m_StrTemplate, in
! Usage():    the mpeu library for a complete list of valid descriptors and
! Usage():    their meanings Default: %s.RS80_57H.%y4%m2%d2.ods.Note: the
! Usage():    descriptor, %s, corresponds to the expid as derived from the first
! Usage():    input ODS file.
!
! \end{verbatim}
!     To set the left margin but disable word wrap-around, the sequence,
!     verb|\L{4} \C| can be used.
!
!     The setting can be specified by the location of the sequences within
!     a paragraph.  For example, hanging indentation can be inserted the
!     sequence, \verb|\H| or \verb|\H{}| within the line as shown in the
!     following portion of the subroutine call:
! \begin{verbatim}
!
!     call PError ( 'Usage', 'RS80_57H.x [-o tplate] infile(s) \n\n'
!    .                    // '\L{4}Required parameters: \n'
!    .                    //    '   infile(s) - \H{}a list input ODS files '
!
! \end{verbatim}
!     Note the position of the sequence \verb|\H{}| which will change the 
!     output to
! \begin{verbatim}
!
! Usage(): RS80_57H.x [-o tplate] infile(s)
! Usage():
! Usage():    Required parameters:
! Usage():       infile(s) - a list input ODS files
! Usage():
! Usage():    Optional parameters:
! Usage():       -o tplate - Specify GrADS-like template to tplate for output
! Usage():                   file name generation.  The following descriptors
! Usage():                   are allowed: %s, %y4, %m2, %m3, %d2, and %h2.  See
! Usage():                   the module, m_StrTemplate, in the mpeu library for
! Usage():                   a complete list of valid descriptors and their
! Usage():                   meanings Default: %s.RS80_57H.%y4%m2%d2.ods.Note:
! Usage():                   the descriptor, %s, corresponds to the expid as
! Usage():                   derived from the first input ODS file.
!
! \end{verbatim}
!
!
! !INTERFACE:
!
      module    m_PText
      implicit  NONE
      private ! except

!     Subroutines and functions
!     -------------------------
      public ::
     .   PText,  ! Print text
     .   PError, ! ... or error message initially set to conventional mode
     .   WPError ! ... or to word-wrap mode
c      public :: InitWP, SetWP, AtoI, FPPar, GetVal, wp_parm, SeqList

      interface PText
         module procedure
     .      PText_basic,   ! ...            (text)
     .      PText_stdout,  ! ...     (where, text)
     .      PText_noPref,  ! ... (lu,        text)
     .      PText_         ! ... (lu, where, text)
      end interface
!
! !REVISION HISTORY:
!     08Aug2002  C. Redder  Original version
!     07Jan2003  C. Redder  Changed WhereSuf to WhereSuf_stderr and added
!                           WhereSuf_stdout
! EOP
!-----------------------------------------------------------------
!
! !DATA STRUCTURES
!
!     Word processing information including ... 
!     -----------------------------------------
      type wp_parm
         integer :: Mode        ! = 0 if in conventional mode
                                ! = 1 if in word wrap-around mode.
         integer :: LWhere      ! Length of prefix portion of the line which
                                !   includes the routine name and it suffix.
         integer :: LMar        ! Left margin (character position)
         integer :: RMar        ! Right margin (if mode is word wrap-around)
         integer :: LSp         ! Indentation in number of characters.
                                !   LSp < 0 denotes "hanging" or reverse
      end type                  !   indentation.

!
! !DEFINED PARAMETERS: 
!
!     Constants for general use.
!     --------------------------
      character (len=*), parameter ::
     .   SP       = achar (32)  ! Space (i.e. blank)
      integer,           parameter ::
     .   No_Error = 0,          ! No error status code
     .   LLMaxDef = 79,         ! Default maximum line length
     .   LSegMin  = 35          ! Minimum segment size allowed for
                                !   wrapped text
!     Modes
!     -----
      integer, parameter ::
     .   Conv     = 0,          ! Conventional
     .   WordWrap = 1

!     Non-printable characters available for use in ...
!     -------------------------------------------------
      character (len=1), parameter ::
     .   NUL = achar (   0 ), SOH = achar (   1 ), STX = achar (   2 ),
     .   EOT = achar (   4 ), ENQ = achar (   5 ), ACK = achar (   6 ),
     .   BEL = achar (   7 ),   !   = audible alert
     .   BS  = achar (   8 ),   !   = backspace
     .   HT  = achar (   9 ),   !   = horizontal tab
     .   LF  = achar (  10 ),   !   = new line
     .   VT  = achar (  11 ),   !   = vertical tab
     .   FF  = achar (  12 ),   !   = form feed
     .   CR  = achar (  13 ),   !   = carriage return
     .   SO  = achar (  14 ), SI  = achar (  15 ), DLE = achar (  16 ),
     .   DC1 = achar (  17 ), DC2 = achar (  18 ), DC3 = achar (  19 ),
     .   DC4 = achar (  20 ), NAK = achar (  21 ), SYN = achar (  22 ),
     .   ETB = achar (  23 ), CAN = achar (  24 ), EM  = achar (  25 ),
     .   SUB = achar (  26 ),
     .   ESC = achar (  27 ),   !   = escape
     .   FS  = achar (  28 ), GS  = achar (  29 ), RS  = achar (  30 ),
     .   US  = achar (  31 ),
     .   DEL = achar ( 127 )    !   = delete
      
!     ... in the list of recognized escape sequences
!         for setting word processing information
!     ----------------------------------------------
      integer, parameter ::
     .   iaSetMin = 1,
     .   iaSetMax = 32
      character (len=1), parameter :: EscChar = achar ( 92 ) ! = '\'
      character (len=*), parameter :: SeqList =
     .!
     .!                ASCII character 
     .!    Sequence      equivalent      Name
     .!    --------    ---------------   -----
     .       '\C'     // SOH // ! Switch to conventional mode
     .       '\W'     // STX // ! Switch to wrap mode
     .!       '\^'     // SO  // ! Set tabbing
     .       '\L'     // SI  // ! Set left margin
     .       '\R'     // DLE // ! Set right margin
     .       '\I'     // DC1 // ! Set indentation 
     .       '\H'     // DC2    ! Set reverse or hanging indentation


      contains
!...................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  InitWP () --- Initalizes word processing parameters
!
! !INTERFACE:
      subroutine InitWP ( lu, Where, WP )
      use m_SysIO, only : LWhere
      implicit NONE
      integer,           intent (in)  :: lu
      character (len=*), intent (in)  :: Where
!
! !OUTPUT PARAMETERS: 
      type ( wp_parm ),  intent (out) :: WP
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Aug2002  Redder    Origional code.
!     07Jan2003  Redder    Added required argument, lu.  Implemented
!                          call to the function, LWhere, to determine
!                          the value for WP % LWhere.
! EOP
!-------------------------------------------------------------------------

      WP % Mode   = Conv
      WP % LWhere = LWhere ( lu, Where )
      WP % LMar   = 1
      WP % RMar   = LLMaxDef
      WP % LSp    = 0

      return
      end subroutine InitWP
!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  PError() --- Print a text string to standard error (conventional mode)
! 
! !DESCRIPTION:
!     This routine formats and prints a line of text to standard error
!     The routine is initially set to conventional mode.
!
! !INTERFACE:
      subroutine PError ( Where, Text )
      use m_SysIO, only : stderr
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len = *), intent (in) ::
     .   Where,            ! The routine name.
     .   Text              ! Text to be processed.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Aug2002  Redder    Original code.
! EOP
!-------------------------------------------------------------------------

      integer :: lu

      lu = stderr
      call PText_ ( lu, Where, Text )

      return
      end subroutine PError
!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  WPError() --- Print a text string to standard error (Wrap mode)
! 
! !DESCRIPTION:
!     This routine formats and prints a line of text to standard error
!     This routine is initially set to word-wrap mode
!
! !INTERFACE:
      subroutine WPError ( Where, Text )
      use m_SysIO, only : stderr
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len = *), intent (in) ::
     .   Where,            ! The routine name.
     .   Text              ! Text to be processed.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Aug2002  Redder    Original code.
! EOP
!-------------------------------------------------------------------------

      integer :: lu

      lu = stderr
      call PText_ ( lu, Where, '\W' // Text )

      return
      end subroutine WPError

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  PText_basic() --- Format and print a text string with minimal input.
! 
! !DESCRIPTION:
!     This routine formats and prints a line of text to standard output with
!     no prefix identifying a given routine.
!
! !INTERFACE:
      subroutine PText_basic ( Text )
      use m_SysIO, only : stdout
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len = *), intent (in) ::
     .   Text              ! Text to be processed.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Aug2002  C. Redder  Original code.
! EOP
!-------------------------------------------------------------------------

      integer :: lu

      lu = stdout
      call PText_ ( lu, ' ', Text )

      return
      end subroutine PText_basic

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  PText_noPref() --- Format and print a text string with no prefix.
! 
! !DESCRIPTION:
!     This routine formats and prints a line of text with no prefix
!     identifying a given routine.
!
! !INTERFACE:
      subroutine PText_noPref ( lu, Text )
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,             intent (in) ::
     .   lu                ! Fortran logical I/O unit
      character (len = *), intent (in) ::
     .   Text              ! Text to be processed.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Aug2002  Redder    Original code.
! EOP
!-------------------------------------------------------------------------

      call PText_ ( lu, ' ', Text )

      return
      end subroutine PText_noPref

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  PText_stdout() --- Format and print a text string to standard output
! 
! !DESCRIPTION:
!     This routine formats and prints a line of text to standard output
!
! !INTERFACE:
      subroutine PText_stdout ( Where, Text )
      use m_SysIO, only : stdout
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len = *), intent (in) ::
     .   Where,            ! The routine name.
     .   Text              ! Text to be processed.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Aug2002  Redder    Original code.
! EOP
!-------------------------------------------------------------------------

      integer :: lu

      lu = stdout
      call PText_ ( lu, Where, Text )

      return
      end subroutine PText_stdout

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  PText_() --- Format and print a text string
! 
! !DESCRIPTION:
!     This routine formats and prints a line of text.
!
! !INTERFACE:
      subroutine PText_ ( lu, Where, Text )
      use m_TextUtil, only : EscC, Len_Line
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,             intent (in) ::
     .   lu                ! Fortran logical I/O unit
      character (len = *), intent (in) ::
     .   Where,            ! The routine name.
     .   Text              ! Text to be processed.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Aug2002  Redder    Original code.
!     06Jan2003  Redder    Removed second declaration of LOP in internal
!                          subroutine m_PText1.
! EOP
!-------------------------------------------------------------------------

!     Translates C-language escape sequences to ASCII characters and
!     then calls internal subroutines to complete the process.
!     --------------------------------------------------------------
      call PText1 ( lu, Where, EscC ( Text, NewList = SeqList ))

      contains
      subroutine PText1 ( lu, Where, Text )
      implicit NONE
      integer,             intent (in) :: lu
      character (len = *), intent (in) :: Where, Text
      type ( wp_parm ) :: ParInfo
      integer :: LOP, iPar, iBOP, iEOP, LOT

!     Initialize word processing parameters
!     -------------------------------------
      call InitWP ( lu, Where, ParInfo )

!     Scan text to ...
!     ----------------
      LOT  = len ( Text )
      iBOP = 1
      ScanLoop : do iPar = 1, LOT + 1           ! Add one to ensure null
                                                !   text is processed
!        ... split the text according to paragraphs ...
!        -----------------------------------------------------
         LOP  = Len_Line ( Text ( iBOP : LOT ), LenPar = .true. )
 
!        ...  and to process each paragraph
!        ----------------------------------
         iEOP = iBOP + LOP - 1 
         call FPPar ( lu, Where, Text ( iBOP : iEOP ), ParInfo )

!        ... and exit if at the end of text
!        ----------------------------------
         if ( iEOP .ge. LOT ) exit
         iBOP = iEOP + 2

      end do ScanLoop

      return
      end subroutine PText1
      end subroutine PText_

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  FPPar() --- Format and print paragraph
! 
! !DESCRIPTION:
!
! !INTERFACE:
      subroutine FPPar ( lu, Where, Text, ParInfo )
!
! !INPUT PARAMETERS:
      use m_SysIO,    only : POut
      use m_TextUtil, only : Wrap, Len_Line, Indent
      implicit NONE
      integer,             intent (in) ::
     .   lu                ! Fortran logical I/O unit
      character (len = *), intent (in) ::
     .   Where,            ! The routine name
     .   Text              ! Text to be processed.
      type ( wp_parm ),    intent (inout) ::
     .   ParInfo           ! Word processing information
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Aug2002  Redder    Original code
!     25Nov2002  Redder    Replaced call to PrLine with call to PErr and
!                          with write statements.  Removed the internal
!                          routine FPPar1
!     29Jan2003  Redder    Fixed bug in inserting spaces in left margin.
! EOP
!-------------------------------------------------------------------------
      integer  :: LOT,  LWhere, LOLMax, LOSMax
      integer  :: Mode, LMar, RMar, LSp
      integer  :: iBOL, iEOL, LLine, iLine, LPrefix
      character (len=1) :: Marker
      character (len = len(Text))          :: Buffer 
      character (len = ParInfo % LMar - 1) :: LMarSp
      LMarSp = SP

!     Update the word processing parameters and strip out the sequences
!     -----------------------------------------------------------------
      call SetWP ( Text, ParInfo, Buffer )

      Mode    = ParInfo % Mode
      LMar    = ParInfo % LMar
      RMar    = ParInfo % RMar
      LSp     = ParInfo % LSp
      LPrefix = ParInfo % LWhere + ParInfo % LMar - 1

!     If mode is set to wrap ...
!     --------------------------
      if ( Mode .eq. WordWrap ) then             ! ... determine ...
         LOLMax = max ( RMar, LPrefix + abs ( LSp ) + LSegMin )
         LOSMax = LOLMax - LPrefix               ! ... maximum size of section
                                                 !     of line after Prefix
         Buffer = Wrap ( Buffer, LOSMax, LSp )   ! ... position and type of
      end if                                     !     line breaks for wrapped

!     ... or if in conventional mode, do no editing
!     ---------------------------------------------

!     Print each line
!     --------------- 
      Marker = SP
      iBOL   = 1
      LOT    = len ( Buffer )
      do iLine = 1, LOT + 1                      ! Add one to ensure null 
                                                 !   paragraph is processed
         LLine  = Len_Line ( Buffer ( iBOL : ))  ! Determine the line length
         iEOL   = iBOL + LLine - 1               ! ... and end of line
         call POut ( lu, Where,                  ! Print text line
     .               LMarSp // trim ( Indent ( Buffer ( iBOL : iEOL ),
     .                                Marker, LSp )))
         if ( iEOL .ge. LOT ) exit               ! Exit if at end of text
         Marker = Buffer ( iEOL+1 : iEOL+1 )     ! Save line break character
         iBOL   = iEOL + 2                       ! Locate next line
      end do

      return
      end subroutine FPPar

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  SetWP() --- Scans text to set word processing parameters
! 
! !DESCRIPTION:
!     This routine scan text for sequences to set word processing parameters.
!     If an error has occurred then the routine replaces the marker with the
!     escape character, '/'
!
! !INTERFACE:
      subroutine   SetWP ( InText, WP, OutText )
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in)    ::
     .   InText  ! Input text.
!
! !INPUT/OUTPUT PARAMETERS: 
      type (wp_parm),    intent (inout) ::
     .   WP      ! Word processing parameters
!
! !OUTPUT PARAMETERS: 
      character (len=*), intent (out), optional ::
     .   OutText ! Output text with set sequences removed
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Aug2002  Redder    Original code.
! EOP
!-------------------------------------------------------------------------
      integer :: iList, LList, iSeqList, Val, stat
      integer ::  LOT1, LOT2, iCh, iaCh, iBOS1, iBOS2, LOS
      integer :: iEOS1, iEOS2
      character (len=1) :: TextCh, Mkr
      logical :: marker ( iaSetMin : iaSetMax ), skip_char, set_out

!     Create logical table indicating whether a character is a set marker
!     -------------------------------------------------------------------
      marker = .false.
      do iList = 3, len ( SeqList ), 3
         marker ( iachar ( SeqList ( iList : iList ))) = .true.
      end do

!     Implement option to output text with set sequences removed
!     ----------------------------------------------------------
      set_out = present ( OutText )

!     For each character scanned ...
!     ------------------------------
      iBOS1 = 1
      iBOS2 = 1
      LOT1  = Len_Trim ( InText )
      do iCh = 1, LOT1
         if ( iBOS1 .gt. LOT1 ) exit             ! ... determine if at the
                                                 !     end of the text
         skip_char = .true.                      ! ... and if the character
         TextCh    = InText ( iBOS1 : iBOS1 )    !     is a marker that
         iaCh      = iachar ( TextCh )           !     begin a parameter
         if ( iaCh .ge. iaSetMin .and.           !     setting sequence
     .        iaCh .le. iaSetMax ) then
            skip_char = .not. marker ( iaCh )
         end if

         if ( skip_char ) then                   ! If not ...
            if ( set_out ) OutText ( iBOS2 : iBOS2 ) = TextCh
            iBOS1 = iBOS1 + 1                    !   then copy the character 
            iBOS2 = iBOS2 + 1                    !   to output and skip to
                                                 !   next character
         else                                    ! Otherwise set the ...
            Mkr   = TextCh
            if         ( Mkr .eq. SOH ) then     ! ... switch to conventional
               WP % Mode = Conv                  !     mode
               iBOS1 = iBOS1 + 1
            else if    ( Mkr .eq. STX ) then     ! ... switch to wrap mode
               WP % Mode = WordWrap
               iBOS1 = iBOS1 + 1
            else if    ( Mkr .eq. SO  ) then     ! ... Set tabbing
c               WP % Mode = Conv
               LOS   = 1
               iBOS1 = iBOS1 + 1
            else
               call GetVal ( InText ( iBOS1 + 1 : ), LOS, Val, stat )
               if ( stat .eq. 1 ) Val = iBOS2 
               if ( stat .ne. 2 ) then
                  if      ( Mkr .eq. SI  ) then  ! ... set left margin
                     if ( Val  .le. 1 ) then
                        stat      = 2            ! Bad value
                     else
                        WP % Mode =  WordWrap
                        WP % LMar =  Val
                     end if
                  else if ( Mkr .eq. DLE ) then  ! ... set right margin
                     if ( Val  .le. 1 ) then
                        stat      = 2            ! Bad value
                     else
                        WP % Mode =  WordWrap
                        if ( stat .eq. 1 ) Val = -Val
                        WP % RMar =  Val
                     end if
                  else if ( Mkr .eq. DC1 ) then  ! ... set indentation 
                     WP % Mode =  WordWrap
                     if ( stat .eq. 1 ) Val = Val - 1
                     WP % LSp  =  Val
                  else if ( Mkr .eq. DC2 ) then  ! ... set hanging
                     WP % Mode =  WordWrap       !     identation
                     if ( stat .eq. 1 ) Val = Val - 1
                     WP % LSp  = -Val
                  end if
                  iBOS1 = iBOS1 + LOS + 1
               end if

               if ( stat .eq. 2 ) then
                  iEOS1 = iBOS1 + LOS + 1        ! If an error has occurred
                  iEOS2 = iBOS2 + LOS + 1        ! ... copy the sequence to
                  if ( set_out ) then            !     the output text
                     OutText ( iBOS2 : iEOS2 ) = InText ( iBOS1 : iEOS1)
                     OutText ( iBOS2 : iBOS2 ) = EscChar
                  end if                         ! ... and replace the marker
                  iBOS1 = iBOS1 + LOS + 1        !     with the escape 
                  iBOS2 = iBOS2 + LOS + 1        !     character
               end if
            end if
         end if
      end do

      LOT2 = iBOS2                               ! Insert trailing spaces to
      if ( set_out ) OutText ( LOT2 : len ( OutText )) = SP ! output text

!     If the right margin is determined by
!     the character position of the marker ...
!     ----------------------------------------
      if ( WP % RMar .lt. 0 )                    ! ... account for prefix and
     .   WP % RMar = WP % LWhere + WP % LMar - WP % RMar - 2 ! ... left margin

      return
      end subroutine SetWP
!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  GetVal() --- Get value enclosed by delimiting symbols.
! 
! !DESCRIPTION:
!     This routine gets the value from a segment of text in the format,
!     {int}, where the braces are the delimiters and int is the value which
!     can contain the sign and upto three other digits.  Blanks are treated
!     as null.  This routine assumes that the delimiters are missing if the
!     first character is not the opening delimiter.
!
! !INTERFACE:
      subroutine   GetVal ( Text, LOS, Val, stat )
c      use m_AtoX, only : AtoI
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in)  ::
     .   Text  ! Input text.
!
! !OUTPUT PARAMETERS: 
      integer,           intent (out) ::
     .   LOS,  ! Length of segment enclosed by the delimiters, inclusively.
     .         !   Zero is returned if delimters are missing or unbalanced.
     .   Val,  ! Retrieved value.
     .   stat  ! = 0 if no error.
               ! = 1 if value is missing
               ! = 2 if conversion error or unbalanced delimiters
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     08Aug2002  Redder    Original code.
! EOP
!...................................................................

      integer :: LOSMax, iCh, LText, iScan, iVerify, iDelmtr, istat
      character (len=1)  :: Ch
      character (len=1), parameter ::
     .   Delmtr1 = '{',
     .   Delmtr2 = '}' 

      LText = len ( Text )
      if ( LText .eq. 0 ) then             ! If input text is null
         LOS  = 0                          ! ... then return with a status of
         Val  = 0                          !     missing value and a zero
         stat = 1                          !     segment size
      else                                 ! Otherwise ...
         Ch = Text ( 1 : 1 )     
         if ( Ch .ne. Delmtr1 ) then       ! ... if the opening delimiter is
            LOS  = 0                       !     missing
            Val  = 0                       ! ... then return with a status of
            stat = 1                       !     missing value and a zero
                                           !     segment size.
         else
            iScan = scan ( Text ( 2 : LText ), Delmtr2 )
            if ( iScan .eq. 0 ) then       ! If the delimiters are unbalanced 
               LOS  = 0                    ! ... then return with an error 
               Val  = 0                    !     status and a zero segment 
               stat = 2                    !     size
            else
               iDelmtr = iScan + 1  
               iVerify = verify ( Text ( 2 : iDelmtr - 1 ), HT // SP )
               if ( iVerify .eq. 0 ) then  ! If segment between the delimeters
                  LOS  = iDelmtr           !     contain only blanks.
                  Val  = 0                 ! ... then return with a status of
                  stat = 1                 !     missing value
               else
                  Val  = AtoI ( Text ( 2 : iDelmtr - 1 ), istat )
                  if ( istat .ne. 0 ) then ! If conversion error ...
                     LOS  = iDelmtr        ! ... then return with an error
                     Val  = 0              !     status
                     stat = 2
                  else                     ! Otherwise
                     LOS  = iDelmtr        ! ... return with a valid error
                     stat = 0              !     status.
                  end if
               end if
            end if
         end if

      end if

      return
      end subroutine GetVal
*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AtoI () --- Convert ASCII (a multcharacter string) to an integer
! 
! !DESCRIPTION: 
!     This function converts a token (stored as a multicharacter string)
!     to an integer according to the FORTRAN format editing descriptors
!     \verb|'(BN,IL)'| where \verb|L| is the input string length.  In
!     other words, blanks (and tabs) are ignored.  This function is 
!     simplified version of the one in the module, m_AtoX.
!
! !INTERFACE: 
      integer function AtoI ( token, stat )
!
! !INPUT PARAMETERS: 
      implicit NONE
      character ( len = * ), intent (in)  ::
     .            token      ! string containing the number  
!
! !OUTPUT PARAMETERS:
      integer,               intent (out) ::
     .            stat       ! Returned status (error) code

!     Error status codes and messages
!     -------------------------------
      integer, parameter ::
     .   No_Error        =  0,     ! Valid status code; no error
     .   IConvErr        =  1      ! Error in converting ASCII to integer
!
! !REVISION HISTORY:
!     31Jan2001  C Redder   Original code
!     06Aug2002  C Redder   Simplified version of the function AtoI in module
!                           m_AtoX.
!
! EOP
!-------------------------------------------------------------------------

      integer :: INum,  iLetter
      integer :: NChar, iChar, iChar1
      character ( len = 1 ) ::  Char1
      integer :: NNumbers
      integer :: SignFactor, NDigits, iLeadDigit, NextDigit
      logical :: sign_found, LeadDigit_Found

!     Contants for ...
!     ----------------
      integer, parameter ::
     .   NDigits_Max = 3,              ! Maximum number of digits
     .   iCharMin    = 0,              ! Upper and ...
     .   iCharMax    = 127,            ! ... lower boundaries in ASCII table.
     .   iTab        = iachar (  HT ), ! ASCII integer representation for tab
     .   iSpace      = iachar (  SP ), ! ... blank
     .   iZero       = iachar ( '0' ), ! ... zero
     .   iNegSign    = iachar ( '-' )  ! ... minus sign

!     Logical tables used to determine if a character is a sign (i.e +/-)
!     -------------------------------------------------------------------
      integer, parameter  :: iBegSign  =  43 - iCharMin
      integer, parameter  :: iEndSign  = iCharMax - 46  + 1
      logical, parameter, dimension ( iCharMin : iCharMax ) ::
     .         sign        = ( / ( .false.,  iLetter = 1, iBegSign ),
     .                             .true.,
     .                             .false.,
     .                             .true.,
     .                           ( .false.,  iLetter = 1, iEndSign ) / )
!     ... a number (i.e. 0-9)
!     -----------------------
      integer, parameter  :: iBegNum   =  48 - iCharMin
      integer, parameter  :: iEndNum   = iCharMax - 58  + 1
      logical, parameter, dimension ( iCharMin : iCharMax ) ::
     .         number      = ( / ( .false.,  iLetter = 1, iBegNum ),
     .                           ( .true.,   iLetter = 1, 10 ),
     .                           ( .false.,  iLetter = 1, iEndNum ) / )

!     Set defaults
!     ------------
      stat = 0
      AtoI = 0

!     Initialize variables for the number of ...
!     ------------------------------------------
      NNumbers         =  0      ! ... characters, 0 - 9
      NDigits          =  0      ! ... all digits beyond the leading zeroes

!     ... positions within the input string of the ...
!     ------------------------------------------------
      iLeadDigit       =  0      ! ... lead digit for the nonexponent part

!     ... numbers for the ...
!     -----------------------
      INum             =  0      ! ... nonexponent part
      SignFactor       =  1      ! ... and its sign

!     ... logical flags indicating whether ...
!     ----------------------------------------
      LeadDigit_Found  = .false. ! ... the lead digit has been found
      Sign_Found       = .false. ! ... the sign has been found

!     Scan the token
!     --------------
      NChar = Len_Trim ( token )
      Scan : do iChar = 1, NChar
         stat   = IConvErr       ! Initially assume conversion error
                                 !   for this cycle
         Char1  = token  ( iChar : iChar )
         iChar1 = iachar (  Char1 )

!        Check for characters
!        ----------------------------
         if ( iChar1 .lt. iCharMin .or.
     .        iChar1 .gt. iCharMax   ) exit Scan

!        ... numbers that are ...
!        -------------------------
         if ( number ( iChar1 ) ) then
            NNumbers = NNumbers + 1

!           ... significant digits
!           ----------------------
            if ( iChar1 .ne. iZero ) then
               LeadDigit_Found = .true.
               if ( iLeadDigit .eq. 0 )
     .            iLeadDigit   =  NNumbers

            end if

!           After lead digit is found ...
!           -----------------------------
            if ( LeadDigit_Found ) then
               NDigits       = NDigits + 1
               NextDigit     = iChar1  - iZero

!              ... convert all digits and accumualte the number
!              ------------------------------------------------
               if      ( NDigits .le. NDigits_Max  ) then
                  INum       = 10 * INum  + NextDigit

!              ... and exit if there are too many digits
!              -----------------------------------------
               else
                  exit Scan
               end if

            end if

!        ... sign ...
!        ------------
         else if ( sign ( iChar1 ) ) then

            if ( NNumbers .gt. 0 ) then
               exit Scan                         ! The sign must be the first
                                                 !   non-blank character
            else if ( Sign_Found ) then          ! No more than one sign
               exit Scan                         !   is allowed

            else
               if ( iChar1 .eq. iNegSign ) SignFactor = -1
               Sign_Found = .true.

            end if

!        ... for anything else (which is illegal except blanks or tabs)
!        --------------------------------------------------------------
         else if ( iChar1 .ne. iTab .and.
     .             iChar1 .ne. iSpace ) then
            exit Scan

         end if

         stat = No_Error                         ! No conversion error
      end do Scan                                !   during this cycle

!     Return the result
!     -----------------
      if ( stat .eq. No_Error ) AtoI = SignFactor * INum
      return

      end function AtoI
!...................................................................
      end module m_PText

!====================================================================
