!====================================================================

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_TextUtil -- Utility routines for text data
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      module    m_TextUtil
      implicit  NONE
      private ! except

!     Subroutines and functions
!     -------------------------
      public ::
     .   ItoA,      ! Return an integer as a character string
     .   ItoDate,   ! Return data/time in character string format
     .   BlankStr,  ! Return a blank string of given length
     .   Wrap,      ! Inserts carriage returns for word wrap around
     .   WrapRange, ! Return line parameters for wrap around
     .   Len_Line,  ! Return length of line
     .   Indent,    ! Return string with leading (or trailing) blanks
     .   EscC,      ! Translates C-language escape sequences
     .   XMLNav,    ! Returns the navigators for ... 
     .   XML,       ! ... the contents between XML start and end tags
     .   FXML,      ! Function version of XML
     .   PrepList,  ! Check and prepare list.
     .   UpperCase, ! Convert string to upper case
     .   LowerCase  ! Convert string to upper case

      interface ItoDate
         module procedure
     .      ItoDate_,      ! ... ( YYYYMMDD )
     .      ItoDateTime    ! ... ( YYYYMMDD, HHMMSS )
      end interface        ! where all parameters are integers
!
! !REVISION HISTORY:
!     31Aug2001  C. Redder  Original version
!     31Jul2002  C. Redder  Removed the routine WrapWP.  Added the routine
!                           Len_Line
!     31Oct2002  C. Redder  Added the public routines, XMLNav and XMLTag
!     31Jan2003  C. Redder  Added the public routine, PrepList
! EOP
!-----------------------------------------------------------------
!
! !DEFINED PARAMETERS: 
!
!     Control characters
!     ------------------
      character ( len =  1 ), parameter ::
     .   NUL = achar (   0 ), !   = null character
     .   SOH = achar (   1 ), STX = achar (   2 ), ETX = achar (   3 ),
     .   EOT = achar (   4 ), ENQ = achar (   5 ), ACK = achar (   6 ),
     .   BEL = achar (   7 ), !   = audible alert
     .   BS  = achar (   8 ), !   = backspace
     .   HT  = achar (   9 ), !   = horizontal tab
     .   LF  = achar (  10 ), !   = new line
     .   VT  = achar (  11 ), !   = vertical tab
     .   FF  = achar (  12 ), !   = form feed
     .   CR  = achar (  13 ), !   = carriage return
     .   SO  = achar (  14 ), SI  = achar (  15 ), DLE = achar (  16 ),
     .   DC1 = achar (  17 ), DC2 = achar (  18 ), DC3 = achar (  19 ),
     .   DC4 = achar (  20 ), NAK = achar (  21 ), SYN = achar (  22 ),
     .   ETB = achar (  23 ), CAN = achar (  24 ), EM  = achar (  25 ),
     .   SUB = achar (  26 ),
     .   ESC = achar (  27 ), !   = escape
     .   FS  = achar (  28 ), GS  = achar (  29 ), RS  = achar (  30 ),
     .   US  = achar (  31 ),
     .   SP  = achar (  32 ), !   = blank
     .   DEL = achar ( 127 )  !   = delete
      character (len=*), parameter ::
     .   NLList = LF // CR,   ! List of character for new line (NL) ...
     .   NPList = LF          ! ... and new paragraph (NP) markers

c      character (len=*), parameter ::
c     .   ComList  = '\w' // DC1
c     .           // '\u' // DC2
c     .           // '\!' // DC3
c     .           // '\I' // DC4,
c     .   ComChars =  DC1 // DC2 // DC3 // DC4,
c     .   BlkList  =  NUL // SOH // STX // ETX // EOT // ENQ // ACK
c     .           //  BS  // VT  // FF
c     .           //  SO  // SI  // DLE
c     .           //  NAK // SYN // ETB // CAN // EM  // SUB // ESC
c     .           //  FS  // GS  // RS  // US  // DEL // SP

!     Parameters for converting integers to ASCII characters
!     ------------------------------------------------------
      integer, parameter :: NDigitsMax = range ( 1 ) + 1
      integer, parameter :: MaxExp     = NDigitsMax - 1
      integer :: iExp
      integer, parameter, dimension ( 0 : MaxExp ) :: ! Table for powers of 10
     .   Exp10   = (/( 10 ** iExp, iExp = 0, MaxExp )/)
      character (len=1), parameter, dimension ( 0 : 9 ) ::
     .   CDigits = (/ '0', '1', '2', '3', '4',
     .                '5', '6', '7', '8', '9' /)

!     Names of the month
!     ------------------
      character ( len =  3 ), dimension ( 12 ), parameter ::
     .   CMonth = (/ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     .               'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /)

!     Parameters for the ASCII collating sequence
!     -------------------------------------------
      integer, parameter ::
     .   IACharMin =   0, ! first and ...
     .   IACharMax = 127  ! ... last character position in the sequence 

!     Table for coverting ASCII character code to number
!     --------------------------------------------------
      integer :: iLetter, iNum
      integer, parameter  :: iBegNum   =  48 - IACharMin
      integer, parameter  :: iEndNum   = IACharMax - 103  + 1
      integer, parameter, dimension ( IACharMin : IACharMax ) ::
     .         Ch2Num      = ( / ( 0,        iLetter = 1, iBegNum ),
     .                           ( iLetter,  iLetter = 0,  9 ),
     .                           ( 0,        iLetter = 1,  7 ),
     .                             10, 11, 12, 13, 14, 15,
     .                           ( 0,        iLetter = 1, 26 ),
     .                             10, 11, 12, 13, 14, 15,
     .                           ( 0,        iLetter = 1, iEndNum ) / )

!     Parameters for the small octal numbers
!     ---------------------------------------
      integer, parameter  :: iBegOct   =  48 - IACharMin
      integer, parameter  :: iEndOct   = IACharMax - 56  + 1
      logical, parameter, dimension ( IACharMin : IACharMax ) ::
     .         octal       = ( / ( .false.,  iLetter = 1, iBegOct ),
     .                           ( .true.,   iLetter = 1, 8 ),
     .                           ( .false.,  iLetter = 1, iEndOct ) / )
      integer, parameter, dimension ( 0 : 63 ) ::
     .         OctTab      = ( / ( iNum * 8, iNum = 0, 63 ) / )  

!     ... small decimal numbers
!     -------------------------
      integer, parameter  :: iBegDec   =  48 - IACharMin
      integer, parameter  :: iEndDec   = IACharMax - 58  + 1
      logical, parameter, dimension ( IACharMin : IACharMax ) ::
     .         decimal     = ( / ( .false.,  iLetter = 1, iBegDec ),
     .                           ( .true.,   iLetter = 1, 10 ),
     .                           ( .false.,  iLetter = 1, iEndDec ) / )
      integer, parameter, dimension ( 0 : 99 ) ::
     .         DecTab      = ( / ( iNum * 10, iNum = 0, 99 ) / )  

!     ... and small hexadecimal numbers
!     ---------------------------------
      integer, parameter  :: iBegHex   =  48 - IACharMin
      integer, parameter  :: iEndHex   = IACharMax - 103  + 1
      logical, parameter, dimension ( IACharMin : IACharMax ) ::
     .         hex         = ( / ( .false.,  iLetter = 1, iBegHex ),
     .                           ( .true.,   iLetter = 1, 10 ),
     .                           ( .false.,  iLetter = 1,  7 ),
     .                           ( .true.,   iLetter = 1,  6 ),
     .                           ( .false.,  iLetter = 1, 26 ),
     .                           ( .true.,   iLetter = 1,  6 ),
     .                           ( .false.,  iLetter = 1, iEndHex ) / )
      integer, parameter, dimension ( 0 : 16 ) ::
     .         HexTab      = ( / ( iNum * 16, iNum = 0, 16 ) / )  

      contains

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  ItoA() --- Returns an integer as a string with left justification
! 
! !DESCRIPTION: Convert integer to string with left justification
!
! !INTERFACE:
!
      function ItoA ( Num )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      integer, intent (in)  :: Num
!
! !OUTPUT PARAMETERS: 
!
      character ( len = NDigitsMax + 1 ) :: ItoA
!
! !REVISION HISTORY: 
!
!     20Apr2001  C. Redder   Initial code
!
! EOP
!-------------------------------------------------------------------------

      integer :: Num_, Digit, iDigit, NDigits, iExp, iStr

!     Determine the number of digits
!     ------------------------------
      Num_    = abs ( Num )
      NDigits = 0
      ScanNum : do iDigit = 1, NDigitsMax - 1
         if ( Exp10 ( iDigit ) .gt. Num_ ) then
            NDigits = iDigit
            exit ScanNum
         end if
      end do ScanNum
      if ( NDigits .eq. 0 ) NDigits = NDigitsMax

!     Convert to character format
!     ---------------------------
      do iStr = 1, NDigits
         iExp  = NDigits - iStr
         Digit = Num_ / Exp10 ( iExp )
         ItoA ( iStr : iStr ) = CDigits ( Digit )
         Num_  = Num_ - Digit * Exp10 ( iExp )

      end do

!     Add sign and trailing blanks
!     ----------------------------
      ItoA = ItoA ( : NDigits )
      if ( Num .lt. 0 ) ItoA = '-' // ItoA ( : NDigits )

      return
      end function ItoA

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  ItoDate_() --- Returns date in character string format
! 
! !DESCRIPTION:
!     This function returns a date as a character string in the format,
!     MMM DD, YYYY where MMM is the three letter abreviation for the month
!     and DD and YYYY are the day and year.  The day may be expressed in
!     less than two digits but the returned string is left justified.
!     If the date is in YYYYMMDDHH format where HH is the hour, the
!     the funciont returns a string of the form. MMM DD, YYYY at HH:00 GMT
!
! !INTERFACE:
!
      function ItoDate_ ( YYYYMMDD )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      integer, intent (in)  :: YYYYMMDD ! ... where MM is the two digit
!                                       !     representation for the month.
! !OUTPUT PARAMETERS: 
!
      character ( len = 12 + 13 * max (0,min (1,YYYYMMDD/100000000))) ::
     .   ItoDate_
!
! !REVISION HISTORY: 
!
!     03Apr2001  C. Redder   Initial code
!     15Jul2003  C. Redder   Enabled function to process a date/time in
!                            YYYYMMDDHH format
!     29Jan2004  C. Redder   Changed declaration of ItoDate_ to make
!                            code portable on an SGI
! EOP
!-------------------------------------------------------------------------

      integer :: Month, Day, Year, Hour, NYMD, NHMS
      character (len=2) :: CDay
      character (len=4) :: CYear

      if ( YYYYMMDD .lt. 100000000 ) then
         Year  =       YYYYMMDD / 10000
         Month = mod ( YYYYMMDD,  10000   ) / 100
         Day   = mod ( YYYYMMDD,  100     )

         Month = max ( min ( Month, 12 ), 1 )
         CDay  = ItoA ( Day  )
         CYear = ItoA ( Year )
         ItoDate_ =  CMonth  ( Month ) // SP
     .            // trim    ( CDay  ) // ',' // SP
     .            // AdjustR ( CYear )

      else 
         NYMD  =       YYYYMMDD / 100
         NHMS  = mod ( YYYYMMDD,  100 ) * 10000
         ItoDate_ = ItoDateTime ( NYMD, NHMS )
      end if

      return
      end function ItoDate_
!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  ItoDateTime() --- Returns date and time in character string format
! 
! !DESCRIPTION:
!     This function returns a date and time as a character string in the
!     format, MMM DD, YYYY at HH:MM GMT where MMM is the three letter
!     abreviation for the month and DD and YYYY are the day and year.  HH
!     and MM are the two digit representation of the hour and minute. 
!     The day, hour and minute may be expressed in less than two digits
!     but the returned string is left justified.
!
! !INTERFACE:
!
      function ItoDateTime ( YYYYMMDD, HHMMSS )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      integer, intent (in)  :: YYYYMMDD ! ... where MM is the two digit
                                        !     representation for the month
      integer, intent (in)  :: HHMMSS   ! ... where SS is the two digit
                                        !     representation for the second
!                                       !     which is ignored.
! !OUTPUT PARAMETERS: 
!
      character ( len = 25 ) :: ItoDateTime
!
! !REVISION HISTORY: 
!
!     03Apr2001  C. Redder   Initial code
!     07Jul2003  C. Redder   Changed units for time from GMT to UTC.
!
! EOP
!-------------------------------------------------------------------------

      integer :: Month, Day, Year, Hour,  Minute
      character (len=2) :: CDay, CHour, CMinute
      character (len=4) :: CYear

      Year    =       YYYYMMDD / 10000
      Month   = mod ( YYYYMMDD,  10000 ) / 100
      Day     = mod ( YYYYMMDD,  100   )
      Hour    =       HHMMSS   / 10000
      Minute  = mod ( HHMMSS,    10000 ) / 100

      Month   = max ( min ( Month, 12 ), 1 )
      CDay    = ItoA      ( Day    )
      CYear   = ItoA      ( Year   )
      CHour   = ItoA      ( Hour   )
      CMinute = ItoA      ( Minute )
      if ( CMinute ( 2 : 2 ) .eq. SP ) 
     .   CMinute = '0' // CMinute ( 1 : 1 )
      ItoDateTime =  CMonth  ( Month   ) // SP
     .           //  trim    ( CDay    ) // ',' // SP
     .           //  AdjustR ( CYear   ) // SP
     .           //  AdjustR ( CHour   ) // ':'
     .           //  AdjustR ( CMinute ) // SP // 'UTC'

      return
      end function ItoDateTime

!....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: BlankStr() --- Return a blank string of given length
! 
! !DESCRIPTION: This function returns a blank string of given length
!
! !INTERFACE:
!
      function BlankStr ( LStr )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      integer, intent (in) :: LStr     ! Length of output string
!
! !OUTPUT PARAMETERS: 
!
      character (len=LStr) :: BlankStr ! Output string of blanks
!
! !REVISION HISTORY: 
!
!     14Feb2001  C. Redder   Initial code
!
! EOP
!-------------------------------------------------------------------------

      BlankStr = SP

      return
      end function BlankStr

!...................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  EscC () --- Translates C-language escape sequences to ASCII characters
! 
! !DESCRIPTION:
!     This function translates C-language escape sequences (e.g. verb|'\n'|
!     and verb|'\x4E'|) to ASCII characters embedded in a givven text string. 
!     In C, the following sequences are defined:
! \begin{verbatim}
!                 ASCII character
!      Sequence     equivalent      Name
!         \n        achar (10)      newline
!         \t        achar ( 9)      horizontal tab
!         \v        achar (11)      vertical tab
!         \b        achar ( 8)      backspece
!         \r        achar (13)      carriage return
!         \f        achar (12)      formfeed
!         \a        achar ( 7)      audible alert
!         \\            \           backslash
!         \?            ?           question mark
!         \'            '           single quotation mark
!         \"            "           double quotation mark
!         \ooo      achar (ooo)     octal (ooo), ...
!         \dnnn     achar (ddd)     decimal (ddd) (not C standard) and ...
!         \xhh      achar (hh)      hexadecimal (hh) number respresentation
!                                     for the ASCII character
! \end{verbatim}
!     Common examples of an octal, hexadecimal, and decimal escape sequence
!     construct are \verb|'\0'|, \verb|'\x0'| (or \verb|'\x'|) and
!     \verb|'\d0'| which represent the null character.  The octal and
!     decimal numbers, \verb|ooo| and \verb|ddd|, can consist of upto 3
!     digits, and the hexadecimal can consist of upto 2 digits; but any
!     digits beyond the maximum are ignored.  That is, the text string
!     \verb|'\x4EE'| would be translated to \verb|'NE'|.  If the value
!     of any of the numbers is outside the range [0,127], then the entire
!     escape sequence is ignored.  Thus, the input string \verb|'\xff'|
!     would be left unmodified.  In addition, any escape sequence not
!     included in the above table (e.g. verb|'\c'|) would also be ignored.
!     Upper case characters are distinguished from lower case except when
!     the characters are involved in hexadecimal numbers.  Thus, \verb|'\N'|
!     is not the same as verb|'\n'| and would be ignored, but verb|'\x4E'|
!     and verb|'\x4e'| produce the same result.  If the input text shrinks
!     as a result of translation(s), then trailing blanks are inserted so
!     that the lengths of the input and output strings remain equal.  That
!     is the input text \verb|'\x4E'| would result in the output string
!     \verb|'N   '|.
!
!     This function also allows input to define (or redefined) escape
!     sequences and their one-character replacements via the optional input
!     argument, \verb|NewList|.  The input to this argument must be in the
!     format,
! \begin{verbatim}
!     \o1n1\o2n2\... 
! \end{verbatim}
!     where \verb|o1|, \verb|o2| denote the old characters to be replaced by
!     the new characters \verb|n1| and \verb|n2|.  
!
! !INTERFACE:
      function EscC ( InText, NewList )
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in) ::
     .   InText          ! Input text with embedded sequences.
      character (len=*), intent (in), optional ::
     .   NewList         ! New list of additional or redefined sequences
!
! !OUTPUT PARAMETERS: 
      character (len=len(InText)) ::
     .   EscC            ! Modified text with trailing blanks.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     23Jul2001  Redder    Origional code.
!
! EOP
!-------------------------------------------------------------------------
      logical :: new_list
      integer :: LSeg, iChar, LText, IANextCh
      integer :: iDefList, iNewLIst, LNewList
      integer :: iBegNum, iEndNum, iDigCh, IADigCh, iNum, NDigits
      integer, dimension (2) :: iBeg, iEnd, LSeq
      character (len=5) :: NewSeq
      character (len=2) :: Seq
      character (len=1) :: NextCh, DigCh
      character (len=len(InText)) :: OutText
      character (len=*), parameter ::
     .   EscCh   = '\',
     .   DefList = '\n' // LF
     .          // '\t' // HT
     .          // '\v' // VT
     .          // '\b' // BS
     .          // '\r' // CR
     .          // '\f' // FF
     .          // '\a' // BEL
     .          // '\?' // '?'
     .          // "\'" // "'"
     .          // '\"' // '"'
     .          // '\\' // '\'

      LText = len ( InText )
      if ( LText .le. 0 ) return  ! If blank, nothing to do

!     Implement option to add list of new sequences
!     ---------------------------------------------
      new_list = .false.
      if ( present ( NewList )) then
         new_list = .true.
         LNewList = len ( NewList )
      end if

      iBeg  = 1
      do iChar = 1, LText

!        Scan for next escape character
!        ------------------------------
         LSeg  = scan ( InText ( iBeg(1) : ), EscCh ) - 1
         iEnd  = iBeg + LSeg - 1

!        If no more escape character exist or
!           at end of input text is reached then ...
!        -------------------------------------------
         if ( LSeg .lt. 0 .or.
     .        iEnd(1) + 1 .eq. LText ) then
            OutText ( iBeg(2) : )
     .               = InText ( iBeg(1) : )   ! ... process remaining text
            exit                              ! ... and exit

         else ! ...
!        ----------
            NextCh   = InText ( iEnd(1) + 2 : iEnd(1) + 2 )
            IANextCh = iachar ( NextCh )
            Seq      = EscCh // NextCh        ! ... search for sequence in the
            iDefList = index ( DefList, Seq ) ! ... default list
            iNewList = 0                      ! ... list of new sequences (if
            if ( new_list )  iNewList = index ( NewList, Seq )     ! desired)
            if (IANextCh .gt. IACharMax .or.  ! ... and insure that the
     .          IANextCh .lt. IACharMin ) then!     integer representation
               iNewList = 0                   !     for the next character is
               iDefList = 0                   !     valid
            end if

!           Determine the new character for the case when the sequence is ...
!           -----------------------------------------------------------------
            if      ( iNewList .gt. 0 ) then  !!! ... in the defined list
               if ( iNewList + 2 .le. LNewList ) then
                  NewSeq  = NewList ( iNewList + 2 : iNewList + 2 )
                  LSeq(1) = 2
                  LSeq(2) = 1
               else
                  NewSeq  = EscCh // NextCh
                  LSeq    = 2
               end if
            else if ( iDefList .gt. 0 )  then !!! ... in the default list
               NewSeq  = DefList ( iDefList + 2 : iDefList + 2 )
               LSeq(1) = 2
               LSeq(2) = 1
            else if ( NextCh .eq. 'd' .or.    !!! ... is a decimal
     .                NextCh .eq. 'x' .or.    !!! ... or hexadecimal
     .                octal( IANextCh )) then !!! ... or octal representation
                                              !!!     of an ASCII character
!              If so, isolate the characters representing the code
!              ---------------------------------------------------
               iBegNum = iEnd(1) + 3
               if ( octal ( IANextCh ) ) iBegNum = iEnd(1) + 2
               iEndNum = iBegNum + 2
               if ( NextCh .eq. 'x' )   iEndNum = iBegNum + 1
               iEndNum = min ( iEndNum, LText )

!              ... and calculate the integer code for the ASCII character
!              ----------------------------------------------------------
               NDigits = 0
               iNum    = 0
               do iDigCh = iBegNum, iEndNum
                  DigCh   = InText ( iDigCh : iDigCh )
                  IADigCh = max ( min ( iachar ( DigCh ),
     .                                  IACharMax ), IACharMin )
                  if      ( NextCh .eq. 'x' ) then
                     if ( .not. hex     ( IADigCh ) ) exit
                     iNum = HexTab ( iNum )
                  else if ( NextCh .eq. 'd' ) then
                     if ( .not. decimal ( IADigCh ) ) exit
                     iNum = DecTab ( iNum )
                  else 
                     if ( .not. octal   ( IADigCh ) ) exit
                     iNum = OctTab ( iNum )
                  end if
                  NDigits = NDigits + 1
                  iNum    = iNum + Ch2Num ( IADigCh )
               end do

!              ... and finally determine the character (if code is valid)
!              ----------------------------------------------------------
               iEndNum = iBegNum + NDigits - 1
               LSeq    = iEndNum - iEnd(1)
               if ( iNum .ge. IACharMin .and.
     .              iNum .le. IACharMax ) then 
                  LSeq(2) = 1
                  NewSeq  = achar ( iNum )
               else ! ... invalid code
                  NewSeq  = EscCh // InText ( iEnd(1) + 2 : iEndNum )
               end if

            else                              !!! ... not valid
               NewSeq  = EscCh // NextCh
               LSeq    = 2

            end if

!           Copy previous segment and append new character to output string
!           ---------------------------------------------------------------
            OutText ( iBeg(2) : iEnd(2) + LSeq(2) )
     .         =  InText ( iBeg(1) : iEnd(1) ) // NewSeq ( : LSeq(1) )
         end if

!        Set pointer to beginning of next segment of input and output text
!        -----------------------------------------------------------------
         iBeg = iEnd + LSeq + 1
      end do

!     Return output text
!     ------------------
      EscC = OutText

      return
      end function EscC

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Len_Line () --- String length of line
! 
! !DESCRIPTION:
!     This function returns the length of a line or the position of the
!     last character before a new marker.  If no new line marker is 
!     found then the result of the intrinsic function len is returned.
!
! !INTERFACE:
!
      function Len_Line ( Text, LenPar )
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in) ::
     .   Text            ! String of text data.
      logical, optional, intent (in) ::
     .   LenPar          ! = .true. = to return the length of a paragraph
!
! !OUTPUT PARAMETERS: 
      integer ::
     .   Len_Line         ! Length of line
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     31Jul2002  Redder    Original code
!
! EOP
!-------------------------------------------------------------------------
      logical :: get_ParLen

      get_ParLen = .false.
      if ( present ( LenPar )) get_ParLen = LenPar

      if ( get_ParLen ) then
         Len_Line = scan ( Text, NPList ) - 1
      else 
         Len_Line = scan ( Text, NLList ) - 1
      end if
      if ( Len_Line .lt. 0 ) Len_Line = len ( Text )

      return
      end function Len_Line
!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE: Indent () --- Insert blanks at the beginning of the line
! 
! !DESCRIPTION:
!     This function inserts blanks to a given text based on given marker and
!     spacing length.  If the marker indicates a new paragraph and the
!     spacing length is positive (normal indentation), or if the marker
!     indicates a new line but not a new paragraph and the spacing length
!     is negative (reverse or hanging indentation), then leading blanks are
!     inserted.  Otherwise, the function inserts trailing blanks.
!
! !INTERFACE:
      function Indent ( Text, Marker, LSp )
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in) ::
     .   Text            ! String of text data.
      character (len=1), intent (in) ::
     .   Marker          ! New line marker.  Blank denotes beginning
                         !   of text (i.e. beginning of first paragraph)
      integer,           intent (in) ::
     .   LSp             ! = spacing length (in number of blank characters)
                         !   assumed for indenting a paragraph.  LSp < 0 then
                         !   reverse (or hanging) indentation is assumed.
                         !   Default: LSp = 0.
!
! !OUTPUT PARAMETERS: 
      character (len = len(Text) + abs (Lsp)) ::
     .   Indent          ! String with inserted blanks
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     01Aug2002  Redder    Original code
!
! EOP
!-------------------------------------------------------------------------
      logical :: soft_break, pgraph_break, new_line
      character (len=abs(LSp)) :: BlankStr

      new_line     = index ( NLList,       Marker ) .gt. 0
      pgraph_break = index ( NPList // SP, Marker ) .gt. 0
      soft_break   = new_line .and. .not. pgraph_break

      BlankStr     = SP
      if      ( pgraph_break .and. LSp .gt. 0 ) then
         Indent = BlankStr // Text
      else if ( soft_break   .and. LSp .lt. 0 ) then
         Indent = BlankStr // Text
      else
         Indent = Text
      end if

      return
      end function Indent
!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Wrap () --- Inserts carriage returns for word wrap around.
! 
! !DESCRIPTION:
!     This function returns the input string with carriage returns
!     (=\verb|achar(13)|) as line separators in word wrap around.  Each
!     line separator is placed just prior to the first non-blank character
!     of the each line.  The function recognizes the any pre-existing line
!     feeds (=\verb|achar(10)|) as paragraph markers.  Any pre-existing
!     carriage return is assumed to mark the beginning of a new line but
!     not a new paragraph.  Each horizontal tab (=\verb|achar(9)|) is
!     treated as a blank.
!
! !INTERFACE:
!
      function Wrap ( Text, LLMax, LSp )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      character (len=*), intent (in) ::
     .   Text            ! String of text data.
      integer,           intent (in) ::
     .   LLMax           ! Maximum string length of the line (or segment).
      integer, optional, intent (in) ::
     .   LSp             ! = spacing length (in number of blank characters)
                         !   assumed for indenting a paragraph.  LSp < 0 then
                         !   reverse (or hanging) indentation is assumed.
                         !   Default: LSp = 0.
! !OUTPUT PARAMETERS: 
!
      character (len = len(Text)) ::
     .   Wrap            ! Out text with inserted line feeds.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     13Aug2001  Redder    Origional code.
!     02Jul2002  Redder    Removed FF and VT as markers for line breaks.
!                          Generalized test for paragraph markers.
! EOP
!-------------------------------------------------------------------------

      integer :: iChT, iBOS, LOS, iEOS, LSpPar, LText, LSpace
      integer :: iChS, iBOL, LOL, iEOL, LSpWr
      logical :: indent
      character (len=1) :: Char
      integer, dimension (2) :: LineNav

      LText = len    ( Text )
      if ( LText .eq. 0 ) return                   ! No text to process
      LText = verify ( Text, SP // HT,             ! Locate end of text
     .                 back = .true. )             ! ... excluding tailing
                                                   !     blanks and tabs
!     Set spacing length for the ...
!     ------------------------------
      LSpPar = 0
      LSpWr  = 0
      if ( present ( LSp ) ) then
         if ( LSp .gt. 0 ) LSpPar =  LSp           ! ... first line 
         if ( LSp .lt. 0 ) LSpWr  = -LSp           ! ... all other lines
      end if                                       !     in each paragraph

      indent  = .true.
      iBOS    =  1
      Wrap    =  Text
      do iChT =  1, LText

!        Determine next segment by
!        -------------------------
         LOS  = scan ( Text ( iBOS: ), NLList ) - 1! ... next new line char
         if ( LOS .lt. 0 ) LOS = LText - iBOS + 1  ! ... trailing blanks/tabs
         iEOS = iBOS + LOS - 1

!        Within the segment ...
!        ----------------------
         iBOL = iBOS
         do iChS = iBOS, iEOS
            LSpace  = LSpWr
            if ( indent ) LSpace = LSpPar
            LineNav = WrapRange ( Text ( iBOL : iEOS ),
     .                            LLMax - LSpace,  ! Define the next line of
     .                            indent = indent) ! wrapped text
            LOL  = LineNav(2)
            if ( LOL .eq. 0 ) exit                 ! Exit loop if no more 
                                                   !   text in the section
            iBOL = iBOL + LineNav(1) - 1
            iEOL = iBOL + LOL - 1
            if ( iChS .gt. iBOS )                  ! Unless already marked   
     .         Wrap  ( iBOL - 1 : iBOL - 1 ) = CR  !   by a new line char ...
                                                   ! ... add CR to mark the
            indent = .false.                       !     start of this line
            iBOL   =  iEOL + 1

         end do

         if ( iEOS .ge. LText ) exit               ! Exit loop if no more
         indent = .false.                          ! Don't begin new paragraph
         Char   =  Text ( iEOS + 1 : iEOS + 1 )    ! ... unless preceeded by
         if ( index ( NPList, Char ) .gt. 0 )      !     a paragraph marker 
     .      indent = .true.                       
         iBOS = iEOS + 2                           ! Account for new line
                                                   !   character
      end do

      return
      end function Wrap 
!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  WrapRange () --- Defines first segment for wrapped text.
! 
! !DESCRIPTION:
!     This function returns the navigators that define the first line (or
!     segment) of wrapped text (or line).  The line begins with the first
!     non-blank/non-tab character (unless the input argument \verb|indent|
!     = \verb|.true.|)  The length of the segment will not exceed the given
!     maximum (unless the first word is too long) and is set so that no
!     word is split between two segments.  If the line contains only
!     blanks or tabs, then zeroes are returned.
!
! !INTERFACE:
!
      function WrapRange ( Text, LLMax, indent )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      character (len=*), intent (in) ::
     .   Text            ! String of text data.
      integer,           intent (in) ::
     .   LLMax           ! Maximum string length of the line (or segment).
      logical, optional, intent (in) ::
     .   indent          ! = .true. to assume that the input text is to be
                         !   indented by including the leading blanks.
                         !   Default: indent = .false.
!
! !OUTPUT PARAMETERS: 
!
      integer,           dimension (2) ::
     .   WrapRange       ! (1) = string position of the beginning of the line
                         ! (2) = length of segment or length of the first
                         !       word if it exceeds the maximum length
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     13Aug2001  Redder    Origional code.
!
! EOP
!-------------------------------------------------------------------------

      integer :: iCh,   iChMax, LText, iBOT, LOT, LOW
      integer :: IACh0, IACh1,  IAChMax, NLeadBLKs
      integer, parameter ::
     .   IASp = IAChar ( SP ),
     .   IAHT = IAChar ( HT )

      WrapRange = 0                         ! Default

!     Search for first non-blank character ...
!     ----------------------------------------
      LText     = len    ( Text )
      iBOT      = verify ( Text, SP // HT )
      if ( iBOT .eq. 0 ) return             ! Text string contains only blanks
                                            !   or tabs
      NLeadBLKs = 0                         ! No leading blanks is assumed
      if ( present ( indent ) ) then        ! ... unless the text is to be
         if ( indent ) NLeadBLKs = iBOT - 1 !     indented
      end if

!     ... and determine the last possible end of the line
!     ---------------------------------------------------
      iChMax  = min    ( LText, max ( 0, LLMax - NLeadBLks ) + iBOT )
      IAChMax = IAChar (  Text ( iChMax : iChMax ))

!     Does the end go beyond the end of the input
!     string that ends with a non-blank character?
!     --------------------------------------------
      if ( iChMax .eq. LText .and. IAChMax .ne. IASp ) then
         LOT = iChMax - iBOT + 1            ! If so, segment length is easily
                                            !   determined
      else !                                ! If not, search for ...
!     ------
         LOT = 0
         ScanLoop : do iCh = iChMax, iBOT + 1, -1
            IACh1 = IAChar ( Text ( iCh : iCh ) ) 
            if ( IACh1 .eq. IASp .or.       ! ... the last blank ...
     .           IACh1 .eq. IAHT ) then     ! ... or  last tab ...
               IACh0 = IAChar ( Text ( iCh - 1 : iCh - 1 ) )
               if ( IACh0 .ne. IASp .and.   ! ... that follows a non-blank ...
     .              IACh0 .ne. IAHT ) then  ! ... and non-tab ...
                  LOT = iCh - iBOT          ! ... to determine the segment
                  exit ScanLoop             !     length
               end if
            end if

         end do ScanLoop

!        Is first word too long to fit?
!        ------------------------------
         if ( LOT .eq. 0 ) then             ! ... If so, return the string
            LOT  = LText - iBOT + 1         !     length of the word, instead.
            LOW  = scan ( Text ( iBOT + 1 : ), SP // HT )
            if ( LOW .gt. 0 ) LOT = LOW

         end if
      end if

      WrapRange (1) = iBOT - NLeadBLKs      ! Account for any leading blanks
      WrapRange (2) =  LOT + NLeadBLKs

      return
      end function WrapRange

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  LText () --- Returns the length of the word processed text
! 
! !DESCRIPTION:
!
! !INTERFACE:
!
c      function LText ( WPParm, Text,
c     .                 i0,i1,i2,i3,i4,i5,i6,i7,i8,i9 )
!
! !INPUT PARAMETERS: 
!
c      implicit NONE
c      type (par_info),     intent (inout) ::
c     .   WPParm            ! Word processing parameters
c      character (len = *), intent (in) ::
c     .   Text              ! Text of the paragraph to be scanned.
c      integer,   optional, intent (in) ::
c     .   i0,i1,i2,i3,i4,i5,i6,i7,i8,i9
c     .                     ! Integers referenced by the input text
!
! !INPUT PARAMETERS: 
!
c      integer :: LText     ! Length of processed text.

!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     24Aug2001  Redder    Origional code.
!
! EOP
!-------------------------------------------------------------------------
c      integer :: iChT, LenText
c      integer :: iChP, iBOP, iEOP
c      type (wp_status)  :: This
c      character (len=len(Text)) :: WText, PText
c      character (len=1) :: Ch

c      LText   = len ( Text )
c      WText   = EscC     (  Text, NewList = ComList )
c      WText   = Replace  ( WText, BlkList, SP )
c      LenText = len_trim ( WText )
c      LText   = 0
c      iBOP    = 1
c      do iChT = 1, LenText
c         LOP  = scan ( Text ( iBOP : LenText ), LF ) - 1
c         if ( LOP .lt. 0 ) LOP = LenText - iBOP + 1
c         iEOP = iBOP + LOP - 1
c         do iChP = 1, LOP
c            iCom = scan ( Text ( iEOC + 1 : ))
c            if ( iCom .le. 0 ) exit
c            iBOC = iCom + iEOC

c         end do
c         if ( LOP .ge. LenText ) exit 
c      end do

c      return
c      end function LText
!....................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Replace() --- Substitute characters
! 
! !DESCRIPTION:
!      Substitue all characters in a given set with a single character.
!
! !INTERFACE:
!
      function  Replace ( Str, Set, NewChar )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      character (len=*), intent (in) :: Str     ! String to be modified
      character (len=*), intent (in) :: Set     ! Set of characters to be
                                                !   substituted with ...
      character (len=1), intent (in) :: NewChar ! ... this new character
!
! !OUTPUT PARAMETERS:
!
      character ( len = len (Str) )  :: Replace ! Modified string
!
!
! !REVISION HISTORY: 
!
!     05Feb2001  C. Redder   Initial code
!
! EOP
!-------------------------------------------------------------------------

      integer :: iChar, LStr, IAStr ! iBeg, iScan,
      integer :: IASet, IASet1, IASet2, IASet3, LSet
      logical, dimension ( IACharMin : IACharMax ) :: appears_in_set

      Replace = Str
      LStr = len ( Str )
      LSet = len ( Set )

!     Substitute if the set size if one
!     ---------------------------------
      if      ( LSet .eq. 1 ) then
         IASet1 = IAChar ( Set )
         do iChar = 1, LStr
            IAStr = IAChar ( Str ( iChar : iChar ) )
            if ( IAStr .eq. IASet1 ) Replace ( iChar : iChar ) = NewChar

         end do

!     ... two
!     -------
      else if ( LSet .eq. 2 ) then

         IASet1 = IAChar ( Set ( 1: 1 ) )
         IASet2 = IAChar ( Set ( 2: 2 ) )
         do iChar = 1, LStr
            IAStr = IAChar ( Str ( iChar : iChar ) )
            if ( IAStr .eq. IASet1 .or.
     .           IAStr .eq. IASet2 ) Replace ( iChar : iChar ) = NewChar

         end do

!     ... three
!     ---------
      else if ( LSet .eq. 3 ) then

         IASet1 = IAChar ( Set ( 1 : 1 ) )
         IASet2 = IAChar ( Set ( 2 : 2 ) )
         IASet3 = IAChar ( Set ( 3 : 3 ) )
         do iChar = 1, LStr
            IAStr = IAChar ( Str ( iChar : iChar ) )
            if ( IAStr .eq. IASet1 .or.
     .           IAStr .eq. IASet2 .or.
     .           IAStr .eq. IASet3 ) Replace ( iChar : iChar ) = NewChar

         end do

!     ... greater than three
!     ----------------------
      else

!        Set up logical table
!        --------------------
         appears_in_set = .false.
         do iChar = 1, LSet
            IASet = IAChar ( Set ( iChar : iChar ) )
            if ( IASet .ge. IACharMin .and.
     .           IASet .le. IACharMax )
     .         appears_in_set ( IASet ) = .true.

         end do

!        ... and replace if ...
!        ----------------------
         ScanLoop : do iChar = 1, LStr
            IAStr  = IAChar ( Str ( iChar : iChar ) )
            if ( IAStr .ge. IACharMin .and.        ! ... corresponding
     .           IAStr .le. IACharMax ) then       !     entry in logical
            if ( appears_in_set ( IAStr ) )        !     table is true
     .         Replace ( iChar : iChar ) = NewChar ! --------------------
            end if

         end do ScanLoop

      end if

      end function Replace
!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  XML () --- Returns the contents between XML tags in text lines
! 
! !DESCRIPTION:
!     For each given text line, this routine gets the contents between two
!     Extensible Markup Language tags.  The tags are assumed to be of the
!     form, <TagName>, and </TagName> where TagName is the given tag name
!     and the backslash denotes the end tag.  If the routine finds a null
!     tag of the form, <TagName/>, then a blank string is returned for 
!     the corresponding text.  An error will result in a status code of N
!     where N is the line index number.
!
! !INTERFACE:
      subroutine XML ( Text, TagName, Contents, stat, start_req )
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in)  ::
     .   Text     (:),   ! Lines of text data with embedded XML tags
     .   TagName         ! Name of start and end tags
      logical, optional, intent (in)  ::
     .   start_req       ! = .true. if start tag is required regardless
                         !  of whether end tag is present
!
! !OUTPUT PARAMETERS:
      character (len=*), intent (out) ::
     .   Contents (:)    ! Lines of text data with embedded XML tags
      integer,           intent (out) ::
     .   stat            ! Returned status code.  0 = all is well.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     22Nov2002  Redder    Origional code.
!
! EOP
!-------------------------------------------------------------------------
      integer :: iBeg, LLen, iEnd, NLines, iLine
      integer, dimension (2) :: Nav
      logical :: start_tag_must_exist

      start_tag_must_exist = .false.        ! Impliment option
      if ( present ( start_req )) start_tag_must_exist = start_req

      NLines = min ( size ( Text ), size ( Contents ))
      do iLine = 1, NLines
         Nav  = XMLNav ( Text ( iLine ), TagName )
         iBeg = Nav (1)                     ! Locate contents
         LLen = Nav (2)
         iEnd = iBeg + LLen - 1

         Contents ( iLine ) = ' '           ! Default output
         stat               = iLine

         if      ( iBeg .eq.  0 ) then      ! Start or null tag not found
            if ( start_tag_must_exist ) return

         else if ( LLen .eq. -1  .or.       ! End tag not found
     .             LLen .eq. -2 ) then      ! ... with no start tag
            return

         else
            Contents ( iLine ) = Text ( iLine ) ( iBeg : iEnd )

         end if
      end do

      stat = 0                              ! All is well
      return
      end subroutine XML

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  FXML () --- Returns the contents between XML tags
! 
! !DESCRIPTION:
!     This routine gets the contents between two Extensible Markup
!     Language tags which are assumed to be of the form, <TagName>, and
!     </TagName> where TagName is the given tag name and the backslash
!     denotes the end tag.  If the routine finds a null tag of the form,
!     <TagName/>, then a blank string is returned.  The function returns
!     with an message if an error occurrs.
!
! !INTERFACE:
      function FXML ( Text, TagName, stat, start_req )
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in)  ::
     .   Text,           ! String of text data with embedded XML tags
     .   TagName         ! Name of start and end tags
      logical, optional, intent (in)  ::
     .   start_req       ! = .true. if start or null tag is required or
!                        !   expected.  Default: start_req = .false.
! !OUTPUT PARAMETERS:
      character (len=*), parameter ::
     .   Error1 = 'Start (or null tag) not found',
     .   Error2 = 'End tag not found',
     .   Error3 = 'End tag with no start tag'
      integer,           intent (out) ::
     .   stat            ! Returned status code.  0 = all is well.
     .                   !   = 0 : all is 
     .                   !   = 1 : no begin or null tag
     .                   !   = 2 : no end tag
     .                   !   = 3 : end tag with no start tag 
      character (len = max(len(Text),
     .                     len(Error1),len(Error2),len(Error3))) ::
     .   FXML            ! The contents between the tags
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     22Nov2002  Redder    Origional code.
! EOP
!-------------------------------------------------------------------------
      integer :: iBeg, LLen, iEnd
      integer, dimension (2) :: Nav
      logical :: start_tag_must_exist

      start_tag_must_exist = .false.     ! Impliment option
      if ( present ( start_req )) start_tag_must_exist = start_req

      Nav  = XMLNav ( Text, TagName )    ! Locate contents
      iBeg = Nav (1)
      LLen = Nav (2)
      iEnd = iBeg + LLen - 1

      FXML = ' '                         ! Default output
      stat = 0

      if      ( iBeg .eq.  0 ) then      ! Start or null tag not found
         if ( start_tag_must_exist ) then
            FXML = Error1
            stat = 1
         end if
      else if ( LLen .eq. -1 ) then      ! End tag not found
         FXML = Error2
         stat = 2
      else if ( LLen .eq. -2 ) then      ! End tag with no start tag
         FXML = Error3
         stat = 3
      else
         FXML = Text ( iBeg : iEnd )
         stat = 0
      end if

      return
      end function FXML
!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  XMLNav () --- Returns the navigator for the contents between XML tags
! 
! !DESCRIPTION:
!     This routine returns the navigators for the contents between two 
!     Extensible Markup Language tags which are assumed to be of the form,
!     <TagName>, and </TagName> where TagName is the given tag name and the
!     backslash denotes the end tag.  If the routine finds a null tag of
!     the form, <TagName/>, then 1 and 0 is returned for the location and
!     length of the null string.  Zero is returned for the location, if the
!     start and null tags are not found and -1 is assigned for the length
!     if the end tag is not found.  If an end tag is found with no 
!     corresponding start and null tags, then the length is set to -2
!     and location set to 1.
!     
!
! !INTERFACE:
      function XMLNav ( Text, TagName )
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in)  ::
     .   Text,           ! String of text data with embedded XML tags
     .   TagName         ! Name of start and end tags
!
! !OUTPUT PARAMETERS:
      integer,           dimension (2) ::
     .   XMLNav          ! Returned navigators
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     22Nov2002  Redder    Origional code.
!     06Jun2003  Redder    Fixed bug (LTag_Trim was not initialized)
! EOP
!-------------------------------------------------------------------------
      integer :: LText, LTag, iBegTag, iEndTag, iTagLev, LTag_Trim
      integer :: iBeg,  iEnd, iCh
      logical :: prev_nonblank
      character (len=1)              :: Ch, LastMkr
      character (len=len(Text))      :: Tag
      character (len=len(TagName)+2) :: StartTag
      character (len=len(TagName)+3) :: EndTag, NullTag

      iBegTag  = verify   ( TagName, ' ' )
      iEndTag  = len_trim ( TagName )
      StartTag = '<'  // TagName ( iBegTag : iEndTag ) //  '>'
      EndTag   = '</' // TagName ( iBegTag : iEndTag ) //  '>'
      NullTag  = '<'  // TagName ( iBegTag : iEndTag ) // '/>'

      LText     = len_trim ( Text )
      LTag      = 0
      LastMkr   = ' '
      iTagLev   = 0
      LTag_Trim = 0
      prev_nonblank = .false.
      do iCh = 1, LText
         Ch = Text ( iCh : iCh )
         if ( Ch .ne. '<' .and.
     .        Ch .ne. '/' .and.
     .        Ch .ne. '>' ) then 
            if     ( Ch .ne. ' '    ) then
               LTag                 = LTag + 1      ! Add non-blank char to
               Tag  ( LTag : LTag ) = Ch            !   tag
               prev_nonblank        = .true.
               LTag_Trim            = LTag

            else if ( prev_nonblank ) then
               LTag                 = LTag + 1      ! Preserve intermediate
               Tag  ( LTag : LTag ) = Ch            !   blanks in tag name

            end if

         else if ( Ch .eq. '<' ) then
            iBegTag = iCh                           ! Begin new tag
            LastMkr =  Ch
            LTag    = 1
            Tag  ( LTag : LTag )    = Ch
            prev_nonblank           = .false.
            LTag_Trim               = LTag

         else if ( Ch .eq. '/' ) then               ! Add XML slash and
            LTag                    = LTag_Trim + 1 !   remove nearby blanks 
            Tag ( LTag : LTag )     = Ch
            LTag_Trim               = LTag

         else if ( Ch .eq. '>' ) then
            LTag           = LTag_Trim + 1          ! End of tag
            Tag ( : LTag ) = Tag ( : LTag_Trim ) // Ch
            if ( LastMkr .eq. '<' ) then
               if      ( Tag ( : LTag ) .eq. StartTag ) then
                  iTagLev    = 1                    ! Start tag found
                  iBeg       = iCh + 1
               else if ( Tag ( : LTag ) .eq. EndTag   ) then
                  if      ( iTagLev .eq. 1 ) then   ! End tag found after
                     iEnd    = iBegTag - 1          !   start tag so return
                     XMLNav (1) = iBeg              !   with navigator
                     XMLNav (2) = iEnd - iBeg + 1
                     return
                  else if ( iTagLev .eq. 0 ) then   ! End tag with no
                     XMLNav (1) =  1                !   corresponding
                     XMLNav (2) = -2                !   start tag
                     return
                  end if
               else if ( Tag ( : LTag ) .eq. NullTag  ) then
                  XMLNav (1) = 1                    ! Null tag found
                  XMLNav (2) = 0
                  return
               end if
            end if
            LastMkr   = Ch
            LTag      = 0
            LTag_Trim = LTag
         end if

      end do
      XMLNav (1) = 0                 ! Start or null tag not found
      XMLNav (2) = 0
      if ( iTagLev .ne. 0 ) then     ! End tag not found
         XMLNav (1) =  iBeg
         XMLNav (2) = -1
      end if

      return
      end function XMLNav
!...................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  PrepList --- Prepare list
! 
! !INTERFACE:
      function PrepList ( List, ItSep )
!
! !USES
!

! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in) ::
     .   List       ! Input list 
      character (len=*), intent (in), optional ::
     .   ItSep      ! Item separator.  Default: ItSep = ','.  If
                    !   blank, then default seperator is used.
!
! !OUTPUT PARAMETERS:
      character (len=len(List))    ::
     .   PrepList   ! Modified list

! !DESCRIPTION:
!     Checks and prepares list of attributes by stripping out intermediate
!     blanks and unnecessary item seperators.
!
! !REVISION HISTORY: 
!     24Sep2002  C Redder   Initial code.
!
!EOP
!-------------------------------------------------------------------------
      integer :: iCh1, iCh2, iCh2_Next, NCh1, LVar
      character (len=len(List))   :: Var
      character (len=len(List)+1) :: InList
      character (len=1) :: Ch1, Sep
      character (len=1), parameter :: DefSep = ','

      Sep      = DefSep
      if ( present ( ItSep )) Sep = ItSep
      if ( Sep .eq. ' '     ) Sep = DefSep
      InList   = List // Sep   
      PrepList = ' '
      NCh1     = len_trim ( List )
      LVar     = 0
      iCh2     = 1
      do iCh1 = 1, NCh1 + 1
         Ch1 = InList ( iCh1 : iCh1 )
         if (  Ch1 .ne. ' '  ) then       ! Skip over blanks
         if (  Ch1 .ne. Sep  ) then       ! If not list entry delimiter
            LVar = LVar + 1               !   then add character to variable
            Var ( LVar : LVar ) = Ch1     !   name stored in scratch space
         else
            if ( LVar .gt. 0 ) then       ! Skip over blank variable
               if ( iCh2 .gt. 1 ) then    ! If not first entry then
                  PrepList ( iCh2 : iCh2 ) = Sep
                  iCh2   = iCh2 + 1       ! ... add preceding comma. 
               end if
               iCh2_Next = iCh2 + LVar    ! ... to each entry in the output
               PrepList ( iCh2 : iCh2_Next - 1 ) = Var ( : LVar ) ! list
               iCh2      = iCh2_Next
            end if
            LVar = 0

         end if
         end if 
      end do

      return
      end function PrepList

!...................................................................
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: UpperCase - convert lowercase letters to uppercase.
!
! !DESCRIPTION:
!
! !INTERFACE:
      function UpperCase ( str ) result ( ustr )
!
! !INPUT PARAMETERS:
      implicit none
      character ( len = * ), intent ( in ) ::  str
!
! !OUTPUT PARAMETERS:
      character ( len = len ( str ))       :: ustr

! !REVISION HISTORY:
!     13Aug1996   J. Guo      Original code
!     11Jul2003   C. Redder   Adapted to this module
!EOP
!-----------------------------------------------------------------------
      integer :: i
      integer, parameter :: il2u = ichar('A') - ichar('a')

      ustr = str
      do i = 1, len_trim ( str )
         if ( str ( i : i ) .ge. 'a' .and.
     .        str ( i : i ) .le. 'z' )
     .       ustr ( i : i ) = char ( ichar ( str ( i : i )) + il2u )
      end do
      end function UpperCase

!...................................................................
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: LowerCase - convert uppercase letters to lowercase.
!
! !DESCRIPTION:
!
! !INTERFACE:

      function LowerCase ( str ) result ( lstr )
!
! !INPUT PARAMETERS:
      implicit none
      character ( len = * ), intent ( in ) :: str
!
! !OUTPUT PARAMETERS:
      character ( len = len ( str ))       :: lstr

! !REVISION HISTORY:
!     13Aug1996   J. Guo      Original code
!     11Jul2003   C. Redder   Adapted to this module
!EOP
!-----------------------------------------------------------------------
      integer :: i
      integer, parameter :: iu2l = ichar ('a') - ichar('A')

      lstr = str
      do i = 1, len_trim ( str )
         if ( str ( i : i ) .ge. 'A' .and.
     .        str ( i : i ) .le. 'Z' )
     .       lstr ( i : i ) = char ( ichar ( str ( i : i )) + iu2l )
      end do
      end function LowerCase
!...................................................................
      end module m_TextUtil

!====================================================================
