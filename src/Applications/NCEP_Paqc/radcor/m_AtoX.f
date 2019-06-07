*====================================================================

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_AtoX -- Library of conversion routines
!
! !DESCRIPTION:
!     This module contains the routines for converting strings to integer,
!     floating point numbers, or double precision numbers.  Also included
!     are routines for generating error messages and routine names based
!     on returned status codes.
!
! !INTERFACE:
!
      module      m_AtoX
      implicit    NONE
      private	! except

*     Subroutine and functions
*     ------------------------
      public ::   AtoI    ! Converts token string to integer 
      public ::   AtoF    ! Converts token string to float
      public ::   AtoD    ! Converts token string to double precision
      public ::   AtoXErr ! Generates error message
      public ::   AtoXWh, ! Returns routine name where error occurred
     .            AtoXWh2

      interface   AtoI
         module procedure ! function parameters
     .      AtoI_Str,     ! ... ( char_token, stat )
     .      AtoI_ACh1     ! ... ( ach1_token, stat )
                          ! char_token = character (len=*)
      end interface       ! ach1_token = character (len=1), dimension(:)
                          ! stat       = integer ! returned error status

      interface   AtoF
         module procedure ! function parameters
     .      AtoF_Str,     ! ... ( char_token, stat )
     .      AtoF_ACh1     ! ... ( ach1_token, stat )

      end interface

      interface   AtoD
         module procedure ! function parameters
     .      AtoD_Str,     ! ... ( char_token, stat )
     .      AtoD_ACh1     ! ... ( ach1_token, stat )

      end interface

      interface   AtoXErr
         module procedure ! function parameters
     .      AtoXErr_Str,  ! ... ( char_token, stat )
     .      AtoXErr_ACh1  ! ... ( ach1_token, stat )
      end interface       ! where = character (len=*) ! routine where the
                                                      !   error occurred
!     Error status codes and messages
!     -------------------------------
      integer, parameter ::
     .   No_Error        =  0      ! Valid status code; no error
      integer, parameter :: 
     .   IConvErr        =  1      ! Error in converting ASCII to integer
      integer, parameter :: 
     .   IOverflow       =  2      ! Integer overflow
      integer, parameter :: 
     .   FConvErr        =  3      ! Error in converting ASCII to real
      integer, parameter :: 
     .   FOverflow       =  4      ! Floating point overflow
      integer, parameter :: 
     .   FUnderflow      =  5      ! Floating point underflow
      integer, parameter :: 
     .   DConvErr        =  6      ! Error in converting ASCII to double
      integer, parameter :: 
     .   DOverflow       =  7      ! Double precision overflow
      integer, parameter :: 
     .   DUnderflow      =  8      ! Double precision underflow
      integer, parameter :: stat_max = 8
!
! !REVISION HISTORY:
!     01Feb2001  C. Redder  Original version
!
! EOP
!-----------------------------------------------------------------
!
! !DEFINED PARAMETERS: 
!
!     Special character for general use.
!     ----------------------------------
      character (  len  =   1 ), parameter ::
     .   BLK    = achar (  32 )    ! blank
 
!     Error handling parameters
!     -------------------------
      character (len=*), parameter :: MyModule = 'm_AtoX'
      character (len=*), dimension (stat_max), parameter ::
     .   messages_part1 =
     .   (/ 'Conversion error; string =           ',
     .      'Integer overflow; number =           ',
     .      'Conversion error; string =           ',
     .      'Floating point overflow; number =    ',
     .      'Floating point underflow; number =   ',
     .      'Conversion error; string =           ',
     .      'Double precision overflow; number =  ',
     .      'Double precision underflow; number = ' /)
      character (len=*), dimension (stat_max), parameter ::
     .   MyNames  = (/ MyModule // '::AtoI_Str',
     .                 MyModule // '::AtoI_Str',
     .                 MyModule // '::AtoF_Str',
     .                 MyModule // '::AtoF_Str',
     .                 MyModule // '::AtoF_Str',
     .                 MyModule // '::AtoD_Str',
     .                 MyModule // '::AtoD_Str',
     .                 MyModule // '::AtoD_Str' /)
      character (len=*), dimension (stat_max), parameter ::
     .   MyNames2 = (/ MyModule // '::AtoI_ACh1',
     .                 MyModule // '::AtoI_ACh1',
     .                 MyModule // '::AtoF_ACh1',
     .                 MyModule // '::AtoF_ACh1',
     .                 MyModule // '::AtoF_ACh1',
     .                 MyModule // '::AtoD_ACh1',
     .                 MyModule // '::AtoD_ACh1',
     .                 MyModule // '::AtoD_ACh1' /)

!     Parameters regarding native integers ...
!     ----------------------------------------
      integer, parameter ::
     .   NIntDigits_Max  = range ( 1 ) + 1,  ! Maximum number of digits
     .   Near_IntMax     = huge  ( 1 ) / 10  ! One tenth of maximum value
      integer, parameter ::                  ! Lead digit at maximum value
     .   IntDigit_Max    = mod   ( huge ( 1 ), Near_IntMax )

!     ... and real numbers
!     --------------------                   ! Maximum number of digits in
      integer, parameter ::                  ! ... mantessa
     .   NFltDigits_Max  = min ( precision ( 1.0 ) + 1, 17 )
      integer, parameter ::                  ! ... and exponent
     .   NFExpDigits_Max = min ( NFLtDigits_Max, NIntDigits_Max / 2 ),
     .   MinFExp         = -range ( 1.0 ),   ! Minimum and
     .   MaxFExp         =  range ( 1.0 )    !   maximum value for exponents
      integer :: iFExp
      real,    parameter, dimension ( MinFExp : MaxFExp ) ::
     .   FExp10   = ( / ( 1.0d1 ** iFExp, iFExp = MinFExp, MaxFExp ) / )
                                             ! Table for powers of 10
      real,    parameter ::
     .   ManFHuge = huge ( 1.0 ) / FExp10 ( MaxFExp ), ! Mantessa for the
     .   ManFTiny = tiny ( 1.0 ) / FExp10 ( MinFExp )  !   extremes.

!     ... and double precision numbers
!     --------------------------------       ! Maximum number of digits in
      integer, parameter ::                  ! ... mantessa
     .   NDblDigits_Max  = min ( precision ( 1.0d0 ) + 1, 34 )
      integer, parameter ::                  ! ... and exponent
     .   NDExpDigits_Max = min ( NDblDigits_Max, NIntDigits_Max / 2 ),
     .   MinDExp         = -range ( 1.0d0 ), ! Minimum and
     .   MaxDExp         =  range ( 1.0d0 )  !   maximum value for exponents
      integer :: iDExp
      double precision, parameter, dimension ( MinDExp : MaxDExp ) ::
     .   DExp10   = ( / ( 1.0d1 ** iDExp, iDExp = MinDExp, MaxDExp ) / )
                                             ! Table for powers of 10
      double precision, parameter ::
     .   ManDHuge = huge ( 1.0d0 ) / DExp10 ( MaxDExp ), ! Mantessa for the
     .   ManDTiny = tiny ( 1.0d0 ) / DExp10 ( MinDExp )  !   extremes.

!     Defines boundaries of logical tables for characters
!     ---------------------------------------------------
      integer, parameter :: iCharMin = 0
      integer, parameter :: iCharMax = 127

!     Logical tables used to determine if a character is
!     legal for a floating point number
!     --------------------------------------------------
      integer            :: iLetter
      integer, parameter :: iBegLegal = 9 - iCharMin
      integer, parameter :: iEndLegal = iCharMax - 102 + 1
      logical, parameter, dimension ( iCharMin : iCharMax ) ::
     .         illegal     = ( / ( .true.,   iLetter = 1, iBegLegal ),
     .                             .false.,
     .                           ( .true.,   iLetter = 1, 22 ),
     .                             .false.,
     .                           ( .true.,   iLetter = 1, 10 ),
     .                             .false.,
     .                             .true.,
     .                           ( .false.,  iLetter = 1,  2 ),
     .                             .true.,
     .                           ( .false.,  iLetter = 1, 10 ),
     .                           ( .true.,   iLetter = 1, 10 ),
     .                           ( .false.,  iLetter = 1,  2 ),
     .                           ( .true.,   iLetter = 1, 30 ),
     .                           ( .false.,  iLetter = 1,  2 ),
     .                           ( .true.,   iLetter = 1, iEndLegal ) /)

!     ... a significant character (i.e. non-blank/tab)
!     ------------------------------------------------
      integer, parameter  :: iBegSig   =  43 - iCharMin
      integer, parameter  :: iEndSig   = iCharMax - 102 + 1
      logical, parameter, dimension ( iCharMin : iCharMax ) ::
     .         significant = ( / ( .false.,  iLetter = 1, iBegSig ),
     .                             .true.,
     .                             .false.,
     .                           ( .true.,   iLetter = 1,  2 ),
     .                             .false.,
     .                           ( .true.,   iLetter = 1, 10 ),
     .                           ( .false.,  iLetter = 1, 10 ),
     .                           ( .true.,   iLetter = 1,  2 ),
     .                           ( .false.,  iLetter = 1, 30 ),
     .                           ( .true.,   iLetter = 1,  2 ),
     .                           ( .false.,  iLetter = 1, iEndSig ) / )

!     ... a sign (i.e +/-)
!     --------------------
      integer, parameter  :: iBegSign  =  43 - iCharMin
      integer, parameter  :: iEndSign  = iCharMax - 46  + 1
      logical, parameter, dimension ( iCharMin : iCharMax ) ::
     .         sign        = ( / ( .false.,  iLetter = 1, iBegSign ),
     .                             .true.,
     .                             .false.,
     .                             .true.,
     .                           ( .false.,  iLetter = 1, iEndSign ) / )

!     ... signifying an exponent (i.e. D, d, E or e)
!     ----------------------------------------------
      integer, parameter  :: iBegExp   =  68 - iCharMin
      integer, parameter  :: iEndExp   = iCharMax - 102 + 1
      logical, parameter, dimension ( iCharMin : iCharMax ) ::
     .         exponent    = ( / ( .false.,  iLetter = 1, iBegExp ),
     .                           ( .true.,   iLetter = 1,  2 ),
     .                           ( .false.,  iLetter = 1, 30 ),
     .                           ( .true.,   iLetter = 1,  2 ),
     .                           ( .false.,  iLetter = 1, iEndExp ) / )

!     ... a number (i.e. 0-9)
!     -----------------------
      integer, parameter  :: iBegNum   =  48 - iCharMin
      integer, parameter  :: iEndNum   = iCharMax - 58  + 1
      logical, parameter, dimension ( iCharMin : iCharMax ) ::
     .         number      = ( / ( .false.,  iLetter = 1, iBegNum ),
     .                           ( .true.,   iLetter = 1, 10 ),
     .                           ( .false.,  iLetter = 1, iEndNum ) / )

!     ASCII code representation for ...
!     ---------------------------------
      integer, parameter  :: iZero    = iachar ( '0' ) ! ... zero
      integer, parameter  :: iNine    = iachar ( '9' ) ! ... nine
      integer, parameter  :: iDecPt   = iachar ( '.' ) ! ... decimal point
      integer, parameter  :: iNegSign = iachar ( '-' ) ! ... minus sign

      contains

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AtoI_Str() --- Convert ASCII (a multcharacter string) to an integer
! 
! !DESCRIPTION: 
!
!     This function converts a token (stored as a multicharacter string)
!     to an integer according to the FORTRAN format editing descriptors
!     \verb|'(BN,IL)'| where \verb|L| is the input string length.  In
!     other words, blanks (and tabs) are ignored.  If an error
!     occurs, then the returned integer is set to 0.
!
! !INTERFACE: 
!
      integer function AtoI_Str ( token, stat )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      character ( len = * ), intent (in)  ::
     .            token      ! string containing the number  
!
! !OUTPUT PARAMETERS:
!
      integer,               intent (out) ::
     .            stat       ! Returned status (error) code
!
! !REVISION HISTORY: 
!
!     31Jan2001  C Redder   Original code
!     22Feb2007  C Redder   Added a comment in the prologue.
!
! EOP
!-------------------------------------------------------------------------

      integer :: INum
      integer :: NChar, iChar, iChar1
      character ( len = 1 ) ::  Char1
      integer :: NNumbers
      integer :: SignFactor, NDigits, iLeadDigit, NextDigit
      logical :: sign_found, LeadDigit_Found

*     Set defaults
*     ------------
      stat     = 0
      AtoI_Str = 0

*     Initialize variables for the number of ...
*     ------------------------------------------
      NNumbers         =  0      ! ... characters, 0 - 9
      NDigits          =  0      ! ... all digits beyond the leading zeroes

*     ... positions within the input string of the ...
*     ------------------------------------------------
      iLeadDigit       =  0      ! ... lead digit for the nonexponent part

*     ... numbers for the ...
*     -----------------------
      INum             =  0      ! ... nonexponent part
      SignFactor       =  1      ! ... and its sign

*     ... logical flags indicating whether ...
*     ----------------------------------------
      LeadDigit_Found  = .false. ! ... the lead digit has been found
      Sign_Found       = .false. ! ... the sign has been found

*     Scan the token
*     --------------
      NChar = Len_Trim ( token )
      Scan : do iChar = 1, NChar
         stat   = IConvErr       ! Initially assume conversion error
                                 !   for this cycle
         Char1  = token  ( iChar : iChar )
         iChar1 = iachar (  Char1 )

*        Check for illegal characters
*        ----------------------------
         if ( iChar1 .lt. iCharMin .or.
     .        iChar1 .gt. iCharMax   ) exit Scan
         if ( illegal     ( iChar1 ) ) exit Scan

*        If non-blank/tab, then check for ...
*        ------------------------------------
         if ( significant ( iChar1 ) ) then

*           ... numbers that are ...
*           -------------------------
            if ( number ( iChar1 ) ) then
               NNumbers = NNumbers + 1

*              ... significant digits
*              ----------------------
               if ( iChar1 .ne. iZero ) then
                  LeadDigit_Found = .true.
                  if ( iLeadDigit .eq. 0 )
     .               iLeadDigit   =  NNumbers

               end if

*              After lead digit is found ...
*              -----------------------------
               if ( LeadDigit_Found ) then
                  NDigits       = NDigits + 1
                  NextDigit     = iChar1  - iZero

*                 ... convert all digits and accumualte the number
*                 ------------------------------------------------
                  if      ( NDigits .lt. NIntDigits_Max  ) then
                     INum       = 10 * INum  + NextDigit

*                 ... and check for overflows
*                 ---------------------------
                  else if ( NDigits .eq. NIntDigits_Max ) then
                     if      ( INum .lt. Near_IntMax ) then
                        INum    = 10 * INum  + NextDigit

                     else if ( INum .eq. Near_IntMax ) then
                        if ( NextDigit .le. IntDigit_Max ) then
                           INum = 10 * INum  + NextDigit

                        else
                           stat = IOverflow
                           exit Scan

                        end if

                     else
                        stat = IOverflow
                        exit Scan

                     end if

                  else
                     stat    = IOverflow
                     exit Scan

                  end if
               end if

*           ... sign ...
*           ------------
            else if ( sign ( iChar1 ) ) then

               if ( NNumbers .gt. 0 ) then
                  exit Scan                      ! The sign must be the
                                                 !   first non-blank
                                                 !   character
               else if ( Sign_Found ) then       ! No more than one sign
                  exit Scan                      !   is allowed

               else
                  if ( iChar1 .eq. iNegSign ) SignFactor = -1
c                  if ( Char1 .eq. '-' ) SignFactor = -1
                  Sign_Found = .true.

               end if

*           ... decimal point
*           -----------------
            else if ( iChar1 .eq. iDecPt ) then
c            else if ( Char1 .eq. '.' ) then
               exit Scan                         ! Decimal points and ...

*           ... character signifying beginning of exponent
*           ----------------------------------------------
            else if ( exponent ( iChar1 ) ) then ! ... exponent characters
               exit Scan                         !   are illegal

            end if
         end if

         stat = No_Error                         ! No conversion error
      end do Scan                                !   during this cycle

*     Return the result
*     -----------------
      if ( stat .eq. No_Error ) AtoI_Str = SignFactor * INum
      return

      end function AtoI_Str

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AtoI_ACh1() --- Convert ASCII (array of 1-character strings) to an integer
! 
! !DESCRIPTION: 
!
!     This function converts an array of one-character strings to an
!     integer according to the FORTRAN format editing descriptors 
!     \verb|'(BN,IL)'| where \verb|L| is the input string length.
!     In other words, blanks (and tabs) are ignored.  If an error
!     occurs, then the returned integer is set to 0.
!
! !INTERFACE: 
!
      integer function AtoI_ACh1 ( token, stat )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      character ( len = 1 ), intent (in), dimension (:) ::
     .            token      ! string containing the number  
!
! !OUTPUT PARAMETERS:
!
      integer,               intent (out) ::
     .            stat       ! Returned status (error) code
!
! !REVISION HISTORY: 
!
!     31Jan2001  C Redder   Original code
!     22Feb2007  C Redder   Added a comment in the prologue.
!
! EOP
!-------------------------------------------------------------------------

      integer :: INum
      integer :: NChar, iChar, iChar1
      character ( len = 1 ) ::  Char1
      integer :: NNumbers
      integer :: SignFactor, NDigits, iLeadDigit, NextDigit
      logical :: Sign_Found, LeadDigit_Found

*     Set defaults
*     ------------
      stat      = 0
      AtoI_ACh1 = 0

*     Initialize variables for the number of ...
*     ------------------------------------------
      NNumbers         =  0      ! ... characters, 0 - 9
      NDigits          =  0      ! ... all digits beyond the leading zeroes

*     ... positions within the input string of the ...
*     ------------------------------------------------
      iLeadDigit       =  0      ! ... lead digit for the nonexponent part

*     ... numbers for the ...
*     -----------------------
      INum             =  0      ! ... nonexponent part
      SignFactor       =  1      ! ... and its sign

*     ... logical flags indicating whether ...
*     ----------------------------------------
      LeadDigit_Found  = .false. ! ... the lead digit has been found
      Sign_Found       = .false. ! ... the sign has been found

*     Scan the token
*     --------------
      NChar = Ch1ArrLen_Trim ( token )
      Scan : do iChar = 1, NChar
         stat   = IConvErr       ! Initially assume conversion error
                                 !   for this cycle
         Char1  = token  ( iChar  )
         iChar1 = iachar (  Char1 )

*        Check for illegal characters
*        ----------------------------
         if ( iChar1 .lt. iCharMin .or.
     .        iChar1 .gt. iCharMax   ) exit Scan
         if ( illegal     ( iChar1 ) ) exit Scan

*        If non-blank/tab, then check for ...
*        ------------------------------------
         if ( significant ( iChar1 ) ) then

*           ... numbers that are ...
*           -------------------------
            if ( number ( iChar1 ) ) then
               NNumbers = NNumbers + 1

*              ... significant digits
*              ----------------------
               if ( iChar1 .ne. iZero ) then
                  LeadDigit_Found = .true.
                  if ( iLeadDigit .eq. 0 )
     .               iLeadDigit   =  NNumbers

               end if

*              After lead digit is found ...
*              -----------------------------
               if ( LeadDigit_Found ) then
                  NDigits       = NDigits + 1
                  NextDigit     = iChar1  - iZero

*                 ... convert all digits and accumualte the number
*                 ------------------------------------------------
                  if      ( NDigits .lt. NIntDigits_Max  ) then
                     INum       = 10 * INum  + NextDigit

*                 ... and check for overflows
*                 ---------------------------
                  else if ( NDigits .eq. NIntDigits_Max ) then
                     if      ( INum .lt. Near_IntMax ) then
                        INum    = 10 * INum  + NextDigit

                     else if ( INum .eq. Near_IntMax ) then
                        if ( NextDigit .le. Near_IntMax ) then
                           INum = 10 * INum  + NextDigit

                        else
                           stat = IOverflow
                           exit Scan

                        end if

                     else
                        stat = IOverflow
                        exit Scan

                     end if

                  else
                     stat    = IOverflow
                     exit Scan

                  end if
               end if

*           ... sign ...
*           ------------
            else if ( sign ( iChar1 ) ) then

*              ... in the right place ...
*              --------------------------
               if ( NNumbers .gt. 0 ) then
                  exit Scan                      ! The sign must be the
                                                 !   first non-blank
                                                 !   character
               else if ( Sign_Found ) then       ! No more than one sign
                  exit Scan                      !   is allowed

               else
                  if ( iChar1 .eq. iNegSign ) SignFactor = -1
c                  if ( Char1 .eq. '-' ) SignFactor = -1
                  Sign_Found = .true.

               end if

*           ... decimal point
*           -----------------
c            else if ( Char1 .eq. '.' ) then
            else if ( iChar1 .eq. iDecPt ) then
               exit Scan                         ! Decimal points and ...

*           ... character signifying beginning of exponent
*           ----------------------------------------------
            else if ( exponent ( iChar1 ) ) then ! ... exponent characters
               exit Scan                         !   are illegal

            end if
         end if

         stat = No_Error                         ! No conversion error
      end do Scan                                !   during this cycle

*     Return the result
*     -----------------
      if ( stat .eq. No_Error ) AtoI_ACh1 = SignFactor * INum
      return

      end function AtoI_ACh1

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AtoF_Str() --- Convert ASCII (a multicharacter string) to a float (real)
! 
! !DESCRIPTION: 
!
!     This function converts a token (stored as a multicharacter string) to
!     a float (i.e. real) according to the FORTRAN format editing
!     descriptors \verb|'(BN,FL.0)'| where \verb|L| is the input string
!     length.  In other words, blanks (and tabs) are ignored and any
!     missing decimal points is assumed to be at the end of the nonexponent
!     part.  The number of digits used to calculate the returned number is
!     limited by the precision of the host machine, or is set to 17,
!     whichever is less.  If an error occurs, then the returned float is
!     set to 0.
!
! !INTERFACE: 
!
      real function AtoF_Str ( token, stat )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      character ( len = * ), intent (in)  ::
     .            token      ! string containing the number  
!
! !OUTPUT PARAMETERS:
!
      integer,               intent (out) ::
     .            stat       ! Returned status (error) code.
!
! !REVISION HISTORY: 
!
!     31Jan2001  C Redder   Original code
!     22Feb2007  C Redder   Added a comment in the prologue.
!
! EOP
!-------------------------------------------------------------------------

      integer ( kind = Selected_Int_Kind ( NFltDigits_Max ) ) :: INum
      integer :: NChar, iChar, iChar1
      character ( len = 1 ) ::  Char1
      real    :: RNum
      integer :: NNumbers
      integer :: SignFactor,       NDigits,         NSigDigits
      integer :: iLeadDigit,       iDPoint
      integer :: ExpSignFactor,    ExpNum, ExpDNum, NExpDigits
      logical :: round_up,         nines_only
      logical :: nonexponent_part, LeadDigit_Found, DPoint_Found
      logical :: exponent_part,    LeadExp_Found
      logical :: Sign_Found,       ESign_Found

*     Set defaults
*     ------------
      stat     = 0
      AtoF_Str = 0.0

*     Initialize variables for the number of ...
*     ------------------------------------------
      NNumbers         =  0      ! ... characters, 0 - 9
      NDigits          =  0      ! ... all digits beyond the leading zeroes
      NSigDigits       =  0      ! ... all digits used in the conversion calc.
      NExpDigits       =  0      ! ... all exponent digits used in the calc.

*     ... positions within the input string of the ...
*     ------------------------------------------------
      iLeadDigit       =  0      ! ... lead digit for the nonexponent part
      iDPoint          =  0      ! ... decimal point

*     ... numbers for the ...
*     -----------------------
      INum             =  0      ! ... nonexponent part
      SignFactor       =  1      ! ... and its sign
      ExpNum           =  0      ! ... exponent ...
      ExpSignFactor    =  1      ! ... and its sign

*     ... logical flags indicating whether ...
*     ----------------------------------------
      round_up         = .false. ! ... to adjust to account for round-off
      nines_only       = .true.  ! ... all significant digits are 9's
      nonexponent_part = .true.  ! ... to assume the nonexponent part 
      LeadDigit_Found  = .false. ! ... the lead digit and ...
      DPoint_Found     = .false. ! ... the decimal point has been found
      exponent_part    = .false. ! ... to assume the exponent part 
      LeadExp_Found    = .false. ! ... the lead digit and the ... 
      Sign_Found       = .false. ! ... the sign of the nonexponent
      ESign_Found      = .false. ! ...   or nonexponent has been found

*     Scan the token
*     --------------
      NChar = Len_Trim ( token )
      Scan : do iChar = 1, NChar
         stat   = FConvErr       ! Initially assume conversion error
                                 !   for this cycle
         Char1  = token  ( iChar : iChar )
         iChar1 = iachar (  Char1 )

*        Check for illegal characters
*        ----------------------------
         if ( iChar1 .lt. iCharMin .or.
     .        iChar1 .gt. iCharMax   ) exit Scan
         if ( illegal     ( iChar1 ) ) exit Scan

*        If non-blank/tab, then check for ...
*        ------------------------------------
         if ( significant ( iChar1 ) ) then

*           ... numbers that are ...
*           -------------------------
            if ( number ( iChar1 ) ) then
               NNumbers = NNumbers + 1

*              ... significant digits prior to ...
*              -----------------------------------
               if    ( nonexponent_part ) then
                  if ( iChar1 .ne. iZero ) then
                     LeadDigit_Found = .true.
                     if ( iLeadDigit .eq. 0 )
     .                  iLeadDigit   =  NNumbers

                  end if

                  if ( LeadDigit_Found ) then
                     NDigits       = NDigits + 1
                     if    ( NDigits    .le. NFltDigits_Max  ) then
                        NSigDigits = NSigDigits + 1
                        if ( NSigDigits .le. NFltDigits_Max  ) then
                           INum    = 10 * INum  + iChar1 - iZero
                           if ( iChar1 .ne. iNine )
     .                          nines_only = .false.
c                           if ( Char1 .ne. '9' ) nines_only = .false.
                        end if

*                    Account for round-off
*                    ---------------------
                     else if ( NDigits .eq. NFltDigits_Max + 1 ) then
                        if ( iChar1 - iZero .ge. 5 )
     .                     round_up = .true.
                     end if
                  end if

*              ... or in the exponent
*              -----------------------
               else
                  if ( iChar1 .ne. iZero ) LeadExp_Found = .true.

                  if ( LeadExp_Found  ) then
                     NExpDigits      =  NExpDigits + 1
                     if ( NExpDigits    .le. NFExpDigits_Max )
     .                  ExpNum       =   10 * ExpNum + iChar1 - iZero
                  end if

               end if

*           ... sign of ...
*           ---------------
            else if ( sign ( iChar1 ) ) then

*              ... the exponent
*              ----------------
               if ( NNumbers .gt. 0 .or. Sign_Found ) then 
                  if ( ESign_Found        ) exit Scan ! Only one is allowed
                  if ( LeadExp_Found      ) exit Scan ! Must not be in
                  ESign_Found      = .true.           !   exponent
                  if ( iChar1 .eq. iNegSign ) ExpSignFactor = -1
c                  if ( Char1 .eq. '-' ) ExpSignFactor = -1
                  exponent_part    = .true.           ! Sign indicates begin-
                  nonexponent_part = .false.          !   ning of exponent
                  if ( NSigDigits .eq. 0 ) INum  = 1  ! Set to 1 if the non-
                                                      !   exponent part is
                                                      !   missing but the 
*              ... nonexponent part                   !   exponent part is not
*              --------------------
               else
                  if ( iChar1 .eq. iNegSign ) SignFactor = -1
c                  if ( Char1 .eq. '-' ) SignFactor = -1
                  Sign_Found = .true.

               end if

*           ... decimal point
*           -----------------
c            else if ( Char1 .eq. '.' ) then
            else if ( iChar1 .eq. iDecPt ) then
               if ( exponent_part         ) exit Scan ! Must not be in exp
               if ( DPoint_Found          ) exit Scan ! Only one is allowed
               DPoint_Found        = .true.
               iDPoint             =  NNumbers

*           ... character signifying beginning of exponent
*           ----------------------------------------------
            else if ( exponent ( iChar1 ) ) then
               if ( exponent_part         ) exit Scan ! Only one is allowed
               exponent_part       = .true.
               nonexponent_part    = .false.
               if ( NSigDigits .eq. 0 ) INum  = 1    ! Set to 1 if the non-
                                                     !   exponent part is
            end if                                   !   missing but the 
         end if                                      !   exponent part is not

         stat = No_Error                             ! No conversion error
      end do Scan                                    !   during this cycle

*     Cannot continue if error 
*     ------------------------
      if ( stat .ne. No_Error ) return

*     Set default position of a nonexistent decimal point
*     ---------------------------------------------------
      if ( .not. DPoint_Found )
     .   iDPoint = iLeadDigit + NDigits - 1

*     Determine the exponent
*     ----------------------
      ExpNum  = ExpSignFactor * ExpNum + iDPoint - iLeadDigit

*     Make any ajustments to account for round-off
*     --------------------------------------------
      if ( round_up ) then
         if ( nines_only ) then
            INum       = INum / 10 + 1
c            INum       = 10 ** ( NSigDigits - 1 )
            ExpNum     = ExpNum + 1

         else
            INum = INum + 1

         end if
      end if

*     If non-exponent part is zero ...
*     --------------------------------
      if      ( INum .eq. 0 ) then           ! ... the result is trivial
         AtoF_Str = 0.0                      ! -------------------------

*     Else, if exponent is too large ...
*     ----------------------------------
      else if ( ExpNum .ge. MaxFExp ) then
         ExpNum   = min ( ExpNum - MaxFExp, MaxFExp )
         RNum     = INum * DExp10 ( 1 - NSigDigits ) * DExp10 ( ExpNum )
         if ( RNum .gt. ManFHuge ) then      ! ... then check for overflow
            stat  = FOverflow                ! ---------------------------

         else
            AtoF_Str = SignFactor * RNum * DExp10 ( MaxFExp )

         end if

*     ... or too small ...
*     -----------------
      else if ( ExpNum .lt. MinFExp ) then
         ExpNum   = max ( ExpNum - MinFExp, MinFExp )
         RNum     = INum * DExp10 ( 1 - NSigDigits ) * DExp10 ( ExpNum )
         if ( RNum .lt. ManFTiny ) then      ! ... then check for underflow
            stat  = FUnderflow               ! ----------------------------

         else
            AtoF_Str = SignFactor * RNum * DExp10 ( MinFExp )

         end if

*     Otherwise, no checks are needed
*     -------------------------------
      else
         ExpDNum = 1 - NSigDigits + ExpNum
         if ( abs ( ExpDNum ) .le. MaxDExp ) then
            AtoF_Str = ( SignFactor * INum ) * DExp10 ( ExpDNum )
         else
            AtoF_Str =   real ( ( SignFactor * INum )
     .               *            DExp10 ( 1 - NSigDigits ) )
     .               *   FExp10 ( ExpNum )
         end if
c         AtoF_Str = ( SignFactor * INum )
c     .            *   FExp10 ( 1 - NSigDigits ) * FExp10 ( ExpNum )

      end if

      return
      end function AtoF_Str

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AtoF_ACh1() --- Convert ASCII (array of 1-character strings) to a float (real)
! 
! !DESCRIPTION: 
!
!     This function converts a token (stored as an array of one-character
!     strings) to a float (i.e. real) according to the FORTRAN format
!     editing descriptors \verb|'(BN,FL.0)'| where \verb|L| is the input
!     string length.  In other words, blanks (and tabs) are ignored and any
!     missing decimal points is assumed to be at the end of the nonexponent
!     part.  The number of digits used to calculate the returned number is
!     limited by the precision of the host machine, or is set to 17,
!     whichever is less.  If an error occurs, then the returned float is
!     set to 0.
!
! !INTERFACE: 
!
      real function AtoF_ACh1 ( token, stat )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      character ( len = 1 ), intent (in), dimension (:) ::
     .            token      ! string containing the number  
!
! !OUTPUT PARAMETERS:
!
      integer,               intent (out) ::
     .            stat       ! Returned status (error) code
!
! !REVISION HISTORY: 
!
!     31Jan2001  C Redder   Original code
!     22Feb2007  C Redder   Added a comment in the prologue.
!
! EOP
!-------------------------------------------------------------------------

      integer ( kind = Selected_Int_Kind ( NFltDigits_Max ) ) :: INum
      integer :: NChar, iChar, iChar1
      character ( len = 1 ) ::  Char1
      real    :: RNum
      integer :: NNumbers
      integer :: SignFactor,       NDigits,         NSigDigits
      integer :: iLeadDigit,       iDPoint
      integer :: ExpSignFactor,    ExpNum, ExpDNum, NExpDigits
      logical :: round_up,         nines_only
      logical :: nonexponent_part, LeadDigit_Found, DPoint_Found
      logical :: exponent_part,    LeadExp_Found
      logical :: Sign_Found,       ESign_Found

*     Set defaults
*     ------------
      stat      = 0
      AtoF_ACh1 = 0.0

*     Initialize variables for the number of ...
*     ------------------------------------------
      NNumbers         =  0      ! ... characters, 0 - 9
      NDigits          =  0      ! ... all digits beyond the leading zeroes
      NSigDigits       =  0      ! ... all digits used in the conversion calc.
      NExpDigits       =  0      ! ... all exponent digits used in the calc.

*     ... positions within the input string of the ...
*     ------------------------------------------------
      iLeadDigit       =  0      ! ... lead digit for the nonexponent part
      iDPoint          =  0      ! ... decimal point

*     ... numbers for the ...
*     -----------------------
      INum             =  0      ! ... nonexponent part
      SignFactor       =  1      ! ... and its sign
      ExpNum           =  0      ! ... exponent ...
      ExpSignFactor    =  1      ! ... and its sign

*     ... logical flags indicating whether ...
*     ----------------------------------------
      round_up         = .false. ! ... to adjust to account for round-off
      nines_only       = .true.  ! ... all significant digits are 9's
      nonexponent_part = .true.  ! ... to assume the nonexponent part 
      LeadDigit_Found  = .false. ! ... the lead digit and ...
      DPoint_Found     = .false. ! ... the decimal point has been found
      exponent_part    = .false. ! ... to assume the exponent part 
      LeadExp_Found    = .false. ! ... the lead digit and the ... 
      Sign_Found       = .false. ! ... the sign of the nonexponent
      ESign_Found      = .false. ! ...   or nonexponent has been found

*     Scan the token
*     --------------
      NChar = Ch1ArrLen_Trim ( token )
      Scan : do iChar = 1, NChar
         stat   = FConvErr       ! Initially assume conversion error
                                 !   for this cycle
         Char1  = token  ( iChar  )
         iChar1 = iachar (  Char1 )

*        Check for illegal characters
*        ----------------------------
         if ( iChar1 .lt. iCharMin .or.
     .        iChar1 .gt. iCharMax   ) exit Scan
         if ( illegal     ( iChar1 ) ) exit Scan

*        If non-blank/tab, then check for ...
*        ------------------------------------
         if ( significant ( iChar1 ) ) then

*           ... numbers that are ...
*           -------------------------
            if ( number ( iChar1 ) ) then
               NNumbers = NNumbers + 1

*              ... significant digits prior to ...
*              -----------------------------------
               if    ( nonexponent_part ) then
                  if ( iChar1 .ne. iZero ) then
                     LeadDigit_Found = .true.
                     if ( iLeadDigit .eq. 0 )
     .                  iLeadDigit   =  NNumbers

                  end if

                  if ( LeadDigit_Found ) then
                     NDigits       = NDigits + 1
                     if    ( NDigits    .le. NFltDigits_Max  ) then
                        NSigDigits = NSigDigits + 1
                        if ( NSigDigits .le. NFltDigits_Max  ) then
                           INum    = 10 * INum  + iChar1 - iZero
                           if ( iChar1 .ne. iNine )
     .                        nines_only = .false.
c                           if ( Char1 .ne. '9' ) nines_only = .false.
                        end if

*                    Account for round-off
*                    ---------------------
                     else if ( NDigits .eq. NFltDigits_Max + 1 ) then
                        if ( iChar1 - iZero .ge. 5 )
     .                     round_up = .true.
                     end if
                  end if

*              ... or in the exponent
*              -----------------------
               else
                  if ( iChar1 .ne. iZero ) LeadExp_Found = .true.

                  if ( LeadExp_Found  ) then
                     NExpDigits      =  NExpDigits + 1
                     if ( NExpDigits    .le. NFExpDigits_Max )
     .                  ExpNum       =   10 * ExpNum + iChar1 - iZero
                  end if

               end if

*           ... sign of ...
*           ---------------
            else if ( sign ( iChar1 ) ) then

*              ... the exponent
*              ----------------
               if ( NNumbers .gt. 0 .or. Sign_Found ) then 
                  if ( ESign_Found        ) exit Scan ! Only one is allowed
                  if ( LeadExp_Found      ) exit Scan ! Must not be in
                  ESign_Found      = .true.           !   exponent
c                  if ( Char1 .eq. '-' ) ExpSignFactor = -1
                  if ( iChar1 .eq. iNegSign ) ExpSignFactor = -1
                  exponent_part    = .true.           ! Sign indicates begin-
                  nonexponent_part = .false.          !   ning of exponent
                  if ( NSigDigits .eq. 0 ) INum  = 1  ! Set to 1 if the non-
                                                      !   exponent part is
                                                      !   missing but the 
*              ... nonexponent part                   !   exponent part is not
*              --------------------
               else
                  if ( iChar1 .eq. iNegSign ) ExpSignFactor = -1
c                  if ( Char1 .eq. '-' ) SignFactor = -1
                  Sign_Found = .true.

               end if

*           ... decimal point
*           -----------------
            else if ( iChar1 .eq. iDecPt ) then
c            else if ( Char1 .eq. '.' ) then
               if ( exponent_part         ) exit Scan ! Must not be in exp
               if ( DPoint_Found          ) exit Scan ! Only one is allowed
               DPoint_Found        = .true.
               iDPoint             =  NNumbers

*           ... character signifying beginning of exponent
*           ----------------------------------------------
            else if ( exponent ( iChar1 ) ) then
               if ( exponent_part         ) exit Scan ! Only one is allowed
               exponent_part       = .true.
               nonexponent_part    = .false.
               if ( NSigDigits .eq. 0 ) INum  = 1    ! Set to 1 if the non-
                                                     !   exponent part is
            end if                                   !   missing but the 
         end if                                      !   exponent part is not

         stat = No_Error                             ! No conversion error
      end do Scan                                    !   during this cycle

*     Cannot continue if error 
*     ------------------------
      if ( stat .ne. No_Error ) return

*     Set default position of a nonexistent decimal point
*     ---------------------------------------------------
      if ( .not. DPoint_Found )
     .   iDPoint = iLeadDigit + NDigits - 1

*     Determine the exponent
*     ----------------------
      ExpNum  = ExpSignFactor * ExpNum + iDPoint - iLeadDigit

*     Make any ajustments to account for round-off
*     --------------------------------------------
      if ( round_up ) then
         if ( nines_only ) then
            INum       = INum / 10 + 1
            ExpNum     = ExpNum + 1

         else
            INum = INum + 1

         end if
      end if


*     If non-exponent part is zero ...
*     --------------------------------
      if      ( INum .eq. 0 ) then           ! ... the result is trivial
         AtoF_ACh1 = 0.0                     ! -------------------------

*     Else, if exponent is too large ...
*     ----------------------------------
      else if ( ExpNum .ge. MaxFExp ) then
         ExpNum   = min ( ExpNum - MaxFExp, MaxFExp )
         RNum     = INum * DExp10 ( 1 - NSigDigits ) * DExp10 ( ExpNum )
         if ( RNum .gt. ManFHuge ) then      ! ... then check for overflow
            stat  = FOverflow                ! ---------------------------

         else
            AtoF_ACh1 = SignFactor * RNum * DExp10 ( MaxFExp )

         end if

*     ... or too small ...
*     -----------------
      else if ( ExpNum .lt. MinFExp ) then
         ExpNum   = max ( ExpNum - MinFExp, MinFExp )
         RNum     = INum * DExp10 ( 1 - NSigDigits ) * DExp10 ( ExpNum )
         if ( RNum .lt. ManFTiny ) then      ! ... then check for underflow
            stat  = FUnderflow               ! ----------------------------

         else
            AtoF_ACh1 = SignFactor * RNum * DExp10 ( MinFExp )

         end if

*     Otherwise, no checks are needed
*     -------------------------------
      else
         ExpDNum = 1 - NSigDigits + ExpNum
         if ( abs ( ExpDNum ) .le. MaxDExp ) then
            AtoF_ACh1 = ( SignFactor * INum ) * DExp10 ( ExpDNum )
         else
            AtoF_ACh1 =   real ( ( SignFactor * INum )
     .                *            DExp10 ( 1 - NSigDigits ) )
     .                *   FExp10 ( ExpNum )
         end if
c         AtoF_ACh1 = ( SignFactor * INum ) 
c     .             *   FExp10 ( 1 - NSigDigits ) * FExp10 ( ExpNum )

      end if

      return
      end function AtoF_ACh1

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AtoD_Str() --- Convert ASCII (a multicharacter string) to double precision
! 
! !DESCRIPTION: 
!
!     This function converts a token (stored as a multicharacter string) to
!     a double precision according to the FORTRAN format editing
!     descriptors \verb|'(BN,DL.0)'| where \verb|L| is the input string
!     length.  In other words, blanks (and tabs) are ignored and any
!     missing decimal points is assumed to be at the end of the nonexponent
!     part.  The number of digits used to calculate the returned number is
!     limited by the precision of the host machine, or is set to 34,
!     whichever is less.  If an error occurs, then the returned double
!     precision float is set to 0.
!
! !INTERFACE: 
!
      double precision function AtoD_Str ( token, stat )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      character ( len = * ), intent (in)  ::
     .            token      ! string containing the number  
!
! !OUTPUT PARAMETERS:
!
      integer,               intent (out) ::
     .            stat       ! Returned status (error) code.
!
! !REVISION HISTORY: 
!
!     01Jan2001  C Redder   Original code
!     22Feb2007  C Redder   Added a comment in the prologue.
!
! EOP
!-------------------------------------------------------------------------

      integer ( kind = Selected_Int_Kind ( NDblDigits_Max ) ) :: INum
      integer :: NChar, iChar, iChar1
      character ( len = 1 ) ::  Char1
      double precision :: DNum
      integer :: NNumbers
      integer :: SignFactor,       NDigits,         NSigDigits
      integer :: iLeadDigit,       iDPoint
      integer :: ExpSignFactor,    ExpNum,          NExpDigits
      logical :: round_up,         nines_only
      logical :: nonexponent_part, LeadDigit_Found, DPoint_Found
      logical :: exponent_part,    LeadExp_Found
      logical :: Sign_Found,       ESign_Found

*     Set defaults
*     ------------
      stat     = 0
      AtoD_Str = 0.0d0

*     Initialize variables for the number of ...
*     ------------------------------------------
      NNumbers         =  0      ! ... characters, 0 - 9
      NDigits          =  0      ! ... all digits beyond the leading zeroes
      NSigDigits       =  0      ! ... all digits used in the conversion calc.
      NExpDigits       =  0      ! ... all exponent digits used in the calc.

*     ... positions within the input string of the ...
*     ------------------------------------------------
      iLeadDigit       =  0      ! ... lead digit for the nonexponent part
      iDPoint          =  0      ! ... decimal point

*     ... numbers for the ...
*     -----------------------
      INum             =  0      ! ... nonexponent part
      SignFactor       =  1      ! ... and its sign
      ExpNum           =  0      ! ... exponent ...
      ExpSignFactor    =  1      ! ... and its sign

*     ... logical flags indicating whether ...
*     ----------------------------------------
      round_up         = .false. ! ... to adjust to account for round-off
      nines_only       = .true.  ! ... all significant digits are 9's
      nonexponent_part = .true.  ! ... to assume the nonexponent part 
      LeadDigit_Found  = .false. ! ... the lead digit and ...
      DPoint_Found     = .false. ! ... the decimal point has been found
      exponent_part    = .false. ! ... to assume the exponent part 
      LeadExp_Found    = .false. ! ... the lead digit and the ... 
      Sign_Found       = .false. ! ... the sign of the nonexponent
      ESign_Found      = .false. ! ...   or nonexponent has been found

*     Scan the token
*     --------------
      NChar = Len_Trim ( token )
      Scan : do iChar = 1, NChar
         stat   = DConvErr       ! Initially assume conversion error
                                 !   for this cycle
         Char1  = token  ( iChar : iChar )
         iChar1 = iachar (  Char1 )

*        Check for illegal characters
*        ----------------------------
         if ( iChar1 .lt. iCharMin .or.
     .        iChar1 .gt. iCharMax   ) exit Scan
         if ( illegal     ( iChar1 ) ) exit Scan

*        If non-blank/tab, then check for ...
*        ------------------------------------
         if ( significant ( iChar1 ) ) then

*           ... numbers that are ...
*           -------------------------
            if ( number ( iChar1 ) ) then
               NNumbers = NNumbers + 1

*              ... significant digits prior to ...
*              -----------------------------------
               if    ( nonexponent_part ) then
                  if ( iChar1 .ne. iZero ) then
                     LeadDigit_Found = .true.
                     if ( iLeadDigit .eq. 0 )
     .                  iLeadDigit   =  NNumbers

                  end if

                  if ( LeadDigit_Found ) then
                     NDigits       = NDigits + 1
                     if    ( NDigits    .le. NDblDigits_Max  ) then
                        NSigDigits = NSigDigits + 1
                        if ( NSigDigits .le. NDblDigits_Max  ) then
                           INum    = 10 * INum  + iChar1 - iZero
                           if ( iChar1 .ne. iNine )
     .                          nines_only = .false.
c                           if ( Char1 .ne. '9' ) nines_only = .false.
                        end if

*                    Account for round-off
*                    ---------------------
                     else if ( NDigits .eq. NDblDigits_Max + 1 ) then
                        if ( iChar1 - iZero .ge. 5 )
     .                     round_up = .true.
                     end if
                  end if

*              ... or in the exponent
*              -----------------------
               else
                  if ( iChar1 .ne. iZero ) LeadExp_Found = .true.

                  if ( LeadExp_Found  ) then
                     NExpDigits      =  NExpDigits + 1
                     if ( NExpDigits    .le. NDExpDigits_Max )
     .                  ExpNum       =   10 * ExpNum + iChar1 - iZero
                  end if

               end if

*           ... sign of ...
*           ---------------
            else if ( sign ( iChar1 ) ) then

*              ... the exponent
*              ----------------
               if ( NNumbers .gt. 0 .or. Sign_Found ) then 
                  if ( ESign_Found        ) exit Scan ! Only one is allowed
                  if ( LeadExp_Found      ) exit Scan ! Must not be in
                  ESign_Found      = .true.           !   exponent
                  if ( iChar1 .eq. iNegSign ) ExpSignFactor = -1
c                  if ( Char1 .eq. '-' ) ExpSignFactor = -1
                  exponent_part    = .true.           ! Sign indicates begin-
                  nonexponent_part = .false.          !   ning of exponent
                  if ( NSigDigits .eq. 0 ) INum  = 1  ! Set to 1 if the non-
                                                      !   exponent part is
                                                      !   missing but the 
*              ... nonexponent part                   !   exponent part is not
*              --------------------
               else
                  if ( iChar1 .eq. iNegSign ) SignFactor = -1
c                  if ( Char1 .eq. '-' ) SignFactor = -1
                  Sign_Found = .true.

               end if

*           ... decimal point
*           -----------------
c            else if ( Char1 .eq. '.' ) then
            else if ( iChar1 .eq. iDecPt ) then
               if ( exponent_part         ) exit Scan ! Must not be in exp
               if ( DPoint_Found          ) exit Scan ! Only one is allowed
               DPoint_Found        = .true.
               iDPoint             =  NNumbers

*           ... character signifying beginning of exponent
*           ----------------------------------------------
            else if ( exponent ( iChar1 ) ) then
               if ( exponent_part         ) exit Scan ! Only one is allowed
               exponent_part       = .true.
               nonexponent_part    = .false.
               if ( NSigDigits .eq. 0 ) INum  = 1    ! Set to 1 if the non-
                                                     !   exponent part is
            end if                                   !   missing but the 
         end if                                      !   exponent part is not

         stat = No_Error                             ! No conversion error
      end do Scan                                    !   during this cycle

*     Cannot continue if error 
*     ------------------------
      if ( stat .ne. No_Error ) return

*     Set default position of a nonexistent decimal point
*     ---------------------------------------------------
      if ( .not. DPoint_Found )
     .   iDPoint = iLeadDigit + NDigits - 1

*     Determine the exponent
*     ----------------------
      ExpNum  = ExpSignFactor * ExpNum + iDPoint - iLeadDigit

*     Make any ajustments to account for round-off
*     --------------------------------------------
      if ( round_up ) then
         if ( nines_only ) then
            INum       = INum / 10 + 1
c            INum       = 10 ** ( NSigDigits - 1 )
            ExpNum     = ExpNum + 1

         else
            INum = INum + 1

         end if
      end if

*     If non-exponent part is zero ...
*     --------------------------------
      if      ( INum .eq. 0 ) then           ! ... the result is trivial
         AtoD_Str = 0.0d0                    ! -------------------------

*     Else, if exponent is too large ...
*     ----------------------------------
      else if ( ExpNum .ge. MaxDExp ) then
         ExpNum   = min ( ExpNum - MaxDExp, MaxDExp )
         DNum     = INum * DExp10 ( 1 - NSigDigits ) * DExp10 ( ExpNum )
         if ( DNum .gt. ManDHuge ) then      ! ... then check for overflow
            stat  = DOverflow                ! ---------------------------

         else
            AtoD_Str = SignFactor * DNum * DExp10 ( MaxDExp )

         end if

*     ... or too small ...
*     --------------------
      else if ( ExpNum .lt. MinDExp ) then
         ExpNum   = max ( ExpNum - MinDExp, MinDExp )
         DNum     = INum * DExp10 ( 1 - NSigDigits ) * DExp10 ( ExpNum )
         if ( DNum .lt. ManDTiny ) then      ! ... then check for underflow
            stat  = DUnderflow               ! ----------------------------

         else
            AtoD_Str = SignFactor * DNum * DExp10 ( MinDExp )

         end if

*     Otherwise, no checks are needed
*     -------------------------------
      else
         AtoD_Str = ( SignFactor * INum )
     .            *   DExp10 ( 1 - NSigDigits ) * DExp10 ( ExpNum )

      end if

      return
      end function AtoD_Str

*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AtoD_ACh1() --- Convert ASCII (array of 1-character strings) to double precision
! 
! !DESCRIPTION: 
!
!     This function converts a token (stored as an array of one-character
!     strings) to double precision according to the FORTRAN format
!     editing descriptors \verb|'(BN,DL.0)'| where \verb|L| is the input
!     string length.  In other words, blanks (and tabs) are ignored and any
!     missing decimal points is assumed to be at the end of the nonexponent
!     part.  The number of digits used to calculate the returned number is
!     limited by the precision of the host machine, or is set to 34,
!     whichever is less.  If an error occurs, then the returned double
!     precision float is set to 0.
!
! !INTERFACE: 
!
      double precision function AtoD_ACh1 ( token, stat )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      character ( len = 1 ), intent (in), dimension (:) ::
     .            token      ! string containing the number  
!
! !OUTPUT PARAMETERS:
!
      integer,               intent (out) ::
     .            stat       ! Returned status (error) code
!
! !REVISION HISTORY: 
!
!     01Feb2001  C Redder   Original code
!     22Feb2007  C Redder   Added a comment in the prologue.
!
! EOP
!-------------------------------------------------------------------------

      integer ( kind = Selected_Int_Kind ( NDblDigits_Max ) ) :: INum
      integer :: NChar, iChar, iChar1
      character ( len = 1 ) ::  Char1
      double precision :: DNum
      integer :: NNumbers
      integer :: SignFactor,       NDigits,         NSigDigits
      integer :: iLeadDigit,       iDPoint
      integer :: ExpSignFactor,    ExpNum,          NExpDigits
      logical :: round_up,         nines_only
      logical :: nonexponent_part, LeadDigit_Found, DPoint_Found
      logical :: exponent_part,    LeadExp_Found
      logical :: Sign_Found,       ESign_Found

*     Set defaults
*     ------------
      stat      = 0
      AtoD_ACh1 = 0.0d0

*     Initialize variables for the number of ...
*     ------------------------------------------
      NNumbers         =  0      ! ... characters, 0 - 9
      NDigits          =  0      ! ... all digits beyond the leading zeroes
      NSigDigits       =  0      ! ... all digits used in the conversion calc.
      NExpDigits       =  0      ! ... all exponent digits used in the calc.

*     ... positions within the input string of the ...
*     ------------------------------------------------
      iLeadDigit       =  0      ! ... lead digit for the nonexponent part
      iDPoint          =  0      ! ... decimal point

*     ... numbers for the ...
*     -----------------------
      INum             =  0      ! ... nonexponent part
      SignFactor       =  1      ! ... and its sign
      ExpNum           =  0      ! ... exponent ...
      ExpSignFactor    =  1      ! ... and its sign

*     ... logical flags indicating whether ...
*     ----------------------------------------
      round_up         = .false. ! ... to adjust to account for round-off
      nines_only       = .true.  ! ... all significant digits are 9's
      nonexponent_part = .true.  ! ... to assume the nonexponent part 
      LeadDigit_Found  = .false. ! ... the lead digit and ...
      DPoint_Found     = .false. ! ... the decimal point has been found
      exponent_part    = .false. ! ... to assume the exponent part 
      LeadExp_Found    = .false. ! ... the lead digit and the ... 
      Sign_Found       = .false. ! ... the sign of the nonexponent
      ESign_Found      = .false. ! ...   or nonexponent has been found

*     Scan the token
*     --------------
      NChar = Ch1ArrLen_Trim ( token )
      Scan : do iChar = 1, NChar
         stat   = DConvErr       ! Initially assume conversion error
                                 !   for this cycle
         Char1  = token  ( iChar  )
         iChar1 = iachar (  Char1 )

*        Check for illegal characters
*        ----------------------------
         if ( iChar1 .lt. iCharMin .or.
     .        iChar1 .gt. iCharMax   ) exit Scan
         if ( illegal     ( iChar1 ) ) exit Scan

*        If non-blank/tab, then check for ...
*        ------------------------------------
         if ( significant ( iChar1 ) ) then

*           ... numbers that are ...
*           -------------------------
            if ( number ( iChar1 ) ) then
               NNumbers = NNumbers + 1

*              ... significant digits prior to ...
*              -----------------------------------
               if    ( nonexponent_part ) then
                  if ( iChar1 .ne. iZero ) then
                     LeadDigit_Found = .true.
                     if ( iLeadDigit .eq. 0 )
     .                  iLeadDigit   =  NNumbers

                  end if

                  if ( LeadDigit_Found ) then
                     NDigits       = NDigits + 1
                     if    ( NDigits    .le. NDblDigits_Max  ) then
                        NSigDigits = NSigDigits + 1
                        if ( NSigDigits .le. NDblDigits_Max  ) then
                           INum    = 10 * INum  + iChar1 - iZero
                           if ( iChar1 .ne. iNine )
     .                          nines_only = .false.
c                           if ( Char1 .ne. '9' ) nines_only = .false.
                        end if

*                    Account for round-off
*                    ---------------------
                     else if ( NDigits .eq. NDblDigits_Max + 1 ) then
                        if ( iChar1 - iZero .ge. 5 )
     .                     round_up = .true.
                     end if
                  end if

*              ... or in the exponent
*              -----------------------
               else
                  if ( iChar1 .ne. iZero ) LeadExp_Found = .true.

                  if ( LeadExp_Found  ) then
                     NExpDigits      =  NExpDigits + 1
                     if ( NExpDigits    .le. NDExpDigits_Max )
     .                  ExpNum       =   10 * ExpNum + iChar1 - iZero
                  end if

               end if

*           ... sign of ...
*           ---------------
            else if ( sign ( iChar1 ) ) then

*              ... the exponent
*              ----------------
               if ( NNumbers .gt. 0 .or. Sign_Found ) then 
                  if ( ESign_Found        ) exit Scan ! Only one is allowed
                  if ( LeadExp_Found      ) exit Scan ! Must not be in
                  ESign_Found      = .true.           !   exponent
                  if ( iChar1 .eq. iNegSign ) ExpSignFactor = -1
c                  if ( Char1 .eq. '-' ) ExpSignFactor = -1
                  exponent_part    = .true.           ! Sign indicates begin-
                  nonexponent_part = .false.          !   ning of exponent
                  if ( NSigDigits .eq. 0 ) INum  = 1  ! Set to 1 if the non-
                                                      !   exponent part is
                                                      !   missing but the 
*              ... nonexponent part                   !   exponent part is not
*              --------------------
               else
                  if ( iChar1 .eq. iNegSign ) SignFactor = -1
c                  if ( Char1 .eq. '-' ) SignFactor = -1
                  Sign_Found = .true.

               end if

*           ... decimal point
*           -----------------
c            else if ( Char1 .eq. '.' ) then
            else if ( iChar1 .eq. iDecPt ) then
               if ( exponent_part         ) exit Scan ! Must not be in exp
               if ( DPoint_Found          ) exit Scan ! Only one is allowed
               DPoint_Found        = .true.
               iDPoint             =  NNumbers

*           ... character signifying beginning of exponent
*           ----------------------------------------------
            else if ( exponent ( iChar1 ) ) then
               if ( exponent_part         ) exit Scan ! Only one is allowed
               exponent_part       = .true.
               nonexponent_part    = .false.
               if ( NSigDigits .eq. 0 ) INum  = 1    ! Set to 1 if the non-
                                                     !   exponent part is
            end if                                   !   missing but the 
         end if                                      !   exponent part is not

         stat = No_Error                             ! No conversion error
      end do Scan                                    !   during this cycle

*     Cannot continue if error 
*     ------------------------
      if ( stat .ne. No_Error ) return

*     Set default position of a nonexistent decimal point
*     ---------------------------------------------------
      if ( .not. DPoint_Found )
     .   iDPoint = iLeadDigit + NDigits - 1

*     Determine the exponent
*     ----------------------
      ExpNum  = ExpSignFactor * ExpNum + iDPoint - iLeadDigit

*     Make any ajustments to account for round-off
*     --------------------------------------------
      if ( round_up ) then
         if ( nines_only ) then
            INum       = INum / 10 + 1
c            INum       = 10 ** ( NSigDigits - 1 )
            ExpNum     = ExpNum + 1

         else
            INum = INum + 1

         end if
      end if

*     If non-exponent part is zero ...
*     --------------------------------
      if      ( INum .eq. 0 ) then           ! ... the result is trivial
         AtoD_ACh1 = 0.0d0                   ! -------------------------

*     Else, if exponent is too large ...
*     ----------------------------------
      else if ( ExpNum .ge. MaxDExp ) then
         ExpNum   = min ( ExpNum - MaxDExp, MaxDExp )
         DNum     = INum * DExp10 ( 1 - NSigDigits ) * DExp10 ( ExpNum )
         if ( DNum .gt. ManDHuge ) then      ! ... then check for overflow
            stat  = DOverflow                ! ---------------------------

         else
            AtoD_ACh1 = SignFactor * DNum * DExp10 ( MaxDExp )

         end if

*     ... or too small ...
*     --------------------
      else if ( ExpNum .lt. MinDExp ) then
         ExpNum   = max ( ExpNum - MinDExp, MinDExp )
         DNum     = INum * DExp10 ( 1 - NSigDigits ) * DExp10 ( ExpNum )
         if ( DNum .lt. ManDTiny ) then      ! ... then check for underflow
            stat  = DUnderflow               ! ----------------------------

         else
            AtoD_ACh1 = SignFactor * DNum * DExp10 ( MinDExp )

         end if

*     Otherwise, no checks are needed
*     -------------------------------
      else
         AtoD_ACh1 = ( SignFactor * INum )
     .             *   DExp10 ( 1 - NSigDigits ) * DExp10 ( ExpNum )

      end if

      return
      end function AtoD_ACh1

*...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AtoXWh () --- Returns the routine name where the error occurred.
! 
! !DESCRIPTION:
!     Given the returned status code, this function returns the name of the
!     routine (including the name of this module) with no trailing blanks.
!     The function will return the routine name only for the status codes
!     listed in the module header.  For all other status codes, the function
!     returns a blank string.
!
! !INTERFACE:
!
      function AtoXWh ( stat )
      implicit NONE
!
! !INPUT PARAMETERS: 
!
      integer,  intent (in) ::
     .   stat ! Status coded returned from an AtoX routine
!
! !OUTPUT PARAMETERS: 
!
      character (len = len_trim ( MyNames (max(1, min(stat,
     .                                                stat_max))))) ::
     .   AtoXWh
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Jul2001  Redder Origional code.
!
! EOP
!-------------------------------------------------------------------------

      if ( stat .le. 0 .or. stat .gt. stat_max ) then
         AtoXWh = BLK
      else
         AtoXWh = MyNames ( stat )
      end if

      return
      end function AtoXWh

*...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AtoXWh2 () --- Returns the routine name where the error occurred for token stored as ACh1
! 
! !DESCRIPTION:
!     Given the returned status code, this function returns the name of the
!     routine (including the name of this module) with no trailing blanks.
!     The routine whose name is being returned is assumed to be for tokens
!     stored as one-character strings.  The function will return the routine
!     name only for the status codes listed in the module header.  For all
!     other status codes, the function returns a blank string.
!
! !INTERFACE:
!
      function AtoXWh2 ( stat )
      implicit NONE
!
! !INPUT PARAMETERS: 
!
      integer,  intent (in) ::
     .   stat ! Status coded returned from an AtoX routine
!
! !OUTPUT PARAMETERS: 
!
      character (len = len_trim ( MyNames2 (max(1, min(stat,
     .                                                 stat_max))))) ::
     .   AtoXWh2
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     20Jul2001  Redder Origional code.
!
! EOP
!-------------------------------------------------------------------------

      if ( stat .le. 0 .or. stat .gt. stat_max ) then
         AtoXWh2 = BLK
      else
         AtoXWh2 = MyNames2 ( stat )
      end if

      return
      end function AtoXWh2

*...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AtoXErr_ACh1 () --- Generate error message.
! 
! !DESCRIPTION:
!     Given the returned status code, this function generates one of the
!     error messages (with no trailing blanks) listed in the prologue to
!     routine, AtoXErr\_Str.  The function will generate a message only for
!     the status codes listed in the module header.  For all other status
!     codes, the function returns a blank string.
!
! !INTERFACE:
!
      function AtoXErr_ACh1 ( stat, token )
      implicit NONE
!
! !INPUT PARAMETERS: 
!
      integer,               intent (in) ::
     .   stat                ! Status coded returned from an AtoX routine
      character ( len = 1 ), intent (in), dimension (:) ::
     .   token               ! Token that produced the error.
!
! !OUTPUT PARAMETERS: 
!
      character (len = len_trim ( messages_part1 (max(1,min(stat,
     .                                                      stat_max))))
     .               + len      ( token ) + 2 ) ::
     .   AtoXErr_ACh1
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     28Mar2001  Redder    Origional code.
!     20Jul2001  Redder    Removed input argument, where, and made the
!                          subroutine a function.
!
! EOP
!-------------------------------------------------------------------------

      AtoXErr_ACh1 = AtoXErr_Str ( stat, Str ( Token ))
      return
      end function AtoXErr_ACh1

*...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AtoXErr_Str () --- Generate error message
! 
! !DESCRIPTION:
!     Given the returned status code, this function generates one of the
!     following error messages (with no trailing blanks):
!\begin{verbatim}
!     Conversion error; token = token
!\end{verbatim}
!
!\begin{verbatim}
!     Integer [or floating point or double precision] overflow; \
!        number = token
!\end{verbatim}
!              or
!\begin{verbatim}
!     Floating point [or double precision] underflow; number = token
!\end{verbatim}
!
!     where \verb|where| is the routine where the message occurred.  The
!     function will generate a message only for the status codes listed in
!     the module header.  For all other status codes, the function returns
!     a blank string.
!
! !INTERFACE:
!
      function AtoXErr_Str ( stat, token )
      implicit NONE
!
! !INPUT PARAMETERS: 
!
      integer,               intent (in) ::
     .   stat                ! Status coded returned from an AtoX routine
      character ( len = * ), intent (in) ::
     .   token               ! Token that produced the error.
!
! !OUTPUT PARAMETERS: 
!
      character (len = len_trim ( messages_part1 (max(1,min(stat,
     .                                                      stat_max))))
     .               + len      ( token ) + 2 ) ::
     .   AtoXErr_Str
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!
!     28Mar2001  Redder    Origional code.
!     20Jul2001  Redder    Removed input argument, where, and made the
!                          subroutine a function.
!
! EOP
!-------------------------------------------------------------------------

      if ( stat .le. 0 .or. stat .gt. stat_max ) then
         AtoXErr_Str = BLK
      else
         AtoXErr_Str =  trim ( messages_part1 ( stat ) )
     .               // BLK // token // '.'
      end if

      return
      end function AtoXErr_Str
*...................................................................
      
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Ch1ArrLen_Trim() --- Returns length of one character string array.
! 
! !DESCRIPTION: 
!     Returns the length of a string excluding trailing blanks.  The
!     input string is stored as an array of one character strings.  This
!     function is a low level routine.
!
! !INTERFACE: 
!
      integer function Ch1ArrLen_Trim ( Arr )
!
! !INPUT PARAMETERS: 
!
      implicit NONE
      character ( len = 1 ), intent (in), dimension (:) ::
     .                       Arr     ! Input array
!
! !REVISION HISTORY:
!     21Aug2001  Redder     Code adapted from the module, m_ACh1
!
! EOP
!-------------------------------------------------------------------------

      integer LArr, iChar, TempLen, IAChar1, IABlk

      Ch1ArrLen_Trim = 0
      LArr           = size ( Arr )
      if ( LArr .le. 0 ) return     ! Nothing to do if zero length

      TempLen = 0
      IABlk   = IAChar ( BLK )
      Scan : do iChar = LArr, 1, -1
         IAChar1 = IAChar ( Arr ( iChar ) )
         if ( IAChar1 .ne. IABlk ) then
            TempLen = iChar
            exit Scan

         end if 

      end do Scan
      Ch1ArrLen_Trim = TempLen

      return
      end function Ch1ArrLen_Trim
*....................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Str --- Copy one character string array to string
!
! !DESCRIPTION:
!
!     This routine copies a one character string array to multicharacter
!     string with a length of the array size.  This function is a low level
!     routine.
!
! !INTERFACE:
!
      function Str ( Arr )
      implicit NONE
!
! !INPUT PARAMETERS: 
!
      character ( len = 1 ), intent (in), dimension (:) ::
     .            Arr               ! Input array of one character strings
!
! !OUTPUT PARAMETERS: 
!
      character ( len = size ( Arr ) ) ::
     .            Str               ! Output multicharacter string
!
! !REVISION HISTORY: 
!     21Aug2001  Redder     Code adapted from the module, m_ACh1
!
! EOP
!-------------------------------------------------------------------------

      integer :: iChar

!     Copy array to string
!     --------------------
      do iChar = 1, size ( Arr )
         Str ( iChar : iChar ) = Arr ( iChar )

      end do

      return
      end function Str
*....................................................................
      end module m_AtoX
*====================================================================
