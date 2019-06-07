!====================================================================

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_AdvError -- Library of advanced error handling routines
!
! !DESCRIPTION:
!     This module is contains functions for generating message for some of 
!     the commonly occurring errors.  This module is also a manager for
!     some of the error handing routines such as PError.
!
! !INTERFACE:
!
      module    m_AdvError
      use       m_TextUtil, only : ItoA, ItoDate
      use       m_ArrList,  only : Arr,  Array, Arrays
      use       m_SysIO,    only : Die_ => Die
      implicit  NONE
      private ! except

!     Constants
!     ---------
      public :: No_Error       ! Status code signifying no error

!     Subroutines and functions
!     -------------------------
      public :: Die,  WDie     ! Format/print text to std error and exit 
      public :: PErr, WPErr    ! Format/print text to std error
      public :: ItoA           ! Return integer as a character string 
      public :: ItoDate        ! Return date in character string format
      public :: Arr, Array,    ! Return array name(s) with given dimensions
     .          Arrays
      public :: AllocErr,      ! Generate message for allocation errors
     .          Alloc
      public :: DeallocErr,    ! Generate message for deallocation errors
     .          Dealloc
      public :: ErrStat        ! Generate message for returned error status

      interface Die            ! routine parameters
         module procedure
     .      Die_,              ! ... (where)
     .      Die_Text           ! ... (where, text, [usage])
      end interface
      interface WDie           ! routine parameters
         module procedure
     .      Die_,              ! ... (where)
     .      WDie_Text          ! ... (where, text, [usage])
                               ! where = name of routine
      end interface
      interface ErrStat
         module procedure      ! function parameters
     .      ErrStat_Basic,     ! ... (source)
     .      ErrStat_iret       ! ... (source, stat)
                               ! source = name of given source routine
      end interface

      interface AllocErr
         module procedure      ! function parameters
     .      AllocErr_Basic,    ! ... (stat)
     .      AllocErr_List,     ! ... (stat, list)
     .      AllocErr_ArrSz,    ! ... (stat, names,  size)
     .      AllocErr_ArrSzs,   ! ... (stat, names,  sizes)
     .      AllocErr_ArrLU,    ! ... (stat, names,  lb,  ub)
     .      AllocErr_ArrLUs,   ! ... (stat, names,  lb,  ubs)
     .      AllocErr_ArrLsUs   ! ... (stat, names,  lbs, ubs)
                               ! lb(s) = lower bound(s)
      end interface            ! ub(s) = upper bound(s)
                               ! The input parameters, lbs and ubs
                               ! correspond to rank 1 arrays
      interface Alloc          ! Same interface as AllocErr
         module procedure
     .      AllocErr_Basic,  AllocErr_List,  AllocErr_ArrSz,
     .      AllocErr_ArrSzs, AllocErr_ArrLU, AllocErr_ArrLUs,
     .      AllocErr_ArrLsUs
      end interface

      interface DeallocErr
         module procedure      ! function parameters
     .      DeallocErr_Basic,  ! ... (stat)
     .      DeallocErr_List    ! ... (stat, list)
      end interface

      interface Dealloc        ! Same interface as DeallocErr
         module procedure
     .      DeallocErr_Basic, DeallocErr_List
      end interface
!
! !REVISION HISTORY:
!     24Apr2001  C. Redder  Original version
!     08Aug2002  C. Redder  Removed the interfaces Erase, EchoMode, PBuf,
!                           PrintBuf, ErrHandler and MPErr.  Added the 
!                           interface WPErr.  Removed the following
!                           routines from the interfaces, AllocErr and
!                           Alloc:
!                           AllocErr_ArrsSz, AllocErr_ArrsSzs,
!                           AllocErr_ArrsLU, AllocErr_ArrsLUs,
!                           AllocErr_ArrsLsUs
!                           Changed module for PErr from m_PError to m_PText.
!     27Jan2003  C. Redder  Added the generic interfaces Die and WDie
!     11Jul2003  C. Redder  Made the routines WPErr and PErr internal to 
!                           this module.
! EOP
!-----------------------------------------------------------------
!
! !DEFINED PARAMETERS: 
!
!     Special constants for general use.
!     ----------------------------------
      integer,                 parameter ::
     .   No_Error      = 0

!     Parameters for determining size of output strings
!     ------------------------------------------------- 
      integer, parameter ::
     .   LNameAve   = 2,            ! assumed average length of array name
     .   ArrLenMax  = 50,           ! max string len of each output array name
     .   MaxLen     = 1000,         ! maximum length of output array.
     .   NDigitsMax = range ( 1 )   ! maximum number of digits for an integer

      contains
!...................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
! BOP
! !ROUTINE: PErr -- Print message in conventional mode
!
! !DESCRIPTION:
!     This routine prints a message in conventional mode.  No message is
!     printed if input text is blank.
!
! !INTERFACE:
      subroutine PErr ( where, text, usage, die )
      use m_SysIO, only : die_PErr => Die
      use m_PText, only : PError
      implicit  NONE
!
! !INPUT PARAMETERS:
      character ( len = * ), intent (in) ::
     .   where,              ! Routine returning the error.
     .   text                ! Text message
      character ( len = * ), intent (in), optional ::
     .   usage               ! Usage
      logical,               intent (in), optional ::
     .   die                 ! = .true. to exit program with error status
!
! !REVISION HISTORY:
!     11Jul2003  C. Redder  Original code
!     07Dec2006  C. Redder  Rename ddie to die_PErr
!
! EOP
!-----------------------------------------------------------------

!     Print text message and ...
!     --------------------------
      if ( len_trim ( text ) .gt. 0 ) call PError ( where, text )

!     ... usage (if desired) ...
!     --------------------------
      if ( present      (  usage )) then
         if ( len_trim  (  usage ) .gt. 0 )
     .      call PError ( 'Usage', usage )
      end if

!     ... and exit
!     ------------
      if ( present ( die )) then
         if ( die ) call die_PErr ( where )
      end if

      return
      end subroutine PErr

!...................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
! BOP
! !ROUTINE: WPErr -- Print message in wrap mode
!
! !DESCRIPTION:
!     This routine prints a message in conventional mode.  No message is
!     printed if input text is blank.
!
! !INTERFACE:
      subroutine WPErr ( where, text, usage, die )
      use m_SysIO, only : die_WPErr => Die
      use m_PText, only : WPError
      implicit  NONE
!
! !INPUT PARAMETERS:
      character ( len = * ), intent (in) ::
     .   where,              ! Routine returning the error.
     .   text                ! Text message
      character ( len = * ), intent (in), optional ::
     .   usage               ! Usage
      logical,               intent (in), optional ::
     .   die                 ! = .true. to exit program with error status
!
! !REVISION HISTORY:
!     11Jul2003  C. Redder  Original code
!     07Dec2006  C. Redder  Rename ddie to die_WPErr
!
! EOP
!-----------------------------------------------------------------

!     Print text message and ...
!     --------------------------
      if ( len_trim ( text ) .gt. 0 ) call WPError ( where, text )

!     ... usage (if desired) ...
!     --------------------------
      if ( present       (  usage )) then
         if ( len_trim   (  usage ) .gt. 0 )
     .      call WPError ( 'Usage', usage )
      end if

!     ... and exit
!     ------------
      if ( present ( die )) then
         if ( die ) call die_WPErr ( where )
      end if

      return
      end subroutine WPErr

!...................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
! BOP
! !ROUTINE: Die_Text -- Print message in conventional mode and exit
!
! !DESCRIPTION:
!     This routine prints a message in conventional mode and exits the
!     program with a non-zero error status.  No message is printed if
!     input text is blank.
!
! !INTERFACE:
      subroutine Die_Text ( where, text, usage )
      use m_SysIO, only : ddie => Die
      use m_PText, only : PError
      implicit  NONE
!
! !INPUT PARAMETERS:
      character ( len = * ), intent (in) ::
     .   where,              ! Routine returning the error.
     .   text                ! Text message
      character ( len = * ), intent (in), optional ::
     .   usage               ! Usage
!
! !REVISION HISTORY:
!     28Jan2003  C. Redder  Original code
!     07Jul2003  C. Redder  Changed name of PErr to PError
!
! EOP
!-----------------------------------------------------------------

!     Print text message and ...
!     --------------------------
      if ( len_trim ( text ) .gt. 0 ) call PError ( where, text )

!     ... usage (if desired) ...
!     --------------------------
      if ( present      (  usage )) then
         if ( len_trim  (  usage ) .gt. 0 )
     .      call PError ( 'Usage', usage )
      end if

!     ... and exit
!     ------------
      call ddie    ( where )

      return
      end subroutine Die_Text
!...................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
! BOP
! !ROUTINE: WDie_Text -- Print message in wrap mode and exit
!
! !DESCRIPTION:
!     This routine prints a message in wrap mode and exits the
!     program with non-zero error status.  No message is printed if
!     input text is blank.
!
! !INTERFACE:
      subroutine WDie_Text ( where, text, usage )
      use m_SysIO, only : wddie  => Die
      use m_PText, only : WPError
      implicit  NONE
!
! !INPUT PARAMETERS:
      character ( len = * ), intent (in) ::
     .   where,              ! Routine returning the error.
     .   text                ! Text message
      character ( len = * ), intent (in), optional ::
     .   usage               ! Usage
!
! !REVISION HISTORY:
!     28Jan2003  C. Redder  Original code
!     07Jul2003  C. Redder  Changed name of WPErr to WPError
!     07Dec2006  C. Redder  Rename ddie to wddie
!
! EOP
!-----------------------------------------------------------------

!     Print text message and ...
!     --------------------------
      if ( len_trim ( text ) .gt. 0 ) call WPError ( where, text )

!     ... usage (if desired) ...
!     --------------------------
      if ( present       (  usage )) then
         if ( len_trim   (  usage ) .gt. 0 )
     .      call WPError ( 'Usage', usage )
      end if

!     ... and exit
!     ------------
      call wddie    ( where )

      return
      end subroutine WDie_Text
!...................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
! BOP
! !ROUTINE: ErrStat_Basic -- Generate message of error status from given routine
!
! !DESCRIPTION:
!     This function generates a message that an error status has been
!     returned from a given routine.
!
! !INTERFACE:
      function ErrStat_Basic ( source )
      implicit  NONE
      character ( len = * ), parameter   ::
     .   Text = 'Error status returned from routine, '
!
! !INPUT PARAMETERS:
      character ( len = * ), intent (in) ::
     .   source              ! Routine returning the error.
!
! !OUTPUT PARAMETERS:
      character ( len = len ( Text ) + len_trim ( source ) + 1 ) ::
     .   ErrStat_Basic       ! The generated error message
!
! !REVISION HISTORY:
!
!     06Jul2001  C. Redder  Original code
!
! EOP
!-----------------------------------------------------------------

      ErrStat_Basic = Text // trim ( source ) // '.'

      return
      end function ErrStat_Basic

!...................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
! BOP
! !ROUTINE: ErrStat_iret -- Generate error message with a given status code from given routine
!
! !DESCRIPTION:
!     This function generates an error message with a given status code from
!     a given routine.
!
! !INTERFACE:
      function ErrStat_iret ( source, istat )
      use m_TextUtil, only : ItoA
      implicit  NONE
      character ( len = * ), parameter ::
     .   Text_part1 = 'Error status (stat = ',    ! // istat 
     .   Text_part2 = ') returned from routine, ' ! // source '.'
!
! !INPUT PARAMETERS:
      character ( len = * ), intent (in) ::
     .   source              ! Routine returning the error.
      integer,               intent (in) ::
     .   istat               ! returned status code
!
! !OUTPUT PARAMETERS:
      character ( len = len      ( Text_part1 )
     .                + NDigitsMax + 1
     .                + len      ( Text_Part2 )
     .                + len_trim ( source ) + 1 ) ::
     .   ErrStat_iret        ! The generated error message
!
! !REVISION HISTORY:
!     06Jul2001  C. Redder  Original code
!
! EOP
!-----------------------------------------------------------------

      ErrStat_iret =  Text_Part1
     .             // trim ( ItoA ( istat ))
     .             // Text_Part2
     .             // trim ( source ) // '.'

      return
      end function ErrStat_iret

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  DeallocErr_Basic() --- Generate basic message for deallocation error.
! 
! !DESCRIPTION:
!     This function generates a bare bones message for deallocation errors.
!     The format of the message is
!\begin{verbatim}
!        Deallocation error (stat = istat).
!\end{verbatim}
!     where \verb|istat| is the returned status from deallocate command.
!
! !INTERFACE:
      function DeallocErr_Basic ( istat )
      use      m_TextUtil, only : ItoA
      implicit  NONE
      character ( len = * ), parameter   ::
     .   Text = 'Deallocation error (stat = ' ! // istat // ').'
!
! !INPUT PARAMETERS: 
      integer,            intent (in) ::
     .   istat            ! Status code returned from the deallocate
!                         !   statement.
! !OUTPUT PARAMETERS: 
      character ( len = len ( Text )
     .                + NDigitsMax + 1
     .                + 2 ) ::
     .   DeallocErr_Basic ! The generated error message
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!     06Jul2001  Redder    Origional code.
!
! EOP
!-------------------------------------------------------------------------

      DeallocErr_Basic = Text // trim ( ItoA ( istat )) // ').'

      return
      end function DeallocErr_Basic

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  DeallocErr_List() --- Generate message for deallocation error with array list.
! 
! !DESCRIPTION:
!     This function generates a message for deallocation errors with an array
!     list.  The format of the message is 
!\begin{verbatim}
!        Error (stat = istat) in deallocating list.
!\end{verbatim}
!     where \verb|istat| is the returned status from the deallocate command
!     and \verb|list| is the array list.
!
! !INTERFACE:
      function DeallocErr_List ( istat, list )
      use m_TextUtil, only : ItoA
      use m_ArrList,  only : ArrList
      implicit  NONE
      character ( len = * ), parameter ::
     .   Text_part1 = 'Error (stat = ',    ! // istat 
     .   Text_part2 = ') in deallocating ' ! // list '.'
!
! !INPUT PARAMETERS:
      integer,        intent (in) ::
     .   istat           ! returned status code
      character (len=*), intent (in) ::
     .   list            ! List of arrays.
!
! !OUTPUT PARAMETERS:
      character ( len = len      ( Text_part1 )
     .                + NDigitsMax + 1
     .                + len      ( Text_Part2 )
     .                + 3 * len_trim ( list ) / 2
     .                + 1 ) ::
     .   DeallocErr_List ! The generated error message
!
! !SEE ALSO: 
!     Array() - print out array(s) with dimension size(s)/bound(s)
!
! !REVISION HISTORY:
!     06Jul2001  Redder    Origional code.
!
! EOP
!-------------------------------------------------------------------------


      DeallocErr_List =  Text_part1
     .                // trim ( ItoA    ( istat ))
     .                // Text_part2
     .                // trim ( ArrList (  list )) // '.' 

      return
      end function DeallocErr_List

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AllocErr_Basic() --- Generate basic message for allocation error.
! 
! !DESCRIPTION:
!     This function generates a bare bones message for allocation errors.
!     The format of the message is
!\begin{verbatim}
!        Allocation error (stat = istat).
!\end{verbatim}
!     where \verb|istat| is the returned status from allocate command.  Any
!     pre-existing text in the internal buffer is erased.
!
! !INTERFACE:
      function AllocErr_Basic ( istat )
      use      m_TextUtil, only : ItoA
      implicit  NONE
      character ( len = * ), parameter   ::
     .   Text = 'Allocation error (stat = '
!
! !INPUT PARAMETERS: 
      integer,           intent (in) ::
     .   istat           ! Status code returned from the allocate statement.
!
! !OUTPUT PARAMETERS: 
      character ( len = len ( Text )
     .                + NDigitsMax + 1
     .                + 2 ) ::
     .   AllocErr_Basic  ! The generated error message
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     06Jun2001  Redder    Origional code.
!
! EOP
!-------------------------------------------------------------------------

      AllocErr_Basic = Text // trim ( ItoA ( istat )) // ').'

      return
      end function AllocErr_Basic

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AllocErr_List() --- Generate message for allocation error with array list.
! 
! !DESCRIPTION:
!     This function generates a message for allocation errors with an array
!     list.  The format of the message is 
!\begin{verbatim}
!        Error (stat = istat) in allocating list.
!\end{verbatim}
!     where \verb|istat| is the returned status from the allocate command
!     and \verb|list| is the array list.
!
! !INTERFACE:
      function AllocErr_List ( istat, list )
      use m_TextUtil, only : ItoA
      use m_ArrList,  only : ArrList
      implicit  NONE
      character ( len = * ), parameter ::
     .   Text_part1 = 'Error (stat = ',
     .   Text_part2 = ') in allocating '
!
! !INPUT PARAMETERS: 
      integer,           intent (in) ::
     .   istat           ! Status code returned from the allocate statement.
      character (len=*), intent (in) ::
     .   list            ! List of arrays.
!
! !OUTPUT PARAMETERS:
      character ( len = len      ( Text_part1 )
     .                + NDigitsMax + 1
     .                + len      ( Text_Part2 )
     .                + 3 * len_trim ( list ) / 2
     .                + 1 ) ::
     .   AllocErr_List   ! The generated error message
!
!     Note: The function, Array, can be useful for parsing items in the
!           array list.  For example, the following code,
!
!        use m_AdvError, only : Array, AllocErr
!        use m_die,      only : PErr
!        character (len=*), parameter :: MyName = 'Example'
!        integer M,N, stat
!        M = 20
!        N = 10 
!        stat = 1
!        call PErr ( MyName,
!     &              trim ( AllocErr ( stat,
!     &                                Array ( 'A1', M )
!     &                             // Array ( 'A2', 0, N )
!     &                             // Array ( 'A3', (/M,N/)) ))
!
!        will genrerate the following message to standard error,
!
!    Example(): Error (stat = 1) in allocating A1(20), A2(0:10), A3(20,10)
!
!        Note that the intrinsic function trim is invoked internally to
!        remove trailing blanks.
!
! !SEE ALSO: 
!     Array () - to parse items in the array list.
!     PErr  () - to print out the generated message in buffer to
!                standard error
!
! !REVISION HISTORY:
!     06Jul2001  Redder    Origional code.
!
! EOP
!-------------------------------------------------------------------------

      AllocErr_List =  Text_part1
     .              // trim ( ItoA    ( istat ))
     .              // Text_part2
     .              // trim ( ArrList (  list )) // '.' 

      return
      end function AllocErr_List

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AllocErr_ArrSz() --- Generate message for allocation error of one-dimensional array(s).
! 
! !DESCRIPTION:
!     This function generates a message for allocation error of
!     one-dimensional array(s).  The format of the message is
!\begin{verbatim}
!        Error (stat = istat) in allocating name(size), ...
!\end{verbatim}
!     where  \verb|istat| is the returned status from the allocate command
!     and \verb|name| of the is the name of the first array of size
!     \verb|size|.
!
! !INTERFACE:
      function AllocErr_ArrSz ( istat, names, size )
      use m_TextUtil, only : ItoA
      use m_ArrList,  only : Arr
      implicit  NONE
      character ( len = * ), parameter ::
     .   Text_part1 = 'Error (stat = ',
     .   Text_part2 = ') in allocating '
!
! !INPUT PARAMETERS: 
      integer,           intent (in) ::
     .   istat           ! Status code returned from the allocate statement.
      character (len=*), intent (in) ::
     .   names           ! List of array names (each separated by a comma)
      integer,           intent (in) ::
     .   size            ! Size of the array(s)
!
! !OUTPUT PARAMETERS:
      character ( len = len      ( Text_part1 )
     .                + NDigitsMax + 1
     .                + len      ( Text_Part2 )
     .                + min ( MaxLen, max ( LNameAve, ArrLenMax
     .                         * ( len_trim ( names )
     .                         / ( LNameAve + 1 ) + 1 )))
     .                + 1 ) ::
     .   AllocErr_ArrSz  ! The generated error message
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     06Jul2001  Redder    Origional code.
!     07Aug2002  Redder    Enable function to process a list of array names
!                          separated by commas.
! EOP
!-------------------------------------------------------------------------

      AllocErr_ArrSz =  Text_part1
     .               // trim ( ItoA ( istat ))
     .               // Text_part2
     .               // trim ( Arr  ( names, size )) // '.'

      return
      end function AllocErr_ArrSz
!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AllocErr_ArrSzs() --- Generate message for allocation error of a multi-dimensional array.
! 
! !DESCRIPTION:
!     This function generates a message for allocation errors of multi-
!     dimensional array(s).  The format of the message is
!\begin{verbatim}
!        Error (stat = istat) in allocating name(sizes).
!\end{verbatim}
!     where  \verb|istat| is the returned status from the allocate command
!     and \verb|name| of the is the name of the first array of sizes
!     \verb|sizes|.
!
! !INTERFACE:
      function AllocErr_ArrSzs ( istat, names, sizes )
      use m_TextUtil, only : ItoA
      use m_ArrList,  only : Arr
      implicit  NONE
      character ( len = * ), parameter ::
     .   Text_part1 = 'Error (stat = ',
     .   Text_part2 = ') in allocating '
!
! !INPUT PARAMETERS:
      integer,           intent (in) ::
     .   istat           ! Status code returned from the allocate statement.
      character (len=*), intent (in) ::
     .   names           ! List of array names (each separated by a comma)
      integer,           intent (in), dimension (:) ::
     .   sizes           ! The sizes of the array(s).  The number of
                         !   dimensions in each array is determined by the
                         !   size of this argument.
!
! !OUTPUT PARAMETERS:
      character ( len = len      ( Text_part1 )
     .                + NDigitsMax + 1
     .                + len      ( Text_Part2 )
     .                + min ( MaxLen, max ( LNameAve, ArrLenMax
     .                         * ( len_trim ( names )
     .                         / ( LNameAve + 1 ) + 1 )))
     .                + 1 ) ::
     .   AllocErr_ArrSzs ! The generated error message
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     06Jul2001  Redder    Origional code.
!     07Aug2002  Redder    Enable function to process a list of array names
!                          separated by commas.
! EOP
!-------------------------------------------------------------------------

      AllocErr_ArrSzs =  Text_part1
     .                // trim ( ItoA ( istat ))
     .                // Text_part2
     .                // trim ( Arr  ( names, sizes )) // '.'

      return
      end function AllocErr_ArrSzs

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AllocErr_ArrLU() --- Generate message for allocation error of one dimensional array(s) with a lower bound
! 
! !DESCRIPTION:
!     This function generates a message for allocation error of one-
!     dimensional array(s) with a defined lower bound.  The format of the
!     message is
!\begin{verbatim}
!         Error (stat = istat) in allocating name(lb:ub).
!\end{verbatim}
!     where  \verb|istat| is the returned status from the allocate command
!     and \verb|name| of the is the name of the first array with lower and
!     upper bounds \verb|lb| and \verb|ub|.
!
! !INTERFACE:
      function AllocErr_ArrLU ( istat, names, lb, ub )
      use m_TextUtil, only : ItoA
      use m_ArrList,  only : Arr
      implicit  NONE
      character ( len = * ), parameter ::
     .   Text_part1 = 'Error (stat = ',
     .   Text_part2 = ') in allocating '
!
! !INPUT PARAMETERS: 
      integer,           intent (in) ::
     .   istat           ! Status code returned from the allocate statement.
      character (len=*), intent (in) ::
     .   names           ! List of array names (each separated by a comma)
      integer,           intent (in) ::
     .   lb,             ! The lower and
     .   ub              ! ... upper bounds
!
! !OUTPUT PARAMETERS: 
      character ( len = len      ( Text_part1 )
     .                + NDigitsMax + 1
     .                + len      ( Text_Part2 )
     .                + min ( MaxLen, max ( LNameAve, ArrLenMax
     .                         * ( len_trim ( names )
     .                         / ( LNameAve + 1 ) + 1 )))
     .                + 1 ) ::
     .   AllocErr_ArrLU  ! The generated error message
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     06Jul2001  Redder    Origional code.
!     07Aug2002  Redder    Enable function to process a list of array names
!                          separated by commas.
! EOP
!-------------------------------------------------------------------------

      AllocErr_ArrLU  =  Text_part1
     .                // trim ( ItoA ( istat ))
     .                // Text_part2
     .                // trim ( Arr  ( names, lb, ub )) // '.'

      return
      end function AllocErr_ArrLU

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AllocErr_ArrLUs() --- Generate message for allocation error of multi-dimensional array(s) with a lower bound
! 
! !DESCRIPTION:
!     This function generates a message for allocation error of multi-
!     dimensional array(s) with a defined lower bound.  The format of the
!     message is
!\begin{verbatim}
!        Error (stat = istat) in allocating name(lb:ub, ... ).
!\end{verbatim}
!     where  \verb|istat| is the returned status from the allocate command
!     and \verb|name| of the is the name of the first array with lower and
!     upper bounds \verb|lb| and \verb|ub|.
!
! !INTERFACE:
      function AllocErr_ArrLUs ( istat, names, lb, ubs )
      use m_TextUtil, only : ItoA
      use m_ArrList,  only : Arr
      implicit  NONE
      character ( len = * ), parameter ::
     .   Text_part1 = 'Error (stat = ',
     .   Text_part2 = ') in allocating '
!
! !INPUT PARAMETERS: 
      integer,           intent (in) ::
     .   istat           ! Status code returned from the allocate statement.
      character (len=*), intent (in) ::
     .   names           ! List of array names (each separated by a comma)
      integer,           intent (in) ::
     .   lb,             ! The lower (for the all dimensions) and
     .   ubs (:)         ! ... upper bounds.  The number of dimensions in
                         !     the array(s) is determined by the size of
                         !     the argument, ubs
!
! !OUTPUT PARAMETERS: 
      character ( len = len      ( Text_part1 )
     .                + NDigitsMax + 1
     .                + len      ( Text_Part2 )
     .                + min ( MaxLen, max ( LNameAve, ArrLenMax
     .                         * ( len_trim ( names )
     .                         / ( LNameAve + 1 ) + 1 )))
     .                + 1 ) ::
     .   AllocErr_ArrLUs ! The generated error message
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     06Jul2001  Redder    Origional code.
!     07Aug2002  Redder    Enable function to process a list of array names
!                          separated by commas.
! EOP
!-------------------------------------------------------------------------

      AllocErr_ArrLUs =  Text_part1
     .                // trim ( ItoA ( istat ))
     .                // Text_part2
     .                // trim ( Arr  ( names, lb, ubs )) // '.'

      return
      end function AllocErr_ArrLUs

!...................................................................

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  AllocErr_ArrLsUs() --- Generate message for allocation error of multi-dimensional array(s) with lower bounds
! 
! !DESCRIPTION:
!     This function generates a message for allocation error of multi-
!     dimensional array(s) with defined lower bounds.  The format of the 
!     message is 
!\begin{verbatim}
!        Error (stat = istat) in allocating name(lb:ub, ... ).
!\end{verbatim}
!     where  \verb|istat| is the returned status from the allocate command
!     and \verb|name| of the is the name of the first array with lower and
!     upper bounds \verb|lb| and \verb|ub|.
!
! !INTERFACE:
      function AllocErr_ArrLsUs ( istat, names, lbs, ubs )
      use m_TextUtil, only : ItoA
      use m_ArrList,  only : Arr
      implicit  NONE
      character ( len = * ), parameter ::
     .   Text_part1 = 'Error (stat = ',
     .   Text_part2 = ') in allocating '
!
! !INPUT PARAMETERS: 
      integer,            intent (in) ::
     .   istat            ! Status code returned from the allocate statement.
      character (len=*),  intent (in) ::
     .   names            ! List of array names (each separated by a comma)
      integer,            intent (in) ::
     .   lbs (:),         ! The lower and
     .   ubs (:)          ! ... upper bounds.  The number of dimensions in
                          !     the array(s) is determined by the maximum
                          !     size of both arguments.
!
! !OUTPUT PARAMETERS: 
      character ( len = len      ( Text_part1 )
     .                + NDigitsMax + 1
     .                + len      ( Text_Part2 )
     .                + min ( MaxLen, max ( LNameAve, ArrLenMax
     .                         * ( len_trim ( names )
     .                         / ( LNameAve + 1 ) + 1 )))
     .                + 1 ) ::
     .   AllocErr_ArrLsUs ! The generated error message
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     06Jul2001  Redder    Origional code.
!     07Aug2002  Redder    Enable function to process a list of array names
!                          separated by commas.
! EOP
!-------------------------------------------------------------------------

      AllocErr_ArrLsUs =  Text_part1
     .                 // trim ( ItoA ( istat ))
     .                 // Text_part2
     .                 // trim ( Arr ( names, lbs, ubs )) // '.'

      return
      end function AllocErr_ArrLsUs
!...................................................................

      end module m_AdvError

*====================================================================
