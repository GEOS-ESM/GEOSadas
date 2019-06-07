!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: m_SysIO - a F90 module defines system I/O parameters
!
! !DESCRIPTION:
!     Define system dependent I/O parameters.
!
! !INTERFACE:
      module       m_SysIO
!     use m_die,   only PErr
!     use m_stdio, only stdin, stdout, stderr
      implicit none
      private
!
      public ::
     .   stdin,  ! a unit linked to UNIX stdin  (standard input )
     .   stdout, ! a unit linked to UNIX stdout (standard output)
     .   stderr, ! a unit linked to UNIX stderr (standard error )
     .   LWhere, ! number of characters in suffix to routine name
     .   PErr,   ! print line of text to standard error
     .   POut,   ! print line of text to file
     .   Die,    ! print rank/routine name to std error and exit program
     .   LUAvail ! return available FORTRAN logical IO unit number
!
! !REVISION HISTORY:
!     10Oct1996 - Jing G.   - Defined
!     29Jul2002 - C. Redder - Adapted to eu2 library.  Module name changed
!                             to m_SysIO
!     25Nov2002 - C. Redder - Added generic interface PErr and parameter
!                             WhereSuf
!     07Jan2003 - C. Redder - Removed public attribute of parameter
!                             WhereSuf and added generic interface POut
!                             and routine LWhere
!EOP
!_______________________________________________________________________

!     Constants for general use.
!     --------------------------
      character (len=*), parameter ::
     .   SP               =  achar (32) ! Space (i.e. blank)

!     Parameters for modifying routine names
!     --------------------------------------
      character (len=*), parameter ::
     .   WherePref        = SP,         ! Prefix added to routine names
     .   WherePref_stderr = SP,         ! ... for messages sent to stderr
     .   WhereSuf         = ': ',       ! Suffix added to routine names
     .   WhereSuf_stderr  = '(): '      ! ... for messages sent to stderr

!     Defines standard I/O units.
!     ---------------------------
      integer, parameter :: stdin  = 5
      integer, parameter :: stdout = 6
      integer, parameter :: stderr = 0

! #ifdef sysHP-UX      
!      integer, parameter :: stderr = 7 ! Setting for HP-UX
! #else
!      integer, parameter :: stderr = 0 ! .. and for generic platforms
! #endif
      interface PErr
         module procedure
     .      PErr1
      end interface
      interface POut
         module procedure
     .      POut1
      end interface

      contains
*...................................................................

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
! BOP
! !ROUTINE: Die -- Print rank and routine name and exit program
!
! !DESCRIPTION:
!     This routine prints the routine in conventional mode and exits the
!     program with a non-zero error status
!
! !INTERFACE:
      subroutine Die ( Where )
!      use m_die, only : ddie => die
      implicit  NONE
!
! !INPUT PARAMETERS:
      character ( len = * ), intent (in) ::
     .   where               ! Routine returning the error.

      integer, parameter :: Error  = 2 ! Return status upon program termination
      integer, parameter :: MyRank = 0 ! MPI rank
!
! !REVISION HISTORY:
!     28Jan2003  C. Redder  Original code
!
! EOP
!-----------------------------------------------------------------

      character (len=*), parameter :: MyName_ = 'die'

      write   ( stderr, '(1x,z3.3,5a)' ) MyRank, '.', MyName_,
     .                                ': from ', trim ( Where ), '()'
      call exit ( Error )

!     call ddie ( Where )

      return
      end subroutine Die
!...................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  LWhere() --- Return string length of routine name with prefix and suffix
! 
! !DESCRIPTION:
!     This routine returns the string length of the routine name including
!     its prefix and suffixes that would be added by the routines in this
!     module.
!
! !INTERFACE:
      function LWhere ( lu, Where )
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,           intent (in) ::
     .   lu      ! Fortran logical unit number for the given file
      character (len=*), intent (in) ::
     .   Where   ! Routine name
! !OUTPUT PARAMETERS:
      integer ::
     .   LWhere  ! Length of string of routine name including prefixes
                 !   and suffixes
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     07Jan2003  Redder Original code.
! EOP
!-------------------------------------------------------------------------

      if ( lu .eq. stderr ) then
         LWhere = len ( WherePref_stderr // Where
     .               // WhereSuf_stderr )
      else
         LWhere = len ( WherePref        // Where // WhereSuf )
      end if
      if ( Where .eq. SP ) LWhere = 1

      return
      end function LWhere
!...................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  POut1() --- Print a line of text to file
! 
! !DESCRIPTION:
!     This routine prints a line of text to a file given by its
!     fortran logical unit number.
!
! !INTERFACE:
      subroutine   POut1 ( lu, Where, Line )
!
! !INPUT PARAMETERS: 
      implicit NONE
      integer,           intent (in) ::
     .   lu      ! Fortran logical unit number for the given file
      character (len=*), intent (in) ::
     .   Where,  ! Routine name
     .   Line    ! Line of text.
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     07Jan2003  Redder Original code.
! EOP
!-------------------------------------------------------------------------

      if ( lu .eq. stderr ) then
         call PErr1 ( Where, Line )

      else
         if ( Where .eq. SP ) then
            write ( lu, '(a)' ) SP // trim ( Line )
         else
            write ( lu, '(a)' )
     .         WherePref // Where //
     .         WhereSuf  // trim ( Line )
         end if
      end if

      return
      end subroutine POut1
!...................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  PErr1() --- Print a line of text to standard error
! 
! !DESCRIPTION:
!     This routine prints a line of text to standard error.
!
! !INTERFACE:
      subroutine   PErr1 ( Where, Line )
!     use m_die, only : PPerr => PErr
!
! !INPUT PARAMETERS: 
      implicit NONE
      character (len=*), intent (in) ::
     .   Where, ! Routine name
     .   Line   ! Line of text
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     25Nov2002  Redder  Original code.
!     07Jan2003  Redder  Renamed input argument Message to Line.  Added
!                        prefix to routine name on output.
! EOP
!-------------------------------------------------------------------------

!     call PPerr ( Where, Line )
      if ( Where .eq. SP ) then
         write ( stderr, '(a)' ) SP // trim ( Line )
      else
         write ( stderr, '(a)' )
     .      WherePref_stderr // Where //
     .      WhereSuf_stderr  // trim ( Line )
      end if

      return
      end subroutine PErr1
!...................................................................
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  LUAvail() --- Locate the next available fortran logical unit
! 
! !DESCRIPTION:
!     Look for an available (not opened and not statically assigned to any
!     I/O attributes to) logical unit.
!
! !INTERFACE:
      function LUAvail ()
!     use m_die, only : PPerr => PErr
!
! !INPUT PARAMETERS: 
      implicit NONE
!
! !OUTPUT PARAMETERS: 
      integer :: LUAvail  ! Available logical unit number.  If none is
!                           found, then -1 is returned
!
! !SEE ALSO: 
!
! !REVISION HISTORY:
!     06May2003   C. Redder  Adapted code from mpeu library
!
! EOP
!-------------------------------------------------------------------------

      integer, parameter :: MX_LU = 255
      integer :: lu, ios
      logical :: in_use
      lu     = -1
      ios    =  0
      in_use = .true.

      do while ( ios .eq. 0 .and. in_use )
         lu = lu + 1

!        Test #1, reserved
!        -----------------
         in_use = lu .eq. stdout .or.
     .            lu .eq. stdin  .or.
     .            lu .eq. stderr

!        Test #2, inquire
!        ----------------
         if ( .not. in_use ) inquire ( unit   = lu,
     .                                 opened = in_use,
     .                                 iostat = ios )
         if ( lu .ge. MX_LU ) ios = -1  ! No more unit available if
      end do                            !   lu exceeds maximum value

      if ( ios .ne. 0 ) lu = -1
      LUAvail = lu

      return
      end function LUAvail
!...................................................................
      end module m_SysIO
!====================================================================
!.
