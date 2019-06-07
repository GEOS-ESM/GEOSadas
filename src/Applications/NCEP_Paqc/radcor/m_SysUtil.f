!====================================================================
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_SysUtil -- System utilities
!
! !DESCRIPTION:
!     This module contains utilities that perfrom operating system commands
!
! !INTERFACE:
!
      module      m_SysUtil
      implicit    NONE
      private	! except

      public ::
     .   Delete   ! Removes file
!
! !REVISION HISTORY:
!     11May2002  C. Redder  Original code
!     08Aug2002  C. Redder  Removed parameter statements defining EOL and BLK
!     30Jan2007  C. Redder  Correct the string assigned for MyModule.
!
!EOP
!-----------------------------------------------------------------
      character (len=*), parameter :: MyModule = 'm_SysUtil'

!     Status codes
!     ------------
      integer, parameter ::
     .   No_Error        = 0,  ! Valid error status; no error
     .   No_LUAvail      = 1,  ! No available FORTRAN logical unit number
     .   Open_Error      = 2,  ! Error in opening a file
     .   Read_Error      = 3,  ! Error in reading a file
     .   Write_Error     = 4,  ! Error in writing to a file
     .   Alloc_Error     = 5,  ! Allocation error
     .   Dealloc_Error   = 6,  ! Allocation error
     .   Close_Error     = 7,  ! Error in closing a file
     .   File_NotFound   = 8,  ! File not found (i.e. File does not exist)
     .   Inquire_Error   = 9   ! Inquire error

      contains
!..............................................................

!-------------------------------------------------------------------------
!         Nasa/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
! BOP
!
! !ROUTINE:  Delete () --- Removes target file
! 
! !DESCRIPTION:
!     This routine removes a file (opened or unopened).
!
! !INTERFACE:
      subroutine Delete ( File, stat, silent )
      use m_AdvError, only : ItoA, PErr, ErrStat
      use m_SysIO,    only : LUAvail
!
! !INPUT PARAMETERS:
      implicit NONE
      character (len=*), intent (in) ::
     .   File            ! Target file
      
      logical,           intent (in), optional ::
     .   silent          ! = .true. to suppress error messages.
!
! !OUTPUT PARAMETERS:
      integer,           intent (out) ::
     .   stat            ! Returned status code
!
! !SEE ALSO:
!
! !REVISION HISTORY: 
!
!     11May2002  C. Redder  Origional code
!     07Aug2002  C. Redder  Replaced calls to subroutine FPErr with calls
!                           to PErr and modified all calls to PErr in
!                           response to changes in the interface.
!     06Dec2006  C. Redder  Modified use statement access m_SysIO instead
!                           of m_ioutil.  Modified messages to stderr to
!                           reflect changes in modules m_AdvError and
!                           m_PText.
! EOP
!-------------------------------------------------------------------------
      character (len=*), parameter :: MyName = MyModule // '::Delete'

      logical :: exist, opened, verbose, OK_if_missing
      integer :: lu

!     Implement the silent option
!     ---------------------------
      verbose       = .true.
      if ( present ( silent )) verbose = .not. silent
      OK_if_missing = .true.

!     Determine if file exists
!     ------------------------
      Inquire ( file = File, exist = exist, iostat = stat )
      if ( stat .ne. 0 ) then
         if ( verbose )
     .      call PErr ( MyName, 'Inquire error (iostat = '
     .                       //  trim ( ItoA ( stat )) // ') \n'
     .                       // '   Target file = ' // trim ( File ))
         stat = Inquire_Error
         return
      end if

!     Return with error status if the file does not exist
!     ---------------------------------------------------
      if ( .not. exist ) then
         if ( verbose .and. .not. OK_if_missing )
     .      call PErr ( MyName, 'Target file does not exist \n'
     .                       // '   File = ' // trim ( File ))
         stat = File_NotFound
         if ( OK_if_missing ) stat = No_Error
         return
      end if

!     Determine if file is opened
!     ---------------------------
      Inquire ( file = File, opened = opened, iostat = stat )
      if ( stat .ne. 0 ) then
         if ( verbose )
     .      call PErr ( MyName, 'Inquire error (iostat = '
     .                       //  trim ( ItoA ( stat )) // ') \n'
     .                       // '   Target file = ' // trim ( File ))
         stat = Inquire_Error
         return
      end if

!     If the file is opened ...
!     -------------------------
      if ( opened ) then

!        ... determine the FORTRAN logical unit number
!        ---------------------------------------------
         Inquire ( file = File, number = lu, iostat = stat )
         if ( stat .ne. 0 ) then
            if ( verbose )
     .         call PErr ( MyName, 'Inquire error (iostat = '
     .                          //  trim ( ItoA ( stat )) // ') \n'
     .                          // '   Target file = ' // trim ( File ))
            stat = Inquire_Error
            return
         end if
      else ! ... determine an available logical unit number
!     -----------------------------------------------------
         lu = LUAvail()
         if ( lu .lt. 0 ) then
            if ( verbose )
     .         call PErr ( MyName, 'No logical units available. \n'
     .                          // '   File = ' // trim ( File ))
            stat = No_LUAvail
            return
         end if

!        ... opened the file with status = replace
!        -----------------------------------------
         open ( unit = lu,
     .          file = File,
     .          status = 'replace',          
     .          iostat =  stat )
         if ( stat .ne. 0 ) then
            if ( verbose )
     .         call PErr ( MyName, 'Open error (iostat = '
     .                          //  trim ( ItoA ( stat )) // ') \n'
     .                          // '   Target file = ' // trim ( File))
            stat = Open_Error
            return
         end if
      end if

!     Close and delete file
!     ---------------------
      close ( unit = lu, status = 'delete', iostat = stat )
      if ( stat .ne. 0 ) then
         if ( verbose )
     .       call PErr ( MyName, 'Close and delete error (iostat = '
     .                        //  trim ( ItoA ( stat )) // ') \n'
     .                        // '   Target file = ' // trim ( File ))
         stat = Close_Error
         return
      end if

!     All is well
!     -----------
      stat = 0

      return
      end subroutine Delete

!.................................................................
      end module m_SysUtil
!====================================================================
