!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3   !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE:  m_pbutil --- utility routines for BUFR, PREPBUFR
!
! !INTERFACE:
!

      MODULE  m_pbutil
      
! !USES:
     
      use m_ioutil, only : luavail
      implicit none
      
! !DESCRIPTION:
!
!  Utility routines for PREPBUFR that are shared by the PB writing
!   modules - open and close the files
!     
!
! !REVISION HISTORY:
!
!  16Apr2004   Meta    Added i_bfr, r_bfr related definitions to handle
!                       calling BUFRLIB compiled with different options
!                       (specifically, -i8) than the module.
!  08Nov2004   Meta    Copied from m_pbmin module
!
!EOP
!-------------------------------------------------------------------------

      integer, parameter :: i_bfr = 4    ! size of integer for bufrlib
      integer, parameter :: r_bfr = 4    ! size of real for bufrlib

      integer ludx                       !  unit number for table file
      integer(i_bfr) ludx_b              !  unit number for table file
      integer lu                         !  unit number for output file
      integer(i_bfr) lu_b                !  unit number for output file
      
      real(8) :: missing = 10.e10
      
      real(r_bfr)    pgm_code            !  'program code' to use for  data

      CONTAINS   

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3   !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_bufr ---  open BUFR file and set up for writing
!
! !INTERFACE:

      subroutine init_bufr(outputfile,
     &      tablefile, isbufr, append, bfrunit, pcode)    !  optional
            
! !INPUT PARAMETERS

      character(len=*),intent(in) ::  outputfile ! name of output BUFR file
      character(len=*), optional, intent(in) :: tablefile ! BUFR table file
      logical, optional, intent(in) :: isbufr  ! BUFR table file indicator
      logical, optional, intent(in) :: append  ! append to prev. file if true
      integer(i_bfr),optional :: bfrunit       ! pre-opened BUFR file unit
                                               !  number to read table from
      real, optional, intent(in)    :: pcode
      
! !DESCRIPTION:
!     
!    Open user specified file, and set up for writing BUFR
!
! !REVISION HISTORY:
!
!  07Apr2004   Meta    Initial code
!  14Jul2004   Meta    Add code for using tables from pre-existing BUFR file
!   9Dec2004   Meta    Now can send unit number of previously opened 
!                         BUFR file to supply the BUFR table
!   8Nov2007   Meta    Add option to append to existing file, will use
!                         BUFR table from that file if found.
!  14Nov2007   Meta    Some refinements to 'append' and other options
!
!EOP
!-------------------------------------------------------------------------

      character(len=200) bufrtable
      integer(i_bfr) idtlen
      logical usebufr
      logical apnfile, ex
      character(len=8) io

      usebufr = .false.
      apnfile = .false.
      
      if (present(tablefile)) then
         bufrtable = tablefile
      else
         bufrtable="prepobs_prep.bufrtable"
      endif

      if (present(isbufr)) then
         usebufr = isbufr
      endif

      if (present(append)) then
         apnfile = append
! if true, inquire about output file.  if not exist, set false
!  IF  true will use 'apn' when opening bufr file.
         if (apnfile) then
            inquire(file=outputfile,exist=ex)
            if (.not. ex) then
               print *,'File does not exist: ',outputfile
               apnfile = .false.
            endif
         endif
      endif

! find unit numbers for files
      lu = luavail()
      lu_b = lu
      io = 'OUT'
      if (apnfile) then
         open(unit=lu,file=outputfile,form='unformatted',
     &     action='readwrite')
      else
         open(unit=lu,file=outputfile,form='unformatted',action='write')
      endif

      if (apnfile) then         ! will use same bufr unit for bufr table
         ludx_b = lu
         io = 'APX'
         usebufr = .false.
      else if (present(bfrunit)) then
         usebufr = .false.
         ludx_b = bfrunit
      else
         ludx = luavail()
         if ( usebufr ) then
           open(unit=ludx,file=bufrtable,action='read',
     &           form='unformatted')
         else
           open(unit=ludx,file=bufrtable,action='read',form='formatted')
         endif
         ludx_b = ludx
      endif
      
      idtlen = 10
      call datelen(idtlen)

! if using table file for BUFR tables, open it
      if (usebufr) call openbf(ludx_b,'IN ',ludx_b)

      call openbf(lu_b,io,ludx_b)

! close input BUFR file used for tables, if opened 
      if (usebufr) then
         call closbf(ludx_b)
      else if ( .not. present(bfrunit) .and. .not. apnfile) then
         close(ludx)
      endif

      if (present(pcode)) then
         pgm_code = pcode
      endif
      
      return

      end subroutine init_bufr

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3   !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: end_bufr ---  flush and close BUFR file
!
! !INTERFACE:

      subroutine end_bufr()
                  
! !DESCRIPTION:
!     
!    Close files opened by subroutine 'init_bufr'
!
! !REVISION HISTORY:
!
!  07Apr2004   Meta    Initial code
!
!EOP
!-------------------------------------------------------------------------

!     close(ludx)
      
! closbf will close and write last BUFR message, then close the file.

      call closbf(lu_b)

      return
      
      end subroutine end_bufr
      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3   !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: touch_date ---  write a date record to BUFR file
!
! !INTERFACE:

      subroutine touch_date(subset, idate)

! !INPUT PARAMETERS
      character(len=8),intent(in) ::  subset
      integer,intent(in)          ::  idate
                  
! !DESCRIPTION:
!     
!    Open and initialize a new BUFR message with a given date/time
!    stamp. 
!    
!
! !REVISION HISTORY:
!
!  29Oct2004   Meta    Initial code
!
!EOP
!-------------------------------------------------------------------------

      integer(i_bfr)  ibdate 

      ibdate = idate

      call openmg (lu_b, subset, ibdate) 

      return
      
      end subroutine touch_date

      end module m_pbutil
