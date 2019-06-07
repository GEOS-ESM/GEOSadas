      program explode
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: explode : separate prepbufr files into files by SUBSET
!
! !INTERFACE:
!
!      Usage:  explode  inbfrfile
!
!      where: inbfrfile   input bufr file
!
!      output files:   SUBSET.yyyymmddhh
!
! !USES:
!
      implicit none

! !DESCRIPTION:  sample program to read in obs from a bufr file and
!                write out in prepbufr files separated by subset type
!
! !REVISION HISTORY:
!
!  09jan2009    Meta   Initial version (?)
!  05may2013    Meta   added prologue
!
!EOP
!-----------------------------------------------------------------------
      
      character(len=100) filnam
      CHARACTER(len=8)  SUBSET, osubset
      integer nargs, iargc
      integer lubfi, lubfo
      integer idate
      integer iret

      data lubfi,lubfo /10,11/

      nargs = iargc()

      if (nargs .lt. 1) then
         print *,'Usage: explode.x BUFR_file'
         stop
      endif

      call getarg(1,filnam)

      call datelen(10)
      osubset = ''

      open (unit=lubfi,file=filnam,form='unformatted')

      CALL OPENBF(LUBFI,'IN',LUBFI)
      CALL READMG(LUBFI,SUBSET,IDATE,IRET)

C  LOOP THROUGH THE INPUT MESSAGES - READ THE NEXT SUBSET
C  ------------------------------------------------------

      do while(iret .eq. 0) 
         if (osubset .ne. subset) then
            if (osubset .ne. '') call closbf(lubfo)
            osubset = subset
            call filopn(subset,idate,lubfi,lubfo)
         endif
         call copymg(lubfi,lubfo)
         CALL READMG(LUBFI,SUBSET,IDATE,IRET)

      end do
      stop
      end program explode


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 610.1, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !ROUTINE: filopn:  open bufr file for output
!
!
! !INTERFACE:
!
      subroutine filopn(subset,idate,lubfi,lubfo)
!
! !USES:
!
      implicit none

! !INPUT PARAMETERS:
!
      character(len=8) subset     ! subset name (used for filename)
      integer idate               ! date for data (used for filename)
      integer lubfi               ! input unit (supplies bufr table)
      integer lubfo               ! output unit

! !REVISION HISTORY:
!
!  09jan2009    Meta   Initial version (?)
!  05may2013    Meta   added prologue
!
!EOP
!-----------------------------------------------------------------------

      character(len=40) outfil
      logical ex
      
      write(outfil,'(a6,a1,i10.10)') subset(1:6),'.',idate

      inquire(file=outfil,exist=ex)

      if (ex)  then       ! file exists, so append
         open(unit=lubfo,file=outfil,form='unformatted',
     &        action='readwrite')
         call openbf(lubfo,'APX',lubfi)
      else
         open(unit=lubfo,file=outfil,form='unformatted',
     &        action='write')
         call openbf(lubfo,'OUT',lubfi)
      endif

      return
      end subroutine filopn
