      program chkbufr
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: chkbufr: Check to see if file can be read as BUFR
!
! !INTERFACE:
!
!      Usage:  chkbufr  input-bufr-file
!
!
! !USES:
!      
      implicit NONE
!
!     link to NCEP bufrlib
      
! !DESCRIPTION:  Open and read first record of a file as in OPENBF
!                to determine if the file is BUFR.  If so, exit w/o error
!
! !REVISION HISTORY:
!
!  21May2009    Meta   Initial version
!
!EOP
!-----------------------------------------------------------------------
      integer argc
      integer ichkstr
      integer i
      integer(4) iargc
      character(len=255) argv
      CHARACTER(len=128) BORT_STR
      CHARACTER*1   BSTR(4)

      integer lunit

      lunit = 35
      argc = iargc()
      if (argc < 1) then
         print *,'Usage: chkbufr.x input-bufr-file'
         stop
      endif
      call GetArg (1_4, argv)
      open(unit=lunit,file=argv,form='unformatted')

      call WRDLEN
      
      REWIND LUNIT
      READ(LUNIT,END=200,ERR=902) (BSTR(I),I=1,4)

C        Confirm that the BSTR array contains 'BUFR' encoded in
C        CCITT IA5 (i.e. ASCII).

      IF(ICHKSTR('BUFR',BSTR,4).NE.0) GOTO 903

      stop

200   CONTINUE
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*, 'CHKBUFR - INPUT BUFR FILE IN UNIT ',LUNIT,
     . ' IS EMPTY'
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
      stop

902   WRITE(BORT_STR,'("CHKBUFR - ERROR READING INPUT FILE '//
     . 'CONNECTED TO UNIT",I4," WHEN CHECKING FOR ''BUFR'' IN FIRST 4'//
     . ' BYTES OF RECORD")') LUNIT
      CALL BORT(BORT_STR)


903   WRITE(BORT_STR,'("CHKBUFR - FIRST 4 BYTES READ FROM '//
     . 'RECORD IN INPUT FILE CONNECTED TO UNIT",I4," NOT ''BUFR'', '//
     . 'DOES NOT CONTAIN BUFR DATA")') LUNIT
      CALL BORT(BORT_STR)

      end program chkbufr
