              PROGRAM PREPOBS_LISTHEADERS
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: PREPOBS_LISTHEADERS
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2013-03-06
!
! ABSTRACT: GENERATES A LIST OF ALL UNIQUE HEADERS IN A PREPBUFR FILE
!    MATCHING INPUT LIST.  REORDERS THE FILE INTO THE INPUT LIST ORDER.
!
! PROGRAM HISTORY LOG:
! 1999-06-29  KISTLER -- ORIGINAL AUTHOR
! 2000-06-15  KEYSER  -- REPLACED INCORRECT CALL TO W3TAGB WITH CALL
!                        TO W3TAGE AT END OF CODE
! 2000-12-05  KEYSER  -- NOW READS THROUGH THE ENTIRE INPUT FILE WHEN
!                        LOOKING FOR MESSAGES MATCHING A PARTICULAR TYPE
!                        FROM THE INPUT LIST, BEFORE IT STOPPED LOOKING
!                        FOR MESSAGES MATCHING THE TYPE IN THE INPUT LIST
!                        ONCE THE INPUT MESSAGE TYPE CHANGED TO ANOTHER
!                        TYPE (THIS ALLOWS FOR INPUT PREPBUFR FILES WITH
!                        INTERMINGLED MESSAGE TYPES TO BE HANDLED
!                        PROPERLY)
! 2013-03-06  KEYSER  -- CHANGES TO RUN ON WCOSS
!
! USAGE:
!   INPUT FILES:
!     STDIN    - NAMELIST/NAMIN/
!     UNIT 11  - PREPBUFR FILE TO BE LISTED
!
!   OUTPUT FILES:
!     UNIT 06  - STANDARD PRINTFILE
!     UNIT 51  - REORDERED PREPBUFR FILE
!     UNIT 52  - LISTING OF UNIQUE HEADERS
!
!   SUBPROGRAMS CALLED:
!     LIBRARY:
!       BUFRLIB  - DATELEN  OPENBF UFBMEM  RDMEMM  CLOSBF
!       W3NCO    - W3TAGB   W3TAGE
!
!   EXIT STATES:
!     COND =   0 - SUCCESSFUL RUN
!     COND =   1 - BUFR I/O ERROR 
!
! REMARKS: NONE.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  NCEP WCOSS
!
!$$$

! program prepobs_listheaders

implicit none

character*8  subset, prvset
character*8, allocatable :: bufrlist(:)
integer,      allocatable :: kount(:)
integer :: iret,jdate,k,m,kmsgs,lunin=11,klist=0,lunot=51,lunlst=52

      CALL W3TAGB('PREPOBS_LISTHEADERS',2013,0065,0057,'NP22')

read(*,*) klist
if (klist.lt.1) then
    write(lunlst,*) ' klist = ',klist, 'not set > 1 on namelist namin'
endif
allocate (bufrlist(klist),kount(klist))

k=0
do k=1,klist
   read(*,'(a6)') bufrlist(k)(1:6)
enddo

call ufbmem(lunin,0,kmsgs,iret)
call datelen(10)
call openbf(lunot,'OUT',lunin)

do k=1,klist
   kount(k)=0
   do m=1,kmsgs+1
      call rdmemm(m,subset,jdate,iret)
      if (iret.ne.0 ) then
! All messages have been read; write out the count associated with the
!  current list type and move on to the next list type
         write(lunlst,*) bufrlist(k)(1:6), kount(k)
         exit
      else  if (subset(1:6) .ne. bufrlist(k)(1:6)) then
! This message does not match the current list type; read in the next
!  message in the input file
         cycle
      else
! This message matches the current list type, count it and copy it to the
!  output file, then read in the next message in the input file
         kount(k)=kount(k)+1
         call copymg(lunin,lunot)
      endif
   enddo
enddo

call closbf(lunin)
      CALL W3TAGE('PREPOBS_LISTHEADERS')
stop
end
