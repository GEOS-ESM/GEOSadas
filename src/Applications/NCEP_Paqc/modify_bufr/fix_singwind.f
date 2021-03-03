      program fix_singwind
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: fix_singwind:  overwrite purge QM on Singapore strat. wind
!
! !INTERFACE:
!
!     Usage:  fix_singwind.x input_prepbufr output_prepbufr
!
! !USES:
!
      implicit NONE
!
!  link to libbfr_r4i4.a library

! !DESCRIPTION:  simple routine to overwrite purge mark for Singapore wind
!                (derived from code to tweak OIQC QMs)
!
! !REVISION HISTORY:
!
!     13Dec2006    Meta   Initial version
!
!EOP
!-----------------------------------------------------------------------

      integer luin, luout       !  unit numbers

      integer argc
      integer(4) iargc
      integer      ireadmg
      integer      ireadsb

      character*120 inputfile
      character*120 outputfile

      character*8  subset       ! name of current BUFR subset
      character*8  psubset      ! name of previous BUFR subset

      integer      idate        ! synoptic date/time YYYYMMDD

      integer      iret         ! subroutine return code

      integer      klev, llev   ! no. of levels in report

      character*30  evnstr,hdstr
      
      logical      mod          ! flag indicating whether to modify report

      integer  i,j              ! loop counters
      integer  n                ! report counter

      real*8     bmiss          !  missing data value
      
      real*8 evn(5,255)         ! wind profile from bufr file
      real*8 hdr(2)
      character*8 sid

      equivalence(hdr,sid)
      real wmax                 ! gross check parameter for wind.
     
      data hdstr  /'SID TYP'/
      data evnstr /'POB PQM UOB VOB WQM'/

      data luin /8/, luout /9/

      bmiss = 10.e10
      wmax = 300.                !  gross check

      argc = iargc()
      if (argc .lt. 2) then
         print *,'usage: fix_singwind.x inputbufr outputbufr'
         stop
      endif
      call GetArg( 1_4, inputfile)
      call GetArg( 2_4, outputfile)

      open(unit=luin,file=trim(inputfile),form='unformatted')
      open(unit=luout,file=trim(outputfile),form='unformatted')
      call openbf(luin,'IN ',luin)
      call openbf(luout,'OUT',luin)

      psubset = ''
      n = 0

      do while (ireadmg(luin,subset,idate).eq. 0)

         if (subset .ne. psubset ) then
            if (psubset .eq. 'ADPUPA') call closmg(luout)
!           if (psubset .ne. '') print *,
!    &           'subset ',psubset,' ',n,' records'
!           print *, 'processing subset ', subset
            psubset = subset
            n = 0
         endif

         if (subset .ne. 'ADPUPA') then
!
! For non-ADPUPA data types, copy entire message buffer to output file
!
            call copymg(luin,luout)
            n = n + 1
            cycle
         else
         
            call openmb(luout,subset,idate)

            do while ( ireadsb(luin) .eq. 0 )

!
! For ADPUPA, copy individual reports from input message buffer to
!   output message buffer
!
               call ufbcpy(luin, luout)

!     Check if Singapore wind report with QM=14 above 100 mb
!     fill in array with new value to write out, if so
!     (provided pressure QM is good and data passes gross check)

               call ufbint(luin,hdr,2,1,klev,hdstr)

               if (sid .eq. '48698   ' .and. hdr(2) .gt. 200) then
!                 print *,'found Singapore wind report'
                  call ufbint(luin,evn,5,255,klev,evnstr)
                  mod = .false.
                  do j = 1,klev
                     if ( evn(1,j) .le. 100. .and. evn(2,j) .lt. 4.
     &                    .and. evn(5,j) .eq. 14) then
                        if ( abs(evn(3,j)) .lt. wmax .and. 
     &                       abs(evn(4,j)) .lt. wmax ) then
                           mod = .true.
                           evn(5,j) = 2.
                        endif
                     endif
                  enddo

!
! If array was modified, write the changes to output buffer
!
                  if (mod) then
                     print *,'Modifying Singapore wind report'
                     call ufbint(luout,evn,5,klev,llev,evnstr)
                     if (llev .ne. klev) print *, 'error ',klev,llev
                     mod = .false.
                  endif
               endif
               n = n + 1
!
!  write output buffer to output file
!
               call writsb(luout)
              
            enddo
            call closmg(luout)
         endif
      enddo      
      call closbf(luout)

!     print *,'subset ',psubset,' ',n,' records '

      stop
      end program fix_singwind
