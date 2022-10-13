      program fix_ascat
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: fix_ascat:  change QM and add obs error to ASCAT winds
!
! !INTERFACE:
!
!     Usage:  fix_ascat.x input_prepbufr output_prepbufr
!
! !USES:
!
      implicit NONE
!
!  link to libbfr_r4i4.a library

! !DESCRIPTION:  simple routine to modify QM and add obs error (3.5m/s)
!                for ASCAT winds
!                (derived from code to tweak OIQC QMs)
!
! !REVISION HISTORY:
!
!     22Apr2011    Meta   Initial version
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

      integer      klev, llev, mlev   ! no. of levels in report

      character*30  evnstr,hdstr, oestr
      
      logical      mod          ! flag indicating whether to modify report

      integer  i,j              ! loop counters
      integer  n                ! report counter

      real*8     bmiss          !  missing data value
      
      real*8 evn(5,255)         ! wind profile from bufr file
      real*8 hdr(2)
      real*8 oes(1,255)         ! obs error array
      character*8 sid

      equivalence(hdr,sid)
      real wmax                 ! gross check parameter for wind.
     
      data hdstr  /'SID TYP'/
      data evnstr /'UOB VOB WQM WPC WRC'/
      data oestr  /'WOE'/
!     data oestr /'WOE UFC VFC'/

      data luin /8/, luout /9/

      bmiss = 10.e10
      wmax = 300.                !  gross check

      argc = iargc()
      if (argc .lt. 2) then
         print *,'usage: fix_ascat.x inputbufr outputbufr'
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

      oes = 3.50             ! obs error = 3.5 m/s

      do while (ireadmg(luin,subset,idate).eq. 0)

         if (subset .ne. psubset ) then
            if (psubset .eq. 'ASCATW') call closmg(luout)
            psubset = subset
            n = 0
         endif
         
         if (subset .ne. 'ASCATW') then
!     
!     For non-ASCATW data types, copy entire message buffer to output file
!     
            call copymg(luin,luout)
            n = n + 1
            cycle
         else
            
            call openmb(luout,subset,idate)
            
            do while ( ireadsb(luin) .eq. 0 )
               
!     
!     For ASCATW, copy individual reports from input message buffer to
!     output message buffer
!     
               call ufbcpy(luin, luout)
               
               call ufbint(luin,hdr,2,1,klev,hdstr)
               if (hdr(2) .ne. 290) then
                  print *,'Error not ASCATW data'
                  cycle
               endif
               call ufbint(luin,evn,5,255,klev,evnstr)
               if (klev .ne. 1) then
                  print *,'multilevel report klev=',klev
               endif
               mod = .false.
               do j = 1,klev
                  if ( evn(3,j) .eq. 9. ) then
                     if ( abs(evn(1,j)) .lt. wmax .and. 
     &                    abs(evn(2,j)) .lt. wmax ) then
                        mod = .true.
                        evn(3,j) = 2.
                        evn(4,j) = 1.
                     endif
                  endif
               enddo
               
!     
!     If array was modified, write the changes to output buffer
!     
               if (mod) then
                  call ufbint(luout,oes,1,klev,mlev,oestr)
                  call ufbint(luout,evn,5,klev,llev,evnstr)
                  if (mlev .ne. klev) print *, 'error ',klev,mlev
                  if (llev .ne. klev) print *, 'error ',klev,llev
                  mod = .false.
               endif
               n = n + 1
!     
!  write output buffer to output file
!     
               call writsb(luout)
            enddo
         endif
         call closmg(luout)
         
      enddo      
      call closbf(luout)

      print *,'wrote ',n,' records'

      stop
      end program fix_ascat
