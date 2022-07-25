      program flag_NP_buoy
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: flag_NP_buoy:  Flag fixed buoys with bad latitude (90N)
!
! !INTERFACE:
!
!     Usage:  flag_NP_buoy.x input_prepbufr output_prepbufr
!
! !USES:
!
      implicit NONE
!
!  link to libbfr_r4i4.a library

! !DESCRIPTION:  simple routine to modify QM to purge buoy obs
!                with bad locations
!                (derived from code to tweak OIQC QMs)
!
! !REVISION HISTORY:
!
!     18Mar2021    Meta   Initial version
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
      logical      flag         ! flag indicating whether to give report a purge mark

      integer  i,j              ! loop counters
      integer  n                ! report counter

      real*8     bmiss          !  missing data value
      
      real*8 evn(4,255)         ! wind profile from bufr file
      real*8 hdr(5)
      real*8 oes(1,255)         ! obs error array
      character*8 sid

      equivalence(hdr,sid)
      real wmax                 ! gross check parameter for wind.
     
      data hdstr  /'SID TYP T29 XOB YOB'/
      data evnstr /'POB  PQM  PPC  PRC '/

      data luin /8/, luout /9/

      bmiss = 10.e10
      wmax = 300.                !  gross check

      argc = iargc()
      if (argc .lt. 2) then
         print *,'usage: flag_NP_buoy.x inputbufr outputbufr'
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
            if (psubset .eq. 'SFCSHP') call closmg(luout)
            psubset = subset
         endif
         
         if (subset .ne. 'SFCSHP') then
!     
!     For non-SFCSHP data types, copy entire message buffer to output file
!     
            call copymg(luin,luout)
            cycle
         else
            
            call openmb(luout,subset,idate)
            
            do while ( ireadsb(luin) .eq. 0 )
               
!     
!     For SFCSHP, copy individual reports from input message buffer to
!     output message buffer
!     
               call ufbcpy(luin, luout)
               flag = .false.
               

!  if observation matches criteria, replace pressure QM with blacklist value
               call ufbint(luin,hdr,5,1,klev,hdstr)
               if ( (hdr(2) == 180 .or. hdr(2) == 280.) .and.
     &            hdr(3) == 561 .and. hdr(5) >= 90.) then
                  call ufbint(luin,evn,4,255,klev,evnstr)
                  if (klev .ne. 1) then
                     print *,'multilevel report klev=',klev
                  endif
                  do j = 1,klev
                     if ( evn(2,j) .lt. 9. ) then
                        evn(2,j) = 14.
                        evn(3,j) = 1.
                        evn(4,j) = 1.
                     endif
                  enddo
                  call ufbint(luout,evn,4,klev,llev,evnstr)
                  if (llev .ne. klev) print *, 'error ',klev,llev
                  n = n + 1
               endif
!     
!  write output buffer to output file
!     
               call writsb(luout)
            enddo
         endif
         call closmg(luout)
         
      enddo      
      call closbf(luout)

      print *,'modified ',n,' records'

      stop
      end program flag_NP_buoy
