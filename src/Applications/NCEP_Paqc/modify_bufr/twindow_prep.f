      program twindow
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: twindow:  apply time window to GOES wind in prepbufr files
!
! !INTERFACE:
!
!     Usage:  twindow.x input_bufr output_bufr
!
! !USES:
!
      implicit NONE
!
!  link to libNCEP_w3_r4i4.a  and libNCEP_bufr_r4i4.a libraries

! !DESCRIPTION:  simple routine to copy data from prepBUFR file to
!                a second file, excluding GOES sounder satwinds
!                outside of a specified time window
!
! !REVISION HISTORY:
!
!     15Jan2015    Meta   Initial version
!
!EOP
!-----------------------------------------------------------------------

      integer luin, luout       !  unit numbers
      
      integer argc
      integer(4) iargc
      integer      ireadsb
      
      character(len=120) inputfile
      character(len=120) outputfile
      
      character(len=8)  subset       ! name of current BUFR subset

      integer      idate        ! synoptic date/time YYYYMMDD
      real         tmin, tmax   ! window limits

      integer      iret         ! subroutine return code

      integer      klev, llev   ! no. of levels in report
      integer j

      logical copyit


      integer no,ni,ne


      real(8) hdr(2)

      integer ityp


      data luin /8/, luout /9/

      no=0
      ni=0
      ne=0


      argc = iargc()
      if (argc .lt. 2) then
         print *,'usage: twindow.x  inputbufr outputbufr'
         stop
      endif
      
      call GetArg( 1_4, inputfile)
      call GetArg( 2_4, outputfile)
      
      open(unit=luin,file=trim(inputfile),form='unformatted')
      open(unit=luout,file=trim(outputfile),form='unformatted')
      call openbf(luin,'IN ',luin)
      call openbf(luout,'OUT',luin)

      call datelen(10)
      call cmpmsg('Y')

! get center date from first message in BUFR file
      call readmg(luin,subset,idate,iret)

! set window max = center time, window min to center time minus 60 minutes
      tmax = 0     ! dhr=0 is max value to keep
      tmin = -1

      do while(iret .eq. 0)
                           
         if (trim(subset).ne.'SATWND') then

! if not satwind, just copy the message

            no = no + 1
            call copymg(luin,luout)
         else

! check if KX is 245 or 246 if not copy subset
! if so, get time and compare to time window, copy obs inside time window
!  to output file
         
            call openmb(luout,subset,idate)

            do while ( ireadsb(luin) .eq. 0 )

               call ufbint(luin,hdr,2,1,klev,'TYP DHR')
               ityp = int(hdr(1))

               copyit = (ityp .ne. 245 .and. ityp .ne. 246)

               copyit = copyit .or. 
     &              (hdr(2) .ge. tmin .and. hdr(2) .le. tmax) 

               if (copyit) then
                  ni=ni+1
                  call ufbcpy(luin, luout)
                  call writsb(luout)
               else
                  ne=ne+1
               end if
            enddo

            call closmg(luout)

         end if

         call readmg(luin,subset,idate,iret)

      end do

      call closbf(luin)
      call closbf(luout)

      stop
      end program twindow
