      program fix_buoy
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: fix_buoy:  screen for drifting buoys
!
! !INTERFACE:
!
!     Usage:  fix_buoy.x input_prepbufr output_prepbufr
!
! !USES:
!
      implicit NONE
!
!  link to libbfr_r4i4.a library

! !DESCRIPTION:  simple routine to check TYP and T29 & station ID
!                to find drifting buoys and change TYP to 199/299
!                (derived from code to tweak OIQC QMs)
!
! !REVISION HISTORY:
!
!     27Oct2020    Meta   Initial version
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

      character*30  hdstr
      
      logical      mod          ! flag indicating whether to modify report

      integer  i,j              ! loop counters
      integer  n                ! report counter
      integer iwmo              ! WMO number
      integer ios

      real*8 hdr(3)
      character*8 sid

      equivalence(hdr,sid)
     
      data hdstr  /'SID TYP T29'/

      data luin /8/, luout /9/


      argc = iargc()
      if (argc .lt. 2) then
         print *,'usage: fix_buoy.x inputbufr outputbufr'
         stop
      endif
      call GetArg( 1_4, inputfile)
      call GetArg( 2_4, outputfile)

      open(unit=luin,file=trim(inputfile),form='unformatted')
      open(unit=luout,file=trim(outputfile),form='unformatted')
      call openbf(luin,'IN ',luin)
      call openbf(luout,'OUT',luin)
      call datelen(10)

      psubset = ''
      n = 0

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
               
               call ufbint(luin,hdr,3,1,klev,hdstr)

               if ( nint(hdr(2)) .eq. 180 .or.
     &              nint(hdr(2)) .eq. 280) then
                  if ( nint(hdr(3)) .eq. 562 .or. 
     &                 nint(hdr(3)) .eq. 564) then
                     
                     read(sid,*,iostat=ios) iwmo
                     if (ios == 0 .and. iwmo > 0) then
                        if(mod(iwmo,1000) >=500) then
                           hdr(2) = hdr(2) + 19
                           call ufbint(luout,hdr,3,klev,mlev,hdstr)
                           n = n + 1
                        end if
                     end if
                  end if
               end if
               
!     
!  write output buffer to output file
!     
               call writsb(luout)
            enddo
         endif
         call closmg(luout)
         
      enddo      
      call closbf(luout)

      print *,'modified ',n,' observation records'

      stop
      end program fix_buoy
