      program fix_tamdar
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: fix_tamdar:  fix KX, SID for TAMDAR data 
!
! !INTERFACE:
!
!     Usage:  fix_tamdar.x input_prepbufr output_prepbufr
!
! !USES:
!
      implicit NONE
!
!  link to libbfr_r4i4.a library

! !DESCRIPTION:  simple routine to modify station ID and KX value
!                for TAMDAR winds (AIRCFT winds with missing station ID
!                between 2008091606 and 2011111918)
!                (derived from code to tweak OIQC QMs)
!
! !REVISION HISTORY:
!
!     20Sep2013    Meta   Initial version
!
!EOP
!-----------------------------------------------------------------------

      integer luin, luout       !  unit numbers

      integer argc
      integer(4) iargc
      integer      ireadmg
      integer      ireadsb
      integer      ibfms

      character(len=120) inputfile
      character(len=120) outputfile

      character(len=8) subset       ! name of current BUFR subset
      character(len=8) psubset      ! name of previous BUFR subset

      integer      idate        ! synoptic date/time YYYYMMDD

      integer      iret         ! subroutine return code

      integer      klev, llev, mlev   ! no. of levels in report

      character*30  hdstr
      
      logical      mod          ! flag indicating whether to modify report

      integer  i,j              ! loop counters
      integer  n                ! report counter
      integer  itamdar          ! counter for TAMDAR obs

      real(8) hdr(5)
      character(len=8) sid, nsid
      real(8) pxob, pyob, pdhr

      equivalence(hdr,sid)
      real wmax                 ! gross check parameter for wind.
     
      data hdstr  /'SID TYP XOB YOB DHR'/

      data luin /8/, luout /9/


      argc = iargc()
      if (argc .lt. 2) then
         print *,'usage: fix_tamdar.x inputbufr outputbufr'
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
      itamdar = 0

      do while (ireadmg(luin,subset,idate).eq. 0)

         if (subset .ne. psubset ) then
            if (psubset .eq. 'AIRCFT') call closmg(luout)
            psubset = subset
         endif
         
         if (subset .ne. 'AIRCFT') then
!     
!     For non-AIRCFT data types, copy entire message buffer to output file
!     
            call copymg(luin,luout)
            cycle
         else
            
            call openmb(luout,subset,idate)
            
            do while ( ireadsb(luin) .eq. 0 )
               
!     
!     For AIRCFT, copy individual reports from input message buffer to
!     output message buffer
!     
               call ufbcpy(luin, luout)
               
               call ufbint(luin,hdr,5,1,klev,hdstr)
               
               n = n + 1

               if (ibfms(hdr(1)) .eq. 1) then
                  if (hdr(2) .gt. 200) then
                     hdr(2) = 234.
                     if (hdr(3).ne.pxob .or. hdr(4).ne.pyob
     &                    .or. hdr(5) .ne. pdhr) then
                        itamdar = itamdar + 1
                        write(nsid,'(a2,i6.6)') 'TM',itamdar
                     end if
                  else
                     hdr(2) = 134.
                     itamdar = itamdar + 1
                     write(nsid,'(a2,i6.6)') 'TM',itamdar
                     pxob = hdr(3)
                     pyob = hdr(4)
                     pdhr = hdr(5)
                  end if
                  sid = nsid
                  call ufbint(luout,hdr(1:2),2,klev,llev,'SID TYP')
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

      print *,'converted ',itamdar,' TAMDAR records'

      stop
      end program fix_tamdar
