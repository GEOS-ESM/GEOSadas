      program change_said
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: change_said:  change satID and write modified BUFR
!
! !INTERFACE:
!
!     Usage:  change_said.x input_bufr output_bufr
!
! !USES:
!
      implicit NONE
!
!  link to libbfr_r4i4.a library

! !DESCRIPTION:  simple routine to copy data from BUFR file and
!                modify the SAID mnemonic
!                (derived from code to tweak OIQC QMs)
!
! !REVISION HISTORY:
!
!     27Mar2013    Meta   Initial version
!     14May2013    Meta   change to modify specific sat ID
!      5Nov2013    Meta   add option to turn on BUFR compression
!
!EOP
!-----------------------------------------------------------------------

      integer luin, luout       !  unit numbers
      
      integer argc
      integer(4) iargc,iarg
      integer      ireadmg
      integer      ireadsb
      
      character*120 inputfile
      character*120 outputfile
      character*8 newid, oldid
      character*1 cf
      
      character*8  subset       ! name of current BUFR subset

      integer      idate        ! synoptic date/time YYYYMMDD

      integer      iret         ! subroutine return code

      integer      klev, llev   ! no. of levels in report

      
 
      integer  m,n              ! report counter

      real       roldid         ! old sat ID to change
      real       rnewid         ! new sat ID to use
      
      real(8) hdr

      data luin /8/, luout /9/

      argc = iargc()
      if (argc .lt. 4) then
         print *,'usage: change_said.x [-c] oldID newID ',
     &          'inputbufr outputbufr'
         stop
      endif

      cf = 'N'
      iarg = 0

      call GetArg( 1_4, oldID)
      if (argc .gt. 4 .and. oldID .eq. '-c') then
        iarg = 1
        cf = 'Y'
        call GetArg( 1_4+iarg, oldID)
      end if
      call GetArg( 2_4+iarg, newID)
      call GetArg( 3_4+iarg, inputfile)
      call GetArg( 4_4+iarg, outputfile)
      
      open(unit=luin,file=trim(inputfile),form='unformatted')
      open(unit=luout,file=trim(outputfile),form='unformatted')
      call openbf(luin,'IN ',luin)
      call openbf(luout,'OUT',luin)

      call cmpmsg(cf)
      
      m = 0
      n = 0
      read(oldID,*) roldid
      read(newID,*) rnewid
      print *,'Will change old satID=',roldid,' to newid=',rnewid
      
      do while (ireadmg(luin,subset,idate).eq. 0)
         
         call openmb(luout,subset,idate)
         
         do while ( ireadsb(luin) .eq. 0 )
            
!     
!     Copy individual reports from input message buffer to
!     output message buffer
!     
            call ufbcpy(luin, luout)
            
! read in satID and write out modified satID
               
            call ufbint(luin,hdr,1,1,klev,'SAID')
            if (klev .eq. 1 .and. hdr .eq. roldid) then
               hdr = rnewid
               call ufbint(luout,hdr,1,1,llev,'SAID')
               m = m + 1
            end if
            
            n = n + 1
!     
!  write output buffer to output file
!     
            call writsb(luout)
         enddo
         
         call closmg(luout)
         
      enddo      
      call closbf(luout)

      print *,'modified ',m,' of ',n,' records'

      stop
      end program change_said
