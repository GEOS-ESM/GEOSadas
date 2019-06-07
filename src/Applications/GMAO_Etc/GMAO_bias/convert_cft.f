
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE:  convert_cft --- convert MERRA2 acftbias to newer format
!
! !INTERFACE:
!
      program convert_cft

! !USES:

      implicit none

!
! !DESCRIPTION:  Reads aircraft bias files and writes out in current (5_13)
!                bias file format.  Note, input values of '-0.0' are converted
!                to '0.0'
!
! !USAGE:        convert_cft.x [-t YYYYMM ] input_bias_file [outputfile] 
!
!
! !REVISION HISTORY
!
!   11Mar2015 Sienkiewicz  Initial code
!   28Jul2016 Sienkiewicz  Switched columns for count and error to be
!                          consistent with NCEP GSI format.
!   02Nov2016 Sienkiewicz  fix small error with output (trade zero,izero)
!
!EOP
!------------------------------------------------------------------------- 

      character(len=180) biasfile, outputfile
      character(len=6) yyyymm         ! year-month string
      character(len=8) sid
      integer iargc, argc, ia, ios, ifmt
      integer lub, luo
      integer izero, idum
      integer idx                     ! index value for tail
      integer iym                     ! YYYYMM for most recent correction
      integer nval                    ! cumuluative aircaft obs count (max=5000)
      integer nobs                    ! obs count for synoptic time
      real abias                      ! bias corr to be used by GSI
      real errr                       ! background error for bias coeff
      real bias                       ! current bias correction
      real sbias                      ! sample bias for synoptic time
      real serr                       ! error of bias estimate for synoptic time
      real dum                        ! dummy value
      real zero
      
      logical merra2
      data lub /10/

      luo = 6
      ia = 0
      zero = 0.0
      izero = 0
      merra2 = .false.
      iym = 111111        !default if YYYYMM not set

      argc = iargc()
      if (argc .lt. 1) call usage

      ia = 0

      call getarg(1, biasfile)

      if (trim(biasfile) .eq. '-t') then
         if (argc .lt. 3) call usage
         call getarg(2,yyyymm)
         call getarg(3,biasfile)
         ia = 2
         read(yyyymm,'(i6.6)') iym
      end if
      
      if (argc .gt. 1+ia) then
         luo = 20
         call getarg(2+ia,outputfile)
         open(unit=luo,file=outputfile,form='formatted')
      end if

      open(unit=lub,file=biasfile,form='formatted',status='old')
      ios = 0

! determine the format of the input file - try to read the first record
      read(lub,2010,iostat=ios) sid, idx, abias, dum, dum, nval, 
     &     idum, idum, errr, dum, dum, iym, bias, 
     &     sbias, serr, nobs
      if (ios .ne. 0) then 
         rewind lub
         read(lub,2000,iostat=ios) sid, abias, errr, nval, bias,
     &      sbias, serr, nobs
         if (ios /= 0) then
            print *,'Error reading input file ',trim(biasfile)
            stop
         end if
         merra2 = .true.
         idx = 1
      end if
      

! read in bias correction from old file, write in new format
!
!  columns being written  1-12 are read by GSI, 13-18 only in external program
!  1 - tail number
!  2 - index for tail ID
!  3 - bias value to be used by GSI (zerored if too 
!      little data used or estimate is too old)
!  4 - zero  (unused second predictor coefficient)
!  5 - zero  (unused third  predictor coefficient)
!  6 - running count of obs
!  7 - zero  (unused count for second predictor)
!  8 - zero  (unused count for third predictor)
!  9 - error value for calculated bias
! 10 - zero  (unused slot for second predictor)
! 11 - zero  (unused slot for third  predictor)
! 12 - YYYYMM time indicator 
! 13 - actual bias value calculated
! 14 - mean OmF for tail number for current synoptic time
! 15 - std.dev OmF for tail number for current synoptic time
! 16 - count for tail number for current synoptic time
! (optional)
! 17 - number of days tail number has appeared
! 18 - count of days missing for tail number

      do while (ios .eq. 0)
         write(luo,2010,iostat=ios) sid, idx, abias, zero, zero, nval, 
     &        izero, izero, errr, zero, zero, iym, bias, 
     &        sbias, serr, nobs
         
         if (merra2) then
            read(lub,2000,iostat=ios) sid, abias, errr, nval, bias,
     &           sbias, serr, nobs
            idx = idx + 1
         else
            read(lub,2010,iostat=ios) sid, idx, abias, dum, dum, nval, 
     &           idum, idum, errr, dum, dum, iym, bias, 
     &           sbias, serr, nobs
         end if
      end do
      
      stop
 2000 format(a8,f10.2,f10.4,i8,2f10.2,f10.3,3i8)
 2010 format(1x,a8,i5,3f10.2,3i8,3f10.4,i8,2f10.2,f10.3,3i8)
      
      end program convert_cft
      subroutine usage()
        
      print *,'usage:  convert_cft.x [-t YYYYMM ] ',
     &    'input_bias_file [outputfile] '
        
      stop
      end subroutine usage
