! 2013-07-15  Sienkiewicz original version
! 2014-03-11  Sienkiewicz update to handle new format of bias file
! 2014-10-08  Sienkiewicz write 1st column from input, as before
!                         (read dummy input from other cols into 'dum1')
! 2016-09-19  Sienkiewicz revised to allow reading of NCEP (-n) format
!                          for varBC without extra columns from offline BC,
!                          and to allow reading of old MERRA2 (-m) format
! 2018-08-10  Sienkiewicz initialize all variables being written to the output
!                          file.  For 'ncep' case write bias(1) for 'abias'
!                          (although '0' would also work)

      implicit none
      
      character(8) sid,sidf
      real  dum, berr(3), bias(3), count(3), abias, bval, vval
      integer nobs, nval, nday, iskip, iym, icount(3)
      integer ios
      integer idx
      character(50) prefix
      character(80) infile
      character(60) outfil
      integer i, is, iargc, ipos
      logical ncepfmt, m2fmt
      
      prefix = 'tails'
      ncepfmt = .false.
      m2fmt = .false.
      ipos = 0
      bval = 0.0
      vval = 0.0
      nday = 0
      iskip = 0
      bias = 0.0
      berr = 0.0
      icount = 0
      iym = 201001
      
      
      is = iargc()
      if (is .lt. 1) then
         print *,'usage: tailwrite.x  [-n | -m] infile [prefix] '
         stop
      end if
      
      call getarg(1,infile)
      if ( infile == '-n') then 
         ncepfmt = .true.
         ipos = ipos + 1
         call getarg(1+ipos,infile)
      else if ( infile == '-m') then
         m2fmt = .true.
         ipos = ipos + 1
         call getarg(1+ipos,infile)
      end if
      if (is .gt. 1+ipos) call getarg(2+ipos,prefix)
      
      open(unit=10,file=infile,status='old',form='formatted')
     
      ios = 0
      do while (ios .eq. 0 )
         if (ncepfmt) then
            read(10,*,iostat=ios) sid, idx, bias, count, berr, 
     &           iym
            icount = nint(count)
            abias = bias(1)
            nval = icount(1)
         else if (m2fmt) then
            read(10,2000,iostat=ios) sid, bias(1), berr(1), icount(1), 
     &           abias, bval, vval, nval, nday, iskip
         else
            read(10,2010,iostat=ios) sid, idx, bias, icount, berr, 
     &           iym, abias, bval, vval, nval, nday, iskip
         endif

         if (ios /= 0) then
            if (ios > 0) write(6,*) 'error reading ',trim(infile)
            exit
         end if
         sidf = sid
         is = len(sid)
         do i = 1,is
            if (sid(i:i) .eq. '/') sidf(i:i) = '0'
         end do
         outfil= trim(prefix) // '/' // trim(sidf)
         open(unit=20,file=outfil,position='APPEND')
         write(20,2020) infile, sid, bias, icount, berr, 
     &     iym, abias, bval, vval, nval, nday, iskip
         close(20)
      end do

      stop


 2000 format(a8,f10.2,f10.4,i8,2f10.2,f10.3,3i8)
 2010 format(1x,a8,i5,3f10.2,3i8,3f10.4,i8,2f10.2,f10.3,3i8)
 2020 format(a40,':',a8,3f10.6,3i8,3f10.6,i8,2f10.2,f10.3,3i8)
 
       end
!  "ncep-like" format for offline bias correction
!  columns being written in bias file  1-12 are read by GSI, 13-18 only in external program
!  1 - tail number
!  2 - index for tail ID
!  3 - bias value to be used by GSI (zerored if too 
!      little data used or estimate is too old)
!  4 - zero  (unused second predictor coefficient)
!  5 - zero  (unused third  predictor coefficient)
!  6 - running count of obs
!  7 - zero  (unused count for second predictor if asc/dsc bias)
!  8 - zero  (unused count for third predictor if asc/dsc bias)
!  9 - error value for calculated bias
! 10 - zero  (unused slot for second predictor)
! 11 - zero  (unused slot for third  predictor)
! 12 - YYYYMM time indicator 
! 13 - actual bias value calculated
! 14 - mean OmF for tail number for current synoptic time
! 15 - std.dev OmF for tail number for current synoptic time
! 16 - count for tail number for current synoptic time (optional)
! 17 - number of days tail number has appeared
! 18 - count of days missing for tail number

!  "ncep-like" format for rewriting variational bias files 
!  columns being written in bias file  1-12 are read by GSI, 13-18 only in external program
!  1 - tail number
!  2 - index for tail ID
!  3 - first predictor coefficient (mean)
!  4 - second predictor coefficient (usually VV)
!  5 - third  predictor coefficient (usually VV^2)
!  6 - count of obs from current synoptic time
!  7 - count for second predictor if asc/dsc bias or zero
!  8 - count for third predictor if asc/dsc bias or zero
!  9 - 'bkg' error for mean predictor
! 10 - 'bkg' error for second predictor
! 11 - 'bkg' error for third  predictor
! 12 - YYYYMM time indicator 
! 13 - copy of first predictor
! 14 - zero
! 15 - zero
! 16 - copy of obs count
! 17 - zero
! 18 - zero

! output format
!
!         write(20,2020) infile, sid, bias, icount, berr, 
!     &     iym, abias, bval, vval, nval, nday, iskip
!
!  1 - fname  file name (with date/time stamp)
!  2 - tailno tail number
!  3 - bias   first predictor coefficient
!  4 - bias2  second predictor coefficient 
!  5 - bias3  third predictor coefficient
!  6 - mcnt   current or cumulative obs count from mean calculation
!  7 - acnt   count for ascending predictor or zero
!  8 - dcnt   count for descending predictor or zero
!  9 - var    'bkg' error for mean predictor
! 10 - var2   'bkg' error for second predictor
! 11 - var3   'bkg' error for third predictor
! 12 - iym    YYYYMM time indicator
! 13 - abias  actual bias calculated or mean predictor
! 14 - bval   mean OmF for tail number for current synoptic time
! 15 - vval   std.dev OmF for tail number for current synoptic time
! 16 - nval   count for tail number for current synoptic time
! 17 - nday   number of days tail number has appeared (optional output)
! 18 - nskip  count of days missing for tail number (optional output)
!
