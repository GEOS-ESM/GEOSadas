!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE:  acdatad  data type for aircraft data read from diag or prepbufr
!
! !INTERFACE:
!
      module acdatad
!
! !USES:

      implicit none
!
! !DESCRIPTION:
!  This module defines observation types for handling aircraft data
!  and aircraft bias data
!
! !REVISION HISTORY:
!
!    Apr2013  Sienkiewicz   Created module
!    4Jun2013 Sienkiewicz   added prologue
!    7Mar2014 Sienkiewicz   added YYYYMM (kym) to bias structure
!
!EOP
!------------------------------------------------------------------------- 

      type :: cft_data_type
         real  :: xob             ! longitude
         real  :: yob             ! latitude
         real  :: elv             ! level
         real  :: dmn             ! delta time (hr) in assim window
         real  :: pob             ! pressure
         real  :: tob             ! temperature
         real  :: omf             ! (obs - ges)
         real  :: alrt            ! ascent/descent rate
         character (len=8) :: sid ! aircraft ID
         integer :: ityp          ! obs type
         integer :: itqm          ! temperature quality mark
         integer :: ks            ! 'sounding' index
         integer :: is            ! index within 'sounding'
      end type cft_data_type

      integer, parameter :: maxobs=300000
      integer, parameter :: maxcft=7000
!
! data type for bias file        
      type :: bias_data_type
         character (len=8) :: sid
         real :: bias
         real :: err
         integer :: nval
         integer :: kount
         integer :: kskip
         integer :: kym
      end type bias_data_type

      end module acdatad


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE:  cft_update --- update aircraft bias correction files
!
! !INTERFACE:
!
      program cft_update

! !USES:

      use acdatad
      use m_MergeSorts

      implicit none

!
! !DESCRIPTION:  Reads in conventional diag file output from GSI and uses
!                the observed-minus-forecast values for aircraft temperature 
!                data (KX=131,133) to update bias correction values for each
!                aircraft tail number.
!
!                This version calculates ascent/descent rate from consecutive
!                observations and optionally uses that information to screen
!                out observations in ascending/descending legs from use in
!                the bias calculation.
!
!
! !USAGE:        cft_update.x diag_file bias_file [outputfile] < gmao_acft_bias.parm
!
!
! !REVISION HISTORY
!
!   15Apr2013 Sienkiewicz  Initial code
!   24Apr2013 Sienkiewicz  revise to use uncorrected OmF in bias calculation
!   15May2013 Sienkiewicz  New more modular version - determine range of
!                            obs with same tail number and pass to subroutine
!                            to calculate bias correction.  Add constraint
!                            for level-flight obs for use in bias calculation
!   29May2013 Sienkiewicz  Namelist input to control parameters for screening
!                            allow to run like original version (all good obs) 
!                            or to screen for level flight obs
!   30May2012 Sienkiewicz  Write elevation in optional output file in terms of kilo-ft
!                            (i.e. ~aircraft flight levels)
!    4Jun2013 Sienkiewicz  added prologue
!   25Jun2013 Sienkiewicz  put additional fields in bias file (for testing)
!                            to track current stats, number of updates to bias
!                            and number of times since bias last updated   
!   26Jun2013 Sienkiewicz  try 'dfact' factor to scale down the bias corr with
!                            time if it is not updated
!   26Nov2013 Sienkiewicz  add restriction of minimum date = 1991040100 for bias corr.
!   11Dec2013 Sienkiewicz  Revised defaults to match namelist values
!    7Mar2014 Sienkiewicz  Modified output to match format from Yanqiu's code, add index
!                             column, extra predictor columns, and column with YYYYMM of
!                             last update for bias.  Results same as before, just format change.
!   28Jul2016 Sienkiewicz  Switch variance and count columns so they are in the same order
!                             as in the NCEP bias files. (Probably will otherwise get confused.)
!   12Jul2018 Sienkiewicz  Reduce the amount of printout for default case.  Move current 'verbose'
!                          printout to 'verbose2' and add 'verbose' condition for printing tail
!                          numbers added or merged into the bias coefficient file.
!   23Aug2018 Sienkiewicz  Try adding option to start bias calculation with zero bias (& large error)
!EOP
!------------------------------------------------------------------------- 

      integer,parameter :: nhdr=8
      integer,parameter :: ntime=8

      real, parameter :: m2kft = 39.37/12000.

      CHARACTER(len=200) diagfile,outputfile,biasfile
      character(len=3) dtype
      CHARACTER(len=8)  SUBSET,cstid

      character(8),allocatable, dimension(:):: cdiagbuf
      real,allocatable,dimension(:,:)::rdiagbuf

      character(len=11) c_nrlqm
      character(len=8) sid
      integer  idate, lui,luo,lub
      type (cft_data_type) :: adata(maxobs)
      integer isrt(maxobs),idex(maxobs),n,m,i1,i2

      type (bias_data_type) :: bdata(maxcft)
      type (bias_data_type) :: cdata(maxcft)
      type (bias_data_type) :: ddata(maxcft)
      integer jdex(maxcft), kdex(maxcft)

      integer argc,iargc
      integer nchar, nreal, nobs, mype
      integer ndat
      integer i,ios
      integer ipre, ip, ks, is, im1
      integer itstart,itend
      integer idum, kount, kskip

      integer nflt, nbflt, mflt
      integer idx, iym, izero

      real bias, errr
      real zero
      integer nval
      real bnew, enew
      real dum

      real plevlim               ! limiting pressure level to use in bias calc
      real adsclm                ! limiting asc/dsc rate to use in bias calc
      real dfact                 ! reduction factor for bias corr
      integer nobsmin            ! min nobs value used to toss old bias entry
      real bvarmin               ! minimum value for bias error 
      integer nminb              ! min nobs value for using bias correction
      logical docount            ! keep tally of # of times tail number
                                 ! appears and count since last updated
      integer mindate            ! minimum date for aircraft bias to be active
      logical apply_bias         ! if .false. set bias (as read by GSI) to zero
                                 !   i.e. if prior to mindate
      logical zerostart          ! initial value of bias is zero 
      real    errstart           ! set initial error (use with zerostart) default 1.

      logical lprint, verbose, verbose2

      namelist/acftbias/ plevlim, adsclm, dfact, nobsmin, bvarmin, nminb,
     &   docount, mindate, zerostart, errstart
      namelist/io_opt/ verbose, verbose2
      integer ichk, jchk, kchk

      data lui /10/,lub/11/

      ndat = 0
      luo = 6
      kdex = -1
      lprint = .false.
      verbose = .false.
      verbose2 = .false.
      dfact = 0.99              ! set '1' to be compatible with original
                                !  recommend value = 0.99
      nobsmin =  0              ! use -1 to match original, recommend > 0
      bvarmin = 0.001           ! default 0.001 to match original
      nminb = 10                ! use 1 to be compatible with original
      docount = .false.         ! default .false. , leave off count
                                !    (avoid eventual integer overflow)
      mindate = 1991040100      ! default minimum date to apply Apr 01, 1991 00z
      apply_bias= .true.        ! default is to apply the bias
      zerostart = .false.       ! default is start with first (bias,error) estimate
      errstart = 1.0

      zero = 0.0
      izero = 0

      argc = iargc()
      if (argc .lt. 2) then
         print *,'usage:  cft_update.x diag_file bias_file '
     &        // '[outputfile] < namelist '
         stop
      end if

! default values for screening
      plevlim = 600.
      adsclm = 10.

      read(5,acftbias,end=1234)
      read(5,io_opt,iostat=ios)

      go to 1235

 1234 continue
      print *,'using default values for aircraft bias namelist'

 1235 continue
      write(*,acftbias)

      call getarg(2,biasfile)
      if (argc .gt. 2) then
         luo = 20
         lprint = .true.
         call getarg(3,outputfile)
         open(unit=luo,file=outputfile,form='formatted')
      end if

      cdata%sid = 'ZZZZZZZZ'
      bdata%sid = 'YZZZZZZZ'
      ddata%bias = 0.0
      ddata%err = 0.0
      ddata%nval = 0


      open(unit=lub,file=biasfile,form='formatted',status='old')
      ios = 0
      nbflt = 0

! read in bias correction from file, apply reduction factor to
! downweight prior bias 

      do while (ios .eq. 0)
         read(lub,2010,iostat=ios) sid, idx, dum, dum, dum, 
     &        nval, idum, idum, 
     &        errr, dum, dum, 
     &        iym, bias, dum, dum, idum, kount, kskip
         if (ios .eq. 0) then
            nbflt = nbflt + 1
            cdata(nbflt)%sid = trim(sid)
            cdata(nbflt)%bias = bias               ! don't rescale bias
            cdata(nbflt)%err = errr*errr/dfact
            cdata(nbflt)%nval = nval*dfact
            cdata(nbflt)%kount = kount
            cdata(nbflt)%kskip = kskip + 1
            cdata(nbflt)%kym = iym
         end if
      end do
      print *,'Read in from biasfile ',trim(biasfile),' total of ',
     &            nbflt,' entries'
      close(lub)
      nflt = nbflt
      call IndexSet(nflt,jdex)
      call IndexSort(nflt,jdex,cdata(1:nflt)%sid,descend=.false.)

!  Read data in from diag files, select observations with kx=131 (AMDAR) or 133 (MDCRS)
!
      call getarg(1,diagfile)
      open(unit=lui,file=diagfile,form='unformatted',status='old')

      read(lui) idate

      if (idate .lt. mindate) then
         print *,'Current date ',idate,' is less than minimum date'
         print *,'Calculate bias but pass zero value to GSI'
         apply_bias = .false.
      end if

      idate = idate / 10000

      do while (ndat .lt. maxobs)
         read(lui,iostat=ios) dtype,nchar,nreal,nobs,mype
         if (ios .ne. 0) exit
         if (dtype .ne. '  t') then
            read(lui)
            cycle
         end if
         allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
         read(lui) cdiagbuf,rdiagbuf
         do i = 1,nobs
            select case (nint(rdiagbuf(1,i)))
            case (131, 133)
               ndat = ndat + 1
               if (ndat .gt. maxobs) then
                  print *,'exceeded maxobs value, ndat = ',ndat
                  exit
               end if
               adata(ndat)%xob = rdiagbuf(4,i)
               adata(ndat)%yob = rdiagbuf(3,i)
               adata(ndat)%elv = rdiagbuf(5,i)*m2kft
               adata(ndat)%dmn = rdiagbuf(8,i)*60.
               adata(ndat)%pob = rdiagbuf(6,i)
               adata(ndat)%tob = rdiagbuf(17,i)
               adata(ndat)%omf = rdiagbuf(19,i)
               adata(ndat)%sid = trim(cdiagbuf(i))
               adata(ndat)%ityp = nint(rdiagbuf(1,i))
               adata(ndat)%itqm = nint(rdiagbuf(12,i))

            case default
            end select
         end do
         deallocate(cdiagbuf,rdiagbuf)

      end do
      print *,'processed ', ndat, ' obs'

      call IndexSet(ndat,idex)
      call IndexSort(ndat,idex,adata(1:ndat)%ityp,descend=.false.)
      call IndexSort(ndat,idex,adata(1:ndat)%dmn,descend=.false.)
      call IndexSort(ndat,idex,adata(1:ndat)%sid,descend=.false.)

!     
!  at this point the obs are sorted by kx, time, and tail number
!  we can move along the index array and label flights and calculate
!  ascent descent rates and mean values

      mflt = 0
      ipre = idex(1)
      ks = 1
      is = 1
      adata(ipre)%ks = ks
      adata(ipre)%is = is
      adata(ipre)%alrt = 0.0
      itstart = 1
      i1 = 2
      do while (i1 <= ndat)
         ip = idex(i1)
         if (adata(ip)%sid .ne. adata(ipre)%sid) then
            itend = i1-1
            call process_tailno(itstart,itend,idex,adata,plevlim)
            call process_bias(itstart,itend,adata,idex,bias,errr,nval,
     &           plevlim,adsclm)
            if (nval .gt. 1) then
               mflt = mflt + 1
               bdata(mflt)%sid = adata(ipre)%sid
               bdata(mflt)%bias = bias
               bdata(mflt)%err = errr
               bdata(mflt)%nval = nval
            end if

            itstart = i1
            ks = ks + 1
            is = 0
         end if
         is = is + 1
         adata(ip)%ks = ks
         adata(ip)%is = is
         ipre = ip
         i1 = i1 + 1
      end do
      call process_tailno(itstart,ndat,idex,adata,plevlim)
      call process_bias(itstart,ndat,adata,idex,bias,errr,nval,
     &           plevlim,adsclm)
      if (nval .gt. 1) then
         mflt = mflt + 1
         bdata(mflt)%sid = adata(ipre)%sid
         bdata(mflt)%bias = bias
         bdata(mflt)%err = errr
         bdata(mflt)%nval = nval
      end if

      print *,'total flights extracted from diag file = ',mflt

      ichk = 1
      kchk = 0

      do jchk = 1,mflt

         do while(ichk .le. nbflt)
            if (cdata(jdex(ichk))%sid .ge. bdata(jchk)%sid) exit
            if (verbose2) print *,cdata(jdex(ichk))%sid, ' lt ', 
     &           bdata(jchk)%sid, ' so no match in new file'
            kchk = kchk + 1
            kdex(kchk) = jdex(ichk)
            ichk = ichk + 1
         end do

         if (ichk .le. nbflt) then

            if (cdata(jdex(ichk))%sid .eq. bdata(jchk)%sid) then

!     combine the entries
               if (verbose) print *,'combining entries for ',bdata(jchk)%sid
               i1 = jdex(ichk)
!               enew =  bdata(jchk)%err*cdata(i1)%err/
!     &              (bdata(jchk)%err+cdata(i1)%err)
               enew = 1./(1./bdata(jchk)%err + 
     &              1./cdata(i1)%err)
               bnew = (cdata(i1)%bias/cdata(i1)%err + 
     &              bdata(jchk)%bias/bdata(jchk)%err)*
     &              bdata(jchk)%err*cdata(i1)%err/
     &              (bdata(jchk)%err+cdata(i1)%err)
               cdata(i1)%bias = bnew
               cdata(i1)%err = max(enew,bvarmin)
               cdata(i1)%nval = min(5000,bdata(jchk)%nval+
     &              cdata(i1)%nval)
               cdata(i1)%kskip = 0
               cdata(i1)%kount = cdata(i1)%kount + 1
               cdata(i1)%kym = idate

               ddata(i1)%bias = bdata(jchk)%bias
               ddata(i1)%err  = bdata(jchk)%err
               ddata(i1)%nval = bdata(jchk)%nval

               kchk = kchk + 1
               kdex(kchk) = i1
               ichk = ichk + 1

            else 

!     add an entry at the end of the array and put the location
!     in the (after merge) pointer array 

               im1 = ichk - 1
               if (verbose) then
                  if (im1 .gt. 0) then 
                     print *,'insert ',bdata(jchk)%sid,' between ',
     &                cdata(jdex(im1))%sid,' and ',cdata(jdex(ichk))%sid
                  else 
                     print *,'insert ',bdata(jchk)%sid,
     &                    ' at start before ', cdata(jdex(ichk))%sid 
                  end if
               end if
               kchk = kchk + 1
               nflt = nflt + 1
               kdex(kchk) = nflt
               if (zerostart) then
                  enew = 1./(1./bdata(jchk)%err + 1./errstart)
                  bnew = (bdata(jchk)%bias/bdata(jchk)%err)*
     &                 bdata(jchk)%err*errstart/
     &                (errstart+bdata(jchk)%err)
               else
                  bnew = bdata(jchk)%bias
                  enew = bdata(jchk)%err
               end if
               cdata(nflt)%sid = bdata(jchk)%sid
               cdata(nflt)%bias = bnew
               cdata(nflt)%err = max(enew,bvarmin)
               cdata(nflt)%nval = bdata(jchk)%nval
               cdata(nflt)%kskip = 0
               cdata(nflt)%kount =  1
               cdata(nflt)%kym = idate

               ddata(nflt)%bias = bdata(jchk)%bias
               ddata(nflt)%err  = bdata(jchk)%err
               ddata(nflt)%nval = bdata(jchk)%nval

            end if
         else
!     add an entry at the end of the array and put the location
!     in the (after merge) pointer array 

            if (verbose) print *,'insert ',bdata(jchk)%sid,
     &           ' at end of array'
            kchk = kchk + 1
            nflt = nflt + 1
            kdex(kchk) = nflt
            if (zerostart) then
               enew = 1./(1./bdata(jchk)%err + 1./errstart)
               bnew = (bdata(jchk)%bias/bdata(jchk)%err)*
     &              bdata(jchk)%err*errstart/
     &              (errstart+bdata(jchk)%err)
            else
               bnew = bdata(jchk)%bias
               enew = bdata(jchk)%err
            end if
            cdata(nflt)%sid = bdata(jchk)%sid
            cdata(nflt)%bias = bnew
            cdata(nflt)%err = max(enew,bvarmin)
            cdata(nflt)%nval = bdata(jchk)%nval
            cdata(nflt)%kskip = 0
            cdata(nflt)%kount = 1
            cdata(nflt)%kym = idate

            ddata(nflt)%bias = bdata(jchk)%bias
            ddata(nflt)%err  = bdata(jchk)%err
            ddata(nflt)%nval = bdata(jchk)%nval

         end if

      end do

      if (ichk .le. nbflt) then
         do i1 = ichk,nbflt
            if (verbose2) print *,cdata(jdex(i1))%sid, 
     &           ' no match in new file'
            kchk = kchk + 1
            kdex(kchk) = jdex(i1)
         end do
      end if

      print *,'total flights in merged file:  ',nflt


      if (lprint) then
         write(luo,1999)
 1999    format('    SID           DMIN       ELEV        POB',
     & '          XOB         YOB        TOB   TQM    KX')
         do i1 = 1,ndat
            i2 = idex(i1)
            write(luo,2000) adata(i2)%sid,adata(i2)%dmn,
     &           adata(i2)%elv,adata(i2)%pob,adata(i2)%xob,
     &           adata(i2)%yob,adata(i2)%tob,
     &           adata(i2)%itqm,adata(i2)%ityp,
     &           adata(i2)%ks,adata(i2)%is,adata(i2)%alrt
         end do

      end if

      open(unit=lub,file=biasfile,form='formatted')
      idx = 0
      do i1 = 1,nflt
         i2 = kdex(i1)
         if (cdata(i2)%nval .gt. nobsmin) then
!  fill in bias value read by GSI
            if ( apply_bias .and. cdata(i2)%nval >= nminb ) then
               bias = cdata(i2)%bias 
            else
               bias = zero
            end if
            idx = idx + 1
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
            if (docount) then
               write(lub,2010) cdata(i2)%sid,idx,bias,zero,zero,
     &              cdata(i2)%nval, izero, izero, 
     &              sqrt(cdata(i2)%err), zero, zero, cdata(i2)%kym, 
     &              cdata(i2)%bias, ddata(i2)%bias, sqrt(ddata(i2)%err), 
     &              ddata(i2)%nval, cdata(i2)%kount, cdata(i2)%kskip
            else
               write(lub,2010) cdata(i2)%sid,idx,bias,zero,zero,
     &              cdata(i2)%nval, izero, izero, 
     &              sqrt(cdata(i2)%err), zero, zero, cdata(i2)%kym, 
     &              cdata(i2)%bias, ddata(i2)%bias, sqrt(ddata(i2)%err), 
     &              ddata(i2)%nval
            endif

         else if (verbose) then
            print *,'Removing old bias correction for ',cdata(i2)%sid
         end if
      end do

      stop
 2000 format(2x,a8,2x,f10.2,5(2x,f10.2),i4,3i6,f10.2)
 2010 format(1x,a8,i5,3f10.2,3i8,3f10.4,i8,2f10.2,f10.3,3i8)

      end program cft_update

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE:  process_tailno
!
! !INTERFACE:
!
      subroutine process_tailno(itstart,itend,idex,adata,plevlim)

! !USES:
      use acdatad
      implicit none

! !INPUT PARAMETERS:
!
      integer itstart            ! start value in index of flight
      integer itend              ! end value in index of flight
      integer idex(maxobs)       ! sorted index of observations
      real plevlim               ! limiting pressure level to use for fill value
!
! !INPUT/OUTPUT PARAMETERS:
      type (cft_data_type) :: adata(maxobs)  ! data structure with observations

! !DESCRIPTION:  process data with same tail number to calculate
!                ascent/descent rates 
!
! !REVISION HISTORY:
!   24Oct2013 Sienkiewicz new wrapper to separate out 'flights' from
!                          all obs with same tail number - copied from
!                          cft_prp_vv.f90
!
!EOP
!-------------------------------------------------------------------------
!
!  run through tail number array to see if delta-dhr is too large, 
!  split into new flight (no acid available in diag file)
!
      integer i
      integer ip1, is1, iptr, iprev
      real, parameter ::  dtlim = 25.
      real dt
      
      is1 = itstart
      ip1 = idex(is1)
      iprev = ip1

      do i = itstart+1,itend
         iptr = idex(i)
         dt = adata(iptr)%dmn-adata(iprev)%dmn
         if ( dt .gt. dtlim ) then
!
!  end of current flight, process and start new flight
            call process_flight(is1,i-1,idex,adata,plevlim)
            is1 = i
            ip1 = iptr
         end if
         iprev = iptr
      end do
      if (is1 <= itend) then
         call process_flight(is1,itend,idex,adata,plevlim)
      end if
      return
      end subroutine process_tailno


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE:  process_flight
!
! !INTERFACE:
!
      subroutine process_flight(itstart,itend,idex,adata,plevlim)

! !USES:
      use acdatad
      implicit none

! !INPUT PARAMETERS:
!
      integer itstart            ! start value in index of flight
      integer itend              ! end value in index of flight
      integer idex(maxobs)       ! sorted index of observations
      real plevlim               ! limiting pressure level to use for fill value
!
! !INPUT/OUTPUT PARAMETERS:
      type (cft_data_type) :: adata(maxobs)  ! data structure with observations

! !DESCRIPTION:  process data with same tail number to calculate
!                ascent/descent rates 
!
! !REVISION HISTORY:
!   15May2013 Sienkiewicz Initial code
!    4Jun2013 Sienkiewicz added prologue
!   23Oct2013 Sienkiewicz trying centered difference calculation
!   29Oct2013 Sienkiewicz add code to exclude 'bad' obs, rename to 
!                          'process_flight' (changes from cft_prp_vv.f90;
!                           note we are using dP/dt hPa/min not dZ/dt m/s)
!   30Oct2013 Sienkiewicz pass in plevlim to limit "fill" values for isolated obs
!EOP
!------------------------------------------------------------------------- 


      real, allocatable :: utime(:), ulev(:),  alr(:)
      integer, allocatable :: nattime(:)

      integer i1,ialrt, intm
      integer ier
      integer iptr
      integer nflt

      nflt = itend - itstart + 1

      if (nflt .lt. 1) then
         print *,'bad flight?',itstart,itend
         return
      end if

      allocate(utime(nflt),ulev(nflt),alr(nflt),nattime(nflt),stat=ier)
      if (ier .ne. 0) then
         print *,'error allocating arrays for ascent/descent processing',ier
         stop
      end if

      utime = 0.0
      ulev = 0.0
      alr = 0.0
      nattime = 0

      iptr = idex(itstart)
      intm = 1
      utime(intm) = adata(iptr)%dmn
      ulev(intm)  = adata(iptr)%pob
      nattime(intm) = 1
      do i1 = itstart+1,itend
         iptr=idex(i1)
         if (adata(iptr)%dmn .ne. utime(intm)) then
!
! if time is different, add new unique time to array
!
            intm = intm + 1
            utime(intm) = adata(iptr)%dmn
            ulev(intm)  = adata(iptr)%pob
            nattime(intm) = 1
         else
!
! if time is the same, combine with other reports with the same time stamp
!
            ulev(intm) = ((ulev(intm)*nattime(intm))+adata(iptr)%pob)/
     &           (nattime(intm)+1)
            nattime(intm) = nattime(intm) + 1
         end if            
      end do

! 
!  add fill value for isolated reports - if below 'plevlim' use -9999.9, if above they
!  may be isolated reports at cruise level so leave the 0.0 value
      if (intm .lt. 2) then
         if(ulev(1) .gt. plevlim) then
            alr(1) = -9999.9
         endif
      else
!
!  we now have 'intm' unique time/level pairs so we can calculate ascent/descent
!  rates for each of these times (with an "average" value for the obs with 
!  identical times)

         alr(1) = (ulev(2)-ulev(1))/(utime(2)-utime(1))
 
         do i1 = 2,intm-1
            alr(i1) = (ulev(i1+1)-ulev(i1-1))/(utime(i1+1)-utime(i1-1))
         end do
         
         alr(intm) = (ulev(intm)-ulev(intm-1))/(utime(intm)-utime(intm-1))

      end if
!  now fill in the calculated ascent/descent rate for each obs in the flight

      ialrt = 1
      do i1 = itstart,itend
         iptr = idex(i1)
         if (adata(iptr)%itqm .ne. 1) then
            adata(iptr)%alrt = -9999.9
            cycle
         end if
         do while(ialrt < intm .and. utime(ialrt) .ne. adata(iptr)%dmn)
            ialrt = ialrt + 1
         end do
         if (utime(ialrt) .ne. adata(iptr)%dmn) then
            print *,'Error, time not found in array'
            adata(iptr)%alrt = -9999.9
         else
            adata(iptr)%alrt = alr(ialrt)
         end if
      end do

      deallocate(utime, ulev, nattime, alr, stat=ier)
      if (ier .ne. 0) then
         print *,'deallocate failed, ier=',ier
      end if

      return

      end subroutine process_flight


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE:  process_bias
!
! !INTERFACE:
!
      subroutine process_bias(itstart,itend,adata,idex,bias,errr,nval,
     &     plevlim,adsclm)

! !USES:
      use acdatad
      implicit none

! !INPUT PARAMETERS:
!
      integer itstart
      integer itend
      type (cft_data_type) :: adata(maxobs)
      integer idex(maxobs)
      real plevlim
      real adsclm

! !OUTPUT PARAMETERS:
      integer nval
      real bias
      real errr

! !DESCRIPTION:  calculate bias for data from given tail number
!                  with restriction based on ascent/descent rate
!                  and pressure level
!
!
! !REVISION HISTORY:
!   15May2013 Sienkiewicz Initial code
!    4Jun2013 Sienkiewicz added prologue
!EOP
!------------------------------------------------------------------------- 


! local variables
      real(8) sum,sum2
      integer i, ii
      real omf

      sum = 0.0
      sum2 = 0.0
      nval = 0
      do i = itstart,itend
         ii = idex(i)
         if (adata(ii)%itqm .ne. 1)  cycle       ! only data that passed qc    
         if (adata(ii)%pob .ge. plevlim) cycle   ! only data higher than plevlim
         if (abs(adata(ii)%alrt) .gt. adsclm) cycle  !only level(ish) flight

         omf = adata(ii)%omf
         sum = sum + omf
         sum2 = sum2 + omf*omf
         nval = nval + 1

      end do

      if (nval .gt. 1) then
         bias = sum/float(nval)
         errr = max((sum2-sum*sum/float(nval))/
     &        float(nval*nval),0.01)
      end if

      return

      end subroutine process_bias
