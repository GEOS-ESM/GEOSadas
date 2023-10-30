      program mlsh5bfrT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: mlsh5bfrT:  write MLS HDF5 temperature in prepBUFR format
!
! !INTERFACE:
!
!     Usage:  mlsh5txtT.x [-f] [-v] [-d yyyymmdd] [-p prefix] h5filename 
!
!       -f             optional: force write regardless of screening
!       -v             optional: flag for verbose output
!       -d yyyymmdd    optional: date to process
!       -p prefix      optional: filename prefix  (default: MLSt)
!       h5filename     name of input HDF5 file
!
!  Output file name has format:   (prefix).yyyymmdd.tHHz.blk
!
! !USES:
!
      use hdf5
      use readmlsh5
      use timefix_mod
      use m_pbutil

      implicit none

!
! !DESCRIPTION:
!
!  Read MLS HDF5 temperature data files and write out text format.
!  Input files contain one day of retrievals, from
!  00:00:00 to 23:59:59.99 UTC.  Output files are separated into 6-hr 
!  segments centered on 00, 05, 12, 18.  For the 00UTC data, the program
!  will append to a previously existing text file containing the retrievals
!  after 21UTC from the previous day.
!
!  If executed with the '-d' flag, the program tries to read data from
!  the specified date from the input file.  Otherwise the program uses
!  date information within the file to determine the date to use.  
!
!  Screening QC based on info from v2.2 Quality document.
!
! !REVISION HISTORY:
!  19 Oct 2006   Meta    Original ozone conversion routine
!   9 Jan 2007   Meta    Adapted program for temperature data
!   8 Nov 2007   Meta    Modify for v2.2 data files, changes from
!                          ozone processing (-v, -f and time handling)
!                          add prologues
!  15 Nov 2007   Meta    Text routine modifed to write prepBUFR,
!                          including modified BUFR table for EOSMLS
!  07 Feb 2008   Meta    Added screening out of pressure levels above
!                         'plvmin' - obs get QM of 9 (if not already bad)
!  19 Feb 2008   Meta    Add command line flag -P to set top level for
!                         'plvmin' and set default 'plvmin' to zero
!  19 Apr 2012   JJJ     Read MLS v3 temperature, and
!                          set typ = 315. 
!  11 Jun 2012   JJJ     Modified date determination ( tcheck ),
!                          see JJJ x1.
!  12 Jun 2012   JJJ     Add screening out of pressure levels below (larger than)
!                          'plvmax' - obs get QM of 9 (if not already bad)
!  03 Sep 2012   JJJ     reset the type # (315 => 311), because 315 is set for
!                           AMSU-A brightness temperature. 
!  25 Apr 2013   j.jin   flag profiles has zero procision (<0.1) in addition to negative
!                           precisions. These zero errors make GSI crash. 
!                
!EOP
!-----------------------------------------------------------------------


      character*250 filename,argv
      character*20 wanted
      character*50 prefix, plvstr
      character*80 outfile
      character*8 ymdstr

      integer(HID_T) fid          ! file identifier

      integer, parameter :: maxtimes = 10000
      integer, parameter :: maxlevs = 100
      integer, parameter :: lvmin = 8       !  261 mb level
      integer, parameter :: lvmax = 49      !  49 is  0.001 mb

      integer nlevs        ! number of levels
      integer ntimes       ! number of times

      integer ier, jer          ! error return code

      real, parameter :: plvmindef = 0.0
      real, parameter :: plvmaxdef = 5.0
      real  plvmin, plvmax

! variables for hdf5 read
      real(8) time(maxtimes), tcheck
      real    latitude(maxtimes)
      real    longitude(maxtimes)
      real    quality(maxtimes)
      real    convergence(maxtimes)
      real    pres(maxlevs)
      real    precision(maxlevs,maxtimes)
      real    value(maxlevs,maxtimes)
      integer istatus(maxtimes)

! variable for BUFR file writing
      real  pob(42),tob(42),tqm(42),tprec(42), qmt, qmret
      real  rstat, xob, yob, dhr, typ
      integer ib, idate
      character*8 stnid, subset

      integer jtm, llv
!      integer mflg
      real  tk
      real  std
      real  sec

      integer iyr,mon,iday, itoday(8), inow(8)
      integer isthr, ihr
      integer isnd
      integer id1, id2
      real rinc(5)

      integer nrec(5),nr

      real(8) tai93_0z, hrmax, hrcnt, dayend, dt

      integer iout                         ! output unit number
      character*89 fmt

      integer argc, iargc, iarg, i
      logical ffound

      logical verbose, notforce, ex

      nrec = 0
      wanted = 'Temperature'          ! name of the swath to read in
      prefix = 'MLSt.'                ! default output filename prefix
      iout = 10
      fmt='(i5,4i3,f6.2,i7,i5,f10.4,f11.4,f16.7,i7,i5,g16.7,g15.7,f6.3)'
      subset = 'EOSMLS'
      !typ = 304.                     ! 304 as set of o3lev. JJJ, 4/19/12.
      !typ = 311.                     ! MLS temperature. JJJ x0 4/19/2012.
      typ = 311.                      ! Changed to 304, 4/24/2013.
      call datelen(10)

      itoday = 0
      ffound = .false.
      verbose = .false.
      notforce = .true.
      plvmin = plvmindef
      plvmax = plvmaxdef
      argc = iargc()
      if (argc < 1 .or. argc > 6) then
         call usage()
      endif
      iarg = 0
      do while (iarg < argc)
         iarg = iarg + 1
         call getarg( iarg, argv )
         if (index(argv,'-d') > 0) then
            if ( iarg+1 > argc ) call usage()
            iarg = iarg + 1
            call getarg( iarg,ymdstr )
            read(ymdstr,'(i4,i2,i2)',iostat=ier) iyr, mon, iday
            itoday(1) = iyr
            itoday(2) = mon
            itoday(3) = iday
         else if (index( argv, '-p') > 0) then
            if ( iarg+1 > argc ) call usage()
            iarg = iarg + 1
            call getarg( iarg, prefix )
         else if (index( argv, '-P') > 0) then
            if ( iarg+1 > argc ) call usage()
            iarg = iarg + 1
            call getarg( iarg, plvstr )
            read(plvstr,*) plvmin
         else if (index(argv, '-v') > 0) then
            verbose = .true.
         else if (index(argv, '-f') > 0) then
            notforce = .false.
         else
            if (ffound) call usage()
            filename = argv
            ffound = .true.
         endif
      end do
       
      if ( .not. ffound ) call usage()

!  initialize HDF interface
      call h5open_f(ier)
      if (ier .lt. 0) then
         print *,'Problem initializing hdf5.'
         stop
      endif

! open the HDF5 file

      call h5fopen_f(filename,H5F_ACC_RDONLY_F,fid,ier)

      if (ier .ne. 0) then
         print *,'error reading file ',trim(filename)
         stop
      else
         print *,'Successfully opened file ',trim(filename)
      endif

      call getfilespecs(fid,iyr,mon,iday,tai93_0z,ier)

      if (ier .eq. 0 .and. tai93_0z .gt. 0.1) then
! fill in year, month, day and check if they match (any) requested date
         print *,'Filename:  ',trim(filename)
         print *,'Year,month,day:  ',iyr,mon,iday
         print *,'TAI at 0z:  ',tai93_0z
         if (itoday(1) == 0) then
            itoday(1) = iyr
            itoday(2) = mon
            itoday(3) = iday
         else if ( itoday(1) /= iyr .or. itoday(2) /= mon 
     &           .or. itoday(3) /= iday) then
            print *,'Requested date: ',itoday(1:3)
            print *,'File date:      ',iyr,mon,iday
            print *,'Date mismatch, stopping program.'
            call h5fclose_f(fid,jer)
            call h5close_f(jer)
            stop
         endif
      else if (itoday(1) /=0 ) then
         call ymd2tai(itoday(1),itoday(2),itoday(3),0,0,0.0,
     &          tai93_0z,ier)
         print *,'Filename:  ',trim(filename)
         print *,'Requested Year,month,day:  ',(itoday(i),i=1,3)
         print *,'Requested TAI at 0z:  ',tai93_0z
      endif

!      if (ier .ne. 0) stop

      call rdmlsh5(fid, wanted, maxtimes, maxlevs, time, 
     &     latitude, longitude, quality, convergence, pres, 
     &     precision, value, istatus, ntimes, nlevs, ier)

      call h5fclose_f(fid,ier)

      call h5close_f(ier)

      if (ier /= 0) then         ! problem reading the data
         print *,'error reading data from file ',trim(filename)
         stop
      else
         print *,'Successful read of obs data from ',trim(filename)
      endif

      if (itoday(1) == 0 ) then     ! metadata missing, date from data
         ! JJJ x1, 11 JUN 2012
         ! Because a few "time" data are set to be -999.99 (e.g., 2004d228),
         !  it produces wrong time to average the fist and the last values.
         !  Now use the mean of the huge and the tine values (both are
         !  positive values).
         !
         ! tcheck = 0.5*(time(1)+time(ntimes))
           tcheck = 0.5*(huge(time)+tiny(time))
         ! JJJ x1 end.  

         if (verbose) print *,'Check TAI time ',tcheck
         call tai2ymd(tcheck,iyr,mon,iday,id1,id2,sec,ier)
         if (verbose) print *,'Year,month,day: ',iyr,mon,iday
         call ymd2tai(iyr,mon,iday,0,0,0.0,tai93_0z,ier)
         itoday(1) = iyr
         itoday(2) = mon
         itoday(3) = iday
      endif

      dayend = tai93_0z+86400.
      isthr = 1
      isnd = 0
      
      do ihr = 0,24,6

         nr = ihr/6+1
         print *,'Start processing ihr = ', ihr
         if (ihr .lt. 24) then
            write(outfile,'(a,i4.4,i2.2,i2.2,''.t'',i2.2,''z.blk'')')
     &           trim(prefix),itoday(1:3),ihr
            idate = (((itoday(1)*100)+itoday(2))*100+itoday(3))*100+ihr
         else
            call w3movdat( (/1.,0.,0.,0.,0./),itoday,inow)
            write(outfile,'(a,i4.4,i2.2,i2.2,''.t'',i2.2,''z.blk'')')
     &           trim(prefix),inow(1),inow(2),inow(3),0
            idate = (((inow(1)*100)+inow(2))*100+inow(3))*100
         endif


c$$$         if (ihr .eq. 0) then
c$$$            open(iout, file=outfile, form='formatted',
c$$$     &           position='append')
c$$$         else
c$$$            open(iout, file=outfile, form='formatted')
c$$$         endif

         if (ihr .eq. 0) then
            inquire(file=outfile,exist=ex)
            if (ex) then
               print *,'Append to output file ',trim(outfile)
               call init_bufr(outfile,append=.true.)
            else
               print *,'Create new output file ',trim(outfile)
               call init_bufr(outfile,tablefile='mls_prepbufr_table',
     &            append=.false.)
            endif
         else
            print *,'Create new output file ',trim(outfile)
            call init_bufr(outfile,tablefile='mls_prepbufr_table')
         endif

         hrmax = (ihr + 3) * 3600.
         hrcnt = ihr * 3600. + tai93_0z

! Process data for this time window, exit when end time is reached.

         do jtm = isthr, ntimes

            if (time(jtm) .lt. tai93_0z .or. 
     &           time(jtm) .gt. dayend) then
               print *,'Skip invalid time for ',jtm, time(jtm)
               cycle
            endif

            dt = time(jtm) - tai93_0z
            if (dt .gt. hrmax) then
               isthr = jtm
               print *,'Start time ',ihr+6,' at index ',isthr
               call end_bufr()
               exit
            endif

            rinc(4) = dt
            call w3movdat(rinc, itoday, inow)
            sec = inow(7) + 1.e-3 * inow(8)
            qmret = 0.0

!  skip retrievals where istatus is an odd number 
            if (mod(istatus(jtm),2) .ne. 0) then
               if (notforce) then
                  cycle
               else
                  qmret = 13.
               endif
            endif
            rstat = istatus(jtm)

!  Quality field:        quality < 0.65     do not use
!
            if (quality(jtm) .lt. 0.65 ) then
               if (notforce) then
                  cycle
               else
                  qmret = 14
               endif
            endif
!            mflg = int(quality(jtm) * 100.)

!  Convergence field:  Only use if convergence < 1.2

            if ( convergence(jtm) .ge. 1.2) then
               if (notforce) then
                  cycle
               else
                  qmret = 12.
               endif
            endif

            isnd = isnd + 1

            ib = 0

!  cycle through the levels of this retrieval
            do llv = lvmin, lvmax

! do not use data for pressure > or = 147 hPa if status is 32 (due to cloud)
          if ( llv .le. 10 ) then
            if (jtm .le. ntimes-1 ) then 
            if (  istatus(jtm+1) .eq. 32 ) then
               if (notforce) then
                  cycle
               else
                  qmret = 13.
               endif
            endif
            endif
            if (jtm .le. ntimes-2 ) then
            if (  istatus(jtm+2) .eq. 32 ) then
               if (notforce) then
                  cycle
               else
                  qmret = 13.
               endif
            endif
            endif
          endif

c$$$ add here any profile based screening (low level screening, for instance)
c$$$
               qmt = qmret

! mark high levels (p<plvmin) with QM '9' if otherwise 'good'
               if (pres(llv) < plvmin .and. qmret .eq. 0.) then
                  qmt = 9.
               else if (pres(llv) > plvmax .and. qmret .eq. 0.) then
                  qmt = 9.
               endif

               if (precision(llv,jtm) .lt. 0.1) then
                  print *,'Neg precision at lev ',llv, 
     &                 ' profile ',jtm
                  qmt = 15.
                  if (notforce) cycle
               endif

               tk = value(llv, jtm) 
               std = precision(llv,jtm) 

c$$$               write(iout,fmt) inow(1:3),inow(5:6),sec,isnd,llv,
c$$$     &              latitude(jtm),longitude(jtm),tk,istatus(jtm),
c$$$     &              mflg,std,pres(llv)

               ib = ib + 1
               pob(ib) = pres(llv)
               tob(ib) = tk - 273.15
               tprec(ib) = std
               if(tprec(ib) < 0.1) write(*,*) llv,ib,pres(llv),tprec(ib)
               tqm(ib) = max(qmt, 2.0)

               
            end do         ! loop over levels of retrieval

            if (ib .gt. 0) then

               dhr = (time(jtm)-hrcnt) / 3600.
               yob = latitude(jtm)
               xob = longitude(jtm)
               if (xob .lt. 0.) xob = xob + 360.

               write(stnid,'(''ML'',i6.6)') isnd
               nrec(nr) = nrec(nr) + 1

               call write_bfr(stnid, xob, yob, dhr, typ, ib,
     &              rstat, quality(jtm), convergence(jtm),
     &              pob, tob, tqm, tprec, subset, idate)

            endif

         end do            ! loop over times in file

      end do

      call end_bufr()

      print *,'Total obs at synoptic times: ',nrec

      stop
      contains
      
      subroutine usage()
      print *, 'Usage: mlsh5bfrT.x [-f] [-d yyyymmdd] [-p prefix] \   '
      print *, '                h5filename                             '
      print *, '  -f             optional: force write w/o quality chk '
      print *, '  -d yyyymmdd    optional: date to process             '
      print *, '  -p prefix      optional: filename prefix             '
      print *, '  h5filename     name of input HDF5 file               '
      stop
      
      end subroutine usage

      subroutine write_bfr(stnid, xob, yob, dhr, typ, ib,
     &              rstat, qual, conv,
     &              pob, tob, tqm, tprec, subset, idate)

      integer idate, ib, l
      character(len=8) stnid, subset
      real xob, yob, dhr, typ, rstat, qual, conv
      real pob(ib), tob(ib), tqm(ib), tprec(ib)

      real(8), dimension(8) ::    hdr    !  observation header
      real(8)               ::    rid
      real(8), dimension(8,255) :: pobs, tobs

      integer, parameter :: MXBLVL = 255 ! max no. of report levels allowed
      integer(i_bfr), parameter :: iarr = 8    ! size of bfr arrays
      integer(i_bfr), parameter :: i1   = 1    ! single level
      integer(i_bfr), parameter :: ilv = MXBLVL  ! multiple level
      integer(i_bfr)  ibdate
      integer(i_bfr)  ilevs
      integer(i_bfr)  iret

      character(len=40) hdstr, pobstr, tobstr
      data hdstr /'SID XOB YOB DHR TYP MLST MLSQ MLSC'/
      data pobstr /'POB PQM PPC PRC CAT'/
      data tobstr /'TOB TQM TPC TRC MLSPT'/

      hdr(1) = transfer(stnid, rid)
      hdr(2) = xob
      hdr(3) = yob
      hdr(4) = dhr
      hdr(5) = typ
      hdr(6) = rstat
      hdr(7) = qual
      hdr(8) = conv

      pobs = missing
      tobs = missing

      do l = 1,ib
         pobs(1,l) = pob(l)
         pobs(2,l) = tqm(l)    ! use same as T
         pobs(3,l) = 1.
         pobs(4,l) = 1.
         pobs(5,l) = 2.
         tobs(1,l) = tob(l)
         tobs(2,l) = tqm(l)
         tobs(3,l) = 1.
         tobs(4,l) = 1.
         tobs(5,l) = tprec(l)
      end do

      ibdate = idate
      ilevs = ib

      call openmb(lu_b, subset, ibdate)
      call ufbint(lu_b, hdr,    iarr, i1   , iret, hdstr)
      call ufbint(lu_b, pobs,   iarr, ilevs, iret, pobstr)
      call ufbint(lu_b, tobs,   iarr, ilevs, iret, tobstr)

      call writsb(lu_b)

      return


      end subroutine write_bfr


      end program mlsh5bfrT


