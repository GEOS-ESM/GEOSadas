      program twindow
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: twindow:  apply time window to satwind files
!
! !INTERFACE:
!
!     Usage:  twindow.x [-rc rcfile] input_bufr output_bufr
!
! !USES:
!
      use m_inpak90                         ! rc input handler

      implicit NONE
!
!  link to libNCEP_w3_r4i4.a  and libNCEP_bufr_r4i4.a libraries

! !DESCRIPTION:  simple routine to copy data from BUFR file to
!                a second file, excluding GOES sounder satwinds
!                outside of a specified time window
!
! !REVISION HISTORY:
!
!     15Jan2015    Meta   Initial version
!     16Jan2015    Meta   exclude AVHRR and SWIR data, replace 'if'
!                         with CASE to allow only specified (EU,JMA,MODIS)
!                         wind types to be copied
!     20May2015    Meta   take out NC005066- EUMETSAT WV, for now, to
!                         reduce obs count for old runs
!     21May2015    Meta   New version reads resource file to determine
!                         obs filtering.
!     22May2015    Meta   deallocate table arrays at end, add I90_Release
!     13Dec2017    Meta   Process GOES-R format and convert
!                         to format that M2 can read (use_flg = 2).
!                         Continue processing in if-block until subset 
!                         read in from input file changes from prior one
!      3Jun2019    Meta   Add screening by satellite ID for GOES-R+
!                         (for GOES 5_12 only checks if satid==259 to
!                         assign subtype 15, can't screen other satIDs)
!     18Sep2019    Meta   Add additional OPENMB calls after READMG -
!                         while 'subset' is the same, 'idate' may
!                         have changed so new message may be required.
!      4Jan2021    Meta   Add code to read new format EUMETSAT winds and
!                         rewrite in old format used by MERRA2
!
!EOP
!-----------------------------------------------------------------------

      integer luin, luout       !  unit numbers
      
      integer argc
      integer(4) iargc
      integer      ireadsb
      
      character(len=120) inputfile
      character(len=120) outputfile
      character(len=120) rcfile
      
      character(len=8)  subset       ! name of BUFR subset read in
      character(len=8)  osubset      ! prior subset name

      character(len=8),allocatable :: wsubsets(:),nsubsets(:)
      integer,allocatable          :: use_flg(:)
      integer,allocatable          :: dtmin(:)
      integer,allocatable          :: dtmax(:)
      integer,allocatable          :: ikeep(:,:)
      integer,allocatable          :: nkeep(:)
      integer nsattype

      integer      idate        ! synoptic date/time YYYYMMDD
      integer      cdate        ! center date *hopefully 1st in the file*
      integer      jdate(5)     ! use for call to w3fs21
      integer      itctr        ! center date (min since 1 jan 78)
      integer      itmin, itmax ! window limits (min since 1 jan 78)
      integer      obtime       ! obs time (min since 1 jan 78)

      integer      iret         ! subroutine return code

      integer      klev, llev   ! no. of levels in report
      integer i, j
      integer narg
      integer idx

      integer rc

      integer no,ni,ne

      real(8) getbmiss, bmiss

      real(8) time(5)
      character(len=80) timestr
      character(len=8) unknown

      data timestr/'YEAR MNTH DAYS HOUR MINU'/


      data luin /8/, luout /9/

! Variables for GOES-16 conversion
      real(8) hdrdat(13), obsdat(4)
      real(8) qcdat(2,12),  amvivr(2,2)
      real(8) amvqic(2,4) 
      real pct1, qm
      logical keep, window_t

      integer ilev, jlev, iqlev

      character(len=70) :: obstr,obstr0,hdrtr,hdrtr0
      character(len=50) :: qcstr
      data hdrtr0 / 'SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SWCM SAZA OGCE SCCF SWQM' /
      data hdrtr  / 'SAID CLAT  CLON  YEAR MNTH DAYS HOUR MINU SWCM SAZA OGCE SCCF SWQM' /
      data obstr0 / 'EHAM PRLC WDIR WSPD' /
      data obstr  / 'HAMD PRLC WDIR WSPD' /
      data qcstr  / 'GNAP PCCF' /

      no=0
      ni=0
      ne=0
      unknown = ''
      narg = 0
      bmiss = getbmiss()


      argc = iargc()
      if (argc .lt. 2) then
         call usage()
         stop
      endif

      rcfile = 'twindow.rc'
      
      call GetArg( 1_4, inputfile)
      if (inputfile == '-rc') then
         if (argc .lt. 4) then
            call usage()
            stop
         end if
         narg = narg + 2
         call GetArg(2_4,rcfile)
         call GetArg(3_4,inputfile)
      end if
      call GetArg( 2_4+narg, outputfile)

      rc = 0
      call read_table(rcfile,wsubsets,use_flg,dtmin,dtmax,
     &     nkeep,ikeep,nsubsets,nsattype,rc)

      if (rc /= 0) then
         print *,'twindow: could not read config table, exiting.'
         stop
      end if
      
      open(unit=luin,file=trim(inputfile),form='unformatted')
      open(unit=luout,file=trim(outputfile),form='unformatted')
      call openbf(luin,'IN ',luin)
      call openbf(luout,'OUT',luin)

      call datelen(10)
      call cmpmsg('Y')

! get center date from first message in BUFR file
      call readmg(luin,subset,idate,iret)

      if (iret /= 0) then
         print *,'twindow, error reading ',trim(inputfile)
         stop
      end if

      cdate = idate
      jdate(5) = 0
      jdate(1) = cdate/1000000
      jdate(2) = mod(cdate,1000000)/10000
      jdate(3) = mod(cdate,10000)/100
      jdate(4) = mod(cdate,100)
      call w3fs21(jdate,itctr)

      idx = 0
      main: do while(iret .eq. 0)

         idx = find_subset(subset,wsubsets,nsattype,idx)

         if (idx == 0) then
            if (subset /= unknown) then
               print *,'twindow: error, ',subset,
     &              ' not found in subset table'
               unknown = subset
            end if
            do while (subset == unknown .and. iret == 0)
               call readmg(luin,subset,idate,iret)
            end do
            cycle
         end if
         
         if (use_flg(idx) == 1) then

            itmin = dtmin(idx) + itctr
            itmax = dtmax(idx) + itctr

! case of windowed obs (NESDIS GOES hourly wind):
! get time and compare to time window, copy obs inside time window
!  to output file, process all messages that match this subset

            osubset = subset
         
            call openmb(luout,subset,idate)
            
            flg1: do while ( iret .eq. 0 )
            
               do while ( ireadsb(luin) .eq. 0 )
                  
                  call ufbint(luin,time,5,1,klev,timestr)
                  do j = 1,5
                     jdate(j) = int(time(j))
                  end do
                  call w3fs21(jdate,obtime)
                  if (obtime .ge. itmin .and. obtime .le. itmax) then
                     ni=ni+1
                     call openmb(luout,subset,idate)
                     call ufbcpy(luin, luout)
                     call writsb(luout)
                  else
                     ne=ne+1
                  end if
               enddo

               call readmg(luin, subset, idate, iret)

               if (iret /= 0 .or. subset /= osubset) exit flg1
               call openmb(luout,subset,idate)

            end do flg1
            
            call closmg(luout)
            
         else if ( use_flg(idx) == 2 ) then

            itmin = dtmin(idx) + itctr
            itmax = dtmax(idx) + itctr

! case of new GOES format that needs to be rewritten for MERRA2
! get time and compare to time window, qc obs inside time window
!  and rewrite in old GOES format to output file - process all
!  the messages that match this subset

!  We are lucky in that the defs for the old GOES wind formats
!  are contained in the BUFR dictionary used for the new winds

            osubset = subset
            
            call openmb(luout,nsubsets(idx),idate)

            flg2: do while ( iret .eq. 0 )
               
               readsb: do while ( ireadsb(luin) .eq. 0 )
                  
                  call ufbint(luin,time,5,1,klev,timestr)
                  do j = 1,5
                     jdate(j) = int(time(j))
                  end do
                  call w3fs21(jdate,obtime)
                  if (obtime .ge. itmin .and. obtime .le. itmax) then
                     ni=ni+1
                     call openmb(luout,nsubsets(idx),idate)

! Read in data from new format file
                     hdrdat = bmiss
                     obsdat = bmiss
                     qcdat = bmiss
                     qm = 2
      
                     call ufbint(luin,hdrdat,13,1,ilev,hdrtr0)
                     call ufbint(luin,obsdat,4,1,ilev,obstr0)
                     call ufbrep(luin,qcdat,2,12,iqlev,qcstr)
                     call ufbrep(luin,amvivr,2,2,ilev,'TCOV CVWD')

                     if ( nkeep(idx) > 0 ) then      ! screen satIDs
                        keep = .false.
                        do j = 1,nkeep(idx)
                           if (nint(hdrdat(1)) .eq. ikeep(j,idx)) then
                              keep = .true.
                              exit
                           end if
                        end do

                        if ( .not. keep ) cycle readsb     ! skip if satID not found

                     end if
                       
                     
! using QM=14 so read_satwnd will skip these data
                     pct1 = amvivr(2,1)
                     if (pct1 < 0.04) qm=14
                     if (pct1 > 0.50) qm=14

                     hdrdat(13) = qm
                     
! write out a record with data in old format, just including
! data read in by MERRA2 code

                     call ufbint(luout,hdrdat,13,1,ilev,hdrtr)
                     call ufbint(luout,obsdat,4,1,ilev,obstr)
                     call ufbrep(luout,qcdat,2,iqlev,jlev,qcstr)
                     
                     call writsb(luout)

                  else   ! data not in time window
                     ne=ne+1
                  end if

               end do readsb

               call readmg(luin, subset, idate, iret)

               if (iret /= 0 .or. subset /= osubset) exit flg2
               call openmb(luout,nsubsets(idx),idate)
               
            end do flg2
               
            call closmg(luout)
               
         else if ( use_flg(idx) == 3 ) then

            if (dtmin(idx) == 0.0 .and. dtmax(idx) == 0.0) then
               window_t = .false.
               itmin = -180
               itmax = 180
            else
               itmin = dtmin(idx) + itctr
               itmax = dtmax(idx) + itctr
               window_t = .true.
            endif

! case of new EUMETSAT format that needs to be rewritten for MERRA2
! get time and compare to time window, qc obs inside time window
!  and rewrite in old EUMETSAT format to output file - process all
!  the messages that match this subset.  Allow for case without
!  time windowing.

!  We are lucky in that the defs for the old wind formats
!  are contained in the BUFR dictionary used for the new winds

            osubset = subset

            call openmb(luout,nsubsets(idx),idate)

            flg3: do while ( iret .eq. 0 )

               readsbe: do while ( ireadsb(luin) .eq. 0 )

                  call ufbint(luin,time,5,1,klev,timestr)
                  do j = 1,5
                     jdate(j) = int(time(j))
                  end do
                  call w3fs21(jdate,obtime)
                  if ((obtime .ge. itmin .and. obtime .le. itmax) .or.
     &                 .not. window_t)  then
                     ni=ni+1
                     call openmb(luout,nsubsets(idx),idate)

! Read in data from new format file
                     hdrdat = bmiss
                     obsdat = bmiss
                     qcdat = bmiss
                     qm = 2
                     
                     call ufbint(luin,hdrdat,13,1,ilev,hdrtr0)
                     call ufbint(luin,obsdat,4,1,ilev,obstr0)
!
!  skipping read of 'pct1' information since it is not used in QC of EU winds (yet)
!  read other quality information
                     call ufbseq(luin,amvqic,2,4,iret, 'AMVQIC') ! AMVQIC:: GNAPS PCCF
!                    qifn = amvqic(2,2) ! QI w/ fcst does not exist in this BUFR
!                    ee = amvqic(2,4) ! NOTE: GOES-R's ee is in [m/s]
                     qcdat(1,4) = 2.0
                     qcdat(2,4) = amvqic(2,2) ! qifn
                     qcdat(1,5) = 3.0
                     qcdat(2,5) = amvqic(2,4) ! "ee"
                     if ( amvqic(2,2) < 85.0 ) then
                        qm = 15
                     end if
                     
                     call ufbint(luout,hdrdat,13,1,ilev,hdrtr)
                     call ufbint(luout,obsdat,4,1,ilev,obstr)
                     call ufbrep(luout,qcdat,2,12,jlev,qcstr)

                     call writsb(luout)

                  else          ! data not in time window
                     ne = ne + 1
                  end if
               end do readsbe
               call readmg(luin, subset, idate, iret)

               if (iret /= 0 .or. subset /= osubset) exit flg3
               call openmb(luout,nsubsets(idx),idate)

            end do flg3

            call closmg(luout)
!
         else if ( use_flg(idx) == 0 ) then
            
! case of nonwindowed old format (EUMETSAT, JMA, MODIS) - just copy the winds as they are
! for all of the messages matching this subset
            osubset = subset
               
            flg0: do while ( iret .eq. 0 )

               no = no + 1
               call copymg(luin,luout)
               call readmg(luin,subset,idate,iret)
               if (iret /= 0 .or. subset /= osubset) exit flg0
               
            end do flg0
            
         else         ! other cases - just skip the messages matching this subset

            osubset = subset

            flgX: do while ( iret .eq. 0 )

               call readmg(luin, subset, idate, iret)
               if (iret /= 0 .or. subset /= osubset) exit flgX

            end do flgx

         end if

         
      end do main
      
      call closbf(luin)
      call closbf(luout)

      deallocate(wsubsets,use_flg,dtmin,dtmax,stat=iret)
      
      stop
      
      contains
        
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 610.1, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  usage -  print the usage instructions
!
! !INTERFACE:

      subroutine usage()
!
!EOP
!-------------------------------------------------------------------------
      print *,'usage: twindow.x  [-rc rcfile] inputbufr outputbufr'
      stop
      end subroutine usage
      
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 610.1, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  read_table -- read the configuration table
!
!
! !INTERFACE:

      subroutine read_table(tablefile,wsubsets,use_flg,dtmin,dtmax,
     &     nkeep, ikeep, nsubsets,nsattype,rc)

! !INPUT PARAMETERS:
!
      character(len=*),intent(in)              :: tablefile
      
! !OUTPUT PARAMETERS:
!
      character(len=8),allocatable,intent(out) :: wsubsets(:)
      integer,allocatable,intent(out)          :: use_flg(:)
      integer,allocatable,intent(out)          :: dtmin(:)
      integer,allocatable,intent(out)          :: dtmax(:)
      integer,allocatable,intent(out)          :: nkeep(:)
      integer,allocatable,intent(out)          :: ikeep(:,:)
      character(len=8),allocatable,intent(out) :: nsubsets(:)
      integer,intent(out)                      :: nsattype
      integer,intent(out)                      :: rc

! !DESCRIPTION:
!
!  load resource file 'tablefile' and read configuration for satellite
!  wind data processing
!
! !REVISION HISTORY:
!
!  21May2015  Meta  New routine
!  22May2015  Meta  add I90_Release to free memory
!  13Dec2017  Meta  Changes for GOES-16 processing, add column for
!                     new subset name
!           
!
!EOP
!-------------------------------------------------------------------------

      integer iret
      integer i, ii, j
      
      character(len=8) str
      rc = 0
      
      call i90_LoadF (tablefile, iret)

      if (iret .ne. 0) then
         print * ,'twindow: failed to load table file ',
     &        trim(tablefile), ' rc = ',iret
         rc = -1
         return
      end if

      call i90_label('action_table::', iret)

      if (iret .ne. 0) then
         print *,'twindow: action table read failed, rc = ', iret
         rc = -1
         return
      end if
!
!  count the number of lines in the table, then allocate space 

      iret = 0
      nsattype = 0
      call i90_gline(iret)
      do while (iret == 0 )
         nsattype = nsattype + 1
         call i90_gline(iret)
      end do

      allocate(wsubsets(nsattype),use_flg(nsattype),
     &     dtmin(nsattype), dtmax(nsattype), nsubsets(nsattype),
     &     nkeep(nsattype), ikeep(4,nsattype), stat=iret)

      if (iret /= 0) then
         print *,'twindow: unable to allocate space'
         rc = -1
         return
      end if

      use_flg = 0
      dtmin   = 0
      dtmax   = 0

      if (iret /= 0) then
         print *,'twindow: error allocating memory for arrays'
         rc = -1
         return
      end if

      call i90_label('action_table::', iret)
      
      do i = 1, nsattype
         call i90_gline(iret)
         if (iret /= 0) then
            print *,'twindow: error reading line ',i
            rc = -1
            return
         end if
         call i90_gtoken(str,iret)
         if (iret /= 0) then
            print *,'twindow: Error reading subset name, line ',i
            rc = -1
            return
         end if
         wsubsets(i) = str
         ii = i90_gint(iret)
         if (iret /= 0) then
            print *,'twindow: Error reading use_flag for subset ',
     &           wsubsets(i)
            rc = -1
            return
         end if
         use_flg(i) = ii
!     
! read time window parameters if use_flg == 1, 2, or 3
         if (use_flg(i) >= 1 .and. use_flg(i) <= 3) then
            ii = i90_gint(iret)
            if (iret /= 0) then
               print *,'twindow: error reading time window ',
     &              'for subset ', wsubsets(i)
               rc = -1
               return
            end if
            dtmin(i) = ii
            ii = i90_gint(iret)
            if (iret /= 0) then
               print *,'twindow: error reading time window ',
     &              'for subset ', wsubsets(i)
               rc = -1
               return
            end if
            dtmax(i) = ii
!
! read subset name for converting new types to old
            if (use_flg(i) == 2 .or. use_flg(i) == 3)  then  
               call i90_gtoken(str,iret)
               if (iret /= 0) then
                  print *,'twindow: Error reading new subset name,',
     &                 ' line ',i
                  rc = -1
                  return
               end if
               nsubsets(i) = str
! check for additional fields
               iret = 0
               j = 0
               do while( iret == 0 .and. j < 4)
                  ii = i90_gint(iret)
                  if (iret /= 0) exit
                  j = j + 1
                  ikeep(j,i) = ii
               end do
               nkeep(i) = j
            end if
         end if
         
      end do
      
      call I90_Release(iret)
      
      return
      
      end subroutine read_table
      
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 610.1, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  find_subset - search for subset name in table
!
!
! !INTERFACE:

      integer function find_subset(subset,wsubsets,nsattype,idx)

! !INPUT PARAMETERS:
!
      integer nsattype
      character(len=8) wsubsets(nsattype)
      character(len=8) subset
      integer idx


! !DESCRIPTION:
!
!  Look for matching subset name in table.  If 'idx' is set nonzero,
!  check wsubsets(idx) first for a match.
!  
!
! !REVISION HISTORY:
!
!  21May2015  Meta  New routine
!
!
!EOP
!-------------------------------------------------------------------------
      integer i

      if (idx .ne. 0) then
         if (wsubsets(idx) == subset) then
            find_subset = idx
            return
         end if
      end if
      
      do i = 1,nsattype
         if (wsubsets(i) == subset) then
            find_subset = i
            return
         end if
      end do
      
      find_subset = 0
      return
      
      end function find_subset

      end program twindow
