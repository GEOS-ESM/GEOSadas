c$$$  Subprogram Documentation Block
c   BEST VIEWED WITH 94-CHARACTER WIDTH WINDOW
c
c Subprogram: output_acqc_noprof 
c   Programmer: D. Keyser       Org: NP22       Date: 2015-12-09
c
c Abstract: Reads an input, pre-PREPACQC PREPBUFR file and matches the subsets within to the
c   "merged" reports contained within the arrays output by the NRL aircraft QC subroutine
c   acftobs_qc. Calls subroutine tranQCflags to translate the QC information (for each
c   variable: pressure, altitude, temperature and moisture for the mass piece; and pressure,
c   altitude and wind for the wind piece) from NRL standards (c_qc array) to their NCEP
c   counterparts and to establish event reason codes for each variable. All of this QC
c   information is then encoded as event stacks in the output PREPBUFR file which will be
c   identical to the input PREPBUFR file except for the new events added by this program and
c   aircraft reports that are removed (possibly) for being outside the requested time window
c   or geographical domain).
c
c Program History Log:
c 2010-11-15  S. Bender  -- Original Author
c 2012-05-08  D. Keyser  -- Prepared for operational implementation
c 2012-11-20  J. Woollen -- Initial port to WCOSS
c 2013-02-07  D. Keyser  -- If the maximum number of merged reports that can be processed
c                           ("max_reps") is exceeded when updating reports in PREPBUFR file
c                           with QC changes, program will no longer stop with r.c. 31, as
c                           though there is an indexing error, instead all original reports
c                           above "max_reps" will be written out without any QC and a message
c                           will be printed to stdout (a diagnostic will have already been
c                           sent to the production joblog file in this case when reports were
c                           first read in by subroutine INPUT_ACQC)
c 2013-02-07  D. Keyser  -- Final changes to run on WCOSS: use formatted print statements
c                           where previously unformatted print was > 80 characters
c 2014-03-06  D. Keyser  -- Moved BUFRLIB routine OPENMB call to after time window and
c                           geographic domain checks to prevent creation of an empty, but
c                           open, BUFR message (type AIRCAR) in (rare) cases where absolutely
c                           no aircraft reports pass these checks (would cause a BUFRLIB
c                           abort due to previous message being open when attempting to copy
c                           first non-aircraft message from input to output PREPBUFR file
c 2013-10-07  Sienkiewicz -- Initialize some uninitialized variables for 'gfortran' compile
c 2015-03-16  D. Keyser  -- Fixed a bug which, for cases where the maximum number of merged
c                           reports that can be processed ("max_reps") is exceeded, prevented
c                           any original reports above "max_reps" from being written out
c                           (without any QC).
c 2015-12-09  D. Keyser  -- 
c                    - Variables holding latitude and longitude data (including input
c                      arguments "alat" and "alon") now double precision. XOB and YOB in
c                      PREPBUFR file now scaled to 10**5 (was 10**2) to handle new v7 AMDAR
c                      and MDCRS reports which have this higher precision.
c                      BENEFIT: Retains exact precison here. Improves QC processing.
c                         - The format for all print statements containing latitude and longitude
c                           changed to print to 5 decimal places.
c
c Usage: call output_acqc_noprof(inlun,outlun,nrpts4QC_pre,max_reps,
c                                bmiss,alat,alon,ht_ft,idt,c_qc,
c                                trad,l_otw,l_nhonly,
c                                ncep_qm_p,ncep_rc_p,
c                                ncep_qm_z,ncep_rc_z,
c                                ncep_qm_t,ncep_rc_t,
c                                ncep_qm_q,ncep_rc_q,
c                                ncep_qm_w,ncep_rc_w,
c                                ncep_rej,
c                                nrlacqc_pc)
c
c   Input argument list:
c     inlun        - Unit number for the input pre-PREPACQC PREPBUFR file containing all data
c                    (separate mass/wind pieces)
c     outlun       - Unit number for the output PREPBUFR file containing all data plus now
c                    with NRLACQC events (separate mass/wind pieces)
c     nrpts4QC_pre - Number of reports in the "merged" single-level aircraft report arrays
c     max_reps     - Maximum number of reports accepted by acftobs_qc
c     bmiss        - BUFRLIB missing value (set in main program)
c     alat         - Array of latitudes for the "merged" reports 
c     alon         - Array of longitudes for the "merged" reports 
c     ht_ft        - Array of altitudes for the "merged" reports
c     idt          - Array of ob-cycle times for the "merged" reports (in seconds)
c     c_qc         - Array of NRLACQC quality information (11 char. string) ("merged" reports)
c     trad         - Time window radius for outputting reports (if l_otw=T) (read in via
c                    namelist)
c     l_otw        - Logical whether or not to eliminate reports outside the time window
c                    radius (trad) (read in via namelist)
c     l_nhonly     - Logical Whether or not to eliminate reports south of 20S latitude (i.e,
c                    outside the tropics and N. Hemisphere) (read in via namelist)
c     nrlacqc_pc   - PREPBUFR program code for the NRLACQC step
c
c   Output argument list:
c     ncep_qm_p    - Array of NCEP PREPBUFR quality marks on pressure for the "merged" reports
c     ncep_rc_p    - Array of NCEP PREPBUFR reason codes on pressure for the "merged" reports
c     ncep_qm_z    - Array of NCEP PREPBUFR quality marks on altitude for the "merged" rpts
c     ncep_rc_z    - Array of NCEP PREPBUFR reason codes on altitude for the "merged" rpts
c     ncep_qm_t    - Array of NCEP PREPBUFR quality marks on temperature for the "merged" rpts
c     ncep_rc_t    - Array of NCEP PREPBUFR reason codes on temperature for the "merged" rpts
c     ncep_qm_q    - Array of NCEP PREPBUFR quality marks on moisture for the "merged" reports
c     ncep_rc_q    - Array of NCEP PREPBUFR reason codes on moisture for the "merged" reports
c     ncep_qm_w    - Array of NCEP PREPBUFR quality marks on wind for the "merged" reports
c     ncep_rc_w    - Array of NCEP PREPBUFR reason codes on wind for the "merged" reports
c     ncep_rej     - Array indicating if "merged" report is (=0) or is not (=1) to be written
c                    to output PREPBUFR file
c   Input files:
c     Unit inlun   - PREPBUFR file containing all obs, prior to any processing by this program
c
c   Output files:
c     Unit 06      - Standard output print
c     Unit outlun  - PREPBUFR file identical to input except containing NRLACQC events
c
c   Subprograms called:
c     Unique:    TRANQCFLAGS
c     Library:
c       SYSTEM:  SYSTEM
c       W3NCO:   ERREXIT    W3TAGE
c	BUFRLIB: IREADMG    COPYMG     OPENMB     IREADSB    UFBINT     UFBCPY     WRITSB
c                WRITLC     CLOSMG     IBFMS
c
c   Exit States:
c     Cond =  0 - successful run
c            31 - indexing problem encountered when trying to match QC'd data in arrays to
c                 mass and wind pieces in original PREPBUFR file 
c
c Remarks: Called by main program.
c
c Attributes:
c   Language: FORTRAN 90
c   Machine:  NCEP WCOSS
c
c$$$
      subroutine output_acqc_noprof(inlun,outlun,nrpts4QC_pre,max_reps,
     +                              bmiss,alat,alon,ht_ft,idt,c_qc,
     +                              trad,l_otw,l_nhonly,l_qmwrite,
     +                              ncep_qm_p,ncep_rc_p,
     +                              ncep_qm_z,ncep_rc_z,
     +                              ncep_qm_t,ncep_rc_t,
     +                              ncep_qm_q,ncep_rc_q,
     +                              ncep_qm_w,ncep_rc_w,
     +                              ncep_rej,
     +                              nrlacqc_pc)

      implicit none

c ------------------------------
c Parameter statements/constants
c ------------------------------
      integer    inlun              ! input unit number for pre-PREPACQC PREPBUFR file to
                                    !  which we are adding NRLACQC events
     +,          outlun             ! output unit number for post-PREPACQC PREPBUFR file
                                    !  with added NRLACQC events
     +,          max_reps           ! maximum number of input merged (mass + wind piece)
                                    !  aircraft-type reports allowed
      real       m2ft
      parameter (m2ft = 3.28084)    ! conversion factor to convert m to ft
cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
c replace above with this in event of future switch to dynamic memory allocation

calloc  integer    max_reps           ! original number of input reports obtained from
calloc                                !  first pass through to get total for array allocation
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      real*8     bmiss              ! BUFRLIB missing value (set in main program)


c ----------------------
c Declaration statements
c ----------------------

c Variables for BUFRLIB interface
c -------------------------------
      character*8  mesgtype          ! BUFR message type (e.g., 'AIRCFT  ')
      integer      mesgdate          ! date time from BUFR message (YYYYMMDDHH)

c Indices/counters 
c ----------------
      integer      i,j               ! loop indices

      integer      nrpts4QC_pre      ! original number of input merged (mass + wind piece)
                                     !  aircraft-type reports (read in from PREPBUFR file)

c Functions
c ---------
      integer      ireadmg           ! BUFRLIB - for reading messages 
     +,            ireadsb           ! BUFRLIB - for reading subsets
     +,            ibfms             ! BUFRLIB - for testing for missing

      character*11 c_qc(max_reps)    ! character QC flags output from NRL QC code
                                     !   1st char - info about reject (if ob was rejected)
                                     !   2nd char - reason why time was rejected
                                     !   3rd char - reason why latitude was rejected
                                     !   4th char - reason why longitude was rejected
                                     !   5th char - reason why pressure/atitude was rejected
                                     !   6th char - readon why temperature was rejected
                                     !   7th char - reason why wind direction was rejected
                                     !   8th char - reason why wind speed was rejected
                                     !   9th char - reason why mixing ratio was rejected
                                     !  10th char - reason for blacklisting the aircraft
                                     !  11th char - info about flight phase

      real*8       alat(max_reps)    ! latitude
     +,            alon(max_reps)    ! longitude
      real         ht_ft(max_reps)   ! altitude in feet
      integer      idt(max_reps)     ! time in seconds to anal. time (- before, + after)
      integer      ncep_qm_p(max_reps) ! NCEP PREPBUFR quality mark pressure (PQM)
     +,            ncep_rc_p(max_reps) ! NCEP PREPBUFR NRLACQC pressure event reason code(PRC)
     +,            ncep_qm_z(max_reps) ! NCEP PREPBUFR quality mark on altitude (ZQM)
     +,            ncep_rc_z(max_reps) ! NCEP PREPBUFR NRLACQC alt/hght event reason code(ZRC)
     +,            ncep_qm_t(max_reps) ! NCEP PREPBUFR quality mark on temperature (TQM)
     +,            ncep_rc_t(max_reps) ! NCEP PREPBUFR NRLACQC temperature evnt rea. code(TRC)
     +,            ncep_qm_q(max_reps) ! NCEP PREPBUFR quality mark on moisture (QQM)
     +,            ncep_rc_q(max_reps) ! NCEP PREPBUFR NRLACQC moisture reason code (QRC)
     +,            ncep_qm_w(max_reps) ! NCEP PREPBUFR quality mark on wind (WQM)
     +,            ncep_rc_w(max_reps) ! NCEP PREPBUFR NRLACQC wind event reason code (WRC)
     +,            ncep_rej(max_reps)  ! NCEP PREPBUFR rejection indicator

c Variables for reading (writing) numeric data out of (in to) BUFR files via BUFRLIB
c ----------------------------------------------------------------------------------
      real*8       arr_8(10,10)      ! array holding BUFR subset values from BUFRLIB call to
                                     !  input PREPBUFR file
     +,            dhr_corr(2)       ! array holding rehabilitated time (DHR TCOR)
     +,            yob_corr(3)       ! array holding rehabilitated latitude (YOB YCOR YORG)
     +,            xob_corr(3)       ! array holding rehabilitated longitude (XOB XCOR XORG)

      integer      nlev              ! number of report levels returned from BUFRLIB call
                                     !  (should always be 1 !)
      integer      iret              ! return code for call to BUFRLIB routine ufbint when
                                     !  writing to PREPBUFR file

c Variables for updating input PREPBUFR reports with QC results/events from NRLACQC
c ---------------------------------------------------------------------------------
      integer      ninssrd           ! number of subsets read in from the input PREPBUFR file
     +,            QCdrptsidx        ! index for report-oriented arrays that are output from
                                     !  acftobs_qc

      real*8    p_event(4)          ! array used to update a pressure event stack
     +,         z_event(4)          ! array used to update an altitude event stack 
     +,         t_event(4)          ! array used to update a temperature event stack
     +,	        q_event(4)          ! array used to update a moisture event stack
     +,	        w_event(5)          ! array used to update a wind event stack

      logical   l_eventupdate       ! T = event was added for the last PREPBUFR report read
                                    ! F = no events were added to the last PREPBUFR rpt read

      integer   input_sqn           ! sequence number of input PREPBUFR report for which we
                                    !  are attempting to add events
     +,         input_sqn_last      ! sequence number of previous PREPBUFR report for which
                                    !  we had attempted to add events
      real*8    input_alat          ! latitude of input PREPBUFR report for which we are 
                                    !  attempting to add events
     +,         input_alon          ! longitude of input PREPBUFR report for which we are
                                    !  attempting to add events
      real      input_ht_ft         ! altitude of input PREPBUFR report for which we are
                                    !  attempting to add events
     +,         input_dhr           ! ob time - cycle time in decimal hours
 
      integer   input_idt           ! ob time - cycle time in seconds of input PREPBUFR
                                    !  report for which we are attempting to add events
     +,         input_typ           ! PREPBUFR report type for input report for which we are
                                    !  attempting to add events
      logical   l_badrpt_p          ! T = pressure/altitude is bad per NRLACQC info (c_qc)
     +,         l_badrpt_z          ! T = pressure/altitude is bad per NRLACQC info (c_qc)
     +,         l_badrpt_t          ! T = temperature is bad per NRLACQC info (c_qc)
     +,         l_badrpt_q          ! T = moisture is bad per NRLACQC info (c_qc)
     +,         l_badrpt_w          ! T = wind is bad per NRLACQC info (c_qc)

      logical   l_duprpt            ! T = report is marked as a duplicate per NRLACQC info
                                    !  (c_qc(1:1)=D/d)

      integer   ipqm_topstk         ! event PQM at top of stack before adding any events 
                                    !  containing info from NRLACQC
     +,         izqm_topstk         ! event ZQM at top of stack before adding any events
                                    !  containing info from NRLACQC
     +,         itqm_topstk         ! event TQM at top of stack before adding any events
                                    !  containing info from NRLACQC
     +,	        iqqm_topstk         ! event QQM at top of stack before adding any events
                                    !  containing info from NRLACQC
     +,	        iwqm_topstk         ! event WQM at top of stack before adding any events
                                    !  containing info from NRLACQC

      integer   ipqm_nrlacqc     ! value for pressure q.m. (PQM) returned from tranQCflags
     +,         iprc_nrlacqc     ! value for pressure r.c. (PRC) returned from tranQCflags
     +,         izqm_nrlacqc     ! value for altitude q.m. (ZQM) returned from tranQCflags
     +,         izrc_nrlacqc     ! value for altitude r.c. (ZRC) returned from tranQCflags
     +,         itqm_nrlacqc     ! value for temperature q.m. (TQM) returned from tranQCflags
     +,         itrc_nrlacqc     ! value for temperature r.c. (TRC) returned from tranQCflags
     +,         iqqm_nrlacqc     ! value for moisture q.m. (QQM) returned from tranQCflags
     +,         iqrc_nrlacqc     ! value for moisture r.c. (QRC) returned from tranQCflags
     +,         iwqm_nrlacqc     ! value for wind q.m. (WQM) returned from tranQCflags
     +,         iwrc_nrlacqc     ! value for wind r.c. (WRC) returned from tranQCflags

c Event counters
c --------------
      integer   nevrd(5)
      integer   nevwrt(5)           ! number of [p,z,t,q,w] events written to output PREPBUFR
                                    !  file
      integer   nev_noupd(5)        ! number of subsets from input PREPBUFR file with no
                                    !  updated [p,z,t,q,w] event
      integer   qm_knt(5,0:15,0:15) ! count of [p,z,t,q,w] NCEP quality marks changed from i
                                    !  (input PREPBUFR value) to j (output PREPBUFR value)
                                    !  by NRLACQC
      integer   p_qm_knt_tot        ! total number of pressure QMs (and therefore events)
                                    !  added to the output PREPBUFR file
     +,         z_qm_knt_tot        ! total number of altitude QMs (and therefore events)
                                    !  added to the output PREPBUFR file
     +,         t_qm_knt_tot        ! total number of temperature QMs (and therefore events)
                                    !  added to the output PREPBUFR file
     +,         q_qm_knt_tot        ! total number of moisture QMs (and therefore events)
                                    !  added to the output PREPBUFR file
     +,         w_qm_knt_tot        ! total number of wind QMs (and therefore events)
                                    !  added to the output PREPBUFR file

      integer   npqm_msg_in         ! number of PQM that are missing in input PREPBUFR file
     +,         npqm_msg_out        ! number of PQM that are missing in output PREPBUFR file
     +,         nzqm_msg_in         ! number of ZQM that are missing in input PREPBUFR file
     +,         nzqm_msg_out        ! number of ZQM that are missing in output PREPBUFR file
     +,         ntqm_msg_in         ! number of TQM that are missing in input PREPBUFR file
     +,         ntqm_msg_out        ! number of TQM that are missing in output PREPBUFR file
     +,         nqqm_msg_in         ! number of QQM that are missing in input PREPBUFR file
     +,         nqqm_msg_out        ! number of QQM that are missing in output PREPBUFR file
     +,         nwqm_msg_in         ! number of WQM that are missing in input PREPBUFR file
     +,         nwqm_msg_out        ! number of WQM that are missing in output PREPBUFR file

c Switches controlling processing (read in from namelist in main program)
c -----------------------------------------------------------------------
      real      trad            ! Time window radius for outputting reports (if l_otw=T)
      logical   l_otw           ! T=eliminate reports outside cycle time window radius (trad)
     +,         l_nhonly        ! T=filter out obs outside tropics and Northern Hemisphere
     +,         l_qmwrite       ! T=write NRL quality marks F=skip it (use with old BUFR formats)

c Counters
      integer   elim_knt(2,3)   ! Count of input PREPBUFR reports (subsets) eliminated from
                                !  write to output PREPBUFR file, and those kept for write to
                                !  output PREPBUFR file -
                                !  first index, message type: 1 - AIRCFT, 2 - AIRCAR
                                !  second index:
                                !    1 - # of reports (subsets) eliminated due to being
                                !         outside time window radius (prior to any
                                !         geographical domain checking)
                                !    2 - # of reports (subsets) eliminated due to being
                                !         outside geographical domain (had passed time window
                                !         radius check)
                                !    3 - # of reports (subsets) passing both time window
                                !         radius and geographical domain checks and thus
                                !         retained for processing into output PREPBUFR file


c Variables to add NRLACQC quality marks to reports
c -------------------------------------------------
      character*11 c_nrlqm                    ! variable used to store NRLACQC quality marks
                                              !  in output PREPBUFR file

c MISC
c ----
      real         nrlacqc_pc ! PREPBUFR program code for the NRLACQC step

      logical      l_skip     ! If true, skip block of code, otherwise exectute block of code
      logical      l_hit_limit! If true, hit limit for number of reports that can be QC'd

      integer      i_hit_limit_first ! flag indicating whether l_hit_limit has occurred prior
                                     !  to this point (if yes, = 1; if no, = 0)

c *******************************************************************

c Initialize variables
c --------------------
      nevwrt      =    0
      ninssrd     =    0
      ncep_qm_p   = 9999
      ncep_rc_p   = 9999
      ncep_qm_z   = 9999
      ncep_rc_z   = 9999
      ncep_qm_t   = 9999
      ncep_rc_t   = 9999
      ncep_qm_q   = 9999
      ncep_rc_q   = 9999
      ncep_qm_w   = 9999
      ncep_rc_w   = 9999
      ncep_rej    =    0
      elim_knt    =    0

      i_hit_limit_first = 0

      npqm_msg_in       = 0
      npqm_msg_out      = 0
      nzqm_msg_in       = 0
      nzqm_msg_out      = 0
      ntqm_msg_in       = 0
      ntqm_msg_out      = 0
      nqqm_msg_in       = 0
      nqqm_msg_out      = 0
      nwqm_msg_in       = 0
      nwqm_msg_out      = 0

      p_qm_knt_tot      = 0
      z_qm_knt_tot      = 0
      t_qm_knt_tot      = 0
      q_qm_knt_tot      = 0
      w_qm_knt_tot      = 0

      nevrd             = 0
      nev_noupd         = 0
      qm_knt            = 0

c Start subroutine
c ----------------
      write(*,*)
      write(*,*) '******************************'
      write(*,*) 'Welcome to output_acqc_noprof.'
      call system('date')
      write(*,*) '******************************'
      write(*,*)

c ----------------------------------------------------------------------
c Translate NRLACQC flags to NCEP events and add events to PREPBUFR file
c ----------------------------------------------------------------------
      l_eventupdate = .false.

      print *
      print *, 'Input/Output PREPBUFR files are open.'
      print *
      print *, 'Reading input PREPBUFR file...'
      print *, 'Applying NRLACQC events to reports...'

      QCdrptsidx = 0       ! starting point for QC'd data arrays' index
      input_sqn_last = -99 ! initial value for last report's sequence number (ensures no
                           !  match for first report read in)

      l_hit_limit = .false.

      do while(ireadmg(inlun,mesgtype,mesgdate).eq.0)

        if(mesgtype.ne.'AIRCFT'.and.mesgtype.ne.'AIRCAR') then
          if(l_eventupdate) then ! need to close leftover AIRCAR or AIRCFT message originally
                                 !  opened by openmb before copymg can copy over an entire
                                 !  message from input to output
            call closmg(outlun)
            l_eventupdate = .false.
          endif

          call copymg(inlun,outlun) ! for non-aircraft BUFR messages, just copy to output
        else
          do while(ireadsb(inlun).eq.0)

c Initialize variables
c --------------------
            ipqm_topstk = 9999
            izqm_topstk = 9999
            itqm_topstk = 9999
            iqqm_topstk = 9999
            iwqm_topstk = 9999

            ninssrd = ninssrd + 1   ! number of input subsets read

c Unpack lat/lon, altitude, ob time, report type and sequence number - will be used to make
c  sure PREPBUFR and QC'd obs are lining up OK
c -----------------------------------------------------------------------------------------
            call ufbint(inlun,arr_8,10,10,nlev,
     +                  'YOB XOB ELV DHR TYP SQN SID')

            input_alat = arr_8(1,1)
            input_alon = arr_8(2,1)
            input_ht_ft = nint(arr_8(3,1)*m2ft)
            input_dhr = arr_8(4,1)    
            input_idt = nint(arr_8(4,1)*3600.)    
            input_typ = nint(arr_8(5,1))
            input_sqn = nint(arr_8(6,1))
            if(input_sqn.ne.input_sqn_last) then

c This input report's sequence number is different that that for the previous report read in
c  - we are at the next report in the QC'd data arrays so increment index QCdrptsidx by 1
              QCdrptsidx = QCdrptsidx + 1
ccccc         if(QCdrptsidx.eq.47955) print *,'WE ARE AT ',QCdrptsidx,'!'

              if(QCdrptsidx.gt.nrpts4QC_pre) then 

c .... the number of merged mass + wind reports read in from the input (non-QC'd) PREPBUFR
c      file exceeds the number of reports QC'd in acftobs_qc - likely due to there being more
c      than "max_reps" merged aircraft reports in the input PREPBUFR file -- in this case no
c      more input PREPBUFR aircraft reports can be QC'd - all remaining reports in input
c      PREPBUFR file will be copied to output PREPBUFR file but they will not be QC'd
                if(i_hit_limit_first.eq.0)  then
                  print *
                  print *, '#####VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV'
                  print *, 'WARNING: QD''d data array index exceeds ',
     +             'the limit of ', nrpts4QC_pre,' - no more reports ',
     +             'can be QC''d'
                  print *, '#####^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
                  print *
                endif
                i_hit_limit_first = 1
                l_hit_limit = .true.
                go to 3400
              endif

              if(QCdrptsidx.gt.1) then
                if(c_qc(QCdrptsidx-1)(2:2).eq.'R'.or.       ! time rehabilitated
     +             c_qc(QCdrptsidx-1)(3:3).eq.'R'.or.       ! latitude rehabilitated
     +             c_qc(QCdrptsidx-1)(4:4).eq.'R'.or.       ! longitude rehabilitated
     +             c_qc(QCdrptsidx-1)(5:5).eq.'R'.or.       ! pressure/altitude rehabilitated
     +             c_qc(QCdrptsidx-1)(6:6).eq.'R'.or.       ! temperature rehabilitated
     +             c_qc(QCdrptsidx-1)(5:5).eq.'r') print 65 ! pressure/altitude rehabilitated
   65 format(131('^'))                                             !  print ^^^ at end of report
              endif
              if(c_qc(QCdrptsidx)(2:2).eq.'R'.or.           ! time rehabilitated
     +           c_qc(QCdrptsidx)(3:3).eq.'R'.or.           ! latitude rehabilitated
     +           c_qc(QCdrptsidx)(4:4).eq.'R'.or.           ! longitude rehabilitated
     +           c_qc(QCdrptsidx)(5:5).eq.'R'.or.           ! pressure/altitude rehabilitated
     +           c_qc(QCdrptsidx)(6:6).eq.'R'.or.           ! temperature rehabilitated
     +           c_qc(QCdrptsidx)(5:5).eq.'r') then         ! pressure/altitude rehabilitated
                print 61                                           ! print ^^^ at beginning of report
   61 format(131('v'))
                if(c_qc(QCdrptsidx)(2:2).eq.'R') then

c Case where time was rehabiltated by NRLACQC, make note of it
c ------------------------------------------------------------
                  print 62,  QCdrptsidx,arr_8(7,1),input_alat,
     +                       input_alon,input_dhr,nint(arr_8(3,1)),
     +                       c_qc(QCdrptsidx)
   62 format(' TIME rehabilitated: input rpt # ',i6,': id ',A8,
     + ', lat ',f9.5,', lon ',f9.5,', dhr ',f10.5,', hght(m)',i6,
     + ', NRLQMS "',A11,'"')
                  print 63, input_dhr,input_idt
   63 format(' INPUT time from PRE-QC PREPBUFR file [DHR,idt(sec)] ',
     + 'is: ',f10.5,i8)
                  print 64, idt(QCdrptsidx)/3600.,idt(QCdrptsidx)
   64 format(' REHABILITATED time from  acftobs_qc  [DHR,idt(sec)] ',
     + 'is: ',f10.5,i8)
                endif
                if(c_qc(QCdrptsidx)(3:3).eq.'R') then

c Case where latitude was rehabiltated by NRLACQC, make note of it
c ----------------------------------------------------------------
                  print 72,  QCdrptsidx,arr_8(7,1),input_alat,
     +                       input_alon,input_dhr,nint(arr_8(3,1)),
     +                       c_qc(QCdrptsidx)
   72 format(' LAT  rehabilitated: input rpt # ',i6,': id ',A8,
     + ', lat ',f9.5,', lon ',f9.5,', dhr ',f10.5,', hght(m)',i6,
     + ', NRLQMS "',A11,'"')
                  print 73, input_alat
   73 format(' INPUT latitude from PRE-QC PREPBUFR file (YOB) is: ',
     + f9.5)
                  print 74, alat(QCdrptsidx)
   74 format(' REHABILITATED latitude from  acftobs_qc  (YOB) is: ',
     + f9.5)
                endif
                if(c_qc(QCdrptsidx)(4:4).eq.'R') then

c Case where longitude was rehabiltated by NRLACQC, make note of it
c -----------------------------------------------------------------
                  print 82,  QCdrptsidx,arr_8(7,1),input_alat,
     +                       input_alon,input_dhr,nint(arr_8(3,1)),
     +                       c_qc(QCdrptsidx)
   82 format(' LON  rehabilitated: input rpt # ',i6,': id ',A8,
     + ', lat ',f9.5,', lon ',f9.5,', dhr ',f10.5,', hght(m)',i6,
     + ', NRLQMS "',A11,'"')
                  print 83, input_alon
   83 format(' INPUT longitude from PRE-QC PREPBUFR file (XOB) is: ',
     + f9.5)
                  print 84, alon(QCdrptsidx)
   84 format(' REHABILITATED longitude from  acftobs_qc  (XOB) is: ',
     + f9.5)
                endif
                if(c_qc(QCdrptsidx)(5:5).eq.'R'.or.
     +             c_qc(QCdrptsidx)(5:5).eq.'r') then

c Case where pressure/altitude was rehabiltated by NRLACQC, make note of it
c -------------------------------------------------------------------------
                  print 92,  QCdrptsidx,arr_8(7,1),input_alat,
     +                       input_alon,input_dhr,nint(arr_8(3,1)),
     +                       c_qc(QCdrptsidx)
   92 format(' P/A  rehabilitated: input rpt # ',i6,': id ',A8,
     + ', lat ',f9.5,', lon ',f9.5,', dhr ',f10.5,', hght(m)',i6,
     + ', NRLQMS "',A11,'"')
                  print 93
   93 format(' %%%%%%%%%%'/' %%%%% WARNING: Currently not accounted ',
     +       'for in output PREPBUFR file'/' %%%%%%%%%%')
                endif
                if(c_qc(QCdrptsidx)(6:6).eq.'R') then

c Case where temperature was rehabiltated by NRLACQC, make note of it
c -------------------------------------------------------------------
                  print 102,  QCdrptsidx,arr_8(7,1),input_alat,
     +                       input_alon,input_dhr,nint(arr_8(3,1)),
     +                       c_qc(QCdrptsidx)
  102 format(' TMP  rehabilitated: input rpt # ',i6,': id ',A8,
     + ', lat ',f9.5,', lon ',f9.5,', dhr ',f10.5,', hght(m)',i6,
     + ', NRLQMS "',A11,'"')
                  print 93
                endif
              endif          ! if any rehabilitated
            endif            ! if(input_sqn.ne.input_sqn_last) 

            dhr_corr = bmiss
            yob_corr = bmiss
            xob_corr = bmiss
            if(c_qc(QCdrptsidx)(2:2).eq.'R') then ! Rehabilitated time
              dhr_corr(1) = idt(QCdrptsidx)/3600. ! Store updated d-time (DHR)
              dhr_corr(2) = 3                     ! Set correction indicator (TCOR) to 3
                                                  ! Original time already stored in RPT
              input_idt = idt(QCdrptsidx)         ! Prevents match check below from failing
              input_dhr = idt(QCdrptsidx)/3600.   ! Allows time window check below to use
                                                  !  rehabilitated time
            endif
            if(c_qc(QCdrptsidx)(3:3).eq.'R') then ! Rehabilitated latitude
              yob_corr(1) = alat(QCdrptsidx)      ! Store updated latitude (YOB)
              yob_corr(2) = 3                     ! Set correction indicator (YCOR) to 3
              yob_corr(3) = input_alat            ! Store original latitude (YORG)
              input_alat = alat(QCdrptsidx)       ! Prevents match check below from failing
                                                  !  and allows geographic domain check below
                                                  !  to use rehabilitated latitude
            endif
            if(c_qc(QCdrptsidx)(4:4).eq.'R') then ! Rehabilitated longitude
              xob_corr(1) = alon(QCdrptsidx)      ! Store updated longitude (XOB)
              xob_corr(2) = 3                     ! Set correction indicator (XCOR) to 3
              xob_corr(3) = input_alon            ! Store original longitude (XORG)
              input_alon = alon(QCdrptsidx)       ! Prevents match check below from failing
                                                  !  and allows geographic domain check below
                                                  !  to use rehabilitated longitude
            endif

            input_sqn_last = input_sqn

c At every 10,000'th QC'd (merged mass + wind) aircraft-type report and at last report, test
c  its lat/lon, time and altitude against corresponding PREPBUFR report values - if values DO
c  NOT match, PROBLEM!!!
c -------------------------------------------------------------------------------------------
            if(mod(QCdrptsidx,10000).eq.0.or.
     +         QCdrptsidx.eq.nrpts4QC_pre) then
              print *, 'ipoint match check at report # ',QCdrptsidx
     	      if(alat(QCdrptsidx).ne.input_alat .or.
     +	         alon(QCdrptsidx).ne.input_alon .or.
     +	         ht_ft(QCdrptsidx).ne.input_ht_ft .or.
     +	         idt(QCdrptsidx).ne. input_idt) then
                print *, 'NO MATCH AT QCdrptsidx = ',QCdrptsidx
                print'(" Indexing problem... could not find the ",
     +                 "current input PREPBUFR report in the report-",
     +	               "oriented arrays.")'
                print *, 'EXITING PROGRAM.'
     
                call w3tage('PREPOBS_PREPACQC')
                call  errexit(31)
              endif
              print *, 'MATCH AT QCdrptsidx = ',QCdrptsidx
     	    endif

 3400       continue

c Before processing this input PREPBUFR report (subset) any further, make sure it is within
c  the requested time window (defined by namelist switch trad) and it is within the requested
c  geographical domain (here north of 20S latitude, if namelist switch l_nhonly is true)
c -------------------------------------------------------------------------------------------

            if(l_otw) then ! check if report (subset) is outside time window (prior to any
                           ! geographical domain checking)
              if(input_dhr.lt.-trad.or.input_dhr.gt.trad) then
                if(mesgtype.eq.'AIRCFT') then
                  elim_knt(1,1) = elim_knt(1,1) + 1
                elseif(mesgtype.eq.'AIRCAR') then
                  elim_knt(2,1) = elim_knt(2,1) + 1
                endif
                if(.not.l_hit_limit) then
                  ncep_rej(QCdrptsidx) = 1
                endif
                cycle ! don't write this subset to output file, move on to next subset
              endif
            endif

            if(l_nhonly) then ! if report (subset) passed time window radius check, then
                              !  check to see if it is outside geographical domain (i.e.,
                              !  south of 20S)
              if(input_alat.lt.-20.0) then
                if(mesgtype.eq.'AIRCFT') then
                  elim_knt(1,2) = elim_knt(1,2) + 1
                elseif(mesgtype.eq.'AIRCAR') then
                  elim_knt(2,2) = elim_knt(2,2) + 1
                endif
                if(.not.l_hit_limit) then
                  ncep_rej(QCdrptsidx) = 1
                endif
                cycle ! don't write this subset to output file, move on to next subset
              endif
            endif

c Counter for number of PREPBUFR reports (subsets) kept
c -----------------------------------------------------
            if(mesgtype.eq.'AIRCFT') then
              elim_knt(1,3) = elim_knt(1,3) + 1
            elseif(mesgtype.eq.'AIRCAR') then
              elim_knt(2,3) = elim_knt(2,3) + 1
            endif

c If the report passes the time window and geographic domain checks, copy the subset to the
c  output PREPBUFR file in anticipation of adding events
c -----------------------------------------------------------------------------------------
            call openmb(outlun,mesgtype,mesgdate) 

            call ufbcpy(inlun,outlun)

            if(l_hit_limit) then

c If this subset exceeds the "max_rep" limit, don't attempt to add QC to it because there is
c  none, instead just write subset to the output PREPBUFR file and move on to next subset
c  (which won't be QC'd either)
c-------------------------------------------------------------------------------------------

               call writsb(outlun)
               cycle
            endif

            if(c_qc(QCdrptsidx)(2:2).eq.'R') then

c Encode rehabilitated time and time correction indicator
c -------------------------------------------------------
              print 66, dhr_corr,input_typ,QCdrptsidx
   66 format(' ENCODE REHABILITATED time ',f10.5, ' as DHR with TCOR=',
     + f3.0,' into PREPBUFR file, rtyp = ',i3,', for input report # ',
     + i8)
              call ufbint(outlun,dhr_corr,2,1,iret,'DHR TCOR')
            endif
            if(c_qc(QCdrptsidx)(3:3).eq.'R') then

c Encode rehabilitated latitude, latitude correction indicator and original latitude
c ----------------------------------------------------------------------------------
              print 76, yob_corr,input_typ,QCdrptsidx
   76 format(' ENCODE REHABILITATED latitude ',f9.5, ' as YOB with ',
     + 'YCOR=',f3.0,' and YORG=',f9.5,' into PREPBUFR file, rtyp = ',i3,
     + ', for input rpt # ',i8)
              call ufbint(outlun,yob_corr,3,1,iret,'YOB YCOR YORG')
            endif
            if(c_qc(QCdrptsidx)(4:4).eq.'R') then

c Encode rehabilitated longitude, longitude correction indicator and original longitude
c -------------------------------------------------------------------------------------
              print 86, xob_corr,input_typ,QCdrptsidx
   86 format(' ENCODE REHABILITATED longitude ',f9.5, ' as XOB with ',
     + 'XCOR=',f3.0,' and XORG=',f9.5,' into PREPBUFR file, rtyp = ',i3,
     + ', for input rpt # ',i8)
              call ufbint(outlun,xob_corr,3,1,iret,'XOB XCOR XORG')
            endif

c If the input PREPBUFR report is a mass report, update the event stack with mass events
c
c If the input PREPBUFR report is a wind rpt, update the event stack with wind events
c
c Also, first initialize the "bad report", "suspect report", and "duplicate report" flags as
c  false - these flags will be set to true if the NRLACQC quality information (array c_qc)
c  indicates that the report or any part of it is bad, suspect or a duplicate
c ------------------------------------------------------------------------------------------
            l_badrpt_p = .false.
            l_badrpt_z = .false.
            l_badrpt_t = .false.
            l_badrpt_q = .false.
            l_badrpt_w = .false.

            l_duprpt = .false.

c Pressure
c --------

c Get POB and PQM at top of stack coming in and store in array p_event, translate NRLACQC
c  quality flags in c_qc to NCEP standards for pressure and store in ipqm_nrlacqc, also store
c  reason code in iprc_nrlacqc (pressure data apply to both mass and wind obs)
c -------------------------------------------------------------------------------------------
            call ufbint(inlun,p_event,4,1,nlev,'POB PQM')
ccccc       if(QCdrptsidx.eq.47955) print *,'input p_event = ',p_event
            nevrd(1) = nevrd(1) + 1

            if(ibfms(p_event(2)).eq.0) then
              if(nint(p_event(2)).ge.0.and.nint(p_event(2)).le.15) then
c PQM for event at top of stack (prior to adding any NRLACQC events)
                ipqm_topstk = nint(p_event(2))
              else
                npqm_msg_in = npqm_msg_in + 1
              endif
            else
              npqm_msg_in = npqm_msg_in + 1
            endif

ccccc       if(QCdrptsidx.eq.47955)
ccccc+       print *,'prior to tranQCflags p_event = ',p_event
            call tranQCflags(c_qc(QCdrptsidx),'p',ipqm_nrlacqc,
     +                       iprc_nrlacqc,l_badrpt_p,l_duprpt)

c if PQM = 2 and PRC = 099 returned from tranQCflags, then can't translate!
            if(ipqm_nrlacqc.eq.2 .and. iprc_nrlacqc.eq.099) then
              print *
              print *, 'Unknown c_qc flag on pressure/altitude:',
     +                 c_qc(QCdrptsidx)(5:5)
              print *, 'PREPBUFR aircraft report number: ',ninssrd
              print *, 'QC ob array index: ',QCdrptsidx
              print *
            endif
ccccc       if(QCdrptsidx.eq.47955)
ccccc+       print *,'after call to tranQCflags PQM, PRC = ',
ccccc+      ipqm_nrlacqc,iprc_nrlacqc

c Altitude
c --------

c Get ZOB and ZQM at top of stack coming in and store in array z_event, translate NRLACQC
c  quality flags in c_qc to NCEP standards for altitude and store in izqm_nrlacqc, also store
c  reason code in izrc_nrlacqc (altitude applies to both mass and wind obs)
c
c Use same quality marks for altitude as were used for pressure - NRLACQC has one flag for
c  both (c_qc(5:5))
c -------------------------------------------------------------------------------------------
            call ufbint(inlun,z_event,4,1,nlev,'ZOB ZQM')
            nevrd(2) = nevrd(2) + 1

            if(ibfms(z_event(2)).eq.0) then
              if(nint(z_event(2)).ge.0.and.nint(z_event(2)).le.15) then
c ZQM for event at top of stack (prior to adding any NRLACQC events)
                izqm_topstk = nint(z_event(2))
              else
                nzqm_msg_in = nzqm_msg_in + 1
              endif
            else
              nzqm_msg_in = nzqm_msg_in + 1
            endif

            call tranQCflags(c_qc(QCdrptsidx),'z',izqm_nrlacqc,
     +                       izrc_nrlacqc,l_badrpt_z,l_duprpt)

c if ZQM = 2 and ZRC = 099 returned from tranQCflags, then can't translate!
            if(izqm_nrlacqc.eq.2 .and. izrc_nrlacqc.eq.099) then
              print *
              print *, 'Unknown c_qc flag on pressure/altitude:',
     +         c_qc(QCdrptsidx)(5:5)
              print *, 'PREPBUFR aircraft report number: ',ninssrd
              print *, 'QC ob array index: ',QCdrptsidx
              print *
            endif

c If the input PREPBUFR report is a mass report, then see if we need to add an event for
c  temperature and moisture - if the input PREPBUFR report is a wind report, then see if we
c  need to add an event for wind
c -----------------------------------------------------------------------------------------

            if(int(input_typ/100).eq.1) then

c -----------
c MASS REPORT
c -----------

c Temperature
c -----------

c Get TOB and TQM at top of stack coming in and store in array t_event, translate NRLACQC
c  quality flags in c_qc to NCEP standards for temperature and store in itqm_nrlacqc, also
c  store reason code in itrc_nrlacqc
c ----------------------------------------------------------------------------------------
              call ufbint(inlun,t_event,4,1,nlev,'TOB TQM')
ccccc         if(QCdrptsidx.eq.47955) print *,'input t_event = ',t_event
              nevrd(3) = nevrd(3) + 1

              if(ibfms(t_event(2)).eq.0) then
                if(nint(t_event(2)).ge.0.and.nint(t_event(2)).le.15)then
c TQM for event at top of stack (prior to adding any NRLACQC events)
                  itqm_topstk = nint(t_event(2))
                else
                  ntqm_msg_in = ntqm_msg_in + 1
                endif
              else
                ntqm_msg_in = ntqm_msg_in + 1
              endif

ccccc         if(QCdrptsidx.eq.47955)
ccccc+         print *,'prior to tranQCflags t_event = ',t_event
              call tranQCflags(c_qc(QCdrptsidx),'t',itqm_nrlacqc,
     +                         itrc_nrlacqc,l_badrpt_t,l_duprpt)

c if TQM = 2 and TRC = 099 returned from tranQCflags, then can't translate!
              if(itqm_nrlacqc.eq.2 .and. itrc_nrlacqc.eq.099) then
                print *
                print *, 'Unknown c_qc flag on temperature:',
     +           c_qc(QCdrptsidx)(6:6)
                print *, 'PREPBUFR aircraft report number: ',ninssrd
                print *, 'QC ob array index: ',QCdrptsidx
                print *
              endif
ccccc         if(QCdrptsidx.eq.47955)
ccccc+         print *,'after call to tranQCflags TQM, TRC = ',
ccccc+         itqm_nrlacqc,itrc_nrlacqc


c Moisture
c --------

c Get QOB and QQM at top of stack coming in and store in array q_event, translate NRLACQC
c  quality flags in c_qc to NCEP standards for moisture and store in iqqm_nrlacqc, also store
c  reason code in iqrc_nrlacqc
c -------------------------------------------------------------------------------------------
              call ufbint(inlun,q_event,4,1,nlev,'QOB QQM')
              nevrd(4) = nevrd(4) + 1

              if(ibfms(q_event(2)).eq.0) then
                if(nint(q_event(2)).ge.0.and.nint(q_event(2)).le.15)then
c QQM for event at top of stack (prior to adding any NRLACQC events)
                  iqqm_topstk = nint(q_event(2))
                else
                  nqqm_msg_in = nqqm_msg_in + 1
                endif
              else
                nqqm_msg_in = nqqm_msg_in + 1
              endif

              call tranQCflags(c_qc(QCdrptsidx),'q',iqqm_nrlacqc,
     +                         iqrc_nrlacqc,l_badrpt_q,l_duprpt)

c if QQM = 2 and QRC = 099 returned from tranQCflags, then can't translate!
              if(iqqm_nrlacqc.eq.2 .and. iqrc_nrlacqc.eq.099) then
                print *
                print *, 'Unknown c_qc flag on moisture:',
     +           c_qc(QCdrptsidx)(9:9)
                print *, 'PREPBUFR aircraft report number: ',ninssrd
                print *, 'QC ob array index: ',QCdrptsidx
                print *
              endif

            elseif(int(input_typ/100).eq.2) then

c -----------
c WIND REPORT
c -----------

c Wind
c ----

c Get UOB, VOB and WQM at top of stack coming in and store in array w_event, translate
c  NRLACQC quality flags in c_qc to NCEP standards for wind and store in iwqm_nrlacqc, also
c  store reason code in iwrc_nrlacqc
c -----------------------------------------------------------------------------------------
              call ufbint(inlun,w_event,5,1,nlev,'UOB VOB WQM')
              nevrd(5) = nevrd(5) + 1

              if(ibfms(w_event(3)).eq.0) then
                if(nint(w_event(3)).ge.0.and.nint(w_event(3)).le.15)then
c WQM for event at top of stack (prior to adding any NRLACQC events)
                  iwqm_topstk = nint(w_event(3))
                else
                  nwqm_msg_in = nwqm_msg_in + 1
                endif
              else
                nwqm_msg_in = nwqm_msg_in + 1
              endif

              call tranQCflags(c_qc(QCdrptsidx),'w',iwqm_nrlacqc,
     +                         iwrc_nrlacqc,l_badrpt_w,l_duprpt)

c if WQM = 2 and WRC = 099 returned from tranQCflags, then can't translate!
              if(iwqm_nrlacqc.eq.2 .and. iwrc_nrlacqc.eq.099) then
                print *
                print *, 'Unknown c_qc flag on wind:',
     +           c_qc(QCdrptsidx)(7:7),'/',c_qc(QCdrptsidx)(8:8)
                print *, 'PREPBUFR aircraft report number: ',ninssrd
                print *, 'QC ob array index: ',QCdrptsidx
                print *
              endif

            endif ! int(input_typ/100) = 1 or 2

ccccc       if(QCdrptsidx.eq.47955)
ccccc+       print *,'prior to entire rpt rej PQM, PRC = ',
ccccc+      ipqm_nrlacqc,iprc_nrlacqc

c If entire report is to be rejected, put reject flags (QM=13) on pressure, altitude,
c  temperature, moisture, and wind
c -----------------------------------------------------------------------------------
            if(l_badrpt_p .or. l_badrpt_z .or.
     +         l_badrpt_t .or. l_badrpt_q .or. l_badrpt_w) then

ccccc       if(QCdrptsidx.eq.47955)
ccccc+       print *, 'we are in reject report logic'

              ipqm_nrlacqc = 13 ! PQM
          ! PRC already encoded into iprc_nrlacqc in subr. tranQCflags

              izqm_nrlacqc = 13 ! ZQM
          ! ZRC already encoded into izrc_nrlacqc in subr. tranQCflags

              if(int(input_typ/100).eq.1) then
                itqm_nrlacqc = 13 ! TQM
          ! TRC already encoded into itrc_nrlacqc in subr. tranQCflags

                iqqm_nrlacqc = 13 ! QQM
          ! QRC already encoded into iqrc_nrlacqc in subr. tranQCflags

              elseif(int(input_typ/100).eq.2) then

                iwqm_nrlacqc = 13 ! WQM
          ! WRC already encoded into iwrc_nrlacqc in subr. tranQCflags

              endif ! int(input_typ/100) = 1 or 2
            endif ! l_badrpt_[p,z,t,q,w]

ccccc       if(QCdrptsidx.eq.47955)
ccccc+       print *,'after entire rpt rej PQM, PRC = ',
ccccc+       ipqm_nrlacqc,iprc_nrlacqc

c If report is marked as a duplicate (c_qc(1:1) = d or D), then mark the entire report with
c  a bad NCEP quality mark (=13)
c -----------------------------------------------------------------------------------------
            if(l_duprpt) then
              ipqm_nrlacqc = 13 ! PQM
          ! PRC already encoded into iprc_nrlacqc in subr. tranQCflags

              izqm_nrlacqc = 13 ! ZQM
          ! ZRC already encoded into izrc_nrlacqc in subr. tranQCflags

              if(int(input_typ/100).eq.1) then
                itqm_nrlacqc = 13 ! TQM
          ! TRC already encoded into itrc_nrlacqc in subr. tranQCflags

                iqqm_nrlacqc = 13 ! QQM
          ! QRC already encoded into iqrc_nrlacqc in subr. tranQCflags

              elseif(int(input_typ/100).eq.2) then

                iwqm_nrlacqc = 13 ! WQM
          ! WRC already encoded into iwrc_nrlacqc in subr. tranQCflags

              endif ! int(input_typ/100) = 1 or 2
            endif ! l_duprpt

c Update pressure, altitude, temperature, moisture and wind stacks with new event in output
c  PREPBUFR file when there has been a qualty mark change by NRLACQC (don't need to write out
c  an event if quality mark has not been changed by this program)
c 
c EXCEPTION: Retain (honor) the incoming quality mark at the top of the stack (i.e., do not
c            write event) when:
c
c       (1) The incoming quality mark at the top of the stack is 0 (keep flag)
c       (2) The incoming quality mark at the top of the stack is between 4 and 15 (bad) -
c           except when NRLACQC itself generates a BAD quality mark (translated to NCEP
c           value of 13), allows reason code to denote why action taken by NRLACQC to mark
c           obs as bad
c       (3) The incoming quality mark at the top of the stack is not between 0 and 15
c           (i.e.,missing)
c       (4) The incoming quality mark at the top of the stack is 3 (suspect) and the NRLACQC
c           generates a GOOD or NEUTRAL or SUSPECT quality mark (translated to NCEP values of
c           1, 2 and 3 resp.) {in other words, unless an ob previously marked as suspect was
c           marked bad by NRLACQC, don't change a suspect quality mark assigned by a PREPBUFR
c           processing step prior to the NRLACQC step}
c       (5) The quality mark translated to its NCEP value is 2 (neutral) and the reason code
c           is returned from tranQCflags is 099 - this indicates that the NRLACQC quality
c           flags in c_qc pertaining to this ob are unknown to transQCflags (the routine
c           tranQCflags may need to be updated to account for the c_qc flags that is coming
c           out of the NRLACQC QC routine for this ob - this would probably only happen if
c           NRL provides an updated/upgraded acftobs_qc module to NCEP)
c       (6) The NCEP equivalent of the NRLACQC is the same as the incoming quality mark of
c           the stack - if there is no change in the quality mark, then do not add a new
c           event and leave the event at the top of the event stack as is with TWO
c           exceptions:
c              a) NRLACQC itself generates a GOOD quality mark (translated to NCEP value of
c                 1)
c              b) NRLACQC itself generates a BAD quality mark (translated to NCEP value of
c                 13)  (see 2 above for more on this)
c -------------------------------------------------------------------------------------------

c Pressure
c --------
ccccc       if(QCdrptsidx.eq.47955)
ccccc+       print *,'prior to writing ? event p_event = ',p_event(1),
ccccc+      ipqm_nrlacqc,p_event(3),iprc_nrlacqc

            l_skip = .true. ! SKIP LOGIC TO WRITE PRESSURE EVENTS - there is no need to do so
                            !  since pressure is a vertical coordinate and it is not analyzed,
                            !  in addition, adding pressure events complicates reason code
                            !  logic

            if(.not.l_skip) then

c .... if here, include logic to write pressure events
              if(ipqm_topstk.eq.0  .or.
     +          (ipqm_topstk.ge.4 .and. ipqm_topstk.le.15) .or. ! ob has already been marked
                                                                !  bad by NCEP codes
     +           ipqm_topstk.eq.9999 .or.
     +          (ipqm_topstk.eq.3.and.ipqm_nrlacqc.le.3) .or.
     +          (ipqm_nrlacqc.eq.2.and.iprc_nrlacqc.eq.099) .or.
     +          (ipqm_topstk.eq.ipqm_nrlacqc.and.ipqm_nrlacqc.ne.1)
     +                  ) then                            ! no event needed; leave PQM as is
     
                ipqm_nrlacqc = ipqm_topstk

                nev_noupd(1) = nev_noupd(1) + 1
              else ! NRL QC produced an event; add this event to top of stack in output
                   !  PREPBUFR file
                p_event(2) = ipqm_nrlacqc
                p_event(3) = nrlacqc_pc
                p_event(4) = iprc_nrlacqc
                call ufbint(outlun,p_event,4,1,iret,'POB PQM PPC PRC') ! pressure & altitude
                                                                       !  apply to both mass
                                                                       !  & wind
                nevwrt(1) = nevwrt(1) + 1
                ncep_rc_p(QCdrptsidx) = iprc_nrlacqc
ccccc           if(QCdrptsidx.eq.47955)
ccccc+           print *,'after writing event p_event = ',p_event
ccccc           if(QCdrptsidx.eq.47955) print *,'after writing event ',
ccccc+           ncep_rc_p = ',ncep_rc_p(QCdrptsidx)
              endif
ccccc         if(QCdrptsidx.eq.47955) print *,'after writing event ',
ccccc+         ncep_qm_p = 'ncep_qm_p(QCdrptsidx)

              if((ipqm_nrlacqc.ge.0.and.ipqm_nrlacqc.le.15).and.
     +           (ipqm_topstk.ge.0.and.ipqm_topstk.le.15)) then
                ncep_qm_p(QCdrptsidx) = ipqm_nrlacqc
                qm_knt(1,ipqm_topstk,ipqm_nrlacqc) =
     +          qm_knt(1,ipqm_topstk,ipqm_nrlacqc) + 1
              else
                npqm_msg_out = npqm_msg_out + 1
              endif

            else

c .... if here, SKIP logic to write pressure events
              ipqm_nrlacqc = ipqm_topstk
              nev_noupd(1) = nev_noupd(1) + 1
              if((ipqm_nrlacqc.ge.0.and.ipqm_nrlacqc.le.15).and.
     +           (ipqm_topstk.ge.0.and.ipqm_topstk.le.15)) then
                ncep_qm_p(QCdrptsidx) = ipqm_nrlacqc
                qm_knt(1,ipqm_topstk,ipqm_nrlacqc) =
     +          qm_knt(1,ipqm_topstk,ipqm_nrlacqc) + 1
              else
                npqm_msg_out = npqm_msg_out + 1
              endif

            endif

c Altitude
c --------

            l_skip = .true. ! SKIP LOGIC TO WRITE ALTITUDE EVENTS - there is no need to do so
                            !  since altitude is a vertical coordinate and it is not analyzed,
                            !  in addition, adding altitude events complicates reason code
                            !  logic

            if(.not.l_skip) then

c .... if here, include logic to write altitude events
              if(izqm_topstk.eq.0  .or.
     +          (izqm_topstk.ge.4 .and. izqm_topstk.le.15) .or. ! ob has already been marked
                                                                !  bad by NCEP codes
     +           izqm_topstk.eq.9999 .or.
     +          (izqm_topstk.eq.3.and.izqm_nrlacqc.le.3) .or.
     +          (izqm_nrlacqc.eq.2.and.izrc_nrlacqc.eq.099) .or.
     +          (izqm_topstk.eq.izqm_nrlacqc.and.izqm_nrlacqc.ne.1)
     +                  ) then                            ! no event needed; leave ZQM as is
                izqm_nrlacqc = izqm_topstk

                nev_noupd(2) = nev_noupd(2) + 1
              else ! NRL QC produced an event; add this event to top of stack in output
                   !  PREPBUFR file
                z_event(2) = izqm_nrlacqc
                z_event(3) = nrlacqc_pc
                z_event(4) = izrc_nrlacqc
                call ufbint(outlun,z_event,4,1,iret,'ZOB ZQM ZPC ZRC') ! pressure & altitude
                                                                       !  apply to both mass
                                                                       !  & wind
                nevwrt(2) = nevwrt(2) + 1
                ncep_rc_z(QCdrptsidx) = izrc_nrlacqc
              endif

              if((izqm_nrlacqc.ge.0.and.izqm_nrlacqc.le.15).and.
     +           (izqm_topstk.ge.0.and.izqm_topstk.le.15)) then
                ncep_qm_z(QCdrptsidx) = izqm_nrlacqc
                qm_knt(2,izqm_topstk,izqm_nrlacqc) =
     +          qm_knt(2,izqm_topstk,izqm_nrlacqc) + 1
              else
                nzqm_msg_out = nzqm_msg_out + 1
              endif

            else

c .... if here, SKIP logic to write altitude events
              izqm_nrlacqc = izqm_topstk
              nev_noupd(2) = nev_noupd(2) + 1
              if((izqm_nrlacqc.ge.0.and.izqm_nrlacqc.le.15).and.
     +           (izqm_topstk.ge.0.and.izqm_topstk.le.15)) then
                ncep_qm_z(QCdrptsidx) = izqm_nrlacqc
                qm_knt(2,izqm_topstk,izqm_nrlacqc) =
     +          qm_knt(2,izqm_topstk,izqm_nrlacqc) + 1
              else
                nzqm_msg_out = nzqm_msg_out + 1
              endif

            endif

            if(int(input_typ/100).eq.1) then

c Temperature
c -----------

c Obs/Events
              if((itqm_topstk.eq.0  .or.
     +          (itqm_topstk.ge.4 .and. itqm_topstk.le.15) .or.  ! ob has already been marked
                                                                 !  bad by NCEP codes
     +           itqm_topstk.eq.9999 .or.
     +          (itqm_topstk.eq.3.and.itqm_nrlacqc.le.3) .or. 
     +          (itqm_nrlacqc.eq.2.and.itrc_nrlacqc.eq.099) .or.
     +          (itqm_topstk.eq.itqm_nrlacqc.and.itqm_nrlacqc.ne.1)
     +                ) .and. (itqm_nrlacqc.ne.13.or.
     +                         itqm_topstk.eq.9999)) then  ! no event needed; leave TQM as is
ccccc           if(QCdrptsidx.eq.47955)
ccccc+           print *,'no t_event written for ',QCdrptsidx
                itqm_nrlacqc = itqm_topstk

                nev_noupd(3) = nev_noupd(3) + 1

              else ! NRL QC produced an event; add this event to top of stack in output
                   !  PREPBUFR file
ccccc           if(QCdrptsidx.eq.47955)
ccccc+           print *,'new t_event written for ',QCdrptsidx
                if(int(itrc_nrlacqc/100).eq.9 .and.
     +             itqm_nrlacqc.eq.13)  itqm_nrlacqc = 14 ! if temperature marked bad here
                                                          !  due to it being on reject list,
                                                          !  reset TQM to 14
                t_event(2) = itqm_nrlacqc
                t_event(3) = nrlacqc_pc
                t_event(4) = itrc_nrlacqc
                call ufbint(outlun,t_event,4,1,iret,'TOB TQM TPC TRC')
                nevwrt(3) = nevwrt(3) + 1
                ncep_rc_t(QCdrptsidx) = itrc_nrlacqc
              endif

              if((itqm_nrlacqc.ge.0.and.itqm_nrlacqc.le.15).and.
     +           (itqm_topstk.ge.0.and.itqm_topstk.le.15)) then
                ncep_qm_t(QCdrptsidx) = itqm_nrlacqc
                qm_knt(3,itqm_topstk,itqm_nrlacqc) =
     +          qm_knt(3,itqm_topstk,itqm_nrlacqc) + 1
              else
                ntqm_msg_out = ntqm_msg_out + 1
              endif

c Moisture
c --------

c Obs/Events
              if((iqqm_topstk.eq.0  .or. 
     +          (iqqm_topstk.ge.4 .and. iqqm_topstk.le.15) .or.  ! ob has already been marked
                                                                 !  bad by NCEP codes
     +           iqqm_topstk.eq.9999 .or.
     +          (iqqm_topstk.eq.3 .and. iqqm_nrlacqc.le.3) .or.
     +          (iqqm_nrlacqc.eq.2.and.iqrc_nrlacqc.eq.099) .or.
     +	        (iqqm_topstk.eq.iqqm_nrlacqc.and.iqqm_nrlacqc.ne.1)
     +                ) .and. (iqqm_nrlacqc.ne.13.or.
     +                         iqqm_topstk.eq.9999)) then  ! no event needed; leave QQM as is
                iqqm_nrlacqc = iqqm_topstk

                nev_noupd(4) = nev_noupd(4) + 1

              else ! NRL QC produced a new event; add this event to top of stack in output
                   !  PREPBUFR file
                if(int(iqrc_nrlacqc/100).eq.9 .and.
     +             iqqm_nrlacqc.eq.13)  iqqm_nrlacqc = 14 ! if moisture marked bad here due
                                                          !  to temperature  being on reject
                                                          !  list, reset QQM to 14
                q_event(2) = iqqm_nrlacqc
                q_event(3) = nrlacqc_pc
                q_event(4) = iqrc_nrlacqc
                call ufbint(outlun,q_event,4,1,iret,'QOB QQM QPC QRC')
                nevwrt(4) = nevwrt(4) + 1
                ncep_rc_q(QCdrptsidx) = iqrc_nrlacqc
              endif

              if((iqqm_nrlacqc.ge.0.and.iqqm_nrlacqc.le.15).and.
     +           (iqqm_topstk.ge.0.and.iqqm_topstk.le.15))then
                ncep_qm_q(QCdrptsidx) = iqqm_nrlacqc
                qm_knt(4,iqqm_topstk,iqqm_nrlacqc) =
     +          qm_knt(4,iqqm_topstk,iqqm_nrlacqc) + 1
              else
                nqqm_msg_out = nqqm_msg_out + 1
              endif

            elseif(int(input_typ/100).eq.2) then

c Wind
C ----

c Obs/Events
              if((iwqm_topstk.eq.0  .or.
     +          (iwqm_topstk.ge.4 .and. iwqm_topstk.le.15) .or.  ! ob has already been marked
                                                                 !  bad by NCEP codes
     +           iwqm_topstk.eq.9999 .or.
     +          (iwqm_topstk.eq.3 .and. iwqm_nrlacqc.le.3) .or.
     +          (iwqm_nrlacqc.eq.2.and.iwrc_nrlacqc.eq.099) .or.
     +          (iwqm_topstk.eq.iwqm_nrlacqc.and.iwqm_nrlacqc.ne.1)
     +                ) .and. (iwqm_nrlacqc.ne.13.or.
     +                         iwqm_topstk.eq.9999)) then  ! no event needed; leave WQM as is
                iwqm_nrlacqc = iwqm_topstk

                nev_noupd(5) = nev_noupd(5) + 1

              else ! NRL QC produced a new event; add this event to top of stack in output
                   !  PREPBUFR file
                if(int(iwrc_nrlacqc/100).eq.9 .and.
     +             iwqm_nrlacqc.eq.13)  iwqm_nrlacqc = 14 ! if wind marked bad here due to it
                                                          !  being on reject list, reset WQM
                                                          !  to 14
                w_event(3) = iwqm_nrlacqc
                w_event(4) = nrlacqc_pc
                w_event(5) = iwrc_nrlacqc
              call ufbint(outlun,w_event,5,1,iret,'UOB VOB WQM WPC WRC')
                nevwrt(5) = nevwrt(5) + 1
                ncep_rc_w(QCdrptsidx) = iwrc_nrlacqc
              endif

              if((iwqm_nrlacqc.ge.0.and.iwqm_nrlacqc.le.15).and.
     +           (iwqm_topstk.ge.0.and.iwqm_topstk.le.15))then
                ncep_qm_w(QCdrptsidx) = iwqm_nrlacqc
                qm_knt(5,iwqm_topstk,iwqm_nrlacqc) =
     +          qm_knt(5,iwqm_topstk,iwqm_nrlacqc) + 1
              else
                nwqm_msg_out = nwqm_msg_out + 1
              endif

            endif

            l_eventupdate = .true.

c After updating all event stacks {pressure (maybe), altitude (maybe), temperature, moisture
c  and wind), write subset to the output PREPBUFR file - also add NRLACQC quality string to
c  this subset, since the string is of length 11 characters, must call WRITLC after call to
c  WRITSB
c ------------------------------------------------------------------------------------------
            call writsb(outlun)

c ***** ----> BUFRLIB routine WRITLC trims the string that is stored, cutting off any blank
c             (" ") characters - since blank characters have meaning in the originally-
c             defined NRLACQC quality string (usually indicating passed all tests and thus
c             good), we earlier (in subroutine acftobs_qc) replaced blank characters with dot
c             (".") characters so these would be retained by WRITLC
c -------------------------------------------------------------------------------------------
            c_nrlqm = c_qc(QCdrptsidx)

ccccc       print *, 'in *noprof.f, writing c_nrlqm=', c_nrlqm

            if (l_qmwrite) then
               call writlc(outlun,c_nrlqm,'NRLQMS')
            end if
c Close loops here
c ----------------
          enddo ! ireadsb
        endif ! check for AIRCFT and AIRCAR messages
      enddo ! ireadmg

c Output counts
c -------------

c Detailed counts of reports eliminated from final PREPBUFR file
c --------------------------------------------------------------
      print *
      print *, '----------------------------------------------------'
      print *, 'Info about reports tossed from final PREPBUFR file: '
      print *, '----------------------------------------------------'
      print *
      if(l_otw) then
        print 96, trad,elim_knt(1,1)
   96 format(' Subsets from AIRCFT msgs tossed because outside req. ',
     + 'time window radius of',F6.2,'hrs (prior to geographical domain',
     + ' chking):',i6)
        print 97, trad,elim_knt(2,1)
   97 format(' Subsets from AIRCAR msgs tossed because outside req. ',
     + 'time window radius of',F6.2,'hrs (prior to geographical domain',
     + ' chking):',i6)
      else
        print *, 'Time window radius check NOT performed, l_otw=',l_otw,
     +           ' (ZERO reports tossed)'
      endif
      print *
      if(l_nhonly) then
        print'(" Subsets from AIRCFT messages passing time window ",
     +         "radius check but outside geographical domain (i.e., S ",
     +         "of 20S lat): ",I0)', elim_knt(1,2)
        print'(" Subsets from AIRCAR messages passing time window ",
     +         "radius check but outside geographical domain (i.e., S ",
     +         "of 20S lat): ",I0)', elim_knt(2,2)
      else
        print'(" Geographical domain check not performed, l_nhonly=",L,
     +         " (ZERO reports tossed)")', l_nhonly
      endif
      print *
      print'(" Number of subsets from AIRCFT messages passing checks ",
     +       "and kept: ",I0)', elim_knt(1,3)
      print'(" Number of subsets from AIRCAR messages passing checks ",
     +       "and kept: ",I0)', elim_knt(2,3)
      print *
      print'(" TOTAL NUMBER OF SUBSETS WRITTEN BACK OUT: ",I0)',
     +     elim_knt(1,3)+elim_knt(2,3)

c Pressure details
c ----------------
      print *
      print *, '***********************'
      print *, 'PQM changes/status quo: '

      print *
      print *, 'Input PQM info:'
      print *, 'PQMs read from PREPBUFR:',nevrd(1)
      print *, 'Obs with MISSING PQMs upon input:',npqm_msg_in
      print *

      print *, 'Output PQM info:'
      print *, 'PQMs written to output PREPBUFR:',nevwrt(1)
      print *, 'Obs with MISSING PQMs (not written to output):',
     +         npqm_msg_out
      print *, 'Obs with NRLACQC QM equal to previous QM (no new ',
     +         'event): ',nev_noupd(1)
 
      print *
      print *, 'Non-missing PQM Breakdown:'
      do i=0,15
        do j=0,15
          if(qm_knt(1,i,j).ne.0) then
            print 50, 'PQM:',i,'->',j,':',qm_knt(1,i,j)
            p_qm_knt_tot = p_qm_knt_tot + qm_knt(1,i,j)
          endif
        enddo
      enddo

      if(p_qm_knt_tot.eq.0) then
        print *, 'NO PQM RESULTS!'
      else
        print 51
        print 52,'TOTAL:',p_qm_knt_tot
      endif

   50 format(1x,a4,1x,i2,1x,a2,1x,i2,a1,1x,i6)
   51 format(1x,'---------------------')
   52 format(1x,a6,9x,i6)

c Altitude details
c ----------------
      print *
      print *, '***********************'
      print *, 'ZQM changes/status quo: '

      print *
      print *, 'Input ZQM info:'
      print *, 'ZQMs read from PREPBUFR:',nevrd(2)
      print *, 'Obs with MISSING ZQMs upon input:',nzqm_msg_in
      print *

      print *, 'Output ZQM info:'
      print *, 'ZQMs written to output PREPBUFR:',nevwrt(2)
      print *, 'Obs with MISSING ZQMs (not written to output):',
     +         nzqm_msg_out
      print *, 'Obs with NRLACQC QM equal to previous QM (no new ',
     +         'event): ',nev_noupd(2)

      print *
      print *, 'Non-missing ZQM Breakdown:'
      do i=0,15
        do j=0,15
          if(qm_knt(2,i,j).ne.0) then
            print 50, 'ZQM:',i,'->',j,':',qm_knt(2,i,j)
            z_qm_knt_tot = z_qm_knt_tot + qm_knt(2,i,j)
          endif
        enddo
      enddo

      if(z_qm_knt_tot.eq.0) then
        print *, 'NO ZQM RESULTS!'
      else
        print 51
        print 52,'TOTAL:',z_qm_knt_tot
      endif

c Temperature details
c -------------------
      print *
      print *, '***********************'
      print *, 'TQM changes/status quo: '

      print *
      print *, 'Input TQM info:'
      print *, 'TQMs read from PREPBUFR:',nevrd(3)
      print *, 'Obs with MISSING TQMs upon input:',ntqm_msg_in
      print *

      print *, 'Output TQM info:'
      print *, 'TQMs written to output PREPBUFR:',nevwrt(3)
      print *, 'Obs with MISSING TQMs (not written to output):',
     +         ntqm_msg_out
      print *, 'Obs with NRLACQC QM equal to previous QM (no new ',
     +         'event): ',nev_noupd(3)

      print *
      print *, 'Non-missing TQM Breakdown:'
      do i=0,15
        do j=0,15
          if(qm_knt(3,i,j).ne.0) then
            print 50, 'TQM:',i,'->',j,':',qm_knt(3,i,j)
            t_qm_knt_tot = t_qm_knt_tot + qm_knt(3,i,j)
          endif
        enddo
      enddo

      if(t_qm_knt_tot.eq.0) then
        print *, 'NO TQM RESULTS!'
      else
        print 51
        print 52,'TOTAL:',t_qm_knt_tot
      endif

c Moisture details 
c ----------------
      print *
      print *, '***********************'
      print *, 'QQM changes/status quo: '

      print *
      print *, 'Input QQM info:'
      print *, 'QQMs read from PREPBUFR:',nevrd(4)
      print *, 'Obs with MISSING QQMs upon input:',nqqm_msg_in
      print *

      print *, 'Output QQM info:'
      print *, 'QQMs written to output PREPBUFR:',nevwrt(4)
      print *, 'Obs with MISSING QQMs (not written to output):',
     +         nqqm_msg_out
      print *, 'Obs with NRLACQC QM equal to previous QM (no new ',
     +         'event): ',nev_noupd(4)

      print *
      print *, 'Non-missing QQM Breakdown:'
      do i=0,15
        do j=0,15
          if(qm_knt(4,i,j).ne.0) then
            print 50, 'QQM:',i,'->',j,':',qm_knt(4,i,j)
            q_qm_knt_tot = q_qm_knt_tot + qm_knt(4,i,j)
          endif
        enddo
      enddo

      if(q_qm_knt_tot.eq.0) then
        print *, 'NO QQM RESULTS!'
      else
        print 51
        print 52,'TOTAL:',q_qm_knt_tot
      endif

c Wind details
c ------------
      print *
      print *, '***********************'
      print *, 'WQM changes/status quo: '

      print *
      print *, 'Input WQM info:'
      print *, 'WQMs read from PREPBUFR:',nevrd(5)
      print *, 'Obs with MISSING WQMs upon input:',nwqm_msg_in
      print *

      print *, 'Output WQM info:'
      print *, 'WQMs written to output PREPBUFR:',nevwrt(5)
      print *, 'Obs with MISSING WQMs (not written to output):',
     +         nwqm_msg_out
      print *, 'Obs with NRLACQC QM equal to previous QM (no new ',
     +         'event): ',nev_noupd(5)

      print *
      print *, 'Non-missing WQM Breakdown:'
      do i=0,15
        do j=0,15
          if(qm_knt(5,i,j).ne.0) then
            print 50, 'WQM:',i,'->',j,':',qm_knt(5,i,j)
            w_qm_knt_tot = w_qm_knt_tot + qm_knt(5,i,j)
          endif
        enddo
      enddo

      if(w_qm_knt_tot.eq.0) then
        print *, 'NO WQM RESULTS!'
      else
        print 51
        print 52,'TOTAL:',w_qm_knt_tot
      endif


      write(*,*)
      write(*,*) '****************************'
      write(*,*) 'output_acqc_noprof has ended'
      call system('date')
      write(*,*) '****************************'
      write(*,*)

      return

      end
