c$$$  Subprogram Documentation Block
c   BEST VIEWED WITH 94-CHARACTER WIDTH WINDOW
c
c Subprogram: sub2mem_um 
c   Programmer: D. Keyser       Org: NP22       Date: 2012-05-08
c
c Abstract: Adds new NRLACQC events for pressure, altitude, temperature, moisture and wind to
c   the top of event stack in memory for a single merged aircraft report. This is
c   accomplished via calls to subroutine tranQCflags to translate the QC information (for
c   each variable) from NRL standards (c_qc_stg array) to their NCEP counterparts and to
c   establish event reason codes for each variable.
c
c Program history log:
c 2010-11-15  S. Bender -- Original Author
c 2012-05-08  D. Keyser -- Prepared for operational implementation
c
c Usage: call sub2mem_um(c_qc_stg,max_reps,mxnmev,j,nevents,
c                        pob_ev,pqm_ev,ppc_ev,prc_ev,
c                        zob_ev,zqm_ev,zpc_ev,zrc_ev,
c                        tob_ev,tqm_ev,tpc_ev,trc_ev,
c                        qob_ev,qqm_ev,qpc_ev,qrc_ev,
c                        uob_ev,vob_ev,wqm_ev,wpc_ev,wrc_ev,
c     			 nrlacqc_pc,l_allev_pf)
c
c   Input argument list:
c     c_qc_stg    - NRLACQC quality information (11 character string)
c     max_reps    - Maximum number of reports accepted by acftobs_qc
c     mxnmev      - Maximum number of events allowed, per variable type
c     j           - Report number index
c     nevents     - Array tracking number of events for variables for each report
c     pob_ev      - Pressure event obs
c     pqm_ev      - Pressure event quality marks
c     ppc_ev      - Pressure event program codes
c     prc_ev      - Pressure event reason codes
c     zob_ev      - Altitude event obs
c     zqm_ev      - Altitude event quality marks
c     zpc_ev      - Altitude event program codes
c     zrc_ev      - Altitude event reason codes
c     tob_ev      - Temperature event obs
c     tqm_ev      - Temperature event quality marks
c     tpc_ev      - Temperature event program codes
c     trc_ev      - Temperature event reason codes
c     qob_ev      - Moisture event obs
c     qqm_ev      - Moisture event quality marks
c     qpc_ev      - Moisture event program codes
c     qrc_ev      - Moisture event reason codes
c     uob_ev      - Wind/u-comp event obs
c     vob_ev      - Wind/v-comp event obs
c     wqm_ev      - Wind event quality marks
c     wpc_ev      - Wind event program codes
c     wrc_ev      - Wind event reason codes
c     nrlacqc_pc  - PREPBUFR program code for the NRLACQC step
c     l_allev_pf  - Logical whether to process latest (likely NRLACQC) event plus all prior
c                   events (TRUE) or only latest event (FALSE) into profiles PREPBUFR-like
c                   file
c
c   Output argument list:
c     nevents     - Array tracking number of events for variables for each report
c     pob_ev      - Pressure event obs
c     ppc_ev      - Pressure event program codes
c     prc_ev      - Pressure event reason codes
c     zob_ev      - Altitude event obs
c     zqm_ev      - Altitude event quality marks
c     zpc_ev      - Altitude event program codes
c     zrc_ev      - Altitude event reason codes
c     tob_ev      - Temperature event obs
c     tqm_ev      - Temperature event quality marks
c     tpc_ev      - Temperature event program codes
c     trc_ev      - Temperature event reason codes
c     qob_ev      - Moisture event obs
c     qqm_ev      - Moisture event quality marks
c     qpc_ev      - Moisture event program codes
c     qrc_ev      - Moisture event reason codes
c     uob_ev      - Wind/u-comp event obs
c     vob_ev      - Wind/v-comp event obs
c     wqm_ev      - Wind event quality marks
c     wpc_ev      - Wind event program codes
c     wrc_ev      - Wind event reason codes
c
c   Output files:
c     Unit 06     - Standard output print
c
c   Subprograms called:
c     Unique:    TRANQCFLAGS
c     Library:
c       BUFRLIB: IBFMS
c
c   Exit States:
c     Cond =  0 - successful run
c
c Remarks: Called by subroutine output_acqc_prof.
c
c Attributes:
c   Language: FORTRAN 90
c   Machine:  NCEP WCOSS
c
c$$$
      subroutine sub2mem_um(c_qc_stg,max_reps,mxnmev,j,nevents,
     +                      pob_ev,pqm_ev,ppc_ev,prc_ev,
     +                      zob_ev,zqm_ev,zpc_ev,zrc_ev,
     +                      tob_ev,tqm_ev,tpc_ev,trc_ev,
     +                      qob_ev,qqm_ev,qpc_ev,qrc_ev,
     +                      uob_ev,vob_ev,wqm_ev,wpc_ev,wrc_ev,
     +                      nrlacqc_pc,l_allev_pf)

      implicit none

c ----------------------
c Declaration statements
c ----------------------

c Indices/counters 
c ----------------
      integer      j                 ! report number index

      character*11 c_qc_stg          ! character QC flags output from NRL QC code
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

c Variables for updating input reports with QC results/events from NRLACQC
c ------------------------------------------------------------------------
      logical   l_badrpt_p          ! T = pressure/altitude is bad per NRLACQC info (c_qc_stg)
     +,	        l_badrpt_z          ! T = pressure/altitude is bad per NRLACQC info (c_qc_stg)
     +,         l_badrpt_t          ! T = temperature is bad per NRLACQC info (c_qc_stg)
     +,         l_badrpt_q          ! T = moisture is bad per NRLACQC info (c_qc_stg)
     +,         l_badrpt_w          ! T = wind is bad per NRLACQC info (c_qc_stg)

      logical   l_duprpt            ! T = report is marked as a duplicate per NRLACQC info
                                    !  (c_qc_stg(1:1)=D/d)

      real*8    pob_topstk          ! event POB at top of stack before adding any events 
                                    !  containing info from NRLACQC
     +,         zob_topstk          ! event ZOB at top of stack before adding any events
                                    !  containing info from NRLACQC
     +,         tob_topstk          ! event TOB at top of stack before adding any events
                                    !  containing info from NRLACQC
     +,	        qob_topstk          ! event QOB at top of stack before adding any events
                                    !  containing info from NRLACQC
     +,	        uob_topstk          ! event UOB at top of stack before adding any events
                                    !  containing info from NRLACQC
     +,	        vob_topstk          ! event VOB at top of stack before adding any events
                                    !  containing info from NRLACQC

      integer   ipqm_topstk         ! event PQM at top of stack before adding any events
                                    !  containing info from NRLACQC
     +,         izqm_topstk         ! event ZQM at top of stack before adding any events
                                    !  containing info from NRLACQC
     +,         itqm_topstk         ! event TQM at top of stack before adding any events
                                    !  containing info from NRLACQC
     +,         iqqm_topstk         ! event QQM at top of stack before adding any events
                                    !  containing info from NRLACQC
     +,         iwqm_topstk         ! event WQM at top of stack before adding any events
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

c Variables used to hold original aircraft data read from input PREPBUFR file - necessary for
c  carrying data through program so that it can be later written to output PREPBUFR-like
c  profiles file from memory instead of going back to input PREPBUFR file and re-reading that
c  file before adding any NRLACQC events
c -------------------------------------------------------------------------------------------
      integer nevents(max_reps,6)   ! array tracking number of events for variables for each
                                    ! report:
                                    !   1 - number of pressure events
                                    !   2 - number of moisture events
                                    !   3 - number of temperature events
                                    !   4 - number of altitude events
                                    !   5 - number of wind (u/v) events
                                    !   6 - number of wind (direction/speed) events

      real*8  pob_ev(max_reps,mxnmev) ! POB values for each report, including all events
     +,       pqm_ev(max_reps,mxnmev) ! PQM values for each report, including all events
     +,       ppc_ev(max_reps,mxnmev) ! PPC values for each report, including all events
     +,       prc_ev(max_reps,mxnmev) ! PRC values for each report, including all events
     +,       zob_ev(max_reps,mxnmev) ! ZOB values for each report, including all events
     +,       zqm_ev(max_reps,mxnmev) ! ZQM values for each report, including all events
     +,       zpc_ev(max_reps,mxnmev) ! ZPC values for each report, including all events
     +,       zrc_ev(max_reps,mxnmev) ! ZRC values for each report, including all events
     +,       tob_ev(max_reps,mxnmev) ! TOB values for each report, including all events
     +,       tqm_ev(max_reps,mxnmev) ! TQM values for each report, including all events
     +,       tpc_ev(max_reps,mxnmev) ! TPC values for each report, including all events
     +,       trc_ev(max_reps,mxnmev) ! TRC values for each report, including all events
     +,       qob_ev(max_reps,mxnmev) ! QOB values for each report, including all events
     +,       qqm_ev(max_reps,mxnmev) ! QQM values for each report, including all events
     +,       qpc_ev(max_reps,mxnmev) ! QPC values for each report, including all events
     +,       qrc_ev(max_reps,mxnmev) ! QRC values for each report, including all events
     +,       uob_ev(max_reps,mxnmev) ! UOB values for each report, including all events
     +,       vob_ev(max_reps,mxnmev) ! VOB values for each report, including all events
     +,       wqm_ev(max_reps,mxnmev) ! WQM values for each report, including all events
     +,       wpc_ev(max_reps,mxnmev) ! WPC values for each report, including all events
     +,       wrc_ev(max_reps,mxnmev) ! WRC values for each report, including all events

      integer mxnmev,max_reps

c Misc.
c -----
      real    nrlacqc_pc              ! PREPBUFR program code for the NRLACQC step

      integer ibfms                   ! BUFRLIB function for testing for missing

      logical l_skip                  ! skip (TRUE) or execute (FALSE) block of code

c Logicals controlling processing (read in from namelist in main program)
c -----------------------------------------------------------------------
      logical l_allev_pf      ! T=process latest (likely NRLACQC) events plus all prior
                              !   events into profiles PREPBUFR-like file
                              !   **CAUTION: More complete option, but will make code take
                              !              longer to run!!!
                              ! F=process ONLY latest (likely NRLACQC) events into profiles
                              !   PREPBUFR-like file (here means input latest events will
                              !   likely be written over by NRLACQC events)
                              ! Note : All pre-existing events plus latest (likely NRLACQC)
                              !        events are always encoded into full PREPBUFR file)


c *******************************************************************

c Initialize variables
c --------------------

      ipqm_topstk = 9999
      izqm_topstk = 9999
      itqm_topstk = 9999
      iqqm_topstk = 9999
      iwqm_topstk = 9999

c Start subroutine
c ----------------

c ---------------------------------------------------------------
c Translate NRLACQC flags to NCEP events and add events to memory
c ---------------------------------------------------------------

c Also, first initialize the "bad report", "suspect report", and "duplicate report" flags as
c  false - these flags will be set to true if the NRLACQC quality information (array
c  c_qc_stg) indicates that the report or any part of it is bad, suspect or a duplicate
c ------------------------------------------------------------------------------------------
      l_badrpt_p = .false.
      l_badrpt_z = .false.
      l_badrpt_t = .false.
      l_badrpt_q = .false.
      l_badrpt_w = .false.

      l_duprpt = .false.

c Pressure
c --------

c Get pressure OB and QM at top of stack coming in (from memory) and store in pob_topstk and
c  ipqm_topstk, translate NRLACQC quality flags in c_qc_stg to NCEP standards for pressure
c  and store in ipqc_nrlacqc, also store reason code in iprc_nrlacqc
c ------------------------------------------------------------------------------------------
      pob_topstk = pob_ev(j,nevents(j,1))
      if(ibfms(pqm_ev(j,nevents(j,1))).eq.0) then
        if(nint(pqm_ev(j,nevents(j,1))).ge.0.and.
     +     nint(pqm_ev(j,nevents(j,1))).le.15) then
c PQM for event at top of stack (prior to adding any NRLACQC events)
          ipqm_topstk = nint(pqm_ev(j,nevents(j,1)))
        endif
      endif

      call tranQCflags(c_qc_stg,'p',ipqm_nrlacqc,iprc_nrlacqc,
     +                 l_badrpt_p,l_duprpt)

c if PQM = 2 and PRC = 099, returned from tranQCflags, then can't translate!
      if(ipqm_nrlacqc.eq.2 .and. iprc_nrlacqc.eq.099) then
        print *
        print *, 'Unknown c_qc_stg flag on pressure/altitude:',
     +           c_qc_stg(5:5)
        print *
      endif

c Altitude
c --------

c Get altitude OB and QM at top of stack coming in (from memory) and store in zob_topstk and
c  izqm_topstk, translate NRLACQC quality flags in c_qc_stg to NCEP standards for altitude
c  and store in izqc_nrlacqc, also store reason code in izrc_nrlacqc
c
c Use same quality marks for altitude as were used for pressure - NRLACQC has one flag for
c  both (c_qc_stg(5:5))
c ------------------------------------------------------------------------------------------
      zob_topstk = zob_ev(j,nevents(j,4))
      if(ibfms(zqm_ev(j,nevents(j,4))).eq.0) then
        if(nint(zqm_ev(j,nevents(j,4))).ge.0.and.
     +     nint(zqm_ev(j,nevents(j,4))).le.15) then
c ZQM for event at top of stack (prior to adding any NRLACQC events)
          izqm_topstk = nint(zqm_ev(j,nevents(j,4)))
        endif
      endif

      call tranQCflags(c_qc_stg,'z',izqm_nrlacqc,izrc_nrlacqc,
     +                 l_badrpt_z,l_duprpt)

c if ZQM = 2 and ZRC = 099 returned from tranQCflags, then can't translate!
      if(izqm_nrlacqc.eq.2 .and. izrc_nrlacqc.eq.099) then
        print *
        print *, 'Unknown c_qc_stg flag on pressure/altitude:',
     +           c_qc_stg(5:5)
        print *
      endif

c Temperature
c -----------

c Get temperature OB and QM at top of stack coming in (from memory) and store in tob_topstk
c  and itqm_topstk, translate NRLACQC quality flags in c_qc_stg to NCEP standards for
c  temperature and store in itqc_nrlacqc, also store reason code in itrc_nrlacqc
c -----------------------------------------------------------------------------------------
      tob_topstk = tob_ev(j,nevents(j,3))
      if(ibfms(tqm_ev(j,nevents(j,3))).eq.0) then
        if(nint(tqm_ev(j,nevents(j,3))).ge.0.and.
     +     nint(tqm_ev(j,nevents(j,3))).le.15) then
c TQM for event at top of stack (prior to adding any NRLACQC events)
          itqm_topstk = nint(tqm_ev(j,nevents(j,3)))
        endif
      endif

      call tranQCflags(c_qc_stg,'t',itqm_nrlacqc,itrc_nrlacqc,
     +                 l_badrpt_t,l_duprpt)

c if TQM = 2 and TRC = 099 returned from tranQCflags, then can't translate!
      if(itqm_nrlacqc.eq.2 .and. itrc_nrlacqc.eq.099) then
        print *
        print *, 'Unknown c_qc_stg flag on temperature:',
     +           c_qc_stg(6:6)
        print *
      endif

c Moisture
c --------

c Get moisture OB and QM at top of stack coming in (from memory) and store in qob_topstk and
c  iqqm_topstk, translate NRLACQC quality flags in c_qc_stg to NCEP standards for moisture
c  and store in iqqc_nrlacqc, also store reason code in iqrc_nrlacqc
c ------------------------------------------------------------------------------------------
      qob_topstk = qob_ev(j,nevents(j,2))
      if(ibfms(qqm_ev(j,nevents(j,2))).eq.0) then
        if(nint(qqm_ev(j,nevents(j,2))).ge.0.and.
     +     nint(qqm_ev(j,nevents(j,2))).le.15) then
c QQM for event at top of stack (prior to adding any NRLACQC events)
          iqqm_topstk = nint(qqm_ev(j,nevents(j,2)))
        endif
      endif

      call tranQCflags(c_qc_stg,'q',iqqm_nrlacqc,iqrc_nrlacqc,
     +                 l_badrpt_q,l_duprpt)

c if QQM = 2 and QRC = 099 returned from tranQCflags, then can't translate!
      if(iqqm_nrlacqc.eq.2 .and. iqrc_nrlacqc.eq.099) then
        print *
        print *, 'Unknown c_qc_stg flag on moisture:',
     +           c_qc_stg(9:9)
        print *
      endif

c Wind
c ----

c Get wind OB (u- and v-) and QM at top of stack coming in (from memory) and store in
c  uob_topstk, vob_topstk, and iwqm_topstk, translate NRLACQC quality flags in c_qc_stg to
c  NCEP standards for wind and store in iwqc_nrlacqc, also store reason code in iwrc_nrlacqc
c ------------------------------------------------------------------------------------------
      uob_topstk = uob_ev(j,nevents(j,5))
      vob_topstk = vob_ev(j,nevents(j,5))
      if(ibfms(wqm_ev(j,nevents(j,5))).eq.0) then
        if(nint(wqm_ev(j,nevents(j,5))).ge.0.and.
     +     nint(wqm_ev(j,nevents(j,5))).le.15) then
c WQM for event at top of stack (prior to adding any NRLACQC events)
          iwqm_topstk = nint(wqm_ev(j,nevents(j,5)))
        endif
      endif

      call tranQCflags(c_qc_stg,'w',iwqm_nrlacqc,iwrc_nrlacqc,
     +                 l_badrpt_w,l_duprpt)

c if WQM = 2 and WRC = 099 returned from tranQCflags, then can't translate!
      if(iwqm_nrlacqc.eq.2 .and. iwrc_nrlacqc.eq.099) then
        print *
        print *, 'Unknown c_qc_stg flag on wind:',
     +           c_qc_stg(7:7),'/',c_qc_stg(8:8)
        print *
      endif

c If entire report is to be rejected, put reject flags (QM=13) on pressure, altitude,
c  temperature, moisture, and wind
c -----------------------------------------------------------------------------------
      if(l_badrpt_p .or. l_badrpt_z .or.
     +   l_badrpt_t .or. l_badrpt_q .or. l_badrpt_w) then
        ipqm_nrlacqc = 13 ! PQM
    ! PRC already encoded into iprc_nrlacqc in subr. tranQCflags

        izqm_nrlacqc = 13 ! ZQM
    ! ZRC already encoded into izrc_nrlacqc in subr. tranQCflags

        itqm_nrlacqc = 13 ! TQM
    ! TRC already encoded into itrc_nrlacqc in subr. tranQCflags

        iqqm_nrlacqc = 13 ! QQM
    ! QRC already encoded into iqrc_nrlacqc in subr. tranQCflags

        iwqm_nrlacqc = 13 ! WQM
    ! WRC already encoded into iwrc_nrlacqc in subr. tranQCflags

      endif ! l_badrpt_[p,z,t,q,w]

c If report is marked as a duplicate (c_qc_stg(1:1) = d or D), then mark the entire report
c  with a bad NCEP quality mark (=13)
c ----------------------------------------------------------------------------------------
      if(l_duprpt) then
        ipqm_nrlacqc = 13 ! PQM
    ! PRC already encoded into iprc_nrlacqc in subr. tranQCflags

        izqm_nrlacqc = 13 ! ZQM
    ! ZRC already encoded into izrc_nrlacqc in subr. tranQCflags

        itqm_nrlacqc = 13 ! TQM
    ! TRC already encoded into itrc_nrlacqc in subr. tranQCflags

        iqqm_nrlacqc = 13 ! QQM
    ! QRC already encoded into iqrc_nrlacqc in subr. tranQCflags

        iwqm_nrlacqc = 13 ! WQM
    ! WRC already encoded into iwrc_nrlacqc in subr. tranQCflags

      endif ! l_duprpt

c Update pressure, altitude, temperature, moisture and wind stacks with new event in memory
c  when there has been a qualty mark change by NRLACQC (don't need to write out an event if
c  quality mark has not been changed by this program)
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
c           flags in c_qc_stg pertaining to this ob are unknown to transQCflags (the routine
c           tranQCflags may need to be updated to account for the c_qc_stg flags that is
c           coming out of the NRLACQC QC routine for this ob - this would probably only
c           happen if NRL provides an updated/upgraded acftobs_qc module to NCEP)
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

      l_skip = .true. ! SKIP LOGIC TO WRITE PRESSURE EVENTS - there is no need to do so since
                      !  pressure is a vertical coordinate and it is not analyzed, in
                      !  addition, adding pressure events complicates reason code logic

      if(.not.l_skip) then

c .... if here, include logic to write pressure events
        if(ipqm_topstk.eq.0  .or.
     +    (ipqm_topstk.ge.4 .and. ipqm_topstk.le.15) .or.  ! ob has already been marked bad
                                                           !  by NCEP codes
     +     ipqm_topstk.eq.9999 .or.
     +    (ipqm_topstk.eq.3.and.ipqm_nrlacqc.le.3) .or.
     +    (ipqm_nrlacqc.eq.2.and.iprc_nrlacqc.eq.099) .or.
     +    (ipqm_topstk.eq.ipqm_nrlacqc.and.ipqm_nrlacqc.ne.1)
     +            ) then                               ! no event needed; leave PQM as is

          ipqm_nrlacqc = ipqm_topstk

        else ! NRL QC produced an event; add this event to top of stack in memory
          if(l_allev_pf) nevents(j,1) = nevents(j,1) + 1 ! add new event (do not write over
                                                         !  event currently at top of stack
                                                         !  since l_allev_pf=TRUE)
          pob_ev(j,nevents(j,1)) = pob_topstk
          pqm_ev(j,nevents(j,1)) = ipqm_nrlacqc
     	  ppc_ev(j,nevents(j,1)) = nrlacqc_pc
     	  prc_ev(j,nevents(j,1)) = iprc_nrlacqc

        endif

      else

c .... if here, SKIP logic to write pressure events
        ipqm_nrlacqc = ipqm_topstk

      endif

c Altitude
c --------

      l_skip = .true. ! SKIP LOGIC TO WRITE ALTITUDE EVENTS - there is no need to do so since
                      !  altitude is a vertical coordinate and it is not analyzed, in
                      !  addition, adding altitude events complicates reason code logic

      if(.not.l_skip) then

c .... if here, include logic to write altitude events
        if(izqm_topstk.eq.0  .or.
     +    (izqm_topstk.ge.4 .and. izqm_topstk.le.15) .or.  ! ob has already been marked bad
                                                           !  by NCEP codes
     +     izqm_topstk.eq.9999 .or.
     +    (izqm_topstk.eq.3.and.izqm_nrlacqc.le.3) .or.
     +    (izqm_nrlacqc.eq.2.and.izrc_nrlacqc.eq.099) .or.
     +    (izqm_topstk.eq.izqm_nrlacqc.and.izqm_nrlacqc.ne.1)
     +            ) then                               ! no event needed; leave ZQM as is
          izqm_nrlacqc = izqm_topstk

        else ! NRL QC produced an event; add this event to top of stack in memory
          if(l_allev_pf) nevents(j,4) = nevents(j,4) + 1 ! add new event (do not write over
                                                         !  event currently at top of stack
                                                         !  since l_allev_pf=TRUE)
          zob_ev(j,nevents(j,4)) = zob_topstk
          zqm_ev(j,nevents(j,4)) = izqm_nrlacqc
          zpc_ev(j,nevents(j,4)) = nrlacqc_pc
          zrc_ev(j,nevents(j,4)) = izrc_nrlacqc
     
        endif

      else

c .... if here, SKIP logic to write altitude events
        izqm_nrlacqc = izqm_topstk

      endif

c Temperature
c -----------

c Obs/Events
      if((itqm_topstk.eq.0  .or.
     +  (itqm_topstk.ge.4 .and. itqm_topstk.le.15) .or.  ! ob has already been marked bad by
                                                         !  NCEP codes
     +   itqm_topstk.eq.9999 .or.
     +  (itqm_topstk.eq.3.and.itqm_nrlacqc.le.3) .or.
     +  (itqm_nrlacqc.eq.2.and.itrc_nrlacqc.eq.099) .or.
     +  (itqm_topstk.eq.itqm_nrlacqc.and.itqm_nrlacqc.ne.1)
     +        ) .and. (itqm_nrlacqc.ne.13.or.
     +                 itqm_topstk.eq.9999)) then  ! no event needed; leave TQM as is
        itqm_nrlacqc = itqm_topstk

      else ! NRL QC produced an event; add this event to top of stack in memory
        if(l_allev_pf) nevents(j,3) = nevents(j,3) + 1 ! add new event (do not write over
                                                       !  event currently at top of stack
                                                       !  since l_allev_pf=TRUE)
        if(int(itrc_nrlacqc/100).eq.9 .and.
     +     itqm_nrlacqc.eq.13)  itqm_nrlacqc = 14 ! if temperature marked bad here due to it
                                                  !  being on reject list, reset TQM to 14
        tob_ev(j,nevents(j,3)) = tob_topstk
        tqm_ev(j,nevents(j,3)) = itqm_nrlacqc
        tpc_ev(j,nevents(j,3)) = nrlacqc_pc
        trc_ev(j,nevents(j,3)) = itrc_nrlacqc

      endif

c Moisture
c --------

c Obs/Events
      if((iqqm_topstk.eq.0  .or.
     +  (iqqm_topstk.ge.4 .and. iqqm_topstk.le.15) .or.  ! ob has already been marked bad by
                                                         !  NCEP codes
     +   iqqm_topstk.eq.9999 .or.
     +  (iqqm_topstk.eq.3 .and. iqqm_nrlacqc.le.3) .or.
     +  (iqqm_nrlacqc.eq.2.and.iqrc_nrlacqc.eq.099) .or.
     +  (iqqm_topstk.eq.iqqm_nrlacqc.and.iqqm_nrlacqc.ne.1)
     +        ) .and. (iqqm_nrlacqc.ne.13.or.
     +                 iqqm_topstk.eq.9999)) then  ! no event needed; leave QQM as is
        iqqm_nrlacqc = iqqm_topstk

      else ! NRL QC produced a new event; add this event to top of stack in memory
        if(l_allev_pf) nevents(j,2) = nevents(j,2) + 1 ! add new event (do not write over
                                                       !  event currently at top of stack
                                                       !  since l_allev_pf=TRUE)
        if(int(iqrc_nrlacqc/100).eq.9 .and.
     +     iqqm_nrlacqc.eq.13)  iqqm_nrlacqc = 14 ! if moisture marked bad here due to
                                                  !  temperature  being on reject list, reset
                                                  !  QQM to 14
        qob_ev(j,nevents(j,2)) = qob_topstk
        qqm_ev(j,nevents(j,2)) = iqqm_nrlacqc
        qpc_ev(j,nevents(j,2)) = nrlacqc_pc
        qrc_ev(j,nevents(j,2)) = iqrc_nrlacqc

      endif

c Wind
c ----

c Obs/Events
      if((iwqm_topstk.eq.0  .or.
     +  (iwqm_topstk.ge.4 .and. iwqm_topstk.le.15) .or.  ! ob has already been marked bad by
                                                         !  NCEP codes
     +   iwqm_topstk.eq.9999 .or.
     +  (iwqm_topstk.eq.3 .and. iwqm_nrlacqc.le.3) .or.
     +  (iwqm_nrlacqc.eq.2.and.iwrc_nrlacqc.eq.099) .or.
     +  (iwqm_topstk.eq.iwqm_nrlacqc.and.iwqm_nrlacqc.ne.1)
     +        ) .and. (iwqm_nrlacqc.ne.13.or.
     +                 iwqm_topstk.eq.9999)) then  ! no event needed; leave WQM as is
        iwqm_nrlacqc = iwqm_topstk

      else ! NRL QC produced a new event; add this event to top of stack in memory
        if(l_allev_pf) nevents(j,5) = nevents(j,5) + 1 ! add new event (do not write over
                                                       !  event currently at top of stack
                                                       !  since l_allev_pf=TRUE)
        if(int(iwrc_nrlacqc/100).eq.9 .and.
     +     iwqm_nrlacqc.eq.13)  iwqm_nrlacqc = 14 ! if wind marked bad here due to it being
                                                  !  on reject list, reset WQM to 14
        uob_ev(j,nevents(j,5)) = uob_topstk
        vob_ev(j,nevents(j,5)) = vob_topstk
        wqm_ev(j,nevents(j,5)) = iwqm_nrlacqc
        wpc_ev(j,nevents(j,5)) = nrlacqc_pc
        wrc_ev(j,nevents(j,5)) = iwrc_nrlacqc

      endif

      return

      end
