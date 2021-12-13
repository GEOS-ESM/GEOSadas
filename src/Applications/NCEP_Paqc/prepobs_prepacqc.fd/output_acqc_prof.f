c$$$  Subprogram Documentation Block
c   BEST VIEWED WITH 94-CHARACTER WIDTH WINDOW
c
c Subprogram: output_acqc_prof 
c   Programmer: D. Keyser       Org: NP22       Date: 2016-12-09
c
c Abstract: Reads in sorted NRLACQC quality controlled single-level aircraft reports and
c   constructs profiles from ascending or descending flights.  Encodes these profiles as
c   merged (mass and wind) reports (subsets) along with (when l_prof1lvl=T) merged
c   single(flight)-level aircraft reports not part of any profile into a PREPBUFR-like file
c   containing only these data.  Single-level reports get PREPBUFR report type 3xx (where xx
c   is original type in 1xx mass and 2xx wind reports), ascending profile reports get
c   PREPBUFR report type 4xx, and descending profile reports get PREPBUFR report type 5xx.
c
c Program History Log:
c 2010-11-15  S. Bender  -- Original Author
c 2012-05-08  D. Keyser  -- Prepared for operational implementation
c 2012-11-20  J. Woollen -- Initial port to WCOSS
c 2013-02-07  D. Keyser  -- Final changes to run on WCOSS: use formatted print statements
c                           where previously unformatted print was > 80 characters
c 2016-12-09  D. Keyser  --
c                 - Nomenclature change: replaced "MDCRS/ACARS" with just "MDCRS".
c                 - Latitude/longitdue arrays "alat" and "alon" passed into of this subroutine
c                   now double precision. XOB and YOB in PREPBUFR file now scaled to 10**5
c                   (was 10**2) to handle new v7 AMDAR and MDCRS reports which have this
c                   higher precision.
c                   BENEFIT: Retains exact precison here. Improves QC processing.
c                         - The format for all print statements containing latitude and longitude
c                           changed to print to 5 decimal places.
c
c Usage: call output_acqc_prof(proflun,nrpts4QC_pre,max_reps,mxnmev,
c                              mxlv,bmiss,cdtg_an,alat,alon,ht_ft,
c                              idt,c_qc,trad,l_otw,l_nhonly,sortidx,
c                              c_acftreg,c_acftid,ob_t,nevents,hdr,
c                              acid,rct,drinfo,acft_seq,mstq,cat,
c                              pob_ev,pqm_ev,ppc_ev,prc_ev,pbg,ppp,
c                              zob_ev,zqm_ev,zpc_ev,zrc_ev,zbg,zpp,
c                              tob_ev,tqm_ev,tpc_ev,trc_ev,tbg,tpp,
c                              qob_ev,qqm_ev,qpc_ev,qrc_ev,qbg,qpp,
c                              uob_ev,vob_ev,wqm_ev,wpc_ev,wrc_ev,
c                              wbg,wpp,ddo_ev,ffo_ev,dfq_ev,dfp_ev,
c                              dfr_ev,nrlacqc_pc,l_allev_pf,
c                              l_prof1lvl,l_mandlvl,tsplines,l_operational,lwr)
c
c   Input argument list:
c     proflun      - Unit number for the output post-PREPACQC PREPBUFR-like file containing
c                    merged profile reports (always) and single(flight)-level reports not
c                    part of any profile (when l_prof1lvl=T) with added NRLACQC events
c                    (aircraft data only)
c     nrpts4QC_pre - Number of reports in the "merged" single-level aircraft report arrays
c     max_reps     - Maximum number of reports accepted by acftobs_qc
c     mxnmev       - Maximum number of events allowed, per variable type
c     mxlv         - Maximum number of levels allowed in a report profile
c     bmiss        - BUFRLIB missing value (set in main program)
c     cdtg_an      - Date/analysis time (YYYYMMDDCC)
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
c     sortidx      - Sort index that specifies the order in which the reports should be
c                     written to the output PREPBUFR-like profiles file
c     c_acftreg    - Array of aircraft tail numbers for the "merged" reports as used in
c                    NRL QC processing
c     c_acftid     - Array of aircraft flight numbers for the "merged" reports as used in
c                    NRL QC processing
c     ob_t         - Array of aircraft temperatures for the "merged" reports
c     nevents      - Array tracking number of events for variables for each report
c     hdr          - Array containing header information for the "merged" reports {word 1 is
c                    flight number for AIREPs, tail number for AMDARs (all types) and MDCRS,
c                    and manfactured id for PIREPs and TAMDARs  - this will be later be
c                    encoded into 'SID' for aircraft reports in output  PREPBUFR-like file)
c     acid         - Array containing flight numbers for the "merged" MDCRS and AMDAR (LATAM
c                    only) reports {this will be encoded into 'ACID' for MDCRS and AMDAR
c                    (LATAM only) reports in output PREPBUFR-like profiles file}
c     rct          - Array containing receipt times for the "merged" reports
c     drinfo       - Array containing drift information for the "merged" reports
c     acft_seq     - Array containing temperature precision and phase of flight for the
c                    "merged" reports
c     mstq         - Array containing moisture quality flags for the "merged" reports
c     cat          - Array containing level category for the "merged" reports
c     pob_ev       - Pressure event obs
c     pqm_ev       - Pressure event quality marks
c     ppc_ev       - Pressure event program codes
c     prc_ev       - Pressure event reason codes
c     pbg          - Pressure background data (POE PFC PFCMOD)
c     ppp          - Pressure post-processing info (PAN PCL PCS)
c     zob_ev       - Altitude event obs
c     zqm_ev       - Altitude event quality marks
c     zpc_ev       - Altitude event program codes
c     zrc_ev       - Altitude event reason codes
c     zbg          - Altitude background data (ZOE ZFC ZFCMOD)
c     zpp          - Altitude post-processing info (ZAN ZCL ZCS)
c     tob_ev       - Temperature event obs
c     tqm_ev       - Temperature event quality marks
c     tpc_ev       - Temperature event program codes
c     trc_ev       - Temperature event reason codes
c     tbg          - Temperature background data (TOE TFC TFCMOD)
c     tpp          - Temperature post-processing info (TAN TCL TCS)
c     qob_ev       - Moisture event obs
c     qqm_ev       - Moisture event quality marks
c     qpc_ev       - Moisture event program codes
c     qrc_ev       - Moisture event reason codes
c     qbg          - Moisture background data (QOE QFC QFCMOD)
c     qpp          - Moisture post-processing info (QAN QCL QCS)
c     uob_ev       - Wind/u-comp event obs
c     vob_ev       - Wind/v-comp event obs
c     wqm_ev       - Wind event quality marks
c     wpc_ev       - Wind event program codes
c     wrc_ev       - Wind event reason codes
c     wbg          - Wind background data (WOE UFC VFC UFCMOD VFCMOD)
c     wpp          - Wind post-processing info (UAN VAN UCL VCL UCS VCS)
c     ddo_ev       - Wind direction event obs
c     ffo_ev       - Wind speed event obs
c     dfq_ev       - Wind direction/speed quality mark
c     dfp_ev       - Wind direction/speed program code
c     dfr_ev       - Wind direction/speed reason code
c     nrlacqc_pc   - PREPBUFR program code for the NRLACQC step
c     l_allev_pf   - Logical whether to process latest (likely NRLACQC) event plus all prior
c                    events (TRUE) or only latest event (FALSE) into profiles PREPBUFR-like
c                    file
c     l_prof1lvl   - Logical whether to encode merged single(flight)-level aircraft reports
c                    with NRLACQC events that are not part of any profile into PREPBUFR-like
c                    file (along with, always, merged profiles from aircraft ascents and
c                    descents)
c     l_mandlvl    - Logical whether to interpolate to mandatory levels in profile generation
c     tsplines     - Logical whether to use tension-splines for aircraft vertical velocity
c                    calculation
c     l_operational- Run program in operational mode if true
c     lwr          - Machine word length in bytes (either 4 or 8)
c
c   Output files:
c     Unit proflun - PREPBUFR-like file containing merged (mass and wind) profile reports
c                    (always) and single(flight)-level reports not part of any profile (when
c                    l_prof1lvl=T) with NRLACQC events
c     Unit 06      - Standard output print
c     Unit 52      - Text file containing listing of all QC'd merged aircraft reports written
c                    to profiles PREPBUFR-like file
c
c   Subprograms called:
c     Unique:    SUB2MEM_MER SUB2MEM_UM
c     Library:
c       SYSTEM:  SYSTEM
c       BUFRLIB: OPENMB      WRITSB
c
c   Exit States:
c     Cond =  0 - successful run
c
c Remarks: Called by main program.
c
c Attributes:
c   Language: FORTRAN 90
c   Machine:  NCEP WCOSS
c
c$$$
      subroutine output_acqc_prof(proflun,nrpts4QC_pre,max_reps,mxnmev,
     +                            mxlv,bmiss,cdtg_an,alat,alon,ht_ft,
     +                            idt,c_qc,trad,l_otw,l_nhonly,sortidx,
     +                            c_acftreg,c_acftid,ob_t,nevents,hdr,
     +                            acid,rct,drinfo,acft_seq,mstq,cat,
     +                            pob_ev,pqm_ev,ppc_ev,prc_ev,pbg,ppp,
     +                            zob_ev,zqm_ev,zpc_ev,zrc_ev,zbg,zpp,
     +                            tob_ev,tqm_ev,tpc_ev,trc_ev,tbg,tpp,
     +                            qob_ev,qqm_ev,qpc_ev,qrc_ev,qbg,qpp,
     +                            uob_ev,vob_ev,wqm_ev,wpc_ev,wrc_ev,
     +                            wbg,wpp,ddo_ev,ffo_ev,dfq_ev,dfp_ev,
     +                            dfr_ev,nrlacqc_pc,l_allev_pf,
     +                            l_prof1lvl,l_mandlvl,tsplines,
     +                            l_operational,lwr)

      implicit none
      integer mevwrt(1)   ! DAK: This is a "dummy" variable, not used anywhere.  For some
                          !      reason if one removes this, moves it to any other place in
                          !      this subr., changes the dimension, or does not initialize it
                          !      as zero (look below)  the compiler can fail under -O3 with
                          !      debugging turned on ("An error occurred during code
                          !      generation.  The code generation return code was 40." 
                          !     "Compilation failed for file output_acqc_prof.f."

c ------------------------------
c Parameter statements/constants
c ------------------------------
      integer    proflun              ! output unit number for post-PREPACQC PREPBUFR-like
                                      !  file containing merged profile reports (always) and
                                      !  single(flight)-level reports not part of any profile
                                      !  (when l_prof1lvl=T) with added NRLACQC events

      integer    max_reps             ! maximum number of input merged (mass + wind piece)
                                      !  aircraft-type reports allowed
cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
c replace above with this in event of future switch to dynamic memory allocation

calloc  integer    max_reps           ! original number of input reports obtained from
calloc                                !  first pass through to get total for array allocation
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      real*8     bmiss                ! BUFRLIB missing value (set in main program)

c ----------------------
c Declaration statements
c ----------------------

c Variables for BUFRLIB interface
c -------------------------------
      character*10 cdtg_an            ! date-time group for analysis (YYYYMMDDCC)
                                      !  (all messages in a PREPBUFR-like file should have
      integer      icdtg_an           !   same YYYYMMDDCC)

c Indices/counters 
c ----------------
      integer      i,j,k,ii           ! loop indices

      integer      nrpts4QC_pre       ! original number of input merged (mass + wind piece)
                                      !  aircraft-type reports (read in from PREPBUFR file)

      integer      sortidx(max_reps)  ! index if reports are to be written back out in a 
                                      !  certain order (determined by calling routine)

      character*11 c_qc(max_reps)     ! character QC flags output from NRL QC code
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

      real*8       alat(max_reps)     ! latitude
     +,            alon(max_reps)     ! longitude
      real         ht_ft(max_reps)    ! altitude in feet

      integer      idt(max_reps)      ! time in seconds to anal. time (- before, + after)
      character*8  c_acftreg(max_reps)! aircraft registration (tail) number as used in NRL
                                      !  QC processing
      character*9  c_acftid(max_reps) ! aircraft flight number as used in NRL QC processing

      real         ob_t(max_reps)     ! temperature

c Logicals controlling processing (read in from namelist in main program)
c -----------------------------------------------------------------------
      real      trad            ! Time window radius for outputting reports (if l_otw=T)
      logical   l_otw           ! T=eliminate reports outside cycle time window radius (trad)
     +,         l_nhonly        ! T=filter out obs outside tropics and Northern Hemisphere
     +,         l_allev_pf      ! T=process latest (likely NRLACQC) events plus all prior
                                !   events into profiles PREPBUFR-like file
                                !   **CAUTION: More complete option, but will make code take
                                !              longer to run!!!
                                ! F=process ONLY latest (likely NRLACQC) events into profiles
                                !   PREPBUFR-like file
                                !
                                ! Note : All pre-existing events plus latest (likely NRLACQC)
                                !        events are always encoded into full PREPBUFR file)

     +,         l_prof1lvl      ! T=encode merged single(flight)-level aircraft reports with
                                !   NRLACQC events that are not part of any profile into
                                !   PREPBUFR-like file, along with merged profiles from
                                !   aircraft ascents and descents
                                !   **CAUTION: Will make code take a bit longer to run!!
                                ! F=do not encode merged single(flight)-level aircraft
                                !   reports with NRLACQC events into PREPBUFR-like file -
                                !   only merged profiles from aircraft ascents and descents
                                !   will be encoded into this file
     +,         l_mandlvl       ! T=interpolate to mandatory levels in profile generation
                                ! F=do not interpolate to mandatory levels in profile
                                !   generation
     +,         tsplines        ! T=use tension-splines for aircraft vertical velocity
                                !   calculation
                                ! F=use finite-differencing for aircraft vertical velocity
                                !   calculation


c Logicals controlling processing (not read in from namelist in main program)
c ---------------------------------------------------------------------------
      logical   l_operational   ! Run program  in operational mode if true

c Counters
c --------
      integer   elim_knt(3)     ! Count of reports eliminated and kept
                                !   1 - # of merged reports outside time radius (prior to any
                                !         geographical domain checking)
                                !   2 - # of merged reports outside geographical domain (had
                                !         passed time window radius check)
                                !   3 - # of merged reports passing both time window radius
                                !         and geographical domain checks and thus retained
                                !         for eventual processing into PREPBUFR-like profiles
                                !         file

c Variables used to write data to output PREPBUFR-like file in sorted order
c -------------------------------------------------------------------------
      character*8 msgtyp2wrt       ! BUFR message type to write to output PREPBUFR-like file

      integer      mxlv            ! maximum number of report levels allowed in aircraft
                                   !  profiles
      character*6  cmxlv           ! character form of mxlv

      integer mxnmev               ! maximum number of events allowed in stack

      integer lvlsinprof(mxlv)     ! array containing a list of pressure levels that are
                                   !  present in the current profile

      logical l_newprofile         ! T = start a new profile

      integer nprofiles	           ! number of "profile" reports identified
     +,       nprofiles_encoded    ! number of "profile" reports actually encoded into
                                   !  PREPBUFR-like file 
     +,       mxe4prof             ! maximum number of events in a single-level merged report
                                   !  (i.e., the maximum amongst the number of pressure,
                                   !  moisture, temperature, altitude, u/v wind and dir/speed
                                   !  wind events)
     +,       nlvinprof	           ! number of levels in a profile
     +,       nlvinprof_last       ! index for level number of last level (for duplicate
                                   !  pressure level removal option #1)
     +,       nlvinprof_temp       ! temporary level number index needed for duplicate
                                   !  pressure level removal

      character*8   tail_curr,     ! aircraft registration (tail) number of current report
     +              tail_prev      ! aircraft registration (tail) number of previous report

      character*9   flt_curr,      ! flight number of current report
     +              flt_prev       ! flight number of previous report

      real          elv_curr,      ! elevation of current report
     +              elv_prev       ! elevation of previous report

      integer       idt_curr,      ! time of current report
     +	            idt_prev       ! time of previous report

      integer       idz_curr,      ! altitude of current report
     +              idz_prev       ! altitude of previous report

      real*8   hdr2wrt(15)              ! array used to pass header info to subroutine
                                        !  sub2mem_mer 
      character*8 c_sid                 ! SID from PREPBUFR file = Site ID
      equivalence(c_sid,hdr2wrt(1))

      real*8   drinfo_accum(3,mxlv)	! array used to accumulate drift info across profile
                                        !  levels

      real*8   acft_seq_accum(2,mxlv)   ! array used to accumulate ACFT_SEQ (PCAT -
                                        !  temperature precision, POAF - phase of flight)
                                        !  info across profile levels
     +,	       mstq_accum(1,mxlv)       ! array used to accumulate moisture QC marks across
                                        !  profile levels
     +,	       cat_accum(1,mxlv)        ! array used to accumulate level category markers
                                        !  across profile levels
     +,	       elv_accum(1,mxlv)        ! array used to accumulate elevation across profile
                                        !  levels
     +,	       rpt_accum(1,mxlv)        ! array used to accumulate reported obs time across
                                        !  profile levels
     +,	       tcor_accum(1,mxlv)       ! array used to accumulate time correction indicator
                                        !  across profile levels
     +,	       rct_accum(1,mxlv)        ! array used to accumulate receipt time across
                                        !  profile levels

      real*8   pevn_accum(4,mxlv,mxnmev)! array used to accumulate pressure data/events for a
                                        !  single profile, across profile levels
     +,	       pbg_accum(3,mxlv)        ! array used to accumulate pressure background info
                                        !  (POE, PFC, PFCMOD) for a single profile, across
                                        !  profile levels
     +,        ppp_accum(3,mxlv)        ! array used to accumulate pressure post-processing
                                        !  info (PAN, PCL, PCS) for a single profile, across
                                        !  profile levels

      real*8   qevn_accum(4,mxlv,mxnmev)! array used to accumulate moisture data/events for a
                                        !  single profile, across profile levels
     +,        qbg_accum(3,mxlv)        ! array used to accumulate moisture background info
                                        !  (QOE, QFC, QFCMOD) for a single profile, across
                                        !  profile levels
     +,        qpp_accum(3,mxlv)        ! array used to accumulate moisture post-processing
                                        !  info (QAN, QCL, QCS) for a single profile, across
                                        !  profile levels

      real*8   tevn_accum(4,mxlv,mxnmev)! array used to accumulate temperature data/events
                                        !  for a single profile, across profile levels
     +,	       tbg_accum(3,mxlv)        ! array used to accumulate temperature background
                                        !  info (TOE, TFC, TFCMOD) for a single profile,
                                        !  across profile levels
     +,        tpp_accum(3,mxlv)	! array used to accumulate temperature post-
                                        !  processing info (TAN, TCL, TCS) for a single
                                        !  profile, across profile levels

      real*8   zevn_accum(4,mxlv,mxnmev)! array used to accumulate altitude data/events for a
                                        !  single profile, across profile levels
     +,        zbg_accum(3,mxlv)        ! array used to accumulate altitude background info
                                        !  (ZOE, ZFC, ZFCMOD) for a single profile, across
                                        !  profile levels
     +,        zpp_accum(3,mxlv)        ! array used to accumulate altitude post-processing
                                        !  info (ZAN, ZCL, ZCS) for a single profile, across
                                        !  profile levels

      real*8   wuvevn_accum(5,mxlv,mxnmev)! array used to accumulate wind data/events (u/v
                                          !  components) for a single profile, across profile
                                          !  levels
     +,        wuvbg_accum(5,mxlv)      ! array used to accumulate wind background info (WOE,
                                        !  UFC, VFC, UFCMOD, VFCMOD)  for a single profile,
                                        !  across profile levels
     +,        wuvpp_accum(6,mxlv)      ! array used to accumulate wind post-processing info
                                        !  (UAN, VAN, UCL, VCL, UCS, VCS) for a single
                                        !  profile, across profile levels

      real*8   wdsevn_accum(5,mxlv,mxnmev)! array used to accumulate wind data/events
                                          !  (direction/speed) for a single profile, across
                                          !  profile levels

      character*11 c_qc_accum(mxlv)    ! array used to accumulate NRLACQC quality information
                                       !  on individual obs in a profile, across profile
                                       !  levels

c Summary counters
c ----------------
      integer num_events_prof          ! total number of events on an ob, across all levels,
                                       !  across all reports, written in the PREPBUFR-like
                                       !  (profiles) file (this value is the same for each
                                       !  ob type)

c Mandatory levels settings
c -------------------------
      integer maxmandlvls              ! maxmum number of mandatory pressure levels to
                                       !  consider for aircraft profiles
      parameter(maxmandlvls = 9)

      integer mandlvls(maxmandlvls)    ! list of mandatory pressure levels to consider for
                                       !  aircraft profiles

      data mandlvls/1000,1500,2000,3000,4000,5000,7000,8500,10000/

c Variables used to hold original aircraft data read from input PREPBUFR file - necessary for
c  carrying data through program so that it can be later written to output PREPBUFR-like
c  profiles file from memory instead of going back to input PREPBUFR file and re-reading that
c  file before adding any NRLACQC events
c -------------------------------------------------------------------------------------------
      integer nevents(max_reps,6)     ! array tracking number of events for variables for
                                      ! each report:
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
     +,       ddo_ev(max_reps,mxnmev) ! DDO values for each report, including all events
     +,       ffo_ev(max_reps,mxnmev) ! FFO values for each report, including all events
     +,       dfq_ev(max_reps,mxnmev) ! DFQ values for each report, including all events
     +,       dfp_ev(max_reps,mxnmev) ! DFP values for each report, including all events
     +,       dfr_ev(max_reps,mxnmev) ! DFR values for each report, including all events

     +,       hdr(max_reps,15)        ! SID XOB YOB DHR ELV TYP T29 TSB ITP SQN PROCN RPT
                                      !  TCOR RSRD EXRSRD
     +,       acid(max_reps)          ! ACID
     +,       rct(max_reps)           ! RCT
     +,       mstq(max_reps)          ! MSTQ
     +,       cat(max_reps)           ! CAT

     +,       pbg(max_reps,3)         ! POE PFC PFCMOD
     +,       zbg(max_reps,3)         ! ZOE ZFC ZFCMOD
     +,       tbg(max_reps,3)         ! TOE TFC TFCMOD
     +,       qbg(max_reps,3)         ! QOE QFC QFCMOD
     +,       wbg(max_reps,5)         ! WOE UFC VFC UFCMOD VFCMOD

     +,       ppp(max_reps,3)         ! PAN PCL PCS
     +,       zpp(max_reps,3)         ! ZAN ZCL ZCS
     +,       tpp(max_reps,3)         ! TAN TCL TCS
     +,       qpp(max_reps,3)         ! QAN QCL QCS
     +,       wpp(max_reps,6)         ! UAN VAN UCL VCL UCS VCS

     +,       drinfo(max_reps,3)      ! XOB YOB DHR
     +,       acft_seq(max_reps,2)    ! PCAT POAF

      real*8  acid_last_profile       ! ACID (aircraft flight number) for last (or only)
                                      !  MDCRS or AMDAR (LATAM only) report in profile (passed
                                      !  into subroutine sub2mem_mer)

      character*9  c_acftid_last_profile  ! aircraft flight number (as processed by NRL QC
                                          !  procesing) for last (or only) report in profile
                                          !  (passed into subroutine sub2mem_mer for printing
                                          !  purposes only)

      character*8  c_acftreg_last_profile ! aircraft tail number (as processed by NRL QC
                                          !  procesing) for last (or only) report in profile
                                          !  (passed into subroutine sub2mem_mer for printing
                                          !  purposes only)

      real    del_time                ! report time difference between two levels, used by
                                      !  profile gross check
     +,       del_hght                ! report time difference between two levels, used by
                                      !  profile gross check
     +,       vvel                    ! vertical velocity between two levels, used by profile
                                      !  gross check

c Misc.
c -----
      integer i_option                ! Duplicate pressure removal option (1 or 2)
     +,       lwr                     ! machine word length in bytes (either 4 or 8)

      real    nrlacqc_pc              ! PREPBUFR program code for the NRLACQC step

ccccc integer iprint                  ! Switch controlling extra diagnostic printout

c *******************************************************************

c Initialize variables
c --------------------

      tail_prev = 'XXXXXXXX'
      flt_prev = 'XXXXXXXXX'
      elv_prev = 99999
      idt_prev = 99999
      idz_prev = 99999

      mxe4prof       = 0
      nlvinprof      = 0
      nlvinprof_last = 0

      lvlsinprof = 99999

      pevn_accum = bmiss 
      pbg_accum  = bmiss 
      ppp_accum  = bmiss 
	
      tevn_accum = bmiss 
      tbg_accum  = bmiss 
      tpp_accum  = bmiss 

      qevn_accum = bmiss 
      qbg_accum  = bmiss 
      qpp_accum  = bmiss 
	
      zevn_accum = bmiss 
      zbg_accum  = bmiss 
      zpp_accum  = bmiss 

      wuvevn_accum = bmiss 
      wuvbg_accum  = bmiss 
      wuvpp_accum  = bmiss 

      wdsevn_accum = bmiss 

      drinfo_accum   = bmiss
      acft_seq_accum = bmiss
      mstq_accum     = bmiss 
      cat_accum      = bmiss 
      elv_accum      = bmiss 
      rpt_accum      = bmiss 
      tcor_accum     = bmiss 
      rct_accum      = bmiss 

      c_qc_accum = 'XXXXXXXXXXX'

      hdr2wrt = bmiss 

      acid_last_profile      = bmiss
      c_acftid_last_profile  = '         '
      c_acftreg_last_profile = '        '

      nprofiles         = 0
      nprofiles_encoded = 0
      mevwrt            = 0 ! DAK: This is a "dummy" variable, not used anywhere.  For some
                            !      reason if one removes this, moves its declaration (look
                            !      above) to any other place in this subr., changes the
                            !      dimension, or does not initialize it as zero (here) the
                            !      CCS XLF compiler can fail under -O3 with debugging turned
                            !      on ("An error occurred during code generation.  The code
                            !      generation return code was 40." "Compilation failed for
                            !      file output_acqc_prof.f."  -- Not sure what might happen
                            !      with ifort compiler on WCOSS

      elim_knt          = 0
      num_events_prof   = 0


c Start subroutine
c ----------------
      write(*,*)
      write(*,*) '***************************'
      write(*,*) 'Welcome to output_acqc_prof'
      call system('date')
      write(*,*) '***************************'
      write(*,*)

      write(*,*)
      write(*,'(" --> Output to PREPBUFR-like file (holding merged QCd",
     +          " aircraft profile rpts & when l_prof1lvl=T single",
     +          "(flight)-level aircraft rpts)")')
      write(*,*)

      if(.not.l_operational) then  ! this is currently invoked because l_operational
                                   !  is hardwired to F for l_ncep=T

c Write merged profile reports and resulting QC decisions to an output file for later perusal
c -------------------------------------------------------------------------------------------

        open(52,file='merged.profile_reports.post_acftobs_qc.sorted',
     +       form='formatted')
        write(52,*)
        write(52,'(" Final listing of all aircraft profile reports in ",
     +             "pseudo-PREPBUFR file after NRLACQC")')
        write(52,'(" -------------------------------------------------",
     +             "----------------------------------")')
        write(52,*)
        write(52,'(" TAMDAR reports here replace characters 1-3 of ",
     +             "manufactured flight # (''000'') with (''TAM'') in ",
     +             "order to create truncated tail # ''TAM'' for ",
     +             "NRLACQC sorting - the PREPBUFR file continues to ",
     +             "encode ''000'' in")')
        write(52,'("  characters 1-3 of manufactured flight # for ",
     +             "TAMDAR (stored as both ''SID'' and ''ACID'')")')

        write(52,*)
        write(52,'(" AIREP and PIREP reports report only a flight # ",
     +             "(manufactured for PIREPs) - a tail # for NRLACQC ",
     +             "sorting is created by truncating the flight # - ",
     +             "the PREPBUFR file will not encode these truncated ",
     +             "tail #''s")')

        write(52,*)
        write(52,'(" All AMDAR reports except LATAM report only a tail",
     +            " # - this is stored as both flight # and tail # for",
     +             " NRLACQC sorting - the PREPBUFR file continues to ",
     +             "encode only tail # (stored in ''SID'')")')
        write(52,*)
        write(52,'(" AMDAR reports from LATAM report both a tail # and",
     +             " a flight # - these are used as reported for ",
     +             "NRLACQC sorting - the PREPBUFR file continues to ",
     +             "encode both tail # and flight # (as ''SID'' and ",
     +             "''ACID'',")')
        write(52,*) 'resp.)'
        write(52,*)
        write(52,'(" MDCRS reports from ARINC report both a tail # and",
     +             " a flight # - these are used as reported for",
     +             " NRLACQC sorting - the PREPBUFR file continues to ",
     +             "encode both tail # and flight # (as ''SID'' and ",
     +             "''ACID'',")')
        write(52,*) 'resp.)'

        write(52,*)
        write(52,3001)
 3001   format(172x,'! _PREPBUFR_QMs_!NRLACQC_REASON_CODE'/
     +         'index flight    tail num itp pf       lat       lon',
     +         '    time  hght   pres temp/evnt spec_h/evnt  uwnd   ',
     +         'vwnd/evnt t-prec !__qc_flag__!rcptm mstq cat  wspd ',
     +         'wdir rtyp         ! Pq Zq Tq Qq Wq!Prc Zrc Trc Qrc ',
     +         'Wrc'/
     +         '----- --------- -------- --- --   -------- --------',
     +         '- ------ ----- ------ --------- ----------- ------  ',
     +         '--------- ------  -----------!----- ---- --- ----- ',
     +         '---- ----         ! -- -- -- -- --!--- --- --- --- ',
     +         '---')
      endif
C--------------------------------------------------------------------------------------------
C  Options for handling duplicate pressures read in for a profile:
C  Option 1: For duplicate pressures read in for a profile, the first duplicate read in is
C            tossed and the second one is kept.  Note: This is how the code originally
C            performed this duplicate check.  Updated logic to make code run faster may cause
C            this option to not always work as expected - not sure.  Also, when debugging is
C            turned on, this option may not compile unless -qhot is added to FFLAGS in
C            makefile (not always the case, however).
C  Option 2: For duplicate pressures read in for a profile, the second duplicate read in is
C            tossed.  This appears to be less problematic than option 1.
C  Currently Option 2 is selected in this code.
      i_option = 2
C--------------------------------------------------------------------------------------------

c Now, loop over NRLACQC arrays and write aircraft type reports to output file in sorted
c  order as specified by sortidx
c --------------------------------------------------------------------------------------
      loop1: do i = 1,nrpts4QC_pre
        j = sortidx(i)

ccccc   print 4077, j,acid(j),rct(j)
c4077   format(1x,'for j = ',i6,', acid(j) = ',a8,', rct(j) = ',f10.3)

c Check to be sure the report is within the requested time window (defined by namelist switch
c  trad) and it is within the requested geographical domain (here north of 20S latitude, if
c  namelist switch l_nhonly is true)
c  {Note: alat(j) and idt(j) will have already been updated with rehabilitated values if
c         NRLACQC performed this task, so these checks will be more precise ...}
c -------------------------------------------------------------------------------------------

        if(l_otw) then ! check if report is outside time window (prior to any geographical
                       !  domain checking)
          if(idt(j).lt.-trad*3600..or.idt(j).gt.trad*3600.) then
	    elim_knt(1) = elim_knt(1) + 1
            cycle ! skip processing of this report, move on to next report
          endif
        endif

        if(l_nhonly) then ! if report passed time window radius check, then check to see if
                          !  it is outside geographical domain (i.e., south of 20S)
          if(alat(j).lt.-20.0) then
	    elim_knt(2) = elim_knt(2) + 1
            cycle ! skip processing of this report, move on to next report
          endif
        endif

c If this point is reached, the report is not to be eliminated
c ------------------------------------------------------------
ccccc   print *, 'keep this report!'

c Counter for number of merged reports kept
        elim_knt(3) = elim_knt(3) + 1

c Check if this ob should be included in current profile
c ------------------------------------------------------
        l_newprofile = .false.

        tail_curr = c_acftreg(j)
        flt_curr  = c_acftid(j)
        elv_curr  = ht_ft(j)
        idt_curr  = idt(j)
        idz_curr  = zob_ev(j,nevents(j,4))

        if(tail_curr.eq.tail_prev .and.flt_curr.eq.flt_prev) then   ! report may be part of current profile; need to make
                                        !  sure it's not the start of a separate profile from
                                        !  the same aircraft and flight

ccccc     iprint=0
ccccc     if(tail_curr.eq.'MSHWUURA ') iprint=1
ccccc     if(flt_curr.eq.'AFZA41   ') iprint=1

c By this point, reports have been sorted with a sort key of:
c
c  type//phase-of-flight//tail//flight//time//elevation//lat//lon
c
c  (see csort_wbad in main program) - phase-of-flight in c_qc(11:11) indicates that the
c  report is indeed part of an ascent or descent - if tail and flight number are equal, check
c  for elev(n)<= for ascents (c_qc(11:11) = a or A) and for elev(n) >=elev(n-1) for descents
c  (c_qc(11:11) = d or D) -- reaching these elevation criteria will signal the start of a new
c  profile
c -------------------------------------------------------------------------------------------
          if(
     +	   ((c_qc(j)(11:11).eq.'a' .or.
     +	     c_qc(j)(11:11).eq.'A') .and.
     +	    (elv_curr .lt. elv_prev)) .or.  ! new profile from the same aircraft/flight
     +       c_qc(j)(11:11).eq.'I'    .or.  !  perhaps the aircraft made a stop
     +       c_qc(j)(11:11).eq.'L'    .or.  !  somewhere and the flight number didn't
     +       c_qc(j)(11:11).eq.'N'    .or.  !  change - or, this report is isolated (I),
     +       c_qc(j)(11:11).eq.'U'    .or.  !  level (L), or its ascent/descent status is
     +	     c_qc(j)(11:11).eq.'-'    .or.  !  unknown (U).  Need to close off the
     +	   ((c_qc(j)(11:11).eq.'d' .or.     !  current profile, write it to output,
     +	     c_qc(j)(11:11).eq.'D') .and.   !  and start a new one
     +	    (elv_curr .gt. elv_prev))     
     +	    ) then

ccccc       if(iprint.eq.1)  print *,'new profile - same flight number'

	    l_newprofile = .true.
	    nprofiles = nprofiles + 1

          else  

ccccc       if(iprint.eq.1)  print *,'keep accumulating'

                 ! keep accumulating data into the current profile

c Perform a gross check on the report times of adjacent levels in the "profile" ...
C  Stop accumulating levels and start a new profile on this level if either:
C     1) The report time difference between this level and the previous level is > 1500 sec
C     2) The report time difference between this level and the previous level is > 1000 sec
C        and </= 1500 sec and the calculated vertical velocity magnitude is < 1.0 m/s {where
C        vertical velocity magnitude = abs(altitude this level minus altitude previous level)
C        divided by abs(time this level minus time previous level)}

            if(abs(idt_curr-idt_prev).gt.1000) then
              if(abs(idt_curr-idt_prev).le.1500) then
                del_time = abs(idt_curr-idt_prev)
                del_hght = abs(idz_curr-idz_prev)
                vvel = del_hght/del_time
                if(vvel.lt.1.00) then
                  print 66, idt_curr,idt_prev,tail_curr,flt_curr,j,
     +                      sortidx(i-1),idz_curr,idz_prev,vvel
   66 format(' Gross time chk exceeded (start new profile): idt_curr ',
     + 'idt_prev tail_curr flt_curr index_curr index_prev idz_curr ',
     + 'idz_prev vvel(m/s)'/48x,i6,3x,i6,2x,a8,2x,a9,2x,i6,5x,i6,4x,i5,
     + 4x,i5,3x,f7.3/)
	          l_newprofile = .true.
	          nprofiles = nprofiles + 1
                endif
              else
                print 67, idt_curr,idt_prev,tail_curr,flt_curr,j,
     +                    sortidx(i-1)
   67 format(' Gross time chk exceeded (start new profile): idt_curr ',
     + 'idt_prev tail_curr flt_curr index_curr index_prev'/,48x,i6,3x,
     + i6,2x,a8,2x,a9,2x,i6,5x,i6/)
	        l_newprofile = .true.
	        nprofiles = nprofiles + 1
              endif
            endif

          endif

        else ! tail, flights not equal, start a new profile

ccccc     if(iprint.eq.1)  print *,'new profile - new flight number'


	  l_newprofile = .true.
	  nprofiles = nprofiles + 1

        endif ! check if tail, flights equal for current and previous reports

	if(l_newprofile.and.nprofiles.gt.1) then ! need to close off current profile and
                                                 !  write to output
          if(l_prof1lvl.or.nlvinprof.gt.1) then

c Open message if necessary
c -------------------------
            call openmb(proflun,msgtyp2wrt,icdtg_an)

c Store contents of the current observation (profile or single/flight-level) into BUFRLIB
c  memory via subroutine sub2mem_mer
c ---------------------------------------------------------------------------------------

ccccc       if(iprint.eq.1)  print *,'call sub2mem_mer - 1st location'

ccccc       print 4078, sortidx(i-1),acid(sortidx(i-1)),acid_last_profile
c4078       format(1x,'1-call sub2mem_mer, last report j-1 = ',i6,
ccccc+             ', acid(j-1) = ',a8,', acid_last_profile = ',a8)
ccccc       if(iprint.eq.1)
ccccc+       print *, 'rct_accum: ',rct_accum(1,1),rct_accum(1,2),
ccccc+                rct_accum(1,3),rct_accum(1,4),
ccccc+                rct_accum(1,5),rct_accum(1,6),
ccccc+                rct_accum(1,7),rct_accum(1,8),
ccccc+                rct_accum(1,9),rct_accum(1,10),
ccccc+                rct_accum(1,11),rct_accum(1,12)

            call sub2mem_mer(proflun,bmiss,mxlv,mxnmev,maxmandlvls,
     +                       mandlvls,msgtyp2wrt,hdr2wrt,
     +	                     acid_last_profile, ! use ACID of last (or only) report in profile
     +	                     c_acftid_last_profile, ! use aircraft flight # (from NRLACQC) of
                                                    ! last (or only) report in profile
     +                       c_acftreg_last_profile,! use aircraft tail # (from NRLACQC) of
                                                    ! last (or only) report in profile
     +                       rct_accum,drinfo_accum,acft_seq_accum,
     +                       mstq_accum,cat_accum,elv_accum,rpt_accum,
     +                       tcor_accum,
     +                       pevn_accum,pbg_accum,ppp_accum,
     +                       qevn_accum,qbg_accum,qpp_accum,
     +                       tevn_accum,tbg_accum,tpp_accum,
     +                       zevn_accum,zbg_accum,zpp_accum,
     +                       wuvevn_accum,wuvbg_accum,wuvpp_accum,
     +                       wdsevn_accum,mxe4prof,c_qc_accum,
     +                       num_events_prof,lvlsinprof,nlvinprof,
     +                       nrlacqc_pc,l_mandlvl,tsplines,
     +                       l_operational,lwr)

c Write the current profile to output
c -----------------------------------
            if(hdr2wrt(6).gt.399.or.l_prof1lvl) then  ! sometimes reports with nlvinprof > 1
                                                      !  still come back as single-level
                                                      !  reports - this check keeps them
                                                      !  out of PREPBUFR-like file when
                                                      !  l_prof1lvl=F
              nprofiles_encoded = nprofiles_encoded + 1

ccccc         if(iprint.eq.1)  print *,'call writsb - 1st location'

              call writsb(proflun)
            endif
          endif

c Clear out accumulation arrays and start over with clear arrays for next profile
c -------------------------------------------------------------------------------
          nlvinprof      = 0
          nlvinprof_last = 0

          lvlsinprof = 99999

          pevn_accum = bmiss 
          pbg_accum  = bmiss 
          ppp_accum  = bmiss 

          qevn_accum = bmiss 
          qbg_accum  = bmiss 
          qpp_accum  = bmiss 

          tevn_accum = bmiss 
          tbg_accum  = bmiss 
          tpp_accum  = bmiss 

          zevn_accum = bmiss 
          zbg_accum  = bmiss 
          zpp_accum  = bmiss 

    	  wuvevn_accum = bmiss 
          wuvbg_accum  = bmiss 
          wuvpp_accum  = bmiss

          wdsevn_accum = bmiss 

          drinfo_accum   = bmiss 
          acft_seq_accum = bmiss
          mstq_accum     = bmiss 
          cat_accum      = bmiss 
          elv_accum      = bmiss
          rpt_accum      = bmiss
          tcor_accum     = bmiss
          rct_accum      = bmiss

          c_qc_accum = 'XXXXXXXXXXX'

          hdr2wrt = bmiss 

	  mxe4prof = 0

	endif ! l_newprofile

c Determine message date and type for output PREPBUFR-like file
c -------------------------------------------------------------
	read(cdtg_an,'(i10.10)') icdtg_an
        if(mod(int(hdr(j,6)),100).eq.33) then
          msgtyp2wrt = 'AIRCAR'
        else
          msgtyp2wrt = 'AIRCFT'
        endif

        if(i_option.eq.1) then
          nlvinprof = nlvinprof_last + 1
        else
          nlvinprof = nlvinprof + 1
        endif

        if(nlvinprof.gt.mxlv) then
C.......................................................................
C There are more levels in profile than "mxlv" -- do not process any more levels
C ------------------------------------------------------------------------------
          print 53, mxlv,mxlv
   53 format(/' #####> WARNING: THERE ARE MORE THAN ',I6,' LEVELS IN ',
     + 'THIS PROFILE -- WILL CONTINUE ON PROCESSING ONLY ',I6,' LEVELS',
     + ' FOR THIS PROFILE'/)
          write(cmxlv,'(i6)') mxlv
          call system('[ -n "$jlogfile" ] && $DATA/postmsg'//
     +     ' "$jlogfile" "***WARNING:'//cmxlv//' AIRCRAFT PROFILE '//
     +     'LEVEL LIMIT EXCEEDED IN PREPOBS_PREPACQC, ONLY '//
     +     cmxlv//' LEVELS PROCESSED"')
          exit loop1
C.......................................................................
        endif

c Subroutine sub2mem_um will update events in memory for this single-level "merged" report -
c  upon output the *_ev arrays will contain the events generated from the NRLACQC decisions
c ------------------------------------------------------------------------------------------

ccccc   if(iprint.eq.1)  print *,'call sub2mem_um'

	call sub2mem_um(c_qc(j),max_reps,mxnmev,j,nevents,
     +                  pob_ev,pqm_ev,ppc_ev,prc_ev,
     +                  zob_ev,zqm_ev,zpc_ev,zrc_ev,
     +                  tob_ev,tqm_ev,tpc_ev,trc_ev,
     +                  qob_ev,qqm_ev,qpc_ev,qrc_ev,
     +                  uob_ev,vob_ev,wqm_ev,wpc_ev,wrc_ev,
     +                  nrlacqc_pc,l_allev_pf)

        mxe4prof = max(mxe4prof,nevents(j,1),nevents(j,2),nevents(j,3),
     +                          nevents(j,4),nevents(j,5),nevents(j,6))


c Gather data into profile arrays before actually writing profile to output file
c ------------------------------------------------------------------------------

c Get header data
c ---------------
        hdr2wrt(:) = hdr(j,:) 

ccccc   if(iprint.eq.1)  print *,'HDR2WRT: ',hdr2wrt

c ------------------------------------------------------------
c Store pressure events, background data, analysis, climo data
c ------------------------------------------------------------
        lvlsinprof(nlvinprof) = int(pob_ev(j,nevents(j,1))*10)
        if(i_option.eq.1) nlvinprof_last = nlvinprof
        nlvinprof_temp = 0

ccccc   if(iprint.eq.1)  print *,'nlvinprof = ',nlvinprof

        if(nlvinprof.gt.1) then
          do ii=1,nlvinprof-1

ccccc       if(iprint.eq.1)  print *,'new ii: lvlsinprof(nlvinprof), ',
ccccc+                               'lvlsinprof(ii): ',
ccccc+                               lvlsinprof(nlvinprof),
ccccc+                               lvlsinprof(ii)

            if(lvlsinprof(nlvinprof).eq.lvlsinprof(ii)) then

ccccc         if(i_option.eq.1) then
ccccc           print'(" WARNING: Pressure level ",I0," was previously",
ccccc+                 " filled for this report - index ",I0," refill ",
ccccc+                 "with this one !!")', lvlsinprof(nlvinprof),ii
ccccc         else
ccccc           print'(" WARNING: Pressure level ",I0," was previously",
ccccc+                 " filled for this report - index ',I0,'keep it,",
ccccc+                 " toss this one !!")', lvlsinprof(nlvinprof),ii
ccccc         endif
ccccc         print *, hdr2wrt

              nlvinprof_temp = ii
              exit
            endif
          enddo
        endif
        if(nlvinprof_temp.gt.0) then
          if(i_option.eq.1) then
            nlvinprof_last = nlvinprof - 1
            nlvinprof = nlvinprof_temp ! DAK: W/ DEBUG ON **MAY** NOT COMPFILE UNLESS ADD
                                       !      -qhot
          else
            nlvinprof = nlvinprof - 1
            cycle ! skip processing
          endif
        endif

        if(l_prof1lvl.or.nlvinprof.gt.1) then
          acid_last_profile = acid(j)
          c_acftid_last_profile = c_acftid(j)
          c_acftreg_last_profile = c_acftreg(j)
        endif

c Store non-NRLACQC events in the "profile" arrays before adding any new events (done later
c  in subroutine sub2mem_mer)
c -----------------------------------------------------------------------------------------
        pevn_accum(1,nlvinprof,1:nevents(j,1))= pob_ev(j,1:nevents(j,1))
	pevn_accum(2,nlvinprof,1:nevents(j,1))= pqm_ev(j,1:nevents(j,1))
	pevn_accum(3,nlvinprof,1:nevents(j,1))= ppc_ev(j,1:nevents(j,1))
	pevn_accum(4,nlvinprof,1:nevents(j,1))= prc_ev(j,1:nevents(j,1))

c Background info
c ---------------
	pbg_accum(:,nlvinprof) = pbg(j,:) ! single-level upon input

c Post processing info
c --------------------
        ppp_accum(:,nlvinprof) = ppp(j,:) ! single-level upon input

c ------------------------------------------------------------
c Store altitude events, background data, analysis, climo data
c ------------------------------------------------------------

c Store non-NRLACQC events in the "profile" arrays before adding any new events (done later
c  in subroutine sub2mem_mer)
c -----------------------------------------------------------------------------------------
        zevn_accum(1,nlvinprof,1:nevents(j,4))= zob_ev(j,1:nevents(j,4))
        zevn_accum(2,nlvinprof,1:nevents(j,4))= zqm_ev(j,1:nevents(j,4))
        zevn_accum(3,nlvinprof,1:nevents(j,4))= zpc_ev(j,1:nevents(j,4))
        zevn_accum(4,nlvinprof,1:nevents(j,4))= zrc_ev(j,1:nevents(j,4))

c Background info
c ---------------
        zbg_accum(:,nlvinprof) = zbg(j,:) ! single-level upon input

c Post processing info
c --------------------
        zpp_accum(:,nlvinprof) = zpp(j,:) ! single-level upon input

c Get drift data - use XOB YOB DHR for drift coordinates when accumulating data into profiles
c -------------------------------------------------------------------------------------------
	drinfo_accum(:,nlvinprof) = drinfo(j,:)

c Get time correction factor
c --------------------------
        tcor_accum(1,nlvinprof) = hdr(j,13)

c -------------------------------------------------------------------------
c -------------------------------------------------------------------------
c Take into account possible rehabilitation of certain paramters by NRLACQC
c  - these will be written into profiles rather than original values
c  - Note: Right now we do not encode updates to XORG, XCOR, YORG or YCOR
c          into PREPBUFR-like profiles file!!
c -------------------------------------------------------------------------


        if(c_qc(j)(2:2).eq.'R'.or.           ! time reabilitated
     +     c_qc(j)(3:3).eq.'R'.or.           ! latitude reabilitated
     +     c_qc(j)(4:4).eq.'R'.or.           ! longitude reabilitated
     +     c_qc(j)(5:5).eq.'R'.or.           ! pressure/altitude reabilitated
     +     c_qc(j)(6:6).eq.'R'.or.           ! temperature reabilitated
     +     c_qc(j)(5:5).eq.'r') then         ! pressure/altitude reabilitated
          print 61
   61     format(131('v'))

          if(c_qc(j)(2:2).eq.'R') then

c Case where time was rehabiltated by NRLACQC, make note of it
c ------------------------------------------------------------
            print 62, j,hdr(j,1),hdr(j,3),hdr(j,2),hdr(j,4),
     +                nint(hdr(j,5)),c_qc(j)
   62 format(' TIME rehab. (prof): input rpt # ',i6,': id ',a8,', lat ',
     + f9.5,', lon ',f9.5,', dhr ',f10.5,', hght(m)',i6,', NRLQMS "',
     + A11,'"')
            print 63, hdr(j,4),nint(hdr(j,4)*3600.)
   63 format(' INPUT time from PRE-QC PREPBUFR file [DHR,idt(sec)] ',
     + 'is: ',f10.5,i8)
            print 64, idt(j)/3600.,idt(j)
   64 format(' REHAB. (prof) time from  acftobs_qc  [DHR,idt(sec)] ',
     + 'is: ',f10.5,i8,' use this in profile if created')
            hdr2wrt(4) = idt(j)/3600.
            drinfo_accum(3,nlvinprof) = idt(j)/3600.
            hdr2wrt(13) = 3
            tcor_accum(1,nlvinprof) = 3 ! Set time correction indicator (TCOR) to 3
            print 44, tcor_accum(1,nlvinprof)
   44 format('   --> Time correction indicator (TCOR) changed to ',f3.0)
          endif
          if(c_qc(j)(3:3).eq.'R') then

c Case where latitude was rehabiltated by NRLACQC, make note of it
c ----------------------------------------------------------------
            print 72, j,hdr(j,1),hdr(j,3),hdr(j,2),hdr(j,4),
     +                nint(hdr(j,5)),c_qc(j)
   72 format(' LAT  rehab. (prof): input rpt # ',i6,': id ',A8,', lat ',
     + f9.5,', lon ',f9.5,', dhr ',f10.5,', hght(m)',i6,', NRLQMS "',
     + A11,'"')
            print 73, hdr(j,3)
   73 format(' INPUT latitude from PRE-QC PREPBUFR file (YOB) is: ',
     + f9.5)
            print 74, alat(j)
   74 format(' REHAB. (prof) latitude from  acftobs_qc  (YOB) is: ',
     + f9.5,' use this in profile if created')
            hdr2wrt(3) = alat(j)
	    drinfo_accum(2,nlvinprof) = alat(j)
          endif
          if(c_qc(j)(4:4).eq.'R') then

c Case where longitude was rehabiltated by NRLACQC, make note of it
c -----------------------------------------------------------------
            print 82, j,hdr(j,1),hdr(j,3),hdr(j,2),hdr(j,4),
     +                nint(hdr(j,5)),c_qc(j)
   82 format(' LON  rehab. (prof): input rpt # ',i6,': id ',A8,', lat ',
     + f9.5,', lon ',f9.5,', dhr ',f10.5,', hght(m)',i6,', NRLQMS "',
     + A11,'"')
            print 83, hdr(j,2)
   83 format(' INPUT longitude from PRE-QC PREPBUFR file (XOB) is: ',
     + f9.5)
            print 84, alon(j)
   84 format(' REHAB. (prof) longitude from  acftobs_qc  (XOB) is: ',
     + f9.5,' use this in profile if created')
            hdr2wrt(2) = alon(j)
	    drinfo_accum(1,nlvinprof) = alon(j)
          endif
          if(c_qc(j)(5:5).eq.'R'.or.c_qc(j)(5:5).eq.'r') then

c Case where pressure/altitude was rehabiltated by NRLACQC, make note of it
c -------------------------------------------------------------------------
            print 92, j,hdr(j,1),hdr(j,3),hdr(j,2),hdr(j,4),
     +                nint(hdr(j,5)),c_qc(j)
   92 format(' P/A  rehab. (prof): input rpt # ',i6,': id ',A8,', lat ',
     + f9.5,', lon ',f9.5,', dhr ',f10.5,', hght(m)',i6,', NRLQMS "',
     + A11,'"')
            print 93
   93 format(' %%%%%%%%%%'/' %%%%% Currently not accounted for in ',
     + 'output PREPBUFR-like profiles file'/' %%%%%%%%%%')
          endif
          if(c_qc(j)(6:6).eq.'R') then

c Case where temperature was rehabiltated by NRLACQC, make note of it
c -------------------------------------------------------------------
            print 102, j,hdr(j,1),hdr(j,3),hdr(j,2),hdr(j,4),
     +                 nint(hdr(j,5)),c_qc(j)
  102 format(' TMP  rehabilitated: input rpt # ',i6,': id ',A8,', lat ',
     + f9.5,', lon ',f9.5,', dhr ',f10.5,', hght(m)',i6,', NRLQMS "',
     + A11,'"')
            print 93
          endif
          print 65
   65     format(131('^'))
        endif
c -------------------------------------------------------------------------
c -------------------------------------------------------------------------

c Get ACFT_SEQ data
c -----------------
	acft_seq_accum(:,nlvinprof) = acft_seq(j,:)

c Get MSTQ
c --------
        mstq_accum(1,nlvinprof) = mstq(j)

c Get level category, elevation, reported observation time
c --------------------------------------------------------
        cat_accum(1,nlvinprof)  = cat(j)
        elv_accum(1,nlvinprof)  = hdr(j,5)
        rpt_accum(1,nlvinprof)  = hdr(j,12)
        rct_accum(1,nlvinprof)  = rct(j)

c Check for mandatory levels (CAT = 1), present temperatures (CAT = 2), missing temperatures
c  (CAT = 3)

        if(ob_t(j).eq.-9999.) then ! temperature is missing
	  cat_accum(1,nlvinprof) = 3
	else
	  cat_accum(1,nlvinprof) = 2
	endif

c Mandatory level can override other CAT settings

	do k = 1,maxmandlvls
	  if(lvlsinprof(nlvinprof).eq.mandlvls(k)) then
	    cat_accum(1,nlvinprof) = 1
	    exit ! exit do loop
	  endif
	enddo 

c Get NRLACQC quality string for this ob in the profile
c -----------------------------------------------------
	c_qc_accum(nlvinprof) = c_qc(j)

c ----------------------------------------------------------
c Get moisture events, background data, analysis, climo data
c ----------------------------------------------------------

c Store non-NRLACQC events in the "profile" arrays before adding any new events (done later
c  in subroutine sub2mem_mer)
c -----------------------------------------------------------------------------------------
        qevn_accum(1,nlvinprof,1:nevents(j,2))= qob_ev(j,1:nevents(j,2))
        qevn_accum(2,nlvinprof,1:nevents(j,2))= qqm_ev(j,1:nevents(j,2))
        qevn_accum(3,nlvinprof,1:nevents(j,2))= qpc_ev(j,1:nevents(j,2))
        qevn_accum(4,nlvinprof,1:nevents(j,2))= qrc_ev(j,1:nevents(j,2))

c Background info
c ---------------
        qbg_accum(:,nlvinprof) = qbg(j,:) ! single-level upon input

c Post processing info
c --------------------
        qpp_accum(:,nlvinprof) = qpp(j,:) ! single-level upon input

c -------------------------------------------------------------
c Get temperature events, background data, analysis, climo data
c -------------------------------------------------------------

c Store non-NRLACQC events in the "profile" arrays before adding any new events (done later
c  in subroutine sub2mem_mer)
c -----------------------------------------------------------------------------------------
        tevn_accum(1,nlvinprof,1:nevents(j,3))= tob_ev(j,1:nevents(j,3))
        tevn_accum(2,nlvinprof,1:nevents(j,3))= tqm_ev(j,1:nevents(j,3))
        tevn_accum(3,nlvinprof,1:nevents(j,3))= tpc_ev(j,1:nevents(j,3))
        tevn_accum(4,nlvinprof,1:nevents(j,3))= trc_ev(j,1:nevents(j,3))

c Background info
c ---------------
        tbg_accum(:,nlvinprof) = tbg(j,:) ! single-level upon input

c Post processing info
c --------------------
        tpp_accum(:,nlvinprof) = tpp(j,:) ! single-level upon input

c -----------------------------------------------------------------------
c Get wind (u/v components) events, background data, analysis, climo data
c -----------------------------------------------------------------------

c Store non-NRLACQC events in the "profile" arrays before adding any new events (done later
c  in subroutine sub2mem_mer)
c -----------------------------------------------------------------------------------------
        wuvevn_accum(1,nlvinprof,1:nevents(j,5)) =
     +  uob_ev(j,1:nevents(j,5))
        wuvevn_accum(2,nlvinprof,1:nevents(j,5)) =
     +   vob_ev(j,1:nevents(j,5))
        wuvevn_accum(3,nlvinprof,1:nevents(j,5)) =
     +   wqm_ev(j,1:nevents(j,5))
        wuvevn_accum(4,nlvinprof,1:nevents(j,5)) =
     +   wpc_ev(j,1:nevents(j,5))
        wuvevn_accum(5,nlvinprof,1:nevents(j,5)) =
     +   wrc_ev(j,1:nevents(j,5))

c Background info
c ---------------
        wuvbg_accum(:,nlvinprof) = wbg(j,:) ! single-level upon input

c Post Processing info
c --------------------
        wuvpp_accum(:,nlvinprof) = wpp(j,:) ! single-level upon input

c ---------------------------
c Get wind (dir/speed) events
c ---------------------------
        wdsevn_accum(1,nlvinprof,1:nevents(j,6)) =
     +   ddo_ev(j,1:nevents(j,6))
        wdsevn_accum(2,nlvinprof,1:nevents(j,6)) =
     +   ffo_ev(j,1:nevents(j,6))
        wdsevn_accum(3,nlvinprof,1:nevents(j,6)) =
     +   dfq_ev(j,1:nevents(j,6))
        wdsevn_accum(4,nlvinprof,1:nevents(j,6)) =
     +   dfp_ev(j,1:nevents(j,6))
        wdsevn_accum(5,nlvinprof,1:nevents(j,6)) =
     +   dfr_ev(j,1:nevents(j,6))
     
c Set tail_prev, flt_prev, elv_prev, idt_prev for comparison to next report to see if we need
c  to start a new profile - also set idz_prev for possible gross check
c -------------------------------------------------------------------------------------------
        tail_prev = c_acftreg(j)
        flt_prev  = c_acftid(j)
        elv_prev  = ht_ft(j)
        idt_prev  = idt(j)
        idz_prev  = zob_ev(j,nevents(j,4))

c Close loops here
c ----------------
      enddo loop1 ! i=1,nrpts4QC_pre 

      if(l_prof1lvl.or.nlvinprof.gt.1) then

c Close out last remaining profile and write it to output - open message if necessary
c -----------------------------------------------------------------------------------
        call openmb(proflun,msgtyp2wrt,icdtg_an)

c Store contents of the current observation (profile or single/flight-level) into BUFRLIB
c  memory via subroutine sub2mem_mer
c ---------------------------------------------------------------------------------------

ccccc     print 4079, sortidx(i-1),acid(sortidx(i-1)),acid_last_profile
c4079     format(1x,'2-call sub2mem_mer, last report j-1 = ',i6,
ccccc+     ', acid(j-1) = ',a8,', acid_last_profile = ',a8)

        call sub2mem_mer(proflun,bmiss,mxlv,mxnmev,maxmandlvls,mandlvls,
     +                   msgtyp2wrt,hdr2wrt,
     +	                 acid_last_profile, ! use ACID of last (or only) report in profile
     +	                 c_acftid_last_profile, ! use aircraft flight # (from NRLACQC) of
                                                ! last (or only) report in profile
     +                   c_acftreg_last_profile,! use aircraft tail # (from NRLACQC) of last
                                                ! (or only) report in profile
     +                   rct_accum,drinfo_accum,acft_seq_accum,
     +                   mstq_accum,cat_accum,elv_accum,rpt_accum,
     +                   tcor_accum,
     +                   pevn_accum,pbg_accum,ppp_accum,
     +                   qevn_accum,qbg_accum,qpp_accum,
     +                   tevn_accum,tbg_accum,tpp_accum,
     +                   zevn_accum,zbg_accum,zpp_accum,
     +                   wuvevn_accum,wuvbg_accum,wuvpp_accum,
     +                   wdsevn_accum,mxe4prof,c_qc_accum,
     +                   num_events_prof,lvlsinprof,nlvinprof,
     +                   nrlacqc_pc,l_mandlvl,tsplines,
     +                   l_operational,lwr)

c Write the current profile to output
c -----------------------------------
        if(hdr2wrt(6).gt.399.or.l_prof1lvl) then  ! sometimes reports with nlvinprof > 1
                                                  !  still come back as single-level reports
                                                  !  - this check keeps them out of PREPBUFR-
                                                  !  like file when when l_prof1lvl=F
          nprofiles_encoded = nprofiles_encoded + 1
          call writsb(proflun)
        endif
      endif

      if(.not.l_operational) close(52)

c Output counts
c -------------

c Detailed counts of reports eliminated from final PREPBUFR-like file
c -------------------------------------------------------------------
      print *
      print *, '----------------------------------------------------'
      print *, 'Info about merged aircraft reports not encoded into '
      print *, 'output PREPBUFR-like (profiles) file:'
      print *, '----------------------------------------------------'
      print *
      if(l_otw) then
        print 76, trad,elim_knt(1)
   76 format(' Number of merged reports tossed because outside req. ',
     +       'time window radius of',F6.2,'hrs (prior to geographical ',
     +       'domain checking):',i6/)
      else
        print *, 'Time window radius check NOT performed, l_otw=',l_otw,
     +           ' (ZERO reports tossed)'
      endif
      print *
      if(l_nhonly) then
        print'(" Number of merged reports passing time window radius ",
     +         "chk but tossed because outside geographical domain ",
     +         "(i.e., S of 20S lat): ",I0)', elim_knt(2)
      else
        print *, 'Geographical domain check not performed, l_nhonly=',
     +           l_nhonly,' (ZERO reports tossed)'
      endif
      print *
      print *, 'Number of merged reports passing checks and kept: ',
     +         elim_knt(3)
      print *

c Info about PREPBUFR-like files containing merged profile and (maybe) single(flight)-level
c  reports
c -----------------------------------------------------------------------------------------
      print *
      print'(" -------------------------------------------------------",
     +       "-------------------------")'
      print'(" Info about QMs applied to merged mass and wind reports",
     +       " in the PREPBUFR-like file")'
      print'(" -------------------------------------------------------",
     +       "-------------------------")'
      print *
      print'(" Number of merged ""profile"" reports written to output ",
     +       "PREPBUFR-like file = "I0)', nprofiles_encoded
      print *
! DAK: num_events_prof does not seem to be the right number when single level reports are not
!      encoded...
      print'(" Total number of events for an ob type, across all ",
     +       "levels, across all reports, written to output PREPBUFR-",
     +       "like")'
      print'("  (profiles) file = ",I0," (this value is the same for ",
     +       "each ob type)")',  num_events_prof
      print *

      write(*,*)
      write(*,*) '**************************'
      write(*,*) 'output_acqc_prof has ended'
      call system('date')
      write(*,*) '**************************'
      write(*,*)

      return

      end
