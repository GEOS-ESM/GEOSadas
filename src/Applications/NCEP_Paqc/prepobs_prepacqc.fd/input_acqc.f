c$$$  Subprogram Documentation Block
c   BEST VIEWED WITH 94-CHARACTER WIDTH WINDOW
c
c Subprogram: input_acqc
c   Programmer: D. Keyser       Org: NP22       Date: 2016-12-09
c
c Abstract: Reads aircraft reports (mass and wind pieces) out of the input PREPBUFR file (in
c   message types 'AIRCAR' and 'AIRCFT') and stores merged (mass and wind) data into memory
c   (e.g., alat, alon, ht_ft, idt, ob_*, xiv_* and ichk_* arrays) for later use by the NRL QC
c   kernel (acftobs_qc).  Some NCEP data values are translated to NRL standards (e.g., u/v to
c   dir/spd, quality information, and report type).  Also stores merged input "event"
c   information into memory (e.g., nevents, *ob_ev, *qm_ev, *pc_ev, *rc_ev, *pg and *pp
c   arrays) for use when later constructing merged (mass and wind) profile reports in
c   PREPBUFR-like file (if requested, i.e., l_doprofiles=T).
c
c Program History Log:
c 2010-11-15  S. Bender  -- Original Author
c 2012-05-08  D. Keyser  -- Prepared for operational implementation
c 2012-11-20  J. Woollen -- Initial port to WCOSS
c 2013-02-07  D. Keyser  -- Will now store pressure and pressure-altitude only from the first
c                           (mass) piece of a mass/wind piece pair rather than re-store it
c                           again from the second (wind) piece - even though they "should" be
c                           the same in both pieces (see % below for exception), there can be
c                           rare cases when at least pressure-altitude is missing in the wind
c                           piece (due to a bug in PREPDATA where unreasonably-high winds are
c                           set to missing and an "empty" wind piece is still encoded into
c                           PREPBUFR, this can lead to floating point exception errors in the
c                           construction of profiles {note that pressure & pressure-altitude
c                           from reports with only a wind piece will be read since it is the
c                           first (only) piece of the report}: % - there can be cases where
c                           the pressure qualty mark (PQM) is different in the mass piece vs.
c                           the wind piece (e.g., when it is set to 10 for reports near
c                           tropical systems by SYNDATA), so it is better to pick up PQM from
c                           the mass report for use in the merged mass/wind profiles, an added
c                           benefit of this chg; if the total number of merged (mass + wind
c                           piece) aircraft-type reports read in from PREPBUFR file is at
c                           least 90% of maximum allowed, print diagnostic warning message
c                           to production joblog file prior to returning from this subroutine
c 2013-02-07  D. Keyser  -- Final changes to run on WCOSS: use formatted print statements
c                           where previously unformatted print was > 80 characters
c 2014-09-03  D. Keyser  -- If no aircraft reports of any type are read from input PREPBUFR
c                           file, no further processing is performed other than the usual
c                           stdout print summary at the end.
c 2013-10-07  Sienkiewicz -- add initialization for 'nmswd' (for gfortran compile)
c 2016-12-09  D. Keyser  --
c                 - Nomenclature change: replaced "MDCRS/ACARS" with just "MDCRS".
c                 - New LATAM AMDARs contain an encrypted flight number (in addition to a tail
c                   number, all other AMDARs have only a tail number which is copied into
c                   flight number). Read this in and use in QC processing.
c                   BENEFIT: Improves track-checking and other QC for LATAM AMDARs.
c                 - Latitude/longitdue arrays "alat" and "alon" passed out of this subroutine
c                   now double precision. XOB and YOB in PREPBUFR file now scaled to 10**5
c                   (was 10**2) to handle new v7 AMDAR and MDCRS reports which have this
c                   higher precision.
c                   BENEFIT: Retains exact precison here. Improves QC processing.
c                      - Note: QC here can be improved further by changing logic to account
c                              for the increased precision. This needs to be investigated.
c                              For now, location in code where this seems possible is noted by
c                              the spanning comments:
c                      ! vvvv DAK-future change perhaps to account for incr. lat/lon precision
c                      ! ^^^^ DAK-future change perhaps to account for incr. lat/lon precision
c
c Usage: call input_acqc(inlun,max_reps,mxnmev,bmiss,imiss,amiss,
c                        m2ft,mxlv,nrpts4QC,cdtg_an,alat,alon,ht_ft,
c                        idt,c_dtg,itype,phase,t_prcn,c_acftreg,
c                        c_acftid,pres,ob_t,ob_q,ob_dir,ob_spd,
c                        ichk_t,ichk_q,ichk_d,ichk_s,
c                        nchk_t,nchk_q,nchk_d,nchk_s,
c                        xiv_t,xiv_q,xiv_d,xiv_s,
c                        l_minus9C,nevents,hdr,acid,rct,drinfo,
c                        acft_seq,turb1seq,turb2seq,turb3seq,
c                        prewxseq,cloudseq,afic_seq,mstq,cat,rolf,
c                        nnestreps,sqn,procn,
c                        pob_ev,pqm_ev,ppc_ev,prc_ev,pbg,ppp,
c                        zob_ev,zqm_ev,zpc_ev,zrc_ev,zbg,zpp,
c                        tob_ev,tqm_ev,tpc_ev,trc_ev,tbg,tpp,
c                        qob_ev,qqm_ev,qpc_ev,qrc_ev,qbg,qpp,
c                        uob_ev,vob_ev,wqm_ev,wpc_ev,wrc_ev,wbg,wpp,
c                        ddo_ev,ffo_ev,dfq_ev,dfp_ev,dfr_ev,
c                        l_allev_pf)
c
c   Input argument list:
c     inlun        - Unit number for the input pre-PREPACQC PREPBUFR file containing all data
c                    (separate mass/wind pieces)
c     max_reps     - Maximum number of reports accepted by acftobs_qc
c     mxnmev       - Maximum number of events allowed, per variable type
c     bmiss        - BUFRLIB missing value (set in main program)
c     imiss        - NRL integer missing value flag (99999)
c     amiss        - NRL real missing value flag (-9999.)
c     m2ft         - NRL conversion factor to convert meters to feet
c     mxlv         - Maximum number of levels allowed in a report profile
c     l_allev_pf   - Logical whether to process latest (likely NRLACQC) event plus all prior
c                    events (TRUE) or only latest event (FALSE) into profiles PREPBUFR-like
c                    file (if TRUE means read in these pre-existing events here)
c
c   Output argument list:
c     nrpts4QC     - Total number of input merged (mass + wind piece) aircraft-type reports
c                    read in from PREPBUFR file
c     cdtg_an      - Date/analysis time (YYYYMMDDCC)
c     alat         - Array of latitudes for the "merged" reports
c     alon         - Array of longitudes for the "merged" reports
c     ht_ft        - Array of altitudes for the "merged" reports
c     idt          - Array of ob-cycle times for the "merged" reports (in seconds)
c     itype        - Array of aircraft type for the "merged" reports
c     phase        - Array of phase of flight for aircraft for the "merged" reports
c     t_prcn       - Array of temperature precision for the "merged" reports
c     c_acftreg    - Array of aircraft tail numbers for the "merged" reports to later be used
c                    in NRL QC processing
c     c_acftid     - Array of aircraft flight numbers for the "merged" reports to later be
c                    used in NRL QC processing
c     pres         - Array of pressure for the "merged" reports
c     ob_t         - Array of aircraft temperature for the "merged" reports
c     ob_q         - Array of aircraft moisture (specific humidity) for the "merged" reports
c     ob_dir       - Array of aircraft wind direction for the "merged" reports
c     ob_spd       - Array of aircraft wind speed for the "merged" reports
c     ichk_t       - NRL QC flag for temperature ob
c     ichk_q       - NRL QC flag for moisture ob
c     ichk_d       - NRL QC flag for wind direction ob
c     ichk_s       - NRL QC flag for wind speed ob
c     nchk_t       - NCEP PREPBUFR QC flag for temperature ob
c     nchk_q       - NCEP PREPBUFR QC flag for moisture ob
c     nchk_d       - NCEP PREPBUFR QC flag for wind direction ob
c     nchk_s       - NCEP PREPBUFR QC flag for wind speed ob
c     xiv_t        - Array of aircraft temperature innovations (ob-bg) for "merged" reports
c     xiv_q        - Array of aircraft moisture innovations (ob-bg) for "merged" reports
c     xiv_d        - Array of aircraft wind direction innovations (ob-bg) for "merged" reports
c     xiv_s        - Array of aircraft wind speed innovations (ob-bg) for "merged" reports
c     l_minus9C    - Array of logicals denoting aircraft with -9C temperature for "merged"
c                    reports
c     nevents      - Array tracking number of events for all variables (p, q, t, z, u/v,
c                    dir/spd) for "merged" reports
c     hdr          - Array of aircraft report headers info for "merged" reports
c     acid         - Array of aircraft report flight numbers for "merged" MDCRS and AMDAR
c                    (LATAM only) reports (read in from 'ACID' in input PREPBUFR file)
c     rct          - Array of aircraft report receipt times for "merged" reports
c     drinfo       - Array of aircraft "drift" info (just XOB, YOB, DHR right now) for
c                    "merged" reports
c     acft_seq     - Array of temperature precision and phase of flight for aircraft for the
c                    "merged" reports
c     turb1seq     - Array of type 1 aircraft turbulence for the "merged" reports
c     turb2seq     - Array of type 2 aircraft turbulence for the "merged" reports
c     turb3seq     - Array of type 3 aircraft turbulence for the "merged" reports
c     prewxseq     - Array of present weather info for the "merged" reports
c     cloudseq     - Array of cloud info for the "merged" reports
c     afic_seq     - Array of aircraft icing info for the "merged" reports
c     mstq         - Array of aircraft moisture flags for the "merged" reports
c     cat          - Array of PREPBUFR level category values ("CAT") for the "merged" reports
c     rolf         - Aircraft of aircraft roll angle flags for the "merged" reports
c     nnestreps    - Array containing the Number of "nested replications" for turbulence,
c                    present weather, cloud and icing for the "merged" reports
c     sqn          - Array containing the original PREPBUFR mass and wind piece sequence
c                    numbers ("SQN") for the "merged" reports
c     procn        - Array containing the original PREPBUFR mass and wind piece poe process
c                    numbers ("PROCN") for the "merged" reports
c     pob_ev       - Array of pressure event obs for "merged" reports
c     pqm_ev       - Array of pressure event quality marks for "merged" reports
c     ppc_ev       - Array of pressure event program codes for "merged" reports
c     prc_ev       - Array of pressure event reason codes for "merged" reports
c     pbg          - Array of pressure background data for "merged" reports
c     ppp          - Array of pressure post-processing info for "merged" reports
c     zob_ev       - Array of altitude event obs for "merged" reports
c     zqm_ev       - Array of altitude event quality marks for "merged" reports
c     zpc_ev       - Array of altitude event program codes for "merged" reports
c     zrc_ev       - Array of altitude event reason codes for "merged" reports
c     zbg          - Array of altitude background data for "merged" reports
c     zpp          - Array of altitude post-processing info for "merged" reports
c     tob_ev       - Array of temperature event obs for "merged" reports
c     tqm_ev       - Array of temperature event quality marks for "merged" reports
c     tpc_ev       - Array of temperature event program codes for "merged" reports
c     trc_ev       - Array of temperature event reason codes for "merged" reports
c     tbg          - Array of temperature background data "merged" reports
c     tpp          - Array of temperature post-processing info for "merged" reports
c     qob_ev       - Array of moisture event obs for "merged" reports
c     qqm_ev       - Array of moisture event quality marks for "merged" reports
c     qpc_ev       - Array of moisture event program codes for "merged" reports
c     qrc_ev       - Array of moisture event reason codes for "merged" reports
c     qbg          - Array of moisture background data for "merged" reports
c     qpp          - Array of moisture post-processing info for "merged" reports
c     uob_ev       - Array of wind/u-comp event obs for "merged" reports
c     vob_ev       - Array of wind/v-comp event obs for "merged" reports
c     wqm_ev       - Array of wind event quality marks for "merged" reports
c     wpc_ev       - Array of wind event program codes for "merged" reports
c     wrc_ev       - Array of wind event reason codes for "merged" reports
c     wbg          - Array of wind background data for "merged" reports
c     wpp          - Array of wind post-processing info for "merged" reports
c     ddo_ev       - Array of wind direction event obs for "merged" reports
c     ffo_ev       - Array of wind speed event obs for "merged" reports
c     dfq_ev       - Array of wind direction/speed quality marks  for "merged" reports
c     dfp_ev       - Array of wind direction/speed program codes for "merged" reports
c     dfr_ev       - Array of wind direction/speed reason codes for "merged" reports
c
c   Input files:
c     Unit inlun   - PREPBUFR file containing all obs, prior to any processing by this program
c
c   Output files:
c     Unit 06      - Standard output print
c
c   Subprograms called:
c     Unique:    none
c     Library:
c       SYSTEM:  SYSTEM
c       W3NCO:   ERREXIT    W3TAGE     W3MOVDAT
c       W3EMC:   W3FC05
c       BUFRLIB: IREADMG    IREADSB    UFBINT     UFBSEQ     UFBEVN     READNS     IBFMS
c
c   Exit States:
c     Cond =  0 - successful run
c            23 - unexpected return code from readns; problems reading BUFR file
c
c Remarks: Called by main program.
c
c Attributes:
c   Language: FORTRAN 90
c   Machine:  NCEP WCOSS
c
c$$$
      subroutine input_acqc(inlun,max_reps,mxnmev,bmiss,imiss,amiss,
     +                      m2ft,mxlv,nrpts4QC,cdtg_an,alat,alon,ht_ft,
     +                      idt,c_dtg,itype,phase,t_prcn,c_acftreg,
     +                      c_acftid,pres,ob_t,ob_q,ob_dir,ob_spd,
     +                      ichk_t,ichk_q,ichk_d,ichk_s,
     +                      nchk_t,nchk_q,nchk_d,nchk_s,
     +                      xiv_t,xiv_q,xiv_d,xiv_s,
     +                      l_minus9C,nevents,hdr,acid,rct,drinfo,
     +                      acft_seq,turb1seq,turb2seq,turb3seq,
     +                      prewxseq,cloudseq,afic_seq,mstq,cat,rolf,
     +                      nnestreps,sqn,procn,
     +                      pob_ev,pqm_ev,ppc_ev,prc_ev,pbg,ppp,
     +                      zob_ev,zqm_ev,zpc_ev,zrc_ev,zbg,zpp,
     +                      tob_ev,tqm_ev,tpc_ev,trc_ev,tbg,tpp,
     +                      qob_ev,qqm_ev,qpc_ev,qrc_ev,qbg,qpp,
     +                      uob_ev,vob_ev,wqm_ev,wpc_ev,wrc_ev,wbg,wpp,
     +                      ddo_ev,ffo_ev,dfq_ev,dfp_ev,dfr_ev,
     +                      l_allev_pf)

      implicit none

c ------------------------------
c Parameter statements/constants
c ------------------------------
      integer inlun             ! input unit number (for pre-prepacqc PREPBUFR file
                                !  containing all obs)
      integer max_reps	        ! maximum number of input merged (mass + wind piece)
                                !  aircraft-type reports allowed
cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
c replace above with this in event of future switch to dynamic memory allocation

calloc  integer max_reps           ! original number of input merged (mass + wind piece)
calloc                             !  aircraft-type reports (obtained from first pass through
calloc                             !  input PREPBUFR file to get total for array
calloc                             !  allocation should = nrpts4QC)
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      character*6  cmax_reps    ! character form of max_reps
      integer      imiss        ! NRL integer missing value flag
      real         amiss        ! NRL real missing value flag
      real*8       bmiss        ! BUFRLIB missing value (set in main program)
      real         m2ft         ! NRL conversion factor to convert m to ft
      integer      mxlv         ! maximum number of report levels allowed in aircraft
                                !  profiles

c ----------------------
c Declaration statements
c ----------------------

c Variables for BUFRLIB interface
c -------------------------------
      character*8  mesgtype     ! BUFR message type (e.g., 'AIRCFT  ')
      integer      mesgdate     ! date time from BUFR message (YYYYMMDDHH)

c Logicals controlling processing (read in from namelist in main program)
c -----------------------------------------------------------------------
      logical l_allev_pf        ! T=process latest (likely NRLACQC) events plus all prior
                                !   events into profiles PREPBUFR-like file (here means must
                                !   read in these pre-existing events)
                                !   **CAUTION: More complete option, but will make code take
                                !              longer to run!!!
                                ! F=process ONLY latest (likely NRLACQC) events into profiles
                                !   PREPBUFR-like file (here means read in only latest events
                                !   which will likely be written over later by NRLACQC events)
                                !
                                ! Note 1: Hardwired to F if l_doprofiles=F
                                ! Note 2: All pre-existing events plus latest (likely NRLACQC)
                                !         events are always encoded into full PREPBUFR file)

c Indices/counters 
c ----------------
      integer      i,j          ! loop indeces
     +,            invi         ! "inverse" of the i counter

c for BUFR messages:
      integer      nACmsg_tot   ! number of acft-type BUFR messages in input PREPBUFR file

c for BUFR subsets/reports:
      integer      nrptsaircar         ! number of AIRCAR BUFR subsets read from PREPBUFR file
                                       !  (should = nmswd(2,1) + nmswd(2,2))
     +,	           nrptsaircft         ! number of AIRCFT BUFR subsets read from PREPBUFR file
                                       !  (should = nmswd(1,1) + nmswd(1,2))
     +,	           nmswd(2,2)          ! number of ((AIRCFT,AIRCAR),(mass,wind)) BUFR subsets
                                       !  read from PREPBUFR file
     +,	           nrpts_rd            ! total number of aircraft-type BUFR subsets read from
                                       !  PREPBUFR file (should =
                                       !  nmswd(1,1) + nmswd(1,2) + nmswd(2,1) + nmswd(2,2))
     +,	           nrpts4QC            ! total number of input merged (mass + wind piece)
                                       !  aircraft-type reports read in from PREPBUFR file
                                       !  (should = numpairs + numorph)

      integer      numpairs  	       ! number of input merged (mass + wind piece) aircraft-
                                       !  type reports read in from PREPBUFR file where there
                                       !  is BOTH mass and wind data
                                       !  (should = numAIRCFTpairs + numAIRCARpairs)
     +,            numorph             ! number of input merged (mass + wind piece) aircraft-
                                       !  type reports read in from PREPBUFR file where there
                                       !  is either ONLY mass data or only wind data (deemed
                                       !  "orphans", of course in reality there is no merging
                                       !  here) (should = numAIRCFTorph + numAIRCARorph)
     +,            numAIRCFTpairs      ! number of input merged (mass + wind piece) reports
                                       !  read in from AIRCFT BUFR messages in PREPBUFR file
                                       !  where there is BOTH mass and wind data
     +,            numAIRCARpairs      ! number of input merged (mass + wind piece) reports
                                       !  read in from AIRCAR BUFR messages in PREPBUFR file
                                       !  where there is BOTH mass and wind data
     +,            numAIRCFTorph       ! number of input merged (mass + wind piece) reports
                                       !  read in from AIRCFT BUFR messages in PREPBUFR file
                                       !  where there is either ONLY mass data or only wind
                                       !  data (deemed "orphans", of course in reality there
                                       !  is no merging here)
     +,	           numAIRCARorph       ! number of input merged (mass + wind piece) reports
                                       !  read in from AIRCAR BUFR messages in PREPBUFR file
                                       !  where there is either ONLY mass data or only wind
                                       !  data (deemed "orphans", of course in reality there
                                       !  is no merging here)

      integer      nPIREP 	       ! number of input merged (mass + wind piece) reports
                                       !  read in from AIRCFT BUFR messages in PREPBUFR file
                                       !  that are deemed to be "PIREP" reports
     +,	           nAUTOAIREP          ! number of input merged (mass + wind piece) reports
                                       !  read in from AIRCFT BUFR messages in PREPBUFR file
                                       !  that are deemed to be automated AIREP reports
     +,	           nMANAIREP           ! number of input merged (mass + wind piece) reports
                                       !  read in from AIRCFT BUFR messages in PREPBUFR file
                                       !  that are deemed to be Manual AIREP (all "voice")
                                       !  reports
     +,	           nAMDAR              ! number of input merged (mass + wind piece) reports
                                       !  read in from AIRCFT BUFR messages in PREPBUFR file
                                       !  that are deemed to be AMDAR reports (excluding
                                       !  Canadian AMDAR)
     +,	           nAMDARcan           ! number of input merged (mass + wind piece) reports
                                       !  read in from AIRCFT BUFR messages in PREPBUFR file
                                       !  that are deemed to be Canadian AMDAR reports
     +,	           nMDCRS              ! number of input merged (mass + wind piece) reports
                                       !  read in from AIRCAR BUFR messages in PREPBUFR file
                                       !  (all are MDCRS reports)
     +,            nTAMDAR             ! number of input merged (mass + wind piece) reports
                                       !  read in from AIRCFT BUFR messages in PREPBUFR file
                                       !  that are deemed to be TAMDAR reports

c Functions
c ---------
      integer      ireadmg             ! BUFRLIB - for reading messages 
     +,            ireadsb             ! BUFRLIB - for reading subsets
     +,            ibfms               ! BUFRLIB - for testing for missing

c Observation arrays
c ------------------
      character*10   cdtg_an             ! date-time group for analysis (YYYYMMDDCC)
      character*14   c_dtg(max_reps)     ! full date-time group (yyyymmddhhmmss)
      character*8    c_acftreg(max_reps) ! aircraft registration (tail) number (used in NRL
                                         !  QC processing)
      character*9    c_acftid(max_reps)  ! aircraft flight number (used in NRL QC processing)
      real*8         alat(max_reps)    ! latitude
     +,              alon(max_reps)    ! longitude
      real           pres(max_reps)    ! pressure
     +,              ht_ft(max_reps)   ! altitude in feet
     +,              t_prcn(max_reps)  ! temperature precision
     +,              ob_t(max_reps)    ! temperature
     +,              ob_q(max_reps)    ! moisture (specific humidity)
     +,              ob_dir(max_reps)  ! wind direction
     +,              ob_spd(max_reps)  ! wind speed
     +,              xiv_t(max_reps)   ! temperature innovation/increment (ob-bg)
     +,              xiv_q(max_reps)   ! specific humidity innovation/increment (ob-bg)
     +,              xiv_d(max_reps)   ! wind direction innovation/increment (ob-bg)
     +,              xiv_s(max_reps)   ! wind speed innovation/increment (ob-bg)
      integer        itype(max_reps)   ! instrument (aircraft) type 
     +,              idt(max_reps)     ! time in seconds to anal. time (- before, + after)
     +,              ichk_t(max_reps)  ! NRL QC flag for temperature ob
     +,              ichk_q(max_reps)  ! NRL QC flag for specific humidity ob
     +,              ichk_d(max_reps)  ! NRL QC flag for wind direction ob
     +,              ichk_s(max_reps)  ! NRL QC flag for wind speed ob
     +,              nchk_t(max_reps)  ! NCEP QC flag for temperature ob
     +,              nchk_q(max_reps)  ! NCEP QC flag for specific humidity ob
     +,              nchk_d(max_reps)  ! NCEP QC flag for wind direction ob
     +,              nchk_s(max_reps)  ! NCEP QC flag for wind speed ob
     +,              phase(max_reps)   ! phase of flight for aircraft

      logical        l_minus9c(max_reps) ! true for MDCRS -9C temperatures

c Variables for reading numeric data out of BUFR files via BUFRLIB
c ----------------------------------------------------------------
      real*8         arr_8(15,10)      ! array holding BUFR subset values from BUFRLIB call to
                                       !  input PREPBUFR file
      integer        nlev              ! number of report levels returned from BUFRLIB call
     +,              iret              ! return code for call to BUFRLIB routine readns

c Variables for reading character data out of BUFR files w/ BUFRLIB
c -----------------------------------------------------------------
      real*8         c_arr_8           ! real*8 PREPBUFR report id ("SID")
      character*8    charstr           ! character*8 equivalent of c_arr_8

      equivalence(charstr,c_arr_8)

c Variables for reading event values out of BUFR files w/ BUFRLIB
c --------------------------------------- -----------------------
      integer mxevdt                   ! maximum number of events allowed for each ob type
      parameter (mxevdt = 10)

      integer mxnmev                   ! maximum number of events allowed in stack
     +,       mxvt                     ! maximum number of variable types (P, Q, T, Z, U, V)
      parameter (mxvt = 6)

      integer      qms(4)              ! pointers to ichk_[t,q,d,s]
      character*1  QM_types(4)         ! characters for QM variable types
     +                         /'T','Q','D','S'/

      real*8  pqtzuvEV(mxevdt,mxlv,mxnmev,mxvt) ! holds values read from PREPBUFR file
                                                !  (according to type,level,event,variable)

      character*80    EVstr(mxvt)                ! mnemonic string for populating pqtzuvEV
     +                 /'POB PQM PPC PRC PFC PAN CAT', ! pressure
     +                  'QOB QQM QPC QRC QFC QAN CAT', ! moisture
     +                  'TOB TQM TPC TRC TFC TAN CAT', ! temperature
     +                  'ZOB ZQM ZPC ZRC ZFC ZAN CAT', ! altitude
     +                  'UOB WQM WPC WRC UFC UAN CAT', ! u-wind
     +                  'VOB WQM WPC WRC VFC VAN CAT'/ ! v-wind

      real    uob                      ! u-component wind for a single report
     +,       vob                      ! v-component wind for a single report
     +,       ufc                      ! u-component background wind for a single report
     +,       vfc                      ! v-component background wind for a single report
     +,       dir_fc                   ! wind direction background for a single report
     +,       spd_fc                   ! wind speed background for a single report

      integer evknt                    ! counter used when determining number of events per
                                       !  variable type

      real*8  df_arr(5,mxlv,mxnmev)    ! array used to read out wind (dir/spd) events

c Variables for determining whether consecutive reports are mass and wind pieces that belong
c  together
c ------------------------------------------------------------------------------------------
      logical l_massrpt                ! TRUE if report read in from PREPBUFR is a mass piece
     +,       l_windrpt                ! TRUE if report read in from PREPBUFR is a wind piece
     +,       l_match                  ! TRUE if mass and wind reports currently being
                                       !  processed match (they are part of the same total
                                       !  aircraft report)

      real    sqn_current              ! PREPBUFR sequence number ("SQN") of current report
     +,       sqn_next                 ! PREPBUFR sequence number ("SQN") of previous report
     +,       procn_current            ! PREPBUFR poes process number ("PROC") of current
                                       !  report

c Variables for converting idt to YYYYMMDDHHMMSS format (stored in array c_dtg)
c -----------------------------------------------------------------------------
      integer year                     ! year of analysis time
     +,       month                    ! month of analysis time
     +,       day                      ! day of analysis time
     +,       hour                     ! hour of analysis time
     +,       idat(8)                  ! input array for call to w3movdat
     +,       jdat(8)                  ! output array for call to w3movdat
      real    rinc(5)                  ! array containing time increment for w3movdat


c Variables used to hold original aircraft data read from the input PREPBUFR file - necessary
c  for carrying data through program so that it can be written to output profiles PREPBUFR-
c  like file from memory instead of going back to input PREPBUFR file and re-reading that
c  file before adding any QC events resulting from a decision made by the NRL QC routine (not
c  applicable for case of single-level QC'd reports written back to full PREPBUFR file)
c --------------------------------------------------------------------------------------------
      integer nevents(max_reps,6)      ! array tracking number of events for variables for
                                       !  each report:
                                       !   1 - number of pressure events
                                       !   2 - number of specific humidity events
                                       !   3 - number of temperature events
                                       !   4 - number of altitude events
                                       !   5 - number of wind (u/v) events
                                       !   6 - number of wind (direction/speed) events

      integer nnestreps(4,max_reps)    ! number of "nested replications" for TURB3SEQ,
                                       !  PREWXSEQ, CLOUDSEQ, AFIC_SEQ

      integer nrep                     ! number of "nested replications" for TURB3SEQ
                                       !  PREWXSEQ, CLOUDSEQ, AFIC_SEQ prior to setting to
                                       !  nnestreps

      real*8  pob_ev(max_reps,mxnmev)  ! POB values for each report, including all events
     +,       pqm_ev(max_reps,mxnmev)  ! PQM values for each report, including all events
     +,       ppc_ev(max_reps,mxnmev)  ! PPC values for each report, including all events
     +,       prc_ev(max_reps,mxnmev)  ! PRC values for each report, including all events
     +,       zob_ev(max_reps,mxnmev)  ! ZOB values for each report, including all events
     +,       zqm_ev(max_reps,mxnmev)  ! ZQM values for each report, including all events
     +,       zpc_ev(max_reps,mxnmev)  ! ZPC values for each report, including all events
     +,       zrc_ev(max_reps,mxnmev)  ! ZRC values for each report, including all events
     +,       tob_ev(max_reps,mxnmev)  ! TOB values for each report, including all events
     +,       tqm_ev(max_reps,mxnmev)  ! TQM values for each report, including all events
     +,       tpc_ev(max_reps,mxnmev)  ! TPC values for each report, including all events
     +,       trc_ev(max_reps,mxnmev)  ! TRC values for each report, including all events
     +,       qob_ev(max_reps,mxnmev)  ! QOB values for each report, including all events
     +,       qqm_ev(max_reps,mxnmev)  ! QQM values for each report, including all events
     +,       qpc_ev(max_reps,mxnmev)  ! QPC values for each report, including all events
     +,       qrc_ev(max_reps,mxnmev)  ! QRC values for each report, including all events
     +,       uob_ev(max_reps,mxnmev)  ! UOB values for each report, including all events
     +,       vob_ev(max_reps,mxnmev)  ! VOB values for each report, including all events
     +,       wqm_ev(max_reps,mxnmev)  ! WQM values for each report, including all events
     +,       wpc_ev(max_reps,mxnmev)  ! WPC values for each report, including all events
     +,       wrc_ev(max_reps,mxnmev)  ! WRC values for each report, including all events
     +,       ddo_ev(max_reps,mxnmev)  ! DDO values for each report, including all events
     +,       ffo_ev(max_reps,mxnmev)  ! FFO values for each report, including all events
     +,       dfq_ev(max_reps,mxnmev)  ! DFQ values for each report, including all events
     +,       dfp_ev(max_reps,mxnmev)  ! DFP values for each report, including all events
     +,       dfr_ev(max_reps,mxnmev)  ! DFR values for each report, including all events

     +,       hdr(max_reps,15)         ! SID XOB YOB DHR ELV TYP T29 TSB ITP SQN PROCN RPT
                                       !  TCOR RSRD EXRSRD
     +,       acid(max_reps)           ! ACID
     +,       rct(max_reps)            ! RCT

     +,       pbg(max_reps,3)          ! POE PFC PFCMOD
     +,       zbg(max_reps,3)          ! ZOE ZFC ZFCMOD
     +,       tbg(max_reps,3)          ! TOE TFC TFCMOD
     +,       qbg(max_reps,3)          ! QOE QFC QFCMOD
     +,       wbg(max_reps,5)          ! WOE UFC VFC UFCMOD VFCMOD

     +,       ppp(max_reps,3)          ! PAN PCL PCS
     +,       zpp(max_reps,3)          ! ZAN ZCL ZCS
     +,       tpp(max_reps,3)          ! TAN TCL TCS
     +,       qpp(max_reps,3)          ! QAN QCL QCS
     +,       wpp(max_reps,6)          ! UAN VAN UCL VCL UCS VCS

     +,       drinfo(max_reps,3)       ! XOB YOB DHR
     +,       acft_seq(max_reps,2)     ! PCAT POAF

     +,       turb1seq(max_reps)       ! TRBX
     +,       turb2seq(max_reps,4)     ! TRBX10 TRBX21 TRBX32 TRBX43
     +,       turb3seq(3,max_reps,5)   ! DGOT HBOT HTOT
     +,       prewxseq(1,max_reps,5)   ! PRWE
     +,       cloudseq(5,max_reps,5)   ! VSSO CLAM CLTP HOCB HOCT
     +,       afic_seq(3,max_reps,5)   ! AFIC HBOI HTOI 
     +,       mstq(max_reps)           ! MSTQ
     +,       cat(max_reps)            ! CAT
     +,       rolf(max_reps)           ! ROLF

     +,       sqn(max_reps,2)          ! SQN (1=SQN for mass, 2=SQN for wind)
     +,       procn(max_reps,2)        ! PROCN (1=PROCN for mass, 2=PROCN for wind)

c *******************************************************************

c Start subroutine
c ----------------
      write(*,*)
      write(*,*) '**********************'
      write(*,*) 'Welcome to input_acqc.'
      call system('date')
      write(*,*) '**********************'
      write(*,*)

c Input PREPBUFR file is open and ready for reading by BUFRLIB
c ------------------------------------------------------------

      print *, 'Initializing...'

c Initialize observation arrays
c -----------------------------
      ob_t   = amiss
      ob_q   = amiss
      ob_dir = amiss
      ob_spd = amiss
      xiv_t  = amiss
      xiv_q  = amiss
      xiv_d  = amiss
      xiv_s  = amiss

      nchk_t = -9
      nchk_q = -9
      nchk_d = -9
      nchk_s = -9

! vvvv DAK-future change perhaps to account for incr. lat/lon precision
      alat   = amiss
      alon   = amiss
! ^^^^ DAK-future change perhaps to account for incr. lat/lon precision
      pres   = amiss
      ht_ft  = amiss
      itype  = imiss
      idt    = imiss

      c_acftreg = '        '
      c_acftid  = '         '
      c_dtg     = '              '

      nevents = 0

      pob_ev = bmiss
      pqm_ev = bmiss
      ppc_ev = bmiss
      prc_ev = bmiss

      zob_ev = bmiss
      zqm_ev = bmiss
      zpc_ev = bmiss
      zrc_ev = bmiss

      tob_ev = bmiss
      tqm_ev = bmiss
      tpc_ev = bmiss
      trc_ev = bmiss

      qob_ev = bmiss
      qqm_ev = bmiss
      qpc_ev = bmiss
      qrc_ev = bmiss

      uob_ev = bmiss
      vob_ev = bmiss
      wqm_ev = bmiss
      wpc_ev = bmiss
      wrc_ev = bmiss

      ddo_ev = bmiss
      ffo_ev = bmiss
      dfq_ev = bmiss
      dfp_ev = bmiss
      dfr_ev = bmiss

      hdr  = bmiss
      rct  = bmiss
      acid = bmiss

      pbg = bmiss
      zbg = bmiss
      qbg = bmiss
      tbg = bmiss
      wbg = bmiss

      ppp = bmiss
      zpp = bmiss
      qpp = bmiss
      tpp = bmiss
      wpp = bmiss

      drinfo   = bmiss
      turb1seq = bmiss
      turb2seq = bmiss
      turb3seq = bmiss
      prewxseq = bmiss
      cloudseq = bmiss
      afic_seq = bmiss
      mstq     = bmiss
      cat      = bmiss
      rolf     = bmiss

      sqn   = 999999
      procn = 999999

      nnestreps = 0

      l_minus9C = .false.

      print *, 'Done initializing arrays...'

c Initialize counters
c -------------------
      nACmsg_tot = 0
      numpairs   = 0
      numorph    = 0

      nrptsaircar = 0
      nrptsaircft = 0

      nrpts_rd = 0
      nrpts4QC = 0

      numAIRCFTpairs = 0
      numAIRCARpairs = 0
      numAIRCFTorph  = 0
      numAIRCARorph  = 0

      nPIREP     = 0
      nAUTOAIREP = 0
      nMANAIREP  = 0
      nAMDAR     = 0
      nAMDARcan  = 0
      nMDCRS     = 0
      nTAMDAR    = 0

      nmswd = 0

      print *, 'Done initializing counters...'
	
c Initialize logicals
c -------------------
      l_massrpt = .false.
      l_windrpt = .false.

      l_match   = .false.

      print *, 'Done initializing logicals...'

c Read data from pre-QC PREPBUFR file
c -----------------------------------
      write(*,*) 'Beginning data read!'

c Start reading messages
c ----------------------
      loop2: do while(ireadmg(inlun,mesgtype,mesgdate).eq.0)

c Only consider reports from messages with type 'AIRCFT' or 'AIRCAR'
c ------------------------------------------------------------------
        if(mesgtype.eq.'AIRCFT'.or.mesgtype.eq.'AIRCAR') then

c Update counters of messages read in and considered
c --------------------------------------------------
          nACmsg_tot = nACmsg_tot + 1

c The date in all NCEP PREPBUFR messages is the date/cycle time - use this for the variable
c  cdtg_an - no need to read in the cycle time from std input
c -----------------------------------------------------------------------------------------
          if(nACmsg_tot.eq.1) then ! obtain date/cycle from the first PREPBUFR message read
            write(cdtg_an,'(i10)') mesgdate ! Convert mesgdate to character
            write(*,*) 'Cycle date/time in PREPBUFR messages: ',cdtg_an
          endif

c Using the function ireadsb, read the PREPBUFR subsets/reports, which are separated into
c  mass and wind pieces (NCEP convention) - we will need to pull out values and populate the
c  following arrays, which will be used by the NRL aircraft QC routine:
c    itype, alat, alon, pres, ht_ft,idt, c_dtg, c_acftreg, c_acftid, t_prcn, ob_t, ob_q,
c    ob_dir, ob_spd, ichk_t, ichk_q, ichk_d, ichk_s, l_minus9C
c ------------------------------------------------------------------------------------------
          do while(ireadsb(inlun).eq.0)

 4001  	    continue
            l_match = .false. ! Reset match indicator. second halves of matches are read
                              !  starting at statement 6001

            if(mesgtype.eq.'AIRCAR') then
              nrptsaircar = nrptsaircar + 1
            elseif (mesgtype.eq.'AIRCFT') then
              nrptsaircft = nrptsaircft + 1
            else ! not an aircraft-type message, cycle back to message reading loop & see if
                 !  there are more in file
              print *, '---> MESGTYPE NOT AIRCRAFT TYPE!!!',' "', 
     +                 mesgtype,'"'
              print *, '---> keep looping through messages in case any',
     +                 ' more are in file'
              cycle loop2
            endif
            nrpts_rd = nrpts_rd + 1 ! number of aircraft-type BUFR subsets read from
                                    !  PREPBUFR file 

 5001  	    continue ! will come here if we've just stored the second of a pair or an orphan;
                     !  need to increment index for the report-oriented arrays
            if(nrpts4QC+1.gt.max_reps) then
c.......................................................................
c There are more reports in input file than "max_reps" -- do not process any more reports
c ---------------------------------------------------------------------------------------
              print 53, max_reps,max_reps
   53 format(/' #####> WARNING: THERE ARE MORE THAN ',I6,' AIRCRAFT ',
     + 'REPORTS IN INPUT FILE -- MUST INCREASE SIZE OF PARAMETER NAME',
     + ' "MAX_REPS" - WILL CONTINUE ON PROCESSING ONLY ',I6,' REPORTS'/)
              write(cmax_reps,'(i6)') max_reps
!             call system('[ -n "$jlogfile" ] && $DATA/postmsg'//
!    +         ' "$jlogfile" "***WARNING:'//cmax_reps//' AIRCRAFT '//
!    +         'REPORT LIMIT EXCEEDED IN PREPOBS_PREPACQC, ONLY '//
!    +         cmax_reps//' RPTS PROCESSED"')
              exit loop2
c.......................................................................
            endif
            nrpts4QC = nrpts4QC + 1 ! number of input merged (mass + wind piece) aircraft-
                                    !  type reports read in from PREPBUFR file (for QC code/
                                    !  index for arrays that will be used by acftobs_qc)
 6001       continue ! will come here if we need to check subset n+1 and see if it matches
                     !  the one just stored (treat subset n+1 as a new subset n)

c Pull out the "header" info for subset n, which will either be a mass or wind piece - header
c  mnemonics are: YOB XOB ELV DHR POAF TYP  PCAT, along w/ SID
c -------------------------------------------------------------------------------------------
            arr_8 = bmiss

            call ufbint(inlun,arr_8,15,10,nlev,
     +'YOB XOB ELV DHR TYP T29 TSB ITP SQN PROCN RPT TCOR RSRD EXPRSRD')

cccccc      hdr(nrpts4QC,1) = SID - stored later in code
            hdr(nrpts4QC,2) = arr_8(2,1) ! XOB
            hdr(nrpts4QC,3) = arr_8(1,1) ! YOB
            hdr(nrpts4QC,4) = arr_8(4,1) ! DHR 
            hdr(nrpts4QC,5) = arr_8(3,1) ! ELV 
            hdr(nrpts4QC,6:15) = arr_8(5:14,1) ! TYP T29 TSB ITP SQN PROCN RPT TCOR RSRD
                                               !  EXRSRD

c Drift information
c -----------------
            drinfo(nrpts4QC,1) = arr_8(2,1) ! XOB/XDR
            drinfo(nrpts4QC,2) = arr_8(1,1) ! YOB/YDR
            drinfo(nrpts4QC,3) = arr_8(4,1) ! DHR/HRDR

c Arrays used in NRL QC routine itself
c ------------------------------------
            alat(nrpts4QC) = arr_8(1,1) ! YOB
            alon(nrpts4QC) = arr_8(2,1) ! XOB
            ht_ft(nrpts4QC) = nint(arr_8(3,1)*m2ft) ! ELV in PREPBUFR is in meters NRL QC
                                                    !  wants feet
            idt(nrpts4QC) = nint(arr_8(4,1)*3600.)  ! NRL QC expects idt in sec

c Determine whether this is a temperature or a wind report
c --------------------------------------------------------
            if(int(arr_8(5,1))/100.eq.1) then
              l_massrpt = .true.
              l_windrpt = .false.

              if(mesgtype.eq.'AIRCFT') then
                nmswd(1,1) = nmswd(1,1) + 1
              elseif(mesgtype.eq.'AIRCAR') then
                nmswd(2,1) = nmswd(2,1) + 1
              endif

            elseif(int(arr_8(5,1))/100.eq.2) then
              l_massrpt = .false.
              l_windrpt = .true.

              if(mesgtype.eq.'AIRCFT') then
                nmswd(1,2) = nmswd(1,2) + 1
              elseif(mesgtype.eq.'AIRCAR') then
                nmswd(2,2) = nmswd(2,2) + 1
              endif

            endif

            itype(nrpts4QC) = mod(int(hdr(nrpts4QC,6)),100) 
                             ! 30 = NCEP: AIREP (NRL Manual AIREP/voice)
                             ! 30 = NCEP: PIREP (NRL Manual AIREP/voice)
                             ! 31 = NCEP: AMDAR (all types except Canadian) (NRL: AMDAR)
                             ! 32 = NCEP; RECCOs, but these are in ADPUPA msgs
                             ! 33 = NCEP: MDCRS (NRL: MDCRS)
                             ! 34 = NCEP: TAMDAR (NRL: ACARS)
                             ! 35 = NCEP: Canadian AMDAR (NRL: AMDAR)

c Process SQN/PROCN - they will be used to construct full reports from mass and wind pieces
c -----------------------------------------------------------------------------------------
            sqn_current = hdr(nrpts4QC,10)
            procn_current = hdr(nrpts4QC,11)

c Get turbulence values, present weather (PRWE), cloud data, etc. (these are all nested-
c  replicated)
c
c Note: These values, while there may be multiple replications of them, should be present
c       only on a single level upon input (aircraft data is organized as single-level data
c       upon input to this program - prior step is PREPDATA)
c ----------------------------------------------------------------------------------------

c turb3seq values: DGOT HBOT HTOT                
c  -- since this is not encoded to PREPBUFR-like (profiles) right now, no need to read in
ccccc       arr_8 = bmiss
ccccc
ccccc       call ufbseq(inlun,arr_8,15,10,nrep,'TURB3SEQ')
ccccc
ccccc       nnestreps(1,nrpts4QC) = nrep
ccccc
ccccc       if(nrep.ne.0) then ! There is turb3seq data to store
ccccc         do i = 1,nrep
ccccc
ccccc           if(nrep.le.5) then
ccccc             turb3seq(:,nrpts4QC,nnestreps(1,nrpts4QC)-nrep+i)
ccccc+             = arr_8(1:3,i)
ccccc           else ! there are more than 5 replications of TURB3SEQ 
ccccc             print *,'there are more than 5 reps of TURB3SEQ'
ccccc             print *,'report #', nrpts4QC
ccccc           endif ! if(nrep.le.5)
ccccc
ccccc         enddo ! do i = 1,nrep
ccccc       endif ! if(nrep.ne.0)

c prewxseq values: PRWE
c  -- since this is not encoded to PREPBUFR-like (profiles) right now, no need to read in
ccccc       arr_8 = bmiss
ccccc
ccccc       call ufbseq(inlun,arr_8,15,10,nrep,'PREWXSEQ')
ccccc
ccccc       nnestreps(2,nrpts4QC) = nrep
ccccc
ccccc       if(nrep.gt.0) then
ccccc         do i = 1,nrep
ccccc
ccccc           if(nrep.le.5) then
ccccc             prewxseq(1,nrpts4QC,nnestreps(2,nrpts4QC)-nrep+i) 
ccccc+             = arr_8(1,i)
ccccc           else ! there are more than 5 replications of PREWXSEQ
ccccc             print *,'there are more than 5 reps of PREWXSEQ'
ccccc             print *,'report #', nrpts4QC
ccccc           endif ! if(nrep.le.5)
ccccc
ccccc         enddo ! do i = 1,nrep
ccccc       endif ! if(nrep.gt.0)
 
c cloudseq values: VSSO CLAM CLTP HOCB HOCT 
c  -- since this is not encoded to PREPBUFR-like (profiles) right now, no need to read in
ccccc       arr_8 = bmiss
ccccc
ccccc       call ufbseq(inlun,arr_8,15,10,nrep,'CLOUDSEQ')
ccccc
ccccc       nnestreps(3,nrpts4QC) = nrep
ccccc
ccccc       if(nrep.gt.0) then
ccccc         do i = 1,nrep
ccccc
ccccc           if(nrep.le.5) then
ccccc             cloudseq(:,nrpts4QC,nnestreps(3,nrpts4QC)-nrep+i) 
ccccc+             = arr_8(1:5,i) 
ccccc         else ! there are more than 5 replications of the cloud data
ccccc             print *,'there are more than 5 reps of CLOUDSEQ'
ccccc             print *,'report #', nrpts4QC
ccccc           endif ! if(nrep.le.5)
ccccc        
ccccc         enddo ! do i = 1,nrep
ccccc       endif ! if(nrep.gt.0)

c afic_seq values: AFIC HBOI HTOI
c  -- since this is not encoded to PREPBUFR-like (profiles) right now, no need to read in
ccccc       arr_8 = bmiss
ccccc
ccccc       call ufbseq(inlun,arr_8,15,10,nrep,'AFIC_SEQ')
ccccc         
ccccc       nnestreps(4,nrpts4QC) = nrep
ccccc        
ccccc       if(nrep.gt.0) then
ccccc         do i = 1,nrep
ccccc
ccccc           if(nrep.le.5) then
ccccc             afic_seq(:,nrpts4QC,nnestreps(4,nrpts4QC)-nrep+i) 
ccccc+              = arr_8(1:3,i)
ccccc           else ! there are more than 5 replications of the aircraft icing data
ccccc             print *,'there are more than 5 reps of AFIC_SEQ'
ccccc             print *,'report #', nrpts4QC
ccccc           endif ! if(nrep.le.5)
ccccc
ccccc         enddo ! do i = 1,nrep		
ccccc       endif ! if(nrep.gt.0)

c Start pulling out non-nested-replicated values
c ----------------------------------------------

c acft_seq values: PCAT POAF
            arr_8 = bmiss

            call ufbint(inlun,arr_8,15,10,nlev,'PCAT POAF')

            acft_seq(nrpts4QC,:) = arr_8(1:2,1)

            if(ibfms(arr_8(2,1)).ne.0 .or. arr_8(2,1).eq.7.) then
              phase(nrpts4QC) = 9                   ! NRL sets a missing value of
            else                                    !  phase of flight = 9
              phase(nrpts4QC) = int(arr_8(2,1))
            endif

            if (ibfms(arr_8(1,1)) .ne. 0 ) then
              t_prcn(nrpts4QC) = amiss
            else
              t_prcn(nrpts4QC) = arr_8(1,1)
            endif

c turb[1,2]seq values: TRBX TRBX10 TRBX21 TRBX32 TRBX43
            arr_8 = bmiss

            call ufbint(inlun,arr_8,15,10,nlev,
     +                 'TRBX TRBX10 TRBX21 TRBX32 TRBX43')

            turb1seq(nrpts4QC) = arr_8(1,1)
            turb2seq(nrpts4QC,:) = arr_8(2:5,1)

c Other misc. values: RCT, ROLF, MSTQ, CAT
            arr_8 = bmiss

            call ufbint(inlun,arr_8,15,10,nlev,'RCT ROLF MSTQ CAT')

            rct(nrpts4QC) = arr_8(1,1)
            mstq(nrpts4QC) = arr_8(3,1)
            cat(nrpts4QC) = arr_8(4,1)
            rolf(nrpts4QC) = arr_8(2,1)

c ----------------------------------------------------------------------------------------
c ----------------------------------------------------------------------------------------
c Populate flight number and tail number arrays (c_acftid and c_acftreg, resp.)
c ----------------------------------------------------------------------------------------
            call ufbint(inlun,c_arr_8,1,1,nlev,'SID')

            hdr(nrpts4QC,1) = c_arr_8

            if(mesgtype.eq.'AIRCFT') then
              if(itype(nrpts4QC).eq.31 .or.
     +           itype(nrpts4QC).eq.35) then

c All AMDAR types currently store tail number in 'SID', while flight number is missing or all
c  blanks for all types except for LATAM (Chile) - if flight number is missing or all blanks,
c  copy 'SID' into BOTH tail number and flight number locations in NRL arrays; if flight
c  number is present and nnot all blanks (LATAM), copy flight number (from 'ACID') into flight
c  number location in NRL array
c  (Note: European AMDARs may have a valid flight number but it is not yet available in
c         PREPBUFR, when it is it will be in mnemonic 'ACID' - DAK)
c ------------------------------------------------------------------------------------------
                c_acftreg(nrpts4QC) = charstr   ! tail number
                c_acftid(nrpts4QC)  = charstr   ! flight number (default is tail number)
                call ufbint(inlun,c_arr_8,1,1,nlev,'ACID')
                if(ibfms(c_arr_8).eq.0) then
                   if(charstr.ne.'        ') then
                      c_acftid(nrpts4QC) = charstr    ! flight number ('ACID' if present, always)
                      acid(nrpts4QC) = c_arr_8        !  the case for LATAM AMDAR
                   endif
                endif

              elseif(itype(nrpts4QC).eq.30 .or.
     +               itype(nrpts4QC).eq.34) then

c AIREP currently stores flight number in 'SID', while PIREP and TAMDAR currently store a
c  manufactured ID in 'SID' - copy this into ONLY flight number location in NRL array
c  (tail number location will store an all blank tail number - missing)
c ---------------------------------------------------------------------------------------
                c_acftid(nrpts4QC)  = charstr   ! flight number
                c_acftreg(nrpts4QC) = '        '! tail number

                if(itype(nrpts4QC).eq.34)       ! TAMDARs replace '000' in characters 1-3
     +           c_acftid(nrpts4QC)(1:3) = 'TAM'!  of flight # with 'TAM' so they will pass
                                                !  "invalid data" check in acftobs_qc
              endif
            
            elseif(mesgtype.eq.'AIRCAR') then

c MDCRS from ARINC currently store tail number in 'SID' and flight number in 'ACID' - copy
c  these into  tail number and flight number locations in NRL arrays
c  (Note: MDCRS from AFWA was a rarely used backup to those from ARINC until it was
c         discontinued on 10/30/2009 - it apparently stored flight number in 'SID' and
c         in 'ACID' - store flight number in 'SID' as tail number and flight number in
c         'ACID' (if present) as flight number (even those would be the same here)
c ---------------------------------------------------------------------------------------
              c_acftreg(nrpts4QC) = charstr     ! tail number
              call ufbint(inlun,c_arr_8,1,1,nlev,'ACID')
              if(ibfms(c_arr_8).eq.0) then
                c_acftid(nrpts4QC) = charstr    ! flight number ('ACID' if present, always)
                acid(nrpts4QC) = c_arr_8        !  the case for MDCRS from ARINC)
              else
                c_acftid(nrpts4QC) = '         '! store flight number as missing (all blanks)
                                                !  if not present (may be the case for MDCRS
                                                !  from AWFA)
              endif
            endif
c ----------------------------------------------------------------------------------------
c ----------------------------------------------------------------------------------------

c Pull out obs and events for subset n
c ------------------------------------

c ********************************
c PRES, OB_T, OB_Q, OB_DIR, OB_SPD
c ********************************

c If l_allev_pf is TRUE, use ufbevn to get at data values & events - all pre-existing events
c  will be encoded into output PREPBUFR-like (profiles) file, (if l_doprofiles=T) along with
c  any new NRLQCQC events on top of them (Note: All pre-existing events are always encoded
c  into full PREPBUFR file)
c
c          |---------> data types (1=ob, 2=qm, 3=pc, 4=rc, 5=fc, 6=an, 7=cat)
c          | |-------> number of levels in the rpt (aircraft data is single level data;
c          | |         set j=1)
c          | | |-----> number of events (will store all events, but only use latest event in
c          | | |        in the top of the stack (k=1) is used by the core NRL QC code)
c          | | | |---> variable types (1=p,2=q,3=t,4=z,5=u,6=v)
c pqtzuvEV(i,j,k,l)
c
c example: pqtzuvEV(2,1,1,5) = QM for U in latest (top-of-stack) event on the 1st level

c OTHERWISE:
c If l_allev_pf is FALSE, use ufbint to get at data values for only latest (top-of-stack)
c  event - only latest event will be encoded into output PREPBUFR-like (profiles) file, (if
c  l_doprofiles=T) along with any new NRLQCQC events on top of it (runs faster but pre-
c  existing events are not recorded in output PREPBUFR-like file) (Note: All pre-existing
c  events are always encoded into full PREPBUFR file)
c
c          |---------> data types (1=ob, 2=qm, 3=pc, 4=rc, 5=fc, 6=an, 7=cat)
c          | |-------> number of levels in the rpt (aircraft data is single level data;
c          | |         set j=1)
c          | | |-----> always 1 since only one (the latest top-of-stack) event is returned
c          | | |       here
c          | | | |---> variable types (1=p,2=q,3=t,4=z,5=u,6=v)
c pqtzuvEV(i,j,1,l)
c
c example: pqtzuvEV(2,1,1,5) = QM for U in latest (top-of-stack) event on the 1st level
c ------------------------------------------------------------------------------------------
            if(.not.l_allev_pf) then
              do i = 1,mxvt
                call ufbint(inlun,pqtzuvEV(1,1,1,i),mxevdt,mxlv,nlev,
     +                      EVstr(i))
              enddo
            else
              do i = 1,mxvt
                call ufbevn(inlun,pqtzuvEV(1,1,1,i),mxevdt,mxlv,mxnmev,
     +                      nlev,EVstr(i))
              enddo
            endif

            if(.not.l_match) then

c pressure and pressure/altitude will only be read in and stored from the first (mass) piece
c  of a mass/wind piece report pair or from the first (only) piece of a wind-only report
c  rather than being re-read and re-stored again from the second (wind) piece (if a second
c  piece exists) - this not only avoids wasted processing time (since the pressure and
c  pressure-altitude should be the same in both pieces), it also prevents this code from
c  reading a missing pressure-altitude in the second piece for those rare cases when
c  unreasonably-high winds can be set to missing in PREPDATA resulting in an "empty" wind
c  piece being encoded into PREPBUFR (and leading to problems in later profile generation in
c  this code) (this is a bug in PREPDATA which will eventually be corrected)

c Count the number of pressure/altitude events in this report
c -----------------------------------------------------------
c pressure (1)
c ------------
              if(l_allev_pf) then
                evknt = 0
                do j = 1,mxnmev
c                                         |---> pressure
                  if(ibfms(pqtzuvEV(1,1,j,1)).ne.0) then
                    nevents(nrpts4QC,1) = evknt
                    exit ! exit j do loop
                  else
                    evknt = evknt + 1
                  endif
                enddo
              else
                nevents(nrpts4QC,1) = 1
              endif

c altitude (4)
c ------------
              if(l_allev_pf) then
                evknt = 0
                do j = 1,mxnmev
c                                         |---> altitude
                  if(ibfms(pqtzuvEV(1,1,j,4)).ne.0) then
                    nevents(nrpts4QC,4) = evknt
                    exit ! exit j do loop
                  else
                    evknt = evknt + 1
                  endif
                enddo
              else
                nevents(nrpts4QC,4) = 1
              endif

c Store pressure in array needed by subroutine acftobs_qc
c -------------------------------------------------------
              if(ibfms(pqtzuvEV(1,1,1,1)).eq.0) then
                pres(nrpts4QC) = pqtzuvEV(1,1,1,1) ! POB at top of stack = pressure fed to
                                                   !  NRL QC
              endif

c Store pressure events in "corral" arrays to carry through this code
c -------------------------------------------------------------------
              if(nevents(nrpts4QC,1).gt.0) then
                do i = 1,nevents(nrpts4QC,1)
                  invi = nevents(nrpts4QC,1)-i+1

c                                                 |---> acft data upon input = "single level"/
c                                                 |      1 replication of PRSLVLA
c                                                 |      |---> pressure
                  pob_ev(nrpts4QC,i) = pqtzuvEV(1,1,invi,1)
                  pqm_ev(nrpts4QC,i) = pqtzuvEV(2,1,invi,1)
                  ppc_ev(nrpts4QC,i) = pqtzuvEV(3,1,invi,1)
                  prc_ev(nrpts4QC,i) = pqtzuvEV(4,1,invi,1)
                enddo
              endif

c Store pressure background info in "corral" arrays to carry through this code
c ----------------------------------------------------------------------------
              call ufbint(inlun,arr_8,15,10,nlev,'POE PFC PFCMOD') ! only one occurence of bg
                                                                   !  info per report/level
              pbg(nrpts4QC,:) = arr_8(1:3,1)

              call ufbint(inlun,arr_8,15,10,nlev,'PAN PCL PCS') ! only one occurence of post-p
                                                                !  info per report/level
              ppp(nrpts4QC,:) = arr_8(1:3,1)

c Store altitude events in "corral" arrays to carry through this code - the actual value of
c  altitude is pulled from ELV and stored in the ht_ft array (needed by acftobs_qc) when the
c  rest of the report header information is pulled
c ------------------------------------------------------------------------------------------
              if(nevents(nrpts4QC,4).gt.0) then
                do i = 1,nevents(nrpts4QC,4)
                  invi = nevents(nrpts4QC,4)-i+1

c                                                        |---> altitude
                  zob_ev(nrpts4QC,i) = pqtzuvEV(1,1,invi,4)
                  zqm_ev(nrpts4QC,i) = pqtzuvEV(2,1,invi,4)
                  zpc_ev(nrpts4QC,i) = pqtzuvEV(3,1,invi,4)
                  zrc_ev(nrpts4QC,i) = pqtzuvEV(4,1,invi,4)
                enddo
              endif

c Store altitude background info in "corral" arrays to carry through this code
c ----------------------------------------------------------------------------
              call ufbint(inlun,arr_8,15,10,nlev,'ZOE ZFC ZFCMOD') ! only one occurence of bg
                                                                   !  info per report/level
              zbg(nrpts4QC,:) = arr_8(1:3,1)

              call ufbint(inlun,arr_8,15,10,nlev,'ZAN ZCL ZCS') ! only one occurence of post-p
                                                                !  info per report/level
              zpp(nrpts4QC,:) = arr_8(1:3,1)

            endif

c Get temperature & moisture obs, increments, quality marks
c ---------------------------------------------------------
            if(l_massrpt) then ! pull mass data

              sqn(nrpts4QC,1) = sqn_current     ! SQN (sequence number) for mass piece
              procn(nrpts4QC,1) = procn_current ! PROCN (process number) for mass piece

c Count the number of moisture events in this report
c --------------------------------------------------
c moisture (2)
c ------------
              if(l_allev_pf) then
                evknt = 0
                do j = 1,mxnmev
c                                         |---> moisture
                  if(ibfms(pqtzuvEV(1,1,j,2)).ne.0) then
                    nevents(nrpts4QC,2) = evknt
                    exit ! exit j do loop
                  else
                    evknt = evknt + 1
                  endif
                enddo
              else
                nevents(nrpts4QC,2) = 1
              endif

c Moisture (specific humidity) - use QOB from the top of event stack (pqtzuvEV(1,1,1,2)
c -------------------------------------------------------------------------------------
              if(ibfms(pqtzuvEV(1,1,1,2)).eq.0) then
                ob_q(nrpts4QC) = pqtzuvEV(1,1,1,2)*0.001 ! NRL code requires g/kg; QOB in
                                                         !  PREPBUFR file is in mg/kg
                xiv_q(nrpts4QC) = (pqtzuvEV(1,1,1,2) -
     +                pqtzuvEV(5,1,1,2))*0.001 ! use QOB at top of stack; also, there is only
                                               !  one QFC per report

                nchk_q(nrpts4QC) = int(pqtzuvEV(2,1,1,2)) ! QQM from top of event stack
              endif

c Store moisture events in "corral" arrays to carry through this code
c -------------------------------------------------------------------
              if(nevents(nrpts4QC,2).gt.0) then
                do i = 1,nevents(nrpts4QC,2)
                  invi = nevents(nrpts4QC,2)-i+1

c                                                        |---> moisture
                  qob_ev(nrpts4QC,i) = pqtzuvEV(1,1,invi,2)
                  qqm_ev(nrpts4QC,i) = pqtzuvEV(2,1,invi,2)
                  qpc_ev(nrpts4QC,i) = pqtzuvEV(3,1,invi,2)
                  qrc_ev(nrpts4QC,i) = pqtzuvEV(4,1,invi,2)
                enddo
              endif

c Store moisture background info in "corral" arrays to carry through this code
c ----------------------------------------------------------------------------
              call ufbint(inlun,arr_8,15,10,nlev,'QOE QFC QFCMOD') ! only one occurence of bg
                                                                   !  info per report/level
              qbg(nrpts4QC,:) = arr_8(1:3,1)

              call ufbint(inlun,arr_8,15,10,nlev,'QAN QCL QCS') ! only one occurence of post-
                                                                !  p info per report/level
              qpp(nrpts4QC,:) = arr_8(1:3,1)

c Count the number of temperature events in this report
c -----------------------------------------------------
c temperature (3)
c ---------------
              if(l_allev_pf) then
                evknt = 0
                do j = 1,mxnmev
c                                         |---> temperature
                  if(ibfms(pqtzuvEV(1,1,j,3)).ne.0) then
                    nevents(nrpts4QC,3) = evknt
                    exit ! exit j do loop
                  else
                    evknt = evknt + 1
                  endif
                enddo
              else
                nevents(nrpts4QC,3) = 1
              endif

c Temperature - use TOB from the top of event stack (pqtzuvEV(1,1,1,3)
c --------------------------------------------------------------------
              if(ibfms(pqtzuvEV(1,1,1,3)).eq.0) then
                ob_t(nrpts4QC) = pqtzuvEV(1,1,1,3) + 273.16 ! convert to K
                xiv_t(nrpts4QC) = pqtzuvEV(1,1,1,3) - pqtzuvEV(5,1,1,3) ! use TOB at top of
                                                                        !  stack; also, there
                                                                        !  is only one TFC
                                                                        !  per report
                nchk_t(nrpts4QC) = int(pqtzuvEV(2,1,1,3)) ! TQM from top of event stack

c Check for -9C temperature (MDCRS only)
c --------------------------------------
                l_minus9C(nrpts4QC) = .false.

                if(itype(nrpts4QC).eq.33) then
                  if(abs(ob_t(nrpts4QC)-264.16).lt.0.05) then
                    l_minus9c(nrpts4QC) = .true.
                  endif
                endif ! check for -9C temp in type = 33
              endif ! check for missing

c Store temperature events in "corral" arrays to carry through this code
c ----------------------------------------------------------------------
              if(nevents(nrpts4QC,3).gt.0) then
                do i = 1,nevents(nrpts4QC,3)
                  invi = nevents(nrpts4QC,3)-i+1

c                                                        |---> temperature
                  tob_ev(nrpts4QC,i) = pqtzuvEV(1,1,invi,3)
                  tqm_ev(nrpts4QC,i) = pqtzuvEV(2,1,invi,3)
                  tpc_ev(nrpts4QC,i) = pqtzuvEV(3,1,invi,3)
                  trc_ev(nrpts4QC,i) = pqtzuvEV(4,1,invi,3)
                enddo
              endif

c Store temperature background info in "corral" arrays to carry through this code
c -------------------------------------------------------------------------------
              call ufbint(inlun,arr_8,15,10,nlev,'TOE TFC TFCMOD') ! only one occurence of bg
                                                                   !  info per report/level
                
              tbg(nrpts4QC,:) = arr_8(1:3,1)

              call ufbint(inlun,arr_8,15,10,nlev,'TAN TCL TCS') ! only one occurence of post-
                                                                !  p info per report/level
              tpp(nrpts4QC,:) = arr_8(1:3,1)

c Get u & v obs, increments, quality marks - convert u & v to direction & speed
c -----------------------------------------------------------------------------
            elseif(l_windrpt) then ! pull u, v, convert to direction & speed

              sqn(nrpts4QC,2) = sqn_current     ! SQN (sequence number) for wind piece
              procn(nrpts4QC,2) = procn_current ! PROCN (process number) for wind piece

c Count the number of wind events in this report
c ----------------------------------------------
c Wind (5/6) - use U/VOB from the top of event stack (pqtzuvEV(1,1,1,5) and
c  (pqtzuvEV(1,1,1,6)
c -------------------------------------------------------------------------
              if(l_allev_pf) then
                evknt = 0
                do j = 1,mxnmev
c                                         |---> 5=u-comp, 6=v-comp
                  if(ibfms(pqtzuvEV(1,1,j,5)).ne.0 .or.
     +               ibfms(pqtzuvEV(1,1,j,6)).ne.0) then
                    nevents(nrpts4QC,5) = evknt
                    exit ! exit j do loop
                  else
                    evknt = evknt + 1
                  endif
                enddo
              else
                nevents(nrpts4QC,5) = 1
              endif

              uob = pqtzuvEV(1,1,1,5)
              vob = pqtzuvEV(1,1,1,6)

              ufc = pqtzuvEV(5,1,1,5) ! only one UFC per report
              vfc = pqtzuvEV(5,1,1,6) ! only one VFC per report

              if(ibfms(pqtzuvEV(1,1,1,5)).eq.0 .and.
     +           ibfms(pqtzuvEV(1,1,1,6)).eq.0 ) then

c Calculate speed & direction from U & V components (for both obs and forecast values)
c
c Per Dennis Keyser on 8/29/05, w3fc05 returns a wind direction of true meteorological nature
c  (e.g., a wind w/ dir =270 is a wind from the west)
c
c ALSO NOTE: w3fc05 adds 0.001 to the direction - in order to get around this (without
c            immediately changing this routine in W3EMC), set any wind directions between
c            360.000 and 360.002 back to 360.00 - might be a good idea to remove the addition
c            of 0.001 to the wind direction in the W3EMC routine w3fc05 some day
c -------------------------------------------------------------------------------------------
                call w3fc05(uob,vob,ob_dir(nrpts4QC),ob_spd(nrpts4QC))

c If-statement below is used to negate effect of 0.001 being added to the wind direction in
c  the W3EMC routine w3fc05
c -----------------------------------------------------------------------------------------

                if(ob_dir(nrpts4QC).gt.360.000 .and.
     +             ob_dir(nrpts4QC).lt.360.002) then

                  ob_dir(nrpts4QC) = 360.00

                endif

                call w3fc05(ufc,vfc,dir_fc,spd_fc) ! similar to cqcbufr/incrw
c increments
c ---------
                xiv_s(nrpts4QC) = ob_spd(nrpts4QC) - spd_fc
                xiv_d(nrpts4QC) = ob_dir(nrpts4QC) - dir_fc
c quality marks
c -------------
                nchk_s(nrpts4QC)=
     +                int(pqtzuvEV(2,1,1,5)) ! use u-component QM
                nchk_d(nrpts4QC)=
     +                int(pqtzuvEV(2,1,1,5)) ! use u-component QM

              endif ! Check for missings

c Store wind events in "corral" arrays to carry through this code
c ---------------------------------------------------------------
              if(nevents(nrpts4QC,5).gt.0) then
                do i = 1,nevents(nrpts4QC,5)
                  invi = nevents(nrpts4QC,5)-i+1

c                                                        |---> wind
                  uob_ev(nrpts4QC,i) = pqtzuvEV(1,1,invi,5)
                  vob_ev(nrpts4QC,i) = pqtzuvEV(1,1,invi,6)
                  wqm_ev(nrpts4QC,i) = pqtzuvEV(2,1,invi,5)
                  wpc_ev(nrpts4QC,i) = pqtzuvEV(3,1,invi,5)
                  wrc_ev(nrpts4QC,i) = pqtzuvEV(4,1,invi,5)
                enddo
              endif

c Store wind background info in "corral" arrays to carry through this code
c ------------------------------------------------------------------------
              call ufbint(inlun,arr_8,15,10,nlev,
     +                   'WOE UFC VFC UFCMOD VFCMOD') ! only one occurence of bg info per
                                                      !  report/level
              wbg(nrpts4QC,:) = arr_8(1:5,1)

              call ufbint(inlun,arr_8,15,10,nlev,
     +                    'UAN VAN UCL VCL UCS VCS')  ! only one occurence of post-p info per
                                                      !  report/level
              wpp(nrpts4QC,:) = arr_8(1:6,1)

c Pull wind (direction/speed) events
c ----------------------------------
              if(.not.l_allev_pf) then
                call ufbint(inlun,df_arr,5,mxlv,nlev,
     +                      'DDO FFO DFQ DFP DFR')
              else
                call ufbevn(inlun,df_arr,5,mxlv,mxnmev,nlev,
     +                      'DDO FFO DFQ DFP DFR')
              endif

c Count the number of wind events (dir/speed) events in this report
c -----------------------------------------------------------------
              if(l_allev_pf) then
                ddo_ev(nrpts4QC,:) = df_arr(1,1,:)
                ffo_ev(nrpts4QC,:) = df_arr(2,1,:)
                dfq_ev(nrpts4QC,:) = df_arr(3,1,:)
                dfp_ev(nrpts4QC,:) = df_arr(4,1,:)
                dfr_ev(nrpts4QC,:) = df_arr(5,1,:)
                evknt = 0
                do j = 1,mxnmev
                  if(ibfms(df_arr(1,1,j)).ne.0 .or.
     +               ibfms(df_arr(2,1,j)).ne.0) then
                    nevents(nrpts4QC,6) = evknt
                    exit ! exit j do loop
                  else
                    evknt = evknt + 1
                  endif
                enddo
              else
                ddo_ev(nrpts4QC,1) = df_arr(1,1,1)
                ffo_ev(nrpts4QC,1) = df_arr(2,1,1)
                dfq_ev(nrpts4QC,1) = df_arr(3,1,1)
                dfp_ev(nrpts4QC,1) = df_arr(4,1,1)
                dfr_ev(nrpts4QC,1) = df_arr(5,1,1)
                nevents(nrpts4QC,6) = 1
              endif

            endif ! Check for mass or wind report

            if(l_match) then ! report just stored was the second half
              call readns(inlun,mesgtype,mesgdate,iret) ! Advance pointer to next subset
              if(iret.eq.-1) then    ! there are no more subsets to read in the PREPBUFR file
                print *, 'READNS: NO MORE SUBSETS TO READ IN THE BUFR'//
     +                   ' FILE'
                exit
              elseif(iret.eq.0) then ! there are still subsets to read; pull the next one
                go to 4001
              else
                print *, 'Unexpected return code(iret=',iret,') from ',
     +                   'readns!'
                call w3tage('PREPOBS_PREPACQC')
                call errexit(23) ! Problems reading BUFR file
              endif
            endif

c At this point, we are done reading in subset n - call readns to get subset n+1 - see if it
c  is the wind part for subset n - store in same report in local arrays if so - if subset n+1
c  is not the second piece of subset n, pull its header along with the data values and
c  events, and store it in its own report
c -------------------------------------------------------------------------------------------
            call readns(inlun,mesgtype,mesgdate,iret)
            if(iret.eq.-1) then    ! there are no more subsets to read in the PREPBUFR file
              print *, 'READNS: NO MORE SUBSETS TO READ IN THE BUFR '//
     +                 'FILE'
              exit
            elseif(iret.eq.0) then ! there are still subsets to read; pull the next one

c Update counters
c ---------------
              if(mesgtype.eq.'AIRCFT') then
                nrptsaircft = nrptsaircft + 1
              elseif(mesgtype.eq.'AIRCAR') then
                nrptsaircar = nrptsaircar + 1
              else ! We're done reading the aircraft-type messages out of this file
                print *, '---> MESGTYPE NOT AIRCRAFT TYPE!!!',' "',
     +                   mesgtype,'"'
                print *, '---> keep looping through messages in case',
     +                   ' any more are in file'
                cycle loop2
              endif
      	      nrpts_rd = nrpts_rd + 1

c Pull out the value of SQN for subset n+1, which will wither be a mass piece or a wind piece
c  (most likely a wind piece for subset n).
c -------------------------------------------------------------------------------------------
              call ufbint(inlun,arr_8,15,10,nlev,'SQN')
              sqn_next = arr_8(1,1)

c Check and see if the report pulled by readns (subset n+1) is the second part for the report
c  pulled by an iteration of do ireadsb (subset n)
c -------------------------------------------------------------------------------------------
              if(sqn_next.eq.sqn_current) then  ! subset n+1 is the second part of subset n.
                                                !  alat,alon,ht_ft,c_acftid (or c_acftreg) 
                                                !  and idt have already been populated; no
                                                !  need to pull these twice 
                numpairs = numpairs + 1

                if(mesgtype.eq.'AIRCFT') then
                  numAIRCFTpairs = numAIRCFTpairs + 1
                elseif(mesgtype.eq.'AIRCAR') then
                  numAIRCARpairs = numAIRCARpairs + 1
                endif

                l_match = .true.

                go to 6001 ! SQN will be read again but nrpts4QC won't be incremented -
                           !  we've just found the 2nd half of the report previously read
              else ! subset n+1 is NOT the second part of subset n (n is an orphan)

c If we get here, we know that subsets n and n+1 are not parts of a pair - subset n is an
c  "orphan" and has already been stored - at this point, we don't yet know whether subset
c  n+1 is another orphan or whether it is the second half of a match - whether subset n+1 is
c  an orphan or part of a pair will be determined on the next iteration of this loop
c
c However, we do know that subset n+1 doesn't belong with subset n because their values of
c  SQN are different - so, we need to increment nrpts4QC before storing subset n+1 in the
c  report-oriented arrays (do so by sending the program to statement 5001)
c
c Send subset n+1 back through the program, treating subset n+1 as the new n
c ------------------------------------------------------------------------------------------
                numorph = numorph + 1

                if(mesgtype.eq.'AIRCFT') then
                  numAIRCFTorph = numAIRCFTorph + 1
                elseif(mesgtype.eq.'AIRCAR') then
                  numAIRCARorph = numAIRCARorph + 1
                endif

                l_match = .false.

c Leave BUFRLIB pointers where they are and treat subset n+1 as a new n
c ---------------------------------------------------------------------  
                go to 5001

              endif ! check to see if subset n+1 is the second part of subset n
            else 
              print *, 'Unexpected return code(iret=',iret,') from ',
     +                 'readns!'
              call w3tage('PREPOBS_PREPACQC')
              call errexit(23) ! Problems reading BUFR file 
            endif ! if(iret.eq.-1) then

          enddo ! do loop for reading BUFR subsets/reports (ireadsb)
        endif ! check for message type
      enddo loop2 ! do loop for reading messages
      print *, '---> DONE READING FROM THIS FILE!!!'
      print *, '---> nrpts_rd = ', nrpts_rd

      if(nrpts_rd.gt.0) then

c Determine ITYPE, C_DTG, etc.
c ----------------------------
      do i=1,nrpts4QC

c nevents can never be zero, otherwise array out-of-bounds issues will occur downstream -
c  make sure nevents is always at least 1 for all variables and all reports
c ---------------------------------------------------------------------------------------
        nevents(i,:) = max(nevents(i,:),1)

c ********************************************
c ITYPE --> REMAP FROM NCEP VALUE TO NRL VALUE
c ********************************************

c Determine type of aircraft report (itype)
c
c Need to check phase of flight and PREPBUFR report type
c PREPBUFR report types (mnemonic = TYP) where x is either: 1=mass, 2=wind part:
c       x30 = NCEP: AIREP (NRL Manual AIREP/voice)
c       x30 = NCEP: PIREP (NRL Manual AIREP/voice)
c       x31 = NCEP: AMDAR (all types except Canadian) (NRL: AMDAR)
c       x33 = NCEP: MDCRS (NRL: MDCRS)
c       x34 = NCEP: TAMDAR (NRL: ACARS)
c       x35 = NCEP: Canadian AMDAR (NRL: AMDAR)
c
c NCEP BUFR MNEMONIC POAF (phase of flight)/BUFR desc. 0-08-004:
c       0-1 = reserved
c       2 = Unsteady
c       3 = Level flight, routine observation
c       4 = Level flight, highest wind encountered
c       5 = Ascending
c       6 = Descending
c       7 = missing (set to 9 prior to this to match NRL's missing value)
c   bmiss = missing (set to 9 prior to this to match NRL's missing value)
c
c ##############################################################
c NRL settings for itype (see function insty_ob_fun):
c   --> Use value of POAF to determine whether ob was taken while the aircraft was ascending,
c       descending, etc.
c
c   Below * means used by NCEP
c
c --------------------------------------------------------------
c ---> NRL AIREPs
c     * 25/'man-airep'  = Manual AIREP (header XRXX)/"typical voice AIREP"
c               -- NOTE: Assign PIREPs (used at NCEP but not at NRL) to this "typical voice
c                        AIREP" category
c               -- NOTE: Assign all AIREPs (for now) to this "typical voice AIREP" category
c       26/'man-Yairep' = Manual AIREP (header YRXX)/keypad AIREP
c               -- NOTE: NCEP does not assign anything to this at the current time
c       30/'airep'      = automated "AIREPs" (AMDAR or UAL MDCRS re-encoded as AIREPs by AFWA)
c               -- NOTE: NCEP does not assign anything to this at the current time
c                        AFWA stopped re-encoding AMDAR and MDCRS into AIREP in Oct 2009
c      131/'airep_asc'  = AIREP ascending profile
c               -- NOTE: NCEP does not assign anything to this at the current time
c      132/'airep_des'  = AIREP descending profile
c               -- NOTE: NCEP does not assign anything to this at the current time
c       33/'airep_lvl'  = AIREP level flight
c               -- NOTE: NCEP does not assign anything to this at the current time
C       34/'airep_msg'  = AIREP w/ missing category (if rpt is not 25, 26, or 30)
c               -- NOTE: NCEP does not assign anything to this at the current time
c --------------------------------------------------------------
c ---> NRL AMDARs
c     * 35/'amdar'      = Automated aircraft data (AMDAR) (POAF cannot be determined)
c     *136/'amdar_asc'  = AMDAR ascending profile
c     *137/'amdar_des'  = AMDAR descending profile
c     * 38/'amdar_lvl'  = AMDAR level flight
c --------------------------------------------------------------
c ---> NRL ACARS {NOTE: Originally deemed "ACARS" by NRL, but this is currently not used by
c                       NRL (per email from Pat Pauley 1/12/05); NCEP will use them to provide
c                       a separate category for TAMDARs and rename them as TAMDAR in all
c                       printouts from acftobs_qc.f}
c       40/'acars'      = Automated aircraft (TAMDAR) (POAF cannot be determined)
c      141/'acars_asc'  = TAMDAR ascending profile
c      142/'acars_des'  = TAMDAR descending profile
c       43/'acars_lvl'  = TAMDAR level flight
c --------------------------------------------------------------
c ---> NRL MDCRS
c     * 45/'mdcrs' = Automated aircraft (MDCRS) (POAF cannot be determined)
c     *146/'mdcrs_asc'  = MDCRS ascending profile
c     *147/'mdcrs_des'  = MDCRS descending profile
c     * 48/'mdcrs_lvl'  = MDCRS level flight
c ##############################################################

        if(itype(i).eq.30) then         ! NCEP: AIREP (NRL Manual AIREP/voice) or
                                        ! NCEP: PIREP (NRL Manual AIREP/voice)
          phase(i) = 9                  !  NRL leaves phase of flight as missing for all
                                        !  AIREP/PIREP types (fine since NCEP does not have
                                        !  phase of flight info for AIREPs or PIREPs)

          if(c_acftid(i)(1:1).eq.'P'.and.c_acftid(i)(6:6).eq.'P') then ! NCEP PIREPs (BUFR
                                                                       ! tank b004/xx002)

c SMB: Data type label changed from 34 -> 25 on 5/5/05.  PIREPs are probably more along the
c      lines of "typical voice reports" than AIREPs with a "missing" category
c DAK: Agreed, if we are still going to use PIREPs lump them into Manual AIREP/voice category
            itype(i) = 25
            nPIREP = nPIREP + 1 

          else ! NCEP AIREPs (BUFR tank b004/xx001)
c SMB: Originally set these to 30 (reformatted something else's/"automated AIREPs")
c DAK: Changed these to 25 on 3/23/12 (30 is reserved for AFWA re-encoded AIREPS, orig. AMDAR
c      or MDCRS - there are none of these after Oct. 2009 per Eric Wise/AFWA)
c      We may want to try to isolate ADS's in N. Atlantic as type 30 (NRL does this) but not
c      at this point (right now ADS's go into NCO's airep decoder and come out in b004/xx001
c      tank)
ccccccccc   itype(i) = 30
ccccccccc   nAUTOAIREP = nAUTOAIREP + 1	
            itype(i) = 25
            nMANAIREP = nMANAIREP + 1	
          endif 

        elseif(itype(i).eq.31) then     ! NCEP: AMDAR (all types except Canadian) (NRL: AMDAR)
                                 ! (BUFR tanks b004/xx003, b004/xx006, b004/xx011, b004/xx103)
          nAMDAR = nAMDAR + 1	
          if(phase(i).eq.3 .or. phase(i).eq.4) then
            itype(i) = 38               ! level flight
          elseif(phase(i).eq.5) then
            itype(i) = 136              ! ascending flight
          elseif(phase(i).eq.6) then
            itype(i) = 137              ! descending flight
          else
            itype(i) = 35               ! unknown phase of flight
          endif

        elseif(itype(i).eq.33) then     ! NCEP: MDCRS (NRL: MDCRS) (BUFR tank b004/xx004)
          nMDCRS = nMDCRS + 1	
          if(phase(i).eq.3 .or. phase(i).eq.4) then
            itype(i) = 48               ! level flight
          elseif(phase(i).eq.5) then
            itype(i) = 146              ! ascending flight
          elseif(phase(i).eq.6) then
            itype(i) = 147              ! descending flight
          else
            itype(i) = 45               ! unknown phase of flight
          endif

        elseif(itype(i).eq.34) then     ! NCEP: TAMDAR (NRL: ACARS)
                                 ! (BUFR tanks b004/xx008, b004/xx010, b004/xx012, b004/xx013)
c DAK: Changed these from NRL AMDAR to NRL ACARS at suggestion of P. Pauley (3/2012), (to hold
c      NCEP TAMDARs) - allows them to be treated in a separate category for stratifying
c      statistics - also seems to flag more AMDARs as bad which is a good thing since there
c      are so many anyway
          nTAMDAR = nTAMDAR + 1	
                  ! NOTE: MADIS-feed TAMDARs currently have missing phase of flight and will
                  !       get set to unknown value initially (may later change)
                  !       AirDAT/Panasonic BUFR-feed TAMDARs do contain phase of flight)
          if(phase(i).eq.3 .or. phase(i).eq.4) then
ccccccccccc itype(i) = 38               ! level flight
            itype(i) = 43               ! level flight
          elseif(phase(i).eq.5) then
ccccccccccc itype(i) = 136              ! ascending flight
            itype(i) = 141              ! ascending flight
          elseif(phase(i).eq.6) then
ccccccccccc itype(i) = 137              ! descending flight
            itype(i) = 142              ! descending flight
          else
ccccccccccc itype(i) = 35               ! unknown phase of flight
            itype(i) = 40               ! unknown phase of flight
          endif

        elseif(itype(i).eq.35) then     ! Canadian AMDAR (NRL: AMDAR) (BUFR tank b004/xx009)
          nAMDARcan = nAMDARcan + 1	
          if(phase(i).eq.3 .or. phase(i).eq.4) then
            itype(i) = 38               ! level flight
          elseif(phase(i).eq.5) then
            itype(i) = 136              ! ascending flight
          elseif(phase(i).eq.6) then
            itype(i) = 137              ! descending flight
          else
            itype(i) = 35               ! unknown phase of flight
          endif

        else
          print'(" Unexpected value for PREPBUFR report type! (itype=",
     +           I0," & should be 30, 31, 33, 34, or 35)")', itype(i)
          print *, 'i=',i

        endif

c *****
c C_DTG
c *****

c Convert idt to YYYYMMDDHHMMSS format
c ------------------------------------
        read(cdtg_an(1:4),'(i4.4)') year
        read(cdtg_an(5:6),'(i2.2)') month
        read(cdtg_an(7:8),'(i2.2)') day
        read(cdtg_an(9:10),'(i2.2)') hour

c Time increment (offset from cycle time)
c ---------------------------------------
        rinc(1) = 0. ! days
        rinc(2) = 0. ! hours
        rinc(3) = 0. ! mins
        rinc(4) = idt(i) ! seconds
        rinc(5) = 0. ! milliseconds

c Date/time for cycle time
c ------------------------
        idat(1) = year
        idat(2) = month
        idat(3) = day
        idat(4) = 0 ! time zone
        idat(5) = hour
        idat(6) = 0 ! mins
        idat(7) = 0 ! secs
        idat(8) = 0 ! millisecs

c Use W3NCO routine w3movdat to get date/time of actual observation
c -----------------------------------------------------------------
        call w3movdat(rinc,idat,jdat)

c Convert jdat values to date/time string in yyyymmddhhmmss format
c -----------------------------------------------------------------
        write(c_dtg(i)(1:4),'(i4.4)') jdat(1)
        write(c_dtg(i)(5:6),'(i2.2)') jdat(2)
        write(c_dtg(i)(7:8),'(i2.2)') jdat(3)
        write(c_dtg(i)(9:10),'(i2.2)') jdat(5)
        write(c_dtg(i)(11:12),'(i2.2)') jdat(6)
        write(c_dtg(i)(13:14),'(i2.2)') jdat(7)

c ****************************************
c TRANSLATE NCEP QC FLAGS TO NRL STANDARDS
c (Store in arrays ichk_[t,q,d,s])
c ****************************************

c QM type:                NCEP values:    NRL values:
c                          nchk_*         ichk_*h
c Not checked/neutral     2                0
c Good                    1               -1
c Suspect                 3               -2
c Bad                     4-15            -3
c Initial/missing value  -9               -9
c ---------------------------------------------------
        qms(1) = nchk_t(i)
        qms(2) = nchk_q(i)
        qms(3) = nchk_d(i)
        qms(4) = nchk_s(i)

c DAK: this could be coded up more efficiently!
        do J=1,4
          if(qms(j).eq.2) then 
            qms(j) = 0
          elseif(qms(j).eq.1) then
            qms(j) = -1
          elseif(qms(j).eq.3) then
            qms(j) = -2
          elseif(qms(j).ge.4 .and. qms(j).le.15) then
            qms(j) = -3

cc smb 8/19/05
c For now, let qms(j)/ichk_q = 0 for non-missing q - this is to bypass ichk_q checks in
c  grchek_qc 
c -------------------------------------------------------------------------------------
            if(ob_q(i).ne.amiss) then
              qms(j) = 0
            endif

          elseif(qms(j).eq.-9) then ! leave it as is
            qms(j) = -9
          else ! Store QM = NRL's missing value
            qms(j) = -9
            print'(" Unexpected value of NCEP j=",I0,"/",A," QM  (",I0,
     +             ") for report number",I0,"!")',j,QM_types(j),qms(j),i
          endif

c If ob is missing, then store NRL quality mark as -9
c ---------------------------------------------------
c DAK: this could be coded up more efficiently!
          if(j.eq.1 .and. ob_t(i).eq.amiss) then
            qms(j) = -9
          elseif(j.eq.2 .and. ob_q(i).eq.amiss) then
            qms(j) = -9
          elseif(j.eq.3 .and. ob_dir(i).eq.amiss) then
            qms(j) = -9
          elseif(j.eq.4 .and. ob_spd(i).eq.amiss) then
            qms(j) = -9
          endif

c Store altered quality marks into NRL QM arrays
c ----------------------------------------------
c DAK: this could be coded up more efficiently!
          if(j.eq.1) then
            ichk_t(i) = qms(j)
          elseif(j.eq.2) then
            ichk_q(i) = qms(j)
          elseif(j.eq.3) then
            ichk_d(i) = qms(j)
          elseif(j.eq.4) then
            ichk_s(i) = qms(j)
          endif

        enddo ! over j
      enddo ! over i
      endif ! nrpts_rd.gt.0

c Output counts
c -------------
      write(*,*) 'NUMBER OF "AIRCFT" RPTS: ',nrptsaircft
      write(*,*) '    --> MASS: ', nmswd(1,1)
      write(*,*) '    --> WIND: ', nmswd(1,2)
      write(*,*) 'NUMBER OF "AIRCAR" RPTS: ',nrptsaircar
      write(*,*) '    --> MASS: ', nmswd(2,1)
      write(*,*) '    --> WIND: ', nmswd(2,2)
      write(*,*) 'TOTAL NUMBER OF MASS AND WIND REPORTS READ: ',
     +             nrpts_rd
      write(*,*) 'TOTAL NUMBER OF PAIRS (merged mass+wind): ',numpairs
      write(*,*) 'TOTAL NUMBER OF ORPHANS (only mass or only wind ',
     +             'present): ', numorph
      write(*,*) 'NUMBER OF "AIRCFT" PAIRS/ORPHANS: ', numAIRCFTpairs,
     +             '/', numAIRCFTorph
      write(*,*) 'NUMBER OF "AIRCAR" PAIRS/ORPHANS: ', numAIRCARpairs,
     +             '/', numAIRCARorph


      write(*,*)
      write(*,*) 'TOTAL NUMBER OF REPORTS FOR QC CODE: ', nrpts4QC
  
      write(*,*)
      write(*,*) 'NUMBER OF PIREPS (MANUAL AIREP/voice): ',nPIREP
      write(*,*) 'NUMBER OF AUTO AIREPS: ',nAUTOAIREP
      write(*,*) 'NUMBER OF AIREPS (MANUAL AIREPS/voice): ',nMANAIREP
      write(*,*) 'NUMBER OF AMDAR (excl. Canadian): ',nAMDAR
      write(*,*) 'NUMBER OF CANADIAN AMDAR: ',nAMDARcan
      write(*,*) 'NUMBER OF MDCRS: ',nMDCRS
      write(*,*) 'NUMBER OF TAMDAR: ',nTAMDAR

c End program
c -----------

      if(nrpts4QC/.90.gt.max_reps .and. nrpts4QC.lt.max_reps ) then

c If the total number of merged (mass + wind piece) aircraft-type reports read in from
c  PREPBUFR file is at least 90% of the maximum allowed ("max_reps"), print diagnostic
c  warning message to production joblog file
c ------------------------------------------------------------------------------------

        print 153, nrpts4QC,max_reps
  153   format(/' #####> WARNING: THE ',I6,' AIRCRAFT RPTS IN INPUT ',
     +   'FILE ARE > 90% OF UPPER LIMIT OF ',I6,' -- INCREASE SIZE OF ',
     +   '"MAX_REPS" SOON!'/)
        write(cmax_reps,'(i6)') max_reps
!       call system('[ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" '//
!    +   '"***WARNING: HIT 90% OF '//cmax_reps//' AIRCRAFT REPORT '//
!    +   'LIMIT IN PREPOBS_PREPACQC, INCREASE SIZE OF PARM MAX_REPS"')
      endif

      write(*,*)
      write(*,*) '********************'
      write(*,*) 'input_acqc has ended'
      call system('date')
      write(*,*) '--> # reports = ',nrpts4QC
      write(*,*) '********************'
      write(*,*)

      return 

      end

