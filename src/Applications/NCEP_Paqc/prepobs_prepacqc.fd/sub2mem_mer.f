c$$$  Subprogram Documentation Block
c   BEST VIEWED WITH 94-CHARACTER WIDTH WINDOW
c
c Subprogram: sub2mem_mer
c   Programmer: D. Keyser       Org: NP22       Date: 2016-12-09
c
c Abstract: Takes a merged (mass and wind) aircraft data profile, containing NRLACQC events,
c   (read from input arrays *_accum), adds mandatory levels (via interpolation from spanning
c   levels) and stores it in BUFRLIB internal memory.  The calling routine will then call
c   WRITSB in order to write the profile as a single report (subset) into the output
c   PREPBUFR-like file containing merged (mass and wind) aircraft reports. Single(flight)-
c   level reports that are not part of any profile will also be processed here if namelist
c   switch l_prof1lvl=T.  A maximum of 255 level can be included in a profile report here.
c
c Program History Log:
c 2010-11-15  S. Bender  -- Original Author
c 2012-05-08  D. Keyser  -- Prepared for operational implementation
c 2012-11-20  J. Woollen -- Initial port to WCOSS
c 2013-02-07  D. Keyser  -- Final changes to run on WCOSS: use formatted print statements
c                           where previously unformatted print was > 80 characters
c 2014-12-09  Y. Zhu     -- Modified the calculation of vertical velocity rate (stored in
c                           rate_accum) still using a finite-difference method, but now
c                           calculated for both ascents and descents using the nearest
c                           neighboring pair which are at least one minute apart (before,
c                           only only be calculated for descents)
c 2014-12-09  Y. Zhu     -- Add new namelist switch "l_mandlvl" which, when F, will skip
c                           interpolation to mandatory levels
c 2014-12-09  J. Purser/Y. Zhu -- Add new namelist switch "tsplines" which, when T, will
c                           calculate vertical velocity rate (stored in rate_accum) using
c                           Jim Purser's tension-spline interpolation utility to get
c                           continuous gradient results in a profile and mitigate missing
c                           time information
c 2014-12-12  D. Keyser  -- Printout from vertical velocity rate calculation information for
c                           QC'd merged aircraft reports written to profiles PREPBUFR-like
c                           file is written to unit 41 rather than stdout.
c 2015-04-17  Y. Zhu     -- 
c                       1) This subroutine is more robust.  If there is an error in the
c                          generation of vertical velocity rate in the tension-spline
c                          interpolation utility pspl (called in this subroutine), this
c                          subroutine (and thus the program itself) will no longer abort
c                          (with either c. code 62, 63 or 64 depending upon which routine
c                          inside pspl generated the error) but will instead revert to the
c                          finite difference method for calculating vertical velocity rate.  
c                       2) Previously, halfgate was set to be 30 for the data profiles that
c                          don't have second information in time, but a tighter value of 10
c                          for the data profiles that do have second information in time. Now
c                          halfgate is relaxed to be 30 for the data profiles that do have
c                          complete time information.
c 2016-12-09  D. Keyser  --
c                 - Nomenclature change: replaced "MDCRS/ACARS" with just "MDCRS".
c                 - The format for a print statement containing latitude and longitude changed
c                   to print to 5 decimal places since some aircraft reports contain this
c                   precision.
c
c Usage: call sub2mem_mer(proflun,bmiss,mxlv,mxnmev,maxmandlvls,
c                         mandlvls,mesgtype,hdr2wrt,
c                         acid1,c_acftid1,c_acftreg1,
c                         rct_accum,drinfo_accum,acft_seq_accum,
c                         mstq_accum,cat_accum,elv_accum,rpt_accum,
c                         tcor_accum,
c     	                  pevn_accum,pbg_accum,ppp_accum,
c                         qevn_accum,qbg_accum,qpp_accum,
c                         tevn_accum,tbg_accum,tpp_accum,
c                         zevn_accum,zbg_accum,zpp_accum,
c                         wuvevn_accum,wuvbg_accum,wuvpp_accum,
c                         wdsevn_accum,mxe4prof,c_qc_accum,
c                         num_events_prof,lvlsinprof,nlvinprof,
c                         nrlacqc_pc,l_mandlvl,tsplines,l_operational,lwr)
c
c   Input argument list:
c     proflun      - Unit number for the output post-PREPACQC PREPBUFR-like file containing
c                    merged profile reports (always) and single(flight)-level reports not
c                    part of any profile (when l_prof1lvl=T) with added NRLACQC events
c                    (aircraft data only)
c     bmiss        - BUFRLIB missing value (set in main program)
c     mxlv         - Maximum number of levels allowed in a report profile
c     mxnmev       - Maximum number of events allowed, per variable type
c     maxmandlvls  - Maxmum number of mandatory pressure levels to consider for aircraft
c                    profiles
c     mandlvls     - List of mandatory pressure levels to consider for aircraft profiles
c     mesgtype     - PREPBUFR message type (AIRCAR or AIRCFT) of the profile in question
c     hdr2wrt      - Array containing header information for the profile report
c     acid1        - Aircraft flight number for the profile MDCRS report {this will be encoded
c                    into 'ACID' for MDCRS or AMDAR (LATAM only) reports in output PREPBUFR-
c                    like profiles file}
c     c_acftreg    - Aircraft tail number for the profile report as used in NRL QC processing
c                    (passed into this subroutine only for printing purposes)
c     c_acftid     - Aircraft flight number for the profile report as used in NRL QC
c                    processing (passed into this subroutine only for printing purposes)
c     rct_accum    - Array containing receipt time on all profile levels
c     drinfo_accum - Array containing drift coordinates (lat, lon, time) on all profile
c                    levels
c     acft_seq_accum - Array containing the temperature precision and flight phase on all
c                    profile levels
c     mstq_accum   - Array containing the moisture quality flag on all profile levels
c     cat_accum    - Array containing the PREPBUFR level categories on all profile levels
c     elv_accum    - Array containing elevation on all profile levels
c     rpt_accum    - Array containing reported observation time on all profile levels
c     tcor_accum   - Array containing time correction indicator on all profile levels
c     pevn_accum   - Array containing all pressure events (ob, qm, pc, rc) on all profile
c                    levels
c     pbg_accum    - Array containing pressure background information on all profile levels
c     ppp_accum    - Array containing pressure post-processing information on all profile
c                    levels
c     qevn_accum   - Array containing all moisture events (ob, qm, pc, rc) on all profile
c                    levels
c     qbg_accum    - Array containing moisture background information on all profile levels
c     qpp_accum    - Array containing moisture post-processing information on all profile
c                    levels
c     tevn_accum   - Array containing all temperature events (ob, qm, pc, rc) on all profile
c                    levels
c     tbg_accum    - Array containing temperature background information on all profile
c                    levels
c     tpp_accum    - Array containing temperature post-processing information on all profile
c                    levels
c     zevn_accum   - Array containing all altitude events (ob, qm, pc, rc) on all profile
c                    levels
c     zbg_accum    - Array containing altitude background information on all profile levels
c     zpp_accum    - Array containing altitude post-processing information on all profile
c                    levels
c     wuvevn_accum - Array containing all wind (u/v) events (ob, qm, pc, rc) on all profile
c                    levels
c     wuvbg_accum  - Array containing wind (u/v) background information on all profile levels
c     wuvpp_accum  - Array containing wind (u/v) post-processing information on all profile
c                    levels
c     wdsevn_accum - Array containing all wind (direction/speed) events (ob, qm, pc, rc) on
c                    all profile levels
c     mxe4prof     - Maximum number of events in a single-level merged report (i.e., the
c                    maximum amongst the number of pressure, moisture, temperature, altitude,
c                    u/v wind and dir/speed wind events)
c     c_qc_accum   - Array containing NQLACQC quality information 11-character strings on all
c                    profile levels
c     lvlsinprof   - Array containing a list of pressure levels that are present in the
c                    current profile
c     nlvinprof    - Number of levels in profile
c     nrlacqc_pc   - PREPBUFR program code for the NRLACQC step
c     l_mandlvl    - Logical whether to interpolate to mandatory levels in profile generation
c     tsplines     - Logical whether to use tension-splines for aircraft vertical velocity
c                    calculation
c     l_operational- Run program in operational mode if true
c     lwr          - Machine word length in bytes (either 4 or 8)
c
c   Output argument list:
c     hdr2wrt      - Array containing header information for the profile report (TYP undated,
c                    (also changed to highest/lowest pressure level for ascents/descents)
c     num_events_prof - Total number of events on an ob, across all levels, across all
c                    reports (to this point), written into the PREPBUFR-like file (this value
c                    is the same for each ob type)
c     lvlsinprof   - Array containing a list of pressure levels that are present in the
c                    current profile (now possibly also contains mandatory levels)
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
c     Unique:    none
c     Library:
c       SYSTEM:  SYSTEM
c       BUFRLIB: UFBINT      IBFMS
c       W3NCO:   W3TAGE      ERREXIT
c       W3EMC:   ORDERS
c
c   Exit States:
c     Cond =  0 - successful run
c            59 - nlvinprof is zero coming into this subroutine (should never happen!)
c            61 - index "j is .le. 1 meaning "iord" array underflow (should never happen!)
c
c Remarks: Called by subroutine output_acqc_prof.
c
c Attributes:
c   Language: FORTRAN 90
c   Machine:  NCEP WCOSS
c
c$$$
      subroutine sub2mem_mer(proflun,bmiss,mxlv,mxnmev,maxmandlvls,
     +                       mandlvls,mesgtype,hdr2wrt,
     +                       acid1,c_acftid1,c_acftreg1,
     +                       rct_accum,drinfo_accum,acft_seq_accum,
     +                       mstq_accum,cat_accum,elv_accum,rpt_accum,
     +                       tcor_accum,
     +	                     pevn_accum,pbg_accum,ppp_accum,
     +                       qevn_accum,qbg_accum,qpp_accum,
     +                       tevn_accum,tbg_accum,tpp_accum,
     +                       zevn_accum,zbg_accum,zpp_accum,
     +                       wuvevn_accum,wuvbg_accum,wuvpp_accum,
     +                       wdsevn_accum,mxe4prof,c_qc_accum,
     +                       num_events_prof,lvlsinprof,nlvinprof,
     +                       nrlacqc_pc,l_mandlvl,tsplines,
     +                       l_operational,lwr)

      use pkind, only: dp
      use pspl, only: bnewton,best_slalom,count_gates,convertd,
     +                convertd_back
      implicit none

c ------------------------------
c Parameter statements/constants
c ------------------------------
      integer      proflun             ! output unit number for post-PREPACQC PREPBUFR-like
                                       !  file containing merged profile reports (always) and
                                       !  single(flight)-level reports not part of any
                                       !  profile (when l_prof1lvl=T) with added NRLACQC
                                       !  events

      real*8       bmiss               ! BUFRLIB missing value (set in main program)

c Variables used to write data to output PREPBUFR-like file in sorted order
c -------------------------------------------------------------------------
      integer      mxlv                ! maximum number of report levels allowed in aircraft
                                       !  profiles
      character*6  cmxlv               ! character form of mxlv

      integer      mxnmev              ! maximum number of events allowed in stack
     +,            lvlsinprof(mxlv)    ! array containing a list of pressure levels that are
                                       !  present in the current profile (later changed to
                                       !  add mandatory levels)
     +,            mxe4prof            ! maximum number of events in a single-level merged
                                       !  report (i.e., the maximum amongst the number of
                                       !  pressure, moisture, temperature, altitude, u/v wind
                                       !  and dir/speed wind events)
     +,            nlvinprof           ! number of levels in a profile upon input

      real*8   hdr2wrt(15)             ! header info for current profile (passed in)
     +,        drinfo_accum(3,mxlv)    ! array used to accumulate drift info across profile
                                       !  levels
     +,        acft_seq_accum(2,mxlv)  ! array used to accumulate ACFT_SEQ (PCAT -temperature
                                       !  precision, POAF - phase of flight) info across
                                       !  profile levels
     +,        mstq_accum(1,mxlv)      ! array used to accumulate moisture QC marks across
                                       !  profile levels
     +,        cat_accum(1,mxlv)       ! array used to accumulate level category markers
                                       !  across profile levels
     +,        elv_accum(1,mxlv)       ! array used to accumulate elevation across profile
                                       !  levels
     +,        rpt_accum(1,mxlv)       ! array used to accumulate reported obs time across
                                       !  profile levels
     +,        tcor_accum(1,mxlv)      ! array used to accumulate time correction factor
                                       !  across profile levels
     +,        rct_accum(1,mxlv)       ! array used to accumulate receipt time across profile
                                       !  levels

      real*8   pevn_accum(4,mxlv,mxnmev)! array used to accumulate pressure data/events for a
                                        !  single profile, across profile levels
     +,        pbg_accum(3,mxlv)        ! array used to accumulate pressure background info
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
     +,        tbg_accum(3,mxlv)        ! array used to accumulate temperature background
                                        !  info (TOE, TFC, TFCMOD) for a single profile,
                                        !  across profile levels
     +,        tpp_accum(3,mxlv)        ! array used to accumulate temperature post-
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

c Logicals controlling processing (not read in from namelist in main program)
c ---------------------------------------------------------------------------
      logical l_mandlvl                ! T=interpolate to mandatory levels in profile
                                       !   generation
                                       ! F=do not interpolate to mandatory levels in profile
                                       !   generation
      logical tsplines                 ! T=use tension-splines for aircraft vertical velocity
                                       !   calculation
                                       ! F=use finite-differencing for aircraft vertical
                                       !   velocity calculation
      logical l_operational            ! Run program  in operational mode if true

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
     +,       mandlvls(maxmandlvls)    ! list of mandatory pressure levels to consider for
                                       !  aircraft profiles
     +,       nmandlvls                ! number of mandatory levels interpolated for this
                                       !  profile
     +,       nmNbtw                   ! number of mandatory levels between "bread of the
                                       !  sandwich" reports

      character*8 mesgtype             ! BUFR message type (e.g., 'AIRCFT  ')

      real*8      acid1                ! aircraft flight number for the profile MDCRS or AMDAR
                                       !  (LATAM only) report
     +,           acid_arr1            ! used with ufbint routine to encode aircraft flight
                                       !  number (ACID) into MDCRS or AMDAR (LATAM only)
                                       !  reports in output PREPBUFR-like file)

      character*9  c_acftid1           ! aircraft flight number (as processed by NRLACQC)
                                       !  for the profile report (used for printing purposes
                                       !  only)

      character*8  c_acftreg1          ! aircraft tail number (as processed by NRLACQC)
                                       !  for the profile report (used for printing purposes
                                       !  only)

      integer     nlv2wrt_tot          ! total number of levels to write in this profile,
                                       !  including any interpolated mandatory levels
      character*6 cnlv2wrt_tot         ! character form of nlv2wrt_tot

      integer     nlv2wrt              ! number of levels in profile to write to output	
      character*6 cnlv2wrt             ! character form of nlv2wrt

      integer     nlvwrt               ! number of levels written to output PREPBUFR-like
                                       !  file

      real*8      pevns4(4,mxlv)       ! array used with ufbint routine to encode pressure
                                       !  events into output PREPBUFR-like file
     +,           qevns4(4,mxlv)       ! array used with ufbint routine to encode moisture
                                       !  events into output PREPBUFR-like file
     +,           tevns4(4,mxlv)       ! array used with ufbint routine to encode temperature
                                       !  events into output PREPBUFR-like file
     +,           zevns4(4,mxlv)       ! array used with ufbint routine to encode altitude
                                       !  events into output PREPBUFR-like file
     +,           wuvevns5(5,mxlv)     ! array used with ufbint routine to encode wind (u/v
                                       !  component) events into output PREPBUFR-like file
     +,	          wdsevns5(5,mxlv)     ! array used with ufbint routine to encode wind
                                       !  (direction/speed) events into output PREPBUFR-like
                                       !  file

c For background/post-processing info
c -----------------------------------
      real*8      pbgarr3(3,mxlv)      ! array used with ufbint routine to encode pressure
                                       !  background info into output PREPBUFR-like file
     +,	          qbgarr3(3,mxlv)      ! array used with ufbint routine to encode moisture
                                       !  background info into output PREPBUFR-like file
     +,	          tbgarr3(3,mxlv)      ! array used with ufbint routine to encode temperature
                                       !  background info into output PREPBUFR-like file
     +,	          zbgarr3(3,mxlv)      ! array used with ufbint routine to encode altitude
                                       !  background info into output PREPBUFR-like file
     +,	          wuvbgarr5(5,mxlv)    ! array used with ufbint routine to encode wind (u/v
                                       !  component) background info into output PREPBUFR-
                                       ! like file
     +,           ppparr3(3,mxlv)      ! array used with ufbint routine to encode pressure
                                       !  post-processing info into output PREPBUFR-like file
     +,           qpparr3(3,mxlv)      ! array used with ufbint routine to encode moisture
                                       !  post-processing info into output PREPBUFR-like file
     +,	          tpparr3(3,mxlv)      ! array used with ufbint routine to encode temperature
                                       !  post-processing info into output PREPBUFR-like file
     +,	          zpparr3(3,mxlv)      ! array used with ufbint routine to encode altitude
                                       !  post-processing info into output PREPBUFR-like file
     +,	          wuvpparr6(6,mxlv)    ! array used with ufbint routine to encode wind (u/v
                                       !  component) info into output PREPBUFR-like file
     +,	          drarr3(3,mxlv)       ! array used with ufbint routine to encode drift info
                                       !  into output PREPBUFR-like file
     +,	          acft_seq_arr2(2,mxlv)! array used with ufbint routine to encode PCAT, POAF
                                       !  into output PREPBUFR-like file
     +,	          mstq_arr1(1,mxlv)    ! array used with ufbint routine to encode moisture QC
                                       !  flag into output PREPBUFR-like file
     +,	          cat_arr1(1,mxlv)     ! array used with ufbint routine to encode level
                                       !  category info into output PREPBUFR-like file
     +,           rct_arr1(1,mxlv)     ! array used with ufbint routine to encode level
                                       !  receipt time info into output PREPBUFR-like file
     +,	          ialr_arr1(1,mxlv)    ! array used with ufbint routine to encode ascent/
                                       !  descent rate into output PREPBUFR-like file
     +,	          turb_arr4(4,mxlv)    ! array used with ufbint routine to encode turbulence
                                       !  data into output PREPBUFR-like file

C Arrays associated with sorting of data
c --------------------------------------
      integer  iwork(mxlv)             ! work array
     +,        iord(mxlv)              ! array containing sorted index

C Loop indices
c ------------
      integer  i,j,k,l                 ! original (unsorted) indices
     +,        iii                     ! index
     +,        jj                      ! sorted (pressure low->high) index pointing to lvl j
     +,        jjp1                    ! sorted index pointing to next level below jj
     +,        jjm1                    ! sorted index pointing to previous level above jj
     +,        jjp2                    ! sorted index pointing to next level below jjp1
     +,        jjm2                    ! sorted index pointing to previous level above jjm1
     +,        jjmaxp                  ! sorted index pointing to level jj with max pressure
     +,        jjminp                  ! sorted index pointing to level jj with min pressure
     +,        jjpnmnbtw               ! sorted index pointing to next level below jj that is
                                       !  not a mandatory pressure level
     +,        jk                      ! index,
     +,        c1_jk                   ! index,
     +,        c2_jk                   ! index,
     +,        jkp                     ! index,
     +,        jkm                     ! index, 
     +,        jjp                     ! index,
     +,        jjm                     ! index,
     +,        kk                      ! sorted (pressure low->high) index pointing to lvl k
     +,        jjpk                    ! sorted index pointing to level jj plus k

      real     pul                     ! pressure ob at level "below" mandatory level (higher
                                       !  pressure, lower altitude)
     +,        pll                     ! pressure ob at level "above" mandatory level (lower
                                       !  pressure, higher altitude)
     +,        pqul                    ! pressure qm at level "below" mandatory level (higher
                                       !  pressure, lower altitude)
     +,        pqll                    ! pressure qm at level "above" mandatory level (lower
                                       !  pressure, higher altitude)
     +,        pml                     ! pressure ob at mandatory level
     +,        tul                     ! temperature ob at level "below" mandatory level
                                       !  (higher pressure, lower altitude)
     +,        tll                     ! temperature ob at level "above" mandatory level
                                       !  (lower pressure, higher altitude)
     +,        tqul                    ! temperature qm at level "below" mandatory level
                                       !  (higher pressure, lower altitude)
     +,        tqll                    ! temperature qm at level "above" mandatory level
                                       !  (lower pressure, higher altitude)
     +,	       tml                     ! temperature ob at mandatory level
     +,	       dt_dlnp                 ! change in temperature w.r.t. change in log-pressure
     +,	       qul                     ! moisture ob at level "below" mandatory level (higher
                                       !  pressure, lower altitude)
     +,	       qll                     ! moisture ob at level "above" mandatory level (lower
                                       !  pressure, higher altitude)
     +,	       qqul                    ! moisture qm at level "below" mandatory level (higher
                                       !  pressure, lower altitude)
     +,        qqll                    ! moisture qm at level "above" mandatory level (lower
                                       !  pressure, higher altitude)
     +,	       qml                     ! moisture ob at mandatory level
     +,	       dq_dlnp                 ! change in moisture w.r.t. change in log-pressure
     +,	       zul                     ! altitude ob at level "below" mandatory level (higher
                                       !  pressure, lower altitude)
     +,	       zll                     ! altitude ob at level "above" mandatory level (lower
                                       !  pressure, higher altitude)
     +,	       zqul                    ! altitude qm at level "below" mandatory level (higher
                                       !  pressure, lower altitude)
     +,        zqll                    ! altitude qm at level "above" mandatory level (lower
                                       !  pressure, higher altitude)
     +,        zml                     ! altitude ob at mandatory level
     +,        dz_dlnp                 ! change in altitude w.r.t. change in log-pressure
     +,        uul                     ! u-comp of wind ob at level "below" mandatory level
                                       !  (higher pressure, lower altitude)
     +,        ull                     ! u-comp of wind ob at level "above" mandatory level
                                       !  (lower pressure, higher altitude)
     +,        uml                     ! u-comp of wind ob at mandatory level
     +,        du_dlnp                 ! change in u-comp of wind w.r.t. change in
                                       !  log-pressure
     +,        vul                     ! v-comp of wind ob at level "below" mandatory level
                                       !  (higher pressure, lower altitude)
     +,        vll                     ! v-comp of wind ob at level "above" mandatory level
                                       !  (lower pressure, higher altitude)
     +,        vml                     ! v-comp of wind ob at mandatory level
     +,        dv_dlnp                 ! change in v-comp of wind w.r.t. change in
                                       !  log-pressure
     +,        uvqul                   ! u/v-comp of wind qm at level "below" mandatory level
                                       !  (higher pressure, lower altitude)
     +,        uvqll                   ! u/v-comp of wind qm at level "above" mandatory level
                                       !  (lower pressure, higher altitude)

      integer  ibfms                   ! BUFRLIB function for testing for missing

      real*8   dtime_dlnp              ! change in time w.r.t. change in log-pressure

      real     dist_pul_pll            ! horizontal distance traveled when going from point
                                       !  at pll to pul
     +,	       spd_pul_pll             ! average speed while traveling from point at pll to
                                       !  pul
     +,        dist2pml                ! horizontal distance traveled when going from point
                                       !  at pll to pml
! vvvv DAK-future change perhaps to account for incr. lat/lon precision
     +,        lat_pul                 ! latitude at data level "below" mandatory level
                                       !  (higher pressure, lower altitude)
     +,        lon_pul                 ! longitude at data level "below" mandatory level
                                       !  (higher pressure, lower altitude)
     +,        lat_pll                 ! latitude at data level "above" mandatory level
                                       !  (lower pressure, higher altitude)
     +,        lon_pll                 ! longitude at data level "above" mandatory level
                                       !  (lower pressure, higher altitude)
! ^^^^ DAK-future change perhaps to account for incr. lat/lon precision
     +,        radius_e                ! radius of the earth in meters
     +,        deg2rad                 ! conversion factor for converting degrees -> radians

      parameter(radius_e = 6371229.)
      parameter(deg2rad = 3.14159274/180.)

      real*8   delx                    ! change in longitude/nmNbtw
     +,        dely                    ! change in latitude/nmNbtw
     +,        dt                      ! delta time (sec) between two levels, used to
                                       !  calculate instantaneous altitude (ascent/descent)
                                       !  rate
     +,        dt_new                  ! delta time
     +,        rate_accum(mxlv)        ! array of instantaneous altitude (ascent/descent)
                                       !  rates on all levels of profile

c Variables used in printing values for a particular report and level
c -------------------------------------------------------------------
      integer  ihdr2wrt9               ! PREPBUFR instrument type ("ITP" from header)
     +,        iacft_seq_accum2        ! temperature precision, and phase of flight
     +,        idrinfo_accum3          ! drift information
     +,        izevn_accum1            ! altitude ob
     +,        iwdsevn_accum1          ! wind direction ob
     +,        ipevn_accum2            ! pressure quality mark
     +,        izevn_accum2            ! altitude quality mark
     +,        itevn_accum2            ! temperature quality mark
     +,        iqevn_accum2            ! moisture quality mark
     +,        iwuvevn_accum3          ! wind quality mark
     +,        ipevn_accum4            ! pressure reason code 
     +,        izevn_accum4            ! altitude reason code
     +,        itevn_accum4            ! temperature reason code
     +,        iqevn_accum4            ! moisture code
     +,        iwuvevn_accum5          ! wind reason code
     +,        nevents_t               ! number of events on temperature
     +,        nevents_q               ! number of events on moisture
     +,        nevents_w               ! number of events on wind
     +,        imstq_accum1            ! moisture qc flag
     +,        icat_accum1             ! PREPBUFR level category ("CAT")
     +,        ihdr2wrt6               ! PREPBUFR report type ("TYP" from header)

      real*8       wspd                ! wind speed ob
     +,            q_sphum             ! moisture (specific humidity) ob
     +,            hdr2wrt1            ! real form of PREPBUFR report id ("SID" from header)

c Misc.
c -----
      real        nrlacqc_pc           ! PREPBUFR program code for the NRLACQC step

      integer     lwr                  ! machine word length in bytes (either 4 or 8)

c Variables related to tspline
      integer, parameter:: nit=30
!     real(dp),parameter:: bigT=120.0,halfgate=30.0,heps=.01
      real(dp),parameter:: bigT=120.0,heps=.01
      integer nh,nh2,m,mh,maxita,maxitb,maxit,maxrts,doru
      integer err_tspline
      real(dp) enbest,timemin
      real(dp) halfgate
      integer, allocatable :: idx(:)
      integer, allocatable :: modebest(:)
      integer, allocatable :: pof(:)
      integer, allocatable :: hgts(:)
      integer, allocatable :: hgtp(:)
      real, allocatable :: tdata(:),hdata(:),wdata(:)
      real(dp), allocatable :: te(:),hs(:),dhdt(:)
      real(dp), allocatable :: hp(:)
      real(dp), allocatable :: qbest(:),habest(:)
      logical descending,FF,nearsec

c ----------------------------------------------------

c Start program
c -------------
ccc   print *, 'in sub2mem_mer for the next merged report'

      rate_accum = bmiss

      if(nlvinprof.eq.0) then
        print *
        print *, '### PROBLEM - into subr, sub2mem_mer with nlvinprof ',
     +           '= ',0
        print *, '              this should never happen!!'
        print *
        call w3tage('PREPOBS_PREPACQC')
        call errexit(59)
      endif

c First sort pressures from lowest to highest, this will also determine the maximum and
c  minimum pressure values in this profile
c -------------------------------------------------------------------------------------
      call orders(1,iwork,lvlsinprof,iord,nlvinprof,1,lwr,2)

ccc   print *, '.. there are originally ',nlvinprof,' p-levels in this',
ccc  +         ' report'

c Interpolate z,t,q,u,v values to mandatory levels - include the levels of 1000, 850, 700,
c  500, 400, 300, 200, 150 and 100 mb in the acceptable mandatory levels for aircraft
c  profiles (not many aircraft flying above 100 mb!)
c ---------------------------------------------------------------------------------------
      nmandlvls = 0
      nlv2wrt_tot = nlvinprof

      if(l_mandlvl .and. nlvinprof.gt.1) then ! do interpolation only for profiles with
                                              !  more than one report!
        loop1: do i = 1,maxmandlvls   ! maxmandlvls=9 - number of mandatory levels to check
          do j = 1,nlvinprof          !  levels will appear in increasing order via index
                                      !  jj... first level might be 247 mb, second might be
                                      !  427 mb, etc.
            jj = iord(j)
            jjp1 = iord(j+1)

            if(j.lt.nlvinprof) then   ! exclude last level in profile (one closest to the
                                      !  ground) (use .lt. instead of .le. to do this); only
                                      !  interpolate for mandatory levels sandwiched by
                                      !  actual data

c Below, jj points to level at a lower pressure/higher altitude and jjp1 points to the
c  adjacent level at a higher pressure, lower altitude)
c ------------------------------------------------------------------------------------
              if(lvlsinprof(jj)  .lt.mandlvls(i) .and.
     +	         lvlsinprof(jjp1).gt.mandlvls(i)) then
 
                if(nlvinprof+nmandlvls+1.gt.mxlv) then
C.......................................................................
C There are more levels in profile than "mxlv" -- do not process any more levels
C ------------------------------------------------------------------------------
                  print 53, mxlv,mxlv
   53 format(/' #####> WARNING: THERE ARE MORE THAN ',I6,' LEVELS IN ',
     + 'THIS PROFILE -- WILL CONTINUE ON PROCESSING ONLY ',I6,' LEVELS',
     + ' FOR THIS PROFILE'/)
                  write(cmxlv,'(i6)') mxlv
                  call system('[ -n "$jlogfile" ] && $DATA/postmsg'//
     +             ' "$jlogfile" "***WARNING:'//cmxlv//' AIRCRAFT '//
     +             'PROFILE LEVEL LIMIT EXCEEDED IN '//
     +             'PREPOBS_PREPACQC, ONLY '//cmxlv//' LEVELS '//
     +             'PROCESSED"')
                  exit loop1
C.......................................................................
                endif

                nmandlvls = nmandlvls + 1

c Now calculate values on mandlvls(i) using values at lvlsinprof(j) (ll/lower level and (j+1)
c  (ul/upper level) - USE REASON CODE 98 FOR INTERPOLATED MANDATORY LEVELS (use highest
c  quality mark amongst lower and upper levels)
c -------------------------------------------------------------------------------------------
                pll  = lvlsinprof(jj)       ! pressure ob at level "above" mandatory level
                pul  = lvlsinprof(jjp1)     ! pressure ob at level "below" mandatory level
                pqll = pevn_accum(2,jj,1)   ! pressure qm at level "above" mandatory level
                pqul = pevn_accum(2,jjp1,1) ! pressure qm at level "below" mandatory level
                pml = mandlvls(i)           ! pressure at mandatory level

                lvlsinprof(nlvinprof+nmandlvls) = mandlvls(i)
                pevn_accum(1,nlvinprof+nmandlvls,1) = pml/10.           ! POB
                pevn_accum(2,nlvinprof+nmandlvls,1) = max(pqll,pqul)    ! PQM 
                pevn_accum(3,nlvinprof+nmandlvls,1) = nrlacqc_pc        ! PPC
                pevn_accum(4,nlvinprof+nmandlvls,1) = 98                ! PRC

                cat_accum(1,nlvinprof+nmandlvls) = 7 ! interpolated mand. levels get CAT = 7

c Temperature
c -----------
                if(ibfms(tevn_accum(1,jj,1)).eq.0 .and.
     +             ibfms(tevn_accum(1,jjp1,1)).eq.0 ) then ! temperature isn't missing
                  do iii = mxe4prof,1,-1
                    if(ibfms(tevn_accum(1,jj,iii)).ne.0) then
                      nevents_t = iii
                    else
                      nevents_t = iii
                      exit
                    endif
                  enddo
                  tll  = tevn_accum(1,jj,nevents_t)   ! temp ob at lvl "above" mandatory level
                  tqll = tevn_accum(2,jj,nevents_t)   ! temp qm at lvl "above" mandatory level
                  do iii = mxe4prof,1,-1
                    if(ibfms(tevn_accum(1,jjp1,iii)).ne.0) then
                      nevents_t = iii
                    else
                      nevents_t = iii
                      exit
                    endif
                  enddo
                  tul  = tevn_accum(1,jjp1,nevents_t) ! temp ob at lvl "below" mandatory level
                  tqul = tevn_accum(2,jjp1,nevents_t) ! temp qm at lvl "below" mandatory level
ccccc             print *, 'pmd, pll, pul, tqll,tqul: ',pml, pll, pul,
ccccc+                      tqll,tqul

                  dt_dlnp = (tul - tll)/alog(pul/pll)

                  tml = tll + (dt_dlnp * (alog(pml/pll)))

                  tevn_accum(1,nlvinprof+nmandlvls,1) = tml             ! TOB
                  tevn_accum(2,nlvinprof+nmandlvls,1) = max(tqll,tqul)  ! TQM 
                  tevn_accum(3,nlvinprof+nmandlvls,1) = nrlacqc_pc      ! TPC
                  tevn_accum(4,nlvinprof+nmandlvls,1) = 98              ! TRC

                endif ! temps missing?

c Moisture
c --------
                if(ibfms(qevn_accum(1,jj,1)).eq.0 .and.
     +             ibfms(qevn_accum(1,jjp1,1)).eq.0 ) then ! moisture isn't missing
                  do iii = mxe4prof,1,-1
                    if(ibfms(qevn_accum(1,jj,iii)).ne.0) then
                      nevents_q = iii
                    else
                      nevents_q = iii
                      exit
                    endif
                  enddo
                  qll  = qevn_accum(1,jj,nevents_q)   ! q ob at level "above" mandatory level
                  qqll = qevn_accum(2,jj,nevents_q)   ! q qm at level "above" mandatory level
                  do iii = mxe4prof,1,-1
                    if(ibfms(qevn_accum(1,jjp1,iii)).ne.0) then
                      nevents_q = iii
                    else
                      nevents_q = iii
                      exit
                    endif
                  enddo
                  qul  = qevn_accum(1,jjp1,nevents_q) ! q ob at level "below" mandatory level
                  qqul = qevn_accum(2,jjp1,nevents_q) ! q qm at level "below" mandatory level

                  dq_dlnp = (qul - qll)/alog(pul/pll)

                  qml = qll + (dq_dlnp * (alog(pml/pll)))

                  qevn_accum(1,nlvinprof+nmandlvls,1) = qml             ! QOB
                  qevn_accum(2,nlvinprof+nmandlvls,1) = max(qqll,qqul)  ! QQM 
                  qevn_accum(3,nlvinprof+nmandlvls,1) = nrlacqc_pc      ! QPC
                  qevn_accum(4,nlvinprof+nmandlvls,1) = 98              ! QRC

                else ! if moisture missing, check to see if QFC is present for "bread"
                     !  levels; if so, interpolate QFC
                  if(ibfms(qbg_accum(2,jj)).eq.0 .and.
     +	             ibfms(qbg_accum(2,jjp1)).eq.0 ) then ! QFC isn't missing for "bread"
                                                          !  levels
                    qll = qbg_accum(2,jj)     ! QFC at ob level "above" mandatory level
                    qul = qbg_accum(2,jjp1)   ! QFC at ob level "below" mandatory level

                    dq_dlnp = (qul - qll)/alog(pul/pll)

                    qml = qll + (dq_dlnp * (alog(pml/pll)))

                    qbg_accum(2,nlvinprof+nmandlvls) = qml ! QFC

                  endif ! is QFC present for "bread" levels when moisture missing?
                endif ! moisture missing?

c Altitude
c --------
                if(ibfms(zevn_accum(1,jj,1)).eq.0 .and.
     +             ibfms(zevn_accum(1,jjp1,1)).eq.0 ) then ! altitude isn't missing
                  zll  = zevn_accum(1,jj,1)           ! z ob at level "above" mandatory level
                  zul  = zevn_accum(1,jjp1,1)         ! z ob at level "below" mandatory level
                  zqll = zevn_accum(2,jj,1)           ! z qm at level "above" mandatory level
                  zqul = zevn_accum(2,jjp1,1)         ! z qm at level "below" mandatory level

                  dz_dlnp = (zul - zll)/alog(pul/pll)

                  zml = zll + (dz_dlnp * (alog(pml/pll)))

                  zevn_accum(1,nlvinprof+nmandlvls,1) = zml             ! ZOB
                  zevn_accum(2,nlvinprof+nmandlvls,1) = max(zqll,zqul)  ! ZQM 
                  zevn_accum(3,nlvinprof+nmandlvls,1) = nrlacqc_pc      ! ZPC
                  zevn_accum(4,nlvinprof+nmandlvls,1) = 98              ! ZRC

                endif ! altitude missing?

c u- and v- components of wind
c ----------------------------
                if(ibfms(wuvevn_accum(1,jj,1)).eq.0 .and.
     +             ibfms(wuvevn_accum(1,jjp1,1)).eq.0 .and.
     +             ibfms(wuvevn_accum(2,jj,1)).eq.0 .and.
     +             ibfms(wuvevn_accum(2,jjp1,1)).eq.0) then ! u and v aren't missing
                  do iii = mxe4prof,1,-1
                    if(ibfms(wuvevn_accum(1,jj,iii)).ne.0 .or.
     +                 ibfms(wuvevn_accum(2,jj,iii)).ne.0) then
                      nevents_w = iii
                    else
                      nevents_w = iii
                      exit
                    endif
                  enddo
                  ull   = wuvevn_accum(1,jj,nevents_w)  ! UOB ob at lvl "above" mandatory lvl
                  vll   = wuvevn_accum(2,jj,nevents_w)  ! VOB ob at lvl "above" mandatory lvl
                  uvqll = wuvevn_accum(3,jj,nevents_w)  ! UOB/VOB qm at lvl "above" mandatory
                                                        !  lvl
                  do iii = mxe4prof,1,-1
                    if(ibfms(wuvevn_accum(1,jjp1,iii)).ne.0 .or.
     +                ibfms(wuvevn_accum(2,jjp1,iii)).ne.0) then
                      nevents_w = iii
                    else
                      nevents_w = iii
                      exit
                    endif
                  enddo
                  uul   = wuvevn_accum(1,jjp1,nevents_w) ! UOB ob at lvl "below" mandatory lvl
                  vul   = wuvevn_accum(2,jjp1,nevents_w) ! VOB ob at lvl "below" mandatory lvl
                  uvqul = wuvevn_accum(3,jjp1,nevents_w) ! UOB/VOB qm at lvl "below" mandatory
                                                         !  lvl

                  du_dlnp = (uul - ull)/alog(pul/pll)
                  dv_dlnp = (vul - vll)/alog(pul/pll)

                  uml = ull + (du_dlnp * (alog(pml/pll)))
                  vml = vll + (dv_dlnp * (alog(pml/pll)))

                  wuvevn_accum(1,nlvinprof+nmandlvls,1) = uml           ! UOB
                  wuvevn_accum(2,nlvinprof+nmandlvls,1) = vml           ! VOB
                  wuvevn_accum(3,nlvinprof+nmandlvls,1) =
     +             max(uvqll,uvqul)                                     ! WQM 
                  wuvevn_accum(4,nlvinprof+nmandlvls,1) = nrlacqc_pc    ! WPC
                  wuvevn_accum(5,nlvinprof+nmandlvls,1) = 98            ! WRC

                endif ! wind missing?

              endif ! calc values for this mandatory level?
            endif ! j.lt.nlvinprof
          enddo ! j = 1,nlvinprof
        enddo loop1 ! i = 1,maxmandlvls

        nlv2wrt_tot = nlvinprof + nmandlvls
ccc     print'(" .. there are eventually ",I0," p-levels in this ",
ccc  +         "report (incl. mand. levels to which obs interp. to)")',
ccc  +       nlv2wrt_tot

c Re-sort pressures (now with mandatory levels inclded) from lowest to highest
c ----------------------------------------------------------------------------
        call orders(1,iwork,lvlsinprof,iord,nlv2wrt_tot,1,lwr,2)

      end if ! l_mandlvl .and. nlvinprof.gt.1

c -----------------------------------------
c Calculate vertical velocity rate_accum
c add ascent/descent rate here
c -----------------------------------------
      write(41,*) 'nlv2wrt_tot=', nlv2wrt_tot,'c_acftreg=',c_acftreg1
      err_tspline = 0

      if ((nlv2wrt_tot.gt.1) .and. tsplines) then
         nh = 0
         do j = 1,nlv2wrt_tot
            jj = iord(j)
            if (ibfms(drinfo_accum(3,jj)).eq.0) then
               nh = nh + 1
c              write(41,*) 'j,ord,z,t=', j, jj,zevn_accum(1,jj,1),
c    +                 drinfo_accum(3,jj)
            end if
         end do
         nh2 = nh * 2

         halfgate=30.0
!        nearsec=.false.
!        do j = 1,nlv2wrt_tot
!           jj = iord(j)
!           if (ibfms(drinfo_accum(3,jj)).eq.0) then
!              timemin=drinfo_accum(3,jj)*60.0
!              timemin=abs(timemin-nint(timemin))
!              if (timemin>=0.01 .and. timemin<=0.99) nearsec=.true.
!           end if
!        end do
!        if (nearsec) halfgate=10.0
         write(41,*) 'halfgate=', halfgate

         allocate(idx(nh),pof(nh))
         allocate(tdata(nh),hdata(nh),wdata(nh))
         allocate(te(nh),hgts(nh),hs(nh),dhdt(nh))
         maxita = 0
         maxitb = 0
         maxrts = 0
         maxit  = 0

         nh = 0
         do j = 1,nlv2wrt_tot
            jj = iord(j)
            if (ibfms(drinfo_accum(3,jj)).eq.0) then
               nh = nh + 1
               tdata(nh) = drinfo_accum(3,jj) ! hours
               hdata(nh) = zevn_accum(1,jj,1) ! meters
               pof(nh)   = nint(acft_seq_accum(2,jj))
               write(41,*) 'tdata,hdata,pof=',nh,tdata(nh),hdata(nh),
     +          pof(nh)
            end if
         end do

c        arrange data with time increase
         call convertd(nh,halfgate,tdata,hdata,pof,
     +        doru,idx,hgts,hs,descending,FF)
!!!!!!!! if (FF) call w3tage('PREPOBS_PREPACQC')
!!!!!!!! if (FF) call errexit(62)
         if (FF) then 
c Error generating vertical velocity rate in tension-spline interpolation utility pspl
c  (coming out of subroutine convertd) - use finite difference method
c ------------------------------------------------------------------------------------
            print*,"WARNING: tspline err in utility pspl, coming out ",
     +             "of subr. convertd - use finite difference method" 
            write(41,*)"WARNING: tspline err in utility pspl, coming ",
     +                 "out of subr. convertd - use finite difference ",
     +                 "method" 
            err_tspline = 1
            go to 666
         end if
         if (descending)then
            write(41,'('' set descending'')')
         else
            write(41,'('' set ascending'')')
         endif

         call count_gates(nh,hgts(1:nh),mh)
         m = mh*2
         allocate(hgtp(m),hp(m),qbest(m),habest(m),modebest(mh))
         call best_slalom(nh,mh,doru,hgts,hs,halfgate,bigT,hgtp,hp,
     +     qbest,habest,enbest,modebest,maxita,maxitb,maxit,maxrts,FF)
          write(41,*) 'maxita,maxitb,maxit,maxrts=',maxita,maxitb,maxit,
     +     maxrts
!!!!!!!! if (FF) call w3tage('PREPOBS_PREPACQC')
!!!!!!!! if (FF) call errexit(63)
         if (FF) then 
c Error generating vertical velocity rate in tension-spline interpolation utility pspl
c  (coming out of subroutine best_slalom) - use finite difference method
c ------------------------------------------------------------------------------------
            print*,"WARNING: tspline err in utility pspl, coming out ",
     +             "of subr. best_slalom - use finite difference method" 
            write(41,*)"WARNING: tspline err in utility pspl, coming ",
     +                 "out of subr. best_slalom - use finite ",
     +                 "difference method" 
            err_tspline = 1
            go to 666
         end if

c        Use bounded Newton iterations to estimate the vertical velocity
         call bnewton(nh,m,bigT,halfgate,hgts,hs,hgtp,habest,
     +        qbest,te(1:nh),dhdt(1:nh),FF)
!!!!!!!! if (FF) call w3tage('PREPOBS_PREPACQC')
!!!!!!!! if (FF) call errexit(64)
         if (FF) then 
c Error generating vertical velocity rate in tension-spline interpolation utility pspl
c  (coming out of subroutine bnewton) - use finite difference method
c ------------------------------------------------------------------------------------
            print*,"WARNING: tspline err in utility pspl, coming out ",
     +             "of subr. bnewton - use finite difference method" 
            write(41,*)"WARNING: tspline err in utility pspl, coming ",
     +                 "out of subr. bnewton - use finite difference ",
     +                 "method" 
            err_tspline = 1
            go to 666
         end if

c        convert back data with time decrease for ascending
         call convertd_back(nh,halfgate,wdata,tdata,dhdt,hgts,idx,
     +                      descending)
         do j = 1, nh
            write(41,*) 'hgts,hs,dhdt,wdata=', j,hgts(j),hs(j),dhdt(j),
     +       wdata(j)
         end do

c        Encode dhdt into PREPBUFR-like file as IALR
         nh = 0
         do j = 1,nlv2wrt_tot
            jj = iord(j)
            if (ibfms(drinfo_accum(3,jj)).eq.0) then
               nh = nh + 1
               rate_accum(jj) = wdata(nh)
               write(41,*) 'j,z,rate=',j,zevn_accum(1,jj,1),
     +          rate_accum(jj)
            end if
         end do

 666     continue

         if(allocated(idx)) deallocate(idx)
         if(allocated(pof)) deallocate(pof)
         if(allocated(tdata)) deallocate(tdata)
         if(allocated(hdata)) deallocate(hdata)
         if(allocated(wdata)) deallocate(wdata)
         if(allocated(te)) deallocate(te)
         if(allocated(hgts)) deallocate(hgts)
         if(allocated(hs)) deallocate(hs)
         if(allocated(dhdt)) deallocate(dhdt)
         if(allocated(hgtp)) deallocate(hgtp)
         if(allocated(hp)) deallocate(hp)
         if(allocated(qbest)) deallocate(qbest)
         if(allocated(habest)) deallocate(habest)
         if(allocated(modebest)) deallocate(modebest)
      end if ! nlv2wrt_tot.gt.1 .and. tsplines

      if (((nlv2wrt_tot.gt.1) .and. (.not.tsplines)) 
     +                           .or. err_tspline>0) then
        do j = 1,nlv2wrt_tot
          jj = iord(j)
          write(41,*) 'j,ord,z,t,pof=', j, jj,zevn_accum(1,jj,1),
     +    drinfo_accum(3,jj),acft_seq_accum(1,jj),acft_seq_accum(2,jj)
        end do

        do j = 1,nlv2wrt_tot
          jj = iord(j)

          jkp = 0
          jkm = 0
          jjp1 = 0
          jjm1 = 0
          if (j .eq. nlv2wrt_tot) then
             if (ibfms(drinfo_accum(3,jj)).eq.0) then
                jjp1 = jj
                jkp = j
             end if
          else
             do jk = j+1,nlv2wrt_tot
                jjp = iord(jk)
                if (jjp > nlvinprof) cycle
                if (ibfms(drinfo_accum(3,jjp)).eq.0) then
                   jjp1 = jjp
                   jkp = jk
                   exit
                end if
             end do
          end if

          if (j .eq. 1 ) then
             if (ibfms(drinfo_accum(3,jj)).eq.0) then
                jjm1 = jj
                jkm = j
             end if
          else
             do jk = j-1,1,-1
                jjm = iord(jk)
                if (jjm > nlvinprof) cycle  ! use real obs only
                if (ibfms(drinfo_accum(3,jjm)).eq.0) then
                   jjm1 = jjm
                   jkm = jk
                   exit
                end if
             end do
          end if

          if ((jjp1 .ne. 0) .and. (jjm1 .ne. 0)) then
             dt = (drinfo_accum(3,jjp1) - drinfo_accum(3,jjm1))*3600. !  seconds 

             c1_jk = 0
             c2_jk = 0
             do while ((abs(dt)<60.) .and. ((jkp+c1_jk<=nlv2wrt_tot)
     +                 .or. (jkm-c2_jk>=1)))
                jjp2 = 0
                jjm2 = 0
                c1_jk = c1_jk+1
                c2_jk = c2_jk+1
                dt_new = dt

                do while (jkp+c1_jk<=nlv2wrt_tot
     +                .and. iord(jkp+c1_jk)>nlvinprof)
                   c1_jk = c1_jk+1   ! skip mandatory level
                end do
                if (jkp+c1_jk<=nlv2wrt_tot
     +                .and. iord(jkp+c1_jk)<=nlvinprof) then
                   jjp = iord(jkp+c1_jk)
                   if (ibfms(drinfo_accum(3,jjp)).eq.0) then
                      jjp2 = jjp
                      dt_new = (drinfo_accum(3,jjp2)
     +                       - drinfo_accum(3,jjm1))*3600.
                   end if
                end if
                if (abs(dt_new) >= 60.) then
                   if (jjp2 .ne. 0) jjp1 = jjp2
                   exit
                end if

                do while (jkm-c2_jk>=1 .and. iord(jkm-c2_jk)>nlvinprof)
                   c2_jk = c2_jk+1   ! skip mandatory level
                end do
                if (jkm-c2_jk>=1 .and. iord(jkm-c2_jk)<=nlvinprof) then
                   jjm = iord(jkm-c2_jk)
                   if (ibfms(drinfo_accum(3,jjm)).eq.0) then
                      jjm2 = jjm
                      dt_new = (drinfo_accum(3,jjp1)
     +                       - drinfo_accum(3,jjm2))*3600.
                   end if
                end if
                if (abs(dt_new) >= 60.) then
                   if (jjm2 .ne. 0) jjm1 = jjm2
                   exit
                end if

                if ((jjp2 .ne. 0) .and. (jjm2 .ne. 0)) then
                   dt_new = (drinfo_accum(3,jjp2)
     +                    - drinfo_accum(3,jjm2))*3600.
                   if (abs(dt_new) >= 60.) then
                      if (jjp2 .ne. 0) jjp1 = jjp2
                      if (jjm2 .ne. 0) jjm1 = jjm2
                      exit
                   end if
                end if
             end do
             dt = (drinfo_accum(3,jjp1) - drinfo_accum(3,jjm1))*3600.

c            write(41,*)' fj,ord1,z1,t1 = ',j,jjp1,zevn_accum(1,jjp1,1),
c    +                               drinfo_accum(3,jjp1)
c            write(41,*)' fj,ord2,z2,t2 = ',j,jjm1,zevn_accum(1,jjm1,1),
c    +                                  drinfo_accum(3,jjm1)
             zul = zevn_accum(1,jjp1,1)   ! meters
             zll = zevn_accum(1,jjm1,1) ! meters

c Need gross checks on ascent/descent rate here?
             if(abs(dt) .gt. 0.)  ! added to avoid divide by zero
     +          rate_accum(jj) = (zul - zll)/dt  ! m/s
                                            ! will be encoded into
                                            ! PREPBUFR-like file as IALR

             write(41,*) ' fj,dt,rate_accum=',j,dt,rate_accum(jj)
             write(41,*) ''
          end if
        end do
      end if ! ((nlv2wrt_tot.gt.1) .and. (.not.tsplines)) .or. err_tspline>0

c Interpolate position and time to mandatory level (will be stored in XDR YDR HRDR) (need to
c   have mandatory levels inserted into the profile before this step)
c ------------------------------------------------------------------------------------------
      if (l_mandlvl .and. nlvinprof.gt.1) then

ccccccc print *, ' nlv2wrt_tot = ',nlv2wrt_tot
        do j = 1,nlv2wrt_tot
          jj = iord(j)
ccccccc   print *, ' j,jj = ',j,jj

          nmNbtw = 0 ! reset 'number of mandatory levels in-between' counter
c------------------------------------------------------------------------------------------
c------------------------------------------------------------------------------------------
! (DAK: verified that logic below gives the correct answer - good news!)
          if(ibfms(drinfo_accum(1,jj)).ne.0 .and.
     +	     ibfms(drinfo_accum(2,jj)).ne.0 .and.
     +       ibfms(drinfo_accum(3,jj)).ne.0) then ! all obs in drift sequence missing likely
                                                  !  means this is a mandatory level for
                                                  !  which these obs must be filled via
                                                  !  interpolation
            nmNbtw = 1 ! set 'number of mandatory levels in-between' counter to 1
ccccc       print *, 'here is a first mand. level - p = ',lvlsinprof(jj)

c see if there is more than one mandatory level in a row for which we need to calculate XDR,
c  YDR and HRDR values
c ------------------------------------------------------------------------------------------
            do k = j+1, nlv2wrt_tot
ccccccc       print *, ' k = ',k
              kk = iord(k)
              if(ibfms(drinfo_accum(1,kk)).ne.0 .and.
     +	         ibfms(drinfo_accum(2,kk)).ne.0 .and.
     +           ibfms(drinfo_accum(3,kk)).ne.0) then ! another mandatory levelw/ missing
                                                      !  XDR, YDR and HRDR
                nmNbtw = nmNbtw + 1    ! increment 'number of mandatory levels in-between'
                                       !  counter by 1

ccccc           print *, 'here is ANOTHER adjacent MANDATORY LEVEL - ',
ccccc+                   'p =',lvlsinprof(kk)
ccccc           print *, 'nmNbtw = ',nmNbtw
              else
                exit ! exit k loop
              endif
            enddo

c At this point, nmNbtw is the number of mandatory levels in a row w/ missing XDR, YDR and
c  HRDR - ow we need to determine the "bread" levels; in other words, levels with real, non-
c  interpolated data, that sandwich the mandatory levels - below, jj points to the mandatory
c  level, jjm1 points to the "bread" level with actual data at the lower pressure/higher
c  altitude and jjpnmNbtw points to the "bread" level with actual data at a higher pressure/
c  lower altitude
c ------------------------------------------------------------------------------------------
            if(j.le.1) then
c DAK: Make sure j is > 1 here !!  (not sure it can ever happen)
              print *
              print *, '### PROBLEM - j .le. 1 (= ',j,') in subr. ',
     +                 'sub2mem_mer, iord array underflow'
              print *, '              this should never happen!!'
              print *
              call w3tage('PREPOBS_PREPACQC')
              call errexit(61)
            endif
            jjm1 = iord(j-1)
            jjpnmNbtw = iord(j+nmNbtw)
            pll = lvlsinprof(jjm1)
            pul = lvlsinprof(jjpnmNbtw)

c Interpolate lat/lon/time to mandatory levels
c --------------------------------------------

c Determine dtime/dlnp, total horizontal distance covered between the two points, and average
c  groundspeed of aircraft between the points
c -------------------------------------------------------------------------------------------
            dtime_dlnp = (drinfo_accum(3,jjpnmNbtw) - 
     +                    drinfo_accum(3,jjm1)) / alog(pul/pll)

c Use Haversine formula to determine distance, given two lat/lons (the same formula is used
c  in the acftobs_qc/gcirc_qc routine and more information is available at
c  http://www.movable-type.co.uk/scripts/GIS-FAQ-5.1.html)
c -----------------------------------------------------------------------------------------
            lat_pul = drinfo_accum(2,jjpnmNbtw)
            lon_pul = drinfo_accum(1,jjpnmNbtw)
            lat_pll = drinfo_accum(2,jjm1)
            lon_pll = drinfo_accum(1,jjm1)

            if(int(lon_pul*100.).eq.int(lon_pll*100.)) then
              dist_pul_pll = radius_e * abs(lat_pul-lat_pll) * deg2rad 
            elseif(int(lat_pul*100.).eq.int(lat_pll*100.)) then
              dist_pul_pll = 2.0*radius_e*
     +         asin(min(1.0,abs(cos(lat_pul*deg2rad)*
     +         sin((lon_pul-lon_pll)*0.5*deg2rad))))
            else
              dist_pul_pll = 2.0*radius_e*
     +         asin(min(1.0,sqrt(
     +                           (sin((lat_pul-lat_pll)*0.5*deg2rad))**2
     +                               +  cos(lat_pul*deg2rad)*
     +                                  cos(lat_pll*deg2rad)*
     +                           (sin((lon_pul-lon_pll)*0.5*deg2rad))**2
     +                                                                 )
     +                                                                 )
     +                                                                 )
            endif

c Check if times are equal, then interpolate lat/lon - assume aircraft is traveling at a
c  constant speed between the locations where pul and pll are observed
c --------------------------------------------------------------------------------------
            if(int(drinfo_accum(3,jjpnmNbtw)*100000.).ne.
     +	       int(drinfo_accum(3,jjm1)*100000.) .and.
     +        dist_pul_pll.ne.0.) then

              spd_pul_pll = dist_pul_pll /
     +                      abs((drinfo_accum(3,jjpnmNbtw) -
     +                           drinfo_accum(3,jjm1))*3600.)

              do k = 0,nmNbtw-1
ccccccc         print *, ' k 2 = ',k
                jjpk = iord(j+k)
                pml = lvlsinprof(jjpk)

c time
                drinfo_accum(3,jjpk) = drinfo_accum(3,jjm1) + 
     +                                 dtime_dlnp*alog(pml/pll)

                dist2pml = spd_pul_pll *
     +                   abs(drinfo_accum(3,jjpk)-drinfo_accum(3,jjm1))*
     +                     3600.         ! sec/hour... drinfo_accum(3,x) values are in hours

c latitude
                drinfo_accum(2,jjpk) = drinfo_accum(2,jjm1) +
     +                                 dist2pml/dist_pul_pll*
     +           (drinfo_accum(2,jjpnmNbtw)-drinfo_accum(2,jjm1))

c longitude
                drinfo_accum(1,jjpk) = drinfo_accum(1,jjm1) +
     +                                 dist2pml/dist_pul_pll*
     +           (drinfo_accum(1,jjpnmNbtw)-drinfo_accum(1,jjm1))

              enddo
            else ! times are equal; assume groundspeed varies linearly -- or, dist_pul_pll=0
                 !  and lat/lons of pul and pll are either equal or very very close

c Determine delx, y
c -----------------
              delx = (drinfo_accum(1,jjpnmNbtw) -
     +                drinfo_accum(1,jjm1))/(nmNbtw+1)
              dely = (drinfo_accum(2,jjpnmNbtw) -
     +                drinfo_accum(2,jjm1))/(nmNbtw+1)

c Store interpolated lat/lon/time values for the levels that need it
c ------------------------------------------------------------------
              do k = 0,nmNbtw-1
ccccccc         print *, ' k 3 = ',k
                jjpk = iord(j+k)
                pml = lvlsinprof(jjpk)
                drinfo_accum(1,jjpk) =
     +           drinfo_accum(1,jjm1) + (k+1)*delx
                drinfo_accum(2,jjpk) =
     +           drinfo_accum(2,jjm1) + (k+1)*dely
                drinfo_accum(3,jjpk) = drinfo_accum(3,jjm1) +
     +                                 dtime_dlnp*alog(pml/pll) ! if times are equal,
                                                                !  dtime_dlnp =0, and then
                                                                !  time at pml = time at pll

cc              drinfo_accum(3,jj) = 
cc   +            drinfo_accum(3,jjpnmNbtw) ! give pml the same time as pul and pll 

              enddo 
            endif ! times of "bread" levels equal?
          endif ! need to interpolate for mandatory level ?
! (DAK: verified that above below gives the correct answer - good news!)
c------------------------------------------------------------------------------------------
c------------------------------------------------------------------------------------------
        enddo ! j = 1,nlv2wrt_tot
      endif ! l_mandlvl .and. nlvinprof.gt.1

c Set TYP to reflect whether or not report is part of a profile, ascending or descending
c --------------------------------------------------------------------------------------
      jjmaxp = iord(nlv2wrt_tot)
      jjminp = iord(1)
      if(nlv2wrt_tot.eq.1) then
        hdr2wrt(6) = 300 + mod(int(hdr2wrt(6)),100)    ! TYP = 3xx for single level merged
                                                       !  (mass + wind) reports
      elseif(nlv2wrt_tot.gt.1 .and.
     +      (c_qc_accum(jjmaxp)(11:11).eq.'a' .or. 
     +	     c_qc_accum(jjmaxp)(11:11).eq.'A')) then   ! ascending profile (merged)
        hdr2wrt(6) = 400 + mod(int(hdr2wrt(6)),100)    !  TYP = 4xx for ascending profile
                                                       !   merged (mass + wind) reports

c Make sure the header information for the ascent is the coordinates, etc, present at the
c  "launch" level (highest pressure/lowest altitude)
c ---------------------------------------------------------------------------------------
        hdr2wrt(2) = drinfo_accum(1,jjmaxp)
        hdr2wrt(3) = drinfo_accum(2,jjmaxp)
        hdr2wrt(4) = drinfo_accum(3,jjmaxp)
        hdr2wrt(5) = elv_accum(1,jjmaxp)
        hdr2wrt(12) = rpt_accum(1,jjmaxp)
        hdr2wrt(13) = tcor_accum(1,jjmaxp)

      elseif(nlv2wrt_tot.gt.1 .and.
     +      (c_qc_accum(jjmaxp)(11:11).eq.'d' .or. 
     +	     c_qc_accum(jjmaxp)(11:11).eq.'D')) then   ! descending profile (merged)
        hdr2wrt(6) = 500 + mod(int(hdr2wrt(6)),100)    !  TYP = 5xx for descending profile
                                                       !   merged (mass + wind) reports

c Make sure the header information for the descent is the coordinates, etc., present at the
c  "launch" level (lowest pressure/highest altitude)
c -----------------------------------------------------------------------------------------
        hdr2wrt(2) = drinfo_accum(1,jjminp)
        hdr2wrt(3) = drinfo_accum(2,jjminp)
        hdr2wrt(4) = drinfo_accum(3,jjminp)
        hdr2wrt(5) = elv_accum(1,jjminp)
        hdr2wrt(12) = rpt_accum(1,jjminp)
        hdr2wrt(13) = tcor_accum(1,jjminp)

      endif
ccc   print *, '.. the report type here is ',hdr2wrt(6)

c Set SQN/PROCN to missing for profiles
c -------------------------------------
      hdr2wrt(10) = bmiss
      hdr2wrt(11) = bmiss

c Write header info/metadata
c --------------------------
      call ufbint(proflun,hdr2wrt,15,1,nlvwrt,
     + 'SID XOB YOB DHR ELV TYP T29 TSB ITP SQN PROCN RPT TCOR '//
     + 'RSRD EXPRSRD')

      acid_arr1 = acid1
      if(ibfms(acid1).eq.0) 
     +  call ufbint(proflun,acid_arr1,1,1,nlvwrt,'ACID')  ! store 'ACID' if present
                                                          !  {currently only in MDCRS or AMDAR
                                                          !  (LATAM only) reports}

      if(mesgtype.ne.'AIRCAR'.and. mesgtype.ne.'AIRCFT') then
        print *, 'Non-compatible message type! (',mesgtype,')'
        print *, 'Skipping this report; it will not be written to ',
     +           'output.'
        go to 9999
      endif

ccc   print *, 'FOR THIS REPORT: mxe4prof = ',mxe4prof

c -------------------------------------
c Process each event set, one at a time
c -------------------------------------
      do i = 1,mxe4prof ! maximum number of events in a single-level merged report (i.e., the
                        !  maximum amongst the number of pressure, moisture,temperature,
                        !  altitude, u/v wind and direction/speed wind events)
ccc     print *, '.. bring in next event for writing out'
ccc     print *, 'Next event is number ',i

c Clear out arrays used with ufbint to store data in memory
c ---------------------------------------------------------
        nlv2wrt   = 0

        pevns4    = bmiss
        qevns4    = bmiss
        tevns4    = bmiss
        zevns4    = bmiss
        wuvevns5  = bmiss
        wdsevns5  = bmiss

        pbgarr3   = bmiss
        qbgarr3   = bmiss
        tbgarr3   = bmiss
        zbgarr3   = bmiss
        wuvbgarr5 = bmiss

        ppparr3   = bmiss
        qpparr3   = bmiss
        tpparr3   = bmiss
        zpparr3   = bmiss
        wuvpparr6 = bmiss

        drarr3    = bmiss

        acft_seq_arr2 = bmiss

        mstq_arr1 = bmiss
        rct_arr1  = bmiss
        cat_arr1  = bmiss
        ialr_arr1 = bmiss
        turb_arr4 = bmiss

c Collapse stacks of events; keep levels where there is pressure data - do this in
c  anticipation of "striping"/layering events onto data upon output - organize data across
c  all levels for each "event set"/"layer"
c ----------------------------------------------------------------------------------------

        do j = nlv2wrt_tot,1,-1

          jj = iord(j)
ccc       print *, 'j: ',j

          nlv2wrt = nlv2wrt + 1 ! nlv2wrt = number of pressure levels to be written out
ccc       print *, 'nlv2wrt = ',nlv2wrt

          if(ibfms(pevn_accum(1,jj,i)).eq.0) then ! if POB is missing, don't process this
                                                  !  event
            pevns4(1:4,nlv2wrt) = pevn_accum(1:4,jj,i)
ccc         print *, 'POB PQM PPC PRC for this level and event:'
ccc         print *, ' pevns4(1,',nlv2wrt,') = ',pevns4(1,nlv2wrt)
ccc         print *, ' pevns4(2,',nlv2wrt,') = ',pevns4(2,nlv2wrt)
ccc         print *, ' pevns4(3,',nlv2wrt,') = ',pevns4(3,nlv2wrt)
ccc         print *, ' pevns4(4,',nlv2wrt,') = ',pevns4(4,nlv2wrt)
ccc       else
ccc         print *, 'POB missing, pevns4 is missing for this level ',
ccc  +               'and event'
          endif

          if(ibfms(qevn_accum(1,jj,i)).eq.0) then ! if QOB is missing, don't process this
                                                  !  event
            qevns4(1:4,nlv2wrt) = qevn_accum(1:4,jj,i)
ccc         print *, 'QOB QQM QPC QRC for this level and event:'
ccc         print *, ' qevns4(1,',nlv2wrt,') = ',qevns4(1,nlv2wrt)
ccc         print *, ' qevns4(2,',nlv2wrt,') = ',qevns4(2,nlv2wrt)
ccc         print *, ' qevns4(3,',nlv2wrt,') = ',qevns4(3,nlv2wrt)
ccc         print *, ' qevns4(4,',nlv2wrt,') = ',qevns4(4,nlv2wrt)
ccc       else
ccc         print *, 'QOB missing, qevns4 is missing for this ',
ccc  +               'level and event'
          endif

          if(ibfms(tevn_accum(1,jj,i)).eq.0) then ! if TOB is missing, don't process this
                                                  !  event
            tevns4(1:4,nlv2wrt) = tevn_accum(1:4,jj,i)
ccc         print *, 'TOB TQM TPC TRC for this level and event:'
ccc         print *, ' tevns4(1,',nlv2wrt,') = ',tevns4(1,nlv2wrt)
ccc         print *, ' tevns4(2,',nlv2wrt,') = ',tevns4(2,nlv2wrt)
ccc         print *, ' tevns4(3,',nlv2wrt,') = ',tevns4(3,nlv2wrt)
ccc         print *, ' tevns4(4,',nlv2wrt,') = ',tevns4(4,nlv2wrt)
ccc       else
ccc         print *, 'TOB missing, tevns4 is missing for this ',
ccc  +               'level and event'
          endif

          if(ibfms(zevn_accum(1,jj,i)).eq.0) then ! if ZOB is missing, don't process this
                                                  !  event
            zevns4(1:4,nlv2wrt) = zevn_accum(1:4,jj,i)
ccc         print *, 'ZOB ZQM ZPC ZRC for this level and event:'
ccc         print *, ' zevns4(1,',nlv2wrt,') = ',zevns4(1,nlv2wrt)
ccc         print *, ' zevns4(2,',nlv2wrt,') = ',zevns4(2,nlv2wrt)
ccc         print *, ' zevns4(3,',nlv2wrt,') = ',zevns4(3,nlv2wrt)
ccc         print *, ' zevns4(4,',nlv2wrt,') = ',zevns4(4,nlv2wrt)
ccc       else
ccc         print *, 'ZOB missing, zevns4 is missing for this level ',
ccc  +               'and event'
          endif

          if(ibfms(wuvevn_accum(1,jj,i)).eq.0 .and.  ! if UOB or VOB are missing, don't
     +       ibfms(wuvevn_accum(2,jj,i)).eq.0) then  !  process this event
            wuvevns5(1:5,nlv2wrt) = wuvevn_accum(1:5,jj,i)
ccc         print *, 'UOB VOB WQM WPC WRC for this level and event:'
ccc         print *, ' wuvevns5(1,',nlv2wrt,') = ',wuvevns5(1,nlv2wrt)
ccc         print *, ' wuvevns5(2,',nlv2wrt,') = ',wuvevns5(2,nlv2wrt)
ccc         print *, ' wuvevns5(3,',nlv2wrt,') = ',wuvevns5(3,nlv2wrt)
ccc         print *, ' wuvevns5(4,',nlv2wrt,') = ',wuvevns5(4,nlv2wrt)
ccc         print *, ' wuvevns5(5,',nlv2wrt,') = ',wuvevns5(5,nlv2wrt)
ccc       else
ccc         print *, 'either UOB or VOB missing, wuvevns5 is missing ',
ccc  +               'for this level and event'
          endif

          wdsevns5(1:5,nlv2wrt) = wdsevn_accum(1:5,jj,i)
ccc       print *, 'DDO FFO DFQ DFP DFR for this level and event:'
ccc       print *, ' wdsevns5(1,',nlv2wrt,') = ',wdsevns5(1,nlv2wrt)
ccc       print *, ' wdsevns5(2,',nlv2wrt,') = ',wdsevns5(2,nlv2wrt)
ccc       print *, ' wdsevns5(3,',nlv2wrt,') = ',wdsevns5(3,nlv2wrt)
ccc       print *, ' wdsevns5(4,',nlv2wrt,') = ',wdsevns5(4,nlv2wrt)
ccc       print *, ' wdsevns5(5,',nlv2wrt,') = ',wdsevns5(5,nlv2wrt)

c Collapse arrays of background, post-processing, drift, acft_seq info - need to accumulate
c  background, etc., across all levels - only write out these values upon writing first
c  "event"/"layer".  These values occur only once per layer, there is no nested replication
c -----------------------------------------------------------------------------------------

          if(i.eq.1) then

            pbgarr3(1:3,nlv2wrt) = pbg_accum(1:3,jj)
ccc         print *, 'POE PFC PFCMOD for this level - NO event:'
ccc         print *, ' pbgarr3(1,',nlv2wrt,') = ',pbgarr3(1,nlv2wrt)
ccc         print *, ' pbgarr3(2,',nlv2wrt,') = ',pbgarr3(2,nlv2wrt)
ccc         print *, ' pbgarr3(3,',nlv2wrt,') = ',pbgarr3(3,nlv2wrt)
            qbgarr3(1:3,nlv2wrt) = qbg_accum(1:3,jj)
ccc         print *, 'QOE QFC QFCMOD for this level - NO event:'
ccc         print *, ' qbgarr3(1,',nlv2wrt,') = ',qbgarr3(1,nlv2wrt)
ccc         print *, ' qbgarr3(2,',nlv2wrt,') = ',qbgarr3(2,nlv2wrt)
ccc         print *, ' qbgarr3(3,',nlv2wrt,') = ',qbgarr3(3,nlv2wrt)
            tbgarr3(1:3,nlv2wrt) = tbg_accum(1:3,jj)
ccc         print *, 'TOE TFC TFCMOD for this level - NO event:'
ccc         print *, ' tbgarr3(1,',nlv2wrt,') = ',tbgarr3(1,nlv2wrt)
ccc         print *, ' tbgarr3(2,',nlv2wrt,') = ',tbgarr3(2,nlv2wrt)
ccc         print *, ' tbgarr3(3,',nlv2wrt,') = ',tbgarr3(3,nlv2wrt)
            zbgarr3(1:3,nlv2wrt) = zbg_accum(1:3,jj)
ccc         print *, 'ZOE ZFC ZFCMOD for this level - NO event:'
ccc         print *, ' zbgarr3(1,',nlv2wrt,') = ',zbgarr3(1,nlv2wrt)
ccc         print *, ' zbgarr3(2,',nlv2wrt,') = ',zbgarr3(2,nlv2wrt)
ccc         print *, ' zbgarr3(3,',nlv2wrt,') = ',zbgarr3(3,nlv2wrt)
            wuvbgarr5(1:5,nlv2wrt) = wuvbg_accum(1:5,jj)
ccc         print *, 'WOE UFC VFC UFCMOD VFCMOD for this level - NO event:'
ccc         print *, ' wuvbgarr5(1,',nlv2wrt,') = ',wuvbgarr5(1,nlv2wrt)
ccc         print *, ' wuvbgarr5(2,',nlv2wrt,') = ',wuvbgarr5(2,nlv2wrt)
ccc         print *, ' wuvbgarr5(3,',nlv2wrt,') = ',wuvbgarr5(3,nlv2wrt)
ccc         print *, ' wuvbgarr5(4,',nlv2wrt,') = ',wuvbgarr5(4,nlv2wrt)
ccc         print *, ' wuvbgarr5(5,',nlv2wrt,') = ',wuvbgarr5(5,nlv2wrt)

            ppparr3(1:3,nlv2wrt) = ppp_accum(1:3,jj)
ccc         print *, 'PAN PCL PCS for this level - NO event:'
ccc         print *, ' ppparr3(1,',nlv2wrt,') = ',ppparr3(1,nlv2wrt)
ccc         print *, ' ppparr3(2,',nlv2wrt,') = ',ppparr3(2,nlv2wrt)
ccc         print *, ' ppparr3(3,',nlv2wrt,') = ',ppparr3(3,nlv2wrt)
            qpparr3(1:3,nlv2wrt) = qpp_accum(1:3,jj)
ccc         print *, 'QAN QCL QCS for this level - NO event:'
ccc         print *, ' qpparr3(1,',nlv2wrt,') = ',qpparr3(1,nlv2wrt)
ccc         print *, ' qpparr3(2,',nlv2wrt,') = ',qpparr3(2,nlv2wrt)
ccc         print *, ' qpparr3(3,',nlv2wrt,') = ',qpparr3(3,nlv2wrt)
            tpparr3(1:3,nlv2wrt) = tpp_accum(1:3,jj)
ccc         print *, 'TAN TCL TCS for this level - NO event:'
ccc         print *, ' tpparr3(1,',nlv2wrt,') = ',tpparr3(1,nlv2wrt)
ccc         print *, ' tpparr3(2,',nlv2wrt,') = ',tpparr3(2,nlv2wrt)
ccc         print *, ' tpparr3(3,',nlv2wrt,') = ',tpparr3(3,nlv2wrt)
            zpparr3(1:3,nlv2wrt) = zpp_accum(1:3,jj)
ccc         print *, 'ZAN ZCL ZCS for this level - NO event:'
ccc         print *, ' zpparr3(1,',nlv2wrt,') = ',zpparr3(1,nlv2wrt)
ccc         print *, ' zpparr3(2,',nlv2wrt,') = ',zpparr3(2,nlv2wrt)
ccc         print *, ' zpparr3(3,',nlv2wrt,') = ',zpparr3(3,nlv2wrt)
            wuvpparr6(1:6,nlv2wrt) = wuvpp_accum(1:6,jj)
ccc         print *, 'UAN VAN UCL UCS VCL VCS for this level - NO event:'
ccc         print *, ' wuvpparr6(1,',nlv2wrt,') = ',wuvpparr6(1,nlv2wrt)
ccc         print *, ' wuvpparr6(2,',nlv2wrt,') = ',wuvpparr6(2,nlv2wrt)
ccc         print *, ' wuvpparr6(3,',nlv2wrt,') = ',wuvpparr6(3,nlv2wrt)
ccc         print *, ' wuvpparr6(4,',nlv2wrt,') = ',wuvpparr6(4,nlv2wrt)
ccc         print *, ' wuvpparr6(5,',nlv2wrt,') = ',wuvpparr6(5,nlv2wrt)
ccc         print *, ' wuvpparr6(6,',nlv2wrt,') = ',wuvpparr6(6,nlv2wrt)

            drarr3(1:3,nlv2wrt) = drinfo_accum(1:3,jj)
ccc         print *, 'XDR YDR HRDR for this level - NO event:'
ccc         print *, ' drarr3(1,',nlv2wrt,') = ',drarr3(1,nlv2wrt)
ccc         print *, ' drarr3(2,',nlv2wrt,') = ',drarr3(2,nlv2wrt)
ccc         print *, ' drarr3(3,',nlv2wrt,') = ',drarr3(3,nlv2wrt)

            acft_seq_arr2(1:2,nlv2wrt) = acft_seq_accum(1:2,jj)
ccc         print *, 'PCAT POAF for this level - NO event:'
ccc         print *, ' acft_seq_arr2(1,',nlv2wrt,') = ',
ccc  +                 acft_seq_arr2(1,nlv2wrt)
ccc         print *, ' acft_seq_arr2(2,',nlv2wrt,') = ',
ccc  +                 acft_seq_arr2(2,nlv2wrt)

            mstq_arr1(1,nlv2wrt) = mstq_accum(1,jj)
ccc         print *, 'MSTQ for this level - NO event:'
ccc         print *, ' mstq_arr1(1,',nlv2wrt,') = ',mstq_arr1(1,nlv2wrt)

            rct_arr1(1,nlv2wrt) = rct_accum(1,jj)
ccc         print *, 'RCT for this level - NO event:'
ccc         print *, ' rct_arr1(1,',nlv2wrt,') = ',rct_arr1(1,nlv2wrt

            cat_arr1(1,nlv2wrt) = cat_accum(1,jj)
ccc         print *, 'CAT for this level - NO event:'
ccc         print *, ' cat_arr1(1,',nlv2wrt,') = ',cat_arr1(1,nlv2wrt)

            ialr_arr1(1,nlv2wrt) = rate_accum(jj)
ccc         print *, 'IALR for this level - NO event:'
ccc         print *, ' ialr_arr1(1,',nlv2wrt,') = ',ialr_arr1(1,nlv2wrt)
          endif

          if(.not.l_operational) then  ! this is currently invoked because l_operational
                                       !  is hardwired to F for l_ncep=T
            if(i.eq.mxe4prof) then
              hdr2wrt1 = hdr2wrt(1)
              if(ibfms(drinfo_accum(3,jj)).ne.0) then
                idrinfo_accum3 = 9999999
              else
                idrinfo_accum3 = nint(drinfo_accum(3,jj) * 3600.)
              endif
              if(ibfms(hdr2wrt(9)).ne.0) then
                ihdr2wrt9 = 99999
              else
                ihdr2wrt9 = nint(hdr2wrt(9))
              endif
              if(ibfms(hdr2wrt(6)).ne.0) then
                ihdr2wrt6 = 9999
              else
                ihdr2wrt6 = nint(hdr2wrt(6))
              endif
              if(ibfms(acft_seq_accum(2,jj)).ne.0) then
                iacft_seq_accum2 = 99
              else
                iacft_seq_accum2 = nint(acft_seq_accum(2,jj))
              endif
              if(ibfms(mstq_accum(1,jj)).ne.0) then
                imstq_accum1 = 9999
              else
                imstq_accum1 = nint(mstq_accum(1,jj))
              endif
              if(ibfms(cat_accum(1,jj)).ne.0) then
                icat_accum1 = 9999
              else
                icat_accum1 = nint(cat_accum(1,jj))
              endif
              do iii = mxe4prof,1,-1
                if(ibfms(tevn_accum(1,jj,iii)).ne.0) then
                  nevents_t = iii
                else
                  nevents_t = iii
                  exit
                endif
              enddo
              if(ibfms(zevn_accum(1,jj,1)).ne.0) then
                izevn_accum1 = 999999
              else
                izevn_accum1 =  nint(zevn_accum(1,jj,1))
              endif
              if(ibfms(wdsevn_accum(1,jj,1)).ne.0) then
                iwdsevn_accum1 = 99999
              else
                iwdsevn_accum1 =  nint(wdsevn_accum(1,jj,1))
              endif
              do iii = mxe4prof,1,-1
                if(ibfms(wuvevn_accum(1,jj,iii)).ne.0 .or.
     +             ibfms(wuvevn_accum(2,jj,iii)).ne.0) then
                  if(iii.eq.1) wspd = bmiss
                  nevents_w = iii
                else
                  wspd = sqrt(wuvevn_accum(1,jj,iii)**2 + 
     +                        wuvevn_accum(2,jj,iii)**2)
                  nevents_w = iii
                  exit
                endif
              enddo
              do iii = mxe4prof,1,-1
                if(ibfms(qevn_accum(1,jj,iii)).ne.0) then
                  if(iii.eq.1) q_sphum = bmiss
                  nevents_q = iii
                else
                  q_sphum = qevn_accum(1,jj,iii) * 0.001
                  nevents_q = iii
                  exit
                endif
              enddo
              if(ibfms(pevn_accum(2,jj,1)).ne.0) then
                ipevn_accum2 = 999
              else
                ipevn_accum2 = nint(pevn_accum(2,jj,1))
              endif
              if(ibfms(zevn_accum(2,jj,1)).ne.0) then
                izevn_accum2 = 999
              else
                izevn_accum2 = nint(zevn_accum(2,jj,1))
              endif
              if(ibfms(tevn_accum(2,jj,nevents_t)).ne.0) then
                itevn_accum2 = 999
              else
                itevn_accum2 = nint(tevn_accum(2,jj,nevents_t))
              endif
              if(ibfms(qevn_accum(2,jj,nevents_q)).ne.0) then
                iqevn_accum2 = 999
              else
                iqevn_accum2 = nint(qevn_accum(2,jj,nevents_q))
              endif
              if(ibfms(wuvevn_accum(3,jj,nevents_w)).ne.0) then
                iwuvevn_accum3 = 999
              else
                iwuvevn_accum3 = nint(wuvevn_accum(3,jj,nevents_w))
              endif
              if(ibfms(pevn_accum(4,jj,1)).ne.0 .or.
     +           nint(pevn_accum(3,jj,1)).ne.nrlacqc_pc) then
                ipevn_accum4 = 9999
              else
                ipevn_accum4 = nint(pevn_accum(4,jj,1))
              endif
              if(ibfms(zevn_accum(4,jj,1)).ne.0 .or.
     +           nint(zevn_accum(3,jj,1)).ne.nrlacqc_pc) then
                izevn_accum4 = 9999
              else
                izevn_accum4 = nint(zevn_accum(4,jj,1))
              endif
              if(ibfms(tevn_accum(4,jj,nevents_t)).ne.0 .or.
     +           nint(tevn_accum(3,jj,nevents_t)).ne.nrlacqc_pc) then
                itevn_accum4 = 9999
              else
                itevn_accum4 = nint(tevn_accum(4,jj,nevents_t))
              endif
              if(ibfms(qevn_accum(4,jj,nevents_q)).ne.0 .or.
     +           nint(qevn_accum(3,jj,nevents_q)).ne.nrlacqc_pc) then
                iqevn_accum4 = 9999
              else
                iqevn_accum4 = nint(qevn_accum(4,jj,nevents_q))
              endif
              if(ibfms(wuvevn_accum(5,jj,nevents_w)).ne.0 .or.
     +           nint(wuvevn_accum(4,jj,nevents_w)).ne.nrlacqc_pc) then
                iwuvevn_accum5 = 9999
              else
                iwuvevn_accum5 = nint(wuvevn_accum(5,jj,nevents_w))
              endif

ccccc         write(52,fmt=7999) i
c7999         format('EVENT # ',i5)
              write(52,fmt=8001) j,c_acftid1,c_acftreg1,ihdr2wrt9,
     +         iacft_seq_accum2,drinfo_accum(2,jj),
     +         drinfo_accum(1,jj),idrinfo_accum3,izevn_accum1,
     +         pevn_accum(1,jj,1),tevn_accum(1,jj,nevents_t)+273.16,
     +         nevents_t,q_sphum,nevents_q,wuvevn_accum(1,jj,nevents_w),
     +         wuvevn_accum(2,jj,nevents_w),nevents_w,
     +         acft_seq_accum(1,jj),c_qc_accum(jj),rct_accum(1,jj),
     +         imstq_accum1,icat_accum1,wspd,iwdsevn_accum1,ihdr2wrt6,
     +         ipevn_accum2,izevn_accum2,itevn_accum2,iqevn_accum2,
     +         iwuvevn_accum3,ipevn_accum4,izevn_accum4,itevn_accum4,
     +         iqevn_accum4,iwuvevn_accum5

 8001 format(i5,1x,a9,1x,a8,1x,i3,2x,i1,1x,2f10.5, 1x,i6,1x,i5,1x,f6.1,
     +       1x,f6.2,i3,1x,f7.2,1x,i3,1x,f6.1,1x,f6.1,1x,i3,1x,f6.2,1x,
     +       '!',a11,'!',f5.2,1x,i3,2x,i2,1x,f6.1,1x,i4,2x,i3,9x,'!',
     +       5(1x,i2.2),'!',i3.3,4(1x,i3.3))

            endif ! i.eq.mxe4prof
          endif ! .not.l_operational
	enddo ! do j = nlv2wrt_tot,1,-1
ccc     print *, '.. will write out ',nlv2wrt,' p-levels for this ',
ccc  +           'report'

c Store pressure events across levels, z events, t, q, w, df events
c -----------------------------------------------------------------
        if(nlv2wrt.gt.0 .and. nlv2wrt.eq.nlv2wrt_tot) then ! should be equal; vertical coord.
                                                           !  is pressure
          call ufbint(proflun,pevns4,4,nlv2wrt,nlvwrt,'POB PQM PPC PRC')
ccc       print *, 'ufbint has stored POB PQM PPC PRC on all levels ',
ccc  +             'for this event:'

          call ufbint(proflun,qevns4,4,nlv2wrt,nlvwrt,'QOB QQM QPC QRC')
ccc       print *, 'ufbint has stored QOB QQM QPC QRC on all levels ',
ccc  +             'for this event:'

          call ufbint(proflun,tevns4,4,nlv2wrt,nlvwrt,'TOB TQM TPC TRC')
ccc       print *, 'ufbint has stored TOB TQM TPC TRC on all levels ',
ccc  +             'for this event:'

          call ufbint(proflun,zevns4,4,nlv2wrt,nlvwrt,'ZOB ZQM ZPC ZRC')
ccc       print *, 'ufbint has stored ZOB ZQM ZPC ZRC on all levels ',
ccc  +             'for this event:'

          call ufbint(proflun,wuvevns5,5,nlv2wrt,nlvwrt,
     +                'UOB VOB WQM WPC WRC')
ccc       print *, 'ufbint has stored UOB VOB WQM WPC WRC on all ',
ccc  +             'levels for this event:'

          call ufbint(proflun,wdsevns5,5,nlv2wrt,nlvwrt,
     +                'DDO FFO DFQ DFP DFR')
ccc       print *, 'ufbint has stored DDO FFO DFQ DFP DFR on all ',
ccc  +             'levels for this event:'

ccc       print *, 'Finished writing p,q,t,u/v,s/d on all ',nlv2wrt,
ccc  +             ' levels for THIS event'
          num_events_prof = num_events_prof + nlv2wrt
ccc       print *, 'Finished writing p,q,t,u/v,s/d on all ',nlv2wrt,
ccc  +             ' levels for THIS event'
ccc       print *, 'num_events_prof = ',num_events_prof

          if(i.eq.1) then ! store/write these only on first event application the following
                          !  values only occur once in the subset; there are no multiple
                          !  events to write out

c -------------------------------------------------------------------------------------------
c Store background and post processing info - each pressure level in the profile gets one set
c  of each (not nested replication like with the events)
c -------------------------------------------------------------------------------------------

ccc         print'(" write background and post-processing info - only ",
ccc  +             "for first ""event"" since there are no events for ",
ccc  +             "these")'

c write background info
            call ufbint(proflun,pbgarr3,3,nlv2wrt,nlvwrt,
     +                  'POE PFC PFCMOD')	
ccc         print *, 'ufbint has stored POE PFC PFCMOD on all levels -',
ccc  +               ' "event" ',i,' ONLY'

            call ufbint(proflun,qbgarr3,3,nlv2wrt,nlvwrt,
     +                  'QOE QFC QFCMOD')	
ccc         print *, 'ufbint has stored QOE QFC QFCMOD on all levels -',
ccc  +               ' "event" ',i,' ONLY'

            call ufbint(proflun,tbgarr3,3,nlv2wrt,nlvwrt,
     +                  'TOE TFC TFCMOD')	
ccc         print *, 'ufbint has stored TOE TFC TFCMOD on all levels -',
ccc  +               ' "event" ',i,' ONLY'

            call ufbint(proflun,zbgarr3,3,nlv2wrt,nlvwrt,
     +                  'ZOE ZFC ZFCMOD')
ccc         print *, 'ufbint has stored ZOE ZFC ZFCMOD on all levels -',
ccc  +               ' "event" ',i,' ONLY'

            call ufbint(proflun,wuvbgarr5,5,nlv2wrt,nlvwrt,
     +                  'WOE UFC VFC UFCMOD VFCMOD')	
ccc         print *, 'ufbint has stored WOE UFC VFC UFCMOD VFCMOD on',
ccc  +               ' all levels - "event" ',i,' ONLY'

c write post-processing info
            call ufbint(proflun,ppparr3,3,nlv2wrt,nlvwrt,'PAN PCL PCS')	
ccc         print *, 'ufbint has stored PAN PCL PCS on all levels - ',
ccc  +               '"event" ',i,' ONLY'

            call ufbint(proflun,qpparr3,3,nlv2wrt,nlvwrt,'QAN QCL QCS')	
ccc         print *, 'ufbint has stored QAN QCL QCS on all levels - ',
ccc  +               '"event" ',i,' ONLY'

            call ufbint(proflun,tpparr3,3,nlv2wrt,nlvwrt,'TAN TCL TCS')	
ccc         print *, 'ufbint has stored TAN TCL TCS on all levels - ',
ccc  +               '"event" ',i,' ONLY'

            call ufbint(proflun,zpparr3,3,nlv2wrt,nlvwrt,'ZAN ZCL ZCS')	
ccc         print *, 'ufbint has stored ZAN ZCL ZCS on all levels - ',
ccc  +               '"event" ',i,' ONLY'

            call ufbint(proflun,wuvpparr6,6,nlv2wrt,nlvwrt,
     +                  'UAN VAN UCL UCS VCL VCS')	
ccc         print *, 'ufbint has stored UAN VAN UCL UCS VCL VCS on all',
ccc  +               ' levels - "event" ',i,' ONLY'

c write out drift info
            call ufbint(proflun,drarr3,3,nlv2wrt,nlvwrt,'XDR YDR HRDR')
ccc         print *, 'ufbint has stored XDR YDR HRDR on all levels - ',
ccc  +               '"event" ',i,' ONLY'

c write out acft_seq info
            call ufbint(proflun,acft_seq_arr2,2,nlv2wrt,nlvwrt,
     +                  'PCAT POAF')
ccc         print *, 'ufbint has stored PCAT POAF on all levels - ',
ccc  +               '"event" ',i,' ONLY'

c There is no turbulence info carried forth into this subroutine right now, comment out
ccccc       call ufbint(proflun,turb_arr4,4,nlv2wrt,nlvwrt,
ccccc+                  'TRBX10 TRBX21 TRBX32 TRBX43')
ccc         print *, 'ufbint has stored TRBX10 TRBX21 TRBX32 TRBX43 on',
ccc  +               ' all levels - "event" ',i,' ONLY'

c write out moisture QC flag
            call ufbint(proflun,mstq_arr1,1,nlv2wrt,nlvwrt,'MSTQ')
ccc         print *, 'ufbint has stored MSTQ on all levels - "event"',
ccc  +               ' ',i,' ONLY'

c write out level receipt time
            call ufbint(proflun,rct_arr1,1,nlv2wrt,nlvwrt,'RCT')
ccc         print *, 'ufbint has stored RCT on all levels - "event" ',i,
ccc  +               ' ONLY'

c write out level category
            call ufbint(proflun,cat_arr1,1,nlv2wrt,nlvwrt,'CAT')
ccc         print *, 'ufbint has stored CAT on all levels - "event" ',i,
ccc  +               ' ONLY'

c write out the ascent/descent rate
            call ufbint(proflun,ialr_arr1,1,nlv2wrt,nlvwrt,'IALR')
ccc         print *, 'ufbint has stored IALR on all levels - "event" ',
ccc  +               i,' ONLY'

          endif ! i.eq.1/1st event? - only write background/pp info once

        else
C.......................................................................
C For some reason the total number of levels written out (nlv2wrt_tot) does not equal the
c  number of pressure levels written out (nlv2wrt) for this profile report - problems!!!
c  (go on to next profile)
c----------------------------------------------------------------------------------------
          print 54, nlv2wrt_tot,nlv2wrt
   54 format(/' #####> WARNING: THE TOTAL # OF LEVELS WRITTEN OUT ',I6,
     + ' .NE. THE # OF PRESSURE LEVELS WRITTEN OUT ',I6,' FOR THIS ',
     + 'PROFILE -- GO ON TO NEXT PROFILE'/)
          write(cnlv2wrt_tot,'(i3)') nlv2wrt_tot
          write(cnlv2wrt,'(i3)') nlv2wrt
          call system('[ -n "$jlogfile" ] && $DATA/postmsg '//
     +     '"$jlogfile" "***WARNING: LEVEL MISMATCH FOR PREPACQC '//
     +     'PROFILE: TOTAL WRITTEN '//cnlv2wrt_tot//' .ne. # PRESS '//
     +     'LVLS WRITTEN '//cnlv2wrt//' - PROFILE SKIPPED"')
          go to 9999
C.......................................................................
        endif

      enddo ! i = 1,mxnmev

      if(.not.l_operational) then
        write(52,fmt=8002)
 8002 format(208('X'))
      endif

 9999 continue

ccc   print *, 'out of sub2mem_mer for this merged report'

      return

      end

