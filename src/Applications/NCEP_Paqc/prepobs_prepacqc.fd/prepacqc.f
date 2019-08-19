c$$$ Main Program Documentation Block
c   BEST VIEWED WITH 94-CHARACTER WIDTH WINDOW
c
c Main Program: PREPOBS_PREPACQC
c   Programmer: D. Keyser       Org: NP22       Date: 2016-12-09
c
c Abstract: Performs the NRL aircraft data quality control on all types of reports (AIREP,
c   PIREP, AMDAR, TAMDAR, MDCRS).  Replaces the previous routine of the same name originally
c   written by Paul Julian (which was less comprehensive and did not handle MDCRS reports).
c   It reads in a PREPBUFR file containing all reports, pulls out "AIRCAR" and "AIRCFT"
c   reports, merges the mass and wind pieces, translates information into NRL "standards" and
c   stores in internal memory.  These are then passed into the NRL quality control kernel
c   (acftqc_obs).  Once the NRL quality control is completed, translates information back to
c   NCEP/PREPBUFR "standards" and encodes the updated information into the full PREPBUFR file
c   as "events" with new NRLACQC reason codes.  The events consist of quality mark changes,
c   although NRLACQC can also remove duplicate reports and rehabilitate (update) the report
c   time, latitude and longitude for some AIREP reports.  An option is to also generate a
c   PREPBUFR-like profiles file containing only aircraft reports in "raob-lookalike"
c   profiles (merged mass and wind data).  These can be used for air quality and verification
c   codes.
c
c Program History Log:
c 2010-11-15  S. Bender  -- Original Author
c 2012-05-08  D. Keyser  -- Prepared for operational implementation
c 2012-11-20  J. Woollen -- Initial port to WCOSS
c 2012-12-11  S. Hsiao   -- Increased maximum number of merged reports that can be processed
c                           "max_reps" from 150K to 155K to handle increase in MDCRS reports
c 2013-02-07  D. Keyser  -- Interface with input PREPBUFR file will now store pressure and
c                           pressure-altitude only from the first (mass) piece of a mass/wind
c                           piece pair rather than re-store it again from the second (wind)
c                           piece - even though they "should" be the same in both pieces (see
c                           % below for exception), there can be rare cases when at least
c                           pressure-altitude is missing in the wind piece (due to a bug in
c                           PREPDATA where unreasonably-high winds are set to missing and an
c                           "empty" wind piece is still encoded into PREPBUFR, this can lead
c                           to floating point exception errors in construction of profiles
c                           {note that pressure & pressure-altitude from reports with only a
c                           wind piece will be read since it is the first (only) piece of the
c                           report}: % - there can be cases where the pressure qualty mark
c                           (PQM) is different in the mass piece vs. the wind piece (e.g.,
c                           when it is set to 10 for reports near tropical systems by
c                           SYNDATA), so it is better to pick up PQM from the mass report for
c                           use in the merged mass/wind profiles, an added benefit of this
c                           change; increased maximum number of merged reports that can be
c                           processed "max_reps" from 155K to 185K to handle future increase
c                           all types of aircraft rpts; if the total number of merged (mass
c                           + wind piece) aircraft-type reports read in from PREPBUFR file is
c                           at least 90% of the maximum allowed, print diagnostic warning
c                           message to production joblog file prior to returning from
c                           subroutine INPUT_ACQC; if the maximum number of merged reports
c                           that can be processed ("max_reps") is exceeded when updating
c                           reports in PREPBUFR file with QC changes in subroutine
c                           OUTPUT_ACQC_NOPROF, program will no longer stop with r.c. 31, as
c                           though there is an indexing error, instead all original reports
c                           above "max_reps" will be written out without any QC and a message
c                           will be printed to stdout (a diagnostic will have already been
c                           sent to the production joblog file in this case when reports were
c                           first read in by subroutine INPUT_ACQC)
c 2013-02-07  D. Keyser  -- Final changes to run on WCOSS: Set BUFRLIB missing (BMISS) to
c                           10E8 rather than 10E10 to avoid integer overflow; use formatted
c                           print statements where previously unformatted print was > 80
c                           characters
c 2014-03-06  D. Keyser  -- Moved BUFRLIB routine OPENMB call in subroutine
c                           output_acqc_noprof to after time window and geographic domain
c                           checks to prevent creation of an empty, but open, BUFR message
c                           (type AIRCAR) in (rare) cases where absolutely no aircraft
c                           reports pass these checks (would cause a BUFRLIB abort due to
c                           previous message being open when attempting to copy first non-
c                           aircraft message from input to output PREPBUFR file
c 2014-07-18  D. Keyser  --
c                 - Increased maximum number of flights that can be processed "maxflt" from
c                   5000 to 7500 to account for increase in aircraft reports.
c                 - Increased maximum number of merged reports that can be processed
c                   "max_reps" from 185K to 220K to handle future increase in all types of
c                   aircraft reports.
c                 - If subroutine acftobs_qc returns abnormally to main program due to the
c                   maximum value for number of flights calculated at some point during its
c                   processing exceeding the allowed limit ("maxflt"), no longer stop with
c                   r.c. 98.  Instead continue on with processing and post a diagnostic
c                   warning message to the production joblog file.  The assumption is that
c                   the resultant PREPBUFR file may not contain fully QC'd aircraft data,
c                   especially if the actual number of flights calculated greatly exceeds
c                   "maxflt" (since obs in flights above the "maxflt" limit may partially be
c                   skipped over in the QC process), but the vast majority should be QC'd,
c                   and all reports originally in the PREPBUFR file will be at least be
c                   retained. (Note that a gradual increase will trigger a warning in the
c                   production joblog now when numbers get too close to the limit - see
c                   change to subroutine acftobs_qc below).
c                 - Increased format width from I5 to I6 in all places where aircraft obs
c                   index is listed out (since there now can be > 99999 reports).
c                 - Subroutine acftobs_qc and its child subroutines:
c                    - Keep track of maximum value for number of flights calculated at some
c                      point during the processing of subroutine acftobs_qc.  If, at the end
c                      of acftobs_qc, this value is at least 90% of the allowed limit
c                      ("maxflt", set in the main program), post a diagnostic warning message
c                      to the production joblog file prior to exiting from acftobs_qc.
c                    - In subr. do_flt and do_reg, return (abnormally) immediately if
c                      "maxflt" is exceeded rather than waiting to test for this at end of
c                      do_flt and do_reg and then return (abnormally).  Prior to return
c                      subtract 1 from number of flights so it will remain at "maxflt". The
c                      immediate return avoids clobbering of memory in these cases.
c                    - In subr. reorder, where any new flight exceeding "maxflt" replaces the
c                      previous flight at index "maxflt" in the arrays to avoid an array
c                      overflow (done in two places in original NRL version), post diagnostic
c                      warning message to the production joblog file (found a third instance
c                      where this needs to be done in subr. reorder - original NRL version
c                      did not trap it and arrays limited to length "maxflt" would have
c                      overflowed).
c                    - If "maxflt" is exceeded in subr. dupchk (1 place possible) or in subr.
c                      do_flt (2 places possible), the abnormal return back to subr.
c                      acftobs_qc results in subr. acftobs_qc now continuing on but setting a
c                      flag for "maxflt_exceeded".  Prior to this, subr.  acftobs_qc itself
c                      immediately performed an abnormal return back to main program in such
c                      cases resulting in no more NRL QC processing.  Now NRL QC processing
c                      will continue on to the end of subr. acftobs_qc where the abnormal
c                      return back to the main program will be triggered by the
c                      "maxflt_exceeded" flag.
c                    - There is one, apparently rare, condition where "maxflt" could be
c                      exceeded in subr. acft_obs itself (within logic which generates master
c                      list of tail numbers and counts).  Since it can't be determined if
c                      continuing on without processing (QC'ing) any more data would yield
c                      acceptable results, the program now immediately stops with condition
c                      code 98 and a diagnostic warning message is posted to the production
c                      joblog file noting that "maxflt" needs to be increased.  Prior to this
c                      it returned to the main program where it also immediately stopped with
c                      condition code 98 (so no real change in what happens here, just where
c                      it happens).
c 2014-09-03  D. Keyser  -- If no aircraft reports of any type are read from input PREPBUFR
c                      file by subr. input_acqc, no further processing is performed in this
c                      subr. other than the usual stdout print summary at its end.  After its
c                      return back to the calling main program, the main program also, in
c                      this case, does no further processing.  Instead the main program stops
c                      with condition code 4 (to alert executing script prepobs_prepacqc.sh)
c                      after printing a diagnostic message to stdout.
c 2014-12-09  J. Purser/Y. Zhu     -- Added new namelist switches "l_mandlvl" and "tsplines",
c                      used by subroutine sub2mem_mer to modify the calculation of vertical
c                      velocity rate in the profiles {l_mandlvl=F excludes interpolation to
c                      mandatory levels; tsplines=T calculates vertical velocity rate using
c                      Jim Purser's tension-spline interpolation utility (source in-lined in
c                      this program at this time) to get continuous gradient results in a
c                      profile and mitigate missing time information; tsplines=F uses finite-
c                      difference method to obtain vertical velocity rate, calculated for
c                      both ascents and descents using the nearest neighboring pair which are
c                      at least one minute apart (before, only finite-difference method was
c                      used to obtain vertical velocity rate and it could only be calculated
c                      for descents).
c 2014-12-12  D. Keyser  -- Printout from vertical velocity rate calculation information for
c                      QC'd merged aircraft reports written to profiles PREPBUFR-like file is
c                      written to unit 41 rather than stdout.
c 2015-03-16  D. Keyser  --
c                 - Increased maximum number of merged reports that can be processed
c                   "max_reps" from 220K to 300K to handle future increase in all types of
c                   aircraft reports.
c                 - In subr. output_acqc_prof, fixed a bug which, for cases where the maximum
c                   number of merged reports that can be processed ("max_reps") is exceeded,
c                   prevented any original reports above "max_reps" from being written out
c                   (without any QC).
c 2015-04-17  J. Purser   -- Updates to tension-spline interpolation utility pspl:
c                   In April 2015 some significant changes were made to pspl.f90 to improve
c                   the robustness of the algorithm and the usefulness of the energy
c                   diagnostic:
c                       1) The allowance of B iterations was increased from 40 to 80 owing to
c                          a single failure in a parallel run (where 43 iterations were
c                          required) (and the halfgate parameter was increased to 30 for all
c                          data in the parallels, which also increases robustness).
c                       2) There was included an explicit energy check at each A iteration to
c                          force an exit when this energy fails to decrease. This change was
c                          prompted by a single failure in a parallel run (courtesy Russ
c                          Treadon) in which the A and B iterations flip-flopped at zero
c                          energy change in a case of grazing contact with a gatepost.
c                       3) The energy is now normalized by the energy that would be computed
c                          from a spline that fits only the first and last gateposts. The
c                          renormalized energy diagnostic tells how sinuous the final profile
c                          is -- very large values are indiciative of a halfgate chosen to be
c                          too narrow for the given profile data.
c                       4) The normalized time data are now handled as integer arrays instead
c                          of reals in those parts of the code dealing with the combinatorics
c                          of routes.  This is just better coding practice.
c 2015-04-17  Y. Zhu -- Updates to subroutine sub2mem_mer:
c                       1) Subroutine is more robust.  If there is an error in the generation
c                          of vertical velocity rate in the tension-spline interpolation
c                          utility pspl (called in this subroutine), this subroutine (and thus
c                          the program itself) will no longer abort (with either c. code 62,
c                          63 or 64 depending upon which routine inside pspl generated the
c                          error) but will instead revert to the finite difference method for
c                          calculating vertical velocity rate.
c                       2) Previously, halfgate was set to be 30 for the data profiles that
c                          don't have second information in time, but a tighter value of 10
c                          for the data profiles that do have second information in time. Now
c                          halfgate is relaxed to be 30 for the data profiles that do have
c                          complete time information. 
c 2016-10-11 M.Sienkiewicz Added a namelist variable and additional code to allow use of an
c                          alternate BUFR table definition file when generating the profile file.
c                          (Solves a problem with mixed BUFR files used for input.)
c 2016-11-09  C. Hill -----
c                 - Increased the maximum number of flights that can be processed, "MAXFLT",
c                   from 7500 to 12500 to resolve >90% warning.
c 2016-12-09  D. Keyser  --
c                 - Nomenclature change: replaced "MDCRS/ACARS" with just "MDCRS".
c                 - New LATAM AMDARs contain an encrypted flight number (in addition to a tail
c                   number, all other AMDARs have only a tail number which is copied into
c                   flight number). Read this in and use in QC processing.
c                   BENEFIT: Improves track-checking and other QC for LATAM AMDARs.
c                 - Since "ACARS" as referred to in NRL QC kernal (acftobs_qc.f) is not used
c                   there and we earlier decided to use this to provide a separate category
c                   for TAMDARs in the NRL QC kernal (for stratifying statistics), all
c                   printout in acftobs_qc.f changes the term "ACARS" to "TAMDAR".  In
c                   addition, all comments now refer to "TAMDAR" instead of "ACARS".
c                 - Variables holding latitude and longitude data (including arrays "alat" and
c                   "alon" passed between subroutines) now double precision. XOB and YOB in
c                   PREPBUFR file now scaled to 10**5 (was 10**2) to handle new v7 AMDAR and
c                   MDCRS reports which have this higher precision.
c                   BENEFIT: Retains exact precison here. Improves QC processing.
c                      - Note: QC here can be improved further by changing logic in many
c                              places to account for the increased precision. This needs to be
c                              investigated.  For now, locations in code where this seems
c                              possible are noted by the spanning comments:
c                    ! vvvv DAK-future change perhaps to account for incr. lat/lon precision
c                    ! ^^^^ DAK-future change perhaps to account for incr. lat/lon precision
c                      - The format for all print statements containing latitude and longitude
c                        changed to print to 5 decimal places.
c
c Usage:
c   Input files:
c     Unit 05  - Standard input (namelist)
c     Unit 11  - PREPBUFR file containing all obs, prior to any processing by this program
c     Unit 12  - file with external table for profile output (if needed)
c
c   Output files:
c     Unit 06  - Standard output print
c     Unit 08  - Text file containing full log of all NRL QC information
c     Unit 30  - Text file containing duplicate data check information
c     Unit 31  - Text file containing spike data check information
c     Unit 32  - Text file containing invalid data check information
c     Unit 33  - Text file containing stuck data check information
c     Unit 34  - Text file containing gross check information
c     Unit 35  - Text file containing position check information
c     Unit 36  - Text file containing ordering check information
c     Unit 37  - Text file containing suspect data check information
c     Unit 38  - Text file containing reject list information
c     Unit 41  - Text file containing vertical velocity rate calculation information for QC'd
c                merged aircraft reports written to profiles PREPBUFR-like file
c     Unit 51  - Text file containing sorted listing of all single-level QC'd aircraft
c                reports written back to full PREPBUFR file
c     Unit 52  - Text file containing sorted listing of all QC'd merged aircraft reports
c                written to profiles PREPBUFR-like file
c     Unit 61  - PREPBUFR file identical to input except containing NRLACQC events
c     Unit 62  - PREPBUFR-like file containing merged (mass and wind) profile reports
c                (always) and single(flight)-level reports not part of any profile (when
c                l_prof1lvl=T) with NRLACQC events
c
c   Subprograms called:
c     Unique:    - ACFTOBS_QC        PR_WORKDATA INDEXC             DUPCHEK_QC
c                - REORDER           DO_FLT      DO_REG             INNOV_QC
c                - BENFORD_QC        INVALID_QC  STK_VAL_QC         GRCHEK_QC
c                - POSCHEK_QC        ORDDUP_QC   ORDCHEK_QC         SUSPECT_QC
c                - REJLIST_QC        P2HT_QC     HT2FL_QC           P_DDTG
c                - SPIKE_QC          SLEN        INSTY_OB_FUN       C_INSTY_OB
c                - GCIRC_QC          INDEXC40    INPUT_ACQC         OUTPUT_ACQC_NOPROF
c                - OUTPUT_ACQC_PROF  SUB2MEM_MER SUB2MEM_UM         TRANQCFLAGS
C     LIBRARY:
c       SYSTEM:  - SYSTEM
c       W3NCO:   - ERREXIT    W3TAGB     W3TAGE     W3MOVDAT   MOVA2I     W3FI04
c       W3EMC:   - W3FC05     ORDERS
c       BUFRLIB: - IREADMG    IREADSB    UFBINT     UFBSEQ     UFBEVN     READNS     IBFMS
c                - COPYMG     OPENMB     UFBCPY     WRITSB     WRITLC     CLOSMG     DATELEN
c                - OPENBF     CLOSBF     UFBQCD     SETBMISS   GETBMISS
c                
c   Exit states:
c     Cond =   0 - successful run
c              4 - no aircraft reports of any type read in
c             23 - unexpected return code from readns; problems reading BUFR file
c             31 - indexing problem encountered when trying to match QC'd data in arrays to
c                  mass and wind pieces in original PREPBUFR file (subroutine
c                  output_acqc_noprof)
c             59 - nlvinprof is zero coming into subroutine sub2mem_mer (should never
c                  happen!)
c             61 - index "j is .le. 1 meaning "iord" array underflow (should never happen!)
c                  (subroutine sub2mem_mer)
c             69 - row number for input data matrix is outside range of 1-34 (subroutine
c                  tranQCflags)
c             79 - characters on this machine are not ASCII, conversion of quality flag to
c                  row number in subroutine tranQCflags cannot be made
c             98 - too many flights in input PREPBUFR file, must increase size of parameter
c                  "maxflt" (in some places code continues but in this case can't be sure
c                  continuing on w/o processing any more data would turn out ok)
calloc        99 - unable to allocate one or more array
c
c   Remarks:
c      Input Namelist switches (namelist &nrlacqcinput)):
c            trad           - time window radius in hours for outputting reports (if l_otw=T)
c                             (default=3.0)
c            l_otw          - logical:
c                                 TRUE  - eliminate reports outside the time window radius
c                                         +/- trad when writing out reports
c                                   
c                                 FALSE - DO NOT eliminate reports outside the time window
c                                         radius +/- trad when writing out reports
c                                 (default=FALSE)
c            l_nhonly       - logical:
c                                 TRUE  - eliminate reports outside tropics & N. Hemisphere
c                                         when writing out reports
c                                 FALSE - DO NOT eliminate reports outside tropics & N.
c                                         Hemisphere when writing out reports
c                                 (default=FALSE)
c            l_doprofiles   - logical:
c                                 TRUE  - create merged raob lookalike QC'd profiles from
c                                         aircraft ascents and descents (always) and output
c                                         these as well as QC'd merged single(flight)-level
c                                         aircraft reports not part of any profile (when
c                                         l_prof1lvl=T) to a PREPBUFR-like file
c                                         **CAUTION: Will make code take quite a bit longer
c                                                    to run!
c                                 FALSE - SKIP creation of merged raob lookalike QC'd
c                                         profiles from aircraft ascents and descents into
c                                         PREPBUFR-like file
c                                 (default=FALSE)
c            l_allev_pf     - logical:
c                                 TRUE  - process latest (likely NRLACQC) events plus all
c                                         prior events into profiles PREPBUFR-like file
c                                         **CAUTION: More complete option, but will make code
c                                                    take longer to run!
c                                 FALSE - process ONLY latest (likely NRLACQC) events into
c                                         profiles PREPBUFR-like file
c                                 (Note 1: Hardwired to FALSE if l_doprofiles=FALSE)
c                                 {Note 2: All pre-existing events plus latest (likely
c                                          NRLACQC) events are always encoded into full
c                                          PREPBUFR file}
c                                 (default=FALSE)
c            l_prof1lvl     - logical:
c                                 TRUE  - encode merged single(flight)-level aircraft reports
c                                         with NRLACQC events that are not part of any
c                                         profile into PREPBUFR-like file, along with merged
c                                         profiles from aircraft ascents and descents
c                                         **CAUTION: Will make code take a bit longer to run!
c                                 FALSE - DO NOT encode merged single(flight)-level aircraft
c                                         reports with NRLACQC events that are not part of
c                                         any profile into PREPBUFR-like file
c                                         - only merged profiles from aircraft ascents and
c                                         descents will be encoded into this file
c                                 (Note:  Applicable only when l_doprofiles=TRUE)
c                                 (default=FALSE)
c            l_mandlvl      - logical:
c                                 TRUE  - interpolate obs data to mandatory levels in profile
c                                         generation
c                                 FALSE - DO NOT interpolate obs data to mandatory levels in
c                                         profile generation
c                                 (Note:  Applicable only when l_doprofiles=TRUE)
c                                 (default=TRUE)
c            tsplines       - logical:
c                                 TRUE  - use Jim Purser's tension-spline interpolation
c                                         utility to generate aircraft vertical velocity rate
c                                         in profile generation
c                                 FALSE - use finite-difference method based on nearest
c                                         neighboring pair of obs which are at least one
c                                         minute apart to generate aircraft vertical velocity
c                                         rate in profile generation
c                                 (Note:  Applicable only when l_doprofiles=TRUE)
c                                 (default=TRUE)
c
c Attributes:
c   Language: FORTRAN 90
c   Machine:  NCEP WCOSS
c
c$$$
      program prepobs_prepacqc 

      implicit none

c ------------------------------
c Parameter statements/constants
c ------------------------------
      integer    inlun                ! input unit number (for pre-prepacqc PREPBUFR file
                                      !  containing all obs)
      parameter (inlun = 11)

      integer    extbl                ! unit number for external table file (if used)
      parameter (extbl = 12)

      integer    outlun               ! output unit number for post-PREPACQC PREPBUFR file
                                      !  with added NRLACQC events
      parameter (outlun=61)

      integer    proflun              ! output unit number for post-PREPACQC PREPBUFR-like
                                      !  file containing merged profile reports (always) and
      parameter (proflun=62)

      integer    max_reps             ! maximum number of input merged (mass + wind piece)
                                      !  aircraft-type reports allowed
      parameter (max_reps = 300000)

cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
c replace above with this in event of future switch to dynamic memory allocation

calloc  integer      max_reps          ! original number of input merged (mass + wind piece)
calloc                                 !  aircraft-type reports (obtained from first pass
calloc                                 !  through input PREPBUFR file to get total for array
calloc                                 !  allocation should = nrpts4QC_pre)
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      integer    maxflt               ! maximum number of flights allowed (inside NRL QC)
      parameter (maxflt = 12500)
      character*6  cmaxflt            ! character form of maxflt

      integer    imiss                ! NRL integer missing value flag
      parameter (imiss = 99999)

      real       amiss                ! NRL real missing value flag
      parameter (amiss = -9999.)

      real*8     bmiss                ! BUFR missing value
      real*8     getbmiss             ! Function to return current bmiss value from BUFRLIB

      real         m2ft               ! NRL conversion factor to convert m to ft

      parameter (m2ft = 3.28084)

c ----------------------
c Declaration statements
c ----------------------

c Indices/counters
c ----------------
      integer      i,j                ! loop indeces

      integer      nrpts4QC_pre       ! original number of input merged (mass + wind piece)
                                      !  aircraft-type reports (read in from PREPBUFR file)
                                      !  (after all is said and done, should equal nrpts4QC +
                                      !   krej)

      integer      nrpts4QC           ! number of merged (mass + wind piece) reports going
                                      !  through NRL QC code  (initially equals nrpts4QC_pre,
                                      !  then reduced as processing continues - ultimately
                                      !  includes only "good" reports)

      integer      krej               ! number of merged (mass + wind piece) reports
                                      !  ulimately rejected by NRL QC code

c Observation variables required by the NRL aircraft QC routine
c -------------------------------------------------------------
      character*10   cdtg_an          ! date-time group for analysis (YYYYMMDDCC)


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

      character*25 csort(max_reps)    ! variable (sort key) used for sorting data in NRL QC
                                      !  code

      integer      itype(max_reps)     ! instrument (aircraft) type
      real*8       alat(max_reps)      ! latitude
     +,            alon(max_reps)      ! longitude
      real         pres(max_reps)      ! pressure
     +,            ht_ft(max_reps)     ! altitude in feet
      integer      idt(max_reps)       ! time in seconds to anal. time (- before, + after)
      integer      idp(max_reps)       ! surface pressure change at ob location (not created
                                       !  anywhere, set to missing)
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

      character*14 c_dtg(max_reps)     ! full date-time group (yyyymmddhhmmss)
      character*8  c_acftreg(max_reps) ! aircraft registration (tail) number (used in NRL QC
                                       !  QC processing)
      character*9  c_acftid(max_reps)  ! aircraft flight number (used in NRL QC processing)

      real         t_prcn(max_reps)    ! temperature precision
     +,            ob_t(max_reps)      ! temperature
     +,            ob_q(max_reps)      ! moisture (specific humidity)
     +,            ob_dir(max_reps)    ! wind direction
     +,            ob_spd(max_reps)    ! wind speed
     +,            xiv_t(max_reps)     ! temperature innovation/increment (ob-bg)
     +,            xiv_q(max_reps)     ! specific humidity innovation/increment (ob-bg)
     +,            xiv_d(max_reps)     ! wind direction innovation/increment (ob-bg)
     +,            xiv_s(max_reps)     ! wind speed innovation/increment (ob-bg)

      integer      ichk_t(max_reps)    ! NRL QC flag for temperature ob
     +,            ichk_q(max_reps)    ! NRL QC flag for specific humidity ob
     +,            ichk_d(max_reps)    ! NRL QC flag for wind direction ob
     +,            ichk_s(max_reps)    ! NRL QC flag for wind speed ob
     +,            nchk_t(max_reps)    ! NCEP QC flag for temperature ob
     +,            nchk_q(max_reps)    ! NCEP QC flag for specific humidity ob
     +,            nchk_d(max_reps)    ! NCEP QC flag for wind direction ob
     +,            nchk_s(max_reps)    ! NCEP QC flag for wind speed ob
     +,            phase(max_reps)     ! phase of flight for aircraft

      logical      l_minus9c(max_reps) ! true for MDCRS -9C temperatures

c Pointers
c --------
      integer      indx(max_reps)      ! pointer index in NRL QC for good reports
     +,            in_bad(max_reps)    ! pointer index in NRL QC for bad reports
     +,            isave(max_reps)     ! second pointer index in NRL QC

c **************************************************
c  All below are output from NRL acftobs_qc routine
c **************************************************

c Flight statistics
c -----------------
      character*8  creg_flt(maxflt)    ! tail number for each flight
      character*9  cid_flt(maxflt)     ! flight id for each flight
     +,            cid_flt_old(maxflt) ! previous value of cid_flt
      integer      nobs_flt(maxflt)    ! number of reports per flight
     +,            ntot_flt(maxflt)    ! total number of reports per flight
     +,            ntot_flt_old(maxflt)! previous value of total num of reports per flt
     +,            nrej_flt(maxflt)    ! number of reports rejected per flight
     +,            nrej_flt_old(maxflt)! previous value of num of reports rejected per flt
     +,            iobs_flt(maxflt)    ! index for first report in each flight
     +,            kflight             ! number of flights in dataset
      logical      l_newflt(maxflt)    ! true if flight is new flight

c Tail number statistics
c ----------------------
      character*8  creg_reg(maxflt)    ! tail numbers
      integer      nobs_reg(maxflt,5)  ! number of reports per tail number per type
     +,            ntot_reg(maxflt,5)  ! total number of reports rejected per tail number
     +,            nrej_reg(maxflt,5)  ! number of reports rejected per tail number
     +,            ntemp_reg(maxflt,5) ! number of reports with rejected temperature
     +,            nwind_reg(maxflt,5) ! number of reports with rejected wind
     +,            nwhol_reg(maxflt,5) ! number of reports with temperature in whole degrees

      character*10 creg_reg_tot(maxflt)     ! master list of tail numbers
      integer      nobs_reg_tot(maxflt,5)   ! number of reports per tail number
     +,            nwhol_reg_tot(maxflt,5)  ! number of temperatures in whole degs/tail number
     +,            nrej_reg_tot(maxflt,5)   ! number of reports rejected per tail number
     +,            ntemp_reg_tot(maxflt,5)  ! number of temperatures rejected per tail number
     +,            nwind_reg_tot(maxflt,5)  ! number of winds rejected per tail number
     +,            nrej_inv_tot(maxflt,5)   ! number of reports rejected in subr. invalid
     +,            nrej_stk_tot(maxflt,5)   ! number of reports rejected in subr. stkchek
     +,            nrej_grc_tot(maxflt,5)   ! number of reports rejected in subr. grchek
     +,            nrej_pos_tot(maxflt,5)   ! number of reports rejected in subr. poschek
     +,            nrej_ord_tot(maxflt,5)   ! number of reports rejected in subr. ordchek
     +,            nrej_sus_tot(maxflt,5)   ! number of reports rejected in suspect data check

      integer      lead_t_tot(maxflt,11,2)  ! distribution of temperature innovations
     +,            lead_d_tot(maxflt,11,2)  ! distribution of wind direction innovations
     +,            lead_s_tot(maxflt,11,2)  ! distribution of wind speed innovations
     +,            n_xiv_t(maxflt,2)        ! number of temperature innovations
     +,            n_xiv_d(maxflt,2)        ! number of wind direction innovations
     +,            n_xiv_s(maxflt,2)        ! number of wind speed innovations

      real         sum_xiv_t(maxflt,2)      ! sum of temperature innovations
     +,            sum_xiv_d(maxflt,2)      ! sum of wind direction innovations
     +,            sum_xiv_s(maxflt,2)      ! sum of wind speed innovations
     +,            sumabs_xiv_t(maxflt,2)   ! sum of absolute value of temperature innovations
     +,            sumabs_xiv_d(maxflt,2)   ! sum of absolute value of wind dir. innovations
     +,            sumabs_xiv_s(maxflt,2)   ! sum of absolute value of wind speed innovations

c **************************************************

c Variables for sorting data by type, tail, flight, etc., including bad reports - will be
c  used AFTER NRL QC code in the generation of profiles PREPBUFR-like profiles file
c ---------------------------------------------------------------------------------------
      integer      iob                      ! loop index
     +,            kidt                     ! idt + 100000 (converted to charcter c_idt and
                                            !  added to csort_wbad sort key string)
     +,            iht_ft                   ! integer of ht_ft (converted to charcter c_ht_ft
                                            !  and added to csort_wbad sort key string)
     +,            ilon                     ! integer of alon (converted to charcter c_lon
                                            !  and added to csort_wbad sort key string)
     +,            ilat                     ! integer of alat (converted to charcter c_lat
                                            !  and added to csort_wbad sort key string)
      character*6  c_lon                    ! character form of ilon (added to csort_wbad
                                            !  sort key string)
      character*7  c_idt                    ! character form of kidt (added to csort_wbad
                                            !  sort key string)
      character*5  c_ht_ft                  ! character form of iht_ft (added to csort_wbad
                                            !  sort key string)
     +,            c_lat                    ! character form of ilat (added to csort_wbad
                                            !  sort key string)
      character*4  c_type                   ! first 4 characters defining aircraft type
                                            !  (added to csort_wbad sort key string)
      character*1  c_qc11                   ! value of 11th char in NRL c_qc string,
                                            !  specifies whether report is part of an ascent,
                                            !  descent, level leg, etc. (added to csort_wbad
                                            !  sort key string)
      character*16 c_insty_ob               ! function - convers aircraft type to character
                                            !  string ((added to csort_wbad sort key string)
      character*40 csort_wbad(max_reps)     ! variable (sort key) used to sort data after NRL
                                            !  QC code - used in generation of profiles
                                            !  PREPBUFR-like profiles file
      integer indx_wbad(max_reps)           ! sorted array index (specifies the order in
                                            !  which reports should be written to the
                                            !  PREPBUFR-like profiles file
c Namelist variables
c ------------------
      namelist /nrlacqcinput/ trad,l_otw,l_nhonly,l_doprofiles,
     +                        l_allev_pf,l_prof1lvl,l_mandlvl,tsplines,
     +                        l_ext_table,l_qmwrite

      real trad               ! Time window radius for outputting reports (if l_otw=T)
      logical l_otw           ! T=eliminate reports outside the time window radius +/- trad
     +,       l_nhonly        ! T=eliminate reports outside tropics & N. Hemisphere
     +,       l_doprofiles    ! T=create merged raob lookalike QC'd profiles from aircraft
                              !   ascents and descents (always) and output these as well as
                              !   QC'd merged single(flight)-level aircraft reports not part
                              !   of any profile (when l_prof1lvl=T) to a PREPBUFR-like file
                              !   **CAUTION: Will make code take quite a bit longer to run!
                              ! F=skip creation of merged raob lookalike QC'd profiles from
                              !   aircraft ascents and descents into PREPBUFR-like file
     +,       l_allev_pf      ! T=process latest (likely NRLACQC) events plus all prior
                              !   events into profiles PREPBUFR-like file
                              !   **CAUTION: More complete option, but will make code take
                              !              longer to run!
                              ! F=process ONLY latest (likely NRLACQC) events into profiles
                              !   PREPBUFR-like file
                              !
                              ! Note 1: Hardwired to F if l_doprofiles=F
                              ! Note 2: All pre-existing events plus latest (likely NRLACQC)
                              !         events are always encoded into full PREPBUFR file)
     +,       l_prof1lvl      ! T=encode merged single(flight)-level aircraft reports with
                              !   NRLACQC events that are not part of any profile into
                              !   PREPBUFR-like file, along with merged profiles from
                              !   aircraft ascents and descents
                              !   **CAUTION: Will make code take a bit longer to run!
                              ! F=do not encode merged single(flight)-level aircraft reports
                              !   with NRLACQC events that are not part of any profile into
                              !   PREPBUFR-like file - only merged profiles from aircraft
                              !   ascents and descents will be encoded into this file
                              ! Note : Applicable only when l_doprofiles=T
     +,       l_mandlvl       ! T=interpolate to mandatory levels in profile generation
                              ! F=do not interpolate to mandatory levels in profile
                              !   generation
     +,       tsplines        ! T=use tension-splines for aircraft vertical velocity
                              !   calculation
                              ! F=use finite-differencing for aircraft vertical velocity
                              !   calculation
                              ! Note : Applicable only when l_doprofiles=T
     +,       l_ext_table     ! T=use external text table to define profile prepbufr format
                              ! F=take prepbufr format definition from input prepbufr file
     +,       l_qmwrite       ! T=write NRL QMs in main prepbufr output file
                              ! F=omit NRL QMs from main prepbufr output file - use with old formats

c Variables used to hold original aircraft data read from the input PREPBUFR file - necessary
c  for carrying data through program so that it can be written to output profiles PREPBUFR-
c  like file from memory instead of going back to input PREPBUFR file and re-reading that
c  file before adding any QC events resulting from a decision made by the NRL QC routine (not
c  applicable for case of single-level QC'd reports written back to full PREPBUFR file)
c --------------------------------------------------------------------------------------------
      integer      mxnmev             ! maximum number of events allowed in stack
      parameter (mxnmev = 15)

      integer      mxlv               ! maximum number of report levels allowed in aircraft
                                      !  profiles
      parameter(mxlv = 255)


      integer nevents(max_reps,6)     ! array tracking number of events for variables for
                                      !  each report:
                                      !   1 - number of pressure events
                                      !   2 - number of specific humidity events
                                      !   3 - number of temperature events
                                      !   4 - number of altitude events
                                      !   5 - number of wind (u/v) events
                                      !   6 - number of wind (direction/speed) events
      integer nnestreps(4,max_reps)   ! number of "nested replications" for TURB3SEQ,
                                      !  PREWXSEQ, CLOUDSEQ, AFIC_SEQ

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

     +,       turb1seq(max_reps)      ! TRBX
     +,       turb2seq(max_reps,4)    ! TRBX10 TRBX21 TRBX32 TRBX43
     +,       turb3seq(3,max_reps,5)  ! DGOT HBOT HTOT
     +,       prewxseq(1,max_reps,5)  ! PRWE
     +,       cloudseq(5,max_reps,5)  ! VSSO CLAM CLTP HOCB HOCT
     +,       afic_seq(3,max_reps,5)  ! AFIC HBOI HTOI
     +,       mstq(max_reps)          ! MSTQ
     +,       cat(max_reps)           ! CAT
     +,       rolf(max_reps)          ! ROLF

     +,       sqn(max_reps,2)         ! SQN (1=SQN for mass, 2=SQN for wind)
     +,       procn(max_reps,2)       ! PROCN (1=PROCN for mass, 2=PROCN for wind)

cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
c add these in place of above declar. in event of future switch to dynamic memory allocation

calloc  character*11,allocatable :: c_qc(:)
calloc  character*25,allocatable :: csort(:)
calloc  integer,allocatable :: itype(:)
calloc  real*8, allocatable :: alat(:)
calloc  real*8, allocatable :: alon(:)
calloc  real,   allocatable :: pres(:)
calloc  real,   allocatable :: ht_ft(:)
calloc  integer,allocatable :: idt(:)
calloc  integer,allocatable :: idp(:)
calloc  character*14,allocatable :: c_dtg(:)
calloc  character*8, allocatable :: c_acftreg(:)
calloc  character*9, allocatable :: c_acftid(:)
calloc  real,   allocatable :: t_prcn(:)
calloc  real,   allocatable :: ob_t(:)
calloc  real,   allocatable :: ob_q(:)
calloc  real,   allocatable :: ob_dir(:)
calloc  real,   allocatable :: ob_spd(:)
calloc  real,   allocatable :: xiv_t(:)
calloc  real,   allocatable :: xiv_q(:)
calloc  real,   allocatable :: xiv_d(:)
calloc  real,   allocatable :: xiv_s(:)
calloc  integer,allocatable :: ichk_t(:)
calloc  integer,allocatable :: ichk_q(:)
calloc  integer,allocatable :: ichk_d(:)
calloc  integer,allocatable :: ichk_s(:)
calloc  integer,allocatable :: nchk_t(:)
calloc  integer,allocatable :: nchk_q(:)
calloc  integer,allocatable :: nchk_d(:)
calloc  integer,allocatable :: nchk_s(:)
calloc  integer,allocatable :: phase(:)
calloc  logical,allocatable :: l_minus9c(:)
calloc  integer,allocatable :: indx(:)
calloc  integer,allocatable :: in_bad(:)
calloc  integer,allocatable :: isave(:)
calloc  character*40,allocatable :: csort_wbad(:)
calloc  integer,allocatable :: indx_wbad(:)
calloc  integer,allocatable :: nevents(:,:)
calloc  integer,allocatable :: nnestreps(:,:)
calloc  real*8,allocatable :: pob_ev(:,:)
calloc  real*8,allocatable :: pqm_ev(:,:)
calloc  real*8,allocatable :: ppc_ev(:,:)
calloc  real*8,allocatable :: prc_ev(:,:)
calloc  real*8,allocatable :: zob_ev(:,:)
calloc  real*8,allocatable :: zqm_ev(:,:)
calloc  real*8,allocatable :: zpc_ev(:,:)
calloc  real*8,allocatable :: zrc_ev(:,:)
calloc  real*8,allocatable :: tob_ev(:,:)
calloc  real*8,allocatable :: tqm_ev(:,:)
calloc  real*8,allocatable :: tpc_ev(:,:)
calloc  real*8,allocatable :: trc_ev(:,:)
calloc  real*8,allocatable :: qob_ev(:,:)
calloc  real*8,allocatable :: qqm_ev(:,:)
calloc  real*8,allocatable :: qpc_ev(:,:)
CAlloc  real*8,allocatable :: qrc_ev(:,:)
calloc  real*8,allocatable :: uob_ev(:,:)
calloc  real*8,allocatable :: vob_ev(:,:)
calloc  real*8,allocatable :: wqm_ev(:,:)
calloc  real*8,allocatable :: wpc_ev(:,:)
calloc  real*8,allocatable :: wrc_ev(:,:)
calloc  real*8,allocatable :: ddo_ev(:,:)
calloc  real*8,allocatable :: ffo_ev(:,:)
calloc  real*8,allocatable :: dfq_ev(:,:)
calloc  real*8,allocatable :: dfp_ev(:,:)
calloc  real*8,allocatable :: dfr_ev(:,:)
calloc  real*8,allocatable :: hdr(:,:)
calloc  real*8,allocatable :: acid(:)
calloc  real*8,allocatable :: rct(:)
calloc  real*8,allocatable :: pbg(:,:)
calloc  real*8,allocatable :: zbg(:,:)
calloc  real*8,allocatable :: tbg(:,:)
calloc  real*8,allocatable :: qbg(:,:)
calloc  real*8,allocatable :: wbg(:,:)
calloc  real*8,allocatable :: ppp(:,:)
calloc  real*8,allocatable :: zpp(:,:)
calloc  real*8,allocatable :: tpp(:,:)
calloc  real*8,allocatable :: qpp(:,:)
calloc  real*8,allocatable :: wpp(:,:)
calloc  real*8,allocatable :: drinfo(:,:)
calloc  real*8,allocatable :: acft_seq(:,:)
calloc  real*8,allocatable :: turb1seq(:)
calloc  real*8,allocatable :: turb2seq(:,:)
calloc  real*8,allocatable :: turb3seq(:,:,:)
calloc  real*8,allocatable :: prewxseq(:,:,:)
calloc  real*8,allocatable :: cloudseq(:,:,:)
calloc  real*8,allocatable :: afic_seq(:,:,:)
calloc  real*8,allocatable :: mstq(:)
calloc  real*8,allocatable :: cat(:)
calloc  real*8,allocatable :: rolf(:)
calloc  real*8,allocatable :: sqn(:,:)
calloc  real*8,allocatable :: procn(:,:)

c Variables for reading numeric data out of BUFR files via BUFRLIB
c ----------------------------------------------------------------
calloc  real*8       sqn_8             ! array holding BUFR subset sequence number from
calloc                                 !  BUFRLIB call to input PREPBUFR file
calloc  integer      nlev              ! number of report levels returned from BUFRLIB call
calloc  Integer      iret              ! return code for call to BUFRLIB routine readns

c Functions
c ---------
calloc  integer      ireadmg           ! for reading messages
callo+,              ireadsb           ! for reading subsets


c Variables for BUFRLIB interface
c -------------------------------
calloc  character*8  mesgtype          ! mesgtype of message
calloc  integer      mesgdate          ! date time from BUFR message

c Variables for determining whether consecutive reports are mass and wind pieces that belong
c  together
c ------------------------------------------------------------------------------------------
calloc  logical l_match
calloc  real sqn_current, sqn_next

c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

c Miscellaneous
c -------------
      real       nrlacqc_pc              ! PREPBUFR program code for the NRL PREPACQC step

      logical    l_first_date            ! true for first date (used inside NRL QC code)
      data       l_first_date /.true./   ! always initialize as T

      logical    l_operational           ! run program in operational mode if true
      data       l_operational /.true./  ! will get reset to F within acftobs_qc since
                                         !  l_ncep=T; must be set to true here so that the
                                         !  first l_operational=F section of the if block in
                                         !  acftobs_qc.f will get skipped over
                              ! DAK: would code run faster if l_operational=F?, does it give
                              !      same answers I wonder ??
      logical    l_pc                    ! true if running checkout at NRL (used inside NRL
                                         !  QC code)
      data       l_pc /.false./          ! always set to F
      logical    l_last                  ! true if last time subroutine acftobs_qc is called
      data       l_last /.true./         ! DAK: I think this should be set to T
      logical    l_ncep                  ! run NRL QC code using NCEP preferences if true
      data       l_ncep /.true./         ! always set to T

c Machine characteristics (obtained from W3FI04)
c ----------------------------------------------
      integer    lwr                     ! machine word length in bytes (either 4 or 8)
     +,          ichtp                   ! machine charatcer type (either 0 for ASCII or 1
                                         !  for EBCDIC)
     +,          iendn                   ! machine Endian configuration (either 0 for Big-
                                         !  Endian or 1 for Little-Endian)

c **********************************************************************************

c Start program
c -------------
      call w3tagb('PREPOBS_PREPACQC',2016,344,1927,'NP22')

      write(*,*)
      write(*,*) '************************************************'
      write(*,*) 'Welcome to PREPOBS_PREPACQC, version 2016-12-09 '
      call system('date')
      write(*,*) '************************************************'
      write(*,*)

C On WCOSS should always set BUFRLIB missing (BMISS) to 10E8 to avoid overflow when either an
C  INTEGER*4 variable is set to BMISS or a REAL*8 (or REAL*4) variable that is missing is
C  NINT'd
C -------------------------------------------------------------------------------------------
ccccc call setbmiss(10E10_8)
      call setbmiss(10E8_8)
      bmiss = getbmiss()
      print *
      print *, 'BUFRLIB value for missing is: ',bmiss
      print *

c Initialize observation arrays
c -----------------------------
      c_qc      = '-----------'
      idp       = imiss  ! this is not created anywhere (even inside acftobs_qc)

c Call W3FI04 to determine machine characteristics {word length (bytes), character type
c  (ASCII or EBCDIC), and Endian-type (Big or Little)}
c -------------------------------------------------------------------------------------
      call w3fi04(iendn,ichtp,lwr)
      print 2213, lwr, ichtp, iendn
 2213 format(/' ---> CALL TO W3FI04 RETURNS: LWR = ',I3,', ICHTP = ',i3,
     + ', IENDN = ',I3/)

c......................................................
      if(ichtp.ne.0)  then

C Characters on this machine are not ASCII!! -- stop 79
c -----------------------------------------------------
        print 217
  217 format(/5x,'++ CHARACTERS ON THIS MACHINE ARE NOT ASCII - STOP ',
     + '79'/)
        call w3tage('PREPOBS_PREPACQC')
        call errexit(79)
      endif
c......................................................

c Read in namelist nrlacqcinput, but set namelist defaults first
c --------------------------------------------------------------
      trad         =    3.0
      l_otw        = .false.
      l_nhonly     = .false.
      l_doprofiles = .false.
      l_allev_pf   = .false.
      l_prof1lvl   = .false.
      l_mandlvl    = .true.
      tsplines     = .true.
      l_ext_table  = .false.
      l_qmwrite    = .true.

      read(5,nrlacqcinput,end=10)
   10 continue
      write(6,nrlacqcinput)

      if(.not.l_doprofiles) l_allev_pf = .false. ! l_allev_pf always set to FALSE if profiles
                                                 !  are not being generated

      call datelen(10)

c Open input PREPBUFR file (contains mass and wind reports for all data types, no NRLACQC
c  events on reports in AIRCAR and AIRCFT message types)
c ---------------------------------------------------------------------------------------
      call openbf(inlun,'IN',inlun)
      print *
      print'(" Opened input PREPBUFR file with all data, including ",
     +       "pre-NRLACQC aircraft data; unit number ",I0)', inlun
      print *

c Open output PREPBUFR file (will eventually be identical to input PREPBUFR file but with
c  NRLACQC events on reports in AIRCAR and AIRCFT message types)
c ---------------------------------------------------------------------------------------
      call openbf(outlun,'OUT',inlun)
      print *
      print'(" Opened output PREPBUFR file - will hold all data, ",
     +       "including post-NRLACQC aircraft data; unit number ",I0)',
     +     outlun
      print *

      if(l_doprofiles) then

c Open output PREPBUFR-like file (will eventually contain merged aircraft mass/wind data in
c  AIRCAR and AIRCFT message types, including constructed profiles, with NRLACQC events on
c  reports)
c -----------------------------------------------------------------------------------------
        if (l_ext_table) then
           open(unit=extbl,form='formatted')
           call openbf(proflun,'OUT',extbl)
           close(extbl)
        else
           call openbf(proflun,'OUT',inlun)
        end if
        print *
        print'(" Opened output PREPBUFR-like file - will hold only ",
     +         "post-NRLACQC merged aircraft profile data; unit ",
     +         "number ",I0)', proflun
        print *
      endif

c Get the program code for NRLACQC
c --------------------------------
      if (.not. l_qmwrite ) then
         nrlacqc_pc = 15
      else         
         call ufbqcd(outlun,'NRLACQC',nrlacqc_pc)
      end if

      print *
      print *, 'NRLACQC PROGRAM CODE IS: ', nrlacqc_pc
      print *

cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
c add this in event of future switch to dynamic memory allocation

calloc  CALL SYSTEM('date')
calloc  max_reps = 0
calloc  l_match = .false.
calloc  write(*,*) 'First time through just get count of number of ',
callo+   'merged reports for dynamic array allocation'
calloc  loop1: do while(ireadmg(inlun,mesgtype,mesgdate).eq.0)
calloc    if((mesgtype.eq.'AIRCFT').or.
callo+       (mesgtype.eq.'AIRCAR')) then
calloc      do while(ireadsb(inlun).eq.0)
c4051         continue
calloc        l_match = .false.
calloc        if(mesgtype.ne.'AIRCAR' .and. mesgtype.ne. 'AIRCFT')
callo+         cycle loop1
c5051         continue
calloc        max_reps = max_reps + 1
calloc        call ufbint(inlun,sqn_8,1,1,nlev,'SQN')
calloc        sqn_current = sqn_8
c6051         continue
calloc        if(l_match) then
calloc          call readns(inlun,mesgtype,mesgdate,iret)
calloc          if(iret.eq.-1) then
calloc            exit
calloc          elseif(iret.eq.0) then
calloc            go to 4051
calloc          else
calloc            print *, 'Unexpected return code(iret=',iret,
callo+                     ') from readns!'
calloc            call w3tage('PREPOBS_PREPACQC')
calloc            call errexit(23) ! Problems reading BUFR file
calloc          endif
calloc        endif
calloc        call readns(inlun,mesgtype,mesgdate,iret)
calloc        if(iret.eq.-1) then
calloc          exit
calloc        elseif(iret.eq.0) then
calloc          if(mesgtype.ne.'AIRCAR' .and. mesgtype.ne. 'AIRCFT')
callo+           cycle loop1
calloc          call ufbint(inlun,sqn_8,1,1,nlev,'SQN')
calloc          sqn_next = sqn_8
calloc          if(sqn_next.eq.sqn_current) then
calloc            l_match = .true.
calloc            go to 6051
calloc          else
calloc            l_match = .false.
calloc            go to 5051
calloc          endif
calloc        else
calloc          print *, 'Unexpected return code(iret=',iret,
callo+                   ') from readns!'
calloc          call w3tage('PREPOBS_PREPACQC')
calloc          call errexit(23) ! Problems reading BUFR file
calloc        endif
calloc      enddo
calloc    endif
calloc  enddo loop1
calloc  write(*,*)
calloc  write(*,*) 'TOTAL NUM OF RPTS IN FIRST READ THROUGH: ',
callo+   max_reps
calloc  call closbf(inlun)
calloc  call openbf(inlun,'IN',inlun)
calloc  CALL SYSTEM('date')
calloc  allocate(c_qc(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(csort(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(itype(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(alat(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(alon(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(pres(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(ht_ft(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(idt(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(idp(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(c_dtg(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(c_acftreg(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(c_acftid(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(t_prcn(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(ob_t(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(ob_q(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(ob_dir(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(ob_spd(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(xiv_t(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(xiv_q(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(xiv_d(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(xiv_s(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(ichk_t(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(ichk_q(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(ichk_d(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(ichk_s(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(nchk_t(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(nchk_q(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(nchk_d(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(nchk_s(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(phase(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(l_minus9c(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(indx(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(in_bad(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(isave(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(csort_wbad(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(indx_wbad(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(nevents(max_reps,6),stat=i);if(i.ne.0) go to 901
calloc  allocate(nnestreps(4,max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(pob_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(pqm_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(ppc_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(prc_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(zob_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(zqm_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(zpc_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(zrc_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(tob_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(tqm_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(tpc_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(trc_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(qob_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(qqm_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(qpc_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(qrc_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(uob_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(vob_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(wqm_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(wpc_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(wrc_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(ddo_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(ffo_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(dfq_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(dfp_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(dfr_ev(max_reps,mxnmev),stat=i);if(i.ne.0) go to 901
calloc  allocate(hdr(max_reps,15),stat=i);if(i.ne.0) go to 901
calloc  allocate(acid(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(rct(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(pbg(max_reps,3),stat=i);if(i.ne.0) go to 901
calloc  allocate(zbg(max_reps,3),stat=i);if(i.ne.0) go to 901
calloc  allocate(tbg(max_reps,3),stat=i);if(i.ne.0) go to 901
calloc  allocate(qbg(max_reps,3),stat=i);if(i.ne.0) go to 901
calloc  allocate(wbg(max_reps,5),stat=i);if(i.ne.0) go to 901
calloc  allocate(ppp(max_reps,3),stat=i);if(i.ne.0) go to 901
calloc  allocate(zpp(max_reps,3),stat=i);if(i.ne.0) go to 901
calloc  allocate(tpp(max_reps,3),stat=i);if(i.ne.0) go to 901
calloc  allocate(qpp(max_reps,3),stat=i);if(i.ne.0) go to 901
calloc  allocate(wpp(max_reps,6),stat=i);if(i.ne.0) go to 901
calloc  allocate(drinfo(max_reps,3),stat=i);if(i.ne.0) go to 901
calloc  allocate(acft_seq(max_reps,2),stat=i);if(i.ne.0) go to 901
calloc  allocate(turb1seq(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(turb2seq(max_reps,4),stat=i);if(i.ne.0) go to 901
calloc  allocate(turb3seq(3,max_reps,5),stat=i);if(i.ne.0) go to 901
calloc  allocate(prewxseq(1,max_reps,5),stat=i);if(i.ne.0) go to 901
calloc  allocate(cloudseq(5,max_reps,5),stat=i);if(i.ne.0) go to 901
calloc  allocate(afic_seq(3,max_reps,5),stat=i);if(i.ne.0) go to 901
calloc  allocate(mstq(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(cat(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(rolf(max_reps),stat=i);if(i.ne.0) go to 901
calloc  allocate(sqn(max_reps,2),stat=i);if(i.ne.0) go to 901
calloc  allocate(procn(max_reps,2),stat=i);if(i.ne.0) go to 901
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

c Call input routine input_acqc to read the input PREPBUFR file, merge the mass and wind
c  pieces, translate some values to NRL standards and store in memory (arrays)
c --------------------------------------------------------------------------------------
      write(*,*)
      write(*,*) 'Calling input_acqc....'
      write(*,*)

      call input_acqc(inlun,max_reps,mxnmev,bmiss,imiss,amiss,m2ft,mxlv,
     +                nrpts4QC_pre,cdtg_an,alat,alon,ht_ft,idt,c_dtg,
     +                itype,phase,t_prcn,c_acftreg,c_acftid,
     +                pres,ob_t,ob_q,ob_dir,ob_spd,
     +                ichk_t,ichk_q,ichk_d,ichk_s,
     +                nchk_t,nchk_q,nchk_d,nchk_s,
     +                xiv_t,xiv_q,xiv_d,xiv_s,l_minus9C,nevents,
     +                hdr,acid,rct,drinfo,acft_seq,turb1seq,turb2seq,
     +                turb3seq,prewxseq,cloudseq,afic_seq,mstq,cat,rolf,
     +                nnestreps,sqn,procn,
     +                pob_ev,pqm_ev,ppc_ev,prc_ev,pbg,ppp,
     +                zob_ev,zqm_ev,zpc_ev,zrc_ev,zbg,zpp,
     +                tob_ev,tqm_ev,tpc_ev,trc_ev,tbg,tpp,
     +                qob_ev,qqm_ev,qpc_ev,qrc_ev,qbg,qpp,
     +                uob_ev,vob_ev,wqm_ev,wpc_ev,wrc_ev,wbg,wpp,
     +                ddo_ev,ffo_ev,dfq_ev,dfp_ev,dfr_ev,l_allev_pf)

c Close input PREPBUFR file
c -------------------------
      call closbf(inlun)
      print *
      print'(" Closed input PREPBUFR file with all data, including ",
     +       "pre-NRLACQC aircraft data; unit number ",I0)', inlun
      print *

      write(*,*)
      write(*,*) 'Back from input_acqc....'
      write(*,'(" There are ",I0," merged reports for acftobs_qc (NRL ",
     +          "aircraft data QC routine).")') nrpts4QC_pre
      write(*,*)

      if(nrpts4QC_pre.gt.0) then

c Now that we are done reading in data from the input PREPBUFR file, need to call acftobs_qc
c  (actual NRL aircraft QC code)
c ------------------------------------------------------------------------------------------
      write(*,*) 'Passing ',nrpts4QC_pre,'obs to acftobs_qc.f...'
      write(*,*)
      write(*,*) 'Calling acftobs_qc...'

c NRPTS4QC_PRE is returned from input_acqc and represents the original number of "merged"
c  reports (mass and wind pieces put together) read in from the PREPBUFR file - we need to
c  save this value now as it will be used later (e.g., to correctly match the QC decisions
c  made by acftobs_qc to the reports originally in the input PREPBUFR file) - we will set
c  NRPTS4qc to NRPTS4QC_PRE at this point and then pass NRPTS4QC into acftobs_qc - the value
c  for NRPTS4Qc gets reduced in the various subroutines in acftobs_qc as it only represents
c  the number of "good" reports coming out of each subroutine
c-------------------------------------------------------------------------------------------  
      nrpts4QC = nrpts4QC_pre

      call acftobs_qc(max_reps,cdtg_an,nrpts4QC,krej,c_acftreg,c_acftid,
     +                itype,idt,idp,alon,alat,pres,ht_ft,ob_t,ob_q,
     +                ob_dir,ob_spd,t_prcn,xiv_t,xiv_q,xiv_d,xiv_s,
     +                ichk_t,ichk_q,ichk_d,ichk_s,nchk_t,nchk_q,nchk_d,
     +                nchk_s,indx,isave,in_bad,c_qc,csort,maxflt,
     +                kflight,creg_flt,cid_flt,cid_flt_old,l_newflt,
     +                nobs_flt,iobs_flt,ntot_flt,nrej_flt,ntot_flt_old,
     +                nrej_flt_old,creg_reg,nobs_reg,ntot_reg,nrej_reg,
     +                ntemp_reg,nwind_reg,nwhol_reg,creg_reg_tot,
     +                nobs_reg_tot,nwhol_reg_tot,nrej_reg_tot,
     +                ntemp_reg_tot,nwind_reg_tot,nrej_inv_tot,
     +                nrej_stk_tot,nrej_grc_tot,nrej_pos_tot,
     +                nrej_ord_tot,nrej_sus_tot,lead_t_tot,lead_d_tot,
     +                lead_s_tot,n_xiv_t,n_xiv_d,n_xiv_s,sum_xiv_t,
     +                sum_xiv_d,sum_xiv_s,sumabs_xiv_t,sumabs_xiv_d,
     +                sumabs_xiv_s,l_minus9c,l_last,l_first_date,
     +                l_operational,l_pc,l_ncep,*99)

      go to 34

c-----------------------------------
   99 continue  ! return 1 out of subr. acftobs_qc comes here - keep going but post message
      print 153, maxflt,maxflt
  153 format(/' #####> WARNING: THERE ARE MORE THAN ',I6,' AIRCRAFT ',
     + '"FLIGHTS" IN INPUT FILE -- MUST INCREASE SIZE OF PARAMETER ',
     +'NAME "MAXFLT" - WILL CONTINUE ON PROCESSING ONLY ',I6,' FLTS-0'/)
      write(cmaxflt,'(i6)') maxflt
      call system('[ -n "$jlogfile" ] && $DATA/postmsg'//
     + ' "$jlogfile" "***WARNING:'//cmaxflt//' AIRCRAFT "FLIGHT" '//
     + 'LIMIT EXCEEDED IN PREPOBS_PREPACQC, ONLY '//
     + cmaxflt//' FLIGHTS PROCESSED-0"')
c-----------------------------------

   34 continue

      write(*,'(" After running acftobs_qc, there are ",I0," good ",
     +          "reports, ",I0," bad reports (total rpts = ",I0,")")')
     +        nrpts4QC,krej,nrpts4QC_pre
      write(*,*)
      write(*,*)

c Sort reports (including bad ones) into profiles (sort logic and sort key construction
c  borrowed from acftobs_qc) (note this is done even if l_doprofiles = FALSE because it
c  is used in the final listing of single-level aircraft reports)
c -------------------------------------------------------------------------------------

c Initialize sort key and sort index
c ----------------------------------
      do i=1,max_reps
        csort_wbad(i)   = 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'
        indx_wbad(i) = i 
      enddo

c Form variable to sort / sort key
c --------------------------------
      write(*,'(" Sorting reports and creating sort index, including ",
     +          "reports marked as bad....")')
 
      do iob=1,nrpts4QC_pre
 
        kidt = idt(iob) + 100000
        if(kidt.ge.1000000) then
          write(*,*)
          write(*,*) '** WARNING: kidt too large (=',kidt,')'
          write(*,*)
          write (*,8073) iob,c_insty_ob(itype(iob)),c_acftreg(iob),
     +                   c_acftid(iob),idt(iob),alat(iob),alon(iob),
     +                   pres(iob),ht_ft(iob),t_prcn(iob),ob_t(iob),
     +                   xiv_t(iob),ichk_t(iob),ob_q(iob),xiv_q(iob),
     +                   ichk_q(iob),ob_dir(iob),xiv_d(iob),ichk_d(iob),
     +                   ob_spd(iob),xiv_s(iob),ichk_s(iob),idp(iob)
 8073 format(i5,1x,a8,1x,a8,1x,a9,1x,i7,1x,2f11.5,1x,f8.1,1x,f7.0,1x,
     +       f5.2,4(2(1x,f8.2),1x,i5),1x,i4)
          write(*,*)
          kidt = 999999
        endif
        write(c_idt,'(i6)') kidt
 
        if(ht_ft(iob).eq.amiss) then
          c_ht_ft = '99999'
        else
          iht_ft = nint(ht_ft(iob))
          if(iht_ft.ge.100000) then
            write(*,*)
            write(*,*) '** WARNING: iht_ft too large (=',iht_ft,')'
            write (*,8073) iob,c_insty_ob(itype(iob)),c_acftreg(iob),
     +                     c_acftid(iob),idt(iob),alat(iob),alon(iob),
     +                     pres(iob),ht_ft(iob),t_prcn(iob),ob_t(iob),
     +                     xiv_t(iob),ichk_t(iob),ob_q(iob),xiv_q(iob),
     +                     ichk_q(iob),ob_dir(iob),xiv_d(iob),
     +                     ichk_d(iob),ob_spd(iob),xiv_s(iob),
     +                     ichk_s(iob),idp(iob)
            write(*,*)
            iht_ft = 99999
          endif

c Make descents look like ascents for sorting purposes (complication comes in when a descent
c  has two obs with the same time, but different altitudes)
c
c *** -> Need to make sure to reverse order upon writing to output in output_acqc_prof for
c         descents - profile levels need to be ordered by decreasing pressure (for example,
c         1st lvl = 1010 mb, 2nd lvl = 987 mb, 3rd lvl = 764 mb, etc.)
c -----------------------------------------------------------------------------------------
	  if(c_qc(iob)(11:11).eq.'d' .or. c_qc(iob)(11:11).eq.'D')
     +     iht_ft = 50000 + (-1)*iht_ft

          if(iht_ft.ge.0) then
            write(c_ht_ft,'(i5.5)') iht_ft
          else
            write(c_ht_ft,'(i5.4)') iht_ft
          endif
        endif
 
! vvvv DAK-future change perhaps to account for incr. lat/lon precision
        if(alat(iob).eq.amiss) then
          c_lat = '99999'
        else
          ilat = nint(alat(iob)*100.)
          if(abs(ilat).ge.100000) then
            write(*,*)
            write(*,*) '** WARNING: ilat too large (=',ilat,')'
            write (*,8073) iob,c_insty_ob(itype(iob)),c_acftreg(iob),
     +                     c_acftid(iob),idt(iob),alat(iob),alon(iob),
     +                     pres(iob),ht_ft(iob),t_prcn(iob),ob_t(iob),
     +                     xiv_t(iob),ichk_t(iob),ob_q(iob),xiv_q(iob),
     +                     ichk_q(iob),ob_dir(iob),xiv_d(iob),
     +                     ichk_d(iob),ob_spd(iob),xiv_s(iob),
     +                     ichk_s(iob),idp(iob)
            write(*,*)
            ilat = 99999
          endif
          write(c_lat,'(i5)') ilat
        endif
 
        if(alon(iob).eq.amiss) then
          c_lon = '999999'
        else
          ilon = nint(alon(iob)*100.)
          if(abs(ilon).ge.1000000) then
            write(*,*)
            write(*,*) '** WARNING: ilon too large (=',ilon,')'
            write (*,8073) iob,c_insty_ob(itype(iob)),c_acftreg(iob),
     +                     c_acftid(iob),idt(iob),alat(iob),alon(iob),
     +                     pres(iob),ht_ft(iob),t_prcn(iob),ob_t(iob),
     +                     xiv_t(iob),ichk_t(iob),ob_q(iob),xiv_q(iob),
     +                     ichk_q(iob),ob_dir(iob),xiv_d(iob),
     +                     ichk_d(iob),ob_spd(iob),xiv_s(iob),
     +                     ichk_s(iob),idp(iob)
            write(*,*)
            ilon = 999999
          endif
          write(c_lon,'(i6)') ilon
        endif
! ^^^^ DAK-future change perhaps to account for incr. lat/lon precision
 
        c_type = c_insty_ob(itype(iob))

c NRL sort key: 
c -------------
cc Option 1: not used
cc      csort_wbad(iob) = c_idt(1:6)          ! time
cc   +                  //c_ht_ft(1:5)        ! altitude
cc   +                  //c_lat(1:5)          ! latitude
cc   +                  //c_lon(1:6)          ! longitude
cc   +                  //c_type(1:2)         ! aircraft type

cc Option 2: not used (tail number first)
cc      csort_wbad(iob) = c_acftreg(iob)(1:7) ! tail number
cc   +                  //c_acftid(iob)(1:7)  ! flight number
cc   +                  //c_idt(1:6)          ! time
cc   +                  //c_ht_ft(1:5)        ! altitude
cc   +                  //c_lat(1:5)          ! latitude
cc   +                  //c_type(1:2)         ! aircraft type

cc Option 3: not used (use type first to group AIRCFT and AIRCAR message types together)
cc      csort_wbad(iob) = c_type(1:2)         ! aircraft type
cc   +                  //c_acftreg(iob)(1:7) ! tail number
cc   +                  //c_acftid(iob)(1:7)  ! flight number
cc   +                  //c_idt(1:6)          ! time
cc   +                  //c_ht_ft(1:5)        ! altitude
cc   +                  //c_lat(1:5)          ! latitude
cc   +                  //c_lon(1:6)          ! longitude

c Option 4: not used
c Sort by altitude before time... want descents in order with an increasing vertical
c  coordinate - but if you have two obs in a descent with the same time but different
c  altitude, the altitudes will show up reversed -- use offset to get around this
c -----------------------------------------------------------------------------------
c
c Option 5: USE THIS (sort by time then altitude that is adjusted for descents)
c -----------------------------------------------------------------------------
        if(c_qc(iob)(11:11).eq.'A') then    ! change 'A' to 'a'
          c_qc11 = 'a'
        elseif(c_qc(iob)(11:11).eq.'D') then
          c_qc11 = 'd'                      ! change 'D' to 'd'
        else
          c_qc11 = c_qc(iob)(11:11)
        endif

c Option 6: not used {sort by altitude first, then time... trust vertical coordinate more
c  than position (many less bad marks in c_qc(5:5)'s vs c_qc(2:4))}
c ---------------------------------------------------------------------------------------
        csort_wbad(iob) = c_type(1:2)//c_qc11               ! aircraft type + ascent/descent
     +                               //c_acftreg(iob)(1:8)  ! tail number
     +                               //c_acftid(iob)(1:7)   ! flight number
ccccc+                               //c_ht_ft(1:5)
ccccc+                               //c_idt(1:6)
     +                               //c_idt(1:6)           ! time
     +                               //c_ht_ft(1:5)         ! altitude
! vvvv DAK-future change perhaps to account for incr. lat/lon precision
     +                               //c_lat(1:5)           ! latitude
     +                               //c_lon(1:6)           ! longitude
! ^^^^ DAK-future change perhaps to account for incr. lat/lon precision

      enddo
 
c Sort reports in file according to array csort_wbad
c --------------------------------------------------
      call indexc40(nrpts4QC_pre,csort_wbad,indx_wbad)

      if(l_doprofiles) then ! takes longer to run, because it outputs profiles in separate
                            !  PREPBUFR-like file

c ----------------------------------------------------------------------------------
c Translate NRL QC flags to NCEP events and add events to PREPBUFRlike profiles file
c ----------------------------------------------------------------------------------
        write(*,*) 'Calling output_acqc_prof....'
        write(*,*)

        call output_acqc_prof(proflun,nrpts4QC_pre,max_reps,mxnmev,mxlv,
     +                        bmiss,cdtg_an,alat,alon,ht_ft,idt,c_qc,
     +	                      trad,l_otw,l_nhonly,indx_wbad,c_acftreg,
     +                        c_acftid,ob_t,nevents,hdr,acid,rct,drinfo,
     +                        acft_seq,mstq,cat,
     +                        pob_ev,pqm_ev,ppc_ev,prc_ev,pbg,ppp,
     +                        zob_ev,zqm_ev,zpc_ev,zrc_ev,zbg,zpp,
     +                        tob_ev,tqm_ev,tpc_ev,trc_ev,tbg,tpp,
     +                        qob_ev,qqm_ev,qpc_ev,qrc_ev,qbg,qpp,
     +                        uob_ev,vob_ev,wqm_ev,wpc_ev,wrc_ev,
     +                        wbg,wpp,
     +	                      ddo_ev,ffo_ev,dfq_ev,dfp_ev,dfr_ev,
     +                        nrlacqc_pc,l_allev_pf,l_prof1lvl,
     +                        l_mandlvl,tsplines,
     +                        l_operational,lwr)

        write(*,*)
        write(*,*)
        write(*,*) 'Back from output_acqc_prof ....'
        write(*,'(" PREPBUFR-like (profiles) file has been updated ",
     +            "with events representing the QC marks applied by ",
     +            "the NRLACQC routine acftobs_qc.")')
        write(*,*)
        write(*,*)

c Close output PREPBUFR-like (profiles) file 
c ------------------------------------------
        call closbf(proflun) ! closbf will take care of flushing last message
        print *
        print'(" Closed output PREPBUFR-like file - now holds post-",
     +         "NRLACQC merged aircraft profile data; unit number ",
     +         I0)', proflun
        print *
      endif

c ----------------------------------------------------------------------
c Always output single-level QC'd aircraft data in regular PREPBUFR file
C ----------------------------------------------------------------------

c Re-open input PREPBUFR file (contains mass and wind reports for all data types, no NRLACQC
c  events on reports in AIRCAR and AIRCFT message types)
c ------------------------------------------------------------------------------------------
      call openbf(inlun,'IN',inlun)
      print *
      print'(" Again opened input PREPBUFR file with all data, ",
     +       "including pre-NRLACQC aircraft data; unit number ",I0)',
     +     inlun
      print *


C Initialize some variables that will be set in output_acqc_noprof and used in printout
c -------------------------------------------------------------------------------------
      ncep_qm_p = 9999
      ncep_rc_p = 9999
      ncep_qm_z = 9999
      ncep_rc_z = 9999
      ncep_qm_t = 9999
      ncep_rc_t = 9999
      ncep_qm_q = 9999
      ncep_rc_q = 9999
      ncep_qm_w = 9999
      ncep_rc_w = 9999
      ncep_rej  =    0

c Translate NRL QC flags to NCEP events and add events to aircraft reports in "AIRCAR" and
c  "AIRCFT" message types in full PREPBUFR file (split mass and wind pieces)
c ----------------------------------------------------------------------------------------
      call output_acqc_noprof(inlun,outlun,nrpts4QC_pre,max_reps,bmiss,
     +                        alat,alon,ht_ft,idt,c_qc,trad,l_otw,
     +                        l_nhonly,l_qmwrite,
     +                        ncep_qm_p,ncep_rc_p,
     +                        ncep_qm_z,ncep_rc_z,
     +                        ncep_qm_t,ncep_rc_t,
     +                        ncep_qm_q,ncep_rc_q,
     +                        ncep_qm_w,ncep_rc_w,
     +                        ncep_rej,nrlacqc_pc)

      write(*,*)
      write(*,*)
      write(*,*) 'Back from output_acqc_noprof ....'
      write(*,'(" PREPBUFR file has been updated with events ",
     +          "representing the QC marks applied by the NRL aircraft",
     +          " QC routine acftobs_qc")')
      write(*,*)
      write(*,*)

c Close input PREPBUFR file
c -------------------------
      call closbf(inlun)
      print *
      print'(" Closed input PREPBUFR file with all data, including ",
     +       "pre-NRLACQC aircraft data; unit number ",I0)', inlun
      print *

c Close output PREPBUFR file 
c --------------------------
      call closbf(outlun) ! closbf will take care of flushing last message
      print *
      print'(" Closed output PREPBUFR file - now holds all data, ",
     +       "including post-NRLACQC aircraft data; unit number ",I0)',
     +     outlun
      print *

      if(.not.l_operational) then

c Write merged reports and resulting NRL QC decisions (array c_qc) to an output file for
c  later perusal
c --------------------------------------------------------------------------------------

        open(51,file='merged.reports.post_acftobs_qc.sorted',form=
     +       'formatted')
        write(51,*)
        write(51,'(" Final listing of all aircraft reports in PREPBUFR",
     +             " file after NRL QC (sorted according to array ",
     +             "csort_wbad)")')
        if(nrpts4QC_pre.eq.max_reps) write(51,'(" (since max report ",
     +   "limit hit, only reports going through QC listed here)")')
        write(51,'(" -------------------------------------------------",
     +             "--------------------------------------------------",
     +             "-------")')
        write(51,*)
        write(51,'(" TAMDAR reports here replace characters 1-3 of ",
     +             "manufactured flight # (''000'') with (''TAM'') in ",
     +             "order to create truncated tail # ''TAM'' for ",
     +             "NRLACQC sorting - the PREPBUFR file continues to ",
     +             "encode ''000'' in")')
        write(51,'("  characters 1-3 of manufactured flight # for ",
     +             "TAMDAR (stored as both ''SID'' and ''ACID'')")')

        write(51,*)
        write(51,'(" AIREP and PIREP reports report only a flight # ",
     +             "(manufactured for PIREPs) - a tail # for NRLACQC ",
     +             "sorting is created by truncating the flight # - ",
     +             "the PREPBUFR file will not encode these truncated ",
     +             "tail #''s")')

        write(51,*)
        write(51,'(" All AMDAR reports except LATAM report only a tail",
     +            " # - this is stored as both flight # and tail # for",
     +             " NRLACQC sorting - the PREPBUFR file continues to ",
     +             "encode only tail # (stored in ''SID'')")')
        write(51,*)
        write(51,'(" AMDAR reports from LATAM report both a tail # and",
     +             " a flight # - these are used as reported for ",
     +             "NRLACQC sorting - the PREPBUFR file continues to ",
     +             "encode both tail # and flight # (as ''SID'' and ",
     +             "''ACID'',")')
        write(51,*) 'resp.)'
        write(51,*)
        write(51,'(" MDCRS reports from ARINC report both a tail # and",
     +             " a flight # - these are used as reported for ",
     +             "NRLACQC sorting - the PREPBUFR file continues to ",
     +             "encode both tail # and flight # (as ''SID'' and ",
     +             "''ACID'',")')
        write(51,*) 'resp.)'

        write(51,*)
        write(51,3001)
 3001   format(173x,'! _PREPBUFR_QMs_!NRLACQC_REASON_CODE'/' index ',
     +         'flight    tail num itp ph      lat       lon    ',
     +         'time  hght   pres  temp/chk spec_h/chk  wspd/chk ',
     +         'wdir/chk t-prec !__qc_flag__!_______________',
     +         'csort_wbad_______________! Pq Zq Tq Qq Wq!Prc Zrc Trc ',
     +         'Qrc Wrc'/'------ --------- -------- --- --  ',
     +         '-------- --------- ------ ----- ------ --------- ',
     +         '---------- --------- -------- ------ !-----------!',
     +         '----------------------------------------! -- -- -- ',
     +         '-- --!--- --- --- --- ---')

        do i=1,nrpts4QC_pre
          j=indx_wbad(i)

          if(ncep_rej(j).eq.0) then
            write(51,fmt=8001) j,c_acftid(j),c_acftreg(j),itype(j),
     +       phase(j),alat(j),alon(j),idt(j),nint(ht_ft(j)),pres(j),
     +       ob_t(j),ichk_t(j),ob_q(j),ichk_q(j),ob_spd(j),ichk_s(j),
     +       nint(ob_dir(j)),ichk_d(j),t_prcn(j),c_qc(j),csort_wbad(j),
     +       ncep_qm_p(j),ncep_qm_z(j),ncep_qm_t(j),ncep_qm_q(j),
     +       ncep_qm_w(j),ncep_rc_p(j),ncep_rc_z(j),ncep_rc_t(j),
     +       ncep_rc_q(j),ncep_rc_w(j)
c           if(ncep_rc_p(j).ge.1000) write(51,fmt=9001) ncep_rc_p(j)
c9001       format(' PRC too large = ',i10)
c           if(ncep_rc_z(j).ge.1000) write(51,fmt=9002) ncep_rc_z(j)
c9002       format(' ZRC too large = ',i10)
c           if(ncep_rc_t(j).ge.1000) write(51,fmt=9003) ncep_rc_t(j)
c9003       format(' TRC too large = ',i10)
c           if(ncep_rc_q(j).ge.1000) write(51,fmt=9004) ncep_rc_q(j)
c9004       format(' QRC too large = ',i10)
c           if(ncep_rc_w(j).ge.1000) write(51,fmt=9005) ncep_rc_w(j)
c9005       format(' WRC too large = ',i10)
          endif
	enddo

 8001 format(i6,1x,a9,1x,a8,i4,1x,i2,2f10.5,1x,i6,1x,i5,1x,f6.1,1x,
     +       f6.2,i3,1x,f7.2,i3,1x,f6.1,i3,2x,i4,i3,1x,f6.2,1x,
     +       '!',a11,'!',a40,'!',5(1x,i2.2),'!',i3.3,4(1x,i3.3))

c Close data listing file
c -----------------------
	close(51)

      endif

c End program
c -----------

      write(*,*)
      write(*,*) '**************************'
      write(*,*) 'PREPOBS_PREPACQC has ended'
      call system('date')
      write(*,*) '**************************'
      write(*,*)
      call w3tage('PREPOBS_PREPACQC')

      else  ! nrpts4QC_pre.le.0

c Input PREPBUFR file contains NO aircraft data of any kind -- STOP 4
c -------------------------------------------------------------------

         WRITE(6,108)
  108 FORMAT(/' INPUT PREPBUFR FILE CONTAINS NO "AIRCAR" OR "AIRCFT" ',
     $ 'MESSAGES WITH REPORTS - STOP 4'/)
         CALL ERREXIT(4)

      endif ! nrpts4QC_pre.gt.0

      stop

cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
c add this event of future switch to dynamic memory allocation

ca901 continue

calloc  print *, '#####PREPOBS_PREPACQC - UNABLE TO ALLOCATE ARRAYS'
calloc  call w3tage('PREPOBS_PREPACQC')
calloc  call errexit(99)
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      end
