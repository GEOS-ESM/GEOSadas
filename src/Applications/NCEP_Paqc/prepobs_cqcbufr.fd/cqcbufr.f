C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: PREPOBS_CQCBUFR
C   PRGMMR: MELCHIOR          ORG: NP22        DATE: 2020-01-09
C
C ABSTRACT: Perform complex quality control of rawinsonde heights
C   and temperatures.  Errors are detected and many corrected.
C   Checks used: Hydrostatic, increment, horizontal statistical,
C   vertical statistical, (temporal,) baseline, and lapse rate.
C   Also calculates intersonde (radiation) bias corrections.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  This code owes its heritage to CQCBUFR.
C     However, this version is completely rewritten.  The qc for
C     mandatory and significant levels is unified.
C 1997-05-12  W. Collins  Corrections to subroutines called by
C     AUXLEVS to make proper changes to auxiliary levels when data
C     levels are corrected.  Also, add switch so that virtual
C     temperature calculation can be bypassed.
C 1997-05-19  W. Collins  Further corrections to subroutines
C     called by AUXLEVS.
C 1997-06-04  W. Collins  Prevent division by 0. when surface
C     pressure is changed to mand lvl value.
C 1997-06-09  W. Collins  Treat tropopause like significant temperature
C     levels for DMA logic.  Slightly modify input to make sure that
C     forecast variables are always picked up.
C 1997-07-02  W. Collins  Allow larger temperature residuals at
C     tropopause.  Correct error so that category 4 heights will be
C     recalculated when necessary.
C 1997-07-23  W. Collins  Correct error in 97-07-02 changes.
C 1997-08-18  D. Keyser   LAPSE more carefully checks for equal
C     spanning pressure (to avoid divide by zero possibility)
C 1997-08-27  W. Collins  VTPEVN error in virtual temperature
C     calculation of dropsondes corrected.
C 1997-09-25  W. Collins  Add switch to choose between using SQN to
C     match parts of report or not. USESQN needs to be .FALSE. for
C     Reanalysis
C 1997-10-16  W. Collins  Turn off type 106 baseline corrections.
C 1997-12-09  W. Collins  Add switch DOHOR to be able to turn off
C     the horizontal check, thus saving time.  VTPEVN sets the reason
C     code for moisture variables to 1.  Corrected error in ZDIF; now
C     correctly finds ZM.  Recalculate Tv when T is corrected.
C 1998-01-07  W. Collins  Change RADEVN so that levels below ground
C     will never get a radiation correction.
C 1998-01-15  W. Collins  Limit computation error corrections to
C     1.5 times limit for single level ht. corrs.
C 1998-01-27  W. Collins  Temporal check is included for use by
C     Reanalysis.
C 1998-02-18  W. Collins  Resolve few remaining inconsistencies in
C     use of TDO (dew-point depression)
C 1998-02-23  W. Collins  Correct T130 to properly handle 1 missing
C     level.  Eliminate warnings when compiled with standard Fortran
C     90.
C 1998-06-24  D. A. Keyser -- Radiosonde Types 37, 52, 61-63 (all
C     Vaisala RS80) are no longer corrected (both in and out of U.S.),
C     Radiosonde Type 60 (Vasiala RS80/Microcora) is still corrected
C     as before (both in and out of U.S.)
C 1998-08-14  W. Collins  Make additional changes for Fortran 90
C     compatibility.  Modifications made for Y2K compliance.
C 1998-10-01  W. Collins  Correct date format in PEVENTS.
C 1998-11-06  W. Collins  Add diagnosis of profiles in which the
C     temperature sensor gets bad and remains so, resulting in large
C     and persistent height and/or temperature residuals.  Error types
C     41 and 42 are used.
C 1998-12-03  W. Collins  Correct error which prevented RADEVN from
C     being applied to all mandatory levels.  Also correct constant
C     which solves for height adjustment due to "radiation" correction.
C 1998-12-09  W. Collins  Change to make diagnosis of type 41-43
C     errors only for WMO blocks 42,43. Add WRT23 to Namelist. Default
C     is .FALSE.  If .TRUE., then statistics file is updated for type
C     41-43 tests. Skip POBERR if error in reading from statistics
C     file.
C 1999-02-01  R. Kistler  Make changes so that code may be run either on
C     Crays or IBM SP.  Most changes involve specifying the second
C     argument to UFBINT to be REAL(8).
C 1999-02-10  W. Collins  Guarantee that ERR710 is called only when all
C     the needed data are non-missing and not = 0.
C 1999-04-27  W. Collins  New observation error diagnosis--
C     uses combination of Bayesian logic, modeled error structure and
C     fuzzy logic to make decisions. All residuals are normalized by
C     sample statistics for each particular run.  A new output is added
C     to help see why observation error diagnoses are made. Note that
C     moisture (q) is quality controlled by this program for the first
C     time.
C 1999-11-16  W. Collins  Correct errors in treatment of moisture
C     quality marks to honor previous marks.
C 1999-12-09  W. Collins  Change VTPEVN so that Tv (virtual
C     temperature) is calculated, even when the moisture quality is >
C     3, except when the moisture exceeds 'Earth' limits.
C 1999-12-14 W. Collins   Include writing the baseline residual in file
C     NFSTN, so that station-by-station statistics can be calculated.
C 2000-01-01 W. Collins   Set switches to use new 'radiation correction'
C     tables (RADT4) as of this date.  These were generated from 1-yr
C     statistics, and attempt to make sondes look like the average
C     Vaisala RS-80 sonde.
C 2000-03-09 W. Collins   Add fort.52--print of residuals and errors for
C     use by SDMedit.
C 2000-03-16 W. Collins   Modify change of 1999-12-14 as follows:  The
C     original change calculated virtual temperature in all cases, but
C     gave quality mark of 13. when the moisture flag was > 3.  This
C     change allows good Tv flags for all temperatures at levels above
C     700mb, regardless of the moisture quality mark.
C 2000-06-13 W. Collins   Slight change to HSC so that HYDN fills HYDS
C     when it is unreasonably large or missing.
C 2000-09-08 W. Collins   New bias corrections for temperature (RADT5),
C     based on all radiosonde data for 1999.  Temperatures are adjusted
C     to mean Vaisala RS-80 (sonde type 61) increments.
C 2001-02-28 W. Collins   Correct error of setting dew-point temperature
C     to missing in FULVAL. Never flag T (Tv) bad as the result of
C     flagging q bad. Make more efficient so that auxiliary level
C     computations are ommitted when not needed.
C 2001-03-02 W. Collins   Small, misc. code errors corrected in
C     collaboration with Meta Sienkiewicz (NASA). May improve diagnosis
C     for surface pressure errors.
C 2001-03-15 W. Collins   Correction to T240 so that if type 106--
C     surface pressure observation error--is allowed to be performed,
C     the corrections form a consistent set and the new residuals near
C     the surface all become small.  Slight changes to diagnosis of
C     types.  Add collection of surface pressure increment and height
C     increment at the surface to fort.16.
C 2001-03-23 W. Collins   Problem in HOLES corrected and then call to
C     HOLES (temporarily?) removed.
C 2001-04-20 W. Collins   Correction to FUZZY to work properly when
C     no guess is available. This allows possible diagnosis of
C     observation errors, even with a missing guess, using other
C     checks. It also prevents flagging all data.  Set surface
C     pressure, even if guess is msg.
C 2001-04-27 D. Keyser    Streamlined VTPEVN. Turned q event processing
C     back on (even when DOVTMP=F). When q bad but passes sanity check,
C     Tv now calculated in all cases but given a suspect q.m. if below
C     700 mb and original TQM not 0 or > 3 (this was original intention
C     of 1999-12-09 change which actually skipped Tv processing in all
C     cases when q was bad)
C 2001-05-11  W. Collins  Convert guess temperature from Tv to T,
C     so that the residuals are comparing comparable quantities.
C 2002-07-22  D. Keyser   If balloon drift coordinates (lat/lon/
C     d-tim) are available, will use these to more precisely obtain
C     estimated sun angle for intersonde bias corrections (RADCOR)
C     (done in new subroutine SOELAN); more info re "RADCOR" now
C     written to unit 68
C 2004-03-17  D. Keyser   Corrected bug discovered by Christopher
C     Redder (NASA/GSFC) where the order of ballon drift coord. (YDR,
C     XDR) was reversed in the argument list for the call to
C     subroutine GETOB (this results in incorrect sun angle calc.
C     and corrections in the RADCOR step); updated subroutine RADT3
C     docblock and added new radiosonde types since the last time it
C     was updated (non of the new types receive corrections)
C 2004-04-21  D. Keyser   For cases when DOVTMP=T, no longer calculates
C     Tv on pressure levels above the tropopause because reported Tdd
C     values at high levels are often inaccurate and unrealistic 0.4 to
C     1.0 increases in Tv vs. T can occur (esp. above 10 mb and at
C     U.S. sites) (q is still updated on these levels, however), if the
C     reported tropopause level is below 500 mb it is not considered
C     to be the tropopause for this test, also if no valid tropopause
C     level is found by the time 80 mb is reached, all levels above
C     this are considered to be above the tropopause for this test
C     {thanks to Christopher Redder (NASA/GSFC) for discovering this
C     problem}
C 2005-08-03  D. Keyser   Revised RADCOR correction table in subr.
C     RADT5 for China's Shanghai Radio sonde (BUFR type 32), after
C     12/31/00 no corrections are made between 700 and 50 mb, inclusive
C     because this sonde is being corrected on site (this will remove
C     cold bias found in these sondes vs. guess below 50 mb) (Note 1:
C     No corrections are made below 700 mb for any sondes, Note 2:
C     Prior to 1/1/01, the previous correction table is still used for
C     this sonde because, according to Yuetang Yhang of the Chinese
C     NMC, this is when the on-site corrections started); added new
C     radiosonde types in subr. RADT5 docblock since the last time it
C     was updated
C 2007-01-19  M. Sienkiewicz  Added integer missing value MMISS  to
C     use in place of the standard BUFR missing value BMISS.  The
C     BMISS value 10e10 is too large to represent in integer*4 and
C     yields different values on different architectures.  If we use
C     INTVAL = BMISS, checks such as INTVAL.NE.BMISS or INTVAL.LT.BMISS
C     are not reliable.
C 2007-06-15  Sienkiewicz - added switch RADCOR to allow turning off
C     radiation correction.  For MERRA we plan to use a different
C     external program to do the radiosonde corrections.
C 2007-09-14  D. Keyser -- In subr. VTPEVN, Q.M. 3 is now assigned to
C     calculated virt. temps only if the moisture Q.M. is truly bad
C     (i.e. > 3 but not 9 or 15) (and, as before, orig. TQM is 1 or 2
C     and POB is below 700 mb) - before, TQM set to 3 when QQM was 9 or
C     15 and all other conditions met.  In RADCOR subroutines, minor
C     code corrections suggested by Pat Pauley (corrects problems that
C     could occur on some remote machines).  Corrected array overflow
C     (and memory clobbering) issue when the number of RAOB IDs stored
C     by radiosonde type in array SIDRAD (in RADCOR processing) is
C     listed in subr. WTSTATS (which summarizes RADCOR stats).  If this
C     limit is exceeded, only the limiting number of RAOBS will now be
C     listed, along with a warning diagnostic - the limit has been
C     increased from 400 to 800 since some reanalysis runs were
C     exceeding the old limit.  Increased maximum number of radiosonde
C     stations that can be processed from 899 to 999 (one reanlysis run
C     hit this limit).  If this limit is exceeded, hard abort will now
C     occur rather than zero r.c. with possible memory clobbering.
C 2008-08-26  Sienkiewicz - merged D. Keyser changes with local modifications
C 2008-04-10  D. A. KEYSER -- Can handle radiosonde types > 99 which
C     will soon be introduced into the BUFR database (based on November
C     2007 WMO BUFR update)
C 2008-10-08  D. A. KEYSER -- All RADCOR changes: Now processes
C     radiosonde types > 111 and < 255 rather than setting them to
C     missing (255), even though there are currently no valid types >
C     111; modified to re-index array "SOLAR" (sun angle on mand. lvls)
C     in oldest set of corrections (subr. RADT1) so that the correct
C     values returned from subr. SOELAN are used for cases where
C     balloon drift information is present (only applies to reanalysis
C     since balloon drift was not available back when subr. RADT1 was
C     called in real-time runs, subr. RADT1 is never called in current
C     real-time runs anyway); modified to re-index array "SOLAR" in
C     subr. SOELAN for cases involving the oldest set of corrections
C     and where balloon drift information was not available so that
C     proper values are returned to calling subr. RADT1 (only applies
C     to reanalysis since subr. RADT1 is never called in current real-
C     time runs); updated list of radiosonde types in newest correction
C     subr. (RADT5) to conform with latest (Nov. 2007) WMO BUFR code
C     table 0-02-011
C 2008-10-08  D. A. KEYSER/J. WOOLLEN -- Subr. T240 modified to correct
C     indexing error which can lead, on rare occasions, to out-of-
C     bounds arrays and to the incorrect indexing of some variables;
C     corrected if tests where integer values were tested for not being
C     equal to real value for missing (BMISS = 10E10), these integer
C     values can never be equal to BMISS for I*4 due to overflow -
C     instead they are now tested for not being equal to new integer
C     value for missing (IMISS, also = 10E10), although this is also an
C     overflow value for I*4, it results in a correct evaluation
C 2008-11-19  D. A. KEYSER -- All RADCOR changes: Updated newest
C     correction subr. (RADT5) to recognize radiosonde types conforming
C     with latest (Nov. 2008) WMO BUFR code table 0-02-011 and to
C     produce more specific diagnostic print related to radiosonde
C     types encountered (i.e., invalid, obsolete, vacant, etc.)
C 2012-11-20  J. WOOLLEN  INITIAL PORT TO WCOSS
C 2013-02-05  D. Keyser   Final changes to run on WCOSS: Set BUFRLIB
C     missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C     overflow (also done for IMISS); rename all REAL(8) variables as
C     *_8; use formatted print statements where previously unformatted
C     print was > 80 characters.
C 2013-09-19  Sienkiewicz - merged WCOSS version with local modifications
C 2016-05-18  D. Keyser   Corrected an integer overflow issue in subroutine
C     INPUT2 which prevented the temporal check from running in the CDAS
C     network (the only network where it currently would run).
C 2017-05-16  Sienkiewicz - adjust maximum BUFR record size (call MAXOUT)
C     to avoid losing soundings that just barely exceed max after CQC
C     changes are added to the record.
C 2020-08-09  S. Melchior - In subroutine TMPCHK, explicitly defined ICK
C     as an integer. Moved ICK.NE.0 logic inside ITI.NE.0 logic.
C     BENEFIT: corrects problems when compiled and run with full DEBUG
C              options enabled.
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT (DATA CARDS)
C    (UNIT 12  - events file for mandatory levels)
C     UNIT 14  - PREPBUFR file (BUFR), data input
C    (UNIT 15  - block totals file)
C    (UNIT 16  - collection of stations received)
C    (UNIT 17  - t-24, used only for temporal check)
C    (UNIT 18  - t-12, used only for temporal check)
C    (UNIT 19  - t+12, used only for temporal check)
C    (UNIT 20  - t+24, used only for temporal check)
C     UNIT 23  - contains statistics for observation error checking
C
C   OUTPUT FILES:
C     UNIT 06  - printout
C    (UNIT 12  - events file for mandatory levels)
C    (UNIT 15  - block totals file)
C    (UNIT 16  - collection of stations received)
C    (UNIT 22  - list of potential wind problems
C     UNIT 23  - contains statistics for observation error checking
C     UNIT 51  - output file of data (PREPBUFR) after correction
C     UNIT 52  - output print file of values, residuals and events
C    (UNIT 55  - ASCII file of all data necessary to run
C                single station version of this code)
C    (UNIT 60  - print file, details of decisions)
C    (UNIT 61  - print file: list of all holes and their extent)
C    (UNIT 68  - intersonde correction information file)
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - ACCUM    BEST     CHANGE  CHECKIT COMPER   DISTR
C                  DMA      ERR123   ERR710  ISGOOD  LEVTYPS  OBERR
C                  PEVENTS  PRNTOUT  PRSTNS  RBLOCKS RESIDS   ROUND
C                  INCR     HOLES    HSC     ERRTYP  SIGERR   SEVENT
C                  SHELL    SORT     ISORT   SIMPLE  STAT     STEVNTS
C                  STNCNT   STYPE    T120    T121    T130     T140
C                  EVPROC   ERR5     GETPS   CKPS    T220     T240
C                  VOI      LAPSE    WBLOCKS ZDIF    INIT     SETTMP
C                  XHORRES  AUXLEVS  WTSTATS WTISO   GETLEV   GETINC
C                  INPUT    DHOR     FILL    GETOB   HORRES   MASEVN
C                  MIMAP    OUTPUT   CHDIST  COORS   DRCTSL   INCRW
C                  ISOLAT   QCOI     SEARCH  VSOLVE  RSTATS   EVENT
C                  VTPEVN   PREP     RADEVN  RADT1   RADT2    RADT3
C                  RADT4    RADT5    TAB     INPUT2  SOELAN   TMPCHK
C                  FILALL   FULVAL   WINDATZ EVENTW  NOBERR   POBERR
C                  FUZZY    AREA     CHKTMP  PATZ    LSTATS   PILNLNP
C                  ALP      ANLWT    CUMPROB LMANLV  MANLEV   NMANLV
C     LIBRARY:
C       W3NCO    - W3FS13   W3FS21   W3TAGB  W3TAGE  ERREXIT  W3MOVDAT
C       W3EMC    - W3FC05
C       BUFRLIB  - CLOSBF   OPENBF   READMG   UFBQCD  CLOSMG  COPYMG
C                  OPENMB   READSB   UFBCPY   UFBINT  UFBEVN  WRITSB
C                  DATELEN  SETBMISS GETBMISS MAXOUT
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C     CODE >   0 - UNSUCCESSFUL RUN
C
C
C REMARKS: NONE
C     Note! The temporal check is not run operationally, but is
C     used for the Reanalysis and in the CDAS.
C     For operational use, units 12, 15, and 16 will be empty on
C     input, with information written to them for summary purposes.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      PROGRAM PREPOBS_CQCBUFR

      REAL(8)          BMISS,GETBMISS

      CHARACTER*8      STN
      CHARACTER*10     EDATE
      LOGICAL          START, ENDIN, ANY, SKIP, WIND, SINGLE, OBS,
     &                 ANYS, SAME, PRNT, RADCOR
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /EVENTS/  STN(2000),    SEQN(2000),  ISCAN(2000),
     &                 LEVL(2000),   PRES(2000),  LTYP(2000),
     &                 IVAR(2000),   COR(2000),   CORVAL(2000),
     &                 LSTYP(2000),  PCOR(2000),  IEVENT,
     &                 BASR(2000),   PISR(2000),  PSINCR(2000),
     &                 QMARK(2000),  IETYP(2000), EDATE(2000)
      COMMON /TESTS/   TEST
      COMMON /DOTYP40/ DOT40
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST, DOVTMP, USESQN, DOHOR, DOTMP, POUT,
     &                 DOT40, WRT23
      NAMELIST /NAMLST/ TEST, DOVTMP, USESQN, DOHOR, DOTMP, DOT40,
     &                  WRT23,RADCOR

      CALL W3TAGB('PREPOBS_CQCBUFR',2020,0009,0067,'NP22')

      TEST   = .TRUE.          ! Set .T. for tests to give more print
                        !!!  #### BE CAREFUL ##### in subr. POBERR,
                        !!!  TEST=T will actually encode additional
                        !!!  events in the output PREPBUFR file !!!
      DOVTMP = .TRUE.          ! Set .F. for RAP (and before that RUC)
      USESQN = .TRUE.          ! Set .F. for Reanalysis or old PREPBUFRs
      DOHOR  = .TRUE.          ! Set .F. for skipping horizontal check
      DOTMP  = .FALSE.         ! Set .T. for temporal check
      DOT40  = .TRUE.          ! Set .F. to exclude type40 check
      WRT23  = .FALSE.         ! Set .T. to update statistics file
      RADCOR = .TRUE.          ! Set .T. to run radiation correction

      SINGLE = .FALSE.
      IF(.NOT.SINGLE) READ(5,NAMLST)
      WRITE(6,700) TEST, DOVTMP, USESQN, DOHOR, DOTMP, DOT40, WRT23
  700 FORMAT(/' WELCOME TO PREPOBS_CQCBUFR, LAST UPDATED 2020-01-09'/
     & '     SWITCHES: TEST =',L2,'  DOVTMP =',L2,'  USESQN =',L2,
     & '  DOHOR =',L2,'  DOTMP =',L2,'  DOT40 =',L2,'  WRT23 =',L2/)

C  On WCOSS should always set BUFRLIB missing (BMISS) to 10E8 to avoid
C   overflow when either an INTEGER*4 variable is set to BMISS or a
C   REAL*8 (or REAL*4) variable that is missing is NINT'd
C  -------------------------------------------------------------------
ccccc CALL SETBMISS(10E10_8)
      CALL SETBMISS(10E8_8)
      BMISS=GETBMISS()
      XMISS=BMISS
      IMISS=10E8
      print *
      print *, 'BUFRLIB value for missing is: ',bmiss
      print *

      CALL ACCUM
      START  = .TRUE.          ! Input opens data file when START=.T.
      ENDIN  = .FALSE.         ! Initialize
      SKIP   = .FALSE.         ! Initialize
      SAME   = .FALSE.         ! Initialize
      WIND   = .FALSE.         ! Initialize

      IF(.NOT.DOHOR) THEN
        CALL INIT
        GOTO 300
      ENDIF

      IF(SINGLE) THEN
        CALL SETTMP
        GOTO 300
      ENDIF

C  TOP OF LOOP TO READ DATA FOR HORIZONTAL CHECK
C  ---------------------------------------------

  100 CONTINUE
      ITIME = 1
      WRITE(60,520) START,ENDIN,SKIP,SAME,WIND,ITIME,USESQN
  520 FORMAT(' MAIN--START,ENDIN,SKIP,SAME,WIND,ITIME,USESQN: ',
     &  5(L2,2X),I5,2X,L2)
      CALL INPUT(START,ENDIN,SKIP,SAME,WIND,ITIME,USESQN)
                                 ! Read data for 1 station
      IF(ENDIN) GOTO 200
      IF(SKIP .OR. WIND) GOTO 100
      CALL RESIDS(ITIME)         ! Calculate all but horizontal resids
      CALL ISGOOD                ! Preliminary quality assessment
      GOTO 100

  200 CONTINUE

      IF(DOTMP) THEN
        CALL INPUT2              ! Read data for temporal check
        CALL TMPCHK              ! Perform temporal check
      ELSE
        CALL SETTMP
      ENDIF
      CALL HORRES                ! Calculate horizontal residuals
      CALL STAT(ITIME)           ! Calculate & print statistics
      CALL XHORRES               ! Calc normalized horizontal residuals

      START = .TRUE.

C  TOP OF LOOP FOR ALL OTHER CHECKS AND DMA
C  ----------------------------------------

  300 CONTINUE
      ITIME = 2
      ISC = 0
      LAST = 0
      ANY = .TRUE.
      ANYS = .FALSE.
      IF(.NOT.SINGLE) THEN
        CALL INPUT(START,ENDIN,SKIP,SAME,WIND,ITIME,USESQN)
                                 ! Read data a 2nd time, 1 rpt at a time
      ELSE
        CALL RBLOCKS(SKIP,WIND,ENDIN)
      ENDIF
      IF(ENDIN) GOTO 600
      IF(SKIP) GOTO 500
      IF(WIND) THEN
        CALL INCRW
        CALL PRSTNS(WIND)
        GOTO 500
      ENDIF
      CALL CKPS
      CALL RESIDS(ITIME)
      CALL PRSTNS(WIND)
      ICALL = 3
      CALL PRNTOUT(SEQQ,ICALL)

C  LOOP THROUGH LEVELS UNTIL TOP IS REACHED
C  ----------------------------------------

  400 CONTINUE
      ISC = ISC + 1
      CALL DMA(ANY,OBS)          ! Decision Making Algorithm
      CALL CHANGE(ANY,SINGLE,SEQQ) ! Apply corrs to the data
      IF(ANY) ANYS = .TRUE.
      CALL RESIDS(ITIME)
      IF(ANY) CALL DHOR          ! Calculate changes to hor resids
      IF((ANY .OR. .NOT.OBS .OR. LAST.LT.NLEV) .AND. ISC.LT.30) GOTO 400
      CALL STEVNTS               ! Print BUFR events for single stn
      ICALL = 1
      IF(ANYS .AND. OBS) CALL PRNTOUT(SEQQ,ICALL)
      CALL MASEVN                ! Write CQC mass events
  500 CONTINUE
      CALL AUXLEVS(SKIP,SAME,WIND,SINGLE,ANYS)
      IF(.NOT.WIND) THEN
        IF (RADCOR) CALL RADEVN  ! Write radiation correction events
        CALL VTPEVN(DOVTMP)      ! Write virtual temperature events
      ENDIF
      CALL OUTPUT(ENDIN,SINGLE)
      GOTO 300                   ! Go back for the next report

C  END OF DATA REACHED
C  -------------------

  600 CONTINUE
      CALL STAT(ITIME)
      CALL PEVENTS               ! Print events
      IF(.NOT.SINGLE) CALL STNCNT  ! Print counts by WMO block
      I2 = IEVENT
      ICALL = 2
      CALL EVPROC(1,I2,ICALL)    ! Generate, print BUFR events
      CALL WTSTATS               ! Write statistics for RADCOR
      CALL WTISO
      CALL CLOSBF(NFIN)
      CALL CLOSBF(NFOUT)

      CALL W3TAGE('PREPOBS_CQCBUFR')

      STOP
      END
C**********************************************************************

      BLOCK DATA

      COMMON /LEV926/ ISET,MANLIN(1001)

      COMMON /ALLOW/   ALLZ(41), ALLT(71), NALLZ, NALLT
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /CONSTS/  R, G, T0, CP, RV
      COMMON /LEVEL/   PMAND(21)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      LOGICAL PRNT
      DATA XMI /0./, XMA /360./, YMI /-90.001/, YMA /90./
      DATA MAND /21/, NFIN /14/, NFOUT /51/
      DATA NFEVN /12/, NFSUM /15/, NFSTN /16/, NFSGL /55/
      DATA NALLZ /41/, NALLT /51/
      DATA R /287.05/, G /9.80665/, T0 /273.15/
      DATA CP /1004.5/, RV /461.5/
      DATA ZCLIM /40./, TCLIM /5.0/, TT /7.0/, NHLIM /7/
      DATA ALLZ /0.,-1.,1.,-2.,2.,-3.,3.,-4.,4.,-5.,5.,
     &    -6.,6.,-7.,7.,-8.,8.,-9.,9.,-10.,10.,-11.,
     &    11.,-12.,12.,-13.,13.,-14.,14.,-15.,15.,-16.,
     &    16.,-17.,17.,-18.,18.,-19.,19.,-20.,20./
      DATA ALLT /0.,-1.,1.,-2.,2.,-3.,3.,-4.,4.,-5.,5.,
     &    -6.,6.,-7.,7.,-8.,8.,-9.,9.,-10.,10.,-11.,
     &    11.,-12.,12.,-13.,13.,-14.,14.,-15.,15.,
     &    -16.,16.,-17.,17.,-18.,18.,-19.,19.,-20.,
     &    20.,-21.,21.,-22.,22.,-23.,23.,-24.,24.,
     &    -25.,25.,-26.,26.,-27.,27.,-28.,28.,-29.,29.,
     &    -30.,30.,-31.,31.,-32.,32.,-33.,33.,-34.,34.,
     &    -35.,35./

C  ALL LIMITS REPRESENT 10. STANDARD DEVIATIONS.
C  --------------------------------------------

	  DATA ISET /0/
      DATA HSCRES /33.,38.,31.,65.,49.,62.,51.,74.,88.,113.,
     &  120.,101.,143.,120.,143.,6*143./
      DATA XINC /186.,144.,129.,129.,161.,194.,241.,
     &  271.,310.,390.,433.,447.,456.,504.,571.,627.,
     &  686.,743.,800.,857.,914.,
     &  27.,24.,19.,16.,16.,17.,19.,21.,24.,24.,24.,24.,
     &  24.,24.,24.,24.,24.,24.,24.,24.,24.,
     &  135.,120.,95.,80.,80.,85.,95.,105.,120.,120.,120.,120.,
     &  120.,120.,120.,120.,120.,120.,120.,120.,120.,
     &  .016,.017,.014,.011,.0043,.0025,.0016,.0008,.0005,.0002,
     &  11*.0002/
      DATA HOIRES /149.,100.,104.,110.,149.,176.,219.,240.,261.,
     &  314.,353.,377.,399.,456.,541.,579.,
     &  614.,650.,686.,721.,757.,
     &  24.,21.,19.,14.,16.,17.,17.,17.,17.,20.,21.,24.,24.,
     &  24.,24.,24.,24.,24.,24.,24.,24.,
     &  120.,105.,95.,70.,80.,85.,85.,85.,85.,100.,105.,120.,120.,
     &  120.,120.,120.,120.,120.,120.,120.,120.,
     &  .014,.013,.014,.010,.0043,.0026,.0015,.0007,.0005,.0002,
     &  11*.0002/
      DATA VOIRES /96.,50.,50.,52.,73.,74.,84.,89.,104.,167.,
     &  193.,190.,196.,249.,356.,493.,571.,643.,714.,786.,857.,
     &  10.,11.,11., 8., 8., 9.,10.,11.,15.,14.,16.,13.,
     &  13.,16.,24.,24.,24.,24.,24.,24.,24.,
     &  50.,55.,55.,40.,40.,45.,50.,55.,75.,70.,80.,65.,
     &  65.,80.,120.,120.,120.,120.,120.,120.,120.,
     &  .009,.008,.008,.006,.0029,.0016,.0010,.0006,.0006,.0006,
     &  .0004,.0003,9*.0002/
C     DATA TMPSTD /86.,86.,93.,107.,143.,157.,171.,179.,186.,
C    &  200.,221.,243.,271.,307.,343.,343.,343.,343.,343.,
C    &  343.,343.,
C    &  12.9,11.6,9.9,9.4,10.3,10.7,11.1,11.6,12.0,12.4,12.9,
C    &  13.3,13.7,14.0,14.6,14.6,14.6,14.6,14.6,14.6,14.6,
C    &  21*0.,
C    &  21*0./
C  FOLLOWING TMPSTD ARE IDEALIZED FROM 3 CASES IN JAN 1998
C  THEY SHOULD PROBABLY BE REDONE AFTER MORE CASES ARE AVAILABLE
C  -------------------------------------------------------------
      DATA TMPSTD /260.,250.,230.,255.,320.,400.,455.,460.,430.,420.,
     &  430.,460.,480.,510.,520.,520.,520.,520.,520.,520.,520.,
     &  30.,28.,27.,24.,22.,21.,22.,24,24.,22,20.,20.,20.,
     &  20.,21.,21.,21.,21.,21.,21.,21.,
     &  21*0.,
     &  21*0./
      DATA TFACT /2.0,2.0,2.0,1.8,1.6,1.6,1.6,1.6,
     &  1.5,1.3,1.0,0.9,0.8,0.8,0.8,0.8,
     &  0.8,0.8,0.8,0.8,0.8,
     &  1.0,1.0,1.4,1.5,1.5,1.5,1.4,1.3,1.3,
     &  1.2,1.2,1.1,1.0,0.9,0.9,0.9,
     &  0.9,0.9,0.9,0.9,0.9,
     &  21*0.,
     &  21*0./
      DATA PMAND /1000.,925.,850.,700.,500.,400.,300.,250.,200.,
     &  150.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1./

      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ACCUM       Get files ready to accumulate information
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: The 3 files are used to summarize the performance of the
C   code and the quality of the data over a period of time (typically
C   a month).  This subroutine positions the file for adding
C   information to them.  Operationally, the files will generally be
C   empty and later filled by the PREPOBS_CQCBUFR. In that case, the 3
C   files must be accessible when the job completes so that they can
C   be copied to a more permanent place and cat'ed with the information
C   from other times.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL ACCUM
C
C   INPUT FILES:
C     NFEVN    - ACCUMULATES EVENTS
C     NFSUM    - ACCUMULATES SUMMARY INFORMATION FOR EVENTS
C     NFSTN    - ACCUMULATES STATION RECEIPT INFORMATION
C
C   OUTPUT FILES:
C     NFEVN    - UNIT NUMBER OF FILE WHICH ACCUMULATES EVENTS
C     NFSUM    - UNIT NUMBER OF FILE WHICH ACCUMULATES SUMMARY
C                INFORMATION FOR EVENTS
C     NFSTN    - UNIT NUMBER OF FILE WHICH ACCUMULATES STATION RECEIPT
C                INFORMATION
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE ACCUM

      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      COMMON /TESTS/   TEST
      LOGICAL          TEST
      CHARACTER*180    LINE

      IF(TEST) WRITE(60,500) NFIN,NFOUT,NFEVN,NFSUM,NFSTN
  500 FORMAT(' ACCUM--NFIN,NFOUT,NFEVN,NFSUM,NFSTN: ',5I5)

C
C     READ TO END OF DATA ON UNIT NFEVN, THEN
C     BACKSPACE OVER EOF AND LAST RECORD.
C     THIS UNIT IS USED TO ACCUMULATE THE EVENTS FILE.
C
  600 CONTINUE
      READ(NFEVN,650,END=690,err=690) LINE
  650 FORMAT(A139)
      GO TO 600
  690 CONTINUE
      BACKSPACE NFEVN

C
C     READ TO END OF DATA ON UNIT NFSUM, THEN
C     BACKSPACE OVER EOF AND LAST RECORD.
C     THIS UNIT IS USED TO ACCUMULATE THE EVENTS FILE.
C
  700 CONTINUE
      READ(NFSUM,750,END=790,err=790) LINE
  750 FORMAT(A132)
      GO TO 700
  790 CONTINUE
      BACKSPACE NFSUM
C
C     READ TO END OF DATA ON UNIT NFSTN, THEN
C     BACKSPACE OVER EOF AND LAST RECORD.
C     THIS UNIT IS USED TO ACCUMULATE THE STATION LIST.
C
  800 CONTINUE
      READ(NFSTN,850,END=890,err=890) LINE
  850 FORMAT(A180)
      GO TO 800
  890 CONTINUE
      BACKSPACE NFSTN

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ALP         RETURN THE LOGARITHM OF PRESSURE
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Return the logarithm of pressure for input argument
C   between 1.0 and 1400.0.  The values are calculated once and
C   subsequently returned for a list.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    X=ALP(P)
C   INPUT ARGUMENT LIST:
C     P        - PRESSURE BETWEEN 1.0 AND 1400.0 hPa
C
C   OUTPUT ARGUMENT LIST:
C     ALP      - RETURNS THE VALUE
C
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      FUNCTION ALP(P)

      REAL(8) BMISS

      SAVE ISET

C  ALP RETURNS THE NATURAL LOGARITHM OF PRESSURE, FOR
C  PRESSURE BETWEEN 1 AND 1400 HPA.  INPUT IS IN HPA.
C  --------------------------------------------------

      COMMON /LOGS/    PRLOG(14000)
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      DATA ISET /0/

      IF(ISET.EQ.0) THEN
        DO I=1,14000
          PRLOG(I) = ALOG(REAL(I)/10.)
        ENDDO
        ISET = 1
      ENDIF

      IP = NINT(P*10.)
      IF(IP.LE.14000 .AND. IP.GE.1) THEN
        ALP = PRLOG(IP)
      ELSE
        IF(P.GT.0.) THEN
          ALP = ALOG(P)
        ELSE
          ALP = BMISS
        ENDIF
      ENDIF

      RETURN
      END

C********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ANLWT       Get analysis weight for an observation
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: FOR A GIVEN X>0., RETURN THE ANALYSIS WEIGHT FOR AN
C   OBSERVATION WITH A GIVEN NORMALIZED RESIDUAL OF X.  IN ORDER TO
C   GET X, THE MEAN IS SUBTRACTED AND THEN DIVIDED BY THE STANDARD
C   DEVIATION.  LINEAR INTERPOLATION IS USED FROM THE FOLLOWING TABLE.
C   IT IS ASSUMED (FROM PAST DATA) THAT PER IS THE PROPORTION OF THE
C   DATA THAT ARE IN ERROR.  ALL ROUGH ERRORS ARE ASSUMED TO BE
C   CONTAINED WITHIN B STANDARD DEVIATIONS.
C
C PROGRAM HISTORY LOG:
C 1999-04-13  W. Collins  Original author.
C
C USAGE:    X=ANLWT(X,B,PER)
C
C   INPUT ARGUMENT LIST:
C     X        - NORMALIZED RESIDUAL
C     B        - ROUGH ERRORS ASSUMED WITHIN B STANDARD DEVIATIONS
C     PER      - PERCENT OF OBSERVATIONS ASSUMED TO BE IN ERROR
C
C   OUTPUT ARGUMENT LIST:
C     ANLWT    - THE 'WEIGHT' ASSIGNED TO OBSERVATION
C
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      FUNCTION ANLWT(X,B,PER)

      REAL W(0:50)
      DATA W/.3989,.3970,.3910,.3814,.3683,.3521,.3332,.3123,
     &       .2897,.2661,.2420,.2179,.1942,.1714,.1497,.1295,
     &       .1109,.0940,.0790,.0656,.0540,.0440,.0355,.0283,
     &       .0224,.0175,.0136,.0104,.0079,.0060,.0044,.0033,
     &       .0024,.0017,.0012,.0009,.0006,.0004,.0003,.0002,
     &       .0001,.00009,.00006,.00004,.00002,.00002,.00001,
     &       .00001,.00000,.00000,.00000/

      IF(X.GE.5.) THEN
        WINT = 0.
      ELSE
        Y  = X*10.
        IY = Y
        F  = Y-IY
        WINT = (1.-F)* W(IY) + F*W(IY+1)
      ENDIF

      PNOT = 1.-PER
      CPRIME = 0.5/B
      DENOM = PNOT*WINT + PER*CPRIME
      IF(DENOM.NE.0.) THEN
        ANLWT = PNOT*WINT/DENOM
      ELSE
        ANLWT = 1.
      ENDIF

C     A = .5*PER/B
C     P = (1.-A)*WINT + A*(B+X)
C     S = (1.-A)*WINT
C
C     IF(P.NE.0.) THEN
C       ANLWT = S/P
C     ELSE
C       ANLWT = 1.
C     ENDIF

      RETURN
      END

C********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    AREA        Determine region from X,Y.
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: From the longitude and latitude, determine the global
C   region in which an observation lies.
C
C PROGRAM HISTORY LOG:
C 1999-04-13  W. Collins  Original author.
C
C USAGE:    CALL AREA(IA,X,Y)
C
C   INPUT ARGUMENT LIST:
C     X        - LONGITUDE
C     Y        - LATITUDE
C
C   OUTPUT ARGUMENT LIST:
C     IA       - REGION (AREA) OF OBSERVATION
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE AREA(IA,X,Y)
      DIMENSION XN(28),XX(28),YN(28),YX(28)
      LOGICAL START

C     ALL LIMITS ARE INCLUSIVE
C     ------------------------

      DATA XN /7*230.,7*-40.,7*50.,7*140./
      DATA XX /7*319.99,7*49.99,7*139.99,7*229.99/
      DATA YN /60.,40.,20.,-20.,-40.,-60.,-90.00,
     &         60.,40.,20.,-20.,-40.,-60.,-89.99,
     &         60.,40.,20.,-20.,-40.,-60.,-89.99,
     &         60.,40.,20.,-20.,-40.,-60.,-89.99/
      DATA YX /90.00,59.99,39.99,19.99,-20.01,-40.01,-60.01,
     &         89.99,59.99,39.99,19.99,-20.01,-40.01,-60.01,
     &         89.99,59.99,39.99,19.99,-20.01,-40.01,-60.01,
     &         89.99,59.99,39.99,19.99,-20.01,-40.01,-60.01/
      DATA START /.TRUE./

C     IF(START) THEN
C       WRITE(6,500)
C       DO IA=1,28
C         WRITE(6,551) IA,XN(IA),XX(IA),YN(IA),YX(IA)
C       ENDDO
C       START = .FALSE.
C     ENDIF
C 551 FORMAT(1X,I7,4F8.2)
C 500 FORMAT(' REGION LIMITS:',/,
C    &       ' REGION    X-MIN   X-MAX   Y-MIN   Y-MAX')

      IF(X.LT.-40.)   X = X + 360.
      IF(X.GT.319.99) X = X - 360.
      DO I=1,28
        IF(X.GE.XN(I) .AND. X.LE.XX(I)  .AND.
     &     Y.GE.YN(I) .AND. Y.LE.YX(I)) THEN
          IA = I
          RETURN
        ENDIF
      ENDDO
      IA = 0
      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    AUXLEVS     INTERPOLATE DATA TO AUXILIARY LEVELS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Interpolate data to auxiliary levels: levels that are not
C   observed but which the regional models use.  Also, interpolate
C   increments to these levels.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL AUXLEVS(SKIP,SAME,WIND,SINGLE,ANYS)
C   INPUT ARGUMENT LIST:
C     SKIP     - TRUE FOR STATIONS TO SKIP (OUT OF REGION, ETC.)
C     SAME     - TRUE WHEN WIND PART GOES WITH PREVIOUSLY READ
C                MASS PART
C     WIND     - TRUE FOR WIND PART OF REPORT
C     SINGLE   - TRUE FOR SINGLE-STATION VERSION
C     ANYS     - TRUE IF PROFILE HAS ANY ERRORS
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE AUXLEVS(SKIP,SAME,WIND,SINGLE,ANYS)
      PARAMETER (NST=1500)

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /EVNSND/  PO(255), TO(255), ZO(255), CA(255),
     &                 PQ(255), TQ(255), ZQ(255), INP(255),
     &                 PR(255), TR(255), ZR(255), INZ(255),
     &                 QO(255), QQ(255), QR(255), INT(255),
     &                 TDE(255),TDQ(255),TDR(255),INQ(255),
     &                 UO(255), VO(255), SPO(255),DIRO(255),
     &                 WQ(255), WR(255)
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /PEROR/   ISERR, ISOBERR
      COMMON /LTP/     LVTYP(9,255), NCH(9,255), LVRC(9,255), LVRCALL
      REAL             WO(2,255)
      LOGICAL          ISERR, ANYS, ISOBERR, PRNT, LVRC, LVRCALL
      CHARACTER*8 SID
      CHARACTER*40 PEVN,TEVN,ZEVN,WEVN,SPVN,DIRVN,QEVN
      LOGICAL SKIP, WIND, SINGLE, SAME
      DATA PEVN   /'POB PQM PPC PRC                         '/
      DATA TEVN   /'TOB TQM TPC TRC                         '/
      DATA ZEVN   /'ZOB ZQM ZPC ZRC                         '/
      DATA QEVN   /'QOB QQM QPC QRC                         '/
      DATA WEVN   /'UOB VOB WQM WPC WRC                     '/
      DATA SPVN   /'FFO WQM WPC WRC                         '/
      DATA DIRVN  /'DDO WQM WPC WRC                         '/

      IF(.NOT. WIND) THEN
        ISERR = ISERR .OR. ANYS .OR. ISOBERR
        ISERR = .FALSE.
      ENDIF

C     IF(TEST) WRITE(60,510) SID(IS), SKIP, WIND, SINGLE, SAME
C 510 FORMAT(' AUXLEVS--SID(IS),SKIP,WIND,SINGLE,SAME: ',A8,2X,4L3)

      IF(SINGLE) RETURN

C  INTERPOLATE TO AUXILIARY LEVELS AND WRITE EVENTS
C  ------------------------------------------------

      IF(.NOT. SKIP) THEN
        CALL GETLEV(WIND)
        IF(.NOT.LVRCALL) RETURN   ! Return if no auxiliary levels
        CALL GETINC(WIND)
        IF(.NOT. WIND) THEN
          CALL FILALL(SAME,WIND)
          CALL FULVAL(WIND)
          CALL EVENT(NFOUT,PEVN,NLV,PO,PQ,PR,INP,NEV,CQCPC)
          WRITE(60,511) NEV,SID(IS)
          CALL EVENT(NFOUT,TEVN,NLV,TO,TQ,TR,INP,NEV,CQCPC)
          WRITE(60,512) NEV,SID(IS)
          CALL EVENT(NFOUT,ZEVN,NLV,ZO,ZQ,ZR,INP,NEV,CQCPC)
          WRITE(60,513) NEV,SID(IS)
          CALL EVENT(NFOUT,QEVN,NLV,QO,QQ,QR,INP,NEV,CQCPC)
          WRITE(60,514) NEV,SID(IS)
        ELSE
          CALL WINDATZ(SAME)
          CALL FILALL(SAME,WIND)
          IF(SAME) THEN
            CALL FULVAL(WIND)
            DO I=1,NEV
              WO(1,I) = UO(I)
              WO(2,I) = VO(I)
            ENDDO
            CALL EVENT(NFOUT,PEVN,NLV,PO,PQ,PR,INP,NEV,CQCPC)
            WRITE(60,511) NEV,SID(IS)
            CALL EVENT(NFOUT,ZEVN,NLV,ZO,ZQ,ZR,INP,NEV,CQCPC)
            WRITE(60,513) NEV,SID(IS)
            CALL EVENTW(NFOUT,WEVN,NLV,WO,WQ,WR,INP,NEV,CQCPC)
            WRITE(60,515) NEV,SID(IS)
          ENDIF
        ENDIF
      ENDIF

  511 FORMAT(' AUXLEVS--',I3,' PRESSURE EVENTS WRITTEN FOR ',A8)
  512 FORMAT(' AUXLEVS--',I3,' TEMPERATURE EVENTS WRITTEN FOR ',A8)
  513 FORMAT(' AUXLEVS--',I3,' HEIGHT EVENTS WRITTEN FOR ',A8)
  514 FORMAT(' AUXLEVS--',I3,' MOISTURE EVENTS WRITTEN FOR ',A8)
  515 FORMAT(' AUXLEVS--',I3,' WIND EVENTS WRITTEN FOR ',A8)

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    BEST        COMPUTES 'BEST' CORRECTION.
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: In most cases, the complex of hydrostatic residuals will
C   provide a test correction.  When it is not adequate, then the
C   other residuals are used to obtain an alternate test correction.
C   The 'best' value is a weighted mean of the available corrections.
C   Each is weighted by the inverse of its climatological standard
C   deviation.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL BEST(XB,X1,Y1,X2,Y2,X3,Y3,X4,Y4,X5,Y5)
C   INPUT ARGUMENT LIST:
C     X1       - FIRST RESIDUAL
C     Y1       - FIRST CLIMATOLOGICAL STANDARD DEVIATION
C     X2       - SECOND RESIDUAL
C     Y2       - SECOND CLIMATOLOGICAL STANDARD DEVIATION
C     X3       - THIRD RESIDUAL
C     Y3       - THIRD CLIMATOLOGICAL STANDARD DEVIATION
C     X4       - FOURTH RESIDUAL
C     Y4       - FOURTH CLIMATOLOGICAL STANDARD DEVIATION
C     X5       - FIFTH RESIDUAL
C     Y5       - FIFTH CLIMATOLOGICAL STANDARD DEVIATION
C
C   OUTPUT ARGUMENT LIST:
C     XB       - 'BEST' CORRECTION
C
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE BEST(XB,X1,Y1,X2,Y2,X3,Y3,X4,Y4,X5,Y5)

      REAL(8) BMISS

      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      IF(X1.NE.XMISS. AND. Y1.NE.XMISS .AND. Y1.NE.0.) THEN
        SUM1 = -X1/Y1
        SUM2 = 1./Y1
      ELSE
        SUM1 = 0.
        SUM2 = 0.
      ENDIF
      IF(X2.NE.XMISS .AND. Y2.NE.XMISS .AND. Y2.NE.0.) THEN
        SUM1 = SUM1 - X2/Y2
        SUM2 = SUM2 + 1./Y2
      ENDIF
      IF(X3.NE.XMISS .AND. Y3.NE.XMISS .AND. Y3.NE.0.) THEN
        SUM1 = SUM1 - X3/Y3
        SUM2 = SUM2 + 1./Y3
      ENDIF
      IF(X4.NE.XMISS .AND. Y4.NE.XMISS .AND. Y4.NE.0.) THEN
        SUM1 = SUM1 - X4/Y4
        SUM2 = SUM2 + 1./Y4
      ENDIF
      IF(X5.NE.XMISS .AND. Y5.NE.XMISS .AND. Y5.NE.0.) THEN
        SUM1 = SUM1 - X5/Y5
        SUM2 = SUM2 + 1./Y5
      ENDIF
      IF(SUM2.NE.0.) THEN
        XB = SUM1/SUM2
      ELSE
        XB = XMISS
      ENDIF

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CHANGE      APPLY CORRECTIONS TO DATA
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Apply corrections to the data.  For pressure, the data are
C   resorted by pressure.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 1998-02-18  W. Collins  Correct recomputation of virtual temperature.
C
C USAGE:    CALL CHANGE(ANY,SINGLE,SEQLP)
C   INPUT ARGUMENT LIST:
C     ANY      - LOGICAL VARIABLE TRUE IF THERE ARE ANY CORRECITIONS
C     SINGLE   - LOGICAL VARIABLE, TRUE FOR SINGLE-STATION RUNNING
C     SEQLP    - SEQUENCE NUMBER
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE CHANGE(ANY,SINGLE,SEQLP)

      REAL(8) BMISS

      SAVE IEVOLD
      PARAMETER (NST=1500)
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /EVENTS/  STN(2000),    SEQN(2000),  ISCAN(2000),
     &                 LEVL(2000),   PRES(2000),  LTYP(2000),
     &                 IVAR(2000),   COR(2000),   CORVAL(2000),
     &                 LSTYP(2000),  PCOR(2000),  IEVENT,
     &                 BASR(2000),   PISR(2000),  PSINCR(2000),
     &                 QMARK(2000),  IETYP(2000), EDATE(2000)
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /STN/     IS
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /CONSTS/  R, G, T0, CP, RV
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST, POUT
      INTEGER          IVIND(255)
      CHARACTER*8      SID, STN
      CHARACTER*10     EDATE
      LOGICAL ANY, SINGLE, PRNT, SDMP, SDMZ, SDMT, SDMQ
      DATA IEVOLD /0/

C-----------------------------------------------------------------------
C FCNS BELOW CONVERT TEMP/TD (K) & PRESS (MB) INTO SAT./ SPEC. HUM.(G/G)
C-----------------------------------------------------------------------
      ES(T) = 6.1078*EXP((17.269*(T - 273.15))/(T - 273.15 + 237.3))
      QS(T,P) = (0.622*ES(T))/(P-0.378*ES(T))
C-----------------------------------------------------------------------

      ANY = .FALSE.
      ICALL = 2
      IF(IEVENT.NE.IEVOLD) THEN
        DO I=IEVOLD+1,IEVENT
          IF(I.EQ.1) THEN
            CALL PRNTOUT(SEQLP,ICALL)
            IF(.NOT.SINGLE) CALL WBLOCKS
          ELSEIF(.NOT.SINGLE .AND. I.EQ.IEVOLD+1
     &      .AND. SEQN(I).NE.SEQLP) THEN
            CALL PRNTOUT(SEQLP,ICALL)
            CALL WBLOCKS
          ELSEIF(SINGLE .AND. .NOT.PRNT .AND.
     &      I.EQ.IEVOLD+1) THEN
            CALL PRNTOUT(SEQLP,ICALL)
          ENDIF
          L  = LEVL(I)
          IV = IVAR(I)
          SDMP = .FALSE.
          SDMZ = .FALSE.
          SDMT = .FALSE.
          SDMQ = .FALSE.
          SDMP = PQM(L).EQ.0. .OR. PQM(L).GE.14. .OR.
     &          (PQM(L).GE.4. .AND. PQM(L).LE.12.)
          SDMZ = ZQM(L).EQ.0. .OR. ZQM(L).GE.14. .OR.
     &          (ZQM(L).GE.4. .AND. ZQM(L).LE.12.)
          SDMT = TQM(L).EQ.0. .OR. TQM(L).GE.14. .OR.
     &          (TQM(L).GE.4. .AND. TQM(L).LE.12.)
          SDMQ = QQM(L).EQ.0. .OR. QQM(L).GE.14. .OR.
     &          (QQM(L).GE.4. .AND. QQM(L).LE.12.)

C  PRINT SOME DIAGNOSTICS
C  ----------------------

          IF(TEST) WRITE(60,500) I,IV,PQM(L),SDMP,ZQM(L),SDMZ,TQM(L),
     &      SDMT,QQM(L),PRES(I),CORVAL(I)
  500     FORMAT(' CHANGE--EVENT: ',I5,' IV: ',I5,
     &      '  PQM,SDMP,ZQM,SDMZ,TQM,SDMT,QQM,PRES,CORVAL:',
     &      3(F8.0,2X,L2,2X),F8.0,2X,F8.1,2X,F8.1)

          IF(IV.EQ.1 .AND. .NOT.SDMP) THEN
            IF(COR(I).NE.BMISS .AND. COR(I).NE.0.) ANY = .TRUE.
            IF(LST(L).EQ.240) THEN
              PS(IS) = CORVAL(I)
C             IF(ABS(PSCOR).LT.25.) THEN
                ZFC(L) = ZFC(L) - ZSCOR
C             ELSE
C               ZFC(L) = ZOB(L)
C             ENDIF
            ENDIF
            POB(L) = CORVAL(I)
            PQM(L) = QMARK(I)
            IF(CAT(L).NE.3 .AND. CAT(L).NE.4 .AND. CAT(L).NE.7) THEN
              PRC(L) = IETYP(I)
            ENDIF
C  RESORT RELATIVE TO NEW PRESSURES
C  --------------------------------
            CALL SHELL(POB,IVIND,NLV,1)
            CALL SORT(ZOB,IVIND,NLV)
            CALL SORT(TOB,IVIND,NLV)
            CALL SORT(TDO,IVIND,NLV)
            CALL SORT(QOB,IVIND,NLV)
            CALL SORT(CAT,IVIND,NLV)
            CALL SORT(HRDR,IVIND,NLV)
            CALL SORT(YDR,IVIND,NLV)
            CALL SORT(XDR,IVIND,NLV)
            CALL SORT(TVO,IVIND,NLV)
            CALL SORT(PQM,IVIND,NLV)
            CALL SORT(ZQM,IVIND,NLV)
            CALL SORT(TQM,IVIND,NLV)
            CALL SORT(QQM,IVIND,NLV)
            CALL SORT(PFC,IVIND,NLV)
            CALL SORT(ZFC,IVIND,NLV)
            CALL SORT(TFC,IVIND,NLV)
            CALL SORT(QFC,IVIND,NLV)
            CALL SORT(PRC,IVIND,NLV)
            CALL SORT(ZRC,IVIND,NLV)
            CALL SORT(TRC,IVIND,NLV)
            CALL SORT(QRC,IVIND,NLV)
            CALL SORT(TDFC,IVIND,NLV)
          ELSEIF(IV.EQ.2 .AND. .NOT.SDMZ) THEN
            IF(COR(I).NE.BMISS .AND. COR(I).NE.0.) ANY = .TRUE.
            ZOB(L) = CORVAL(I)
            ZQM(L) = QMARK(I)
            IF(CAT(L).NE.3 .AND. CAT(L).NE.4 .AND. CAT(L).NE.7) THEN
              ZRC(L) = IETYP(I)
            ENDIF
          ELSEIF(IV.EQ.3 .AND. .NOT.SDMT) THEN
            IF(COR(I).NE.BMISS .AND. COR(I).NE.0.) ANY = .TRUE.
            TOB(L) = CORVAL(I)
C           TDO(L) = TDO(L)            ! No change for depression
            IF(QOB(L).NE.BMISS .AND.
     &        TDO(L).NE.BMISS .AND.
     &        POB(L).NE.BMISS) THEN                !Recalculate q, Tv
              TD     = TOB(L)-TDO(L)+T0            !TDO is depression
              P      = POB(L)
              QOB(L) = QS(TD,P)  !Recalculated for new T, original Td
              TVO(L) = (TOB(L)+T0)*(1.+.61*QOB(L)) - T0
            ELSE
              TVO(L) = BMISS
            ENDIF
            TQM(L) = QMARK(I)
            IF(CAT(L).NE.3 .AND. CAT(L).NE.4 .AND. CAT(L).NE.7) THEN
              TRC(L) = IETYP(I)
            ENDIF
          ELSEIF((IV.EQ.4 .OR.IV.EQ.5) .AND. .NOT.SDMQ) THEN
            IF(COR(I).NE.BMISS .AND. COR(I).NE.0.) ANY = .TRUE.
            QOB(L) = CORVAL(I)
            QQM(L) = QMARK(I)
            IF(CAT(L).NE.3 .AND. CAT(L).NE.4 .AND. CAT(L).NE.7) THEN
              QRC(L) = IETYP(I)
            ENDIF
          ENDIF
        ENDDO
      ENDIF

      IEVOLD = IEVENT
      IF(ANY) LAST = 0

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: CHDIST
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1990-11-06
C
C ABSTRACT: COMPUTES CHORD LENGTH DISTANCE FROM A FIXED POINT TO AN
C   ARRAY OF POINTS USING THE FORMULA: S**2/2 = 1 - COS(Y1-Y2) +
C   COS(Y1)*COS(Y2)*(1-COS(X1-X2)).  ALSO THE DIRECTION OF EACH POINT
C   IS COMPUTED WITH RESPECT TO THE FIXED POINT AND RETURNED AS WELL.
C
C PROGRAM HISTORY LOG:
C 1990-11-06  J. Woollen  Original author.
C
C USAGE:    CALL CHDIST(X1,Y1,X2,Y2,DIST,DIRN,NP)
C   INPUT ARGUMENTS:
C     X1         - X-COORDINATE  (LONGITUDE) OF FIXED POINT
C     Y1         - Y-COORDINATE  (LATITUDE ) OF FIXED POINT
C     X2         - X-COORDINATES (LONGITUDE) FOR SET OF POINTS
C     Y2         - Y-COORDINATES (LATITUDE ) FOR SET OF POINTS
C     NP         - NUMBER OF OUTPUTS REQUESTED
C
C   OUTPUT ARGUMENTS:
C     DIST       - CHORD LENGTH DISTANCES FROM FIXED POINT (KM)
C     DIRN       - DIRECTION FROM FIXED POINT (DEG)
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE CHDIST(X1,Y1,X2,Y2,DIST,DIRN,NP)

      DIMENSION  X2(NP),Y2(NP),DIST(NP),DIRN(NP)

      DATA PI180/.0174532 /,RADE/6371./

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IF(NP.EQ.0) RETURN

C  COMPUTE THE DISTANCE
C  --------------------

      DO 10 I=1,NP
      COSY1 = COS(Y1*PI180)
      COSY2 = COS(Y2(I)*PI180)
      COSDX = COS((X1-X2(I))*PI180)
      COSDY = COS((Y1-Y2(I))*PI180)
      S = 1.0-COSDY+COSY1*COSY2*(1.0-COSDX)
      S = SQRT(2.*S)
      IF(S.LE..002) S = 0.
      DIST(I) = S*RADE
10    CONTINUE

C  COMPUTE DIRECTIONS
C  ------------------

      DO 20 I=1,NP
      DX = (X2(I)-X1)*COS(Y1)
      DY = (Y2(I)-Y1)
      IF(DX.GT.0.) THEN
         DIRN(I) = 0. + ATAN(DY/DX)/PI180
      ELSE IF(DX.LT.0.) THEN
         DIRN(I) = 180. + ATAN(DY/DX)/PI180
      ELSE IF(DX.EQ.0.) THEN
         DIRN(I) = SIGN(90.,DY)
      ENDIF
      IF(DIRN(I).LT.0.) DIRN(I) = DIRN(I) + 360.
20    CONTINUE

C      DO 30 I=1,NP
C30    PRINT*,DIST(I),DIRN(I)

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CHECKIT     CONFIRMS CORRECTIONS
C   PRGMMR: W. COLLINS       ORG: NP20       DATE: 1997-03-18
C
C ABSTRACT: Confirms whether or not a correction is acceptable.
C   The array GOOD has members for each check, as described below,
C   telling whether the check value is acceptable or not after the
C   test correction.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL CHECKIT(IER,ZC1,RIZ1,RVZ1,RHZ1,RTZ1,ZC2,RIZ2,RVZ2,
C                        RHZ2,RTZ2,TC1,RIT1,RVT1,RHT1,RTT1,TC2,RIT2,
C                        RVT2,RHT2,RTT2,S1,S2,S3,B1,B2,B3,GOOD)
C
C   INPUT ARGUMENT LIST:
C     IER      - ERROR TYPE
C                =  1 HEIGHT
C                =  2 TEMPERATURE
C                =  3 Z AND T
C                =  7 Z AT TWO LEVELS
C                =  8 T AT TWO LEVELS
C                =  9 Z AT LOWER, T AT UPPER LEVEL
C                = 10 T AT LOWER, Z AT UPPER LEVEL
C                = 33 Z AND T WITH COMPENSATION; NO CHECK ON HYDROSTATIC
C     ZC1      -
C     RIZ1     -
C     RVZ1     -
C     RHZ1     -
C     RTZ1     -
C     ZC2      -
C     RIZ2     -
C     RVZ2     -
C     RHZ2     -
C     RTZ2     -
C     TC1      -
C     RIT1     -
C     RVT1     -
C     RHT1     -
C     RTT1     -
C     TC2      -
C     RIT2     -
C     RVT2     -
C     RHT2     -
C     RTT2     -
C     S1       -
C     S2       -
C     S3       -
C     B1       -
C     B2       -
C     B3       -
C
C   OUTPUT ARGUMENT LIST:
C     GOOD     - TELLS WHICH RESIDUALS ARE MADE SMALLER BY CORRECTION
C            GOOD(CHECK,LEVEL,VARIABLE)
C              CHECK:
C              1 - INCREMENT
C              2 - VERTICAL
C              3 - HORIZONTAL
C              4 - HYDROSTATIC
C              5 - TEMPORAL
C              6 - ALL
C              LEVEL:
C              1 - LOWER
C              2 - UPPER
C              VARIABLE:
C              1 - HEIGHT
C              2 - TEMPERATURE
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE CHECKIT(IER,ZC1,RIZ1,RVZ1,RHZ1,RTZ1,ZC2,RIZ2,RVZ2,
     &                   RHZ2,RTZ2,TC1,RIT1,RVT1,RHT1,RTT1,TC2,RIT2,
     &                   RVT2,RHT2,RTT2,S1,S2,S3,B1,B2,B3,GOOD)

      REAL(8) BMISS

      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST
      REAL  ZC(2),TC(2),RIZ(2),RIT(2),RVZ(2),RVT(2),
     &      RHZ(2),RHT(2),RTZ(2),RTT(2),S(3),B(3)
      LOGICAL GOOD(6,2,2)

      ZC(1)  = ZC1
      ZC(2)  = ZC2
      TC(1)  = TC1
      TC(2)  = TC2
      RIZ(1) = RIZ1
      RIZ(2) = RIZ2
      RIT(1) = RIT1
      RIT(2) = RIT2
      RVZ(1) = RVZ1
      RVZ(2) = RVZ2
      RVT(1) = RVT1
      RVT(2) = RVT2
      RHZ(1) = RHZ1
      RHZ(2) = RHZ2
      RHT(1) = RHT1
      RHT(2) = RHT2
      RTZ(1) = RTZ1
      RTZ(2) = RTZ2
      RTT(1) = RTT1
      RTT(2) = RTT2
      S(1)   = S1
      S(2)   = S2
      S(3)   = S3
      B(1)   = B1
      B(2)   = B2
      B(3)   = B3


      IF(IER.LE.3) THEN
        LEV = 1
      ELSE
        LEV = 2
      ENDIF

      DO K=1,2
        DO J=1,2
          DO I=1,6
            GOOD(I,J,K) = .FALSE.
          ENDDO
        ENDDO
      ENDDO

C  EVALUATE CORRECTIONS FOR INCREMENT, VERTICAL, HORIZONTAL
C  TEMPORAL RESIDUALS
C  --------------------------------------------------------

      DO L=1,LEV
        IF(ABS(RIZ(L)+ZC(L)).LT.ABS(RIZ(L)) .OR. RIZ(L).EQ.XMISS)
     &     GOOD(1,L,1) = .TRUE.
        IF(ABS(RIT(L)+TC(L)).LT.ABS(RIT(L)) .OR. RIT(L).EQ.XMISS)
     &     GOOD(1,L,2) = .TRUE.
        IF(ABS(RVZ(L)+ZC(L)).LT.ABS(RVZ(L)) .OR. RVZ(L).EQ.XMISS)
     &     GOOD(2,L,1) = .TRUE.
        IF(ABS(RVT(L)+TC(L)).LT.ABS(RVT(L)) .OR. RVT(L).EQ.XMISS)
     &     GOOD(2,L,2) = .TRUE.
        IF(ABS(RHZ(L)+ZC(L)).LT.ABS(RHZ(L)) .OR. RHZ(L).EQ.XMISS)
     &     GOOD(3,L,1) = .TRUE.
        IF(ABS(RHT(L)+TC(L)).LT.ABS(RHT(L)) .OR. RHT(L).EQ.XMISS)
     &     GOOD(3,L,2) = .TRUE.
        IF(ABS(RTZ(L)+ZC(L)).LT.ABS(RTZ(L)) .OR. RTZ(L).EQ.XMISS)
     &     GOOD(5,L,1) = .TRUE.
        IF(ABS(RTT(L)+TC(L)).LT.ABS(RTT(L)) .OR. RTT(L).EQ.XMISS)
     &     GOOD(5,L,2) = .TRUE.
      ENDDO

C  EVALUATE CORRECTIONS FOR HYDROSTATIC RESIDUALS
C  AND FOR OVERALL
C  ----------------------------------------------

      IF(IER.EQ.1) THEN
        IF((ABS(S(1)+ZC(1)).LE.ABS(S(1)) .OR. S(1).EQ.XMISS) .AND.
     &     (ABS(S(2)-ZC(1)).LE.ABS(S(2)) .OR. S(2).EQ.XMISS))
     &     GOOD(4,1,1) = .TRUE.
        IF((GOOD(1,1,1) .OR. GOOD(2,1,1)) .AND. GOOD(3,1,1) .AND.
     &      GOOD(4,1,1) .AND. GOOD(5,1,1)) GOOD(6,1,1) = .TRUE.

      ELSEIF(IER.EQ.2) THEN
        IF((ABS(S(1)-B(1)*TC(1)).LE.ABS(S(1)) .OR. S(1).EQ.XMISS) .AND.
     &     (ABS(S(2)-B(2)*TC(1)).LE.ABS(S(2)) .OR. S(2).EQ.XMISS))
     &     GOOD(4,1,2) = .TRUE.
        IF(GOOD(1,1,2) .AND. GOOD(2,1,2) .AND. GOOD(3,1,2) .AND.
     &      GOOD(4,1,2) .AND. GOOD(5,1,2)) GOOD(6,1,2) = .TRUE.

      ELSEIF(IER.EQ.3) THEN
        IF((ABS(S(1)+ZC(1)-B(1)*TC(1)).LE.ABS(S(1)) .OR.
     &      S(1).EQ.XMISS) .AND.
     &     (ABS(S(2)-ZC(1)-B(2)*TC(1)).LE.ABS(S(2)) .OR.
     &      S(2).EQ.XMISS))  THEN
          GOOD(4,1,1) = .TRUE.
          GOOD(4,1,2) = .TRUE.
        ENDIF
        IF((GOOD(1,1,1) .OR. GOOD(2,1,1)) .AND. GOOD(3,1,1) .AND.
     &      GOOD(4,1,1) .AND. GOOD(5,1,1)) GOOD(6,1,1) = .TRUE.
        IF(GOOD(1,1,2) .AND. GOOD(2,1,2) .AND. GOOD(3,1,2) .AND.
     &      GOOD(4,1,2) .AND. GOOD(5,1,2)) GOOD(6,1,2) = .TRUE.

      ELSEIF(IER.EQ.33) THEN
        IF((GOOD(1,1,1) .OR. GOOD(2,1,1)) .AND. GOOD(3,1,1)
     &     .AND. GOOD(5,1,1)) GOOD(6,1,1) = .TRUE.
        IF(GOOD(1,1,2) .AND. GOOD(2,1,2) .AND. GOOD(3,1,2)
     &     .AND. GOOD(5,1,2)) GOOD(6,1,2) = .TRUE.

      ELSEIF(IER.EQ.7) THEN
        IF((ABS(S(1)+ZC(1)).LE.ABS(S(1)) .OR. S(1).EQ.XMISS) .AND.
     &     (ABS(S(2)+ZC(2)-ZC(1)).LE.ABS(S(2)) .OR. S(2).EQ.XMISS) .AND.
     &      ABS(S(3)-ZC(2)).LE.ABS(S(3))) THEN
          GOOD(4,1,1) = .TRUE.
          GOOD(4,2,1) = .TRUE.
        ENDIF
        IF((GOOD(1,1,1) .OR. GOOD(2,1,1)) .AND. GOOD(3,1,1) .AND.
     &      GOOD(4,1,1) .AND. GOOD(5,1,1)) GOOD(6,1,1) = .TRUE.
        IF((GOOD(1,2,1) .OR. GOOD(2,2,1)) .AND. GOOD(3,2,1) .AND.
     &      GOOD(4,2,1) .AND. GOOD(5,2,1)) GOOD(6,2,1) = .TRUE.

      ELSEIF(IER.EQ.8) THEN
        IF((ABS(S(1)-B(1)*TC(1)).LE.ABS(S(1)) .OR. S(1).EQ.XMISS) .AND.
     &     (ABS(S(2)-B(2)*(TC(1)+TC(2))).LE.ABS(S(2)) .OR.
     &      S(2).EQ.XMISS) .AND.
     &     (ABS(S(3)-B(3)*TC(2)).LE.ABS(S(3)) .OR. S(3).EQ.XMISS)) THEN
          GOOD(4,1,2) = .TRUE.
          GOOD(4,2,2) = .TRUE.
        ENDIF
        IF(GOOD(1,1,2) .AND. GOOD(2,1,2) .AND. GOOD(3,1,2) .AND.
     &      GOOD(4,1,2) .AND. GOOD(5,1,2)) GOOD(6,1,2) = .TRUE.
        IF(GOOD(1,2,2) .AND. GOOD(2,2,2) .AND. GOOD(3,2,2) .AND.
     &      GOOD(4,2,2) .AND. GOOD(5,2,2)) GOOD(6,2,2) = .TRUE.

      ELSEIF(IER.EQ.9) THEN
        IF((ABS(S(1)+ZC(1)).LE.ABS(S(1)) .OR. S(1).EQ.XMISS) .AND.
     &     (ABS(S(2)-ZC(1)-B(2)*TC(2)).LE.ABS(S(2)) .OR.
     &      S(2).EQ.XMISS) .AND.
     &     (ABS(S(3)-B(2)*TC(2)).LE.ABS(S(3)) .OR. S(3).EQ.XMISS)) THEN
          GOOD(4,1,1) = .TRUE.
          GOOD(4,2,2) = .TRUE.
        ENDIF
        IF((GOOD(1,1,1) .OR. GOOD(2,1,1)) .AND. GOOD(3,1,1) .AND.
     &      GOOD(4,1,1) .AND. GOOD(5,1,1)) GOOD(6,1,1) = .TRUE.
        IF(GOOD(1,2,2) .AND. GOOD(2,2,2) .AND. GOOD(3,2,2) .AND.
     &      GOOD(4,2,2) .AND. GOOD(5,2,2)) GOOD(6,2,2) = .TRUE.

      ELSEIF(IER.EQ.10) THEN
        IF((ABS(S(1)-B(1)*TC(1)).LE.ABS(S(1)) .OR. S(1).EQ.XMISS) .AND.
     &     (ABS(S(2)+ZC(2)-B(2)*TC(1)).LE.ABS(S(2)) .OR.
     &      S(2).EQ.XMISS) .AND.
     &     (ABS(S(3)-ZC(2)).LE.ABS(S(3)) .OR. S(3).EQ.XMISS)) THEN
          GOOD(4,1,2) = .TRUE.
          GOOD(4,2,1) = .TRUE.
        ENDIF
        IF(GOOD(1,1,2) .AND. GOOD(2,1,2) .AND. GOOD(3,1,2) .AND.
     &      GOOD(4,1,2) .AND. GOOD(5,1,2)) GOOD(6,1,2) = .TRUE.
        IF((GOOD(1,2,1) .OR. GOOD(2,2,1)) .AND. GOOD(3,2,1) .AND.
     &      GOOD(4,2,1) .AND. GOOD(5,2,1)) GOOD(6,2,1) = .TRUE.
      ENDIF

      IF(TEST) WRITE(60,500) (((GOOD(I,J,K),I=1,6),J=1,2),K=1,2)
  500 FORMAT(' CHECKIT--GOOD: ',4(2X,6L2))

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CHKTMP      CHECK TEMPORAL CHECK FOR BAD INFLUENCE
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1998-02-11
C
C ABSTRACT: CHECK TEMPORAL CHECK FOR THE INFLUENCE OF BAD DATA.  DO
C   NOT USE DATA AT OFF-TIMES IF THE INCREMENT THERE EXCEEDS 3.5
C   STANDARD DEVIATIONS
C
C PROGRAM HISTORY LOG:
C 1992-05-26  W. Collins  Original author.
C
C USAGE:    CALL CHKTMP(A,C,R,ICK)
C   INPUT ARGUMENT LIST:
C     A        - INCREMENT AT T-12 OR T-24 HOURS
C     C        - INCREMENT AT T+12 OR T+24 HOURS
C     R        - PROPORTIONAL TO LIMIT
C
C   OUTPUT ARGUMENT LIST:
C     ICK      - INDICATOR OF INFLUENCE OF BAD DATA:
C              = 0 INFLUENCING STATIONS OK
C              = 1 INFLUENCING STATION QUENSTIONABLE
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE CHKTMP(A,C,R,ICK)
      IF(ABS(A).GT.0.35*R .OR. ABS(C).GT.0.35*R) THEN
        ICK = 1
      ELSE
        ICK = 0
      ENDIF
      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CKPS        CHECK FOR SURFACE PRESSURE = 0.
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Check for surface pressure = 0.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL CKPS
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE CKPS
      PARAMETER (NST=1500)

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      CHARACTER*8 SID
      LOGICAL PRNT

      IF(ISC.NE.1) RETURN

C  CHECK FOR SURFACE PRESSURE = 0.
C  -------------------------------

      DO LL=1,NLV
        L = LL
        IF(CAT(L).EQ.0) GOTO 10
      ENDDO
      RETURN
   10 CONTINUE
      IF(POB(L).NE.0.) RETURN

C  HAVE FOUND A SURFACE PRESSURE = 0.
C  NOW LOOK FOR THE FIRST COMPLETE LEVEL ABOVE
C  -------------------------------------------

      WRITE(6,500) SID(IS)
  500 FORMAT(' CKPS--SURFACE PRESSURE = 0. FOR ',A8)

      DO LL=L+1,NLV
        LP = LL
        IF(CAT(LL).EQ.1 .AND. LEVTYP(LL).LE.2) GOTO 20
      ENDDO
      RETURN
   20 CONTINUE

C  COMPLETE LEVEL FOUND ABOVE
C  SOLVE FOR GOOD VALUE OF SURFACE PRESSURE
C  ----------------------------------------

      CALL GETPS(PSL,ELV(IS),ZOB(LP),TOB(LP),POB(LP))

C  SET POB(L) AND PS(IS) EQUAL TO THIS VALUE
C  -----------------------------------------

C     POB(L) = PSL
C     PS(IS) = PSL

C  SAVE AN EVENT
C  -------------

C     CALL SEVENT(SID(IS),SQN(IS),L,240,POB(L),LEVTYP(L),4,POB(L),
C    &  POB(L),POB(L),1.,107,0.,0.,0.)

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    COMPER      DIAGNOSE COMPUTAION ERRORS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Diagnose and find corrections for computation errors and
C   errors in temperature and height at the same level where the
C   hydrostatic residuals compensate in such a way to make one of them
C   small.  In either case, there is just one large hydrostatic
C   residual.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL COMPER(L,LM)
C   INPUT ARGUMENT LIST:
C     L        - LEVEL INDEX WITHIN MANDATORY LEVELS
C     LM       - LEVEL INDEX WITHIN ALL REPORTED LEVELS
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE COMPER(L,LM)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /TMPSND/  ZOBT(21,NST,4), TOBT(21,NST,4),
     &                 ZIT(21,NST,4),  TIT(21,NST,4),
     &                 ZTMP(21,NST),   TTMP(21,NST),
     &                 NZTMP(21,NST),  NTTMP(21,NST),
     &                 ITERR(4)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /ALLOW/   ALLZ(41), ALLT(71), NALLZ, NALLT
      COMMON /CONSTS/  R, G, T0, CP, RV
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST, PRNT
      CHARACTER*8 SID
      LOGICAL POUT, GOOD(6,2,2)
      LOGICAL ZG, TG, TDG, QG, ERROR

      IF(TEST) WRITE(60,500) SID(IS),POB(LM),L,LM,LST(LM)
  500 FORMAT(' COMPER--CALLED FOR: ',A8,'   POB,L,LM,LST(LM): ',
     &  F10.1,3I5)

C  THIS SUBROUTNE DEALS WITH TWO DISTINCT TYPES OF ERRORS:
C     1) COMPUTATION ERRORS
C     2) TYPE 3 ERRORS WITH COMPENSATION
C  THEY ARE BOTH DIAGNOSED BY AN ISOLATED LARGE HYDROSTATIC RESIDUAL
C-------------------------------------------------------------------

      IF(HYD(L).EQ.BMISS) RETURN
      IF(LST(LM).EQ.120) THEN
        HYDLM = BASRES(IS)
        IF(POB(LM).GT.0.) THEN
          BLM = 0.5*(R/G)*ALOG(PS(IS)/POB(LM))
        ELSE
          BLM = BMISS
        ENDIF
        IF(BLM.LT.0.01) BLM = BMISS
        ZILM = 0.
      ELSE
        IF(HYD(L-1).NE.BMISS) THEN
          HYDLM = HYD(L-1)
          BLM = B(L-1)
          LL = LMAND(L-1)
C       ELSEIF(L.GT.2) THEN
C         IF(HYD(L-2).NE.BMISS) THEN
C           HYDLM = HYD(L-2)
C           BLM   = B(L-2)
C           LL    = LMAND(L-2)
C         ELSE
C           HYDLM = BMISS
C           BLM   = BMISS
C           LL    = BMISS
C         ENDIF
        ELSE
          HYDLM = BMISS
          BLM   = BMISS
          LL    = BMISS
        ENDIF
        IF(LL.LE.MAND) THEN
          ZILM = ZI(LL)
          ZVLM = ZV(LL)
          TILM = TI(LL)
          TVLM = TV(LL)
        ELSE
          ZILM = 0.
          ZVLM = 0.
          TILM = 0.
          TVLM = 0.
        ENDIF
      ENDIF

      IF(TEST) WRITE(60,503) HYDLM, BLM, ZILM, HYD(L), B(L), ZI(LM)
  503 FORMAT(' COMPER--HYDLM,BLM,ZILM: ',3F10.1,'  HYD(L),',
     &  'B(L),ZI(LM): ',3F10.1)

      IF(HYDLM.NE.BMISS) THEN
        POUT = .TRUE.
        IF(LST(LM).EQ.120) THEN
          E2 = 1.
          E1 = 0.
        ELSE
          CALL BEST(ZCBEST(1),XMISS,XMISS,ZI(LM),XINC(L,1),
     &      ZV(LM),VOIRES(L,1),ZHM(L,IS),HOIRES(L,1),
     &      ZTMP(L,IS),TMPSTD(L,1))
          CALL BEST(TCBEST(1),XMISS,XMISS,TI(LM),XINC(L,2),
     &      TV(LM),VOIRES(L,2),THM(L,IS),HOIRES(L,2),
     &      TTMP(L,IS),TMPSTD(L,2))

          ZDIF = ZI(LM) - ZILM
          ZCR = -B(L)*HYDLM/(BLM+B(L))
          TCR = HYDLM/(BLM+B(L))

C  DECIDE WHICH ERROR IS MORE LIKELY
C  ---------------------------------

          E1 = ABS(ZDIF-HYDLM)
          E2 = 0.5*(ABS(ZCBEST(1)-ZCR) + BLM*ABS(TCBEST(1)-TCR))
          IF(TEST) WRITE(60,504) E1,E2,ZCBEST(1),TCBEST(1),ZDIF,ZCR,TCR
  504     FORMAT(' COMPER--E1,E2,ZCBEST,TCBEST,ZDIF,ZCR,TCR: ',
     &      2F10.3,5F10.1)
        ENDIF

C  TYPE 3 ERROR WITH COMPENSATION
C  ------------------------------

        IF(E2.LT.E1) THEN
          IF(TEST) WRITE(60,501) SID(IS),POB(LM)
  501     FORMAT(' COMPER--POSSIBLE TYPE 3 WITH COMPENSATION FOR: ',A8,
     &      ' AT: ',F10.1)
          IHSC(2,LM) = 3
          IHSC(3,LM) = 3

C  SINCE XCR AND XBEST ARE REASONABLE CORRECTIONS, AVERAGE THEM
C  ------------------------------------------------------------

          ZCR = 0.5* (ZCR + ZCBEST(1))
          IF(ABS(ZCR).LT.ZCLIM) ZCR = 0.
          TCR = 0.5* (TCR + TCBEST(1))
          IF(ABS(TCR).LT.TCLIM) TCR = 0.
          CALL SIMPLE(ZCR,ZOB(LL),ALLZ,NALLZ,1.)
          CALL ROUND(ZCR,POB(LL),2)
          CALL SIMPLE(TCR,TOB(LL),ALLT,NALLT,10.)
          CALL ROUND(TCR,POB(LL),3)
          PZCOR = ZCR
          PTCOR = TCR
          CALL CHECKIT(33,ZCR,   ZI(LM), ZV(LM),  ZHM(L,IS),ZTMP(L,IS),
     &                   XMISS,  XMISS,  XMISS,  XMISS,    XMISS,
     &                   TCR,    TI(LM), TV(LM), THM(L,IS),TTMP(L,IS),
     &                   XMISS,  XMISS,  XMISS,  XMISS,    XMISS,
     &              HYDLM,HYD(L),XMISS,BLM,B(L),XMISS,GOOD)
          IF(TEST) WRITE(60,505) ZCR,ZI(LM),ZV(LM),ZHM(L,IS),ZTMP(L,IS),
     &      (GOOD(I,1,1),I=1,5),TCR,TI(LM),TV(LM),THM(L,IS),TTMP(L,IS),
     &      (GOOD(I,1,2),I=1,5)
  505     FORMAT(' COMPER--ZCR,ZI,ZV,ZHM,ZTMP,GOOD-S: ',5F10.1,5L2,/,
     &           '         TCR,TI,TV,THM,TTMP,GOOD-S: ',5F10.1,5L2)
          IF(GOOD(6,1,1) .AND. GOOD(6,1,2) .AND.
     &      ABS(ZCR).GE.ZCLIM .AND. ABS(TCR).GE.TCLIM) THEN
            QM = 1.
            CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &        2,ZCR,ZOB(LM)+ZCR,PZCOR,QM,IHSC(2,LM),XMISS,XMISS,XMISS)
            CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &        3,TCR,TOB(LM)+TCR,PTCOR,QM,IHSC(3,LM),XMISS,XMISS,XMISS)
          ELSE
            IF(ABS(ZCBEST(1)).GT.0.25*XINC(L,1)) THEN
              ZCR = 0.
              QM = 13.
            ELSE
              IHSC(2,LM) = 0
              PZCOR = ZCR
              ZCR = 0.
              QM = 1.
            ENDIF
            IF(IHSC(2,LM).NE.0) THEN
              CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &          2,ZCR,ZOB(LM)+ZCR,PZCOR,QM,IHSC(2,LM),XMISS,XMISS,XMISS)
            ENDIF
            IF(ABS(TCBEST(1)).GT.0.25*XINC(L,2)) THEN
              PTCOR = TCR
              TCR = 0.
              QM = 13.
            ELSE
              IHSC(3,LM) = 0
              PTCOR = TCR
              TCR = 0.
              QM = 1.
            ENDIF
            IF(IHSC(3,LM).NE.0) THEN
              CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &          3,TCR,TOB(LM)+TCR,PTCOR,QM,IHSC(3,LM),XMISS,XMISS,XMISS)
            ENDIF
          ENDIF

C  COMPUTATION ERROR
C  -----------------

        ELSE
          ZCR = -HYDLM
          CALL ROUND(ZCR,500.,2)
          PZCOR = ZCR
          IF(ABS(ZCR).LT.1.5*ZCLIM) THEN
            ZCR = 0.
            RETURN
          ENDIF
          NSUM = 0
          SUM  = 0.
          SUMS = 0.
          DO I=LM,MIN(LM+3,NLEV)
            LL = MANLEV(POB(I))
            IF(LL.NE.0 .AND. ZI(I).NE.BMISS) THEN
              IF(LL.GE.L) THEN
                ZZ = ZI(I) + ZCR
              ELSE
                ZZ = ZI(I)
              ENDIF
              NSUM = NSUM + 1
              SUM  = SUM + ZZ
              SUMS = SUMS + ZZ**2
            ENDIF
          ENDDO
          XSUM = NSUM
          IF(XSUM.NE.0.) THEN
            SX = (SUMS - (SUM**2)/XSUM)/XSUM
          ELSE
            SX = BMISS
          ENDIF
          IF(SX.GT.0.) THEN
            SX = SQRT(SX)
          ELSE
            SX = 0.
          ENDIF
          IF(TEST) WRITE(60,502) SID(IS),POB(LM),SX,ZCR
  502     FORMAT(' COMPER--POSSIBLE COMPUTATION ERROR FOR: ',A8,
     &      '   AT: ',F10.1,'   SX =',F10.2,'  ZCR =',F10.0)
          IF(SX.LT.40.) THEN
            BAD = 0.
            ALL = 0.
            DO I=L,MIN(L+3,LMAND(0))
              LL = LMAND(I)
              IF(LL.LE.MAND) THEN
                ALL = ALL + 1.
                CALL CHECKIT(3,ZCR,  ZI(LL),XMISS,ZHM(I,IS),ZTMP(I,IS),
     &                         XMISS,XMISS, XMISS,XMISS,    XMISS,
     &                         XMISS,XMISS, XMISS,XMISS,    XMISS,
     &                         XMISS,XMISS, XMISS,XMISS,    XMISS,
     &                    XMISS,XMISS,XMISS,XMISS,XMISS,XMISS,GOOD)
                IF(.NOT.GOOD(1,1,1)) BAD = BAD + 1.
              ENDIF
            ENDDO
            IF(ALL.NE.0.) THEN
              RATIO = BAD/ALL
            ELSE
              RATIO = 0.
            ENDIF
            IF(TEST) WRITE(60,506) RATIO
  506       FORMAT(' COMPER--RATIO: ',F10.2)
            IF(RATIO.LT.0.5) THEN
              QM = 1.
              DO I=L,LMAND(0)
                LL = LMAND(I)
                IF(LL.LE.MAND) THEN
                  IHSC(2,LL) = 6
                  IF(ZOB(LL).NE.BMISS) THEN
                    CALL SEVENT(SID(IS),SQN(IS),LL,LST(LL),POB(LL),
     &                LEVTYP(LL),2,ZCR,ZOB(LL)+ZCR,PZCOR,QM,IHSC(2,LL),
     &                  XMISS,XMISS,XMISS)
                  ENDIF
                ENDIF
              ENDDO
            ELSE
              ZCR = 0.
              QM = 3.
              IHSC(2,LM) = 6
              IF(ZOB(LM).NE.BMISS) THEN
                CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),
     &            LEVTYP(LM),2,ZCR,ZOB(LM)+ZCR,PZCOR,QM,IHSC(2,LM),
     &              XMISS,XMISS,XMISS)
              ENDIF
            ENDIF
          ELSE
            ZCR = 0.
            QM = 3.
            IHSC(2,LM) = 6
            IF(ZOB(LM).NE.BMISS) THEN
              CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),
     &          LEVTYP(LM),2,ZCR,ZOB(LM)+ZCR,PZCOR,QM,IHSC(2,LM),
     &            XMISS,XMISS,XMISS)
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: COORS
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1990-11-06
C
C ABSTRACT: COMPUTES CHORD LENGTH DISTANCE FOR A MATRIX OF
C   LOCATIONS <(X,Y)I,(X,Y)J> USING THE NORMAL ANGLE  FORMULA:
C     S**2/2 = 1 - COS(Y1-Y2) + COS(Y1)*COS(Y2)*(1-COS(X1-X2))
C   DERIVATIVES OF DISTANCE WITH RESPECT TO LAT AND LON ARE ALSO
C   COMPUTED AND COMBINED WITH APPROPRIATE CORRELATION FUNCTIONS
C   AND DERIVATIVES WITH RESPECT TO DISTANCE TO FORM THE MULTI-
C   VARIATE CORRELATIONS FOR THE HEIGHT WIND ANALYSIS.
C
C PROGRAM HISTORY LOG:
C 1990-11-06  J. Woollen  Original author.
C
C USAGE:    CALL COORS(NP,CHTWV,X1,Y1,X2,Y2,D,ZZ)
C   INPUT ARGUMENTS:
C     NP     - NUMBER OF OUTPUTS REQUESTED
C     CHTWV  - VECTOR OF CONSTANTS FOR THE GAUSSIAN LENGTH SCALE
C     X1     - X-COORDINATES (LONGITUDE) FOR 1ST SET OF POINTS
C     Y1     - Y-COORDINATES (LATITUDE ) FOR 1ST SET OF POINTS
C     X2     - X-COORDINATES (LONGITUDE) FOR 2ND SET OF POINTS
C     Y2     - Y-COORDINATES (LATITUDE ) FOR 2ND SET OF POINTS
C
C   OUTPUT ARGUMENTS:
C     D     - DISTANCES BETWEEN ARRAYS OF POINTS IN RADIANS
C     ZZ    - CORRELATION BETWEEN HEIGHT AND HEIGHT
C
C REMARKS: THIS PROGRAM COMPUTES A GAUSSIAN CORRELATION WITH
C   LENGTH SCALE GIVEN BY THE CHTWV INPUT ARGUMENT.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE COORS(NP,CHTWV,X1,Y1,X2,Y2,D,ZZ)


      DIMENSION  CHTWV(NP),X1(NP),Y1(NP),X2(NP),Y2(NP)
      DIMENSION  D(NP),ZZ(NP)


      DATA PI180 /.0174532 /

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IF(NP.EQ.0) RETURN

C  LOOP OVER SET OF INPUT POINTS
C  -----------------------------

      DO 20 I=1,NP

C  COMPUTE THE MATRIX OF SINES AND COSINES
C  ---------------------------------------

      COSY1 = COS(Y1(I)*PI180)
      COSY2 = COS(Y2(I)*PI180)
      COSDX = COS((X1(I)-X2(I))*PI180)
      COSDY = COS((Y1(I)-Y2(I))*PI180)

C  COMPUTE THE NORMAL ANGLE
C  ------------------------

      S = 1.0-COSDY+COSY1*COSY2*(1.0-COSDX)
      S = SQRT(2.*S)


C  COMPUTE THE VARIOUS CORRELATIONS
C  --------------------------------

      ZZ(I) = EXP(-CHTWV(I)*S*S)
      D(I)  = S

20    CONTINUE

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CUMPROB     Calculate cumulative probability
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Calculate cumulative probability, interpolated from table.
C
C PROGRAM HISTORY LOG:
C 1999-04-13  W. Collins  Original author.
C
C USAGE:    Y=CUMPROB(X)
C
C   INPUT ARGUMENT LIST:
C     X        - VALUE
C
C   OUTPUT ARGUMENT LIST:
C     CUMPROB  - Cumulative probability
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      FUNCTION CUMPROB(X)
C
C  FOR A GIVEN X, RETURN THE CUMULATIVE PROBABILITY FOR
C  ABS(VALUE <= X). X IS ASSUMED TO BE NORMAL VARIATE.
C  USE LINEAR INTERPOLATION IN A TABLE.
C

      REAL P(0:40)
      DATA P/.5000,.5398,.5793,.6179,.6554,.6915,
     &       .7257,.7580,.7881,.8159,.8413,
     &       .8643,.8849,.9032,.9192,.9332,
     &       .9452,.9554,.9641,.9713,.9772,
     &       .9821,.9861,.9893,.9918,.9938,
     &       .9953,.9965,.9974,.9981,.9987,
     &       .9990,.9993,.9995,.9997,.9998,
     &       .9998,.9999,.9999,1.0000,1.0000/

      IF(X.LT.0. OR. X.GE.4.) THEN
        CUMPROB = 1.
      ELSE
        Y  = X*10.
        IY = Y
        F  = Y-IY
        PY = (1.-F)* P(IY) + F*P(IY+1)
        CUMPROB = 2.*(PY - .5000)
      ENDIF

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    DHOR        CALCULATE CHANGES TO HORIZONTAL RESIDUAL
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Calculate changes to the horizontal residual that would
C   result from the corrections to temperature and height that were
C   found earlier.  The hydrostatic residual itself is not recalculate
C   under the assumption that it was screened from the influence of
C   bad neighbors when first calculated.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 1998-02-11  W. Collins  Add update to temporal check.
C
C USAGE:    CALL DHOR
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE DHOR

      REAL(8) BMISS

      SAVE IEVOLD
      PARAMETER (NST=1500)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /TMPSND/  ZOBT(21,NST,4), TOBT(21,NST,4),
     &                 ZIT(21,NST,4),  TIT(21,NST,4),
     &                 ZTMP(21,NST),   TTMP(21,NST),
     &                 NZTMP(21,NST),  NTTMP(21,NST),
     &                 ITERR(4)
      COMMON /EVENTS/  STN(2000),    SEQN(2000),  ISCAN(2000),
     &                 LEVL(2000),   PRES(2000),  LTYP(2000),
     &                 IVAR(2000),   COR(2000),   CORVAL(2000),
     &                 LSTYP(2000),  PCOR(2000),  IEVENT,
     &                 BASR(2000),   PISR(2000),  PSINCR(2000),
     &                 QMARK(2000),  IETYP(2000), EDATE(2000)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /STN/     IS
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST
      CHARACTER*8      STN
      CHARACTER*10     EDATE
      LOGICAL          PRNT
      LOGICAL ZG, TG, TDG, QG, ERROR
      DATA IEVOLD /0/

      IF(IEVENT.NE.IEVOLD) THEN
        DO I=IEVOLD+1,IEVENT
          LM = LEVL(I)
          L  = MANLEV(PRES(I))
          IF(L.EQ.0) GOTO 100
          IV = IVAR(I)
          IF(TEST) WRITE(60,500) STN(I),I,LM,L,IV
  500     FORMAT(' DHOR--STN,EVENT,LM,L,IV: ',A8,2X,4I5)
          IF(IV.EQ.1) THEN
            GOTO 100
          ELSEIF(IV.EQ.2) THEN
            IF(ZHM(L,IS).NE.BMISS) THEN
              ZHM(L,IS)  = ZHM(L,IS) + COR(I)
              NZHM(L,IS) = MIN1(20.,20.*ABS(ZHM(L,IS))/HOIRES(L,1))
            ENDIF
            IF(ZTMP(L,IS).NE.BMISS) THEN
              ZTMP(L,IS) = ZTMP(L,IS) + COR(I)
              NZTMP(L,IS) = MIN1(20.,20.*ABS(ZTMP(L,IS)/
     &           (TMPSTD(L,1)*TFACT(L,1))))
            ENDIF
          ELSEIF(IV.EQ.3) THEN
            IF(THM(L,IS).NE.BMISS) THEN
              THM(L,IS)  = THM(L,IS) + COR(I)
              NTHM(L,IS) = MIN1(20.,20.*ABS(THM(L,IS))/HOIRES(L,2))
            ENDIF
            IF(TTMP(L,IS).NE.BMISS) THEN
              TTMP(L,IS) = TTMP(L,IS) + COR(I)
              NTTMP(L,IS) = MIN1(20.,20.*ABS(TTMP(L,IS)/
     &           (TMPSTD(L,2)*TFACT(L,2))))
            ENDIF
          ENDIF
  100     CONTINUE
        ENDDO
      ENDIF
      IEVOLD = IEVENT

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    DISTR       CALCULATE MOMENTS OF DISTRIBUTION
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1991-07-23
C
C ABSTRACT: FOR GIVEN DATA, CALCULATE THE 0TH THROUGH 4TH MOMENTS.
C   ALSO PRODUCE BINNED DISTRIBUTION.
C
C PROGRAM HISTORY LOG:
C 1991-07-23  W. Collins  Original author.
C
C USAGE:    CALL DISTR(X,MSK,XLIM,XMSG,NX,N,NDIV,DDIV,NZERO,
C                      DZERO,NS,X1,SD,SK,XK)
C   INPUT ARGUMENT LIST:
C     X        - DATA
C     MSK      - MASK: VALUE = 0 -- USE IN COMPUTATIONS
C                      VALUE.NE.0 -- DONT USE
C     XLIM     - (1) MINIMUM VALUE TO USE
C                (2) MAXIMUM VALUE TO USE
C     XMSG     - MISSING VALUE FOR VARIABLE
C     NX       - DIMENSION OF X
C     NDIV     - NUMBER OF DIVISIONS
C     NZERO    - = 0, USE DZERO AS CENTER
C                = 1, USE MEAN AS CENTER
C     DZERO    - CENTRAL DIVISION LOCATION
C
C   OUTPUT ARGUMENT LIST:
C     N        - OUTPUT DISTRIBUTION, COUNT IN EACH DIVISION
C     NS       - COUNT OF VALUES USED
C     X1       - FIRST MOMENT (MEAN)
C     X2       - SECOND MOMENT
C     X3       - THIRD MOMENT
C     X4       - FOURTH MOMENT
C     SD       - STANDARD DEVIATION (POPULATION)
C                  = SQRT(X2 - X1**2)
C     SK       - SKEWNESS = X3/(X2)**(3/2)
C     XK       - KURTOSIS = X4/(SD**4) - 3
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE DISTR(X,MSK,XLIM,XMSG,NX,N,NDIV,DDIV,
     &  NZERO,DZERO,NS,X1,SD,SK,XK)
      PARAMETER (NST=1500)

      INTEGER N(23), MSK(NST)
      REAL X(NST),XLIM(2)

C
C     COMPUTE VARIOUS POWERS OF X.
C
      NS = 0
      SUM1 = 0.
      SUM2 = 0.
      SUM3 = 0.
      SUM4 = 0.
      DO 10 I=1,NX
        IF(MSK(I).EQ.0.AND.ABS(X(I)).LT.0.5*XMSG
     &    .AND.X(I).GE.XLIM(1).AND.X(I).LE.XLIM(2)) THEN
          NS = NS + 1
          SUM1 = SUM1 + X(I)
          SUM2 = SUM2 + X(I)**2
          SUM3 = SUM3 + X(I)**3
          SUM4 = SUM4 + X(I)**4
        ENDIF
   10 CONTINUE
      IF(NS.LT.1) THEN
        DZERO = 0.
        X1 = 0.
        SD = 0.
        SK = 0.
        XK = 0.
        GO TO 15
      ENDIF
      SUM1 = SUM1/NS
      SUM2 = SUM2/NS
      SUM3 = SUM3/NS
      SUM4 = SUM4/NS
C
C     CALCULATE VARIOUS OUTPUT STATISTICS.
C
      X1 = SUM1
      ARG = SUM2 - SUM1**2
      IF(ARG.GT.0.) THEN
        SD = SQRT(ARG)
      ELSE
        SD = 0.
      ENDIF
      X3 = SUM3 - 3.*SUM2*SUM1 + 2.*SUM1**3
      IF(SUM2.GT.0.) THEN
        SK = X3/(SUM2)**1.5
      ELSE
        SK = 0.
      ENDIF
      X4 = SUM4 - 4.*SUM3*SUM1 + 6.*SUM2*SUM1**2 - 3.*SUM1**4
      IF(SD.GT.0.) THEN
        XK = (X4/SD**4) - 3.0
      ELSE
        XK = 0.
      ENDIF
C
C     COUNT THE NUMBER OF OBSERVATIONS IN EACH DIVISION.
C
   15 CONTINUE
      IF(DDIV.EQ.0.) DDIV = 1.
      IF(NZERO.EQ.1) DZERO = SUM1
      DO 18 I=1,NDIV
        N(I) = 0
   18 CONTINUE
      CON = 0.5*NDIV + 1.0 - DZERO/DDIV
      DO 20 I=1,NX
        IF(MSK(I).EQ.0.AND.ABS(X(I)).LT.XMSG
     &    .AND.X(I).GE.XLIM(1).AND.X(I).LE.XLIM(2)) THEN
          INDEX = X(I)/DDIV + CON
          IF(INDEX.LT.1) INDEX = 1
          IF(INDEX.GT.NDIV) INDEX = NDIV
          N(INDEX) = N(INDEX) + 1
        ENDIF
   20 CONTINUE
      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    DMA         PERFORM DECISION MAKING
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT:  Decision Making Algorithm:  makes all decision regarding
C   data quality and determines corrections.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL DMA(ANY,OBS)
C   INPUT ARGUMENT LIST:
C     ANY      - TRUE IF THERE ARE ANY ERRORS
C     OBS      - TRUE FOR LAST SCAN WHEN OBSERVATION ERRORS ARE
C                DETERMINED
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE DMA(ANY,OBS)
      PARAMETER (NST=1500)
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /DOTYP40/ DOT40
      COMMON /TESTS/   TEST
      LOGICAL          TEST, DOT40
      CHARACTER*8 SID
      LOGICAL POUT, ANY, OBS, PRNT
      LOGICAL ZG, TG, TDG, QG, ERROR

      IF(TEST) WRITE(60,500) SID(IS)
  500 FORMAT(' DMA--CALLED FOR: ',A8)
      OBS = .FALSE.

C  DETERMINE SURFACE TYPE
C  ----------------------

      IF(ISC.EQ.1) THEN
        CALL STYPE
C       CALL HOLES
      ENDIF

C  PREFORM ANALYSIS, LAYER BY LAYER
C  --------------------------------

      IF(ANY .OR. LAST.LT.NLEV) THEN
        CALL ERRTYP
      ELSE
        OBS = .TRUE.
C       CALL OBERR
        CALL NOBERR
        IF(DOT40) CALL POBERR
      ENDIF

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: DRCTSL
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1990-11-06
C
C ABSTRACT: DRIVER FOR CHOLESKY TYPE LINEAR EQUATION SOLVER.
C
C PROGRAM HISTORY LOG:
C 1990-11-06  J. Woollen  Original author.
C
C USAGE:    CALL DRCTSL(FAALL,RAALL,DOTPRD,NDIM,MAXDIM,NXXYY,NFT,LEV,
C                       IV)
C   INPUT ARGUMENTS:
C     FAALL      - ARRAY OF SYMMETRIC MATRIXES
C     RAALL      - ARRAY OF MATRIX RIGHT HAND SIDES
C     NDIM       - ARRAY OF MATRIX RANKS
C     MAXDIM     - MAXIMUM RANK MATRIX IN MATRIX ARRAY
C     NXXYY      - NUMBER OF MATRIXES IN MATRIX ARRAY
C     NFT        - NUMBER OF RIGHT HAND SIDE VECTORS PER MATRIX
C     LEV        - LEVEL
C     IV         - VARIABLE
C   OUTPUT ARGUMENTS:
C     RAALL      - ARRAY OF MATRIX SOLUTIONS
C     DOTPRD     - ARRAY OF DOT PRODUCTS OF RHS VECTORS WITH MATRIX
C                  SOLUTIONS
C
C REMARKS: ILL CONDITIONED OR NON POSITIVE DEFINITE MATRIXES ARE
C   IDENTIFIED BY DOT PRODUCTS GT 1 OR LT 0 OR BY A MESSAGE FROM
C   VSOLVE. FIVE ATTEMPTS ARE MADE TO SOLVE BAD ONES, BY RIDGE
C   REGRESSION, AFTER WHICH A NULL SOLUTION IS RETURNED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
C-----------------------------------------------------------------------

      SUBROUTINE DRCTSL(FAALL,RAALL,DOTPRD,NDIM,MAXDIM,NXXYY,NFT,LEV,IV)

      DIMENSION  FAALL(1000,10), DOTPRD(1000,1),
     .           RAALL(1000,4,1), NDIM(1000)

      DIMENSION  A(1000,10),B(1000,4,1),BAD(1000),SMOOTH(6)

      LOGICAL BAD
      DATA SMOOTH /1.00,1.01,1.02,1.05,1.10,2.00/

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C  LOOP FOR POSSIBILITY OF BAD MATRIXS
C  -----------------------------------

      DO 50 ITRY=1,6

      NBAD = 0

C  INITIALIZE THE WORKING ARRAYS
C  -----------------------------

      DO J=1,MAXDIM*(MAXDIM+1)/2
        DO I=1,NXXYY
          A(I,J) = FAALL(I,J)
        ENDDO
      ENDDO

      DO K=1,NFT
        DO J=1,MAXDIM
          DO I=1,NXXYY
            B(I,J,K) = RAALL(I,J,K)
          ENDDO
        ENDDO
      ENDDO

      DO J=1,NFT
        DO I=1,NXXYY
          DOTPRD(I,J) = 0.
        ENDDO
      ENDDO

      DO J=1,MAXDIM
        JJ = J*(J+1)/2
        DO I=1,NXXYY
          A(I,JJ) = FAALL(I,JJ)*SMOOTH(ITRY)
        ENDDO
      ENDDO

C  CALL THE MATRIX SOLVER
C  ----------------------

      CALL VSOLVE (A,B,NDIM,BAD,NFT,NXXYY,MAXDIM)

C  MAKE THE DOT PRODUCTS OF SOLUTIONS WITH RIGHT HAND SIDES
C  --------------------------------------------------------

      DO K=1,NFT
        DO J=1,MAXDIM
          DO I=1,NXXYY
            DOTPRD(I,K) = DOTPRD(I,K) + RAALL(I,J,K)*B(I,J,K)
          ENDDO
        ENDDO
      ENDDO

      DO K=1,NFT
        DO I=1,NXXYY
          IF(DOTPRD(I,K).GT.1.)THEN
            DO J=1,MAXDIM
              B(I,J,K) = B(I,J,K)/DOTPRD(I,K)
            ENDDO
            DOTPRD(I,K) = 1.
          ENDIF
        ENDDO
      ENDDO

C  CHECK FOR BAD ONES - DO IT AGAIN IF NECESSARY
C  ---------------------------------------------

      DO J=1,NFT
        DO I=1,NXXYY
          BAD(I) = BAD(I) .OR. DOTPRD(I,J).LT.0.
     &      .OR. DOTPRD(I,J).GT.1.
        ENDDO
      ENDDO

      DO I=1,NXXYY
        IF(BAD(I)) THEN
           DO K=1,NFT
             DOTPRD(I,K) = 0.
             DO J=1,MAXDIM
               B(I,J,K) = 0.
             ENDDO
           ENDDO
           NBAD = NBAD + 1
        ENDIF
      ENDDO

      IF(NBAD.NE.0) THEN
        PRINT*, 'NBAD=',NBAD,' ON TRY ',ITRY
     &         , ' LEV=',LEV,' IV=',IV
      ELSE
        GOTO 55
      ENDIF

50    CONTINUE

C  COPY SOLUTIONS INTO OUTPUT ARRAY - ZERO BAD ONES OUT
C  ----------------------------------------------------

55    CONTINUE
      DO K=1,NFT
        DO J=1,MAXDIM
          DO I=1,NXXYY
            RAALL(I,J,K) = B(I,J,K)
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ERR123      PERFORM ERROR ANALYSIS FOR TYPE 1,2,3.
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Perform error analysis for single height (type 1), single
C   temperature (type 2) and height and temperature (type 3) at the
C   same pressure level.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL ERR123(L,LM)
C   INPUT ARGUMENT LIST:
C     L        - LEVEL INDEX WITHIN MANDATORY LEVELS
C     LM       - LEVEL INDEX WITHIN ALL REPORTED LEVELS
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE ERR123(L,LM)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /ALLOW/   ALLZ(41), ALLT(71), NALLZ, NALLT
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /TMPSND/  ZOBT(21,NST,4), TOBT(21,NST,4),
     &                 ZIT(21,NST,4),  TIT(21,NST,4),
     &                 ZTMP(21,NST),   TTMP(21,NST),
     &                 NZTMP(21,NST),  NTTMP(21,NST),
     &                 ITERR(4)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /CONSTS/  R, G, T0, CP, RV
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST, SDMQ
      CHARACTER*8 SID
      LOGICAL POUT, GOOD(6,2,2)
      LOGICAL ZG, TG, TDG, QG, ERROR, PRNT

      IF(TEST) WRITE(60,500) SID(IS), POB(LM), LST(LM), L, LM
  500 FORMAT(' ERR123--CALLED FOR: ',A8,'  AT: ',F10.1,
     &  ' WITH LST(LM),L,LM: ',3I5)

      QM = 2.
      QMT = 2.
      QMZ = 2.
      NHYDLM = BMISS

C  DETERMINE APPROPRIATE HYDROSTATIC RESIDUAL, B TO USE
C  ----------------------------------------------------

      IF(LST(LM).NE.121) THEN
        HYDLT = HYDN(L)
        HYDLZ = HYD(L)
        NHYDL = NHYD(L)
        BL   = B(L)
      ELSE
        HYDLT = BMISS
        HYDLZ = BMISS
        NHYDL = -1
        BL   = BMISS
      ENDIF
      IF(LST(LM).EQ.130) THEN
        IF(HYDN(L-1).NE.BMISS) THEN
          HYDLMT = HYDN(L-1)
          HYDLMZ = HYD(L-1)
          NHYDLM = NHYD(L-1)
          BLM = B(L-1)
        ELSEIF(L.GT.2) THEN
          IF(HYDN(L-2).NE.BMISS) THEN
            HYDLMT = HYDN(L-2)
            HYDLMZ = HYD(L-2)
            NHYDLM = NHYD(L-2)
            BLM = B(L-2)
          ELSE
            HYDLMT = BMISS
            HYDLMZ = BMISS
            NHYDL = -1
            BLM = BMISS
          ENDIF
        ELSE
          HYDLMT = BMISS
          HYDLMZ = BMISS
          NHYDL = -1
          BLM = BMISS
        ENDIF
      ELSEIF(LST(LM).EQ.120) THEN
        HYDLMT = BASRES(IS)
        HYDLMZ = BASRES(IS)
        NHYDLM = NBAS
        BLM = 0.5*(R/G)*ALOG(PS(IS)/POB(LM))
        IF(BLM.LT.0.01) BLM = BMISS
      ELSE
        RETURN
      ENDIF

      H1 = BMISS
      H2 = BMISS
      T1 = BMISS
      H1 = ABS(HYDLMZ+HYDLZ)
C     H1 = ABS(HYDLMT+HYDLT)
      H2 = 2.*TT * SQRT(BLM**2 + BL**2)
      T1 = ABS(HYDLT/BL - HYDLMT/BLM)

      IF(TEST) WRITE(60,504) POB(LM),H1,H2,T1,HYDLZ,HYDLMZ,HYDLT,
     &  HYDLMT,BL,BLM
  504 FORMAT(' ERR123--POB,H1,H2,T1: ',4F10.1,'  HYDLZ,HYDLMZ: ',
     &  2F10.1,'  HYDLT,HYDLMT: ',2F10.1,'  BL,BLM: ',2F10.3)

C  HEIGHT ERROR
C  ------------

      IF(H1.LT.H2 .AND. T1.GE.TT .AND.
     &  NHYDL.GT.NHLIM .AND. NHYDLM.GT.NHLIM) THEN
        IF(TEST) WRITE(60,501) POB(LM)
  501   FORMAT(' ERR123--HEIGHT ERROR AT: ',F10.1)
C       ZCOR(1) = (HYDLZ/BL**2 - HYDLMZ/BLM**2)/
C    &         (1./BL**2 + 1./BLM**2)   ! Based upon X's
        ZCOR(1) = 0.5*(HYDLZ-HYDLMZ)    ! Based upon s's
        IHSC(2,LM) = 1
        POUT = .TRUE.
        IF(NZI(LM).LT.6) THEN
          ZDIF = ZI(LM)
        ELSE
          ZDIF = ZD(LM)
        ENDIF
        CALL BEST(ZCBEST(1),XMISS,XMISS,ZDIF,XINC(L,1),
     &    ZV(LM),VOIRES(L,1),ZHM(L,IS),HOIRES(L,1),
     &    ZTMP(L,IS),TMPSTD(L,1))
        ZR = ABS(ZCOR(1)-ZCBEST(1))/XINC(L,1)
        IF(ZR.LT.0.25) THEN
          ZC1 = ZCOR(1)
          CALL SIMPLE(ZC1,ZOB(LM),ALLZ,NALLZ,1.)
          CALL ROUND(ZC1,POB(LM),2)
          PZCOR = ZC1
          IF(ABS(ZC1).LT.ZCLIM) THEN
            ZC1 = 0.
            QM = 3.
          ELSE
            QM = 1.
          ENDIF
          ZCOR(1) = ZC1
        ELSE
          ZC1 = ZCBEST(1)
          PZCOR = ZC1
          IF(ABS(ZC1).LT.ZCLIM) THEN
            ZC1 = 0.
            QM = 3.
          ELSE
            CALL CHECKIT(1,ZC1,  ZDIF, ZV(LM),ZHM(L,IS),ZTMP(L,IS),
     &                     XMISS,XMISS,XMISS, XMISS,    XMISS,
     &                     XMISS,XMISS,XMISS, XMISS,    XMISS,
     &                     XMISS,XMISS,XMISS, XMISS,    XMISS,
     &                     HYDLMZ,HYDLZ,XMISS,BLM,BL,XMISS,GOOD)
            IF(GOOD(6,1,1)) THEN
              ZC1 = ZCBEST(1)
              ZC0 = ZC1
              CALL SIMPLE(ZC1,ZOB(LM),ALLZ,NALLZ,1.)
              CALL ROUND(ZC1,POB(LM),2)
              PZCOR = ZC1
              CALL CHECKIT(1,ZC1,  ZDIF, ZV(LM),ZHM(L,IS),ZTMP(L,IS),
     &                       XMISS,XMISS,XMISS, XMISS,    XMISS,
     &                       XMISS,XMISS,XMISS, XMISS,    XMISS,
     &                       XMISS,XMISS,XMISS, XMISS,    XMISS,
     &                       HYDLMZ,HYDLZ,XMISS,BLM,BL,XMISS,GOOD)
C  THE FOLLOWING LOGIC, AND ELSEWHERE, CAN BE ELABORATED SO THAT
C  EVEN IF GOOD(6) IS NOT .TRUE. BUT GOOD(4) IS .TRUE., THEN THE
C  CORRECTION WILL BE MADE BUT FLAGGED AS QUESTIONABLE.  THIS IS
C  THE CASE WHERE A HYDROSTATIC AND OBSERVATIONAL ERROR MAY BE
C  COMBINED.
              IF(GOOD(6,1,1)) THEN
                QM = 1.
              ELSE
                ZC1 = ZC0
                PZCOR = ZC1
                QM = 1.
              ENDIF
            ELSE
              ZC1 = 0.
              QM = 13.
            ENDIF
          ENDIF
        ENDIF
        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),2,ZC1,
     &    ZOB(LM)+ZC1,PZCOR,QM,IHSC(2,LM),XMISS,XMISS,XMISS)

C  TEMPERATURE ERROR
C  -----------------

      ELSEIF(T1.LT.TT .AND. H1.GE.H2) THEN
        IF(TEST) WRITE(60,502) POB(LM)
  502   FORMAT(' ERR123--TEMPERATURE ERROR AT: ',F10.1)
C       TCOR(1) = 0.5 * (HYDLMT/BLM + HYDLT/BL)   ! Based upon X's
        TCOR(1) = (HYDLMT*BLM + HYDLT*BL)/(BLM**2 + BL**2) ! on s's
        IHSC(3,LM) = 2
        POUT = .TRUE.
        CALL BEST(TCBEST(1),XMISS,XMISS,TI(LM),
     &    XINC(L,2),TV(LM),VOIRES(L,2),THM(L,IS),HOIRES(L,2),
     &    TTMP(L,IS),TMPSTD(L,2))
        TR = ABS(TCOR(1)-TCBEST(1))/XINC(L,2)
        IF(TR.LT.0.25) THEN
          TC1 = TCOR(1)
          CALL SIMPLE(TC1,TOB(LM),ALLT,NALLT,10.)
          CALL ROUND(TC1,POB(LM),3)
          PTCOR = TC1
          IF(ABS(TC1).LT.TCLIM) THEN
            TC1 = 0.
            QM = 3.
          ELSE
            QM = 1.
          ENDIF
          TCOR(1) = TC1
        ELSE
          TC1 = TCBEST(1)
          PTCOR = TC1
          IF(ABS(TC1).LT.TCLIM) THEN
            TC1 = 0.
            QM = 3.
          ELSE
            CALL CHECKIT(2,XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                     XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                     TC1,  TI(LM),TV(LM),THM(L,IS),TTMP(L,IS),
     &                     XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                     HYDLMT,HYDLT,XMISS,BLM,BL,XMISS,GOOD)
            IF(GOOD(6,1,2)) THEN
              TC1  = TCBEST(1)
              TC0 = TC1
              CALL SIMPLE(TC1,TOB(LM),ALLT,NALLT,10.)
              CALL ROUND(TC1,POB(LM),3)
              PTCOR = TC1
              CALL CHECKIT(2,XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                       XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                       TC1,  TI(LM),TV(LM),THM(L,IS),TTMP(L,IS),
     &                       XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                       HYDLMT,HYDLT,XMISS,BLM,BL,XMISS,GOOD)
              IF(GOOD(6,1,2)) THEN
                QM = 1.
              ELSE
                TC1 = TC0
                PTCOR = TC1
                QM = 1.
              ENDIF
            ELSE
              TC1 = 0.
              QM = 13.
            ENDIF
          ENDIF
        ENDIF
        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),3,TC1,
     &    TOB(LM)+TC1,PTCOR,QM,IHSC(3,LM),XMISS,XMISS,XMISS)
        SDMQ = .FALSE.
        SDMQ = QQM(LM).EQ.0. .OR. QQM(LM).GE.14. .OR.
     &        (QQM(LM).GE.4. .AND. QQM(LM).LE.12.)
        IF((QM.EQ.3. .OR. QM.EQ.13.) .AND. QOB(LM).LT.BMISS .AND.
     &     .NOT.SDMQ) THEN
          CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),5,
     &      0.,QOB(LM),0.,QM,IHSC(4,LM),XMISS,XMISS,XMISS)
        ENDIF

C  NO ERROR
C  --------

      ELSEIF(NHYDL.LT.NHLIM .AND. NHYDLM.LT.NHLIM) THEN

        IF(TEST) WRITE(60,506) POB(LM)
  506   FORMAT(' ERR123--DECIDED NO HYDROSTATIC ERROR AT: ',F10.1)

        RETURN

C  HEIGHT AND TEMPERATURE ERROR POSSIBLE
C  -------------------------------------

      ELSE
        IF(TEST) WRITE(60,503) POB(LM)
  503   FORMAT(' ERR123--Z AND T ERROR AT: ',F10.1)
        ZCOR(1) = (BL*HYDLZ - BLM*HYDLMZ)/(BLM+BL)
        TCOR(1) = (HYDLMT + HYDLT)/(BLM + BL)
        IHSC(2,LM) = 3
        IHSC(3,LM) = 3
        POUT = .TRUE.
        IF(NZI(LM).LT.6) THEN
          ZDIF = ZI(LM)
        ELSE
          ZDIF = ZD(LM)
        ENDIF
        CALL BEST(ZCBEST(1),XMISS,XMISS,ZDIF,XINC(L,1),
     &    ZV(LM),VOIRES(L,1),ZHM(L,IS),HOIRES(L,1),
     &    ZTMP(L,IS),TMPSTD(L,1))
        CALL BEST(TCBEST(1),XMISS,XMISS,TI(LM),
     &    XINC(L,2),TV(LM),VOIRES(L,2),THM(L,IS),HOIRES(L,2),
     &    TTMP(L,IS),TMPSTD(L,2))
        ZR = ABS(ZCOR(1)-ZCBEST(1))/XINC(L,1)
        TR = ABS(TCOR(1)-TCBEST(1))/XINC(L,2)
        IF(TEST) WRITE(60,505) POB(LM),ZR,TR
  505   FORMAT(' ERR123--POB,ZR,TR: ',F10.1,2F8.3)
        IF(ZR.LT.0.20 .AND. TR.LT.0.20) THEN    !All resids agree
          ZC1 = ZCOR(1)                         !Try hydrostatic corrs
          TC1 = TCOR(1)
          CALL SIMPLE(ZC1,ZOB(LM),ALLZ,NALLZ,1.)
          CALL ROUND(ZC1,POB(LM),2)
          CALL SIMPLE(TC1,TOB(LM),ALLT,NALLT,10.)
          CALL ROUND(TC1,POB(LM),3)
          PZCOR = ZC1
          PTCOR = TC1
          CALL CHECKIT(3,ZC1,  ZDIF,  ZV(LM),ZHM(L,IS),ZTMP(L,IS),
     &                   XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                   TC1,  TI(LM),TV(LM),THM(L,IS),TTMP(L,IS),
     &                   XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                   HYDLMT,HYDLT,XMISS,BLM,BL,XMISS,GOOD)
          IF(ABS(ZC1).LT.ZCLIM) THEN
            IHSC(2,LM) = 0                       !Corrs too small
            ZC1 = 0.
            QMZ = 1.
          ELSEIF(GOOD(6,1,1) .AND. ABS(ZC1).GE.ZCLIM) THEN
            QMZ = 1.                             !Good corr
          ELSE
            ZC1 = 0.                             !Bad corr, but large
            QMZ = 13.                            !Flag value
          ENDIF
          IF(ABS(TC1).LT.TCLIM) THEN
            IHSC(3,LM) = 0                       !Corr too small
            TC1 = 0.
            QMT = 1.
          ELSEIF(GOOD(6,1,2) .AND. ABS(TC1).GE.TCLIM) THEN
            QMT = 1.                             !Good corr
          ELSE
            TC1 = 0.                             !Bad corr, but large
            QMT = 13.                            !Flag value
          ENDIF
        ELSE                             !Disagreement among residuals
          ZC1 = ZCBEST(1)                        !Try best corrs
          TC1 = TCBEST(1)
          CALL ROUND(ZC1,POB(LM),2)
          CALL ROUND(TC1,POB(LM),3)
          PZCOR = ZC1
          PTCOR = TC1
          CALL CHECKIT(3,ZC1,  ZDIF,ZV(LM),ZHM(L,IS),ZTMP(L,IS),
     &                   XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                   TC1,  TI(LM),TV(LM),THM(L,IS),TTMP(L,IS),
     &                   XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                   HYDLMT,HYDLT,XMISS,BLM,BL,XMISS,GOOD)
          ZC0 = ZC1
          TC0 = TC1
          IF(GOOD(6,1,1) .AND. GOOD(6,1,2)) THEN   !Best corrs look good
            CALL SIMPLE(ZC1,ZOB(LM),ALLZ,NALLZ,1.)
            CALL ROUND(ZC1,POB(LM),2)
            CALL SIMPLE(TC1,TOB(LM),ALLT,NALLT,10.)
            CALL ROUND(TC1,POB(LM),3)
            PZCOR = ZC1
            PTCOR = TC1
            CALL CHECKIT(3,ZC1,  ZDIF,ZV(LM),ZHM(L,IS),ZTMP(L,IS),
     &                     XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                     TC1,  TI(LM),TV(LM),THM(L,IS),TTMP(L,IS),
     &                     XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                     HYDLMT,HYDLT,XMISS,BLM,BL,XMISS,GOOD)
            IF(GOOD(6,1,1) .AND. GOOD(6,1,2)) THEN   !Try simple corrs
              QMZ = 1.
              QMT = 1.
              IF(ABS(ZC1).LT.ZCLIM) THEN
                IHSC(2,LM) = 0                    !Corr too small
                ZC1 = 0.
                QMZ = 1.
              ENDIF
              IF(ABS(TC1).LT.TCLIM) THEN
                IHSC(3,LM) = 0                    !Corr too small
                TC1 = 0.
                QMT = 1.
              ENDIF
              ZCOR(1) = ZC1
              TCOR(1) = TC1
            ELSE                            !Simple corrs not both good
              QMZ = 1.
              QMT = 1.
              ZC1 = ZC0                     !Revert to non-simple corrs
              TC1 = TC0
              IF(ABS(ZC1).LT.ZCLIM) THEN
                IHSC(2,LM) = 0              !Corr too small
                ZC1 = 0.
                QMZ = 1.
              ENDIF
              IF(ABS(TC1).LT.TCLIM) THEN
                IHSC(3,LM) = 0              !Corr too small
                TC1 = 0.
                QMT = 1.
              ENDIF
            ENDIF
          ELSE                                !Check z,T separately
            CALL CHECKIT(3,ZC1,  ZDIF,ZV(LM),ZHM(L,IS),ZTMP(L,IS),
     &                     XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                     XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                     XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                     XMISS,XMISS,XMISS,XMISS,XMISS,XMISS,GOOD)
            IF(GOOD(6,1,1)) THEN
              ZC0 = ZC1
              CALL SIMPLE(ZC1,ZOB(LM),ALLZ,NALLZ,1.)
              CALL ROUND(ZC1,POB(LM),2)
              PZCOR = ZC1
              CALL CHECKIT(3,ZC1,  ZDIF,ZV(LM),ZHM(L,IS),ZTMP(L,IS),
     &                       XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                       XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                       XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                       XMISS,XMISS,XMISS,XMISS,XMISS,XMISS,GOOD)
              IF(GOOD(6,1,1)) THEN           !Simple z-corr is good
                QMZ = 1.
                IF(ABS(ZC1).LT.ZCLIM) THEN
                  IHSC(2,LM) = 0             !Corr too small
                  ZC1 = 0.
                  QMZ = 1.
                ENDIF
                ZCOR(1) = ZC1
              ELSE                           !Simple corr is not good
                QMZ = 1.
                ZC1 = ZC0                    !Revert to non-simple corr
                PZCOR = ZC1
                IF(ABS(ZC1).LT.ZCLIM) THEN
                  IHSC(2,LM) = 0             !Corr too small
                  ZC1 = 0.
                  QMZ = 1.
                ENDIF
              ENDIF
            ENDIF
            CALL CHECKIT(3,XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                     XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                     TC1,  TI(LM),TV(LM),THM(L,IS),TTMP(L,IS),
     &                     XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                     XMISS,XMISS,XMISS,XMISS,XMISS,XMISS,GOOD)
            IF(GOOD(6,1,2)) THEN
              TC0 = TC1
              CALL SIMPLE(TC1,TOB(LM),ALLT,NALLT,10.)
              CALL ROUND(TC1,POB(LM),3)
              PTCOR = TC1
              CALL CHECKIT(3,XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                       XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                       TC1,  TI(LM),TV(LM),THM(L,IS),TTMP(L,IS),
     &                       XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                       XMISS,XMISS,XMISS,XMISS,XMISS,XMISS,GOOD)
              IF(GOOD(6,1,2)) THEN
                QMT = 1.
                IF(ABS(TC1).LT.TCLIM) THEN
                  IHSC(3,LM) = 0                !Corr too small
                  TC1 = 0.
                  QMT = 1.
                ENDIF
                TCOR(1) = TC1
              ELSE
                TC1 = TC0
                PTCOR = TC1
                QMT = 1.
                IF(ABS(TC1).LT.TCLIM) THEN
                  IHSC(3,LM) = 0
                  TC1 = 0.
                  QMT = 1.
                ENDIF
              ENDIF
            ENDIF
            IHSC(2,LM) = 0
            IHSC(3,LM) = 0
            ZC1 = 0.                           !Neither corr is good
            TC1 = 0.
            QMZ = 1.
            QMT = 1.
          ENDIF
        ENDIF

        IF(IHSC(2,LM).NE.0) THEN
          CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),2,
     &      ZC1,ZOB(LM)+ZC1,PZCOR,QMZ,IHSC(2,LM),XMISS,XMISS,XMISS)
        ENDIF
        IF(IHSC(3,LM).NE.0) THEN
          CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),3,
     &      TC1,TOB(LM)+TC1,PTCOR,QMT,IHSC(3,LM),XMISS,XMISS,XMISS)
          SDMQ = .FALSE.
          SDMQ = QQM(LM).EQ.0. .OR. QQM(LM).GE.14. .OR.
     &        (QQM(LM).GE.4. .AND. QQM(LM).LE.12.)
          IF((QMT.EQ.3. .OR. QMT.EQ.13.) .AND. QOB(LM).LT.BMISS .AND.
     &       .NOT.SDMQ) THEN
            CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),5,
     &        0.,QOB(LM),0.,QMT,IHSC(4,LM),XMISS,XMISS,XMISS)
          ENDIF
        ENDIF

      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ERR5        DETERMINE ERRORS AT THE TOP LEVEL
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Determine errors of type 5: top mandatory level.  Diagnosis
C   is for surface types 131: mand, middle, lower hole boundary, and
C   type 140: mandatory, top.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL ERR5(L,LM)
C   INPUT ARGUMENT LIST:
C     L        - LEVEL INDEX WITHIN MANDATORY LEVELS
C     LM       - LEVEL INDEX WITHIN ALL REPORTED LEVELS
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE ERR5(L,LM)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /TMPSND/  ZOBT(21,NST,4), TOBT(21,NST,4),
     &                 ZIT(21,NST,4),  TIT(21,NST,4),
     &                 ZTMP(21,NST),   TTMP(21,NST),
     &                 NZTMP(21,NST),  NTTMP(21,NST),
     &                 ITERR(4)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /ALLOW/   ALLZ(41), ALLT(71), NALLZ, NALLT
      COMMON /STN/     IS
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST
      CHARACTER*8 SID
      LOGICAL POUT, GOOD(6,2,2)
      LOGICAL ZG, TG, TDG, QG, ERROR, PRNT

      IF(TEST) WRITE(60,500) SID(IS)
  500 FORMAT(' ERR5--CALLED FOR: ',A8)

C  TYPE 131: MAND,MIDDLE, LOWER HOLE BOUNDARY AND
C  TYPE 140: MAND, TOP
C  -------------------------------------------------
C  CALCULATE THE ERROR, BASED UPON ONLY HEIGHT ERROR
C  OR ONLY TEMPERATURE ERROR.
C  -------------------------------------------------

      IF(HYDN(L-1).NE.BMISS) THEN
        ZCOR(1) = -HYDS(L-1)
        TCOR(1) = HYDN(L-1)/B(L-1)
      ELSEIF(HYDN(L-2).NE.BMISS) THEN
        ZCOR(1) = -HYDS(L-2)
        TCOR(1) = HYDN(L-2)/B(L-2)
      ENDIF

C  CALCULATE THE STATISTICALLY BEST COR FROM OTHER RESIDUALS
C  ---------------------------------------------------------

      IF(NZI(LM).LT.6) THEN
        ZDIF = ZI(LM)
      ELSE
        ZDIF = ZD(LM)
      ENDIF
      CALL BEST(ZCBEST(1),XMISS,XMISS,ZDIF,
     &  XINC(L,1),ZV(LM),VOIRES(L,1),ZHM(L,IS),HOIRES(L,1),
     &  ZTMP(L,IS),TMPSTD(L,1))
      CALL BEST(TCBEST(1),XMISS,XMISS,TI(LM),
     &  XINC(L,2),TV(LM),VOIRES(L,2),THM(L,IS),HOIRES(L,2),
     &  TTMP(L,IS),TMPSTD(L,2))

      IF(TEST) WRITE(60,501) ZCOR(1),TCOR(1),ZCBEST(1),TCBEST(1)
  501 FORMAT(' ERR5--ZCOR,TCOR,ZCBEST,TCBEST: ',4F10.1)

C  SEE IF HYDROSTATIC AND BEST CORS FIT A HEIGHT ERROR
C  ---------------------------------------------------

      ZR = ABS(ZCOR(1)-ZCBEST(1))/XINC(L,1)
      IF(ZR.LT.0.25) THEN
        ZC = ZCOR(1)
        CALL SIMPLE(ZC,ZOB(LM),ALLZ,NALLZ,1.)
        CALL ROUND(ZC,POB(LM),2)
        PZCOR = ZC
        IF(ABS(ZC).LT.ZCLIM) THEN
          ZC = 0.
          QM = 1.
          ZCOR(1) = ZC
        ELSE
          ZCOR(1) = ZC
          CALL CHECKIT(1,ZC,   ZD(LM),ZV(LM),ZHM(L,IS),ZTMP(L,IS),
     &                   XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                   XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                   XMISS,XMISS, XMISS, XMISS,    XMISS,
     &         HYDN(L-1),HYDN(L),XMISS,B(L-1),B(L),XMISS,GOOD)
          IF(GOOD(6,1,1)) THEN
            QM = 1.
            IHSC(2,LM) = 5
            POUT = .TRUE.
            CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &        2,ZC,ZOB(LM)+ZC,PZCOR,QM,IHSC(2,LM),XMISS,XMISS,XMISS)
            RETURN
          ENDIF
        ENDIF
      ENDIF

C  SEE IF HYDROSTATIC AND BEST CORS FIT A TEMPERATURE ERROR
C  --------------------------------------------------------

      TR = ABS(TCOR(1)-TCBEST(1))/XINC(L,2)
      IF(TR.LT.0.25) THEN
        TC = TCOR(1)
        CALL SIMPLE(TC,TOB(LM),ALLT,NALLT,10.)
        CALL ROUND(TC,POB(LM),3)
        PTCOR = TC
        IF(ABS(TC).LT.TCLIM) THEN
          TC = 0.
          QM = 0.
          TCOR(1) = TC
        ELSE
          TCOR(1) = TC
          CALL CHECKIT(2,XMISS,  XMISS,  XMISS,  XMISS,    XMISS,
     &                   XMISS,  XMISS,  XMISS,  XMISS,    XMISS,
     &                   TC,     TI(LM), TV(LM), THM(L,IS),TTMP(L,IS),
     &                   XMISS,  XMISS,  XMISS,  XMISS,    XMISS,
     &         HYDN(L-1),HYDN(L),XMISS,B(L-1),B(L),XMISS,GOOD)
          IF(GOOD(6,1,2)) THEN
            QM = 1.
            IHSC(3,LM) = 5
            POUT = .TRUE.
            CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &        3,TC,TOB(LM)+TC,PTCOR,QM,IHSC(3,LM),XMISS,XMISS,XMISS)
            RETURN
          ENDIF
        ENDIF
      ENDIF

C  TRY TYPE 3 CORRECTION
C  ---------------------

      ZC = ZCBEST(1)
      IF(ZC.NE.BMISS) THEN
        CALL SIMPLE(ZC,ZOB(LM),ALLZ,NALLZ,1.)
        CALL ROUND(ZC,POB(LM),2)
      ENDIF
      PZCOR = ZC
      TC = TCBEST(1)
      IF(TC.NE.BMISS) THEN
        CALL SIMPLE(TC,TOB(LM),ALLT,NALLT,10.)
        CALL ROUND(TC,POB(LM),3)
      ENDIF
      PTCOR = TC
      IF(ZC.NE.BMISS .AND. TC.NE.BMISS .AND.
     &  ABS(ZC).GT.ZCLIM .AND. ABS(TC).GT.TCLIM) THEN
        ZCOR(1) = ZC
        TCOR(1) = TC
        CALL CHECKIT(3,ZC,   ZD(LM),ZV(LM),ZHM(L,IS),ZTMP(L,IS),
     &                 XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                 TC,   TI(LM),TV(LM),THM(L,IS),TTMP(L,IS),
     &                 XMISS,XMISS, XMISS, XMISS,    XMISS,
     &       HYDN(L-1),HYDN(L),XMISS,B(L-1),B(L),XMISS,GOOD)
      ELSEIF(ZC.NE.BMISS .AND. ABS(ZC).GT.ZCLIM) THEN
        TC = 0.
        CALL CHECKIT(3,ZC,   ZD(LM),ZV(LM),ZHM(L,IS),ZTMP(L,IS),
     &                 XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                 XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                 XMISS,XMISS, XMISS, XMISS,    XMISS,
     &       HYDN(L-1),HYDN(L),XMISS,B(L-1),B(L),XMISS,GOOD)
      ELSEIF(TC.NE.BMISS .AND. ABS(TC).GT.TCLIM) THEN
        ZC = 0.
        CALL CHECKIT(3,XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                 XMISS,XMISS, XMISS, XMISS,    XMISS,
     &                 TC,   TI(LM),TV(LM),THM(L,IS),TTMP(L,IS),
     &                 XMISS,XMISS, XMISS, XMISS,    XMISS,
     &       HYDN(L-1),HYDN(L),XMISS,B(L-1),B(L),XMISS,GOOD)
      ELSE
        ZC = 0.
        TC = 0.
        GOOD(6,1,1) = .FALSE.
        GOOD(6,1,2) = .FALSE.
      ENDIF
      IF(GOOD(6,1,1)) THEN
        QM = 1.
        IHSC(2,LM) = 5
        POUT = .TRUE.
        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &    2,ZC,ZOB(LM)+ZC,PZCOR,QM,IHSC(2,LM),XMISS,XMISS,XMISS)
      ENDIF
      IF(GOOD(6,1,2)) THEN
        QM = 1.
        IHSC(3,LM) = 5
        POUT = .TRUE.
        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &    3,TC,TOB(LM)+TC,PTCOR,QM,IHSC(3,LM),XMISS,XMISS,XMISS)
      ENDIF
      IF(GOOD(6,1,1) .OR. GOOD(6,1,2)) RETURN

C  OTHERWISE, FLAG BOTH AS QUESTIONABLE OR BAD
C  -------------------------------------------

      IHSC(2,LM) = 5
      IHSC(3,LM) = 5
      IF(ABS(ZCOR(1)).GT.XINC(L,1)) THEN
        QM = 13.
        ZC = 0.
        ZCOR(1) = 0.
        POUT = .TRUE.
        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &    2,ZC,ZOB(LM)+ZC,PZCOR,QM,IHSC(2,LM),XMISS,XMISS,XMISS)
      ELSE
C       QM = 3.
C       ZC = 0.
C       ZCOR(1) = 0.
C       POUT = .TRUE.
C       CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
C    &    2,ZC,ZOB(LM)+ZC,PZCOR,QM,IHSC(2,LM),XMISS,XMISS,XMISS)
      ENDIF

      IF(ABS(TCOR(1)).GT.XINC(L,2)) THEN
        QM = 13.
        TC = 0.
        TCOR(1) = 0.
        POUT = .TRUE.
        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &    3,TC,TOB(LM)+TC,PTCOR,QM,IHSC(3,LM),XMISS,XMISS,XMISS)
      ELSE
C       QM = 3.
C       TC = 0.
C       TCOR(1) = 0.
C       POUT = .TRUE.
C       CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
C    &    3,TC,TOB(LM)+TC,PTCOR,QM,IHSC(3,LM),XMISS,XMISS,XMISS)
      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ERR710      PERFORM ERROR ANALYSIS FOR TYPES 7-10.
C   PRGMMR: KEYSER         ORG: NP22       DATE: 2013-02-05
C
C ABSTRACT: Perform error analysis for errors at adjacent levels:
C   2 heights, 2 temperatures, or 1 temperature and C1 height error.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2008-10-08  Woollen/Keyser -- corrected if tests where integer values
C     were tested for not being equal to real value for missing (BMISS
C     = 10E10), these integer values can never be equal to BMISS for
C     I*4 due to overflow - instead they are now tested for not being
C     equal to new integer value for missing (IMISS, also = 10E10),
C     although this is also an overflow value for I*4, it results in a
C     correct evaluation
C 2013-02-05  D. Keyser -- Final changes to run on WCOSS: Set BUFRLIB
C     missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C     overflow (also done for IMISS).
C
C USAGE:    CALL ERR710(L,LM)
C   INPUT ARGUMENT LIST:
C     L        - LEVEL INDEX WITHIN MANDATORY LEVELS
C     LM       - LEVEL INDEX WITHIN ALL REPORTED LEVELS
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE ERR710(L,LM)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /ALLOW/   ALLZ(41), ALLT(71), NALLZ, NALLT
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /TMPSND/  ZOBT(21,NST,4), TOBT(21,NST,4),
     &                 ZIT(21,NST,4),  TIT(21,NST,4),
     &                 ZTMP(21,NST),   TTMP(21,NST),
     &                 NZTMP(21,NST),  NTTMP(21,NST),
     &                 ITERR(4)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /CONSTS/  R, G, T0, CP, RV
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST
      CHARACTER*8 SID
      LOGICAL POUT, GOOD(6,2,2)
      LOGICAL C2, C3, C5, C6
      LOGICAL ZG, TG, TDG, QG, ERROR, PRNT, SDMQ

      IF(TEST) WRITE(60,500) SID(IS), L ,LM
  500 FORMAT(' ERR710--CALLED FOR: ',A8,'  WITH L,LM:',2I4)

C  DETERMINE APPROPRIATE HYDROSTATIC RESIDUAL, B TO USE
C  ----------------------------------------------------

      IF(B(L).EQ.BMISS .OR. B(L+1).EQ.BMISS .OR.
     &   B(L).EQ.0. .OR. B(L+1).EQ.0.) RETURN

      IF(LST(LM).EQ.130) THEN
        IF(HYDN(L-1).NE.BMISS) THEN
          HYDLM = HYDN(L-1)
          BLM = B(L-1)
        ELSEIF(L.GT.2) THEN
          IF(HYDN(L-2).NE.BMISS) THEN
            HYDLM = HYDN(L-2)
            BLM = B(L-2)
          ELSE
            HYDLM = BMISS
            BLM = BMISS
          ENDIF
        ELSE
          HYDLM = BMISS
          BLM = BMISS
        ENDIF
      ELSEIF(LST(LM).EQ.120) THEN
        HYDLM = BASRES(IS)
        IF(POB(LM).NE.0.) THEN
          BLM = 0.5*(R/G)*ALOG(PS(IS)/POB(LM))
        ELSE
          BLM = BMISS
        ENDIF
        IF(BLM.LT.0.01) BLM = BMISS
      ELSE
        RETURN
      ENDIF
      IF(TEST) WRITE(60,566) HYDLM,BLM,B(L),B(L+1)
  566 FORMAT(' ERR710--HYDLM,BLM,B(L),B(L+1): ',4F12.2)

C  DETERMINE THE MOST LIKELY ERROR TYPE
C  ------------------------------------

      R7 = ABS(HYDLM+HYDN(L)+HYDN(L+1))/(TT
     &      *SQRT(BLM**2+B(L)**2+B(L+1)**2))
      R8 = ABS(HYDLM/BLM-HYDN(L)/B(L)+HYDN(L+1)/B(L+1))
     &      /(SQRT(3.)*TT)
      R9 = ABS(HYDLM+HYDN(L)-(B(L)/B(L+1))*HYDN(L+1))
     &      /(TT*SQRT(BLM**2 + 2.*B(L)**2))
      R10 = ABS(HYDN(L)+HYDN(L+1)-(B(L)/BLM)*HYDLM)
     &      /(TT*SQRT(B(L+1)**2 + 2.*B(L)**2))

      C2 = .FALSE.
      C3 = .FALSE.
      C5 = .FALSE.
      C6 = .FALSE.
      IF(ABS(HYDLM).GT.TT*SQRT(BLM**2+B(L)**2))        C2 = .TRUE.
      IF(ABS(HYDN(L+1)).GT.TT*SQRT(B(L+1)**2+B(L)**2)) C3 = .TRUE.
      IF(BLM.NE.0.) THEN
      IF(ABS(HYDLM)/BLM.GT.TT)                         C5 = .TRUE.
      ENDIF
      IF(B(L+1).NE.0.) THEN
      IF(ABS(HYDN(L+1))/B(L+1).GT.TT)                  C6 = .TRUE.
      ENDIF

C  SEE WHICH TYPES ARE POSSIBLE
C  ----------------------------

      RAT = 1.
      IET = 0
      IF(R7.LT.1. .AND. C2 .AND. C3) THEN
        RAT = R7
        IET = 7
      ENDIF
      IF(R8.LT.1. .AND. C5 .AND. C6) THEN
        IF(R8.LT.RAT) THEN
          RAT = R8
          IET = 8
        ENDIF
      ENDIF
      IF(R9.LT.1. .AND. C2 .AND. C6) THEN
        IF(R9.LT.RAT) THEN
          RAT = R9
          IET = 9
        ENDIF
      ENDIF
      IF(R10.LT.1. .AND. C3 .AND. C5) THEN
        IF(R10.LT.RAT) THEN
          RAT = R10
          IET = 10
        ENDIF
      ENDIF

      IF(TEST) WRITE(60,510) IET,R7,R8,R9,R10,C2,C3,C5,C6,HYDLM,BLM
  510 FORMAT(' ERR710--IET,R7,R8,R9,R10,C2,C3,C5,C6: ',I4,4F10.2,4L3,
     &  '  HYDLM,BLM: ',F10.1,F10.4)

C  IET TELLS WHICH ERROR TYPE IS MOST LIKELY
C  T-S TELL WHICH TYPE CRITERIA ARE SATISFIED
C  ------------------------------------------

      IF(IET.EQ.0 .AND. HYDN(L).NE.BMISS) CALL ERR123(L,LM)

C  CALCULATE HYDROSTATIC CORRECTIONS
C  ---------------------------------

      LP  = L+1
      LMP = LMAND(LP)
      B1  = 1./BLM
      B1S = B1**2
      B2  = 1./B(L)
      B2S = B2**2
      B3  = 1./B(L+1)
      B3S = B3**2
      S1  = HYDLM
      S2  = HYDN(L)
      S3  = HYDN(L+1)
      IF(IET.EQ.7) THEN
        ZCOR(1) = (-S1*B1S*(B2S+B3S) + (S2+S3)*B2S*B3S)
     &    /((B1S+B3S)*B2S + B1S*B3S)
        ZCOR(2) = (-(S1+S2)*B1S*B2S + S3*B3S*(B1S+B2S))
     &    /((B1S+B3S)*B2S + B1S*B3S)
      ELSEIF(IET.EQ.8) THEN
        TCOR(1) = (2.*S1*B1+S2*B2-S3*B3)/3.
        TCOR(2) = (2.*S3*B3+S2*B2-S1*B1)/3.
      ELSEIF(IET.EQ.9) THEN
        ZCOR(1) = (S2*B2S-2.*S1*B1S-S3*B3*B2)/(2.*B1S+B2S)
        TCOR(2) = (S2*B2*B1S+S1*B1S*B2+S3*B3*(B1S+B2S))
     &    /(2.*B1S+B2S)
      ELSEIF(IET.EQ.10) THEN
        TCOR(1) = (S1*B1*(B2S+B3S)+S2*B2*B3S+S3*B2*B3S)
     &    /(B2S+2.*B3S)
        ZCOR(2) = (S1*B1*B2+2.*S3*B3S-S2*B2S)/(B2S+2.*B3S)
      ENDIF

C  CALCULATE BEST CORRECTIONS FOR COMPARISON
C  -----------------------------------------

      IF(NZI(LM).LT.6) THEN
        ZDIF = ZI(LM)
      ELSE
        ZDIF = ZD(LM)
      ENDIF
cdak  IF(LMP.NE.BMISS) THEN
      IF(LMP.NE.IMISS) THEN ! LMP will never test = bmiss, use imiss
        IF(NZI(LMP).LT.6) THEN
          ZDIFP = ZI(LMP)
        ELSE
          ZDIFP = ZD(LMP)
        ENDIF
      ELSE
        ZDIFP = BMISS
      ENDIF
      IF(IET.EQ.7) THEN
        CALL BEST(ZCBEST(1),XMISS,XMISS,ZI(LM),XINC(L,1),
     &    XMISS,XMISS,ZHM(L,IS),HOIRES(L,1),
     &    ZTMP(L,IS),TMPSTD(L,1))
        CALL BEST(ZCBEST(2),XMISS,XMISS,ZI(LMP),XINC(LP,1),
     &    XMISS,XMISS,ZHM(LP,IS),HOIRES(LP,1),
     &    ZTMP(LP,IS),TMPSTD(LP,1))
      ELSEIF(IET.EQ.8) THEN
        CALL BEST(TCBEST(1),XMISS,XMISS,TI(LM),
     &    XINC(L,2),XMISS,XMISS,THM(L,IS),HOIRES(L,2),
     &    TTMP(L,IS),TMPSTD(L,2))
        CALL BEST(TCBEST(2),XMISS,XMISS,TI(LMP),
     &    XINC(LP,2),XMISS,XMISS,THM(LP,IS),HOIRES(LP,2),
     &    TTMP(LP,IS),TMPSTD(LP,2))
      ELSEIF(IET.EQ.9) THEN
        CALL BEST(ZCBEST(1),XMISS,XMISS,ZDIF,XINC(L,1),
     &    ZV(LM),VOIRES(L,1),ZHM(L,IS),HOIRES(L,1),
     &    ZTMP(L,IS),TMPSTD(L,1))
        CALL BEST(TCBEST(2),XMISS,XMISS,TI(LMP),
     &    XINC(LP,2),TV(LMP),VOIRES(LP,2),THM(LP,IS),HOIRES(LP,2),
     &    TTMP(LP,IS),TMPSTD(LP,2))
      ELSEIF(IET.EQ.10) THEN
        CALL BEST(TCBEST(1),XMISS,XMISS,TI(LM),
     &    XINC(L,2),TV(LM),VOIRES(L,2),THM(L,IS),HOIRES(L,2),
     &    TTMP(L,IS),TMPSTD(L,2))
        CALL BEST(ZCBEST(2),XMISS,XMISS,ZDIFP,XINC(LP,1),
     &    ZV(LMP),VOIRES(LP,1),ZHM(LP,IS),HOIRES(LP,1),
     &    ZTMP(LP,IS),TMPSTD(LP,1))
      ENDIF

C  TEST TO SEE WHICH CORRECTIONS TO USE, IF ANY
C  CONSIDER EACH POTENTIAL CORRECTION SEPARATELY
C  ---------------------------------------------
C  IN THE (NEAR) FUTURE, THE LOGIC SHOULD BE EXPANDED SO THAT WHEN
C  A PART OF THE POTENTIAL CORRECTION IS REJECTED, THEN THE
C  SUGGESTED CORRECTION WILL BE RECALCULATED USING ONLY THE
C  RELEVANT HYDROSTATIC RESIDUALS.
C  QUESTION: SHOULD BEST BE RECALCULATED USING VERTICAL RESIDS?
C  ------------------------------------------------------------

C  TYPE 7 CORRECTIONS
C  ------------------

      IF(IET.EQ.7) THEN

        POUT = .TRUE.
        IHSC(2,LM) = 7
        IHSC(2,LMP) = 7
        RZ1 = ABS(ZCOR(1)-ZCBEST(1))/XINC(L,1)
        RZ2 = ABS(ZCOR(2)-ZCBEST(2))/XINC(L+1,1)

        IF(RZ1.LT.0.15 .AND. RZ2.LT.0.15) THEN
          ZC1 = ZCOR(1)
          CALL SIMPLE(ZC1,ZOB(LM),ALLZ,NALLZ,1.)
          CALL ROUND(ZC1,POB(LM),2)
          PZCOR1 = ZC1
          IF(ABS(ZC1).LT.ZCLIM) ZC1 = 0.
          ZCOR(1) = ZC1
          QM1 = 1.
          ZC2 = ZCOR(2)
          CALL SIMPLE(ZC2,ZOB(LMP),ALLZ,NALLZ,1.)
          CALL ROUND(ZC2,POB(LMP),2)
          PZCOR2 = ZC2
          IF(ABS(ZC2).LT.ZCLIM) ZC2 = 0.
          ZCOR(2) = ZC2
          QM2 = 1.
        ELSE
          ZC1  = ZCBEST(1)
          ZC2  = ZCBEST(2)
          CALL SIMPLE(ZC1,ZOB(LM),ALLZ,NALLZ,1.)
          CALL ROUND(ZC1,POB(LM),2)
          CALL SIMPLE(ZC2,ZOB(LMP),ALLZ,NALLZ,1.)
          CALL ROUND(ZC2,POB(LMP),2)
          PZCOR1 = ZC1
          PZCOR2 = ZC2
          IF(ABS(ZC1).LT.0.25*XINC(L,1)) ZC1 = 0.
          IF(ABS(ZC2).LT.0.25*XINC(L+1,1)) ZC2 = 0.
          CALL CHECKIT(7,ZC1,  ZDIF, XMISS,ZHM(L,IS),  ZTMP(L,IS),
     &                   ZC2,  ZDIFP,XMISS,ZHM(L+1,IS),ZTMP(L+1,IS),
     &                   XMISS,XMISS,XMISS,XMISS,      XMISS,
     &                   XMISS,XMISS,XMISS,XMISS,      XMISS,
     &                   HYDLM,HYD(L),XMISS,BLM,B(L),XMISS,GOOD)
          IF(TEST) WRITE(60,501) ZC1, ZI(LM), ZV(LM), ZHM(L,IS),
     &               ZTMP(L,IS),ZC2, ZI(LMP),ZV(LMP),ZHM(L+1,IS),
     &               ZTMP(L+1,IS),
     &               HYDLM,HYD(L),BLM,B(L),
     &               ((GOOD(I,J,1),I=1,5),J=1,2)
  501     FORMAT(' ERR710--(TYPE 7) ZC1,ZI(LM),ZV(LM),ZHM(L,IS),',
     &           'ZTMP(L,IS),ZC2,ZI(LMP),ZV(LMP),ZHM(L+1,IS),',
     &           'ZTMP(L+1,IS),HYDLM,HYD(L),BLM,B(L),GOOD(z1,z2):'/
     &           9X,14F10.1,5L4,2X,5L4)

          IF(GOOD(6,1,1)) THEN
            QM1 = 1.
          ELSE
            IF(ZC1.EQ.0.) THEN
              QM1 = 3.
            ELSE
              ZC1 = 0.
              QM1 = 13.
            ENDIF
          ENDIF
          IF(GOOD(6,2,1)) THEN
            QM2 = 1.
          ELSE
            IF(ZC2.EQ.0.) THEN
              QM2 = 3.
            ELSE
              ZC2 = 0.
              QM2 = 13.
            ENDIF
          ENDIF
        ENDIF

        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),2,ZC1,
     &    ZOB(LM)+ZC1,PZCOR1,QM1,IHSC(2,LM),XMISS,XMISS,XMISS)
        CALL SEVENT(SID(IS),SQN(IS),LMP,LST(LMP),POB(LMP),LEVTYP(LMP),
     &    2,ZC2,ZOB(LMP)+ZC2,PZCOR2,QM2,IHSC(2,LMP),XMISS,XMISS,XMISS)

C  TYPE 8 CORRECTIONS
C  ------------------

      ELSEIF(IET.EQ.8) THEN

        POUT = .TRUE.
        IHSC(3,LM) = 8
        IHSC(3,LMP) = 8
        RT1 = ABS(TCOR(1)-TCBEST(1))/XINC(L,2)
        RT2 = ABS(TCOR(2)-TCBEST(2))/XINC(L+1,2)

        IF(RT1.LT.0.15 .AND. RT2.LT.0.15) THEN
          TC1 = TCOR(1)
          CALL SIMPLE(TC1,TOB(LM),ALLT,NALLT,10.)
          CALL ROUND(TC1,POB(LM),3)
          PTCOR1 = TC1
          IF(ABS(TC1).LT.0.25*XINC(L,2)) TC1 = 0.
          TCOR(1) = TC1
          QM1 = 1.
          TC2 = TCOR(2)
          CALL SIMPLE(TC2,TOB(LMP),ALLT,NALLT,10.)
          CALL ROUND(TC2,POB(LMP),3)
          PTCOR2 = TC2
          IF(ABS(TC2).LT.0.25*XINC(L+1,2)) TC2 = 0.
          TCOR(2) = TC2
          QM2 = 1.
        ELSE
          TC1  = TCBEST(1)
          TC2  = TCBEST(2)
          CALL SIMPLE(TC1,TOB(LM),ALLT,NALLT,10.)
          CALL ROUND(TC1,POB(LM),3)
          CALL SIMPLE(TC2,TOB(LMP),ALLT,NALLT,10.)
          CALL ROUND(TC2,POB(LMP),3)
          PTCOR1 = TC1
          PTCOR2 = TC2
          IF(ABS(TC1).LT.TCLIM) TC1 = 0.
          IF(ABS(TC2).LT.TCLIM) TC2 = 0.
          CALL CHECKIT(8,XMISS,XMISS,  XMISS,XMISS,      XMISS,
     &                   XMISS,XMISS,  XMISS,XMISS,      XMISS,
     &                   TC1,  TI(LM), XMISS,THM(L,IS),  TTMP(L,IS),
     &                   TC2,  TI(LMP),XMISS,THM(L+1,IS),TTMP(L+1,IS),
     &                   HYDLM,HYDN(L),XMISS,BLM,B(L),XMISS,GOOD)
          IF(TEST) WRITE(60,502) TC1,TI(LM),TV(LM),THM(L,IS),
     &               TTMP(L,IS),TC2,TI(LMP),TV(LMP),THM(L+1,IS),
     &               TTMP(L+1,IS),HYDLM,HYDN(L),BLM,B(L),
     &               ((GOOD(I,J,2),I=1,5),J=1,2)
  502     FORMAT(' ERR710--(TYPE 8) TC1,TI(LM),TV(LM),THM(L,IS),',
     &               'TTMP(L,IS),TC2,TI(LMP),TV(LMP),THM(L+1,IS),',
     &               'TTMP(L+1,IS),HYDLM,HYDN(L),BLM,B(L),GOOD(T1,T2):'/
     &               9X,14F10.1,5L4,2X,5L4)
          IF(GOOD(6,1,2)) THEN
            QM1 = 1.
          ELSE
            IF(TC1.EQ.0.) THEN
              QM1 = 3.
            ELSE
              TC1 = 0.
              QM1 = 13.
            ENDIF
          ENDIF
          IF(GOOD(6,2,2)) THEN
            QM2 = 1.
          ELSE
            IF(TC2.EQ.0.) THEN
              QM2 = 3.
            ELSE
              TC2 = 0
              QM2 = 13.
            ENDIF
          ENDIF
        ENDIF

        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),3,TC1,
     &    TOB(LM)+TC1,PTCOR1,QM1,IHSC(3,LM),XMISS,XMISS,XMISS)
        CALL SEVENT(SID(IS),SQN(IS),LMP,LST(LMP),POB(LMP),LEVTYP(LMP),
     &    3,TC2,TOB(LMP)+TC2,PTCOR2,QM2,IHSC(3,LMP),XMISS,XMISS,XMISS)
        SDMQ = .FALSE.
        SDMQ = QQM(LM).EQ.0. .OR. QQM(LM).GE.14. .OR.
     &        (QQM(LM).GE.4. .AND. QQM(LM).LE.12.)
        IF((QM1.EQ.3. .OR. QM1.EQ.13.) .AND. QOB(LM).LT.BMISS .AND.
     &     .NOT.SDMQ) THEN
          CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),5,
     &      0.,QOB(LM),0.,QM1,IHSC(4,LM),XMISS,XMISS,XMISS)
        ENDIF
        SDMQ = .FALSE.
        SDMQ = QQM(LMP).EQ.0. .OR. QQM(LMP).GE.14. .OR.
     &        (QQM(LMP).GE.4. .AND. QQM(LMP).LE.12.)
        IF((QM2.EQ.3. .OR. QM2.EQ.13.) .AND. QOB(LMP).LT.BMISS .AND.
     &     .NOT.SDMQ) THEN
          CALL SEVENT(SID(IS),SQN(IS),LMP,LST(LMP),POB(LMP),
     &      LEVTYP(LMP),5,0.,QOB(LMP),0.,QM2,IHSC(4,LMP),XMISS,
     &      XMISS,XMISS)
        ENDIF

C  TYPE 9 CORRECITONS
C  ------------------

      ELSEIF(IET.EQ.9) THEN

        POUT = .TRUE.
        IHSC(2,LM) = 9
        IHSC(3,LMP) = 9
        RZ1 = ABS(ZCOR(1)-ZCBEST(1))/XINC(L,1)
        RT2 = ABS(TCOR(2)-TCBEST(2))/XINC(L+1,2)

        IF(RZ1.LT.0.15 .AND. RT2.LT.0.15) THEN
          ZC1 = ZCOR(1)
          CALL SIMPLE(ZC1,ZOB(LM),ALLZ,NALLZ,1.)
          CALL ROUND(ZC1,POB(LM),2)
          PZCOR1 = ZC1
          IF(ABS(ZC1).LT.0.25*XINC(L,1)) ZC1 = 0.
          ZCOR(1) = ZC1
          QM1 = 1.
          TC2 = TCOR(2)
          CALL SIMPLE(TC2,TOB(LMP),ALLT,NALLT,10.)
          CALL ROUND(TC2,POB(LMP),3)
          PTCOR2 = TC2
          IF(ABS(TC2).LT.0.25*XINC(L+1,2)) TC2 = 0.
          TCOR(2) = TC2
          QM2 = 1.
        ELSE
          ZC1  = ZCBEST(1)
          TC2  = TCBEST(2)
          CALL SIMPLE(ZC1,ZOB(LM),ALLZ,NALLZ,1.)
          CALL ROUND(ZC1,POB(LM),2)
          CALL SIMPLE(TC2,TOB(LMP),ALLT,NALLT,10.)
          CALL ROUND(TC2,POB(LMP),3)
          PZCOR1 = ZC1
          TCOR(2) = TC2
          IF(ABS(ZC1).LT.ZCLIM) ZC1 = 0.
          IF(ABS(TC2).LT.TCLIM) TC2 = 0.
          CALL CHECKIT(9,ZC1,  ZDIF,   ZV(LM), ZHM(L,IS),  ZTMP(L,IS),
     &                   XMISS,XMISS,  XMISS,  XMISS,      XMISS,
     &                   XMISS,XMISS,  XMISS,  XMISS,      XMISS,
     &                   TC2,  TI(LMP),TV(LMP),THM(L+1,IS),TTMP(L+1,IS),
     &                   HYDLM,HYDN(L),XMISS,BLM,B(L),XMISS,GOOD)
          IF(TEST) WRITE(60,503) ZC1,ZI(LM),ZV(LM),ZHM(L,IS),
     &               ZTMP(L,IS),TC2,TI(LMP),TV(LMP),THM(L+1,IS),
     &               TTMP(L+1,IS),HYDLM,HYDN(L),BLM,B(L),
     &               (GOOD(I,1,1),I=1,5),(GOOD(J,2,2),J=1,5)
  503     FORMAT(' ERR710--(TYPE 9)  ZC1,ZI(LM),ZV(LM),ZHM(L,IS),',
     &               'ZTMP(L,IS),TC2,TI(LMP),TV(LMP),THM(L+1,IS),',
     &               'TTMP(L+1,IS),HYDLM,HYDN(L),BLM,B(L),GOOD(z1,T2):'/
     &               9X,14F10.1,5L4,2X,5L4)
          IF(GOOD(6,1,1)) THEN
            QM1 = 1.
          ELSE
            IF(ZC1.EQ.0.) THEN
              QM1 = 0.
            ELSE
              ZC1 = 0.
              QM1 = 13.
            ENDIF
          ENDIF
          IF(GOOD(6,2,2)) THEN
            QM2 = 1.
          ELSE
            IF(TC2.EQ.0.) THEN
              QM2 = 3.
            ELSE
              TC2 = 0
              QM2 = 13.
            ENDIF
          ENDIF
        ENDIF

        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),2,ZC1,
     &    ZOB(LM)+ZC1,PZCOR1,QM1,IHSC(2,LM),XMISS,XMISS,XMISS)
        CALL SEVENT(SID(IS),SQN(IS),LMP,LST(LMP),POB(LMP),LEVTYP(LMP),
     &    3,TC2,TOB(LMP)+TC2,PTCOR2,QM2,IHSC(3,LMP),XMISS,XMISS,XMISS)
        SDMQ = .FALSE.
        SDMQ = QQM(LMP).EQ.0. .OR. QQM(LMP).GE.14. .OR.
     &        (QQM(LMP).GE.4. .AND. QQM(LMP).LE.12.)
        IF((QM2.EQ.3. .OR. QM2.EQ.13.) .AND. QOB(LMP).LT.BMISS .AND.
     &     .NOT.SDMQ) THEN
          CALL SEVENT(SID(IS),SQN(IS),LMP,LST(LMP),POB(LMP),
     &      LEVTYP(LMP),5,0.,QOB(LMP),0.,QM2,IHSC(4,LMP),XMISS,
     &      XMISS,XMISS)
        ENDIF

C  TYPE 10 CORRECTIONS
C  -------------------

      ELSEIF(IET.EQ.10) THEN

        POUT = .TRUE.
        IHSC(3,LM) = 10
        IHSC(2,LMP) = 10
        RZ2 = ABS(ZCOR(2)-ZCBEST(2))/XINC(L+1,1)
        RT1 = ABS(TCOR(1)-TCBEST(1))/XINC(L,2)
        IF(RT1.LT.0.15 .AND. RZ2.LT.0.15) THEN
          TC1 = TCOR(1)
          CALL SIMPLE(TC1,TOB(LM),ALLT,NALLT,10.)
          CALL ROUND(TC1,POB(LM),3)
          PTCOR1 = TC1
          IF(ABS(TC1).LT.TCLIM) TC1 = 0.
          TCOR(1) = TC1
          QM1 = 1.
          ZC2 = ZCOR(2)
          CALL SIMPLE(ZC2,ZOB(LMP),ALLZ,NALLZ,1.)
          CALL ROUND(ZC2,POB(LMP),2)
          PZCOR2 = ZC2
          IF(ABS(ZC2).LT.ZCLIM) ZC2 = 0.
          ZCOR(2) = ZC2
          QM2 = 1.
        ELSE
          TC1  = TCBEST(1)
          ZC2  = ZCBEST(2)
          CALL SIMPLE(TC1,TOB(LM),ALLT,NALLT,10.)
          CALL ROUND(TC1,POB(LM),3)
          CALL SIMPLE(ZC2,ZOB(LMP),ALLZ,NALLZ,1.)
          CALL ROUND(ZC2,POB(LMP),2)
          PTCOR1 = TC1
          PZCOR2 = ZC2
          IF(ABS(TC1).LT.TCLIM) TC1 = 0.
          IF(ABS(ZC2).LT.ZCLIM) ZC2 = 0.
          CALL CHECKIT(10,XMISS,XMISS, XMISS,  XMISS,      XMISS,
     &                    ZC2,  ZDIFP, ZV(LMP),ZHM(L+1,IS),ZTMP(L+1,IS),
     &                    TC1,  TI(LM),TV(LM), THM(L,IS),  TTMP(L,IS),
     &                    XMISS, XMISS, XMISS, XMISS,      XMISS,
     &                    HYDLM,HYDN(L),XMISS,BLM,B(L),XMISS,GOOD)
          IF(TEST) WRITE(60,504) ZC2,ZI(LMP),ZV(LMP),ZHM(L+1,IS),
     &               ZTMP(L+1,IS),TC1,TI(LM),TV(LM),THM(L,IS),
     &               TTMP(L,IS),HYDLM,HYDN(L),BLM,B(L),
     &               (GOOD(I,1,2),I=1,5),(GOOD(J,2,1),J=1,5)
  504     FORMAT(' ERR710--(TYPE 10) ZC2,ZI(LMP),ZV(LMP),ZHM(L+1,IS),',
     &               'ZTMP(L+1,IS),TC1,TI(LM),TV(LM),THM(L,IS),',
     &               'TTMP(L,IS),HYDLM,HYDN(L),BLM,B(L),GOOD(T1,z2):'/
     &               9X,14F10.1,5L4,2X,5L4)
          IF(GOOD(6,1,2)) THEN
            QM1 = 1.
          ELSE
            IF(TC1.EQ.0.) THEN
              QM1 = 3.
            ELSE
              TC1 = 0.
              QM1 = 13.
            ENDIF
          ENDIF
          IF(GOOD(6,2,1)) THEN
            QM2 = 1.
          ELSE
            IF(ZC2.EQ.0.) THEN
              QM2 = 3.
            ELSE
              ZC2 = 0
              QM2 = 13.
            ENDIF
          ENDIF
        ENDIF

        CALL SEVENT(SID(IS),SQN(IS),LMP,LST(LMP),POB(LMP),LEVTYP(LMP),
     &    2,ZC2,ZOB(LMP)+ZC2,PZCOR2,QM2,IHSC(2,LMP),XMISS,XMISS,XMISS)
        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),3,TC1,
     &    TOB(LM)+TC1,PTCOR1,QM1,IHSC(3,LM),XMISS,XMISS,XMISS)
        SDMQ = .FALSE.
        SDMQ = QQM(LM).EQ.0. .OR. QQM(LM).GE.14. .OR.
     &        (QQM(LM).GE.4. .AND. QQM(LM).LE.12.)
        IF((QM1.EQ.3. .OR. QM1.EQ.13.) .AND. QOB(LM).LT.BMISS .AND.
     &     .NOT.SDMQ) THEN
          CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),
     &      LEVTYP(LM),5,0.,QOB(LM),0.,QM1,IHSC(4,LM),XMISS,
     &      XMISS,XMISS)
        ENDIF

      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ERRTYP      CHECK QC FOR APPROPRIATE LEVEL TYPE
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Call the appropriate subroutine for each level to check
C   the data quality.  See STYPE for a listing of these types.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL ERRTYP
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE ERRTYP
      PARAMETER (NST=1500)

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /TESTS/   TEST
      LOGICAL          TEST
      CHARACTER*8 SID
      LOGICAL POUT
      LOGICAL ZG, TG, TDG, QG, ERROR, PRNT

      IF(TEST) WRITE(60,503) SID(IS)
  503 FORMAT(' ERRTYP--CALLED FOR: ',A8)

C  PREFORM ANALYSIS, LAYER BY LAYER
C  --------------------------------

      DO I=1,2
        ZCOR(I)   = 0.
        TCOR(I)   = 0.
        ZCBEST(I) = 0.
        TCBEST(I) = 0.
      ENDDO
      H1   = 0.
      H2   = 0.
      T1   = 0.
      POUT = .FALSE.
      IF(ISC.EQ.1) THEN
        L1 = 1
      ELSE
        L1 = LAST+1
      ENDIF
      IF(L1.GT.NLEV) L1 = NLEV
      DO LM=L1,NLEV
        LS = LM
        IF(LM.LE.0) THEN
          WRITE(60,504) LM,NLEV
  504     FORMAT(' ERRTYP--PROBLEM! LM = ',I5,'  NLEV = ',I5)
          RETURN
        ENDIF
        L = MANLEV(POB(LM))
        LAST = LM

C  TYPE 240: SURFACE
C  -----------------

        IF(LST(LM).EQ.240) THEN
          CALL T240(L,LS)
          IF(POUT) GOTO 110

C  TYPE 120: MAND, FIRST ABOVE GROUND
C  ----------------------------------

        ELSEIF(LST(LM).EQ.120) THEN
          CALL T120(L,LS)
          IF(POUT) GOTO 110

C  TYPE 130: MAND, MIDDLE
C  ----------------------

        ELSEIF(LST(LM).EQ.130) THEN
          CALL T130(L,LS)
          IF(POUT) GOTO 110

C  TYPE 140: MAND, TOP, AND
C  TYPE 131: MAND,MIDDLE, LOWER HOLE BOUNDARY
C  ------------------------------------------

        ELSEIF(LST(LM).EQ.131 .OR. LST(LM).EQ.140) THEN
          CALL T140(L,LS)
          IF(POUT) GOTO 110

C  TYPE 132: MAND,MIDDLE, UPPER HOLE BOUNDARY
C  ------------------------------------------

        ELSEIF(LST(LM).EQ.132) THEN
          ! No hydrostatic correction

C  TYPE 142: MAND, TOP, UPPER HOLE BOUNDARY (ISOLATED) AND
C  TYPE 133: MAND, MIDDLE, ISOLATED
C  -------------------------------------------------------

        ELSEIF(LST(LM).EQ.142 .OR.LST(LM).EQ.133) THEN
          ! No hydrostatic correction

C  TYPE 121: MAND 1ST ABOVE GROUND, LOWER HOLE BOUNDARY (ISOLATED)
C  ---------------------------------------------------------------

        ELSEIF(LST(LM).EQ.121) THEN
          CALL T121(L,LS)
          IF(POUT) GOTO 110

C  TYPE 220: SIGNIFICANT LEVEL
C  ---------------------------

        ELSEIF(LST(LM).EQ.220) THEN
          CALL T220(L,LS)
          IF(POUT) GOTO 110

C  TYPE 224: SIGNIFICANT LEVEL, ABOVE TOP MAND LVL
C  -----------------------------------------------

        ELSEIF(LST(LM).EQ.220) THEN
          ! No hydrostatic correction

C  TYPE 150: INCOMPLETE MANDATORY LEVEL
C  ------------------------------------

        ELSEIF(LST(LM).EQ.150) THEN
          CALL T220(L,LS)
          IF(POUT) GOTO 110
        ENDIF
      ENDDO

  110 CONTINUE
      IF(POUT .AND. LM.LE.NLEV) THEN
        IF(TEST) WRITE(60,500) SID(IS)
        IF(TEST) WRITE(60,501)
        IF(TEST) WRITE(60,502) POB(LM),IHSC(1,LM),PSCOR,IHSC(2,LM),
     &    ZCOR(1),IHSC(3,LM),TCOR(1),LST(LM),ZCBEST(1),TCBEST(1)
        IF(L+1.LE.MAND) THEN
          LX = LMAND(L+1)
          IF(LX.LE.255) THEN
            IF(TEST) WRITE(60,502) POB(LX),IHSC(1,LX),0.,IHSC(2,LX),
     &        ZCOR(2),IHSC(3,LX),TCOR(2),LST(LX),ZCBEST(2),TCBEST(2)
          ENDIF
        ENDIF
      ENDIF
  500 FORMAT(//' ERRTYP--HYDROSTATIC DIAGNOSIS FOR STATION: ',A8)
  501 FORMAT('   PRESS IHSC-P    PCOR IHSC-Z    ZCOR IHSC-T    TCOR',
     &  '   LST  ZCBEST  TCBEST')
  502 FORMAT(F8.1,2(I7,F8.0),I7,F8.1,I6,F8.0,F8.1)

      RETURN
      END

C************************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    EVENT         WRITE AN EVENT.
C   PRGMMR: J. Woollen       ORG: NP20       DATE: 1994-MM-DD
C
C ABSTRACT: COMPUTE EXPANSION FACTOR FOR CORECT.
C
C PROGRAM HISTORY LOG:
C 1994-MM-DD  J. Woollen  Original author.
C
C USAGE:    CALL EVENT(LUNIT,EVNSTR,NLV,OBS,QMS,RCS,IND,NEVN,QCPC)
C   INPUT ARGUMENT LIST:
C     LUNIT    - UNIT NUMBER
C     EVNSTR   - EVENT STREAM OF CHARACTERS
C     NLV      - NUMBER OF LEVELS
C     OBS      - OBSERVED VALUE
C     QMS      - QUALITY MARKER
C     RCS      - REASON CODE
C     IND      - INDIRECT ADDRESS OF VALUE
C     NEVN     - NUMBER OF EVENTS
C     QCPC     - PROGRAM CODE
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE EVENT(LUNIT,EVNSTR,NLV,OBS,QMS,RCS,IND,NEVN,QCPC)

      REAL(8) BMISS,EVNS_8(4,255)

      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST
      CHARACTER*(*) EVNSTR
      DIMENSION     OBS(NEVN),QMS(NEVN),RCS(NEVN),IND(NEVN)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0
      IF(NEVN.EQ.0) GOTO 100


C  CLEAR THE UFB ARRAY FIRST
C  -------------------------

      EVNS_8 = BMISS

C  TRANSFER EVENT ARRAYS INTO UFB ARRAY
C  ------------------------------------

      DO I=1,NEVN
        J = IND(I)
        IF(OBS(I).LT.0.5*BMISS .AND. J.LE.255) THEN
           EVNS_8(1,J) = OBS(I)
           EVNS_8(2,J) = QMS(I)
           EVNS_8(3,J) = QCPC
           EVNS_8(4,J) = RCS(I)
        ENDIF
      ENDDO

C  WRITE THE EVENTS
C  ----------------

      CALL UFBINT(LUNIT,EVNS_8,4,NLV,IRET,EVNSTR)

  100 CONTINUE
      IF(TEST) WRITE(60,500) LUNIT,NEVN,NLV,IRET,EVNSTR,QCPC
  500 FORMAT(' EVENT--LUNIT,NEVN,NLV,IRET,EVNSTR:',4I5,A60,
     &  ' QCPC:',F8.0)

      RETURN
      END

C*******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    EVENTW        WRITE A WIND EVENT.
C   PRGMMR: J. Woollen       ORG: NP20       DATE: 1994-MM-DD
C
C ABSTRACT: COMPUTE EXPANSION FACTOR FOR CORECT.
C
C PROGRAM HISTORY LOG:
C 1994-MM-DD  J. Woollen  Original author.
C 1997-05-23  W. Collins  Modified for wind event.
C
C USAGE:    CALL EVENTW(LUNIT,EVNSTR,NLV,OBS,QMS,RCS,IND,NEVN,QCPC)
C   INPUT ARGUMENT LIST:
C     LUNIT    - UNIT NUMBER
C     EVNSTR   - EVENT STREAM OF CHARACTERS
C     NLV      - NUMBER OF LEVELS
C     OBS      - OBSERVED VALUES
C     QMS      - QUALITY MARKER
C     RCS      - REASON CODE
C     IND      - INDIRECT ADDRESS OF VALUE
C     NEVN     - NUMBER OF EVENTS
C     QCPC     - PROGRAM CODE
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE EVENTW(LUNIT,EVNSTR,NLV,OBS,QMS,RCS,IND,NEVN,QCPC)

      REAL(8) BMISS,EVNS_8(5,255)

      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST
      CHARACTER*(*) EVNSTR
      DIMENSION     OBS(2,NEVN),QMS(NEVN),RCS(NEVN),IND(NEVN)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0
      IF(NEVN.EQ.0) GOTO 100


C  CLEAR THE UFB ARRAY FIRST
C  -------------------------

      EVNS_8 = BMISS

C  TRANSFER EVENT ARRAYS INTO UFB ARRAY
C  ------------------------------------

      DO I=1,NEVN
        J = IND(I)
        IF(OBS(1,I).LT.BMISS .AND. J.LE.255) THEN
           EVNS_8(1,J) = OBS(1,I)
           EVNS_8(2,J) = OBS(2,I)
           EVNS_8(3,J) = QMS(I)
           EVNS_8(4,J) = QCPC
           EVNS_8(5,J) = RCS(I)
        ENDIF
      ENDDO

C  WRITE THE EVENTS
C  ----------------

      CALL UFBINT(LUNIT,EVNS_8,5,NLV,IRET,EVNSTR)

  100 CONTINUE
      IF(TEST) WRITE(60,500) LUNIT,NEVN,NLV,IRET,EVNSTR,QCPC
  500 FORMAT(' EVENT--LUNIT,NEVN,NLV,IRET,EVNSTR:',4I5,A60,
     &  ' QCPC:',F8.0)

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    EVPROC      PREPARE EVENTS FOR PREPBUFR OUTPUT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Prepare events for PREPBUFR output.  This is necessary
C   since there may be multiple actions on the same variable, but only
C   one event should be written.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL EVPROC(I1,I2,ICALL)
C   INPUT ARGUMENT LIST:
C     I1       - INDEX OF FIRST EVENT TO CONSIDER
C     I2       - INDEX OF LAST EVENT TO CONSIDER
C     ICALL    - 1 for call from MAIN
C              - 2 for call from STEVNTS
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 52  - OUTPUT PRINT FILE OF VALUES, RESIDUALS AND EVENTS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE EVPROC(I1,I2,ICALL)
      PARAMETER (NST=1500)

      COMMON /EVENTS/  STN(2000),    SEQN(2000),  ISCAN(2000),
     &                 LEVL(2000),   PRES(2000),  LTYP(2000),
     &                 IVAR(2000),   COR(2000),   CORVAL(2000),
     &                 LSTYP(2000),  PCOR(2000),  IEVENT,
     &                 BASR(2000),   PISR(2000),  PSINCR(2000),
     &                 QMARK(2000),  IETYP(2000), EDATE(2000)
      COMMON /EVNBUF/ STNB(2000),   SEQNB(2000), ISCANB(2000),
     &                LEVLB(2000),  PRESB(2000), LTYPB(2000),
     &                IVARB(2000),  CORB(2000),  CORVALB(2000),
     &                QMARKB(2000), IETYPB(2000),EDATEB(2000),
     &                LSTYPB(2000), PCORB(2000), IEVENTB
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)

      REAL            BSEQN(2000), CORL(0:100), CORVALL(0:100),
     &                QMARKL(0:100)
      INTEGER         IBVAR(2000), IBLVL(2000), IETYPL(0:100),
     &                IEV(2000)
      CHARACTER*8     STN, STNB
      CHARACTER*10    EDATE, EDATEB, BDATE(2000)
      CHARACTER*2     CVAR(5)
      LOGICAL         QGOOD
      DATA CVAR/' P',' Z',' T','TD',' Q'/

C  THIS SUBROUTINE PROCESSES THE EVENTS FILE TO PRODUCE A LIST OF EVENTS
C  TO BE ADDED TO THE PREPBUFR FILE. FIRST, MAKE A LIST OF ALL THE
C  UNIQUE DATA FOR WHICH THERE IS AN EVENT.
C  ---------------------------------------------------------------------

      IBUF = 0
      DO I=I1,I2
        IF(IBUF.NE.0) THEN
          DO J=1,IBUF
            IF(EDATE(I).EQ.BDATE(J) .AND.
     &         SEQN(I).EQ.BSEQN(J)  .AND.
     &         IVAR(I).EQ.IBVAR(J)  .AND.
     &         LEVL(I).EQ.IBLVL(J)) GOTO 10
          ENDDO

C  MUST ADD TO LIST
C  ----------------

          IBUF = IBUF + 1
          IEV(IBUF)   = I
          BDATE(IBUF) = EDATE(I)
          BSEQN(IBUF) = SEQN(I)
          IBVAR(IBUF) = IVAR(I)
          IBLVL(IBUF) = LEVL(I)

C  ALREADY ON LIST
C  --------------

   10     CONTINUE

        ELSE

C  ADD FIRST ITEM TO LIST
C  ----------------------

          IBUF = IBUF + 1
          IEV(IBUF)   = I
          BDATE(IBUF) = EDATE(I)
          BSEQN(IBUF) = SEQN(I)
          IBVAR(IBUF) = IVAR(I)
          IBLVL(IBUF) = LEVL(I)

        ENDIF
      ENDDO
      IEVENTB = IBUF

C  FOR EACH DATUM ON LIST, MAKE A LIST OF EVENTS
C  ---------------------------------------------

      DO J=1,IBUF
        LIST = 0
        DO I=I1,I2
          IF(EDATE(I).EQ.BDATE(J) .AND.
     &       SEQN(I).EQ.BSEQN(J)  .AND.
     &       IVAR(I).EQ.IBVAR(J)  .AND.
     &       LEVL(I).EQ.IBLVL(J)) THEN

            LIST = LIST + 1
            IF(LIST.GT.100) GOTO 15
            CORL(LIST)    = COR(I)
            CORVALL(LIST) = CORVAL(I)
            QMARKL(LIST)  = QMARK(I)
            IETYPL(LIST)  = IETYP(I)
          ENDIF
        ENDDO
   15   CONTINUE

C  CREATE A FINAL EVENT FOR EACH ON LIST
C  -------------------------------------

C  IF FINAL QMARKL IS GOOD, THEN CORRECTION IS SUM OF
C  CORRECTIONS.  OTHERWISE, CORRECTION = 0.
C  --------------------------------------------------

        QGOOD = .TRUE.
        DO I=1,LIST
          IF(QMARKL(I).GT.2.) QGOOD = .FALSE.
        ENDDO
        C = 0.
        IF(QGOOD) THEN
          DO I=1,LIST
            C = C + CORL(I)
          ENDDO
        ENDIF
        CORL(0) = C

C  CORRECTED VALUE IS LAST VALUE, AND
C  QUALITY MARK HAS LAST VALUE
C  ----------------------------------

        CORVALL(0) = CORVALL(LIST)
        QMARKL(0)  = QMARKL(LIST)

C  IF QMARKL(0) .GE. 3, RESET CORL(0) TO 0.
C  ----------------------------------------

        IF(QMARKL(0).GE.3) CORL(0) = 0.

C  ERROR TYPE WILL BE THAT OF FIRST CORRECTION, IF ANY.
C  OTHERWISE, SET IT TO LAST OTHER THAN 30, OTHERWISE,
C  SET TO 30.
C  ----------------------------------------------------

        IETYPL(0) = 0
        DO I=1,LIST
          IF(CORL(I).NE.0.) THEN
            IETYPL(0) = IETYPL(I)
            GOTO 20
          ENDIF
        ENDDO

        DO I=1,LIST
          IF(IETYPL(I).NE.0 .AND. IETYPL(I).NE.30) THEN
            IETYPL(0) = IETYPL(I)
          ENDIF
        ENDDO

        IF(IETYPL(0).EQ.0) THEN
          DO I=1,LIST
            IF(IETYPL(I).EQ.30) THEN
              IETYPL(0) = 30
            ENDIF
          ENDDO
        ENDIF

   20   CONTINUE

C  CREATE BUFR EVENTS FILE
C  -----------------------

        I = IEV(J)
        STNB(J)    = STN(I)
        SEQNB(J)   = SEQN(I)
        LEVLB(J)   = LEVL(I)
        PRESB(J)   = PRES(I)
        LTYPB(J)   = LTYP(I)
        IVARB(J)   = IVAR(I)
        CORB(J)    = CORL(0)
        CORVALB(J) = CORVALL(0)
        QMARKB(J)  = QMARKL(0)
        IETYPB(J)  = IETYPL(0)
        EDATEB(J)  = EDATE(I)
        LSTYPB(J)  = LSTYP(I)
        PCORB(J)   = PCOR(I)

      ENDDO

C  WRITE BUFR EVENTS TO UNIT 6
C  ---------------------------

      IF(I1.EQ.1 .AND. I2.EQ.IEVENT .AND. I2.NE.0) WRITE(6,500)
      DO I=1,IBUF
        IF(MOD(I,10).EQ.1) WRITE(6,503)
        OVAL = CORVALB(I) - CORB(I)
        WRITE(6,504) PRESB(I),CVAR(IVARB(I)),IETYPB(I),QMARKB(I),
     &    OVAL,CORB(I),CORVALB(I),LEVLB(I),LTYPB(I),STNB(I),
     &    SEQNB(I),EDATEB(I)
      ENDDO

C  WRITE BUFR EVENTS TO UNIT 52 FOR SDM-S
C  --------------------------------------

      IF(ICALL.EQ.1) THEN
        DO I=1,IBUF
          IF(MOD(I,10).EQ.1) WRITE(52,503)
          OVAL = CORVALB(I) - CORB(I)
          WRITE(52,504) PRESB(I),CVAR(IVARB(I)),IETYPB(I),QMARKB(I),
     &      OVAL,CORB(I),CORVALB(I),LEVLB(I),LTYPB(I),STNB(I),
     &      SEQNB(I),EDATEB(I)
        ENDDO
      ENDIF

  500 FORMAT(//'BUFR EVENTS')
  503 FORMAT('  PRESSURE VAR IETYP QMARK  ORIG-VAL       COR',
     &       '   NEW-VAL  LEVEL LEVTYP  STATION     SQN        DATE')
  504 FORMAT(1X,F9.1,2X,A2,I6,F6.0,3F10.1,I7,I7,4X,A8,F6.0,1X,A10)

      RETURN
      END

C************************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FILALL      FILL AUXILIARY LEVELS WITH DATA
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Fill auxiliary level with data.  I.e., interpolate data
C   from levels with LVTYP = 0 to levels with LVTYP = 1. See GETLEV
C   for how LVTYP is specified.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 1997-05-12  W. Collins  Correct logic for top level.
C 1997-05-12  W. Collins  Write height event only if the height
C     changes by at least 2 m.
C 1997-07-23  W. Collins  Limit the layer thickness to 200 hPa
C     over which heights are recalculated.
C
C USAGE:    CALL FILALL(SAME,WIND)
C   INPUT ARGUMENT LIST:
C     SAME     - TRUE WHEN WIND PART GOES WITH PREVIOUSL READ
C                MASS PART
C     WIND     - TRUE FOR WIND PART OF REPORT
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE FILALL(SAME,WIND)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      REAL            VAR(9,255), VARINC(9,255), PLOG(255), POBL(255)
      CHARACTER*8     SID
      LOGICAL         SAME, WIND
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /ALSNDW/ POBW(255),PFCW(255),PQMW(255),PRCW(255),
     &                ZOBW(255),ZFCW(255),ZQMW(255),ZRCW(255),
     &                UOB(255),VOB(255),UFC(255),VFC(255),
     &                WQM(255),WRC(255),SP(255), DIR(255),
     &                CATW(255)
      COMMON /INC/    PNC(255),TNC(255),QNC(255),TDNC(255),ZNC(255)
      COMMON /INCW/   PNCW(255),ZNCW(255),UNC(255),VNC(255)
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /PEROR/  ISERR, ISOBERR
      COMMON /LTP/    LVTYP(9,255), NCH(9,255), LVRC(9,255), LVRCALL
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL ISERR, LVRC, ISOBERR, LVRCALL
      DATA   PPI /57.29578/
      DATA   T0 /273.15/, R /287.05/, G /9.80665/

      VARINC = BMISS

      IF(WIND) THEN
C       IF(.NOT. SAME) WRITE(6,500) SID
  500   FORMAT(' FILALL--SAME IS .FALSE. FOR: ',A8)
        IF(.NOT. SAME) RETURN
        IV1 = 7
        IV2 = 8
        DO L=1,NLV
          VARINC(6,L) = PNCW(L)
          VARINC(7,L) = UNC(L)
          VARINC(8,L) = VNC(L)
          VARINC(9,L) = ZNC(L)
          POBL(L)     = POBW(L)
          VAR(6,L)    = POBW(L)
          VAR(7,L)    = UOB(L)
          VAR(8,L)    = VOB(L)
          VAR(9,L)    = ZOBW(L)
          PLOG(L)     = ALP(POBW(L))
        ENDDO
      ELSE
        IV1 = 2
        IV2 = 3
        DO L=1,NLV
          VARINC(1,L) = PNC(L)
          VARINC(2,L) = TNC(L)
          VARINC(3,L) = TDNC(L)
          VARINC(4,L) = QNC(L)
          VARINC(5,L) = ZNC(L)
          POBL(L)     = POB(L)
          VAR(1,L)    = POB(L)
          VAR(2,L)    = TOB(L)
          VAR(3,L)    = TDO(L)
          VAR(4,L)    = QOB(L)
          VAR(5,L)    = ZOB(L)
          PLOG(L)     = ALP(POB(L))
        ENDDO
      ENDIF

C  INTERPOLATE INCREMENTS FROM LEVELS WHERE LVTYP = 1
C  TO LEVELS WHERE LVTYP = 0, BUT ONLY IF THERE WAS
C  AN ORIGINAL VALUE PRESENT
C  DO FOR THE VARIABLES IN THE RANGE IV1 TO IV2
C  --------------------------------------------------------

      DO IV=IV1,IV2
        ipr = 0

C  FIND FIRST GOOD LEVEL TO INTERPOLATE FROM
C  -----------------------------------------

        DO L=1,NLV
          LFIRST = L
          IF(LVTYP(IV,L).EQ.1 .AND. VARINC(IV,L).NE.BMISS) GOTO 100
        ENDDO
  100   CONTINUE

C  FIND LAST GOOD LEVEL TO INTERPOLATE FROM
C  ----------------------------------------

        DO L=NLV,1,-1
          LAST = L
          IF(LVTYP(IV,L).EQ.1 .AND. VARINC(IV,L).NE.BMISS) GOTO 200
        ENDDO
  200   CONTINUE
        IF(LAST.LT.LFIRST) GOTO 300

        DO L=1,NLV
          IF(LVRC(IV,L) .AND. VAR(IV,L).NE.BMISS) THEN
            VARINC(IV,L) = 0.
            ipr = ipr + 1
C           if(ipr.eq.1 .and. iserr) write(6,504)

C  FIND TWO LEVELS TO USE FOR INTERPOLATION, ONE BELOW AND ONE ABOVE
C  -----------------------------------------------------------------

            LEV1 = 1
            LEV2 = NLV
            IF(L.LT.LFIRST .AND. ABS(POBL(LFIRST)-POBL(L)).LT.100.) THEN
              VARINC(IV,L) = VARINC(IV,LFIRST)
            ELSE
              DO LL=L-1,1,-1
                LEV1 = LL
                IF(LVTYP(IV,LL).EQ.1 .AND.
     &             VARINC(IV,LL).NE.BMISS) GOTO 10
              ENDDO
            ENDIF

C  NO LEVEL FOUND BELOW. TRY TO FIND A LEVEL ABOVE.
C  ------------------------------------------------

            DO LL=L+1,NLV
              LEV2 = LL
              IF(LVTYP(IV,LL).EQ.1 .AND.
     &           VARINC(IV,LL).NE.BMISS) GOTO 5
            ENDDO

C  NONE ABOVE EITHER
C  -----------------

            GOTO 30

C  NONE BELOW, BUT FOUND A LEVEL ABOVE
C  -----------------------------------


    5       CONTINUE
            IF(ABS(POBL(L)-POBL(LEV2)).LT.100.) THEN
              VARINC(IV,L) = VARINC(IV,LEV2)
            ENDIF
            GOTO 30

C  LEVEL FOUND BELOW
C  -----------------

   10       CONTINUE
            IF(L.GT.LAST) THEN
              IF(ABS(POBL(L)-POBL(LAST)).LT.100.) THEN
                VARINC(IV,L) = VARINC(IV,LAST)
              ENDIF
              GOTO 30
            ELSE
              DO LL=L+1,NLV
                LEV2 = LL
                IF(LVTYP(IV,LL).EQ.1 .AND.
     &             VARINC(IV,LL).NE.BMISS) GOTO 20
              ENDDO
            ENDIF

C  NO LEVEL FOUND ABOVE; USE INCREMENT FROM BELOW
C  ----------------------------------------------

            IF(ABS(POBL(L)-POBL(LEV1)).LT.100.) THEN
              VARINC(IV,L) = VARINC(IV,LEV1)
            ENDIF
            GOTO 30

   20       CONTINUE

C  BOTH LEVELS FOUND--PERFORM INTERPOLATION, USING 2 LEVELS
C  --------------------------------------------------------

            IF(PLOG(LEV2)-PLOG(LEV1).NE.0. .AND.
     &        ABS(POBL(LEV1)-POBL(LEV2)).LT.250.) THEN
              VARINC(IV,L) = ((PLOG(LEV2)-PLOG(L))*VARINC(IV,LEV1)
     &          + (PLOG(L)-PLOG(LEV1))*VARINC(IV,LEV2)) /
     &            (PLOG(LEV2)-PLOG(LEV1))
            ENDIF
  30        CONTINUE

            if(iserr) then
              write(6,501) iv,l,lev1,lev2,pobl(l),pobl(lev1),pobl(lev2),
     &          cat(l),varinc(iv,l),varinc(iv,lev1),varinc(iv,lev2)
  501         format(10x,4i5,3f12.1,f8.0,3f12.1)
  504         format(' filall--    iv    l lev1 lev2      pob(l)',
     &          '   pob(lev1)   pob(lev2)     cat      inc(l)',
     &          '   inc(lev1)   inc(lev2)')
            endif

          ENDIF
        ENDDO
  300   CONTINUE
      ENDDO

      IF(WIND) THEN
        DO L=1,NLV
          PNCW(L) = VARINC(6,L)
          UNC(L)  = VARINC(7,L)
          VNC(L)  = VARINC(8,L)
          ZNCW(L) = VARINC(9,L)
          SP(L)   = SQRT(UNC(L)**2 + VNC(L)**2)
          IF(VNC(L).NE.0. .OR. UNC(L).NE.0.) THEN
            DIR(L) = 90. - PPI*ATAN2(VNC(L),UNC(L))
          ELSE
            DIR(L) = 0.
          ENDIF
        ENDDO
      ELSE
        DO L=1,NLV
          PNC(L)  = VARINC(1,L)
          TNC(L)  = VARINC(2,L)
          TDNC(L) = VARINC(3,L)
          QNC(L)  = VARINC(4,L)
          ZNC(L)  = VARINC(5,L)
        ENDDO
      ENDIF

C  THE COUNTER NCH TELLS WHETHER AN ORIGINAL VALUE
C  HAS BEEN CHANGED.  AN EVENT WILL BE CREATED
C  FOR THESE VARIABLES.  INITIALIZE NCH.
C  -----------------------------------------------

      NCH = 0

C  HYDROSTATICALLY RE-FILL ALL HEIGHTS THAT WERE
C  ORIGINALLY HYDROSTATICALLY INTERPOLATED.
C  ----------------------------------------------

      IF(.NOT. WIND) THEN
        DO L=2,NLV
          IF(LVRC(5,L)) THEN

C  FIND NEXT LEVEL BELOW WITH NECESSARY GOOD DATA
C  ----------------------------------------------

            DO LL=L-1,1,-1
              IF(POB(LL).NE.BMISS .AND. PQM(LL).LT.3. .AND.
     &           ABS(POB(LL)-POB(L)).LE.200. .AND.
     &           ZOB(LL).NE.BMISS .AND. ZQM(LL).LT.3. .AND.
     &           TOB(LL).NE.BMISS .AND. TQM(LL).LT.3.) GOTO 40
            ENDDO

C  NO LEVEL FOUND BELOW WITH NECESSARY DATA
C  ----------------------------------------

            LL = 1
            GOTO 50
   40       CONTINUE
            IF(TNC(L).LT.BMISS .AND.
     &         TFC(L).LT.BMISS .AND.
     &         QOB(L).LT.BMISS) THEN
              TL = (TNC(L)+TFC(L)+T0)*(1.+.61*QOB(L))
            ELSEIF(TNC(L).LT.BMISS .AND.
     &        TFC(L).LT.BMISS) THEN
              TL = TNC(L)+TFC(L)+T0
            ELSE
              GOTO 50
            ENDIF
            IF(QOB(LL).LT.BMISS) THEN
              TLL = (TOB(LL)+T0)*(1.+.61*QOB(LL))
            ELSE
              TLL = TOB(LL)+T0
            ENDIF
            ZOB(L) = ZOB(LL) - 0.5*(R/G)*(TL+TLL)*(PLOG(L)-PLOG(LL))
            ZNC(L) = ZOB(L) - ZFC(L)
            NCH(5,L) = 1
            IF(ZQM(L).LE.2.) ZQM(L) = 1.
            ZRC(L) = 15.
   50       CONTINUE
            if(iserr) then
              write(6,505) l,ll,pob(l),pob(ll),zob(l),zob(ll),tl,tll
  505         format(' filall--l,ll,pob-s,zob-s,tl,tll: ',
     &          2i4,2f10.1,2f8.0,2f8.1)
            endif
          ENDIF
        ENDDO
        if(iserr) then
          write(6,502) nlv
          write(6,503) (pob(l),tob(l),tfc(l),tnc(l),qob(l),zob(l),
     &      l=1,nlv)
  502     format(' filall--pob    tob    tfc    tnc      qob',
     &      '     zob   nlv:',i4)
  503     format(1x,f11.1,3f7.1,f9.4,f8.0)
        endif
      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FILL        FILL EVENTS AS DESCRIBED BELOW
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Fill events.  This subroutine is used with GETOB to be
C   able to get input data as of a particular point in the processing,
C   e.g. after PREVENT, or after CQCHT.  In order to do this, some
C   events must be filled either forward or backward in the processing
C   stream.  For instance, the guess appears only at the last stage,
C   but should be available at all stages.  This subroutine does the
C   necessary filling.  The selection of which stage date to use is
C   made in INPUT (variable MP).
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL FILL(OBS,NLEV)
C   INPUT ARGUMENT LIST:
C     OBS      - ARRAY OF EVENTS, READ BY GETOB
C     NLEV     - NUMBER OF LEVELS OF DATA
C
C   OUTPUT ARGUMENT LIST:
C     OBS      - ARRAY OF EVENTS, FILLED
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE FILL(OBS,NLEV)
      PARAMETER (MVO=5)        ! p,T,z,q,Td
      PARAMETER (MVR=10)       ! dimensions of header
      PARAMETER (MLV=255)      ! number of possible levels
      PARAMETER (MEV=13)       ! number of possible events

C     Last index of OBS: value, fcst value, quality mark, reason code.

      DIMENSION OBS(MVO,MLV,0:MEV,4)

      REAL(8) BMISS

      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      BMAX = 0.5*BMISS

C     FILL FORWARD WITH NON-MISSING VALUES.

      DO J=1,NLEV
        DO K=1,MEV
          DO L=1,4
            IF(L.EQ.2) GOTO 6
            DO I=1,MVO
              IF(OBS(I,J,K,L).LT.BMAX) GOTO 5
              DO KK=K-1,1,-1
                IF(OBS(I,J,KK,L).LT.BMAX) THEN
                  OBS(I,J,K,L) = OBS(I,J,KK,L)
                  GOTO 5
                ENDIF
              ENDDO
    5         CONTINUE
            ENDDO
    6       CONTINUE
          ENDDO
        ENDDO
      ENDDO

C     FILL BOTH WAYS FOR THE GUESS VALUES.

      DO J=1,NLEV
        DO I=1,MVO
          DO K=1,MEV
            GES = BMISS
            IF(OBS(I,J,K,2).LT.BMAX) THEN
              GES = OBS(I,J,K,2)
              GOTO 20
            ENDIF
          ENDDO
   20     CONTINUE
          DO K=1,MEV
            OBS(I,J,K,2) = GES
          ENDDO
        ENDDO
      ENDDO

C     FILL BOTH WAYS FOR DEW-POINT TEMPERATURE.

      DO J=1,NLEV
        TD = BMISS
        DO M=1,MEV
          IF(OBS(5,J,M,1).LT.BMAX) THEN
            TD = OBS(5,J,M,1)
            GOTO 30
          ENDIF
        ENDDO
   30   CONTINUE
        DO M=1,MEV
          OBS(5,J,M,1) = TD
        ENDDO
      ENDDO

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FULVAL      ADD INCREMENT TO GUESS TO GET FULL VALUE
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1995-12-18
C
C ABSTRACT: ADD THE INCREMENT TO THE FIRST GUESS TO GET THE FULL VALUE.
C
C PROGRAM HISTORY LOG:
C 1995-12-18  W. Collins  Original author.
C 1997-04-28  W. Collins  Make sure that the events correspond to the
C     changes to the data.
C 1997-05-19  W. Collins  Make QOB correspond to the modified TDO.
C
C USAGE:    CALL FULVAL(WIND)
C   INPUT ARGUMENT LIST:
C     WIND     - .TRUE. FOR WIND PART OF REPORT, .FALSE. OTHERWISE
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE FULVAL(WIND)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /ALSNDW/ POBW(255),PFCW(255),PQMW(255),PRCW(255),
     &                ZOBW(255),ZFCW(255),ZQMW(255),ZRCW(255),
     &                UOB(255),VOB(255),UFC(255),VFC(255),
     &                WQM(255),WRC(255),SP(255), DIR(255),
     &                CATW(255)
      COMMON /EVNSND/ PO (255),TO (255),ZO (255),CA (255),
     &                PQ (255),TQ (255),ZQ (255),INP(255),
     &                PR (255),TR (255),ZR (255),INZ(255),
     &                QO (255),QQ (255),QR (255),INT(255),
     &                TDE(255),TDQ(255),TDR(255),INQ(255),
     &                UO(255), VO(255), SPO(255), DIRO(255),
     &                WQ(255), WR(255)
      COMMON /INC/    PNC(255),TNC(255),QNC(255),TDNC(255),ZNC(255)
      COMMON /INCW/   PNCW(255),ZNCW(255),UNC(255),VNC(255)
      COMMON /PEROR/  ISERR, ISOBERR
      COMMON /LTP/    LVTYP(9,255), NCH(9,255), LVRC(9,255), LVRCALL
      COMMON /STN/     IS
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL         TEST
      LOGICAL         ISERR, LVRC, ISOBERR, PRNT, LVRCALL
      CHARACTER*8     SID
      LOGICAL         WIND, SDM, SDMT, SDMZ, ISNCH

C-----------------------------------------------------------------------
C FCNS BELOW CONVERT TEMP/TD (K) & PRESS (MB) INTO SAT./ SPEC. HUM.(G/G)
C-----------------------------------------------------------------------
      !ES(T) = 6.1078*EXP((17.269*(T - 273.15))/(T - 273.15 + 237.3))
      !QS(T,P) = (0.622*ES(T))/(P-0.378*ES(T))
C-----------------------------------------------------------------------

C  CLEAR THE EVENTS ARRAYS
C  -----------------------

      PO = BMISS
      TO = BMISS
      TDE = BMISS
      QO = BMISS
      ZO = BMISS
      UO = BMISS
      VO = BMISS
      IN = BMISS

C  ADD INCREMENTS TO FORECAST TO GET FULL VALUES.
C  DO ONLY FOR AUXILIARY LEVELS
C  -------------------------------------------------

      IF(.NOT. WIND) THEN
        DO L=1,NLV
          QM   = TQM(L)
          SDMT = QM.EQ.0 .OR. QM.EQ.8 .OR. QM.EQ.9 .OR.
     &           QM.EQ.11 .OR. QM.EQ.12 .OR. QM.EQ.14 .OR.
     &           QM.EQ.15
          QM   = ZQM(L)
          SDMZ = QM.EQ.0 .OR. QM.EQ.8 .OR. QM.EQ.9 .OR.
     &           QM.EQ.11 .OR. QM.EQ.12 .OR. QM.EQ.14 .OR.
     &           QM.EQ.15
C         IF(LVRC(1,L) .AND.
C    &      PNC(L).NE.BMISS .AND. PFC(L).NE.BMISS) THEN
C           POB(L)  = PNC(L)+PFC(L)
C           NCH(1,L) = 1
C         ENDIF
          IF(LVRC(2,L) .AND. .NOT.SDMT .AND.
     &      TNC(L).NE.BMISS .AND. TFC(L).NE.BMISS) THEN
            TOB(L)  = TNC(L)+TFC(L)
            NCH(2,L) = 1
          ENDIF
C         IF(LVRC(4,L) .AND. .NOT.SDMT .AND.
C    &      TDNC(L).NE.BMISS .AND. TDFC(L).NE.BMISS) THEN
C           TDO(L) = TDNC(L)+TDFC(L)
C           NCH(4,L) = 1
C  IF TDO IS CHANGED, RECALCULATE QOB.
C           IF(TDO(L).NE.BMISS .AND. POB(L).NE.BMISS) THEN
C             QOB(L)  = QS(TDO(L)+TOB(L)+273.15,POB(L))
C             NCH(3,L) = 1
C           ENDIF
C         ENDIF
C         IF(LVRC(5,L) .AND. .NOT.SDMZ .AND.
C    &      ZNC(L).NE.BMISS .AND. ZFC(L).NE.BMISS) THEN
C           ZOB(L)  = ZNC(L)+ZFC(L)
C           NCH(5,L) = 1
C         ENDIF
        ENDDO
      ELSE
        DO L=1,NLV
          QM = WQM(L)
          SDM = QM.EQ.0 .OR. QM.EQ.8 .OR. QM.EQ.9 .OR.
     &          QM.EQ.11 .OR. QM.EQ.12 .OR. QM.EQ.14 .OR.
     &          QM.EQ.15
C         IF(LVRC(6,L) .AND.
C    &      PNCW(L).NE.BMISS .AND. PFCW(L).NE.BMISS) THEN
C           POBW(L) = PNCW(L)+PFCW(L)
C           NCH(6,L) = 1
C         ENDIF
          IF(LVRC(7,L) .AND. .NOT. SDM .AND.
     &      UNC(L).NE.BMISS .AND. UFC(L).NE.BMISS) THEN
            UOB(L)  = UNC(L)+UFC(L)
            NCH(7,L) = 1
          ENDIF
          IF(LVRC(8,L) .AND. .NOT. SDM .AND.
     &      VNC(L).NE.BMISS .AND. VFC(L).NE.BMISS) THEN
            VOB(L)  = VNC(L)+VFC(L)
            NCH(8,L) = 1
          ENDIF
C         IF(LVRC(9,L) .AND.
C    &      ZNCW(L).NE.BMISS .AND. ZFCW(L).NE.BMISS) THEN
C           ZOBW(L) = ZNCW(L)+ZFCW(L)
C           NCH(9,L) = 1
C         ENDIF
        ENDDO
      ENDIF

C  PREPARE EVENT ARRAYS FOR UFB AND EXIT
C  LOOP THROUGH LEVELS, LOOKING FOR EVENTS
C  ---------------------------------------

      IEV = 0
      IF(.NOT. WIND) THEN
        ISNCH = .FALSE.
        DO L=1,NLV

C  SET QQM TO LARGER OF QQM AND TQM
C  --------------------------------

CWC       QQM(L) = AMAX1(QQM(L),TQM(L))

          DO N=1,5
            ISNCH = ISNCH .OR. NCH(N,L).EQ.1
          ENDDO
          IF(ISNCH) THEN
          IEV = IEV + 1
          INP(IEV) = L
          IF(NCH(1,L).EQ.1) THEN
            PO(IEV) = POB(L)
            PQM(L)  = 1.
            PQ(IEV) = PQM(L)
            PR(IEV) = PRC(L)
          ENDIF
          IF(NCH(2,L).EQ.1) THEN
            TO(IEV) = TOB(L)
            TQM(L)  = 1.
            TQ(IEV) = TQM(L)
            TR(IEV) = TRC(L)
          ENDIF
CWC       IF(NCH(3,L).EQ.1) THEN
CWC         IF(QOB(L).LT.BMISS) THEN
CWC           QO(IEV) = QOB(L)*1.E6
CWC           QQM(L)  = 1.
CWC           QQ(IEV) = QQM(L)
CWC           QR(IEV) = QRC(L)
CWC         ELSE
CWC           QO(IEV) = BMISS
CWC           QQ(IEV) = BMISS
CWC           QR(IEV) = BMISS
CWC         ENDIF
CWC       ENDIF
CWC       IF(NCH(4,L).EQ.1) THEN
CWC         IF(TDO(L).LT.BMISS .AND. TOB(L).LT.BMISS) THEN
CWC           TDE(IEV) = TOB(L) - TDO(L)
CWC           QQM(L)  = 1.
CWC           TDQ(IEV) = QQM(L)
CWC           TDR(IEV) = QRC(L)
CWC         ELSE
CWC           TDE(IEV) = BMISS
CWC           TDQ(IEV) = BMISS
CWC           TDR(IEV) = BMISS
CWC         ENDIF
CWC       ENDIF
          IF(NCH(5,L).EQ.1) THEN
            ZO(IEV) = ZOB(L)
            ZQM(L)  = 1.
            ZQ(IEV) = ZQM(L)
            ZR(IEV) = ZRC(L)
          ENDIF
          IF(ISERR) THEN
            IF(IEV.EQ.1) THEN
              IF(TEST) THEN
                WRITE(6,504) SID(IS)
  504           FORMAT(' FULVAL--event for: ',A8)
                WRITE(6,505)
  505           FORMAT('   pressure      ht    temp  sp hum pqm',
     &                 ' zqm tqm qqm  pr  zr  tr  qr ind')
              ENDIF
            ENDIF
            IF(TEST) WRITE(6,506) PO(IEV),ZO(IEV),TO(IEV),QO(IEV),
     &        PQ(IEV),ZQ(IEV),TQ(IEV),QQ(IEV),PR(IEV),ZR(IEV),TR(IEV),
     &        QR(IEV),INP(IEV)
  506       FORMAT(1X,F10.0,1X,F7.1,1X,F7.1,1X,F7.0,8(1X,F3.0),
     &        1X,I3)
          ENDIF
          ENDIF
          NEV = IEV
        ENDDO
      ELSE
        ISNCH = .FALSE.
        DO L=1,NLV
          DO N=6,9
            ISNCH = ISNCH .OR. NCH(N,L).EQ.1
          ENDDO
          IF(ISNCH) THEN
          IEV = IEV + 1
          INP(IEV) = L
          IF(NCH(6,L).EQ.1) THEN
            PO(IEV) = POBW(L)
            PQMW(L) = 1.
            PQ(IEV) = PQMW(L)
            PR(IEV) = PRCW(L)
          ENDIF
          IF(NCH(9,L).EQ.1) THEN
            ZO(IEV) = ZOBW(L)
            ZQMW(L) = 1.
            ZQ(IEV) = ZQMW(L)
            ZR(IEV) = ZRCW(L)
          ENDIF
          IF(NCH(7,L).EQ.1 .OR. NCH(8,L).EQ.1) THEN
            UO(IEV) = UOB(L)
            VO(IEV) = VOB(L)
            WQM(L)  = 1.
            WQ(IEV) = WQM(L)
            WR(IEV) = WRC(L)
          ENDIF
          IF(ISERR) THEN
            WRITE(6,504) SID
            WRITE(6,507)
            WRITE(6,508) PO(IEV),ZO(IEV),UO(IEV),VO(IEV),PQ(IEV),
     &      ZQ(IEV),WQ(IEV),PR(IEV),ZR(IEV),WR(IEV),INP(IEV)
  507       FORMAT('   press      ht  u-comp  v-comp pqm zqm',
     &             ' wqm  pr  zr  wr ind')
  508       FORMAT(4(1X,F7.1),6(1X,F3.0),1X,I3)
          ENDIF
          ENDIF
          NEV = IEV
        ENDDO
      ENDIF

      RETURN
      END

C*********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FUZZY       COMPUTE OBS ERROR FUZZY LOGIC RESULT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1995-12-18
C
C ABSTRACT: GIVEN INCREMENT, VERTICAL RESIDUAL, HORIZONTAL RESIDUAL
C   AS NORMAL VARIATES, CALCULATE THE FUZZY LOGIC ESTIMATE OF THE
C   PRESENCE OF OBSERVATION ERROR.  THE RESULT IS IN TERMS OF THE MEAN
C   NORMAL VARIATE.  THE ACTUAL DECISION REGARDING AN ERROR OR NOT IS
C   NOT DONE HERE.
C
C PROGRAM HISTORY LOG:
C 1999-04-13  W. Collins  Original author.
C 2001-04-20  W. Collins  Correct error that showed up only when a
C     guess was not available: test leading to P = 0.
C
C USAGE:    CALL FUZZY(P,XI,XV,XH,B,PER)
C   INPUT ARGUMENT LIST:
C     XI   - INCREMENT, NORMAL VARIATE
C     XV   - VERTICAL RESIDUAL, NORMAL VARIATE
C     XH   - HORIZONTAL RESIDUAL, NORMAL VARIATE
C     B    - MAXIMUM RANGE OF ROUGH ERRORS
C     PER  - ASSUMED PROPORTION OF ROUGH ERRORS
C
C   OUTPUT ARGUMENT LIST:
C     P    - MEAN "ANALYSIS WEIGHT" FOR XI, XV, XH
C
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE FUZZY(P,XI,XV,XH,B,PER)

      REAL(8) BMISS

      REAL X(3)
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      X(1) = ABS(XI)
      X(2) = ABS(XV)
      X(3) = ABS(XH)

      XUM = 0.
      SUM = 0.
      DO I=1,3
        IF(X(I).LT..1*BMISS) THEN
          XUM = XUM + 1.
          SUM = SUM + ANLWT(X(I),B,PER)
C         SUM = SUM + CUMPROB(ABS(X(I)))
        ENDIF
      ENDDO

      IF(X(1).LT..1*BMISS .AND. X(1).GE.7.) THEN
        P = 0.
      ELSEIF(XUM.GT.0.) THEN
        P = SUM/XUM
      ELSE
        P = BMISS
      ENDIF

      RETURN
      END

C*********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GETINC      COMPUTE INCREMENTS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1995-12-18
C
C ABSTRACT: COMPUTE THE INCREMENT (OBSERVED VALUE MINUS FORECAST) AT
C   ALL "TRUE DATA LEVELS", I.E. LEVELS AT WHICH DATA WAS ACTUALLY
C   OBSERVED.
C
C PROGRAM HISTORY LOG:
C 1995-12-18  W. Collins  Original author.
C 1997-05-19  W. Collins  Only get increments TNC,UNC and VNC.
C
C USAGE:    CALL GETINC(WIND)
C   INPUT ARGUMENT LIST:
C     WIND     - .TRUE. FOR WIND PART OF REPORT, .FALSE. OTHERWISE
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 22  - LIST OF POTENTIAL WIND PROBLEMS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE GETINC(WIND)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      LOGICAL         WIND
      CHARACTER*8     SID
      CHARACTER*10    CDATE
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /ALSNDW/ POBW(255),PFCW(255),PQMW(255),PRCW(255),
     &                ZOBW(255),ZFCW(255),ZQMW(255),ZRCW(255),
     &                UOB(255),VOB(255),UFC(255),VFC(255),
     &                WQM(255),WRC(255),SP(255), DIR(255),
     &                CATW(255)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /INC/    PNC(255),TNC(255),QNC(255),TDNC(255),ZNC(255)
      COMMON /INCW/   PNCW(255),ZNCW(255),UNC(255),VNC(255)
      COMMON /PEROR/  ISERR, ISOBERR
      COMMON /LTP/    LVTYP(9,255), NCH(9,255), LVRC(9,255), LVRCALL
      COMMON /STN/    IS
      COMMON /DATEX/  JDATE(5), CDATE
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL ISERR, LVRC, ISOBERR, START, PRNT, LVRCALL
      DATA START /.TRUE./

C  INITIALIZE INCREMENTS
C  ---------------------

      PNC  = BMISS
      TNC  = BMISS
      QNC  = BMISS
      TDNC = BMISS
      ZNC  = BMISS
      PNCW = BMISS
      ZNCW = BMISS
      UNC  = BMISS
      VNC  = BMISS
      IF(START) THEN
C       WRITE(22,505)
        START = .FALSE.
      ENDIF


C  SOLVE FOR INCREMENTS AT NON-AUXILIARY LEVELS WITH
C  GOOD, NON-MISSING DATA (I.E. LVTYP = 1)
C  -------------------------------------------------

      IF(.NOT. WIND) THEN
        DO L=1,NLV
C         IF(LVTYP(1,L).EQ.1 .AND. POB(L).NE.BMISS .AND.
C    &       PFC(L).NE.BMISS) PNC(L)  = POB(L)-PFC(L)
          IF(LVTYP(2,L).EQ.1 .AND. TOB(L).NE.BMISS .AND.
     &       TFC(L).NE.BMISS) TNC(L)  = TOB(L)-TFC(L)
C         IF(LVTYP(3,L).EQ.1 .AND. QOB(L).NE.BMISS .AND.
C    &       QFC(L).NE.BMISS) QNC(L)  = QOB(L)-QFC(L)
          IF(LVTYP(4,L).EQ.1 .AND. TDO(L).NE.BMISS .AND.
     &       TDFC(L).NE.BMISS .AND. TOB(L).NE.BMISS)
     &       TDNC(L) = (TOB(L)-TDO(L)) - TDFC(L)  !TDO is depression
C         IF(LVTYP(5,L).EQ.1 .AND. ZOB(L).NE.BMISS .AND.
C    &       ZFC(L).NE.BMISS) ZNC(L)  = ZOB(L)-ZFC(L)
  	  IF(ISERR) THEN
            WRITE(6,500) SID(IS),L,POB(L),PNC(L),TNC(L),QNC(L),TDNC(L),
     &        ZNC(L)
  500       FORMAT(' GETINC--SID,L,POB,PNC,TNC,QNC,TDNC,ZNC: ',
     &        A8,I5,2X,6F10.1)
          ENDIF
        ENDDO

      ELSE
        DO L=1,NLV
C         IF(LVTYP(6,L).EQ.1 .AND. POBW(L).NE.BMISS .AND.
C    &       PFCW(L).NE.BMISS) PNCW(L) = POBW(L)-PFCW(L)
C  If CAT=4, first solve for POBW?
C         IF(LVTYP(9,L).EQ.1 .AND. ZOB(L).NE.BMISS .AND.
C    &       ZFCW(L).NE.BMISS) ZNCW(L) = ZOBW(L)-ZFCW(L)
C  If LVTYP=1, solve hydrostatically for ZOBW (?)
          IF(LVTYP(7,L).EQ.1 .AND. UOB(L).NE.BMISS .AND.
     &       UFC(L).NE.BMISS) UNC(L)  = UOB(L)-UFC(L)
          IF(LVTYP(8,L).EQ.1 .AND. VOB(L).NE.BMISS .AND.
     &       VFC(L).NE.BMISS) VNC(L)  = VOB(L)-VFC(L)
          IF(UNC(L).NE.BMISS .AND. VNC(L).NE.BMISS) THEN
             WNC = SQRT(UNC(L)**2 + VNC(L)**2)
             IF(WNC.GT.25.) THEN
               IF(UOB(L).NE.0. .OR. VOB(L).NE.0.) THEN
                 ANGOB = 90. - 57.29578 * ATAN2(VOB(L),UOB(L))
                 IF(ANGOB.LT.0.) ANGOB = ANGOB+360.
               ELSE
                 ANGOB = 0.
               ENDIF
               IF(UFC(L).NE.0. .OR. VFC(L).NE.0.) THEN
                 ANGFC = 90. - 57.29578 * ATAN2(VFC(L),UFC(L))
                 IF(ANGFC.LT.0.) ANGFC = ANGFC+360.
               ELSE
                 ANGFC = 0.
               ENDIF
               IANGOB = ANGOB
               IANGFC = ANGFC
               SPOB  = SQRT(VOB(L)**2 + UOB(L)**2)
               SPFC  = SQRT(VFC(L)**2 + UFC(L)**2)
C              WRITE(22,502) CDATE,DHR(IS),SID(IS),XOB(IS),YOB(IS),
C    &           POBW(L),IANGOB,IANGFC,SPOB,SPFC,WNC
             ENDIF
  502        FORMAT(1X,A10,F8.0,2X,A8,2F8.2,F8.1,4X,I5,'/',I5,1X,
     &         2F8.1,F8.0)
  505        FORMAT(' POTENTIAL WIND PROBLEMS:',/,
     &         59X,'DIRECTION         SPEED',/,
     &         '     DATE    REL-HR    STN       LONG     LAT   PRESS',
     &         '      OBS/GUESS      OBS   GUESS  V-DIFF')
          ENDIF
  	  IF(ISERR) THEN
            WRITE(6,501) SID(IS),L,POBW(L),PNCW(L),ZNCW(L),UNC(L),VNC(L)
  501       FORMAT(' GETINC--SID,L,POBW,PNCW,ZNCW,UNC,VNC:    ',
     &        A8,I5,2X,5F10.1)
          ENDIF
        ENDDO
      ENDIF

      RETURN
      END

C*****************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GETLEV      DETERMINE LEVELS OF ORIGINAL DATA
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1995-12-18
C
C ABSTRACT: DETERMINE THE LEVELS OF ORIGINAL DATA.  THE LEVELS WILL BE
C   USED ONLY IF THE ORIGINAL QUALITY WAS GOOD, OR THE MODIFIED VALUES
C   ARE JUDGED TO BE GOOD.  ALL "TRUE DATA LEVELS", I.E. LEVELS AT
C   WHICH DATA WAS ACTUALLY OBSERVED.
C
C PROGRAM HISTORY LOG:
C 1995-12-18  W. Collins  Original author.
C 1997-05-12  W. Collins  Add LVRC.
C 1997-07-23  W. Collins  For height, add CAT=4 to list making LVRC
C     .TRUE.
C
C USAGE:    CALL GETLEV(WIND)
C   INPUT ARGUMENT LIST:
C     WIND     - .TRUE. FOR WIND PART OF REPORT, .FALSE. OTHERWISE
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

C  INDIVIDUALLY FOR PRESSURE, TEMPERATURE, MOISTURE, AND HEIGHT,
C  DETERMINE THE LEVELS OF ORIGINAL DATA.  THE LEVELS WILL BE USED
C  ONLY IF THE ORIGINAL QUALITY WAS GOOD, OR THE MODIFIED VALUES
C  ARE JUDGED TO BE GOOD.
C  LVTYP, FIRST INDEX:  = 1   PRESSURE     \
C                       = 2   TEMPERATURE   | MASS PART
C                       = 3   SPEC HUM      |
C                       = 4   DEW PT TEMP   |
C                       = 5   HEIGHT       /
C
C                       = 6   PRESSURE     \
C                       = 7   U-COMPONENT   | WIND PART
C                       = 8   V-COMPONENT   |
C                       = 9   HEIGHT       /
C
C  LVTYP  = 1 FOR ORIGINAL DATA LEVELS WITH GOOD QUALITY
C         = 0 OTHERWISE
C
C  LVRC   = .TRUE. FOR AUXILIARY LEVELS
C         = .FALSE. OTHERWISE
C

      SUBROUTINE GETLEV(WIND)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      LOGICAL WIND
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /ALSNDW/ POBW(255),PFCW(255),PQMW(255),PRCW(255),
     &                ZOBW(255),ZFCW(255),ZQMW(255),ZRCW(255),
     &                UOB(255),VOB(255),UFC(255),VFC(255),
     &                WQM(255),WRC(255),SP(255), DIR(255),
     &                CATW(255)
      COMMON /PEROR/  ISERR, ISOBERR
      COMMON /LTP/    LVTYP(9,255), NCH(9,255), LVRC(9,255), LVRCALL
      COMMON /STN/     IS
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL ISERR, LVRC, ISOBERR, PRNT, LVRCALL
      CHARACTER*8     SID
      LVTYP = 0
      LVRC = .FALSE.
      LVRCALL = .FALSE.
      IF(.NOT. WIND) THEN

C  MASS PART OF REPORT
C  -------------------

        IPR = 0
        DO L=1,NLV
          LCAT = CAT(L)
          IF(ISERR) THEN
            IPR = IPR + 1
            IF(IPR.EQ.1) WRITE(6,502)
          ENDIF

C  LVRC = .true. if the level is an auxiliary level
C       = .false. otherwise
C  ------------------------------------------------

          LVRC(1,L) = PRC(L).EQ.101. .OR. PRC(L).EQ.103.
     &                .OR. PRC(L).EQ.105.
          LVRC(2,L) = TRC(L).EQ.101. .OR. TRC(L).EQ.102.
     &                .OR. TRC(L).EQ.103.
          LVRC(3,L) = QRC(L).EQ.101. .OR. QRC(L).EQ.102.
     &                .OR. QRC(L).EQ.103.
          LVRC(4,L) = QRC(L).EQ.101. .OR. QRC(L).EQ.102.
     &                .OR. QRC(L).EQ.103.
          LVRC(5,L) = ZRC(L).EQ.101. .OR. ZRC(L).EQ.103.
     &                .OR. CAT(L).EQ.4.
          DO I=1,5
            IF(LVRC(I,L)) LVRCALL = .TRUE.
          ENDDO
          IF(.NOT.LVRCALL) RETURN

C  LVTYP = 1 iff variable is not missing and
C                variable is not auxiliary (shown by rc) and
C                quality is good
C        = 0 otherwise
C  ----------------------------------------------------------

          IF(POB(L).NE.BMISS  .AND. .NOT.LVRC(1,L) .AND.
     &      (PQM(L).LE.2 .OR. PQM(L).GT.15)) LVTYP(1,L) = 1
          IF(TOB(L).NE.BMISS  .AND. .NOT.LVRC(2,L) .AND.
     &      (TQM(L).LE.2 .OR. TQM(L).GT.15)) LVTYP(2,L) = 1
          IF(QOB(L).NE.BMISS  .AND. .NOT.LVRC(2,L) .AND.
     &      (QQM(L).LE.2 .OR. QQM(L).GT.15)) LVTYP(3,L) = 1
          IF(TDO(L).NE.BMISS .AND. .NOT.LVRC(2,L) .AND.
     &      (QQM(L).LE.2 .OR. QQM(L).GT.15)) LVTYP(4,L) = 1
          IF(ZOB(L).NE.BMISS  .AND. .NOT.LVRC(5,L) .AND.
     &      (ZQM(L).LE.2 .OR. ZQM(L).GT.15)) LVTYP(5,L) = 1
          IF(ISERR) THEN
            WRITE(6,500) SID(IS),NLV,L,POB(L),LCAT,(LVTYP(I,L),I=1,5),
     &                   (LVRC(J,L),J=1,5)
  500       FORMAT(10X,A8,2X,2I5,F8.1,I5,5I3,2X,5(1X,L1))
  502       FORMAT(' GETLEV--   SID       NLV    L     POB LCAT',
     &        '-----LVTYPs----  ---LVRCs--')
          ENDIF
        ENDDO

      ELSE

C  WIND PART OF REPORT
C  -------------------

        IPR = 0
        DO L=1,NLV

          LCAT = CAT(L)
          LPRC = PRCW(L)
          LWRC = WRC(L)
          LZRC = ZRCW(L)
          IF(ISERR) THEN
            IPR = IPR + 1
            IF(IPR.EQ.1) WRITE(6,501)
          ENDIF

          LVRC(6,L) = PRC(L).EQ.101. .OR. PRC(L).EQ.103.
     &                .OR. PRC(L).EQ.105.
          LVRC(7,L) = WRC(L).EQ.101. .OR. WRC(L).EQ.102.
     &                .OR. WRC(L).EQ.103.
          LVRC(8,L) = WRC(L).EQ.101. .OR. WRC(L).EQ.102.
     &                .OR. WRC(L).EQ.103.
          LVRC(9,L) = ZRC(L).EQ.101. .OR. ZRC(L).EQ.103.
          DO I=6,9
            IF(LVRC(I,L)) LVRCALL = .TRUE.
          ENDDO
          IF(.NOT.LVRCALL) RETURN

          IF(LCAT.EQ.1 .OR. LCAT.EQ.3 .OR.
     &        LCAT.EQ.4 .OR. LCAT.EQ.5) THEN
            IF(POBW(L).NE.BMISS .AND. .NOT.LVRC(6,L) .AND.
     &        (PQM(L).LE.2 .OR. PQM(L).GT.15)) LVTYP(6,L) = 1
            IF(UOB(L).NE.BMISS .AND. .NOT.LVRC(7,L) .AND.
     &        (WQM(L).LE.2 .OR. WQM(L).GT.15)) LVTYP(7,L) = 1
            IF(VOB(L).NE.BMISS .AND. .NOT.LVRC(8,L) .AND.
     &        (WQM(L).LE.2 .OR. WQM(L).GT.15)) LVTYP(8,L) = 1
            IF(ZOBW(L).NE.BMISS .AND. .NOT.LVRC(9,L) .AND.
     &        (ZQM(L).LE.2 .OR. ZQM(L).GT.15)) LVTYP(9,L) = 1
          ENDIF
          IF(ISERR) THEN
            WRITE(6,503) SID(IS),NLV,L,POB(L),LCAT,LPRC,LWRC,LZRC,
     &        (LVTYP(I,L),I=6,9),(LVRC(J,L),J=6,9)
  501       FORMAT(' GETLEV--   SID       NLV    L     POB LCAT',
     &        '  PRC  WRC  ZRC-----LVTYPs----  ---LVRCs--')
  503       FORMAT(10X,A8,2X,2I5,F8.1,4I5,4I3,2X,5(1X,L1))
          ENDIF
        ENDDO

      ENDIF

      RETURN
      END

C*************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GETOB       READ DATA FOR 1 OBSERVATION
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Read data for a single observation. Place the date in OBS.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL GETOB(LUBFR,MP,HDR_8,NLEV,CAT,HRDR,YDR,XDR,OBS)
C   INPUT ARGUMENT LIST:
C     LUBFR    - UNIT NO. OF INPUT PREPBUFR FILE
C     MP       - PROCESSING STAGE AT WHICH DATA IS RETURNED:
C                0 - FINAL VALUES
C                1 - PREPRO
C                2 - SYNDATA
C                3 - CLIMO
C                4 - PREVENT
C                5 - CQCHT
C                6 - RADCOR
C                7 - PREPACQC
C                8 - VIRTMP
C                9 - OIQC
C                10 - SSI  (list valid as of 3/97)
C     HDR_8    - HEADER
C     NLEV     - NUMBER OF OBSERVATION LEVELS
C     CAT      - ARRAY OF CATEGORY BY LEVEL
C     HRDR     - ARRAY OF BALLOON DRIFT D-TIME BY LEVEL
C     YDR      - ARRAY OF BALLOON DRIFT LATITUDE BY LEVEL
C     XDR      - ARRAY OF BALLOON DRIFT LONGITUDE BY LEVEL
C
C   OUTPUT ARGUMENT LIST:
C     OBS      - ARRAY OF OBSERVATIONS
C
C   OUTPUT FILES:
C     UNIT LUBFR  - UNIT NUMBER OF PREPBUFR FILE
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE GETOB(LUBFR,MP,HDR_8,NLEV,CAT,HRDR,YDR,XDR,OBS)

C     THIS SUBROUTINE RETURNS ALL EVENTS FOR AN OBSERVATION

      PARAMETER (MVO=5)        ! p,T,z,q,Td
      PARAMETER (MVR=10)       ! dimensions of header
      PARAMETER (MLV=255)      ! number of possible levels
      PARAMETER (MEV=13)       ! number of possible events

      REAL(8) BMISS,HDR_8(MVR),EVR_8(MVO,MLV,MEV,5),CAT_8(MLV),
     &        HRDR_8(MLV),XDR_8(MLV),YDR_8(MLV)

C     Last index of EVR_8: value, fcst value, quality mark, program
C     code, reason code
C     Last index of OBS: value, fcst value, quality mark, reason code.

      CHARACTER*50 HDSTR, OBSTR(2,5)
      DIMENSION    OBS(MVO,MLV,0:MEV,4),CAT(MLV),HRDR(MLV),XDR(MLV),
     &             YDR(MLV)
      INTEGER      IORDER(0:13)

      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      DATA IORDER /0,1,2,3,4,7,5,11,12,6,8,9,10,13/
      DATA HDSTR /'SID XOB YOB DHR ELV ITP TYP T29 SQN'/
      DATA OBSTR /'POB TOB ZOB QOB TDO' , 'POB UOB VOB ZOB ZOB',
     &            'PFC TFC ZFC QFC QFC' , 'PFC UFC VFC ZFC ZFC',
     &            'PQM TQM ZQM QQM QQM' , 'PQM WQM WQM ZQM ZQM',
     &            'PPC TPC ZPC QPC QPC' , 'PPC WPC WPC ZPC ZPC',
     &            'PRC TRC ZRC QRC QRC' , 'PRC WRC WRC ZRC ZRC'/

      EVR_8 = BMISS
      OBS = BMISS

C  READ HEADER
C  -----------

      CALL UFBINT(LUBFR,HDR_8,MVR,1,IRET,HDSTR)
      JKX = INT(HDR_8(7))/100

C  GET BALLOON DRIFT INFORMATION
C  -----------------------------

      CALL UFBINT(LUBFR,HRDR_8,1,MLV,NLEV,'HRDR');HRDR=HRDR_8
      CALL UFBINT(LUBFR,YDR_8,1,MLV,NLEV,'YDR');YDR=YDR_8
      CALL UFBINT(LUBFR,XDR_8,1,MLV,NLEV,'XDR');XDR=XDR_8

C  GET LEVEL CATEGORIES
C  --------------------

      CALL UFBINT(LUBFR,CAT_8,1,MLV,NLEV,'CAT');CAT=CAT_8
      NLEV = MIN(255,NLEV)

C  READ EVENTS
C  -----------

      IF(MP.EQ.0) THEN
        DO K=1,3
          CALL UFBINT(LUBFR,EVR_8(1,1,1,K),MVO,MLV,NLV,OBSTR(JKX,K))
          DO J=1,NLEV
            DO I=1,MVO
              OBS(I,J,MP,K) = EVR_8(I,J,1,K)
            ENDDO
          ENDDO
        ENDDO
        K = 5
        CALL UFBINT(LUBFR,EVR_8(1,1,1,K),MVO,MLV,NLV,OBSTR(JKX,K))
        DO J=1,NLEV
          DO I=1,MVO
            OBS(I,J,MP,4) = EVR_8(I,J,1,K)
          ENDDO
        ENDDO
      ELSE
        DO I=1,5
          CALL UFBEVN(LUBFR,EVR_8(1,1,1,I),MVO,MLV,MEV,NLV,OBSTR(JKX,I))
          IF(NLEV.NE.NLV) THEN
            PRINT *, 'NLEVS NOT EQUAL - STOP 99'
            CALL W3TAGE('PREPOBS_CQCBUFR')
            CALL ERREXIT(99)
          ENDIF
        ENDDO

C  EXPAND THE EVENTS INTO A REGULAR ARRAY
C  --------------------------------------

        DO K=1,MEV
          DO J=1,NLEV
            DO I=1,MVO
              IP = NINT(EVR_8(I,J,K,4))
              IF(IP.GE.0 .AND. IP.LE.MEV) THEN
                IPC = IORDER(IP)
                OBS(I,J,IPC,1) = EVR_8(I,J,K,1)
                OBS(I,J,IPC,2) = EVR_8(I,J,K,2)
                OBS(I,J,IPC,3) = EVR_8(I,J,K,3)
                OBS(I,J,IPC,4) = EVR_8(I,J,K,5)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GETPS       SOLVE FOR SURFACE PRESSURE
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT:  Solve for surface pressure, given station elevation and
C   height and temperature at a known mandatory level above. The lapse
C   rate is specified from U.S. Standard atmosphere.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL GETPS(PS,ZS,Z1,T1,P1)
C   INPUT ARGUMENT LIST:
C     ZS       - STATION ELEVATION (M)
C     Z1       - HEIGHT AT FIRST MANDATORY LEVEL ABOVE THE SURFACE
C     T1       - TEMPERATURE AT Z1
C     P1       - PRESSURE AT Z1
C
C   OUTPUT ARGUMENT LIST:
C     PS       - SURFACE PRESSURE
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE GETPS(PS,ZS,Z1,T1,P1)
      COMMON /CONSTS/  R, G, T0, CP, RV
      DATA ALPHA /54./

C  ASSUMPTIONS:
C     HYDROSTATIC
C     T = T1 + ALPHA*LN(P/P1)
C     ALPHA TAKEN FROM STANDARD ATMOSPHERE
C  ---------------------------------------

      GOR = G/R
      TA = T1 + T0
      P1 = ALOG(P1)
      RAD = (TA-ALPHA*P1)**2 + 2.*ALPHA*(-.5*ALPHA*P1**2 +
     &  TA*P1 + GOR*(Z1-ZS))
      IF(RAD.LE.0.) THEN
        RAD = 0.
      ELSE
        RAD = SQRT(RAD)
      ENDIF
      PS = EXP(P1 + (-TA+RAD)/ALPHA)

      WRITE(60,500) PS,ZS,Z1,T1,P1
  500 FORMAT(' GETPS--PS,ZS,Z1,T1,P1: ',5F10.1)

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    HOLES       FIND HOLES IN THE DATA
C   PRGMMR: KEYSER         ORG: NP22       DATE: 2013-02-05
C
C ABSTRACT: Find holes in the data.  A 'hole' is a region between
C   mandatory levels, where mandatory levels are not complete. There
C   must be at least 2 levels with incomplete information for it to be
C   a hole.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2008-10-08  Woollen/Keyser -- corrected if tests where integer values
C     were tested for not being equal to real value for missing (BMISS
C     = 10E10), these integer values can never be equal to BMISS for
C     I*4 due to overflow - instead they are now tested for not being
C     equal to new integer value for missing (IMISS, also = 10E10),
C     although this is also an overflow value for I*4, it results in a
C     correct evaluation
C 2013-02-05  D. Keyser -- Final changes to run on WCOSS: Set BUFRLIB
C     missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C     overflow (also done for IMISS).
C
C USAGE:    CALL HOLES
C   OUTPUT FILES:
C     UNIT 61  - PRINTOUT OF 'HOLE' EXTENTS
C
C REMARKS: (This subroutine is for
C           monitoring purposes only and is not needed for running
C           of PREPOBS_CQCBUFR.)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE HOLES
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /STN/     IS
      COMMON /DATEX/   JDATE(5), CDATE
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST
      CHARACTER*8 SID
      CHARACTER*10    CDATE

      LBELOW = BMISS
      LABOVE = BMISS
      LB     = BMISS
      LA     = BMISS
      DO L=1,NLV
        IF(LST(L).EQ.121 .OR. LST(L).EQ.131 .OR.
     &     LST(L).EQ.132 .OR. LST(L).EQ.142 .OR.
     &     LST(L).EQ.133) THEN
C         IF(TEST) WRITE(61,501) CDATE,SID(IS),MANLEV(POB(L))
  501     FORMAT(1X,A10,2X,A8,I5)
        ENDIF
        IF(CAT(L).EQ.1) THEN
          IF(LST(L).EQ.121 .OR. LST(L).EQ.131) THEN
            LB     = L
            LBELOW = MANLEV(POB(L))
          ENDIF
                            ! LBELOW will never test = bmiss, use imiss
cdak      IF(LBELOW.NE.BMISS .AND. (LST(L).EQ.133 .OR.
          IF(LBELOW.NE.IMISS .AND. (LST(L).EQ.133 .OR.
     &       LST(L).EQ.132 .OR. LST(L).EQ.142)) THEN
            LA     = L
            LABOVE = MANLEV(POB(L))
            IF(LA.GT.0 .AND. LA.LE.255 .AND.
     &         LB.GT.0 .AND. LB.LE.255) THEN
              IF(TEST) WRITE(61,500) CDATE,SID(IS),LBELOW,LABOVE,
     &          LST(LB),LST(LA)
            ENDIF
  500       FORMAT(1X,A10,2X,A8,4(1X,I4))
            IF(LST(L).EQ.133) THEN
              LB     = LA
              LBELOW = LABOVE
            ELSE
              LB     = BMISS
              LBELOW = BMISS
            ENDIF
            LA     = BMISS
            LABOVE = BMISS
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END

C***********************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    HORRES      PERFORM HORIZONTAL CHECK
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Perform horizontal check, solving for the horizontal
C   residual.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL HORRES
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE HORRES
      PARAMETER (NST=1500)

      REAL(8) BMISS

      REAL             HRES(21,4,NST),HSTD(21,4,NST),
     &                 WTH(4,21,4,NST),OINC(21,4,NST)
      INTEGER          IDH(4,21,4,NST)
      LOGICAL          OG(21,4,NST)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /STATS/   NV(21,7), AVGV(21,7), STDV(21,7),
     &                 NH(21,7), AVGH(21,7), STDH(21,7),
     &                 NI(21,7), AVGI(21,7), STDI(21,7),
     &                 NT(21,4),  AVGT(21,4), STDT(21,4),
     &                 NHY(21),   AVGHY(21),  STDHY(21),
     &                 NB,        AVGB,       STDB,
     &                 SKV(21,7), XKV(21,7),
     &                 SKH(21,7), XKH(21,7),
     &                 SKI(21,7), XKI(21,7),
     &                 SKT(21,4), XKT(21,4),
     &                 SKHY(21),  XKHY(21),
     &                 SKB,       XKB
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /SERCH/   RSCAN,DELLAT,DELLON(181)
      COMMON /CORP/    CHLP(0:1500),CVC,IFA(4,4)
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      CHARACTER*8      SID,SIDH(4,4),BLANK
      LOGICAL          ZG, TG, TDG, QG, ERROR, PRNT
      DATA PI180/.0174532/, RADE/6371./, MAXDIM/4/
      DATA BLANK /'        '/


C  MAKE THE OBSERVATION INDEX MAP
C  ------------------------------

      CALL MIMAP(XOB,YOB,NOBS)

C  SET UP THE SEARCH ARRAYS
C  ------------------------

      RSCAN  = 1000.
      DELLAT = RSCAN/(PI180*RADE)
      DO I=1,181
        BLAT      = I - 91
        COSLAT    = COS(BLAT*PI180)
        DELLON(I) = MIN(DELLAT/COSLAT,180.0)
      ENDDO

C  SYMMETRIC MATRIX STORAGE CHART
C  ------------------------------

      IJ = 1
      DO I = 1,MAXDIM
        DO J = 1,I
          IFA(I,J) = IJ
          IFA(J,I) = IJ
          IJ = IJ + 1
        ENDDO
      ENDDO

C  MAKE A TABLE OF LENGTH SCALES BY MILIBARS
C  -----------------------------------------

      DO I=1,1500
        CHLP(I) = 120.
      ENDDO

      CVC = 5.0

C  SET HORIZONTAL RESIDUAL ARRAYS TO MISSING
C  -----------------------------------------

      DO K=1,NST
        DO J=1,4
          DO I=1,21
            HRES(I,J,K) = BMISS
            HSTD(I,J,K) = BMISS
            DO L=1,4
              IDH(L,I,J,K)  = 0
              WTH(L,I,J,K)  = BMISS
            ENDDO
          ENDDO
        ENDDO
      ENDDO

C  PUT INCREMENTS OF Z, T, TD, Q, AND THICKNESS INTO OINC.
C  ALSO PUT PRELIMINARY ASSESSMENTS INTO OG.
C  -------------------------------------------------------

      DO N=1,NOBS
        DO L=1,MAND
          OINC(L,1,N) = ZIM(L,N)
          OINC(L,2,N) = TIM(L,N)
          OINC(L,3,N) = TDIM(L,N)
          OINC(L,4,N) = QIM(L,N)
          OG(L,1,N)   = ZG(L,N)
          OG(L,2,N)   = TG(L,N)
          OG(L,3,N)   = TDG(L,N)
          OG(L,4,N)   = QG(L,N)
        ENDDO
      ENDDO

C     do n=1,nobs
C       if(test) WRITE(60,507) sid(n)
C       do l=1,mand
C         if(test) WRITE(60,508) pmand(l),(og(l,i,n),i=1,4)
C       enddo
C     enddo
  507 format(' HORRES--STATION: ',A8)
  508 format(' HORRES--PRESS,OG: ',F10.1,2X,4L2,' (before search)')


c  loop over packages of problems no more than 1000 at a time
c  ----------------------------------------------------------

      DO N=1,NOBS,1000
      NOB1=N;NOB2=MIN(NOB1+999,NOBS)
      print*,'n1,n2=',nob1,nob2
      CALL SEARCH(21,4,MAND,4,NOB1,NOB2,IDH,OINC,OG)
      CALL QCOI  (21,4,MAND,4,NOB1,NOB2,IDH,OINC,HRES,HSTD,WTH)
      ENDDO

C     do n=1,nobs
C       if(test) WRITE(60,505) sid(n)
C       do l=1,mand
C         do j=1,4
C           do k=1,4
C             if(idh(j,l,k,n).ne.0) then
C               sidh(j,k) = sid(idh(j,l,k,n))
C             else
C               sidh(j,k) = blank
C             endif
C           enddo
C         enddo
C         if(test) WRITE(60,500) pmand(l),(sidh(i,1),i=1,4),
C    &                  (wth(i,l,1,n),i=1,4),og(l,1,n)
C         if(test) WRITE(60,501) pmand(l),(sidh(i,2),i=1,4),
C    &                  (wth(i,l,2,n),i=1,4),og(l,2,n)
C         if(test) WRITE(60,502) pmand(l),(sidh(i,3),i=1,4),
C    &                  (wth(i,l,3,n),i=1,4),og(l,3,n)
C         if(test) WRITE(60,503) pmand(l),(sidh(i,4),i=1,4),
C    &                  (wth(i,l,4,n),i=1,4),og(l,4,n)
C       enddo
C     enddo
  500 format(' horres--idh,wth,og for z at: ',f8.0/(4a10,4f8.3,l3))
  501 format(' horres--idh,wth,og for T at: ',f8.0/(4a10,4f8.3,l3))
  502 format(' horres--idh,wth,og for Td at:',f8.0/(4a10,4f8.3,l3))
  503 format(' horres--idh,wth,og for q at: ',f8.0/(4a10,4f8.3,l3))
  505 format(' horres--sid: ',a8)


C  PUT RESULTS BACK INTO RESIDUAL ARRAYS
C  -------------------------------------

      DO N=1,NOBS
        DO L=1,MAND
          ZHM(L,N)  = HRES(L,1,N)
          THM(L,N)  = HRES(L,2,N)
          TDHM(L,N) = HRES(L,3,N)
          QHM(L,N)  = HRES(L,4,N)
        ENDDO
      ENDDO

C  CALCULATE NORMALIZED HORIZONTAL RESIDUALS.
C  ------------------------------------------

      DO J=1,NST
        DO I=1,21
          NZHM(I,J)  = -1
          NTHM(I,J)  = -1
          NTDHM(I,J) = -1
          NQHM(I,J)  = -1
C         XZHM(I,J)  = BMISS
C         XTHM(I,J)  = BMISS
C         XTDHM(I,J) = BMISS
C         XQHM(I,J)  = BMISS
        ENDDO
      ENDDO
      DO N=1,NOBS
        DO L=1,MAND
          IF(ZHM(L,N).LT.BMISS/2) THEN
            NZHM(L,N) = MIN1(20.,20.*ABS(ZHM(L,N))/HOIRES(L,1))
C           XZHM(L,N) = (ABS(ZHM(L,N))-AVGH(L,1))/STDH(L,1)
          ENDIF
          IF(THM(L,N).LT.BMISS/2) THEN
            NTHM(L,N) = MIN1(20.,20.*ABS(THM(L,N))/HOIRES(L,2))
C           XTHM(L,N) = (ABS(THM(L,N))-AVGH(L,2))/STDH(L,2)
          ENDIF
          IF(TDHM(L,N).LT.BMISS/2) THEN
            NTDHM(L,N) = MIN1(20.,20.*ABS(TDHM(L,N))/HOIRES(L,3))
C           XTDHM(L,N) = (ABS(TDHM(L,N))-AVGH(L,3))/STDH(L,3)
          ENDIF
          IF(QHM(L,N).LT.BMISS/2) THEN
            NQHM(L,N) = MIN1(20.,20.*ABS(QHM(L,N))/HOIRES(L,4))
C           XQHM(L,N) = (ABS(QHM(L,N))-AVGH(L,4))/STDH(L,4)
          ENDIF
        ENDDO

C       if(test) WRITE(60,505) sid(n)
C       if(test) WRITE(60,506) (zhm(l,n),nzhm(l,n),thm(l,n),nthm(l,n),
C    &    tdhm(l,n),ntdhm(l,n),qhm(l,n),nqhm(l,n),
C    &    l=1,mand)
  506   format(' horres--'/'        zhm  nzhm         thm  nthm  ',
     &    '      tdhm ntdhm         qhm  nqhm '/
     &    (1x,4(e10.3,i6,2x)))

      ENDDO

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    HSC         CALCULATE HYDROSTATIC RESIDUALS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Calculate the hydrostatic residuals.  The residuals are
C   calculated both with and without use of significant level
C   temperatures.  They are also calculated with and without including
C   moisture:
C     HYDN IS HYDROSTATIC RESIDUAL USING MANDATORY TEMPERATURES
C     HYDQ IS HYD RESIDUAL USING MAND VIRTUAL TEMPERATURES
C     HYDS IS HYD RESIDUAL USING ALL VIRTUAL TEMPERATURES
C     HYD  IS HYD RESIDUAL BEST TO BE USED
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2000-06-13  W. Collins  Slight change so that HYDN fills HYDS
C     when it is unreasonably large or missing.
C
C USAGE:    CALL HSC
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE HSC
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /CONSTS/  R, G, T0, CP, RV
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      CHARACTER*8      SID
      LOGICAL ZG, TG, TDG, QG, ERROR, PRNT

C  CALCULATE HYDROSTATIC RESIDUALS
C    HYDN IS HYDROSTATIC RESIDUAL USING MANDATORY TEMPERATURES
C    HYDQ IS HYD RESIDUAL USING MAND VIRTUAL TEMPERATURES
C    HYDS IS HYD RESIDUAL USING ALL VIRTUAL TEMPERATURES
C    HYD  IS HYD RESIDUAL BEST TO BE USED
C  -----------------------------------------------------

      RTOG = R*T0/G
      RO2G = .5*R/G
      GOR  = G/R

      BASRES(IS) = BMISS
      BRES(IS) = BMISS
      PSINC  = BMISS
      B = BMISS

C  FIND INDEX OF THE FIRST MANDATORY LEVEL ABOVE THE SURFACE
C  AT WHICH HEIGHT AND TEMPERATURE ARE AVAILABLE.
C  ---------------------------------------------------------

      LUP = BMISS
      DO L=LSFC+1,NLEV
        LUP = L
        IF(CAT(L).EQ.1 .AND. LEVTYP(L).LE.2) GOTO 20
      ENDDO
      RETURN

C  TEST TO SEE IF NECESSARY INFO IS AVAILABLE FOR BASELINE RESIDUALS
C  AND THAT THE FIRST MANDATORY LEVEL IS NOT TOO FAR ABOVE SURFACE
C  -----------------------------------------------------------------

   20 CONTINUE
      IF(LSFC.EQ.0)         GOTO 100        ! No sfc level
      IF(LEVTYP(LSFC).GT.2) GOTO 100        ! Info not available
      IF(POB(LSFC)-POB(LUP).GT.200.) GOTO 100

C  GET HEIGHT DIFFERENCE BETWEEN SURFACE PRESS AND 1ST MAND LEVEL,
C  ACCORDING TO TEMPERATURES: BASRES
C  NOTE!  BASRES COULD BE CALCULATED VIA THREE METHODS AS IS THE
C  HYDROSTATIC RESIDUAL, AND CHOOSE THE BEST.
C  --------------------------------------------------------------
      SUM = 0.
      L1 = LSFC
      IF(LEVTYP(L1).EQ.1 .AND. TVO(L1).NE.BMISS) THEN
        T1 = TVO(L1)
      ELSE
        T1 = TOB(L1)
      ENDIF
      DO L=LSFC,LUP-1
        IF(LEVTYP(L+1).NE.3) THEN
          L2 = L+1
          IF(LEVTYP(L2).EQ.4 .AND. TVO(L2).NE.BMISS) THEN
            T2 = TVO(L2)
          ELSE
            T2 = TOB(L2)
          ENDIF
          SUM = SUM - (ALP(POB(L1))-ALP(POB(L2))) *
     &      (RTOG + RO2G*(T1+T2))
          L1 = L2
          T1 = T2
        ENDIF
      ENDDO
      BASRES(IS) = ZOB(LUP) - ZOB(LSFC) + SUM
      BRES(IS) = BASRES(IS)
      NBAS = MIN1(20.,20.*ABS(BASRES(IS))/HSCRES(1))

C  GET PRESSURE DIFFERENCE BETWEEN SURFACE AND 1ST MAND LEVEL,
C  USING REPORTED SURFACE PRESSURE, TEMPERATURES AND HEIGHTS:
C  PSINC
C  NOTE!  PSINC COULD BE CALCULATED VIA THREE METHODS AS IS THE
C  HYDROSTATIC RESIDUAL, AND CHOOSE THE BEST.
C  -----------------------------------------------------------

      IF(POB(LSFC).EQ.BMISS) GOTO 100
      Z1 = ZOB(LUP) + SUM
      IF(LEVTYP(LSFC).EQ.4 .AND. TVO(LSFC).NE.BMISS) THEN
        TSFC = TVO(LSFC) + T0
      ELSE
        TSFC = TOB(LSFC) + T0
      ENDIF
      PSFC = POB(LSFC)*EXP(GOR*(Z1-ZOB(LSFC))/TSFC)
      PSINC = POB(LSFC) - PSFC

C  CALCULATE HYDROSTATIC RESIDUALS FOR REST OF MAND LEVELS
C  -------------------------------------------------------

  100 CONTINUE
      IF(LUP.EQ.0) GOTO 200
      L11  = LUP
      L21  = LUP
      L31  = LUP
      LAST = 0
      DO I=1,21
        HYDN(I) = BMISS
        HYDQ(I) = BMISS
        HYDS(I) = BMISS
        HYD(I)  = BMISS
      ENDDO
      SUM3 = 0.
      DO L=LUP,NLEV-1
        MLEV = MANLEV(POB(L11))
        IF(LEVTYP(L11).LE.2 .AND. LEVTYP(L+1).LE.2 .AND.
     &    MLEV.NE.0 .AND. (CAT(L+1).EQ.1 .OR. CAT(L+1).EQ.5)) THEN
          LAST = L+1
          L12  = L+1
          B(MLEV) = RO2G*(ALP(POB(L11))-ALP(POB(L12)))
          SUM1 = -(ALP(POB(L11))-ALP(POB(L12))) *
     &     (RTOG + RO2G*(TOB(L11)+TOB(L12)))
          HYDN(MLEV) = ZOB(L12)-ZOB(L11) + SUM1
          L11 = L12
        ENDIF
        MLEV = MANLEV(POB(L21))
        IF(LEVTYP(L21).EQ.1 .AND. LEVTYP(L+1).EQ.1 .AND.
     &    MLEV.NE.0 .AND. (CAT(L+1).EQ.1 .OR. CAT(L+1).EQ.5)) THEN
          L22  = L+1
          SUM2 = -(ALP(POB(L21))-ALP(POB(L22))) *
     &     (RTOG + RO2G*(TVO(L21)+TVO(L22)))
          HYDQ(MLEV) = ZOB(L22)-ZOB(L21) + SUM2
          L21 = L22
        ENDIF
        IF(LEVTYP(L+1).NE.3) THEN
          L32 = L+1
          IF((LEVTYP(L).EQ.1 .OR. LEVTYP(L).EQ.4) .AND.
     &       TVO(L).NE.BMISS) THEN
            T1 = TVO(L)
          ELSE
            T1 = TOB(L)
          ENDIF
          IF((LEVTYP(L32).EQ.1 .OR. LEVTYP(L32).EQ.4) .AND.
     &       TVO(L32).NE.BMISS) THEN
            T2 = TVO(L32)
          ELSE
            T2 = TOB(L32)
          ENDIF
          SUM3 = SUM3 - (ALP(POB(L))-ALP(POB(L32))) *
     &     (RTOG + RO2G*(T1+T2))
          MLEV = MANLEV(POB(L31))
          IF(LEVTYP(L32).LE.2 .AND. MLEV.NE.0) THEN
            HYDS(MLEV) = ZOB(L32)-ZOB(L31) + SUM3
            L31 = L32
            SUM3 = 0.
          ENDIF
        ENDIF
      ENDDO
  200 CONTINUE

C  DECIDE WHICH HYDROSTATIC RESIDUAL TO USE.
C  -----------------------------------------

      DO I=1,21
        NHYD(I)  = -1
        NHYDN(I) = -1
        IF(ABS(HYDS(I)).GT..001*BMISS) HYDS(I) = HYDN(I)
      ENDDO
      IF(LAST.NE.0) THEN
        LEV1 = MANLEV(POB(LUP))
        LEV2 = MANLEV(POB(LAST))
        DO L=LEV1,LEV2
          IF(HYDS(L).NE.BMISS .AND.
     &      ABS(HYDN(L)-HYDQ(L)).LT.15. .AND.
     &      ABS(HYDQ(L)-HYDS(L)).LT.15.) THEN
            HYD(L) = HYDS(L)
            HYDM(L,IS) = HYDS(L)
          ELSEIF(HYDS(L).NE.BMISS .AND.
     &      ABS(HYDS(L)).LT.ABS(HYDN(L)) .AND.
     &      ABS(HYDS(L)).LT.ABS(HYDQ(L))) THEN
            HYD(L) = HYDS(L)
            HYDM(L,IS) = HYDS(L)
          ELSEIF(HYDQ(L).NE.BMISS .AND.
     &      ABS(HYDN(L)-HYDQ(L)).LT.15.) THEN
            HYD(L) = HYDQ(L)
            HYDM(L,IS) = HYDQ(L)
          ELSEIF(HYDN(L).NE.BMISS) THEN
            HYD(L) = HYDN(L)
            HYDM(L,IS) = HYDN(L)
          ELSEIF(HYDS(L).NE.BMISS) THEN
            HYD(L) = HYDS(L)
            HYDM(L,IS) = HYDS(L)
          ELSE
            HYD(L) = HYDQ(L)
            HYDM(L,IS) = HYDQ(L)
          ENDIF
          IF(HYDS(L).NE.BMISS) THEN
            HYD(L) = HYDS(L)       ! Note
            HYDM(L,IS) = HYDS(L)   ! Note
          ENDIF
          IF(HYD(L).NE.BMISS) THEN
            NHYD(L) = MIN1(20.,20.*ABS(HYD(L))/HSCRES(L))
          ENDIF
          IF(HYDN(L).NE.BMISS) THEN
            NHYDN(L) = MIN1(20.,20.*ABS(HYDN(L))/HSCRES(L))
          ENDIF
        ENDDO
      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INCR        CALCULATE INCREMENTS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Calculate increments for all mass variables.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL INCR(ITIME)
C   INPUT ARGUMENT LIST:
C     ITIME    - 1 - FIRST CALL TO INPUT FOR STATION
C                2 - SECOND CALL TO INPUT FOR STATION
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE INCR(ITIME)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /STATS/   NV(21,7), AVGV(21,7), STDV(21,7),
     &                 NH(21,7), AVGH(21,7), STDH(21,7),
     &                 NI(21,7), AVGI(21,7), STDI(21,7),
     &                 NT(21,4),  AVGT(21,4), STDT(21,4),
     &                 NHY(21),   AVGHY(21),  STDHY(21),
     &                 NB,        AVGB,       STDB,
     &                 SKV(21,7), XKV(21,7),
     &                 SKH(21,7), XKH(21,7),
     &                 SKI(21,7), XKI(21,7),
     &                 SKT(21,4), XKT(21,4),
     &                 SKHY(21),  XKHY(21),
     &                 SKB,       XKB
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      CHARACTER*8      SID
      LOGICAL ZG, TG, TDG, QG, ERROR, PRNT

      DATA ISTART /0/

C  INITIALIZE VALUES (INCLUDING SOME NON-INCREMENT FIELDS)
C  -------------------------------------------------------

      IF(ISTART.EQ.0 .OR. IS.EQ.1) THEN
        DO N=1,NST
          DO L=1,MAND
            ZIM(L,N)   = BMISS
            TIM(L,N)   = BMISS
            TDIM(L,N)  = BMISS
            QIM(L,N)   = BMISS
            ZVM(L,N)   = BMISS
            TVM(L,N)   = BMISS
            TDVM(L,N)  = BMISS
            QVM(L,N)   = BMISS
            HYDM(L,N)  = BMISS
          ENDDO
        ENDDO
        DO N=1,NST
          DO I=1,MAND
            NZIM(I,N)  = -1
            NTIM(I,N)  = -1
            NTDIM(I,N) = -1
            IF(ITIME.EQ.2) THEN
              XZIM(I,N)  = BMISS
              XTIM(I,N)  = BMISS
              XTDIM(I,N) = BMISS
              XQIM(I,N)  = BMISS
            ENDIF
          ENDDO
        ENDDO
        ISTART = 1
      ENDIF
      DO I=1,255
        PI(I)    = BMISS
        ZI(I)    = BMISS
        TI(I)    = BMISS
        TDI(I)   = BMISS
        QI(I)    = BMISS
        NZI(I)   = -1
        NTI(I)   = -1
        NTDI(I)  = -1
        NQI(I)   = -1
        IF(ITIME.EQ.2) THEN
          XZI(I)   = BMISS
          XTI(I)   = BMISS
          XTDI(I)  = BMISS
          XQI(I)   = BMISS
        ENDIF
      ENDDO

      DO L=1,255
        DO I=1,4
          IHSC(I,L)  = 0
        ENDDO
      ENDDO

      IF(IS.EQ.0) RETURN

C  CALCULATE INCREMENTS.
C  ---------------------

   10 CONTINUE
      IF(PS(IS).LT.BMISS .AND. GESPS(IS).LT.BMISS) THEN
        PIS(IS) = PS(IS) - GESPS(IS)
      ENDIF
      DO L=1,NLEV

        I = MANLEV(POB(L))
        IF(I.NE.0) THEN
          IF(ZOB(L).LT.BMISS .AND. ZFC(L).LT.BMISS) THEN
            ZIM(I,IS)  = ZOB(L) - ZFC(L)
            NZIM(I,IS) = MIN1(20.,20.*ABS(ZIM(I,IS))/XINC(I,1))
            IF(ITIME.EQ.2) THEN
            XZIM(I,IS) = (ZOB(L)-ZFC(L)-AVGI(I,1))
     &                   /MAX(.1*XINC(I,1),STDI(I,1))
            ENDIF
          ENDIF
          IF(TOB(L).LT.BMISS .AND. TFC(L).LT.BMISS) THEN
            TIM(I,IS)  = TOB(L) - TFC(L)
            NTIM(I,IS) = MIN1(20.,20.*ABS(TIM(I,IS))/XINC(I,2))
            IF(ITIME.EQ.2) THEN
              XTIM(I,IS) = (TOB(L)-TFC(L)-AVGI(I,2))
     &                     /MAX(.1*XINC(I,2),STDI(I,2))
            ENDIF
          ENDIF
          IF(TDO(L).LT.BMISS .AND. TDFC(L).LT.BMISS
     &      .AND. TOB(L).LT.BMISS) THEN
            TDIM(I,IS) = TDO(L) - TDFC(L)
            NTDIM(I,IS)= MIN1(20.,20.*ABS(TDIM(I,IS))/XINC(I,3))
            IF(ITIME.EQ.2) THEN
            XTDIM(I,IS)= (TDIM(I,IS)-AVGI(I,3))
     &                   /MAX(.1*XINC(I,3),STDI(I,3))
            ENDIF
          ENDIF
          IF(QOB(L).LT.BMISS .AND. QFC(L).LT.BMISS) THEN
            QIM(I,IS)  = QOB(L) - QFC(L)
            IF(ITIME.EQ.2) THEN
            XQIM(I,IS) = (ABS(QOB(L)-QFC(L)-AVGI(I,4)))
     &                   /MAX(.1*XINC(I,4),STDI(I,4))
            ENDIF
          ENDIF
        ENDIF

        J = NMANLV(POB(L))        ! index of nearest mand lvl
	IF(J.NE.0) THEN
          IF(POB(L).LT.BMISS .AND. PFC(L).LT.BMISS) THEN
            PI(L)   = POB(L) - PFC(L)
          ENDIF
          IF(ZOB(L).LT.BMISS .AND. ZFC(L).LT.BMISS) THEN
            ZI(L)   = ZOB(L) - ZFC(L)
            NZI(L)  = MIN1(20.,20.*ABS(ZI(L))/XINC(J,1))
            IF(ITIME.EQ.2) THEN
              XZI(L)  = (ZOB(L)-ZFC(L)-AVGI(J,1))
     &                  /MAX(.1*XINC(J,1),STDI(J,1))
            ENDIF
          ENDIF
          IF(TOB(L).LT.BMISS .AND. TFC(L).LT.BMISS) THEN
            TI(L)   = TOB(L) - TFC(L)
            NTI(L)  = MIN1(20.,20.*ABS(TI(L))/XINC(J,2))
            IF(ITIME.EQ.2) THEN
              XTI(L)  = (TOB(L)-TFC(L)-AVGI(J,2))
     &                  /MAX(.1*XINC(J,2),STDI(J,2))
            ENDIF
          ENDIF
          IF(TDO(L).LT.BMISS .AND. TDFC(L).LT.BMISS
     &      .AND. TOB(L).LT.BMISS) THEN
            TDI(L)  = TDO(L) - TDFC(L)
            NTDI(L) = MIN1(20.,20.*ABS(TDI(L))/XINC(J,3))
            IF(ITIME.EQ.2) THEN
              XTDI(L) = (TDI(L)-AVGI(J,3))
     &                  /MAX(.1*XINC(J,3),STDI(J,3))
            ENDIF
          ENDIF
          IF(QOB(L).LT.BMISS .AND. QFC(L).LT.BMISS) THEN
            QI(L)   = QOB(L) - QFC(L)
            NQI(L)  = MIN1(20.,20.*ABS(QI(L))/XINC(J,4))
            IF(ITIME.EQ.2) THEN
              XQI(L)  = (QOB(L)-QFC(L)-AVGI(J,4))
     &                  /MAX(.1*XINC(J,4),STDI(J,4))
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INCRW       CALCULATE INCREMENTS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Calculate increments for all wind variables.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL INCRW
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE INCRW
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /ALSNDW/  POBW(255), PFCW(255), PQMW(255), PRCW(255),
     &                 ZOBW(255), ZFCW(255), ZQMW(255), ZRCW(255),
     &                 UOB(255),  VOB(255),  UFC(255),  VFC(255),
     &                 WQM(255),  WRC(255),  SP(255),   DIR(255),
     &                 CATW(255)
      COMMON /MANRSW/  UIM(21,NST), VIM(21,NST), SPIM(21,NST),
     &                 DIRIM(21,NST)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      CHARACTER*8      SID
      LOGICAL PRNT

      DATA ISTART /0/
      BMIS = 0.5*BMISS

C  INITIALIZE VALUES (INCLUDING SOME NON-INCREMENT FIELDS)
C  -------------------------------------------------------

      IF(ISTART.EQ.0 .OR. IS.EQ.1) THEN
        DO N=1,NST
          DO L=1,21
            UIM(L,N)   = BMISS
            VIM(L,N)   = BMISS
            SPIM(L,N)  = BMISS
            DIRIM(L,N) = BMISS
          ENDDO
        ENDDO
        ISTART = 1
      ENDIF

      IF(IS.EQ.0) RETURN

C  CALCULATE INCREMENTS.
C  ---------------------

      DO L=1,NLEV

        I = MANLEV(POBW(L))
        IF(I.NE.0) THEN
          IF(UOB(L).LT.BMIS .AND. UFC(L).LT.BMIS) THEN
            UIM(I,IS)  = UOB(L) - UFC(L)
          ENDIF
          IF(VOB(L).LT.BMIS .AND. VFC(L).LT.BMIS) THEN
            VIM(I,IS)  = VOB(L) - VFC(L)
          ENDIF
          IF(UFC(L).LT.BMIS .AND. VFC(L).LT.BMIS) THEN
            IF(VFC(L).EQ.0.) VFC(L) = .01
            CALL W3FC05(-UFC(L),-VFC(L),DIRFC,SPFC)
            IF(SP(L).LT.BMIS) THEN
              SPIM(I,IS)  = SP(L) - SPFC
            ENDIF
            IF(DIR(L).LT.BMIS) THEN
              DIRIM(I,IS)  = AMOD(DIR(L)-DIRFC+360.,360.)
              IF(DIRIM(I,IS).GT.180.) DIRIM(I,IS) = DIRIM(I,IS)-360.
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INIT        INITIALIZE HORIZONTAL RESIDUALS.
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-12-09
C
C ABSTRACT: Initailize horizontal residuals when they will not be
C   calculated by the horizontal check.
C
C PROGRAM HISTORY LOG:
C 1997-12-09  W. Collins  Original author.
C
C USAGE:    CALL INIT
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE INIT
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL ZG, TG, TDG, QG, ERROR
      DO J=1,NST
        DO I=1,21
          ZHM(I,J)  = BMISS
          THM(I,J)  = BMISS
          TDHM(I,J) = BMISS
          QHM(I,J)  = BMISS
        ENDDO
      ENDDO
      RETURN
      END

C***************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INPUT       READ INPUT FOR ONE STATION
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Read input for one station.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2001-05-11  W. Collins  Convert guess temperature from Tv to T,
C     so that the residuals are comparing comparable quantities.
C
C USAGE:    CALL INPUT(START,ENDIN,SKIP,SAME,WIND,ITIME,USESQN)
C   INPUT ARGUMENT LIST:
C     START    - TRUE ON FIRST CALL
C     ENDIN    - TRUE AT END OF INPUT
C     SKIP     - TRUE FOR STATIONS TO SKIP (OUT OF REGION, ETC.)
C     SAME     - TRUE WHEN WIND PART GOES WITH PREVIOUSL READ
C                MASS PART
C     WIND     - TRUE FOR WIND PART OF REPORT
C     ITIME    - 1 - FIRST CALL TO INPUT FOR STATION
C                2 - SECOND CALL TO INPUT FOR STATION
C     USESQN   - TRUE WHEN SQN IS USED TO FIT PARTS OF REPORT
C                TOGETHER
C
C   OUTPUT ARGUMENT LIST:
C   INPUT ARGUMENT LIST:
C     START    - TRUE ON FIRST CALL
C     ENDIN    - TRUE AT END OF INPUT
C     SKIP     - TRUE FOR STATIONS TO SKIP (OUT OF REGION, ETC.)
C     SAME     - TRUE WHEN WIND PART GOES WITH PREVIOUSLY READ
C                MASS PART
C     WIND     - TRUE FOR WIND PART OF REPORT
C
C   INPUT FILES:
C     UNIT NFIN - UNIT NUMBER OF INPUT PREPBUFR FILE
C
C   OUTPUT FILES:
C     UNIT NFOUT - UNIT NUMBER OF OUTPUT PREPBUFR FILE
C     UNIT 06    - PRINTOUT
C     UNIT 60    - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE INPUT(START,ENDIN,SKIP,SAME,WIND,ITIME,USESQN)
      SAVE
      PARAMETER (NST=1500)      ! maximum number of stations
      PARAMETER (MVO=5)        ! p,T,z,q,Td
      PARAMETER (MLV=255)      ! number of possible levels
      PARAMETER (MEV=13)       ! number of possible programs/events
      PARAMETER (MP=0)         ! program code for data

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /MANSND/  ZM(21,NST), TM(21,NST), TDM(21), QM(21), MANDL,
     &                 MANDF, ZFM(21), TFM(21), QFM(21), PM(21)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),ERROR(NST)
      COMMON /ALSNDW/  POBW(255), PFCW(255), PQMW(255), PRCW(255),
     &                 ZOBW(255), ZFCW(255), ZQMW(255), ZRCW(255),
     &                 UOB(255),  VOB(255),  UFC(255),  VFC(255),
     &                 WQM(255),  WRC(255),  SP(255),   DIR(255),
     &                 CATW(255)
      COMMON /DATEX/   JDATE(5), CDATE
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      COMMON /STN/     IS
      COMMON /CONSTS/  R, G, T0, CP, RV
      COMMON /STNKNT/  ISHIP,IBLK(100)
      COMMON /TESTS/   TEST
      LOGICAL          TEST, USESQN, PRNT
      INTEGER          IV(255)
      REAL(8)          BMISS,HDR_8(10),SF0_8(10),HDR2_8(10)
      DIMENSION        OBS(MVO,MLV,0:MEV,4),
     &                 FCST(10,255), QMKS(10,255),
     &                 RCDS(10,255), OBSW(10,255), OBSW2(10,255),
     &                 ZTMP(21),TTMP(21)

C  OBS((p,T,z,q,Td),no. levs, no. prog/events,(obs,fcst,quality,reason))
C  ---------------------------------------------------------------------

      CHARACTER*8      SID, SUBSET, CDR(10), PRG(0:13)
      CHARACTER*10     CDATE
      CHARACTER*40     HSTR, OSTR, FCSTR, RCSTR, C1STR, CAT0,
     &                 C1FSTR, OSTRW, OSTRW2
      CHARACTER*50     OBSTR(2,5)
      CHARACTER*5      CMAXOUT
      LOGICAL          START, ENDIN, WIND, SKIP, SAME
      LOGICAL          ZG, TG, TDG, QG, ERROR
      EQUIVALENCE      (HDR_8,CDR)
      INTEGER          IORDER(0:13)
      INTEGER          IMAXOUT, LENGTH

      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      DATA IORDER /0,1,2,3,4,7,5,11,12,6,8,9,10,13/
      DATA HSTR   /'SID XOB YOB DHR ELV ITP TYP T29 SQN     '/
      DATA OSTR   /'POB ZOB TOB TDO QOB CAT                 '/
      DATA FCSTR  /'PFC ZFC TFC QFC                         '/
      DATA RCSTR  /'PRC ZRC TRC QRC                         '/
      DATA C1STR  /'CAT=1 POB ZOB TOB TDO QOB CAT           '/
      DATA C1FSTR /'CAT=1 PFC ZFC TFC     QFC               '/
      DATA OSTRW  /'POB ZOB UOB VOB PFC ZFC UFC VFC PQM ZQM '/
      DATA OSTRW2 /'WQM PRC ZRC WRC CAT                     '/
      DATA CAT0   /'CAT=0 POB PFC                           '/
      DATA OBSTR /'POB TOB ZOB QOB TDO' , 'POB UOB VOB ZOB ZOB',
     &            'PFC TFC ZFC QFC QFC' , 'PFC UFC VFC ZFC ZFC',
     &            'PQM TQM ZQM QQM QQM' , 'PQM WQM WQM ZQM ZQM',
     &            'PPC TPC ZPC QPC QPC' , 'PPC WPC WPC ZPC ZPC',
     &            'PRC TRC ZRC QRC QRC' , 'PRC WRC WRC ZRC ZRC'/
      DATA PRG /' ','PREPRO','SYNDATA','CLIMO','PREVENT',
     &          'PREPACQC','CQCHT','PROFCQC','CQCVAD','RADCOR',
     &          'VIRTMP','OIQC','SSI','R3DVAR'/

      DATA PPI /57.29578/
      DATA IPCNT /0/
      BMAX = 0.5*BMISS

      ENDIN = .FALSE.
      SKIP  = .FALSE.
      WIND  = .FALSE.

C  OPEN THE INPUT FILE
C  -------------------

      IF(START) THEN
        CALL DATELEN(10)
        CALL CLOSBF(NFIN)
        REWIND NFIN
        CALL OPENBF(NFIN,'IN',NFIN)
        CALL READMG(NFIN,SUBSET,IDATE,IRETMG)
        IF(IRETMG.NE.0) GOTO 6
        WRITE(CDATE,'(I10)') IDATE
        READ (CDATE,'(5I2)') JDATE
        PRINT*,'DATA VALID AT ',CDATE
        CALL UFBQCD(NFIN,'CQCHT',CQCPC)
        CALL UFBQCD(NFIN,'VIRTMP',VTPPC)
        CALL UFBQCD(NFIN,'RADCOR',RADPC)
        CALL CLOSBF(NFIN)
        REWIND NFIN
        CALL OPENBF(NFIN,'IN',NFIN)
        IF(ITIME.EQ.2) CALL OPENBF(NFOUT,'OUT',NFIN)
        CALL GET_ENVIRONMENT_VARIABLE('BUFR_MAXOUT',cmaxout,length)
        if (length > 0) then
          read(cmaxout,*) imaxout
          if (imaxout > 15000) then
            call maxout(imaxout)
          else
            call maxout(15000)
          end if
        else
          call maxout(15000)
        end if

        IS = 0
        DO I=1,NST
          SQN(I) = 0.
        ENDDO
        SQNOLD = 0
        SIDOLD = 0
        START = .FALSE.
        IRETSB = 1
      ENDIF

C  READ A REPORT
C  -------------

    5 CONTINUE
      IF(IRETSB.NE.0) THEN
        CALL READMG(NFIN,SUBSET,IDATE,IRETMG)
        IF(TEST) WRITE(60,501) NFIN,SUBSET,IDATE,IRETMG
  501   FORMAT(' INPUT--READMG CALLED: NFIN,SUBSET,IDATE,IRETMG = ',
     &    I5,2X,A8,2X,I10,2X,I5)
      ENDIF
    6 IF(IRETMG.NE.0) THEN
        ENDIN = .TRUE.
        CALL CLOSBF(NFIN)
        CALL CLOSBF(NFOUT)
        RETURN
      ENDIF

C  UNPACK AN ADPUPA SUBSET
C  -----------------------

   10 CONTINUE
      IF(ITIME.EQ.2) THEN
        IF(SUBSET.NE.'ADPUPA') THEN
          CALL CLOSMG(NFOUT)
          CALL COPYMG(NFIN,NFOUT)
          GOTO 5
        ELSE
          CALL OPENMB(NFOUT,SUBSET,IDATE)
        ENDIF
      ELSEIF(SUBSET.NE.'ADPUPA') THEN
        GOTO 5
      ENDIF
      CALL READSB(NFIN,IRETSB)
      IF(TEST) WRITE(60,502) IRETSB
  502 FORMAT(' INPUT--READSB CALLED: IRETSB = ',I5)
      IF(IRETSB.NE.0) GOTO 5
      IF(ITIME.EQ.2) CALL UFBCPY(NFIN,NFOUT)

        CALL UFBINT(NFIN,HDR_8,10,  1,IRET,HSTR)
        CALL UFBINT(NFIN,SF0_8,10,  1,ISF0,CAT0)

C  SEVERAL CONDITIONALS FOR PROCESSING THE ADPUPA REPORTS
C  ------------------------------------------------------

        TYP     = HDR_8(7)
        T29     = HDR_8(8)

        SKIP = SKIP .OR. HDR_8(2).LT.XMI. OR. HDR_8(2).GT.XMA
     &    .OR. HDR_8(3).LT.YMI .OR. HDR_8(3).GT.YMA
     &    .OR. T29.LT.11 .OR. (T29.GT.13.AND.T29.LT.21) .OR. T29.GT.23
     &    .OR. MOD(TYP,100.).NE.20

        WIND = TYP.GE.200
        IF(WIND) THEN
          IF(IS.EQ.0) IS = 1
          ITP(IS) = 20000
        ENDIF
        IF(USESQN) THEN
          IF(.NOT.WIND .AND. HDR_8(9).NE.SQNOLD) THEN
            IS = IS + 1
            SQNOLD = HDR_8(9)
            SAME = .FALSE.
          ELSE
            SAME = .TRUE.
          ENDIF
        ELSE
          IF(.NOT.WIND .AND. HDR_8(1).NE.SIDOLD) THEN
            IS = IS + 1
            SIDOLD = HDR_8(1)
            SAME = .FALSE.
          ELSE
            SAME = .TRUE.
          ENDIF
        ENDIF

        IF(IS+1.GT.NST) THEN
            PRINT *, 'MAXOBS (NST) EXCEEDED IN INPUT - STOP 99'
            CALL W3TAGE('PREPOBS_CQCBUFR')
            CALL ERREXIT(99)
        ENDIF

        NOBS = IS
        SID(IS) = CDR(1)
        XOB(IS) = HDR_8(2)
        YOB(IS) = HDR_8(3)
        DHR(IS) = HDR_8(4)
        ELV(IS) = HDR_8(5)
        ITP(IS) = HDR_8(6)
        SQN(IS) = HDR_8(9)
        ISF(IS) = 0

        IF(.NOT.WIND .AND. ITIME.EQ.1) THEN
          IDENT = 0
          READ(SID(IS),'(I5)',ERR=15) IDENT

          IF(T29.LE.13) THEN
            IBL = IDENT/1000
            IF(IBL.EQ.0) IBL = 100
            IBL = MIN(IBL,100)
            IBLK(IBL) = IBLK(IBL)+1
          ENDIF

15        IF(IDENT.EQ.0) THEN
             ISHIP = ISHIP + 1
             IDENT = 10000 + ISHIP
          ENDIF
        ENDIF

C       WRITE(6,503) START,ITIME,IS,IDENT,SKIP
        IF(TEST) WRITE(60,503) START,ITIME,IS,IDENT,SKIP
  503   FORMAT(' INPUT--START,ITIME,IS,IDENT,SKIP: ',L2,3I8,L5)

C  STORE THE SURFACE DATA AND FIRST GUESS VALUES
C  ---------------------------------------------

        PS(IS) = BMISS
        GESPS(IS) = BMISS
        PIS(IS) = BMISS
        IF(ELV(IS).LT.BMISS .AND. ISF0.EQ.1) THEN
          PS(IS)  = SF0_8(1)
          IF(SF0_8(2).LT.BMISS) THEN
            GESPS(IS) = SF0_8(2)
            PIS(IS)   = PS(IS) - GESPS(IS)
          ELSE
            GESPS(IS) = BMISS
            PIS(IS) = 0.
          ENDIF
        ENDIF

C  IT'S GOING TO BE NECESSARY TO SORT ACCORDING TO PRESSURE
C  SOMETIMES THEY ARE OUT OF ORDER!!
C  ---------------------------------

c --> error discovered by Christopher Redder NASA/GSFC, fixed
c      3/17/2004
ccccccccCALL GETOB(NFIN,MP,HDR2_8,NLEV,CAT,HRDR,XDR,YDR,OBS)
        CALL GETOB(NFIN,MP,HDR2_8,NLEV,CAT,HRDR,YDR,XDR,OBS)
        IF(MP.NE.0) CALL FILL(OBS,NLEV)
        NLV = NLEV

C  PRINT SELECTED RESULTS
C  ----------------------

        JKX = INT(HDR_8(7))/100
        IF(IPCNT.LT.10000) THEN
            IPCNT = IPCNT + 1
            WRITE(60,600) SUBSET,SID(IS),CDATE,(HDR2_8(I),I=2,9)
            IF(JKX.EQ.1) WRITE(60,601) NLV
            IF(JKX.EQ.2) WRITE(60,603) NLV
            DO L=1,NLV
                MISS = 0
                DO J=1,4
                  DO I=1,MVO
C                   IF(OBS(I,L,MP,J).GE.BMAX) MISS = MISS + 1
                  ENDDO
                ENDDO
                IF(MISS.LT.4*MVO) THEN
                  IF(JKX.EQ.1) THEN
                    WRITE(60,602) PRG(IORDER(MP)),((OBS(I,L,MP,J),
     &                J=1,4),I=1,4),OBS(5,L,MP,1),CAT(L)
                  ELSEIF(JKX.EQ.2) THEN
                    WRITE(60,604) PRG(IORDER(MP)),((OBS(I,L,MP,J),
     &                J=1,4),I=1,3),CAT(L)
                  ENDIF
                ENDIF
            ENDDO
        ENDIF

  600 FORMAT(/1X,A8,2X,A8,2X,A10,2X,4F8.2,4F5.0)
  601 FORMAT(' PROGRAM        POB     PFC PQM PRC     TOB     TFC',
     &  ' TQM TRC     ZOB     ZFC ZQM ZRC',
     &          '     QOB     QFC QQM QRC     TDO CAT  NLV=',I5)
  603 FORMAT(' PROGRAM        POB     PFC PQM PRC     UOB     UFC',
     &  ' WQM WRC     VOB     VFC WQM WRC CAT  NLV=',I5)
  602 FORMAT(1X,A8,2X,4(1X,F7.1,1X,F7.1,1X,F3.0,1X,F3.0),
     &  1X,F7.1,1X,F3.0)
  604 FORMAT(1X,A8,2X,3(1X,F7.1,1X,F7.1,1X,F3.0,1X,F3.0),1X,F3.0)

C       WRITE(6,605) TYP,WIND,NLV
  605   FORMAT(' INPUT--TYP,WIND,NLV: ',F7.0,2X,L2,2X,I4)

        IF(TYP.GE.BMISS) THEN
           PRINT *, 'INPUT - NOT A PREPBUFR FILE - STOP 99'
           CALL W3TAGE('PREPOBS_CQCBUFR')
           CALL ERREXIT(99)
        ENDIF

        IF(.NOT. WIND) THEN
          DO L=1,MAND
            ZM(L,IS) = BMISS
            TM(L,IS) = BMISS
          ENDDO
          TDM = BMISS
          QM  = BMISS
          ZFM = BMISS
          TFM = BMISS
          QFM = BMISS
          POB = BMISS
          ZOB = BMISS
          TOB = BMISS
          TDO = BMISS
          TVO = BMISS
          UOB = BMISS
          VOB = BMISS
          QOB = BMISS
          PQM = BMISS
          TQM = BMISS
          ZQM = BMISS
          QQM = BMISS
          WQM = BMISS
          PFC = BMISS
          ZFC = BMISS
          TFC = BMISS
          QFC = BMISS
          UFC = BMISS
          VFC = BMISS
          PPC = BMISS
          TPC = BMISS
          ZPC = BMISS
          QPC = BMISS
          WPC = BMISS
          PRC = BMISS
          ZRC = BMISS
          TRC = BMISS
          QRC = BMISS
          WRC = BMISS
          NLVM = NLV
          ISF = 0
          LSFC = 0
          DO LL=1,NLVM
           L = MANLEV(OBS(1,LL,MP,1))
            IF(L.NE.0 .AND. CAT(LL).EQ.1) THEN
              PM(L)  = OBS(1,LL,MP,1)
              ZM(L,IS)  = OBS(3,LL,MP,1)
              TM(L,IS)  = OBS(2,LL,MP,1)
              TDM(L) = OBS(5,LL,MP,1)
              IF(OBS(4,LL,MP,1).LT.BMISS) QM(L) = OBS(4,LL,MP,1)*1.0E-6
              ZFM(L) = OBS(3,LL,MP,2)
              TFM(L) = OBS(2,LL,MP,2)
              IF(OBS(4,LL,MP,2).LT.BMISS) QFM(L) = OBS(4,LL,MP,2)*1.0E-6
              MANDL = L
            ENDIF
          ENDDO
          IV = 0
          CALL SHELL(PM,IV,MANDL,1)
          CALL SORT(ZM(1,IS),IV,MANDL)
          CALL SORT(TM(1,IS),IV,MANDL)
          CALL SORT(TDM,IV,MANDL)
          CALL SORT(QM,IV,MANDL)
          CALL SORT(ZFM,IV,MANDL)
          CALL SORT(TFM,IV,MANDL)
          CALL SORT(QFM,IV,MANDL)

C  PRINT A FEW...
C  --------------

          IF(IPCNT.LT.10) THEN
            WRITE(60,610)
            WRITE(60,611) (PM(I),TM(I,IS),TFM(I),ZM(I,IS),ZFM(I),
     &                    TDM(I),QM(I),QFM(I),I=1,MANDL)
  610       FORMAT('   PRESS    TEMP  T-FCST      HT  Z-FCST',
     &             '      TD       Q  Q-FCST')
  611       FORMAT(1X,F7.1,2F8.1,2F8.0,F8.1,2F8.5)
          ENDIF

          DO L=1,NLV
            IND(L) = L
            POB(L) = OBS(1,L,MP,1)
            ZOB(L) = OBS(3,L,MP,1)
            TOB(L) = OBS(2,L,MP,1)
            IF(TOB(L).LT.BMISS .AND. OBS(5,L,MP,1).LT.BMISS) THEN
              TDO(L) = TOB(L) - OBS(5,L,MP,1)   ! Store as depression
            ENDIF
            IF(OBS(4,L,MP,1).LT.BMISS) THEN
              QOB(L) = OBS(4,L,MP,1)*1.0E-6
              TVO(L)  = (TOB(L)+T0)*(1.+.61*QOB(L)) - T0
            ENDIF
            PQM(L) = OBS(1,L,MP,3)
            ZQM(L) = OBS(3,L,MP,3)
            TQM(L) = OBS(2,L,MP,3)
            QQM(L) = OBS(4,L,MP,3)
            PFC(L) = OBS(1,L,MP,2)
            ZFC(L) = OBS(3,L,MP,2)
            TFC(L) = OBS(2,L,MP,2)
            IF(OBS(4,L,MP,2).LT.BMISS) THEN
              QFC(L) = OBS(4,L,MP,2)*1.0E-6
C  If possible, convert T-guess from Tv to T.
              TFC(L) = (TFC(L)+T0)/(1.+.61*QFC(L)) - T0
            ENDIF
            PRC(L) = OBS(1,L,MP,4)
            ZRC(L) = OBS(3,L,MP,4)
            TRC(L) = OBS(2,L,MP,4)
            QRC(L) = OBS(4,L,MP,4)
            IF(CAT(L).EQ.0) THEN
              ISF(IS) = L
              LSFC    = L
            ENDIF
            IF(TFC(L).NE.BMISS .AND. QFC(L).NE.BMISS .AND.
     &         POB(L).NE.BMISS) THEN
              Q     = QFC(L)
              WMIX  = Q/(1.-Q)
              ES    = WMIX*POB(L) / (.622 + WMIX)
              IF(ES.GT.0.) THEN
                TDFC(L) = (35.85*(ALOG(ES)-ALOG(6.1078))-T0*17.269)
     &                / (ALOG(ES)-ALOG(6.1078)-17.269) - T0
                IF(TDFC(L).GT.TFC(L)) TDFC(L) = TFC(L)
                TDFC(L) = TFC(L) - TDFC(L)  ! Change to depression
              ELSE
                TDFC(L) = BMISS
              ENDIF
            ELSE
              TDFC(L) = BMISS
            ENDIF
          ENDDO
          IV = 0
          CALL SHELL(POB,IV,NLV,1)
          CALL ISORT(IND,IV,NLV)
          CALL SORT(ZOB,IV,NLV)
          CALL SORT(TOB,IV,NLV)
          CALL SORT(TDO,IV,NLV)
          CALL SORT(QOB,IV,NLV)
          CALL SORT(CAT,IV,NLV)
          CALL SORT(HRDR,IV,NLV)
          CALL SORT(YDR,IV,NLV)
          CALL SORT(XDR,IV,NLV)
          CALL SORT(TVO,IV,NLV)
          CALL SORT(PQM,IV,NLV)
          CALL SORT(ZQM,IV,NLV)
          CALL SORT(TQM,IV,NLV)
          CALL SORT(QQM,IV,NLV)
          CALL SORT(PFC,IV,NLV)
          CALL SORT(ZFC,IV,NLV)
          CALL SORT(TFC,IV,NLV)
          CALL SORT(QFC,IV,NLV)
          CALL SORT(PRC,IV,NLV)
          CALL SORT(ZRC,IV,NLV)
          CALL SORT(TRC,IV,NLV)
          CALL SORT(QRC,IV,NLV)
          CALL SORT(TDFC,IV,NLV)
          ISURF = 0

C  PRINT A FEW...
C  --------------

          IF(IPCNT.LT.1000) THEN
            WRITE(60,612)
            WRITE(60,613) (IND(I),POB(I),TOB(I),TFC(I),ZOB(I),ZFC(I),
     &                    TDO(I),QOB(I),QFC(I),QQM(I),CAT(I),I=1,NLV)
  612       FORMAT('  IND   PRESS    TEMP  T-FCST      HT  Z-FCST',
     &             '      TD       Q  Q-FCST  QQM  CAT',
     &             '  (in INPUT, after sorts)')
  613       FORMAT(1X,I4,F8.1,2F8.1,2F8.0,F8.1,2F8.5,2F5.0)
          ENDIF

          DO L=1,NLV
            MANDF = L
            IF(CAT(L).EQ.0) ISURF = 1
            IF(ISURF.EQ.1 .AND. CAT(L).EQ.1) GOTO 20
          ENDDO
   20     CONTINUE
          ICOUNT = ICOUNT + 1
        ELSE
          POBW = BMISS
          ZOBW = BMISS
          UOB  = BMISS
          VOB  = BMISS
          SP   = BMISS
          DIR  = BMISS
          PFCW = BMISS
          ZFCW = BMISS
          UFC  = BMISS
          VFC  = BMISS
          PQMW = BMISS
          ZQMW = BMISS
          WQM  = BMISS
          PRCW = BMISS
          ZRCW = BMISS
          WRC  = BMISS
          CATW = BMISS
          ISF  = 0
          LSFC = 0
          NLVW = NLV
          DO L=1,NLV
            IND(L) = L
            POBW(L) = OBS(1,L,MP,1)
            ZOBW(L) = OBS(4,L,MP,1)
            UOB(L)  = OBS(2,L,MP,1)
            VOB(L)  = OBS(3,L,MP,1)
            SP(L)   = SQRT(UOB(L)**2 + VOB(L)**2)
            IF(VOB(L).NE.0. .AND. UOB(L).NE.0.) THEN
              DIR(L)  = 90. - PPI * ATAN2(VOB(L),UOB(L))
            ENDIF
            PFCW(L) = OBS(1,L,MP,2)
            ZFCW(L) = OBS(4,L,MP,2)
            UFC(L)  = OBS(2,L,MP,2)
            VFC(L)  = OBS(3,L,MP,2)
            PQMW(L) = OBS(1,L,MP,3)
            ZQMW(L) = OBS(4,L,MP,3)
            WQM(L)  = OBS(2,L,MP,3)
            PRCW(L) = OBS(1,L,MP,4)
            ZRCW(L) = OBS(4,L,MP,4)
            WRC(L)  = OBS(2,L,MP,4)
            CATW(L) = CAT(L)
            IF(CATW(L).EQ.0) THEN
              ISF(IS) = L
              LSFC    = L
            ENDIF
          ENDDO
        ENDIF

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INPUT2      READ INPUT FOR ALL STATIONS FOR TMP CHK
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-05-18
C
C ABSTRACT: Read input for all stations for temporal check.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2016-05-18  D. Keyser   Corrected an integer overflow issue which prevented
C     the temporal check from running in the CDAS network (the only network
C     where it currently would run).
C
C USAGE:    CALL INPUT2
C
C   INPUT FILES:
C     UNIT NFTMP(1) - INPUT PREPBUFR FILE at T--
C     UNIT NFTMP(2) - INPUT PREPBUFR FILE at T-
C     UNIT NFTMP(3) - INPUT PREPBUFR FILE at T+
C     UNIT NFTMP(4) - INPUT PREPBUFR FILE at T++
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE INPUT2
      PARAMETER(NST=1500)

      REAL(8) BMISS,RIT_8,HDR_8(10),UPA_8(10,255)

      COMMON /TMPSND/  ZOBT(21,NST,4), TOBT(21,NST,4),
     &                 ZIT(21,NST,4),  TIT(21,NST,4),
     &                 ZTMP(21,NST),   TTMP(21,NST),
     &                 NZTMP(21,NST),  NTTMP(21,NST),
     &                 ITERR(4)
      COMMON /DATEX/   JDATE(5), CDATE
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      CHARACTER*8      SID, SUBSET, CIT
      CHARACTER*10     CDATE
      CHARACTER*40     HSTR, USTR
      DIMENSION        IIT(4), NFTMP(4)
      REAL             RINC(5)
      INTEGER          IDAT(8), JDAT(8)
      LOGICAL          PRNT
      EQUIVALENCE      (RIT_8,CIT)

      DATA HSTR  /'SID XOB YOB DHR ELV T29 TYP             '/
      DATA USTR  /'CAT=1 POB ZOB TOB ZFC TFC               '/
      DATA IIT   /-24,-12,12,24/
      DATA NFTMP /17,18,19,20/

C  INITIALIZE THE ARRAYS FOR TEMPORAL CHECK
C  ----------------------------------------

      DO K=1,4
        ITERR(K) = 0
      ENDDO
      DO J=1,NST
        DO I=1,21
          ZTMP(I,J) = BMISS
          TTMP(I,J) = BMISS
          DO K=1,4
            ZOBT(I,J,K) = BMISS
            TOBT(I,J,K) = BMISS
            ZIT(I,J,K)  = BMISS
            TIT(I,J,K)  = BMISS
          ENDDO
        ENDDO
      ENDDO

C  BEGIN LOOP ON TIMES FOR TEMPORAL CHECK
C  --------------------------------------

      DO IT=1,4
        MATCH = 0
        REWIND NFTMP(IT)
        READ(NFTMP(IT),END=200)
        WRITE(6,500) NFTMP(IT), IIT(IT)
  500   FORMAT(1X,'INPUT2--READING FROM FILE ',I5,' AT TIME = ',I5)

C  FILE IS ATTACHED, OPEN IT AND CHECK THE DATE/TIME
C  -------------------------------------------------

        CALL DATELEN(10)
        CALL OPENBF(NFTMP(IT),'IN',NFTMP(IT))
    5   CONTINUE
        CALL READMG(NFTMP(IT),SUBSET,IDATE,IRET)
        IF(SUBSET.NE.'ADPUPA') THEN
          IF(IRET.NE.0) GOTO 200
          GOTO 5
        ENDIF

        RINC(1) = 0
        RINC(2) = IIT(IT)
        RINC(3) = 0
        RINC(4) = 0
        RINC(5) = 0
        IDAT(1) = JDATE(1)*100 + JDATE(2)
        IDAT(2) = JDATE(3)
        IDAT(3) = JDATE(4)
        IDAT(4) = 0
        IDAT(5) = JDATE(5)
        IDAT(6) = 0
        IDAT(7) = 0
        IDAT(8) = 0
        CALL W3MOVDAT(RINC,IDAT,JDAT)
        KDATE = JDAT(1)*1000000 + JDAT(2)*10000 + JDAT(3)*100 + JDAT(5)
        IF(IDATE.NE.KDATE) GOTO 200

C  READ AND STORE ALL MATCHING DATA FROM THIS TIME
C  -----------------------------------------------

   10   CALL READSB(NFTMP(IT),IRET)
        IF(IRET.NE.0) THEN
   20     CALL READMG(NFTMP(IT),SUBSET,IDATE,IRET)
          IF(SUBSET.NE.'ADPUPA') THEN
            IF(IRET.NE.0) GOTO 100
            GOTO 20
          ENDIF
          GOTO 10
        ENDIF

        CALL UFBINT(NFTMP(IT),HDR_8,10,  1,IRET,HSTR)
        CALL UFBINT(NFTMP(IT),UPA_8,10,255,NRET,USTR)

        RIT_8 = HDR_8(1)
        X   = HDR_8(2)
        Y   = HDR_8(3)
        THR = HDR_8(4)
        ELV = HDR_8(5)
        RPT = HDR_8(6)

        IF(HDR_8(7).GT.200) GOTO 10

C  FIND A MATCHING STATION
C  -----------------------

        DO N=1,NOBS
          IF(CIT.EQ.SID(N) .AND. X.EQ.XOB(N) .AND. Y.EQ.YOB(N)) THEN
            MATCH = MATCH + 1
            DO L=1,NRET
              P = UPA_8(1,L)
              MANL = MANLEV(P)
              IF(MANL.GT.0 .AND. MANL.LE.MAND) THEN
                ZOBT(MANL,N,IT) = UPA_8(2,L)
                TOBT(MANL,N,IT) = UPA_8(3,L)
                IF(UPA_8(2,L).NE.BMISS .AND. UPA_8(4,L).NE.BMISS)
     &            ZIT(MANL,N,IT)  = UPA_8(2,L) - UPA_8(4,L)
                IF(UPA_8(3,L).NE.BMISS .AND. UPA_8(5,L).NE.BMISS)
     &            TIT(MANL,N,IT)  = UPA_8(3,L) - UPA_8(5,L)
              ENDIF
            ENDDO
C           WRITE(60,516) IT,N,(ZOBT(L,N,IT),L=1,NRET)
C 516       FORMAT(' INPUT2--IT,N,ZOBT: ',2I5,21F8.1)
C           WRITE(60,518) IT,N,(ZIT(L,N,IT),L=1,NRET)
C 518       FORMAT(' INPUT2--IT,N,ZIT: ',2I5,21F8.1)
C           WRITE(60,517) IT,N,(TOBT(L,N,IT),L=1,NRET)
C 517       FORMAT(' INPUT2--IT,N,TOBT: ',2I5,21F8.1)
C           WRITE(60,519) IT,N,(TIT(L,N,IT),L=1,NRET)
C 519       FORMAT(' INPUT2--IT,N,TIT: ',2I5,21F8.1)
            GOTO 10
          ENDIF
        ENDDO

        GOTO 10

C  NORMAL END TO READING OF FILE
C  -----------------------------

  100   WRITE(6,501) MATCH,NFTMP(IT)
  501   FORMAT(1X,I5,' OBS FOUND AT MATCHING STATIONS IN UNIT ',I2)
        GOTO 300

C  NO PROPER FILE FOUND AT THIS TIME
C  ---------------------------------

  200   ITERR(IT) = 1
        WRITE(6,502) IIT(IT)
  502   FORMAT(1X,'OFF-TIME UNAVAILABLE FOR ',I3)

  300   CONTINUE
        CALL CLOSBF(NFTMP(IT))
      ENDDO
      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ISGOOD      MAKE QUICK DIAGNOSIS OF ERRORS.
C   PRGMMR: KEYSER         ORG: NP22       DATE: 2013-02-05
C
C ABSTRACT: Make a quick estimate of which data are in error, using
C   hydrostatic, increment, vertical, and lapse rate residuals. Make
C   the determination only for mandatory levels.  These results are
C   used to determine which data are eligible to be used in the
C   horizontal check.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2008-10-08  Woollen/Keyser -- corrected if tests where integer values
C     were tested for not being equal to real value for missing (BMISS
C     = 10E10), these integer values can never be equal to BMISS for
C     I*4 due to overflow - instead they are now tested for not being
C     equal to new integer value for missing (IMISS, also = 10E10),
C     although this is also an overflow value for I*4, it results in a
C     correct evaluation
C 2013-02-05  D. Keyser -- Final changes to run on WCOSS: Set BUFRLIB
C     missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C     overflow (also done for IMISS).
C
C USAGE:    CALL ISGOOD
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE ISGOOD
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /LEVEL/   PMAND(21)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /STN/     IS
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST
      CHARACTER*8 SID
      LOGICAL          ERROR,ZG,TG,TDG,QG,SINGLE, PRNT

      DO I=1,21
        ZG(I,IS)    = .TRUE.
        TG(I,IS)    = .TRUE.
        TDG(I,IS)   = .TRUE.
        QG(I,IS)    = .TRUE.
      ENDDO
      ERROR(IS) = .FALSE.
      DO L=1,255
        DO I=1,4
          IHSC(I,L)  = 0
        ENDDO
      ENDDO
      L1    = 0
      LOW   = 1

C  DO PRELIMINARY QUALITY CHECK FOR MANDATORY LEVELS ONLY
C  ------------------------------------------------------

      IF(TEST) WRITE(60,503) SID(IS),NLV
  503 FORMAT(' ISGOOD--STN,NLV: ',A8,2X,I5)
      DO L=1,MAND
        LM = LMAND(L)
C       IF(LM.GT.255 .OR. LM.EQ.0) GOTO 100
        IF(LM.GT.255 .OR. LM.EQ.0) THEN
          IF(TEST) WRITE(60,504) L,LM,CAT(L),MANLEV(POB(L))
  504     FORMAT(' ISGOOD--L,LM,CAT,MANLEV: ',I5,2X,I12,F8.0,I5)
          GOTO 100
        ENDIF
        IF(L1.EQ.0) L1 = L
        ISUMZ  = 0
        ISUMT  = 0
        ISUMTD = 0
        ISUMQ  = 0
        ITSTZ  = 0
        ITSTT  = 0
        ITSTTD = 0
        ITSTQ  = 0
        IF(NZIM(L,IS).GE.10)      ISUMZ  = ISUMZ + 1
        IF(NZV(LM).GE.10)         ISUMZ  = ISUMZ + 1
C       IF(NZHM(L,IS).GE.10)      ISUMZ  = ISUMZ + 1
        IF(NTIM(L,IS).GE.10)      ISUMT  = ISUMT + 1
        IF(NTV(LM).GE.10)         ISUMT  = ISUMT + 1
C       IF(NTHM(L,IS).GE.10)      ISUMT  = ISUMT + 1
        IF(NTDIM(L,IS).GE.10)     ISUMTD = ISUMTD + 1
        IF(NTDV(LM).GE.10)        ISUMTD = ISUMTD + 1
C       IF(NTDHM(L,IS).GE.10)     ISUMTD = ISUMTD + 1
        IF(NQV(LM).GE.10)         ISUMQ  = ISUMQ + 1
C       IF(NQHM(L,IS).GE.10)      ISUMQ  = ISUMQ + 1
        IF(NZIM(L,IS).NE.-1)      ITSTZ  = ITSTZ + 1
        IF(NZV(LM).NE.-1)         ITSTZ  = ITSTZ + 1
C       IF(NZHM(L,IS).NE.-1)      ITSTZ  = ITSTZ + 1
        IF(NTIM(L,IS).NE.-1)      ITSTT  = ITSTT + 1
        IF(NTV(LM).NE.-1)         ITSTT  = ITSTT + 1
C       IF(NTHM(L,IS).NE.-1)      ITSTT  = ITSTT + 1
        IF(NTDIM(L,IS).NE.-1)     ITSTTD = ITSTTD + 1
        IF(NTDV(LM).NE.-1)        ITSTTD = ITSTTD + 1
C       IF(NTDHM(L,IS).NE.-1)     ITSTTD = ITSTTD + 1
        IF(NQV(LM).NE.-1)         ITSTQ  = ITSTQ + 1
C       IF(NQHM(L,IS).NE.-1)      ITSTQ  = ITSTQ + 1
        IF(L.EQ.L1) THEN
          IF(NHYDN(L1).GE.10) THEN
            ISUMZ = ISUMZ + 1
            ISUMT = ISUMT + 1
            ITSTZ = ITSTZ + 1
            ITSTT = ITSTT + 1
          ENDIF
          I = LMAND(L1)
          IF(NTL(I).GE.3) THEN
            ISUMT = ISUMT + 1
            ITSTT = ITSTT + 1
          ENDIF
        ELSEIF(L.EQ.LMAND(0)) THEN
          IF(NHYDN(LOW).GE.10) THEN
            ISUMZ = ISUMZ + 1
            ISUMT = ISUMT + 1
            ITSTZ = ITSTZ + 1
            ITSTT = ITSTT + 1
          ENDIF
          I = LMAND(LOW)
          IF(NTL(I).GE.3) THEN
            ISUMT = ISUMT + 1
            ITSTT = ITSTT + 1
          ENDIF
        ELSE
          IF(NHYD(LOW).GE.10 .OR. NHYDN(L).GE.10) THEN
            ISUMZ = ISUMZ + 1
            ISUMT = ISUMT + 1
            ITSTZ = ITSTZ + 1
            ITSTT = ITSTT + 1
          ENDIF
          IM = LMAND(LOW)
cdak      IF(IM.NE.BMISS) THEN
          IF(IM.NE.IMISS) THEN ! IM will never test = bmiss, use imiss
            IF(NTL(IM).GE.3) THEN
              ISUMT = ISUMT + 1
              ITSTT = ITSTT + 1
            ENDIF
          ENDIF
          I  = LMAND(L)
cdak      IF(I.NE.BMISS) THEN
          IF(I.NE.IMISS) THEN ! IM will never test = bmiss, use imiss
            IF(NTL(I).GE.3) THEN
              ISUMT = ISUMT + 1
              ITSTT = ITSTT + 1
            ENDIF
          ENDIF
        ENDIF

        IF(ITSTZ.EQ.0) ITSTZ = 1
        IF(ITSTT.EQ.0) ITSTT = 1
        IF(ITSTTD.EQ.0) ITSTTD = 1
        RZ  = FLOAT(ISUMZ)/FLOAT(ITSTZ)
        RT  = FLOAT(ISUMT)/FLOAT(ITSTT)
        RTD = FLOAT(ISUMTD)/FLOAT(ITSTTD)

        IF(ISUMZ.GE.1 .AND. RZ.GE..5) THEN
          ZG(L,IS) = .FALSE.
          IHSC(2,LM) = 99
          IF(TEST) WRITE(60,500) SID(IS),PMAND(L),ZG(L,IS),ISUMZ,ITSTZ
  500     FORMAT(' ISGOOD--Z    BAD FOR ',A8,' AT ',F10.1,' MB',
     &      '  ZG: ',L1,'  ISUMZ,ITSTZ: ',2I2)
        ENDIF
        IF(ISUMT.GE.1 .AND. RT.GE..5) THEN
          TG(L,IS) = .FALSE.
          IHSC(3,LM) = 99
          IF(TEST) WRITE(60,501) SID(IS),PMAND(L),TG(L,IS),ISUMT,ITSTT
  501     FORMAT(' ISGOOD--T    BAD FOR ',A8,' AT ',F10.1,' MB',
     &      '  TG: ',L1,'  ISUMT,ITSTT: ',2I2)
        ENDIF
        IF(ISUMTD+ISUMQ.GE.1 .AND. RTD.GT..5) THEN
          TDG(L,IS) = .FALSE.
          QG(L,IS)  = .FALSE.
          IHSC(4,LM) = 99
          IF(TEST) WRITE(60,502) SID(IS),PMAND(L),TDG(L,IS),QG(L,IS),
     &      ISUMTD,ITSTTD
  502     FORMAT(' ISGOOD--TD,Q BAD FOR ',A8,' AT ',F10.1,' MB',
     &      '  TDG,QG: ',2L1,'  ISUMTD,ITSTTD: ',2I2)
        ENDIF

        IF(.NOT. ZG(L,IS) .OR. .NOT.TG(L,IS)) ERROR(IS) = .TRUE.

        LOW = L
  100   CONTINUE
      ENDDO

C  TEST FOR BASELINE ERROR
C  -----------------------

      IF(NBAS.GT.10 .OR. ABS(PIS(IS)).GT.5.
     &   .OR. ABS(PSINC).GT.5.) ERROR(IS) = .TRUE.

C  TEST FOR POSSIBLE OBSERVATION ERRORS
C  ------------------------------------

      DO L=1,MAND
        LM = LMAND(L)
        IF(LM.GT.0 .AND. LM.LE.255) THEN
          IF(NZI(LM).GT.10 .AND. NZV(LM).GT.10) THEN
            ZG(L,IS) = .FALSE.
            IHSC(2,LM) = 99
            ERROR(IS) = .TRUE.
            IF(TEST) WRITE(60,500) SID(IS),PMAND(L),ZG(L,IS)
          ENDIF
          IF(NTI(LM).GT.6 .AND. NTV(LM).GT.6) THEN
            TG(L,IS) = .FALSE.
            IHSC(3,LM) = 99
            ERROR(IS) = .TRUE.
            IF(TEST) WRITE(60,501) SID(IS),PMAND(L),TG(L,IS)
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ISOLAT      GET LIST OF ISOLATED STATIONS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1994-03-17
C
C ABSTRACT: GET LIST OF ISOLATED STATIONS.
C
C PROGRAM HISTORY LOG:
C 1994-03-17  W. Collins  Original author.
C
C USAGE:    CALL ISOLAT(ID)
C   INPUT ARGUMENT LIST:
C     ID       - STATION IDENTIFIER
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE ISOLAT(ID)
      PARAMETER (NST=1500)

C
C     COLLECT LIST OF ISOLATED STATIONS.
C     THESE STATIONS HAD NO NEIGHBORS FOR HORIZONTAL CHECK.
C
      COMMON /ISO/ IDISO(NST),NUM,ISISO
      LOGICAL ISISO
      DATA ISTART /0/
      IF(ISTART.EQ.0) THEN
        DO I=1,NST
          IDISO(I) = 0
        END DO
        ISTART = 1
        NUM = 0
      ENDIF

C     CHECK TO SEE IF ID IS ON THE LIST.

      DO I=1,NUM
        IF(ID.EQ.IDISO(I)) GO TO 10
      END DO
        NUM = NUM + 1
        IDISO(NUM) = ID
   10 CONTINUE
      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ISORT       SORT, BASED ON ORDER IN INDX
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1994-03-17
C
C ABSTRACT: SORT INTEGERS IA ACCORDING TO THE ORDER SPECIFIED BY THE
C   INDICES IN INDX.
C
C PROGRAM HISTORY LOG:
C 1994-03-17  W. Collins  Original author.
C
C USAGE:    CALL SORT(IA,INDX,N)
C   INPUT ARGUMENT LIST:
C     IA       - VARIABLE
C     INDX     - ORDER FOR REARRANGEMENT OF RA
C     N        - DIMENSION OF RA
C
C   OUTPUT ARGUMENT LIST:
C     IA       - VARIABLE
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE ISORT(IA,INDX,N)
C
C     SORT IA ACCORDING TO THE ORDER SPECIFIED BY THE
C     INDICES IN INDX.

      PARAMETER (NST=1500)
      DIMENSION IA(*), IKSP(NST)
      INTEGER INDX(*)
      DO J=1,N
         IKSP(J) = IA(J)
      ENDDO
      DO J=1,N
         IA(J) = IKSP(INDX(J))
      ENDDO
      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    LAPSE       PERFORM LAPSE RATE CHECK
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Perform lapse rate check. The parameter NTL is assigned
C   to indicate the stability as follows:
C     1 - ABSOLUTELY STABLE
C     2 - STABLE TO UNSATURATED AIR BUT UNSTABLE TO SATURATED AIR
C     3 - ABSOLUTELY UNSTABLE
C     4 - ABSOLUTELY UNSTABLE WITH LOOSE LIMITS
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL LAPSE
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE LAPSE
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /CONSTS/  R, G, T0, CP, RV
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          PRNT

C  CALCULATE LAPSE RATES BETWEEN ALL PAIRS OF LEVELS ABOVE THE SFC
C  CALCULATE ONLY FOR LEVELS WITH NON-MISSING TEMPERATURES
C  ASSIGN NTL AS FOLLOWS:
C    1 - ABSOLUTELY STABLE
C    2 - STABLE TO UNSATURATED AIR BUT UNSTABLE TO SATURATED AIR
C    3 - ABSOLUTELY UNSTABLE
C    4 - ABSOLUTELY UNSTABLE WITH LOOSE LIMITS
C  ---------------------------------------------------------------

      GOR   = G/R
      DAL   = G/CP
      DO I=1,255
        TL(I)    = BMISS
        TLSAT(I) = BMISS
        NTL(I)   = -1
      ENDDO
      LMIN = MAX(1,LSFC)
      DO L=LMIN,NLEV-1
        IF(LEVTYP(L).EQ.3) GOTO 100
        DO LL=L+1,NLEV
          IF(LEVTYP(LL).NE.3) GOTO 50
        ENDDO
        GOTO 100
   50   CONTINUE
        TKL = TOB(L) + T0
        TKLL = TOB(LL) + T0
        IF(NINT(POB(LL)*10.).GE.NINT(POB(L)*10.)) GOTO 100
        IF(TKL.NE.0. .AND. POB(LL).NE.0. .AND. POB(L).NE.0.) THEN
          IF((TKLL/TKL).GT.0. .AND. (POB(LL)/POB(L)).GT.0.) THEN
            TL(L) = GOR * ALOG((TKLL)/(TKL)) / ALOG(POB(LL)/POB(L))
            TLL   = GOR * ALOG((TKLL+2.)/(TKL)) / ALOG(POB(LL)/POB(L))
          ENDIF
        ELSE
          GOTO 100
        ENDIF
        XL = (2.501 - .00237*TOB(L))*1.E6
        ES = 6.1078 * EXP((17.269*TOB(L))/(TOB(L)+237.3))
        PV = .622*XL / (R*TKL)
        TLSAT(L) = (G/CP)*((POB(L)+PV)/(POB(L)+(XL/(CP*TKL))*
     &    (R/RV)*PV))
        IF(TLL.GT.DAL) THEN
          NTL(L) = 4
        ELSEIF(TL(L).GT.DAL) THEN
          NTL(L) = 3
        ELSEIF(TL(L).GT.TLSAT(L) .AND. TL(L).LT.DAL) THEN
          NTL(L) = 2
        ELSE
          NTL(L) = 1
        ENDIF
  100   CONTINUE
      ENDDO

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    LEVTYPS     CHARACTERIZE LEVEL TYPE
C   PRGMMR: KEYSER         ORG: NP22       DATE: 2013-02-05
C
C ABSTRACT: Assign a type to each level, determined by what data are
C   non-missing:
C     1 - POB,ZOB,TOB,TDO  NON-MISSING (MAND OR SURVACE)
C     2 - POB,ZOB,TOB      NON-MISSING (MAND ONLY)
C     3 - POB,ZOB          NON-MISSING (MAND ONLY)
C     4 - POB,    TOB,TDO  NON-MISSING (MAND OR SIG)
C     5 - POB,    TOB      NON-MISSING (MAND OR SIG)
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2008-10-08  Woollen/Keyser -- corrected if tests where integer values
C     were tested for not being equal to real value for missing (BMISS
C     = 10E10), these integer values can never be equal to BMISS for
C     I*4 due to overflow - instead they are now tested for not being
C     equal to new integer value for missing (IMISS, also = 10E10),
C     although this is also an overflow value for I*4, it results in a
C     correct evaluation
C 2013-02-05  D. Keyser -- Final changes to run on WCOSS: Set BUFRLIB
C     missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C     overflow (also done for IMISS).
C
C USAGE:    CALL LEVTYPS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE LEVTYPS
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      CHARACTER*8      SID
      LOGICAL          PRNT

C  ASSIGN LEVEL TYPES:
C    1 - POB,ZOB,TOB,TDO  NON-MISSING (MAND OR SURVACE)
C    2 - POB,ZOB,TOB      NON-MISSING (MAND ONLY)
C    3 - POB,ZOB          NON-MISSING (MAND ONLY)
C    4 - POB,    TOB,TDO  NON-MISSING (MAND OR SIG)
C    5 - POB,    TOB      NON-MISSING (MAND OR SIG)
C    6 - (POB),(TDO)      SECOND SURFACE LEVEL (CAT=MSG)
C  -----------------------------------------------------

      DO I=1,255
        LEVTYP(I) = BMISS
      ENDDO
      DO L=1,NLEV
        IF(POB(L).EQ.BMISS .OR. CAT(L).EQ.BMISS) THEN
          LEVTYP(L) = 6
          GOTO 10
        ENDIF
        IF(ZOB(L).LT.BMISS .AND. TOB(L).LT.BMISS .AND.
     &    TDO(L).LT.BMISS) THEN
          LEVTYP(L) = 1
          GOTO 10
        ELSEIF(ZOB(L).LT.BMISS .AND. TOB(L).LT.BMISS) THEN
          LEVTYP(L) = 2
          GOTO 10
        ELSEIF(ZOB(L).LT.BMISS) THEN
          LEVTYP(L) = 3
          GOTO 10
        ELSEIF(TOB(L).LT.BMISS .AND. TDO(L).LT.BMISS) THEN
          LEVTYP(L) = 4
          GOTO 10
        ELSEIF(TOB(L).LT.BMISS) THEN
          LEVTYP(L) = 5
          GOTO 10
        ELSE
          LEVTYP(L) = 6
        ENDIF
   10   CONTINUE
      ENDDO

C  SET UP ARRAY OF INDICES OF THE MANDATORY LEVELS
C  ELEMENT 0 WILL CONTAIN THE NUMBER OF MANDATORY LEVELS
C  ALSO SOLVE FOR LEVEL OF SURFACE: LSFC
C  -----------------------------------------------------

      LSFC = 0
      DO I=0,21
        LMAND(I) = BMISS
      ENDDO
      MM = 0
      DO L=1,NLEV
        IF(CAT(L).EQ.0) THEN
          LSFC = L
          IF(MANLEV(POB(L)).NE.0) THEN
            MM = MANLEV(POB(L))
            LMAND(MM) = L
          ENDIF
        ELSEIF((CAT(L).EQ.1 .OR. CAT(L).EQ.5) .AND.
     &    MANLEV(POB(L)).NE.0) THEN
          MM = MANLEV(POB(L))
          LMAND(MM) = L
        ENDIF
      ENDDO
      LMAND(0) = MM
      IF(LMAND(0).LT.0) LMAND(0) = 0
                          ! LMAND(0) will never test = bmiss, use imiss
cdak  IF(LMAND(0).NE.BMISS .AND. LMAND(0).GT.21) LMAND(0) = 21
      IF(LMAND(0).NE.IMISS .AND. LMAND(0).GT.21) LMAND(0) = 21

      RETURN
      END

C******************************************************************
C$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    LMANLV      RETURN NEAREST MANDATORY LEVEL CNT BELOW
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Return the value of the count of the nearest mandatory
C   level below: 1-1000, 2-925, ...
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    X=LMANLV(P)
C   INPUT ARGUMENT LIST:
C     P        - PRESSURE
C
C   OUTPUT ARGUMENT LIST:
C     LMANLV   - RETURNS THE VALUE
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      FUNCTION LMANLV(P)
      SAVE ISET, LANLIN

      REAL(8) BMISS

      INTEGER LANLIN(10501)

      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      DATA ISET /0/

C  DETERMINE THE INDEX OF THE NEAREST MANDATORY LEVEL BELOW
C  --------------------------------------------------------

      IF(ISET.EQ.0) THEN
         DO I=1,10501
         LANLIN(I) = 0
         IF(I.LE.10000.AND. I.GE.9251) LANLIN(I) = 1
         IF(I.LE.9250 .AND. I.GE.8501) LANLIN(I) = 2
         IF(I.LE.8500 .AND. I.GE.7001) LANLIN(I) = 3
         IF(I.LE.7000 .AND. I.GE.5001) LANLIN(I) = 4
         IF(I.LE.5000 .AND. I.GE.4001) LANLIN(I) = 5
         IF(I.LE.4000 .AND. I.GE.3001) LANLIN(I) = 6
         IF(I.LE.3000 .AND. I.GE.2501) LANLIN(I) = 7
         IF(I.LE.2500 .AND. I.GE.2001) LANLIN(I) = 8
         IF(I.LE.2000 .AND. I.GE.1501) LANLIN(I) = 9
         IF(I.LE.1500 .AND. I.GE.1001) LANLIN(I) = 10
         IF(I.LE.1000 .AND. I.GE. 701) LANLIN(I) = 11
         IF(I.LE. 700 .AND. I.GE. 501) LANLIN(I) = 12
         IF(I.LE. 500 .AND. I.GE. 401) LANLIN(I) = 13
         IF(I.LE. 400 .AND. I.GE. 301) LANLIN(I) = 14
         IF(I.LE. 300 .AND. I.GE. 201) LANLIN(I) = 15
         IF(I.LE. 200 .AND. I.GE. 101) LANLIN(I) = 16
         IF(I.LE. 100 .AND. I.GE.  71) LANLIN(I) = 17
         IF(I.LE.  70 .AND. I.GE.  51) LANLIN(I) = 18
         IF(I.LE.  50 .AND. I.GE.  31) LANLIN(I) = 19
         IF(I.LE.  30 .AND. I.GE.  21) LANLIN(I) = 20
         IF(I.LE.  20                ) LANLIN(I) = 21
C        IF(                I.GE.9250) LANLIN(I) = 1
C        IF(I.LE.9249 .AND. I.GE.8500) LANLIN(I) = 2
C        IF(I.LE.8499 .AND. I.GE.7000) LANLIN(I) = 3
C        IF(I.LE.6999 .AND. I.GE.5000) LANLIN(I) = 4
C        IF(I.LE.4999 .AND. I.GE.4000) LANLIN(I) = 5
C        IF(I.LE.3999 .AND. I.GE.3000) LANLIN(I) = 6
C        IF(I.LE.2999 .AND. I.GE.2500) LANLIN(I) = 7
C        IF(I.LE.2499 .AND. I.GE.2000) LANLIN(I) = 8
C        IF(I.LE.1999 .AND. I.GE.1500) LANLIN(I) = 9
C        IF(I.LE.1499 .AND. I.GE.1000) LANLIN(I) = 10
C        IF(I.LE. 999 .AND. I.GE. 700) LANLIN(I) = 11
C        IF(I.LE. 699 .AND. I.GE. 500) LANLIN(I) = 12
C        IF(I.LE. 499 .AND. I.GE. 400) LANLIN(I) = 13
C        IF(I.LE. 399 .AND. I.GE. 300) LANLIN(I) = 14
C        IF(I.LE. 299 .AND. I.GE. 200) LANLIN(I) = 15
C        IF(I.LE. 199 .AND. I.GE. 100) LANLIN(I) = 16
C        IF(I.LE.  99 .AND. I.GE.  70) LANLIN(I) = 17
C        IF(I.LE.  69 .AND. I.GE.  50) LANLIN(I) = 18
C        IF(I.LE.  49 .AND. I.GE.  30) LANLIN(I) = 19
C        IF(I.LE.  29 .AND. I.GE.  20) LANLIN(I) = 20
C        IF(I.LE.  19                ) LANLIN(I) = 21
         ENDDO
         ISET = 1
      ENDIF

      IF(P.GE.BMISS) THEN
         LMANLV = 0
      ELSE
         IP = NINT(P*10.)

         IF(IP.GT.10500 .OR. IP.LT.5) THEN
            LMANLV = 0
         ELSE
            LMANLV = LANLIN(IP)
         ENDIF
      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MANLEV      RETURN LEVEL COUNT FOR MANDATORY LEVEL
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: For pressures equal to mandatory level values, return the
C   count of the mandatory level.  Otherwise, return 0.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    X=MANLEV(P)
C   INPUT ARGUMENT LIST:
C     P        - PRESSURE
C
C   OUTPUT ARGUMENT LIST:
C     MANLEV   - RETURNS THE VALUE
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      FUNCTION MANLEV(P)
      SAVE ISET, MANLIN

      REAL(8) BMISS

      INTEGER MANLIN(10001)

      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      DATA ISET /0/

      IF(ISET.EQ.0) THEN
         DO I=1,10001
         MANLIN(I) = 0
         IF(I.EQ.10000) MANLIN(I) = 1
         IF(I.EQ. 9250) MANLIN(I) = 2
         IF(I.EQ. 8500) MANLIN(I) = 3
         IF(I.EQ. 7000) MANLIN(I) = 4
         IF(I.EQ. 5000) MANLIN(I) = 5
         IF(I.EQ. 4000) MANLIN(I) = 6
         IF(I.EQ. 3000) MANLIN(I) = 7
         IF(I.EQ. 2500) MANLIN(I) = 8
         IF(I.EQ. 2000) MANLIN(I) = 9
         IF(I.EQ. 1500) MANLIN(I) = 10
         IF(I.EQ. 1000) MANLIN(I) = 11
         IF(I.EQ.  700) MANLIN(I) = 12
         IF(I.EQ.  500) MANLIN(I) = 13
         IF(I.EQ.  300) MANLIN(I) = 14
         IF(I.EQ.  200) MANLIN(I) = 15
         IF(I.EQ.  100) MANLIN(I) = 16
         IF(I.EQ.   70) MANLIN(I) = 17
         IF(I.EQ.   50) MANLIN(I) = 18
         IF(I.EQ.   30) MANLIN(I) = 19
         IF(I.EQ.   20) MANLIN(I) = 20
         IF(I.EQ.   10) MANLIN(I) = 21
         ENDDO
         ISET = 1
      ENDIF

      IF(P.GE.BMISS) THEN
         MANLEV = 0
      ELSE
         IP = NINT(P*10.)

         IF(IP.GT.10000 .OR. IP.LT.10 .OR. MOD(IP,10).NE.0) THEN
            MANLEV = 0
         ELSE
            MANLEV = MANLIN(NINT(P*10.))
         ENDIF
      ENDIF

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MASEVN      WRITE A MASS EVENT FOR MANDATORY LEVELS.
C   PRGMMR: J. Woollen       ORG: NP20       DATE: 1994-03-17
C
C ABSTRACT: WRITE AN EVENT FOR MANDATORY LEVELS.
C
C PROGRAM HISTORY LOG:
C 1994-03-17  J. Woollen  Original author.
C 1995-12-18  W. Collins  Changes to guarantee that preexisting
C     flags are honored.
C 1996-07-22  W. Collins  Modify for use by PREPOBS_CQCBUFR.
C
C USAGE:    CALL MASEVN
C
C OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE MASEVN
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISX(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /EVNSND/  PO(255), TO(255), ZO(255), CA(255),
     &                 PQ(255), TQ(255), ZQ(255), INP(255),
     &                 PR(255), TR(255), ZR(255), INZ(255),
     &                 QO(255), QQ(255), QR(255), INT(255),
     &                 TDE(255),TDQ(255),TDR(255),INQ(255),
     &                 UO(255), VO(255), SPO(255),DIRO(255),
     &                 WQ(255), WR(255)
      COMMON /EVNBUF/ STNB(2000),   SEQNB(2000), ISCANB(2000),
     &                LEVLB(2000),  PRESB(2000), LTYPB(2000),
     &                IVARB(2000),  CORB(2000),  CORVALB(2000),
     &                QMARKB(2000), IETYPB(2000),EDATEB(2000),
     &                LSTYPB(2000), PCORB(2000), IEVENTB
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      COMMON /STN/     IS
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST

      CHARACTER*8 COB(2),SID,CID,STNB
      CHARACTER*10     EDATEB
      CHARACTER*40 PEVN,TEVN,ZEVN,QEVN
      DIMENSION   IOB(2)
      EQUIVALENCE (COB(1),IOB(1))
      LOGICAL     SDM,LEVCHK, PRNT

      DATA PEVN   /'POB PQM PPC PRC                         '/
      DATA TEVN   /'TOB TQM TPC TRC                         '/
      DATA ZEVN   /'ZOB ZQM ZPC ZRC                         '/
      DATA QEVN   /'QOB QQM QPC QRC                         '/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CLEAR THE WORK ARRAYS FOR THIS OB
C  ---------------------------------

      PO = BMISS
      TO = BMISS
      ZO = BMISS
      QO = BMISS
      TDE = BMISS
      INP = BMISS
      INZ = BMISS
      INT = BMISS
      IEVP = 0
      IEVZ = 0
      IEVT = 0
      IEVQ = 0

C  LOOP OVER ALL EVENTS LOOKING FOR THIS PARTICULAR OB
C  ---------------------------------------------------

      DO 100 I=1,IEVENTB

        CID = STNB(I)
        IV = IVARB(I)
        IL = LEVLB(I)
        IF(SQN(IS).NE.SEQNB(I) .OR. IL.GT.NLEV) GOTO 100

C  FOUND A MATCH - DOUBLE CHECK TO MAKE SURE
C  -----------------------------------------

        LEVCHK = IL.LT.1 .OR. IL.GT.NLV
        IF(LEVCHK) GOTO 100
        IF(TEST) WRITE(60,502) SID(IS),IV,PRESB(I),CORVALB(I),
     &    QMARKB(I),IETYPB(I)
  502   FORMAT(' MASEVN--MATCH FOUND FOR: ',A8,'  IVAR,PRES,',
     &    'CORVAL,QMARK,IETYP: ',I5,2X,F8.1,2X,F8.1,2X,F4.0,2X,I5)

C       QM = QMARKB(I)
        IF(IV.EQ.1)   QM = PQM(IL)
        IF(IV.EQ.2)   QM = ZQM(IL)
        IF(IV.EQ.3)   QM = TQM(IL)
        IF(IV.EQ.4 .OR. IV.EQ.5)   QM = QQM(IL)
        IF(IV.EQ.1) DEC = PRC(IL)
        IF(IV.EQ.2) DEC = ZRC(IL)
        IF(IV.EQ.3) DEC = TRC(IL)
        IF(IV.EQ.4 .OR. IV.EQ.5) DEC = QRC(IL)
C       IF(QM.EQ.0.)  DEC = 0.
C       IF(QM.EQ.1.)  DEC = 1.
C       IF(QM.EQ.3.)  DEC = 3.
C       IF(QM.EQ.13.) DEC = 4.
        IF(CAT(IL).EQ.3 .OR. CAT(IL).EQ.4 .OR. CAT(IL).EQ.7) THEN
          IF(IV.EQ.1) DEC = PRC(IL)
          IF(IV.EQ.2) DEC = ZRC(IL)
          IF(IV.EQ.3) DEC = TRC(IL)
          IF(IV.EQ.4 .OR. IV.EQ.5) DEC = QRC(IL)
        ENDIF

C       IF(IV.EQ.1) THEN
C         POD = PRESB(I)
C         ISURF = 0
C         DO L=1,NLV
C           DIF = ABS(POB(L)-POD)
C           IF(DIF.LT..1) ISURF = L
C         ENDDO
C       ENDIF
        ISURF = LEVLB(I)

C  HONOR KEEPS OR PURGES
C  ---------------------

        SDM = QM.EQ.0.OR.QM.EQ.8.OR.QM.EQ.9.OR.QM.EQ.11.OR.
     &        QM.EQ.12.OR.QM.EQ.14.OR.QM.EQ.15

        IF(TEST) WRITE(60,501) CID,QM,DEC,SDM
  501   FORMAT(' MASEVN--station,quality mark,decision,SDM:  ',
     &         a8,1x,2f10.0,l8)

C  PUT TOGETHER A BUFR EVENT
C  -------------------------

        IF(.NOT.SDM) THEN
          IF(IV.EQ.2) THEN
            IEVZ = IEVZ + 1
            INZ(IEVZ) = LEVLB(I)
            ZO(IEVZ)  = CORVALB(I)
            ZQ(IEVZ)  = QMARKB(I)
            ZR(IEVZ)  = DEC
          ELSEIF(IV.EQ.3) THEN
            IEVT = IEVT + 1
            INT(IEVT) = LEVLB(I)
            TO(IEVT)  = CORVALB(I)
            TQ(IEVT)  = QMARKB(I)
            TR(IEVT)  = DEC
          ELSEIF(IV.EQ.4 .OR. IV.EQ.5) THEN
            IEVQ = IEVQ + 1
            INQ(IEVQ) = LEVLB(I)
            QO(IEVQ)  = CORVALB(I)*1.E6
            QQ(IEVQ)  = QMARKB(I)
            QR(IEVQ)  = DEC
          ELSEIF(IV.EQ.1) THEN
            IF(ISURF.NE.0) THEN
              IEVP = IEVP + 1
              INP(IEVP) = LEVLB(I)
              PO(IEVP)  = CORVALB(I)
              PQ(IEVP)  = QMARKB(I)
              PR(IEVP)  = DEC
            ENDIF
          ENDIF
        ENDIF
100   CONTINUE

      IF(TEST) WRITE(60,505) SID(IS),IEVP,IEVZ,IEVT,IEVQ,IEVENTB
  505 FORMAT(' MASEVN--CALLED FOR STATION: ',A8,
     &  ' IEVP,IEVZ,IEVT,IEVQ,IEVENTB: ',5I5)

      CALL EVENT(NFOUT,PEVN,NLV,PO,PQ,PR,INP,IEVP,CQCPC)
      WRITE(60,510) IEVP,SID(IS)
      CALL EVENT(NFOUT,ZEVN,NLV,ZO,ZQ,ZR,INZ,IEVZ,CQCPC)
      WRITE(60,511) IEVZ,SID(IS)
      CALL EVENT(NFOUT,TEVN,NLV,TO,TQ,TR,INT,IEVT,CQCPC)
      WRITE(60,512) IEVT,SID(IS)
      CALL EVENT(NFOUT,QEVN,NLV,QO,QQ,QR,INQ,IEVQ,CQCPC)
      WRITE(60,513) IEVQ,SID(IS)

      IF(IEVP.NE.0) THEN
        IF(TEST) WRITE(60,506) (INP(I),PO(I),PQ(I),PR(I),I=1,IEVP)
      ENDIF
      IF(IEVZ.NE.0) THEN
        IF(TEST) WRITE(60,507) (INZ(I),ZO(I),ZQ(I),ZR(I),I=1,IEVZ)
      ENDIF
      IF(IEVT.NE.0) THEN
        IF(TEST) WRITE(60,508) (INT(I),TO(I),TQ(I),TR(I),I=1,IEVT)
      ENDIF
      IF(IEVQ.NE.0) THEN
        IF(TEST) WRITE(60,509) (INQ(I),QO(I),QQ(I),QR(I),I=1,IEVQ)
      ENDIF
  506 FORMAT(' MASEVN--PRESSURE EVENTS:'/(10X,I4,3F12.1))
  507 FORMAT(' MASEVN--HEIGHT EVENTS:'/(10X,I4,3F12.1))
  508 FORMAT(' MASEVN--TEMPERATURE EVENTS:'/(10X,I4,3F12.1))
  509 FORMAT(' MASEVN--MOISTURE EVENTS:   '/(10X,I4,F12.1,2F12.1))
  510 FORMAT(' MASEVN--',I3,' PRESSURE EVENTS WRITTEN FOR: ',A8)
  511 FORMAT(' MASEVN--',I3,' HEIGHT EVENTS WRITTEN FOR: ',A8)
  512 FORMAT(' MASEVN--',I3,' TEMPERATURE EVENTS WRITTEN FOR: ',A8)
  513 FORMAT(' MASEVN--',I3,' MOISTURE EVENTS WRITTEN FOR: ',A8)

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MIMAP       FILL IMAP FOR HORIZONTAL SEARCH
C   PRGMMR: J. WOOLLEN       ORG: NP20       DATE: 1992-??-??
C
C ABSTRACT: FILL IMAP FOR HORIZONTAL SEARCH.
C
C PROGRAM HISTORY LOG:
C 1992-??-??  J. Woollen  Original author.
C
C USAGE:    CALL MIMAP(XQC,YQC,NREP)
C   INPUT ARGUMENT LIST:
C     XQC      - ARRAY OF LATITUDES TO SEARCH
C     YQC      - ARRAY OF LONGITUDES TO SEARCH
C     NREP     - SIZE OF LAT/LON ARRAYS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE MIMAP(XQC,YQC,NREP)

      PARAMETER (NST=1500)

      DIMENSION XQC(NREP),YQC(NREP)

      COMMON /OBLIST/ NOB,INOB(NST),IMAP(360,181)

      DIMENSION INDD(NST)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  MAKE SURE NONE OF THE LONGITUDES EQUAL 360.
C  -------------------------------------------

      DO N=1,NREP
        IF(XQC(N).EQ.360.) XQC(N) = 0.
      ENDDO

C  LONGITUDE SORT
C  --------------

      NN = 0
      DO NX=1,360
        DO N=1,NREP
C         IX = NINT(XQC(N)+1.)
          IX = MOD(NINT(XQC(N)+1.),360)+1
          IF(IX.EQ.NX) THEN
             NN = NN+1
             INDD(NN) = N
          ENDIF
        ENDDO
      ENDDO

C  LATITUDE SORT
C  -------------

      NOB = 0
      DO NY=1,181
        DO N=1,NN
          IY = NINT(YQC(INDD(N))+91.)
          IF(IY.LT.1) IY = 1
          IF(IY.GT.181) IY = 181
          IF(IY.EQ.NY) THEN
             NOB = NOB+1
             INOB(NOB) = INDD(N)
          ENDIF
        ENDDO
      ENDDO

C  INITIALIZE IMAP ARRAY
C  ---------------------

      IF(NN.NE.NREP .OR. NOB.NE.NREP) THEN
        PRINT *, 'MIMAP: NN.NE.NREP .OR. NOB.NE.NREP - STOP 99'
        CALL W3TAGE('PREPOBS_CQCBUFR')
        CALL ERREXIT(99)
      ENDIF

      DO J=1,181
        DO I=1,360
          IMAP(I,J) = 0
        ENDDO
      ENDDO

      DO N=1,NOB
        NX = XQC(INOB(N)) + 1.
        NY = YQC(INOB(N)) + 91.
        IF(IMAP(NX,NY).EQ.0) IMAP(NX,NY) = N
      ENDDO

C  FILL GAPS IN IMAP FOR CONTINUITY (BACKWARDS)
C  --------------------------------------------

      LASTN = NOB
      DO NY=181,1,-1
        DO NX=360,1,-1
          IF(IMAP(NX,NY).EQ.0) THEN
             IMAP(NX,NY) = LASTN
          ELSE
             LASTN = IMAP(NX,NY)
          ENDIF
        ENDDO
      ENDDO

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    NMANLV      RETURN NEAREST MANDATORY LEVEL COUNT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Return the value of the count of the nearest mandatory
C   level: 1-1000, 2-925, ...
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    X=NMANLV(P)
C   INPUT ARGUMENT LIST:
C     P        - PRESSURE
C
C   OUTPUT ARGUMENT LIST:
C     NMANLV   - RETURNS THE VALUE
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      FUNCTION NMANLV(P)
      SAVE ISET, NANLIN

      INTEGER NANLIN(10501)
      DATA ISET /0/

C  DETERMINE THE INDEX OF THE NEAREST MANDATORY LEVEL
C  --------------------------------------------------

      IF(ISET.EQ.0) THEN
         DO I=1,10501
         NANLIN(I) = 0
         IF(                I.GE.9620) NANLIN(I) = 1
         IF(I.LE.9619 .AND. I.GE.8880) NANLIN(I) = 2
         IF(I.LE.8879 .AND. I.GE.7750) NANLIN(I) = 3
         IF(I.LE.7749 .AND. I.GE.6000) NANLIN(I) = 4
         IF(I.LE.5999 .AND. I.GE.4500) NANLIN(I) = 5
         IF(I.LE.4499 .AND. I.GE.3500) NANLIN(I) = 6
         IF(I.LE.3499 .AND. I.GE.2750) NANLIN(I) = 7
         IF(I.LE.2749 .AND. I.GE.2250) NANLIN(I) = 8
         IF(I.LE.2249 .AND. I.GE.1750) NANLIN(I) = 9
         IF(I.LE.1749 .AND. I.GE.1250) NANLIN(I) = 10
         IF(I.LE.1249 .AND. I.GE. 850) NANLIN(I) = 11
         IF(I.LE. 849 .AND. I.GE. 600) NANLIN(I) = 12
         IF(I.LE. 599 .AND. I.GE. 400) NANLIN(I) = 13
         IF(I.LE. 399 .AND. I.GE. 250) NANLIN(I) = 14
         IF(I.LE. 249 .AND. I.GE. 150) NANLIN(I) = 15
         IF(I.LE. 149 .AND. I.GE.  85) NANLIN(I) = 16
         IF(I.LE.  84 .AND. I.GE.  60) NANLIN(I) = 17
         IF(I.LE.  59 .AND. I.GE.  40) NANLIN(I) = 18
         IF(I.LE.  39 .AND. I.GE.  25) NANLIN(I) = 19
         IF(I.LE.  24 .AND. I.GE.  15) NANLIN(I) = 20
         IF(I.LE.  14                ) NANLIN(I) = 21
         ENDDO
         ISET = 1
      ENDIF

      IP = NINT(P*10.)

      IF(IP.GT.10500) THEN
         NMANLV = 1
      ELSEIF(IP.LT.5) THEN
         NMANLV = 21
      ELSE
         NMANLV = NANLIN(IP)
      ENDIF

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    NOBERR      NEW OBSERVATION ERROR ROUTINE
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1999-04-27
C
C ABSTRACT: Find data with observation errors in height, temperature
C   and dew-point temperature (if TDQC = .true.).
C
C PROGRAM HISTORY LOG:
C 1999-04-27  W. Collins  Original author.
C
C USAGE:    CALL NOBERR
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE NOBERR
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /TMPSND/  ZOBT(21,NST,4), TOBT(21,NST,4),
     &                 ZIT(21,NST,4),  TIT(21,NST,4),
     &                 ZTMP(21,NST),   TTMP(21,NST),
     &                 NZTMP(21,NST),  NTTMP(21,NST),
     &                 ITERR(4)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /PEROR/  ISERR, ISOBERR
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      REAL        OBLIM(2,2)
      CHARACTER*8 SID
      LOGICAL     POUT, GOOD(6,2,2), TDQC
      LOGICAL     TEST, ISERR, ISOBERR, PRNT
      LOGICAL     ZG, TG, TDG, QG, ERROR
      LOGICAL     SDMZ, SDMT, SDMQ

      IF(TEST) WRITE(60,500) SID(IS)
  500 FORMAT(' NOBERR--CALLED FOR: ',A8)

      ISOBERR = .FALSE.
      TDQC = .TRUE.
      PZCOR = 0.

      DO L=1,NLVM
        SDMZ = .FALSE.
        SDMT = .FALSE.
        SDMQ = .FALSE.
        SDMZ = ZQM(L).EQ.0. .OR. ZQM(L).GE.14. .OR.
     &        (ZQM(L).GE.4. .AND. ZQM(L).LE.12.)
        SDMT = TQM(L).EQ.0. .OR. TQM(L).GE.14. .OR.
     &        (TQM(L).GE.4. .AND. TQM(L).LE.12.)
        SDMQ = QQM(L).EQ.0. .OR. QQM(L).GE.14. .OR.
     &        (QQM(L).GE.4. .AND. QQM(L).LE.12.)
        M = MANLEV(POB(L))
        IF(M.NE.0) THEN
          XZH(L)  = XZHM(M,IS)
          XTH(L)  = XTHM(M,IS)
          XTDH(L) = XTDHM(M,IS)
        ELSE
          XZH(L)  = BMISS
          XTH(L)  = BMISS
          XTDH(L) = BMISS
        ENDIF

        FACT = 0.01

C  HEIGHT
C  ------

        IF(.NOT.SDMZ) THEN
          B1 = 10.
          PER = .0035
          P3 = 0.5*FACT
          P13 = 0.1*FACT
          CALL FUZZY(PZ,XZI(L),XZV(L),XZH(L),B1,PER)
          QM = 0.
          IF(PZ.NE.BMISS .AND. PZ.LT.P3) THEN
            POUT = .TRUE.
            QM = 3.
            ISOBERR = .TRUE.
            ZC = 0.
            IHSC(2,L) = 49.
          ENDIF
          IF(PZ.NE.BMISS .AND. PZ.LT.P13) QM = 13.
          IF(QM.NE.0.) CALL SEVENT(SID(IS),SQN(IS),L,LST(L),POB(L),
     &         LEVTYP(L),2,ZC,ZOB(L)+ZC,ZC,QM,IHSC(2,L),XMISS,
     &         XMISS,XMISS)
        ENDIF


C  TEMPERATURE
C  -----------

C  ARTIFICIALLY MAKE IT HARDER TO FLAG BAD TEMPERATURES
C  NEAR THE SURFACE
C  ----------------------------------------------------

        IF(PS(IS)-POB(L).GT.200.) THEN
          CON = 1.0
        ELSEIF(PS(IS)-POB(L).LE.100.) THEN
          CON = 3.0
        ELSE
          CON = 3.0 - 2.*(PS(IS)-POB(L)-100.)/100.
        ENDIF
        IF(POB(L).GT.PS(IS)) CON = 1.0
        IF(CAT(L).EQ.5) CON = 3.0

        IF(.NOT.SDMT) THEN
          B1 = 10.
          PER = .0035
          P3 = 0.5*FACT
          P13 = 0.1*FACT
C         IF(L-ISF(IS).EQ.0) THEN
C           P3  = .002*FACT
C           P13 = .001*FACT
C         ENDIF
C         IF(L-ISF(IS).NE.0 .AND. L-ISF(IS).LT.4) THEN
C           P3  = 0.05*FACT
C           P13 = 0.02*FACT
C         ENDIF
C         IF(L-ISF(IS).EQ.4) THEN
C           P3  = 0.15*FACT
C           P13 = 0.07*FACT
C         ENDIF
          CALL FUZZY(PT,XTI(L)/CON,XTV(L)/CON,XTH(L)/CON,B1,PER)
          QM = 0.
          IF(PT.NE.BMISS .AND. PT.LT.P3) THEN
            POUT = .TRUE.
            QM = 3.
            ISOBERR = .TRUE.
            TC = 0.
            IHSC(3,L) = 49.
          ENDIF
          IF(PT.NE.BMISS .AND. PT.LT.P13) QM = 13.
          IF(QM.NE.0.) CALL SEVENT(SID(IS),SQN(IS),L,LST(L),POB(L),
     &         LEVTYP(L),3,TC,TOB(L)+TC,TC,QM,IHSC(3,L),XMISS,
     &         XMISS,XMISS)
        ENDIF

C  MOISTURE
C  --------

        IF(.NOT.SDMQ) THEN
          B1 = 20.
          PER = .1
          P3 = 0.5*FACT
          P13 = 0.1*FACT
          CALL FUZZY(PTD,XTDI(L),XTDV(L),XTDH(L),B1,PER)
          QM = 0.
          IF(PTD.NE.BMISS .AND. PTD.LT.P3) THEN
            POUT = .TRUE.
            QM = 3.
            ISOBERR = .TRUE.
            IHSC(4,L) = 49.
          ENDIF
          IF(PTD.NE.BMISS .AND. PTD.LT.P13) QM = 13.
          IF(QM.NE.0.) CALL SEVENT(SID(IS),SQN(IS),L,LST(L),POB(L),
     &         LEVTYP(L),5,0.,QOB(L),0.,QM,IHSC(4,L),XMISS,
     &         XMISS,XMISS)
        ENDIF
      ENDDO

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    OBERR       FIND DATA WITH OBSERVATION ERRORS
C   PRGMMR: KEYSER         ORG: NP22       DATE: 2013-02-05
C
C ABSTRACT: Find data with observation errors in height, temperature
C   and dew-point temperature (if TDQC = .true.).
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2008-10-08  Woollen/Keyser -- corrected if tests where integer values
C     were tested for not being equal to real value for missing (BMISS
C     = 10E10), these integer values can never be equal to BMISS for
C     I*4 due to overflow - instead they are now tested for not being
C     equal to new integer value for missing (IMISS, also = 10E10),
C     although this is also an overflow value for I*4, it results in a
C     correct evaluation
C 2013-02-05  D. Keyser -- Final changes to run on WCOSS: Set BUFRLIB
C     missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C     overflow (also done for IMISS).
C
C USAGE:    CALL OBERR
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE OBERR
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /TMPSND/  ZOBT(21,NST,4), TOBT(21,NST,4),
     &                 ZIT(21,NST,4),  TIT(21,NST,4),
     &                 ZTMP(21,NST),   TTMP(21,NST),
     &                 NZTMP(21,NST),  NTTMP(21,NST),
     &                 ITERR(4)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /PEROR/  ISERR, ISOBERR
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST, ISERR, ISOBERR, PRNT
      REAL        OBLIM(2,2)
      CHARACTER*8 SID
      LOGICAL     POUT, GOOD(6,2,2), TDQC
      LOGICAL ZG, TG, TDG, QG, ERROR, SDMQ
C     DATA OBLIM /.55,.40,.40,.30/
C     DATA OBLIM /.66,.48,.48,.36/
C     DATA OBLIM /.55,.37,.40,.27/
      DATA OBLIM /.605,.403,.440,.293/

      IF(TEST) WRITE(60,500) SID(IS)
  500 FORMAT(' OBERR--CALLED FOR: ',A8)

      POUT = .FALSE.
      TDQC = .FALSE.
      ISOBERR = .FALSE.

      LAST = NLEV
      DO LM=1,NLEV

      L = MANLEV(POB(LM))
      LN = NMANLV(POB(LM))
      IF(CAT(LM).EQ.1 .AND. L.NE.0) THEN
        CALL BEST(TCBEST(1),XMISS,XMISS,TI(LM),XINC(L,2),
     &    XMISS,XMISS,THM(L,IS),HOIRES(L,2),
     &    TTMP(L,IS),TMPSTD(L,2))
      ELSE
        CALL BEST(TCBEST(1),XMISS,XMISS,TI(LM),XINC(LN,2),
     &    XMISS,XMISS,XMISS,XMISS,XMISS,XMISS)
      ENDIF
      IF(LM.GE.2) THEN
        LB = NMANLV(POB(LM-1))
        IF(LB.LT.1) LB = 1
        IF(CAT(LM-1).EQ.1 .AND. LB.NE.0) THEN
          CALL BEST(TCBB,XMISS,XMISS,TI(LM-1),XINC(LB,2),
     &      XMISS,XMISS,THM(LB,IS),HOIRES(LB,2),
     &      TTMP(LB,IS),TMPSTD(LB,2))
        ELSE
          CALL BEST(TCBB,XMISS,XMISS,TI(LM-1),XINC(LB,2),
     &      XMISS,XMISS,XMISS,XMISS,XMISS,XMISS)
        ENDIF
      ELSE
        LB = L
        IF(LB.EQ.0) LB = LN
        TCBB = BMISS
      ENDIF
      IF(LM.LT.NLVM-1) THEN
        LA = NMANLV(POB(LM+1))
        IF(LA.GT.NLVM) LA = NLVM
        IF(CAT(LM+1).EQ.1 .AND. LA.NE.0) THEN
          CALL BEST(TCBA,XMISS,XMISS,TI(LM+1),XINC(LA,2),
     &      XMISS,XMISS,THM(LA,IS),HOIRES(LA,2),
     &      TTMP(LA,IS),TMPSTD(LA,2))
        ELSE
          CALL BEST(TCBA,XMISS,XMISS,TI(LM+1),XINC(LA,2),
     &      XMISS,XMISS,XMISS,XMISS,XMISS,XMISS)
        ENDIF
      ELSE
        LA = L
        IF(LA.EQ.0) LA = LN
        TCBA = BMISS
      ENDIF

C  TEST FOR TEMPERATURE OBSERVATION ERROR
C  --------------------------------------

      QM = 0.
      IF(PS(IS)-POB(LM).GT.200.) THEN
        CON = 1.0
      ELSEIF(PS(IS)-POB(LM).LE.100.) THEN
        CON = 3.0
      ELSE
        CON = 3.0 - 2.*(PS(IS)-POB(LM)-100.)/100.
      ENDIF
      IF(POB(LM).GT.PS(IS)) CON = 1.0
      IF(CAT(LM).EQ.5) CON = 3.0

      NTVP = 0
                         ! NTV(LM+1) will never test = bmiss, use imiss
cdak  IF(LM+1.LE.NLVM .AND. NTV(LM+1).NE.BMISS) NTVP = NTV(LM+1)
      IF(LM+1.LE.NLVM .AND. NTV(LM+1).NE.IMISS) NTVP = NTV(LM+1)
      NTVM = 0
                         ! NTV(LM-1) will never test = bmiss, use imiss
cdak  IF(LM-1.GE.1 .AND. NTV(LM-1).NE.BMISS) NTVM = NTV(LM-1)
      IF(LM-1.GE.1 .AND. NTV(LM-1).NE.IMISS) NTVM = NTV(LM-1)

      CC1 = CON*OBLIM(1,2)*XINC(LN,2)
      CC2 = CON*OBLIM(2,2)*XINC(LN,2)
      IF(TEST) WRITE(60,501) POB(LM),TCBEST(1),TCBB,TCBA,CC1,CC2
  501 FORMAT(' OBERR--POB,TCBEST(1),TCBB,TCBA,CC1,CC2: ',6F10.1)

      IF(TCBEST(1).NE.BMISS .AND.
     &  ABS(TCBEST(1)).GT.CON*OBLIM(1,2)*XINC(LN,2)) THEN
        POUT = .TRUE.
        TC = 0.
        IHSC(3,LM) = 30.
        IF(ABS(TCBEST(1)).GT.2.0*CON*OBLIM(1,2)*XINC(LN,2)) THEN
          QM = 13.
          ISOBERR = .TRUE.
        ELSEIF(NTV(LM).GT.7 .AND. NTVP.LT.5 .AND. NTVM.LT.5) THEN
          QM = 13.
          ISOBERR = .TRUE.
        ELSE
          QM = 3.
          ISOBERR = .TRUE.
        ENDIF
      ELSEIF(TCBEST(1).NE.BMISS .AND.
     &  ABS(TCBEST(1)).GT.CON*OBLIM(2,2)*XINC(LN,2) .AND.
     &  (ABS(TCBB).GT.CON*OBLIM(2,2)*XINC(LB,2) .OR.
     &   ABS(TCBA).GT.CON*OBLIM(2,2)*XINC(LA,2))) THEN
        POUT = .TRUE.
        TC = 0.
        IHSC(3,LM) = 31.
        IF(NTV(LM).GT.7 .AND. NTVP.LT.5 .AND. NTVM.LT.5) THEN
          QM = 13.
          ISOBERR = .TRUE.
        ELSE
          QM = 3.
          ISOBERR = .TRUE.
        ENDIF
      ELSEIF(TCBEST(1).NE.BMISS .AND.
     &  ABS(TCBEST(1)).GT.CON*OBLIM(2,2)*XINC(LN,2)) THEN
        IF(LM.GT.1) THEN
          IF(NTL(LM).EQ.4 .OR. NTL(LM-1).EQ.4) THEN
            POUT = .TRUE.
            TC = 0.
            IHSC(3,LM) = 32.
            IF(NTV(LM).GT.7 .AND. NTVP.LT.5 .AND. NTVM.LT.5) THEN
              QM = 13.
              ISOBERR = .TRUE.
            ELSE
              QM = 3.
              ISOBERR = .TRUE.
            ENDIF
          ELSE
            POUT = .TRUE.
            QM = 3.
            ISOBERR = .TRUE.
            TC = 0.
            IHSC(3,LM) = 33.
          ENDIF
        ELSEIF(LM.EQ.1) THEN
          IF(NTL(LM).EQ.4) THEN
            POUT = .TRUE.
            TC = 0.
            IHSC(3,LM) = 34.
            QM = 13.
            ISOBERR = .TRUE.
          ELSE
            POUT = .TRUE.
            QM = 3.
            ISOBERR = .TRUE.
            TC = 0.
            IHSC(3,LM) = 35.
          ENDIF
        ENDIF
      ENDIF
      IF(QM.EQ.3. .OR. QM.EQ.13.) THEN
        PTCOR = 0.
        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),3,TC,
     &    TOB(LM)+TC,PTCOR,QM,IHSC(3,LM),XMISS,XMISS,XMISS)
      ENDIF

C  TEST FOR HEIGHT OBSERVATION ERROR
C  ---------------------------------

      IF(CAT(LM).EQ.2 .OR. CAT(LM).EQ.5) GOTO 100
      QM = 0.
      LN = NMANLV(POB(LM))
                                ! LN will never test = bmiss, use imiss
cdak  IF(LN.GT.0 .AND. LN.NE.BMISS) THEN
      IF(LN.GT.0 .AND. LN.NE.IMISS) THEN
        IF(NZI(LM).LT.6) THEN
          ZDIF = ZI(LM)
        ELSE
          ZDIF = ZD(LM)
        ENDIF
C       CALL BEST(ZCBEST(1),XMISS,XMISS,ZDIF,XINC(LN,1),
C    &    XMISS,XMISS,ZHM(LN,IS),HOIRES(LN,1),
C    &    ZTMP(LN,IS),TMPSTD(LN,1))
        CALL BEST(ZCBEST(1),XMISS,XMISS,ZI(LM),XINC(LN,1),
     &    XMISS,XMISS,ZHM(LN,IS),HOIRES(LN,1),
     &    ZTMP(LN,IS),TMPSTD(LN,1))
      ELSE
        ZCBEST(1) = BMISS
      ENDIF

      CC1 = OBLIM(1,1)*XINC(LN,1)
      CC2 = OBLIM(2,1)*XINC(LN,1)
      IF(TEST) WRITE(60,502) POB(LM),ZCBEST(1),CC1,CC2
  502 FORMAT(' OBERR--POB,ZCBEST(1),CC1,CC2: ',4F10.1)

      IF(ZCBEST(1).NE.BMISS .AND.
     &  ABS(ZCBEST(1)).GT.OBLIM(1,1)*XINC(LN,1)) THEN
        POUT = .TRUE.
        QM = 13.
        ISOBERR = .TRUE.
        ZC = 0.
        IHSC(2,LM) = 36.
      ELSEIF(ZCBEST(1).NE.BMISS .AND.
     &  ABS(ZCBEST(1)).GT.OBLIM(2,1)*XINC(LN,1)) THEN
        POUT = .TRUE.
        QM = 3.
        ISOBERR = .TRUE.
        ZC = 0.
        IHSC(2,LM) = 37.
      ENDIF
      IF(QM.EQ.3. .OR. QM.EQ.13.) THEN
        PZCOR = 0.
        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),2,ZC,
     &    ZOB(LM)+ZC,PZCOR,QM,IHSC(2,LM),XMISS,XMISS,XMISS)
      ENDIF
  100 CONTINUE

C  TEST FOR SPECIFIC HUMIDITY OBSERVATION ERROR
C  --------------------------------------------

      IF(TDQC) THEN
        QM = 0.
        IF(NTDI(LM).GE.20 .OR. NTDV(LM).GE.20) THEN
          POUT = .TRUE.
          QM = 13.
          ISOBERR = .TRUE.
          IHSC(4,LM) = 38.
          IF(TEST) WRITE(60,601) POUT,QM,ISOBERR,IHSC(4,LM)
  601     FORMAT(' POBERR--POUT,QM,ISOBERR,IHSC(4,LM): ',L2,F6.0,L2,I5)
          SDMQ = .FALSE.
          SDMQ = QQM(LM).EQ.0. .OR. QQM(LM).GE.14. .OR.
     &        (QQM(LM).GE.4. .AND. QQM(LM).LE.12.)
          IF(.NOT.SDMQ) THEN
            CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),5,
     &        0.,QOB(LM),0.,QM,IHSC(4,LM),XMISS,XMISS,XMISS)
          ENDIF
        ENDIF
        IF(L.NE.0) THEN
          IF(NTDHM(L,IS).GE.20) THEN
            POUT = .TRUE.
            QM = 13.
            ISOBERR = .TRUE.
            IHSC(4,LM) = 39.
            PTDCOR = 0.
            IF(TEST) WRITE(60,601) POUT,QM,ISOBERR,IHSC(4,LM)
            SDMQ = .FALSE.
            SDMQ = QQM(LM).EQ.0. .OR. QQM(LM).GE.14. .OR.
     &        (QQM(LM).GE.4. .AND. QQM(LM).LE.12.)
            IF(.NOT.SDMQ) THEN
              CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &          5,0.,QOB(LM),0.,QM,IHSC(4,LM),XMISS,XMISS,XMISS)
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      ENDDO

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    OUTPUT      COMPLETE WRITING TO OUTPUT PREPBUFR FILE
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Complete writing to output PREPBUFR file.  At the end of
C   all stations, also close the output file.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL OUTPUT(ENDIN,SINGLE)
C   INPUT ARGUMENT LIST:
C     ENDIN    - TRUE AT END OF INPUT
C     SINGLE   - TRUE FOR SINGLE-STATION VERSION
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE OUTPUT(ENDIN,SINGLE)
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      COMMON /TESTS/   TEST
      LOGICAL          TEST
      LOGICAL ENDIN,SINGLE, PRNT, HGTTBL

      IF(.NOT.SINGLE .AND. .NOT.ENDIN) THEN
        CALL WRITSB(NFOUT)
        IF(TEST) WRITE(60,500)
  500   FORMAT(' OUTPUT--WRITSB CALLED')
      ENDIF

      IF(ENDIN) CALL CLOSBF(NFOUT)

      RETURN
      END

C****************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PATZ        FIND PRESSURE AT GIVEN HEIGHT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1995-12-18
C
C ABSTRACT: FIND THE PRESSURE AT A GIVEN HEIGHT.  THERE ARE THREE
C   POSSIBLE METHODS USED, AS DESCRIBED BELOW.
C
C PROGRAM HISTORY LOG:
C 1995-12-18  W. Collins  Original author.
C
C USAGE:    CALL PATZ(P1,P2,P3,Z1,Z2,Z3,T1C,T3C,ALPHA,METHOD)
C   INPUT ARGUMENT LIST:
C     P1       - PRESSURE FOR METHODS 1 AND 2.          (HPA)
c     P3       - PRESSURE FOR METHODS 1 AND 2.          (HPA)
C     Z1       - HEIGHT FOR METHODS 1 AND 2.            (M)
C     Z2       - HEIGHT AT WHICH PRESSURE IS NEEDED     (M)
C     Z3       - OPTIONAL SECOND HEIGHT FOR METHODS
C                1 AND 2.                               (M)
C     T1C      - TEMPERATURE FOR METHODS 1 AND 2.       (DEG-C)
C     T3C      - TEMPERATURE FOR METHODS 1 AND 2.       (DEG-C)
C     ALPHA    - LAPSE RATE, EITHER SPECIFIED OR
C                CALCULATED IF MISSING.
C                FOR METHOD 1:  (DELTA-T/DELTA(LN(P))
C                FOR METHOD 2:  (DELTA-T/DELTA-Z)
C     METHOD   - DETERMINES WHICH SOLUTION METHOD TO USE.
C
C   OUTPUT ARGUMENT LIST:
C     P2       - PRESSURE AT GIVEN HEIGHT (Z2)          (HPA)
C
C OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE PATZ(P1,P2,P3,Z1,Z2,Z3,T1C,T3C,ALPHA,METHOD)

      REAL(8) BMISS

      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

C     Find the pressure (P2) at a given height, Z2, using available
C     information at levels 1 and 3.  Z2 must always be non-missing.
C     P1, T1C, Z1 must always be non-missing in addition for methods
C     1 and 2.  Several cases are possible.  The methods are:
C       METHOD = 1
C            Assume T linear in ln(pressure).
C            Calculate ALPHA (lapse rate wrt ln(p)) if (P3,T3C) are
C            non-missing, otherwise ALPHA must be specified on input.
C       METHOD = 2
C            Assume T linear in height.
C            Calculate ALPHA (lapse rate wrt z) if (P3,Z3) are
C            non-missing, otherwise ALPHA must be specified on input.
C       METHOD = 3
C            Use U.S. Standard Atmosphere pressure at Z2.

      DATA G /9.80665/, R /287.05/, T0 /273.15/
      DATA C0 /234.51/, C1 /10769.23/, C4 /.190284/, C5 /-33538.5/

C     WRITE(60,500) p1,p2,p3,z1,z2,z3,t1c,t3c,alpha,method
  500 format(' PATZ--(begin)p1,p2,p3,z1,z2,z3,t1c,t3c,alpha,method: ',
     &  8f8.1,f8.5,1x,i2)


      IF(Z2.GT.0.5*BMISS) RETURN
      IF(P1.NE.BMISS .AND. Z1.NE.BMISS .AND. Z2.EQ.Z1) THEN
        P2 = P1
        RETURN
      ENDIF

      P2 = BMISS
      GOR = G/R
      T1 = T1C + T0
      T3 = T3C + T0

      IF(METHOD.NE.3 .AND. (P1.EQ.BMISS.OR.T1C.EQ.BMISS.OR.Z1.EQ.BMISS))
     &  METHOD = 3
      IF(METHOD.EQ.2 .AND. (Z1.EQ.BMISS.OR.Z2.EQ.BMISS.OR.Z3.EQ.BMISS))
     &  METHOD = 3

      IF(METHOD.EQ.1) THEN
        IF(P3.NE.BMISS .AND. T3C.NE.BMISS) THEN
          P1L = LOG(P1)
          P3L = LOG(P3)
          IF(P3L.NE.P1L) THEN
            ALPHA = (T3-T1)/(P3L-P1L)
          ELSE
            ALPHA = 0.
          ENDIF
        ELSE
C         ALPHA is specified on input.
          IF(ALPHA.EQ.BMISS) RETURN
        ENDIF
        IF(ALPHA.GT.0. .AND.
     &    ((T1/ALPHA)**2 - (2*GOR/ALPHA) * (Z2-Z1)).GT.0.) THEN
          P2L = P1L - (T1/ALPHA) + SQRT((T1/ALPHA)**2
     &          - (2*GOR/ALPHA) * (Z2-Z1))
        ELSEIF(ALPHA.LT.0. .AND.
     &    ((T1/ALPHA)**2 - (2*GOR/ALPHA) * (Z2-Z1)).GT.0.) THEN
          P2L = P1L - (T1/ALPHA) -SQRT((T1/ALPHA)**2
     &          - (2*GOR/ALPHA) * (Z2-Z1))
        ELSE
          P2L = P1L - (GOR/T1) * (Z2-Z1)
        ENDIF
        P2 = EXP(P2L)

      ELSEIF(METHOD.EQ.2) THEN
        IF(P3.NE.BMISS .AND. Z3.NE.BMISS) THEN
          IF(Z3.NE.Z1) THEN
            ALPHA = (T3-T1)/(Z3-Z1)
          ELSE
            ALPHA = 0.
          ENDIF
        ELSE
C         ALPHA is specified on input.
          IF(ALPHA.EQ.BMISS) RETURN
        ENDIF
        P2 = P1 * ((T1+ALPHA*Z2) / (T1+ALPHA*Z1))**(-GOR)

      ELSEIF(METHOD.EQ.3 .AND. Z2.NE.BMISS) THEN
        IF(Z2.LT.C1) THEN
          P2 = C0 * EXP(LOG((Z2-C1)/C5 + 1.)/C4)
        ELSE
          P2 = C0 * EXP((Z2-C1)/(C4*C5))
        ENDIF
      ENDIF

C     WRITE(60,501) p1,p2,p3,z1,z2,z3,t1c,t3c,alpha,method
  501 format(' PATZ--(end)p1,p2,p3,z1,z2,z3,t1c,t3c,alpha,method: ',
     &  8f8.1,f8.5,1x,i2)

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PEVENTS     PRINT EVENTS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Print events.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL PEVENTS
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     NFEVN    - UNIT NUMBER OF FILE CONTAINING PRINTOUT OF ALL EVENTS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE PEVENTS

      COMMON /EVENTS/  STN(2000),    SEQN(2000),  ISCAN(2000),
     &                 LEVL(2000),   PRES(2000),  LTYP(2000),
     &                 IVAR(2000),   COR(2000),   CORVAL(2000),
     &                 LSTYP(2000),  PCOR(2000),  IEVENT,
     &                 BASR(2000),   PISR(2000),  PSINCR(2000),
     &                 QMARK(2000),  IETYP(2000), EDATE(2000)
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL

      CHARACTER*2     CVAR(5)
      CHARACTER*8     STN
      CHARACTER*10    EDATE
      DATA CVAR/' P',' Z',' T','TD',' Q'/

      WRITE(6,500)
      DO I=1,IEVENT
        IF(QMARK(I).LT.3) THEN
          OVAL = CORVAL(I) - COR(I)
        ELSE
          OVAL = CORVAL(I)
        ENDIF
        IF(MOD(I-1,10).EQ.0) WRITE(6,501)
        WRITE(6,502) STN(I),EDATE(I),SEQN(I),ISCAN(I),LEVL(I),
     &    PRES(I),LTYP(I),LSTYP(I),CVAR(IVAR(I)),OVAL,
     &    COR(I),CORVAL(I),PCOR(I),QMARK(I),IETYP(I),BASR(I),
     &    PISR(I),PSINCR(I)
        WRITE(NFEVN,502) STN(I),EDATE(I),SEQN(I),ISCAN(I),LEVL(I),
     &    PRES(I),LTYP(I),LSTYP(I),CVAR(IVAR(I)),OVAL,
     &    COR(I),CORVAL(I),PCOR(I),QMARK(I),IETYP(I),BASR(I),
     &    PISR(I),PSINCR(I)
      ENDDO

  500 FORMAT(///' EVENTS--')
  501 FORMAT('   STN          DATE  SQN  SCAN LEVEL  PRESSURE LEVTYP',
     &       ' SFCTYP VAR  ORIG-VAL       COR   NEW-VAL  ORIG-COR',
     &       ' QMARK IETYP BAS-RES  PS-INC  PS-RES')
  502 FORMAT(1X,A8,1X,A10,F6.0,I5,I6,F10.1,2I7,2X,A2,4F10.1,F6.0,I6,
     &  3(1X,F7.1))

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    POBERR      FIND OBSERVATION ERRORS AT TOP OF PROFILE
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1998-08-21
C
C ABSTRACT: Find data with observation errors in height, temperature
C   at some level and persisting vertically.  In such cases, it
C   assumed that the temperature sensor has gone bad and all levels of
C   temperature and height above should be rejected.
C
C PROGRAM HISTORY LOG:
C 1998-08-21  W. Collins  Original author.
C
C USAGE:    CALL POBERR
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE POBERR
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /REGION/  AVGIY(21,2,28), STDIY(21,2,28)
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /PEROR/  ISERR, ISOBERR
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /ISO/     IDISO(NST),NUM,ISISO
      COMMON /STN/     IS
      COMMON /NO40S/   NORD23, WRT23
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          WRT23
      COMMON /TESTS/   TEST
      REAL             TLO(21), THI(21), ZLO(21), ZHI(21)
      REAL             TLO0(21), THI0(21), ZLO0(21), ZHI0(21)
      LOGICAL          TEST, POUT, ISOBERR, TBAD(255), ZBAD(255)
      LOGICAL          ISISO, NORD23
      CHARACTER*8      SID
      DATA TLO0/-6.7,-5.6,-4.8,-3.3,-2.4,-2.6,-2.8,-3.4,
     &  -3.4,-3.2,-3.4,-3.6,-3.8,-3.6,-4.4,6*-5.8/
      DATA THI0/0.9,1.6,2.0,2.3,2.4,2.6,2.8,3.4,
     &  3.4,3.2,3.4,3.6,3.8,3.6,4.4,6*5.8/
      DATA ZLO0/-29.,-24.,-21.,-23.,-28.,-31.,-37.,-42.,
     &  -51.,-63.,-76.,-77.,-72.,-74.,-86.,-121.,5*-150./
      DATA ZHI0/33.,25.,23.,28.,39.,47.,56.,63.,
     &  74.,90.,118.,126.,133.,162.,178.,223.,5*250./
      DATA TLIMIT /0.65/, ZLIMIT /0.75/, STDNUM /2.5/

!
!  SKIP THIS SUBROUTINE IF UNIT23 IS EMPTY
!
      IF(NORD23) RETURN
!
!  GET WMO BLOCK FOR DECISIONS TO BE MADE LATER
!
      IBL = 0
      READ(SID(IS),'(I5)',ERR=100) IDENT
      IBL = IDENT/1000
!
!  DO NOT DIAGNOSE TYPE 40'S FOR ISOLATED STATIONS
!
      DO I=1,NUM
        IF(IDENT.EQ.IDISO(I)) RETURN
      ENDDO
  100 CONTINUE

      POUT = .FALSE.
      ISOBERR = .FALSE.

      DO L=1,255
        TBAD(L) = .FALSE.
        ZBAD(L) = .FALSE.
      ENDDO

      CALL AREA(IA,XOB(IS),YOB(IS))

      IF(IA.NE.0) THEN
        DO L=1,21
          ZLO(L) = AVGIY(L,1,IA) - STDNUM * STDIY(L,1,IA)
          TLO(L) = AVGIY(L,2,IA) - STDNUM * STDIY(L,2,IA)
          ZHI(L) = AVGIY(L,1,IA) + STDNUM * STDIY(L,1,IA)
          THI(L) = AVGIY(L,2,IA) + STDNUM * STDIY(L,2,IA)
        ENDDO
      ELSE
        DO L=1,21
          ZLO(L) = ZLO0(L)
          TLO(L) = TLO0(L)
          ZHI(L) = ZHI0(L)
          THI(L) = THI0(L)
          WRITE(60,515) SID(IS),XOB(IS),YOB(IS)
  515     FORMAT(' POBERR--DEFAULT STATISTICS USED FOR ',A8,' WITH',
     &      ' (LAT,LON) = ',2F8.2)
        ENDDO
      ENDIF

      DO LM=1,NLEV
        LN = NMANLV(POB(LM))    ! INDEX OF NEAREST MAND LVL
        IF(TI(LM).NE.BMISS .AND.
     &    (TI(LM).LT.TLO(LN) .OR. TI(LM).GT.THI(LN))) TBAD(LM) = .TRUE.
        IF(ZI(LM).NE.BMISS .AND.
     &    (ZI(LM).LT.ZLO(LN) .OR. ZI(LM).GT.ZHI(LN))) ZBAD(LM) = .TRUE.
      ENDDO
      IF(TEST) WRITE(60,500) SID(IS),(TBAD(L),L=1,NLEV)
      IF(TEST) WRITE(60,501)         (ZBAD(L),L=1,NLEV)
  500 FORMAT(' POBERR--STN: ',A8,' TBAD: ',(25(L1,1X),/))
  501 FORMAT('              ',8X,' ZBAD: ',(25(L1,1X),/))
!
!  FIND LOWEST LEVEL OF TEMPERATURE WITH TLIMIT AMOUNT OF
!  TEMPERATURE INCREMENTS ABOVE THAT EXCEED LIMITS TLO OR THI.
!  HOWEVER, ONLY CHECK INCREMENTS AT OR ABOVE GROUND
!
      TRAT   = 0.
      LOWT   = 0
      LEVCNT = 0
      LEVBAD = 0
      DO LM=1,NLEV
        IF(TBAD(LM) .AND. POB(LM).LE.PS(IS)) THEN
          LEVCNT = 1
          LEVBAD = 1
          DO L=LM+1,NLEV
            IF(TI(L).NE.BMISS) LEVCNT = LEVCNT + 1
            IF(TBAD(L))        LEVBAD = LEVBAD + 1
          ENDDO
          TRAT = REAL(LEVBAD)/REAL(LEVCNT)
          IF(TRAT.GE.TLIMIT) THEN
            LOWT = LM
            GOTO 10
          ENDIF
        ENDIF
      ENDDO
   10 CONTINUE
      IF(TEST) WRITE(60,503) LEVCNT,LEVBAD,TRAT,LOWT
  503 FORMAT(' POBERR--LEVCNT,LEVBAD,TRAT,LOWT: ',2I5,F8.3,I5)
!
!  FIND LOWEST LEVEL OF HEIGHT WITH ZLIMIT AMOUNT OF
!  HEIGHT INCREMENTS ABOVE THAT EXCEED LIMITS ZLO OR ZHI.
!  HOWEVER, ONLY CHECK INCREMENTS AT OR ABOVE GROUND
!  AND DO NOT ALLOW LOWZ TO BE AT UPPERMOST HEIGHT LEVEL.
!
      LOWZ = 0
      ZRAT = 0
      DO LM=1,NLEV
        IF(ZBAD(LM) .AND. POB(LM).LE.PS(IS)) THEN
          LEVCNT = 1
          LEVBAD = 1
          DO L=LM+1,NLEV
            IF(ZI(L).NE.BMISS) LEVCNT = LEVCNT + 1
            IF(ZBAD(L))        LEVBAD = LEVBAD + 1
          ENDDO
          ZRAT = REAL(LEVBAD)/REAL(LEVCNT)
          IF(ZRAT.GE.ZLIMIT) THEN
            LOWZ = LM
            DO L=LOWZ+1,NLEV
              IF(ZI(L).NE.BMISS) GOTO 20
            ENDDO
            LOWZ = 0
            GOTO 20
          ENDIF
        ENDIF
      ENDDO
   20 CONTINUE
      IF(TEST) WRITE(60,504) LEVCNT,LEVBAD,ZRAT,LOWZ
  504 FORMAT(' POBERR--LEVCNT,LEVBAD,ZRAT,LOWZ: ',2I5,F8.3,I5)
!
!  SEE IF THERE ARE BAD T-S OR Z-S
!
      IF(LOWT.NE.0 .AND. LOWZ.NE.0) THEN
        IF(LOWT.EQ.LOWZ) THEN
          IER = 43
        ELSEIF(LOWT.LT.LOWZ) THEN
          IER = 42
        ELSE
          IER = 41
        ENDIF
        LOW = MIN(LOWT,LOWZ)
      ELSEIF(LOWT.NE.0) THEN
        LOW = LOWT
        IER = 42
      ELSEIF(LOWZ.NE.0) THEN
        LOW = LOWZ
        IER = 41
      ELSE
        LOW = 0
      ENDIF
!
!  DO NOT FLAG LESS THAN 3 LEVELS; LET OBERR DO THE JOB
!
      IF(NLEV-LOW.LT.2) LOW = 0
!
!  IF LOW.NE.0 THEN Z AND T ARE BAD FROM LEVEL LOW TO TOP
!  WRITE EVENTS
!
      IF(IBL.EQ.42 .OR. IBL.EQ.43) THEN
        QM = 13.
      ELSE
        IF(TEST) THEN
! DAK: This is the place in this code where TEST=T will actually encode
!      additional events in the output PREPBUFR file even though it is
!      mean to only add additional daignostic print!!
          QM = 2.    ! This allows diagnosis of errors
        ELSE
          RETURN
        ENDIF
      ENDIF
      TC = 0.
      ZC = 0.
      PTCOR = 0.
      PZCOR = 0.
      IF(LOW.NE.0) THEN
        POUT = .TRUE.
        ISOBERR = .TRUE.
        IF(TEST) WRITE(60,502) SID(IS),LOW
  502   FORMAT(' POBERR--BAD T AND Z FOR STATION ',A8,
     &    ', BEGINNING AT LEVEL ',I3)
        DO L=LOW,NLEV
          IHSC(3,L) = IER
          IHSC(2,L) = IER
          IF(TOB(L).NE.BMISS) THEN
            CALL SEVENT(SID(IS),SQN(IS),L,LST(L),POB(L),LEVTYP(L),3,
     &        TC,TOB(L)+TC,PTCOR,QM,IHSC(3,L),XMISS,XMISS,XMISS)
          ENDIF
          IF(ZOB(L).NE.BMISS) THEN
            CALL SEVENT(SID(IS),SQN(IS),L,LST(L),POB(L),LEVTYP(L),2,
     &        ZC,ZOB(L)+ZC,PZCOR,QM,IHSC(2,L),XMISS,XMISS,XMISS)
          ENDIF
        ENDDO
      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PRNTOUT     PRINT RESULTS FOR A STATION
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Print the results of cqc for a single station.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL PRNTOUT(SEQLP,ICALL)
C   INPUT ARGUMENT LIST:
C     ICALL  - 1 FOR CALL FROM MAIN
C            - 2 FOR CALL FROM CHANGE
C
C   OUTPUT ARGUMENT LIST:
C     SEQLP    - SEQUENCE NO. OF STATION
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 52  - OUTPUT PRINT FILE OF VALUES, RESIDUALS AND EVENTS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE PRNTOUT(SEQLP,ICALL)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      REAL             ZH(255), TH(255), TDH(255), HY(255),
     &                 X(255),  XS(255), HYN(255),
     &                 ZA(255), TA(255)
      INTEGER          NZH(255),  NTH(255),  NTDH(255), NHY(255),
     &                 NHYN(255), NTA(255), NZA(255)
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /TMPSND/  OBT(21,NST,4,2),
     &                 ZIT(21,NST,4), TIT(21,NST,4),
     &                 TMP(21,NST,2), NTMP(21,NST,2),
     &                 ITERR(4)
      COMMON /MANSND/  ZM(21,NST), TM(21,NST), TDM(21), QM(21), MANDL,
     &                 MANDF, ZFM(21), TFM(21), QFM(21), PM(21)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      CHARACTER*8      SID
      CHARACTER*10     CDATE
      COMMON /DATEX/   JDATE(5), CDATE
      COMMON /STN/     IS
      LOGICAL          QUICK, PRNT, XQUICK
      LOGICAL ZG, TG, TDG, QG, ERROR

      PRNT = .TRUE.
      IPEVT = 0
      IF(ICALL.NE.3) THEN
        SEQLPOLD = SEQLP
        SEQLP = SQN(IS)
      ENDIF

      DO I=1,255
        ZH(I)    = BMISS
        TH(I)    = BMISS
        TDH(I)   = BMISS
        HY(I)    = BMISS
        HYN(I)   = BMISS
        X(I)     = BMISS
        XS(I)    = BMISS
        NZH(I)   = -1
        NTH(I)   = -1
        NTDH(I)  = -1
        NHY(I)   = -1
        NHYN(I)  = -1
      ENDDO
      DO L=1,NLVM
        M = MANLEV(POB(L))
        IF(M.NE.0) THEN
          DO LL=1,MAND
            MM = MANLEV(PM(LL))
            IF(MM.EQ.M) GOTO 60
          ENDDO
          GOTO 100
   60     CONTINUE
          ZH(L)  = ZHM(LL,IS)
          TH(L)  = THM(LL,IS)
          TDH(L) = TDHM(LL,IS)
          HY(L)  = HYD(LL)
          HYN(L) = HYDN(LL)
          IF(B(LL).NE.0. .AND. HYDN(LL).NE.BMISS) THEN
            X(L)   = HYDN(LL)/B(LL)
          ENDIF
          IF(B(LL).NE.0. .AND. HYDS(LL).NE.BMISS) THEN
            XS(L)  = HYDS(LL)/B(LL)
          ENDIF
          NZH(L)  = NZHM(LL,IS)
          NTH(L)  = NTHM(LL,IS)
          NTDH(L) = NTDHM(LL,IS)
          NHY(L)  = NHYD(LL)
          NHYN(L) = NHYDN(LL)
  100     CONTINUE
        ENDIF
      ENDDO
      IF(PSINC.NE.BMISS .AND. PSINC.NE.0.) THEN
        IPSINC = ABS(PSINC)/3.5
      ELSE
        IPSINC = 0.
      ENDIF

      IF(ICALL.EQ.3) GOTO 700

C  QUICK RECOGNITION PART
C  ----------------------

      DO L=1,255
        NZA(L) = -1
        NTA(L) = -1
        ZA(L)  = BMISS
        TA(L)  = BMISS
      ENDDO
      DO L=1,NLVM
        LL = MANLEV(POB(L))
        IF(LL.NE.0) THEN
          NZA(L) = NTMP(LL,IS,1)
          NTA(L) = NTMP(LL,IS,2)
          ZA(L)  = TMP(LL,IS,1)
          TA(L)  = TMP(LL,IS,2)
        ENDIF
      ENDDO

      QUICK = .FALSE.
      IF(QUICK) THEN
        WRITE(6,500) SID(IS),YOB(IS),XOB(IS),ELV(IS)
        WRITE(6,504)
        WRITE(6,505) POB(1),NZI(1),NZV(1),NZH(1),NZA(1),NHY(1),
     &    NTI(1),NTV(1),NTH(1),NTL(1),NTA(1),NHYN(1),NTDI(1),
     &   NTDV(1),NTDH(1),(IHSC(I,1),I=1,4),NBAS,IPSINC
        DO L=2,NLVM
          WRITE(6,506) POB(L),NZI(L),NZV(L),NZH(L),NZA(L),NHY(L),
     &      NTI(L),NTV(L),NTH(L),NTL(L),NTA(L),NHYN(L),NTDI(L),
     &     NTDV(L),NTDH(L),(IHSC(I,L),I=1,4)
        ENDDO
      ENDIF

  504 FORMAT(/'          ---Height----  ------Temp------ -DewP T-',
     &        '   ------IHSC------'/
     &        '   PRESS  I  V  H  T HY  I  V  H LP  T HY  I  V  H',
     &        '      P   Z   T  Td  BAS IPSINC')
  505 FORMAT(1X,F7.1,14I3,3X,4I4,I5,I7)
  506 FORMAT(1X,F7.1,14I3,3X,4I4)

C  ALTERNATE QUICK RECOGNITION PART
C  --------------------------------

      XQUICK = .TRUE.
      IF(XQUICK) THEN
        LM = MANLEV(POB(1))
        IF(LM.NE.0) THEN
          XZH(1) = XZHM(LM,IS)
          XTH(1) = XTHM(LM,IS)
          XTDH(1) = XTDHM(LM,IS)
        ELSE
          XZH(1) = BMISS
          XTH(1) = BMISS
          XTDH(1) = BMISS
        ENDIF
        B1 = 10.
        PER = .035
        CALL FUZZY(PZ,XZI(1),XZV(1),XZH(1),B1,PER)
        PER = .035
        CALL FUZZY(PT,XTI(1),XTV(1),XTH(1),B1,PER)
        WRITE(6,500) SID(IS),YOB(IS),XOB(IS),ELV(IS)
        WRITE(6,604)
        WRITE(6,605) POB(1),XZI(1),XZV(1),XZH(1),PZ,NZA(1),NHY(1),
     &    XTI(1),XTV(1),XTH(1),PT,NTL(1),NZA(2),NHYN(1),XTDI(1),
     &   XTDV(1),XTDH(1),(IHSC(I,1),I=1,4),NBAS,IPSINC
        DO L=2,NLVM
          LM = MANLEV(POB(L))
          IF(LM.NE.0) THEN
            XZH(L) = XZHM(LM,IS)
            XTH(L) = XTHM(LM,IS)
            XTDH(L) = XTDHM(LM,IS)
          ELSE
            XZH(L) = BMISS
            XTH(L) = BMISS
            XTDH(L) = BMISS
          ENDIF
          B1 = 10.
          PER = .035
          CALL FUZZY(PZ,XZI(L),XZV(L),XZH(L),B1,PER)
          PER = .035
          CALL FUZZY(PT,XTI(L),XTV(L),XTH(L),B1,PER)
          WRITE(6,606) POB(L),XZI(L),XZV(L),XZH(L),PZ,NZA(L),NHY(L),
     &      XTI(L),XTV(L),XTH(L),PT,NTL(L),NTA(L),NHYN(L),XTDI(L),
     &     XTDV(L),XTDH(L),(IHSC(I,L),I=1,4)
        ENDDO
      ENDIF

  604 FORMAT(/'          ---------------Height---------------',
     &        '  --------------Temperature--------------',
     &        ' ------Dew Pt Temp------',
     &        '   ------IHSC------'/
     &        '   PRESS       I       V       H     ACP  T HY    ',
     &        '   I       V       H     ACP LP  T HY       I     ',
     &        '  V       H      P   Z   T  Td  BAS IPSINC')
  605 FORMAT(1X,F7.1,3(1X,F7.2),1X,F7.3,2I3,3(1X,F7.2),1X,F7.3,3I3,
     &       3(1X,F7.2),3X,4I4,I5,I7)
  606 FORMAT(1X,F7.1,3(1X,F7.2),1X,F7.3,2I3,3(1X,F7.2),1X,F7.3,3I3,
     &       3(1X,F7.2),3X,4I4)

C  PRINT INFORMATION FOR MONITORING
C  --------------------------------

      WRITE(6,500) SID(IS),YOB(IS),XOB(IS),ELV(IS)
      WRITE(6,501) CDATE,DHR(IS),ISC,ITP(IS),PS(IS),PIS(IS),PSINC,
     &             BASRES(IS)
      WRITE(6,502)
      DO L=1,NLVM
        WRITE(6,503) POB(L),ZOB(L),ZI(L),ZV(L),ZH(L),ZA(L),HY(L),HYN(L),
     &    TOB(L),TI(L),TV(L),TH(L),TA(L),X(L),TDO(L),TDI(L),TDV(L),
     &    TDH(L),LST(L),CAT(L)
      ENDDO
      WRITE(6,507)

  700 CONTINUE

C  PRINT INFORMATION FOR SDM-S
C  ---------------------------

      IF(ICALL.EQ.3 .OR. ICALL.EQ.1) THEN
        WRITE(52,500) SID(IS),YOB(IS),XOB(IS),ELV(IS)
        WRITE(52,501) CDATE,DHR(IS),ISC,ITP(IS),PS(IS),PIS(IS),PSINC,
     &             BASRES(IS)
        WRITE(52,508)
        DO L=1,NLVM
          WRITE(52,509) POB(L),ZOB(L),ZI(L),ZV(L),ZH(L),HY(L),HYN(L),
     &      TOB(L),TI(L),TV(L),TH(L),X(L),CAT(L)
        ENDDO
        WRITE(52,507)
      ENDIF

  500 FORMAT(//' STN ID: ',A8,'  LAT:',F7.2,'  LON:',F7.2,'  STN HT:',
     &  F7.0)
  501 FORMAT(' DATE/TIME: ',A10,'  DHOUR:',F4.1,'  SCAN:',I4,
     &  '  INST TYPE:',I4.3,/,' SURFACE PRESS:',F8.1,'    PIS:',F8.1,
     &  '  PSINC:',F8.1,'  BASRES:',F8.0)
  502 FORMAT(/'          <------------Height-------------> ',
     &  '<-Hydr Res-->',
     &  ' <--------------Temperature--------------> <--Dew Point ',
     &  'Temperature--> ',
     &       /'   PRESS    ZOB     ZI     ZV     ZH     ZT   HYDS  ',
     &  ' HYDN    TOB     TI     TV     TH     TT      X    TDO    TDI',
     &  '    TDV    TDH  LST  CAT')
  503 FORMAT(1X,F7.1,7(1X,F6.0),10(1X,F6.1),I5,F5.0)
  507 FORMAT(/)
  508 FORMAT(/'          <---------Height---------> ',
     &  '<-Hydr Res-->',
     &  ' <-----------Temperature----------> ',
     &       /'   PRESS    ZOB     ZI     ZV     ZH   HYDS  ',
     &  ' HYDN    TOB     TI     TV     TH      X',
     &  '  CAT')
  509 FORMAT(1X,F7.1,6(1X,F6.0),5(1X,F6.1),F5.0)

      RETURN
      END

C*********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PRSTNS      PRINT STATION CATEGORY INFORMATION
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT:
C
C PROGRAM HISTORY LOG:
C 1997-03-18 W. Collins  Original author.
C 1999-12-14 W. Collins  Include writing the baseline residual in file
C     NFSTN, so that station-by-station statistics can be calculated.
C
C USAGE:    CALL PRSTNS(WIND)
C   INPUT ARGUMENTS:
C     WIND     - TRUE FOR WIND PART OF REPORT
C
C   OUTPUT FILES:
C     NFSTN    - UNIT NUMBER OF FILE CONTAINING STATION RECEIPT
C                INFORMATION
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE PRSTNS(WIND)
      PARAMETER (NST=1500)
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /ALSNDW/  POBW(255), PFCW(255), PQMW(255), PRCW(255),
     &                 ZOBW(255), ZFCW(255), ZQMW(255), ZRCW(255),
     &                 UOB(255),  VOB(255),  UFC(255),  VFC(255),
     &                 WQM(255),  WRC(255),  SP(255),   DIR(255),
     &                 CATW(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRSW/  UIM(21,NST), VIM(21,NST), SPIM(21,NST),
     &                 DIRIM(21,NST)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      COMMON /DATEX/   JDATE(5), CDATE
      COMMON /STN/     IS
      REAL             ZINC(21),TINC(21),QINC(21),UINC(21),VINC(21),
     &                 SPINC(21),DIRINC(21)
      CHARACTER*4      PART(2)
      CHARACTER*8      SID
      CHARACTER*10     CDATE
      INTEGER          ICAT(5)
      LOGICAL          ZG,TG,TDG,QG,WIND, PRNT, ERROR
      DATA SMISS /9999.9/
      DATA PART /'MASS','WIND'/

C  COUNT THE NUMBER OF LEVELS OF EACH CATEGORY
C  -------------------------------------------

      DO I=1,5
        ICAT(I) = 0
      ENDDO
      DO L=1,NLV
        IF(WIND) THEN
          LCAT = CATW(L)
        ELSE
          LCAT = CAT(L)
        ENDIF
        IF(LCAT.GE.1 .AND. LCAT.LE.5) ICAT(LCAT) = ICAT(LCAT) + 1
      ENDDO

      IF(WIND) THEN

C  PREPARE INCREMENTS FOR PRINTING
C  -------------------------------

        DO I=1,21
          UINC(I)   = AMIN1(UIM(I,IS),SMISS)
          VINC(I)   = AMIN1(VIM(I,IS),SMISS)
          SPINC(I)  = AMIN1(SPIM(I,IS),SMISS)
          DIRINC(I) = AMIN1(DIRIM(I,IS),SMISS)
        ENDDO

        WRITE(NFSTN,502) CDATE,DHR(IS),SID(IS),NLV,PART(2),
     &    (ICAT(I),I=1,5), (UINC(J),J=1,21), (VINC(J),J=1,21),
     &    (SPINC(J),J=1,21), (DIRINC(J),J=1,21)

      ELSE

C  PREPARE INCREMENTS FOR PRINTING
C  -------------------------------

        DO I=1,21
          ZINC(I) = SMISS
          TINC(I) = SMISS
          QINC(I) = SMISS
          BASELINE = SMISS
          PINC = SMISS
        ENDDO
        DO I=1,21
          IF(ZIM(I,IS).LT.SMISS .AND. ZG(I,IS)) ZINC(I) = ZIM(I,IS)
          IF(TIM(I,IS).LT.SMISS .AND. TG(I,IS)) TINC(I) = TIM(I,IS)
          IF(1.E6*QIM(I,IS).LT.SMISS .AND. QG(I,IS))
     &                                        QINC(I) = 1.E6*QIM(I,IS)
          BASELINE = AMIN1(BRES(IS),SMISS)
          PINC = AMIN1(PIS(IS),SMISS)
        ENDDO
        IF(ISF(IS).NE.0) THEN
          ZSINC = AMIN1(ZI(ISF(IS)),SMISS)
        ELSE
          ZSINC = SMISS
        ENDIF
        WRITE(60,501) SID(IS),ISC
        WRITE(NFSTN,500) CDATE,DHR(IS),SID(IS),NLV,PART(1),
     &    (ICAT(I),I=1,5), (ZINC(J),J=1,21), (TINC(K),K=1,21),
     &    (QINC(L),L=1,21), BASELINE, PINC, ZSINC
      ENDIF


  500 FORMAT(1X,A10,1X,F8.2,1X,A8,1X,I5,1X,A4,1X,5I5,/,10X,21F8.0,/,
     &  10X,21F8.1,/,10X,21F8.0,/,10X,2F8.1,F8.0)
  501 FORMAT(' PRSTNS--SID,ISC: ',A8,2X,I3)
  502 FORMAT(1X,A10,1X,F8.2,1X,A8,1X,I5,1X,A4,1X,5I5,/,10X,21F8.1,/,
     &  10X,21F8.1,/,10X,21F8.1,/,10X,21F8.1)

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    QCOI        THE QC ANALYSIS KERNAL
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1990-03-24
C
C ABSTRACT: COMPUTES AND SETS UP THE COEFFIENT MATRIX AND RIGHT
C   HAND SIDES.  SOLVES THE MATRIX PROBLEMS. APPLIES THE COMPUTED
C   WEIGHTS TO THE RESIDUALS TO OBTAIN THE ANALYSIS INCREMENTS AND
C   ANALYSIS ERRORS.
C
C PROGRAM HISTORY LOG:
C 1988-02-05 D. Deaven   Original author.
C 1990-03-24 J. Woollen  ?????.
C
C USAGE:    CALL QCOI(LDIM,IDIM,L0,IV0,NOB1,NOB2,IDH,OINC,HRES,HSTD,
C                     WTS)
C   INPUT ARGUMENTS:
C     LDIM     - L-DIMENSION OF MATRIX
C     IDIM     - I-DIMENSION OF MATRIX
C     L0       -
C     IV0      -
C     NOB1     -
C     NOB2     -
C     IDH      -
C     OINC     -
C   OUTPUT ARGUMENTS:
C     HRES     - ANALYSIS INCREMENTS
C     HSTD     - ANALYSIS STANDARD DEVIATION
C     WTS      - COMPUTED WEIGHTS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
C-----------------------------------------------------------------------
      SUBROUTINE QCOI(LDIM,IDIM,L0,IV0,NOB1,NOB2,IDH,OINC,HRES,HSTD,WTS)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      DIMENSION IDH (4,LDIM,IDIM,NOBN)
      DIMENSION OINC(  LDIM,IDIM,NOBN)
      DIMENSION HRES(  LDIM,IDIM,NOB1:NOB2)
      DIMENSION HSTD(  LDIM,IDIM,NOB1:NOB2)
      DIMENSION WTS (4,LDIM,IDIM,NOB1:NOB2)

      COMMON /OBLIST/ NOBN,INOB(NST),IMAP(360,181)
      COMMON /CORP  / CHLP(0:1500),CVC,IFA(4,4)
      COMMON /LEVEL/   PMAND(21)
      COMMON /HEADER/  CID(NST), DHR(NST), SLON(NST), SLAT(NST),
     &                 SELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      CHARACTER*8 CID

      DIMENSION  NCIND(1000+1),NDIM(1000),NKOBS(4000),
     .           XXCP(4000),YYCP(4000),IICP(4000),
     .           XXOBS(4000),YYOBS(4000),FCAOBS(4000),
     .           CHTWV(10000),DIST(10000),ZZ(10000),
     .           XXI(10000),YYI(10000),XXJ(10000),YYJ(10000)

      DIMENSION FAALL(1000,10),RAALL(1000,4,1),DOTPRD(1000,1)

      DATA MAXDIM /4/
      DATA MINDIM /2/
      DATA MAXOBS /1000/
      DATA NFT    /1/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  SEE IF THIS IS A ONE SHOT DEAL
C  ------------------------------

      IF(NOB1.EQ.NOB2) THEN
         L1   = L0
         L2   = L0
         IV1  = IV0
         IV2  = IV0
      ELSE
         L1   = 1
         L2   = L0
         IV1  = 1
         IV2  = IV0
      ENDIF

C  LOOP OVER VARIABLES AND LEVELS
C  ------------------------------

      DO IV=IV1,IV2
        DO LEV=L1,L2

          LL = LEV
          IPRES = PMAND(LL)

C  CLEAR OUT THE GARBAGE IN THE MATRIX ARRAYS
C  ------------------------------------------

          DO J=1,MAXDIM*(MAXDIM+1)/2
            DO I=1,MAXOBS
              FAALL(I,J) = 0.
            ENDDO
          ENDDO

          DO K=1,NFT
            DO J=1,MAXDIM
              DO I=1,MAXOBS
                RAALL(I,J,K) = 0.
              ENDDO
            ENDDO
          ENDDO

          DO NOB=NOB1,NOB2
            HRES(LEV,IV,NOB) = BMISS
            HSTD(LEV,IV,NOB) = BMISS
            DO I=1,4
              WTS(I,LEV,IV,NOB) = 0.
            ENDDO
          ENDDO

C  STORE THE DATA INDEXS FOR THIS LEVEL
C  ------------------------------------

          IVMAX = 0
          NXXYY = 0
          NCIND(1) = 1

          DO NOB=NOB1,NOB2
            NXXYY = NXXYY + 1
            MDIM  = 0
            IF(IDH(MINDIM,LEV,IV,NOB).NE.0) THEN
              DO N=1,MAXDIM
                INDEX = IDH(N,LEV,IV,NOB)
                IF(INDEX.NE.0) THEN
                  IVMAX = IVMAX + 1
                  IICP(IVMAX) = NOB
                  NKOBS(IVMAX) = INDEX
                  IF(N.GE.MINDIM) MDIM = N
                ENDIF
              ENDDO
            ENDIF
            NDIM(NXXYY) = MDIM
            NCIND(NXXYY+1) = NCIND(NXXYY) + MDIM
          ENDDO

C  PULL THE OBSERVATION INFORMATION OUT OF STORAGE
C  -----------------------------------------------

          DO I=1,IVMAX
            IOB = IICP(I)
            NOB = NKOBS(I)
            XXCP(I)   = SLON(IOB)
            YYCP(I)   = SLAT(IOB)
            XXOBS(I)  = SLON(NOB)
            YYOBS(I)  = SLAT(NOB)
            FCAOBS(I) = OINC(LEV,IV,NOB)
            CHTWV(I)  = CHLP(IPRES)
          ENDDO

C  INITIALIZE THE MATRIX DIAGONAL TERMS
C  ------------------------------------

          DO IGRP=1,NXXYY
            DO IOB=1,NDIM(IGRP)
              FAALL(IGRP,IFA(IOB,IOB)) = 1.5
            ENDDO
          ENDDO

C  COMPUTE THE RIGHT HAND SIDES
C  ----------------------------

          CALL COORS(IVMAX,CHTWV,XXOBS,YYOBS,XXCP,YYCP,DIST,ZZ)

C  STACK THE RHS TERMS IN THE SOLVER ARRAY
C  ---------------------------------------

          DO IGRP=1,NXXYY
            DO IOB=1,NDIM(IGRP)
              I = NCIND(IGRP) + IOB - 1
              RAALL(IGRP,IOB,1) = ZZ(I)
            ENDDO
          ENDDO

C  COMPUTE THE MATRIX TERMS AND STORE IN THE SOLVER
C  ------------------------------------------------

          M = 0
          DO IGRP=1,NXXYY
            IF(NDIM(IGRP).GT.0) THEN
              DO IOB = 1,NDIM(IGRP)-1
                DO JOB = IOB+1,NDIM(IGRP)
                  I = NCIND(IGRP) + IOB - 1
                  J = NCIND(IGRP) + JOB - 1
                  M = M+1
                  XXI(M)   = XXOBS(I)
                  YYI(M)   = YYOBS(I)
                  XXJ(M)   = XXOBS(J)
                  YYJ(M)   = YYOBS(J)
                  CHTWV(M) = CHLP(IPRES)
                ENDDO
              ENDDO
            ENDIF
          ENDDO

          CALL COORS(M,CHTWV,XXI,YYI,XXJ,YYJ,DIST,ZZ)

          M = 0
          DO IGRP=1,NXXYY
            IF(NDIM(IGRP).GT.0) THEN
              DO IOB = 1,NDIM(IGRP)-1
                DO JOB = IOB+1,NDIM(IGRP)
                  M = M+1
                  FAALL(IGRP,IFA(IOB,JOB)) = ZZ(M)
                ENDDO
              ENDDO
            ENDIF
          ENDDO

C  CALL THE MATRIX SOLVER
C------------------------

          CALL DRCTSL(FAALL,RAALL,DOTPRD,NDIM,MAXDIM,NXXYY,NFT,LEV,IV)

C  AFTER SOLUTION OF ALL THE PROBLEMS STORE THE RESULTS
C  ----------------------------------------------------

          DO IGRP=1,NXXYY
            IF(NDIM(IGRP).GT.0 .AND. DOTPRD(IGRP,1).GT.0.) THEN
              NNOB = IICP(NCIND(IGRP))
              SUM = 0.
              DO IOB=1,NDIM(IGRP)
                I = NCIND(IGRP) + IOB - 1
                SUM = SUM + FCAOBS(I) * RAALL(IGRP,IOB,1)
                WTS(IOB,LEV,IV,NNOB) = RAALL(IGRP,IOB,1)
              ENDDO
              IF(ABS(OINC(LEV,IV,NNOB)).LT.BMISS) THEN
                HRES(LEV,IV,NNOB) = OINC(LEV,IV,NNOB) - SUM
                IF(ABS(HRES(LEV,IV,NNOB)).GT.10000.)
     &            HRES(LEV,IV,NNOB) = BMISS
              ELSE
                HRES(LEV,IV,NNOB) = BMISS
              ENDIF
              HSTD(LEV,IV,NNOB) = SQRT(1.5 - DOTPRD(IGRP,1))
            ENDIF
          ENDDO

        ENDDO
      ENDDO

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    RBLOCKS     READ INPUT BLOCKS FOR SINGLE STATION VER
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Read all data necessary to perform cqcht, one station at
C   a time.  This is called only if SINGLE = .true. and if a previous
C   full run has produced the necessary file. This is most useful for
C   testing new logic and runs well on a workstation.  When run in
C   this way, only the Csubroutines in part1 are needed (up through
C   ZDIF).  See Collins for particulars.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL RBLOCKS(SKIP,WIND,ENDIN)
C   OUTPUT ARGUMENT LIST:
C     SKIP     - SET TO .FALSE.
C     WIND     - SET TO .FALSE.
C     ENDIN    - SET TO .TRUE. AT END OF SUBROUTINE
C
C   INPUT FILES:
C     UNIT NFSGL  - ASCII FILE OF ALL DATA NECESSARY TO RUN SINGLE
C                   STATION VERSION OF THIS CODE
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE RBLOCKS(SKIP,WIND,ENDIN)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      COMMON /STN/     IS
      COMMON /DATEX/   JDATE(5), CDATE
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      CHARACTER*8      SID
      CHARACTER*10     CDATE
      COMMON /MANSND/  ZM(21,NST), TM(21,NST), TDM(21), QM(21), MANDL,
     &                 MANDF, ZFM(21), TFM(21), QFM(21), PM(21)
      COMMON /TESTS/   TEST
      LOGICAL          TEST
      REAL             P(255,23)
      INTEGER          IP(255,3)
      LOGICAL SKIP,WIND,ENDIN,PRNT
      LOGICAL ZG, TG, TDG, QG, ERROR
      DATA BMS /99999.99/, QBMS /9.999999/
      IBMS = BMS

C  INITIALIZE QUANTITIES AND SET HORIZONTAL RESIDUALS TO BMISS
C  -----------------------------------------------------------

      PRNT  = .FALSE.
      SKIP  = .FALSE.
      WIND  = .FALSE.
      ENDIN = .FALSE.
      DO I=1,21
        NHYD(I) = -1
        NHYDN(I) = -1
      ENDDO
      DO J=1,NST
        DO I=1,21
          ZHM(I,J)   = BMISS
          THM(I,J)   = BMISS
          TDHM(I,J)  = BMISS
          QHM(I,J)   = BMISS
          NZHM(I,J)  = -1
          NTHM(I,J)  = -1
          NTDHM(I,J) = -1
          NQHM(I,J)  = -1
        ENDDO
      ENDDO
      IS    = 1
      NOBS  = IS

C  READ CONTENTS OF COMMON BLOCKS FROM UNIT 55, FORMATTED FOR EDITING
C  ------------------------------------------------------------------

      IF(TEST) WRITE(60,505)
  505 FORMAT(' RBLOCKS--')
      READ(NFSGL,500,END=100,ERR=100) CDATE,(JDATE(I),I=1,4)
      IF(TEST) WRITE(60,500) CDATE,(JDATE(I),I=1,4)
  500 FORMAT(2X,A10,2X,4I2)
      READ(NFSGL,501,END=100,ERR=100) SID(IS),DHR(IS),XOB(IS),YOB(IS),
     &                 ELV(IS), SQN(IS), ITP(IS), NLV,
     &                 NEV, ISF(IS), NLVM, NLVW
      IF(TEST) WRITE(60,5501) SID(IS), DHR(IS), XOB(IS), YOB(IS),
     &                 ELV(IS), SQN(IS), ITP(IS), NLV,
     &                 NEV, ISF(IS), NLVM, NLVW
  501 FORMAT(2X,A8,4F10.2,F10.0,6I10)
 5501 FORMAT(2X,A8,4F10.2,F10.0,I10.3,5I10)
      READ(NFSGL,503,END=100,ERR=100) MANDL,MANDF,PS(IS),GESPS(IS)
  503 FORMAT(2I10,2F10.2)
      PIS(IS) = PS(IS) - GESPS(IS)
      DO I=1,NLV
        READ(NFSGL,502,END=100,ERR=100) (P(I,J),J=1,23),(IP(I,K),K=1,3)
      ENDDO
  502 FORMAT(4F10.2,F10.6,5F10.2,F10.6,9F10.2,3F10.4,3I10)

      NLVM = NLV
      NLEV = NLV
      DO L=1,NLV
        DO I=1,23
          IF(P(L,I).EQ.BMS) P(L,I) = BMISS
        ENDDO
        IF(P(L,5).EQ.QBMS)  P(L,5) = BMISS
        IF(P(L,11).EQ.QBMS) P(L,11) = BMISS
        DO I=1,3
          IF(IP(L,I).EQ.IBMS) IP(L,I) = BMISS
        ENDDO
      ENDDO

      DO L=1,NLV
        POB(L) = P(L,1)
        ZOB(L) = P(L,2)
        TOB(L) = P(L,3)
        TDO(L) = P(L,4)
        QOB(L) = P(L,5)
        TVO(L) = P(L,6)
        PFC(L) = P(L,7)
        ZFC(L) = P(L,8)
        TFC(L) = P(L,9)
        TDFC(L) = P(L,10)
        QFC(L) = P(L,11)
        PQM(L) = P(L,12)
        ZQM(L) = P(L,13)
        TQM(L) = P(L,14)
        QQM(L) = P(L,15)
        PRC(L) = P(L,16)
        ZRC(L) = P(L,17)
        TRC(L) = P(L,18)
        QRC(L) = P(L,19)
        CAT(L) = P(L,20)
        HRDR(L)= P(L,21)
        YDR(L) = P(L,22)
        XDR(L) = P(L,23)
        IND(L) = IP(L,1)
        LEVTYP(L) = IP(L,2)
        LST(L) = IP(L,3)
      ENDDO

C  FILL MANDATORY LEVEL FIELDS
C  ---------------------------

      DO I=1,NLV
        L = MANLEV(POB(I))
        IF(L.NE.0) THEN
          PM(L) = POB(I)
          ZM(L,IS) = ZOB(I)
          TM(L,IS) = TOB(I)
          TDM(L) = TDO(I)
          QM(L) = QOB(L)
          ZFM(L) = ZFC(I)
          TFM(L) = TFC(I)
          QFM(L) = QFC(I)
        ENDIF
      ENDDO

      RETURN

  100 CONTINUE
      ENDIN = .TRUE.

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    RESIDS      CALCULATE RESIDUALS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Calculate residuals: increment, hydrostatic, vertical
C   statistical, and lapse rate.  Also, determine level types and
C   calculate z-differences from vertical neighbors.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL RESIDS(ITIME)
C   INPUT ARGUMENT LIST:
C     ITIME    - 1 - FIRST CALL TO INPUT FOR STATION
C                2 - SECOND CALL TO INPUT FOR STATION
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE RESIDS(ITIME)

      CALL LEVTYPS
      CALL INCR(ITIME)
      CALL HSC
      CALL VOI(ITIME)
      CALL ZDIF
      CALL LAPSE

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ROUND       ROUND DATA APPROPRIATELY
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Round pressure to nearest 0.1 hPa, height to nearest
C   meter or 10 meters, and temperature to nearest 0.1 degree.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL ROUND(VAL,P,IV)
C   INPUT ARGUMENT LIST:
C     VAL      - UNROUNDED VALUE
C     P        - PRESSURE
C     IV       - VARIABLE:
C                1 - PRESSURE
C                2 - HEIGHT
C                3 - TEMPERATURE
C
C   OUTPUT ARGUMENT LIST:
C     VAL      - ROUNDED VALUE
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE ROUND(VAL,P,IV)

C  ROUND VAL:
C     IF IV.EQ.1 (PRESSURE), THEN
C       ROUND TO NEAREST 0.1 MB
C     IF IV.EQ.2 (HEIGHT), THEN
C       IF(P.GE.700) ROUND TO NEAREST METER
C       ELSE ROUND TO NEAREST 10 METERS
C     IF IV.EQ.3 (TEMPERATURE), THEN
C       ROUND TO NEAREST 0.1 DEGREE
C---------------------------------------------

      IF(VAL.EQ.0.) RETURN

      IF(IV.EQ.2) THEN
        IF(P.GE.700.) THEN
          VAL = ANINT(VAL)
        ELSE
          VAL = 10.*ANINT(VAL/10.)
        ENDIF
      ELSEIF(IV.EQ.1 .OR. IV.EQ.3) THEN
        VAL = ANINT(10.*VAL)/10.
      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: SEARCH
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1990-11-01
C
C ABSTRACT: SELECTS OBS TO BE USED FOR OI QUALITY CONTROL. FIVE
C   FIVE GROUPS OF OBSERVATIONS ARE SELECTED FOR EACH OB BEING
C   CHECKED. ANY OB SELECTED MUST HAVE PASSED THE QC IN THE PREVIOUS
C   ITERATION. INDEXES OF EACH SELECTED OB ARE STORED IN A LIST
C   WHICH THE OI ANALYSIS (SUBROUTINE QCOI) USES TO PERFORM THE
C   QC ANALYSIS ON A PACKAGE OF OBS BEING CHECKED AT ONE TIME.
C
C PROGRAM HISTORY LOG:
C 1990-11-06  J. Woollen  Original author.
C
C USAGE:    CALL SEARCH(LDIM,IDIM,L0,IV0,NOB1,NOB2,IDH,OINC,OG)
C   INPUT ARGUMENTS:
C     LDIM     - L-DIMENSION OF MATRIX
C     IDIM     - I-DIMENSION OF MATRIX
C     L0       -
C     IV0      -
C     NOB1     -
C     NOB2     -
C     IDH      -
C     OINC     -
C     OG       -
C
C OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE SEARCH(LDIM,IDIM,L0,IV0,NOB1,NOB2,IDH,OINC,OG)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      INTEGER   IDH (4,LDIM,IDIM,NOBN)
      REAL      OINC(  LDIM,IDIM,NOBN)

      COMMON /OBLIST/ NOBN,INOB(NST),IMAP(360,181)
      COMMON /HEADER/  CID(NST), DHR(NST), SLON(NST), SLAT(NST),
     &                 SELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /SERCH / RSCAN,DELLAT,DELLON(181)
      COMMON /ISO/ IDISO(NST),NUM,ISISO
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST
      CHARACTER*8 CID

      DIMENSION  NIND(1000),RLAT(1000),RLON(1000),
     .           DIST(1000),DIRN(1000),OBOK(1000),
     .           NDIR(1000),IA(2),IB(2)

      LOGICAL    OBOK, ISISO
      LOGICAL    OG(21,4,NST)

      DATA PI180/.0174532/, RADE/6371./
      DATA MAXCYL/1000/

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C  SEE IF THIS IS A ONE SHOT DEAL
C  ------------------------------

      IF(NOB1.EQ.NOB2) THEN
         L1   = L0
         L2   = L0
         IV1  = IV0
         IV2  = IV0
      ELSE
         L1   = 1
         L2   = L0
         IV1  = 1
         IV2  = IV0
      ENDIF

C  LOOP OVER STATIONS TO BE CHECKED
C  --------------------------------

      DO 60 NOB=NOB1,NOB2
      BLAT   = SLAT(NOB)
      BLON   = SLON(NOB)

C  ITEMIZE REPORTS IN THE SEACH CYLINDER
C  -------------------------------------

      LASTN = 0
      NOBS  = 0

      J1 = MAX(BLAT-DELLAT,-90.) + 91
      J2 = MIN(BLAT+DELLAT, 90.) + 91

      DO J=J1,J2

        I1 = MOD(BLON-DELLON(J)+360.,360.) + 1
        I2 = MOD(BLON+DELLON(J)     ,360.) + 1

        IF(I1.LT.I2) THEN
          IA(1) = I1
          IB(1) = I2
          KK = 1
        ELSE IF(I1.GT.I2) THEN
          IA(1) = I1
          IB(1) = 360
          IA(2) = 1
          IB(2) = I2
          KK = 2
        ELSE IF(I1.EQ.I2) THEN
          IA(1) = 1
          IB(1) = 360
          KK = 1
        ENDIF

        DO K=1,KK
          DO N=IMAP(IA(K),J),IMAP(IB(K),J)
            IF(N.NE.LASTN .AND. INOB(N).NE.NOB) THEN
              NOBS = NOBS+1
              NIND(NOBS) = INOB(N)
              RLAT(NOBS) = SLAT(INOB(N))
              RLON(NOBS) = SLON(INOB(N))
            ENDIF
          ENDDO
          LASTN = N-1
        ENDDO
      ENDDO

C     IF(NOBS.GT.MAXCYL) THEN
C        PRINT *, 'SEARCH: NOBS GT MAXCYL - STOP 99'
C        CALL W3TAGE('PREPOBS_CQCBUFR')
C        CALL ERREXIT
C     ENDIF

C  COMPUTE DISTANCE AND DIRECTION FROM THE CYLINDER CENTER
C  -------------------------------------------------------

      CALL CHDIST(BLON,BLAT,RLON,RLAT,DIST,DIRN,NOBS)

C  CATAGORIZE OBS BY DISTACE AND DIRECTION
C  ---------------------------------------

      DO N=1,NOBS
        NDIR(N) = MIN(IFIX(DIRN(N)/90.) + 1.,4.)
      ENDDO

C  LOOP OVER THE OBSERVATION LEVELS AND VARIABLES TO BE CHECKED
C  ------------------------------------------------------------

      NPICS = 0
      NPICV = 0
      DO L=L1,L2
        DO IV=IV1,IV2
          IF(ABS(OINC(L,IV,NOB)).LT.BMISS) NPICV = NPICV + 1

C  CLEAR THE SEARCH ARRAY
C  ----------------------

          DO I=1,4
            IDH(I,L,IV,NOB) = 0
          ENDDO

C  MARK BAD DATA
C  -------------

C         DO N=1,NOBS
C           OBOK(N) = OG(L,IV,NIND(N))
C         ENDDO

C  PICK CLOSEST DATA FROM EACH DIRECTION
C  -------------------------------------

          IF(ABS(OINC(L,IV,NOB)).LT.BMISS) THEN
            NPIC = 0
            DO J=1,4
              DMIN = BMISS
              DO N=1,NOBS
C               IF(NDIR(N).EQ.J .AND. OBOK(N) .AND.
                IF(NDIR(N).EQ.J .AND. OG(L,IV,NIND(N)) .AND.
     &            DIST(N).GT.10. AND. DIST(N).LT.DMIN) THEN
                  DMIN = DIST(N)
                  NN = N
                ENDIF
              ENDDO
              IF(DMIN.LT.RSCAN) THEN
                NPIC  = NPIC + 1
                NPICS = NPICS + 1
                IDH(NPIC,L,IV,NOB) = NIND(NN)
                IF(TEST) WRITE(60,500) NPIC,L,IV,CID(NOB),
     &            CID(IDH(NPIC,L,IV,NOB)),
     &            OG(L,IV,NIND(NN))
  500           FORMAT(' SEARCH--NPIC,L,IV,CID,CID(IDH),OG: ',
     &            3I7,2X,A8,2X,A8,2X,L1)
              ENDIF
            ENDDO
          ENDIF

        ENDDO
      ENDDO

      READ(CID(NOB),'(I5)',ERR=60) ID
      ISISO = .FALSE.
      IF(NPICS.EQ.0 .AND. NPICV.NE.0) THEN
        ISISO = .TRUE.
        CALL ISOLAT(ID)
      ENDIF

60    CONTINUE                     ! Ends loop:  DO 60 NOB=NOB1,NOB2

      RETURN
      END

C***************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: SETTMP
C   PRGMMR: COLLINS          ORG: NP22       DATE: 1990-11-01
C
C ABSTRACT: SETS TEMPORARY ARRYS TO MISSING
C
C PROGRAM HISTORY LOG:
C 1990-11-06  W. Collins  Original author.
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE SETTMP
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /TMPSND/  OBT(21,NST,4,2),
     &                 ZIT(21,NST,4), TIT(21,NST,4),
     &                 TMP(21,NST,2), NTMP(21,NST,2), ITERR(4)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          PRNT
      DO IV=1,2
        DO IS=1,NST
          DO L=1,MAND
            DO IT=1,4
              OBT(L,IS,IT,IV) = BMISS
            ENDDO
            TMP(L,IS,IV) = BMISS
            NTMP(L,IS,IV) = 0
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C***************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SEVENT      SAVE AN EVENT FOR LATER USE
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Save an event in /EVENTS/ for later use.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL SEVENT(ST,SEQ,L,LST,P,LT,IV,C,CV,CP,QM,IE,
C                       BAS,PIS,PSINC)
C   INPUT ARGUMENT LIST:
C     ST       - STATION ID
C     SEQ      - SEQUENCE NUMBER
C     L        - LEVEL
C     P        - PRESSURE
C     LT       - LEVEL TYPE
C     IV       - VARIABLE
C     C        - CORRECTION
C     CV       - CORRECTED VALUE
C     CP       - ORIGINAL CORRECTION
C     QM       - QUALITY MARK
C     IE       - ERROR TYPE
C     BAS      - BASELINE RESIDUAL
C     PIS      - SURFACE PRESSURE INCREMENT
C     PSINC    - BASELINE RESIDUAL IN TERMS OF PRESSURE
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE SEVENT(ST,SEQ,L,LST,P,LT,IV,C,CV,CP,QM,IE,
     &  BAS,PIS,PSINC)

      REAL(8) BMISS

      SAVE I

      COMMON /EVENTS/  STN(2000),    SEQN(2000),  ISCAN(2000),
     &                 LEVL(2000),   PRES(2000),  LTYP(2000),
     &                 IVAR(2000),   COR(2000),   CORVAL(2000),
     &                 LSTYP(2000),  PCOR(2000),  IEVENT,
     &                 BASR(2000),   PISR(2000),  PSINCR(2000),
     &                 QMARK(2000),  IETYP(2000), EDATE(2000)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /DATEX/   JDATE(5), CDATE
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST, PRNT
      CHARACTER*8     STN, ST
      CHARACTER*10    EDATE, CDATE
      CHARACTER*2     CVAR(5)
      DATA CVAR/' P',' Z',' T','TD',' Q'/
      DATA I/0/

      I = I + 1
      IEVENT = I
      IF(I.GE.2000) THEN
        I = I-1
        IEVENT = I
        WRITE(6,500)
  500   FORMAT(' TOO MANY EVENTS TO RECORD THEM ALL')
        RETURN
      ENDIF
      STN(I)    = ST                           ! station
      SEQN(I)   = SEQ                          ! sequence number
      ISCAN(I)  = ISC                          ! scan
      LEVL(I)   = L                            ! level
      PRES(I)   = P                            ! pressure
      LTYP(I)   = LT                           ! level type
      LSTYP(I)  = LST                          ! surface type
      IVAR(I)   = IV                           ! variable (p,z,T,Td,q)
      COR(I)    = C                            ! correction
      CORVAL(I) = CV                           ! corrected value
      IF(CV.NE.XMISS .AND. C.NE.XMISS) THEN
        OVAL    = CV - C                       ! original value
      ELSE
        OVAL    = XMISS
      ENDIF
      PCOR(I)   = CP                           ! original correciton
      QMARK(I)  = QM                           ! quality mark
      IETYP(I)  = IE                           ! error type
      EDATE(I)  = CDATE                        ! date/time
      BASR(I)   = BAS                          ! baseline residual
      PISR(I)   = PIS                          ! increment of sfc press
      PSINCR(I) = PSINC                        ! baseline res in press

      IPEVT = IPEVT + 1
      IF(TEST) THEN
       WRITE(60,501)
       IF(IVAR(I).LE.3) THEN
        WRITE(60,502) STN(I),EDATE(I),SEQN(I),ISCAN(I),LEVL(I),
     &    PRES(I),LTYP(I),CVAR(IVAR(I)),OVAL,COR(I),CORVAL(I),QMARK(I),
     &    IETYP(I),LSTYP(I),PCOR(I),BASR(I),PISR(I),PSINCR(I)
       ELSE
        WRITE(60,503) STN(I),EDATE(I),SEQN(I),ISCAN(I),LEVL(I),
     &    PRES(I),LTYP(I),CVAR(IVAR(I)),OVAL,COR(I),CORVAL(I),QMARK(I),
     &    IETYP(I),LSTYP(I),PCOR(I),BASR(I),PISR(I),PSINCR(I)
       ENDIF
      ENDIF

  501 FORMAT(' SEVENT--STN          DATE  SQN  SCAN LEVEL  PRESSURE',
     &       ' LEVTYP VAR  ORIG-VAL       COR   NEW-VAL QMARK IETYP',
     &       ' SFCTYP  ORIG-COR BAS-RES  PS-INC  PS-RES')
  502 FORMAT(7X,A8,1X,A10,F6.0,I5,I6,F10.1,I7,2X,A2,3F10.1,F6.0,I6,
     &       I7,F10.1,3(1X,F7.1))
  503 FORMAT(7X,A8,1X,A10,F6.0,I5,I6,F10.1,I7,2X,A2,3F10.6,F6.0,I6,
     &       I7,F10.1,3(1X,F7.1))

      RETURN
      END

C***************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SHELL       SHELL SORT BASED ON V.
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 12-24-91
C
C ABSTRACT: SHELL SORT, BASED UPON THE VALUES OF V.
C   IV IS THE ORIGINAL INDEX OF EACH ELEMENT OF V.
C
C PROGRAM HISTORY LOG:
C 1991-12-24  W. Collins  Original author.
C
C USAGE:    CALL SHELL(V,IV,MAX,IREV)
C   INPUT ARGUMENT LIST:
C     V        - VARIABLE
C     MAX      - DIMENSION OF V
C     IREV     = 0 FOR ASCENDING ORDER
C             <> 0 FOR DESCENDING ORDER
C
C   OUTPUT ARGUMENT LIST:
C     IV       - ORIGINAL INDEX OF VARIABLE
C     V        - VARIABLE, SORTED.
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE SHELL(V,IV,MAX,IREV)
      REAL V(*)
      INTEGER IV(*)
      DO 10 I=1,MAX
        IV(I) = I
   10 CONTINUE
      IOFSET = MAX/2
   20 CONTINUE
      LIM = MAX - IOFSET
   30 CONTINUE
      ISW = 0
      DO 40 I=1,LIM
        IF(V(I).GT.V(I+IOFSET)) THEN
          VT = V(I)
          V(I) = V(I+IOFSET)
          V(I+IOFSET) = VT
          IVT = IV(I)
          IV(I) = IV(I+IOFSET)
          IV(I+IOFSET) = IVT
          ISW = I
        ENDIF
   40 CONTINUE
      LIM = ISW - IOFSET
      IF(ISW.NE.0) GO TO 30
      IOFSET = IOFSET/2
      IF(IOFSET.GT.0) GO TO 20

      IF(IREV.NE.0) THEN
C        REVERSE SORT ORDER...
         NH = MAX/2
         DO I=1,NH
            ITEMP = IV(I)
            IV(I) = IV(MAX+1-I)
            IV(MAX+1-I) = ITEMP
            TEMP = V(I)
            V(I) = V(MAX+1-I)
            V(MAX+1-I) = TEMP
         ENDDO
      ENDIF
      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SIGERR      TEST FOR CORRECTIONS AT SIGNIFICANT LEVEL
C   PRGMMR: KEYSER         ORG: NP22       DATE: 2013-02-05
C
C ABSTRACT: Test for temperature corrections at significant levels.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2008-10-08  Woollen/Keyser -- corrected if tests where integer values
C     were tested for not being equal to real value for missing (BMISS
C     = 10E10), these integer values can never be equal to BMISS for
C     I*4 due to overflow - instead they are now tested for not being
C     equal to new integer value for missing (IMISS, also = 10E10),
C     although this is also an overflow value for I*4, it results in a
C     correct evaluation
C 2013-02-05  D. Keyser -- Final changes to run on WCOSS: Set BUFRLIB
C     missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C     overflow (also done for IMISS).
C
C USAGE:    CALL SIGERR(LM)
C   INPUT ARGUMENT LIST:
C     LM       - LEVEL INDEX WITHIN ALL REPORTED LEVELS
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE SIGERR(LM)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /ALLOW/   ALLZ(41), ALLT(71), NALLZ, NALLT
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /STN/     IS
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST
      CHARACTER*8 SID
      LOGICAL POUT, GOOD(6,2,2)
      LOGICAL ZG, TG, TDG, QG, ERROR, PRNT, SDMQ
      POUT = .FALSE.

      IF(TEST) WRITE(60,500) SID(IS),POB(LM),NTI(LM),NTV(LM)
  500 FORMAT(' SIGERR--CALLED FOR: ',A8,'  ',F10.1,' MB,    NTI,NTV: ',
     &  2I5)

      PTCOR = 0.

      NTVP = 0
                         ! NTV(LM+1) will never test = bmiss, use imiss
cdak  IF(LM+1.LE.NLVM .AND. NTV(LM+1).NE.BMISS) NTVP = NTV(LM+1)
      IF(LM+1.LE.NLVM .AND. NTV(LM+1).NE.IMISS) NTVP = NTV(LM+1)
      NTVM = 0
                         ! NTV(LM-1) will never test = bmiss, use imiss
cdak  IF(LM-1.GE.1 .AND. NTV(LM-1).NE.BMISS) NTVM = NTV(LM-1)
      IF(LM-1.GE.1 .AND. NTV(LM-1).NE.IMISS) NTVM = NTV(LM-1)

      IF(TI(LM).NE.BMISS .AND. NTI(LM).GT.7 .AND.
     &   TV(LM).NE.BMISS .AND. NTV(LM).GT.7) THEN
        NL = NMANLV(POB(LM))
        RATIO = (ABS(TV(LM)-TI(LM)))/XINC(NL,2)
        IF(RATIO.LT.0.25) THEN
          CALL BEST(TCBEST(1),XMISS,XMISS,TI(LM),
     &      XINC(NL,2),TV(LM),VOIRES(NL,2),XMISS,XMISS,XMISS,XMISS)
          IHSC(3,LM) = 2
          TCOR(1) = TCBEST(1)
          TC1 = TCBEST(1)
          CALL SIMPLE(TC1,TOB(LM),ALLT,NALLT,10.)
          CALL ROUND(TC1,POB(LM),3)
          PTCOR = TC1
          TCOR(1) = TC1
          CALL CHECKIT(2,XMISS,XMISS, XMISS, XMISS,XMISS,
     &                   XMISS,XMISS, XMISS, XMISS,XMISS,
     &                   TC1,  TI(LM),TV(LM),XMISS,XMISS,
     &                   XMISS,XMISS, XMISS, XMISS,XMISS,
     &                   XMISS,XMISS,XMISS,XMISS,XMISS,XMISS,GOOD)
          IF(GOOD(6,1,2)) THEN
            QM = 1.
            IHSC(3,LM) = 20
            POUT = .TRUE.
          ELSE
            TC1 = 0.
            QM = 13.
            IHSC(3,LM) = 21
            POUT = .TRUE.
          ENDIF
        ELSE
          TC1 = -TI(LM)
          CALL SIMPLE(TC1,TOB(LM),ALLT,NALLT,10.)
          CALL ROUND(TC1,POB(LM),3)
          PTCOR = TC1
          TCOR(1) = TC1
          CALL CHECKIT(2,XMISS,  XMISS,  XMISS,  XMISS,XMISS,
     &                   XMISS,  XMISS,  XMISS,  XMISS,XMISS,
     &                   TC1,    TI(LM), TV(LM), XMISS,XMISS,
     &                   XMISS,  XMISS,  XMISS,  XMISS,XMISS,
     &                   XMISS,XMISS,XMISS,XMISS,XMISS,XMISS,GOOD)
          IF(GOOD(6,1,2)) THEN
          QM = 1.
          IHSC(3,LM) = 22
          POUT = .TRUE.
          ELSE
            TC1 = 0.
            QM = 13.
            IHSC(3,LM) = 23
            POUT = .TRUE.
          ENDIF
        ENDIF
      ELSEIF(TI(LM).NE.BMISS .AND. NTI(LM).GT.7) THEN
        TC1 = 0.
        PTCOR = TC1
        QM = 3.
        IHSC(3,LM) = 24
        POUT = .TRUE.
      ELSEIF(TV(LM).NE.BMISS .AND. NTV(LM).GT.7 .AND.
     &       NTVP.LT.5 .AND. NTVM.LT.5) THEN    ! Perhaps tropopause
        TC1 = 0.
        PTCOR = TC1
        QM = 1.
        IHSC(3,LM) = 0
        POUT = .TRUE.
      ELSEIF(TV(LM).NE.BMISS .AND. NTV(LM).GT.7 .AND.
     &       NTVP.LT.7 .AND. NTVM.LT.7) THEN
        TC1 = 0.
        PTCOR = TC1
        QM = 3.
        IHSC(3,LM) = 25
        POUT = .TRUE.
      ELSE
        TC1 = 0.
        PTCOR = TC1
        QM = 1.
        IHSC(3,LM) = 0
      ENDIF
      IF(IHSC(3,LM).NE.0) THEN
        CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),3,TC1,
     &    TOB(LM)+TC1,PTCOR,QM,IHSC(3,LM),XMISS,XMISS,XMISS)
        SDMQ = .FALSE.
        SDMQ = QQM(LM).EQ.0. .OR. QQM(LM).GE.14. .OR.
     &        (QQM(LM).GE.4. .AND. QQM(LM).LE.12.)
        IF((QM.EQ.3. .OR. QM.EQ.13.) .AND. QOB(LM).LT.BMISS .AND.
     &     .NOT.SDMQ) THEN
          CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),
     &      LEVTYP(LM),5,0.,QOB(LM),0.,QM,IHSC(4,LM),XMISS,
     &      XMISS,XMISS)
        ENDIF
      ENDIF

      RETURN
      END

C***************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SIMPLE      ATTEMPT TO FIND ONE-DIGIT CORRECTION
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1988-10-17
C
C ABSTRACT: TESTS CORRECTION OF ONE DIGIT OR INTERCHANGE
C   OF DIGITS OF VAL WITH A VALUE OF COR WITHIN RANGE SPECIFIED BY
C   ALL.
C
C PROGRAM HISTORY LOG:
C 1988-10-17  W. Collins  Original author.
C
C USAGE:    CALL SIMPLE(COR,VAL,ALL,N,CON)
C   INPUT ARGUMENT LIST:
C     COR      - HYDROSTATIC CORRECTION (UNITS VARY)
C     VAL      - VALUE TO BE CORRECTED (UNITS VARY)
C     ALL      - ARRAY OF ALLOWABLE VARIATION
C     N        - DIMENSION OF ALL
C     CON      = 1.  FOR HEIGHT
C              = 10. FOR TEMPERATURE
C
C   OUTPUT ARGUMENT LIST:
C     COR      - CORRECTION AS (POSSIBLY) MODIFIED BY
C              - THIS SUBROUTINE
C
C OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE SIMPLE(COR,VAL,ALL,N,CON)
C
C     SUBROUTINE TO TEST CORRECTION OF ONE DIGIT OF VAL
C     OR TWO OR MORE DIGITS OF VAL FOR PERMUTATION
C     WITH A VALUE OF COR WITHIN RANGE SPECIFIED BY ALL.
C       AS AN EXAMPLE:
C         FOR HEIGHT, ALL MIGHT HAVE VALUES OF 0., -10., +10.
C         THE VALUES SHOULD BE IN THE ORDER TO BE TESTED.
C

      REAL(8) BMISS

      REAL ALL(*)
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST
      INTEGER IDIG(0:10), IDIGT(0:10), JDIG(0:10), JDIGT(0:10)

      IF(COR.GE.BMISS) RETURN
      IF(TEST) WRITE(60,500) COR
  500 FORMAT(' SIMPLE--ORIGINAL COR: ',F10.2)
  501 FORMAT('         FINAL COR:    ',F10.2)

C  TEST FOR SIGN CORRECTION
C  ------------------------

      IF(ABS(2.*VAL+COR).LE.ALL(N)/CON) THEN
        COR = -2.*VAL
        RETURN
      ENDIF

      VAL0 = VAL
      IVAL = NINT(CON*VAL)
      VAL  = IVAL
      COR0 = COR
      ICOR = NINT(CON*COR)
      COR  = ICOR
      COR1 = ICOR

      DO 100 NN=1,N
      DO 10 I=0,10
        IDIG(I) = 0
        IDIGT(I) = 0
        JDIG(I) = 0
        JDIGT(I) = 0
   10 CONTINUE

C
C     PUT THE DIGITS OF VAL INTO IDIG, BEGINNING WITH THE
C     UNIT DIGIT AND GOING TO HIGHER DIGITS.
C     DO NOT PAY ATTENTION TO THE SIGN.
C     ALSO COUNT NUMBER OF EACH DIGIT IN JDIG.
C
      IVAL = ABS(VAL)
      IF(IVAL.EQ.0) GO TO 40
      DO 30 I=0,10
        IDIG(I) = IVAL - 10 * (IVAL/10)
        II = IDIG(I) + 1
        JDIG(II) = JDIG(II) + 1
        IVAL = IVAL/10
        IF(IVAL.EQ.0) GO TO 40
   30 CONTINUE
   40 CONTINUE
C
C     MAKE A PROVISIONAL CORRECTION TO VAL.
C
      VALT = VAL + COR + ALL(NN)
C
C     PUT DIGITS OF VALT INTO IDIGT.
C     ALSO COUNT NUMBER OF EACH DIGIT IN JDIGT.
C
      IVALT = ABS(VALT)
      IF(IVALT.EQ.0) GO TO 60
      DO 50 I=0,10
        IDIGT(I) = IVALT - 10 * (IVALT/10)
        II = IDIGT(I) + 1
        JDIGT(II) = JDIGT(II) + 1
        IVALT = IVALT/10
        IF(IVALT.EQ.0) GO TO 60
   50 CONTINUE
   60 CONTINUE
C
C     COUNT THE NUMBER OF DIGITS THAT ARE DIFFERENT
C     BETWEEN IDIG AND IDIGT.
C
      ICNT = 0
      DO 70 I=0,10
        IF(IDIG(I).NE.IDIGT(I)) ICNT = ICNT + 1
   70 CONTINUE
C
C     TEST OF PERMUTATION OF DIGITS.
C
      JCNT = 0
      DO 80 I=0,10
        JCNT = JCNT + (JDIG(I) - JDIGT(I))**2
   80 CONTINUE
C
C     IF (ICNT.EQ.1.(ONE DIGIT DIFFERENT)
C       OR .JCNT.EQ.0) (PERMUTATION OF DIGITS)
C       THEN COR + ALL(NN) IS ACCEPTABLE.
C
      IF(ICNT.EQ.1.OR.JCNT.EQ.0) THEN
        COR = COR + ALL(NN)
        COR = COR/CON
        VAL = VAL0
        IF(TEST) WRITE(60,501) COR
        RETURN
      ENDIF
  100 CONTINUE
C
C     IF THIS POINT IS REACHED, THE ORIGINAL COR, ROUNDED, IS RETAINED.
C
      COR = COR1/CON
      VAL = VAL0
      IF(TEST) WRITE(60,501) COR
      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SORT        SORT, BASED ON ORDER IN INDX
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1994-03-17
C
C ABSTRACT: SORT RA ACCORDING TO THE ORDER SPECIFIED BY THE INDICES
C   IN INDX.
C
C PROGRAM HISTORY LOG:
C 1994-03-17  W. Collins  Original author.
C
C USAGE:    CALL SORT(RA,INDX,N)
C   INPUT ARGUMENT LIST:
C     RA       - VARIABLE
C     INDX     - ORDER FOR REARRANGEMENT OF RA
C     N        - DIMENSION OF RA
C
C   OUTPUT ARGUMENT LIST:
C     RA       - VARIABLE
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE SORT(RA,INDX,N)
C
C     SORT RA ACCORDING TO THE ORDER SPECIFIED BY THE
C     INDICES IN INDX.
C
      PARAMETER (NST=1500)

      DIMENSION RA(*), WKSP(NST)
      INTEGER INDX(*)
      DO J=1,N
         WKSP(J) = RA(J)
      ENDDO
      DO J=1,N
         RA(J) = WKSP(INDX(J))
      ENDDO
      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    STAT        CALCULATE STATISTICS FOR CHECKS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1991-07-23
C
C ABSTRACT: CALCULATE VARIOUS MOMENT STATISTICS FOR INCREMENTS
C   AND RESIDUALS.
C
C PROGRAM HISTORY LOG:
C 1991-07-23  W. Collins  Original author.
C 1998-09-23  W. Collins  Include statistics for 28 regions.  Also,
C     update these statistics to unit 23.
C
C USAGE:    CALL STAT(ITIME)
C   INPUT ARGUMENT LIST:
C     ITIME    - 1 - FIRST CALL TO INPUT FOR STATION
C                2 - SECOND CALL TO INPUT FOR STATION
C
C   INPUT FILES:
C     UNIT 23  - CONTAINS STATISTICS FOR OBSERVATION ERROR CHECKING
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 23  - CONTAINS STATISTICS FOR OBSERVATION ERROR CHECKING
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE STAT(ITIME)
      PARAMETER (NST=1500)

C
C     CALCULATE STATISTICS:
C       V  - VERTICAL
C       H  - HORIZONTAL
C       I  - INCREMENT
C       HY - HYDROSTATIC
C       B  - BASELINE
C       T  - TEMPORAL
C
C     LAST INDEX IN /STATS/ VARIABLES:
C       1  - HEIGHT
C       2  - TEMPERATURE
C       3  - DEW-POINT TEMPERATURE
C       4  - SPECIFIC HUMIDITY
C       5  - NORMALIZED HEIGHT
C       6  - NORMALIZED TEMPERATURE
C       7  - NORMALIZED DEW-POINT TEMPERATURE
C

      REAL(8) BMISS

      REAL XLIM(2), X(NST), DD(7)
      REAL XX(NST,28), AVGIX(21,2,28), STDIX(21,2,28),
     &     SKIX(21,2,28), XKIX(21,2,28)
      INTEGER JJ(28), MSKX(NST,28), NIX(21,2,28)
      INTEGER NDIST(23), MSK(NST)
      COMMON /REGION/  AVGIY(21,2,28), STDIY(21,2,28)
      COMMON /STATS/   NV(21,7), AVGV(21,7), STDV(21,7),
     &                 NH(21,7), AVGH(21,7), STDH(21,7),
     &                 NI(21,7), AVGI(21,7), STDI(21,7),
     &                 NT(21,4),  AVGT(21,4), STDT(21,4),
     &                 NHY(21),   AVGHY(21),  STDHY(21),
     &                 NB,        AVGB,       STDB,
     &                 SKV(21,7), XKV(21,7),
     &                 SKH(21,7), XKH(21,7),
     &                 SKI(21,7), XKI(21,7),
     &                 SKT(21,4), XKT(21,4),
     &                 SKHY(21),  XKHY(21),
     &                 SKB,       XKB
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /TMPSND/  ZOBT(21,NST,4), TOBT(21,NST,4),
     &                 ZIT(21,NST,4),  TIT(21,NST,4),
     &                 ZTMP(21,NST),   TTMP(21,NST),
     &                 NZTMP(21,NST),  NTTMP(21,NST),
     &                 ITERR(4)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /LEVEL/   PMAND(21)
      COMMON /DATEX/   JDATE(5), CDATE
      COMMON /TESTS/   TEST
      COMMON /NO40S/   NORD23, WRT23
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST, NORD23, WRT23
      CHARACTER*8      SID
      CHARACTER*10     CDATE
      LOGICAL ZG, TG, TDG, QG, ERROR, PRNT

      DATA DD /20.,1.,2.,.0001,.5,.5,.5/
      DATA BETA /2.0/

      NLEVM = NLEV - 1
      WRITE(6,533) CDATE
  533 FORMAT(////' DATE: ',A10)
C
C     ZERO FIELDS.
C
      DO I=1,21
        DO J=1,7
          NV(I,J)    = 0
          NH(I,J)    = 0
          NI(I,J)    = 0
          AVGV(I,J)  = 0.
          AVGH(I,J)  = 0.
          AVGI(I,J)  = 0.
          STDV(I,J)  = 0.
          STDH(I,J)  = 0.
          STDI(I,J)  = 0.
        ENDDO
        DO J=1,4
          NT(I,J)    = 0
          AVGT(I,J)  = 0.
          STDT(I,J)  = 0.
        ENDDO
      ENDDO
      DO I=1,21
        NHY(I)   = 0
        AVGHY(I) = 0.
        STDHY(I) = 0.
      ENDDO
      NDIV  = 23
      NB    = 0
      AVGB  = 0.
      STDB  = 0.
      NZERO = 0
      DZERO = 0.
C
C     DEFINE MASK FOR OBSERVATIONS TO INCLUDE IN COMPUTATIONS.
C
      DO I=1,NST
        MSK(I) = 0
      ENDDO
      DO I=1,NOBS
        IF(ERROR(I)) MSK(I) = 1
      ENDDO
      IF(TEST) WRITE(60,536)
  536 FORMAT(' MASK:')
      IF(TEST) WRITE(60,535) (I,MSK(I),I=1,NOBS)
  535 FORMAT(1X,20(I4,I2),/)
      DO 32 IV=1,7
        DDIV = DD(IV)
        IF(ITIME.EQ.1) THEN
          IF(IV.EQ.1) WRITE(6,515)
          IF(IV.EQ.2) WRITE(6,516)
          IF(IV.EQ.3) WRITE(6,525)
          IF(IV.EQ.4) WRITE(6,526)
          IF(IV.LE.4) WRITE(6,512) DZERO, DDIV
        ELSE
          IF(IV.EQ.5) WRITE(6,515)
          IF(IV.EQ.6) WRITE(6,516)
          IF(IV.EQ.7) WRITE(6,525)
          IF(IV.GE.5) WRITE(6,565) DZERO, DDIV
        ENDIF
        WRITE(6,534)
        DO 30 L=1,MAND
          IF(IV.LE.4) THEN
            XLIM(1) = -XINC(L,IV)
            XLIM(2) = XINC(L,IV)
          ELSE
            XLIM(1) = -10.
            XLIM(2) = 10.
          ENDIF
          IF(ITIME.EQ.1) THEN
            IF(IV.EQ.1) THEN
              DO I=1,NOBS
                X(I) = ZIM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NI(L,IV),AVGI(L,IV),
     &          STDI(L,IV),SKI(L,IV),XKI(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.2) THEN
              DO I=1,NOBS
                X(I) = TIM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NI(L,IV),AVGI(L,IV),
     &          STDI(L,IV),SKI(L,IV),XKI(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.3) THEN
              DO I=1,NOBS
                X(I) = TDIM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NI(L,IV),AVGI(L,IV),
     &          STDI(L,IV),SKI(L,IV),XKI(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.4) THEN
              DO I=1,NOBS
                X(I) = QIM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NI(L,IV),AVGI(L,IV),
     &          STDI(L,IV),SKI(L,IV),XKI(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ENDIF
          ELSE
            IF(IV.EQ.5) THEN
              DO I=1,NOBS
                X(I) = XZIM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NI(L,IV),AVGI(L,IV),
     &          STDI(L,IV),SKI(L,IV),XKI(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.6) THEN
              DO I=1,NOBS
                X(I) = XTIM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NI(L,IV),AVGI(L,IV),
     &          STDI(L,IV),SKI(L,IV),XKI(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.7) THEN
              DO I=1,NOBS
                X(I) = XTDIM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NI(L,IV),AVGI(L,IV),
     &          STDI(L,IV),SKI(L,IV),XKI(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ENDIF
          ENDIF
   30   CONTINUE
   32 CONTINUE
  512 FORMAT(' DISTRIBUTION FOR OBSERVED INCREMENTS.',
     &  '  DIVISION ZERO = ',1PE12.3,'  DIVISION INCREMENT = ',
     &  1PE12.3)
  565 FORMAT(' DISTRIBUTION FOR NORMALIZED INCREMENTS.',
     &  '  DIVISION ZERO = ',1PE12.3,'  DIVISION INCREMENT = ',
     &  1PE12.3)
  534   FORMAT(' ONLY STATIONS WITHOUT HYDROSTATIC SUSPICIONS',
     &    ' ARE INCLUDED.')
  513 FORMAT(/'  LEV  ... -10  -9  -8  -7  -6  -5',
     &  '  -4  -3  -2  -1   0   1   2   3   4   5   6',
     &  '   7   8   9  10 ...    MAXIMUM')
  514 FORMAT(1X,I4,1X,23I4,1PE11.3)
  515 FORMAT(/'HEIGHT')
  516 FORMAT(/'TEMPERATURE')
  525 FORMAT(/'DEW-POINT TEMPERATURE')
  526 FORMAT(/'SPECIFIC HUMIDITY')
      DO 42 IV=1,7
        DDIV = DD(IV)
        IF(ITIME.EQ.1) THEN
          IF(IV.EQ.1) WRITE(6,515)
          IF(IV.EQ.2) WRITE(6,516)
          IF(IV.EQ.3) WRITE(6,525)
          IF(IV.EQ.4) WRITE(6,526)
          IF(IV.LE.4) WRITE(6,517) DZERO, DDIV
        ELSE
          IF(IV.EQ.5) WRITE(6,515)
          IF(IV.EQ.6) WRITE(6,516)
          IF(IV.EQ.7) WRITE(6,525)
          IF(IV.GE.5) WRITE(6,566) DZERO, DDIV
        ENDIF
        DO 40 L=1,MAND
          IF(IV.LE.4) THEN
            XLIM(1) = -VOIRES(L,IV)
            XLIM(2) = VOIRES(L,IV)
          ELSE
            XLIM(1) = -10.
            XLIM(2) = 10.
          ENDIF
          IF(ITIME.EQ.1) THEN
            IF(IV.EQ.1) THEN
              DO I=1,NOBS
                X(I) = ZVM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NV(L,IV),AVGV(L,IV),
     &          STDV(L,IV),SKV(L,IV),XKV(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.2) THEN
              DO I=1,NOBS
                X(I) = TVM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NV(L,IV),AVGV(L,IV),
     &          STDV(L,IV),SKV(L,IV),XKV(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.3) THEN
              DO I=1,NOBS
                X(I) = TDVM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NV(L,IV),AVGV(L,IV),
     &          STDV(L,IV),SKV(L,IV),XKV(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.4) THEN
              DO I=1,NOBS
                X(I) = QVM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NV(L,IV),AVGV(L,IV),
     &          STDV(L,IV),SKV(L,IV),XKV(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ENDIF
          ELSE
            IF(IV.EQ.5) THEN
              DO I=1,NOBS
                X(I) = XZVM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NV(L,IV),AVGV(L,IV),
     &          STDV(L,IV),SKV(L,IV),XKV(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.6) THEN
              DO I=1,NOBS
                X(I) = XTVM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NV(L,IV),AVGV(L,IV),
     &          STDV(L,IV),SKV(L,IV),XKV(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.7) THEN
              DO I=1,NOBS
                X(I) = XTDVM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NV(L,IV),AVGV(L,IV),
     &          STDV(L,IV),SKV(L,IV),XKV(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ENDIF
          ENDIF
   40   CONTINUE
   42 CONTINUE
  517 FORMAT(' DISTRIBUTION FOR VERTICAL RESIDUALS.',
     &  '  DIVISION ZERO = ',1PE12.3,'  DIVISION INCREMENT = ',
     &  1PE12.3)
  566 FORMAT(' DISTRIBUTION FOR NORMALIZED VERTICAL RESIDUALS.',
     &  '  DIVISION ZERO = ',1PE12.3,'  DIVISION INCREMENT = ',
     &  1PE12.3)
      DO 52 IV=1,7
        DDIV = DD(IV)
        IF(ITIME.EQ.1) THEN
          IF(IV.EQ.1) WRITE(6,515)
          IF(IV.EQ.2) WRITE(6,516)
          IF(IV.EQ.3) WRITE(6,525)
          IF(IV.EQ.4) WRITE(6,526)
          IF(IV.LE.4) WRITE(6,518) DZERO, DDIV
        ELSE
          IF(IV.EQ.5) WRITE(6,515)
          IF(IV.EQ.6) WRITE(6,516)
          IF(IV.EQ.7) WRITE(6,525)
          IF(IV.GE.5) WRITE(6,567) DZERO, DDIV
        ENDIF
        DO 50 L=1,MAND
          IF(IV.LE.4) THEN
            XLIM(1) = -HOIRES(L,IV)
            XLIM(2) = HOIRES(L,IV)
          ELSE
            XLIM(1) = -10.
            XLIM(2) = 10.
          ENDIF
          IF(ITIME.EQ.1) THEN
            IF(IV.EQ.1) THEN
              DO I=1,NOBS
                X(I) = ZHM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NH(L,IV),AVGH(L,IV),
     &          STDH(L,IV),SKH(L,IV),XKH(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.2) THEN
              DO I=1,NOBS
                X(I) = THM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NH(L,IV),AVGH(L,IV),
     &          STDH(L,IV),SKH(L,IV),XKH(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.3) THEN
              DO I=1,NOBS
                X(I) = TDHM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NH(L,IV),AVGH(L,IV),
     &          STDH(L,IV),SKH(L,IV),XKH(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.4) THEN
              DO I=1,NOBS
                X(I) = QHM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NH(L,IV),AVGH(L,IV),
     &          STDH(L,IV),SKH(L,IV),XKH(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ENDIF
          ELSE
            IF(IV.EQ.5) THEN
              DO I=1,NOBS
                X(I) = XZHM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NH(L,IV),AVGH(L,IV),
     &          STDH(L,IV),SKH(L,IV),XKH(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.6) THEN
              DO I=1,NOBS
                X(I) = XTHM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NH(L,IV),AVGH(L,IV),
     &          STDH(L,IV),SKH(L,IV),XKH(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ELSEIF(IV.EQ.7) THEN
              DO I=1,NOBS
                X(I) = XTDHM(L,I)
              ENDDO
              IF(L.EQ.1) WRITE(6,513)
              CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &          DDIV,NZERO,DZERO,NH(L,IV),AVGH(L,IV),
     &          STDH(L,IV),SKH(L,IV),XKH(L,IV))
              WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
            ENDIF
          ENDIF

   50   CONTINUE
   52 CONTINUE
  518 FORMAT(' DISTRIBUTION FOR HORIZONTAL RESIDUALS.',
     &  '  DIVISION ZERO = ',1PE12.3,'  DIVISION INCREMENT = ',
     &  1PE12.3)
  567 FORMAT(' DISTRIBUTION FOR NORMALIZED HORIZONTAL RESIDUALS.',
     &  '  DIVISION ZERO = ',1PE12.3,'  DIVISION INCREMENT = ',
     &  1PE12.3)
C---------------------------------------------
      IF(ITIME.EQ.2) RETURN
C---------------------------------------------
      DO IV=1,2
        DDIV = DD(IV)
        IF(IV.EQ.1) WRITE(6,515)
        IF(IV.EQ.2) WRITE(6,516)
        WRITE(6,534)
        WRITE(6,571) DZERO, DDIV
        WRITE(6,513)
        DO L=1,MAND
          XLIM(1) = -HOIRES(L,IV)
          XLIM(2) = HOIRES(L,IV)

          IF(IV.EQ.1) THEN
            DO I=1,NOBS
              X(I) = ZTMP(L,I)
            ENDDO
          ELSEIF(IV.EQ.2) THEN
            DO I=1,NOBS
              X(I) = TTMP(L,I)
            ENDDO
          ENDIF

          CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &      DDIV,NZERO,DZERO,NT(L,IV),AVGT(L,IV),
     &      STDT(L,IV),SKT(L,IV),XKT(L,IV))
          WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
        ENDDO
      ENDDO
  571 FORMAT(' DISTRIBUTION FOR TEMPORAL RESIDUALS.',
     &  '  DIVISION ZERO = ',1PE12.3,'  DIVISION INCREMENT = ',
     &  1PE12.3)
C---------------------------------------------
      DDIV = 5.
      WRITE(6,515)
      WRITE(6,534)
      WRITE(6,519) DZERO, DDIV
      WRITE(6,513)
      DO 60 L=1,MAND-1
        XLIM(1) = -HSCRES(L)
        XLIM(2) = HSCRES(L)
        DO I=1,NOBS
          X(I) = HYDM(L,I)
        ENDDO
        CALL DISTR(X,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,
     &    DDIV,NZERO,DZERO,NHY(L),AVGHY(L),
     &    STDHY(L),SKHY(L),XKHY(L))
        WRITE(6,514) L, (NDIST(I),I=1,NDIV), XLIM(2)
   60 CONTINUE
  519 FORMAT(' DISTRIBUTION FOR HYDROSTATIC RESIDUALS.',
     &  '  DIVISION ZERO = ',1PE12.3,'  DIVISION INCREMENT = ',
     &  1PE12.3)
      XLIM(1) = -100.
      XLIM(2) = 100.
      DDIV = 2.
      WRITE(6,520) DZERO, DDIV
      WRITE(6,513)
      WRITE(6,534)
      CALL DISTR(BASRES,MSK,XLIM,XMISS,NOBS,NDIST,NDIV,DDIV,NZERO,
     &  DZERO,NB,AVGB,STDB,SKB,XKB)
      WRITE(6,521) (NDIST(I),I=1,NDIV)
  520 FORMAT(/'DISTRIBUTION FOR BASELINE RESIDUALS.',
     &  '  DIVISION ZERO = ',1PE12.3,'  DIVISION INCREMENT = ',
     &  1PE12.3)
  521 FORMAT(1X,5X,23I4)
C
C     PRINT STATISTICS.
C     THEY WILL BE PRINTED IN FIVE GROUPS:
C       NUMBER, AVERAGE, STANDARD DEVIATION, SKEWNESS, KURTOSIS.
C
C              -------- TEMP --------   --------- HT ---------
C       LEVEL  VERT   HOR   INC   TMP   VERT   HOR   INC   TMP   HYDRO
C         1      X     X     X     X      X      X    X     X      X
C        ...    ...   ...   ...   ...    ...   ...   ...   ...    ...
C        14      X     X     X     X      X     X     X     X      X
C        15      X     X     X     X      X     X     X     X
C
      WRITE(6,500) XMI, XMA, YMI, YMA
  500 FORMAT(////'OVERALL STATISTICS FOR LONGITUDES:',2F8.1,
     &  /,'     AND LATITUDES:',2F8.1)
      WRITE(6,506)
      WRITE(6,501)
  501 FORMAT(/'           ----------- HT ----------',
     &  '   --------- TEMP ----------',
     &  '   --- DEW-PT TEMP --   ----- SP HUM -----')
      WRITE(6,502)
  502 FORMAT(' LEVEL     VERT    HOR    INC    TMP   VERT    HOR',
     &  '    INC    TMP   VERT    HOR    INC   VERT    HOR    INC',
     &  '  HYDRO')
      DO 100 L=1,MAND
        WRITE(6,503) L,NV(L,1),NH(L,1),NI(L,1),NT(L,1),NV(L,2),NH(L,2),
     &     NI(L,2),NT(L,2),NV(L,3),NH(L,3),NI(L,3),NV(L,4),NH(L,4),
     &     NI(L,4),NHY(L)
  100 CONTINUE
      WRITE(6,505) NB
  503 FORMAT(1X,I5,2X,15I7)
  505 FORMAT(' BASELINE:',I7)
  506 FORMAT(/'NUMBERS OF OBSERVATIONS FOR EACH CHECK:')
      WRITE(6,507)
      WRITE(6,538)
      WRITE(6,539)
      DO 110 L=1,MAND
        WRITE(6,540) L,AVGV(L,1),AVGH(L,1),AVGI(L,1),AVGT(L,1),
     &    AVGV(L,2),AVGH(L,2),AVGI(L,2),AVGT(L,2),AVGV(L,3),AVGH(L,3),
     &    AVGI(L,3),AVGV(L,4),AVGH(L,4),AVGI(L,4),AVGHY(L)
  110 CONTINUE
      WRITE(6,511) AVGB
  509 FORMAT(1X,I5,2X,9F7.1,3F7.4,F7.1)
  538 FORMAT(/'           ----------- HT ----------',
     &  '   --------- TEMP ----------',
     &  '   --- DEW-PT TEMP --   -------- SP HUM --------')
  539 FORMAT(' LEVEL     VERT    HOR    INC    TMP   VERT    HOR',
     &  '    INC    TMP   VERT    HOR    INC     VERT      HOR',
     &  '      INC  HYDRO')
  540 FORMAT(1X,I5,2X,11F7.1,3F9.5,F7.1)
  541 FORMAT(1X,I5,2X,15F7.1)
  511 FORMAT(' BASELINE:',F7.1)
  507 FORMAT(/'MEAN OF OBSERVATIONS FOR EACH CHECK:')
      WRITE(6,508)
      WRITE(6,538)
      WRITE(6,539)
      DO 120 L=1,MAND
        WRITE(6,540) L,STDV(L,1),STDH(L,1),STDI(L,1),STDT(L,1),
     &    STDV(L,2),STDH(L,2),STDI(L,2),STDT(L,2),STDV(L,3),STDH(L,3),
     &    STDI(L,3),STDV(L,4),STDH(L,4),STDI(L,4),STDHY(L)
  120 CONTINUE
      WRITE(6,511) STDB
  508 FORMAT(/'STANDARD DEVIATION OF OBSERVATIONS FOR EACH CHECK:')
      WRITE(6,531)
      WRITE(6,501)
      WRITE(6,502)
      DO 130 L=1,MAND
        WRITE(6,541) L,SKV(L,1),SKH(L,1),SKI(L,1),SKT(L,1),SKV(L,2),
     &  SKH(L,2),SKI(L,2),SKT(L,2),SKV(L,3),SKH(L,3),SKI(L,3),
     &  SKV(L,4),SKH(L,4),SKI(L,4),SKHY(L)
  130 CONTINUE
      WRITE(6,511) SKB
  531 FORMAT(/'SKEWNESS OF OBSERVATIONS FOR EACH CHECK:')
      WRITE(6,532)
      WRITE(6,501)
      WRITE(6,502)
      DO 140 L=1,MAND
        WRITE(6,541) L,XKV(L,1),XKH(L,1),XKI(L,1),XKT(L,1),XKV(L,2),
     &    XKH(L,2),XKI(L,2),XKT(L,2),XKV(L,3),XKH(L,3),XKI(L,3),
     &    XKV(L,4),XKH(L,4),XKI(L,4),XKHY(L)
  140 CONTINUE
      WRITE(6,511) XKB
  532 FORMAT(/'KURTOSIS OF OBSERVATIONS FOR EACH CHECK:')

C
C     COMPUTE STATISTICS FOR OBSERVATION ERROR CHECKING: 28 REGIONS
C
      DO IV=1,2
        DO L=1,MAND
          DO J=1,28
            DO I=1,NST
              MSKX(I,J) = 0
            ENDDO
          ENDDO
          XLIM(1) = -XINC(L,IV)
          XLIM(2) = XINC(L,IV)
          IF(IV.EQ.1) THEN
            DO IJ=1,28
              JJ(IJ) = 0
            ENDDO
            DO I=1,NOBS
              CALL AREA(IA,XOB(I),YOB(I))
              IF(IA.NE.0) THEN
                JJ(IA) = JJ(IA) + 1
                XX(JJ(IA),IA) = ZIM(L,I)
                IF(ERROR(I)) MSKX(JJ(IA),IA) = 1
              ENDIF
            ENDDO
          ELSEIF(IV.EQ.2) THEN
            DO IJ=1,28
              JJ(IJ) = 0
            ENDDO
            DO I=1,NOBS
              CALL AREA(IA,XOB(I),YOB(I))
              IF(IA.NE.0) THEN
                JJ(IA) = JJ(IA) + 1
                XX(JJ(IA),IA) = TIM(L,I)
                IF(ERROR(I)) MSKX(JJ(IA),IA) = 1
              ENDIF
            ENDDO
          ENDIF
          DO IA=1,28
            DO I=1,JJ(IA)
              X(I) = XX(I,IA)
              MSK(I) = MSKX(I,IA)
            ENDDO
            CALL DISTR(X,MSK,XLIM,XMISS,JJ(IA),NDIST,NDIV,
     &        DDIV,NZERO,DZERO,NIX(L,IV,IA),AVGIX(L,IV,IA),
     &        STDIX(L,IV,IA),SKIX(L,IV,IA),XKIX(L,IV,IA))
          ENDDO
        ENDDO
      ENDDO
C
C     PRINT RESULTS
C
C     WRITE(6,550)
C 550 FORMAT(' HEIGHT STATISTICS FOR TODAY BY REGION')
C     DO L=1,21
C       WRITE(6,552) PMAND(L)
C 552   FORMAT(/' PRESSURE: ',F8.0)
C       DO J1=1,7
C         J2 = J1+7
C         J3 = J1+14
C         J4 = J1+21
C         WRITE(6,553) NIX(L,1,J1),NIX(L,1,J2),NIX(L,1,J3),NIX(L,1,J4),
C    &      AVGIX(L,1,J1),AVGIX(L,1,J2),AVGIX(L,1,J3),AVGIX(L,1,J4),
C    &      STDIX(L,1,J1),STDIX(L,1,J2),STDIX(L,1,J3),STDIX(L,1,J4)
C 553     FORMAT(1X,4I6,8F8.2)
C       ENDDO
C     ENDDO

C     WRITE(6,554)
C 554 FORMAT(' TEMPERATURE STATISTICS FOR TODAY BY REGION')
C     DO L=1,21
C       WRITE(6,552) PMAND(L)
C       DO J1=1,7
C         J2 = J1+7
C         J3 = J1+14
C         J4 = J1+21
C         WRITE(6,553) NIX(L,2,J1),NIX(L,2,J2),NIX(L,2,J3),NIX(L,2,J4),
C    &      AVGIX(L,2,J1),AVGIX(L,2,J2),AVGIX(L,2,J3),AVGIX(L,2,J4),
C    &      STDIX(L,2,J1),STDIX(L,2,J2),STDIX(L,2,J3),STDIX(L,2,J4)
C       ENDDO
C     ENDDO
C
C     READ OLD STATISTICS FROM UNIT 23 SO THAT THEY CAN BE UPDATED.
C
      DO IA=1,28
        DO IV=1,2
          READ(23,557,ERR=600) (AVGIY(I,IV,IA),I=1,21)
          READ(23,558,ERR=600) (STDIY(I,IV,IA),I=1,21)
        ENDDO
      ENDDO
      NORD23 = .FALSE.
  557 FORMAT(1X,21F8.2)
  558 FORMAT(1X,21F8.3)
      DO IA=1,28
        DO IV=1,2
          DO L=1,21
            ALPHA = AMIN1(1.,REAL(NIX(L,IV,IA))/400.)
            AVGIY(L,IV,IA) = (1.-ALPHA)*AVGIY(L,IV,IA)
     &        + ALPHA*AVGIX(L,IV,IA)
            STDIY(L,IV,IA) = (1.-ALPHA)*STDIY(L,IV,IA)
     &        + ALPHA*STDIX(L,IV,IA)
          ENDDO
        ENDDO
      ENDDO
C
C     PRINT REVISED STATISTICS
C
C     WRITE(6,560)
C 560 FORMAT(' REVISED HEIGHT STATISTICS BY REGION')
C     DO L=1,21
C       WRITE(6,552) PMAND(L)
C       DO J1=1,7
C         J2 = J1+7
C         J3 = J1+14
C         J4 = J1+21
C         WRITE(6,561)
C    &      AVGIY(L,1,J1),AVGIY(L,1,J2),AVGIY(L,1,J3),AVGIY(L,1,J4),
C    &      STDIY(L,1,J1),STDIY(L,1,J2),STDIY(L,1,J3),STDIY(L,1,J4)
C       ENDDO
C     ENDDO
C 561 FORMAT(1X,24X,8F8.2)

C     WRITE(6,564)
C 564 FORMAT(' REVISED TEMPERATURE STATISTICS BY REGION')
C     DO L=1,21
C       WRITE(6,552) PMAND(L)
C       DO J1=1,7
C         J2 = J1+7
C         J3 = J1+14
C         J4 = J1+21
C         WRITE(6,561)
C    &      AVGIY(L,2,J1),AVGIY(L,2,J2),AVGIY(L,2,J3),AVGIY(L,2,J4),
C    &      STDIY(L,2,J1),STDIY(L,2,J2),STDIY(L,2,J3),STDIY(L,2,J4)
C       ENDDO
C     ENDDO
C
C     WRITE NEW STATISTICS TO UNIT 23
C
      IF(WRT23) THEN
        REWIND 23
        DO IA=1,28
          DO IV=1,2
            WRITE(23,557) (AVGIY(I,IV,IA),I=1,21)
            WRITE(23,558) (STDIY(I,IV,IA),I=1,21)
          ENDDO
        ENDDO
      ENDIF

      RETURN

  600 CONTINUE
      NORD23 = .TRUE.
      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    STEVNTS     CALLS EVPROC FOR NEW EVENTS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Calls EVPROC with events created since last call.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL STENVTS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE STEVNTS
      SAVE IEVOLD
      COMMON /EVENTS/  STN(2000),    SEQN(2000),  ISCAN(2000),
     &                 LEVL(2000),   PRES(2000),  LTYP(2000),
     &                 IVAR(2000),   COR(2000),   CORVAL(2000),
     &                 LSTYP(2000),  PCOR(2000),  IEVENT,
     &                 BASR(2000),   PISR(2000),  PSINCR(2000),
     &                 QMARK(2000),  IETYP(2000), EDATE(2000)
      CHARACTER*8 STN
      CHARACTER*10    EDATE
      DATA IEVOLD /0/

      I2 = IEVENT
      ICALL = 1
      CALL EVPROC(IEVOLD+1,I2,ICALL)
      IEVOLD = IEVENT

      RETURN
      END

C*****************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    STNCNT      COUNT STATIONS BY WMO BLOCK.
C   PRGMMR: W. Collins       ORG: NP22       DATE: 1996-08-29
C
C ABSTRACT: ACCUMULATE COUNT OF RADIOSONDE STATIONS PROCESSED BY
C    WMO BLOCK AND TOTAL NUMBER OF SHIPS/RECOS.
C
C PROGRAM HISTORY LOG:
C 1996-08-29  W. Collins  Original author.
C
C USAGE:    CALL STNCNT
C
C   OUTPUT FILES:
C     UNIT NFSUM - UNIT NUMBER OF BLOCK TOTALS FILE
C     UNIT 06    - PRINTOUT
C     UNIT 60    - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE STNCNT

      COMMON /DATEX/   JDATE(5), CDATE
      COMMON /STNKNT/  ISHIP,IBLK(100)
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      COMMON /TESTS/   TEST
      LOGICAL          TEST

      CHARACTER*8      BLOCK
      CHARACTER*10     CDATE

      DATA BLOCK /'BLOCK TO'/
      DATA NZERO /0/

C  GET THE STATION COUNT TOTAL
C  ---------------------------

      ISUM = 0
      DO I=1,99
      ISUM = ISUM + IBLK(I)
      ENDDO

C  WRITE THE SUMMARY TO STANDARD OUTPUT FILES
C  ------------------------------------------

      WRITE(6 ,500) ISUM
      WRITE(6 ,501) ISHIP
      WRITE(6 ,507)

      IF(TEST) WRITE(60,500) ISUM
      IF(TEST) WRITE(60,501) ISHIP
      IF(TEST) WRITE(60,507)

C  WRITE THE SUMMARY TO AN EVENTS FILE
C  -----------------------------------

      DO I=0,90,10
      IF(I.EQ.0) THEN
         WRITE(6 ,508) I, NZERO, (IBLK(J),J=1,9)
         IF(TEST) WRITE(60,508) I, NZERO, (IBLK(J),J=1,9)
      ELSE
         WRITE(6 ,508) I, (IBLK(I+J-1),J=1,10)
         IF(TEST) WRITE(60,508) I, (IBLK(I+J-1),J=1,10)
      ENDIF
      ENDDO

C  WRITE VALUES TO EVENTS FILE FOR SUMMARY
C  ---------------------------------------

      WRITE(NFSUM,509) BLOCK, ISUM, ISHIP, (IBLK(I),I=1,25), CDATE
      WRITE(NFSUM,510) BLOCK, (IBLK(I),I=26,50), CDATE
      WRITE(NFSUM,510) BLOCK, (IBLK(I),I=51,75), CDATE
      WRITE(NFSUM,511) BLOCK, (IBLK(I),I=76,99), CDATE

C  FORMAT STATEMENTS
C  -----------------

500   FORMAT(/'TOTAL NO. OF LAND STATIONS CHECKED:',I5)
501   FORMAT(' TOTAL NO. OF SHIPS/RECOS CHECKED:',I5)
507   FORMAT(' STATION COUNTS BY WMO BLOCK:',//,
     .    6X,'    0    1    2    3    4    5    6    7',
     .    '    8    9',/)
508   FORMAT(1X,11I5)
509   FORMAT(1X,A8,I6,I5,25I4,1X,A10)
510   FORMAT(1X,A8,11X  ,25I4,1X,A10)
511   FORMAT(1X,A8,11X  ,24I4,5X,A10)

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    STYPE       DETERMINE THE TYPE OF ALL SURFACES
C   PRGMMR: KEYSER         ORG: NP22       DATE: 2013-02-05
C
C ABSTRACT: Determine the type of all surfaces.  They are:
C     110 - mandatory, below ground
C     120 - mandatory, first above ground
C     121 - mandatory, first above ground, lower hole boundary
C     130 - mandatory, middle
C     131 - mandatory, middle, lower hole boundary
C     132 - mandatory, middle, upper hole boundary
C     133 - mandatory, middle, isolated
C     140 - mandatory, top
C     142 - mandatory, top, upper hole boundary
C     150 - mandatory, incomplete
C     220 - significant, middle or top
C     224 - significant, middle or top, above top mand level
C     240 - surface
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2008-10-08  Woollen/Keyser -- corrected if tests where integer values
C     were tested for not being equal to real value for missing (BMISS
C     = 10E10), these integer values can never be equal to BMISS for
C     I*4 due to overflow - instead they are now tested for not being
C     equal to new integer value for missing (IMISS, also = 10E10),
C     although this is also an overflow value for I*4, it results in a
C     correct evaluation
C 2013-02-05  D. Keyser -- Final changes to run on WCOSS: Set BUFRLIB
C     missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C     overflow (also done for IMISS).
C
C USAGE:    CALL STYPE
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE STYPE
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          PRNT

      LF  = 0
      DO I=1,255
        LST(I) = BMISS
      ENDDO
      DO L=1,NLEV

C  FIND LM, THE MANDATORY LEVEL INDEX OF NEXT MAND LVL BELOW, AND
C       LP, THE MANDATORY LEVEL INDEX OF NEXT MAND LVL ABOVE
C       WHICH CONTAIN BOTH HEIGHT AND TEMPERATURE.
C  --------------------------------------------------------------

        LL = MANLEV(POB(L))
        LM = BMISS
        IF(L.EQ.1) GOTO 10
        DO K=L-1,1,-1
          IF(CAT(K).EQ.1 .AND. LEVTYP(K).LE.2) THEN
            LM = MANLEV(POB(K))
            GOTO 10
          ENDIF
        ENDDO
   10   CONTINUE
        LP = BMISS
        IF(L.EQ.NLEV) GOTO 20
        DO K=L+1,NLEV
          IF(CAT(K).EQ.1 .AND. LEVTYP(K).LE.2) THEN
            LP = MANLEV(POB(K))
            GOTO 20
          ENDIF
        ENDDO
   20   CONTINUE

C  NOW DETERMINE SURFACE TYPE
C  --------------------------

        IF(CAT(L).EQ.0) THEN
          LST(L) = 240
        ELSEIF(CAT(L).EQ.1) THEN
          IF(LF.EQ.0 .AND. POB(L).LE.PS(IS)) LF = L
          IF(POB(L).GT.PS(IS) .AND. PS(IS).NE.0.) THEN
            LST(L) = 110
          ELSEIF(LEVTYP(L).GE.3) THEN
            LST(L) = 150
                                ! LP will never test = bmiss, use imiss
cdak      ELSEIF(L.EQ.LF .AND. LP.NE.BMISS .AND. LP.GE.LL+3) THEN
          ELSEIF(L.EQ.LF .AND. LP.NE.IMISS .AND. LP.GE.LL+3) THEN
            LST(L) = 121
          ELSEIF(L.EQ.LF) THEN
            LST(L) = 120
                         ! LP and LM will never test = bmiss, use imiss
cdak      ELSEIF(LP.NE.BMISS .AND. LM.NE.BMISS .AND.
          ELSEIF(LP.NE.IMISS .AND. LM.NE.IMISS .AND.
     &           LP.LT.LL+3 .AND. LM.GT.LL-3) THEN
            LST(L) = 130
                         ! LP and LM will never test = bmiss, use imiss
cdak      ELSEIF(LP.NE.BMISS .AND. LM.NE.BMISS .AND.
          ELSEIF(LP.NE.IMISS .AND. LM.NE.IMISS .AND.
     &           LP.GE.LL+3 .AND. LM.GT.LL-3) THEN
            LST(L) = 131
                         ! LP and LM will never test = bmiss, use imiss
cdak      ELSEIF(LP.NE.BMISS .AND. LM.NE.BMISS .AND.
          ELSEIF(LP.NE.IMISS .AND. LM.NE.IMISS .AND.
     &           LP.LT.LL+3 .AND. LM.LE.LL-3) THEN
            LST(L) = 132
                         ! LP and LM will never test = bmiss, use imiss
cdak      ELSEIF(LP.NE.BMISS .AND. LM.NE.BMISS .AND.
          ELSEIF(LP.NE.IMISS .AND. LM.NE.IMISS .AND.
     &           LP.GE.LL+3 .AND. LM.LE.LL-3) THEN
            LST(L) = 133
                          ! LMAND(0) will never test = bmiss, use imiss
cdak      ELSEIF(LMAND(0).NE.BMISS) THEN
          ELSEIF(LMAND(0).NE.IMISS) THEN
                                ! LM will never test = bmiss, use imiss
cdak        IF(L.EQ.LMAND(LMAND(0)) .AND. LM.NE.BMISS
            IF(L.EQ.LMAND(LMAND(0)) .AND. LM.NE.IMISS
     &        .AND. LM.LE.LL-3) THEN
              LST(L) = 142
            ELSEIF(L.EQ.LMAND(LMAND(0))) THEN
              LST(L) = 140
            ELSE
              LST(L) = 130
            ENDIF
          ENDIF
        ELSEIF(CAT(L).EQ.2 .OR. CAT(L).EQ.5) THEN
                          ! LMAND(0) will never test = bmiss, use imiss
cdak      IF(LMAND(0).NE.BMISS) THEN
          IF(LMAND(0).NE.IMISS) THEN
            IF(L.GT.LSFC .AND. L.GT.LMAND(LMAND(0))) THEN
              LST(L) = 224
            ELSEIF(L.GT.LSFC) THEN
              LST(L) = 220
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    T120        FIND ERRORS AT TYPE 120 LEVELS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Find errors at type 120 levels: mandatory, first above
C   ground.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL T120(L,LM)
C   INPUT ARGUMENT LIST:
C     L        - LEVEL INDEX WITHIN MANDATORY LEVELS
C     LM       - LEVEL INDEX WITHIN ALL REPORTED LEVELS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE T120(L,LM)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /STN/     IS
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL LGM,LG0,LGP,POUT, PRNT

      IF(L.LE.0) RETURN

      IF(PS(IS)-POB(LM).GT.200.) THEN
        CON = 1.0
      ELSEIF(PS(IS)-POB(LM).LT.100.) THEN
        CON = 2.0
      ELSE
        CON = 2.0 - (PS(IS)-POB(LM)-100.)/100.
      ENDIF

C  TYPE 120: MAND, FIRST ABOVE GROUND
C  ----------------------------------

C  WHICH HYDROSTATIC RESIDUALS ARE LARGE?
C  --------------------------------------

      POUT = .FALSE.
      LGM = .FALSE.
      LG0 = .FALSE.
      LGP = .FALSE.
      IF(BASRES(IS).NE.BMISS .AND. NBAS.GT.8) LGM = .TRUE.
      IF(HYDN(L).NE.BMISS) THEN
        IF(NHYDN(L).GT.NHLIM) LG0 = .TRUE.
      ENDIF
      IF(HYDN(L+1).NE.BMISS) THEN
        IF(NHYDN(L+1).GT.NHLIM) LGP=.TRUE.
      ELSEIF(L+2.LE.21 .AND. HYDN(L+2).NE.BMISS) THEN
        IF(NHYDN(L+2).GT.NHLIM) LGP=.TRUE.
      ENDIF

C  MAKE DECISION FOR POSSIBLE ERROR TYPE
C  -------------------------------------

      IF(.NOT.LGM .AND. .NOT.LG0) THEN
C                                       POSSIBLE OBSERVATION ERROR
      ELSEIF(((LGM .AND. LG0 .AND. LGP) .OR. (LGM .AND. LGP))
     &  .AND. LEVTYP(LM).LE.2) THEN
        CALL ERR710(L,LM)             ! POSSIBLE TYPE 7-10 ERRORS
C     ELSEIF(LGM .AND. LG0 .AND. .NOT.LGP
      ELSEIF(LG0 .AND. .NOT.LGP
     &  .AND. LEVTYP(LM).LE.2) THEN
        CALL ERR123(L,LM)             ! POSSIBLE TYPE 1,2,3 ERRORS
      ELSEIF(LGM .AND. .NOT.LG0) THEN
        CALL COMPER(L,LM)             ! POSSIBLE COMPUTATION ERROR
      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    T121        FIND ERRORS AT TYPE 121 LEVELS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Find errors at type 121 levels: mandatory, first above
C   ground, lower hole boundary.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL T121(L,LM)
C   INPUT ARGUMENT LIST:
C     L        - LEVEL INDEX WITHIN MANDATORY LEVELS
C     LM       - LEVEL INDEX WITHIN ALL REPORTED LEVELS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE T121(L,LM)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /STN/     IS
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL LGM,POUT

C  TYPE 121: MAND, FIRST ABOVE GROUND, LOWER HOLE BNDRY (ISOLATED)
C  ---------------------------------------------------------------

C  SEE IF BASRES IS LARGE
C  ----------------------

      POUT = .FALSE.
      LGM = .FALSE.
      IF(BASRES(IS).LT.BMISS .AND. NBAS.GT.10)
     &  LGM = .TRUE.

C  MAKE DECISION FOR POSSIBLE ERROR TYPE
C  -------------------------------------

      IF(.NOT.LGM) THEN
C       POSSIBLE OBSERVATION ERROR
      ELSEIF(LGM) THEN
        CALL ERR123(L,LM)              ! POSSIBLE TYPE 1,2,3 ERRORS
      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    T130        FIND ERRORS AT TYPE 130 LEVELS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Find errors at type 130 levels: mandatory, middle.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL T130(L,LM)
C   INPUT ARGUMENT LIST:
C     L        - LEVEL INDEX WITHIN MANDATORY LEVELS
C     LM       - LEVEL INDEX WITHIN ALL REPORTED LEVELS
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE T130(L,LM)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST

      LOGICAL LGM,LGMM,LG0,LGP,POUT, PRNT
      IF(L.LE.1) RETURN

      IF(PS(IS)-POB(LM).GT.200.) THEN
        CON = 1.0
      ELSEIF(PS(IS)-POB(LM).LT.100.) THEN
        CON = 2.0
      ELSE
        CON = 2.0 - (PS(IS)-POB(LM)-100.)/100.
      ENDIF

C  WHICH HYDROSTATIC RESIDUALS ARE LARGE?
C  ALLOW A SINGLE MISSING LEVEL
C  --------------------------------------

      POUT = .FALSE.
      LGM  = .FALSE.
      LGMM = .FALSE.
      LG0  = .FALSE.
      LGP  = .FALSE.

      IF(HYDN(L-1).NE.BMISS) THEN
        IF(NHYDN(L-1).GT.NHLIM) LGM = .TRUE.
      ELSEIF(L.GT.2) THEN
        IF(HYDN(L-2).NE.BMISS) THEN
          IF(NHYDN(L-2).GT.NHLIM) LGMM = .TRUE.
        ENDIF
      ENDIF

      IF(HYDN(L).NE.BMISS) THEN
        IF(NHYDN(L).GT.NHLIM) LG0 = .TRUE.
      ENDIF

      IF(L+1.LT.MAND) THEN
        IF(HYDN(L+1).NE.BMISS) THEN
          IF(NHYDN(L+1).GT.NHLIM) LGP=.TRUE.
        ELSEIF(L+2.LE.MAND .AND. HYDN(L+2).NE.BMISS) THEN
          IF(NHYDN(L+2).GT.NHLIM) LGP=.TRUE.
        ENDIF
      ENDIF

      IF(TEST) WRITE(60,500) POB(LM),L,LM,LGM,LGMM,LG0,LGP,NHYDN(L-1),
     &  NHLIM
  500 FORMAT(' T130--POB(LM),L,LM,LGM,LGMM,LG0,LGP,NHYDN(L-1),NHLIM: ',
     &  F10.1,2I5,4L5,2I5)

C  MAKE DECISION FOR POSSIBLE ERROR TYPE
C  -------------------------------------

      IF(.NOT.LGM .AND. .NOT.LGMM .AND. .NOT.LG0) THEN
C                                       POSSIBLE OBSERVATION ERROR
      ELSEIF((((LGM.OR.LGMM) .AND. LG0 .AND. LGP) .OR.
     &  (LGM .AND. LGP)) .AND. LEVTYP(LM).LE.2) THEN
        CALL ERR710(L,LM)             ! POSSIBLE TYPE 7-10 ERRORS
      ELSEIF((LGM .OR. LGMM) .AND. LG0) THEN
        WRITE(60,501) LGM, LG0
  501   FORMAT(' T130--POSSIBLE TYPE 1,2,3 ERRORS, LGM,LG0: ',2L5)
        CALL ERR123(L,LM)             ! POSSIBLE TYPE 1,2,3 ERRORS
      ELSEIF(LG0 .AND. .NOT.LGP
     &  .AND. LEVTYP(LM).LE.2) THEN
        WRITE(60,502) LGM, LG0
  502   FORMAT(' T130--POSSIBLE TYPE 3 ERROR, LGM,LGP,LEVTYP(LM): ',
     &    2L5,I5)
C       CALL ERR123(L,LM)       ! POSSIBLE TYPE 3 ERROR W COMPENSATION
      ELSEIF(LGM .AND. .NOT.LG0) THEN
        CALL COMPER(L,LM)             ! POSSIBLE COMPUTATION ERROR
      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    T140        FIND ERRORS AT TYPE 140 LEVELS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Find errors at type 140 levels: mandatory, top.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL T140(L,LM)
C   INPUT ARGUMENT LIST:
C     L        - LEVEL INDEX WITHIN MANDATORY LEVELS
C     LM       - LEVEL INDEX WITHIN ALL REPORTED LEVELS
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE T140(L,LM)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL LGM,POUT

C  WHICH HYDROSTATIC RESIDUALS ARE LARGE?
C  --------------------------------------

      POUT = .FALSE.
      LGM = .FALSE.
      IF(L.GT.1) THEN
        IF(HYDN(L-1).LT.BMISS .AND. NHYDN(L-1).GT.NHLIM) LGM = .TRUE.
      ELSEIF(L.GT.2) THEN
        IF(HYDN(L-2).LT.BMISS .AND. NHYDN(L-2).GT.NHLIM) LGM = .TRUE.
      ENDIF

C  MAKE DECISION FOR POSSIBLE ERROR TYPE
C  -------------------------------------

      IF(LGM) THEN
        CALL ERR5(L,LM)
      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    T220        DETERMINE ERRORS FOR SIGNIFICANT LEVELS
C   PRGMMR: KEYSER         ORG: NP22       DATE: 2013-02-05
C
C ABSTRACT: Determine for significant levels: middle or top.  Only
C   suspect communication errors if the hydrostatic residual is large
C   enough.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2008-10-08  Woollen/Keyser -- corrected if tests where integer values
C     were tested for not being equal to real value for missing (BMISS
C     = 10E10), these integer values can never be equal to BMISS for
C     I*4 due to overflow - instead they are now tested for not being
C     equal to new integer value for missing (IMISS, also = 10E10),
C     although this is also an overflow value for I*4, it results in a
C     correct evaluation
C 2013-02-05  D. Keyser -- Final changes to run on WCOSS: Set BUFRLIB
C     missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C     overflow (also done for IMISS).
C
C USAGE:    CALL T220(L,LM)
C   INPUT ARGUMENT LIST:
C     L        - LEVEL INDEX WITHIN MANDATORY LEVELS
C     LM       - LEVEL INDEX WITHIN ALL REPORTED LEVELS
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE T220(L,LM)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /LEVEL/   PMAND(21)
      COMMON /STN/     IS
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST, PRNT

      CHARACTER*8 SID

C  DMA FOR MIDDLE OR TOP SIG LVL
C  ANY ERROR MUST BE ASSUMED TO BE OBSERVATION ERROR IF THE
C  HYDROSTATIC RESIDUAL IS MISSING.
C  --------------------------------------------------------

      LBELOW = LMANLV(POB(LM))
      IF(LBELOW.GT.1) THEN
        IF(HYD(LBELOW).EQ.BMISS) LBELOW = LBELOW - 1
      ENDIF

                            ! LBELOW will never test = bmiss, use imiss
cdak  IF(TEST .AND. LBELOW.GT.1 .AND. LBELOW.NE.BMISS)
      IF(TEST .AND. LBELOW.GT.1 .AND. LBELOW.NE.IMISS)
     &  WRITE(60,501) POB(LM), L, LM, LBELOW, HYDS(LBELOW)
  501 FORMAT(' T220--POB,L,LM,LBELOW,HYDS(LBELOW):  ',F10.1,
     &  3I5,2X,F10.1)

                            ! LBELOW will never test = bmiss, use imiss
cdak  IF(LBELOW.GT.0 .AND. LBELOW.NE.BMISS) THEN
      IF(LBELOW.GT.0 .AND. LBELOW.NE.IMISS) THEN
        IF(HYDS(LBELOW).NE.BMISS) THEN
          IF(LMAND(LBELOW).GT.0 .AND. LMAND(LBELOW).LE.255) THEN
            P1  = POB(LMAND(LBELOW))
            AP1 = ALP(P1)
          ELSE
            AP1 = BMISS
          ENDIF
          PN  = POB(LM)
          APN = ALP(PN)
          IF(LM+1.LE.NLVM) THEN
            PLP  = POB(LM+1)
            APLP = ALP(PLP)
          ELSE
            PLP  = BMISS
            APLP = BMISS
          ENDIF
          IF(LM-1.GE.1) THEN
            PLM  = POB(LM-1)
            APLM = ALP(PLM)
          ELSE
            PLM  = BMISS
            APLM = BMISS
          ENDIF

          IF(TEST) WRITE(60,502) LMAND(LBELOW),P1,PN,PLP,PLM
  502     FORMAT(' T220--LMAND(LBELOW),P1,PN,PLP,PLM: ',
     &      I5,5F10.1)

          IF(AP1.NE.BMISS .AND. APN.NE.BMISS .AND.
     &       APLM.NE.BMISS .AND. APLP.NE.BMISS .AND.
     &       AP1.NE.APN) THEN
            RATIO = (APLP-APLM)/(APN-AP1)
          ELSE
            RATIO = 1.0
          ENDIF
          HYDLG = ABS(HYD(LBELOW)) * RATIO

C  TEST FOR LARGE HYDROSTATIC RESIDUAL AT SIG LVL
C  ----------------------------------------------

          IF(HYDLG.GT.0.20*HSCRES(NMANLV(POB(LM)))) THEN
            CALL SIGERR(LM)
          ELSE
            IF(TEST) WRITE(60,500) HYDLG,SID(IS),POB(LM)
  500       FORMAT(' T220--HYDLG = ',F10.1,' NOT LARGE; NO CORR',
     &        ' ATTEMPTED FOR: ',A8,' AT ',F10.1,' MB')
          ENDIF
        ENDIF
      ENDIF

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    T240        DETERMINE ERRORS AT THE SURFACE
C   PRGMMR: KEYSER         ORG: NP22       DATE: 2013-02-05
C
C ABSTRACT: Determine errors at the surface:
C           Type 100 - surface pressure communication error
C           Type 106 - surface pressure observation error
C           Type 102 - surface temperature communication error
C           Type 105 - surface error of undetermined type
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2008-10-08  Woollen/Keyser -- modified to correct indexing error
C     which can lead, on rare occasions, to out-of-bounds arrays and
C     to the incorrect indexing of some variables; corrected if tests
C     where integer values were tested for not being equal to real
C     value for missing (BMISS = 10E10), these integer values can
C     never be equal to BMISS for I*4 due to overflow - instead they
C     are now tested for not being equal to new integer value for
C     missing (IMISS, also = 10E10), although this is also an overflow
C     value for I*4, it results in a correct evaluation
C 2013-02-05  D. Keyser -- Final changes to run on WCOSS: Set BUFRLIB
C     missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C     overflow (also done for IMISS).
C
C USAGE:    CALL T240(L,LM)
C   INPUT ARGUMENT LIST:
C     L        - LEVEL INDEX WITHIN MANDATORY LEVELS
C     LM       - LEVEL INDEX WITHIN ALL REPORTED LEVELS
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 22  - LIST OF POTENTIAL WIND PROBLEMS
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE T240(L,LM)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /LEVEL/   PMAND(21)
      COMMON /CONSTS/  R, G, T0, CP, RV
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /ALLOW/   ALLZ(41), ALLT(71), NALLZ, NALLT
      COMMON /STN/     IS
      COMMON /HYDSTUF/ ZCOR(2),TCOR(2),H1,H2,T1,ZCBEST(2),TCBEST(2),
     &                 POUT,LAST,PSCOR,TSCOR,ZSCOR
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST, PRNT, FIRST
      CHARACTER*8 SID
      LOGICAL POUT, GOOD(6,2,2), L100, L106, L102, L105
      LOGICAL ZG, TG, TDG, QG, ERROR
      DATA PSLIM /3./

      IF(TEST) WRITE(60,500) SID(IS),PSINC,PIS(IS),BASRES(IS)
  500 FORMAT(' T240--FOR STATION: ',A8,'  PSINC: ',F10.1,
     &       '  PIS: ',F10.1,' BASRES: ',F10.1)

      LS = NMANLV(POB(LM))
      LSP = LMAND(MIN(LS+1,MAND))
      LSS = LMAND(LS)

      RO2G  = 0.5*R/G
C     C1    = .434294
      C1    = .5
      IF(POB(LM).NE.0. .AND. POB(LM).NE.BMISS) THEN
        DZDP  = R*(TOB(LM)+T0)/(G*POB(LM))
      ELSE
        DZDP = 8.
      ENDIF
C     write(6,600) dzdp
  600 format(' T240--dzdp: ',f10.2)
      PSCOR = 0.
      TSCOR = 0.
      ZSCOR = 0.
      PPSCOR = 0.
      PZSCOR = 0.
      PTSCOR = 0.
      IHSC(1,LM) = 0
      IHSC(2,LM) = 0
      IHSC(3,LM) = 0
      IHSC(4,LM) = 0

C  TYPE 240: SURFACE
C  -----------------

C  CALCULATE QUANTITIES TO ALLOW DETERMINATION OF MOST LIKELY ERROR
C  ----------------------------------------------------------------

      R0 = BMISS
      R1 = BMISS
      R2 = BMISS
      R3 = BMISS
      R4 = BMISS
      R5 = BMISS
      SX = BMISS
      X0 = 0.5
      X1 = 1.0
      X2 = 1.0
      X3 = 0.65
      X4 = 0.25
      X5 = 1.0
      L100 = .FALSE.
      L106 = .FALSE.
      L102 = .FALSE.
      L105 = .FALSE.

      R0 = ABS(BASRES(IS)/DZDP)/PSLIM
      IF(PSINC.NE.BMISS .AND. ABS(PSINC).GT.PSLIM .AND.
     &   PIS(IS).NE.BMISS .AND. ABS(PIS(IS)).GT.PSLIM) THEN
        R1 = ABS(PSINC-PIS(IS))/(PSLIM*C1*ALP(ABS(PSINC)))
C       write(6,601) r1
  601   format(' T240--r1: ',f10.2)
      ENDIF

      IF(PIS(IS).NE.BMISS .AND. ABS(PIS(IS)).GT.PSLIM) THEN

        IF(LSS.NE.IMISS) THEN
           R2 = ABS(PIS(IS)-ZI(LSS)/DZDP)/PSLIM
C          write(6,602) r2
  602      format(' T240--r2: ',f10.2)
                               ! LSP will never test = bmiss, use imiss
cdak       IF(LSP.NE.BMISS) THEN
           IF(LSP.NE.IMISS) THEN
              R5 = ABS((ZI(LSP)-ZI(LSS))/DZDP)/PSLIM
C             write(6,603) r5
  603         format(' T240--r5: ',f10.2)
           ENDIF
        ENDIF

        NSUM = 0
        SUM  = 0.
        SUMS = 0.
        DO I=1,NLEV
          IF(MANLEV(POB(I)).NE.0 .AND. ZI(I).NE.BMISS) THEN
            NSUM = NSUM + 1
            SUM  = SUM + ZI(I)
            SUMS = SUMS + ZI(I)**2
          ENDIF
        ENDDO
        XSUM = NSUM
        IF(XSUM.NE.0.) THEN
          SX = (SUMS - (SUM**2)/XSUM)/XSUM
        ELSE
          SX = 0.
        ENDIF
        IF(SX.GT.0.) THEN
          SX = SQRT(SX)
        ELSE
          SX = 0.
        ENDIF
        R3 = (SX/DZDP)/PSLIM
C       write(6,604) r3
  604   format(' T240--r3: ',f10.2)
      ENDIF

      IF(BASRES(IS).NE.BMISS .AND. ABS(BASRES(IS)).GT.20.) THEN
        DO I=LM+1,NLEV
          IF(LEVTYP(I).NE.3) GOTO 10
        ENDDO
        GOTO 20
   10   CONTINUE
        IF(ABS(POB(I)-POB(LM)).GT.0.05) THEN
          TSCOR = -BASRES(IS)/(RO2G*(ALP(POB(I))-ALP(POB(LM))))
        ELSE
          TSCOR = 0.
        ENDIF
C       write(6,606) tscor
  606   format(' T240--tscor: ',f10.1)
        CALL ROUND(TSCOR,POB(LM),3)
        CALL BEST(TCBEST(1),XMISS,XMISS,TI(LM),
     &    XINC(LS,2),TV(LM),VOIRES(LS,2),XMISS,XMISS,XMISS,XMISS)
        R4 = ABS(TSCOR-TCBEST(1))/XINC(LS,2)
C       write(6,605) r4
  605   format(' T240--r4: ',f10.2)
   20   CONTINUE
      ENDIF

      R100 = BMISS
      R106 = BMISS
      R102 = BMISS

      IF(R1.NE.BMISS) R100 = R1/X1

C     IF(R0.NE.BMISS .AND. R2.NE.BMISS .AND. R3.NE.BMISS .AND.
C    &   R5.NE.BMISS) THEN
C       R106 = AMAX1(R0/X0,R2/X2,R3/X3,R5/X5)
C     ELSEIF(R2.NE.BMISS .AND. R3.NE.BMISS .AND. R5.NE.BMISS) THEN
C       R106 = AMAX1(R2/X2,R5/X5,R3/X3)
C     ELSEIF(R2.NE.BMISS .AND. R3.NE.BMISS) THEN
C       R106 = AMAX1(R2/X2,R3/X3)
C     ENDIF

      IF(R0.NE.BMISS .AND. R2.NE.BMISS .AND. R5.NE.BMISS) THEN
        R106 = AMAX1(R0/X0,R2/X2,R5/X5)
      ELSEIF(R2.NE.BMISS .AND. R5.NE.BMISS) THEN
        R106 = AMAX1(R2/X2,R5/X5)
      ENDIF

      IF(R4.NE.BMISS) R102 = R4/X4

C  THE MOST LIKELY ERROR IS ASSOCIATED WITH THE SMALLEST Rxxx
C  ----------------------------------------------------------

      IF(R100.LT.1.0 .AND. R100.LE.R106 .AND.
     &   R100.LE.R102)                           L100 = .TRUE.
      IF(R106.LT.1.0 .AND. R106.LE.R100 .AND.
     &   R106.LE.R102)                           L106 = .TRUE.
      IF(R102.LT.1.0 .AND. R102.LT.R100 .AND.
     &   R102.LE.R106)                           L102 = .TRUE.
      IF(.NOT.L100 .AND. .NOT.L106 .AND. .NOT.L102 .AND.
     &   (((BASRES(IS).NE.BMISS .AND. ABS(BASRES(IS)).GT.20.) .OR.
     &       (PSINC.NE.BMISS .AND. ABS(PSINC).GT.PSLIM) .OR.
     &       (PIS(IS).NE.BMISS .AND. ABS(PIS(IS)).GT.2.*PSLIM)))
     &                     )                     L105 = .TRUE.
C    &       .AND. ISC.GT.1)                     L105 = .TRUE.

      IF(TEST) WRITE(60,506) ISC,R100,R106,R102,L100,L106,L102,L105
  506 FORMAT(' T240--ISC,R100,R106,R102,L100,L106,L102,L105: ',
     &  I5,2X,3F16.3,4L3)

C  WRITE DIAGNOSTIC STUFF TO UNIT 22
C  ---------------------------------

      FIRST = .TRUE.
      DO I=1,NLEV
        M = MANLEV(POB(I))
        IF(M.NE.0) THEN
          IF(FIRST .AND. HYD(M).NE.BMISS) THEN
            HYD1 = HYD(M)
            FIRST = .FALSE.
          ENDIF
        ENDIF
      ENDDO
      IF(R1.NE.BMISS .OR. R2.NE.BMISS .OR. R3.NE.BMISS .OR.
     &   R4.NE.BMISS .OR. R5.NE.BMISS) THEN
         IF(LSS.NE.IMISS .AND .LSP.NE.IMISS) THEN
      WRITE(22,507) SID(IS),XOB(IS),YOB(IS),ELV(IS),PS(IS),LSS,POB(LSS),
     &  LSP,POB(LSP)
  507 FORMAT('--------------------------------------------------------',
     &       '--------------------------------------------------------',
     &       '------------------------------------------------------',/,
     &       'SID, XOB, YOB, ELV, PS, LSS, P(LSS), LSP, P(LSP)        ',
     &  A10,2F10.2,F10.0,F10.1,I10,F10.1,I10,F10.1)
      WRITE(22,508) PIS(IS),PSINC,BASRES(IS),HYD1,ZI(LSS),ZI(LSP),
     &  ZI(LSP)-ZI(LSS),SX
  508 FORMAT('PIS,PSINC,BASRES,HYD1,ZI(LSS),ZI(LSP),ZI(LSP)-ZI(LSS),SX',
     &  2F10.1,5F10.0,F10.1)
      WRITE(22,509) PSINC-PIS(IS),PIS(IS)-ZI(LSS)/DZDP,DZDP,TSCOR,
     &  TCBEST(1),TSCOR-TCBEST(1)
  509 FORMAT('PSINC-PIS,PIS-ZI(LSS)/DZDP,DZDP,TSCOR,TCBEST,',
     & 'TSCOR-TCBEST',2F10.1,F10.2,3F10.1)
      WRITE(22,510) R0,R1,R2,R3,R4,R5,R100,R102,R106,L100,L102,L105,L106
  510 FORMAT('R0,R1,R2,R3,R4,R5,R100,R102,R106,L100,L102,L105,L106    ',
     &  9F10.2,4L5)
      ENDIF
      ENDIF

C  IM NOT READY TO LET TYPE 106 CORRECTIONS BE PERFORMED
C  -----------------------------------------------------

      IF(L106) THEN
        L106 = .FALSE.
        L105 = .TRUE.
      ENDIF

      IF(L100) GOTO 100
      IF(L106) GOTO 200
      IF(L102) GOTO 300
      IF(L105) GOTO 400
C598  IF(PSINC.NE.BMISS .AND. ABS(PSINC).GT.2.*PSLIM) GOTO 400
      RETURN

C  CHECK FOR PS-COMM ERROR (TYPE 100)
C  ----------------------------------

  100 CONTINUE

      POUT = .TRUE.
      IF(TEST) WRITE(60,501) R1
  501 FORMAT(' T240--POSSIBLE PS-COMM ERROR, R1: ',F10.2)
      LMAND1 = LMAND(1)
      PSCOR = -PSINC
      CALL SIMPLE(PSCOR,POB(LM),ALLZ,NALLZ,10.)
      CALL ROUND(PSCOR,POB(LM),1)
      ZSCOR = -PSCOR*DZDP
      PPSCOR = PSCOR
      PZSCOR = ZSCOR
      IV = 1
      QM = 1.
      IHSC(1,LM) = 100
      CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &  1,PSCOR,POB(LM)+PSCOR,PPSCOR,QM,IHSC(1,LM),BASRES(IS),PIS(IS),
     &  PSINC)
      RETURN

C  CHECK FOR PS-OBS ERROR
C  ----------------------

  200 CONTINUE

      IF(TEST) WRITE(60,502)
  502 FORMAT(' T240--POSSIBLE PS-OBS ERROR')
      POUT = .TRUE.
      IF(TEST) WRITE(60,503) sx,r2,r3,r5,dzdp
  503 FORMAT(' T240--std of ZI,R2,R3,R5,DZDP: ',5f10.2)
      PSCOR = -PIS(IS)
      CALL SIMPLE(PSCOR,POB(LM),ALLZ,NALLZ,10.)
      CALL ROUND(PSCOR,POB(LM),1)
      ZCR = PSCOR*DZDP
      IF(ABS(ZCR).LT.ZCLIM) RETURN
      CALL ROUND(ZCR,500.,2)
      ZSCOR =  ZCR
      PPSCOR = PSCOR
      PZSCOR = ZSCOR
      QM = 1.
      IHSC(1,LM) = 106
      CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),1,
     &  PSCOR,POB(LM)+PSCOR,PPSCOR,QM,IHSC(1,LM),BASRES(IS),
     &  PIS(IS),PSINC)
      QM = 1.
      IHSC(2,LSFC) = 106
      CALL SEVENT(SID(IS),SQN(IS),LSFC,LST(LSFC),POB(LM)+PSCOR,
     &  LEVTYP(LSFC),2,0.,ELV(IS),0.,QM,IHSC(2,LSFC),
     &  BASRES(IS),PIS(IS),PSINC)
      DO I=1,NLEV
      IHSC(2,I) = 106
        IF(MANLEV(POB(I)).NE.0 .AND. ZOB(I).NE.BMISS) THEN
          CALL SEVENT(SID(IS),SQN(IS),I,LST(I),POB(I),LEVTYP(I),2,
     &      ZCR,ZOB(I)+ZCR,PZSCOR,QM,IHSC(2,I),BASRES(IS),
     &      PIS(IS),PSINC)
        ENDIF
      ENDDO
      RETURN

C  CHECK FOR TS ERROR
C  ------------------

  300 CONTINUE

      TC1 = 0.5*(TSCOR+TCBEST(1))
      PTSCOR = TC1
      IF(TEST) WRITE(60,504)
  504 FORMAT(' T240--POSSIBLE TS ERROR')
      IF(TEST) WRITE(60,505) TSCOR,TCBEST(1),TC1,XINC(LS,2),R4
  505 FORMAT(' T240--TSCOR: ',F10.1,'  TCBEST(1): ',F10.1,
     &  '  GUESS COR: ',F10.1,' XINC: ',F10.1,' R4: ',F10.1)
      IHSC(3,LM) = 102
      POUT = .TRUE.
C     TC1 = TSCOR
      CALL SIMPLE(TC1,TOB(LM),ALLT,NALLT,10.)
      CALL ROUND(TC1,POB(LM),3)
      IF(ABS(TC1).LT.TCLIM) THEN
        IF(ABS(PTSCOR).LT.TCLIM) THEN
          TC1 = 0.
          QM = 3.
          IHSC(3,LM) = 105
        ELSE
          TC1 = PTSCOR
          CALL ROUND(TC1,POB(LM),3)
          QM = 1.
        ENDIF
      ELSE
        QM = 1.
      ENDIF
      TCOR(1) = TC1
      CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &  3,TC1,TOB(LM)+TC1,PTSCOR,QM,IHSC(3,LM),BASRES(IS),
     &  PIS(IS),PSINC)
      RETURN


C  OTHER ERRORS, PROBABLY IN PS
C  ----------------------------

  400 CONTINUE

      IHSC(1,LM) = 105
      PC1 = 0.
      QM = 1.
      IF(BASRES(IS).NE.BMISS .AND. ABS(BASRES(IS)).GT.20.)    QM = 3.
      IF(PSINC.NE.BMISS      .AND. ABS(PSINC).GT.2.*PSLIM)    QM = 3.
      IF(PIS(IS).NE.BMISS    .AND. ABS(PIS(IS)).GT.4.*PSLIM)  QM = 3.
      IF(BASRES(IS).NE.BMISS .AND. ABS(BASRES(IS)).GT.50.)    QM = 13.
      IF(PSINC.NE.BMISS      .AND. ABS(PSINC).GT.4.*PSLIM)    QM = 13.
      IF(PIS(IS).NE.BMISS    .AND. ABS(PIS(IS)).GT.8.*PSLIM)  QM = 13.
      CALL SEVENT(SID(IS),SQN(IS),LM,LST(LM),POB(LM),LEVTYP(LM),
     &  1,PC1,POB(LM)+PC1,0.,QM,IHSC(1,LM),BASRES(IS),
     &  PIS(IS),PSINC)
      RETURN

      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    TMPCHK      COMPUTES TEMPORAL RESIDUALS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1990-12-06
C
C ABSTRACT: COMPUTES TEMPORAL RESIDUALS
C
C PROGRAM HISTORY LOG:
C   UNKNOWN   W. Collins  Original author.
C   2020-08-09  S. Melchior - explicitly defined ICK as an
C       integer. Moved ICK.NE.0 logic inside ITI.NE.0 logic.
C       BENEFIT: corrects problems when compiled and run with
C                full DEBUG options enabled.
C
C USAGE:    CALL TMPCHK
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE TMPCHK
      PARAMETER (NST=1500)

      REAL(8) BMISS

C     COMMON /TMPSND/  ZOBT(21,NST,4), TOBT(21,NST,4),
C    &                 ZIT(21,NST,4),  TIT(21,NST,4),
C    &                 ZTMP(21,NST),   TTMP(21,NST),
C    &                 NZTMP(21,NST),  NTTMP(21,NST),
C    &                 ITERR(4)
      COMMON /TMPSND/  OBT(21,NST,4,2), OIT(21,NST,4,2),
     &                 TMP(21,NST,2), NTMP(21,NST,2), ITERR(4)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /MANSND/  OM(21,NST,2), TDM(21), QM(21), MANDL, MANDF,
     &                 ZFM(21),TFM(21),QFM(21), PM(21)
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          PRNT
      INTEGER IND(5), ICK

      DO IS=1,NOBS
      DO IV=1,2
        DO L=1,MAND
          DO IT=1,4
            IF(ITERR(IT).NE.0
     &        .OR.OBT(L,IS,IT,IV).EQ.BMISS) THEN
              IND(IT) = 0
            ELSE
              IND(IT) = 1
            ENDIF
          ENDDO
          IF(OM(L,IS,IV).EQ.BMISS) THEN
            IND(5) = 0
          ELSE
            IND(5) = 1
          ENDIF
          IF(IV.EQ.2.AND.L.LE.2
     &      .AND.IND(1)+IND(5)+IND(4).EQ.3) THEN
            IM = 1
            IP = 4
            TMP(L,IS,IV) = (OM(L,IS,IV)
     &        -0.5*(OBT(L,IS,1,IV)+OBT(L,IS,4,IV)))
          ELSEIF(IND(2)+IND(5)+IND(3).EQ.3) THEN
            IM = 2
            IP = 3
            TMP(L,IS,IV) = OM(L,IS,IV)
     &        - 0.5*(OBT(L,IS,2,IV) + OBT(L,IS,3,IV))
          ELSEIF(IND(2)+IND(5)+IND(4).EQ.3) THEN
            IM = 2
            IP = 4
            TMP(L,IS,IV) = (OM(L,IS,IV)
     &        -(2./3.)*OBT(L,IS,2,IV)-(1./3.)*OBT(L,IS,4,IV))
          ELSEIF(IND(1)+IND(5)+IND(3).EQ.3) THEN
            IM = 1
            IP = 3
            TMP(L,IS,IV) = (OM(L,IS,IV)
     &        -(1./3.)*OBT(L,IS,1,IV)-(2./3.)*OBT(L,IS,3,IV))
          ELSEIF(IND(1)+IND(5)+IND(4).EQ.3) THEN
            IM = 1
            IP = 4
            TMP(L,IS,IV) = (OM(L,IS,IV)
     &        -0.5*(OBT(L,IS,1,IV)+OBT(L,IS,4,IV)))
          ELSE
            TMP(L,IS,IV) = BMISS
          ENDIF

C           TEMPORAL CHECK FLAGS.
C
          IF(TMP(L,IS,IV).EQ.BMISS) THEN
            ITI = 0
          ELSE
            ITI = 20.*ABS(TMP(L,IS,IV))
     &            /(TMPSTD(L,IV)*TFACT(L,IV))
            ITI = MIN(ITI,20)
            IF(ITI.NE.0) THEN
              CALL CHKTMP(OIT(L,IS,IM,IV),OIT(L,IS,IP,IV),
     &             TMPSTD(L,IV)*TFACT(L,IV),ICK)
              IF(ICK.NE.0) THEN
                ITI = 0
                TMP(L,IS,IV) = BMISS
              ENDIF
            ENDIF
          ENDIF
          NTMP(L,IS,IV) = ITI
        ENDDO
        IF(IS.LE.10) THEN
          WRITE(60,500) IS,IV,(NTMP(L,IS,IV),L=1,15),
     &      (TMP(L,IS,IV),L=1,15)
  500     FORMAT(' TMPCHK--IS,IV,NTMP-S: ',2I5,2X,15I4,/,
     &           '                TMP-S: ',15F12.1)
        ENDIF
      ENDDO
      IF(IS.LE.10) THEN
        WRITE(60,501) (OBT(5,IS,1,IV),OBT(5,IS,2,IV),OM(5,IS,IV),
     &    OBT(5,IS,3,IV),OBT(5,IS,4,IV),IV=1,2)
  501   FORMAT('               Z500-S: ',5F12.1,/,
     &           '               T500-S: ',5F12.1)
      ENDIF
      ENDDO
      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    VOI         PERFORM VERTICAL OI CHECK
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Perform vertical statistical interpolation (OI) check.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 1997-04-28  W. Collins  Exclude use of data that were vertically
C     interpolated in PREPOBS_PREPDATA.
C
C USAGE:    CALL VOI(ITIME)
C   INPUT ARGUMENT LIST:
C     ITIME    - 1 - FIRST CALL TO INPUT FOR STATION
C                2 - SECOND CALL TO INPUT FOR STATION
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE VOI(ITIME)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      REAL             VI(255,4), VR(255,4), CC(4)
      INTEGER          LEV(4), IL(255,4)
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /STATS/   NV(21,7), AVGV(21,7), STDV(21,7),
     &                 NH(21,7), AVGH(21,7), STDH(21,7),
     &                 NI(21,7), AVGI(21,7), STDI(21,7),
     &                 NT(21,4),  AVGT(21,4), STDT(21,4),
     &                 NHY(21),   AVGHY(21),  STDHY(21),
     &                 NB,        AVGB,       STDB,
     &                 SKV(21,7), XKV(21,7),
     &                 SKH(21,7), XKH(21,7),
     &                 SKI(21,7), XKI(21,7),
     &                 SKT(21,4), XKT(21,4),
     &                 SKHY(21),  XKHY(21),
     &                 SKB,       XKB
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /STN/     IS
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          PRNT, START
      LOGICAL ZG, TG, TDG, QG, ERROR, RCP, RCT, RCZ, RCATT, RCATZ
      DATA CC /1.1,8.,1.,1./, GAMA /0.5/, POW /1.2/
      DATA START /.TRUE./
      COR(P1,P2,CCC) = 1./(1.+CCC*(ABS(ALOG(P1/P2)))**POW)

      PGAM = 1. + GAMA
      PGAMS = PGAM**2

      IF(START) THEN
        XZVM = BMISS
        XTVM = BMISS
        XTDVM = BMISS
        START = .FALSE.
      ENDIF
      DO I=1,255
        ZV(I)   = BMISS
        TV(I)   = BMISS
        TDV(I)  = BMISS
        QV(I)   = BMISS
        DO J=1,4
          VI(I,J)   = BMISS
          VR(I,J)   = BMISS
        ENDDO
        NZV(I)  = -1
        NTV(I)  = -1
        NTDV(I) = -1
        NQV(I)  = -1
        IF(ITIME.EQ.2) THEN
          XZV(I)  = BMISS
          XTV(I)  = BMISS
          XTDV(I) = BMISS
          XQV(I)  = BMISS
        ENDIF
      ENDDO
      IF(NLEV.LT.2) RETURN

C  PUT NON-MISSING INCREMENTS INTO LOCAL ARRAY FOR
C  CAT = 1,2,5 LEVELS ONLY
C  -----------------------------------------------

      DO J=1,4
        DO I=1,255
          IL(I,J)  = 0
        ENDDO
      ENDDO
      LZ  = 0
      LT  = 0
      LTD = 0
      LQ  = 0
      DO L=1,NLEV
C       RCT = TRC(L).EQ.101. .OR. TRC(L).EQ.102. .OR. TRC(L).EQ.103.
C       RCZ = ZRC(L).EQ.101. .OR. ZRC(L).EQ.103.
        RCATT = .FALSE.
        RCATZ = .FALSE.
        RCATZ = RCATZ .OR. CAT(L).EQ.1
        RCATT = RCATT .OR. CAT(L).EQ.1 .OR. CAT(L).EQ.2 .OR. CAT(L).EQ.5
        IF(ZI(L).LT.BMISS .AND. RCATZ) THEN
          LZ = LZ+1
          VI(LZ,1) = ZI(L)
          IL(LZ,1) = L
        ENDIF
        IF(TI(L).LT.BMISS .AND. RCATT) THEN
          LT = LT+1
          VI(LT,2) = TI(L)
          IL(LT,2) = L
        ENDIF
        IF(TDI(L).LT.BMISS .AND. RCATT) THEN
          LTD = LTD+1
          VI(LTD,3) = TDI(L)
          IL(LTD,3) = L
        ENDIF
        IF(QI(L).LT.BMISS .AND. RCATT) THEN
          LQ = LQ+1
          VI(LQ,4) = QI(L)
          IL(LQ,4) = L
        ENDIF
      ENDDO

      LEV(1) = LZ
      LEV(2) = LT
      LEV(3) = LTD
      LEV(4) = LQ

      DO IV=1,4
        CCC = CC(IV)

C  BOTTOM LEVEL (SURFACE)
C  ----------------------

	IF(IL(1,IV).EQ.0 .OR. IL(2,IV).EQ.0) GOTO 100
        PR1 = POB(IL(1,IV))
        PR2 = POB(IL(2,IV))
        H1  = COR(PR1,PR2,CCC)/PGAM
        VR(1,IV) = VI(1,IV) - H1*VI(2,IV)

C  TOP LEVEL
C  ---------

        IF(IL(LEV(IV),IV).EQ.0 .OR. IL(LEV(IV)-1,IV).EQ.0) GOTO 100
        PR1 = POB(IL(LEV(IV),IV))
        PR2 = POB(IL(LEV(IV)-1,IV))
        H1  = COR(PR1,PR2,CCC)/PGAM
        VR(LEV(IV),IV) = VI(LEV(IV),IV) - H1*VI(LEV(IV)-1,IV)

C  NOW OTHER LEVELS (This version does not check the distance
C                    to the neighboring levels.)
C  ---------------------------------------------

   50   CONTINUE
        IF(LEV(IV).LT.3) GOTO 100
        DO L=2,LEV(IV)-1
          IF(IL(L,IV).EQ.0 .OR.IL(L-1,IV).EQ.0 .OR.
     &       IL(L+1,IV).EQ.0) GOTO 60
          PR0 = POB(IL(L,IV))
          PR1 = POB(IL(L-1,IV))
          PR2 = POB(IL(L+1,IV))
          C01 = COR(PR0,PR1,CCC)
          C02 = COR(PR0,PR2,CCC)
          C12 = COR(PR1,PR2,CCC)
          H1  = (PGAM*C01 - C02*C12) / (PGAMS - C12**2)
          H2  = (PGAM*C02 - C01*C12) / (PGAMS - C12**2)
          VR(L,IV) = VI(L,IV) - H1*VI(L-1,IV) - H2*VI(L+1,IV)
   60     CONTINUE
        ENDDO
  100   CONTINUE
      ENDDO

C  PLACE VERTICAL RESIDUALS INTO PROPER COMMON ARRAYS
C  AND SOLVE FOR NORMALIZED VALUES.
C  --------------------------------------------------

      DO L=1,LEV(1)
        I = NMANLV(POB(IL(L,1)))
        II = MANLEV(POB(IL(L,1)))
        IF(VI(L,1).LT.BMISS) THEN
          IF(I.NE.0) THEN
            ZV(IL(L,1)) = VR(L,1)
            NZV(IL(L,1)) = MIN1(20.,20.*ABS(VR(L,1))/VOIRES(I,1))
            IF(ITIME.EQ.2) THEN
              XZV(IL(L,1)) = (VR(L,1)-AVGV(I,1))
     &                       /MAX(.1*VOIRES(I,1),STDV(I,1))
            ENDIF
          ENDIF
          IF(II.NE.0) THEN
            ZVM(II,IS) = VR(L,1)
            XZVM(II,IS) = XZV(IL(L,1))
          ENDIF
        ENDIF
      ENDDO
      DO L=1,LEV(2)
        I = NMANLV(POB(IL(L,2)))
        II = MANLEV(POB(IL(L,2)))
        IF(VI(L,2).LT.BMISS) THEN
          IF(I.NE.0) THEN
            TV(IL(L,2)) = VR(L,2)
            NTV(IL(L,2)) = MIN1(20.,20.*ABS(VR(L,2))/VOIRES(I,2))
            IF(ITIME.EQ.2) THEN
              XTV(IL(L,2)) = (VR(L,2)-AVGV(I,2))
     &                       /MAX(.1*VOIRES(I,2),STDV(I,2))
            ENDIF
          ENDIF
          IF(II.NE.0) THEN
            TVM(II,IS) = VR(L,2)
            XTVM(II,IS) = XTV(IL(L,2))
          ENDIF
        ENDIF
      ENDDO
      DO L=1,LEV(3)
        I = NMANLV(POB(IL(L,3)))
        II = MANLEV(POB(IL(L,3)))
        IF(VI(L,3).LT.BMISS) THEN
          IF(I.NE.0) THEN
            TDV(IL(L,3)) = VR(L,3)
            NTDV(IL(L,3)) = MIN1(20.,20.*ABS(VR(L,3))/VOIRES(I,3))
            IF(ITIME.EQ.2) THEN
              XTDV(IL(L,3)) = (VR(L,3)-AVGV(I,3))
     &                        /MAX(.1*VOIRES(I,3),STDV(I,3))
            ENDIF
          ENDIF
          IF(II.NE.0) THEN
            TDVM(II,IS) = VR(L,3)
            XTDVM(II,IS) = XTDV(IL(L,3))
          ENDIF
        ENDIF
      ENDDO
      DO L=1,LEV(4)
        I = NMANLV(POB(IL(L,4)))
        II = MANLEV(POB(IL(L,4)))
        IF(VI(L,4).LT.BMISS .AND. I.NE.0) THEN
          IF(I.NE.0) THEN
            QV(IL(L,4)) = VR(L,4)
            NQV(IL(L,4)) = MIN1(20.,20.*ABS(VR(L,4))/VOIRES(I,4))
            IF(ITIME.EQ.2) THEN
              XQV(IL(L,4)) = (VR(L,4)-AVGV(I,4))
     &                       /MAX(.1*VOIRES(I,4),STDV(I,4))
            ENDIF
          ENDIF
          IF(II.NE.0) THEN
            QVM(II,IS) = VR(L,4)
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: VSOLVE
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1990-11-06
C
C ABSTRACT: CHOLESKY TYPE SOLUTION FOR ARRAYS OF POSITIVE DEFINITE
C   SYMMETRIC MATRIXES.
C
C PROGRAM HISTORY LOG:
C 1990-11-06  J. Woollen  Original author.
C
C USAGE:    CALL VSOLVE (A,B,NDIM,BAD,NFT,NS,MAXDIM)
C   INPUT ARGUMENTS:
C     A          - ARRAY OF SYMMETRIC MATRIXES
C     B          - ARRAY OF RIGHT HAND SIDE VECTORS
C     NDIM       - ARRAY OF MATRIX RANKS
C     BAD        - BAD MATRIX INDICATOR
C     NFT        - NUMBER OF RIGHT HAND SIDES PER MATRIX
C     NS         - NUMBER OF MATRIXES TO SOLVE
C     MAXDIM     - LARGEST RANK MATRIX IN STACK
C
C   OUTPUT ARGUMENTS:
C     B          - CONTAINS THE SOLUTIONS UPON RETURN
C
C REMARKS: TRIANGULAR REPRESENTATION LISTS ELEMENTS TOWARDS THE
C   DIAGONAL. ALGORITHM FROM H.CARUS.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
C-----------------------------------------------------------------------

      SUBROUTINE VSOLVE (A,B,NDIM,BAD,NFT,NS,MAXDIM)


      DIMENSION A(1000,10),B(1000,4,1),NDIM(1000),BAD(1000)
      LOGICAL BAD

      DIMENSION T(1000)

      DATA CNUM/1.E-15/

C----------------------------------------------------------------------
      IX (I,J) = I*(I-1)/2 + J
C----------------------------------------------------------------------

      N = MAXDIM

      DO M=1,NS
        BAD(M) = .FALSE.
      ENDDO

C  DECOMPOSE THE MATRIXES
C  ----------------------

      DO I=1,N
        DO J=1,I

          DO M=1,NS
            T(M) = A(M,IX(I,J))
          ENDDO

          DO K=1,J-1
            DO M=1,NS
              T(M) = T(M) - A(M,IX(I,K)) * A(M,IX(J,K))
            ENDDO
          ENDDO

          IF(I.GT.J) THEN
            DO M=1,NS
              A(M,IX(I,J)) = T(M) * A(M,IX(J,J))
            ENDDO
          ELSE
            DO M=1,NS
              IF(T(M).LT.CNUM .AND. NDIM(M).GE.I) BAD(M) = .TRUE.
              IF(T(M).LE.0) T(M) = 1.
            ENDDO

            DO M=1,NS
              A(M,IX(I,I)) = 1./SQRT(T(M))
            ENDDO
          ENDIF

        ENDDO
      ENDDO

C  SOLVE FOR ALL RIGHT HAND SIDES
C  ------------------------------

      DO NF=1,NFT

C  FORWARD SUBSTITUTION
C  --------------------

        DO I=1,N
          DO M=1,NS
            T(M) = B(M,I,NF)
          ENDDO
          DO J=1,I-1
            DO M=1,NS
              T(M) = T(M) - A(M,IX(I,J)) * B(M,J,NF)
            ENDDO
          ENDDO
          DO M=1,NS
            B(M,I,NF) = T(M) * A(M,IX(I,I))
          ENDDO
        ENDDO

C  BACKWARD SUBSTITUTION
C  ---------------------

        DO I=N,1,-1
          DO M=1,NS
            T(M) = B(M,I,NF)
          ENDDO
          IF(I.NE.N) THEN
            DO J=I+1,N
              DO M=1,NS
                T(M) = T(M) - A(M,IX(J,I)) * B(M,J,NF)
              ENDDO
            ENDDO
          ENDIF
          DO M=1,NS
            B(M,I,NF) = T(M) * A(M,IX(I,I))
          ENDDO
        ENDDO

      ENDDO

      RETURN
      END

C****************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    VTPENV      CALCULATE VIRTUAL TEMPERATURE
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2007-09-14
C
C ABSTRACT: CREATES VIRTUAL TEMPERATURE EVENTS.  THIS CONSISTS OF
C   FIRST RE-CALCULATING THE SPECIFIC HUMIDITY FROM THE FINAL UPDATED
C   DEWPOINT TEMPERATURE (CALCULATED FROM THE FINAL UPDATED SENSIBLE
C   TEMPERATURE AND THE ORIGINAL OBSERVED DEWPOINT DEPRESSION) AND
C   PRESSURE, FOLLOWED BY THE CALCULATION OF VIRTUAL TEMPERATURE FROM
C   THE JUST-CALCULATED SPECIFIC HUMIDITY AND THE FINAL UPDATED
C   SENSIBLE TEMPERATURE.  THE RE-CALCULATED SPECIFIC HUMIDITY IS
C   THEN ENCODED AS A STACKED EVENT TO BE LATER WRITTEN INTO THE
C   PREPBUFR FILE (UNDER PROGRAM "VIRTMP", REASON CODE 1).  IF THE
C   NAMELIST SWITCH DOVTMP IS TRUE, THEN THE JUST-CALCULATED VIRTUAL
C   TEMPERATURE IS THEN ALSO ENCODED AS A STACKED EVENT TO BE LATER
C   WRITTEN INTO THE PREPBUFR FILE (UNDER PROGRAM "VIRTMP", REASON
C   CODE EITHER 1 OR 7).  THERE ARE CERTAIN CASES WHEN THE VIRTUAL
C   TEMPERATURE IS NOT CALUCLATED (SEE COMMENTS IN SUBROUTINE).
C
C PROGRAM HISTORY LOG:
C 1994-MM-DD  J. Woollen  Original author.
C 1997-08-27  W. Collins  Add direct calculation from q.  Add DOVTMP
C     switch.
C 1999-12-09  W. Collins  Change VTPEVN so that Tv (virtual
C     temperature) is calculated, even when the moisture quality is >
C     3, except when the moisture exceeds 'Earth' limits.
C 2001-02-28  W. Collins  Correct error of setting dew-point
C     temperature to missing in FULVAL. Never flag T (Tv) bad as the
C     result of flagging q bad. Make more efficient so that auxiliary
C     level computations are ommitted when not needed.
C 2001-04-27  D. Keyser   Streamlined code. Turned q event processing
C     back on (even when DOVTMP=F). When q bad but passes sanity check,
C     Tv now calculated in all cases but given a suspect q.m. if below
C     700 mb and original TQM not 0 or > 3 (this was original intention
C     of 1999-12-09 change which actually skipped Tv processing in all
C     cases when q was bad).
C 2004-04-21  D. Keyser   For cases when DOVTMP=T, no longer calculates
C     Tv on pressure levels above the tropopause because reported Tdd
C     values at high levels are often inaccurate and unrealistic 0.4 to
C     1.0 increases in Tv vs. T can occur (esp. above 10 mb and at
C     U.S. sites) (q is still updated on these levels, however), if the
C     reported tropopause level is below 500 mb it is not considered
C     to be the tropopause for this test, also if no valid tropopause
C     level is found by the time 80 mb is reached, all levels above
C     this are considered to be above the tropopause for this test
C     {thanks to Christopher Redder (NASA/GSFC) for discovering this
C     problem}
C 2007-09-14  D. Keyser -- Q.M. 3 is now assigned to calculated virt.
C     temps only if the moisture Q.M. is truly bad (i.e. > 3 but not 9
C     or 15) (and, as before, orig. TQM is 1 or 2 and POB is below 700
C     mb) - before, TQM set to 3 when QQM was 9 or 15 and all other
C     conditions met
C
C USAGE:    CALL VTPEVN(DOVTMP)
C
C INPUT ARGUMENTS:
C           DOVTMP - LOGICAL ARGUMENT, TRUE IF VIRTUAL TEMPERATURE IS
C                    CALCULATED
C
C   OUTPUT FILES:
C     UNIT 60  - PRINT FILE, DETAILS OF DECISIONS
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE VTPEVN(DOVTMP)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      CHARACTER*8      SID
      CHARACTER*40     TEVN, QEVN
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /STN/     IS
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /EVNSND/ PO (255),TO (255),ZO (255),CA (255),
     &                PQ (255),TQ (255),ZQ (255),INP(255),
     &                PR (255),TR (255),ZR (255),INZ(255),
     &                QO (255),QQ (255),QR (255),INT(255),
     &                TDE(255),TDQ(255),TDR(255),INQ(255),
     &                UO(255), VO(255), SPO(255), DIRO(255),
     &                WQ(255), WR(255)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST, DOVTMP, PRNT, TROP

      DATA TEVN   /'TOB TQM TPC TRC                         '/
      DATA QEVN   /'QOB QQM QPC QRC                         '/

C-----------------------------------------------------------------------
C FCNS BELOW CONVERT TEMP/TD (K) & PRESS (MB) INTO SAT./ SPEC. HUM.(G/G)
C-----------------------------------------------------------------------
      ES(T) = 6.1078*EXP((17.269*(T - 273.15))/(T - 273.15 + 237.3))
      QS(T,P) = (0.622*ES(T))/(P-0.378*ES(T))
C-----------------------------------------------------------------------

C  CLEAR TEMPERATURE AND Q EVENTS
C  ------------------------------

      TO   = BMISS
      TQ   = BMISS
      TR   = BMISS
      QO   = BMISS
      QQ   = BMISS
      QR   = BMISS
      INT  = BMISS
      INQ  = BMISS
      IEVT = 0
      IEVQ = 0
      TROP = .FALSE.

C  COMPUTE VIRTUAL TEMPERATURES AND SPECIFIC HUMIDITIES USING FINAL
C   UPDATED TEMPERATURE AND ORIGINAL OBSERVED DEWPOINT DEPRESSION
C  ----------------------------------------------------------------

      DO L=1,NLV
         IF(POB(L).LT.BMISS .AND. TOB(L).LT.BMISS
     $                      .AND. TDO(L).LT.BMISS) THEN
            TD = TOB(L)-TDO(L) ! TD is dewpt (C), TDO is depression
            IF(QQM(L).GT.3) THEN
C  Don't update q or calculate Tv if bad moisture obs fails sanity check
               IF(TD.LT.-103.15 .OR. TD.GT.46.83 .OR. POB(L).LT.0.1 .OR.
     $          POB(L).GT.1100.)  CYCLE
            ENDIF
            Q = QS(TD+273.15,POB(L))
            QO(IEVQ+1) = Q*1E6
            QQ(IEVQ+1) = QQM(L) ! Moist qm same as before for re-calc. q
            QR(IEVQ+1) = 1      ! Re-calc. q gets unique reason code 1
C  Test this level to see if at or above trop (trop must be above
C   500 mb to pass test; if no trop level found assume it's at 80 mb)
C  Don't calculate Tv on this level if at or above trop (doesn't
C   affect q calculation)
            TROP = ((CAT(L).EQ.5 .AND. POB(L).LT.500.) .OR.
     $       POB(L).LT. 80. .OR. TROP)
            IF(DOVTMP .AND. .NOT.TROP) THEN
               TO(IEVT+1) = (TOB(L)+273.15) * (1.+.61*Q) - 273.15
               IF((QQM(L).LT.4.OR.QQM(L).EQ.9.OR.QQM(L).EQ.15)
     $          .OR. TQM(L).EQ.0 .OR. TQM(L).GT.3
     $          .OR. POB(L).LE.700.) THEN
                  TQ(IEVT+1) = TQM(L) ! Tv qm same as for T when q is ok
                                      ! or q flagged by PREPRO (but not
                                      ! bad)
                  TR(IEVT+1) = 1      ! Tv gets unique reason code 1
               ELSE
                  TQ(IEVT+1) = 3 ! Tv qm susp for bad moist below 700 mb
                  TR(IEVT+1) = 7 ! Tv gets unique reason code 7
               ENDIF
               INT(IEVT+1) = L
               IEVT = IEVT+1
            ENDIF
            INQ(IEVQ+1) = L
            IEVQ = IEVQ+1
         ENDIF
      ENDDO

      IF(DOVTMP) THEN
        CALL EVENT(NFOUT,TEVN,NLV,TO,TQ,TR,INT,IEVT,VTPPC)
        WRITE(60,510) IEVT,SID(IS)
  510   FORMAT(' VTPEVN--',I3,' TEMPERATURE EVENTS WRITTEN FOR:',A8)
      ENDIF
      CALL EVENT(NFOUT,QEVN,NLV,QO,QQ,QR,INQ,IEVQ,VTPPC)
      WRITE(60,511) IEVQ,SID(IS)
  511 FORMAT(' VTPEVN--',I3,' MOISTURE EVENTS WRITTEN FOR:',A8)

      RETURN
      END

C**********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    WBLOCKS     WRITE OUTPUT FOR 'SINGLE' INPUT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT:  Write all data necessary for running single-station
C   version of code to unit NFSGL.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL WBLOCKS
C   OUTPUT FILES:
C     UNIT NFSGL - UNIT NUMBER OF FILE CONTAINING ALL DATA NECESSARY
C                  FOR SINGLE-STATION VERSION
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE WBLOCKS
      PARAMETER (NST=1500)

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      COMMON /STN/     IS
      COMMON /DATEX/   JDATE(5), CDATE
      CHARACTER*8      SID
      CHARACTER*10     CDATE
      COMMON /MANSND/  ZM(21,NST), TM(21,NST), TDM(21), QM(21), MANDL,
     &                 MANDF, ZFM(21), TFM(21), QFM(21), PM(21)
      COMMON /TESTS/   TEST
      LOGICAL          TEST
      REAL             P(255,23)
      INTEGER          IP(255,3)
      DATA BMS /99999.99/, QBMS /9.999999/

C  WRITE CONTENTS OF COMMON BLOCKS TO UNIT NFSGL, FORMATTED FOR EDITING
C  --------------------------------------------------------------------

      IF(TEST) WRITE(NFSGL,500) CDATE,(JDATE(I),I=1,4)
  500 FORMAT(2X,A10,2X,4I2)
      IF(TEST) WRITE(NFSGL,501) SID(IS), DHR(IS), XOB(IS), YOB(IS),
     &                 ELV(IS), SQN(IS), ITP(IS), NLV,
     &                 NEV, ISF(IS), NLVM, NLVW
  501 FORMAT(2X,A8,4F10.2,F10.0,I10.3,5I10)
      IF(TEST) WRITE(NFSGL,503) MANDL,MANDF,PS(IS),GESPS(IS)
  503 FORMAT(2I10,2F10.2)

      IBMS = BMS
      DO L=1,NLV
        P(L,1) = AMIN1(POB(L),BMS)
        P(L,2) = AMIN1(ZOB(L),BMS)
        P(L,3) = AMIN1(TOB(L),BMS)
        P(L,4) = AMIN1(TDO(L),BMS)
        P(L,5) = AMIN1(QOB(L),QBMS)
        P(L,6) = AMIN1(TVO(L),BMS)
        P(L,7) = AMIN1(PFC(L),BMS)
        P(L,8) = AMIN1(ZFC(L),BMS)
        P(L,9) = AMIN1(TFC(L),BMS)
        P(L,10) = AMIN1(TDFC(L),BMS)
        P(L,11) = AMIN1(QFC(L),QBMS)
        P(L,12) = AMIN1(PQM(L),BMS)
        P(L,13) = AMIN1(ZQM(L),BMS)
        P(L,14) = AMIN1(TQM(L),BMS)
        P(L,15) = AMIN1(QQM(L),BMS)
        P(L,16) = AMIN1(PRC(L),BMS)
        P(L,17) = AMIN1(ZRC(L),BMS)
        P(L,18) = AMIN1(TRC(L),BMS)
        P(L,19) = AMIN1(QRC(L),BMS)
        P(L,20) = AMIN1(CAT(L),BMS)
        P(L,21) = AMIN1(HRDR(L),BMS)
        P(L,22) = AMIN1(YDR(L),BMS)
        P(L,23) = AMIN1(XDR(L),BMS)
        IP(L,1) = MIN0(IND(L),IBMS)
        IP(L,2) = MIN0(LEVTYP(L),IBMS)
        IP(L,3) = MIN0(LST(L),IBMS)
      ENDDO

      DO L=1,NLV
        IF(TEST) WRITE(NFSGL,502) (P(L,I),I=1,23),(IP(L,J),J=1,3)
      ENDDO
  502 FORMAT(4F10.2,F10.6,5F10.2,F10.6,9F10.2,3F10.4,3I10)

      RETURN
      END

C****************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    WINDATZ     FIND PRESSURE AT HEIGHTS FOR CATEGORY 4
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: RESOLVE FOR THE PRESSURE AT THE HEIGHTS
C   OF WINDS-BY-HEIGHT, CATEGORY 4.  THERE WILL BE EXISTING PRESSURES,
C   PROVIDED BY PREPOBS_PREPDATA.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL WINDATZ(SAME)
C   INPUT ARGUMENT LIST:
C     SAME     - TRUE WHEN WIND PART GOES WITH PREVIOUSLY READ MASS
C                PART
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE WINDATZ(SAME)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      REAL            PA(510), TA(510), ZA(510), QA(510), CATA(510)
      INTEGER         INDX(510)
      CHARACTER*8 SID
      LOGICAL         SAME

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /ALSNDW/  POBW(255), PFCW(255), PQMW(255), PRCW(255),
     &                 ZOBW(255), ZFCW(255), ZQMW(255), ZRCW(255),
     &                 UOB(255),  VOB(255),  UFC(255),  VFC(255),
     &                 WQM(255),  WRC(255),  SP(255),   DIR(255),
     &                 CATW(255)
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      DATA T0 /273.15/

C  THE PURPOSE OF THIS SUBROUTINE IS TO RESOLVE FOR THE PRESSURE AT
C  THE HEIGHTS OF WINDS-BY-HEIGHT, CATEGORY 4.  THERE WILL BE
C  EXISTING PRESSURES, PROVIDED BY PREPOBS_PREPDATA.
C  ----------------------------------------------------------------

      IF(.NOT. SAME) RETURN

C  SET THE COMBINED FIELDS TO MISSING
C  ----------------------------------

      PA   = BMISS
      TA   = BMISS
      ZA   = BMISS
      QA   = BMISS
      CATA = BMISS

C  MAKE A COMBINED LIST OF PRESSURES
C  ---------------------------------

      DO L=1,NLVM
        PA(L) = POB(L)
      ENDDO

      DO L=1,NLVW
        PA(L+NLVM) = POBW(L)
      ENDDO
      NLVA = NLVM + NLVW

C  LOOK FOR DUPLICATE PRESSURES AND SET ANY TO MISSING
C  ---------------------------------------------------

      DO L=1,NLVA
        DO LL=L+1,NLVA
          IF(PA(L).EQ.PA(LL)) PA(LL) = BMISS
        ENDDO
      ENDDO

C  SORT PRESSURES, DESCENDING
C  --------------------------

      CALL SHELL(PA,INDX,NLVA,1)

C  DETERMINE THE NUMBER OF NON-MISSING PRESSURES, RESET NLVA
C  ---------------------------------------------------------

      DO L=1,NLVA
        LL = L
        IF(PA(L).NE.BMISS) GOTO 10
      ENDDO
   10 CONTINUE
      DO L=LL,NLVA
        PA(L-LL+1) = PA(LL)
      ENDDO
      NLVA = NLVA-LL+1
      IF(NLVA.LE.2) RETURN

C  FILL TA,ZA,QA,CATA.
C  -------------------------

      DO L=1,NLVM
        DO LL=1,NLVA
          IF(POB(L).EQ.PA(LL)) THEN
            IF(TOB(L).NE.BMISS) TA(LL)   = TOB(L)
            IF(ZOB(L).NE.BMISS) ZA(LL)   = ZOB(L)
            IF(QOB(L).NE.BMISS) QA(LL)   = QOB(L)
            IF(CAT(L).NE.BMISS) CATA(LL) = CAT(L)
          ENDIF
        ENDDO
      ENDDO

      DO L=1,NLVW
        DO LL=1,NLVA
          IF(POBW(L).EQ.PA(LL)) THEN
            IF(ZOBW(L).NE.BMISS) ZA(LL)   = ZOBW(L)
            IF(CATW(L).NE.BMISS) CATA(LL) = CATW(L)
          ENDIF
        ENDDO
      ENDDO

C  GO THROUGH LIST, LOOKING FOR CATA=4 LEVELS.  AT EACH, FIND
C  SURROUNDING PRESSURES, IF THEY EXIST.  SOLVE HYDORSTATICALLY
C  FOR THE PRESSURE OF THIS LEVEL AND REPLACE PA WITH THIS
C  VALUE.  (USE VIRTUAL TEMPERATURES, IF POSSIBLE.)
C  ------------------------------------------------------------

      IF(CATA(1).EQ.4.) THEN
        P1    = PA(2)
        Z1    = ZA(2)
        IF(QA(2).NE.BMISS) THEN
          T1C = (TA(2)+T0)*(1.+.61*QA(2)) - T0
        ELSE
          T1C = TA(2)
        ENDIF
        Z2    = ZA(1)
        P3    = BMISS
        Z3    = BMISS
        T3C   = BMISS
        ALPHA = -.0065
        METH = 2
        CALL PATZ(P1,P2,P3,Z1,Z2,Z3,T1C,T3C,ALPHA,METH)
        PA(1) = P2
      ENDIF

      DO L=2,NLVA-1
        IF(CATA(L).EQ.4.) THEN
        DO LL=L-1,1,-1
          L1 = LL
          IF(CATA(LL).NE.4) GOTO 11
        ENDDO
   11   CONTINUE
          P1    = PA(L1)
          Z1    = ZA(L1)
          IF(QA(L1).NE.BMISS) THEN
            T1C = (TA(L1)+T0)*(1.+.61*QA(L1)) - T0
          ELSE
            T1C = TA(L1)
          ENDIF
          Z2    = ZA(L)
          DO LL=L+1,NLVA
            L2 = LL
            IF(CATA(LL).NE.4) GOTO 12
          ENDDO
          P3    = BMISS
          Z3    = BMISS
   12     CONTINUE
          P3    = PA(L2)
          Z3    = ZA(L2)
          IF(QA(L2).NE.BMISS) THEN
            T3C = (TA(L2)+T0)*(1.+.61*QA(L2)) - T0
          ELSE
            T3C = TA(L2)
          ENDIF
          ALPHA = BMISS
          METH = 1
          CALL PATZ(P1,P2,P3,Z1,Z2,Z3,T1C,T3C,ALPHA,METH)
          PA(L) = P2
        ENDIF
      ENDDO

      IF(CATA(NLVA).EQ.4.) THEN
        P1    = PA(NLVA-1)
        Z1    = ZA(NLVA-1)
        IF(QA(NLVA-1).NE.BMISS) THEN
          TIC = (TA(NLVA-1)+T0)*(1.+.61*QA(NLVA-1)) - T0
        ELSE
          T1C = TA(NLVA-1)
        ENDIF
        Z2    = ZA(NLVA)
        P3    = BMISS
        Z3    = BMISS
        T3C   = BMISS
        ALPHA = -.0065
        METH = 2
        CALL PATZ(P1,P2,P3,Z1,Z2,Z3,T1C,T3C,ALPHA,METH)
        PA(NLVA) = P2
      ENDIF

C  SET CAT=4 PRESSURES TO THOSE JUST SOLVED FOR
C  --------------------------------------------

      DO L=1,NLVM
        IF(CAT(L).EQ.4.) THEN
          DO LL=1,NLVA
            IF(ZA(LL).EQ.ZOB(L)) GOTO 40
          ENDDO
          GOTO 50
   40     CONTINUE
          POB(L) = PA(LL)
        ENDIF
   50   CONTINUE
      ENDDO

      DO L=1,NLVW
        IF(CATW(L).EQ.4.) THEN
          DO LL=1,NLVA
            IF(ZA(LL).EQ.ZOBW(L)) GOTO 20
          ENDDO
          GOTO 30
   20     CONTINUE
          POBW(L) = PA(LL)
        ENDIF
   30   CONTINUE
      ENDDO

      RETURN
      END

C************************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    WTISO       Write list of isolated observations
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: WRITE LIST OF ISOLATED OBSERVATIONS
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL WTISO
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE WTISO
      PARAMETER (NST=1500)
      COMMON /ISO/ IDISO(NST),NUM,ISISO
      LOGICAL ISISO
      WRITE(6,600)
      WRITE(6,601) (IDISO(I),I=1,NUM)
  600 FORMAT(' LIST OF ISOLATED STATIONS:')
  601 FORMAT(I12.5)
      RETURN
      END

C************************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    WTSTATS     WRITE SUMMARY OF RADCOR STATISTICS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2007-09-14
C
C ABSTRACT: WRITE SUMMARY OF RADCOR STATISTICS
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C 2007-09-14  D. Keyser   Corrected array overflow (and memory
C     clobbering) issue when the number of RAOB IDs stored by
C     radiosonde type in array SIDRAD (in RADCOR processing) is listed.
C     If this limit is exceeded, only the limiting number of RAOBS will
C     now be listed, along with a warning diagnostic - the limit has
C     been increased from 400 to 800 since some reanalysis runs were
C     exceeding the old limit.
C
C USAGE:    CALL WTSTATS
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 68  - INTERSONDE CORRECTION INFORMATION FILE
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE WTSTATS

      PARAMETER (NRID=800) ! max # of raob ids listed per inst. type
      CHARACTER*8 SIDNOR, SIDRAD
      COMMON/SWITCH/LWCORR,LEVRAD,IRCTBL,HGTTBL,BAL_DRIFT
      COMMON/COUNT/   NTYPE(69),KTYPE(69),SIDNOR(2000),SIDRAD(NRID,255),
     &                IICNT,JJCNT(255)
      LOGICAL HGTTBL

C  PRINT FINAL STATISTICS FOR INTERSONDE (RADIATION) CORRECTION
C  ------------------------------------------------------------

      IF(LEVRAD.GT.0)  THEN
         CALL LSTATS(1)
         CALL LSTATS(2)
      END IF
      PRINT 88
      WRITE(68,88)
   88 FORMAT(/38X,'THE FOLLOWING "ADPUPA" MASS REPORTS WERE NOT ',
     . 'CORRECTED'/)
      PRINT 89, (SIDNOR(I),I=1,IICNT)
      WRITE(68,89) (SIDNOR(I),I=1,IICNT)
   89 FORMAT(2X,9A8)

      PRINT 188
      WRITE(68,188)
  188 FORMAT(/13X,'THE FOLLOWING "ADPUPA" MASS REPORTS WERE SUBJECT TO',
     . ' CORRECTION (BASED ON SUN ANGLE AND PRESSURE LEVELS)'/)
      DO  J = 1,255
         IF(JJCNT(J).GT.0)  THEN
            PRINT 189, J
            WRITE(68,189) J
  189       FORMAT(/20X,'** RADIOSONDE TYPE ',I3.3,' **'/)
            JJCNT_MAX=MIN(JJCNT(J),NRID)
            PRINT 89, (SIDRAD(I,J),I=1,JJCNT_MAX)
            WRITE(68,89) (SIDRAD(I,J),I=1,JJCNT_MAX)
            IF(JJCNT_MAX.NE.JJCNT(J)) THEN
               PRINT'(" WARNING: THERE ARE TOTAL OF ",I0," REPORTS ",
     .          "WITH THIS INST. TYPE - CAN ONLY PRINT FIRST ",I0,
     .          " HERE DUE TO ARRAY SIZE LIMITATIONS")',JJCNT(J),NRID
               WRITE(68,'("WARNING: THERE ARE TOTAL OF ",I4," REPORTS ",
     .          "WITH THIS INST. TYPE - CAN ONLY PRINT FIRST ",I4,
     .          " HERE DUE TO ARRAY SIZE LIMITATIONS")') JJCNT(J),NRID
            END IF
         END IF
      END DO

      PRINT 90
      WRITE(68,90)
   90 FORMAT(///,49X,'"ADPUPA" MASS REPORT COUNTS BY RADIOSONDE TYPE',/)
      PRINT 92, (NTYPE(I), I= 1,17),(KTYPE(I), I= 1,17)
      WRITE(68,92) (NTYPE(I), I= 1,17),(KTYPE(I), I= 1,17)
      PRINT 92, (NTYPE(I), I=18,34),(KTYPE(I), I=18,34)
      WRITE(68,92) (NTYPE(I), I=18,34),(KTYPE(I), I=18,34)
      PRINT 92, (NTYPE(I), I=35,51),(KTYPE(I), I=35,51)
      WRITE(68,92) (NTYPE(I), I=35,51),(KTYPE(I), I=35,51)
      PRINT 92, (NTYPE(I), I=52,68),(KTYPE(I), I=52,68)
      WRITE(68,92) (NTYPE(I), I=52,68),(KTYPE(I), I=52,68)
   92 FORMAT(/,3X,17I7.3,/,3X,17I7)
      PRINT 93, KTYPE(69)
      WRITE(68,93) KTYPE(69)
   93 FORMAT(/,36X,'>>>>> TOTAL NUMBER OF "ADPUPA" MASS REPORTS =',I5,
     . ' <<<<<')
      PRINT 91
      WRITE(68,91)
   91 FORMAT(//,27X,'***** INTERSONDE (RADIATION) CORRECTION PROGRAM ',
     . ' SUCCESSFULLY COMPLETED *****',////)

      RETURN
      END

C*********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    XHORRES     CALCULATE NORMALIZED HORIZONTAL RESIDUAL
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: CALCULATE NORMALIZED HORIZONTAL RESIDUAL
C
C PROGRAM HISTORY LOG:
C 1999-04-13  W. Collins  Original author.
C
C USAGE:    CALL XHORRES
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE XHORRES
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /LIMS/    HSCRES(21), XINC(21,4), HOIRES(21,4),
     &                 VOIRES(21,4), TMPSTD(21,4), TFACT(21,4),
     &                 ZCLIM, TCLIM, NHLIM
      COMMON /MANRES/  ZIM(21,NST),TIM(21,NST),TDIM(21,NST),QIM(21,NST),
     &                 ZHM(21,NST),THM(21,NST),TDHM(21,NST),QHM(21,NST),
     &                 ZVM(21,NST),TVM(21,NST),TDVM(21,NST),QVM(21,NST),
     &                 NZIM(21,NST),NTIM(21,NST),NTDIM(21,NST),
     &                 XZIM(21,NST),XTIM(21,NST),XTDIM(21,NST),
     &                 XQIM(21,NST),XZVM(21,NST),XTVM(21,NST),
     &                 NZHM(21,NST),XZHM(21,NST),NTHM(21,NST),
     &                 XTHM(21,NST),XTDVM(21,NST),
     &                 NTDHM(21,NST),XTDHM(21,NST),NQHM(21,NST),
     &                 XQHM(21,NST),
     &                 ZG(21,NST), TG(21,NST), TDG(21,NST), QG(21,NST),
     &                 PIS(NST),   HYDM(21,NST),BRES(NST),  ERROR(NST)
      COMMON /STATS/   NV(21,7), AVGV(21,7), STDV(21,7),
     &                 NH(21,7), AVGH(21,7), STDH(21,7),
     &                 NI(21,7), AVGI(21,7), STDI(21,7),
     &                 NT(21,4),  AVGT(21,4), STDT(21,4),
     &                 NHY(21),   AVGHY(21),  STDHY(21),
     &                 NB,        AVGB,       STDB,
     &                 SKV(21,7), XKV(21,7),
     &                 SKH(21,7), XKH(21,7),
     &                 SKI(21,7), XKI(21,7),
     &                 SKT(21,4), XKT(21,4),
     &                 SKHY(21),  XKHY(21),
     &                 SKB,       XKB
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

C  CALCULATE NORMALIZED HORIZONTAL RESIDUALS.
C  ------------------------------------------

      DO J=1,NST
        DO I=1,21
          XZHM(I,J)  = BMISS
          XTHM(I,J)  = BMISS
          XTDHM(I,J) = BMISS
          XQHM(I,J)  = BMISS
        ENDDO
      ENDDO
      DO N=1,NOBS
        DO L=1,MAND
          IF(ZHM(L,N).LT.BMISS/2) THEN
            XZHM(L,N) = (ZHM(L,N)-AVGH(L,1))
     &                  /MAX(.1*HOIRES(L,1),STDH(L,1))
          ENDIF
          IF(THM(L,N).LT.BMISS/2) THEN
            XTHM(L,N) = (THM(L,N)-AVGH(L,2))
     &                  /MAX(.1*HOIRES(L,2),STDH(L,2))
          ENDIF
          IF(TDHM(L,N).LT.BMISS/2) THEN
            XTDHM(L,N) = (TDHM(L,N)-AVGH(L,3))
     &                   /MAX(.1*HOIRES(L,3),STDH(L,3))
          ENDIF
          IF(QHM(L,N).LT.BMISS/2) THEN
            XQHM(L,N) = (QHM(L,N)-AVGH(L,4))
     &                  /MAX(.1*HOIRES(L,4),STDH(L,4))
          ENDIF
        ENDDO
      ENDDO

      RETURN
      END

C******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ZDIF        CALCULATE HEIGHT DIFF FROM NEIGHBORS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Calculate height difference from nearest neighbors.
C
C PROGRAM HISTORY LOG:
C 1997-03-18  W. Collins  Original author.
C
C USAGE:    CALL ZDIF
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE ZDIF
      PARAMETER (NST=1500)

      REAL(8) BMISS

      REAL             VI(255,4), VR(255,4), CC(4)
      INTEGER          LEV(4), IL(255,4)
      COMMON /RESID/   PI(255), ZI(255), TI(255), TDI(255), QI(255),
     &                 TL(255), ZV(255), TV(255), TDV(255), QV(255),
     &                 NZI(255),NTI(255),NTDI(255),NQI(255),
     &                 XZI(255),XTI(255),XTDI(255),XQI(255),
     &                 NTL(255),NZV(255),NTV(255),NTDV(255),NQV(255),
     &                 XZV(255),XTV(255),XTDV(255),XQV(255),
     &                 HYDN(21),HYDQ(21),HYDS(21),HYD(21),  NHYD(21),
     &                 IHSC(4,255),BASRES(NST), PSINC, TLSAT(255),
     &                 B(21), NBAS, ZD(255),NHYDN(21),XZH(255),XTH(255),
     &                 XTDH(255)
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL ZG, TG, TDG, QG, ERROR, PRNT

      DO L=1,NLEV
        ZM = BMISS
        IF(L.EQ.1) GOTO 10
        DO I=L-1,1,-1
          IF(ZI(I).NE.BMISS) THEN
            ZM = ZI(I)
            GOTO 10
          ENDIF
        ENDDO
   10   CONTINUE

        Z0 = BMISS
        IF(ZI(L).NE.BMISS) Z0 = ZI(L)

        ZP = BMISS
        IF(L.EQ.NLEV) GOTO 20
        DO I=L+1,NLEV
          IF(ZI(I).NE.BMISS) THEN
            ZP = ZI(I)
            GOTO 20
          ENDIF
        ENDDO
   20   CONTINUE

        IF(Z0.NE.BMISS) THEN
          IF(ZM.NE.BMISS .AND. ZP.NE.BMISS) THEN
            ZD(L) = Z0 - 0.5*(ZM+ZP)
          ELSEIF(ZM.NE.BMISS .AND. ZP.EQ.BMISS) THEN
            ZD(L) = Z0 - ZM
          ELSEIF(ZP.NE.BMISS .AND. ZM.EQ.BMISS) THEN
            ZD(L) = Z0 - ZP
          ENDIF
        ELSE
          ZD(L) = ZI(L)
        ENDIF
      ENDDO

      RETURN
      END
