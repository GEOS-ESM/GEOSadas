C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: PREPOBS_PROFCQC
C   PRGMMR: DONG             ORG: NP22        DATE: 2020-01-09
C
C ABSTRACT: PERFORMS COMPLEX QUALITY CONTROL OF PROFILER DOPPLER
C   WINDS.  THE INPUT AND OUTPUT ARE IN PREPBUFR FORMAT.  THE
C   CHECKS USED ARE: INCREMENT, VERTICAL STATISTICAL, TEMPORAL
C   STATISTICAL, AND COMBINED VERTICAL-TEMPORAL.
C
C PROGRAM HISTORY LOG:
C 1996-11-20  W. Collins  I/O is completely new, but the algorithms
C     are identical to those already used in the RUC.
C 1997-05-27  W. Collins  24 hours maximum can be qc'd. The NAMELIST
C     reads in the hour limits with respect to the central data time.
C 1998-09-16  W. Collins  Replace faulty date routine with W3MOVDAT.
C 1998-12-22  W. Collins  Correct copy of reports to output.  Previous
C     implementation introduced skip of first block.
C 2000-12-04  W. Collins  Generalize heights of observations to allow
C     any set heights for each station. Honor pre-existing quality
C     marks.
C 2000-12-14  W. Collins  Modify to perform qc at all hours of data,
C     including first and last.  Also prints results for all hours.
C 2001-02-07  W. Collins  Correct error in DMA that did not flag all
C     bad data.  Logic change that allows some off-time data to be
C     qc'd.
C 2001-04-25  W. Collins  Modify to allow up to 143 levels of profiler
C     winds. (Previous limit was 43.)
C 2004-09-09  D. Keyser   Major changes: 1) To correct errors: Lvl
C     index in all arrays now site specific to allow proper temporal
C     checking at each individual site (each site examined over all
C     rpt times to generate hgt profile containing all possible lvls,
C     times w/ new lvls inserted get missing wind). Incr., temporal,
C     vert. & median chk error limits now directly assoc. w/ a rpted
C     hgt above ground lvl rather than w/ lvl index in arrays (ensures
C     proper error limit used at all lvls & eliminates hgt above ground
C     vs. hgt above sea lvl discrepancy - error limits values remain
C     unchanged). Kurtotis stat. now computed correctly. 2) To
C     account for new CAP & JMA rpts: Max. no. of sites processed
C     incr. from 40 to 120. No. of time periods input now site specific
C     (was hardwired to be same for all rpts). Temporal check for CAPs
C     attempt to get 2-hr. spanning del-time so error limits are valid
C     (since based on NPNs), else del-time must be .ge. half-hr on one
C     side, else temporal check not performed.  Max. del-time on one
C     side is 2-hrs for all rpts. 3) Changes which improve q.c. alg.:
C     Namelist switches added to specify output time window - allows
C     input to have wider time window so temporal chk at all output
C     times can be based on 2-sided diff {before temporal chk on
C     output rpts on time (assimilation) bdry based on 1-sided diff}.
C     4) Cosmetic changes: DOCBLOCKS & comments more descriptive.  DO-
C     LOOP logic more concise (esp. in temporal & vert. chks). Stdout
C     easier to read & more complete.  Excessive stdout print of
C     differencing results can be removed w/ new namelist switch which
C     controls the degree of printout. Reason codes are expanded (see
C     remarks). Rpt lvl info. in various output listings expanded &
C     sorted into bins containing both all data lvls & only lvls w/
C     either suspect or bad q. marks; output written to text files but
C     not incl. in stdout. Stat. output clarified & written to 2 text
C     files separate from stdout.  Computation & printout of stats, as
C     well as combination of stat types printed out controlled via new
C     namelist switches. Stats generated separately for NPN, CAP & JMA
C     rpts and stratified according to hgt above ground.
C 2007-01-24 M. Sienkiewicz  Modified a few 'IF(NINT' checks to allow
C     execution on Discover cluster where NINT(BMISS) < 0.  Now checks
C     for missing value along with NINT(ZZ-Z0)
C 2012-11-20  J. WOOLLEN  INITIAL PORT TO WCOSS 
C 2013-02-05  D. Keyser   Final changes to run on WCOSS: Set BUFRLIB
C     missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C     overflow; rename all REAL(8) variables as *_8; use formatted
C     print statements where previously unformatted print was > 80
C     characters.
C 2016-12-20  Stokes/Keyser  Increase the max allowable number of times
C     per station.  Skip reports and print warning if that limit is 
C     exceeded.
C 2020-01-09  J. Dong  In subroutine READPROF, changed the windowing
C     decade from 20 to 40 for cases when the years is represented by
C     2 digits instead of 4.
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - NAMELIST &RDATA (SEE REMARKS)
C     UNIT 14  - PREPBUFR FILE
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - PREPBUFR FILE
C     UNIT 52  - LISTING (TYPE 1) OF ALL OUTPUT WIND PROFILER LEVELS
C     UNIT 53  - LISTING (TYPE 2) OF ALL OUTPUT WIND PROFILER LEVELS
C     UNIT 54  - LISTING (TYPE 1) OF WIND PROFILER LEVELS WITH EVENTS
C                FROM THIS PROGRAM PLUS PRE-EXISTING PREPBUFR BAD
C                LEVELS
C     UNIT 55  - LISTING (TYPE 2) OF WIND PROFILER LEVELS WITH EVENTS
C                FROM THIS PROGRAM PLUS PRE-EXISTING PREPBUFR BAD
C                LEVELS
C     UNIT 61  - PRINTOUT OF SELECTED STATISTICS OF RESULTS ON EACH 
C                LEVEL (BY REPORT TYPE, REPORT TIME AND FIELD)
C     UNIT 62  - STATISTICS FILE CONTAINING ALL STATISTICS ON EACH
C                LEVEL (BY REPORT TYPE (BY REPORT TYPE AND FIELD, BUT
C                OVER ALL TIMES COMBINED)
C
C   SUBPROGRAMS CALLED:
C     SYSTEM:    - SYSTEM
C     UNIQUE:    - WRITBUFR CHECKS   CSTATS   DIFS
C                  DMA      EVENT    INCR     INDICATE INIT
C                  ISDONE   MODQUAL  PLINT    PRNTDATA
C                  PUTDATA  READPROF RESIDUAL PSTAT    MSTATS
C                  TWODIM   RESTRUCT SORT     ISORT    SHELL
C     LIBRARY:
C       W3NCO    - W3TAGB   W3TAGE   W3MOVDAT ERREXIT
C       BUFRLIB  - CLOSBF   CLOSMG   COPYMG   DATELEN  OPENBF
C                  OPENMG   READMG   READSB   UFBCPY   UFBINT
C                  UFBQCD   WRITSB   UFBCNT   SETBMISS GETBMISS
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C              4 - NO WIND PROFILER REPORTS READ IN
C
C REMARKS: CONTENTS OF INPUT NAMELIST &RDATA:
C               IPRINT   - Include additional diagnostic print
C                          in stdout?  (=0 - No, =1 - Yes)   (Default=0)
C               TIMWIN_E - Earliest time relative to cycle time
C                          for outputting reports (hours)    (Def=-3.00)
C               TIMWIN_L - Latest   time relative to cycle time
C                          for outputting reports (hours)    (Def=+2.99)
C               STATS    - Print out stats to units 61 and 62?
C                   STATS=TRUE  -- Yes
C                   STATS=FALSE -- No                          (Default)
C            If STATS=TRUE:
C               MEAN     - Print mean to unit 61?
C                   MEAN=TRUE  -- Yes                          (Default)
C                   MEAN=FALSE -- No
C               STDDEV   - Print standard deviation to unit 61?
C                   STDDEV=TRUE  -- Yes                        (Default)
C                   STDDEV=FALSE -- No
C               SKEW     - Print skewness to unit 61?
C                   SKEW=TRUE  -- Yes
C                   SKEW=FALSE -- No                           (Default)
C               KURT     - Print kurtosis to unit 61?
C                   KURT=TRUE  -- Yes
C                   KURT=FALSE -- No                           (Default)
C               (Note: IF STATS=TRUE, all four moments above are
C                      printed to unit 62 regardless of switches above)
C
C    Reason Codes:
C       Prior to ??/??/2003:
C           3 - Wind obs found to be of questionable quality (wind
C                quality marker set to "3" for suspect)
C          13 - Wind obs failed quality control checks (wind quality
C                marker set to "13" for bad)
C       After ??/??/2003:
C          The reason code provides detailed information about the
C          nature of the quality marker change.  It is defined as a
C          3-digit number XYZ, where the value of each digit has the
C          following meaning:
C
C          Hundredth's digit (X):
C             1 - Median check and vertical check both passed or not
C                  possible
C             2 - Median check suspect, vertical check passed or not
C                  possible
C             3 - Median check passed or not possible, vertical check
C                  suspect
C             4 - Median check and vertical check both suspect
C             5 - Median check failed, vertical check did not fail (may
C                  have passed, may be suspect, or may not be possible)
C             6 - Median check did not fail (may have passed, may be
C                  suspect, or may not be possible), vertical check
C                  failed
C             7 - Median check and vertical check both failed
C
C          Tenth's digit (Y):
C             1 - Temporal check and increment check both passed or not
C                  possible
C             2 - Temporal check suspect, increment check passed or not
C                  possible
C             3 - Temporal check passed or not possible, increment
C                  check suspect
C             4 - Temporal check and increment check both suspect
C             5 - Temporal check failed, increment check did not fail
C                  (may have passed, may be suspect, or may not be
C                  possible)
C             6 - Temporal check did not fail (may have passed, may be
C                  suspect, or may not be possible), increment check
C                  failed
C             7 - Temporal check and increment check both failed
C
C          Unit's digit (Z):
C             0 - Either: none of the checks failed (but one or more
C                  are suspect, see hundredth's and tenth's digits), or
C                  one or more checks failed (see hundredth's and
C                  tenth's digits) but no vector differences could be
C                  calculated
C             1 - One or more checks failed (see hundredth's and
C                  tenth's digits), where only one vector difference
C                  (increment-temporal, increment-vertical, temporal-
C                  vertical, temporal-median, vertical-median) could be
C                  calculated
C             2 - One or more checks failed (see hundredth's and
C                  tenth's digits), where only increment-temporal
C                  vector difference is small (all others are either
C                  large or could not be calculated)
C             3 - One or more checks failed (see hundredth's and
C                  tenth's digits), where only increment-vertical
C                  vector difference is small (all others are either
C                  large or could not be calculated)
C             4 - One or more checks failed (see hundredth's and
C                  tenth's digits), where only increment-median vector
C                  difference is small (all others are either large or
C                  could not be calculated)
C             5 - One or more checks failed (see hundredth's and
C                  tenth's digits), where only temporal-vertical vector
C                  difference is small (all others are either large or
C                  could not be calculated)
C             6 - One or more checks failed (see hundredth's and
C                  tenth's digits), where only temporal-median vector
C                  difference is small (all others are either large or
C                  could not be calculated)
C             7 - One or more checks failed (see hundredth's and
C                  tenth's digits), where only the vertical-median
C                  vector difference is small (all others are either
C                  large or could not be calculated)
C             8 - One or more checks failed (see hundredth's and
C                  tenth's digits), where any two or three of the
C                  vector differences (increment-temporal, increment-
C                  vertical, temporal-vertical, temporal-median,
C                  vertical-median) are small (all others are either
C                  large or could not be calculated)
C             9 - One or more checks failed (see hundredth's and
C                  tenth's digits), where any four, five or six of the
C                  vector differences (increment-temporal, increment-
C                  vertical, temporal-vertical, temporal-median,
C                  vertical-median) are small (all others are either
C                  large or could not be calculated)
C
C          Note: The wind quality marker is set to suspect ("3") for
C                any reason code XYZ, where X is 1-4, and Y is 1-4, and
C                Z is 0.  Any other reason code (X is 5-7, or Y is 5-7,
C                or Z is 1-9) results in a wind quality marker set to
C                bad ("13"). 
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS (iDataPlex and Cray-XC40)
C
C$$$
      PROGRAM PREPOBS_PROFCQC

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      LOGICAL  DONE,STATS,MEAN,STDDEV,SKEW,KURT
      CHARACTER*8  STN
      REAL(8)   BMISS,GETBMISS

      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON/TIMEO/TIMWIN_E,TIMWIN_L
      COMMON/STATZ/UZS(120,0:24,9,3:8),VZS(120,0:24,9,3:8),
     $ UIS(120,0:24,9,3:8),VIS(120,0:24,9,3:8),UTS(120,0:24,9,3:8),
     $ VTS(120,0:24,9,3:8),UVS(120,0:24,9,3:8),VVS(120,0:24,9,3:8),
     $ VVIS(120,0:24,9,3:8),VVTS(120,0:24,9,3:8),VVVS(120,0:24,9,3:8),
     $ VVMS(120,0:24,9,3:8),UMS(120,0:24,9,3:8),VMS(120,0:24,9,3:8)
      COMMON/PST/IOFFSET(3:8),LVLINCR(3:8),ILEVELS(3:8),STATS,MEAN,
     $ STDDEV,SKEW,KURT
      COMMON /BUFRLIB_MISSING/BMISS

      NAMELIST/RDATA/IPRINT,TIMWIN_E,TIMWIN_L,STATS,MEAN,STDDEV,SKEW,
     $ KURT

      CALL W3TAGB('PREPOBS_PROFCQC',2020,0009,1200,'NP22')

      WRITE(6,100)

      IPRINT =       0 ! default is no additional diag. print to stdout
      TIMWIN_E = -3.00 ! default earliest time to output reports
      TIMWIN_L = +2.99 ! default latest   time to output reports
      STATS = .FALSE.  ! default is to NOT calc./print stats
      MEAN  = .TRUE.   ! if STATS=T, default is to print mean
      STDDEV= .TRUE.   ! if STATS=T, default is to print stddev
      SKEW  = .FALSE.  ! if STATS=T, default is to NOT print skewness
      KURT  = .FALSE.  ! if STATS=T, default is to NOT print kurtosis
      READ(5,RDATA)           
      WRITE(6,RDATA)

C  On WCOSS should always set BUFRLIB missing (BMISS) to 10E8 to avoid
C   overflow when either an INTEGER*4 variable is set to BMISS or a
C   REAL*8 (or REAL*4) variable that is missing is NINT'd
C  -------------------------------------------------------------------
ccccc CALL SETBMISS(10E10_8)
      CALL SETBMISS(10E8_8)
      BMISS=GETBMISS()
      print *
      print *, 'BUFRLIB value for missing is: ',bmiss
      print *

      WRITE(52,101)
      WRITE(53,101)
      WRITE(54,101)
      WRITE(55,101)

      CALL INIT               ! initialize
      CALL READPROF           ! read in wind profiler data
      CALL RESTRUCT           ! restructure (sort) vertical & times
      CALL INCR               ! calculate increments

      DO IS=1,NSTN            ! Loop through all the unique stations
         WRITE(6,102) STN(IS)
         NSCAN = 0
         DONE = .FALSE.
         DO WHILE (.NOT.DONE)      ! Loop through scans
            NSCAN = NSCAN + 1
            CALL RESIDUAL(IS)      ! calculate residuals
            CALL INDICATE(IS)      ! calculate indicators
            CALL DIFS(IS,IPRINT)   ! calculate vector differences
            CALL DMA(IS)           ! decision making algorithm
            CALL ISDONE(DONE)      ! check to see if done
         ENDDO
         IF(STATS) CALL CSTATS(IS) ! collect statistics
         CALL MODQUAL(IS)          ! modify quality marks
         CALL PRNTDATA(IS)         ! print data, quality marks
      ENDDO

      IF(STATS) CALL PSTAT ! compute mean, std dev, etc. & prnt stats
      CALL WRITBUFR        ! write PREPBUFR file w/ profiler events

      CALL W3TAGE('PREPOBS_PROFCQC')

      STOP

  100 FORMAT(' WELCOME TO PREPOBS_PROFCQC -- VERSION 01/09/2020'//)
  101 FORMAT(1X,128('*'))
  102 FORMAT(/'xxxxxxxxxxxxxxxxxxxxxxxxxxx'/'Process Stn. ',A8)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    BLOCK DATA
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: BLOCK DATA FOR PREPOBS_PROFCQC.
C
C PROGRAM HISTORY LOG:
C 1996-11-20  W. Collins -- Original author.
C 2004-09-09  D. Keyser  -- XI, XT, XV now expanded to cover all
C     possible levels.
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      BLOCK DATA

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      COMMON/PST/IOFFSET(3:8),LVLINCR(3:8),ILEVELS(3:8),STATS,MEAN,
     $ STDDEV,SKEW,KURT
      COMMON/LIMITS/XI(9125), XT(15750), XV(15750)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT

      DATA  NSTN/0/,NSTN1/0/,NREP/0/,NREP1/0/,NREP_OUT/0/,NLEV/143/,
     $ NSCAN/0/,NFIN/14/,NFOUT/51/,NCH/0/

C --> Height index calculation variables:
C                       NPN  n/a  n/a  n/a  CAP  JMA
C                       ---  ---  ---  ---  ---  ---
      DATA  IOFFSET   / 250, -99, -99, -99,   0, 100/ ! Offset
      DATA  LVLINCR   / 250, -99, -99, -99,  50, 300/ ! Increment
      DATA  ILEVELS   /  64, -99, -99, -99, 120,  30/ ! Max. no. lvls

C  FOLLOWING DATA SUITABLE FOR NDAS/NAM ANALYSIS BACKGROUND.
C  ALSO USED FOR RAP (AND BEFORE THAT RUC) BACKGROUND.
C  ---------------------------------------------------------

C               1 -  6875 m : 2.10
C            6876 -  7125 m : 2.20
C            7126 -  7375 m : 2.40
C            7376 -  7625 m : 2.60
C            7626 -  7875 m : 2.80
C            7876 -  8125 m : 3.00
C            8126 -  8375 m : 3.20
C            8376 -  8625 m : 3.40
C            8626 -  8875 m : 3.60
C            8876 -  9125 m : 3.70
C                 >  9125 m : 3.80

      DATA XI/6875*2.10,  250*2.20,  250*2.40,  250*2.60,  250*2.80,
     $         250*3.00,  250*3.20,  250*3.40,  250*3.60,  250*3.70/


C               1 -  7125 m : 1.20
C            7126 -  7375 m : 1.25
C            7376 -  7625 m : 1.30
C            7626 -  7875 m : 1.35
C            7876 -  8125 m : 1.40
C            8126 -  8375 m : 1.45
C            8376 -  8625 m : 1.50
C            8626 -  8875 m : 1.55
C            8876 -  9125 m : 1.60
C            9126 -  9750 m : 1.65
C            9501 - 10750 m : 1.70
C           10751 - 11750 m : 1.75
C           11751 - 12750 m : 1.80
C           12751 - 13750 m : 1.85
C           13751 - 14750 m : 1.90
C           14751 - 15750 m : 1.95
C                 > 15750 m : 2.00

      DATA XT/7125*1.20,  250*1.25,  250*1.30,  250*1.35,  250*1.40,
     $         250*1.45,  250*1.50,  250*1.55,  250*1.60,  625*1.65,
     $        1000*1.70, 1000*1.75, 1000*1.80, 1000*1.85, 1000*1.90,
     $        1000*1.95/


C               1 -   625 m : 2.20
C             626 -   875 m : 2.00
C             876 -  1125 m : 1.80
C            1126 -  1375 m : 1.50
C            1376 -  1625 m : 1.30
C            1626 -  1875 m : 1.00
C            1876 -  8125 m : 0.70
C            8126 -  8375 m : 0.80
C            8376 -  8625 m : 1.00
C            8626 -  8875 m : 1.20
C            8876 -  9125 m : 1.40
C            9126 -  9750 m : 1.60
C            9501 - 10750 m : 1.80
C           10751 - 11750 m : 2.00
C           11751 - 12750 m : 2.20
C           12751 - 13750 m : 2.40
C           13751 - 14750 m : 2.60
C           14751 - 15750 m : 2.80
C                 > 15750 m : 3.00

      DATA XV/ 625*2.20,  250*2.00,  250*1.80,  250*1.50,  250*1.30,
     $         250*1.00, 6250*0.70,  250*0.80,  250*1.00,  250*1.20,
     $         250*1.40,  625*1.60, 1000*1.80, 1000*2.00, 1000*2.20,
     $        1000*2.40, 1000*2.60, 1000*2.80/

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    WRITBUFR
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: WRITE OUTPUT PREPBUFR FILE, IDENTICAL TO INPUT
C   EXCEPT FOR THE ADDITION OF WIND PROFILER Q.C. EVENTS.
C
C PROGRAM HISTORY LOG:
C 1996-11-20  W. Collins -- Original author.
C 2004-09-09  D. Keyser  -- Namelist switches added to specify output
C     time window - allows input to have wider time window so temporal
C     chk at all output times can be based on 2-sided diff {before
C     temporal chk on output rpts on time (assimilation) bdry based on
C     1-sided diff}.
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL WRITBUFR
C   INPUT FILES:
C     NFIN    - PREPBUFR INPUT TO THIS PROGRAM
C
C   OUTPUT FILES:
C     NFOUT   - PREPBUFR OUTPUT FROM THIS PROGRAM (INCL. WIND PROFILER
C             - EVENTS)
C     UNIT 06 - STANDARD OUTPUT PRINT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE WRITBUFR

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      LOGICAL  FIRST
      CHARACTER*8  STN,LAST,SUBSET,CID
      CHARACTER*40 HSTR, OSTR, ESTR
      REAL  UVO(255,2),VQM(255),VRC(255)
      REAL(8)  SID_8,HDR_8(10),OBS_8(10,255),BMISS
      INTEGER  IND(NL)

      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON/DATEZ/JDATE(4),KDATE(5,NT,NS),MDATE(5),NDATE(5)
      COMMON/HEADER/SID_8,XOB,YOB,DHR,ELV,XTP,TYP,T29,NLV
      COMMON/PROBS/ZOB(255),UOB(255),VOB(255),WQM(255),POB(255),
     $ UFC(255),VFC(255),CAT(255),SOB(255),DOB(255)
      COMMON/PROFZ/ZZ(NL,NT,NS),UZ(NL,NT,NS),VZ(NL,NT,NS),
     $ QZ(NL,NT,NS),PZ(NL,NT,NS),UFZ(NL,NT,NS),VFZ(NL,NT,NS),
     $ SZ(NL,NT,NS),DZ(NL,NT,NS),LEVZ(NL,NT,NS)
      COMMON/PROFP/ZP(NL,NT,NS),UP(NL,NT,NS),VP(NL,NT,NS),QP(NL,NT,NS),
     $ PP(NL,NT,NS),UFP(NL,NT,NS),VFP(NL,NT,NS),LEVP(NL,NT,NS)
      COMMON/QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON/TIMEO/TIMWIN_E,TIMWIN_L
      COMMON /BUFRLIB_MISSING/BMISS

      EQUIVALENCE  (SID_8,CID)

      DATA  HSTR/'SID XOB YOB DHR ELV ITP TYP T29        '/
      DATA  OSTR/'ZOB UOB VOB WQM POB UFC VFC CAT FFO DDO'/
      DATA  ESTR/'UOB VOB WQM WPC WRC                    '/

      DATA  LAST/'XXXXXXXX'/ISUBO/0/,ISUBOT/0/,IRECOL/0/,IRECO/0/

      WRITE(6,100)

C  OPEN INPUT FILE AND GET DATE
C  ----------------------------

      REWIND NFIN
      CALL DATELEN(10)
      CALL OPENBF(NFIN,'IN',NFIN)
      WRITE(6,101) NFIN
      CALL OPENBF(NFOUT,'OUT',NFIN)
      WRITE(6,102) NFOUT,NFIN
      CALL UFBQCD(NFIN,'CQCPROF',CQCPC)

   10 CONTINUE

C  READ NEXT INPUT BUFR MESSAGE
C  ----------------------------

      CALL READMG(NFIN,SUBSET,IDATEP,IRET)
      IF(IRET.NE.0) THEN
C ALL BUFR MESSAGES IN FILE HAVE BEEN READ CLOSE INPUT DATA SET
         IF(LAST.EQ.'PROFLR  ') THEN
            CALL UFBCNT(NFOUT,IRECO,ISUBO)
            ISUBOT = ISUBOT + ISUBO
            WRITE(6,103) IRECO,LAST,ISUBO,ISUBOT
         ENDIF
         WRITE(6,104) IRECO,ISUBOT,NFIN
         CALL CLOSBF(NFOUT)
         WRITE(6,105)  NFOUT,NREP_OUT
         RETURN
      ENDIF
      CALL UFBCNT(NFIN,IRECI,ISUBI)
      IF(SUBSET.EQ.'PROFLR  ')  WRITE(6,106) IRECI,SUBSET
      IF(LAST.NE.SUBSET) THEN
         IF(LAST.EQ.'PROFLR  ') THEN
            CALL UFBCNT(NFOUT,IRECO,ISUBO)
            ISUBOT = ISUBOT + ISUBO
            WRITE(6,103) IRECO,LAST,ISUBO,ISUBOT
C MUST CLOSE THE LAST "PROFLR" TABLE A ENTRY MESSAGE
            CALL CLOSMG(NFOUT)
         ENDIF
         WRITE(6,107) NFOUT,SUBSET,IDATEP
         CALL UFBCNT(NFOUT,IRECOL,ISUBO)
         IRECOL = IRECOL + 1
      ENDIF
      LAST = SUBSET
      IF(SUBSET.NE.'PROFLR  ') THEN
C ALL TABLE A ENTRY BUFR MESSAGES THAT ARE NOT "PROFLR" ARE SIMPLY
C  COPIED FROM INPUT FILE TO OUTPUT FILE AS IS (NO DECODING OF SUBSETS)
         CALL COPYMG(NFIN,NFOUT)
         CALL UFBCNT(NFOUT,IRECO,ISUBO)
         ISUBOT = ISUBOT + ISUBO
         GOTO 10
      ENDIF
C TABLE A ENTRY "PROFLR" MESSAGES COME HERE TO DECODE/ENCODE EACH SUBSET
      CALL OPENMB(NFOUT,SUBSET,IDATEP)

   20 CONTINUE

C READ IN NEXT SUBSET (REPORT) FROM THIS BUFR MESSAGE
      CALL READSB(NFIN,IRET)
C NON-ZERO IRET IN READSB MEANS ALL SUBSETS IN BUFR MSG HAVE BEEN READ
C GO ON TO READ NEXT BUFR MESSAGE
      IF(IRET.NE.0) GOTO 10

C  READ AND STORE HEADER ELEMENTS
C  ------------------------------

      CALL UFBINT(NFIN,HDR_8,10,  1,IRET,HSTR)
      SID_8 = HDR_8(1)
      XOB = HDR_8(2)
      YOB = HDR_8(3)
      DHR = HDR_8(4)
      ELV = HDR_8(5)
      XTP = HDR_8(6)
      TYP = HDR_8(7)
      T29 = HDR_8(8)

ccccc print*, CID,(HDR_8(i),i=2,8)

C  DETERMINE IF THIS REPORT IS WITHIN OUTPUT TIME WINDOW RANGE
C  -----------------------------------------------------------

      IF(NINT(DHR*1000.).LT.NINT(TIMWIN_E*1000.).OR.
     $   NINT(DHR*1000.).GT.NINT(TIMWIN_L*1000.)) THEN
         WRITE(6,108)  CID,YOB,XOB,DHR
         GOTO 20
      ENDIF

C  MATCH REPORT ID TO STORED DATA
C  ------------------------------

      DO NN=1,NSTN
        N = NN
        IF(CID.EQ.STN(N)) THEN

C  MATCH REPORT D-TIME TO STORED DATA
C  ----------------------------------

            DO MM=1,NTIMES(N)
               M = MM
               IF(DHR.EQ.TIM(MM,N))  GOTO 30
            ENDDO
         ENDIF
      ENDDO


C  NO MATCH FOR ID OR TIME; COPY REPORT TO OUTPUT
C  ----------------------------------------------

      WRITE(6,109)  CID,DHR
      NREP_OUT = NREP_OUT + 1
      CALL UFBCPY(NFIN,NFOUT)
      CALL WRITSB(NFOUT)
      GOTO 20

   30 CONTINUE

      WRITE(6,110)  CID,N,M

C  READ DATA OF PROPER SUBSET AND STATION
C  --------------------------------------

      CALL UFBINT(NFIN,OBS_8,10,255,NLV,OSTR)
      DO L=1,NLV
        ZOB(L) = OBS_8(1,L)
        UOB(L) = OBS_8(2,L)
        VOB(L) = OBS_8(3,L)
        WQM(L) = OBS_8(4,L)
        POB(L) = OBS_8(5,L)
        UFC(L) = OBS_8(6,L)
        VFC(L) = OBS_8(7,L)
        CAT(L) = OBS_8(8,L)
        SOB(L) = OBS_8(9,L)
        DOB(L) = OBS_8(10,L)
      ENDDO

C  COPY ORIGINAL REPORT TO OUTPUT
C  ------------------------------

      CALL UFBCPY(NFIN,NFOUT)

C  FORM BUFR EVENTS
C  ----------------

      FIRST = .TRUE.
      LL = 0
      DO LZ=1,NZZ(N)
        IF(IQCC(LZ,M,N).NE.0 .AND. LEVZ(LZ,M,N).GT.0) THEN
          IF(FIRST) THEN
            WRITE(6,111)
            FIRST = .FALSE.
          ENDIF
          LL = LL + 1
          L0 = LEVZ(LZ,M,N)
          UVO(LL,1) = UOB(L0)
          UVO(LL,2) = VOB(L0)
          VQM(LL) = IQCC(LZ,M,N)
          VRC(LL) = IRC(LZ,M,N)
          IND(LL) = L0
          WRITE(6,112) L0,CID,NINT(CAT(L0)),ZOB(L0),LL,LZ,M,N,UVO(LL,1),
     $     UVO(LL,2),IQCC(LZ,M,N)
          IF(L0.GT.1) THEN
            LOOP1: DO LM=L0-1,1,-1
             IF(ZOB(LM).LT.BMISS) EXIT LOOP1
              IF(CAT(LM).EQ.7) THEN
                LL = LL + 1
                UVO(LL,1) = UOB(LM)
                UVO(LL,2) = VOB(LM)
                VQM(LL) = IQCC(LZ,M,N)
                VRC(LL) = IRC(LZ,M,N)
                IND(LL) = LM
                WRITE(6,113) LM,CID,NINT(CAT(LM)),ZOB(LM),LL,LZ,M,N,
     $           UVO(LL,1),UVO(LL,2),IQCC(LZ,M,N)
              ENDIF
            ENDDO LOOP1
          ENDIF
        ENDIF
      ENDDO

C  WRITE EVENTS
C  ------------

      IF(LL.NE.0) THEN
        CALL EVENT(NFOUT,ESTR,NLV,UVO,VQM,VRC,IND,LL,CQCPC)
      ELSE
         WRITE(6,114)
      ENDIF

      NREP_OUT = NREP_OUT + 1
      CALL WRITSB(NFOUT)
      CALL UFBCNT(NFOUT,IRECO,ISUBON)
      IF(IRECO.GT.IRECOL) THEN
         IRECOL = IRECO
         ISUBOT = ISUBOT + ISUBO
         WRITE(6,115) IRECO-1,ISUBO,ISUBOT
      ENDIF
      ISUBO = ISUBON

      GOTO 20

  100 FORMAT(//'READY TO READ ORIGINAL DATA, COPY TO OUTPUT AND ADD ',
     $ 'WIND PROFILER EVENTS'/)
  101 FORMAT(/5X,'+++> PREPBUFR DATA SET IN UNIT ',I3,' SUCCESSFULLY',
     $ ' OPENED FOR INPUT; FIRST MESSAGE CONTAINS BUFR TABLES A,B,D'/)
  102 FORMAT(/5X,'+++> PREPBUFR DATA SET IN UNIT ',I3,' SUCCESSFULLY',
     $ ' OPENED FOR OUTPUT; CUSTOMIZED BUFR TABLES A,B,D IN UNIT ',I3/
     $ 12X,'READ IN AND ENCODED INTO MESSAGE NO. 1 OF OUTPUT DATA SET'/)
  103 FORMAT(/' --- WROTE BUFR DATA MSG NO. ',I10,' -- TABLE A ENTRY "',
     $A8,'" - CONTAINS',I6,' REPORTS (TOTAL NO. RPTS WRITTEN =',I7,')'/)
  104  FORMAT(/' --- ALL TOTAL OF',I11,' BUFR MESSAGES WRITTEN OUT -- ',
     $ 'TOTAL NUMBER OF REPORTS WRITTEN =',I7//5X,'===> PREPBUFR DATA ',
     $ 'SET IN UNIT ',I3,' SUCCESSFULLY CLOSED FROM FINAL READ OF ALL ',
     $ 'OBS')
  105 FORMAT(/5X,'===> PREPBUFR DATA SET IN UNIT ',I3,' SUCCESSFULLY',
     $ ' CLOSED AFTER WRITING OF ALL OBS.'/10X,'THE TOTAL NUMBER OF ',
     $ 'WIND PROFILER REPORTS WRITTEN WAS',I4/25X,' *** ALL DONE ***'/)
  106 FORMAT(' --- READ IN BUFR DATA MESSAGE NUMBER',I6,' WITH TABLE ',
     $ 'A ENTRY "',A8)
  107 FORMAT(/' ===> NEXT MESSAGE IN OUTPUT PREPBUFR DATA SET IN UNIT ',
     $ I3,' HAS NEW TABLE A ENTRY OF "',A6,'" -- DATE IS',I11)
  108 FORMAT(/'##WRITBUFR: RPT OUTSIDE TIME WINDOW, ID ',A8,', LAT=',
     $ F7.2,'N, LON=',F8.2,'E, D-TIM=',F7.3,' -- DO NOT COPY RPT TO ',
     $ 'OUTPUT FILE'/)
  109 FORMAT(/'##WRITBUFR: NO ID AND D-TIME MATCH FOR ID ',A8,' AT ',
     $ 'D-TIME ',F6.2,' -- COPY REPORT TO OUTPUT PREPBUFR FILE W/O ANY',
     $ ' Q.C.'/)
  110 FORMAT(' WRITBUFR: FOUND A MATCH FOR ID ',A8,' (STN. NO.',I4,')',
     $ ' AT TIME PERIOD',I3)
  111 FORMAT(10X,'  STATION   CAT       ZOB EVNT#  LVL  TIM  STN     ',
     $ 'UO      VO IQCC')
  112 FORMAT(' L0: ',I5,2X,A8,I5,2X,F8.0,4I5,2F8.1,I5)
  113 FORMAT(' LM: ',I5,2X,A8,2I5,X,F8.0,4I5,2F8.1,I5)
  114 FORMAT('  ... NO EVENTS WRITTEN')
  115 FORMAT(/' --- THIS REPORT OPENS NEW MSG (SAME TABLE A): LAST ',
     $ 'DATA MSG WAS NO.',I5,' WITH',I5,' REPORTS (TOTAL NO. REPORTS ',
     $ 'WRITTEN =',I7,')'/)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CHECKS
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: PERFORM INCREMENT, TEMPORAL AND VERTICAL CHECKS FOR
C   A SINGLE UNIQUE STATION.
C
C PROGRAM HISTORY LOG:
C 1995-04-04  W. Collins -- Original author.
C 2004-09-09  D. Keyser  -- Lvl index in all arrays now site specific
C     to allow proper temporal checking at each individual site. Incr.,
C     temporal, vert. & median chk error limits now directly assoc. w/
C     a rpted hgt above ground lvl rather than w/ lvl index in arrays
C     (ensures proper error limit used at all lvls & eliminates hgt
C     above ground vs. hgt above sea lvl discrepancy - error limits
C     values remain unchanged). Temporal check for CAPs attempt to get
C     2-hr. spanning del-time so error limits are valid (since based on
C     NPNs), else del-time must be .ge. half-hr on one side, else
C     temporal check not performed.  Max. del-time on one side is 2-hrs
C     for all rpts. DO-LOOP logic more concise (esp. in temporal &
C     vert. chks.)
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:   SUBROUTINE CHECKS(IS)
C   INPUT ARGUMENT LIST:
C     IS       - STATION INDEX
C
C REMARKS: 
C   VARIABLE NAMES:
C     U        - U-COMPONENT OF WIND (M)
C     V        - V-COMPONENT OF WIND (M)
C     UI       - U-COMPONENT INCREMENT
C     VI       - V-COMPONENT INCREMENT
C     UT       - U-COMPONENT TEMPORAL RESIDUAL
C     VT       - V-COMPONENT TEMPORAL RESIDUAL
C     UV       - U-COMPONENT VERTICAL RESIDUAL
C     VV       - V-COMPONENT VERTICAL RESIDUAL
C     VVI      - VECTOR INCREMENT
C     VVT      - VECTOR TEMPORAL RESIDUAL
C     VVV      - VECTOR VERTICAL RESIDUAL
C     ICQC     - DMA INDEX FOR WIND QUALITY CONTROL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE CHECKS(IS)

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      CHARACTER*8     STN
      REAL(8)   BMISS

      COMMON/PROFZ/ZZ(NL,NT,NS),UZ(NL,NT,NS),VZ(NL,NT,NS),
     $ QZ(NL,NT,NS),PZ(NL,NT,NS),UFZ(NL,NT,NS),VFZ(NL,NT,NS),
     $ SZ(NL,NT,NS),DZ(NL,NT,NS),LEVZ(NL,NT,NS)
      COMMON/RESIDS/UI(NL,NT,NS),VI(NL,NT,NS),UT(NL,NT),VT(NL,NT),
     $ UV(NL,NT),VV(NL,NT),VVI(NL,NT),VVT(NL,NT),VVV(NL,NT),UM(NL,NT),
     $ VM(NL,NT),VVM(NL,NT),NI(NL,NT),NTMP(NL,NT),NV(NL,NT),NM(NL,NT),
     $ ZI(NL,NT),ZT(NL,NT),ZV(NL,NT),ZM(NL,NT),DIT(NL,NT),DIV(NL,NT),
     $ DTV(NL,NT),DIM(NL,NT),DVM(NL,NT),DTM(NL,NT)
      COMMON /QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/LIMITS/XI(9125),XT(15750),XV(15750)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON /BUFRLIB_MISSING/BMISS

C     PERFORM TEMPORAL AND VERTICAL CHECKS.
C     USE ONLY NON-MISSING DATA AT ADJACENT TIME OR LEVEL.
C     DO NOT USE INFLUENCING DATA THAT ARE BAD.
C     CHECKS ARE PERFORMED ON INCREMENTS.
C     ALSO CALCULATE THE VECTOR INCREMENT, TEMPORAL RESIDUAL,
C     AND VERTICAL RESIDUAL.

      NZZM1 = NZZ(IS) - 1
      NZZM2 = NZZ(IS) - 2

C  CALCULATE THE VECTOR INCREMENT
C  ------------------------------

      DO N=1,NTIMES(IS)
         DO L=1,NZZ(IS)
            IF(MAX(ABS(UI(L,N,IS)),ABS(VI(L,N,IS))).LT.BMISS) THEN
               VVI(L,N) = SQRT(UI(L,N,IS)**2 + VI(L,N,IS)**2)
            ELSE
               VVI(L,N) = BMISS
            ENDIF
         ENDDO
      ENDDO

      DO N=1,NTIMES(IS)
        LOOP1: DO L=1,NZZ(IS)
          IF(MAX(ZZ(L,N,IS),Z0(IS)) .LT. BMISS .AND.
     .         NINT(ZZ(L,N,IS)-Z0(IS)).LT.9126) THEN
             VMX =  10.0 * XI(NINT(ZZ(L,N,IS)-Z0(IS)))
          ELSE
             VMX =  10.0 * 3.80
          ENDIF
          IF(L.NE.NZZ(IS)) THEN
             IF(MAX(ZZ(L+1,N,IS),Z0(IS)) .LT. BMISS .AND.
     .            NINT(ZZ(L+1,N,IS)-Z0(IS)).LT.9126) THEN
                VPMX = 10.0 * XI(NINT(ZZ(L+1,N,IS)-Z0(IS)))
             ELSE
                VPMX = 10.0 * 3.80
             ENDIF
          ENDIF
          IF(L.NE.1) THEN
             IF(MAX(ZZ(L-1,N,IS),Z0(IS)) .LT. BMISS .AND.
     .            NINT(ZZ(L-1,N,IS)-Z0(IS)).LT.9126) THEN
                VMMX = 10.0 * XI(NINT(ZZ(L-1,N,IS)-Z0(IS)))
             ELSE
                VMMX = 10.0 * 3.80
             ENDIF 
          ENDIF 

C  PERFORM TEMPORAL CHECK
C  ----------------------

          UT(L,N)  = BMISS
          VT(L,N)  = BMISS
          VVT(L,N) = BMISS
          IF(NTIMES(IS).LT.2)  GOTO 707
          IF(MAX(UI(L,N,IS),VI(L,N,IS)).GE.BMISS) GOTO 707
          ITRY  = 0
          ITLM1 = 100
          ITLM2 = 100

  606     CONTINUE

          DO NN = 1,NTIMES(IS)-1
            N1 = NN - 1
            IF(N-NN.GE.1) THEN
              IF(NINT(ABS(TIM(N,IS)*100-TIM(N-NN,IS)*100)).LT.ITLM1)THEN
                CYCLE
              ELSE
                N1 = NN
                EXIT
              ENDIF
            ENDIF
            EXIT
          ENDDO
          DO NN = 1,NTIMES(IS)-1
            N2 = NN - 1
            IF(N+NN.LE.NTIMES(IS)) THEN
              IF(NINT(ABS(TIM(N,IS)*100-TIM(N+NN,IS)*100)).LT.ITLM2)THEN
                CYCLE
              ELSE
                N2 = NN
                EXIT
              ENDIF
            ENDIF
            EXIT
          ENDDO
          IF(MAX(N1,N2).EQ.0)  GOTO 707
          IF(MIN(N1,N2).GT.0) THEN
            IF(MAX(ICQC(L,N-N1,IS),ICQC(L,N+N2,IS)).EQ.0) THEN
              IF(MAX(ABS(UI(L,N-N1,IS)),ABS(VI(L,N-N1,IS)),
     $               ABS(UI(L,N+N2,IS)),ABS(VI(L,N+N2,IS))).LT.VMX) THEN
                DT(L,N,IS) = ABS(TIM(N-N1,IS) - TIM(N+N2,IS))
                IF(DT(L,N,IS).LT.2.01) THEN
                  UT(L,N) =UI(L,N,IS) -0.5*(UI(L,N-N1,IS)+UI(L,N+N2,IS))
                  VT(L,N) =VI(L,N,IS) -0.5*(VI(L,N-N1,IS)+VI(L,N+N2,IS))
                  VVT(L,N) = SQRT(UT(L,N)**2 + VT(L,N)**2)
                ELSE
                  ITRY = ITRY + 1
                  IF(ITRY.EQ.1) THEN
                    ITLM1 = 075
                    GOTO 606
                  ELSE IF(ITRY.EQ.2) THEN
                    ITLM2 = 075
                    GOTO 606
                  ELSE IF(ITRY.EQ.3) THEN
                    ITLM1 = 050
                    GOTO 606
                  ELSE IF(ITRY.EQ.4) THEN
                    ITLM2 = 050
                    GOTO 606
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF(N1.EQ.0) THEN
C  Use one-sided differences at earliest time
            IF(ICQC(L,N+N2,IS).EQ.0) THEN
              IF(MAX(ABS(UI(L,N+N2,IS)),ABS(VI(L,N+N2,IS))).LT.VMX) THEN
                DT(L,N,IS) = ABS(TIM(N,IS) - TIM(N+N2,IS))
                IF(DT(L,N,IS).LT.1.01) THEN
                  UT(L,N) = UI(L,N,IS) - UI(L,N+N2,IS)
                  VT(L,N) = VI(L,N,IS) - VI(L,N+N2,IS)
                  VVT(L,N) = SQRT(UT(L,N)**2 + VT(L,N)**2)
                ELSE
                  ITRY = ITRY + 1
                  IF(ITRY.EQ.1) THEN
                    ITLM2 = 075
                    GOTO 606
                  ELSE IF(ITRY.EQ.2) THEN
                    ITLM2 = 050
                    GOTO 606
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF(N2.EQ.0) THEN
C  Use one-sided differences at latest time
            IF(ICQC(L,N-N1,IS).EQ.0) THEN
              IF(MAX(ABS(UI(L,N-N1,IS)),ABS(VI(L,N-N1,IS))).LT.VMX) THEN
                DT(L,N,IS) = ABS(TIM(N,IS) - TIM(N-N1,IS))
                IF(DT(L,N,IS).LT.1.01) THEN
                  UT(L,N) = UI(L,N,IS) - UI(L,N-N1,IS)
                  VT(L,N) = VI(L,N,IS) - VI(L,N-N1,IS)
                  VVT(L,N) = SQRT(UT(L,N)**2 + VT(L,N)**2)
                ELSE
                  ITRY = ITRY + 1
                  IF(ITRY.EQ.1) THEN
                    ITLM1 = 075
                    GOTO 606
                  ELSE IF(ITRY.EQ.2) THEN
                    ITLM1 = 050
                    GOTO 606
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF

  707     CONTINUE

C  PERFORM VERTICAL CHECK
C  ----------------------

          UV(L,N)  = BMISS
          VV(L,N)  = BMISS
          VVV(L,N) = BMISS
          IF(L.EQ.1) THEN
C  Use one-sided differences at bottom, allow up to one missing or bad
C   intervening level
            IF(MAX(ABS(UI(1,N,IS)),ABS(VI(1,N,IS))).LT.BMISS) THEN
              DO J = 2,3
                IF(ICQC(J,N,IS).EQ.0) THEN
                  IF(MAX(ABS(UI(J,N,IS)),ABS(VI(J,N,IS))).LT.VPMX) THEN
                    UV(1,N) = UI(1,N,IS) - UI(J,N,IS)
                    VV(1,N) = VI(1,N,IS) - VI(J,N,IS)
                    VVV(1,N) = SQRT(UV(1,N)**2 + VV(1,N)**2)
                    CYCLE LOOP1
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ELSE IF(L.EQ.NZZ(IS)) THEN
C  Use one-sided differences at top, allow up to one missing or bad
C   intervening level
            IF(MAX(ABS(UI(NZZ(IS),N,IS)),ABS(VI(NZZ(IS),N,IS)))
     $       .LT.BMISS) THEN
              DO J = NZZM1,NZZM2,-1
                IF(ICQC(J,N,IS).EQ.0) THEN
                  IF(MAX(ABS(UI(J,N,IS)),ABS(VI(J,N,IS))).LT.VMMX) THEN
                    UV(NZZ(IS),N)  = UI(NZZ(IS),N,IS) - UI(J,N,IS)
                    VV(NZZ(IS),N)  = VI(NZZ(IS),N,IS) - VI(J,N,IS)
                    VVV(NZZ(IS),N) =
     $               SQRT(UV(NZZ(IS),N)**2 + VV(NZZ(IS),N)**2)
                    CYCLE LOOP1
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ELSE IF(L.GT.2 .AND. L.LT.NZZM2) THEN
C  Attempt two-sided differences when two levels or more away from top
C   and two levels or more away from bottom, allow up to one missing or
C   bad intervening level on either side (but NOT on BOTH sides)
            IF(MAX(ABS(UI(L,N,IS)),ABS(VI(L,N,IS))).LT.BMISS) THEN
              DO K = 1,2
                DO J = 1,2
                  IF(MIN(J,K).EQ.2) GOTO 300
                  IF(MAX(ICQC(L+J,N,IS),ICQC(L-K,N,IS)).EQ.0) THEN
                    IF(MAX(ABS(UI(L+J,N,IS)),ABS(VI(L+J,N,IS))).LT.VPMX
     $           .AND. MAX(ABS(UI(L-K,N,IS)),ABS(VI(L-K,N,IS))).LT.VMMX)
     $               THEN
                      UV(L,N)=UI(L,N,IS)-0.5*(UI(L+J,N,IS)+UI(L-K,N,IS))
                      VV(L,N)=VI(L,N,IS)-0.5*(VI(L+J,N,IS)+VI(L-K,N,IS))
                      VVV(L,N) = SQRT(UV(L,N)**2 + VV(L,N)**2)
                      CYCLE LOOP1
                    ENDIF
                  ENDIF
                ENDDO
              ENDDO

  300         CONTINUE

C  Use one-sided differences if two or more missing or bad intervening
C   levels on one side and a non-missing and non-bad neighboring level
C   on the other side (but do NOT allow for any missing or bad
C   intervening levels on the differencing side)
              DO J = 1,-1,-2
                IF(ICQC(L+J,N,IS).EQ.0) THEN
                  IF(MAX(ABS(UI(L+J,N,IS)),ABS(VI(L+J,N,IS))).LT.VPMX)
     $             THEN
                    UV(L,N) = UI(L,N,IS) - UI(L+J,N,IS)
                    VV(L,N) = VI(L,N,IS) - VI(L+J,N,IS)
                    VVV(L,N) = SQRT(UV(L,N)**2 + VV(L,N)**2)
                    CYCLE LOOP1
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ELSE
C  Attempt two-sided differences when either one level away from top or
C   on level away from bottom, do NOT allow any missing or bad
C   intervening levels on EITHER side (even though one may be such on
C   one side)
            IF(MAX(ABS(UI(L,N,IS)),ABS(VI(L,N,IS))).LT.BMISS) THEN
              IF(MAX(ICQC(L+1,N,IS),ICQC(L-1,N,IS)).EQ.0) THEN
                IF(MAX(ABS(UI(L+1,N,IS)),ABS(VI(L+1,N,IS))).LT.VPMX.AND.
     $             MAX(ABS(UI(L-1,N,IS)),ABS(VI(L-1,N,IS))).LT.VMMX)THEN
                  UV(L,N) =UI(L,N,IS) -0.5 *(UI(L+1,N,IS) +UI(L-1,N,IS))
                  VV(L,N) =VI(L,N,IS) -0.5 *(VI(L+1,N,IS) +VI(L-1,N,IS))
                  VVV(L,N) = SQRT(UV(L,N)**2 + VV(L,N)**2)
                  CYCLE LOOP1
                ENDIF
              ENDIF
C  Use one-sided differences if a missing or bad neighboring level on
C   one side and a non-missing and non-bad neighboring level on the
C   other side (but do NOT allow for any missing or bad intervening
C   levels on the differencing side)
              DO J = 1,-1,-2
                IF(ICQC(L+J,N,IS).EQ.0) THEN
                  IF(MAX(ABS(UI(L+J,N,IS)),ABS(VI(L+J,N,IS))).LT.VPMX)
     $             THEN
                    UV(L,N) = UI(L,N,IS) - UI(L+J,N,IS)
                    VV(L,N) = VI(L,N,IS) - VI(L+J,N,IS)
                    VVV(L,N) = SQRT(UV(L,N)**2 + VV(L,N)**2)
                    CYCLE LOOP1
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO  LOOP1
      ENDDO

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CSTATS
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: CALCULATE MOMENTS FOR RESIDUALS FOR A SINGLE STATION.
C
C PROGRAM HISTORY LOG:
C 1995-04-04  W. Collins -- Original author.
C 2004-09-09  D. Keyser  -- Stats generated separately for NPN, CAP &
C     JMA rpts and stratified according to hgt above ground.
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:   SUBROUTINE CSTATS(IS)
C
C   INPUT ARGUMENT LIST:
C     IS       - STATION INDEX
C
C   OUTPUT FILES:
C     UNIT 61  - PRINTOUT OF SELECTED STATISTICS OF RESULTS ON EACH 
C                LEVEL (BY REPORT TYPE, REPORT TIME AND FIELD)
C
C REMARKS: 
C   VARIABLE NAMES:
C     UT       - U-COMPONENT TEMPORAL RESIDUAL
C     VT       - V-COMPONENT TEMPORAL RESIDUAL
C     UV       - U-COMPONENT VERTICAL RESIDUAL
C     VV       - V-COMPONENT VERTICAL RESIDUAL
C     UM       - U-COMPONENT MEDIAN RESIDUAL
C     VM       - V-COMPONENT MEDIAN RESIDUAL
C     VVI      - VECTOR INCREMENT
C     VVT      - VECTOR TEMPORAL RESIDUAL
C     VVV      - VECTOR VERTICAL RESIDUAL
C     VVM      - VECTOR MEDIAN RESIDUAL
C
C   OUTPUT ARGUMENT LIST:
C     ALL PARAMETERS HAVE AN 'S' ADDED TO BASIC INPUT VARIABLE.
C     THE MEANING IS DESCRIBED IN THE COMMENTS BELOW.
C     UZS...VVMS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE CSTATS(IS)

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      CHARACTER*8  STN
      REAL(8)   BMISS

      COMMON/DATEZ/JDATE(4),KDATE(5,NT,NS),MDATE(5),NDATE(5)
      COMMON/PROFZ/ZZ(NL,NT,NS),UZ(NL,NT,NS),VZ(NL,NT,NS),
     $ QZ(NL,NT,NS),PZ(NL,NT,NS),UFZ(NL,NT,NS),VFZ(NL,NT,NS),
     $ SZ(NL,NT,NS), DZ(NL,NT,NS),LEVZ(NL,NT,NS)
      COMMON/RESIDS/UI(NL,NT,NS),VI(NL,NT,NS),UT(NL,NT),VT(NL,NT),
     $ UV(NL,NT),VV(NL,NT),VVI(NL,NT),VVT(NL,NT),VVV(NL,NT),UM(NL,NT),
     $ VM(NL,NT),VVM(NL,NT),NI(NL,NT),NTMP(NL,NT),NV(NL,NT),NM(NL,NT),
     $ ZI(NL,NT),ZT(NL,NT),ZV(NL,NT),ZM(NL,NT),DIT(NL,NT),DIV(NL,NT),
     $ DTV(NL,NT),DIM(NL,NT),DVM(NL,NT),DTM(NL,NT)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON/QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/STATZ/UZS(120,0:24,9,3:8),VZS(120,0:24,9,3:8),
     $ UIS(120,0:24,9,3:8),VIS(120,0:24,9,3:8),UTS(120,0:24,9,3:8),
     $ VTS(120,0:24,9,3:8),UVS(120,0:24,9,3:8),VVS(120,0:24,9,3:8),
     $ VVIS(120,0:24,9,3:8),VVTS(120,0:24,9,3:8),VVVS(120,0:24,9,3:8),
     $ VVMS(120,0:24,9,3:8),UMS(120,0:24,9,3:8),VMS(120,0:24,9,3:8)
      COMMON/PST/IOFFSET(3:8),LVLINCR(3:8),ILEVELS(3:8),STATS,MEAN,
     $ STDDEV,SKEW,KURT
      COMMON/TIMEO/TIMWIN_E,TIMWIN_L
      COMMON /BUFRLIB_MISSING/BMISS

C  HEIGHT INDEX
C  ------------
C
C NOAA PROFILER NETWORK:
C  250 m increments starting at 376 m and ending at 16365 m (64 levels)
C  (height above ground)
C
C COOPERATIVE AGENCY PROFILERS (INCLUDING BOUNDARY LAYER):
C  50 m increments starting at 26 m and ending at 6025 m (120 levels)
C  (height above ground)
C
C JAPANESE METEOROLOGICAL AGENCY PROFILERS:
C  300 m increments starting at 251 m and ending at 9250 m (30 levels)
C  (height above ground)
C
C PROFILERS DECODED FROM PILOT (PIBAL) FORMAT BULLETINS:
C  Currently no statistics are produced

C     COLLECT STATISTICS:
C       EXAMPLE-- UZS(LEVEL,HOUR,STATISTIC)
C       LEVEL     - 1 TO NZZ(IS)
C       HOUR      - 1 TO NTIMES(IS), WHERE NTIMES(IS) IS FOR ALL HOURS
C                                    FOR STATION "IS"
C       STATISTIC - 1: COUNT
C                   2: SUM
C                   3: SUM OF SQUARED VALUE
C                   4: SUM OF CUBED VALUE
C                   5: SUM OF VALUE TO 4TH POWER
C                   6: MEAN
C                   7: STANDARD DEVIATION
C                   8: SKEWNESS
C                   9: KURTOSIS
C     NOTE! STATISTICS 6-9 ARE NOT CALCULATED UNTIL ALL
C           VALUES OF STATISTICS 1-5 ARE COLLECTED FOR ALL STATIONS.
C           THAT CALCULATION IS PERFORMED IN PSTAT.

C What type of station is this? (=3-NPN, =7-CAP, =8=JMA)
      ITYPE = MOD(IRTP(IS),10)
      IF(ITYPE.LT.3.OR.ITYPE.GT.8)  THEN
        write(61,*)
        write(61,'("Stn. ",A," does not have a valid report type (typ=",
     $   I0,") -- it will not be included in statistical database")')
     $  STN(IS),irtp(is)
        write(61,*)
        RETURN
      ENDIF

      LOOP1: DO NN=1,NTIMES(IS)
        IF(KDATE(5,NN,IS).NE.0) THEN
          write(61,*)
          write(61,'("Stn. ",A," at d-time ",F5.2,"is not reporting on",
     $     " the hour -- this time will not be included in statistical",
     $     "database")') STN(IS),TIM(NN,IS)
          write(61,*)
          CYCLE LOOP1
        ENDIF
        N = KDATE(4,NN,IS)
        IF(N.LT.0.OR.N.GT.23) THEN
          write(61,*)
          write(61,'("Stn. ",A," at d-time ",F5.2,"is reporting an ",
     $    "invalid hour -- this time will not be included in ",
     $    "statistical database")') STN(IS),TIM(NN,IS)
          write(61,*)
          CYCLE LOOP1
        ENDIF
        IF(NINT(TIM(NN,IS)*1000.).LT.NINT(TIMWIN_E*1000.).OR.
     $     NINT(TIM(NN,IS)*1000.).GT.NINT(TIMWIN_L*1000.)) CYCLE LOOP1
        LOOP1n1: DO LL=1,NZZ(IS)
          L = NINT((ZZ(LL,NN,IS)-Z0(IS)-IOFFSET(ITYPE)-0.5)/
     $     LVLINCR(ITYPE))
          IF(L.LT.0.OR.L.GT.ILEVELS(ITYPE)) THEN
            write(61,*)
            write(61,'("Stn. ",A," at time ",I0,"Z is reporting a ",
     $       "height above ground outside expected range (hght=",I0,
     $       ") -- this level will not be included in statistical ",
     $       "database")') STN(IS),N,NINT(ZZ(LL,NN,IS)-Z0(IS))
            write(61,*)
            CYCLE LOOP1n1
          ENDIF
          IF(IQ(LL,NN,IS).GT.3.OR.ICQC(LL,NN,IS).GT.1) CYCLE LOOP1n1
          IF(MAX(ABS(UZ(LL,NN,IS)),ABS(VZ(LL,NN,IS))).LT.BMISS) THEN
            UZS(L,N,1,ITYPE)  = UZS(L,N,1,ITYPE)  + 1
            UZS(L,N,2,ITYPE)  = UZS(L,N,2,ITYPE)  + UZ(LL,NN,IS)
            UZS(L,N,3,ITYPE)  = UZS(L,N,3,ITYPE)  + UZ(LL,NN,IS)**2
            UZS(L,N,4,ITYPE)  = UZS(L,N,4,ITYPE)  + UZ(LL,NN,IS)**3
            UZS(L,N,5,ITYPE)  = UZS(L,N,5,ITYPE)  + UZ(LL,NN,IS)**4
            VZS(L,N,1,ITYPE)  = VZS(L,N,1,ITYPE)  + 1
            VZS(L,N,2,ITYPE)  = VZS(L,N,2,ITYPE)  + VZ(LL,NN,IS)
            VZS(L,N,3,ITYPE)  = VZS(L,N,3,ITYPE)  + VZ(LL,NN,IS)**2
            VZS(L,N,4,ITYPE)  = VZS(L,N,4,ITYPE)  + VZ(LL,NN,IS)**3
            VZS(L,N,5,ITYPE)  = VZS(L,N,5,ITYPE)  + VZ(LL,NN,IS)**4
            UZS(L,24,1,ITYPE) = UZS(L,24,1,ITYPE) + 1
            UZS(L,24,2,ITYPE) = UZS(L,24,2,ITYPE) + UZ(LL,NN,IS)
            UZS(L,24,3,ITYPE) = UZS(L,24,3,ITYPE) + UZ(LL,NN,IS)**2
            UZS(L,24,4,ITYPE) = UZS(L,24,4,ITYPE) + UZ(LL,NN,IS)**3
            UZS(L,24,5,ITYPE) = UZS(L,24,5,ITYPE) + UZ(LL,NN,IS)**4
            VZS(L,24,1,ITYPE) = VZS(L,24,1,ITYPE) + 1
            VZS(L,24,2,ITYPE) = VZS(L,24,2,ITYPE) + VZ(LL,NN,IS)
            VZS(L,24,3,ITYPE) = VZS(L,24,3,ITYPE) + VZ(LL,NN,IS)**2
            VZS(L,24,4,ITYPE) = VZS(L,24,4,ITYPE) + VZ(LL,NN,IS)**3
            VZS(L,24,5,ITYPE) = VZS(L,24,5,ITYPE) + VZ(LL,NN,IS)**4
          ENDIF
          IF(MAX(ABS(UI(LL,NN,IS)),ABS(VI(LL,NN,IS))).LT.BMISS) THEN
            UIS(L,N,1,ITYPE)  = UIS(L,N,1,ITYPE)  + 1
            UIS(L,N,2,ITYPE)  = UIS(L,N,2,ITYPE)  + UI(LL,NN,IS)
            UIS(L,N,3,ITYPE)  = UIS(L,N,3,ITYPE)  + UI(LL,NN,IS)**2
            UIS(L,N,4,ITYPE)  = UIS(L,N,4,ITYPE)  + UI(LL,NN,IS)**3
            UIS(L,N,5,ITYPE)  = UIS(L,N,5,ITYPE)  + UI(LL,NN,IS)**4
            VIS(L,N,1,ITYPE)  = VIS(L,N,1,ITYPE)  + 1
            VIS(L,N,2,ITYPE)  = VIS(L,N,2,ITYPE)  + VI(LL,NN,IS)
            VIS(L,N,3,ITYPE)  = VIS(L,N,3,ITYPE)  + VI(LL,NN,IS)**2
            VIS(L,N,4,ITYPE)  = VIS(L,N,4,ITYPE)  + VI(LL,NN,IS)**3
            VIS(L,N,5,ITYPE)  = VIS(L,N,5,ITYPE)  + VI(LL,NN,IS)**4
            UIS(L,24,1,ITYPE) = UIS(L,24,1,ITYPE) + 1
            UIS(L,24,2,ITYPE) = UIS(L,24,2,ITYPE) + UI(LL,NN,IS)
            UIS(L,24,3,ITYPE) = UIS(L,24,3,ITYPE) + UI(LL,NN,IS)**2
            UIS(L,24,4,ITYPE) = UIS(L,24,4,ITYPE) + UI(LL,NN,IS)**3
            UIS(L,24,5,ITYPE) = UIS(L,24,5,ITYPE) + UI(LL,NN,IS)**4
            VIS(L,24,1,ITYPE) = VIS(L,24,1,ITYPE) + 1
            VIS(L,24,2,ITYPE) = VIS(L,24,2,ITYPE) + VI(LL,NN,IS)
            VIS(L,24,3,ITYPE) = VIS(L,24,3,ITYPE) + VI(LL,NN,IS)**2
            VIS(L,24,4,ITYPE) = VIS(L,24,4,ITYPE) + VI(LL,NN,IS)**3
            VIS(L,24,5,ITYPE) = VIS(L,24,5,ITYPE) + VI(LL,NN,IS)**4
          ENDIF
          IF(MAX(ABS(UT(LL,NN)),ABS(VT(LL,NN))).LT.BMISS) THEN
            UTS(L,N,1,ITYPE)  = UTS(L,N,1,ITYPE)  + 1
            UTS(L,N,2,ITYPE)  = UTS(L,N,2,ITYPE)  + UT(LL,NN)
            UTS(L,N,3,ITYPE)  = UTS(L,N,3,ITYPE)  + UT(LL,NN)**2
            UTS(L,N,4,ITYPE)  = UTS(L,N,4,ITYPE)  + UT(LL,NN)**3
            UTS(L,N,5,ITYPE)  = UTS(L,N,5,ITYPE)  + UT(LL,NN)**4
            VTS(L,N,1,ITYPE)  = VTS(L,N,1,ITYPE)  + 1
            VTS(L,N,2,ITYPE)  = VTS(L,N,2,ITYPE)  + VT(LL,NN)
            VTS(L,N,3,ITYPE)  = VTS(L,N,3,ITYPE)  + VT(LL,NN)**2
            VTS(L,N,4,ITYPE)  = VTS(L,N,4,ITYPE)  + VT(LL,NN)**3
            VTS(L,N,5,ITYPE)  = VTS(L,N,5,ITYPE)  + VT(LL,NN)**4
            UTS(L,24,1,ITYPE) = UTS(L,24,1,ITYPE) + 1
            UTS(L,24,2,ITYPE) = UTS(L,24,2,ITYPE) + UT(LL,NN)
            UTS(L,24,3,ITYPE) = UTS(L,24,3,ITYPE) + UT(LL,NN)**2
            UTS(L,24,4,ITYPE) = UTS(L,24,4,ITYPE) + UT(LL,NN)**3
            UTS(L,24,5,ITYPE) = UTS(L,24,5,ITYPE) + UT(LL,NN)**4
            VTS(L,24,1,ITYPE) = VTS(L,24,1,ITYPE) + 1
            VTS(L,24,2,ITYPE) = VTS(L,24,2,ITYPE) + VT(LL,NN)
            VTS(L,24,3,ITYPE) = VTS(L,24,3,ITYPE) + VT(LL,NN)**2
            VTS(L,24,4,ITYPE) = VTS(L,24,4,ITYPE) + VT(LL,NN)**3
            VTS(L,24,5,ITYPE) = VTS(L,24,5,ITYPE) + VT(LL,NN)**4
          ENDIF
          IF(MAX(ABS(UV(LL,NN)),ABS(VV(LL,NN))).LT.BMISS) THEN
            UVS(L,N,1,ITYPE)  = UVS(L,N,1,ITYPE)  + 1
            UVS(L,N,2,ITYPE)  = UVS(L,N,2,ITYPE)  + UV(LL,NN)
            UVS(L,N,3,ITYPE)  = UVS(L,N,3,ITYPE)  + UV(LL,NN)**2
            UVS(L,N,4,ITYPE)  = UVS(L,N,4,ITYPE)  + UV(LL,NN)**3
            UVS(L,N,5,ITYPE)  = UVS(L,N,5,ITYPE)  + UV(LL,NN)**4
            VVS(L,N,1,ITYPE)  = VVS(L,N,1,ITYPE)  + 1
            VVS(L,N,2,ITYPE)  = VVS(L,N,2,ITYPE)  + VV(LL,NN)
            VVS(L,N,3,ITYPE)  = VVS(L,N,3,ITYPE)  + VV(LL,NN)**2
            VVS(L,N,4,ITYPE)  = VVS(L,N,4,ITYPE)  + VV(LL,NN)**3
            VVS(L,N,5,ITYPE)  = VVS(L,N,5,ITYPE)  + VV(LL,NN)**4
            UVS(L,24,1,ITYPE) = UVS(L,24,1,ITYPE) + 1
            UVS(L,24,2,ITYPE) = UVS(L,24,2,ITYPE) + UV(LL,NN)
            UVS(L,24,3,ITYPE) = UVS(L,24,3,ITYPE) + UV(LL,NN)**2
            UVS(L,24,4,ITYPE) = UVS(L,24,4,ITYPE) + UV(LL,NN)**3
            UVS(L,24,5,ITYPE) = UVS(L,24,5,ITYPE) + UV(LL,NN)**4
            VVS(L,24,1,ITYPE) = VVS(L,24,1,ITYPE) + 1
            VVS(L,24,2,ITYPE) = VVS(L,24,2,ITYPE) + VV(LL,NN)
            VVS(L,24,3,ITYPE) = VVS(L,24,3,ITYPE) + VV(LL,NN)**2
            VVS(L,24,4,ITYPE) = VVS(L,24,4,ITYPE) + VV(LL,NN)**3
            VVS(L,24,5,ITYPE) = VVS(L,24,5,ITYPE) + VV(LL,NN)**4
          ENDIF
          IF(MAX(ABS(UM(LL,NN)),ABS(VM(LL,NN))).LT.BMISS) THEN
            UMS(L,N,1,ITYPE)  = UMS(L,N,1,ITYPE)  + 1
            UMS(L,N,2,ITYPE)  = UMS(L,N,2,ITYPE)  + UM(LL,NN)
            UMS(L,N,3,ITYPE)  = UMS(L,N,3,ITYPE)  + UM(LL,NN)**2
            UMS(L,N,4,ITYPE)  = UMS(L,N,4,ITYPE)  + UM(LL,NN)**3
            UMS(L,N,5,ITYPE)  = UMS(L,N,5,ITYPE)  + UM(LL,NN)**4
            VMS(L,N,1,ITYPE)  = VMS(L,N,1,ITYPE)  + 1
            VMS(L,N,2,ITYPE)  = VMS(L,N,2,ITYPE)  + VM(LL,NN)
            VMS(L,N,3,ITYPE)  = VMS(L,N,3,ITYPE)  + VM(LL,NN)**2
            VMS(L,N,4,ITYPE)  = VMS(L,N,4,ITYPE)  + VM(LL,NN)**3
            VMS(L,N,5,ITYPE)  = VMS(L,N,5,ITYPE)  + VM(LL,NN)**4
            UMS(L,24,1,ITYPE) = UMS(L,24,1,ITYPE) + 1
            UMS(L,24,2,ITYPE) = UMS(L,24,2,ITYPE) + UM(LL,NN)
            UMS(L,24,3,ITYPE) = UMS(L,24,3,ITYPE) + UM(LL,NN)**2
            UMS(L,24,4,ITYPE) = UMS(L,24,4,ITYPE) + UM(LL,NN)**3
            UMS(L,24,5,ITYPE) = UMS(L,24,5,ITYPE) + UM(LL,NN)**4
            VMS(L,24,1,ITYPE) = VMS(L,24,1,ITYPE) + 1
            VMS(L,24,2,ITYPE) = VMS(L,24,2,ITYPE) + VM(LL,NN)
            VMS(L,24,3,ITYPE) = VMS(L,24,3,ITYPE) + VM(LL,NN)**2
            VMS(L,24,4,ITYPE) = VMS(L,24,4,ITYPE) + VM(LL,NN)**3
            VMS(L,24,5,ITYPE) = VMS(L,24,5,ITYPE) + VM(LL,NN)**4
          ENDIF
          IF(ABS(VVI(LL,NN)).LT.BMISS) THEN
            VVIS(L,N,1,ITYPE)  = VVIS(L,N,1,ITYPE)  + 1
            VVIS(L,N,2,ITYPE)  = VVIS(L,N,2,ITYPE)  + VVI(LL,NN)
            VVIS(L,N,3,ITYPE)  = VVIS(L,N,3,ITYPE)  + VVI(LL,NN)**2
            VVIS(L,N,4,ITYPE)  = VVIS(L,N,4,ITYPE)  + VVI(LL,NN)**3
            VVIS(L,N,5,ITYPE)  = VVIS(L,N,5,ITYPE)  + VVI(LL,NN)**4
            VVIS(L,24,1,ITYPE) = VVIS(L,24,1,ITYPE) + 1
            VVIS(L,24,2,ITYPE) = VVIS(L,24,2,ITYPE) + VVI(LL,NN)
            VVIS(L,24,3,ITYPE) = VVIS(L,24,3,ITYPE) + VVI(LL,NN)**2
            VVIS(L,24,4,ITYPE) = VVIS(L,24,4,ITYPE) + VVI(LL,NN)**3
            VVIS(L,24,5,ITYPE) = VVIS(L,24,5,ITYPE) + VVI(LL,NN)**4
          ENDIF
          IF(ABS(VVT(LL,NN)).LT.BMISS) THEN
            VVTS(L,N,1,ITYPE)  = VVTS(L,N,1,ITYPE)  + 1
            VVTS(L,N,2,ITYPE)  = VVTS(L,N,2,ITYPE)  + VVT(LL,NN)
            VVTS(L,N,3,ITYPE)  = VVTS(L,N,3,ITYPE)  + VVT(LL,NN)**2
            VVTS(L,N,4,ITYPE)  = VVTS(L,N,4,ITYPE)  + VVT(LL,NN)**3
            VVTS(L,N,5,ITYPE)  = VVTS(L,N,5,ITYPE)  + VVT(LL,NN)**4
            VVTS(L,24,1,ITYPE) = VVTS(L,24,1,ITYPE) + 1
            VVTS(L,24,2,ITYPE) = VVTS(L,24,2,ITYPE) + VVT(LL,NN)
            VVTS(L,24,3,ITYPE) = VVTS(L,24,3,ITYPE) + VVT(LL,NN)**2
            VVTS(L,24,4,ITYPE) = VVTS(L,24,4,ITYPE) + VVT(LL,NN)**3
            VVTS(L,24,5,ITYPE) = VVTS(L,24,5,ITYPE) + VVT(LL,NN)**4
          ENDIF
          IF(ABS(VVV(LL,NN)).LT.BMISS) THEN
            VVVS(L,N,1,ITYPE)  = VVVS(L,N,1,ITYPE)  + 1
            VVVS(L,N,2,ITYPE)  = VVVS(L,N,2,ITYPE)  + VVV(LL,NN)
            VVVS(L,N,3,ITYPE)  = VVVS(L,N,3,ITYPE)  + VVV(LL,NN)**2
            VVVS(L,N,4,ITYPE)  = VVVS(L,N,4,ITYPE)  + VVV(LL,NN)**3
            VVVS(L,N,5,ITYPE)  = VVVS(L,N,5,ITYPE)  + VVV(LL,NN)**4
            VVVS(L,24,1,ITYPE) = VVVS(L,24,1,ITYPE) + 1
            VVVS(L,24,2,ITYPE) = VVVS(L,24,2,ITYPE) + VVV(LL,NN)
            VVVS(L,24,3,ITYPE) = VVVS(L,24,3,ITYPE) + VVV(LL,NN)**2
            VVVS(L,24,4,ITYPE) = VVVS(L,24,4,ITYPE) + VVV(LL,NN)**3
            VVVS(L,24,5,ITYPE) = VVVS(L,24,5,ITYPE) + VVV(LL,NN)**4
          ENDIF
          IF(ABS(VVM(LL,NN)).LT.BMISS) THEN
            VVMS(L,N,1,ITYPE)  = VVMS(L,N,1,ITYPE)  + 1
            VVMS(L,N,2,ITYPE)  = VVMS(L,N,2,ITYPE)  + VVM(LL,NN)
            VVMS(L,N,3,ITYPE)  = VVMS(L,N,3,ITYPE)  + VVM(LL,NN)**2
            VVMS(L,N,4,ITYPE)  = VVMS(L,N,4,ITYPE)  + VVM(LL,NN)**3
            VVMS(L,N,5,ITYPE)  = VVMS(L,N,5,ITYPE)  + VVM(LL,NN)**4
            VVMS(L,24,1,ITYPE) = VVMS(L,24,1,ITYPE) + 1
            VVMS(L,24,2,ITYPE) = VVMS(L,24,2,ITYPE) + VVM(LL,NN)
            VVMS(L,24,3,ITYPE) = VVMS(L,24,3,ITYPE) + VVM(LL,NN)**2
            VVMS(L,24,4,ITYPE) = VVMS(L,24,4,ITYPE) + VVM(LL,NN)**3
            VVMS(L,24,5,ITYPE) = VVMS(L,24,5,ITYPE) + VVM(LL,NN)**4
          ENDIF
        ENDDO  LOOP1n1
      ENDDO  LOOP1

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    DIFS
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: COMPUTE DIFFERENCES OF RESIDUALS, USED IN DMA.
C
C PROGRAM HISTORY LOG:
C 1995-04-04  W. Collins -- Original author.
C 2004-09-09  D. Keyser  -- Excessive stdout print of differencing
C     results can be removed w/ new namelist switch which controls the
C     degree of printout.
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL DIFS(IS,IPRINT)
C
C   INPUT ARGUMENT LIST:
C     IS       - STATION INDEX
C     IPRINT   - =1 INCLUDE ADDITIONAL DIAGNOSTIC PRINT IN STDOUT,
C                ELSE = 0
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS: 
C   VARIABLE NAMES:
C     UI       - U INCREMENT
C     VI       - V INCREMENT
C     UT       - U-COMPONENT TEMPORAL RESIDUAL
C     VT       - V-COMPONENT TEMPORAL RESIDUAL
C     UV       - U-COMPONENT VERTICAL RESIDUAL
C     VV       - V-COMPONENT VERTICAL RESIDUAL
C     UM       - U-COMPONENT MEDIAN RESIDUAL
C     VM       - V-COMPONENT MEDIAN RESIDUAL
C     VVI      - VECTOR INCREMENT
C     VVT      - VECTOR TEMPORAL RESIDUAL
C     VVM      - VECTOR MEDIAN RESIDUAL
C     VVV      - VECTOR VERTICAL RESIDUAL
C     DIT      - VECTOR DIFFERENCE: INCREMENT/TEMPORAL
C     DIV      - VECTOR DIFFERENCE: INCREMENT/VERTICAL
C     DTV      - VECTOR DIFFERENCE: TEMPORAL/VERTICAL
C     DIM      - VECTOR DIFFERENCE: INCREMENT/MEDIAN
C     DVM      - VECTOR DIFFERENCE: VERTICAL/MEDIAN
C     DTM      - VECTOR DIFFERENCE: TEMPORAL/MEDIAN
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE DIFS(IS,IPRINT)

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      CHARACTER*8  STN
      REAL(8)   BMISS

      COMMON/RESIDS/UI(NL,NT,NS),VI(NL,NT,NS),UT(NL,NT),VT(NL,NT),
     $ UV(NL,NT),VV(NL,NT),VVI(NL,NT),VVT(NL,NT),VVV(NL,NT),UM(NL,NT),
     $ VM(NL,NT),VVM(NL,NT),NI(NL,NT),NTMP(NL,NT),NV(NL,NT),NM(NL,NT),
     $ ZI(NL,NT),ZT(NL,NT),ZV(NL,NT),ZM(NL,NT),DIT(NL,NT),DIV(NL,NT),
     $ DTV(NL,NT),DIM(NL,NT),DVM(NL,NT),DTM(NL,NT)
      COMMON/QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON/LIMITS/XI(9124),XT(15750),XV(15750)
      COMMON /STNLST/ STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON /BUFRLIB_MISSING/BMISS

C     SPECIFY DIT,DIV,DTV,DIM,DVM,DTM -- MEASURES OF DIFFERENCES
C     OF THE RESIDUALS.

      DO J=1,NTIMES(IS)
        DO I=1,NZZ(IS)
          IF(IPRINT.EQ.1) THEN
            IF(MIN(VVI(I,J),VVT(I,J),VVV(I,J),VVM(I,J)).LT.BMISS) THEN
               WRITE(6,100)NSCAN,J,I,VVI(I,J),VVT(I,J),VVV(I,J),VVM(I,J)
            ELSE
               WRITE(6,101)NSCAN,J,I,VVI(I,J),VVT(I,J),VVV(I,J),VVM(I,J)
            ENDIF
          ENDIF
          IF(MAX(UI(I,J,IS),VI(I,J,IS),UT(I,J),VT(I,J)).LT.BMISS
     $      .AND. VVI(I,J)+VVT(I,J).NE.0.) THEN
            DIT(I,J) = SQRT((UI(I,J,IS) - UT(I,J))**2
     $                    + (VI(I,J,IS) - VT(I,J))**2)
     $                / (0.5*(VVI(I,J) + VVT(I,J)))
          ELSE
            DIT(I,J) = BMISS
          ENDIF
          IF(MAX(UI(I,J,IS),VI(I,J,IS),UV(I,J),VV(I,J)).LT.BMISS
     $      .AND. VVI(I,J)+VVV(I,J).NE.0.) THEN
            DIV(I,J) = SQRT((UI(I,J,IS) - UV(I,J))**2
     $                    + (VI(I,J,IS) - VV(I,J))**2)
     $                / (0.5*(VVI(I,J) + VVV(I,J)))
          ELSE
            DIV(I,J) = BMISS
          ENDIF
          IF(MAX(UT(I,J),VT(I,J),UV(I,J),VV(I,J)).LT.BMISS
     $      .AND. VVT(I,J)+VVV(I,J).NE.0.) THEN
            DTV(I,J) = SQRT((UT(I,J) - UV(I,J))**2
     $                    + (VT(I,J) - VV(I,J))**2)
     $                / (0.5*(VVT(I,J) + VVV(I,J)))
          ELSE
            DTV(I,J) = BMISS
          ENDIF
          IF(MAX(UI(I,J,IS),VI(I,J,IS),UM(I,J),VM(I,J)).LT.BMISS
     $      .AND. VVI(I,J)+VVM(I,J).NE.0.) THEN
            DIM(I,J) = SQRT((UI(I,J,IS) - UM(I,J))**2
     $                    + (VI(I,J,IS) - VM(I,J))**2)
     $                / (0.5*(VVI(I,J) + VVM(I,J)))
          ELSE
            DIM(I,J) = BMISS
          ENDIF
          IF(MAX(UV(I,J),VV(I,J),UM(I,J),VM(I,J)).LT.BMISS
     $      .AND. VVV(I,J)+VVM(I,J).NE.0.) THEN
            DVM(I,J) = SQRT((UV(I,J) - UM(I,J))**2
     $                    + (VV(I,J) - VM(I,J))**2)
     $                / (0.5*(VVV(I,J) + VVM(I,J)))
          ELSE
            DVM(I,J) = BMISS
          ENDIF
          IF(MAX(UT(I,J),VT(I,J),UM(I,J),VM(I,J)).LT.BMISS
     $      .AND. VVT(I,J)+VVM(I,J).NE.0.) THEN
            DTM(I,J) = SQRT((UT(I,J) - UM(I,J))**2
     $                    + (VT(I,J) - VM(I,J))**2)
     $                / (0.5*(VVT(I,J) + VVM(I,J)))
          ELSE
            DTM(I,J) = BMISS
          ENDIF
        ENDDO
      ENDDO

      RETURN

  100 FORMAT(' DIFS: scan=',I2,', time period=',I2,', lvl=',I3,
     $ '; vector: incr=',F6.2,', temporal resid=',F6.2,', vertical ',
     $ 'resid=',F6.2,', median resid=',F6.2)
  101 FORMAT('~DIFS: scan=',I2,', time period=',I2,', lvl=',I3,
     $ '; vector: incr=',F6.2,', temporal resid=',F6.2,', vertical ',
     $ 'resid=',F6.2,', median resid=',F6.2)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    DMA
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: DECISION MAKING ALGORITHM FOR PROFILER WIND QC.
C
C PROGRAM HISTORY LOG:
C 1995-04-05  W. Collins -- Original author.
C 2004-09-09  D. Keyser  -- Reason codes are expanded (see MAIN program
C     remarks).
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL DMA(IS)
C   INPUT ARGUMENT LIST:
C     IS       - STATION INDEX
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS: 
C   VARIABLE NAMES:
C     ZI       - NORMALIZED VECTOR INCREMENT
C     ZT       - NORMALIZED VECTOR TEMPORAL RESIDUAL
C     ZV       - NORMALIZED VECTOR VERTICAL RESIDUAL
C     ZM       - NORMALIZED VECTOR MEDIAN RESIDUAL
C     DIT      - VECTOR DIFFERENCE: INCREMENT/TEMPORAL
C     DIV      - VECTOR DIFFERENCE: INCREMENT/VERTICAL
C     DTV      - VECTOR DIFFERENCE: TEMPORAL/VERTICAL
C     DIM      - VECTOR DIFFERENCE: INCREMENT/MEDIAN
C     DVM      - VECTOR DIFFERENCE: VERTICAL/MEDIAN
C     DTM      - VECTOR DIFFERENCE: TEMPORAL/MEDIAN
C     ICQC     - DMA INDEX FOR WIND QUALITY CONTROL
C     NCH      - NUMBER OF CHANGED QUALITY MARKS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE DMA(IS)

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      CHARACTER*8  STN,CID
      REAL(8)   BMISS

      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON/RESIDS/UI(NL,NT,NS),VI(NL,NT,NS),UT(NL,NT),VT(NL,NT),
     $ UV(NL,NT),VV(NL,NT),VVI(NL,NT),VVT(NL,NT),VVV(NL,NT),UM(NL,NT),
     $ VM(NL,NT),VVM(NL,NT),NI(NL,NT),NTMP(NL,NT),NV(NL,NT),NM(NL,NT),
     $ ZI(NL,NT),ZT(NL,NT),ZV(NL,NT),ZM(NL,NT),DIT(NL,NT),DIV(NL,NT),
     $ DTV(NL,NT),DIM(NL,NT),DVM(NL,NT),DTM(NL,NT)
      COMMON/PROFZ/ZZ(NL,NT,NS),UZ(NL,NT,NS),VZ(NL,NT,NS),QZ(NL,NT,NS),
     $ PZ(NL,NT,NS),UFZ(NL,NT,NS),VFZ(NL,NT,NS),SZ(NL,NT,NS),
     $ DZ(NL,NT,NS),LEVZ(NL,NT,NS)
      COMMON/QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON/LIMITS/XI(9125),XT(15750),XV(15750)
      COMMON /BUFRLIB_MISSING/BMISS

      DATA  R11/8.0/,R12/3.0/,R2/0.4/,R31/4.0/,R32 /1.5/

      CID = STN(IS)

C     DETERMINE FLAGS FOR QUESTIONABLE AND BAD DATA.
C     SET ICQC TO:
C       0 IF DATA ARE GOOD
C       1 IF DATA ARE QUESTIONABLE
C       2 IF DATA ARE BAD
C     ALSO RETURN IN NCH THE NUMBER OF DECISIONS THAT CHANGE.

      NCH = 0
      DO N=1,NTIMES(IS)
        DO L=1,NZZ(IS)
          ICQCO = ICQC(L,N,IS)

C         FIND AVERAGE VECTOR RESIDUAL MAGNITUDE.

          SUM = 0.
          NRES = 0
          RESMN = 0.
          IF(ZI(L,N).LT.BMISS) THEN
            NRES = NRES + 1
            SUM = SUM + ZI(L,N)
          ENDIF
          IF(ZT(L,N).LT.BMISS) THEN
            NRES = NRES + 1
            SUM = SUM + ZT(L,N)
          ENDIF
          IF(ZV(L,N).LT.BMISS) THEN
            NRES = NRES + 1
            SUM = SUM + ZV(L,N)
          ENDIF
          IF(ZM(L,N).LT.BMISS) THEN
            NRES = NRES + 1
            SUM = SUM + ZM(L,N)
          ENDIF
          IF(NRES.GT.0) THEN
            RESMN = SUM / NRES
          ENDIF

C         FIND AVERAGE VECTOR RESIDUAL DIFFERENCE.

          SUM = 0.
          NRD = 0
          RESDM = 0.
          IF(DIT(L,N).LT.BMISS) THEN
            NRD = NRD + 1
            SUM = SUM + DIT(L,N)
          ENDIF
          IF(DIV(L,N).LT.BMISS) THEN
            NRD = NRD + 1
            SUM = SUM + DIV(L,N)
          ENDIF
          IF(DTV(L,N).LT.BMISS) THEN
            NRD = NRD + 1
            SUM = SUM + DTV(L,N)
          ENDIF
          IF(DIM(L,N).LT.BMISS) THEN
            NRD = NRD + 1
            SUM = SUM + DIM(L,N)
          ENDIF
          IF(DVM(L,N).LT.BMISS) THEN
            NRD = NRD + 1
            SUM = SUM + DVM(L,N)
          ENDIF
          IF(DTM(L,N).LT.BMISS) THEN
            NRD = NRD + 1
            SUM = SUM + DTM(L,N)
          ENDIF
          IF(NRD.GT.0) THEN
            RESDM = SUM / NRD
          ENDIF

C         FOR AN ERROR, REQUIRE RESMN LARGE AND RESDM TO BE SMALL.

          Z1 = 10000.
          Z2 = 13000.
          IF(ZZ(L,N,IS).LE.Z1) THEN ! ZZ .le. 10000 m
            R1 = R11
            R3 = R31
          ELSE IF(ZZ(L,N,IS).GE.Z2) THEN ! ZZ .ge. 13000 m
            R1 = R12
            R3 = R32
          ELSE ! ZZ between 10000 and 13000 m
            R1 = ((Z2-ZZ(L,N,IS))*R11 + (ZZ(L,N,IS)-Z1)*R12)/(Z2-Z1)
            R3 = ((Z2-ZZ(L,N,IS))*R31 + (ZZ(L,N,IS)-Z1)*R32)/(Z2-Z1)
          ENDIF
          K1 = 0
          K2 = 0
          K3 = 0
          ITOT = 0
          IDIT = 0
          IDIV = 0
          IDIM = 0
          IDTV = 0
          IDTM = 0
          IDVM = 0
          IF(RESMN.GT.R1 .AND. RESDM.LT.R2  .OR.
     $      (RESMN.GT.R1 .AND. NRD.EQ.1)) THEN ! Bad
             INEW = 2
            IF(NRD.EQ.1.AND.RESDM.GE.R2) THEN
              K3 = 1
            ELSE
              IF(DIT(L,N).LT.R2) THEN
                IDIT = 1
                ITOT = ITOT + 1
              ENDIF
              IF(DIV(L,N).LT.R2) THEN
                IDIV = 1
                ITOT = ITOT + 1
              ENDIF
              IF(DIM(L,N).LT.R2) THEN
                IDIM = 1
                ITOT = ITOT + 1
              ENDIF
              IF(DTV(L,N).LT.R2) THEN
                IDTV = 1
                ITOT = ITOT + 1
              ENDIF
              IF(DTM(L,N).LT.R2) THEN
                IDTM = 1
                ITOT = ITOT + 1
              ENDIF
              IF(DVM(L,N).LT.R2) THEN
                IDVM = 1
                ITOT = ITOT + 1
              ENDIF
              IF(ITOT.GE.4) THEN
                K3 = 9
              ELSE IF(ITOT.GE.2) THEN
                K3 = 8
              ELSE IF(ITOT.EQ.1) THEN
                IF(IDIT.EQ.1) THEN
                  K3 = 2
                ELSE IF(IDIV.EQ.1) THEN
                  K3 = 3
                ELSE IF(IDIM.EQ.1) THEN
                  K3 = 4
                ELSE IF(IDTV.EQ.1) THEN
                  K3 = 5
                ELSE IF(IDTM.EQ.1) THEN
                  K3 = 6
                ELSE IF(IDVM.EQ.1) THEN
                  K3 = 7
                ENDIF
              ENDIF
            ENDIF
            K1 = 1
            IF(MAX(ZM(L,N),ZV(L,N)).LT.BMISS.AND.MIN(ZM(L,N),ZV(L,N))
     $       .GT.R1) THEN
              K1 = 7
            ELSE IF(ZV(L,N).GT.R1.AND.ZV(L,N).LT.BMISS) THEN
              K1 = 6
            ELSE IF(ZM(L,N).GT.R1.AND.ZM(L,N).LT.BMISS) THEN
              K1 = 5
            ENDIF
            K2 = 1
            IF(MAX(ZT(L,N),ZI(L,N)).LT.BMISS.AND.MIN(ZT(L,N),ZI(L,N))
     $       .GT.R1) THEN
              K2 = 7
            ELSE IF(ZI(L,N).GT.R1.AND.ZI(L,N).LT.BMISS) THEN
              K2 = 6
            ELSE IF(ZT(L,N).GT.R1.AND.ZT(L,N).LT.BMISS) THEN
              K2 = 5
            ENDIF
          ELSE IF(RESMN.GT.R3) THEN
            INEW = 1 ! Suspect
            K1 = 1
            IF(MAX(ZM(L,N),ZV(L,N)).LT.BMISS.AND.MIN(ZM(L,N),ZV(L,N))
     $       .GT.R3) THEN
              K1 = 4
            ELSE IF(ZV(L,N).GT.R3.AND.ZV(L,N).LT.BMISS) THEN
              K1 = 3
            ELSE IF(ZM(L,N).GT.R3.AND.ZM(L,N).LT.BMISS) THEN
              K1 = 2
            ENDIF
            K2 = 1
            IF(MAX(ZT(L,N),ZI(L,N)).LT.BMISS.AND.MIN(ZT(L,N),ZI(L,N))
     $       .GT.R3) THEN
              K2 = 4
            ELSE IF(ZI(L,N).GT.R3.AND.ZI(L,N).LT.BMISS) THEN
              K2 = 3
            ELSE IF(ZT(L,N).GT.R3.AND.ZT(L,N).LT.BMISS) THEN
              K2 = 2
            ENDIF
          ELSE
            INEW = 0 ! Passed checks (good)
          ENDIF
          ICQC(L,N,IS) = INEW
          IF(INEW.GT.0)  IRC(L,N,IS) = K1*100 + K2*10 + K3

C         CHECK FOR DECISION CHANGES

          IF(ICQCO.NE.ICQC(L,N,IS)) NCH = NCH + 1
        ENDDO
      ENDDO

      WRITE(6,100) NSCAN,NCH

      RETURN

  100 FORMAT('--> DMA: For scan',I3,' the total number of q.m. changes',
     $ ' is',I4)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    EVENT
C   PRGMMR: J. WOOLLEN       ORG: NP20       DATE: 1994-??-??
C
C ABSTRACT: WRITE OUT EVENTS.
C
C PROGRAM HISTORY LOG:
C 1994-??-??  J. WOOLLEN -- Original author.
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
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE EVENT(LUNIT,EVNSTR,NLV,OBS,QMS,RCS,IND,NEVN,QCPC)

      CHARACTER*(*)  EVNSTR
      REAL(8)  EVNS_8(5,255),BMISS
      DIMENSION  OBS(255,2),QMS(NEVN),RCS(NEVN),IND(NEVN)

      COMMON /BUFRLIB_MISSING/BMISS

      IF(NEVN.EQ.0) THEN
         WRITE(6,100)
         RETURN
      ENDIF

C  CLEAR THE UFB ARRAY FIRST
C  -------------------------

      EVNS_8 = BMISS

C  TRANSFER EVENT ARRAYS INTO UFB ARRAY
C  ------------------------------------

      DO I=1,NEVN
         J = IND(I)
         IF(MIN(OBS(I,1),OBS(I,2)).LT.BMISS) THEN
            EVNS_8(1,J) = OBS(I,1)
            EVNS_8(2,J) = OBS(I,2)
            EVNS_8(3,J) = QMS(I)
            EVNS_8(4,J) = QCPC
            EVNS_8(5,J) = RCS(I)
         ENDIF
      ENDDO

C  WRITE THE EVENTS AND EXIT
C  -------------------------

      CALL UFBINT(LUNIT,EVNS_8,5,NLV,IRET,EVNSTR)
      WRITE(6,101) NLV,IRET

      RETURN

  100 FORMAT('  NO EVENTS WRITTEN')
  101 FORMAT('  EVENTS WRITTEN, NLV,IRET:',2I4)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    INCR
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: CALCULATE FORECAST INCREMENTS, I.E. THE DIFFERENCE 
C   BETWEEN THE OBSERVED VALUE AND THE FORECAST VALUE (OF THE
C   WIND COMPONENTS).
C
C PROGRAM HISTORY LOG:
C 1996-11-20  W. Collins -- Original author.
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL INCR
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE INCR

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      CHARACTER*8  STN
      REAL(8)  BMISS

      COMMON/PROFZ/ZZ(NL,NT,NS),UZ(NL,NT,NS),VZ(NL,NT,NS),QZ(NL,NT,NS),
     $ PZ(NL,NT,NS),UFZ(NL,NT,NS),VFZ(NL,NT,NS),SZ(NL,NT,NS),
     $ DZ(NL,NT,NS),LEVZ(NL,NT,NS)
      COMMON/RESIDS/UI(NL,NT,NS),VI(NL,NT,NS),UT(NL,NT),VT(NL,NT),
     $ UV(NL,NT),VV(NL,NT),VVI(NL,NT),VVT(NL,NT),VVV(NL,NT),UM(NL,NT),
     $ VM(NL,NT),VVM(NL,NT),NI(NL,NT),NTMP(NL,NT),NV(NL,NT),NM(NL,NT),
     $ ZI(NL,NT),ZT(NL,NT),ZV(NL,NT),ZM(NL,NT),DIT(NL,NT),DIV(NL,NT),
     $ DTV(NL,NT),DIM(NL,NT),DVM(NL,NT),DTM(NL,NT)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON /QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON /STNLST/ STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON /BUFRLIB_MISSING/BMISS

      WRITE(6,100)

      DO N=1,NSTN
         DO M=1,NTIMES(N)
            DO L=1,NZZ(N)
	       IF(MAX(UZ(L,M,N),UFZ(L,M,N)).LT.BMISS) THEN
                  UI(L,M,N) = UZ(L,M,N) - UFZ(L,M,N)
               ELSE
                  UI(L,M,N) = BMISS
               ENDIF
	       IF(MAX(VZ(L,M,N),VFZ(L,M,N)).LT.BMISS) THEN
                  VI(L,M,N) = VZ(L,M,N) - VFZ(L,M,N)
               ELSE
                  VI(L,M,N) = BMISS
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      RETURN

  100 FORMAT(//'CALCULATING WIND COMPONENT INCREMENTS'//)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    INDICATE
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: COMPUTE NORMALIZED RESIDUALS FOR A SINGLE UNIQUE
C   STATION.
C
C PROGRAM HISTORY LOG:
C 1995-04-05  W. Collins -- Original author.
C 2004-09-09  D. Keyser  -- Incr., temporal, vert. & median chk error
C     limits now directly assoc. w/ a rpted hgt above ground lvl rather
C     than w/ lvl index in arrays (ensures proper error limit used at
C     all lvls & eliminates hgt above ground vs. hgt above sea lvl
C     discrepancy - error limits values remain unchanged).
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL INDICATE(IS)
C   INPUT ARGUMENT LIST:
C     IS       - STATION INDEX
C
C REMARKS: 
C   VARIABLE NAMES:
C     VVI      - VECTOR INCREMENT
C     VVT      - VECTOR TEMPORAL RESIDUAL
C     VVV      - VECTOR VERTICAL RESIDUAL
C     VVM      - VECTOR MEDIAN RESIDUAL
C     NI       - NORMALIZED VECTOR INCREMENT
C     NTMP     - NORMALIZED VECTOR TEMPORAL RESIDUAL
C     NV       - NORMALIZED VECTOR VERTICAL RESIDUAL
C     NM       - NORMALIZED MEDIAN RESIDUAL
C     ZI       - NORMALIZED VECTOR INCREMENT
C     ZT       - NORMALIZED VECTOR TEMPORAL RESIDUAL
C     ZV       - NORMALIZED VECTOR VERTICAL RESIDUAL
C     ZM       - NORMALIZED VECTOR MEDIAN RESIDUAL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C        
C$$$
      SUBROUTINE INDICATE(IS)

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      CHARACTER*8  STN
      REAL(8)  BMISS

      COMMON/PROFZ/ZZ(NL,NT,NS),UZ(NL,NT,NS),VZ(NL,NT,NS),QZ(NL,NT,NS),
     $ PZ(NL,NT,NS),UFZ(NL,NT,NS),VFZ(NL,NT,NS),SZ(NL,NT,NS),
     $ DZ(NL,NT,NS),LEVZ(NL,NT,NS)
      COMMON/RESIDS/UI(NL,NT,NS),VI(NL,NT,NS),UT(NL,NT),VT(NL,NT),
     $ UV(NL,NT),VV(NL,NT),VVI(NL,NT),VVT(NL,NT),VVV(NL,NT),UM(NL,NT),
     $ VM(NL,NT),VVM(NL,NT),NI(NL,NT),NTMP(NL,NT),NV(NL,NT),NM(NL,NT),
     $ ZI(NL,NT),ZT(NL,NT),ZV(NL,NT),ZM(NL,NT),DIT(NL,NT),DIV(NL,NT),
     $ DTV(NL,NT),DIM(NL,NT),DVM(NL,NT),DTM(NL,NT)
      COMMON/QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON/LIMITS/XI(9125),XT(15750),XV(15750)
      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON /BUFRLIB_MISSING/BMISS

      XM(XIZ,XTZ,XVZ) = SQRT((XIZ**2+XTZ**2+XVZ**2)/3.)

C     SET INDICATORS.
C     A VALUE OF 2 CORRESPONDS TO 1.0 STANDARD DEVIATION.
C     VALUES ARE CAPPED AT 20.

      DO N=1,NTIMES(IS)
        DO L=1,NZZ(IS)

          XIZ = 3.80
          XTZ = 2.00
          XVZ = 3.00
          IF(MAX(ZZ(L,N,IS),Z0(IS)) .LT. BMISS) THEN
             IF(NINT(ZZ(L,N,IS)-Z0(IS)).LT.15751) THEN
                XTZ = XT(NINT(ZZ(L,N,IS)-Z0(IS)))
                XVZ = XV(NINT(ZZ(L,N,IS)-Z0(IS)))
                IF(NINT(ZZ(L,N,IS)-Z0(IS)).LT.9126) THEN
                   XIZ = XI(NINT(ZZ(L,N,IS)-Z0(IS)))
                ENDIF
             ENDIF
          ENDIF
          IF(VVI(L,N).LT.BMISS) THEN
            NI(L,N) = VVI(L,N)/(0.5*XIZ)
            ZI(L,N) = VVI(L,N)/XIZ
          ELSE
            NI(L,N) = -1
            ZI(L,N) = BMISS
          ENDIF
          NI(L,N) = MIN(NI(L,N),20)
	
          IF(VVT(L,N).LT.BMISS) THEN
            NTMP(L,N) = VVT(L,N)/(0.5*XTZ)
            ZT(L,N) = VVT(L,N)/XTZ
          ELSE
            NTMP(L,N) = -1
            ZT(L,N) = BMISS
          ENDIF
          NTMP(L,N) = MIN(NTMP(L,N),20)
	
          IF(VVV(L,N).LT.BMISS) THEN
            NV(L,N) = VVV(L,N)/(0.5*XVZ)
            ZV(L,N) = VVV(L,N)/XVZ
          ELSE
            NV(L,N) = -1
            ZV(L,N) = BMISS
          ENDIF
          NV(L,N) = MIN(NV(L,N),20)
	
          IF(VVM(L,N).LT.BMISS) THEN
            NM(L,N) = VVM(L,N)/(0.5*XM(XIZ,XTZ,XVZ))
            ZM(L,N) = VVM(L,N)/XM(XIZ,XTZ,XVZ)
          ELSE
            NM(L,N) = -1
            ZM(L,N) = BMISS
          ENDIF
          NM(L,N) = MIN(NM(L,N),20)
	
        ENDDO
      ENDDO

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C 
C SUBPROGRAM:    INIT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: INITIALIZES SEVERAL VARIABLES.
C
C PROGRAM HISTORY LOG:
C 1996-11-20  W. Collins -- Original author.
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL INIT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE INIT

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      CHARACTER*8 STN
      REAL(8)  BMISS

      COMMON/PROFZ/ZZ(NL,NT,NS),UZ(NL,NT,NS),VZ(NL,NT,NS),QZ(NL,NT,NS),
     $ PZ(NL,NT,NS),UFZ(NL,NT,NS),VFZ(NL,NT,NS),SZ(NL,NT,NS),
     $ DZ(NL,NT,NS),LEVZ(NL,NT,NS)
      COMMON/PROFP/ZP(NL,NT,NS),UP(NL,NT,NS),VP(NL,NT,NS),QP(NL,NT,NS),
     $ PP(NL,NT,NS),UFP(NL,NT,NS),VFP(NL,NT,NS),LEVP(NL,NT,NS)
      COMMON/QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON/RESIDS/UI(NL,NT,NS),VI(NL,NT,NS),UT(NL,NT),VT(NL,NT),
     $ UV(NL,NT),VV(NL,NT),VVI(NL,NT),VVT(NL,NT),VVV(NL,NT),UM(NL,NT),
     $ VM(NL,NT),VVM(NL,NT),NI(NL,NT),NTMP(NL,NT),NV(NL,NT),NM(NL,NT),
     $ ZI(NL,NT),ZT(NL,NT),ZV(NL,NT),ZM(NL,NT),DIT(NL,NT),DIV(NL,NT),
     $ DTV(NL,NT),DIM(NL,NT),DVM(NL,NT),DTM(NL,NT)
      COMMON/STATZ/UZS(120,0:24,9,3:8),VZS(120,0:24,9,3:8),
     $ UIS(120,0:24,9,3:8),VIS(120,0:24,9,3:8),UTS(120,0:24,9,3:8),
     $ VTS(120,0:24,9,3:8),UVS(120,0:24,9,3:8),VVS(120,0:24,9,3:8),
     $ VVIS(120,0:24,9,3:8),VVTS(120,0:24,9,3:8),VVVS(120,0:24,9,3:8),
     $ VVMS(120,0:24,9,3:8),UMS(120,0:24,9,3:8),VMS(120,0:24,9,3:8)
      COMMON /BUFRLIB_MISSING/BMISS

      ZZ   = BMISS
      UZ   = BMISS
      VZ   = BMISS
      QZ   = BMISS
      PZ   = BMISS
      UFZ  = BMISS
      VFZ  = BMISS
      SZ   = BMISS
      DZ   = BMISS
      LEVZ = 0

      ZP   = BMISS
      UP   = BMISS
      VP   = BMISS
      QP   = BMISS
      PP   = BMISS
      UFP  = BMISS
      VFP  = BMISS
      LEVP = 0

      STN  = '99999999'
      X    = BMISS
      Y    = BMISS
      Z    = BMISS
      TIM  = BMISS
      NTIMES = 0

      UI   = BMISS
      VI   = BMISS

      QM   = 0
      IQ   = 99999
      ICQC = 0
      IQCC = 0

      IBAD_FLAG = 0
      JBAD_FLAG = 0

      IRC = 99999

      MLEV = 0

      DT   = BMISS

      UZS  = 0
      VZS  = 0
      UIS  = 0
      VIS  = 0
      UTS  = 0
      VTS  = 0
      UVS  = 0
      VVS  = 0
      VVIS = 0
      VVTS = 0
      VVVS = 0
      VVMS = 0
      UMS  = 0
      VMS  = 0

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    ISDONE
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1996-11-20
C
C ABSTRACT: TEST TO SEE IF QUALITY CONTROL IS COMPLETE FOR THIS
C   STATION.
C
C PROGRAM HISTORY LOG:
C 1996-11-20  W. Collins -- Original author.
C
C USAGE:    CALL ISDONE(DONE)
C   INPUT ARGUMENT LIST:
C     DONE     - LOGICAL VARIABLE, TRUE WHEN COMPLETE
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE ISDONE(DONE)

      LOGICAL DONE

      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT

      IF((NSCAN.GT.1 .AND. NCH.EQ.0) .OR. NSCAN.GT.8) THEN
         DONE = .TRUE.
         IF(NCH.EQ.0) THEN
            WRITE(6,100)
         ELSE
            WRITE(6,101)
         ENDIF
      ELSE
         DONE = .FALSE.
         IF(NCH.EQ.0) THEN
            WRITE(6,102)
         ELSE
            WRITE(6,103)
         ENDIF
      ENDIF

      RETURN

  100 FORMAT('NO Q.M. CHANGES IN THIS SCAN, DONE WITH THIS STN')
  101 FORMAT('HIT LIMIT OF 9 SCANS STILL WITH Q.M. CHANGES, ',
     $ 'DONE WITH THIS STN')
  102 FORMAT('NO Q.M. CHANGES IN THIS SCAN, BUT THIS IS FIRST SCAN, ',
     $ 'SCAN ONCE AGAIN')
  103 FORMAT('SINCE Q.M. CHANGES IN THIS SCAN, SCAN ONCE AGAIN')

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    ISORT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1994-03-17
C
C ABSTRACT: SORT INTEGERS IA ACCORDING TO THE ORDER SPECIFIED BY THE
C   INDICES IN INDX.
C
C PROGRAM HISTORY LOG:
C 1994-03-17  W. Collins -- Original author.
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
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE ISORT(IA,INDX,N)

C     SORT IA ACCORDING TO THE ORDER SPECIFIED BY THE INDICES IN INDX.

      INTEGER INDX(*)
      DIMENSION IA(*), IKSP(899)

      IKSP(1:N) = IA(1:N)

      IA(1:N) = IKSP(INDX(1:N))

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MODQUAL
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: CONVERTS THIS PROGRAM'S DMA INDEX FOR WIND QUALITY
C   CONTROL FOR SUSPECT OR BAD (1 OR 2, RESP.) TO PREPBUFR
C   QUALITY MARK FOR SUSPECT OR BAD (3 OR 13, RESP.).  PRE-
C   EXISTING BAD PREPBUFR QUALITY MARKS ARE HONORED.
C
C PROGRAM HISTORY LOG:
C 1995-04-05  W. Collins -- Original author.
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL MODQUAL(IS)
C   INPUT ARGUMENT LIST:
C     IS       - STATION INDEX
C
C REMARKS: 
C   VARIABLE NAMES:
C     IQ       - PRE-EXISTING PREPBUFR WIND QUALITY MARKS
C     ICQC     - DMA INDEX FOR WIND QUALITY CONTROL
C     IQCC     - EITHER 0 IF PRE-EXISTING PREPBUFR WIND QUALITY
C                MARK NOT CHANGED BY DMA (I.E., NO EVENT), 3 IF
C                DMA DETERMINES QUALITY IS SUSPECT, OR 13 IF DMA
C                DETERMINES QUALITY IS BAD
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE MODQUAL(IS)

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      CHARACTER*8  STN

      COMMON/QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON/TIMEO/TIMWIN_E,TIMWIN_L

      DO IT=1,NTIMES(IS)
        DO L=1,NZZ(IS)

C Honor pre-exiting PREPBUFR q.m. as follows:
C  - If it's BAD, then don't use bad or suspect q.m. from this program
C  - If it's SUSPECT, then don't use suspect q.m. from this program
C     (do use BAD q.m. from this program, however)

          IF(IQ(L,IT,IS).LT.4) THEN
            IF(ICQC(L,IT,IS).EQ.1.AND.IQ(L,IT,IS).LT.3) THEN
               IQCC(L,IT,IS) = 3  ! Suspect
            ELSE IF(ICQC(L,IT,IS).EQ.2) THEN
               IQCC(L,IT,IS) = 13 ! Bad
            ELSE
               IRC(L,IT,IS) = 99999
            ENDIF
          ELSE
            IRC(L,IT,IS) = 99999
          ENDIF
          IF(NINT(TIM(IT,IS)*1000.).LT.NINT(TIMWIN_E*1000.).OR.
     $       NINT(TIM(IT,IS)*1000.).GT.NINT(TIMWIN_L*1000.)) CYCLE
          IF((IQ(L,IT,IS).GT.2.AND.IQ(L,IT,IS).LT.16) .OR.
     $     ICQC(L,IT,IS).GT.0) THEN
            IBAD_FLAG(IT,IS) = 1
            JBAD_FLAG(IS) = 1
          ENDIF
        ENDDO
      ENDDO

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    PLINT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1995-04-05
C
C ABSTRACT: INTERPOLATE FIN AT (2,2), NOT USING VALUE AT THIS POINT.
C   INTERPOLATION IS PLANAR LEAST SQUARES FIT TO SURROUNDING DATA.
C
C PROGRAM HISTORY LOG:
C 1995-04-05  W. Collins -- Original author.
C
C USAGE:    CALL PLINT(FIN,FUT,BMISS)
C   INPUT ARGUMENT LIST:
C     FIN      - INPUT FIELD
C     BMISS    - VALUE INDICATING MISSING
C
C   OUTPUT ARGUMENT LIST:
C     FUT      - OUTPUT INTERPOLATED VALUE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE PLINT(FIN,FUT,BMISS)

      REAL FIN(3,3),F(9)
      REAL(8) BMISS
      INTEGER IX(9),IY(9)

      IPNT = 0
      FSUM = 0.

      DO I=1,3
        DO J=1,3
          FSUM = FSUM + ABS(FIN(I,J))
        ENDDO
      ENDDO
      IF(FSUM.LT..01) THEN
        FUT = BMISS
        RETURN
      ENDIF

      F  = BMISS
      IX = 99
      IY = 99

      DO J=1,3
        DO I=1,3
          IF(I.EQ.2.AND.J.EQ.2) CYCLE
          IF(FIN(I,J).EQ.BMISS) CYCLE
          IPNT = IPNT + 1
          F(IPNT) = FIN(I,J)
          IX(IPNT) = I-2
          IY(IPNT) = J-2
        ENDDO
      ENDDO

      IF(IPNT.GE.4) GOTO 30

      IF(IPNT.LE.2) THEN
        FUT = BMISS
        RETURN
      ENDIF

      IF((IX(1).EQ.IX(2).AND.IX(2).EQ.IX(3))
     &  .OR.(IY(1).EQ.IY(2).AND.IY(2).EQ.IY(3))) THEN
        FUT = BMISS
        RETURN
      ENDIF

   30 CONTINUE

      XB  = 0.
      YB  = 0.
      XYB = 0.
      ZYB = 0.
      ZXB = 0.
      YYB = 0.
      XXB = 0.
      ZB  = 0.

      DO I=1,IPNT
        XB = XB + IX(I)
        YB = YB + IY(I)
        XXB = XXB + IX(I)**2
        YYB = YYB + IY(I)**2
        XYB = XYB + IX(I)*IY(I)
        ZB = ZB + F(I)
        ZXB = ZXB + F(I)*IX(I)
        ZYB = ZYB + F(I)*IY(I)
      ENDDO

      F1=(XB*(XYB*ZYB-YYB*ZXB)-YB*(XXB*ZYB-XYB*ZXB)+ZB*(XXB*YYB-XYB**2))
      F2 = (XB*(XYB*YB-XB*YYB)-YB*(XXB*YB-XB*XYB)+IPNT*(XXB*YYB-XYB**2))

      IF(F2.EQ.0.) THEN
        FUT = BMISS
      ELSE
        FUT = F1/F2
      ENDIF

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    PRNTDATA
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: WRITE WIND PROFILER DATA TO VARIOUS UNITS FOR A SINGLE
C   UNIQUE STATION.
C
C PROGRAM HISTORY LOG:
C 1995-04-05  W. Collins -- Original author.
C 2004-09-09  D. Keyser  -- Stdout easier to read & more complete. Rpt
C     lvl info. in various output listings expanded & sorted into bins
C     containing both all data lvls & only lvls w/ either suspect or
C     bad q. marks; output written to text files but not incl. in
C     stdout.
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL PRNTDATA(IS)
C   INPUT ARGUMENT LIST:
C     IS       - STATION INDEX
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 52  - LISTING (TYPE 1) OF ALL OUTPUT WIND PROFILER LEVELS
C     UNIT 53  - LISTING (TYPE 2) OF ALL OUTPUT WIND PROFILER LEVELS
C     UNIT 54  - LISTING (TYPE 1) OF WIND PROFILER LEVELS WITH EVENTS
C                FROM THIS PROGRAM PLUS PRE-EXISTING PREPBUFR BAD
C                LEVELS
C     UNIT 55  - LISTING (TYPE 2) OF WIND PROFILER LEVELS WITH EVENTS
C                FROM THIS PROGRAM PLUS PRE-EXISTING PREPBUFR BAD
C                LEVELS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE PRNTDATA(IS)

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      CHARACTER*8  STN
      CHARACTER*1  CEVN

      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON/DATEZ/JDATE(4),KDATE(5,NT,NS),MDATE(5),NDATE(5)
      COMMON/PROFZ/ZZ(NL,NT,NS),UZ(NL,NT,NS),VZ(NL,NT,NS),QZ(NL,NT,NS),
     $ PZ(NL,NT,NS),UFZ(NL,NT,NS),VFZ(NL,NT,NS),SZ(NL,NT,NS),
     $ DZ(NL,NT,NS),LEVZ(NL,NT,NS)
      COMMON/RESIDS/UI(NL,NT,NS),VI(NL,NT,NS),UT(NL,NT),VT(NL,NT),
     $ UV(NL,NT),VV(NL,NT),VVI(NL,NT),VVT(NL,NT),VVV(NL,NT),UM(NL,NT),
     $ VM(NL,NT),VVM(NL,NT),NI(NL,NT),NTMP(NL,NT),NV(NL,NT),NM(NL,NT),
     $ ZI(NL,NT),ZT(NL,NT),ZV(NL,NT),ZM(NL,NT),DIT(NL,NT),DIV(NL,NT),
     $ DTV(NL,NT),DIM(NL,NT),DVM(NL,NT),DTM(NL,NT)
      COMMON/QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/TIMEO/TIMWIN_E,TIMWIN_L

      WRITE(52,100) STN(IS)
      WRITE(53,100) STN(IS)
      IF(JBAD_FLAG(IS).EQ.1) THEN
        WRITE(54,101) STN(IS)
        WRITE(55,101) STN(IS)
        WRITE(54,102)
        WRITE(55,103)
      ENDIF
      WRITE(52,102)
      WRITE(53,103)
      DO N=1,NTIMES(IS)
        IF(NINT(TIM(N,IS)*1000.).LT.NINT(TIMWIN_E*1000.).OR.
     $     NINT(TIM(N,IS)*1000.).GT.NINT(TIMWIN_L*1000.)) CYCLE

        IF(NZZ(IS).GT.0) THEN
           WRITE(6,104) STN(IS),Z0(IS),(KDATE(J,N,IS),J=1,5)
           WRITE(6,102)
        ELSE
           CYCLE
        ENDIF

C       PRINT TO VARIOUS UNITS AFTER MAKING SURE THAT
C       THE NUMBERS ARE SMALL ENOUGH TO FIT IN FORMAT.

        IW10000 = 0
        IW13000 = 0
        DO I=1,NZZ(IS)
          IF(ZZ(I,N,IS).GT.99999.) CYCLE
          IF(IQ(I,N,IS).GT.3.OR.IQCC(I,N,IS).EQ.0) THEN
             IQCCP = IQ(I,N,IS)
          ELSE
             IQCCP = IQCC(I,N,IS)
          ENDIF

          CEVN = ' '
          IF(IQCC(I,N,IS).GT.0) THEN
             CEVN = '+'
          ENDIF

C         WRITE THIS PROGRAM'S EVENTS + PRE-EXISTING PREPBUFR BAD
C          LEVELS TO UNIT 54 - TYPE 1 LISTING

          IF((IQ(I,N,IS).GT.2.AND.IQ(I,N,IS).LT.16) .OR.
     $        ICQC(I,N,IS).GT.0) THEN
            WRITE(54,105) (KDATE(J,N,IS),J=1,5),STN(IS),
     $       NINT(ZZ(I,N,IS)),NINT(SZ(I,N,IS)),NINT(DZ(I,N,IS)),
     $       UZ(I,N,IS),VZ(I,N,IS),UFZ(I,N,IS),VFZ(I,N,IS),UT(I,N),
     $       VT(I,N),UV(I,N),VV(I,N),UM(I,N),VM(I,N),VVI(I,N),VVT(I,N),
     $       VVV(I,N),VVM(I,N),IQ(I,N,IS),NI(I,N),NTMP(I,N),NV(I,N),
     $       NM(I,N),ICQC(I,N,IS),IQCCP,CEVN
          ENDIF

C         WRITE THIS PROGRAM'S EVENTS + PRE-EXISTING PREPBUFR BAD
C          LEVELS TO UNIT 55 - TYPE 2 LISTING

          IF((IQ(I,N,IS).GT.2.AND.IQ(I,N,IS).LT.16) .OR.
     $       ICQC(I,N,IS).GT.0) THEN
            IF(NINT(ZZ(I,N,IS)).GT.10000.AND.IW10000.EQ.0) THEN
               WRITE(55,106)
               IW10000 = 1
            ENDIF
            IF(NINT(ZZ(I,N,IS)).GE.13000.AND.IW13000.EQ.0) THEN
               WRITE(55,106)
               IW13000 = 1
            ENDIF
            WRITE(55,107) (KDATE(J,N,IS),J=1,5),STN(IS),
     $       NINT(ZZ(I,N,IS)),UZ(I,N,IS),VZ(I,N,IS),UFZ(I,N,IS),
     $       VFZ(I,N,IS), ZI(I,N),ZT(I,N),DT(I,N,IS),ZV(I,N),ZM(I,N),
     $       DIT(I,N),DIV(I,N),DTV(I,N),DIM(I,N),DVM(I,N),DTM(I,N),
     $       IQ(I,N,IS),ICQC(I,N,IS),IQCCP,CEVN,IRC(I,N,IS)
          ENDIF

C         WRITE ALL DATA TO STANDARD OUTPUT (UNIT 06)

          WRITE(6,105) (KDATE(J,N,IS),J=1,5),STN(IS),NINT(ZZ(I,N,IS)),
     $      NINT(SZ(I,N,IS)),NINT(DZ(I,N,IS)),UZ(I,N,IS),VZ(I,N,IS),
     $      UFZ(I,N,IS),VFZ(I,N,IS),UT(I,N),VT(I,N),UV(I,N),VV(I,N),
     $      UM(I,N),VM(I,N),VVI(I,N),VVT(I,N),VVV(I,N),VVM(I,N),
     $      IQ(I,N,IS),NI(I,N),NTMP(I,N),NV(I,N),NM(I,N),ICQC(I,N,IS),
     $      IQCCP,CEVN

C         WRITE ALL DATA TO UNIT 52 - TYPE 1 LISTING

          WRITE(52,105) (KDATE(J,N,IS),J=1,5),STN(IS),NINT(ZZ(I,N,IS)),
     $     NINT(SZ(I,N,IS)),NINT(DZ(I,N,IS)),UZ(I,N,IS),VZ(I,N,IS),
     $     UFZ(I,N,IS),VFZ(I,N,IS),UT(I,N),VT(I,N),UV(I,N),VV(I,N),
     $     UM(I,N),VM(I,N),VVI(I,N),VVT(I,N),VVV(I,N),VVM(I,N),
     $     IQ(I,N,IS),NI(I,N),NTMP(I,N),NV(I,N),NM(I,N),ICQC(I,N,IS),
     $     IQCCP,CEVN

C         WRITE ALL DATA TO UNIT 53 - TYPE 2 LISTING

           WRITE(53,107) (KDATE(J,N,IS),J=1,5),STN(IS),NINT(ZZ(I,N,IS)),
     $      UZ(I,N,IS),VZ(I,N,IS),UFZ(I,N,IS),VFZ(I,N,IS),ZI(I,N),
     $      ZT(I,N),DT(I,N,IS),ZV(I,N),ZM(I,N),DIT(I,N),DIV(I,N),
     $      DTV(I,N),DIM(I,N),DVM(I,N),DTM(I,N),IQ(I,N,IS),ICQC(I,N,IS),
     $      IQCCP,CEVN,IRC(I,N,IS)
        ENDDO
        WRITE(53,108)
        IF(IBAD_FLAG(N,IS).EQ.1) THEN
           WRITE(54,108)
           WRITE(55,108)
        ENDIF
      ENDDO

      RETURN

  100 FORMAT(/1X,128('+')/' FINAL LISTING OF ALL LVLS FOR ',A8,'(+ in ',
     $ 'QQ column means this program wrote a q.c. event)')
  101 FORMAT(/1X,128('+')/' FINAL LISTING OF ALL SUSPECT OR BAD LVLS ',
     $ 'FOR ',A8,'(+ in QQ column means this program wrote a q.c. ',
     $ 'event)')
  102 FORMAT('  DATETIME    STN        Z  SP DIR     U     V    UG    ',
     $  'VG   UT   VT   UV   VV   UM   VM  VVI  VVT  VVV  VVM IQ NI NT',
     $ ' NV NM PQ OQ')
  103 FORMAT('  DATETIME    STN        Z     U     V    UG    ',
     $  'VG    ZI    ZT(del-t)    ZV    ZM   DIT   DIV   DTV   DIM   ',
     $ 'DVM   DTM IQ PQ OQ   RC')
  104 FORMAT(' FINAL LISTING OF ALL LVLS FOR ',A8,', ELEV ',F6.1,' AT ',
     $ I4,'/',I2.2,'/',I2.2,'/',I2.2,I2.2,' (+ in QQ column means this',
     $ ' program wrote a q.c. event)')
  105 FORMAT(I4,4I2.2,1X,A8,I5,1X,I3,1X,I3,4(1X,F5.1),10(1X,F4.1),1X,I2,
     $ 4(1X,I2),2(1X,I2),A1)
  106 FORMAT(1X,64('. '))
  107 FORMAT(I4,4I2.2,1X,A8,I5,6(1X,F5.1),'(',F5.2,')',8(1X,F5.1),
     $ 3(1X,I2),A1,1X,I3.3)
  108 FORMAT(1X,128('-'))

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    PUTDATA
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: PLACE DATA FOR A SINGLE REPORT INTO COMMON BLOCKS FOR
C   USE.  THE ORIGINAL REPORTED LEVELS ARE SPLIT FROM THE AUXILIARY
C   LEVELS OPTIONALLY ADDED BY THE PREVIOUS PREPOBS_PREPDATA PROGRAM.
C   ONLY THE ORIGINAL REPORTED LEVELS ARE QC'D AND THE QUALITY OF THE
C   AUXILIARY LEVELS IS IMPLIED.
C
C PROGRAM HISTORY LOG:
C 1996-11-20  W. Collins -- Original author.
C 1998-09-16  W. COLLINS -- Use W3LIB routine W3MOVDAT for date
C     change computation.
C 2004-09-09  D. Keyser  -- Max. no. of sites processed incr. from 40
C     to 120. No. of time periods input now site specific (was
C     hardwired to be same for all rpts).
C 2016-12-20  Stokes/Keyser  Increase the max allowable number of times
C     per station.  Skip reports and print warning if that limit is 
C     exceeded.
C
C USAGE:    CALL PUTDATA
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE PUTDATA

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      CHARACTER*8  STN,CID,cNT
      REAL  RINC(5)
      REAL(8)  SID_8
      INTEGER  IDAT(8),JDAT(8)

      COMMON/DATEZ/JDATE(4),KDATE(5,NT,NS),MDATE(5),NDATE(5)
      COMMON/HEADER/SID_8,XOB,YOB,DHR,ELV,XTP,TYP,T29,NLV
      COMMON/PROBS/ZOB(255),UOB(255),VOB(255),WQM(255),POB(255),
     $ UFC(255),VFC(255),CAT(255),SOB(255),DOB(255)
      COMMON/PROFZ/ZZ(NL,NT,NS),UZ(NL,NT,NS),VZ(NL,NT,NS),QZ(NL,NT,NS),
     $ PZ(NL,NT,NS),UFZ(NL,NT,NS),VFZ(NL,NT,NS),SZ(NL,NT,NS),
     $ DZ(NL,NT,NS),LEVZ(NL,NT,NS)
      COMMON/PROFP/ZP(NL,NT,NS),UP(NL,NT,NS),VP(NL,NT,NS),QP(NL,NT,NS),
     $ PP(NL,NT,NS),UFP(NL,NT,NS),VFP(NL,NT,NS),LEVP(NL,NT,NS)
      COMMON/QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT

      EQUIVALENCE (SID_8,CID)

      data ifirst/0/

      NREP1  = NREP1  + 1

C  FIND A STATION MATCH
C  --------------------

      IF(NSTN.GT.0) THEN
         DO NN=1,NSTN
            N = NN
            IF(STN(N).EQ.CID) GOTO 10    ! We have a match
         ENDDO
      ENDIF

C  IF NO MATCH, ADD THIS STATION TO LIST
C  -------------------------------------

      NSTN  = NSTN  + 1
      NSTN1 = NSTN1 + 1
      IF(NSTN1.GT.NS) THEN
         WRITE(6,100) CID,NS
         NSTN = NS
         RETURN
      ENDIF
      N = NSTN
      STN(N)  = CID
      X(N)    = XOB
      Y(N)    = YOB
      Z0(N)   = ELV
      IRTP(N) = NINT(TYP)

   10 CONTINUE

C  PRINT WARNING IF NUMBER OF OB TIMES > LIMIT FOR A PARTICULAR STATION
C  --------------------------------------------------------------------

      NTIMES(N) = NTIMES(N) + 1
      IF(NTIMES(N).GT.NT) THEN
         WRITE(6,105) CID,NT,DHR
         NTIMES(N) = NT
c$$$         if(ifirst.eq.0) then
c$$$            write(cNT,'(i8)') NT
c$$$            call system('[ -n "$jlogfile" ] && $DATA/postmsg'//
c$$$     $       ' "$jlogfile" "***WARNING: THE NUMBER OF OB TIMES FOR 1'//
c$$$     $       ' OR MORE IDs EXCEEDS LIMIT OF '//cNT//', SOME REPORTS '//
c$$$     $       'NOT PROCESSED - INCR. SIZE OF NT"')
c$$$            ifirst = 1
c$$$         endif
         RETURN
      ENDIF
      TIM(NTIMES(N),N) = DHR

cdak  WRITE(6,101) N,CID

      NREP  = NREP  + 1

      RINC      = 0.
      RINC(2)   = DHR
      IDAT      = 0
      IDAT(1:3) = JDATE(1:3)
      IDAT(5)   = JDATE(4)

      CALL W3MOVDAT(RINC,IDAT,JDAT)

C KDATE WILL HOLD REPORT TIME

      KDATE(1:3,NTIMES(N),N) = JDAT(1:3)
      KDATE(4:5,NTIMES(N),N) = JDAT(5:6)

C MLEV WILL HOLD NUMBER OF LEVELS

      MLEV(NTIMES(N),N) = NLV

C  SEPARATE THE REPORT INTO TWO PARTS, THE ORIGINAL REPORTED LEVELS
C  (CAT. 4) AND THE OPTIONAL AUXILIARY LEVELS AT 25 MB INTERVALS
C  (CAT. 7) -- ONLY THE ORIGINAL REPORTED LEVELS WILL BE Q.C.'D
C  ----------------------------------------------------------------

      LP = 0
      DO LZ=1,NLV
         IF(CAT(LZ).EQ.4 .AND. ZOB(LZ).NE.ELV) THEN    ! Reported level
            ZZ(LZ,NTIMES(N),N)   = ZOB(LZ)
            UZ(LZ,NTIMES(N),N)   = UOB(LZ)
            VZ(LZ,NTIMES(N),N)   = VOB(LZ)
            QZ(LZ,NTIMES(N),N)   = WQM(LZ)
            IQ(LZ,NTIMES(N),N)   = NINT(WQM(LZ))
            PZ(LZ,NTIMES(N),N)   = POB(LZ)
            UFZ(LZ,NTIMES(N),N)  = UFC(LZ)
            VFZ(LZ,NTIMES(N),N)  = VFC(LZ)
            SZ(LZ,NTIMES(N),N)   = SOB(LZ)
            DZ(LZ,NTIMES(N),N)   = DOB(LZ)
            LEVZ(LZ,NTIMES(N),N) = LZ
         ELSE IF(CAT(LZ).EQ.7) THEN    ! Auxiliary level
            LP = LP + 1
            ZP(LP,NTIMES(N),N)   = ZOB(LZ)
            UP(LP,NTIMES(N),N)   = UOB(LZ)
            VP(LP,NTIMES(N),N)   = VOB(LZ)
            QP(LP,NTIMES(N),N)   = WQM(LZ)
            PP(LP,NTIMES(N),N)   = POB(LZ)
            UFP(LP,NTIMES(N),N)  = UFC(LZ)
            VFP(LP,NTIMES(N),N)  = VFC(LZ)
            LEVP(LP,NTIMES(N),N) = LZ
         ENDIF
      ENDDO

      RETURN

  100 FORMAT(/'##PUTDATA: ID ',A8,' CANNOT BE Q.C.-d BECAUSE THE ',
     $ 'NUMBER OF UNIQUE STATIONS EXCEEDS THE LIMIT OF',I4/)
  101 FORMAT(' PUTDATA: N,CID: ',I5,2X,A8)
  105 FORMAT(/'##PUTDATA: THE NUMBER OF OB TIMES FOR ID ',A8,
     $ ' EXCEEDS THE LIMIT OF ',I0,'. SKIP REPORT FOR DHR=',F7.3/)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    READPROF
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: READ PROFILER REPORTS FROM THE PREPBUFR FILE AND STORE
C   INTO MEMORY.
C
C PROGRAM HISTORY LOG:
C 1996-11-20  W. Collins -- Original author.
C 2004-09-09  D. Keyser  -- Namelist switches added to specify output
C     time window.
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C 2020-01-09  J. Dong   Changed the windowing decade from 20 to 40
C     for cases when the year is represented by 2 digits instead of 4.
C
C USAGE:    CALL READPROF
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE READPROF

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      LOGICAL  FIRST
      CHARACTER*8  SUBSET,CID
      CHARACTER*40  HSTR,OSTR
      REAL  RINC(5)
      REAL(8)  SID_8,HDR_8(10),OBS_8(10,255)
      INTEGER  IDAT(8),JDAT(8)

      COMMON/DATEZ /JDATE(4),KDATE(5,NT,NS),MDATE(5),NDATE(5)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON/HEADER/SID_8,XOB,YOB,DHR,ELV,XTP,TYP,T29,NLV
      COMMON/TIMEO/TIMWIN_E,TIMWIN_L
      COMMON/PROBS/ZOB(255),UOB(255),VOB(255),WQM(255),POB(255),
     $ UFC(255),VFC(255),CAT(255),SOB(255),DOB(255)

      EQUIVALENCE  (SID_8,CID)

      DATA HSTR /'SID XOB YOB DHR ELV ITP TYP T29        '/
      DATA OSTR /'ZOB UOB VOB WQM POB UFC VFC CAT FFO DDO'/
      DATA FIRST /.TRUE./

      IF(FIRST) THEN    ! Do several things the first time in here
         CALL DATELEN(10)
         WRITE(6,100) NFIN
         CALL OPENBF(NFIN,'IN',NFIN)

   10    CONTINUE

         CALL READMG(NFIN,SUBSET,IDATEP,IRETMG)
         IF(IRETMG.NE.0)   GOTO 999
         IF(SUBSET.NE.'PROFLR  ')  GOTO 10
         WRITE(6,101) IDATEP
         IF(IDATEP.LT.1000000000) THEN

C If 2-digit year returned in IDATEP, must use "windowing" technique
C  to create a 4-digit year

C IMPORTANT: IF DATELEN(10) IS CALLED, THE DATE HERE SHOULD ALWAYS
C            CONTAIN A 4-DIGIT YEAR, EVEN IF INPUT FILE IS NOT
C            Y2K COMPLIANT (BUFRLIB DOES THE WINDOWING HERE)

            PRINT *,'##THE FOLLOWING SHOULD NEVER HAPPEN!!!!'
            PRINT'("##2-DIGIT YEAR IN IDATEP RETURNED FROM ",
     $       "READMG (IDATEP IS: ",I0,") - USE WINDOWING TECHNIQUE",
     $       " TO OBTAIN 4-DIGIT YEAR")', IDATEP
C IF IDATEP=41~99 THEN IDATEP=1941~1999
C IF IDATEP=00~40 THEN IDATEP=2000~2040
            IF(IDATEP/1000000.GT.40) THEN
               IDATEP = 1900000000 + IDATEP
            ELSE
               IDATEP = 2000000000 + IDATEP
            ENDIF
            PRINT *, '##CORRECTED IDATEP WITH 4-DIGIT YEAR, ',
     $       'IDATEP NOW IS: ',IDATEP
         ENDIF

C JDATE WILL HOLD PREPBUFR CENTER TIME

         JDATE(1) = IDATEP/1000000
         JDATE(2) = MOD((IDATEP/10000),100)
         JDATE(3) = MOD((IDATEP/100),100)
         JDATE(4) = MOD(IDATEP,100)

         RINC      = 0.
         RINC(2)   = TIMWIN_E
         IDAT      = 0
         IDAT(1:3) = JDATE(1:3)
         IDAT(5)   = JDATE(4)

         CALL W3MOVDAT(RINC,IDAT,JDAT)

C MDATE WILL HOLD EARLIEST ALLOWED OUTPUT REPORT TIME

         MDATE(1:3) = JDAT(1:3)
         MDATE(4:5) = JDAT(5:6)

         RINC(2) = TIMWIN_L

         CALL W3MOVDAT(RINC,IDAT,JDAT)

C NDATE WILL HOLD LATEST ALLOWED OUTPUT REPORT TIME

         NDATE(1:3) = JDAT(1:3)
         NDATE(4:5) = JDAT(5:6)

         WRITE(6,102) MDATE,NDATE

         FIRST = .FALSE.
         WRITE(6,103)
         GOTO 20    ! Read in first wind profiler report
      ENDIF

   30 CONTINUE

      CALL READMG(NFIN,SUBSET,IDATEP,IRET)
      IF(IRET.NE.0) THEN    ! All BUFR messages have been read
         CALL CLOSBF(NFIN)
         WRITE(6,104)  NFIN,NSTN1,NSTN,NREP1,NREP
         RETURN
      ENDIF
      IF(SUBSET.NE.'PROFLR  ') GOTO  30

   20 CONTINUE

      CALL READSB(NFIN,IRET)
      IF(IRET.NE.0) GOTO 30

C  READ AND STORE HEADER ELEMENTS
C  ------------------------------

      CALL UFBINT(NFIN,HDR_8,10,  1,IRET,HSTR)
      CALL UFBINT(NFIN,OBS_8,10,255,IRET,OSTR)
      SID_8 = HDR_8(1)
      XOB = HDR_8(2)
      YOB = HDR_8(3)
      DHR = HDR_8(4)
      ELV = HDR_8(5)
      XTP = HDR_8(6)
      TYP = HDR_8(7)
      T29 = HDR_8(8)

C  READ DATA OF PROPER SUBSET
C  --------------------------

      CALL UFBINT(NFIN,OBS_8,10,255,NLV,OSTR)
      ZOB(1:NLV) = OBS_8(1,1:NLV)
      UOB(1:NLV) = OBS_8(2,1:NLV)
      VOB(1:NLV) = OBS_8(3,1:NLV)
      WQM(1:NLV) = OBS_8(4,1:NLV)
      POB(1:NLV) = OBS_8(5,1:NLV)
      UFC(1:NLV) = OBS_8(6,1:NLV)
      VFC(1:NLV) = OBS_8(7,1:NLV)
      CAT(1:NLV) = OBS_8(8,1:NLV)
      SOB(1:NLV) = OBS_8(9,1:NLV)
      DOB(1:NLV) = OBS_8(10,1:NLV)

      WRITE(6,105) CID,YOB,XOB,DHR,ELV,XTP,TYP,T29,NLV,SUBSET

      WRITE(6,106)
      WRITE(6,107) (POB(L),ZOB(L),UOB(L),VOB(L),UFC(L),VFC(L),WQM(L),
     $ CAT(L),SOB(L),DOB(L),L=1,NLV)

C  PUT THE REPORT INTO MEMORY
C  -------------------------

      CALL PUTDATA

      GOTO 20

C-----------------------------------------------------------------------
  999 CONTINUE

C PREPBUFR DATA SET CONTAINS NO "PROFLR" TABLE A MSGS -- STOP 4  !!!
      WRITE(6,108)
      CALL CLOSBF(NFIN)
      CALL W3TAGE('PREPOBS_PROFCQC')
      CALL ERREXIT(4)
C-----------------------------------------------------------------------

  100 FORMAT(/'READING REPORTS FROM INPUT PREPBUFR FILE IN UNIT ',I3)
  101 FORMAT(/'First PROFLR message found ... '/'PREPBUFR File Sec. 1 ',
     $ 'message date = ',I10)
  102 FORMAT(/'Time window for outputting wind profiler reports is:',1X,
     $ I4,4I2.2,' to ',I4,4I2.2/)
  103 FORMAT(/'==> Begin listing of input wind profiler reports'/)
  104 FORMAT(/5X,'===> PREPBUFR DATA SET IN UNIT ',I3,' SUCCESSFULLY',
     $ ' CLOSED FROM INITIAL READ OF WIND PROFILER OBS.'/10X,'THERE ',
     $ 'ARE A TOTAL OF',I4,' UNIQUE WIND PROFILER STATIONS OVER ALL ',
     $ 'TIME PERIODS'/10X,'A TOTAL OF',I4,' UNIQUE STATIONS WERE ',
     $ 'Q.C.-d'/10X,'THE TOTAL NUMBER OF REPORTS UNPACKED WAS',I4/10X,
     $ 'THE TOTAL NUMPER OF REPORTS Q.C.-d WAS',I4/)
  105 FORMAT(/' ID=',A8,' LAT=',F10.2,' LON=',F10.2,' DHR=',F8.2,
     $ ' ELV=',F8.2,' ITYP=',F6.0,' RTYP=',F6.0,' T29=',F6.0,' LVLS=',
     $ I6,' SUBSET=',A8)
  106 FORMAT('       POB       ZOB       UOB       VOB       UFC',
     $       '       VFC       WQM       CAT       SOB       DOB')
  107 FORMAT(F10.1,F10.0,4F10.1,2F10.0,F10.1,F10.0)
  108 FORMAT(/' PREPBUFR DATA SET CONTAINS NO "PROFLR" TABLE A ',
     $ 'MESSAGES - STOP 4'/)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RESIDUAL
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: CALCULATE ALL RESIDUALS FOR A SINGLE UNIQUE STATION.
C
C PROGRAM HISTORY LOG:
C 1996-11-20  W. Collins -- Original author.
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL RESIDUAL(IS)
C   INPUT ARGUMENT LIST:
C     IS       - STATION INDEX
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE RESIDUAL(IS)

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      COMMON/PROFZ/ZZ(NL,NT,NS),UZ(NL,NT,NS),VZ(NL,NT,NS),QZ(NL,NT,NS),
     $ PZ(NL,NT,NS),UFZ(NL,NT,NS),VFZ(NL,NT,NS),SZ(NL,NT,NS),
     $ DZ(NL,NT,NS),LEVZ(NL,NT,NS)
      COMMON/RESIDS/UI(NL,NT,NS),VI(NL,NT,NS),UT(NL,NT),VT(NL,NT),
     $ UV(NL,NT),VV(NL,NT),VVI(NL,NT),VVT(NL,NT),VVV(NL,NT),UM(NL,NT),
     $ VM(NL,NT),VVM(NL,NT),NI(NL,NT),NTMP(NL,NT),NV(NL,NT),NM(NL,NT),
     $ ZI(NL,NT),ZT(NL,NT),ZV(NL,NT),ZM(NL,NT),DIT(NL,NT),DIV(NL,NT),
     $ DTV(NL,NT),DIM(NL,NT),DVM(NL,NT),DTM(NL,NT)
      COMMON/QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT

C  PERFORM INCREMENTAL, VERTICAL AND TEMPORAL CHECKS
C  -------------------------------------------------

      CALL CHECKS(IS)

C  PERFORM PLANAR-LINEAR INTERPOLATION, GET RESIDUAL
C  -------------------------------------------------

      CALL TWODIM(IS)

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RESTRUCT
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: VERTICALLY AND TEMPORALLY RESTRUCTURE THE DATA.
C
C PROGRAM HISTORY LOG:
C 2000-12-01  W. Collins -- Original author (vertical restructuring
C     only).
C 2004-09-09  D. Keyser  -- Lvl index in all arrays now site specific
C     to allow proper temporal checking at each individual site (each
C     site examined over all rpt times to generate hgt profile
C     containing all possible lvls, times w/ new lvls inserted get
C     missing wind).
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL RESTRUCT
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE RESTRUCT

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      CHARACTER*8     STN
      REAL(8)  BMISS

      REAL  ZZLEV(NL,NS),UZZ(NL,NT,NS),VZZ(NL,NT,NS),QZZ(NL,NT,NS),
     $ PZZ(NL,NT,NS),UFZZ(NL,NT,NS),VFZZ(NL,NT,NS),SZZ(NL,NT,NS),
     $ DZZ(NL,NT,NS),IQZZ(NL,NT,NS),LEVZZ(NL,NT,NS),WKSP(NL,NT,NS),
     $ ZLEV(NL,NT,NS)
      INTEGER  IKSP(NL,NT,NS),JKSP(5,NT,NS),NZ(NT,NS),IV(NL)

      COMMON/PROFZ/ZZ(NL,NT,NS),UZ(NL,NT,NS),VZ(NL,NT,NS),QZ(NL,NT,NS),
     $ PZ(NL,NT,NS),UFZ(NL,NT,NS),VFZ(NL,NT,NS),SZ(NL,NT,NS),
     $ DZ(NL,NT,NS),LEVZ(NL,NT,NS)
      COMMON/PROFP/ZP(NL,NT,NS),UP(NL,NT,NS),VP(NL,NT,NS),QP(NL,NT,NS),
     $ PP(NL,NT,NS),UFP(NL,NT,NS),VFP(NL,NT,NS),LEVP(NL,NT,NS)
      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON/QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/DATEZ/JDATE(4),KDATE(5,NT,NS),MDATE(5),NDATE(5)
      COMMON /BUFRLIB_MISSING/BMISS

      WRITE(6,100)

C  MAKE LIST OF HEIGHTS OF OBSERVATIONS FOR EACH STATION
C  -----------------------------------------------------

      ZLEV = BMISS
      NZ = 0
      LOOP1: DO IS=1,NSTN
         LOOP1n1: DO IT=1,NTIMES(IS)
            LOOP1n2: DO IL=1,MLEV(IT,IS)
               LOOP1n3: DO I=1,MLEV(IT,IS)
                  IF(ZZ(IL,IT,IS).EQ.ZLEV(I,IT,IS)) CYCLE LOOP1n1
               ENDDO  LOOP1n3
               NZ(IT,IS) = NZ(IT,IS) + 1
               ZLEV(NZ(IT,IS),IT,IS) = ZZ(IL,IT,IS)
            ENDDO  LOOP1n2
         ENDDO  LOOP1n1
      ENDDO  LOOP1

      ZZLEV = BMISS
      NZZ = 0
      LOOP1x: DO IS=1,NSTN
         LOOP1n1x: DO IT=1,NTIMES(IS)
            LOOP1n2x: DO IL=1,MLEV(IT,IS)
               LOOP1n3x: DO I=1,NZZ(IS)
                  IF(ZZ(IL,IT,IS).EQ.ZZLEV(I,IS)) CYCLE LOOP1n2x
               ENDDO  LOOP1n3x
               NZZ(IS) = NZZ(IS) + 1
               ZZLEV(NZZ(IS),IS) = ZZ(IL,IT,IS)
            ENDDO  LOOP1n2x
         ENDDO  LOOP1n1x
      ENDDO  LOOP1x

C  SORT LIST OF HEIGHTS FOR EACH STATION
C  -------------------------------------

      IREV  = 0
      UZZ   = BMISS
      VZZ   = BMISS
      QZZ   = BMISS
      PZZ   = BMISS
      UFZZ  = BMISS
      VFZZ  = BMISS
      SZZ   = BMISS
      DZZ   = BMISS
      IQZZ  = 99999
      LEVZZ = 0

      DO IS=1,NS
         CALL SHELL(ZZLEV(1,IS),IV,NZZ(IS),IREV)
         DO IT=1,NTIMES(IS)
            DO IL=1,MLEV(IT,IS)
               DO ILL=1,NZZ(IS)
                  IF(NINT(ZZ(IL,IT,IS)).EQ.NINT(ZZLEV(ILL,IS))) THEN
                     UZZ(ILL,IT,IS)   = UZ(IL,IT,IS)
                     VZZ(ILL,IT,IS)   = VZ(IL,IT,IS)
                     QZZ(ILL,IT,IS)   = QZ(IL,IT,IS)
                     PZZ(ILL,IT,IS)   = PZ(IL,IT,IS)
                     UFZZ(ILL,IT,IS)  = UFZ(IL,IT,IS)
                     VFZZ(ILL,IT,IS)  = VFZ(IL,IT,IS)
                     SZZ(ILL,IT,IS)   = SZ(IL,IT,IS)
                     DZZ(ILL,IT,IS)   = DZ(IL,IT,IS)
                     IQZZ(ILL,IT,IS)  = IQ(IL,IT,IS)
                     LEVZZ(ILL,IT,IS) = LEVZ(IL,IT,IS)
                  ENDIF
               ENDDO
            ENDDO
            ZZ(:,IT,IS) = ZZLEV(:,IS)
         ENDDO
      ENDDO
      UZ   = UZZ
      VZ   = VZZ
      QZ   = QZZ
      PZ   = PZZ
      UFZ  = UFZZ
      VFZ  = VFZZ
      SZ   = SZZ
      DZ   = DZZ
      IQ   = IQZZ
      LEVZ = LEVZZ

C  SORT STATIONS BY D-TIME
C  -----------------------

      IREV = 0
      DO I=1,NS
         CALL SHELL(TIM(1,I),IV,NTIMES(I),IREV)
         WKSP = ZZ
         ZZ(:,1:NTIMES(I),I) = WKSP(:,IV(1:NTIMES(I)),I)
         WKSP = UZ
         UZ(:,1:NTIMES(I),I) = WKSP(:,IV(1:NTIMES(I)),I)
         WKSP = VZ
         VZ(:,1:NTIMES(I),I) = WKSP(:,IV(1:NTIMES(I)),I)
         WKSP = QZ
         QZ(:,1:NTIMES(I),I) = WKSP(:,IV(1:NTIMES(I)),I)
         WKSP = PZ
         PZ(:,1:NTIMES(I),I) = WKSP(:,IV(1:NTIMES(I)),I)
         WKSP = UFZ
         UFZ(:,1:NTIMES(I),I) = WKSP(:,IV(1:NTIMES(I)),I)
         WKSP = VFZ
         VFZ(:,1:NTIMES(I),I) = WKSP(:,IV(1:NTIMES(I)),I)
         WKSP = SZ
         SZ(:,1:NTIMES(I),I) = WKSP(:,IV(1:NTIMES(I)),I)
         WKSP = DZ
         DZ(:,1:NTIMES(I),I) = WKSP(:,IV(1:NTIMES(I)),I)
         IKSP = LEVZ
         LEVZ(:,1:NTIMES(I),I) = IKSP(:,IV(1:NTIMES(I)),I)
         IKSP = IQ
         IQ(:,1:NTIMES(I),I) = IKSP(:,IV(1:NTIMES(I)),I)
         JKSP = KDATE
         KDATE(:,1:NTIMES(I),I) = JKSP(:,IV(1:NTIMES(I)),I)
      ENDDO

      RETURN

  100 FORMAT(//'RESTRUCTURING (SORTING) VERTICAL LEVELS AND TIME ',
     $ 'PERIODS'//)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SHELL
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1991-12-24
C
C ABSTRACT: SHELL SORT, BASED UPON THE VALUES OF V.  IV IS THE
C   ORIGINAL INDEX OF EACH ELEMENT OF V.
C
C PROGRAM HISTORY LOG:
C 1991-12-24  W. COLLINS -- Original author.
C
C USAGE:    CALL SHELL(V,IV,MAX,IREV)
C   INPUT ARGUMENT LIST:
C     V        - VARIABLE
C     MAX      - DIMENSION OF V
C     IREV     = 0 FOR ASCENDING ORDER
C              > or < 0 FOR DESCENDING ORDER
C
C   OUTPUT ARGUMENT LIST:
C     IV       - ORIGINAL INDEX OF VARIABLE
C     V        - VARIABLE, SORTED
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE SHELL(V,IV,MAX,IREV)

      REAL V(*)
      INTEGER IV(*)

      DO I=1,MAX
        IV(I) = I
      ENDDO

      IOFSET = MAX/2

   20 CONTINUE

      LIM = MAX - IOFSET

   30 CONTINUE

      ISW = 0
      DO I=1,LIM
        IF(V(I).GT.V(I+IOFSET)) THEN
          VT = V(I)
          V(I) = V(I+IOFSET)
          V(I+IOFSET) = VT
          IVT = IV(I)
          IV(I) = IV(I+IOFSET)
          IV(I+IOFSET) = IVT
          ISW = I
        ENDIF
      ENDDO
      LIM = ISW - IOFSET
      IF(ISW.NE.0) GOTO 30
      IOFSET = IOFSET/2
      IF(IOFSET.GT.0) GOTO 20

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

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SORT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1994-03-17
C
C ABSTRACT: SORT RA ACCORDING TO THE ORDER SPECIFIED BY THE INDICES
C   IN INDX.
C
C PROGRAM HISTORY LOG:
C 1994-03-17  W. Collins -- Original author.
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
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE SORT(RA,INDX,N)

C     SORT RA ACCORDING TO THE ORDER SPECIFIED BY THE INDICES IN INDX.

      INTEGER  INDX(*)
      DIMENSION  RA(*),WKSP(899)

      WKSP(1:N) = RA(1:N)

      RA(1:N) = WKSP(INDX(1:N))

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    TWODIM
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: COMPUTE MEDIAN RESIDUALS FOR A SINGLE UNIQUE
C   STATION.
C
C PROGRAM HISTORY LOG:
C 1995-04-05  W. Collins -- Original author.
C 2004-09-09  D. Keyser  -- Lvl index in all arrays now site specific
C     to allow proper temporal checking at each individual site times
C     w/ new lvls inserted get missing wind). No. of time periods input
C     now site specific (was hardwired to be same for all rpts).
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL TWODIM(IS)
C   INPUT ARGUMENT LIST:
C     IS       - STATION INDEX
C
C REMARKS:
C   VARIABLE NAMES:
C     U        - U-COMPONENT
C     V        - V-COMPONENT
C     ICQC     - DMA INDEX FOR WIND QUALITY CONTROL
C     UM       - MEDIAN RESIDUAL FOR U-COMPONENT
C     VM       - MEDIAN RESIDUAL FOR V-COMPONENT
C     VVM      - VECTOR MEDIAN RESIDUAL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE TWODIM(IS)

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      CHARACTER*8  STN
      REAL  U(NL,NT),V(NL,NT),TIN(3,3),TUT(NL,NT)
      REAL(8)  BMISS

      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON/PROFZ/ZZ(NL,NT,NS),UZ(NL,NT,NS),VZ(NL,NT,NS),QZ(NL,NT,NS),
     $ PZ(NL,NT,NS),UFZ(NL,NT,NS),VFZ(NL,NT,NS),SZ(NL,NT,NS),
     $ DZ(NL,NT,NS),LEVZ(NL,NT,NS)
      COMMON/RESIDS/UI(NL,NT,NS),VI(NL,NT,NS),UT(NL,NT),VT(NL,NT),
     $ UV(NL,NT),VV(NL,NT),VVI(NL,NT),VVT(NL,NT),VVV(NL,NT),UM(NL,NT),
     $ VM(NL,NT),VVM(NL,NT),NI(NL,NT),NTMP(NL,NT),NV(NL,NT),NM(NL,NT),
     $ ZI(NL,NT),ZT(NL,NT),ZV(NL,NT),ZM(NL,NT),DIT(NL,NT),DIV(NL,NT),
     $ DTV(NL,NT),DIM(NL,NT),DVM(NL,NT),DTM(NL,NT)
      COMMON/QUAL/QM(NL,NT,NS),IQ(NL,NT,NS),ICQC(NL,NT,NS),
     $ IQCC(NL,NT,NS),MLEV(NT,NS),DT(NL,NT,NS),NZZ(NS),IBAD_FLAG(NT,NS),
     $ JBAD_FLAG(NS),IRC(NL,NT,NS)
      COMMON/PARAMS/NCH,NLEV,NSCAN,NSTN,NSTN1,NREP,NREP1,NREP_OUT,
     $ NFIN,NFOUT
      COMMON /BUFRLIB_MISSING/BMISS

      UM  = BMISS
      VM  = BMISS
      VVM = BMISS

      U(1:NL,1:NTIMES(IS)) = UZ(1:NL,1:NTIMES(IS),IS)
      V(1:NL,1:NTIMES(IS)) = VZ(1:NL,1:NTIMES(IS),IS)

C     INPUT FIELD, TIN, WILL CONTAIN MISSING FOR EITHER MISSING OR
C     'BAD' VALUES.

C     U FIRST.

      DO L=1,NTIMES(IS)
        JSTART = 1
        JEND   = 3
        IF(L.EQ.1)          JSTART=2
        IF(L.EQ.NTIMES(IS)) JEND  =2
        DO K=2,NZZ(IS)-1
          TIN = BMISS
          DO J=JSTART,JEND
            DO I=1,3
              IF(U(K+I-2,L+J-2).LT.BMISS .AND.
     $           ICQC(K+I-2,L+J-2,IS).EQ.0) THEN
                TIN(I,J) = U(K+I-2,L+J-2)
              ENDIF
            ENDDO
          ENDDO

C         DO INTERPOLATION.  TUT IS ARRAY OF INTERPOLATED VALUES.

          CALL PLINT(TIN,TUT(K,L),BMISS)

C         CALCULATE 2 D RESIDUAL, UM.

          IF(MAX(U(K,L),TUT(K,L)).LT.BMISS) THEN
            UM(K,L) = U(K,L) - TUT(K,L)
          ELSE
            UM(K,L) = BMISS
          ENDIF

C     AND NOW V.

          TIN = BMISS
          DO J=JSTART,JEND
            DO I=1,3
              IF(V(K+I-2,L+J-2).LT.BMISS .AND.
     &           ICQC(K+I-2,L+J-2,IS).EQ.0) THEN
                TIN(I,J) = V(K+I-2,L+J-2)
              ENDIF
            ENDDO
          ENDDO

C         DO INTERPOLATION.  TUT IS FIELD OF INTERPOLATED VALUES.

          CALL PLINT(TIN,TUT(K,L),BMISS)

C         COMPUTE 2 D RESIDUAL, VM.

          IF(MAX(V(K,L),TUT(K,L)).LT.BMISS) THEN
            VM(K,L) = V(K,L) - TUT(K,L)
          ELSE
            VM(K,L) = BMISS
          ENDIF

C           AND COMPUTE VECTOR RESIDUAL.
          IF(MAX(UM(K,L),VM(K,L)).LT.BMISS) THEN
            VVM(K,L) = SQRT(UM(K,L)**2 + VM(K,L)**2)
          ELSE
            VVM(K,L) = BMISS
          ENDIF
        ENDDO
      ENDDO

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    PSTAT
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: DRIVER SUBROUTINE TO COMPUTE AND PRINT MOMENT
C   STATISTICS. CALLS SUBROUTINE MSTATS TO ACTUALLY DO THE WORK
C   FOR A PARTICULAR FIELD AND REPORT TYPE.
C
C PROGRAM HISTORY LOG:
C 1995-04-05  W. Collins -- Original author.
C 2004-09-09  D. Keyser  -- Stats generated separately for NPN, CAP &
C     JMA rpts.
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL PSTAT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE PSTAT

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      LOGICAL  STATS,MEAN,STDDEV,SKEW,KURT
      CHARACTER*8  STN

      COMMON/STATZ/UZS(120,0:24,9,3:8),VZS(120,0:24,9,3:8),
     $ UIS(120,0:24,9,3:8),VIS(120,0:24,9,3:8),UTS(120,0:24,9,3:8),
     $ VTS(120,0:24,9,3:8),UVS(120,0:24,9,3:8),VVS(120,0:24,9,3:8),
     $ VVIS(120,0:24,9,3:8),VVTS(120,0:24,9,3:8),VVVS(120,0:24,9,3:8),
     $ VVMS(120,0:24,9,3:8),UMS(120,0:24,9,3:8),VMS(120,0:24,9,3:8)
      COMMON/STNLST/STN(NS),X(NS),Y(NS),Z0(NS),NTIMES(NS),TIM(NT,NS),
     $ IRTP(NS)
      COMMON/PST/IOFFSET(3:8),LVLINCR(3:8),ILEVELS(3:8),STATS,MEAN,
     $ STDDEV,SKEW,KURT

C  CALL MSTATS TO PROCESS AND PRINT DESIRED STATISTICS
C  --------------------------------------------------

      CALL MSTATS(UZS,3,'U-COMPONENT WIND',1)
      CALL MSTATS(UZS,7,'U-COMPONENT WIND',1)
      CALL MSTATS(UZS,8,'U-COMPONENT WIND',1)

      CALL MSTATS(VZS,3,'V-COMPONENT WIND',2)
      CALL MSTATS(VZS,7,'V-COMPONENT WIND',2)
      CALL MSTATS(VZS,8,'V-COMPONENT WIND',2)

      CALL MSTATS(UIS,3,'U-INCREMENT',3)
      CALL MSTATS(UIS,7,'U-INCREMENT',3)
      CALL MSTATS(UIS,8,'U-INCREMENT',3)

      CALL MSTATS(VIS,3,'V-INCREMENT',4)
      CALL MSTATS(VIS,7,'V-INCREMENT',4)
      CALL MSTATS(VIS,8,'V-INCREMENT',4)

      CALL MSTATS(UTS,3,'U-TEMPORAL RESIDUAL',5)
      CALL MSTATS(UTS,7,'U-TEMPORAL RESIDUAL',5)
      CALL MSTATS(UTS,8,'U-TEMPORAL RESIDUAL',5)

      CALL MSTATS(VTS,3,'V-TEMPORAL RESIDUAL',6)
      CALL MSTATS(VTS,7,'V-TEMPORAL RESIDUAL',6)
      CALL MSTATS(VTS,8,'V-TEMPORAL RESIDUAL',6)

      CALL MSTATS(UVS,3,'U-VERTICAL RESIDUAL',7)
      CALL MSTATS(UVS,7,'U-VERTICAL RESIDUAL',7)
      CALL MSTATS(UVS,8,'U-VERTICAL RESIDUAL',7)

      CALL MSTATS(VVS,3,'V-VERTICAL RESIDUAL',8)
      CALL MSTATS(VVS,7,'V-VERTICAL RESIDUAL',8)
      CALL MSTATS(VVS,8,'V-VERTICAL RESIDUAL',8)

      CALL MSTATS(UMS,3,'U-2-DIM MEDIAN RESIDUAL',9)
      CALL MSTATS(UMS,7,'U-2-DIM MEDIAN RESIDUAL',9)
      CALL MSTATS(UMS,8,'U-2-DIM MEDIAN RESIDUAL',9)

      CALL MSTATS(VMS,3,'V-2-DIM MEDIAN RESIDUAL',10)
      CALL MSTATS(VMS,7,'V-2-DIM MEDIAN RESIDUAL',10)
      CALL MSTATS(VMS,8,'V-2-DIM MEDIAN RESIDUAL',10)

      CALL MSTATS(VVIS,3,'VECTOR INCREMENT',11)
      CALL MSTATS(VVIS,7,'VECTOR INCREMENT',11)
      CALL MSTATS(VVIS,8,'VECTOR INCREMENT',11)

      CALL MSTATS(VVTS,3,'VECTOR TEMPORAL RESIDUAL',12)
      CALL MSTATS(VVTS,7,'VECTOR TEMPORAL RESIDUAL',12)
      CALL MSTATS(VVTS,8,'VECTOR TEMPORAL RESIDUAL',12)

      CALL MSTATS(VVVS,3,'VECTOR VERTICAL RESIDUAL',13)
      CALL MSTATS(VVVS,7,'VECTOR VERTICAL RESIDUAL',13)
      CALL MSTATS(VVVS,8,'VECTOR VERTICAL RESIDUAL',13)

      CALL MSTATS(VVMS,3,'VECTOR 2-DIM MEDIAN RESIDUAL',14)
      CALL MSTATS(VVMS,7,'VECTOR 2-DIM MEDIAN RESIDUAL',14)
      CALL MSTATS(VVMS,8,'VECTOR 2-DIM MEDIAN RESIDUAL',14)

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MSTATS
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2016-12-20
C
C ABSTRACT: COMPUTES THE VARIOUS MOMENT STATISTICS AND PRINTS
C   TO FILES.
C
C PROGRAM HISTORY LOG:
C 1995-04-05  W. Collins -- Original author.
C 2004-09-09  D. Keyser  -- Kurtotis stat. now computed correctly.
C     Stat. output clarified & written to 2 text files separate from
C     stdout.  Computation & printout of stats, as well as combination
C     of stat types printed out controlled via new namelist switches.
C     Stats generated separately for NPN, CAP & JMA rpts and stratified
C     according to hgt above ground.
C 2016-12-20  D. Stokes   Increase the max allowable number of times 
C     per station.
C
C USAGE:    CALL MSTATS(FIELD,ITYPE,TITLE,NUM)
C   INPUT ARGUMENT LIST:
C     FIELD    - INPUT FIELD
C     ITYPE    - PROFILER REPORT TYPE (=3-NPN, =7-CAP, =8=JMA)
C     TITLE    - CHARACTER TITLE FOR FIELD
C     IFLD     - INDICATOR FOR TYPE OF FIELD (=1-U-COMP WIND,
C                =2-V-COMP WIND, =3-U-INCR, =4-V-INCR, =5-U-TEMPORAL
C                RESID, =6-V-TEMPORAL RESID, =7-U-VERTICAL RESID,
C                =8-V-VERTICAL RESID, =9-U-MEDIAN RESID, =10-V-MEDIAN
C                RESID, =11-VECTOR INCR, =12-VECTOR TEMPORAL RESID,
C                =13-VECTOR VERTICAL RESID, =14-VECTOR MEDIAN RESID)
C
C   OUTPUT FILES:
C     UNIT 61  - PRINTOUT OF SELECTED STATISTICS OF RESULTS ON EACH 
C                LEVEL (BY REPORT TYPE, REPORT TIME AND FIELD)
C     UNIT 62  - STATISTICS FILE CONTAINING ALL STATISTICS ON EACH
C                LEVEL (BY REPORT TYPE (BY REPORT TYPE AND FIELD, BUT
C                OVER ALL TIMES COMBINED)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE MSTATS(FIELD,ITYPE,TITLE,IFLD)

      PARAMETER  (NL=150,NT=100,NS=120)    ! (levels,times,stations)

      LOGICAL  STATS,LPRNT
      CHARACTER*8   STATISTIC(4)
      CHARACTER*(*) TITLE
      CHARACTER*36  TEXT(3:8)
      REAL  FIELD(120,0:24,9,3:8)
      INTEGER  NTIM(26),IZ(120)

      COMMON/DATEZ/JDATE(4),KDATE(5,NT,NS),MDATE(5),NDATE(5)
      COMMON/PST/IOFFSET(3:8),LVLINCR(3:8),ILEVELS(3:8),STATS,LPRNT(4)

      DATA  STATISTIC/'  MEAN  ','STD DEV.','SKEWNESS','KURTOSIS'/
      DATA  TEXT/'NOAA Profiler Network (NPN) Reports:',
     $           'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
     $           'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
     $           'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
     $           'Cooperative Agency Profiler Reports:',
     $           'Jap. Met. Agency Profiler Reports:  '/

      IF(ITYPE.NE.3.AND.ITYPE.NE.7.AND.ITYPE.NE.8)  THEN
        write(61,*)
        write(61,'("#### Mean and higher order statistics for field ",
     $   A," cannot be computed or printed because wind profiler ",
     $   "report type (=",I0,") is not valid")') TITLE,ITYPE
        write(61,*)
        RETURN
      ENDIF
       
      ITEST = 0
      INDX  = 0
      IADD = 0
      IF(MDATE(5).NE.0)  THEN
         IADD = 1
         IF(MDATE(4).EQ.23)  IADD = -23
      ENDIF
      NDATE4 = NDATE(4)
      IF(NDATE(4).LT.MDATE(4)+IADD)  NDATE4 = NDATE(4) + 24
      DO NN=MDATE(4)+IADD,NDATE4+1
        IF(NN.LT.NDATE4+1) THEN
           INDX = INDX + 1
           N = NN
           IF(N.GT.23)  N = N - 24
           NTIM(INDX) = N
        ELSE
           N = 24
        ENDIF
        DO L=1,ILEVELS(ITYPE)
          IF(NINT(FIELD(L,N,1,ITYPE)).LT.1) CYCLE
          ITEST = 1
          IZ(L) = (L * LVLINCR(ITYPE)) + IOFFSET(ITYPE)
          SUM1 = FIELD(L,N,2,ITYPE)/FIELD(L,N,1,ITYPE)
          SUM2 = FIELD(L,N,3,ITYPE)/FIELD(L,N,1,ITYPE)
          SUM3 = FIELD(L,N,4,ITYPE)/FIELD(L,N,1,ITYPE)
          SUM4 = FIELD(L,N,5,ITYPE)/FIELD(L,N,1,ITYPE)

C  MEAN (m/s)
C  ----------
          FIELD(L,N,6,ITYPE) = SUM1

C  STANDARD DEVIATION (m/s)
C  ------------------------

          ARG = SUM2 - SUM1**2
          IF(ARG.GT.0.) FIELD(L,N,7,ITYPE) = SQRT(ARG)

C  SKEWNESS (m/s)
C  --------------

          S1 = SUM3 - 3.*SUM2*SUM1 + 2.*SUM1**3
          IF(SUM2.GT.0.) FIELD(L,N,8,ITYPE) = S1/(SUM2)**1.5

C  KURTOSIS (m/s)
C  --------------

          S1 = SUM4 - 4.*SUM3*SUM1 + 6.*SUM2*(SUM1**2) - 3.*SUM1**4
          IF(FIELD(L,N,7,ITYPE).GT.0.001) FIELD(L,N,9,ITYPE) =
     $     (S1/FIELD(L,N,7,ITYPE)**4) - 3.0

C  PRINT STATISTICS TO FILE (ONLY OVER ALL TIME PERIODS)
C  -----------------------------------------------------

          IF(N.EQ.24) WRITE(62,100) ITYPE,IFLD,IZ(L),
     $     (FIELD(L,24,J,ITYPE),J=1,9)

        ENDDO
      ENDDO

C  PRINT SELECTED STATISTICS (OVER SEPARATE HOURS) (USER-FRIENDLY FORM)
C  -------------------------------------------------------------------

      IF(INDX.GT.0.AND.INDX.LT.24)  THEN
        WRITE(61,101) TEXT(ITYPE),TITLE
        IF(ITEST.EQ.0)  THEN
          WRITE(61,102)
          RETURN
        ENDIF
        DO ISTAT=1,4
          IF(.NOT.LPRNT(ISTAT)) CYCLE
          JSTAT = ISTAT + 5
          WRITE(61,103) STATISTIC(ISTAT)
          WRITE(61,104) (NTIM(I),I=1,INDX)
          DO L=1,ILEVELS(ITYPE)
            IF(NINT(FIELD(L,24,1,ITYPE)).LE.1) CYCLE
            WRITE(61,105) IZ(L),(FIELD(L,NTIM(N),JSTAT,ITYPE),
     $       NINT(FIELD(L,NTIM(N),1,ITYPE)),N=1,INDX),
     $       FIELD(L,24,JSTAT,ITYPE),NINT(FIELD(L,24,1,ITYPE))
          ENDDO
          WRITE(61,106)
        ENDDO
      ENDIF

      RETURN

  100 FORMAT(1X,I3,1X,I3,',',I6,',',F8.0,',',8(F12.2,','))
  101 FORMAT(/128('*')/'==> Statistics for ',A36//'Field: ',A/)
  102 FORMAT(/'  ~~~~~> No reports of this type present'/)
  103 FORMAT(35X,A8,'/COUNT'//'~LEVEL(m)    Hour (UTC) ==============>')
  104 FORMAT('above ground',<INDX>(14X,I2.2),14X,'ALL'/'------------',
     $ <INDX>(14X,'--'),14X,'---')
  105 FORMAT(I9,6X,<INDX+1>(F12.2,'/',I3))
  106 FORMAT(//)

      END
