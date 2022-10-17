C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: PREPOBS_CQCVAD
C   PRGMMR: MELCHIOR         ORG: NP22        DATE: 2016-12-18
C
C ABSTRACT: PERFORM COMPLEX QUALITY CONTROL OF VAD WINDS FROM
C   WSR-88D RADARS.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS  ORIGINAL AUTHOR
C 1999-10-26  W. COLLINS  ADD DIAGNOSIS TO SCREEN BIRD INFLUENCE
C 2000-10-26  W. COLLINS  GENERALIZE TO ALLOW ANY HEIGHT UP TO
C       35,000 FT.
C 2001-10-08  D. KEYSER   CODE STREAMLINED; NOW TABULATES PERCENT
C       OF LEVELS < 18K FAILING BIRD ALGORITM CHECK ACCORDING TO
C       STATION AND 1-HOUR TIME WINDOWS CENTERED ON 30-MIN PAST
C       HOUR, THIS BIRD Q.C. INFORMATION WRITTEN TO UNIT 52 FOR
C       LATER USE BY 3DVAR IN Q.C. OF NEXRAD RADIAL VELOCITY DATA;
C       CORRECTED ERROR WHICH HAD RESULTED IN NO Q.C. PERFORMED ON
C       LEVELS WHERE EITHER THE U- OR V- INCREMENT WAS ZERO; VALID
C       DATES FOR CHECKING FOR MIGRATING BIRDS EXPANDED FROM 2/15
C       THROUGH 6/15 TO 2/1 THROUGH 6/30 IN SPRING AND FROM 8/15
C       THROUGH 11/15 TO 8/1 THROUGH 11/30 IN FALL; LIMITING
C       V-WIND INCREMENT FOR APPLYING BIRD CONTAMINATION TIGHTENED
C       FROM +8 TO +5 M/S IN SPRING AND FROM -8 TO -5 M/S IN FALL,
C       MORE LEVELS WILL NOW BE FLAGGED
C 2012-03-05  S. MELCHIOR  CODE FURTHER STREAMLINED. CORRECTED VARIOUS
C       TYPO'S IN LEGACY DOCUMENTATION. ADDED MORE CONCISE DESCRIPTIONS
C       TO LEGACY DOCUMENTATION. ADDED MORE COMPLETE DIAGNOSTIC OUTPUT
C       STATEMENTS FOR INSTANCES WHEN SPECIFIC DO LOOPS ARE OUT OF
C       BOUNDS AND WHEN CONDITIONAL STATEMENTS ARE NOT MET. ADDED MORE
C       CONCISE COMMENTS TO IDENTIFY DO LOOP BOUNDARIES. CORRECTED A DO
C       LOOP TERMINUS PARAMETER FROM NLEV TO NLV IN SUBROUTINE COMSTAT.
C       ADDED A MISSING RETURN STATEMENT TO SUBROUTINE HT. UPDATED
C       SUBROUTINE DISTR TO BE MORE FLEXIBLE AND UTILIZE NDIV AND NX AS
C       ARRAY LIMITS RATHER THAN THE HARD CODED 23 AND 899,
C       RESPECTIVELY. EXTENDED MSK ARRAY ALLOCATION FROM 899 TO 1000 IN
C       SUBROUTINES INCDIST AND RESDIST TO ALLOW FOR MORE STATION
C       STATISTICS TO BE PROCESSED - ON OCCASION THE 899 LIMIT HAD BEEN
C       MET AND EXCEEDED CAUSING MEMORY LEAKS. UPDATED SUBROUTINE
C       EVNOUT TO USE INCOMING ARGUMENT NLV TO DEFINE MEMORY ALLOCATION
C       FOR VARIOUS REAL AND INTEGER ARRAYS. THIS MAKES THE SUBROUTINE
C       MORE ROBUST AND EFFICIENT BY ONLY ALLOTTING THE MEMORY SPACE
C       NECESSARY, NOTHING MORE. CREATED SEVERAL NEW PARAMETERS FOR
C       VARIOUS SUBROUTINES TO TIDY UP THE CODE. NTIMES=6 HAS BEEN
C       ADDED TO COMSTAT, GETDAT, INCDIST, INCR, INIT, MATR, DMA,
C       RESDIST, SELECT, SOLVE, ZTOI, AND ZTRES.  NINC=3 HAS BEEN ADDED
C       TO COMSTAT, GETDAT, INCDIST, INCR, INIT, MATR, DMA, RESDIST,
C       SELECT, SOLVE, AND ZTRES.  NEVNT=160000 WAS ADDED TO DMA AND
C       EVNOUT. NDIV=23 HAS BEEN ADED TO INCDIST AND RESDIST.  RENAMED
C       PARAMETER NE TO NME FOR CLARIFICATION SINCE NE IS DIFFICULT TO
C       SEARCH ON AND IS ALSO USED FOR INEQUALITY COMPARISON TESTING
C       (.NE.).  ADDED LOGIC TO TURN OFF BIRD CHECK ALGORITHM FOR VAD
C       WIND REPORTS FROM LEVEL 2 DECODER.  INCREASED NRPT FROM 80000
C       TO 160000 TO ACCOMMODATE VAD WIND REPORTS FROM LEVEL 2 DECODER.
C       CORRECTED BUG IN SUBROUTINE GETDAT TO PROPERLY HANDLE SITUATION
C       WHEN SUBSET IS VADWND AND THE NUMBER OF LEVELS (NUM) EXCEEDS
C       NRPT (160000).
C 2012-11-20  J. WOOLLEN  INITIAL PORT TO WCOSS 
C 2013-02-05  D. Keyser   Final changes to run on WCOSS:  Set BUFRLIB
C       missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C       overflow; rename all REAL(8) variables as *_8.
C 2014-01-15  S. Melchior  Increased NRPT (total number of levels
C       amongst all VAD reports that can be processed) and NEVNT (total
C       number of events amongst all VAD reports that can be processed)
C       both from 160000 to 500000 to accommodate VAD wind reports from
C       Level 2 decoder.
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.  Made minor correction in GETDAT.
C 2022-07-21  M. Sienkiewicz  Increased NRPT and NEVNT from 500000 to
C       800000 to accommodate recent increase in numbers of reports 
C
C USAGE:
C   INPUT FILES:
C     UNIT 11  - INPUT PREPBUFR FILE
C     UNIT 14  - 10-DIGIT TIME (YYYYMMDDHH)
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - OUTPUT PREPBUFR FILE
C     UNIT 52  - BIRD Q.C. INFORMATION LIST
C     UNIT 53  - FINAL LISTING OF REPORTS WITH Q.C. INFO
C     UNIT 55  - ????????????????????????????
C     UNIT 60  - LISTING OF EVENT INFORMATION
C
C   SUBPROGRAMS CALLED:
C     SYSTEM:    - SYSTEM
C     UNIQUE:    - INIT     GETDAT   INCR     ZTOI     DMA
C                  INCDIST  COMSTAT  RESDIST  CSTATS   VSOLVE
C                  EVENTW   GETOBV   EVNOUT   DISTR    HT
C                  SORTD    DRCTSL   INDEXX   SELECT   MATR
C                  SOLVE    ZTRES    CUMPROB  ANLWT    BLOCK DATA
C     LIBRARY:
C       W3NCO    - W3TAGB   W3TAGE   W3MOVDAT W3DIFDAT
C       BUFRLIB  - DATELEN  CLOSBF   OPENBF   READMG   UFBQCD
C                  CLOSMG   COPYMG   OPENMB   READSB   UFBCPY
C                  WRITSB   UFBINT   SETBMISS GETBMISS
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS:
C
C    CONTENTS OF NAMELIST SWITCH "NAMLST"
C        HONOR_FLAGS - LOGICAL, IF TRUE THEN LEVELS WITH BAD Q. MARK
C                      FLAGS ARE HONORED                  (DEFAULT=TRUE)
C        PRINT_52    - LOGICAL, IF TRUE THEN WRITES BIRD QUALITY
C                      CONTROL INFORMATION TO UNIT 52     (DEFAULT=TRUE)
C        PRINT_53    - LOGICAL, IF TRUE THEN WRITES A FINAL REPORT
C                      LISTING WITH Q.C. INFORMATION TO UNIT 53
C                                                        (DEFAULT=FALSE)
C        PRINT_60    - LOGICAL, IF TRUE THEN WRITES EVENT INFORMATION
C                      TO UNIT 60                        (DEFAULT=FALSE)
C        TEST        - LOGICAL, IF TRUE THEN WRITES DIAGNOSTIC PRINT
C                      TO STDOUT (UNIT 06)               (DEFAULT=FALSE)
C        
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      PROGRAM PREPOBS_CQCVAD

      PARAMETER(NLEV=35)

      integer idat(8),jdat(8),idate_e(4,-3:3),idate_l(4,-3:3)
      real    rinc(5)
      LOGICAL HONOR_FLAGS, PRINT_52, PRINT_53, PRINT_60, TEST, guess
      character*8 stnidx
      REAL(8)   BMISS,GETBMISS

      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /BIRDST/stnidx(9999,-3:3),alatx(9999,-3:3),
     & alonx(9999,-3:3),knt_5_yes(9999,-3:3),knt_5_no(9999,-3:3),
     & numrpt(9999,-3:3)
      common /datet/  idate(4), itim(6)
      common /switches/  print_52, print_53, print_60, test
      common /flag/   guess
      COMMON /BUFRLIB_MISSING/BMISS

      NAMELIST /NAMLST/ HONOR_FLAGS, PRINT_52, PRINT_53, PRINT_60, TEST

      CALL W3TAGB('PREPOBS_CQCVAD',2016,0353,1200,'NP22')

      PRINT *, ' ==> WELCOME TO CQCVAD, VERSION 2016-12-18 <=='
      PRINT *, ' '

C  Set up default values for namelist switches
C  -------------------------------------------

      HONOR_FLAGS = .TRUE.
      PRINT_52    = .TRUE.
      PRINT_53    = .FALSE.
      PRINT_60    = .FALSE.
      TEST        = .FALSE.

      READ(5,NAMLST,ERR=10,END=10)
   10 CONTINUE
      WRITE(6,NAMLST)

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

C  PROGRAM TO QUALITY CONTROL VADWINDS
C  -----------------------------------

      CALL INIT

C  GET DATA AND PUT INTO LINEAR ARRAY
C  ----------------------------------

      ITIME = 1
      CALL GETDAT(ITIME)

C  OBTAIN INCREMENTS
C  -----------------

      CALL INCR

C  PERFORM OI IN HEIGHT-TIME PLANE (unless no first guess)
C  -------------------------------------------------------

      if (guess) CALL ZTOI

C  DECISION-MAKING ALGORITHM, ALSO SAVE EVENTS
C  -------------------------------------------

      CALL DMA(HONOR_FLAGS)

C  GET STATISTICAL DISTRIBUTIONS OF INCREMENTS (unless no first guess)
C  -------------------------------------------------------------------

      if (guess) CALL INCDIST

C  COMPUTE STATISTICS BASED UPON DELTA-T AND DELTA-Z
C  -------------------------------------------------

      CALL COMSTAT

C  GET STATISTICAL DISTRIBUTION OF RESIDUALS (unless no first guess)
C  -----------------------------------------------------------------

      if (guess) CALL RESDIST

C  READ DATA A SECOND TIME AND WRITE EVENTS
C  ----------------------------------------

      ITIME = 2
      CALL GETDAT(ITIME)

      if(print_52)  then

C  WRITE BIRD Q.C. INFORMATION TO UNIT 52
C  --------------------------------------

         write(52,'("===> VAD WIND BIRD MIGRATION Q.C. INFORMATION")')
         write(52,'(/6x,"Only levels below 18,000 ft are considered")')
         write(52,'(6x,"Stations are parsed into 1-hour time blocks")')
         write(52,'(/6x,"PREPBUFR CENTER TIME IS: ",i4,3i2.2//)') idate
         write(52,'(" STNID   LAT(N)   LON(E)",9x,"TIME RANGE",10x, ' //
     &    '"# LVLS =< 18K FT    # OF RPTS    % LVLS FLAGGED BY BIRD",'//
     &    '" ALG.")')
         write(52,'(1x,5("-"),3x,6("-"),3x,6("-"),3x,23("-"),3x,' //
     &    '16("-"),4x,9("-"),4x,27("-")/)')
         idate_e   = 0
         idate_l   = 0
         idat      = 0
         idat(1:3) = idate(1:3)
         idat(5)   = idate(4)
         rinc      = 0
         do  jtim = -3,3
            rinc(2)   = jtim - 1
            if(jtim.eq.-3)  then
               call w3movdat(rinc,idat,jdat)
               idate_e(1:3,jtim) = jdat(1:3)
               idate_e(4,jtim)   = jdat(5)
            else
               idate_e(:,jtim) = idate_l(:,jtim-1)
               jdat = idat
            end if
            idat = 0
            rinc(2)   = 1
            call w3movdat(rinc,jdat,idat)
            idate_l(1:3,jtim) = idat(1:3)
            idate_l(4,jtim)   = idat(5)
         end do
         do irep = 1,9999
            do jtim = -3,3
               if(stnidx(irep,jtim)(1:1).ne.'X')  then
                  if(knt_5_yes(irep,jtim)+knt_5_no(irep,jtim).gt.0) then
                     percent_5 = (knt_5_yes(irep,jtim)*100.)/
     &                        (knt_5_yes(irep,jtim)+knt_5_no(irep,jtim))
                  else
                     percent_5 = 0.
                  end if
               write(52,501) stnidx(irep,jtim),alatx(irep,jtim),
     &                       alonx(irep,jtim),idate_e(:,jtim),
     &                       idate_l(:,jtim),
     &                       knt_5_yes(irep,jtim)+knt_5_no(irep,jtim),
     &                       numrpt(irep,jtim),percent_5
 501  format(a8,1x,f6.2,3x,f6.2,3x,i4,3i2.2,' - ',i4,3i2.2,8x,i5,12x,i3,
     & 19x,f5.1)
               end if
            end do
         end do
      end if

      CALL W3TAGE('PREPOBS_CQCVAD')

      STOP
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BLOCK DATA
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1999-08-18
C
C ABSTRACT: BLOCK DATA
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  RENAMED NE TO NME FOR EASE OF NAVIGATION AND
C       FOR CLARIFICATION SINCE NE IS ALSO USED FOR INEQUALITY TESTING
C       (.NE.). INCREASED NEV FROM 80000 TO 160000 TO ACCOMMODATE VAD
C       WIND REPORTS FROM LEVEL 2 DECODER.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      BLOCK DATA
      PARAMETER (NLEV=35)
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /HTS/ IZLEV(NLEV), ZLEV(NLEV)
      COMMON /CONSTS/ MAT,E,AA,B,XL,R
      COMMON /BIRDST/stnidx(9999,-3:3),alatx(9999,-3:3),
     & alonx(9999,-3:3),knt_5_yes(9999,-3:3),knt_5_no(9999,-3:3),
     & numrpt(9999,-3:3)
      common /flag/   guess
      character*8 stnidx
      logical guess
      DATA IUNIT /11/, IUTIM /14/, IOUT /51/
      DATA CON /304.8/
      DATA IHE /-3/
      DATA NEV /160000/
      DATA IZLEV /1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
     &  19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35/
      DATA MAT /9/, E /.1/, B /.67/, XL /2.3/, R /1.5/
      DATA AA /.25/
      data stnidx/69993*'XXXXXXXX'/
      data alatx/69993*99999./,alonx/69993*99999./
      data knt_5_yes/69993*0/,knt_5_no/69993*0/,numrpt/69993*0/
      data guess/.true./
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: COMSTAT
C   PRGMMR: S. Melchior      ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT: COMPUTE STATISTICS FOR HEIGHT-TIME INCREMENT INTERPOLATION.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  CORRECTED DO LOOP TERMINUS PARAMETER FROM
C       NLEV TO NLV. ALSO ADDED PARAMETERS NTIMES=6 AND NINC=3 TO TIDY
C       UP THE CODE.  RENAMED NE TO NME FOR EASE OF NAVIGATION AND FOR
C       CLARIFICATION SINCE NE IS ALSO USED FOR INEQUALITY TESTING
C       (.NE.). INCREASED NEV FROM 80000 TO 160000 TO ACCOMMODATE VAD
C       WIND REPORTS FROM LEVEL 2 DECODER.
C 2014-01-15  S. Melchior  Increased NRPT (total number of levels
C       amongst all VAD reports that can be processed) from 160000 to
C       500000 to accommodate VAD wind reports from Level 2 decoder.
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C 2022-07-21  M. Sienkiewicz  Increased NRPT and NEVNT from 500000 to
C       800000 to accommodate recent increase in numbers of reports 
C
C USAGE:    CALL COMSTAT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE COMSTAT

      PARAMETER (NL=34,NTIM=5,NTIMES=6,NRPT=800000,NSTN=300)
      PARAMETER (NLEV=35,NINC=3)
      INTEGER N12(0:NL,0:NTIM)
      REAL    U1(0:NL,0:NTIM),   V1(0:NL,0:NTIM),
     &        U1S(0:NL,0:NTIM),  V1S(0:NL,0:NTIM),
     &        U2(0:NL,0:NTIM),   V2(0:NL,0:NTIM),
     &        U2S(0:NL,0:NTIM),  V2S(0:NL,0:NTIM),
     &        U12(0:NL,0:NTIM),  V12(0:NL,0:NTIM),
     &        U1B(0:NL,0:NTIM),  U2B(0:NL,0:NTIM),
     &        V1B(0:NL,0:NTIM),  V2B(0:NL,0:NTIM),
     &        U1SD(0:NL,0:NTIM), V1SD(0:NL,0:NTIM),
     &        U2SD(0:NL,0:NTIM), V2SD(0:NL,0:NTIM),
     &        U12V(0:NL,0:NTIM), V12V(0:NL,0:NTIM),
     &        U12R(0:NL,0:NTIM), V12R(0:NL,0:NTIM)
      REAL(8)   BMISS

      COMMON /SINGLE/ ZOB(NRPT), ITM(NRPT),TIM(NRPT),
     &                UOB(NRPT), VOB(NRPT), LOB(NRPT),
     &                XLA(NRPT), XLO(NRPT), ZST(NRPT),
     &                UFF(NRPT), VFF(NRPT), TFF(NRPT),
     &                WQM(NRPT)
      COMMON /INCS/   UIN(NLEV,NTIMES,NINC,NSTN),
     &                VIN(NLEV,NTIMES,NINC,NSTN),
     &                UUU(NLEV,NTIMES,NINC,NSTN),
     &                VVV(NLEV,NTIMES,NINC,NSTN),
     &                TIMIN(NLEV,NTIMES,NINC,NSTN),
     &                NIN(NLEV,NTIMES,NSTN),
     &                LLL(NLEV,NTIMES,NINC,NSTN),
     &                TFC(NLEV,NTIMES,NINC,NSTN),
     &                QQQ(NLEV,NTIMES,NINC,NSTN)
      COMMON /DATET/  IDATE(4), ITIM(6)
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /BUFRLIB_MISSING/BMISS
      COMMON /HTS/    IZLEV(NLEV), ZLEV(NLEV)
      COMMON /DMATYP/ IQC(NLEV,NTIMES,NINC,NSTN)

C  INITIALIZE STATISTICS
C  ---------------------

      N12  = 0
      U1   = 0.
      V1   = 0.
      U1S  = 0.
      V1S  = 0.
      U2   = 0.
      V2   = 0.
      U2S  = 0.
      V2S  = 0.
      U12  = 0.
      V12  = 0.
      U1B  = 0.
      U2B  = 0.
      V1B  = 0.
      V2B  = 0.
      U1SD = 0.
      V1SD = 0.
      U2SD = 0.
      V2SD = 0.
      U12V = 0.
      V12V = 0.
      U12R = 0.
      V12R = 0.

C  COLLECT STATISTICS BY TIME AND HEIGHT DIFFERENCES
C  -------------------------------------------------
      DO IS=1,NST ! nst max is nstn=300
        DO L=1,NLEV ! nlev max is 35
          DO I=1,NTIMES ! ntimes max is 6
           DO IT=1,NIN(L,I,IS) !  nin(L,I,IS) max is 3
            DO LL=1,NLEV ! nlev max is 35
              DO II=1,NTIMES ! ntimes max is 6
               DO IIT=1,NIN(LL,II,IS) !  nin(LL,II,IS) max is 3
                IF(MAX(UIN(L,I,IT,IS),UIN(LL,II,IIT,IS),VIN(L,I,IT,IS),
     &           VIN(LL,II,IIT,IS)).LT.BMISS .AND.
     &           IQC(L,I,IT,IS).EQ.0 .AND. IQC(LL,II,IIT,IS).EQ.0 .AND.
     &          .NOT.(TIMIN(L,I,IT,IS).EQ.TIMIN(LL,II,IIT,IS) .AND.
     &                                               L.EQ.LL)) THEN
                  IDL = ABS(IZLEV(L) - IZLEV(LL)) + 0.5
                  IDI = ABS(TIMIN(L,I,IT,IS) - TIMIN(LL,II,IIT,IS))+0.5
	          IF(MIN(IDL,IDI).GE.0 .AND. IDL.LE.NL 
     &               .AND. IDI.LE.NTIM) THEN
                    N12(IDL,IDI) = N12(IDL,IDI) + 1
                    U1(IDL,IDI)  = U1(IDL,IDI)  + UIN(L,I,IT,IS)
                    U1S(IDL,IDI) = U1S(IDL,IDI) + UIN(L,I,IT,IS)**2
                    V1(IDL,IDI)  = V1(IDL,IDI)  + VIN(L,I,IT,IS)
                    V1S(IDL,IDI) = V1S(IDL,IDI) + VIN(L,I,IT,IS)**2
                    U2(IDL,IDI)  = U2(IDL,IDI)  + UIN(LL,II,IIT,IS)
                    U2S(IDL,IDI) = U2S(IDL,IDI) + UIN(LL,II,IIT,IS)**2
                    V2(IDL,IDI)  = V2(IDL,IDI)  + VIN(LL,II,IIT,IS)
                    V2S(IDL,IDI) = V2S(IDL,IDI) + VIN(LL,II,IIT,IS)**2
                    U12(IDL,IDI) = U12(IDL,IDI) +
     &                                  UIN(L,I,IT,IS)*UIN(LL,II,IIT,IS)
                    V12(IDL,IDI) = V12(IDL,IDI) +
     &                                  VIN(L,I,IT,IS)*VIN(LL,II,IIT,IS)
                  else
C                   WARNING: Either IDL or IDI out of bounds
C                   Cannot calculate stats!
C                   NTIM=5, so IDI=6 will trigger boundary fault.
C                   NL=34; IDL rarely exceeds NL
                  ENDIF
                ENDIF
               ENDDO
              ENDDO
            ENDDO
           ENDDO
          ENDDO
        ENDDO
      ENDDO

C  CALCULATE MEAN, STANDARD DEVIATION, CORRELATION COEFFICIENT
C  -----------------------------------------------------------

      DO J=0,NTIM
        DO I=0,NL
          CALL CSTATS(N12(I,J),U1(I,J),U1S(I,J),U2(I,J),U2S(I,J),
     &      U12(I,J),U1B(I,J),U2B(I,J),U1SD(I,J),U2SD(I,J),U12V(I,J),
     &      U12R(I,J))
          CALL CSTATS(N12(I,J),V1(I,J),V1S(I,J),V2(I,J),V2S(I,J),
     &      V12(I,J),V1B(I,J),V2B(I,J),V1SD(I,J),V2SD(I,J),V12V(I,J),
     &      V12R(I,J))
        ENDDO
      ENDDO

C  PRINT RESULTS
C  -------------

      WRITE(6,500) (ITIM(I),I=1,NTIM+1)
  500 FORMAT(/' TIMES: ',6I12)
      WRITE(6,501)
      DO I=0,NL
        WRITE(6,502) I,(N12(I,J),J=0,NTIM)
      ENDDO
  501 FORMAT(/' Count:',/,
     &       22X,'TIME DIFFERENCE (hours)'/
     &       '   DZ         0         1         2',
     &       '         3         4         5')
  502 FORMAT(I5,6I10)

      WRITE(6,500) (ITIM(I),I=1,NTIM)
      WRITE(6,503)
      DO I=0,NL
        WRITE(6,507) I,(U1SD(I,J),J=0,NTIM)
      ENDDO
  503 FORMAT(/' Standard Deviation, u-comp for data',
     &       ' at same (dz,dt):',/,
     &       5X,'TIME DIFFERENCE (hours)'/
     &       '   DZ    0    1    2    3    4    5')

      WRITE(6,500) (ITIM(I),I=1,NTIM+1)
      WRITE(6,504)
      DO I=0,NL
        WRITE(6,507) I,(V1SD(I,J),J=0,NTIM)
      ENDDO
  504 FORMAT(/' Standard Deviation, v-comp for data',
     &       ' at same (dz,dt):',/,
     &       5X,'TIME DIFFERENCE (hours)'/
     &       '   DZ    0    1    2    3    4    5')

      WRITE(6,500) (ITIM(I),I=1,NTIM)
      WRITE(6,505)
      DO I=0,NL
        WRITE(6,508) I,(U12R(I,J),J=0,NTIM)
      ENDDO
  505 FORMAT(/' Correlation Coefficient, u-comp for data',
     &       ' at same (dz,dt):',/,
     &       5X,'TIME DIFFERENCE (hours)'/
     &       '   DZ    0    1    2    3    4    5')

      WRITE(6,500) (ITIM(I),I=1,NTIM)
      WRITE(6,506)
      DO I=0,NL
        WRITE(6,508) I,(V12R(I,J),J=0,NTIM)
      ENDDO
  506 FORMAT(/' Correlation Coefficient, v-comp for data',
     &       ' at same (dz,dt):',/,
     &       5X,'TIME DIFFERENCE (hours)'/
     &       '   DZ    0    1    2    3    4    5')

  507 FORMAT(I5,6F5.1)
  508 FORMAT(I5,6F5.2)

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: CSTATS
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1999-08-18
C
C ABSTRACT: CALCULATE MEANS, STANDARD DEVIATIONS, COVARIANCE, AND
C   CORRELATION COEFFICIENT.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  CORRECTED SOME DOCUMENTATION TYPO'S.
C
C USAGE:    CALL CSTATS(N,XS,XSS,YS,YSS,XYS,XB,YB,SX,SY,SXY,RXY)
C   INPUT ARGUMENT LIST:
C     N   - COUNT
C     XS  - SUM(SERIES 1 VALUES)
C     XSS - SUM(SQUARES OF SERIES 1 VALUES)
C     YS  - SUM(SERIES 2 VALUES)
C     YSS - SUM(SQUARES OF SERIES 2 VALUES)
C     XYS - SUM(PRODUCTS OF SERIES 1 AND SERIES 2 VALUES)
C
C   OUTPUT ARGUMENT LIST:
C     XB  - MEAN FOR SERIES 1
C     YB  - MEAN FOR SERIES 2
C     SX  - STANDARD DEVIATION FOR SERIES 1
C     SY  - STANDARD DEVIATION FOR SERIES 2
C     SXY - COVARIANCE OF SERIES 1 AND SERIES 2 VALUES
C     RXY - CORRELATION COEFFICIENT FOR SERIES 1 AND SERIES 2 VALUES
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE CSTATS(N,XS,XSS,YS,YSS,XYS,XB,YB,SX,SY,SXY,RXY)

C  CALCULATE MEANS, STANDARD DEVIATIONS, COVARIANCE, AND
C  CORRELATION COEFFICIENT.
C     N   - COUNT
C     XS  - SUM(SERIES 1 VALUES)
C     XSS - SUM(SQUARES OF SERIES 1 VALUES)
C     YS  - SUM(SERIES 2 VALUES)
C     YSS - SUM(SQUARES OF SERIES 2 VALUES)
C     XYS - SUM(PRODUCTS OF SERIES 1 AND SERIES 2 VALUES)
C     XB  - MEAN FOR SERIES 1
C     YB  - MEAN FOR SERIES 2
C     SX  - STANDARD DEVIATION FOR SERIES 1
C     SY  - STANDARD DEVIATION FOR SERIES 2
C     SXY - COVARIANCE OF SERIES 1 AND SERIES 2 VALUES
C     RXY - CORRELATION COEFFICIENT FOR SERIES 1 AND SERIES 2 VALUES
C  ----------------------------------------------------------------

      IF(N.NE.0) THEN
        ON  = 1./N
        XB  = XS*ON
        YB  = YS*ON
        RAD = (XSS-ON*XS**2)*ON
        IF(RAD.GT.0.) THEN
          SX = SQRT(RAD)
        ELSE
          SX = 0.
        ENDIF
        RAD = (YSS-ON*YS**2)*ON
        IF(RAD.GT.0.) THEN
          SY = SQRT(RAD)
        ELSE
          SY = 0.
        ENDIF
        SXY = (XYS-ON*XS*YS)*ON
	IF(SX*SY.NE.0.) THEN
          RXY = SXY/(SX*SY)
        ELSE
	  RXY = 0.
        ENDIF
      ENDIF
      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: CUMPROB
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1999-08-18
C
C ABSTRACT: FOR A GIVEN X, RETURN THE CUMULATIVE PROBABILITY FOR
C   ABS(VALUE <= X). X IS ASSUMED TO BE NORMAL VARIATE.
C   USE LINEAR INTERPOLATION IN A TABLE.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C
C USAGE:    Y=CUMPROB(X)
C   INPUT ARGUMENT LIST:
C     X        - NORMAL VARIATE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      FUNCTION CUMPROB(X)

C  FOR A GIVEN X, RETURN THE CUMULATIVE PROBABILITY FOR
C  ABS(VALUE <= X). X IS ASSUMED TO BE NORMAL VARIATE.
C  USE LINEAR INTERPOLATION IN A TABLE.

      REAL P(0:40)
      DATA P/.5000,.5398,.5793,.6179,.6554,.6915,
     &       .7257,.7580,.7881,.8159,.8413,
     &       .8643,.8849,.9032,.9192,.9332,
     &       .9452,.9554,.9641,.9713,.9772,
     &       .9821,.9861,.9893,.9918,.9938,
     &       .9953,.9965,.9974,.9981,.9987,
     &       .9990,.9993,.9995,.9997,.9998,
     &       .9998,.9999,.9999,1.0000,1.0000/

      IF(X.GE.4.) THEN
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

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: ANLWT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2000-01-07
C
C ABSTRACT: FOR A GIVEN X>0., RETURN THE ANALYSIS WEIGHT FOR AN
C   OBSERVATION WITH A GIVEN NORMALIZED RESIDUAL OF X.  IN
C   ORDER TO GET X, THE MEAN IS SUBTRACTED AND THEN DIVIDED BY
C   THE STANDARD DEVIATION.  LINEAR INTERPOLATION IS USED FROM THE
C   FOLLOWING TABLE.  IT IS ASSUMED (FROM PAST DATA) THAT PER IS
C   THE PROPORTION OF THE DATA THAT ARE IN ERROR.  ALL ROUGH
C   ERRORS ARE ASSUMED TO BE CONTAINED WITHIN B STANDARD DEVIATIONS.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2000-01-07  W. COLLINS  CHANGE TO AGREE WITH FORMULATION BY
C                         CQCBUFR.
C
C USAGE:    Y=ANLWT(X,B,PER)
C   INPUT ARGUMENT LIST:
C     X        - NORMAL VARIATE
C     B        - LIMIT WRT STANDARD DEVIATIONS FOR ROUGH ERRORS
C     PER      - ASSUMED PROPORTION OF DATA WITH ERRORS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      FUNCTION ANLWT(X,B,PER)

C  FOR A GIVEN X>0., RETURN THE ANALYSIS WEIGHT FOR AN OBSERVATION
C  WITH A GIVEN NORMALIZED RESIDUAL OF X.  IN ORDER TO GET X,
C  THE MEAN IS SUBTRACTED AND THEN DIVIDED BY THE STANDARD
C  DEVIATION.  LINEAR INTERPOLATION IS USED FROM THE FOLLOWING
C  TABLE.  IT IS ASSUMED (FROM PAST DATA) THAT PER IS THE
C  PROPORTION OF THE DATA THAT ARE IN ERROR.  ALL ROUGH ERRORS
C  ARE ASSUMED TO BE CONTAINED WITHIN B STANDARD DEVIATIONS.

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

cwgc  A = .5*PER/B
cwgc  P = (1.-A)*WINT + A*(B+X)
cwgc  S = (1.-A)*WINT

cwgc  IF(P.NE.0.) THEN
cwgc    ANLWT = S/P
cwgc  ELSE
cwgc    ANLWT = 1.
cwgc  ENDIF

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: DISTR
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1991-07-23
C
C ABSTRACT: FOR GIVEN DATA, CALCULATE THE 0TH THROUGH 4TH MOMENTS.
C   ALSO PRODUCE BINNED DISTRIBUTION.
C
C PROGRAM HISTORY LOG:
C 1991-07-23  W. COLLINS
C 2012-03-05  S. MELCHIOR  ADDED FLEXIBILITY BY UTILIZING NDIV AND
C       NX AS ARRAY LIMITS RATHER THAN THE HARD-CODED 23 AND 899,
C       RESPECTIVELY. ADDED MORE DESCRIPTIVE DOCUMENTATION. ALSO 
C       ADDED DIAGNOSTIC OUTPUT STATEMENT WHEN LIMITS ARE EXCEEDED. 
C
C USAGE:    CALL DISTR(X, MSK, XLIM, XMSG, NX, N, NDIV, DDIV, NZERO,
C                      DZERO, NS, X1, SD, SK, XK)
C   INPUT ARGUMENT LIST:
C     X        - DATA
C     MSK      - MASK: VALUE = 0 -- USE IN COMPUTATIONS
C                      VALUE.NE.0 -- DONT USE
C     XLIM     - (1) MINIMUM VALUE TO USE
C                (2) MAXIMUM VALUE TO USE
C     XMSG     - MISSING VALUE FOR VARIABLE
C     NX       - DIMENSION OF X AND MSK
C     NDIV     - NUMBER OF DIVISIONS (DIMENSION OF N)
C     DDIV     - DIVISION SIZE
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
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE DISTR(X,MSK,XLIM,XMSG,NX,N,NDIV,DDIV,NZERO,DZERO,NS,X1,
     &                 SD,SK,XK)

      INTEGER N(NDIV), MSK(NX)
      REAL X(NX),XLIM(2)

      DZERO0 = DZERO

C     COMPUTE VARIOUS POWERS OF X.
C     ----------------------------

      NS   = 0
      SUM1 = 0.
      SUM2 = 0.
      SUM3 = 0.
      SUM4 = 0.
      DO I=1,NX
        IF(MSK(I).EQ.0.AND.ABS(X(I)).LT.XMSG
     &    .AND.X(I).GE.XLIM(1).AND.X(I).LE.XLIM(2)) THEN
          NS = NS + 1
          SUM1 = SUM1 + X(I)
          SUM2 = SUM2 + X(I)**2
          SUM3 = SUM3 + X(I)**3
          SUM4 = SUM4 + X(I)**4
        ENDIF
      END DO
      IF(NS.LT.1) THEN
        DZERO0 = 0.
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

C     CALCULATE VARIOUS OUTPUT STATISTICS.
C     ------------------------------------

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

C     COUNT THE NUMBER OF OBSERVATIONS IN EACH DIVISION.
C     --------------------------------------------------

   15 CONTINUE

      IF(DDIV.EQ.0.) DDIV = 1.
      IF(NZERO.EQ.1) DZERO0 = SUM1
      N(1:NDIV) = 0
      CON = 0.5*NDIV + 1.0 - DZERO0/DDIV
      DO I=1,NX
        IF(MSK(I).EQ.0.AND.ABS(X(I)).LT.XMSG
     &          ) THEN
cwgc &    .AND.X(I).GE.XLIM(1).AND.X(I).LE.XLIM(2)) THEN
          INDEX = X(I)/DDIV + CON
          IF(INDEX.LT.1) INDEX = 1
          if(index.gt.ndiv) then
C           print *, 'WARNING: INDEX > NDIV, SET INDEX = NDIV'
            index = ndiv
          endif
          N(INDEX) = N(INDEX) + 1
        ENDIF
      END DO
      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: DRCTSL
C   PRGMMR: WOOLLEN          ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT: DRIVER FOR CHOLESKY TYPE LINEAR EQUATION SOLVER.
C
C PROGRAM HISTORY LOG:
C 1990-11-06  J. WOOLLEN
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C
C USAGE:
C   INPUT ARGUMENTS:
C     FAALL      - ARRAY OF SYMMETRIC MATRIXES
C     RAALL      - ARRAY OF MATRIX RIGHT HAND SIDES
C     NDIM       - ARRAY OF MATRIX RANKS
C     MAXDIM     - MAXIMUM RANK MATRIX IN MATRIX ARRAY
C     NXXYY      - NUMBER OF MATRIXES IN MATRIX ARRAY
C     NFT        - NUMBER OF RIGHT HAND SIDE VECTORS PER MATRIX
C   OUTPUT ARGUMENTS:
C     RAALL      - ARRAY OF MATRIX SOLUTIONS
C     DOTPRD     - ARRAY OF DOT PRODUCTS OF RHS VECTORS WITH MATRIX
C                  SOLUTIONS
C
C REMARKS: ILL CONDITIONED OR NON POSITIVE DEFINITE MATRIXES ARE
C          IDENTIFIED BY DOT PRODUCTS GT 1 OR LT 0 OR BY A MESSAGE
C          FROM VSOLVE. FIVE ATTEMPTS ARE MADE TO SOLVE BAD ONES,
C          BY RIDGE REGRESSION, AFTER WHICH A NULL SOLUTION IS RETURNED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE DRCTSL(FAALL,RAALL,DOTPRD,NDIM,MAXDIM,NXXYY,NFT)

      PARAMETER(NSTN=300)
      DIMENSION  FAALL(NSTN,45), DOTPRD(NSTN,1), RAALL(NSTN,9,1),
     &           NDIM(NSTN)
      DIMENSION  A(NSTN,45),B(NSTN,9,1),BAD(NSTN),SMOOTH(6)
      LOGICAL BAD
      DATA SMOOTH /1.00,1.01,1.02,1.05,1.10,2.00/

C----------------------------------------------------------------------

C  LOOP FOR POSSIBILITY OF BAD MATRICES
C  ------------------------------------

      DO ITRY=1,6

         NBAD = 0

C  INITIALIZE THE WORKING ARRAYS
C  -----------------------------

         A(1:NXXYY,1:MAXDIM*(MAXDIM+1)/2) =
     &                              FAALL(1:NXXYY,1:MAXDIM*(MAXDIM+1)/2)

         B(1:NXXYY,1:MAXDIM,1:NFT) = RAALL(1:NXXYY,1:MAXDIM,1:NFT)

         DOTPRD(1:NXXYY,1:NFT) = 0.

         DO J=1,MAXDIM
            JJ = J*(J+1)/2
            A(1:NXXYY,JJ) = FAALL(1:NXXYY,JJ)*SMOOTH(ITRY)
         END DO

C  CALL THE MATRIX SOLVER
C  ----------------------

         CALL VSOLVE (A,B,NDIM,BAD,NFT,NXXYY,MAXDIM)

C  MAKE THE DOT PRODUCTS OF SOLUTIONS WITH RIGHT HAND SIDES
C  --------------------------------------------------------

         DO K=1,NFT
            DO J=1,MAXDIM
               DO I=1,NXXYY
                  DOTPRD(I,K) = DOTPRD(I,K) + RAALL(I,J,K)*B(I,J,K)
               END DO
            END DO
         END DO

         DO K=1,NFT
            DO I=1,NXXYY
               IF(DOTPRD(I,K).GT.1.)THEN
                  DO J=1,MAXDIM
                     B(I,J,K) = B(I,J,K)/DOTPRD(I,K)
                  END DO
                  DOTPRD(I,K) = 1.
               ENDIF
            END DO
         END DO

C  CHECK FOR BAD ONES - DO IT AGAIN IF NECESSARY
C  ---------------------------------------------

         DO J=1,NFT
            DO I=1,NXXYY
               BAD(I) = BAD(I).OR.DOTPRD(I,J).LT.0..OR.DOTPRD(I,J).GT.1.
            END DO
         END DO

         DO I=1,NXXYY
            IF(BAD(I)) THEN
               DOTPRD(I,1:NFT) = 0.
               B(I,1:MAXDIM,1:NFT) = 0.
               NBAD = NBAD + 1
            ENDIF
         END DO

         IF(NBAD.NE.0) THEN
            PRINT*, 'NBAD=',NBAD,' ON TRY ',ITRY
         ELSE
            GOTO 55
         ENDIF

      END DO

C  COPY SOLUTIONS INTO OUTPUT ARRAY - ZERO BAD ONES OUT
C  ----------------------------------------------------

55    CONTINUE
      
      RAALL(1:NXXYY,1:MAXDIM,1:NFT) = B(1:NXXYY,1:MAXDIM,1:NFT)

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: EVNOUT
C   PRGMMR: S. Melchior      ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT: THE PRESENT REPORT IS IN /SINGLE/ AND STNID WITH INDICES
C   FROM NUM1 TO NUM2.  FOLLOWING CODE WILL LOOK AT THE EVENTS
C   STORED IN /EVENTS/ FOR MATCHES.  ARRAYS OF MATCHES WILL
C   BE FORMED FOR WRITING THE ACTUAL EVENTS TO THE OUTPUT
C   BUFR FILE.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2001-10-08  D. KEYSER   NOW TABULATES PERCENT OF LEVELS < 18K
C       FAILING BIRD ALGORITM CHECK ACCORDING TO STATION AND
C       1-HOUR TIME WINDOWS CENTERED ON 30-MIN PAST HOUR, THIS
C       BIRD Q.C. INFORMATION WRITTEN TO UNIT 52 FOR LATER USE
C       BY SSI IN Q.C. OF NEXRAD RADIAL VELOCITY DATA
C 2012-03-05  S. MELCHIOR  UPDATED TO USE INCOMING ARGUMENT NLV TO
C       DEFINE MEMORY ALLOCATION FOR VARIOUS REAL AND INTEGER ARRAYS.
C       THIS MAKES THE SUBROUTINE MORE ROBUST AND EFFICIENT BY ONLY
C       ALLOTTING THE MEMORY SPACE NECESSARY, NOTHING MORE.  ADDED
C       PARAMETER NEVNT=160000 TO TIDY UP THE CODE. CORRECTED  SOME
C       DOCUMENTATION TYPO'S. ADDED DIAGNOSTIC OUTPUT STATEMENT WHEN
C       LIMITS ARE EXCEEDED. RENAMED NE to NME TO EASE NAVIGATION AND
C       FOR CLARIFICATION SINCE NE IS USED FOR INEQUALITY TESTING
C       (.NE.). INCREASED NRPT FROM 80000 TO 160000 TO ACCOMMODATE VAD
C       WIND REPORTS FROM LEVEL 2 DECODER.
C 2012-03-05  D. KEYSER   UPDATED PROCESSING OF RADAR STATION IDS TO
C       ACCOMMODATE VAD WIND REPORTS FROM LEVEL 2 DECODER.  THESE
C       REPORTS COME IN WITH THE STATION ID FORMATTED WITH ONLY 4
C       CHARACTERS WHEREAS LEGACY (RADAR CODED MESSAGE) VAD WIND
C       REPORTS HAVE STATION IDS WITH 7 CHARACTERS.
C 2014-01-15  S. Melchior  Increased NRPT (total number of levels
C       amongst all VAD reports that can be processed) and NEVNT (total
C       number of events amongst all VAD reports that can be processed)
C       both from 160000 to 500000 to accommodate VAD wind reports from
C       Level 2 decoder.
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C 2022-07-21  M. Sienkiewicz  Increased NRPT and NEVNT from 500000 to
C       800000 to accommodate recent increase in numbers of reports 
C
C USAGE:    CALL EVNOUT(NUM1,NUM2,NLV)
C   INPUT ARGUMENT LIST:
C     NUM1     - LOWER LIMIT OF EVENTS TO SCAN
C     NUM2     - UPPER LIMIT OF EVENTS TO SCAN
C     NLV      - NUMBER OF LEVELS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE EVNOUT(NUM1,NUM2,NLV)

      PARAMETER (NRPT=800000,NSTN=300,NLEV=35)
      parameter (nevnt=800000)
      REAL(8)   BMISS

      COMMON /SINGLE/ ZOB(NRPT), ITM(NRPT),TIM(NRPT),
     &                UOB(NRPT), VOB(NRPT), LOB(NRPT),
     &                XLA(NRPT), XLO(NRPT), ZST(NRPT),
     &                UFF(NRPT), VFF(NRPT), TFF(NRPT),
     &                WQM(NRPT)
      COMMON /STN/    SLAT(NSTN), SLON(NSTN), SIDS(NSTN), STNID(NRPT),
     &                ZSTN(NSTN)
      COMMON /EVENTS/ SIDEV(nevnt), HTEV(nevnt),LEV(nevnt),
     &                TIMEV(nevnt), UEV(nevnt), VEV(nevnt),
     &                QALEV(nevnt), RCEV(nevnt)
      COMMON /HTS/    IZLEV(NLEV), ZLEV(NLEV)
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /BUFRLIB_MISSING/BMISS
      COMMON /FLAG/   guess
      COMMON /BIRDST/stnidx(9999,-3:3),alatx(9999,-3:3),
     & alonx(9999,-3:3),knt_5_yes(9999,-3:3),knt_5_no(9999,-3:3),
     & numrpt(9999,-3:3)
      common /switches/  print_52, print_53, print_60, test
      LOGICAL         guess, PRINT_52, PRINT_53
      REAL            UE(nlv*2), VE(nlv*2), QALE(nlv*2), RCE(nlv*2),
     &                ETYP(nlv*2)

      INTEGER         INP(nlv*2)
      CHARACTER*8     SIDS, STNID, SIDEV
      character*8     stnidx
      CHARACTER*40    WEVN
      DATA WEVN   /'UOB VOB WQM WPC WRC                     '/

      IF(NUM2.LT.NUM1 .OR. NME.EQ.0) RETURN

      INP  = 0
      UE   = BMISS
      VE   = BMISS
      QALE = BMISS
      RCE  = BMISS

C  THE PRESENT REPORT IS IN /SINGLE/ AND STNID WITH INDICES
C  FROM NUM1 TO NUM2.  FOLLOWING CODE WILL LOOK AT THE EVENTS
C  STORED IN /EVENTS/ FOR MATCHES.  ARRAYS OF MATCHES WILL
C  BE FORMED FOR WRITING THE ACTUAL EVENTS TO THE OUPUT
C  BUFR FILE.

C  LOOP THRU EVENTS
C  ----------------

      ETYP = 0
      NEVN = 0
      loop1: do n=num1,num2
       DO IEV=1,NME !  nme max is nev and nevnt
         IF(SIDEV(IEV).EQ.STNID(N) .AND.
     &      ABS(HTEV(IEV)-ZOB(N)).LT.10. .AND.
     &      TIMEV(IEV).EQ.TIM(N)) THEN
           NEVN = NEVN + 1
  500      FORMAT(' N,IEV,NEVN,NLV,NUM1,NUM2,LEV =',7I5,' FOR ',A8)
           if(nevn.gt.nlv*2) then
             print *, 'WARNING: NEVN > NLV, EXIT LOOP'
             WRITE(6,500) N,IEV,NEVN,NLV,NUM1,NUM2,LEV(IEV),STNID(N)
             nevn = nevn - 1
             exit LOOP1
           endif
           INP(NEVN) = LEV(IEV)
           UE(NEVN) = UEV(IEV)
           VE(NEVN) = VEV(IEV)
           QALE(NEVN) = QALEV(IEV)
           RCE(NEVN)  = RCEV(IEV)
           ETYP(LOB(N)) = RCEV(IEV)
         ENDIF
       ENDDO
      enddo loop1

      IF(NEVN.GT.0) CALL EVENTW(IOUT,WEVN,NLV,UE,VE,QALE,RCE,INP,NEVN,
     &                          CQCPC)

      if(print_52)  then

C  KEEP TRACK OF LEVELS FAILING BIRD ALG. FOR LATER Q.C. INFO LISTING
C  ------------------------------------------------------------------

         read(stnid(num1),'(i4)',err=99)  kndx
         go to 98  ! these are ids from Radar Coded Message
   99    continue  ! these are ids from Level 2 decoder
         kndx = 0
         do iii = 1,nst
            if(stnid(num1).eq.sids(iii)) then
               kndx = iii
               exit
            endif
         enddo
   98    continue
         if(kndx.ge.1.and.kndx.le.9999)  then
            jtim = nint(tim(num1)+0.50)
            if(jtim.ge.-3.and.jtim.le.3)  then
               stnidx(kndx,jtim) = stnid(num1)
               alatx(kndx,jtim) = xla(num1)
               alonx(kndx,jtim) = xlo(num1)
               numrpt(kndx,jtim) = numrpt(kndx,jtim) + 1
            end if
         end if
      end if

      IF(PRINT_53) THEN
         WRITE(53,501) ITM(NUM1),TIM(NUM1),STNID(NUM1),XLA(NUM1),
     &     XLO(NUM1),ZST(NUM1)
         WRITE(53,502)
      END IF
      if(print_52 .or. print_53) then
         DO N=NUM1,NUM2
            IF(PRINT_53)WRITE(53,503)LOB(N),ZOB(N),UOB(N),VOB(N),UFF(N),
     &       VFF(N),TFF(N),ETYP(LOB(N))
            if(print_52) then
               if(kndx.ge.1.and.kndx.le.9999) then
                  if(jtim.ge.-3.and.jtim.le.3)  then
                     if(zob(n).lt.5487.) then ! less than 18,000 feet 
                        if(nint(etyp(lob(n))).eq.5) then
                           knt_5_yes(kndx,jtim) = knt_5_yes(kndx,jtim)+1
                        else
                           knt_5_no(kndx,jtim) = knt_5_no(kndx,jtim) + 1
                        end if
                     end if
                  end if
               end if
            end if
         ENDDO
      end if
  501 FORMAT(/,1X,I10,1X,F8.3,1X,A8,1X,F7.2,1X,F8.2,1X,F8.0)
  502 FORMAT(' LOB    ZOB    UOB    VOB    UFF    VFF    TFF',
     &       ' TYP')
  503 FORMAT(2X,I2,F7.0,5(F7.1),F4.0)

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: EVENTW
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-05-23
C
C ABSTRACT: COMPUTE EXPANSION FACTOR FOR CORECT.
C
C PROGRAM HISTORY LOG:
C 1994-MM-DD  J. WOOLLEN
C 1997-05-23  W. COLLINS  MODIFIED FOR WIND EVENT.
C 2012-03-05  S. MELCHIOR  CLARIFIED DOCUMENTATION.
C
C USAGE:   CALL EVENTW(LUNIT,EVNSTR,NLV,UOBS,VOBS,QMS,RCS,IND,NEVN,QCPC)
C   INPUT ARGUMENT LIST:
C     LUNIT    - UNIT NUMBER
C     EVNSTR   - EVENT STREAM OF CHARACTERS
C     NLV      - NUMBER OF LEVELS
C     UOBS     - OBSERVED U VALUES
C     VOBS     - OBSERVED V VALUES
C     QMS      - QUALITY MARKER
C     RCS      - REASON CODE
C     IND      - INDIRECT ADDRESS OF VALUE
C     NEVN     - NUMBER OF EVENTS
C     QCPC     - PROGRAM CODE
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE EVENTW(LUNIT,EVNSTR,NLV,UOBS,VOBS,QMS,RCS,IND,NEVN,
     &                  QCPC)
      LOGICAL  PRINT_60
      CHARACTER*(*) EVNSTR
      DIMENSION     UOBS(NEVN),VOBS(NEVN),QMS(NEVN),RCS(NEVN),IND(NEVN)
      REAL(8)       EVNS_8(5,255),BMISS

      COMMON /BUFRLIB_MISSING/BMISS
      common /switches/  print_52, print_53, print_60, test

C-----------------------------------------------------------------------

      IF(NEVN.EQ.0) GOTO 100


C  CLEAR THE UFB ARRAY FIRST
C  -------------------------

      EVNS_8 = BMISS

C  TRANSFER EVENT ARRAYS INTO UFB ARRAY
C  ------------------------------------

      DO I=1,NEVN
        J = IND(I)
        IF(UOBS(I).LT.BMISS .AND. J.LE.255) THEN
           EVNS_8(1,J) = UOBS(I)
           EVNS_8(2,J) = VOBS(I)
           EVNS_8(3,J) = QMS(I)
           EVNS_8(4,J) = QCPC
           EVNS_8(5,J) = RCS(I)
        ENDIF
      ENDDO

C  WRITE THE EVENTS
C  ----------------

      CALL UFBINT(LUNIT,EVNS_8,5,NLV,IRET,EVNSTR)

  100 CONTINUE

      IF(PRINT_60) WRITE(60,501) LUNIT,NEVN,NLV,IRET,EVNSTR,
     & QCPC
  501 FORMAT(' EVENTW--LUNIT,NEVN,NLV,IRET,EVNSTR:',4I5,A60,
     &  ' QCPC:',F8.0)

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: GETDAT
C   PRGMMR: S. Melchior      ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT: READ PREPBUFR DATA
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2001-10-08  D. KEYSER   VALID DATES FOR CHECKING FOR MIGRATING
C       BIRDS EXPANDED FROM 2/15 THROUGH 6/15 TO 2/1 THROUGH 6/30
C       IN SPRING AND FROM 8/15 THROUGH 11/15 TO 8/1 THROUGH 11/30
C       IN FALL
C 2012-03-05  S. MELCHIOR  ADDED PARAMETERS NTIMES=6 AND NINC=3 TO
C       TIDY UP THE CODE. ADDED DIAGNOSTIC OUTPUT PRINT STATEMENTS
C       WHEN LIMITS ARE EXCEEDED. RENAMED NE TO NME TO EASE NAVIGATION
C       AND FOR CLARIFICATION SINCE NE IS USED FOR INEQUALITY TESTING
C       (.NE.).  ADDED LOGIC TO TURN OFF BIRD CHECK ALGORITHM FOR VAD
C       WIND REPORTS FROM LEVEL 2 DECODER.  CORRECTED LOGIC TO HANDLE
C       SITUATIONS WHEN THE SUBSET IS VADWND BUT THE NUMBER OF LEVELS
C       (NUM) EXCEEDS NRPT.  INCREASED NRPT FROM 80000 TO 160000 TO
C       ACCOMMODATE VAD WIND REPORTS FROM LEVEL 2 DECODER.
C 2014-01-15  S. Melchior  Increased NRPT (total number of levels
C       amongst all VAD reports that can be processed) from 160000 to
C       500000 to accommodate VAD wind reports from Level 2 decoder.
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.  Also made a minor logic correction
C       so that the station info for the "NSTN'th" id is stored if
C       there are at least that many unique ids.
C 2022-07-21  M. Sienkiewicz  Increased NRPT and NEVNT from 500000 to
C       800000 to accommodate recent increase in numbers of reports 
C
C USAGE:    CALL GETDAT(ITIME)
C   INPUT ARGUMENT LIST:
C     ITIME    - INDICTES FIRST OR SECOND READ OF DATA
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE GETDAT(ITIME)

      PARAMETER (NRPT=800000,NSTN=300,NLEV=35,NTIMES=6,NINC=3)
      PARAMETER (MLV=255)         ! no. of possible levels
      INTEGER IDAT(8), JDAT(8), ITIMES(8,4)
      REAL    TDIF(5,4), RINC(5)
      REAL(8) HDR_8(10), OBS_8(7,MLV), BMISS
      CHARACTER*2 AYR, AHR
      CHARACTER*3 ADOY
      CHARACTER*8 CDR(10),SUBSET,YYMM,DDHH,Q,SID,STNID,SIDS
      CHARACTER*10 CDATE
      EQUIVALENCE (HDR_8,CDR)
      COMMON /SINGLE/ ZOB(NRPT), ITM(NRPT),TIM(NRPT),
     &                UOB(NRPT), VOB(NRPT), LOB(NRPT),
     &                XLA(NRPT), XLO(NRPT), ZST(NRPT),
     &                UFF(NRPT), VFF(NRPT), TFF(NRPT),
     &                WQM(NRPT)
      COMMON /INCS/   UIN(NLEV,NTIMES,NINC,NSTN),
     &                VIN(NLEV,NTIMES,NINC,NSTN),
     &                UUU(NLEV,NTIMES,NINC,NSTN),
     &                VVV(NLEV,NTIMES,NINC,NSTN),
     &                TIMIN(NLEV,NTIMES,NINC,NSTN),
     &                NIN(NLEV,NTIMES,NSTN),
     &                LLL(NLEV,NTIMES,NINC,NSTN),
     &                TFC(NLEV,NTIMES,NINC,NSTN),
     &                QQQ(NLEV,NTIMES,NINC,NSTN)
      COMMON /STN/    SLAT(NSTN), SLON(NSTN), SIDS(NSTN), STNID(NRPT),
     &                ZSTN(NSTN)
      COMMON /DATET/  IDATE(4), ITIM(6)
      COMMON /HTS/ IZLEV(NLEV), ZLEV(NLEV)
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT,
     &                CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /BUFRLIB_MISSING/BMISS
      COMMON /BIRDS/ BIRDTIME
      DATA ITIMES/0, 2,01,0,0,0,0,0,
     &            0, 6,30,0,0,0,0,0,
     &            0, 8,01,0,0,0,0,0,
     &            0,11,30,0,0,0,0,0/

C  READ CENTRAL TIME (year,month,day,hour)
C  ---------------------------------------

      IF(ITIME.EQ.1) THEN
        CALL DATELEN(10)
        REWIND IUTIM
        READ(IUTIM,500) (IDATE(I),I=1,4)
        WRITE(6,501) (IDATE(I),I=1,4)
  500   FORMAT(I4,3I2)
  501   FORMAT(' GETDAT--IDATE (central data time): ',I4,3I2.2)

C------------------------------------------------
C  NCEP absolute date and time data structure:
C    (1)     4-digit year
C    (2)     month of year (1-12)
C    (3)     day of month (1-31)
C    (4)     hhmm time zone differential from UTC
C    (5)     hour of day in 24-hour clock (0-23)
C    (6)     minutes of hour (0-59)
C    (7)     seconds of minute (0-59)
C    (8)     milliseconds of second (0-999)
C------------------------------------------------
C  NCEP relative time interval data structure
C    (1)     number of days
C    (2)     number of hours
C    (3)     number of minutes
C    (4)     number of seconds
C    (5)     number of milliseconds
C------------------------------------------------
C  Note! IDAT and JDAT are in absolute time, while
C  RINC is in relative time.
C  IDAT and JDAT are INTEGER;  RINC is REAL.
C------------------------------------------------

        IDAT      = 0
        IDAT(1:3) = IDATE(1:3)
        IDAT(5)   = IDATE(4)
        DO I=1,6
          MIT = I-1+IHE
          RINC(1) = 0
          RINC(2) = MIT
          RINC(3) = 0
          RINC(4) = 0
          RINC(5) = 0
          CALL W3MOVDAT(RINC,IDAT,JDAT)
          ITIM(I) = JDAT(5)+100*(JDAT(3)+100*(JDAT(2)+100*JDAT(1)))
        ENDDO

        WRITE(6,505) (ITIM(I),I=1,6)
  505   FORMAT(' GETDAT--ITIM(I): ',6I12)

C  COMPARE TIMES FOR "ANTI-BIRD" ROUTINE
C  -------------------------------------

        ITIMES(1,1:4) = IDATE(1)
        DO J=1,4
          CALL W3DIFDAT(IDAT,ITIMES(1,J),1,TDIF(1,J))
          WRITE(6,511) (IDAT(II),II=1,8),(ITIMES(II,J),II=1,8),
     &                  TDIF(1,J)
  511     FORMAT(' GETDAT--IDAT,ITIMES,TDIF: ',8I5,4X,8I5,4X,F8.2)
        ENDDO
        IF(TDIF(1,1).GT.0. .AND. MAX(TDIF(1,2),TDIF(1,3),TDIF(1,4))
     &   .LT.0.) THEN
          BIRDTIME = 1.           ! between 01 Feb and 30 Jun
        ELSE IF(MIN(TDIF(1,1),TDIF(1,2),TDIF(1,3)).GT.0. .AND.
     $   TDIF(1,4).LT.0.) THEN
          BIRDTIME = 2.           ! between 01 Aug and 30 Nov
        ELSE
          BIRDTIME = 0.
        ENDIF

        WRITE(6,506) BIRDTIME
  506   FORMAT(' GETDAT--BIRDTIME: ',F5.0,/,
     &    '     1. - between 01 Feb and 30 Jun',/,
     &    '     2. - between 01 Aug and 30 Nov',/,
     &    '     0. - otherwise')

      ENDIF

C  INITIALIZE DATA COLLECITON ARRAYS
C  ---------------------------------

      NUM   = 0
      OBS_8   = BMISS
      ZOB   = BMISS
      UOB   = BMISS
      VOB   = BMISS
      UIN   = BMISS
      VIN   = BMISS
      XLA   = BMISS
      XLO   = BMISS
      ITM   = 0
      TIM   = BMISS
      STNID = '        '

      CALL CLOSBF(IUNIT)
      REWIND IUNIT
      CALL OPENBF(IUNIT,'IN',IUNIT)
      CALL READMG(IUNIT,SUBSET,KDATE,IRETMG)
      IF(IRETMG.NE.0) GOTO 6
      WRITE(CDATE,'(I10)') KDATE
      PRINT*,'DATA VALID AT ',CDATE
      CALL UFBQCD(IUNIT,'CQCVAD ',CQCPC)
      WRITE(6,502) CQCPC
  502 FORMAT(' PROGRAM CODE NUMBER =',F5.0)
      CALL CLOSBF(IUNIT)
      REWIND IUNIT
      CALL OPENBF(IUNIT,'IN',IUNIT)
      IF(ITIME.EQ.2) CALL OPENBF(IOUT,'OUT',IUNIT)
      IS = 0
      IRETSB = 1

C  READ A REPORT
C  -------------

    5 CONTINUE
      IF(IRETSB.NE.0) CALL READMG(IUNIT,SUBSET,KDATE,IRETMG)
    6 CONTINUE
      IF(IRETMG.NE.0) THEN
        WRITE(6,510) IUNIT, IOUT
  510   FORMAT(' GETDAT--CLOSING UNITS:',2I4)
        CALL CLOSBF(IUNIT)
        CALL CLOSBF(IOUT)
        GOTO 20
      ENDIF
      IF(ITIME.EQ.2) THEN
        IF(SUBSET.NE.'VADWND') THEN
          CALL CLOSMG(IOUT)
          CALL COPYMG(IUNIT,IOUT)
          GOTO 5
        ELSE
          if(num.ge.nrpt) then
            iretsb = -1
            go to 5
          endif
          CALL OPENMB(IOUT,SUBSET,KDATE)
        ENDIF
      ELSE IF(SUBSET.NE.'VADWND') THEN
        GOTO 5
      else if(num.ge.nrpt) then
        iretsb = -1
        go to 5
      ENDIF
   30 CONTINUE
      CALL READSB(IUNIT,IRETSB)
      IF(IRETSB.NE.0) GOTO 5
 
      CALL GETOBV(IUNIT,HDR_8,NLV,OBS_8)

C  COLLECT STATION DATA
C  --------------------

      NUM1 = NUM + 1
      RINC = 0
      IDAT = 0
      DO I=1,NLV
        IF(OBS_8(1,I).GE.CON.AND.MAX(OBS_8(2,I),OBS_8(3,I)).LT.BMISS)
     $   THEN
          NUM        = NUM + 1
          if(num.ge.nrpt) then
            print *,'WARNING: NUM>=NRPT, can''t process anymore VAD obs'
 !           call system('[ -n "$jlogfile" ] && $DATA/postmsg'//
 !    $       ' "$jlogfile" "***WARNING: NUM >= NRPT, cannot process '//
 !    $       'any more VAD reports"')
            iretsb = -1
            go to 5
          endif
          STNID(NUM) = CDR(1)
          XLO(NUM)   = HDR_8(2)
          XLA(NUM)   = HDR_8(3)
          ZST(NUM)   = HDR_8(5)
          IDAT(1) = ITIM(1)/1000000
          IDAT(2) = MOD(ITIM(1)/10000,100)
          IDAT(3) = MOD(ITIM(1)/100,100)
          IDAT(5) = MOD(ITIM(1),100)
          RINC(2) = HDR_8(4) - IHE
          CALL W3MOVDAT(RINC,IDAT,JDAT)
          ITM(NUM) = JDAT(5)+100*(JDAT(3)+100*(JDAT(2)+100*JDAT(1)))
          TIM(NUM)   = HDR_8(4)
          LOB(NUM)   = I
          ZOB(NUM)   = OBS_8(1,I)
          UOB(NUM)   = OBS_8(2,I)
          VOB(NUM)   = OBS_8(3,I)
          UFF(NUM)   = OBS_8(4,I)
          VFF(NUM)   = OBS_8(5,I)
          TFF(NUM)   = OBS_8(6,I)
          WQM(NUM)   = OBS_8(7,I)
        ENDIF

c     Turn off bird algorithm QC for VAD wind reports from Level 2
c      decoder
      read(stnid(num),'(i4)',err=89) kdmy
      go to 90 ! radar coded vadwnd data
   89 continue ! VAD wind reports from Level 2 decoder
      birdtime = 0. ! override regardless of season
   90 continue

      ENDDO
      NUM2 = NUM
      if(num.ge.nrpt) then
        print *,'WARNING: NUM>=NRPT, can''t process anymore VAD obs'
!        call system('[ -n "$jlogfile" ] && $DATA/postmsg'//
!     $  ' "$jlogfile" "***WARNING: NUM >= NRPT, cannot process '//
!     $  'any more VAD reports"')
!        iretsb = -1
        go to 5
      endif

      IF(ITIME.EQ.2) THEN
        CALL UFBCPY(IUNIT,IOUT)
        CALL EVNOUT(NUM1,NUM2,NLV)
        CALL WRITSB(IOUT)
      ENDIF

      GOTO 30

   20 CONTINUE

      WRITE(6,508) NUM, ITIME
  508 FORMAT(' Number of levels reported: ',I6,'  for ITIME: ',I1)

      IF(ITIME.EQ.2) RETURN

C  ALL DATA IS NOW READ IN.  DO PROCESSING ON LINEAR ARRAYS OF DATA.
C  -----------------------------------------------------------------

C  MAKE A LIST OF THE STATIONS REPORTING
C  -------------------------------------

      NST = 0
      sids(1) = '        ' ! initialize sids(1)
      LOOP1: DO I=1,NUM
        LOOP1n1: DO IS=1,NSTN
          IF(SIDS(IS).EQ.STNID(I)) CYCLE LOOP1
        ENDDO LOOP1n1
        if(nst.ge.nstn) then
          print *, 'WARNING: NST>=NSTN, EXIT LOOP'
          exit LOOP1
        endif
        NST = NST + 1
        SIDS(NST) = STNID(I)
        SLAT(NST) = XLA(I)
        SLON(NST) = XLO(I)
        ZSTN(NST) = ZST(I)
      ENDDO LOOP1

      WRITE(6,507) NST
  507 FORMAT(' Number of stations reporting: ',I5,/,
     &       '    Station     Lat     Lon  Stn Ht')
      WRITE(6,509) (SIDS(I),SLAT(I),SLON(I),ZSTN(I),I=1,NST)
  509 FORMAT(1X,A10,2F8.2,F8.0)

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: GETOBV
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1997-03-18
C
C ABSTRACT: Read data for a single observation. Place the data in
C   OBS_8.
C
C PROGRAM HISTORY LOG:
C   97-03-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  ADDED MORE DESCRIPTIVE DOCUMENTATION AND
C       CORRECTED DOCUMENTATION TYPO.
C
C USAGE:    CALL GETOBV(LUBFR,HDR_8,NLEV,OBS_8)
C   INPUT ARGUMENT LIST:
C     LUBFR    - UNIT NO. OF INPUT BUFR FILE
C
C   OUTPUT ARGUMENT LIST:
C     HDR_8    - ARRAY OF OB HEADER INFORMATION
C     NLEV     - NUMBER OF LEVELS
C     OBS_8    - ARRAY OF OBSERVATIONS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      SUBROUTINE GETOBV(LUBFR,HDR_8,NLEV,OBS_8)

C     THIS SUBROUTINE RETURNS ALL EVENTS FOR AN OBSERVATION

      PARAMETER (MVR=10)       ! dimensions of header
      PARAMETER (MLV=255)      ! number of possible levels

C     Last index of OBS_8: value, fcst value, quality mark, program
c     code, reason code

      CHARACTER*28 OBSTR
      CHARACTER*40 HDSTR
      REAL(8) HDR_8(10), OBS_8(7,MLV), BMISS

      COMMON /BUFRLIB_MISSING/BMISS

      DATA HDSTR /'SID XOB YOB DHR ELV ITP TYP             '/
      DATA OBSTR /'ZOB UOB VOB UFC VFC TFC WQM '/

      HDR_8 = BMISS
      OBS_8 = BMISS

C  READ HEADER
C  -----------

      CALL UFBINT(LUBFR,HDR_8,MVR,1,IRET,HDSTR)

C  READ EVENTS
C  -----------

      CALL UFBINT(LUBFR,OBS_8,7,MLV,NLEV,OBSTR)

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: HT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1999-08-18
C
C ABSTRACT: COMPUTE VERTICAL INDEX FOR HEIGHT
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  ADDED MISSING RETURN STATEMENT.
C       RENAMED NE TO NME FOR EASE OF NAVIGATION AND FOR
C       CLARIFICATION SINCE NE IS USED FOR INEQUALITY 
C       TESTING (.NE.).
C
C USAGE:    CALL HT(Z,IHT,IER)
C   INPUT ARGUMENT LIST:
C     Z        - HEIGHT (M)
C
C   OUTPUT ARGUMENT LIST:
C     IHT      - VERTICAL INDEX FOR HEIGHT
C     IER      = 0  ALL OK
C              = 1  BAD HEIGHT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE HT(Z,IHT,IER)

      PARAMETER (NLEV=35)
      COMMON /HTS/ IZLEV(NLEV), ZLEV(NLEV)
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME

      IHT = 0
      IER = 0
      IZ = Z/CON + 0.49
      DO L=1,NLEV
        LL = L
        IF(IZ.EQ.IZLEV(L)) THEN
           IHT = LL
           RETURN
        END IF
      ENDDO

      IER = 1
      return

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: INCDIST
C   PRGMMR: S. Melchior      ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT: COMPUTE STATISTICS AND DISTRIBUTIONS FOR INCREMENTS
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  EXTENDED MSK ARRAY ALLOCATION FROM 899 TO
C       1000 TO ALLOW FOR MORE STATION STATISTICS TO BE PROCESSED - ON
C       OCCASION THE 899 LIMIT HAD BEEN MET AND EXCEEDED. ADDED
C       PARAMETERS NTIMES=6, NINC=3, AND NDIV=23  TO TIDY UP THE CODE. 
C       ADDED DIAGNOSTIC OUTPUT STATEMENTS WHEN LIMITS ARE EXCEEDED.
C       RENAMED NE TO NME TO EASE NAVIGATION AND FOR CLARIFICATION AS
C       NE IS USED FOR INEQUALITY TESTING (.NE.). INCREASED NRPT FROM
C       80000 TO 160000 TO ACCOMMODATE VAD WIND REPORTS FROM LEVEL 2
C       DECODER.
C 2014-01-15  S. Melchior Added integer "icntmx" to define the max
C       value of icnt in the multi-nested do loop that hinges on the
C       number of levels (NLEV), the number of stations (NST), the
C       number of times (NTIMES), and the number of increments (NINC).
C       Increased NRPT (total number of levels amongst all VAD reports
C       that can be processed) from 160000 to 500000 to accommodate
C       VAD wind reports from Level 2 decoder.
C 2014-08-29  S. Melchior  Extended MSK, UW, and VW array allocations
C       from 1000 to 3600 (NSTN*NTIMES*NINC) to allow for a complete
C       set of station statistics to be processed.  On occasion the
C       1000 limit had been met and exceeded, losing out on station
C       statistics.
C 2014-09-04  S. Melchior  Redefined MSK, UW, and VW arrays to
C       utilize parameters NSTN, NTIMES, and NINC rather than 
C       explicitly setting the array size at 3600. "icntmx" is no 
C       longer necessary but will be retained because it may prove
C       useful for future debugging.
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C 2022-07-21  M. Sienkiewicz  Increased NRPT and NEVNT from 500000 to
C       800000 to accommodate recent increase in numbers of reports 
C
C USAGE:    CALL INCDIST
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE INCDIST

      PARAMETER (NRPT=800000,NSTN=300,NLEV=35,NDIV=23,NTIMES=6,NINC=3)

      COMMON /STN/    SLAT(NSTN), SLON(NSTN), SIDS(NSTN), STNID(NRPT),
     &                ZSTN(NSTN)
      COMMON /INCS/   UIN(NLEV,NTIMES,NINC,NSTN),
     &                VIN(NLEV,NTIMES,NINC,NSTN),
     &                UUU(NLEV,NTIMES,NINC,NSTN),
     &                VVV(NLEV,NTIMES,NINC,NSTN),
     &                TIMIN(NLEV,NTIMES,NINC,NSTN),
     &                NIN(NLEV,NTIMES,NSTN),
     &                LLL(NLEV,NTIMES,NINC,NSTN),
     &                TFC(NLEV,NTIMES,NINC,NSTN),
     &                QQQ(NLEV,NTIMES,NINC,NSTN)
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /HTS/ IZLEV(NLEV), ZLEV(NLEV)
      COMMON /DATET/  IDATE(4), ITIM(6)

      CHARACTER*8 SIDS, STNID
      COMMON /STATS/  UW(nstn*ntimes*ninc),VW(nstn*ntimes*ninc),
     &                XM(NLEV,2),SD(NLEV,2),XLIM(2)
      COMMON /DMATYP/ IQC(NLEV,NTIMES,NINC,NSTN)
      REAL        UWS(60,NSTN),VWS(60,NSTN)
      REAL        XMS(NLEV,2,NSTN),SDS(NLEV,2,NSTN)

      INTEGER     NMS(NDIV,NLEV,2,NSTN),NCS(NLEV,2,NSTN),ICNTS(NSTN)
      INTEGER     NM(NDIV,NLEV,2),MSK(nstn*ntimes*ninc),NC(NLEV,2),
     &            icntmx
      LOGICAL BYSTN
      BYSTN = .TRUE.
      DATA XMSG /99999./
      XLIM(1) = -12.
      XLIM(2) =  12.
      MSK = 0
      icntmx = nst*ntimes*ninc
      DO L=1,NLEV

C  PUT DATA INTO WORK ARRAY
C  ------------------------

	ICNT = 0
  	UW = 0.
  	VW = 0.
  	ICNTS = 0
        UWS = 0.
        VWS = 0.
        LOOP1n1: DO IS=1,NST !  nst max is nstn
          DO IT=1,NTIMES ! ntimes max is 6
            IF(NIN(L,IT,IS).GT.0) THEN
              DO N=1,NIN(L,IT,IS) !  nin(L,IT,IS) max is 3
                IF(IQC(L,IT,N,IS).EQ.0) THEN
                  ICNT = ICNT + 1
                  UW(ICNT) = UIN(L,IT,N,IS)
                  VW(ICNT) = VIN(L,IT,N,IS)
                ENDIF
                IF(BYSTN .AND. (IQC(L,IT,N,IS).EQ.0 .OR.
     &            IQC(L,IT,N,IS).EQ.5)) THEN  ! Dont exclude bird winds
                  ICNTS(IS) = ICNTS(IS) + 1
                  if(icnts(is).gt.60) then
                    print *, 'WARNING: ICNTS(IS)>60, EXIT LOOP'
                    icnts(is) = icnts(is) - 1
                    exit LOOP1n1
                  endif
                  UWS(ICNTS(IS),IS) = UIN(L,IT,N,IS)
                  VWS(ICNTS(IS),IS) = VIN(L,IT,N,IS)
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDDO LOOP1n1

C  GET DISTRIBUTIONS
C  -----------------

	BINI = 2.0
        CALL DISTR(UW,MSK,XLIM,XMSG,ICNT,NM(1,L,1),NDIV,BINI,0,0.,
     &             NC(L,1),XM(L,1),SD(L,1),SK,XK)
        CALL DISTR(VW,MSK,XLIM,XMSG,ICNT,NM(1,L,2),NDIV,BINI,0,0.,
     &             NC(L,2),XM(L,2),SD(L,2),SK,XK)
        IF(BYSTN) THEN
          DO IS=1,NST !  nst max is nstn
            CALL DISTR(UWS(1,IS),MSK,XLIM,XMSG,ICNTS(IS),NMS(1,L,1,IS),
     &        NDIV,BINI,0,0.,NCS(L,1,IS),XMS(L,1,IS),SDS(L,1,IS),SK,XK)
            CALL DISTR(VWS(1,IS),MSK,XLIM,XMSG,ICNTS(IS),NMS(1,L,2,IS),
     &        NDIV,BINI,0,0.,NCS(L,2,IS),XMS(L,2,IS),SDS(L,2,IS),SK,XK)
          ENDDO
        ENDIF

      ENDDO

C  PRINT DISTRIBUTIONS
C  -------------------

      WRITE(6,500) (IDATE(I),I=1,4), BINI
      WRITE(6,501)
      WRITE(6,502)
      WRITE(6,503) (IZLEV(L), (NM(I,L,1),I=1,NDIV), L=1,NLEV)

      WRITE(6,504)
      WRITE(6,502)
      WRITE(6,503) (IZLEV(L), (NM(I,L,2),I=1,NDIV), L=1,NLEV)

      WRITE(6,505)
      WRITE(6,506) (IZLEV(L),NC(L,1),XM(L,1),SD(L,1),XM(L,2),
     &              SD(L,2),SQRT(SD(L,1)**2+SD(L,2)**2),L=1,NLEV)

      IF(BYSTN) THEN
       IDATE(1) = IDATE(1) - 1000
       WRITE(55) IDATE,NST,SIDS,SLAT,SLON,ZSTN
       WRITE(55) NCS,XMS,SDS,NMS,BINI,XLIM
       IDATE(1) = IDATE(1) + 1000
       DO IS=1,NST
c       WRITE(6,507) SIDS(IS), SLON(IS), SLAT(IS)
c       WRITE(6,501)
c       WRITE(6,502)
c       WRITE(6,503) (IZLEV(L), (NMS(I,L,1,IS),I=1,23), L=1,NLEV)

c       WRITE(6,507) SIDS(IS), SLON(IS), SLAT(IS)
c       WRITE(6,504)
c       WRITE(6,502)
c       WRITE(6,503) (IZLEV(L), (NMS(I,L,2,IS),I=1,23), L=1,NLEV)

        WRITE(6,507) SIDS(IS), SLON(IS), SLAT(IS)
        WRITE(6,505)
        DO L=1,NLEV
         IF(NCS(L,1,IS).GT.0) WRITE(6,506) IZLEV(L),NCS(L,1,IS),
     &    XMS(L,1,IS),SDS(L,1,IS),XMS(L,2,IS),SDS(L,2,IS),
     &    SQRT(SDS(L,1,IS)**2+SDS(L,2,IS)**2)
        ENDDO
       ENDDO
      ENDIF


  500 FORMAT(/' VAD wind increment statistics for ',I4,3I2.2,
     &       '   Bin increment = ',F8.2,' m/s',
     &       '  (bad & bird winds excluded)')
  501 FORMAT(/50X,'--------DISTRIBUTION FOR U-COMPONENT--------')
  502 FORMAT(' HEIGHT(1000s ft) <-10  -10   -9   -8   -7   -6',
     &  '   -5   -4   -3   -2   -1    0    1    2    3    4    5',
     &  '    6    7    8    9   10  >10')
  503 FORMAT(6X,I5,6X,23I5)
  504 FORMAT(/50X,'--------DISTRIBUTION FOR V-COMPONENT--------')
  505 FORMAT(/' HEIGHT(1000s ft)  Num      U-Mean  U-StdDev',
     &       '    V-Mean  V-StdDev Mag-StdDev')
  506 FORMAT(6X,I5,6X,I5,2X,5F10.1)
  507 FORMAT(/'STATISTICS FOR STATION: ',A8,'  AT (',F8.2,',',F8.2,')',
     &  '  (bad, but not bird winds excluded)')

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: INCR
C   PRGMMR: S. Melchior      ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT: Compute increments (observation - guess)
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  ADDED PARAMETERS NTIMES=6 AND NINC=3 TO TIDY
C       UP THE CODE. RENAMED NE TO NME TO EASE NAVIGATION AND FOR
C       CLARIFICATION AS NE IS USED FOR INEQUALITY TESTING (.NE.).
C       INCREASED NRPT FROM 80000 TO 160000 TO ACCOMMODATE VAD WIND
C       REPORTS FROM LEVEL 2 DECODER.
C 2014-01-15  S. Melchior  Increased NRPT (total number of levels
C       amongst all VAD reports that can be processed) from 160000 to
C       500000 to accommodate VAD wind reports from Level 2 decoder.
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C 2022-07-21  M. Sienkiewicz  Increased NRPT and NEVNT from 500000 to
C       800000 to accommodate recent increase in numbers of reports 
C
C USAGE:    CALL INCR
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE INCR

      PARAMETER (NRPT=800000,NSTN=300,NLEV=35,NTIMES=6,NINC=3)

      REAL(8)  BMISS

      COMMON /DATET/  IDATE(4), ITIM(6)
      COMMON /STN/    SLAT(NSTN), SLON(NSTN), SIDS(NSTN), STNID(NRPT),
     &                ZSTN(NSTN)
      COMMON /SINGLE/ ZOB(NRPT), ITM(NRPT),TIM(NRPT),
     &                UOB(NRPT), VOB(NRPT), LOB(NRPT),
     &                XLA(NRPT), XLO(NRPT), ZST(NRPT),
     &                UFF(NRPT), VFF(NRPT), TFF(NRPT),
     &                WQM(NRPT)
      COMMON /INCS/   UIN(NLEV,NTIMES,NINC,NSTN),
     &                VIN(NLEV,NTIMES,NINC,NSTN),
     &                UUU(NLEV,NTIMES,NINC,NSTN),
     &                VVV(NLEV,NTIMES,NINC,NSTN),
     &                TIMIN(NLEV,NTIMES,NINC,NSTN),
     &                NIN(NLEV,NTIMES,NSTN),
     &                LLL(NLEV,NTIMES,NINC,NSTN),
     &                TFC(NLEV,NTIMES,NINC,NSTN),
     &                QQQ(NLEV,NTIMES,NINC,NSTN)
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /BUFRLIB_MISSING/BMISS
      COMMON /FLAG/ guess
      LOGICAL     guess, FIRST
      CHARACTER*8 SIDS, STNID
      DATA   FIRST /.TRUE./

C  CALCULATE THE INCREMENT (OBSERVATION - GUESS)
C  ---------------------------------------------

      DO I=1,NUM !  num max is nrpt
        DO J=1,NST ! nst max is nstn
          IF(STNID(I).EQ.SIDS(J)) THEN
            CALL HT(ZOB(I),IHT,IER)
            IF(IER.EQ.0 .AND. IHT.GT.0 .AND. IHT.LE.NLEV) THEN
              IT = 0
              LOOP1n2: DO II=1,6
                IF(ITIM(II).EQ.ITM(I)) THEN
	          IT = II
                  EXIT LOOP1n2
                  ENDIF
              ENDDO  LOOP1n2
              IF(IT.GE.1 .AND. IT.LE.6) THEN

C  THE USE OF N HERE WILL CAPTURE THE FIRST TWO AND LAST
C  INCREMENTS FOR EACH HOUR
C  -----------------------------------------------------

                N = NIN(IHT,IT,J)
                IF(N.LT.3) THEN
                  N = N+1
                  NIN(IHT,IT,J) = N
                ENDIF
                IF(MAX(UFF(I),VFF(I)).LT.BMISS) THEN
                  UIN(IHT,IT,N,J)   = UOB(I) - UFF(I)
                  VIN(IHT,IT,N,J)   = VOB(I) - VFF(I)
                ELSE
                  IF(FIRST) THEN
C  Since guess is missing, mark all data as bad
                     guess = .FALSE.
                     WRITE(6,500)
                     WRITE(6,500)
                     WRITE(6,500)
  500                FORMAT(' *****GUESS IS MISSING; ALL DATA ARE',
     &                      ' FLAGGED*****')
                     FIRST = .FALSE.
                  ENDIF
                ENDIF
                LLL(IHT,IT,N,J)   = LOB(I)
                UUU(IHT,IT,N,J)   = UOB(I)
                VVV(IHT,IT,N,J)   = VOB(I)
                QQQ(IHT,IT,N,J)   = WQM(I)
                TFC(IHT,IT,N,J)   = TFF(I)
                TIMIN(IHT,IT,N,J) = TIM(I)
              ENDIF
            ELSE
              PRINT *,' UNUSUAL HT: ',ZOB(I)
            ENDIF
          ENDIF
        ENDDO
      ENDDO

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: INDEXX
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1999-08-18
C
C ABSTRACT: Indexes an array arr(1:n), i.e., outputs the array
C   indx(1:n) such that arr(indx(j)) is in ascending order for
C   j=1,2,...,N.  The input quantities n and arr are not changed.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C
C USAGE:    CALL indexx(n,arr,indx)
C   INPUT ARGUMENT LIST:
C     arr      - input array
C     n        - array size
C
C   OUTPUT ARGUMENT LIST:
C     indx     - index array for ascending order of arr
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE indexx(n,arr,indx)

      INTEGER n,indx(n),M,NSTACK
      REAL arr(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
      REAL a

C     Indexes an array arr(1:n), i.e., outputs the array indx(1:n) such
C     that arr(indx(j)) is in ascending order for j=1,2,...,N.  The
C     input quantities n and arr are not changed.

      do j=1,n
        indx(j)=j
      enddo
      jstack=0
      l=1
      ir=n
1     continue
      if(ir-l.lt.M)then
        do j=l+1,ir
          indxt=indx(j)
          a=arr(indxt)
          do i=j-1,1,-1
            if(arr(indx(i)).le.a)goto 2
            indx(i+1)=indx(i)
          enddo
          i=0
2         continue
          indx(i+1)=indxt
        enddo
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l)).gt.arr(indx(ir)))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l+1)).gt.arr(indx(l)))then
          itemp=indx(l+1)
          indx(l+1)=indx(l)
          indx(l)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l)
        a=arr(indxt)
3       continue
          i=i+1
        if(arr(indx(i)).lt.a)goto 3
4       continue
          j=j-1
        if(arr(indx(j)).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       continue
        indx(l)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
        if(jstack.gt.NSTACK) then
            print *, 'NSTACK too small in indexx'
            stop
        endif
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: INIT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT: Initialize some quantities.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  ADDED PARAMETERS NTIMES=6 AND NINC=3 TO
C       TIDY UP THE CODE. RENAMED NE TO NME FOR EASE OF 
C       NAVIGATION AND FOR CLARIFICATION AS NE IS USED FOR 
C       INEQUALITY TESTING (.NE.).
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C
C USAGE:    CALL INIT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE INIT

      PARAMETER(NSTN=300,NLEV=35,NTIMES=6,NINC=3)

      REAL(8)   BMISS

      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /INCS/   UIN(NLEV,NTIMES,NINC,NSTN),
     &                VIN(NLEV,NTIMES,NINC,NSTN),
     &                UUU(NLEV,NTIMES,NINC,NSTN),
     &                VVV(NLEV,NTIMES,NINC,NSTN),
     &                TIMIN(NLEV,NTIMES,NINC,NSTN),
     &                NIN(NLEV,NTIMES,NSTN),
     &                LLL(NLEV,NTIMES,NINC,NSTN),
     &                TFC(NLEV,NTIMES,NINC,NSTN),
     &                QQQ(NLEV,NTIMES,NINC,NSTN)
      COMMON /HTS/ IZLEV(NLEV), ZLEV(NLEV)
      COMMON /BUFRLIB_MISSING/BMISS

      ZLEV(:) = CON * IZLEV(:)

      UIN =   BMISS
      VIN =   BMISS
      TIMIN = BMISS
      NIN   = 0

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: MATR
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT: SET UP THE MATRICES FOR THE HEIGHT-TIME OI ANALYSIS
C   THE ANALYSIS EQUATION IS AW=C
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  ADDED PARAMETERS NTIMES=6 AND NINC=3 TO
C       TIDY UP THE CODE. RENAMED NE TO NME FOR EASE OF 
C       NAVIGATION AND ALSO FOR CLARIFICATION AS NE IS USED FOR
C       INEQUALITY TESTING (.NE.).
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C
C USAGE:    CALL MATR(IS,IT)
C   INPUT ARGUMENT LIST:
C     IS       - STATION INDEX
C     IT       - TIME INDEX
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE MATR(IS,IT)

      PARAMETER(NSTN=300,NLEV=35,NTIMES=6,NINC=3)
      COMMON /MATRIC/ A(NSTN,45), C(NSTN,9), NMAT(NSTN), NM
      COMMON /COLECT/ LS(NSTN,NLEV,NINC), JS(NSTN,NLEV,NINC),
     $                NS(NSTN,NLEV,NINC), NC(NLEV,NINC),
     &                DS(NSTN,NLEV,NINC),
     &                LTSCOL(9,NLEV,NTIMES,NINC,NSTN)
      COMMON /INCS/   UIN(NLEV,NTIMES,NINC,NSTN),
     &                VIN(NLEV,NTIMES,NINC,NSTN),
     &                UUU(NLEV,NTIMES,NINC,NSTN),
     &                VVV(NLEV,NTIMES,NINC,NSTN),
     &                TIMIN(NLEV,NTIMES,NINC,NSTN),
     &                NIN(NLEV,NTIMES,NSTN),
     &                LLL(NLEV,NTIMES,NINC,NSTN),
     &                TFC(NLEV,NTIMES,NINC,NSTN),
     &                QQQ(NLEV,NTIMES,NINC,NSTN)
      COMMON /HTS/  IZLEV(NLEV), ZLEV(NLEV)
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /CONSTS/ MAT,E,AA,B,XL,R
      DATA IFIRST /0/

C  SET UP THE MATRICES FOR THE HEIGHT-TIME OI ANALYSIS
C  THE ANALYSIS EQUATION IS AW=C
C  ---------------------------------------------------

      DIS(T1,T2,Z1,Z2) = SQRT((T2-T1)**2 + (R*(Z2-Z1))**2)
      COV(X) = AA + B*(1.+X/XL)*EXP(-X/XL)
      IX(I,J) = I*(I-1)/2 + J

      IF(IFIRST.EQ.0) THEN
        WRITE(6,509) MAT,E,AA,B,XL,R
  509   FORMAT(/,I3,' POINTS USED IN INTERPOLATION',/,
     &       ' NOISE-TO-SIGNAL RATIO ASSUMED TO BE:',F8.2,/,
     &       ' COV(X) = A+B*(1.+X/XL)*EXP(-X/XL) WITH A = ',
     &       F8.2,'  B = ',F8.2,' AND XL = ',F8.2,/,
     &       ' DIS(T1,T2,Z1,Z2) = SQRT((T2-T1)**2 + (R*(Z2-Z1))**2)',
     &       ' WITH R = ',F8.2/)
        IFIRST = 1
      ENDIF

      NM   = 0
      A    = 0.
      C    = 0.
      NMAT = 0
      DO L=1,NLEV
        IF(NIN(L,IT,IS).NE.0) THEN
          DO N=1,NIN(L,IT,IS) !  nin(L,IT,IS) max is 3
            NM = NM + 1
            MT = MIN(MAT,NC(L,N))
            NMAT(NM) = MT

C  MATRIX A DIAGONALS, USING TRIANGULAR FORM OF STORAGE
C  ----------------------------------------------------

            DO I=1,MT
              A(NM,IX(I,I)) = 1. + E
            ENDDO

C  REST OF MATRIX A
C  ----------------

            DO J=1,MT
              DO I=1,MT
                IF(I.GT.J) THEN
                  LI = LS(I,L,N)
                  NI = NS(I,L,N)
                  II = JS(I,L,N)
                  LJ = LS(J,L,N)
                  NJ = NS(J,L,N)
                  IJ = JS(J,L,N)
                  ZI = IZLEV(LI)
                  ZJ = IZLEV(LJ)
                  D  = DIS(TIMIN(LI,II,NI,IS),TIMIN(LJ,IJ,NJ,IS),
     &                     ZI,ZJ)
                  A(NM,IX(I,J)) = COV(D)
                  IF(NM.LE.2) THEN
                  ENDIF
                ENDIF
              ENDDO
            ENDDO

C  RIGHT-HAND-SIDE, MATRIX C
C  -------------------------

            DO I=1,MT
              C(NM,I) = COV(DS(I,L,N))
            ENDDO
          ENDDO
        ENDIF
      ENDDO

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: DMA
C   PRGMMR: S. Melchior      ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT: THIS IS THE DECISION MAKING ALGORITHM.  IT DETERMINES
C   THE DATA QUALITY.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2001-10-08  D. KEYSER   CORRECTED ERROR WHICH HAD RESULTED IN NO
C       Q.C. PERFORMED ON LEVELS WHERE EITHER THE U- OR V- INCREMENT
C       WAS ZERO; LIMITING V-WIND INCREMENT FOR APPLYING BIRD
C       CONTAMINATION TIGHTENED FROM +8 TO +5 M/S IN SPRING AND FROM
C       -8 TO -5 M/S IN FALL, MORE LEVELS WILL NOW BE FLAGGED
C 2012-03-05  S. MELCHIOR  ADDED PARAMETERS NTIMES=6, NINC=3, AND
C       NEVNT=160000 TO TIDY UP THE CODE. ADDED DIAGNOSTIC OUTPUT
C       STATEMENTS FOR INSTANCES WHEN LIMITS ARE EXCEEDED. RENAMED NE
C       TO NME FOR EASE OF NAVIGATION AND ALSO FOR CLARIFICATION AS NE
C       IS USED FOR INEQUALITY TESTING (.NE.). INCREASED NRPT FROM
C       80000 TO 160000 TO ACCOMMODATE VAD WIND REPORTS FROM LEVEL 2
C       DECODER.
C 2014-01-15  S. Melchior  Increased NRPT (total number of levels
C       amongst all VAD reports that can be processed) and NEVNT (total
C       number of events amongst all VAD reports that can be processed)
C       both from 160000 to 500000 to accommodate VAD wind reports from
C       Level 2 decoder.
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C 2022-07-21  M. Sienkiewicz  Increased NRPT and NEVNT from 500000 to
C       800000 to accommodate recent increase in numbers of reports 
C
C USAGE:    CALL DMA(HONOR_FLAGS)
C   INPUT ARGUMENT LIST:
C     HONOR_FLAGS  - TRUE IF EXISTING FLAGS ARE HONORED
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE DMA(HONOR_FLAGS)

      PARAMETER (NRPT=800000,NSTN=300,NLEV=35,NTIMES=6,NINC=3)
      PARAMETER (nevnt=800000)
      CHARACTER*8 SIDS, STNID, SIDEV

      REAL(8)   BMISS

      COMMON /HTRES/  RGU(NLEV,NTIMES,NINC,NSTN),
     &                RGV(NLEV,NTIMES,NINC,NSTN)
      COMMON /INCS/   UIN(NLEV,NTIMES,NINC,NSTN),
     &                VIN(NLEV,NTIMES,NINC,NSTN),
     &                UUU(NLEV,NTIMES,NINC,NSTN),
     &                VVV(NLEV,NTIMES,NINC,NSTN),
     &                TIMIN(NLEV,NTIMES,NINC,NSTN),
     &                NIN(NLEV,NTIMES,NSTN),
     &                LLL(NLEV,NTIMES,NINC,NSTN),
     &                TFC(NLEV,NTIMES,NINC,NSTN),
     &                QQQ(NLEV,NTIMES,NINC,NSTN)
      COMMON /EVENTS/ SIDEV(nevnt), HTEV(nevnt),LEV(nevnt),
     &                TIMEV(nevnt), UEV(nevnt), VEV(nevnt),
     &                QALEV(nevnt), RCEV(nevnt)
      COMMON /STATS/  UW(nstn*ntimes*ninc),VW(nstn*ntimes*ninc),
     &                XM(NLEV,2),SD(NLEV,2),XLIM(2)
      COMMON /STN/    SLAT(NSTN), SLON(NSTN), SIDS(NSTN), STNID(NRPT),
     &                ZSTN(NSTN)
      COMMON /COLECT/ LS(NSTN,NLEV,NINC), JS(NSTN,NLEV,NINC),
     &                NS(NSTN,NLEV,NINC), NC(NLEV,NINC),
     &                DS(NSTN,NLEV,NINC),
     &                LTSCOL(9,NLEV,NTIMES,NINC,NSTN)
      COMMON /HTS/ IZLEV(NLEV), ZLEV(NLEV)
      COMMON /CONSTS/ MAT,E,AA,B,XL,R
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /BIRDS/  BIRDTIME
      COMMON /DMATYP/ IQC(NLEV,NTIMES,NINC,NSTN)
      COMMON /DATET/  IDATE(4), ITIM(6)
      COMMON /BUFRLIB_MISSING/BMISS
      common /switches/  print_52, print_53, print_60, test
      common /flag/ guess
      LOGICAL         VSNG, VDBL, TEST, VBIRD, VBIG,
     &                HONOR_FLAGS, guess
      integer numerr(0:9,0:NLEV), ndata(0:NLEV)

      IQC    = 0
      ndata  = 0
      numerr = 0
      NME     = 0
      nme1    = 0
      nme2    = 0
      DO IS=1,NST ! nst max is nstn
        DO IT=1,6
          VECINL = 0.
          VECZTL = 0.
          LAST   = 0
          DO L=1,NLEV
            IF(NIN(L,IT,IS).NE.0) THEN
              DO N=1,NIN(L,IT,IS) !  nin(L,IT,IS) max is 3
                VSNG  = .FALSE.
                VDBL  = .FALSE.
                VBIRD = .FALSE.
                VBIG  = .FALSE.

c               count data
c               ----------

                ndata(0) = ndata(0) + 1
                ndata(l) = ndata(l) + 1

                if(guess)  then

C------------------------------------------------------------------
C  VECIN AND VECZT ARE ESTIMATES OF STANDARDIZED NORMAL VARIATE
C------------------------------------------------------------------

                   IF(SD(L,1).NE.0. .AND. SD(L,2).NE.0.) THEN
                     VECIN = SQRT(((UIN(L,IT,N,IS)-XM(L,1))/SD(L,1))**2
     &                          + ((VIN(L,IT,N,IS)-XM(L,2))/SD(L,2))**2)
                   ELSE
                     VECIN = SQRT(((UIN(L,IT,N,IS)-XM(L,1))/5.5)**2
     &                          + ((VIN(L,IT,N,IS)-XM(L,2))/5.5)**2)
                   ENDIF
                   VECZT = SQRT((RGU(L,IT,N,IS)/1.4)**2
     &                        + (RGV(L,IT,N,IS)/1.4)**2)

                   B     = 10.0        ! Max error
                   PER   = 0.07        ! Proportion of rough errors
                   WLIM1 = 0.55
                   WLIM2 = 0.3

                   WP  = 0.5*(ANLWT(VECZT ,B,PER) + ANLWT(VECIN ,B,PER))
                   WPL = 0.5*(ANLWT(VECZTL,B,PER) + ANLWT(VECINL,B,PER))
                   IF(WP.LT.WLIM2)          VSNG = .TRUE.
                   IF(MAX(WP,WPL).LT.WLIM1) VDBL = .TRUE.

                   CP  = 0.5*(CUMPROB(VECZT ) + CUMPROB(VECIN ))
                   CPL = 0.5*(CUMPROB(VECZTL) + CUMPROB(VECINL))
cwgc               IF(CP.GE..995 .OR. (UUU(L,IT,N,IS)**2
cwgc &               + VVV(L,IT,N,IS)**2).LT.1.)  VSNG = .TRUE.
cwgc               IF(MIN(CP,CPL).GE.0.995)       VDBL = .TRUE.

                   IF((UUU(L,IT,N,IS)**2 + VVV(L,IT,N,IS)**2).GT.1.)THEN
                      IF(TFC(L,IT,N,IS).GT.-3..OR.TFC(L,IT,N,IS).GE.
     &                 BMISS) THEN
cdak  10/08/2001         IF(BIRDTIME.EQ.1. AND. VIN(L,IT,N,IS).GT. 8.)
                         IF(BIRDTIME.EQ.1. AND. VIN(L,IT,N,IS).GT. 5.)
     &                    VBIRD = .TRUE.
cdak  10/08/2001         IF(BIRDTIME.EQ.2. AND. VIN(L,IT,N,IS).LT.-8.)
                         IF(BIRDTIME.EQ.2. AND. VIN(L,IT,N,IS).LT.-5.)
     &                    VBIRD = .TRUE.
                      END IF
                      IF(MAX(ABS(UIN(L,IT,N,IS)),ABS(VIN(L,IT,N,IS)))
     &                 .GT.12.) VBIG = .TRUE.
                   END IF

                   ITYP = 0
                   IF(VBIRD)  THEN
                      ITYP = 5
                   ELSE IF((UUU(L,IT,N,IS)**2+VVV(L,IT,N,IS)**2).LT.1.)
     &              THEN
                      ITYP = 4
                   ELSE IF(VSNG) THEN
                      ITYP = 1
                      IF(VDBL) ITYP = 3
                   ELSE IF(VDBL) THEN
                      ITYP = 2
                   ELSE  IF(VBIG) THEN
                      ITYP = 7
                   END IF
                else
                   ityp = 9
                end if

C  DO NOT WRITE A NEW EVENT IF PRE-EXISTING FLAG IS TO BE HONORED
C  AND INDICATES DATA BAD OR NOT TO BE USED
C  --------------------------------------------------------------

                IF(HONOR_FLAGS .AND. QQQ(L,IT,N,IS).GE.4.) THEN
                  IQC(L,IT,N,IS) = QQQ(L,IT,N,IS)
                  ITYP = 0
                ENDIF

                IF(ITYP.NE.0 .OR. .NOT.HONOR_FLAGS) THEN
                  numerr(ityp,0) = numerr(ityp,0) + 1
                  numerr(ityp,l) = numerr(ityp,l) + 1

C  SAVE EVENTS
C  -----------

                  NME  = NME + 1
                  NME1 = NME1 + 1
                  if(nme.gt.nev) then
                    print *, 'WARNING: NME > NEV, STOP PROCESSING'
                    go to 900
                  endif
                  SIDEV(NME) = SIDS(IS)
                  HTEV(NME)  = ZLEV(L)
                  LEV(NME)   = LLL(L,IT,N,IS)
                  TIMEV(NME) = TIMIN(L,IT,N,IS)
                  UEV(NME)   = UUU(L,IT,N,IS)
                  VEV(NME)   = VVV(L,IT,N,IS)
                  IF(ITYP.NE.0) THEN
                    QALEV(NME) = 13.
                  ELSE
                    QALEV(NME) = 1.
                  ENDIF
                  RCEV(NME)  = ITYP
                  IQC(L,IT,N,IS) = ITYP
                  IF(VDBL .AND. LAST.NE.0) THEN
                    NME  = NME + 1
                    NME2 = NME2 + 1
                    if(nme.gt.nev) then
                      print *, 'WARNING: NME > NEV, STOP PROCESSING'
                      go to 900
                    endif
                    SIDEV(NME) = SIDS(IS)
                    HTEV(NME)  = ZLEV(LAST)
                    LEV(NME)   = LLL(LAST,IT,N,IS)
                    TIMEV(NME) = TIMIN(LAST,IT,N,IS)
                    UEV(NME)   = UUU(LAST,IT,N,IS)
                    VEV(NME)   = VVV(LAST,IT,N,IS)
                    QALEV(NME) = 13.
                    RCEV(NME)  = ITYP
                    IQC(LAST,IT,N,IS) = ITYP
                  ENDIF

C  WRITE STATION INFORMATION
C  -------------------------

                  IF(TEST) THEN
                     WRITE(6,500)
                     WRITE(6,501) SIDS(IS),TIMIN(L,IT,N,IS),L,IT,
     &                N,UUU(L,IT,N,IS),VVV(L,IT,N,IS),UIN(L,IT,N,IS),
     &                VIN(L,IT,N,IS),RGU(L,IT,N,IS),RGV(L,IT,N,IS),
     &                CP,CPL,WP,WPL,ITYP,TFC(L,IT,N,IS)
                  END IF
                  ITEMP = 0
                  ISUBT = 0
                  IF(LTSCOL(1,L,IT,N,IS).NE.0) THEN

C  WRITE INFORMATION ABOUT DATA USED IN Z-T INTERPOLATION
C  ------------------------------------------------------

                    LOOP1: DO I=1,9
                      IF(LTSCOL(I,L,IT,N,IS).EQ.0) EXIT LOOP1
                      LL = LTSCOL(I,L,IT,N,IS)/100
                      IF(LL.LT.1 .OR. LL.GT.NLEV) EXIT LOOP1
                      ITS = LTSCOL(I,L,IT,N,IS)-100*LL
                      IF(ITS.NE.0) THEN
                         ITEMP = ITS/10
                         ISUBT = ITS-10*ITEMP
                         IF(ITEMP.LT.1 .OR. ITEMP.GT.5 .OR.
     &                      ISUBT.LT.1 .OR. ISUBT.GT.3) EXIT LOOP1
                      END IF
                      DIST = SQRT((TIMIN(L,IT,N,IS)
     &                  - TIMIN(LL,ITEMP,ISUBT,IS))**2
     &                  + (R**2) * (IZLEV(L)-IZLEV(LL))**2)
                      if(guess)  then
                         IF(SD(LL,1).NE.0. .AND. SD(LL,2).NE.0.) THEN
                            VECTIN =
     &                       SQRT(((UIN(LL,ITEMP,ISUBT,IS)-XM(LL,1))
     &                       / SD(LL,1))**2 + ((VIN(LL,ITEMP,ISUBT,IS)
     &                       - XM(LL,2))/SD(LL,2))**2)
                         ELSE
                            VECTIN =
     &                       SQRT(((UIN(LL,ITEMP,ISUBT,IS)-XM(LL,1))
     &                       / 5.5)**2 + ((VIN(LL,ITEMP,ISUBT,IS)
     &                       - XM(LL,2))/5.5)**2)
                         ENDIF
                      end if
                      IF(TEST) WRITE(6,504) DIST,LL,ITEMP,ISUBT,
     &                 UIN(LL,ITEMP,ISUBT,IS),VIN(LL,ITEMP,ISUBT,IS)
                    ENDDO  LOOP1
                  END IF
                ENDIF
                VECZTL = VECZT
                VECINL = VECIN
                LAST = L
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDDO

  500 FORMAT(/1X,'STN/DIST  TIME   L  IT   N (   UOB,    VOB)',
     &       ' (  UIN,    VIN) (  RGU,    RGV)     CP     CPL',
     &       '      WP     WPL ITYP     TFC')
  501 FORMAT(1X,A8,F6.2,3I4,6F8.2,4F8.3,I5,F8.1)
  504 FORMAT(1X,F8.1,3I4,16X,2F8.2)

      write(6,511) (idate(j),j=1,4),nst
      write(6,512)
      write(6,513) ndata(0),(numerr(i,0),i=1,5),numerr(7,0),numerr(9,0),
     &             nme1
      do l=1,NLEV
        nsum = 0
        do ie=1,7
          nsum = nsum + numerr(ie,l)
        enddo
        write(6,514) izlev(l),ndata(l),(numerr(i,l),i=1,5),
     &    numerr(7,l),numerr(9,l),nsum
      enddo
  511 format(/' Date: ',i4,3i2.2,'   Number of stations reporting: ',i4)
  512 format(21x,'<---------------error type------------->',/,
     &  ' Height  Data_count    1     2     3     4     5     7     9',
     &       '  Error_count')
  513 format('    all',i9,2x,7i6,i8)
  514 format(1x,i6,i9,2x,7i6,i8)

      RETURN

  900 CONTINUE
      WRITE(6,505)
  505 FORMAT(' TOO MANY EVENTS')

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: RESDIST
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT: COMPUTE STATISTICS AND DISTRIBUTIONS OF RESIDUALS OF
C   CHECKS.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  EXTENDED MSK ARRAY ALLOCATION FROM 899 TO
C       1000 TO ALLOW FOR MORE STATION STATISTICS TO BE PROCESSED -
C       ON OCCASION THE 899 LIMIT HAD BEEN MET AND EXCEEDED. ADDED
C       PARAMETERS NDIV=23, NTIMES=6, NINC=3 TO TIDY UP THE CODE.
C       ADDIED DIAGNOSTIC OUTPUT STATEMENTS FOR INSTANCES WHEN LIMITS
C       ARE EXCEEDED. RENAMED NE TO NME FOR EASE OF NAVIGATION AND 
C       FOR CLARIFICATION AS NE IS USED FOR INEQUALITY TESTING (.NE.).
C 2014-01-15  S. Melchior Added integer "icntmx" to define the max
C       value of icnt in the multi-nested do loop that hinges on the
C       number of levels (NLEV), the number of stations (NST), the
C       number of times (NTIMES), and the number of increments (NINC).
C 2014-08-29  S. Melchior  Extended MSK, UW, and VW array allocations
C       from 1000 to 3600 (NSTN*NTIMES*NINC) to allow for a complete 
C       set of station statistics to be processed.  On occasion the
C       1000 limit had been met and exceeded, losing out on station 
C       statistics.
C 2014-09-04  S. Melchior  Redefined MSK, UW, and VW arrays to
C       utilize parameters NSTN, NTIMES, and NINC rather than 
C       explicitly setting the array size at 3600. "icntmx" is no 
C       longer necessary but will be retained because it may prove
C       useful for future debugging.
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C
C USAGE:    CALL RESDIST
C
C   OUTPUT ARGUMENT LIST:
C     UNIT 06 - STANDARD OUTPUT PRINT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE RESDIST

      PARAMETER(NSTN=300,NLEV=35,NDIV=23,NTIMES=6,NINC=3)

      COMMON /INCS/   UIN(NLEV,NTIMES,NINC,NSTN),
     &                VIN(NLEV,NTIMES,NINC,NSTN),
     &                UUU(NLEV,NTIMES,NINC,NSTN),
     &                VVV(NLEV,NTIMES,NINC,NSTN),
     &                TIMIN(NLEV,NTIMES,NINC,NSTN),
     &                NIN(NLEV,NTIMES,NSTN),
     &                LLL(NLEV,NTIMES,NINC,NSTN),
     &                TFC(NLEV,NTIMES,NINC,NSTN),
     &                QQQ(NLEV,NTIMES,NINC,NSTN)
      COMMON /HTRES/  RGU(NLEV,NTIMES,NINC,NSTN),
     &                RGV(NLEV,NTIMES,NINC,NSTN)
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /HTS/    IZLEV(NLEV), ZLEV(NLEV)
      COMMON /DATET/  IDATE(4), ITIM(6)
      COMMON /DMATYP/ IQC(NLEV,NTIMES,NINC,NSTN)
      REAL        UW(nstn*ntimes*ninc),VW(nstn*ntimes*ninc),
     &            XM(NLEV,2),SD(NLEV,2),XLIM(2)
      INTEGER     NM(NDIV,NLEV,2),MSK(nstn*ntimes*ninc),NC(NLEV,2),
     &            icntmx
      DATA XLIM /-5.,5./, XMSG /99999./

      MSK = 0
      icntmx = nst*ntimes*ninc

      DO L=1,NLEV ! nlev max is 35

C  PUT DATA INTO WORK ARRAY
C  ------------------------

        ICNT = 0
        UW = 0
        VW = 0
        LOOP1: DO IS=1,NST !  nst max is nstn
          DO IT=1,NTIMES ! ntimes max is 6
            IF(NIN(L,IT,IS).GT.0) THEN
              DO N=1,NIN(L,IT,IS) !  nin(L,IT,IS) max is 3
                IF(IQC(L,IT,N,IS).EQ.0) THEN
                  ICNT = ICNT + 1
                  UW(ICNT) = RGU(L,IT,N,IS)
                  VW(ICNT) = RGV(L,IT,N,IS)
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDDO  LOOP1

C  GET DISTRIBUTIONS
C  -----------------

        BINI = 0.5
        CALL DISTR(UW,MSK,XLIM,XMSG,ICNT,NM(1,L,1),NDIV,BINI,0,0.,
     &             NC(L,1),XM(L,1),SD(L,1),SK,XK)
        CALL DISTR(VW,MSK,XLIM,XMSG,ICNT,NM(1,L,2),NDIV,BINI,0,0.,
     &             NC(L,2),XM(L,2),SD(L,2),SK,XK)

      ENDDO

C  PRINT DISTRIBUTIONS
C  -------------------

      WRITE(6,500) (IDATE(I),I=1,4), BINI
  500 FORMAT(/' VAD wind z-t residual statistics for ',I4,3I2.2,
     &       '   Bin increment = ',F8.2,' m/s')
      WRITE(6,501)
  501 FORMAT(/50X,'--------DISTRIBUTION FOR U-COMPONENT--------')
      WRITE(6,502)
  502 FORMAT(' HEIGHT(1000s ft) <-10  -10   -9   -8   -7   -6',
     &  '   -5   -4   -3   -2   -1    0    1    2    3    4    5',
     &  '    6    7    8    9   10  >10')
      WRITE(6,503) (IZLEV(L), (NM(I,L,1),I=1,NDIV), L=1,NLEV)
  503 FORMAT(6X,I5,6X,23I5)

      WRITE(6,504)
  504 FORMAT(/50X,'--------DISTRIBUTION FOR V-COMPONENT--------')
      WRITE(6,502)
      WRITE(6,503) (IZLEV(L), (NM(I,L,2),I=1,NDIV), L=1,NLEV)

      WRITE(6,505)
  505 FORMAT(/' HEIGHT(1000s ft)  Num      U-Mean  U-StdDev',
     &       '    V-Mean  V-StdDev Mag-StdDev')
      WRITE(6,506) (IZLEV(L),NC(L,1),XM(L,1),SD(L,1),XM(L,2),
     &              SD(L,2),SQRT(SD(L,1)**2+SD(L,2)**2),L=1,NLEV)
  506 FORMAT(6X,I5,6X,I5,2X,5F10.1)

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: SELECT
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT: SELECT DATA TO BE USED IN Z-T OI ANALYSIS
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  ADDED PARAMETERS NTIMES=6 AND NINC=3 TO
C       TIDY UP THE CODE. RENAMED NE TO NME FOR EASE OF 
C       NAVIGATION AND FOR CLARIFICATION SINCE NE IS USED FOR
C       INEQUALITY TESTING (.NE.).
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C
C USAGE:    CALL SELECT(IS,IT)
C   INPUT ARGUMENT LIST:
C     IS       - STATION INDEX
C     IT       - TIME INDEX
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE SELECT(IS,IT)

      PARAMETER(NSTN=300,NLEV=35,NTIMES=6,NINC=3)
      COMMON /INCS/   UIN(NLEV,NTIMES,NINC,NSTN),
     &                VIN(NLEV,NTIMES,NINC,NSTN),
     &                UUU(NLEV,NTIMES,NINC,NSTN),
     &                VVV(NLEV,NTIMES,NINC,NSTN),
     &                TIMIN(NLEV,NTIMES,NINC,NSTN),
     &                NIN(NLEV,NTIMES,NSTN),
     &                LLL(NLEV,NTIMES,NINC,NSTN),
     &                TFC(NLEV,NTIMES,NINC,NSTN),
     &                QQQ(NLEV,NTIMES,NINC,NSTN)
      COMMON /HTS/ IZLEV(NLEV), ZLEV(NLEV)
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /COLECT/ LS(NSTN,NLEV,NINC), JS(NSTN,NLEV,NINC),
     &                NS(NSTN,NLEV,NINC), NC(NLEV,NINC),
     &                DS(NSTN,NLEV,NINC),
     &                LTSCOL(9,NLEV,NTIMES,NINC,NSTN)
      COMMON /CONSTS/ MAT,E,AA,B,XL,R

      DS = 0.
      LS = 0
      JS = 0
      NS = 0
      NC = 0

C  SELECT DATA TO BE USED IN Z-T OI ANALYSIS
C  -----------------------------------------

C  OBS IS AT (L,IT,IS,N).  CALCULATE DISTANCE TO NEIGHBORS.
C  --------------------------------------------------------

      DO L=1,NLEV
        IF(NIN(L,IT,IS).NE.0) THEN
          DO N=1,NIN(L,IT,IS) !  nin(L,IT,IS) max is 3
            NM = 0
            L1 = MAX(L-3,1)
            L2 = MIN(L+3,NLEV)
            DO LL=L1,L2
              DO I=1,NTIMES
                IF(NIN(LL,I,IS).NE.0) THEN
                  DO NN=1,NIN(LL,I,IS) !  nin(LL,I,IS) max is 3
                    IF(.NOT. ((L.EQ.LL) .AND. (I.EQ.IT) .AND.
     &                       (N.EQ.NN))) THEN
                      NM = NM + 1
                      DS(NM,L,N) = SQRT((TIMIN(L,IT,N,IS)
     &                  - TIMIN(LL,I,NN,IS))**2 + (R**2) *
     &                  (IZLEV(L)-IZLEV(LL))**2)
                      LS(NM,L,N) = LL
                      JS(NM,L,N) = I
                      NS(NM,L,N) = NN
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
            ENDDO
            NC(L,N) = NM
          ENDDO
        ENDIF
      ENDDO

C  SORT THE QUANTITIES BY DESCENDING DISTANCE
C  ------------------------------------------

      DO L=1,NLEV
        IF(NIN(L,IT,IS).NE.0) THEN
          DO N=1,NIN(L,IT,IS) !  nin(L,IT,IS) max is 3
            CALL SORTD(DS(1,L,N),LS(1,L,N),JS(1,L,N),NS(1,L,N),NC(L,N))
            LTSCOL(1:9,L,IT,N,IS) = 
     &                        100*LS(1:9,L,N)+10*JS(1:9,L,N)+NS(1:9,L,N)
          ENDDO
        ENDIF
      ENDDO

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: SOLVE
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT:  SOLVE THE MATRIX PROBLEMS
C   A IS THE SYMMETRIC MATRIX (IN TRIANGULAR FORM)
C   C IS THE RIGHT-HAND-SIDE VECTOR ON INPUT AND
C   THE ANALYSIS WEIGHTS ON OUTPUT FROM DRCTSL.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  ADDED PARAMETERS NTIMES=6 AND NINC=3 TO
C       TIDY UP THE CODE. RENAMED NE TO NME FOR EASE OF
C       NAVIGATION AND FOR CLARIFICATION AS NE IS USED FOR
C       INEQUALITY TESTING (.NE.).
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C
C USAGE:    CALL SOLVE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE SOLVE

      PARAMETER(NSTN=300,NLEV=35,NTIMES=6,NINC=3)
      REAL            DPR(NSTN,1)
      COMMON /INCS/   UIN(NLEV,NTIMES,NINC,NSTN),
     &                VIN(NLEV,NTIMES,NINC,NSTN),
     &                UUU(NLEV,NTIMES,NINC,NSTN),
     &                VVV(NLEV,NTIMES,NINC,NSTN),
     &                TIMIN(NLEV,NTIMES,NINC,NSTN),
     &                NIN(NLEV,NTIMES,NSTN),
     &                LLL(NLEV,NTIMES,NINC,NSTN),
     &                TFC(NLEV,NTIMES,NINC,NSTN),
     &                QQQ(NLEV,NTIMES,NINC,NSTN)
      COMMON /MATRIC/ A(NSTN,45), C(NSTN,9), NMAT(NSTN), NM
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME

C  SOLVE THE MATRIX PROBLEMS
C    A IS THE SYMMETRIC MATRIX (IN TRIANGULAR FORM)
C    C IS THE RIGHT-HAND-SIDE VECTOR ON INPUT AND
C    THE ANALYSIS WEIGHTS ON OUTPUT FROM DRCTSL.

      CALL DRCTSL(A,C,DPR,NMAT,9,NM,1)

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: SORTD
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2016-12-18
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C
C ABSTRACT: SORT D, L, J, N, ALL ACCORDING TO ORDER OF D.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C
C USAGE:    CALL SORTD(D,L,J,N,NC)
C   INPUT ARGUMENT LIST:
C     D        - ARRAY GOVERNING ORDER OF SORT FOR ALL
C     L        - ARRAY TO SORT
C     J        - ARRAY TO SORT
C     N        - ARRAY TO SORT
C     NC       - NUMBER OF ELEMENTS TO SORT
C
C   OUTPUT ARGUMENT LIST:
C     D        - SORTED ARRAY
C     L        - SORTED ARRAY
C     J        - SORTED ARRAY
C     N        - SORTED ARRAY
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE SORTD(D,L,J,N,NC)

      PARAMETER(NSTN=300)
      REAL    D(*), W(NSTN)
      INTEGER L(*), J(*), N(*), INDX(NSTN), IW(NSTN)
      INDX = 0

      CALL INDEXX(NC,D,INDX)

      W(1:NC) = D(1:NC)
      D(1:NC) = W(INDX(1:NC))

      IW(1:NC) = L(1:NC)
      L(1:NC) = IW(INDX(1:NC))

      IW(1:NC) = J(1:NC)
      J(1:NC) = IW(INDX(1:NC))

      IW(1:NC) = N(1:NC)
      N(1:NC) = IW(INDX(1:NC))

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: VSOLVE
C   PRGMMR: WOOLLEN          ORG: NMC22      DATE: 2016-12-18
C
C ABSTRACT: CHOLESKY TYPE SOLUTION FOR ARRAYS OF POSITIVE DEFINITE
C   SYMMETRIC MATRIXES.
C
C PROGRAM HISTORY LOG:
C   90-11-06  J. WOOLLEN
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C
C USAGE:
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
      SUBROUTINE VSOLVE (A,B,NDIM,BAD,NFT,NS,MAXDIM)

      PARAMETER(NSTN=300)
      DIMENSION A(NSTN,45),B(NSTN,9,1),NDIM(NSTN),BAD(NSTN),T(NSTN)
      LOGICAL BAD

      DATA CNUM/1.E-15/

C----------------------------------------------------------------------
      IX (I,J) = I*(I-1)/2 + J
C----------------------------------------------------------------------

      N = MAXDIM

      BAD(1:NS) = .FALSE.

C  DECOMPOSE THE MATRIXES
C  ----------------------

      DO I=1,N
         DO J=1,I
            T(1:NS) = A(1:NS,IX(I,J))
            DO K=1,J-1
               DO M=1,NS
                  T(M) = T(M) - A(M,IX(I,K)) * A(M,IX(J,K))
               END DO
            END DO
            IF(I.GT.J) THEN
               DO M=1,NS
                  A(M,IX(I,J)) = T(M) * A(M,IX(j,J))
               END DO
            ELSE
               DO M=1,NS
                  IF(T(M).LT.CNUM .AND. NDIM(M).GE.I) BAD(M) = .TRUE.
                  IF(T(M).LE.0) T(M) = 1.
               END DO
               A(1:NS,IX(I,I)) = 1./SQRT(T(1:NS))
            ENDIF
         END DO
      END DO

C  SOLVE FOR ALL RIGHT HAND SIDES
C  ------------------------------

      DO NF=1,NFT

C  FORWARD SUBSTITUTION
C  --------------------

         DO I=1,N
            T(1:NS) = B(1:NS,I,NF)
            DO J=1,I-1
               DO M=1,NS
                  T(M) = T(M) - A(M,IX(I,J)) * B(M,J,NF)
               END DO
            END DO
            B(1:NS,I,NF) = T(1:NS) * A(1:NS,IX(I,I))
         END DO

C  BACKWARD SUBSTITUTION
C  ---------------------

         DO I=N,1,-1
            T(1:NS) = B(1:NS,I,NF)
            IF(I.NE.N) THEN
               DO J=I+1,N
                  DO M=1,NS
                     T(M) = T(M) - A(M,IX(J,I)) * B(M,J,NF)
                  END DO
               END DO
            END IF
            B(1:NS,I,NF) = T(1:NS) * A(1:NS,IX(I,I))
         END DO
      END DO

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: ZTOI
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 1999-08-18
C
C ABSTRACT: PERFORM OI ANALYSIS IN Z-T PLANE, ONE STATION AT A TIME.
C   PERFORM OI ANALYSIS FOR ALL POSSIBLE HOURS.
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  ADDED PARAMETER NTIMES=6 TO TIDY UP THE
C       CODE. RENAMED NE TO NME FOR EASE OF NAVIGATION AND FOR
C       CLARIFICATION SINCE NE IS USED FOR INEQUALITY TESTING
C       (.NE.).
C
C USAGE:    CALL ZTOI
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE ZTOI

      PARAMETER (NLEV=35,NTIMES=6)
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME

C  PERFORM OI ANALYSIS IN Z-T PLANE, ONE STATION AT A TIME.
C  PERFORM OI ANALYSIS FOR THE CENTRAL 3 HOURS
C  --------------------------------------------------------

      DO IS=1,NST !  nst max is nstn
        DO IT=1,NTIMES

C  SELECT DATA FOR THE ANALYSIS
C  ----------------------------

          CALL SELECT(IS,IT)

C  SET UP MATRICES
C  ---------------

          CALL MATR(IS,IT)

C  SOLVE MATRICES
C  --------------

          CALL SOLVE

C  GET RESIDUALS
C  -------------

          CALL ZTRES(IS,IT)

        ENDDO
      ENDDO
      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: ZTRES
C   PRGMMR: W. COLLINS       ORG: NP22       DATE: 2016-12-18
C
C ABSTRACT: SOLVE FOR THE HEIGHT-TIME OI ANALYSIS RESIDUALS
C
C PROGRAM HISTORY LOG:
C 1999-08-18  W. COLLINS
C 2012-03-05  S. MELCHIOR  ADDED PARAMETERS NTIMES=6 AND NINC=3 TO
C       TIDY UP THE CODE. RENAMED NE TO NME FOR EASE OF 
C       NAVIGATION AND FOR CLARIFICATION AS NE IS USED FOR
C       INEQUALITY TESTING (.NE.).
C 2016-12-18  D. Stokes  Increased NSTN (maximum number of stations to
C       process) from 200 to 300.
C
C USAGE:    CALL ZTRES(IS,IT)
C   INPUT ARGUMENT LIST:
C     IS       - STATION INDEX
C     IT       - TIME INDEX
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE ZTRES(IS,IT)

      PARAMETER(NSTN=300,NLEV=35,NTIMES=6,NINC=3)
      REAL WU(NSTN), WV(NSTN)

      COMMON /MATRIC/ A(NSTN,45), C(NSTN,9), NMAT(NSTN), NM
      COMMON /PARAMS/ IUNIT, IUTIM, IOUT, CON, IHE, NUM, NST,
     &                CQCPC, NEV, NME
      COMMON /INCS/   UIN(NLEV,NTIMES,NINC,NSTN),
     &                VIN(NLEV,NTIMES,NINC,NSTN),
     &                UUU(NLEV,NTIMES,NINC,NSTN),
     &                VVV(NLEV,NTIMES,NINC,NSTN),
     &                TIMIN(NLEV,NTIMES,NINC,NSTN),
     &                NIN(NLEV,NTIMES,NSTN),
     &                LLL(NLEV,NTIMES,NINC,NSTN),
     &                TFC(NLEV,NTIMES,NINC,NSTN),
     &                QQQ(NLEV,NTIMES,NINC,NSTN)
      COMMON /COLECT/ LS(NSTN,NLEV,NINC), JS(NSTN,NLEV,NINC),
     &                NS(NSTN,NLEV,NINC), NC(NLEV,NINC),
     &                DS(NSTN,NLEV,NINC),
     &                LTSCOL(9,NLEV,NTIMES,NINC,NSTN)
      COMMON /HTRES/  RGU(NLEV,NTIMES,NINC,NSTN),
     &                 RGV(NLEV,NTIMES,NINC,NSTN)

C  SOLVE FOR THE HEIGHT-TIME OI ANALYSIS RESIDUALS
C  -----------------------------------------------

      NN = 0
      DO L=1,NLEV
        IF(NIN(L,IT,IS).NE.0) THEN
          DO N=1,NIN(L,IT,IS) !  nin(L,IT,IS) max is 3
            NN = NN + 1
            RGU(L,IT,N,IS) = UIN(L,IT,N,IS)
            RGV(L,IT,N,IS) = VIN(L,IT,N,IS)
            LOOP1: DO I=1,NMAT(NN)

              IF(LTSCOL(I,L,IT,N,IS).EQ.0) CYCLE LOOP1
              LL = LTSCOL(I,L,IT,N,IS)/100
              IF(LL.LT.1 .OR. LL.GT.NLEV) CYCLE LOOP1
              ITS = LTSCOL(I,L,IT,N,IS)-100*LL
              IF(ITS.EQ.0) CYCLE LOOP1
              ITEMP = ITS/10
              ISUBT = ITS-10*ITEMP

              RGU(L,IT,N,IS) = RGU(L,IT,N,IS) -
     &            C(NN,I) * UIN(LL,ITEMP,ISUBT,IS)
              RGV(L,IT,N,IS) = RGV(L,IT,N,IS) -
     &            C(NN,I) * VIN(LL,ITEMP,ISUBT,IS)
            ENDDO  LOOP1
            WU(NN) = RGU(L,IT,N,IS)
            WV(NN) = RGV(L,IT,N,IS)
          ENDDO
        ENDIF
      ENDDO

      RETURN
      END
