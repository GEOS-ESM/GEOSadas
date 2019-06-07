C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RADEVN      PREPARES REPORT FOR RADIATION CORRECTIONS
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2013-02-05
C
C ABSTRACT: PREPARES FOR RADIOSONDE HEIGHT AND TEMPERATURE CORRECTIONS
C   DUE TO INTERSONDE DIFFERENCES (MAINLY RADIATIVE EFFECTS ON THE
C   INSTRUMENT (SEE REMARKS FOR LEVELS).  THE SOLAR HOUR ANGLE AND
C   SOLAR ELEVATION ANGLE ARE CALCULATED. THE PROPER SUBROUTINE
C   CONTAINING CORRECTION TABLES IS THEN CALLED.  THE CORRECTIONS ARE
C   THEN APPLIED DIRECTLY TO THE MANDATORY LEVELS, AND (FOR THE
C   "NEWER" TABLES) THE TEMPERATURE CORRECTIONS ARE INTERPOLETED TO
C   THE SURFACE, SIGINIFICANT AND TROPOSPHERIC LEVELS.  THIS SUBROUTINE
C   HANDLES ONE REPORT AT A TIME.
C
C PROGRAM HISTORY LOG:
C 1994-02-27  J. WOOLLEN   -- COPIED FROM ORIGINAL "GETRAD" SUROUTINE
C     WHICH WAS PART OF RADNCORR PROGRAM, AND CONVERTED FOR USE WITH
C     BUFR
C 1996-02-20  D. KEYSER    -- CORRECTED PROBLEM OF DOUBLE CORRECTION
C     OF HEIGHTS AND TEMPERATURES AT AND ABOVE 10 MB WHICH CAN OCCUR
C     DUE TO IMPRECISION IN STORAGE OF PRESSURE OB
C 2002-07-22  D. KEYSER    -- IF BALLOON DRIFT COORDINATES (LAT/LON/
C     D-TIM) ARE AVAILABLE, WILL USE THESE TO MORE PRECISELY OBTAIN
C     ESTIMATED SUN ANGLE (DONE IN NEW SUBROUTINE SOELAN); MORE
C     INFO RE RADCOR NOW WRITTEN TO UNIT 68
C 2006-06-01  R. Todling  -- bumpped up NST from 899 to 1500 follow 
C                            J. Woollen's recommendation.
C 2007-09-14  D. KEYSER    -- INCREASED THE UPPER-LIMIT FOR THE NUMBER
C     OF RAOB IDS STORED BY RADIOSONDE TYPE IN ARRAY SIDRAD (FOR LATER
C     LISTING AT THE END OF THE MAIN PROGRAM WHEN SUMMARIZING RADCOR
C     STATS) FROM 400 TO 800 SINCE SOME REANALYSIS RUNS WERE EXCEEDING
C     THE OLD LIMIT CAUSING ARRAY OVERFLOW (AND MEMORY CLOBBERING)
C     ISSUES; INCREASED MAXIMUM NUMBER OF RADIOSONDE STATIONS THAT CAN
C     BE PROCESSED FROM 899 TO 999 (ONE REANLYSIS RUN HIT THIS LIMIT)
C 2008-04-10  D. A. KEYSER -- CAN HANDLE RADIOSONDE TYPES > 99 WHICH
C     WILL SOON BE INTRODUCED INTO THE BUFR DATABASE (BASED ON NOVEMBER
C     2007 WMO BUFR UPDATE)
C 2008-10-08  D. A. KEYSER -- NOW PROCESSES RADIOSONDE TYPES > 111 AND
C     < 255 RATHER THAN SETTING THEM TO MISSING (255), EVEN THOUGH
C     THERE ARE CURRENTLY NO VALID TYPES > 111; MODIFIED TO RE-INDEX
C     ARRAY "SOLAR" (SUN ANGLE ON MAND. LVLS) IN OLDEST SET OF
C     CORRECTIONS (SUBR. RADT1) SO THAT THE CORRECT VALUES RETURNED
C     FROM SUBR. SOELAN ARE USED FOR CASES WHERE BALLOON DRIFT
C     INFORMATION IS PRESENT (ONLY APPLIES TO REANALYSIS SINCE BALLOON
C     DRIFT WAS NOT AVAILABLE BACK WHEN SUBR. RADT1 WAS CALLED IN REAL-
C     TIME RUNS, SUBR. RADT1 IS NEVER CALLED IN CURRENT REAL-TIME RUNS
C     ANYWAY); MODIFIED TO RE-INDEX ARRAY "SOLAR" IN SUBR. SOELAN FOR
C     CASES INVOLVING THE OLDEST SET OF CORRECTIONS AND WHERE BALLOON
C     DRIFT INFORMATION WAS NOT AVAILABLE SO THAT PROPER VALUES ARE
C     RETURNED TO CALLING SUBR. RADT1 (ONLY APPLIES TO REANALYSIS SINCE
C     SUBR. RADT1 IS NEVER CALLED IN CURRENT REAL-TIME RUNS); UPDATED
C     LIST OF RADIOSONDE TYPES IN NEWEST CORRECTION SUBR. (RADT5) TO
C     CONFORM WITH LATEST (NOV. 2007) WMO BUFR CODE TABLE 0-02-011
C 2008-11-19  D. A. KEYSER -- UPDATED NEWEST CORRECTION SUBR. (RADT5)
C     TO RECOGNIZE RADIOSONDE TYPES CONFORMING WITH LATEST (NOV. 2008)
C     WMO BUFR CODE TABLE 0-02-011 AND TO PRODUCE MORE SPECIFIC
C     DIAGNOSTIC PRINT RELATED TO RADIOSONDE TYPES ENCOUNTERED (I.E.,
C     INVALID, OBSOLETE, VACANT, ETC.)
C 2012-11-20  J. WOOLLEN  INITIAL PORT TO WCOSS 
C 2013-02-05  D. Keyser   Final changes to run on WCOSS: Set BUFRLIB
C     missing (BMISS) to 10E8 rather than 10E10 to avoid integer
C     overflow; use formatted print statements where previously
C     unformatted print was > 80 characters.
C 2013-09-18  M. Sienkiewicz  Completed merge of WCOSS code with GMAO
C     version.
C
C USAGE:    CALL RADENV
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 68  - RADCOR INFORMATION FILE
C
C REMARKS: CORRECTIONS ARE APPLIED AT AND ABOVE THE FOLLOWING LEVELS
C   BASED ON VARIABLE "IRCTBL":
C                          IRCTBL = 1 --  100 MB ;
C                          IRCTBL = 2 --  700 MB ;
C                          IRCTBL = 3 -- 1000 MB ;
C                          IRCTBL = 4 --  700 MB ;
C                          IRCTBL = 5 --  700 MB ;
C   CALLED BY SUBROUTINE "DMA22".
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE RADEVN
      PARAMETER (NST=1500)
      PARAMETER (NRID=800) ! max # of raob ids listed per inst. type

      REAL(8) BMISS
      REAL             SOLAR(16)
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
     &                TDW(255),TDQ(255),TDR(255),INQ(255),
     &                UO(255), VO(255), SPO(255), DIRO(255),
     &                WQ(255), WR(255)
      COMMON /RADCOM/ HGT(16),TMP(16),DHT(16),DTP(16),JTYPE
      COMMON /SWITCH/ LWCORR,LEVRAD,IRCTBL,HGTTBL,BAL_DRIFT
      COMMON /PMAND / PRES(16),KMIN,KMAX,INM(16)
      COMMON/COUNT/   NTYPE(69),KTYPE(69),SIDNOR(2000),SIDRAD(NRID,255),
     &                IICNT,JJCNT(255)
      COMMON /DATEX/   IDATE(5), CDATE
      COMMON /FILES/   NFIN, NFOUT, NFEVN, NFSUM, NFSTN, NFSGL
      COMMON /PARAMS/  MAND,NOBS,XMI,XMA,YMI,YMA,NLEV,VTPPC,RADPC,
     &                 LMAND(0:21),LSFC,ISC,IPEVT,PRNT,CQCPC,TT
      COMMON /TESTS/   TEST
      COMMON /DATE_IDT/IDT
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL          TEST, HGTTBL, BAL_DRIFT

      CHARACTER*1  VAR(2)
      CHARACTER*40 TEVN,ZEVN
      CHARACTER*8  SID, SIDNOR, SIDRAD
      CHARACTER*10     CDATE
      DIMENSION    KPRES(21),IFIRST(255)
      LOGICAL      INIT,NOSWC, PRNT

      DATA KPRES /1000 , 925 , 850 , 700 , 500 , 400 , 300,
     .             250 , 200 , 150 , 100 ,  70 ,  50 ,  30,
     .              20 ,  10 ,   7 ,   5 ,   3 ,   2 ,   1/
      DATA TEVN  /'TOB TQM TPC TRC                         '/
      DATA ZEVN  /'ZOB ZQM ZPC ZRC                         '/
      DATA INIT  /.TRUE./
      DATA VAR   /'Z','T'/
      DATA IFIRST /255*0/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  FIRST TIME CALL PREP TO READ NAMELISTS AND SET PARAMETERS
C  ---------------------------------------------------------

      IF(INIT) THEN
         INIT = .FALSE.
         LEVRAD = 0
         NTYPE = 99999
         KTYPE = 0
         IICNT = 0
         JJCNT = 0
         SIDNOR = '        '
         SIDRAD = '        '
         PRINT 321, IDATE
         WRITE(68,321)  IDATE
  321 FORMAT(//23X,'WELCOME TO THE RADIOSONDE INTERSONDE (RADIATION) ',
     $ 'CORRECTION PART OF THE CQC PROGRAM'/53X,'LAST UPDATED 05 FEB ',
     $ '2013'///45X,'DATE FROM BUFR MESSAGES IS: ',I3.2,I2.2,3I3.2//)
         WRITE(68,322)  CDATE
  322    FORMAT(3X,'CDATE is ',A)
         CALL PREP(IDATE)
         PRES = KPRES(1:16)
      ENDIF

C  INITIALIZE ALL UNCORRECTED HEIGHTS AND TEMPS AS MISSING
C  -------------------------------------------------------

      KMIN  = LEVRAD
      KMAX  = 16
      DHT   = 0
      DTP   = 0
      NEV   = 0
      TMP   = BMISS
      HGT   = BMISS
      TO    = BMISS
      ZO    = BMISS
      IN    = BMISS
      INM   = 0


      ALON  = MOD(720.+360.-XOB(IS),360.)
      JTYPE = ITP(IS)
      IF(JTYPE.LE.0 .OR. (JTYPE.GE.255.AND.JTYPE.NE.20000)) JTYPE = 255
      IF(JTYPE.NE.20000)  CALL TAB(JTYPE) ! (inst. type may be set to
                                          !  20000 for special cases in
                                          !  cqcbufr.f)

      IF(IRCTBL.EQ.1)  THEN

C  COME HERE FOR IRCTBL=1 CORRECTIONS (OLDEST TABLES)
C  --------------------------------------------------

C     NEW CHECKS FOR THE NEW RADIOSONDE TYPES
C     ---------------------------------------

         IF(JTYPE.EQ.14)  JTYPE =  4  ! VAISALA (IN/OUT OF FINLAND)

         IF(JTYPE.EQ.20)  JTYPE = 12  ! RUSSIAN RKZ
         IF(JTYPE.EQ.21)  JTYPE = 12  ! UNKNOWN RUSSIAN

C     NEW CHECKS FOR THE OLD RADIOSONDE TYPES
C     ---------------------------------------

C        EXCLUDE CHINESE (WMO BLOCKS 50-59) FROM RUSSIAN TYPE
C        ----------------------------------------------------

         IF(SID(IS)(1:1).EQ.'5')  JTYPE = 19

      ENDIF

      BAL_DRIFT = .TRUE.

C  FILL THE MANDATORY LEVEL EVENT AND CORRECTION ARRAYS FOR THIS REPORT
C  --------------------------------------------------------------------

      DO L=1,NLV
         M = MANLEV(POB(L))
         IF(M.GT.0 .AND. M.LE.KMAX) THEN
            TMP(M) = TOB(L)
            HGT(M) = ZOB(L)
            INM(M)  = L
            IF(MAX(HRDR(INM(M)),XDR(INM(M)),YDR(INM(M))).GE.BMISS)
     $       BAL_DRIFT = .FALSE.
         ENDIF
      ENDDO

      WRITE (68,*)
      IF(.NOT.BAL_DRIFT) THEN
         WRITE(68,'(" Report ",A," does not have balloon drift ",
     &    "coordinates - use ""old"" method for determining sun ",
     &    "angle")') SID(IS)
      ELSE
         WRITE(68,'(" Report ",A," has balloon drift coordinates - use",
     &    " ""new"" method for determining sun angle")') SID(IS)
      END IF

C  CALL APPROPRIATE SUBROUTINE TO APPLY CORRECTIONS - SAVE DELTAS
C  --------------------------------------------------------------

      IF(IRCTBL.EQ.1)  CALL RADT1(NOSWC,IRET)
      IF(IRCTBL.EQ.2)  CALL RADT2(NOSWC,IRET)
      IF(IRCTBL.EQ.3)  CALL RADT3(IRET)
      IF(IRCTBL.EQ.4)  CALL RADT4(IRET)
      IF(IRCTBL.EQ.5)  CALL RADT5(IRET)

      IF(IRET.NE.0)  THEN
         if(iret.eq.-1)  then
            IICNT = IICNT + 1
            IF(IICNT.LT.2001)  THEN
               SIDNOR(IICNT) = SID(IS)
            ELSE
               PRINT'(" WARNING: > 2000 REPORTS WITH NO CORRECTION ",
     .          "- CAN ONLY PRINT FIRST 2000 REPORTS IN SUMMARY AT ",
     .          "END")'
               WRITE(68,'("WARNING: > 2000 REPORTS WITH NO CORRECTION ",
     .          "- CAN ONLY PRINT FIRST 2000 REPORTS IN SUMMARY AT ",
     $          "END")')
            END IF
cppppp
         else
               WRITE(68,'("^^^^^Here is a IRET = -99")')
cppppp
         end if
         RETURN
      END IF

      JJCNT(JTYPE) = JJCNT(JTYPE) + 1
      IF(JJCNT(JTYPE).LE.NRID)  THEN
         SIDRAD(JJCNT(JTYPE),JTYPE) = SID(IS)
      ELSE IF(IFIRST(JTYPE).EQ.0) THEN
         WRITE(6,'("WARNING: AT LEAST ",I4," REPORTS WITH INST. TYPE ",
     .    I3.3," - CAN ONLY PRINT FIRST ",I4," REPORTS IN CORRECTION ",
     .    "SUMMARY AT END")') JJCNT(JTYPE),JTYPE,NRID
         WRITE(68,'("WARNING: AT LEAST ",I4," REPORTS WITH INST. TYPE ",
     .    I3.3," - CAN ONLY PRINT FIRST ",I4," REPORTS IN CORRECTION ",
     .    "SUMMARY AT END")') JJCNT(JTYPE),JTYPE,NRID
         IFIRST(JTYPE) = 1
      END IF

      IF(IRCTBL.LE.2.AND.NOSWC) KMIN = 16

C  APPLY CORRECTIONS TO MANDATORY LEVELS
C  -------------------------------------

      LSFM = LMANLV(PS(IS))
      KMIN = MAX0(KMIN,LSFM+1)
      DO M=KMIN,KMAX
         IF(INM(M).GT.0 .AND. INM(M).LE.NLV) THEN
         IF(TMP(M).LT.0.5*BMISS .AND. TOB(INM(M)).LT.0.5*BMISS) THEN
            IV = 2
            CALL RSTATS(IV,TOB(INM(M))*10.,TMP(M)*10.,ALON*100.,
     &         YOB(IS)*100.,KPRES(M))
            IF(DTP(M).NE.0) THEN
               NEV = NEV+1
               TO(NEV) = TMP(M)
               TQ(NEV) = TQM(INM(M))
               TR(NEV) = 1
               INT(NEV) = INM(M)
cppppp
cdak           IF(ITP(IS).EQ.49) THEN
cppppp
                  WRITE(68,722) DHR(IS),ITP(IS),SID(IS)(1:5),
     &                          NINT(POB(INM(M))),VAR(IV),TOB(INM(M)),
     &                          TMP(M),TFC(INM(M))
  722             FORMAT(' DHR=',F5.2,' IT=',I3,' ID=',A5,' P=',I4,1X,
     &                   A1,': ORG=',F7.1,' COR=',F7.1,' FST=',F7.1)
cppppp
cdak           ENDIF
cppppp
               TOB(INM(M)) = TMP(M)
            ENDIF
         ENDIF
         ENDIF
      ENDDO

      CALL EVENT(NFOUT,TEVN,NLV,TO,TQ,TR,INT,NEV,RADPC)
      WRITE(60,510) NEV,SID(IS)
      WRITE(68,510) NEV,SID(IS)
  510 FORMAT(' RADEVN--',I3,' TEMPERATURE EVENTS WRITTEN FOR: ',A8)

      NEV = 0
      DO M=KMIN,KMAX
         IF(INM(M).GT.0 .AND. INM(M).LE.NLV) THEN
         IF(HGT(M).LT.0.5*BMISS .AND. ZOB(INM(M)).LT.0.5*BMISS) THEN
           IV = 1
           CALL RSTATS(IV,ZOB(INM(M)),HGT(M),ALON*100.,
     &       YOB(IS)*100.,KPRES(M))
            IF(DHT(M).NE.0) THEN
               NEV = NEV+1
               ZO(NEV) = HGT(M)
               ZQ(NEV) = ZQM(INM(M))
               ZR(NEV) = 1
               INZ(NEV) = INM(M)
cppppp
cdak           IF(ITP(IS).EQ.49) THEN
cppppp
                  WRITE(68,722) DHR(IS),ITP(IS),SID(IS)(1:5),
     &                          NINT(POB(INM(M))),VAR(IV),ZOB(INM(M)),
     &                          HGT(M),ZFC(INM(M))
cppppp
cdak           ENDIF
cppppp
               ZOB(INM(M)) = HGT(M)
            ENDIF
         ENDIF
         ENDIF
      ENDDO

      CALL EVENT(NFOUT,ZEVN,NLV,ZO,ZQ,ZR,INZ,NEV,RADPC)
      WRITE(60,511) NEV,SID(IS)
      WRITE(68,511) NEV,SID(IS)
  511 FORMAT(' RADEVN--',I3,' HEIGHT EVENTS WRITTEN FOR: ',A8)

      IF(IRCTBL.EQ.1) RETURN

C INTERPOLATE CORRECTIONS TO SFC, SIG, TROP LVLS & MAND. LVLS ABOVE KMAX
C ----------------------------------------------------------------------

      NEV = 0
      LSURF = NLV+1
      DO L=1,NLV
        IF(ABS(POB(L)-PS(IS)).LT..05) THEN
          LSURF = L+1
          GOTO 10
        ENDIF
      ENDDO
   10 CONTINUE
      IV = 2
      DO L=LSURF,NLV
         IF(MANLEV(POB(L)).EQ.0 .OR. NINT(POB(L)).LT.KPRES(KMAX)) THEN
            TCOR = PILNLNP(POB(L),PRES,DTP,KMAX)
            IF(TOB(L).LT.0.5*BMISS .AND. TCOR.NE.0) THEN
               IF(MANLEV(POB(L)).GT.0) THEN
                 CALL RSTATS(IV,TOB(L)*10.,(TOB(L)+TCOR)*10.,
     &             ALON*100.,YOB(IS)*100.,NINT(POB(L)))
               ENDIF
               NEV = NEV+1
               TO(NEV) = TOB(L) + TCOR
cppppp
               IF(ITP(IS).EQ.49) THEN
cppppp
                  WRITE(68,822) SID(IS)(1:5),NINT(POB(L)),VAR(IV),
     &                          TOB(L),TO(NEV)
  822             FORMAT(' ID=',A5,' P=',I4,1X,A1,': ORG=',F7.1,' COR=',
     &                   F7.1)
cppppp
               ENDIF
cppppp
               TOB(L)  = TO(NEV)
               TQ(NEV) = TQM(L)
               TR(NEV) = 1
               INT(NEV) = L
            ENDIF
         ENDIF
      ENDDO
      CALL EVENT(NFOUT,TEVN,NLV,TO,TQ,TR,INT,NEV,RADPC)
      WRITE(60,510) NEV,SID(IS)
      WRITE(68,510) NEV,SID(IS)

      NEV = 0
      IV = 1
      DO L=LSURF,NLV
         IF(MANLEV(POB(L)).EQ.0 .OR. NINT(POB(L)).LT.KPRES(KMAX)) THEN
            ZCOR = PILNLNP(POB(L),PRES,DHT,KMAX)
            IF(ZOB(L).LT.0.5*BMISS .AND. ZCOR.NE.0 .AND. L.NE.ISF(IS))
     &        THEN
               IF(MANLEV(POB(L)).GT.0) THEN
                 CALL RSTATS(IV,ZOB(L),ZOB(L)+ZCOR,ALON*100.,
     &             YOB(IS)*100.,NINT(POB(L)))
               ENDIF
               NEV = NEV+1
               ZO(NEV) = ZOB(L) + ZCOR
cppppp
               IF(ITP(IS).EQ.49) THEN
cppppp
                  WRITE(68,822) SID(IS)(1:5),NINT(POB(L)),VAR(IV),
     &                          ZOB(L),ZO(NEV)
cppppp
               ENDIF
cppppp
               ZOB(L)  = ZO(NEV)
               ZQ(NEV) = ZQM(L)
               ZR(NEV) = 1
               INZ(NEV) = L
            ENDIF
         ENDIF
      ENDDO
      CALL EVENT(NFOUT,ZEVN,NLV,ZO,ZQ,ZR,INZ,NEV,RADPC)
      WRITE(60,511) NEV,SID(IS)
      WRITE(68,511) NEV,SID(IS)

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    PREP        SET UP VARIABLES FOR RADIATION CORRECTION
C   PRGMMR: J. WOOLLEN       ORG: GSC        DATE: 1994-02-27
C
C ABSTRACT: DOES PRELIMINARY WORK PRIOR TO APPLYING RADIOSONDE HEIGHT
C   AND TEMPERATURE CORRECTIONS DUE TO INTERSONDE DIFFERENCES (MAINLY
C   RADIATIVE EFFECTS ON THE INSTRUMENT).  THE JULIAN DAY AND TRUE
C   SOLAR NOON ARE DETERMINED FOR THIS DAY OF THE YEAR.  BASED ON THE 
C   DATE OF THE DATA (FROM THE BUFR MESSAGE) THE PROPER VALUE FOR THE
C   VARIABLE "IRCTBL" IS DETERMINED (THIS WILL LATER POINT TO PROPER
C   SET OF CORRECTION TABLES).
C
C PROGRAM HISTORY LOG:
C 1994-02-27  J. WOOLLEN   -- COPIED FROM ORIGINAL PARTS OF ORIGINAL
C     RADNCORR PROGRAM, AND CONVERTED FOR USE WITH BUFR
C
C USAGE:    CALL PREP(IDATE)
C   INPUT ARGUMENT LIST:
C     IDATE    - 5-WORD INTEGER {YY(1-2),YY(3-4),MM,DD,HH} CONTAINING
C                BUFR MESSAGE DATE
C
C   INPUT FILES:
C     UNIT 04  - NAMELIST CONTAINING VARIABLES READ INTO SUBROUTINE
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 68  - RADCOR INFORMATION FILE
C
C REMARKS: CALLED BY SUBROUTINE "RADEVN".
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE PREP(IDATE)

      COMMON /SWITCH/ LWCORR,LEVRAD,IRCTBL,HGTTBL,BAL_DRIFT
      COMMON /SUNNY / IDAYR,IDAYSY,TSNOON
      COMMON /DATE_IDT/IDT
      COMMON /USVAI/ CORUSVAI

      LOGICAL   HGTTBL, CORUSVAI
      NAMELIST/KDTA/IRCTBL
      NAMELIST/LDTA/LEVRAD,LWCORR,CORUSVAI

      DIMENSION IDATE(5),JDATE(5)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C***********************************************************************
C  COMPUTE: IDAYR   = DAY OF THE YEAR
C           IDAYSY  = TOTAL NUMBER OF DAYS IN YEAR
C           TSNOON  = TRUE SOLAR NOON FOR TODAY'S DATE (HRS GMT)
C***********************************************************************

      IYR = 100*IDATE(1) + IDATE(2)
      IMO = IDATE(3)
      IDY = IDATE(4)
      CALL W3FS13(IYR,IMO,IDY,IDAYR)
      CALL W3FS13(IYR,12,31,IDAYSY)
      DANGL  = 6.2831853 * (REAL(IDAYR) - 79.)/REAL(IDAYSY)
      SDGL   = SIN(DANGL)
      CDGL   = COS(DANGL)
      TSNOON = -.030*SDGL-.120*CDGL+.330*SDGL*CDGL+.0016*SDGL**2-.0008
      WRITE(60,500) IYR,IMO,IDY,IDAYR,IDAYSY,TSNOON
      WRITE(68,500) IYR,IMO,IDY,IDAYR,IDAYSY,TSNOON
  500 FORMAT(' PREP--IYR,IMO,IDY,IDAYR,IDAYSY,TSNOON:',I6,2I3,2I5,F10.4)

C-----------------------------------------------------------
C  IDTTS1 IS NO. OF MIN. FROM 00Z 01/01/78 TO 12Z 07/30/1986
C  IDTTS2 IS NO. OF MIN. FROM 00Z 01/01/78 TO 12Z 01/22/1992
C  IDTTS3 IS NO. OF MIN. FROM 00Z 01/01/78 TO 12Z 12/17/1991
C  IDTTS4 IS NO. OF MIN. FROM 00Z 01/01/78 TO 06Z 03/18/1992
C  IDTTS5 IS NO. OF MIN. FROM 00Z 01/01/78 TO 00Z 10/01/1993
C  IDTTS6 IS NO. OF MIN. FROM 00Z 01/01/78 TO 00Z 01/01/1998
C  IDTTS7 IS NO. OF MIN. FROM 00Z 01/01/78 TO 00Z 01/01/2000
C  IDT    IS NO. OF MIN. FROM 00Z 01/01/78 TO THE DD/MM/YYYY
C-----------------------------------------------------------

      IDTTS1 = 4510800
      IDTTS2 = 7393680
      IDTTS3 = 7341840
      IDTTS4 = 7473960
      IDTTS5 = 8282880
      IDTTS6 = 10519200
      IDTTS7 = 10519931  ! number of min from 00Z 01/01/78 TO
                         ! 00Z 01/01/2000 is actually 11570400

C  FIGURE OUT WHAT DATE THIS IS
C  ----------------------------

      JDATE(1) = IDATE(1)*100 + IDATE(2)
      JDATE(2) = IDATE(3)
      JDATE(3) = IDATE(4)
      JDATE(4) = IDATE(5)
      JDATE(5) = 0
      CALL W3FS21(JDATE,IDT)

C  HEIGHT CORRECTIONS ARE NOT OBTAINED FROM TABLES AFTER 10/1/93
C  -------------------------------------------------------------

      HGTTBL = (IDT.LT.IDTTS5)

C  THE DEFAULT VALUE FOR IRCTBL DEPENDS ON THE DATE OF THE DATA
C  ------------------------------------------------------------

      IF(IDT.LT.IDTTS1)  THEN
         IRCTBL = 1
      ELSE  IF(IDT.LT.IDTTS2)  THEN
         IRCTBL = 2
      ELSE IF(IDT.LT.IDTTS7) THEN
         IRCTBL = 3
      ELSE
         IRCTBL = 5 
      END IF
      READ(4,KDTA,END=1,err=1)
    1 CONTINUE

C  THE DEF. VALUES FOR LWCORR AND LEVRAD DEPEND UPON THE VALUE OF IRCTBL
C  ---------------------------------------------------------------------

      LWCORR =  1
      LEVRAD = 11
      IF(IRCTBL.GE.2)  LEVRAD = 1
      READ(4,LDTA,END=2,err=2)
    2 CONTINUE

C  FOR IRCTBL = 3, LWCORR IS NOT APPLICABLE SO IT IS ALWAYS SET TO ZERO
C  ---------------------------------------------------------------------
      IF(IRCTBL.EQ.3)  LWCORR = 0

C  PRINT THE PARAMETERS AND EXIT
C  -----------------------------

      WRITE(6,KDTA)
      WRITE(68,KDTA)
      WRITE(6,LDTA)
      WRITE(68,LDTA)
      IF(IRCTBL.EQ.1)  THEN
         PRINT 200
         WRITE(68,200)
      ELSE IF(IRCTBL.EQ.2)  THEN
         PRINT 201
         WRITE(68,201)
      ELSE IF(IRCTBL.EQ.3)  THEN
         PRINT 202
         WRITE(68,202)
      ELSE IF(IRCTBL.EQ.4)  THEN
         PRINT 204
         WRITE(68,204)
      ELSE IF(IRCTBL.EQ.5)  THEN
         PRINT 205
         WRITE(68,205)
      END IF
      IF(LEVRAD.LE.0)  THEN
         PRINT 203
         WRITE(68,203)
      END IF

      RETURN

C  FORMAT STATEMENTS
C  -----------------

200   FORMAT(//,13X,'CORRECTIONS ARE BASED ON STUDY BY MCINTURFF AND ',
     &       'FINGER (1968) -- OPERATIONAL ONLY PRIOR TO 12Z 07/30/86')
201   FORMAT(//,9X,'CORRECTIONS ARE BASED ON STUDY BY MCINTURFF ET AL.',
     &       ' (1979) -- OPERATIONAL 12Z 07/30/86 TO 12Z 01/22/92')
202   FORMAT(//,5X,'CORRECTIONS ARE BASED ON STUDY BY SCHMIDLIN(1990) ',
     &       'AND P JULIAN (NMC ON 374) -- OPERATIONAL 12Z 01/22/92',//)
203   FORMAT(10X,'===> SWITCH "LEVRAD" = 0 -- NO CORRECTIONS APPLIED',/)
204   FORMAT(//,5X,'CORRECTIONS ARE BASED ON STUDY BY COLLINS',
     &  ' (1999)',//)
205   FORMAT(//,5X,'CORRECTIONS ARE BASED ON STUDY BY COLLINS',
     &' (2000)',//)

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RADT1       APPLIES OLDEST SET OF RADIATION CORRECTNS
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2008-10-08
C
C ABSTRACT: THIS SUBROUTINE APPLIES RADIATION CORRECTIONS TO THE
C   OBSERVED RADIOSONDE HEIGHTS AND TEMPERATURES FOR ALL SIX
C   STRATOSPHERIC LEVELS: 100, 70, 50, 30, 20, AND 10 MB.  DATA ARE
C   PASSED THROUGH COMMON /RADCOM/.  THE CORRECTION OF RAOBS FOR SHORT-
C   WAVE RADIATIVE EFFECTS IS ACCOMPLISHED BY READING IN TABLES OF
C   NUMBERS WHICH ARE DEPENDENT ON THE RADIOSONDE TYPE.  THE CORRECTION
C   OF RAOBS FOR LONG-WAVE RADIATION (AT 10 MB ONLY) IS BASED ON THE
C   4'TH POWER OF THE 10 MB TEMPERATURE.  CALLED FOR ONE REPORT
C   AT A TIME.
C
C PROGRAM HISTORY LOG:
C 1971-12-19  KEITH JOHNSON -- W/NMC??
C 1973-09-13  PPC
C 1990-12-05  D. A. KEYSER -- CONVERTED TO VS FORTRAN(77)& RESTRUCTURED
C 1994-02-27  J. WOOLLEN   -- CONVERTED FOR USE WITH BUFR
C 2002-07-22  D. A. KEYSER -- CALLS NEW ROUTINE SOELAN TO CALCULATE SUN
C     ANGLE ON VALID MANDATORY LEVELS RATHER THAN DOING CALCULATION IN
C     THIS SUBROUTINE
C 2007-09-14  D. KEYSER    -- MINOR CODE CORRECTIONS SUGGESTED BY PAT
C     PAULEY (CORRECTS PROBLEMS THAT COULD OCCUR ON SOME REMOTE
C     MACHINES)
C 2008-10-08  D. KEYSER    -- MODIFIED TO RE-INDEX ARRAY "SOLAR" (SUN
C     ANGLE ON MAND. LVLS) SO THAT THE CORRECT VALUES RETURNED FROM
C     SUBR. SOELAN ARE USED FOR CASES WHERE BALLOON DRIFT INFORMATION
C     IS PRESENT (ONLY APPLIES TO REANALYSIS SINCE BALLOON DRIFT WAS
C     NOT AVAILABLE BACK WHEN THIS SUBR. WAS CALLED IN REAL-TIME RUNS,
C     THIS SUBR. IS NEVER CALLED IN CURRENT REAL-TIME RUNS ANYWAY)
C
C USAGE:    CALL RADT1(NOSWC,IRET)
C   OUTPUT ARGUMENT LIST:
C     NOSWC    - LOGICAL INDICATING NO SHORT-WAVE CORRECTION APPLIED
C     IRET     - RETURN CODE (0=CORRECTIONS, -1=NO CORRECTIONS,
C              - -99=??????)
C
C REMARKS: THESE CORRECTIONS USE THE ORIGINAL NMC OFFICE NOTE 29
C   RADIOSONDE TYPE CODES OBTAINED FROM THE NMC UPPER-AIR DICTIONARY.
C   CALLED BY SUBROUTINE "RADEVN".
C
C
C   KEY FOR RADIOSONDE TYPES USED HERE:
C
C JTYPE(ON29)  CODE NAME (DICTIONARY)  DESCRIPTION
C
C     UNSPEC.        T5N    (INACTIVE) FRENCH METOX
C        1           T1J               U.S. NOAA / VIZ
C        2 ** NO CORRECTION **         RESERVED
C        3           T3L               U.S. AN/AMT 4
C        4           T4M               FINNISH VAISALA (IN FINLAND)
C        5 ** NO CORRECTION **         FRENCH MESURAL (IN FRANCE)
C        6 ** NO CORRECTION (INACTIVE) PORTUGAL (CANADIAN MODEL IV)
C        7 ** NO CORRECTION **         W. GERMAN GRAW
C        8 ** NO CORRECTION **         RESERVED
C        9           T9R               JAPANESE "CODE SENDING"
C       10           TASLSH (INACTIVE) E. GERMAN FREIBERG
C       11           TBS               U.K. KEW
C       12           TCT               U.S.S.R A-22
C       13 ** NO CORRECTION **         RESERVED
C       14 (CHGED. TO JTYPE= 4 INSTR)  FINNISH VAISALA (IN/OUT FINLAND)
C       15 (CHGED. TO JTYPE= 5 INSTR)  FRENCH MESURAL (OUT OF FRANCE)
C       16 ** NO CORRECTION **         AUSTRALIAN PHILLIPS
C       17 ** NO CORRECTION (INACTIVE) AUSTRALIAN "DIAMOND HINMAN"
C       18 ** NO CORRECTION **         CANADIAN "SANGAMO"
C       19 ** NO CORRECTION **         CHINESE
C       20 (CHGED. TO JTYPE=12 INSTR)  U.S.S.R. RKZ
C       21 (CHGED. TO JTYPE=12 INSTR)  U.S.S.R. UNKNOWN (AVG. A-22&RKZ)
C       22 ** NO CORRECTION **         INDIAN MET. SERVICE
C       23 ** NO CORRECTION **         AUSTRIAN ELIN
C       24 ** NO CORRECTION **         KOREAN JINYANG / VIZ
C       25 ** NO CORRECTION **         SWISS METEOLABOR
C       26 ** NO CORRECTION **         CZECH VINOHRADY
C       27 ** NO CORRECTION **         U.S. MSS (SPACE DATA CORP.)
C       28.. 97  ** NO CORRECTION **   RESERVED
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE RADT1(NOSWC,IRET)

      REAL(8) BMISS

      COMMON /RADCOM/ HGT(16),TMP(16),DHT(16),DTP(16),JTYPE
      COMMON /SWITCH/ LWCORR,LEVRAD,IRCTBL,HGTTBL,BAL_DRIFT
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      REAL         CUTOFF(6),SOLAR(16),PRES6(6),DTS(6),DHS(6),ALP(5),
     .             ANGLE(11),T1J(66),T3L(66),T4M(66),T5N(66),T9R(66),
     .             TASLSH(66),TBS(66),TCT(66),H1J(11),H3L(11),H4M(11),
     .             H5N(11),H9R(11),HASLSH(11),HBS(11),HCT(11),
     .             TTAB(66,8),HTAB(11,8)
      INTEGER      ITYPTB(255)
      EQUIVALENCE  (TTAB(1,1),T1J(1)),(TTAB(1,2),T3L(1)),
     .             (TTAB(1,3),T4M(1)),(TTAB(1,4),T5N(1)),
     .             (TTAB(1,5),T9R(1)),(TTAB(1,6),TASLSH(1)),
     .             (TTAB(1,7),TBS(1)),(TTAB(1,8),TCT(1))
      EQUIVALENCE  (HTAB(1,1),H1J(1)),(HTAB(1,2),H3L(1)),
     .             (HTAB(1,3),H4M(1)),(HTAB(1,4),H5N(1)),
     .             (HTAB(1,5),H9R(1)),(HTAB(1,6),HASLSH(1)),
     .             (HTAB(1,7),HBS(1)),(HTAB(1,8),HCT(1))
      LOGICAL      INIT,NOSWC, HGTTBL

C  TEMPERATURE CORRECTIONS (*10 K) FOR S-W RADIATION EFFECTS
C  ---------------------------------------------------------

C     NOAA / VIZ

      DATA T1J /
     $   0.,  3.,  6.,  8.,  9., 10., 10., 10.,  9.,  8.,  5.,
     $   0.,  4.,  7.,  9., 10., 11., 11., 11., 10.,  9.,  5.,
     $   0.,  5.,  8., 10., 12., 12., 13., 13., 11., 10.,  6.,
     $   0.,  7., 12., 14., 16., 16., 17., 16., 14., 11.,  7.,
     $   0.,  9., 16., 18., 20., 20., 20., 19., 16., 12.,  8.,
     $   0., 15., 22., 25., 26., 27., 26., 25., 22., 18., 12./

C      AN/AMT 4   NOAA/ VIZ MILITARY TYPE

      DATA T3L /
     $   0.,  2.,  4.,  5.,  6.,  7.,  8.,  8.,  6.,  2.,  0.,
     $   0.,  2.,  4.,  6.,  7.,  8.,  9.,  9.,  7.,  2.,  0.,
     $   0.,  2.,  4.,  7.,  9., 10., 11., 10.,  8.,  3.,  0.,
     $   0.,  3.,  6., 10., 12., 13., 14., 12., 10.,  4.,  0.,
     $   0.,  6.,  9., 12., 15., 16., 17., 15., 12.,  4.,  0.,
     $   0., 12., 15., 18., 20., 22., 22., 20., 16.,  6.,  0./

C     VAISALA INSIDE FINLAND (ALSO USED FOR VAISALA OUT OF FINLAND)

      DATA T4M /
     $   0.,  2.,  4.,  6.,  8., 10., 11., 11., 10.,  8.,  6.,
     $  -3.,  1.,  3.,  6.,  8., 10., 11., 12., 10.,  8.,  6.,
     $  -6.,  0.,  3.,  6.,  8., 10., 13., 12., 10.,  8.,  6.,
     $ -10., -3.,  2.,  4.,  8., 10., 13., 13., 11.,  9.,  7.,
     $ -14., -6.,  0.,  4.,  8., 10., 14., 13., 11.,  9.,  7.,
     $ -23., -6.,  0.,  5.,  9., 11., 14., 13., 12., 10.,  8./

C     FRENCH METOX (INACTIVE - CORRECTIONS NOT APPLIED)

      DATA T5N /
     $   0.,  0.,  8., 12., 16., 20., 23., 26., 24., 17.,  9.,
     $   0.,  0., 13., 20., 25., 30., 35., 38., 38., 31., 17.,
     $   0.,  0., 18., 27., 34., 41., 46., 51., 52., 46., 26.,
     $   0.,  0., 27., 42., 54., 64., 70., 76., 76., 65., 30.,
     $   0.,  0., 36., 57., 72., 82., 90., 96., 95., 83., 55.,
     $   0.,  0., 56., 83.,105.,122.,132.,140.,140.,130.,116./

C     JAPANESE

      DATA T9R /
     $   0.,  0., 12., 14., 14., 13., 11.,  9.,  7.,  0.,  0.,
     $   0.,  0., 18., 21., 20., 17., 14., 11.,  7.,  0.,  0.,
     $   0.,  0., 24., 28., 26., 22., 18., 13.,  6.,  0.,  0.,
     $   0.,  0., 30., 36., 33., 29., 24., 18., 11.,  0.,  0.,
     $   0.,  0., 33., 39., 36., 30., 23., 16.,  5.,  0.,  0.,
     $   0.,  0., 38., 47., 40., 30., 20.,  7., -8.,  0.,  0./

C     EAST GERMAN FREIBERG (INACTIVE - CORRECTIONS NOT APPLIED)

      DATA TASLSH /
     $   0.,  0.,  8., 11., 14., 18., 20., 23.,  0.,  0.,  0.,
     $   0.,  0., 12., 15., 18., 22., 24., 26.,  0.,  0.,  0.,
     $   0.,  0., 16., 19., 22., 26., 28., 30.,  0.,  0.,  0.,
     $   0.,  0., 24., 34., 42., 48., 52., 54.,  0.,  0.,  0.,
     $   0.,  0., 30., 45., 55., 61., 65., 67.,  0.,  0.,  0.,
     $   0.,  0., 58., 68., 76., 80., 81., 82.,  0.,  0.,  0./

C     UNITED KINGDOM KEW (WPT HAD THIS INSTRUMENT AS PAKISTANI FAN-TYPE)

      DATA TBS /
     $   0.,  0.,  6., 10., 10.,  6.,  6.,  6., -4.,-12.,  0.,
     $   0.,  0.,  6., 11., 10.,  6.,  8.,  8., -1.,-11.,  0.,
     $   0.,  0.,  6., 12., 10.,  6., 10., 11.,  2., -8.,  0.,
     $   0.,  0., 16., 20., 16., 10., 14., 14., -8.,-34.,  0.,
     $   0., 15., 26., 28., 30., 24., 24., 22.,  0.,-36.,  0.,
     $   0., 40., 52., 54., 50., 44., 44., 46., 28.,  7.,  0./

C     U.S.S.R. A-22

      DATA  TCT /
     $  -2.,  2.,  8.,  9., 10., 10., 10., 10.,  8.,  0.,  0.,
     $  -1.,  4., 10., 11., 13., 14., 15., 14., 12.,  0.,  0.,
     $  -1.,  6., 12., 14., 16., 18., 20., 19., 17.,  0.,  0.,
     $   2., 10., 14., 18., 20., 23., 25., 24., 22.,  0.,  0.,
     $   3., 14., 22., 25., 27., 29., 30., 28., 26.,  0.,  0.,
     $   9., 24., 32., 37., 40., 43., 44., 41., 39.,  0.,  0./

C  HEIGHT CORRECTIONS (M) AT 100 MB FOR S-W RADIATION EFFECTS
C  ----------------------------------------------------------

      DATA  H1J   /  0., 10.,20.,29.,35.,38.,39.,37., 30., 27.,25./
      DATA  H3L   / -9., -2., 8.,17.,23.,25.,26.,25., 18.,  0., 0./
      DATA  H4M   /  0.,  7.,17.,22.,27.,32.,36.,36., 34., 30.,27./
      DATA  H5N   /  0.,  0.,10.,22.,34.,44.,53.,60., 59., 48.,26./
      DATA  H9R   /  0.,  0.,38.,38.,38.,37.,36.,35., 34.,  0., 0./
      DATA  HASLSH/  0.,  0.,19.,29.,38.,47.,56.,65.,  0.,  0., 0./
      DATA  HBS   /-33.,-22.,-1.,11.,13., 5., 6.,10.,-12.,-47., 0./
      DATA  HCT   /-14.,  6.,22.,33.,38.,41.,42.,40., 38.,  0., 0./


      DATA INIT   /.TRUE./
      DATA PRES6  /100.,70.,50.,30.,20.,10./
      DATA ANGLE  /0.,0.,10.,20.,30.,40.,50.,60.,70.,80.,90./
      DATA CUTOFF /-5.54,-5.76,-6.02,-6.31,-6.55,-7.04/
      DATA ITYPTB /1,0,2,3,0,0,0,0,5,6,7,8,0,242*0/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

C  EACH TIME THROUGH, COMPUTE LOGS OF PRESSURE RATIOS
C  --------------------------------------------------

      DO I=1,5
         ALP(I) = 1.46 * ALOG(PRES6(I)/PRES6(I+1))
      ENDDO

C  GET TABLE INDEX FROM RADIOSONDE TYPE
C  ------------------------------------

      ITAB = 0
      IF(JTYPE.GT.0 .AND. JTYPE.LT.256) ITAB = ITYPTB(JTYPE)
      NOSWC = (JTYPE.LT.1.OR.JTYPE.GE.14.OR.ITAB.LE.0)

C  IF LWCORR=1 AND NOSWC THEN NO CORRECTIONS ARE MADE
C  --------------------------------------------------

      IF(LWCORR.LE.1.AND.NOSWC) THEN
         IRET = -1
         if(jtype.eq.20000)  iret = -99
         RETURN
      ENDIF

C  GET SOLAR ELEVATION ANGLE ON ALL VALID MANDATORY LEVELS
C  -------------------------------------------------------

      CALL SOELAN(SOLAR)

C  APPLY SHORT- (AND POSS. LONG-) WAVE CORRECTIONS (LEVELS 11 TO 16)
C  -----------------------------------------------------------------

      IF(NOSWC) THEN
         L = 6
      ELSE
         L = 1
      ENDIF

      DO I=L,6
         ANGLE(1) = CUTOFF(I)
         DTS(I)   = 0.
         DHS(I)   = 0.
         DTL      = 0.
         DHL      = 0.

         IF(.NOT.NOSWC .AND. SOLAR(10+I).GE.CUTOFF(I))  THEN
            DO K=2,10
               IF(SOLAR(10+I).LT.ANGLE(K))  GO TO 40
            ENDDO
            K = 11
40          KK = K + (11 * (I - 1))
            DSUN = (SOLAR(10+I) - ANGLE(K-1))/(ANGLE(K) - ANGLE(K-1))

C  COMPUTE S-W TEMP CORRECTIONS FOR ALL LEVELS BETWEEN 100 & 10 MB
C  ---------------------------------------------------------------

            DTS(I) =DSUN*(TTAB(KK,ITAB)-TTAB(KK-1,ITAB))+TTAB(KK-1,ITAB)

C  COMPUTE S-W HEIGHT CORRECTIONS AT 100 MB & INTERP. TO HIGHER LEVELS
C  -------------------------------------------------------------------

            IF(I.EQ.1)  THEN
               DHS(1) =DSUN*(HTAB(K,ITAB)-HTAB(K-1,ITAB))+HTAB(K-1,ITAB)
            ELSE
               DHS(I) = DHS(I-1) + (ALP(I-1) * (DTS(I) + 10.*DTS(I-1)))
            ENDIF
         ENDIF

C  CORRECT DTS BY A FACTOR OF 10
C  -----------------------------

         DTS(I) = .1*DTS(I)
         IF(I.EQ.6 .AND. LWCORR.GT.0 .AND. TMP(16).LT.BMISS)  THEN

C  FOR LWCORR.GT.0, COMPUTE L-W TEMP & HGHT CORRECTIONS (ONLY AT 10 MB)
C  --------------------------------------------------------------------

            DTL = .0625*(TMP(16)-DTS(6)) + 5.09
            DHL = (TMP(16)-DTS(6)) + 81.4
         ENDIF

C  SAVE AND APPLY THE CORRECTIONS
C  ------------------------------

         DHT(10+I) = DHL - DHS(I)
         DTP(10+I) = DTL - DTS(I)

         IF(HGT(10+I).LT.BMISS)  HGT(10+I) = HGT(10+I) + DHT(10+I)
         IF(TMP(10+I).LT.BMISS)  TMP(10+I) = TMP(10+I) + DTP(10+I)

      ENDDO

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RADT2       APPLIES NEWER SET OF RADIATION CORRECTONS
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2007-09-14
C
C ABSTRACT: THIS SUBROUTINE APPLIES RADIATION CORRECTIONS TO THE
C   OBSERVED RADIOSONDE HEIGHTS AND TEMPERATURES ON THE BOTTOM 16
C   MANDATORY PRESSURE LEVELS.  DATA ARE PASSED THROUGH COMMON
C   /RADCOM/.  THE CORRECTION OF RAOBS FOR SHORT-WAVE RADIATIVE EFFECTS
C   IS ACCOMPLISHED BY READING IN TABLES OF NUMBERS WHICH ARE DEPENDENT
C   ON THE RADIOSONDE TYPE.  THE CORRECTION OF RAOBS FOR LONG-WAVE
C   RADIATION (AT 10 MB ONLY) IS BASED ON THE 4'TH POWER OF THE 10 MB
C   TEMPERATURE.  CALLED FOR ONE REPORT AT A TIME.
C
C PROGRAM HISTORY LOG:
C   UNKNOWN   G. D. DIMEGO
C 1988-03-10  D. A. KEYSER  -- PROCESSING OF 'LWCALL' SWITCH
C 1988-11-04  D. A. KEYSER -- ADDED CORRECTIONS FOR NEW NOAA VIZ-A,
C     VIZ-B, AND SPACE DATA CORP.
C 1989-03-10  D. A. KEYSER -- EXPANDED TABLES FOR SINGLE NIGHTTIME
C     CORR. & CORR. BELOW 700 MB, ADDED TABLE FOR SDC-89 SONDE BASED
C     ON VIZ CORR. + (VIZ - SDC) BIAS
C 1990-12-05  D. A. KEYSER -- CONVERTED TO VS FORTRAN(77)& RESTRUCTURED
C 1992-03-18  D. A. KEYSER -- ADDED CORRECTIONS AT 925 MB TO TABLES
C 1994-02-27  J. WOOLLEN   -- CONVERTED FOR USE WITH BUFR
C 2002-07-22  D. A. KEYSER -- CALLS NEW ROUTINE SOELAN TO CALCULATE SUN
C     ANGLE ON VALID MANDATORY LEVELS RATHER THAN DOING CALCULATION IN
C     THIS SUBROUTINE
C 2007-09-14  D. KEYSER    -- MINOR CODE CORRECTIONS SUGGESTED BY PAT
C     PAULEY (CORRECTS PROBLEMS THAT COULD OCCUR ON SOME REMOTE
C     MACHINES)
C
C USAGE:    CALL RADT2(NOSWC,IRET)
C   OUTPUT ARGUMENT LIST:
C     NOSWC    - LOGICAL INDICATING NO SHORT-WAVE CORRECTION APPLIED
C     IRET     - RETURN CODE (0=CORRECTIONS, -1=NO CORRECTIONS,
C              - -99=??????)
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 68  - RADCOR INFORMATION FILE
C
C REMARKS: THESE CORRECTIONS USE THE ORIGINAL NMC OFFICE NOTE 29
C   RADIOSONDE TYPE CODES OBTAINED FROM THE NMC UPPER-AIR DICTIONARY.
C   CALLED BY SUBROUTINE "RADEVN".
C
C
C   KEY FOR RADIOSONDE TYPES USED HERE:
C
C JTYPE(ON29)  CODE NAME (DICTIONARY)  DESCRIPTION
C
C     1           T1J00Z         U.S. NOAA / VIZ-A - 1988 -- 00Z
C      -- OR --   T1J12Z         U.S. NOAA / VIZ-A - 1988 -- 12Z
C     2    ** NO CORRECTION **   RESERVED
C     3           T3L            U.S. AN/AMT 4
C     4           T4M            FINNISH VAISALA (IN FINLAND)
C     5           T5N            FRENCH MESURAL (IN FRANCE)
C     6    ** NO CORRECTION **   PORTUGAL (CANADIAN MODEL IV) (INACTIVE)
C     7           T7P            WEST GERMAN GRAW
C     8    ** NO CORRECTION **   RESERVED
C     9           T9R            JAPANESE "CODE SENDING"
C    10           TASLSH         EAST GERMAN FREIBERG (INACTIVE)
C    11           TBS            UNITED KINGDOM KEW
C    12           T2200Z         U.S.S.R A-22 -- 00Z
C      -- OR --   T2212Z         U.S.S.R A-22 -- 12Z
C    13    ** NO CORRECTION **   RESERVED
C    14           TVSL2          FINNISH VAISALA (IN/OUT FINLAND)
C    15           TFMR2          FRENCH MESURAL (OUT OF FRANCE)
C    16    ** NO CORRECTION **   AUSTRALIAN PHILLIPS
C    17           TAUS           AUSTRALIAN "DIAMOND HINMAN" (INACTIVE)
C    18           TCSG           CANADIAN "SANGAMO"
C    19           TCHI           CHINESE
C    20           TRK00Z         U.S.S.R. RKZ -- 00Z
C      -- OR --   TRK12Z         U.S.S.R. RKZ -- 12Z
C    21           TAV00Z         U.S.S.R. UNKNOWN (AVG. A-22&RKZ) -- 00Z
C      -- OR --   TAV12Z         U.S.S.R. UNKNOWN (AVG. A-22&RKZ) -- 12Z
C    22    ** NO CORRECTION **   INDIAN MET. SERVICE
C    23    ** NO CORRECTION **   AUSTRIAN ELIN
C    24    ** NO CORRECTION **   KOREAN JINYANG / VIZ
C    25    ** NO CORRECTION **   SWISS METEOLABOR
C    26    ** NO CORRECTION **   CZECH VINOHRADY
C    27    ** NO CORRECTION **   U.S. MSS (SPACE DATA CORP.)
C    28           T1J00Z         U.S. NOAA / VIZ-B - 1988 -- 00Z
C      -- OR --   T1J12Z         U.S. NOAA / VIZ-B - 1988 -- 12Z
C    29           TSD00Z         U.S. NOAA / SPACE DATA CORP. - 1989
C      -- OR --   TSD12Z         U.S. NOAA / SPACE DATA CORP. - 1989
C    30 \
C   THRU > ** NO CORRECTION **   RESERVED
C    97 /
C
C
C   KEY FOR LEVELS IN DATA (CORRECTION) TABLES:
C
C     1 - 1000 MB
C     2 -- 925 MB
C     3 -- 850 MB        T --  20 MB
C     4 -- 700 MB        L --  10 MB
C     5 -- 500 MB        A -- 400 MB (INTERP. BETWEEN 500 & 300 MB)
C     6 -- 300 MB        B -- 250 MB (INTERP. BETWEEN 300 & 200 MB)
C     7 -- 200 MB        C -- 150 MB (INTERP. BETWEEN 200 & 100 MB)
C     8 -- 100 MB        D --  70 MB (INTERP. BETWEEN 100 &  50 MB)
C     9 --  50 MB        E --  30 MB (INTERP. BETWEEN  50 &  10 MB)
C     $ --  30 MB        F --  20 MB (INTERP. BETWEEN  50 &  10 MB)
C
C
C   THE RADIATION CORRECTION TABLES ARE OBTAINED FROM NOAA TECH. MEMO.
C   NWS NMC 63, BY MCINTURFF ET. AL. (1972).
C
C
C   THE CUTOFF ANGLE AT EACH LEVEL IS CALCULATED USING THE RELATION:
C              CUTOFF ANGLE = -1.76459 * (Z**.40795) ,
C   WHERE Z IS A REFERENCE HEIGHT FOR THE LEVEL (IN KILOMETERS).
C   (SEE NMC OFFICE NOTE 306, TABLE 4 FOR LIST OF REFERENCE HEIGHTS.)
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE RADT2(NOSWC,IRET)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /RADCOM/ HGT(16),TMP(16),DHT(16),DTP(16),JTYPE
      COMMON /SWITCH/ LWCORR,LEVRAD,IRCTBL,HGTTBL,BAL_DRIFT
      COMMON /PMAND / PRES(16),KMIN,KMAX,INM(16)
      COMMON /STN/     IS
      COMMON /DATEX/   IDATE(5), CDATE
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      REAL        T1J00Z(11,16),T1J12Z(11,16),T2200Z(11,16),
     .            T2212Z(11,16),TRK00Z(11,16),TRK12Z(11,16),
     .            TAV00Z(11,16),TAV12Z(11,16),T3L(11,16),T4M(11,16),
     .            T5N(11,16),T7P(11,16),T9R(11,16),TASLSH(11,16),
     .            TBS(11,16),TVSL2(11,16),TFMR2(11,16),TAUS(11,16),
     .            TCSG(11,16),TCHI(11,16),TSD00Z(11,16),TSD12Z(11,16),
     .            TTAB(11,16,22),CUTOFF(16),SOLAR(16),ANGLE(11),
     .            ALP(16),DTS(16),DHS(16)
      INTEGER     ITYPTB(255)
      EQUIVALENCE (TTAB(1,1,1), T1J00Z(1,1)),(TTAB(1,1,2), T1J12Z(1,1)),
     .            (TTAB(1,1,3), T3L(1,1)),   (TTAB(1,1,4), T4M(1,1))   ,
     .            (TTAB(1,1,5), T5N(1,1)),   (TTAB(1,1,6), T7P(1,1))   ,
     .            (TTAB(1,1,7), T9R(1,1)),   (TTAB(1,1,8), TASLSH(1,1)),
     .            (TTAB(1,1,9), TBS(1,1)),   (TTAB(1,1,10),T2200Z(1,1)),
     .            (TTAB(1,1,11),T2212Z(1,1)),(TTAB(1,1,12),TVSL2(1,1)) ,
     .            (TTAB(1,1,13),TFMR2(1,1)), (TTAB(1,1,14),TAUS(1,1))  ,
     .            (TTAB(1,1,15),TCSG(1,1)),  (TTAB(1,1,16),TCHI(1,1))  ,
     .            (TTAB(1,1,17),TRK00Z(1,1)),(TTAB(1,1,18),TRK12Z(1,1)),
     .            (TTAB(1,1,19),TAV00Z(1,1)),(TTAB(1,1,20),TAV12Z(1,1)),
     .            (TTAB(1,1,21),TSD00Z(1,1)),(TTAB(1,1,22),TSD12Z(1,1))
      CHARACTER*8 SID
      CHARACTER*10     CDATE
      LOGICAL     NOSWC,SWC, HGTTBL

      DATA  ANGLE  /-90.,-5.,5.,15.,25.,35.,45.,55.,65.,75.,85./
      DATA  CUTOFF/-0.73,-1.58,-2.06,-2.77,-3.56,-3.95,-4.36,-4.58,
     .             -4.83,-5.12,-5.49,-5.79,-6.06,-6.43,-6.72,-7.17/

C  ITYPTB ASSIGNS PROPER TEMPERATURE CORRECTION TABLE TO INST. TYPES
C  -----------------------------------------------------------------

C  ===> JTYPE:       1    2    3    4    5    6    7    8    9   10

      DATA  ITYPTB / 1 ,  0 ,  3 ,  4 ,  5 ,  0 ,  6 ,  0 ,  7 ,  8 ,

C  ===> JTYPE:      11   12   13   14   15   16   17   18   19   20

     .               9 , 10 ,  0 , 12 , 13 ,  0 , 14 , 15 , 16 , 17 ,

C  ===> JTYPE:      21   22   23   24   25   26   27   28   29  30-256

     .              19 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  1 , 21 ,226*0/

C  TEMPERATURE CORRECTIONS (*10 K) FOR S-W RADIATION EFFECTS
C  ---------------------------------------------------------


C    NOAA / VIZ - A 1988, TECH MEMO TABLE FOR 00Z AFTERNOON DAYLIGHT
C               =====>  JTYPE =  1  --   ITAB =  1  <=====
C                                -- OR --
C    NOAA / VIZ - B 1988, TECH MEMO TABLE FOR 00Z AFTERNOON DAYLIGHT
C               =====>  JTYPE = 28  --   ITAB =  1  <=====

      DATA  T1J00Z /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0.,  2.,  3.,  5.,  6.,  6.,  3.,  4.,  5.,  5.,  4.,
     5    0.,  2.,  3.,  4.,  5.,  5.,  4.,  4.,  5.,  5.,  6.,
     A    0.,  2.,  3.,  5.,  5.,  5.,  4.,  4.,  5.,  5.,  6.,
     6    0.,  2.,  4.,  6.,  6.,  6.,  5.,  5.,  5.,  5.,  6.,
     B    0.,  2.,  4.,  6.,  6.,  6.,  5.,  6.,  6.,  6.,  7.,
     7    0.,  1.,  5.,  6.,  7.,  7.,  6.,  7.,  7.,  7.,  8.,
     C    0.,  1.,  6.,  7.,  8.,  8.,  8.,  8.,  8.,  9.,  9.,
     8    0.,  2.,  7.,  9., 10., 10., 10., 10., 10., 12., 10.,
     D    0.,  3.,  9., 11., 12., 12., 11., 10., 11., 11., 10.,
     9    0.,  5., 11., 13., 14., 14., 12., 11., 13., 10.,  9.,
     $    0.,  9., 16., 18., 21., 19., 14., 14., 10., 16., 12.,
     T    0., 12., 19., 23., 26., 24., 18., 15., 13., 13., 22.,
     L    0., 19., 24., 29., 32., 26., 28., 23., 25., 11., 32./


C    NOAA / VIZ - A 1988, TECH MEMO TABLE FOR 12Z MORNING DAYLIGHT
C               =====>  JTYPE =  1  --   ITAB =  2  <=====
C                                -- OR --
C    NOAA / VIZ - B 1988, TECH MEMO TABLE FOR 12Z MORNING DAYLIGHT
C               =====>  JTYPE = 28  --   ITAB =  2  <=====

      DATA  T1J12Z /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0., -2., -1.,  2.,  4.,  5.,  4.,  8.,  9.,  0.,  0.,
     5    0., -2., -1.,  2.,  4.,  4.,  5., 10.,  9.,  0.,  0.,
     A    0., -2., -1.,  2.,  3.,  4.,  6., 11., 10.,  0.,  0.,
     6    0., -2.,  0.,  2.,  2.,  4.,  7., 12., 11.,  0.,  0.,
     B    0., -1.,  1.,  3.,  3.,  6., 11., 11., 10.,  0.,  0.,
     7    0.,  0.,  2.,  5.,  4.,  8., 15., 10.,  9.,  0.,  0.,
     C    0.,  0.,  3.,  6.,  5.,  8., 14., 13.,  9.,  0.,  0.,
     8    0.,  1.,  5.,  7.,  7.,  8., 12., 16., 10.,  0.,  0.,
     D    0.,  1.,  5.,  7.,  8.,  9., 13., 13., 12.,  0.,  0.,
     9    0.,  0.,  5.,  7.,  9., 11., 14., 10., 15.,  0.,  0.,
     $    0., -1.,  6., 10., 11., 14., 13., 22., -4.,  0.,  0.,
     T    0., -2.,  8., 11., 11., 15., 12., 13.,  0.,  0.,  0.,
     L    0.,  0., 13., 13., 11., 13., 19., 35.,  0.,  0.,  0./


C                   AN/AMT 4 NOAA / VIZ MILITARY TYPE
C               =====>  JTYPE =  3  --   ITAB =  3  <=====

      DATA  T3L /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___ ___      ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0., -1.,  0.,  0.,  2.,  3.,  3.,  3.,  2.,  1.,  0.,
     5    0., -5., -1.,  0.,  2.,  4.,  3.,  4.,  4.,  3.,  0.,
     A    0., -3.,  0.,  1.,  2.,  4.,  3.,  4.,  4.,  3.,  0.,
     6    0., -2.,  1.,  4.,  4.,  5.,  4.,  5.,  6.,  5.,  0.,
     B    0., -1.,  2.,  4.,  4.,  5.,  4.,  5.,  6.,  5.,  0.,
     7    0.,  0.,  5.,  6.,  5.,  5.,  4.,  6.,  7.,  6.,  0.,
     C    0.,  0.,  4.,  6.,  6.,  6.,  6.,  7.,  7.,  7.,  0.,
     8    0., -1.,  4.,  8.,  8.,  9.,  9., 10.,  8.,  9.,  0.,
     D    0.,  0.,  6.,  8.,  9., 10., 10., 12., 11.,  0.,  0.,
     9    0.,  2.,  8.,  8., 11., 11., 11., 14., 15.,  0.,  0.,
     $    0.,  8., 12., 17., 14., 18., 15., 17., 18.,  0.,  0.,
     T    0.,  0., 17., 18., 18., 16., 17., 17., 16., 12.,  0.,
     L    0., 15., 14., 29., 17., 20., 25., 27., 23., 19.,  0./


C     VAISALA INSIDE FINLAND (NEW VALUES INTERPOLATED FOR LEVELS E&F)
C               =====>  JTYPE =  4  --   ITAB =  4  <=====

      DATA  T4M /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0., -1.,  1.,  1.,  1.,  1.,  1.,  0.,  0.,  0.,  0.,
     5    0., -1.,  2.,  4., -2.,  4.,  0.,  0.,  0.,  0.,  0.,
     A    0., -1.,  1.,  3.,  0.,  2.,  0.,  0.,  0.,  0.,  0.,
     6    0., -3.,  0.,  2.,  1.,  1.,  0.,  0.,  0.,  0.,  0.,
     B    0., -2.,  0.,  0.,  0.,  1.,  0.,  0.,  0.,  0.,  0.,
     7    0., -1.,  1., -2., -2.,  1.,  0.,  0.,  0.,  0.,  0.,
     C    0.,  0.,  0., -1., -1.,  0.,  0.,  0.,  0.,  0.,  0.,
     8    0.,  0., -1.,  0., -1.,  0.,  0.,  0.,  0.,  0.,  0.,
     D    0., -2., -1.,  0.,  0.,  2.,  0.,  0.,  0.,  0.,  0.,
     9    0., -4., -2.,  0.,  1.,  4.,  0.,  0.,  0.,  0.,  0.,
     E    0., -2., -2., -1.,  2.,  4.,  0.,  0.,  0.,  0.,  0.,
     F    0.,  1., -1., -3.,  4.,  3.,  0.,  0.,  0.,  0.,  0.,
     T    0.,  3.,  0., -4.,  5.,  2.,  0.,  0.,  0.,  0.,  0./


C                 MESURAL INSIDE METROPOLITAN FRANCE
C               =====>  JTYPE =  5  --   ITAB =  5  <=====

      DATA T5N  /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0.,  0.,  0.,  3.,  2.,  3.,  2.,  2., -2.,  0.,  0.,
     5    0.,  0.,  0.,  4.,  2.,  3.,  6.,  3.,  2.,  0.,  0.,
     A    0.,  0.,  0.,  3.,  2.,  3.,  5.,  3.,  1.,  0.,  0.,
     6    0.,  0.,  0.,  3.,  2.,  4.,  5.,  3.,  1.,  0.,  0.,
     B    0.,  0.,  0.,  1.,  3.,  4.,  2.,  3.,  1.,  0.,  0.,
     7    0.,  0.,  0., -1.,  5.,  5., -1.,  5.,  2.,  0.,  0.,
     C    0.,  0.,  0.,  1.,  5.,  5.,  0.,  5.,  3.,  0.,  0.,
     8    0.,  0.,  0.,  4.,  6.,  6.,  3.,  6.,  5.,  0.,  0.,
     D    0.,  0.,  0.,  6.,  7.,  7.,  5.,  7.,  8.,  0.,  0.,
     9    0.,  0.,  0.,  9.,  8.,  8.,  8.,  8., 12.,  0.,  0.,
     $    0.,  0.,  0., 11., 12., 10., 13., 11.,  9.,  0.,  0.,
     T    0.,  0.,  0., 12., 13., 18., 12., 16., 18.,  0.,  0.,
     L    0.,  0.,  0., 19., 22., 26., 32., 11.,  9.,  0.,  0./


C                           WEST GERMAN GRAW
C               =====>  JTYPE =  7  --   ITAB =  6  <=====

      DATA  T7P /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0.,  0.,  0.,  1., -1.,  0.,  1.,  0., -2.,  0.,  0.,
     5    0.,  0.,  0.,  0., -1.,  0.,  0.,  0.,  2.,  0.,  0.,
     A    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,  0.,
     6    0.,  0.,  0.,  0.,  1.,  0.,  0.,  0.,  0.,  0.,  0.,
     B    0.,  0.,  0.,  0.,  1.,  1.,  1.,  0.,  0.,  0.,  0.,
     7    0.,  0.,  0.,  1.,  2.,  3.,  3.,  1.,  2.,  0.,  0.,
     C    0.,  0.,  0.,  2.,  3.,  3.,  3.,  1.,  3.,  0.,  0.,
     8    0.,  0.,  0.,  4.,  5.,  3.,  4.,  3.,  6.,  0.,  0.,
     D    0.,  0.,  0.,  5.,  5.,  4.,  2.,  3.,  5.,  0.,  0.,
     9    0.,  0.,  0.,  6.,  5.,  6.,  2.,  4.,  5.,  0.,  0.,
     $    0.,  0.,  9.,  6.,  6.,  8.,  1.,  0.,  0.,  0.,  0.,
     T    0.,  0., 10.,  5.,  6.,  6., -1.,  4.,  0.,  0.,  0.,
     L    0.,  0.,  0.,  7.,  3.,  7., -2., -3.,  0.,  0.,  0./


C                                JAPANESE
C               =====>  JTYPE =  9  --   ITAB =  7  <=====

      DATA  T9R /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0.,  0.,  3.,  0.,  1.,  2.,  2.,  2.,  3.,  0.,  0.,
     5    0.,  0.,  0.,  2.,  3.,  3.,  4.,  5.,  7.,  0.,  0.,
     A    0.,  0.,  0.,  3.,  3.,  3.,  4.,  5.,  6.,  0.,  0.,
     6    0.,  0.,  0.,  6.,  4.,  3.,  4.,  5.,  6.,  0.,  0.,
     B    0.,  0.,  0.,  5.,  4.,  4.,  4.,  4.,  5.,  0.,  0.,
     7    0.,  0.,  0.,  4.,  6.,  6.,  4.,  4.,  5.,  0.,  0.,
     C    0.,  0.,  0.,  5.,  6.,  6.,  5.,  5.,  4.,  0.,  0.,
     8    0.,  0.,  0.,  7.,  8.,  7.,  8.,  8.,  4.,  0.,  0.,
     D    0.,  0.,  0., 10., 10.,  8.,  8.,  8.,  2.,  0.,  0.,
     9    0.,  0.,  0., 13., 12., 10.,  8.,  9.,  2.,  1.,  0.,
     $    0.,  0.,  0., 26., 19., 16., 12., 11., 10., -5.,  0.,
     T    0.,  0.,  0.,  0., 30., 28., 19., 13., 14.,-10.,  0.,
     L    0.,  0.,  0.,  0., 41., 35., 16., 18., 14.,  0.,  0./


C         EAST GERMAN FREIBERG (INACTIVE - WAS NOT IN TECH MEMO)
C         SOURCE UNKNOWN POSSIBLY TAKEN FROM ORIGINAL RADNCORR)
C               =====>  JTYPE = 10  --   ITAB =  8  <=====

      DATA  TASLSH /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     5    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     A    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     6    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     B    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     7    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     C    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     8    0.,  0.,  4.,  9., 12., 16., 19., 21., 11.,  0.,  0.,
     D    0.,  0.,  6., 13., 16., 20., 23., 25., 13.,  0.,  0.,
     9    0.,  0.,  8., 17., 20., 24., 27., 29., 15.,  0.,  0.,
     $    0.,  0., 12., 29., 38., 45., 50., 53., 27.,  0.,  0.,
     T    0.,  0., 15., 37., 50., 58., 63., 66., 33.,  0.,  0.,
     L    0.,  0., 29., 63., 72., 78., 80., 81., 41.,  0.,  0./


C   UNITED KINGDOM KEW (WPT HAD THIS INSTRUMENT AS PAKISTANI FAN-TYPE)
C               =====>  JTYPE = 11  --   ITAB =  9  <=====

      DATA  TBS /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0.,  0., -3., -1., -1.,  0., -1., -1., -1.,  0.,  0.,
     5    0.,  0., -2.,  0.,  0.,  0.,  0.,  0., -1., -6.,  0.,
     A    0.,  0., -2.,  0.,  0.,  0.,  0.,  0.,  0., -4.,  0.,
     6    0.,  0., -2.,  0.,  0.,  2.,  1.,  1.,  0., -3.,  0.,
     B    0.,  0., -1.,  0.,  0.,  1.,  0.,  0.,  0.,  0.,  0.,
     7    0.,  0., -1., -1.,  0.,  0.,  0., -2.,  0.,  0.,  0.,
     C    0.,  0.,  0.,  0.,  1.,  0.,  0., -2.,  0.,  0.,  0.,
     8    0.,  0., -2.,  1.,  3.,  2.,  0., -3., -1.,  0.,  0.,
     D    0.,  0., -6., -1.,  1.,  0., -1., -1.,  1.,  0.,  0.,
     9    0.,  0.,-10., -3.,  1., -1., -3.,  0.,  3.,  0.,  0.,
     $    0.,  0., -5., -1.,  8.,  3.,  1.,  4., -1.,  0.,  0.,
     T    0.,  0.,  3.,  4., 16., 15., 11.,  8.,  0.,  0.,  0.,
     L    0.,  0., 32., 33., 43., 33., 28., 14.,  0.,  0.,  0./


C          USSR A-22 TECH MEMO TABLE FOR 00Z MORNING DAYLIGHT
C               =====>  JTYPE = 12  --   ITAB = 10  <=====

      DATA  T2200Z /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0.,  0.,  1.,  0., -1.,  0., -1., -2.,  0.,  0.,  0.,
     5    0., -1.,  0.,  0., -1.,  0.,  0., -2.,  0.,  0.,  0.,
     A    0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,  0.,  0.,  0.,
     6    0.,  0.,  0.,  0.,  2.,  1.,  2.,  1.,  0.,  0.,  0.,
     B    0.,  0.,  1.,  0.,  2.,  1.,  1.,  0.,  0.,  0.,  0.,
     7    0.,  0.,  2.,  1.,  2.,  2., -1., -2.,  0.,  0.,  0.,
     C    0.,  0.,  2.,  1.,  2.,  3.,  0.,  0.,  0.,  0.,  0.,
     8    0.,  0.,  2.,  2.,  3.,  5.,  1.,  3.,  0.,  0.,  0.,
     D    0.,  0.,  2.,  3.,  4.,  6.,  3.,  3.,  0.,  0.,  0.,
     9    0., -1.,  3.,  5.,  5.,  8.,  5.,  3.,  0.,  0.,  0.,
     $    0., -2.,  4.,  6.,  5.,  7.,  6.,  4.,  3.,  0.,  0.,
     T    0., -1.,  6.,  9.,  9., 11., 12.,  9., -6.,  0.,  0.,
     L    0.,  1.,  9., 19., 28.,  5., 23., 24.,  0.,  0.,  0./


C          USSR A-22 TECH MEMO TABLE FOR 12Z AFTERNOON DAYLIGHT
C               =====>  JTYPE = 12  --   ITAB = 11  <=====

      DATA  T2212Z /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0.,  1.,  3.,  3.,  5.,  2.,  2.,  1.,  1.,  0.,  0.,
     5    0.,  2.,  2.,  3.,  3.,  2.,  2.,  3.,  0.,  0.,  0.,
     A    0.,  2.,  2.,  3.,  2.,  3.,  3.,  3.,  2.,  0.,  0.,
     6    0.,  1.,  3.,  2.,  1.,  4.,  4.,  2.,  4.,  0.,  0.,
     B    0.,  1.,  3.,  2.,  2.,  3.,  2.,  0.,  2.,  0.,  0.,
     7    0.,  1.,  3.,  2.,  3.,  1.,  0., -2.,  0.,  0.,  0.,
     C    0.,  1.,  4.,  3.,  4.,  3.,  1., -1.,  0.,  0.,  0.,
     8    0.,  2.,  5.,  4.,  5.,  5.,  3.,  0.,  0.,  0.,  0.,
     D    0.,  2.,  5.,  5.,  6.,  7.,  4.,  2.,  0.,  0.,  0.,
     9    0.,  3.,  6.,  7.,  7.,  9.,  6.,  4.,  0.,  0.,  0.,
     $    0.,  2.,  3.,  8.,  9.,  9.,  5.,  8.,  0.,  0.,  0.,
     T    0.,  3.,  5., 10.,  1., 12., 10.,  2.,  0.,  0.,  0.,
     L    0., -2.,  2., 25.,  3., 18.,  2.,  0.,  0.,  0.,  0./


C                   VAISALA INSIDE & OUTSIDE OF FINLAND
C               =====>  JTYPE = 14  --   ITAB = 12  <=====

      DATA  TVSL2 /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0., -2.,  1.,  1.,  3.,  1.,  2.,  3.,  5.,  3.,  2.,
     5    0.,  2.,  1.,  0.,  1.,  2.,  2.,  3.,  4.,  6.,  0.,
     A    0.,  0.,  1.,  0.,  1.,  2.,  1.,  3.,  4.,  6.,  0.,
     6    0., -1.,  1.,  1.,  2.,  2.,  3.,  3.,  5.,  6.,  0.,
     B    0.,  0.,  1.,  1.,  1.,  2.,  3.,  3.,  5.,  6.,  0.,
     7    0.,  0.,  1.,  1.,  1.,  2.,  3.,  5.,  6.,  6.,  0.,
     C    0.,  0.,  1.,  1.,  2.,  3.,  3.,  6.,  8.,  8.,  0.,
     8    0.,  1.,  3.,  3.,  4.,  6.,  5.,  8., 13., 13.,  0.,
     D    0.,  1.,  5.,  6.,  7.,  9.,  8.,  9., 10.,  0.,  0.,
     9    0.,  1.,  8., 10., 10., 12., 12., 10.,  8.,  0.,  0.,
     $    0.,  1., 20., 24., 22., 20., 15.,  4.,  1.,  0.,  0.,
     T    0.,  1., 19., 24., 20., 20., 16.,  4.,  1.,  0.,  0.,
     L    0., 26., 31., 40., 18., 33., 14., 25.,  0.,  0.,  0./


C                     MESURAL USED OUTSIDE OF FRANCE
C               =====>  JTYPE = 15  --   ITAB = 13  <=====

      DATA  TFMR2 /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0.,  0.,  0.,  0.,  8.,  6.,  8.,  5.,  2.,  0.,  0.,
     5    0.,  0.,  0.,  0.,  5.,  8.,  9.,  7.,  5.,  4.,  0.,
     A    0.,  0.,  0.,  0.,  8.,  8., 11.,  8.,  6.,  4.,  0.,
     6    0.,  0.,  0.,  0., 13.,  9., 14., 11.,  8.,  6.,  0.,
     B    0.,  0.,  0.,  0., 17., 10., 32., 28., 38., 20.,  0.,
     7    0.,  0.,  0.,  0., 23., 22., 55., 49., 76., 38.,  0.,
     C    0.,  0.,  0.,  0., 28., 23., 42., 39., 54., 27.,  0.,
     8    0.,  0.,  0.,  0., 37., 25., 24., 27., 23., 12.,  0.,
     D    0.,  0.,  0.,  0., 49., 33., 33., 15., 30., 18.,  0.,
     9    0.,  0.,  0.,  0., 62., 42., 43.,  4., 37., 24.,  0.,
     $    0.,  0.,  0.,  0., 76., 66., 51., 51., 53.,  0.,  0.,
     T    0.,  0.,  0.,  0.,114., 88., 85., 93., 50.,  0.,  0.,
     L    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0./


C                   AUSTRALIAN DIAMOND HINMAN (INACTIVE)
C               =====>  JTYPE = 17  --   ITAB = 14  <=====

      DATA  TAUS /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0.,  0.,  3.,  0.,  3.,  2.,  1.,  3.,  7.,  3.,  4.,
     5    0.,  0.,  0.,  2.,  1.,  2.,  2.,  4.,  5.,  4., -3.,
     A    0.,  0.,  0.,  2.,  1.,  2.,  2.,  3.,  3.,  2.,  0.,
     6    0.,  0.,  0.,  2.,  2.,  2.,  3.,  2.,  2.,  1.,  0.,
     B    0.,  0.,  0.,  2.,  2.,  3.,  4.,  3.,  3.,  2.,  0.,
     7    0.,  0.,  2.,  4.,  4.,  5.,  6.,  5.,  6.,  5.,  9.,
     C    0.,  0.,  0.,  4.,  5.,  5.,  7.,  7.,  8.,  7.,  8.,
     8    0.,  0.,  0.,  6.,  8.,  7.,  9., 10., 11., 10.,  8.,
     D    0.,  0.,  0.,  7.,  9.,  9., 11., 10., 12., 12.,  0.,
     9    0.,  0.,  0.,  9., 10., 11., 13., 11., 14., 14.,  0.,
     $    0.,  0.,  0.,  8., 17., 17., 17., 18., 14., 24.,  0.,
     T    0.,  0.,  0., 18., 17., 16., 28., 22., 17., 14.,  0.,
     L    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0./


C                           SANGAMO CANADIAN
C               =====>  JTYPE = 18  --   ITAB = 15  <=====

      DATA  TCSG /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0.,  0.,  1.,  3.,  4.,  3.,  2., -1., -3.,  0.,  0.,
     5    0.,  0.,  0.,  4.,  5.,  5.,  1.,  3.,  1.,  0.,  0.,
     A    0.,  0.,  0.,  4.,  5.,  5.,  2.,  3.,  3.,  0.,  0.,
     6    0.,  0.,  2.,  4.,  5.,  7.,  5.,  5.,  6., -1.,  0.,
     B    0.,  0.,  2.,  4.,  5.,  6.,  3.,  5.,  7.,  3.,  0.,
     7    0.,  0.,  4.,  4.,  5.,  6.,  2.,  6.,  9., 10.,  0.,
     C    0.,  0.,  4.,  5.,  5.,  7.,  4.,  8.,  9.,  8.,  0.,
     8    0.,  1.,  6.,  7.,  7.,  9.,  8., 13., 10.,  8.,  0.,
     D    0.,  1.,  7.,  9.,  8., 10.,  9.,  9.,  9., 12.,  0.,
     9    0.,  1.,  8., 11.,  9., 12., 11.,  7.,  9., 17.,  0.,
     $    0.,  4., 11., 13., 14., 12., 10.,  8., 13., 16.,  0.,
     T    0.,  5., 14., 14., 14., 13., 10., 14., 14., 12.,  0.,
     L    0., 15., 19., 13., 12., 22.,  4., 17., 10., 13.,  0./


C                                 CHINESE
C               =====>  JTYPE = 19  --   ITAB = 16  <=====

      DATA  TCHI /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0., -2., -2., -1., -1.,  0.,  0.,  0.,  0.,  0.,  0.,
     5    0.,  1., -1., -1.,  0., -4.,  0.,  0.,  0.,  0.,  0.,
     A    0.,  0., -2., -2., -2., -3.,  0.,  0.,  0.,  0.,  0.,
     6    0.,  0., -4., -5., -6., -3.,  0.,  0.,  0.,  0.,  0.,
     B    0.,  0., -4., -4., -4., -3.,  0.,  0.,  0.,  0.,  0.,
     7    0.,  1., -4., -3., -2., -4.,  0.,  0.,  0.,  0.,  0.,
     C    0.,  0., -3., -1.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     8    0.,  0., -2.,  0.,  1.,  4.,  6.,  0.,  0.,  0.,  0.,
     D    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     9    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     $    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     T    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     L    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0./


C           USSR RKZ TECH MEMO TABLE FOR 00Z MORNING DAYLIGHT
C               =====>  JTYPE = 20  --   ITAB = 17  <=====

      DATA  TRK00Z /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0., -2.,  1.,  0., -1., -3., -3., -3.,  0.,  0.,  0.,
     5    0.,  0.,  0.,  0., -1., -1., -1., -3.,  0.,  0.,  0.,
     A    0.,  0.,  0.,  1.,  0., -1.,  0., -2.,  0.,  0.,  0.,
     6    0.,  0.,  0.,  2.,  1.,  0.,  1., -1.,  0.,  0.,  0.,
     B    0.,  0.,  0.,  2.,  2.,  0.,  1., -1.,  0.,  0.,  0.,
     7    0., -1.,  1.,  1.,  4.,  1.,  1., -1.,  0.,  0.,  0.,
     C    0.,  0.,  2.,  2.,  3.,  1.,  1.,  0.,  0.,  0.,  0.,
     8    0.,  0.,  3.,  3.,  2.,  1.,  0.,  2.,  0.,  0.,  0.,
     D    0.,  0.,  3.,  4.,  2.,  1.,  0.,  2.,  0.,  0.,  0.,
     9    0.,  1.,  4.,  5.,  2.,  1.,  1.,  1.,  0.,  0.,  0.,
     $    0.,  1.,  7.,  6.,  3.,  1.,  0., -1., -2.,  0.,  0.,
     T    0.,  2.,  9.,  9.,  6.,  3.,  1.,  3.,  2.,  0.,  0.,
     L    0.,  5., 15., 17., 18., 16.,  5.,  1.,  9.,  0.,  0./


C        USSR RKZ TECH MEMO TABLE FOR 12Z AFTERNOON DAYLIGHT
C               =====>  JTYPE = 20  --   ITAB = 18  <=====

      DATA  TRK12Z /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     2    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     3    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     4    0.,  1.,  2.,  3.,  3.,  3.,  3.,  1.,  1.,  0.,  0.,
     5    0.,  0.,  2.,  3.,  3.,  3.,  3.,  2.,  1.,  0.,  0.,
     A    0.,  0.,  3.,  3.,  3.,  3.,  3.,  2.,  0.,  0.,  0.,
     6    0.,  1.,  4.,  3.,  3.,  3.,  4.,  2.,  0.,  0.,  0.,
     B    0.,  1.,  4.,  3.,  3.,  3.,  3.,  1.,  0.,  0.,  0.,
     7    0.,  2.,  3.,  4.,  2.,  3.,  2.,  0.,  0.,  0.,  0.,
     C    0.,  2.,  4.,  4.,  2.,  2.,  1.,  0.,  0.,  0.,  0.,
     8    0.,  2.,  5.,  5.,  2.,  1.,  0.,  0.,  0.,  0.,  0.,
     D    0.,  3.,  6.,  6.,  3.,  1.,  1.,  1.,  0.,  0.,  0.,
     9    0.,  5.,  8.,  7.,  5.,  2.,  3.,  3.,  0.,  0.,  0.,
     $    0.,  7., 10.,  9.,  7.,  4.,  2., -2.,  0.,  0.,  0.,
     T    0.,  8., 11., 11.,  6.,  6.,  1., -1.,  0.,  0.,  0.,
     L    0., 18., 28., 17.,  5.,  5.,  0.,  0.,  0.,  0.,  0./


C        USSR UNKNOWN (AVG. OF A-22 AND RKZ) FOR 00Z MORNING DAYLIGHT
C               =====>  JTYPE = 21  --   ITAB = 19  <=====

      DATA  TAV00Z /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     2   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     3   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     4   0.0,-1.0, 1.0, 0.0,-1.0,-1.5,-2.0,-2.5, 0.0, 0.0, 0.0,
     5   0.0,-0.5, 0.0, 0.0,-1.0,-0.5,-0.5,-2.5, 0.0, 0.0, 0.0,
     A   0.0, 0.0, 0.0, 0.5, 0.0,-0.5, 0.5,-1.0, 0.0, 0.0, 0.0,
     6   0.0, 0.0, 0.0, 1.0, 1.5, 0.5, 1.5, 0.0, 0.0, 0.0, 0.0,
     B   0.0, 0.0, 0.5, 1.0, 2.0, 0.5, 1.0,-0.5, 0.0, 0.0, 0.0,
     7   0.0,-0.5, 1.5, 1.0, 3.0, 1.5, 0.0,-1.5, 0.0, 0.0, 0.0,
     C   0.0, 0.0, 2.0, 1.5, 2.5, 2.0, 0.5, 0.0, 0.0, 0.0, 0.0,
     8   0.0, 0.0, 2.5, 2.5, 2.5, 3.0, 0.5, 2.5, 0.0, 0.0, 0.0,
     D   0.0, 0.0, 2.5, 3.5, 3.0, 3.5, 1.5, 2.5, 0.0, 0.0, 0.0,
     9   0.0, 0.0, 3.5, 5.0, 3.5, 4.5, 3.0, 2.0, 0.0, 0.0, 0.0,
     $   0.0,-0.5, 5.5, 6.0, 4.0, 4.0, 3.0, 1.5, 0.5, 0.0, 0.0,
     T   0.0, 0.5, 7.5, 9.0, 7.5, 7.0, 6.5, 6.0,-2.0, 0.0, 0.0,
     L   0.0, 3.0,12.0,18.0,23.0,10.5,14.0,12.5, 4.5, 0.0, 0.0/


C      USSR UNKNOWN (AVG. OF A-22 AND RKZ) FOR 12Z AFTERNOON DAYLIGHT
C               =====>  JTYPE = 21  --   ITAB = 20  <=====

      DATA  TAV12Z /
C         N   -5    5   15   25   35   45   55   65   75   85  --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___     ANGLE
C LEVEL
     1   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     2   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     3   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     4   0.0, 1.0, 2.5, 3.0, 4.0, 2.5, 2.5, 1.0, 1.0, 0.0, 0.0,
     5   0.0, 1.0, 2.0, 3.0, 3.0, 2.5, 2.5, 2.5, 0.5, 0.0, 0.0,
     A   0.0, 1.0, 2.5, 3.0, 2.5, 3.0, 3.0, 2.5, 1.0, 0.0, 0.0,
     6   0.0, 1.0, 3.5, 2.5, 2.0, 3.5, 4.0, 2.0, 2.0, 0.0, 0.0,
     B   0.0, 1.0, 3.5, 2.5, 2.5, 3.0, 2.5, 0.5, 1.0, 0.0, 0.0,
     7   0.0, 1.5, 3.0, 3.0, 2.5, 2.0, 1.0,-1.0, 0.0, 0.0, 0.0,
     C   0.0, 1.5, 4.0, 3.5, 3.0, 2.5, 1.0,-0.5, 0.0, 0.0, 0.0,
     8   0.0, 2.0, 5.0, 4.5, 3.5, 3.0, 1.5, 0.0, 0.0, 0.0, 0.0,
     D   0.0, 2.5, 5.5, 5.5, 4.5, 4.0, 2.5, 1.5, 0.0, 0.0, 0.0,
     9   0.0, 4.0, 7.0, 7.0, 6.0, 5.5, 4.5, 3.5, 0.0, 0.0, 0.0,
     $   0.0, 4.5, 6.5, 8.5, 8.0, 6.5, 3.5, 3.0, 0.0, 0.0, 0.0,
     T   0.0, 5.5, 8.0,10.5, 3.5, 9.0, 5.5, 0.5, 0.0, 0.0, 0.0,
     L   0.0, 8.0,15.0,21.0, 4.0,11.5, 1.0, 0.0, 0.0, 0.0, 0.0/


C    NOAA / SPACE DATA CORP. 1989, TABLE FOR 00Z AFTERNOON DAYLIGHT
C      (TECH MEMO TABLE FOR NOAA / VIZ-A 1988 WITH A BIAS APPLIED)
C               =====>  JTYPE = 29  --   ITAB = 21  <=====

      DATA  TSD00Z /
C         N   -5    5   15   25   35   45   55   65   75   85 --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___    ANGLE
C LEVEL
     1  -2.7,-2.7,-2.7,-2.7,-2.7,-2.7,-2.7,-2.7,-2.7,-2.7,-2.7,
     2  -2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,
     3  -2.3,-2.3,-2.3,-2.3,-2.3,-2.3,-2.3,-2.3,-2.3,-2.3,-2.3,
     4  -1.7, 0.3, 1.3, 3.3, 4.3, 4.3, 1.3, 2.3, 3.3, 3.3, 2.3,
     5   0.9, 2.9, 3.9, 4.9, 5.9, 5.9, 4.9, 4.9, 5.9, 5.9, 6.9,
     A   4.2, 6.2, 7.2, 9.2, 9.2, 9.2, 8.2, 8.2, 9.2, 9.2,10.2,
     6   4.2, 6.2, 8.2,10.2,10.2,10.2, 9.2, 9.2, 9.2, 9.2,10.2,
     B   4.2, 6.2, 8.2,10.2,10.2,10.2, 9.2,10.2,10.2,10.2,11.2,
     7   6.3, 7.3,11.3,12.3,13.3,13.3,12.3,13.3,13.3,13.3,14.3,
     C   6.9, 7.9,12.9,13.9,14.9,14.9,14.9,14.9,14.9,15.9,15.9,
     8   6.6, 8.6,13.6,15.6,16.6,16.6,16.6,16.6,16.6,18.6,16.6,
     D   7.0,10.0,16.0,18.0,19.0,19.0,18.0,17.0,18.0,18.0,17.0,
     9   7.4,12.4,18.4,20.4,21.4,21.4,19.4,18.4,20.4,17.4,16.4,
     $   8.1,17.1,24.1,26.1,29.1,27.1,22.1,22.1,18.1,24.1,20.1,
     T   8.6,20.6,27.6,31.6,34.6,32.6,26.6,23.6,21.6,21.6,30.6,
     L   7.9,26.9,31.9,36.9,39.9,33.9,35.9,30.9,32.9,18.9,39.9/


C     NOAA / SPACE DATA CORP. 1989, TABLE FOR 12Z MORNING DAYLIGHT
C      (TECH MEMO TABLE FOR NOAA / VIZ-A 1988 WITH A BIAS APPLIED)
C               =====>  JTYPE = 29  --   ITAB = 22  <=====

      DATA  TSD12Z /
C         N   -5    5   15   25   35   45   55   65   75   85 --> SOLAR
C        ___  ___  ___  ___  ___  ___  ___  ___  ___  ___  ___    ANGLE
C LEVEL
     1  -2.7,-2.7,-2.7,-2.7,-2.7,-2.7,-2.7,-2.7,-2.7,-2.7,-2.7,
     2  -2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,
     3  -2.3,-2.3,-2.3,-2.3,-2.3,-2.3,-2.3,-2.3,-2.3,-2.3,-2.3,
     4  -1.7,-3.7,-2.0, 0.3, 2.3, 3.3, 2.3, 6.3, 7.3,-1.7,-1.7,
     5   0.9,-1.1,-0.1, 2.9, 4.9, 4.9, 5.9,10.9, 9.9, 0.9, 0.9,
     A   4.2, 2.2, 3.2, 6.2, 7.2, 8.2,10.2,15.2,14.2, 4.2, 4.2,
     6   4.2, 2.2, 4.2, 6.2, 6.2, 8.2,11.2,16.2,15.2, 4.2, 4.2,
     B   4.2, 3.2, 5.2, 7.2, 7.2,10.2,15.2,15.2,14.2, 4.2, 4.2,
     7   6.3, 6.3, 8.3,11.3,10.3,14.3,21.3,16.3,15.3, 6.3, 6.3,
     C   6.9, 6.9, 9.9,12.9,11.9,14.9,20.9,19.9,15.9, 6.9, 6.9,
     8   6.6, 7.6,11.6,13.6,13.6,14.6,18.6,22.6,16.6, 6.6, 6.6,
     D   7.0, 8.0,12.0,14.0,15.0,16.0,20.0,20.0,19.0, 7.0, 7.0,
     9   7.4, 7.4,12.4,14.4,16.4,18.4,21.4,17.4,22.4, 7.4, 7.4,
     $   8.1, 7.1,14.1,18.1,19.1,22.1,21.1,30.1, 4.1, 8.1, 8.1,
     T   8.6, 6.6,16.6,19.6,19.6,23.6,20.6,21.6, 8.6, 8.6, 8.6,
     L   7.9, 7.9,20.9,20.9,18.9,20.9,26.9,42.9, 7.9, 7.9, 7.9/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

C  EACH TIME THROUGH, COMPUTE LOGS OF PRESSURE RATIOS AND ASSIGN
C  TO LEVELS 2 THROUGH 16 -- IF R. TYPE IS NOT 29 (S.D.C. - 1989), SET
C  VALUES ON 925, 850 & 700 MB TO 0. TO KEEP CONSISTENT W/ PRIOR VERSION
C  WHICH APPLIES NO HEIGHT CORRECTION AT 700 MB (NOTE: THIS MAY BE
C  CHANGED -- WHY SHOULDN'T 700 MB HAVE A HEIGHT CORRECTION?)
C  ---------------------------------------------------------------------

      ALP(1) = 0.0
      DO I=2,16
         ALP(I) = ALOG(PRES(I-1)/PRES(I))
      ENDDO
      IF(JTYPE.NE.29)  THEN
         ALP(2) = 0.0
         ALP(3) = 0.0
         ALP(4) = 0.0
      ENDIF

C  GET TABLE INDEX FROM RADIOSONDE TYPE - CHECK FOR PM CORRECTIONS
C  ---------------------------------------------------------------

      ITAB = 0
      IF(JTYPE.GT.0 .AND. JTYPE.LT.256) ITAB = ITYPTB(JTYPE)

      OBSTM = IDATE(5) + DHR(IS)

      IF(OBSTM.GE.9. AND. OBSTM.LT.18) THEN
         IF(ITAB.EQ.1 ) ITAB = ITAB+1
         IF(ITAB.EQ.10) ITAB = ITAB+1
         IF(ITAB.EQ.17) ITAB = ITAB+1
         IF(ITAB.EQ.19) ITAB = ITAB+1
         IF(ITAB.EQ.21) ITAB = ITAB+1
      ENDIF

C  CHECK FOR VALID CORRECTION PARAMETERS
C  -------------------------------------

      NOSWC = JTYPE.LT.1 .OR. JTYPE.GT.29
      IF(NOSWC .AND. JTYPE.NE.99 .and. jtype.ne.20000)  THEN
         WRITE(6,1)  SID(IS),JTYPE
         WRITE(68,1) SID(IS),JTYPE
      END IF
1     FORMAT(/5X,'* * *  STN. ID ',A8,' HAS AN INVALID RAOB TYPE ',
     . 'DESIGNATION (BUFR/ON 29 CODE ',I8,') - NO CORRECTION POSSIBLE')

      NOSWC = NOSWC .OR. ITAB.LE.0
      SWC   = .NOT.NOSWC

      IF(LWCORR.LE.1 .AND. NOSWC) THEN
         IRET = -1
         if(jtype.eq.20000)  iret = -99
         RETURN
      ENDIF

C  GET SOLAR ELEVATION ANGLE ON ALL VALID MANDATORY LEVELS
C  -------------------------------------------------------

      CALL SOELAN(SOLAR)

C  APPLY SHORT- (& POSS. LONG-) WAVE TEMPERATURE AND HEIGHT CORRECTIONS
C  --------------------------------------------------------------------

      DO ILEV=1,16
         ANGLE(2)  = CUTOFF(ILEV)
         DTS(ILEV) = 0
         DHS(ILEV) = 0
         DTL       = 0
         DHL       = 0

         IF(SWC)  THEN
            IF(SOLAR(ILEV).GE.CUTOFF(ILEV))  THEN
               IF(SOLAR(ILEV).LT.ANGLE(11)) THEN
                  DO K=3,11
                     IF(SOLAR(ILEV).LT.ANGLE(K)) THEN
                        TABU = TTAB(K  ,ILEV,ITAB)
                        TABD = TTAB(K-1,ILEV,ITAB)
                        DSUN = (SOLAR(ILEV) - ANGLE(K-1))/
     .                   (ANGLE(K) - ANGLE(K-1))
                        DTS(ILEV) = TABD + DSUN*(TABU-TABD)
                        GO TO 10
                     ENDIF
                  ENDDO
               ELSE
                  DTS(ILEV) = TTAB(11,ILEV,ITAB)
               ENDIF
            ELSE

C  IF ANGLE < CUTOFF (NIGHT), TEMP CORRECTION FROM 'NIGHT' VALUE IN TBL
C  --------------------------------------------------------------------

               DTS(ILEV) = TTAB(1,ILEV,ITAB)
            ENDIF

C  COMPUTE SHORT-WAVE HEIGHT CORRECTIONS, VIA HYDROSTATIC INTERPOLATION
C  --------------------------------------------------------------------

10          HYF = 1.46*ALP(ILEV)
            IF(ILEV.EQ.1) THEN
               DHS(ILEV) = HYF*DTS(ILEV)
            ELSE
               DHS(ILEV) = DHS(ILEV-1) + HYF*(10.*DTS(ILEV-1)+DTS(ILEV))
            ENDIF

         ENDIF

C  CORRECT DTS BY A FACTOR OF 10
C  -----------------------------

         DTS(ILEV) = .1*DTS(ILEV)
         IF(ILEV.EQ.16 .AND. LWCORR.GT.0 .AND. TMP(16).LT.BMISS) THEN

C  FOR LWCORR .GT. 0, COMPUTE L-W TEMP & HGHT CORRECTIONS (ONLY AT 10MB)
C  ---------------------------------------------------------------------

            DTL = .0625*(TMP(ILEV)-DTS(ILEV)) + 5.09
            DHL =  (TMP(ILEV)-DTS(ILEV)) + 81.4
         ENDIF

C  SAVE AND APPLY THE CORRECTIONS
C  ------------------------------

         DHT(ILEV) = DHL-DHS(ILEV)
         DTP(ILEV) = DTL-DTS(ILEV)

         IF(HGT(ILEV).LT.BMISS) HGT(ILEV) = HGT(ILEV) + DHT(ILEV)
         IF(TMP(ILEV).LT.BMISS) TMP(ILEV) = TMP(ILEV) + DTP(ILEV)

      ENDDO

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RADT3       APPLIES NEWEST SET OF RADIATION CORRECTNS
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 2007-09-14
C
C ABSTRACT: THIS SUBROUTINE APPLIES RADIATION CORRECTIONS TO THE
C   OBSERVED RADIOSONDE HEIGHTS AND TEMPERATURES ON THE BOTTOM 16
C   MANDATORY PRESSURE LEVELS.  DATA ARE PASSED THROUGH COMMON
C   /RADCOM/.  THE CORRECTION OF RAOBS FOR BOTH SHORT- AND LONG-WAVE
C   EFFECTS IN ACCOMPLISHED BY READING IN TABLES OF NUMBERS WHICH ARE
C   DEPENDENT ON THE RADIOSONDE TYPE. CALLED FOR ONE REPORT AT A TIME.
C
C PROGRAM HISTORY LOG:
C 1990-06-20  P. R. JULIAN(W/NMC00) -- ORIGINAL AUTHOR
C 1990-12-12  D. A. KEYSER -- FURTHER REFINEMENTS
C 1992-03-18  D. A. KEYSER -- ADDED CORRECTIONS AT 925 MB TO TABLES;
C     AIR IS-4A-1680 & AIR IS-4A-1680X SONDES RECEIVE VIZ CORRECTIONS
C     (BEFORE THEY WERE NOT CORRECTED)
C 1993-05-26  D. A. KEYSER -- ADDED RADIOSONDE TYPE 47 FOR JAPAN-MEISEI
C     RS2-91, RECEIVES SAME CORRECTIONS AS MEISEI RS2-80 (I. TYPE 22)
C 1993-09-01  D. A. KEYSER -- AFTER 00Z 10/1/93 GRAVITATION CONSTANT
C     IN MICROARTS CORRECTED, VIZ & SDC SONDES NOW CORRECT GEOPOTENT.
C     VIA HYDRO. INTEGR. (LIKE ALL OTHER SONDES) RATHER THAN FROM
C     SPECIAL CORRECTION TABLES
C 1994-02-27  J. WOOLLEN   -- CONVERTED FOR USE WITH BUFR
C 1994-09-20  D. A. KEYSER -- ADDED NEW CORRECTION TABLE TO CORRECT
C     FOR VAISALA INSTRUMENT AT U.S. SITES (THIS TABLE SLIGHTLY
C     DIFFERENT THAT FOR VAISALA INSTRUMENT AT NON-U.S. SITES SINCE
C     A DIFFERENT ON-SITE CORRECTION IS APPLIED)
C 1995-11-07  D. A. KEYSER -- UPDATED THE LIST OF U.S. SITES THAT  
C     ARE TO USE THE NEW U.S. VAISALA CORRECTION TABLE IF AND WHEN 
C     THEY SWITCH TO THE VAISALA INSTRUMENT
C 1997-05-13  D. A. KEYSER -- ADDED NEW RADIOSONDE TYPES CORRESPONDING
C     TO CODE FIGURES 48-52 AND 64,65 (TYPE 52 GETS A CORRECTION)
C 1997-05-29  D. A. KEYSER -- RADIOSONDE TYPE 51 (U.S. VIZ B2 SONDE)
C     NOW GETS SAME CORRECTIONS AS TYPE 11 (VIZ B SONDE), BEFORE TYPE
C     51 WAS NOT CORRECTED.
C 1998-06-24  D. A. KEYSER -- RADIOSONDE TYPES 37, 52, 61-63 (ALL
C     VAISALA RS80) ARE NO LONGER CORRECTED (BOTH IN AND OUT OF U.S.),
C     RADIOSONDE TYPE 60 (VASIALA RS80/MICROCORA) IS STILL CORRECTED
C     AS BEFORE (BOTH IN AND OUT OF U.S.)
C 2002-07-22  D. A. KEYSER -- CALLS NEW ROUTINE SOELAN TO CALCULATE SUN
C     ANGLE ON VALID MANDATORY LEVELS RATHER THAN DOING CALCULATION IN
C     THIS SUBROUTINE
C 2007-09-14  D. KEYSER    -- MINOR CODE CORRECTIONS SUGGESTED BY PAT
C     PAULEY (CORRECTS PROBLEMS THAT COULD OCCUR ON SOME REMOTE
C     MACHINES)
C
C USAGE:    CALL RADT3(IRET)
C   OUTPUT ARGUMENT LIST:
C     IRET     - RETURN CODE (0=CORRECTIONS, -1=NO CORRECTIONS,
C              - -99=??????)
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 68  - RADCOR INFORMATION FILE
C
C REMARKS: THESE CORRECTIONS USE THE BUFR CODE FIGURE FOR RADIOSONDE
C   TYPE OBTAINED FROM BUFR-FM94 TABLE 0 02 011.  THE NMC UPPER-AIR
C   DICTIONARY ALSO CONTAINS THIS BUFR CODE FIGURE FOR RADIOSONDE
C   TYPE.  CALLED BY SUBROUTINE "RADEVN".
C
C
C   KEY FOR RADIOSONDE TYPES USED HERE:
C
C  JTYPE                   DESCRIPTION                     TABLE NUMBER
C
C  < 0                     INVALID
C  0 TO 6                  NOT USED FOR SONDES
C  7 TO 8                  RESERVED
C    9                     UNKNOWN TYPE
C   10                     (U.S.) NOAA / VIZ TYPE  A          3(T),1(G)@
C   11                     (U.S.) NOAA / VIZ TYPE  B          3(T),1(G)@
C   12                     (U.S.) NOAA / SPACE DATA CORP.     4(T),2(G)@
C   13   ==> NOT IN USE    (AUSTRALIA) ASTOR
C   14                     (U.S.) VIZ MARK I MICROSONDE
C   15                     (U.S.) EEC COMPANY TYPE  23
C   16                     (AUSTRIA) ELIN
C   17                     (GERMANY) GRAW  G
C   18                     RESERVED
C   19   ==> ONLY 1 SITE   (GERMANY) GRAW  M60                6(T)
C   20                     INDIAN MET. SERVICE TYPE  MK3
C   21                     (S. KOREA) VIZ/JIN YANG MARK I MICROSONDE
C   22                     (JAPAN) MEISEI  RS2-80             1(T)
C   23                     (FRANCE) MENSURAL FMO  1950A
C   24                     (FRANCE) MENSURAL FMO  1945A
C   25                     (FRANCE) MENSURAL  MH73A
C   26                     (SWITZERLAND) METEOLABOR BASORA
C   27                     (RUSSIA) AVK-MRZ                   7(T)
C   28                     (RUSSIA) METEORIT  MARZ2-1         8(T)
C   29                     (RUSSIA) METEORIT  MARZ2-2         8(T)
C   30                     (JAPAN) OKI  RS2-80
C   31                     (CANADA) VIZ/VALCOM TYPE A         2(T)
C   32                     (CHINA) SHANGHAI RADIO            10(T)
C   33   ==> NOT IN USE    (U.K.) UK MET OFFICE  MK3          9(T)
C   34                     (CZECHOSLOVAKIA) VINOHRADY
C   35                     VAISALA  RS18
C   36                     VAISALA  RS21
C   37                     VAISALA  RS80
C   38                     (U.S.) VIZ LOCATE (LORAN-C)
C   39                     (GERMANY) SPRENGER  E076
C   40                     (GERMANY) SPRENGER  E084
C   41                     (GERMANY) SPRENGER  E085
C   42                     (GERMANY) SPRENGER  E086
C   43                     (U.S.) AIR IS - 4A - 1680          3(T)
C   44                     (U.S.) AIR IS - 4A - 1680 X        3(T)
C   45                     (U.S.) RS MSS                      4(T)
C   46                     (U.S.) AIR IS - 4A - 403
C   47                     (JAPAN) MEISEI  RS2-91             1(T)
C   48                     (CANADA) VALCOM
C   49                     (U.S.) VIZ MARK II
C   50                     (GERMANY) GRAW DFM-90
C   51                     (U.S.) NOAA / VIZ TYPE  B2         3(T),1(G)@
C   52                     VAISALA  RS80/MICROARTS
C 53 TO 59                 RESERVED FOR ADDITIONAL SONDES
C   60                     VAISALA  RS80/MICROCORA            5 OR 11(T)
C   61                     VAISALA  RS80/DIGICORA  OR MARWIN
C   62                     VAISALA  RS80/PCCORA
C   63                     VAISALA  RS80/STAR
C   64                     (U.S.) ORBITAL SCI. CORP TYPE 909-11-xx      
C   65                     (U.S.) VIZ/TRANSPONDER TYPE 1499-520
C 66 TO 89                 RESERVED FOR ADDITIONAL AUTOMATED SYSTEMS
C   90                     UNKNOWN TYPE
C   91                     PRESSURE ONLY
C   92                     PRESSURE ONLY PLUS TRANSPONDER
C   93                     PRESSURE ONLY PLUS RADAR REFLECTOR
C   94                     NO PRESSURE PLUS TRANSPONDER
C   95                     NO PRESSURE PLUS RADAR REFLECTOR
C   96                     DESCENDING
C 97 TO 254                RESERVED
C  255                     MISSING VALUE
C >255                     INVALID
C
C  @ - VALID ONLY PRIOR TO 00Z 02/01/92
C
C
C   KEY FOR LEVELS IN DATA (CORRECTION) TABLES:
C
C     1 - 1000 MB
C     2 -- 925 MB
C     3 -- 850 MB        T --  20 MB
C     4 -- 700 MB        L --  10 MB
C     5 -- 500 MB        A -- 400 MB (INTERP. BETWEEN 500 & 300 MB)
C     6 -- 300 MB        B -- 250 MB (INTERP. BETWEEN 300 & 200 MB)
C     7 -- 200 MB        C -- 150 MB (INTERP. BETWEEN 200 & 100 MB)
C     8 -- 100 MB        D --  70 MB (INTERP. BETWEEN 100 &  50 MB)
C     9 --  50 MB        E --  30 MB (INTERP. BETWEEN  50 &  10 MB)
C     $ --  30 MB        F --  20 MB (INTERP. BETWEEN  50 &  10 MB)
C
C
C   THE RADIATION CORRECTION TABLES ARE BASED UPON DATA FROM TESTS BY
C   F. SCHMIDLIN AND FROM OBSERVED INCREMENTS FROM NWP ASSIMILATION.
C   REFER TO NMC OFFICE NOTE 374 BY P. JULIAN, ET. AL.
C
C
C   THE CUTOFF ANGLE AT EACH LEVEL CALCULATED USING THE RELATION:
C              CUTOFF ANGLE = -1.76459 * (Z**.40795) ,
C   WHERE Z IS A REFERENCE HEIGHT FOR THE LEVEL (IN KILOMETERS)
C   (SEE NMC OFFICE NOTE 306, TABLE 4 FOR LIST OF REFERENCE HEIGHTS.)
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE RADT3(IRET)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /RADCOM/ HGT(16),TMP(16),DHT(16),DTP(16),JTYPE
      COMMON /SWITCH/ LWCORR,LEVRAD,IRCTBL,HGTTBL,BAL_DRIFT
      COMMON /PMAND / PRES(16),KMIN,KMAX,INM(16)
      COMMON /STN/     IS
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      REAL        TTAB(10,16,12),ZTAB(10,16,2),CUTOFF(16),
     .            SOLAR(16),ANGLE(10),ALP(16),DTS(16),DHS(16),
     .            JAPANT(10,16),CANADT(10,16),USVZAT(10,16),
     .            VRS80T(10,16),GRM60T(10,16),A22IVT(10,16),
     .            UKMK3T(10,16),SHANGT(10,16),USVZAG(10,16),
     .            USSDCT(10,16),MARZ2T(10,16),USSDCG(10,16),
     .            VRS80U(10,16),USMK2T(10,16)
      INTEGER     ITYPTT(255),ITYPTG(255)
      EQUIVALENCE (TTAB(1,1, 1),JAPANT(1,1)),(TTAB(1,1, 2),CANADT(1,1)),
     .            (TTAB(1,1, 3),USVZAT(1,1)),(TTAB(1,1, 4),USSDCT(1,1)),
     .            (TTAB(1,1, 5),VRS80T(1,1)),(TTAB(1,1, 6),GRM60T(1,1)),
     .            (TTAB(1,1, 7),A22IVT(1,1)),(TTAB(1,1, 8),MARZ2T(1,1)),
     .            (TTAB(1,1, 9),UKMK3T(1,1)),(TTAB(1,1,10),SHANGT(1,1)),
     .            (TTAB(1,1,11),VRS80U(1,1)),(TTAB(1,1,12),USMK2T(1,1)),
     .            (ZTAB(1,1, 1),USVZAG(1,1)),(ZTAB(1,1, 2),USSDCG(1,1))
      LOGICAL     HGTTBL
      CHARACTER*8 SID

      DATA  ANGLE  / -90, -5, 5, 15, 25, 35, 45, 55, 65, 75/
      DATA  CUTOFF / -.73, -1.58, -2.06, -2.77,
     .              -3.56, -3.95, -4.36, -4.58,
     .              -4.83, -5.12, -5.49, -5.79,
     .              -6.06, -6.43, -6.72, -7.17/

C  ITYPTT ASSIGNS PROPER TEMPERATURE CORRECTION TABLE TO INST. TYPES
C  -----------------------------------------------------------------

C  ===> JTYPE:      1  2  3  4  5  6  7  8  9 10 11 12 13 14 15

      DATA  ITYPTT/-1,-1,-1,-1,-1,-1,-1,-1,-1, 3, 3, 4, 0, 0, 0,

C  ===> JTYPE:     16 17 18 19 20 21 22 23 24 25 26 27 28 29 30

     $              0, 0,-1, 6, 0, 0, 1, 0, 0, 0, 0, 7, 8, 8, 0,

C  ===> JTYPE:     31 32 33 34 35 36 37 38 39 40 41 42 43 44 45

     $              2,10, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 4,

C  ===> JTYPE:     46 47 48 49 50 51 52 53 54 55 56 57 58 59 60

     $              0, 1, 0, 0, 0, 3, 0,-1,-1,-1,-1,-1,-1,-1, 5,

C  ===> JTYPE:     61 62 63 64 65 66-89 90-96 97 98 99 100-255

     $              0, 0, 0, 0, 0,24*-1, 7*0, -1,-1,-1, 156*0/

C  ITYPTG ASSIGNS PROPER GEOPOTENTIAL CORRECTION TABLE TO INST. TYPES
C  ------------------------------------------------------------------

C  ===> JTYPE:      1  2  3  4  5  6  7  8  9 10 11 12 13 14 15

      DATA  ITYPTG/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 0, 0, 0,

C  ===> JTYPE:     16 17 18 19 20 21 22 23 24 25 26 27 28 29 30

     $              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,

C  ===> JTYPE:     31 32 33 34 35 36 37 38 39 40 41 42 43 44 45

     $              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,

C  ===> JTYPE:     46 47 48 49 50 51 52 53 54 55 56 57 58 59 60

     $              0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,

C  ===> JTYPE:     61 62 63 64 65 66-255

     $              0, 0, 0, 0, 0,190*0/

C  TEMPERATURE CORRECTION TABLES ( * 10 K) FOR RADIATIVE EFFECTS
C  -------------------------------------------------------------


C                      (JAPAN) MEISEI  RS2-80
C              =====>  JTYPE = 22  --   ITAB = 1   <=====
C                             -- OR --
C                      (JAPAN) MEISEI  RS2-91
C              =====>  JTYPE = 47  --   ITAB = 1   <=====

      DATA  JAPANT  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C        ___  ___    ___   ___   ___   ___   ___   ___   ___   ___
C LEVEL
     1   0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     2   0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     3   0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     4   0.0,  0.0,  3.0,  0.0,  1.0,  2.0,  2.0,  2.0,  3.0,  0.0,
     5   0.0,  0.0,  0.0,  2.0,  3.0,  3.0,  4.0,  5.0,  7.0,  0.0,
     A   0.0,  0.0,  0.0,  3.0,  3.0,  3.0,  4.0,  5.0,  6.0,  0.0,
     6   0.0,  0.0,  0.0,  6.0,  4.0,  3.0,  4.0,  5.0,  6.0,  0.0,
     B   0.0,  0.0,  0.0,  5.0,  4.0,  4.0,  4.0,  4.0,  5.0,  0.0,
     7   0.0,  0.0,  0.0,  4.0,  6.0,  6.0,  4.0,  4.0,  5.0,  0.0,
     C   0.0,  0.0,  0.0,  5.0,  6.0,  6.0,  5.0,  5.0,  4.0,  0.0,
     8   0.0,  0.0,  0.0,  7.0,  8.0,  7.0,  8.0,  8.0,  4.0,  0.0,
     D   0.0,  0.0,  0.0, 10.0, 10.0,  8.0,  8.0,  8.0,  2.0,  0.0,
     9   0.0,  0.0,  0.0, 13.0, 12.0, 10.0,  8.0,  9.0,  2.0,  1.0,
     $   0.0,  0.0,  0.0, 26.0, 19.0, 16.0, 12.0, 11.0, 10.0, -5.0,
     T   0.0,  0.0,  0.0, 33.0, 30.0, 28.0, 19.0, 13.0, 14.0,-10.0,
     L   0.0,  0.0,  0.0, 43.0, 41.0, 35.0, 16.0, 18.0, 14.0,  0.0/


C        (CANADA) VIZ/VALCOM TYPE A -- SAME CORRECTIONS AS VIZ-A
C              =====>  JTYPE = 31  --   ITAB = 2   <=====

      DATA  CANADT  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C        ___  ___    ___   ___   ___   ___   ___   ___   ___   ___
C LEVEL
     1   0.2,  0.2,  0.2,  0.8,  0.8,  0.9,  0.9,  0.9,  0.9,  0.9,
     2  -0.1, -0.1,  0.3,  1.1,  1.1,  1.2,  1.3,  1.3,  1.3,  1.3,
     3  -0.3, -0.3,  0.3,  1.3,  1.3,  1.5,  1.7,  1.7,  1.7,  1.7,
     4  -0.5, -0.5,  0.6,  1.9,  1.9,  2.0,  2.0,  2.0,  2.0,  2.0,
     5  -0.6, -0.6,  1.3,  2.9,  2.9,  2.7,  2.5,  2.5,  2.5,  2.5,
     A  -0.5, -0.5,  1.7,  3.7,  3.7,  3.3,  2.8,  2.8,  2.8,  2.8,
     6  -0.2, -0.2,  2.4,  4.3,  4.3,  3.8,  3.3,  3.3,  3.3,  3.3,
     B   0.1,  0.1,  3.1,  4.8,  4.8,  4.5,  4.3,  4.3,  4.3,  4.3,
     7   0.1,  0.1,  3.7,  5.2,  5.2,  5.4,  5.6,  5.6,  5.6,  5.6,
     C  -0.2, -0.2,  4.4,  6.0,  6.0,  6.5,  7.1,  7.1,  7.1,  7.1,
     8  -0.7, -0.7,  5.5,  7.2,  7.2,  7.6,  8.1,  8.1,  8.1,  8.1,
     D  -1.5, -1.5,  6.3,  8.4,  8.4,  8.6,  8.9,  8.9,  8.9,  8.9,
     9  -2.3, -2.3,  6.1,  9.2,  9.2,  9.3,  9.5,  9.5,  9.5,  9.5,
     $  -3.3, -3.3,  5.8, 10.2, 10.2, 10.0,  9.9,  9.9,  9.9,  9.9,
     T  -4.8, -4.8,  5.4, 10.7, 10.7, 10.2,  9.8,  9.8,  9.8,  9.8,
     L -10.6,-10.6,  3.8, 11.8, 11.8, 10.5,  9.3,  9.3,  9.3,  9.3/


C          (U.S.) NOAA / VIZ  TYPE A -- BASED ON SCHMIDLIN
C              =====>  JTYPE = 10  --   ITAB = 3   <=====
C                             -- OR --
C          (U.S.) NOAA / VIZ  TYPE B -- BASED ON SCHMIDLIN
C              =====>  JTYPE = 11  --   ITAB = 3   <=====
C                             -- OR --
C          (U.S.) AIR IS - 4A - 1680 -- BASED ON SCHMIDLIN
C              =====>  JTYPE = 43  --   ITAB = 3   <=====
C                             -- OR --
C          (U.S.) AIR IS - 4A - 1680 X -- BASED ON SCHMIDLIN
C              =====>  JTYPE = 44  --   ITAB = 3   <=====

      DATA  USVZAT  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C      _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
C LEVEL
     1   0.2,  0.2,  0.2,  0.8,  0.8,  0.9,  0.9,  0.9,  0.9,  0.9,
     2  -0.1, -0.1,  0.3,  1.1,  1.1,  1.2,  1.3,  1.3,  1.3,  1.3,
     3  -0.3, -0.3,  0.3,  1.3,  1.3,  1.5,  1.7,  1.7,  1.7,  1.7,
     4  -0.5, -0.5,  0.6,  1.9,  1.9,  2.0,  2.0,  2.0,  2.0,  2.0,
     5  -0.6, -0.6,  1.3,  2.9,  2.9,  2.7,  2.5,  2.5,  2.5,  2.5,
     A  -0.5, -0.5,  1.7,  3.7,  3.7,  3.3,  2.8,  2.8,  2.8,  2.8,
     6  -0.2, -0.2,  2.4,  4.3,  4.3,  3.8,  3.3,  3.3,  3.3,  3.3,
     B   0.1,  0.1,  3.1,  4.8,  4.8,  4.5,  4.3,  4.3,  4.3,  4.3,
     7   0.1,  0.1,  3.7,  5.2,  5.2,  5.4,  5.6,  5.6,  5.6,  5.6,
     C  -0.2, -0.2,  4.4,  6.0,  6.0,  6.5,  7.1,  7.1,  7.1,  7.1,
     8  -0.7, -0.7,  5.5,  7.2,  7.2,  7.6,  8.1,  8.1,  8.1,  8.1,
     D  -1.5, -1.5,  6.3,  8.4,  8.4,  8.6,  8.9,  8.9,  8.9,  8.9,
     9  -2.3, -2.3,  6.1,  9.2,  9.2,  9.3,  9.5,  9.5,  9.5,  9.5,
     $  -3.3, -3.3,  5.8, 10.2, 10.2, 10.0,  9.9,  9.9,  9.9,  9.9,
     T  -4.8, -4.8,  5.4, 10.7, 10.7, 10.2,  9.8,  9.8,  9.8,  9.8,
     L -10.6,-10.6,  3.8, 11.8, 11.8, 10.5,  9.3,  9.3,  9.3,  9.3/


C                   (U.S.) RS SDC / SPACE DATA CORP.
C              =====>  JTYPE = 12  --   ITAB = 4   <=====
C                             -- OR --
C                            (U.S.) RS MSS
C              =====>  JTYPE = 45  --   ITAB = 4   <=====

      DATA  USSDCT  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C      _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
C LEVEL
     1  -2.4, -2.4, -2.3, -2.1, -1.8, -1.5, -1.2, -1.2, -1.2, -1.2,
     2  -2.6, -2.6, -2.2, -1.8, -1.4, -1.0, -0.5, -0.5, -0.5, -0.5,
     3  -2.8, -2.8, -2.1, -1.4, -1.0, -0.4,  0.2,  0.2,  0.2,  0.2,
     4  -2.8, -2.8, -1.7,  0.2,  0.8,  1.3,  1.6,  1.6,  1.6,  1.6,
     5  -1.3, -1.3,  1.1,  3.5,  3.8,  4.1,  4.5,  4.5,  4.5,  4.5,
     A   0.7,  0.7,  3.0,  6.0,  6.2,  6.4,  6.7,  6.7,  6.7,  6.7,
     6   2.8,  2.8,  6.5,  9.1,  9.4,  9.7,  9.9,  9.9,  9.9,  9.9,
     B   3.8,  3.8,  7.9, 11.4, 11.8, 12.1, 12.4, 12.4, 12.4, 12.4,
     7   4.9,  4.9,  9.1, 13.7, 14.2, 14.6, 14.8, 14.8, 14.8, 14.8,
     C   4.9,  4.9, 10.9, 16.0, 16.6, 16.8, 17.1, 17.1, 17.1, 17.1,
     8   4.7,  4.7, 11.0, 17.2, 18.4, 18.8, 19.1, 19.1, 19.1, 19.1,
     D   4.3,  4.3, 12.2, 19.4, 20.5, 20.7, 21.9, 21.9, 21.9, 21.9,
     9   3.9,  3.9, 14.0, 21.2, 22.2, 23.4, 24.5, 24.5, 24.5, 25.5,
     $   3.1,  3.1, 17.4, 23.2, 23.5, 24.4, 25.9, 25.9, 25.9, 25.9,
     T   1.7,  1.7, 14.7, 24.7, 25.5, 26.2, 26.8, 26.8, 26.8, 26.8,
     L  -4.0, -4.0, 12.0, 25.8, 26.6, 27.7, 28.3, 28.3, 28.3, 28.3/


C               VAISALA  RS80/MICROCORA -- NON-U.S. SITES
C              =====>  JTYPE = 60  --   ITAB = 5   <=====

      DATA  VRS80T  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C      _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
C LEVEL
     1   2.0,  2.0,  0.0,  2.0,  2.0,  1.5,  1.0,  1.0,  1.0,  1.0,
     2   1.0,  1.0,  0.5,  2.0,  2.0,  1.5,  1.0,  1.0,  1.0,  1.0,
     3   0.0,  0.0,  1.0,  2.0,  2.0,  1.5,  1.0,  1.0,  1.0,  1.0,
     4   1.0,  1.0,  2.0,  3.0,  3.0,  1.5,  0.0,  0.0,  0.0,  0.0,
     5   1.0,  1.0,  2.0,  1.0,  1.0,  0.5,  0.0,  0.0,  0.0,  0.0,
     A   2.0,  2.0,  4.0,  1.0,  1.0,  0.0, -1.0, -1.0, -1.0, -1.0,
     6   4.0,  4.0,  5.0,  3.0,  3.0,  2.0,  1.0,  1.0,  1.0,  1.0,
     B   6.0,  6.0,  7.0,  4.0,  4.0,  3.5,  3.0,  3.0,  3.0,  3.0,
     7   5.0,  5.0,  5.0,  5.0,  5.0,  4.5,  4.0,  4.0,  4.0,  4.0,
     C   5.0,  5.0,  6.0,  3.0,  3.0,  3.5,  4.0,  4.0,  4.0,  4.0,
     8   5.0,  5.0,  6.0,  3.0,  3.0,  3.0,  3.0,  3.0,  3.0,  3.0,
     D   4.0,  4.0,  7.0,  3.0,  3.0,  2.5,  2.0,  2.0,  2.0,  2.0,
     9   6.0,  6.0,  4.0, -1.0, -1.0,  1.0,  3.0,  3.0,  3.0,  3.0,
     $   7.0,  7.0,  3.0, -2.0, -2.0, -1.0,  0.0,  0.0,  0.0,  0.0,
     T  11.0, 11.0,  3.0, -2.0, -2.0, -1.5, -1.0, -1.0, -1.0, -1.0,
     L  20.0, 20.0, 15.0, 11.0, 11.0,  9.0,  7.0,  7.0,  7.0,  7.0/


C               (GERMANY) GRAW  M60 -- BASED ON SCHMIDLIN
C              =====>  JTYPE = 19  --   ITAB = 6   <=====

      DATA  GRM60T  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C      _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
C LEVEL
     1  -5.0, -5.0, -2.0,  0.0,  0.0,  1.5,  3.0,  3.0,  3.0,  3.0,
     2  -1.0, -1.0, -1.5,  0.5,  0.5,  1.5,  2.5,  2.5,  2.5,  2.5,
     3   3.0,  3.0, -1.0,  1.0,  1.0,  1.5,  2.0,  2.0,  2.0,  2.0,
     4   4.0,  4.0,  0.0, -2.0, -2.0, -1.5, -1.0, -1.0, -1.0, -1.0,
     5   2.0,  2.0,  3.0,  4.0,  4.0,  2.5,  1.0,  1.0,  1.0,  1.0,
     A   2.0,  2.0,  6.0,  3.0,  3.0,  2.0,  1.0,  1.0,  1.0,  1.0,
     6   6.0,  6.0,  8.0,  8.0,  8.0,  5.0,  3.0,  3.0,  3.0,  3.0,
     B   8.0,  8.0,  8.0,  9.0,  9.0,  6.0,  3.0,  3.0,  3.0,  3.0,
     7   8.0,  8.0,  8.0,  5.0,  5.0,  4.5,  4.0,  4.0,  4.0,  4.0,
     C   5.0,  5.0,  7.0,  1.0,  1.0,  3.5,  6.0,  6.0,  6.0,  6.0,
     8   3.0,  3.0,  8.0,  9.0,  9.0,  7.5,  6.0,  6.0,  6.0,  6.0,
     D   1.0,  1.0,  9.0,  5.0,  5.0,  7.5, 10.0, 10.0, 10.0, 10.0,
     9   2.0,  2.0,  1.0,  2.0,  3.0,  5.0,  9.0,  9.0,  9.0,  9.0,
     $  -6.0, -6.0,-11.0, -8.0, -5.0,  0.0,  4.0,  4.0,  4.0,  4.0,
     T  -3.0, -3.0,-15.0,-10.0, -7.5, -5.0,  0.0,  0.0,  0.0,  0.0,
     L  -3.0, -3.0,-99.0,-75.0,-50.0,-35.0,-27.0,-27.0,-27.0,-27.0/


C        (RUSSIA) METEORIT  AVK-MRZ - AVERAGED FROM PREVIOUS TABLES
C              =====>  JTYPE = 27  --   ITAB = 7   <=====

      DATA  A22IVT  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C      _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
C LEVEL
     1   0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     2   0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     3   0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     4   0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     5   0.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     A   0.0,  0.0,  1.0,  1.0,  1.0,  1.0,  0.0,  0.0,  0.0,  0.0,
     6   0.0,  0.0,  2.0,  1.0,  1.0,  1.0,  0.0,  0.0,  0.0,  0.0,
     B   0.0,  0.0,  2.0,  2.0,  2.0,  2.0,  0.0,  0.0,  0.0,  0.0,
     7   0.0,  0.0,  2.0,  2.0,  2.0,  2.0,  1.0,  0.0,  0.0,  0.0,
     C   0.0,  0.0,  2.0,  3.0,  3.0,  3.0,  2.0,  0.0,  0.0,  0.0,
     8   0.0,  0.0,  3.0,  4.0,  4.0,  5.0,  3.0,  1.0,  0.0,  0.0,
     D   0.0,  0.0,  3.0,  4.0,  5.0,  7.0,  5.0,  3.0,  0.0,  0.0,
     9   0.0,  0.0,  4.0,  6.0,  6.0,  8.0,  6.0,  4.0,  0.0,  0.0,
     $   0.0,  0.0,  4.0,  7.0,  7.0,  8.0,  6.0,  6.0,  0.0,  0.0,
     T   0.0,  0.0,  5.0, 10.0,  5.0, 11.0, 11.0,  6.0,  0.0,  0.0,
     L   0.0,  0.0,  6.0, 22.0, 16.0, 11.5, 11.3, 11.0,  0.0,  0.0/


C      (RUSSIA) METEORIT  MARZ2-1 -- AVERAGED FROM PREVIOUS TABLES
C              =====>  JTYPE = 28  --   ITAB = 8   <=====
C                              -- OR --
C      (RUSSIA) METEORIT  MARZ2-2 -- AVERAGED FROM PREVIOUS TABLES
C              =====>  JTYPE = 29  --   ITAB = 8   <=====

      DATA  MARZ2T  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C      _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
C LEVEL
     1   0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     2   0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     3   0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     4   0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     5   0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
     A   0.0,  0.0,  1.0,  1.0,  1.0,  1.0,  0.0,  0.0,  0.0,  0.0,
     6   0.0,  0.0,  1.0,  1.0,  1.0,  1.0,  0.0,  0.0,  0.0,  0.0,
     B   0.0,  0.0,  2.0,  2.0,  2.0,  1.0,  0.0,  0.0,  0.0,  0.0,
     7   0.0,  0.0,  2.0,  2.0,  2.0,  1.0,  0.0,  0.0,  0.0,  0.0,
     C   0.0,  0.0,  3.5,  3.0,  2.5,  1.0,  0.5,  0.0,  0.0,  0.0,
     8   0.0,  1.0,  4.0,  4.0,  2.5,  1.0,  0.5,  0.0,  0.0,  0.0,
     D   0.0,  2.0,  4.5,  4.5,  2.5,  1.0,  0.5,  0.0,  0.0,  0.0,
     9   0.0,  3.0,  6.0,  6.0,  3.5,  1.5,  1.0,  0.0,  0.0,  0.0,
     $   0.0,  4.0,  8.5,  7.5,  5.0,  2.5,  1.0,  0.0,  0.0,  0.0,
     T   0.0,  5.0, 10.0, 10.0,  6.0,  4.5,  1.0,  0.0,  0.0,  0.0,
     L   0.0, 11.0, 22.0, 17.0, 11.5, 10.5,  2.0,  1.0,  0.0,  0.0/


C            (U.K.) UK MET OFFICE  MK3 -- BASED ON SCHMIDLIN
C       (CURRENTLY NOT IN USE -- UK STATIONS USE VAISALA SONDE)
C              =====>  JTYPE = 33  --   ITAB = 9   <=====

      DATA  UKMK3T  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C      _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
C LEVEL
     1   3.0,  3.0, -4.0, -3.0, -3.0, -4.0, -6.0, -6.0, -6.0, -6.0,
     2   0.5,  0.5, -4.0, -2.0, -2.0, -3.0, -4.5, -4.5, -4.5, -4.5,
     3  -2.0, -2.0, -4.0, -1.0, -1.0, -2.0, -3.0, -3.0, -3.0, -3.0,
     4  -1.0, -1.0,  1.0,  0.0,  0.0, -1.0, -2.0, -2.0, -2.0, -2.0,
     5   0.0,  0.0, -2.0,  0.0,  0.0, -1.0, -3.0, -3.0, -3.0, -3.0,
     A  -1.0, -1.0,  0.0,  0.0,  0.0, -1.0, -3.0, -3.0, -3.0, -3.0,
     6  -1.0, -1.0,  0.0,  1.0,  1.0,  0.0, -2.0, -2.0, -2.0, -2.0,
     B   0.0,  0.0,  2.0,  3.0,  3.0,  1.0, -1.0, -1.0, -1.0, -1.0,
     7   0.0,  0.0,  1.0,  2.0,  2.0,  1.0,  1.0,  1.0,  1.0,  1.0,
     C   0.0,  0.0,  2.0,  2.0,  2.0,  1.0,  1.0,  1.0,  1.0,  1.0,
     8   0.0,  0.0,  2.0,  1.0,  1.0,  1.0,  2.0,  2.0,  2.0,  2.0,
     D   0.0,  0.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,
     9   0.0,  0.0,  3.0,  5.0,  5.0,  3.0,  2.0,  2.0,  2.0,  2.0,
     $   1.0,  1.0,  2.0,  4.0,  4.0,  3.0,  2.0,  2.0,  2.0,  2.0,
     T   3.0,  3.0,  3.0,  5.0,  5.0,  3.0,  1.0,  1.0,  1.0,  1.0,
     L   6.0,  6.0, 11.0, 11.0, 11.0, 10.0, 10.0, 10.0, 10.0, 10.0/


C              (CHINA) SHANGHAI RADIO -- BASED ON JULIAN
C              =====>  JTYPE = 32  --   ITAB = 10  <=====

      DATA  SHANGT  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C      _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
C LEVEL
     1   0.5,  0.3,  0.0,  0.4,  0.5,  0.9,  0.9,  0.9,  0.9,  0.9,
     2   0.5,  0.6,  1.0,  1.2,  1.0,  0.9,  0.9,  0.9,  0.9,  0.9,
     3   0.5,  0.9,  1.9,  2.0,  1.5,  0.9,  0.9,  0.9,  0.9,  0.9,
     4  -0.1,  1.9,  2.6,  2.7,  1.8,  1.8,  1.8,  1.9,  1.9,  1.9,
     5   1.2,  5.5,  6.3,  6.0,  4.9,  4.1,  4.1,  4.1,  4.2,  4.2,
     A   4.7,  8.8,  9.8,  9.7,  9.1,  9.0,  9.0,  9.0,  9.0,  9.0,
     6  10.3, 13.7, 14.2, 14.7, 15.2, 16.0, 16.0, 16.0, 16.0, 16.0,
     B  13.7, 15.3, 16.4, 17.2, 18.5, 19.9, 19.9, 19.9, 19.9, 19.9,
     7  16.7, 17.4, 18.0, 18.9, 21.1, 22.8, 22.8, 22.8, 22.8, 22.8,
     C  17.4, 17.2, 17.1, 17.6, 20.3, 22.0, 22.0, 22.0, 22.0, 22.0,
     8   8.7,  8.3,  8.1,  6.0,  7.6,  7.4,  7.4,  7.4,  7.4,  7.4,
     D -11.5,-10.1,- 9.1,-13.9,-13.4,-13.2,-13.2,-13.2,-13.2,-13.2,
     9 - 9.0,-12.6,-13.3,-20.3,-18.9,-18.9,-18.9,-18.9,-18.9,-18.9,
     $   1.8,- 6.7,- 7.4,-15.3,-15.0,-17.0,-17.1,-17.1,-17.1,-17.1,
     T  11.4,  5.0,  2.0,- 5.0,- 6.4,-10.0,-10.0,-10.0,-10.0,-10.0,
     L  16.6, 13.5, 12.1,  8.5,  5.7,  4.0,  4.0,  4.0,  4.0,  4.0/


C                 VAISALA  RS80/MICROCORA -- U.S. SITES
C              =====>  JTYPE = 60  --   ITAB = 11  <=====

      DATA  VRS80U  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C      _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
C LEVEL
     1   2.0,  2.0,  0.0,  2.0,  2.0,  1.5,  1.0,  1.0,  1.0,  1.0,
     2   1.0,  1.0,  0.5,  2.0,  2.0,  1.5,  1.0,  1.0,  1.0,  1.0,
     3   0.0,  0.0,  1.0,  2.0,  2.0,  1.5,  1.0,  1.0,  1.0,  1.0,
     4   1.0,  1.0,  2.0,  3.0,  3.0,  1.5,  0.0,  0.0,  0.0,  0.0,
     5   1.0,  1.0,  2.0,  1.0,  1.0,  0.5,  0.0,  0.0,  0.0,  0.0,
     A   2.0,  2.0,  4.0,  1.0,  1.0,  0.0, -1.0, -1.0, -1.0, -1.0,
     6   4.0,  4.0,  5.0,  3.0,  3.0,  2.0,  1.0,  1.0,  1.0,  1.0,
     B   6.0,  6.0,  7.0,  4.0,  4.0,  3.5,  3.0,  3.0,  3.0,  3.0,
     7   5.0,  5.0,  5.0,  5.0,  5.0,  4.5,  4.0,  4.0,  4.0,  4.0,
     C   5.0,  5.0,  6.0,  3.0,  3.0,  3.5,  4.0,  4.0,  4.0,  4.0,
     8   5.0,  5.0,  6.0,  3.0,  3.0,  3.0,  3.0,  3.0,  3.0,  3.0,
     D   4.0,  4.0,  7.0,  3.0,  3.0,  2.5,  2.0,  2.0,  2.0,  2.0,
     9   6.0,  6.0,  4.0, -1.0, -1.0,  1.0,  3.0,  3.0,  3.0,  3.0,
     $   4.0,  4.0,  2.0, -2.0, -2.0, -1.0,  0.0,  0.0,  0.0,  0.0,
     T   7.0,  7.0,  2.0, -2.0, -2.0, -1.5, -1.0, -1.0, -1.0, -1.0,
     L  13.0, 13.0, 13.0, 11.0, 11.0,  9.0,  7.0,  7.0,  7.0,  7.0/


C                 VIZ MICROSONDE (MARK II)
C              =====>  JTYPE = 49  --   ITAB = 12  <=====

      DATA  USMK2T  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C      _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
C LEVEL
     1   1.2,  1.2,  0.2,  0.8,  0.8,  0.9,  0.9,  0.9,  0.9,  0.9,
     2   0.9,  0.9,  0.3,  1.1,  1.1,  1.2,  1.3,  1.3,  1.3,  1.3,
     3   0.7,  0.7,  0.3,  1.3,  1.3,  1.5,  1.7,  1.7,  1.7,  1.7,
     4   0.5,  0.5,  0.6,  1.9,  1.9,  2.0,  2.0,  2.0,  2.0,  2.0,
     5   0.4,  0.4,  1.3,  2.9,  2.9,  2.7,  2.5,  2.5,  2.5,  2.5,
     A   0.5,  0.5,  1.7,  3.7,  3.7,  3.3,  2.8,  2.8,  2.8,  2.8,
     6   0.8,  0.8,  2.4,  4.3,  4.3,  3.8,  3.3,  3.3,  3.3,  3.3,
     B   0.9,  1.1,  3.1,  4.8,  4.8,  4.5,  4.3,  4.3,  4.3,  4.3,
     7   0.9,  0.1,  2.7,  4.2,  4.2,  4.4,  4.6,  4.6,  4.6,  4.6,
     C   0.8, -0.2,  3.4,  4.0,  4.0,  4.5,  5.1,  5.1,  5.1,  5.1,
     8   0.3, -0.7,  4.5,  5.2,  5.2,  5.6,  6.1,  6.1,  6.1,  6.1,
     D  -0.5, -1.5,  4.3,  5.4,  6.4,  6.6,  5.9,  6.9,  6.9,  6.9,
     9  -2.3, -2.3,  4.1,  6.2,  6.2,  6.3,  6.5,  6.5,  6.5,  6.5,
     $  -3.3, -4.3,  2.8,  5.2,  5.2,  5.0,  4.9,  4.9,  4.9,  4.9,
     T  -5.8, -7.8,  0.4,  2.7,  2.7,  5.2,  4.8,  4.8,  4.8,  4.8,
     L -12.6,-15.6, -5.2, -2.2, -2.2, -3.5, -4.7, -4.7, -4.7, -4.7/


C  GEOPOTENTIAL CORRECTION TABLES (METERS) FOR RADIATIVE EFFECTS
C  AND INCORRECT GRAVITY CONSTANT IN MICROARTS
C  -------------------------------------------------------------


C          (U.S.) NOAA / VIZ  TYPE A -- BASED ON SCHMIDLIN
C              =====>  JTYPE = 10  --   JTAB = 1  <=====
C                             -- OR --
C          (U.S.) NOAA / VIZ  TYPE B -- BASED ON SCHMIDLIN
C              =====>  JTYPE = 11  --   JTAB = 1  <=====

      DATA  USVZAG  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C      _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
C LEVEL
     1    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     2    0.,   0.,   0.,   1.,   1.,   1.,   1.,   1.,   1.,   1.,
     3    1.,   1.,   1.,   2.,   2.,   2.,   2.,   2.,   2.,   2.,
     4    2.,   2.,   3.,   4.,   4.,   4.,   4.,   4.,   4.,   4.,
     5    3.,   3.,   5.,   8.,   8.,   8.,   8.,   8.,   8.,   8.,
     A    4.,   4.,   7.,  11.,  11.,  11.,  11.,  11.,  11.,  11.,
     6    5.,   5.,  10.,  16.,  16.,  16.,  15.,  15.,  15.,  15.,
     B    6.,   6.,  13.,  19.,  19.,  18.,  17.,  17.,  17.,  17.,
     7    7.,   7.,  16.,  23.,  23.,  22.,  21.,  21.,  21.,  21.,
     C    8.,   8.,  20.,  29.,  29.,  29.,  28.,  28.,  28.,  28.,
     8    9.,   9.,  28.,  39.,  39.,  39.,  39.,  39.,  39.,  39.,
     D    9.,   9.,  36.,  48.,  48.,  48.,  49.,  49.,  49.,  49.,
     9    9.,   9.,  43.,  58.,  59.,  59.,  60.,  60.,  60.,  60.,
     $    7.,   7.,  54.,  75.,  75.,  75.,  76.,  76.,  76.,  76.,
     T    4.,   4.,  63.,  89.,  89.,  89.,  90.,  90.,  90.,  90.,
     L   -9.,  -9.,  75., 115., 115., 113., 112., 112., 112., 110./


C     (U.S.) RS SDC / SPACE DATA CORP. -- BASED ON SCHMIDLIN/AHNERT
C              =====>  JTYPE = 12  --   JTAB = 2  <=====

      DATA  USSDCG  /
C       NITE   -5     5    15     25    35    45    55    65    75 S.ANG
C      _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
C LEVEL
     1    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     2    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     3    0.,   0.,   0.,   1.,   1.,   1.,   1.,   1.,   1.,   1.,
     4    0.,   0.,   1.,   1.,   2.,   3.,   3.,   3.,   3.,   3.,
     5   -1.,  -1.,   3.,   5.,   6.,   7.,   7.,   7.,   7.,   7.,
     A    0.,   0.,   4.,   9.,  11.,  11.,  12.,  12.,  12.,  12.,
     6    3.,   3.,  10.,  17.,  18.,  19.,  21.,  21.,  21.,  21.,
     B    5.,   5.,  13.,  23.,  25.,  26.,  27.,  27.,  27.,  27.,
     7    9.,   9.,  16.,  32.,  33.,  35.,  37.,  37.,  37.,  37.,
     C   14.,  14.,  25.,  46.,  48.,  50.,  52.,  52.,  52.,  52.,
     8   21.,  21.,  38.,  67.,  69.,  72.,  75.,  75.,  75.,  75.,
     D   27.,  27.,  56.,  87.,  90.,  94.,  98.,  98.,  98.,  98.,
     9   33.,  33.,  85., 108., 113., 117., 122., 122., 122., 122.,
     $   41.,  41.,  94., 144., 152., 157., 162., 162., 162., 162.,
     T   45.,  45., 123., 175., 182., 189., 195., 162., 162., 162.,
     L   46.,  46., 155., 230., 236., 247., 256., 256., 256., 256./

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

C  EACH TIME THROUGH, COMPUTE LOGS OF PRESSURE RATIOS
C  --------------------------------------------------

      ALP(1) = 0.0
      DO I=2,16
         ALP(I) = ALOG(PRES(I-1)/PRES(I))
      ENDDO

C  GET TABLE INDEX FROM RAOB TYPE - CHECK FOR VALID CORR. PARAMETERS
C  (GEOPOTENTIAL CORRECTION TABLE JTAB ONLY VALID PRIOR TO 00Z 10/1/93)
C  --------------------------------------------------------------------

      ITAB = 0
      JTAB = 0
      IF(JTYPE.GT.0.AND.JTYPE.LT.256) THEN
         ITAB = ITYPTT(JTYPE)
         IF(ITAB.EQ.0)  THEN
            IRET = -1
            RETURN
         ELSE  IF(ITAB.EQ.5)  THEN
            IF(SID(IS)(1:1).EQ.'W'.OR.SID(IS)(1:2).EQ.'70'.OR.
     &       SID(IS)(1:2).EQ.'72'.OR.SID(IS)(1:2).EQ.'74'.OR.
     &       SID(IS).EQ.'89009   '.OR.SID(IS).EQ.'89664   '.OR.
     &       SID(IS).EQ.'91762   '.OR.SID(IS).EQ.'91765   '.OR.
     &       (SID(IS)(1:5).GT.'91065'.AND.SID(IS)(1:5).LT.'91191').OR.
     &       (SID(IS)(1:5).GT.'91284'.AND.SID(IS)(1:5).LT.'91357').OR.
     &       (SID(IS)(1:5).GT.'91407'.AND.SID(IS)(1:5).LT.'91435'))
     &        ITAB =11
         ELSE  IF(ITAB.LT.0)  THEN
            WRITE(6,1)  SID(IS),JTYPE
            WRITE(68,1) SID(IS),JTYPE
            IRET = -1
            RETURN
         END IF
         IF(ITAB.EQ.11)  THEN
            PRINT *, '##### A U.S. VAISALA AT STNID :',SID(IS)
            WRITE(68,'("##### A U.S. VAISALA AT STNID :",A)') SID(IS)
         END IF
         IF(HGTTBL) JTAB = ITYPTG(JTYPE)
      ELSE
         WRITE(6,1)  SID(IS),JTYPE
         WRITE(68,1) SID(IS),JTYPE
1        FORMAT(5X,'* * *  STN. ID ',A8,' HAS AN INVALID RAOB TYPE ',
     .    'DESIGNATION (BUFR/ON29 CODE ',I8,') - NO CORRECTION ',
     .    'POSSIBLE')
         IRET = -1
         if(jtype.eq.20000)  iret = -99
         RETURN
      ENDIF

C  GET SOLAR ELEVATION ANGLE ON ALL VALID MANDATORY LEVELS
C  -------------------------------------------------------

      CALL SOELAN(SOLAR)

C  APPLY TEMPERATURE AND HEIGHT CORRECTIONS
C  ----------------------------------------

      DO ILEV=1,16
         ANGLE(2)  = CUTOFF(ILEV)
         DTS(ILEV) = 0
         DHS(ILEV) = 0
         DTL       = 0
         DHL       = 0

         IF(SOLAR(ILEV).GE.CUTOFF(ILEV))  THEN
            IF(SOLAR(ILEV).LT.ANGLE(10)) THEN
               DO K=3,10
                  IF(SOLAR(ILEV).LT.ANGLE(K)) THEN
                     TABU = TTAB(K  ,ILEV,ITAB)
                     TABD = TTAB(K-1,ILEV,ITAB)
                     DSUN=(SOLAR(ILEV)-ANGLE(K-1))/(ANGLE(K)-ANGLE(K-1))
                     DTS(ILEV) = TABD + DSUN*(TABU-TABD)
                     IF(JTAB.GT.0)  THEN
                        TABU = ZTAB(K  ,ILEV,JTAB)
                        TABD = ZTAB(K-1,ILEV,JTAB)
                        DHS(ILEV) = TABD + DSUN*(TABU-TABD)
                     ENDIF
                     GO TO 10
                  ENDIF
               ENDDO
            ELSE
               DTS(ILEV) = TTAB(10,ILEV,ITAB)
               IF(JTAB.GT.0)  DHS(ILEV) = ZTAB(10,ILEV,JTAB)
            ENDIF
         ELSE

C  IF ANGLE < CUTOFF (NIGHT), CORRECTION FROM 'NIGHT' VALUE IN TBL
C  --------------------------------------------------------------------

            DTS(ILEV) = TTAB(1,ILEV,ITAB)
            IF(JTAB.GT.0)  DHS(ILEV) = ZTAB(1,ILEV,JTAB)
         ENDIF

C  IF JTAB IS ZERO, COMPUTE HEIGHT CORRECTIONS VIA HYDROSTATIC INTERP.
C  -------------------------------------------------------------------

10       IF(JTAB.LE.0)  THEN

C  The constant is (R/(20*g)) = 1.46355

            HYF = 1.46355*ALP(ILEV)
            IF(ILEV.EQ.1) THEN
               DHS(ILEV) = HYF*DTS(ILEV)
            ELSE
               DHS(ILEV) = DHS(ILEV-1) + HYF*(10.*DTS(ILEV-1)+DTS(ILEV))
            ENDIF
         ENDIF

C  CORRECT DTS BY A FACTOR OF 10
C  -----------------------------

         DTS(ILEV) = .1*DTS(ILEV)

C  SAVE AND APPLY THE CORRECTIONS
C  ------------------------------

         DHT(ILEV) = DHL - DHS(ILEV)
         DTP(ILEV) = DTL - DTS(ILEV)

         IF(HGT(ILEV).LT.BMISS) HGT(ILEV) = HGT(ILEV) + DHT(ILEV)
         IF(TMP(ILEV).LT.BMISS) TMP(ILEV) = TMP(ILEV) + DTP(ILEV)

      ENDDO

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RADT4       APPLIES NEWEST SET OF RADIATION CORRECTNS
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 2007-09-14
C
C ABSTRACT: THIS SUBROUTINE APPLIES RADIATION CORRECTIONS TO THE
C   OBSERVED RADIOSONDE HEIGHTS AND TEMPERATURES ON THE BOTTOM 16
C   MANDATORY PRESSURE LEVELS.  DATA ARE PASSED THROUGH COMMON
C   /RADCOM/.  THE CORRECTION OF RAOBS FOR BOTH SHORT- AND LONG-WAVE
C   EFFECTS IN ACCOMPLISHED BY READING IN TABLES OF NUMBERS WHICH ARE
C   DEPENDENT ON THE RADIOSONDE TYPE. CALLED FOR ONE REPORT AT A TIME.
C
C PROGRAM HISTORY LOG:
C 1999-06-02  W. COLLINS (W/NP22) -- ORIGINAL AUTHOR
C 2002-07-22  D. A. KEYSER -- CALLS NEW ROUTINE SOELAN TO CALCULATE SUN
C     ANGLE ON VALID MANDATORY LEVELS RATHER THAN DOING CALCULATION IN
C     THIS SUBROUTINE
C 2007-09-14  D. KEYSER    -- MINOR CODE CORRECTIONS SUGGESTED BY PAT
C     PAULEY (CORRECTS PROBLEMS THAT COULD OCCUR ON SOME REMOTE
C     MACHINES)
C
C USAGE:    CALL RADT4(IRET)
C   OUTPUT ARGUMENT LIST:
C     IRET     - RETURN CODE (0=CORRECTIONS, -1=NO CORRECTIONS,
C              - -99=??????)
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 68  - RADCOR INFORMATION FILE
C
C REMARKS: THESE CORRECTIONS USE THE BUFR CODE FIGURE FOR RADIOSONDE
C   TYPE OBTAINED FROM BUFR-FM94 TABLE 0 02 011.  THE NMC UPPER-AIR
C   DICTIONARY ALSO CONTAINS THIS BUFR CODE FIGURE FOR RADIOSONDE
C   TYPE.  CALLED BY SUBROUTINE "RADEVN".
C
C
C   KEY FOR RADIOSONDE TYPES USED HERE:
C
C  JTYPE   (   * =     )   DESCRIPTION                      TABLE NUMBER
C          (in use 1998)
C  < 0                     INVALID
C  0 TO 6                  NOT USED FOR SONDES
C  7 TO 8                  RESERVED
C    9          *          UNKNOWN TYPE (Don't adjust)            1
C   10          *          (U.S.) NOAA / VIZ TYPE  A              2
C   11          *          (U.S.) NOAA / VIZ TYPE  B              3
C   12          *          (U.S.) NOAA / SPACE DATA CORP.         4
C   13                     (AUSTRALIA) ASTOR
C   14                     (U.S.) VIZ MARK I MICROSONDE
C   15                     (U.S.) EEC COMPANY TYPE  23
C   16                     (AUSTRIA) ELIN
C   17                     (GERMANY) GRAW  G
C   18                     RESERVED
C   19          *          (GERMANY) GRAW  M60                    5
C   20          *          INDIAN MET. SERVICE TYPE  MK3          6
C   21          *          (S. KOREA) VIZ/JIN YANG MARK I MICROSONDE 7
C   22          *          (JAPAN) MEISEI  RS2-80                 8
C   23                     (FRANCE) MENSURAL FMO  1950A
C   24                     (FRANCE) MENSURAL FMO  1945A
C   25                     (FRANCE) MENSURAL  MH73A
C   26                     (SWITZERLAND) METEOLABOR BASORA
C   27          *          (RUSSIA) AVK-MRZ                       9
C   28          *          (RUSSIA) METEORIT  MARZ2-1            10
C   29          *          (RUSSIA) METEORIT  MARZ2-2            11
C   30                     (JAPAN) OKI  RS2-80
C   31          *          (CANADA) VIZ/VALCOM TYPE A            12
C   32          *          (CHINA) SHANGHAI RADIO                13
C   33                     (U.K.) UK MET OFFICE  MK3    
C   34                     (CZECHOSLOVAKIA) VINOHRADY
C   35                     VAISALA  RS18
C   36                     VAISALA  RS21
C   37          *          VAISALA  RS80                         14
C   38                     (U.S.) VIZ LOCATE (LORAN-C)
C   39                     (GERMANY) SPRENGER  E076
C   40                     (GERMANY) SPRENGER  E084
C   41                     (GERMANY) SPRENGER  E085
C   42                     (GERMANY) SPRENGER  E086
C   43          *          (U.S.) AIR IS - 4A - 1680             15
C   44          *          (U.S.) AIR IS - 4A - 1680 X           15 
C   45          *          (U.S.) RS MSS                         16 
C   46                     (U.S.) AIR IS - 4A - 403
C   47          *          (JAPAN) MEISEI  RS2-91                17 
C   48                     (CANADA) VALCOM
C   49          *          (U.S.) VIZ MARK II                    18
C   50          *          (GERMANY) GRAW DFM-90                 19
C   51          *          (U.S.) NOAA / VIZ TYPE  B2            20
C   52          *          VAISALA  RS80/MICROARTS               21
C 53 TO 59                 RESERVED FOR ADDITIONAL SONDES
C   60          *          VAISALA  RS80/MICROCORA               22
C   61          *          VAISALA  RS80/DIGICORA  OR MARWIN     23
C   62          *          VAISALA  RS80/PCCORA                  24
C   63          *          VAISALA  RS80/STAR                    25
C   64                     (U.S.) ORBITAL SCI. CORP TYPE 909-11-xx
C   65                     (U.S.) VIZ/TRANSPONDER TYPE 1499-520
C 66 TO 70                 RESERVED FOR ADDITIONAL AUTOMATED SYSTEMS
C   71        (1999)       VAISALA RS90/DIGICORA OR MARWIN
C   72                     VAISALA RS90/PCCORA
C   73                     VAISALA RS90/AUTOSONDE
C   74                     VAISALA RS90/STAR
C 75 TO 89                 RESERVED FOR ADDITIONAL AUTOMATED SYSTEMS
C   90          *          UNKNOWN TYPE (Don't correct)          26
C   91                     PRESSURE ONLY
C   92          *          PRESSURE ONLY PLUS TRANSPONDER No corr 27
C   93                     PRESSURE ONLY PLUS RADAR REFLECTOR
C   94                     NO PRESSURE PLUS TRANSPONDER
C   95                     NO PRESSURE PLUS RADAR REFLECTOR
C   96                     DESCENDING
C 97 TO 254                RESERVED
C  255                     MISSING VALUE
C >255                     INVALID
C
C
C   KEY FOR LEVELS IN DATA (CORRECTION) TABLES:
C
C     1 - 1000 MB
C     2 -- 925 MB
C     3 -- 850 MB
C     4 -- 700 MB
C     5 -- 500 MB
C     6 -- 400 MB
C     7 -- 300 MB
C     8 -- 250 MB
C     9 -- 200 MB
C     A -- 150 MB
C     B -- 100 MB
C     C --  70 MB
C     D --  50 MB
C     E --  30 MB
C     F --  20 MB
C     G --  10 MB
C
C
C   THE RADIATION CORRECTION TABLES ARE BASED UPON
C   OBSERVED INCREMENTS FROM NWP ASSIMILATION.  THE ADJUSTMENTS
C   ARE TO MAKE ALL SONDE TYPES COMPARE WITH THE MEAN VAISALA
C   RS80 SONDE.
C
C
C   THE CUTOFF ANGLE AT EACH LEVEL CALCULATED USING THE RELATION:
C              CUTOFF ANGLE = -1.76459 * (Z**.40795) ,
C   WHERE Z IS A REFERENCE HEIGHT FOR THE LEVEL (IN KILOMETERS)
C   (SEE NMC OFFICE NOTE 306, TABLE 4 FOR LIST OF REFERENCE HEIGHTS.)
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE RADT4(IRET)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /RADCOM/ HGT(16),TMP(16),DHT(16),DTP(16),JTYPE
      COMMON /SWITCH/ LWCORR,LEVRAD,IRCTBL,HGTTBL,BAL_DRIFT
      COMMON /PMAND / PRES(16),KMIN,KMAX,INM(16)
      COMMON /STN/     IS
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      REAL        TTAB(5,16,27),CUTOFF(16),
     .            SOLAR(16),ANGLE(5),ALP(16),DTS(16),DHS(16),
     .            USVIZA(5,16), USVIZB(5,16), USSDCP(5,16),
     .            GRAW60(5,16), INDMK3(5,16), KORMK1(5,16),
     .            MEIS80(5,16), RUSAVK(5,16), RUSZ21(5,16),
     .            RUSZ22(5,16), CANVZA(5,16), CHINSR(5,16),
     .            VARS80(5,16), AIRR80(5,16), USRSMS(5,16),
     .            MEIS91(5,16), VIZMK2(5,16), GRAW90(5,16),
     .            USVZB2(5,16), MICROA(5,16), MICORA(5,16),
     .            DICORA(5,16), PCCORA(5,16), VASTAR(5,16),
     .            UNKN90(5,16), UNKN09(5,16), TYPE92(5,16)
      INTEGER     ITYPTT(255)
      EQUIVALENCE (TTAB(1,1, 1),UNKN09(1,1)),(TTAB(1,1, 2),USVIZA(1,1)),
     .            (TTAB(1,1, 3),USVIZB(1,1)),(TTAB(1,1, 4),USSDCP(1,1)),
     .            (TTAB(1,1, 5),GRAW60(1,1)),(TTAB(1,1, 6),INDMK3(1,1)),
     .            (TTAB(1,1, 7),KORMK1(1,1)),(TTAB(1,1, 8),MEIS80(1,1)),
     .            (TTAB(1,1, 9),RUSAVK(1,1)),(TTAB(1,1,10),RUSZ21(1,1)),
     .            (TTAB(1,1,11),RUSZ22(1,1)),(TTAB(1,1,12),CANVZA(1,1)),
     .            (TTAB(1,1,13),CHINSR(1,1)),(TTAB(1,1,14),VARS80(1,1)),
     .            (TTAB(1,1,15),AIRR80(1,1)),(TTAB(1,1,16),USRSMS(1,1)),
     .            (TTAB(1,1,17),MEIS91(1,1)),(TTAB(1,1,18),VIZMK2(1,1)),
     .            (TTAB(1,1,19),GRAW90(1,1)),(TTAB(1,1,20),USVZB2(1,1)),
     .            (TTAB(1,1,21),MICROA(1,1)),(TTAB(1,1,22),MICORA(1,1)),
     .            (TTAB(1,1,23),DICORA(1,1)),(TTAB(1,1,24),PCCORA(1,1)),
     .            (TTAB(1,1,25),VASTAR(1,1)),(TTAB(1,1,26),UNKN90(1,1)),
     .            (TTAB(1,1,27),TYPE92(1,1))
      LOGICAL     HGTTBL
      CHARACTER*8 SID

      DATA  ANGLE  / -90, -10, 15, 30, 60/
      DATA  CUTOFF / -.73, -1.58, -2.06, -2.77,
     .              -3.56, -3.95, -4.36, -4.58,
     .              -4.83, -5.12, -5.49, -5.79,
     .              -6.06, -6.43, -6.72, -7.17/

C  ITYPTT ASSIGNS PROPER TEMPERATURE CORRECTION TABLE TO INST. TYPES
C  -----------------------------------------------------------------

C  ===> JTYPE:      1  2  3  4  5  6  7  8  9 10 11 12 13 14 15

      DATA  ITYPTT/-1,-1,-1,-1,-1,-1,-1,-1,-1, 2, 3, 4, 0, 0, 0,

C  ===> JTYPE:     16 17 18 19 20 21 22 23 24 25 26 27 28 29 30

     $              0, 0,-1, 5, 6, 7, 8, 0, 0, 0, 0, 9,10,11, 0,

C  ===> JTYPE:     31 32 33 34 35 36 37 38 39 40 41 42 43 44 45

     $             12,13, 0, 0, 0, 0,14, 0, 0, 0, 0, 0,15,15,16,

C  ===> JTYPE:     46 47 48 49 50 51 52 53 54 55 56 57 58 59 60

     $              0,17, 0,18,19,20,21,-1,-1,-1,-1,-1,-1,-1,22,

C  ===> JTYPE:     61 62 63 64 65 66-89 90-96 97 98 99 100-255

     $             23,24,25, 0, 0,24*-1, 7*0, -1,-1,-1, 156*0/


C  TEMPERATURE CORRECTION TABLES ( * 10 K) FOR RADIATIVE EFFECTS
C  -------------------------------------------------------------

C          UNKNOWN TYPE                        
C     ====> JTYPE =  9  --  ITAB = 1  <====
 
      DATA  UNKN09  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -5.8, -7.1,-14.9,-13.6, -4.7,
     2  -2.9,  -.7, -8.2, -5.1,-11.1,
     3   1.0, -1.1, -2.3,   .0, -1.3,
     4    .3,  1.5,  -.9,   .3,  2.4,
     5   -.2,  3.7,  1.4,  1.8,  3.7,
     6   -.5,  3.2,  2.2,  1.2,  3.2,
     7   1.3,  3.0,  5.0,  4.5,  1.3,
     8   2.3,  5.6,  6.3,  6.3,  5.2,
     9    .1,  2.4,   .8,  1.5,  1.7,
     A   -.1,  1.1, -3.4, -3.6, -2.4,
     B   -.3,  1.8, -5.6, -5.6, -1.8,
     C  -3.6, -1.1,   .5,   .0, -8.0,
     D  -2.3,  2.6,  7.3,  6.2,   .5,
     E    .0,   .4, 13.8, 15.0, 11.3,
     F   -.4,  3.4, 14.8, 14.8, 14.8,
     G -13.4,  -.6, -7.7, -5.7, -5.7/
 
 
C          (U.S.) NOAA / VIZ TYPE  A           
C     ====> JTYPE = 10  --  ITAB = 2  <====
 
      DATA  USVIZA  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -8.0,-11.6,-14.3,-11.6,   .6,
     2  -4.0, -8.5, -9.1, -3.3,  3.0,
     3  -1.9,  -.4, -3.6,  2.3, -4.5,
     4  -2.1,  3.4,  -.2,  1.8, -1.1,
     5  -2.6,   .0, -2.2,  1.4,  1.7,
     6  -3.9,  1.5,  -.2,   .8, -1.8,
     7  -1.1,  4.0,  2.8,  3.1,  1.1,
     8    .6,  6.2,  3.6,  4.1,  2.0,
     9  -1.7,   .6, -5.0,  4.8, -1.1,
     A  -5.6, -3.5,-10.8,  5.7, -1.2,
     B  -4.7, -2.7, -7.6,  2.8, 14.1,
     C  -4.2, -1.6,  3.3,  9.4, 13.7,
     D   2.1,  6.3, 18.9, 14.0, 21.9,
     E   7.6,  4.8, 31.3, 22.4, 16.4,
     F  10.5, 12.1, 31.2, 18.0, 18.3,
     G  -1.9, -8.9, 21.7, -2.3,  9.5/
 
 
C          (U.S.) NOAA / VIZ TYPE  B           
C     ====> JTYPE = 11  --  ITAB = 3  <====
 
      DATA  USVIZB  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1 -10.5, -3.7, -2.7,  3.4,  1.5,
     2  -6.6, -4.7, -2.1,  4.0,  2.5,
     3  -2.8,  -.4,  -.1,  3.7,  -.4,
     4  -3.0, -1.3,  -.2,   .9,  1.0,
     5  -3.9,  -.7,  1.0,  1.9, -2.1,
     6  -4.0,   .6,  1.7,  1.5, -2.3,
     7  -2.2,  2.0,  3.8,  2.5,  4.8,
     8  -3.0,  2.5,  5.1,  3.4,  3.8,
     9  -5.7,   .7,  3.9,  5.4,  1.4,
     A -10.7,  1.4,  4.7,  5.8,  2.3,
     B  -6.8,  1.4,  9.6, 13.2, 10.3,
     C  -4.0,  -.6,  4.3, 12.4, 16.6,
     D   1.4,   .3,  9.6, 15.8, 21.6,
     E  -1.6,  1.2, 16.5, 22.6, 20.8,
     F  -5.5,  5.3, 17.4, 22.5, 20.4,
     G -17.4, -1.8, 13.3, 15.9,  6.8/
 
 
C          (U.S.) NOAA / SPACE DATA CORP.      
C     ====> JTYPE = 12  --  ITAB = 4  <====
 
      DATA  USSDCP  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1 -13.8,-17.0, -8.2, -9.2, -9.2,
     2  -8.1,  2.8,  -.2,  6.6,  6.6,
     3  -4.1,  3.4,  4.4,  4.4,  4.4,
     4  -2.1,  1.1,  -.6,   .4,   .4,
     5  -2.1, -1.9,  1.4,  -.2,  -.2,
     6  -4.6, -1.7,  -.4,  1.8,  1.8,
     7  -4.3,  1.3,  2.9,  2.7,  2.7,
     8  -3.8,  1.0,  2.0,  3.4,  3.4,
     9  -6.3,  1.8,  8.5,  7.9,  7.9,
     A  -6.2,  5.2, 14.2, 16.7, 16.7,
     B  -5.5,  5.8, 11.4, 13.4, 13.4,
     C   1.4,  2.4,  3.8,  4.8,  4.8,
     D   4.8,  3.2,   .5, -1.3, -1.3,
     E   7.9,  5.1,  3.9,  9.0,  9.0,
     F  17.6,  4.6,  2.8, 13.7, 13.7,
     G   5.8, 18.8, 54.2, 17.3, 17.3/
 
 
C          (GERMANY) GRAW  M60 (one site)      
C     ====> JTYPE = 19  --  ITAB = 5  <====
 
      DATA  GRAW60  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  10.3,  3.2,  6.2, -7.0, 20.0,
     2   -.8,  2.8,  1.3,  -.3,  1.8,
     3   -.6,  9.4,  1.5,  3.2,   .7,
     4   -.3,  5.2,   .7,  5.7,-10.3,
     5   -.3, -2.1,   .7,  4.8, -1.7,
     6  -1.1, -5.7,  -.5,  4.5,  -.3,
     7  -5.0,-11.3, -2.4,-10.1, -3.9,
     8  -4.0, -5.9,  -.4,  3.3,  1.8,
     9   2.9,  2.5, 12.3, 11.4,  8.0,
     A   3.8,  6.7,  9.5, 10.4, 15.2,
     B   8.2,  7.3,  8.8,  7.4, 11.6,
     C   4.6,  7.0,  6.3,  6.0,  3.2,
     D    .7,  2.6,  1.5,   .2, -6.1,
     E  -3.7, -4.6, -7.5, -3.4, -4.9,
     F  -7.0, -1.0, -8.9,-22.6, -2.7,
     G  -5.9,-15.1,-10.1, 20.6, 10.3/
 
 
C          INDIAN MET. SERVICE TYPE  MK3       
C     ====> JTYPE = 20  --  ITAB = 6  <====
 
      DATA  INDMK3  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1 -16.8,-14.7, -6.6,  1.9,  1.9,
     2 -10.4, -9.3, -6.1, -7.1, -7.1,
     3   1.5,  1.6,  4.0,  3.0,  3.0,
     4   5.4,  4.9,  8.2, 11.9, 11.9,
     5  -6.8, -1.2,  2.5,  6.7,  6.7,
     6  -7.4,  -.8,  6.4,  8.7,  8.7,
     7  -7.6,  1.6,  8.3, 21.1, 21.1,
     8  -7.6,  3.2, 11.7, 25.6, 25.6,
     9 -15.5, -4.5,  6.4, 17.9, 17.9,
     A -22.0, -8.9,  2.6, 13.1, 13.1,
     B -13.4, -2.2,  5.9,  4.5,  4.5,
     C  -6.2,  1.1,  3.2,  3.2,  3.2,
     D   -.3, 11.8, 15.3, 15.3, 15.3,
     E   -.9, 13.1, 14.1, 14.1, 14.1,
     F  -1.4,  7.4,  8.7,  8.7,  8.7,
     G   -.5, -3.0,   .6,   .6,   .6/
 
 
C          (S. KOREA) VIZ/JIN YANG MARK I MICRO
C     ====> JTYPE = 21  --  ITAB = 7  <====
 
      DATA  KORMK1  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -5.5, -7.2, -7.2,  -.6,  -.6,
     2  -4.8,  6.1,  6.1, -2.2, -2.2,
     3  -6.9,  4.3,  4.3, -4.6, -4.6,
     4  -5.5,  1.2,  1.2, -3.0, -3.0,
     5  -3.8,  1.4,  1.4,   .4,   .4,
     6  -1.6,  4.5,  4.5,  3.7,  3.7,
     7   3.1, 15.3, 11.0,  9.5,  9.5,
     8   4.1, 21.2, 13.2,  9.8,  9.8,
     9  -1.6, 15.1, 15.1,  5.3,  5.3,
     A  -8.2,  2.8,  8.1,  3.4,  3.4,
     B  -8.7,  -.6, -1.0,  7.3,  7.3,
     C  -4.6,  6.5,  5.1,  5.8,  5.8,
     D   -.9,  2.5,  9.4, 11.1, 11.1,
     E   6.8, 19.6, 14.7, 17.3, 17.3,
     F   9.9, 26.6, 26.0, 20.6, 20.6,
     G   2.1, 23.7, 15.3, 19.3, 19.4/
 
 
C          (JAPAN) MEISEI  RS2-80              
C     ====> JTYPE = 22  --  ITAB = 8  <====
 
      DATA  MEIS80  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1 -19.3,-24.5,-25.5,-20.3, -4.5,
     2 -16.3, 12.7,-10.3,-10.2, -6.8,
     3 -10.1,  3.6, -6.3, -6.1,   .9,
     4  -7.5,  5.8, -7.3, -3.6,  -.7,
     5  -3.2, -2.3,  1.8, -1.4, -2.6,
     6  -1.8,   .7,  1.6,  1.9, -2.3,
     7  -1.3,-19.2,  2.1,  1.6,  1.4,
     8  -1.6, -9.0,  6.0,  2.9, -1.8,
     9  -6.4, -2.8,  2.8,   .0,  -.2,
     A -10.9,  2.4,   .2,  -.9, -5.2,
     B  -6.6,  1.0,  6.8,  8.8,  4.3,
     C  -6.9,  1.8,  6.8,  5.5,  5.0,
     D   6.0, -5.0, 19.2, 12.9,  8.7,
     E  12.2, -3.9, 17.7, 11.8,  8.1,
     F   8.5, -2.5, 11.0, 11.8,  2.2,
     G -11.9,-12.4, 27.1, 22.7, -6.2/
 
 
C          (RUSSIA) AVK-MRZ                    
C     ====> JTYPE = 27  --  ITAB = 9  <====
 
      DATA  RUSAVK  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   9.9,  8.8, 12.7, 10.7, 10.7,
     2   4.9,  3.3,  4.9,  5.1,  5.1,
     3   5.5,  5.1,  3.6,  2.6,  2.6,
     4   1.1,  1.3,  1.4,  -.3,  -.3,
     5   1.5,  2.9,  5.9,  5.2,  2.2,
     6   3.4,  4.3,  7.9,  8.2,  9.2,
     7   4.7,  5.5,  9.7,  9.6,  7.1,
     8   3.5,  4.2,  8.8,  9.7,  7.0,
     9   2.7,  5.5, 12.5, 15.6, 15.9,
     A   3.2,  9.8, 16.2, 17.3, 26.1,
     B   4.1,  9.7, 16.1, 17.5, 23.4,
     C    .8, 10.4, 15.6, 13.2, 13.2,
     D  -4.3,  5.8, 10.7,  7.9,  7.9,
     E  -4.9,  5.5,  9.9,  7.4,  7.4,
     F  -3.2,  4.0,  7.5,  3.5,  3.5,
     G  -6.7,  5.9, 10.8,  5.7,  5.7/
 
 
C          (RUSSIA) METEORIT  MARZ2-1          
C     ====> JTYPE = 28  --  ITAB = 10 <====
 
      DATA  RUSZ21  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  10.8,   .2, 22.5,  -.8,  -.8,
     2   5.6,  3.9,  8.8,  6.0,  6.0,
     3   5.3,  5.1,  6.0,  4.1,  4.1,
     4   3.3,  3.0,  1.3,  2.7,  2.7,
     5   4.0,  6.0,  5.9,  5.1,  5.1,
     6   5.7,  7.1,  7.8,  6.6,  6.6,
     7   5.6,  6.6,  8.8,  6.1,  6.1,
     8   2.7,  5.0,  9.1,  6.1,  6.1,
     9   1.6,  5.0, 14.0, 17.7, 17.7,
     A   2.8, 10.5, 16.0, 20.1, 20.1,
     B   5.5, 10.3, 14.2, 20.5, 20.5,
     C   4.0, 10.9, 14.7, 18.0, 18.0,
     D   -.9,  6.0, 10.8, 10.8, 10.8,
     E  -4.1,  3.7, 11.0,  7.0,  7.0,
     F  -3.8,  3.9, 11.6,  4.1,  4.1,
     G   2.7,  6.9, 15.0,  1.9,  1.9/
 
 
C          (RUSSIA) METEORIT  MARZ2-2          
C     ====> JTYPE = 29  --  ITAB = 11 <====
 
      DATA  RUSZ22  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   9.2,  8.7, 11.0,  8.1,  8.1,
     2  10.3,  4.2,  3.7,  6.0,  6.0,
     3   8.0,  6.1,  6.0,  3.2,  3.2,
     4   4.6,  4.5,  5.4,  2.6,  2.6,
     5  -1.1,  3.4,  6.9,  8.8,  8.8,
     6  -3.5,  1.4,  5.7, 11.2, 11.2,
     7  -7.3, -3.1, -1.3, 10.7, 10.7,
     8  -3.2,  2.0,  4.2, 10.7, 10.7,
     9   3.3, 11.4, 17.7, 15.3, 15.3,
     A   5.4, 11.7, 16.5, 19.5, 19.5,
     B   4.8,  9.8, 14.9, 17.4, 17.4,
     C   2.7, 11.9, 16.4, 14.7, 14.7,
     D  -4.1,  7.5, 12.8, 10.8, 10.8,
     E  -7.7, 10.2, 12.0, 10.6, 10.6,
     F  -3.9,  9.2,  8.8,  2.3,  2.3,
     G -17.6,  9.4,  7.7,  1.8,  1.8/
 
 
C          (CANADA) VIZ/VALCOM TYPE A          
C     ====> JTYPE = 31  --  ITAB = 12 <====
 
      DATA  CANVZA  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  22.7, 10.5, 13.3, 30.0, 30.0,
     2   9.4,   .9,  7.4, 12.6, 12.6,
     3   8.0,  2.2,  6.2, 13.5, 13.5,
     4   1.1, -1.5, -2.9,  1.5,  1.5,
     5    .4,  1.6,  2.0,  3.0,  3.0,
     6  -2.7,  1.1,   .0,  1.8,  1.8,
     7   1.1, -1.7,  2.6,  3.2,  3.2,
     8    .2, -1.2,   .7, -1.4, -1.4,
     9   2.7,  2.4,  7.2,  7.9,  7.9,
     A  -3.4,  1.0,  7.3,  2.7,  2.7,
     B   4.7,  6.1, 11.2, 16.0, 16.0,
     C   2.4,  6.1,  8.8,  5.8,  5.8,
     D   3.4,  1.5,  3.6,  -.8,  -.8,
     E   7.1,  2.7,  1.5,  2.9,  2.9,
     F   7.8, -1.0, -5.1, -3.9, -3.9,
     G   6.2, -2.2, -4.2,  1.6,  1.6/
 
 
C          (CHINA) SHANGHAI RADIO              
C     ====> JTYPE = 32  --  ITAB = 13 <====
 
      DATA  CHINSR  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   -.3,  4.0, -3.7, -9.5, -9.5,
     2   -.2, -1.7, -7.8, -7.8, -7.8,
     3   3.3,  -.2, -4.5, -3.4, -3.4,
     4   2.7,  -.1,  -.7,   .5,   .5,
     5   4.0,  5.6,  7.6,  9.8,  9.8,
     6   4.8,  9.9, 13.3, 14.6, 14.6,
     7   7.9, 10.9, 13.1, 15.2, 15.2,
     8   8.3,  9.8, 13.9, 16.3, 16.3,
     9   7.9,  5.6, 10.3,  9.0,  9.0,
     A   9.2,  8.8, 11.1, 10.3, 10.3,
     B   3.2,  4.8,  6.3, 11.2, 11.2,
     C  -7.0, -2.1, -8.1, -4.1, -4.1,
     D  -6.1, -1.8, -7.1, -4.9, -4.9,
     E  -2.8,  2.4,  1.9,   .9,   .9,
     F  -3.1,  6.2,  6.5, -1.8, -1.8,
     G -15.8, -9.5,-15.3,-23.8,-23.8/
 
 
C          VAISALA  RS80                       
C     ====> JTYPE = 37  --  ITAB = 14 <====
 
      DATA  VARS80  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -8.9,-14.7, -3.9, -3.2, -2.3,
     2  -2.0, -3.3, -1.9, -1.2,   .3,
     3  -1.7, -1.3, -2.0, -1.5,  1.8,
     4    .7,  1.0,   .5,   .4,  2.8,
     5   -.4,   .7,   .9,   .3,  2.1,
     6   -.8,  1.3,   .8,   .2,  2.5,
     7  -1.1,  -.3,   .7,  1.6,   .6,
     8   -.8,   .2,   .4,  1.2,  1.3,
     9  -2.1, -2.5, -2.1, -2.6,  1.1,
     A  -2.3, -2.9, -3.9, -3.4, -1.9,
     B  -5.6, -7.2, -7.7, -5.4, -4.6,
     C  -2.1,  -.3, -1.8, -2.1, -3.6,
     D   1.3,   .4,  2.9,  3.4,   .8,
     E   4.1,   .9,  1.1,  5.2,  7.2,
     F   7.7,  2.7,  7.6, 10.7, 19.1,
     G   4.0,  4.5,  -.6,   .4, 13.5/
 
 
C          (U.S.) AIR IS - 4A - 1680           
C          (U.S.) AIR IS - 4A - 1680 X         
C     ====> JTYPE = 43  --  ITAB = 15 <====
 
      DATA  AIRR80  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   3.4, 15.5,-11.5,   .0,  4.8,
     2  -6.0, 18.1, -6.4, -5.5,   .4,
     3   -.6, 15.7,   .1,   .4,   .2,
     4  -2.6, 12.8,  6.1,  3.1, -1.4,
     5    .1,  7.5,  8.0,  8.6,  2.2,
     6   -.6,  4.9,  9.2, 10.9,  2.9,
     7    .8, -4.6,   .8, 15.3,  3.3,
     8    .1,  3.7,  9.5, 18.9,  4.4,
     9  -4.7,  7.6, 16.8, 18.0,  6.7,
     A -10.3,  6.9,  9.3, 17.1,  2.2,
     B  -3.9,  3.7, 14.5, 25.5, 27.8,
     C  -6.4,  6.1, 21.2, 29.5, 10.1,
     D  10.6,  8.7, 25.9, 37.5, 36.6,
     E  19.7, 17.0, 21.6, 45.4, 27.2,
     F  32.8, 12.3, 15.3, 44.6, 51.7,
     G   2.7,  2.7,  6.2, 67.8, 67.8/
 
 
C          (U.S.) RS MSS                       
C     ====> JTYPE = 45  --  ITAB = 16 <====
 
      DATA  USRSMS  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -9.6,-13.9, -9.1,   .3,  2.8,
     2  -7.0, -5.5,   .0,  4.6,  3.9,
     3  -3.7, -2.9,  -.2,   .9,  1.5,
     4   -.2, -1.1,  -.9,   .4,  1.3,
     5  -2.9, -1.8,   .0,   .9, -2.3,
     6  -5.3, -3.3,  -.1,  -.6, -3.0,
     7  -4.1, -1.7,  3.4,  -.2, -1.7,
     8  -6.0, -1.9,  3.1,  1.4, -2.7,
     9 -10.6, -5.9,  -.7, -3.7, -4.5,
     A -13.7, -6.9,  -.6, -6.1, -5.6,
     B  -9.0, -3.7,   .1,  -.8,   .7,
     C   -.6, -2.6, -4.5,  4.6,  9.8,
     D   5.3,  2.7,  2.8, 12.0, 11.9,
     E  10.4,  7.4,  7.6, 18.7, 11.5,
     F  10.3, 12.9, 11.6, 18.7, 12.8,
     G   7.6, 24.5, 49.6,  6.4,  7.7/
 
 
C          (JAPAN) MEISEI  RS2-91              
C     ====> JTYPE = 47  --  ITAB = 17 <====
 
      DATA  MEIS91  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1 -10.8,  -.7,  -.7, -5.3, -6.0,
     2  -9.9,  5.5,  5.5, -4.4, -7.2,
     3  -4.7,  6.8,  6.8, -4.7, -4.7,
     4  -4.7, -2.9, -2.9, -4.1,   .2,
     5  -1.5, -1.3, -1.3, -1.3, -2.1,
     6   1.9,   .5,   .5,  2.4,   .9,
     7   5.3,  4.4,  4.4,  5.8,  4.6,
     8   5.2,  8.7,  8.7,  5.2,  3.8,
     9    .8,  6.9,  6.9,   .3,  2.9,
     A  -7.3,  -.7,  -.7, -1.4, -4.9,
     B  -7.5, -3.6, -3.6,  2.0,  8.5,
     C -10.5, -5.9, -5.9, -5.3,  2.1,
     D  -2.9,  -.3,  -.3,  2.1,  3.0,
     E   3.6,  5.0,  5.0,  5.0,  2.8,
     F   1.8,  7.8,  7.8,  3.2,  1.4,
     G -11.0,  6.9,  6.9,  -.4,  1.8/
 
 
C          (U.S.) VIZ MARK II                  
C     ====> JTYPE = 49  --  ITAB = 18 <====
 
      DATA  VIZMK2  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -1.0, -1.0, -1.0, -1.0, -1.0, !1000
     2  -1.0, -1.0, -1.0, -1.0, -1.0, ! 925
     3  -1.0, -1.0, -1.0, -1.0, -1.0, ! 850
     4  -1.0, -1.0, -1.0, -1.0, -1.0, ! 700
     5  -3.0, -3.0, -3.0, -3.0, -3.0, ! 500
     6  -3.0, -3.0, -3.0, -3.0, -3.0, ! 400
     7  -3.0, -3.0, -3.0, -3.0, -3.0, ! 300
     8  -3.0, -3.0,  3.0,  4.0,  5.0, ! 250
     9  -3.0, -3.0,  3.0,  4.0,  5.0, ! 200
     A  -3.0, -3.0,  5.0,  7.0,  6.0, ! 150
     B  -3.0, -3.0,  5.0,  7.0,  6.0, ! 100
     C  -4.0, -3.0,  7.0,  9.0,  8.0, !  70
     D  -6.0, -4.0,  8.0, 10.0,  9.0, !  50
     E  -8.0, -6.0,  9.0, 11.0, 10.0, !  30
     F -12.0,-10.0,  9.0, 12.0, 10.0, !  20
     G -24.0,-20.0, 11.0, 13.0, 12.0/ !  10
C  Table above if from UK Met data for summer:
C  1 April to 30 September.


C     DATA  VIZMK2  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
C    1   -.2,   .2, -2.8, -3.3,-16.7,
C    2  -1.6,  1.5, -3.2, -1.8, -5.6,
C    3  -1.7,  -.1, -1.4,   .3,   .5,
C    4  -1.9, -2.6, -1.1,   .2,   .4,
C    5  -1.6,   .0,  1.0,  1.0,  3.6,
C    6  -1.4,   .1,  1.4,  2.0,  2.1,
C    7    .1,  3.8,  4.2,  4.9,  3.6,
C    8  -1.0,  2.6,  3.8,  5.1,  4.8,
C    9  -3.6,   .4,  2.4,  1.2,  6.3,
C    A  -4.8,   .1,  3.5,  2.3,  9.8,
C    B  -3.4,  3.0,  6.8,  6.2, 13.3,
C    C  -2.9,  2.8,  4.1,  6.0,  7.2,
C    D  -1.7,  2.6,  5.3,  9.0,  6.9,
C    E  -2.0,  5.6, 12.0, 15.5, 14.5,
C    F  -6.4,  2.9, 12.0, 17.7, 25.6,
C    G -10.2,  9.6, 19.7, 13.6, 13.6/
 
 
C          (GERMANY) GRAW DFM-90               
C     ====> JTYPE = 50  --  ITAB = 19 <====
 
      DATA  GRAW90  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  12.6, 12.5, 11.3, -6.7,-13.7,
     2    .7,  4.2,  4.0,  6.2,  -.4,
     3   -.1,  2.7,  2.4,  4.4,  5.8,
     4  -2.5,  1.6,  1.4,  1.4, -1.3,
     5   -.8,  2.2,  4.2,  4.7,  6.9,
     6   -.4,  4.4,  5.6,  8.4, 11.0,
     7   2.3,  9.3, 10.9, 10.5,  9.7,
     8   2.6,  9.7, 10.4,  9.5,  7.2,
     9   3.7,  5.9,  9.0,  8.6,  7.0,
     A   3.0,  5.9, 10.7,  9.2,  9.2,
     B   5.1,  5.8, 12.0, 12.0, 12.0,
     C   7.4, 11.2, 11.4,  9.7,  9.7,
     D   3.3, 11.3,  7.5,  4.2,  4.2,
     E   2.0, 19.4, 10.5,  4.7,  4.7,
     F  -6.5, 19.2, -3.0, -6.5, -6.5,
     G   1.3, 28.6,  6.3, -8.1, -8.1/
 
 
C          (U.S.) NOAA / VIZ TYPE  B2          
C     ====> JTYPE = 51  --  ITAB = 20 <====
 
      DATA  USVZB2  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   -.2, -4.5, -6.4,-14.1, -2.1,
     2   -.4, -3.6, -4.2, -5.7, -3.5,
     3  -1.0, -2.6, -3.1,   .3, -7.0,
     4  -3.5, -3.1, -3.2, -1.8, -4.5,
     5  -2.0,  -.1,   .5,   .3,   .4,
     6  -1.6,   .4,  2.3,  1.4,  6.1,
     7  -1.2,  2.8,  3.5,  3.9,  2.6,
     8    .1,  3.4,  5.5,  3.8,  7.4,
     9  -1.5,  -.3,  3.7,  7.4,  9.4,
     A  -2.4,   .2,  6.7,  8.5, 18.6,
     B   -.6,  2.2, 10.1, 13.6, 23.4,
     C  -7.6, -2.4,  4.4,  8.7,  8.4,
     D  -7.6, -1.6,  5.1,  5.6, 10.8,
     E  -9.0, -1.4,  7.3, 10.4, 10.8,
     F  -6.7,  1.6,  9.7,  8.8, 11.8,
     G  -5.5,  8.7, 12.9,  7.9,  7.9/
 
 
C          VAISALA  RS80/MICROARTS             
C     ====> JTYPE = 52  --  ITAB = 21 <====
 
      DATA  MICROA  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -6.6, -2.0, -3.8,  3.6,  -.4,
     2  -3.7, -6.4, -6.2,   .1, -2.2,
     3  -1.5, -4.9, -5.9,  -.4, -4.2,
     4  -1.9, -4.4, -5.7, -2.6, -2.9,
     5  -2.2, -2.5, -3.1, -3.5, -7.6,
     6  -1.9, -1.7, -2.0, -3.7, -7.8,
     7  -1.5,  -.8, -1.8, -4.2, -3.9,
     8  -1.7,   .1, -2.1, -5.1, -5.6,
     9  -2.1, -2.5, -4.4, -2.5,-11.4,
     A  -4.1, -3.4, -5.7, -4.5,-11.2,
     B   -.3, -2.6, -2.3,  1.1, -6.8,
     C  -1.3, -4.8, -6.7, -1.2,   .6,
     D   -.1, -3.4, -5.1, -3.2,  3.6,
     E   1.6, -1.4, -2.3, -4.0,  -.6,
     F   4.8,  1.4, -1.5, -7.4, -3.9,
     G   2.3,   .9,  2.4, -1.5,-12.4/
 
 
C          VAISALA  RS80/MICROCORA             
C     ====> JTYPE = 60  --  ITAB = 22 <====
 
      DATA  MICORA  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1    .7, -1.2, -7.8, -1.5,  3.8,
     2   4.2,  3.3, -3.7, -5.1,  5.7,
     3  -1.7,  1.8, -2.0, -3.6,   .3,
     4    .6,  5.6,  4.5,   .7,   .3,
     5   1.5,  3.9,  3.6,   .6,  4.0,
     6   2.3,  2.3,  1.3,   .3,  2.7,
     7   1.8,  3.5,   .4,   .7,  4.2,
     8   1.8,  2.8,  -.5,   .6,  3.3,
     9   2.7,  2.9, -1.2,   .9,  4.1,
     A   4.5,  7.4, -1.3,  1.4, 14.2,
     B  -4.1,  1.0, -2.3, -5.7,  9.6,
     C  -9.5,  -.9, -6.7, -9.4, -2.4,
     D  -3.4,  -.5,  1.7, -3.6, -2.2,
     E   4.1, -6.1, -1.5, -3.9, -5.0,
     F   6.8, -5.4,  5.3, -3.6, -9.7,
     G   3.7,  9.2,  8.3,  4.0,  8.6/
 
 
C          VAISALA  RS80/DIGICORA  OR MARWIN   
C     ====> JTYPE = 61  --  ITAB = 23 <====
 
      DATA  DICORA  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   2.1,  2.6,  1.0,   .0,  -.9,
     2   1.0,  2.8,  1.0,   .8,   .2,
     3   1.0,  2.7,  1.7,   .6,   .4,
     4    .5,  1.8,   .9,   .1,   .0,
     5    .4,   .9,   .0,   .1,   .5,
     6    .2,   .4,  -.1,   .2,   .0,
     7    .1,   .0,  -.3,  -.4,  -.2,
     8    .2,  -.4,   .1,  -.1, -1.0,
     9    .4,  1.6,  1.6,  1.3, -1.0,
     A   1.0,  1.6,  2.2,  2.4,  1.1,
     B   1.0,  1.9,  2.3,  2.8,  2.5,
     C   -.5,  2.1,  2.3,   .6,   .5,
     D   -.9,  1.5,   .5,  -.7,   .2,
     E  -1.3,   .4,  -.3, -1.7, -1.5,
     F  -1.0, -1.3, -1.1,  -.2,  -.6,
     G    .2, -1.2, -1.3,   .0,  5.4/
 
 
C          VAISALA  RS80/PCCORA                
C     ====> JTYPE = 62  --  ITAB = 24 <====
 
      DATA  PCCORA  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   2.6, -1.8,   .2, -3.2,  3.8,
     2   1.0, -1.2,  1.6,  -.3,  3.2,
     3   -.4, -1.1,   .6,  -.1,  -.7,
     4   -.4,   .5,   .6,  -.1, -3.9,
     5    .8,  1.3,  1.5,   .8,   .5,
     6    .6,  1.1,   .9,   .8,   .0,
     7   -.2,  2.2,  1.3,   .5,  -.5,
     8   -.9,  2.1,   .5,   .2,   .0,
     9    .9,   .1,   .8,  1.2,  5.2,
     A   1.3,  2.0,   .8,   .1,  5.6,
     B   1.9,  1.2,   .2,   .3,  3.8,
     C   3.4,  2.5,  1.8,  1.2,   .7,
     D   1.5,  1.6,   .8,  -.2, -3.4,
     E    .0,  1.6,  1.1,   .5, -3.9,
     F  -4.0,  -.4,   .0, -1.4, -4.7,
     G  -5.6,  6.1,  1.1,  1.1,  5.5/
 
 
C          VAISALA  RS80/STAR                  
C     ====> JTYPE = 63  --  ITAB = 25 <====
 
      DATA  VASTAR  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -6.8,  1.4,  2.3,  5.5,  1.9,
     2  -3.0, 10.0,  3.2, -1.0, -1.7,
     3  -1.9,  6.3,  2.3,   .6,   .0,
     4   -.1, 10.5,  3.4,  1.1,  1.1,
     5  -1.0,  4.7,  1.3,  -.3,  -.2,
     6    .1,  1.9,   .9,  -.1,   .8,
     7   2.8, -3.0,  3.3,  2.2,  1.8,
     8   4.2, -5.6,  3.9,  2.3,  3.6,
     9    .9, -2.6, -1.0, -3.5,  3.3,
     A  -1.7,  6.7,   .5, -4.7,  -.1,
     B  -4.4,  5.5,  -.5, -7.2, -1.8,
     C   2.1,  8.8,   .6,   .9,  1.5,
     D   3.2,  5.6,  3.4,  3.3,  -.9,
     E   1.0,  8.5,  6.7,  5.5,  1.9,
     F  -7.6,  9.3,  1.5,  1.9, -5.7,
     G  -2.7,  9.2,-11.1, -3.0, -3.0/
 
 
C          UNKNOWN TYPE                        
C     ====> JTYPE = 90  --  ITAB = 26 <====
 
      DATA  UNKN90  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -3.9,-17.9,-15.7,-32.5,-32.5,
     2  -6.6,-11.5,-10.5,-22.5,-22.5,
     3  -3.8, -5.8, -4.1,-17.6,-17.6,
     4  -5.3, -1.1, -3.0,-15.1,-15.1,
     5  -3.0, -1.2, -2.7,-14.1,-14.1,
     6  -2.2,  2.4,   .5,-11.9,-11.9,
     7  -3.0,  6.9,  4.5, -4.3, -4.3,
     8  -2.2,  9.7,  5.8, -4.1, -4.1,
     9  -2.3,  5.5,  -.5,-12.0,-12.0,
     A  -3.1,  9.5, -5.8,-17.1,-17.1,
     B  -6.7, 10.8, -6.7,-17.9,-17.9,
     C  -7.4,  9.2, -2.5,  4.2,  4.2,
     D  -8.0,  6.0, 12.2,  3.0,  3.0,
     E  -7.8,   .8, 27.6, 16.6, 16.6,
     F  -7.0, 17.2, 23.8, 15.4, 15.4,
     G -15.8,  2.4, 15.0,  6.2,  6.2/
 
 
C           PRESSURE ONLY PLUS TRANSPONDER     
C     ====> JTYPE = 92  --  ITAB = 27 <====
 
      DATA  TYPE92  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  16.1,  3.3,  3.3,  3.3,  3.3,
     2  16.1,  3.3, 18.6, 18.6, 18.6,
     3  -1.3,   .2,  9.9,  9.9,  9.9,
     4   2.6, -4.6, -4.2, -4.2, -4.2,
     5  -2.8,  1.9,  4.6,  4.6,  4.6,
     6  -1.5,   .6,  9.6,  9.6,  9.6,
     7   5.1,  7.5,  8.0,  8.0,  8.0,
     8   1.6,  7.2, 12.3, 12.3, 12.3,
     9    .7,  4.8, -1.1, -1.1, -1.1,
     A  -6.7,  1.0, 23.2, 23.2, 23.2,
     B  -1.7,  5.6, 17.4, 17.4, 17.4,
     C   2.1,  5.5,  8.1,  8.1,  8.1,
     D   -.4,  6.3,  8.6,  8.6,  8.6,
     E  -2.6,  5.2, 11.8, 11.8, 11.8,
     F  -1.8,  6.5,  9.3,  9.3,  9.3,
     G  17.2,  8.9, 12.0, 12.0, 12.0/
 

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

C  EACH TIME THROUGH, COMPUTE LOGS OF PRESSURE RATIOS
C  --------------------------------------------------

      ALP(1) = 0.0
      DO I=2,16
         ALP(I) = ALOG(PRES(I-1)/PRES(I))
      ENDDO

C  GET TABLE INDEX FROM RAOB TYPE - CHECK FOR VALID CORR. PARAMETERS
C  -----------------------------------------------------------------

      ITAB = 0
      IF(JTYPE.GT.0.AND.JTYPE.LT.256) THEN
         ITAB = ITYPTT(JTYPE)
         IF(ITAB.EQ.0)  THEN
            IRET = -1
            RETURN
         ELSE  IF(ITAB.LT.0)  THEN
            WRITE(6,1)  SID(IS),JTYPE
            WRITE(68,1) SID(IS),JTYPE
            IRET = -1
            RETURN
         END IF
      ELSE
         WRITE(6,1)  SID(IS),JTYPE
         WRITE(68,1) SID(IS),JTYPE
1        FORMAT(5X,'* * *  STN. ID ',A8,' HAS AN INVALID RAOB TYPE ',
     .    'DESIGNATION (BUFR/ON29 CODE ',I8,') - NO CORRECTION ',
     .    'POSSIBLE')
         IRET = -1
         if(jtype.eq.20000)  iret = -99
         RETURN
      ENDIF

C  GET SOLAR ELEVATION ANGLE ON ALL VALID MANDATORY LEVELS
C  -------------------------------------------------------

      CALL SOELAN(SOLAR)

C  APPLY TEMPERATURE AND HEIGHT ADJUSTMENTS
C  DO NOT APPLY AN ADJUSTMENT BELOW 700 HPA
C  ----------------------------------------

      DTS = 0
      DHS = 0
      DO ILEV=4,16
         ANGLE(2)  = CUTOFF(ILEV)

         IF(SOLAR(ILEV).GE.CUTOFF(ILEV))  THEN
            IF(SOLAR(ILEV).LT.ANGLE(5)) THEN
               DO K=3,5
                  IF(SOLAR(ILEV).LT.ANGLE(K)) THEN
                     TABU = TTAB(K  ,ILEV,ITAB)
                     TABD = TTAB(K-1,ILEV,ITAB)
                     DSUN=(SOLAR(ILEV)-ANGLE(K-1))/(ANGLE(K)-ANGLE(K-1))
                     DTS(ILEV) = TABD + DSUN*(TABU-TABD)
                     GO TO 10
                  ENDIF
               ENDDO
            ELSE
               DTS(ILEV) = TTAB(5,ILEV,ITAB)
            ENDIF
         ELSE

C  IF ANGLE < CUTOFF (NIGHT), CORRECTION FROM 'NIGHT' VALUE IN TBL
C  --------------------------------------------------------------------

            DTS(ILEV) = TTAB(1,ILEV,ITAB)
         ENDIF

C  The constant is (R/(20*g)) = 1.46355
C  ------------------------------------

   10    CONTINUE
         HYF = 1.46355*ALP(ILEV)
         IF(ILEV.EQ.1) THEN
            DHS(ILEV) = HYF*DTS(ILEV)
         ELSE
            DHS(ILEV) = DHS(ILEV-1) + HYF*(10.*DTS(ILEV-1)+DTS(ILEV))
         ENDIF

C  CORRECT DTS BY A FACTOR OF 10
C  -----------------------------

         DTS(ILEV) = .1*DTS(ILEV)

C  SAVE AND APPLY THE CORRECTIONS
C  ------------------------------

         DHT(ILEV) = -DHS(ILEV)
         DTP(ILEV) = -DTS(ILEV)

         IF(HGT(ILEV).LT.BMISS) HGT(ILEV) = HGT(ILEV) + DHT(ILEV)
         IF(TMP(ILEV).LT.BMISS) TMP(ILEV) = TMP(ILEV) + DTP(ILEV)

      ENDDO

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RADT5       APPLIES NEWEST SET OF RADIATION CORRECTNS
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 2008-11-19
C
C ABSTRACT: THIS SUBROUTINE APPLIES RADIATION CORRECTIONS TO THE
C   OBSERVED RADIOSONDE HEIGHTS AND TEMPERATURES ON THE BOTTOM 16
C   MANDATORY PRESSURE LEVELS.  DATA ARE PASSED THROUGH COMMON
C   /RADCOM/.  THE CORRECTION OF RAOBS FOR BOTH SHORT- AND LONG-WAVE
C   EFFECTS IN ACCOMPLISHED BY READING IN TABLES OF NUMBERS WHICH ARE
C   DEPENDENT ON THE RADIOSONDE TYPE. CALLED FOR ONE REPORT AT A TIME.
C
C PROGRAM HISTORY LOG:
C 1999-06-02  W. COLLINS (W/NP22) -- ORIGINAL AUTHOR
C 2002-07-22  D. A. KEYSER -- CALLS NEW ROUTINE SOELAN TO CALCULATE SUN
C     ANGLE ON VALID MANDATORY LEVELS RATHER THAN DOING CALCULATION IN
C     THIS SUBROUTINE
C 2004-03-17  D. A. KEYSER --  UPDATED DOCBLOCK AND ADDED NEW
C     RADIOSONDE TYPES SINCE THE LAST TIME IT WAS UPDATED (NONE OF THE
C     NEW TYPES RECEIVE CORRECTIONS)
C 2005-08-03  D. A. KEYSER --  REVISED CORRECTION TABLE FOR CHINA'S
C     SHANGHAI RADIO SONDE (BUFR TYPE 32), AFTER 12/31/00 NO
C     CORRECTIONS ARE MADE BETWEEN 700 AND 50 MB, INCLUSIVE BECAUSE
C     THIS SONDE IS BEING CORRECTED ON SITE (THIS WILL REMOVE COLD BIAS
C     FOUND IN THESE SONDES VS. GUESS BELOW 50 MB) (NOTE 1: NO
C     CORRECTIONS ARE MADE BELOW 700 MB FOR ANY SONDES, NOTE 2: PRIOR
C     TO 1/1/01, THE PREVIOUS CORRECTION TABLE IS STILL USED FOR THIS
C     SONDE BECAUSE, ACCORDING TO YUETANG YHANG OF THE CHINESE NMC,
C     THIS IS WHEN THE ON-SITE CORRECTIONS STARTED); ADDED NEW
C     RADIOSONDE TYPES IN DOCBLOCK SINCE THE LAST TIME IT WAS UPDATED
C 2007-09-14  D. KEYSER    -- MINOR CODE CORRECTIONS SUGGESTED BY PAT
C     PAULEY (CORRECTS PROBLEMS THAT COULD OCCUR ON SOME REMOTE
C     MACHINES)
C 2008-04-10  D. A. KEYSER --  UPDATED DOCBLOCK AND ADDED NEW
C     RADIOSONDE TYPES SINCE THE LAST TIME IT WAS UPDATED (NONE OF THE
C     NEW TYPES RECEIVE CORRECTIONS); CAN HANDLE RADIOSONDE TYPES > 99
C     WHICH WILL SOON BE INTRODUCED INTO THE BUFR DATABASE (BASED ON
C     NOVEMBER 2007 WMO BUFR UPDATE)
C 2008-10-08  D. A. KEYSER -- UPDATED LIST OF RADIOSONDE TYPES TO
C     CONFORM WITH LATEST (NOV. 2007) WMO BUFR CODE TABLE 0-02-011
C 2008-11-19  D. A. KEYSER -- UPDATED LIST OF RADIOSONDE TYPES TO
C     CONFORM WITH LATEST (NOV. 2008) WMO BUFR CODE TABLE 0-02-011;
C     MORE SPECIFIC DIAGNOSTIC PRINT RELATED TO RADIOSONDE TYPES
C     ENCOUNTERED (I.E., INVALID, OBSOLETE, VACANT, ETC.)
C
C USAGE:    CALL RADT5(IRET)
C   OUTPUT ARGUMENT LIST:
C     IRET     - RETURN CODE
C                  =   0 - CORRECTIONS
C                  =  -1 - NO CORRECTIONS
C                  = -99 - BIG PROBLEM
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 68  - RADCOR INFORMATION FILE
C
C REMARKS: THESE CORRECTIONS USE THE BUFR CODE FIGURE FOR RADIOSONDE
C   TYPE OBTAINED FROM BUFR-FM94 TABLE 0 02 011.  THE NMC UPPER-AIR
C   DICTIONARY ALSO CONTAINS THIS BUFR CODE FIGURE FOR RADIOSONDE
C   TYPE.  CALLED BY SUBROUTINE "RADEVN".
C
C
C   KEY FOR RADIOSONDE TYPES USED HERE (EFFECTIVE 11/2008):
C
C  JTYPE       KEY         DESCRIPTION                      TABLE NUMBER
C          ("*" in use 11/2008) 
C < 000                    Invalid
C  000                     Reserved
C  001          *          (U.S.) iMet-1-BB
C 002 to 006               Not used for radiosondes
C  007          *          (U.S.) iMet-1-AB
C  008                     Not used for radiosondes
C  009          *          No radiosonde - system unknown or
C                          unspecified
C  010                     (U.S.) VIZ type A pressure-commutated  2
C  011                     (U.S.) VIZ type B time-commutated      3 
C  012          *          (U.S.) RS SDC (Space Data Corp.)       4
C  013                     (Australia) Astor
C  014                     (U.S.) VIZ Mark I MICROSONDE
C  015                     (U.S.) EEC Company type 23
C  016                     (Austria) Elin
C  017          *          (Germany) Graw G
C  018          *          (Germany) Graw DFM-06
C  019          *          (Germany) Graw M60                     
C  020          *          Indian Met. Service Type  MK3          5
C  021          *          (S. Korea) VIZ/Jin Yang Mark I         6
C                          MICROSONDE                             
C  022          *          (Japan) Meisei RS2-80                  7
C  023                     (France) Mesural FMO 1950A
C  024                     (France) Mesural FMO 1945A
C  025                     (France) Mesural MH73A
C  026          *          (Switzerland) Meteolabor Basora        8
C  027          *          (Russia) AVK-MRZ                       9
C  028          *          (Russia) Meteorit Marz2-1             10
C  029          *          (Russia) Meteorit Marz2-2             11
C  030                     (Japan) OKI RS2-80
C  031                     (Canada) VIZ/Valcom type A pressure-
C                          commutated
C  032          *          (China) Shanghai Radio
C                                     prior to 01/01/01          12
C                                     after    12/31/00          28
C  033                     (U.K.) UK Met Office MK3    
C  034                     (Czechoslovakia) Vinohrady
C  035                     Vaisala RS18
C  036          *          Vaisala RS21
C  037          *          Vaisala RS80                          13
C  038                     (U.S.) VIZ LOCATE (Loran-C)
C  039                     (Germany) Sprenger E076
C  040                     (Germany) Sprenger E084
C  041                     (Germany) Sprenger E085
C  042                     (Germany) Sprenger E086
C  043                     (U.S.) AIR IS-4A-1680                 14
C  044                     (U.S.) AIR IS-4A-1680 X               15 
C  045                     (U.S.) RS MSS                         15 
C  046                     (U.S.) AIR IS-4A-403                  16
C  047          *          (Japan) Meisei RS2-91                 17 
C  048                     (Canada) VALCOM
C  049          *          (U.S.) VIZ MARK II                    18
C  050          *          (Germany) GRAW DFM-90                 19
C  051          *          (U.S.) VIZ-B2                         20
C  052          *          Vaisala RS8-57H                       21
C  053          *          (Russia) AVK-RF95
C  054          *          (Germany) GRAW DFM-97
C  055          *          (Japan) Meisei RS-016
C  056          *          (France) M2K2
C  057          *          (France) M2K2-P
C  058          *          (Russia) AVK-BAR
C  059          *          (France) Modem M2K2-R 1680 MHz RDF
C                          with pressure sensor chip
C  060          *          Vaisala RS80/MicroCora                22
C  061          *          Vaisala RS80/Loran/Digicora I,II or
C                          Marwin (baseline)
C  062          *          Vaisala RS80/PCCora                   24
C  063          *          Vaisala RS80/Star                     25
C  064                     (U.S.) Orbital Sciences Corp. Space
C                          Data Division, transponder
C                          radiosonde, type 909-11-xx (where xx
C                          corresponds to the model of the
C                          instrument)
C  065                     (U.S.) VIZ transponder radiosonde,
C                          model number 1499-520
C  066          *          Vaisala RS80/Autosonde
C  067          *          Vaisala RS80/Digicora III
C  068          *          (Russia) AVK-RZM-2
C  069          *          (Russia) MARL-A or Vektor-M-RZM-2
C  070          *          Vaisala RS92/Star
C  071          *          Vaisala RS90/Digicora I,II or Marwin  26
C  072          *          Vaisala RS90/PC-Cora
C  073          *          Vaisala RS90/Autosonde
C  074          *          Vaisala RS90/Star                        
C  075          *          (Russia) AVK-MRZ-ARMA             
C  076          *          (Russia) AVK-RF95-ARMA                27
C  077                     (France) GEOLINK GPSonde GL98
C  078          *          Vaisala RS90/Digicora III
C  079          *          Vaisala RS92/Digicora I,II or Marwin
C  080          *          Vaisala RS92/Digicora III
C  081          *          Vaisala RS92/Autosonde
C  082          *          (U.S.) Sippican MK2 GPS/STAR with
C                          rod thermistor, carbon element and
C                          derived pressure
C  083          *          (U.S.) Sippican MK2 GPS/W9000 with
C                          rod thermistor, carbon element and
C                          derived pressure
C  084          *          (U.S.) Sippican MARK II with chip
C                          thermistor, carbon element and
C                          derived pressure from GPS height
C  085          *          (U.S.) Sippican MARK IIA with chip
C                          thermistor, carbon element and
C                          derived pressure from GPS height
C  086          *          (U.S.) Sippican MARK II with chip
C                          thermistor, pressure and carbon
C                          element
C  087          *          (U.S.) Sippican MARK IIA with chip
C                          thermistor, pressure and carbon
C                          element
C  088          *          (Russia) MARL-A or Vektor-M-MRZ
C  089          *          (RUSSIA) MARL-A or Vektor-M-BAR
C  090          *          Radiosonde not specified or unknown
C  091                     Pressure-only radiosonde
C  092                     Pressure-only radiosonde plus
C                          transponder
C  093                     Pressure-only radiosonde plus radar-
C                          reflector
C  094                     No-pressure radiosonde plus
C                          transponder
C  095                     No-pressure radiosonde plus radar-
C                          reflector
C  096          *          Descending radiosonde
C  097          *          (S. Africa) BAT-16P
C  098          *          (S. Africa) BAT-16G
C  099          *          (S. Africa) BAT-4G
C  100                     Reserved
C  101                     Not vacant
C 102 TO 106               Reserved
C  107                     Not vacant
C 108 TO 109               Reserved
C  110          *          (U.S.) Sippican LMS5 w/Chip
C                          Thermistor, duct mounted
C                          capacitance relative humidity sensor,
C                          and derived pressure from GPS height
C  111          *          (U.S.) Sippican LMS6 w/Chip
C                          Thermistor, external boom mounted
C                          capacitance relative humidity sensor,
C                          and derived pressure from GPS height
C  112                     Not vacant
C 113 TO 116               Vacant
C 117 TO 122               Not vacant
C 123 TO 125               Vacant
C 126 TO 129               Not vacant
C 130 TO 131               Vacant
C  132                     Not vacant
C 133 TO 135               Vacant
C 136 TO 137               Not vacant
C 138 TO 146               Vacant
C  147                     Not vacant
C  148                     Vacant
C 149 TO 163               Not vacant
C 164 TO 165               Vacant
C 166 TO 176               Not vacant
C  177                     Vacant
C 178 TO 189               Not vacant
C 190 TO 196               Reserved
C 197 TO 199               Not vacant
C 200 TO 254               Reserved
C  255                     Missing value
C >255                     Invalid
C
C
C   KEY FOR LEVELS IN DATA (CORRECTION) TABLES:
C
C     1 - 1000 MB
C     2 -- 925 MB
C     3 -- 850 MB
C     4 -- 700 MB
C     5 -- 500 MB
C     6 -- 400 MB
C     7 -- 300 MB
C     8 -- 250 MB
C     9 -- 200 MB
C     A -- 150 MB
C     B -- 100 MB
C     C --  70 MB
C     D --  50 MB
C     E --  30 MB
C     F --  20 MB
C     G --  10 MB
C
C
C   THE RADIATION CORRECTION TABLES ARE BASED UPON
C   OBSERVED INCREMENTS FROM NWP ASSIMILATION.  THE ADJUSTMENTS
C   ARE TO MAKE ALL SONDE TYPES COMPARE WITH THE VAISALA
C   RS80 DIGICORA SONDE.
C
C
C   THE CUTOFF ANGLE AT EACH LEVEL CALCULATED USING THE RELATION:
C              CUTOFF ANGLE = -1.76459 * (Z**.40795) ,
C   WHERE Z IS A REFERENCE HEIGHT FOR THE LEVEL (IN KILOMETERS)
C   (SEE NMC OFFICE NOTE 306, TABLE 4 FOR LIST OF REFERENCE HEIGHTS.)
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE RADT5(IRET)
      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /RADCOM/ HGT(16),TMP(16),DHT(16),DTP(16),JTYPE
      COMMON /SWITCH/ LWCORR,LEVRAD,IRCTBL,HGTTBL,BAL_DRIFT
      COMMON /USVAI/ CORUSVAI
      COMMON /PMAND / PRES(16),KMIN,KMAX,INM(16)
      COMMON /STN/     IS
      COMMON /DATE_IDT/IDT
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      REAL        TTAB(5,16,29),CUTOFF(16),
     .            SOLAR(16),ANGLE(5),ALP(16),DTS(16),DHS(16),
     .            USVIZA(5,16), USVIZB(5,16), USSDCP(5,16),
     .            AVKR95(5,16), INDMK3(5,16), KORMK1(5,16),
     .            MEIS80(5,16), RUSAVK(5,16), RUSZ21(5,16),
     .            RUSZ22(5,16), CHINSR(5,16),
     .            VARS80(5,16), AIRR80(5,16), USRSMS(5,16),
     .            MEIS91(5,16), VIZMK2(5,16), GRAW90(5,16),
     .            USVZB2(5,16), MICROA(5,16), MICORA(5,16),
     .            DICORA(5,16), PCCORA(5,16), VASTAR(5,16),
     .            UNKN90(5,16), UNKN09(5,16), CHINSX(5,16),
     .            METEOB(5,16), AIR403(5,16), VARS90(5,16)
      INTEGER     ITYPTT(255)
      EQUIVALENCE (TTAB(1,1, 1),UNKN09(1,1)),(TTAB(1,1, 2),USVIZA(1,1)),
     .            (TTAB(1,1, 3),USVIZB(1,1)),(TTAB(1,1, 4),USSDCP(1,1)),
     .            (TTAB(1,1, 5),INDMK3(1,1)),(TTAB(1,1, 6),KORMK1(1,1)),
     .            (TTAB(1,1, 7),MEIS80(1,1)),(TTAB(1,1, 8),METEOB(1,1)),
     .            (TTAB(1,1, 9),RUSAVK(1,1)),(TTAB(1,1,10),RUSZ21(1,1)),
     .            (TTAB(1,1,11),RUSZ22(1,1)),(TTAB(1,1,12),CHINSR(1,1)),
     .            (TTAB(1,1,13),VARS80(1,1)),(TTAB(1,1,14),AIRR80(1,1)),
     .            (TTAB(1,1,15),USRSMS(1,1)),(TTAB(1,1,16),AIR403(1,1)),
     .            (TTAB(1,1,17),MEIS91(1,1)),(TTAB(1,1,18),VIZMK2(1,1)),
     .            (TTAB(1,1,19),GRAW90(1,1)),(TTAB(1,1,20),USVZB2(1,1)),
     .            (TTAB(1,1,21),MICROA(1,1)),(TTAB(1,1,22),MICORA(1,1)),
     .            (TTAB(1,1,23),DICORA(1,1)),(TTAB(1,1,24),PCCORA(1,1)),
     .            (TTAB(1,1,25),VASTAR(1,1)),(TTAB(1,1,26),VARS90(1,1)),
     .            (TTAB(1,1,27),AVKR95(1,1)),(TTAB(1,1,28),CHINSX(1,1)),
     .            (TTAB(1,1,29),UNKN90(1,1))
      LOGICAL     HGTTBL, CORUSVAI
      CHARACTER*8 SID

      DATA  ANGLE  / -90, -10, 15, 30, 60/
      DATA  CUTOFF / -.73, -1.58, -2.06, -2.77,
     .              -3.56, -3.95, -4.36, -4.58,
     .              -4.83, -5.12, -5.49, -5.79,
     .              -6.06, -6.43, -6.72, -7.17/

C  ITYPTT ASSIGNS PROPER TEMPERATURE CORRECTION TABLE TO INST. TYPES
C  -----------------------------------------------------------------
C    Key:  = 1-29
C               -- This is a valid radiosonde type in 11/2008 and it
C                  uses the correction table noted by the key value.
C          = 101-129
C               -- This is an obsolete radiosonde type in 11/2008 and
C                  will never again be used, none-the-less there is a
C                  correction table available for it noted by the key
C                  value minus 100.
C          =  0 -- This is a valid radiosonde type in 11/2008 but
C                  there is no correction table available for it.
C          = -1 -- This is an obsolete radiosonde type in 11/2008 and
C                  will never again be used (no correction is made).
C          = -2 -- This is a vacant radiosonde type in 11/2008 but
C                  it may become a valid radiosonde type at some point
C                  after that date (no correction is made).
C          = -3 -- This is, and always has been, an invalid radiosonde
C                  type (no correction is made).

C  ===> JTYPE:       1   2   3   4   5   6   7   8   9 *10 *11  12  13

      DATA  ITYPTT/  0, -3, -3, -3, -3, -3,  0, -3,  0,102,103,  4, -1,

C  ===> JTYPE:      14  15  16  17  18  19  20  21  22  23  24  25  26

     $              -1, -1, -1,  0,  0,  0,  5,  6,  7, -1, -1, -1,  8,

C  ===> JTYPE:      27  28  29  30  31  32  33  34  35  36  37  38  39

     $               9, 10, 11, -1, -1, 12, -1, -1, -1,  0, 13, -1, -1,

C  ===> JTYPE:      40  41  42 *43 *44 *45 *46  47  48  49  50  51  52

     $              -1, -1, -1,114,115,115,116, 17, -1, 18, 19, 20, 21,

C  ===> JTYPE:      53  54  55  56  57  58  59  60  61  62  63  64  65

     $               0,  0,  0,  0,  0,  0,  0, 22,  0, 24, 25, -1, -1,

C  ===> JTYPE:      66  67  68  69  70  71  72  73  74  75  76  77  78
     $               0,  0,  0,  0,  0, 26,  0,  0,  0,  0, 27, -1,  0,

C  ===> JTYPE:      79  80  81  82  83  84  85  86  87  88  89  90  91
     $               0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,

C  ===> JTYPE:      92  93  94  94  96  97  98  99  100-109  110  111
     $               0,  0,  0,  0,  0,  0,  0,  0,   10*-3,   0,   0,

C  ===> JTYPE:      112  113-116  117-122  123-125  126-129  130  131
     $               -3,    4*-2,    6*-3,    3*-2,    4*-3,  -2,  -2,

C  ===> JTYPE:      132  133-135  136  137  138-146  147  148  149-163
     $               -3,    3*-2,  -3,  -3,    9*-2,  -3,  -2,   15*-3,

C  ===> JTYPE:      164  165  166-176  177  178-255 
     $               -2,  -2,   11*-3,  -2,   78*-3/



C  TEMPERATURE CORRECTION TABLES ( * 10 K) FOR RADIATIVE EFFECTS
C  -------------------------------------------------------------
 
C  This set of tables is modified to insert reasonable
C  numbers for entries with 0 or small count. Data 
C  are for all available for 1999.
C  Values are smoothed with single pass of weighted
C  1-2-1 smoother in the vertical.
C  Values are increment differences between given
C  type and Vaisala RS-80 Digicora (type 61).
 
C  NO RADIOSONDE - SYSTEM UNKNOWN OR UNSPECIFIED
C     ====> JTYPE =  009  --  ITAB = X  <====
C            (no longer corrected)
 
      DATA  UNKN09  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1    .9,   .1, -7.0, -8.4,-16.6,
     2  -1.2,  -.7, -2.1,  -.7, -3.5,
     3    .3,  2.1,   .6,  2.6,  2.2,
     4   1.7,  3.4,   .8,  2.5,  3.0,
     5   2.3,  3.3,   .7,  2.1,   .0,
     6   2.5,  3.7,  2.9,  3.3,  -.1,
     7   3.0,  4.6,  6.4,  5.7,  3.1,
     8   3.4,  5.9,  7.0,  6.8,  3.9,
     9   1.5,  6.7,  3.1,  4.2,  -.7,
     A  -2.0,  6.7, -1.9, -1.1, -8.8,
     B  -3.9,  2.0, -3.4, -4.3,-14.5,
     C  -2.6, -6.2,  2.2,   .5,-10.2,
     D   -.7, -7.4,  8.9,  7.9,  3.6,
     E   -.4,  -.5, 13.3, 11.4,  5.2,
     F   1.2,  7.8, 18.6, 10.7, -1.3,
     G   9.5, 15.3, 21.2, 10.0, 10.0/
 
 
C     (U.S.) VIZ TYPE A PRESSURE-COMMUTATED
C     ====> JTYPE = 010  --  ITAB = 2  <====
 
      DATA  USVIZA  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   3.7, -9.9, -8.2,-14.9,   .5,
     2   -.1,  -.7,  -.3, -1.1,  7.1,
     3    .9,  3.0,  2.4,  3.1,  5.5,
     4   1.5,  2.2,  1.7,  5.3,  1.2,
     5    .0,   .0,  -.6,  5.3,   .0,
     6  -1.3,  -.1,  -.4,  6.3,  1.3,
     7   -.5,  1.9,  2.4,  9.4,  4.3,
     8   1.5,  1.7,  1.5, 11.8,  6.3,
     9   3.0, -1.3, -4.6, 10.9,  5.3,
     A    .9, -1.2,-10.9,  6.5,   .9,
     B  -3.1,   .1, -9.3,  4.3, -3.0,
     C  -1.3, -1.7,  3.4,  8.7,  3.7,
     D   1.9,  1.2, 14.0, 14.5, 11.0,
     E   3.4,  5.5, 21.1, 20.3, 11.4,
     F  13.3,  7.5, 25.8, 22.5,   .9,
     G  27.5, 28.0, 29.0, 22.5, 22.5/
 
 
C       (U.S.) VIZ TYPE B TIME-COMMUTATED
C     ====> JTYPE = 011  --  ITAB = 3  <====
 
      DATA  USVIZB  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -9.5,  2.9,  1.6,-22.5,-22.5,
     2  -5.1, -1.8,  1.2, -1.9, -1.9,
     3  -2.9, -1.2,   .6,  1.6,  1.6,
     4  -2.1, -1.0,  -.1,  -.4,  -.4,
     5  -2.1, -1.6,  -.9,  2.1,  2.1,
     6  -2.2,  -.7,   .1,  4.6,  4.6,
     7   -.8,  1.7,  2.8,  3.1,  3.1,
     8    .0,  1.0,  3.4,  2.1,  2.1,
     9  -1.5, -2.7,   .8, -2.5, -2.5,
     A  -3.4, -2.5,  1.7, -3.0, -3.0,
     B  -4.0, -1.0,  6.7,   .4,   .4,
     C  -3.3,   .3,  9.3, -5.0, -5.0,
     D  -3.5,  2.8, 13.4, -1.6, -1.6,
     E  -6.3,  4.9, 20.0, 20.4, 20.4,
     F  -8.9,  8.2, 24.8, 34.6, 34.6,
     G  -8.9, 10.0, 26.2, 34.6, 34.6/
 
 
C        (U.S.) RS SDC (SPACE DATA CORP.)
C     ====> JTYPE = 012  --  ITAB = 4  <====
 
      DATA  USSDCP  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -7.4,-28.8,-34.4,-35.8,-35.8,
     2   1.3,-13.4, -3.5,  3.6,  3.6,
     3    .6, -4.3,  1.2,  3.2,  3.2,
     4   -.8,  3.2, -2.1,  -.5,  -.5,
     5  -2.1,  4.8, -2.5,  -.9,  -.9,
     6  -4.1,  4.6, -2.0,   .8,   .8,
     7  -4.0, 10.0,  -.2,  3.9,  3.9,
     8  -2.1, 15.5,  1.8,  7.8,  7.8,
     9   2.9, 17.8,  4.8, 12.7, 12.7,
     A   9.0, 22.3, 11.9, 15.8, 15.8,
     B   6.4, 20.5, 10.5, 10.2, 10.2,
     C   1.5,  7.8,  2.5,  4.5,  4.5,
     D  -2.3,  2.0,  1.5,  4.6,  4.6,
     E  -3.5,  9.0,  4.3, 10.6, 10.6,
     F   1.5, 25.6,  8.0,  8.0,  8.0,
     G  12.0, 18.4, 21.0, 21.0, 21.0/
 
 
C          INDIAN MET. SERVICE TYPE MK3
C     ====> JTYPE = 020  --  ITAB = 5  <====
 
      DATA  INDMK3  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -3.7,  -.8,  4.3, 19.2, 19.2,
     2  -3.5, -1.4,  2.3,  9.7,  9.7,
     3    .1,  1.4,  3.5,  9.7,  9.7,
     4   -.5,  1.9,  4.3, 11.0, 11.0,
     5  -5.9,   .1,  3.6, 10.4, 10.4,
     6  -8.7,   .5,  4.4, 10.8, 10.8,
     7  -8.9,  3.8,  7.7, 16.2, 16.2,
     8 -10.8,  3.4,  9.2, 19.7, 19.7,
     9 -13.8, -2.1,  6.1, 16.1, 16.1,
     A -15.9, -5.0,  3.1,  8.8,  8.8,
     B -12.4,  -.6,  5.7,  4.3,  4.3,
     C   -.6, 10.9, 14.4,  9.8,  9.8,
     D   5.5, 19.5, 21.5, 19.2, 19.2,
     E   1.8, 17.5, 19.2, 13.6, 13.6,
     F  -3.6, 12.9, 13.0,  1.0,  1.0,
     G  -1.6, 14.3, 11.4, -3.1, -3.1/
 
 
C         (S. KOREA) VIZ/JIN YANG MARK I
C     ====> JTYPE = 021  --  ITAB = 6  <====
 
      DATA  KORMK1  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -7.6, -3.5,-16.0,   .9,   .9,
     2  -5.1, -2.6, -5.2,  -.1,  -.1,
     3  -3.9, -2.1, -3.6,  -.9,  -.9,
     4  -2.9, -3.5, -3.2,   .2,   .2,
     5  -1.8, -3.8,  -.3,  2.8,  2.8,
     6    .3,   .8,  3.7,  6.2,  6.2,
     7   3.1, 13.6,  4.1,  9.9,  9.9,
     8   4.3, 21.7,  5.1, 11.9, 11.9,
     9    .8, 16.8,  7.0, 11.0, 11.0,
     A  -4.7,  9.1,  4.4, 10.2, 10.2,
     B  -7.4,  5.2,  1.3,  9.3,  9.3,
     C  -7.6,  9.7,   .3,  6.2,  6.2,
     D  -6.3, 11.4,  2.4,  6.6,  6.6,
     E  -3.8, 13.7,  7.5, 11.3, 11.3,
     F  -3.7, 17.1, 16.3, 15.8, 15.8,
     G  -6.9, 13.0, 25.8, 18.6, 14.7/
 
 
C           (JAPAN) MEISEI RS2-80 
C     ====> JTYPE = 022  --  ITAB = 7  <====
 
      DATA  MEIS80  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1 -10.6, 16.0,-14.7, -7.5,   .9,
     2 -10.4, 14.5, -6.4, -3.1, -9.3,
     3  -7.5,  1.4, -3.3, -1.6, -6.6,
     4  -4.1, -4.7, -3.8,  -.1,   .8,
     5  -2.1, -4.9, -2.9,  1.2,  3.1,
     6  -1.1, -6.7, -1.5,  2.8,  3.4,
     7    .1,-12.6,  1.1,  5.6,  5.6,
     8  -1.0,-15.9,  5.0,  6.9,  7.4,
     9  -5.1,-13.2,  5.8,  6.2,  7.1,
     A  -8.2, -6.9,  5.5,  7.0,  7.5,
     B  -8.7, -8.9,  4.3,  7.7,  7.7,
     C  -7.2, -8.6,  4.0,  6.6,  4.6,
     D  -1.3, -5.5,  6.8,  9.1,  4.4,
     E   3.6, -6.4,  5.4, 11.9,  5.5,
     F   4.9, -7.9,  1.6, 11.2,  6.2,
     G   6.0,   .7,  6.8, 12.8, 12.8/
 
 
C        (SWITZERLAND) METEOLABOR BASORA
C     ====> JTYPE = 026  --  ITAB = 8  <====
 
      DATA  METEOB  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  12.1,  4.4,  6.2, -9.7,-11.3,
     2  12.1,  4.4,  6.2, -9.7,-11.3,
     3   2.2,  2.3,  2.6, -3.7, -7.3,
     4  -2.7,  1.0,  -.4, -1.4, -3.7,
     5  -1.4,  1.8,  -.6,   .0,   .3,
     6   -.4,  3.0,   .6,  1.7,  2.6,
     7  -1.2,  3.2,  1.0,  1.4,  2.3,
     8   -.8,  1.3,  1.2,   .8,  3.5,
     9   -.6, -1.4,  3.4,  2.5,  9.1,
     A  -1.7, -1.6,  4.7,  5.7, 14.0,
     B   -.8,  -.7,  3.6,  6.4, 11.6,
     C    .3,  -.6,  2.9,  4.3,  2.7,
     D  -2.5, -1.0,  2.6,  1.2, -3.1,
     E  -5.1,  1.9,  3.5,  1.0, -2.4,
     F  -4.2,  3.9,  2.7,  3.7,  -.7,
     G  -2.8,   .3, -4.6,  5.3,  1.0/
 
 
C            (RUSSIA) AVK-MRZ
C     ====> JTYPE = 027  --  ITAB = 9  <====
 
      DATA  RUSAVK  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   4.2, -2.2,  4.7,   .4,  -.4,
     2  -1.6,   .1,  1.7,  -.4,  -.4,
     3  -3.1,  1.7,  1.0,  -.3,  -.3,
     4  -1.8,  2.0,  2.4,  1.4,  1.4,
     5   -.3,  2.5,  4.8,  4.7,  4.7,
     6    .0,  2.9,  5.5,  6.9,  9.3,
     7   -.2,  1.1,  4.3,  6.1,  5.7,
     8    .4,   .1,  4.7,  7.4,  5.6,
     9   2.7,  3.3, 10.7, 14.2, 14.9,
     A   4.2,  6.0, 14.6, 18.0, 22.6,
     B   4.4,  5.9, 13.6, 16.8, 23.7,
     C   3.2,  4.7, 11.3, 11.8, 11.8,
     D   1.3,  4.1, 10.7,  8.0,  8.0,
     E   -.1,  3.5, 12.5,  6.9,  6.9,
     F  -1.0,  2.8, 15.5,  6.4,  6.4,
     G   1.5,  6.1, 19.2,  7.6,  7.6/
 
 
C           (RUSSIA) METEORIT MARZ2-1 
C     ====> JTYPE = 028  --  ITAB = 10  <====
 
      DATA  RUSZ21  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -3.3,  1.3, 10.2,  -.3,  -.3,
     2  -3.1,  -.1,   .8,  2.5,  2.5,
     3  -2.2,  1.8,  -.2,  1.4,  1.4,
     4    .0,  3.6,  1.6,  1.7,  1.7,
     5   2.7,  4.2,  3.6,  4.4,  4.4,
     6   4.1,  4.1,  3.8,  6.0,  6.0,
     7   2.8,  1.9,  2.9,  5.5,  5.5,
     8    .8,  -.7,  6.2,  9.9,  9.9,
     9   1.4,  2.0, 14.1, 21.1, 21.1,
     A   3.9,  5.8, 17.3, 26.3, 26.3,
     B   5.5,  6.3, 16.7, 25.6, 25.6,
     C   3.9,  6.4, 14.4, 22.7, 22.7,
     D   -.4,  6.0, 12.1, 21.9, 21.9,
     E  -5.1,  4.8, 11.6, 21.5, 21.5,
     F  -8.3,  5.4, 12.9, 21.8, 21.8,
     G  -6.3, 10.1, 17.9, 24.3, 24.3/
 
 
C          (RUSSIA) METEORIT MARZ2-2 
C     ====> JTYPE = 029  --  ITAB = 11  <====
 
      DATA  RUSZ22  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1 -14.2,  1.9, -2.0, 11.0, 11.0,
     2  -1.9, -5.1, -7.8,  5.3,  5.3,
     3   3.1, -2.9, -3.8,  3.2,  3.2,
     4   6.5,   .3,  2.3,  3.5,  3.5,
     5   5.6,  2.2,  5.8,  6.3,  6.3,
     6  -1.6,  1.3,  5.3,  8.7,  9.7,
     7  -9.0, -2.2,   .9,  9.5,  9.5,
     8  -9.1, -1.4,   .0, 11.4, 11.4,
     9  -1.1,  4.8,  6.9, 16.0, 16.0,
     A   4.5,  7.5, 11.2, 18.7, 18.7,
     B   4.7,  6.9, 13.7, 17.6, 17.6,
     C   1.9,  7.0, 16.0, 13.4, 13.4,
     D  -1.2,  8.2, 15.5, 11.2, 11.2,
     E  -3.1,  8.4, 12.1, 11.3, 14.4,
     F  -1.9,  6.7,  6.4,  7.3,  7.3,
     G   0.9,  7.7,  4.1,  1.8,  1.8/
 
 
C   (CHINA) SHANGHAI RADIO - PRIOR TO 1/1/2001
C     ====> JTYPE = 032  --  ITAB = 12  <====
 
      DATA  CHINSR  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -4.1,  3.0,  2.3, -9.0, -9.0,
     2   -.9,   .2,   .8, -2.9, -2.9,
     3    .9,  2.0,  1.8,  1.2,  1.2,
     4   2.9,  3.6,  4.9,  6.1,  6.1,
     5   4.5,  5.1,  8.8, 11.2, 11.2,
     6   5.2,  7.2, 11.3, 14.4, 14.4,
     7   5.2,  8.0, 11.3, 14.6, 14.6,
     8   5.1,  7.3, 10.7, 13.0, 13.0,
     9   6.8,  7.2, 10.2, 12.2, 12.2,
     A   7.6,  7.2,  8.4, 13.3, 13.3,
     B   3.2,  3.9,  2.4,  9.3,  9.3,
     C  -4.4, -2.7, -5.9,  -.1,  -.1,
     D  -8.7, -5.1, -8.1, -3.5, -3.5,
     E -10.4,   .4, -3.4, -1.1, -1.1,
     F -10.9,  8.8,  3.2,  1.4,  1.4,
     G  -6.6, 17.0,  9.2,  5.0,  5.0/
 
 
C                VAISALA RS80
C     ====> JTYPE = 037  --  ITAB = 13  <====
 
      DATA  VARS80  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -2.4,  3.4, -1.2,  2.0, -1.5,
     2   3.5, -3.6,   .9,  4.5,  1.1,
     3   3.6,  -.7,  1.0,  4.1,  1.3,
     4    .9,  -.3,  1.0,  2.3,  2.0,
     5   -.7,  -.2,   .7,  2.1,  2.7,
     6   -.6,  1.0,   .6,  3.4,  3.2,
     7    .5,  1.1,  1.2,  5.4,  4.5,
     8   1.4, -1.0,  1.0,  6.8,  5.2,
     9   1.2, -2.2,  1.1,  6.4,  5.3,
     A  -1.0,  -.5,  2.9,  4.9,  3.7,
     B  -3.5,   .7,  2.7,  3.2, -1.4,
     C  -2.0,   .4,  -.5,  3.6, -3.8,
     D   1.5,  -.1,  -.2,  6.3,  1.3,
     E   1.5,  -.2,  4.7,  7.5,  8.8,
     F  -1.1,  1.0,  5.3,  4.8, 16.8,
     G   -.6,  5.4, -1.7,  5.2, 16.4/
 
 
C            (U.S.) AIR IS-4A-1680
C     ====> JTYPE = 043  --  ITAB = 14  <====
 
      DATA  AIRR80  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -6.2,  4.6,  8.6, -8.9, -8.9,
     2  -2.7, 10.0,   .7, -1.3, -1.3,
     3   -.8,  8.0,   .8,   .9,   .9,
     4   -.6,  4.7,  1.5,  4.0,  4.0,
     5  -1.9,  -.2,   .2,  6.9,  6.9,
     6  -3.4, -4.8,  -.1,  8.2,  8.2,
     7  -3.8, -7.5,   .2,  9.2,  9.2,
     8  -4.2, -5.8,  -.5, 10.5, 10.5,
     9  -6.3,   .6, -3.7, 10.7, 10.7,
     A  -7.4,  5.4, -8.6, 12.5, 12.5,
     B  -5.7,  6.5, -8.1, 16.6, 16.6,
     C  -1.5,  6.4,  2.4, 20.9, 20.3,
     D   8.5,  6.0, 15.7, 27.7, 27.8,
     E  20.9,  4.8, 23.3, 37.1, 37.1,
     F  20.9, 18.3, 26.3, 45.2, 45.2,
     G  20.9, 18.3, 26.3, 45.2, 45.2/
 
 
C            (U.S.) AIR IS-4A-1680 X 
C     ====> JTYPE = 044  --  ITAB = 15  <====
C               (U.S.) RS MSS 
C     ====> JTYPE = 045  --  ITAB = 15  <====
 
      DATA  USRSMS  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -5.7, -5.9, -9.5,  1.3,  6.3,
     2  -3.0, -2.3,  2.3,  5.1,  5.9,
     3    .0,  -.7,  1.0,  4.3,  2.4,
     4  -1.3, -2.1,   .0,  1.6,   .0,
     5  -3.1, -3.4,   .8,   .4, -2.2,
     6  -3.3, -3.4,   .9,  -.5, -2.8,
     7  -2.9, -2.3,  1.2, -1.2, -1.8,
     8  -3.2, -2.6,  1.9, -1.3, -1.5,
     9  -4.6, -4.3,  3.0, -1.4, -2.5,
     A  -7.3, -3.8,  4.8, -1.7, -2.4,
     B  -8.8, -3.8,  3.9,  1.9,  -.5,
     C  -5.6, -2.9,  1.7, 14.2,  3.7,
     D   -.3,   .5,  3.4, 15.1, 10.7,
     E   2.8,  4.9,  8.2,  2.1, 15.1,
     F   2.6, 10.5, 14.8, -9.8, 14.7,
     G   4.1, 19.1, 29.1, -1.6, 16.9/
 
 
C             (U.S.) AIR IS-4A-403
C     ====> JTYPE = 046  --  ITAB = 16  <====
 
      DATA  AIR403  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -9.4, 13.2, -2.1, -2.6, -2.6,
     2   -.9,  1.0, -2.4,  1.8,  1.8,
     3    .8,  -.5,  1.1,  3.8, 15.0,
     4    .1,  2.7,   .8,  4.3, 14.4,
     5   -.3,  5.3,  2.4,  5.0, 11.8,
     6    .0,  5.0,  3.7,  5.1, 18.0,
     7    .2,  3.7,  1.5,  4.9, 22.6,
     8   1.3,  5.1, -1.1,  6.1, 21.8,
     9   4.1,  6.6, -2.8,  7.7, 23.1,
     A   4.2,  5.3, -4.1, 10.3, 27.7,
     B   1.5,  3.2, -5.0, 13.5, 32.4,
     C   1.3,  2.9, -6.9, 15.3, 15.3,
     D   6.4,  5.7,  4.3, 20.6, 20.6,
     E  15.1, 10.4, 21.5, 28.6, 28.6,
     F  21.7, 14.0, 25.2, 30.3, 30.3,
     G  27.5, 13.8, 21.4, 32.2, 32.2/
 
 
C             (JAPAN) MEISEI RS2-91
C     ====> JTYPE = 047  --  ITAB = 17  <====
 
      DATA  MEIS91  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -5.9, -6.7,  -.4,   .4,  -.6,
     2  -4.3,  3.2,  2.8,  -.7, -1.6,
     3  -2.7,   .9,  -.6, -2.1, -1.3,
     4  -2.2, -2.3, -5.9, -2.3,  -.7,
     5   -.9, -3.9, -4.4, -1.0, -3.0,
     6   1.9, -5.7, -2.3,  1.3, -1.2,
     7   4.7, -8.5, -2.3,  3.9,  2.9,
     8   5.2, -6.1,   .1,  5.4,  4.3,
     9   2.3,  1.1,  3.1,  4.6,  3.5,
     A  -2.4,  1.9,  2.2,  3.7,  4.8,
     B  -6.8, -1.5, -3.5,  1.7,  5.6,
     C  -8.6, -4.5, -7.3, -2.1,  1.5,
     D  -6.2, -7.3, -9.2, -2.0,   .4,
     E  -3.0, -9.4,  -.8,   .3,  1.1,
     F  -5.4, -9.4, 22.1,  1.5, -1.1,
     G  -8.6, -7.5, 39.3,  4.7,   .0/
 
 
C               (U.S.) VIZ MARK II
C     ====> JTYPE = 049  --  ITAB = 18 <====
 
      DATA  VIZMK2  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   5.0, -1.8,  3.9,  3.2, -8.4,
     2   3.0, -1.7,  2.4,  4.1, -3.0,
     3    .5, -1.8,   .8,  3.5,  -.8,
     4  -1.7, -1.7,  -.2,  2.3,  -.7,
     5  -2.9,  -.5,   .5,  2.2,  -.1,
     6  -2.5,  1.6,  1.7,  3.4,  1.3,
     7  -1.9,  3.4,  2.7,  5.1,  2.7,
     8  -1.7,  2.1,  2.7,  5.2,  3.4,
     9  -1.2,  -.7,  1.9,  3.6,  2.3,
     A  -2.1,  -.8,  3.2,  3.2,   .4,
     B  -4.2,   .3,  4.5,  4.1,   .7,
     C  -4.2,   .6,  4.3,  5.3,  5.4,
     D  -3.2,   .9,  5.8,  8.5, 10.1,
     E  -3.7,  1.9, 10.2, 14.0, 12.4,
     F  -7.5,  3.0, 13.7, 17.9, 14.3,
     G  -9.0,  4.8, 11.6, 16.4, 18.0/
 
 
C             (GERMANY) GRAW DFM-90
C     ====> JTYPE = 050  --  ITAB = 19  <====
 
      DATA  GRAW90  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   -.3,  2.6,  2.2,-10.1,-20.9,
     2   -.3, -2.1,  4.6,  5.9, -3.7,
     3  -1.3, -2.0,  2.4,  4.7, -2.0,
     4  -2.7,  -.9,  1.6,  2.9, -1.2,
     5  -2.8,  1.0,  3.3,  4.2,  3.6,
     6   -.7,  4.0,  5.8,  7.2,  7.2,
     7   2.8,  7.0,  7.6,  9.3,  8.5,
     8   4.0,  6.9,  7.8,  8.9,  9.4,
     9   2.4,  4.5,  7.1,  8.5,  8.5,
     A   1.8,  3.9,  7.7, 10.7, 10.7,
     B   2.7,  4.3,  8.5, 11.3, 11.3,
     C   2.8,  5.4,  8.9,  7.8,  7.8,
     D   2.7,  9.7, 10.4,  4.5,  4.5,
     E   2.2, 15.3, 12.0,  2.8,  2.8,
     F   1.4, 19.6, 10.5,  1.1,  1.1,
     G   4.7, 25.1,  8.5,  1.1,  1.1/
 
 
C                 (U.S.) VIZ-B2
C     ====> JTYPE = 051  --  ITAB = 20  <====
 
      DATA  USVZB2  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   -.7,   .8, -1.3,  2.7,  5.3,
     2  -3.1, -2.5, -1.0,  4.4,  5.9,
     3  -4.0, -2.1, -1.9,  2.9,  3.4,
     4  -4.6, -1.9, -1.6,  -.8,   .6,
     5  -4.5, -1.5,  -.1, -2.0, -1.7,
     6  -3.9,   .0,  1.5,  -.8,  -.1,
     7  -2.7,  1.8,  3.1,  1.9,  3.7,
     8  -1.8,  1.4,  4.3,  4.0,  3.8,
     9  -1.9,   .8,  5.7,  5.2,  2.0,
     A  -1.1,  2.4,  8.6,  9.1,  3.4,
     B  -1.5,  2.0,  9.0, 11.3,  7.4,
     C  -4.8,  -.5,  6.8,  9.8, 11.3,
     D  -8.2, -1.8,  6.0, 10.6, 14.5,
     E -11.0,  -.3,  7.6, 13.0, 14.6,
     F -11.6,  3.0,  9.9, 12.7,  8.2,
     G  -8.4,  7.4, 12.8,  9.3,  -.3/
 
 
C               VAISALA RS8-57H
C     ====> JTYPE = 052  --  ITAB = 21  <====
 
      DATA  MICROA  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1   5.5,  1.3,  1.3,  3.9,  5.3,
     2   -.8, -3.1,  -.7,  1.8,  2.5,
     3  -3.5, -3.5, -3.2,   .1,   .4,
     4  -3.7, -3.2, -3.7, -1.6, -2.0,
     5  -2.6, -2.5, -2.9, -2.7, -5.1,
     6  -1.7, -1.1, -2.4, -2.9, -4.9,
     7   -.9,   .6, -2.0, -3.0, -2.2,
     8    .2,   .0, -2.1, -3.6, -2.5,
     9   1.4, -1.7, -2.9, -4.0, -6.4,
     A   2.2,  -.9, -2.4, -1.3, -8.8,
     B    .5, -2.3, -3.3,  1.5, -7.3,
     C  -3.6, -5.5, -7.0, -1.2, -3.5,
     D  -6.0, -6.7, -9.1, -5.4,  -.8,
     E  -6.2, -5.3, -7.8, -6.6,   .1,
     F  -4.7, -1.9, -4.7, -5.5, -7.8,
     G    .0,  3.1,  -.4, -2.2,-16.4/
 
 
C             VAISALA RS80/MICROCORA
C     ====> JTYPE = 060  --  ITAB = 22  <====
 
      DATA  MICORA  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1    .4,  -.2,  8.5,  2.2, -5.9,
     2  -2.7, -4.8, -1.2, -5.7, -4.1,
     3  -3.9, -1.5,  1.3, -2.9, -4.5,
     4  -1.5,  4.6,  4.6,   .3, -2.6,
     5    .9,  5.1,  3.4,  1.0,   .8,
     6   1.2,  2.1,  -.1,   .6,  1.3,
     7    .6, -2.1, -3.7,   .8,   .2,
     8   1.2, -7.1, -7.0,  1.0,  2.3,
     9   2.0, -8.9, -9.4,   .1,  4.4,
     A  -1.3, -6.4, -7.8, -3.3,  2.6,
     B  -6.2, -5.2, -8.1, -8.1, -2.2,
     C  -3.7, -3.2, -8.2, -7.6, -7.6,
     D   1.3, -1.5, -3.5, -3.6, -3.6,
     E   2.7,   .0,  1.8, -1.2, -1.2,
     F   3.4,  3.2,  6.4,  1.4,  1.4,
     G   5.5,  7.7, 14.3,  5.2,  5.2/
 
 
C     VAISALA RS80/LORAN/DIGICORA OR MARWIN
C     ====> JTYPE = 061  --  ITAB = X  <====
C       (baseline for all corrections)
 
      DATA  DICORA  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1    .0,   .0,   .0,   .0,   .0,
     2    .0,   .0,   .0,   .0,   .0,
     3    .0,   .0,   .0,   .0,   .0,
     4    .0,   .0,   .0,   .0,   .0,
     5    .0,   .0,   .0,   .0,   .0,
     6    .0,   .0,   .0,   .0,   .0,
     7    .0,   .0,   .0,   .0,   .0,
     8    .0,   .0,   .0,   .0,   .0,
     9    .0,   .0,   .0,   .0,   .0,
     A    .0,   .0,   .0,   .0,   .0,
     B    .0,   .0,   .0,   .0,   .0,
     C    .0,   .0,   .0,   .0,   .0,
     D    .0,   .0,   .0,   .0,   .0,
     E    .0,   .0,   .0,   .0,   .0,
     F    .0,   .0,   .0,   .0,   .0,
     G    .0,   .0,   .0,   .0,   .0/
 
 
C              VAISALA RS80/PCCORA
C     ====> JTYPE = 062  --  ITAB = 24  <====
 
      DATA  PCCORA  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -4.5, -5.0, -5.3, -3.3, -7.7,
     2  -1.4, -3.7, -3.3, -1.1, -4.2,
     3  -1.0, -2.1, -2.0,  -.2, -3.8,
     4  -1.1, -1.1,  -.8,  -.5, -2.0,
     5   -.5,  -.3,  -.3,  -.1,   .3,
     6    .4,   .9,  -.3,  1.3,  2.5,
     7    .9,  2.6,   .1,  2.3,  3.5,
     8    .7,  2.3,   .2,  2.3,  3.2,
     9    .5,   .3,  -.8,   .7,  3.0,
     A    .0, -1.0, -2.9, -1.4,  2.0,
     B    .2, -1.5, -4.3, -2.4, -1.7,
     C   1.3, -1.2, -3.1, -2.1, -6.0,
     D   1.6,  -.4,  -.7, -1.6, -7.0,
     E    .7,  1.7,  2.8,  -.4, -6.6,
     F  -1.6,  4.5,  6.0,  1.0, -4.9,
     G   -.9,  8.6,  6.8,  4.4,  1.8/
 
 
C              VAISALA RS80/STAR 
C     ====> JTYPE = 063  --  ITAB = 25  <====
 
      DATA  VASTAR  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -1.1, -2.4,  8.3, 10.8,  4.6,
     2   -.6,  1.3,  6.0,  4.1,   .7,
     3  -1.0,  1.9,  3.4,  1.9,   .4,
     4  -1.2,  1.8,  1.6,   .5,   .7,
     5  -1.0,  1.7,   .7,  -.2,   .6,
     6    .4,  1.4,   .6,  -.5,   .8,
     7   2.9,  1.5,  1.4,  -.5,  2.1,
     8   3.9,  1.2,  1.1, -1.2,  2.9,
     9   1.4,  1.2,  -.4, -3.2,  1.2,
     A  -1.5,  2.3, -1.1, -4.6, -2.3,
     B  -1.1,  2.5, -1.2, -2.8, -2.8,
     C   2.3,  2.5,   .0,   .8,   .4,
     D   4.7,  3.1,  3.0,  2.8,  1.2,
     E   2.6,  4.4,  6.8,  5.2,   .0,
     F  -2.4,  7.3, 11.3,  8.1,  -.5,
     G  -1.3, 14.2, 15.7, 13.1,  4.3/
 
 
C       VAISALA RS90/DIGICORA I,II OR MARWIN
C     ====> JTYPE = 071  --  ITAB = 26  <====
 
      DATA  VARS90  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -3.6,  -.1, -3.8,-13.5, -6.9,
     2  -4.0, -2.9, -3.4, -6.7, -4.8,
     3  -4.5, -2.3, -3.4, -5.3, -5.9,
     4  -3.7, -1.4, -2.2, -4.7, -5.5,
     5  -2.5,  -.9, -1.5, -3.6, -3.7,
     6  -2.6,  -.2, -1.2, -2.0, -2.0,
     7  -2.6,   .9,  -.4,  -.1, -1.1,
     8   -.6,   .7,   .2,  2.1,   .5,
     9   1.8,   .0,  1.3,  5.1,  4.5,
     A   1.7,   .0,  3.2,  7.5,  9.1,
     B    .7,  -.4,  2.9,  6.6, 11.5,
     C   -.4, -1.3,   .4,  1.1,  6.0,
     D  -1.7, -1.4,  -.6, -1.7,  1.6,
     E  -3.7,  -.4,  1.7,  1.7,  2.6,
     F  -4.0,  1.5,  5.7,  7.1,  6.1,
     G   2.7,  7.4, 12.6, 14.3, 14.1/
 
 
C            (RUSSIA) AVK-RF95-ARMA
C     ====> JTYPE = 076  --  ITAB = 27  <====
 
      DATA  AVKR95  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -2.1,  1.6, -7.8, 11.8, 11.8,
     2  -6.2, -1.2, -4.1,  4.8,  4.8,
     3  -4.7,  1.2, -1.2,  1.5,  1.5,
     4  -3.0,  2.3,  1.5,  -.6,  -.6,
     5  -3.9,  1.5,  1.7, -1.1, -1.1,
     6  -2.8,   .0, -2.1, -2.3, -2.3,
     7  -1.0, -3.1, -8.9, -5.3, -5.3,
     8    .6, -4.1,-13.3, -2.3, -2.3,
     9   3.3,  -.5, -2.6,  7.1,  7.1,
     A   3.8,   .5,  7.4, 11.2, 11.2,
     B   3.2,  1.4, 10.8, 12.3, 12.3,
     C   -.9,  2.3,  9.8,  8.5,  8.5,
     D  -4.1,   .7,  6.9,  3.8,  3.8,
     E   -.5, -2.4,  4.2,  1.1,  4.4,
     F   2.6, -5.8,  1.3, -1.0,  8.0,
     G   2.6, -5.9,  1.3,  1.9, 16.2/


C    (CHINA) SHANGHAI RADIO - AFTER 12/31/2000
C     ====> JTYPE = 032  --  ITAB = 28  <====

      DATA  CHINSX  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  -4.1,  3.0,  2.3, -9.0, -9.0,
     2   -.9,   .2,   .8, -2.9, -2.9,
     3    .9,  2.0,  1.8,  1.2,  1.2,
     4    .0,   .0,   .0,   .0,   .0,
     5    .0,   .0,   .0,   .0,   .0,
     6    .0,   .0,   .0,   .0,   .0,
     7    .0,   .0,   .0,   .0,   .0,
     8    .0,   .0,   .0,   .0,   .0,
     9    .0,   .0,   .0,   .0,   .0,
     A    .0,   .0,   .0,   .0,   .0,
     B    .0,   .0,   .0,   .0,   .0,
     C    .0,   .0,   .0,   .0,   .0,
     D    .0,   .0,   .0,   .0,   .0,
     E -10.4,   .4, -3.4, -1.1, -1.1,
     F -10.9,  8.8,  3.2,  1.4,  1.4,
     G  -6.6, 17.0,  9.2,  5.0,  5.0/
 
 
C      RADIOSONDE NOT SPECIFIED OR UNKNOWN
C     ====> JTYPE = 090  --  ITAB = X  <====
C            (no longer corrected)
 
      DATA  UNKN90  /
C       NITE   -10    15    30    60  Solar Angle
C        ___   ___   ___   ___   ___
C LEVEL
     1  11.2,   .4, -9.9,-15.6,-15.6,
     2   3.9,  1.5, -2.2, -8.9, -8.9,
     3  -1.6,   .5,  -.7, -4.6, -4.6,
     4  -3.2, -2.9, -1.0, -3.4, -3.4,
     5  -3.1, -2.4,  -.1, -2.4, -2.4,
     6  -3.1,  -.2,  1.0,  -.9,  -.9,
     7  -3.0,  1.7,  2.1,  1.4,  1.4,
     8  -1.2,  1.4,  3.0,  4.1,  4.1,
     9   2.9,   .8,  4.1,  6.0,  6.0,
     A   4.0,  4.4,  6.7,  7.0,  7.0,
     B   -.2,  5.8,  8.9,  7.3,  7.3,
     C  -4.1,  3.9,  9.0,  7.0,  6.3,
     D  -7.2,  2.6,  8.9,  9.2, 10.8,
     E  -8.1,  6.0, 12.2, 15.0, 15.0,
     F  -3.5,  9.5, 16.3, 19.0, 19.0,
     G   4.5,  8.0, 15.6, 21.7, 21.7/
 

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

C  EACH TIME THROUGH, COMPUTE LOGS OF PRESSURE RATIOS
C  --------------------------------------------------

      ALP(1) = 0.0
      DO I=2,16
         ALP(I) = ALOG(PRES(I-1)/PRES(I))
      ENDDO

C  GET TABLE INDEX FROM RAOB TYPE - CHECK FOR VALID CORR. PARAMETERS
C  -----------------------------------------------------------------

      ITAB = 0
      IF(JTYPE.GT.000.AND.JTYPE.LT.256) THEN
         ITAB = ITYPTT(JTYPE)
         if (CORUSVAI .and. jtype .eq. 52) ITAB=0
         IF(ITAB.LT.-3 .OR. (ITAB.GT.29.AND.ITAB.LT.101) .OR.
     .      ITAB.GT.129)  THEN
            WRITE(6,4)  SID(IS),JTYPE,ITAB
            WRITE(68,4) SID(IS),JTYPE,ITAB
4        FORMAT(2X,'* * * STN ',A8,' WITH RAOB TYPE ',I5.3,' HAS AN ',
     .    'INVALID CORRECTION TABLE ASSIGNMENT OF ',I5,' - NO ',
     .    'CORRECTION POSSIBLE')
            IRET = -1
            RETURN
         ELSE  IF(ITAB.EQ.0)  THEN
            IRET = -1
            RETURN
         ELSE  IF(ITAB.EQ.-2)  THEN
            WRITE(6,3)  SID(IS),JTYPE
            WRITE(68,3) SID(IS),JTYPE
3        FORMAT(2X,'* * * STN ',A8,' HAS A VACANT RAOB TYPE ',I5.3,
     .    ' BASED ON 11/2008 BUFR C. TBL (IT MAY NOW BE ASSIGNED?) - ',
     .    'NO CORRECTION POSSIBLE')
            IRET = -1
            RETURN
         ELSE  IF(ITAB.EQ.-1)  THEN
            WRITE(6,2)  SID(IS),JTYPE
            WRITE(68,2) SID(IS),JTYPE
2        FORMAT(2X,'* * * STN ',A8,' HAS OBSOLETE RAOB TYPE ',I5.3,
     .    ' BASED ON 11/2008 BUFR C. TBL (WILL NEVER AGAIN BE USED) - ',
     .    'NO CORRECTION POSSIBLE')
            IRET = -1
            RETURN
         ELSE  IF(ITAB.EQ.-3)  THEN
            WRITE(6,1)  SID(IS),JTYPE
            WRITE(68,1) SID(IS),JTYPE
            IRET = -1
            RETURN
         ELSE  IF(ITAB.GT.100)  THEN
            WRITE(6,5)  SID(IS),JTYPE,ITAB-100
            WRITE(68,5) SID(IS),JTYPE,ITAB-100
5        FORMAT(2X,'* * * STN ',A8,' HAS OBSOLETE RAOB TYPE ',I5.3,
     .    ' BASED ON 11/2008 BUFR C. TBL (NEVER AGAIN USED), STILL IT ',
     .    'USES CORR. TABLE ',I2)
            ITAB = ITAB - 100
         END IF
         IF(ITAB.EQ.12 .AND. IDT.GE.12097440) THEN

C  AFTER 12/31/00 USE CORRECTION TBL 28 FOR CHINA SHANGHAI RADIO SONDES
C  --------------------------------------------------------------------

            ITAB = 28
         END IF
      ELSE
         WRITE(6,1)  SID(IS),JTYPE
         WRITE(68,1) SID(IS),JTYPE
1        FORMAT(2X,'* * * STN ',A8,' HAS AN INVALID RAOB TYPE ',I8.3,
     .    ' - NO CORRECTION POSSIBLE')
         IRET = -1
         if(jtype.eq.20000)  iret = -99
         RETURN
      ENDIF

C  GET SOLAR ELEVATION ANGLE ON ALL VALID MANDATORY LEVELS
C  -------------------------------------------------------

      CALL SOELAN(SOLAR)

C  APPLY TEMPERATURE AND HEIGHT ADJUSTMENTS
C  DO NOT APPLY AN ADJUSTMENT BELOW 700 HPA
C  ----------------------------------------

      DTS = 0.
      DHS = 0.
      DO ILEV=4,16
         ANGLE(2)  = CUTOFF(ILEV)

         IF(SOLAR(ILEV).GE.CUTOFF(ILEV))  THEN
            IF(SOLAR(ILEV).LT.ANGLE(5)) THEN
               DO K=3,5
                  IF(SOLAR(ILEV).LT.ANGLE(K)) THEN
                     TABU = TTAB(K  ,ILEV,ITAB)
                     TABD = TTAB(K-1,ILEV,ITAB)
                     DSUN=(SOLAR(ILEV)-ANGLE(K-1))/(ANGLE(K)-ANGLE(K-1))
                     DTS(ILEV) = TABD + DSUN*(TABU-TABD)
                     GO TO 10
                  ENDIF
               ENDDO
            ELSE
               DTS(ILEV) = TTAB(5,ILEV,ITAB)
            ENDIF
         ELSE

C  IF ANGLE < CUTOFF (NIGHT), CORRECTION FROM 'NIGHT' VALUE IN TBL
C  --------------------------------------------------------------------

            DTS(ILEV) = TTAB(1,ILEV,ITAB)
         ENDIF

C  The constant is (R/(20*g)) = 1.46355
C  ------------------------------------

   10    CONTINUE
         HYF = 1.46355*ALP(ILEV)
         IF(ILEV.EQ.1) THEN
            DHS(ILEV) = HYF*DTS(ILEV)
         ELSE
            DHS(ILEV) = DHS(ILEV-1) + HYF*(10.*DTS(ILEV-1)+DTS(ILEV))
         ENDIF

C  CORRECT DTS BY A FACTOR OF 10
C  -----------------------------

         DTS(ILEV) = .1*DTS(ILEV)

C  SAVE AND APPLY THE CORRECTIONS
C  ------------------------------

         DHT(ILEV) = -DHS(ILEV)
         DTP(ILEV) = -DTS(ILEV)

         IF(HGT(ILEV).LT.BMISS) HGT(ILEV) = HGT(ILEV) + DHT(ILEV)
         IF(TMP(ILEV).LT.BMISS) TMP(ILEV) = TMP(ILEV) + DTP(ILEV)

      ENDDO

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RSTATS      ACCUMS STATS FOR INTERSONDE CORRECTIONS
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 2002-07-22
C
C ABSTRACT: ACCUMULATES STATISTICS INCLUDING BIAS, RMS AND MAXIMUM
C   DIFFERENCE FOR EACH UNCORRECTED VS. CORRECTED "ADPUPA" MASS
C   REPORT'S HEIGHT OR TEMPERATURE ON ALL MANDATORY PRESSURE LEVELS.
C   ENTRY LSTATS COMPUTES THE THE ABOVE STATISTICS FOR PRINT OUTPUT.
C
C PROGRAM HISTORY LOG:
C   UNKNOWN   G. D. DIMEGO
C 1990-12-06  D. A. KEYSER --  CONVERTED TO VS FORTRAN (77) AND
C     RESTRUCTURED; COMBINED OLD SUBR. RSTATS AND TSTATS WHICH DID
C     HGHT AND TEMP. CALC. SEPARATELY
C 2002-07-22  D. A. KEYSER --  CORRECTED ERROR WHICH DID NOT ADJUST
C     SCALE FOR TEMPERATURE MEAN AND RMS AND HEIGHT RMS IN FINAL
C     PRINTOUT WHEN THERE WAS ONLY ONE STATION IN SAMPLE
C
C USAGE:    CALL RSTATS(IVAR,ZIN,ZOUT,XI,YJ,KP)
C   INPUT ARGUMENT LIST:
C     IVAR     - INTEGER INDICATING VARIABLE FOR WHICH STATS ARE
C              - GENERATED (=1-HEIGHT; =2-TEMPERATURE)
C     ZIN      - REAL UNCORRECTED VARIABLE {HGHT(M), TEMP(*10 DEG. C)}
C     ZOUT     - REAL CORRECTED VARIABLE {HGHT(M), TEMP(*10 DEG. C)}
C     XI       - REAL STATION LONGITUDE (DEG. WEST *100)
C     YJ       - REAL STATION LATITUDE (DEG. NORTH *100)
C     KP       - INTEGER MANDATORY PRESSURE LEVEL (MB)
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 68  - RADCOR INFORMATION FILE
C
C REMARKS: CALLED BY SUBROUTINE "RADEVN".  ENTRY LSTATS CALLED BY
C   SUBROUTINE "DMA22".
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE RSTATS(IVAR,ZIN,ZOUT,XI,YJ,KP)
      LOGICAL  INIT
      REAL(8) BMISS
      REAL  STATS(8,21,2)
      INTEGER  IMAX(21,2),JMAX(21,2),IPMAND(21)

      COMMON/SWITCH/LWCORR,LEVRAD,IRCTBL,HGTTBL,BAL_DRIFT
      COMMON /TESTS/   TEST
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL HGTTBL, TEST
      DATA  INIT/.TRUE./,IPMAND/1000,925,850,700,500,400,300,250,200,
     $ 150,100,70,50,30,20,10,7,5,3,2,1/
      SAVE INIT,STATS,IMAX,JMAX,IPMAND

C  FIRST TIME THRU SET ALL STATISTICAL VARIABLES TO ZERO
C  -----------------------------------------------------

      IF(INIT)  THEN
        DO K=1,2
          DO J=1,21
            IMAX(J,K) = 0.0
            JMAX(J,K) = 0.0
          ENDDO
        ENDDO
        DO K=1,2
          DO J=1,21
            DO I=1,8
              STATS(I,J,K) = 0.0
            ENDDO
          ENDDO
        ENDDO
        INIT = .FALSE.
      END IF

      DO I=1,21
         K = I
         IF(KP.GE.IPMAND(I))  GO TO 11
      ENDDO
      RETURN
   11 CONTINUE

C  ACCUMULATE COUNT AND VARIOUS SUMS
C  ---------------------------------

      IF(ABS(ZOUT)+ABS(ZIN).GE.0.5*BMISS) RETURN
      ZDIF = ZOUT - ZIN
      STATS(8,K,IVAR) = STATS(8,K,IVAR) + 1.
      STATS(1,K,IVAR) = STATS(1,K,IVAR) + ZIN
      STATS(2,K,IVAR) = STATS(2,K,IVAR) + ZOUT
      STATS(3,K,IVAR) = STATS(3,K,IVAR) + ZDIF
      STATS(4,K,IVAR) = STATS(4,K,IVAR) + (ZIN * ZIN)
      STATS(5,K,IVAR) = STATS(5,K,IVAR) + (ZOUT * ZOUT)
      STATS(6,K,IVAR) = STATS(6,K,IVAR) + (ZDIF * ZDIF)

C  FIND REPORT WITH MAXIMUM DIFFERENCE
C  -----------------------------------

      IF(ABS(ZDIF).GT.STATS(7,K,IVAR)) THEN
         STATS(7,K,IVAR) = ABS(ZDIF)
         IMAX(K,IVAR) = REAL(XI)
         JMAX(K,IVAR) = REAL(YJ)
      ENDIF
      RETURN

C  COMPUTE AND PRINT OUT STATISTICS -- ENTRY LSTATS
C  ------------------------------------------------

         ENTRY LSTATS(IVAR)

      IF(IVAR.EQ.1) THEN
         PRINT 20
         WRITE(68,20)
         SCALE = 1.0
      ELSE
         PRINT 920
         WRITE(68,920)
         SCALE = 10.0
      ENDIF

      DO K=1,21

         COUNT = AMAX1(1.0,STATS(8,K,IVAR))
cdakcdak IF(COUNT.GT.1.) THEN
         IF(COUNT.GT.0.) THEN
           COUNT = 1.0/COUNT
           DO I=1,3
             IF(STATS(I,K,IVAR) * COUNT .NE. 0.)
     &       STATS(I,K,IVAR)   = (STATS(I,K,IVAR) * COUNT)/SCALE
             IF(STATS(I+3,K,IVAR) * COUNT .NE. 0.)
     &       STATS(I+3,K,IVAR) = (SQRT(STATS(I+3,K,IVAR)*COUNT))/SCALE
           ENDDO
           IF(STATS(7,K,IVAR) .NE. 0.)
     &     STATS(7,K,IVAR) = STATS(7,K,IVAR)/SCALE
         ENDIF
      ENDDO

      DO K=21,1,-1
         IF(K.GE.LEVRAD) THEN
            KNT = IFIX(STATS(8,K,IVAR))
            PRINT 30, KNT,IPMAND(K),(STATS(I,K,IVAR),I=1,3),
     $       (STATS(I,K,IVAR),I=6,7),IMAX(K,IVAR),JMAX(K,IVAR)
            WRITE(68,30) KNT,IPMAND(K),(STATS(I,K,IVAR),I=1,3),
     $       (STATS(I,K,IVAR),I=6,7),IMAX(K,IVAR),JMAX(K,IVAR)
         ENDIF
      ENDDO

      RETURN

   20 FORMAT(//27X,'***** STATISTICAL ANALYSIS BY PRESSURE FOR INTERSO',
     $ 'NDE HEIGHT CORRECTIONS *****'//3X,'COUNT',5X,'PRESSURE',3X,'UN',
     $ 'CORR. MEAN',3X,'CORRECTED MEAN',7X,'BIAS',12X,'RMS',10X,'MAXDI',
     $ 'F',4X,'AT',4X,'LON*100',8X,'LAT*100',/,15X,'(MB)',11X,'(M)',12X,
     $ '(M)',13X,'(M)',12X,'(M)',12X,'(M)',10X,'(DEG. W)',7X,'(DEG. N)')
  920 FORMAT(//24X,'***** STATISTICAL ANALYSIS BY PRESSURE FOR INTERSO',
     $ 'NDE TEMPERATURE CORRECTIONS *****',//,3X,'COUNT',5X,'PRESSURE',
     $ 3X,'UNCORR. MEAN',3X,'CORRECTED MEAN',7X,'BIAS',12X,'RMS',10X,
     $ 'MAXDIF',4X,'AT',4X,'LON*100',8X,'LAT*100',
     $ /,15X,'(MB)',8X,'(DEG. C)',7X,'(DEG. C)',8X,'(DEG. C)',
     $ 7X,'(DEG. C)',7X,'(DEG. C)',8X,'(DEG. W)',7X,'(DEG. N)')
   30 FORMAT(I7,5X,I6,7X,5(F10.3,5X),3X,2(I7,8X))

      END

C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C
C SUBPROGRAM: PILNLNP        PRESSURE INTERPOLATION LINEAR IN LOG P
C   PRGMMR: WOOLLEN          ORG: W/NMC22    DATE: 92-07-29
C
C ABSTRACT: FUNCTION WHICH GIVEN AN PROFILE OF DESCENDING PRESSURES
C   AND A PROFILE OF QUANTITIES VALID AT THOSE PRESSURES, RETURNS
C   AN INTERPOLATED QUANTITY VALID AT A GIVEN ARBITRARY PRESSURE.
C
C PROGRAM HISTORY LOG:
C   86-03-21  G. DIMEGO
C   88-11-24  D. DEAVEN RECODED FOR CYBER 205
C
C USAGE:    X = PILNLNP(P,PARAY,QARAY,KMAX)
C
C   INPUT ARGUMENTS:
C     P       - PRESSURE TO INTERPOLATE TO
C     PARAY   - GIVEN PRESSURE PROFILE
C     QARAY   - QUANTITIES VALID FOR PRESSURE PROFILE
C     KMAX    - LENGTH OF PROFILE
C
C   FUNCTION RETURN VALUE
C     PILNLNP - INTERPOLATED QUANTITY
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      FUNCTION PILNLNP(P,PARAY,QARAY,KMAX)

      REAL(8) BMISS

      DIMENSION PARAY(KMAX),QARAY(KMAX)

      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      PILNLNP = 0.

C  FIND ADJACENT LEVELS
C  --------------------

      DO LA=1,KMAX
         IF(P.GE.PARAY(LA)) EXIT
      END DO

      IF(LA.GT.KMAX) THEN
         LA = KMAX
         LB = KMAX
      ELSE IF(LA.EQ.1) THEN
         LA = 1
         LB = 1
      ELSE
         LB = LA-1
      ENDIF

C  INTERPOLATE IF BOTH ADJACENT VALUES PRESENT
C  -------------------------------------------

      IF(QARAY(LA).LT.BMISS .AND. QARAY(LB).LT.BMISS) THEN
         PA = PARAY(LA)
         PB = PARAY(LB)
         IF(PA.NE.PB) THEN
            WK = LOG(P/PB) / LOG(PA/PB)
         ELSE
            WK = 0.
         ENDIF
         PILNLNP = QARAY(LB) + (QARAY(LA)-QARAY(LB)) * WK
      ENDIF

      RETURN
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SOELAN
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 2008-10-08
C
C ABSTRACT: DETERMINES THE SOLAR ELEVATION ANGLE ON ALL OBSERVED
C   MANDATORY PRESSURE LEVELS FOR A REPORT BASED ON THE TIME THE
C   BALLOON CROSSES THE MANDATORY LEVEL (BASED ON THE ESTIMATED
C   BALLOON DRIFT DELTA-TIME ON LEVEL, THE DAY OF THE YEAR FOR
C   THE CURRENT DAY, THE NUMBER OF DAYS IN THE YEAR FOR THE CURRENT
C   YEAR, AND THE LATITUDE AND LONGITUDE OF THE BALLOON WHEN IT
C   CROSSES THE MANDATORY LEVEL (BASED ON THE ESTIMATED BALLOON DRIFT
C   COORDINATES ON THE LEVEL).  WHEN BALLOON DRIFT TIME AND LOCATION
C   COORDINATES ARE NOT AVAILABLE (AS FOR "OLDER" DATA), THE SUN
C   ANGLE IS ESTIMATED AT BOTH THE LOWEST POSSIBLE CORRECTED
C   MANDATORY LEVEL (EITHER 100 MB OR 700 MB DEPENDING UPON
C   WHICH TABLES ARE BEING USED) AND AT 10 MB, BASED ON AN
C   ESTIMATED BALLOON CROSSING TIME (FROM OBS. TIME AND CONSTANT
C   TIME DIFFERENCE VALUE) AND OBS. LAT/LON., AND THE SUN ANGLE IS
C   INTERPOLATED TO THE REMAINING MANDATORY LEVELS.
C
C PROGRAM HISTORY LOG:
C 2002-07-19  D. A. KEYSER -- ORIGINAL AUTHOR
C 2008-10-08  D. KEYSER    -- MODIFIED TO RE-INDEX ARRAY "SOLAR" (SUN
C     ANGLE ON MAND. LVLS) FOR CASES INVOLVING THE OLDEST SET OF
C     CORRECTIONS AND WHERE BALLOON DRIFT INFORMATION WAS NOT
C     AVAILABLE SO THAT PROPER VALUES ARE RETURNED TO CALLING SUBR.
C     RADT1 (ONLY APPLIES TO REANALYSIS SINCE SUBR. RADT1 IS NEVER
C     CALLED IN CURRENT REAL-TIME RUNS);
C
C USAGE:    CALL SOELAN(SOLAR)
C   OUTPUT ARGUMENT LIST:
C     SOLAR    - 16-WORD ARRAY CONTAINING SOLAR ELEVATION ANGLE ON
C                ALL MANDATORY PRESSURE LEVELS OVER WHICH CORRECTIONS
C                ARE MADE
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 68  - RADCOR INFORMATION FILE
C
C REMARKS: CALLED BY SUBROUTINES "RADT1", "RADT2", "RADT3", "RADT4"
C   AND "RADT5".
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE SOELAN(SOLAR)

      PARAMETER (NST=1500)

      REAL(8) BMISS

      COMMON /STN/     IS
      COMMON /HEADER/  SID(NST), DHR(NST), XOB(NST), YOB(NST),
     &                 ELV(NST), SQN(NST), ITP(NST), NLV,
     &                 NEV, ISF(NST), NLVM, NLVW
      COMMON /PMAND /  PRES(16),KMIN,KMAX,INM(16)
      COMMON /SUNNY /  IDAYR,IDAYSY,TSNOON
      COMMON /ALLSND/  POB(255), PFC(255), PQM(255), PRC(255),
     &                 ZOB(255), ZFC(255), ZQM(255), ZRC(255),
     &                 TOB(255), TFC(255), TQM(255), TRC(255),
     &                 TDO(255), TDFC(255),TVO(255),
     &                 QOB(255), QFC(255), QQM(255), QRC(255),
     &                 CAT(255), IND(255), LEVTYP(255),
     &                 PS(NST),  GESPS(NST),LST(255), HRDR(255),
     &                 YDR(255), XDR(255)
      COMMON /DATEX/   IDATE(5), CDATE
      COMMON/SWITCH/   LWCORR,LEVRAD,IRCTBL,HGTTBL,BAL_DRIFT
      COMMON /BUFRLIB_MISSING/BMISS,XMISS,IMISS

      LOGICAL BAL_DRIFT
      REAL    SOLAR(16)

      CHARACTER*8      SID
      CHARACTER*10     CDATE

      SOLAR = BMISS

      DO M=KMIN,KMAX
         IF(.NOT.BAL_DRIFT .OR. (INM(M).GT.0 .AND. INM(M).LE.NLV)) THEN
            ADDTIM = BMISS
            IF(BAL_DRIFT)  THEN
               ADDTIM = HRDR(INM(M)) ! Balloon est. to cross this pres.
               XLON = XDR(INM(M))    !  lvl at balloon-drift d-time on
               YLAT = YDR(INM(M))    !  lvl (rel. to BUFR message hour)
                                     !  and at balloon-drift lat/lon
            ELSE

C  WHEN BALLOON DRIFT COORDINATES ARE NOT AVAILABLE (BAL_DRIFT=F),
C   MUST DETERMINE SUN ANGLE VIA "OLD" METHOD: THIS MEANS COMPUTING
C   SOLAR ELEVATION ANGLE AT LOWEST CORRECTED LEVEL AND AT 10 MB LEVEL
C   THEN INTERPOLATING BETWEEN THE TWO
C  -------------------------------------------------------------------
C
               IF(PRES(M).EQ.100. .AND. IRCTBL.EQ.1)  THEN
                  ADDTIM = DHR(IS) - 0.5  ! For IRCTBL=1 corrections
                                          !  start at 100mb - balloon
                                          !  est. to cross this lvl at
                                          !  obs. d-time (rel. to BUFR
                                          !  message hour) plus 0.5
                                          !  hours
               ELSE  IF(PRES(M).EQ.700. .AND. IRCTBL.GT.1)  THEN
                  ADDTIM = DHR(IS) - 0.2  ! For IRCTBL>1 corrections
                                          !  start at 700mb (1000mb for
                                          !  IRCTBL=3) - balloon est.
                                          !  to cross 700mb at obs.
                                          !  d-time minus 0.2 hours
               ELSE  IF(PRES(M).EQ.10.)  THEN
                  ADDTIM = DHR(IS) + 1.3  ! For all tables, corrections
                                          !  end at 10mb - balloon est.
                                          !  to cross this lvl at obs.
                                          !  d-time plus 1.3 hours
               END IF
               XLON = XOB(IS)    !  Balloon lat/lon on this lvl is
               YLAT = YOB(IS)    !   obs. lat/lon
            END IF
            IF(ADDTIM.GE.BMISS)  CYCLE
            BALTM = IDATE(5) + ADDTIM

C  AFY is angular fraction of year
C  -------------------------------

            AFY = 6.2831853 *(REAL(IDAYR)-1. + (BALTM/24.))/REAL(IDAYSY)
            SAFY = SIN(AFY)
            CAFY = COS(AFY)

C  SSOD is sine of solar declination
C  ---------------------------------

            SSOD = .3978492 * SIN(4.88578 + AFY + (.033420 * SAFY)
     &             - (.001388 * CAFY) + (.000696 * SAFY * CAFY)
     &             + (.000028 * (SAFY**2 - CAFY**2)))
            ALON  = MOD(720.+360.-XLON,360.)

C  SHA is solar hour angle
C  -----------------------

            SHA = .0174532925 * ((15. * (TSNOON + BALTM + 36.)) - ALON)
            RLAT  = .0174532925 * YLAT
            SOLAR(M) = 57.29578 * ASIN((SSOD * SIN(RLAT)) +
     &               (SQRT(1.0 - SSOD**2) * COS(RLAT) * COS(SHA)))
cppppp
            write(68,722) pres(m),addtim,ylat,xlon,baltm,solar(m),
     &                    sid(is)
  722       FORMAT(' pres=',F6.1,' addtim=',F8.3,' ylat=',F6.2,' xlon=',
     &             F6.2,' baltm=',F8.3,' solar=',F6.2,' id=',A8)
cppppp
         ENDIF
      ENDDO

      IF(BAL_DRIFT)  RETURN

C  "OLD" METHOD: INTERPOLATE SOLAR ELEVATION ANGLES BETWEEN AT
C  MANDATORY LEVELS (BASED ON SPANNING LEVELS CALC. ABOVE)
C  -------------------------------------------------------------------

      IF(IRCTBL.EQ.1)  THEN
         SUNBA = SOLAR(16)-SOLAR(11) ! For IRCTBL=1 between 100 & 10 mb
         SOLAR(12) = SOLAR(11) + (0.15252 * SUNBA)
         SOLAR(13) = SOLAR(11) + (0.29520 * SUNBA)
         SOLAR(14) = SOLAR(11) + (0.51538 * SUNBA)
         SOLAR(15) = SOLAR(11) + (0.69250 * SUNBA)
      ELSE
         SUNBA = SOLAR(16)-SOLAR(4) ! For IRCTBL>1 between 700 and 10 mb
                                    !  NOTE: SOLAR(1)-SOLAR(3) are
                                    !  crude est. at 1000, 925 & 850 mb
                                    !  (based on ratio of higher lvl
                                    !  values to diff in reference hght)
                                    !  (these are needed for IRCTBL=3)
         SOLAR(1 ) = SOLAR(4) - (.0826254 * SUNBA)
         SOLAR(2 ) = SOLAR(4) - (.0647634 * SUNBA)
         SOLAR(3 ) = SOLAR(4) - (.0456353 * SUNBA)
         SOLAR(5 ) = SOLAR(4) + (.0791979 * SUNBA)
         SOLAR(6 ) = SOLAR(4) + (.1317209 * SUNBA)
         SOLAR(7 ) = SOLAR(4) + (.1994348 * SUNBA)
         SOLAR(8 ) = SOLAR(4) + (.2423491 * SUNBA)
         SOLAR(9 ) = SOLAR(4) + (.2948721 * SUNBA)
         SOLAR(10) = SOLAR(4) + (.3625860 * SUNBA)
         SOLAR(11) = SOLAR(4) + (.4580233 * SUNBA)
         SOLAR(12) = SOLAR(4) + (.5419766 * SUNBA)
         SOLAR(13) = SOLAR(4) + (.6211745 * SUNBA)
         SOLAR(14) = SOLAR(4) + (.7414114 * SUNBA)
         SOLAR(15) = SOLAR(4) + (.8308487 * SUNBA)
      END IF

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    TAB         COUNTS RADIOSONDE TYPES FOR "ADPUPA" MASS
C   PRGMMR: D. A. KEYSER     ORG: W/NMC22    DATE: 90-12-06
C
C ABSTRACT: ACCUMULATES DATA COUNTS FOR ALL "ADPUPA" MASS RADIOSONDE
C   TYPES.  ALSO ACCUMULATES DATA COUNTS FOR ALL RADIOSONDE TYPES
C   LUMPED TOGETHER.  INPUT RADIOSONDE TYPES ARE ASSIGNED TO A VARIABLE
C   READ IN VIA COMMON BLOCK /COUNT/.  THE DATA COUNTER IS ALSO READ IN
C   VIA THIS COMMON BLOCK.
C
C PROGRAM HISTORY LOG:
C   UNKNOWN   G. D. DIMEGO
C   90-12-05  D. A. KEYSER -- CONVERTED TO VS FORTRAN(77)& RESTRUCTURED
C
C USAGE:    CALL TAB(ID)
C   INPUT ARGUMENT LIST:
C     ID       - INTEGER INTRUMENT TYPE CODE
C
C REMARKS: CALLED BY SUBROUTINE "RADEVN".
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE TAB(ID)
      PARAMETER (NRID=800) ! max # of raob ids listed per inst. type
      CHARACTER*8  SIDNOR, SIDRAD
      COMMON/COUNT/   NTYPE(69),KTYPE(69),SIDNOR(2000),SIDRAD(NRID,255),
     &                IICNT,JJCNT(255)
      DO I=1,68
         IF(NTYPE(I).EQ.99999)   GO TO 40
         IF(NTYPE(I).EQ.ID)  GO TO 50
      ENDDO
      NTYPE(69) = ID
      GO TO 60
   40 CONTINUE
      NTYPE(I) = ID
   50 CONTINUE
      KTYPE(I) = KTYPE(I) + 1
   60 CONTINUE
      KTYPE(69) = KTYPE(69) + 1
      RETURN
      END

