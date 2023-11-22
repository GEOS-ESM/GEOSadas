C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C  
C MAIN PROGRAM:  READ_PREPBUFR
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2002-01-28
C
C ABSTRACT: READS SUBSETS (REPORTS) FROM A PREPBUFR FILE, MERGING
C   MASS AND WIND SUBSETS FROM THE SAME ORIGINAL REPORT.  MERGED
C   REPORTS ARE PARSED ACCORDING TO THEIR BUFR MESSAGE TYPE AND
C   ARE LISTED IN OUTPUT TEXT FILES.
C
C PROGRAM HISTORY LOG:
C ????-??-??  WOOLLEN/GSC ORIGINAL AUTHOR
C ????-??-??  ATOR        STREAMLINED AND DOCUMENTED
C 2001-10-26  KEYSER      FURTHER DOCUMENTATION FOR WEB, UPDATED
C                         TO REPLACE OBSOLETE MESSAGE TYPE "SATBOG"
C                         WITH "QKSWND"; ADDED MORE VARAIBLES AND
C                         MANY OTHER UPDATES
C 2002-01-28  KEYSER      NOW THAT "PCAT" (PRECISION OF TEMPERATURE
C                         OBS.) HAS BEEN ADDED TO PREPBUFR FILE FOR
C                         "AIRCFT" AND "AIRCAR" MESSAGE TYPES, ADDED
C                         THIS TO LISTING
C 2015-12-02  SIENKIEWICZ PATCHED TO RESOLVE PROBLEM WITH READING 
C                         AIRCRAFT HEADER SINCE SOME MNEMONICS NOW 
C                         STORED ON DIFFERENT BUFR NODES
C
C USAGE:
C   INPUT FILES:
C     UNIT 11  - PREPBUFR FILE
C
C   OUTPUT FILES: 
C     UNIT 06  - UNIT 6 (STANDARD PRINTFILE)
C     UNIT 51  - LISTING OF REPORTS IN MESSAGE TYPE "ADPUPA"
C     UNIT 52  - LISTING OF REPORTS IN MESSAGE TYPE "AIRCAR"
C     UNIT 53  - LISTING OF REPORTS IN MESSAGE TYPE "AIRCFT"
C     UNIT 54  - LISTING OF REPORTS IN MESSAGE TYPE "SATWND"
C     UNIT 55  - LISTING OF REPORTS IN MESSAGE TYPE "PROFLR"
C     UNIT 56  - LISTING OF REPORTS IN MESSAGE TYPE "VADWND"
C     UNIT 57  - LISTING OF REPORTS IN MESSAGE TYPE "SATEMP"
C     UNIT 58  - LISTING OF REPORTS IN MESSAGE TYPE "ADPSFC"
C     UNIT 59  - LISTING OF REPORTS IN MESSAGE TYPE "SFCSHP"
C     UNIT 60  - LISTING OF REPORTS IN MESSAGE TYPE "SFCBOG"
C     UNIT 61  - LISTING OF REPORTS IN MESSAGE TYPE "SPSSMI"
C     UNIT 62  - LISTING OF REPORTS IN MESSAGE TYPE "SYNDAT"
C     UNIT 63  - LISTING OF REPORTS IN MESSAGE TYPE "ERS1DA"
C     UNIT 64  - LISTING OF REPORTS IN MESSAGE TYPE "GOESND"
C     UNIT 65  - LISTING OF REPORTS IN MESSAGE TYPE "QKSWND"
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - READPB   INDEXF
C     LIBRARY:
C       W3LIB    - ERREXIT
C       BUFR     - OPENBF   DATELEN  READNS   UFBINT   UFBEVN
C                  UFBQCP
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C             22 - ABORT, NUMBER OF BALLOON DRIFT LEVELS RETURNED
C                  DOES NOT EQUAL NUMBER OF P/T/Z/Q/U/V LEVELS
C                  RETURNED (ADPUPA MESSAGE TYPE ONLY)
C             33 - ABORT, NUMBER OF VIRTUAL TEMP./DEWPOINT TEMP.
C                  LEVELS RETURNED DOES NOT EQUAL NUMBER OF
C                  P/T/Z/Q/U/V LEVELS RETURNED (ADPUPA MESSAGE
C                  TYPE ONLY)
C
C REMARKS: IN THIS PROGRAM, THE TERM "SUBSET" REFERS TO THE
C   COMPONENTS IN A BUFR MESSAGE.  THESE COULD BE CONSIDERED TO
C   BE "REPORTS" IN THE PREPBUFR FILE.  HERE, "REPORT" IS USED TO
C   REFER TO THE FINAL PRODUCT WHICH CONTAINS MERGED MASS AND WIND
C   SUBSETS FROM THE SAME ORIGINAL OBSERVATION.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP, SGI
C
C$$$

      PROGRAM   READ_PREPBUFR

      REAL*8    R8BFMS

      PARAMETER ( R8BFMS = 9.0E08 )! Missing value threshhold for BUFR
      PARAMETER ( NHR8PM = 14 )    ! Actual number of BUFR parameters
                                   !  in header
      PARAMETER ( MXR8PM =  8 )    ! Maximum number of BUFR parameters
                                   !  in level data (non-radiance) or
                                   !  in channel data (radiance)
      PARAMETER ( MXR8LV = 255 )   ! Maximum number of BUFR levels/
                                   !                        channels
      PARAMETER ( MXR8VN = 10 )    ! Max. number of BUFR event sequences
                                   !  (non-radiance reports)
      PARAMETER ( MXR8VT = 17)     ! Max. number of BUFR variable types
                                   !  (non-radiance reports)
      PARAMETER ( NFILO = 15 )

      REAL*8    hdr ( NHR8PM ), qkswnd_hdr(3), aircar_hdr(6),
     +          aircft_hdr(4),  adpupa_hdr(1), goesnd1_hdr(6),
     +          goesnd2_hdr(MXR8LV), adpsfc_hdr(5), sfcshp_hdr(2),
     +          satwnd_hdr(1), evns ( MXR8PM, MXR8LV, MXR8VN, MXR8VT ),
     +          drft ( 3, MXR8LV ), brts ( MXR8PM, MXR8LV ),
     +          toth ( 2, MXR8LV )

      INTEGER   ifirst ( NFILO ), iunso ( NFILO ), indxvr1 ( 100:299 ),
     +          indxvr2 ( 100:299 )

      CHARACTER    msgtyp*8, qc_step*8
      CHARACTER*18 var ( MXR8VT )
      CHARACTER*6  filo ( NFILO )

      LOGICAL   found

      COMMON  / PREPBC /      hdr, evns, drft, toth, brts, qkswnd_hdr,
     +  aircar_hdr, aircft_hdr, adpupa_hdr, goesnd1_hdr, goesnd2_hdr,
     +  adpsfc_hdr, sfcshp_hdr, satwnd_hdr, indxvr1, indxvr2, nlev, nchn

      DATA      iunso
     +        /   51,   52,   53,   54,   55,
     +            56,   57,   58,   59,   60,
     +            61,   62,   63,   64,   65  /
      DATA         var /
     +  'PRESSURE (MB)     ','SP HUMIDITY(MG/KG)','TEMPERATURE (C)   ',
     +  'HEIGHT (METERS)   ','U-COMP WIND (M/S) ','V-COMP WIND (M/S) ',
     +  'WIND DIR (DEG)    ','WIND SPEED (KNOTS)','TOTAL PWATER (MM) ',
     +  'RAIN RATE (MM/HR) ','1.-.9 sig PWAT(MM)','.9-.7 sig PWAT(MM)',
     +  '.7-.3 sig PWAT(MM)','.3-0. sig PWAT(MM)','CLOUD-TOP PRES(MB)',
     +  'CLOUD-TOP TEMP (K)','TOT. CLD. COVER(%)' /
      DATA         filo
     +        / 'ADPUPA', 'AIRCAR', 'AIRCFT', 'SATWND', 'PROFLR',
     +          'VADWND', 'SATEMP', 'ADPSFC', 'SFCSHP', 'SFCBOG',
     +          'SPSSMI', 'SYNDAT', 'ERS1DA', 'GOESND', 'QKSWND'/

C-----------------------------------------------------------------------

      IFIRST = 0

C       Open the output files.
C       ----------------------

      DO ifile = 1, NFILO
          OPEN  ( UNIT = iunso ( ifile ),
     +            FILE = 'readpb.out.' // filo ( ifile ) )
      END DO

C       Open the input PREPBUFR file.
C       -----------------------------
      OPEN  ( UNIT = 11, FILE = 'prepbufr.in', FORM = 'UNFORMATTED' )
      CALL OPENBF(11,'IN',11 )  ! BUFRLIB routine to open file
      CALL DATELEN(10)  ! BUFRLIB routine to use 10-digit date

C       Get the next subset from the input file.  Merge mass and
C        wind subsets for the same "true" report.
C       --------------------------------------------------------

  10  CALL READPB(11,msgtyp,idate,ierrpb )
      IF ( ierrpb .eq. -1 )  THEN
          print *, '1-All subsets read in and processed'
          STOP      ! All subsets read in and processed
      END IF

C       Set the appropriate output file unit number based on the
C        BUFR message type the subset is in.
C       --------------------------------------------------------

      ifile = 1
      found = .false.
      DO WHILE  ( ( .not. found ) .and. ( ifile .le. NFILO ) )
          IF  ( msgtyp (1:6) .eq. filo ( ifile ) )  THEN
              found = .true.
              iuno = iunso ( ifile )
          ELSE
              ifile = ifile + 1
          END IF
      END DO
      IF  ( ( .not. found ) .and. ( ierrpb .eq. 0 ) )  THEN
          GO TO 10
      END IF

      IF ( ifirst(ifile) .eq. 0 )  THEN
         WRITE  ( UNIT = iuno,
     +    FMT = '("LISTING FOR MESSAGE TYPE ",A6//' //
     +    '"Key for header:"/' //
     +    '"SID  -- Station identification"/' //
     +    '"YOB  -- Latitude (N+, S-)"/' //
     +    '"XOB  -- Longitude (E)"/' //
     +    '"ELV  -- Elevation (meters)"/' //
     +    '"DHR  -- Observation time minus cycle time (hours)"/' //
     +    '"RPT  -- Reported observation time (hours)"/' //
     +    '"TCOR -- Indicator whether observation time used to ",' //
     +     '"generate DHR was corrected (0- Observation time ",' //
     +     '"is reported time, no"/9X,"correction, 1- Observation ",' //
     +     '"time corrected based on reported launch time, 2- ",' //
     +     '"Observation time corrected even though"/9X,"launch ", ' //
     +     '"time is missing)"/' //
     +    '"TYP  -- PREPBUFR report type"/' //
     +    '"TSB  -- PREPBUFR report subtype"/' //
     +    '"T29  -- Input report type"/' //
     +    '"ITP  -- Instrument type (BUFR code table 0-02-001)"/' //
     +    '"SQN  -- Report sequence number"/' //
     +    '"PROCN - Process number for this MPI run (obtained from ",'//
     +     '"script)"/' //
     +    '"SAID -- Satellite identifier (BUFR code table 0-01-007)")')
     +    filo(ifile)
         IF (filo(ifile) .eq. 'ADPUPA' ) then
            WRITE  ( UNIT = iuno, FMT =
     +       '("SIRC -- Rawinsonde solar & infrared radiation ",' //
     +        '"correction indicator (BUFR code table 0-02-013)")')
         ELSE IF (filo(ifile) .eq. 'GOESND' .or.
     +            filo(ifile) .eq. 'SATEMP' ) then
            WRITE  ( UNIT = iuno, FMT =
     +       '("For reports with non-radiance data:"/' //
     +       '"  ACAV -- Total number with respect to accumulation ",'//
     +        '"or average"/' //
     +       '"  PRSS -- Surface pressure observation (mb)"/' //
     +       '"For reports with radiance data:"/' //
     +       '"  ELEV -- Satellite elevation (zenith angle - deg.)"/' //
     +       '"  SOEL -- Solar elevation (zenith angle - deg.)"/' //
     +       '"  OZON -- Total ozone (Dobson units)"/' //
     +       '"  TMSK -- Skin temperature (Kelvin)"/' //
     +       '"  CLAM -- Cloud Amount (BUFR code table 0-20-011)")')
         ELSE IF (filo(ifile) .eq. 'QKSWND' ) then
            WRITE  ( UNIT = iuno, FMT =
     +       '("ATRN -- Along track row number"/' //
     +       '"CTCN -- Cross track cell number"/' //
     +       '"SPRR -- Seawinds probability of rain (%)")')
         ELSE IF (filo(ifile) .eq. 'AIRCAR' ) then
            WRITE  ( UNIT = iuno, FMT =
     +       '("PCAT -- Precision of temperature obs. (Kelvin)"/' //
     +       '"POAF -- Phase of aircraft flight (BUFR code table ",' //
     +        '"0-08-004)"/' //
     +       '"TRBX10 -- Turbulence index for period TOB-1 min to ",' //
     +        '"TOB"/' //
     +       '"TRBX21 -- Turbulence index for period TOB-2 min to ",' //
     +        '"TOB-1 min"/' //
     +       '"TRBX32 -- Turbulence index for period TOB-3 min to ",' //
     +        '"TOB-2 min"/' //
     +       '"TRBX43 -- Turbulence index for period TOB-4 min to ",' //
     +        '"TOB-3 min")')
         ELSE IF (filo(ifile) .eq. 'AIRCFT' ) then
            WRITE  ( UNIT = iuno, FMT =
     +       '("RCT  -- Receipt time (hours)"/' //
     +       '"PCAT -- Precision of temperature obs. (Kelvin)"/' //
     +       '"POAF -- Phase of aircraft flight (BUFR code table ",' //
     +        '"0-08-004)"/' //
     +       '"DGOT -- Degree of turbulence (BUFR code table ",' //
     +        '"0-11-031)")')
         ELSE IF (filo(ifile) .eq. 'ADPSFC' ) then
            WRITE  ( UNIT = iuno, FMT =
     +       '("PMO  -- Mean sea-level pressure observation (mb)"/' //
     +       '"PMQ  -- Mean sea-level pressure quality marker"/' //
     +       '"ALSE -- Altimeter setting observation (mb)"/' //
     +       '"SOB  -- Wind speed observation (stored when ",' //
     +        '"direction is missing) (m/s)"/' //
     +       '"SQM  -- Wind speed quality marker (stored when ",' //
     +        '"direction is missing)")')
         ELSE IF (filo(ifile) .eq. 'SFCSHP' ) then
            WRITE  ( UNIT = iuno, FMT =
     +       '("PMO  -- Mean sea-level pressure observation (mb)"/' //
     +       '"PMQ  -- Mean sea-level pressure quality marker")')
         ELSE IF (filo(ifile) .eq. 'SATWND' ) then
            WRITE  ( UNIT = iuno, FMT =
     +       '("RFFL -- NESDIS recursive filter flag")')
         END IF
         WRITE  ( UNIT = iuno, FMT = '(//"PREPBUFR FILE DATE IS: ",
     +    I10)') idate
         WRITE  ( UNIT = iuno, FMT = '(/)')
         ifirst(ifile)=1
      END IF

      WRITE  ( UNIT = iuno,
     + FMT = '(/"SID=",A8,", YOB=",F6.2,", XOB=",F6.2,", ELV=",I5,' //
     + '", DHR=",F6.3,", RPT=",F8.3,", TCOR=",I1,", TYP=",I3,' //
     + '", TSB=",I1,", T29=",I3,", ITP=",I2,", SQN=",I6/,"PROCN=",I2,'//
     + '", SAID=",I3)')
     + (hdr(ii),ii=1,3),nint(hdr(4)),hdr(5),hdr(6),(nint(hdr(ii)),
     + ii=7,14)

      IF (filo(ifile) .eq. 'ADPUPA' ) then
         WRITE  ( UNIT = iuno, FMT = '("SIRC=",I2)') nint(adpupa_hdr)
      ELSE IF (filo(ifile) .eq. 'GOESND' .or.
     +         filo(ifile) .eq. 'SATEMP' ) then
         IF (nlev.gt.0)  THEN
            WRITE  ( UNIT = iuno, FMT = '("ACAV=",I3,", PRSS=",F6.1)')
     +       nint(goesnd1_hdr(1)),goesnd2_hdr(1)*0.01
         END IF
         IF (nchn.gt.0)  THEN
            WRITE  ( UNIT = iuno, FMT = '("ELEV=",F6.2,", SOEL=",' //
     +       'F6.2,", OZON=",I4,", TMSK=",F5.1,", CLAM=",I2)')
     +       goesnd1_hdr(2),goesnd1_hdr(3),nint(goesnd1_hdr(4)),
     +       goesnd1_hdr(5),nint(goesnd1_hdr(6))
         END IF
      ELSE IF (filo(ifile) .eq. 'QKSWND' ) then
         WRITE  ( UNIT = iuno, FMT = '("ATRN=",I3,", CTCN=",I3,' //
     +   '", SPRR=",I3)') (nint(qkswnd_hdr(ii)),ii=1,3)
      ELSE IF (filo(ifile) .eq. 'AIRCAR' ) then
         WRITE  ( UNIT = iuno, FMT = '("PCAT=",F5.2,", POAF=",I3,' //
     + '", TRBX10=",I3,", TRBX21=",I3,", TRBX32=",I3,", TRBX43=",I3)')
     +    aircar_hdr(1),(nint(aircar_hdr(ii)),ii=2,6)
      ELSE IF (filo(ifile) .eq. 'AIRCFT' ) then
         WRITE  ( UNIT = iuno, FMT = '("RCT=",F7.2,", PCAT=",F5.2,' //
     +    '", POAF=",I3,", DGOT=",I3)') aircft_hdr(1),aircft_hdr(2),
     +    (nint(aircft_hdr(ii)),ii=3,4)
      ELSE IF (filo(ifile) .eq. 'ADPSFC' ) then
         WRITE  ( UNIT = iuno, FMT = '("PMO=",F6.1,", PMQ=",I2,' //
     +    '", ALSE=",F6.1,", SOB=",F5.1,", SQM=",I2)')
     +   adpsfc_hdr(1),nint(adpsfc_hdr(2)),adpsfc_hdr(3)*0.01,
     +   adpsfc_hdr(4),nint(adpsfc_hdr(5))
      ELSE IF (filo(ifile) .eq. 'SFCSHP' ) then
         WRITE  ( UNIT = iuno, FMT = '("PMO=",F6.1,", PMQ=",I2)')
     +     sfcshp_hdr(1),nint(sfcshp_hdr(2))
      ELSE IF (filo(ifile) .eq. 'SATWND' ) then
         WRITE  ( UNIT = iuno, FMT = '("RFFL=",I3)') nint(satwnd_hdr(1))
      END IF

      IF (nlev .gt. 0 )  THEN

C       Print the level (non-radiance) data for report (if separate
C        mass & wind pieces in PREPBUFR file, these have been merged).
C       --------------------------------------------------------------

         DO lv = 1, nlev  ! loop through the levels
            WRITE  ( UNIT = iuno, FMT = '( 94("-") )' )
            WRITE  ( UNIT = iuno,
     +               FMT = '( 10X,"level ", I4, 2X, A9,A8,A10,5A9 )' )
     +       lv, 'obs      ', 'qmark   ', 'qc_step   ', 'rcode    ',
     +       'fcst     ', 'anal     ', 'oberr    ', 'category '
            WRITE  ( UNIT = iuno, FMT = '( 94("-") )' )
            DO kk = indxvr1(nint(hdr(8))),indxvr2(nint(hdr(8)))
                             ! loop through the variables
               IF( kk .eq. 10 )  ! convert rain rate from mm/s to mm/hr
     +                 evns( 1, lv, :,kk) = evns( 1, lv, :,kk)*3600.0_8
               IF( kk .eq. 15 ) ! convert cloud-top press. from Pa to mb
     +                 evns( 1, lv, :,kk) = evns( 1, lv, :,kk)*0.01
               DO jj = 1, MXR8VN  ! loop through the events
                  IF(min(evns(1,lv,jj,kk),evns(2,lv,jj,kk),
     +                   evns(3,lv,jj,kk),evns(4,lv,jj,kk),
     +                   evns(5,lv,jj,kk),evns(6,lv,jj,kk),
     +                   evns(7,lv,jj,kk))
     +                   .lt. R8BFMS) THEN
                     IF(evns(3,lv,jj,kk) .le. R8BFMS) THEN

C       Copy forecast value, analyzed value, observation error and
C        PREPBUFR category from latest event sequence to all earlier
C        valid event sequences
C       --------------------------------------------------------------

                        evns(5:8,lv,jj,kk) = evns(5:8,lv,1,kk)
                        pcode = evns(3,lv,jj,kk)
                        CALL UFBQCP(11,pcode,QC_STEP)
                     ELSE
                        QC_STEP = '        '
                     END IF
                     WRITE  ( UNIT = iuno,
     + FMT = '( A18,1X,F8.1,2X,F6.0,4X,A8,2X,F5.0,3(1X,F8.1),2X,F7.0)' )
     +               var (kk), ( evns ( ii, lv, jj, kk ), ii = 1, 2 ),
     +               qc_step,  ( evns ( ii, lv, jj, kk ), ii = 4, 8 )
                  END IF
               END DO
            END DO
            IF ( msgtyp .eq. 'ADPUPA' )  THEN
               WRITE  ( UNIT = iuno,
     +    FMT = '( 10X,"BALLOON DRIFT: D-TIME(HRS)=",F7.3,", LAT(N)=",
     +             F6.2,", LON(E)=",F6.2)' ) ( drft ( ii, lv), ii = 1,3)
               IF(min(toth(1,lv),toth(2,lv)) .lt. R8BFMS)
     +          WRITE  ( UNIT = iuno,
     + FMT = '( 10X,"Through PREPRO STEP only: VIRTUAL TEMP (C)=",F8.1,
     +       ", DEWPOINT TEMP (C)=",F8.1)' ) toth (1,lv), toth (2,lv)
            END IF
         END DO
      END IF

      IF (nchn .gt. 0 )  THEN

C       Print the channel (radiance) data for report
C       --------------------------------------------

         WRITE  ( UNIT = iuno, FMT = '( 42("-") )' )
         WRITE  ( UNIT = iuno,
     +            FMT = '("BRIGHTNESS TEMP  channel  observation (K)")')
         WRITE  ( UNIT = iuno, FMT = '( 42("-") )' )
         DO lv = 1, nchn  ! loop through the channels
            IF(min(brts(1,lv),brts(2,lv)) .lt. R8BFMS) THEN
               WRITE  ( UNIT = iuno, FMT = '(19X,I3,7X,F6.2)' )
     +          nint(brts(1,lv)), brts(2,lv)
            END IF
         END DO
      END IF

      IF  ( ierrpb .eq. 0 )  GO TO 10

      print *, '2-All subsets read in and processed'

      STOP      ! All subsets read in and processed

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    READPB
C   PRGMMR: KEYSER           ORG: NP12        DATE: 2002-01-28
C
C ABSTRACT: THIS SUBROUTINE READS IN SUBSETS FROM THE PREPBUFR
C   FILE.  SINCE ACTUAL OBSERVATIONS ARE SPLIT INTO MASS AND
C   WIND SUBSETS, THIS ROUTINE COMBINES THEM INTO A SINGLE
C   REPORT FOR OUTPUT.  IT IS STYLED AFTER BUFRLIB ENTRY POINT
C   READNS, AND IT ONLY REQUIRES THE PREPBUFR FILE TO BE OPENED
C   FOR READING WITH OPENBF.  THE COMBINED REPORT IS RETURNED TO
C   THE CALLER IN COMMON /PREPBC/.  THIS COMMON AREA CONTAINS
C   THE NUMBER OF LEVELS (NON-RADIANCES) IN THE REPORT, THE
C   NUMBER OF CHANNELS (RADIANCES) IN THE REPORT, A ONE
C   DIMENSIONAL ARRAY WITH THE HEADER INFORMATION, A FOUR
C   DIMENSIONAL ARRAY CONTAINING ALL EVENTS FROM ALL
C   NON-RADIANCE OBSERVATIONAL VARIABLE TYPES, AND A TWO
C   DIMENSIONAL ARRAY CONTAINING ALL CHANNELS OF BRIGHTNESS
C   TEMPERATURE FOR REPORTS CONTAINING RADIANCE DATA.
C
C PROGRAM HISTORY LOG:
C ????-??-??  WOOLLEN/GSC ORIGINAL AUTHOR
C ????-??-??  ATOR        STREAMLINED AND DOCUMENTED
C 2001-10-26  KEYSER      FURTHER DOCUMENTATION FOR WEB, UPDATED
C                         TO REPLACE OBSOLETE MESSAGE TYPE "SATBOG"
C                         WITH "QKSWND"; ADDED MORE VARAIBLES AND
C                         MANY OTHER UPDATES
C 2002-01-28  KEYSER      NOW THAT "PCAT" (PRECISION OF TEMPERATURE
C                         OBS.) HAS BEEN ADDED TO PREPBUFR FILE FOR
C                         "AIRCFT" AND "AIRCAR" MESSAGE TYPES, ADDED
C                         THIS TO LISTING
C
C USAGE:    CALL READPB  (LUNIT, MSGTYP, IDATE, IRET)
C   INPUT ARGUMENT LIST:
C     LUNIT    - UNIT NUMBER OF INPUT PREPBUFR FILE
C
C   OUTPUT ARGUMENT LIST:
C     MSGTYP   - BUFR MESSAGE TYPE (CHARACTER)
C     IDATE    - BUFR MESSAGE DATE IN FORM YYYYMMDDHH
C     IRET     - ERROR RETURN CODE (0 - normal return; 1 - the report
C                within COMMON /PREPBC/ contains the last available
C                report from within the prepbufr file; -1 - there are
C                no more reports available from within the PREPBUFR
C                file, all subsets have been processed)
C
C REMARKS: 
C 
C   The header array HDR contains the following list of mnemonics:
C         HDR(1)  Station identification (SID)
C         HDR(2)  Latitude  (YOB)
C         HDR(3)  Longitude (XOB)
C         HDR(4)  Elevation (ELV)
C         HDR(5)  Observation time minus cycle time (DHR)
C         HDR(6)  Reported observation time (RPT)
C         HDR(7)  Indicator whether observation time used to generate
C                  "DHR" was corrected (TCOR)
C         HDR(8)  PREPBUFR report type (TYP)
C         HDR(9) PREPBUFR report subtype (TSB)
C         HDR(10) Input report type (T29)
C         HDR(11) Instrument type (ITP)
C         HDR(12) Report sequence number (SQN)
C         HDR(13) Process number for this MPI run (obtained from
C                  script (PROCN)
C         HDR(14) Satellite identifier (SAID)
C 
C   The 4-D array of non-radiance data, EVNS (ii, lv, jj, kk), is
C    indexed as follows:
C     "ii" indexes the event data types; these consist of:
C         1) Observation       (e.g., POB, ZOB, UOB, VOB, TOB, QOB, PWO)
C         2) Quality mark      (e.g., PQM, ZRM, WQM, TQM, QQM, PWQ)
C         3) Program code      (e.g., PPC, ZPC, WPC, TPC, QPC, PWP)
C         4) Reason code       (e.g., PRC, ZRC, WRC, TRC, QRC, PWR)
C         5) Forecast value    (e.g., PFC, ZFC, UFC, VFC, TFC, QFC, PWF)
C         6) Analyzed value    (e.g., PAN, ZAN, UAN, VAN, TAN, QAN, PWA)
C         7) Observation error (e.g., POE, ZOE, WOE, TOE, QOE, PWO)
C         8) PREPBUFR data level category (CAT)
C     "lv" indexes the levels of the report
C         1) Lowest level
C     "jj" indexes the event stacks
C         1) N'th event
C         2) (N-1)'th event (if present)
C         3) (N-2)'th event (if present)
C                ...
C        10) (N-9)'th event (if present)
C     "kk" indexes the variable types
C         1) Pressure
C         2) Specific humidity
C         3) Temperature
C         4) Height
C         5) U-component wind
C         6) V-component wind
C         7) Wind direction
C         8) Wind speed
C         9) Total precipitable water
C        10) Rain rate
C        11) 1.0 to 0.9 sigma layer precipitable water
C        12) 0.9 to 0.7 sigma layer precipitable water
C        13) 0.7 to 0.3 sigma layer precipitable water
C        14) 0.3 to 0.0 sigma layer precipitable water
C        15) Cloud-top pressure
C        16) Cloud-top temperature
C        17) Total cloud cover
C 
C     Note that the structure of this array is identical to one
C      returned from BUFRLIB routine UFBEVN, with an additional (4'th)
C      dimension to include the 14 variable types into the same
C      array.
C 
C   The 2-D array of radiance data, BRTS (ii, lv), is indexed
C    as follows:
C     "ii" indexes the event data types; these consist of:
C         1) Observation       (CHNM, TMBR)
C     "lv" indexes the channels (CHNM gives channel number)
C 
C     Note that this array is directly returned from BUFRLIB
C      routine UFBINT.
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
      SUBROUTINE READPB  ( lunit, msgtyp, idate, iret )

      REAL*8    R8BFMS

      PARAMETER ( R8BFMS = 9.0E08 )! Missing value threshhold for BUFR
      PARAMETER ( NHR8PM = 14 )    ! Actual number of BUFR parameters
                                   !  in header
      PARAMETER ( MXR8PM =  8 )    ! Maximum number of BUFR parameters
                                   !  in level data (non-radiance) or
                                   !  in channel data (radiance)
      PARAMETER ( MXR8LV = 255 )   ! Maximum number of BUFR levels/
                                   !                        channels
      PARAMETER ( MXR8VN = 10 )    ! Max. number of BUFR event sequences
                                   !  (non-radiance reports)
      PARAMETER ( MXR8VT = 17)     ! Max. number of BUFR variable types
                                   !  (non-radiance reports)
      PARAMETER ( MXSTRL = 80 )    ! Maximum size of a string

      REAL*8    hdr ( NHR8PM ), qkswnd_hdr(3), aircar_hdr(6),
     +          aircft_hdr(4),  adpupa_hdr(1), goesnd1_hdr(6),
     +          goesnd2_hdr(MXR8LV), adpsfc_hdr(5), sfcshp_hdr(2),
     +          satwnd_hdr(1), evns ( MXR8PM, MXR8LV, MXR8VN, MXR8VT ),
     +          drft ( 3, MXR8LV ), brts ( MXR8PM, MXR8LV ),
     +          toth ( 2, MXR8LV )
      REAL*8    hdr2 ( NHR8PM ), qkswnd_hdr2(3), aircar_hdr2(6),
     +          aircft_hdr2(4),  adpupa_hdr2(1), goesnd1_hdr2(6),
     +          goesnd2_hdr2(MXR8LV), adpsfc_hdr2(5), sfcshp_hdr2(2),
     +          satwnd_hdr2(1), evns2 ( MXR8PM, MXR8LV, MXR8VN, MXR8VT),
     +          drft2 ( 3, MXR8LV ), toth2 ( 2, MXR8LV )
      REAL*8    hdr_save  ( NHR8PM ), adpsfc_hdr_save(5),
     +          sfcshp_hdr_save(2),
     +          evns_save ( MXR8PM, MXR8LV, MXR8VN, MXR8VT ),
     +          drft_save ( 3, MXR8LV ), toth_save ( 2, MXR8LV )
      REAL*8    r8sid, r8sid2, pob1, pob2

      REAL      p ( MXR8LV )

      INTEGER  indr ( MXR8LV ), indxvr1 ( 100:299 ), indxvr2 ( 100:299 )

      CHARACTER    msgtyp*8, msgtyp_process*8
      CHARACTER*8  csid, csid2, msgtp2
      CHARACTER*(MXSTRL)      head1, head2, ostr ( MXR8VT )

      LOGICAL         match, seq_match, single_msgtyp, radiance,
     +                not_radiance

      EQUIVALENCE     ( r8sid, csid ), ( r8sid2, csid2 )

      COMMON  / PREPBC /      hdr, evns, drft, toth, brts, qkswnd_hdr,
     +  aircar_hdr, aircft_hdr, adpupa_hdr, goesnd1_hdr, goesnd2_hdr,
     +  adpsfc_hdr, sfcshp_hdr, satwnd_hdr, indxvr1, indxvr2, nlev, nchn

      DATA   head1
     +        / 'SID YOB XOB ELV DHR RPT TCOR TYP TSB T29 ITP SQN' /
      DATA   head2
     +        / 'PROCN SAID' /
      DATA   ostr
     +        / 'POB PQM PPC PRC PFC PAN POE CAT',
     +          'QOB QQM QPC QRC QFC QAN QOE CAT',
     +          'TOB TQM TPC TRC TFC TAN TOE CAT',
     +          'ZOB ZQM ZPC ZRC ZFC ZAN ZOE CAT',
     +          'UOB WQM WPC WRC UFC UAN WOE CAT',
     +          'VOB WQM WPC WRC VFC VAN WOE CAT',
     +          'DDO DFQ DFP DFR NULL NULL NULL CAT',
     +          'FFO DFQ DFP DFR NULL NULL NULL CAT',
     +          'PWO PWQ PWP PWR PWF PWA PWE CAT',
     + 'REQ6 REQ6_QM REQ6_PC REQ6_RC REQ6_FC REQ6_AN REQ6_OE CAT',
     +          'PW1O PW1Q PW1P PW1R PW1F PW1A PW1E CAT',
     +          'PW2O PW2Q PW2P PW2R PW2F PW2A PW2E CAT',
     +          'PW3O PW3Q PW3P PW3R PW3F PW3A PW3E CAT',
     +          'PW4O PW4Q PW4P PW4R PW4F PW4A PW4E CAT',
     + 'CDTP CDTP_QM CDTP_PC CDTP_RC CDTP_TC CDTP_AN CDTP_OE CAT',
     +          'GCDTT NULL NULL NULL NULL NULL NULL CAT' ,
     +          'TOCC  NULL NULL NULL NULL NULL NULL CAT' /
      DATA            match / .true. /, seq_match / .false. /

      SAVE            match, msgtp2, idate2
C-----------------------------------------------------------------------


      single_msgtyp = .false.   ! set to true if you want to process
                                !  reports from only ONE message type


      msgtyp_process = 'ADPUPA' ! if single_msgtyp=T, then only
                                !  reports in this msgtyp will be
                                !  processed

 1000 continue

      iret = 0

C       If the previous call to this subroutine did not yield matching
C        mass and wind subsets, then BUFRLIB routine READNS is already
C        pointing at an unmatched subset.  Otherwise, call READNS to
C        advance the subset pointer to the next subset.
C       --------------------------------------------------------------
      IF  ( match )  THEN
          CALL READNS(lunit,msgtyp,idate,jret) ! BUFRLIB routine to
                                               !  read in next subset
          IF  ( jret .ne. 0 )  THEN
             iret = -1 ! there are no more subsets in the PREPBUFR file
                       !  and all subsets have been processed
             RETURN
          END IF
          if(single_msgtyp .and. msgtyp.ne.msgtyp_process) go to 1000
      ELSE
          msgtyp = msgtp2
          idate = idate2
      END IF

C       Read the report header based on mnemonic string "head" and
C        transfer 1 for 1 into array HDR for the subset that is
C        currently being pointed to.
C       -----------------------------------------------------------

                       !BUFRLIB routine to read specific info from
                       ! report (no event info though)
      CALL UFBINT(lunit,hdr,           12,1,jret,head1)
      CALL UFBINT(lunit,hdr(13),NHR8PM-12,1,jret,head2)

C       Read header information that is specific to the various
C        message types
C       -------------------------------------------------------

      qkswnd_hdr=10E10
      aircar_hdr=10E10
      aircft_hdr=10E10
      adpupa_hdr=10E10
      goesnd1_hdr=10E10
      goesnd2_hdr=10E10
      adpsfc_hdr=10E10
      sfcshp_hdr=10E10
      satwnd_hdr=10E10

      IF( msgtyp .eq. 'QKSWND')  THEN
         CALL UFBINT(lunit,qkswnd_hdr,3,1,jret,'CTCN ATRN SPRR ')
      ELSE IF( msgtyp .eq. 'AIRCAR')  THEN
         CALL UFBINT(lunit,aircar_hdr,6,1,jret,
     +    'PCAT POAF TRBX10 TRBX21 TRBX32 TRBX43 ')
      ELSE IF( msgtyp .eq. 'AIRCFT')  THEN
         CALL UFBINT(lunit,aircft_hdr,1,1,jret,'RCT ')
         CALL UFBINT(lunit,aircft_hdr(2:3),2,1,jret,'PCAT POAF ')
         CALL UFBINT(lunit,aircft_hdr(4),1,1,jret,'DGOT ')
      ELSE IF( msgtyp .eq. 'ADPUPA')  THEN
         CALL UFBINT(lunit,adpupa_hdr,1,1,jret,'SIRC ')
      ELSE IF( msgtyp .eq. 'GOESND' .or.  msgtyp .eq. 'SATEMP' )  THEN
         CALL UFBINT(lunit,goesnd1_hdr,6,1,jret,
     +               'ACAV ELEV SOEL OZON TMSK CLAM')
         IF( msgtyp .eq. 'GOESND' ) CALL UFBINT(lunit,goesnd2_hdr,1,
     +                                          MXR8LV,jret,'PRSS ')
      ELSE IF( msgtyp .eq. 'ADPSFC')  THEN
         CALL UFBINT(lunit,adpsfc_hdr,5,1,jret,'PMO PMQ ALSE SOB SQM ')
      ELSE IF( msgtyp .eq. 'SFCSHP')  THEN
         CALL UFBINT(lunit,sfcshp_hdr,2,1,jret,'PMO PMQ ')
      ELSE IF( msgtyp .eq. 'SATWND')  THEN
         CALL UFBINT(lunit,satwnd_hdr,1,1,jret,'RFFL ')
      END IF

C       From PREPBUFR report type, determine if this report contains
C        only non-radiance data, only radiance data, or both
C       ------------------------------------------------------------

      radiance = (  nint(hdr(8)) .eq. 102 .or.
     +            ( nint(hdr(8)) .ge. 160 .and. nint(hdr(8)) .le. 179 ))
      not_radiance = ( .not.radiance .or. 
     +          ( nint(hdr(8)) .ge. 160 .and. nint(hdr(8)) .le. 163 )
     +     .or. ( nint(hdr(8)) .ge. 170 .and. nint(hdr(8)) .le. 173 ))

      EVNS = 10E10
      BRTS = 10E10
      nlev = 0
      nchn = 0

      IF ( not_radiance )  THEN

C       For reports with non-radiance data, read the report level data
C        for the variable types based on the variable-specific mnemonic
C        string "ostr(kk)" and transfer all levels, all events 1 for 1
C        into array EVNS for the subset that is currently being pointed
C        to.
C       ---------------------------------------------------------------


         DO kk = indxvr1(nint(hdr(8))),indxvr2(nint(hdr(8)))
                                 ! loop through the variables
             CALL UFBEVN(lunit,evns(1,1,1,kk),MXR8PM,MXR8LV,MXR8VN,nlev,
     +        ostr(kk)) ! BUFRLIB routine to read specific info from
                        ! report, accounting for all possible events
         END DO

         drft=10E10
         toth=10E10
         IF( msgtyp .eq. 'ADPUPA')  THEN

C       Read balloon drift level data for message type ADPUPA
C       -----------------------------------------------------

            CALL UFBINT(lunit,drft,3,MXR8LV,nlevd,'HRDR YDR XDR ')
            if(nlevd .ne. nlev)  CALL ERREXIT(22)

C       Read virtual temperature and dewpoint temperature (through
C        "PREPRO" step only) data for message type ADPUPA
C       ----------------------------------------------------------

            CALL UFBINT(lunit,toth,2,MXR8LV,nlevt,'TVO TDO ')
            if(nlevt .ne. nlev)  CALL ERREXIT(33)

         END IF

      END IF

      IF ( radiance)  THEN

C       For reports with radiance data, read the report channel data
C        for the variable types based on the mnemonic string
C        "CHNM TMBR" and transfer all channels 1 for 1 into array BRTS
C        for the subset that is currently being pointed to.
C       --------------------------------------------------------------

          CALL UFBINT(lunit,brts,MXR8PM,MXR8LV,nchn,'CHNM TMBR')

      END IF

 2000 CONTINUE

C      Now, advance the subset pointer to the next subset and read
C       its report header.
C      -----------------------------------------------------------

      CALL READNS(lunit,msgtp2,idate2,jret)
      IF  ( jret .ne. 0 )  THEN
         iret = 1 ! there are no more subsets in the PREPBUFR file
                  !  but we must still return and process the
                  !  previous report in common /PREPBC/
         RETURN
      END IF
      if(single_msgtyp .and. msgtp2.ne.msgtyp_process) go to 2000
      CALL UFBINT(lunit,hdr2,           12,1,jret,head1)
      CALL UFBINT(lunit,hdr2(13),NHR8PM-12,1,jret,head2)

C       Next, check whether this subset and the previous one are
C        matching mass and wind components for a single "true" report.
C       --------------------------------------------------------------

      match = .true.
      seq_match = .false.

      IF ( max( hdr (12), hdr2 (12) ) .le. R8BFMS )  THEN
         IF ( hdr (13)  .gt. R8BFMS )  hdr (13) = 0.
         IF ( hdr2(13)  .gt. R8BFMS )  hdr2(13) = 0.
         seq  = (hdr (13) * 1000000.) + hdr (12) ! combine seq. nunmber
         seq2 = (hdr2(13) * 1000000.) + hdr2(12) !  and MPI proc. number
         IF  ( seq .ne. seq2 )  THEN
            match = .false.
            RETURN  ! the two process/sequence numbers do not match,
                    !  return and process information in common /PREPBC/
         ELSE
            seq_match = .true.  ! the two process/sequence numbers match
         END IF
      END IF

      IF  ( msgtyp .ne. msgtp2 )  THEN
          match = .false.
          RETURN   ! the two message types do not match, return and
                   !  process information in common /PREPBC/
      END IF

      IF ( .not. seq_match )  THEN

C       The two process/sequence numbers are missing, so as a last
C        resort must check whether this subset and the previous one
C        have identical values for SID, YOB, XOB, ELV, and DHR in
C        which case they match.
C       -----------------------------------------------------------

         r8sid  = hdr (1)
         r8sid2 = hdr2(1)
         IF  ( csid .ne. csid2 )  THEN
             match = .false.
             RETURN   ! the two report id's do not match, return and
                      !  process information in common /PREPBC/
         END IF

         DO ii = 5, 2, -1
            IF  ( hdr (ii) .ne. hdr2 (ii) )  THEN
               match = .false.
               RETURN   ! the two headers do not match, return and
                        !  process information in common /PREPBC/
            END IF
         END DO
      END IF

C       The two headers match, read level data for the second subset.
C        (Note: This can only happen for non-radiance reports)
C       -------------------------------------------------------------

      DO kk = indxvr1(nint(hdr(8))),indxvr2(nint(hdr(8)))
                                 ! loop through the variables
          CALL UFBEVN(lunit,evns2 (1,1,1,kk),MXR8PM,MXR8LV,MXR8VN,
     +     nlev2,ostr(kk))
      ENDDO

      drft2=10E10
      toth2=10E10
      IF( msgtp2 .eq. 'ADPUPA')  THEN

C       Read balloon drift level data for message type ADPUPA
C        for the second subset
C       -----------------------------------------------------

         CALL UFBINT(lunit,drft2,3,MXR8LV,nlev2d,'HRDR YDR XDR ')
         if(nlev2d .ne. nlev2)  CALL ERREXIT(22)

C       Read virtual temperature and dewpoint temperature (through
C        "PREPRO" step only) data for message type ADPUPA for the
C        second subset
C       ----------------------------------------------------------

         CALL UFBINT(lunit,toth2,2,MXR8LV,nlev2t,'TVO TDO ')
         if(nlev2t .ne. nlev2)  CALL ERREXIT(33)

      END IF

C       Read header information that is specific to the various
C        message types for the second supset
C       -------------------------------------------------------

      qkswnd_hdr2=10E10
      aircar_hdr2=10E10
      aircft_hdr2=10E10
      adpupa_hdr2=10E10
      goesnd1_hdr2=10E10
      goesnd2_hdr2=10E10
      adpsfc_hdr2=10E10
      sfcshp_hdr2=10E10
      satwnd_hdr2=10E10
      IF( msgtp2 .eq. 'QKSWND')  THEN
         CALL UFBINT(lunit,qkswnd_hdr2,3,1,jret,'CTCN ATRN SPRR ')
      ELSE IF( msgtp2 .eq. 'AIRCAR')  THEN
         CALL UFBINT(lunit,aircar_hdr2,6,1,jret,
     +    'PCAT POAF TRBX10 TRBX21 TRBX32 TRBX43 ')
      ELSE IF( msgtp2 .eq. 'AIRCFT')  THEN
         CALL UFBINT(lunit,aircft_hdr2,1,1,jret,'RCT ')
         CALL UFBINT(lunit,aircft_hdr2(2:3),2,1,jret,'PCAT POAF ')
         CALL UFBINT(lunit,aircft_hdr2(4),1,1,jret,'DGOT ')
      ELSE IF( msgtp2 .eq. 'ADPUPA')  THEN
         CALL UFBINT(lunit,adpupa_hdr2,1,1,jret,'SIRC ')
      ELSE IF( msgtp2 .eq. 'GOESND' .or.  msgtp2 .eq. 'SATEMP' )  THEN
         CALL UFBINT(lunit,goesnd1_hdr2,6,1,jret,
     +               'ACAV ELEV SOEL OZON TMSK CLAM')
         IF( msgtp2 .eq. 'GOESND' ) CALL UFBINT(lunit,goesnd2_hdr2,1,
     +                                          MXR8LV,jret,'PRSS ')
      ELSE IF( msgtp2 .eq. 'ADPSFC')  THEN
         CALL UFBINT(lunit,adpsfc_hdr2,5,1,jret,'PMO PMQ ALSE SOB SQM ')
      ELSE IF( msgtp2 .eq. 'SFCSHP')  THEN
         CALL UFBINT(lunit,sfcshp_hdr2,2,1,jret,'PMO PMQ ')
      ELSE IF( msgtp2 .eq. 'SATWND')  THEN
         CALL UFBINT(lunit,satwnd_hdr2,1,1,jret,'RFFL ')
      END IF

      IF (nint(hdr(8)) .ge. 280)  THEN

C       If this is a surface report, the wind subset precedes the
C        mass subset - switch the subsets around in order to combine
C        the surface pressure properly
C       ------------------------------------------------------------

         evns_save = evns2
         evns2 = evns
         evns = evns_save
         hdr_save = hdr2
         hdr2 = hdr
         hdr = hdr_save
         adpsfc_hdr_save = adpsfc_hdr2
         adpsfc_hdr2 = adpsfc_hdr
         adpsfc_hdr = adpsfc_hdr_save
         sfcshp_hdr_save = sfcshp_hdr2
         sfcshp_hdr2 = sfcshp_hdr
         sfcshp_hdr = sfcshp_hdr_save
      END IF

C       Combine the message type-specific header data for the two
C        matching subsets into a single array. 
C       ---------------------------------------------------------

      qkswnd_hdr(:) = min(qkswnd_hdr(:),qkswnd_hdr2(:))
      aircar_hdr(:) = min(aircar_hdr(:),aircar_hdr2(:))
      aircft_hdr(:) = min(aircft_hdr(:),aircft_hdr2(:))
      adpupa_hdr(:) = min(adpupa_hdr(:),adpupa_hdr2(:))
      goesnd1_hdr(:) = min(goesnd1_hdr(:),goesnd1_hdr2(:))
      goesnd2_hdr(:) = min(goesnd2_hdr(:),goesnd2_hdr2(:))
      adpsfc_hdr(:) = min(adpsfc_hdr(:),adpsfc_hdr2(:))
      sfcshp_hdr(:) = min(sfcshp_hdr(:),sfcshp_hdr2(:))
      satwnd_hdr(:) = min(satwnd_hdr(:),satwnd_hdr2(:))

C       Combine the data for the two matching subsets into a single 4-D
C        array.  Do this by merging the EVNS2 array into the EVNS array.
C       ----------------------------------------------------------------

      LOOP1: DO lv2 = 1, nlev2  ! loop through the levels of subset 2
         DO lv1 = 1, nlev  ! loop through the levels of subset 1
            pob1 = evns  ( 1, lv1, 1, 1 )
            pob2 = evns2 ( 1, lv2, 1, 1 )
            IF  ( pob1 .eq. pob2 )  THEN

C       This pressure level from the second subset also exists in the
C        first subset, so overwrite any "missing" piece of data for
C        this pressure level in the first subset with the corresponding
C        piece of data from the second subset (since this results in no
C        net loss of data!).
C       ---------------------------------------------------------------

               DO kk = indxvr1(nint(hdr(8))),indxvr2(nint(hdr(8)))
                                 ! loop through the variables
                  DO jj = 1,MXR8VN  ! loop through the events
                     DO ii = 1,MXR8PM-1 ! loop through BUFR parameters
                                        ! skip the CAT parameter
                        IF ( evns (ii,lv1,jj,kk ) .gt. R8BFMS ) THEN
                           evns  ( ii, lv1, jj, kk ) =
     +                     evns2 ( ii, lv2, jj, kk )
                           IF ( evns2 (ii,lv2,jj,kk).le.R8BFMS) THEN

C       If any piece of data in the first subset is missing and the
C        corresponding piece of data is NOT missing in the second
C        subset, also overwrite the PREPBUFR category parameter in the
C        first subset with that from the second subset regardless of
C        its value in either subset
C       --------------------------------------------------------------

                              evns  (  8, lv1, jj, kk ) =
     +                        evns2 (  8, lv2, jj, kk )
                           END IF
                        END IF
                     END DO
                  END DO
               END DO
               CYCLE LOOP1
            ELSE IF  (  ( pob2 .gt. pob1 )  .or.
     +                  ( lv1  .eq. nlev )  )  THEN

C       Either all remaining pressure levels within the first subset
C        are less than this pressure level from the second subset
C        (since levels within each subset are guaranteed to be in
C        descending order wrt pressure!) *OR* there are more total
C        levels within the second subset than in the first subset.
C        In either case, we should now add this second subset level
C        to the end of the EVNS array.
C       ------------------------------------------------------------

               nlev = nlev + 1
               evns  ( :, nlev, :, : ) = evns2 ( :, lv2,  :, : )
               drft  ( :, nlev ) = drft2 ( :, lv2 )
               toth  ( :, nlev ) = toth2 ( :, lv2 )
               CYCLE LOOP 1
            END IF
         END DO
      END DO  LOOP1

C       Sort combined report according to decreasing pressure.
C       ------------------------------------------------------

      p(:) = evns  ( 1, :, 1, 1)
      if(nlev .gt. 0)  call indexf(nlev,p,indr)
      evns_save = evns
      drft_save = drft
      toth_save = toth
      do i = 1,nlev
         j = indr(nlev-i+1)
         evns ( :, i, :, :) = evns_save ( :, j, :, :)
         drft ( :, i) = drft_save ( :, j)
         toth ( :, i) = toth_save ( :, j)
      end do

C       Return to calling program with a complete report in common
C        /PREPBC/.
C       ----------------------------------------------------------

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INDEXF      GENERAL SORT ROUTINE FOR REAL ARRAY
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 1995-05-30
C
C ABSTRACT: USES EFFICIENT SORT ALGORITHM TO PRODUCE INDEX SORT LIST
C   FOR AN REAL ARRAY.  DOES NOT REARRANGE THE FILE.
C
C PROGRAM HISTORY LOG:
C 1993-06-05  R  KISTLER -- FORTRAN VERSION OF C-PROGRAM
C 1995-05-30  D. A. KEYSER - TESTS FOR < 2 ELEMENTS IN SORT LIST,
C             IF SO RETURNS WITHOUT SORTING (BUT FILLS INDX ARRAY)
C
C USAGE:    CALL INDEXF(N,ARRIN,INDX)
C   INPUT ARGUMENT LIST:
C     N        - SIZE OF ARRAY TO BE SORTED
C     ARRIN    - REAL ARRAY TO BE SORTED
C
C   OUTPUT ARGUMENT LIST:
C     INDX     - ARRAY OF POINTERS GIVING SORT ORDER OF ARRIN IN
C              - ASCENDING ORDER {E.G., ARRIN(INDX(I)) IS SORTED IN
C              - ASCENDING ORDER FOR ORIGINAL I = 1, ... ,N}
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM-SP
C
C$$$
      SUBROUTINE INDEXF(N,ARRIN,INDX)
      INTEGER  INDX(N)
      REAL  ARRIN(N)
      DO J = 1,N
         INDX(J) = J
      ENDDO
C MUST BE > 1 ELEMENT IN SORT LIST, ELSE RETURN
      IF(N.LE.1)  RETURN
      L = N/2 + 1
      IR = N
   33 CONTINUE
      IF(L.GT.1)  THEN
         L = L - 1
         INDXT = INDX(L)
         AA = ARRIN(INDXT)
      ELSE
         INDXT = INDX(IR)
         AA = ARRIN(INDXT)
         INDX(IR) = INDX(1)
         IR = IR - 1
         IF(IR.EQ.1)  THEN
            INDX(1) = INDXT
            RETURN
         END IF
      END IF
      I = L
      J = L * 2
   30 CONTINUE
      IF(J.LE.IR)  THEN
         IF(J.LT.IR)  THEN
            IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))  J = J + 1
         END IF
         IF(AA.LT.ARRIN(INDX(J)))  THEN
            INDX(I) = INDX(J)
            I = J
            J = J + I
         ELSE
            J = IR + 1
         END IF
      END IF
      IF(J.LE.IR)  GO TO 30
      INDX(I) = INDXT
      GO TO 33
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      BLOCK DATA

      PARAMETER ( NHR8PM = 14 )    ! Actual number of BUFR parameters
                                   !  in header
      PARAMETER ( MXR8PM =  8 )    ! Maximum number of BUFR parameters
                                   !  in level data (non-radiance) or
                                   !  in channel data (radiance)
      PARAMETER ( MXR8LV = 255 )   ! Maximum number of BUFR levels/
                                   !                        channels
      PARAMETER ( MXR8VN = 10 )    ! Max. number of BUFR event sequences
                                   !  (non-radiance reports)
      PARAMETER ( MXR8VT = 17)     ! Max. number of BUFR variable types
                                   !  (non-radiance reports)

      REAL*8    hdr ( NHR8PM ), qkswnd_hdr(3), aircar_hdr(6),
     +          aircft_hdr(4),  adpupa_hdr(1), goesnd1_hdr(6),
     +          goesnd2_hdr(MXR8LV), adpsfc_hdr(5), sfcshp_hdr(2),
     +          satwnd_hdr(1), evns ( MXR8PM, MXR8LV, MXR8VN, MXR8VT ),
     +          drft ( 3, MXR8LV ), brts ( MXR8PM, MXR8LV ),
     +          toth ( 2, MXR8LV )

      INTEGER   indxvr1 ( 100:299 ), indxvr2 ( 100:299 )

      COMMON  / PREPBC /      hdr, evns, drft, toth, brts, qkswnd_hdr,
     +  aircar_hdr, aircft_hdr, adpupa_hdr, goesnd1_hdr, goesnd2_hdr,
     +  adpsfc_hdr, sfcshp_hdr, satwnd_hdr, indxvr1, indxvr2, nlev, nchn

      DATA      nlev /0/, nchn /0/

      DATA      indxvr1
     + /   20*1,    30*1,   10,  15,   9,    3*1,    4*11,    40*1 ,
C         100-119  120-149  150  151  152  153-155  156-159  160-199
     +     20*1,    40*1,    25*1,    1,   1,   13*1    /
C         200-219  220-259  260-284  285  286  287-299

      DATA      indxvr2
     + /   20*4,    30*8,   10,  17,   9,    3*6,    4*14,    40*6 ,
C         100-119  120-149  150  151  152  153-155  156-159  160-299
     +     20*6,    40*8,    25*6,    8,   8,   13*6    /
C         200-219  220-259  260-284  285  286  287-299

      END



