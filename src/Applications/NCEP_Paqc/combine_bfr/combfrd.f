C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: BUFR_COMBFR
C   PRGMMR: KEYSER           ORG: NP22        DATE: 1999-07-14
C
C ABSTRACT: COMBFR WILL CONCATENATE BUFR FILES. THE DUMP SCRIPT MAY
C   PRODUCE MORE THAN ONE DUMP FILE IN A DATA GROUP (E.G. ADPUPA),
C   AND COMBJB WILL CONSOLIDATE THESE FILES AS A USER OPTION. UP
C   TO 30 FILES MAY BE SPECIFIED FOR COMBINATION. THE PATH/FILE
C   NAMES OF THE FILES TO COMBINE ARE READ FROM STANDARD INPUT (UNIT
C   5) AND CONNECTED VIA THE FORTRAN OPEN STATEMENT. THE OUTPUT FILE
C   (UNIT 50) IS CONNECTED EXTERNALLY BECAUSE THE FORM=SYSTEM OPTION
C   IS NOT SUPPORTED IN CRAY FORTRAN.
C
C PROGRAM HISTORY LOG:
C 1996-09-06  J. WOOLLEN  ORIGINAL VERSION FOR IMPLEMENTATION
C 1996-11-27  J. WOOLLEN  MADE OUTPUT FILE BUFR TABLE CHOOSING MORE
C                         SECURE
C 1999-06-03  D. KEYSER   MODIFIED TO PORT TO IBM SP AND RUN IN 4 OR
C                         8 BYTE STORAGE
! 2006-01-10  Sienkiewicz  add command line specfication of output file
! 2007-11-15  Sienkiewicz  call to BORT if input list contains all zero-length
!                          files - avoids system message about formatted input
! 2008-04-16  Sienkiewicz  New version - combfrd based on 'combfr' program.
!                          Modify so that 'center time' dates in files must
!                          match time in first file, or a time specified
!                          on the command line.
! 2013-12-09  Sienkiewicz  copy J. Woollen's change in current NCEP code
!                          to avoid using MESGBF to find BUFR tables
!
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STDIN
C     UNIT 20  - INPUT FILE 1
C     UNIT 21  - INPUT FILE 2
C     UNIT 22  - INPUT FILE 3
C     ...
C     UNIT 49  - INPUT FILE 30
C
C   OUTPUT FILES:
C     UNIT 50  - COMBINED FILE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - NONE
C     LIBRARY:   - W3LIB90,BUFRLIB90
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      PROGRAM COMBFR
 
      CHARACTER*80 FILE(30)
      CHARACTER*8  SUBSET, JSUBSET
      character*120 outfile
      character*10 cdate
      integer mdate, jdate
 
      DATA LUNIN,LUNOT/20,50/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     CALL W3TAGB('BUFR_COMBFR',1999,0195,0053,'NP22')
 
      NFIL = 0
      MDATE = 0
      JDATE = 0
      call datelen(10)

      nargs = iargc()
      if ( nargs .gt. 0 ) then
        call getarg(1,outfile)
        if (trim(outfile) .eq. '-d') then
           if (nargs .lt. 3) then
              print 90
              call exit(3)
           endif
           call getarg(2,cdate)
           call getarg(3,outfile)
           read(cdate,'(i10)') mdate
           print 100, mdate
        endif
        open(unit=lunot,file=outfile,form='unformatted')
      else
         print 90
         call exit(2)
      endif
 
C  READ THE LOCATIONS OF FILES TO COMBINE
C  --------------------------------------
 
      DO WHILE(NFIL.GE.0)
      IF(NFIL+1.GT.30) CALL BORT('TOO MANY INPUT FLIES')
      READ(5,'(A80)',END=1) FILE(NFIL+1)
      NFIL = NFIL+1
      ENDDO
1     CONTINUE
 
C  OPEN THE OUTPUT FILE WITH TABLES FROM THE FIRST INPUT THAT HAS DATA
C  -------------------------------------------------------------------
 
      DO N=1,NFIL
         CALL CLOSBF(LUNIN)
         OPEN(LUNIN,FILE=FILE(N),FORM='UNFORMATTED')
         CALL OPENBF(LUNIN,'IN ',LUNIN)
         IF(IREADMG(LUNIN,SUBSET,IDATE)==0) THEN
            CALL OPENBF(LUNOT,'OUT',LUNIN)
            CALL CLOSBF(LUNIN)
            GOTO 2
         ENDIF
      ENDDO
      call bort('BUFR TABLES NOT FOUND - ALL ZERO LENGTH INPUT FILES?')
 2    CONTINUE
      
C  COMBINE ALL MESSAGES FROM ALL INPUT FILES
C  -----------------------------------------
 
      DO N=1,NFIL
         CALL CLOSBF(LUNIN)
         OPEN(LUNIN,FILE=FILE(N),FORM='UNFORMATTED')
         CALL OPENBF(LUNIN,'IN',LUNOT)
C
C  * Read the first record from the file
C
         if (ireadmg(lunin,subset,idate).eq.0) then
C
C  * If mdate not set from command line, use the date/time stamp returned
C     to set mdate (the required date)
C
            if (mdate .eq. 0) then
               mdate = idate
               print 100, mdate
            endif
C
C  * Check if date/time stamp returned matches the required date
C     If not - print messge
C     If so - copy this message buffer and the remaining ones in the file
C      to the output BUFR file
C  

            if (idate .ne. mdate) then
               print 110, trim(file(n)), idate
            else
               CALL COPYMG(LUNIN,LUNOT)
               
               DO WHILE(IREADMG(LUNIN,SUBSET,IDATE).EQ.0)
                  CALL COPYMG(LUNIN,LUNOT)
               ENDDO
            endif
         endif
      ENDDO
C     CALL W3TAGE('BUFR_COMBFR')

 90   format('USAGE: combfrd.x [-d yyyymmddhh] outfile')
 100  format('COMBFRD: RETAIN DATA WITH CENTER DATE:  ',i10)
 110  format('COMBFRD: DISCARD DATA FROM FILE ',a,', DATE:  ',i10)
      STOP
      END
