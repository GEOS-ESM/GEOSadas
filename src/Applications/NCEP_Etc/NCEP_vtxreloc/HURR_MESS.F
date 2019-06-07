      SUBROUTINE HURR_MESS(ITIM,IMAX,JMAX,GLON,GLAT)
      use m_hurrmess, only: NST, KSTM, ST_NAME, TCVT
      use m_hurrmess, only: clon_tcvin, clat_tcvin, stat_tcvin
      use m_hurrmess, only: STRM_Normal, STRM_Bogus, STRM_NoTrack, STRM_Excluded
      use m_hurrmess, only: southEx_lat,northEx_lat
      use m_hurrmess, only: CLON_N, CLAT_N, SLON_N, SLAT_N, DLON_N, DLAT_N, IC_N, JC_N

      use m_vtxgrid, only: vtxgrid_lonNear
      use m_die    , only: die
      implicit none
      integer,intent(in) :: ITIM
      integer,intent(in) :: IMAX,JMAX
      REAL,intent(in) :: GLON(IMAX)
      REAL,intent(in) :: GLAT(JMAX)

      integer, PARAMETER :: IRX=41,JRX=41

      integer, PARAMETER :: MAXERR=100

      real :: STMDIR,STMSPD
      DIMENSION STMDIR(NST),STMSPD(NST)
      CHARACTER(len=3),dimension(NST) :: STMNAME

      CHARACTER(len= 1) :: BUFIN(len(TCVT))
      CHARACTER(len= 1) :: BUFY2K(len(TCVT))
      CHARACTER(len=12) :: STMNAM(NST)
      CHARACTER(len= 9) :: STMNMZ

      integer, PARAMETER :: MAXVIT=15
      CHARACTER FMTVIT(MAXVIT)*6,BUFINZ*100,LATNS*1,LONEW*1

      integer :: IVTVAR,ISTVAR,IENVAR
      real    :: VITVAR,VITFAC
      DIMENSION IVTVAR(MAXVIT),VITVAR(MAXVIT),VITFAC(MAXVIT),
     1          ISTVAR(MAXVIT),IENVAR(MAXVIT)

      DATA ISTVAR/20,29,34,39,45,49,53,58,63,68,71,75,80,85,90/
      DATA IENVAR/27,32,36,42,47,51,56,61,66,69,73,78,83,88,93/
      DATA VITFAC/2*1.0,2*0.1,1.0,0.1,9*1.0/
      DATA FMTVIT/'(I8.8)','(I4.4)','(I3.3)','(I4.4)',2*'(I3.3)',
     1            3*'(I4.4)','(I2.2)','(I3.3)',4*'(I4.4)'/

      integer :: ISTMCX1,ISTMCY1
      real    :: STMCX,STMCY
      DIMENSION ISTMCX1(3,NST),ISTMCY1(3,NST),STMCX(NST),STMCY(NST)

      real :: STMLTZ,STMLNZ,STMDRZ,STMSPZ
      EQUIVALENCE (BUFIN(37),LATNS),(BUFIN(43),LONEW),
     1            (BUFIN(10),STMNMZ),(BUFIN(1),BUFINZ)
C      EQUIVALENCE (IVTVAR(1),IDATEZ),(IVTVAR(2),IUTCZ)
C
      EQUIVALENCE (VITVAR( 3),STMLTZ),(VITVAR( 4),STMLNZ),
     1            (VITVAR( 5),STMDRZ),(VITVAR( 6),STMSPZ)
      INTEGER ERRCNT
      integer :: I,J,K,KREC,KSTORM,NERROR,NCH,IV,IERDEC
      integer :: KC
      real    :: PI,PI180,DT,ONEDEG,FACT
      integer :: INDX1,K1STM,IFWRT
      real    :: XDIST6H,YDIST6H,USTM,VSTM
      real    :: AMN,DMN,BMN,OMN
      real    :: CLAT,CLON
      integer :: JC,ILA,IC,ILO,IC2

      character(len=*),parameter:: myname='HURR_MESS'
C
      DO I=1,10
        SLON_N(I)=0.
        SLAT_N(I)=0.
        CLON_N(I)=0.
        CLAT_N(I)=0.
        IC_N(I)=0
        JC_N(I)=0
      END DO

  90  REWIND 11
      KREC=0
      KSTORM=0
      NERROR=0
C
C  Get the hurricane center from the hurricane message made by NHC
C
C     READ A RECORD INTO BUFFER
C
      ERRCNT = 0
  100 CONTINUE
      READ(11,101,ERR=990,END=200) (BUFIN(NCH),NCH=1,95)
  101 FORMAT(95A1)

      if(BUFIN(35).eq.'N' .or. BUFIN(35).eq.'S')  then

         print *, ' '
         print *, '==> Read in RECORD from tcvitals file -- contains a',
     $    ' 2-digit year "'
         print *, ' '

         BUFY2K(1:19) = BUFIN(1:19)
         IF(BUFIN(20)//BUFIN(21).GT.'20')  THEN
            BUFY2K(20) = '1'
            BUFY2K(21) = '9'
         ELSE
            BUFY2K(20) = '2'
            BUFY2K(21) = '0'
         ENDIF
         BUFY2K(22:95) = BUFIN(20:93)
         BUFIN = BUFY2K

         print *, ' '
         print *, '==> 2-digit year converted to 4-digit year "'
         print *, ' '

      else  if(BUFIN(37).eq.'N' .or. BUFIN(37).eq.'S')  then

         print *, ' '
         print *, '==> Read in RECORD from tcvitals file -- contains a',
     $    ' 4-digit year "'
         print *, ' '

      else

         print *, ' '
         print *, '***** Cannot determine if this record contains ',
     $    'a 2-digit year or a 4-digit year - skip it and try reading ',
     $    'the next record'
         print *, ' '
         go to 100

      end if

      KREC=KREC+1

      DO I=1,3
        ST_NAME(KREC)(I:I)=BUFIN(I+5)
      END DO
      DO I=1,95
        TCVT(KREC)(I:I)=BUFIN(I)
      END DO
C
C     DECODE DATE AND TIME
C
      DO 110 IV=1,2
      CALL DECVAR(ISTVAR(IV),IENVAR(IV),IVTVAR(IV),IERDEC,FMTVIT(IV),
     1            BUFINZ)

  110 CONTINUE

      DO 140 IV=3,MAXVIT
      CALL DECVAR(ISTVAR(IV),IENVAR(IV),IVTVAR(IV),IERDEC,FMTVIT(IV),
     1            BUFINZ)
      VITVAR(IV)=REAL(IVTVAR(IV))*VITFAC(IV)
  140 CONTINUE

C          *****************************************************
C          *****************************************************
C          ****            IMPORTANT NOTES:                 ****
C          ****                                             ****
C          ****    ALL STORM LONGITUDES CONVERTED TO        ****
C          ****    0-360 DEGREES, POSITIVE EASTWARD  !!!    ****
C          ****                                             ****
C          ****    ALL STORM SPEEDS ARE IN M/SEC            ****
C          ****                                             ****
C          ****    ALL DISTANCE DATA ARE IN KM              ****
C          ****                                             ****
C          ****    ALL PRESSURE DATA ARE IN HPA (MB)        ****
C          *****************************************************
C          *****************************************************
C
C     SIGN OF LATITUDE AND CONVERT LONGITUDE
C
      IF(LATNS .EQ. 'S')  THEN
      STMLTZ=-STMLTZ
      ELSE IF(LATNS .NE. 'N')  THEN
      WRITE(6,153) STMLTZ,STMLNZ,LATNS
  153 FORMAT('******ERROR DECODING LATNS, ERROR RECOVERY NEEDED.',
     1       '  STMLTZ,STMLNZ,LATNS=',2F12.2,2X,A1)
      GO TO 100
      ENDIF
        ! STMLTZ (storm latitude) values are now all in latitude-north
C
      IF(LONEW .EQ. 'W')  THEN
      STMLNZ=360.-STMLNZ
      ELSE IF(LONEW .NE. 'E')  THEN
      WRITE(6,157) STMLTZ,STMLNZ,LATNS
  157 FORMAT('******ERROR DECODING LONEW, ERROR RECOVERY NEEDED.',
     1       '  STMLTZ,STMLNZ,LATNS=',2F12.2,2X,A1)
      ENDIF
        ! STMLNZ (storm longitude) values are now all in longitude-east
C
      IF(KSTORM .LT. 10)  THEN
      KSTORM=KSTORM+1
      CLAT_N(KSTORM)=STMLTZ
      CLON_N(KSTORM)=STMLNZ
      STMDIR(KSTORM)=STMDRZ
      STMSPD(KSTORM)=STMSPZ
      STMNAM(KSTORM)=STMNMZ

        clon_tcvin(KSTORM) = CLON_N(KSTORM)
        clat_tcvin(KSTORM) = CLAT_N(KSTORM)
        stat_tcvin(KSTORM) = STRM_Normal
        if( clat_tcvin(KSTORM)<=southEx_lat .or.
     &      clat_tcvin(KSTORM)>=northEx_lat ) then
          stat_tcvin(KSTORM) = STRM_Excluded
        endif
      GO TO 100
C
      ELSE

  300 WRITE(6,301) KSTORM
  301 FORMAT(/'******KSTORM EXCEEDS AVAILABLE SPACE, KSTORM=',I5
     1       ,/,' Results may have serious problem')
      GO TO 200

      ENDIF

  200 IF(KSTORM .GT. 0)  THEN
      WRITE(6,201)KSTORM,KREC
  201 FORMAT(/'...FOUND STORM IN VITALS FILE.',/,4X,I5,
     2       ' TOTAL NUMBER OF RECORDS READ=',I7)
      ELSE
      WRITE(6,202)
  202 FORMAT(/'NO STORM FOUND IN VITALS FILE.')
      CALL W3TAGE('RELOCATE_MV_NVORTEX_T254L64')
      CALL ERREXIT(56)
      END IF
C
c  Correct to the storm center position

      PI=ATAN(1.0)*4.E+00
      PI180 = PI/180.
      DT=(float(ITIM)-6.)*3600.                     !  Second
      ONEDEG=360./(2.*PI*6.37E6)                    !  Degree/Meter
      FACT=DT*ONEDEG

      KSTM=KSTORM

c      WRITE(12, 233) KSTM
c 233  FORMAT(2x,I5)

      DO I=1,KSTM

      WRITE(*,430)STMNAM(I),CLAT_N(I),CLON_N(I),STMDIR(I),STMSPD(I)
  430 FORMAT(/' STORM NAME: ',A12,/, ' READIN STORM CENTER=',2F12.4,
     1       /,' STORM DIR and SPEED: ',2F12.4)

cnew        USTM=STMSPD(I)*SIN(PI180*STMDIR(I))
cnew        VSTM=STMSPD(I)*COS(PI180*STMDIR(I))
cnew        CLON_N(I)=CLON_N(I)+USTM*FACT/COS(PI180*CLAT_N(I))
cnew        CLAT_N(I)=CLAT_N(I)+VSTM*FACT

cnew        PRINT*, 'CORRECTED STORM CENTER AT TIME HOUR ',ITIM,' =',
cnew     1           CLON_N(I),CLAT_N(I)

      END DO

      INDX1=ITIM/3

      K1STM=0
      DO I=1,10
        STMCX(I)=0.
        STMCY(I)=0.
        STMNAME(I)='NUL'
        READ(30,442,end=436)
     &     (ISTMCY1(J,I),ISTMCX1(J,I),J=1,3),STMNAME(I)
        STMCX(I)=360.-ISTMCX1(INDX1,I)*0.1
        STMCY(I)=ISTMCY1(INDX1,I)*0.1
        K1STM=K1STM+1
        PRINT'(a,i2.2,2a,2f10.4)', ' CT STORM Model CENTER at ',ITIM,'h = ',
     &          STMNAME(I),STMCX(I),STMCY(I)
      END DO
 442  FORMAT(22x,6i4,25x,A3)
 436  CONTINUE

      REWIND 30

      DO I=1,KSTM

        KC=-1
        DO K=1,K1STM
                ! Look for a matching storm in the tracker reports.
          IF(STMNAME(K).EQ.ST_NAME(I))THEN
            IFWRT=0
            DO J=1,3
            IF(ISTMCY1(J,K).EQ.0.and.ISTMCX1(J,K).EQ.0)THEN
              IFWRT=1
            END IF
            END DO
            IF(IFWRT.EQ.0)THEN
                ! If all forecast track data are good for this storm, code below
                ! is equivalent to,
                !
                !       CLON_N(I) = CLON_N(I) + ( STMCX(K)-(360.-ISTMCX1(2,K)*0.1) )
                !       CLAT_N(I) = CLAT_N(I) + ( STMCY(K)-ISTMCY1(2,K)*0.1 )
                !
                ! as a time correction to the observed center (CLON_N,CLAT_N),
                ! based on the forecast.

              XDIST6H=CLON_N(I)-(360.-ISTMCX1(2,K)*0.1)
              YDIST6H=CLAT_N(I)-ISTMCY1(2,K)*0.1
              CLON_N(I)=STMCX(K)+XDIST6H
              CLAT_N(I)=STMCY(K)+YDIST6H

            ELSE
                ! Otherwise, this time correction to the observed center is
                ! done, based on the observed velosity of the storm center.

              USTM=STMSPD(I)*SIN(PI180*STMDIR(I))
              VSTM=STMSPD(I)*COS(PI180*STMDIR(I))
              CLON_N(I)=CLON_N(I)+USTM*FACT/COS(PI180*CLAT_N(I))
              CLAT_N(I)=CLAT_N(I)+VSTM*FACT
              stat_tcvin(I) = STRM_Bogus     ! IFWRT==1
            END IF
            PRINT'(a,i2.2,2a,2f10.4)', ' CT STORM OBS. CENTER at ',ITIM,'h = ',
     &               STMNAME(K),CLON_N(I),CLAT_N(I)
            KC=K
            exit
          END IF
        END DO    ! K=1,K1STM

        if(KC<0) then
                ! Time adjustment of the storm center.
          USTM=STMSPD(I)*SIN(PI180*STMDIR(I))
          VSTM=STMSPD(I)*COS(PI180*STMDIR(I))
          CLON_N(I)=CLON_N(I)+USTM*FACT/COS(PI180*CLAT_N(I))
          CLAT_N(I)=CLAT_N(I)+VSTM*FACT
          stat_tcvin(I) = STRM_NoTrack
        endif
          

      END DO    ! I=1,KSTM

      DO 900 I=1,KSTM

      CLON=CLON_N(I)
      CLAT=CLAT_N(I)

      JC=-1
      AMN = 500.
      DO 10 ILA = 1,JMAX
      DMN = ABS (GLAT(ILA) - CLAT)
C
      IF (DMN.LE.AMN) THEN      ! locate the nearest grid point
      AMN = DMN
      JC  = ILA
      ENDIF
C
10    CONTINUE
        if (JC<1 .or. JC>JMAX) then
          write(6,'(2a,i10  )') myname,'() >>> ERROR <<< undetermined location, JC =',JC
          write(6,'(2a,i10  )') myname,'() >>> ERROR <<<                      JMAX =',JMAX
          write(6,'(2a,f10.3)') myname,'() >>> ERROR <<<                      CLAT =',CLAT
          write(6,'(2a,f10.3)') myname,'() >>> ERROR <<<                GLAT(   1) =',GLAT(1)
          write(6,'(2a,f10.3)') myname,'() >>> ERROR <<<                GLAT(JMAX) =',GLAT(JMAX)
          call die(myname)
        endif
C
      BMN = 500.
      DO 20 ILO = 1,IMAX
      OMN = ABS (GLON(ILO) - CLON)
C
      IF (OMN.LE.BMN) THEN      ! locate the nearest grid point
      BMN = OMN
      IC  = ILO
      ENDIF
C
20    CONTINUE

      IC2 = vtxgrid_lonNear(GLON(1:IMAX),CLON)
        if (IC/=IC2) then
          write(6,'(2a,i10  )') myname,'() >>> WARNING <<< different grid location, new-IC =',IC2
          write(6,'(2a,i10  )') myname,'() >>> WARNING <<<                          old-IC =',IC
          write(6,'(2a,f10.3)') myname,'() >>> WARNING <<<                            CLON =',CLON
          write(6,'(2a,f10.3)') myname,'() >>> WARNING <<<                    GLON(new-IC) =',GLON(IC2)
          if(IC>=1.and.IC<=IMAX)
     &    write(6,'(2a,f10.3)') myname,'() >>> WARNING <<<                    GLON(old-IC) =',GLON(IC)
          IC = IC2
        endif

        if (IC<1 .or. IC>IMAX) then
          write(6,'(2a,i10  )') myname,'() >>> ERROR <<< undetermined location, IC =',IC
          write(6,'(2a,i10  )') myname,'() >>> ERROR <<<                      IMAX =',IMAX
          write(6,'(2a,f10.3)') myname,'() >>> ERROR <<<                      CLON =',CLON
          write(6,'(2a,f10.3)') myname,'() >>> ERROR <<<                GLON(   1) =',GLON(1)
          write(6,'(2a,f10.3)') myname,'() >>> ERROR <<<                GLON(IMAX) =',GLON(IMAX)
          call die(myname)
        endif

      IC_N(I)=IC
      JC_N(I)=JC
C
      PRINT *,'  '
c      PRINT *,'==========AT SUB HURR_MESS============='
c      PRINT *,'... 1st guess ... center of hurricane'
c      PRINT *,'===IC,JC=== ',IC,JC,GLON(IC),GLAT(JC)
c      PRINT *,'==DIST OF CLON AND IC===',BMN
c      PRINT *,'==DIST OF CLAT AND JC===',AMN

      SLON_N(I) = IFIX(GLON(IC)+0.5 - IRX/2)
      SLAT_N(I) = IFIX(GLAT(JC)+0.5 - JRX/2)
      SLAT_N(I) = max(-90.,SLAT_N(I))
      if( SLAT_N(I)+(JRX-1)*DLAT_N > +90. ) then
        SLAT_N(I) = 90.-(JRX-1)*DLAT_N
      endif
      PRINT *,' '
c      PRINT *,'=========================================='
c      PRINT *,'SLAT, SLON = ', SLAT_N(I),SLON_N(I)
c      WRITE(12,123)SLON_N(I),SLAT_N(I),CLON_N(I),CLAT_N(I)
c123   FORMAT(1x,4F10.3)
      PRINT *,'=========================================='

  900 CONTINUE

      RETURN

  990 WRITE(6,991) BUFIN
  991 FORMAT('******ERROR READING STORM RECORD.  BUFIN IS:',/,
     1       ' ******',A95,'******')
      ERRCNT = ERRCNT + 1
      IF (ERRCNT .LE. MAXERR) GO TO 100
      call die(myname,'STOP 99')
      STOP 99

      END SUBROUTINE HURR_MESS

      SUBROUTINE DECVAR(ISTART,IEND,IVALUE,IERDEC,FMT,BUFF)
C
      PARAMETER (NCHLIN=130)
C
      CHARACTER FMT*(*),BUFF*(*),OUTLIN*1
C
c      SAVE
C
      DIMENSION OUTLIN(NCHLIN)
C
c && 2 comments
CC    WRITE(6,1) FMT,BUFF
CC  1 FORMAT(/'...FMT=',A10,/,' ...BUFF=',A100)
C
      READ(BUFF(ISTART:IEND),FMT,ERR=10)  IVALUE
      IERDEC=0
      RETURN
C
   10 CONTINUE  ! for READ(BUFF,ERR=10)
C
      OUTLIN=' '
C
      IERDEC=10
      OUTLIN(ISTART:IEND)='*'
C
      WRITE(6,31) (OUTLIN(ICH1),ICH1=1,NCHLIN)
      WRITE(6,32) BUFF
   31 FORMAT(/'******ERROR DECODING, BUFF=',/,130A1)
   32 FORMAT(A130)
C
      RETURN
      END SUBROUTINE DECVAR
