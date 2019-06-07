      subroutine setglonglat(IMAX,JMAX,GLON,GLAT)
!-----------------------------------------------------------------------
      use m_die,only : die
      implicit none
      integer,intent(in) :: IMAX,JMAX
      real,dimension(imax),intent(out) :: GLON
      real,dimension(jmax),intent(out) :: GLAT

      integer :: JHF
      real,dimension(JMAX/2) :: COLRAD,WGT,WGTCS,RCS2
      real :: PI,RDR,DLN
      integer :: LL,LLS,LN

      if(mod(JMAX,2)/=0) call die("setglonglat","mod(JMAX,2)/=0")
      JHF=JMAX/2

      CALL GLATS(JHF,COLRAD,WGT,WGTCS,RCS2)

      PI=ASIN(1.)*2
      RDR=180./PI
C
      DO LL = 1,JHF
      LLS = JMAX+1 - LL
      GLAT(LL)  = 90. - COLRAD(LL)*RDR
      GLAT(LLS) = -GLAT(LL)
      ENDDO
C
      DLN = 360.0/FLOAT(IMAX)
      DO LN = 1,IMAX
      GLON(LN) = (LN-1) * DLN
      ENDDO
      end subroutine setglonglat
	subroutine setglonglat2(IMAX,JMAX,GLON,GLAT)
!-----------------------------------------------------------------------
!-- define 2-d (lon,lat) grid with Gaussian latitudes.
	use m_die,only : die
	implicit none
	integer,intent(in) :: IMAX,JMAX
	real,dimension(IMAX,JMAX),intent(out) :: GLON
	real,dimension(IMAX,JMAX),intent(out) :: GLAT

	integer :: JHF
	real,dimension(JMAX/2) :: COLRAD,WGT,WGTCS,RCS2

        real :: PI,RAD,DLN
	integer :: I,LL,LLS
	integer :: J,LN

	if(mod(JMAX,2)/=0) call die("setglonglat2","mod(JMAX,2)/=0")
	JHF=JMAX/2

      CALL GLATS(JHF,COLRAD,WGT,WGTCS,RCS2)
C
      PI=ASIN(1.)*2
      RAD=PI/180.
C
      DO I = 1,IMAX
      DO LL = 1,JHF
      LLS = JMAX+1 - LL
      GLAT(I,LL)  = 90. - COLRAD(LL)/RAD
      GLAT(I,LLS) = -GLAT(I,LL)
      ENDDO
      ENDDO
C
      DLN = 360.0/FLOAT(IMAX)
      DO J = 1,JMAX
      DO LN = 1,IMAX
      GLON(LN,J) = (LN-1) * DLN
      ENDDO
      ENDDO
      end subroutine setglonglat2
      SUBROUTINE GLATS(LGGHAF,COLRAD,WGT,WGTCS,RCS2)
!-----------------------------------------------------------------------
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GLATS       COMPUTES LOCATION OF GAUSSIAN LATITUDES.
C   PRGMMR: JOSEPH SELA      ORG: W/NMC23    DATE: 88-04-05
C
C ABSTRACT: COMPUTES THE LOCATION OF THE GAUSSIAN LATITUDES FOR THE
C   INPUT LGGHAF.  THE LATITUDES ARE DETERMINED BY FINDING
C   THE ZEROS OF THE LEGENDRE POLYNOMIALS.
C
C PROGRAM HISTORY LOG:
C   88-04-05  JOSEPH SELA
C
C USAGE:    CALL GLATS (LGGHAF, COLRAD, WGT, WGTCS, RCS2)
C   INPUT ARGUMENT LIST:
C     LGGHAF   - NUMBER OF GAUSSIAN LATITUDES IN A HEMISPHERE.
C
C   OUTPUT ARGUMENT LIST:
C     COLRAD   - ARRAY OF COLATITUDE OF GAUSSIAN LATITUDES
C                IN NORTHERN HEMISPHERE.
C     WGT      - ARRAY OF WEIGHTS AT EACH GAUSSIAN LATITUDE
C                REQUIRED FOR GAUSSIAN QUADRATURE.
C     WGTCS    - ARRAY OF GAUSSIAN WEIGHT/SIN OF COLATITUDE SQUARED.
C     RCS2     - ARRAY OF RECIPROCAL  OF  SIN OF COLATITUDE SQUARED.
C
C   OUTPUT FILES:
C     OUTPUT   - PRINTOUT FILE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 200.
C   MACHINE:  CYBER 205.
C
C$$$
CCCC  HALF PRECISION COLRAD,WGT,WGTCS,RCS2
      REAL COLRAD(LGGHAF),WGT(LGGHAF),WGTCS(LGGHAF)
      REAL RCS2(LGGHAF)
      EPS=1.E-12
C     PRINT 101
C101  FORMAT ('0 I   COLAT   COLRAD     WGT', 12X, 'WGTCS',
CCCC 1 10X, 'ITER  RES')
      SI = 1.0
      L2=2*LGGHAF
      RL2=L2
      SCALE = 2.0/(RL2*RL2)
      K1=L2-1
      PI = ATAN(SI)*4.E+00
      DRADZ = PI / 360.
      RAD = 0.0
      DO 1000 K=1,LGGHAF
      ITER=0
      DRAD=DRADZ
1     CALL POLY(L2,RAD,P2)
2     P1 =P2
      ITER=ITER+1
      RAD=RAD+DRAD
      CALL POLY(L2,RAD,P2)
      IF(SIGN(SI,P1).EQ.SIGN(SI,P2)) GO TO 2
      IF(DRAD.LT.EPS)GO TO 3
      RAD=RAD-DRAD
      DRAD = DRAD * 0.25
      GO TO 1
3     CONTINUE
      COLRAD(K)=RAD
      PHI = RAD * 180 / PI
      CALL POLY(K1,RAD,P1)
      X = COS(RAD)
      W = SCALE * (1.0 - X*X)/ (P1*P1)
      WGT(K) = W
      SN = SIN(RAD)
      W=W/(SN*SN)
      WGTCS(K) = W
      RC=1./(SN*SN)
      RCS2(K) = RC
      CALL POLY(L2,RAD,P1)
C     PRINT 102,K,PHI,COLRAD(K),WGT(K),WGTCS(K),ITER,P1
C102  FORMAT(1H ,I2,2X,F6.2,2X,F10.7,2X,E13.7,2X,E13.7,2X,I4,2X,D13.7)
1000  CONTINUE
      RETURN
      END SUBROUTINE GLATS
      SUBROUTINE POLY(N,RAD,P)
!-----------------------------------------------------------------------
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    POLY        EVALUATES LEGENDRE POLYNOMIAL.
C   PRGMMR: JOSEPH SELA      ORG: W/NMC23    DATE: 88-04-01
C
C ABSTRACT: EVALUATES THE UNNORMALIZED LEGENDRE POLYNOMIAL
C   OF SPECIFIED DEGREE AT A GIVEN COLATITUDE USING A STANDARD
C   RECURSION FORMULA.  REAL ARITHMETIC IS USED.
C
C PROGRAM HISTORY LOG:
C   88-04-01  JOSEPH SELA
C
C USAGE:    CALL POLY (N, RAD, P)
C   INPUT ARGUMENT LIST:
C     N        - DEGREE OF LEGENDRE POLYNOMIAL.
C     RAD      - REAL COLATITUDE IN RADIANS.
C
C   OUTPUT ARGUMENT LIST:
C     P        - REAL VALUE OF LEGENDRE POLYNOMIAL.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 200.
C   MACHINE:  CYBER 205.
C
C$$$
      X = COS(RAD)
      Y1 = 1.0
      Y2=X
      DO 1 I=2,N
      G=X*Y2
      Y3=G-Y1+G-(G-Y1)/FLOAT(I)
      Y1=Y2
      Y2=Y3
1     CONTINUE
      P=Y3
      RETURN
      END SUBROUTINE POLY
      subroutine maxmin(a,len,k,k1,k2,ch)
!-----------------------------------------------------------------------
      dimension a(len,k)
      character ch*(*)
c
cccc cmic$ do all
cccc cmic$1 shared(a,ch,len,k1,k2)
cccc cmic$1 private(aamax,aamin,m)
      do 100 j=k1,k2
      aamax = a(1,j)
      aamin = a(1,j)
      do 10 m=1,len
      aamax = max( aamax, a(m,j) )
      aamin = min( aamin, a(m,j) )
10    continue
C      print   *,ch,' has max=',aamax,' min=',aamin
100   continue
      return
      end subroutine maxmin
      SUBROUTINE WRIT1(IRX,JRX,DIN,NWT1,NWRT1,MTV2,NST,IUT,HDAT)
      !!PARAMETER (IRX=41,JRX=41,NST=10)
      implicit none
      integer,intent(in) :: IRX,JRX
      REAL,intent(in) :: DIN(IRX,JRX)
      integer,intent(inout) :: NWT1,NWRT1
      integer,intent(in) :: MTV2,NST,IUT
      real,intent(out) :: HDAT(IRX,JRX,MTV2,NST)

      integer :: I,J
      NWRT1=NWRT1+1
      NWT1=NWT1+1
c      PRINT*,'WRIT1 COUNT = ',NWRT1,NWT1,IUT
      DO J=1,JRX
      DO I=1,IRX
        HDAT(I,J,NWRT1,IUT)=DIN(I,J)
      END DO
      END DO
      END SUBROUTINE WRIT1
      SUBROUTINE READ1(IUT,NRED1,MTV3,DOUT,PDAT)
      PARAMETER (IRX=41,JRX=41)
      REAL DOUT(IRX,JRX),PDAT(IRX,JRX,MTV3)
      NRED1=NRED1+1
c      PRINT*,'READ1 COUNT = ',NRED1
      DO J=1,JRX
      DO I=1,IRX
        DOUT(I,J)=PDAT(I,J,NRED1)
      END DO
      END DO
      END SUBROUTINE READ1
      SUBROUTINE WRIT2(IMAX,JMAX,NWRT2,MTV,DIN,HDATA)
      REAL DIN(IMAX,JMAX),HDATA(IMAX,JMAX,MTV)
      NWRT2=NWRT2+1
c      PRINT*,'WRIT2 COUNT = ',NWRT2
c      call maxmin(DIN,IMAX*JMAX,1,1,1,'DIN in gbl')
      DO J=1,JMAX
      DO I=1,IMAX
        HDATA(I,J,NWRT2)=DIN(I,J)
      END DO
      END DO
      END SUBROUTINE WRIT2
      SUBROUTINE READ2(IMAX,JMAX,NRED2,MTV,DOUT,HDATA)
      REAL DOUT(IMAX,JMAX),HDATA(IMAX,JMAX,MTV)
      NRED2=NRED2+1
c      PRINT*,'READ2 COUNT = ',NRED2
      DO J=1,JMAX
      DO I=1,IMAX
        DOUT(I,J)=HDATA(I,J,NRED2)
      END DO
      END DO
      END SUBROUTINE READ2
      SUBROUTINE FIND_NEWCT(UD,VD)
      PARAMETER (IR=15,IT=24,IX=41,JX=41,ID=7,JD=7)
      DIMENSION TNMX(ID,JD),UD(IX,JX),VD(IX,JX)
      DIMENSION WTM(IR),R0(IT)
       
	! intent(in) :: ix,jx
	! intent(in) :: ud(ix,jx),vd(ix,jx)
	! intent(in) :: ir,it
      	! intent(in) :: slon,slat,clon,clat
	! intent(inout) :: clon_new,clat_new
	! unused :: r0(it), xvect(it), yvect(it)
	! remove :: common/POSIT/,common/vect/
	! integer,parameter,local :: id=7,jd=7
	! local :: wnmx(id,jd),wtm(ir)

      COMMON /POSIT/CLON_NEW,CLAT_NEW,SLON,SLAT,CLON,CLAT,RAD
      COMMON /vect/R0,XVECT(IT),YVECT(IT)
C
      PI=ASIN(1.)*2.
      RAD=PI/180.
C
      XLAT = CLAT-3.
      XLON = CLON-3.
c      print *,'STARTING LAT, LON AT FIND NEW CENTER ',XLAT,XLON
C
      DO I=1,ID
      DO J=1,JD
      TNMX(I,J) = 0.
      BLON = XLON + (I-1)
      BLAT = XLAT + (J-1)
C
C.. CALCULATE TANGENTIAL WIND EVERY 1 deg INTERVAL
C..  7*7 deg AROUND 1ST 1ST GUESS VORTEX CENTER
C
      DO 10 JL=1,IR
      WTS= 0.
      DO 20 IL=1,IT
      DR = JL
      DD = (IL-1)*15*RAD
      DLON = DR*COS(DD)
      DLAT = DR*SIN(DD)
      TLON = BLON + DLON
      TLAT = BLAT + DLAT
C.. INTERPOLATION U, V AT TLON,TLAT AND CLACULATE TANGENTIAL WIND
      IDX = IFIX(TLON) - SLON + 1
      IDY = IFIX(TLAT) - SLAT + 1
      DXX  = TLON - IFIX(TLON)
      DYY  = TLAT - IFIX(TLAT)
C
      X1 = UD(IDX  ,IDY+1)*DYY + UD(IDX  ,IDY)*(1-DYY)
      X2 = UD(IDX+1,IDY+1)*DYY + UD(IDX+1,IDY)*(1-DYY)
      Y1 = UD(IDX+1,IDY  )*DXX + UD(IDX,IDY  )*(1-DXX)
      Y2 = UD(IDX+1,IDY+1)*DXX + UD(IDX,IDY+1)*(1-DXX)
      UT = (X1*(1-DXX)+X2*DXX + Y1*(1-DYY)+Y2*DYY)/2.
      IF(IL.EQ.0.OR.IL.EQ.13) UT = Y1
      IF(IL.EQ.7.OR.IL.EQ.19) UT = X1
C
      X1 = VD(IDX  ,IDY+1)*DYY + VD(IDX  ,IDY)*(1-DYY)
      X2 = VD(IDX+1,IDY+1)*DYY + VD(IDX+1,IDY)*(1-DYY)
      Y1 = VD(IDX+1,IDY  )*DXX + VD(IDX,IDY  )*(1-DXX)
      Y2 = VD(IDX+1,IDY+1)*DXX + VD(IDX,IDY+1)*(1-DXX)
      VT = (X1*(1-DXX)+X2*DXX + Y1*(1-DYY)+Y2*DYY)/2.
      IF(IL.EQ.0.OR.IL.EQ.13) VT = Y1
      IF(IL.EQ.7.OR.IL.EQ.19) VT = X1
C.. TANGENTIAL WIND
      WT = -SIN(DD)*UT + COS(DD)*VT
      WTS = WTS+WT
20    CONTINUE
      WTM(JL) = WTS/24.
10    CONTINUE
C
C Southern Hemisphere
      IF(CLAT_NEW.LT.0)THEN
        DO JL=1,IR
          WTM(JL)=-WTM(JL)
        END DO
      END IF
C EnD SH

      TX = -10000000.
      DO KL = 1,IR
      IF(WTM(KL).GE.TX) THEN
      TX = WTM(KL)
      ENDIF
      ENDDO
C
      TNMX(I,J) = TX
      ENDDO
      ENDDO
C.. FIND NEW CENTER
      TTX = -1000000.
      DO I=1,ID
      DO J=1,JD
      IF(TNMX(I,J).GE.TTX) THEN
      TTX = TNMX(I,J)
      NIC = I
      NJC = J
      ENDIF
      ENDDO
      ENDDO
C
      CLAT_NEW = XLAT + (NJC-1)
      CLON_NEW = XLON + (NIC-1)
C
      print *,'NEW CENTER,  I, J IS   ',NIC,NJC
      print *,'NEW CENTER, LAT,LON IS ',CLAT_NEW,CLON_NEW
      print *,'MAX TAN. WIND AT NEW CENTER IS ',TTX
C
      RETURN
      END SUBROUTINE FIND_NEWCT
      SUBROUTINE TWIND(UD,VD,TW)
C
      PARAMETER (IX=41,JX=41,NF=11,IT=24,IR=120)
      DIMENSION UD(IX,JX),VD(IX,JX),TW(IT,IR),R0(IT)
      COMMON /POSIT/CLON_NEW,CLAT_NEW,SLON,SLAT,CLON,CLAT,RAD
      COMMON /vect/R0,XVECT(IT),YVECT(IT)
c      COMMON /CT/SLON,SLAT,CLON,CLAT,RAD
c      COMMON /GA/CLON_NEW,CLAT_NEW,R0
C
      DO J=1,IR
      DO I=1,IT
C.. DETERMINE LAT, LON AREOUND CIRCLE
      DR = 0.1*J
      DD = (I-1)*15.*RAD
      DLON = DR*COS(DD)
      DLAT = DR*SIN(DD)
      TLON = CLON_NEW + DLON
      TLAT = CLAT_NEW + DLAT
C.. INTERPOLATION U, V AT TLON,TLAT AND CLACULATE TANGENTIAL WIND
      IDX = IFIX(TLON) - SLON + 1
      IDY = IFIX(TLAT) - SLAT + 1
      DXX  = TLON - IFIX(TLON)
      DYY  = TLAT - IFIX(TLAT)
C
      X1 = UD(IDX  ,IDY+1)*DYY + UD(IDX  ,IDY)*(1-DYY)
      X2 = UD(IDX+1,IDY+1)*DYY + UD(IDX+1,IDY)*(1-DYY)
      Y1 = UD(IDX+1,IDY  )*DXX + UD(IDX,IDY  )*(1-DXX)
      Y2 = UD(IDX+1,IDY+1)*DXX + UD(IDX,IDY+1)*(1-DXX)
      UT = (X1*(1-DXX)+X2*DXX + Y1*(1-DYY)+Y2*DYY)/2.
      IF(I.EQ.0.OR.I.EQ.13) UT = Y1
      IF(I.EQ.7.OR.I.EQ.19) UT = X1
C
      X1 = VD(IDX  ,IDY+1)*DYY + VD(IDX  ,IDY)*(1-DYY)
      X2 = VD(IDX+1,IDY+1)*DYY + VD(IDX+1,IDY)*(1-DYY)
      Y1 = VD(IDX+1,IDY  )*DXX + VD(IDX,IDY  )*(1-DXX)
      Y2 = VD(IDX+1,IDY+1)*DXX + VD(IDX,IDY+1)*(1-DXX)
      VT = (X1*(1-DXX)+X2*DXX + Y1*(1-DYY)+Y2*DYY)/2.
      IF(I.EQ.0.OR.I.EQ.13) VT = Y1
      IF(I.EQ.7.OR.I.EQ.19) VT = X1
C.. TANGENTIAL WIND
      TW(I,J) = -SIN(DD)*UT + COS(DD)*VT
C
      ENDDO
      ENDDO
C SH
      IF(CLAT_NEW.LT.0)THEN
        DO J=1,IR
        DO I=1,IT
          TW(I,J)=-TW(I,J)
        ENDDO
        ENDDO
      END IF
C End SH
C
      RETURN
      END SUBROUTINE TWIND
      SUBROUTINE STRT_PT(RMX,TW,RFAVG)
C
      PARAMETER (IX=41,JX=41,NF=11,IT=24,IR=120)
      DIMENSION TW(IT,IR),TWM(IR),TMXX(IT),RMX(IT)
      REAL JMX
C
      DO I=1,IR
      TWM(I) = 0.
      ENDDO
C
C.. CALCULATE MEAN TANGENTIAL WIND
C
      DO 10 J=1,IR
      TM=0.
      DO 20 I=1,IT
      TM = TM + TW(I,J)
20    CONTINUE
      TWM(J) = TM/24.
c      print *,'MEAN TANGENTIAL WIND ',J,TWM(J)
10    CONTINUE
C
C.. FIND MAXIMUM TANGENTIAL WIND RADIUS
C
      TMX=-100000000000.
      DO J=1,IR
      IF(TWM(J).GE.TMX) THEN
      TMX=TWM(J)
      JMX = J*0.1
      ENDIF
      ENDDO
C
      print *,'MAXIMUM TANGENTIAL WIND RADIUS ',JMX
      JJ=IFIX(JMX*10.)
      print *,'MAXIMUM TANGENTIAL WIND SPEED  ',TWM(JJ)
C
      JXX = 15 * JMX
c      print *,'JXX, 15*JMX is ',JXX
C
      ICK = 1
      CNT = 0.000004
c      print *,'CNT  ',CNT
C
      DO 30 K=JXX,IR-1
      IF(TWM(K).GE.6..OR.TWM(K).LT.3.) GO TO 30
      DXX = 10000.
      DV = TWM(K) - TWM(K+1)
      DVDR = DV/DXX
      IF(DVDR.LT.CNT) ICK = ICK+1
      IF(ICK.EQ.3) THEN
      RF=K*0.1
      GO TO 40
      ENDIF
30    CONTINUE
C
40    CONTINUE
      IF(ICK.NE.3) THEN
      DO IK=JXX,120
      IF(TWM(IK).LE.3) THEN
      RF = IK*0.1
      ICK=3
      GO TO 50
      ENDIF
      ENDDO
      ENDIF
C
50    CONTINUE
      IF(ICK.NE.3) RF = 12.
C
      RFAVG = RF
c
C.. CALCULATE Ra, Rb..  REF. KURIHARA ET AL. 1995
C
      RA = IFIX((0.5 * JMX)*10.)/10.
      RB = IFIX((0.75 * JMX + 0.25 * RF)*10.)/10.
      IRA = IFIX(RA*10.+0.5)
      IRB = IFIX(RB*10.+0.5)
C
c      print *,'Ra, Rb, Rf  ', RA,RB,RF
C
C.. DETERMINE STARTING POINT FOR EVERY 24 DIRECTION
C
      DO I=1,IT
      TMXX(I) = -100000000.
      DO J=1,IR
      IF(TW(I,J).GE.TMXX(I)) THEN
      TMXX(I) = TW(I,J)
      RMX(I) = J*0.1*1.1
      ENDIF
      ENDDO
      ENDDO
C
c      DO I=1,IT
c      print *,'I, MX TANGENTIAL WIND RADIUS ',I,RMX(I),TMXX(I)
c      ENDDO
C
      DO I=1,IT
      IF (RMX(I).GT.RB.OR.RMX(I).LT.RA) THEN
      TMX = -10000000.
      DO KK=IRA,IRB
      IF(TW(I,KK).GE.TMX) RM = KK * 0.1 * 1.1
      ENDDO
      MR = IFIX(RM*10. + 0.5)
      ICL=0
      DO LL = MR,IRB
      IF(TW(I,LL).LT.0.) ICL=ICL+1
      ENDDO
      IF(ICL.EQ.0) RMX(I) = RM*1.1
      ENDIF
      ENDDO
C
c      DO I=1,IT
c      print *,'I, RST ',I,RMX(I)
c      ENDDO
C
      RETURN
      END SUBROUTINE STRT_PT
      SUBROUTINE FILTER(RS,TW,RF,RFAVG)
      PARAMETER (IX=41,JX=41,IT=24,IR=120)
C
      DIMENSION RS(IT),TW(IT,IR),RF(IT),R0(IT),IST(IT)
      COMMON /vect/R0,XVECT(IT),YVECT(IT)
c      COMMON /GA/CLON_NEW,CLAT_NEW,R0
C
      ICK = 1
      CNT = 0.000004
c      print *,'CNT  ',CNT
C
      DO I=1,IT
      IST(I) = IFIX(RS(I)*10)
c      print *,'STARTING POINT ',I,IST(I)
      ENDDO
C
      DO 100 I=1,IT
      IS = IST(I)
C
      DO 30 K=IS,IR-1
      IF(TW(I,K).GE.6..OR.TW(I,K).LT.3.) GO TO 30
      DXX = 10000.
      DV = TW(I,K) - TW(I,K+1)
      DVDR = DV/DXX
      IF(DVDR.LT.CNT) THEN
      ICK = ICK+1
      ENDIF
      IF(ICK.EQ.3) THEN
      RF(I)=K*0.1 + 0.0000001
c      print *,'1st Catagory ',I
      GO TO 100
      ENDIF
30    CONTINUE
C
40    CONTINUE
      DO IK=IS,IR
      IF(TW(I,IK).LE.3) THEN
      RF(I) = IK*0.1 + 0.00000001
c      print *,'2nd Catagory ',I
      GO TO 100
      ENDIF
      ENDDO
C
50    CONTINUE
c      print *,'3rd Catagory ',I
      RF(I) = 12.
100   CONTINUE
C
c      RMAX=0.
      DO I=1,IT
      R0(I) = 1.25 * RF(I)
!! NEW
      IF(R0(I).LT.2.0)R0(I)=2.0
c      IF(RMAX.LT.R0(I))RMAX=R0(I)
c      print *,'R0,Rf AT EACH DIRECTION ',I,R0(I),RF(I)
      ENDDO
C test for circular domain
c      DO I=1,IT
c         R0(I)=RMAX
cc        R0(I) = RFAVG*1.25
c      print *,'R0,Rf AT EACH DIRECTION ',I,R0(I),RF(I)
c      ENDDO
C
      RETURN
      END SUBROUTINE FILTER
      SUBROUTINE GMOVE(KST,KSTM,
     1		  KMAX,IGU,JGU,MTV,GLON,GLAT,HDATA,PSLB,ZDATG,
     1            IX,JX,ALON,ALAT,DM1,
     2		  IB,ING,JNG,MDX,MDY,AMDX,AMDY, IV,IS1,IFLAG)

      use m_die,only : die
      use m_vtxgrid, only: vtxgrid_lonModulo
! (index,count) of all storm grids
      integer,intent(in) :: KST,KSTM	! they should be removed later
      					! along with ZDATG/T1/PSLB and
					! SPL2SP().

! grid attributes of the background state
      integer,intent(in) :: KMAX,IGU,JGU,MTV
      real,dimension(IGU,JGU),intent(in) :: GLON,GLAT
      REAL,dimension(IGU,JGU,MTV),intent(inout) :: HDATA
      REAL,dimension(IGU,JGU),intent(inout) :: PSLB
      REAL,dimension(IGU,JGU),intent(in) :: ZDATG

      integer,intent(in) :: IX,JX
      real,intent(in) :: ALAT(JX),ALON(IX)
      real,intent(in) :: DM1(IX,JX)

! grid mapping information for a given storm grid
      integer,intent(in) :: IB
      integer,dimension(IB),intent(in) :: ING,JNG
      integer,intent(in) :: MDX,MDY
      real,intent(in) :: AMDX,AMDY

      	! Additional control flags
      integer,intent(in) :: IV,IS1,IFLAG

! Local variables
      DIMENSION DMM(IX,JX),DATG(IGU,JGU),DDAT(IGU,JGU)
      DIMENSION T1(IGU,JGU),PSL(IGU,JGU)

      COMMON /HDAT3/NWRT2,NRED2	! pointers of HDATA's put() and get().

      DIMENSION DATG2(IGU,JGU)

      real:: HLO,HLA
      integer,parameter:: RK=kind(HLO)

C
C.. SETTING BASIC VARIABLES FOR INTERPOLATING GAUSSIAN GRID
C
!!	print'(1x,a,8i6)','GMOVE(): ',
!!     &		KST,IV,IS1,IFLAG,NRED2+1,NWRT2+1

      ISE = IS1
      DO I=1,IX
      DO J=1,JX
      DMM(I,J) = DM1(I,J)
      ENDDO
      ENDDO
C
C.. INTERPOLATE TO GAUSSIAN GRID
C
      CALL READ2(IGU,JGU,NRED2,MTV,DATG,HDATA)
      	!!?? DATG(:,:)=HDATA(:,:,NRED2++)
c
      DO I=1,IGU
      DO J=1,JGU
        DATG2(I,J)=DATG(I,J)
        DDAT(I,J)=0.
      ENDDO
      ENDDO
C
      RDIST2=AMDX*AMDX+AMDY*AMDY
      IF(RDIST2.GT.0.02)THEN
cc test
      DO I = 1,IB
      IW = ING(I)
      JW = JNG(I)

c      DO IW = 1, IGU
c      DO JW = 1, JGU
      HLA = GLAT(IW,JW)

        ! Bi-linearly interpolate a field DMM defined on grid (ALON,ALAT),
        ! to scattered locations (HLO,HLA) of DATG on grid (GLON,GLAT).

        ! First, if HLO is out of the range of ALON(1:IX), adjust it to one
        ! of its periodic (n*360, for n=...,-3,-2,-1,0,1,2,3,...) values,
        ! such that it falls inside the range [ALON(1), ALON(1)+360.).

      HLO = GLON(IW,JW)
        ! This is, HLO=ALON(1)+modulo(HLO-ALON(1),360.).  Condition
        ! HLO>ALON(IX) is used instead of HLO>=ALON(1)+360., because there
        ! is a possibility of HLO, being out of the range [ALON(1),ALON(IX)].
      if(HLO<ALON(1).or.HLO>ALON(IX)) HLO=vtxgrid_lonModulo(HLO,reflon=ALON(1))

C
      LX=-2
      LY=-2
      DO II=1,IX-1
       IF( (II==1.and.HLO==ALON(II)) .or.
     &     (HLO.GT.ALON(II).and.HLO.LE.ALON(II+1)) )THEN
!       IF( (HLO.GT.ALON(II).and.HLO.LE.ALON(II+1)) )THEN
        LX=II
        LY=-1
        DO JJ=1,JX-1
        IF( (JJ==1.and.HLA==ALAT(JJ)) .or.
     &      (HLA.GT.ALAT(JJ).and.HLA.LE.ALAT(JJ+1)) )THEN
!        IF( (HLA.GT.ALAT(JJ).and.HLA.LE.ALAT(JJ+1)) )THEN
         LX=II
         LY=JJ

         DXX = HLO-ALON(LX)
         DYY = HLA-ALAT(LY)
C
         X1 = DMM(LX  ,LY+1)*DYY + DMM(LX  ,LY  )*(1-DYY)
         X2 = DMM(LX+1,LY+1)*DYY + DMM(LX+1,LY  )*(1-DYY)
         Y1 = DMM(LX+1,LY  )*DXX + DMM(LX  ,LY  )*(1-DXX)
         Y2 = DMM(LX+1,LY+1)*DXX + DMM(LX  ,LY+1)*(1-DXX)
         DATG(IW,JW)=(X1*(1-DXX)+X2*DXX + Y1*(1-DYY)+Y2*DYY)/2.

         IF(ISE.GE.2) DDAT(IW,JW)=DATG2(IW,JW)-DATG(IW,JW)
         GO TO 555

        END IF
        END DO
       END IF
      END DO
      if(LX<0.or.LY<0) then
      	write(6,'(a,2i4  )') 'GMOVE() >>> ERROR <<< unlocated LHS (H) grid point, IW,JW =',IW,JW
      	write(6,'(a,2i4  )') 'GMOVE() >>> ERROR <<<  indexed RHS (A) location at, LX,LY =',LX,LY
      	write(6,'(a,3f8.2)') 'GMOVE() >>> ERROR <<<              HLO, ALON(1), ALON(IX) =',HLO,ALON(1),ALON(IX)
      	write(6,'(a,3f8.2)') 'GMOVE() >>> ERROR <<<              HLA, ALAT(1), ALAT(JX) =',HLA,ALAT(1),ALAT(JX)
        call die("GMOVE()")
      endif
 555   CONTINUE
c      ENDDO
c      ENDDO
      ENDDO
      END IF
c end test

      IF(ISE.EQ.1) THEN
c
c        READ(70) PSL
        PSL=PSLB

        DO I = 1,IB
          IW = ING(I)
          JW = JNG(I)
          DDAT(IW,JW)=PSL(IW,JW)-DATG(IW,JW)
          PSL(IW,JW)=DATG(IW,JW)
        END DO
c
c Move vortex

cc        DO I = 1,IB
cc          IW = ING(I)
cc          JW = JNG(I)
cc          IWX=IW+MDX
cc          JWY=JW+MDY
cc          IF(IWX.GT.IGU)IWX=IWX-IGU
cc          IF(IWX.LT.1)IWX=IWX+IGU
CQLIUC
cc          PSL(IWX,JWY) = PSL(IWX,JWY)+DDAT(IW,JW)
cc        ENDDO

        CALL MOVETX(IGU,JGU,GLON,GLAT,PSL,DDAT,
     &    IB,ING,JNG,MDX,MDY,AMDX,AMDY)

        PSLB = PSL

!!!        CALL WRIT2(IGU,JGU,NWRT2,MTV,PSL,HDATA)
	NWRT2=NWRT2+1
c
      ELSEIF(ISE.EQ.2) THEN
cyc   REWIND 36
cyc   READ(36) PSL
       PSL = PSLB
       IF(IFLAG.EQ.1)THEN
        DO I=1,IGU
        DO J=1,JGU
         T1(I,J) = DATG2(I,J)
        ENDDO
        ENDDO
       ELSE
        DO I=1,IGU
        DO J=1,JGU
         T1(I,J) = DATG(I,J)
        ENDDO
        ENDDO
       END IF
        IF(KST.EQ.KSTM)THEN
          CALL SLP2SP(IGU,JGU,ZDATG,KUNIT,T1,PSL)
!!	  print*,'PSL(GMOVE/0) = ',exp(minval(PSL)),
!!     &				   exp(maxval(PSL)),NWRT2
	  NWRT2=NWRT2-1		! backup 1 record for PSL output
          CALL WRIT2(IGU,JGU,NWRT2,MTV,PSL,HDATA)
!!	  print*,'PSL(GMOVE/1) = ',exp(minval(HDATA(:,:,2))),
!!     &				   exp(maxval(HDATA(:,:,2)))
        END IF
      END IF

c temperature field
c qliu

      IF(ISE.GE.2.and.ISE.LE.(KMAX+1))then
        IF(IFLAG.EQ.1)THEN
cold          IF(KST.EQ.KSTM) THEN
cql            READ(20)SKIP2
cold            NCNT2 = NCNT2 + 1
cold            WRITE(KUNIT)(SKIP2(NW,NCNT2),NW=1,MAXWV2)
cold          END IF
          CALL WRIT2(IGU,JGU,NWRT2,MTV,DATG2,HDATA)
        ELSE

c Move vortex
cc          DO I = 1,IB
cc            IW = ING(I)
cc            JW = JNG(I)
cc            IWX=IW+MDX
cc            JWY=JW+MDY
cc            IF(IWX.GT.IGU)IWX=IWX-IGU
cc            IF(IWX.LT.1)IWX=IWX+IGU
CQLIUC
cc            DATG(IWX,JWY) = DATG(IWX,JWY)+DDAT(IW,JW)
cc          ENDDO

          CALL MOVETX(IGU,JGU,GLON,GLAT,DATG,DDAT,
     &      IB,ING,JNG,MDX,MDY,AMDX,AMDY)

cnew          IF(KST.EQ.KSTM) THEN
cql            READ(20)SKIP2
cnew            NCNT2 = NCNT2 + 1
cnew            CALL G2SPC(DATG)
cnew          END IF

          CALL WRIT2(IGU,JGU,NWRT2,MTV,DATG,HDATA)

        END IF
      END IF
C
      IF(ISE.GT.(KMAX+1).and.ISE.LE.(3*KMAX+1))THEN
c Move vortex

cc        DO I = 1,IB
cc          IW = ING(I)
cc          JW = JNG(I)
cc          IWX=IW+MDX
cc          JWY=JW+MDY
cc          IF(IWX.GT.IGU)IWX=IWX-IGU
cc          IF(IWX.LT.1)IWX=IWX+IGU
CQLIUC
cc          DATG(IWX,JWY) = DATG(IWX,JWY)+DDAT(IW,JW)
cc        ENDDO

         CALL MOVETX(IGU,JGU,GLON,GLAT,DATG,DDAT,
     &     IB,ING,JNG,MDX,MDY,AMDX,AMDY)
C
cnew        IF(KST.EQ.KSTM) THEN
cnew          CALL G2SPC(DATG)
cnew        END IF

        CALL WRIT2(IGU,JGU,NWRT2,MTV,DATG,HDATA)

      ENDIF

      IF(ISE.GT.(3*KMAX+1))THEN
        IF(IFLAG.EQ.1)THEN
cold          IF(KST.EQ.KSTM) THEN
cold            CALL G2SPC(KUNIT,MWAVE,IGU,JGU,DATG2)
cold          END IF
          CALL WRIT2(IGU,JGU,NWRT2,MTV,DATG2,HDATA)
        ELSE

c Move vortex
cc          DO I = 1,IB
cc            IW = ING(I)
cc            JW = JNG(I)
cc            IWX=IW+MDX
cc            JWY=JW+MDY
cc            IF(IWX.GT.IGU)IWX=IWX-IGU
cc            IF(IWX.LT.1)IWX=IWX+IGU
CQLIUC
cc            DATG(IWX,JWY) = DATG(IWX,JWY)+DDAT(IW,JW)
cc          ENDDO

           CALL MOVETX(IGU,JGU,GLON,GLAT,DATG,DDAT,
     &       IB,ING,JNG,MDX,MDY,AMDX,AMDY)

cnew          IF(KST.EQ.KSTM) THEN
cnew            CALL G2SPC(DATG)
cnew          END IF

          CALL WRIT2(IGU,JGU,NWRT2,MTV,DATG,HDATA)

        END IF
      ENDIF
!!	  print*,'PSL(GMOVE/2) = ',exp(minval(HDATA(:,:,2))),
!!     &				   exp(maxval(HDATA(:,:,2))),NWRT2,ISE

C
      RETURN
      END SUBROUTINE GMOVE
      SUBROUTINE SLP2SP(IGU,JGU,ZDATG,KUNIT,T1,PSL)
c      PARAMETER (IGU=384,JGU=190)
C
      DIMENSION T1(IGU,JGU),PSL(IGU,JGU)
      DIMENSION ZDATG(IGU,JGU)
c      COMMON /TR/ZDATG,GLON,GLAT,ING,JNG,IB
C
C.. MAKE SFC PRESSURE FROM MSLP
C
      G = 9.8
      R = 287.05
      GAMMA = 6.7*0.001
C
      DO JH=1,JGU
      DO IH=1,IGU
      PMSL = ALOG(PSL(IH,JH))
      A = (GAMMA * ZDATG(IH,JH)) / T1(IH,JH)
      B = ALOG(1+A)
      C = (G*B)/(R*GAMMA)
      DD = PMSL - C
      D1 = EXP(DD)/1000.
c      IF (D1.LE.10.) PRINT*,'SP is Less than 100mb at ',IH,JH,D1
      PSL(IH,JH) = ALOG(D1)
      ENDDO
      ENDDO
C
C.. GAUSSIAN GRID TO SPECTRAL COEFFEICENT
C
!!      call maxmin(psl,igu*jgu,1,1,1,'global SLP at SLP after int')
!!      CALL G2SPC(KUNIT,MWAVE,IGU,JGU,PSL)
!!	print*,'PSL(SLP2SP) = ',exp(minval(PSL)),exp(maxval(PSL))

!!      call maxmin(t1,igu*jgu,1,1,1,'global T1 at SLP after int')
c      CALL G2SPC(KUNIT,T1)
C
      RETURN
      END SUBROUTINE SLP2SP
      SUBROUTINE G2SPC(KUNIT,MWAVE,IMAX,JMAX,Q1)
C
c      PARAMETER ( IMAX= 384,JMAX= 190 )
C
      REAL Q1(IMAX,JMAX)

      REAL,   ALLOCATABLE :: DN(:)
      REAL(4),ALLOCATABLE :: WORK_3(:)

      MAXWV2=(MWAVE+1)*(MWAVE+2)
      MAXWV22=MAXWV2+1

      ALLOCATE ( DN(MAXWV22) )
      ALLOCATE ( WORK_3(MAXWV2) )
C
c      call maxmin(dn,MAXWV2,1,1,1,'surface pressure after making')

!!      print*,'specgrid(g2spc), grid_psfc=',sum(Q1)
       call SPTEZ(0,MWAVE,4,IMAX,JMAX,DN,Q1,-1)

      DO I=1,MAXWV2
        WORK_3(I)=DN(I)
      END DO
      WRITE(KUNIT) (WORK_3(NW),NW=1,MAXWV2)
!!      print*,'specgrid(g2spc), spec_psfc=',sum(work_3)

      DEALLOCATE (DN)
      DEALLOCATE (WORK_3)

      RETURN
      END SUBROUTINE G2SPC
