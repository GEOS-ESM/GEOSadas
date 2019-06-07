!#define DEBUG_TRACE
#include "mytrace.H"
#define _SIZE_OF_ING_JNG_ 25600
      SUBROUTINE HURR_REL(ITIM,IMAX,JMAX,KMAX,MTV,
     &  GLON,GLAT,aki,SI,akl,SL,PSLB,HDATA,
     &  IX,JX,MTV2,NSTM,HDAT,MTV3)

      use m_vtxgrid, only: vtxgrid_nmiss
      use m_vtxgrid, only: vtxgrid_lonDiff
      use m_vtxgrid, only: vtxgrid_lonRange
      use m_hurrmess, only: NST, KSTM, ST_NAME, TCVT
      use m_hurrmess, only: SLON_N,SLAT_N,CLON_N,CLAT_N
      use m_hurrmess, only: SLON_N,SLAT_N,CLON_N,CLAT_N
      use m_hurrmess, only: hurrmess_summary
      use m_hurrmess, only: stat_tcvin, clon_tcvin, clat_tcvin
      use m_hurrmess, only: STRM_Normal, STRM_Excluded, STRM_NoTrack, STRM_Bogus
      use m_hurrmess, only: STRM_ZGT500m,STRM_TooClose, STRM_TooWide
      use m_die, only: die
      implicit none
      integer,intent(in) :: ITIM
      integer,intent(in) :: IMAX,JMAX,KMAX,MTV
      real,dimension(IMAX,JMAX),intent(in) :: GLON,GLAT
      real,dimension(KMAX+1),intent(in) :: aki,SI
      real,dimension(KMAX  ),intent(in) :: akl,SL
      REAL,dimension(IMAX,JMAX),intent(in) :: PSLB
      REAL,dimension(IMAX,JMAX,MTV),intent(in) :: HDATA

      integer,intent(in) :: IX,JX,MTV2,NSTM
      REAL,dimension(IX,JX,MTV2,NSTM),intent(in) :: HDAT
      integer,intent(in) :: MTV3

! workspaces

C
C SEPARATE HURRICANE VORTEX FROM ENVIRONMENTAL FIELD, THEN
C RELOCATE THE HURRICANCE VORTEX TO NEW LOCATION
C      REF, Kurihara et al 1992, 1995. MWR
C
      integer,PARAMETER :: IT=24
      integer,PARAMETER :: IR=120
c      PARAMETER (IMAX=384,JMAX=190,NSG=2000)
c      PARAMETER (JHF=JMAX/2,NST=10)
C
      real :: U(IX,JX),V(IX,JX),UD(IX,JX),US(IX,JX),VS(IX,JX)
      real :: SKIP(IX,JX)
      real :: TW(IT,IR)
      real :: VD(IX,JX)
      real :: DKY(IX,JX)
      real :: YTU(IX,JX),YTV(IX,JX),RS(IT),RF(IT)
      real :: ALAT(JX),ALON(IX),ZG(IX,JX),DATG(IMAX,JMAX)
      real :: ZDATG(IMAX,JMAX)

      	! storm center parameters
      integer,parameter :: IRX=41,JRX=41


!!      integer :: KSTM,IC_N,JC_N
!!      COMMON /NHC/ KSTM,IC_N(NST),JC_N(NST)
!!      real :: SLON_N,SLAT_N,CLON_N,CLAT_N
!!      COMMON /NHC1/SLON_N(NST),SLAT_N(NST),CLON_N(NST),CLAT_N(NST)

      	! Following variables are defined at a given KST.
      integer :: MDX,MDY
      COMMON /NHC2/MDX,MDY
      real :: AMDX,AMDY
      COMMON /NHC3/AMDX,AMDY

      integer,PARAMETER :: NSG=_SIZE_OF_ING_JNG_
      integer :: ING(NSG),JNG(NSG),IB
      COMMON /TR/ING,JNG,IB

      	! intent(out) :: clon_new,clat_new
	! intent(out) :: slon,slat,clon,clat ! to find_newct()
	! intent(out) :: rad ! to twind()

      real :: CLON_NEW,CLAT_NEW,SLON,SLAT,CLON,CLAT,RAD
      COMMON /POSIT/CLON_NEW,CLAT_NEW,SLON,SLAT,CLON,CLAT,RAD
      real :: R0(IT),XVECT(IT),YVECT(IT)
      COMMON /vect/R0,XVECT,YVECT

      integer :: NWRT1,NRED1,NWT1
      COMMON /HDAT1/NWRT1,NRED1,NWT1
      integer :: NWRT2,NRED2
      COMMON /HDAT3/NWRT2,NRED2

      REAL PDAT(IX,JX,MTV3)
      REAL HSIG(IX,JX,KMAX),HP(IX,JX,2*KMAX+1)

      INTEGER ISTMCX1(3,NST),ISTMCY1(3,NST)
      REAL    STMCX(NST),STMCY(NST)
      CHARACTER STMNAME(NST)*3

      	! local variables ...
      real :: DKM,ENV,ENV1
      DIMENSION DKM(IX,JX)
      DIMENSION ENV(IX,JX,MTV2),ENV1(IX,JX,MTV3)
C
      integer,PARAMETER :: NF=11
      real :: M(NF),FK(NF)
      real :: XTU(IX,NF),XTV(IX,NF)
      DATA M/2,3,4,2,5,6,7,2,8,9,2/
C
      real :: PSC_MX,PSC_MX1
      integer LO,I,J,INDX1,K1STM,N,K,K850,K8501,IFLAG
      integer :: KST
      real :: PI,DLN
      real :: CLON_TIM,CLAT_TIM
      real :: CLON_NHC,CLAT_NHC
      real :: DIST1,DIST2,RDIST2
      integer :: ICHEK,IFWRT,MNHC,MNEW
      real :: fact,RFAVG

      real :: A,B,R,TH
      integer :: IC,M2,lsflag,KMP,KDIV1,KQ1,IG,IV,IREM,KDIV2,KQ2
      real :: rpsmean
      integer :: kbot,ktop,kinc

      real:: alon_min,alon_max
      real:: alat_min,alat_max

      real:: glon_min,glon_max
      real:: glat_min,glat_max
      integer:: nmiss

      character(len=*),parameter :: myname_='HURR_REL'
      _ENTRY_

      DO LO = 1,NSG
      ING(LO) = 0
      JNG(LO) = 0
      ENDDO

      PI=ASIN(1.)*2
      RAD=PI/180.
      DLN = 360.0/FLOAT(IMAX)

      DO I=1,NF
      FK(I)=0.5/(1-COS(2.*PI/M(I)))
      ENDDO
C

      WRITE(*,244) KSTM
 244  FORMAT('NUMBER OF STORMS: ',I5)

!----------------------------------------------------------------------------------------------
!                       -- load all-track-data again (file="**.trak.GDA.**") --
      INDX1=ITIM/3      ! time-now index

      K1STM=0
      DO I=1,NST
        STMCX(I)=0.
        STMCY(I)=0.
        STMNAME(I)='NUL'
        READ(30,442,end=436)
     &     (ISTMCY1(J,I),ISTMCX1(J,I),J=1,3),STMNAME(I)
        STMCX(I)=360.-ISTMCX1(INDX1,I)*0.1              ! tracked center at time-now (=INDX1)
        STMCY(I)=ISTMCY1(INDX1,I)*0.1                   ! tracked center at time-now (=INDX1)
        K1STM=K1STM+1
c        PRINT*,'QLIU test=',STMNAME(I),STMCX(I),STMCY(I)
      END DO
 442  FORMAT(22x,6i4,25x,A3)
 436  CONTINUE          ! the exit point of the do-loop above

!----------------------------------------------------------------------------------------------
!                       -- THE major loop of this routine, for all storms --
      DO 788 KST=1,KSTM
!!      	print*,'HURR_REL: loop 788, KST=',KST,'/',KSTM
        if( stat_tcvin(KST) == STRM_Excluded .or.
     &      stat_tcvin(KST) == STRM_NoTrack
     &    ) then
          cycle
        endif

        DO K=1,MTV2
        DO J=1,JX
        DO I=1,IX
          ENV(I,J,K)=HDAT(I,J,K,KST)
        END DO
        END DO
        END DO

        PSC_MX=0.
        DO J=1,JX
        DO I=1,IX
          IF(PSC_MX.LT.ENV(I,J,2))PSC_MX=ENV(I,J,2)
        END DO
        END DO
        PSC_MX1=EXP(PSC_MX)*1000.+500.0
        PRINT*,'MAX SFC PRESS=',PSC_MX1

        !-------------------------------------------------------------------------------------
        ! see file sig_p_convt1.f,
        CALL SIG2P(IX,JX,KMAX,MTV2,MTV3,ENV(1,1,1),PDAT(1,1,1),
     &             PSC_MX1,HSIG,HP,KST,aki,SI,akl,SL)

      NWRT1 = 0
      NWRT2 = 0
      NRED1 = 0
      NRED2 = 0

        ! NRED2_ZDATG = NRED2+1 == 1
        ! ZDATG(:,:) = HDATA(:,:,NRED2_ZDATG)
      CALL READ2(IMAX,JMAX,NRED2,MTV,ZDATG,HDATA)

c     WRIT2(NWRT2,MTV,ZDATG,HDATA)
      NWRT2 = 1

        ! NRED1_ZG = NRED1+1 == 1
        ! ZG(:,:) = PDAT(:,:,NRED1_ZG)
      CALL READ1(KST,NRED1,MTV3,ZG,PDAT)

      call maxmin(zg,ix*jx,1,1,1,'regional terrain')
      IFLAG=0
cnew      DO J=1,JX
cnew      DO I=1,IX
cnew        IF(ZG(I,J).GT.200.)THEN
cnew          IFLAG=1
c          PRINT*,'Max Terrain Height > 200 m'
cnew          GO TO 443
cnew        END IF
cnew      END DO
cnew      END DO
cnew 443  CONTINUE
C

!---------------------------------------------------------------------------------------------
!               -- locate level of 850mb, and get (u,v)_850
C.. READ U, V at ~850 mb

      	! HDATA(:,:,2): log(psfc) in log_kPa
      rpsmean=sum(1./exp(HDATA(:,:,2)))/(1000.*imax*jmax)

      kbot=1
      ktop=kmax
      kinc=1
      if(akl(kbot)+SL(kbot)*1000. < akl(ktop)+SL(ktop)*1000.) then
        kbot=kmax
	ktop=1
	kinc=-1
      endif
!      K850=3+KMAX+(KMAX/4)*4+1
      K8501=1
      !! DIST2=ABS(SL(kbot)-0.85)
      DIST2=ABS(AKL(kbot)*rpsmean+SL(kbot)-0.85)
      DO K=kbot,ktop,kinc
        !! DIST1=ABS(SL(K)-0.85)
        DIST1=ABS(AKL(K)*rpsmean+SL(K)-0.85)
        IF(DIST1.LT.DIST2)THEN
          K8501=K
          DIST2=DIST1
        END IF
      END DO

! Be consistent with 2001 operational model for KMAX=42
! set K8501=K8501+1
      IF(KMAX.EQ.42) K8501=K8501+kinc

      K850=3+KMAX+4*(K8501-kinc)+1

      IF(K8501.LT.1.OR.K8501.GT.KMAX)THEN
        PRINT*,'K8501 is out of bound'
        call die('HURR_REL','STOP at "K8501 is out of bound", K8501 =',K8501)
        STOP
      END IF

      PRINT*,'QLIUQLIU test',K850

      NRED1 = NRED1 + K850
      DO J=1,JX
      DO I=1,IX
        U(I,J)=HDAT(I,J,K850+2,KST)
        V(I,J)=HDAT(I,J,K850+3,KST)
      END DO
      END DO

C
!---------------------------------------------------------------------------------------------
c qliu
c get Hurricane Center
c      READ(12,123)SLON,SLAT,CLON_NHC,CLAT_NHC
c123   FORMAT(1X,4F10.2)
      SLON = SLON_N(KST)
      SLAT = SLAT_N(KST)
      CLON_NHC = CLON_N(KST)
      CLAT_NHC = CLAT_N(KST)

      CLON = SLON+20.
      CLAT = SLAT+20.
c      PRINT*,'CLON, CLAT, SLON, SLAT=',CLON, CLAT, SLON, SLAT
c      PRINT*,'CLON_NHC,CLAT_NHC=',CLON_NHC,CLAT_NHC
c      fact=cos(CLAT*rad)
      fact=1.0
      do j=1,jx
      do i=1,ix
! East-West wind in new coordinate (phi,theta)
! this conversion only affects Hurrican Center determination and R0
        U(I,J)=U(I,J)/fact
      end do
      end do
C.. DO ZONAL FILTER
C
      DO 100 J=1,JX
      DO N=1,NF
      XTU(1,N)  = U(1,J)
      XTU(IX,N) = U(IX,J)
      XTV(1,N)  = V(1,J)
      XTV(IX,N) = V(IX,J)
      ENDDO
C
      DO I=2,IX-1
      XTU(I,1) = U(I,J)+FK(1)*(U(I-1,J)+U(I+1,J)-2.*U(I,J))
      XTV(I,1) = V(I,J)+FK(1)*(V(I-1,J)+V(I+1,J)-2.*V(I,J))
      ENDDO
C
      DO N=2,NF
      DO I=2,IX-1
      XTU(I,N)=XTU(I,N-1)+FK(N)*(XTU(I-1,N-1)+XTU(I+1,N-1)-2.
     1         *XTU(I,N-1))
      XTV(I,N)=XTV(I,N-1)+FK(N)*(XTV(I-1,N-1)+XTV(I+1,N-1)-2.
     1         *XTV(I,N-1))
      ENDDO
      ENDDO
C
      DO I=1,IX
      US(I,J) = XTU(I,NF)
      VS(I,J) = XTV(I,NF)
      ENDDO
C
100   CONTINUE
C
C.. DO MERIDIONAL FILTER
C
      DO 200 I=1,IX
C
      DO N=1,NF
      YTU(1,N)  = US(I,1)
      YTU(JX,N) = US(I,JX)
      YTV(1,N)  = VS(I,1)
      YTV(JX,N) = VS(I,JX)
      ENDDO
C
      DO J = 2 , JX-1
      YTU(J,1) = US(I,J) + FK(1)*(US(I,J-1) + US(I,J+1)
     *                          -2.*US(I,J))
      YTV(J,1) = VS(I,J) + FK(1)*(VS(I,J-1) + VS(I,J+1)
     *                          -2.*VS(I,J))
      ENDDO
CC
      DO N = 2 , NF
      DO J = 2 , JX-1
      YTU(J,N) = YTU(J,N-1) + FK(N)*(YTU(J-1,N-1)  +
     *              YTU(J+1,N-1) - 2.*YTU(J,N-1))
      YTV(J,N) = YTV(J,N-1) + FK(N)*(YTV(J-1,N-1)  +
     *              YTV(J+1,N-1) - 2.*YTV(J,N-1))
      ENDDO
      ENDDO
C
      DO J = 1 , JX
      US(I,J)   =  YTU(J,NF)
      VS(I,J)   =  YTV(J,NF)
      ENDDO
200   CONTINUE
C
C.. GET THE DISTURBANCE FIELD
C
      DO I=1,IX
      DO J=1,JX
      UD(I,J) = U(I,J) - US(I,J)
      VD(I,J) = V(I,J) - VS(I,J)
      ENDDO
      ENDDO
c      WRITE(39) ((U(I,J),I=1,IX),J=Jx,1,-1)
c      WRITE(39) ((V(I,J),I=1,IX),J=Jx,1,-1)
c      WRITE(39) ((US(I,J),I=1,IX),J=Jx,1,-1)
c      WRITE(39) ((VS(I,J),I=1,IX),J=Jx,1,-1)
c      WRITE(39) ((UD(I,J),I=1,IX),J=Jx,1,-1)
c      WRITE(39) ((VD(I,J),I=1,IX),J=Jx,1,-1)
C
C.. FIND NEW VORTEX CENTER
C
      DO I=1,IX
        ALON(I)=SLON+(I-1)
      END DO
      DO J=1,JX
        ALAT(J)=SLAT+(J-1)
      END DO

c      CALL FIND_NEWCT1(UD,VD)
      CALL FIND_NEWCT(UD,VD)

      ICHEK=0
      CLON_TIM=0.
      CLAT_TIM=0.
      DO I=1,K1STM
        IF(STMNAME(I).EQ.ST_NAME(KST))THEN
          CLON_TIM=STMCX(I)
          CLAT_TIM=STMCY(I)
          ICHEK=1
          GO TO 446
        END IF
      END DO
 446  CONTINUE

      if(ICHEK/=1) then ! this should not happen
        write(6,'(2a,i2)') myname_,'(): >>> ERROR <<< no track found, ICHEK =',ICHEK
        write(6,'(2a,i2)') myname_,'(): >>> ERROR <<<                   KST =',KST
        write(6,'(2a,2a)') myname_,'(): >>> ERROR <<<             TCVT(KST) = "',TCVT(KST)(1:43),'"'
        call die(myname_)
      endif

      IF((ICHEK.EQ.1).AND.(ABS(CLON_TIM).LT.359.5))THEN
        CLON_NEW=CLON_TIM
        CLAT_NEW=CLAT_TIM
      ELSE
        PRINT*,'GFDL CENTER= ',ITIM,'h ',CLON_NEW,CLAT_NEW
        CLON_NEW=CLON_NHC
        CLAT_NEW=CLAT_NHC
      ENDIF

C
C.. CALCULATE TANGENTIAL WIND AROUND CIRCLE
C             24 DIRECTION, RADIALLY 0.1DEG INTERVAL
C
      CALL TWIND(UD,VD,TW)
C
C.. CALCULATE STARTING POINT AT EACH DIRECTION
C
      CALL STRT_PT(RS,TW,RFAVG)
C
C.. DETERMINE FILTER DOMAIN D0 (=1.25*Rf)
C
      CALL FILTER(RS,TW,RF,RFAVG)

      AMDX=CLON_NHC-CLON_NEW
      AMDY=CLAT_NHC-CLAT_NEW
      MDX=IFIX((CLON_NHC-CLON_NEW)/DLN)

      IF(ITIM.EQ.6)THEN
        WRITE(52,65)TCVT(KST)(1:32),
     &             CLON_NHC,CLAT_NHC,CLON_NEW,
     &             CLAT_NEW,CLON_TIM,CLAT_TIM,AMDX,AMDY,
     &             SQRT(AMDX*AMDX+AMDY*AMDY)
 65   FORMAT(/'STORM NAME: ',A32,
     &       /'  OBSERVED CENTER POSITION:     ',2F10.2,
     &       /'  MODEL CENTER POSITION :       ',2F10.2,
     &       /'  MODEL CENTER POSITION (TIM):  ',2F10.2,
     &       /'  RELOCATION DISTANCE (DEGREE): ',3F10.2)
        ICHEK=0
        DO I=1,K1STM
          IF(STMNAME(I).EQ.ST_NAME(KST))THEN
            ICHEK=1
            IFWRT=0
            DO J=1,3
              IF(ISTMCY1(J,I).EQ.0.and.ISTMCX1(J,I).EQ.0)THEN
                IFWRT=1
              END IF
            END DO
            IF(IFWRT.EQ.1)THEN
              if(stat_tcvin(KST)==STRM_Normal) stat_tcvin(KST)=STRM_Bogus
              WRITE(52,76)TCVT(KST)(1:32)
            ELSE
              WRITE(52,77)TCVT(KST)(1:32)
            END IF
 76         FORMAT(/'  STORM: ',A32,10x,' is bogused')
 77         FORMAT(/'  STORM: ',A32,10x)
            WRITE(52,79)
     &      (ISTMCY1(J,I),ISTMCX1(J,I),J=1,3),STMNAME(I)
 79         FORMAT(/'  TRACKER OUTPUT: ',6i4,5x,A3)
          END IF
        END DO

        if(ICHEK/=1) then ! this should not happen
          write(6,'(2a,i2)') myname_,'(): >>> ERROR <<< no track found, ICHEK =',ICHEK
          write(6,'(2a,i2)') myname_,'(): >>> ERROR <<<                   KST =',KST
          write(6,'(2a,2a)') myname_,'(): >>> ERROR <<<             TCVT(KST) = "',TCVT(KST)(1:43),'"'
          call die(myname_)
        endif

      END IF

c test by qliu
c      MDX=MDX+50
c      AMDX=AMDX+50*DLN

      DO J=1,JMAX-1
        IF(CLAT_NHC.LT.GLAT(1,J).and.
     &         CLAT_NHC.GE.GLAT(1,J+1))THEN
          MNHC=J
          IF(CLAT_NHC.LT.0.5*(GLAT(1,J)+GLAT(1,J+1)))MNHC=J+1
          GO TO 566
        END IF
      END DO
 566  CONTINUE
      DO J=1,JMAX-1
        IF(CLAT_NEW.LT.GLAT(1,J).and.
     &         CLAT_NEW.GE.GLAT(1,J+1))THEN
          MNEW=J
          IF(CLAT_NEW.LT.0.5*(GLAT(1,J)+GLAT(1,J+1)))MNEW=J+1
          GO TO 577
        END IF
      END DO
 577  CONTINUE
      MDY=MNHC-MNEW
!!      PRINT*,'MDX,MDY,MNHC,MNEW=',MDX,MDY,MNHC,MNEW
!!      PRINT*,'AMDX,AMDY=',AMDX,AMDY
!!      PRINT*,'CLON_NHC,CLAT_NHC=',CLON_NHC,CLAT_NHC
!!      PRINT*,'CLON_NEW,CLAT_NEW=',CLON_NEW,CLAT_NEW
      RDIST2=AMDX*AMDX+AMDY*AMDY
      IF(RDIST2.LE.0.02)THEN
        PRINT*,'   '
        PRINT*,' FORECAST TIME= ',ITIM,'h ',
     1         ', STORM NAME= ',ST_NAME(KST)
        PRINT*,' CENTER DISTANCE is less than 15 km',
     1  ', storm is not relocated'
        AMDX=0.
        AMDY=0.
        MDX=0
        MDY=0
        if(stat_tcvin(KST)==STRM_Normal) stat_tcvin(KST) = STRM_TooClose
      ELSE
        PRINT*,'    '
        PRINT*,' FORECAST TIME= ',ITIM,'h',
     1         ' STORM NAME= ',ST_NAME(KST)
        PRINT*,' Center Distance = ',SQRT(RDIST2),' Deg.',
     3  ', relocation starts ...... '

      END IF
c
      IB=0
C
      DO J=1,JMAX
      DO I=1,IMAX
C
!     A = GLON(I,J) - CLON_NEW
      A = vtxgrid_lonDiff(CLON_NEW, GLON(I,J))
      B = GLAT(I,J) - CLAT_NEW
      R = SQRT(A**2. + B**2.)
      IF(R.EQ.0.) GO TO 444
      TH = ACOS(A/R) / RAD
      IF(B.LT.0.) TH = 360-TH
C
      IF(TH.LE.7.5 .OR. TH.GT.352.5 ) IC = 1
      DO M2=2,24
        IF((TH.GT.(15.*(M2-1)-7.5)).and.
     &     (TH.LE.(15.*M2-7.5)))IC=M2
      END DO
C
      IF(R.LT.R0(IC)) THEN
      IB = IB+1
      if(IB <= NSG) then
      ING(IB) = I
      JNG(IB) = J
      endif
      ENDIF
C
      GO TO 22
C
444   continue
      IB = IB+1
      TH = 0.
      if(IB <= NSG) then
      ING(IB) = I
      JNG(IB) = J
      endif
22    CONTINUE
C
      ENDDO
      ENDDO

      if(IB > NSG) then
        write(*,'(2a,i6,a,i6)') 'HURR_REL(): >>> ERROR <<< NSG=_SIZE_OF_ING_JNG_ ',
     &    '= ',NSG,', expecting IB =',IB
        call exit(2)
      endif

      nmiss = vtxgrid_nmiss((/ (GLAT(ING(I),JNG(I)),i=1,IB) /),
     &                      (/ (GLON(ING(I),JNG(I)),I=1,IB) /),
     &                      ALAT(1:JX),ALON(1:IX) )
      if(nmiss > 0) then

        write(6,'(2a,i2,3a   )') myname_,
     &  '(): >>> WARNING <<< storm #',KST,' (',trim(ST_NAME(KST)),')'
        write(6,'(4a         )') myname_,
     &  '(): >>> WARNING <<<                                     tcvital message = "',trim(TCVT(KST)(1:32)),'"'
        write(6,'(2a,2f10.3  )') myname_,
     &  '(): >>> WARNING <<<          read-in observed storm location, (lat,lon) = ',clat_tcvin(KST),clon_tcvin(KST)

        write(6,'(2a,i6,4x,i6)') myname_,
     &  '(): >>> WARNING <<<             dimension of the 40x40 range, (lat,lon) = ',JX,IX
        write(6,'(2a,2f10.3  )') myname_,
     &  '(): >>> WARNING <<<    upper-right corner of the 40x40 range, (lat,lon) = ',ALAT(JX),ALON(IX)
        write(6,'(2a,2f10.3  )') myname_,
     &  '(): >>> WARNING <<<         on-time estimated storm location, (lat,lon) = ',CLAT_NHC,CLON_NHC
        write(6,'(2a,2f10.3  )') myname_,
     &  '(): >>> WARNING <<<     lower-left corner of the 40x40 range, (lat,lon) = ',ALAT(1),ALON(1)

        call vtxgrid_lonRange(glon_min,glon_max, (/ (GLON(ING(I),JNG(I)),I=1,IB) /), reflon=CLON_NEW)
        glat_min = minval    ((/ (GLAT(ING(I),JNG(I)),I=1,IB) /))
        glat_max = maxval    ((/ (GLAT(ING(I),JNG(I)),I=1,IB) /))
        
        write(6,'(2a,i6,a,i4,a)') myname_,
     &  '(): >>> WARNING <<<       total count of modeled vortex grid points, IB = ',IB,' (with',nmiss,' miss)'
        write(6,'(2a,2f10.3  )') myname_,
     &  '(): >>> WARNING <<< upper-right corner of the modeled vortex, (lat,lon) = ',glat_max,glon_max
        write(6,'(2a,2f10.3  )') myname_,
     &  '(): >>> WARNING <<<                  modeled vortex location, (lat,lon) = ',CLAT_NEW,CLON_NEW
        write(6,'(2a,2f10.3  )') myname_,
     &  '(): >>> WARNING <<<  lower-left corner of the modeled vortex, (lat,lon) = ',glat_min,glon_min

        write(6,'(2a         )') myname_,
     &  '(): >>> WARNING <<< This vortex is not all within the 40x40 range of observed storm, and not relocated'

        AMDX=0.
        AMDY=0.
        MDX=0
        MDY=0
        if(stat_tcvin(KST)==STRM_Normal) stat_tcvin(KST) = STRM_TooWide
      endif

C
      _TRACE_('calling landcmsk()')
      CALL landcmsk(IMAX,JMAX,GLON,GLAT,ZDATG,IFLAG,lsflag,kst,ITIM)
        ! lsflag is not used anywhere later.
      if(IFLAG/=0 .and. stat_tcvin(kst)==STRM_Normal) 
     &  stat_tcvin(kst) = STRM_ZGT500m

c temp relocation turned on
c      IFLAG = 0        ! IFLAG==1 is no_temp_relocation 

c Check if the syndata need to be called

      _TRACE_('if(ITIM.EQ.3)')
      IF(ITIM.EQ.3)THEN
        DO I=1,K1STM
          IF(STMNAME(I).EQ.ST_NAME(KST))THEN
            IFWRT=0
            DO J=1,3
        IF(ISTMCY1(J,I).EQ.0.and.ISTMCX1(J,I).EQ.0)THEN
              IFWRT=1
            END IF
            END DO
            IF(IFWRT.EQ.1)THEN
              WRITE(55,101) TCVT(KST)
            END IF
  101       FORMAT(A95)
          END IF
        END DO
      END IF


c      print *,'GAUSSIAN GRID # WITHIN R0 ',IB
c      DO I = 1,IB
c      print *,'GAUSSIAN GRID WITHIN R0, LAT,LON ',
c     1      GLAT(ING(I),JNG(I)),GLON(ING(I),JNG(I))
c      print *,'GAUSSIAN GRID WITHIN R0 ',ING(I),JNG(I)
c      ENDDO

C.. SETTING VALUE for xvect, yvect, a(,), capd2

      _TRACE_('calling rodist()')
      call rodist

      _TRACE_('calling amatrix()')
      call amatrix
c

      KMP=2*KMAX+1
      KDIV1=3+KMP
      KQ1=KDIV1+4*KMP

      NRED1 = 0

      IG = 0
      DO 777 IV = 1,MTV3
!!      	print*,'HURR_REL: loop 777, IV=',IV,'/',MTV3

      IREM = -22
C
        ! NRED1_SKIP = NRED1+1 == 0+IV
        ! SKIP(:,:) = PDAT(:,:,NRED1_SKIP)
      CALL READ1(KST,NRED1,MTV3,SKIP,PDAT)
C
      DO J=1,JX
      DO I=1,IX
        ENV1(I,J,IV) = SKIP(I,J)
      ENDDO
      ENDDO

      IF(IV.GT.KDIV1.AND.IV.LE.KQ1)IREM=MOD(IV-KDIV1,4)
      IF((IV.GE.3.AND.IV.LE.KDIV1).OR.(IV.GT.KQ1).OR.
     1     (IREM.EQ.1.OR.IREM.EQ.2)) THEN
      IG = IG+1
c      print *,'ORIGINAL VARIABLE # IS ',IV
c      print *,'VARIABLE # IS ',IG

c added by Qingfu Liu
c obtain the disturbance field

      DO J=1,JX
      DO I=1,IX
        U(I,J)=SKIP(I,J)
      END DO
      END DO
c
c First smooth in east-west direction
c
      DO 107 J=1,JX
      DO N=1,NF
      XTU(1,N)  = U(1,J)
      XTU(IX,N) = U(IX,J)
      ENDDO
C
      DO I=2,IX-1
      XTU(I,1) = U(I,J)+FK(1)*(U(I-1,J)+U(I+1,J)-2.*U(I,J))
      ENDDO
C
      DO N=2,NF
      DO I=2,IX-1
      XTU(I,N)=XTU(I,N-1)+FK(N)*(XTU(I-1,N-1)+XTU(I+1,N-1)-2.
     1         *XTU(I,N-1))
      ENDDO
      ENDDO
C
      DO I=1,IX
      US(I,J) = XTU(I,NF)
      ENDDO
C
 107  CONTINUE
C
C.. DO MERIDIONAL FILTER
C
      DO 207 I=1,IX
C
      DO N=1,NF
      YTU(1,N)  = US(I,1)
      YTU(JX,N) = US(I,JX)
      ENDDO
C
      DO J = 2 , JX-1
      YTU(J,1) = US(I,J) + FK(1)*(US(I,J-1) + US(I,J+1)
     *                          -2.*US(I,J))
      ENDDO
CC
      DO N = 2 , NF
      DO J = 2 , JX-1
      YTU(J,N) = YTU(J,N-1) + FK(N)*(YTU(J-1,N-1)  +
     *              YTU(J+1,N-1) - 2.*YTU(J,N-1))
      ENDDO
      ENDDO
C
      DO J = 1 , JX
      US(I,J)   =  YTU(J,NF)
      ENDDO
 207  CONTINUE
C
C.. GET THE DISTURBANCE FIELD
C
      DO I=1,IX
      DO J=1,JX
      DKY(I,J) = U(I,J) - US(I,J)
      ENDDO
      ENDDO

      DKM=DKY
      CALL SEPAR(DKY,DKM)

      DO J=1,JX
      DO I=1,IX
        SKIP(I,J)=DKM(I,J)
c        SKIP(I,J)=U(I,J)
        DKY(I,J) = DKM(I,J) + US(I,J)
c        DKY(I,J) = U(I,J)
      ENDDO
      ENDDO

      DO J=1,JX
      DO I=1,IX
cnew        ENV1(I,J,IV) = DKY(I,J)
        ENV1(I,J,IV) = DKY(I,J)-PDAT(I,J,IV)
      ENDDO
      ENDDO

      ENDIF

 777  CONTINUE

      ENV=0.

      CALL P2SIG(IX,JX,KMAX,MTV2,MTV3,ENV(1,1,1),ENV1(1,1,1),
     &       PDAT(1,1,1),HDAT(1,1,1,KST),PSC_MX1,HSIG,HP,KST,
     &	aki,SI,akl,SL)

!!      CALL P2SIG(KMAX,MTV2,MTV3,ENV(1,1,1),ENV1(1,1,1),
!!     &       PDAT(1,1,1),HDAT(1,1,1,KST),PSC_MX1,HSIG,HP,KST)

cnew    K=1,2 and the U,V field was doubled here, but never used later
      DO K=1,MTV2
      DO J=1,JX
      DO I=1,IX
        ENV(I,J,K)=ENV(I,J,K)+HDAT(I,J,K,KST)
      END DO
      END DO
      END DO
cnew

      KDIV2=3+KMAX
      KQ2=KDIV2+4*KMAX

      IG = 0
!!      print'(1x,a,8i6)','HURR_REL(): ',
!!     &		KMAX,MTV,MTV2,MTV3
      DO 781 IV = 1,MTV2
!!      	print*,'HURR_REL: loop 781, IV=',IV,'/',MTV2

      IREM = -22
C
      IF(IV.GT.KDIV2.AND.IV.LE.KQ2)IREM=MOD(IV-KDIV2,4)
      IF((IV.GE.3.AND.IV.LE.KDIV2).OR.(IV.GT.KQ2).OR.
     1     (IREM.EQ.1.OR.IREM.EQ.2)) THEN
      IG = IG+1

      DO J=1,JX
      DO I=1,IX
        DKY(I,J)=ENV(I,J,IV)
      ENDDO
      ENDDO
      CALL GMOVE(KST,KSTM,
     1		KMAX,IMAX,JMAX,MTV,GLON,GLAT,HDATA,PSLB,ZDATG,
     1          IX,JX,ALON,ALAT,DKY,
     2		IB,ING(1:IB),JNG(1:IB),MDX,MDY,AMDX,AMDY, IV,IG,IFLAG)

c      CALL GMOVE(KST,MWAVE,MAXWV2,MTV,MTV1,HDATA,SKIP2,DKY,
c     1          IG,IFLAG,PSLB)

      ENDIF

 781  CONTINUE
!!	  print*,'PSL(781/) = ',exp(minval(HDATA(:,:,2))),
!!     &                         exp(maxval(HDATA(:,:,2)))

C
!!	  print*,'PSL(788/) = ',exp(minval(HDATA(:,:,2))),
!!     &                         exp(maxval(HDATA(:,:,2)))

      RDIST2=AMDX*AMDX+AMDY*AMDY
      IF(RDIST2.LE.0.02) THEN
        PRINT'(4a)',myname_,'(): storm "',ST_NAME(KST),'" is not relocated.'
      ELSE
        if(IFLAG==0) then
          PRINT'(4a,i2)',myname_,'(): storm "',ST_NAME(KST),'" is relocated.  IFLAG =',IFLAG
        else
          PRINT'(4a,i2)',myname_,'(): storm "',ST_NAME(KST),'" is relocated (wind only).  IFLAG =',IFLAG
        endif
      END IF

 788  CONTINUE
      call hurrmess_summary(6)
      PRINT*,myname_,'(): TIME= ',ITIM,'h, vortex relocation is completed'
      _EXIT_
      END SUBROUTINE HURR_REL

      subroutine landcmsk(IK,JK,GLON,GLAT,ZDATG,IFLAG,lsflag,kst,ITIM) 
      use m_hurrmess, only: ST_NAME
      implicit none
      integer,intent(in) :: IK,JK
      real,DIMENSION(IK,JK),intent(in) :: GLON,GLAT,ZDATG
      integer,intent(out) :: IFLAG,lsflag
      integer,intent(in) :: KST,ITIM
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer,parameter :: NSG=_SIZE_OF_ING_JNG_
      integer :: ING,JNG,IB
      DIMENSION ING(NSG),JNG(NSG)
      COMMON /TR/ING,JNG,IB

      integer :: I,IW,JW
      character(len=*),parameter :: myname_='landcmsk'
      _ENTRY_
c
      lsflag = 1

      DO I = 1,IB
        IW = ING(I)
        JW = JNG(I)
        IF(ZDATG(IW,JW).gt.500.)then
          iflag = 1
          print*,' Filter domain topography height > 500 m'
     1           ,', storm name = ', ST_NAME(KST),
     2           ', forecast time = ',ITIM,'h',
     3            ', only wind field is relocated, if a relocation is done.'
          go to 50
        END IF
      END DO        
 
 50   continue

      _EXIT_
      end subroutine landcmsk

