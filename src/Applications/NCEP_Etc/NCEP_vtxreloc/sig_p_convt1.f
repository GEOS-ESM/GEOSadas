!#define DEBUG_TRACE
!#include "mytrace.H"
        SUBROUTINE SIG2P(IX,JX,KMAX,MTV2,MTV3,HDAT,PDAT,PSFCM,H,HP,KST,
     &    aki,SI,akl,SL)
!??        implicit none
        integer,intent(in) :: IX,JX     ! (lon,lat) sizes
        integer,intent(in) :: KMAX      ! level size of 3-d HDAT fields
        integer,intent(in) :: MTV2      ! var-lev size of HDAT
        integer,intent(in) :: MTV3      ! var-lev size of PDAT
        real,dimension(IX,JX,MTV2),intent(in ) :: HDAT ! a storm
        real,dimension(IX,JX,MTV3),intent(out) :: PDAT ! a storm
        real,intent(in) :: PSFCM        ! mean(?) p_sfc
        REAL,dimension(IX,JX,KMAX),intent(inout) :: H(IX,JX,KMAX)
        REAL,dimension(IX,JX,KMAX),intent(inout) :: HP(IX,JX,2*KMAX+1)
        integer,intent(in) :: KST       ! storm index; not used.
        real,dimension(KMAX+1),intent(in) :: aki,SI     ! "i"nterface
        real,dimension(KMAX  ),intent(in) :: akl,SL     ! "l"ayer-mean
c
c subprogram:
c   prgmmr: Qingfu Liu    org:              date: 2000-04-25 
c
c abstract:
c   Convert data from SIG surface to P surface.
c
c history:
c   27Jul2005 Todling - fixed splie2 call to spline (wrong # args)
c   21Oct2005 Todling - forced exit when SOMETHING IS WRONG
c usage: call
c   Input: HDAT - DATA at SIG surface
c          KST: not used
C   Ouput: PDAT - DATA at P surface

!!        PARAMETER (IX=41, JX=41)
 
!!        REAL HDAT(IX,JX,MTV2),PDAT(IX,JX,MTV3)
        REAL ZS(IX,JX),PS(IX,JX),APS(IX,JX)
!!        REAL H(IX,JX,KMAX),HP(IX,JX,2*KMAX+1)

c        REAL(4) FHOUR,X(160),SI(KMAX+1),SL(KMAX)
!!        REAL*4 FHOUR,DUMMY(245)
!!        COMMON /COEF3/FHOUR,DUMMY
        
        REAL, ALLOCATABLE :: TV(:,:,:),DIV(:,:,:),VORT(:,:,:),
     &                       U(:,:,:),V(:,:,:),SH(:,:,:)
        REAL, ALLOCATABLE :: PSIG(:,:,:),RH(:,:,:),
     &                       APG(:,:,:),T(:,:,:)
        REAL, ALLOCATABLE :: P(:),AP(:)
        REAL, ALLOCATABLE :: DIVP(:,:,:),VORTP(:,:,:),UP(:,:,:),
     &                 VP(:,:,:),RHP(:,:,:),SHP(:,:,:),TP(:,:,:)      

 !!       REAL, ALLOCATABLE :: SI(:),SL(:)
 
        KMAX1=KMAX+1
        NMAX=2*KMAX+1

!!        ALLOCATE ( SI(KMAX1),SL(KMAX) )

!!        DO K=1,KMAX1
!!          SI(K)=DUMMY(K)
!!        END DO
!!        DO K=1,KMAX
!!          SL(K)=DUMMY(KMAX1+K)
!!        END DO

        ALLOCATE ( TV(IX,JX,KMAX), DIV(IX,JX,KMAX),
     &             VORT(IX,JX,KMAX),U(IX,JX,KMAX),
     &             V(IX,JX,KMAX),SH(IX,JX,KMAX) )

        ALLOCATE ( PSIG(IX,JX,KMAX),RH(IX,JX,KMAX),
     &         APG(IX,JX,KMAX),T(IX,JX,KMAX) )

        ALLOCATE ( P(NMAX),AP(NMAX) )
        ALLOCATE ( DIVP(IX,JX,NMAX),VORTP(IX,JX,NMAX),
     &             UP(IX,JX,NMAX), VP(IX,JX,NMAX),
     &             RHP(IX,JX,NMAX),SHP(IX,JX,NMAX),
     &             TP(IX,JX,NMAX) )

        COEF1=461.5/287.05-1.
        COEF2=287.05/9.8

c Surface Height and Surface Press
        DO J=1,JX
        DO I=1,IX
          ZS(I,J)=HDAT(I,J,1)
          PS(I,J)=EXP(HDAT(I,J,2))*1000.
          APS(I,J)=ALOG(PS(I,J))
        END DO
        END DO

c DIV, VORT, U, V, T and Specific Humidity at Sigma Level
        DO K=1,KMAX
          DO J=1,JX
          DO I=1,IX
            DIV(I,J,K)=HDAT(I,J,KMAX+4+4*(K-1))    
            VORT(I,J,K)=HDAT(I,J,KMAX+5+4*(K-1))   
            U(I,J,K)=HDAT(I,J,KMAX+6+4*(K-1)) 
            V(I,J,K)=HDAT(I,J,KMAX+7+4*(K-1))
            SH(I,J,K)=HDAT(I,J,KMAX*5+3+K)
            TV(I,J,K)=HDAT(I,J,3+K)
            T(I,J,K)=TV(I,J,K)/(1.+COEF1*SH(I,J,K))
          END DO
          END DO
        END DO
     
c Press at Sigma-Level
        DO K=1,KMAX
        DO J=1,JX
        DO I=1,IX
          PSIG(I,J,K)=SL(K)*PS(I,J)+akl(K)
          APG(I,J,K)=ALOG(PSIG(I,J,K))
        END DO
        END DO
        END DO

        DO J=1,JX
        DO I=1,IX
          TVD=TV(I,J,1)
          H(I,J,1)=ZS(I,J)-
     &            COEF2*TVD*(APG(I,J,1)-APS(I,J))        
          DO K=2,KMAX
            TVU=TV(I,J,K)
            H(I,J,K)=H(I,J,K-1)-
     &      COEF2*0.5*(TVD+TVU)*(APG(I,J,K)-APG(I,J,K-1))
            TVD=TVU
          END DO
        END DO
        END DO

c Const. P-Level      
        DO K=1,KMAX
          P(2*K-1)=SI(K)*PSFCM+aki(K)
          P(2*K)  =SL(K)*PSFCM+akl(K)
        END DO
        P(NMAX)=.5*(SL(KMAX)*PSFCM+akl(KMAX))
        DO N=1,NMAX
          AP(N)=ALOG(P(N))
        END DO

        GAMA=6.5E-3
        COEF3=COEF2*GAMA
        DO J=1,JX
        DO I=1,IX
          HP(I,J,1)=H(I,J,1)+
     &        T(I,J,1)/GAMA*(1.-(P(1)/PSIG(I,J,1))**COEF3)
          HP(I,J,NMAX)=H(I,J,KMAX)+
     &    T(I,J,KMAX)/GAMA*(1.-(P(NMAX)/PSIG(I,J,KMAX))**COEF3)
          DO N=2,NMAX-1
            K=(N-1)/2+1
            HP(I,J,N)=H(I,J,K)+
     &          T(I,J,K)/GAMA*(1.-(P(N)/PSIG(I,J,K))**COEF3)
          END DO
        END DO
        END DO
 
        DO N=1,NMAX
          K=(N-1)/2+1
c          PRINT*,'Press=',N,P(N)/100.
!          PRINT*,'Press1=',N,K,P(N),HP(20,20,N),H(20,20,K)
        END DO

c RH at K=1 (Sigma=0.995)
!        DO K=1,KMAX
        K=1
        DO J=1,JX
        DO I=1,IX
          DTEMP=T(I,J,K)-273.15
          ES=611.2*EXP(17.67*DTEMP/(DTEMP+243.5))
          SHS=0.622*ES/(PSIG(I,J,K)-0.378*ES)
          RH(I,J,K)=MIN(MAX(SH(I,J,K)/SHS,0.),1.0)
        END DO
        END DO
!        END DO

! Interpolate to Const. Press Level.
        DO J=1,JX
        DO I=1,IX
        DO N=1,NMAX
          IF(P(N).GE.PSIG(I,J,1))THEN        
! below SIGMA K=1
            DIVP(I,J,N)=DIV(I,J,1)
            VORTP(I,J,N)=VORT(I,J,1)
            UP(I,J,N)=U(I,J,1)
            VP(I,J,N)=V(I,J,1)
            RHP(I,J,N)=RH(I,J,1)           ! RH at SIGMA K=1
            TDRY=T(I,J,1)-GAMA*(HP(I,J,N)-H(I,J,1))
            DTEMP=TDRY-273.15
            ES=611.2*EXP(17.67*DTEMP/(DTEMP+243.5))
            SHS=0.622*ES/(P(N)-0.378*ES)
            SHP(I,J,N)=RHP(I,J,N)*SHS
            TP(I,J,N)=TDRY*(1.+COEF1*SHP(I,J,N))
! within domain
           ELSE IF((P(N).LT.PSIG(I,J,1)).AND.
     &             (P(N).GT.PSIG(I,J,KMAX)))THEN                            
             DO L=1,KMAX
              IF((P(N).LT.PSIG(I,J,L)).AND.
     &               (P(N).GE.PSIG(I,J,L+1)))THEN 
                W=(AP(N)-APG(I,J,L))/(APG(I,J,L+1)-APG(I,J,L))
c             W1=(P(N)-PSIG(I,J,L))/(PSIG(I,J,L+1)-PSIG(I,J,L))
                DIVP(I,J,N)=DIV(I,J,L)+
     &                      W*(DIV(I,J,L+1)-DIV(I,J,L))
                VORTP(I,J,N)=VORT(I,J,L)+
     &                      W*(VORT(I,J,L+1)-VORT(I,J,L))
                UP(I,J,N)=U(I,J,L)+W*(U(I,J,L+1)-U(I,J,L))
                VP(I,J,N)=V(I,J,L)+W*(V(I,J,L+1)-V(I,J,L))
                TP(I,J,N)=TV(I,J,L)+W*(TV(I,J,L+1)-TV(I,J,L))
                SHP(I,J,N)=SH(I,J,L)+W*(SH(I,J,L+1)-SH(I,J,L)) 
                GO TO 123
              END IF
             END DO
 123         CONTINUE
! above top
         ELSE IF(P(N).LE.PSIG(I,J,KMAX))THEN
           DIVP(I,J,N)=DIV(I,J,KMAX)
           VORTP(I,J,N)=VORT(I,J,KMAX)
           UP(I,J,N)=U(I,J,KMAX)
           VP(I,J,N)=V(I,J,KMAX)
           TDRY=T(I,J,KMAX)-GAMA*(HP(I,J,N)-H(I,J,KMAX))
           SHP(I,J,N)=SH(I,J,KMAX)
           TP(I,J,N)=TDRY*(1.+COEF1*SHP(I,J,N))
         ELSE
           PRINT*,'SOMETHING IS WRONG'
           call exit (1)
         END IF

        END DO
        END DO
        END DO

        DO J=1,JX
        DO I=1,IX
          PDAT(I,J,1)=HDAT(I,J,1)
          PDAT(I,J,2)=HDAT(I,J,2)
          PDAT(I,J,3)=HDAT(I,J,3)
          DO N=1,NMAX
            PDAT(I,J,NMAX+4+4*(N-1))=DIVP(I,J,N)
            PDAT(I,J,NMAX+5+4*(N-1))=VORTP(I,J,N)
            PDAT(I,J,NMAX+6+4*(N-1))=UP(I,J,N)
            PDAT(I,J,NMAX+7+4*(N-1))=VP(I,J,N)
            PDAT(I,J,NMAX*5+3+N)=SHP(I,J,N) 
            PDAT(I,J,3+N)=TP(I,J,N)
          END DO
        END DO
        END DO

!!        DEALLOCATE ( SI,SL )

        DEALLOCATE ( T, TV, DIV, VORT, U, V, SH )

        DEALLOCATE ( PSIG, RH, APG )

        DEALLOCATE ( P, AP )
        DEALLOCATE ( DIVP, VORTP, UP, VP, RHP, SHP, TP )


        END SUBROUTINE SIG2P


        SUBROUTINE P2SIG(IX,JX,KMAX,MTV2,MTV3,HDPB,PDPB,PDAT,HDAT,
     &                   PSFCM,H,HP,KST,aki,SI,akl,SL)
!??        implicit none
        integer,intent(in) :: IX,JX     ! (lon,lat) sizes
        integer,intent(in) :: KMAX      ! level size of HDAT(?)
        integer,intent(in) :: MTV2
        integer,intent(in) :: MTV3
        REAL,dimension(IX,JX,MTV2) :: HDPB,HDAT
        REAL,dimension(IX,JX,MTV3) :: PDPB,PDAT
        REAL,dimension(IX,JX,KMAX) :: H
        REAL,dimension(IX,JX,2*KMAX+1) :: HP
        integer,intent(in) :: KST
        real,dimension(KMAX+1),intent(in) :: aki,SI
        real,dimension(KMAX  ),intent(in) :: akl,SL

c P to SIG conversion
c
c Input: HDPB (perturbation part), PDPB (perturbation part)
c Input: PDAT (total field), PDPB+PDAT = ENV part
C Ouput: HDPB (the value at the top most level kmax is not changed)
c KST: not used

!!        PARAMETER (IX=41, JX=41)

!!        REAL HDPB(IX,JX,MTV2),HDAT(IX,JX,MTV2)
!!        REAL PDPB(IX,JX,MTV3),PDAT(IX,JX,MTV3)
        REAL ZS(IX,JX),PS(IX,JX),APS(IX,JX)
!!        REAL H(IX,JX,KMAX),HP(IX,JX,2*KMAX+1) 

c        REAL(4) FHOUR,X(160),SI(KMAX+1),SL(KMAX)
!!        REAL*4 FHOUR,DUMMY(245)
!!        COMMON /COEF3/FHOUR,DUMMY

        REAL, ALLOCATABLE :: TV(:,:,:),DIV(:,:,:),VORT(:,:,:),
     &                       U(:,:,:),V(:,:,:),SH(:,:,:)
        REAL, ALLOCATABLE :: PSIG(:,:,:),RH(:,:,:),
     &                       APG(:,:,:)
        REAL, ALLOCATABLE :: P(:),AP(:)
        REAL, ALLOCATABLE :: DIVP(:,:,:),VORTP(:,:,:),UP(:,:,:),
     &                       VP(:,:,:),RHP(:,:,:)
        REAL, ALLOCATABLE :: TVP(:,:,:),TVP_E(:,:,:)
        REAL, ALLOCATABLE :: TP_E(:,:,:)
        REAL, ALLOCATABLE :: SHP(:,:,:),SHP_E(:,:,:)
        REAL, ALLOCATABLE :: HT_T(:,:,:),HSH_T(:,:,:)

!!        REAL, ALLOCATABLE :: SI(:),SL(:)

        KMAX1=KMAX+1
        NMAX=2*KMAX+1

!!        ALLOCATE ( SI(KMAX1),SL(KMAX) )

!!        DO K=1,KMAX1
!!          SI(K)=DUMMY(K)
!!        END DO
!!        DO K=1,KMAX
!!          SL(K)=DUMMY(KMAX1+K)
!!        END DO

        ALLOCATE ( TV(IX,JX,KMAX), DIV(IX,JX,KMAX),
     &             VORT(IX,JX,KMAX),U(IX,JX,KMAX),
     &             V(IX,JX,KMAX),SH(IX,JX,KMAX) )

        ALLOCATE ( PSIG(IX,JX,KMAX),RH(IX,JX,KMAX),
     &             APG(IX,JX,KMAX) )

        ALLOCATE ( HT_T(IX,JX,KMAX),HSH_T(IX,JX,KMAX) )

        ALLOCATE ( TVP(IX,JX,NMAX),TVP_E(IX,JX,NMAX),
     &             SHP(IX,JX,NMAX),SHP_E(IX,JX,NMAX),
     &             TP_E(IX,JX,NMAX) )

        ALLOCATE ( P(NMAX),AP(NMAX) )
        ALLOCATE ( DIVP(IX,JX,NMAX),VORTP(IX,JX,NMAX),
     &             UP(IX,JX,NMAX), VP(IX,JX,NMAX),
     &             RHP(IX,JX,NMAX) )

        COEF1=461.5/287.05-1.
        COEF2=287.05/9.8

c Surface Height and Surface Press
        DO J=1,JX
        DO I=1,IX
          ZS(I,J)=PDPB(I,J,1)                 ! Full field
          PS(I,J)=EXP(PDPB(I,J,2))*1000.      ! FULL field
          APS(I,J)=ALOG(PS(I,J))
        END DO
        END DO

c DIV, VORT, U, V, T and Specific Humidity at P-Level
        DO J=1,JX
        DO I=1,IX
        DO N=1,NMAX
          DIVP(I,J,N)=PDPB(I,J,NMAX+4+4*(N-1))
          VORTP(I,J,N)=PDPB(I,J,NMAX+5+4*(N-1))
          UP(I,J,N)=PDPB(I,J,NMAX+6+4*(N-1))
          VP(I,J,N)=PDPB(I,J,NMAX+7+4*(N-1))
          SHP(I,J,N)=PDPB(I,J,NMAX*5+3+N)
          SHP_E(I,J,N)=SHP(I,J,N)+PDAT(I,J,NMAX*5+3+N)
          TVP(I,J,N)=PDPB(I,J,3+N)
          TVP_E(I,J,N)=TVP(I,J,N)+PDAT(I,J,3+N)
          TP_E(I,J,N)=TVP_E(I,J,N)/(1.+COEF1*SHP_E(I,J,N))
        END DO
        END DO
        END DO

        DO J=1,JX
        DO I=1,IX
          DO K=1,KMAX-1
            HSH_T(I,J,K)=HDAT(I,J,KMAX*5+3+K)      ! Specific Hum.
            HT_T(I,J,K)=HDAT(I,J,3+K)
          END DO
        END DO
        END DO

c Const. P-Level      
        DO K=1,KMAX
          P(2*K-1)=SI(K)*PSFCM+aki(K)
          P(2*K)  =SL(K)*PSFCM+akl(K)
        END DO
        P(NMAX)=.5*(SL(KMAX)*PSFCM+akl(KMAX))
        DO N=1,NMAX
          AP(N)=ALOG(P(N))
        END DO

        GAMA=6.5E-3
        COEF3=COEF2*GAMA
!        DO J=1,JX
!        DO I=1,IX
!          TVD=TVP_E(I,J,1)
!          HP(I,J,1)=ZS(I,J)-
!     &            TP_E(I,J,1)/GAMA*(1.-(PS(I,J)/P(1))**COEF3)
!          DO N=2,NMAX
!            TVU=TVP_E(I,J,N)
!            HP(I,J,N)=HP(I,J,N-1)-
!     &      COEF2*0.5*(TVD+TVU)*(AP(N)-AP(N-1))
!            TVD=TVU
!          END DO
!        END DO
!        END DO

c Press at Sigma-Level
        DO K=1,KMAX
        DO J=1,JX
        DO I=1,IX
          PSIG(I,J,K)=SL(K)*PS(I,J)+akl(K)
          APG(I,J,K)=ALOG(PSIG(I,J,K))
        END DO
        END DO
        END DO


!        DO K=1,KMAX
!          N=2*K
!          DO J=1,JX
!          DO I=1,IX
!            H(I,J,K)=HP(I,J,N)+
!     &        TP_E(I,J,N)/GAMA*(1.-(PSIG(I,J,K)/P(N))**COEF3)
!          END DO
!          END DO
!        END DO

        DO N=1,NMAX
          K=(N-1)/2+1
c          PRINT*,'Press=',N,P(N)/100.
c          PRINT*,'Press2=',N,K,P(N),HP(20,20,N),H(20,20,K)
        END DO
 
c RH at Press level
!        DO N=1,NMAX
        N=1
        DO J=1,JX
        DO I=1,IX
          DTEMP=TP_E(I,J,N)-273.15
          ES=611.2*EXP(17.67*DTEMP/(DTEMP+243.5))
          SHS=0.622*ES/(P(N)-0.378*ES)
          RHP(I,J,N)=MIN(MAX(SHP_E(I,J,N)/SHS,0.),1.0)
        END DO
        END DO
!        END DO

! Interpolate to Sigma Level.
        DO J=1,JX
        DO I=1,IX
        DO K=1,KMAX
          IF(PSIG(I,J,K).GE.P(1))THEN        
! below Press K=1
            DIV(I,J,K)=DIVP(I,J,1)
            VORT(I,J,K)=VORTP(I,J,1)
            U(I,J,K)=UP(I,J,1)
            V(I,J,K)=VP(I,J,1)
            RH(I,J,K)=RHP(I,J,1)           ! RH at SIGMA K=1
            TDRY=TP_E(I,J,1)-GAMA*(H(I,J,K)-HP(I,J,1))
            DTEMP=TDRY-273.15
            ES=611.2*EXP(17.67*DTEMP/(DTEMP+243.5))
            SHS=0.622*ES/(PSIG(I,J,K)-0.378*ES)
            SH_E=RH(I,J,K)*SHS
            SH(I,J,K)=SH_E-HSH_T(I,J,K)           ! Pert. Part
            TV(I,J,K)=TDRY*(1.+COEF1*SH_E)-HT_T(I,J,K)
!            PRINT*,'LLL2=',SHP(I,J,1),SHP_E(I,J,K)
!            PRINT*,'     ',SH(I,J,K),SH_E
! within domain
           ELSE IF((PSIG(I,J,K).LT.P(1)).AND.
     &             (PSIG(I,J,K).GT.P(NMAX)))THEN                            
             DO L=1,NMAX-1
              IF((PSIG(I,J,K).LT.P(L)).AND.
     &               (PSIG(I,J,K).GE.P(L+1)))THEN 
                W=(APG(I,J,K)-AP(L))/(AP(L+1)-AP(L))
c                W1=(PSIG(I,J,K)-P(L))/(P(L+1)-P(L))
                DIV(I,J,K)=DIVP(I,J,L)+
     &                     W*(DIVP(I,J,L+1)-DIVP(I,J,L))
                VORT(I,J,K)=VORTP(I,J,L)+
     &                      W*(VORTP(I,J,L+1)-VORTP(I,J,L))
                U(I,J,K)=UP(I,J,L)+W*(UP(I,J,L+1)-UP(I,J,L))
                V(I,J,K)=VP(I,J,L)+W*(VP(I,J,L+1)-VP(I,J,L))
                TV(I,J,K)=TVP(I,J,L)+W*(TVP(I,J,L+1)-TVP(I,J,L))
              SH(I,J,K)=SHP(I,J,L)+W*(SHP(I,J,L+1)-SHP(I,J,L)) 
                GO TO 123
              END IF
             END DO
 123         CONTINUE
! above top
         ELSE IF(PSIG(I,J,K).LE.P(NMAX))THEN
           DIV(I,J,K)=DIVP(I,J,NMAX)
           VORT(I,J,K)=VORTP(I,J,NMAX)
           U(I,J,K)=UP(I,J,NMAX)
           V(I,J,K)=VP(I,J,NMAX)
           TDRY=TP_E(I,J,NMAX)-GAMA*(H(I,J,K)-HP(I,J,NMAX))
           SH(I,J,K)=SHP(I,J,NMAX)
           SH_E=SH(I,J,K)+HSH_T(I,J,K)
           TV(I,J,K)=TDRY*(1.+COEF1*SH_E)-HT_T(I,J,K)
         ELSE
           PRINT*,'SOMETHING IS WRONG'
           call exit (2)
         END IF

        END DO
        END DO
        END DO

        DO J=1,JX
        DO I=1,IX
          HDPB(I,J,1)=PDPB(I,J,1)
          HDPB(I,J,2)=PDPB(I,J,2)
          HDPB(I,J,3)=PDPB(I,J,3)
          DO K=1,KMAX-1
            HDPB(I,J,KMAX+4+4*(K-1))=DIV(I,J,K)
            HDPB(I,J,KMAX+5+4*(K-1))=VORT(I,J,K)
            HDPB(I,J,KMAX+6+4*(K-1))=U(I,J,K)
            HDPB(I,J,KMAX+7+4*(K-1))=V(I,J,K)
            HDPB(I,J,KMAX*5+3+K)=SH(I,J,K) 
            HDPB(I,J,3+K)=TV(I,J,K)
          END DO
        END DO
        END DO

!!        DEALLOCATE ( SI,SL )

        DEALLOCATE ( TV, DIV, VORT, U, V, SH )

        DEALLOCATE ( PSIG, RH, APG )

        DEALLOCATE ( P, AP )
        DEALLOCATE ( DIVP, VORTP, UP, VP, RHP, SHP )

        DEALLOCATE ( TVP, TVP_E, TP_E, SHP_E, HT_T, HSH_T)

        END SUBROUTINE P2SIG

        SUBROUTINE MOVETX(IGU,JGU,GLON,GLAT,DATG,DDAT,
     &    NSG,ING,JNG,MDX,MDY,AMDX,AMDY)
       
        use m_die, only: die, perr
        use m_vtxgrid, only: vtxgrid_lonNear
        use m_vtxgrid, only: vtxgrid_lonRange
        implicit none
        integer,PARAMETER :: NSG1=200

        integer,intent(in) :: IGU,JGU
        real   ,intent(in) :: GLAT(IGU,JGU),GLON(IGU,JGU)
        real   ,intent(inout) :: DATG(IGU,JGU)
        real   ,intent(in) :: DDAT(IGU,JGU)
        integer,intent(in) :: NSG       ! == IB
        integer,intent(in) :: ING(NSG),JNG(NSG)
        integer,intent(in) :: MDX,MDY
        real   ,intent(in) :: AMDX,AMDY

        ! workspace:
        real:: hlon,hlat
        DIMENSION HLON(NSG1),HLAT(NSG1)
        real:: DTT,DTT2
        DIMENSION DTT(NSG1,NSG1),DTT2(NSG1,NSG1)

        character(len=*),parameter:: myname_='sig_p_convt1.MOVETX'

        integer :: IB
        integer :: I,J,IW,JW,II,JJ,IIM,JJM
        integer :: II1,JJ1,IW1,JW1
        integer :: IWMIN,IWMAX,IWMIN1,IWMAX1
        integer :: JWMIN,JWMAX,JWMIN1,JWMAX1
        real:: RDIST2,DATT
        real:: HLO,HLA
        real:: hlon_min,hlon_max

        IB=NSG

!      _SHOWX_('minval(in.DATG)', (minval(DATG)) )
!      _SHOWX_('maxval(in.DATG)', (maxval(DATG)) )
!      _SHOWX_('   avg(in.DATG)', (   sum(DATG)/size(DATG)) )
!      _SHOWX_('minval(in.DDAT)', (minval(DDAT)) )
!      _SHOWX_('maxval(in.DDAT)', (maxval(DDAT)) )
!      _SHOWX_('   avg(in.DDAT)', (   sum(DDAT)/size(DDAT)) )

        RDIST2=AMDX*AMDX+AMDY*AMDY
        IF(RDIST2.LE.0.02)THEN
          DO I = 1,IB
            IW = ING(I)
            JW = JNG(I)
            DATG(IW,JW)=DATG(IW,JW)+DDAT(IW,JW)
          END DO
          RETURN
        END IF

!      _SHOWX_('minval(01.DATG)', (minval(DATG)) )
!      _SHOWX_('maxval(01.DATG)', (maxval(DATG)) )
!      _SHOWX_('   avg(01.DATG)', (   sum(DATG)/size(DATG)) )

c        TEST=DATG
c        CALL MOVETX1(TEST,DDAT)

        IWMAX=0.
        IWMIN=1000.
        JWMAX=0.
        JWMIN=1000.
        DO I = 1,IB
          IW = ING(I)
          JW = JNG(I)
          IF(IWMAX.LT.IW)IWMAX=IW
          IF(IWMIN.GT.IW)IWMIN=IW
          IF(JWMAX.LT.JW)JWMAX=JW
          IF(JWMIN.GT.JW)JWMIN=JW
        END DO
!       print'(a,3i5,2x,2f10.4)','IWMAX,IWMIN=',IWMAX,IWMIN,IWMAX-IWMIN,
!    &          GLON(MODULO(IWMIN-1,IGU)+1,10),
!    &          GLON(MODULO(IWMAX-1,IGU)+1,10)

! Redo the search for IWMIN and IWMAX, for the range of the storm on the
! grid.  GLON(:,:) is assumed to be defined on meridian lines.
        call vtxgrid_lonRange(hlon_min,hlon_max, (/ (GLON(ING(I),JNG(I)),I=1,IB) /))
        IWMIN=vtxgrid_lonNear(GLON(1:IGU,10), hlon_min)
        IWMAX=vtxgrid_lonNear(GLON(1:IGU,10), hlon_max)
        if(IWMAX<IWMIN) IWMAX=IWMAX+IGU
!       print'(a,3i5,2x,4f10.4)','           =',IWMAX,IWMIN,IWMAX-IWMIN,
!    &          GLON(MODULO(IWMIN-1,IGU)+1,10),
!    &          GLON(MODULO(IWMAX-1,IGU)+1,10),
!    &          hlon_min, hlon_max

        IWMAX1=IWMAX+1
        IWMIN1=IWMIN-1
        JWMAX1=JWMAX+1
        JWMIN1=JWMIN-1

c        print*,'qliu=',IWMAX1,IWMIN1,JWMAX1,JWMIN1

        IIM=IWMAX-IWMIN+5
        JJM=JWMAX-JWMIN+5
        if(IIM>NSG1.or.JJM>NSG1) then
          call perr("MOVETX","insufficient workspace, NSG1 =",NSG1)
          call perr("MOVETX","                         IIM =",IIM)
          call perr("MOVETX","                         JJM =",JJM)
          call  die("MOVETX")
        endif

!!        write(6,'(a,10i4)') 'MOVETX>splin2() IGU,IWMIN1,IWMAX1,MDX,JGU,JWMIN1,JWMAX1,MDY =',
!!     &         IGU,IWMIN1,IWMAX1,MDX,JGU,JWMIN1,JWMAX1,MDY

        DO II=1,IIM
          II1=II+IWMIN-3 
          IF(II1.GT.IGU) THEN
            II1=II1-IGU
            HLON(II) = GLON(II1,10)+360.+AMDX
          ELSEIF(II1.LT.1) THEN
            II1=II1+IGU
            HLON(II) = GLON(II1,10)-360.+AMDX
          ELSE
            HLON(II) = GLON(II1,10)+AMDX
          ENDIF
          DO JJ=1,JJM
            JJ1=JJ+JWMIN-3
            HLAT(JJ)=90.-(GLAT(10,JJ1)+AMDY)
            DTT(II,JJ)=DDAT(II1,JJ1)
          END DO
        END DO

        call spline_gridCheck('MOVETX','HLON',HLON(1:IIM))
        call spline_gridCheck('MOVETX','HLAT',HLAT(1:JJM))

        CALL splie2(HLON,HLAT,size(DTT,1),DTT,IIM,JJM,size(DTT2,1),DTT2)

        DO IW1 = IWMIN1,IWMAX1
        DO JW1 = JWMIN1,JWMAX1
          IW=IW1+MDX
          JW=JW1+MDY
          IF(IW.GT.IGU) THEN
            IW = IW-IGU
            HLO = GLON(IW,JW) + 360.
          ELSEIF(IW.LT.1) THEN
            IW=IW+IGU
            HLO = GLON(IW,JW) - 360.
          ELSE
            HLO = GLON(IW,JW)
          ENDIF
          HLA = 90.-GLAT(IW,JW)
C
          CALL splin2(HLON,HLAT,size(DTT,1),DTT,size(DTT2,1),DTT2,IIM,JJM,HLO,HLA,DATT)
!!          write(6,'(a,4i4,2f10.3,3f20.8)') 'MOVETX>splin2() IW1,IW,JW1,JW,HLA,HLO, DATT =',IW1,IW,JW1,JW,HLA,HLO,
!!     &                DATG(IW,JW),DATT,DATG(IW,JW)+DATT
          DATG(IW,JW)=DATG(IW,JW)+DATT

c        DIFF=TEST(IW,JW)-DATG(IW,JW)
c        DIFF1=ABS(DIFF/(ABS(TEST(IW,JW))+1.E-15))
c        IF(DIFF1.GT.0.2)THEN
c          PRINT*,'QQQQ=',DIFF,TEST(IW,JW),DATG(IW,JW)
c        END IF
        ENDDO
        ENDDO

!!      _SHOWX_('minval(02.DATG)', (minval(DATG)) )
!!      _SHOWX_('maxval(02.DATG)', (maxval(DATG)) )
!!      _SHOWX_('   avg(02.DATG)', (   sum(DATG)/size(DATG)) )

      return
      contains

      subroutine spline_gridCheck(who,what,x)
        use m_die, only: die,perr
        implicit none
        character(len=*),intent(in):: who
        character(len=*),intent(in):: what
        real,dimension(:),intent(in):: x

        character(len=*),parameter:: myname_='MOVETX.spline_gridCheck'
        integer:: n,i,j
        logical:: valid_

        n=size(x)
        valid_ = n>=3
        if(valid_) then
          if(x(1)<x(2)) then
            i=3
            do while( valid_.and.i<n )
              valid_ = x(i-1)<x(i)
              i=i+1
            enddo
          elseif(x(1)>x(2)) then
            i=3
            do while( valid_.and.i<n )
              valid_ = x(i-1)>x(i)
              i=i+1
            enddo
          else
            valid_ = .false.
          endif
        endif

        if(.not.valid_) then
          call perr(who,'invalid spline grid '//trim(what)//', size =',n)
          do i=1,n,10
            j=min(n,i+10-1)
            write(6,'(a,i4.2,10f10.3)') trim(who)//'('//trim(what)//'): ',i-1,x(i:j)
          end do
          call die(myname_,'invalid spline grid '//trim(who)//'('//trim(what)//')')
        endif
      end subroutine spline_gridCheck

      END SUBROUTINE MOVETX

        SUBROUTINE splie2(x1a,x2a,ldy,ya,m,n,ldy2,y2a)
        use m_die, only: die, perr
        implicit none
        INTEGER ldy,ldy2
        INTEGER m,n
        REAL x1a(m),x2a(n),y2a(ldy2,n),ya(ldy,n)
        INTEGER j,k
        REAL y2tmp(max(n,m)),ytmp(max(n,m))
        character(len=*),parameter:: myname_='sig_p_convt1.splie2'
        if(m<=0.or.n<=0) return
        if(ldy<m.or.ldy2<m) then
          call perr(myname_,'bad array shape, the expected size,   m =', m)
          call perr(myname_,'actual leading sizes,               ldy =', ldy)
          call perr(myname_,'                                   ldy2 =', ldy2)
          call  die(myname_)
        endif
        do j=1,m
          do k=1,n
            ytmp(k)=ya(j,k)
          end do
          call spline(x2a,ytmp,n,1.e30,1.e30,y2tmp)
          do k=1,n
            y2a(j,k)=y2tmp(k)
          end do
        end do
        return
        END SUBROUTINE splie2

        SUBROUTINE splin2(x1a,x2a,ldy,ya,ldy2,y2a,m,n,x1,x2,y)
        use m_die, only: die, perr
        implicit none
        INTEGER ldy,ldy2
        INTEGER m,n
        REAL x1,x2,y,x1a(m),x2a(n)
        REAL y2a(ldy2,n),ya(ldy,n)
        INTEGER j,k
        REAL y2tmp(max(n,m)),ytmp(max(n,m)),yytmp(max(n,m))
        character(len=*),parameter:: myname_='sig_p_convt1.splin2'
        if(m<=0.or.n<=0) return
        if(ldy<m.or.ldy2<m) then
          call perr(myname_,'bad array shape, the expected size,   m =', m)
          call perr(myname_,'actual leading sizes,               ldy =', ldy)
          call perr(myname_,'                                   ldy2 =', ldy2)
          call  die(myname_)
        endif
        do j=1,m
          do k=1,n
            ytmp(k)=ya(j,k)
            y2tmp(k)=y2a(j,k)
          end do
          call splint(x2a,ytmp,y2tmp,n,x2,yytmp(j))
        end do
        call spline(x1a,yytmp,m,1.e30,1.e30,y2tmp)
        call splint(x1a,yytmp,y2tmp,m,x1,y)
        return
        END SUBROUTINE splin2
 
       
        SUBROUTINE splint(xa,ya,y2a,n,x,y)
        use m_die,only: die
        implicit none
        INTEGER n
        REAL x,y,xa(n),y2a(n),ya(n)
        INTEGER k,khi,klo
        REAL a,b,h
        character(len=*),parameter:: myname_='sig_p_convt1.splint'
        if(n<=0) return
        klo=1
        khi=n
   1    if((khi-klo).gt.1)then
          k=(khi+klo)/2
          if(xa(k).gt.x)then
            khi=k
          else
            klo=k
          end if
          go to 1
        end if
        h=xa(khi)-xa(klo)
        if(h.eq.0.) call die(myname_,'bad xa input in splint')
        a=(xa(khi)-x)/h
        b=(x-xa(klo))/h
        y=a*ya(klo)+b*ya(khi)+
     *    ((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
        return
        END SUBROUTINE splint

        SUBROUTINE spline(x,y,n,yp1,ypn,y2)
        implicit none
        INTEGER n
        REAL yp1,ypn,x(n),y(n),y2(n)
        INTEGER i,k
        REAL p,qn,sig,un,u(n)
        if(n<=0) return
        if(yp1.gt..99e30)then
          y2(1)=0.
          u(1)=0.
        else
          y2(1)=-0.5
          u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
        end if
        do i=2,n-1
          sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
          p=sig*y2(i-1)+2.
          y2(i)=(sig-1.)/p
          u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))
     *         /(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
        end do
        if(ypn.gt..99e30)then
          qn=0.
          un=0.
        else
          qn=0.5
          un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
        end if
        y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
        do k=n-1,1,-1
          y2(k)=y2(k)*y2(k+1)+u(k)
        end do
        return
        END SUBROUTINE spline
