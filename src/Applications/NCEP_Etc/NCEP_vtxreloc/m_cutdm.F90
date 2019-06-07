      module m_cutdm
      implicit none
      private
      public :: cutdm_allstm
      interface cutdm_allstm; module procedure allstm_; end interface

!-----------------------------------------------------------------------
! Usage:
!   use m_cutdm,only : cutdm_alltrm
! -- define the global grid:
!   [IMAX,JMAX,KMAX,MTV]
!   [GLON,GLAT]
!   [HDATA,wdata]
! -- define the storm grid:
!   [IRX,JRX,NSTM,SLON_N(NSTM),SLAT_N(NSTM)]
! -- allocate for the result:
!   [HDAT]
! -- then, to convert data from (HDATA,wdata) to HDAT:
!   call cutdm_allstm(imax,jmax,glon,glat,kmax,mtv,HDATA,wdata, &
!     IRX,JRX,MTV2,NSTM,SLON_N,SLAT_N,HDAT)
!-----------------------------------------------------------------------
      character(len=*),parameter :: myname='m_cutdm'
      logical,parameter :: traceme_=.false.

!#define DEBUG_TRACE
#include "mytrace.H"
      contains
!-----------------------------------------------------------------------
      subroutine allstm_(imax,jmax,glon,glat,kmax,mtv,HDATA,wdata, &
     &	p_msl,IRX,JRX,MTV2,NSTM,SLON_N,SLAT_N,HDAT)
!-----------------------------------------------------------------------
!-- Interpolate storm grid data fields and store them into HDAT.

	implicit none

! Input gridded fields
	integer,intent(in) :: imax,jmax,kmax,mtv
	real,dimension(imax),intent(in) :: glon
	real,dimension(jmax),intent(in) :: glat
	real,dimension(imax,jmax,mtv),intent(in) :: HDATA
	real,dimension(imax,jmax,kmax,2),intent(in) :: wdata

! Output storm grid data fields
	real,dimension(imax,jmax),intent(out) :: p_msl
	integer,intent(in) :: IRX,JRX,MTV2,NSTM	! shape data
	real,dimension(NSTM),intent(in) :: SLON_N,SLAT_N ! locations
	real,dimension(IRX,JRX,MTV2,NSTM),intent(out) :: HDAT

!-----------------------------------------------------------------------
! local workspace variables
	character(len=*),parameter :: myname_=myname//'.allstm_'
	real,dimension(IRX,JRX) :: psfcn,pmsln,zsfcn,tsfcn,rx
	real,dimension(imax,jmax) :: hdata_lnps
	integer :: k,istm
	integer :: k_zsfc,k_psfc,k_tsfc,k_temp
	integer :: k_divr,k_vort,k_uwnd,k_vwnd
	integer :: k_qwmr
	integer :: nwrt1,nwt1
	real :: slon,slat
	real :: hmean
!-----------------------------------------------------------------------
! Define variable pointers (indices of HDATA and wdata).
        _ENTRY_

! ... I beleive that index definitions below should be moved to another
! location, such as module m_HDATA, if there is one.
	k_zsfc=1		! 2-d variable
	k_psfc=2		! 2-d variable
	k_tsfc=3		! 2-d (k=1) from 3-d temp(1:kmax)
	k_temp=k_tsfc-1		! 1 slab before k=1 of temp(1:kmax)
	k_vort=k_temp+kmax	! 2 slabs before k=1 of vort(1:kmax)
	k_divr=k_vort-1		! 2 slabs before k=1 of divr(1:kmax)
	k_qwmr=k_vort+kmax+kmax	! 1 slab before k=1 of qwmr(1:kmax)

	k_uwnd=1		! the first level of u in wdata
	k_vwnd=2		! the first level of v in wdata

!       print*,'kmax=',kmax
!	print*,'k_zsfc=',k_zsfc
!	print*,'k_psfc=',k_psfc
!	print*,'k_tsfc=',k_tsfc
!	print*,'k_temp=',k_temp
!	print*,'k_divr=',k_divr
!	print*,'k_vort=',k_vort
!	print*,'k_qwmr=',k_qwmr
!	print*,'k_uwnd=',k_uwnd
!	print*,'k_vwnd=',k_vwnd
! ...

!-----------------------------------------------------------------------
! Derive p_msl from (log_p_sfc,t_sfc,z_sfc)

	hdata_lnps(:,:)=log(1000.*exp(hdata(:,:,k_psfc)))
	call ps2pmsl(imax,jmax, hdata(:,:,k_zsfc),hdata(:,:,k_tsfc), &
     &				hdata_lnps(:,:)  ,p_msl(:,:) )
 
!-----------------------------------------------------------------------
! For all storms, work on all variables

	print'(1x,2a,i5)',myname_,'(): nstm =',nstm
	do istm=1,nstm
	  slon=slon_n(istm)
	  slat=slat_n(istm)
	  print'(1x,2a,i2,2f14.7)',myname_,'(): istm,slon,slat =', &
	    istm,slon,slat

	  print'(1x,2a,3f18.10)',myname_,'(): Hdata_zsfc =', &
	  	sum(hdata(:,:,k_zsfc))/(imax*jmax),  &
		minval(hdata(:,:,k_zsfc)),  &
		maxval(hdata(:,:,k_zsfc))

	  nwrt1=0	! reset the pointer (slab index) of HDAT
	  nwt1=0 	! Every writ1() does nwrt1++ before storing,
	  		! and nwt1++ for bean counting.  I don''t see
			! the need for either.

! Derive and store storm grid z_sfc
	  call cut_dm(imax,jmax,glon,glat,hdata(:,:,k_zsfc), &
     &	  	irx,jrx,slon,slat,zsfcn, corner=.true.)
	  print'(1x,2a,3f18.10)',myname_,'(): zsfcn =', &
	  	sum(zsfcn(:,:))/(irx*jrx),  &
		minval(zsfcn),    &
		maxval(zsfcn)
	  call writ1(irx,jrx,zsfcn,nwt1,nwrt1,mtv2,nstm,istm,HDAT)
	  print'(1x,2a,3f18.10)',myname_,'(): Hdat_zsfc =',    &
	  	sum(HDAT(:,:,1,istm))/(irx*jrx),    &
		minval(HDAT(:,:,1,istm)), &
		maxval(HDAT(:,:,1,istm))

! Derive storm grid p_msl and t_sfc.
	  call cut_dm(imax,jmax,glon,glat,p_msl(:,:), &
     &	  	irx,jrx,slon,slat,pmsln, corner=.false.)
     	  pmsln(:,:)=exp(log(pmsln(:,:)))	! needed for zero-diff

	  call cut_dm(imax,jmax,glon,glat,hdata(:,:,k_tsfc), &
     &	  	irx,jrx,slon,slat,tsfcn, corner=.false.)

! Derive storm grid log_p_sfc from (p_msl,t_sfc,z_sfc)
	  call pmsl2ps(IRX,JRX,zsfcn,tsfcn,pmsln,psfcn)

		! store log_p_sfc
	  call writ1(irx,jrx,psfcn,nwt1,nwrt1,mtv2,nstm,istm,HDAT)
	  	! store p_msl
	  call writ1(irx,jrx,pmsln,nwt1,nwrt1,mtv2,nstm,istm,HDAT)
		! store t_sfc
	  call writ1(irx,jrx,tsfcn,nwt1,nwrt1,mtv2,nstm,istm,HDAT)
 
! Derive and store storm grid upper-air temperature, temp(2:kmax).
	  do k=2,kmax
	    call cut_dm(imax,jmax,glon,glat,hdata(:,:,k_temp+k), &
     &		irx,jrx,slon,slat,rx, corner=.false.)
	    call writ1(irx,jrx,rx,nwt1,nwrt1,mtv2,nstm,istm,HDAT)
	  end do

! Derive and store storm grid wind fields, (divr,vort,uwnd,vwnd).
	  do k=1,kmax
	    call cut_dm(imax,jmax,glon,glat,hdata(:,:,k_divr+k+k), &
     &	    	irx,jrx,slon,slat,rx, corner=.false.)
	    call writ1(irx,jrx,rx,nwt1,nwrt1,mtv2,nstm,istm,HDAT)

	    call cut_dm(imax,jmax,glon,glat,hdata(:,:,k_vort+k+k), &
     &	    	irx,jrx,slon,slat,rx, corner=.false.)
	    call writ1(irx,jrx,rx,nwt1,nwrt1,mtv2,nstm,istm,HDAT)

	    call cut_dm(imax,jmax,glon,glat,wdata(:,:,k,k_uwnd), &
     &	    	irx,jrx,slon,slat,rx, corner=.false.)
	    call writ1(irx,jrx,rx,nwt1,nwrt1,mtv2,nstm,istm,HDAT)

	    call cut_dm(imax,jmax,glon,glat,wdata(:,:,k,k_vwnd), &
     &	    	irx,jrx,slon,slat,rx, corner=.false.)
	    call writ1(irx,jrx,rx,nwt1,nwrt1,mtv2,nstm,istm,HDAT)
	  end do

! Derive and store storm grid water vapor mixing ratio
	  do k=1,kmax
	    call cut_dm(imax,jmax,glon,glat,hdata(:,:,k_qwmr+k), &
     &	    	irx,jrx,slon,slat,rx, corner=.false.)
	    call writ1(irx,jrx,rx,nwt1,nwrt1,mtv2,nstm,istm,HDAT)
	  end do
	end do		! istm=1,NSTM
	_EXIT_
	end subroutine allstm_
!-----------------------------------------------------------------------
      SUBROUTINE PMSL2PS(IRX,JRX,ZN,TN,PSN,PSFCN)
! --- convert p_msl to p_sfc:

      implicit none
      integer,intent(in) :: IRX,JRX		! shape of the grid
      REAL,dimension(IRX,JRX),intent(in) :: ZN		! z_sfc
      REAL,dimension(IRX,JRX),intent(in) :: TN		! t_sfc
      REAL,dimension(IRX,JRX),intent(in) :: PSN		! p_msl (Pa)
      REAL,dimension(IRX,JRX),intent(out) :: PSFCN	! p_sfc (log_Pa)
!
	character(len=*),parameter :: myname_=myname//'.pmsl2ps'
      integer :: I,J
      real :: G,R,GAMMA
      real :: A,B,C,DD,D1
      _ENTRY_

      G = 9.8
      R = 287.05
      GAMMA = 6.7*0.001
!
!.. Using interpolated MSLP, Make surface pressure
!
      DO I=1,IRX 
      DO J=1,JRX
      A = (GAMMA * ZN(I,J)) / TN(I,J)
      B = ALOG(1+A)
      C = (G*B)/(R*GAMMA)
      DD = ALOG(PSN(I,J)) - C
      D1 = EXP(DD)/1000.
      PSFCN(I,J) = ALOG(D1)
      ENDDO
      ENDDO
!
      _EXIT_
      END SUBROUTINE PMSL2PS
!-----------------------------------------------------------------------
      SUBROUTINE PS2PMSL(IMAX,JMAX,ZG,T1,PSFC,PMSL)
!
      implicit none
      integer,intent(in) :: IMAX,JMAX	! shape of 2-d variables below
      real,dimension(IMAX,JMAX),intent(in) :: ZG	! z_sfc
      real,dimension(IMAX,JMAX),intent(in) :: T1	! t_sfc
      real,dimension(IMAX,JMAX),intent(in) :: PSFC	! p_sfc (log_Pa)
      real,dimension(IMAX,JMAX),intent(out) :: PMSL	! p_msl (Pa)

	character(len=*),parameter :: myname_=myname//'.ps2pmsl'
      real :: G,R,GAMMA
      real :: A,B,C
      integer :: I,J
      _ENTRY_

      G = 9.8
      R = 287.05
      GAMMA = 6.7*0.001

	! T1: t_sfc
	! ZG: z_sfc
	! PSFC: p_sfc in log_Pa
	! PMSL: p_msl in Pa

      DO I=1,IMAX
      DO J=1,JMAX
      A = (GAMMA * ZG(I,J)) / T1(I,J)
      B = ALOG(1+A)
      C = (G*B)/(R*GAMMA)
      PMSL(I,J) = exp(PSFC(I,J) + C)
      ENDDO
      ENDDO
      _EXIT_
      END SUBROUTINE PS2PMSL
!-----------------------------------------------------------------------
      SUBROUTINE CUT_DM(IMAX,JMAX,GLON,GLAT,OLD, &
      			 IRX, JRX,SLON,SLAT,NEW,corner)
      		! The last argument IV is replaced by flag corner
		! for corner = IV==2.

      use m_die, only: die
      use m_vtxgrid, only: vtxgrid_lonLower
      use m_vtxgrid, only: vtxgrid_lonDiff
      use m_vtxgrid, only: vtxgrid_lonModulo
      implicit none
      integer,intent(in) :: IMAX,JMAX
      real,dimension(imax),intent(in) :: GLON
      real,dimension(jmax),intent(in) :: GLAT
      real,dimension(imax,jmax),intent(in) :: OLD
      logical,intent(in) :: corner

      integer,intent(in) :: IRX,JRX
      real   ,intent(in) :: SLON,SLAT
      REAL,dimension(IRX,JRX),intent(out) :: NEW

	character(len=*),parameter :: myname_=myname//'.cut_dm'
      integer :: I,J
      real :: X,BLA,BLO
      integer, parameter:: RK=kind(BLO)
      real   , parameter:: R360 = 360._RK

      integer :: IG,IX,IXp1
      real :: DON,DX
      integer :: JG,IY
      real :: GLA,DAT,DY,Y
      real :: DD1,DD2,DD3,DD4
      real :: X1,X2,XX
      real :: Y1,Y2,YY

      _ENTRY_
! Loop over all storm grid points (IRX,JRX)
      X=360./FLOAT(IMAX)        ! This statement assumes that the longitude grid
                                ! GLON(1:IMAX) is equally spaced in degree, and periodic.
      DO J=1,JRX
      BLA = 90. - SLAT - (J-1)
      DO I=1,IRX
      BLO = SLON + (I-1)        ! BLO-grid is 1-degree equally spaced.
        ! It is assumed here, that GLON(1)<GLON(IMAX), in degree-longitude.
        ! So if BLO is on the left of GLON(1) to begin with, one must to
        ! consider the periodic nature of the longitude grid.
        ! This is, BLO=GLON(1)+modulo(BLO-GLON(1),360.)
      if (BLO<GLON(1).or.BLO>=GLON(1)+R360) BLO=vtxgrid_lonModulo(BLO,reflon=GLON(1))

! Search for storm lon-grid points in GLON(:)
      IX=-1
      DO IG=IMAX,1,-1
      DON = BLO - GLON(IG)
      IF (DON.GE.0) THEN
      DX = DON
      IX = IG
      exit
      ENDIF
      ENDDO ! IG=IMAX,1,-1
      IG = vtxgrid_lonLower(GLON(1:IMAX),BLO)
      if(IX/=IG) &
        print '(a,2i6,2f10.4)', 'IX,IG,DX,... =',IX, IG-IX, DX, vtxgrid_lonDiff(GLON(IG),BLO)-DX
      IX = IG
      DX = vtxgrid_lonDiff(GLON(IG),BLO)

      if(IX<=0) then
        write(6,'(2a,x,i10  )') myname_,'() >>> ERROR <<< unlocated grid point, IX =',IX
        write(6,'(2a,x,i10  )') myname_,'() >>> ERROR <<<                     IMAX =',IMAX
        write(6,'(2a,x,f10.5)') myname_,'() >>> ERROR <<<                      BLO =',BLO
        write(6,'(2a,x,f10.5)') myname_,'() >>> ERROR <<<               GLON(IMAX) =',GLON(IMAX)
        write(6,'(2a,x,f10.5)') myname_,'() >>> ERROR <<<               GLON(   1) =',GLON(1)
        call die(myname_)
      endif
        ! Assign the index to the grid point next to IX.  IXp1=1 is if IX==IMAX.
      IXp1=1
      if(IX<IMAX) IXp1=IX+1

! search for storm lat-grid points in GLAT(:)
      IY=-1
      DO JG=JMAX,1,-1
      GLA = 90 - GLAT(JG)
      DAT = BLA - GLA
      IF (DAT.GE.0) THEN
      DY = DAT
      IY = JG
      Y = GLAT(JG)-GLAT(JG+1)
      exit
      ENDIF
      END DO	! JG=JMAX,1,-1
      if(IY<=0) then
        write(6,'(2a,x,i10  )') myname_,'() >>> ERROR <<< unlocated grid point, IY =',IY
        write(6,'(2a,x,i10  )') myname_,'() >>> ERROR <<<                     JMAX =',JMAX
        write(6,'(2a,x,f10.5)') myname_,'() >>> ERROR <<<                      BLA =',BLA
        write(6,'(2a,x,f10.5)') myname_,'() >>> ERROR <<<               GLAT(JMAX) =',GLAT(JMAX)
        write(6,'(2a,x,f10.5)') myname_,'() >>> ERROR <<<               GLAT(   1) =',GLAT(1)
        call die(myname_)
      endif
      if(IY>=JMAX) then
        write(6,'(2a,x,i10  )') myname_,'() >>> ERROR <<< out of its range, IY =',IY
        write(6,'(2a,x,i10  )') myname_,'() >>> ERROR <<<                 JMAX =',JMAX
        write(6,'(2a,x,f10.5)') myname_,'() >>> ERROR <<<                  BLA =',BLA
        write(6,'(2a,x,f10.5)') myname_,'() >>> ERROR <<<           GLAT(JMAX) =',GLAT(JMAX)
        write(6,'(2a,x,f10.5)') myname_,'() >>> ERROR <<<           GLAT(   1) =',GLAT(1)
        call die(myname_)
      endif

! Special interpolation near corners.
      IF (corner) THEN
        DD1 = SQRT(DX**2.+DY**2.)
        DD2 = SQRT(DX**2.+(Y-DY)**2.)
        DD3 = SQRT((X-DX)**2.+DY**2.)
        DD4 = SQRT((X-DX)**2.+(Y-DY)**2.)
        IF(DD1.LE.0.2) THEN
          NEW(I,J) = OLD(IX  ,IY  )
	  cycle
        ENDIF
        IF(DD2.LE.0.2) THEN
          NEW(I,J) = OLD(IX  ,IY+1)
	  cycle
        ENDIF
        IF(DD3.LE.0.2) THEN
          NEW(I,J) = OLD(IXp1,IY  )
	  cycle
        ENDIF
        IF(DD4.LE.0.2) THEN
          NEW(I,J) = OLD(IXp1,IY+1)
	  cycle
        ENDIF
      ENDIF 

! Interpolation algorithm is implemented as it is, although it seems
! that this implementation uses the same expression twice then divided
! it by 2.  I assume there is a good reason behind this.

      X1 = ( DY*OLD(IX  ,IY+1) + (Y-DY)*OLD(IX  ,IY  ) ) / Y
      X2 = ( DY*OLD(IXp1,IY+1) + (Y-DY)*OLD(IXp1,IY  ) ) / Y
      Y1 = ( DX*OLD(IXp1,IY  ) + (X-DX)*OLD(IX  ,IY  ) ) / X
      Y2 = ( DX*OLD(IXp1,IY+1) + (X-DX)*OLD(IX  ,IY+1) ) / X
      XX = (DX*X2 + (X-DX)*X1)/X 
      YY = (DY*Y2 + (Y-DY)*Y1)/Y
      NEW(I,J) = (XX+YY)/2.

      END DO	! I=1,IRX
      END DO	! J=1,JRX
      _EXIT_
      END SUBROUTINE CUT_DM
!-----------------------------------------------------------------------
      end module m_cutdm
