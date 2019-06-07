!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_duplicate - Wraper for duplicate removal
!
! !DESCRIPTION:
!
! !INTERFACE:

      module m_duplicate

      use m_ods
      use m_ods_structure
      use m_odsmeta, only: KTMAX, KXMAX, X_DUP, ktUS, ktVS, ktUU, ktVV
      use m_Regioner,only : Regioner_init
      use m_Regioner,only : Regioner_clean
      use m_Regioner,only : Regioner_part
      use m_stdio  , only : stdout
      use m_die
!!!   use m_zeit

      implicit none

      PRIVATE

      public  DupObs

      interface  DupObs; module procedure dupobs_; end interface

! !TO DO:
!   1. Optimize Regioner_part as it is the bulk of the cost here. 
!
! !REVISION HISTORY:
!       16Feb01 - Todling  - Wraper to dupelim
!	31Oct01 - Todling  - Replaced QC's partitioner by PSAS'
!       08Jan02 - da Silva - Added [vh]Tolerance 
!EOP ___________________________________________________________________

      character(len=*), parameter :: myname = 'm_duplicate'

!     Tolerance to define a duplicate
!     NOTE: This is important because of CRAY's IEEE conversion
!     ---------------------------------------------------------
      real*8, parameter :: TOL = 1.E-4
      real*8 :: vTOL, hTOL                 ! user may change this

      contains

      subroutine dupobs_ ( data, nobs,
     &                     vTolerance, hTolerance )

      implicit none

      integer, intent(in) :: nobs
      real*8, intent(in), OPTIONAL :: vTolerance ! log(p) tolerance
      real*8, intent(in), OPTIONAL :: hTolerance ! lat/lon tolerance


      type( obs_vect ), intent(inout) :: data

      integer,allocatable,dimension(:)   :: iregbeg, ireglen
      integer,allocatable,dimension(:,:) :: lc_kt  , ln_kt
      integer,allocatable,dimension(:)   :: indx

      integer toexcl (nobs)
      integer nu, nv
      integer indxu (nobs), indxv(nobs)

      integer :: nxkt
      integer :: nxkr
      integer :: ier
      integer :: i

      real*8 :: r8_lon(nobs), r8_lat(nobs), r8_lev(nobs)

!     Tolerances for position check
!     -----------------------------
      if ( present(vTolerance) ) then
         vTOL = vTolerance
      else 
         vTOL = TOL
      end if
      if ( present(hTolerance) ) then
         hTOL = hTolerance
      else 
         hTOL = TOL
      end if


      nxkt=KTMAX
      call Regioner_init(nxkr)

      write(stdout, *) myname, ': ', count(data%qcexcl==X_DUP),
     &                            ' duplicates marks found on entry'

      allocate( indx(nobs), iregbeg(nxkr),ireglen(nxkr),
     &          lc_kt(nxkt,nxkr),ln_kt(nxkt,nxkr),stat=ier)
          if(ier/=0) call die ( myname, 'alloc(), error', ier)

!     Store original data order
!     -------------------------
      indx(1:nobs) = ( (/ (i,i=1,nobs) /) )

!!!        call zeit_ci ('Regioner')
      r8_lon(1:nobs) = data%lon(1:nobs)
      r8_lat(1:nobs) = data%lat(1:nobs)
      r8_lev(1:nobs) = data%lev(1:nobs)
      call Regioner_part ( nxkr, nxkt, iregbeg, ireglen, lc_kt, ln_kt,
     &                     nobs, r8_lat, r8_lon, r8_lev, 
     &                           data%kx , data%kt , indx )
!!!        call zeit_co ('Regioner')

!     Order exclusion flags accordingly
!     ---------------------------------
      toexcl(1:nobs) = data%qcexcl( (/ (indx(i),i=1,nobs) /) )

!     Flag out duplicate data
!     -----------------------
      call dupelim_ ( nobs, data%lat, data%lon, data%lev, data%kx, data%kt, 
     &                      toexcl, nxkr, iregbeg, ireglen )

!     Reorder data that's been shaffled according to orginal order
!     ------------------------------------------------------------
      data%kx    ( (/ (indx(i),i=1,nobs) /) ) = data%kx    (1:nobs)
      data%kt    ( (/ (indx(i),i=1,nobs) /) ) = data%kt    (1:nobs)
      data%lat   ( (/ (indx(i),i=1,nobs) /) ) = data%lat   (1:nobs)
      data%lon   ( (/ (indx(i),i=1,nobs) /) ) = data%lon   (1:nobs)
      data%lev   ( (/ (indx(i),i=1,nobs) /) ) = data%lev   (1:nobs)
           toexcl( (/ (indx(i),i=1,nobs) /) ) =      toexcl(1:nobs)

      where(data%kt/=ktUS .and. data%kt/=ktVS .and.
     &      data%kt/=ktUU .and. data%kt/=ktVV .and.
     &      toexcl==X_DUP                    ) data%qcexcl = toexcl

!     Get indices for U's and V's
!     ---------------------------
      nu = 0
      nv = 0
      do i = 1, nobs
         if(data%kt(i)==ktUS .or. data%kt(i)==ktUU)then
           nu = nu + 1
           indxu(nu) = i
         endif
         if(data%kt(i)==ktVS .or. data%kt(i)==ktVV)then
           nv = nv + 1
           indxv(nv) = i
         endif
      end do
      if(nu/=nv) call die(myname,'no. of u/v not the same')

!     The following assumes u and v are paired "side-by-side"
!     ------------------------------------------------------
      do i = 1, nu
         if (data%ks(indxu(i)) == data%ks(indxv(i)) ) then
             if(toexcl(indxu(i))==X_DUP .or. toexcl(indxv(i))==X_DUP) then
                data%qcexcl(indxu(i)) = X_DUP
                data%qcexcl(indxv(i)) = X_DUP
              end if
         end if
      end do

      deallocate ( indx, iregbeg, ireglen, lc_kt, ln_kt, stat=ier )
          if(ier/=0) call die ( myname, 'dealloc(), error', ier)

      call Regioner_clean()

      write(stdout, *) myname, ': ', count(data%qcexcl==X_DUP),
     &                            ' duplicates removed after wind check'

      return
      end subroutine dupobs_

      subroutine DUPELIM_ ( nobs, lat, lon, lev, kx, kt, qcexcl, 
     &                            maxreg,  iregbeg, ireglen )

 
      implicit none

!
!                                                   01/13/94 - dupelim.f
!  07oct94 - A. da S. - introduced tolerance (TOL) to compare lat/lon/lev
!                       this fix needed because CRAY was missing duplicates
!                       because of its IEEE conversion to 64 bits.
!  16feb01 - Todling  - obs kept as entered; qcexcl flag setting
!  15feb02 - da Silva   Changed algorithm to search all observations 
!                       whithin a region for duplicates; overal optmization
!                       with OMP directives added; verified that no
!                       duplicates are found if this routine is called
!                       twice in a row.
!
! !TO DO:
!    1) Build time check - make sure obs that get eliminated are
!       preferably those not at the current synoptic time.
!       (this should better be done in the thinner)
!    2) Make this order independent.
!
!
!.... Eliminate duplicate observations and readjust region pointers

!     Argument declarations.
!     ----------------------
      integer      nobs
      integer      kx(nobs), kxn
      integer      kt(nobs), ktn
      real         lat(nobs), latn
      real         lon(nobs), lonn
      real         lev(nobs), levn
      integer      qcexcl(nobs), qcxn
      integer      maxreg
      integer      iregbeg(maxreg)
      integer      ireglen(maxreg)
      integer      itots(maxreg)

      character(len=*), parameter :: myname_ = myname//'*dupelim'


      integer      itot
      integer      ireg
      integer      ibeg, ilen, n, n1, ilast

!     Local storage.
!     --------------
      logical      is_dup

!  input at this pt is sorted by region, type, kx, lat, lon, and level
!  scan through all regions 
!
!.......................................................................

      itots = 0

!     Loop over the regions.
!     ----------------------
!$omp parallel do
!$omp& default(shared)
!$omp& private(ireg,ibeg,ilen,ilast,n,n1,is_dup,levn,lonn,latn,qcxn,kxn,ktn)
      do ireg = 1, maxreg

!        print *, 'region: ', ireg

!        Check whether there is data in the current region.
!        --------------------------------------------------
         if( ireglen(ireg).eq.0 ) cycle

         ibeg = iregbeg(ireg)
         ilen = ireglen(ireg)
         ilast = ibeg+ilen-1

         do n = ibeg, ilast-1

            levn = lev(n)
            lonn = lon(n)
            latn = lat(n)
            qcxn = qcexcl(n)
            kxn  = kx(n)
            ktn  = kt(n)

            if ( qcxn .ne. 0 ) cycle

            do n1 = n+1, ilast

              if ( qcexcl(n1) .ne. 0 ) cycle
              if ( ktn .ne. kt(n1)   ) cycle
              if ( kxn .ne. kx(n1)   ) cycle

              is_dup = .not.
     $                  ( (abs(log(levn/lev(n1))) .gt. vTOL) .or.
     $                  (abs(lonn-lon(n1))        .gt. hTOL) .or.
     $                  (abs(latn-lat(n1))        .gt. hTOL)  )

              if( is_dup ) then 
                 qcexcl(n) = X_DUP
                 itots(ireg) = itots(ireg) + 1
                 exit
              end if
              
           end do

        end do

      end do

      itot = sum(itots)

!     Report number of duplicates found and flag them out
!     ---------------------------------------------------
      if ( itot .eq. 0 ) return

      write(stdout, *) myname_, ': ', itot,' duplicate obs found'

!     All done.
!     ---------
      return
      end subroutine dupelim_

      end module m_duplicate
