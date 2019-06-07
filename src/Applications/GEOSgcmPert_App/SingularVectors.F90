! $Id$

#define I_AM_MAIN

#include "MAPL_Generic.h"

Program SingularVectors

   use ESMF
   use MAPL_Mod
   use MAPL_CFIOMod

   use GEOS_AgcmPertGridCompMod, only:  ROOT_SetServices => SetServices
   use GEOS_PertSharedMod, only: ak=>pert_ak, bk=>pert_ak, BothPhases

   use fv_mp_mod, only: mp_reduce_sum
   use fv_timing_mod, only: timing_clear

   use svecs_utils
   use svecs_cf
   use svecs_proj
   use svecs_weights
   use svecs_norms
   use svecs_tlmadm

   use tapenade_iter, only: cp_iter_controls, finalize_cp_iter

   implicit none

   integer           :: STATUS
   character(len=18) :: Iam="SingularVectors"

   call PERT_CAP(ROOT_SetServices, rc=STATUS)
   VERIFY_(STATUS)

   if( STATUS.eq.0 ) then
       close(99)
       open (99,file='EGRESS',form='formatted')
       close(99)
   endif

   call exit(0)

 contains

#undef MAPL_ErrLog_DONE
#undef I_AM_MAIN
#include "MAPL_Generic.h"

  subroutine PERT_CAP(ROOT_SetServices, Name, AmIRoot, RC)

    external                             :: ROOT_SetServices
    character*(*), optional, intent(IN ) :: Name
    logical,       optional, intent(OUT) :: AmIRoot
    integer,       optional, intent(OUT) :: rc

! Handles to the CAP's Gridded Components GCs
! -------------------------------------------

   integer                      :: ROOT
   integer                      :: FHIST
   integer                      :: BHIST
   character(len=ESMF_MAXSTR)   :: ROOT_NAME

! A MAPL object for the cap
!--------------------------

   type(MAPL_MetaComp)          :: MAPLOBJ
   type(MAPL_MetaComp), pointer :: CHILD_MAPL

! The children's GCs and IM/Ex states
!------------------------------------

   type(ESMF_GridComp), pointer :: GCS(:)
   type(ESMF_State),    pointer :: IMPORTS(:)
   type(ESMF_State),    pointer :: EXPORTS(:)

! ESMF stuff
!-----------

   type(ESMF_VM)                :: VM
   type(ESMF_Config)            :: config
   type(ESMF_Config)            :: cf
   type(ESMF_Clock)             :: clock

! ErrLog variables
!-----------------

   integer                      :: STATUS
   character(len=ESMF_MAXSTR)   :: Iam="SingularVectorsPertCap"
   logical                      :: AmIRoot_

! TLM/ADM locals
!------------

   integer                      :: CallsPerWindow
   character(len=ESMF_MAXSTR)   :: DYNV_FILE
   type(ESMF_Grid)              :: LLPERTgrid
   type(ESMF_FieldBundle)       :: LLPertBundle
   type(ESMF_FieldBundle)       :: X0PertBundle
   type(ESMF_Time)              :: startTime
   type(ESMF_Time)              :: startTime_tlm
   integer                      :: IM_WORLD, JM_WORLD, LM_WORLD

! SV things
! ---------

   type(ESMF_FieldBundle)       :: TrajBundleI, TrajBundleF
   type(ESMF_Field)             :: Field
   character(len=ESMF_MAXSTR) :: svecrc
   integer :: dodottest
   real(8) :: xdot(4)
   real(ESMF_KIND_R8), pointer  :: centerX(:,:)
   real(ESMF_KIND_R8), pointer  :: centerY(:,:)
   character(len=ESMF_MAXSTR)   :: traj_fileI, traj_fileF, lstvars
   character(len=ESMF_MAXSTR), parameter   :: attrName = MAPL_BundleItemOrderList
   character(len=ESMF_MAXSTR)              :: fieldName
   character(len=ESMF_MAXSTR), allocatable :: currList(:)
   integer                                 :: nn, nv, natt, fieldIndex,dimcount
   integer                                 :: i, l
   type(trajPointer)                       :: xtrajI
   type(trajPointer)                       :: xtrajF
   real(8)                                 :: SumWeights
   type(pertState)                         :: xpert, xpertcheck
   type(ESMF_FieldBundle)       :: SvecBundle   
   character(len=ESMF_MAXSTR)   :: svecfilename
   type(MAPL_CFIO)              :: cfio


   integer              :: comm         !Comm for Lanczos
   integer              :: n            ! number of independent (initial state) variables
   integer              :: m            ! DUMMY 
   integer              :: nzvecsize    ! size of z vector 
   real(8)              :: nzvecsizetot ! size of z vector 
   real(8)              :: coefftpItot
   real(8)              :: coefftpFtot
   integer              :: nev, ncv, nconv
   integer              :: ido
   integer              :: lworkl
   integer              :: info
   integer              :: ierr
   integer              :: ipntr(11)
   integer              :: lanmax
   integer              :: lanstrt
   integer              :: itercount
   character*2          :: which
   character*4          :: what
   real                 :: asigma       ! shifts for ARPACK (when applied)
   real(8), allocatable :: Eval(:)      ! singular values
   real(8), allocatable :: Lvec(:,:)    ! singular vectors
   real(8), allocatable :: errbnd(:)    ! error bounds for singular values
   real(8), allocatable :: resid(:)     ! all residuals
   real(8), allocatable :: workd(:)     ! workspace for reverse communication
   real(8), allocatable :: workl(:)     ! internal ARPACK workspace
   logical, allocatable :: select(:)    ! specify how many/which Ritz 
                                        !   vectors to calculate
 

! Initialize the TLM/ADM modeling environment
! -------------------------------------------
   call tlmadm_init( ROOT_SetServices,Name,AmIRoot,rc,&
                     ROOT,FHIST,BHIST,ROOT_NAME,MAPLOBJ,CHILD_MAPL,&
                     GCS,IMPORTS,EXPORTS,VM,CONFIG,cf,clock,&
                     CallsPerWindow,DYNV_FILE,LLPERTgrid,LLPertBundle,X0PertBundle,startTime,&
                     IM_WORLD,JM_WORLD,LM_WORLD)
    
   startTime_tlm = startTime

! Get local grid dimensions
! -------------------------
   call ESMF_GridGetCoord (LLPERTgrid, coordDim=1, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=centerX, rc=status)
   VERIFY_(STATUS)
   call ESMF_GridGetCoord (LLPERTgrid, coordDim=2, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=centerY, rc=status)
   VERIFY_(STATUS)

   !These are in svecs_cf
   IM  = size(centerX,1)
   JM  = size(centerX,2)
   LM  = LM_WORLD
   npx = IM_WORLD
   npy = JM_WORLD
   npz = LM_WORLD
   nq  = 4 !Number of tracers (q,qi,ql,o3)

! Allocate configuration
! ---------------------

   call allocate_config

! Fill grid arrays
! ----------------

   !Grid in degrees
   lons(1:im) = centerX(:,1)*180.0/MAPL_PI
   lats(1:jm) = centerY(1,:)*180.0/MAPL_PI
   do l = 1,lm
      levs(l) = l
   enddo

   lambda(1:im) = centerX(:,1)
   theta(1:jm) = centerY(1,:)

   !Local start end indices
   call GridFirstLast (LLPERTgrid,ifirst,ilast,jfirst,jlast)

! Create a bundle to hold the lat-lon trajectory and read it
! ----------------------------------------------------------

  !Need traj for Intitial and Final time norms
  traj_fileI = 'progI.eta.nc4'
  traj_fileF = 'progF.eta.nc4'
  lstvars = 'u,v,tv,delp,sphu,ozone,qitot,qltot'
  TrajBundleI = ESMF_FieldBundleCreate ( name='TrajectoryI Pert Bundle', __RC__)
  call ESMF_FieldBundleSet ( TrajBundleI, grid=LLPERTgrid, __RC__)
  call MAPL_CFIORead(traj_fileI, startTime, TrajBundleI, only_vars=lstvars, __RC__ )

  TrajBundleF = ESMF_FieldBundleCreate ( name='TrajectoryF Pert Bundle', __RC__)
  call ESMF_FieldBundleSet ( TrajBundleF, grid=LLPERTgrid, __RC__)
  call MAPL_CFIORead(traj_fileF, startTime, TrajBundleF, only_vars=lstvars, __RC__ )

! Get Fortran pointers to fields at intital time
! ----------------------------------------------
    call ESMF_AttributeGet(TrajBundleI, NAME=attrName, itemcount=natt, RC=STATUS)
    VERIFY_(STATUS)

    allocate(currList(natt), stat=status)
    VERIFY_(STATUS)

    !Get a list of fields in the bundle
    call ESMF_AttributeGet(TrajBundleI, NAME=attrName, VALUELIST=currList, rc=status)
    VERIFY_(STATUS)

    do nn = 1,natt
       if (MAPL_AM_I_ROOT()) print*, 'Fields in bundle: ', trim(currList(nn))
       if (trim(currList(nn))=='delp') then
           fieldIndex = nn

           fieldName = currList(fieldIndex)
           call ESMF_FieldBundleGet(TrajBundleI, fieldIndex, Field, rc=status)
           VERIFY_(STATUS)
       
           call ESMF_FieldGet(field, dimcount=dimcount, rc=status)
           VERIFY_(STATUS)
       
           !Get fortran pointer to delp field
           call ESMF_FieldGet(field, localDE=0, farrayPtr=xtrajI%delp, rc=status)
           VERIFY_(STATUS)
       endif
       if (trim(currList(nn))=='sphu') then
           fieldIndex = nn

           fieldName = currList(fieldIndex)
           call ESMF_FieldBundleGet(TrajBundleI, fieldIndex, Field, rc=status)
           VERIFY_(STATUS)
       
           call ESMF_FieldGet(field, dimcount=dimcount, rc=status)
           VERIFY_(STATUS)
       
           !Get fortran pointer to delp field
           call ESMF_FieldGet(field, localDE=0, farrayPtr=xtrajI%sphu, rc=status)
           VERIFY_(STATUS)
       endif
       if (trim(currList(nn))=='tv') then
           fieldIndex = nn

           fieldName = currList(fieldIndex)
           call ESMF_FieldBundleGet(TrajBundleI, fieldIndex, Field, rc=status)
           VERIFY_(STATUS)
       
           call ESMF_FieldGet(field, dimcount=dimcount, rc=status)
           VERIFY_(STATUS)
       
           !Get fortran pointer to delp field
           call ESMF_FieldGet(field, localDE=0, farrayPtr=xtrajI%t, rc=status)
           VERIFY_(STATUS)
       endif

    enddo

    deallocate(currList)


! Get Fortran pointers to fields at intital time
! ----------------------------------------------
    call ESMF_AttributeGet(TrajBundleF, NAME=attrName, itemcount=natt, RC=STATUS)
    VERIFY_(STATUS)

    allocate(currList(natt), stat=status)
    VERIFY_(STATUS)

    !Get a list of fields in the bundle
    call ESMF_AttributeGet(TrajBundleF, NAME=attrName, VALUELIST=currList, rc=status)
    VERIFY_(STATUS)

    do nn = 1,natt
       if (MAPL_AM_I_ROOT()) print*, 'Fields in bundle: ', trim(currList(nn))
       if (trim(currList(nn))=='delp') then
           fieldIndex = nn

           fieldName = currList(fieldIndex)
           call ESMF_FieldBundleGet(TrajBundleF, fieldIndex, Field, rc=status)
           VERIFY_(STATUS)
       
           call ESMF_FieldGet(field, dimcount=dimcount, rc=status)
           VERIFY_(STATUS)
       
           !Get fortran pointer to delp field
           call ESMF_FieldGet(field, localDE=0, farrayPtr=xtrajF%delp, rc=status)
           VERIFY_(STATUS)
       endif
       if (trim(currList(nn))=='sphu') then
           fieldIndex = nn

           fieldName = currList(fieldIndex)
           call ESMF_FieldBundleGet(TrajBundleF, fieldIndex, Field, rc=status)
           VERIFY_(STATUS)
       
           call ESMF_FieldGet(field, dimcount=dimcount, rc=status)
           VERIFY_(STATUS)
       
           !Get fortran pointer to delp field
           call ESMF_FieldGet(field, localDE=0, farrayPtr=xtrajF%sphu, rc=status)
           VERIFY_(STATUS)
       endif
       if (trim(currList(nn))=='tv') then
           fieldIndex = nn

           fieldName = currList(fieldIndex)
           call ESMF_FieldBundleGet(TrajBundleF, fieldIndex, Field, rc=status)
           VERIFY_(STATUS)
       
           call ESMF_FieldGet(field, dimcount=dimcount, rc=status)
           VERIFY_(STATUS)
       
           !Get fortran pointer to delp field
           call ESMF_FieldGet(field, localDE=0, farrayPtr=xtrajF%t, rc=status)
           VERIFY_(STATUS)
       endif

    enddo

    deallocate(currList)


! Allocate the holder for the tlm/adm state
! -----------------------------------------

  call allocate_xpert(xpert)
  call allocate_xpert(xpertcheck)

! Get comm from VM
! ----------------

  call ESMF_VMGet(vm, mpiCommunicator=COMM, rc=status)


! Initialize options, model, and set the first guess of the control variables
! ---------------------------------------------------------------------------
  what   = ' LSV'                               ! problem identifier 
  ncalls = 0                                    ! initialize lanczos counter

  ! Get number of the independent and dependent variables
  call numvars( n, m, IM, JM, LM ) !dh, check this for cnop

  !Resource file for setting svec config
  svecrc = 'svec.rc'

  if (MAPL_AM_I_ROOT()) print*, ''
  if (MAPL_AM_I_ROOT()) print*, 'Getting the configuration from ', trim(svecrc)
  if (MAPL_AM_I_ROOT()) print*, '--------------------------------------'
  if (MAPL_AM_I_ROOT()) print*, ''

  call SetSvecs(svecrc)
  if ( isolve .eq. 1 ) call SetSvecs_ARPACK( svecrc, nev, ncv, which )
  if ( isolve .eq. 2 ) call SetSvecs_NAG   ( svecrc, lanmax )
  if ( isolve .eq. 3 ) call SetSvecs_CNOP  ( svecrc, what, ncv )

  if (MAPL_AM_I_ROOT()) print*, ''
  if (MAPL_AM_I_ROOT()) print*, 'Getting the projections'
  if (MAPL_AM_I_ROOT()) print*, '-----------------------'
  if (MAPL_AM_I_ROOT()) print*, ''

  !Initialize norm/projector
  call proj_init(svecrc)

! Intialize area and level weighting
! ----------------------------------

  if (MAPL_AM_I_ROOT()) print*, ''
  if (MAPL_AM_I_ROOT()) print*, 'Creating the grid weighting'
  if (MAPL_AM_I_ROOT()) print*, '---------------------------'
  if (MAPL_AM_I_ROOT()) print*, ''

  ptop = dble(ak(0))
  call compute_weights(svecrc,xtrajI,xtrajF)

  !Check sum of weights
  sumweights = sum(gridweightI)
  call mp_reduce_sum(sumweights)
  if (MAPL_AM_I_ROOT()) then
     print*, ' '
     print*, 'Check on the sum of weights for intitial time:'
     print*, 'Sum weights = ', sumweights
  endif
  sumweights = sum(gridweightF)
  call mp_reduce_sum(sumweights)
  if (MAPL_AM_I_ROOT()) then
     print*, 'Check on the sum of weights for final time:'
     print*, 'Sum weights = ', sumweights
     print*, ' '
  endif

  if ( isolve .ne. 3 ) then
       call norm_zvecsize ( nzvecsize ) ! determine z vector size 
  else
       nzvecsize = n  ! needs revision when SPMD
  endif

  nzvecsizetot = nzvecsize
  coefftpItot = sum(coefftpI)
  coefftpFtot = sum(coefftpF)
  call mp_reduce_sum(nzvecsizetot)
  call mp_reduce_sum(coefftpItot)
  call mp_reduce_sum(coefftpFtot)
  if (MAPL_AM_I_ROOT()) then
     print*, 'Check on length of z:'
     print*, 'nzvecsize = ', nint(nzvecsizetot)
     print*, 'sum coefftpItot = ', coefftpItot
     print*, 'sum coefftpFtot = ', coefftpFtot
     print*, ' '
  endif

  !Starting iteration for Lanczos
  lanstrt = 0
  if( isolve == 2 ) ncv = min( lanmax, lanstrt+maxitr )
  if( isolve <= 2 ) then
     if( MAPL_AM_I_ROOT() ) print*, 'Starting iteration for Lanczos= ', lanstrt
  endif


! ENTERING lanc_ARPACK
! --------------------

  !Allocate workspace for eigenpairs
  allocate ( Eval(ncv), Lvec(nzvecsize,ncv), errbnd(ncv) )
  Eval = 0.0_8
  Lvec = 0.0_8
  errbnd = 0.0_8

  !Allocate workspace for eigen-decomposition
  lworkl = ncv*(ncv+8)
  allocate ( resid(nzvecsize), workd(3*nzvecsize), workl(lworkl) )
  resid = 0.0_8
  workd = 0.0_8
  workl = 0.0_8

   ido  = 0
   info = 0
   if(MAPL_AM_I_ROOT()) print*, ': Entering SV/ARPACK Loop ...'

   itercount = 0
   dodottest = 1

   !call replicate_pole_points(xpert)
   !return

   startTime = startTime_tlm

   BothPhases = .true.

   do while ( (ido==0 .or. ido==-1 .or. ido==1) .and. (ncalls<maxitr+lanstrt) )

      itercount = itercount + 1

      if (MAPL_AM_I_Root()) print*, '------------------------------'
      if (MAPL_AM_I_Root()) print*, 'Iteration Number: ', itercount
      if (MAPL_AM_I_Root()) print*, '------------------------------'

      !Repeatedly call Lanczos and take actions indicated by parameter IDO until
      !either convergence is achieved or maximum number of iterations has been exceeded.

      if (nzvecsize > 0) then
         !Only call Lanczos if the norm is active for the geographical region 
         !that this processor is responsible for.

         if (MAPL_AM_I_Root()) print*, ' '
         if (MAPL_AM_I_Root()) print*, ' Call arpack Lanczos routine'
         if (MAPL_AM_I_Root()) print*, ' '

         call pdsaupd ( comm, ido, bmat, nzvecsize, which, nev, tol, resid, &
                        ncv, Lvec, nzvecsize, iparam, ipntr, &
                        workd, workl, lworkl, info )
      endif

      !If not obtained convergence on requested number of evecs
      if ( ido /= 99 ) then

         !The following code performs:
         !znew = [ PI^T EI^(-T/2) M^T P^T EF^(T/2) EF^(1/2) P M EI^(-1/2) PI ] zold
   
         !PI       : Start time projection (z to xpert)
         !EI^(-1/2): Inverse square root of initial time norm
         !M        : Tangent linear model
         !PF       : Final time projection
         !EF^(1/2) : Square root of final time norm
         !EF^(T/2) : Adjoint of square root of final time norm
         !PF^T     : Adjoint of final time projection
         !M^T      : Adjoint of model
         !EI^(-T/2): Adjoint of inverse square root of initial time norm 
         !PI^T     : Adjoint of start time projection (xpert to z)

         if (MAPL_AM_I_Root()) print*, ' '
         if (MAPL_AM_I_Root()) print*, ' Computing S '
         if (MAPL_AM_I_Root()) print*, ' '

         !Convert z to xpert
         call proj_start_z2xpert(workd(ipntr(1):ipntr(1)+nzvecsize-1),xpert,nzvecsize)

         if (dodottest==1) then
            xpertcheck = xpert
         endif

         !EI^(-1/2)
         call svec_norm_a(xpert)

         !Convert T to Tv
         call t2tv(xpert,xtrajI)

         !Put xpert variables into ESMF imports for TLM
         call xpert_to_state(xpert,IMPORTS,ROOT)

         !M, call the tangent linear model
         call cycle_tlm( rc,ROOT,FHIST,GCS,IMPORTS,EXPORTS,VM,CLOCK, &
                            CallsPerWindow, DYNV_FILE, LLPERTgrid, LLPertBundle, startTime)

         !Get exports and put into xpert
         call state_to_xpert(EXPORTS,xpert,ROOT)

         !Convert Tv to T
         call tv2t(xpert,xtrajF)

         !P projection operator
         call proj_final_xpert2xpert(xpert)

         !EF^(1/2)
         call svec_norm_bc1(xpert)

         if (dodottest==1) then
            call svec_norm_a(xpertcheck)
            call svec_norm_a_inv(xpertcheck)
            call dot_prod_xx(xpertcheck,xpertcheck,xdot(4))
            call dot_prod_xx(xpert,xpert,xdot(2))
         endif

         if (MAPL_AM_I_Root()) print*, ' '
         if (MAPL_AM_I_Root()) print*, ' Computing S^T '
         if (MAPL_AM_I_Root()) print*, ' '

         !EF^(T/2)
         call svec_norm_bc2(xpert)

         !P^T projection operator
         call proj_final_xpert2xpert(xpert)

         !Adjoint of convert Tv to T
         call tv2t_ad(xpert,xtrajF)

         !Move xpert to adjoint imports
         call xpert_to_state(xpert,IMPORTS,ROOT)

         !M^T, call the adjoint model 
         call cycle_adm(rc,ROOT,BHIST,GCS,IMPORTS,EXPORTS,VM,CLOCK,CallsPerWindow,X0PertBundle)

         !Move adjoint exports into xpert
         call state_to_xpert(EXPORTS,xpert,ROOT)

         !Adjoint of convert T to Tv
         call t2tv(xpert,xtrajI)

         !EI^(-T/2)
         call svec_norm_d(xpert)

         !Go back to z
         call proj_start_xpert2z(workd(ipntr(2):ipntr(2)+nzvecsize-1),xpert,nzvecsize)

         if (dodottest==1) then
            call dot_prod_zz(workd(ipntr(1):ipntr(1)+nzvecsize-1),workd(ipntr(1):ipntr(1)+nzvecsize-1),nzvecsize,xdot(1))
            call dot_prod_zz(workd(ipntr(1):ipntr(1)+nzvecsize-1),workd(ipntr(2):ipntr(2)+nzvecsize-1),nzvecsize,xdot(3))
            if (MAPL_AM_I_Root()) then
               print *
               print *, ' Dot product test...'
               print *
               print *, ' (x,x)     = ',  xdot(4), '(Sum of the squares = 1)'
               print *, ' (z,z)     = ',  xdot(1), '(Sum of the squares = 1)'
               print *, ' (Sz,Sz)   = ',  xdot(2)
               print *, ' (S''Sz,z)  = ', xdot(3)
               if(abs(xdot(2)>0.d0)) &
               print *, ' Rel error = ', abs(xdot(2)-xdot(3))/xdot(2)
               print *
           endif
         endif

         call timing_clear

      endif

      if (MAPL_AM_I_Root()) print*, ' Number of converged eigenvectors = ', iparam(5)
      if (MAPL_AM_I_Root()) print*, ' ido: ', ido
      if (MAPL_AM_I_Root()) print*, ' '
      if (MAPL_AM_I_Root()) print*, ' '

   end do

   if (cp_iter_controls%cp_i .ne. 0) call finalize_cp_iter

  if ( info<0 ) then
 
     ! Error message. Check the documentation in pdsaupd
     if (MAPL_AM_I_Root()) print*, 'Error with _saupd, info = ', info
     return

  else

     nconv =  iparam(5)

     if (MAPL_AM_I_Root()) print*, 'Final number of converged eigenvectors = ', iparam(5)

     !Make eval convergence info (error bounds) available for output
     errbnd(1:nconv) = workl(3*ncv+1:3*ncv+nconv)
     allocate ( select(ncv))

     ierr = 0

     call pdseupd( comm, rvec, 'A', select, Eval, Lvec, nzvecsize, asigma, &
                   bmat, nzvecsize, which, nev, tol, resid, ncv, Lvec, &
                   nzvecsize, iparam, ipntr, workd, workl, lworkl, ierr )

     if ( ierr/=0 ) then
      
        !Error condition: Check the documentation of SSEUPD. |
        if (MAPL_AM_I_Root()) print*, 'Error in _seupd, info ',ierr
      
     else if ( eigchk ) then
      
        !call chk_svec ( nzvecsize, ncv, nconv, Eval, Lvec )
      
     end if

     deallocate(select)

  endif


  !Write every converged singular vector to file
  do nn = 1,iparam(5)

     !arpack returns in ascending order
     nv = iparam(5) - nn + 1

     !TLM has just run so rewind the clock
     if (nn > 1) then
        DO I=1, CallsPerWindow
           call ESMF_ClockSet( CLOCK, direction=ESMF_DIRECTION_REVERSE, RC=status )
           VERIFY_(STATUS)
           call ESMF_ClockAdvance ( CLOCK, RC=STATUS )
           VERIFY_(STATUS)
        enddo
     endif

     startTime = startTime_tlm
 
     if (MAPL_AM_I_ROOT()) print*, ' '
     if (MAPL_AM_I_ROOT()) print*, 'Writing Singular vector with singular value:'
     if (MAPL_AM_I_ROOT()) print*, 'lambda = ', eval(nv)
     if (MAPL_AM_I_ROOT()) print*, ' '

     !Create xpert structure containing converged svec
     call proj_start_z2xpert(Lvec(:,nv),xpert,nzvecsize)
     call svec_norm_a(xpert)
     call t2tv(xpert,xtrajI)

     !Put in an ESMF State
     call xpert_to_state(xpert,IMPORTS,ROOT)

     !Now put in a bundle
     call ESMFL_State2Bundle(IMPORTS(ROOT),LLPertBundle)

     !Create a new bundle for converged SVECS 
     SvecBundle = ESMF_FieldBundleCreate ( name='Svec bundle', __RC__)
     call ESMF_FieldBundleSet ( SvecBundle, grid=LLPERTgrid, __RC__) 

     !Get info for the bundle from a similar file
     call MAPL_CFIORead (DYNV_FILE,  startTime, SvecBundle, noread=.true., __RC__ )

     !Move xpert from one bundle to another changing variable names
     call BundleInt2Ext (0,bk,LLpertBundle,Svecbundle)

     !Filename for converged singular vector
     write (svecfilename, "(A4,I3.3,A8)") "svec", nn, ".eta.nc4"

     !Create the file to write to
     call MAPL_CFIOCreate ( cfio, trim(svecfilename), clock, SvecBundle, DESCR='Write Converged Svec', EXPID='junk', __RC__ )
     call MAPL_CFIOSet ( cfio, Root=1, __RC__ )

     !Write the singular vector to the file
     call MAPL_CFIOWrite ( cfio, clock, SvecBundle, verbose = .true., __RC__ )

     !Deallocate the cfio
     call MAPL_cfioDestroy ( cfio )

     if (propsvec) then

        if (MAPL_AM_I_ROOT()) print *
        if (MAPL_AM_I_ROOT()) print*, 'Evolving singular vector'

        call xpert_to_state(xpert,IMPORTS,ROOT)
        call cycle_tlm( rc,ROOT,FHIST,GCS,IMPORTS,EXPORTS,VM,CLOCK, &
                        CallsPerWindow, DYNV_FILE, LLPERTgrid, LLPertBundle, startTime)
   
        !Now put in a bundle
        call ESMFL_State2Bundle(EXPORTS(ROOT),LLPertBundle)
   
        !Create a new bundle for converged SVECS 
        SvecBundle = ESMF_FieldBundleCreate ( name='Svec bundle', __RC__)
        call ESMF_FieldBundleSet ( SvecBundle, grid=LLPERTgrid, __RC__) 
   
        !Get info for the bundle from a similar file
        call MAPL_CFIORead (DYNV_FILE,  startTime, SvecBundle, noread=.true., __RC__ )
   
        !Move xpert from one bundle to another changing variable names
        call BundleInt2Ext (0,bk,LLpertBundle,Svecbundle)
   
        !Filename for converged singular vector
        write (svecfilename, "(A4,I3.3,A15)") "svec", nn, "evolved.eta.nc4"
   
        !Create the file to write to
        call MAPL_CFIOCreate ( cfio, trim(svecfilename), clock, SvecBundle, DESCR='Write Converged Svec', EXPID='junk', __RC__ )
        call MAPL_CFIOSet ( cfio, Root=1, __RC__ )
   
        !Write the singular vector to the file
        call MAPL_CFIOWrite ( cfio, clock, SvecBundle, verbose = .true., __RC__ )
   
        !Deallocate the cfio
        call MAPL_cfioDestroy ( cfio )

        !Test for comparing with eigenvalue
        call state_to_xpert(EXPORTS,xpert,ROOT)
        call tv2t(xpert,xtrajF)
        call proj_final_xpert2xpert(xpert)
        call svec_norm_bc1(xpert)

        call dot_prod_xx(xpert,xpert,xdot(4))

        if (MAPL_AM_I_Root()) then
           print *
           print *, ' Evolved svec norm test...'
           print *
           print *, ' (x^TE^T/2E^1/2x)  = ',  xdot(4)
           print *
        endif

     endif

  enddo

  deallocate ( resid, workd, workl )
  deallocate ( Eval, Lvec, errbnd )

  call deallocate_config()
  call deallocate_xpert(xpert)
  call deallocate_xpert(xpertcheck)

  !Finalize the TLM/ADM environment
  call tlmadm_finalize(rc)

 end subroutine PERT_CAP

 end Program SingularVectors
