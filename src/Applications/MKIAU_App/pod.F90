! pod.x - ESMF/MAPL application to perform Proper Orthogonal Decomposition.
!
! This program implements the following calculations:
!
!   Given a set of M state-vectors X_m, it calculates corresponding anomalies
! dX_m = X_m - <X>, where <X> = (1/M) sum_m X_m, and finds eigenpairs of the 
! MxM symmetric matrix:
!
!    S = [ dX_1^T dX_2^T ... dX_M^T ]^T W [ dX_1 dX_2 ... dX_M ]
! 
! That is, this program solves: S U = U D, with U U^T = I, where U is the MxM 
! matrix of eigenvectors, D is the MxM diagonal matrix of eigenvalues, and
! W is a weighting matrix (usually some form of total energy measure, assuming
! the state vector is composed of the usual set of meteorological fields, or 
! a diagonal matrix of variances, so the resulting S matrix corresponds to a 
! correlation matrix).
!
! The output of the program is a set of M''(<=M) basis vectors defined as:
!
!  POD = V = [ dX_1 dX_2 ... dX_M ] U D^(1/2)
!
! where the sqrt of the eigenvalues are used since U we are really interested in
! the SVD-problem.
!
! A general vector X can thus be expanded in terms of the PODs, that is,
! projected onto POD space, as in:
!
!     X = sum_m a_m V_m
!
! for m=1,...,M'', where V_m is the m-th column of V. The coefficients a_m are 
! given by
!
!   a_m = < V_m, X >
!
! Since the basis of PODs does not span the whole space of possibilities, the
! vector X_pod, derived from the expansion, only represents a part of the full
! general vector X. The ratio 100 * ||X_pod||_W / ||X||_W corresponds to the
! percentage amount of X captured by the POD space.
!
! Consequently, this program is also capable of expanding a general vector X
! onto a previously-calculated basis of PODs and telling how much the PODs are
! capable of explaining the general vector X.
!
! REMARKS: 
!   a) mp_stats.x can be used to generate the set anomaly fields required
!      by this program. For example:
!          mp_stats.x -tmpl anomaly.%y4%m2%d2_%h2z -alpha -1.0 \
!                     -date 19990101 0 hy05a.ana.eta.*
!   b) This program requires an RC file: pod.rc (default name)
!
! Ricardo Todling, February 2013
!............................................................................
!  !REVISION_HISTORY:  
!   18Jun2013  Todling   - initial code 
!   14Jan2015  Todling  redef grid name following GEOS conventions (per !   Atanas)
!----------------------------------------------------------------------------

#  include "MAPL_Generic.h"

   Program POD

   use ESMF
   use MAPL_Mod
   use MAPL_CFIOMod
   use pflogger, only: pfl_initialize => initialize
   use m_StrTemplate, only: StrTemplate
   use m_set_eta, only: set_eta

   implicit NONE

!  Basic ESMF objects being used in this example
!  ---------------------------------------------
   type(ESMF_Grid)         :: INGrid    ! Input Grid
   type(ESMF_Grid)         :: OUGrid    ! Output Grid
   type(ESMF_Grid)         :: ANAgrid   ! ANA Grid
   type(ESMF_FieldBundle)  :: InBundle  ! Bundle to hold read in fields
   type(ESMF_FieldBundle)  :: EnBundle  ! Bundle to hold energy
   type(ESMF_Field)        :: Field     ! Field from bundle
   type(ESMF_Field)        :: enField   ! Field holding energy
   type(ESMF_FieldBundle)  :: MaskBundle! Bundle to hold mask field

   type(ESMF_VM)           :: vm       ! ESMF Virtual Machine
   type(ESMF_Time)         :: Time     ! Time objects
   type(ESMF_TimeInterval) :: TimeStep ! used to define a clock
   type(ESMF_Config)       :: CF       ! configuration settings
   type(MAPL_CFIO)         :: cfio

!  Grid Component Objects
!  ----------------------
   type(ESMF_Clock)    :: CLOCK
   type(MAPL_MetaComp) :: MAPLOBJ

!  Basic information about the parallel environment
!         PET = Persistent Execution Threads
!  In the current implementation, a PET is equivalent 
!  to an MPI process
!  ------------------------------------------------
   integer :: myPET   ! The local PET number
   integer :: nPET    ! The total number of PETs you are running on

   integer :: status, rc
   integer :: i, j, L, n, im, jm, lm, ii, nf, nn
   integer :: ifld, i2d, i3d, rank
   integer :: nfld, n2d, n3d, nfl
   integer :: nymd,nhms                ! work date/time
   integer :: nymdi,nhmsi              ! input (auxiliar) date/time
   integer :: nymdo,nhmso              ! output date/time

   integer :: Nx, Ny                   ! Layout
   integer :: im_out, jm_out, lm_out   ! Full dimension of output fields
   integer :: freq
   integer :: m,nconv

   integer :: comm
   logical :: xfld_recenter
   logical :: vnorm
   real    :: val,scl,cin,cm1,alpha,rmask
   real    :: eps_eer
   character(len=ESMF_MAXSTR)               :: levunits
   real,    pointer                         :: levels(:)
   real(8), pointer                         :: ak(:), bk(:)
   real,    pointer,     dimension(:,:)     :: ptr2d=>NULL()
   real,    pointer,     dimension(:,:,:)   :: ptr3d=>NULL()
   real   , allocatable, dimension(:,:,:)   :: mean2d
   real   , allocatable, dimension(:,:,:,:) :: mean3d
   real   , allocatable, dimension(:,:,:,:) :: flds4ene
   real   , allocatable, dimension(:,:,:)   :: cnt2d
   real   , allocatable, dimension(:,:,:,:) :: cnt3d

   real   , allocatable, dimension(:,:)       :: mask
   real   , allocatable, dimension(:,:)       :: scal2d
   real   , allocatable, dimension(:,:,:)     :: scal3d
   real   , allocatable, dimension(:,:,:,:)   :: flds2d
   real   , allocatable, dimension(:,:,:,:,:) :: flds3d
   real   , allocatable, dimension(:)         :: xscl2d,yscl2d
   real   , allocatable, dimension(:,:)       :: xscl3d,yscl3d
   real   , allocatable, dimension(:,:,:)     :: xfld2d
   real   , allocatable, dimension(:,:,:,:)   :: xfld3d

   integer, allocatable, dimension(:)        :: nconvs
   real   , allocatable, dimension(:,:)      :: evals
   real   , allocatable, dimension(:,:,:)    :: evecs

   character(len=ESMF_MAXSTR), allocatable, dimension(:) :: names2d
   character(len=ESMF_MAXSTR), allocatable, dimension(:) :: names3d

   logical :: per_fld_per_lev
   logical :: do_zsum
   logical, save :: recursive_stats=.true.   ! this is never to be make optional
                                              ! the internal option false is
                                              ! used for debugging and
                                              ! making sure results are as
                                              ! expected.
!  Energy measure-related variables
!  --------------------------------
   logical :: append_counter
   logical :: skip_main
   character(len=ESMF_MAXSTR), allocatable, dimension(:) :: enames
   real   , allocatable, dimension(:,:,:,:)  :: energy_fields
   real   , allocatable, dimension(:,:,:,:)  :: energy_accums
   real   , allocatable, dimension(:,:)      :: jweights_glb
   real   , allocatable, dimension(:,:)      :: jweights_lcl
   real   , allocatable, dimension(:,:,:)    :: delp
   real   , allocatable, dimension(:,:,:)    :: tv
   real   , allocatable, dimension(:,:,:)    :: sphu
   real   , allocatable, dimension(:,:)      :: pert_ps ! ps-perturbation

   logical :: verbose
   integer :: id
   integer :: MAXFILES = 500
   integer :: nfiles,nmembers
   character(len=ESMF_MAXSTR),allocatable :: files(:)
   character(len=ESMF_MAXSTR)             :: mfile     ! filename for user-splied mean
   character(len=ESMF_MAXSTR)             :: ifile     ! filename for main output (1st moments)
   character(len=ESMF_MAXSTR)             :: ofile     ! filename for main output (1st moments)
   character(len=ESMF_MAXSTR)             :: fmask     ! filename where to find mask parameter
   character(len=ESMF_MAXSTR)             :: nmask     ! name of field used for mask
   character(len=ESMF_MAXSTR)             :: ftmpl     ! filename template for output (when applicable)
   character(len=ESMF_MAXSTR)             :: outfname, myftmpl  ! aux

   character(len=ESMF_MAXSTR) :: ovars
   character(len=ESMF_MAXSTR) :: egress
   character(len=ESMF_MAXSTR) :: name
   character(len=ESMF_MAXSTR) :: myRC

   character(len=*), parameter :: Iam = 'POD'
   character(len=*), parameter :: myRC_def= 'pod.rc'

!                             -----
    
    call Main()

CONTAINS

    subroutine Main()

    character(len=30) ABKGGRIDNAME

!   Initialize the ESMF. For performance reasons, it is important
!    to turn OFF ESMF''s automatic logging feature
!   -------------------------------------------------------------
    call ESMF_Initialize (LogKindFlag=ESMF_LOGKIND_NONE, vm=vm, __RC__)

    call ESMF_VMGetCurrent(vm=vm, rc=status)
    call ESMF_VMGet(vm,mpiCommunicator=comm,rc=status)

    call pfl_initialize()

    call cmdline_ ( myRC, nymdo, nhmso )

    call init_ ( CF, myRC, __RC__ )

!   Check the number of processors
!   ------------------------------
    call ESMF_VMGet(vm, localPET=myPET, PETcount=nPET)  
    if ( nPET /= Nx * Ny ) then
       if ( MAPL_am_I_root() ) then
          print *, 'Error: expecting ', Nx*Ny, ' PETs but found ', nPET, 'PETs'
          print *, 'Try:  mpirun -np ', Nx*Ny, ' mp_stats.x'
       end if
       ASSERT_(.FALSE.)
    end if

    if ( MAPL_am_I_root() ) then
         print *
         print *, 'Starting ' // Iam // ' with ', nPET, ' PETs ...'
         print *
    end if

!   Create a regular Lat/Lon grid
!   -----------------------------
    call MAPL_DefGridName (IM_OUT,JM_OUT,ABKGGRIDNAME,MAPL_am_I_root())
    inGrid = grid_manager%make_grid(LatLonGridFactory(grid_name=trim(ABKGGRIDNAME), nx=nx, ny=ny, &
             im_world=im_out, jm_world=jm_out, lm=lm_out, &
             pole='PC',dateline='DC', rc=status))
    _VERIFY(status)


!   Validate grid
!   -------------
    call ESMF_GridValidate(INgrid,__RC__)

!   Create a clock
!   --------------
    call inqNCdatetime_ ( trim(mfile), nymd, nhms )
    call set_datetime_  ( nymd, nhms )
    CLOCK = ESMF_ClockCreate ( name="StatsClock", timeStep=TimeStep, startTime=Time, __RC__ )

!   Create bundle to hold background and read background
!   ----------------------------------------------------
    InBundle = ESMF_FieldBundleCreate ( name='Input bundle', __RC__ )
    call ESMF_FieldBundleSet(InBundle, grid=INgrid, __RC__ )

!   When applicable, read in mask field
!   -----------------------------------
    if (trim(fmask)/="NONE") then
        MaskBundle = ESMF_FieldBundleCreate ( name='mask bundle', __RC__ )
        call ESMF_FieldBundleSet(MaskBundle, grid=INgrid, __RC__ )
        call MAPL_CFIORead  ( trim(fmask), Time, Maskbundle, &
                              only_vars=nmask,&
                              TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
        call ESMF_FieldBundleGet(Maskbundle,FieldCount=nfld, __RC__ )
        DO ifld=1,nfld
           call ESMF_FieldBundleGet(MaskBundle, ifld, Field, __RC__ )
           call ESMF_FieldGet(Field, NAME=NAME, dimCount = rank, __RC__ )
           if (trim(NAME)/=trim(nmask)) cycle
           if (rank==2) then
              call ESMF_FieldGet(Field, farrayPtr=ptr2d, __RC__ )
              allocate(mask(size(ptr2d,1),size(ptr2d,2)))
              mask = ptr2d
           else
              if ( MAPL_am_I_root() ) then
                 print *, 'Mask has incorrect rank, aborting ... '
              end if
              ASSERT_(.FALSE.)
           endif
        END DO 
        call ESMF_FieldBundleDestroy (MaskBundle, __RC__)
    endif

!   Now initialize reading of fields of interest 
!   --------------------------------------------
    if (trim(ovars)=="NONE") then
        call MAPL_CFIORead  ( trim(mfile), Time, INbundle, &
                              TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
    else
        call MAPL_CFIORead  ( trim(mfile), Time, INbundle, &
                              only_vars=ovars,&
                              TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
    endif

    call get_fields_info_ (im,jm,lm,n2d,n3d,nfld)

!   Allocate number of required fields
!   ----------------------------------
    nmembers=nfiles
    if(n2d>0) then
       allocate(mean2d(im,jm,n2d),cnt2d(im,jm,n2d))
       allocate(names2d(n2d))
       allocate(flds2d(im,jm,n2d,nfiles))
       allocate(scal2d(n2d,nfiles))
       cnt2d  = 0.0
       mean2d = 0.0
    endif
    if(n3d>0) then
       allocate(mean3d(im,jm,lm,n3d),cnt3d(im,jm,lm,n3d))
       allocate(names3d(n3d))
       allocate(flds3d(im,jm,lm,n3d,nfiles))
       allocate(scal3d(lm,n3d,nfiles))
       cnt3d  = 0.0
       mean3d = 0.0
    endif

!   If recursive mean
!   -----------------
    if (.not.skip_main) then

!    Loop over files ...
!    -------------------
     DO nf = 1,nfiles

!      Check date/time and adjust clock if needed
!      ------------------------------------------
       call inqNCdatetime_ ( trim(files(nf)), nymdi, nhmsi )
       if(nymdi/=nymd.or.nhmsi/=nhms) then
          call set_datetime_  ( nymdi, nhmsi )
          CLOCK = ESMF_ClockCreate ( name="StatsClock", timeStep=TimeStep, startTime=Time, __RC__ )
          nymd=nymdi;nhms=nhmsi
       endif
       if (trim(ovars)=="NONE") then
           call MAPL_CFIORead  ( trim(files(nf)), Time, INbundle, &
                                 TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
       else
           call MAPL_CFIORead  ( trim(files(nf)), Time, INbundle, &
                                 only_vars=ovars, &
                                 TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
       endif

!      Reset variables
!      ---------------
       id  = 0
       i2d = 0
       i3d = 0

!      Accumulate sums
!      ---------------
       DO ifld=1,nfld
           call ESMF_FieldBundleGet(INbundle, ifld, Field, __RC__ )
           call ESMF_FieldGet(Field, NAME=NAME, dimCount = rank, __RC__ )
           if (.not. check_list_(NAME,ovars)) cycle
           if (rank==2) then
             i2d=i2d+1
             call ESMF_FieldGet(Field, farrayPtr=ptr2d, __RC__ )
             names2d(i2d) = trim(NAME)
             flds2d(:,:,i2d,nf) = ptr2d
             do j=1,jm
                do i=1,im
                   if(defined_(ptr2d(i,j),MAPL_UNDEF)) then ! mimic GFIO_mean for consistency
                      cnt2d(i,j,i2d) = cnt2d(i,j,i2d) + 1.0
                      cin = 1.0/cnt2d(i,j,i2d)
                      cm1 = cnt2d(i,j,i2d)-1.0
                      val = ptr2d(i,j)
                      if (recursive_stats) then
                          mean2d(i,j,i2d) = cm1*mean2d(i,j,i2d) + val
                          mean2d(i,j,i2d) = cin*mean2d(i,j,i2d)
                      else
                          mean2d(i,j,i2d) = mean2d(i,j,i2d) + val
                      endif
                   else ! not defined ...
                      cnt2d (i,j,i2d) = 0
                      mean2d(i,j,i2d) = MAPL_UNDEF
                   endif
                enddo
             enddo
           endif ! <rank=2>
           if (rank==3) then
             i3d=i3d+1
             call ESMF_FieldGet(Field, farrayPtr=ptr3d, __RC__ )
             names3d(i3d) = trim(NAME)
             flds3d(:,:,:,i3d,nf) = ptr3d
!            where(abs(flds3d(:,:,:,i3d,nf))>rmask)
!               flds3d(:,:,:,i3d,nf) = rmask
!            endwhere
             do L=1,lm
                do j=1,jm
                   do i=1,im
                      if(defined_(ptr3d(i,j,L),MAPL_UNDEF)) then ! mimic GFIO_mean for consistency
                         cnt3d(i,j,L,i3d) = cnt3d(i,j,L,i3d) + 1.0
                         cin = 1.0/cnt3d(i,j,L,i3d)
                         cm1 = cnt3d(i,j,L,i3d)-1.0
                         val = ptr3d(i,j,L)
                         if (recursive_stats) then
                             mean3d(i,j,L,i3d) = cm1*mean3d(i,j,L,i3d) + val
                             mean3d(i,j,L,i3d) = cin*mean3d(i,j,L,i3d)
                         else
                             mean3d(i,j,L,i3d) = mean3d(i,j,L,i3d) + val
                         endif
                      else ! not defined ...
                         cnt3d (i,j,L,i3d) = 0
                         mean3d(i,j,L,i3d) = MAPL_UNDEF
                      end if
                  end do ! <i>
                end do ! <j>
             end do ! <L>
           end if ! <rank=3>
       END DO

     END DO ! <files>
 
    end if ! <skip_main>

    if (rmask<MAPL_UNDEF) then
       if (n2d>0) then
          where(flds2d(:,:,:,:)>rmask/lm) flds2d(:,:,:,:)=rmask/lm
          where(mean2d(:,:,:)  >rmask   ) mean2d(:,:,:)  =rmask
       endif
       if (n3d>0) then
          where(flds3d(:,:,:,:,:)>rmask/lm) flds3d(:,:,:,:,:)=rmask/lm
          where(mean3d(:,:,:,:)  >rmask   ) mean3d(:,:,:,:)  =rmask
       endif
    endif

!   Sum vertically (not always meaningful) 
!   --------------
    if (do_zsum) then
       do nf=1,nfiles
         do i3d=1,n3d
            do ii=lm-1,1,-1
               flds3d(:,:,lm,i3d,nf) = flds3d(:,:,lm,i3d,nf) + flds3d(:,:,ii,i3d,nf)
            enddo
         enddo
       enddo
       do i3d=1,n3d
          do ii=lm-1,1,-1
             mean3d(:,:,lm,i3d) = mean3d(:,:,lm,i3d) + mean3d(:,:,ii,i3d)
          enddo
       enddo
    endif

    do i2d=1,n2d
       do nf=1,nfiles
          call rectangle_mask (mask,flds2d(:,:,i2d,nf))
       enddo
       call rectangle_mask (mask,mean2d(:,:,i2d))
    enddo
    
!   Before write out, add ak/bk to the grid
!   ---------------------------------------
    call ESMF_AttributeSet(INgrid, name='ak', valuelist=real(ak,4), __RC__ )
    call ESMF_AttributeSet(INgrid, name='bk', valuelist=real(bk,4), __RC__ )

! Check/Set time of write out
! ---------------------------
    if (nymdo>0.and.nhmso>=0) then
       call set_datetime_  ( nymdo, nhmso )
       CLOCK = ESMF_ClockCreate ( name="StatsClock", timeStep=TimeStep, startTime=Time, __RC__ )
    endif

!  De-bias fields (or do: xin = xin + alpha * mean_x)
!  --------------
   if (trim(ftmpl) /= "NONE" ) then

!      Loop over samples (files) ...
!      ------------------------
       DO nf = 1,nfiles

          ! Remove mean (i.e., do: xin = xin + alpha * mean_x)
          ! --------------------------------------------------
          if ( abs(alpha)>1.e-10 ) then
             if(allocated(flds2d)) flds2d(:,:,:,  nf) = flds2d(:,:,:,  nf) + alpha*mean2d
             if(allocated(flds3d)) flds3d(:,:,:,:,nf) = flds3d(:,:,:,:,nf) + alpha*mean3d
          endif

          ! Scale/Normalize fields
          ! ----------------------
          call normlz_fields_ ( -1, scal2d(:,nf), flds2d(:,:,:,nf), scal3d(:,:,nf), flds3d(:,:,:,:,nf) ) 

      enddo

   endif

!  Allocate space for eigenvalue problem matrices
!  ----------------------------------------------
   m=nfiles
   if (per_fld_per_lev) then
      nfl=n2d+n3d*LM
   else
      nfl=1
   endif
   allocate (evals(m,nfl), evecs(m,m,nfl),nconvs(nfl))

!  When using direct solver, build matrix to decompose
!  (store matrix in evecs array)
!  ---------------------------------------------------
   call build_matrix_ ( evecs, flds2d, flds3d )

!  Solve eigenvalue problem
!  ------------------------
   do nn=1,nfl
      call solver1_ ( evecs(:,:,nn), evals(:,nn), nconvs(nn) )
      if ( MAPL_am_I_root() ) then
         print*, 'nn, evals(nconv) = ', nn,evals(m-nconvs(nn)+1:m,nn)
      endif
   enddo

!  Construct PODs
!  --------------
   call basis_func_ ( flds2d, flds3d, evals, evecs, nconvs )

!  Partial clean up after solver
!  -----------------------------
   deallocate(evals,evecs)

!  If so, project vector onto POD basis
!  ------------------------------------
   if (trim(ifile) /= "NONE" ) then

!      Check date/time and adjust clock if needed
!      ------------------------------------------
       call inqNCdatetime_ ( trim(ifile), nymdi, nhmsi )
       if(nymdi/=nymd.or.nhmsi/=nhms) then
          call set_datetime_  ( nymdi, nhmsi )
          CLOCK = ESMF_ClockCreate ( name="StatsClock", timeStep=TimeStep, startTime=Time, __RC__ )
          nymd=nymdi;nhms=nhmsi
       endif
       if (trim(ovars)=="NONE") then
           call MAPL_CFIORead  ( trim(ifile), Time, INbundle, &
                                 TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
       else
           call MAPL_CFIORead  ( trim(ifile), Time, INbundle, &
                                 only_vars=ovars, &
                                 TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
       endif

!      Allocate space for input vector
!      -------------------------------
       if (n2d>0) then
          allocate(xfld2d(im,jm,n2d))
          allocate(xscl2d(n2d))
          allocate(yscl2d(n2d))
       endif
       if (n3d>0) then
          allocate(xfld3d(im,jm,lm,n3d))
          allocate(xscl3d(lm,n3d))
          allocate(yscl3d(lm,n3d))
       endif

!      Put incoming vector in array Xfld
!      ---------------------------------
       call fields2bundle_ (xfld2d,xfld3d,bundle2fields=.true.)

!      Remove mean (i.e., do: xin = xin + alpha * mean_x)
!      --------------------------------------------------
       if ( abs(alpha)>1.e-10 .and. xfld_recenter ) then
          if(allocated(xfld2d)) xfld2d = xfld2d + alpha*mean2d
          if(allocated(xfld3d)) xfld3d = xfld3d + alpha*mean3d
       endif

!      Calculate normalization factors of Xfld
!      ---------------------------------------
       call normlz_fields_ ( 1, xscl2d, xfld2d, xscl3d, xfld3d )

!      Project Xfld onto PODs
!      ----------------------
       call project_onto_pods_ (nconv, flds2d,flds3d, xfld2d,xfld3d)

!      Calculate explained variance
!      ----------------------------
       call normlz_fields_ ( 1, yscl2d, xfld2d, yscl3d, xfld3d )
       call explained_var_ ( xscl2d,xscl3d, yscl2d,yscl3d )

!      Write out projected vector
!      --------------------------
       if (trim(ofile) /= "NONE" ) then

!          Put projected vector into bundle
!          --------------------------------
           call fields2bundle_ (xfld2d,xfld3d)

!          Write out fields
!          ----------------
           if(nymdo>0.and.nhmso>=0) then
              call set_datetime_  ( nymdo, nhmso )
              CLOCK = ESMF_ClockCreate ( name="StatsClock", timeStep=TimeStep, startTime=Time, __RC__ )
              nymd=nymdo;nhms=nhmso
           endif
           call MAPL_CFIOCreate ( cfio, trim(ofile), clock, InBundle,  &
                                  FREQUENCY=freq, &
                                  DESCR='Projected Fields', __RC__ )
           call MAPL_CFIOWrite ( cfio, clock, InBundle, verbose=verbose, __RC__ )
           call MAPL_cfioDestroy ( cfio )

       endif

!      Deallocate space for input vector
!      ---------------------------------
       if (n2d>0) then
          deallocate(yscl2d)
          deallocate(xscl2d)
          deallocate(xfld2d)
       endif
       if (n3d>0) then
          deallocate(yscl3d)
          deallocate(xscl3d)
          deallocate(xfld3d)
       endif

   endif

!  Write-out basis functions
!  -------------------------
   if (trim(ftmpl) /= "NONE" ) then

!      Loop over samples (files) ...
!      ------------------------
!      nconv=minval(nconvs)
       nconv=4
       DO nf = nfiles-nconv+1,nfiles

!         copy fields from nf case to bundle
!         ----------------------------------
          call fields2bundle_ (flds2d(:,:,:,nf),flds3d(:,:,:,:,nf))

!         if (append_counter) then
             write(myftmpl,'(2a,i3.3,a)') trim(ftmpl), '.', nf, '.nc4'
!         else
!            write(myftmpl,'(2a)') trim(ftmpl), '.nc4'
!         endif
          if(nymdo>0.and.nhmso>=0) then
             call set_datetime_  ( nymdo, nhmso )
             CLOCK = ESMF_ClockCreate ( name="StatsClock", timeStep=TimeStep, startTime=Time, __RC__ )
             nymd=nymdo;nhms=nhmso
          endif
          call StrTemplate ( outfname, myftmpl, 'GRADS', nymd=nymd, nhms=nhms, stat=status)
          call MAPL_CFIOCreate ( cfio, trim(outfname), clock, InBundle,  &
                                 FREQUENCY=freq, &
                                 DESCR='POD Fields', __RC__ )
          call MAPL_CFIOWrite ( cfio, clock, InBundle, verbose=verbose, __RC__ )
          call MAPL_cfioDestroy ( cfio )
      enddo

   endif

!  remaining clean up
!  ------------------
   deallocate(nconvs)
   if(allocated(mask)) deallocate(mask)

!  All done
!  --------
   if (MAPL_AM_I_ROOT()) then
       close(999)
       open (999,file=trim(egress),form='formatted')
       close(999)
   end if
   call final_
   call ESMF_Finalize(__RC__)

  end subroutine Main

  subroutine get_fields_info_ (im,jm,lm,n2d,n3d,nfld)

  implicit none
  integer,intent(out) :: im,jm,lm,n2d,n3d,nfld

  integer j2d,j3d
  logical read2d,read3d
    
! First get dims ...
! ------------------
    read2d=.false.
    read3d=.false.
    call ESMF_FieldBundleGet(INbundle,FieldCount=nfld, __RC__ )
    n2d=0;n3d=0
    DO ifld=1,nfld
       call ESMF_FieldBundleGet(INbundle, ifld, Field, __RC__ )
       call ESMF_FieldGet(Field, NAME=NAME, dimCount=rank, __RC__ )
       if (.not. check_list_(NAME,ovars)) cycle
       if(rank==2) then
          n2d=n2d+1
          if (.not.read2d) then
             call ESMF_FieldGet(Field, farrayPtr=ptr2d, __RC__ )
             im=size(ptr2d,1)
             jm=size(ptr2d,2)
             read2d=.true.
          endif
       endif
       if(rank==3) then
          n3d=n3d+1
          if (.not.read3d) then
             call ESMF_FieldGet(Field, farrayPtr=ptr3d, __RC__ )
             im=size(ptr3d,1)
             jm=size(ptr3d,2)
             lm=size(ptr3d,3)
             read3d=.true.
          endif
       endif
    END DO

  end subroutine get_fields_info_

  subroutine fields2bundle_ (x2d,x3d,bundle2fields)

  implicit none
  real,intent(inout) :: x2d(:,:,:)
  real,intent(inout) :: x3d(:,:,:,:)
  logical,optional,intent(in) :: bundle2fields

  integer j2d,j3d
  logical b2f

  b2f=.false.
  if(present(bundle2fields)) then
    if(bundle2fields) b2f=.true.
  endif

  ! Place stats in Inbundle
  j2d = 0
  j3d = 0
  DO ifld=1,nfld
     call ESMF_FieldBundleGet(INbundle, ifld, Field, __RC__ )
     call ESMF_FieldGet(Field, NAME=NAME, dimCount=rank, __RC__ )
     if (.not. check_list_(NAME,ovars)) cycle
     if (rank == 2) then
       j2d=j2d+1
       call ESMF_FieldGet(Field, farrayPtr=ptr2d, __RC__ )
       if (b2f) then
          x2d(:,:,j2d) = ptr2d
       else
          ptr2d = x2d(:,:,j2d)
          if (.not.recursive_stats) then
             where(cnt2d(:,:,j2d)>0.0)
                ptr2d = ptr2d/cnt2d(:,:,j2d)
             endwhere
          endif
       endif
     endif ! <rank=2>
     if (rank == 3) then
       j3d=j3d+1
       call ESMF_FieldGet(Field, farrayPtr=ptr3d, __RC__ )
       if (b2f) then
          x3d(:,:,:,j3d) = ptr3d
       else
          ptr3d = x3d(:,:,:,j3d)
          if (.not.recursive_stats) then
             where(cnt3d(:,:,:,j3d)>0.0)
                ptr3d = ptr3d/cnt3d(:,:,:,j3d)
             endwhere
          endif
       endif
     end if ! <rank=3>
  END DO

  end subroutine fields2bundle_

  subroutine saxpy_ (alpha,x2d,x3d)

  implicit none
  real,intent(in) :: alpha ! scaling coefficient
  real,intent(in) :: x2d(:,:,:)
  real,intent(in) :: x3d(:,:,:,:)

  integer ii,jj,kk
  integer i,j,k
  integer j2d,j3d

  ! Place stats in Inbundle
  j2d = 0
  j3d = 0
  DO ifld=1,nfld
     call ESMF_FieldBundleGet(INbundle, ifld, Field, __RC__ )
     call ESMF_FieldGet(Field, NAME=NAME, dimCount=rank, __RC__ )
     if (.not. check_list_(NAME,ovars)) cycle
     if (rank == 2) then
       j2d=j2d+1
       call ESMF_FieldGet(Field, farrayPtr=ptr2d, __RC__ )
       ii=size(ptr2d,1)
       jj=size(ptr2d,2)
       do j=1,jj
          do i=1,ii
             if( defined_(ptr2d(i,j),MAPL_UNDEF) ) then
                 ptr2d(i,j) = ptr2d(i,j) + alpha * x2d(i,j,j2d)
             endif
          enddo
       enddo
     endif ! <rank=2>
     if (rank == 3) then
       j3d=j3d+1
       call ESMF_FieldGet(Field, farrayPtr=ptr3d, __RC__ )
       ii=size(ptr3d,1)
       jj=size(ptr3d,2)
       kk=size(ptr3d,3)
       do k=1,kk
          do j=1,jj
             do i=1,ii
                if( defined_(ptr3d(i,j,k),MAPL_UNDEF) ) then
                    ptr3d(i,j,k) = ptr3d(i,j,k) + alpha * x3d(i,j,k,j3d)
                endif
             enddo
          enddo
       enddo
     end if ! <rank=3>
  END DO

  end subroutine saxpy_

  subroutine normlz_fields_ ( dothis, scal2d, x2d, scal3d, x3d )

  use m_parDOT, only : parNRM2
  use m_die, only : mp_die
  implicit none
  character(len=*), parameter :: myname_ = 'normlz_fields_'

  integer, intent(in)    :: dothis ! 0=unscale; 1=scale
  real   , intent(inout) :: scal2d(:), scal3d(:,:)

  real   , intent(inout) :: x2d(:,:,:), x3d(:,:,:,:)
 
  logical, save :: norm_factors_done=.false.
  integer ierr,n,ll
  
  if ( abs(dothis)==1 ) then  ! get normalization factors
     do n=1,n2d
        scal2d(n)  = parNRM2 ( reshape(x2d(:,:,n),(/im*jm/)), comm )
     enddo

     do n=1,n3d
        do ll=1,lm
           scal3d(ll,n)  = parNRM2 ( reshape(x3d(:,:,ll,n),(/im*jm/)), comm )
        enddo
     enddo
     norm_factors_done=.true.
  endif

  if ( dothis==-1 ) then  ! scale vectors
     if (.not.norm_factors_done ) then
        call MP_die(myname_,'error, trying to scale before factor calculation done',ierr)
     endif
     do n=1,n2d
        x2d(:,:,n) = (1.0/scal2d(n)) * x2d(:,:,n)
     enddo

     do n=1,n3d
        do ll=1,n3d
           x3d(:,:,ll,n) = (1.0/scal3d(ll,n)) * x3d(:,:,ll,n)
        enddo
     enddo
  endif

  if ( dothis==0 ) then  ! unscale vectors
     if (.not.norm_factors_done ) then
        call MP_die(myname_,'error, trying to scale before factor calculation done',ierr)
     endif
     do n=1,n2d
        x2d(:,:,n) = scal2d(n) * x2d(:,:,n)
     enddo

     do n=1,n3d
        do ll=1,lm
           x3d(:,:,ll,n) = scal3d(ll,n) * x3d(:,:,ll,n)
        enddo
     enddo
  endif

  end subroutine normlz_fields_

  subroutine build_matrix_ ( amatrix, flds2d, flds3d )
  use m_parDOT, only : parDOT
  implicit none
  real, intent(out) :: amatrix(:,:,:)
  real, intent(in)  :: flds2d(:,:,:,:)
  real, intent(in)  :: flds3d(:,:,:,:,:)

  real, allocatable :: xvec(:,:)
  real, allocatable :: row2d(:),col2d(:)
  real, allocatable :: row3d(:),col3d(:)

  integer ierr,n,msam
  integer ndim, ndim2d, ndim3d
  integer i1,j1,l1,n1,m1
  integer i2,j2,l2,n2,m2
  integer jl,jf,nf

  msam   = nfiles
  
  if (nfl==1) then
     ndim2d = im*jm*n2d
     ndim3d = im*jm*lm*n3d
     ndim   = ndim3d + ndim2d
  else
     ndim2d = im*jm
     ndim3d = im*jm
  endif

  allocate(row2d(ndim2d),col2d(ndim2d))
  allocate(row3d(ndim3d),col3d(ndim3d))
  amatrix=0.0
  if (nfl==1) then
     do m2=1,msam
        if(n2d>0)then
           col2d = reshape(flds2d(:,:,:,m2),(/ndim2d/))
           do m1=1,msam
              row2d = reshape(flds2d(:,:,:,m1),(/ndim2d/))
              amatrix(m1,m2,1) = parDOT(col2d,row2d,comm)
           enddo
        endif
        if(n3d>0)then
           col3d = reshape(flds3d(:,:,:,:,m2),(/ndim3d/))
           do m1=1,msam
              row3d = reshape(flds3d(:,:,:,:,m1),(/ndim3d/))
              amatrix(m1,m2,1) = amatrix(m1,m2,1) + parDOT(col3d,row3d,comm)
           enddo
        endif
     enddo
  else
     do m2=1,msam
        if(n2d>0)then
           do jf=1,n2d ! loop over fields
              col2d = reshape(flds2d(:,:,jf,m2),(/ndim2d/))
              do m1=1,msam
                 row2d = reshape(flds2d(:,:,jf,m1),(/ndim2d/))
                 amatrix(m1,m2,jf) = parDOT(col2d,row2d,comm)
              enddo
           enddo
        endif
        if(n3d>0)then
           nf=n2d
           do jf=1,n3d ! loop over fields
              do jl=1,lm ! loop over levels
                 col3d = reshape(flds3d(:,:,jl,jf,m2),(/ndim3d/))
                 nf=nf+1
                 do m1=1,msam
                    row3d = reshape(flds3d(:,:,jl,jf,m1),(/ndim3d/))
                    amatrix(m1,m2,nf) = amatrix(m1,m2,nf) + parDOT(col3d,row3d,comm)
                 enddo
              enddo
           enddo
        endif
     enddo
  endif
  deallocate(row3d,col3d)
  deallocate(row2d,col2d)

  end subroutine build_matrix_

  subroutine solver1_ (A,W,NC)
  implicit none
  real, intent(inout) :: A(:,:)
  real, intent(inout) :: W(:)
  integer, intent(out) :: NC ! converged evals

  integer :: N, LDA, LWORK, INFO
  real, allocatable :: WORK(:)
  character(len=ESMF_MAXSTR) :: msg
  
  nconv = 0
  LDA = size(A,1)
  N   = size(A,2)
  LWORK = 3*N-1
  allocate(WORK(LWORK))

  call SSYEV( 'V', 'U', N, A, LDA, W, WORK, LWORK, INFO )

  if (INFO/=0) then
     write(msg,'(a,i5)') 'Error in eigensolver, info = ', info
     call WRITE_PARALLEL(trim(msg))
  endif
  deallocate(WORK)
  NC = count(W>0.0,1) ! For now, simple criterium of acceptance 

  end subroutine solver1_

  subroutine basis_func_ ( flds2d, flds3d, evals, evecs, mconvs )

  real,intent(inout) :: flds2d(:,:,:,:)
  real,intent(inout) :: flds3d(:,:,:,:,:)
  real,intent(in)    :: evals(:,:)
  real,intent(inout) :: evecs(:,:,:)
  integer,intent(in) :: mconvs(:)

  real,allocatable::aux(:)
  integer ierr,n,msam,nc,mconv
  integer ndim2d, ndim3d
  integer ii,jj,ll,nf,m,mbeg

  msam=nfiles
  
  if (nfl==1) then
      ndim2d = im*jm*n2d
      ndim3d = im*jm*lm*n3d
  else
      ndim2d = im*jm
      ndim3d = im*jm
  endif

! Scale evecs by evals
  do nc=1,nfl
     mbeg=msam-mconvs(nc)+1
     do m=mbeg,msam
        evecs(:,m,nc) = evecs(:,m,nc)/sqrt(evals(m,nc))
     enddo
  enddo

! computationally expensive way
  do nf=1,n2d
    mconv=mconvs(nf)
    mbeg=msam-mconvs(nf)+1
    allocate(aux(mconv))
    do jj=1,jm
      do ii=1,im
        aux = flds2d(ii,jj,nf,:)
        do m=mbeg,msam
           flds2d(ii,jj,nf,m) = dot_product(aux,evecs(:,m,nf))
        enddo
      enddo
    enddo
    deallocate(aux)
  enddo

  do nf=1,n3d
    nc=nf+n2d
    mconv=mconvs(nc)
    mbeg=msam-mconvs(nc)+1
    allocate(aux(mconv))
    do ll=1,lm
      do jj=1,jm
        do ii=1,im
          aux = flds3d(ii,jj,ll,nf,:)
          do m=mbeg,msam
             flds3d(ii,jj,ll,nf,m) = dot_product(aux,evecs(:,m,nc))
          enddo
        enddo
      enddo
    enddo
    deallocate(aux)
  enddo
  
  end subroutine basis_func_

  subroutine project_onto_pods_ (mconv, f2d,f3d, x2d,x3d)

  use m_parDOT, only : parDOT
  implicit none
  integer,intent(in):: mconv
  real,intent(in)   :: f2d(:,:,:,:)
  real,intent(in)   :: f3d(:,:,:,:,:)
  real,intent(inout):: x2d(:,:,:)
  real,intent(inout):: x3d(:,:,:,:)
 
  real,allocatable,dimension(:) :: coeffs
  real,allocatable,dimension(:) :: x2,f2
  real,allocatable,dimension(:) :: x3,f3
  integer msam,m,ndim2d,ndim3d

  allocate(coeffs(mconv))

  msam   = nfiles
  ndim2d = im*jm*n2d
  ndim3d = im*jm*lm*n3d

! calculate projection coefficients
! ---------------------------------
  coeffs = 0.0
  do m=msam-mconv+1,msam
     if (ndim2d>0) then
        allocate(f2(ndim2d),x2(ndim2d))
        f2 = reshape(f2d(:,:,:,m),(/ndim2d/))
        x2 = reshape(x2d         ,(/ndim2d/))
        coeffs(m) = parDOT(x2,f2,comm)
        deallocate(f2,x2)
     endif
     if (ndim3d>0) then
        allocate(f3(ndim3d),x3(ndim3d))
        f3 = reshape(f3d(:,:,:,:,m),(/ndim3d/))
        x3 = reshape(x3d           ,(/ndim3d/))
        coeffs(m) = coeffs(m) + parDOT(x3,f3,comm)
        deallocate(f3,x3)
     endif
  enddo
  
! projected vector
! ----------------
  if(ndim2d>0) x2d = 0.0
  if(ndim3d>0) x3d = 0.0
  do m=msam-mconv+1,msam
     if (ndim2d>0) then
        x2d = x2d + coeffs(m) * f2d(:,:,:,m)
     endif
     if (ndim3d>0) then
        x3d = x3d + coeffs(m) * f3d(:,:,:,:,m)
     endif
  enddo
   
  deallocate(coeffs)

  end subroutine project_onto_pods_

  subroutine explained_var_ ( xscl2d,xscl3d, yscl2d,yscl3d )

  real,intent(in) :: xscl2d(:)
  real,intent(in) :: xscl3d(:,:)
  real,intent(in) :: yscl2d(:)
  real,intent(in) :: yscl3d(:,:)

  real    var
  integer n,ll

  do n=1,n2d
     var = 100.*yscl2d(n)/xscl2d(n)
     print*, 'Explained var for ',trim(names2d(n)), ' ', var
  enddo
  do n=1,n3d
     do ll=1,lm
        var = 100.*yscl3d(ll,n)/xscl3d(ll,n)
        print*, 'Explained var for ',trim(names3d(n)), ' ', var, ' at lev ', ll
     enddo
  enddo

  end subroutine explained_var_


!   Allocate number of required fields
!BOP
! !ROUTINE: init_: initialize POD
!
! !DESCRIPTION:
!
! !INTERFACE:
!
    subroutine init_ ( CF, myRC, rc ) 

! !USES:

    use ESMF, only: ESMF_FALSE
    use m_StrTemplate, only: StrTemplate
    use m_die, only: MP_die

    implicit NONE

! !INPUT/OUTPUT PARAMETERS:

    type(ESMF_Config)     :: CF

! !OUTPUT PARAMETERS:

    character(len=*), intent(in)   :: myRC      ! resource filename
    integer,          intent(out)  :: rc      ! return error code
!
! !REVISION HISTORY:
!
!	25Feb2013 Todling  Initial code.
!
!EOP

    character*4, parameter :: myname_ = 'init_'

    integer  idum, status
    logical  iexist
    character(len=ESMF_MAXSTR) :: tmpl
    character(len=ESMF_MAXSTR) :: norm_type

!   Check on existence of rc file
!   -----------------------------
    inquire(file=trim(myrc),exist=iexist)
    if(.not.iexist) call usage_

!   Create Config and Initialize Clock 
!   ----------------------------------
    CF = ESMF_ConfigCreate   (__RC__)
    call ESMF_ConfigLoadFile ( CF, myrc, __RC__ )

!  Set defaults
!  ------------
   rc = 0

   call ESMF_ConfigGetAttribute( CF, NX, label ='NX:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, NY, label ='NY:', __RC__ )

   call ESMF_ConfigGetAttribute( CF, IM_OUT, label ='MP_STATS_IM:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, JM_OUT, label ='MP_STATS_JM:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, LM_OUT, label ='MP_STATS_LM:', __RC__ )

   call ESMF_ConfigGetAttribute( CF, eps_eer  , label ='EPS_EER:', DEFAULT=0.0, __RC__ )
   call ESMF_ConfigGetAttribute( CF, norm_type, label ='VNORM:'  , DEFAULT="NO", __RC__ )
   if(trim(norm_type)/="NO") then 
      vnorm=.true.
   else
      vnorm=.false.
   endif

!  get dims
!  --------
!  call getdim_ ( bkgfname, IM_BKG, JM_BKG, LM_BKG, idum, status )

!  define vertical grid (should be put in the grid ...)
!  ----------------------------------------------------
   allocate(ak(LM_OUT+1),bk(LM_OUT+1))
   call DefVertGrid_(CF,ak,bk,LM_OUT,verbose,status)

   end subroutine init_

   subroutine set_datetime_ (nymd, nhms)

   implicit none
   integer, intent(in) :: nymd, nhms

   integer thistime(6)

   thistime(1) =     nymd/10000
   thistime(2) = mod(nymd,10000)/100
   thistime(3) = mod(nymd,100)
   thistime(4) =     nhms/10000
   thistime(5) = mod(nhms,10000)/100
   thistime(6) = mod(nhms,100)

!  Set ESMF date/time
!  ------------------
   call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN )
   call ESMF_TimeSet(Time, yy=thistime(1), mm=thistime(2), dd=thistime(3), &
                            h=thistime(4), m =thistime(5),  s=thistime(6))
   call ESMF_TimeIntervalSet( TimeStep, h=6, m=0, s=0, __RC__ )

   end subroutine set_datetime_


   subroutine cmdline_ ( myRC, nymd, nhms )

   use m_mpif90, only: mp_character
   use m_mpif90, only: mp_integer
   use m_mpif90, only: mp_logical
   use m_mpif90, only: mp_real4
   use m_die,    only: mp_die
   use MAPL_Mod, only: MAPL_ROOT, MAPL_AM_I_ROOT

   implicit none

   integer,          intent(out)   :: nymd, nhms
   character(len=*), intent(inout) :: myRC

   character(len=*), parameter :: myname_ = 'cmdline_'
   integer iret, i, iarg, argc, iargc
   integer nymd_,nhms_
   integer freq_,freq_hr,freq_mn
   character(len=255) :: argv

!  Defaults
!  --------
   rmask   =  MAPL_UNDEF
   alpha   =  0.0
   egress  = 'POD_EGRESS'
   nymd_   = -1
   nhms_   = -1
   freq_   = -1
   myRC    = "NONE"
   verbose = .false.
   ftmpl   = "NONE"
   fmask   = "NONE"
   ifile   = "NONE"
   ofile   = "NONE"
   mfile   = "NONE"
   ovars   = "NONE"
   skip_main     = .false.
   append_counter= .false.
   xfld_recenter = .false.
   per_fld_per_lev=.false.
   do_zsum=.false.

   allocate(files(maxfiles))

!  Read in command line
!  --------------------
   if (MAPL_AM_I_ROOT()) then

!     Parse command line
!     ------------------
      argc =  iargc()
      if ( argc .lt. 1 ) call usage_()

      iarg = 0
      nfiles = 0

      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iarg, argv )

         select case (argv)
           case ("-alpha")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv,*) alpha
           case ("-date")
             if ( iarg+2 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv,*) nymd_
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv,*) nhms_
           case ("-egress")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, egress )
           case ("-h")
             if ( iarg+1 .gt. argc ) call usage_()
           case ("-i")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, ifile )
           case ("-inc")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv,*) freq_
           case ("-irecenter")
             if ( iarg+1 .gt. argc ) call usage_()
             xfld_recenter = .true.
           case ("-nmask")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, nmask )
           case ("-o")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, ofile )
           case ("-per_fld_per_lev")
             if ( iarg+1 .gt. argc ) call usage_()
             per_fld_per_lev = .true.
           case ("-rc")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, myRC )
           case ("-fmask")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iarg, fmask )
           case ("-rmask")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv,*) rmask
           case ("-tmpl")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, ftmpl )
           case ("-vars")
             iarg = iarg + 1
             call GetArg ( iArg, ovars )
           case ("-verbose")
             verbose = .true.
           case ("-zsum")
             if ( iarg+1 .gt. argc ) call usage_()
             do_zsum = .true.
           case default
             nfiles = nfiles + 1
             if ( nfiles .gt. maxfiles ) call usage_()
             files(nfiles) = trim(argv)

         end select
      end do
      print *
      print *, 'Files to handle: '
      print *
      do i=1,nfiles
         print*, 'input from file:              ', trim(files(i))
      enddo
      print *
      if(mfile/="NONE") &
      print *, 'user-supplied mean:              ', trim(mfile) 
      if(ifile/="NONE") &
      print *, 'vector to expand onto PODs:      ', trim(ifile)
      if(ofile/="NONE") &
      print *, 'given vector projected onto PODs:', trim(ofile)
      if(ftmpl/="NONE") &
      print *, 'template for output members: ', trim(ftmpl) 
      if(fmask/="NONE" .and. nmask/="NONE") then 
         print *, 'Mask filenames: ', trim(fmask) 
         print *, 'Mask name: ', trim(nmask) 
      endif
      if(ovars/="NONE") &
      print *, 'Requested vars:              ', trim(ovars) 
      print *
      print *, 'NOTE: Yes, I will read first file twice'

   endif ! <ROOT section>

!  Broadcast command line
!  ----------------------
   call mpi_bcast(nfiles,         1,mp_integer  ,MAPL_root,comm,iret)
   call mpi_bcast(nymd_ ,         1,mp_integer  ,MAPL_root,comm,iret)
   call mpi_bcast(nhms_ ,         1,mp_integer  ,MAPL_root,comm,iret)
   call mpi_bcast(freq_ ,         1,mp_integer  ,MAPL_root,comm,iret)
   call mpi_bcast(alpha ,         1,mp_real4    ,MAPL_root,comm,iret)
   call mpi_bcast(rmask ,         1,mp_real4    ,MAPL_root,comm,iret)
   call mpi_bcast(xfld_recenter,  1,mp_logical  ,MAPL_root,comm,iret)
   call mpi_bcast(do_zsum        ,1,mp_logical  ,MAPL_root,comm,iret)
   call mpi_bcast(per_fld_per_lev,1,mp_logical  ,MAPL_root,comm,iret)
   call mpi_bcast(verbose,        1,mp_logical  ,MAPL_root,comm,iret)
   do i = 1,nfiles
      call mpi_bcast(files(i),ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   enddo
   call mpi_bcast(ovars ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(ifile ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(ofile ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(mfile ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(fmask ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(nmask ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(ftmpl ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(myRC  ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(egress,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)

!  Define RC file
!  --------------
   if(myRC=="NONE") myRC = myRC_def

!  Possibly overwrite date/time w/ user''s specification
!  -----------------------------------------------------
   nymd=nymd_
   nhms=nhms_
   if (freq_<0) then
       freq = 6 * 3600 ! 6 hours by default
   else
       freq_hr = freq_/10000
       freq_mn = mod(freq_,10000)/100
       freq    = mod(freq_,100)
       freq    = freq_hr*3600 + freq_mn*60 + freq
   endif

   if (trim(mfile)=="NONE" ) then
      mfile = files(1)
   endif

   if(trim(ftmpl)/="NONE") then ! make sure -date has not been specified by user
     if(nymd_>0.and.nhms_>=0) then
        append_counter=.true.
     endif
   endif

   end subroutine cmdline_

   subroutine usage_

   use MAPL_Mod, only: MAPL_AM_I_ROOT
   implicit none
   if( MAPL_AM_I_ROOT() ) then
       write(6,*)
       write(6,'(a)') 'Usage: mp_pod.x [options] files'
       write(6,*)
       write(6,'(a)') 'options:'
       write(6,*)
       write(6,'(a)') '-i     iFILE      specify filename w/ fields to be projected'
       write(6,'(a)') '-o     oFILE      specify output filename of projected fields'
       write(6,'(a)') '-irecenter        when specified, it removed mean from input'
       write(6,'(a)') '                    vector before projected onto PODs'
       write(6,'(a)') '-alpha NUMBER     specify multiplicative coeff to scale mean '
       write(6,'(a)') '                    before adding result to each file read in'
       write(6,'(a)') '                    (see -tmpl)'
       write(6,'(a)') '-date  NYMD NHMS  date/time of output file(s)'
       write(6,'(a)') '                    (when absent use date of last file read)'
       write(6,'(a)') '-tmpl  FNAMETMPL  specify filename template of output files'
       write(6,'(a)') '                    NOTE: do not provide filename extension'
       write(6,'(a)') '                          (nc4 will be appended to name)'
       write(6,'(a)') '                    (e.g., -tmpl myfiles.%y4%m2%d2_%h2z)     '
       write(6,'(a)') '-vars  LIST       where LIST is a list of variable separate' 
       write(6,*)
       write(6,'(a)') 
       write(6,'(a)') 'Example usage:'
       write(6,*)
       write(6,'(a)') ' 1. Calculate POD of SLP from MERRA-2:'
       write(6,'(a)') '    mp_pod.x -vars SLP -alpha -1 -tmpl slp_pod Y*/M*/m2.slp*nc4'
       write(6,*)
       write(6,'(a)') 'REMARKS:'

       write(6,*)
   endif
   call ESMF_Finalize(__RC__)
   call exit(1)
   end subroutine usage_

   subroutine DefVertGrid_(CF,ak,bk,nsig,verbose,rc)
   use MAPL_Mod, only: MAPL_ROOT
   use m_mpif90,only : MP_REAL8
   use m_mpif90,only : MP_comm_rank
!-------------------------------------------------------------------------
!
! !REVISION HISTORY:
!
!-------------------------------------------------------------------------
   type(ESMF_Config)      :: CF
   integer, intent(in   ) :: nsig
   real*8 , intent(inout) :: ak(nsig+1), bk(nsig+1)
   logical, intent(in)    :: verbose
   integer, intent(out)   :: rc

!  local variables
   character(len=*), parameter       :: IAm='DefVertGrid_'
   character(len=20)                 :: vgridlabl
   character(len=3)                  :: cnsig
   integer                           :: i,k,ks,myID,ierr
   real*8                            :: ptop,pint

! start

   if (verbose) then
      if(MAPL_AM_I_ROOT()) print *,trim(Iam),': Get GSI g.c. parameters '
   endif

! Create the label to be searched for in the RC file based on nsig

   call MP_comm_rank(comm,myID,ierr)
   if(myiD==0) then
     call set_eta ( nsig,ks,ptop,pint,ak,bk )
   endif
   call mpi_bcast(ak, nsig+1,MP_REAL8,0,comm,rc)
   call mpi_bcast(bk, nsig+1,MP_REAL8,0,comm,rc)

   if (verbose) then
      if(MAPL_AM_I_ROOT()) then
         print *,trim(IAm),' - lev, ak, bk - '
         do i=1,nsig+1
            write(*,'(1x,i3,2f16.6)') i,real(ak(i),4),real(bk(i),4)
         end do
      end if
   end if
   rc=0

   end subroutine DefVertGrid_
#ifdef _OLD_
   subroutine DefVertGrid_(CF,ak,bk,nsig,verbose,rc)
!-------------------------------------------------------------------------
!
! !REVISION HISTORY:
!
!-------------------------------------------------------------------------
   type(ESMF_Config)      :: CF
   integer, intent(in   ) :: nsig
   real   , intent(inout) :: ak(nsig+1), bk(nsig+1)
   logical, intent(in)    :: verbose
   integer, intent(out)   :: rc

!  local variables
   character(len=*), parameter       :: IAm='DefVertGrid_'
   character(len=20)                 :: vgridlabl
   character(len=3)                  :: cnsig
   integer                           :: i,k
   real*8, allocatable, dimension(:) :: ak5r4,bk5r4

! start

   if (verbose) then
      if(MAPL_AM_I_ROOT()) print *,trim(Iam),': Get GSI g.c. parameters '
   endif

! Create the label to be searched for in the RC file based on nsig

   write(cnsig,'(i3.3)',iostat=STATUS)nsig
   VERIFY_(STATUS)
   vgridlabl = "AGCM_VERTGRID"//cnsig//":"

   CALL ESMF_ConfigFindLabel(CF, label = trim(vgridlabl), __RC__ )
   allocate ( ak5r4(nsig+1), bk5r4(nsig+1))
   DO i = 1, nsig+1
      CALL ESMF_ConfigNextLine    (CF, __RC__ )
      CALL ESMF_ConfigGetAttribute(CF, k, __RC__ )
      CALL ESMF_ConfigGetAttribute(CF, ak5r4(i), __RC__ )
      CALL ESMF_ConfigGetAttribute(CF, bk5r4(i), __RC__ )
      ak(i)=ak5r4(i)
      bk(i)=bk5r4(i)
   END DO
   deallocate ( ak5r4, bk5r4 )

   if (verbose) then
      if (MAPL_AM_I_ROOT()) then
         print *,trim(IAm),' - lev, ak, bk - '
         do i=1,nsig+1
            write(*,'(1x,i3,2f16.6)') i,ak(i),bk(i)
         end do
      end if
   end if

   end subroutine DefVertGrid_
#endif /* _OLD_ */

   subroutine final_
    call ESMF_FieldBundleDestroy (INBundle, __RC__)
    call ESMF_GridDestroy (InGrid, __RC__)
    deallocate(files)
    if(allocated(mean2d)) then
       deallocate(mean2d)
       deallocate(names2d)
    endif
    if(allocated(mean3d)) then
       deallocate(mean3d)
       deallocate(names3d)
    endif
    if(allocated(flds2d)) deallocate(flds2d)
    if(allocated(flds3d)) deallocate(flds3d)
    if(allocated(scal2d)) deallocate(scal2d)
    if(allocated(scal3d)) deallocate(scal3d)
    deallocate(ak,bk)
   end subroutine final_
 
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  getdim_  - Returns dimensions of dynamics vector
!
! !INTERFACE:
!
    subroutine GetDim_ ( fname, im, jm, km, lm, rc )
!
! !USES:
!
  implicit NONE
!
! !INPUT PARAMETERS:
!
 character(len=*), intent(in) :: fname   ! dyn-vector filename
!
! !OUTPUT PARAMETERS:
!
 integer, intent(out)         :: im      ! zonal dimension
 integer, intent(out)         :: jm      ! meridional dimension
 integer, intent(out)         :: km      ! vertical dimension
 integer, intent(out)         :: lm      ! "tracer" dimension

 integer, intent(out)         :: rc      ! return error code
                                                                                                                              
!
! !DESCRIPTION: This routine returns dimensions of a dynamics vector
!               read from a file.
!
! !REVISION HISTORY:
!
!  21Nov2007 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------

  integer :: myim, myjm, mykm, mylm
  integer :: fid, nvars, ngatts, ier
  integer, parameter :: READ_ONLY = 1

   rc = 0

!  Open the file
!  -------------
   call GFIO_Open ( trim(fname), READ_ONLY, fid, ier )
   if ( ier .ne. 0 ) then
     write(6,*) 'dyn_getdim: trouble reading dims from ',trim(fname)
     rc = 1
     return
   endif

!  Get dimensions
!  --------------
   call GFIO_DimInquire ( fid, myim, myjm, mykm, mylm, nvars, ngatts, ier )
   if ( ier .ne. 0 ) then
     write(6,*) 'dyn_getdim: trouble getting dims from ',trim(fname)
     rc = 2
     return
   endif

! Close GFIO file
! ---------------
  call GFIO_close ( fid, ier )

  im = myim
  jm = myjm
  km = mykm
  lm = mylm

  end subroutine getdim_

  subroutine inqNCdatetime_ ( fname, nymd, nhms )
! NOTE: since MAPL_CFIO read requires a Clock ... 
!       inquire date and time from NC file
!       and reset clock to present time
! Todling - partly stolen from reset_time.x
  use m_mpif90, only: mp_integer
  implicit none
  include 'netcdf.inc'
  character(len=*),intent(in)    :: fname
  integer,         intent(inout) :: nymd
  integer,         intent(inout) :: nhms

  integer nymdf,nhmsf,timeId
  integer fid,iret,rc,ifailed,dimsize
  character*31 dimName        ! variable name

  iret = 0
  if (MAPL_AM_I_ROOT()) then
  ! open file
    fid = ncopn (fname, NCNOWRIT, iret)
    timeId = ncvid (fid, 'time', iret)
    if (iret/=0) then
       print *, 'this is really executed ....!'
       timeId = ncdid (fid, 'time', iret)
    endif
    if (iret/=0) then
       print *, "Can't get ID for time"
       ifailed = 1
    end if
!   get time dimension
!   call ncdinq (fid, timeId, dimName, dimSize, rc)
!   read begin_date
    if(iret==0) call ncagt (fid,timeId,'begin_date',nymdf,iret)
!   read begin_time
    if(iret==0) call ncagt (fid,timeId,'begin_time',nhmsf,iret)
!   close file
    call ncclos(fid, iret)

  endif ! <ROOT>

! Distribute information read in
  call mpi_bcast(iret,   1,mp_integer  ,MAPL_root,comm,rc)
  if (iret/=0) then
  endif
  call mpi_bcast(nymdf,  1,mp_integer  ,MAPL_root,comm,rc)
  call mpi_bcast(nhmsf,  1,mp_integer  ,MAPL_root,comm,rc)
  
  nymd = nymdf 
  nhms = nhmsf 

  if (verbose) then
     if (MAPL_AM_I_ROOT()) then
         print *, 'From file: nymd, nhms =', nymdi, nhmsi
      endif
  endif

  end subroutine inqNCdatetime_
  
  logical function check_list_ (this,list)
  implicit none
  character(len=*), intent(in) :: this
  character(len=*), intent(in) :: list
  integer i,ll,ls,le
  check_list_=.false.
  if (trim(list)=="NONE") then ! this is when does not specify variables
      check_list_=.true.
  endif
  ll  = len_trim(list)
  ls  = 1
  do i=1,ll
     le=i
     if(list(i:i)==",") then
!       print*, list(ls:le-1)
        if (trim(this)==list(ls:le-1)) then
           check_list_=.true.
           exit
        endif
        ls=le+1
     endif
  enddo
! print*, list(ls:ll)  
  if (trim(this)==list(ls:ll)) then
      check_list_=.true.
  endif
  end function check_list_

  logical function defined_ ( q,undef )
  implicit none
  real     q,undef
! Check for NaNs
  if(isNan(q)) then
     q = undef
  endif
  defined_ = abs(q-undef).gt.0.1*abs(undef)
  end function defined_

! What follows is here for consistency with GMAO_hermes:
!    hermes_levels_
!    dpref_
! ------------------------------------------------------
     subroutine hermes_levels_ (lev)

     implicit none
     real,intent(inout) :: lev(:)

     integer k,km
     real ptop

!  Vertical coordinates: fake something for GrADS sake
!  ---------------------------------------------------
     km=size(lev)
     ptop = ak(1)
     lev(1) = ptop + 0.5 * dpref_(1)
     do k = 2, km
        lev(k) = lev(k-1) + 0.5 * ( dpref_(k-1) + dpref_(k) )
     end do
     lev(1:km) = lev(1:km) / 100.
     levunits = 'hPa'

     end subroutine hermes_levels_

!    Reference pressure thickness assuming ps ~ 984 hPa
!    ---------------------------------------------------
     real function dpref_ (k)
     implicit none
     integer k
     dpref_   = ( ak(k+1) - ak(k) ) + &
                ( bk(k+1) - bk(k) ) * 98400.
     end function dpref_

     subroutine writeout_fields_(fname,nam2d,nam3d,fld3d,fld2d)

     implicit none
     character(len=*),intent(in) :: fname
     real,optional,   intent(in) :: fld3d(:,:,:,:) 
     real,optional,   intent(in) :: fld2d(:,:,:) 
     character(len=*),optional,intent(in) :: nam2d(:)
     character(len=*),optional,intent(in) :: nam3d(:)

     integer m2d, m3d

      ! Create bundle to hold energy fields
      ! -----------------------------------
      EnBundle = ESMF_FieldBundleCreate ( name='Bundle', __RC__ )
      call ESMF_FieldBundleSet(EnBundle, grid=INgrid, __RC__ )

      if(present(fld2d)) then
         m2d = size(fld2d,3) 
         if(.not.present(nam2d)) then
            status=99
            VERIFY_(STATUS)
         endif
      else
         m2d = 0
      endif
      if(present(fld3d)) then
         m3d = size(fld3d,4) 
         if(.not.present(nam3d)) then
            status=99
            VERIFY_(STATUS)
         endif
      else
         m3d = 0
      endif

      !  Fill in bundle with 2d-fields first
      !  -----------------------------------
      DO ifld=1,m2d
         if (.not.associated(ptr2d)) then
            allocate(ptr2d(size(fld2d,1),size(fld2d,2)))
         endif
         ptr2d = fld2d(:,:,ifld)
         enField = ESMF_FieldCreate(grid=INgrid, fArrayptr=ptr2d, &
                                    name=trim(nam2d(ifld)), &
                                    datacopyflag=ESMF_DATACOPY_VALUE, __RC__ )
         ! the following two lines are mambo-jambo that if not present mess up MAPL output
!        call ESMF_AttributeSet(enField, NAME='VLOCATION', &
!                               VALUE=MAPL_VLocationCenter,__RC__)
!        call ESMF_AttributeSet(enField, NAME='DIMS', &
!                               VALUE=MAPL_DimsHorzVert,__RC__)
         call MAPL_FieldBundleAdd(EnBundle, enField, rc=STATUS)
      END DO

      !  Fill in bundle with 3d-fields next
      !  ----------------------------------
      DO ifld=1,m3d
         if (.not.associated(ptr3d)) then
            allocate(ptr3d(size(fld3d,1),size(fld3d,2),size(fld3d,3)))
         endif
         ptr3d = fld3d(:,:,:,ifld)
         enField = ESMF_FieldCreate(grid=INgrid, fArrayptr=ptr3d, &
                                    name=trim(nam3d(ifld)), &
                                    datacopyflag=ESMF_DATACOPY_VALUE, __RC__ )
         ! the following two lines are mambo-jambo that if not present mess up MAPL output
         call ESMF_AttributeSet(enField, NAME='VLOCATION', &
                                VALUE=MAPL_VLocationCenter,__RC__)
         call ESMF_AttributeSet(enField, NAME='DIMS', &
                                VALUE=MAPL_DimsHorzVert,__RC__)
         call MAPL_FieldBundleAdd(EnBundle, enField, rc=STATUS)
      END DO

      !  Write out energy-scaled diagnostic
      !  ----------------------------------
      allocate(levels(lm))
      call hermes_levels_ (levels)
      call MAPL_CFIOCreate ( cfio, trim(fname), clock, EnBundle,  &
                             FREQUENCY=freq, &
!_RT-> troubled line         LEVELS=levels,VUNIT=levunits,&!VCOORD='eta',&
                             DESCR='Write Fields', __RC__ )
      call MAPL_CFIOWrite ( cfio, clock, EnBundle, verbose=verbose, __RC__ )
      call MAPL_cfioDestroy ( cfio )
      deallocate(levels)

     end subroutine writeout_fields_

     subroutine rectangle_mask (mask,fld2d)

     use, intrinsic :: iso_fortran_env, only: REAL64
     implicit none

!    real, intent(in)    :: latmask(2),lonmask(2)
     real                :: latmask(2),lonmask(2)
     real, intent(in)    :: mask(:,:)
     real, intent(inout) :: fld2d(:,:)

     integer :: iml,jml
     real, pointer  :: Local(:,:)
     real(KIND=REAL64), pointer  :: R8D2(:,:)

!    First apply overall mask
!    ------------------------
     if (fmask/="NONE")then
        where(mask>0.0) fld2d=0.0
     endif

!    Then appy rectangular mask
!    --------------------------
     latmask(1) = -26
     latmask(2) =  26
     lonmask(1) = -80
     lonmask(2) = 110
     iml=size(fld2d,1)
     jml=size(fld2d,2)

     allocate(LOCAL(IML,JML),STAT=STATUS)
     VERIFY_(STATUS)

     call ESMF_GridGetCoord(ingrid, localDE=0, coordDim=1, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=R8D2, rc=status)
     VERIFY_(STATUS)

     ! TBD: got be careful w/ grid orientation
     LOCAL = 0.0
     where(R8D2*(180._REAL64/MAPL_PI_R8)<lonmask(1)) LOCAL=1.0
     where(R8D2*(180._REAL64/MAPL_PI_R8)>lonmask(2)) LOCAL=1.0
!    Apply mask to input field
     fld2d = fld2d*LOCAL

     call ESMF_GridGetCoord(ingrid, localDE=0, coordDim=2, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=R8D2, rc=status)
     VERIFY_(STATUS)

     LOCAL = 1.0
     where(R8D2*(180._REAL64/MAPL_PI_R8)<latmask(1)) LOCAL=0.0
     where(R8D2*(180._REAL64/MAPL_PI_R8)>latmask(2)) LOCAL=0.0
!    Apply mask to input field
     fld2d = fld2d*LOCAL

     deallocate(LOCAL)
     end subroutine rectangle_mask
end Program POD
