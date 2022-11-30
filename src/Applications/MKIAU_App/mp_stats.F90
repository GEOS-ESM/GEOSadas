! mp_stats.x - ESMF/MAPL application to calculate average and statistics of
!              GEOS-like lat-lon output files
!
! This program implements the following calculations:
!
!   1. First and second (diagnonal-only) moments
!   2. Energy-based error
!   3. Error fields (i.e., anomalies)
!
! The main feature of this code, as compared to GFIO_mean, is that here
! (1) and (2) are done recursively, thus requiring only a single pass 
! through the data. Furhermore, unlike GFIO_mean, the present code relies
! on MAPL/ESMF and works on a distributed domain. It also run on a single
! processor w/o need for invoking mpirun: in this mode, it''s speeds are 
! comparable to those of GFIO_mean. Results for mean and rms have been
! compared to those from GFIO_mean and are zero diff (when not running
! recursively).
!
! REMARKS: 
!   a) This program requires an RC file: mp_stats.rc (default name)
!
! TO DO: 
!
!   a) Add -anomaly option
!
! Ricardo Todling, February 2013
!............................................................................
!  !REVISION_HISTORY:  
!   28Feb2013 Todling   - initial code 
!                       - implemented recursive (one-pass) 1st/2nd moments
!                         calculations following (M2/S3) the evaluation of
!                         P. M. Neely (1966; Comm. ACM, Vol. 9, 496-499) on
!                         and the method of B. P. Welford, (1962;
!                         Technometrics 4, 419-420.
!  08Feb2013  Todling   - add knob to do general mean (such as monthly means)
!                       - add ability to remove mean (or operate w/ mean)
!                       - add ability to write out modified input fields
!  14Jan2015  Todling  redef grid name following GEOS conventions (per Atanas)
!  23Feb2017  Todling  add zeit calls
!  05Jun2020  Todling  add opt to use read-parallel
!----------------------------------------------------------------------------

#  include "MAPL_Generic.h"

   Program mp_stats

   use ESMF
   use MAPL_Mod
   use pflogger, only: pfl_initialize => initialize
   use MAPL_CFIOMod
   use m_StrTemplate, only: StrTemplate
   use m_const, only : zvir
   use m_set_eta, only : set_eta
   use m_zeit, only: zeit_ci,zeit_co,zeit_flush
   use m_zeit, only: zeit_allflush

   implicit NONE

!  Basic ESMF objects being used in this example
!  ---------------------------------------------
   character(len=*), parameter :: myname = 'MP_STATS'
   type(ESMF_Grid)         :: INGrid    ! Input Grid
   type(ESMF_Grid)         :: OUGrid    ! Output Grid
   type(ESMF_Grid)         :: ANAgrid   ! ANA Grid
   type(ESMF_FieldBundle),pointer :: InBundle(:) ! Bundle to hold read in fields
   type(ESMF_FieldBundle)  :: EnBundle  ! Bundle to hold energy
   type(ESMF_Field)        :: Field     ! Field from bundle
   type(ESMF_Field)        :: enField   ! Field holding energy

   type(ESMF_VM)           :: vm       ! ESMF Virtual Machine
   type(ESMF_Time)         :: Time     ! Time objects
   type(ESMF_Time), allocatable  :: TimeList(:)
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
   integer :: i, j, L, n, im, jm, lm, ii, nf, nb
   integer :: ifld, i2d, i3d, rank
   integer :: nfld, n2d, n3d
   integer,allocatable :: nymdi(:),nhmsi(:) ! input (auxiliar) date/time
   integer :: nymdo,nhmso              ! output date/time
   integer :: nymd,nhms                ! work date/time
   integer :: this_nymd,this_nhms      ! auxiliar date/times

   integer :: Nx, Ny                   ! Layout
   integer :: im_out, jm_out, lm_out   ! Full dimension of output fields
   integer :: freq

   integer :: comm
   integer :: method
   logical :: damp
   logical :: cubed
   logical :: vnorm
   logical :: umean
   logical :: log_transf
   logical :: log_transf_back
   real    :: val,scl,cin,cm1,smom,alpha
   real    :: eps_eer
   real    :: log_eps
   character(len=ESMF_MAXSTR)               :: levunits
   real,    pointer                         :: levels(:)
   real(8), pointer                         :: ak(:), bk(:)
   real,    pointer,     dimension(:,:)     :: ptr2d=>NULL()
   real,    pointer,     dimension(:,:,:)   :: ptr3d=>NULL()
   real   , allocatable, dimension(:,:,:)   :: umean2d ! hold 2d-mean when needed
   real   , allocatable, dimension(:,:,:,:) :: umean3d ! hold 3d-mean when needed
   real   , allocatable, dimension(:,:,:)   :: smom2d  ! hold 2d-2nd-moment (diagonal)
   real   , allocatable, dimension(:,:,:,:) :: smom3d  ! hold 2d-2nd-moment (diagonal)
   real   , allocatable, dimension(:,:,:)   :: mean2d
   real   , allocatable, dimension(:,:,:,:) :: mean3d
   real   , allocatable, dimension(:,:,:,:) :: flds4ene
   real   , allocatable, dimension(:,:,:)   :: cnt2d
   real   , allocatable, dimension(:,:,:,:) :: cnt3d

   real   , pointer    , dimension(:,:,:,:) :: debug3d

   character(len=ESMF_MAXSTR), allocatable, dimension(:) :: names2d
   character(len=ESMF_MAXSTR), allocatable, dimension(:) :: names3d

   integer, parameter :: neflds = 6                    ! number of energy-partition fields
   integer, parameter :: nkxe = 1   ! index for kinetic energy contribution
   integer, parameter :: nape = 2   ! index for avail. pot. energy contribution
   integer, parameter :: ngpe = 3   ! index for geopotential. energy contribution
   integer, parameter :: nqxe = 4   ! index for moist energy contribution
   integer, parameter :: ntxe = 5   ! index for total dry energy
   integer, parameter :: ntwe = 6   ! index for total wet energy
   character(len=*), parameter :: ene_fld_names(neflds) = (/ & ! name   of energy-partition fields
                      'kxe', 'ape', 'pse','qxe','txe','twe'/)

   logical :: fast_read
   logical :: tv2t                            ! convert virtual T to dry T
   logical :: recursive_ene                   ! when calculating energy-based
                                              ! measure do it recursively when
                                              ! .t.. NOTE: this only gives
                                              ! approx the same results as that
                                              ! of non-recursive, but it does it
                                              ! in a single pass of the data.
   logical, save :: recursive_stats=.true.    ! this is never to be made optional
                                              ! the internal option false is
                                              ! used for debugging and
                                              ! making sure results are as
                                              ! expected.
   integer  ::debug_cfioread                  ! <= 0 only cfioread
                                              ! == 1 only dynget
                                              ! == 2 both
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

   logical :: verbose,rms,stdv,doene
   integer :: id
   integer :: MAXFILES = 100
   integer :: nfiles
   integer :: blocksize
   character(len=ESMF_MAXSTR),allocatable :: files(:)
   character(len=ESMF_MAXSTR)             :: mfile     ! filename for user-splied mean
   character(len=ESMF_MAXSTR)             :: ofile     ! filename for main output (1st moments)
   character(len=ESMF_MAXSTR)             :: sfile     ! filename for 2nd moments (when recursive mean)
   character(len=ESMF_MAXSTR)             :: efile     ! members (input files to do stats over)
   character(len=ESMF_MAXSTR)             :: etmpl     ! filename template for inidividual energy fields output
   character(len=ESMF_MAXSTR)             :: ftmpl     ! filename template for output (when applicable)
   character(len=ESMF_MAXSTR)             :: outfname, myftmpl  ! aux

   character(len=ESMF_MAXSTR) :: ovars
   character(len=ESMF_MAXSTR) :: egress
   character(len=ESMF_MAXSTR) :: name
   character(len=ESMF_MAXSTR) :: myRC

   character(len=*), parameter :: Iam = 'MP_STATS'
   character(len=*), parameter :: myRC_def= 'mp_stats.rc'

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

    call zeit_ci('MP_STATS')

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
    call mapl_defgridname (IM_OUT,JM_OUT,ABKGGRIDNAME,MAPL_am_I_root())
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
    allocate(nymdi(nfiles),nhmsi(nfiles))
    do nf=1,nfiles
       call inqNCdatetime_ ( trim(files(nf)), nymdi(nf), nhmsi(nf) )
    enddo
    if (fast_read) then
       allocate(InBundle(nfiles))
       allocate(TimeList(nfiles))
       do nf=1,nfiles
          InBundle(nf) = ESMF_FieldBundleCreate ( name='Input Bundle', __RC__ )
          call ESMF_FieldBundleSet ( InBundle(nf), grid=InGrid, __RC__ )
          call set_datetime_ ( nymdi(nf), nhmsi(nf), TimeList(nf) )
       enddo
    else
       allocate(InBundle(1))
       InBundle(1) = ESMF_FieldBundleCreate ( name='Input bundle', __RC__ )
       call ESMF_FieldBundleSet(InBundle(1), grid=INgrid, __RC__ )
    endif

    call zeit_ci('CFIORead')
    if (trim(ovars)=="NONE") then
        call MAPL_CFIORead  ( trim(mfile), Time, INbundle(1), &
                              doParallel=.true.,noread=.true.,&
                              TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
    else
        call MAPL_CFIORead  ( trim(mfile), Time, INbundle(1), &
                              only_vars=ovars,                &
                              doParallel=.true.,noread=.true.,&
                              TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
    endif
    call zeit_co('CFIORead')

    call get_fields_info_ (im,jm,lm,n2d,n3d,nfld,0)

!   Testing only ...   
!   ----------------
    if (debug_cfioread>0) then
        call zeit_ci('MyRead')
        call MyRead_ ( trim(mfile), InGrid, nymd, nhms, IM_OUT, JM_OUT, LM_OUT )
        call zeit_co('MyRead')
    endif

!   Allocate number of required fields
!   ----------------------------------
    if(n2d>0) then
       allocate(mean2d(im,jm,n2d),cnt2d(im,jm,n2d))
       allocate(names2d(n2d))
       cnt2d  = 0.0
       mean2d = 0.0
    endif
    if(n3d>0) then
       allocate(mean3d(im,jm,lm,n3d),cnt3d(im,jm,lm,n3d))
       allocate(names3d(n3d))
       cnt3d  = 0.0
       mean3d = 0.0
    endif
    if (doene) then
       allocate(flds4ene(im,jm,lm,4)) ! four: u,v,t,q
       allocate(enames(4))            ! four: u,v,t,q
       allocate(pert_ps(im,jm))
       allocate(sphu(im,jm,lm),tv(im,jm,lm),delp(im,jm,lm))
       allocate(energy_accums(im,jm,lm,neflds))
       allocate(energy_fields(im,jm,lm,neflds))
       allocate(jweights_glb(jm_out,2),jweights_lcl(jm,2))
       energy_accums=0.0
       energy_fields=0.0
       call horiz_grid_(jm_out,jweights_glb)       ! global array with latitude weights
       call gbl2lcl_(INgrid,jweights_glb,jweights_lcl) ! get lat weights in the local domain
    endif

!   call it a second time to collect var names
!   ------------------------------------------
    call get_fields_info_ (im,jm,lm,n2d,n3d,nfld,1)

!   If fast read ...
!   ----------------
    if (fast_read) then
       call zeit_ci('CFIORead')
       call MAPL_CFIOReadParallel(InBundle,files(1:nfiles),Time,blocksize=blocksize,timelist=TimeList, __RC__ )
       call zeit_co('CFIORead')
    endif

!   If recursive mean, rms and/or stdv are required ...
!   ---------------------------------------------------
    if (.not.skip_main) then

!    Loop over files ...
!    -------------------
     DO nf = 1,nfiles

!      Check date/time and adjust clock if needed
!      ------------------------------------------
       if (fast_read) then
          nb=nf
       else
          nb=1
          if(nymdi(nf)/=nymd.or.nhmsi(nf)/=nhms) then
             call set_datetime_  ( nymdi(nf), nhmsi(nf) )
             CLOCK = ESMF_ClockCreate ( name="StatsClock", timeStep=TimeStep, startTime=Time, __RC__ )
             nymd=nymdi(nf);nhms=nhmsi(nf)
          endif
          if (debug_cfioread<=0.or.debug_cfioread>1) then
             call zeit_ci('CFIORead')
             if (trim(ovars)=="NONE") then
                 call MAPL_CFIORead  ( trim(files(nf)), Time, INbundle(nb), &
                                       doParallel=.true.,                  &
                                       TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
             else
                 call MAPL_CFIORead  ( trim(files(nf)), Time, INbundle(nb), &
                                       only_vars=ovars,                    &
                                       doParallel=.true.,                  &
                                       TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
             endif
             call zeit_co('CFIORead')
          endif

!         Testing only ...   
!         ----------------
          if (debug_cfioread>=1) then
             if(.not.associated(debug3d)) allocate(debug3d(im,jm,lm,10))
             call zeit_ci('MyRead')
             call MyRead_ ( trim(files(nf)), InGrid, nymd, nhms, IM_OUT, JM_OUT, LM_OUT, debug3d )
             call zeit_co('MyRead')
          endif
       endif

!      If so, save qv
!      --------------
       if (tv2t) then
          DO ifld=1,nfld
              call ESMF_FieldBundleGet(INbundle(nb), ifld, Field, __RC__ )
              call ESMF_FieldGet(Field, NAME=NAME, dimCount = rank, __RC__ )
              if (rank==2) cycle
                 call ESMF_FieldGet(Field, farrayPtr=ptr3d, __RC__ )
                 if(trim(NAME)=='sphu') then
                    if(.not.allocated(sphu)) allocate(sphu(im,jm,lm))
                    sphu = ptr3d
                 endif
          END DO
       endif

!      Reset variables
!      ---------------
       id  = 0

!      Accumulate sums
!      ---------------
       DO ifld=1,nfld
           call ESMF_FieldBundleGet(INbundle(nb), ifld, Field, __RC__ )
           call ESMF_FieldGet(Field, NAME=NAME, dimCount = rank, __RC__ )
           if (.not. check_list_(NAME,ovars)) cycle
           if (rank==2) then
             i2d=getindex_(trim(NAME),names2d)
             call ESMF_FieldGet(Field, farrayPtr=ptr2d, __RC__ )
             do j=1,jm
                do i=1,im
                   if(defined_(ptr2d(i,j),MAPL_UNDEF)) then ! mimic GFIO_mean for consistency
                      cnt2d(i,j,i2d) = cnt2d(i,j,i2d) + 1.0
                      cin = 1.0/cnt2d(i,j,i2d)
                      cm1 = cnt2d(i,j,i2d)-1.0
                      if(log_transf) then
                         val = log(ptr2d(i,j)+log_eps)
                      else
                         val = ptr2d(i,j)
                      endif
                      if (stdv) then
                         if (recursive_stats) then
                             smom = val - mean2d(i,j,i2d); smom = smom*smom
                             smom2d(i,j,i2d) = smom2d(i,j,i2d) + cin*cm1*smom
                         else
                             val = val - umean2d(i,j,i2d) 
                         endif
                      endif
                      if(rms) then
                         val = val * val
                      endif
                      if (recursive_stats) then
                          mean2d(i,j,i2d) = cm1*mean2d(i,j,i2d) + val
                          mean2d(i,j,i2d) = cin*mean2d(i,j,i2d)
                      else
                          mean2d(i,j,i2d) = mean2d(i,j,i2d) + val
                      endif
                   else ! not defined ...
                      cnt2d (i,j,i2d) = 0
                      mean2d(i,j,i2d) = MAPL_UNDEF
                      if (stdv.and.recursive_stats) then
                         smom2d(i,j,i2d) = MAPL_UNDEF
                      endif
                   endif
                enddo
             enddo
             ! If required fields for energy calculate
             !----------------------------------------
             if (doene.and.recursive_ene) then
                if(trim(NAME)=='ps') then
                   if(umean) then
                      pert_ps=ptr2d-umean2d(:,:,i2d) ! ps anomaly/error
                   else
                      pert_ps=ptr2d- mean2d(:,:,i2d) ! ps anomaly/error
                   endif
                endif
             endif
           endif ! <rank=2>
           if (rank==3) then
             i3d=getindex_(trim(NAME),names3d)
             call ESMF_FieldGet(Field, farrayPtr=ptr3d, __RC__ )
             if(debug_cfioread)then
               if(trim(NAME)=='delp') ptr3d => debug3d(:,:,:,1)
               if(trim(NAME)=='u'   ) ptr3d => debug3d(:,:,:,2)
               if(trim(NAME)=='v'   ) ptr3d => debug3d(:,:,:,3)
               if(trim(NAME)=='tv'  ) ptr3d => debug3d(:,:,:,4)
             endif
             if (tv2t) then
                ptr3d = ptr3d / ( 1.0+zvir*sphu)
             endif
             do L=1,lm
                do j=1,jm
                   do i=1,im
                      if(defined_(ptr3d(i,j,L),MAPL_UNDEF)) then ! mimic GFIO_mean for consistency
                         cnt3d(i,j,L,i3d) = cnt3d(i,j,L,i3d) + 1.0
                         cin = 1.0/cnt3d(i,j,L,i3d)
                         cm1 = cnt3d(i,j,L,i3d)-1.0
                         val = ptr3d(i,j,L)
                         if (stdv) then
                             if (recursive_stats) then
                                 smom = val - mean3d(i,j,L,i3d); smom = smom*smom
                                 smom3d(i,j,L,i3d) = smom3d(i,j,L,i3d) + cin*cm1*smom
                             else
                                 val = val - umean3d(i,j,L,i3d) 
                             endif
                         endif
                         if(rms) then
                            val = val * val
                         endif
                         if (recursive_stats) then
                             mean3d(i,j,L,i3d) = cm1*mean3d(i,j,L,i3d) + val
                             mean3d(i,j,L,i3d) = cin*mean3d(i,j,L,i3d)
                         else
                             mean3d(i,j,L,i3d) = mean3d(i,j,L,i3d) + val
                         endif
                      else ! not defined ...
                         cnt3d (i,j,L,i3d) = 0
                         mean3d(i,j,L,i3d) = MAPL_UNDEF
                         if (stdv.and.recursive_stats) then
                            smom3d(i,j,L,i3d) = MAPL_UNDEF
                         endif
                      end if
                  end do ! <i>
                end do ! <j>
             end do ! <L>
             ! If required fields for energy calculate
             !----------------------------------------
             if (doene.and.recursive_ene) then
                call save4ene_ ( NAME, i3d, id, enames, flds4ene )
             endif
           end if ! <rank=3>
       END DO

!      Now having collected all required fields, calculate energy-fields when needed
!      ------------------------------------------------------------------------------
       if (doene.and.recursive_ene) then
           energy_fields=0.0
           call compute_e_field_ (jweights_lcl(:,2),real(ak(1),4),eps_eer,'a',vnorm,  &
                                  .false.,flds4ene,delp,pert_ps,enames,   &
                                  energy_fields,.not.tv2t,rc) 
           energy_accums = (nf-1.0)*energy_accums + energy_fields
           energy_accums = energy_accums/float(nf)
       endif

     END DO ! <files>
 
    end if ! <skip_main>

!  If calculation of energy-measure is being done non-recusively, do it now
!  ------------------------------------------------------------------------
   if (.not.recursive_ene) then
     if (doene) then

!      Loop over files ...
!      -------------------
       DO nf = 1,nfiles

!         Check date/time and adjust clock if needed
!         ------------------------------------------
          if(nymdi(nf)/=nymd.or.nhmsi(nf)/=nhms) then
             call set_datetime_  ( nymdi(nf), nhmsi(nf) )
             CLOCK = ESMF_ClockCreate ( name="StatsClock", timeStep=TimeStep, startTime=Time, __RC__ )
             nymd=nymdi(nf);nhms=nhmsi(nf)
          endif
          if (fast_read) then
             nb=nf
          else
             nb=1
             if (trim(ovars)=="NONE") then
                 call MAPL_CFIORead  ( trim(files(nf)), Time, INbundle(nb), &
                                       TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
             else
                 call MAPL_CFIORead  ( trim(files(nf)), Time, INbundle(nb), &
                                       only_vars=ovars, &
                                       TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
             endif
          endif

!         Reset variables
!         ---------------
          id  = 0

!         Accumulate sums
!         ---------------
          DO ifld=1,nfld
              call ESMF_FieldBundleGet(INbundle(nb), ifld, Field, __RC__ )
              call ESMF_FieldGet(Field, NAME=NAME, dimCount = rank, __RC__ )
              if (.not. check_list_(NAME,ovars)) cycle
              if (rank==2) then
                i2d=getindex_(trim(NAME),names2d)
                call ESMF_FieldGet(Field, farrayPtr=ptr2d, __RC__ )
                ! If required fields for energy calculate
                !----------------------------------------
                if(trim(NAME)=='ps') then
                   if(umean) then
                      pert_ps=ptr2d-umean2d(:,:,i2d) ! ps anomaly/error
                   else
                      pert_ps=ptr2d- mean2d(:,:,i2d) ! ps anomaly/error
                   endif
                endif
              endif ! <rank=2>
              if (rank==3) then
                i3d=getindex_(trim(NAME),names3d)
                call ESMF_FieldGet(Field, farrayPtr=ptr3d, __RC__ )
                ! If required fields for energy calculate
                !----------------------------------------
                call save4ene_ ( NAME, i3d, id, enames, flds4ene )
              end if ! <rank=3>
          END DO
   
!         Now having collected all required fields, calculate energy-fields when needed
!         ------------------------------------------------------------------------------
          energy_fields=0.0
          call compute_e_field_ (jweights_lcl(:,2),real(ak(1),4),eps_eer,'a',vnorm,  &
                                 .true.,flds4ene,delp,pert_ps,enames,    &
                                 energy_fields,.true.,rc) 
          energy_accums = (nf-1.0)*energy_accums + energy_fields
          energy_accums = energy_accums/float(nf)
          if (trim(etmpl)/="NONE") then
              write(myftmpl,'(2a,i3.3,a)') trim(etmpl), '.', nf, '.nc4'
              call StrTemplate ( outfname, myftmpl, 'GRADS', nymd=nymd, nhms=nhms, stat=status)
              call writeout_energy_(outfname,energy_fields)
          endif
      END DO ! <files>
     endif ! <doene>
   endif ! <.not.recursive_ene>

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

!  Write out first moment statistics
!  ---------------------------------
   if (trim(ofile) /= "NONE" ) then

      ! Copy 1st moments to bundle
      ! --------------------------
      if (log_transf.and.log_transf_back) then
         call fields2bundle_ ( 1, mean2d, mean3d, s2d=smom2d, s3d=smom3d )
      else
         call fields2bundle_ ( 1, mean2d, mean3d )
      endif

      ! Write out bundle
      ! ----------------
      call MAPL_CFIOCreate ( cfio, trim(ofile), clock, InBundle(1),  &
                             FREQUENCY=freq, &
                             DESCR='Write Stats Fields', __RC__ )
      call MAPL_CFIOWrite ( cfio, clock, InBundle(1), verbose=verbose, __RC__ )
      call MAPL_cfioDestroy ( cfio )
   endif

!  Write out second moment (diagonal-only) statistics
!  --------------------------------------------------
   if (trim(sfile) /= "NONE" ) then

      ! Copy 2nd moments to bundle
      ! --------------------------
      if (log_transf.and.log_transf_back) then
         call fields2bundle_ ( 2, mean2d, mean3d, s2d=smom2d, s3d=smom3d )
      else
         call fields2bundle_ ( 2, smom2d, smom3d )
      endif

      ! Write out bundle
      ! ----------------
      call MAPL_CFIOCreate ( cfio, trim(sfile), clock, InBundle(1),  &
                             FREQUENCY=freq, &
                             DESCR='Write Stats Fields', __RC__ )
      call MAPL_CFIOWrite ( cfio, clock, InBundle(1), verbose=verbose, __RC__ )
      call MAPL_cfioDestroy ( cfio )
   endif

!  Write out de-biased files by removing the mean (or do: xin = xin + alpha * mean_x)
!  ----------------------------------------------
   if (trim(ftmpl) /= "NONE" ) then

!      Loop over files ...
!      -------------------
       DO nf = 1,nfiles

          if (fast_read) then
             nb=nf
          else
             ! Check date/time and adjust clock if needed
             ! ------------------------------------------
             if(nymdi(nf)/=nymd.or.nhmsi(nf)/=nhms) then
                call set_datetime_  ( nymdi(nf), nhmsi(nf) )
                CLOCK = ESMF_ClockCreate ( name="StatsClock", timeStep=TimeStep, startTime=Time, __RC__ )
                nymd=nymdi(nf);nhms=nhmsi(nf)
             endif
             nb=1
             if (trim(ovars)=="NONE") then
                 call MAPL_CFIORead  ( trim(files(nf)), Time, INbundle(nb), &
                                       TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
             else
                 call MAPL_CFIORead  ( trim(files(nf)), Time, INbundle(nb), &
                                       only_vars=ovars, &
                                       TIME_IS_CYCLIC=.false., verbose=verbose, __RC__ )
             endif
          endif

          ! Remove mean (i.e., do: xin = xin + alpha * mean_x)
          ! --------------------------------------------------
          if (umean) then
             call saxpy_ (nb, alpha, umean2d, umean3d)
          else
             call saxpy_ (nb, alpha,  mean2d,  mean3d)
          endif

          ! Write out bundle
          ! ----------------
          if(nymdo>0.and.nhmso>=0) then
             call set_datetime_  ( nymdo, nhmso )
             CLOCK = ESMF_ClockCreate ( name="StatsClock", timeStep=TimeStep, startTime=Time, __RC__ )
             nymd=nymdo;nhms=nhmso
          endif
          call StrTemplate ( myftmpl, ftmpl, 'GRADS', nymd=nymd, nhms=nhms, stat=status)
          if (append_counter) then
             write(outfname,'(2a,i3.3,a)') trim(myftmpl), '.', nf, '.nc4'
          else
             write(outfname,'(2a)') trim(myftmpl), '.nc4'
          endif
          call MAPL_CFIOCreate ( cfio, trim(outfname), clock, InBundle(nb),  &
                                 FREQUENCY=freq, &
                                 DESCR='Write Stats Fields', __RC__ )
          call MAPL_CFIOWrite ( cfio, clock, InBundle(nb), verbose=verbose, __RC__ )
          call MAPL_cfioDestroy ( cfio )

      enddo

   endif

!  If energy fields are available, write them out now
!  --------------------------------------------------
   if (doene) then
      ! Write out accumulated estimate of energy-based measure
      ! ------------------------------------------------------
      if(MAPL_AM_I_ROOT()) print*, 'Accumulated energy-based measure results:'
      call echo_ene_(energy_accums)

      call writeout_energy_(efile,fld3d=energy_accums)
   endif

!   All done
!   --------
    if (MAPL_AM_I_ROOT()) then
        close(999)
        open (999,file=trim(egress),form='formatted')
        close(999)
    end if
    call zeit_co('MP_STATS')
    if(MAPL_AM_I_ROOT()) call zeit_flush(6,subname_at_end=.true.)
    call final_
    call ESMF_Finalize(__RC__)

  end subroutine Main

  subroutine get_fields_info_ (im,jm,lm,n2d,n3d,nfld,npass)

  implicit none
  integer,intent(in)  :: npass
  integer,intent(out) :: im,jm,lm,n2d,n3d,nfld

  integer j2d,j3d
  logical read2d,read3d
    
! First get dims ...
! ------------------
    read2d=.false.
    read3d=.false.
    call ESMF_FieldBundleGet(INbundle(1),FieldCount=nfld, __RC__ )
    n2d=0;n3d=0
    DO ifld=1,nfld
       call ESMF_FieldBundleGet(INbundle(1), ifld, Field, __RC__ )
       call ESMF_FieldGet(Field, NAME=NAME, dimCount=rank, __RC__ )
       if (.not. check_list_(NAME,ovars)) cycle
       if(rank==2) then
          n2d=n2d+1
          if (.not.read2d) then ! only need read once to get dims
             call ESMF_FieldGet(Field, farrayPtr=ptr2d, __RC__ )
             im=size(ptr2d,1)
             jm=size(ptr2d,2)
             read2d=.true.
          endif
          if(allocated(names2d)) names2d(n2d)=trim(NAME)
       endif
       if(rank==3) then
          n3d=n3d+1
          if (.not.read3d) then ! only need read once to get dims
             call ESMF_FieldGet(Field, farrayPtr=ptr3d, __RC__ )
             im=size(ptr3d,1)
             jm=size(ptr3d,2)
             lm=size(ptr3d,3)
             read3d=.true.
          endif
          if(allocated(names3d)) names3d(n3d)=trim(NAME)
       endif
    END DO

    if(npass>0) return

!   If needed, store mean ...
!   -------------------------
!   if (stdv.or.doene) then
       if(umean) then
          if(n2d>0) then
             allocate(umean2d(im,jm,n2d))
             umean2d = 0.0
          endif
          if(n3d>0) then
             allocate(umean3d(im,jm,lm,n3d))
             umean3d = 0.0
          endif
          call ESMF_FieldBundleGet(INbundle(1),FieldCount=nfld, __RC__ )
          j2d=0;j3d=0
          DO ifld=1,nfld
             call ESMF_FieldBundleGet(INbundle(1), ifld, Field, __RC__ )
             call ESMF_FieldGet(Field, NAME=NAME, dimCount=rank, __RC__ )
             if (.not. check_list_(NAME,ovars)) cycle
             if(rank==2) then
                j2d=j2d+1
                call ESMF_FieldGet(Field, farrayPtr=ptr2d, __RC__ )
                umean2d(:,:,j2d) = ptr2d
             endif
             if(rank==3) then
                j3d=j3d+1
                call ESMF_FieldGet(Field, farrayPtr=ptr3d, __RC__ )
                umean3d(:,:,:,j3d) = ptr3d
             endif
          END DO
       else ! <.not.umean>
    if (stdv.or.doene) then
          if(n2d>0) then
             allocate(smom2d(im,jm,n2d))
             smom2d = 0.0
          endif
          if(n3d>0) then
             allocate(smom3d(im,jm,lm,n3d))
             smom3d = 0.0
          endif
    endif ! <stdv>
       endif ! <umean>
!   endif ! <stdv>

  end subroutine get_fields_info_

  subroutine fields2bundle_ (sec_mom,x2d,x3d,s2d,s3d)

  use m_die, only: MP_die
  implicit none
  character(len=*),parameter::myname_=myname//'*fields2bundle_'
  integer,intent(in) :: sec_mom ! 1=1st moment; 2=2nd moment
  real,intent(in) :: x2d(:,:,:)
  real,intent(in) :: x3d(:,:,:,:)
  real,intent(in),optional :: s2d(:,:,:)
  real,intent(in),optional :: s3d(:,:,:,:)

  integer j2d,j3d,ierr

  ! Place stats in Inbundle
  j2d = 0
  j3d = 0
  DO ifld=1,nfld
     call ESMF_FieldBundleGet(INbundle(1), ifld, Field, __RC__ )
     call ESMF_FieldGet(Field, NAME=NAME, dimCount=rank, __RC__ )
     if (.not. check_list_(NAME,ovars)) cycle
     if (rank == 2) then
       j2d=getindex_(trim(NAME),names2d)
       call ESMF_FieldGet(Field, farrayPtr=ptr2d, __RC__ )
       if (log_transf.and.log_transf_back) then
          if(.not.present(s2d)) then
             call MP_die(myname_,'Alloc(s2d)',999)
          endif
          if (sec_mom==1) then
              where(cnt2d(:,:,j2d)>0.0)
                !ptr2d = max(log_eps,exp(x2d(:,:,j2d)                                )-log_eps)
                 ptr2d = max(log_eps,exp(x2d(:,:,j2d)+0.5*s2d(:,:,j2d)/cnt2d(:,:,j2d))-log_eps)
              endwhere
          else if (sec_mom==2) then
              ! what about eps?
              where(cnt2d(:,:,j2d)>0.0)
                 ptr2d = exp(2.0*x2d(:,:,j2d)+s2d(:,:,j2d)/cnt2d(:,:,j2d))*(exp(s2d(:,:,j2d)/cnt2d(:,:,j2d))-1.0)
              endwhere
          else
             call MP_die(myname_,'Alloc(sec_mom-2d)',999)
          endif
       else
          ptr2d = x2d(:,:,j2d)
       endif
       if (.not.recursive_stats) then
          where(cnt2d(:,:,j2d)>0.0)
             ptr2d = ptr2d/cnt2d(:,:,j2d)
          endwhere
       endif
       if (sec_mom==2) then
          if (rms) then
             where(cnt2d(:,:,j2d)>0.0)
                ptr2d = sqrt(ptr2d)
             endwhere
          endif
          if (stdv) then
             where(cnt2d(:,:,j2d)>0.0)
                ptr2d = sqrt(ptr2d/cnt2d(:,:,j2d))
             endwhere
          endif
       endif ! <sec_mom>
     endif ! <rank=2>
     if (rank == 3) then
       j3d=getindex_(trim(NAME),names3d)
       call ESMF_FieldGet(Field, farrayPtr=ptr3d, __RC__ )
       ptr3d = 0.0
       if (log_transf.and.log_transf_back) then
          if(.not.present(s3d)) then
             call MP_die(myname_,'Alloc(s3d)',999)
          endif
          if (sec_mom==1) then
              where(cnt3d(:,:,:,j3d)>0.0)
                 ptr3d = max(log_eps,exp(x3d(:,:,:,j3d)+0.5*s3d(:,:,:,j2d)/cnt3d(:,:,:,j3d))-log_eps)
              endwhere
          else if (sec_mom==2) then
              ! what about eps?
              where(cnt3d(:,:,:,j3d)>0.0)
                  ptr3d = exp(2.0*x3d(:,:,:,j3d)+s3d(:,:,:,j3d)/cnt3d(:,:,:,j3d))*(exp(s3d(:,:,:,j3d)/cnt3d(:,:,:,j3d))-1.0)
              endwhere
          else
             call MP_die(myname_,'Alloc(sec_mom-3d)',999)
          endif
       else
          ptr3d = x3d(:,:,:,j3d)
       endif
       if (.not.recursive_stats) then
          where(cnt3d(:,:,:,j3d)>0.0)
             ptr3d = ptr3d/cnt3d(:,:,:,j3d)
          endwhere
       endif
       if (sec_mom==2) then
          if (rms) then
             where(cnt3d(:,:,:,j3d)>0.0)
                ptr3d = sqrt(ptr3d)
             endwhere
          endif
          if (stdv) then
             where(cnt3d(:,:,:,j3d)>0.0)
                ptr3d = sqrt(ptr3d/cnt3d(:,:,:,j3d))
             endwhere
          endif
       endif ! <sec_mom>
     end if ! <rank=3>
  END DO

  end subroutine fields2bundle_

  subroutine saxpy_ (mb,alpha,x2d,x3d)

  implicit none
  integer,intent(in) :: mb
  real,intent(in) :: alpha ! scaling coefficient
  real,intent(in) :: x2d(:,:,:)
  real,intent(in) :: x3d(:,:,:,:)

  integer ii,jj,kk
  integer i,j,k
  integer j2d,j3d

  ! Place stats in Inbundle
  DO ifld=1,nfld
     call ESMF_FieldBundleGet(INbundle(mb), ifld, Field, __RC__ )
     call ESMF_FieldGet(Field, NAME=NAME, dimCount=rank, __RC__ )
     if (.not. check_list_(NAME,ovars)) cycle
     if (rank == 2) then
       j2d=getindex_(trim(NAME),names2d)
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
       j3d=getindex_(trim(NAME),names3d)
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

!   Allocate number of required fields
!BOP
! !ROUTINE: init_: initialize mp_stats
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
    character(len=ESMF_MAXSTR) :: aux

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

   call ESMF_ConfigGetAttribute( CF, BLOCKSIZE, label ='MP_STATS_READ_BLOCKSIZE:', DEFAULT=4, __RC__ )

   call ESMF_ConfigGetAttribute( CF, eps_eer  , label ='EPS_EER:', DEFAULT=0.0, __RC__ )
   call ESMF_ConfigGetAttribute( CF, aux      , label ='VNORM:'  , DEFAULT="NO", __RC__ )
   if(trim(aux)/="NO") then 
      vnorm=.true.
   else
      vnorm=.false.
   endif
   call ESMF_ConfigGetAttribute( CF, aux, label ='MP_STATS_FAST_READ:', DEFAULT="NO", __RC__ )
   if(trim(aux)/="NO") then 
      fast_read=.true.
   else
      fast_read=.false.
   endif

   call ESMF_ConfigGetAttribute( CF, debug_cfioread, label ='DEBUG_CFIOREAD:', DEFAULT=0, __RC__ )

!  get dims
!  --------
!  call getdim_ ( bkgfname, IM_BKG, JM_BKG, LM_BKG, idum, status )

!  define vertical grid (should be put in the grid ...)
!  ----------------------------------------------------
   allocate(ak(LM_OUT+1),bk(LM_OUT+1))
   call DefVertGrid_(CF,ak,bk,LM_OUT,verbose,status)

   end subroutine init_

   subroutine set_datetime_ (nymd, nhms, This_Time)

   implicit none
   integer, intent(in) :: nymd, nhms
   type(ESMF_Time),optional :: This_Time

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
   if (present(This_Time))then
     call ESMF_TimeSet(This_Time, yy=thistime(1), mm=thistime(2), dd=thistime(3), &
                                   h=thistime(4), m =thistime(5),  s=thistime(6))
   else
     call ESMF_TimeSet(Time, yy=thistime(1), mm=thistime(2), dd=thistime(3), &
                              h=thistime(4), m =thistime(5),  s=thistime(6))
   endif
   if (fast_read) then
     call ESMF_TimeIntervalSet( TimeStep, h=0, m=0, s=0, __RC__ )
   else
     call ESMF_TimeIntervalSet( TimeStep, h=6, m=0, s=0, __RC__ )
   endif 

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
   alpha   =  0.0
   log_eps =  0.0
   egress  = 'MP_STATS_EGRESS'
   nymd_   = -1
   nhms_   = -1
   freq_   = -1
   myRC    = "NONE"
   verbose = .false.
   rms     = .false.
   stdv    = .false.
   umean   = .false.
   doene   = .false.
   efile   = "NONE"
   etmpl   = "NONE"
   ftmpl   = "NONE"
   ofile   = "NONE"
   sfile   = "NONE"
   mfile   = "NONE"
   ovars   = "NONE"
   tv2t    = .false.
   skip_main     = .false.
   recursive_ene = .true.
   append_counter= .false.
   log_transf      = .false.
   log_transf_back = .true.

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
           case ("-ene")
             iarg = iarg + 1
             call GetArg ( iArg, efile )
             doene = .true.
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
           case ("-etmpl")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, etmpl )
           case ("-inc")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv,*) freq_
           case ("-o")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, ofile )
           case ("-h")
             if ( iarg+1 .gt. argc ) call usage_()
           case ("-nonrecene")
             recursive_ene = .false.
           case ("-tv2t")
             tv2t = .true.
           case ("-log_transf")
             log_transf = .true.
           case ("-dont_log_transf_back")
             log_transf_back = .false.
           case ("-log_eps")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, argv )
             read(argv,*) log_eps
           case ("-rc")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, myRC )
           case ("-rms")
             rms = .true.
           case ("-stdv")
             iarg = iarg + 1
             call GetArg ( iArg, sfile )
             stdv = .true.
           case ("-tmpl")
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call GetArg ( iArg, ftmpl )
           case ("-vars")
             iarg = iarg + 1
             call GetArg ( iArg, ovars )
           case ("-usrmean")
             iarg = iarg + 1
             call GetArg ( iArg, mfile )
             umean = .true.
           case ("-verbose")
             verbose = .true.
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
      print *, 'user-supplied mean:          ', trim(mfile) 
      if(ofile/="NONE") &
      print *, 'first  moment output file:   ', trim(ofile)
      if(sfile/="NONE") &
      print *, 'second moment output file:   ', trim(sfile) 
      if(ftmpl/="NONE") &
      print *, 'template for output members: ', trim(ftmpl) 
      if(etmpl/="NONE") &
      print *, 'template for output energy:  ', trim(etmpl) 
      print *
      print *, 'NOTE: Yes, I will read first file twice'
      if(log_transf) then
         print * 
         print *, 'Will transform variable(s) to log+eps, calc mean/etc, ...'
         print *, 'and transform back'
         print * 
      endif

   endif ! <ROOT section>

!  Broadcast command line
!  ----------------------
   call mpi_bcast(nfiles,        1,mp_integer  ,MAPL_root,comm,iret)
   call mpi_bcast(nymd_ ,        1,mp_integer  ,MAPL_root,comm,iret)
   call mpi_bcast(nhms_ ,        1,mp_integer  ,MAPL_root,comm,iret)
   call mpi_bcast(freq_ ,        1,mp_integer  ,MAPL_root,comm,iret)
   call mpi_bcast(alpha ,        1,mp_real4    ,MAPL_root,comm,iret)
   call mpi_bcast(log_eps,       1,mp_real4    ,MAPL_root,comm,iret)
   call mpi_bcast(rms   ,        1,mp_logical  ,MAPL_root,comm,iret)
   call mpi_bcast(stdv  ,        1,mp_logical  ,MAPL_root,comm,iret)
   call mpi_bcast(umean ,        1,mp_logical  ,MAPL_root,comm,iret)
   call mpi_bcast(doene ,        1,mp_logical  ,MAPL_root,comm,iret)
   call mpi_bcast(verbose,       1,mp_logical  ,MAPL_root,comm,iret)
   call mpi_bcast(recursive_ene, 1,mp_logical  ,MAPL_root,comm,iret)
   call mpi_bcast(tv2t,          1,mp_logical  ,MAPL_root,comm,iret)
   call mpi_bcast(log_transf,    1,mp_logical  ,MAPL_root,comm,iret)
   call mpi_bcast(log_transf_back,1,mp_logical ,MAPL_root,comm,iret)
   do i = 1,nfiles
      call mpi_bcast(files(i),ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   enddo
   call mpi_bcast(ovars ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(ofile ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(mfile ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(etmpl ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(ftmpl ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(myRC  ,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
   call mpi_bcast(egress,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)

!  Define RC file
!  --------------
   if(myRC=="NONE") myRC = myRC_def

!  Possibly overwrite date/time w/ user''s specification
!  ----------------------------------------------------
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
   if (stdv) then
      call mpi_bcast(sfile,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
      if(recursive_stats) then
         rms = .false.
      else
         rms = .true.
      endif
   endif
   if (doene) then
      ! this only makes sense when applied to anomalies (errors)
      ! so stdv better be on, but I will not enforce this since
      ! program could be reading pre-computed perturbation
      call mpi_bcast(efile,ESMF_MAXSTR,mp_character,MAPL_root,comm,iret)
      if( MAPL_AM_I_ROOT() ) then
         print*
         if (recursive_ene) then
            print*, 'NOTE: Energy-based measure calculated recursively.' 
            print*, '      Results are reasonable approx to non-recursive calc'
            print*, '      when number of samples is reasonable with the added'
            print*, '      bonus of passing through the data only once.'
         else
            print*, 'NOTE: De-activated recursive calculation of energy-based measure.'
            print*, '      Will sweep through the data twice.'
         endif
         print*
      endif ! <root>
   endif

   if (trim(mfile)=="NONE" ) then
      mfile = files(1)
   endif

   if(trim(ftmpl)/="NONE".or.trim(etmpl)/="NONE") then ! make sure -date has not been specified by user
     if(nymd_>0.and.nhms_>=0) then
        append_counter=.true.
     endif
   endif

   if(trim(etmpl)/="NONE") doene=.true.

   if (umean) then             ! when mean is provided by user ...
       if(.not.rms) then       ! ... and rms is not requested ...
          if(.not.stdv) then   ! ... and stdv is also not requested
             skip_main=.true.  ! then bypass main part of usual calculation
          endif
       endif
   endif

   end subroutine cmdline_

   subroutine usage_

   use MAPL_Mod, only: MAPL_AM_I_ROOT
   implicit none
   if( MAPL_AM_I_ROOT() ) then
       write(6,*)
       write(6,'(a)') 'Usage: mp_stat.x [options] files'
       write(6,*)
       write(6,'(a)') 'options:'
       write(6,*)
       write(6,'(a)') '-o     FILE       specify output filename'
       write(6,'(a)') '-alpha NUMBER     specify multiplicative coeff to scale mean '
       write(6,'(a)') '                    before adding result to each file read in'
       write(6,'(a)') '                    (see -tmpl)'
       write(6,'(a)') '-date  NYMD NHMS  date/time of output file(s)'
       write(6,'(a)') '                    (when absent use date of last file read)'
       write(6,'(a)') '-ene   FILE       spefify output file containing energy-based'
       write(6,'(a)') '                    measure (NOTE: this triggers the calculation)'
       write(6,'(a)') '-etmpl ENEFTMPL   template for individual energy estimates for each member'
       write(6,'(a)') '                    (e.g., -etmpl myenergy.%y4%m2%d2_%h2z)     '
       write(6,'(a)') '-nonrecene        de-activate recursive calculation of energy measure'
       write(6,'(a)') '                     (NOTE: this will sweep through the data twice)'
       write(6,'(a)') '-rms              calculate rms'
       write(6,'(a)') '-stdv  FILE       provide filename of output stdv'
       write(6,'(a)') '-umean FILE       provide available estimate of mean'
       write(6,'(a)') '                    (only used in non-recursive calcuation'
       write(6,'(a)') '                     of standard deviations)'
       write(6,'(a)') '-tmpl  FNAMETMPL  specify filename template of output files'
       write(6,'(a)') '                    NOTE: do not provide filename extension'
       write(6,'(a)') '                          (nc4 will be appended to name)'
       write(6,'(a)') '                    (e.g., -tmpl myfiles.%y4%m2%d2_%h2z)     '
       write(6,'(a)') '-vars  LIST       where LIST is a list of variable separate' 
       write(6,*)
       write(6,'(a)') 
       write(6,'(a)') 'Example usage:'
       write(6,*)
       write(6,'(a)') ' 1. Obtain mean:'
       write(6,'(a)') '    mp_stats.x -o mean.nc4 mem0*/hy05a.bkg.eta.20120410_00z.nc4'
       write(6,*)
       write(6,'(a)') '    1a. calculating monthly means (i.e., files from diff times)'
       write(6,'(a)') '        can be done by specifying date of output file, e.g.,   '
       write(6,'(a)') '    mp_stats.x -o apr_mean.nc4 -date 20120401 0 mem0*/hy05a.bkg.eta.201204*z.nc4'
       write(6,*)
       write(6,'(a)') ' 2. Recursively obtain rms from original fields:'
       write(6,'(a)') '    mp_stats.x -o rms.nc4 -rms mem0*/hy05a.bkg.eta.20120410_00z.nc4'
       write(6,*)
       write(6,'(a)') ' 3. Recursively obtain stdv from original fields:'
       write(6,'(a)') '    mp_stats.x -o mean.nc4 -stdv stdv.nc4 mem0*/hy05a.bkg.eta.20120410_00z.nc4'
       write(6,*)
       write(6,'(a)') ' 4. Non-recursively obtain stdv by subtracting user-provided mean can be triggered by:'
       write(6,'(a)') '    mp_stats.x -o stdv.nc4 -usrmean mean.nc4 -rms -stdv NONE mem0*/hy05a.bkg.eta.20120410_00z.nc4'
       write(6,'(a)') '    in this case, the file mean.nc4 is an input such as that obtained from (1)'
       write(6,*)
       write(6,'(a)') ' 5. Non-recursive calc of energy-based error:'
       write(6,'(a)') '    mp_stats.x -usrmean mean.nc4 -ene ene.nc4 mem0*/hy05a.bkg.eta.20120410_00z.nc4'
       write(6,'(a)') '    in this case, the file mean.nc4 is an input such as that obtained w/ (1)'
       write(6,*)
       write(6,'(a)') ' 6. removing mean from samples and writing out anomalies:'
       write(6,'(a)') '    mp_stats.x -tmpl anomaly.%y4%m2%d2_%h2z -alpha -1.0 -date 19990101 0 hy05a.ana.eta.*'
       write(6,*)
       write(6,'(a)') ' 7. calculate energy-measure wrt to mean and write out individual member energy-measure estimates:'
       write(6,'(a)') '    mp_stats.x -nonrecene -ene mean_ene.nc4 -etmpl energy.%y4%m2%d2_%h2z mem0*/hy05a.bkg.eta.20120410_00z.nc4'
       write(6,'(a)') '       where: mean_ene.nc4   is ouput containing mean energy'
       write(6,'(a)') '           templated-files are ouput files containing each member''s energy'
       write(6,*)
       write(6,'(a)') ' 8. calculate energy-measure wrt central analysis and write out individual member energy-measure estimates:'
       write(6,'(a)') '    mp_stats.x -nonrecene -etmpl energy.%y4%m2%d2_%h2z -usrmean central_ana.nc4 -date 20120410 0 mem0*/hy05a.bkg.eta.20120410_00z.nc4'
       write(6,'(a)') '       where: central_ana.nc4   is input central field'
       write(6,'(a)') '           templated-files     are ouput files containing each member''s energy wrt to central'
       write(6,*)
       write(6,'(a)') ' 9. removing mean of energy fields:'
       write(6,'(a)') '    mp_stats.x -alpha -1.0 -tmpl energy_anomaly.%y4%m2%d2_%h2z -usrmean mean_energy.nc4 energy.20120410_00z.*.nc4'
       write(6,*)
       write(6,'(a)') 'REMARKS:'
       write(6,'(a)') ' a. To calculate energy measure for single case (member)'
       write(6,'(a)') '    user must provide reference state (e.g. mean), which '
       write(6,'(a)') '    automatically de-activates recursive calculation'
       write(6,'(a)') ' b. Presently results from the calculations of mean and stdev are'
       write(6,'(a)') '    reproducible and independent of NX and NY. CAUTION: However, '
       write(6,'(a)') '    the energy-measure calculation is not ... needs work'
       write(6,'(a)') ' c. When specifying -tmpl and -date together, templated name will appended '
       write(6,'(a)') '    3-digit counter, e.g., -date 20120410 0 -tmpl myfile.%y4%m2%d2_%h2z will'
       write(6,'(a)') '    create output as: myfile.20120410_00z.001.nc4, myfile.20120410_00z.002.nc4, etc'

       write(6,*)
   endif
   call ESMF_Finalize(__RC__)
   call exit(1)
   end subroutine usage_


   subroutine DefVertGrid_(CF,ak,bk,nsig,verbose,rc)
   use MAPL_Mod, only: MAPL_ROOT
   use m_mpif90,only : MP_REAL8
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
   integer                           :: i,k,ks
   real*8                            :: ptop,pint

! start

   if (verbose) then
      if(MAPL_AM_I_ROOT()) print *,trim(Iam),': Get GSI g.c. parameters '
   endif

! Create the label to be searched for in the RC file based on nsig

   if(MAPL_AM_I_ROOT()) then
     call set_eta ( nsig,ks,ptop,pint,ak,bk )
   endif
   call mpi_bcast(ak, nsig+1,MP_REAL8,MAPL_root,comm,rc)
   call mpi_bcast(bk, nsig+1,MP_REAL8,MAPL_root,comm,rc)

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

   subroutine final_
    if (doene) then
        call ESMF_FieldBundleDestroy (EnBundle, __RC__)
    endif
    do nf=size(InBundle),1,-1
       call ESMF_FieldBundleDestroy (INBundle(nf), __RC__)
    enddo
    if(allocated(TimeList)) deallocate(TimeList)
    call ESMF_GridDestroy (InGrid, __RC__)
    deallocate(files)
    deallocate(nymdi,nhmsi)
    if(allocated(mean2d)) then
       deallocate(mean2d)
       deallocate(names2d)
    endif
    if(allocated(mean3d)) then
       deallocate(mean3d)
       deallocate(names3d)
    endif
    if(allocated(umean2d)) deallocate(umean2d)
    if(allocated(umean3d)) deallocate(umean3d)
    if(allocated(smom2d)) deallocate(smom2d)
    if(allocated(smom3d)) deallocate(smom3d)
    if (doene) then
       if(allocated(pert_ps))  deallocate(pert_ps)
       if(allocated(delp))     deallocate(delp)
       if(allocated(tv))       deallocate(tv)
       if(allocated(sphu))     deallocate(sphu)
       if(allocated(jweights_glb)) deallocate(jweights_glb)
       if(allocated(jweights_lcl)) deallocate(jweights_lcl)
       if(allocated(energy_fields)) deallocate(energy_fields)
       if(allocated(enames))   deallocate(enames)
       if(allocated(flds4ene)) deallocate(flds4ene)
    endif
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
  character(len=31) dimName       ! variable name

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

  subroutine save4ene_ ( NAME, i3d, id, enames, flds4ene )
  implicit none
  character(len=*), intent(in)    :: NAME
  integer,          intent(in)    :: i3d
  integer,          intent(inout) :: id
  character(len=*), intent(out)   :: enames(:)
  real,             intent(out)   :: flds4ene(:,:,:,:)

! Store error fields ...
  if(trim(NAME) == 'u'  .or. trim(NAME)== 'v'   .or. &
     trim(NAME) == 'tv' .or. trim(NAME)=='sphu' ) then
     id=id+1
     enames(id)=trim(NAME)
     if (umean) then
        flds4ene(:,:,:,id)=ptr3d-umean3d(:,:,:,i3d)
     else
        flds4ene(:,:,:,id)=ptr3d- mean3d(:,:,:,i3d)
     endif
  endif
! Store basic (reference) flow ...
  if(trim(NAME)=='sphu') then
     if (umean) then
         sphu = umean3d(:,:,:,i3d) !ptr3d
     else
         sphu =  mean3d(:,:,:,i3d) !ptr3d
     endif
  endif
  if(trim(NAME)=='tv') then
     if (umean) then
         tv = umean3d(:,:,:,i3d) !ptr3d
     else
         tv =  mean3d(:,:,:,i3d) !ptr3d
     endif
  endif
  if(trim(NAME)=='delp') then
     if (umean) then
         delp = umean3d(:,:,:,i3d) !ptr3d
     else
         delp =  mean3d(:,:,:,i3d) !ptr3d
     endif
  endif
  end subroutine save4ene_
 
!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !ROUTINE: compute_e_field_ --- Compute contributions to the total energy norm E.
!
!
! !INTERFACE:
!
      subroutine compute_e_field_ (jweights, ptop, eps_eer, wgrid, vnorm,   &
                                   diag,fields,delp_ref,psfield,fnames,     &
                                   energy, IhaveTV, rc) 

!USES:

      ! The following come from HERMES: Caution when using hermes in the
      ! present MPI-REAL4-MAPL-context
      use m_const, only : Cp     => cpm
      use m_const, only : R      => rgas
      use m_const, only : pstd
      use m_const, only : alhl
      use m_const, only : tref   => tstd
      use m_const, only : zvir
      use m_const, only : kappa

      use m_mpif90, only: mp_real4
      use m_mpif90, only: mp_sum

      use m_die, only: MP_die
      implicit none

! !INPUT PARAMETERS:

      real, intent(in)  :: jweights(:)
      real, intent(in)  :: ptop
      real, intent(in)  :: eps_eer
      real, intent(in)  :: fields(:,:,:,:)
      real, intent(in)  :: delp_ref(:,:,:)
      real, intent(in)  :: psfield(:,:)   
      character(len=*),intent(in) :: fnames(:) 
      character(len=*),intent(in) :: wgrid
      logical, intent(in) :: vnorm
      logical, intent(in) :: diag
      logical, intent(in) :: IhaveTV

! !OUTPUT PARAMETERS:

      real,            intent(inout) :: energy(:,:,:,:)
      integer,         intent(out)   :: rc 

! !INPUT/OUTPUT PARAMETERS:
 
! !DESCRIPTION:
! 
! Compute contributions to the total energy norm E.  This is the same E as
! defined for the FVGCM Singular Vector calculations. Separate contributions
! by kinetic and potential energies for each pressure layer are determined. 
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!
!  04Jun2003  R. Errico  Initial algorithm
!  08Apr2005  R. Errico  Corrected assumption that sum(jweights)=1
!  10Jan2008  Todling    - Add wet(q) energy component  
!                        - Generalized to output all energy partitions
!  22Jan2012  Todling    Allow for vertical norm change
!  16Mar2012  Todling    Handle d/a grids
!  27Feb2013  Todling    Hijacked from svec:
!                        - only accumulate in real 8
!                        - redefine interface (small params passed in)
!                        - got rid of d-grid option (not applicable)
!
!EOP
!-------------------------------------------------------------------------
!
!  local variables

      character(len=*),parameter:: myname_='compute_e_field_'

      real(8), parameter :: pref = 100.d0*pstd

      integer  :: imb,ime,jmb,jme,nl1
      integer  :: i,j,k
      integer  :: idu,idv,idt,idq
      real     :: tfac, ufac, pfac, qfac
      real     :: value

      integer    :: nlp1,ierr
      real, allocatable :: fa(:,:) 
      real, allocatable :: fb(:,:) 
      real, allocatable :: ps_ref(:,:)
      real, allocatable :: dsigma(:,:,:)
      real, allocatable :: pe(:,:,:)
      real, allocatable :: jfac(:)      
        
      rc = 0

      imb=lbound(fields,1)
      ime=ubound(fields,1)
      jmb=lbound(fields,2)
      jme=ubound(fields,2)
      nl1=  size(fields,3)


! One factor of 0.5 is for defenition of energy as 1/2 * ...
! Assumption here is that sum(jweights)=1
      tfac=0.5d0*Cp/tref
      ufac=0.5d0
      pfac=0.5d0*R*tref/pref**2
      qfac=0.5d0*eps_eer*alhl*alhl/(Cp*tref)

      do i=1,size(fnames)
         if (trim(fnames(i))=='u'   ) idu=i
         if (trim(fnames(i))=='v'   ) idv=i
         if (trim(fnames(i))=='tv'  ) idt=i
         if (trim(fnames(i))=='sphu') idq=i
      enddo

      if ( trim(wgrid) /= 'a' ) then
         rc = 1
         print*, myname_, ': can only handle a-grid winds'
         return
      endif
! compute 3-D varying dsigma

      allocate(dsigma(imb:ime,jmb:jme,nl1))
      allocate(fa(imb:ime,jmb:jme)) 
      allocate(fb(imb:ime,jmb:jme)) 
      allocate(jfac(jmb:jme)) 

      jfac=jweights/float(IM_OUT)

      if ( vnorm ) then

          nlp1 = nl1+1
          allocate( pe(imb:ime,jmb:jme,nlp1), stat=ierr )
              if(ierr/=0) call MP_die(myname_,'Alloc(pe)',ierr)

          pe(:,:,1) = ptop
          do k=2,nlp1
             pe(:,:,k) = pe(:,:,k-1) + delp_ref(:,:,k-1)
          enddo
          do k=1,nl1
             dsigma(:,:,k) = log(pe(:,:,k+1)/pe(:,:,k))/log(pe(:,:,nlp1)/pe(:,:,1))
          enddo
          deallocate( pe, stat=ierr )
              if(ierr/=0) call MP_die(myname_,'DeAlloc(pe)',ierr)
          if ( verbose .and. MAPL_am_I_root() ) print *, 'Using V-norm'

      else

         allocate(ps_ref(imb:ime,jmb:jme))
         ps_ref(:,:) = sum(delp_ref(:,:,:),3)
         do k=1,nl1
            dsigma(:,:,k)=delp_ref(:,:,k)/ps_ref(:,:)
         enddo    
         deallocate(ps_ref)
         if ( verbose .and. MAPL_am_I_root() ) print *, 'Using E-norm'

      endif
!
!  Loop over levels
!
      do k=1, nl1
!     
!  Calculations for u field
!     
        fa(:,:)=0.0
        fb(:,:)=0.0
        do j=jmb,jme
          do i=imb,ime
            fa(i,j)=fields(i,j,k,idu)   
          enddo
        enddo


        if ( wgrid=='a' ) then
           do j=jmb,jme
             do i=imb,ime
               value=dsigma(i,j,k)*jfac(j)*fa(i,j)*fa(i,j)
               energy(i,j,k,nkxe)=energy(i,j,k,nkxe) + ufac*value
             enddo
           enddo
        else
           rc = 2
           if(MAPL_am_I_root()) &
           print*, myname_,'grid for winds not properly defined, aborting'
           return
        endif
!     
!  Calculations for v field
!    
        fa(:,:)=0.0
        fb(:,:)=0.0
        do j=jmb,jme
          do i=imb,ime
            fa(i,j)=fields(i,j,k,idv)     
          enddo
        enddo
        if ( wgrid=='a') then
           do j=jmb,jme
             do i=imb,ime
               value=dsigma(i,j,k)*jfac(j)*fa(i,j)*fa(i,j)
               energy(i,j,k,nkxe)=energy(i,j,k,nkxe) + ufac*value
             enddo
           enddo
        else ! will never make it here
           rc = 2
           if(MAPL_am_I_root()) &
           print*, myname_,'grid for winds not properly defined, aborting'
           return
        endif
!     
!  Calculations for T field
!     
        fa(:,:)=0.0
        do j=jmb,jme
          do i=imb,ime
            fa(i,j)=fields(i,j,k,idt) 
          enddo
        enddo
        ! convert to perturbation in dry-temperature
        if (IhaveTV) then
           fa = fa* (1.d0+zvir*sphu(:,:,k)) - zvir*tv(:,:,k)*fields(:,:,k,idq)
           fa = fa/((1.d0+zvir*sphu(:,:,k))*(1.d0+zvir*sphu(:,:,k)))
        endif

        do j=jmb,jme
          do i=imb,ime
            value=dsigma(i,j,k)*jfac(j)*fa(i,j)*fa(i,j)
            energy(i,j,k,nape)=energy(i,j,k,nape) + tfac*value
          enddo
        enddo
!     
!  Calculations for Q field
!     
        fa(:,:)=0.0
        do j=jmb,jme
          do i=imb,ime
            fa(i,j)=fields(i,j,k,idq) 
          enddo
        enddo

        do j=jmb,jme
          do i=imb,ime
            value=dsigma(i,j,k)*jfac(j)*fa(i,j)*fa(i,j)
            energy(i,j,k,nqxe)=energy(i,j,k,nqxe) + qfac*value
          enddo
        enddo
!
      enddo ! loop over k
!
! Calculations for ps field
!
      fa(:,:)=0.0
      do j=jmb,jme
        do i=imb,ime
          fa(i,j)=psfield(i,j)
        enddo
      enddo

      do j=jmb,jme
        do i=imb,ime
          value=jfac(j)*fa(i,j)*fa(i,j)
          do k=1, nl1
             energy(i,j,k,ngpe)=energy(i,j,k,ngpe)+pfac*dsigma(i,j,k)*value
          enddo
        enddo
      enddo

      energy(:,:,:,nape) = energy(:,:,:,nape) + energy(:,:,:,ngpe)
      energy(:,:,:,ntxe) = energy(:,:,:,nape) + energy(:,:,:,nkxe)
      energy(:,:,:,ntwe) = energy(:,:,:,nqxe) + energy(:,:,:,ntxe)

      if(diag) call echo_ene_(energy)

      deallocate(jfac,stat=ierr) 
          if(ierr/=0) call MP_die(myname_,'DeAlloc(jfac)',ierr)
      deallocate(fb,stat=ierr) 
          if(ierr/=0) call MP_die(myname_,'DeAlloc(fb)',ierr)
      deallocate(fa,stat=ierr) 
          if(ierr/=0) call MP_die(myname_,'DeAlloc(fa)',ierr)
      deallocate( dsigma, stat=ierr )
          if(ierr/=0) call MP_die(myname_,'DeAlloc(dsigma)',ierr)

      end subroutine compute_e_field_  

      subroutine echo_ene_ (energy)

      implicit none
      real, intent(in) :: energy(:,:,:,:)

      integer i
      real, allocatable  :: summary(:)
      real, allocatable  :: dsumary(:)

      allocate(summary(neflds),dsumary(neflds))
      do i=1,neflds
         summary(i)=sum(energy(:,:,:,i:i))
      enddo
      call MAPL_CommsAllReduceSum(vm, sendbuf=summary, &
                                      recvbuf=dsumary, cnt= neflds, __RC__ )
      if ( MAPL_am_I_root() ) then
         print *, 'Energy partition: '
         print *, 'Kinetic          Energy: ', dsumary(nkxe)
         print *, 'Avail. Potential Energy: ', dsumary(nape)
         print *, 'Potential        Energy: ', dsumary(ngpe)
         print *, 'Moist            Energy: ', dsumary(nqxe)
         print *, 'Total dry        Energy: ', dsumary(ntxe)
         print *, 'Total wet        Energy: ', dsumary(ntwe)
      endif
      deallocate(summary,dsumary)

      end subroutine echo_ene_

!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !ROUTINE: horiz_grid_ --- Determine some D-grid information required
!
!
! !INTERFACE:
!
      subroutine horiz_grid_ (jn1, jweights, glats)

!USES:

      implicit none

! !INPUT PARAMETERS:

      integer,  intent(in)  :: jn1    

! !OUTPUT PARAMETERS:

      real, intent(out) :: jweights(jn1,2)         ! area weights (1) u, (2) T   
      real, intent(out), optional :: glats(jn1,2)  ! degrees lats (1) u, (2) T   

! !INPUT/OUTPUT PARAMETERS:
 
! !DESCRIPTION:
!
!  Determine some D-grid information required
!
!  i=1 on v field corresponds to long=0
!  i=1 on T,u fields corresponds to long=0+360/(2*im1)
!  v defined on same lats as T, excluding poles
!  u defined between T lats, but on T lons
!
! !SEE ALSO:
!
! !REVISION HISTORY:
!
!  04Jun2003  R. Errico  Initial algorithm
!  03Mar2013  Todling    Massaged to fit present context
!
!EOP
!-------------------------------------------------------------------------
!
!  local variables

      integer   :: i,j
      real      :: slats(jn1,2)  ! sines of latitude for (1)u and (2)T
      real      :: pi, pi180
      real      :: rlat, rlat_half
      real      :: tlat, ulat
      real,allocatable::glats_(:,:)
      
      allocate(glats_(size(jweights,1),size(jweights,2)))

      pi=4.d0*datan(1.d0)
      pi180=180.d0/pi
      rlat=pi/dble(jn1-1)
      rlat_half=0.5d0*rlat

      tlat=-0.5d0*pi   ! latitude of south pole
      glats_(1,1)=pi180*rlat  ! a special value since otherwise not used
      glats_(1,2)=pi180*tlat  
      slats(1,1)=0.d0  ! a value not used
      slats(1,2)=-1.d0
      do j=2,jn1
        ulat=tlat+rlat_half
        tlat=tlat+rlat
        glats_(j,1)=pi180*ulat
        glats_(j,2)=pi180*tlat
        slats(j,1)=sin(ulat)
        slats(j,2)=sin(tlat)
      enddo
!
       jweights(1,1)=0.d0  ! not used
       jweights(1,2)=0.5d0*(1.d0+slats(2,1))
       do j=2,jn1-1
         jweights(j,1)=0.5d0*(slats(j,2)-slats(j-1,2))
         jweights(j,2)=0.5d0*(slats(j+1,1)-slats(j,1))
       enddo
       jweights(jn1,1)=0.5d0*(slats(jn1,2)-slats(jn1-1,2))
       jweights(jn1,2)=0.5d0*(1.d0-slats(jn1,1))
    
       if(present(glats))then
          glats=glats_
       endif
       deallocate(glats_)
       end subroutine horiz_grid_

       subroutine gbl2lcl_ (Grid,global,local)
       implicit none
       type(ESMF_Grid)    :: Grid
       real,intent(in)    :: global(:,:)
       real,intent(inout) :: local(:,:)
       integer :: i1,in,j1,jn,kk
       call ESMF_grid_interior(Grid,i1,in,j1,jn)
       do kk=1,size(global,2)
          local(:,kk) = global(j1:jn,kk)
       enddo
       end subroutine gbl2lcl_
 
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

     subroutine writeout_energy_(fname,fld3d,fld2d)

     implicit none
     character(len=*),intent(in) :: fname
     real,optional,   intent(in) :: fld3d(:,:,:,:) 
     real,optional,   intent(in) :: fld2d(:,:,:) 

     integer m2d, m3d

      ! Create bundle to hold energy fields
      ! -----------------------------------
      EnBundle = ESMF_FieldBundleCreate ( name='Energy bundle', __RC__ )
      call ESMF_FieldBundleSet(EnBundle, grid=INgrid, __RC__ )

      if(present(fld2d)) then
         m2d = size(fld2d,3) 
      else
         m2d = 0
      endif
      if(present(fld3d)) then
         m3d = size(fld3d,4) 
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
                                    name=trim(ene_fld_names(ifld)), &
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
                                    name=trim(ene_fld_names(ifld)), &
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
                             DESCR='Write Energy Fields', __RC__ )
      call MAPL_CFIOWrite ( cfio, clock, EnBundle, verbose=verbose, __RC__ )
      call MAPL_cfioDestroy ( cfio )
      deallocate(levels)

     end subroutine writeout_energy_

     subroutine MyRead_ ( fname, myGrid, nymd, nhms, im_world, jm_world, km_world, myptr3d )
     use m_dyn, only: dyn_vect
     use m_dyn, only: dyn_get
     use m_dyn, only: dyn_init
     use m_dyn, only: dyn_clean
     implicit none
     character(len=*),intent(in)    :: fname
     type(ESMF_Grid), intent(in)    :: myGrid
     integer,         intent(inout) :: nymd, nhms
     integer,         intent(in)    :: im_world,jm_world,km_world
     integer freq, nstep, nc, ier
     type(dyn_vect) w_e
     real(4),optional,pointer,dimension(:,:,:,:)::myptr3d
!
     if ( MAPL_am_I_root() ) then
        print *, 'dyn_get will read: ', trim(fname)
!       call dyn_init ( im, jm, lm, 10, w_e, ier, vectype=5 ) 
        call dyn_get ( fname, nymd, nhms, w_e, ier, timidx=1, freq=freq, nstep=nstep, vectype=5 )
!       call dyn_clean ( w_e )
     endif
     if(.not.present(myptr3d)) return
     call myscatter_ ( 'null', myGrid, im_world, jm_world, km_world, w_e%delp, myptr3d(:,:,:,1) )
     call myscatter_ ( 'null', myGrid, im_world, jm_world, km_world, w_e%u   , myptr3d(:,:,:,2) )
     call myscatter_ ( 'null', myGrid, im_world, jm_world, km_world, w_e%v   , myptr3d(:,:,:,3) )
     call myscatter_ ( 'null', myGrid, im_world, jm_world, km_world, w_e%pt  , myptr3d(:,:,:,4) )
     do nc=1,6
        call myscatter_ ( 'null', myGrid, im_world, jm_world, km_world, w_e%q(:,:,:,nc), myptr3d(:,:,:,nc+4) )
     enddo
!    just for the hack of timing ... overdo: since I am not doing 2 variables
!    do nc=1,1
!       call myscatter_ ( 'null', myGrid, im_world, jm_world, km_world, w_e%q(:,:,:,nc), work )
!    enddo
     end subroutine MyRead_

     subroutine myscatter_ ( what, myGrid, im_world, jm_world, km_world, vari, varo )
!    NOTE: there is no need for Halo here ... this is going onto a "model" field
     implicit none
     character(len=*),intent(in)    :: what
     type(ESMF_Grid), intent(in)    :: myGrid
     integer,         intent(in)    :: im_world,jm_world,km_world
     real(8),         intent(in)    :: vari(im_world,jm_world,km_world)
     real(4),         intent(inout) :: varo(:,:,:)

     character(len=*),parameter :: Iam=myname//'myscatter_'
     real(4),allocatable :: work2d(:,:)
     integer  k, rc,status

      if(MAPL_am_I_root())then
        allocate(work2d(im_world,jm_world))
      endif
      if (what=='adm') then
          do k=1,km_world  ! this would be a good place to swapV
             if(MAPL_am_I_root())then
                work2d=vari(:,:,k)
             endif
             call ArrayScatter(varo(:,:,k), work2d, myGrid, rc=status)   ! _RT: ideally this should be the AD of gather
                  VERIFY_(STATUS)
          enddo
      else
          do k=1,km_world  ! this would be a good place to swapV
             if(MAPL_am_I_root())then
                work2d=vari(:,:,k)
             endif
             call ArrayScatter(varo(:,:,k), work2d, myGrid, rc=status)
                  VERIFY_(STATUS)
          enddo
      endif
      if ( MAPL_am_I_root() ) then
         deallocate(work2d)
      endif

   end subroutine myscatter_

   integer function getindex_(var,vars)
   use m_die, only: die
   implicit none
   character(len=*),intent(in) :: var
   character(len=*),intent(in) :: vars(:)
   integer ii
   getindex_=-1
   do ii=1,size(vars)
      if(trim(var)==vars(ii)) then
         getindex_=ii
         exit
      endif
   enddo
   if(getindex_==-1) then
     call die('getindex_',': cannot find var index '//trim(var),99)
   endif
   end function  getindex_

end Program mp_stats
