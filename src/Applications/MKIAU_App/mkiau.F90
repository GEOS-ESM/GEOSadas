! mkiau.x - ESMF/MAPL application to calculate and write out IAU increment
!
! 1. Read BKG file
! 2. Defined IAU state at user''s desired resolution
! 3. Invokes mkiauGridCompMod to read ANA file and create IAU increment
!    and the resolution of the background
! 4. If needed regrids IAU increment to desired output resolution
! 5. Let IAU_GridComp write out file equivalent to agcm_internal_rst
!
! REMARKS: 
!   a) This program requires an RC file: mkiau.rc
!   b) No divr or diva options are yet available 
!   c) Note that when increment of DPEDT is passed via sdf we split it 
!      into two parts: DPEM1DT (all levels from 1 to LM) and DPSDT which
!      is level LM+1.
!
! Ricardo Todling, March 2011
!............................................................................
!  !REVISION_HISTORY:  
!   02Aug2012  Todling  handle wind increments (L2C) as vector winds (works)
!   25Oct2012  Todling  pass ak/bk as import to state of mkiauGridComp
!   23Mar2013  Todling  add ability to output increment as sdf-file (see c)
!   23Sep2013  Todling  have sclinc applied to increment as desired
!   03Nov2014  Todling  knob to write (real) internal AGCM file
!   14Jan2015  Todling  redef grid name following GEOS conventions (per Atanas)
!   08Jun2016  Todling  allow for bkg to be on cube; use delp to build bkg ple
!   01Oct2016  Todling  at last the GCM is importing PS,DELP, and TV - the
!                       analysis variables: updated this code accordingly.
!   19Feb2017  Todling  add zeit calls
!----------------------------------------------------------------------------

#  include "MAPL_Generic.h"

   Program mkIAU

   use ESMF
   use ESMF_CFIOFileMod
   use ESMF_CFIOUtilMod
   use MAPL
   use MAPL_CubedSphereGridFactoryMod 
   use CubeToLatLonRegridderMod
   use CubeToCubeRegridderMod
   use LatLonToCubeRegridderMod
   use m_set_eta, only: set_eta
   use m_ioutil, only: luavail
   use m_StrTemplate, only: StrTemplate
   use m_zeit, only: zeit_ci,zeit_co,zeit_flush
   use m_zeit, only: zeit_allflush
   use GEOS_mkiauGridCompMod,   only: MKIAUSetServices  => SetServices
   use IAU_GridCompMod,         only:   IAUSetServices  => SetServices
   use MAPL_Profiler, only: BaseProfiler, TimeProfiler, get_global_time_profiler, get_global_memory_profiler

   implicit NONE

!  Basic ESMF objects being used in this example
!  ---------------------------------------------
   type(ESMF_Grid)         :: GCMgrid   ! GCM Grid
   type(ESMF_Grid)         :: BKGgrid   ! BKG Grid
   type(ESMF_Grid)         :: ANAgrid   ! ANA Grid
   type(ESMF_FieldBundle)  :: BkgBundle ! Bundle to hold background
   type(ESMF_FieldBundle)  :: IOBundle
   type(ESMF_Field)        :: Field

   type(ESMF_VM)           :: vm       ! ESMF Virtual Machine
   type(ESMF_Time)         :: Time     ! Time objects
   type(ESMF_TimeInterval) :: TimeStep ! used to define a clock
   type(ESMF_Config)       :: CF       ! configuration settings

   type(MAPL_CFIO)         :: CFIO

!  Grid Component Objects
!  ----------------------
   integer :: BASE, STUB
   type(ESMF_GridComp),pointer :: GCS(:)
   type(ESMF_State)   ,pointer :: IMPORTS(:)
   type(ESMF_State)   ,pointer :: EXPORTS(:)
   type(ESMF_State)   ,pointer :: INTERNAL
   type(ESMF_Clock)    :: CLOCK
   type(MAPL_MetaComp), pointer :: MAPLOBJ

   type (CubedSphereGridFactory) :: cs_factory
   type (LatlonGridFactory) :: ll_factory

!  Basic information about the parallel environment
!         PET = Persistent Execution Threads
!  In the current implementation, a PET is equivalent 
!  to an MPI process
!  ------------------------------------------------
   integer :: myPET   ! The local PET number
   integer :: nPET    ! The total number of PETs you are running on

   integer :: status, rc
   integer :: userRC
   integer :: i, j, n, im, jm, ii
   integer :: ifld, nfld, rank
   integer :: nymd,nhms
   integer :: ino_dqvdt

   integer :: Nx, Ny                   ! Layout
   integer :: nx_cube,  ny_cube
   integer :: im_bkg, jm_bkg, lm_bkg   ! Bkg Grid dimensions
   integer :: im_iau, jm_iau, lm_iau   ! IAU Grid dimensions (revisit for cubed)

   integer, parameter :: r_quad  = selected_real_kind(20)
   integer :: comm
   logical :: SDFoutput,OIFoutput
   logical :: sameres,cubediau,cubedbkg
   real    :: sclinc
   logical :: summarize
!
   real*8, pointer :: ak(:), bk(:)
   real,   pointer :: levels(:)=>NULL()
   character(len=ESMF_MAXSTR) :: levunits
!
   character(len=ESMF_MAXSTR) :: dyntyp
   character(len=ESMF_MAXSTR) :: sdf_ofname
   character(len=ESMF_MAXSTR) :: own_internal_fname
!
   real, pointer, dimension(:,:)   :: ptr2d
   real, pointer, dimension(:,:,:) :: ptr3d
   real, pointer, dimension(:,:,:) :: ple_bkg
   real, pointer, dimension(:,:,:) :: pke_bkg
   real, pointer, dimension(:,:,:) :: pk_bkg
   real, pointer, dimension(:,:,:) :: dudt, sdudt
   real, pointer, dimension(:,:,:) :: dvdt, sdvdt
   real, pointer, dimension(:,:,:) :: dtdt, sdtdt
   real, pointer, dimension(:,:,:) :: dpedt, sdpedt
   real, pointer, dimension(:,:,:) :: dqvdt, sdqvdt
   real, pointer, dimension(:,:,:) :: do3dt, sdo3dt
   real, pointer, dimension(:,:)   :: dtsdt, sdtsdt

   character(len=ESMF_MAXSTR) :: bkgfname
   character(len=ESMF_MAXSTR) :: uname,vname,qname,tname,dpname,o3name

   logical,save:: c2l_fwtest = .false.
   logical,save:: c2l_adtest = .false.
   logical,save:: l2c_adtest = .false.

   logical,save:: proper_winds = .true.

!  Coordinate variables
!  --------------------
   character(len=ESMF_MAXSTR)    :: name
!  real, pointer, dimension(:,:) :: Array, newArray 

   character(len=*), parameter :: Iam = 'mkIAU'
   character(len=*), parameter :: myRC= 'mkiau.rc'

!                             -----
   class (BaseProfiler), pointer :: t_p
   type(ESMF_GridComp) :: temp_gc
   type(ESMF_Config) :: temp_config
   type(ServerManager) :: io_server
    
    call Main()

CONTAINS

    subroutine Main()

    character(len=80) ABKGGRIDNAME

!   Initialize the ESMF. For performance reasons, it is important
!    to turn OFF ESMF''s automatic logging feature
!   -------------------------------------------------------------
    call ESMF_Initialize (logKindFlag=ESMF_LOGKIND_NONE, vm=vm, __RC__)
    call ESMF_VMGetCurrent(vm=vm, rc=status)
    call ESMF_VMGet(vm,mpiCommunicator=comm,rc=status)

    !mapl_comm%mapl%comm=comm
    !mapl_comm%esmf%comm=comm
    call MAPL_Initialize(rc=status)
    VERIFY_(status)
    call io_server%initialize(comm)
    t_p => get_global_time_profiler()
    call t_p%start("mkiau.x")

    call init_ ( CF, nymd, nhms, __RC__ )

    call zeit_ci('MKIAU')

!   Check the number of processors
!   ------------------------------
    call ESMF_VMGet(vm, localPET=myPET, PETcount=nPET)  
    if ( nPET /= Nx * Ny ) then
       if ( MAPL_am_I_root() ) then
          print *, 'Error: expecting ', Nx*Ny, ' PETs but found ', nPET, 'PETs'
          print *, 'Try:  mpirun -np ', Nx*Ny, ' mkIAU.x'
       end if
       ASSERT_(.FALSE.)
    end if

    if ( MAPL_am_I_root() ) then
         print *
         print *, 'Starting ' // Iam // ' with ', nPET, ' PETs ...'
         print *
    end if

!   If bkg grid is cubed or desired iau output is cubed ...
!   -------------------------------------------------------
    if(JM_BKG==6*IM_BKG) cubedbkg=.true.

!   Create a regular Lat/Lon grid over which BKG/ANA defined
!   --------------------------------------------------------
    call MAPL_DefGridName (IM_BKG,JM_BKG,ABKGGRIDNAME,MAPL_am_I_root())
    if(cubedbkg) then
       if ( MAPL_am_I_root() ) then
          print *
          print *, 'Background on the cubed grid ', trim(ABKGGRIDNAME)
          print *
       endif
       cs_factory = CubedSphereGridFactory(im_world=IM_BKG,lm=LM_BKG,nx=nx_cube,ny=ny_cube,__RC__)
       BKGGrid = grid_manager%make_grid(cs_factory,__RC__)
    else
       if ( MAPL_am_I_root() ) then
          print *
          print *, 'Background on the lat-lon grid ', trim(ABKGGRIDNAME)
          print *
       endif
       ll_factory = LatLonGridFactory(grid_name=trim(ABKGGRIDNAME), &
                        Nx = Nx, Ny = Ny,   &
                        IM_World = IM_BKG,  &
                        JM_World = JM_BKG,  &
                        LM = LM_BKG, pole='PC', dateline='DC',  &
                               __RC__)
       BKGgrid = grid_manager%make_grid(ll_factory,__RC__)
    endif

!   Validate grid
!   -------------
    call ESMF_GridValidate(BKGgrid,__RC__)

!   Create either a regular Lat/Lon grid or cubed grid over which IAU defined
!   -------------------------------------------------------------------------
    if (cubediau) then
       cs_factory = CubedSphereGridFactory(im_world=IM_IAU,lm=LM_IAU,nx=nx_cube,ny=ny_cube,__RC__)
       GCMGrid = grid_manager%make_grid(cs_factory,__RC__)

    else
       call MAPL_DefGridName (IM_IAU,JM_IAU,ABKGGRIDNAME,MAPL_am_I_root())
       ll_factory = LatLonGridFactory(grid_name=trim(ABKGGRIDNAME), &
                        Nx = Nx, Ny = Ny,   &
                        IM_World = IM_IAU,  &
                        JM_World = JM_IAU,  &
                        LM = LM_IAU, pole='PC', dateline='DC',  &
                               __RC__)
       GCMgrid = grid_manager%make_grid(ll_factory,__RC__)

    endif

!   Validate grid
!   -------------
    call ESMF_GridValidate(GCMgrid,__RC__)

    sameres = IM_BKG==IM_IAU .and. JM_BKG==JM_IAU

!   Create a clock
!   --------------
    CLOCK = ESMF_ClockCreate ( name="IAUClock", timeStep=TimeStep, startTime=Time, __RC__ )

!   Create bundle to hold background and read background
!   ----------------------------------------------------
    BkgBundle = ESMF_FieldBundleCreate ( name='BKG bundle', __RC__ )
    call ESMF_FieldBundleSet ( BkgBundle, grid=BKGgrid, __RC__ )

    call MAPL_read_bundle( BkgBundle, bkgfname, Time, __RC__ )

!   Now create a component to handle the increment output
!   -----------------------------------------------------
    temp_config=ESMF_ConfigCreate()
    temp_gc = ESMF_GridCompCreate(name="cap_name", config=temp_config, rc=status)
    VERIFY_(status)

    maplobj => null()
    call MAPL_InternalStateCreate(temp_gc, maplobj, rc=status)
    VERIFY_(status)
    call MAPL_InternalStateRetrieve(temp_gc, maplobj, RC=status)
    VERIFY_(status)
    BASE = MAPL_AddChild ( MAPLOBJ, Grid=BKGgrid,    &
                                       ConfigFile=myRC,   &
                                          name= 'ABKG',   &
                                SS = MKIAUSetServices,    &
                                                  __RC__  )
    if (cubediau) then
       STUB = MAPL_AddChild ( MAPLOBJ, Grid=GCMgrid,    &
                                          ConfigFile=myRC,     &
                                             name= 'AGCM',     &
                                     SS = IAUSetServices,      &
                                                     __RC__    )
    else
       STUB = MAPL_AddChild ( MAPLOBJ, Grid=GCMgrid,           &
                                          ConfigFile=myRC,     &
                                             name= 'AGCM',     &
                                     SS = IAUSetServices,      &
                                                     __RC__    )
    endif

!   Initialize component
!   --------------------
    call MAPL_Get  ( MAPLOBJ, GCS=GCS, GIM=IMPORTS, GEX=EXPORTS, __RC__ )

    if (cubediau) then
       call MAPL_GridCreate  (GCS(STUB),ESMFGRID=GCMgrid, __RC__)
       call ESMF_GridCompGet (GCS(STUB), grid=GCMgrid, __RC__ )
       call ESMF_GridValidate(GCMgrid,__RC__)
    endif

    call ESMF_GridCompInitialize ( GCS(BASE), importState=IMPORTS(BASE), &
         exportState=EXPORTS(BASE), clock=CLOCK, userRC=userRC, RC=STATUS)
    ASSERT_(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS)

    call ESMF_GridCompInitialize ( GCS(STUB), importState=IMPORTS(STUB), &
         exportState=EXPORTS(STUB), clock=CLOCK, userRC=userRC, RC=STATUS)
    ASSERT_(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS)

    !if ( cubed ) then ! initialize GetWeights and FMS mambo-jambo
         !call GetWeights_init (6,1,im_iau,im_iau,lm_iau,Nx_cube,Ny_cube*6,.true.,.false.,comm)
    !endif

#if 0
    if ( MAPL_AM_I_ROOT() ) then
       call ESMF_StatePrint(IMPORTS(BASE))
       call ESMF_StatePrint(IMPORTS(STUB))
    end if
#endif

!   Prepare import of base state
!   ----------------------------
    call set_()

!   First run component to calculate IAU increment
!   ----------------------------------------------
    call ESMF_GridCompRun (GCS(BASE), importState=IMPORTS(BASE), &
         exportState=EXPORTS(BASE), clock=CLOCK, userRC=userRC, RC=STATUS)
    ASSERT_(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS)

!   Connect Exports of Run above with Imports of one below; regrid if needed
!   ------------------------------------------------------------------------
    call connect_()

!   Second run component to write out IAU increments
!   ------------------------------------------------
    call ESMF_GridCompRun (GCS(STUB), importState=IMPORTS(STUB), &
         exportState=EXPORTS(STUB), clock=CLOCK, userRC=userRC, phase=1, RC=STATUS)
    ASSERT_(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS)

!   Finalize component
!   ------------------
    call ESMF_GridCompFinalize ( GCS(BASE), importState=IMPORTS(BASE), &
         exportState=EXPORTS(BASE), clock=CLOCK, userRC=userRC, RC=STATUS)
    ASSERT_(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS)
    call ESMF_GridCompFinalize ( GCS(STUB), importState=IMPORTS(STUB), &
         exportState=EXPORTS(STUB), clock=CLOCK, userRC=userRC, RC=STATUS)
    ASSERT_(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS)

!   All done
!   --------
    call zeit_co('MKIAU')
    if (MAPL_AM_I_ROOT()) then
          call zeit_flush(6,subname_at_end=.true.)
          close(999)
          open (999,file='IAU_EGRESS',form='formatted')
          close(999)
    end if
    call final_
    call t_p%stop('mkiau.x')
    call io_server%finalize()
    call MAPL_Finalize()
    call ESMF_Finalize(__RC__)

  end subroutine Main

!BOP
! !ROUTINE: init_: initialize mkiau
!
! !DESCRIPTION:
!
! !INTERFACE:
!
    subroutine init_ ( CF, nymd, nhms, rc ) 

! !USES:

    use ESMF, only: ESMF_FALSE
    use m_StrTemplate, only: StrTemplate

    implicit NONE

! !INPUT/OUTPUT PARAMETERS:

    type(ESMF_Config)     :: CF

! !OUTPUT PARAMETERS:

    integer, intent(out)  :: nymd    ! date as in YYYYMMDD
    integer, intent(out)  :: nhms    ! time as in HHMMSS

    integer, intent(out)  :: rc      ! return error code
!
! !REVISION HISTORY:
!
!	03Feb2011 Todling  Initial code.
!
!EOP

    character*4, parameter :: myname = 'init_'

    integer thistime(6), idum, itest, status
    character(len=ESMF_MAXSTR) :: tmpl

!   Create Config and Initialize Clock 
!   ----------------------------------
    CF = ESMF_ConfigCreate   (__RC__)
    call ESMF_ConfigLoadFile ( CF, myrc, __RC__ )

!  Set defaults
!  ------------
   rc = 0
   cubediau = .false.
   cubedbkg = .false.

   !call ESMF_ConfigGetAttribute( CF, NX, label ='NX:', __RC__ )
   !call ESMF_ConfigGetAttribute( CF, NY, label ='NY:', __RC__ )

   call MAPL_MakeDecomposition(nx,ny,__RC__)
   call MAPL_MakeDecomposition(nx_cube,ny_cube,reduceFactor=6,__RC__)
   call ESMF_ConfigGetAttribute( CF, IM_IAU, label ='AGCM.IM_WORLD:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, JM_IAU, label ='AGCM.JM_WORLD:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, LM_IAU, label ='AGCM.LM:', __RC__ )

   call ESMF_ConfigGetAttribute( CF, DYNTYP, label ='DYCORE:', __RC__ )

   call ESMF_ConfigGetAttribute( CF, UNAME ,label ='UNAME:', default='u', __RC__ )
   call ESMF_ConfigGetAttribute( CF, VNAME ,label ='VNAME:', default='v', __RC__ )
   call ESMF_ConfigGetAttribute( CF, QNAME ,label ='QNAME:', default='sphu', __RC__ )
   call ESMF_ConfigGetAttribute( CF, TNAME ,label ='TNAME:', default='tv', __RC__ )
   call ESMF_ConfigGetAttribute( CF, DPNAME,label ='DPNAME:', default='delp', __RC__ )
   call ESMF_ConfigGetAttribute( CF, O3NAME,label ='O3NAME:', default='ozone', __RC__ )

   call ESMF_ConfigGetAttribute( CF, itest, label ='TEST_CASE:', default=0, __RC__ )
   if(itest==1) c2l_fwtest = .true.
   if(itest==2) c2l_adtest = .true.
   if(itest==3) l2c_adtest = .true.
   if(JM_IAU==6*IM_IAU) cubediau=.true.

   summarize = .false.
   call ESMF_ConfigGetAttribute( CF, idum, label ='SUMMARIZE:', default=0, __RC__ )
   if (idum==1) summarize = .true.

   call ESMF_ConfigGetAttribute( CF, ino_dqvdt, label ='NO_DQVDT:', default=0, __RC__ )

   call ESMF_ConfigGetAttribute( CF, nymd, label ='IAU_DATE:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, nhms, label ='IAU_TIME:', __RC__ )

   call ESMF_ConfigGetAttribute( CF, tmpl,  label ='REPLAY_BKG:', __RC__ )
   call StrTemplate ( bkgfname, tmpl, 'GRADS', nymd=nymd, nhms=nhms, stat=status)

   thistime(1) =     nymd/10000
   thistime(2) = mod(nymd,10000)/100
   thistime(3) = mod(nymd,100)
   thistime(4) =     nhms/10000
   thistime(5) = mod(nhms,10000)/100
   thistime(6) = mod(nhms,100)

   call ESMF_ConfigGetAttribute( CF, sclinc, label ='SCLINC:', default=1.0, rc=status )

   call ESMF_ConfigGetAttribute( CF, sdf_ofname, label ='SDF_FILENAME:', default='NONE', __RC__ )
   if (trim(sdf_ofname)=="NONE") then
       SDFoutput = .false.
   else
       SDFoutput = .true.
   endif

   call ESMF_ConfigGetAttribute( CF, own_internal_fname, label ='WRITE_AGCM_INTERNAL_FILE:', default='NONE', __RC__ )
   if (trim(own_internal_fname)=="NONE") then
       OIFoutput = .false.
   else
       OIFoutput = .true.
   endif

!  get dims
!  --------
   call getdim_ ( bkgfname, IM_BKG, JM_BKG, LM_BKG, idum, status )

!  define vertical grid (should be put in the grid ...)
!  ----------------------------------------------------
   allocate(ak(LM_IAU+1),bk(LM_IAU+1))
   call DefVertGrid_(CF,ak,bk,lm_iau)
   allocate(levels(LM_IAU))
   call hermes_levels_(levels)

!  Set ESMF date/time
!  ------------------
   call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN )
   call ESMF_TimeSet(Time, yy=thistime(1), mm=thistime(2), dd=thistime(3), &
                            h=thistime(4), m =thistime(5),  s=thistime(6))
   call ESMF_TimeIntervalSet( TimeStep, h=6, m=0, s=0, __RC__ )

   end subroutine init_

   subroutine info_ (agrid_bkg,dgrid_bkg,tvflag_bkg,thvflag_bkg )
   implicit none
   logical,intent(in):: agrid_bkg,dgrid_bkg,tvflag_bkg,thvflag_bkg
      if( myPET==MAPL_ROOT ) then
          print *
          print *, '         BKG resolution: ',IM_BKG,JM_BKG,LM_BKG
          print *
          print *, '              agrid_bkg: ',  agrid_bkg
          print *, '              dgrid_bkg: ',  dgrid_bkg
          print *, '             tvflag_bkg: ', tvflag_bkg
          print *, '            thvflag_bkg: ',thvflag_bkg
          print *
          print *, '                   Date: ',nymd,nhms
          print *, '      Output resolution: ',IM_IAU,JM_IAU,LM_IAU
          print *
      endif
   end subroutine info_

   subroutine set_()
   
   implicit none

   type(MAPL_SimpleBundle) :: bkg

   logical ::  agrid_bkg, dgrid_bkg, tvflag_bkg, thvflag_bkg

   real, pointer ::pak(:),pbk(:)
   real, pointer ::phis_bkg(:,:)
   real, pointer ::  ts_bkg(:,:)
   real, pointer ::  ps_bkg(:,:)
   real, pointer ::   u_bkg(:,:,:)
   real, pointer ::   v_bkg(:,:,:)
   real, pointer ::   t_bkg(:,:,:)
   real, pointer ::   q_bkg(:,:,:)
   real, pointer ::  o3_bkg(:,:,:)
   real, pointer ::  dp_bkg(:,:,:)

   real, parameter :: EPS = MAPL_RVAP/MAPL_RGAS-1.0
   integer ii,jj,L
   integer imb, jmb
   integer ib_ps, ib_ts, ib_ph, ib_q, ib_o3, ib_u, ib_v, ib_t
   integer ii_ts, ii_u , ii_v , ii_t, ii_q,  ii_o3, ii_pe
   integer ib_dp

!  Link background Bundle to Simple-Bundle
!  ---------------------------------------
   bkg = MAPL_SimpleBundleCreate ( BkgBundle, __RC__ )

!  Get pointers from background bundle
!  -----------------------------------
   ib_ps = MAPL_SimpleBundleGetIndex ( bkg, 'ps',    2, __RC__ )
   ib_ts = MAPL_SimpleBundleGetIndex ( bkg, 'ts',    2, __RC__ )
   ib_ph = MAPL_SimpleBundleGetIndex ( bkg, 'phis',  2, __RC__ )
   ib_q  = MAPL_SimpleBundleGetIndex ( bkg, trim(QNAME),  3, __RC__ )
   if ( trim(O3NAME) /= 'NULL' ) then
      ib_o3 = MAPL_SimpleBundleGetIndex ( bkg, trim(O3NAME), 3, __RC__ )
   else
      ib_o3 = -1
   endif
   agrid_bkg = .true.
   dgrid_bkg = .false.
   ib_u  = MAPL_SimpleBundleGetIndex ( bkg, trim(UNAME),     3, __RC__ )
   ib_v  = MAPL_SimpleBundleGetIndex ( bkg, trim(VNAME),     3, __RC__ )
   if (trim(UNAME)=='uwnd' .and. trim(VNAME)=='vwnd' ) then
       agrid_bkg = .false.
       dgrid_bkg = .true.
   endif
   tvflag_bkg  = .true.
   thvflag_bkg = .false.
   imb = size(bkg%r2(ib_ps)%q,1)
   jmb = size(bkg%r2(ib_ps)%q,2)
   ib_t = MAPL_SimpleBundleGetIndex ( bkg, trim(TNAME),    3, __RC__ )
   if ( trim(TNAME) == 't' ) then ! convert to virtual-T
       bkg%r3(ib_t)%q = bkg%r3(ib_t)%q * ( 1.0 + eps * bkg%r3(ib_q )%q )
   endif
   if ( trim(TNAME) == 'theta' ) then
       tvflag_bkg  = .false.
       thvflag_bkg = .true.
   endif
   if ( trim(DPNAME) /= 'NULL' ) then
      ib_dp = MAPL_SimpleBundleGetIndex ( bkg, 'delp',    3, __RC__ )
   else
      ib_dp = -1
   endif

   call info_ (agrid_bkg,dgrid_bkg,tvflag_bkg,thvflag_bkg )

!  Now fill in import of MKIAU, that is, give it the background
!  TO BE DONE: care for MERRA (dgrid/thv) cases - following assumes A-grid and TV
!  ------------------------------------------------------------
   call MAPL_GetPointer(IMPORTS(BASE),pak, 'AK', __RC__)
   pak(0:size(ak)-1) = ak
   call MAPL_GetPointer(IMPORTS(BASE),pbk, 'BK', __RC__)
   pbk(0:size(bk)-1) = bk
   call MAPL_GetPointer(IMPORTS(BASE),phis_bkg, 'PHIS', __RC__)
   phis_bkg = bkg%r2(ib_ph)%q
   call MAPL_GetPointer(IMPORTS(BASE), ts_bkg, 'TS',__RC__)
   ts_bkg = bkg%r2(ib_ts)%q
   call MAPL_GetPointer(IMPORTS(BASE), ps_bkg, 'PS',__RC__)
   ps_bkg = bkg%r2(ib_ps)%q
   call MAPL_GetPointer(IMPORTS(BASE), u_bkg, 'U', __RC__)
   u_bkg = bkg%r3(ib_u )%q
   call MAPL_GetPointer(IMPORTS(BASE), v_bkg, 'V', __RC__)
   v_bkg = bkg%r3(ib_v )%q
   call MAPL_GetPointer(IMPORTS(BASE), q_bkg, 'QV',__RC__)
   q_bkg = bkg%r3(ib_q )%q
   call MAPL_GetPointer(IMPORTS(BASE), dp_bkg, 'DELP',__RC__)
   if (ib_dp>0 ) then
      dp_bkg = bkg%r3(ib_dp)%q
   else
      do L=1,lm_iau
         dp_bkg(:,:,L)=(bk(L+1)-bk(L))*bkg%r2(ib_ps)%q(:,:)
      end do
   endif
   call MAPL_GetPointer(IMPORTS(BASE), t_bkg, 'TV', __RC__)
   if (thvflag_bkg) then ! convert thetav to tv, assume model background is indeed on eta
      allocate(ple_bkg(imb,jmb,0:lm_iau))
      allocate(pke_bkg(imb,jmb,0:lm_iau))
      allocate(pk_bkg(imb,jmb,lm_iau))
      ple_bkg(:,:,0) = pak(0)
      do L=1,lm_iau
         ple_bkg(:,:,L) = ple_bkg(:,:,L-1) + dp_bkg(:,:,L)
      enddo
      pke_bkg(:,:,:)  = ple_bkg(:,:,:)**MAPL_KAPPA
      do L=1,lm_iau
       pk_bkg(:,:,L)  = ( pke_bkg(:,:,L)-pke_bkg(:,:,L-1) ) &
                      / ( MAPL_KAPPA*log(ple_bkg(:,:,L)/ple_bkg(:,:,L-1)) )
      enddo
      t_bkg = bkg%r3(ib_t)%q*pk_bkg
      deallocate(pk_bkg)
      deallocate(pke_bkg)
      deallocate(ple_bkg)
   else
      t_bkg = bkg%r3(ib_t)%q  ! hold tv
   endif
   call MAPL_GetPointer(IMPORTS(BASE), o3_bkg, 'O3PPMV',__RC__)
   if (ib_o3>0) then
      o3_bkg = bkg%r3(ib_o3)%q
   else
      o3_bkg = 0.0
   endif
   
!  Get pointer from Export of MKIAU (the actual increments)
!  RC to HAVE:  COMP_EXPORT_ALLOCATE TRUE (talk to Atanas)
!  --------------------------------------------------------
   call ESMFL_StateGetPointerToData(EXPORTS(BASE), dudt , 'DUDT' ,alloc=.true.,__RC__ )
   call ESMFL_StateGetPointerToData(EXPORTS(BASE), dvdt , 'DVDT' ,alloc=.true.,__RC__ )
   call ESMFL_StateGetPointerToData(EXPORTS(BASE), dtdt , 'DTDT' ,alloc=.true.,__RC__ )
   call ESMFL_StateGetPointerToData(EXPORTS(BASE), dpedt, 'DPEDT',alloc=.true.,__RC__ )
   call ESMFL_StateGetPointerToData(EXPORTS(BASE), dqvdt, 'DQVDT',alloc=.true.,__RC__ )
   call ESMFL_StateGetPointerToData(EXPORTS(BASE), do3dt, 'DO3DT',alloc=.true.,__RC__ )
   call ESMFL_StateGetPointerToData(EXPORTS(BASE), dtsdt, 'DTSDT',alloc=.true.,__RC__ )

!  Clean up

   end subroutine set_

   subroutine connect_

   use ESMFL_Mod, only: ESMFL_State2Bundle
   use ESMFL_Mod, only: ESMFL_Bundle2State
   use ESMFL_Mod, only: ESMFL_Regrid
   use ESMFL_Mod, only: ESMFL_FieldRegrid

   use m_mpif90, only: MP_TYPE,MP_SUM

   implicit none

   type(ESMF_FieldBundle)   :: IAUBundleBase
   type(ESMF_FieldBundle)   :: IAUBundleStub
   type(MAPL_SimpleBundle)  :: IAUBase
   type(MAPL_SimpleBundle)  :: IAUStub
   type(CubedSphereGridFactory) :: cs_factory
   type(latLonGridFactory) :: ll_factory
   class(AbstractRegridder), pointer :: L2C => null()
   class(AbstractRegridder), pointer :: C2L => null()
   type(TransposeRegridder) :: L2C_AD
   type(TransposeRegridder) :: C2L_AD 

   character(len=*), parameter :: Iam = "connect_"
   character(len=ESMF_MAXSTR) :: ofname
   integer,parameter :: lu=10
   integer ii, jj, lll, ku
   integer iu, ju, iv, jv
   integer iii, jjj, kkk
   integer mx,my,mz
   integer ndim_ll, im_ll,jm_ll,km_ll
   integer ndim_cb, im_cb,jm_cb,km_cb
   integer, allocatable :: gridToFieldMap(:)
   integer              :: dimCount
   real,pointer,dimension(:,:,:)::aux3d
   real,allocatable::aux(:,:,:)
   real,allocatable::aux2(:,:,:)
   real(8) :: sdot,rdot1,rdot2

   if ( ino_dqvdt/=0 ) then
      dqvdt = 0.0
   endif

   if ( sameres ) then 

!       Set imports of IAU_GridComp - this is so I can output the fields as an agcm_import file
!       ---------------------------
        call MAPL_GetPointer(IMPORTS(STUB),  sdudt, 'DUDT' , __RC__)
        sdudt = sclinc * dudt
        if ( summarize ) then
           call var3d_summary_ ('DUDT', sdudt)
        endif
        call MAPL_GetPointer(IMPORTS(STUB),  sdvdt, 'DVDT' , __RC__)
        sdvdt = sclinc * dvdt
        if ( summarize ) then
           call var3d_summary_ ('DVDT', sdvdt)
        endif
        call MAPL_GetPointer(IMPORTS(STUB),  sdtdt, 'DTDT' , __RC__)
        sdtdt = sclinc * dtdt
        if ( summarize ) then
           call var3d_summary_ ('DTDT', sdtdt)
        endif
        call MAPL_GetPointer(IMPORTS(STUB), sdpedt, 'DPEDT', __RC__)
        sdpedt = sclinc * dpedt
        if ( summarize ) then
           call var3d_summary_ ('DPEDT', sdpedt)
        endif
        call MAPL_GetPointer(IMPORTS(STUB), sdqvdt, 'DQVDT', __RC__)
        sdqvdt = sclinc * dqvdt
        if ( summarize ) then
           call var3d_summary_ ('DQVDT', sdqvdt)
        endif
        call MAPL_GetPointer(IMPORTS(STUB), sdo3dt, 'DO3DT', __RC__)
        if ( trim(O3NAME) /= 'NULL' ) then
           sdo3dt = sclinc * do3dt
        else
           sdo3dt = 0.0
        endif
        if ( summarize ) then
           call var3d_summary_ ('DO3DT', sdo3dt)
        endif
        call MAPL_GetPointer(IMPORTS(STUB), sdtsdt, 'DTSDT', __RC__)
        if(associated(sdtsdt).and.associated(dtsdt))then
           sdtsdt = sclinc * dtsdt
           if ( summarize ) then
              call var2d_summary_ ('DRSDT', sdtsdt)
           endif
        endif

        if (SDFoutput) then
           call ESMFL_State2Bundle (IMPORTS(STUB), IAUBundleStub)
        endif

   else

        if ( MAPL_am_I_root() ) then
             print *
             print *, 'Regridding from BKG/ANA grid to GCM/IAU ...'
             print *
        end if
        call zeit_ci('Regrid')
    
!       Create a bundle to hold IAU increment at final resolution
!       ---------------------------------------------------------
        IAUBundleStub = ESMF_FieldBundleCreate ( name='New res IAU bundle', __RC__ )
        call ESMF_FieldBundleSet ( IAUBundleStub, grid=GCMgrid, __RC__ )

        call ESMFL_State2Bundle (IMPORTS(STUB), IAUBundleStub)

!       Create a bundle with IAU increment available at BKG resolution
!       --------------------------------------------------------------
        IAUBundleBase = ESMF_FieldBundleCreate ( name='Original IAU bundle', __RC__ )
        call ESMF_FieldBundleSet ( IAUBundleBase, grid=BKGgrid, __RC__ )

        call ESMFL_State2Bundle (EXPORTS(BASE), IAUBundleBase)

        if (cubediau) then

            iaubase = MAPL_SimpleBundleCreate ( IAUBundleBase, __RC__ )
            iaustub = MAPL_SimpleBundleCreate ( IAUBundleStub, __RC__ )

!           Create transform from cubed to lat-lon
!           --------------------------------------
            L2C => new_regridder_manager%make_regridder(BKGGrid, GCMGrid, REGRID_METHOD_BILINEAR,__RC__)

            if (proper_winds) then
                iu = MAPL_SimpleBundleGetIndex ( iaubase, 'DUDT', 3, __RC__ )
                iv = MAPL_SimpleBundleGetIndex ( iaubase, 'DVDT', 3, __RC__ )
                ju = MAPL_SimpleBundleGetIndex ( iaustub, 'DUDT', 3, __RC__ )
                jv = MAPL_SimpleBundleGetIndex ( iaustub, 'DVDT', 3, __RC__ )
                call L2C%regrid(iaubase%r3(iu)%q,iaubase%r3(iv)%q,iaustub%r3(ju)%q,iaustub%r3(jv)%q,__RC__)
            else
                if ( MAPL_am_I_root() ) then
                     print *
                     print *, 'Attention: treating winds as scalars! '
                     print *
                end if
                iu = MAPL_SimpleBundleGetIndex ( iaubase, 'DUDT', 3, __RC__ )
                ju = MAPL_SimpleBundleGetIndex ( iaustub, 'DUDT', 3, __RC__ )
                call L2C%regrid(iaubase%r3(iu)%q, iaustub%r3(ju)%q, __RC__)
    
                iv = MAPL_SimpleBundleGetIndex ( iaubase, 'DVDT', 3, __RC__ )
                jv = MAPL_SimpleBundleGetIndex ( iaustub, 'DVDT', 3, __RC__ )
                call L2C%regrid(iaubase%r3(iv)%q, iaustub%r3(jv)%q, __RC__ )
            endif
            iaustub%r3(ju)%q = sclinc * iaustub%r3(ju)%q
            iaustub%r3(jv)%q = sclinc * iaustub%r3(jv)%q
            if ( summarize ) then
               call var3d_summary_ ('DUDT', iaustub%r3(ju)%q)
               call var3d_summary_ ('DVDT', iaustub%r3(jv)%q)
            endif

            ii = MAPL_SimpleBundleGetIndex ( iaubase, 'DTDT', 3, __RC__ )
            jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DTDT', 3, __RC__ )
            call L2C%regrid(iaubase%r3(ii)%q, iaustub%r3(jj)%q, __RC__ )
            iaustub%r3(jj)%q = sclinc * iaustub%r3(jj)%q
            if ( summarize ) then
               call var3d_summary_ ('DTDT', iaustub%r3(jj)%q)
            endif

            ii = MAPL_SimpleBundleGetIndex ( iaubase, 'DPEDT', 3, __RC__ )
            jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DPEDT', 3, __RC__ )
            call L2C%regrid(iaubase%r3(ii)%q, iaustub%r3(jj)%q, __RC__ )
            iaustub%r3(jj)%q = sclinc * iaustub%r3(jj)%q
            if ( summarize ) then
               call var3d_summary_ ('DPEDT', iaustub%r3(jj)%q)
            endif

            ii = MAPL_SimpleBundleGetIndex ( iaubase, 'DQVDT', 3, __RC__ )
            jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DQVDT', 3, __RC__ )
            call L2C%regrid(iaubase%r3(ii)%q, iaustub%r3(jj)%q, __RC__ )
            iaustub%r3(jj)%q = sclinc * iaustub%r3(jj)%q
            if ( summarize ) then
               call var3d_summary_ ('DQVDT', iaustub%r3(jj)%q)
            endif

            ii = MAPL_SimpleBundleGetIndex ( iaubase, 'DO3DT', 3, __RC__ )
            jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DO3DT', 3, __RC__ )
            call L2C%regrid(iaubase%r3(ii)%q, iaustub%r3(jj)%q, __RC__ )
            iaustub%r3(jj)%q = sclinc * iaustub%r3(jj)%q
            if ( summarize ) then
               call var3d_summary_ ('DO3DT', iaustub%r3(jj)%q)
            endif

            ii = MAPL_SimpleBundleGetIndex ( iaubase, 'DTSDT', 2, __RC__ )
            jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DTSDT', 2, __RC__ )
            if (ii>0.and.jj>0) then
               allocate(aux (size(iaubase%r2(ii)%q,1),size(iaubase%r2(ii)%q,2),1))
               allocate(aux2(size(iaustub%r2(jj)%q,1),size(iaustub%r2(jj)%q,2),1))
! 2d interface to HorzT seems buggy
!             call MAPL_HorzTransformRun (L2C, iaubase%r2(ii)%q, iaustub%r2(jj)%q, __RC__ )
                   aux (:,:,1) = iaubase%r2(ii)%q
              call L2C%regrid(aux, aux2, __RC__ )
                 iaustub%r2(jj)%q = sclinc * aux2(:,:,1)
                 if ( summarize ) then
                    call var2d_summary_ ('DRSDT', iaustub%r2(jj)%q)
                 endif
                 deallocate(aux2)
                 deallocate(aux )
            endif

           if ( OIFoutput ) then
               ku=luavail()
               open (ku,file=trim(own_internal_fname),form='unformatted')
               ifld = MAPL_SimpleBundleGetIndex ( iaustub, 'DUDT', 3, __RC__ )
               km_cb = size(iaustub%r3(ifld)%q,3)
                   call writefld_(ku,MAPL_am_I_root(),IM_IAU,JM_IAU,GCMgrid,iaustub%r3(ifld)%q)
               ifld = MAPL_SimpleBundleGetIndex ( iaustub, 'DVDT', 3, __RC__ )
                  call writefld_(ku,MAPL_am_I_root(),IM_IAU,JM_IAU,GCMgrid,iaustub%r3(ifld)%q)
               ifld = MAPL_SimpleBundleGetIndex ( iaustub, 'DTDT', 3, __RC__ )
                  call writefld_(ku,MAPL_am_I_root(),IM_IAU,JM_IAU,GCMgrid,iaustub%r3(ifld)%q)
               ifld = MAPL_SimpleBundleGetIndex ( iaustub, 'DPEDT', 3, __RC__ )
                  call writefld_(ku,MAPL_am_I_root(),IM_IAU,JM_IAU,GCMgrid,iaustub%r3(ifld)%q)
               ifld = MAPL_SimpleBundleGetIndex ( iaustub, 'DQVDT', 3, __RC__ )
                  call writefld_(ku,MAPL_am_I_root(),IM_IAU,JM_IAU,GCMgrid,iaustub%r3(ifld)%q)
               ifld = MAPL_SimpleBundleGetIndex ( iaustub, 'DO3DT', 3, __RC__ )
                  call writefld_(ku,MAPL_am_I_root(),IM_IAU,JM_IAU,GCMgrid,iaustub%r3(ifld)%q)
               !ifld = MAPL_SimpleBundleGetIndex ( iaustub, 'DTSDT', 2, __RC__ )
	       ! following interface will need attention
               !call writefld_(ku,IM_IAU,JM_IAU,1,GCMgrid,iaustub%r2(ifld)%q)
               close(ku)
           endif ! OIFoutput


!           -----------------------------------------------------
!           Now that L2C transform has been done we can possibly
!           try the following tests:
!               1) convert back to Lat-Lon grid
!               2) check adjoint of L2C
!           Only one test can be done per run (for now) ...
!           -----------------------------------------------------
            if ( c2l_fwtest ) then

                 open   (lu,file='c2l_test.bin',form='unformatted',access='sequential')
                 rewind (lu)
                 im_ll = size(iaubase%r3(ii)%q,1)
                 jm_ll = size(iaubase%r3(ii)%q,2)
                 km_ll = size(iaubase%r3(ii)%q,3)
                 if ( MAPL_am_I_root() ) then
                      print *
                      print *, 'Converting back from Cubed to LatLon: ', IM_BKG, ' x ', JM_BKG
                      print *
                 end if
    
                 allocate(aux(im_ll,jm_ll,km_ll))

!                Create transform from lat-lon to cubed
!                --------------------------------------
                 C2L => new_regridder_manager%make_regridder(GCMGrid, BKGGrid, REGRID_METHOD_BILINEAR,__RC__)

                 jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DUDT', 3, __RC__ )
                 call C2L%regrid(iaustub%r3(jj)%q, aux, __RC__ )
                 call xwritit ( aux,im_ll,jm_ll,km_ll,lu,BKGgrid )

                 jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DVDT', 3, __RC__ )
                 call C2L%regrid(iaustub%r3(jj)%q, aux, __RC__ )
                 call xwritit ( aux,im_ll,jm_ll,km_ll,lu,BKGgrid )

                 jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DTDT', 3, __RC__ )
                 call C2L%regrid(iaustub%r3(jj)%q, aux, __RC__ )
                 call xwritit ( aux,im_ll,jm_ll,km_ll,lu,BKGgrid )

                 deallocate(aux)
                 allocate(aux(im_ll,jm_ll,km_ll+1))
                 jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DPEDT', 3, __RC__ )
                 call C2L%regrid(iaustub%r3(jj)%q, aux, __RC__ )
                 call xwritit ( aux,im_ll,jm_ll,km_ll+1,lu,BKGgrid )
                 deallocate(aux)
                 allocate(aux(im_ll,jm_ll,km_ll))

                 jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DQVDT', 3, __RC__ )
                 call C2L%regrid(iaustub%r3(jj)%q, aux, __RC__ )
                 call xwritit ( aux,im_ll,jm_ll,km_ll,lu,BKGgrid )

                 jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DO3DT', 3, __RC__ )
                 call C2L%regrid(iaustub%r3(jj)%q, aux, __RC__ )
                 call xwritit ( aux,im_ll,jm_ll,km_ll,lu,BKGgrid )

                 deallocate(aux)

                 jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DTSDT', 2, __RC__ )
                 allocate(aux (im_ll,jm_ll,1))
                 allocate(aux2(im_ll,jm_ll,1))
                 aux2(:,:,1) = iaustub%r2(jj)%q
                 call C2L%regrid(aux2, aux, __RC__ )
                 call xwritit ( aux,im_ll,jm_ll,1,lu,BKGgrid )
                 deallocate(aux2)
                 deallocate(aux)

                 close (lu)


                 !call MAPL_HorzTransformDestroy(C2L, __RC__ )


            endif

            if ( c2l_adtest ) then

!                Create transform from lat-lon to cubed
!                --------------------------------------
                 C2L => new_regridder_manager%make_regridder(GCMGrid, BKGGrid, REGRID_METHOD_BILINEAR,__RC__)

!                Create adjoint of transform from cube to lat-lon
!                ------------------------------------------------

                 im_ll = size(iaubase%r3(jj)%q,1)
                 jm_ll = size(iaubase%r3(jj)%q,2)
                 km_ll = size(iaubase%r3(jj)%q,3)
                 ndim_ll = im_ll*jm_ll*km_ll
                 ii = MAPL_SimpleBundleGetIndex ( iaubase, 'DTDT', 3, __RC__ )
                 im_cb = size(iaustub%r3(ii)%q,1)
                 jm_cb = size(iaustub%r3(ii)%q,2)
                 km_cb = size(iaustub%r3(ii)%q,3)
                 ndim_cb = im_cb*jm_cb*km_cb
                 jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DTDT', 3, __RC__ )
     
                 allocate(aux (im_cb,jm_cb,km_cb))
                 allocate(aux2(im_ll,jm_ll,km_ll))

                 ! x=cb
                 ! T=C2L
                 ! < Tx, Tx >
                 ! ----------
                 call C2L%regrid(iaustub%r3(jj)%q, aux2, __RC__ )

                 sdot=mydot_product_(reshape(aux2,(/ndim_ll/)),&
                                     reshape(aux2,(/ndim_ll/)) )
                 call MPI_allreduce(sdot,rdot1,1,MP_TYPE(sdot),MP_SUM,comm,status)
    
                 ! apply T'' to Tx
                 ! ---------------
                 C2L_AD = TransposeRegridder(C2L)

                 call C2L_AD%regrid(aux2, aux, __RC__ )

                 ! < T''Tx, x >
                 ! ------------
                 sdot=mydot_product_(reshape(iaustub%r3(jj)%q,(/ndim_cb/)), &
                                     reshape(aux,             (/ndim_cb/))  )
                 call MPI_allreduce(sdot,rdot2,1,MP_TYPE(sdot),MP_SUM,comm,status)

                 if ( MAPL_am_I_root() ) then
                      print *
                      print *, 'Testing AD of T=C2L ...'
                      print *
                      print *, '(Tx,Tx)  = ', rdot1
                      print *, '(T''Tx,x) = ', rdot2
                      print *, 'rel error = ', abs(rdot1-rdot2)/rdot1
                      print *
                 end if
                 deallocate(aux2)
                 deallocate(aux)

            endif ! <c2l_adtest>

            if ( l2c_adtest ) then

!                Create adjoint of transform from cube to lat-lon
!                ------------------------------------------------

                 im_ll = size(iaubase%r3(jj)%q,1)
                 jm_ll = size(iaubase%r3(jj)%q,2)
                 km_ll = size(iaubase%r3(jj)%q,3)
                 ndim_ll = im_ll*jm_ll*km_ll
                 ii = MAPL_SimpleBundleGetIndex ( iaubase, 'DTDT', 3, __RC__ )
                 im_cb = size(iaustub%r3(ii)%q,1)
                 jm_cb = size(iaustub%r3(ii)%q,2)
                 km_cb = size(iaustub%r3(ii)%q,3)
                 ndim_cb = im_cb*jm_cb*km_cb
                 jj = MAPL_SimpleBundleGetIndex ( iaustub, 'DTDT', 3, __RC__ )

                 ! x=ll
                 ! T=L2C
                 ! < Tx, Tx >
                 ! ----------
                 sdot=mydot_product_(reshape(iaustub%r3(jj)%q,(/ndim_cb/)),&
                                     reshape(iaustub%r3(jj)%q,(/ndim_cb/)) )
                 call MPI_allreduce(sdot,rdot1,1,MP_TYPE(sdot),MP_SUM,comm,status)

                 ! apply T'' to Tx
                 ! ---------------
                 allocate(aux(im_ll,jm_ll,km_ll))
                 L2C_AD = TransposeRegridder(L2C)

                 call L2C_AD%regrid(iaustub%r3(jj)%q, aux, __RC__ )

                 ! < T''Tx, x >
                 ! ------------
                 sdot=mydot_product_(reshape(iaubase%r3(ii)%q,(/ndim_ll/)), &
                                     reshape(aux             ,(/ndim_ll/))  )
                 call MPI_allreduce(sdot,rdot2,1,MP_TYPE(sdot),MP_SUM,comm,status)

                 if ( MAPL_am_I_root() ) then
                      print *
                      print *, 'Testing AD of T=L2C ...'
                      print *
                      print *, '(Tx,Tx)  = ', rdot1
                      print *, '(T''Tx,x) = ', rdot2
                      print *, 'rel error = ', abs(rdot1-rdot2)/rdot1
                      print *
                 end if
                 deallocate(aux)

            endif ! <l2c_adtest>

            !call MAPL_HorzTransformDestroy(L2C, __RC__ )


        else

!           Regrid bundle
!           -------------
            call ESMFL_Regrid ( IAUBundleBase, IAUbundleStub, __RC__ )

        endif

        call zeit_co('Regrid')

   endif

   if (SDFoutput) then

!      Write out increment as SDF output
!      ---------------------------------
       IOBundle = ESMF_FieldBundleCreate ( name='IO bundle', __RC__ )
       call ESMF_FieldBundleSet(IOBundle, grid=GCMgrid, __RC__ )
       call ESMF_FieldBundleGet(IAUBundleStub,FieldCount=nfld, __RC__ )
       do ifld=1,nfld
          call ESMF_FieldBundleGet(IAUBundleStub, ifld, Field, __RC__ )
          call ESMF_FieldGet(Field, NAME=NAME, __RC__ )
          if (trim(NAME)=='DPEDT') then
              call ESMF_FieldGet(Field,fArrayPtr=ptr3d,rc=status)
              mx=size(ptr3d,1)
              my=size(ptr3d,2)
              ! handle first LM levels 
              allocate(aux3d(mx,my,LM_IAU))
              aux3d = ptr3d(:,:,0:LM_IAU-1)
              Field = ESMF_FieldCreate(grid=GCMgrid, fArrayptr=aux3d, &
                                       name='DPEM1DT', &
                                       datacopyflag=ESMF_DATACOPY_VALUE, __RC__ )
              call ESMF_AttributeSet(Field, NAME='VLOCATION', &
                                  VALUE=MAPL_VLocationCenter,__RC__)
              call ESMF_AttributeSet(Field, NAME='DIMS', &
                                     VALUE=MAPL_DimsHorzVert,__RC__)
              call MAPL_FieldBundleAdd(IOBundle, Field, rc=STATUS)
              ! handle level LM+1
              allocate(ptr2d(mx,my))
              ptr2d = ptr3d(:,:,LM_IAU)
              call ESMF_GridGet(GCMgrid,dimCount=dimCount,__RC__)
              if (dimCount == 2) then
                 allocate(gridToFieldMap(2))
                 gridToFieldMap(1) = 1
                 gridToFieldMap(2) = 2
              else if (dimCount ==3) then
                 allocate(gridToFieldMap(3))
                 gridToFieldMap(1) = 1
                 gridToFieldMap(2) = 2
                 gridToFieldMap(3) = 0
              end if
              Field = ESMF_FieldCreate(grid=GCMgrid, fArrayptr=ptr2d, name='DPSDT', &
                                        gridToFieldMap=gridToFieldMap, &
                                        datacopyflag=ESMF_DATACOPY_VALUE, __RC__ )
              call ESMF_AttributeSet(Field, NAME='VLOCATION', &
                                     VALUE=MAPL_VLocationNone,__RC__)
              call ESMF_AttributeSet(Field, NAME='DIMS', &
                                     VALUE=MAPL_DimsHorzVert,__RC__)
              call MAPL_FieldBundleAdd(IOBundle, Field, rc=STATUS)
              deallocate(ptr2d)
              deallocate(gridToFieldMap)
          else  ! done with edge field
                ! handle non-edge fields ...
              call MAPL_FieldBundleAdd(IOBundle, Field, rc=STATUS)
          endif
       enddo
       call StrTemplate ( ofname, trim(sdf_ofname), 'GRADS', nymd=nymd, nhms=nhms, stat=status )
       VERIFY_(status)
       call MAPL_CFIOCreate ( CFIO, trim(ofname), CLOCK, IOBundle,  &
!!                            FREQUENCY=freq, &
!                             LEVELS=levels, &
                              DESCR='IAU Forcing Fields', __RC__ )
       call MAPL_CFIOWrite ( CFIO, CLOCK, IOBundle )
       call MAPL_cfioDestroy ( CFIO )

    endif ! <SDFoutput>

   end subroutine connect_

   subroutine writefld_(ku,am_I_root,im_world,jm_world,GCMgrid,fldi)
   implicit none
   type(ESMF_Grid)         :: GCMgrid   ! GCM Grid
   integer ku, im_world, jm_world
   real :: fldi(:,:,:)
   logical am_i_root
   real,allocatable :: work2d(:,:)
   integer k,km_world, status
   km_world=size(fldi,3)
   allocate(work2d(im_world,jm_world))
   do k=1,km_world  ! this would be a good place to swapV
      call ArrayGather(fldi(:,:,k), work2d, GCMgrid, rc=status)
      if(am_i_root) then 
        write(ku) work2d
      endif
   enddo
   deallocate(work2d)
   end subroutine writefld_

   double precision function mydot_product_ (x,y)
!  Todling - add after Jong Kim found fortran dot_product not to sufffice for
!            ad-test (sums must be accummulated at high precision)
   implicit none
   real,intent(in) :: x(:),y(:)
   integer n,m,i
   n=size(x)
   m=size(y)
   mydot_product_=0.d0
   if (n==m) then
      do i=1,n
         mydot_product_ = mydot_product_ + x(i)*y(i)
      enddo
   endif
   end function mydot_product_

   subroutine DefVertGrid_(CF,ak,bk,nsig)
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

!  local variables
   character(len=*), parameter       :: IAm='DefVertGrid_'
   character(len=20)                 :: vgridlabl
   character(len=3)                  :: cnsig
   integer                           :: i,k,ks,myID,ierr
   real*8                            :: ptop,pint

! start

   if(MAPL_AM_I_ROOT()) print *,trim(Iam),': Get GSI g.c. parameters '

! Create the label to be searched for in the RC file based on nsig

   call MP_comm_rank(comm,myID,ierr)
   if(myID==0) then
     call set_eta ( nsig,ks,ptop,pint,ak,bk )
   endif
   call mpi_bcast(ak, nsig+1,MP_REAL8,0,comm,status)
   call mpi_bcast(bk, nsig+1,MP_REAL8,0,comm,status)

   if(MAPL_AM_I_ROOT()) then
      print *,trim(IAm),' - lev, ak, bk - '
      do i=1,nsig+1
         write(*,'(1x,i3,2f16.6)') i,ak(i),bk(i)
      end do
   end if

   end subroutine DefVertGrid_

! What follows is here for consistency with GMAO_hermes:
!    hermes_levels_
!    dpref_
! ------------------------------------------------------
   subroutine hermes_levels_ (lev)

     implicit none
     real,pointer,intent(inout) :: lev(:)

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

!  Reference pressure thickness assuming ps ~ 984 hPa
!  ---------------------------------------------------
   real function dpref_ (k)
     implicit none
     integer k
     dpref_   = ( ak(k+1) - ak(k) ) + &
                ( bk(k+1) - bk(k) ) * 98400.
   end function dpref_

   subroutine final_
    call ESMF_FieldBundleDestroy (BkgBundle, __RC__)
    deallocate(levels)
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
   call CFIO_Open ( trim(fname), READ_ONLY, fid, ier )
   if ( ier .ne. 0 ) then
     write(6,*) 'dyn_getdim: trouble reading dims from ',trim(fname)
     rc = 1
     return
   endif

!  Get dimensions
!  --------------
   call CFIO_DimInquire ( fid, myim, myjm, mykm, mylm, nvars, ngatts, rc=ier )
   if ( ier .ne. 0 ) then
     write(6,*) 'dyn_getdim: trouble getting dims from ',trim(fname)
     rc = 2
     return
   endif

! Close file
! ----------
  call CFIO_close ( fid, ier )

  im = myim
  jm = myjm
  km = mykm
  lm = mylm

  end subroutine getdim_

   subroutine xwritit ( q,im,jm,lm,ku,Grid )
         use ESMF, only: ESMF_VMGetCurrent
         use ESMF, only: ESMF_VMGet
         use ESMF, only: ESMF_VM
         use ESMF, only: ESMF_GridCompGet
         use ESMF, only: ESMF_GridComp
         use ESMF, only: ESMF_Grid
         implicit none
         type ( ESMF_Grid ) Grid
         integer  im,jm,lm
         real   q(im,jm,lm)
   !
         type(ESMF_GridComp),pointer :: GC
         type(ESMF_VM) :: vm
         real,   allocatable :: glo(:,:)
         real*4, allocatable ::   a(:,:)
         integer  L,ku,img,jmg,myid,status
         integer  dims(3)
         call ESMF_VMGetCurrent(vm=vm, rc=status)
         call ESMF_VMGet(vm, localPET=myid)
         call MAPL_GridGet(Grid, globalCellCountPerDim=DIMS, RC=STATUS)
         img=dims(1)
         jmg=dims(2)
         allocate ( glo(img,jmg) )
         allocate (   a(img,jmg) )
         do L=1,lm
   !        call timebeg ('   Gather')
            call ArrayGather(q(:,:,L), glo, Grid, rc=status)
   !        call timeend ('   Gather')
            if( myid.eq.0 ) then
                          a = glo
                write(ku) a
            endif
         enddo
         deallocate ( glo )
         deallocate ( a   )
         return
   end subroutine xwritit

   subroutine var2d_summary_ (varname, var)
   use m_mpif90, only: MP_TYPE,MP_MAX,MP_MIN
   implicit none
   character(len=*) :: varname
   real,pointer, intent(in) :: var(:,:)

   real smax, smin
   real rmax, rmin

   smax = maxval(var); smin = minval(var)
   call MPI_allreduce(smax,rmax,1,MP_TYPE(smax),MP_MAX,comm,status)
   call MPI_allreduce(smin,rmin,1,MP_TYPE(smin),MP_MIN,comm,status)
   if ( MAPL_am_I_root() ) then
        write(6,'(2a,1p,2(a,e13.6))') trim(varname), ':', ' min= ', smin, ' max= ', smax
   endif
   end subroutine var2d_summary_

   subroutine var3d_summary_ (varname, var)
   use m_mpif90, only: MP_TYPE,MP_MAX,MP_MIN
   implicit none
   character(len=*) :: varname
   real,pointer, intent(in) :: var(:,:,:)

   real smax, smin
   real rmax, rmin

   smax = maxval(var); smin = minval(var)
   call MPI_allreduce(smax,rmax,1,MP_TYPE(smax),MP_MAX,comm,status)
   call MPI_allreduce(smin,rmin,1,MP_TYPE(smin),MP_MIN,comm,status)
   if ( MAPL_am_I_root() ) then
        write(6,'(2a,1p,2(a,e13.6))') trim(varname), ':', ' min= ', smin, ' max= ', smax
   endif
   end subroutine var3d_summary_

end Program mkIAU
