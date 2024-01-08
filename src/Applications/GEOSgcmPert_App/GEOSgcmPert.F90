! $Id$

! *********************************************************************
! *****                      Main Program                          ****
! *****         Finite-Volume Dynamical Core (Lin/Rood)            ****
! *****         Forced by GEOS5 Physics              ****
! *********************************************************************

#define I_AM_MAIN

#include "MAPL_Generic.h"
!#define DEBUG

Program GEOS5_Main


   use ESMF
   use MAPL
   use MPI

   use MAPL_HistoryGridCompMod, only : FHist_SetServices => SetServices
   use MAPL_HistoryGridCompMod, only : BHist_SetServices => SetServices
   use MAPL_HistoryGridCompMod, only : HISTORY_ExchangeListWrap

   use GEOS_AgcmPertGridCompMod, only:  ROOT_SetServices => SetServices
   use GEOS_PertSharedMod,       only: NUM_GSIvars, GSIvars, vec2list
   use GEOS_PertSharedMod,       only: apert_dot_product
   use GEOS_PertSharedMod,       only: apert_state2state

   use GEOS_PertSharedMod,       only: TLMPhase
   use GEOS_PertSharedMod,       only: ADJPhase
   use GEOS_PertSharedMod,       only: AgcmPert_phase => phase
   use GEOS_PertSharedMod,       only: phase_opAdjoint
   use GEOS_PertSharedMod,       only: phase_addImport
   use GEOS_PertSharedMod,       only: phase_getExport
   use GEOS_PertSharedMod,       only: DefVertGrid
   use GEOS_PertSharedMod,       only: ak=>pert_ak
   use GEOS_PertSharedMod,       only: bk=>pert_bk
   use GEOS_PertSharedMod,       only: GetShapiroCoeff
   use GEOS_PertSharedMod,       only: shapiro_coeff_=>pert_shapiro_coeff

   use MAPL_Profiler, only: BaseProfiler, get_global_time_profiler

   implicit none

!EOP

!EOC

   integer           :: STATUS
   character(len=18) :: Iam="GEOS5_Main"

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

! !ARGUMENTS:

    external                             :: ROOT_SetServices
    character*(*), optional, intent(IN ) :: Name
    logical,       optional, intent(OUT) :: AmIRoot
    integer,       optional, intent(OUT) :: rc

!EOPI

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
   type(ESMF_Config)            :: config,cf_fhist,cf_bhist
   type(ESMF_Config)            :: cf
   type(ESMF_Clock)             :: clock

   class (BaseProfiler),pointer :: t_p

! ErrLog variables
!-----------------

   integer                      :: STATUS
   character(len=ESMF_MAXSTR)   :: Iam="GEOSgcmPert"

! Misc locals
!------------

   character(len=ESMF_MAXSTR)   :: ROOT_CF
   character(len=ESMF_MAXSTR)   :: FHIST_CF
   character(len=ESMF_MAXSTR)   :: BHIST_CF
   character(len=ESMF_MAXSTR)   :: enableTimers
   character(len=ESMF_MAXSTR)   :: enableMemUtils

   logical                      :: AmIRoot_
   logical                      :: done
   integer                      :: printSpec

   integer                      :: HEARTBEAT_DT
   integer                      :: RUN_DT
   integer                      :: PERT_PHASE

   integer*8, pointer           :: FLSADDR(:) => null()
   integer*8, pointer           :: BLSADDR(:) => null()
   type(HISTORY_ExchangeListWrap) :: flswrap
   type(HISTORY_ExchangeListWrap) :: blswrap

   integer                      :: CallsPerWindow
   integer                      :: I, iv, PHASE, currPhase
   integer                      :: ipe, ipb

   character(len=ESMF_MAXSTR)   :: PERT_FILE, pert_file_def, fname, lstvars
   character(len=ESMF_MAXSTR)   :: DYNV_FILE, dynv_file_def
   type(ESMF_Grid)              :: LLPERTgrid
   type(ESMF_FieldBundle)       :: LLPertBundle
   type(ESMF_FieldBundle)       :: X0PertBundle
   type(ESMF_FieldBundle)       :: IniBundle
   type(ESMF_FieldBundle)       :: OutBundle
   type(ESMF_Time)              :: startTime, stopTime
   type(MAPL_CFIO)              :: cfio
   real, pointer                :: ptr(:,:,:)
   double precision             :: rdot(3)
   integer                      :: IM_PERT
   integer                      :: JM_PERT
   integer                      :: LM_PERT
   integer                      :: Nx, Ny
   integer                      :: dotp_test
   integer                      :: datetime(2)
   integer                      :: LDIMS(3)
   integer                      :: shapiro_filter
   type(LatLonGridFactory)      :: ll_factory
   type(ServerManager)          :: pert_server

   character(len=30) PERTGRIDNAME

! Begin
!------

!  Initialize ESMF
!-----------------

#if defined(ENABLE_ESMF_ERR_LOGGING)
   call ESMF_Initialize (vm=vm, rc=status)
#else
   call ESMF_Initialize (vm=vm, logKindFlag=ESMF_LOGKIND_NONE, rc=status)
#endif
   VERIFY_(STATUS)

   call MAPL_Initialize(rc=status)
   VERIFY_(status)
   t_p => get_global_time_profiler()
   call t_p%start("geosgcmpert.x")
   call pert_server%initialize(mpi_comm_world,rc=status)
   VERIFY_(status)

   AmIRoot_ = MAPL_Am_I_Root(vm)
   if (present(AmIRoot)) then
      AmIRoot = AmIRoot_
   end if

!  Open the CAP's configuration from CAP.rc
!------------------------------------------

   config = ESMF_ConfigCreate (                   rc=STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigLoadFile   ( config, 'CAP_apert.rc', rc=STATUS )
   VERIFY_(STATUS)

   call MAPL_GetNodeInfo(comm=mpi_comm_world,rc=status)
   VERIFY_(status)
   call MAPL_InitializeShmem(rc=status)
   VERIFY_(status)


!  CAP's MAPL MetaComp
!---------------------

   if(present(Name)) then
      call MAPL_Set (MAPLOBJ, name= Name, cf=CONFIG,    rc=STATUS )
      VERIFY_(STATUS)
   else
      call MAPL_Set (MAPLOBJ, name='CAP', cf=CONFIG,    rc=STATUS )
      VERIFY_(STATUS)
   end if

!  Create Clock. This is a private routine that sets the start and 
!   end times and the time interval of the clock from the configuration.
!   The start time is temporarily set to 1 interval before the time in the
!   configuration. Once the Alarms are set in intialize, the clock will
!   be advanced to guarantee it and its alarms are in the same state as they
!   were after the last advance before the previous Finalize.
!---------------------------------------------------------------------------

   call MAPL_ClockInit ( MAPLOBJ, clock,             rc=STATUS )
   VERIFY_(STATUS)

!  Get configurable info to create HIST 
!  and the ROOT of the computational hierarchy
!---------------------------------------------

!BOR

! !xRESOURCE_ITEM: string :: Name of ROOT's config file
   call MAPL_GetResource(MAPLOBJ, ROOT_CF,      "ROOT_CF:", &
                         default="ROOT.rc",       RC=STATUS ) 
   VERIFY_(STATUS)

! !xRESOURCE_ITEM: string :: Name to assign to the ROOT component
   call MAPL_GetResource(MAPLOBJ, ROOT_NAME,    "ROOT_NAME:",  &
                         default="ROOT",           RC=STATUS ) 
   VERIFY_(STATUS)

! !xRESOURCE_ITEM: string :: Name of HISTORY's config file 
   call MAPL_GetResource(MAPLOBJ, FHIST_CF,      "FHIST_CF:", &
                         default="FHIST.rc",       RC=STATUS ) 
   VERIFY_(STATUS)
   call MAPL_GetResource(MAPLOBJ, BHIST_CF,      "BHIST_CF:", &
                         default="BHIST.rc",       RC=STATUS ) 
   VERIFY_(STATUS)

! !xRESOURCE_ITEM: string :: Control Timers 
   call MAPL_GetResource(MAPLOBJ, enableTimers, "MAPL_ENABLE_TIMERS:", &
                         default='NO',             RC=STATUS )
   VERIFY_(STATUS)

! !xRESOURCE_ITEM: string :: Control Memory Diagnostic Utility 
   call MAPL_GetResource(MAPLOBJ, enableMemUtils, "MAPL_ENABLE_MEMUTILS:", &
                         default='NO',             RC=STATUS )
   VERIFY_(STATUS)

   if (enableTimers /= 'YES' .and. enableTimers /= 'yes') then
      call MAPL_ProfDisable( rc=STATUS )
      VERIFY_(STATUS)
   end if

  if (enableMemUtils /= 'YES' .and. enableMemUtils /= 'yes') then
     call MAPL_MemUtilsDisable( rc=STATUS )
     VERIFY_(STATUS)
  else
     call MAPL_MemUtilsInit( rc=STATUS )
     VERIFY_(STATUS)
  end if

   call MAPL_GetResource( MAPLOBJ, printSpec, label='PRINTSPEC:', default = 0, rc=STATUS )
   VERIFY_(STATUS)


! Handle RUN_DT in ROOT_CF
!-------------------------

   call ESMF_ConfigGetAttribute(config, value=HEARTBEAT_DT, &
        Label="HEARTBEAT_DT:", rc=status)
   VERIFY_(STATUS)

   cf = ESMF_ConfigCreate(rc=STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigLoadFile(cf, ROOT_CF, rc=STATUS )
   VERIFY_(STATUS)

   call ESMF_ConfigGetAttribute(cf, value=RUN_DT, Label="RUN_DT:", rc=status)
   if (STATUS == ESMF_SUCCESS) then
      if (heartbeat_dt /= run_dt) then
         if (MAPL_AM_I_Root(VM)) then
            print *, "ERROR: inconsistent values of HEATBEAT_DT and RUN_DT"
         end if
         call ESMF_VMBarrier(VM)
         RETURN_(ESMF_FAILURE)
      end if
   else
      call ESMF_ConfigSetAttribute(cf, value=heartbeat_dt, Label="RUN_DT:", rc=status)
!ALT: it is VERY important NOT to check the status
!     if the label is not in the config the above calls behaves correctly
!     but returns a non-zero exit code
   endif
   
   call ESMF_ConfigGetAttribute(cf, value=DOTP_TEST, &
        default=0,Label="DOTP_TEST:", rc=status)

   call ESMF_ConfigGetAttribute(cf, value=shapiro_filter, &
        default=0,Label="SHAPIRO_FILTER:", rc=status)

   if (dotp_test==0) then

!      Define PERT_PHASE: 1=TLM; 2=ADM; 0=all
!      --------------------------------------
       call ESMF_ConfigGetAttribute(cf, value=PERT_PHASE, &
            default=1,Label="PERT_PHASE:", rc=status)
       VERIFY_(STATUS)

   else
       PERT_PHASE=0
   endif

! Register the children with MAPL
!--------------------------------

!  Create Root child
!-------------------
   call MAPL_Set(MAPLOBJ, CF=CF, RC=STATUS)
   VERIFY_(STATUS)

   call cap_initialize ( CF, IM_PERT, JM_PERT, LM_PERT, Nx, Ny, status )

   call MAPL_DefGridName (IM_PERT,JM_PERT,PERTGRIDNAME,MAPL_am_I_root())
   ll_factory=LatLonGridFactory(grid_name=trim(PERTGRIDNAME),nx=nx,ny=ny, &
                                im_world=im_pert,jm_world=jm_pert,lm=lm_pert, &
                                pole='PC',dateline='DC',__RC__)
   LLPERTGrid = grid_manager%make_grid(ll_factory,__RC__)

   ROOT = MAPL_AddChild ( MAPLOBJ, Grid=LLPERTGrid, &
        name       = ROOT_NAME,        &
        SS         = ROOT_SetServices, &
                             rc=STATUS )  
   VERIFY_(STATUS)

!  Read in ak/bk from RC file and put'em in the LLgrid
!  ---------------------------------------------------
   allocate(ak(0:LM_PERT),bk(0:LM_PERT))
   call DefVertGrid (CF,ak,bk,LM_PERT,.true.,status)

   if (shapiro_filter == 1) then
       allocate(shapiro_coeff_(1:LM_PERT))      
       call GetShapiroCoeff (CF,shapiro_coeff_,LM_PERT,.true.,status)
   endif

!  Create History children
!-------------------------

   call ESMF_ConfigGetAttribute(cf, value = NX, Label="NX:", rc=status)
   VERIFY_(STATUS)
   call ESMF_ConfigGetAttribute(cf, value = NY, Label="NY:", rc=status)
   VERIFY_(STATUS)
  
   cf_fhist=ESMF_ConfigCreate(__RC__)
   call ESMF_ConfigLoadFile(cf_fhist,FHIST_CF,rc=status)
   VERIFY_(status) 
   cf_bhist=ESMF_ConfigCreate(__RC__)
   call ESMF_ConfigLoadFile(cf_bhist,BHIST_CF,rc=status)
   VERIFY_(status) 
   call MAPL_ConfigSetAttribute(cf_fhist, value=NX,  Label="NX:",  rc=status)
   VERIFY_(STATUS)
   call MAPL_ConfigSetAttribute(cf_fhist, value=NY,  Label="NY:",  rc=status)
   VERIFY_(STATUS)
   call MAPL_ConfigSetAttribute(cf_fhist, value=RUN_DT,  Label="RUN_DT:",  rc=status)
   VERIFY_(STATUS)
   call MAPL_ConfigSetAttribute(cf_bhist, value=NX,  Label="NX:",  rc=status)
   VERIFY_(STATUS)
   call MAPL_ConfigSetAttribute(cf_bhist, value=NY,  Label="NY:",  rc=status)
   VERIFY_(STATUS)
   call MAPL_ConfigSetAttribute(cf_bhist, value=RUN_DT,  Label="RUN_DT:",  rc=status)
   VERIFY_(STATUS)

   call MAPL_Set(MAPLOBJ,cf=cf_fhist,rc=status)
   VERIFY_(status)
   FHIST = MAPL_AddChild ( MAPLOBJ,      &
         name       = 'FHIST',           &
         SS         = FHIST_SetServices, &
                              rc=STATUS )  
   VERIFY_(STATUS)
   call MAPL_Set(MAPLOBJ,cf=cf_bhist,rc=status)
   VERIFY_(status)
   BHIST = MAPL_AddChild ( MAPLOBJ,      &
         name       = 'BHIST',           &
         SS         = BHIST_SetServices, &
                              rc=STATUS )  
   VERIFY_(STATUS)
   call MAPL_Set(MAPLOBJ,cf=cf,rc=status)
   VERIFY_(status)

!  Query MAPL for the the children's for GCS, IMPORTS, EXPORTS
!-------------------------------------------------------------

   call MAPL_Get ( MAPLOBJ, GCS=GCS, GIM=IMPORTS, GEX=EXPORTS,      RC=STATUS )
   VERIFY_(STATUS)

   call MAPL_GetObjectFromGC(GCS(ROOT), CHILD_MAPL, RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetResource(CHILD_MAPL, CallsPerWindow, 'CallsPerWindow:', &
        default=12,  rc=STATUS )
   VERIFY_(STATUS)                                         


! Create my child's grid
!-------------------
   call MAPL_GridCreate(GCS(ROOT), rc=status)
   VERIFY_(STATUS)

! Run as usual unless PRINTSPEC> 0 as set in CAP.rc. If set then
! model will not run completely and instead it will simply run MAPL_SetServices
! and print out the IM/EX specs. This step uses MAPL_StatePrintSpecCSV found
! in MAPL_Generic.F90.


   if (printSpec>0) then

      call MAPL_StatePrintSpecCSV(GCS(ROOT), printSpec, RC=status)
      VERIFY_(STATUS)
      call ESMF_VMBarrier       ( VM,                            RC=STATUS )
      VERIFY_(STATUS)

   else

!  Initialize the Computational Hierarchy
!----------------------------------------

   call ESMF_GridCompInitialize ( GCS(ROOT), importState=IMPORTS(ROOT), &
                                  exportState=EXPORTS(ROOT), &
                                  clock=CLOCK, userRC=STATUS )
   VERIFY_(STATUS)

! All the EXPORTS of the Hierachy are made IMPORTS of History
!------------------------------------------------------------

   call ESMF_StateAdd ( IMPORTS(FHIST), (/EXPORTS(ROOT)/),       RC=STATUS )
   VERIFY_(STATUS)
   call ESMF_StateAdd ( IMPORTS(BHIST), (/EXPORTS(ROOT)/),       RC=STATUS )
   VERIFY_(STATUS)


   allocate(flswrap%ptr, stat=status)
   VERIFY_(STATUS)
   call ESMF_UserCompSetInternalState(GCS(FHIST), 'MAPL_LocStreamList', &
        flswrap, STATUS)
   VERIFY_(STATUS)
   call MAPL_GetAllExchangeGrids(GCS(ROOT), FLSADDR, RC=STATUS)
   VERIFY_(STATUS)
   flswrap%ptr%LSADDR_PTR => FLSADDR

   allocate(blswrap%ptr, stat=status)
   VERIFY_(STATUS)
   call ESMF_UserCompSetInternalState(GCS(BHIST), 'MAPL_LocStreamList', &
        blswrap, STATUS)
   VERIFY_(STATUS)
   call MAPL_GetAllExchangeGrids(GCS(ROOT), BLSADDR, RC=STATUS)
   VERIFY_(STATUS)
   blswrap%ptr%LSADDR_PTR => BLSADDR

! Set initial condition IMPORT
!=============================

!   Create bundle to hold input perturbation and read pert into bundle
!   ------------------------------------------------------------------
    LLPertBundle = ESMF_FieldBundleCreate ( name='Pert bundle', __RC__)
    call ESMF_FieldBundleSet ( LLPertBundle, grid=LLPERTgrid, __RC__)

!   Get pointers to IMPORT (associate pointers)
!   -------------------------------------------
    call MAPL_GridCreate(GCS(ROOT), ESMFGRID=LLPertGrid, __RC__ )
    do iv = 1, NUM_GSIvars
       call MAPL_GetPointer(IMPORTS(ROOT), ptr, GSIvars(iv),  __RC__ )
    enddo

    if ( PERT_PHASE == 0 .or.  PERT_PHASE == 1 ) then
         dynv_file_def = 'fvpert.eta.nc4'
         pert_file_def = 'fvpert.eta.nc4'
    endif
    if ( PERT_PHASE == 2                       ) then
         dynv_file_def = 'Jgradf.eta.nc4'
         pert_file_def = 'Jgradf.eta.nc4'
    endif
    call MAPL_GetResource ( MAPLOBJ, PERT_FILE,'PERT_FILE:', default=pert_file_def, __RC__ )
    call MAPL_GetResource ( MAPLOBJ, DYNV_FILE,'PERT_FILE:', default=dynv_file_def, __RC__ )

    lstvars = 'u,v,tv,delp,sphu,ozone,qitot,qltot'!_RT vec2list(GSIvars)
    IniBundle = ESMF_FieldBundleCreate ( name='Initial Pert Bundle', __RC__)
    call ESMF_FieldBundleSet ( IniBundle,    grid=LLPERTgrid, __RC__)
    call ESMFL_State2Bundle(IMPORTS(ROOT),LLPertBundle)
    if ( PERT_PHASE == 0 .or.  PERT_PHASE == 1 ) then
        call ESMF_ClockGet(Clock, CurrTime=startTime, __RC__)
        call MAPL_CFIORead(PERT_FILE,  startTime, IniBundle, only_vars=lstvars, __RC__ )
        call BundleExt2Int(IniBundle,LLPertBundle)
        if(dotp_test) then
           !Create a new empty bundle with GCM variable names
           X0PertBundle = MAPL_BundleCreate ( name='X0 bundle', grid=LLPERTgrid, fieldNames=GSIvars, __RC__ )
           !Copy the GSI vars in IniBundle to the GCM vars in X0Bundle
           call BundleExt2Int(IniBundle,X0PertBundle)
           call mychopit ( LLPertBundle )
           call mychopit ( X0PertBundle )
           rdot    = 0.d0
           rdot(1) = apert_dot_product (LLPertBundle,LLPertBundle,__RC__)
        endif
    else
        call ESMF_ClockGet(Clock, StopTime=stopTime, __RC__)
        call MAPL_CFIORead(PERT_FILE,  stopTime, IniBundle, only_vars=lstvars, __RC__ )
        call BundleExt2Int(IniBundle,LLPertBundle)
    endif
    call ESMFL_Bundle2State (LLPertBundle, IMPORTS(ROOT),__RC__)


! Initialize the History
!------------------------

   call ESMF_GridCompInitialize ( GCS(FHIST), importState=IMPORTS(FHIST), &
                                  exportState=EXPORTS(FHIST), &
                                  clock=CLOCK, userRC=STATUS )
   VERIFY_(STATUS)
   call ESMF_GridCompInitialize ( GCS(BHIST), importState=IMPORTS(BHIST), &
                                  exportState=EXPORTS(BHIST), &
                                  clock=CLOCK, userRC=STATUS )
   VERIFY_(STATUS)
 

! FORWARD Time Loop starts by checking for Segment Ending Time
!-------------------------------------------------------------

   if (PERT_PHASE==0 .or. PERT_PHASE==1) then

    phase=TLMPhase
    done=.false.
    FORWARD_TIME_LOOP: do

      call MAPL_MemUtilsWrite(vm, 'GEOSgcmPert:TimeLoop',           RC=STATUS )
      VERIFY_(STATUS)

!@      DONE = ESMF_ClockIsStopTime( CLOCK,                        RC=STATUS )
!@      VERIFY_(STATUS)

      if ( DONE ) exit

! Call Record for intermediate checkpoint (if desired)
!  This is currently implemented as a phase of Finalize
!  rather than as a separate registered method. Note that
!  we are not doing a Record for History.
! ------------------------------------------------------
      call ESMF_GridCompWriteRestart( GCS(ROOT), importState=IMPORTS(ROOT), &
                                      exportState=EXPORTS(ROOT), &
                                      clock=CLOCK, userRC=STATUS )
      VERIFY_(STATUS)

!     Run the Gridded Component
!     --------------------------
      DO I=1, CallsPerWindow
         currPhase=AgcmPert_phase(adjoint=phase==ADJphase,	&
				 import =(I==1), export=.true.)

!        Run TLM ...
!        -----------
         call ESMF_GridCompRun(GCS(ROOT), importState=IMPORTS(ROOT), &
                               exportState=EXPORTS(ROOT), &
                               clock=CLOCK, phase=currPhase, userRC=STATUS )
         VERIFY_(STATUS)

!        Synchronize for Next TimeStep  _RT Why do you needs this??
!        -----------------------------
         call ESMF_VMBarrier ( VM, RC=STATUS )
         VERIFY_(STATUS)

!        Advance the Clock before running History and Record
!        ---------------------------------------------------
         call ESMF_ClockSet( CLOCK, direction=ESMF_DIRECTION_FORWARD, RC=status )
         VERIFY_(STATUS)
         call ESMF_ClockAdvance ( CLOCK, RC=STATUS )
         VERIFY_(STATUS)

!        Call History Run for Output
!        ---------------------------
         call ESMF_GridCompRun ( GCS(FHIST), importState=IMPORTS(FHIST), exportState=EXPORTS(FHIST), &
                                 clock=CLOCK, userRC=STATUS )
         VERIFY_(STATUS)

      END DO
      DONE=.true.

      call ESMFL_State2Bundle(EXPORTS(ROOT),LLPertBundle)
      OutBundle = ESMF_FieldBundleCreate ( name='output bundle', __RC__)
      call ESMF_FieldBundleSet ( OutBundle, grid=LLPERTgrid, __RC__)
      call MAPL_CFIORead (DYNV_FILE,  startTime, OutBundle, noread=.true., __RC__ )
      call BundleInt2Ext (PERT_PHASE,bk,LLpertBundle,Outbundle)
      call MAPL_CFIOCreate ( cfio, 'fvpert.eta.%y4%m2%d2_%h2z.nc4', clock, OutBundle,  &
                             DESCR='Write TLM State', &
                             EXPID='junk', __RC__ )
      call MAPL_CFIOSet ( cfio, Root=1, __RC__ )

      ! Write out evolved perturbation at final time
      ! --------------------------------------------
      if (dotp_test==0) then !_DH only do if not in middle of dotp test
         call MAPL_CFIOWrite ( cfio, clock, OutBundle, verbose = .true., __RC__ )
         call MAPL_cfioDestroy ( cfio )
      endif

!      If so, calculate <Tx,Tx>
!      ------------------------
      if (dotp_test) then
          rdot(2) = apert_dot_product (EXPORTS(ROOT),EXPORTS(ROOT),__RC__)
      endif

    enddo FORWARD_TIME_LOOP ! end of FORWARD time loop

   endif ! check on PERT_PHASE


! BACKWARD Time Loop starts by checking for Segment Ending Time
!--------------------------------------------------------------

   if (PERT_PHASE==0 .or. PERT_PHASE==2) then

!   If so, advance history clock all the way to the end
!   ---------------------------------------------------
    if ( PERT_PHASE==2 )  then
        DO I=1, CallsPerWindow
           call ESMF_ClockSet( CLOCK, direction=ESMF_DIRECTION_FORWARD, __RC__ )
           call ESMF_ClockAdvance ( CLOCK, __RC__ )
        enddo
    endif

!   In case TLM has just ran, start ADM from TLM output
!   ---------------------------------------------------
    if ( PERT_PHASE==0 )  then
        call apert_state2state (EXPORTS(ROOT),IMPORTS(ROOT))
    endif
    
    phase=ADJPhase
    done=.false.
    do I=1, CallsPerWindow
       currPhase=AgcmPert_phase(adjoint=phase==ADJphase,	&
			 import =(I==1), export=.true.)

!      Call History Run for Output
!      ---------------------------
       call ESMF_GridCompRun ( GCS(BHIST), importState=IMPORTS(BHIST), exportState=EXPORTS(BHIST), &
                               clock=CLOCK, userRC=STATUS )
       VERIFY_(STATUS)

!      Run ADM ...
!      -----------
       call ESMF_GridCompRun(GCS(ROOT), importState=IMPORTS(ROOT), &
                             exportState=EXPORTS(ROOT), &
                             clock=CLOCK, phase=currPhase, userRC=STATUS )
       VERIFY_(STATUS)

!      Synchronize for Next TimeStep  _RT Why do you needs this??
!      -----------------------------
       call ESMF_VMBarrier ( VM, RC=STATUS )
       VERIFY_(STATUS)

!      Advance the Clock before running History and Record
!      ---------------------------------------------------
       call ESMF_ClockSet( CLOCK, direction=ESMF_DIRECTION_REVERSE, RC=status )
       VERIFY_(STATUS)
       call ESMF_ClockAdvance ( CLOCK, RC=STATUS )
       VERIFY_(STATUS)

!      Call History Run for Output
!      ---------------------------
!      call ESMF_GridCompRun ( GCS(BHIST), importState=IMPORTS(BHIST), exportState=EXPORTS(BHIST), &
!                              clock=CLOCK, userRC=STATUS )
!      VERIFY_(STATUS)

    enddo

    if (dotp_test==0) then !_DH only do if not doing dotp test else causes crash
       call ESMFL_State2Bundle(EXPORTS(ROOT),LLPertBundle)
       OutBundle = ESMF_FieldBundleCreate ( name='output bundle', __RC__)
       call ESMF_FieldBundleSet ( OutBundle, grid=LLPERTgrid, __RC__)
       call MAPL_CFIORead (DYNV_FILE,  stopTime, OutBundle, noread=.true., __RC__ )
       call BundleInt2Ext (PERT_PHASE,bk,LLpertBundle,Outbundle)
       call MAPL_CFIOCreate ( cfio, 'fsens.eta.%y4%m2%d2_%h2z.nc4', clock, OutBundle,  &
                              DESCR='Write ADM State', &
                              EXPID='junk', __RC__ )
       call MAPL_CFIOSet ( cfio, Root=1, __RC__ )
       ! Write out sensitivity at final time
       call MAPL_CFIOWrite ( cfio, clock, OutBundle, verbose = .true., __RC__ )
       call MAPL_cfioDestroy ( cfio )
    endif

#ifdef SOON
    BACKWARD_TIME_LOOP: do

      call MAPL_MemUtilsWrite(vm, 'GEOSgcmPert:TimeLoop',           RC=STATUS )
      VERIFY_(STATUS)

!@      DONE = ESMF_ClockIsStopTime( CLOCK,                        RC=STATUS )
!@      VERIFY_(STATUS)

      if ( DONE ) exit

! Call Record for intermediate checkpoint (if desired)
!  This is currently implemented as a phase of Finalize
!  rather than as a separate registered method. Note that
!  we are not doing a Record for History.
! ------------------------------------------------------

! What is this?
!_RT  call ESMF_GridCompWriteRestart( GCS(ROOT), IMPORTS(ROOT), EXPORTS(ROOT), &
!_RT       CLOCK,  RC=STATUS )
!_RT  VERIFY_(STATUS)

      DO I=1, CallsPerWindow

!       Call History Run for Output
!       ---------------------------
!       call ESMF_GridCompRun ( GCS(BHIST), IMPORTS(BHIST), EXPORTS(BHIST), &
!                               CLOCK, userRC=STATUS )
!       VERIFY_(STATUS)

!       Advance the Clock (backward) after running History
!       --------------------------------------------------
        call ESMF_ClockSet( CLOCK, direction=ESMF_DIRECTION_REVERSE, rc=status )
        VERIFY_(STATUS)
        call ESMF_ClockAdvance ( CLOCK, RC=STATUS )
        VERIFY_(STATUS)
        call echo_time_(CLOCK,'bkwd   ')

!       Run the Gridded Component
!       --------------------------
        currPhase=AgcmPert_phase(adjoint=phase==ADJphase,	&
				 import =(I==1), export=.true.)

        call ESMF_GridCompRun(GCS(ROOT), importState=IMPORTS(ROOT), &
                              exportState=EXPORTS(ROOT), &
                              clock=CLOCK, phase=currPhase, userRC=STATUS )
        VERIFY_(STATUS)

!       Synchronize for Next TimeStep
!       -----------------------------
        call ESMF_VMBarrier ( VM, RC=STATUS )
        VERIFY_(STATUS)

      END DO
      DONE=.true.

!     Write out hitory from final state
!     ---------------------------------
!     call ESMF_GridCompRun      ( GCS(BHIST), IMPORTS(BHIST), EXPORTS(BHIST), &
!                                  CLOCK, userRC=STATUS )
!     VERIFY_(STATUS)

!     Since backward history does not work we will force output
!     writing EXPORT out directly via CFIOWrite
!     ---------------------------------------------------------
       call ESMFL_State2Bundle(EXPORTS(ROOT),LLPertBundle)
       OutBundle = ESMF_FieldBundleCreate ( name='output bundle', __RC__)
       call ESMF_FieldBundleSet ( OutBundle, grid=LLPERTgrid, __RC__)
       call MAPL_CFIORead (DYNV_FILE,  stopTime, OutBundle, noread=.true., __RC__ )
       call BundleInt2Ext (PERT_PHASE,bk,LLpertBundle,Outbundle)
       call MAPL_CFIOCreate ( cfio, 'fsens.eta.%y2%m2%d2_%h2z.nc4', clock, OutBundle,  &
                              DESCR='Write ADM State', &
                              EXPID='junk', __RC__ )
       call MAPL_CFIOSet ( cfio, Root=1, __RC__ )
       call MAPL_CFIOWrite ( cfio, clock, OutBundle, verbose = .true., __RC__ )
       call MAPL_cfioDestroy ( cfio )

    enddo BACKWARD_TIME_LOOP ! end of BACKWARD time loop
#endif /* SOON */

    if (dotp_test) then

!       Complete dot product test
!       -------------------------
          rdot(3) = apert_dot_product (EXPORTS(ROOT),X0PertBundle)
          if (MAPL_AM_I_Root(VM)) then
              print *
              print *, 'Testing ADM/TLM dot product ...'
              print *
              print *, '(x,x)    = ',  rdot(1)
              print *, '(Tx,Tx)  = ',  rdot(2)
              print *, '(T''Tx,x) = ', rdot(3)
              if(abs(rdot(2)>0.d0)) &
              print *, 'rel error = ', abs(rdot(2)-rdot(3))/rdot(2)
              print *
              call ESMF_FieldBundleDestroy (X0PertBundle, __RC__)
          endif

    endif

   endif ! check on PERT_PHASE

!  Finalize
!  --------

!_RT   call ESMF_GridCompFinalize( GCS(ROOT),IMPORTS(ROOT),EXPORTS(ROOT),CLOCK,userRC=STATUS )
!_RT   VERIFY_(STATUS)
!_RT   call ESMF_GridCompFinalize( GCS(BHIST),IMPORTS(BHIST),EXPORTS(BHIST),CLOCK,userRC=STATUS )
!_RT   call ESMF_GridCompFinalize( GCS(FHIST),IMPORTS(FHIST),EXPORTS(FHIST),CLOCK,userRC=STATUS )

!  Finalize itselt
! ----------------

!@@   call CAP_Finalize(CLOCK, "cap_restart", rc=STATUS)
!   VERIFY_(STATUS)

   end if ! PRINTSPEC


!  Finalize framework
!  ------------------
   deallocate(ak,bk)

   if (shapiro_filter==1) deallocate(shapiro_coeff_)

!   All done
!   --------
    if (MAPL_AM_I_ROOT()) then
          close(999)
          open (999,file='PERT_EGRESS',form='formatted')
          close(999)
    end if

   call ESMF_FieldBundleDestroy (OutBundle, __RC__)
   call ESMF_FieldBundleDestroy (IniBundle, __RC__)
   call ESMF_FieldBundleDestroy (LLPertBundle, __RC__)
   call t_p%stop('geosgcmpert.x')
   call MAPL_Finalize()
   call pert_server%finalize()
   call ESMF_Finalize (__RC__)

 end subroutine PERT_CAP

 subroutine echo_time_ ( CLOCK, msg )
 use m_StrTemplate
 implicit none
 type(ESMF_Clock) :: Clock
 character(len=*) :: msg
 
 type(ESMF_Time)  :: Time
 integer :: YY, MM, DD, HH, MN, SC, nymd, nhms
 integer :: rc
 
 call ESMF_ClockGet(Clock, CurrTime=Time, __RC__)
 call ESMF_TimeGet(Time, yy=YY, mm=MM, dd=DD, h=HH, m=MN, s=SC, rc=rc)
 if(MAPL_AM_I_ROOT()) write(6,'(a,1x,i4.4,4(a,i2.2))') trim(msg)//" The current TIME is ", &
                                     YY, "/", MM, "/", DD, " ", HH, ":", MN
 end subroutine echo_time_

 subroutine CAP_INITIALIZE ( CF, im, jm, lm, nx, ny, rc )
 implicit none
 
  type(ESMF_Config)     :: CF

  integer, intent(out)  :: im      !
  integer, intent(out)  :: jm      !
  integer, intent(out)  :: lm      !
  integer, intent(out)  :: nx      !
  integer, intent(out)  :: ny      !
  integer, intent(out)  :: rc      ! return error code

  character(len=*), parameter :: myname = 'cap_initialize'

  integer  status

!  Set defaults
!  ------------

   call ESMF_ConfigGetAttribute( CF, NX, label ='NX:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, NY, label ='NY:', __RC__ )

   call ESMF_ConfigGetAttribute( CF, IM, label ='AGCM_PERT_IM:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, JM, label ='AGCM_PERT_JM:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, LM, label ='AGCM_PERT_LM:', __RC__ )
 
   rc = 0

 end subroutine CAP_INITIALIZE

 subroutine CAP_FINALIZE ( clock,filen, rc )

   type(ESMF_Clock),    intent(in   ) :: clock
   character(len=*),    optional      :: filen
   integer, optional,   intent(  out) :: rc

   integer        :: UNIT
   integer        :: datetime(2)
   integer        :: YY, MM, DD, H, M, S
   integer        :: status
   character(len=ESMF_MAXSTR), parameter :: IAm="CAP_FINALIZE"
   character(len=ESMF_MAXSTR)            :: filen_

   type(ESMF_Time)     :: CurrentTime
   
   filen_ = "cap_restart"
   if (present(filen))     filen_ = trim(filen )
   
! Retrieve Current Time for Cap Restart
! -------------------------------------

   call ESMF_ClockGet ( clock, currTime=currentTime, rc=status )
   VERIFY_(STATUS)
   call ESMF_TimeGet  ( CurrentTime, YY = YY, &
                                     MM = MM, &
                                     DD = DD, &
                                     H  = H , &
                                     M  = M , &
                                     S  = S, rc=status )
   VERIFY_(STATUS)

   CALL MAPL_PackDateTime(DATETIME, YY, MM, DD, H, M, S)

! Write CAP Restart File and Ending Time for Current Segment
! ----------------------------------------------------------

    if( MAPL_AM_I_ROOT() ) then
       UNIT = GETFILE( filen_, form="formatted" )
       write(unit,100) datetime
100    format(i8.8,1x,i6.6)
       call FREE_FILE (UNIT)
    endif

    RETURN_(ESMF_SUCCESS)
  end subroutine CAP_FINALIZE

!BOPI

! !IROUTINE: MAPL_ClockInit -- Sets the clock

! !INTERFACE: 

  subroutine MAPL_ClockInit ( MAPLOBJ, Clock, rc)

! !ARGUMENTS:

     type(MAPL_MetaComp), intent(inout) :: MAPLOBJ
     type(ESMF_Clock),    intent(  out) :: Clock
     integer, optional,   intent(  out) :: rc

!  !DESCRIPTION:

!   This is a private routine that sets the start and 
!   end times and the time interval of the application clock from the configuration.
!   This time interal is the ``heartbeat'' of the application.
!   The Calendar is set to Gregorian by default. 
!   The start time is temporarily set to 1 interval before the time in the
!   configuration. Once the Alarms are set in intialize, the clock will
!   be advanced to guarantee it and its alarms are in the same state as they
!   were after the last advance before the previous Finalize.
!

!EOPI

     type(ESMF_Time)          :: StartTime    ! Initial     Begin  Time of Experiment
     type(ESMF_Time)          :: EndTime      ! Final       Ending Time of Experiment
     type(ESMF_Time)          :: StopTime     ! Final       Ending Time of Experiment
     type(ESMF_Time)          :: CurrTime     ! Current     Current Time of Experiment
     type(ESMF_TimeInterval)  :: timeStep     ! HEARTBEAT
     type(ESMF_TimeInterval)  :: duration
     type(ESMF_Calendar)      :: cal
     character(ESMF_MAXSTR)   :: CALENDAR

     integer                  :: STATUS
     character(ESMF_MAXSTR)   :: IAM="MAPL_ClockInit"

     integer        :: BEG_YY
     integer        :: BEG_MM
     integer        :: BEG_DD
     integer        :: BEG_H
     integer        :: BEG_M
     integer        :: BEG_S

     integer        :: CUR_YY
     integer        :: CUR_MM
     integer        :: CUR_DD
     integer        :: CUR_H
     integer        :: CUR_M
     integer        :: CUR_S

     integer        :: END_YY
     integer        :: END_MM
     integer        :: END_DD
     integer        :: END_H
     integer        :: END_M
     integer        :: END_S

     integer        :: DUR_YY
     integer        :: DUR_MM
     integer        :: DUR_DD
     integer        :: DUR_H
     integer        :: DUR_M
     integer        :: DUR_S

     integer        :: HEARTBEAT_DT
     integer        :: NUM_DT
     integer        :: DEN_DT

     integer        :: UNIT
     integer        :: datetime(2)

! Begin
!------

! Read Times From Config
! ----------------------

!BOR

     call MAPL_GetResource( MAPLOBJ, datetime, label='BEG_DATE:', rc=STATUS )
     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, BEG_YY, BEG_MM, BEG_DD, BEG_H, BEG_M, BEG_S)
     else

! !xRESOURCE_ITEM: year :: Beginning year (integer)
        call MAPL_GetResource( MAPLOBJ, BEG_YY, label='BEG_YY:', DEFAULT=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: month :: Beginning month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, BEG_MM, label='BEG_MM:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: day  :: Beginning day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, BEG_DD, label='BEG_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: hour :: Beginning hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, BEG_H , label='BEG_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: minute :: Beginning minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, BEG_M , label='BEG_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: second :: Beginning second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, BEG_S , label='BEG_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

     call MAPL_GetResource( MAPLOBJ, datetime, label='END_DATE:', rc=STATUS )
     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, END_YY, END_MM, END_DD, END_H, END_M, END_S)
     else
! !xRESOURCE_ITEM: year :: Ending year (integer)
        call MAPL_GetResource( MAPLOBJ, END_YY, label='END_YY:', DEFAULT=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: month :: Ending month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, END_MM, label='END_MM:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, END_DD, label='END_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, END_H , label='END_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: minute :: Ending minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, END_M , label='END_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: second :: Ending second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, END_S , label='END_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

! Replace JOB_DURATION with JOB_SGMT as prefered RC parameter
! -----------------------------------------------------------
     call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_SGMT:',     rc=STATUS )
     if(STATUS/=ESMF_SUCCESS) then
     call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_DURATION:', rc=STATUS )
     end if

     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, DUR_YY, DUR_MM, DUR_DD, DUR_H, DUR_M, DUR_S)
     else
! !xRESOURCE_ITEM: year :: Ending year (integer)
        call MAPL_GetResource( MAPLOBJ, DUR_YY, label='DUR_YY:', DEFAULT=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: month :: Ending month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, DUR_MM, label='DUR_MM:', default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, DUR_DD, label='DUR_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, DUR_H , label='DUR_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: minute :: Ending minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, DUR_M , label='DUR_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: second :: Ending second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, DUR_S , label='DUR_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

! !xRESOURCE_ITEM: seconds :: Interval of the application clock (the Heartbeat)
     call MAPL_GetResource( MAPLOBJ, HEARTBEAT_DT, label='HEARTBEAT_DT:',            rc=STATUS )
     VERIFY_(STATUS)
! !xRESOURCE_ITEM: 1 :: numerator of decimal fraction of time step
     call MAPL_GetResource( MAPLOBJ, NUM_DT, label='NUM_DT:', default=0, rc=STATUS )
     VERIFY_(STATUS)
! !xRESOURCE_ITEM: 1 :: denominator of decimal fraction of time step
     call MAPL_GetResource( MAPLOBJ, DEN_DT, label='DEN_DT:', default=1, rc=STATUS )
     VERIFY_(STATUS)
! !xRESOURCE_ITEM: string :: Calendar type
     call MAPL_GetResource( MAPLOBJ, CALENDAR, label='CALENDAR:', default="GREGORIAN", rc=STATUS )
     VERIFY_(STATUS)

!EOR

     ASSERT_(NUM_DT>=0)
     ASSERT_(DEN_DT> 0)
     ASSERT_(HEARTBEAT_DT>=0)
!     ASSERT_(NUM_DT*HEARTBEAT_DT>0)
     ASSERT_(NUM_DT<DEN_DT)

! initialize calendar to be Gregorian type
! ----------------------------------------

     if    (CALENDAR=="GREGORIAN") then
        cal = ESMF_CalendarCreate( ESMF_CALKIND_GREGORIAN, name="ApplicationCalendar", rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, RC=STATUS)
        VERIFY_(STATUS)
     elseif(CALENDAR=="JULIAN"   ) then
        cal = ESMF_CalendarCreate( ESMF_CALKIND_JULIAN, name="ApplicationCalendar", rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CALKIND_JULIAN, RC=STATUS)
        VERIFY_(STATUS)
     elseif(CALENDAR=="NOLEAP"   ) then
        cal = ESMF_CalendarCreate( ESMF_CALKIND_NOLEAP, name="ApplicationCalendar", rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CALKIND_NOLEAP, RC=STATUS)
        VERIFY_(STATUS)
     else
        ASSERT_(.false.)
     endif

! initialize start time for Alarm frequencies
! -------------------------------------------

     call ESMF_TimeSet( StartTime, YY = BEG_YY, &
                                   MM = BEG_MM, &
                                   DD = BEG_DD, &
                                    H = BEG_H , &
                                    M = BEG_M , &
                                    S = BEG_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)

     call ESMF_TimeSet(   EndTime, YY = END_YY, &
                                   MM = END_MM, &
                                   DD = END_DD, &
                                    H = END_H , &
                                    M = END_M , &
                                    S = END_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)  

! Read CAP Restart File for Current Time
! --------------------------------------

     CUR_YY = BEG_YY
     CUR_MM = BEG_MM
     CUR_DD = BEG_DD
     CUR_H  = BEG_H
     CUR_M  = BEG_M
     CUR_S  = BEG_S

     UNIT = GETFILE ( "cap_restart", form="formatted", ALL_PES=.true., rc=status )
     VERIFY_(STATUS)

     read(UNIT,100,err=999,end=999) datetime
100  format(i8.8,1x,i6.6)

     if( MAPL_AM_I_ROOT() ) then
         print *, 'Read CAP restart properly, Current Date = ', datetime(1)
         print *, '                           Current Time = ', datetime(2)
         print *
     endif

     CALL MAPL_UnpackDateTime(DATETIME, CUR_YY, CUR_MM, CUR_DD, CUR_H, CUR_M, CUR_S)


999  continue  ! Initialize Current time

     call FREE_FILE (UNIT)

     call ESMF_TimeSet( CurrTime, YY = CUR_YY, &
                                  MM = CUR_MM, &
                                  DD = CUR_DD, &
                                   H = CUR_H , &
                                   M = CUR_M , &
                                   S = CUR_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)

! initialize final stop time
! --------------------------

     call ESMF_TimeIntervalSet(  duration, YY = DUR_YY, &
                                   MM = DUR_MM, &
                                    D = DUR_DD, &
                                    H = DUR_H , &
                                    M = DUR_M , &
                                    S = DUR_S , &
!                                    calendar = cal, &
                                    startTime = currTime, &
                                    rc = STATUS  )
     VERIFY_(STATUS)

     stopTime = currTime + duration

! initialize model time step
! --------------------------

     call ESMF_TimeIntervalSet( timeStep, S=HEARTBEAT_DT, sN=NUM_DT, sD=DEN_DT, rc=STATUS )
     VERIFY_(STATUS)

! Create Clock and set it to one time step before StartTime.
! After Initialize has created all alarms, we will advance the
! clock to ensure the proper ringing state of all alarms
!-------------------------------------------------------------

     if (endTime < stopTime) then
        clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
             startTime=StartTime, stopTime=EndTime, rc=STATUS )
     else
        clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
             startTime=StartTime, stopTime=StopTime, rc=STATUS )
     end if
     VERIFY_(STATUS)

     call ESMF_ClockSet ( clock, CurrTime=CurrTime, rc=status )
     VERIFY_(STATUS)

     RETURN_(ESMF_SUCCESS)
   end subroutine MAPL_ClockInit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !subroutine MAPL_PackDateTime(DATETIME, YY, MM, DD, H, M, S)
     !integer, intent(IN   ) :: YY, MM, DD, H, M, S
     !integer, intent(  OUT) :: DATETIME(:)

     !datetime(1) = 10000*YY + 100*MM + DD
     !datetime(2) = 10000* H + 100* M + S
     !return
   !end subroutine MAPL_PackDateTime

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !subroutine MAPL_UnpackDateTime(DATETIME, YY, MM, DD, H, M, S)
     !integer, intent(IN   ) :: DATETIME(:)
     !integer, intent(  OUT) :: YY, MM, DD, H, M, S

     !YY =     datetime(1)/10000
     !MM = mod(datetime(1),10000)/100
     !DD = mod(datetime(1),100)
     !H  =     datetime(2)/10000
     !M  = mod(datetime(2),10000)/100
     !S  = mod(datetime(2),100)
     !return
   !end subroutine MAPL_UnpackDateTime

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This allows zeroing out fields from input bundle to help test ADM/TLM
   subroutine mychopit ( Bundle )
   implicit none
   type(ESMF_FieldBundle)  :: Bundle
   type(MAPL_SimpleBundle) :: bnd
   integer iptr
   integer status
   real x
#ifdef DEBUG
!   bnd  = MAPL_SimpleBundleCreate ( Bundle, rc=status )
!   iptr = MAPL_SimpleBundleGetIndex ( bnd, 'U', 3, rc=status )
!   bnd%r3(iptr)%q = 1.d0
!   CALL RANDOM_SEED()
!   CALL RANDOM_NUMBER(x)
!   bnd%r3(iptr)%q = bnd%r3(iptr)%q * (-x/100)

!   iptr = MAPL_SimpleBundleGetIndex ( bnd, 'V', 3, rc=status )
!   bnd%r3(iptr)%q = 1.d0
!   CALL RANDOM_SEED()
!   CALL RANDOM_NUMBER(x)
!   bnd%r3(iptr)%q = bnd%r3(iptr)%q * (-x/100)

!   iptr = MAPL_SimpleBundleGetIndex ( bnd, 'TV', 3, rc=status )
!   bnd%r3(iptr)%q = 1.d0
!   CALL RANDOM_SEED()
!   CALL RANDOM_NUMBER(x)
!   bnd%r3(iptr)%q = bnd%r3(iptr)%q * (-x/100)

!   iptr = MAPL_SimpleBundleGetIndex ( bnd, 'DP', 3, rc=status )
!   bnd%r3(iptr)%q = 1.d0
!   CALL RANDOM_SEED()
!   CALL RANDOM_NUMBER(x)
!   bnd%r3(iptr)%q = bnd%r3(iptr)%q * (-x/100)

!   iptr = MAPL_SimpleBundleGetIndex ( bnd, 'QV', 3, rc=status )
!   bnd%r3(iptr)%q = 1.d0
!   CALL RANDOM_SEED()
!   CALL RANDOM_NUMBER(x)
!   bnd%r3(iptr)%q = bnd%r3(iptr)%q * (-x/100)
#endif

   end subroutine mychopit

   subroutine BundleExt2Int (DynBundle,IntBundle)
!  This subroutine maps the internal bundle onto a dyn-like vector
   implicit none
   type(ESMF_FieldBundle)  :: DynBundle ! dyn-like vector
   type(ESMF_FieldBundle)  :: IntBundle ! internal bundle
   type(MAPL_SimpleBundle) :: bnd
   type(MAPL_SimpleBundle) :: dyn
   integer iv,ibnd,idyn
   integer status
   integer, parameter :: nv=8
   character(len=*),parameter :: bvars(nv) = (/ &
                                              'U ', 'V ', 'TV', 'DP', &
                                              'QV', 'O3', 'QI', 'QL'  &
                                              /)
   character(len=*),parameter :: dvars(nv) = (/'u    ', 'v    ', 'tv   ', &
                                               'delp ', 'sphu ', 'ozone', &
                                               'qitot', 'qltot' /)
   do iv = 1, nv
      bnd  = MAPL_SimpleBundleCreate ( IntBundle, rc=status )
      dyn  = MAPL_SimpleBundleCreate ( DynBundle, rc=status )
      ibnd = MAPL_SimpleBundleGetIndex ( bnd, bvars(iv), 3, rc=status )
      idyn = MAPL_SimpleBundleGetIndex ( dyn, dvars(iv), 3, rc=status )
      bnd%r3(ibnd)%q = dyn%r3(idyn)%q
   enddo

   end subroutine BundleExt2Int

   subroutine BundleInt2Ext (thisphase,bk,IntBundle,DynBundle)
!  This subroutine maps the internal bundle onto a dyn-like vector
   use m_mpif90,only : MP_comm_rank
   use m_mpif90,only : MP_comm_world
   use m_mpif90,only : MP_REAL8
   use m_ioutil, only: luavail
   implicit none
   integer,intent(in)      :: thisphase
   real,pointer,intent(in) :: bk(:)
   type(ESMF_FieldBundle)  :: IntBundle ! internal bundle
   type(ESMF_FieldBundle)  :: DynBundle ! dyn-like vector
   type(MAPL_SimpleBundle) :: bnd
   type(MAPL_SimpleBundle) :: dyn
   integer iv,ibnd,idyn,k,kdim,lu,myid
   integer, parameter :: root=0
   integer status
   integer, parameter :: nv=8
   character(len=*),parameter :: bvars(nv) = (/ &
                                              'U ', 'V ', 'TV', 'DP', &
                                              'QV', 'O3', 'QI', 'QL'  &
                                              /)
   character(len=*),parameter :: dvars(nv) = (/'u    ', 'v    ', 'tv   ', &
                                               'delp ', 'sphu ', 'ozone', &
                                               'qitot', 'qltot' /)
   bnd  = MAPL_SimpleBundleCreate ( IntBundle, rc=status )
   dyn  = MAPL_SimpleBundleCreate ( DynBundle, rc=status )
   do iv = 1, nv
      ibnd = MAPL_SimpleBundleGetIndex ( bnd, bvars(iv), 3, rc=status )
      idyn = MAPL_SimpleBundleGetIndex ( dyn, dvars(iv), 3, rc=status )
      dyn%r3(idyn)%q = bnd%r3(ibnd)%q
   enddo

!  exception for ps
!  ----------------
   ibnd = MAPL_SimpleBundleGetIndex ( bnd, 'DP', 3, rc=status )
   idyn = MAPL_SimpleBundleGetIndex ( dyn, 'ps', 2, rc=status )
   kdim=size(bnd%r3(ibnd)%q,3)

   dyn%r2(idyn)%q(:,:) = 0.0
   if (thisphase==2) then
      do k = 1, kdim
         dyn%r2(idyn)%q(:,:) = dyn%r2(idyn)%q(:,:) + (bk(k)-bk(k-1))*bnd%r3(ibnd)%q(:,:,k)
      enddo
   else
      do k = 1, kdim
         dyn%r2(idyn)%q(:,:) = dyn%r2(idyn)%q(:,:) + bnd%r3(ibnd)%q(:,:,k)
      enddo
   endif

   end subroutine BundleInt2Ext

 end Program GEOS5_Main

!EOC
