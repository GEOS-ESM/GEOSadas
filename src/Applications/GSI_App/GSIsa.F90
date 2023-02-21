!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!    NASA/GSFC, Global Modeling and Assimilation Office, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: GSIsa:  Stand-alone GSI driver program
!
! !INTERFACE:
!
#include "MAPL_ErrLogMain.h"

   Program GSIsa

! !USES:
 
   use ESMF  
   use MAPL
   use MPI
   use MAPL_Profiler, only: BaseProfiler, TimeProfiler, get_global_time_profiler, get_global_memory_profiler
   use GEOSaana_GridCompMod, only: SetServices   
   use gsi_chemguess_mod, only : gsi_chemguess_get
   use m_StrTemplate      
   use m_mpif90,only : MP_comm_world
   use m_mpif90,only : MP_REAL8

   use gsi_fixture, only: fixture_config

        ! Generic interface names are aliased to zeit_ to avoid some code
        ! changes below.
   use timermod, only: zeit_ci       => timer_on
   use timermod, only: zeit_co       => timer_off
   use timermod, only: zeit_flush    => timer_flush
   use timermod, only: zeit_allflush => timer_allflush

   use mpeu_util, only: perr,die
 
   implicit NONE

! !DESCRIPTION: Runs GSI in stand-alone mode from GEOS-5 background files. 
!               This gives the template for what calling GSI from within
!   a full, single-executable, application would look like.
!
! !REVISION HISTORY:
!
!  02Feb2007  Cruz/daSilva Initial code.
!  07Feb2007  Cruz         Moved file I/O from aana to here
!  04Mar2007  Todling      Various adaptations to mold to GEOS/DAS
!  04Apr2007  Cruz         Add loop to read over bkg files
!  14Apr2007  Todling      - Generalized analysis interval - works for extended 4dvar-like time window
!                          - Modified time loop to be more like the gcm; corresponding changes in rc files
!  21Apr2007  Todling      - renamed u/v/tv properly and replaced qi/ql w/ qctot
!                          - reading ak/bks from rc file
!  25Apr2007  Todling      Add lwi and delp to output file
!  13Jun2007  Todling      Add MAPL-IOwrite
!  05Jul2007  Todling      Adjustments to allow writing analysis at requested time
!  10Jul2007  Todling      Allow writing of increments instead of full analysis
!  08Jun2008  Todling      Update to latest MAPL (see remaks)
!  10Mar2009  Todling      - Back to usinging qitot and qltot; remove qctot
!                          - Remove lwi; use fractions frland/frlake/etc
!  16Feb2011  Todling      Reorganized how bkg files are read in
!  20Aug2014  Weir         Changed call to gsi_chemguess_get to olist::tracers instead of list::tracers
!  02Feb2015  Todling      Allow for number of levels larger than 99
!
! !REMARKS:
!  1. I recognize that having gsi_chemguess_get get RC-like info breaks 
!     the ESMF obj oriented approach of extracting such info from CF instead
!
!EOP
!-----------------------------------------------------------------------

   integer             :: rc
   integer :: IM_World=144, JM_World=91, LM_World=72  ! DEBUG ONLY

   type(ESMF_VM)       :: VM            ! virtual machine
   type(ESMF_DELayout) :: layout        
   type(ESMF_GridComp) :: gcAANA        ! AANA gridded component
   type(ESMF_Grid)     :: gridAANA      ! AANA grid
   type(ESMF_State)    :: expAANA       ! AANA export state (GEOS imp)
   type(ESMF_State)    :: impAANA       !  "   import  "    ( "   exp)
   type(ESMF_Alarm)    :: Alarm         ! Final End Time alarm
   type(ESMF_Clock)    :: Clock         
   type(ESMF_Time)     :: currT   
   type(ESMF_Time)     :: AnaTime   
   type(ESMF_Config)   :: cf
   type(MAPL_CFIO)     :: cfio
   type(ESMF_FieldBundle)  :: AnaBundle
   type(ESMF_TimeInterval) :: aTimeStep

   integer,    pointer :: resolution(:) ! Dummy pointers for now
   real,       pointer :: levels(:)     ! Dummy pointers for now
   integer             :: ANAfreq_hr              ! Analysis time window (hrs)
   integer             :: i, BKGfreq, n, k
   integer             :: status
   integer             :: userRC
   integer             :: yy,mm,dd,hh,mn,sec
   integer             :: comm
   character(len=ESMF_MAXSTR) :: exp_vars
   character(len=ESMF_MAXSTR) :: anafntmpl
   character(len=ESMF_MAXSTR) :: expid
   character(len=ESMF_MAXSTR) :: wrtana
   character(len=ESMF_MAXSTR) :: enableTimers
   character(len=*), parameter :: Iam = 'GSIsa'
   class (BaseProfiler), pointer :: t_p
   type(MAPL_MetaComp), pointer :: child_maplobj, maplobj
   type(ESMF_GridComp) :: temp_gc
   integer :: aana_root
   type(ESMF_GridComp), allocatable :: child_gcs(:)
   type(ESMF_State), allocatable :: child_imports(:), child_exports(:)
   type(ESMF_Config) :: temp_config

! start

!  Initialize framework
!  --------------------
#if defined(ENABLE_ESMF_ERR_LOGGING)
   call ESMF_Initialize (vm=vm, rc=status)
#else
   call ESMF_Initialize (vm=vm, logKindFlag=ESMF_LOGKIND_NONE, rc=status)
#endif
   VERIFY_(status)

   call MAPL_Initialize(rc=status)
   VERIFY_(status)
   t_p => get_global_time_profiler()
   call t_p%start('GSIsa.x')

   call ESMF_vmGet (vm, mpicommunicator=comm, rc=status)
   call MAPL_GetNodeInfo (comm=comm, rc=status)

!  Create Config and Initialize Clock 
!  ----------------------------------
   call fixture_config()

   call zeit_ci('GSIsa')
   cf = ESMF_ConfigCreate (rc=status)
   VERIFY_(STATUS)
   call ESMF_ConfigLoadFile   ( cf,'GSI_GridComp.rc',rc=status )
   VERIFY_(STATUS)

! !xRESOURCE_ITEM: string :: Control Timers
   call ESMF_ConfigGetAttribute( cf, enableTimers, label='MAPL_ENABLE_TIMERS:', &
                                 default='NO', rc=status )
   VERIFY_(status)

   if (enableTimers /= 'YES' .and. enableTimers /= 'yes') then
      call MAPL_ProfDisable( rc=STATUS )
      VERIFY_(STATUS)
   end if

   call ESMF_ConfigGetAttribute( cf, exp_vars, label ='vars_anaeta_file:', default='no', rc = status )
   VERIFY_(status)

   call init_cf_(cf,expid,anafntmpl,wrtana)

   call Clock_Init ( cf,clock,Alarm,'GSIsa','test',rc=status )
   VERIFY_(STATUS)

!  Create AANA grid (which is the same as the GEOS5 grid)
!  ------------------------------------------------------
   gridAANA = AANAGridCreate ( vm, cf, rc=status )
   VERIFY_(status)

!  States
!  ------ 
   !expAANA = ESMF_StateCreate (name="AANA_Export",      &
        !stateIntent = ESMF_STATEINTENT_EXPORT, &
        !RC=STATUS )
   !VERIFY_(STATUS)
   !impAANA = ESMF_StateCreate (name="AANA_Import",      &       
        !stateIntent = ESMF_STATEINTENT_IMPORT, & 
        !RC=STATUS ) 
   !VERIFY_(STATUS)

!  Create component
!  ---------------
    temp_config=ESMF_ConfigCreate()
    temp_gc = ESMF_GridCompCreate(name="cap_name", config=temp_config, rc=status)
    VERIFY_(status)

    child_maplobj => null()
    call MAPL_InternalStateCreate(temp_gc, child_maplobj, rc=status)
    VERIFY_(status)
   call MAPL_InternalStateRetrieve(temp_gc, child_maplobj, RC=status)
   VERIFY_(status)

   !gcAANA = ESMF_GridCompCreate ( name='AANAaana', &
        !configfile="GSI_GridComp.rc", &
        !grid=gridAANA, &
!!        gridcompType=ESMF_ATM, &
        !rc=status)

!  Make sure aana G.C. gets its grid
!  ---------------------------------
   !call ESMF_GridCompSet(gcAANA, grid=gridAANA, rc=STATUS); VERIFY_(STATUS)

   !call MAPL_InternalStateRetrieve(gcAANA, CHILD_MAPLOBJ, RC=status)
   !VERIFY_(status)
!  CHILD_MAPLOBJ%t_profiler = TimeProfiler('AANAaana', comm_world = MPI_COMM_WORLD)
!
!  Register component
!  ------------------
   !VERIFY_(status)
   !call ESMF_GridCompSetServices ( gcAANA, SetServices, rc=STATUS ); VERIFY_(STATUS)

   call MAPL_Set(CHILD_MAPLOBJ, CF=CF, RC=STATUS)
   VERIFY_(STATUS)
   aana_root = MAPL_AddChild(child_maplobj,grid=gridAANA,name="AANAaana",ss=SetServices,rc=status)
   VERIFY_(status)
   call MAPL_Get(child_maplobj,childrens_gridcomps=child_gcs, &
    childrens_import_states = child_imports, childrens_export_states = child_exports, rc = status)
   VERIFY_(status)
   impAANA=child_imports(aana_root)
   expAANA=child_exports(aana_root)
   gcAANA=child_gcs(aana_root)

!  Init component
!  --------------
   call ESMF_GridCompInitialize ( gcAANA, importState=impAANA, &
        exportState=expAANA, clock=clock, &
        userRC=userRC, RC=STATUS)
   ASSERT_(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS)

   block
   type(ESMF_Field) :: field
   integer :: rank
   call ESMF_StateGet(impAANA,'frlake',field,rc=status)
   VERIFY_(status)
   call ESMF_FieldValidate(field,rc=status)
   VERIFY_(status)
   call ESMF_FieldGet(field,dimCount=rank,rc=status)
   VERIFY_(status)
   end block

!  Advance clock
!  -------------
   call ESMF_ClockAdvance ( clock = clock, rc=status ); VERIFY_(status)

!  -------------------------
!  Start main loop over time
!  -------------------------
   do 
      !  Read/Update background
      !  ----------------------  
      call Populate_State ( cf, clock, gridAANA, impAANA, expAANA, rc=status ); VERIFY_(status)

      !   Run component
      !   -------------
      call ESMF_GridCompRun ( gcAANA, importState=impAANA, exportState=expAANA, clock=clock, userRC=userRC, RC=status )
      ASSERT_(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS)

      !   Advance clock
      !   -------------
      call ESMF_ClockAdvance ( clock = clock, rc=status ); VERIFY_(status)

      ! Check for Segment Ending Time
      ! -----------------------------
      if ( ESMF_ClockIsStopTime( clock, rc=status ) ) then
           call Populate_State ( cf, clock, gridAANA, impAANA, expAANA, rc=status ); VERIFY_(status)
           call ESMF_GridCompRun ( gcAANA, importState=impAANA, &
                exportState=expAANA, clock=clock, &
                userRC=userRC, RC=STATUS)
           ASSERT_(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS)
           exit
      endif

   end do

!  Write out analysis if desired
!  -----------------------------
   if ( scan(wrtana(1:1),"yY")/=0 ) then
       call zeit_ci('GSIsa_Write')

!      Extract bundle from state
!      -------------------------
       AnaBundle = ESMF_FieldBundleCreate ( name = 'ana', rc = STATUS ); VERIFY_(STATUS)
       call ESMF_FieldBundleSet ( AnaBundle, grid=gridAAna, rc = STATUS ); VERIFY_(STATUS)

       call ESMFL_State2Bundle ( expAANA, AnaBundle, usrfldlist=exp_vars )

!   Before writing to the file, we create a CFIO object
!   Here we define a file with reduced resolution
!   ---------------------------------------------------
       call ESMF_TimeIntervalSet( aTimeStep, h=0, m=0, s=0, rc=status ); VERIFY_(STATUS)
       call ESMF_ClockSet       (  clock, CurrTime=AnaTime, rc=status ); VERIFY_(STATUS)
       call MAPL_CFIOCreate     ( cfio, anafntmpl, clock, AnaBundle,  &
                                  DESCR='Write Analysis State', &
                                  EXPID=expid,             rc=status ); VERIFY_(status)

       call MAPL_CFIOWrite ( cfio, clock, AnaBundle, verbose = .true., &
                             rc=status ); VERIFY_(STATUS)

       call MAPL_cfioDestroy ( cfio )
       call zeit_co('GSIsa_Write')

   endif ! < wrtana >

!  Finalize component
!  ------------------
   call ESMF_GridCompFinalize ( gcAANA, importState=impAANA, &
                exportState=expAANA, clock=clock, &
                userRC=userRC, RC=STATUS)

   if( userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS ) then
      if (MAPL_AM_I_ROOT()) then
         close(999)
         open (999,file='GSI_EGRESS',form='formatted')
         close(999)
      end if
   endif

  !Destroy Objects
  !---------------
  !call ESMF_GridCompDestroy ( gcAANA,  rc=status ); VERIFY_(STATUS)
  !call ESMF_StateDestroy    ( impAANA, rc=status ); VERIFY_(STATUS)
  !call ESMF_StateDestroy    ( expAANA, rc=status ); VERIFY_(STATUS)
  !call ESMF_ClockDestroy    ( clock,   rc=status ); VERIFY_(STATUS)

!  Show time
!  ---------
   call zeit_co('GSIsa')
   if(MAPL_AM_I_ROOT()) call zeit_flush(6)

!  call ESMF_VMGet(vm,mpiCommunicator=comm,rc=status); VERIFY_(status)
   call zeit_allflush(6,comm=MP_comm_world,root=0)

!   All done
!   -------- 
   call t_p%stop('GSIsa.x')
   call MAPL_Finalize()
   call ESMF_Finalize()

#include "MAPL_ErrLog.h"

   contains

!-------------------------------------------------------------------
   subroutine init_cf_ (cf,expid,anafntmpl,wrtana)
!-------------------------------------------------------------------
   type(ESMF_Config), intent(inout):: cf
   character(len=ESMF_MAXSTR),intent(inout) :: expid
   character(len=ESMF_MAXSTR),intent(inout) :: anafntmpl
   character(len=ESMF_MAXSTR),intent(inout) :: wrtana

   character(len=ESMF_MAXSTR) :: wrtinc

   call ESMF_ConfigGetAttribute( cf, expid, label ='expid:', rc = status )
   VERIFY_(status)

   call ESMF_ConfigGetAttribute( cf, wrtinc, label ='ANALYSIS_INCREMENT:', rc = status )
   VERIFY_(status)
   wrtinc = adjustl(wrtinc)

   if ( scan(wrtinc(1:1),"Yy")/=0  ) then
       call ESMF_ConfigGetAttribute( cf, anafntmpl, label ='upper-air_inc_filename:', rc = status )
       VERIFY_(status)
   else
       call ESMF_ConfigGetAttribute( cf, anafntmpl, label ='upper-air_ana_filename:', rc = status )
       VERIFY_(status)
   endif

   call ESMF_ConfigGetAttribute( cf, wrtana, label ='RECORD_ANALYSIS:', rc = status )
   VERIFY_(status)
   wrtana = adjustl(wrtana)

   return
   end subroutine init_cf_
!-------------------------------------------------------------------
   subroutine populate_state(cf, clock, gridAANA, impAANA, expAANA, rc)
!-------------------------------------------------------------------
   type(ESMF_Config), intent(inout):: cf
   type(ESMF_Clock),  intent(inout):: clock
   type(ESMF_Grid)  , intent(inout):: gridAANA
   type(ESMF_State)  :: impAANA
   type(ESMF_State)  :: expAANA
   integer, optional, intent(out)  :: rc

   ! locals   
   character(len=ESMF_MAXSTR) :: bupafname,bsfcfname
   integer, parameter         :: nxfiles=2  ! number of additional bkg files (other than bkg.eta & bkg.sfc
   character(len=ESMF_MAXSTR) :: xfnames(nxfiles)
   type(ESMF_FieldBundle)     :: AnaBundleIn
   type(ESMF_FieldBundle)     :: AnaBundleOut
   type(ESMF_FieldBundle),allocatable :: XBundles(:)
   type(ESMF_Time)            :: CurrTime   
   character(len=*), parameter:: Iam = 'populate_state'

   integer:: status, ntgases, naero, nbuns, ib, ic
   character(len=ESMF_MAXSTR):: tgaslist
   character(len=ESMF_MAXSTR):: exp_vars
   type(ESMF_VM) :: vm


   block
   type(ESMF_Field) :: field
   integer :: rank
   call ESMF_StateGet(impAANA,'frlake',field,rc=status)
   VERIFY_(status)
   call ESMF_FieldValidate(field,rc=status)
   VERIFY_(status)
   call ESMF_FieldGet(field,dimCount=rank,rc=status)
   VERIFY_(status)
   end block

   if(present(rc)) rc=0

   call zeit_ci ('GSIsa_populate')

   call ESMF_ConfigGetAttribute( cf, exp_vars, label ='vars_anaeta_file:', default='no', rc = status )
   VERIFY_(status)
   call ESMF_ClockGet (clock, currTime=currTime, rc=status)
   VERIFY_(STATUS)

!  Figure out indispensible input files
!  ------------------------------------
   call create_fname(cf,currTime,'upper-air_bkg_filename:',bupafname,rc=status)
     VERIFY_(STATUS)
   call create_fname(cf,currTime,'surface_bkg_filename:'  ,bsfcfname,rc=status)
     VERIFY_(STATUS)

   AnaBundleIn = ESMF_FieldBundleCreate ( name='AANA bundle', rc=status )
   VERIFY_(status)
   call ESMF_FieldBundleSet ( AnaBundleIn, grid=gridAANA, rc=status )
   VERIFY_(status)

   call read_fnames(cf,bupafname, bsfcfname, currTime, gridAANA, AnaBundleIn)

!  Figure out complementary input files
!  ------------------------------------
   call create_fname(cf,currTime,'chem_bkg_filename:',xfnames(1))! ignore rc
   call create_fname(cf,currTime,'aero_bkg_filename:',xfnames(2))! ignore rc

   nbuns=0
   call gsi_chemguess_get('dim',ntgases,status)
   if (ntgases>0) then
       do ib=1,nxfiles
          if(trim(xfnames(ib))/='NONE') nbuns=nbuns+1
       enddo
       allocate(XBundles(nbuns))
       ic=0
       do ib=1,nxfiles
          tgaslist=''
          if(ib==1) then  ! trace gases only
             call gsi_chemguess_get('olist::tracers',tgaslist,status)
          endif
          if(ib==2) then  ! aerosols only
             call gsi_chemguess_get('olist::aerosols',tgaslist,status)
          endif
          if(status==0.and.trim(xfnames(ib))/='NONE')then
             ic=ic+1
             XBundles(ic) = ESMF_FieldBundleCreate ( name='X bundle', rc=status )
             VERIFY_(status)
             call ESMF_FieldBundleSet ( XBundles(ic), grid=gridAANA, rc=status )
             VERIFY_(status)
             call get_file2bundle(xfnames(ib), tgaslist, currTime, gridAANA, XBundles(ic))
             call ESMFL_Add2Bundle(XBundles(ic),AnaBundleIn)
          endif
       enddo
   endif

   call ESMFL_Bundle2State (AnaBundleIn, impAANA,rc=status)

   do ib=1,nbuns
       call ESMF_FieldBundleDestroy ( XBundles(ib), rc=STATUS )
       VERIFY_(STATUS)
   enddo
   call ESMF_FieldBundleDestroy ( AnaBundleIn, rc=STATUS )
   VERIFY_(STATUS)

   call zeit_co ('GSIsa_populate')

   RETURN_(ESMF_SUCCESS)

   end subroutine populate_state

!-------------------------------------------------------------------
   subroutine read_fnames(cf,bupafname, bsfcfname, currT, &
                          gridAANA, AnaBundleIn, & !expAANA,&
                          rc)
!-------------------------------------------------------------------
! !REVISION HISTORY:
!
!  22Apr2007 Todling Using proper names for u/v/tv fields; use qctot
!  25Apr2007 Todling Added ts and lwi to export; filespec compliance
!  27Feb2009 Todling - Convert bundle to fieldbundle
!                    - currT intent inout
!  10Mar2009 Todling Back to qi/qltot instead of qctot
!  21Apr2010 Todling Add (preliminary) chem tracer capability
!  30May2012 Todling Merge in NSST changes
!  28Oct2013 Todling Pass vars to get from input through rc file
!
!-------------------------------------------------------------------
   type(ESMF_Config), intent(inout)  :: cf
   type(ESMF_Time)  , intent(inout)  :: currT   
   type(ESMF_Grid)  , intent(inout)  :: gridAANA
   type(ESMF_FieldBundle), intent(inout) :: AnaBundleIn   ! bundle to hold GEOS import UPA fields
!   type(ESMF_State)  , intent(inout) :: expAANA
   integer, optional, intent(out)  :: rc
   character(len=ESMF_MAXSTR), intent(in) :: bupafname, bsfcfname
!
   type(ESMF_FieldBundle)   :: GEOSdynBUN1   ! bundle to hold GEOS import UPA fields
   type(ESMF_FieldBundle)   :: GEOSsfcBUN1   !  "          "              SFC fields
   type(ESMF_FieldBundle)   :: GEOSdynBUN2   !  "          "       export UPA   fields
   integer :: status
   character(len=*), parameter :: Iam = 'read_fnames'

   integer:: ntgases
   character(len=ESMF_MAXSTR):: tgaslist
   character(len=ESMF_MAXSTR):: exp_vars,sfc_vars

! start

   call ESMF_ConfigGetAttribute( cf, exp_vars, label ='vars_bkgeta_file:', default='no', rc = status )
   VERIFY_(status)
   call ESMF_ConfigGetAttribute( cf, sfc_vars, label ='vars_bkgsfc_file:', default='no', rc = status )
   VERIFY_(status)

!  Create empty bundles to hold GEOS fields
!  Note: GEOS export = AANA import = GSI import
!        GEOS import = AANA export = GSI export
!  --------------------------------------------
   GEOSdynBUN1 = ESMF_FieldBundleCreate ( name='AANA imp dyn bundle', rc=status )
   VERIFY_(status)
   call ESMF_FieldBundleSet ( GEOSdynBUN1, grid=gridAANA, rc=status )
   VERIFY_(status)
   GEOSdynBUN2 = ESMF_FieldBundleCreate ( name='AANA exp dyn bundle', rc=status )
   VERIFY_(status)
   call ESMF_FieldBundleSet ( GEOSdynBUN2, grid=gridAANA, rc=status )
   VERIFY_(status)
   GEOSsfcBUN1 = ESMF_FieldBundleCreate ( name='AANA imp sfc bundle', rc=status )
   VERIFY_(status)
   call ESMF_FieldBundleSet ( GEOSsfcBUN1, grid=gridAANA, rc=status )
   VERIFY_(status)

   !  Read impAANA  from files
   !  ------------------------
   call MAPL_CFIORead  ( bupafname, currT, GEOSdynBUN1, &
!       only_vars='phis,ps,ts,frland,frlandice,frlake,frocean,frseaice,u,v,tv,sphu,ozone,qitot,qltot', &
        only_vars=trim(exp_vars),&
        TIME_IS_CYCLIC=.false., verbose=.true.,&
        rc=status )
   VERIFY_(status)
   if ( trim(bsfcfname) /= 'NONE' ) then
      call MAPL_CFIORead  ( bsfcfname, currT, GEOSsfcBUN1, &
           only_vars=trim(sfc_vars),&
           TIME_IS_CYCLIC=.false., verbose=.true.,&
           rc=status )
      VERIFY_(status)
   endif
   
   !   Extract fields from bundles and use them to populate states
   !   -----------------------------------------------------------
   call ESMFL_Bundles2Bundle (GEOSdynBUN1, GEOSsfcBUN1, AnaBundleIn)

   call ESMF_FieldBundleDestroy ( GEOSsfcBUN1, rc=STATUS )
   VERIFY_(STATUS)
   call ESMF_FieldBundleDestroy ( GEOSdynBUN1, rc=STATUS )
   VERIFY_(STATUS)

   RETURN_(ESMF_SUCCESS)

   end subroutine read_fnames

   subroutine get_file2bundle(fname, vars, currT, &
                        gridAANA, AnaBundle,&
                        rc)
!-------------------------------------------------------------------
! !REVISION HISTORY:
!
!  12Feb2011 Todling Trying to get the slate clean
!
!-------------------------------------------------------------------
   type(ESMF_Time)  , intent(inout)  :: currT   
   type(ESMF_Grid)  , intent(inout)  :: gridAANA
   type(ESMF_FieldBundle),intent(inout) :: AnaBundle
!  type(ESMF_FieldBundle),intent(inout) :: AnaBundleX
   integer, optional, intent(out)  :: rc
   character(len=*),  intent(in)   :: vars
   character(len=ESMF_MAXSTR), intent(in) :: fname
!
   type(ESMF_FieldBundle)   :: GEOSbundle0
   type(ESMF_FieldBundle)   :: GEOSbundle1
   integer :: status
   character(len=*), parameter :: Iam = 'get_file2bundle'

! start

!  Create empty bundles to hold GEOS fields
!  ----------------------------------------
   GEOSbundle0 = ESMF_FieldBundleCreate ( name='AANA bundle holder', rc=status )
   VERIFY_(status)
   call ESMF_FieldBundleSet ( GEOSBundle0, grid=gridAAna, rc = STATUS ); VERIFY_(STATUS)
   VERIFY_(status)
   GEOSbundle1 = ESMF_FieldBundleCreate ( name='AANA bundle holder', rc=status )
   VERIFY_(status)
   call ESMF_FieldBundleSet ( GEOSBundle1, grid=gridAAna, rc = STATUS ); VERIFY_(STATUS)
   VERIFY_(status)

   !  Read impAANA  from files
   !  ------------------------
   if ( trim(fname)/='NONE' ) then
      call MAPL_CFIORead  ( fname, currT, AnaBundle, only_vars=trim(vars), &
                            TIME_IS_CYCLIC=.false., verbose=.true., rc=status )
      VERIFY_(status)
   endif
   
   !   Extract fields from bundles and use them to populate states
   !   -----------------------------------------------------------
!  call ESMFL_Bundles2Bundle (AnaBundleIn, GEOSbundle1, AnaBundleOut)  ! does this really work?

   call ESMF_FieldBundleDestroy ( GEOSbundle1, rc=STATUS )
   VERIFY_(STATUS)
   call ESMF_FieldBundleDestroy ( GEOSbundle0, rc=STATUS )
   VERIFY_(STATUS)

   RETURN_(ESMF_SUCCESS)

   end subroutine get_file2bundle

!-------------------------------------------------------------------
   subroutine create_fname (cf,currT,namestrg,fname,rc)
!-------------------------------------------------------------------
!  12Feb2010  Todling - process only a single file template
!
   type(ESMF_Config), intent(inout)  :: cf
   type(ESMF_Time),   intent(inout)  :: currT
   character(len=*),  intent(in)     :: namestrg ! name string to look for in RC
   character(len=ESMF_MAXSTR), intent(out) :: fname    ! fully resolved filename
   integer, optional, intent(out) :: rc
!
   character(len=ESMF_MAXSTR) :: bkgpath
   character(len=ESMF_MAXSTR) :: DateStamp
   character(len=ESMF_MAXSTR) :: expid
   character(len=ESMF_MAXSTR) :: tmpl
   integer :: nymd, nhms, yy, mm, dd, hh, mn, sc
   integer :: status
   character(len=*), parameter :: Iam = 'create_fname'  

   fname='NONE'
   if(present(rc)) rc=0
 
   call ESMF_ConfigGetAttribute( cf, expid, label ='expid:', rc = status )
   VERIFY_(status)
   call ESMF_ConfigGetAttribute( cf, tmpl, label =trim(namestrg), rc = status )
   if(status/=0) then  ! let calling prog decide what to do
      rc=1
      return
   endif

   call ESMF_TimeGet(currT, yy=YY, mm=MM, dd=DD, h=HH, m=MN, s=SC, rc=rc) 
   if(MAPL_AM_I_ROOT()) write(6,'(a,1x,i4.4,4(a,i2.2))') trim(Iam)//" The current TIME is ", &
                                       YY, "/", MM, "/", DD, " ", HH, ":", MN

   nymd = yy*10000 + mm*100 + dd
   nhms = hh*10000 + mn*100 + sc
   write(DateStamp,'(i8.8,a,i6.6,a)') nymd, '_', nhms, 'z'

   call StrTemplate ( fname, tmpl, 'GRADS', xid=expid, nymd=nymd, nhms=nhms, &
        stat=status )
   VERIFY_(STATUS)
   if ( MAPL_AM_I_ROOT() ) print *, trim(namestrg), ' ',trim(fname)

   end subroutine create_fname

! Adopted from GEOSgcm.F90:

!-------------------------------------------------------------------
   subroutine Clock_Init ( config,clock,Final_End_Time_Alarm, &
                           expid,expdsc,rc )
!-------------------------------------------------------------------

   type(ESMF_Config)                  :: config
   type(ESMF_Clock),    intent(  out) :: clock
   type(ESMF_Alarm)                   :: Final_End_Time_Alarm
   character(len=*)                   :: expid
   character(len=*)                   :: expdsc
   integer, optional,   intent(  out) :: rc

   type(ESMF_Time)          :: StartTime    ! Initial     Begin  Time of Experiment
   type(ESMF_Time)          ::  StopTime    ! Final       Ending Time of Experiment
   type(ESMF_Time)          ::  CurrTime    ! Current            Time of Experiment
   type(ESMF_Time)          ::   SegTime    ! Job Segment Ending Time of Experiment
   type(ESMF_Time)          ::      Time    ! Current (temp only!)
   type(ESMF_TimeInterval)  ::  timeStep    ! Model TimeStep
   type(ESMF_Calendar),save ::  cal
   
   integer        :: nymd,nhms  ! Current Restart Time
   integer        :: sec,status

   integer        :: UNIT
   integer        :: START_TIME(6), BEG_DATE(2)
   integer        ::  STOP_TIME(6), END_DATE(2)
   integer        ::  SGMT_TIME(6), JOB_SGMT(2)
   integer        ::  CURR_TIME(6)
   integer        ::   REF_TIME(6), RREF_DATE, RREF_TIME

   integer        :: ANAfreq
   integer        :: BKGfreq_hr
   integer        :: BKGfreq_mn
   integer        :: BKGfreq_sc

   type(ESMF_Time)               :: RingTime
   type(ESMF_TimeInterval)       :: Frequency


! Read Clock Resource File
! ------------------------
   call ESMF_ConfigGetAttribute( config, BEG_DATE, count=2, label='BEG_DATE:', rc=status )
   VERIFY_(STATUS)
   call ESMF_ConfigGetAttribute( config, END_DATE, count=2, label='END_DATE:', rc=status )
   VERIFY_(STATUS)
   call ESMF_ConfigGetAttribute( config, JOB_SGMT, count=2, label='JOB_SGMT:', rc=status )
   VERIFY_(STATUS)
   CALL ESMF_ConfigGetAttribute( config, RREF_DATE, label = 'RECORD_REF_DATE:', rc=status )
   VERIFY_(status)
   CALL ESMF_ConfigGetAttribute( config, RREF_TIME, label = 'RECORD_REF_TIME:', rc=status )
   VERIFY_(status)
   CALL ESMF_ConfigGetAttribute( config, BKGfreq, label = 'BKG_FREQUENCY:', rc=status )
   VERIFY_(status)
   CALL ESMF_ConfigGetAttribute( config, ANAfreq, label = 'ANA_FREQUENCY:', rc=status )
   VERIFY_(status)

   START_TIME(1) =     BEG_DATE(1)/10000
   START_TIME(2) = mod(BEG_DATE(1),10000)/100
   START_TIME(3) = mod(BEG_DATE(1),100)
   START_TIME(4) =     BEG_DATE(2)/10000
   START_TIME(5) = mod(BEG_DATE(2),10000)/100
   START_TIME(6) = mod(BEG_DATE(2),100)

   STOP_TIME(1) =     END_DATE(1)/10000
   STOP_TIME(2) = mod(END_DATE(1),10000)/100
   STOP_TIME(3) = mod(END_DATE(1),100)
   STOP_TIME(4) =     END_DATE(2)/10000
   STOP_TIME(5) = mod(END_DATE(2),10000)/100
   STOP_TIME(6) = mod(END_DATE(2),100)

   REF_TIME(1) =     RREF_DATE/10000
   REF_TIME(2) = mod(RREF_DATE,10000)/100
   REF_TIME(3) = mod(RREF_DATE,100)
   REF_TIME(4) =     RREF_TIME/10000
   REF_TIME(5) = mod(RREF_TIME,10000)/100
   REF_TIME(6) = mod(RREF_TIME,100)


!  Determine analysis frequency
!  -----------------------------
   ANAfreq_hr = ANAfreq/10000

!  For now, bkg cannot be more frequent than 1mn; see ESMF_TimeIntervalSet below
!  -----------------------------------------------------------------------------
   BKGfreq_hr = BKGfreq/10000
   BKGfreq_mn = mod(BKGfreq,10000)/100
   BKGfreq_sc = mod(BKGfreq,100)
   BKGfreq_sc = BKGfreq_hr*3600 + BKGFreq_mn*60 + BKGfreq_sc

   if( MAPL_AM_I_ROOT() ) then
      print *
      print *, 'ExpID: ',trim(expid)
      print *, 'Descr: ',trim(expdsc)
      print *
      write(*,'(a,i8.8,1x,i6.6)') 'Initial Start_Time = ', beg_date
      write(*,'(a,i8.8,1x,i6.6)') '  Final  Stop_Time = ', end_date
      write(*,'(a,i8.8,1x,i6.6)') '  Current Job_Sgmt = ', job_sgmt
      write(*,'(a,i6.6)')         '   BKGfreq(HHMMSS) = ', BKGfreq
      print *
   endif

   nymd =  START_TIME(1)*10000 &
        +  START_TIME(2)*100   &
        +  START_TIME(3)
   nhms =  START_TIME(4)*10000 &
        +  START_TIME(5)*100   &
        +  START_TIME(6)
   if( MAPL_AM_I_ROOT() ) then
      print *, 'Created date from resource file, Start Date = ',nymd
      print *, '                                 Start Time = ',nhms
      print *
   endif

! Set CURR_TIME based on Current Restart Time
! -------------------------------------------
   CURR_TIME(1) =     nymd/10000
   CURR_TIME(2) = mod(nymd,10000)/100
   CURR_TIME(3) = mod(nymd,100)
   CURR_TIME(4) =     nhms/10000
   CURR_TIME(5) = mod(nhms,10000)/100
   CURR_TIME(6) = mod(nhms,100)
   
! initialize calendar to be Gregorian type
! ----------------------------------------
   call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, RC=STATUS)
   VERIFY_(STATUS)

   cal = ESMF_CalendarCreate( ESMF_CALKIND_GREGORIAN, name="GregorianCalendar", rc=status )
   VERIFY_(STATUS)

! initialize start time for Alarm frequencies
! -------------------------------------------
   call ESMF_TimeSet( StartTime, YY = START_TIME(1), &
                                 MM = START_TIME(2), &
                                 DD = START_TIME(3), &
                                 H  = START_TIME(4), &
                                 M  = START_TIME(5), &
                                 S  = START_TIME(6), calendar=cal, rc=status )
   VERIFY_(STATUS)

! initialize final stop time
! --------------------------
   call ESMF_TimeSet(  StopTime, YY =  STOP_TIME(1), &
                                 MM =  STOP_TIME(2), &
                                 DD =  STOP_TIME(3), &
                                 H  =  STOP_TIME(4), &
                                 M  =  STOP_TIME(5), &
                                 S  =  STOP_TIME(6), calendar=cal, rc=status )
   VERIFY_(STATUS)

! set current time
! ----------------
   call ESMF_TimeSet(  CurrTime, YY =  CURR_TIME(1), &
                                 MM =  CURR_TIME(2), &
                                 DD =  CURR_TIME(3), &
                                 H  =  CURR_TIME(4), &
                                 M  =  CURR_TIME(5), &
                                 S  =  CURR_TIME(6), calendar=cal, rc=status )
   VERIFY_(STATUS)
   
! set output time
! ---------------
   call ESMF_TimeSet(  AnaTime, YY =  REF_TIME(1), &
                                MM =  REF_TIME(2), &
                                DD =  REF_TIME(3), &
                                H  =  REF_TIME(4), &
                                M  =  REF_TIME(5), &
                                S  =  REF_TIME(6), calendar=cal, rc=status )
   VERIFY_(STATUS)
   
! initialize model time step
! --------------------------
   call ESMF_TimeIntervalSet( TimeStep, s=BKGfreq_sc, rc=status ) 
   VERIFY_(STATUS)

! Back-off StartTime and CurrTime by One TimeStep. 
! After Initialize we'll advance the clock one timestep
! to set the proper ringing state of all alarms 
! >>  This mess here is simply to mimic the way 
! >>  the GCM deals w/ the clock

   StartTime = StartTime - TimeStep
    CurrTime =  CurrTime - TimeStep

! initialize the clock with the above values
! ------------------------------------------
   clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
        startTime=StartTime, stopTime=StopTime, rc=STATUS )
   VERIFY_(STATUS)

   call ESMF_ClockSet ( clock,CurrTime=CurrTime,rc=status )
   VERIFY_(STATUS)

   Final_End_Time_Alarm = ESMF_AlarmCreate ( clock=clock, RingTime=StopTime, rc=status )
   VERIFY_(STATUS)

   RETURN_(ESMF_SUCCESS)

   end subroutine Clock_Init

!-------------------------------------------------------------------
   function AANAGridCreate ( vm, cf, rc) result(grid)
!-------------------------------------------------------------------

    type (ESMF_VM),    intent(INOUT) :: VM
    type(ESMF_Config), intent(INOUT) :: cf
    integer, optional, intent(OUT)   :: rc
    type (ESMF_Grid)                 :: grid

! Local vars
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='AANAGridCreate'

    type (ESMF_DELayout)                 :: layout
    integer                         :: IM,JM,LM
    integer                         :: L
    integer                         :: NX, NY
    integer, allocatable            :: IMXY(:), JMXY(:)
    character(len=ESMF_MAXSTR)      :: gridname
    real(ESMF_KIND_R8)              :: minCoord(3)
    real(ESMF_KIND_R8)              :: deltaX, deltaY, deltaZ
    type(LatLonGridFactory)         :: ll_factory

! grid create

   call ESMF_ConfigGetAttribute( cf, im, label ='GEOS IM:', rc = status )
   VERIFY_(status)
   call ESMF_ConfigGetAttribute( cf, jm, label ='GEOS JM:', rc = status )
   VERIFY_(status)
   call ESMF_ConfigGetAttribute( cf, LM, label ='GEOS KM:', rc = status )
   VERIFY_(status)
   call ESMF_ConfigGetAttribute( cf, NX, label ='NX:',      rc = status )
   VERIFY_(status)
   call ESMF_ConfigGetAttribute( cf, NY, label ='NY:',      rc = status )
   VERIFY_(status)

    ll_factory = LatLonGridFactory(grid_name=trim('AANA Regular Grid'), nx=nx, ny=ny, &
                                  IM_World=im, jm_world=jm, lm=lm, pole='PC',dateline='DC', rc=status)
    VERIFY_(status)
    grid = grid_manager%make_grid(ll_factory,rc=status)
    VERIFY_(status)
    
    call GetVertParms_ (grid,cf,LM,rc=STATUS)
    VERIFY_(STATUS)

    RETURN_(STATUS)

   end function AANAGridCreate

!-------------------------------------------------------------------------
   subroutine GetVertParms_(grid,cf,nsig,rc)
   use m_set_eta, only: set_eta
!-------------------------------------------------------------------------
!
! !REVISION HISTORY:
!
!  20Apr2007 Todling  Transplanted here from GSIGridComp
!
! !REMARKS: This information should be extracted from the bkg files
!           directly and not be read from the GSIGridComp rc. GSI
!           has it's own grid and this is assuming bkg and gsi have
!           the same grid which is not necessary always the case.
!    
! !CAUTION: Levels in RC file are reversed, therefore we read them
!           here in reversed order to be consistent w/ bkg
!
! !TO DO: There needs to be an auto general grid setting when 
!         reading an hdf file
!
!-------------------------------------------------------------------------
   type (ESMF_Grid),  intent(INOUT)   :: grid
   type(ESMF_Config), intent(INOUT)   :: cf
   integer,             intent(  IN ) :: nsig
   integer, optional,   intent(  OUT) :: rc     ! Error code:
   integer                            :: k,ks
   real(8)                            :: ptop,pint
   real(8),allocatable,  dimension(:) :: ak (:),bk (:)
   real, allocatable,  dimension(:)   :: ak5(:),bk5(:)
   character(len=16)                  :: vgridlabl
   character(len=3)                   :: cnsig
   character(len=ESMF_MAXSTR), parameter :: IAm='GetVertParms'
   real,    parameter :: Pa_per_kPa=1000.    ! convert kPa to Pa

! Create the label to be searched for in the RC file based on nsig

   allocate ( ak5(nsig+1), bk5(nsig+1), stat=STATUS )
   allocate ( ak (nsig+1), bk (nsig+1), stat=STATUS )
   if(MAPL_AM_I_ROOT()) then
      call set_eta ( nsig,ks,ptop,pint,ak,bk )
   endif
   call mpi_bcast(ak, nsig+1,MP_REAL8,MAPL_root,comm,status)
   call mpi_bcast(bk, nsig+1,MP_REAL8,MAPL_root,comm,status)
   do i=1,nsig+1 ! levs were being reversed twice: once here, once in the RC file
                 ! so at this point levs are GEOS compliant - no need to reverse
      k=i 
      ak5(k)=ak(i)
      bk5(k)=bk(i)
   enddo
   deallocate(ak,bk)

   if(MAPL_AM_I_ROOT()) then
      print *,'Background Vertical Grid - lev, ak, bk - '
      do i=1,nsig+1
         write(*,'(1x,i3,2f16.6)') i,ak5(i),bk5(i)
      end do
   end if

   call ESMF_AttributeSet(grid, 'ak', valuelist=ak5, rc=STATUS)
   VERIFY_(STATUS)
   call ESMF_AttributeSet(grid, 'bk', valuelist=bk5, rc=STATUS)
   VERIFY_(STATUS)
!  call ESMF_AttributeSet(grid, 'ck', valuelist=ck5, rc=STATUS)
!  VERIFY_(STATUS)

   call ESMF_AttributeSet(grid, "FLIP_LONS", .true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMF_AttributeSet(grid, "FLIP_POLES", .false., rc=STATUS)
   VERIFY_(STATUS)

   deallocate ( ak5, bk5, stat=STATUS )
   VERIFY_(STATUS)

   end subroutine GetVertParms_

   end program GSIsa
