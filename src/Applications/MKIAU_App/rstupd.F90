! rstupd.x - ESMF/MAPL application to update model restarts with analysis
!            increment
!
! 1. Read increment file from analysis (inc.eta)
! 2. Defined IAU state at user's desired resolution
! 3. Invokes mkiauGridCompMod to read ANA file and create IAU increment
!    and the resolution of the background
! 4. If needed regrids IAU increment to desired output resolution
! 5. Let IAU_GridComp write out file equivalent to agcm_internal_rst
!
! REMARKS: 
!   a) This program requires an RC file: rstupd.rc
!
! Ricardo Todling, July 2012
!............................................................................
!  !REVISION_HISTORY:
!----------------------------------------------------------------------------

#  include "MAPL_Generic.h"

   Program rstUPD

   use ESMF
   use MAPL_Mod
   use GEOS_mkiauGridCompMod,  only: mkiauSetServices => SetServices
   use GEOS_MoistGridCompMod,   only: MoistSetServices  => SetServices
   use GEOS_PChemGridCompMod,   only: PChemSetServices  => SetServices
   use DynVec_GridCompMod,      only: DynVecSetServices => SetServices
   use IAU_GridCompMod,         only: AINCSetServices => SetServices

   use m_StrTemplate, only: StrTemplate

   implicit NONE

!  Basic ESMF objects being used in this example
!  ---------------------------------------------
   type(ESMF_Grid)         :: GCMgrid   ! GCM Grid
   type(ESMF_Grid)         :: INCgrid   ! INC Grid
   type(ESMF_Grid)         :: ANAgrid   ! ANA Grid
   type(ESMF_FieldBundle)  :: IncBundle ! Bundle to hold analysis increment

   type(ESMF_VM)           :: vm       ! ESMF Virtual Machine
   type(ESMF_Time)         :: Time     ! Time objects
   type(ESMF_TimeInterval) :: TimeStep ! used to define a clock
   type(ESMF_Config)       :: CF       ! configuration settings


!  Grid Component Objects
!  ----------------------
   integer :: AINC, BASE, ADYN, MOIST, PCHEM
   type(ESMF_GridComp),pointer :: GCS(:)
   type(ESMF_State)   ,pointer :: IMPORTS(:)
   type(ESMF_State)   ,pointer :: EXPORTS(:)
   type(ESMF_State)   ,pointer :: INTERNAL
   type(ESMF_Clock)    :: CLOCK
   type(MAPL_MetaComp) :: MAPLOBJ

!  Basic information about the parallel environment
!         PET = Persistent Execution Threads
!  In the current implementation, a PET is equivalent 
!  to an MPI process
!  ------------------------------------------------
   integer, parameter :: r8 = 4

   integer :: myPET   ! The local PET number
   integer :: nPET    ! The total number of PETs you are running on

   integer :: status, rc
   integer :: i, j, n, im, jm, ii, iqvupd, io3upd
   integer :: nymd,nhms

   integer :: Nx, Ny                   ! Layout
   integer :: im_inc, jm_inc, lm_inc   ! Inc Grid dimensions
   integer :: im_bkg, jm_bkg, lm_bkg   ! Bkg Grid dimensions
   integer :: im_dyn, jm_dyn, lm_dyn   ! IAU Grid dimensions (revisit for cubed)

   integer :: comm
   logical :: SDFoutput,OIFoutput
   logical :: sameres,cubed
   real    :: sclinc
   real, pointer :: ak(:), bk(:)
   real, pointer, dimension(:,:,:) :: pple_bkg
   real, pointer, dimension(:,:,:) :: thv_bkg, tv_bkg, ple_bkg
   real, pointer, dimension(:,:,:) ::  dudt, sdudt
   real, pointer, dimension(:,:,:) ::  dvdt, sdvdt
   real, pointer, dimension(:,:,:) ::  dtdt, sdtdt
   real, pointer, dimension(:,:,:) :: dpedt, sdpedt
   real, pointer, dimension(:,:,:) :: dqvdt, sdqvdt
   real, pointer, dimension(:,:,:) :: do3dt, sdo3dt
   real, pointer, dimension(:,:)   :: dtsdt, sdtsdt

   real, pointer, dimension(:,:,:) :: dyn_u
   real, pointer, dimension(:,:,:) :: dyn_v
   real, pointer, dimension(:,:,:) :: dyn_pt
!  real, pointer, dimension(:,:,:) :: dyn_qv
   real, pointer, dimension(:,:,:) :: dyn_pe
   real, pointer, dimension(:,:,:) :: dyn_pkz
   real, pointer, dimension(:,:,:) :: dyn_dz
   real, pointer, dimension(:,:,:) :: dyn_w

   real, pointer, dimension(:,:,:) ::   qvbkg
   real, pointer, dimension(:,:,:) :: qtotbkg

   real, pointer, dimension(:,:,:) :: ana_pe

   character(len=ESMF_MAXSTR) :: incfname
   character(len=ESMF_MAXSTR) :: sdf_ofname

   logical,save:: c2l_fwtest = .false.
   logical,save:: c2l_adtest = .false.
   logical,save:: l2c_adtest = .false.

   logical,save:: proper_winds = .true.

!  Coordinate variables
!  --------------------
   character(len=ESMF_MAXSTR)    :: name

   character(len=*), parameter :: Iam = 'rstUPD'
   character(len=*), parameter :: myRC= 'rstupd.rc'

!                             -----
    
    call Main()

CONTAINS

    subroutine Main()

    character(len=30) ABKGGRIDNAME

!   Initialize the ESMF. For performance reasons, it is important
!    to turn OFF ESMF's automatic logging feature
!   -------------------------------------------------------------
    call ESMF_Initialize (logKindFlag=ESMF_LOGKIND_NONE, vm=vm, __RC__)

    call init_ ( CF, nymd, nhms, __RC__ )

!   Check the number of processors
!   ------------------------------
    call ESMF_VMGet(vm, localPET=myPET, PETcount=nPET)  
    if ( nPET /= Nx * Ny ) then
       if ( MAPL_am_I_root() ) then
          print *, 'Error: expecting ', Nx*Ny, ' PETs but found ', nPET, 'PETs'
          print *, 'Try:  mpirun -np ', Nx*Ny, ' ', trim(Iam)
       end if
       ASSERT_(.FALSE.)
    end if

    if ( MAPL_am_I_root() ) then
         print *
         print *, 'Starting ' // Iam // ' with ', nPET, ' PETs ...'
         print *
    end if
    call ESMF_VMGetCurrent(vm=vm, rc=status)
    call ESMF_VMGet(vm,mpiCommunicator=comm,rc=status)

!   Create either a regular Lat/Lon grid or cubed grid over which IAU defined
!   -------------------------------------------------------------------------
    if (cubed) then
       ! check for pert-get-weights
       if (Ny/=6*Nx) then
          if ( MAPL_am_I_root() ) then
             print *, 'Error: expecting Ny=6*Nx, since this uses old get-weights'
             print *, 'Error: aborting ...'
          end if
       end if
    else
       call MAPL_DefGridName (IM_DYN,JM_DYN,ABKGGRIDNAME,MAPL_am_I_root())
       GCMgrid = grid_manager%make_grid(LatLonGridFactory(grid_name=trim(ABKGGRIDNAME), nx=nx, ny=ny, &
                 im_world=im_dyn, jm_world=jm_dyn, lm=lm_dyn, &
                 pole='PC',dateline='DC', rc=status))
       _VERIFY(status)

!   Validate grid
!   -------------
       call ESMF_GridValidate(GCMgrid,__RC__)
    endif

    sameres = IM_INC==IM_DYN .and. JM_INC==JM_DYN

!   Create a clock
!   --------------
    CLOCK = ESMF_ClockCreate ( name="IAUClock", timeStep=TimeStep, startTime=Time, __RC__ )

    AINC = MAPL_AddChild ( MAPLOBJ,  & !Grid=INCgrid,    &
                                       ConfigFile=myRC,   &
                                        name= 'AINC',     &
                                 SS = AINCSetServices,  &
                                                  __RC__  )
    if (cubed) then
       ADYN = MAPL_AddChild ( MAPLOBJ, &!Grid=GCMgrid,      &
                                          ConfigFile=myRC,  &
                                              name= 'DYN', &
                                   SS = DynVecSetServices,  &
                                                     __RC__ )
       MOIST= MAPL_AddChild ( MAPLOBJ, &!Grid=GCMgrid,      &
                                          ConfigFile=myRC,  &
                                            name= 'MOIST',  &
                                   SS = MoistSetServices,   &
                                                     __RC__ )
       PCHEM= MAPL_AddChild ( MAPLOBJ, &!Grid=GCMgrid,      &
                                          ConfigFile=myRC,  &
                                            name= 'PCHEM',  &
                                   SS = PchemSetServices,   &
                                                     __RC__ )
    else
       ADYN = MAPL_AddChild ( MAPLOBJ, Grid=GCMgrid,        &
                                           ConfigFile=myRC, &
                                               name= 'DYN', &
                                    SS = DynVecSetServices, &
                                                     __RC__ )
    endif

!   Initialize component
!   --------------------
    call MAPL_Get  ( MAPLOBJ, GCS=GCS, GIM=IMPORTS, GEX=EXPORTS, __RC__ )

    if (cubed) then
       call MAPL_GridCreate  (GCS(ADYN),ESMFGRID=GCMgrid, __RC__)
       call ESMF_GridCompGet (GCS(ADYN), grid=GCMgrid, __RC__ )
       call ESMF_GridValidate(GCMgrid,__RC__)
       call ESMF_GridCompSet (GCS(MOIST), Grid=GCMgrid, __RC__ )
       call ESMF_GridCompSet (GCS(PCHEM), Grid=GCMgrid, __RC__ )
       call ESMF_GridCompSet (GCS(AINC) , Grid=GCMgrid, __RC__ )
    endif

    call ESMF_GridCompInitialize ( GCS(ADYN) , importState=IMPORTS(ADYN),  exportState=EXPORTS(ADYN),  clock=CLOCK, __RC__ )
    call ESMF_GridCompInitialize ( GCS(AINC) , importState=IMPORTS(AINC),  exportState=EXPORTS(AINC),  clock=CLOCK, __RC__ )
    call ESMF_GridCompInitialize ( GCS(MOIST), importState=IMPORTS(MOIST), exportState=EXPORTS(MOIST), clock=CLOCK, __RC__ )
    call ESMF_GridCompInitialize ( GCS(PCHEM), importState=IMPORTS(PCHEM), exportState=EXPORTS(PCHEM), clock=CLOCK, __RC__ )
    if ( cubed ) then ! initialize GetWeights and FMS mambo-jambo
         call GetWeights_init (6,1,im_dyn,im_dyn,lm_dyn,Nx,Ny,.true.,.false.,comm)
    endif

#if 0
    if ( MAPL_AM_I_ROOT() ) then
       call ESMF_StatePrint(IMPORTS(ADYN))
    end if
#endif

!   Prepare import of base state
!   ----------------------------
    call set_()

!   Get exports from internal state
!   -------------------------------
    call ESMF_GridCompRun (GCS(AINC),importState=IMPORTS(AINC), exportState=EXPORTS(AINC), clock=CLOCK, PHASE=2, __RC__)

!   Connect Exports of Run above with Imports of one below
!   ------------------------------------------------------
    call connect_()

!   Update dyn-state
!   ----------------
    call ESMF_GridCompRun (GCS(ADYN),importState=IMPORTS(ADYN), exportState=EXPORTS(ADYN), clock=CLOCK, __RC__)
    call fvwrtout_()
    if ( iqvupd/=0 ) then
       call ESMF_GridCompRun (GCS(MOIST),importState=IMPORTS(MOIST), exportState=EXPORTS(MOIST), phase=2, clock=CLOCK, __RC__)
    endif
    if ( io3upd/=0 ) then
       ana_pe=dyn_pe
       call ESMF_GridCompRun (GCS(PCHEM),importState=IMPORTS(PCHEM), exportState=EXPORTS(PCHEM), phase=2, clock=CLOCK, __RC__)
    endif

!   Finalize component
!   ------------------
    call ESMF_GridCompFinalize ( GCS(PCHEM), importState=IMPORTS(PCHEM), exportState=EXPORTS(PCHEM), clock=CLOCK, __RC__ )
    call ESMF_GridCompFinalize ( GCS(MOIST), importState=IMPORTS(MOIST), exportState=EXPORTS(MOIST), clock=CLOCK, __RC__ )
    call ESMF_GridCompFinalize ( GCS(AINC) , importState=IMPORTS(AINC) , exportState=EXPORTS(AINC) , clock=CLOCK, __RC__ )
    call ESMF_GridCompFinalize ( GCS(ADYN) , importState=IMPORTS(ADYN) , exportState=EXPORTS(ADYN) , clock=CLOCK, __RC__ )

!   All done
!   --------
    if (MAPL_AM_I_ROOT()) then
          close(999)
          open (999,file='RSTUPD_EGRESS',form='formatted')
          close(999)
    end if
#ifdef _SOON_
    call final_
#endif /* _SOON_ */
    call ESMF_Finalize(__RC__)

  end subroutine Main

!BOP
! !ROUTINE: init_: initialize rstupd
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

    integer thistime(6), status
    character(len=ESMF_MAXSTR) :: tmpl
    character(len=ESMF_MAXSTR) :: dyntyp

!   Create Config and Initialize Clock 
!   ----------------------------------
    CF = ESMF_ConfigCreate   (__RC__)
    call ESMF_ConfigLoadFile ( CF, myrc, __RC__ )

!  Set defaults
!  ------------
   rc = 0
   cubed = .false.

   call ESMF_ConfigGetAttribute( CF, NX, label ='NX:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, NY, label ='NY:', __RC__ )

   call ESMF_ConfigGetAttribute( CF, IM_DYN, label ='DYN_IM:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, JM_DYN, label ='DYN_JM:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, LM_DYN, label ='DYN_LM:', __RC__ )

   call ESMF_ConfigGetAttribute( CF, DYNTYP, label ='DYCORE:', __RC__ )
   if(trim(dyntyp)=='FV3') cubed=.true.

   call ESMF_ConfigGetAttribute( CF, nymd, label ='DYN_DATE:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, nhms, label ='DYN_TIME:', __RC__ )

   call ESMF_ConfigGetAttribute( CF, sclinc, label ='SCLINC:', default=1.0, rc=status )

!  following default is set to be consistent with the GCM component 
!  to get an update user must specify RC parameter
   call ESMF_ConfigGetAttribute( CF, iqvupd, label ='ALLOW_MOIST_AINC_UPDATE:', default=0, rc=status )
   call ESMF_ConfigGetAttribute( CF, io3upd, label ='ALLOW_PCHEM_AINC_UPDATE:', default=0, rc=status )

   call ESMF_ConfigGetAttribute( CF, sdf_ofname, label ='SDF_FILENAME:', default='NONE', __RC__ )
   if (trim(sdf_ofname)=="NONE") then
       SDFoutput = .false.
   else
       SDFoutput = .true.
   endif


!  call ESMF_ConfigGetAttribute( CF, tmpl,  label ='REPLAY_FILE:', __RC__ )
!  call StrTemplate ( incfname, tmpl, 'GRADS', nymd=nymd, nhms=nhms, stat=status)

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

   end subroutine init_

   subroutine info_ (agrid_bkg,dgrid_bkg,tvflag_bkg,thvflag_bkg )
   use MAPL_Mod, only: MAPL_ROOT
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
          print *, '      Output resolution: ',IM_DYN,JM_DYN,LM_DYN
          print *
      endif
   end subroutine info_

   subroutine set_ ()
   
!  use MAPL_SimpleBundleMod, only: MAPL_SimpleBundleCreate
!  use MAPL_SimpleBundleMod, only: MAPL_SimpleBundleGetIndex
!  use MAPL_SimpleBundleMod, only: MAPL_SimpleBundle
!  use MAPL_SimpleBundleMod, only: MAPL_SimpleBundlePrint

   use MAPL_Mod, only: MAPL_RVAP
   use MAPL_Mod, only: MAPL_RGAS

   implicit none

   call MAPL_GetPointer(EXPORTS(AINC), dudt , 'DUDT' ,alloc=.true.,__RC__ )
   call MAPL_GetPointer(EXPORTS(AINC), dvdt , 'DVDT' ,alloc=.true.,__RC__ )
   call MAPL_GetPointer(EXPORTS(AINC), dtdt , 'DTDT' ,alloc=.true.,__RC__ )
   call MAPL_GetPointer(EXPORTS(AINC), dpedt, 'DPEDT',alloc=.true.,__RC__ )
   call MAPL_GetPointer(EXPORTS(AINC), dqvdt, 'DQVDT',alloc=.true.,__RC__ )
   call MAPL_GetPointer(EXPORTS(AINC), do3dt, 'DO3DT',alloc=.true.,__RC__ )
   call MAPL_GetPointer(EXPORTS(AINC), dtsdt, 'DTSDT',alloc=.true.,__RC__ )

   call MAPL_GetPointer(IMPORTS(ADYN), sdudt , 'DUDT' ,alloc=.true.,__RC__ )
   call MAPL_GetPointer(IMPORTS(ADYN), sdvdt , 'DVDT' ,alloc=.true.,__RC__ )
   call MAPL_GetPointer(IMPORTS(ADYN), sdtdt , 'DTDT' ,alloc=.true.,__RC__ )
   call MAPL_GetPointer(IMPORTS(ADYN), sdpedt, 'DPEDT',alloc=.true.,__RC__ )

   if (iqvupd/=0) then ! by construction, when iqvupd==0 import does not even exist
      call MAPL_GetPointer(IMPORTS(MOIST), sdqvdt, 'QVAINC',alloc=.true.,__RC__ )

      call MAPL_GetPointer(IMPORTS(ADYN), qtotbkg,   'QTOT',alloc=.true.,__RC__ )
      call MAPL_GetPointer(EXPORTS(MOIST),  qvbkg,   'QICN',alloc=.true.,__RC__ )
      qtotbkg=qvbkg
      call MAPL_GetPointer(EXPORTS(MOIST),  qvbkg,   'QLCN',alloc=.true.,__RC__ )
      qtotbkg=qtotbkg+qvbkg
      call MAPL_GetPointer(EXPORTS(MOIST),  qvbkg,   'QLLS',alloc=.true.,__RC__ )
      qtotbkg=qtotbkg+qvbkg
      call MAPL_GetPointer(EXPORTS(MOIST),  qvbkg,   'QLCN',alloc=.true.,__RC__ )
      qtotbkg=qtotbkg+qvbkg
      call MAPL_GetPointer(EXPORTS(MOIST),  qvbkg,      'Q',alloc=.true.,__RC__ )
      qtotbkg=qtotbkg+qvbkg
   endif

   if (io3upd/=0) then ! by construction, when iqvupd==0 import does not even exist
      call MAPL_GetPointer(IMPORTS(PCHEM), sdo3dt, 'O3AINC',alloc=.true.,__RC__ )
      call MAPL_GetPointer(IMPORTS(PCHEM), ana_pe,   'PLE' ,alloc=.true.,__RC__ )
      call MAPL_GetPointer(EXPORTS(ADYN),  dyn_pe,    'PE' ,alloc=.true.,__RC__ )
   endif

   if ( SDFoutput ) then
      if(.not.associated(dyn_pe)) &
        call MAPL_GetPointer(EXPORTS(ADYN),  dyn_pe,    'PE' ,alloc=.true.,__RC__ )
        call MAPL_GetPointer(EXPORTS(ADYN), dyn_pkz,   'PKZ' ,alloc=.true.,__RC__ )
        call MAPL_GetPointer(EXPORTS(ADYN),   dyn_u,     'U' ,alloc=.true.,__RC__ )
        call MAPL_GetPointer(EXPORTS(ADYN),   dyn_v,     'V' ,alloc=.true.,__RC__ )
        call MAPL_GetPointer(EXPORTS(ADYN),  dyn_pt,    'PT' ,alloc=.true.,__RC__ )
        call MAPL_GetPointer(EXPORTS(ADYN),  dyn_dz,    'DZ' ,alloc=.true.,__RC__ )
        call MAPL_GetPointer(EXPORTS(ADYN),   dyn_w,     'W' ,alloc=.true.,__RC__ )
   endif

!  call MAPL_GetPointer(IMPORTS(STUB), sdtsdt, 'DTSDT',alloc=.true.,__RC__ )

!  Clean up

   end subroutine set_

   subroutine connect_

!  use MAPL_SimpleBundleMod, only: MAPL_SimpleBundleCreate
!  use MAPL_SimpleBundleMod, only: MAPL_SimpleBundleGetIndex
!  use MAPL_SimpleBundleMod, only: MAPL_SimpleBundle
!  use MAPL_SimpleBundleMod, only: MAPL_SimpleBundlePrint

   use m_mpif90, only: MP_TYPE,MP_SUM

   implicit none

   type(ESMF_FieldBundle)   :: DYNBundle

!  type(MAPL_HorzTransform) :: L2C
!  type(MAPL_HorzTransform) :: C2L
!  type(MAPL_HorzTransform) :: L2C_AD
!  type(MAPL_HorzTransform) :: C2L_AD

   character(len=*), parameter :: Iam = "connect_"
   integer,parameter :: lu=10
   integer ii, jj
   integer iu, ju, iv, jv
   integer iii, jjj, kkk
   integer ndim_ll, im_ll,jm_ll,km_ll
   integer ndim_cb, im_cb,jm_cb,km_cb
   integer indx
   real(8) :: sdot,rdot1,rdot2

   if (MAPL_AM_I_ROOT()) then
      print *, 'Increment scaled by: ', sclinc
   end if
   sdudt =sclinc*dudt
   sdvdt =sclinc*dvdt
   sdtdt =sclinc*dtdt
   sdpedt=sclinc*dpedt

   if (associated(sdqvdt)) then
      sdqvdt=sclinc*dqvdt
   endif

   if (associated(sdo3dt)) then
      sdo3dt=sclinc*do3dt
   endif

   end subroutine connect_

   subroutine fvwrtout_

   use ESMFL_Mod, only: ESMFL_State2Bundle

   implicit none

   type(MAPL_CFIO)         :: CFIO
   type(ESMF_FieldBundle)  :: FVBundle
   type(ESMF_FieldBundle)  :: IOBundle  ! Used for output purposes
   type(ESMF_Field)        :: Field

   character(len=ESMF_MAXSTR) :: ofname
   integer  ifld, nfld
   integer  mx,my
   integer  dimCount
   integer, allocatable :: gridToFieldMap(:)
   real, pointer, dimension(:,:)   :: ptr2d
   real, pointer, dimension(:,:,:) :: ptr3d
   real,pointer,dimension(:,:,:)::aux3d

   if ( .not. SDFoutput ) return

       FVBundle = ESMF_FieldBundleCreate ( name='FV bundle', __RC__ )
       call ESMF_FieldBundleSet ( FVBundle, grid=GCMgrid, __RC__ )

       call ESMFL_State2Bundle (EXPORTS(ADYN), FVBundle)

!      Write out increment as SDF output
!      ---------------------------------
       IOBundle = ESMF_FieldBundleCreate ( name='IO bundle', __RC__ )
       call ESMF_FieldBundleSet(IOBundle, grid=GCMgrid, __RC__ )
       call ESMF_FieldBundleGet(FVBundle,FieldCount=nfld, __RC__ )
       do ifld=1,nfld
          call ESMF_FieldBundleGet(FVBundle, ifld, Field, __RC__ )
          call ESMF_FieldGet(Field, NAME=NAME, __RC__ )
          if (trim(NAME)=='AK' .or. &
              trim(NAME)=='BK') cycle
          if (trim(NAME)=='PE'.or.trim(NAME)=='PKZ') then
              call ESMF_FieldGet(Field,fArrayPtr=ptr3d,rc=status)
              mx=size(ptr3d,1)
              my=size(ptr3d,2)
              ! handle first LM levels 
              allocate(aux3d(mx,my,LM_DYN))
              aux3d = ptr3d(:,:,0:LM_DYN-1)
              Field = ESMF_FieldCreate(grid=GCMgrid, fArrayptr=aux3d, &
                                       name=trim(NAME)//'M1', &
                                       datacopyflag=ESMF_DATACOPY_VALUE, __RC__ )
              call ESMF_AttributeSet(Field, NAME='VLOCATION', &
                                  VALUE=MAPL_VLocationCenter,__RC__)
              call ESMF_AttributeSet(Field, NAME='DIMS', &
                                     VALUE=MAPL_DimsHorzVert,__RC__)
              call MAPL_FieldBundleAdd(IOBundle, Field, rc=STATUS)
              ! handle level LM+1
              allocate(ptr2d(mx,my))
              ptr2d = ptr3d(:,:,LM_DYN)
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
              Field = ESMF_FieldCreate(grid=GCMgrid, fArrayptr=ptr2d, name='PS'//trim(NAME), &
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
              call MAPL_FieldBundleAdd(IOBundle, Field,rc=STATUS)
              call ESMF_FieldGet(Field,fArrayPtr=ptr3d,rc=STATUS)
          endif
       enddo
       call StrTemplate ( ofname, trim(sdf_ofname), 'GRADS', nymd=nymd, nhms=nhms, stat=status )
       VERIFY_(status)
       call MAPL_CFIOCreate ( CFIO, trim(ofname), CLOCK, IOBundle,  &
                              DESCR='FV Core Fields', __RC__ )
       call MAPL_CFIOWrite ( CFIO, CLOCK, IOBundle )
       call MAPL_cfioDestroy ( CFIO )

   end subroutine fvwrtout_
   
   subroutine final_
!   call ESMF_FieldBundleDestroy (IncBundle, __RC__)
!   deallocate(thv_bkg,tv_bkg,ple_bkg)
!   deallocate(ak,bk)
   end subroutine final_
 
end Program rstUPD
