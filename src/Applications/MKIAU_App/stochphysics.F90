
#  include "MAPL_Generic.h"

program stochphysics

  use ESMF
  use MAPL
  use LatLonToCubeRegridderMod
  use CubeToLatLonRegridderMod
  use CubeToCubeRegridderMod
  use stoch_module

  implicit none

  character(len=*), parameter :: myRC= 'stochphysics.rc'
  character(len=*), parameter :: Iam = 'stochphysics'
  character(len=ESMF_MAXSTR)  :: testcase
  character(len=80)           :: fnout
!
  type(ESMF_grid)   :: GRID
  type(ESMF_config) :: CF
  type(ESMF_VM)     :: vm
  type(CubedSphereGridFactory) :: factory
  type(CubedSphereGridFactory) :: cs_factory
  type (CubeToLatLonRegridder) :: cube_to_latlon_prototype
  type (LatLonToCubeRegridder) :: latlon_to_cube_prototype
!
  real(4), pointer, dimension(:,:,:) :: uwnd,vwnd
  real(4), allocatable  :: RNDPTR(:,:,:)
  real(4), allocatable  :: PPREF(:)    
  real(4), parameter    :: del_t=450.
  integer, parameter    :: lu=10
  integer, parameter    :: lun=22
  integer :: im_sppt,jm_sppt,lm_sppt
  integer :: dims(3),IM,JM,LM,k
  integer :: myPET,nPET,Nx,Ny
  integer :: comm,status,rc,iloop
  logical :: icubed
   

  ! Initialize the ESMF. For performance reasons, it is important
  ! to turn OFF ESMF's automatic logging feature
  ! -------------------------------------------------------------
  call ESMF_Initialize (logKindFlag=ESMF_LOGKIND_NONE, vm=vm,rc=status)

  call init_ ( CF,rc=status )

  ! Check the number of processors
  ! ------------------------------
  call ESMF_VMGet(vm, localPET=myPET, PETcount=nPET)
  if ( nPET /= Nx * Ny ) then
     if ( MAPL_am_I_root() ) then
        print *, 'Error: expecting ', Nx*Ny, ' PETs but found ', nPET, 'PETs'
        print *, 'Try:  mpirun -np ', Nx*Ny, ' mkIAU.x'
     end if
  end if

  if ( MAPL_am_I_root() ) then
     print *
     print *, 'Starting ' // Iam // ' with ', nPET, ' PETs ...'
     print *, 'NX , NY ', NX , NY
     print *
     print *, 'WARNING, Creating empty input.nml file'
     open(lun,file='input.nml',form='formatted')
        write(lun,*) "   "
     close(lun)
  end if
  call ESMF_VMGetCurrent(vm=vm, rc=status)
  call ESMF_VMGet(vm,mpiCommunicator=comm,rc=status)

  ! Create a regular Lat/Lon grid over which SPPT PATERNS defined
  ! --------------------------------------------------------
  icubed=.false.
  if (jm_sppt==6*im_sppt) icubed=.true.
  if (icubed) then
!     GRID = AppGridCreateF(IM_SPPT,JM_SPPT,LM_SPPT,Nx,Ny,STATUS) 
    cs_factory = CubedSphereGridFactory(IM_World=IM_SPPT,LM=LM_SPPT,Nx=Nx,Ny=Ny/6,rc=status)
    GRID = grid_manager%make_grid(cs_factory,rc=status)   
  else
    ll_factory = LatLonGridFactory(grid_name='GRID', &
                      Nx = Nx, Ny = Ny,  &
                      IM_World = IM_SPPT,  &
                      JM_World = JM_SPPT,  &
                      LM = LM_SPPT, pole='PC', dateline='DC',  &
                                 rc=status )
    GRID = grid_manager%make_grid(ll_factory)
  endif

  ! Validate grid
  ! -------------
  call ESMF_GridValidate(GRID,rc=status)
  call MAPL_GridGet(GRID, localCellCountPerDim=DIMS, RC=STATUS)
   
  if ( icubed ) then
    print*,'calling getweights', dims(1), dims(2), dims(3)
     call GetWeights_init (6,1,im_sppt,im_sppt,lm_sppt,Nx,Ny,.true.,.false.,comm)
    print*,'done getweights', dims(1), dims(2), dims(3)
  endif

  ! GLOBAL FOR NOW
  IM=dims(1)
  JM=dims(2)
  LM=dims(3)


  print*, 'global dimensions are ', IM,JM,LM

  allocate(PPREF(LM+1))
  open(lun,file='pref.txt',form='formatted',status='old')
    do k=1,LM+1
      read(lun,*) ppref(k)
    enddo 
  close(lun)

  allocate(RNDPTR(IM,JM,LM))
  RNDPTR = 0.

  if (testcase=='skeb') then
    allocate(uwnd(IM,JM,LM))
    allocate(vwnd(IM,JM,LM))
    call setup_pattern(CF,GRID)
    call getwinds_(uwnd,vwnd)
    call skeb_pattern(CF,GRID,UWND,VWND,PPREF,IM,JM,LM,del_t)
  elseif(testcase=='sppt') then
    do iloop = 1, 10 
       print*,' looping ', iloop
       call sppt_pattern(CF,GRID,RNDPTR,PPREF,IM,JM,LM,del_t)
    enddo 
  endif

  open (lu,file=trim(fnout),form='unformatted',access='sequential',convert='little_endian')
  if(testcase=='skeb') then
    call writout4(lu,uwnd,Grid,JM,IM,LM)
    call writout4(lu,vwnd,Grid,JM,IM,LM)
    deallocate(uwnd)
    deallocate(vwnd)
  else
    call writout4(lu,RNDPTR,Grid,JM,IM,LM)
  endif
  close(lu)
  deallocate(PPREF)
  deallocate(RNDPTR)

  !   All done
  !   -------- 
  call clear_pattern
  call ESMF_Finalize()

  !===========================================================

contains

  !===========================================================
  subroutine init_ ( CF, rc )

    use ESMF, only: ESMF_FALSE
    use m_StrTemplate, only: StrTemplate

    implicit NONE

    type(ESMF_Config)     :: CF
    integer, intent(out)  :: rc      ! return error code
    character*4, parameter :: myname = 'init_'
    integer   status

    !   Create Config and Initialize Clock 
    !   ----------------------------------
    CF = ESMF_ConfigCreate   (rc=status)
    call ESMF_ConfigLoadFile ( CF, myRC, rc=status )

    !  Set defaults
    !  ------------
    rc = 0
    icubed = .false.

    call ESMF_ConfigGetAttribute( CF, NX, label ='NX:',rc=status )
    call ESMF_ConfigGetAttribute( CF, NY, label ='NY:',rc=status )

    call ESMF_ConfigGetAttribute( CF, IM_SPPT, label ='AGCM_IM:', rc=status )
    call ESMF_ConfigGetAttribute( CF, JM_SPPT, label ='AGCM_JM:', rc=status )
    call ESMF_ConfigGetAttribute( CF, LM_SPPT, label ='AGCM_LM:', rc=status )

    call ESMF_ConfigGetAttribute( CF, TESTCASE, label ='TESTCASE:', default='sppt', rc=status )
    fnout = 'rndpttn.grd'
    if(testcase=='skeb') fnout = 'skebwnd.grd'

  end subroutine init_

  !===========================================================
  subroutine getwinds_ (u,v)

    implicit none

    real(4),pointer,intent(inout) :: u(:,:,:) 
    real(4),pointer,intent(inout) :: v(:,:,:) 
    type(ESMF_FieldBundle)        :: InBundle
    type(ESMF_Time)               :: Time
    type(ESMF_TimeInterval)       :: TimeStep ! used to define a clock
    type(ESMF_Field)              :: Field    

    character(len=ESMF_MAXSTR)    :: filen
    character(len=ESMF_MAXSTR)    :: ovars
    character(len=ESMF_MAXSTR)    :: name
    integer n,nymd,nhms,nfld
      
    filen = 'prePP_rt.bkg.eta.20160522_12z.nc4'  ! wired for now
    ovars = 'u,v'
    nymd = 20160522 ! wired for now
    nhms = 120000   ! wired for now
    call set_datetime_ (nymd, nhms, Time, TimeStep)

    ! assume input grid same as internal grid
    InBundle = ESMF_FieldBundleCreate ( name='Input bundle', rc=status )
    call ESMF_FieldBundleSet(InBundle, grid=Grid, rc=status )

    call MAPL_CFIORead  ( trim(filen), Time, INbundle,      &
                          TIME_IS_CYCLIC=.false., rc=status )

    ! extract desired fields
    call ESMF_FieldBundleGet(INbundle,FieldCount=nfld, rc=status )
    do n=1,nfld
       call ESMF_FieldBundleGet(INbundle, n, Field, rc=status )
       call ESMF_FieldGet(Field, NAME=name,         rc=status )
       if(trim(name)=='u') then
         if(MAPL_am_I_root()) print *, 'loading u ...'
         call ESMF_FieldGet(Field, farrayPtr=u,     rc=status )
       endif
       if(trim(name)=='v') then
         if(MAPL_am_I_root()) print *, 'loading v ...'
         call ESMF_FieldGet(Field, farrayPtr=v,     rc=status )
       endif
    enddo

  end subroutine getwinds_

  !===========================================================
  subroutine set_datetime_ (nymd, nhms, Time, TimeStep)

    implicit none
    integer, intent(in)     :: nymd, nhms
    type(ESMF_Time)         :: Time     ! Time objects
    type(ESMF_TimeInterval) :: TimeStep ! used to define a clock

    integer thistime(6)

    thistime(1) =     nymd/10000
    thistime(2) = mod(nymd,10000)/100
    thistime(3) = mod(nymd,100)
    thistime(4) =     nhms/10000
    thistime(5) = mod(nhms,10000)/100
    thistime(6) = mod(nhms,100)

    ! Set ESMF date/time
    ! ------------------
    call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN )
    call ESMF_TimeSet(Time, yy=thistime(1), mm=thistime(2), dd=thistime(3), &
                             h=thistime(4), m =thistime(5),  s=thistime(6))
    call ESMF_TimeIntervalSet( TimeStep, h=6, m=0, s=0, rc=status )

  end subroutine set_datetime_
  !===========================================================

  end program stochphysics

