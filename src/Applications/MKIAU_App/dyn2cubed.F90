! dyn2cubed.x - ESMF/MAPL application to convert dyn to cubed-sphere
!
! 1. Read DYN file using MAPL
! 2. Write it out in either lat/lon or cube
!
! REMARKS: 
!   a) This program requires an RC file: dyn2cubed.rc
!
! Ricardo Todling, September 2020
!............................................................................
!  !REVISION_HISTORY:  
!   14Sep2020  Todling  Ripe from dyn2cubed
!----------------------------------------------------------------------------

#  include "MAPL_Generic.h"

   Program dyn2cubed

   use ESMF
   use MAPL
   use CubeToLatLonRegridderMod
   use CubeToCubeRegridderMod
   use LatLonToCubeRegridderMod
   use m_set_eta, only: set_eta
   use m_ioutil, only: luavail
   use m_StrTemplate, only: StrTemplate
   use m_die, only: mp_die
   use m_zeit, only: zeit_ci,zeit_co,zeit_flush
   use m_zeit, only: zeit_allflush
   

   implicit NONE

   character(len=*), parameter :: myname = 'Dyn2Cubed'

!  Basic ESMF objects being used in this example
!  ---------------------------------------------
   type(ESMF_Grid)         :: GCMgrid   ! GCM Grid
   type(ESMF_Grid)         :: BKGgrid   ! BKG Grid
   type(ESMF_Grid)         :: ANAgrid   ! ANA Grid
   type(ESMF_FieldBundle)  :: BkgBundle ! Bundle to hold LL background
   type(ESMF_FieldBundle)  :: GCMBundle ! Bundle to hold CC background
   type(ESMF_FieldBundle)  :: TSTBundle ! Bundle to hold LL background
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
   type(MAPL_MetaComp),pointer :: MAPLOBJ

   type (CubedSphereGridFactory) :: factory
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

   integer :: Nx, Ny                   ! Layout
   integer :: nx_cube, ny_cube
   integer :: im_bkg, jm_bkg, lm_bkg   ! Bkg Grid dimensions
   integer :: im_iau, jm_iau, lm_iau   ! IAU Grid dimensions (revisit for cubed)

   integer, parameter :: r_quad  = selected_real_kind(20)
   integer :: comm
   logical :: sameres,cubed
   real    :: sclinc
!
   real*8, pointer :: ak(:), bk(:)
   real,   pointer :: levels(:)=>NULL()
   real(8),allocatable :: beta_weight(:)
   character(len=ESMF_MAXSTR) :: levunits
!
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
   character(len=ESMF_MAXSTR) :: ivars
   character(len=ESMF_MAXSTR) :: outfname

   integer :: proper_winds = 1
   logical :: overwrite_with_gsibetas=.false.

!  Coordinate variables
!  --------------------
   character(len=ESMF_MAXSTR)    :: name

   character(len=*), parameter :: Iam = 'dyn2cubed'
   character(len=*), parameter :: myRC= 'dyn2cubed.rc'

   type(ESMF_GridComp) :: temp_gc
   type(ESMF_Config) :: temp_config


!                             -----
    
    call Main()

CONTAINS

    subroutine Main()

    character(len=30) ABKGGRIDNAME

!   Initialize the ESMF. For performance reasons, it is important
!    to turn OFF ESMF''s automatic logging feature
!   -------------------------------------------------------------
    call ESMF_Initialize (logKindFlag=ESMF_LOGKIND_NONE, vm=vm, __RC__)
    call ESMF_VMGetCurrent(vm=vm, rc=status)
    call ESMF_VMGet(vm,mpiCommunicator=comm,rc=status)

    call MAPL_Initialize(rc=status)
    VERIFY_(status)

    call init_ ( CF, nymd, nhms, __RC__ )

    call zeit_ci('dyn2cubed')

!   Check the number of processors
!   ------------------------------
    call ESMF_VMGet(vm, localPET=myPET, PETcount=nPET)  
    if ( nPET /= Nx * Ny ) then
       if ( MAPL_am_I_root() ) then
          print *, 'Error: expecting ', Nx*Ny, ' PETs but found ', nPET, 'PETs'
          print *, 'Try:  mpirun -np ', Nx*Ny, ' dyn2cubed.x'
       end if
       ASSERT_(.FALSE.)
    end if

    if ( MAPL_am_I_root() ) then
         print *
         print *, 'Starting ' // Iam // ' with ', nPET, ' PETs ...'
         print *
    end if


!   Create a regular Lat/Lon grid over which BKG/ANA defined
!   --------------------------------------------------------
    if (JM_BKG==6*IM_BKG) then
      print *, "I am cubed "
       cs_factory = CubedSphereGridFactory(im_world=IM_BKG,lm=LM_BKG,nx=nx_cube,ny=ny_cube,__RC__)
       BKGGrid = grid_manager%make_grid(cs_factory,__RC__)
    else
       call MAPL_DefGridName (IM_BKG,JM_BKG,ABKGGRIDNAME,MAPL_am_I_root())
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
    if (cubed) then
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

!   Validate grid
!   -------------
       call ESMF_GridValidate(GCMgrid,__RC__)
    endif

    sameres = IM_BKG==IM_IAU .and. JM_BKG==JM_IAU

!   Create a clock
!   --------------
    CLOCK = ESMF_ClockCreate ( name="IAUClock", timeStep=TimeStep, startTime=Time, __RC__ )

!   Create bundle to hold background/perturbations
!   ----------------------------------------------
    BkgBundle = ESMF_FieldBundleCreate ( name='BKG bundle', __RC__ )
    call ESMF_FieldBundleSet ( BkgBundle, grid=BKGgrid, __RC__ )

    call MAPL_CFIORead  ( bkgfname, Time, BkgBundle, &
                          only_vars=trim(ivars), &
                          TIME_IS_CYCLIC=.false., verbose=.true., __RC__ )


!   Create bundle to hold cubed-fields
!   ----------------------------------
    GCMBundle = BundleClone(BKGBundle,GCMgrid,__RC__)

!   Convert to cubed-sphere
!   -----------------------
    call ll2cc_(BKGBundle,GCMBundle,.true.)
 
!   Write out bundle
!   ----------------
    call MAPL_CFIOCreate ( cfio, trim(outfname), clock, GCMBundle,  &
!                          FREQUENCY=freq, &
                           DESCR='Write Stats Fields', __RC__ )
    call MAPL_CFIOWrite ( cfio, clock, GCMBundle, __RC__ )
    call MAPL_cfioDestroy ( cfio )

!   Test ... convert back to LL
!   ---------------------------
#ifdef _RECOVER_FIELDS_
    TSTBundle = BundleClone(BKGBundle,BKGgrid,__RC__)

    call ll2cc_(GCMBundle,TSTBundle,.false.)

    call MAPL_CFIOCreate ( cfio, 'recover.nc4', clock, TSTBundle,  &
                           DESCR='Write Stats Fields', __RC__ )
    call MAPL_CFIOWrite ( cfio, clock, TSTBundle, __RC__ )
    call MAPL_cfioDestroy ( cfio )
#endif /* _RECOVER_FIELDS_ */

!   All done
!   --------
    call zeit_co('dyn2cubed')
    if (MAPL_AM_I_ROOT()) then
          call zeit_flush(6,subname_at_end=.true.)
          close(999)
          open (999,file='DYN2CUBED',form='formatted')
          close(999)
    end if
    if(allocated(beta_weight)) deallocate(beta_weight)
    call final_
    call MAPL_Finalize()
    call ESMF_Finalize(__RC__)

  end subroutine Main

!BOP
! !ROUTINE: init_: initialize dyn2cubed
!
! !DESCRIPTION:
!
! !INTERFACE:
!
    subroutine init_ ( CF, nymd, nhms, rc ) 

! !USES:

    use ESMF, only: ESMF_FALSE
    use m_StrTemplate, only: StrTemplate
    use m_mpif90, only: mp_integer,mp_character

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

    character(len=*), parameter :: myname_ = myname//'*init_'

    integer thistime(6), idum, iarg, itest, argc, k, status
    character(len=ESMF_MAXSTR) :: tmpl
    character(len=ESMF_MAXSTR) :: dyntyp
    character(len=ESMF_MAXSTR) :: covweights
    character(len=ESMF_MAXSTR) :: covoption
    character(len=255) :: argv

!   Parsed from command line
!   ------------------------
    rc = 0
    if ( MAPL_am_I_root() ) then

       bkgfname = "NULL"
       outfname = "NULL"
       argc =  iargc()
       if ( argc .lt. 1 ) then
          call usage_()
          rc = 1
       else
          iarg = 1
          call GetArg ( iarg, argv )
          read(argv,*) nymd
          iarg = iarg + 1
          call GetArg ( iarg, argv )
          read(argv,*) nhms
          if (argc>2) then
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv,*) bkgfname
             iarg = iarg + 1
             call GetArg ( iarg, argv )
             read(argv,*) outfname
          endif
       endif
    endif
    if (rc/=0) then
       call mp_die(myname,": try again", rc)
    endif
    call mpi_bcast(nymd, 1,mp_integer,MAPL_root,comm,rc)
    call mpi_bcast(nhms, 1,mp_integer,MAPL_root,comm,rc)
    call mpi_bcast(bkgfname,ESMF_MAXSTR,mp_character,MAPL_root,comm,rc)
    call mpi_bcast(outfname,ESMF_MAXSTR,mp_character,MAPL_root,comm,rc)

    if ( MAPL_am_I_root() ) then
       print*, ' Input file: ', trim(bkgfname)
       print*, 'Output file: ', trim(outfname)
    endif
    if (trim(bkgfname) == "NULL" .or. trim(outfname) == "NULL" ) then
       call mp_die ('main',": file names undefied, abort",99)
    endif

!   Create Config and Initialize Clock 
!   ----------------------------------
    CF = ESMF_ConfigCreate   (__RC__)
    call ESMF_ConfigLoadFile ( CF, myrc, __RC__ )

!  Set defaults
!  ------------
   rc = 0
   cubed = .false.

   call MAPL_MakeDecomposition(nx,ny,__RC__)
   call MAPL_MakeDecomposition(nx_cube,ny_cube,reduceFactor=6,__RC__)

   call ESMF_ConfigGetAttribute( CF, IM_IAU, label ='AGCM.IM_WORLD:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, JM_IAU, label ='AGCM.JM_WORLD:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, LM_IAU, label ='AGCM.LM:', __RC__ )

   call ESMF_ConfigGetAttribute( CF, proper_winds, label ='PROPER_WINDS:', default=proper_winds, __RC__ )

   call ESMF_ConfigGetAttribute( CF, covweights, label ='GSI_COVWEIGHTS:', default='NULL', __RC__ )
   call ESMF_ConfigGetAttribute( CF, covoption, label ='GSI_COVOPTION:', default='NULL', __RC__ )

   call ESMF_ConfigGetAttribute( CF, DYNTYP, label ='DYCORE:', __RC__ )
   if(trim(dyntyp)=='FV3' .or. JM_BKG==6*IM_BKG) cubed=.true.

   call ESMF_ConfigGetAttribute( CF, ivars, label ='INPUT_VARS:', __RC__ )

   if (trim(bkgfname) == "NULL") then
   call ESMF_ConfigGetAttribute( CF, tmpl,  label ='INPUT_FILE:', __RC__ )
   call StrTemplate ( bkgfname, tmpl, 'GRADS', nymd=nymd, nhms=nhms, stat=status)
   endif

   if (trim(outfname) == "NULL") then
   call ESMF_ConfigGetAttribute( CF, tmpl,  label ='OUTPUT_FILE:', __RC__ )
   call StrTemplate ( outfname, tmpl, 'GRADS', nymd=nymd, nhms=nhms, stat=status)
   endif

   thistime(1) =     nymd/10000
   thistime(2) = mod(nymd,10000)/100
   thistime(3) = mod(nymd,100)
   thistime(4) =     nhms/10000
   thistime(5) = mod(nhms,10000)/100
   thistime(6) = mod(nhms,100)

   call ESMF_ConfigGetAttribute( CF, sclinc, label ='SCLINC:', default=1.0, rc=status )

!  get dims
!  --------
   call getdim_ ( bkgfname, IM_BKG, JM_BKG, LM_BKG, idum, status )

!  define vertical grid (should be put in the grid ...)
!  ----------------------------------------------------
   allocate(ak(LM_IAU+1),bk(LM_IAU+1))
   call DefVertGrid_(CF,ak,bk,lm_iau)
   allocate(levels(LM_IAU))
   call hermes_levels_(levels)
   if (trim(covweights)/='NULL') then
      if(LM_IAU/=LM_BKG) then
        call mp_die(myname_,' diff vertical grids not supported ', 99)
      endif
      allocate(beta_weight(LM_IAU))
      call read_covweights_ (covweights, covoption, beta_weight)
      overwrite_with_gsibetas=.true.
   endif

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

   if(MAPL_AM_I_ROOT()) print *,trim(Iam),': Vert-grid parameters '

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

  function BundleClone(Bundle_old,grid_new,rc) result(Bundle_new)

!   Todling: stolen from MAPL, Ben Auer

    type(ESMF_FieldBundle), intent(inout) :: Bundle_old
    type(ESMF_Grid),        intent(inout) :: grid_new
 
    type(ESMF_FieldBundle)                :: Bundle_new
    integer, optional,      intent(out  ) :: rc

    integer :: status
    character(LEN=ESMF_MAXSTR) :: Iam

    integer :: bcount, i
    type(ESMF_Field) :: field
    type(ESMF_Field) :: field_old
    character(LEN=ESMF_MAXSTR) :: FieldName
    integer :: dims_orig

    Iam = "BundleClone"        


    call ESMF_FieldBundleGet(bundle_old,fieldCount=bcount,rc=status)
    _VERIFY(STATUS)

    bundle_new = ESMF_FieldBundleCreate(name="newBundle",rc=status)
    _VERIFY(STATUS)
    call ESMF_FieldBundleSet(bundle_new,grid=grid_new,__RC__)

    do i=1,bcount
       ! get info about original fields in bundle
       call ESMF_FieldBundleGet(bundle_old,i,field=field_old,rc=status)
       _VERIFY(STATUS)
       call ESMF_FieldGet(field_old,name=FieldName,rc=status)
       _VERIFY(STATUS)

       call ESMF_AttributeGet(field_old,NAME='DIMS',value=dims_orig,rc=status)
       _VERIFY(STATUS)
       field = MAPL_FieldCreate(field_old,grid_new,rc=status)
       _VERIFY(STATUS)
       call ESMF_AttributeSet(field,NAME='DIMS',value=dims_orig,rc=status)
       _VERIFY(STATUS)
       call MAPL_FieldBundleAdd(bundle_new,field,__RC__)

    end do

    _RETURN(ESMF_SUCCESS)

  end function BundleClone

  subroutine ll2cc_ (InpBundle,OutBundle,ll2cc)

   use ESMFL_Mod, only: ESMFL_State2Bundle
   use ESMFL_Mod, only: ESMFL_Bundle2State
   use ESMFL_Mod, only: ESMFL_Regrid
   use ESMFL_Mod, only: ESMFL_FieldRegrid

   use m_mpif90, only: MP_TYPE,MP_SUM

   implicit none

   type(ESMF_FieldBundle)  :: InpBundle ! Input  Bundle
   type(ESMF_FieldBundle)  :: OutBundle ! Output Bundle
   logical, intent(in) :: ll2cc ! when .t. does l2c; when .f. does c2l

   type(MAPL_SimpleBundle)  :: BKGBase
   type(MAPL_SimpleBundle)  :: GCMStub
   type(CubedSphereGridFactory) :: cs_factory
   type(latLonGridFactory) :: ll_factory
   class(AbstractRegridder), pointer :: L2C => null()
   class(AbstractRegridder), pointer :: C2L => null()
   
   integer ii,jj,ju,jv,ifld,rank,kk
   integer nfld,n2d,n3d
   real, allocatable, dimension(:,:,:) :: aux,aux2
   real, allocatable, dimension(:,:,:) :: ubkg,vbkg
   real, pointer, dimension(:,:)  :: ptr2d=>NULL()
   real, pointer, dimension(:,:,:):: ptr3d=>NULL()
   logical wind

   bkgbase = MAPL_SimpleBundleCreate ( InpBundle, __RC__ )
   gcmstub = MAPL_SimpleBundleCreate ( OutBundle, __RC__ )

!  Create transform from cubed to lat-lon
!  --------------------------------------
   if (ll2cc) then
      L2C => new_regridder_manager%make_regridder(BKGGrid, GCMGrid, REGRID_METHOD_BILINEAR,__RC__)
   else
      C2L => new_regridder_manager%make_regridder(GCMGrid, BKGGrid, REGRID_METHOD_BILINEAR,__RC__)
   endif
   call ESMF_FieldBundleGet(InpBundle,FieldCount=nfld, __RC__ )
   n2d=0;n3d=0
   DO ifld=1,nfld
      call ESMF_FieldBundleGet(InpBundle, ifld, Field, __RC__ )
      call ESMF_FieldGet(Field, NAME=NAME, dimCount=rank, __RC__ )
!     if (.not. check_list_(NAME,ovars)) cycle
      if(rank==2) then
         n2d=n2d+1
!        if(allocated(names2d)) names2d(n2d)=trim(NAME)
         ii = MAPL_SimpleBundleGetIndex ( bkgbase, NAME, 2, __RC__ )
         jj = MAPL_SimpleBundleGetIndex ( gcmstub, NAME, 2, __RC__ )
         allocate(aux (size(bkgbase%r2(ii)%q,1),size(bkgbase%r2(ii)%q,2),1))
         allocate(aux2(size(gcmstub%r2(jj)%q,1),size(gcmstub%r2(jj)%q,2),1))
         !2d interface to HorzT seems buggy
         !call MAPL_HorzTransformRun (L2C, bkgbase%r2(ii)%q, gcmstub%r2(jj)%q, __RC__ )
         aux (:,:,1) = bkgbase%r2(ii)%q
         if (ll2cc) then
            call L2C%regrid(aux, aux2, __RC__ )
         else
            call C2L%regrid(aux, aux2, __RC__ )
         endif
         if (overwrite_with_gsibetas) then
            kk=size(beta_weight)
            aux2(:,:,1) = beta_weight(kk)
         endif
         gcmstub%r2(jj)%q = sclinc * aux2(:,:,1)
         deallocate(aux2)
         deallocate(aux )
      endif
      if(rank==3) then
         n3d=n3d+1
!        if(allocated(names3d)) names3d(n3d)=trim(NAME)
         ii = MAPL_SimpleBundleGetIndex ( bkgbase, NAME, 3, __RC__ )
         jj = MAPL_SimpleBundleGetIndex ( gcmstub, NAME, 3, __RC__ )
         wind = trim(NAME) == 'u' .or. trim(NAME) == 'v'
         if (proper_winds==1 .and. wind) then
            if ( trim(NAME) == 'u' ) then
                allocate(ubkg(size(bkgbase%r3(ii)%q,1),size(bkgbase%r3(ii)%q,2),size(bkgbase%r3(ii)%q,3)))
                ubkg = bkgbase%r3(ii)%q
            endif
            if ( trim(NAME) == 'v' ) then
                allocate(vbkg(size(bkgbase%r3(ii)%q,1),size(bkgbase%r3(ii)%q,2),size(bkgbase%r3(ii)%q,3)))
                vbkg = bkgbase%r3(ii)%q
            endif
         else
            if (ll2cc) then
               call L2C%regrid(bkgbase%r3(ii)%q, gcmstub%r3(jj)%q, __RC__ )
               if (overwrite_with_gsibetas) then
                  do kk=1,size(gcmstub%r3(jj)%q,3)
                     gcmstub%r3(jj)%q(:,:,kk) = beta_weight(kk)
                  enddo
               endif
            else
               call C2L%regrid(bkgbase%r3(ii)%q, gcmstub%r3(jj)%q, __RC__ )
            endif
         endif
      endif
   END DO
 
   if (proper_winds==1) then
       if (allocated(ubkg) .and. allocated(vbkg) ) then
          ju = MAPL_SimpleBundleGetIndex ( gcmstub, 'u', 3, __RC__ )
          jv = MAPL_SimpleBundleGetIndex ( gcmstub, 'v', 3, __RC__ )
          if (ll2cc) then
             call L2C%regrid(ubkg,vbkg,gcmstub%r3(ju)%q,gcmstub%r3(jv)%q,__RC__)
             if (overwrite_with_gsibetas) then
                do kk=1,size(gcmstub%r3(ju)%q,3)
                   gcmstub%r3(ju)%q(:,:,kk) = beta_weight(kk)
                   gcmstub%r3(jv)%q(:,:,kk) = beta_weight(kk)
                enddo
             endif
          else
             call C2L%regrid(ubkg,vbkg,gcmstub%r3(ju)%q,gcmstub%r3(jv)%q,__RC__)
          endif
          deallocate(vbkg)
          deallocate(ubkg)
       else
          print*, "Arrays not associated, aborting ..."
          rc = 99
          call ESMF_Finalize(__RC__)
       endif
  endif

  end subroutine ll2cc_

  subroutine usage_

  implicit none

  if( MAPL_AM_I_ROOT() ) then
       write(6,*)
       write(6,'(a)') 'Usage: dyn2cubed.x nymd nhms '
       write(6,*)
  endif
  end subroutine usage_

  subroutine read_covweights_ (fname, which, weight)
    character(len=*), intent(in) :: fname
    character(len=*), intent(in) :: which
    real(8), intent(inout):: weight(:)
    character(len=*), parameter :: myname_=myname//'*read_covweights_'
    integer :: lu,lm,istat,k
    integer :: nlevs
    real :: dummy
    real(8), allocatable:: beta_s(:), beta_e(:)
    lm = size(beta_weight)
    allocate(beta_s(lm),beta_e(lm))
    lu = luavail()
    open(lu,file=trim(fname),form='formatted')
    rewind(lu)
    read(lu,'(i4)',iostat=istat) nlevs
    if ( istat /= 0 ) then
       call mp_die(myname_,': Error reading covariance/weights',97)
    endif
    if ( nlevs /= lm ) then
       close(lu)
       call mp_die(myname_,': Error reading covariance/weights, dims unmatched',98)
    endif
    do k = 1,nlevs
       read(lu,'(F8.1,3x,F8.3,F8.4,3x,F8.4)') dummy, dummy, beta_s(k), beta_e(k)
    enddo
    close(lu)
!   assume readin levels are in GSI orientation: flip to GEOS orientation
    beta_s = beta_s(lm:1:-1)
    beta_e = beta_e(lm:1:-1)
    if (trim(which)=='berror_clim') then
       weight = beta_s
    else if (trim(which)=='berror_ens') then
       weight = beta_e
    else if (trim(which)=='berror_tropo') then ! for testing only
       weight = 0.0d0
       weight(lm/2:lm) = 1.0d0
    else
       call mp_die(myname_,': Unknown covariance formulation option',99)
    endif
    if ( MAPL_am_I_root() ) then
       print *
       write(6,'(a)') '*******************************************'
       write(6,'(a)') '  Fields will be overwritten with weights  '
       write(6,'(a)') '*******************************************'
       print *
       write(6,'(a)') '  beta_s  beta_e  ref-pres'
       do k=1,LM_IAU
          write(6,'(F8.4,3x,F8.4,F10.4)') beta_s(k), beta_e(k), levels(k)
       enddo
    endif
    deallocate(beta_s,beta_e)
  end subroutine read_covweights_

end Program dyn2cubed
