! updAincWpert.x - ESMF/MAPL application to calculate and write out IAU increment
!
! 1. Read analysis increment file
! 2. Read evolved perturbation
! 3. Update (overwrite) analysis increment with evolved perturbation 
!
! REMARKS: 
!   a) This program only exists because there are some inconsistencies
!      in the perturbations written out by the TLM as compared with the
!      analysis increment written out by GSI. This program tries to tackle
!      these inconsistencies and write out a file similar to the analysis
!      increment so everything else in the DAS can proceed smoothly.
!
! Ricardo Todling, May 2016
!............................................................................
!  !REVISION_HISTORY:  
!   08May2016  Todling  Initial version
!----------------------------------------------------------------------------

#  include "MAPL_Generic.h"

   Program updAincWpert

   use ESMF
   use MAPL_Mod
   use MAPL_CFIOMod
   use m_ioutil, only: luavail
   use m_StrTemplate, only: StrTemplate

   implicit NONE

!  Basic ESMF objects being used in this example
!  ---------------------------------------------
   type(ESMF_Grid)         :: AINCgrid   ! analysis increment Grid
   type(ESMF_FieldBundle)  :: AIncBundle ! Bundle to analysis increment
   type(ESMF_FieldBundle)  :: PertBundle ! Bundle to TLM perturbation
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
   type(MAPL_MetaComp) :: MAPLOBJ

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
   integer :: im_inc, jm_inc, lm_inc   ! Ainc Grid dimensions

   integer :: comm
   logical :: SDFoutput,OIFoutput
   logical :: sameres,cubed
   real    :: sclinc
!
   character(len=ESMF_MAXSTR) :: sdf_ofname
!
   character(len=ESMF_MAXSTR) :: aincfname
   character(len=ESMF_MAXSTR) :: pertfname

   character(len=ESMF_MAXSTR) :: recalc_ps

!  Coordinate variables
!  --------------------
   character(len=ESMF_MAXSTR)    :: name

   character(len=*), parameter :: Iam = 'updAincWpert'
   character(len=*), parameter :: myRC= 'updAincWpert.rc'

!                             -----
    
    call Main()

CONTAINS

    subroutine Main()

    character(len=30) AINCGRIDNAME

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
          print *, 'Try:  mpirun -np ', Nx*Ny, ' updAincWpert.x'
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


!   Create a regular Lat/Lon grid over which ANA-INC defined
!   --------------------------------------------------------
    call mapl_defgridname (IM_INC,JM_INC,AINCGRIDNAME,MAPL_am_I_root())
    AINCGrid = grid_manager%make_grid(LatLonGridFactory(grid_name=trim(AINCGRIDNAME), nx=nx, ny=ny, &
               im_world=im_inc, jm_world=jm_inc, lm=lm_inc, &
               pole='PC',dateline='DC', rc=status))
    _VERIFY(status)

!   Validate grid
!   -------------
    call ESMF_GridValidate(AINCgrid,__RC__)

!   Create a clock
!   --------------
    CLOCK = ESMF_ClockCreate ( name="INCClock", timeStep=TimeStep, startTime=Time, __RC__ )

!   Create bundle to hold analysis increment and read increment
!   -----------------------------------------------------------
    AIncBundle = ESMF_FieldBundleCreate ( name='AINC bundle', __RC__ )
    call ESMF_FieldBundleSet ( AIncBundle, grid=AINCgrid, __RC__ )

    call MAPL_CFIORead  ( aincfname, Time, AINCBundle, &
                          TIME_IS_CYCLIC=.false., verbose=.true., __RC__ )

!   Create bundle to hold TLM perturbation and read perturbation
!   ------------------------------------------------------------
    PertBundle = ESMF_FieldBundleCreate ( name='PERT bundle', __RC__ )
    call ESMF_FieldBundleSet ( PertBundle, grid=AINCgrid, __RC__ )

    call MAPL_CFIORead  ( pertfname, Time, PERTBundle, &
                          TIME_IS_CYCLIC=.false., verbose=.true., __RC__ )

!   Update fields
!   -------------
    call upd_()

!   Write out evolved analysis increment 
!   ------------------------------------
    call writeout_()

!   All done
!   --------
    if (MAPL_AM_I_ROOT()) then
          close(999)
          open (999,file='UPDAINC_EGRESS',form='formatted')
          close(999)
    end if
    call final_
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

   call ESMF_ConfigGetAttribute( CF, nymd, label ='AINC_DATE:', __RC__ )
   call ESMF_ConfigGetAttribute( CF, nhms, label ='AINC_TIME:', __RC__ )

   call ESMF_ConfigGetAttribute( CF, tmpl,  label ='AINC_FILENAME:', __RC__ )
   call StrTemplate ( aincfname, tmpl, 'GRADS', nymd=nymd, nhms=nhms, stat=status)

   call ESMF_ConfigGetAttribute( CF, tmpl,  label ='PERT_FILENAME:', __RC__ )
   call StrTemplate ( pertfname, tmpl, 'GRADS', nymd=nymd, nhms=nhms, stat=status)

   call ESMF_ConfigGetAttribute( CF, recalc_ps,  label ='RECALCULATE_PS:', default='NO', __RC__ )

   thistime(1) =     nymd/10000
   thistime(2) = mod(nymd,10000)/100
   thistime(3) = mod(nymd,100)
   thistime(4) =     nhms/10000
   thistime(5) = mod(nhms,10000)/100
   thistime(6) = mod(nhms,100)

   call ESMF_ConfigGetAttribute( CF, sdf_ofname, label ='UPD_FILENAME:', default='NONE', __RC__ )
   if (trim(sdf_ofname)=="NONE") then
       SDFoutput = .false.
   else
       SDFoutput = .true.
   endif

!  get dims
!  --------
   call getdim_ ( aincfname, IM_INC, JM_INC, LM_INC, idum, status )

!  Set ESMF date/time
!  ------------------
   call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN )
   call ESMF_TimeSet(Time, yy=thistime(1), mm=thistime(2), dd=thistime(3), &
                            h=thistime(4), m =thistime(5),  s=thistime(6))
   call ESMF_TimeIntervalSet( TimeStep, h=6, m=0, s=0, __RC__ )

   end subroutine init_


   subroutine upd_ ()
   
   use MAPL_SimpleBundleMod, only: MAPL_SimpleBundleCreate
   use MAPL_SimpleBundleMod, only: MAPL_SimpleBundleGetIndex
   use MAPL_SimpleBundleMod, only: MAPL_SimpleBundle
   use MAPL_SimpleBundleMod, only: MAPL_SimpleBundlePrint

   implicit none

   type(MAPL_SimpleBundle) :: ainc
   type(MAPL_SimpleBundle) :: pert

   real, pointer ::  ptr2d_ainc(:,:)
   real, pointer ::  ptr2d_pert(:,:)
   real, pointer ::  ptr3d_ainc(:,:,:)
   real, pointer ::  ptr3d_pert(:,:,:)

   real, pointer ::  ps_ainc(:,:)
   real, pointer ::  ts_ainc(:,:)
   real, pointer ::   u_ainc(:,:,:)
   real, pointer ::   v_ainc(:,:,:)
   real, pointer ::  tv_ainc(:,:,:)
   real, pointer ::  qv_ainc(:,:,:)
   real, pointer ::  o3_ainc(:,:,:)

!  only ps is taken from pert ...
   character(len=*), parameter :: vars2d(1) = (/'ps   '/)
!  all meteorology and water from pert ...
   character(len=*), parameter :: vars3d(8) = (/'u    ', 'v    ', 'tv   ', &
                                                'delp ', 'sphu ', 'ozone', &
                                                'qitot', 'qltot' /)     
   integer ia, ip, k, nf, nfld

!  Link background Bundle to Simple-Bundle
!  ---------------------------------------
   ainc = MAPL_SimpleBundleCreate ( AIncBundle, __RC__ )
   pert = MAPL_SimpleBundleCreate ( PertBundle, __RC__ )

!  Take care of 2d-fields
!  ----------------------
   if (trim(recalc_ps)=="NO" .or. recalc_ps=='no') then
      do nf = 1, size(vars2d)
         ia  = MAPL_SimpleBundleGetIndex ( ainc, trim(vars2d(nf)),  2, __RC__ )
         ip  = MAPL_SimpleBundleGetIndex ( pert, trim(vars2d(nf)),  2, __RC__ )
         ainc%r2(ia)%q = pert%r2(ip)%q
      enddo
   endif

!  Take care of 3d-fields
!  ----------------------
   do nf = 1, size(vars3d)
      ia  = MAPL_SimpleBundleGetIndex ( ainc, trim(vars3d(nf)),  3, __RC__ )
      ip  = MAPL_SimpleBundleGetIndex ( pert, trim(vars3d(nf)),  3, __RC__ )
      ainc%r3(ia)%q = pert%r3(ip)%q
      if (trim(vars3d(nf))=='delp') then
         if (trim(recalc_ps)=="YES" .or. recalc_ps=='yes') then
            ia = MAPL_SimpleBundleGetIndex ( ainc, 'ps', 2, __RC__ )
            ainc%r2(ia)%q = 0.0
            do k = 1, size(pert%r3(ip)%q,3)
               ainc%r2(ia)%q = ainc%r2(ia)%q + pert%r3(ip)%q(:,:,k)
            enddo
         endif
      endif
   enddo

!  Clean up

   end subroutine upd_
#ifdef SOON
#endif /* SOON */

   subroutine writeout_

   character(len=ESMF_MAXSTR) :: ofname
   real, pointer, dimension(:,:)   :: ptr2d
   real, pointer, dimension(:,:,:) :: ptr3d

   if (SDFoutput) then

!      Write out increment as SDF output
!      ---------------------------------
#ifdef SOON
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
#endif /* SOON */
       call StrTemplate ( ofname, trim(sdf_ofname), 'GRADS', nymd=nymd, nhms=nhms, stat=status )
       VERIFY_(status)
       call MAPL_CFIOCreate ( CFIO, trim(ofname), CLOCK, AINCBundle,  &
!!                            FREQUENCY=freq, &
!                             LEVELS=levels, &
                              DESCR='Evolved Analysis Increment', __RC__ )
       call MAPL_CFIOWrite ( CFIO, CLOCK, IOBundle )
       call MAPL_cfioDestroy ( CFIO )

    endif ! <SDFoutput>

   end subroutine writeout_

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

   subroutine final_
    call ESMF_FieldBundleDestroy (PERTBundle, __RC__)
    call ESMF_FieldBundleDestroy (AINCBundle, __RC__)
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

end Program updAincWpert
