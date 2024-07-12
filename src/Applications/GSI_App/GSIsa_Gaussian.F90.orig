! *********************************************************************
! *****         GSI gridded component stand alone driver           ****
! *****                   Gaussian grid version                    ****
!  08Jun2008  Todling      Update to latest MAPL 
!
! *********************************************************************

#include "MAPL_ErrLogMain.h"

   Program GSIsa_Gaussian

   use ESMF  
   use MAPL_Mod
   use MAPL_GenericMod
   use MAPL_CFIOMod
   use GEOSaana_GridCompMod, only: SetServices   
   use m_StrTemplate      
 
   implicit NONE

   integer             :: rc

   type (ESMF_VM)      :: VM            ! virtual machine
   type(ESMF_DELayout) :: layout        
   type(ESMF_GridComp) :: gcAANA        ! AANA gridded component
   type(ESMF_State)    :: expAANA       ! AANA export state (GEOS imp)
   type(ESMF_State)    :: impAANA       !  "   import  "    ( "   exp)
   type(ESMF_Grid)     :: gridAANA      ! AANA grid
   type(ESMF_Alarm)    :: Alarm         ! Final End Time alarm
   type(ESMF_Clock)    :: Clock         
   type(ESMF_Time)     :: currT   
   type(ESMF_Config)   :: cf
   integer             :: i, numBKGs, BKGinterval, gridtype
   integer             :: status
   integer, pointer, dimension(:) :: hrBKGs
   character(len=ESMF_MAXSTR) :: dynfile, sfcfile
   character(len=*), parameter :: Iam = 'GSIsa_Gaussian'

! start

!  Initialize framework
!  --------------------
   call ESMF_Initialize (vm=vm, lognone=0, rc=status) 
   VERIFY_(status)

!  Get the global vm 
!  -----------------
   call ESMF_VMGetGlobal(vm, rc=status) 
   VERIFY_(status)

!  Create config and Initialize Clock 
!  ----------------------------------
   cf = ESMF_ConfigCreate (rc=status)
   VERIFY_(STATUS)
   call ESMF_ConfigLoadFile   ( cf,'GSI_GridComp.rc',rc=status )
   VERIFY_(STATUS)

   call ESMF_ConfigGetAttribute( CF, GridType, label ='GSIGridType:', rc = STATUS )
   VERIFY_(STATUS)

   CALL ESMF_ConfigGetAttribute(cf, numBKGs,   label = 'nfldsig:',   &
        rc=status); VERIFY_(status)
   allocate(hrBKGs(1:numBKGs), stat=status)
   VERIFY_(status)
   CALL ESMF_ConfigFindLabel(cf, label = 'hrdifsig:', rc = status)
   VERIFY_(status)
   DO i = 1, numBKGs
      CALL ESMF_ConfigGetAttribute(cf, hrBKGs(i), rc = status)
      VERIFY_(status)
   END DO
   if(numBKGs>1) then 
      BKGinterval = hrBKGs(2)-hrBKGs(1)
   else
      BKGinterval = 6
   end if

   call Clock_Init ( cf,clock,Alarm,'GSIsa','test',rc=status )
   VERIFY_(STATUS)

!  Create AANA grid (which is the same as the GSI gaussian grid)
!  ------------------------------------------------------------
   gridAANA = AANAGridCreate ( vm, cf, rc=status )
   VERIFY_(status)

!  States
!  ------ 
   expAANA = ESMF_StateCreate (statename="AANAaana_Export",      &
        statetype = ESMF_STATE_EXPORT, &
        RC=STATUS )
   VERIFY_(STATUS)
   impAANA = ESMF_StateCreate (statename="AANAaana_Import",      &       
        statetype = ESMF_STATE_IMPORT, & 
        RC=STATUS ) 
   VERIFY_(STATUS)

!  Create component
!  ----------------
   gcAANA = ESMF_GridCompCreate ( vm, 'AANAaana', &
        configfile="GSI_GridComp.rc", &
        grid=gridAANA, &
        gridcompType=ESMF_ATM, &
        rc=status)

! Make sure aana G.C. gets its grid

   call ESMF_GridCompSet(gcAANA, grid=gridAANA, rc=STATUS)
   VERIFY_(STATUS)

!  Register component
!  ------------------
   call ESMF_GridCompSetServices ( gcAANA, SetServices, rc )

!  Init component
!  --------------
   call ESMF_GridCompInitialize ( gcAANA, impAANA, expAANA, clock, &
        ESMF_SINGLEPHASE, rc=status )
   VERIFY_(status)

!  Advance the clock 1 timestep to set proper "ringing" state for all alarms
!  -------------------------------------------------------------------------
   call ESMF_ClockAdvance ( clock = clock, rc=status )
   VERIFY_(status)

!  populate states
!  --------------- 
   dynfile = 'sigf06.gaus'
   sfcfile = 'sfcf06'
   call populate_states(dynfile, sfcfile,&
                        gridAANA, impAANA, expAANA, rc=status)
   VERIFY_(status)

!   Run component
!   -------------
   call ESMF_GridCompRun ( gcAANA, impAANA, expAANA, clock, rc=status )

!   Finalz component
!   ----------------
   call ESMF_GridCompFinalize ( gcAANA, impAANA, expAANA, clock, rc=status )

!  create GRADS binary files
!  -------------------------
! Note: This requires that MAPL_ESMFStateWriteToFile be public in 
! MAPL_Generic, which generally isn't.
   call MAPL_ESMFStateWriteToFile(impAANA,clock,'bkg.dat',&
        'BINARY',rc=status)
   VERIFY_(status)  
   call MAPL_ESMFStateWriteToFile(expAANA,clock,'ana.dat',&
        'BINARY',rc=status)
   VERIFY_(status)  

!  Destroy Objects
!  ---------------
   call ESMF_GridCompDestroy ( gcAANA, rc=status )
   VERIFY_(STATUS)
   call ESMF_StateDestroy ( impAANA, rc=status )
   VERIFY_(STATUS)
   call ESMF_StateDestroy ( expAANA, rc=status )
   VERIFY_(STATUS)
   call ESMF_ClockDestroy(clock, rc=status )
   VERIFY_(STATUS)

!   All done
!   -------- 
   call ESMF_Finalize()
   VERIFY_(STATUS)

#include "MAPL_ErrLog.h"

   contains

!-------------------------------------------------------------------
   subroutine populate_states(dynfile, sfcfile, &
                              gridAANA, impAANA, expAANA,&
                              rc)
!-------------------------------------------------------------------
   type(ESMF_Grid)  , intent(in)  :: gridAANA
   type(ESMF_State)  , intent(inout) :: impAANA,expAANA
   integer, optional, intent(out)  :: rc
   character(len=ESMF_MAXSTR), intent(in) :: dynfile, sfcfile
!
   type(ESMF_Bundle)   :: GEOSbundle1   ! bundle to hold GEOS export UPA+SFC fields
   type(ESMF_Bundle)   :: GEOSdynBUN2   !  "          "       import UPA   "
   integer :: status
   integer, parameter :: nIMPflds = 16
   integer, parameter :: nEXPflds = 7
   character(len=*), parameter :: Iam = 'populate_states'

! start

!  Create empty bundles to hold GEOS fields
!  Note: GEOS export = AANA import = GSI import
!        GEOS import = AANA export = GSI export
!  --------------------------------------------

   GEOSbundle1 = ESMF_BundleCreate ( name='AANA imp dyn+sfc bundle', &
        grid=gridAANA, rc=status )
   VERIFY_(status)
   GEOSdynBUN2 = ESMF_BundleCreate ( name='AANA exp dyn bundle', &
        grid=gridAANA, rc=status )
   VERIFY_(status)

   call FillBundle(GEOSbundle1, nIMPflds, 1, rc=status)
   VERIFY_(status)

!   We may want to do the following - instead of ReadFirstGuess
!   UNIT = GETFILE(trim(dynfile), form='unformatted', rc=status)
!   VERIFY_(STATUS)
!   call MAPL_VarRead(unit,GEOSbundle1, rc=status)
!   VERIFY_(status)
 
   call FillBundle(GEOSdynBUN2, nEXPflds, 2, rc=status)
   VERIFY_(status)

   call ESMFL_Bundle2State (GEOSbundle1, impAANA)
   call ESMFL_Bundle2State (GEOSdynBUN2, expAANA)  

   call ReadFirstGuess (dynfile, sfcfile, impAANA)

   call ESMF_BundleDestroy ( GEOSdynBUN2, rc=STATUS )
   VERIFY_(STATUS)
   call ESMF_BundleDestroy ( GEOSbundle1, rc=STATUS )
   VERIFY_(STATUS)


   RETURN_(ESMF_SUCCESS)

   end subroutine populate_states
 
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
   integer        ::  SGMT_TIME(6), JOB_DATE(2)
   integer        ::  CURR_TIME(6)
   integer        ::   JOB_SGMT(6)

   integer                       :: ref_date
   integer                       :: ref_time
   type(ESMF_Time)               :: RefTime, RingTime
   type(ESMF_TimeInterval)       :: Frequency


! Read Clock Resource File
! ------------------------
   call ESMF_ConfigGetAttribute( config, BEG_DATE, 2, label='BEG_DATE:', rc=status )
   VERIFY_(STATUS)
   call ESMF_ConfigGetAttribute( config, END_DATE, 2, label='END_DATE:', rc=status )
   VERIFY_(STATUS)

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

   if( MAPL_AM_I_ROOT() ) then
      print *
      print *, 'ExpID: ',trim(expid)
      print *, 'Descr: ',trim(expdsc)
      print *
      write(*,101) 'Initial Start_Time = ', beg_date
      write(*,101) '  Final  Stop_Time = ', end_date
      write(*,101) '  Current Job_Sgmt = ', job_date
      write(*,102) '  BKGinterval(hrs) = ', BKGinterval
      print *
101      format(a,i8.8,1x,i6.6)
102      format(a,i4)
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
   call ESMF_CalendarSetDefault(ESMF_CAL_GREGORIAN, RC=STATUS)
   VERIFY_(STATUS)

   cal = ESMF_CalendarCreate( "GregorianCalendar",ESMF_CAL_GREGORIAN,rc=status )
   VERIFY_(STATUS)

! initialize start time for Alarm frequencies
! -------------------------------------------
   call ESMF_TimeSet( StartTime, YY = START_TIME(1), &
        MM = START_TIME(2), &
        DD = START_TIME(3), &
        H = START_TIME(4), &
        M = START_TIME(5), &
        S = START_TIME(6), calendar=cal, rc=status )
   VERIFY_(STATUS)

! initialize final stop time
! --------------------------
   call ESMF_TimeSet(  StopTime, YY =  STOP_TIME(1), &
        MM =  STOP_TIME(2), &
        DD =  STOP_TIME(3), &
        H =  STOP_TIME(4), &
        M =  STOP_TIME(5), &
        S =  STOP_TIME(6), calendar=cal, rc=status )
   VERIFY_(STATUS)

! initialize current time
! -----------------------
   call ESMF_TimeSet(  CurrTime, YY =  CURR_TIME(1), &
        MM =  CURR_TIME(2), &
        DD =  CURR_TIME(3), &
        
        H =  CURR_TIME(4), &
        M =  CURR_TIME(5), &
        S =  CURR_TIME(6), calendar=cal, rc=status )
   VERIFY_(STATUS)
   
! initialize model time step
! --------------------------
   call ESMF_TimeIntervalSet( timeStep, h=BKGinterval, m=0, s=0, rc=status ) 
   VERIFY_(STATUS)

! Back-off StartTime and CurrTime by One TimeStep. After Initialize we'll advance the clock 1 timestep to set the  
! proper ringing state of all alarms 

   StartTime = StartTime - TimeStep
   CurrTime =  CurrTime - TimeStep

! initialize the clock with the above values
! ------------------------------------------
   clock = ESMF_ClockCreate( "ApplClock",timeStep,StartTime,StopTime, rc=status )
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

    type (ESMF_VM),    intent(IN   ) :: VM
    type(ESMF_Config), intent(INOUT) :: cf
    integer, optional, intent(OUT)   :: rc
    type (ESMF_Grid)                 :: grid

! Local vars
    integer                         :: status
    type (ESMF_DELayout)            :: layout
    integer                         :: nlon,nlat,nsig
    integer                         :: jmax, jb, je
    integer                         :: i1, i, j, k
    integer                         :: NX, NY
    integer, allocatable            :: IMXY(:), JMXY(:)
    character(len=ESMF_MAXSTR)      :: gridname
    real(ESMF_KIND_R8)              :: minCoord(3),deltaZ
    real(ESMF_KIND_R8), allocatable :: deltaX(:), deltaY(:)
    real                            :: LON0, LAT0
    real                            :: pi, d2r, pih, dlon, dlat
    real(8),allocatable,dimension(:):: slat
    real(8),allocatable,dimension(:):: rlats   ! grid latitudes (radians)
    real(8),allocatable,dimension(:):: rlons   ! grid longitudes (radians)
    character(len=ESMF_MAXSTR), parameter   :: IAm='AANAGridCreate'

! start

   call ESMF_ConfigGetAttribute( cf, nlon, label ='GEOS IM:', rc = status )
   VERIFY_(status)
   call ESMF_ConfigGetAttribute( cf, nlat, label ='GEOS JM:', rc = status )
   VERIFY_(status)
   call ESMF_ConfigGetAttribute( cf, nsig,       label ='GEOS KM:', rc = status )
   VERIFY_(status)
   call ESMF_ConfigGetAttribute( cf, NX,       label ='NX:', rc = status )
   VERIFY_(status)
   call ESMF_ConfigGetAttribute( cf, NY,       label ='NY:', rc = status )
   VERIFY_(status)

    pi  = 4.0 * atan ( 1.0 ) 
   pih  = pi/2.0
   d2r  = pi / 180.

! Query start longitude and latitude

   call ESMF_ConfigGetAttribute( CF, LON0, label ='ORIGIN_CENTER_LON:', rc = STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigGetAttribute( CF, LAT0, label ='ORIGIN_CENTER_LAT:', rc = STATUS )
   VERIFY_(STATUS)

   lon0 = lon0 * d2r
   lat0 = lat0 * d2r
   dlon=(pi+pi)/nlon	! in radians
   dlat=pi/(nlat-1)

   allocate(deltaX(nlon), deltaY(nlat), stat=STATUS)
   VERIFY_(STATUS)

! Get the IMXY vector
! -------------------
    allocate( imxy(0:nx-1) )
    call MAPL_DecomposeDim ( nlon, imxy, nx )

! Get the JMXY vector
! -------------------
    allocate( jmxy(0:ny-1) )   
    call MAPL_DecomposeDim ( nlat, jmxy, ny )

! Set Gaussian grid lon/lat arrays used by GSI.

   jmax=nlat-2
   jb=1
   je=(jmax+1)/2
   allocate( slat(jmax), stat=STATUS)
   
   call gauss_lat_nmc (slat,jmax)

   allocate(rlats(nlat),rlons(nlon), stat=STATUS)
   VERIFY_(STATUS)  
   do i=1,nlon
      rlons(i)=float(i-1)*dlon
   end do
   do i=jb,je
      i1=i+1
      rlats(i1)=-asin(slat(i))
      i1=nlat-i
      rlats(i1)=asin(slat(i))
   end do
   rlats(1)=-pih
   rlats(nlat)=pih

! Set deltaX
   deltaX = 2.0*pi/nlon

! Set deltaY
   do j=1,nlat-1
      deltaY(j)=rlats(j+1)-rlats(j)
   end do
   deltaY(nlat) = rlats(nlat)-rlats(nlat-1)

   deltaZ = 1.0
 
! Re-Define South-West Corner of First Grid-Box
! ---------------------------------------------
!
!   NW---------NE
!   |           |
!   |     C     |
!   |           |
!   SW---------SE
!
! In ESMF the minimum coordinate is defined as the SW corner of the first
! grid-box:
!

   minCoord(1) = lon0 - deltaX(1)/2 
   minCoord(2) = lat0 - deltaY(1)/2 

! Create Grid. The deltas define whether is is uniform or non-uniform

   grid = ESMF_GridCreateHorzLatLon( &
        minGlobalCoordPerDim=minCoord(1:2), &
        delta1=deltaX,                                                  &
	delta2=deltaY,                                                  &
        horzstagger=ESMF_GRID_HORZ_STAGGER_A,                           &
        periodic=(/ESMF_TRUE, ESMF_FALSE/),                             &
        name='GSI Gaussian Grid', rc=STATUS)
   VERIFY_(STATUS)

   call ESMF_GridAddVertHeight(grid,            &
        delta=(/(deltaZ, k=1,nsig) /),            &
        vertStagger=ESMF_GRID_VERT_STAGGER_TOP, &
        rc=STATUS)
   VERIFY_(STATUS)

! distribute grid
! ---------------

! create an nxpe*nype layout used to distribute the grid

   LAYOUT = ESMF_DELayoutCreate(vm, deCountList=(/nx, ny/), rc=STATUS)
   VERIFY_(STATUS)

   call ESMF_GridDistribute(grid,               &
        deLayout=LAYOUT,                        &
        countsPerDEDim1=imxy,                   &
        countsPerDEDim2=jmxy,                   &
        rc=STATUS) 

   deallocate(imxy, jmxy, stat=STATUS)
   VERIFY_(STATUS)
   deallocate(deltaX, deltaY, stat=STATUS)
   VERIFY_(STATUS)
   deallocate(rlons, rlats, slat, stat=STATUS)
   VERIFY_(STATUS)

   RETURN_(STATUS)
    
   end function AANAGridCreate

!----------------------------------------------------------------------
   subroutine ReadFirstGuess (dynfile, sfcfile, impSt)
!----------------------------------------------------------------------
! args
   type(ESMF_State), intent(inout) :: impSt  ! import state
   character(len=*) :: dynfile, sfcfile
! locals
   real    :: fhour4
   integer :: idate(4)
   integer :: status, k, nlon, nlat, deid, npe, isfc
   integer :: ITEMCOUNT, UNIT, UNIT2, J
   type (ESMF_DELayout) :: LAYOUT
   type (ESMF_Field) :: Efield
   type (ESMF_Array) :: Earray
   type (ESMF_VM)    :: vm
   type (ESMF_Grid)  :: grid
   type (ESMF_StateItemType), pointer    :: ITEMTYPES(:)
   character(len=ESMF_MAXSTR ), pointer  :: ITEMNAMES(:)
   character(len=*), parameter :: IAm = 'ReadFirstGuess'

! start

   call ESMF_VMGetGlobal(vm)
   call ESMF_VMGet(vm, petCount=npe, localPet=deid, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGet(impSt,ITEMCOUNT=ITEMCOUNT,RC=STATUS)
   VERIFY_(STATUS)
   if(deid==0) print *,' import state contains ',ITEMCOUNT,' items'

   allocate(ITEMNAMES(ITEMCOUNT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(ITEMTYPES(ITEMCOUNT),STAT=STATUS)
   VERIFY_(STATUS)

   call ESMF_StateGet(impSt,ITEMNAMELIST=ITEMNAMES,STATEITEMTYPELIST=ITEMTYPES,RC=STATUS)
   VERIFY_(STATUS)

! Open file
!----------

!!  only_vars='phis,ps,uwnd,vwnd,theta,sphu,ozone,qitot,qltot')

!I/O

   UNIT = GETFILE(trim(dynfile), form='unformatted', rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'phis', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status) !HS
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'ps', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'theta', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status) !PS3
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'theta', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status) !delp
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'theta', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'sphu', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'uwnd', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status) !RH
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'uwnd', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'vwnd', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'ozone', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status) !DIV
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'qitot', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status) !VOR
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'ozone', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'qltot', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, rc=status)
   VERIFY_(STATUS)

   call FREE_FILE(UNIT)

   isfc=1
   unit2 = GETFILE(trim(sfcfile), form='unformatted', rc=status)
   VERIFY_(STATUS)

   if(deid==0)then
   read (unit2)
   read (unit2) fhour4,idate
   print *,fhour4,idate
   end if

!!  only_vars='TSKIN,U10M,V10M,SNOWDP,GWETTOP,TSOIL1,ORO,TA,QA')

   call ESMF_StateGetField(impSt, 'TSKIN', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'GWETTOP', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'SNOWDP', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'TSOIL1', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'ORO', Efield, rc=status) !d1
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'ORO', Efield, rc=status) !d2
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)

   call ESMF_StateGetField(impSt, 'ORO', Efield, rc=status) !d3
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)
   
   call ESMF_StateGetField(impSt, 'ORO', Efield, rc=status) !d4
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)
   
   call ESMF_StateGetField(impSt, 'ORO', Efield, rc=status) !d5
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)
   
   call ESMF_StateGetField(impSt, 'ORO', Efield, rc=status) !d6
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)
   
   call ESMF_StateGetField(impSt, 'ORO', Efield, rc=status)
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)
   
   call ESMF_StateGetField(impSt, 'U10M', Efield, rc=status)  !VFRAC
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)
   
   call ESMF_StateGetField(impSt, 'V10M', Efield, rc=status) !d7
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)
   
   call ESMF_StateGetField(impSt, 'V10M', Efield, rc=status) !F10m
   VERIFY_(STATUS)
   call FieldRead (unit, Efield, isfc, rc=status)
   VERIFY_(STATUS)
   
   call FREE_FILE(unit2)
   
   deallocate(ITEMNAMES)
   deallocate(ITEMTYPES)
   
   end subroutine ReadFirstGuess
   
!----------------------------------------------------------------------
  subroutine VarRead_R4_2d(UNIT, GRID, A, sfc, RC)
!----------------------------------------------------------------------

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(IN   ) :: GRID
    real(kind=ESMF_KIND_R4)     , intent(  OUT) :: A(:,:)
    integer, optional, intent(in) :: sfc
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real,  allocatable :: VAR(:,:)
    real,  allocatable :: VARp2(:,:)
    integer                               :: IM_WORLD
    integer                               :: JM_WORLD
    integer                               :: status
    integer                               :: gridRank
    integer                               :: DIMS(ESMF_MAXGRIDDIM)
    type (ESMF_DELayout)                  :: layout
    character(len=ESMF_MAXSTR)            :: IAm='GEOS_VarRead_R4_2d'
   
   
    call ESMF_GridGet(GRID, dimCount=gridRank, rc=STATUS)
    VERIFY_(STATUS)
    if (gridRank == 3) then
       call ESMF_GridGet(GRID, horzRelLoc=ESMF_CELL_CENTER, &
            vertRelLoc=ESMF_CELL_CENTER, &
            globalCellCountPerDim=DIMS, RC=STATUS)
    else ! if (gridRank == 2)
       call ESMF_GridGet(GRID, horzRelLoc=ESMF_CELL_CENTER, &
            globalCellCountPerDim=DIMS, RC=STATUS)
    end if
    VERIFY_(STATUS)

    IM_WORLD = DIMS(1)
    JM_WORLD = DIMS(2)

    allocate(VAR(IM_WORLD,JM_WORLD), stat=status)
    VERIFY_(STATUS)
    if(present(sfc)) then
      allocate(VARp2(IM_WORLD,JM_WORLD-2), stat=status)
      VERIFY_(STATUS)
    end if

    call ESMF_GridGet(grid, delayout=layout, rc=status)
    VERIFY_(STATUS)

    if (MAPL_am_i_root(layout)) then
       if(present(sfc)) then
         read (UNIT, IOSTAT=status) VARp2
         print *,' READ 2D VARp2 = ',minval(VARp2),maxval(VARp2)
         VAR(:,2:JM_WORLD-1) = VARp2
         VAR (:,1) = sum(VARp2(:,JM_WORLD-2))/IM_WORLD
         VAR (:,JM_WORLD) = sum(VARp2(:,1))/IM_WORLD
       else
         read (UNIT, IOSTAT=status) VAR
         print *,' READ 2D VAR = ',minval(VAR),maxval(VAR)
       end if
       VERIFY_(STATUS)
    end if

    call ArrayScatter(A, VAR, grid, rc=status)
    VERIFY_(STATUS)

    deallocate(VAR)
    if(present(sfc)) then
      deallocate(VARp2)
    end if

  RETURN_(ESMF_SUCCESS)

  end subroutine VarRead_R4_2d

!----------------------------------------------------------------------
  subroutine VarRead_R4_3d(UNIT, GRID, A, RC)
!----------------------------------------------------------------------

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(IN   ) :: GRID
    real                        , intent(  OUT) :: A(:,:,:)
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: status
    character(len=ESMF_MAXSTR)            :: IAm='GEOS_VarRead_R4_3d'

    integer :: L

    do L = 1, size(A,3)
        call  VarRead_R4_2d(UNIT, GRID, A(:,:,L))
    end do

  RETURN_(ESMF_SUCCESS)
  end subroutine VarRead_R4_3d

!----------------------------------------------------------------------
  subroutine FieldRead(UNIT,FIELD, sfc, RC)
!----------------------------------------------------------------------
    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Field)           , intent(INOUT) :: field
    integer,           optional , intent(INOUT) :: sfc
    integer,           optional , intent(  OUT) :: RC

! Local vars
    type (ESMF_Array)                  :: array
    type (ESMF_DELayout)               :: layout
    type (ESMF_Grid)                   :: GRID
    type (ESMF_VM)                     :: VM
    integer                            :: rank
    integer                            :: status
    real, pointer, dimension(:)        :: var_1d
    real, pointer, dimension(:,:)      :: var_2d
    real, pointer, dimension(:,:,:)    :: var_3d
    real, pointer, dimension(:,:,:,:)  :: var_4d
    character(len=ESMF_MAXSTR)         :: FORMATTED
    integer                            :: count
    integer                            :: counts(ESMF_MAXDIM)
    integer                            :: dims
    integer                            :: deid
    character(len=ESMF_MAXSTR)         :: IAm='FieldRead'

    inquire(unit=UNIT, formatted=FORMATTED)
    
    call ESMF_FieldGet(field, grid, rc=status)
    VERIFY_(STATUS)
    call ESMF_GridGet(grid, deLayout=layout, rc=status)
    VERIFY_(STATUS)

    call ESMF_VMGetGlobal(vm)
    call ESMF_VMGet(vm, localPet=deid, rc=status)
    VERIFY_(STATUS)

    call ESMF_FieldGetAttribute(field, name='DIMS', value=DIMS, rc=status)
    VERIFY_(STATUS)
    call ESMF_FieldGetArray(field, array, rc=status)
    VERIFY_(STATUS)
    call ESMF_ArrayGet(array, rank, rc=status)
    VERIFY_(STATUS)
    if (rank == 2) then
       call ESMF_ArrayGetData(array, var_2d, ESMF_DATA_REF, rc=status)
       VERIFY_(STATUS)
       if (FORMATTED=="YES") THEN
          call READ_PARALLEL(layout, &
                             var_2d(lbound(var_3d,1),:), unit, rc=status)
       else
          if(present(sfc)) then
            call VarRead_R4_2d(unit, grid, var_2d, sfc, rc=status)
          else
            call VarRead_R4_2d(unit, grid, var_2d, rc=status)
          end if
       end if
    else if (rank == 3) then
       call ESMF_ArrayGetData(array, var_3d, ESMF_DATA_REF, rc=status)
       VERIFY_(STATUS)
       if (associated(var_3d)) then !ALT: temp kludge
          if (FORMATTED=="YES") THEN
             call READ_PARALLEL(layout, &
                                var_3d(lbound(var_3d,1),lbound(var_3d,2),:), unit)
          else
             call VarRead_R4_3d(unit, grid, var_3d, rc=status)
          endif
       end if
      !if(deid==0)print *,' READ 3D VAR = ',minval(var_3d),maxval(var_3d)
    else
       print *, "ERROR: unsupported RANK"
    endif
    VERIFY_(STATUS)

  RETURN_(ESMF_SUCCESS)
  end subroutine FieldRead

!-------------------------------------------------------------------
  subroutine FillBundle( BUNDLE, NVARS, stType, RC)
!-------------------------------------------------------------------
  
! !ARGUMENTS:

    type(ESMF_BUNDLE),           intent(INOUT) :: BUNDLE
    integer,                     intent(IN   ) :: NVARS, stType
    integer, optional,           intent(  OUT) :: RC
    
! !DESCRIPTION:
    

!EOP
    
    character(len=*), parameter  :: Iam="FillBundle"
    integer                      :: STATUS
    
! Locals
    
    type(ESMF_GRID)              :: ESMFGRID
    type(ESMF_FIELD)             :: FIELD
    type(ESMF_ARRAY)             :: ARRAY
    type(ESMF_DELAYOUT)          :: LAYOUT
    type(ESMF_FieldDataMap)      :: DATAMAP2
    type(ESMF_FieldDataMap)      :: DATAMAP3
    integer                      :: IM,  JM,  LM
    integer                      :: IM0, JM0
    integer                      :: L1, L, K
    integer                      :: NumVars
    integer                      :: counts(5)
    integer                      :: dims(3)
    logical                      :: IamRoot, twoD
    real, pointer                :: lons_in(:), lats_in(:)
    real, pointer                ::  PTR2(:,:),  PTR3(:,:,:)
    character(len=ESMF_MAXSTR)   :: NAME
    character(len=ESMF_MAXSTR) :: difnam(16), defnam(7)
    integer                    :: difdim(16), defdim(7)

   data difnam /'phis','ps','uwnd','vwnd','theta','sphu','ozone',&
              'qitot','qltot','TSKIN','U10M','V10M','SNOWDP', &
              'GWETTOP','TSOIL1','ORO'/
   data difdim /2,2,3,3,3,3,3,3,3,2,2,2,2,2,2,2/
   data defnam /'dps','du','dv','dt','dq','doz','dql'/
   data defdim /2,3,3,3,3,3,3/

! Start

! Get info from the bundle
!-------------------------

   call ESMF_BundleGet     (Bundle,   Grid=ESMFGRID, FieldCount=NUMVARS, RC=STATUS)
   VERIFY_(STATUS)
   call ESMF_GridGet       (ESMFGRID, horzRelLoc=ESMF_CELL_CENTER, &
                             vertRelloc = ESMF_CELL_CELL,           &
                             globalCellCountPerDim=COUNTS,                RC=STATUS)
   VERIFY_(STATUS)
   call ESMF_GridGet       (ESMFGRID, delayout=LAYOUT,                   RC=STATUS)
   VERIFY_(STATUS)

   IamRoot = MAPL_AM_I_ROOT(LAYOUT)

   call ESMF_GridGetDELocalInfo(ESMFGRID, &
         horzRelLoc=ESMF_CELL_CENTER, &
         vertRelLoc=ESMF_CELL_CELL, &
         localCellCountPerDim=DIMS, RC=STATUS)
   VERIFY_(STATUS)

   ASSERT_( counts(3) /= 1 )

! If the bundle is empty, read entire varlist from file
!------------------------------------------------------

   NUMVARS = nvars

   call ESMF_FieldDataMapSetDefault(datamap2, 2, rc=status)
   VERIFY_(STATUS)
   call ESMF_FieldDataMapSetDefault(datamap3, 3, rc=status)
   VERIFY_(STATUS)

   L1 = 0
   do L=1,NUMVARS

      if(stType==1) then
         if(difdim(L)==2) then
            twoD=.true.
         else
            twoD=.false.
         end if
         Name = difnam(L)
      else
         if(defdim(L)==2) then
            twoD=.true.
         else
            twoD=.false.
         end if
         Name = defnam(L)
      end if
      L1 = L1 + 1

      if(twoD) then
         allocate(PTR2(DIMS(1),DIMS(2)),stat=STATUS)
         VERIFY_(STATUS)
         PTR2  = 0.0
         FIELD = ESMF_FieldCreate(grid=ESMFGRID, copyflag=ESMF_DATA_REF,   &
              fptr=PTR2, name=trim(Name), datamap=datamap2, RC=STATUS)
         VERIFY_(STATUS) 
         call ESMF_FieldSetAttribute(FIELD, NAME='VLOCATION', &
              VALUE=MAPL_VLocationNone, RC=STATUS)
         VERIFY_(STATUS)
         call ESMF_FieldSetAttribute(FIELD, NAME='DIMS', &
              VALUE=MAPL_DimsHorzOnly, RC=STATUS)
         VERIFY_(STATUS)
      else 
         allocate(PTR3(DIMS(1),DIMS(2),DIMS(3)),stat=STATUS)
         VERIFY_(STATUS)
         PTR3  = 0.0
         FIELD = ESMF_FieldCreate(grid=ESMFGRID, copyflag=ESMF_DATA_REF,   &
              fptr=PTR3, name=trim(name), datamap=datamap3, RC=STATUS)
         VERIFY_(STATUS)
         call ESMF_FieldSetAttribute(FIELD, NAME='VLOCATION', &
              VALUE=MAPL_VLocationCenter, RC=STATUS)
         VERIFY_(STATUS)
         call ESMF_FieldSetAttribute(FIELD, NAME='DIMS', &
              VALUE=MAPL_DimsHorzVert, RC=STATUS)
         VERIFY_(STATUS)
      end if
      call ESMF_BundleAddField(BUNDLE,FIELD, RC=STATUS)
      VERIFY_(STATUS)
      
   end do
   NUMVARS = L1  ! could be less than on file if user chooses to

   end subroutine FillBundle
   
   end program GSIsa_Gaussian
