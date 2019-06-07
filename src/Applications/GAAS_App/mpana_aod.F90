#  include "MAPL_Generic.h"

      program mpana_aod

!-----------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: ana_aod:  AOD Analysis Application
!
! !INTERFACE:
!
!      Usage:  mpana_aod.x [-o fname]  [OPTIONS]  aerFile  [odsfiles]
!
!      where
!
!      -o fname   output ODS file name (default: obs.ods)
!      aerfile    gridded aerosol mixing ratio file name
!      odsfile    input ODS file name(s)
!
! !USES:
!
      use  ESMF
      use  MAPL_Mod
      use  Chem_SimpleBundleMod
      use  Chem_RegistryMod
      use  Chem_MieMod
      use  Chem_AodMod

      use  m_ana
      use  m_ods
      use  m_odsmeta, only: X_PASSIVE
      use  m_obs
      use  m_simul


      use  m_die
      use  m_zeit   ! timer
      use  m_FileResolv
      use  m_StrTemplate
      use  m_inpak90
      use  m_psas

      implicit NONE

! !DESCRIPTION: Given {\em Dynamics vector}} and {\em ODS} files,
!               this application invokes the Observer() producing
!  a quality conbtroled, post-analysis ODS file. Since no analysis
!  is performed, the O-A (observation minus analysis) attribute is
!  not set.
!
! !REVISION HISTORY:
!
!  19oct1999  da Silva  Derived from ods_qc.f
!  20dec1999  da Silva  Changed name from ut_obs.x to obs.x
!  05Jun2002  Dee       Added -pick option
!  11Jun2002  Todling   Bug fix in call to maxval
!  03Feb2003  Todling   Updated init of PSAS 
!  04Mar2003  Todling   Moved psas_init call after init.
!  09feb2010  da Silva  Adapt for AOD.
!
!EOP
!-----------------------------------------------------------------------

      character(len=*), parameter :: myname = 'ana_aod'

!     Local variables
!     ---------------
      integer :: im, jm, km, nch
      integer :: rc, nobs, nobs_good, nODS, i
      integer :: nymd=0, nhms=0, nymd_b, nhms_b
      integer :: yy, mm, dd, h, m, s

      real*8  :: alpha         ! bias update coefficient
      real*8  :: chAOD(2)      ! Range of AOD waenumbers (nm) to analyze

      logical :: verbose = .FALSE.
      logical :: do_sbc=.FALSE.
      logical :: do_avk=.FALSE.
      logical :: skipPSAS=.FALSE.

      integer, parameter :: mODS = 32


      character(len=ESMF_MAXSTR) :: pre_ods(mODS) ! input pre-analysis ODS file name
      character(len=ESMF_MAXSTR) :: pos_ods       ! post-analysis ODS file name
      character(len=ESMF_MAXSTR) :: expid, fname
      character(len=ESMF_MAXSTR) :: ftypes(mODS)  ! ods file type

!     Control variables and obervations
!     ---------------------------------
      type (MAPL_SimpleBundle)   :: q_f          ! Gridded concentration background 
      type (MAPL_SimpleBundle)   :: q_a          ! Gridded concentration analysis
      type (MAPL_SimpleBundle)   :: y_f          ! Gridded AOD background
      type (MAPL_SimpleBundle)   :: y_a, dy_a    ! Gridded AOD analysis, increments
      type (MAPL_SimpleBundle)   :: b_f          ! Gridded AOD bias estimate
      type (MAPL_SimpleBundle)   :: y_k          ! Gridded averaging kernel approximation

      type (Chem_Mie)      :: Mie                ! Mie Tables, etc
      type (Chem_Registry) :: aerReg             ! Registry with many species
      type (Chem_Registry) :: aodReg             ! Registry with single XX tracer
      type (ods_vect)      :: ods

!     Basic ESMF objects
!     ------------------
      type(ESMF_Config)    :: CF       ! Resource file
      type(ESMF_Grid)      :: etaGrid  ! Eta Grid (lon, lat, eta)
      type(ESMF_Grid)      :: aodGrid  ! AOD Grid (lon, lat, channel)
      type(ESMF_Time)      :: Time
      type(ESMF_VM)        :: VM

      integer :: Nx, Ny         ! Layout
      integer :: IM_World, JM_World, LM_WORLD ! Global Grid dimensions
      integer :: CM_World       ! Number of channels

      integer :: root, comm

      call Main()

CONTAINS

!...............................................................................................
      Subroutine Main()

                                   __Iam__('ana_aod')

      call zeit_ci ( 'ana_aod' )

!   Initialize PSAS
!   ---------------
    call psas_init(root, comm)

!   Initialize the ESMF. For performance reasons, it is important
!    to turn OFF ESMF's automatic logging feature
!   -------------------------------------------------------------
    call ESMF_Initialize (logKindFlag=ESMF_LOGKIND_NONE, VM=VM, __RC__)
    call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, __RC__ )

    if ( MAPL_am_I_root() ) then
       print *
       print *, '     --------------------------------------'
       print *, '     ana_aod - the aod analysis application'
       print *, '     --------------------------------------'
       print *
    end if

!   Parse command line
!   ------------------
    call Init_ ( pre_ods, mODS, pos_ods, nODS ) 

!                                     -------------------
!                                       ESMF Grid, Etc
!                                     -------------------

!   Load resources
!   --------------
    CF = ESMF_ConfigCreate(__RC__)
    call ESMF_ConfigLoadFile(CF, fileName='mpana.rc', __RC__)
    call ESMF_ConfigGetAttribute(CF, expid, Label='ExpId:',  __RC__ )

!   Switches
!   --------
    call ESMF_ConfigGetAttribute(CF, verbose,  Label='verbose:',  __RC__ )
    call ESMF_ConfigGetAttribute(CF, do_sbc,   Label='do_averaging_kernel:',  __RC__ )
    call ESMF_ConfigGetAttribute(CF, do_avk,   Label='do_statistical_bias_correction:',  __RC__ )
    call ESMF_ConfigGetAttribute(CF, skipPSAS, Label='do_you_want_to_skip_PSAS:',  __RC__ )

    call ESMF_ConfigGetAttribute(CF, alpha, Label='alpha_for_bias_estimation:',  __RC__ )
    call ESMF_ConfigGetAttribute(CF, chAOD, count=2, Label='range_of_wavenumbers_to_analyze_in_nm:',  __RC__ )

!   World grid dimensions and layout
!   --------------------------------
    call ESMF_ConfigGetAttribute(CF, IM_World, Label='IM_World:',  __RC__ )
    call ESMF_ConfigGetAttribute(CF, JM_World, Label='JM_World:',  __RC__ )
    call ESMF_ConfigGetAttribute(CF, LM_World, Label='LM_World:',  __RC__ )
    call ESMF_ConfigGetAttribute(CF, CM_World, Label='CM_World:',  __RC__ )
    call ESMF_ConfigGetAttribute(CF, Nx,       Label='Layout_Nx:', __RC__ )
    call ESMF_ConfigGetAttribute(CF, Ny,       Label='Layout_Ny:', __RC__ )

!   Create global grids:
!   -------------------
    etaGrid = MAPL_LatLonGridCreate (Name='etaGrid',        &
                                     Nx = Nx, Ny = Ny,      &
                                     IM_World = IM_World,   &
                                     JM_World = JM_World,   &
                                     LM_World = LM_World,   &
                                    __RC__ )

    aodGrid = MAPL_LatLonGridCreate (Name='aodGrid',        &
                                     Nx = Nx, Ny = Ny,      &
                                     IM_World = IM_World,   &
                                     JM_World = JM_World,   &
                                     LM_World = CM_World,   &
                                    __RC__ )


!   Validate grid
!   -------------
    call ESMF_GridValidate(etaGrid,__RC__)
    call ESMF_GridValidate(aodGrid,__RC__)

!   Get date/time from CF
!   ---------------------
    call ESMF_ConfigGetAttribute(CF, nymd, Label='nymd:', __RC__ )
    call ESMF_ConfigGetAttribute(CF, nhms, Label='nhms:', __RC__ )

!   Create ESMF Time
!   ----------------
    yy = nymd/10000; mm = (nymd-yy*10000) / 100; dd = nymd - (10000*yy + mm*100)
    h  = nhms/10000;  m = (nhms - h*10000) / 100;  s = nhms - (10000*h  +  m*100)
    call ESMF_TimeSet(Time, yy=yy, mm=mm, dd=dd,  h=h,  m=m, s=s)

!                                     -------------------
!                                     Gridded Background
!                                     -------------------

!     Registries
!     ----------
      aerReg = Chem_RegistryCreate ( rc, 'GAAS_AerRegistry.rc' )
      if ( rc == 0 ) then
         if ( MAPL_AM_I_ROOT() ) then
            call Chem_RegistryPrint(aerReg)
         end if
      else
         call die(myname,'could not read Chem Registry for specifies')
      end if
      aodReg = Chem_RegistryCreate ( rc, 'GAAS_AodRegistry.rc' )
      if ( rc == 0 ) then
         if ( MAPL_AM_I_ROOT() ) then
              call Chem_RegistryPrint(aodReg)
         end if
      else
         call die(myname,'could not read Chem Registry for AOD')
      end if

!     Get AOD background
!     -------------------
      q_f = Chem_SimpleBundleRead (CF, 'aer_bkg_filename', etaGrid, time=Time, __RC__ )
      Mie = Chem_MieCreate(CF, chemReg=aerReg, __RC__)
      y_f = Chem_SimpleBundleCreate ('aod_bkg', aodReg, aodGrid, &
                                      Levs=1.e9*Mie%channels,    &
                                      LevUnits="nm", delp=null(), __RC__) 
      call Chem_AodCalculator (y_f, q_f, Mie, verbose, __RC__)

      call MAPL_SimpleBundlePrint(q_f)
      call MAPL_SimpleBundlePrint(y_f)

      nch = CM_World

!     Write AOD background file (before bias correction s applied
!     -----------------------------------------------------------
      call Chem_SimpleBundleWrite (y_f, CF, 'aod_bkg_filename', Time, __RC__ )

      call exit(1)

!                                     ---------------------------
!                                      Background Bias Correction
!                                     ---------------------------

      if ( do_sbc ) then

!       Read in bias restart
!       --------------------
        b_f = Chem_SimpleBundleRead (CF, 'aodbias_internal_restart', aodGrid,  rc=status )

!       If bias restart is not present, bootstrap it
!       --------------------------------------------
        if ( STATUS == 0 ) then
           if ( MAPL_AM_I_ROOT() ) print *, '[ ] Read in bias file '
        else
           b_f = Chem_SimpleBundleCreate('aod_bias', aodReg, aodGrid, __RC__) 
        end if           

!       Unbias background
!       -----------------
        if ( log_transf ) then
             y_f%r3(1)%q = log(eps+y_f%r3(1)%q) - b_f%r3(1)%q 
             y_f%r3(1)%q = exp(y_f%r3(1)%q) - eps
        else
             y_f%r3(1)%q = y_f%r3(1)%q - b_f%r3(1)%q 
        end if

      end if

!     Ensure positiveness
!     -------------------
      y_f%r3(1)%q = max(0.0,y_f%r3(1)%q)


!                                     -------------------
!                                        Observations
!                                     -------------------

#if 0

!     Resolve ODS files
!     -----------------
      do i = 1, nODS
         call FileResolv ( expid, nymd, nhms, pre_ods(i), pre_ods(i) )
         print *, '[ ] ODS file ', i, trim(pre_ods(i))
      end do

!     Read observations for this this synoptic time
!     ---------------------------------------------
                                                                                call zeit_ci ( 'ods_get' )
      call ODS_Get ( nODS, pre_ods, nymd, nhms, ftypes, ods, rc )
                                                                                call zeit_co ( 'ods_get' )
      if ( rc .eq. 0 ) then
         nobs =  ods%data%nobs
         print *, myname//': read ODS files'
         print *, myname//': nobs = ', nobs
         print *
      else
         call die ( myname, 'could not read ODS files')
      end if

!     Prints out input ODS summary
!     ----------------------------
      call ODS_Tally ( 6, ods, nobs, rc )

!     Call observer for this date/time
!     --------------------------------
      call zeit_ci ( 'Observer' )
      call Observer ( nymd, nhms, y_f, nobs, ods, nobs_good )
                                                                               call zeit_co ( 'Observer' )
                                                                              
!     Move all QC'ed data to front
!     ----------------------------
      call ODS_Select ( ods, nobs, nobs_good, rc, &
                        qcexcl_list=(/ 0, X_PASSIVE /))
      ods%data%nobs = nobs_good

#endif

!                                     -------------------
!                                          Analysis
!                                     -------------------

!     Create AOD analysis increment
!     -----------------------------
      dy_a = Chem_SimpleBundleCreate('aod_ainc', aodReg, aodGrid, __RC__) 

!     Create averaging kernel
!     -----------------------
      if ( do_avk ) then 
         y_k = Chem_SimpleBundleCreate('aod_avek', aodReg, aodGrid, __RC__) 
      end if

!     Perform AOD analysis proper; analysis variable may be LogAOD
!     ------------------------------------------------------------
#if 0
                                                                              call zeit_ci ( 'Analyzer' )
      call Analyzer ( ods, nobs_good, dy_a, chAOD, do_avk, y_k, skipPSAS )
                                                                              call zeit_co ( 'Analyzer' )
#endif

!     Update bias estimate
!     --------------------
      if ( do_sbc ) then
         b_f%r3(1)%q = b_f%r3(1)%q - alpha * dy_a%r3(1)%q
      end if

!     Create AOD analysis
!     -------------------
      y_a = Chem_SimpleBundleCreate('aod_ana', aodReg, aodGrid, __RC__) 

!     At this point analysis will be LogAOD if using log-transform
!     ------------------------------------------------------------
      if ( log_transf ) then
         y_a%r3(1)%q = log(eps+y_f%r3(1)%q) + dy_a%r3(1)%q 
      else
         y_a%r3(1)%q = y_f%r3(1)%q + dy_a%r3(1)%q 
      end if

!     Compute O-A
!     -----------
#if 0
                                                                             call zeit_ci ( 'simulator' )
      call Simulator ( y_a, ods%data, ods%data%oma, &
                       nobs_good, noqc=.true., log_transf=log_transf)
                                                                             call zeit_co ( 'simulator' )
#endif

!     Now that O-A is computed, convert analysis variable to AOD, 
!      if needed (ensure positiveness)
!     -----------------------------------------------------------
      if ( log_transf ) &
           y_a%r3(1)%q = max(0.0,exp(y_a%r3(1)%q) - eps)

!     Prints out input ODS summary
!     ----------------------------
#if 0
      if ( MAPL_AM__I_ROOT() ) then
           call ODS_Tally ( 6, ods, nobs, rc )
      end if
#endif

!                                     -------------------
!                                           Output
!                                     -------------------

!     Write AOD analysis file
!     -----------------------
      call Chem_SimpleBundleWrite (y_a, CF, 'aod_ana_filename', Time, __RC__ )

!     Write AOD analysis increment file
!     ---------------------------------
      call Chem_SimpleBundleWrite (dy_a, CF, 'aod_inc_filename', Time, __RC__ )

!     Write Averaging Kernel
!     ----------------------
      if ( do_avk ) then
         call Chem_SimpleBundleWrite (y_k, CF, 'aod_avk_filename', Time, __RC__ )
      end if
 
!     Update bias restart file
!     ------------------------
      if ( do_sbc ) then
        call Chem_SimpleBundleWrite (b_f, CF, 'bias_internal_checkpoint', Time, __RC__ )
      end if

!     Write QC'd ods file
!     -------------------
#if 0
      call StrTemplate ( fname, pos_ods, xid=expid, nymd=nymd, nhms=nhms)
      call ODS_Put ( fname, 'post_analysis', nymd, nhms, ods, rc )
      if ( rc .eq. 0 ) then
         print *, '   [] Wrote ODS file '//trim(fname)//' with ', nobs_good, ' obs'
      else
         call die ( myname, 'could not write ODS file '//trim(fname) )
      endif
#endif

!     Free memory
!     -----------
#if 0
      call MAPL_SimpleBundleDestroy(y_f,__RC__)
      call MAPL_SimpleBundleDestroy(y_a,__RC__)
      call MAPL_SimpleBundleDestroy(dy_a,__RC__)
      call MAPL_SimpleBundleDestroy(y_k,__RC__)
      call ods_clean ( ods, rc )
#endif

!     Print out timing info
!     ---------------------
      call zeit_co    ( 'ana_aod' )

      if ( MAPL_am_I_root() ) then
         call zeit_flush ( 6 )
         close(999)
         open (999,file='ANAAOD_EGRESS',form='formatted')
         close(999)
      end if

!     Finalize PSAS
!     -------------
      call psas_end()

      call exit(0)

     end subroutine Main

!-----------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: Init_ --- Initialize the AOD analysis app
!
! !INTERFACE:
!
      subroutine Init_ ( pre_ods, mODS, pos_ods, nODS )

! !USES:

      use m_die
      implicit NONE

!
! !OUTPUT PARAMETERS:

      integer, intent(in)        :: mODS          ! maximum number of pre-analysis
                                                  !  ODS files

      character(len=ESMF_MAXSTR), intent(out) :: pre_ods(mODS) !  input ODS file name

      character(len=ESMF_MAXSTR), intent(out) :: pos_ods       ! post-analysis ODS file name

      integer, intent(out)       :: nODS          ! actual number of pre-analysis
                                                  !  ODS files
!
! !DESCRIPTION: parses command line.
!
! !REVISION HISTORY:
!       12deb2010  da Silva  Initial code based on GEOS-4 implementation.
!
!EOP
!-----------------------------------------------------------------------

      character*4, parameter :: myname = 'init'

      integer iret, i, iarg, argc, iargc, n, ier
      character(len=ESMF_MAXSTR) :: argv, rcfile, fname, answer

!     Default file names
!     ------------------
      pos_ods = '%s.inst1d_aod_Nx.%y4%m2%d2_%h2.ods'
      pre_ods(:) = '/dev/null'

      rcfile='ana.rc'

!                                 ----------------------
!                                   Parse Command Line
!                                 ----------------------

      argc =  iargc()
      nODS = 0
      iarg = 0
      n = 0
      do i = 1, argc
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iArg, argv )
         if (index(argv,'-o' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, pos_ods )
         end if
      end do

!                                 -------------------
!                                 Parse Resource File
!                                 -------------------

!     Load resource
!     -------------
      call i90_loadf ( rcfile, ier )
      if ( ier .ne. 0 ) call die(myname, 'could not read rc file '//trim(rcfile) )

!     Read resource file if pre-analysis ODS files have not been specified
!     --------------------------------------------------------------------
      if ( nODS .eq. 0 ) then

         call i90_label ( 'ODS_files::', ier )
         if ( ier .ne. 0 ) call die (myname, 'could not read ODS file names' )
         nODS = 0
         do i = 1, mODS
            call i90_gline ( ier )
            if ( ier .eq. -1 ) &
                 call die ( myname, &
                 'premature end of rc file; make sure table ends with "::"')
            if ( ier .eq. 1 ) exit
            call i90_gtoken ( fname, ier )
            if ( ier .ne. 0 ) call die (myname, 'premature end of rc file')
            nODS = nODS + 1
            pre_ods(nODS) = fname
         end do

      end if

      if ( nODS .eq. 0 ) call die ( myname, 'no ODS file specified' )


!     release resources
!     -----------------
      call i90_release()

      end subroutine Init_

      subroutine usage_()
        if ( MAPL_am_I_root() ) then
           print *
           print *, 'USAGE:  ana_aod.x [-o pos_ods]'
           print *
           print *, 'where'
           print *
           print *, ' -o pos_ods   post-analysis ODS file name'
           print *
           print *
        end if
        call exit(1)
      end subroutine usage_

    end program mpana_aod



