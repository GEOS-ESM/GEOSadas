
      program ana_aod

!-----------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: ana_aod:  AOD Analysis Application
!
! !INTERFACE:
!
!      Usage:  ana_aod.x [-o fname]  [OPTIONS]  aerFile  [odsfiles]
!
!      where
!
!      -o fname   output ODS file name (default: obs.ods)
!      aerfile    gridded aerosol mixing ratio file name
!      odsfile    input ODS file name(s)
!
! !USES:
!
      use  Chem_RegistryMod
      use  Chem_BundleMod

      use  m_ana
      use  m_ods
      use  m_odsmeta, only: X_PASSIVE, X_BUDDY, ktAOD, ktLogAOD
      use  m_obs
      use  m_simul


      use m_mpif90, only : mp_init
      use  m_die
      use  m_zeit   ! timer
      use  m_FileResolv
      use  m_StrTemplate
      use  m_inpak90
      use  m_psas
      use mpi

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
!  19mar2017  Todling   Handle for Log(AOD) in ODS files.
!
!EOP
!-----------------------------------------------------------------------

      character(len=*), parameter :: myname = 'ana_aod'

!     Local variables
!     ---------------
      integer :: im, jm, km, nch
      integer :: im_so, jm_so    ! dimension of superob space
      integer :: rc, nobs, nobs_good, nODS, i, naod, nlaod
      integer :: nymd=0, nhms=0, nymd_b, nhms_b
      integer :: myKTobs
      integer :: kt_list(2)

      real*8  :: alpha         ! bias update coefficient
      real*8  :: chAOD(2)      ! Range of AOD waenumbers (nm) to analyze

      real :: AINC_MAX ! used to check PSAS convergence

      logical :: pick=.FALSE., verbose = .FALSE.
      logical :: do_sbc=.FALSE.
      logical :: do_avk=.FALSE.
      logical :: skipPSAS=.FALSE.

      integer, parameter :: mODS = 32

      character*256 :: expid 

      character*256 :: aerFile       ! Aerosol backround file name (concentration)
      character*256 :: pre_ods(mODS) ! input pre-analysis ODS file name

      character*256 :: aer_a         ! Aerosol analysis file name (concentration)
      character*256 :: aod_f         ! AOD background file name
      character*256 :: aod_a         ! AOD analyis file name
      character*256 :: aod_d         ! AOD analyis increment file name
      character*256 :: aod_k         ! AOD averaging kernel file name
      character*256 :: pos_ods       ! post-analysis ODS file name
      character*256 :: fname

      character*256 :: bias_rs       ! AOD bias restart file name
      character*256 :: bias_ck       ! AOD bias checkpoint file name

      character*256 :: ftypes(mODS)  ! ods file type

!     COntrol variables and obervations
!     ---------------------------------
      type (Chem_Bundle)   :: x_f          ! Gridded concentration background 
      type (Chem_Bundle)   :: x_a          ! Gridded concentration analysis
      type (Chem_Bundle)   :: y_f          ! Gridded AOD background
      type (Chem_Bundle)   :: y_a, dy_a    ! Gridded AOD analysis, increments
      type (Chem_Bundle)   :: b_f          ! Gridded AOD bias estimate
      type (Chem_Bundle)   :: y_k          ! Gridded averaging kernel aproximation

      type (Chem_Registry) :: aerReg       ! Registry with many species
      type (Chem_Registry) :: aodReg        ! Registry with single XX tracer
      type (ods_vect)      :: ods, ods_


      logical:: test_nan
      integer k,l,ier


      Call mp_init(ier)
      if ( ier /= 0) call mp_die ( myname, 'MP_INIT()',ier)

      call zeit_ci ( 'ana_aod' )

!     Parse command line
!     ------------------
      call Init_ ( expid, aerFile, pre_ods, mODS,  
     &             aer_a, aod_a, aod_d, aod_f, aod_k, pos_ods, bias_rs, bias_ck,
     &             do_sbc, do_avk, alpha, chAOD, skipPSAS,      
     &             pick, nymd, nhms, im_so, jm_so, nODS, verbose )
     
!     Initialize PSAS
!     ---------------
      call psas_init(topology = PURE_OMP)

!                                     -------------------
!                                     Gridded Background
!                                     -------------------

!     Registries
!     ----------
      aerReg = Chem_RegistryCreate ( rc, 'GAAS_AerRegistry.rc' )
      if ( rc == 0 ) then
         call Chem_RegistryPrint(aerReg)
      else
         call die(myname,'could not read Chem Registry for species')
      end if
      aodReg = Chem_RegistryCreate ( rc, 'GAAS_AodRegistry.rc' )
      if ( rc .ne. 0 ) then
         call die(myname,'could not read Chem Registry for AOD')
      end if


!     Load AOD background
!     -------------------
      call AOD_GetDims (aerFile, im, jm, km, rc)
      if ( rc /= 0 ) then
         print *, ' rc = ', rc
         call die(myname, 
     &       'could not determine dimensions of mixing ratio file' 
     &        //trim(aerfile))
      end if

!     Handle optional superob dimension
!     ---------------------------------
      if ( im_so < 1 ) im_so = im
      if ( jm_so < 1 ) jm_so = jm


                                                                               call zeit_ci ( 'aod_get' )
      call  AOD_GetTau ( aerFile, im, jm, km, nymd, nhms, verbose, 
     &                   y_f, rc )
                                                                               call zeit_co ( 'aod_get' )
      if ( rc /= 0 ) then
         call die(myname,'could not read mixing ratio file'
     &                   //trim(aerFile))
      end if

      nch = y_f%grid%km 

!     Write AOD background file (before bias correction s applied
!     -----------------------------------------------------------
      if ( aod_f(1:4) .ne. 'NONE' ) then
           call StrTemplate ( fname, aod_f, xid=expid, nymd=nymd, nhms=nhms )
           call Chem_BundleWrite ( fname, nymd, nhms, 0, y_f, rc, verbose=.true. )
           if ( rc .ne. 0 ) 
     &          call die ( myname, 'could not write AOD background file '//trim(fname) )
      end if

!                                     ---------------------------
!                                      Background Bias Correction
!                                     ---------------------------

      if ( do_sbc ) then

!       Read in bias restart
!       --------------------
        call Chem_BundleRead ( bias_rs, nymd_b, nhms_b, b_f, rc, 
     &                         ChemReg=aodReg )

!       If bias restart is not present, bootstrap it
!       --------------------------------------------
        if ( rc == 0 ) then
           print *, '[ ] Read in bias file '//trim(bias_rs)//
     &              ' on ', nymd_b, nhms_b
        else
           call Chem_BundleCreate( aodReg, im, jm, nch, b_f, rc )
           if ( rc .eq. 0 ) then
              b_f%delp = b_f%missing_value 
              b_f%rh   = b_f%missing_value
              b_f%qa(1)%data3d = 0.0
              b_f%grid%lev => y_f%grid%lev
              b_f%grid%levUnits = y_f%grid%levUnits
              print *, '[*] Bias has been initialized'
           else
              call die(myname,'cannot create bundle for bias')
           end if
        end if           

!       Unbias background
!       -----------------
        if ( log_transf ) then
             y_f%qa(1)%data3d = log(eps+y_f%qa(1)%data3d) - b_f%qa(1)%data3d 
             y_f%qa(1)%data3d = exp(y_f%qa(1)%data3d) - eps
        else
             y_f%qa(1)%data3d = y_f%qa(1)%data3d - b_f%qa(1)%data3d 
        end if

      end if

!     Ensure positiveness
!     -------------------
      y_f%qa(1)%data3d = max(0.0,y_f%qa(1)%data3d)


!                                     -------------------
!                                        Observations
!                                     -------------------

!     Resolve ODS files
!     -----------------
      do i = 1, nODS
         call FileResolv ( expid, nymd, nhms, pre_ods(i), pre_ods(i) )
         print *, '[ ] ODS file ', i, trim(pre_ods(i))
      end do

!     Read observations for this this synoptic time
!     ---------------------------------------------
                                                                                call zeit_ci ( 'ods_get' )
      call ODS_Get ( nODS, pre_ods, nymd, nhms, ftypes, ods_, rc )
                                                                                call zeit_co ( 'ods_get' )

      if ( rc /= 0 ) then
         call die ( myname, 'could not read ODS files')
      end if
      ! can read AOD and log AOD obs
      kt_list = (/ktAOD,ktLogAOD/)
      call ods_select( ods_, ods_%data%nobs, nobs, rc, 
     &                 qcexcl=0, kt_list=kt_list, odss = ods ) ! keep what we need
      if ( rc .eq. 0 ) then
         nobs  = ods%data%nobs
         naod  = count(ods%data%kt==ktAOD)
         nlaod = count(ods%data%kt==ktLogAOD)
         ! if Log AOD comes in as input, convert obs into AOD
         if (eps<=0.0) then 
             call die ( myname, 'eps not defined for conversion of log(AOD) obs into AOD')
         else
           if (nlaod >0) then 
               where (ods%data%kt == ktLogAOD) 
                      ods%data%obs = max(eps,exp(ods%data%obs)-eps)
                      ods%data%kt  = ktAOD            
               endwhere
           endif
         endif
         myKTobs = ktAOD  ! all obs are now supposed to be AOD 
         call ods_clean(ods_,rc) ! get rid of what we do not need
         print *, myname//': read ODS files'
         print *, myname//': nobs = ', nobs
         print *, myname//': naod = ', naod, ' and nlaod = ', nlaod
         print *
      else
         call die ( myname, 'could not perform ODS select')
      end if


!     Prints out input ODS summary
!     ----------------------------
      call fix_odsmeta(ods)
      call ODS_Tally ( 6, ods, nobs, rc )

!     Call observer for this date/time (the convert option becomes obsolete now)
!     --------------------------------
                                                                               call zeit_ci ( 'Observer' )
      call Observer ( nymd, nhms, y_f, nobs, ods, nobs_good, convertOb2AOD=(myKTobs==ktLogAOD) )
                                                                               call zeit_co ( 'Observer' )
                                                                              
!     Move all QC'ed data to front
!     ----------------------------
      call ODS_Select ( ods, nobs, nobs_good, rc, 
     &                  qcexcl_list=(/ 0, X_PASSIVE, X_BUDDY /))
      ods%data%nobs = nobs_good

      call ODS_Tally ( 6, ods, nobs, rc )

!                                     -------------------
!                                          Analysis
!                                     -------------------

!     Create AOD analysis
!     -------------------
      call Chem_BundleCreate( aodReg, im, jm, nch, dy_a, rc )
      if ( rc .eq. 0 ) then
           dy_a%delp = dy_a%missing_value 
           dy_a%rh   = dy_a%missing_value
           dy_a%grid%lev => y_f%grid%lev
           dy_a%grid%levUnits = y_f%grid%levUnits
      else 
           call die(myname,'cannot create bundle for AOD increments')
      end if

!     Create averaging kernel
!     -----------------------
      if ( do_avk ) then 
         call Chem_BundleCreate( aodReg, im, jm, nch, y_k, rc )
         if ( rc .eq. 0 ) then
            y_k%delp = y_k%missing_value 
            y_k%rh   = y_k%missing_value
            y_k%grid%lev => y_f%grid%lev
            y_k%grid%levUnits = y_f%grid%levUnits
         else 
            call die(myname,'cannot create bundle for Averaging Kernel')
         end if
      end if

!     Perform AOD analysis proper; analysis variable may be LogAOD
!     ------------------------------------------------------------
                                                                              call zeit_ci ( 'Analyzer' )
      call Analyzer ( ods, nobs_good, im_so, jm_so, dy_a, 
     &                 chAOD, do_avk, y_k, skipPSAS )

      
      
                                                                              call zeit_co ( 'Analyzer' )
!     Stability device:
!        Check for signs of non-convergence; redo analysis at lower
!        resolution if this is the case     
!     -----------------------------------------------------------
  
      if (.not. PSAS_convergence(dy_a)) then

         print *, "<<WARNING>> PSAS appears to have NOT converged at native resolution, trying superob at lower resolution ..."
               im_so = im / 2
               jm_so = (jm-1)/2 + 1
               call Analyzer ( ods, nobs_good, im_so, jm_so, dy_a, 
     &              chAOD, do_avk, y_k, skipPSAS )
         
               if (PSAS_convergence(dy_a) ==.FALSE.) then
                   print*, "<<WARNING>><<WARNING>> no convergence at low resolution as well; so increments will be all set to zeros"
                   dy_a%qa(1)%data3d = 0.0 
               else
                   print*, "<<WARNING>> PSAS appears to have converged now at lower resolution"
               end if
      end if

!     Update bias estimate
!     --------------------
      if ( do_sbc ) then
!!!      b_f%qa(1)%data3d = (1.-alpha) * b_f%qa(1)%data3d - alpha * dy_a%qa(1)%data3d
         b_f%qa(1)%data3d = b_f%qa(1)%data3d - alpha * dy_a%qa(1)%data3d
      end if

!     Create AOD analysis
!     -------------------
      call Chem_BundleCreate( aodReg, im, jm, nch, y_a, rc )
      if ( rc == 0 ) then
           y_a%delp = y_a%missing_value 
           y_a%rh   = y_a%missing_value
           y_a%grid%lev => y_f%grid%lev ! same levels as background
           y_a%grid%levUnits = y_f%grid%levUnits  
      else
         call die(myname,'cannot create bundle for AOD analysis')
      end if

!     At this point analysis will be LogAOD if using log-transform
!     ------------------------------------------------------------
      if ( log_transf ) then
         y_a%qa(1)%data3d = log(eps+y_f%qa(1)%data3d) + dy_a%qa(1)%data3d 
      else
         y_a%qa(1)%data3d = y_f%qa(1)%data3d + dy_a%qa(1)%data3d 
      end if

!     Compute O-A
!     -----------
                                                                             call zeit_ci ( 'simulator' )
      call Simulator ( y_a, ods%data, ods%data%oma, 
     &                 nobs_good, noqc=.true., log_transf=log_transf)
                                                                             call zeit_co ( 'simulator' )


!     Now that O-A is computed, convert analysis variable to AOD, 
!      if needed (ensure positiveness)
!     -----------------------------------------------------------
      if ( log_transf ) 
     &     y_a%qa(1)%data3d = max(0.0,exp(y_a%qa(1)%data3d) - eps)

!     Prints out input ODS summary
!     ----------------------------
      call ODS_Tally ( 6, ods, nobs, rc )

!                                     -------------------
!                                           Output
!                                     -------------------

!     Write AOD analysis file
!     -----------------------
      if ( aod_a(1:4) .ne. 'NONE' ) then
           call StrTemplate ( fname, aod_a, xid=expid, nymd=nymd, nhms=nhms )
           call Chem_BundleWrite ( fname, nymd, nhms, 0, y_a, rc, verbose=.true. )
           if ( rc .ne. 0 ) 
     &          call die ( myname, 'could not write AOD analysis file '//trim(fname) )
      end if

!     Write AOD analysis increment file
!     ---------------------------------
      if ( .not. skipPSAS ) then
         call StrTemplate ( fname, aod_d, xid=expid, nymd=nymd, nhms=nhms )
         call Chem_BundleWrite ( fname, nymd, nhms, 0, dy_a, rc, verbose=.true. )
         if ( rc .ne. 0 ) 
     &        call die ( myname, 'could not write AOD analysis increment file '
     &                           //trim(fname) )
      endif

!     Write Averaging Kernel
!     ----------------------
      if ( do_avk ) then
         call StrTemplate ( fname, aod_k, xid=expid, nymd=nymd, nhms=nhms )
         call Chem_BundleWrite ( fname, nymd, nhms, 0, y_k, rc, verbose=.true. )
         if ( rc .ne. 0 ) 
     &        call die ( myname, 'could not write Averaging Kernel file '//trim(fname) )
      end if

!     Update bias restart file
!     ------------------------
      if ( do_sbc ) then
           call StrTemplate ( fname, bias_ck, xid=expid, nymd=nymd, nhms=nhms )
           call Chem_BundleWrite ( fname, nymd, nhms, 0, b_f, rc, verbose=.true. )
           if ( rc .ne. 0 ) 
     &          call die ( myname, 'could not write AOD bias checkpoint file '//trim(fname) )
      end if

!     Write QC'd ods file
!     -------------------
      call StrTemplate ( fname, pos_ods, xid=expid, nymd=nymd, nhms=nhms)
      call ODS_Put ( fname, 'post_analysis', nymd, nhms, ods, rc )
      if ( rc .eq. 0 ) then
         print *, '   [] Wrote ODS file '//trim(fname)//' with ', nobs_good, ' obs'
      else
         call die ( myname, 'could not write ODS file '//trim(fname) )
      endif

!     Free memory
!     -----------
#if 0
      call Chem_BundleDestroy(y_f,rc)
      call Chem_BundleDestroy(y_a,rc)
      call Chem_BundleDestroy(dy_a,rc)
      call ods_clean ( ods, rc )
#endif

!     Print out timing info
!     ---------------------
      call zeit_co    ( 'ana_aod' )
      call zeit_flush ( 6 )

!     Finalize PSAS
!     -------------
      call psas_end()

!     Signal completion
!     -----------------
      close(999)
      open (999,file='ANAAOD_EGRESS',form='formatted')
      close(999)

      call exit(0)

      CONTAINS


      logical function PSAS_convergence(dy_a)
            type(chem_bundle), intent(in) :: dy_a
            integer i, j, k
            PSAS_convergence = .TRUE.
            do k = 1, nch
               do i = 1, im 
                  do j = 1, jm
                     if (ISNAN(dy_a%qa(1)%data3d(i, j, k))) then
                         PSAS_convergence = .FALSE.
                     return
                     endif  
                  enddo      
               enddo
            enddo

            if ( log_transf ) then
                  AINC_MAX = 4.0
            else
                  AINC_MAX = 20.
            end if

            if ( maxval(abs(dy_a%qa(1)%data3d)) > AINC_MAX )  then
                    PSAS_convergence = .FALSE.
                    return
            endif
      
      end function PSAS_convergence



!-----------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1   !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: Init_ --- Initialize the AOD analysis app
!
! !INTERFACE:
!
      subroutine Init_ ( expid, aerFile, pre_ods, mODS,    
     &                   aer_a, aod_a, aod_d, aod_f, aod_k, pos_ods, bias_rs, bias_ck,
     &                   do_sbc, do_avk, alpha, chAOD, skipPSAS, 
     &                   pick, nymd, nhms, im_so, jm_so, nODS, verbose )

! !USES:

      use m_die
      implicit NONE

!
! !OUTPUT PARAMETERS:

      integer, intent(in)        :: mODS          ! maximum number of pre-analysis
                                                  !  ODS files

      character*256, intent(out) :: expid         ! experiment ID

      character*256, intent(out) :: aerFile       ! Aerosol backround file name (concentration)
      character*256, intent(out) :: pre_ods(mODS) !  input ODS file name

      character*256, intent(out) :: aer_a         ! Aerosol analysis file name (concentration)
      character*256, intent(out) :: aod_a         ! AOD analyis file name
      character*256, intent(out) :: aod_f         ! AOD background file name
      character*256, intent(out) :: aod_k         ! AOD averaging kernel file name
      character*256, intent(out) :: aod_d         ! AOD analyis increment file name
      character*256, intent(out) :: pos_ods       ! post-analysis ODS file name

      logical , intent(out)      :: do_sbc        ! if true, perform bias correction
      logical , intent(out)      :: do_avk        ! if true, compute averaging kernel
      real*8, intent(out)        :: alpha         ! bias update coefficient
 
      real*8,  intent(out)       :: chAOD(2)      ! Range of AOD waenumbers (nm) to analyze

      logical , intent(out)      :: skipPSAS      ! if true, do not call PSAS

      character*256, intent(out) :: bias_rs       ! AOD bias restart file name
      character*256, intent(out) :: bias_ck       ! AOD bias checkpoint file name

      logical , intent(out)      :: pick          ! if true, 
                                                  ! (nymd,nhms) are input
                                                  ! parameters to dyn_get.

      integer, intent(out)       :: nymd, nhms    ! date/time for first guess
                                                  ! if pick = .true.

      integer, intent(out)       :: im_so, jm_so  ! superob dimension

      integer, intent(out)       :: nODS          ! actual number of pre-analysis
                                                  !  ODS files
      logical , intent(out)      :: verbose
!
! !DESCRIPTION: parses command line.
!
! !REVISION HISTORY:
!       12dec2010  da Silva  Initial code based on GEOS-4 implementation.
!       21oct2016  Todling   Allow user to specify alternative rc-file
!
!EOP
!-----------------------------------------------------------------------

      character*4, parameter :: myname = 'init'

      integer iret, i, iarg, argc, iargc, n, ier
      character*256 :: argv, rcfile, fname, answer

!     Default file names
!     ------------------
      expid = 'gaas'
      aer_a = '%s.inst3d_ana-aer_Nv.%y4%m2%d2_%h2.nc'
      aod_a = 'NONE' ! '%s.inst2d_ana-aod_Nx.%y4%m2%d2_%h2.nc'
      aod_f = 'NONE' ! '%s.inst2d_ana-aod_Nx.%y4%m2%d2_%h2.nc'
      aod_k = 'NONE' ! '%s.inst2d_ana-aod_Nx.%y4%m2%d2_%h2.nc'
      aod_d = '%s.inst2d_ainc-aod_Nx.%y4%m2%d2_%h2.nc'
      bias_rs = 'aodbias_internal_restart.nc'
      bias_ck = 'aodbias_internal_checkpoint.nc'
      pos_ods = '%s.inst1d_aod_Nx.%y4%m2%d2_%h2.ods'

      aerFile = '/dev/null'
      pre_ods(:) = '/dev/null'

      rcfile='ana.rc'
      nymd = -1
      nhms = -1
      im_so = -1
      jm_so = -1
      pick = .false.
      do_sbc = .false.
      do_avk = .false.
      verbose = .false.

      print *
      print *, '     --------------------------------------'
      print *, '     ana_aod - the aod analysis application'
      print *, '     --------------------------------------'
      print *

!                                 ----------------------
!                                   Parse Command Line
!                                 ----------------------

      argc =  iargc()
      if ( argc < 1 ) call usage_()
      nODS = 0
      iarg = 0
      n = 0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iArg, argv )
         if (index(argv(1:2),'-A' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, aer_a )
         else if (index(argv(1:4),'-avk' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            do_avk = .true.
         else if (index(argv(1:2),'-a' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, aod_a )
         else if (index(argv(1:2),'-f' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, aod_f )
         else if (index(argv(1:2),'-k' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, aod_k )
         else if (index(argv(1:2),'-d' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, aod_d )
         else if (index(argv(1:2),'-b' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, bias_ck )
         else if (index(argv(1:2),'-x' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, expid )
         else if (index(argv(1:2),'-o' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, pos_ods )
         else if (index(argv(1:3),'-rc' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, rcfile )
         else if (index(argv(1:4),'-sbc' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            do_sbc = .true.
         else if (index(argv(1:9),'-skipPSAS' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            skipPSAS = .true.
         else if (index(argv(1:5),'-pick' ) .gt. 0 ) then
            if ( iarg+2 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) nymd
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) nhms
            pick = .true.
         else if (index(argv(1:2),'-v' ) .gt. 0 ) then
            verbose = .true.
         else
            n = n + 1
            if ( n .eq. 1 ) then
               !!! call FileResolv ( trim(prefix), nymd, nhms, argv, dyn_f )
               aerFile = argv
            else
               nODS = nODS + 1
               if ( nODS .gt. mODS ) call die ( myname, 'too many ODS files' )
               pre_ods(nODS) = trim(argv)
            end if
         end if
      end do

!     Derived parameters
!     ------------------
      if ( aod_k(1:4) == 'NONE' ) then
           do_avk = .false. ! without a file name, skip it
      else
           do_avk = .true. 
      end if


!                                 -------------------
!                                 Parse Resource File
!                                 -------------------

!     Load resource
!     -------------
      call i90_loadf ( rcfile, ier )
      if ( ier .eq. 0 ) then
         if (trim(rcfile)/='ana.rc') then
            print *
            print *, 'Reading user-specified rc-file: ',trim(rcfile)
            print *
         endif
      else
         call die(myname, 'could not read rc file '//trim(rcfile) )
      endif

!     Read resource file if pre-analysis ODS files have not been specified
!     --------------------------------------------------------------------
      if ( nODS .eq. 0 ) then

         call i90_label ( 'ODS_files::', ier )
         if ( ier .ne. 0 ) call die (myname, 'could not read ODS file names' )
         nODS = 0
         do i = 1, mODS
            call i90_gline ( ier )
            if ( ier .eq. -1 )
     &           call die ( myname,
     &           'premature end of rc file; make sure table ends with "::"')
            if ( ier .eq. 1 ) exit
            call i90_gtoken ( fname, ier )
            if ( ier .ne. 0 ) call die (myname, 'premature end of rc file')
            nODS = nODS + 1
            pre_ods(nODS) = fname
         end do

      end if

      if ( nODS .eq. 0 ) call die ( myname, 'no ODS file specified' )

!     Do we want to perform statistical bias correction (SBC)?
!     NOTE: command line takes precedence over rc file
!     -------------------------------------------------------
      if ( .not. do_avk ) then
         call i90_label ( 'do_averaging_kernel:', ier )
         if ( ier .eq. 0 ) then
            call i90_gtoken ( answer, ier )
            if ( ier .eq. 0 ) then
               if ( answer(1:1) .eq. 'n' .or. answer(1:1) .eq. 'N'  ) then
                    do_avk = .false.
                else
                   do_avk = .true.
                end if
             end if
          end if
      end if


!     Do we want to perform statistical bias correction (SBC)?
!     NOTE: command line takes precedence over rc file
!     -------------------------------------------------------
      if ( .not. do_sbc ) then
         call i90_label ( 'do_statistical_bias_correction:', ier )
         if ( ier .eq. 0 ) then
            call i90_gtoken ( answer, ier )
            if ( ier .eq. 0 ) then
               if ( answer(1:1) .eq. 'n' .or. answer(1:1) .eq. 'N'  ) then
                    do_sbc = .false.
                else
                   do_sbc = .true.
                end if
             end if
          end if
      end if

!     Try reading eps, ok if not present but code will only handle
!     AOD in the input file; code will fail is log AOD found and eps
!     is not defined.
      call i90_label ( 'eps_for_log_transform_aod_on_input:', ier )
      if (ier==0) then
         eps = i90_gfloat ( ier )
      else
         print *, trim(myname), ': WARNING, eps not defined early enough'
         print *, trim(myname), ': WARNING, code can only handle AOD in input(ODS)'
      endif

!     Bias coefficient
!     ----------------
      if ( do_SBC ) then
         call i90_label ( 'alpha_for_bias_estimation:', ier )
         if ( ier .eq. 0 ) then
              alpha = i90_gfloat ( ier )
         else
              call die ( myname, 'cannot read alpha for bias estimation' )
         end if
      end if

!     Skip PSAS
!     ---------
      if ( .not. skipPSAS ) then
         call i90_label ( 'do_you_want_to_skip_PSAS:', ier )
         if ( ier .eq. 0 ) then
            call i90_gtoken ( answer, ier )
            if ( ier .eq. 0 ) then
               if ( answer(1:1) .eq. 'n' .or. answer(1:1) .eq. 'N'  ) then
                  skipPSAS = .false.
               else
                  skipPSAS = .true.
               end if
            end if
         end if
      end if         

!     Range of channls to analyze
!     ---------------------------
      call i90_label ('range_of_wavenumbers_to_analyze_in_nm:', ier )
      if ( ier .eq. 0 ) then
        chAOD(1) = i90_gfloat ( ier )
        if ( ier .ne. 0 ) 
     &       call die(myname,'cannot read range of wanumbers to analyze')
        chAOD(2) = i90_gfloat ( ier )
        if ( ier .ne. 0 )  chAOD(2) = chAOD(1)
      end if

!     Superob dimension
!     -----------------
      call i90_label ('superob_IMxJM:', ier )
      if ( ier .eq. 0 ) then
        im_so = i90_gInt ( ier )
        if ( ier .ne. 0 ) 
     &       call die(myname,'cannot read superob IM')
        jm_so = i90_gInt ( ier )
        if ( ier .ne. 0 ) 
     &       call die(myname,'cannot read superob JM')
      else
         im_so = -1
         jm_so = -1
      end if


!     release resources
!     -----------------
      call i90_release()

!     Echo the parameters
!     -------------------
      print *, ' Input Aerosol file: ', trim(aerFile)
      print *, '    Input ODS files: ', nODS
      do i = 1, nODS
      print *, '                     ', trim(pre_ods(i))
      end do
      print *, '   Output ODS files: ', trim(pos_ods)
      print *, '    Bias Correction: ', do_sbc
      print *, '   Averaging Kernel: ', do_avk
      print *, '          Skip PSAS: ', skipPSAS
      print *

      end subroutine Init_

      subroutine usage_()
      print *
      print *, 'USAGE:  ana_aod.x [OPTIONS] aer_f [pre_ods]'
      print *
      print *, 'where'
      print *
!!!      print *, ' -A aer_a     Aerosol concentration analysis file name'
!!!      print *
      print *, ' -a aod_a     AOD analysis file name'
      print *
      print *, ' -avk         calculate approximate averaging kernel'
      print *
      print *, ' -f aod_f     AOD background file name'
      print *
      print *, ' -d aod_d     AOD analysis increment file name'
      print *
      print *, ' -k aod_k     AOD averaging kernel file name'
      print *
      print *, ' -b bias_ck   AOD bias checkpoint file name'
      print *
      print *, ' -o pos_ods   post-analysis ODS file name'
      print *
      print *, ' -pick nymd nhms'
      print *, '              date and time to be read from first guess'
      print *, '              file (default is fist date/time on file)'
      print *
      print *, ' -sbc         perform Statistical Bias Correction (SBC)'
      print *
      print *, ' -skipPSAS    skip analysis (debugging)'
      print *
      print *, ' -v           turn on verbose'
      print *
      print *, ' -x expid     experiment ID'
      print *
      print *, ' aer_f        background aerosol concentration file name'
      print *
      print *, ' pre_ods      input pre-analyis ODS file name; if omitted, file names'
      print *, '               must be provided in resource file'
      print *
      call exit(1)
      end subroutine usage_


      end program ana_aod



