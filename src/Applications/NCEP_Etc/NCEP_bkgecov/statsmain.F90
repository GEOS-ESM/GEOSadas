program statsmain
!$$$  main program documentation block
!                .      .    .                                       .
! main program: statsmain
!   prgrmmr: kleist           org: np20                date: 2005-01-15
!
! program history log:
!   2005-01-xx  kleist   initial version of stats code
!   2005-04-18  kleist   use sp library calls, add ability to easily run
!                        in single or double precision, modify post module
!                        to create byte addressable files for viewing using
!                        GRADS, compute variances for pseudo-RH and normalized
!                        RH, use IO on gridded fields to reduce memory usage
!   2005-04-xx  todling  read of namelist fix for most machines
!   2005-04-25  wgu      use LAPACK library calls to do SVD
!                        different buffers are specified in mpi_allreduce 
!                        (no aliasing) for code use in Halem(OSF)
!   2005-08-01  wgu      routines: m_GsiGrided.f90, m_fvHeader.f90, 
!                        m_rdgrids.f90, m_stvp.f90, m_calap.f90,
!                        m_getcases.f90, m_readpairs.f90, m_vordiv.f90
!                        m_speclap.f90 added to handle the forecast fields
!                        at regular lat-long grids, and to generate the 
!                        background stats at lat-lon grids by setting 
!                        lgaus=.false. or at Gaussian grids with lgaus=.true.
!   2014-xx  ElAkkraoui  perform overall revamping of the background error stats code
!                        include addoz and gsi-ready stats output in main stats program 
!   03May2015 Todling    turn hydrometeors optional for consistency w/ MERRA & M2 codes
!   03May2015 Todling    enable L132
!   23Apr2020 Todling    add timers
!   11Jun2020 Todling    add nreaders: controls num of readers; avoids mem contention
!                        add readperts: calc-stats from NMC diff files directly
!                        add calchrzscl and calcvrtscl options to ease debugging
!
! abstract:
!   This code computes background error statistics to be used with the
!   GSI analysis code.  The NMC method, utilizing 24/48 hour forecast 
!   pairs valid at the same time, is used to compute variance, length
!   scale, and linear balance projection estimates.
!
! input files:
!   fort.10      - listing of NCEP global model forecast files
!   berror_sst   - fixed, global sst statistics file
!
! output files:
!   gsi.berror_stats - double precision stats output (serves as input to GSI)
!   bgstats_sp.grd   - single precision byte-addressable file for latidude 
!                      dependent variables
!   sststats_sp.grd  - single precision byte-addressable file SST statistics
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use type_kinds, only: fp_kind
  use variables, only: mype,npe
  use variables, only: nsig,nlat,nlon,fnm0
  use variables, only: lgaus,lsmver,biasrm,laddoz,lbal
  use variables, only: maxcases,hybrid,smoothdeg
  use variables, only: smooth_vert_variances
  use variables, only: init_vars
  use variables, only: create_vars,create_grids
  use variables, only: destroy_grids,destroy_vars
  use variables, only: create_bias_g
  use variables, only: destroy_bias_g
  use variables, only: create_mapping,destroy_mapping
  use variables, only: hydromet
  use variables, only: init_smooth_vars
  use variables, only: hcoeffs,vcoeffs
  use variables, only: nreaders
  use variables, only: readperts
  use variables, only: calchrzscl,calcvrtscl
  use variables, only: rhbounds
  use variables, only: hrzsfactor
  use specgrid,  only: jcap,nc,slat,jb,je
  use specgrid,  only: init_spec
  use postmod,   only: writefiles
  use m_GsiGrided, only: vectype
  use m_GsiGrided, only: input_vpsf
  use m_GsiGrided, only: GsiGrided_set
  use comm_mod, only: nxpe,nype,init_mpi_vars,destroy_mpi_vars
  use m_zeit, only: zeit_ci,zeit_co,zeit_flush
! use MAPL_Mod
! use ESMF
! use MAPL_BaseMod
#ifndef ibm_sp
  use m_mpif
#endif
  implicit none
#ifdef ibm_sp
  include 'mpif.h'
#endif

  integer k,n,total,numcases,mycases,ierror

! define namelist
! NAMSTAT
!   jcap      - spectral resolution of forecast pairs
!   nsig      - number of vertical levels
!   nlat      - number of latitudes
!   nlon      - number of longitudes
!   maxcases  - maximum number of forecast pairs to process
!   hybrid    - logical for hybrid vertical coordinate
!   smoothdeg - degree of horizontal smoothing to apply in latitudinal direction
!   hydromet  - controls addition of hydrometeors
!   smooth_vert_variances  - smooth variances vertically
!   lbal      - control for balance operator
!   nreaders  - number of PE participating in reading files
!   readperts - read NMC diffs instead of full fields
!   calchrzscl- useful for debug to bypass horz scales calc
!   calcvrtscl- useful for debug to bypass vert scales calc
!   rhbounds(2)-specify lower and uppper bounds on rh diff
!   nxpe,nype  -allow testings w/ ESMF-like distribution
!   hrzsfactor -parameter allowing for extra adjustment to horizontal scales
!   input_vpsf -.true. when input has vp/sf in u/v slots

  namelist/namstat/jcap,lgaus,lsmver,nsig,nlat,nlon,&
                   maxcases,hybrid,smoothdeg,fnm0,vectype,biasrm,laddoz,lbal,&
                   hydromet,smooth_vert_variances,nreaders,readperts,calchrzscl,&
                   calcvrtscl,rhbounds,nxpe,nype,hrzsfactor,input_vpsf
  namelist/smoothvars/hcoeffs,vcoeffs

! MPI initial setup
  call mpi_init(ierror)
  call mpi_comm_size(mpi_comm_world,npe,ierror)
  call mpi_comm_rank(mpi_comm_world,mype,ierror)

! Initialize defaults for namelist variables
  call init_vars
  call init_spec
  call GsiGrided_set

! Read in namelist
#ifdef ibm_sp
  read(5,namstat)
#else
  open(11,file='stats.parm')
  read(11,namstat)
#endif

  if(mype==0) then
    write(6,*)'starting computation of background stats with ',npe,' tasks'
    write(6,namstat)
  endif

  if(mype==0) write(6,*) 'INITIALIZE VARIABLES'
  call zeit_ci('init')
  call create_grids(nlat,nlon,jcap,lgaus)
  call create_mapping
  call create_vars(nlat,nlon,nsig)
  call init_mpi_vars(nsig,mype)

  call init_smooth_vars(nsig)
  call zeit_co('init')

! Read in namelist
#ifdef ibm_sp
  read(5,smoothvars)
#else
  open(11,file='stats.parm')
  read(11,smoothvars)
  close(11)
#endif

 
! Call routine to do subdomain decomposition based on
! grid size and number of pe''s
  call zeit_ci('domain')
  call deter_subdomain(mype)
  call init_commvars(mype)
  call zeit_co('domain')

! Make call to see how many available files there are
  if(mype==0) write(6,*) 'COUNT NUMBER OF AVAILABLE CASES'
#ifdef gmao_intf
    call zeit_ci('getcases')
    call m_getcases(numcases,mype)
    call zeit_co('getcases')
#else
    call getcases(numcases,mype)
#endif

#ifdef gmao_intf
    call zeit_ci('rdpairs')
    call m_readpairs(npe,mype,numcases,mycases)
    call zeit_co('rdpairs')
#endif
  if (mype==0) then 
     write(6,*) 'main: numcases,mycases= ', numcases,mycases
  endif

! get balanced projections
  call zeit_ci('calcbal')
  call calcbal(mycases,numcases,npe,mype)
  call zeit_co('calcbal')

! get variances
  call zeit_ci('variances')
  call variances(mycases,numcases,npe,mype)
  call zeit_co('variances')

  if (calcvrtscl) then
!    get vertical length scale estimates
     call zeit_ci('vertlength')
     call vertlength(mycases,numcases,npe,mype)
     call zeit_co('vertlength')
  endif

  if (calchrzscl) then
!    get horizontal length scale estimates
     call zeit_ci('horlength')
     call horlength(mycases,numcases,npe,mype)
     call zeit_co('horlength')
  endif

  call zeit_ci('writeout')
  if (mype==0) then
!   Replace Ozone field
    if ( laddoz ) then
      call addoz
    end if
!   post Processing
    write(6,*) 'WRITE OUT BACKGROUND ERROR STATISTICS'
    call writefiles
  end if
  call zeit_co('writeout')

!#ifdef gmao_intf
!    call destroy_bias_g
!#endif

  call destroy_grids
  call destroy_mapping
  call destroy_mpi_vars
  call destroy_vars

  if(mype==0) call zeit_flush(6,subname_at_end=.true.)
  if (mype==0) write(6,*) '*** STATS CODE COMPLETE! ***'
  call mpi_finalize(ierror)

end program statsmain 
